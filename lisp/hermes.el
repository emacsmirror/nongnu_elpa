;;; hermes.el --- Emacs frontend for Hermes Agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.1.0
;; URL: https://git.thanosapollo.org/emacs-hermes
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.3.1") (websocket "1.15") (markdown-mode "2.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fresh Emacs frontend for Hermes Agent.  The entry point is an
;; EWOC-backed dashboard over live Hermes chat buffers.

;;; Code:

(require 'ewoc)
(require 'keymap-popup)
(require 'subr-x)
(require 'hermes-chat)
(require 'hermes-sessions)
(require 'hermes-inventory)
(require 'hermes-rollback)
(require 'hermes-subagents)
(require 'hermes-cron)
(require 'hermes-profiles)
(require 'hermes-kanban)
(require 'hermes-tracker)
(require 'hermes-mcp)
(require 'hermes-browser)
(require 'hermes-onboarding)

(defgroup hermes nil
  "Emacs frontend for Hermes Agent."
  :group 'applications)

(defcustom hermes-dashboard-buffer-name "*Hermes Dashboard*"
  "Name of the Hermes dashboard buffer."
  :type 'string)

(defcustom hermes-dashboard-refresh-delay 0.2
  "Seconds to debounce dashboard refreshes after chat state changes."
  :type 'number)

(defcustom hermes-dashboard-stale-after 30
  "Seconds before an active chat without updates is shown as stale.
A nil value disables stale status display."
  :type '(choice (const :tag "Never mark stale" nil)
                 (number :tag "Seconds")))

(defcustom hermes-dashboard-stale-refresh-interval 2
  "Seconds between dashboard refreshes while live chats are active.
This lets stale dashboard statuses appear even when no new transport event
arrives."
  :type '(choice (const :tag "Do not refresh for stale status" nil)
                 (number :tag "Seconds")))

(defface hermes-dashboard-heading
  '((t :inherit (bold font-lock-constant-face)))
  "Face used for the Hermes dashboard heading.")

(defface hermes-dashboard-title
  '((t :inherit bold))
  "Face used for dashboard card titles.")

(defface hermes-dashboard-muted
  '((t :inherit shadow))
  "Face used for muted dashboard details.")

(defface hermes-dashboard-status-ready
  '((t :inherit success))
  "Face used for ready dashboard statuses.")

(defface hermes-dashboard-status-running
  '((t :inherit font-lock-keyword-face))
  "Face used for running dashboard statuses.")

(defface hermes-dashboard-status-waiting
  '((t :inherit warning))
  "Face used for waiting dashboard statuses.")

(defface hermes-dashboard-status-stale
  '((t :inherit warning))
  "Face used for stale dashboard statuses.")

(defface hermes-dashboard-status-error
  '((t :inherit error))
  "Face used for error dashboard statuses.")

(defvar-local hermes-dashboard--ewoc nil
  "EWOC displaying dashboard cards in the current dashboard buffer.")

(defvar-local hermes-dashboard--nodes nil
  "Hash table mapping dashboard node ids to EWOC nodes.")

(defvar-local hermes-dashboard--needs-onboarding nil
  "Non-nil when the gateway reports no usable provider credentials.
Set by `hermes-dashboard--check-auth' to surface a provider-onboarding card.")

(defvar hermes-dashboard--refresh-timer nil
  "Timer used to debounce dashboard refreshes.")

(defvar-local hermes-dashboard--stale-refresh-timer nil
  "Timer used to refresh stale status display in this dashboard buffer.")

(defvar hermes-dashboard-mode-map)

(keymap-popup-define hermes-dashboard-mode-map
  "Hermes Dashboard"
  :parent special-mode-map
  :description "Hermes Dashboard"
  :group "Navigate"
  "n" ("Next card" hermes-dashboard-next)
  "p" ("Previous card" hermes-dashboard-previous)
  "RET" ("Open" hermes-dashboard-open)
  "<mouse-1>" ("Open with mouse" hermes-dashboard-mouse-open)
  :group "Session"
  "c" ("Chat" hermes-chat)
  "e" ("Connect provider" hermes-onboarding-connect-provider)
  "S" ("Sessions" hermes-list-sessions)
  :group "Selected chat"
  "i" ("Interrupt" hermes-dashboard-interrupt)
  "s" ("Steer" hermes-dashboard-steer)
  "a" ("Respond to prompt" hermes-dashboard-respond)
  "m" ("Switch model" hermes-dashboard-switch-model)
  "d" ("Disconnect" hermes-dashboard-disconnect)
  :group "View"
  "g" ("Refresh" hermes-dashboard-refresh)
  "I" ("Inventory" hermes-list-inventory)
  "R" ("Rollbacks" hermes-list-rollbacks)
  "A" ("Subagents" hermes-list-subagents)
  "C" ("Cron jobs" hermes-list-crons)
  "K" ("Kanban" hermes-list-kanban)
  "T" ("Tracker" hermes-list-tracker-repositories)
  "X" ("MCP servers" hermes-list-mcp)
  "?" ("Help" hermes-dashboard-popup))

(keymap-set hermes-dashboard-mode-map "h" #'hermes-dashboard-popup)

(defun hermes-dashboard--header-line ()
  "Return the dashboard header line."
  (let ((chat-count 0))
    (when (hash-table-p hermes-dashboard--nodes)
      (maphash (lambda (_id node)
                 (when (eq (plist-get (ewoc-data node) :kind) 'chat)
                   (setq chat-count (1+ chat-count))))
               hermes-dashboard--nodes))
    (format " Hermes Dashboard  |  %d live chat%s  |  g refresh  ? help "
            chat-count (if (= chat-count 1) "" "s"))))

(defun hermes-dashboard--node-id (node)
  "Return dashboard NODE's stable id."
  (plist-get node :id))

(defun hermes-dashboard--node-kind (node)
  "Return dashboard NODE's kind."
  (plist-get node :kind))

(defconst hermes-dashboard--status-table
  (cl-loop for (statuses label face)
           in '((("ready" "done" "completed" "complete" "success" "succeeded"
                  "idle")
                 "Ready" hermes-dashboard-status-ready)
                (("running" "streaming" "progress" "tool" "busy" "started"
                  "preparing" "in-progress")
                 "Running" hermes-dashboard-status-running)
                (("pending" "waiting" "queued")
                 "Waiting" hermes-dashboard-status-waiting)
                (("starting" "loading")
                 "Loading" hermes-dashboard-status-running)
                (("connecting" "reconnecting")
                 "Connecting" hermes-dashboard-status-running)
                (("closed" "disconnected")
                 "Disconnected" hermes-dashboard-status-error)
                (("stale") "Stale" hermes-dashboard-status-stale)
                (("approval-requested")
                 "Approval requested" hermes-dashboard-status-waiting)
                (("requested" "input-requested")
                 "Input requested" hermes-dashboard-status-waiting)
                (("interrupted") "Interrupted" hermes-dashboard-status-error)
                (("error" "failed" "failure" "cancelled" "canceled")
                 "Error" hermes-dashboard-status-error))
           append (mapcar (lambda (status) (list status label face)) statuses))
  "Known normalized dashboard statuses mapped to (STATUS LABEL FACE).")

(defun hermes-dashboard--status-name (status)
  "Return STATUS as a normalized comparison string."
  (and-let* ((name (cond
                    ((symbolp status) (symbol-name status))
                    ((stringp status) status))))
    (let ((trimmed (string-trim name)))
      (unless (string-empty-p trimmed)
        (replace-regexp-in-string "[[:space:]_.]+" "-"
                                  (downcase trimmed))))))

(defun hermes-dashboard--status-title (status)
  "Return a fallback display title for STATUS."
  (if-let* ((name (hermes-dashboard--status-name status)))
      (capitalize (replace-regexp-in-string "-" " " name))
    "Unknown"))

(defun hermes-dashboard--status-entry (status)
  "Return STATUS's (STATUS LABEL FACE) table entry, or nil when unknown."
  (and-let* ((name (hermes-dashboard--status-name status)))
    (assoc name hermes-dashboard--status-table)))

(defun hermes-dashboard--status-symbol (status)
  "Return the known normalized status symbol for STATUS."
  (and-let* ((entry (hermes-dashboard--status-entry status)))
    (intern (car entry))))

(defun hermes-dashboard--status-label (status)
  "Return human display label for STATUS."
  (if-let* ((entry (hermes-dashboard--status-entry status)))
      (nth 1 entry)
    (hermes-dashboard--status-title status)))

(defun hermes-dashboard--status-face (status)
  "Return face for dashboard STATUS."
  (or (nth 2 (hermes-dashboard--status-entry status))
      'hermes-dashboard-muted))

(defun hermes-dashboard--nonempty-string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun hermes-dashboard--time-age (time)
  "Return age of TIME in seconds, or nil."
  (and time (float-time (time-since time))))

(defun hermes-dashboard--stale-chat-p (snapshot)
  "Return non-nil when SNAPSHOT describes a stale active chat."
  (let ((status (plist-get snapshot :status))
        (updated (plist-get snapshot :updated)))
    (and (numberp hermes-dashboard-stale-after)
         (or (plist-get snapshot :pending-assistant-p)
             (hermes-chat--active-status-p status))
         updated
         (> (or (hermes-dashboard--time-age updated) 0)
            hermes-dashboard-stale-after))))

(defun hermes-dashboard--format-chat-detail (node)
  "Return detail strings for chat dashboard NODE."
  (let* ((activity (hermes-dashboard--nonempty-string
                    (plist-get node :activity)))
         (connection (hermes-dashboard--nonempty-string
                      (plist-get node :connection)))
         (session-id (hermes-dashboard--nonempty-string
                      (plist-get node :session-id)))
         (active-tools (plist-get node :active-tools))
         (pending-prompts (or (plist-get node :pending-prompts) 0))
         (tools (and active-tools
                     (format "tools: %s" (string-join active-tools "; "))))
         (prompts (and (> pending-prompts 0)
                       (format "%d pending prompt%s"
                               pending-prompts
                               (if (= pending-prompts 1) "" "s")))))
    (delq nil
          (list activity
                connection
                (and session-id (format "session %s" session-id))
                tools
                prompts
                (and (plist-get node :stale-p) "no recent updates")))))

(defun hermes-dashboard--insert-key (key)
  "Insert dashboard KEY using the `help-key-binding' face."
  (insert (propertize (format "[%s]" key) 'face 'help-key-binding)))

(defun hermes-dashboard--card-properties (node help &rest extra)
  "Return text properties for dashboard NODE with HELP and EXTRA."
  (append (list 'mouse-face 'highlight
                'help-echo help
                'hermes-dashboard-node-id (hermes-dashboard--node-id node))
          extra))

(defun hermes-dashboard--add-card-properties (start node help &rest extra)
  "Add text properties from START to point for NODE with HELP and EXTRA."
  (when (< start (point))
    (add-text-properties
     start (point)
     (apply #'hermes-dashboard--card-properties node help extra))))

(defun hermes-dashboard--print-action-node (node)
  "Insert action dashboard NODE at point."
  (let ((start (point))
        (key (plist-get node :key))
        (title (plist-get node :title))
        (subtitle (plist-get node :subtitle))
        (action (plist-get node :action)))
    (hermes-dashboard--insert-key key)
    (insert " " (propertize title 'face 'hermes-dashboard-title) "\n")
    (when (hermes-dashboard--nonempty-string subtitle)
      (insert "    " (propertize subtitle 'face 'hermes-dashboard-muted) "\n"))
    (hermes-dashboard--add-card-properties
     start node (format "Run %s" title) 'hermes-dashboard-action action)))

(defun hermes-dashboard--print-chat-node (node)
  "Insert chat dashboard NODE at point."
  (let* ((start (point))
         (buffer (plist-get node :buffer))
         (title (or (plist-get node :title)
                    (and (buffer-live-p buffer) (buffer-name buffer))
                    "<killed chat>"))
         (status (plist-get node :status))
         (status-label (or (plist-get node :status-label)
                           (hermes-dashboard--status-label status)))
         (status-face (hermes-dashboard--status-face status)))
    (insert (propertize "●" 'face status-face)
            " "
            (propertize title 'face 'hermes-dashboard-title)
            "        "
            (propertize status-label 'face status-face)
            "\n")
    (dolist (detail (hermes-dashboard--format-chat-detail node))
      (insert "    " detail "\n"))
    (insert (propertize "    RET open   i interrupt   s steer   a respond\n"
                        'face 'hermes-dashboard-muted))
    (hermes-dashboard--add-card-properties
     start node (format "Open %s" title) 'hermes-dashboard-buffer buffer)))

(defun hermes-dashboard--print-empty-node (node)
  "Insert empty-state dashboard NODE at point."
  (let ((start (point)))
    (insert (propertize (or (plist-get node :title)
                            "No live Hermes chat buffers")
                        'face 'hermes-dashboard-muted)
            "\n")
    (when-let* ((subtitle (hermes-dashboard--nonempty-string
                           (plist-get node :subtitle))))
      (insert "    " (propertize subtitle 'face 'hermes-dashboard-muted) "\n"))
    (hermes-dashboard--add-card-properties
     start node "No live chat buffer is attached to this card")))

(defun hermes-dashboard--print-node (node)
  "Insert dashboard NODE at point."
  (pcase (hermes-dashboard--node-kind node)
    ('action (hermes-dashboard--print-action-node node))
    ('chat (hermes-dashboard--print-chat-node node))
    ('empty (hermes-dashboard--print-empty-node node))
    (_ (insert (propertize (format "%S\n" node)
                           'face 'hermes-dashboard-muted)))))

(defun hermes-dashboard--chat-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Hermes chat buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'hermes-chat-mode))))

(defun hermes-dashboard--chat-buffers ()
  "Return live Hermes chat buffers in `buffer-list' order."
  (seq-filter #'hermes-dashboard--chat-buffer-p (buffer-list)))

(defun hermes-dashboard--onboarding-node ()
  "Return the provider-onboarding action node."
  (list :id "action:onboarding"
        :kind 'action
        :key "e"
        :title "Connect a provider"
        :subtitle "No usable provider credentials -- paste an API key to connect"
        :action #'hermes-onboarding-connect-provider))

(defun hermes-dashboard--action-nodes ()
  "Return static action nodes for the dashboard.
The onboarding node leads when `hermes-dashboard--needs-onboarding' is set."
  (append
   (and hermes-dashboard--needs-onboarding
        (list (hermes-dashboard--onboarding-node)))
   (list (list :id "action:chat"
               :kind 'action
               :key "c"
               :title "Chat"
               :subtitle "Open a new Hermes chat (prompts for a profile)"
               :action #'hermes-chat))))

(defun hermes-dashboard--check-auth ()
  "Surface an onboarding card when a live connection reports no usable provider.
Only runs against an existing live chat client -- it never spawns a transient
connection just to check, so opening the dashboard stays passive.  Branches on
the result `ok' flag because `setup.runtime_check' reports a credential failure
as `ok' nil, not a JSON-RPC error."
  (when (hermes-browser--existing-client)
    (let ((buffer (current-buffer)))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-setup-runtime-check client))
       (lambda (result)
         (when (and (buffer-live-p buffer)
                    (not (eq (hermes-transport--get result 'ok) t)))
           (with-current-buffer buffer
             (setq hermes-dashboard--needs-onboarding t)
             (hermes-dashboard-refresh))))))))

(defun hermes-dashboard--warm-profile-cache ()
  "Warm the profile cache that feeds the chat picker, unless already fresh.
Best-effort and asynchronous: opening the dashboard never blocks or errors on
this, mirroring `hermes-dashboard--check-auth'.  A live chat client's session
token is used when one exists, else plain REST auth.  The cache is keyed by
dashboard URL, so it re-fetches automatically after the configured URL changes."
  (unless (hermes-dashboard-transport-cached-profile-list)
    (hermes--promise-catch
     (hermes-dashboard-transport-profile-list-async
      (hermes-chat--existing-dashboard-client))
     #'ignore)))

(defun hermes-dashboard--chat-node (buffer)
  "Return one chat dashboard node for BUFFER."
  (with-current-buffer buffer
    (let* ((snapshot (hermes-chat--dashboard-snapshot))
           (stale-p (hermes-dashboard--stale-chat-p snapshot))
           (status (if stale-p 'stale (plist-get snapshot :status))))
      (append (list :id (format "chat:%s" (buffer-name buffer))
                    :kind 'chat)
              snapshot
              (list :status status
                    :status-label (hermes-dashboard--status-label status)
                    :stale-p stale-p)))))

(defun hermes-dashboard--empty-node ()
  "Return the dashboard empty-state node."
  (list :id "empty:chats"
        :kind 'empty
        :title "No live Hermes chat buffers"
        :subtitle "Press c to open Chat or N for a new session."))

(defun hermes-dashboard--collect-nodes ()
  "Return dashboard nodes for actions and live chat buffers."
  (let ((chat-nodes (mapcar #'hermes-dashboard--chat-node
                            (hermes-dashboard--chat-buffers))))
    (append (hermes-dashboard--action-nodes)
            (if chat-nodes chat-nodes (list (hermes-dashboard--empty-node))))))

(defun hermes-dashboard--node-at-point (&optional position)
  "Return dashboard EWOC node at POSITION or point."
  (let* ((pos (or position (point)))
         (id (or (get-text-property pos 'hermes-dashboard-node-id)
                 (and (> pos (point-min))
                      (get-text-property (1- pos)
                                         'hermes-dashboard-node-id)))))
    (or (and id hermes-dashboard--nodes (gethash id hermes-dashboard--nodes))
        (and hermes-dashboard--ewoc (ewoc-locate hermes-dashboard--ewoc pos)))))

(defun hermes-dashboard--selected-chat-buffer ()
  "Return the chat buffer selected by the dashboard card at point."
  (let* ((node (hermes-dashboard--node-at-point))
         (data (and node (ewoc-data node)))
         (buffer (plist-get data :buffer)))
    (unless (eq (plist-get data :kind) 'chat)
      (user-error "No Hermes chat card selected"))
    (unless (buffer-live-p buffer)
      (user-error "Selected Hermes chat buffer is no longer live"))
    buffer))

(defun hermes-dashboard--current-ids ()
  "Return dashboard EWOC node ids in display order."
  (let (ids)
    (when hermes-dashboard--ewoc
      (ewoc-map (lambda (node)
                  (push (hermes-dashboard--node-id node) ids)
                  nil)
                hermes-dashboard--ewoc))
    (nreverse ids)))

(defun hermes-dashboard--clear-ewoc ()
  "Remove all nodes from the current dashboard EWOC."
  (when hermes-dashboard--ewoc
    (ewoc-filter hermes-dashboard--ewoc #'ignore)))

(defun hermes-dashboard--rebuild-ewoc (nodes)
  "Rebuild the dashboard EWOC from NODES."
  (let* ((selected (hermes-dashboard--node-at-point))
         (selected-id (and selected
                           (hermes-dashboard--node-id (ewoc-data selected)))))
    (setq hermes-dashboard--nodes (make-hash-table :test #'equal))
    (let ((inhibit-read-only t))
      (hermes-dashboard--clear-ewoc)
      (dolist (node-data nodes)
        (let ((node (ewoc-enter-last hermes-dashboard--ewoc node-data)))
          (puthash (hermes-dashboard--node-id node-data)
                   node hermes-dashboard--nodes))))
    (cond
     ((and selected-id (gethash selected-id hermes-dashboard--nodes))
      (ewoc-goto-node hermes-dashboard--ewoc
                      (gethash selected-id hermes-dashboard--nodes)))
     ((ewoc-nth hermes-dashboard--ewoc 0)
      (ewoc-goto-node hermes-dashboard--ewoc
                      (ewoc-nth hermes-dashboard--ewoc 0))))))

(defun hermes-dashboard--sync-ewoc (nodes)
  "Synchronize the current dashboard EWOC with NODES."
  (let ((current-ids (hermes-dashboard--current-ids))
        (new-ids (mapcar #'hermes-dashboard--node-id nodes)))
    (if (not (equal current-ids new-ids))
        (hermes-dashboard--rebuild-ewoc nodes)
      (dolist (node-data nodes)
        (let* ((id (hermes-dashboard--node-id node-data))
               (node (gethash id hermes-dashboard--nodes)))
          (unless node
            (hermes-dashboard--rebuild-ewoc nodes))
          (when (and node (not (equal node-data (ewoc-data node))))
            (let ((inhibit-read-only t))
              (ewoc-set-data node node-data)
              (ewoc-invalidate hermes-dashboard--ewoc node))))))))

(defun hermes-dashboard--ensure-ewoc ()
  "Ensure the current dashboard buffer has an EWOC."
  (unless (hash-table-p hermes-dashboard--nodes)
    (setq hermes-dashboard--nodes (make-hash-table :test #'equal)))
  (unless hermes-dashboard--ewoc
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (insert "\n"
              (propertize "Hermes" 'face 'hermes-dashboard-heading)
              "\n\n")
      (setq hermes-dashboard--ewoc
            (ewoc-create #'hermes-dashboard--print-node nil "\n" 'nosep)))))

(defun hermes-dashboard-refresh ()
  "Refresh the current Hermes dashboard buffer."
  (interactive)
  (unless (derived-mode-p 'hermes-dashboard-mode)
    (user-error "Not in a Hermes dashboard buffer"))
  (hermes-dashboard--ensure-ewoc)
  (hermes-dashboard--sync-ewoc (hermes-dashboard--collect-nodes))
  (force-mode-line-update))

(defun hermes-dashboard-next (&optional arg)
  "Move to the ARGth next dashboard card."
  (interactive "p")
  (unless hermes-dashboard--ewoc
    (user-error "No dashboard EWOC in this buffer"))
  (condition-case nil
      (ewoc-goto-next hermes-dashboard--ewoc (or arg 1))
    (error (user-error "No next dashboard card"))))

(defun hermes-dashboard-previous (&optional arg)
  "Move to the ARGth previous dashboard card."
  (interactive "p")
  (unless hermes-dashboard--ewoc
    (user-error "No dashboard EWOC in this buffer"))
  ;; `ewoc-goto-prev' clamps at the first element instead of signalling (unlike
  ;; `ewoc-goto-next'), so detect a no-op move to report it like its sibling.
  (let ((before (ewoc-locate hermes-dashboard--ewoc)))
    (ewoc-goto-prev hermes-dashboard--ewoc (or arg 1))
    (when (eq before (ewoc-locate hermes-dashboard--ewoc))
      (user-error "No previous dashboard card"))))

(defun hermes-dashboard-open ()
  "Activate the dashboard card at point."
  (interactive)
  (let* ((node (hermes-dashboard--node-at-point))
         (data (and node (ewoc-data node))))
    (pcase (plist-get data :kind)
      ('action
       (let ((action (plist-get data :action)))
         (if (commandp action)
             (call-interactively action)
           (funcall action))))
      ('chat
       (let ((buffer (plist-get data :buffer)))
         (unless (buffer-live-p buffer)
           (user-error "Selected Hermes chat buffer is no longer live"))
         (pop-to-buffer-same-window buffer)
         (goto-char (or (hermes-chat--input-position) (point-max)))))
      (_ (user-error "No dashboard action at point")))))

(defun hermes-dashboard-mouse-open (event)
  "Activate the dashboard card clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (hermes-dashboard-open))

(defun hermes-dashboard-popup ()
  "Show the dashboard keymap popup."
  (interactive)
  (keymap-popup hermes-dashboard-mode-map))

(defun hermes-dashboard-interrupt ()
  "Interrupt the Hermes chat selected by the dashboard card at point."
  (interactive)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-interrupt)))

(defun hermes-dashboard-steer ()
  "Steer the Hermes chat selected by the dashboard card at point."
  (interactive)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-steer-message)))

(defun hermes-dashboard-respond ()
  "Respond to a prompt in the Hermes chat selected at point."
  (interactive)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-respond-to-prompt)))

(defun hermes-dashboard-switch-model ()
  "Switch the model for the Hermes chat selected at point."
  (interactive)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-switch-model)))

(defun hermes-dashboard-disconnect ()
  "Disconnect the Hermes chat session selected by the dashboard card at point."
  (interactive)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-disconnect)))

(defun hermes-dashboard-refresh-visible ()
  "Refresh the visible Hermes dashboard buffer, when it exists."
  (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'hermes-dashboard-mode)
          (hermes-dashboard-refresh))))))

(defun hermes-dashboard--schedule-refresh ()
  "Schedule a debounced refresh of the Hermes dashboard."
  (when (timerp hermes-dashboard--refresh-timer)
    (cancel-timer hermes-dashboard--refresh-timer))
  (setq hermes-dashboard--refresh-timer
        (run-with-timer hermes-dashboard-refresh-delay nil
                        #'hermes-dashboard-refresh-visible)))

(defun hermes-dashboard--revert (&rest _ignore)
  "Refresh the current dashboard for `revert-buffer'."
  (hermes-dashboard-refresh))

(defun hermes-dashboard--active-chat-buffers-p ()
  "Return non-nil when any live Hermes chat has active status."
  (catch 'active
    (dolist (buffer (hermes-dashboard--chat-buffers))
      (with-current-buffer buffer
        (when (or hermes-chat--pending-assistant-id
                  (hermes-chat--active-status-p
                   (plist-get hermes-chat--status-state :status)))
          (throw 'active t))))
    nil))

(defun hermes-dashboard--stale-refresh (buffer)
  "Refresh dashboard BUFFER when active chats may become stale."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'hermes-dashboard-mode)
                 (hermes-dashboard--active-chat-buffers-p))
        (hermes-dashboard-refresh)))))

(defun hermes-dashboard--cancel-stale-refresh-timer ()
  "Cancel this dashboard buffer's stale refresh timer."
  (when (timerp hermes-dashboard--stale-refresh-timer)
    (cancel-timer hermes-dashboard--stale-refresh-timer))
  (setq hermes-dashboard--stale-refresh-timer nil))

(defun hermes-dashboard--start-stale-refresh-timer ()
  "Start this dashboard buffer's stale refresh timer when configured."
  (hermes-dashboard--cancel-stale-refresh-timer)
  (when (and (numberp hermes-dashboard-stale-refresh-interval)
             (> hermes-dashboard-stale-refresh-interval 0))
    (setq hermes-dashboard--stale-refresh-timer
          (run-with-timer hermes-dashboard-stale-refresh-interval
                          hermes-dashboard-stale-refresh-interval
                          #'hermes-dashboard--stale-refresh
                          (current-buffer)))))

(define-derived-mode hermes-dashboard-mode special-mode "Hermes Dashboard"
  "Major mode for the Hermes dashboard."
  :keymap hermes-dashboard-mode-map
  :interactive nil
  (setq-local header-line-format '(:eval (hermes-dashboard--header-line)))
  (setq-local revert-buffer-function #'hermes-dashboard--revert)
  (add-hook 'kill-buffer-hook #'hermes-dashboard--cancel-stale-refresh-timer
            nil t)
  (add-hook 'change-major-mode-hook
            #'hermes-dashboard--cancel-stale-refresh-timer nil t)
  (setq hermes-dashboard--nodes (make-hash-table :test #'equal)
        hermes-dashboard--ewoc nil)
  (setq-local display-line-numbers nil)
  (hermes-dashboard--start-stale-refresh-timer))

(defun hermes-dashboard--render ()
  "Render the Hermes dashboard in the current buffer."
  (hermes-dashboard--ensure-ewoc)
  (hermes-dashboard-refresh)
  (when-let* ((node (ewoc-nth hermes-dashboard--ewoc 0)))
    (ewoc-goto-node hermes-dashboard--ewoc node)))

(add-hook 'hermes-chat-state-change-hook #'hermes-dashboard--schedule-refresh)

;;;###autoload
(defun hermes ()
  "Open the Hermes dashboard."
  (interactive)
  (let ((buffer (get-buffer-create hermes-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hermes-dashboard-mode)
        (hermes-dashboard-mode))
      (hermes-dashboard--render)
      (hermes-dashboard--check-auth)
      (hermes-dashboard--warm-profile-cache))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (keymap-popup hermes-dashboard-mode-map))))

(provide 'hermes)
;;; hermes.el ends here
