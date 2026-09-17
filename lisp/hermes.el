;;; hermes.el --- Emacs frontend for Hermes Agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Version: 0.7.0
;; URL: https://git.thanosapollo.org/emacs-hermes
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.4.0") (websocket "1.15") (markdown-mode "2.6"))

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

(require 'hermes-buffer)
(require 'cl-lib)
(require 'ewoc)
(require 'keymap-popup)
(require 'seq)
(require 'subr-x)
(require 'hermes-chat)
(autoload 'hermes-request "hermes-request")
(require 'hermes-sessions)
(require 'hermes-projects)
(require 'hermes-inventory)
(require 'hermes-rollback)
(require 'hermes-subagents)
(require 'hermes-cron)
(require 'hermes-profiles)
(require 'hermes-messaging)
(require 'hermes-kanban)
(require 'hermes-mcp)
(require 'hermes-config)
(require 'hermes-plugins)
(require 'hermes-system)
(require 'hermes-admin)
(require 'hermes-command-palette)
(require 'hermes-browser)
(autoload 'hermes-files "hermes-files" nil t)
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

(defvar-local hermes-dashboard--auth-request-token nil
  "Identity of the newest credential check for this dashboard buffer.")

(defvar hermes-dashboard--refresh-timer nil
  "Timer used to debounce dashboard refreshes.")

(defvar-local hermes-dashboard--stale-refresh-timer nil
  "Timer used to refresh stale status display in this dashboard buffer.")

(defvar hermes-dashboard-mode-map)

(defun hermes-dashboard--chat-unavailable-p (&optional predicate)
  "Return non-nil without a selected chat or when PREDICATE rejects it."
  (let* ((node (hermes-dashboard--node-at-point))
         (data (and node (ewoc-data node)))
         (buffer (plist-get data :buffer)))
    (or (not (eq (plist-get data :kind) 'chat))
        (not (buffer-live-p buffer))
        (and predicate (with-current-buffer buffer (funcall predicate))))))

(defun hermes-dashboard--popup-title ()
  "Identify the selected dashboard chat without fetching remote state."
  (let* ((node (hermes-dashboard--node-at-point))
         (data (and node (ewoc-data node))))
    (concat "Dashboard: "
            (if (hermes-dashboard--chat-unavailable-p)
                "no chat selected"
              (propertize (or (plist-get data :title)
                              (buffer-name (plist-get data :buffer)))
                          'face 'font-lock-type-face)))))

(keymap-popup-define hermes-dash-chat-map
  "Commands for the selected dashboard chat."
  :description #'hermes-dashboard--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Selected chat"
  "i" ("Interrupt" hermes-dashboard-interrupt
       :inapt-if (lambda () (hermes-dashboard--chat-unavailable-p
                            #'hermes-chat--interrupt-unavailable-p)))
  "s" ("Steer / send" hermes-dashboard-steer
       :inapt-if #'hermes-dashboard--chat-unavailable-p)
  "a" ("Answer prompt" hermes-dashboard-respond
       :inapt-if (lambda () (hermes-dashboard--chat-unavailable-p
                            (lambda () (not (hermes-chat--pending-prompt-p))))))
  "m" ("Switch model" hermes-dashboard-switch-model
       :inapt-if (lambda () (hermes-dashboard--chat-unavailable-p
                            #'hermes-chat--active-turn-p)))
  "d" ("Disconnect" hermes-dashboard-disconnect
       :inapt-if #'hermes-dashboard--chat-unavailable-p))

(put 'hermes-dash-chat-map-popup 'command-modes '(hermes-dashboard-mode))

(keymap-popup-define hermes-dash-res-map
  "Browse Hermes resources and work."
  :description #'hermes-dashboard--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Resources"
  "I" ("Inventory" hermes-list-inventory)
  "T" ("Projects" hermes-list-projects)
  "O" ("Managed files" hermes-files)
  "X" ("MCP servers" hermes-list-mcp)
  :group "Work"
  "K" ("Kanban" hermes-list-kanban)
  "A" ("Subagents" hermes-list-subagents)
  "C" ("Cron jobs" hermes-list-crons)
  "R" ("Rollbacks" hermes-list-rollbacks))

(put 'hermes-dash-res-map-popup 'command-modes '(hermes-dashboard-mode))

(keymap-popup-define hermes-dash-mgr-map
  "Configure Hermes and its access routes."
  :description #'hermes-dashboard--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "Manage"
  "F" ("Profiles" hermes-list-profiles)
  "Z" ("Configuration" hermes-config)
  "J" ("Agent plugins" hermes-list-plugins)
  "M" ("Messaging" hermes-list-messaging-platforms)
  :group "Access"
  "e" ("Connect provider" hermes-onboarding-connect-provider)
  "o" ("Provider accounts" hermes-onboarding-oauth-connect)
  "B" ("Pairing" hermes-list-pairing)
  "W" ("Webhooks" hermes-list-webhooks))

(put 'hermes-dash-mgr-map-popup 'command-modes '(hermes-dashboard-mode))

(keymap-popup-define hermes-dash-sys-map
  "Inspect the Hermes gateway."
  :description #'hermes-dashboard--popup-title
  :popup-key "?"
  :exit-key "q"
  :group "System"
  "G" ("Gateway status" hermes-system-status)
  "L" ("Gateway logs" hermes-system-logs))

(put 'hermes-dash-sys-map-popup 'command-modes '(hermes-dashboard-mode))

(keymap-popup-define hermes-dashboard-mode-map
  "Hermes Dashboard"
  :parent special-mode-map
  :popup-key "h"
  :description #'hermes-dashboard--popup-title
  :group "Navigate"
  "n" ("Next" hermes-dashboard-next :stay-open t)
  "p" ("Previous" hermes-dashboard-previous :stay-open t)
  "RET" ("Open" hermes-dashboard-open)
  :group "Chats"
  "c" ("New chat" hermes-chat)
  "S" ("Sessions" hermes-list-sessions)
  "v" ("Selected chat" :keymap hermes-dash-chat-map
       :inapt-if #'hermes-dashboard--chat-unavailable-p)
  :row
  :group "Tools"
  "b" ("Browse" :keymap hermes-dash-res-map)
  "z" ("Manage" :keymap hermes-dash-mgr-map)
  "!" ("System" :keymap hermes-dash-sys-map)
  "P" ("Command palette" hermes-command-palette)
  :group "View"
  "g" ("Refresh" hermes-dashboard-refresh :stay-open t)
  "?" ("Help" hermes-dashboard-popup))

(dolist (command '(hermes-dashboard-mode-map-popup
                  hermes-dashboard-mode-map--enter-hermes-dash-chat-map
                  hermes-dashboard-mode-map--enter-hermes-dash-res-map
                  hermes-dashboard-mode-map--enter-hermes-dash-mgr-map
                  hermes-dashboard-mode-map--enter-hermes-dash-sys-map))
  (put command 'command-modes '(hermes-dashboard-mode)))

;; Preserve direct shortcuts and user bindings; children own the action list.
(dolist (map (list hermes-dash-chat-map hermes-dash-res-map
                   hermes-dash-mgr-map hermes-dash-sys-map))
  (map-keymap
   (lambda (event binding)
     (when (and (symbolp binding)
                (not (lookup-key hermes-dashboard-mode-map (vector event))))
       (define-key hermes-dashboard-mode-map (vector event) binding)))
   map)
  ;; The minimum supported popup release cannot dispatch ancestor launchers
  ;; after leaving a child.  Keep back navigation native, as in chat menus.
  (dolist (key '("v" "b" "z" "!"))
    (unless (keymap-lookup map key)
      (keymap-set map key #'hermes-chat--submenu-root-key))))

(unless (keymap-lookup hermes-dashboard-mode-map "<mouse-1>")
  (keymap-set hermes-dashboard-mode-map "<mouse-1>" #'hermes-dashboard-mouse-open))

;; Standard palette alias; `P' remains the sole popup entry.
(keymap-set hermes-dashboard-mode-map "C-c C-p" #'hermes-command-palette)

(defun hermes-dashboard--header-line ()
  "Return the dashboard header line."
  (let ((chat-count 0)
        (separator (propertize "  |  " 'face 'hermes-dashboard-muted)))
    (when (hash-table-p hermes-dashboard--nodes)
      (maphash (lambda (_id node)
                 (when (eq (plist-get (ewoc-data node) :kind) 'chat)
                   (setq chat-count (1+ chat-count))))
               hermes-dashboard--nodes))
    (concat
     " "
     (propertize "Hermes Dashboard" 'face 'hermes-dashboard-heading)
     separator
     (propertize (format "%d live chat%s" chat-count
                         (if (= chat-count 1) "" "s"))
                 'face 'hermes-dashboard-title)
     separator
     (propertize "g refresh  ? help" 'face 'hermes-dashboard-muted)
     " ")))

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

(defun hermes-dashboard--activity-summary (text)
  "Return a single subdued line of activity TEXT for a dashboard card."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (width (if windows (apply #'min (mapcar #'window-body-width windows))
                  (window-body-width))))
    (propertize
     (truncate-string-to-width
      (string-join (split-string text "[[:space:]]+" t) " ")
      (max 1 (min 72 (- width 6))) nil nil t)
     'face 'hermes-dashboard-muted)))

(defun hermes-dashboard--format-chat-detail (node)
  "Return detail strings for chat dashboard NODE."
  (let* ((activity (hermes-dashboard--nonempty-string
                    (plist-get node :activity)))
         (connection (hermes-dashboard--nonempty-string
                      (plist-get node :connection)))
         (instance (and (hermes-instance-multiple-p)
                        (not (plist-get node :instance-grouped-p))
                        (hermes-dashboard--nonempty-string
                         (plist-get node :instance))))
         (session-id (hermes-dashboard--nonempty-string
                      (plist-get node :session-id)))
         (active-tools (plist-get node :active-tools))
         (pending-prompts (or (plist-get node :pending-prompts) 0))
         (tools (and active-tools
                     (format "%d active tool%s · RET open for details"
                             (length active-tools)
                             (if (= (length active-tools) 1) "" "s"))))
         (prompts (and (> pending-prompts 0)
                       (format "%d pending prompt%s"
                               pending-prompts
                               (if (= pending-prompts 1) "" "s")))))
    (delq nil
          (list (and activity (hermes-dashboard--activity-summary activity))
                (and connection (propertize connection 'face 'hermes-dashboard-muted))
                (and instance (format "instance %s" instance))
                (and session-id (propertize (format "session %s" session-id)
                                            'face 'hermes-dashboard-muted))
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
  (when-let* ((heading (hermes-dashboard--nonempty-string
                        (plist-get node :instance-heading))))
    (insert "\n" (propertize heading 'face 'hermes-dashboard-heading) "\n"))
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
     start node (string-join
                 (delq nil (append (list (format "Open %s" title)
                                          (plist-get node :activity))
                                    (plist-get node :active-tools)))
                 "\n")
     'hermes-dashboard-buffer buffer)))

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
               :subtitle "Open a new Hermes chat"
               :action #'hermes-chat))))

(defun hermes-dashboard--check-auth (&optional force)
  "Surface an onboarding card when a live connection reports no usable provider.
Only runs against an existing live chat client -- it never spawns a transient
connection just to check, so opening the dashboard stays passive.  FORCE allows
an authentication-change callback to use a transient client.  Branches on the
result `ok' flag because `setup.runtime_check' reports a credential failure as
`ok' nil, not a JSON-RPC error."
  (when (and (hermes-instance-context)
             (or force (hermes-browser--existing-client)))
    (let ((buffer (current-buffer))
          (token (setq hermes-dashboard--auth-request-token (list 'auth))))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-setup-runtime-check client))
       (lambda (result)
         (when (and (eq buffer (hermes-buffer--find
                                hermes-dashboard-buffer-name
                                'hermes-dashboard-mode))
                    (eq token
                        (buffer-local-value
                         'hermes-dashboard--auth-request-token buffer)))
           (with-current-buffer buffer
             (let ((needs-onboarding
                    (not (eq (hermes-transport--get result 'ok) t))))
               (unless (eq needs-onboarding
                           hermes-dashboard--needs-onboarding)
                 (setq hermes-dashboard--needs-onboarding needs-onboarding)
                 (hermes-dashboard-refresh))))))))))

(defun hermes-dashboard--warm-profile-cache ()
  "Warm the profile cache that feeds the chat picker, unless already fresh.
Best-effort and asynchronous: opening the dashboard never blocks or errors on
this, mirroring `hermes-dashboard--check-auth'.  A live chat client's session
token is used when one exists, else plain REST auth.  The cache is keyed by
dashboard URL, so it re-fetches automatically after the configured URL changes."
  (when-let* ((instance (hermes-instance-context)))
    (let ((hermes-instance instance)
          (hermes-dashboard-transport-url (hermes-instance-url instance)))
      (unless (hermes-dashboard-transport-cached-profile-list)
        (hermes--promise-catch
         (hermes-dashboard-transport-profile-list-async
          (hermes-chat--existing-dashboard-client))
         #'ignore)))))

(defun hermes-dashboard--chat-node (buffer)
  "Return one chat dashboard node for BUFFER."
  (with-current-buffer buffer
    (let* ((snapshot (hermes-chat--dashboard-snapshot))
           (stale-p (hermes-dashboard--stale-chat-p snapshot))
           (status (if stale-p 'stale (plist-get snapshot :status))))
      (append (list :id (format "chat:%s" (buffer-name buffer))
                    :kind 'chat)
              (plist-put snapshot :status status)
              (list :status-label (hermes-dashboard--status-label status)
                    :stale-p stale-p)))))

(defun hermes-dashboard--instance-group (heading nodes)
  "Return NODES marked as one instance group headed by HEADING."
  (when nodes
    (cons (append (car nodes)
                  (list :instance-grouped-p t :instance-heading heading))
          (mapcar (lambda (node)
                    (append node '(:instance-grouped-p t)))
                  (cdr nodes)))))

(defun hermes-dashboard--node-instance-p (node instance)
  "Return non-nil when chat NODE belongs to INSTANCE."
  (if-let* ((id (plist-get node :instance-id)))
      (equal id (hermes-instance-id instance))
    (equal (plist-get node :instance) (hermes-instance-name instance))))

(defun hermes-dashboard--group-chat-nodes (nodes)
  "Group chat NODES by configured instance when multiple are available."
  (if (not (hermes-instance-multiple-p))
      nodes
    (let* ((instances (hermes-instance-configured))
           (known
            (apply #'append
                   (mapcar
                    (lambda (instance)
                      (hermes-dashboard--instance-group
                       (hermes-instance-name instance)
                       (seq-filter
                        (lambda (node)
                          (hermes-dashboard--node-instance-p node instance))
                        nodes)))
                    instances)))
           (other
            (seq-remove
             (lambda (node)
               (seq-some (lambda (instance)
                           (hermes-dashboard--node-instance-p node instance))
                         instances))
             nodes)))
      (append known (hermes-dashboard--instance-group "Other" other)))))

(defun hermes-dashboard--empty-node ()
  "Return the dashboard empty-state node."
  (list :id "empty:chats"
        :kind 'empty
        :title "No live Hermes chat buffers"
        :subtitle "Press c to open Chat."))

(defun hermes-dashboard--collect-nodes ()
  "Return dashboard nodes for actions and live chat buffers."
  (let ((chat-nodes
         (hermes-dashboard--group-chat-nodes
          (mapcar #'hermes-dashboard--chat-node
                  (hermes-dashboard--chat-buffers)))))
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

(defun hermes-dashboard--reader-anchor (position)
  "Return the card identity, index, line and column at POSITION."
  (save-excursion
    (goto-char position)
    (let* ((located (hermes-dashboard--node-at-point))
           (node (and located (<= (ewoc-location located) position) located))
           (id (and node (hermes-dashboard--node-id (ewoc-data node)))))
      (list id (cl-position id (hermes-dashboard--current-ids) :test #'equal)
            (if node (count-lines (ewoc-location node) (line-beginning-position)) 0)
            (current-column) position))))

(defun hermes-dashboard--reader-position (anchor)
  "Resolve ANCHOR, using its bounded card index if its card disappeared."
  (let* ((node (or (gethash (car anchor) hermes-dashboard--nodes)
                   (and (nth 1 anchor)
                        (ewoc-nth hermes-dashboard--ewoc
                                  (min (nth 1 anchor)
                                       (1- (hash-table-count hermes-dashboard--nodes)))))))
         (next (and node (ewoc-next hermes-dashboard--ewoc node))))
    (save-excursion
      (if (not node)
          (goto-char (min (point-max) (nth 4 anchor)))
        (save-restriction
          (narrow-to-region (ewoc-location node)
                            (if next (1- (ewoc-location next)) (point-max)))
          (goto-char (point-min))
          (forward-line (nth 2 anchor))
          (move-to-column (nth 3 anchor))))
      (point))))

(defun hermes-dashboard--rebuild-ewoc (nodes)
  "Rebuild the dashboard EWOC from NODES."
  (setq hermes-dashboard--nodes (make-hash-table :test #'equal))
  (let ((inhibit-read-only t))
    (hermes-dashboard--clear-ewoc)
    (dolist (node-data nodes)
      (let ((node (ewoc-enter-last hermes-dashboard--ewoc node-data)))
        (puthash (hermes-dashboard--node-id node-data)
                 node hermes-dashboard--nodes)))))

(defun hermes-dashboard--sync-ewoc (nodes)
  "Synchronize NODES, preserving each dashboard reader's card and viewport."
  (let ((current-ids (hermes-dashboard--current-ids))
        (new-ids (mapcar #'hermes-dashboard--node-id nodes))
        (anchor (hermes-dashboard--reader-anchor (point)))
        (windows (mapcar
                  (lambda (window)
                    (list window
                          (hermes-dashboard--reader-anchor (window-start window))
                          (hermes-dashboard--reader-anchor (window-point window))))
                  (get-buffer-window-list (current-buffer) nil t))))
    (unwind-protect
        (if (not (equal current-ids new-ids))
            (hermes-dashboard--rebuild-ewoc nodes)
          (dolist (node-data nodes)
            (let* ((id (hermes-dashboard--node-id node-data))
                   (node (gethash id hermes-dashboard--nodes)))
              (when (not (equal node-data (ewoc-data node)))
                (let ((inhibit-read-only t))
                  (ewoc-set-data node node-data)
                  (ewoc-invalidate hermes-dashboard--ewoc node))))))
      (goto-char (hermes-dashboard--reader-position anchor))
      (dolist (state windows)
        (when (and (window-live-p (car state))
                   (eq (window-buffer (car state)) (current-buffer)))
          (set-window-point (car state)
                            (hermes-dashboard--reader-position (nth 2 state)))
          (set-window-start (car state)
                            (hermes-dashboard--reader-position (nth 1 state)) t))))))

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
  (interactive nil hermes-dashboard-mode)
  (unless (derived-mode-p 'hermes-dashboard-mode)
    (user-error "Not in a Hermes dashboard buffer"))
  (hermes-dashboard--ensure-ewoc)
  (hermes-dashboard--sync-ewoc (hermes-dashboard--collect-nodes))
  (force-mode-line-update))

(defun hermes-dashboard-next (&optional arg)
  "Move to the ARGth next dashboard card."
  (interactive "p" hermes-dashboard-mode)
  (unless hermes-dashboard--ewoc
    (user-error "No dashboard EWOC in this buffer"))
  (condition-case nil
      (ewoc-goto-next hermes-dashboard--ewoc (or arg 1))
    (error (user-error "No next dashboard card"))))

(defun hermes-dashboard-previous (&optional arg)
  "Move to the ARGth previous dashboard card."
  (interactive "p" hermes-dashboard-mode)
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
  (interactive nil hermes-dashboard-mode)
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
  (interactive "e" hermes-dashboard-mode)
  (mouse-set-point event)
  (hermes-dashboard-open))

(defun hermes-dashboard-popup ()
  "Show the dashboard keymap popup."
  (interactive nil hermes-dashboard-mode)
  (keymap-popup hermes-dashboard-mode-map))

(defun hermes-dashboard-interrupt ()
  "Interrupt the Hermes chat selected by the dashboard card at point."
  (interactive nil hermes-dashboard-mode)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-interrupt)))

(defun hermes-dashboard-steer ()
  "Steer the Hermes chat selected by the dashboard card at point."
  (interactive nil hermes-dashboard-mode)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-steer-message)))

(defun hermes-dashboard-respond ()
  "Respond to a prompt in the Hermes chat selected at point."
  (interactive nil hermes-dashboard-mode)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-respond-to-prompt)))

(defun hermes-dashboard-switch-model ()
  "Switch the model for the Hermes chat selected at point."
  (interactive nil hermes-dashboard-mode)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-switch-model)))

(defun hermes-dashboard-disconnect ()
  "Disconnect the Hermes chat session selected by the dashboard card at point."
  (interactive nil hermes-dashboard-mode)
  (with-current-buffer (hermes-dashboard--selected-chat-buffer)
    (call-interactively #'hermes-chat-disconnect)))

(defun hermes-dashboard-refresh-visible ()
  "Refresh the visible Hermes dashboard buffer, when it exists."
  (when-let* ((buffer (hermes-buffer--find hermes-dashboard-buffer-name
                                           'hermes-dashboard-mode)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'hermes-dashboard-mode)
          (hermes-dashboard-refresh))))))

(defun hermes-dashboard--provider-auth-changed ()
  "Refresh dashboard state after changing provider authentication."
  (when-let* ((buffer (hermes-buffer--find hermes-dashboard-buffer-name
                                           'hermes-dashboard-mode)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'hermes-dashboard-mode)
          (hermes-dashboard-refresh)
          (hermes-dashboard--check-auth t))))))

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
                 (not (hermes-buffer--retired-p))
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
  (add-hook 'after-set-visited-file-name-hook
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

(hermes-chat-register-state-change-function
 #'hermes-dashboard--schedule-refresh)
(hermes-onboarding-set-auth-changed-function
 #'hermes-dashboard--provider-auth-changed)

(defun hermes--managed-buffer-p (buffer)
  "Return non-nil if BUFFER is still explicitly owned by Hermes."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (hermes-buffer--owned-p))))

(defun hermes--managed-buffers ()
  "Return every live buffer still explicitly owned by Hermes."
  (seq-filter #'hermes--managed-buffer-p (buffer-list)))

(defun hermes--kill-managed-buffers (claims)
  "Kill buffers retaining the exact captured CLAIMS and return their count.
CLAIMS is an alist of buffer objects to their ownership occurrences."
  (let ((killed 0))
    (dolist (claim claims killed)
      (let ((buffer (car claim)))
        (when (and (hermes--managed-buffer-p buffer)
                   (eq (cdr claim) (buffer-local-value 'hermes-buffer--owner buffer))
                   (kill-buffer buffer))
          (cl-incf killed))))))

(defun hermes--call-if-defined (function)
  "Call FUNCTION when it is defined."
  (when (fboundp function)
    (funcall function)))

;;;###autoload
(defun hermes-close ()
  "Close local Hermes state so the frontend can restart cleanly.
Stop optional capability and eval services, kill still-owned views and chats,
and force-stop remaining shared dashboard clients.  Durable Hermes
sessions and backend data are preserved."
  (interactive)
  (let* ((buffers (hermes--managed-buffers))
         (claims (mapcar (lambda (buffer)
                           (cons buffer (buffer-local-value 'hermes-buffer--owner buffer)))
                         buffers)))
    (when (yes-or-no-p
           (format "Close Hermes connections and kill %d buffer%s? "
                   (length buffers)
                   (if (= (length buffers) 1) "" "s")))
      (mapc #'hermes--call-if-defined
            '(hermes-capabilities-stop hermes-exec-stop))
      (let* ((killed (hermes--kill-managed-buffers claims))
             (connections
              (hermes-dashboard-transport-stop-all
               "Hermes closed for restart")))
        (message "Hermes closed: %d buffer%s, %d connection%s"
                 killed (if (= killed 1) "" "s")
                 connections (if (= connections 1) "" "s"))))))

;;;###autoload
(defun hermes ()
  "Open the Hermes dashboard."
  (interactive)
  (let ((buffer (hermes-buffer--get hermes-dashboard-buffer-name #'hermes-dashboard-mode)))
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
