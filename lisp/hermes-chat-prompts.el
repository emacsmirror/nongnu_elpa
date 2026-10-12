;;; hermes-chat-prompts.el --- Prompt and approval responses for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; Pending-prompt state and approval/clarify/sudo/secret response handling for
;; `hermes-chat'.  Backend prompt-request events are recorded here, optionally
;; auto-prompted in a visible chat buffer, and answered through the dashboard
;; transport.  This module preserves the existing `hermes-chat--*' symbols and
;; the public commands `hermes-chat-respond-to-prompt' and
;; `hermes-chat-cancel-prompt' while isolating prompt-specific code.  The chat
;; facade requires it after `hermes-chat-buffer' and before
;; `hermes-chat-dashboard'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-format)
(require 'hermes-chat-buffer)

(defvar websocket-debug)

(defvar hermes-chat--prompt-control-client-function nil
  "Function returning the dashboard client for an unowned prompt response.
Installed by the dashboard session owner; called without arguments.")

(defcustom hermes-chat-auto-prompt-requests t
  "Whether visible chat buffers should prompt for backend input requests.
When non-nil, approvals, sudo passwords, secrets, and browser-vault requests
automatically open the usual minibuffer prompt in a visible interactive chat.
Clarifications wait for the chat input or `hermes-chat-respond-to-prompt'.
Invisible buffers and batch sessions record every prompt and show a message."
  :type 'boolean
  :group 'hermes)

(defvar hermes-chat--auto-prompting-p nil
  "Non-nil while an automatic minibuffer prompt is reading a response.")

(defvar hermes-chat--prompt-user-invoked-p nil
  "Non-nil while the user explicitly opened the current prompt reader.
Only real user boundaries bind this: interactive invocation of
`hermes-chat-respond-to-prompt' and the transcript Answer button.  It
opts approval readers into one-key answers; every other caller gets a
reader that requires an explicit RET submission.")

(defvar hermes-chat--prompt-indicator-buffers nil
  "Chat buffers with pending prompt requests, oldest first.
The global prompt indicator reads this registry rather than scanning every
buffer on redisplay.  Prompt mutations keep it current.")

(defvar hermes-chat-prompt-indicator-mode-line
  '(:eval (hermes-chat--prompt-indicator-string))
  "Mode-line construct showing pending Hermes chat prompts.")
(put 'hermes-chat-prompt-indicator-mode-line 'risky-local-variable t)

(defun hermes-chat--set-prompt-indicator (symbol value)
  "Set SYMBOL to VALUE and add or remove the global prompt indicator.
Install the segment when VALUE is non-nil, even before any prompt arrives,
so chats that already wait for input show up at once."
  (set-default symbol value)
  (if value
      (hermes-chat--prompt-indicator-install)
    (hermes-chat--prompt-indicator-uninstall))
  (force-mode-line-update t))

(defcustom hermes-chat-prompt-indicator t
  "Whether pending chat prompts are shown in the global mode line.
When non-nil, `global-mode-string' shows a segment such as
\"[Hermes: Clarify]\" while any chat awaits an approval, clarification or
other input, so a prompt stays noticeable when its chat is not selected.
The segment names only the prompt kind and count, never prompt contents.
Clicking it answers from the owning chat.  Setting this option with
Customize also adds or removes the segment.  The previous value of
`global-mode-string' keeps its rendering, and removal takes out only the
segment."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set #'hermes-chat--set-prompt-indicator
  :group 'hermes)

(defvar hermes-chat--reset-clarify-owner-sink nil
  "Dynamically bound holder for clarifications accepted during reset.")

(defun hermes-chat--prompt-request-event-p (event)
  "Return non-nil when EVENT is a dashboard prompt request."
  (and (eq (plist-get event :type) 'status)
       (plist-get event :prompt-request-p)))

(defun hermes-chat--prompt-expire-event-p (event)
  "Return non-nil when EVENT expires a dashboard prompt request."
  (and (eq (plist-get event :type) 'status)
       (plist-get event :prompt-expire-p)))

(defun hermes-chat--ensure-pending-prompts ()
  "Return the current buffer's pending prompt table."
  (or hermes-chat--pending-prompts
      (setq hermes-chat--pending-prompts (make-hash-table :test #'equal))))

(defun hermes-chat--prompt-event-type (event)
  "Return EVENT's prompt type string, or nil."
  (hermes-chat--event-string event '(:prompt-type :prompt_type)))

(defun hermes-chat--prompt-event-key (event)
  "Return the stable pending-prompt key for EVENT."
  (or (hermes-chat--event-string event '(:request-id :request_id))
      (and-let* ((type (hermes-chat--prompt-event-type event)))
        (format "%s:%s" type
                (or (hermes-chat--event-string event '(:session-id :session_id))
                    "global")))))

(defun hermes-chat--approval-prompt-p (prompt)
  "Return non-nil when PROMPT is an approval request."
  (equal (hermes-chat--prompt-event-type prompt) "approval"))

(defun hermes-chat--prepare-prompt-request (event key assistant-id)
  "Return EVENT prepared for prompt state under KEY and ASSISTANT-ID."
  (let ((prompt (plist-put (copy-sequence event) :prompt-key key)))
    (setq prompt (plist-put prompt :prompt-content
                            (plist-get prompt :content)))
    (when assistant-id
      (setq prompt (plist-put prompt :assistant-id assistant-id)))
    prompt))

(defun hermes-chat--prompt-without-response-token (prompt)
  "Return PROMPT without its client-local response token."
  (cl-loop for (key value) on prompt by #'cddr
           unless (eq key :response-token)
           append (list key value)))

(defun hermes-chat--equivalent-prompt-replay-p (existing prompt)
  "Return non-nil when EXISTING and PROMPT differ only by response token."
  (equal (hermes-chat--prompt-without-response-token existing)
         (hermes-chat--prompt-without-response-token prompt)))

(defun hermes-chat--approval-prompt-with-queue (queue)
  "Return the oldest approval prompt in QUEUE with count metadata."
  (let* ((prompt (copy-sequence (car queue)))
         (count (length queue))
         (content (or (plist-get prompt :prompt-content)
                      (plist-get prompt :content))))
    (setq prompt (plist-put prompt :prompt-queue queue))
    (setq prompt (plist-put prompt :prompt-count count))
    (plist-put prompt :content
               (if (> count 1)
                   (format "%s (%d pending approvals)" content count)
                 content))))

(defun hermes-chat--reconcile-batch-prompt (existing prompt)
  "Merge accepted answers from PROMPT into the same live EXISTING request.
Keep locally accepted answers absent from a replay and preserve in-flight
response ownership.  Released snapshots have no revision or unlock operation."
  (when-let* ((request (plist-get existing :server-request))
              ((eq request (plist-get prompt :server-request)))
              ((hermes-dashboard-transport-server-request-current-p request))
              ((hermes-chat--batch-clarify-p existing)))
    (let* ((incoming (hermes-chat--batch-clarify-answer-alist prompt))
           (answers (append incoming
                            (cl-remove-if
                             (lambda (answer) (assoc (car answer) incoming))
                             (hermes-chat--batch-clarify-answer-alist existing))))
           (next (plist-put (copy-sequence existing) :answers answers))
           (content (hermes-dashboard-transport--batch-clarify-content next)))
      (plist-put (plist-put next :content content) :prompt-content content))))

(defun hermes-chat--record-prompt-request (event assistant-id)
  "Record prompt request EVENT for ASSISTANT-ID and return display event."
  (if-let* ((key (hermes-chat--prompt-event-key event)))
      (let* ((prompt (hermes-chat--prepare-prompt-request
                      event key assistant-id))
             (table (hermes-chat--ensure-pending-prompts))
             (existing (gethash key table))
             (reconciled (hermes-chat--reconcile-batch-prompt existing prompt))
             (stored (or reconciled
                         (if (and existing (hermes-chat--approval-prompt-p prompt))
                             (hermes-chat--approval-prompt-with-queue
                              (append (plist-get existing :prompt-queue)
                                      (list prompt)))
                           (if (hermes-chat--approval-prompt-p prompt)
                               (hermes-chat--approval-prompt-with-queue
                                (list prompt))
                             prompt))))
             (token (and existing
                         (or (and (hermes-chat--approval-prompt-p existing)
                                  (hermes-chat--approval-prompt-p prompt))
                             (hermes-chat--equivalent-prompt-replay-p
                              existing stored))
                         (plist-get existing :response-token))))
        (when (and existing (hash-table-p hermes-chat--auto-prompt-keys))
          (remhash key hermes-chat--auto-prompt-keys))
        (when token
          (setq stored (plist-put stored :response-token token)))
        (puthash key stored table)
        (hermes-chat--prompt-indicator-sync)
        stored)
    event))

(defun hermes-chat--prompt-expiry-matches-p (prompt event)
  "Return non-nil when expiry EVENT owns pending PROMPT exactly."
  (and prompt
       (equal (hermes-chat--prompt-event-type prompt)
              (hermes-chat--prompt-event-type event))
       (equal (hermes-chat--event-string prompt '(:request-id :request_id))
              (hermes-chat--event-string event '(:request-id :request_id)))
       (equal (hermes-chat--event-string prompt '(:session-id :session_id))
              (hermes-chat--event-string event '(:session-id :session_id)))))

(defun hermes-chat--expire-pending-prompt (event)
  "Remove the exact pending prompt owned by expiry EVENT."
  (when-let* ((key (hermes-chat--event-string event '(:request-id :request_id)))
              ((hermes-chat--event-string event '(:session-id :session_id)))
              (prompt (and (hash-table-p hermes-chat--pending-prompts)
                           (gethash key hermes-chat--pending-prompts)))
              ((hermes-chat--prompt-expiry-matches-p prompt event)))
    (when-let* (((hermes-chat--clarify-prompt-p prompt))
                (token (plist-get prompt :response-token))
                (owner (seq-find
                        (lambda (owner)
                          (eq (plist-get owner :response-token) token))
                        hermes-chat--retained-clarify-owners)))
      ;; Keep both authorities until the answer has a recoverable projection.
      (hermes-chat--restore-retained-clarify (list :retained-owner owner)))
    (remhash key hermes-chat--pending-prompts)
    (when (hash-table-p hermes-chat--auto-prompt-keys)
      (remhash key hermes-chat--auto-prompt-keys))
    (hermes-chat--prompt-indicator-sync)
    (hermes-chat--notify-state-change)
    (unless (hermes-chat--show-pending-prompt-state)
      (hermes-chat--set-header-state
       :status (if (hermes-chat--active-turn-p) 'running 'ready)
       :activity (plist-get event :content)))
    t))

(defun hermes-chat--ensure-auto-prompt-keys ()
  "Return the current buffer's scheduled auto-prompt key table."
  (or hermes-chat--auto-prompt-keys
      (setq hermes-chat--auto-prompt-keys (make-hash-table :test #'equal))))

(defun hermes-chat--release-auto-prompt-claim (key &optional claim)
  "Release KEY's auto-prompt CLAIM when it remains current.
With nil CLAIM, release any claim for KEY."
  (when (and (hash-table-p hermes-chat--auto-prompt-keys)
             (or (null claim)
                 (eq (gethash key hermes-chat--auto-prompt-keys) claim)))
    (remhash key hermes-chat--auto-prompt-keys)))

(defun hermes-chat--prompt-owner-context (key &optional prompt claim)
  "Return current ownership context for KEY, PROMPT, and scheduling CLAIM."
  (list :client hermes-chat--dashboard-client
        :session-id hermes-chat--dashboard-active-session-id
        :generation hermes-chat--lifecycle-generation
        :prompts hermes-chat--pending-prompts
        :key key
        :prompt prompt
        :claim claim))

(defun hermes-chat--prompt-owner-current-p (context)
  "Return non-nil when prompt owner CONTEXT still owns this chat."
  (let ((client (plist-get context :client))
        (key (plist-get context :key))
        (expected (plist-get context :prompt))
        (claim (plist-get context :claim)))
    (and (eq hermes-chat--dashboard-client client)
         (hermes-dashboard-transport-client-p client)
         (hermes-dashboard-transport-client-websocket client)
         (equal hermes-chat--dashboard-active-session-id
                (plist-get context :session-id))
         (eql hermes-chat--lifecycle-generation (plist-get context :generation))
         (eq hermes-chat--pending-prompts (plist-get context :prompts))
         (when-let* ((prompt (gethash key hermes-chat--pending-prompts)))
           (and (or (null expected) (eq prompt expected))
                (or (not (plist-get prompt :server-request))
                    (hermes-dashboard-transport-server-request-current-p
                     (plist-get prompt :server-request)))
                (or (null claim)
                    (and (hash-table-p hermes-chat--auto-prompt-keys)
                         (eq (gethash key hermes-chat--auto-prompt-keys)
                             claim))))))))

(defun hermes-chat--prompt-notice-text (prompt)
  "Return a safe one-line notice for PROMPT."
  (let ((summary (or (plist-get prompt :content)
                     (hermes-chat--prompt-display-name prompt))))
    (format "Hermes %s pending: %s"
            (downcase (hermes-chat--prompt-display-name prompt))
            (string-trim
             (truncate-string-to-width (or summary "") 96 nil nil "…")))))

(defun hermes-chat--auto-prompt-schedulable-p (buffer)
  "Return non-nil if BUFFER may schedule an automatic prompt."
  (and hermes-chat-auto-prompt-requests
       (not noninteractive)
       (get-buffer-window buffer t)))

(defun hermes-chat--run-auto-prompt (buffer key context)
  "Prompt for KEY in BUFFER while prompt owner CONTEXT remains current."
  (hermes-chat--in-buffer buffer
    (when (hermes-chat--prompt-owner-current-p context)
      (when-let* ((prompt (gethash key hermes-chat--pending-prompts)))
        (cond
         ((not (hermes-chat--auto-prompt-schedulable-p buffer))
          (hermes-chat--release-auto-prompt-claim
           key (plist-get context :claim)))
         ((or (hermes-chat--prompt-response-in-flight-p key)
              (not (zerop (minibuffer-depth))))
          (hermes-chat--release-auto-prompt-claim
           key (plist-get context :claim))
          (hermes-chat--schedule-auto-prompt prompt t 0.25))
         (t
          (hermes-chat--release-auto-prompt-claim
           key (plist-get context :claim))
          (condition-case err
              (let ((hermes-chat--auto-prompting-p t))
                (hermes-chat-respond-to-prompt key))
            (quit
             (message "Hermes prompt left pending: %s" key))
            (user-error
             (message "%s" (error-message-string err)))
            (error
             (message "Hermes auto prompt failed: %s"
                      (error-message-string err))))))))))

(defun hermes-chat--schedule-auto-prompt (prompt &optional quiet delay)
  "Announce PROMPT and schedule an automatic minibuffer response prompt.
When QUIET is non-nil, do not emit another echo-area notice.  DELAY is the
number of seconds to wait before trying to prompt."
  (when-let* ((key (plist-get prompt :prompt-key)))
    (unless quiet
      (message "%s (%s)"
               (hermes-chat--prompt-notice-text prompt)
               (cond
                ((hermes-chat--batch-clarify-p prompt)
                 (format "RET answers next question: %s; C-c C-a for all"
                         (or (hermes-transport--get
                              (car (hermes-chat--unanswered-batch-questions prompt))
                              'question)
                             "none pending")))
                ((hermes-chat--clarify-prompt-p prompt)
                 "answer in chat with RET or C-c C-a")
                (t "respond with C-c C-a"))))
    (when (and (not (equal (hermes-chat--prompt-event-type prompt) "clarify"))
               (hermes-chat--auto-prompt-schedulable-p (current-buffer)))
      (let ((scheduled (hermes-chat--ensure-auto-prompt-keys)))
        (unless (gethash key scheduled)
          (let ((claim (list key prompt)))
            (puthash key claim scheduled)
            (run-at-time (or delay 0) nil #'hermes-chat--run-auto-prompt
                         (current-buffer) key
                         (hermes-chat--prompt-owner-context
                          key prompt claim))))))))

(defun hermes-chat--prompt-session-match-p (prompt session-id)
  "Return non-nil if PROMPT belongs to SESSION-ID.
A nil SESSION-ID matches every prompt in the current buffer."
  (or (null session-id)
      (null (plist-get prompt :session-id))
      (equal (plist-get prompt :session-id) session-id)))

(defun hermes-chat--clear-pending-prompts (&optional session-id)
  "Remove pending prompt requests for SESSION-ID, or all when nil."
  (when (hash-table-p hermes-chat--pending-prompts)
    (let (keys)
      (maphash (lambda (key prompt)
                 (when (hermes-chat--prompt-session-match-p prompt session-id)
                   (push key keys)))
               hermes-chat--pending-prompts)
      (dolist (key keys)
        (remhash key hermes-chat--pending-prompts)
        (hermes-chat--release-auto-prompt-claim key))
      (when keys
        (hermes-chat--prompt-indicator-sync)
        (hermes-chat--notify-state-change)))))

(defun hermes-chat--pending-prompt-p ()
  "Return non-nil when the current chat has pending prompt requests."
  (and hermes-chat--pending-prompts
       (> (hash-table-count hermes-chat--pending-prompts) 0)))

(defun hermes-chat--pending-prompt-count ()
  "Return the number of pending prompt requests in the current chat."
  (let ((count 0))
    (when (hash-table-p hermes-chat--pending-prompts)
      (maphash (lambda (_key prompt)
                 (setq count
                       (+ count
                          (or (plist-get prompt :prompt-count)
                              (and (plist-get prompt :prompt-queue)
                                   (length (plist-get prompt :prompt-queue)))
                              1))))
               hermes-chat--pending-prompts))
    count))

(defun hermes-chat--pending-prompt-keys ()
  "Return pending prompt keys in deterministic order."
  (let (keys)
    (when (hash-table-p hermes-chat--pending-prompts)
      (maphash (lambda (key _prompt) (push key keys))
               hermes-chat--pending-prompts))
    (sort keys #'string<)))

(defun hermes-chat--select-pending-prompt-key (key)
  "Return KEY or interactively select a pending prompt key."
  (or key
      (pcase (hermes-chat--pending-prompt-keys)
        ('() (user-error "No pending Hermes prompt requests"))
        (`(,only) only)
        (keys (completing-read "Hermes prompt: " keys nil t)))))

(defun hermes-chat--pending-prompt (key)
  "Return pending prompt for KEY, or signal a user error."
  (or (and hermes-chat--pending-prompts
           (gethash key hermes-chat--pending-prompts))
      (user-error "No pending Hermes prompt request %s" key)))

(defun hermes-chat--pending-clarify-key ()
  "Return the sole pending clarification key, or nil."
  (pcase (hermes-chat--pending-prompt-keys)
    (`(,key)
     (and (equal (hermes-chat--prompt-event-type
                  (gethash key hermes-chat--pending-prompts))
                 "clarify")
          key))))

(defun hermes-chat--prompt-display-name (prompt)
  "Return display name for PROMPT."
  (pcase (hermes-chat--prompt-event-type prompt)
    ("approval" "Approval")
    ("clarify" "Clarify")
    ("sudo" "Sudo")
    ("secret" "Secret")
    ("vault.unlock_prompt" "Vault unlock")
    ("vault.save_login" "Save browser login")
    ("vault.code" "Browser verification code")
    ("terminal" "Terminal read")
    (_ "Prompt")))

(defun hermes-chat--prompt-entry-node (prompt)
  "Return the transcript node that rendered PROMPT, or nil."
  (when-let* ((assistant-id (plist-get prompt :assistant-id))
              (event-id (hermes-chat--transport-entry-id prompt)))
    (gethash (format "%s:%s" assistant-id event-id) hermes-chat--nodes)))

(defun hermes-chat--insert-prompt-status (prompt content status)
  "Insert CONTENT with STATUS immediately after PROMPT's transcript entry."
  (let* ((prompt-node (hermes-chat--prompt-entry-node prompt))
         (next-node (and prompt-node
                         (ewoc-next hermes-chat--ewoc prompt-node))))
    (hermes-chat--insert-entry
     (hermes-chat--make-entry 'status content status)
     next-node)))

;;;; Transcript prompt actions

(defvar-local hermes-chat--prompt-action-entries nil
  "Alist of (ENTRY-ID . PROMPT-KEY) rendered with a live Answer button.")

(defun hermes-chat--prompt-entry-key (entry)
  "Return the pending prompt key ENTRY's request still owns, or nil."
  (when-let* ((event (plist-get (plist-get entry :metadata) :event))
              ((plist-get event :prompt-request-p))
              (key (plist-get event :prompt-key))
              ((hash-table-p hermes-chat--pending-prompts))
              ((gethash key hermes-chat--pending-prompts)))
    key))

(defun hermes-chat--prompt-entry-button (button)
  "Reopen the reader for the pending prompt attached to BUTTON."
  (let ((hermes-chat--prompt-user-invoked-p t))
    (hermes-chat-respond-to-prompt
     (button-get button 'hermes-chat-prompt-key))))

(defun hermes-chat--insert-prompt-entry-actions (entry)
  "Insert an Answer button after ENTRY while its prompt is pending.
The button reopens the same native reader as \\[hermes-chat-respond-to-prompt],
so dismissing the minibuffer never loses the request."
  (when-let* ((key (hermes-chat--prompt-entry-key entry)))
    (insert "    ")
    (insert-text-button
     "[Answer]" 'face 'button 'follow-link t
     'help-echo "Reopen this prompt's reader"
     'hermes-chat-prompt-key key
     'action #'hermes-chat--prompt-entry-button)
    (insert " "
            (propertize
             (substitute-command-keys
              "\\<hermes-chat-mode-map>\\[hermes-chat-respond-to-prompt]")
             'face 'help-key-binding)
            "\n")
    (setf (alist-get (plist-get entry :id) hermes-chat--prompt-action-entries
                     nil nil #'equal)
          key)))

(defun hermes-chat--refresh-prompt-entry-actions ()
  "Repaint transcript entries whose Answer button no longer has a prompt."
  (let (stale)
    (pcase-dolist (`(,id . ,key) hermes-chat--prompt-action-entries)
      (unless (and (hash-table-p hermes-chat--pending-prompts)
                   (gethash key hermes-chat--pending-prompts))
        (push id stale)))
    (dolist (id stale)
      (setq hermes-chat--prompt-action-entries
            (assoc-delete-all id hermes-chat--prompt-action-entries))
      (when (and (hash-table-p hermes-chat--nodes) hermes-chat--ewoc)
        (hermes-chat--update-entry id #'identity t)))))

(add-hook 'hermes-chat--entry-suffix-functions
          #'hermes-chat--insert-prompt-entry-actions)

(defun hermes-chat--first-pending-prompt ()
  "Return the first pending prompt in deterministic key order."
  (and-let* ((key (car (hermes-chat--pending-prompt-keys))))
    (gethash key hermes-chat--pending-prompts)))

;;;; Global pending-prompt indicator

(defun hermes-chat--mode-line-segments-p (value)
  "Return non-nil if mode-line construct VALUE is a list of segments.
Such a list renders its elements in order, unlike a special construct
headed by a symbol or an integer."
  (and (proper-list-p value)
       (or (stringp (car value)) (consp (car value)))))

(defun hermes-chat--prompt-indicator-install ()
  "Add the pending-prompt segment to the default `global-mode-string'.
Append it to a list of segments, as other global segments do.  Wrap any
other value, such as a string or a special construct, in a fresh symbol
so it renders as before: a string held by a symbol is shown literally,
but a string inside a list is %-processed."
  (let ((value (default-value 'global-mode-string))
        (segment 'hermes-chat-prompt-indicator-mode-line))
    (cond
     ((null value)
      (set-default 'global-mode-string (list "" segment)))
     ((not (hermes-chat--mode-line-segments-p value))
      (let ((saved (make-symbol "hermes-saved-global-mode-string")))
        (set saved value)
        (put saved 'risky-local-variable t)
        (put saved 'hermes-chat--prompt-indicator-wrapper t)
        (set-default 'global-mode-string (list "" saved segment))))
     ((not (memq segment value))
      (set-default 'global-mode-string (append value (list segment)))))))

(defun hermes-chat--prompt-indicator-uninstall ()
  "Remove the pending-prompt segment from the default `global-mode-string'.
Keep other elements.  If only the wrapper made on install remains, restore
the value it holds; if nothing else remains, restore nil."
  (let ((value (default-value 'global-mode-string))
        (segment 'hermes-chat-prompt-indicator-mode-line))
    (when (and (hermes-chat--mode-line-segments-p value)
               (memq segment value))
      (let ((rest (remq segment value)))
        (set-default
         'global-mode-string
         (pcase rest
           ('("") nil)
           (`("" ,(and (pred symbolp) saved))
            (if (get saved 'hermes-chat--prompt-indicator-wrapper)
                (symbol-value saved)
              rest))
           (_ rest)))))))

(defun hermes-chat--prompt-indicator-forget ()
  "Remove the current buffer and dead buffers from the prompt indicator."
  (let ((buffer (current-buffer)))
    (when (memq buffer hermes-chat--prompt-indicator-buffers)
      (setq hermes-chat--prompt-indicator-buffers
            (seq-filter (lambda (other)
                          (and (buffer-live-p other) (not (eq other buffer))))
                        hermes-chat--prompt-indicator-buffers))
      (force-mode-line-update t))))

(defun hermes-chat--prompt-indicator-sync ()
  "Register or retire the current chat in the global prompt indicator.
Membership does not depend on `hermes-chat-prompt-indicator', so enabling
the option later shows prompts that arrived while it was off.  Killing the
chat or changing its major mode retires it."
  (hermes-chat--refresh-prompt-entry-actions)
  (let ((pending (hermes-chat--pending-prompt-p))
        (known (memq (current-buffer) hermes-chat--prompt-indicator-buffers)))
    (cond
     ((and pending (not known))
      (setq hermes-chat--prompt-indicator-buffers
            (append hermes-chat--prompt-indicator-buffers
                    (list (current-buffer))))
      (add-hook 'kill-buffer-hook #'hermes-chat--prompt-indicator-forget nil t)
      (add-hook 'change-major-mode-hook
                #'hermes-chat--prompt-indicator-forget nil t)
      (when hermes-chat-prompt-indicator
        (hermes-chat--prompt-indicator-install))
      (force-mode-line-update t))
     ((and known (not pending))
      (hermes-chat--prompt-indicator-forget))
     (known (force-mode-line-update t)))))

(defun hermes-chat--prompt-indicator-pending ()
  "Return live registered chat buffers with pending prompt requests."
  (seq-filter (lambda (buffer)
                (and (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (hermes-chat--pending-prompt-p))))
              hermes-chat--prompt-indicator-buffers))

(defvar-keymap hermes-chat--prompt-indicator-map
  :doc "Mouse access to pending Hermes prompts from the global mode line."
  "<mode-line> <mouse-1>" #'hermes-chat-prompt-indicator-respond)

(defun hermes-chat--prompt-indicator-string ()
  "Return the global pending-prompt indicator, or nil if none is pending.
Name only the oldest chat's first prompt kind and the total count: prompt
contents may be sensitive and never belong in the mode line."
  (when-let* ((hermes-chat-prompt-indicator)
              (buffers (hermes-chat--prompt-indicator-pending)))
    (let* ((owner (car buffers))
           (kind (with-current-buffer owner
                   (hermes-chat--prompt-display-name
                    (hermes-chat--first-pending-prompt))))
           (count (apply #'+ (mapcar (lambda (buffer)
                                       (with-current-buffer buffer
                                         (hermes-chat--pending-prompt-count)))
                                     buffers))))
      (propertize
       (format " [Hermes: %s%s]" kind
               (if (> count 1) (format " +%d" (1- count)) ""))
       'face 'warning
       'mouse-face 'mode-line-highlight
       'local-map hermes-chat--prompt-indicator-map
       'help-echo (format "%d pending Hermes prompt%s\nmouse-1: answer in %s"
                          count (if (= count 1) "" "s") (buffer-name owner))))))

(defun hermes-chat-prompt-indicator-respond ()
  "Respond to a pending prompt in the oldest waiting Hermes chat.
This is the mouse action of the global pending-prompt indicator."
  (interactive)
  (let ((buffer (car (hermes-chat--prompt-indicator-pending))))
    (unless buffer
      (user-error "No pending Hermes prompt requests"))
    (pop-to-buffer buffer)
    (call-interactively #'hermes-chat-respond-to-prompt)))

(defun hermes-chat--prompt-header-status (prompt)
  "Return header status symbol for pending PROMPT."
  (if (hermes-chat--approval-prompt-p prompt) 'approval-requested 'requested))

(defun hermes-chat--prompt-header-activity (prompt)
  "Return header activity for pending PROMPT."
  (or (hermes-chat--header-activity-for-event prompt)
      (format "%s requested" (hermes-chat--prompt-display-name prompt))))

(defun hermes-chat--show-pending-prompt-state (&optional prompt)
  "Show PROMPT or any pending prompt in the chat header."
  (when-let* ((pending (or prompt (hermes-chat--first-pending-prompt))))
    (hermes-chat--set-header-state
     :status (hermes-chat--prompt-header-status pending)
     :activity (hermes-chat--prompt-header-activity pending))
    pending))

(defun hermes-chat--retain-pending-prompt-header ()
  "Restore the pending prompt's header state if later activity replaced it.
A pending prompt blocks the turn, so unrelated status events arriving while
it waits must not hide the header's only in-chat signal that input is due."
  (when (and (hermes-chat--pending-prompt-p)
             (not (memq (plist-get hermes-chat--status-state :status)
                        '(approval-requested requested))))
    (hermes-chat--show-pending-prompt-state)))

(defun hermes-chat--clarify-prompt-p (prompt)
  "Return non-nil when PROMPT accepts recoverable clarification text."
  (equal (hermes-chat--prompt-event-type prompt) "clarify"))

(defun hermes-chat--retain-clarify-response (context response)
  "Return CONTEXT retaining a copied clarification RESPONSE owner."
  (let* ((active-sink hermes-chat--reset-clarify-owner-sink)
         (sink (and (eq (car active-sink) (current-buffer)) active-sink))
         (owner (list :buffer (current-buffer)
                      :generation hermes-chat--lifecycle-generation
                      :response-token (plist-get context :token)
                      :text (copy-sequence response))))
    (if sink
        (setf (cadr sink) (append (cadr sink) (list owner)))
      (setq hermes-chat--retained-clarify-owners
            (append hermes-chat--retained-clarify-owners (list owner))))
    (setq context (plist-put context :retained-owner owner))
    (when sink
      (setq context (plist-put context :retained-owner-sink sink)))
    context))

(defun hermes-chat--settle-retained-clarify (context &optional keep-sink)
  "Remove CONTEXT's exact retained owner unless KEEP-SINK preserves it."
  (when-let* ((owner (plist-get context :retained-owner)))
    (let ((sink (plist-get context :retained-owner-sink)))
      (unless (and keep-sink sink)
        (if sink
            (setf (cadr sink) (delq owner (cadr sink)))
          (setq hermes-chat--retained-clarify-owners
                (delq owner hermes-chat--retained-clarify-owners)))))))

(defun hermes-chat--drain-reset-clarify-owners (sink)
  "Drain SINK once into the fresh chat input in acceptance order."
  (let ((owners (cadr sink)))
    (setf (cadr sink) nil)
    (when owners
      (hermes-chat--restore-input-tail
       (mapconcat (lambda (owner) (plist-get owner :text)) owners "\n")))))

(defun hermes-chat--terminal-approval-session (prompt)
  "Return PROMPT's exact approval session, including nil."
  (hermes-chat--event-string prompt '(:session-id :session_id)))

(defun hermes-chat--terminal-prompt-entry (key prompt)
  "Return terminal authority for PROMPT under KEY."
  (let* ((approval-p (hermes-chat--approval-prompt-p prompt))
         (queue (and approval-p (or (plist-get prompt :prompt-queue)
                                    (list prompt)))))
    (list :key key
          :prompt prompt
          :response-token (plist-get prompt :response-token)
          :approval-p approval-p
          :approval-members (and approval-p (copy-sequence queue))
          :session-id (and approval-p
                           (hermes-chat--terminal-approval-session
                            (car queue))))))

(defconst hermes-chat--invalid-terminal-auto-claims
  'hermes-chat--invalid-terminal-auto-claims
  "Marker for malformed terminal auto-prompt claim state.")

(defun hermes-chat--terminal-auto-claims ()
  "Return exact sorted auto-prompt claims, or the private invalid marker."
  (cond
   ((null hermes-chat--auto-prompt-keys) nil)
   ((not (hash-table-p hermes-chat--auto-prompt-keys))
    hermes-chat--invalid-terminal-auto-claims)
   (t
    (catch 'invalid
      (let ((missing (make-symbol "missing-prompt")) entries keys)
        (maphash
         (lambda (key claim)
           (unless (and (stringp key) (not (member key keys)))
             (throw 'invalid hermes-chat--invalid-terminal-auto-claims))
           (push key keys)
           (push (cons key claim) entries))
         hermes-chat--auto-prompt-keys)
        (mapcar
         (lambda (entry)
           (let* ((key (car entry))
                  (claim (cdr entry))
                  (prompt (if (hash-table-p hermes-chat--pending-prompts)
                              (gethash key hermes-chat--pending-prompts missing)
                            missing)))
             (unless (and (eql (proper-list-p claim) 2)
                          (equal (car claim) key)
                          prompt
                          (not (eq prompt missing))
                          (eq (cadr claim) prompt))
               (throw 'invalid hermes-chat--invalid-terminal-auto-claims))
             (list :key key :claim claim :prompt prompt)))
         (sort entries (lambda (left right)
                         (string< (car left) (car right))))))))))

(defun hermes-chat--capture-terminal-prompts ()
  "Return immutable plain terminal prompt authority for the current chat."
  (list :buffer (current-buffer)
        :generation hermes-chat--lifecycle-generation
        :prompt-table hermes-chat--pending-prompts
        :auto-table hermes-chat--auto-prompt-keys
        :auto-claims (hermes-chat--terminal-auto-claims)
        :retained-owners (copy-sequence hermes-chat--retained-clarify-owners)
        :entries
        (mapcar (lambda (key)
                  (hermes-chat--terminal-prompt-entry
                   key (gethash key hermes-chat--pending-prompts)))
                (hermes-chat--pending-prompt-keys))))

(defun hermes-chat--terminal-snapshot-current-p (snapshot)
  "Return non-nil when SNAPSHOT owns a live current prompt lifecycle."
  (let ((buffer (plist-get snapshot :buffer)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (derived-mode-p 'hermes-chat-mode)
                (eql hermes-chat--lifecycle-generation
                     (plist-get snapshot :generation))
                (eq hermes-chat--pending-prompts
                    (plist-get snapshot :prompt-table))
                (eq hermes-chat--auto-prompt-keys
                    (plist-get snapshot :auto-table)))))))

(defun hermes-chat--terminal-entry-matches-p (entry prompt)
  "Return non-nil when nonapproval PROMPT is owned by ENTRY."
  (and prompt
       (not (hermes-chat--approval-prompt-p prompt))
       (let ((token (plist-get entry :response-token)))
         (if token
             (eq (plist-get prompt :response-token) token)
           (eq prompt (plist-get entry :prompt))))))

(defun hermes-chat--terminal-auto-prompt (key)
  "Return KEY's current auto-prompt owner prompt, or nil."
  (and (hash-table-p hermes-chat--auto-prompt-keys)
       (cadr (gethash key hermes-chat--auto-prompt-keys))))

(defun hermes-chat--take-terminal-nonapproval (entry)
  "Take nonapproval ENTRY and return its response token, or nil."
  (let* ((key (plist-get entry :key))
         (prompt (gethash key hermes-chat--pending-prompts)))
    (when (hermes-chat--terminal-entry-matches-p entry prompt)
      (when (hermes-chat--terminal-entry-matches-p
             entry (hermes-chat--terminal-auto-prompt key))
        (remhash key hermes-chat--auto-prompt-keys))
      (remhash key hermes-chat--pending-prompts)
      (hermes-chat--prompt-indicator-sync)
      (plist-get entry :response-token))))

(defun hermes-chat--take-terminal-approval (entry)
  "Remove ENTRY's captured-session approval aggregate."
  (let* ((key (plist-get entry :key))
         (session (plist-get entry :session-id))
         (prompt (gethash key hermes-chat--pending-prompts))
         (queue (and (hermes-chat--approval-prompt-p prompt)
                     (or (plist-get prompt :prompt-queue) (list prompt))))
         (represented
          (cl-some (lambda (item)
                     (and (equal (hermes-chat--terminal-approval-session item)
                                 session)
                          (memq item queue)))
                   (plist-get entry :approval-members)))
         (remaining
          (and represented
               (cl-remove-if
                (lambda (item)
                  (equal (hermes-chat--terminal-approval-session item) session))
                queue))))
    (when represented
      (let ((auto (hermes-chat--terminal-auto-prompt key)))
        (when (and (hermes-chat--approval-prompt-p auto)
                   (equal (hermes-chat--terminal-approval-session auto) session))
          (remhash key hermes-chat--auto-prompt-keys)))
      (if remaining
          (puthash key (hermes-chat--approval-prompt-with-queue remaining)
                   hermes-chat--pending-prompts)
        (remhash key hermes-chat--pending-prompts))
      (hermes-chat--prompt-indicator-sync))))

(defun hermes-chat--terminal-restore-effect (owner)
  "Return a dormant one-shot restoration thunk for OWNER."
  (let ((pending t)
        (buffer (plist-get owner :buffer))
        (generation (plist-get owner :generation))
        (text (plist-get owner :text)))
    (lambda ()
      (when pending
        (setq pending nil)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (derived-mode-p 'hermes-chat-mode)
                       (eql hermes-chat--lifecycle-generation generation))
              (hermes-chat--restore-prompt-response text))))))))

(defun hermes-chat--take-terminal-retained (snapshot tokens)
  "Take SNAPSHOT retained owners matching exact TOKENS and return effects."
  (delq nil
        (mapcar
         (lambda (owner)
           (when (and (memq owner hermes-chat--retained-clarify-owners)
                      (memq (plist-get owner :response-token) tokens))
             (setq hermes-chat--retained-clarify-owners
                   (delq owner hermes-chat--retained-clarify-owners))
             (hermes-chat--terminal-restore-effect owner)))
         (plist-get snapshot :retained-owners))))

(defun hermes-chat--take-terminal-prompts (snapshot)
  "Take current authority from terminal prompt SNAPSHOT and return effects."
  (when (hermes-chat--terminal-snapshot-current-p snapshot)
    (with-current-buffer (plist-get snapshot :buffer)
      (let ((tokens
             (delq nil
                   (mapcar
                    (lambda (entry)
                      (if (plist-get entry :approval-p)
                          (hermes-chat--take-terminal-approval entry)
                        (hermes-chat--take-terminal-nonapproval entry)))
                    (plist-get snapshot :entries)))))
        (hermes-chat--take-terminal-retained snapshot tokens)))))

(defun hermes-chat--response-redaction-variants (response)
  "Return string variants of RESPONSE that may appear in errors."
  (when (and (stringp response) (not (string-empty-p response)))
    (let* ((variants (list response))
           (encoded (json-encode-string response)))
      (push encoded variants)
      (when (and (> (length encoded) 1)
                 (eq (aref encoded 0) ?\")
                 (eq (aref encoded (1- (length encoded))) ?\"))
        (push (substring encoded 1 -1) variants))
      (sort (delete-dups variants)
            (lambda (left right)
              (> (length left) (length right)))))))

(defun hermes-chat--redact-response-value (text response)
  "Return TEXT with RESPONSE variants replaced by a redaction marker."
  (let ((message (or text "")))
    (dolist (variant (hermes-chat--response-redaction-variants response)
                     message)
      (setq message (string-replace variant "<redacted>" message)))))

(defun hermes-chat--prompt-safe-error (prompt response message)
  "Return safe error MESSAGE for PROMPT and RESPONSE."
  (if (hermes-chat--clarify-prompt-p prompt)
      message
    (hermes-chat--redact-response-value message response)))

(defun hermes-chat--prompt-choices (prompt)
  "Return PROMPT choices as strings, or nil."
  (and-let* ((choices (hermes-chat--event-value prompt '(:choices))))
    (delq nil (mapcar #'hermes-chat--scalar-string
                      (if (vectorp choices) (append choices nil) choices)))))

(defun hermes-chat--prompt-questions (prompt)
  "Return PROMPT's batch clarification questions as a list, or nil."
  (when-let* ((questions (hermes-chat--event-value prompt '(:questions)))
              ((or (vectorp questions) (listp questions))))
    (append questions nil)))

(defun hermes-chat--batch-clarify-p (prompt)
  "Return non-nil when PROMPT is a batched clarification."
  (and (hermes-chat--clarify-prompt-p prompt)
       (hermes-chat--prompt-questions prompt)))

(defun hermes-chat--batch-question-choices (question)
  "Return QUESTION's choices as strings, or nil."
  (when-let* ((choices (hermes-transport--get question 'choices))
              ((or (vectorp choices) (listp choices))))
    (delq nil
          (mapcar #'hermes-chat--scalar-string (append choices nil)))))

(defun hermes-chat--clarify-reader-prompt (question &optional multiple)
  "Return a minibuffer prompt asking QUESTION.
MULTIPLE notes that several comma-separated answers are accepted."
  (let ((text (string-trim (or question ""))))
    (concat (if (string-empty-p text) "Clarify" text)
            (and multiple " (comma-separated)")
            (if (string-match-p "[?:]\\'" text) " " ": "))))

(defun hermes-chat--read-batch-question-response (question)
  "Read and return one batched clarification QUESTION response."
  (let* ((text (hermes-transport--scalar-string
                (hermes-transport--get question 'question)))
         (choices (hermes-chat--batch-question-choices question))
         (multiple (and choices
                        (eq (hermes-transport--get question 'multi_select) t)))
         (prompt (hermes-chat--clarify-reader-prompt text multiple)))
    (cond
     (multiple (completing-read-multiple prompt choices))
     (choices (completing-read prompt choices))
     (t (read-string prompt)))))

(defun hermes-chat--unanswered-batch-questions (prompt)
  "Return PROMPT's unanswered batch questions in their original order."
  (let ((answers (hermes-chat--event-value prompt '(:answers))))
    (seq-remove
     (lambda (question)
       (when-let* ((qid (hermes-transport--scalar-string
                        (hermes-transport--get question 'qid))))
         (hermes-transport--field-present-p answers qid)))
     (hermes-chat--prompt-questions prompt))))

(defun hermes-chat--batch-clarify-input-response (prompt input)
  "Return INPUT qualified for PROMPT's next unanswered batch question."
  (let* ((question (car (hermes-chat--unanswered-batch-questions prompt)))
         (qid (hermes-transport--non-empty-string
               (hermes-transport--scalar-string
                (hermes-transport--get question 'qid)))))
    (unless question
      (user-error "No unanswered Hermes clarification questions"))
    (unless qid
      (user-error "Hermes batch clarification has no question id"))
    (list (cons qid (if (eq (hermes-transport--get question 'multi_select) t)
                        (list input)
                      input)))))

(defun hermes-chat--batch-clarify-responses (prompt response)
  "Return unanswered question responses for batch PROMPT.
RESPONSE is an optional alist keyed by question id; nil reads interactively."
  (when (and response (not (listp response)))
    (user-error "Batch clarification responses must be keyed by question id"))
  (mapcar
   (lambda (question)
     (let* ((qid (hermes-transport--scalar-string
                  (hermes-transport--get question 'qid)))
            (provided (and response (assoc qid response))))
       (unless qid
         (user-error "Hermes batch clarification has no question id"))
       (when (and response (null provided))
         (user-error "No response supplied for Hermes question %s" qid))
       (cons qid (if response (cdr provided)
                   (hermes-chat--read-batch-question-response question)))))
   (hermes-chat--unanswered-batch-questions prompt)))

(defun hermes-chat--approval-choice-label (choice)
  "Return a minibuffer label for approval CHOICE."
  (pcase choice
    ("once" "Approve once")
    ("session" "Approve for session")
    ("always" "Always approve")
    ("deny" "Deny")
    (_ choice)))

(defun hermes-chat--approval-response-candidates (prompt)
  "Return completion candidates for responding to approval PROMPT.
The choice vocabulary comes from PROMPT's `:choices' when present and
otherwise defaults to the server's once/session/always/deny set; the
backend never gates \"always\", so no choice is filtered locally."
  (let ((choices (or (hermes-chat--prompt-choices prompt)
                     '("once" "session" "always" "deny"))))
    (append (mapcar (lambda (choice)
                      (cons (hermes-chat--approval-choice-label choice) choice))
                    choices)
            '(("Cancel / ignore" . nil)))))

(defconst hermes-chat--approval-inline-lines 6
  "Most command lines an approval reader shows in the echo area itself.
Longer commands are shown in full in a separate window while reading.")

(defconst hermes-chat--approval-choice-keys
  '(("once" . ?o) ("session" . ?s) ("always" . ?a) ("deny" . ?d))
  "One-key answers for the server's approval vocabulary.")

(defun hermes-chat--approval-choice-key (choice used)
  "Return an unused answer key for approval CHOICE, avoiding USED keys."
  (or (alist-get choice hermes-chat--approval-choice-keys nil nil #'equal)
      (seq-find (lambda (char) (not (memq char used)))
                (append (downcase choice) "123456789"))))

(defun hermes-chat--approval-read-choices (prompt)
  "Return `read-multiple-choice' choices for approval PROMPT.
Each element is (KEY NAME DESCRIPTION VALUE); VALUE nil, under `q', leaves
the request pending exactly like the former \"Cancel / ignore\" choice."
  (let ((used (list ?q ??)) choices)
    (pcase-dolist (`(,label . ,value)
                   (hermes-chat--approval-response-candidates prompt))
      (let ((key (if value (hermes-chat--approval-choice-key value used) ?q)))
        (push key used)
        (push (list key (or value "quit")
                    (if value label "Leave the request pending")
                    value)
              choices)))
    (nreverse choices)))

(defun hermes-chat--approval-command-lines (prompt)
  "Return the command lines approval PROMPT asks about."
  (when-let* ((command (hermes-chat--event-string prompt '(:command))))
    (split-string (string-trim-right command) "\n")))

(defun hermes-chat--approval-heading (prompt)
  "Return the first line of approval PROMPT's reader."
  (let ((description (hermes-chat--event-string prompt '(:description)))
        (count (or (plist-get prompt :prompt-count) 1)))
    (concat (propertize "⚠ Approval" 'face 'warning)
            (and description (concat " · " description))
            (and (> count 1)
                 (concat " "
                         (propertize (format "(1 of %d)" count)
                                     'face 'font-lock-type-face))))))

(defun hermes-chat--approval-reader-prompt (prompt &optional detail-buffer)
  "Return the reader text for approval PROMPT.
Show every command line, or only the first and a pointer to DETAIL-BUFFER
when the command is too long for the echo area."
  (let* ((lines (hermes-chat--approval-command-lines prompt))
         (shown (if detail-buffer (list (car lines)) lines)))
    (concat (hermes-chat--approval-heading prompt) "\n"
            (mapconcat (lambda (line) (concat "  " line "\n")) shown "")
            (when detail-buffer
              (propertize (format "  … %d more lines in %s\n"
                                  (1- (length lines))
                                  (buffer-name detail-buffer))
                          'face 'shadow))
            (unless (or lines (hermes-chat--event-string prompt '(:description)))
              (concat "  " (or (plist-get prompt :content) "") "\n"))
            "Decision")))

(defun hermes-chat--approval-detail-buffer (prompt)
  "Return a new buffer showing approval PROMPT's long command, or nil.
The buffer is always freshly generated, so a foreign buffer that happens
to carry the same name is never adopted, erased or killed; the caller owns
the returned object and must kill exactly it."
  (let ((lines (hermes-chat--approval-command-lines prompt)))
    (when (> (length lines) hermes-chat--approval-inline-lines)
      (let ((buffer (generate-new-buffer " *Hermes approval command*" t))
            done)
        (unwind-protect
            (with-current-buffer buffer
              (insert (hermes-chat--approval-heading prompt) "\n\n"
                      (string-join lines "\n") "\n")
              (setq buffer-read-only t)
              (goto-char (point-min))
              (setq done t))
          (unless done
            (kill-buffer buffer)))
        buffer))))

(defun hermes-chat--read-approval-submitted (text candidates)
  "Read one of approval CANDIDATES after TEXT, requiring explicit submission.
CANDIDATES are (LABEL . VALUE) pairs.  Unlike a one-key reader, nothing
is decided until the user submits a candidate with RET, so a stray
keystroke meant for the composer cannot authorise anything; an empty
submission picks the non-deciding \"Cancel / ignore\" candidate."
  (let* ((default (or (car (rassoc nil candidates)) (caar candidates)))
         (label (completing-read (concat text ": ")
                                 (mapcar #'car candidates) nil t nil nil
                                 default))
         (candidate (assoc label candidates)))
    (unless candidate
      (user-error "Unknown approval decision: %s" label))
    (cdr candidate)))

(defun hermes-chat--read-approval-one-key (text choices)
  "Read one of approval CHOICES after TEXT with a single key."
  (let* ((answer (read-multiple-choice
                  text (mapcar (lambda (choice) (seq-take choice 3)) choices)))
         (choice (assq (car answer) choices)))
    (unless choice
      (user-error "Unknown approval decision: %s" (cadr answer)))
    (nth 3 choice)))

(defun hermes-chat--read-approval-response (prompt)
  "Read an approval response for PROMPT, showing what it approves.
The reader names the tool description and the full command; a very long
command is shown in full in a separate window while reading.  The
reader requires an explicit RET submission, so a stray keystroke cannot
authorise a command; an automatically opened reader also discards
type-ahead.  Only a reader the user explicitly opened (see
`hermes-chat--prompt-user-invoked-p') takes one-key answers.  `q',
\"Cancel / ignore\" or \\[keyboard-quit] leave the request pending."
  (let* ((auto hermes-chat--auto-prompting-p)
         (one-key (and hermes-chat--prompt-user-invoked-p (not auto)))
         ;; Timers and hooks running inside the reader are not the user.
         (hermes-chat--prompt-user-invoked-p nil)
         detail window value)
    (when auto
      (discard-input))
    (unwind-protect
        (progn
          (setq detail (hermes-chat--approval-detail-buffer prompt))
          (when (and detail (not noninteractive))
            (setq window (display-buffer
                          detail '(display-buffer-below-selected
                                   (window-height . fit-window-to-buffer)
                                   (inhibit-same-window . t)))))
          (let ((text (hermes-chat--approval-reader-prompt prompt detail)))
            (setq value
                  (if one-key
                      (hermes-chat--read-approval-one-key
                       text (hermes-chat--approval-read-choices prompt))
                    (hermes-chat--read-approval-submitted
                     text (hermes-chat--approval-response-candidates prompt))))))
      (when (and (window-live-p window) (eq (window-buffer window) detail))
        (quit-restore-window window))
      (when (buffer-live-p detail)
        (kill-buffer detail)))
    (or value (keyboard-quit))))

(defun hermes-chat--terminal-read-text (entries &optional start count)
  "Return a JSON terminal-read snapshot for chat transcript ENTRIES.
START (0-indexed, default 0) and COUNT (default all) page over transcript
lines.  The desktop read-terminal tool returns the in-app terminal pane; in
Emacs the closest analog is the chat transcript, encoded with the same
`total_lines'/`start'/`end'/`viewport_rows'/`cursor_row'/`text' shape."
  (let* ((all-text (string-join
                    (delq nil
                          (mapcar (lambda (entry)
                                    (plist-get entry :content))
                                  entries))
                    "\n"))
         (lines (if (string-empty-p all-text)
                    nil
                  (split-string all-text "\n")))
         (total (length lines))
         (from (max 0 (or (and (integerp start) start) 0)))
         (limit (and (integerp count) (max 1 count)))
         (end (if limit (min total (+ from limit)) total))
         (rows (max 0 (- end from)))
         (page (seq-subseq lines (min from total) (min end total))))
    (json-encode
     `((total_lines . ,total)
       (start . ,(min from total))
       (end . ,end)
       (viewport_rows . ,rows)
       (cursor_row . ,(if (zerop rows) 0 (1- rows)))
       (text . ,(string-join page "\n"))))))

(defun hermes-chat--vault-prompt-p (prompt)
  "Return non-nil when PROMPT is a supported browser-vault request."
  (member (hermes-chat--prompt-event-type prompt)
          '("vault.unlock_prompt" "vault.save_login" "vault.code")))

(defun hermes-chat--vault-prompt-label (prompt)
  "Return the backend and site identity for vault PROMPT."
  (let* ((client (plist-get (plist-get prompt :server-request) :client))
         (url (hermes-dashboard-transport-client-redacted-websocket-url client)))
    (dolist (field (pcase (hermes-chat--prompt-event-type prompt)
                    ("vault.unlock_prompt" '(:backend :display-name))
                    ("vault.save_login" '(:site :origin))))
      (unless (hermes-transport--non-empty-string (plist-get prompt field))
        (user-error "Hermes vault destination identity unavailable")))
    (format "Hermes backend %s — %s"
            (or (hermes-transport--non-empty-string url)
                (user-error "Hermes vault backend identity unavailable"))
            (pcase (hermes-chat--prompt-event-type prompt)
              ("vault.unlock_prompt"
               (format "unlock %S (%S)"
                       (plist-get prompt :display-name) (plist-get prompt :backend)))
              ("vault.save_login"
               (format "save login for %S, origin %S"
                       (plist-get prompt :site) (plist-get prompt :origin)))
              (_ (format "code for %S (%S)"
                         (or (plist-get prompt :site) "unspecified site")
                         (or (plist-get prompt :hint) "no hint")))))))

(defun hermes-chat--vault-read (label masked current)
  "Read LABEL with MASKED input while CURRENT retains authority.
Do not store input in history.  Retire the exact native reader when its
request expires, without aborting unrelated recursive input."
  (unless (funcall current) (user-error "Hermes vault request retired"))
  (let ((active t) timer)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (let ((input (current-buffer)) (depth (minibuffer-depth))
                    (recursion (recursion-depth)))
                (setq timer
                      (run-at-time
                       0.1 0.1
                       (lambda ()
                         (when (and active (not (funcall current))
                                    (= recursion (recursion-depth))
                                    (= depth (minibuffer-depth))
                                    (eq input (window-buffer
                                               (minibuffer-window))))
                           (abort-recursive-edit)))))))
          (if masked (read-passwd label) (read-string label nil t)))
      (setq active nil)
      (when timer (cancel-timer timer)))))

(defun hermes-chat--vault-answer (prompt current)
  "Collect the released value for vault PROMPT under CURRENT authority."
  (let ((label (hermes-chat--vault-prompt-label prompt)))
    (if (equal (hermes-chat--prompt-event-type prompt) "vault.save_login")
        (let* ((identifier (hermes-chat--vault-read
                            (concat label " — identifier: ") nil current))
               (password (hermes-chat--vault-read
                          (concat label " — password (empty declines): ") t current)))
          (if (string-empty-p password) ""
            (json-encode `((identifier . ,identifier) (password . ,password)))))
      (hermes-chat--vault-read (concat label " (empty declines): ") t current))))

(defun hermes-chat--respond-to-vault (key prompt context)
  "Read and send vault PROMPT under KEY and exact owner CONTEXT.
Only the backend stores credentials and controls its browser.  This reader
does not provide memory zeroization or isolation from trusted Emacs Lisp."
  (let* ((buffer (current-buffer))
         (current (lambda ()
                    (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (and (derived-mode-p 'hermes-chat-mode)
                                (hermes-chat--prompt-owner-current-p context))))))
         (debug-on-error nil) (debug-on-quit nil) (debug-on-signal nil)
         (websocket-debug nil) (message-log-max nil)
         (kill-ring (copy-sequence kill-ring)) (kill-ring-yank-pointer nil)
         ;; Native appended kills mutate existing menu entries, not just its
         ;; spine.  Copy every cons so no retained menu aliases receive input.
         (yank-menu (copy-tree yank-menu))
         (interprogram-cut-function nil) (interprogram-paste-function nil)
         (select-active-regions nil) (save-interprogram-paste-before-kill nil))
    (condition-case nil
        (let ((answer (condition-case nil
                          (hermes-chat--vault-answer prompt current)
                        (quit ""))))
          (when (funcall current)
            (with-current-buffer buffer
              (hermes-chat--send-prompt-response
               key prompt answer nil (string-empty-p answer) nil context))))
      (error (user-error "Hermes vault response failed")))))

(defun hermes-chat--read-prompt-response (prompt)
  "Read a response for PROMPT using an Emacs-native minibuffer UI."
  (pcase (hermes-chat--prompt-event-type prompt)
    ("approval"
     (hermes-chat--read-approval-response prompt))
    ("clarify"
     (let ((question (hermes-chat--event-string prompt '(:question :content))))
       (if-let* ((choices (hermes-chat--prompt-choices prompt)))
           ;; Choices are suggestions, not a closed set: the agent's clarify
           ;; tool always lets the user type their own answer, so do not
           ;; require a match.
           (if (and (plist-get prompt :server-request)
                    (eq (plist-get prompt :multi-select) t))
               (json-encode
                (vconcat (completing-read-multiple
                          (hermes-chat--clarify-reader-prompt question t)
                          choices)))
             (completing-read (hermes-chat--clarify-reader-prompt question)
                              choices))
         (read-string (hermes-chat--clarify-reader-prompt question)))))
    ("sudo" (read-passwd "Sudo password: "))
    ("secret"
     (read-passwd (or (hermes-chat--event-string prompt '(:prompt :content))
                      "Secret: ")))
    ("terminal"
     (hermes-chat--terminal-read-text
      (hermes-chat--entries)
      (plist-get prompt :start)
      (plist-get prompt :count)))
    (_ (read-string "Prompt response: "))))

(defun hermes-chat--approval-response-resolved-count (result)
  "Return positive resolved approval count from RESULT, or nil."
  (let ((resolved (hermes-transport--get result 'resolved)))
    (and (integerp resolved) (> resolved 0) resolved)))

(defun hermes-chat--advance-prompt-response (context prompt count)
  "Advance COUNT queued records for PROMPT owned by CONTEXT.
Return the next pending prompt."
  (let* ((key (plist-get context :key))
         (current (gethash key hermes-chat--pending-prompts))
         (queue (and (hermes-chat--approval-prompt-p prompt)
                     (or (plist-get current :prompt-queue)
                         (plist-get prompt :prompt-queue))))
         (remaining (and queue (nthcdr count queue)))
         (next (and remaining
                    (hermes-chat--approval-prompt-with-queue remaining))))
    (hermes-chat--release-auto-prompt-claim key)
    (if next
        (progn
          (puthash key next hermes-chat--pending-prompts)
          (hermes-chat--upsert-transport-entry
           (or (plist-get next :assistant-id)
               (plist-get prompt :assistant-id))
           next))
      (remhash key hermes-chat--pending-prompts))
    (hermes-chat--prompt-indicator-sync)
    next))

(defun hermes-chat--prompt-response-complete (context prompt canceled result)
  "Mark PROMPT response owned by CONTEXT complete, noting CANCELED and RESULT."
  (let* ((resolved-count
          (and (hermes-chat--approval-prompt-p prompt)
               (hermes-chat--approval-response-resolved-count result)))
         (next-prompt
          (hermes-chat--advance-prompt-response
           context prompt (or resolved-count
                              (plist-get context :response-count)))))
    (let ((status (concat (hermes-chat--prompt-display-name prompt)
                          " "
                          (if canceled "canceled" "response sent"))))
      (hermes-chat--insert-prompt-status
       prompt status (if canceled 'error 'done))
      (unless (hermes-chat--show-pending-prompt-state next-prompt)
        (hermes-chat--set-header-state
         :status (if (hermes-chat--active-turn-p) 'running 'ready)
         :activity status))
      (when next-prompt
        (hermes-chat--schedule-auto-prompt next-prompt)))))

(defun hermes-chat--approval-response-unresolved-p (prompt result)
  "Return non-nil when approval PROMPT RESULT resolved no backend prompt."
  (and (hermes-chat--approval-prompt-p prompt)
       (equal (hermes-transport--get result 'resolved) 0)))

(defun hermes-chat--prompt-response-expired-p (result)
  "Return non-nil when RESULT reports an expired backend prompt."
  (equal (hermes-transport--scalar-string
          (hermes-transport--get result 'status))
         "expired"))

(defun hermes-chat--prompt-response-stale (context prompt)
  "Clear stale PROMPT owned by CONTEXT without claiming a response was sent."
  (let ((status (concat (hermes-chat--prompt-display-name prompt)
                        " request no longer pending"))
        (next-prompt
         (hermes-chat--advance-prompt-response
          context prompt (plist-get context :response-count))))
    (hermes-chat--insert-local-status status 'error)
    (unless (hermes-chat--show-pending-prompt-state next-prompt)
      (hermes-chat--set-header-state
       :status (if (hermes-chat--active-turn-p) 'running 'ready)
       :activity status))
    (when next-prompt
      (hermes-chat--schedule-auto-prompt next-prompt))))

(defun hermes-chat--prompt-missing-error-p (prompt message)
  "Return non-nil when MESSAGE is PROMPT's exact backend-missing error."
  (when-let* ((message (and (stringp message) (downcase (string-trim message)))))
    (pcase (hermes-chat--prompt-event-type prompt)
      ("clarify" (equal message "no pending answer request"))
      ("terminal" (equal message "no pending text request"))
      ("sudo" (equal message "no pending password request"))
      ("secret" (equal message "no pending value request"))
      ("approval" (member message '("no pending approval"
                                     "no pending approval request"))))))

(defun hermes-chat--restore-prompt-response (response)
  "Restore failed prompt RESPONSE without queueing a turn or moving the reader."
  (hermes-chat--restore-input-tail
   response "Restored failed prompt response after current draft"))

(defun hermes-chat--prompt-response-rejected
    (context prompt response message &optional preserve-response)
  "Render rejection MESSAGE for PROMPT and RESPONSE owned by CONTEXT.
When PRESERVE-RESPONSE is non-nil, keep clarification RESPONSE recoverable."
  (let* ((safe-message
          (hermes-chat--prompt-safe-error prompt response message))
         (next-prompt
          (if (or (hermes-chat--prompt-missing-error-p prompt message)
                  (and (plist-get prompt :server-request)
                       (not (hermes-dashboard-transport-server-request-current-p
                             (plist-get prompt :server-request)))))
              (hermes-chat--advance-prompt-response
               context prompt (plist-get context :response-count))
            (hermes-chat--release-prompt-response context)
            nil)))
    (hermes-chat--settle-retained-clarify context t)
    (when (and preserve-response
               (hermes-chat--clarify-prompt-p prompt)
               (not (plist-get context :retained-owner-sink)))
      (hermes-chat--restore-prompt-response response))
    (hermes-chat--command-error safe-message)
    (when next-prompt
      (hermes-chat--show-pending-prompt-state next-prompt)
      (hermes-chat--schedule-auto-prompt next-prompt))))

(defun hermes-chat--prompt-response-in-flight-p (key)
  "Return non-nil when prompt KEY already has a response in flight."
  (and-let* ((prompt (and (hash-table-p hermes-chat--pending-prompts)
                          (gethash key hermes-chat--pending-prompts))))
    (plist-get prompt :response-token)))

(defun hermes-chat--release-all-prompt-response-claims ()
  "Release stale response claims and retained owners after invalidation."
  (when (hash-table-p hermes-chat--pending-prompts)
    (maphash
     (lambda (key prompt)
       (let ((token (plist-get prompt :response-token)))
         (when (and token
                    (not (eql (cadr token)
                              hermes-chat--lifecycle-generation)))
           (puthash key (plist-put (copy-sequence prompt) :response-token nil)
                    hermes-chat--pending-prompts))))
     hermes-chat--pending-prompts))
  (setq hermes-chat--retained-clarify-owners
        (cl-remove-if
         (lambda (owner)
           (and (eq (plist-get owner :buffer) (current-buffer))
                (not (eql (plist-get owner :generation)
                          hermes-chat--lifecycle-generation))))
         hermes-chat--retained-clarify-owners)))

(defun hermes-chat--prompt-response-context (client key prompt all)
  "Claim ownership context for CLIENT, KEY, PROMPT, and ALL scope."
  (unless (eq prompt (and (hash-table-p hermes-chat--pending-prompts)
                          (gethash key hermes-chat--pending-prompts)))
    (user-error "Hermes prompt request is no longer pending"))
  (when (hermes-chat--prompt-response-in-flight-p key)
    (user-error "Hermes is accepting the previous prompt response"))
  (hermes-chat--release-auto-prompt-claim key)
  (let ((token (list key hermes-chat--lifecycle-generation))
        (response-count
         (if (and all (hermes-chat--approval-prompt-p prompt))
             (length (plist-get prompt :prompt-queue))
           1)))
    (puthash key
             (plist-put (copy-sequence (gethash key hermes-chat--pending-prompts))
                        :response-token token)
             hermes-chat--pending-prompts)
    (list :buffer (current-buffer)
          :client client
          :session-id hermes-chat--dashboard-active-session-id
          :generation hermes-chat--lifecycle-generation
          :prompts hermes-chat--pending-prompts
          :key key
          :token token
          :response-count response-count)))

(defun hermes-chat--release-prompt-response (context)
  "Release the response claim owned by CONTEXT."
  (let* ((key (plist-get context :key))
         (prompt (gethash key hermes-chat--pending-prompts))
         (claim (and (hash-table-p hermes-chat--auto-prompt-keys)
                     (gethash key hermes-chat--auto-prompt-keys))))
    (when prompt
      (hermes-chat--release-auto-prompt-claim key claim)
      (let ((restored (plist-put (copy-sequence prompt)
                                 :response-token nil)))
        (puthash key restored hermes-chat--pending-prompts)
        (when claim
          (hermes-chat--schedule-auto-prompt restored t))
        restored))))

(defun hermes-chat--prompt-response-current-p (context)
  "Return non-nil when prompt response CONTEXT still owns this chat."
  (and (eq hermes-chat--dashboard-client (plist-get context :client))
       (equal hermes-chat--dashboard-active-session-id
              (plist-get context :session-id))
       (eql hermes-chat--lifecycle-generation (plist-get context :generation))
       (eq hermes-chat--pending-prompts (plist-get context :prompts))
       (eq (plist-get
            (gethash (plist-get context :key) hermes-chat--pending-prompts)
            :response-token)
           (plist-get context :token))))

(defun hermes-chat--restore-retained-clarify (context)
  "Restore CONTEXT's retained clarification input once, then settle its owner."
  (when-let* ((owner (plist-get context :retained-owner))
              ((not (plist-get context :retained-owner-sink)))
              ((memq owner hermes-chat--retained-clarify-owners)))
    (hermes-chat--restore-prompt-response (plist-get owner :text))
    (hermes-chat--settle-retained-clarify context)))

(defun hermes-chat--prompt-success-callback (context prompt canceled)
  "Return a success callback for PROMPT response owned by CONTEXT."
  (lambda (result)
    (hermes-chat--in-buffer (plist-get context :buffer)
      (when (hermes-chat--prompt-response-current-p context)
        (if (or (hermes-chat--approval-response-unresolved-p prompt result)
                (hermes-chat--prompt-response-expired-p result))
            (progn
              (when (hermes-chat--clarify-prompt-p prompt)
                (hermes-chat--restore-retained-clarify context))
              (hermes-chat--prompt-response-stale context prompt))
          (hermes-chat--settle-retained-clarify context)
          (hermes-chat--prompt-response-complete
           context prompt canceled result))))))

(defun hermes-chat--prompt-reject-callback
    (context prompt response preserve-response)
  "Return an error callback for PROMPT and RESPONSE owned by CONTEXT."
  (lambda (message)
    (hermes-chat--in-buffer (plist-get context :buffer)
      (when (hermes-chat--prompt-response-current-p context)
        (hermes-chat--prompt-response-rejected
         context prompt response message preserve-response)))))

(defun hermes-chat--call-prompt-response
    (context prompt response preserve-response send)
  "Call SEND for CONTEXT and PROMPT, settling failed RESPONSE ownership.
PRESERVE-RESPONSE keeps submitted clarification text recoverable."
  (condition-case err
      (funcall send)
    (error
     (if (hermes-chat--prompt-response-current-p context)
         (hermes-chat--prompt-response-rejected
          context prompt response (error-message-string err) preserve-response)
       (signal
        (car err)
        (if (hermes-chat--clarify-prompt-p prompt)
            (cdr err)
          (list (hermes-chat--prompt-safe-error
                 prompt response (error-message-string err)))))))
    (quit
     (when (hermes-chat--prompt-response-current-p context)
       (hermes-chat--release-prompt-response context)
       (hermes-chat--settle-retained-clarify context t)
       (when (and preserve-response
                  (hermes-chat--clarify-prompt-p prompt)
                  (not (plist-get context :retained-owner-sink)))
         (hermes-chat--restore-prompt-response response)))
     (signal (car err) (cdr err)))))

(defun hermes-chat--batch-clarify-answer-alist (prompt)
  "Return PROMPT's accepted batch answers as a string-keyed alist."
  (let ((answers (hermes-chat--event-value prompt '(:answers))))
    (cl-loop for question in (hermes-chat--prompt-questions prompt)
             for qid = (hermes-transport--scalar-string
                        (hermes-transport--get question 'qid))
             when (and qid (hermes-transport--field-present-p answers qid))
             collect (cons qid (hermes-transport--get answers qid)))))

(defun hermes-chat--record-batch-clarify-answer (context qid answer)
  "Record QID's accepted ANSWER in the batch prompt owned by CONTEXT."
  (let* ((key (plist-get context :key))
         (prompt (gethash key hermes-chat--pending-prompts))
         (answers (hermes-chat--batch-clarify-answer-alist prompt))
         (updated (cons (cons qid answer)
                        (cl-remove qid answers :key #'car :test #'equal)))
         (base (plist-put (copy-sequence prompt) :answers updated))
         (content (hermes-dashboard-transport--batch-clarify-content base))
         (next (plist-put (plist-put base :content content)
                          :prompt-content content)))
    (puthash key next hermes-chat--pending-prompts)
    (when-let* ((assistant-id (plist-get next :assistant-id)))
      (hermes-chat--upsert-transport-entry assistant-id next))))

(defun hermes-chat--batch-clarify-success-callback
    (context prompt qid answer remaining)
  "Return callback advancing PROMPT response CONTEXT through REMAINING answers."
  (lambda (result)
    (hermes-chat--in-buffer (plist-get context :buffer)
      (when (hermes-chat--prompt-response-current-p context)
        (if (hermes-chat--prompt-response-expired-p result)
            (progn
              (hermes-chat--restore-retained-clarify context)
              (hermes-chat--prompt-response-stale context prompt))
          (hermes-chat--record-batch-clarify-answer context qid answer)
          (hermes-chat--settle-retained-clarify context)
          ;; A replay may have accepted a queued questionnaire answer while
          ;; this lock was in flight.  Never overwrite that surface's answer.
          (let ((accepted (hermes-chat--batch-clarify-answer-alist
                           (gethash (plist-get context :key)
                                    hermes-chat--pending-prompts))))
            (setq remaining
                  (cl-remove-if (lambda (entry) (assoc (car entry) accepted))
                                remaining)))
          (if remaining
              (let ((pending (hermes-chat--release-prompt-response context)))
                ;; Each question gets a fresh claim: a duplicate receipt for
                ;; an accepted answer must not settle or resend its successor.
                (hermes-chat--send-batch-clarify-response
                 (plist-get context :key) pending remaining context))
            (let ((current (gethash (plist-get context :key)
                                    hermes-chat--pending-prompts)))
              (if (hermes-chat--unanswered-batch-questions current)
                  (let ((pending (hermes-chat--release-prompt-response context)))
                    (hermes-chat--show-pending-prompt-state pending)
                    (hermes-chat--schedule-auto-prompt pending))
                (hermes-chat--prompt-response-complete
                 context prompt nil result)))))))))

(defun hermes-chat--send-next-batch-clarify (context prompt responses)
  "Send RESPONSES' next batch answer for PROMPT owned by CONTEXT."
  (pcase-let* ((`(,qid . ,answer) (car responses))
               (request (hermes-chat--request-prompt-id
                         (plist-get context :key) prompt))
               (input (plist-get (plist-get context :retained-owner) :text)))
    (hermes-chat--call-prompt-response
     context prompt (or input answer) input
     (lambda ()
       (funcall (if (plist-get prompt :server-request)
                    #'hermes-dashboard-transport-clarify-lock
                  #'hermes-dashboard-transport-clarify-question-respond)
        (plist-get context :client)
        (or (plist-get prompt :server-request) request) qid answer
        (hermes-chat--batch-clarify-success-callback
         context prompt qid answer (cdr responses))
        (hermes-chat--prompt-reject-callback
         context prompt (or input answer) input))))))

(defun hermes-chat--send-batch-clarify-response
    (key prompt responses owner &optional input)
  "Send RESPONSES for batch PROMPT under KEY and OWNER.
Retain unaccepted answers for manual recovery, separated by newlines.
When INPUT is non-nil, retain that literal composer text instead."
  (unless responses
    (user-error "No unanswered Hermes clarification questions"))
  (let* ((client (plist-get owner :client))
         (text (or input
                   (mapconcat
                    (lambda (response)
                      (let ((answer (cdr response)))
                        (if (stringp answer) answer
                          (mapconcat #'identity answer "\n"))))
                    responses "\n")))
         (context (hermes-chat--prompt-response-context
                   client key prompt nil))
         (context (if (hermes-transport--non-empty-string text)
                      (hermes-chat--retain-clarify-response context text)
                    context)))
    (hermes-chat--send-next-batch-clarify context prompt responses)))

(defun hermes-chat--send-clarify-input (key input)
  "Send composer INPUT to clarification KEY, retaining failed submissions.
For a batch, answer only the next unanswered question."
  (let* ((prompt (hermes-chat--pending-prompt key))
         (owner (hermes-chat--prompt-owner-context key prompt))
         (responses (and (hermes-chat--batch-clarify-p prompt)
                         (hermes-chat--batch-clarify-input-response prompt input))))
    (unless (hermes-chat--prompt-owner-current-p owner)
      (user-error "Hermes prompt request is no longer current"))
    (when (hermes-chat--prompt-response-in-flight-p key)
      (user-error "Hermes is accepting the previous prompt response"))
    (hermes-chat--delete-input-tail)
    (if responses
        (hermes-chat--send-batch-clarify-response key prompt responses owner input)
      (hermes-chat--send-prompt-response key prompt input nil nil t owner))))

(defun hermes-chat--approval-session-id (prompt)
  "Return the dashboard session id for approval PROMPT."
  (or (hermes-chat--event-string prompt '(:session-id :session_id))
      hermes-chat--dashboard-active-session-id))

(defun hermes-chat--request-prompt-id (key prompt)
  "Return request id for prompt KEY/PROMPT."
  (or (hermes-chat--event-string prompt '(:request-id :request_id)) key))

(defun hermes-chat--dispatch-prompt-response
    (client key prompt response all resolve reject)
  "Dispatch RESPONSE for KEY/PROMPT on CLIENT with RESOLVE and REJECT.
ALL applies to approval prompts only."
  (let ((type (hermes-chat--prompt-event-type prompt))
        (request (plist-get prompt :server-request)))
    (cond
     (request
      (hermes-dashboard-transport-answer-request
       request
       (pcase type
         ("approval" `((choice . ,response) (all . ,(if all t :false))))
         ("clarify" (if (hermes-chat--batch-clarify-p prompt)
                        (make-hash-table :test #'equal)
                      `((answer . ,response))))
         ((or "sudo" "secret" "vault.unlock_prompt" "vault.save_login" "vault.code")
          `((value . ,response))))
       resolve reject))
     ((equal type "approval")
        (hermes-dashboard-transport-approval-respond
         client :session-id (hermes-chat--approval-session-id prompt)
         :choice response :all (and all t) :resolve resolve :reject reject))
     (t
      (funcall (pcase type
                 ("clarify" #'hermes-dashboard-transport-clarify-respond)
                 ("sudo" #'hermes-dashboard-transport-sudo-respond)
                 ("secret" #'hermes-dashboard-transport-secret-respond)
                 ("terminal" #'hermes-dashboard-transport-terminal-read-respond)
                 (_ (user-error "Unsupported Hermes prompt type: %s" type)))
               client (hermes-chat--request-prompt-id key prompt) response
               resolve reject)))))

(defun hermes-chat--send-prompt-response
    (key prompt response all canceled &optional preserve-response owner)
  "Send RESPONSE for prompt KEY/PROMPT through the dashboard transport."
  (when (and owner (not (hermes-chat--prompt-owner-current-p owner)))
    (user-error "Hermes prompt request is no longer current"))
  (let* ((client (if owner
                     (plist-get owner :client)
                   (if hermes-chat--prompt-control-client-function
                        (funcall hermes-chat--prompt-control-client-function)
                      (user-error "Hermes dashboard prompt controls are unavailable"))))
         (context (hermes-chat--prompt-response-context
                   client key prompt all))
         (context (if (and (not canceled)
                           (hermes-chat--clarify-prompt-p prompt)
                           (hermes-transport--non-empty-string response))
                      (hermes-chat--retain-clarify-response context response)
                    context)))
    (hermes-chat--call-prompt-response
     context prompt response preserve-response
     (lambda ()
       (hermes-chat--dispatch-prompt-response
        client key prompt response all
        (hermes-chat--prompt-success-callback context prompt canceled)
        (hermes-chat--prompt-reject-callback
         context prompt response preserve-response))))))

(defun hermes-chat-respond-to-prompt (&optional key response all preserve-response)
  "Respond to pending prompt KEY with RESPONSE.
When called interactively, select the prompt and read RESPONSE in the
minibuffer; an interactive approval reader takes one-key answers, while
Lisp callers get a reader that requires an explicit RET submission.  With
prefix argument ALL, approval responses apply to all pending
approvals in the dashboard session.  Unaccepted clarification answers remain
recoverable if the request expires.  PRESERVE-RESPONSE also keeps programmatic
clarification input recoverable when the request fails.  Browser-vault
requests always use native readers, ignoring RESPONSE and PRESERVE-RESPONSE."
  (interactive (list nil nil current-prefix-arg) hermes-chat-mode)
  (let* ((hermes-chat--prompt-user-invoked-p
          (or hermes-chat--prompt-user-invoked-p
              (called-interactively-p 'any)))
         (prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key))
         (context (hermes-chat--prompt-owner-context prompt-key prompt)))
    (when (and (plist-get prompt :server-request)
               (not (hermes-chat--prompt-owner-current-p context)))
      (user-error "Hermes prompt request is no longer current"))
    (when (hermes-chat--prompt-response-in-flight-p prompt-key)
      (user-error "Hermes is accepting the previous prompt response"))
    (cond
     ((hermes-chat--vault-prompt-p prompt)
      (hermes-chat--respond-to-vault prompt-key prompt context))
     ((hermes-chat--batch-clarify-p prompt)
      (let ((responses (hermes-chat--batch-clarify-responses prompt response)))
        (unless (hermes-chat--prompt-owner-current-p context)
          (user-error "Hermes prompt request is no longer current"))
        (hermes-chat--send-batch-clarify-response
         prompt-key prompt responses context)))
     (t
      (let ((answer (or response (hermes-chat--read-prompt-response prompt))))
        (unless (hermes-chat--prompt-owner-current-p context)
          (user-error "Hermes prompt request is no longer current"))
        (hermes-chat--send-prompt-response
         prompt-key prompt answer all nil preserve-response context))))))

(defun hermes-chat-cancel-prompt (&optional key)
  "Cancel pending prompt KEY by sending the protocol's safe empty/deny value."
  (interactive nil hermes-chat-mode)
  (let* ((prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key))
         (response (if (equal (hermes-chat--prompt-event-type prompt) "approval")
                       "deny"
                     "")))
    (hermes-chat--send-prompt-response prompt-key prompt response nil t)))

(add-hook 'hermes-chat-lifecycle-invalidation-hook #'hermes-chat--release-all-prompt-response-claims)

(provide 'hermes-chat-prompts)
;;; hermes-chat-prompts.el ends here
