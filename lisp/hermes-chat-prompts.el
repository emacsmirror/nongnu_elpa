;;; hermes-chat-prompts.el --- Prompt and approval responses for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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


;; The one sanctioned upward reference among the chat siblings:
;; `hermes-chat-dashboard' requires this file (its event path records
;; prompt requests here), so the respond dispatcher below cannot require
;; it back and declares this client accessor instead.
(declare-function hermes-chat--dashboard-control-client "hermes-chat-dashboard" ())

(defcustom hermes-chat-auto-prompt-requests t
  "Whether visible chat buffers should prompt for backend input requests.
When non-nil, approvals, sudo passwords, secrets, and terminal reads
automatically open the usual minibuffer prompt in a visible interactive chat.
Clarifications wait for the chat input or `hermes-chat-respond-to-prompt'.
Invisible buffers and batch sessions record every prompt and show a message."
  :type 'boolean
  :group 'hermes)

(defvar hermes-chat--auto-prompting-p nil
  "Non-nil while an automatic minibuffer prompt is reading a response.")

(defun hermes-chat--prompt-request-event-p (event)
  "Return non-nil when EVENT is a dashboard prompt request."
  (and (eq (plist-get event :type) 'status)
       (plist-get event :prompt-request-p)))

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

(defun hermes-chat--record-prompt-request (event assistant-id)
  "Record prompt request EVENT for ASSISTANT-ID and return display event."
  (if-let* ((key (hermes-chat--prompt-event-key event)))
      (let* ((prompt (hermes-chat--prepare-prompt-request
                      event key assistant-id))
             (table (hermes-chat--ensure-pending-prompts))
             (existing (gethash key table))
             (stored (if (and existing (hermes-chat--approval-prompt-p prompt))
                         (hermes-chat--approval-prompt-with-queue
                          (append (plist-get existing :prompt-queue)
                                  (list prompt)))
                       (if (hermes-chat--approval-prompt-p prompt)
                           (hermes-chat--approval-prompt-with-queue
                            (list prompt))
                         prompt)))
             (token (and existing (plist-get existing :response-token))))
        (when token
          (setq stored (plist-put stored :response-token token)))
        (puthash key stored table)
        stored)
    event))

(defun hermes-chat--ensure-auto-prompt-keys ()
  "Return the current buffer's scheduled auto-prompt key table."
  (or hermes-chat--auto-prompt-keys
      (setq hermes-chat--auto-prompt-keys (make-hash-table :test #'equal))))

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

(defun hermes-chat--run-auto-prompt (buffer key)
  "Prompt for pending prompt KEY in BUFFER, when it is still safe to do so."
  (hermes-chat--in-buffer buffer
    (when (hash-table-p hermes-chat--auto-prompt-keys)
      (remhash key hermes-chat--auto-prompt-keys))
    (when-let* ((prompt (and hermes-chat--pending-prompts
                             (gethash key hermes-chat--pending-prompts))))
      (cond
       ((not (hermes-chat--auto-prompt-schedulable-p buffer)) nil)
       ((not (zerop (minibuffer-depth)))
        (hermes-chat--schedule-auto-prompt prompt t 0.25))
       (t
        (condition-case err
            (let ((hermes-chat--auto-prompting-p t))
              (hermes-chat-respond-to-prompt key))
          (quit
           (message "Hermes prompt left pending: %s" key))
          (user-error
           (message "%s" (error-message-string err)))
          (error
           (message "Hermes auto prompt failed: %s"
                    (error-message-string err)))))))))

(defun hermes-chat--schedule-auto-prompt (prompt &optional quiet delay)
  "Announce PROMPT and schedule an automatic minibuffer response prompt.
When QUIET is non-nil, do not emit another echo-area notice.  DELAY is the
number of seconds to wait before trying to prompt."
  (when-let* ((key (plist-get prompt :prompt-key)))
    (unless quiet
      (message "%s (%s)"
               (hermes-chat--prompt-notice-text prompt)
               (if (equal (hermes-chat--prompt-event-type prompt) "clarify")
                   "answer in chat with RET or C-c C-a"
                 "respond with C-c C-a")))
    (when (and (not (equal (hermes-chat--prompt-event-type prompt) "clarify"))
               (hermes-chat--auto-prompt-schedulable-p (current-buffer)))
      (let ((scheduled (hermes-chat--ensure-auto-prompt-keys)))
        (unless (gethash key scheduled)
          (puthash key t scheduled)
          (run-at-time (or delay 0) nil #'hermes-chat--run-auto-prompt
                       (current-buffer) key))))))

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
        (remhash key hermes-chat--pending-prompts))
      (when keys
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
    ("terminal" "Terminal read")
    (_ "Prompt")))

(defun hermes-chat--first-pending-prompt ()
  "Return the first pending prompt in deterministic key order."
  (and-let* ((key (car (hermes-chat--pending-prompt-keys))))
    (gethash key hermes-chat--pending-prompts)))

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

(defun hermes-chat--prompt-secret-response-p (prompt)
  "Return non-nil when PROMPT's answer must be redacted."
  (member (hermes-chat--prompt-event-type prompt) '("sudo" "secret")))

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
  (if (hermes-chat--prompt-secret-response-p prompt)
      (hermes-chat--redact-response-value message response)
    message))

(defun hermes-chat--prompt-choices (prompt)
  "Return PROMPT choices as strings, or nil."
  (and-let* ((choices (hermes-chat--event-value prompt '(:choices))))
    (delq nil (mapcar #'hermes-chat--scalar-string
                      (if (vectorp choices) (append choices nil) choices)))))

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

(defun hermes-chat--read-approval-response (prompt)
  "Read an approval response for PROMPT."
  (let* ((candidates (hermes-chat--approval-response-candidates prompt))
         (default (if hermes-chat--auto-prompting-p
                      (or (car (rassoc nil candidates)) (caar candidates))
                    (caar candidates)))
         (choice (completing-read "Approval decision: "
                                  (mapcar #'car candidates) nil t nil nil
                                  default))
         (candidate (assoc choice candidates)))
    (unless candidate
      (user-error "Unknown approval decision: %s" choice))
    (or (cdr candidate) (keyboard-quit))))

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

(defun hermes-chat--read-prompt-response (prompt)
  "Read a response for PROMPT using an Emacs-native minibuffer UI."
  (pcase (hermes-chat--prompt-event-type prompt)
    ("approval"
     (hermes-chat--read-approval-response prompt))
    ("clarify"
     (if-let* ((choices (hermes-chat--prompt-choices prompt)))
         ;; Choices are suggestions, not a closed set: the agent's clarify tool
         ;; always lets the user type their own answer, so do not require a match.
         (completing-read "Clarify: " choices)
       (read-string (or (hermes-chat--event-string prompt '(:question :content))
                        "Clarify: "))))
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
    (if next
        (progn
          (puthash key next hermes-chat--pending-prompts)
          (hermes-chat--upsert-transport-entry
           (or (plist-get next :assistant-id)
               (plist-get prompt :assistant-id))
           next))
      (remhash key hermes-chat--pending-prompts))
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
    (let ((message (format "%s %s"
                           (hermes-chat--prompt-display-name prompt)
                           (if canceled "canceled" "response sent"))))
      (hermes-chat--insert-local-status message (if canceled 'error 'done))
      (unless (hermes-chat--show-pending-prompt-state next-prompt)
        (hermes-chat--set-header-state
         :status (if (hermes-chat--active-turn-p) 'running 'ready)
         :activity message))
      (when next-prompt
        (hermes-chat--schedule-auto-prompt next-prompt)))))

(defun hermes-chat--approval-response-unresolved-p (prompt result)
  "Return non-nil when approval PROMPT RESULT resolved no backend prompt."
  (and (hermes-chat--approval-prompt-p prompt)
       (equal (hermes-transport--get result 'resolved) 0)))

(defun hermes-chat--prompt-response-stale (context prompt)
  "Clear stale PROMPT owned by CONTEXT without claiming a response was sent."
  (let ((message (format "%s request no longer pending"
                         (hermes-chat--prompt-display-name prompt)))
        (next-prompt
         (hermes-chat--advance-prompt-response
          context prompt (plist-get context :response-count))))
    (hermes-chat--insert-local-status message 'error)
    (unless (hermes-chat--show-pending-prompt-state next-prompt)
      (hermes-chat--set-header-state
       :status (if (hermes-chat--active-turn-p) 'running 'ready)
       :activity message))
    (when next-prompt
      (hermes-chat--schedule-auto-prompt next-prompt))))

(defun hermes-chat--prompt-missing-error-p (message)
  "Return non-nil when MESSAGE reports that the backend prompt is gone."
  (and (stringp message)
       (string-match-p "\\bno pending\\b" (downcase message))))

(defun hermes-chat--restore-prompt-response (response)
  "Restore failed chat-tail prompt RESPONSE without queueing a new turn."
  (when-let* ((text (hermes-transport--non-empty-string response)))
    (if (string-empty-p (string-trim (hermes-chat-input-string)))
        (hermes-chat--replace-input-tail text)
      (hermes-chat--append-input-tail text)
      (hermes-chat--insert-local-status
       "Restored failed prompt response after current draft" 'error))))

(defun hermes-chat--prompt-response-rejected
    (context prompt response message &optional preserve-response)
  "Render rejection MESSAGE for PROMPT and RESPONSE owned by CONTEXT.
When PRESERVE-RESPONSE is non-nil, keep RESPONSE recoverable in chat input."
  (let ((next-prompt
         (if (hermes-chat--prompt-missing-error-p message)
             (hermes-chat--advance-prompt-response
              context prompt (plist-get context :response-count))
           (hermes-chat--release-prompt-response context)
           nil)))
    (hermes-chat--command-error
     (hermes-chat--prompt-safe-error prompt response message))
    (when preserve-response
      (hermes-chat--restore-prompt-response response))
    (when next-prompt
      (hermes-chat--show-pending-prompt-state next-prompt)
      (hermes-chat--schedule-auto-prompt next-prompt))))

(defun hermes-chat--prompt-response-in-flight-p (key)
  "Return non-nil when prompt KEY already has a response in flight."
  (and-let* ((prompt (and (hash-table-p hermes-chat--pending-prompts)
                          (gethash key hermes-chat--pending-prompts))))
    (plist-get prompt :response-token)))

(defun hermes-chat--prompt-response-context (client key prompt all)
  "Claim ownership context for CLIENT, KEY, PROMPT, and ALL scope."
  (when (hermes-chat--prompt-response-in-flight-p key)
    (user-error "Hermes is accepting the previous prompt response"))
  (let ((token (list key))
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
  (when-let* ((prompt (gethash (plist-get context :key)
                               hermes-chat--pending-prompts)))
    (puthash (plist-get context :key)
             (plist-put (copy-sequence prompt) :response-token nil)
             hermes-chat--pending-prompts)))

(defun hermes-chat--prompt-response-current-p (context)
  "Return non-nil when prompt response CONTEXT still owns this chat."
  (and (eq hermes-chat--dashboard-client (plist-get context :client))
       (equal hermes-chat--dashboard-active-session-id
              (plist-get context :session-id))
       (= hermes-chat--lifecycle-generation (plist-get context :generation))
       (eq hermes-chat--pending-prompts (plist-get context :prompts))
       (eq (plist-get
            (gethash (plist-get context :key) hermes-chat--pending-prompts)
            :response-token)
           (plist-get context :token))))

(defun hermes-chat--prompt-success-callback (context prompt canceled)
  "Return a success callback for PROMPT response owned by CONTEXT."
  (lambda (result)
    (hermes-chat--in-buffer (plist-get context :buffer)
      (when (hermes-chat--prompt-response-current-p context)
        (if (hermes-chat--approval-response-unresolved-p prompt result)
            (hermes-chat--prompt-response-stale context prompt)
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

(defun hermes-chat--approval-session-id (prompt)
  "Return the dashboard session id for approval PROMPT."
  (or (hermes-chat--event-string prompt '(:session-id :session_id))
      hermes-chat--dashboard-active-session-id))

(defun hermes-chat--request-prompt-id (key prompt)
  "Return request id for prompt KEY/PROMPT."
  (or (hermes-chat--event-string prompt '(:request-id :request_id)) key))

(defun hermes-chat--send-prompt-response
    (key prompt response all canceled &optional preserve-response)
  "Send RESPONSE for prompt KEY/PROMPT through the dashboard transport."
  (let* ((client (hermes-chat--dashboard-control-client))
         (context (hermes-chat--prompt-response-context
                   client key prompt all))
         (type (hermes-chat--prompt-event-type prompt)))
    (condition-case err
        (pcase type
          ("approval"
           (hermes-dashboard-transport-approval-respond
            client :session-id (hermes-chat--approval-session-id prompt)
            :choice response :all (and all t)
            :resolve (hermes-chat--prompt-success-callback
                      context prompt canceled)
            :reject (hermes-chat--prompt-reject-callback
                     context prompt response preserve-response)))
          ((or "clarify" "sudo" "secret")
           (funcall (pcase type
                      ("clarify" #'hermes-dashboard-transport-clarify-respond)
                      ("sudo" #'hermes-dashboard-transport-sudo-respond)
                      ("secret" #'hermes-dashboard-transport-secret-respond))
                    client (hermes-chat--request-prompt-id key prompt) response
                    (hermes-chat--prompt-success-callback
                     context prompt canceled)
                    (hermes-chat--prompt-reject-callback
                     context prompt response preserve-response)))
          ("terminal"
           (hermes-dashboard-transport-terminal-read-respond
            client (hermes-chat--request-prompt-id key prompt) response
            (hermes-chat--prompt-success-callback
             context prompt canceled)
            (hermes-chat--prompt-reject-callback
             context prompt response preserve-response)))
          (_ (user-error "Unsupported Hermes prompt type: %s" type)))
      (error
       (when (hermes-chat--prompt-response-current-p context)
         (hermes-chat--prompt-response-rejected
          context prompt response (error-message-string err)
          preserve-response))))))

(defun hermes-chat-respond-to-prompt (&optional key response all preserve-response)
  "Respond to pending prompt KEY with RESPONSE.
When called interactively, select the prompt and read RESPONSE in the
minibuffer.  With prefix argument ALL, approval responses apply to all pending
approvals in the dashboard session.  PRESERVE-RESPONSE keeps programmatic
chat-tail input recoverable when the request fails."
  (interactive (list nil nil current-prefix-arg))
  (let* ((prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key)))
    (when (hermes-chat--prompt-response-in-flight-p prompt-key)
      (user-error "Hermes is accepting the previous prompt response"))
    (let ((answer (or response (hermes-chat--read-prompt-response prompt))))
      (hermes-chat--send-prompt-response
       prompt-key prompt answer all nil preserve-response))))

(defun hermes-chat-cancel-prompt (&optional key)
  "Cancel pending prompt KEY by sending the protocol's safe empty/deny value."
  (interactive)
  (let* ((prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key))
         (response (if (equal (hermes-chat--prompt-event-type prompt) "approval")
                       "deny"
                     "")))
    (hermes-chat--send-prompt-response prompt-key prompt response nil t)))

(provide 'hermes-chat-prompts)
;;; hermes-chat-prompts.el ends here
