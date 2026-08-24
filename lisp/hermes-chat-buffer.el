;;; hermes-chat-buffer.el --- EWOC buffer spine for Hermes chat  -*- lexical-binding: t; -*-

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

;; Stateful EWOC rendering and editable-tail buffer helpers for the Hermes
;; chat UI: the EWOC spine, the writable input tail, the header line, the
;; shared entry/header-state primitives, and the queue/drain input flow.
;; One area of the single logical chat module (see the AGENTS.md map); it
;; depends only downward (format/render) and reaches the submit pipeline in
;; `hermes-chat' through `hermes-chat--submit-function', never by name.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'hermes-chat-format)
(require 'hermes-chat-render)

(defvar hermes-instance)
(defvar hermes-instances)

(defmacro hermes-chat--in-buffer (buffer &rest body)
  "Evaluate BODY in BUFFER when it is live, else do nothing.
Lets asynchronous transport callbacks run in their originating chat buffer
without each one repeating the liveness guard."
  (declare (indent 1) (debug (form body)))
  `(when (buffer-live-p ,buffer)
     (with-current-buffer ,buffer
       ,@body)))

(defmacro hermes-chat--in-lifetime (buffer lifetime &rest body)
  "Evaluate BODY when BUFFER still owns chat LIFETIME."
  (declare (indent 2) (debug (form form body)))
  `(hermes-chat--in-buffer ,buffer
     (when (hermes-chat--current-lifetime-p ,lifetime)
       ,@body)))



(defvar-local hermes-chat--auto-prompt-keys nil
  "Scheduled auto-prompt keys for the current chat buffer.")
(defvar-local hermes-chat--dashboard-active-session-id nil
  "Live dashboard session id for the current chat buffer.")
(defvar-local hermes-chat--dashboard-client nil
  "Shared dashboard transport client this chat buffer is attached to.")
(defvar-local hermes-chat--dashboard-token nil
  "This chat buffer's subscriber token on the shared dashboard client.")
(defvar-local hermes-chat--dashboard-detached-assistant-id nil
  "Detached dashboard assistant entry id for the current chat buffer.")
(defvar-local hermes-chat--dashboard-running-p nil
  "Non-nil while the dashboard reports that this session is running.")
(defvar-local hermes-chat--dashboard-session-ready-p nil
  "Non-nil when a dashboard session is ready for the current chat buffer.")
(defvar-local hermes-chat--dashboard-stream-assistant-id nil
  "Assistant entry id receiving live dashboard stream events.")
(defvar-local hermes-chat--dashboard-interim-assistant-id nil
  "Most recent assistant entry sealed by a dashboard interim boundary.")
(defvar-local hermes-chat--dashboard-suppress-stream-p nil
  "Non-nil when live dashboard stream events are suppressed.")
(defvar-local hermes-chat--dashboard-idle-count 0
  "Number of explicit dashboard idle events seen by this chat buffer.")
(defvar-local hermes-chat--dashboard-last-start-idle-count 0
  "Idle count observed at the most recent dashboard `message.start'.")
(defvar-local hermes-chat--server-queued-assistant-id nil
  "Assistant entry waiting for a backend-owned queued turn to start.")
(defvar-local hermes-chat--server-queued-user-id nil
  "User entry owned by the backend's queued next turn.")
(defvar-local hermes-chat--server-queued-after-idle-count nil
  "Idle count that must advance before the server-queued turn can start.")
(defvar-local hermes-chat--server-queued-prior-terminal-p nil
  "Non-nil after the prior turn ends without an explicit idle event.")
(defvar-local hermes-chat--busy-submit-context nil
  "Busy dashboard submission awaiting its policy result.")
(defvar-local hermes-chat--unsettled-submit-context nil
  "Dashboard submit context whose RPC result has not arrived yet.")
(defvar-local hermes-chat--prepared-submit-assistant-id nil
  "Assistant reset at a queued `message.start' before submit settlement.")
(defvar-local hermes-chat--ewoc nil
  "EWOC displaying chat transcript entries.")
(defvar-local hermes-chat--input-marker nil
  "Marker at the beginning of the writable chat input tail.")
(defvar-local hermes-chat--nodes nil
  "Hash table mapping Hermes entry ids to EWOC nodes.")
(defvar-local hermes-chat--pending-assistant-id nil
  "ID of the assistant entry awaiting transport completion.")
(defvar-local hermes-chat--pending-prompts nil
  "Pending dashboard prompt requests by prompt key.")
(defvar-local hermes-chat--process nil
  "Current Hermes transport process or token.")
(defvar-local hermes-chat--queued-messages nil
  "FIFO queue of message plists waiting for an idle Hermes session.")
(defvar-local hermes-chat--queued-submit-id nil
  "Queued message id currently awaiting transport acceptance.")
(defvar-local hermes-chat--input-history nil
  "Sent chat inputs, newest first.")
(defvar-local hermes-chat--input-history-index nil
  "Current index while navigating `hermes-chat--input-history'.")
(defvar-local hermes-chat--input-history-draft ""
  "Draft restored after moving forward past the newest history entry.")
(defvar-local hermes-chat--queue-panel-buffer nil
  "Side-panel buffer displaying this chat buffer's queued messages.")
(defvar-local hermes-chat-queue-panel--owner nil
  "Chat buffer whose FIFO is displayed by this queue panel.")
(defvar-local hermes-chat--session-id nil
  "Durable Hermes session key for the current chat buffer.")
(defvar-local hermes-chat--dashboard-create-model nil
  "Buffer-local model override applied after the next `session.create'.
Nil means inherit the profile default.  Applied to the fresh session via
`config.set' (the create handler ignores runtime overrides).  Kept
buffer-local so two chat buffers sharing one dashboard socket each create
their own session with their own runtime.")
(defvar-local hermes-chat--dashboard-create-provider nil
  "Buffer-local provider override applied after the next `session.create'.")
(defvar-local hermes-chat--dashboard-create-reasoning-effort nil
  "Buffer-local reasoning effort applied after the next `session.create'.")
(defvar-local hermes-chat--dashboard-create-fast-p nil
  "Buffer-local fast/service-tier flag applied after the next `session.create'.")
(defvar-local hermes-chat--create-override-owner nil
  "Identity owning application of pending create-time runtime overrides.")
(defvar-local hermes-chat--create-overrides-retry-session-id nil
  "Session id owning unapplied create-time runtime overrides, or nil.")
(defvar-local hermes-chat--transport-generation 0
  "Monotonic transport-callback generation for the current chat buffer.
Bumped per turn and transcript reset so stale async callbacks become obsolete.
Owned here; `hermes-chat' and `hermes-chat-dashboard' only re-declare it.")
(defvar hermes-chat--lifetime-sequence 0
  "Process-wide sequence issuing opaque chat lifetime tokens.")
(defvar-local hermes-chat--lifecycle-generation nil
  "Opaque process-unique token for the current chat mode lifetime.")
(defvar-local hermes-chat--cleanup-done-p nil
  "Non-nil after resources for the current chat lifetime were released.")
(defvar-local hermes-chat--ansi-fragments nil
  "Hash of entry key to pending ANSI escape fragment.")
(defvar-local hermes-chat--interrupted-assistant-id nil
  "Assistant entry whose remaining turn events must be ignored.")
(defvar-local hermes-chat--interrupted-events nil
  "Turn events held while an interrupt request is unresolved.")
(defvar-local hermes-chat--interrupt-request-pending-p nil
  "Non-nil until the current `session.interrupt' request settles.")
(defvar hermes-chat--transient-entry-roles)

(defun hermes-chat--next-lifetime-token ()
  "Return a fresh process-unique chat lifetime token."
  (cons (cl-incf hermes-chat--lifetime-sequence) nil))

(defun hermes-chat--current-lifetime-p (lifetime)
  "Return non-nil when LIFETIME owns the exact current chat mode."
  (and (eq major-mode 'hermes-chat-mode)
       (eql lifetime hermes-chat--lifecycle-generation)))

(defun hermes-chat--transport-entry-role (event)
  "Return EWOC entry role for transport EVENT."
  (pcase (plist-get event :type)
    ('status 'status)
    ('progress 'progress)
    ('tool 'tool)
    ('commentary 'commentary)
    ('diff 'diff)
    ('unknown 'status)))

(defun hermes-chat--commentary-event-name (event)
  "Return EVENT's commentary event name in lowercase, or nil."
  (and-let* ((name (hermes-chat--event-string event '(:event))))
    (downcase name)))

(defun hermes-chat--commentary-delta-p (event)
  "Return non-nil when EVENT is a commentary/thinking delta."
  (and (eq (plist-get event :type) 'commentary)
       (and-let* ((name (hermes-chat--commentary-event-name event)))
         (or (member name '("reasoning.delta" "thinking.delta"))
             (string-suffix-p ".delta" name)))))

(defun hermes-chat--commentary-key (event)
  "Return stable EWOC key for commentary EVENT."
  (let ((name (hermes-chat--commentary-event-name event)))
    (if (or (null name)
            (member name '("reasoning.delta" "thinking.delta"
                           "reasoning.available")))
        "thinking"
      name)))

(defun hermes-chat--transport-entry-status (event)
  "Return EWOC entry status for transport EVENT."
  (or (hermes-chat--event-value event '(:status))
      (and (eq (plist-get event :type) 'unknown) 'error)
      (and (eq (plist-get event :type) 'commentary) 'running)
      (hermes-chat--event-phase event)
      (pcase (plist-get event :type)
        ((or 'status 'progress 'tool) 'running)
        (_ 'done))))

(defun hermes-chat--transport-entry-content (event)
  "Return display content for transport EVENT."
  (pcase (plist-get event :type)
    ('status (hermes-chat--format-status-event event))
    ('progress (hermes-chat--format-progress-event event))
    ('tool (hermes-chat--format-tool-event event))
    ((or 'commentary 'diff) (hermes-chat--event-string event '(:content :text)))
    ('unknown (hermes-chat--unknown-event-content event))
    (_ nil)))

(defun hermes-chat--transport-key-fragment (event keys)
  "Return a stable key fragment from EVENT using KEYS."
  (and-let* ((value (hermes-chat--event-string event keys)))
    (unless (string-empty-p value)
      value)))

(defun hermes-chat--session-scoped-status-id (event)
  "Return a buffer-global id for operational status EVENT, or nil.
`warn', `compacting', and `compressing' are session-scoped notices: the
gateway may re-emit them on every blocked or in-flight compression, and
assistant ids rotate mid-turn.  Key them by kind so later frames replace
one line instead of appending."
  (let ((status (hermes-chat--status-name (plist-get event :status))))
    (and (member status '("warn" "compacting" "compressing"))
         (concat "status:" status))))

(defun hermes-chat--transport-entry-id (event)
  "Return stable EWOC entry id for keyed transport EVENT, or nil."
  (pcase (plist-get event :type)
    ('status
     (or (hermes-chat--session-scoped-status-id event)
         (when-let* ((key (or (hermes-chat--transport-key-fragment
                               event '(:prompt-key :prompt_key
                                                   :request-id :request_id
                                                   :status-key :status_key :key
                                                   :notification-key
                                                   :run-id :run_id
                                                   :message-id :message_id))
                              (hermes-chat--transport-key-fragment
                               event '(:event)))))
           (concat "status:" key))))
    ((or 'progress 'tool)
     ;; Without a call id the tool name is the key, so two concurrent
     ;; same-named calls coalesce into one entry: gateways that interleave
     ;; tools are expected to tag frames with a tool_call_id.
     (when-let* ((key (or (hermes-chat--transport-key-fragment
                           event '(:tool-call-id :tool_call_id :call-id
						 :call_id :id :message-id
						 :message_id :index :seq))
                          (hermes-chat--tool-name event)
                          (hermes-chat--transport-key-fragment
                           event '(:event)))))
       (concat "tool:" key)))
    ('commentary
     (concat "commentary:" (hermes-chat--commentary-key event)))
    ('unknown
     (when-let* ((key (hermes-chat--transport-key-fragment
                       event '(:event :session-id :session_id))))
       (concat "unknown:" key)))))

(defun hermes-chat--transport-entry-metadata (assistant-id event)
  "Return metadata plist for transport EVENT tied to ASSISTANT-ID."
  (list :assistant-id assistant-id :event event))

(defconst hermes-chat--transient-summary-width 100
  "Maximum width of a collapsed transient summary line.")

(defun hermes-chat--first-line (text)
  "Return TEXT's first line, trimmed and truncated for a one-line summary."
  (truncate-string-to-width
   (string-trim (car (split-string text "\n")))
   hermes-chat--transient-summary-width nil nil "…"))

(defun hermes-chat--multiline-content-p (text)
  "Return non-nil when TEXT spans more than one line."
  (string-match-p "\n" (string-trim-right text)))

(defun hermes-chat--insert-transient-toggle (entry summary expanded)
  "Insert a toggle labeled SUMMARY for transient ENTRY in EXPANDED state."
  (let ((start (point)))
    (insert (if expanded "▾ " "▸ ") summary)
    (make-text-button start (point)
                      'face 'shadow
                      'mouse-face 'highlight
                      'follow-link t
                      'help-echo "Toggle full output"
                      'hermes-chat-entry-id (plist-get entry :id)
                      'action #'hermes-chat--toggle-entry-button)))

(defun hermes-chat--insert-transient-content (entry)
  "Insert a compact transient transport ENTRY.
Multiline content collapses to a one-line summary with a `▸'/`▾' toggle, like
the thinking disclosure; diffs become View Diff links."
  (let ((content (or (plist-get entry :content) "")))
    (unless (string-empty-p content)
      (insert (propertize (format "  %s "
                                  (hermes-chat--status-icon
                                   (plist-get entry :status)))
                          'face (hermes-chat--status-face
                                 (plist-get entry :status))))
      (let ((blocks (hermes-chat--diff-blocks content)))
        (if (and (memq (plist-get entry :role) '(tool progress))
                 (hermes-chat--multiline-content-p content)
                 (null blocks))
            (let ((expanded (hermes-chat--entry-expanded-p entry)))
              (hermes-chat--insert-transient-toggle
               entry (hermes-chat--first-line content) expanded)
              (insert "\n")
              (when expanded
                (hermes-chat--insert-shadow content)
                (insert "\n")))
          (hermes-chat--insert-diffed content #'hermes-chat--insert-shadow blocks)
          (insert "\n"))))))

(defun hermes-chat--compact-commentary-paragraph (paragraph)
  "Return PARAGRAPH with token-stream line noise collapsed."
  (replace-regexp-in-string
   "[ \t]+" " "
   (string-trim
    (replace-regexp-in-string "[ \t]*\n[ \t]*" " " paragraph))))

(defun hermes-chat--commentary-normalize-line-endings (content)
  "Return CONTENT with escaped newline artifacts normalized."
  (let ((text (hermes-chat--sanitize-content content)))
    ;; The gateway double-encodes reasoning newlines as the literal "^J"
    ;; artifact (see the reasoning-collapse test), so it is un-escaped here.
    ;; This is scoped to commentary content only, where the artifact occurs.
    (dolist (artifact '(("\\r\\n" . "\n") ("\\n" . "\n")
                        ("\\r" . "\n") ("^J" . "\n")))
      (setq text (replace-regexp-in-string
                  (regexp-quote (car artifact)) (cdr artifact) text t t)))
    text))

(defun hermes-chat--commentary-display-content (content)
  "Return CONTENT formatted for expanded thinking display."
  (let* ((text (hermes-chat--commentary-normalize-line-endings content))
         (paragraphs (split-string
                      (string-trim text) "[ \t]*\n[ \t]*\n[ \t\n]*" t)))
    (string-join
     (mapcar #'hermes-chat--compact-commentary-paragraph paragraphs)
     "\n\n")))

(defun hermes-chat--insert-commentary-toggle (entry expanded)
  "Insert a toggle button for commentary ENTRY in EXPANDED state."
  (insert "  ")
  (let ((start (point)))
    (insert (if expanded "▾ Reasoning" "▸ Reasoning"))
    (make-text-button start (point)
                      'face 'shadow
                      'mouse-face 'highlight
                      'follow-link t
                      'help-echo "Toggle Hermes reasoning"
                      'hermes-chat-entry-id (plist-get entry :id)
                      'action #'hermes-chat--toggle-entry-button))
  (insert "\n"))

(defun hermes-chat--insert-commentary-content (entry)
  "Insert collapsed or expanded commentary ENTRY."
  (let ((expanded (hermes-chat--entry-expanded-p entry)))
    (hermes-chat--insert-commentary-toggle entry expanded)
    (when expanded
      (let ((content (hermes-chat--commentary-display-content
                      (or (plist-get entry :content) ""))))
        (unless (string-empty-p content)
          (let ((start (point)))
            (insert content "\n")
            (add-text-properties start (point) '(face shadow))))))))

(defun hermes-chat--insert-user-content (content)
  "Insert user CONTENT with a compact prompt prefix."
  (insert (propertize "> " 'face 'font-lock-keyword-face)
          (propertize content 'face 'hermes-chat-user-input)
          "\n"))

(defun hermes-chat--insert-entry-content (content &optional streaming)
  "Insert assistant or system CONTENT.
While STREAMING, insert CONTENT as plain text so a long reply is not
re-fontified on every delta.  Once the entry settles, render CONTENT as
markdown with diff blocks replaced by View Diff links and embedded images
lifted into inline images."
  (if streaming
      (insert content "\n")
    (hermes-chat--insert-content-with-images content #'hermes-chat--insert-markdown)
    (insert "\n")))

(defun hermes-chat--print-entry (entry)
  "Insert a display representation of chat ENTRY at point."
  (let ((role (plist-get entry :role))
        (content (or (plist-get entry :content) "")))
    (cond
     ((eq role 'user)
      (hermes-chat--insert-user-content content))
     ((eq role 'commentary)
      (hermes-chat--insert-commentary-content entry))
     ((eq role 'diff)
      (unless (string-empty-p content)
        (hermes-chat--insert-diff-entry content)))
     ((eq role 'background)
      (hermes-chat--insert-background-entry entry))
     ((memq role hermes-chat--transient-entry-roles)
      (hermes-chat--insert-transient-content entry))
     ((not (string-empty-p content))
      (hermes-chat--insert-entry-content
       content (eq (plist-get entry :status) 'streaming))))))

(defun hermes-chat--input-position ()
  "Return the numeric input marker position."
  (and (markerp hermes-chat--input-marker)
       (marker-position hermes-chat--input-marker)))

(defun hermes-chat--point-in-input-p ()
  "Return non-nil if point is in the writable input tail."
  (let ((pos (hermes-chat--input-position)))
    (and pos (>= (point) pos))))

(defun hermes-chat--protect-transcript ()
  "Make transcript and prompt read-only while keeping input tail writable.
Only touch subranges that actually need the change: this runs after every
streamed delta, so rewriting properties across the whole transcript would
cost O(buffer) interval churn per chunk.  Do not record these internal
text-property changes in the undo list."
  (when-let* ((pos (hermes-chat--input-position)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (start (point-min)))
      (while (and (< start pos)
                  (setq start (text-property-not-all start pos 'read-only t)))
        (let ((end (or (text-property-any start pos 'read-only t) pos)))
          (add-text-properties start end
                               '(read-only t front-sticky t rear-nonsticky t))
          (setq start end)))
      (remove-text-properties pos (point-max)
                              '(read-only nil front-sticky nil rear-nonsticky nil)))))

(defmacro hermes-chat--preserve-input-point (&rest body)
  "Run BODY preserving point's offset into the writable input tail."
  (declare (indent 0) (debug t))
  `(let ((offset (and (hermes-chat--point-in-input-p)
                      (- (point) (hermes-chat--input-position)))))
     (prog1 (progn ,@body)
       (hermes-chat--protect-transcript)
       (when offset
         (goto-char (min (point-max)
                         (+ (hermes-chat--input-position) offset)))))))

(defun hermes-chat--separator ()
  "Return a full-width rule string separating the transcript from the input."
  (propertize " " 'display '(space :width text) 'face 'hermes-chat-separator))

(defun hermes-chat--setup-buffer ()
  "Initialize the current buffer as an empty Hermes chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (cl-incf hermes-chat--transport-generation)
    (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token)
          hermes-chat--cleanup-done-p nil)
    (erase-buffer)
    (setq-local header-line-format '(:eval (hermes-chat--header-line)))
    (hermes-chat--reset-header-state)
    (setq hermes-chat--nodes (make-hash-table :test 'equal)
          hermes-chat--pending-assistant-id nil
          hermes-chat--queued-messages nil
          hermes-chat--queued-submit-id nil
          hermes-chat--pending-prompts (make-hash-table :test #'equal)
          hermes-chat--auto-prompt-keys (make-hash-table :test #'equal)
          hermes-chat--process nil
          hermes-chat--dashboard-client nil
          hermes-chat--dashboard-token nil
          hermes-chat--dashboard-session-ready-p nil
          hermes-chat--dashboard-active-session-id nil
          hermes-chat--dashboard-detached-assistant-id nil
          hermes-chat--dashboard-running-p nil
          hermes-chat--create-override-owner nil
          hermes-chat--create-overrides-retry-session-id nil
          hermes-chat--dashboard-stream-assistant-id nil
          hermes-chat--dashboard-suppress-stream-p nil
          hermes-chat--dashboard-idle-count 0
          hermes-chat--dashboard-last-start-idle-count 0
          hermes-chat--server-queued-assistant-id nil
          hermes-chat--server-queued-user-id nil
          hermes-chat--server-queued-after-idle-count nil
          hermes-chat--server-queued-prior-terminal-p nil
          hermes-chat--busy-submit-context nil
          hermes-chat--unsettled-submit-context nil
          hermes-chat--prepared-submit-assistant-id nil
          hermes-chat--interrupted-assistant-id nil
          hermes-chat--interrupted-events nil
          hermes-chat--interrupt-request-pending-p nil
          hermes-chat--ansi-fragments (make-hash-table :test #'equal)
          hermes-chat--session-id nil
          hermes-chat--ewoc (ewoc-create #'hermes-chat--print-entry
                                         nil
                                         (concat "\n" (hermes-chat--separator) "\n")
                                         'nosep))
    (goto-char (point-max))
    ;; Keep the marker at the beginning of the editable input tail.  With an
    ;; insertion-type marker, normal typing moves the marker after the inserted
    ;; text, making the input appear empty to `hermes-chat-input-string'.
    (setq hermes-chat--input-marker (copy-marker (point) nil))
    (hermes-chat--protect-transcript)
    (goto-char hermes-chat--input-marker)))

(defun hermes-chat--invalidate-transport-state ()
  "Invalidate callbacks and pending work before releasing this buffer's client."
  (cl-incf hermes-chat--transport-generation)
  (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token))
  (run-hooks 'hermes-chat-lifecycle-invalidation-hook)
  (when (hash-table-p hermes-chat--auto-prompt-keys)
    (clrhash hermes-chat--auto-prompt-keys))
  (setq hermes-chat--pending-assistant-id nil
        hermes-chat--queued-messages nil
        hermes-chat--queued-submit-id nil
        hermes-chat--process nil
        hermes-chat--dashboard-running-p nil
        hermes-chat--create-override-owner nil
        hermes-chat--create-overrides-retry-session-id nil
        hermes-chat--dashboard-stream-assistant-id nil
        hermes-chat--dashboard-interim-assistant-id nil
        hermes-chat--dashboard-suppress-stream-p nil
        hermes-chat--server-queued-assistant-id nil
        hermes-chat--server-queued-user-id nil
        hermes-chat--server-queued-after-idle-count nil
        hermes-chat--server-queued-prior-terminal-p nil
        hermes-chat--busy-submit-context nil
        hermes-chat--unsettled-submit-context nil
        hermes-chat--prepared-submit-assistant-id nil
        hermes-chat--interrupted-assistant-id nil
        hermes-chat--interrupted-events nil
        hermes-chat--interrupt-request-pending-p nil))

(defun hermes-chat--register-node (entry node)
  "Register ENTRY's ID for NODE and return NODE."
  (when-let* ((id (plist-get entry :id)))
    (puthash id node hermes-chat--nodes))
  node)

(defun hermes-chat--pending-assistant-node ()
  "Return the EWOC node of the pending assistant reply, if any."
  (and hermes-chat--pending-assistant-id
       hermes-chat--nodes
       (gethash hermes-chat--pending-assistant-id hermes-chat--nodes)))

(defun hermes-chat--insert-entry (entry &optional before-node)
  "Insert ENTRY into the current chat EWOC and return its node.
With BEFORE-NODE, insert ENTRY before that node instead of at the end, so the
agent's reply can stay last while tool/status/diff entries land above it."
  (let ((node (hermes-chat--preserve-input-point
               (let ((node (let ((inhibit-read-only t)
                                 (buffer-undo-list t))
                             (if before-node
                                 (ewoc-enter-before hermes-chat--ewoc
                                                    before-node entry)
                               (ewoc-enter-last hermes-chat--ewoc entry)))))
                 (hermes-chat--register-node entry node)))))
    (hermes-chat--notify-state-change)
    node))

(defun hermes-chat--entries ()
  "Return chat entries from the current buffer in display order."
  (ewoc-collect hermes-chat--ewoc #'identity))

(defun hermes-chat--update-entry (id function)
  "Update entry ID by applying FUNCTION to its entry plist.
Return the updated entry, or nil when ID names no live entry -- callers run
from WebSocket callbacks and timers, where the entry may already be gone
because the chat was cleared mid-turn, like `hermes-chat--remove-entry'."
  (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (let ((entry (hermes-chat--preserve-input-point
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t)
                        (entry (funcall function (ewoc-data node))))
                    (ewoc-set-data node entry)
                    (ewoc-invalidate hermes-chat--ewoc node)
                    entry))))
      (hermes-chat--notify-state-change)
      entry)))

(defun hermes-chat--remove-entry (id)
  "Remove chat entry ID from the EWOC and node table."
  (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (hermes-chat--preserve-input-point
     (let ((inhibit-read-only t)
           (buffer-undo-list t))
       (ewoc-delete hermes-chat--ewoc node)))
    (remhash id hermes-chat--nodes)
    (hermes-chat--notify-state-change)))

(defun hermes-chat--toggle-entry-expanded (id)
  "Toggle detail expansion for entry ID."
  (hermes-chat--update-entry
   id
   (lambda (entry)
     (let ((metadata (copy-sequence (plist-get entry :metadata))))
       (hermes-chat--entry-with
        entry :metadata
        (plist-put metadata :expanded
                   (not (hermes-chat--entry-expanded-p entry))))))))

(defun hermes-chat--toggle-entry-button (button)
  "Toggle the collapsible entry attached to BUTTON."
  (when-let* ((id (button-get button 'hermes-chat-entry-id)))
    (hermes-chat--toggle-entry-expanded id)))

(defun hermes-chat--assistant-ansi-key (assistant-id)
  "Return ANSI-fragment key for ASSISTANT-ID stream chunks."
  (list 'assistant assistant-id))

(defun hermes-chat--ansi-fragment (key)
  "Return the pending ANSI fragment for KEY, or nil."
  (and key hermes-chat--ansi-fragments
       (gethash key hermes-chat--ansi-fragments)))

(defun hermes-chat--record-ansi-fragment (key fragment)
  "Record ANSI FRAGMENT for KEY, or clear KEY when FRAGMENT is nil."
  (when key
    (unless hermes-chat--ansi-fragments
      (setq hermes-chat--ansi-fragments (make-hash-table :test #'equal)))
    (if fragment
        (puthash key fragment hermes-chat--ansi-fragments)
      (remhash key hermes-chat--ansi-fragments))))

(defun hermes-chat--clear-ansi-fragment (key)
  "Clear the pending ANSI fragment for KEY."
  (hermes-chat--record-ansi-fragment key nil))

(defun hermes-chat--sanitize-stream-content (content key)
  "Sanitize stream CONTENT using the pending ANSI fragment for KEY."
  (let ((result (hermes-chat--sanitize-content-with-fragment
                 content (hermes-chat--ansi-fragment key))))
    (hermes-chat--record-ansi-fragment key (cdr result))
    (car result)))

(defun hermes-chat--append-assistant-content (assistant-id content status)
  "Append CONTENT to ASSISTANT-ID and set STATUS."
  (let ((ansi-key (hermes-chat--assistant-ansi-key assistant-id)))
    (unless (eq status 'streaming)
      (hermes-chat--clear-ansi-fragment ansi-key))
    (hermes-chat--update-entry
     assistant-id
     (lambda (entry)
       (let ((text (concat (or (plist-get entry :content) "")
                           (if (eq status 'streaming)
                               (hermes-chat--sanitize-stream-content
                                content ansi-key)
                             (hermes-chat--sanitize-content content)))))
         (hermes-chat--entry-with
          entry
          :status status
          :content (hermes-chat--strip-session-id-lines text)))))))

(defun hermes-chat--mark-assistant (assistant-id status &optional content final)
  "Set ASSISTANT-ID to STATUS, optionally replacing CONTENT.
When FINAL is non-nil, strip any trailing transport metadata."
  (hermes-chat--clear-ansi-fragment
   (hermes-chat--assistant-ansi-key assistant-id))
  (hermes-chat--update-entry
   assistant-id
   (lambda (entry)
     (let ((text (if content content (or (plist-get entry :content) ""))))
       (if (or content final)
           (hermes-chat--entry-with
            entry
            :status status
            :content (hermes-chat--sanitize-assistant-content text final))
         (hermes-chat--entry-with entry :status status))))))

(defun hermes-chat--normalize-for-dedup (text)
  "Return TEXT with whitespace collapsed for echo comparison."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))

(defun hermes-chat--text-echoes-p (a b)
  "Return non-nil when normalized A and B duplicate each other."
  (let ((a (hermes-chat--normalize-for-dedup a))
        (b (hermes-chat--normalize-for-dedup b)))
    (and (not (string-empty-p a))
         (not (string-empty-p b))
         (or (string= a b)
             (string-prefix-p a b)
             (string-prefix-p b a)))))

(defun hermes-chat--entry-content-by-id (id)
  "Return entry ID's content in the current chat buffer, or nil."
  (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (plist-get (ignore-errors (ewoc-data node)) :content)))

(defun hermes-chat--thinking-entry-content (assistant-id)
  "Return ASSISTANT-ID's thinking/commentary content, or nil."
  (hermes-chat--entry-content-by-id
   (format "%s:commentary:thinking" assistant-id)))

(defun hermes-chat--thinking-only-final-content-p (assistant-id content)
  "Return non-nil when CONTENT only repeats ASSISTANT-ID's thinking entry."
  (let ((content (hermes-chat--normalize-for-dedup
                  (and content
                       (hermes-chat--sanitize-assistant-content content t))))
        (thinking (hermes-chat--normalize-for-dedup
                   (hermes-chat--thinking-entry-content assistant-id)))
        (assistant (hermes-chat--normalize-for-dedup
                    (hermes-chat--entry-content-by-id assistant-id))))
    (and (not (string-empty-p content))
         (string= content thinking)
         (string-empty-p assistant))))

(defun hermes-chat--thinking-echo-delta-p (assistant-id content)
  "Return non-nil when CONTENT is a transient echo of ASSISTANT-ID thinking."
  (let ((content (hermes-chat--normalize-for-dedup
                  (hermes-chat--sanitize-assistant-content content nil)))
        (thinking (hermes-chat--normalize-for-dedup
                   (hermes-chat--thinking-entry-content assistant-id)))
        (assistant (hermes-chat--normalize-for-dedup
                    (hermes-chat--entry-content-by-id assistant-id))))
    (and (string-empty-p assistant)
         (not (string-empty-p content))
         (string= content thinking))))

(defun hermes-chat--assistant-done-content (assistant-id content)
  "Return ASSISTANT-ID final CONTENT, suppressing thinking-only echo."
  (unless (hermes-chat--thinking-only-final-content-p assistant-id content)
    content))

(defun hermes-chat--drop-duplicate-thinking (assistant-id)
  "Remove ASSISTANT-ID's reasoning entry when it only repeats the reply.
Some providers emit `reasoning.available' equal to the final message; that is
noise, not a thinking process.  Reasoning that genuinely differs is kept."
  (when-let* ((tid (format "%s:commentary:thinking" assistant-id))
              (tnode (and hermes-chat--nodes (gethash tid hermes-chat--nodes)))
              (anode (gethash assistant-id hermes-chat--nodes)))
    (when (hermes-chat--text-echoes-p
           (plist-get (ignore-errors (ewoc-data tnode)) :content)
           (plist-get (ignore-errors (ewoc-data anode)) :content))
      (hermes-chat--remove-entry tid))))

(defun hermes-chat--reasoning-available-event-p (event)
  "Return non-nil when EVENT is a `reasoning.available' commentary preview."
  (and (eq (plist-get event :type) 'commentary)
       (equal (hermes-chat--commentary-event-name event) "reasoning.available")))

(defun hermes-chat--updated-transport-content (entry event content)
  "Return updated display CONTENT for ENTRY from transport EVENT."
  (let ((clean-content (hermes-chat--sanitize-content content)))
    (cond
     ((and entry (hermes-chat--commentary-delta-p event))
      (concat (or (plist-get entry :content) "") clean-content))
     ;; `reasoning.available' is a preview of the same reasoning entry already
     ;; built from `reasoning.delta' chunks (both key to `commentary:thinking');
     ;; never let the shorter preview shrink the fuller streamed reasoning.
     ((and entry (hermes-chat--reasoning-available-event-p event))
      (let ((existing (or (plist-get entry :content) "")))
        (if (>= (length existing) (length clean-content)) existing clean-content)))
     (t clean-content))))

(defun hermes-chat--upsert-transport-entry (assistant-id event)
  "Insert or update a compact transport EVENT for ASSISTANT-ID."
  (let* ((role (hermes-chat--transport-entry-role event))
         (event-id (hermes-chat--transport-entry-id event))
         (id (and event-id
                  (if (hermes-chat--session-scoped-status-id event)
                      event-id
                    (format "%s:%s" assistant-id event-id))))
         (status (hermes-chat--transport-entry-status event))
         (content (or (hermes-chat--transport-entry-content event) ""))
         (metadata (hermes-chat--transport-entry-metadata assistant-id event)))
    (when (and role (not (string-empty-p content)))
      (if (and id (gethash id hermes-chat--nodes))
          (hermes-chat--update-entry
           id
           (lambda (entry)
             (let ((metadata (hermes-chat--metadata-preserve-expanded
                              entry metadata)))
               (hermes-chat--entry-with
                entry
                :role role
                :status status
                :content (hermes-chat--updated-transport-content
                          entry event content)
                :metadata metadata
                :updated (current-time)))))
        (hermes-chat--insert-entry
         (hermes-chat--make-entry role content status id metadata)
         (hermes-chat--pending-assistant-node))))))

(defun hermes-chat--transient-entry-p (entry)
  "Return non-nil if ENTRY is a compact transport activity entry."
  (memq (plist-get entry :role)
        (cons 'commentary hermes-chat--transient-entry-roles)))

(defun hermes-chat--entry-assistant-id (entry)
  "Return ENTRY's owning assistant id from metadata, if any."
  (plist-get (plist-get entry :metadata) :assistant-id))

(defun hermes-chat--settle-transport-entries (assistant-id status)
  "Set active transport entries for ASSISTANT-ID to STATUS."
  (hermes-chat--preserve-input-point
   (let ((inhibit-read-only t)
         (buffer-undo-list t))
     (maphash
      (lambda (_id node)
        (when-let* ((entry (ignore-errors (ewoc-data node))))
          (when (and (hermes-chat--transient-entry-p entry)
                     (equal (hermes-chat--entry-assistant-id entry)
                            assistant-id)
                     (hermes-chat--active-status-p
                      (plist-get entry :status)))
            (ewoc-set-data node (hermes-chat--entry-with entry :status status))
            (ewoc-invalidate hermes-chat--ewoc node))))
      hermes-chat--nodes))))

(defun hermes-chat-input-string ()
  "Return the current input tail as a plain string."
  (let ((pos (hermes-chat--input-position)))
    (unless pos
      (user-error "No Hermes chat input marker in this buffer"))
    (buffer-substring-no-properties pos (point-max))))

(defun hermes-chat--delete-input-tail ()
  "Delete the current writable input tail."
  (delete-region (hermes-chat--input-position) (point-max)))

(defun hermes-chat--replace-input-tail (content)
  "Replace the current writable input tail with CONTENT."
  (hermes-chat--delete-input-tail)
  (goto-char (hermes-chat--input-position))
  (insert content))

(defun hermes-chat--append-input-tail (content)
  "Append CONTENT to the writable input tail, preserving existing draft text."
  (goto-char (point-max))
  (unless (string-suffix-p "\n" (hermes-chat-input-string))
    (insert "\n"))
  (insert content))

(defun hermes-chat--record-input-history (content)
  "Record non-empty CONTENT in the current buffer's sent-input history."
  (when-let* ((text (hermes-transport--non-empty-string content)))
    (setq hermes-chat--input-history
          (cons text (delete text hermes-chat--input-history))
          hermes-chat--input-history-index nil
          hermes-chat--input-history-draft "")))

(defun hermes-chat-input-history-previous ()
  "Replace the writable tail with the previous sent input."
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is outside the Hermes input area"))
  (unless hermes-chat--input-history
    (user-error "No Hermes input history"))
  (when (null hermes-chat--input-history-index)
    (setq hermes-chat--input-history-draft (hermes-chat-input-string)))
  (setq hermes-chat--input-history-index
        (min (1- (length hermes-chat--input-history))
             (1+ (or hermes-chat--input-history-index -1))))
  (hermes-chat--replace-input-tail
   (nth hermes-chat--input-history-index hermes-chat--input-history)))

(defun hermes-chat-input-history-next ()
  "Replace the writable tail with the next sent input or saved draft."
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is outside the Hermes input area"))
  (unless (numberp hermes-chat--input-history-index)
    (user-error "Already at newest Hermes input"))
  (setq hermes-chat--input-history-index
        (and (> hermes-chat--input-history-index 0)
             (1- hermes-chat--input-history-index)))
  (hermes-chat--replace-input-tail
   (if hermes-chat--input-history-index
       (nth hermes-chat--input-history-index hermes-chat--input-history)
     hermes-chat--input-history-draft)))

(defun hermes-chat--preserve-control-content (content)
  "Keep busy-control CONTENT recoverable after a dashboard bootstrap error."
  (when-let* ((text (hermes-transport--non-empty-string content)))
    (if (string-empty-p (string-trim (hermes-chat-input-string)))
        (hermes-chat--replace-input-tail text)
      (if (null hermes-chat--queued-messages)
          (hermes-chat--queue-content
           text "Preserved busy-control text after dashboard error")
        (hermes-chat--append-input-tail text)
        (hermes-chat--insert-local-status
         "Restored busy-control text in input tail after dashboard error"
         'error)))))

;;; Entry and header-state primitives

;; Shared by every chat sibling module; they live here, next to the
;; buffer-local state they touch, so no module needs a
;; declare-function to reach them.

(defvar hermes-chat-state-change-hook nil
  "Hook run in a Hermes chat buffer when dashboard-visible state changes.")

(defun hermes-chat-register-state-change-function (function)
  "Add FUNCTION to `hermes-chat-state-change-hook'."
  (add-hook 'hermes-chat-state-change-hook function))

(defvar hermes-chat-lifecycle-invalidation-hook nil
  "Hook run before a Hermes chat releases lifecycle-owned state.")

(defvar hermes-chat-submit-inhibit-functions nil
  "Functions returning a reason that this chat must reject submission.")

(defun hermes-chat-register-submit-inhibit-function (function)
  "Register FUNCTION as a submission ownership guard."
  (add-hook 'hermes-chat-submit-inhibit-functions function))

(defun hermes-chat--submit-inhibit-reason ()
  "Return the first reason this chat must reject submission, or nil."
  (run-hook-with-args-until-success 'hermes-chat-submit-inhibit-functions))

(defun hermes-chat--ensure-submit-allowed ()
  "Signal a user error when another operation exclusively owns this chat."
  (when-let* ((reason (hermes-chat--submit-inhibit-reason)))
    (user-error "%s" reason)))

(defvar-local hermes-chat--status-state nil
  "Plist describing the live status shown in the chat header.")

(defvar-local hermes-chat--active-tools nil
  "Hash table of active tool summaries shown in the chat header.")

(defvar-local hermes-chat--model nil
  "Model id reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--agent-name nil
  "Agent/profile name reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--context nil
  "Context-window usage plist (:used :max :percent) for the header.")

(defvar-local hermes-chat--goal nil
  "Compact structured goal state for the chat header.")

(defvar-local hermes-chat--runtime-flags nil
  "Runtime flag plist (:reasoning-effort :fast :yolo) from `session.info'.
Shown as annotations after the model in the chat header.")

(defvar hermes-chat--entry-counter 0
  "Counter used to generate local chat entry IDs.")

(defun hermes-chat--next-id (role)
  "Return a new local entry ID for ROLE."
  (format "%s-%d" role (cl-incf hermes-chat--entry-counter)))

(defun hermes-chat--entry-with (entry &rest props)
  "Return a copy of ENTRY with PROPS applied."
  (let ((copy (copy-sequence entry)))
    (while props
      (setq copy (plist-put copy (pop props) (pop props))))
    copy))

(defun hermes-chat--make-entry (role content &optional status id metadata)
  "Return a chat entry plist for ROLE and CONTENT.
STATUS defaults to `done'.  ID defaults to a generated local ID.
METADATA is stored as the entry's `:metadata' plist."
  (list :id (or id (hermes-chat--next-id role))
        :role role
        :status (or status 'done)
        :content (hermes-chat--sanitize-content content)
        :created (current-time)
        :metadata metadata))

(defun hermes-chat--notify-state-change ()
  "Run `hermes-chat-state-change-hook' in the current chat buffer."
  (run-hooks 'hermes-chat-state-change-hook))

(defun hermes-chat--set-header-state (&rest props)
  "Merge PROPS into `hermes-chat--status-state' and refresh the header."
  (setq hermes-chat--status-state
        (apply #'hermes-chat--entry-with
               hermes-chat--status-state
               (append props (list :updated (current-time)))))
  (force-mode-line-update)
  (hermes-chat--notify-state-change))

(defun hermes-chat--reset-header-state ()
  "Reset live header and session identity state for the current chat buffer."
  (setq hermes-chat--active-tools (make-hash-table :test 'equal)
        hermes-chat--status-state
        (list :status 'ready :activity "Ready" :updated (current-time))
        hermes-chat--model nil
        hermes-chat--agent-name nil
        hermes-chat--context nil
        hermes-chat--goal nil
        hermes-chat--runtime-flags nil)
  (force-mode-line-update)
  (hermes-chat--notify-state-change))

(defun hermes-chat--active-turn-p ()
  "Return non-nil when this chat buffer has an active Hermes turn."
  (or hermes-chat--pending-assistant-id
      hermes-chat--dashboard-running-p
      hermes-chat--server-queued-assistant-id
      hermes-chat--unsettled-submit-context
      hermes-chat--queued-submit-id
      (hermes-chat--submit-inhibit-reason)))

(defun hermes-chat--trailing-active-assistant-node ()
  "Return the trailing assistant node during an active turn, if any."
  (when (hermes-chat--active-turn-p)
    (when-let* ((node (and hermes-chat--ewoc
                           (ewoc-nth hermes-chat--ewoc -1))))
      (and (eq (plist-get (ewoc-data node) :role) 'assistant)
           node))))

(defun hermes-chat--insert-local-status (content &optional status)
  "Insert local status CONTENT with optional STATUS."
  (hermes-chat--insert-entry
   (hermes-chat--make-entry 'status content (or status 'done))
   (hermes-chat--trailing-active-assistant-node)))

(defun hermes-chat--command-error (message)
  "Render dashboard command error MESSAGE."
  (hermes-chat--insert-local-status message 'error)
  (hermes-chat--set-header-state :status 'error :activity message))


(defun hermes-chat--preview (content)
  "Return a compact preview for CONTENT."
  (truncate-string-to-width (string-replace "\n" " " content) 80 nil nil "…"))

(defvar hermes-chat--submit-function #'ignore
  "Function submitting CONTENT as a new user turn, set by `hermes-chat'.
Takes (CONTENT &optional DISPLAY QUEUE-ENTRY).  The queue/drain flow below
calls it so this file never references the submit pipeline defined above it.")

(defvar hermes-chat--queue-drain-ready-function (lambda () t)
  "Function returning non-nil when the current queue may drain.")

(defun hermes-chat--queue-or-submit-content (content &optional display)
  "Queue CONTENT during an active turn, otherwise submit it now.
DISPLAY is the compact user-turn text to show instead of CONTENT."
  (hermes-chat--ensure-submit-allowed)
  (if (or (hermes-chat--active-turn-p) hermes-chat--queued-messages)
      (progn
        (hermes-chat--queue-content content nil display)
        (hermes-chat--drain-queued-message))
    (funcall hermes-chat--submit-function content display)))

(defun hermes-chat--make-queue-entry (content display)
  "Return a queued message entry for CONTENT and DISPLAY."
  (list :id (hermes-chat--next-id 'queue)
        :content content
        :display display))

(defun hermes-chat--queue-head-id ()
  "Return the id of the first queued message, or nil."
  (plist-get (car hermes-chat--queued-messages) :id))

(defun hermes-chat--queue-submit-current-p (entry-id)
  "Return non-nil when ENTRY-ID owns the current queued submission."
  (and (equal entry-id hermes-chat--queued-submit-id)
       (equal entry-id (hermes-chat--queue-head-id))))

(defun hermes-chat--queue-submit-accepted (entry-id)
  "Remove accepted queue ENTRY-ID without disturbing a newer head."
  (when (hermes-chat--queue-submit-current-p entry-id)
    (setq hermes-chat--queued-messages (cdr hermes-chat--queued-messages)
          hermes-chat--queued-submit-id nil)
    (hermes-chat--queue-panel-refresh-if-live)
    (hermes-chat--drain-queued-message)))

(defun hermes-chat--turn-entry-ids (assistant-id)
  "Return transport entry ids owned by ASSISTANT-ID."
  (cl-loop for id being the hash-keys of hermes-chat--nodes
           for node = (gethash id hermes-chat--nodes)
           for entry = (ignore-errors (ewoc-data node))
           when (equal (hermes-chat--entry-assistant-id entry) assistant-id)
           collect id))

(defun hermes-chat--reset-submit-assistant (assistant-id)
  "Clear misattributed output from pending ASSISTANT-ID."
  (mapc #'hermes-chat--remove-entry
        (hermes-chat--turn-entry-ids assistant-id))
  (hermes-chat--mark-assistant assistant-id 'pending "" t))

(defun hermes-chat--rollback-queued-turn (user-id assistant-id)
  "Remove optimistic USER-ID and ASSISTANT-ID turn entries."
  (mapc #'hermes-chat--remove-entry
        (hermes-chat--turn-entry-ids assistant-id))
  (hermes-chat--remove-entry assistant-id)
  (hermes-chat--remove-entry user-id))

(defun hermes-chat--queue-submit-rejected
    (entry-id user-id assistant-id message)
  "Retain queue ENTRY-ID after MESSAGE, rolling back USER-ID and ASSISTANT-ID."
  (when (hermes-chat--queue-submit-current-p entry-id)
    (setq hermes-chat--queued-submit-id nil
          hermes-chat--dashboard-running-p nil)
    (when (equal hermes-chat--pending-assistant-id assistant-id)
      (setq hermes-chat--pending-assistant-id nil
            hermes-chat--process nil))
    (when (equal hermes-chat--dashboard-stream-assistant-id assistant-id)
      (setq hermes-chat--dashboard-stream-assistant-id nil))
    (when (equal hermes-chat--dashboard-detached-assistant-id assistant-id)
      (setq hermes-chat--dashboard-detached-assistant-id nil))
    (setq hermes-chat--dashboard-suppress-stream-p nil)
    (hermes-chat--rollback-queued-turn user-id assistant-id)
    (hermes-chat--insert-local-status
     (format "Queued message retained: %s" message) 'error)
    (hermes-chat--set-header-state
     :status 'error :activity "Queued message was not sent")))

(defun hermes-chat--drain-queued-message ()
  "Submit one queued message after the active turn settles."
  (when (and hermes-chat--queued-messages
             (not (hermes-chat--active-turn-p))
             (funcall hermes-chat--queue-drain-ready-function))
    (let ((entry (car hermes-chat--queued-messages)))
      (setq hermes-chat--queued-submit-id (plist-get entry :id))
      (condition-case err
          (funcall hermes-chat--submit-function
                   (plist-get entry :content)
                   (plist-get entry :display)
                   entry)
        (error
         (setq hermes-chat--queued-submit-id nil)
         (hermes-chat--command-error (error-message-string err)))))))

(defun hermes-chat--clear-submit-context (context)
  "Clear CONTEXT when it is still the unresolved dashboard submission."
  (when (eq context hermes-chat--unsettled-submit-context)
    (setq hermes-chat--unsettled-submit-context nil
          hermes-chat--prepared-submit-assistant-id nil)
    (hermes-chat--drain-queued-message)))

(defun hermes-chat--queue-content (content &optional note display)
  "Queue CONTENT for the next turn, inserting NOTE when non-nil.
DISPLAY is the compact user-turn text shown when the queued message is sent."
  (setq hermes-chat--queued-messages
        (append hermes-chat--queued-messages
                (list (hermes-chat--make-queue-entry content display))))
  (hermes-chat--queue-panel-refresh-if-live)
  (hermes-chat--insert-local-status
   (or note (format "Queued next message: %s"
                    (hermes-chat--preview (or display content))))
   'queued)
  (hermes-chat--set-header-state
   :status 'queued :activity "Queued next message"))

(defun hermes-chat--queue-panel-refresh-if-live ()
  "Refresh this chat's live queue side panel, when present."
  (when (buffer-live-p hermes-chat--queue-panel-buffer)
    (with-current-buffer hermes-chat--queue-panel-buffer
      (hermes-chat-queue-panel-refresh))))

(defun hermes-chat--queue-panel-owner ()
  "Return this panel's live chat owner or signal a user error."
  (unless (and (buffer-live-p hermes-chat-queue-panel--owner)
               (eq (buffer-local-value 'major-mode
                                       hermes-chat-queue-panel--owner)
                   'hermes-chat-mode)
               (buffer-local-value 'hermes-chat--input-marker
                                   hermes-chat-queue-panel--owner))
    (user-error "The owning Hermes chat is no longer live"))
  hermes-chat-queue-panel--owner)

(defun hermes-chat--queue-panel-entry-id ()
  "Return the queue entry id at point in a queue panel."
  (or (get-text-property (point) 'hermes-chat-queue-id)
      (get-text-property (line-beginning-position) 'hermes-chat-queue-id)
      (user-error "No queued message on this line")))

(defun hermes-chat-queue-panel-refresh (&rest _)
  "Render the owning chat's FIFO entries in the current side panel."
  (interactive)
  (let* ((owner (hermes-chat--queue-panel-owner))
         (entries (buffer-local-value 'hermes-chat--queued-messages owner))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Hermes queued messages" 'face 'bold) "\n\n")
    (if entries
        (cl-loop for entry in entries
                 for index from 1
                 do (let ((start (point)))
                      (insert (format "%d. %s\n" index
                                      (hermes-chat--preview
                                       (plist-get entry :content))))
                      (add-text-properties
                       start (point)
                       (list 'hermes-chat-queue-id (plist-get entry :id)))))
      (insert "No queued messages\n"))
    (goto-char (point-min))
    (forward-line 2)))

(defun hermes-chat--queue-edit-entry (owner id)
  "Edit queue entry ID owned by chat buffer OWNER."
  (with-current-buffer owner
    (when (equal id hermes-chat--queued-submit-id)
      (user-error "That queued message is currently being submitted"))
    (let* ((entry (seq-find (lambda (candidate)
                              (equal (plist-get candidate :id) id))
                            hermes-chat--queued-messages))
           (content (and entry
                         (string-trim
                          (read-string-from-buffer
                           "Queued message: " (plist-get entry :content))))))
      (when (equal id hermes-chat--queued-submit-id)
        (user-error "That queued message is currently being submitted"))
      (unless (seq-find (lambda (candidate)
                          (equal (plist-get candidate :id) id))
                        hermes-chat--queued-messages)
        (user-error "Queued message is no longer present"))
      (unless entry (user-error "Queued message is no longer present"))
      (when (string-empty-p content)
        (user-error "Queued message cannot be empty"))
      (setq hermes-chat--queued-messages
            (mapcar (lambda (candidate)
                      (if (equal (plist-get candidate :id) id)
                          (hermes-chat--entry-with
                           candidate :content content :display nil)
                        candidate))
                    hermes-chat--queued-messages))
      (hermes-chat--insert-local-status "Queued message updated" 'done))))

(defun hermes-chat--queue-remove-entry (owner id)
  "Remove queue entry ID owned by chat buffer OWNER."
  (with-current-buffer owner
    (when (equal id hermes-chat--queued-submit-id)
      (user-error "That queued message is currently being submitted"))
    (unless (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                      hermes-chat--queued-messages)
      (user-error "Queued message is no longer present"))
    (setq hermes-chat--queued-messages
          (seq-remove (lambda (entry) (equal (plist-get entry :id) id))
                      hermes-chat--queued-messages))
    (hermes-chat--insert-local-status "Queued message removed" 'done)))

(defun hermes-chat-queue-panel-edit ()
  "Edit the queued message at point."
  (interactive)
  (hermes-chat--queue-edit-entry
   (hermes-chat--queue-panel-owner) (hermes-chat--queue-panel-entry-id))
  (hermes-chat-queue-panel-refresh))

(defun hermes-chat--queue-panel-move-entry (owner id delta)
  "Move OWNER's queue entry ID by DELTA positions."
  (with-current-buffer owner
    (when (equal id hermes-chat--queued-submit-id)
      (user-error "That queued message is currently being submitted"))
    (let* ((index (cl-position id hermes-chat--queued-messages
                               :key (lambda (entry) (plist-get entry :id))
                               :test #'equal))
           (other (and index (+ index delta)))
           (neighbor (and other (nth other hermes-chat--queued-messages)))
           (neighbor-id (and neighbor (plist-get neighbor :id))))
      (unless (and other (>= other 0) (< other (length hermes-chat--queued-messages)))
        (user-error "Queued message cannot move farther"))
      (when (equal neighbor-id hermes-chat--queued-submit-id)
        (user-error "Cannot swap with a message that is currently being submitted"))
      (let ((entry (nth index hermes-chat--queued-messages)))
        (setq hermes-chat--queued-messages
              (cl-loop for candidate in hermes-chat--queued-messages
                       for position from 0
                       collect (cond ((= position index) neighbor)
                                     ((= position other) entry)
                                     (t candidate))))))))

(defun hermes-chat-queue-panel-move-up ()
  "Move the queued message at point one position earlier."
  (interactive)
  (hermes-chat--queue-panel-move-entry
   (hermes-chat--queue-panel-owner) (hermes-chat--queue-panel-entry-id) -1)
  (hermes-chat-queue-panel-refresh))

(defun hermes-chat-queue-panel-move-down ()
  "Move the queued message at point one position later."
  (interactive)
  (hermes-chat--queue-panel-move-entry
   (hermes-chat--queue-panel-owner) (hermes-chat--queue-panel-entry-id) 1)
  (hermes-chat-queue-panel-refresh))

(defun hermes-chat-queue-panel-remove ()
  "Remove the queued message at point."
  (interactive)
  (let ((owner (hermes-chat--queue-panel-owner))
        (id (hermes-chat--queue-panel-entry-id)))
    (hermes-chat--queue-remove-entry owner id))
  (hermes-chat-queue-panel-refresh))

(defvar-keymap hermes-chat-queue-panel-mode-map
  :doc "Keymap for the Hermes chat queue side panel."
  :parent special-mode-map
  "g" #'hermes-chat-queue-panel-refresh
  "e" #'hermes-chat-queue-panel-edit
  "u" #'hermes-chat-queue-panel-move-up
  "d" #'hermes-chat-queue-panel-move-down
  "D" #'hermes-chat-queue-panel-remove
  "q" #'quit-window)

(define-derived-mode hermes-chat-queue-panel-mode special-mode "Hermes Queue"
  "Major mode for editing one chat buffer's queued messages."
  :interactive nil)

(defun hermes-chat-queue-panel ()
  "Display an editable FIFO queue side panel for the current chat buffer."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (let* ((owner (current-buffer))
         (buffer (get-buffer-create
                  (format "*Hermes Queue: %s*" (buffer-name owner)))))
    (setq hermes-chat--queue-panel-buffer buffer)
    (with-current-buffer buffer
      (hermes-chat-queue-panel-mode)
      (setq hermes-chat-queue-panel--owner owner)
      (hermes-chat-queue-panel-refresh))
    (display-buffer buffer
                    '((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 0.28)))
    buffer))

;;; Active-tool registry and event activity

(defun hermes-chat--active-tools-table ()
  "Return the active-tools hash for this buffer, creating it when absent.
This feeds the dashboard's per-session tool list via
`hermes-chat--dashboard-snapshot'; the chat header itself never shows tools."
  (unless (hash-table-p hermes-chat--active-tools)
    (setq hermes-chat--active-tools (make-hash-table :test 'equal)))
  hermes-chat--active-tools)

(defun hermes-chat--clear-active-tools ()
  "Forget currently active tools in the chat header."
  (when (hash-table-p hermes-chat--active-tools)
    (clrhash hermes-chat--active-tools)))

(defun hermes-chat--active-tool-summaries ()
  "Return active tool summaries for the chat header."
  (let (summaries)
    (when (hash-table-p hermes-chat--active-tools)
      (maphash (lambda (_key summary) (push summary summaries))
               hermes-chat--active-tools))
    (nreverse summaries)))

(defun hermes-chat--header-activity-for-event (event)
  "Return a compact activity string for transport EVENT."
  (hermes-transport--non-empty-string
   (or (hermes-chat--transport-entry-content event)
       (hermes-chat--event-string event '(:content :text :preview :event)))))


;;; Header line

(defface hermes-chat-header-directory
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the working directory leading a Hermes chat header."
  :group 'hermes)

(define-obsolete-face-alias 'hermes-chat-header-profile
  'hermes-chat-header-directory "0.4.0")

(defface hermes-chat-header-model
  '((t :inherit font-lock-type-face))
  "Face for model names in a Hermes chat header."
  :group 'hermes)

(defface hermes-chat-header-reasoning
  '((t :inherit font-lock-keyword-face))
  "Face for reasoning effort in a Hermes chat header."
  :group 'hermes)

(defface hermes-chat-header-tier
  '((t :inherit font-lock-builtin-face))
  "Face for service-tier flags in a Hermes chat header."
  :group 'hermes)

(defface hermes-chat-header-warning
  '((t :inherit warning))
  "Face for risk-bearing runtime flags in a Hermes chat header."
  :group 'hermes)

(defface hermes-chat-header-context
  '((t :inherit font-lock-number-face))
  "Face for context token values in a Hermes chat header."
  :group 'hermes)

(defvar-local hermes-chat--profile nil
  "Profile name for this chat's dashboard session, or nil for the default.")

(defvar-local hermes-chat--working-directory nil
  "Gateway-native working directory for this chat session.
This path belongs to the Hermes instance and need not exist on Emacs's host.")

(defun hermes-chat--current-working-directory ()
  "Return this chat's gateway working directory or local launch directory."
  (or hermes-chat--working-directory default-directory))

(defun hermes-chat--directory-basename (&optional directory)
  "Return the final component of gateway-native DIRECTORY."
  (let* ((directory (or directory (hermes-chat--current-working-directory)))
         (trimmed (replace-regexp-in-string "[/\\\\]+\\'" "" directory)))
    (cond
     ((string-empty-p trimmed) directory)
     ((string-match "[^/\\\\]+\\'" trimmed) (match-string 0 trimmed))
     (t trimmed))))

(defun hermes-chat--header-directory-segment ()
  "Return the propertized working-directory basename for the header."
  (propertize (hermes-chat--directory-basename)
              'face 'hermes-chat-header-directory))

(defun hermes-chat--header-detail (label)
  "Return the live detail to append after LABEL in the header, or nil.
The activity is used, with a leading copy of LABEL stripped so a label-prefixed
activity is not shown twice.  Tool commands are deliberately not surfaced here:
the header keeps the kawaii thinking status as its only live detail, while the
transcript carries the full tool detail."
  (when-let* ((activity (hermes-transport--non-empty-string
                         (plist-get hermes-chat--status-state :activity))))
    (if (string-prefix-p (downcase label) (downcase activity))
        (hermes-transport--non-empty-string
         (string-trim (substring activity (length label)) "[-: ]+"))
      activity)))

(defun hermes-chat--header-status-segment ()
  "Return the propertized status segment: icon, label, and live detail.
The label carries the high-level state (Ready/Running/...) and an active tool
or distinct activity is appended so the live detail is not lost.  The `thinking'
state is shown bare (kawaii face plus verb, no icon or label) since the face is
self-explanatory."
  (let ((status (plist-get hermes-chat--status-state :status)))
    (if (eq status 'thinking)
        (propertize (or (hermes-transport--non-empty-string
                         (plist-get hermes-chat--status-state :activity))
                        "Thinking")
                    'face (hermes-chat--header-status-face 'running))
      (let* ((label (hermes-chat--header-status-label status))
             (detail (hermes-chat--header-detail label)))
        (propertize
         (format "%s %s" (hermes-chat--status-icon status)
                 (if detail (format "%s: %s" label detail) label))
         'face (hermes-chat--header-status-face status))))))

(defun hermes-chat--header-model-segment ()
  "Return the propertized header model segment, or nil."
  (and-let* ((model (hermes-transport--non-empty-string hermes-chat--model)))
    (propertize model 'face 'hermes-chat-header-model)))

(defun hermes-chat--header-goal-segment ()
  "Return the compact running-goal segment, or nil."
  (when (eq (plist-get hermes-chat--goal :running) t)
    (let ((turns (plist-get hermes-chat--goal :turns-used))
          (limit (plist-get hermes-chat--goal :max-turns)))
      (propertize
       (if (and (numberp turns) (numberp limit))
           (format "Goal %d/%d" turns limit)
         "Goal")
       'face (hermes-chat--header-status-face 'running)))))

(defun hermes-chat--header-runtime-segments ()
  "Return propertized runtime-flag segments for the chat header."
  (delq nil
        (list
         (when-let* ((effort (hermes-transport--non-empty-string
                              (plist-get hermes-chat--runtime-flags
                                         :reasoning-effort))))
           (propertize effort 'face 'hermes-chat-header-reasoning))
         (and (plist-get hermes-chat--runtime-flags :fast)
              (propertize "fast" 'face 'hermes-chat-header-tier))
         (and (plist-get hermes-chat--runtime-flags :yolo)
              (propertize "YOLO" 'face 'hermes-chat-header-warning)))))

(defun hermes-chat--header-context-segment ()
  "Return the propertized context-window segment, or nil."
  (when-let* ((context (hermes-chat--format-context hermes-chat--context)))
    (propertize context 'face 'hermes-chat-header-context)))

(defun hermes-chat--header-parts ()
  "Return ordered semantic segments for the Hermes chat header."
  (delq nil
        (append
         (list (hermes-chat--header-directory-segment)
               (hermes-chat--header-status-segment)
               (hermes-chat--header-goal-segment)
               (hermes-chat--header-model-segment))
         (hermes-chat--header-runtime-segments)
         (list (hermes-chat--header-context-segment)))))

(defun hermes-chat--header-line ()
  "Return the directory-first semantic header line for this chat."
  (let* ((separator (propertize "  |  " 'face 'shadow))
         (text (concat " " (string-join (hermes-chat--header-parts)
                                         separator)
                       " "))
         (width (max 1 (window-total-width))))
    (string-replace "%" "%%" (truncate-string-to-width text width nil nil "…"))))


(provide 'hermes-chat-buffer)
;;; hermes-chat-buffer.el ends here
