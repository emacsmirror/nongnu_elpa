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
;; One area of the single logical chat module assembled by `hermes-chat'; it
;; depends only downward (format/render) and reaches the submit pipeline in
;; `hermes-chat' through `hermes-chat--submit-function', never by name.

;;; Code:

(require 'hermes-buffer)
(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'project)
(require 'subr-x)
(require 'text-property-search)
(require 'hermes-chat-format)
(require 'hermes-chat-render)
(require 'hermes-preview-format)
(require 'hermes-dashboard-api)
(require 'hermes-transport)

(autoload 'hermes-preview-open "hermes-preview")

(defvar hermes-instance)
(defvar hermes-instances)
(defvar hermes-chat--profile)

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



(defvar-local hermes-chat--new-output nil
  "Non-nil when output arrived that no window showing this chat displays.")
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
(defcustom hermes-chat-stream-format-delay 0.1
  "Seconds between progressive Markdown passes on a streaming reply.
Use a regular timer so formatting also progresses while typing.  Completed
paragraphs and fenced blocks are formatted once.  Table rows append unless
their compact column allocation grows or the viewport changes; the unfinished
tail stays literal until more source arrives or the backend completes."
  :type 'number
  :group 'hermes)

(defvar-local hermes-chat--stream-formats nil
  "Entry IDs mapped to incremental presentation cursors and owned timers.
Raw source and backend lifecycle remain exclusively in EWOC entry data.")

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
(defvar-local hermes-chat-queue-panel--attachment nil
  "Owner lifetime, client and session captured when opening this panel.")
(defvar-local hermes-chat--session-id nil
  "Durable Hermes session key for the current chat buffer.")
(defvar-local hermes-chat--pinned-url nil
  "Captured backend URL for an explicitly backend-owned chat.
Nil preserves ordinary instance resolution.  Retain this pin across reconnect.")

(defvar-local hermes-chat--bot-chat-root nil
  "Backend-confirmed canonical Bot Chat root, or nil for ordinary chats.
This marks the interface, not a lookup cache.
Profiles always reads the backend registry.")
(defvar-local hermes-chat--dashboard-create-model nil
  "Buffer-local model override for the next `session.create'.
Nil means inherit the profile default.  Kept buffer-local so two chat buffers
sharing one dashboard socket each create their own session with their own
runtime.")
(defvar-local hermes-chat--dashboard-create-provider nil
  "Buffer-local provider override applied after the next `session.create'.")
(defvar-local hermes-chat--dashboard-create-cwd nil
  "Deliberate workspace pending authoritative post-create acceptance.
Retain the chosen path across create readback and failed override retries.")
(defvar-local hermes-chat--cwd-explicit-p nil
  "Non-nil when the user deliberately selected this chat's workspace.")
(defvar-local hermes-chat--dashboard-create-reasoning-effort nil
  "Buffer-local reasoning effort applied after the next `session.create'.")
(defvar-local hermes-chat--dashboard-create-fast-p nil
  "Buffer-local fast/service-tier flag applied after the next `session.create'.")
(defvar-local hermes-chat--create-override-owner nil
  "Identity owning application of pending create-time runtime overrides.")
(defvar-local hermes-chat--create-overrides-retry-session-id nil
  "Session id owning unapplied create-time runtime overrides, or nil.")
(defvar-local hermes-chat--session-bootstrap nil "Session setup or history owner.")
(defvar-local hermes-chat--restored-history nil
  "Exact accepted normal resume response, qualified by durable session identity.")
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

(defvar-local hermes-chat--application-context nil
  "Request-scoped observer retained beyond ordinary submit admission.")

(defun hermes-chat--application-current-p (context)
  "Return non-nil while CONTEXT owns this ordinary chat turn."
  (and (eq context hermes-chat--application-context)
       (hermes-buffer--owned-p 'hermes-chat-mode)
       (eq hermes-buffer--owner (plist-get context :application-claim))
       (hermes-chat--current-lifetime-p (plist-get context :lifetime))
       (eql hermes-chat--transport-generation (plist-get context :generation))
       (eq hermes-chat--dashboard-client (plist-get context :client))
       (equal hermes-chat--dashboard-active-session-id (plist-get context :session-id))))

(defun hermes-chat--application-call (context kind payload)
  "Schedule CONTEXT's observer with KIND and PAYLOAD after native settlement.
The observer runs outside renderer/receipt callbacks and may reenter safely.
Ownership is captured at notification, not inferred from the current buffer."
  (let ((observer (plist-get context :application-observer)))
    (run-at-time
     0 nil (lambda ()
             (condition-case nil
                 (funcall observer context kind payload)
               ((error quit) (message "Hermes application observer failed")))))))

(defun hermes-chat--application-retire ()
  "Reject the retained observer without interrupting the ordinary chat."
  (when-let* ((context hermes-chat--application-context))
    (setq hermes-chat--application-context nil)
    (hermes-chat--application-call context 'rejected "Chat owner retired; inspect history")))

(defun hermes-chat--application-notify (context kind payload)
  "Record owned CONTEXT's admission or terminal KIND with literal PAYLOAD.
Retain terminal-before-receipt evidence.  A queued receipt excludes the prior
backend turn unless the native queued handoff identified the new turn."
  (when (and context (hermes-chat--application-current-p context))
    (pcase kind
      ('rejected
       (setq hermes-chat--application-context nil)
       (hermes-chat--application-call context kind payload))
      ('admitted
       (setf (plist-get context :application-admitted) payload)
       (hermes-chat--application-call context kind payload))
      ('terminal
       (setq payload (copy-tree payload t))
       (when (stringp (plist-get payload :final-text))
         (setf (plist-get payload :final-text)
               (substring-no-properties (plist-get payload :final-text))))
       (setf (plist-get context :application-terminal) payload)))
    (when (and (hermes-chat--application-current-p context)
               (plist-get context :application-admitted)
               (plist-get context :application-terminal)
               (or (not (equal "queued" (hermes-transport--get
                                        (plist-get context :application-admitted) 'status)))
                   (plist-get context :application-boundary)))
      (setq hermes-chat--application-context nil)
      (hermes-chat--application-call
       context 'terminal (plist-get context :application-terminal)))))

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

(defun hermes-chat--insert-transient-toggle (entry summary expanded &optional face help)
  "Insert a toggle labeled SUMMARY for transient ENTRY in EXPANDED state.
FACE is the head-line face, nil for the default face; HELP extends the
collapsed `help-echo'."
  (let ((start (point)))
    (insert (if expanded "▾ " "▸ ") summary)
    (make-text-button start (point)
                      'face (or face 'default)
                      'mouse-face 'highlight
                      'follow-link t
                      'help-echo (cond (expanded "Collapse output")
                                       (help (concat "Expand output: " help))
                                       (t "Expand output"))
                      'hermes-chat-entry-id (plist-get entry :id)
                      'action #'hermes-chat--toggle-entry-button)))

(defconst hermes-chat--status-collapse-lines 3
  "Status and system notices longer than this many lines start collapsed.")

(defun hermes-chat--entry-unknown-event (entry)
  "Return ENTRY's unknown transport event, or nil."
  (let ((event (plist-get (plist-get entry :metadata) :event)))
    (and (eq (plist-get event :type) 'unknown) event)))

(defvar-local hermes-chat--unknown-event-detail-cache nil
  "Weak table from unknown transport events to their expanded detail text.")

(defun hermes-chat--unknown-event-body (event content)
  "Return the expanded body for unknown EVENT shown with CONTENT.
Formatting the raw payload is deferred until the disclosure is expanded and
cached per event, so repaints do not pretty-print it again."
  (unless hermes-chat--unknown-event-detail-cache
    (setq hermes-chat--unknown-event-detail-cache
          (make-hash-table :test #'eq :weakness 'key)))
  (or (gethash event hermes-chat--unknown-event-detail-cache)
      (puthash event
               (hermes-chat--sanitize-content
                (concat content "\n\n" (hermes-chat--unknown-event-detail event)))
               hermes-chat--unknown-event-detail-cache)))

(defun hermes-chat--transient-disclosure (entry content blocks)
  "Return (SUMMARY BODY HELP) when transient ENTRY should collapse, or nil.
CONTENT is ENTRY's text and BLOCKS its diff blocks.  BODY is a string or a
function returning one, called only when the disclosure is expanded.
Unknown events always collapse with their raw payload as BODY; tool output
collapses when multiline; status notices collapse beyond
`hermes-chat--status-collapse-lines'."
  (let ((role (plist-get entry :role))
        (lines (length (split-string (string-trim-right content) "\n"))))
    (cond
     ((when-let* ((event (hermes-chat--entry-unknown-event entry)))
        (list (hermes-chat--unknown-event-summary event)
              (lambda () (hermes-chat--unknown-event-body event content))
              content)))
     (blocks nil)
     ((and (memq role '(tool progress)) (> lines 1))
      (list (hermes-chat--first-line content) content nil))
     ((and (eq role 'status) (> lines hermes-chat--status-collapse-lines))
      (list (format "%s  (%d lines)" (hermes-chat--first-line content) lines)
            content nil)))))

(defun hermes-chat--insert-transient-content (entry)
  "Insert a compact transient transport ENTRY.
Running rows show `●' in `hermes-chat-tool-running'; failures carry `error'
on the whole head line.  Multiline tool output, unknown events and long
notices collapse to a one-line summary with a `▸'/`▾' toggle, like the
thinking disclosure; diffs become View Diff links.  Offline notices carry
owner-bound recovery buttons."
  (let ((content (or (plist-get entry :content) ""))
        (status (plist-get entry :status)))
    (unless (string-empty-p content)
      (pcase-let ((`(,marker . ,marker-face) (hermes-chat--entry-marker status))
                  (face (hermes-chat--entry-head-face status)))
        (insert (propertize (format "  %s " marker) 'face marker-face))
        (let* ((blocks (hermes-chat--diff-blocks content))
               (disclosure (hermes-chat--transient-disclosure entry content blocks)))
          (if disclosure
              (let ((expanded (hermes-chat--entry-expanded-p entry)))
                (hermes-chat--insert-transient-toggle
                 entry (nth 0 disclosure) expanded face (nth 2 disclosure))
                (insert "\n")
                (when expanded
                  (let ((body (nth 1 disclosure)))
                    (hermes-chat--insert-detail-body
                     entry (lambda ()
                             (hermes-chat--insert-shadow
                              (if (functionp body) (funcall body) body))
                             (insert "\n"))))))
            (hermes-chat--insert-diffed
             content
             (lambda (text)
               (unless (string-empty-p text)
                 (insert (if face (propertize text 'face face) text))))
             blocks)
            (when (plist-get (plist-get entry :metadata) :recovery-owner)
              (hermes-chat--insert-recovery-actions entry))
            (insert "\n")))))))

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
                      'help-echo (if expanded "Collapse reasoning" "Expand reasoning")
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
          (hermes-chat--insert-detail-body
           entry #'insert (propertize (concat content "\n") 'face 'shadow)))))))

(defun hermes-chat--insert-user-content (content)
  "Insert user CONTENT with a compact prompt prefix."
  (insert (propertize "> " 'face 'font-lock-keyword-face)
          (propertize (hermes-chat--code-marked-string content)
                      'face 'hermes-chat-user-input)
          "\n"))

(defun hermes-chat--insert-entry-content (content &optional streaming)
  "Insert assistant or system CONTENT.
While STREAMING, insert CONTENT literally; a throttled regular timer promotes
completed units without re-fontifying prior output.  Once the entry settles,
render CONTENT as
markdown with diff blocks replaced by View Diff links and embedded images
lifted into inline images."
  (if streaming
      (insert content "\n")
    (hermes-chat--insert-content-with-images content #'hermes-chat--insert-markdown)
    (insert "\n")))

(defvar hermes-chat-entry-printers nil
  "Alist of (ROLE . FUNCTION) printing feature-owned transcript entries.
FUNCTION receives the entry plist and inserts its text at point.")

(defvar hermes-chat--entry-suffix-functions nil
  "Functions called with each printed chat ENTRY to append contextual actions.
Each runs at the end of ENTRY's text and may insert whole lines there.")

(defun hermes-chat--print-entry (entry)
  "Insert a display representation of chat ENTRY at point."
  (let ((role (plist-get entry :role))
        (content (or (plist-get entry :content) "")))
    (cond
     ((alist-get role hermes-chat-entry-printers)
      (funcall (alist-get role hermes-chat-entry-printers) entry))
     ((eq role 'user)
      (hermes-chat--insert-user-content content))
     ((eq role 'activity)
      (insert (propertize "Working…\n" 'face 'shadow)))
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
       content (eq (plist-get entry :status) 'streaming))))
    (run-hook-with-args 'hermes-chat--entry-suffix-functions entry)
    (hermes-chat--insert-previews entry)))

(defun hermes-chat--preview-owner ()
  "Capture this chat attachment for an explicit output preview action."
  (let ((client hermes-chat--dashboard-client))
    (list (current-buffer) hermes-chat--lifecycle-generation
          (copy-sequence hermes-chat--dashboard-active-session-id) client
          (and client (hermes-dashboard-transport-client-generation client))
          (and client (hermes-dashboard-transport--api-client-base-url client)))))

(defun hermes-chat--preview-button (button)
  "Open the exact output descriptor retained on BUTTON."
  (hermes-preview-open (button-get button 'hermes-preview-descriptor)
                       (button-get button 'hermes-preview-owner)))

(defun hermes-chat--insert-previews (entry)
  "Insert compact output preview actions for ENTRY, without fetching anything."
  (dolist (descriptor (hermes-preview-entry entry))
    (insert "  ")
    (insert-text-button
     "[Preview]" 'face 'link 'follow-link t
     'help-echo (concat "Inert preview: " (plist-get descriptor :label))
     'hermes-preview-descriptor descriptor
     'hermes-preview-owner (hermes-chat--preview-owner)
     'action #'hermes-chat--preview-button)
    (insert " " (propertize
                 (truncate-string-to-width
                  (prin1-to-string (plist-get descriptor :label)) 70 nil nil t)
                 'face 'shadow) "\n")))

(defun hermes-chat-preview-output ()
  "Choose a generated output from this chat and open its inert preview."
  (interactive nil hermes-chat-mode)
  (let ((button (next-button (point-min))) candidates)
    (while button
      (when (button-get button 'hermes-preview-descriptor)
        (push (cons (format "%d: %s" (1+ (length candidates))
                            (plist-get (button-get button 'hermes-preview-descriptor)
                                       :label))
                    (list (button-get button 'hermes-preview-descriptor)
                          (button-get button 'hermes-preview-owner)))
              candidates))
      (setq button (next-button (button-end button))))
    (unless candidates (user-error "No generated outputs in this chat"))
    (let* ((choices (nreverse candidates))
           (choice (completing-read "Preview output: " choices nil t)))
      (apply #'hermes-preview-open (cdr (assoc choice choices))))))

(defun hermes-chat--input-position ()
  "Return the numeric input marker position."
  (and (markerp hermes-chat--input-marker)
       (marker-position hermes-chat--input-marker)))

(defun hermes-chat--point-in-input-p ()
  "Return non-nil if point is in the writable input tail."
  (let ((pos (hermes-chat--input-position)))
    (and pos (>= (point) pos))))

(defvar-local hermes-chat--protected-frontier nil
  "Cons (MARKER . POSITION): text before MARKER is known read-only.
POSITION is where MARKER stood when last validated.  A silent insertion or
deletion below the frontier moves MARKER away from POSITION, which resets
protection to `point-min'.  Changes seen by `before-change-functions' and
explicit `hermes-chat--protect-retreat' calls pull the frontier back first.")

(defun hermes-chat--protect-retreat (position)
  "Forget that text from POSITION onwards is protected.
Changes reported to `before-change-functions' retreat automatically.  Code
that edits the transcript with modification hooks inhibited must call this
before the edit, with POSITION at or before its start: retreating first
keeps the frontier marker at its validated position, whereas an unannounced
edit below the frontier moves it and forces a full rescan.

Silent edits that leave the frontier marker where it stood are not
detected at all: changing `read-only' or other text properties, or
replacing text with text of equal length, while modification hooks are
inhibited.  Such code must call this function first, or protection
skips the edited text."
  (let ((frontier hermes-chat--protected-frontier))
    (when (and (consp frontier) (markerp (car frontier))
               (eq (marker-buffer (car frontier)) (current-buffer))
               (= (marker-position (car frontier)) (cdr frontier)))
      (when (< position (cdr frontier))
        (set-marker (car frontier) position)
        (setcdr frontier (marker-position (car frontier)))))))

(defun hermes-chat--protect-before-change (beg _end)
  "Retreat the protected frontier to text about to change from BEG.
Before the change no marker has moved yet, so the frontier stays valid."
  (hermes-chat--protect-retreat beg))

(defun hermes-chat--protect-start ()
  "Return the first position that may lack transcript protection.
Install the frontier and its change hook on first use.  Whenever the
frontier is missing, foreign or moved since validation, return
`point-min' so protection never relies on an unproven prefix."
  (let ((frontier hermes-chat--protected-frontier))
    (unless (memq #'hermes-chat--protect-before-change before-change-functions)
      (add-hook 'before-change-functions #'hermes-chat--protect-before-change nil t)
      (setq frontier nil))
    (if (and (consp frontier) (markerp (car frontier))
             (eq (marker-buffer (car frontier)) (current-buffer))
             (= (marker-position (car frontier)) (cdr frontier)))
        (cdr frontier)
      (point-min))))

(defun hermes-chat--protect-reset ()
  "Forget all transcript protection knowledge in this buffer."
  (when (consp hermes-chat--protected-frontier)
    (set-marker (car hermes-chat--protected-frontier) nil))
  (setq hermes-chat--protected-frontier nil))

(defun hermes-chat--protect-transcript ()
  "Make transcript and prompt read-only while keeping input tail writable.
Scan only from the protected frontier: this runs after every streamed delta,
footer tick and entry insert, so scanning from `point-min' would make each
chunk cost O(transcript).  Do not record these internal text-property
changes in the undo list."
  (when-let* ((pos (hermes-chat--input-position)))
    (save-restriction
      (widen)
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (start (min (hermes-chat--protect-start) pos))
            (frontier hermes-chat--protected-frontier))
        (while (and (< start pos)
                    (setq start (text-property-not-all start pos 'read-only t)))
          (let ((end (or (text-property-any start pos 'read-only t) pos)))
            (add-text-properties start end
                                 '(read-only t front-sticky t rear-nonsticky t))
            (setq start end)))
        (remove-text-properties pos (point-max)
                                '(read-only nil front-sticky nil rear-nonsticky nil))
        ;; Validate after our own property changes.  A cloned buffer
        ;; shares the original's marker and cons; never adopt them.
        (if (and (consp frontier) (markerp (car frontier))
                 (eq (marker-buffer (car frontier)) (current-buffer)))
            (progn (set-marker (car frontier) pos)
                   (setcdr frontier pos))
          (setq hermes-chat--protected-frontier
                (cons (copy-marker pos) pos)))))))

(defun hermes-chat--adjust-undo-element (element deltas)
  "Adjust draft undo ELEMENT by DELTAS, including native change groups."
  (pcase element
    (`(apply ,delta ,beg ,end undo--wrap-and-run-primitive-undo
             ,inner-beg ,inner-end ,records)
     ;; `undo-adjust-elt' leaves apply records untouched.  This native
     ;; wrapper carries both region bounds and a nested ordinary undo log.
     `(apply ,delta ,(undo-adjust-pos beg deltas)
             ,(undo-adjust-pos end deltas)
             undo--wrap-and-run-primitive-undo
             ,(undo-adjust-pos inner-beg deltas)
             ,(undo-adjust-pos inner-end deltas)
             ,(mapcar (lambda (record)
                        (hermes-chat--adjust-undo-element record deltas))
                      records)))
    (_ (undo-adjust-elt element deltas))))

(defun hermes-chat--adjust-draft-undo (position)
  "Rebase draft undo records after the input moved from POSITION.
Preserve list cells: native pending undo and redo equivalences refer to
these same tails.  Markers already follow the transcript edits themselves."
  (let ((delta (- position (hermes-chat--input-position))))
    (unless (zerop delta)
      ;; Put the synthetic change before both old and new input starts:
      ;; shrinking must not clamp old draft positions at the old boundary.
      (let ((deltas (list (cons (1- (min position (hermes-chat--input-position)))
                                delta)))
            (seen (make-hash-table :test #'eq)))
        (dolist (tail (list buffer-undo-list pending-undo-list))
          (while (and (consp tail) (not (gethash tail seen)))
            (puthash tail t seen)
            (setcar tail (hermes-chat--adjust-undo-element (car tail) deltas))
            (setq tail (cdr tail))))))))

(defun hermes-chat--reader-anchor (position)
  "Return a logical transcript or draft anchor for POSITION."
  (save-excursion
    (goto-char position)
    (let ((input (hermes-chat--input-position)))
      (if (>= position input)
          (list 'input (- position input))
        (let* ((located (ewoc-locate hermes-chat--ewoc position))
               (node (and located (<= (ewoc-location located) position) located))
               (start (and node (ewoc-location node))))
          (list (and node (plist-get (ewoc-data node) :id))
                (if start (count-lines start (line-beginning-position)) 0)
                (current-column) position))))))

(defun hermes-chat--reader-position (anchor)
  "Resolve ANCHOR, bounding shortened or removed transcript content."
  (if (eq (car anchor) 'input)
      (min (point-max) (+ (hermes-chat--input-position) (cadr anchor)))
    (let* ((node (gethash (car anchor) hermes-chat--nodes))
           (next (and node (ewoc-next hermes-chat--ewoc node)))
           (end (if next (ewoc-location next) (hermes-chat--input-position))))
      (save-excursion
        (if (not node)
            (goto-char (min end (nth 3 anchor)))
          (save-restriction
            (narrow-to-region (ewoc-location node) (max (ewoc-location node) (1- end)))
            (goto-char (point-min))
            (forward-line (cadr anchor))
            (move-to-column (nth 2 anchor))))
        (point)))))

(defmacro hermes-chat--preserve-readers (&rest body)
  "Run replacement BODY preserving each window's logical reader anchors."
  (declare (indent 0) (debug t))
  `(let ((anchor (hermes-chat--reader-anchor (point)))
         (windows (mapcar
                   (lambda (window)
                     (list window
                           (hermes-chat--reader-anchor (window-start window))
                           (hermes-chat--reader-anchor (window-point window))))
                   (get-buffer-window-list (current-buffer) nil t))))
     (unwind-protect
         (progn ,@body)
       (goto-char (hermes-chat--reader-position anchor))
       (dolist (state windows)
         (when (and (window-live-p (car state))
                    (eq (window-buffer (car state)) (current-buffer)))
           (set-window-point (car state)
                             (hermes-chat--reader-position (nth 2 state)))
           ;; NOFORCE lets native redisplay follow an offscreen draft point.
           (set-window-start (car state)
                             (hermes-chat--reader-position (nth 1 state)) t))))))

(defmacro hermes-chat--preserve-input-point (&rest body)
  "Run transcript BODY preserving restriction, draft undo and input offset."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (let* ((marker hermes-chat--input-marker)
            (position (hermes-chat--input-position))
            (offset (and (hermes-chat--point-in-input-p)
                         (- (point) position))))
       (unwind-protect
           (progn ,@body)
         ;; Native change observers can retire the mode or its composer.
         (when (and (eq marker hermes-chat--input-marker)
                    (markerp marker) (marker-buffer marker))
           (hermes-chat--adjust-draft-undo position)
           (hermes-chat--protect-transcript)
           (when offset
             (goto-char (min (point-max)
                             (+ (hermes-chat--input-position) offset)))))))))

(defun hermes-chat--reflow-table-windows ()
  "Reflow this chat's tables after a window configuration change.
The buffer-local hook also runs when another window stops showing this
chat.  Hidden buffers wait until displayed; changing mode removes the hook."
  (when (and (derived-mode-p 'hermes-chat-mode)
             (not (hermes-buffer--retired-p))
             (hermes-chat--input-position)
             (get-buffer-window (current-buffer) t))
    (hermes-chat--preserve-input-point
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (hermes-chat--reflow-tables hermes-chat--input-marker
                                    (hermes-chat--table-window-width))
        ;; The native table owner can change presentation lengths on resize.
        (when (hash-table-p hermes-chat--stream-formats)
          (maphash
           (lambda (id state)
             (when-let* ((node (gethash id hermes-chat--nodes)))
               (let* ((next (ewoc-next hermes-chat--ewoc node))
                      (end (if next (ewoc-location next)
                             (- (hermes-chat--input-position)
                                (length (cdr (ewoc-get-hf hermes-chat--ewoc)))))))
                 (setf (plist-get state :display-length)
                       (- end (ewoc-location node) 1)))))
           hermes-chat--stream-formats))))))

(defun hermes-chat--separator ()
  "Return a full-width rule string separating the transcript from the input."
  (propertize " " 'display '(space :width text) 'face 'hermes-chat-separator))

(defun hermes-chat--setup-buffer ()
  "Initialize the current buffer as an empty Hermes chat buffer."
  ;; Selective undo owns a separate list.  Keep it local so an asynchronous
  ;; transcript update cannot rebase another buffer's pending undo sequence.
  (setq-local pending-undo-list nil)
  ;; Detail bodies carry symbolic `invisible' values; with the default spec
  ;; t they would all be hidden.  Keep any kinds the user already hid.
  (when (eq buffer-invisibility-spec t)
    (setq-local buffer-invisibility-spec (list t)))
  (hermes-chat--stream-format-retire)
  (hermes-chat--status-rule-retire)
  (add-hook 'after-set-visited-file-name-hook #'hermes-chat--stream-format-retire nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-chat--status-rule-retire nil t)
  (add-hook 'window-configuration-change-hook #'hermes-chat--reflow-table-windows nil t)
  (add-hook 'window-configuration-change-hook #'hermes-chat--status-rule-sync nil t)
  (add-hook 'hermes-chat-state-change-hook #'hermes-chat--status-rule-sync nil t)
  (add-hook 'window-scroll-functions #'hermes-chat--new-output-scrolled nil t)
  (setq hermes-chat--new-output nil)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (cl-incf hermes-chat--transport-generation)
    (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token)
          hermes-chat--cleanup-done-p nil)
    (hermes-chat--protect-reset)
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
          hermes-chat--session-bootstrap nil
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
          hermes-chat--unknown-event-detail-cache nil
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

(defvar-local hermes-chat--pending-steers nil
  "Oldest-first occurrences of direct steer text awaiting acknowledgement.")

(defvar-local hermes-chat--retained-clarify-owners nil
  "Oldest-first immutable owners of accepted clarification responses.")

(defvar-local hermes-chat--recovery-buffer nil
  "Editable document holding input preserved by explicit disconnect.")
(defvar-local hermes-chat--recovery-copies nil
  "Alist of occurrence identities and their last copied revisions.")
(defvar-local hermes-chat--disconnect-in-progress nil
  "Non-nil dynamically during explicit disconnect in this buffer.")

(defun hermes-chat--recovery-records ()
  "Return local input occurrences as (IDENTITY DISPOSITION CONTENT DISPLAY)."
  (let* ((uncertain "Delivery uncertain — do not resend automatically")
         (queue hermes-chat--queued-messages)
         (contexts (delq nil (list hermes-chat--unsettled-submit-context
                                   hermes-chat--busy-submit-context)))
         (draft (save-restriction
                  (widen)
                  (hermes-chat-input-string))))
    (append
     (mapcar (lambda (entry)
               (list entry
                     (cond ((equal (plist-get entry :id)
                                   hermes-chat--queued-submit-id) uncertain)
                           ((plist-get entry :rejected-p)
                            "Rejected — retained for manual recovery")
                           (t "Never sent"))
                     (plist-get entry :content) (plist-get entry :display)))
             queue)
     (cl-loop for context in (cl-delete-duplicates contexts :test #'eq)
              unless (cl-find (plist-get context :queue-id) queue
                              :key (lambda (entry) (plist-get entry :id))
                              :test #'equal)
              collect (list context uncertain (plist-get context :content)
                            (plist-get context :display)))
     (mapcar (lambda (owner)
               (list owner uncertain (plist-get owner :text) nil))
             (append hermes-chat--retained-clarify-owners
                     hermes-chat--pending-steers))
     (unless (string-empty-p draft)
       (list (list 'draft "Draft — also remains in original chat" draft nil))))))

(defun hermes-chat--recovery-text (records copies)
  "Return literal recovery text for RECORDS, marking revisions in COPIES."
  (mapconcat
   (lambda (record)
     (pcase-let ((`(,identity ,status ,content ,display) record))
       (concat "\n\n" status
               (when (assq identity copies) " (later revision)")
               "\nContent:\n" content
               (when display (concat "\nDisplay:\n" display)))))
   records ""))

(defun hermes-chat--recovery-document (text session profile instance)
  "Return recovery TEXT with instructions for SESSION, PROFILE and INSTANCE."
  (concat
   (format "Hermes recovery\nSession: %s\nProfile: %s\nInstance: %s\n"
           session profile instance)
   "Open Sessions in this instance/profile and resume this session,\n"
   "or use M-x hermes-chat-resume-session in that instance.\n"
   "Inspect history before copying selected text and explicitly sending.\n"
   "Never automatically resend Delivery uncertain text.\n"
   "This editable buffer is in-memory only; save it if needed."
   text))

(defun hermes-chat--capture-recovery ()
  "Append changed local input to an editable recovery document.
Stage all text before writing; errors and quits leave input ownership intact."
  (let* ((live (buffer-live-p hermes-chat--recovery-buffer))
         (copies (and live hermes-chat--recovery-copies))
         (records (cl-remove-if
                   (lambda (record)
                     (equal (cdr record) (cdr (assq (car record) copies))))
                   (hermes-chat--recovery-records)))
         (text (hermes-chat--recovery-text records copies))
         (revisions (mapcar (lambda (record)
                              (cons (car record)
                                    (mapcar (lambda (value)
                                              (if (stringp value)
                                                  (copy-sequence value) value))
                                            (cdr record))))
                            records)))
    (when records
      (unless live
        (setq text (hermes-chat--recovery-document
                    text hermes-chat--session-id hermes-chat--profile
                    (hermes-instance-name hermes-instance))))
      (let ((buffer (if live hermes-chat--recovery-buffer
                      (generate-new-buffer "*Hermes recovery*")))
            committed)
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (let ((inhibit-modification-hooks t))
                    (atomic-change-group
                      (goto-char (point-max))
                      (insert text)))))
              (setq hermes-chat--recovery-buffer buffer
                    hermes-chat--recovery-copies
                    (append revisions
                            (cl-remove-if (lambda (copy)
                                            (assq (car copy) revisions)) copies))
                    committed t))
          (unless (or committed live) (kill-buffer buffer)))))))

(defun hermes-chat--invalidate-transport-state (&optional recover-input)
  "Invalidate pending work before releasing this buffer's client.
With RECOVER-INPUT, preserve hook-added input before clearing its owners."
  (cl-incf hermes-chat--transport-generation)
  (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token))
  (hermes-chat--stream-format-retire)
  (hermes-chat--status-rule-retire)
  ;; Teardown must not publish through display hooks before resource release.
  (hermes-chat--reasoning-row hermes-chat--pending-assistant-id nil t)
  (hermes-chat--application-retire)
  (run-hooks 'hermes-chat-lifecycle-invalidation-hook)
  (when (hash-table-p hermes-chat--auto-prompt-keys)
    (clrhash hermes-chat--auto-prompt-keys))
  (when recover-input (hermes-chat--capture-recovery))
  (setq hermes-chat--pending-assistant-id nil
        hermes-chat--pending-steers nil
        hermes-chat--queued-messages nil
        hermes-chat--queued-submit-id nil
        hermes-chat--process nil
        hermes-chat--dashboard-running-p nil
        hermes-chat--create-override-owner nil
        hermes-chat--create-overrides-retry-session-id nil
        hermes-chat--session-bootstrap nil
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
        hermes-chat--interrupt-request-pending-p nil)
  ;; Invalidation hooks still need the queue for recovery; render only after
  ;; those hooks have run and the local FIFO has actually been cleared.
  (condition-case err
      (hermes-chat--queue-panel-refresh-if-live)
    ((error quit)
     (message "Hermes queue panel refresh failed (%s)" (car err)))))

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

(defun hermes-chat--entry-insert-position (before-node)
  "Return where a new entry lands: BEFORE-NODE's start, else the footer's."
  (if before-node
      (ewoc-location before-node)
    (- (or (hermes-chat--input-position) (point-max))
       (length (cdr (ewoc-get-hf hermes-chat--ewoc))))))
;;; Unseen output indicator

(defun hermes-chat--transcript-end ()
  "Return the position just after the transcript, before the composer rule."
  (- (hermes-chat--input-position)
     (length (cdr (ewoc-get-hf hermes-chat--ewoc)))))

(defun hermes-chat--window-shows-latest-p (window)
  "Return non-nil when WINDOW displays the latest output.
That holds when its point is in the composer, which redisplay keeps in view,
or when the transcript end is visible."
  (or (>= (window-point window) (hermes-chat--input-position))
      (pos-visible-in-window-p (hermes-chat--transcript-end) window)))

(defun hermes-chat--new-output-set (value)
  "Set the unseen-output flag to VALUE, refreshing mode lines on change."
  (unless (eq (not value) (not hermes-chat--new-output))
    (setq hermes-chat--new-output value)
    (force-mode-line-update)))

(defun hermes-chat--new-output-note ()
  "Flag new transcript output that no window showing this chat displays.
Cheap once flagged: windows are inspected only while the flag is clear."
  (when (and (not hermes-chat--new-output)
             (hermes-chat--status-rule-live-p)
             (not (seq-some #'hermes-chat--window-shows-latest-p
                            (get-buffer-window-list nil nil t))))
    (hermes-chat--new-output-set t)))

(defun hermes-chat--new-output-scrolled (window _start)
  "Clear the unseen-output flag once WINDOW displays the transcript end.
Runs from the buffer-local `window-scroll-functions'."
  (when (and hermes-chat--new-output
             (hermes-chat--status-rule-live-p)
             (hermes-chat--window-shows-latest-p window))
    (hermes-chat--new-output-set nil)))

(defun hermes-chat--jump-to-latest ()
  "Move to the composer end, below the latest output, and clear the flag."
  (widen)
  (goto-char (point-max))
  (hermes-chat--new-output-set nil))

(defun hermes-chat-jump-to-latest-at-mouse (event)
  "Show the latest output of the chat whose mode line received EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (select-window window))
    (unless (derived-mode-p 'hermes-chat-mode)
      (user-error "No Hermes chat under the mouse"))
    (hermes-chat--jump-to-latest)))

(defvar hermes-chat-new-output-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'hermes-chat-jump-to-latest-at-mouse)
    map)
  "Keymap active on the mode line's unseen-output indicator.")

(defun hermes-chat--new-output-mode-line ()
  "Return the compact unseen-output mode-line segment, or nil."
  (when hermes-chat--new-output
    (propertize " ↓ new" 'face 'mode-line-emphasis
                'mouse-face 'mode-line-highlight
                'local-map hermes-chat-new-output-map
                'help-echo (substitute-command-keys
                            "mouse-1 or \\[hermes-chat-go-to-composer]: \
jump to the latest output"))))

(defun hermes-chat--insert-entry (entry &optional before-node)
  "Insert ENTRY into the current chat EWOC and return its node.
With BEFORE-NODE, insert ENTRY before that node instead of at the end, so the
agent's reply can stay last while tool/status/diff entries land above it."
  (let ((node
         ;; Include transcript protection, but not state-change notification.
         (let ((inhibit-modification-hooks
                (or inhibit-modification-hooks
                    (eq (plist-get entry :role) 'activity))))
           (hermes-chat--preserve-input-point
            (let ((node (let ((inhibit-read-only t)
                              (buffer-undo-list t))
                          ;; Activity insertion inhibits change hooks.
                          (hermes-chat--protect-retreat
                           (hermes-chat--entry-insert-position before-node))
                          (if before-node
                              (ewoc-enter-before hermes-chat--ewoc
                                                 before-node entry)
                            (ewoc-enter-last hermes-chat--ewoc entry)))))
              (hermes-chat--register-node entry node))))))
    (hermes-chat--stream-format-schedule node)
    (hermes-chat--retire-recovery-notices)
    (hermes-chat--new-output-note)
    (hermes-chat--notify-state-change)
    node))

(defun hermes-chat--entries ()
  "Return chat entries from the current buffer in display order."
  (ewoc-collect hermes-chat--ewoc #'identity))

(defun hermes-chat--stream-format-retire (&optional id)
  "Cancel progressive presentation for ID, or every entry when ID is nil."
  (when (hash-table-p hermes-chat--stream-formats)
    (if id
        (progn
          (when-let* ((timer (plist-get (gethash id hermes-chat--stream-formats) :timer)))
            (cancel-timer timer))
          (remhash id hermes-chat--stream-formats))
      (maphash (lambda (key _) (hermes-chat--stream-format-retire key))
               hermes-chat--stream-formats))))

(defun hermes-chat--stream-format-schedule (node)
  "Schedule one regular formatting pass for streaming assistant NODE."
  (let* ((entry (ewoc-data node))
         (id (plist-get entry :id)))
    (when (and (eq (plist-get entry :role) 'assistant)
               (eq (plist-get entry :status) 'streaming)
               (not (hermes-buffer--retired-p)))
      (unless hermes-chat--stream-formats
        (setq hermes-chat--stream-formats (make-hash-table :test #'equal)))
      (let ((state (or (gethash id hermes-chat--stream-formats)
                       (puthash id (list :end 0 :scan 0 :searched 0 :fence nil
                                        :header nil :table nil :updates nil :rewrite nil :timer nil
                                        :display-length (length (plist-get entry :content)))
                                hermes-chat--stream-formats))))
        (unless (plist-get state :timer)
          (setf (plist-get state :timer)
                (run-at-time (max 0.01 hermes-chat-stream-format-delay) nil
                             #'hermes-chat--stream-format-deliver
                             (current-buffer) hermes-chat--lifecycle-generation
                             node state)))))))

(defun hermes-chat--stream-format-table (text line end state width)
  "Promote table LINE through END in TEXT using STATE and viewport WIDTH.
Append stable single-grid rows; repaint only the active table for width
changes or column panels, reusing cached fontified rows for the latter."
  (let* ((table (plist-get state :table))
         (old (or (cdr (assq table (plist-get state :updates))) table))
         (cells (hermes-chat--table-cells (hermes-chat--fontify-markdown-string line)))
         (rows (cons cells (plist-get old :rows)))
         (natural (cl-loop for i below (max (length cells) (length (plist-get old :cells)))
                           for a = (or (nth i cells) "")
                           for b = (or (nth i (plist-get old :cells)) "")
                           collect (if (> (string-width a) (string-width b)) a b)))
         (widths (hermes-chat--stream-table-widths natural width))
         (source (substring text (plist-get old :start) end))
         (changed (not (equal widths (plist-get old :widths))))
         (panels (> (+ 1 (* 3 (length widths)) (apply #'+ widths)) width))
         (next (list :start (plist-get old :start) :source source :cells natural
                     :rows rows :widths widths :width width))
         (grid (cond
                (panels (hermes-chat--stream-table-row cells widths width (reverse rows)))
                (changed (hermes-chat--format-table source width widths))
                (t (hermes-chat--stream-table-row cells widths width)))))
    (setf (alist-get table (plist-get state :updates) nil nil #'eq) next
          (plist-get state :end) end)
    (when (or changed panels) (setf (plist-get state :rewrite) table))
    (add-text-properties 0 (length grid)
                         (list 'hermes-chat-inline-table table
                               'hermes-chat-table-width width 'rear-nonsticky t) grid)
    grid))

(defun hermes-chat--stream-format-line (text start end state width)
  "Consume a complete TEXT line at START through END in STATE using WIDTH.
Return newly completed presentation, updating only the copied cursor."
  (let* ((line (substring text start (1- end)))
         (begin (plist-get state :end))
         (fence (plist-get state :fence))
         (header (plist-get state :header))
         (table (plist-get state :table))
         (opening (and (not fence) (hermes-chat--stream-fence line))))
    (cond
     (fence
      (when (string-match-p
             (concat "\\`[[:blank:]]*" (regexp-quote fence) "+[[:blank:]]*\\'") line)
        (setf (plist-get state :fence) nil (plist-get state :end) end)
        (hermes-chat--fontify-markdown-string (substring text begin end))))
     (opening
      (setf (plist-get state :fence) opening
            (plist-get state :header) nil (plist-get state :table) nil
            (plist-get state :end) start)
      (when (> start begin)
        (hermes-chat--fontify-markdown-string (substring text begin start))))
     ((and header (markdown--is-delimiter-row line))
      (setf (plist-get state :table)
            (list :start begin :source nil :cells nil :rows nil :widths nil :width nil)
            (plist-get state :header) nil)
      (hermes-chat--stream-format-table text header end state width))
     ((and table (string-match-p markdown-table-line-regexp line))
      (hermes-chat--stream-format-table text line end state width))
     ((string-match-p "\\`[ \t]*\\'" line)
      (setf (plist-get state :end) end (plist-get state :header) nil
            (plist-get state :table) nil)
      (hermes-chat--fontify-markdown-string (substring text begin end)))
     (t
      (setf (plist-get state :table) nil (plist-get state :header) nil)
      (when (and (string-match-p markdown-table-line-regexp line)
                 (hermes-chat--table-cells line))
        (setf (plist-get state :header) line (plist-get state :end) start)
        (when (> start begin)
          (hermes-chat--fontify-markdown-string (substring text begin start))))))))

(defun hermes-chat--stream-format-scan (text state width)
  "Scan only newly arrived TEXT lines in STATE at WIDTH.
The search watermark also advances across incomplete lines, avoiding repeated
scans of a long literal fragment.  Return just the newly formatted units."
  (let ((search (plist-get state :searched))
        (old-end (plist-get state :end)) rewrite parts)
    (setf (plist-get state :updates) nil (plist-get state :rewrite) nil)
    (while (string-match "\n" text search)
      (let ((end (match-end 0)))
        (when-let* ((part (hermes-chat--stream-format-line
                          text (plist-get state :scan) end state width)))
          (when-let* ((table (plist-get state :rewrite)))
            ;; Active-table replacement supersedes only its earlier pieces.
            (setq parts (seq-remove
                         (lambda (piece)
                           (eq table (get-text-property 0 'hermes-chat-inline-table piece)))
                         parts))
            (when (< (plist-get table :start) old-end) (setq rewrite table))
            (setf (plist-get state :rewrite) nil))
          (push part parts))
        (setq search end)
        (setf (plist-get state :scan) end)))
    (setf (plist-get state :searched) (length text)
          (plist-get state :rewrite) rewrite)
    (apply #'concat (nreverse parts))))

(defun hermes-chat--stream-format-deliver (buffer lifetime node state)
  "Promote completed units for NODE's exact STATE in BUFFER LIFETIME."
  (hermes-chat--in-lifetime buffer lifetime
    (let* ((entry (ewoc-data node))
           (id (plist-get entry :id)))
      (when (and (derived-mode-p 'hermes-chat-mode)
                 (not (hermes-buffer--retired-p))
                 (eq (gethash id hermes-chat--nodes) node)
                 (eq (gethash id hermes-chat--stream-formats) state)
                 (eq (plist-get entry :status) 'streaming))
        (setf (plist-get state :timer) nil)
        (let* ((text (plist-get entry :content))
               (cursor (copy-sequence state))
               (formatted (hermes-chat--stream-format-scan
                           text cursor (hermes-chat--table-window-width)))
               (old-end (plist-get state :end))
               (new-end (plist-get cursor :end)))
          (if (= old-end new-end)
              (puthash id cursor hermes-chat--stream-formats)
            (hermes-chat--stream-format-replace
             node entry state cursor formatted old-end new-end)))))))

(defun hermes-chat--stream-format-replace (node entry state cursor text start end)
  "Replace NODE ENTRY's literal START..END with TEXT, publishing CURSOR.
STATE and the native change tick fence before-change observers too.
Native diff markers retain unchanged reader prefixes and literal suffixes."
  (hermes-chat--preserve-input-point
    (let* ((inhibit-read-only t) (buffer-undo-list t)
           (id (plist-get entry :id))
           (tick (buffer-chars-modified-tick))
           (boundary (+ (ewoc-location node)
                        (- (plist-get state :display-length)
                           (- (length (plist-get entry :content)) start))))
           (table (plist-get cursor :rewrite))
           (position (if table
                         (text-property-any (ewoc-location node) boundary
                                            'hermes-chat-inline-table table)
                       boundary))
           (limit (+ boundary (- end start))))
      (catch 'superseded
        (combine-change-calls position limit
          (unless (and (hash-table-p hermes-chat--nodes)
                       (eq (gethash id hermes-chat--nodes) node)
                       (eq (ewoc-data node) entry)
                       (eq (gethash id hermes-chat--stream-formats) state)
                       (= tick (buffer-chars-modified-tick))
                       (not (hermes-buffer--retired-p)))
            (throw 'superseded nil))
          (let ((inhibit-modification-hooks t))
            (hermes-chat--protect-retreat position)
            (save-excursion
              (replace-region-contents position limit (lambda () text))
              ;; Native diff replacement does not refresh matched properties.
              (let ((offset 0))
                (while (< offset (length text))
                  (let ((next (next-property-change offset text (length text))))
                    (set-text-properties (+ position offset) (+ position next)
                                         (text-properties-at offset text))
                    (setq offset next)))))
            (dolist (update (plist-get cursor :updates))
              (dolist (key '(:source :cells :rows :widths :width))
                (setf (plist-get (car update) key) (plist-get (cdr update) key))))
            (setf (plist-get cursor :display-length)
                  (+ (plist-get state :display-length) (length text) (- position limit))
                  (plist-get cursor :updates) nil (plist-get cursor :rewrite) nil)
            (puthash id cursor hermes-chat--stream-formats)))))))

(defun hermes-chat--insert-stream-suffix (node entry prefix)
  "Insert ENTRY's suffix after PREFIX only while NODE still owns its extent."
  (let ((buffer (current-buffer))
        (ewoc hermes-chat--ewoc)
        (tick (buffer-chars-modified-tick))
        (position (+ (ewoc-location node)
                     (or (plist-get (and hermes-chat--stream-formats
                                         (gethash (plist-get entry :id)
                                                  hermes-chat--stream-formats)) :display-length)
                         (length prefix)))))
    (catch 'hermes-chat--superseded-append
      (save-excursion
        (combine-change-calls position position
          ;; Before-change observers may settle or replace this entry.  Skip
          ;; both the obsolete edit and its after-change notification then.
          (unless (and (eq (current-buffer) buffer)
                       (eq hermes-chat--ewoc ewoc)
                       (eq (gethash (plist-get entry :id) hermes-chat--nodes) node)
                       (eq (ewoc-data node) entry)
                       (= tick (buffer-chars-modified-tick)))
            (throw 'hermes-chat--superseded-append nil))
          (let ((inhibit-modification-hooks t))
            (hermes-chat--protect-retreat position)
            (goto-char position)
            (insert (substring (plist-get entry :content) (length prefix)))
            (when-let* ((state (and hermes-chat--stream-formats
                                   (gethash (plist-get entry :id) hermes-chat--stream-formats))))
              (cl-incf (plist-get state :display-length)
                       (- (length (plist-get entry :content)) (length prefix))))))))))

(defun hermes-chat--append-stream-entry (node entry)
  "Append only the new literal suffix when NODE can display ENTRY in place.
Return non-nil on success.  Empty placeholders, changed prefixes and all
settled content use the full printer.  Native markers preserve readers on
this non-destructive path; logical reader anchors belong to replacement.
This bounds buffer insertion, not accumulated string copying or scanning."
  (let* ((old (ewoc-data node))
         (prefix (plist-get old :content))
         (text (plist-get entry :content))
         (state (and hermes-chat--stream-formats
                     (gethash (plist-get old :id) hermes-chat--stream-formats)))
         (next (ewoc-next hermes-chat--ewoc node))
         (end (if next (ewoc-location next)
                (- (hermes-chat--input-position)
                   (length (cdr (ewoc-get-hf hermes-chat--ewoc)))))))
    (when (and (eq (plist-get old :role) 'assistant)
               (eq (plist-get old :status) 'streaming)
               (stringp prefix) (not (string-empty-p prefix))
               (= end (+ (ewoc-location node)
                         (or (plist-get state :display-length) (length prefix)) 1))
               (eq (char-before end) ?\n)
               (stringp text) (string-prefix-p prefix text)
               (equal entry (hermes-chat--entry-with old :content text)))
      ;; Accept the cumulative model even when a change observer signals.
      ;; Neither hook phase may publish this old entry over a successor.
      (ewoc-set-data node entry)
      (hermes-chat--insert-stream-suffix node entry prefix)
      t)))

(defun hermes-chat--update-entry (id function &optional quiet)
  "Update entry ID by applying FUNCTION to its entry plist.
With QUIET, leave state-change notification to the caller.
Return the updated entry, or nil when ID names no live entry -- callers run
from WebSocket callbacks and timers, where the entry may already be gone
because the chat was cleared mid-turn, like `hermes-chat--remove-entry'."
  (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (let ((entry (hermes-chat--preserve-input-point
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t)
                        (entry (funcall function (ewoc-data node))))
                    (unless (hermes-chat--append-stream-entry node entry)
                      (hermes-chat--stream-format-retire id)
                      (hermes-chat--preserve-readers
                        (ewoc-set-data node entry)
                        (ewoc-invalidate hermes-chat--ewoc node)))
                    entry))))
      (hermes-chat--stream-format-schedule node)
      (hermes-chat--new-output-note)
      (unless quiet (hermes-chat--notify-state-change))
      entry)))

(defun hermes-chat--remove-entry (id &optional quiet)
  "Remove chat entry ID from the EWOC and node table.
With QUIET, leave state-change notification to the caller."
  (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (hermes-chat--stream-format-retire id)
    ;; Include transcript protection in the activity mutation, not publication.
    (let ((inhibit-modification-hooks
           (or inhibit-modification-hooks
               (eq (plist-get (ewoc-data node) :role) 'activity))))
      (hermes-chat--preserve-input-point
       (let ((inhibit-read-only t)
             (buffer-undo-list t))
         (hermes-chat--preserve-readers
           (remhash id hermes-chat--nodes)
           (hermes-chat--protect-retreat (ewoc-location node))
           (ewoc-delete hermes-chat--ewoc node)))))
    (unless quiet (hermes-chat--notify-state-change))))

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
  "Toggle the collapsible entry attached to BUTTON.
Expanding an entry whose kind is globally hidden shows that kind again."
  (when-let* ((id (button-get button 'hermes-chat-entry-id))
              (entry (hermes-chat--toggle-entry-expanded id)))
    (when (hermes-chat--entry-expanded-p entry)
      (let ((kind (hermes-chat--entry-detail-kind entry)))
        (when (hermes-chat--details-hidden-p kind)
          (hermes-chat--set-details-hidden kind nil))))))

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
         (hermes-chat--entry-with entry :status status :content text))))))

(defun hermes-chat--mark-assistant (assistant-id status &optional content final quiet)
  "Set ASSISTANT-ID to STATUS, optionally replacing CONTENT.
When FINAL is non-nil, re-sanitize the stored content as settled text.
With QUIET, leave state-change notification to the caller."
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
         (hermes-chat--entry-with entry :status status))))
   quiet))

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
  "Return non-nil when CONTENT is a transient echo of ASSISTANT-ID thinking.
Only a still-blank reply can receive an echo, so test that first: once the
reply has text, no per-delta work may scale with the accumulated reply."
  (and (string-blank-p (or (hermes-chat--entry-content-by-id assistant-id) ""))
       (let ((content (hermes-chat--normalize-for-dedup
                       (hermes-chat--sanitize-assistant-content content nil))))
         (and (not (string-empty-p content))
              (string= content
                       (hermes-chat--normalize-for-dedup
                        (hermes-chat--thinking-entry-content assistant-id)))))))

(defun hermes-chat--assistant-segment-content (assistant-id content)
  "Return ASSISTANT-ID's portion of final or interim CONTENT.
A local redirect can split a cumulative backend message.  Remove its exact
saved prefix only when the live suffix corroborates it.  Without that evidence,
keep ambiguous final-only corrections verbatim rather than risk losing text."
  (when content
    (let* ((node (and hermes-chat--nodes (gethash assistant-id hermes-chat--nodes)))
           (entry (and node (ewoc-data node)))
           (prefix (plist-get entry :stream-prefix))
           (stream (plist-get entry :content))
           (clean (hermes-chat--sanitize-assistant-content content t)))
      (if (and (stringp prefix) (not (string-empty-p prefix))
               (string-prefix-p prefix clean)
               stream (not (string-empty-p stream))
               (string-prefix-p stream (substring clean (length prefix)))
               (not (string-prefix-p stream clean)))
          (substring clean (length prefix))
        content))))

(defun hermes-chat--assistant-done-content (assistant-id content)
  "Return ASSISTANT-ID final CONTENT, suppressing thinking-only echo."
  (unless (hermes-chat--thinking-only-final-content-p assistant-id content)
    (hermes-chat--assistant-segment-content assistant-id content)))

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

(defun hermes-chat--reasoning-row (assistant-id active &optional quiet)
  "Show or remove ASSISTANT-ID's static row according to ACTIVE.
With QUIET, remove the row without publishing during teardown."
  (when assistant-id
    (let ((id (concat assistant-id ":activity")))
      (if (not active)
          (hermes-chat--remove-entry id quiet)
        (when (and (equal assistant-id hermes-chat--pending-assistant-id)
                   (hermes-chat--pending-assistant-node)
                   (not (gethash id hermes-chat--nodes)))
          (hermes-chat--insert-entry
           (list :id id :role 'activity :assistant-id assistant-id)
           (hermes-chat--pending-assistant-node)))))))

(defun hermes-chat--rotate-live-tools (assistant-id next-id)
  "Transfer active tool rows from ASSISTANT-ID to NEXT-ID."
  (hermes-chat--preserve-input-point
   (let ((inhibit-read-only t)
         (buffer-undo-list t))
     (dolist (node (hash-table-values hermes-chat--nodes))
       (let* ((entry (ewoc-data node))
              (id (plist-get entry :id)))
         (when (and (memq (plist-get entry :role) '(tool progress))
                    (equal (hermes-chat--entry-assistant-id entry) assistant-id)
                    (hermes-chat--active-status-p (plist-get entry :status)))
           (let ((next (concat next-id (substring id (length assistant-id))))
                 (metadata (hermes-chat--entry-with
                            (plist-get entry :metadata) :assistant-id next-id)))
             (remhash id hermes-chat--nodes)
             (puthash next node hermes-chat--nodes)
             (ewoc-set-data node (hermes-chat--entry-with entry :id next :metadata metadata))
             ;; Capture after the rename so anchors resolve the new ID.
             (hermes-chat--preserve-readers
               (ewoc-invalidate hermes-chat--ewoc node)))))))))

(defun hermes-chat--settle-transport-entries (assistant-id status)
  "Set active transport entries for ASSISTANT-ID to STATUS."
  (hermes-chat--reasoning-row assistant-id nil)
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
            (hermes-chat--preserve-readers
              (ewoc-invalidate hermes-chat--ewoc node)))))
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
  (interactive nil hermes-chat-mode)
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
  (interactive nil hermes-chat-mode)
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

(defun hermes-chat--restore-input-tail (content &optional notice)
  "Restore literal CONTENT beside any newer draft, without queueing it.
Preserve point and narrowing.  When appending, insert error NOTICE if non-nil."
  (when-let* ((text (hermes-transport--non-empty-string content)))
    (save-excursion
      (save-restriction
        (widen)
        (atomic-change-group
          (if (string-empty-p (hermes-chat-input-string))
              (hermes-chat--replace-input-tail text)
            (hermes-chat--append-input-tail text)
            (when notice
              (hermes-chat--insert-local-status notice 'error))))))))

(defun hermes-chat--preserve-control-content (content)
  "Keep busy-control CONTENT recoverable after a dashboard bootstrap error."
  (when-let* ((text (hermes-transport--non-empty-string content)))
    (save-restriction
      (widen)
      (if (and (not (string-empty-p (hermes-chat-input-string)))
               (null hermes-chat--queued-messages))
          (hermes-chat--queue-content
           text "Preserved busy-control text after dashboard error")
        (hermes-chat--restore-input-tail
         text "Restored busy-control text in input tail after dashboard error")))))

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

;;; Offline recovery actions

(defvar-local hermes-chat--recovery-notice-ids nil
  "Entry ids of offline notices whose recovery buttons may still be live.")

(defun hermes-chat--recovery-owner ()
  "Capture the current chat attachment for offline recovery buttons."
  (let ((client hermes-chat--dashboard-client))
    (list :buffer (current-buffer)
          :lifetime hermes-chat--lifecycle-generation
          :client client
          :connection (and (hermes-dashboard-transport-client-p client)
                           (hermes-dashboard-transport-client-generation client))
          :session hermes-chat--dashboard-active-session-id
          :recovery (and (buffer-live-p hermes-chat--recovery-buffer)
                         hermes-chat--recovery-buffer))))

(defun hermes-chat--recovery-owner-current-p (owner)
  "Return non-nil when OWNER still names this exact chat attachment.
A new lifetime, client, socket generation or attached session retires it,
so a recovery button never acts on a successor."
  (let ((client (plist-get owner :client)))
    (and (eq (current-buffer) (plist-get owner :buffer))
         (hermes-chat--current-lifetime-p (plist-get owner :lifetime))
         (eq hermes-chat--dashboard-client client)
         (equal (and (hermes-dashboard-transport-client-p client)
                     (hermes-dashboard-transport-client-generation client))
                (plist-get owner :connection))
         (equal hermes-chat--dashboard-active-session-id
                (plist-get owner :session)))))

(defun hermes-chat--recovery-actions (owner)
  "Return (LABEL COMMAND HELP) recovery actions available for OWNER."
  (delq nil
        (list
         (and (plist-get owner :client) (fboundp 'hermes-dashboard-reconnect)
              (list "[Reconnect]" 'hermes-dashboard-reconnect
                    "Reconnect this chat's dashboard socket; nothing is resent"))
         (and (buffer-live-p (plist-get owner :recovery))
              (fboundp 'hermes-chat-show-recovery)
              (list "[Recovery draft]" 'hermes-chat-show-recovery
                    "Show the input preserved for manual sending"))
         (and (fboundp 'hermes-chat-session-details)
              (list "[Session details]" 'hermes-chat-session-details
                    "Show this chat's session details")))))

(defun hermes-chat--recovery-live-p (entry)
  "Return non-nil when offline ENTRY's recovery buttons may act."
  (let ((metadata (plist-get entry :metadata)))
    (and (not (plist-get metadata :recovery-retired))
         (hermes-chat--recovery-owner-current-p
          (plist-get metadata :recovery-owner)))))

(defun hermes-chat--insert-recovery-actions (entry)
  "Insert offline ENTRY's recovery buttons, inert once its owner retires."
  (let* ((owner (plist-get (plist-get entry :metadata) :recovery-owner))
         (live (hermes-chat--recovery-live-p entry))
         (actions (hermes-chat--recovery-actions owner)))
    (when actions
      (insert (propertize " ·" 'face 'shadow))
      (pcase-dolist (`(,label ,command ,help) actions)
        (insert " ")
        (if live
            (insert-text-button
             label 'face 'link 'mouse-face 'highlight 'follow-link t
             'help-echo help
             'hermes-chat-recovery-owner owner
             'hermes-chat-recovery-command command
             'action #'hermes-chat--recovery-button)
          (insert (propertize label 'face 'shadow
                              'help-echo "Retired: this belongs to an earlier connection")))))))

(defun hermes-chat--recovery-button (button)
  "Run BUTTON's recovery command in its exact owning chat attachment."
  (let* ((owner (button-get button 'hermes-chat-recovery-owner))
         (buffer (plist-get owner :buffer)))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (or (hermes-chat--recovery-owner-current-p owner)
                       (progn (hermes-chat--retire-recovery-notices) nil))))
      (user-error "This recovery action belongs to an earlier connection"))
    (with-current-buffer buffer
      (call-interactively (button-get button 'hermes-chat-recovery-command)))))

(defun hermes-chat--retire-recovery-notices ()
  "Re-render offline notices whose owner retired, making their buttons inert."
  (when hermes-chat--recovery-notice-ids
    (setq hermes-chat--recovery-notice-ids
          (seq-filter
           (lambda (id)
             (when-let* ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
               (or (hermes-chat--recovery-live-p (ewoc-data node))
                   (progn
                     (hermes-chat--update-entry
                      id (lambda (entry)
                           (hermes-chat--entry-with
                            entry :metadata
                            (plist-put (copy-sequence (plist-get entry :metadata))
                                       :recovery-retired t)))
                      t)
                     nil))))
           hermes-chat--recovery-notice-ids))))

(defun hermes-chat--insert-offline-notice (content)
  "Insert offline CONTENT with recovery buttons bound to this attachment.
The buttons only reach existing manual commands; nothing resends.  A notice
for the same still-current attachment is not repeated."
  (hermes-chat--retire-recovery-notices)
  (unless hermes-chat--recovery-notice-ids
    (let ((entry (hermes-chat--make-entry
                  'status content 'disconnected nil
                  (list :recovery-owner (hermes-chat--recovery-owner)))))
      (hermes-chat--insert-entry entry (hermes-chat--trailing-active-assistant-node))
      (push (plist-get entry :id) hermes-chat--recovery-notice-ids))))

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
  (if (or (eq (plist-get hermes-chat--session-bootstrap :kind) 'history)
          (hermes-chat--active-turn-p) hermes-chat--queued-messages)
      (progn
        (hermes-chat--queue-content content nil display)
        (hermes-chat--drain-queued-message))
    (funcall hermes-chat--submit-function content display)))

(defun hermes-chat--make-queue-entry (content display)
  "Return a queued message entry for CONTENT and DISPLAY."
  (list :id (hermes-chat--next-id 'queue)
        :content content
        :display display
        :rejected-p nil))

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
    (setf (plist-get (car hermes-chat--queued-messages) :rejected-p) t)
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
    (hermes-chat--queue-panel-refresh-if-live)
    (hermes-chat--set-header-state
     :status 'error :activity "Queued message was not sent")))

(defun hermes-chat--drain-queued-message (&optional retry-entry)
  "Submit one queued message after the active turn settles.
Only an explicit RETRY-ENTRY identical to the head may retry a rejection."
  (when (and hermes-chat--queued-messages
             (or (not (plist-get (car hermes-chat--queued-messages) :rejected-p))
                 (eq retry-entry (car hermes-chat--queued-messages)))
             (not (hermes-chat--active-turn-p))
             (funcall hermes-chat--queue-drain-ready-function))
    (let ((entry (car hermes-chat--queued-messages)))
      (setf (plist-get entry :rejected-p) nil)
      (setq hermes-chat--queued-submit-id (plist-get entry :id))
      (condition-case err
          (funcall hermes-chat--submit-function
                   (plist-get entry :content)
                   (plist-get entry :display)
                   entry)
        (error
         (setf (plist-get entry :rejected-p) t)
         (setq hermes-chat--queued-submit-id nil)
         (hermes-chat--command-error (error-message-string err)))))))

(defun hermes-chat--clear-submit-context (context &optional no-drain)
  "Clear CONTEXT when it is still the unresolved dashboard submission.
When NO-DRAIN is non-nil, retain queued input without retrying it."
  (when (eq context hermes-chat--unsettled-submit-context)
    (let ((owner hermes-buffer--owner)
          (lifetime hermes-chat--lifecycle-generation))
      (setq hermes-chat--unsettled-submit-context nil
            hermes-chat--prepared-submit-assistant-id nil)
      ;; Admission can be the last blocker after an earlier terminal event.
      ;; Give ordinary FIFO entries priority before waking other waiting work.
      (unless no-drain
        (hermes-chat--drain-queued-message))
      (when (and (hermes-buffer--owned-p 'hermes-chat-mode)
                 (eq owner hermes-buffer--owner)
                 (hermes-chat--current-lifetime-p lifetime))
        (hermes-chat--notify-state-change)))))

(defun hermes-chat--queue-content (content &optional note display image-record)
  "Queue CONTENT for the next turn, inserting NOTE when non-nil.
DISPLAY is the compact user-turn text shown when the queued message is sent.
IMAGE-RECORD retains local bytes for an explicitly image-bearing queue entry."
  (setq hermes-chat--queued-messages
        (append hermes-chat--queued-messages
                (list (append (hermes-chat--make-queue-entry content display)
                              (and image-record (list :image-record image-record))))))
  (hermes-chat--queue-panel-refresh-if-live)
  (hermes-chat--insert-local-status
   (or note (format "Queued next message: %s"
                    (hermes-chat--preview (or display content))))
   'queued)
  (hermes-chat--set-header-state
   :status 'queued :activity "Queued next message"))

(defun hermes-chat--queue-panel-refresh-if-live ()
  "Refresh this chat's live queue side panel, when present.
Also schedule the composer's queued-message lines, which mirror the FIFO."
  (hermes-chat--status-rule-schedule)
  (when (buffer-live-p hermes-chat--queue-panel-buffer)
    (let ((owner (current-buffer)))
      (with-current-buffer hermes-chat--queue-panel-buffer
        (when (and (hermes-buffer--owned-p 'hermes-chat-queue-panel-mode)
                   (eq hermes-chat-queue-panel--owner owner))
          (hermes-chat-queue-panel-refresh))))))

(defun hermes-chat--queue-panel-owner ()
  "Return this panel's live chat owner or signal a user error."
  (unless (hermes-buffer--owned-p 'hermes-chat-queue-panel-mode)
    (user-error "Queue panel is retired"))
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

(defun hermes-chat--queue-panel-anchor (position)
  "Return the queued identity, line and column at POSITION."
  (save-excursion
    (goto-char position)
    (list (get-text-property position 'hermes-chat-queue-id)
          (if (zerop (buffer-size)) 3 (line-number-at-pos))
          (current-column))))

(defun hermes-chat--queue-panel-position (anchor)
  "Resolve ANCHOR, falling back to the nearest remaining queue row."
  (save-excursion
    (goto-char (point-min))
    (let ((position (and (car anchor)
                         (let ((match (text-property-search-forward
                                       'hermes-chat-queue-id (car anchor) #'equal)))
                           (and match (prop-match-beginning match))))))
      (if position (goto-char position)
        (forward-line (min (1- (nth 1 anchor))
                           (max 2 (- (line-number-at-pos (point-max)) 2)))))
      (move-to-column (nth 2 anchor))
      (point))))

(defun hermes-chat-queue-panel-refresh (&rest _)
  "Render the owning chat's FIFO entries in the current side panel."
  (interactive nil hermes-chat-queue-panel-mode)
  (let* ((owner (hermes-chat--queue-panel-owner))
         (entries (buffer-local-value 'hermes-chat--queued-messages owner))
         (anchor (hermes-chat--queue-panel-anchor (point)))
         (windows (mapcar
                   (lambda (window)
                     (list window
                           (hermes-chat--queue-panel-anchor (window-start window))
                           (hermes-chat--queue-panel-anchor (window-point window))))
                   (get-buffer-window-list (current-buffer) nil t)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Hermes queued messages" 'face 'bold) "\n\n")
    (if entries
        (cl-loop for entry in entries
                 for index from 1
                 do (let ((start (point)))
                      (insert (format "%d. %s%s\n" index
                                      (if (plist-get entry :rejected-p)
                                          (propertize "Rejected [r: Retry queued message] "
                                                      'face 'error)
                                        "")
                                      (hermes-chat--preview
                                       (plist-get entry :content))))
                      (add-text-properties
                       start (point)
                       (list 'hermes-chat-queue-id (plist-get entry :id)))))
      (insert "No queued messages\n"))
    (goto-char (hermes-chat--queue-panel-position anchor))
    (dolist (state windows)
      (set-window-point (car state)
                        (hermes-chat--queue-panel-position (nth 2 state)))
      (set-window-start (car state)
                        (hermes-chat--queue-panel-position (nth 1 state)) t))))

(defun hermes-chat--queue-edit-entry (owner id)
  "Edit queue entry ID owned by chat buffer OWNER from this queue panel."
  (let ((panel (current-buffer))
        (claim hermes-buffer--owner))
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
        (unless (and (buffer-live-p panel)
                     (with-current-buffer panel
                       (and (eq claim hermes-buffer--owner)
                            (eq owner (hermes-chat--queue-panel-owner)))))
          (user-error "Queue panel changed during editing"))
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
        (when-let* ((record (plist-get entry :image-record)))
          (setf (plist-get record :content) content))
        (hermes-chat--status-rule-schedule)
        (hermes-chat--insert-local-status "Queued message updated" 'done)))))

(defun hermes-chat--queue-remove-entry (owner id)
  "Remove queue entry ID owned by chat buffer OWNER."
  (with-current-buffer owner
    (when (equal id hermes-chat--queued-submit-id)
      (user-error "That queued message is currently being submitted"))
    (unless (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                      hermes-chat--queued-messages)
      (user-error "Queued message is no longer present"))
    (when-let* ((entry (seq-find (lambda (entry) (equal (plist-get entry :id) id))
                                hermes-chat--queued-messages))
                (record (plist-get entry :image-record)))
      (setf (plist-get record :state) 'removed))
    (setq hermes-chat--queued-messages
          (seq-remove (lambda (entry) (equal (plist-get entry :id) id))
                      hermes-chat--queued-messages))
    (hermes-chat--status-rule-schedule)
    (hermes-chat--insert-local-status "Queued message removed" 'done)))

(defun hermes-chat-queue-panel-retry ()
  "Retry the rejected FIFO head at point when its chat is idle and attached.
Refuse stale panels and busy or disconnected owners without clearing the pause."
  (interactive nil hermes-chat-queue-panel-mode)
  (let ((owner (hermes-chat--queue-panel-owner))
        (panel (current-buffer))
        (attachment hermes-chat-queue-panel--attachment)
        (id (hermes-chat--queue-panel-entry-id)))
    (with-current-buffer owner
      (unless (and (eq panel hermes-chat--queue-panel-buffer)
                   (eql (nth 0 attachment) hermes-chat--lifecycle-generation)
                   (eq (nth 1 attachment) hermes-chat--dashboard-client)
                   (equal (nth 2 attachment) hermes-chat--dashboard-active-session-id))
        (user-error "Queue owner changed; reopen the queue panel"))
      (let ((entry (car hermes-chat--queued-messages)))
        (unless (and (equal id (plist-get entry :id))
                     (plist-get entry :rejected-p))
          (user-error "Select the rejected first queued message"))
        (when (or (hermes-chat--active-turn-p)
                  (not (funcall hermes-chat--queue-drain-ready-function)))
          (user-error "The owning Hermes chat is busy or disconnected"))
        (hermes-chat--drain-queued-message entry)))
    (hermes-chat-queue-panel-refresh)))

(defun hermes-chat-queue-panel-edit ()
  "Edit the queued message at point."
  (interactive nil hermes-chat-queue-panel-mode)
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
                                     (t candidate))))
        (hermes-chat--status-rule-schedule)))))

(defun hermes-chat-queue-panel-move-up ()
  "Move the queued message at point one position earlier."
  (interactive nil hermes-chat-queue-panel-mode)
  (hermes-chat--queue-panel-move-entry
   (hermes-chat--queue-panel-owner) (hermes-chat--queue-panel-entry-id) -1)
  (hermes-chat-queue-panel-refresh))

(defun hermes-chat-queue-panel-move-down ()
  "Move the queued message at point one position later."
  (interactive nil hermes-chat-queue-panel-mode)
  (hermes-chat--queue-panel-move-entry
   (hermes-chat--queue-panel-owner) (hermes-chat--queue-panel-entry-id) 1)
  (hermes-chat-queue-panel-refresh))

(defun hermes-chat-queue-panel-remove ()
  "Remove the queued message at point."
  (interactive nil hermes-chat-queue-panel-mode)
  (let ((owner (hermes-chat--queue-panel-owner))
        (id (hermes-chat--queue-panel-entry-id)))
    (hermes-chat--queue-remove-entry owner id))
  (hermes-chat-queue-panel-refresh))

(defvar-keymap hermes-chat-queue-panel-mode-map
  :doc "Keymap for the Hermes chat queue side panel."
  :parent special-mode-map
  "g" #'hermes-chat-queue-panel-refresh
  "e" #'hermes-chat-queue-panel-edit
  "r" #'hermes-chat-queue-panel-retry
  "u" #'hermes-chat-queue-panel-move-up
  "d" #'hermes-chat-queue-panel-move-down
  "D" #'hermes-chat-queue-panel-remove
  "q" #'quit-window)

(define-derived-mode hermes-chat-queue-panel-mode special-mode "Hermes Queue"
  "Major mode for editing one chat buffer's queued messages."
  :interactive nil)

(defun hermes-chat-queue-panel ()
  "Display an editable FIFO queue side panel for the current chat buffer."
  (interactive nil hermes-chat-mode)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (let* ((owner (current-buffer))
         (attachment (list hermes-chat--lifecycle-generation
                           hermes-chat--dashboard-client
                           hermes-chat--dashboard-active-session-id))
         (buffer (hermes-buffer--get
                  (format "*Hermes Queue: %s*" (buffer-name owner))
                  #'hermes-chat-queue-panel-mode t)))
    (setq hermes-chat--queue-panel-buffer buffer)
    (with-current-buffer buffer
      (setq hermes-chat-queue-panel--owner owner
            hermes-chat-queue-panel--attachment attachment)
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
  '((t :inherit shadow))
  "Face for the chat header's working directory.
The header shows the gateway cwd only while it differs from the project
naming the chat, so the segment stays quiet."
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

(defvar-local hermes-chat--launch-project-root nil
  "Launching Emacs project root for this chat, or nil.")

(defvar-local hermes-chat--working-directory nil
  "Gateway-native working directory for this chat session.
This path belongs to the Hermes instance and need not exist on Emacs's host.")

(defvar-local hermes-chat--initial-working-directory nil
  "First gateway working directory recorded for this chat, or nil.
It names a chat without a launch project; later gateway directory changes
leave it, and therefore the buffer name, unchanged.")

(defvar-local hermes-chat--project-identity nil
  "Cached (DIRECTORY ROOT . NAME) project identity for the naming directory.")

(defvar-local hermes-chat--cwd-worktree nil
  "Cached (CWD . WORKTREE) for the last recorded gateway cwd.
WORKTREE is (ROOT . TOP) when CWD lies in a linked Git worktree, ROOT being
its main project root and TOP the worktree's top, and nil otherwise.  It is
resolved when the cwd is recorded so redisplay never probes the filesystem.")

(defvar-local hermes-chat--resolved-start-mode nil
  "Concrete `spawn' or `remote' transport mode owned by this chat buffer.")

(defun hermes-chat--current-working-directory ()
  "Return this chat's gateway cwd, falling back only for local gateways."
  (or hermes-chat--working-directory
      (and (eq hermes-chat--resolved-start-mode 'spawn) default-directory)))

(defun hermes-chat--naming-directory ()
  "Return the directory naming this chat's project.
Prefer the launch-project root, then the first recorded gateway directory.
Before any is recorded, use the current gateway or editor directory."
  (or hermes-chat--launch-project-root
      hermes-chat--initial-working-directory
      hermes-chat--working-directory
      default-directory))

(defmacro hermes-chat--with-local-files (&rest body)
  "Run BODY with file name handlers disabled and a local `default-directory'.
A handler-managed `default-directory', such as a TRAMP chat buffer's, is
otherwise consulted even when expanding absolute or `~' paths."
  (declare (indent 0) (debug t))
  `(let ((file-name-handler-alist nil)
         (default-directory "/"))
     ,@body))

(defun hermes-chat--local-directory-p (directory)
  "Return non-nil when gateway-native DIRECTORY is a plain local directory.
Refuse relative, `~'-prefixed and file-name-handler paths without probing
them.  Existence does not prove the gateway shares Emacs's filesystem."
  (and (stringp directory)
       (file-name-absolute-p directory)
       (not (string-prefix-p "~" directory))
       (not (find-file-name-handler directory nil))
       (hermes-chat--with-local-files (file-directory-p directory))))

(defun hermes-chat--directory-basename (&optional directory)
  "Return the final component of gateway-native DIRECTORY, or `detached'."
  (if-let* ((directory (or directory (hermes-chat--current-working-directory))))
      (let ((trimmed (replace-regexp-in-string "[/\\\\]+\\'" "" directory)))
        (cond
         ((string-empty-p trimmed) directory)
         ((string-match "[^/\\\\]+\\'" trimmed) (match-string 0 trimmed))
         (t trimmed)))
    "detached"))

(defconst hermes-chat--git-pointer-limit 4096
  "Maximum bytes of a Git `.git' or `commondir' pointer line.")

(defun hermes-chat--plain-local-path (path)
  "Return absolute PATH when no file name handler manages it, else nil.
Only the name is inspected; nothing is probed."
  (and (stringp path)
       (file-name-absolute-p path)
       (not (string-prefix-p "~" path))
       (not (find-file-name-handler path nil))
       path))

(defun hermes-chat--read-git-pointer (file prefix)
  "Return the path following PREFIX on plain local FILE's first line, or nil.
Read at most `hermes-chat--git-pointer-limit' bytes literally.  Refuse a
first line that the bound cuts off, an empty value and control characters."
  (when-let* ((file (hermes-chat--plain-local-path file))
              ((file-regular-p file))
              (line (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally
                       file nil 0 (1+ hermes-chat--git-pointer-limit))
                      (goto-char (point-min))
                      ;; A line longer than the limit is either cut off
                      ;; or unterminated within the bytes read.
                      (let ((end (line-end-position)))
                        (and (<= (1- end) hermes-chat--git-pointer-limit)
                             (buffer-substring-no-properties
                              (point-min) end)))))
              (line (decode-coding-string
                     line (or file-name-coding-system
                              default-file-name-coding-system
                              'utf-8)))
              ((string-match (concat "\\`" (regexp-quote prefix)
                                     "[ \t]*\\([^[:cntrl:]]*[^[:cntrl:] \t]\\)"
                                     "[ \t\r]*\\'")
                             line)))
    (match-string 1 line)))

(defun hermes-chat--git-pointer-path (value directory)
  "Return Git pointer VALUE expanded against DIRECTORY, or nil.
Refuse a `~'-prefixed VALUE.  Call within `hermes-chat--with-local-files'."
  (and (stringp value)
       (not (string-prefix-p "~" value))
       (hermes-chat--plain-local-path (expand-file-name value directory))))

(defun hermes-chat--read-linked-worktree (directory)
  "Return (ROOT TOP . NAME) for DIRECTORY, read from Git metadata.
See `hermes-chat--git-linked-worktree'.  Call within
`hermes-chat--with-local-files'."
  (when-let* ((top (locate-dominating-file directory ".git"))
              ;; `locate-dominating-file' abbreviates, e.g. to `~/'.
              (top (file-name-as-directory (expand-file-name top)))
              (gitdir (hermes-chat--git-pointer-path
                       (hermes-chat--read-git-pointer
                        (expand-file-name ".git" top) "gitdir:")
                       top))
              (common (hermes-chat--git-pointer-path
                       (hermes-chat--read-git-pointer
                        (expand-file-name "commondir" gitdir) "")
                       gitdir))
              (common (directory-file-name common)))
    (if (equal (file-name-nondirectory common) ".git")
        (list (file-name-directory common) top)
      (cons (file-name-as-directory common)
            (cons top (hermes-chat--bare-repository-name common))))))

(defun hermes-chat--git-linked-worktree (directory)
  "Return (ROOT TOP . NAME) when local DIRECTORY is in a linked Git worktree.
TOP is the worktree's top directory.  ROOT is its main working tree, with
NAME nil, or for a bare repository the repository itself, with NAME its
basename less \".git\".  Only the `.git' file and `commondir' are read;
Git is never run.  Metadata is resolved within
`hermes-chat--with-local-files', so no handler is ever invoked.  Return nil
for main checkouts, submodules, unreadable metadata and a ROOT or TOP that
a handler would manage."
  (when-let* (((hermes-chat--plain-local-path directory))
              (linked (hermes-chat--with-local-files
                        (hermes-chat--read-linked-worktree directory)))
              ((hermes-chat--plain-local-path (car linked)))
              ((hermes-chat--plain-local-path (cadr linked))))
    linked))

(defun hermes-chat--bare-repository-name (common)
  "Return the project name of bare repository directory COMMON.
Strip a \".git\" suffix; name a hidden layout such as `PROJECT/.bare'
after its parent directory."
  (let ((name (string-remove-suffix ".git" (file-name-nondirectory common))))
    (if (or (string-empty-p name) (string-prefix-p "." name))
        (hermes-chat--directory-basename (file-name-directory common))
      name)))

(defun hermes-chat--local-project-identity (directory)
  "Return (ROOT . NAME) for existing local DIRECTORY's project, or nil.
A linked Git worktree resolves to its main repository's project.  Call
within `hermes-chat--with-local-files'."
  (let* ((linked (hermes-chat--read-linked-worktree directory))
         (root (if linked (car linked) directory)))
    (cond
     ((cddr linked) (cons root (cddr linked)))
     ((and linked (not (file-directory-p root)))
      (cons root (hermes-chat--directory-basename root)))
     (t
      (when-let* ((project (project-current nil (file-name-as-directory root)))
                  (root (expand-file-name (project-root project))))
        (cons root (or (hermes-transport--non-empty-string
                        (project-name project))
                       (hermes-chat--directory-basename root))))))))

(defun hermes-chat--resolve-project-identity (directory)
  "Return (ROOT . NAME) for DIRECTORY's project.
Consult Git worktree metadata, `project-current' and `project-name' only
for an existing local DIRECTORY, within `hermes-chat--with-local-files'
so no handler is ever invoked.  Otherwise, or when a handler would manage
the resolved ROOT, ROOT is DIRECTORY and NAME its basename."
  (or (when-let* (((hermes-chat--local-directory-p directory))
                  ;; Naming is cosmetic: a failing project backend or a
                  ;; malformed `.dir-locals.el' must not prevent opening or
                  ;; renaming a chat.
                  (identity (condition-case nil
                                (hermes-chat--with-local-files
                                  (hermes-chat--local-project-identity
                                   directory))
                              (error nil)))
                  ((hermes-chat--plain-local-path (car identity))))
        identity)
      (cons directory (hermes-chat--directory-basename directory))))

(defun hermes-chat--note-working-directory (directory)
  "Resolve and cache the linked worktree, if any, holding gateway DIRECTORY."
  (unless (equal directory (car hermes-chat--cwd-worktree))
    (setq hermes-chat--cwd-worktree
          (cons directory
                (when-let* (((hermes-chat--local-directory-p directory))
                            (linked (condition-case nil
                                        (hermes-chat--git-linked-worktree
                                         directory)
                                      (error nil))))
                  (cons (car linked) (cadr linked)))))))

(defun hermes-chat--project-identity (directory)
  "Return (ROOT . NAME) for DIRECTORY.
Cache the result when DIRECTORY is this chat buffer's naming directory."
  (let ((cached hermes-chat--project-identity))
    (if (and cached (equal directory (car cached)))
        (cdr cached)
      (let ((identity (hermes-chat--resolve-project-identity directory)))
        (when (and (derived-mode-p 'hermes-chat-mode)
                   (equal directory (hermes-chat--naming-directory)))
          (setq hermes-chat--project-identity (cons directory identity)))
        identity))))

(defun hermes-chat--project-root-for-display ()
  "Return the cached project root naming this chat, without probing paths."
  (let ((directory (hermes-chat--naming-directory))
        (cached hermes-chat--project-identity))
    (if (and cached (equal directory (car cached))) (cadr cached) directory)))

(defun hermes-chat--directory-key (directory)
  "Return gateway-native DIRECTORY without trailing separators."
  (let ((trimmed (replace-regexp-in-string "[/\\\\]+\\'" "" directory)))
    (if (string-empty-p trimmed) directory trimmed)))

(defun hermes-chat--directory-label (directory root)
  "Return gateway-native DIRECTORY abbreviated for display beside ROOT.
Show a descendant of ROOT relative to it, and abbreviate a path under the
user's home directory with `~'.  Never consult the filesystem or a file
name handler."
  (hermes-chat--with-local-files
    (let ((key (hermes-chat--directory-key directory))
          (prefix (concat (hermes-chat--directory-key root) "/"))
          (home (directory-file-name (expand-file-name "~"))))
      (cond
       ((string-prefix-p prefix key) (substring key (length prefix)))
       ((or (equal key home) (string-prefix-p (concat home "/") key))
        (concat "~" (substring key (length home))))
       (t key)))))

(defun hermes-chat--divergent-directory-label ()
  "Return \"→ CWD\" while the gateway cwd differs from the project root.
Show a descendant of the root relative to it, a linked worktree of the same
project as \"…/WORKTREE\", and other paths abbreviated.  Return nil when the
cwd is unknown or is the root.  Never consult the filesystem or a file
name handler."
  (when-let* ((directory (hermes-chat--current-working-directory))
              (root (hermes-chat--project-root-for-display))
              (root-key (hermes-chat--directory-key root))
              ((not (equal (hermes-chat--directory-key directory) root-key))))
    (let ((linked (and (equal (car hermes-chat--cwd-worktree) directory)
                       (cdr hermes-chat--cwd-worktree))))
      (concat
       "→ "
       (if (and linked
                (equal (hermes-chat--directory-key (car linked)) root-key)
                (not (string-prefix-p (concat root-key "/")
                                      (hermes-chat--directory-key directory))))
           (concat "…/" (hermes-chat--directory-label
                         directory (hermes-chat--with-local-files
                                     (file-name-directory
                                      (hermes-chat--directory-key
                                       (cdr linked))))))
         (hermes-chat--directory-label directory root))))))

(defun hermes-chat--header-directory-segment ()
  "Return the propertized gateway cwd when it differs from the project root.
Return `detached' while the cwd is unknown, and nil when it is the chat's
project root."
  (when-let* ((label (if (hermes-chat--current-working-directory)
                         (hermes-chat--divergent-directory-label)
                       "detached")))
    (propertize label 'face 'hermes-chat-header-directory)))

(defun hermes-chat--header-detail (label)
  "Return the live detail to append after LABEL in the header, or nil.
The activity is used, with a leading copy of LABEL stripped so a label-prefixed
activity is not shown twice.  Tool commands are deliberately not surfaced here:
the header keeps the kawaii thinking status as its only live detail, while the
transcript carries the full tool detail."
  (when-let* ((activity (hermes-transport--non-empty-string
                         (plist-get hermes-chat--status-state :activity))))
    (let ((case-fold-search t))
      (cond
       ;; Thinking and Reasoning name the same live state, not extra detail.
       ((and (equal label "Thinking")
             (string-match-p
              "\\`[^[:alpha:]]*\\(?:Thinking\\|Reasoning\\)[.…[:space:]]*\\'" activity))
        nil)
       ((string-prefix-p (downcase label) (downcase activity))
        (hermes-transport--non-empty-string
         (string-trim (substring activity (length label)) "[-: ]+")))
       (t activity)))))

(defun hermes-chat--setting-value (value &optional pending label)
  "Return bounded, faced setting VALUE, marking PENDING choices explicitly.
With LABEL, budget the complete action label, not only its value."
  (let* ((value (hermes-transport--non-empty-string value))
         (pending (and value pending))
         (prefix (and label (concat label (and pending " (pending)") ": ")))
         (suffix (and (not label) pending " (pending)")))
    (concat
     prefix
     (propertize
      (concat (truncate-string-to-width
               (replace-regexp-in-string "[\n\r\t]" " " (or value "unknown"))
               (- 40 (string-width (or prefix "")) (length suffix)) nil nil "…")
              suffix)
      'face (cond ((null value) 'shadow) (pending 'warning)
                  (t 'keymap-popup-value))
      'help-echo value))))

(defun hermes-chat--pending-setting-p ()
  "Return non-nil when create-time settings belong to this chat session."
  (or (null hermes-chat--dashboard-active-session-id)
      hermes-chat--create-override-owner
      (equal hermes-chat--create-overrides-retry-session-id
             hermes-chat--dashboard-active-session-id)))

(defun hermes-chat--header-model-segment ()
  "Return the propertized header model segment, or nil."
  (and-let* ((model (hermes-transport--non-empty-string hermes-chat--model)))
    (propertize model 'face 'hermes-chat-header-model)))

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

(declare-function hermes-chat-context "hermes-context" ())

(defun hermes-chat-context-at-mouse (event)
  "Open the context budget of the chat whose header line received EVENT.
Select EVENT's window first, so an unselected chat cannot borrow the
selected chat's context."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (select-window window))
    (unless (derived-mode-p 'hermes-chat-mode)
      (user-error "No Hermes chat under the mouse"))
    (require 'hermes-context)
    (call-interactively #'hermes-chat-context)))

(defvar hermes-chat-context-label-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'hermes-chat-context-at-mouse)
    map)
  "Keymap active on the header line's context-window usage.")

(defun hermes-chat--context-percent (context)
  "Return CONTEXT's used share of its window as a float percentage, or nil."
  (let ((used (plist-get context :used))
        (max (plist-get context :max)))
    (and (numberp used) (numberp max) (> max 0)
         (/ (* 100.0 used) max))))

(defun hermes-chat--context-face (percent)
  "Return the header face for context usage PERCENT.
Thresholds follow the TUI: warning from 80%, error from 95%."
  (cond ((null percent) 'hermes-chat-header-context)
        ((>= percent 95) 'error)
        ((>= percent 80) 'warning)
        (t 'hermes-chat-header-context)))

(defun hermes-chat--header-context-segment ()
  "Return the propertized context-window segment, or nil.
Usage near the limit takes warning or error faces; `help-echo' gives exact
tokens and `hermes-chat-context-label-map' opens the context budget view."
  (when-let* ((context (hermes-chat--format-context hermes-chat--context)))
    (let* ((percent (hermes-chat--context-percent hermes-chat--context))
           (used (plist-get hermes-chat--context :used)))
      (propertize
       context 'face (hermes-chat--context-face percent)
       'mouse-face 'highlight
       'keymap hermes-chat-context-label-map
       'help-echo (format "Context %s / %d tokens%s\nmouse-1: context budget"
                          (if (numberp used) (format "%d" used) "?")
                          (plist-get hermes-chat--context :max)
                          (if percent (format " (%.1f%%)" percent) ""))))))

(defconst hermes-chat--header-state-codes
  '(("Ready" . "RDY") ("Running" . "RUN") ("Error" . "ERR")
    ("Approval requested" . "AP?") ("Input requested" . "IN?")
    ("Disconnected" . "OFF") ("Thinking" . "THK") ("Waiting" . "WAIT")
    ("Loading" . "LOAD") ("Connecting" . "CON") ("Streaming" . "STR")
    ("Queued" . "Q") ("Handing off" . "HAND") ("Interrupted" . "INT")
    ("Cancelled" . "CAN") ("Idle" . "IDL"))
  "Compact header codes, also explained in local session details.")

(defvar-local hermes-chat--work-owner nil
  "Exact attachment owning observed work, requests and cadence timer.")

(defface hermes-work-running
  '((t :inherit font-lock-keyword-face))
  "Face for currently observed running work."
  :group 'hermes)

(defface hermes-work-unknown
  '((t :inherit shadow))
  "Face for unknown, partial or stale work observations."
  :group 'hermes)

(defun hermes-chat--work-source (&optional kind)
  "Return current attachment's KIND source, or stale detached evidence.
KIND defaults to delegates."
  (let ((source (plist-get hermes-chat--work-owner (or kind :delegates)))
        (current-p (plist-get hermes-chat--work-owner :current-p)))
    (if (and current-p (funcall current-p hermes-chat--work-owner))
        source
      (plist-put (copy-sequence source) :coverage 'stale))))

(defun hermes-chat--work-running-count (source)
  "Return the observed running count in normalized SOURCE, even if stale."
  (seq-count (lambda (row) (eq (plist-get row :state) 'running))
             (plist-get source :rows)))

(declare-function hermes-chat-work "hermes-subagents" ())

(defun hermes-chat-work-at-mouse (event)
  "Show observed work for the chat whose header line received EVENT.
Select EVENT's window first, so an unselected chat cannot borrow the
selected chat's context."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (select-window window))
    (unless (derived-mode-p 'hermes-chat-mode)
      (user-error "No Hermes chat under the mouse"))
    (require 'hermes-subagents)
    (call-interactively #'hermes-chat-work)))

(defvar hermes-chat-work-label-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'hermes-chat-work-at-mouse)
    (define-key map [mode-line mouse-1] #'hermes-chat-work-at-mouse)
    map)
  "Keymap active on the header line's observed work count.")

(defun hermes-chat--work-label-help ()
  "Return the work count's help text, derived from its keymap."
  (let ((key (where-is-internal #'hermes-chat-work-at-mouse
                                hermes-chat-work-label-map t)))
    (format "%s: show observed workers and processes"
            (if key (single-key-description (aref key (1- (length key))))
              "mouse-1"))))

(defun hermes-chat--work-label (compact)
  "Return kind-specific active counts, or nil for fully current empty work.
Use COMPACT notation when non-nil; incomplete observations remain explicit."
  (when hermes-chat--work-owner
    (let* ((sources (mapcar #'hermes-chat--work-source '(:delegates :processes)))
           (counts (mapcar (lambda (source)
                             (if (memq (plist-get source :coverage) '(current partial))
                                 (hermes-chat--work-running-count source) 0)) sources))
           (unknown (seq-some
                     (lambda (source)
                       (or (not (eq (plist-get source :coverage) 'current))
                           (seq-some (lambda (row) (eq (plist-get row :state) 'unknown))
                                     (plist-get source :rows)))) sources))
           (parts (cl-mapcar
                   (lambda (count kind)
                     (when (> count 0)
                       (cond
                        (compact (format "%d%s" count kind))
                        ((equal kind "a")
                         (format "%s %d" (if (char-displayable-p ?🤖) "🤖" "Agents") count))
                        ((char-displayable-p ?⚙) (format "⚙ %d" count))
                        (t (format "%d %s" count (if (= count 1) "process" "processes"))))))
                   counts '("a" "p")))
           (active (string-join (delq nil parts) " / ")))
      (unless (and (string-empty-p active) (not unknown))
        (propertize
         (if (string-empty-p active)
             (format "%s ?" (if (char-displayable-p ?🤖) "🤖" "Agents"))
           (concat active (and unknown " ?")))
         'face (if (string-empty-p active) 'hermes-work-unknown 'hermes-work-running)
         'mouse-face 'highlight
         'help-echo (hermes-chat--work-label-help)
         'local-map hermes-chat-work-label-map)))))


(defun hermes-chat--work-details ()
  "Return local source coverage without fetching or claiming total liveness."
  (if (not hermes-chat--work-owner)
      "Session work: unavailable here."
    (concat
     "Session work: observed delegates and registered processes; not a full work ledger.\n"
     (mapconcat
      (lambda (kind)
        (let ((source (hermes-chat--work-source kind)))
          (format "%s: %s%s; %d running in %d observed rows. Observed: %s. %s"
                  (if (eq kind :delegates) "Delegates" "Processes (runtime-scoped)")
                  (or (plist-get source :coverage) 'unbound)
                  (if (plist-get source :paused) "; automatic refresh paused" "")
                  (hermes-chat--work-running-count source)
                  (length (plist-get source :rows))
                  (if-let* ((time (plist-get source :observed)))
                      (format-time-string "%F %T" (seconds-to-time time)) "—")
                  (or (plist-get source :reason) ""))))
      '(:delegates :processes) "\n")
     "\nRefresh: M-x hermes-chat-work-refresh. Disappearance does not prove completion.")))

(defun hermes-chat--truncate-left (string width)
  "Return STRING fitting WIDTH columns, keeping its end after an ellipsis.
A path's final components are its most specific part."
  (if (<= (string-width string) width)
      string
    (let ((start (length string))
          (used 0))
      (while (and (> start 0)
                  (<= (+ used (char-width (aref string (1- start))))
                      (- width (string-width "…"))))
        (setq start (1- start)
              used (+ used (char-width (aref string start)))))
      (concat (propertize "…" 'face (get-text-property 0 'face string))
              (substring string start)))))

(defun hermes-chat--header-fit (width status yolo directory optional &optional work)
  "Fit STATUS, YOLO, DIRECTORY and OPTIONAL segments within WIDTH columns.
Reserve state and risk, then WORK, before identity.
Drop optional segments whole."
  (let* ((separator (propertize " | " 'face 'shadow))
         (risk (and yolo (propertize (cond ((= width 1) "!")
                                          ((or (< width 12)
                                               (< width (+ (string-width (or status "")) 7))) "Y!")
                                          (t "YOLO"))
                                    'face 'hermes-chat-header-warning)))
         (required (string-join (delq nil (list status risk))
                                (if (< width 30) " " separator)))
         (required (if (and work (<= (+ (string-width required)
                                         (if (string-empty-p required) 0 3)
                                         (string-width work)) width))
                       (string-join (delq nil (list (unless (string-empty-p required) required) work)) separator) required))
         (room (- width (string-width required)
                  (if (string-empty-p required) 0 (string-width separator))))
         (identity (and directory
                        (or (>= width 30) (string-empty-p required)) (> room 0)
                        (hermes-chat--truncate-left directory room)))
         (text (string-join (delq nil (list identity (unless (string-empty-p required) required))) separator)))
    (if (> (string-width text) width)
        (truncate-string-to-width (or risk status "") width nil nil "")
      (dolist (part optional)
        (when (and part (<= (+ (string-width text) 3 (string-width part)) width))
          (setq text (if (string-empty-p text) part (concat text separator part)))))
      text)))

(defun hermes-chat--session-details-text ()
  "Return local, full session details without fetching or interpreting paths."
  (let ((print-length nil)
        (print-level nil))
    (format "Directory: %s\nParent: %s\nActivity: %s\nModel: %s\nRuntime: %S\nGoal: %S\nContext: %S\n\nReady means ready for parent input; work may still be running.\n%s\n\nCompact header codes: %s. Y! (or !) means YOLO.\n"
            (or (hermes-chat--current-working-directory) "detached")
            (hermes-chat--header-status-label (plist-get hermes-chat--status-state :status))
            (or (plist-get hermes-chat--status-state :activity) "—")
            (or hermes-chat--model "unknown") hermes-chat--runtime-flags
            hermes-chat--goal hermes-chat--context
            (hermes-chat--work-details)
            (mapconcat (lambda (entry) (format "%s=%s" (cdr entry) (car entry)))
                       hermes-chat--header-state-codes "; "))))

(defun hermes-chat-session-details ()
  "Display full local session details, including fields omitted from the header."
  (interactive nil hermes-chat-mode)
  (let ((text (hermes-chat--session-details-text))
        (owner hermes-chat--work-owner)
        (refresh (plist-get hermes-chat--work-owner :refresh))
        (buffer (hermes-buffer--get "*Hermes Session Details*" #'help-mode)))
    (with-help-window buffer
      (princ text)
      (when owner
        (with-current-buffer standard-output
          (insert-text-button
           "Browse observed work" 'follow-link t
           'action (lambda (_button)
                     (unless (funcall (plist-get owner :current-p) owner)
                       (user-error "Work owner detached"))
                     (with-current-buffer (plist-get owner :buffer)
                       (require 'hermes-subagents)
                       (call-interactively 'hermes-chat-work)))))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (hermes-buffer--owned-p 'help-mode)
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key (kbd "g") (lambda () (interactive)
                                    (when refresh (funcall refresh)))))))))

(defun hermes-chat--header-line (&optional width)
  "Return a priority-budgeted header fitting WIDTH display columns.
During redisplay Emacs selects the window whose header is being evaluated."
  (let* ((width (max 1 (or width (window-body-width))))
         (state (plist-get hermes-chat--status-state :status))
         (label (hermes-chat--header-status-label state))
         (short (pcase label
                  ("Approval requested" "Approval")
                  ("Input requested" "Input") ("Disconnected" "Offline")
                  (_ label)))
         ;; Reserve a space and Y! before optional detail can consume room.
         (status (and (member label '("Disconnected" "Error" "Approval requested"
                                      "Input requested" "Interrupted" "Cancelled"))
                      (propertize
                  (cond ((or (< width 12)
                             (< width (+ (string-width short)
                                         (if (plist-get hermes-chat--runtime-flags :yolo)
                                             3 0))))
                         (or (cdr (assoc label hermes-chat--header-state-codes)) "?"))
                        ((< width 30) short)
                        (t (concat (hermes-chat--status-icon state) " " label)))
                  'face (hermes-chat--header-status-face state))))
         (text (hermes-chat--header-fit
                width status (plist-get hermes-chat--runtime-flags :yolo)
                (hermes-chat--header-directory-segment)
                (list (hermes-chat--header-model-segment)
                      (hermes-chat--header-context-segment)
                      (and status (hermes-chat--header-detail label)))
                (hermes-chat--work-label (< width 50))))
         (text (if (and (char-displayable-p ?⚕)
                        (<= (+ (string-width text) (string-width "⚕ | ")) width))
                   (if (string-empty-p text)
                       "⚕"
                     (concat "⚕" (propertize " | " 'face 'shadow) text))
                 text)))
    (hermes-chat--fill-text-property
     (copy-sequence (string-replace "%" "%%" text))
     'help-echo (hermes-chat--session-details-text))))

(defun hermes-chat--fill-text-property (string property value)
  "Set PROPERTY to VALUE on STRING wherever it is unset; return STRING.
Segments carrying their own PROPERTY, such as a clickable count's
`help-echo', keep it."
  (let ((pos 0) (end (length string)))
    (while (< pos end)
      (let ((next (next-single-property-change pos property string end)))
        (unless (get-text-property pos property string)
          (put-text-property pos next property value string))
        (setq pos next)))
    string))

;;; Composer status rule and visible queue

;; The rule between transcript and composer is the EWOC footer.  While a turn
;; is active it carries the lifecycle word, elapsed time, current tool, feature
;; segments and the queued count; queued FIFO messages are listed above it.
;; Repaints replace only the footer and rebase the composer around it.

(defface hermes-chat-turn-running
  '((t :inherit font-lock-keyword-face))
  "Face for the running-turn status word on the composer rule."
  :group 'hermes)

(defcustom hermes-chat-status-rule-linger 30
  "Seconds the last turn's duration remains on the composer rule."
  :type 'number
  :group 'hermes)

(defvar hermes-chat-status-rule-segment-functions nil
  "Functions returning an optional propertized composer-rule segment.
Each runs in the chat buffer while a turn is active.  Segments follow the
current activity and precede the queued count.  The rule always stays one
visual line: line breaks become spaces, the activity is truncated first,
then the quiet hint and whole segments are dropped.")

(defvar-local hermes-chat--status-rule-start nil
  "Float time at which the active turn was first observed, or nil.")
(defvar-local hermes-chat--status-rule-last nil
  "List (DURATION FINISHED LABEL) describing the last ended turn, or nil.")
(defvar-local hermes-chat--status-rule-timer nil
  "Cons (KIND . TIMER) owning the rule's tick or linger timer, or nil.")
(defvar-local hermes-chat--status-rule-pending nil
  "Zero-delay timer owning the next coalesced footer repaint, or nil.")
(defvar-local hermes-chat--status-rule-last-event nil
  "Float time at which this chat last admitted a backend event, or nil.")

(defcustom hermes-chat-status-rule-quiet-after 30
  "Seconds without a backend event before a running turn shows its silence.
The composer rule then appends \"quiet M:SS\" until the next event arrives."
  :type 'number
  :group 'hermes)

(defun hermes-chat--status-rule-quiet (now)
  "Return seconds the active turn has heard nothing as of NOW, or nil.
Silence shorter than `hermes-chat-status-rule-quiet-after' returns nil."
  (when hermes-chat--status-rule-start
    (let ((quiet (- now (max hermes-chat--status-rule-start
                             (or hermes-chat--status-rule-last-event 0)))))
      (and (>= quiet hermes-chat-status-rule-quiet-after) quiet))))

(defun hermes-chat--status-rule-note-event ()
  "Record that a backend event reached this chat now.
Only admitted transport events count, never the rule's own timer ticks.  A
visible quiet hint is cleared by one coalesced repaint."
  (let* ((now (float-time))
         (quiet (hermes-chat--status-rule-quiet now)))
    (setq hermes-chat--status-rule-last-event now)
    (when quiet (hermes-chat--status-rule-schedule))))

(defun hermes-chat--status-rule-cancel ()
  "Cancel this chat's rule timers without touching the displayed footer."
  (when (timerp (cdr hermes-chat--status-rule-timer))
    (cancel-timer (cdr hermes-chat--status-rule-timer)))
  (when (timerp hermes-chat--status-rule-pending)
    (cancel-timer hermes-chat--status-rule-pending))
  (setq hermes-chat--status-rule-timer nil
        hermes-chat--status-rule-pending nil))

(defun hermes-chat--status-rule-retire ()
  "Retire rule timers and forget turn timing at a lifecycle boundary."
  (hermes-chat--status-rule-cancel)
  (setq hermes-chat--status-rule-start nil
        hermes-chat--status-rule-last nil
        hermes-chat--status-rule-last-event nil))

(defun hermes-chat--status-rule-live-p ()
  "Return non-nil when this buffer is a live, initialized chat.
A chat reassociated with a file stays retired after the name is detached."
  (and (derived-mode-p 'hermes-chat-mode)
       (not (hermes-buffer--retired-p))
       (not hermes-chat--cleanup-done-p)
       hermes-chat--ewoc
       (hermes-chat--input-position)))

(defun hermes-chat--status-rule-track ()
  "Record active-turn start and end transitions from the lifecycle state."
  (let ((active (hermes-chat--active-turn-p))
        (now (float-time)))
    (cond
     ((and active (not hermes-chat--status-rule-start))
      (setq hermes-chat--status-rule-start now
            hermes-chat--status-rule-last nil))
     ((and (not active) hermes-chat--status-rule-start)
      (setq hermes-chat--status-rule-last
            (list (- now hermes-chat--status-rule-start) now
                  (hermes-chat--header-status-label
                   (plist-get hermes-chat--status-state :status)))
            hermes-chat--status-rule-start nil)))))

(defun hermes-chat--status-rule-linger-left ()
  "Return seconds the last turn's duration remains shown, or nil."
  (when-let* ((last hermes-chat--status-rule-last))
    (let ((left (- (+ (nth 1 last) hermes-chat-status-rule-linger) (float-time))))
      (and (> left 0) left))))

(defun hermes-chat--status-rule-fire (buffer lifetime box)
  "Run timer BOX for BUFFER while it still owns chat LIFETIME."
  (if (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (and (eq lifetime hermes-chat--lifecycle-generation)
                  (eq (car box) (cdr hermes-chat--status-rule-timer))
                  (hermes-chat--status-rule-live-p))))
      (with-current-buffer buffer (hermes-chat--status-rule-update))
    (cancel-timer (car box))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq (car box) (cdr hermes-chat--status-rule-timer))
          (hermes-chat--status-rule-cancel))))))

(defun hermes-chat--status-rule-arm ()
  "Own exactly the timer the current rule needs.
Tick each second while a turn is active and the chat is visible; otherwise
expire the last duration once, or own no timer at all."
  (let* ((left (hermes-chat--status-rule-linger-left))
         (kind (cond ((and hermes-chat--status-rule-start
                           (get-buffer-window (current-buffer) 'visible))
                      'tick)
                     ((and (not hermes-chat--status-rule-start) left) 'linger))))
    (unless (eq kind (car hermes-chat--status-rule-timer))
      (when (timerp (cdr hermes-chat--status-rule-timer))
        (cancel-timer (cdr hermes-chat--status-rule-timer)))
      (setq hermes-chat--status-rule-timer nil)
      (when kind
        (let ((box (list nil)))
          (setcar box (run-at-time (if (eq kind 'tick) 1 left)
                                   (and (eq kind 'tick) 1)
                                   #'hermes-chat--status-rule-fire
                                   (current-buffer)
                                   hermes-chat--lifecycle-generation box))
          (setq hermes-chat--status-rule-timer (cons kind (car box))))))))

(defun hermes-chat--status-rule-update ()
  "Track the turn, own the right timer and repaint the footer now."
  (when (hermes-chat--status-rule-live-p)
    (hermes-chat--status-rule-track)
    (hermes-chat--status-rule-arm)
    (hermes-chat--status-rule-repaint)))

(defun hermes-chat--status-rule-sync ()
  "Record lifecycle transitions and coalesce a footer repaint.
Runs from state-change and window-configuration hooks, so it never edits the
buffer synchronously: callers may be inside another transcript mutation."
  (when (hermes-chat--status-rule-live-p)
    (hermes-chat--status-rule-track)
    (hermes-chat--status-rule-arm)
    (hermes-chat--status-rule-schedule)))

(defun hermes-chat--status-rule-schedule ()
  "Schedule one coalesced footer repaint owned by this chat lifetime."
  (when (and (hermes-chat--status-rule-live-p)
             (not (memq hermes-chat--status-rule-pending timer-list)))
    (let ((buffer (current-buffer))
          (lifetime hermes-chat--lifecycle-generation))
      (setq hermes-chat--status-rule-pending
            (run-at-time
             0 nil
             (lambda ()
               (hermes-chat--in-buffer buffer
                 (when (eq lifetime hermes-chat--lifecycle-generation)
                   (setq hermes-chat--status-rule-pending nil)
                   (if (hermes-chat--status-rule-live-p)
                       (hermes-chat--status-rule-update)
                     (hermes-chat--status-rule-cancel))))))))))

(defun hermes-chat--status-rule-duration (seconds &optional compact)
  "Return SECONDS as m:ss, or as 12s / 3m 05s when COMPACT."
  (let ((s (max 0 (floor seconds))))
    (cond (compact (if (< s 60) (format "%ds" s)
                     (format "%dm %02ds" (/ s 60) (% s 60))))
          ((< s 3600) (format "%d:%02d" (/ s 60) (% s 60)))
          (t (format "%d:%02d:%02d" (/ s 3600) (/ (% s 3600) 60) (% s 60))))))

(defun hermes-chat--status-rule-word ()
  "Return (WORD . FACE) for the active turn from the authoritative state."
  (let ((label (hermes-chat--header-status-label
                (plist-get hermes-chat--status-state :status))))
    (cond
     ((or (member label '("Approval requested" "Input requested"))
          (and (hash-table-p hermes-chat--pending-prompts)
               (> (hash-table-count hermes-chat--pending-prompts) 0)))
      '("Waiting for you" . warning))
     (hermes-chat--interrupt-request-pending-p '("Interrupting" . warning))
     ((equal label "Disconnected") '("Offline" . warning))
     ((equal label "Error") '("Error" . error))
     ((member label '("Connecting" "Loading" "Handing off")) (cons label 'shadow))
     (t '("Running" . hermes-chat-turn-running)))))

(defconst hermes-chat--status-rule-preview-limit 512
  "Characters of a tool summary considered for its one-line rule preview.")

(defun hermes-chat--status-rule-one-line (string)
  "Return STRING with line breaks and control characters as single spaces.
Text properties of the remaining characters are kept."
  (replace-regexp-in-string "[[:cntrl:]\u007f-\u009f\u2028\u2029]+" " " string t t))

(defun hermes-chat--status-rule-activity ()
  "Return the most recent active tool summary as one shadowed line, or nil.
The complete summary stays in the preview's `help-echo'."
  (when-let* ((summary (car (last (hermes-chat--active-tool-summaries))))
              (line (hermes-transport--non-empty-string
                     (string-trim
                      (replace-regexp-in-string
                       " \\{2,\\}" " "
                       (hermes-chat--status-rule-one-line
                        (substring-no-properties
                         summary 0 (min (length summary)
                                        hermes-chat--status-rule-preview-limit))))))))
    (propertize line 'face 'shadow 'help-echo (string-trim summary))))

(defun hermes-chat--status-rule-segments ()
  "Return one-line feature segments for the active rule, in registration order."
  (let (segments)
    (run-hook-wrapped 'hermes-chat-status-rule-segment-functions
                      (lambda (function)
                        (when-let* ((segment (funcall function)))
                          (push (hermes-chat--status-rule-one-line segment) segments))
                        nil))
    (nreverse segments)))

(defun hermes-chat--status-rule-truncate (string fits &optional minimum)
  "Return STRING, or its longest ellipsis truncation satisfying FITS, or nil.
FITS is a predicate on the candidate.  Truncations to fewer than MINIMUM
columns (default 2) count as not fitting.  Widths are monotonic in the
truncation column, so a binary search over columns suffices."
  (if (funcall fits string)
      string
    (let ((low (or minimum 2))
          (high (1- (string-width string)))
          best)
      (while (<= low high)
        (let* ((middle (/ (+ low high) 2))
               (candidate (truncate-string-to-width string middle nil nil "…")))
          (if (funcall fits candidate)
              (setq best candidate low (1+ middle))
            (setq high (1- middle)))))
      best)))

(defun hermes-chat--status-rule-fit (head activity segments queued fits
                                          &optional quiet compact)
  "Join HEAD, QUIET, ACTIVITY, SEGMENTS and QUEUED as one rule line.
FITS is a predicate accepting a candidate line, or a column count.  HEAD
and QUEUED are kept, HEAD replaced by COMPACT when only that keeps both;
SEGMENTS are kept in order while they fit, then QUIET; ACTIVITY receives
the remaining room with an ellipsis.  When even that does not fit, the
line is truncated.  Return nil when not even a truncated head fits."
  (let* ((fits (if (numberp fits)
                   (lambda (text) (<= (string-width text) fits))
                 fits))
         (separator (propertize " · " 'face 'shadow))
         (head (if (and compact
                        (not (funcall fits (string-join (delq nil (list head queued))
                                                        separator))))
                   compact
                 head))
         (join (lambda (quiet activity kept)
                 (string-join (delq nil (append (list head quiet activity) kept
                                                (list queued)))
                              separator)))
         (kept (seq-reduce (lambda (kept segment)
                             (let ((more (append kept (list segment))))
                               (if (funcall fits (funcall join nil nil more)) more kept)))
                           segments nil))
         (quiet (and quiet (funcall fits (funcall join quiet nil kept)) quiet))
         (activity
          (when-let* ((preview (and activity
                                    (hermes-chat--status-rule-truncate
                                     activity
                                     (lambda (candidate)
                                       (funcall fits (funcall join quiet candidate kept)))
                                     6))))
            (propertize preview 'face (get-text-property 0 'face activity)
                        'help-echo (get-text-property 0 'help-echo activity)))))
    (let ((line (funcall join quiet activity kept)))
      (if (funcall fits line) line (hermes-chat--status-rule-truncate head fits)))))

(defun hermes-chat--status-rule-text (fits)
  "Return propertized rule text accepted by FITS, or nil for a plain rule.
FITS is a predicate on candidate text, or a column count."
  (let ((fits (if (numberp fits) (lambda (text) (<= (string-width text) fits)) fits)))
    (cond
     (hermes-chat--status-rule-start
      (pcase-let* ((`(,word . ,face) (hermes-chat--status-rule-word))
                   (now (float-time))
                   (quiet (and (eq face 'hermes-chat-turn-running)
                               (hermes-chat--status-rule-quiet now)))
                   (count (length hermes-chat--queued-messages))
                   (clock (propertize (hermes-chat--status-rule-duration
                                       (- now hermes-chat--status-rule-start))
                                      'face 'shadow)))
        (hermes-chat--status-rule-fit
         (concat (propertize (concat "● " word) 'face face) " " clock)
         (hermes-chat--status-rule-activity)
         (hermes-chat--status-rule-segments)
         (and (> count 0)
              (propertize (format "%d queued" count) 'face 'font-lock-type-face))
         fits
         (and quiet
              (propertize (concat "quiet " (hermes-chat--status-rule-duration quiet))
                          'face 'warning))
         (concat (propertize "●" 'face face 'help-echo word) " " clock))))
     ((hermes-chat--status-rule-linger-left)
      (pcase-let ((`(,duration ,_finished ,label) hermes-chat--status-rule-last))
        (hermes-chat--status-rule-truncate
         (if (member label '("Ready" "Idle"))
             (propertize (concat "✓ " (hermes-chat--status-rule-duration duration t))
                         'face 'shadow)
           (concat (propertize label 'face (hermes-chat--header-status-face
                                            (plist-get hermes-chat--status-state :status)))
                   (propertize (concat " after "
                                       (hermes-chat--status-rule-duration duration t))
                               'face 'shadow)))
         fits))))))

(defun hermes-chat--status-rule-decorate (text)
  "Return the rule's visible prefix around TEXT, without its trailing fill."
  (concat (propertize " " 'display '(space :width 2) 'face 'hermes-chat-separator)
          " " text " "))

(defun hermes-chat--status-rule-line (text)
  "Return the composer rule carrying TEXT, or the plain rule when TEXT is nil."
  (if (not text)
      (hermes-chat--separator)
    (concat (hermes-chat--status-rule-decorate text)
            (propertize " " 'display '(space :align-to right)
                        'face 'hermes-chat-separator))))

(defun hermes-chat--footer-window-budget (window)
  "Return the pixels one footer line may occupy in WINDOW.
Exclude the line-number gutter and reserve one column for the continuation
glyph, so a line of this width never wraps."
  ;; Selecting an unselected chat window adopts its draft point.
  (save-excursion
    (with-selected-window window
      (- (window-body-width window t)
         (if display-line-numbers (line-number-display-width t) 0)
         (window-font-width window)))))

(defun hermes-chat--footer-measure (window)
  "Return a function measuring a string's pixels as WINDOW would display it.
The string is laid out in a work buffer carrying this buffer's face
remapping and display table, with WINDOW's frame fonts."
  (let ((remapping face-remapping-alist)
        (table buffer-display-table))
    (lambda (string)
      (with-current-buffer (get-buffer-create " *hermes-chat-rule-measure*")
        (setq-local face-remapping-alist remapping)
        (setq buffer-display-table table)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (erase-buffer)
          (insert string))
        (car (buffer-text-pixel-size nil window t))))))

(defun hermes-chat--footer-fits (decorate)
  "Return a predicate accepting text whose DECORATE line fits on one line.
The line must fit every window displaying this buffer, measured as that
window displays it: window-filtered face remapping can give sibling windows
different fonts, so the narrowest window need not be the tightest.  A hidden
buffer uses 78 columns."
  (let ((limits (mapcar (lambda (window)
                          (cons (hermes-chat--footer-measure window)
                                (hermes-chat--footer-window-budget window)))
                        (get-buffer-window-list (current-buffer) nil t))))
    (if (null limits)
        (lambda (text) (<= (string-width (funcall decorate text)) 76))
      (lambda (text)
        (let ((line (funcall decorate text)))
          (seq-every-p (lambda (limit) (<= (funcall (car limit) line) (cdr limit)))
                       limits))))))

(defun hermes-chat--queue-line-button (button)
  "Open the queue panel with point on BUTTON's queued message."
  (let* ((id (button-get button 'hermes-chat-queue-id))
         (panel (hermes-chat-queue-panel)))
    (with-current-buffer panel
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward
                          'hermes-chat-queue-id id #'equal)))
        (goto-char (prop-match-beginning match))))
    (when-let* ((window (get-buffer-window panel)))
      (set-window-point window (with-current-buffer panel (point)))
      (select-window window))
    panel))

(defun hermes-chat--queue-lines (fits)
  "Return dim queued-message button lines, each accepted by FITS.
FITS is a predicate on a line's text, or a column count."
  (let ((icon (if (char-displayable-p ?⏳) "⏳" "Queued"))
        (fits (if (numberp fits) (lambda (text) (<= (string-width text) fits)) fits)))
    (cl-loop
     for entry in hermes-chat--queued-messages
     for index from 1
     concat
     (let* ((source (or (plist-get entry :display) (plist-get entry :content) ""))
            (first (or (seq-find (lambda (line) (not (string-blank-p line)))
                                 (split-string source "\n"))
                       ""))
            (text (format "%s %d  %s%s" icon index
                          (if (plist-get entry :rejected-p) "Not sent: " "")
                          (string-trim (hermes-chat--status-rule-one-line first)))))
       (concat (make-text-button
                (or (hermes-chat--status-rule-truncate text fits) "…") nil
                'face (if (plist-get entry :rejected-p) 'warning 'shadow)
                'mouse-face 'highlight 'follow-link t
                'help-echo "Open the queue panel at this message"
                'hermes-chat-queue-id (plist-get entry :id)
                'action #'hermes-chat--queue-line-button)
               "\n")))))

(defun hermes-chat--footer-string ()
  "Return the footer: queued-message lines above the one-line status rule.
Every footer line is fitted to the windows displaying this buffer."
  (concat "\n" (hermes-chat--queue-lines (hermes-chat--footer-fits #'identity))
          (hermes-chat--status-rule-line
           (hermes-chat--status-rule-text
            (hermes-chat--footer-fits #'hermes-chat--status-rule-decorate)))
          "\n"))

(defun hermes-chat--status-rule-repaint ()
  "Repaint the footer when its text changed, leaving the composer intact."
  (when (hermes-chat--status-rule-live-p)
    (save-restriction
      (widen)
      (let ((hf (ewoc-get-hf hermes-chat--ewoc))
            (footer (hermes-chat--footer-string)))
        (unless (equal-including-properties footer (cdr hf))
          (hermes-chat--replace-footer (car hf) (cdr hf) footer))))))

(defun hermes-chat--replace-footer (header old new)
  "Replace footer OLD by NEW, keeping HEADER, and rebase composer state.
`ewoc-set-hf' collapses markers at the old composer start before the new
footer; restore the input marker, draft undo, point, mark, overlays and every
window's start and point by the footer's length change."
  (let* ((input (hermes-chat--input-position))
         (start (- input (length old)))
         (delta (- (length new) (length old)))
         (remap (lambda (pos)
                  (cond ((>= pos input) (+ pos delta))
                        ((> pos start) (min pos (+ input delta)))
                        (t pos))))
         (point (point))
         (mark (marker-position (mark-marker)))
         (overlays (mapcar (lambda (o) (list o (overlay-start o) (overlay-end o)))
                           (overlays-in start (point-max))))
         (windows (mapcar (lambda (w) (list w (window-start w) (window-point w)))
                          (get-buffer-window-list (current-buffer) nil t))))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (ewoc-set-hf hermes-chat--ewoc header new))
    (set-marker hermes-chat--input-marker (+ input delta))
    (hermes-chat--adjust-draft-undo input)
    (hermes-chat--protect-transcript)
    (pcase-dolist (`(,overlay ,beg ,end) overlays)
      (when (overlay-buffer overlay)
        (move-overlay overlay (funcall remap beg) (funcall remap end))))
    (when mark (set-marker (mark-marker) (funcall remap mark)))
    (goto-char (funcall remap point))
    (pcase-dolist (`(,window ,window-start ,window-point) windows)
      (set-window-point window (funcall remap window-point))
      (set-window-start window (funcall remap window-start) t))))

(provide 'hermes-chat-buffer)
;;; hermes-chat-buffer.el ends here
