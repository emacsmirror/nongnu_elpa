;;; hermes-chat.el --- EWOC chat buffer for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.3.1") (websocket "1.15"))

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

;; ERC/emacs-jabber-shaped chat buffer for hermes-el.  Transcript entries are
;; EWOC nodes before a read-only prompt; the input tail remains writable.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'diff-mode)
(require 'ewoc)
(require 'goto-addr)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)

(defcustom hermes-chat-buffer-name "*Hermes Chat*"
  "Name of the Hermes chat buffer."
  :type 'string
  :group 'hermes)

(defcustom hermes-chat-use-dashboard-transport t
  "Whether chat sends use the dashboard transport by default.
When non-nil, `hermes-chat-send' uses the dashboard/TUI WebSocket path while
`hermes-transport-send-function' remains at its default CLI fallback function.
Rebinding `hermes-transport-send-function' still overrides the chat transport,
which keeps tests and user custom transports working."
  :type 'boolean
  :group 'hermes)

(defcustom hermes-chat-dashboard-session-title "Hermes Chat"
  "Title sent when creating a dashboard transport chat session."
  :type 'string
  :group 'hermes)

(defface hermes-chat-user-input
  '((t :inherit highlight))
  "Face for submitted user turns in the chat transcript."
  :group 'hermes)

(defvar hermes-chat-state-change-hook nil
  "Hook run in a Hermes chat buffer when dashboard-visible state changes.")

(defvar-local hermes-chat--ewoc nil
  "EWOC displaying chat transcript entries in the current Hermes chat buffer.")

(defvar-local hermes-chat--input-marker nil
  "Marker at the beginning of the writable chat input tail.")

(defvar-local hermes-chat--nodes nil
  "Hash table mapping Hermes entry IDs to EWOC nodes.")

(defvar-local hermes-chat--process nil
  "Current Hermes transport process or token for this buffer.")

(defvar-local hermes-chat--dashboard-client nil
  "Dashboard transport client associated with this buffer, if any.")

(defvar-local hermes-chat--dashboard-session-ready-p nil
  "Non-nil when the dashboard client has a live session for this buffer.")

(defvar-local hermes-chat--dashboard-active-session-id nil
  "Live dashboard/TUI session ID used for submit requests and event filtering.")

(defvar-local hermes-chat--session-id nil
  "Durable Hermes session key used for future dashboard `session.resume'.")

(defvar-local hermes-chat--profile nil
  "Profile name for this chat's dashboard session, or nil for the default.")

(defvar-local hermes-chat--model nil
  "Model id reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--agent-name nil
  "Agent/profile name reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--pending-assistant-id nil
  "ID of the assistant entry awaiting transport completion.")

(defvar-local hermes-chat--transport-generation 0
  "Monotonic generation used to ignore stale transport callbacks.")

(defvar-local hermes-chat--dashboard-detached-assistant-id nil
  "Assistant entry that was pending when the dashboard transport detached.")

(defvar-local hermes-chat--dashboard-stream-assistant-id nil
  "Assistant entry that should receive live dashboard stream events.")

(defvar-local hermes-chat--dashboard-suppress-stream-p nil
  "Non-nil when a resumed live turn has no local stream target.")

(defvar-local hermes-chat--status-state nil
  "Plist describing the live status shown in the chat header.")

(defvar-local hermes-chat--active-tools nil
  "Hash table of active tool summaries shown in the chat header.")

(defvar-local hermes-chat--queued-message nil
  "Plain message queued to send after the active Hermes turn settles.")

(defvar-local hermes-chat--pending-prompts nil
  "Hash table of pending dashboard prompt requests by prompt key.")

(defvar-local hermes-chat--draining-queued-message-p nil
  "Non-nil while the queued message is being submitted.")

(defvar-local hermes-chat--ansi-fragments nil
  "Hash table of partial ANSI escape sequences by stream key.")

(defvar hermes-chat--entry-counter 0
  "Counter used to generate local chat entry IDs.")

(defconst hermes-chat--transient-entry-roles '(status progress tool)
  "Entry roles used for compact transport status/progress lines.")

(defconst hermes-chat--unknown-event-raw-preview-width 180
  "Maximum width for raw unknown transport event previews.")

(defun hermes-chat--next-id (role)
  "Return a new local entry ID for ROLE."
  (format "%s-%d" role (cl-incf hermes-chat--entry-counter)))

(defun hermes-chat--entry-with (entry &rest props)
  "Return a copy of ENTRY with PROPS applied."
  (let ((copy (copy-sequence entry)))
    (while props
      (setq copy (plist-put copy (pop props) (pop props))))
    copy))

(defun hermes-chat--entry-expanded-p (entry)
  "Return non-nil when ENTRY's detail view is expanded."
  (plist-get (plist-get entry :metadata) :expanded))

(defun hermes-chat--metadata-preserve-expanded (entry metadata)
  "Return METADATA preserving ENTRY's explicit expansion state."
  (if-let* ((tail (plist-member (plist-get entry :metadata) :expanded)))
      (plist-put metadata :expanded (cadr tail))
    metadata))

(defun hermes-chat--displayable-char-p (char)
  "Return non-nil if CHAR is safe to display in chat content."
  (or (memq char '(?\t ?\n))
      (and (>= char ?\s)
           (/= char ?\177)
           (not (and (>= char #x80)
                     (<= char #x9f))))))

(defun hermes-chat--strip-ansi-escape-sequences (content &optional fragment)
  "Return (TEXT . FRAGMENT) for CONTENT after stripping ANSI escapes.
FRAGMENT is a partial escape sequence from the same stream, or nil."
  (let ((text (concat (or fragment "") (or content "")))
        next-fragment)
    (setq text (replace-regexp-in-string
                "\e\\[[0-?]*[ -/]*[@-~]" "" text t t))
    (setq text (replace-regexp-in-string
                "\e\\][^\a\e]*\\(?:\a\\|\e\\\\\\)" "" text t t))
    (when (string-match
           "\\(\e\\][^\a]*\\|\e\\[[0-?]*[ -/]*\\|\e\\)\\'" text)
      (setq next-fragment (match-string 1 text)
            text (substring text 0 (match-beginning 1))))
    (cons text next-fragment)))

(defun hermes-chat--ansi-fragment (key)
  "Return pending ANSI fragment for KEY, or nil."
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
  "Clear pending ANSI fragment for KEY."
  (hermes-chat--record-ansi-fragment key nil))

(defun hermes-chat--sanitize-content (content &optional ansi-key)
  "Return sanitized CONTENT for display in chat buffers.
When ANSI-KEY is non-nil, preserve split ANSI sequences for that stream."
  (let* ((stripped (hermes-chat--strip-ansi-escape-sequences
                    content (hermes-chat--ansi-fragment ansi-key)))
         (text (car stripped)))
    (when ansi-key
      (hermes-chat--record-ansi-fragment ansi-key (cdr stripped)))
    (unless (multibyte-string-p text)
      (setq text (decode-coding-string text 'utf-8-unix t)))
    (with-temp-buffer
      (dolist (char (string-to-list text))
        (when (hermes-chat--displayable-char-p char)
          (insert-char char)))
      (buffer-string))))

(defun hermes-chat--strip-session-id-lines (content &optional final)
  "Return CONTENT without Hermes CLI session-id lines.
When FINAL is non-nil, also remove a final session-id line without newline."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((regexp (if final
                      "^session_id:[^\n]*\\(\n\\|\\'\\)"
                    "^session_id:[^\n]*\n")))
      (while (re-search-forward regexp nil t)
        (replace-match "" nil nil)))
    (buffer-string)))

(defun hermes-chat--sanitize-assistant-content (content &optional final)
  "Return assistant CONTENT cleaned for display.
When FINAL is non-nil, strip trailing transport metadata too."
  (hermes-chat--strip-session-id-lines
   (hermes-chat--sanitize-content content)
   final))

(defun hermes-chat--offset-range (start end)
  "Return zero-based half-open range for buffer positions START and END."
  (cons (1- start) (1- end)))

(defun hermes-chat--markdown-diff-ranges ()
  "Return diff-fence ranges in the current buffer as zero-based offsets."
  (let ((case-fold-search t)
        ranges)
    (goto-char (point-min))
    (while (re-search-forward "^```[ \t]*\\(?:diff\\|patch\\)\\(?:[ \t].*\\)?\n" nil t)
      (let ((start (point)))
        (if (re-search-forward "^```[ \t]*$" nil t)
            (push (hermes-chat--offset-range start (match-beginning 0))
                  ranges)
          (push (hermes-chat--offset-range start (point-max)) ranges))))
    ranges))

(defun hermes-chat--unified-diff-hunk-counts ()
  "Return old/new line counts for a unified diff hunk at point."
  (when (looking-at diff-hunk-header-re-unified)
    (cons (if-let* ((count (match-string 2)))
              (string-to-number count)
            1)
          (if-let* ((count (match-string 4)))
              (string-to-number count)
            1))))

(defun hermes-chat--unified-diff-hunk-header-p ()
  "Return non-nil if point is at a unified diff hunk header."
  (hermes-chat--unified-diff-hunk-counts))

(defun hermes-chat--unified-diff-header-line-p ()
  "Return non-nil if point is at unified diff file metadata."
  (looking-at
   (concat "^\\(?:diff --git \\|index \\|old mode \\|new mode \\|"
           "new file mode \\|deleted file mode \\|similarity index \\|"
           "dissimilarity index \\|rename from \\|rename to \\|"
           "copy from \\|copy to \\|--- \\|\\+\\+\\+ \\)")))

(defun hermes-chat--unified-diff-body-line-counts ()
  "Return old/new line counts for the current unified diff body line."
  (cond
   ((looking-at "^\\\\ No newline at end of file") '(0 . 0))
   ((looking-at "^\\+") '(0 . 1))
   ((looking-at "^-") '(1 . 0))
   ((looking-at "^ ") '(1 . 1))
   ((looking-at "^$") '(1 . 1))))

(defun hermes-chat--consume-unified-diff-hunk ()
  "Move over a unified diff hunk at point.
Return non-nil when the consumed hunk contains an added or removed line."
  (let ((start (point)))
    (when-let* ((counts (hermes-chat--unified-diff-hunk-counts)))
      (let ((old-left (car counts))
            (new-left (cdr counts))
            saw-change valid)
        (forward-line 1)
        (setq valid t)
        (while (and valid
                    (not (and (<= old-left 0) (<= new-left 0)))
                    (not (eobp)))
          (if-let* ((line-counts
                     (hermes-chat--unified-diff-body-line-counts)))
              (let ((old-count (car line-counts))
                    (new-count (cdr line-counts)))
                (if (or (> old-count old-left)
                        (> new-count new-left))
                    (setq valid nil)
                  (when (or (and (= old-count 1) (= new-count 0))
                            (and (= old-count 0) (= new-count 1)))
                    (setq saw-change t))
                  (setq old-left (- old-left old-count)
                        new-left (- new-left new-count))
                  (forward-line 1)))
            (setq valid nil)))
        (while (and valid
                    (not (eobp))
                    (looking-at "^\\\\ No newline at end of file"))
          (forward-line 1))
        (if (and valid saw-change)
            t
          (goto-char start)
          nil)))))

(defun hermes-chat--unified-diff-range-at-point ()
  "Return unified diff range at point as zero-based offsets, or nil."
  (let ((start (point)) saw-hunk saw-change)
    (when (or (hermes-chat--unified-diff-header-line-p)
              (hermes-chat--unified-diff-hunk-header-p))
      (while (and (not saw-hunk)
                  (hermes-chat--unified-diff-header-line-p))
        (forward-line 1))
      (let ((keep-scanning t))
        (while (and keep-scanning
                    (hermes-chat--unified-diff-hunk-header-p))
          (if (hermes-chat--consume-unified-diff-hunk)
              (setq saw-hunk t
                    saw-change t)
            (setq keep-scanning nil))))
      (when (and saw-hunk saw-change (< start (point)))
        (hermes-chat--offset-range start (point))))))

(defun hermes-chat--inline-diff-ranges ()
  "Return inline unified diff ranges as zero-based offsets."
  (let (ranges)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((range (hermes-chat--unified-diff-range-at-point)))
          (progn
            (push range ranges)
            (goto-char (1+ (cdr range))))
        (forward-line 1)))
    ranges))

(defun hermes-chat--merge-ranges (ranges)
  "Return RANGES sorted and merged."
  (let (merged)
    (dolist (range (sort (copy-sequence ranges)
                         (lambda (left right) (< (car left) (car right))))
                   (nreverse merged))
      (when (< (car range) (cdr range))
        (if (and merged (<= (car range) (cdar merged)))
            (setcdr (car merged) (max (cdr range) (cdar merged)))
          (push (cons (car range) (cdr range)) merged))))))

(defun hermes-chat--diff-ranges (content)
  "Return diff-like ranges in CONTENT as zero-based offsets."
  (with-temp-buffer
    (insert content)
    (hermes-chat--merge-ranges
     (append (hermes-chat--markdown-diff-ranges)
             (hermes-chat--inline-diff-ranges)))))

(defun hermes-chat--fontified-diff-string (content)
  "Return CONTENT fontified with `diff-mode'."
  (with-temp-buffer
    (let ((needs-final-newline (not (string-suffix-p "\n" content))))
      (insert content)
      (when needs-final-newline
        (insert "\n"))
      (delay-mode-hooks (diff-mode))
      (font-lock-mode 1)
      (font-lock-ensure (point-min) (point-max))
      (buffer-substring (point-min)
                        (if needs-final-newline
                            (1- (point-max))
                          (point-max))))))

(defun hermes-chat--fontify-diff-ranges (start content)
  "Fontify diff-like ranges from CONTENT inserted at START."
  (dolist (range (reverse (hermes-chat--diff-ranges content)))
    (let* ((begin (+ start (car range)))
           (end (+ start (cdr range)))
           (fontified (hermes-chat--fontified-diff-string
                       (buffer-substring-no-properties begin end))))
      (delete-region begin end)
      (goto-char begin)
      (insert fontified))))

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

(defun hermes-chat--scalar-string (value)
  "Return VALUE as a display string, or nil for nil."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun hermes-chat--event-value (event keys)
  "Return the first non-nil plist value in EVENT for KEYS."
  (catch 'found
    (dolist (key keys)
      (when-let* ((tail (plist-member event key))
                  (value (cadr tail)))
        (throw 'found value)))
    nil))

(defun hermes-chat--event-string (event keys)
  "Return the first scalar plist value in EVENT for KEYS as a string."
  (hermes-chat--scalar-string (hermes-chat--event-value event keys)))

(defun hermes-chat--event-phase (event)
  "Return EVENT's trailing event-name phase, if any."
  (and-let* ((name (hermes-chat--event-string event '(:event))))
    (car (last (split-string name "\\." t)))))

(defun hermes-chat--unknown-event-content (event)
  "Return visible diagnostic text for unknown transport EVENT."
  (let* ((name (or (hermes-chat--event-string event '(:event)) "unnamed"))
         (raw (plist-get event :raw))
         (preview (and raw
                       (truncate-string-to-width
                        (format "%S" raw)
                        hermes-chat--unknown-event-raw-preview-width
                        nil nil "…"))))
    (string-join
     (delq nil (list (format "Unknown Hermes transport event: %s" name)
                     (and preview (format "raw: %s" preview))))
     "\n")))

(defun hermes-chat--humanize-event-name (name)
  "Return NAME as a compact human-readable event label."
  (and name
       (string-trim
	(replace-regexp-in-string "[._-]+" " " name))))

(defun hermes-chat--status-name (status)
  "Return normalized display/comparison name for STATUS."
  (and-let* ((name (hermes-chat--scalar-string status)))
    (downcase (replace-regexp-in-string "_" "-" name))))

(defun hermes-chat--active-status-p (status)
  "Return non-nil when STATUS denotes an unsettled transport entry."
  (member (hermes-chat--status-name status)
          '("pending" "waiting" "queued" "streaming" "started" "starting"
            "loading" "connecting" "reconnecting" "running" "busy"
            "progress" "in-progress" "preparing" "requested"
            "approval-requested")))

(defun hermes-chat--finished-status-p (status)
  "Return non-nil when STATUS denotes a settled transport entry."
  (member (hermes-chat--status-name status)
          '("done" "completed" "complete" "success" "succeeded"
            "ready" "closed" "error" "failed" "failure" "cancelled"
            "canceled" "interrupted")))

(defun hermes-chat--status-icon (status)
  "Return compact icon for transport STATUS."
  (pcase (hermes-chat--status-name status)
    ((or "done" "completed" "complete" "success" "succeeded" "ready") "✓")
    ((or "error" "failed" "failure" "cancelled" "canceled" "interrupted"
         "closed") "!")
    ((or "pending" "waiting" "queued" "streaming" "started" "starting"
         "loading" "connecting" "reconnecting" "running" "busy"
         "progress" "in-progress" "preparing" "requested"
         "approval-requested") "…")
    (_ "·")))

(defun hermes-chat--header-status-label (status)
  "Return compact header label for STATUS."
  (pcase (hermes-chat--status-name status)
    ((or "done" "completed" "complete" "success" "succeeded" "ready") "Ready")
    ((or "error" "failed" "failure") "Error")
    ((or "cancelled" "canceled") "Cancelled")
    ((or "closed") "Disconnected")
    ((or "interrupted") "Interrupted")
    ((or "approval-requested") "Approval requested")
    ((or "requested") "Input requested")
    ((or "queued") "Queued")
    ((or "pending" "waiting") "Waiting")
    ((or "starting" "loading") "Loading")
    ((or "connecting" "reconnecting") "Connecting")
    ((or "streaming") "Streaming")
    ((or "started" "running" "busy" "progress" "in-progress" "preparing") "Running")
    (_ "Idle")))

(defun hermes-chat--header-status-face (status)
  "Return face for STATUS in the chat header."
  (pcase (hermes-chat--status-name status)
    ((or "done" "completed" "complete" "success" "succeeded" "ready") 'success)
    ((or "error" "failed" "failure" "cancelled" "canceled" "interrupted") 'error)
    ((or "closed") 'warning)
    ((or "pending" "waiting" "queued" "streaming" "started" "starting"
         "loading" "connecting" "reconnecting" "running" "busy"
         "progress" "in-progress" "preparing" "requested"
         "approval-requested") 'font-lock-keyword-face)
    (_ 'shadow)))

(defun hermes-chat--nonempty-string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value)
       (not (string-empty-p value))
       value))

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
  "Reset live header state for the current chat buffer."
  (setq hermes-chat--active-tools (make-hash-table :test 'equal)
        hermes-chat--status-state
        (list :status 'ready :activity "Ready" :updated (current-time)))
  (force-mode-line-update)
  (hermes-chat--notify-state-change))

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

(defun hermes-chat--active-tool-summary ()
  "Return one compact active-tool summary for the chat header."
  (let ((summaries (hermes-chat--active-tool-summaries)))
    (pcase (length summaries)
      (0 nil)
      (1 (car summaries))
      (count (format "%d tools: %s" count (string-join summaries "; "))))))

(defun hermes-chat--header-tool-key (event)
  "Return stable header key for EVENT's tool-like activity."
  (or (hermes-chat--transport-entry-id event)
      (hermes-chat--tool-name event)
      (hermes-chat--event-string event '(:event :seq :index))))

(defun hermes-chat--header-tool-summary (event)
  "Return compact header summary for tool-like EVENT."
  (hermes-chat--nonempty-string
   (pcase (plist-get event :type)
     ('progress (hermes-chat--format-progress-event event))
     ('tool (hermes-chat--format-tool-event event))
     (_ nil))))

(defun hermes-chat--remember-header-tool (event)
  "Update chat header tool state from EVENT."
  (when-let* ((summary (hermes-chat--header-tool-summary event)))
    (unless (hash-table-p hermes-chat--active-tools)
      (setq hermes-chat--active-tools (make-hash-table :test 'equal)))
    (let* ((key (or (hermes-chat--header-tool-key event) summary))
           (status (hermes-chat--transport-entry-status event)))
      (if (hermes-chat--finished-status-p status)
          (remhash key hermes-chat--active-tools)
        (puthash key summary hermes-chat--active-tools))
      (hermes-chat--set-header-state
       :status (if (hermes-chat--active-tool-summary) 'running status)
       :activity (or (hermes-chat--active-tool-summary) summary)
       :last-tool summary))))

(defun hermes-chat--header-activity-for-event (event)
  "Return a compact activity string for transport EVENT."
  (hermes-chat--nonempty-string
   (or (hermes-chat--transport-entry-content event)
       (hermes-chat--event-string event '(:content :text :preview :event)))))

(defun hermes-chat--error-status (event)
  "Return terminal status to display for an error-like transport EVENT."
  (or (hermes-chat--event-value event '(:status)) 'error))

(defun hermes-chat--capture-session-identity (event)
  "Record the model and agent name carried by a `session.info' EVENT."
  (when-let* ((model (plist-get event :model)))
    (setq hermes-chat--model model))
  (when-let* ((agent (plist-get event :agent-name)))
    (setq hermes-chat--agent-name agent)))

(defun hermes-chat--status-event-activity (event)
  "Return the header activity for a status EVENT.
`session.info' carries the model/provider, now shown in their own header
fields, so it collapses to a plain ready state instead of repeating them."
  (if (equal (hermes-chat--event-string event '(:event)) "session.info")
      "Ready"
    (or (hermes-chat--header-activity-for-event event) "Working")))

(defun hermes-chat--update-header-for-event (event)
  "Update chat header state from transport EVENT."
  (hermes-chat--capture-session-identity event)
  (pcase (plist-get event :type)
    ('delta
     (unless (hermes-chat--active-tool-summary)
       (hermes-chat--set-header-state
        :status 'streaming :activity "Writing response")))
    ('done
     (hermes-chat--clear-active-tools)
     (hermes-chat--set-header-state
      :status 'ready :activity "Ready"
      :usage (plist-get event :usage)))
    ('error
     (hermes-chat--clear-active-tools)
     (hermes-chat--set-header-state
      :status (hermes-chat--error-status event)
      :activity (or (hermes-chat--event-string event '(:content :error))
                    "Transport error")))
    ('status
     (hermes-chat--set-header-state
      :status (if (plist-get event :prompt-request-p)
                  (if (equal (hermes-chat--prompt-event-type event) "approval")
                      'approval-requested
                    'requested)
                (hermes-chat--transport-entry-status event))
      :activity (hermes-chat--status-event-activity event)))
    ((or 'progress 'tool)
     (hermes-chat--remember-header-tool event))
    ('commentary
     (hermes-chat--set-header-state :status 'running :activity "Thinking..."))
    ('diff
     (hermes-chat--set-header-state :status 'running :activity "Reviewing diff"))
    ('unknown
     (hermes-chat--set-header-state
      :status 'error
      :activity (hermes-chat--unknown-event-content event)))))

(defun hermes-chat--header-agent-name ()
  "Return the agent/profile name shown in the chat header."
  (or (hermes-chat--nonempty-string hermes-chat--agent-name)
      (hermes-chat--nonempty-string hermes-chat--profile)
      "Hermes"))

(defun hermes-chat--header-detail (label)
  "Return the live detail to append after LABEL in the header, or nil.
An active tool wins; otherwise the activity is used, with a leading copy of
LABEL stripped so a label-prefixed activity is not shown twice."
  (or (hermes-chat--active-tool-summary)
      (when-let* ((activity (hermes-chat--nonempty-string
                             (plist-get hermes-chat--status-state :activity))))
        (if (string-prefix-p (downcase label) (downcase activity))
            (hermes-chat--nonempty-string
             (string-trim (substring activity (length label)) "[-: ]+"))
          activity))))

(defun hermes-chat--header-status-segment ()
  "Return the propertized status segment: icon, label, and live detail.
The label carries the high-level state (Ready/Running/...) and an active tool
or distinct activity is appended so the live detail is not lost."
  (let* ((status (plist-get hermes-chat--status-state :status))
         (label (hermes-chat--header-status-label status))
         (detail (hermes-chat--header-detail label)))
    (propertize
     (format "%s %s" (hermes-chat--status-icon status)
             (if detail (format "%s: %s" label detail) label))
     'face (hermes-chat--header-status-face status))))

(defun hermes-chat--header-line ()
  "Return the chat buffer header line: agent, status, and model."
  (let* ((parts (delq nil
                      (list (propertize (hermes-chat--header-agent-name)
                                        'face 'mode-line-emphasis)
                            (hermes-chat--header-status-segment)
                            (hermes-chat--nonempty-string hermes-chat--model))))
         (text (concat " " (string-join parts "  |  ") " "))
         (width (max 20 (window-total-width))))
    (truncate-string-to-width text width nil nil "…")))

(defun hermes-chat--format-usage (usage)
  "Return a compact token-usage string for USAGE, or nil.
USAGE is a plist of :input and :output token counts."
  (and usage
       (let ((in (or (plist-get usage :input) 0))
             (out (or (plist-get usage :output) 0)))
         (and (or (> in 0) (> out 0))
              (format "%d↑ %d↓ tok" in out)))))

(defun hermes-chat--format-duration (duration)
  "Return DURATION as a compact seconds string, or nil."
  (cond
   ((numberp duration) (format "%.1fs" duration))
   ((stringp duration) duration)))

(defun hermes-chat--format-status-event (event)
  "Return display content for a transport status EVENT."
  (or (hermes-chat--event-string event '(:content :text :preview))
      (let ((event-name (hermes-chat--event-string event '(:event)))
            (status (or (hermes-chat--event-string event '(:status))
                        (hermes-chat--event-phase event))))
        (string-join (delq nil (list (hermes-chat--humanize-event-name event-name)
                                     status))
                     ": "))))

(defun hermes-chat--tool-name (event)
  "Return tool name from transport EVENT, if present."
  (hermes-chat--event-string event '(:name :tool-name :tool_name :tool :kind)))

(defun hermes-chat--format-progress-event (event)
  "Return display content for a tool progress EVENT."
  (let ((name (hermes-chat--tool-name event))
        (content (hermes-chat--event-string event '(:content :delta :text :preview)))
        (progress (hermes-chat--event-string event '(:progress))))
    (string-join (delq nil (list name (or content progress))) ": ")))

(defun hermes-chat--format-tool-event (event)
  "Return display content for a tool lifecycle EVENT."
  (let* ((name (hermes-chat--tool-name event))
         (status (or (hermes-chat--event-string event '(:status))
                     (hermes-chat--event-phase event)))
         (preview (hermes-chat--event-string event '(:preview :content :delta :text)))
         (duration (hermes-chat--format-duration
                    (hermes-chat--event-value event '(:duration))))
         (error (hermes-chat--event-string event '(:error))))
    (cond
     (error
      (string-join (delq nil (list name "failed" error)) ": "))
     (preview
      (string-join (delq nil (list name preview)) ": "))
     ((and status duration)
      (string-join (delq nil (list name (format "%s (%s)" status duration)))
                   " "))
     (status
      (string-join (delq nil (list name status)) " "))
     (name name))))

(defun hermes-chat--transport-entry-role (event)
  "Return EWOC entry role for transport EVENT."
  (pcase (plist-get event :type)
    ('status 'status)
    ('progress 'progress)
    ('tool 'tool)
    ('commentary 'commentary)
    ('diff 'assistant)
    ('unknown 'status)))

(defun hermes-chat--commentary-event-name (event)
  "Return EVENT's commentary event name in lowercase, or nil."
  (and-let* ((name (hermes-chat--event-string event '(:event))))
    (downcase name)))

(defun hermes-chat--commentary-delta-p (event)
  "Return non-nil when EVENT is a commentary/thinking delta."
  (and (eq (plist-get event :type) 'commentary)
       (when-let* ((name (hermes-chat--commentary-event-name event)))
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
  (when-let* ((value (hermes-chat--event-string event keys)))
    (unless (string-empty-p value)
      value)))

(defun hermes-chat--transport-entry-id (event)
  "Return stable EWOC entry id for keyed transport EVENT, or nil."
  (pcase (plist-get event :type)
    ('status
     (when-let* ((key (or (hermes-chat--transport-key-fragment
                           event '(:prompt-key :prompt_key
                                               :request-id :request_id
                                               :status-key :status_key :key :run-id
                                               :run_id :session-id :session_id
                                               :message-id :message_id))
                          (hermes-chat--transport-key-fragment
                           event '(:event)))))
       (concat "status:" key)))
    ((or 'progress 'tool)
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

(defun hermes-chat--insert-transient-content (entry)
  "Insert compact transient transport ENTRY."
  (let ((content (or (plist-get entry :content) "")))
    (unless (string-empty-p content)
      (insert (propertize (format "  %s "
                                  (hermes-chat--status-icon
                                   (plist-get entry :status)))
                          'face 'shadow))
      (let ((content-start (point)))
        (insert (propertize content 'face 'shadow))
        (hermes-chat--fontify-diff-ranges content-start content)
        (goto-char (+ content-start (length content))))
      (insert "\n"))))

(defun hermes-chat--compact-commentary-paragraph (paragraph)
  "Return PARAGRAPH with token-stream line noise collapsed."
  (replace-regexp-in-string
   "[ \t]+" " "
   (string-trim
    (replace-regexp-in-string "[ \t]*\n[ \t]*" " " paragraph))))

(defun hermes-chat--commentary-normalize-line-endings (content)
  "Return CONTENT with escaped newline artifacts normalized."
  (let ((text (hermes-chat--sanitize-content content)))
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
    (insert (if expanded "▾ Thinking..." "▸ Thinking..."))
    (make-text-button start (point)
                      'face 'shadow
                      'mouse-face 'highlight
                      'follow-link t
                      'help-echo "Toggle Hermes thinking"
                      'hermes-chat-entry-id (plist-get entry :id)
                      'action #'hermes-chat--toggle-thinking-button))
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

(defun hermes-chat--insert-entry-content (content)
  "Insert assistant or system CONTENT."
  (let ((content-start (point)))
    (insert content)
    (hermes-chat--fontify-diff-ranges content-start content)
    (goto-char (+ content-start (length content)))
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
     ((memq role hermes-chat--transient-entry-roles)
      (hermes-chat--insert-transient-content entry))
     ((not (string-empty-p content))
      (hermes-chat--insert-entry-content content)))))

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
Do not record these internal text-property changes in the undo list."
  (when-let* ((pos (hermes-chat--input-position)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (remove-text-properties (point-min) (point-max)
                              '(read-only nil front-sticky nil rear-nonsticky nil))
      (add-text-properties (point-min) pos
                           '(read-only t front-sticky t rear-nonsticky t)))))

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

(defun hermes-chat--setup-buffer ()
  "Initialize the current buffer as an empty Hermes chat buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (setq-local header-line-format '(:eval (hermes-chat--header-line)))
    (hermes-chat--reset-header-state)
    (setq hermes-chat--nodes (make-hash-table :test 'equal)
          hermes-chat--pending-assistant-id nil
          hermes-chat--queued-message nil
          hermes-chat--pending-prompts (make-hash-table :test #'equal)
          hermes-chat--draining-queued-message-p nil
          hermes-chat--transport-generation 0
          hermes-chat--process nil
          hermes-chat--dashboard-client nil
          hermes-chat--dashboard-session-ready-p nil
          hermes-chat--dashboard-active-session-id nil
          hermes-chat--dashboard-detached-assistant-id nil
          hermes-chat--dashboard-stream-assistant-id nil
          hermes-chat--dashboard-suppress-stream-p nil
          hermes-chat--ansi-fragments (make-hash-table :test #'equal)
          hermes-chat--session-id nil
          hermes-chat--ewoc (ewoc-create #'hermes-chat--print-entry
                                         nil "\n> " 'nosep))
    (goto-char (point-max))
    ;; Keep the marker at the beginning of the editable input tail.  With an
    ;; insertion-type marker, normal typing moves the marker after the inserted
    ;; text, making the input appear empty to `hermes-chat-input-string'.
    (setq hermes-chat--input-marker (copy-marker (point) nil))
    (hermes-chat--protect-transcript)
    (goto-char hermes-chat--input-marker)))

(defun hermes-chat--register-node (entry node)
  "Register ENTRY's ID for NODE and return NODE."
  (when-let* ((id (plist-get entry :id)))
    (puthash id node hermes-chat--nodes))
  node)

(defun hermes-chat--insert-entry (entry)
  "Insert ENTRY into the current chat EWOC and return its node."
  (let ((node (hermes-chat--preserve-input-point
               (let ((node (let ((inhibit-read-only t)
				 (buffer-undo-list t))
                             (ewoc-enter-last hermes-chat--ewoc entry))))
                 (hermes-chat--register-node entry node)))))
    (hermes-chat--notify-state-change)
    node))

(defun hermes-chat--entries ()
  "Return chat entries from the current buffer in display order."
  (ewoc-collect hermes-chat--ewoc #'identity))

(defun hermes-chat--update-entry (id function)
  "Update entry ID by applying FUNCTION to its entry plist."
  (let ((node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (unless node
      (user-error "No Hermes chat entry with id %s" id))
    (let ((entry (hermes-chat--preserve-input-point
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t)
                        (entry (funcall function (ewoc-data node))))
                    (ewoc-set-data node entry)
                    (ewoc-invalidate hermes-chat--ewoc node)
                    entry))))
      (hermes-chat--notify-state-change)
      entry)))

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

(defun hermes-chat--toggle-thinking-button (button)
  "Toggle the thinking entry attached to BUTTON."
  (when-let* ((id (button-get button 'hermes-chat-entry-id)))
    (hermes-chat--toggle-entry-expanded id)))

(defun hermes-chat--assistant-ansi-key (assistant-id)
  "Return ANSI-fragment key for ASSISTANT-ID stream chunks."
  (list 'assistant assistant-id))

(defun hermes-chat--append-assistant-content (assistant-id content status)
  "Append CONTENT to ASSISTANT-ID and set STATUS."
  (let ((ansi-key (hermes-chat--assistant-ansi-key assistant-id)))
    (unless (eq status 'streaming)
      (hermes-chat--clear-ansi-fragment ansi-key))
    (hermes-chat--update-entry
     assistant-id
     (lambda (entry)
       (let ((text (concat (or (plist-get entry :content) "")
                           (hermes-chat--sanitize-content
                            content (and (eq status 'streaming) ansi-key)))))
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

(defun hermes-chat--updated-transport-content (entry event content)
  "Return updated display CONTENT for ENTRY from transport EVENT."
  (let ((clean-content (hermes-chat--sanitize-content content)))
    (if (and entry (hermes-chat--commentary-delta-p event))
        (concat (or (plist-get entry :content) "") clean-content)
      clean-content)))

(defun hermes-chat--upsert-transport-entry (assistant-id event)
  "Insert or update a compact transport EVENT for ASSISTANT-ID."
  (let* ((role (hermes-chat--transport-entry-role event))
         (event-id (hermes-chat--transport-entry-id event))
         (id (and event-id (format "%s:%s" assistant-id event-id)))
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
         (hermes-chat--make-entry role content status id metadata))))))

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

(defun hermes-chat--closed-status-event-p (event)
  "Return non-nil when EVENT reports a closed live transport."
  (and (eq (plist-get event :type) 'status)
       (equal (hermes-chat--status-name (plist-get event :status))
              "closed")))

(defun hermes-chat--closed-status-error-event (event)
  "Return an error event corresponding to transport close EVENT."
  (list :type 'error
        :event (or (plist-get event :event) "dashboard.closed")
        :content (or (hermes-chat--transport-entry-content event)
                     "Hermes dashboard WebSocket closed")))

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
      (when-let* ((type (hermes-chat--prompt-event-type event)))
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
                         prompt))))
        (puthash key stored table)
        stored)
    event))

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

(defun hermes-chat--event-session-id (event)
  "Return EVENT's dashboard session id, or nil."
  (hermes-chat--event-string event '(:session-id :session_id)))

(defun hermes-chat--clear-terminal-prompts (event)
  "Remove pending prompt requests associated with terminal EVENT."
  (hermes-chat--clear-pending-prompts
   (hermes-chat--event-session-id event)))

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

(defun hermes-chat--dashboard-connection-label ()
  "Return a compact dashboard connection label for the current chat."
  (cond
   ((hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    "connected")
   ((and hermes-chat--dashboard-client (hermes-chat--active-turn-p))
    "connecting")
   (hermes-chat--dashboard-client "disconnected")
   (hermes-chat--process "transport active")
   (t nil)))

(defun hermes-chat--dashboard-snapshot ()
  "Return display-safe dashboard state for the current chat buffer."
  (list :buffer (current-buffer)
        :title (buffer-name)
        :session-id hermes-chat--session-id
        :connection (hermes-chat--dashboard-connection-label)
        :status (or (plist-get hermes-chat--status-state :status) 'ready)
        :activity (plist-get hermes-chat--status-state :activity)
        :active-tools (hermes-chat--active-tool-summaries)
        :pending-prompts (hermes-chat--pending-prompt-count)
        :pending-assistant-p (and hermes-chat--pending-assistant-id t)
        :updated (plist-get hermes-chat--status-state :updated)))

(defun hermes-chat--forget-live-dashboard-session ()
  "Forget the live dashboard session while preserving the durable session key."
  (setq hermes-chat--dashboard-session-ready-p nil
        hermes-chat--dashboard-active-session-id nil))

(defun hermes-chat--stop-dashboard-client ()
  "Release this buffer's dashboard client and live-session state.
The buffer-local client and session state are always cleared, even after a
partial or failed teardown, so a new session can be started afterwards."
  (when-let* ((client hermes-chat--dashboard-client))
    (hermes-dashboard-transport-stop client "Hermes dashboard transport stopped")
    (when (eq hermes-chat--process client)
      (setq hermes-chat--process nil))
    (setq hermes-chat--dashboard-client nil))
  (hermes-chat--forget-live-dashboard-session))

(defun hermes-chat--cleanup-buffer ()
  "Release per-buffer Hermes chat resources before killing the buffer."
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--notify-state-change))

(defun hermes-chat--next-transport-generation ()
  "Advance and return this buffer's transport callback generation."
  (cl-incf hermes-chat--transport-generation))

(defun hermes-chat--current-transport-generation-p (generation)
  "Return non-nil when GENERATION is the current transport generation."
  (= generation hermes-chat--transport-generation))

(defun hermes-chat--dashboard-terminal-event-p (event)
  "Return non-nil when EVENT should settle a suppressed dashboard stream."
  (or (memq (plist-get event :type) '(done error))
      (hermes-chat--closed-status-event-p event)))

(defun hermes-chat--dashboard-suppressed-content-event-p (event)
  "Return non-nil when suppressed EVENT must not update reply text."
  (and hermes-chat--dashboard-suppress-stream-p
       (memq (plist-get event :type) '(done error))))

(defun hermes-chat--dashboard-suppressed-terminal-status (event)
  "Return assistant status for suppressed dashboard terminal EVENT."
  (if (eq (plist-get event :type) 'error)
      (hermes-chat--error-status event)
    'done))

(defun hermes-chat--dashboard-suppressed-header-event (event)
  "Return a safe header event for suppressed dashboard terminal EVENT."
  (if (eq (plist-get event :type) 'error)
      '(:type error
              :content "Hermes session ended; prompt was not submitted")
    '(:type done)))

(defun hermes-chat--dashboard-control-error-event-p (event)
  "Return non-nil when EVENT is an error from a control RPC."
  (and (eq (plist-get event :type) 'error)
       (when-let* ((method (hermes-chat--event-string event '(:method))))
         (not (member method '("prompt.submit" "session.create"
                               "session.resume"))))))

(defun hermes-chat--dashboard-event-assistant-id (fallback-id event)
  "Return assistant id that should receive dashboard EVENT.
FALLBACK-ID is the assistant id captured by the transport callback."
  (cond
   (hermes-chat--dashboard-stream-assistant-id)
   (hermes-chat--dashboard-suppress-stream-p
    (and (hermes-chat--dashboard-terminal-event-p event) fallback-id))
   (t fallback-id)))

(defun hermes-chat--dashboard-finish-assistant (assistant-id)
  "Clear dashboard stream bookkeeping for ASSISTANT-ID when applicable."
  (when (equal hermes-chat--dashboard-stream-assistant-id assistant-id)
    (setq hermes-chat--dashboard-stream-assistant-id nil))
  (when (and hermes-chat--dashboard-suppress-stream-p
             (equal hermes-chat--pending-assistant-id assistant-id))
    (setq hermes-chat--dashboard-suppress-stream-p nil))
  (when (equal hermes-chat--dashboard-detached-assistant-id assistant-id)
    (setq hermes-chat--dashboard-detached-assistant-id nil)))

(defun hermes-chat--handle-suppressed-dashboard-terminal-event
    (assistant-id event)
  "Settle suppressed dashboard terminal EVENT for ASSISTANT-ID.
The event belongs to a resumed in-flight turn without a local assistant entry,
so do not copy its final content into the unsubmitted retry placeholder."
  (let ((status (hermes-chat--dashboard-suppressed-terminal-status event)))
    (hermes-chat--clear-terminal-prompts event)
    (hermes-chat--update-header-for-event
     (hermes-chat--dashboard-suppressed-header-event event))
    (hermes-chat--mark-assistant assistant-id status nil t)
    (hermes-chat--settle-transport-entries assistant-id status)
    (hermes-chat--dashboard-finish-assistant assistant-id)
    (setq hermes-chat--pending-assistant-id nil
          hermes-chat--process nil)
    (hermes-chat--drain-queued-message)))

(defun hermes-chat--stale-assistant-event-p (assistant-id)
  "Return non-nil when ASSISTANT-ID is older than the active pending turn."
  (and hermes-chat--pending-assistant-id
       (not (equal hermes-chat--pending-assistant-id assistant-id))))

(defun hermes-chat--handle-closed-status (assistant-id event)
  "Handle a transport closed status EVENT for ASSISTANT-ID."
  (hermes-chat--forget-live-dashboard-session)
  (hermes-chat--clear-terminal-prompts event)
  (if (equal hermes-chat--pending-assistant-id assistant-id)
      (progn
        (hermes-chat--handle-transport-event
         assistant-id (hermes-chat--closed-status-error-event event))
        (setq hermes-chat--dashboard-detached-assistant-id assistant-id
              hermes-chat--dashboard-stream-assistant-id nil
              hermes-chat--dashboard-suppress-stream-p nil))
    (progn
      (hermes-chat--update-header-for-event event)
      (hermes-chat--upsert-transport-entry assistant-id event))))

(defun hermes-chat--handle-transport-event (assistant-id event)
  "Apply transport EVENT to ASSISTANT-ID in the current chat buffer."
  (cond
   ((hermes-chat--stale-assistant-event-p assistant-id) nil)
   ((hermes-chat--closed-status-event-p event)
    (hermes-chat--handle-closed-status assistant-id event))
   (t
    (when (hermes-chat--prompt-request-event-p event)
      (setq event (hermes-chat--record-prompt-request event assistant-id)))
    (hermes-chat--update-header-for-event event)
    (pcase (plist-get event :type)
      ('delta
       (hermes-chat--append-assistant-content
        assistant-id (or (plist-get event :content) "") 'streaming))
      ('done
       (hermes-chat--clear-terminal-prompts event)
       (hermes-chat--mark-assistant
        assistant-id 'done (plist-get event :content) t)
       (hermes-chat--settle-transport-entries assistant-id 'done)
       (hermes-chat--dashboard-finish-assistant assistant-id)
       (setq hermes-chat--pending-assistant-id nil
             hermes-chat--process nil)
       (hermes-chat--drain-queued-message))
      ('error
       (let ((status (hermes-chat--error-status event)))
         (hermes-chat--clear-terminal-prompts event)
         (hermes-chat--append-assistant-content
          assistant-id
          (let ((content (or (plist-get event :content) "")))
            (if (string-empty-p content) "Transport error" content))
          status)
         (hermes-chat--settle-transport-entries assistant-id status)
         (hermes-chat--dashboard-finish-assistant assistant-id)
         (setq hermes-chat--pending-assistant-id nil
               hermes-chat--process nil)
         (hermes-chat--drain-queued-message)))
      ((or 'status 'progress 'tool 'commentary 'diff)
       (hermes-chat--upsert-transport-entry assistant-id event))
      ('unknown
       (message "%s" (hermes-chat--unknown-event-content event))
       (hermes-chat--upsert-transport-entry assistant-id event))
      (_
       (message "Unknown Hermes transport event: %S" event))))))

(defun hermes-chat--dashboard-default-transport-p ()
  "Return non-nil when chat should use the dashboard transport."
  (and hermes-chat-use-dashboard-transport
       (eq hermes-transport-send-function #'hermes-transport-send)))

(defun hermes-chat--dashboard-client-live-p (client)
  "Return non-nil when CLIENT has an open dashboard WebSocket."
  (and (hermes-dashboard-transport-client-p client)
       (hermes-dashboard-transport-client-websocket client)))

(defun hermes-chat--dashboard-cols ()
  "Return the current chat width for dashboard session requests."
  (max 20 (window-total-width)))

(defun hermes-chat--dashboard-result-string (result keys)
  "Return RESULT's first scalar string value among KEYS."
  (hermes-transport--scalar-string
   (hermes-transport--get-any result keys)))

(defun hermes-chat--dashboard-active-id-from-result (client result)
  "Return CLIENT's live dashboard session id from RPC RESULT."
  (or (hermes-chat--dashboard-result-string result '(session_id id))
      (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport-client-session-id client))))

(defun hermes-chat--dashboard-stored-id-from-result (client result active-id)
  "Return durable session key from CLIENT, RPC RESULT, and ACTIVE-ID."
  (or (hermes-chat--dashboard-result-string
       result '(stored_session_id resumed session_key))
      (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport-client-stored-session-id client))
      active-id))

(defun hermes-chat--dashboard-record-session (client result)
  "Record live and durable session identifiers from CLIENT RPC RESULT."
  (when-let* ((active-id
               (hermes-chat--dashboard-active-id-from-result client result)))
    (let ((stored-id
           (hermes-chat--dashboard-stored-id-from-result client result active-id)))
      (setq hermes-chat--dashboard-active-session-id active-id
            hermes-chat--session-id stored-id
            hermes-chat--dashboard-session-ready-p t)
      (when (hermes-dashboard-transport-client-p client)
        (setf (hermes-dashboard-transport-client-session-id client) active-id
              (hermes-dashboard-transport-client-stored-session-id client)
              stored-id)))))

(defun hermes-chat--dashboard-result-live-turn-p (result)
  "Return non-nil when RESULT reports the resumed session is still busy."
  (or (hermes-transport--get result 'running)
      (hermes-transport--get result 'inflight)))

(defun hermes-chat--dashboard-mark-unsubmitted-retry (assistant-id)
  "Mark ASSISTANT-ID as an unsubmitted retry placeholder."
  (hermes-chat--mark-assistant
   assistant-id 'error
   "Hermes session is still running; prompt was not submitted." t)
  (hermes-chat--settle-transport-entries assistant-id 'error))

(defun hermes-chat--dashboard-insert-inflight-assistant ()
  "Insert and return an assistant entry for a resumed in-flight turn."
  (let* ((entry (hermes-chat--make-entry 'assistant "" 'streaming))
         (assistant-id (plist-get entry :id)))
    (hermes-chat--insert-entry entry)
    assistant-id))

(defun hermes-chat--dashboard-bind-stream-callback (client assistant-id)
  "Bind CLIENT events to ASSISTANT-ID in the current buffer."
  (when (and (hermes-dashboard-transport-client-p client) assistant-id)
    (setf (hermes-dashboard-transport-client-callback client)
          (hermes-chat--transport-callback
           (current-buffer) assistant-id t
           (hermes-chat--next-transport-generation)))))

(defun hermes-chat--dashboard-restore-inflight-turn (client)
  "Restore local busy state for CLIENT's resumed in-flight turn."
  (let* ((retry-id hermes-chat--pending-assistant-id)
         (stream-id (or hermes-chat--dashboard-detached-assistant-id
                        (and hermes-chat--dashboard-stream-assistant-id
                             (not (equal hermes-chat--dashboard-stream-assistant-id
                                         retry-id))
                             hermes-chat--dashboard-stream-assistant-id))))
    (cond
     (stream-id
      (when (and retry-id (not (equal retry-id stream-id)))
        (hermes-chat--dashboard-mark-unsubmitted-retry retry-id))
      (hermes-chat--clear-active-tools)
      (hermes-chat--mark-assistant stream-id 'streaming "" t)
      (setq hermes-chat--pending-assistant-id stream-id
            hermes-chat--process client
            hermes-chat--dashboard-stream-assistant-id stream-id
            hermes-chat--dashboard-suppress-stream-p nil)
      (hermes-chat--handle-transport-event
       stream-id
       '(:type status
               :status-key "session.resume"
               :status "running"
               :content "Hermes session is still running; reattached")))
     (retry-id
      (hermes-chat--clear-active-tools)
      (hermes-chat--mark-assistant
       retry-id 'streaming
       "Hermes session is still running; prompt was not submitted." t)
      (setq hermes-chat--pending-assistant-id retry-id
            hermes-chat--process client
            hermes-chat--dashboard-stream-assistant-id nil
            hermes-chat--dashboard-suppress-stream-p t)
      (hermes-chat--set-header-state
       :status 'running
       :activity "Hermes session is still running"
       :assistant-id retry-id))
     (t
      (let ((assistant-id (hermes-chat--dashboard-insert-inflight-assistant)))
        (hermes-chat--clear-active-tools)
        (setq hermes-chat--pending-assistant-id assistant-id
              hermes-chat--process client
              hermes-chat--dashboard-stream-assistant-id assistant-id
              hermes-chat--dashboard-suppress-stream-p nil)
        (hermes-chat--handle-transport-event
         assistant-id
         '(:type status
		 :status-key "session.resume"
		 :status "running"
		 :content "Hermes session is still running; reattached")))))))

(defun hermes-chat--dashboard-start (callback)
  "Return a dashboard client whose events are sent to CALLBACK."
  (if (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
      (progn
        (setf (hermes-dashboard-transport-client-callback
               hermes-chat--dashboard-client)
              callback)
        hermes-chat--dashboard-client)
    (hermes-chat--stop-dashboard-client)
    (setq hermes-chat--dashboard-session-ready-p nil
          hermes-chat--dashboard-active-session-id nil
          hermes-chat--dashboard-client
          (hermes-dashboard-transport-start :callback callback))))

(defun hermes-chat--dashboard-submit-prompt (client prompt)
  "Submit PROMPT to CLIENT's active dashboard session."
  (unless hermes-chat--dashboard-active-session-id
    (user-error "Hermes dashboard did not return a live session id"))
  (hermes-dashboard-transport-prompt-submit
   client prompt :session-id hermes-chat--dashboard-active-session-id))

(defun hermes-chat--dashboard-after-session
    (client prompt result &optional resume-p)
  "Record CLIENT session RESULT and submit PROMPT.
When RESUME-P is non-nil and RESULT reports a live turn, keep local busy
state instead of submitting another prompt into that durable session."
  (hermes-chat--dashboard-record-session client result)
  (if (and resume-p (hermes-chat--dashboard-result-live-turn-p result))
      (hermes-chat--dashboard-restore-inflight-turn client)
    (setq hermes-chat--dashboard-detached-assistant-id nil)
    (hermes-chat--dashboard-submit-prompt client prompt)))

(defun hermes-chat--dashboard-session-resolver (buffer client prompt &optional resume-p)
  "Return a callback that records CLIENT's session in BUFFER and sends PROMPT.
RESUME-P means the callback handles a `session.resume' response."
  (lambda (result)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (hermes-chat--dashboard-after-session
         client prompt result resume-p)))))

(defun hermes-chat--dashboard-session-attached-p ()
  "Return non-nil when the current buffer has a live dashboard session."
  (and hermes-chat--dashboard-session-ready-p
       hermes-chat--dashboard-active-session-id))

(defun hermes-chat--dashboard-ensure-session (client prompt buffer)
  "Create or resume CLIENT's dashboard session before submitting PROMPT.
Record asynchronous session results in BUFFER."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (hermes-chat--dashboard-submit-prompt client prompt))
   (hermes-chat--session-id
    (hermes-dashboard-transport-session-resume
     client hermes-chat--session-id
     :cols (hermes-chat--dashboard-cols)
     :resolve (hermes-chat--dashboard-session-resolver
               buffer client prompt t)))
   (t
    (hermes-dashboard-transport-session-create
     client
     :cols (hermes-chat--dashboard-cols)
     :title hermes-chat-dashboard-session-title
     :profile hermes-chat--profile
     :resolve (hermes-chat--dashboard-session-resolver buffer client prompt)))))

(defun hermes-chat--dashboard-event-for-session-p (event)
  "Return non-nil when EVENT belongs to this buffer's live dashboard session."
  (let ((session-id (plist-get event :session-id)))
    (or (null session-id)
        (and hermes-chat--dashboard-active-session-id
             (equal session-id hermes-chat--dashboard-active-session-id)))))

(defun hermes-chat--dashboard-send (prompt callback)
  "Send PROMPT through the dashboard transport and stream to CALLBACK."
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-start callback)))
    (hermes-chat--dashboard-ensure-session client prompt buffer)
    client))

(defun hermes-chat--send-prompt (prompt callback)
  "Send PROMPT to Hermes and stream transport events to CALLBACK."
  (if (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--dashboard-send prompt callback)
    (funcall hermes-transport-send-function prompt callback)))

(defun hermes-chat--active-turn-p ()
  "Return non-nil when this chat buffer has an active Hermes turn."
  hermes-chat--pending-assistant-id)

(defun hermes-chat--busy-message ()
  "Return the user-facing busy/backpressure message."
  (concat "A Hermes reply is still pending; use C-c C-i to interrupt, "
          "C-c C-q to queue, C-c C-s to steer, C-c C-k to "
          "interrupt+send, "
          (and (hermes-chat--pending-prompt-p)
               "C-c C-a to answer the prompt, C-c C-d to cancel it, ")
          "or C-c C-n for a new session"))

(defun hermes-chat--trimmed-input ()
  "Return the current input tail trimmed for sending."
  (string-trim (hermes-chat-input-string)))

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

(defun hermes-chat--preview (content)
  "Return a compact preview for CONTENT."
  (truncate-string-to-width (string-replace "\n" " " content) 80 nil nil "…"))

(defun hermes-chat--insert-local-status (content &optional status)
  "Insert local status CONTENT with optional STATUS."
  (hermes-chat--insert-entry
   (hermes-chat--make-entry 'status content (or status 'done))))

(defun hermes-chat--queue-content (content &optional note)
  "Queue CONTENT for the next turn, inserting NOTE when non-nil."
  (when hermes-chat--queued-message
    (user-error "A Hermes message is already queued"))
  (setq hermes-chat--queued-message content)
  (hermes-chat--insert-local-status
   (or note (format "Queued next message: %s" (hermes-chat--preview content)))
   'queued)
  (hermes-chat--set-header-state
   :status 'queued :activity "Queued next message"))

(defun hermes-chat--preserve-control-content (content)
  "Keep busy-control CONTENT recoverable after a dashboard bootstrap error."
  (when-let* ((text (hermes-chat--nonempty-string content)))
    (if (string-empty-p (string-trim (hermes-chat-input-string)))
        (hermes-chat--replace-input-tail text)
      (if (not hermes-chat--queued-message)
          (hermes-chat--queue-content
           text "Preserved busy-control text after dashboard error")
        (hermes-chat--append-input-tail text)
        (hermes-chat--insert-local-status
         "Restored busy-control text in input tail after dashboard error"
         'error)))))

(defun hermes-chat--dashboard-bootstrap-error (message &optional content)
  "Render dashboard session bootstrap MESSAGE and preserve CONTENT."
  (hermes-chat--command-error (format "Dashboard session failed: %s" message))
  (hermes-chat--preserve-control-content content))

(defun hermes-chat--call-with-dashboard-bootstrap-error (content thunk)
  "Call THUNK, preserving CONTENT if dashboard bootstrap signals."
  (condition-case err
      (funcall thunk)
    (error
     (hermes-chat--dashboard-bootstrap-error (error-message-string err)
                                             content))))

(defun hermes-chat--dashboard-control-client ()
  "Return a dashboard client for control RPCs without replacing live callbacks."
  (cond
   ((hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    hermes-chat--dashboard-client)
   ((hermes-chat--dashboard-default-transport-p)
    (hermes-chat--stop-dashboard-client)
    (setq hermes-chat--dashboard-session-ready-p nil
          hermes-chat--dashboard-active-session-id nil
          hermes-chat--dashboard-client
          (hermes-dashboard-transport-start :callback #'ignore)))
   (t
    (user-error "Hermes dashboard transport controls are unavailable"))))

(defun hermes-chat--dashboard-action-resolver (buffer client action)
  "Return a resolver to record CLIENT's session in BUFFER, then call ACTION."
  (lambda (result)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (hermes-chat--dashboard-record-session client result)
        (when (hermes-chat--dashboard-result-live-turn-p result)
          (hermes-chat--dashboard-restore-inflight-turn client)
          (hermes-chat--dashboard-bind-stream-callback
           client hermes-chat--pending-assistant-id))
        (funcall action client)))))

(defun hermes-chat--dashboard-action-rejecter (buffer reject)
  "Return a reject callback to run REJECT visibly in BUFFER."
  (lambda (message)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if reject
            (funcall reject message)
          (hermes-chat--command-error message))))))

(defun hermes-chat--dashboard-ensure-session-action
    (client buffer action &optional reject)
  "Ensure CLIENT has a session in BUFFER, then call ACTION with CLIENT.
When dashboard session bootstrap fails, call REJECT with the error message."
  (cond
   ((hermes-chat--dashboard-session-attached-p)
    (funcall action client))
   (hermes-chat--session-id
    (hermes-dashboard-transport-session-resume
     client hermes-chat--session-id
     :cols (hermes-chat--dashboard-cols)
     :resolve (hermes-chat--dashboard-action-resolver buffer client action)
     :reject (hermes-chat--dashboard-action-rejecter buffer reject)))
   (t
    (hermes-dashboard-transport-session-create
     client
     :cols (hermes-chat--dashboard-cols)
     :title hermes-chat-dashboard-session-title
     :resolve (hermes-chat--dashboard-action-resolver buffer client action)
     :reject (hermes-chat--dashboard-action-rejecter buffer reject)))))

(defun hermes-chat--dashboard-stored-session-needs-resume-p ()
  "Return non-nil when a durable dashboard session may be active remotely."
  (and (hermes-chat--dashboard-default-transport-p)
       hermes-chat--session-id
       (not (hermes-chat--dashboard-session-attached-p))
       (not (hermes-chat--active-turn-p))))

(defun hermes-chat--queue-or-submit-content (content)
  "Queue CONTENT during an active turn, otherwise submit it now."
  (if (hermes-chat--active-turn-p)
      (hermes-chat--queue-content content)
    (hermes-chat--submit-content content)))

(defun hermes-chat--dashboard-queue-or-submit (content buffer)
  "Resume stored dashboard session in BUFFER before queuing or submitting CONTENT."
  (if (hermes-chat--dashboard-stored-session-needs-resume-p)
      (hermes-chat--call-with-dashboard-bootstrap-error
       content
       (lambda ()
         (let ((client (hermes-chat--dashboard-control-client)))
           (hermes-chat--dashboard-ensure-session-action
            client buffer
            (lambda (_live-client)
              (hermes-chat--queue-or-submit-content content))
            (lambda (message)
              (hermes-chat--dashboard-bootstrap-error message content))))))
    (hermes-chat--queue-or-submit-content content)))

(defun hermes-chat--result-string (result key)
  "Return RESULT's scalar value for KEY as a string."
  (hermes-transport--scalar-string (hermes-transport--get result key)))

(defun hermes-chat--result-type (result)
  "Return command RESULT's lower-case type string."
  (when-let* ((type (hermes-chat--result-string result 'type)))
    (downcase type)))

(defun hermes-chat--result-output (result)
  "Return display output from command RESULT."
  (let ((warning (hermes-chat--nonempty-string
                  (hermes-chat--result-string result 'warning)))
        (body (cl-some
               (lambda (key)
                 (hermes-chat--nonempty-string
                  (hermes-chat--result-string result key)))
               '(output notice message target))))
    (cond
     ((and warning body) (format "warning: %s\n%s" warning body))
     (body)
     (warning (format "warning: %s" warning)))))

(defun hermes-chat--alias-content (target arg)
  "Return slash content for alias TARGET with original ARG."
  (when-let* ((command (hermes-chat--nonempty-string
			(string-trim (or target "")))))
    (string-join
     (delq nil (list (concat "/" (string-remove-prefix "/" command))
                     (hermes-chat--nonempty-string arg)))
     " ")))

(defun hermes-chat--handle-alias-result (target arg)
  "Follow command-dispatch alias TARGET with original ARG."
  (if-let* ((content (hermes-chat--alias-content target arg))
            (parsed (hermes-chat--parse-slash content)))
      (pcase-let ((`(,name . ,next-arg) parsed))
        (hermes-chat--dashboard-slash-exec name next-arg (substring content 1)))
    (user-error "Command alias target missing")))

(defun hermes-chat--handle-send-result (message &optional notice)
  "Handle command-dispatch MESSAGE with optional NOTICE."
  (when (hermes-chat--nonempty-string notice)
    (hermes-chat--insert-local-status notice 'done))
  (cond
   ((not (hermes-chat--nonempty-string message))
    (user-error "Command returned no message to send"))
   ((hermes-chat--active-turn-p)
    (hermes-chat--queue-content message))
   (t
    (hermes-chat--dashboard-queue-or-submit message (current-buffer)))))

(defun hermes-chat--handle-skill-result (message name)
  "Handle command-dispatch skill MESSAGE named NAME."
  (let ((skill-name (or (hermes-chat--nonempty-string name) "skill")))
    (hermes-chat--insert-local-status
     (format "⚡ loading skill: %s" skill-name) 'done))
  (hermes-chat--handle-send-result message))

(defun hermes-chat--prefill-input (message)
  "Replace the input tail with MESSAGE."
  (hermes-chat--delete-input-tail)
  (insert (or message "")))

(defun hermes-chat--handle-prefill-result (message notice)
  "Handle command-dispatch prefill MESSAGE with optional NOTICE."
  (when (hermes-chat--nonempty-string notice)
    (hermes-chat--insert-local-status notice 'done))
  (hermes-chat--prefill-input message))

(defun hermes-chat--handle-command-result (result &optional arg)
  "Render or act on a dashboard command RESULT using optional ARG."
  (pcase (hermes-chat--result-type result)
    ("alias"
     (hermes-chat--handle-alias-result
      (hermes-chat--result-string result 'target) arg))
    ("send"
     (hermes-chat--handle-send-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'notice)))
    ("skill"
     (hermes-chat--handle-skill-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'name)))
    ("prefill"
     (hermes-chat--handle-prefill-result
      (hermes-chat--result-string result 'message)
      (hermes-chat--result-string result 'notice)))
    (_
     (when-let* ((output (hermes-chat--result-output result)))
       (hermes-chat--insert-local-status output 'done)))))

(defun hermes-chat--command-error (message)
  "Render dashboard command error MESSAGE."
  (hermes-chat--insert-local-status message 'error)
  (hermes-chat--set-header-state :status 'error :activity message))

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

(defun hermes-chat--prompt-display-name (prompt)
  "Return display name for PROMPT."
  (pcase (hermes-chat--prompt-event-type prompt)
    ("approval" "Approval")
    ("clarify" "Clarify")
    ("sudo" "Sudo")
    ("secret" "Secret")
    (_ "Prompt")))

(defun hermes-chat--first-pending-prompt ()
  "Return the first pending prompt in deterministic key order."
  (when-let* ((key (car (hermes-chat--pending-prompt-keys))))
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
    (let ((variants (list response)))
      (when (fboundp 'json-encode-string)
        (let ((encoded (json-encode-string response)))
          (push encoded variants)
          (when (and (> (length encoded) 1)
                     (eq (aref encoded 0) ?\")
                     (eq (aref encoded (1- (length encoded))) ?\"))
            (push (substring encoded 1 -1) variants))))
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
  (when-let* ((choices (hermes-chat--event-value prompt '(:choices))))
    (delq nil (mapcar #'hermes-chat--scalar-string
                      (if (vectorp choices) (append choices nil) choices)))))

(defun hermes-chat--read-prompt-response (prompt)
  "Read a response for PROMPT using an Emacs-native minibuffer UI."
  (pcase (hermes-chat--prompt-event-type prompt)
    ("approval"
     (completing-read "Approval decision: "
                      '("once" "session" "always" "deny") nil t nil nil
                      "once"))
    ("clarify"
     (if-let* ((choices (hermes-chat--prompt-choices prompt)))
         (completing-read "Clarify: " choices nil t)
       (read-string (or (hermes-chat--event-string prompt '(:question :content))
                        "Clarify: "))))
    ("sudo" (read-passwd "Sudo password: "))
    ("secret"
     (read-passwd (or (hermes-chat--event-string prompt '(:prompt :content))
                      "Secret: ")))
    (_ (read-string "Prompt response: "))))

(defun hermes-chat--approval-response-resolved-count (result)
  "Return positive resolved approval count from RESULT, or nil."
  (let ((resolved (hermes-transport--get result 'resolved)))
    (and (integerp resolved) (> resolved 0) resolved)))

(defun hermes-chat--prompt-response-complete (key prompt canceled all
                                                  &optional result)
  "Mark prompt KEY/PROMPT complete, noting CANCELED, ALL, and RESULT."
  (let* ((approval-p (hermes-chat--approval-prompt-p prompt))
         (current (and (hash-table-p hermes-chat--pending-prompts)
                       (gethash key hermes-chat--pending-prompts)))
         (queue (and approval-p
                     (or (plist-get current :prompt-queue)
                         (plist-get prompt :prompt-queue))))
         (resolved-count (and approval-p
                              (hermes-chat--approval-response-resolved-count
                               result)))
         (remaining (and queue
                         (nthcdr (or resolved-count
                                     (if all (length queue) 1))
                                 queue)))
         next-prompt)
    (when (hash-table-p hermes-chat--pending-prompts)
      (if remaining
          (let ((next (hermes-chat--approval-prompt-with-queue remaining)))
            (setq next-prompt next)
            (puthash key next hermes-chat--pending-prompts)
            (hermes-chat--upsert-transport-entry
             (or (plist-get next :assistant-id)
                 (plist-get prompt :assistant-id))
             next))
        (remhash key hermes-chat--pending-prompts)))
    (let ((message (format "%s %s"
                           (hermes-chat--prompt-display-name prompt)
                           (if canceled "canceled" "response sent"))))
      (hermes-chat--insert-local-status message (if canceled 'error 'done))
      (unless (hermes-chat--show-pending-prompt-state next-prompt)
        (hermes-chat--set-header-state
         :status (if (hermes-chat--active-turn-p) 'running 'ready)
         :activity message)))))

(defun hermes-chat--approval-response-unresolved-p (prompt result)
  "Return non-nil when approval PROMPT RESULT resolved no backend prompt."
  (and (hermes-chat--approval-prompt-p prompt)
       (equal (hermes-transport--get result 'resolved) 0)))

(defun hermes-chat--prompt-response-stale (key prompt)
  "Clear stale prompt KEY/PROMPT without claiming a response was sent."
  (when (hash-table-p hermes-chat--pending-prompts)
    (remhash key hermes-chat--pending-prompts))
  (let ((message (format "%s request no longer pending"
                         (hermes-chat--prompt-display-name prompt))))
    (hermes-chat--insert-local-status message 'error)
    (unless (hermes-chat--show-pending-prompt-state)
      (hermes-chat--set-header-state
       :status (if (hermes-chat--active-turn-p) 'running 'ready)
       :activity message))))

(defun hermes-chat--prompt-missing-error-p (message)
  "Return non-nil when MESSAGE reports that the backend prompt is gone."
  (and (stringp message)
       (string-match-p "\\bno pending\\b" (downcase message))))

(defun hermes-chat--prompt-response-rejected (key prompt response message)
  "Render rejection MESSAGE for prompt KEY, PROMPT, and RESPONSE."
  (when (and (hash-table-p hermes-chat--pending-prompts)
             (hermes-chat--prompt-missing-error-p message))
    (remhash key hermes-chat--pending-prompts))
  (hermes-chat--command-error
   (hermes-chat--prompt-safe-error prompt response message)))

(defun hermes-chat--prompt-success-callback (buffer key prompt canceled all)
  "Return a success callback for prompt response KEY in BUFFER."
  (lambda (result)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (hermes-chat--approval-response-unresolved-p prompt result)
            (hermes-chat--prompt-response-stale key prompt)
          (hermes-chat--prompt-response-complete
           key prompt canceled all result))))))

(defun hermes-chat--prompt-reject-callback (buffer key prompt response)
  "Return an error callback for prompt KEY, PROMPT, and RESPONSE in BUFFER."
  (lambda (message)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (hermes-chat--prompt-response-rejected key prompt response message)))))

(defun hermes-chat--approval-session-id (prompt)
  "Return the dashboard session id for approval PROMPT."
  (or (hermes-chat--event-string prompt '(:session-id :session_id))
      hermes-chat--dashboard-active-session-id))

(defun hermes-chat--request-prompt-id (key prompt)
  "Return request id for prompt KEY/PROMPT."
  (or (hermes-chat--event-string prompt '(:request-id :request_id)) key))

(defun hermes-chat--send-prompt-response (key prompt response all canceled)
  "Send RESPONSE for prompt KEY/PROMPT through the dashboard transport."
  (let ((client (hermes-chat--dashboard-control-client))
        (buffer (current-buffer))
        (type (hermes-chat--prompt-event-type prompt)))
    (condition-case err
        (pcase type
          ("approval"
           (hermes-dashboard-transport-approval-respond
            client :session-id (hermes-chat--approval-session-id prompt)
            :choice response :all (and all t)
            :resolve (hermes-chat--prompt-success-callback
                      buffer key prompt canceled all)
            :reject (hermes-chat--prompt-reject-callback
                     buffer key prompt response)))
          ((or "clarify" "sudo" "secret")
           (funcall (pcase type
                      ("clarify" #'hermes-dashboard-transport-clarify-respond)
                      ("sudo" #'hermes-dashboard-transport-sudo-respond)
                      ("secret" #'hermes-dashboard-transport-secret-respond))
                    client (hermes-chat--request-prompt-id key prompt) response
                    (hermes-chat--prompt-success-callback
                     buffer key prompt canceled all)
                    (hermes-chat--prompt-reject-callback
                     buffer key prompt response)))
          (_ (user-error "Unsupported Hermes prompt type: %s" type)))
      (error
       (hermes-chat--prompt-response-rejected
        key prompt response (error-message-string err))))))

(defun hermes-chat-respond-to-prompt (&optional key response all)
  "Respond to pending prompt KEY with RESPONSE.
When called interactively, select the prompt and read RESPONSE in the
minibuffer.  With prefix argument ALL, approval responses apply to all pending
approvals in the dashboard session."
  (interactive (list nil nil current-prefix-arg))
  (let* ((prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key))
         (answer (or response (hermes-chat--read-prompt-response prompt))))
    (hermes-chat--send-prompt-response prompt-key prompt answer all nil)))

(defun hermes-chat-cancel-prompt (&optional key)
  "Cancel pending prompt KEY by sending the protocol's safe empty/deny value."
  (interactive)
  (let* ((prompt-key (hermes-chat--select-pending-prompt-key key))
         (prompt (hermes-chat--pending-prompt prompt-key))
         (response (if (equal (hermes-chat--prompt-event-type prompt) "approval")
                       "deny"
                     "")))
    (hermes-chat--send-prompt-response prompt-key prompt response nil t)))

(defun hermes-chat--listify (value)
  "Return VALUE as a list when it is a list or vector."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)))

(defun hermes-chat--pair-command (pair)
  "Return command name from catalog PAIR."
  (cond
   ((vectorp pair) (and (> (length pair) 0) (aref pair 0)))
   ((consp pair) (car pair))))

(defun hermes-chat--pair-description (pair)
  "Return command description from catalog PAIR."
  (cond
   ((vectorp pair) (and (> (length pair) 1) (aref pair 1)))
   ((consp pair) (cadr pair))))

(defun hermes-chat--format-command-pair (pair)
  "Return a readable catalog line for PAIR."
  (let ((name (hermes-chat--scalar-string (hermes-chat--pair-command pair)))
        (desc (hermes-chat--scalar-string (hermes-chat--pair-description pair))))
    (string-join (delq nil (list name desc)) " — ")))

(defun hermes-chat--format-command-category (category)
  "Return readable command catalog text for CATEGORY."
  (let* ((name (or (hermes-chat--result-string category 'name) "Commands"))
         (pairs (hermes-chat--listify (hermes-transport--get category 'pairs)))
         (lines (mapcar #'hermes-chat--format-command-pair pairs)))
    (string-join (cons name (mapcar (lambda (line) (concat "  " line)) lines))
                 "\n")))

(defun hermes-chat--commands-categories-content (result)
  "Return readable command categories from catalog RESULT."
  (let ((categories (hermes-chat--listify
                     (hermes-transport--get result 'categories))))
    (if categories
        (string-join (mapcar #'hermes-chat--format-command-category categories)
                     "\n\n")
      (hermes-chat--format-command-category
       `((name . "Commands") (pairs . ,(hermes-transport--get result 'pairs)))))))

(defun hermes-chat--maplike-entries (value)
  "Return VALUE's entries when VALUE is an alist or hash table."
  (cond
   ((hash-table-p value)
    (let (entries)
      (maphash (lambda (key item) (push (cons key item) entries)) value)
      (nreverse entries)))
   ((listp value) value)))

(defun hermes-chat--subcommand-name (value)
  "Return VALUE as a slash command name without leading slash."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (string-remove-prefix "/" name)))

(defun hermes-chat--format-subcommand-entry (entry)
  "Return readable catalog line for subcommand ENTRY."
  (let* ((command (hermes-chat--subcommand-name (car-safe entry)))
         (items (hermes-chat--listify (cdr-safe entry)))
         (subs (delq nil
                     (mapcar (lambda (item)
                               (when-let* ((sub (hermes-chat--scalar-string item)))
                                 (format "/%s %s" command sub)))
                             items))))
    (when (and (hermes-chat--nonempty-string command) subs)
      (concat "  " (string-join subs ", ")))))

(defun hermes-chat--commands-subcommands-content (result)
  "Return readable subcommand catalog section from RESULT."
  (let* ((sub (hermes-transport--get result 'sub))
         (entries (hermes-chat--maplike-entries sub))
         (lines (delq nil
                      (mapcar #'hermes-chat--format-subcommand-entry entries))))
    (when lines
      (string-join (cons "Subcommands" lines) "\n"))))

(defun hermes-chat--commands-catalog-content (result)
  "Return readable command catalog RESULT content."
  (let ((warning (hermes-chat--result-string result 'warning)))
    (string-join
     (delq nil
           (list (and (hermes-chat--nonempty-string warning)
                      (format "Warning: %s" warning))
                 (hermes-chat--commands-categories-content result)
                 (hermes-chat--commands-subcommands-content result)))
     "\n\n")))

(defun hermes-chat--parse-slash (content)
  "Return (NAME . ARG) when CONTENT is a slash command."
  (when (string-prefix-p "/" content)
    (let* ((rest (substring content 1))
           (space (string-match-p "[ \t\n]" rest)))
      (if space
          (cons (downcase (substring rest 0 space))
                (string-trim-left (substring rest space)))
        (cons (downcase rest) "")))))

(defun hermes-chat-input-string ()
  "Return the current input tail as a plain string."
  (let ((pos (hermes-chat--input-position)))
    (unless pos
      (user-error "No Hermes chat input marker in this buffer"))
    (buffer-substring-no-properties pos (point-max))))

(defun hermes-chat-newline ()
  "Insert a literal newline in the Hermes chat input tail."
  (interactive)
  (unless (hermes-chat--point-in-input-p)
    (goto-char (hermes-chat--input-position)))
  (insert "\n"))

(defun hermes-chat--drain-queued-message ()
  "Submit one queued message after the active turn settles."
  (when (and hermes-chat--queued-message
             (not hermes-chat--pending-assistant-id)
             (not hermes-chat--draining-queued-message-p))
    (let ((content hermes-chat--queued-message))
      (setq hermes-chat--queued-message nil
            hermes-chat--draining-queued-message-p t)
      (unwind-protect
          (hermes-chat--submit-content content)
        (setq hermes-chat--draining-queued-message-p nil)))))

(defun hermes-chat--submit-content (content)
  "Submit CONTENT as a new user turn."
  (when (hermes-chat--active-turn-p)
    (user-error "%s" (hermes-chat--busy-message)))
  (let* ((user-entry (hermes-chat--make-entry 'user content 'done))
         (assistant-entry (hermes-chat--make-entry 'assistant "" 'pending))
         (assistant-id (plist-get assistant-entry :id))
         (buffer (current-buffer))
         (dashboard-p (hermes-chat--dashboard-default-transport-p))
         (generation (hermes-chat--next-transport-generation)))
    (hermes-chat--insert-entry user-entry)
    (hermes-chat--insert-entry assistant-entry)
    (hermes-chat--clear-active-tools)
    (hermes-chat--set-header-state
     :status 'pending :activity "Waiting for Hermes"
     :assistant-id assistant-id :last-tool nil :started (current-time))
    (setq hermes-chat--pending-assistant-id assistant-id
          hermes-chat--dashboard-stream-assistant-id (and dashboard-p assistant-id)
          hermes-chat--dashboard-suppress-stream-p nil)
    (condition-case err
        (setq hermes-chat--process
              (hermes-chat--send-prompt
               content
               (hermes-chat--transport-callback
                buffer assistant-id dashboard-p generation)))
      (error
       (hermes-chat--handle-transport-event
        assistant-id (list :type 'error :content (error-message-string err)))
       (message "Hermes transport failed: %s" (error-message-string err))))))

(defun hermes-chat--transport-callback
    (buffer assistant-id dashboard-p generation)
  "Return transport callback for BUFFER, ASSISTANT-ID, DASHBOARD-P, and GENERATION."
  (lambda (event)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (hermes-chat--current-transport-generation-p generation)
                   (or (not dashboard-p)
                       (and (not (hermes-chat--dashboard-control-error-event-p
                                  event))
                            (hermes-chat--dashboard-event-for-session-p event))))
          (when-let* ((target-id (if dashboard-p
                                     (hermes-chat--dashboard-event-assistant-id
                                      assistant-id event)
                                   assistant-id)))
            (if (and dashboard-p
                     (hermes-chat--dashboard-suppressed-content-event-p
                      event))
                (hermes-chat--handle-suppressed-dashboard-terminal-event
                 target-id event)
              (hermes-chat--handle-transport-event target-id event))))))))

(defun hermes-chat--dashboard-dispatch-command (name arg &optional preserve-content)
  "Dispatch dashboard command NAME with ARG and render its result.
PRESERVE-CONTENT is restored if session bootstrap fails before dispatch."
  (let ((buffer (current-buffer))
        (raw (or preserve-content (hermes-chat--alias-content name arg))))
    (hermes-chat--call-with-dashboard-bootstrap-error
     raw
     (lambda ()
       (let ((client (hermes-chat--dashboard-control-client)))
         (hermes-chat--dashboard-ensure-session-action
          client buffer
          (lambda (live-client)
            (hermes-dashboard-transport-command-dispatch
             live-client name arg
             :session-id hermes-chat--dashboard-active-session-id
             :resolve (lambda (result)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (hermes-chat--handle-command-result result arg))))
             :reject (lambda (message)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (hermes-chat--command-error message))))))
          (lambda (message)
            (hermes-chat--dashboard-bootstrap-error message raw))))))))

(defun hermes-chat--dashboard-slash-exec (name arg raw)
  "Run RAW slash command, falling back to command dispatch for NAME/ARG."
  (let ((buffer (current-buffer))
        (preserve-content (concat "/" raw)))
    (hermes-chat--call-with-dashboard-bootstrap-error
     preserve-content
     (lambda ()
       (let ((client (hermes-chat--dashboard-control-client)))
         (hermes-chat--dashboard-ensure-session-action
          client buffer
          (lambda (live-client)
            (hermes-dashboard-transport-slash-exec
             live-client raw
             :session-id hermes-chat--dashboard-active-session-id
             :resolve (lambda (result)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (hermes-chat--handle-command-result result arg))))
             :reject (lambda (_message)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (hermes-chat--dashboard-dispatch-command
                            name arg preserve-content))))))
          (lambda (message)
            (hermes-chat--dashboard-bootstrap-error
             message preserve-content))))))))

(defun hermes-chat-show-commands ()
  "Fetch and display the dashboard slash command catalog."
  (interactive)
  (let ((buffer (current-buffer))
        (client (hermes-chat--dashboard-control-client)))
    (hermes-dashboard-transport-commands-catalog
     client
     :resolve (lambda (result)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (hermes-chat--insert-local-status
                     (hermes-chat--commands-catalog-content result) 'done))))
     :reject (lambda (message)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--command-error message)))))))

(defun hermes-chat--handle-slash-content (content)
  "Handle slash command CONTENT from the input tail."
  (pcase-let ((`(,name . ,arg) (hermes-chat--parse-slash content)))
    (hermes-chat--delete-input-tail)
    (pcase name
      ("commands" (hermes-chat-show-commands))
      ((or "queue" "q") (hermes-chat--dashboard-dispatch-command name arg))
      ("steer" (hermes-chat-steer-message arg))
      (_ (hermes-chat--dashboard-slash-exec name arg (substring content 1))))))

(defun hermes-chat-queue-message (&optional message)
  "Queue MESSAGE to send after the active Hermes turn, or send now if idle."
  (interactive)
  (let ((content (string-trim (or message (hermes-chat-input-string)))))
    (when (string-empty-p content)
      (user-error "No Hermes input to queue"))
    (unless message
      (hermes-chat--delete-input-tail))
    (hermes-chat--dashboard-queue-or-submit content (current-buffer))))

(defun hermes-chat--steer-rejected (content message)
  "Handle rejected steer CONTENT with fallback MESSAGE."
  (hermes-chat--insert-local-status
   (format "Steer unavailable (%s); queued next message" message) 'error)
  (hermes-chat--queue-content content "Queued next message after steer fallback"))

(defun hermes-chat--steer-active-turn (content buffer)
  "Steer active dashboard turn with CONTENT in BUFFER, or queue when unsupported."
  (if (not (hermes-chat--dashboard-session-attached-p))
      (hermes-chat--queue-content content "Steer unavailable; queued next message")
    (hermes-dashboard-transport-session-steer
     hermes-chat--dashboard-client content
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (result)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (if (equal (hermes-chat--status-name
                                (hermes-chat--result-string result 'status))
                               "rejected")
                        (hermes-chat--steer-rejected content "rejected")
                      (hermes-chat--insert-local-status
                       (format "Steer queued: %s"
                               (hermes-chat--preview content))
                       'queued)))))
     :reject (lambda (err)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--steer-rejected content err)))))))

(defun hermes-chat--steer-or-submit (content buffer)
  "Steer active turn with CONTENT in BUFFER, or submit CONTENT when idle."
  (if (hermes-chat--active-turn-p)
      (hermes-chat--steer-active-turn content buffer)
    (hermes-chat--submit-content content)))

(defun hermes-chat--dashboard-steer-or-submit (content buffer)
  "Resume stored dashboard session in BUFFER before steering or submitting CONTENT."
  (if (hermes-chat--dashboard-stored-session-needs-resume-p)
      (hermes-chat--call-with-dashboard-bootstrap-error
       content
       (lambda ()
         (let ((client (hermes-chat--dashboard-control-client)))
           (hermes-chat--dashboard-ensure-session-action
            client buffer
            (lambda (_live-client)
              (hermes-chat--steer-or-submit content buffer))
            (lambda (message)
              (hermes-chat--dashboard-bootstrap-error message content))))))
    (hermes-chat--steer-or-submit content buffer)))

(defun hermes-chat-steer-message (&optional message)
  "Steer the active dashboard run with MESSAGE, falling back to queue."
  (interactive)
  (let ((content (string-trim (or message (hermes-chat-input-string))))
        (buffer (current-buffer)))
    (when (string-empty-p content)
      (user-error "No Hermes input to steer"))
    (unless message
      (hermes-chat--delete-input-tail))
    (hermes-chat--dashboard-steer-or-submit content buffer)))

(defun hermes-chat-interrupt ()
  "Request interruption of the active dashboard run."
  (interactive)
  (unless (hermes-chat--active-turn-p)
    (user-error "No active Hermes run to interrupt"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support interrupt"))
  (let ((buffer (current-buffer)))
    (hermes-dashboard-transport-session-interrupt
     hermes-chat--dashboard-client
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (_result)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (hermes-chat--insert-local-status
                     "Interrupt requested" 'interrupted)
                    (hermes-chat--set-header-state
                     :status 'interrupted :activity "Interrupt requested"))))
     :reject (lambda (message)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--command-error message)))))))

(defun hermes-chat-interrupt-and-send (&optional message)
  "Queue MESSAGE from the input tail and interrupt the active run."
  (interactive)
  (unless (hermes-chat--active-turn-p)
    (user-error "No active Hermes run to interrupt"))
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "Current Hermes transport does not support interrupt"))
  (hermes-chat-queue-message message)
  (hermes-chat-interrupt))

(defun hermes-chat-disconnect ()
  "End this chat's dashboard session so a new one can be started.
Tears down the live client when present (best effort, even when it is stale
or in an error state) and clears the live session state.  The durable
session key is preserved, so the conversation can still be resumed."
  (interactive)
  (unless (or hermes-chat--dashboard-client
              hermes-chat--process
              hermes-chat--dashboard-active-session-id)
    (user-error "This Hermes chat has no session to disconnect"))
  (hermes-chat--stop-dashboard-client)
  (hermes-chat--insert-local-status "Session disconnected" 'disconnected)
  (hermes-chat--set-header-state :status 'disconnected :activity "Disconnected"))

(defun hermes-chat--model-id (model)
  "Return the model id string from a `model.options' MODEL entry."
  (or (hermes-transport--scalar-string model)
      (hermes-transport--scalar-string (hermes-transport--get model 'id))))

(defun hermes-chat--model-candidates (payload)
  "Return de-duplicated selectable model ids from `model.options' PAYLOAD.
Models from authenticated providers are listed first."
  (let (authed other)
    (dolist (row (hermes-transport--get payload 'providers))
      (let ((authenticated (hermes-transport--get row 'authenticated)))
        (dolist (model (hermes-transport--get row 'models))
          (when-let* ((id (hermes-chat--model-id model)))
            (if authenticated (push id authed) (push id other))))))
    (delete-dups (append (nreverse authed) (nreverse other)))))

(defun hermes-chat--apply-model (buffer client model confirm)
  "Set MODEL on BUFFER's session via CLIENT, passing expensive-model CONFIRM."
  (with-current-buffer buffer
    (hermes-dashboard-transport-config-set
     client "model" model
     :session-id hermes-chat--dashboard-active-session-id
     :confirm-expensive-model confirm
     :resolve (lambda (result)
                (hermes-chat--model-set-result buffer client model result))
     :reject (lambda (message)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--command-error message)))))))

(defun hermes-chat--model-set-result (buffer client model result)
  "Report MODEL switch RESULT for BUFFER, re-confirming through CLIENT when asked."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (hermes-transport--get result 'confirm_required)
          (if (yes-or-no-p
               (or (hermes-transport--scalar-string
                    (hermes-transport--get result 'confirm_message))
                   "Confirm switching to this model? "))
              (hermes-chat--apply-model buffer client model t)
            (hermes-chat--insert-local-status "Model switch cancelled" 'ready))
        (hermes-chat--insert-local-status
         (format "Model set to %s" model) 'ready)))))

(defun hermes-chat--prompt-and-set-model (buffer client result)
  "Prompt for a model from RESULT and apply it to BUFFER's session via CLIENT."
  (when (buffer-live-p buffer)
    (let ((candidates (hermes-chat--model-candidates result))
          (current (hermes-transport--scalar-string
                    (hermes-transport--get result 'model))))
      (if (null candidates)
          (message "Hermes: no models available to switch to")
        (let ((choice (completing-read
                       (format "Switch model (current %s): " (or current "?"))
                       candidates nil t)))
          (unless (string-empty-p choice)
            (hermes-chat--apply-model buffer client choice nil)))))))

(defun hermes-chat-switch-model ()
  "Switch the model used by the current Hermes chat session."
  (interactive)
  (unless (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (user-error "Connect this chat (send a message) before switching models"))
  (when (hermes-chat--active-turn-p)
    (user-error "Interrupt the active turn before switching models"))
  (let ((buffer (current-buffer))
        (client hermes-chat--dashboard-client))
    (hermes-dashboard-transport-model-options
     client
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (result)
                (hermes-chat--prompt-and-set-model buffer client result))
     :reject (lambda (message)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--command-error message)))))))

(defun hermes-chat-new-session ()
  "Open a new Hermes chat buffer with a fresh dashboard session."
  (interactive)
  (let ((buffer (generate-new-buffer hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (hermes-chat-mode))
    (pop-to-buffer-same-window buffer)
    (goto-char (or (hermes-chat--input-position) (point-max)))
    buffer))

(defun hermes-chat-new-profile-session (profile)
  "Open a new Hermes chat buffer for a dashboard session under PROFILE.
A blank PROFILE keeps the dashboard's default profile.  The profile applies
when the session is created on the first interaction."
  (interactive (list (read-string "Profile (blank for default): ")))
  (let ((buffer (hermes-chat-new-session)))
    (with-current-buffer buffer
      (setq hermes-chat--profile
            (and (not (string-empty-p profile)) profile)))
    buffer))

(defun hermes-chat-resume-session (session-id &optional title)
  "Open a Hermes chat buffer that resumes dashboard SESSION-ID.
TITLE, when given, names the buffer.  Resume happens on the next interaction,
like any chat buffer holding a durable session key."
  (interactive (list (read-string "Resume Hermes session id: ")))
  (when (or (null session-id) (string-empty-p session-id))
    (user-error "No Hermes session id to resume"))
  (let ((buffer (generate-new-buffer
                 (if (and title (not (string-empty-p title)))
                     (format "*Hermes: %s*" title)
                   hermes-chat-buffer-name))))
    (with-current-buffer buffer
      (hermes-chat-mode)
      (setq hermes-chat--session-id session-id))
    (pop-to-buffer-same-window buffer)
    (goto-char (or (hermes-chat--input-position) (point-max)))
    buffer))

(defun hermes-chat-send ()
  "Send the current Hermes chat input."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (unless (hermes-chat--point-in-input-p)
    (user-error "Point is not in the Hermes chat input area"))
  (let ((content (hermes-chat--trimmed-input)))
    (when (string-empty-p content)
      (user-error "No Hermes input to send"))
    (if (hermes-chat--parse-slash content)
        (hermes-chat--handle-slash-content content)
      (when (hermes-chat--active-turn-p)
        (user-error "%s" (hermes-chat--busy-message)))
      (hermes-chat--delete-input-tail)
      (hermes-chat--submit-content content))))

;;; Attachments view

(defun hermes-chat--text-urls (text)
  "Return the URLs found in TEXT, in order of appearance."
  (let ((case-fold-search t) (start 0) urls)
    (while (and text (string-match goto-address-url-regexp text start))
      (push (match-string 0 text) urls)
      (setq start (match-end 0)))
    (nreverse urls)))

(defun hermes-chat--collect-urls (entries)
  "Return ordered, de-duplicated URLs from ENTRIES' content."
  (seq-uniq
   (mapcan (lambda (entry) (hermes-chat--text-urls (plist-get entry :content)))
           entries)))

(defvar-local hermes-chat-attachments--source nil
  "Chat buffer whose links populate this attachments buffer.")

(defun hermes-chat-attachments--follow (button)
  "Open BUTTON's URL in a browser."
  (browse-url (button-label button)))

(defun hermes-chat--attachments-revert (&rest _)
  "Re-collect links from the source chat buffer."
  (let ((source hermes-chat-attachments--source))
    (unless (buffer-live-p source)
      (user-error "Source chat buffer is gone"))
    (hermes-chat--render-attachments
     (with-current-buffer source
       (hermes-chat--collect-urls (hermes-chat--entries)))
     source)))

(define-derived-mode hermes-chat-attachments-mode special-mode "Hermes Attachments"
  "Major mode listing links collected from a Hermes chat transcript."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-chat--attachments-revert))

(defun hermes-chat--render-attachments (urls source)
  "Render URLS gathered from the SOURCE chat buffer, returning the buffer."
  (with-current-buffer (get-buffer-create "*Hermes Attachments*")
    (hermes-chat-attachments-mode)
    (setq hermes-chat-attachments--source source)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Links from %s\n\n" (buffer-name source)))
      (if (null urls)
          (insert "No links found.\n")
        (dolist (url urls)
          (insert-text-button url
                              'action #'hermes-chat-attachments--follow
                              'help-echo "Open link in browser"
                              'follow-link t)
          (insert "\n"))))
    (goto-char (point-min))
    (current-buffer)))

(defun hermes-chat-view-attachments ()
  "Display a buffer listing every link from the current chat transcript."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Not in a Hermes chat buffer"))
  (pop-to-buffer
   (hermes-chat--render-attachments
    (hermes-chat--collect-urls (hermes-chat--entries))
    (current-buffer))))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline
  "C-c C-i" #'hermes-chat-interrupt
  "C-c C-k" #'hermes-chat-interrupt-and-send
  "C-c C-q" #'hermes-chat-queue-message
  "C-c C-s" #'hermes-chat-steer-message
  "C-c C-a" #'hermes-chat-respond-to-prompt
  "C-c C-d" #'hermes-chat-cancel-prompt
  "C-c C-/" #'hermes-chat-show-commands
  "C-c C-l" #'hermes-chat-view-attachments
  "C-c C-n" #'hermes-chat-new-session)

(define-derived-mode hermes-chat-mode fundamental-mode "Hermes Chat"
  "Major mode for Hermes chat buffers."
  :keymap hermes-chat-mode-map
  :interactive nil
  (visual-line-mode 1)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 5)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode 0))
  (add-hook 'kill-buffer-hook #'hermes-chat--cleanup-buffer nil t)
  (hermes-chat--setup-buffer))

;;;###autoload
(defun hermes-chat ()
  "Open the Hermes chat buffer."
  (interactive)
  (let ((buffer (get-buffer-create hermes-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hermes-chat-mode)
        (hermes-chat-mode)))
    (pop-to-buffer-same-window buffer)
    (goto-char (or (hermes-chat--input-position) (point-max)))))

(provide 'hermes-chat)
;;; hermes-chat.el ends here
