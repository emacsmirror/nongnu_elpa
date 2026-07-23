;;; hermes-chat-format.el --- Pure formatting helpers for Hermes chat  -*- lexical-binding: t; -*-

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

;; Pure, side-effect-free formatting and sanitizing helpers shared by the
;; Hermes chat buffer.  These take plain values (strings, plists, transport
;; events) and return display strings, faces, or icons without touching buffer,
;; process, or header state.  `hermes-chat' requires this file and keeps the
;; stateful rendering, EWOC, and transport code.

;;; Code:

(require 'diff-mode)
(require 'markdown-mode)
(require 'subr-x)
(require 'hermes-transport)

(defun hermes-chat--displayable-char-p (char)
  "Return non-nil if CHAR is safe to display in chat content."
  (or (memq char '(?\t ?\n))
      (and (>= char ?\s)
           (/= char ?\177)
           (not (and (>= char #x80)
                     (<= char #x9f))))))

(defun hermes-chat--strip-ansi-escape-sequences (content &optional fragment)
  "Return (TEXT . FRAGMENT) for CONTENT after stripping ANSI escapes.
FRAGMENT is a partial escape sequence carried over from the same stream, or
nil.  This is deliberately not `ansi-color-filter-apply': besides SGR/CSI
color codes it also strips OSC sequences (terminal-title and the like) that
`ansi-color' leaves in place, and it carries a trailing partial escape -- a
CSI, an OSC, or a lone ESC -- across stream chunks through FRAGMENT.
`ansi-color''s single global context models neither."
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

(defun hermes-chat--sanitize-content-with-fragment (content fragment)
  "Return (TEXT . FRAGMENT) after sanitizing CONTENT with ANSI FRAGMENT."
  (let* ((stripped (hermes-chat--strip-ansi-escape-sequences content fragment))
         (text (car stripped)))
    (unless (multibyte-string-p text)
      (setq text (decode-coding-string text 'utf-8-unix t)))
    (cons (concat (seq-filter #'hermes-chat--displayable-char-p text))
          (cdr stripped))))

(defun hermes-chat--sanitize-content (content)
  "Return sanitized CONTENT for display in chat buffers."
  (car (hermes-chat--sanitize-content-with-fragment content nil)))

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

(defun hermes-chat--scalar-string (value)
  "Return VALUE as a display string, stringifying non-scalar values.
Extends `hermes-transport--scalar-string', which returns nil for compound
values, with a `format' fallback the chat renderer relies on."
  (or (hermes-transport--scalar-string value)
      (and value (format "%s" value))))

(defun hermes-chat--event-value (event keys)
  "Return the first non-nil plist value in EVENT for KEYS."
  (cl-some (lambda (key) (cadr (plist-member event key))) keys))

(defun hermes-chat--event-string (event keys)
  "Return the first scalar plist value in EVENT for KEYS as a string."
  (hermes-chat--scalar-string (hermes-chat--event-value event keys)))

(defun hermes-chat--event-phase (event)
  "Return EVENT's trailing event-name phase, if any."
  (and-let* ((name (hermes-chat--event-string event '(:event))))
    (car (last (split-string name "\\." t)))))

(defun hermes-chat--humanize-event-name (name)
  "Return NAME as a compact human-readable event label."
  (and name
       (string-trim
	(replace-regexp-in-string "[._-]+" " " name))))

(defun hermes-chat--status-name (status)
  "Return normalized display/comparison name for STATUS."
  (and-let* ((name (hermes-chat--scalar-string status)))
    (downcase (replace-regexp-in-string "_" "-" name))))

(defconst hermes-chat--ready-statuses
  '("done" "completed" "complete" "success" "succeeded" "ready")
  "Status names that read as a successful, settled turn.")

(defconst hermes-chat--error-statuses
  '("error" "failed" "failure" "cancelled" "canceled" "interrupted")
  "Status names that read as a failed or aborted turn.")

(defconst hermes-chat--active-statuses
  '("pending" "waiting" "queued" "streaming" "started" "starting"
    "loading" "connecting" "reconnecting" "running" "busy"
    "progress" "in-progress" "preparing" "handoff" "requested"
    "approval-requested")
  "Status names denoting an unsettled transport entry.")

(defun hermes-chat--active-status-p (status)
  "Return non-nil when STATUS denotes an unsettled transport entry."
  (and (member (hermes-chat--status-name status) hermes-chat--active-statuses)
       t))

(defun hermes-chat--finished-status-p (status)
  "Return non-nil when STATUS denotes a settled transport entry."
  (and (member (hermes-chat--status-name status)
               (append hermes-chat--ready-statuses
                       hermes-chat--error-statuses
                       '("closed")))
       t))

(defun hermes-chat--status-icon (status)
  "Return compact icon for transport STATUS.
Active statuses show a neutral dot rather than the settled checkmark."
  (let ((name (hermes-chat--status-name status)))
    (cond
     ((member name hermes-chat--ready-statuses) "✓")
     ((or (member name hermes-chat--error-statuses) (equal name "closed")) "!")
     (t "·"))))

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
    ((or "handoff") "Handing off")
    ((or "started" "running" "busy" "progress" "in-progress" "preparing") "Running")
    (_ "Idle")))

(defun hermes-chat--status-face (status)
  "Return face for transport STATUS."
  (let ((name (hermes-chat--status-name status)))
    (cond
     ((member name hermes-chat--ready-statuses) 'success)
     ((member name hermes-chat--error-statuses) 'error)
     ((equal name "closed") 'warning)
     (t 'shadow))))

(defun hermes-chat--header-status-face (status)
  "Return face for STATUS in the chat header."
  (hermes-chat--status-face status))

(defun hermes-chat--error-status (event)
  "Return terminal status to display for an error-like transport EVENT."
  (or (hermes-chat--event-value event '(:status)) 'error))

(defun hermes-chat--message-start-noise-content-p (content)
  "Return non-nil when CONTENT is only message-start status boilerplate."
  (let ((content (downcase (string-trim (or content "")))))
    (or (string-empty-p content)
        (member content
                '("started" "message start: started"
                  "message.start: started" "message_start: started")))))

(defun hermes-chat--thinking-activity (content)
  "Return a header label from a `thinking.delta' CONTENT string.
CONTENT looks like \"(◔_◔) pondering...\"; keep the kawaii face, drop the
trailing dots, and title-case the verb.  Fall back to \"Thinking\" when CONTENT
carries no verb."
  (let ((text (string-trim-right (or content "") "[.…[:space:]]+")))
    (if (string-match-p "[A-Za-z]" text)
        (replace-regexp-in-string "[A-Za-z]+" #'capitalize text t)
      "Thinking")))

(defun hermes-chat--abbrev-tokens (n)
  "Return token count N abbreviated, e.g. 45k."
  (cond
   ((not (numberp n)) "?")
   ((>= n 1000) (format "%dk" (round (/ n 1000.0))))
   (t (number-to-string n))))

(defun hermes-chat--format-context (context)
  "Return a compact context-window string for CONTEXT, or nil.
CONTEXT is a plist of :used, :max, and :percent."
  (and-let* ((max (plist-get context :max))
             ((and (numberp max) (> max 0))))
    (format "ctx %s/%s · %d%%"
            (hermes-chat--abbrev-tokens (plist-get context :used))
            (hermes-chat--abbrev-tokens max)
            (or (plist-get context :percent) 0))))

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

(defconst hermes-chat--tool-emojis
  '(("terminal" . "💻") ("read_file" . "📖") ("write_file" . "✍️")
    ("patch" . "🔧") ("search_files" . "🔎") ("web_search" . "🔍")
    ("web_extract" . "📄") ("todo" . "📋") ("session_search" . "🔍")
    ("memory" . "🧠") ("skill_view" . "📚") ("skills_list" . "📚")
    ("skill_manage" . "📝") ("read_terminal" . "🖥️") ("send_message" . "📨")
    ("process" . "⚙️") ("browser_navigate" . "🌐") ("image_generate" . "🎨")
    ("text_to_speech" . "🔊") ("vision_analyze" . "👁️")
    ("mixture_of_agents" . "🧠") ("delegate_task" . "🧠") ("clarify" . "❓"))
  "Tool name to display emoji, mirroring the Hermes tool registry.")

(defconst hermes-chat--tool-primary-args
  '(("terminal" . command) ("web_search" . query) ("web_extract" . urls)
    ("read_file" . path) ("write_file" . path) ("patch" . path)
    ("search_files" . pattern) ("browser_navigate" . url)
    ("browser_click" . ref) ("browser_type" . text) ("image_generate" . prompt)
    ("text_to_speech" . text) ("vision_analyze" . question)
    ("mixture_of_agents" . user_prompt) ("skill_view" . name)
    ("skills_list" . category) ("cronjob" . action) ("execute_code" . code)
    ("delegate_task" . goal) ("clarify" . question) ("skill_manage" . name)
    ("session_search" . query) ("memory" . content))
  "Tool name to its primary argument key, mirroring `build_tool_preview'.")

(defconst hermes-chat--tool-detail-keys
  '(command path query pattern name skill url goal code prompt content question text)
  "Fallback argument keys to derive a tool detail string.")

(defun hermes-chat--tool-emoji (name)
  "Return the display emoji for tool NAME."
  (or (and name (cdr (assoc name hermes-chat--tool-emojis))) "⚡"))

(defun hermes-chat--first-arg-detail (args keys)
  "Return the first non-empty scalar value among KEYS in ARGS."
  (cl-some (lambda (key)
             (hermes-transport--non-empty-string
              (hermes-transport--scalar-string
               (hermes-transport--get args key))))
           keys))

(defun hermes-chat--tool-args-detail (event name)
  "Return a detail string from EVENT's args for tool NAME, or nil.
Args arrive as a structured map on `tool.complete' and as text when verbose."
  (let ((args (hermes-chat--event-value event '(:args))))
    (cond
     ((stringp args) (hermes-transport--non-empty-string args))
     ((or (consp args) (hash-table-p args))
      (hermes-chat--first-arg-detail
       args (delq nil (cons (cdr (assoc name hermes-chat--tool-primary-args))
                            hermes-chat--tool-detail-keys)))))))

(defun hermes-chat--tool-detail (event name)
  "Return the best command/path detail string for tool EVENT named NAME.
Prefers the gateway preview, then the call arguments, so the command survives
a `tool.complete' that omits the start preview."
  (or (hermes-transport--non-empty-string (hermes-chat--event-string event '(:context)))
      (hermes-chat--tool-args-detail event name)
      (hermes-transport--non-empty-string
       (hermes-chat--event-string event '(:preview :summary)))))

(defun hermes-chat--tool-head (name detail)
  "Return the emoji-prefixed head for tool NAME with optional DETAIL."
  (if detail
      (format "%s %s: %s" (hermes-chat--tool-emoji name) name detail)
    (format "%s %s" (hermes-chat--tool-emoji name) name)))

(defun hermes-chat--format-progress-event (event)
  "Return display content for a tool progress EVENT."
  (let* ((name (or (hermes-chat--tool-name event) "tool"))
         (detail (or (hermes-chat--event-string
                      event '(:content :delta :text :preview))
                     (hermes-chat--tool-detail event name)
                     (hermes-chat--event-string event '(:progress)))))
    (hermes-chat--tool-head name detail)))

(defun hermes-chat--format-tool-event (event)
  "Return display content for a tool lifecycle EVENT.
Shows the tool emoji, name, and its command/path detail, plus a duration or
error.  The entry's status icon conveys running/done/failed separately, so the
detail is kept rather than replaced by a bare \"completed\" line."
  (let* ((name (or (hermes-chat--tool-name event) "tool"))
         (head (hermes-chat--tool-head name (hermes-chat--tool-detail event name)))
         (duration (hermes-chat--format-duration
                    (hermes-chat--event-value event '(:duration))))
         (error (hermes-chat--event-string event '(:error))))
    (cond
     (error (format "%s  %s" head error))
     (duration (format "%s  %s" head duration))
     (t head))))

;;; Diff detection

(defun hermes-chat--fenced-diff-blocks ()
  "Return fenced ```diff/```patch blocks as (START END TEXT) zero-based.
START and END span the whole fenced block to replace; TEXT is its inner diff."
  (let ((case-fold-search t)
        blocks)
    (goto-char (point-min))
    (while (re-search-forward "^```[ \t]*\\(?:diff\\|patch\\)\\(?:[ \t].*\\)?\n" nil t)
      (let ((block-start (match-beginning 0))
            (inner-start (point)))
        (if (re-search-forward "^```[ \t]*$" nil t)
            (push (list (1- block-start)
                        (1- (min (point-max) (1+ (line-end-position))))
                        (buffer-substring-no-properties inner-start
                                                        (match-beginning 0)))
                  blocks)
          (push (list (1- block-start) (1- (point-max))
                      (buffer-substring-no-properties inner-start (point-max)))
                blocks))))
    blocks))

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

(defun hermes-chat--inline-diff-blocks ()
  "Return inline unified diff blocks as (START END TEXT) zero-based."
  (let (blocks)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((range (hermes-chat--unified-diff-range-at-point)))
          (progn
            (push (list (car range) (cdr range)
                        (buffer-substring-no-properties
                         (1+ (car range)) (1+ (cdr range))))
                  blocks)
            (goto-char (1+ (cdr range))))
        (forward-line 1)))
    blocks))

(defun hermes-chat--merge-diff-blocks (blocks)
  "Return BLOCKS sorted by start, dropping empty and overlapping ranges."
  (let ((sorted (sort (copy-sequence blocks)
                      (lambda (left right) (< (nth 0 left) (nth 0 right)))))
        result last-end)
    (dolist (block sorted (nreverse result))
      (when (and (< (nth 0 block) (nth 1 block))
                 (or (null last-end) (>= (nth 0 block) last-end)))
        (push block result)
        (setq last-end (nth 1 block))))))

(defun hermes-chat--diff-blocks (content)
  "Return diff blocks in CONTENT as (START END TEXT), sorted and non-overlapping.
A fenced block subsumes the inline diff it contains, so each diff yields one
block spanning the whole region to replace with a link."
  (with-temp-buffer
    (insert content)
    (hermes-chat--merge-diff-blocks
     (append (hermes-chat--fenced-diff-blocks)
             (hermes-chat--inline-diff-blocks)))))

;;; Markdown fontification

(defun hermes-chat--fontify-markdown-string (text)
  "Return TEXT fontified with `markdown-mode', or TEXT on failure.
Markup markers (* _ ` # ...) keep their faces but are never hidden, so the raw
markdown stays visible and easy to copy."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (markdown-mode))
        ;; `font-lock-mode' refuses temp buffers; `font-lock-ensure' suffices.
        (font-lock-ensure (point-min) (point-max))
        (remove-text-properties (point-min) (point-max) '(invisible nil))
        (buffer-string))
    (error text)))

;;; Value coercion helpers

;; Pure plist/alist/vector coercions shared by the chat siblings;
;; they live in this pure layer so no module needs a declare-function
;; to reach them.

(defun hermes-chat--result-string (result key)
  "Return RESULT's scalar value for KEY as a string."
  (hermes-transport--scalar-string (hermes-transport--get result key)))

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

(defun hermes-chat--maplike-entries (value)
  "Return VALUE's entries when VALUE is an alist or hash table."
  (cond
   ((hash-table-p value)
    (let (entries)
      (maphash (lambda (key item) (push (cons key item) entries)) value)
      (nreverse entries)))
   ((listp value) value)))


;;; Event classification

;; Pure event predicates and diagnostics shared by the chat siblings.

(defconst hermes-chat--unknown-event-raw-preview-width 180
  "Maximum width for raw unknown transport event previews.")

(defun hermes-chat--session-info-event-p (event)
  "Return non-nil when EVENT is a `session.info' status event."
  (equal (hermes-chat--event-string event '(:event)) "session.info"))

(defun hermes-chat--message-start-status-event-p (event)
  "Return non-nil when EVENT is low-value message-start status noise."
  (and (eq (plist-get event :type) 'status)
       (and-let* ((name (hermes-chat--event-string event '(:event))))
         (member (downcase name)
                 '("message.start" "message_start"
                   "message.started" "message_started")))
       (equal (hermes-chat--status-name (plist-get event :status)) "started")
       (hermes-chat--message-start-noise-content-p
        (hermes-chat--event-string event '(:content :text :preview)))))

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


(defun hermes-chat--model-config-value (candidate)
  "Return the `config.set' model value for CANDIDATE."
  (let ((model (if (stringp candidate)
                   candidate
                 (plist-get candidate :model)))
        (provider (and (not (stringp candidate))
                       (plist-get candidate :provider))))
    (if (and provider (not (string-empty-p provider)))
        (format "%s --provider %s" model provider)
      model)))

(provide 'hermes-chat-format)
;;; hermes-chat-format.el ends here
