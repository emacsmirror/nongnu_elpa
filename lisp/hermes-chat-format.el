;;; hermes-chat-format.el --- Pure formatting helpers for Hermes chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

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

(require 'subr-x)
(require 'hermes-transport)

;; Defined in hermes-chat.el; the ANSI-fragment hash mutators stay there while
;; `hermes-chat--sanitize-content' lives here.
(declare-function hermes-chat--ansi-fragment "hermes-chat" (key))
(declare-function hermes-chat--record-ansi-fragment "hermes-chat" (key fragment))

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
    "progress" "in-progress" "preparing" "requested" "approval-requested")
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

(defun hermes-chat--nonempty-string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value)
       (not (string-empty-p value))
       value))

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
  (when-let* ((max (plist-get context :max))
              ((and (numberp max) (> max 0))))
    (format "%s/%s ctx (%d%%)"
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
  (catch 'found
    (dolist (key keys)
      (when-let* ((value (hermes-chat--nonempty-string
                          (hermes-transport--scalar-string
                           (hermes-transport--get args key)))))
        (throw 'found value)))))

(defun hermes-chat--tool-args-detail (event name)
  "Return a detail string from EVENT's args for tool NAME, or nil.
Args arrive as a structured map on `tool.complete' and as text when verbose."
  (let ((args (hermes-chat--event-value event '(:args))))
    (cond
     ((stringp args) (hermes-chat--nonempty-string args))
     ((or (consp args) (hash-table-p args))
      (hermes-chat--first-arg-detail
       args (delq nil (cons (cdr (assoc name hermes-chat--tool-primary-args))
                            hermes-chat--tool-detail-keys)))))))

(defun hermes-chat--tool-detail (event name)
  "Return the best command/path detail string for tool EVENT named NAME.
Prefers the gateway preview, then the call arguments, so the command survives
a `tool.complete' that omits the start preview."
  (or (hermes-chat--nonempty-string (hermes-chat--event-string event '(:context)))
      (hermes-chat--tool-args-detail event name)
      (hermes-chat--nonempty-string
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

(provide 'hermes-chat-format)
;;; hermes-chat-format.el ends here
