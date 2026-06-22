;;; hermes-chat.el --- EWOC chat buffer for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; ERC/emacs-jabber-shaped chat buffer for hermes-el.  Transcript entries are
;; EWOC nodes before a read-only prompt; the input tail remains writable.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'diff-mode)
(require 'ewoc)
(require 'goto-addr)
(require 'keymap-popup)
(require 'markdown-mode)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-chat-format)

(declare-function hermes-chat--prompt-event-type "hermes-chat-prompts" (event))

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

(defface hermes-chat-separator
  '((((background light)) :strike-through "gray70" :foreground "gray70")
    (t :strike-through "gray30" :foreground "gray30"))
  "Face for the full-width rule above the Hermes chat input area."
  :group 'hermes)

(defvar hermes-chat-state-change-hook nil
  "Hook run in a Hermes chat buffer when dashboard-visible state changes.")

(defvar-local hermes-chat--ewoc nil
  "EWOC displaying chat transcript entries in the current Hermes chat buffer.")

(defvar-local hermes-chat--input-marker nil
  "Marker at the beginning of the writable chat input tail.")

(defvar-local hermes-chat--nodes nil
  "Hash table mapping Hermes entry IDs to EWOC nodes.")

;; Connection state owned by `hermes-chat-buffer'; re-declared here for the
;; byte-compiler.  See that file for the authoritative defvar-locals and docs.
(defvar hermes-chat--process)
(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-session-ready-p)
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--session-id)

(defvar-local hermes-chat--profile nil
  "Profile name for this chat's dashboard session, or nil for the default.")

(defvar-local hermes-chat--model nil
  "Model id reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--agent-name nil
  "Agent/profile name reported by the live dashboard session, for the header.")

(defvar-local hermes-chat--context nil
  "Context-window usage plist (:used :max :percent) for the header.")

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--pending-assistant-id)

;; Owned by `hermes-chat-buffer'; declared here for the byte-compiler.
(defvar hermes-chat--transport-generation)

(defvar hermes-chat--dashboard-detached-assistant-id)
(defvar hermes-chat--dashboard-stream-assistant-id)
(defvar hermes-chat--dashboard-suppress-stream-p)

(defvar-local hermes-chat--status-state nil
  "Plist describing the live status shown in the chat header.")

(defvar-local hermes-chat--title nil
  "Human title for this chat session.
Set by `hermes-chat-rename'.  Shown in the buffer name and reported to the
dashboard; nil falls back to the buffer name.")

(defvar-local hermes-chat--active-tools nil
  "Hash table of active tool summaries shown in the chat header.")

(defvar-local hermes-chat--queued-message nil
  "Plain message queued to send after the active Hermes turn settles.")

(defvar-local hermes-chat--queued-display nil
  "Compact display text for the queued message's user turn, or nil.")

(defvar-local hermes-chat--draining-queued-message-p nil
  "Non-nil while the queued message is being submitted.")

(defvar-local hermes-chat--ansi-fragments nil
  "Hash table of partial ANSI escape sequences by stream key.")

(defvar-local hermes-chat--commands-cache nil
  "Cached slash command catalog as an alist of (NAME . DESCRIPTION).")

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

(defun hermes-chat--fontify-markdown-string (text)
  "Return TEXT fontified with `markdown-mode', or TEXT on failure.
Markup markers (* _ ` # ...) keep their faces but are never hidden, so the raw
markdown stays visible and easy to copy."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (markdown-mode))
        (font-lock-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (remove-text-properties (point-min) (point-max) '(invisible nil))
        (buffer-string))
    (error text)))

(defun hermes-chat--insert-markdown (text)
  "Insert TEXT fontified as markdown when it is non-empty."
  (unless (string-empty-p text)
    (insert (hermes-chat--fontify-markdown-string text))))

(defun hermes-chat--insert-shadow (text)
  "Insert TEXT with the `shadow' face when it is non-empty."
  (unless (string-empty-p text)
    (insert (propertize text 'face 'shadow))))

(defun hermes-chat--show-diff (diff &optional buffer-name)
  "Show DIFF in a dedicated `diff-mode' buffer.
BUFFER-NAME overrides the default \"*Hermes Diff*\" buffer.  The buffer is made
read-only so `diff-mode' installs its navigation keymap (n/p hunks, q quits)
instead of `view-mode' shadowing those keys."
  (let ((buffer (get-buffer-create (or buffer-name "*Hermes Diff*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff)
        (unless (string-suffix-p "\n" diff) (insert "\n")))
      (goto-char (point-min))
      (delay-mode-hooks (diff-mode))
      (font-lock-mode 1)
      (font-lock-ensure (point-min) (point-max))
      (read-only-mode 1))
    (pop-to-buffer buffer)))

(defun hermes-chat--view-diff-button (button)
  "Open the diff stored on BUTTON in its own buffer."
  (hermes-chat--show-diff (button-get button 'hermes-chat-diff)))

(defun hermes-chat--diff-strip-prefix (path)
  "Return PATH without a leading a/ or b/ diff prefix."
  (replace-regexp-in-string "\\`[ab]/" "" path))

(defun hermes-chat--diff-header-match (regexp)
  "Return the first capture group of REGEXP in the current buffer, or nil."
  (goto-char (point-min))
  (and (re-search-forward regexp nil t) (match-string 1)))

(defun hermes-chat--diff-label (diff)
  "Return a compact target-file label for DIFF, or nil.
A standard `+++ b/path' or `diff --git' header wins; otherwise fall back to the
gateway's pre-rendered `a/path -> b/path' header."
  (with-temp-buffer
    (insert diff)
    (when-let* ((path (or (hermes-chat--diff-header-match "^\\+\\+\\+ \\(.+\\)$")
                          (hermes-chat--diff-header-match
                           "^diff --git a/.+? b/\\(.+\\)$")
                          (hermes-chat--diff-header-match "^.* → \\(.+\\)$"))))
      (hermes-chat--nonempty-string
       (hermes-chat--diff-strip-prefix (string-trim path))))))

(defun hermes-chat--insert-diff-button (diff)
  "Insert a shadow file label and a View Diff link that opens DIFF."
  (when-let* ((label (hermes-chat--diff-label diff)))
    (insert (propertize (concat label "  ") 'face 'shadow)))
  (insert-text-button
   "[View Diff]"
   'face 'link
   'mouse-face 'highlight
   'follow-link t
   'help-echo "Open this diff in a separate buffer"
   'hermes-chat-diff (string-trim diff)
   'action #'hermes-chat--view-diff-button)
  (insert "\n"))

(defun hermes-chat--insert-diff-entry (content)
  "Insert a whole-diff CONTENT (a `diff' event) as a labeled View Diff link."
  (hermes-chat--insert-diff-button content))

(defun hermes-chat--insert-diffed (content insert-text &optional blocks)
  "Insert CONTENT, replacing diff blocks with View Diff links.
INSERT-TEXT inserts each non-diff text segment (markdown or shadow text).
BLOCKS, when given, is a precomputed `hermes-chat--diff-blocks' result."
  (let ((blocks (or blocks (hermes-chat--diff-blocks content)))
        (pos 0))
    (dolist (block blocks)
      (funcall insert-text (substring content pos (nth 0 block)))
      (hermes-chat--insert-diff-button (nth 2 block))
      (setq pos (nth 1 block)))
    (funcall insert-text (substring content pos))))

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

(defun hermes-chat--active-tools-table ()
  "Return the active-tools hash for this buffer, creating it when absent.
This feeds the dashboard's per-session tool list via
`hermes-chat--dashboard-snapshot'; the chat header itself never shows tools."
  (unless (hash-table-p hermes-chat--active-tools)
    (setq hermes-chat--active-tools (make-hash-table :test 'equal)))
  hermes-chat--active-tools)

(defun hermes-chat--header-activity-for-event (event)
  "Return a compact activity string for transport EVENT."
  (hermes-chat--nonempty-string
   (or (hermes-chat--transport-entry-content event)
       (hermes-chat--event-string event '(:content :text :preview :event)))))

(defun hermes-chat--capture-session-identity (event)
  "Record the model, agent name, and context usage carried by EVENT."
  (when-let* ((model (plist-get event :model)))
    (setq hermes-chat--model model))
  (when-let* ((agent (plist-get event :agent-name)))
    (setq hermes-chat--agent-name agent))
  (when-let* ((context (plist-get event :context)))
    (setq hermes-chat--context context)))

(defun hermes-chat--status-event-activity (event)
  "Return the header activity for a status EVENT.
`session.info' carries the model/provider, now shown in their own header
fields, so it collapses to a plain ready state instead of repeating them."
  (if (hermes-chat--session-info-event-p event)
      "Ready"
    (or (hermes-chat--header-activity-for-event event) "Working")))

(defun hermes-chat--session-info-event-p (event)
  "Return non-nil when EVENT is a `session.info' status event."
  (equal (hermes-chat--event-string event '(:event)) "session.info"))

(defun hermes-chat--message-start-status-event-p (event)
  "Return non-nil when EVENT is low-value message-start status noise."
  (and (eq (plist-get event :type) 'status)
       (when-let* ((name (hermes-chat--event-string event '(:event))))
         (member (downcase name)
                 '("message.start" "message_start"
                   "message.started" "message_started")))
       (equal (hermes-chat--status-name (plist-get event :status)) "started")
       (hermes-chat--message-start-noise-content-p
        (hermes-chat--event-string event '(:content :text :preview)))))

;;; Turn-state reducer
;;
;; The header-affecting state of a turn -- the status line and the active-tool
;; set -- is computed by the pure `hermes-chat--turn-reduce'.  The reducer takes
;; the wall-clock NOW as data so it can stamp the status line itself, and returns
;; (NEW-STATE . EFFECTS) where each effect is a uniform (TYPE . PAYLOAD) tool
;; delta.  The boundary only persists NEW-STATE and replays the deltas; it makes
;; no decisions of its own.

(defun hermes-chat--turn-state (&rest kvs)
  "Return a turn-state plist built from KVS."
  kvs)

(defun hermes-chat--turn-state-get (state key)
  "Return KEY from turn-state STATE."
  (plist-get state key))

(defun hermes-chat--turn-state-put (state key value)
  "Return a copy of turn-state STATE with KEY set to VALUE."
  (plist-put (copy-sequence state) key value))

(defun hermes-chat--status-header-props (event)
  "Return the (:status :activity) header props for a status EVENT."
  (list :status (if (plist-get event :prompt-request-p)
                    (if (equal (hermes-chat--prompt-event-type event) "approval")
                        'approval-requested
                      'requested)
                  (hermes-chat--transport-entry-status event))
        :activity (hermes-chat--status-event-activity event)))

(defun hermes-chat--turn-header-props (event)
  "Return header props for any header-affecting EVENT, or nil for none."
  (pcase (plist-get event :type)
    ('status (hermes-chat--status-header-props event))
    ('commentary '(:status running :activity "Thinking..."))
    ('thinking (list :status 'thinking
                     :activity (hermes-chat--thinking-activity
                                (plist-get event :content))))
    ('diff '(:status running :activity "Reviewing diff"))
    ('done (list :status 'ready :activity "Ready"
                 :usage (plist-get event :usage)))
    ('error (list :status (hermes-chat--error-status event)
                  :activity (or (hermes-chat--event-string event '(:content :error))
                                "Transport error")))
    ('unknown (list :status 'error
                    :activity (hermes-chat--unknown-event-content event)))))

(defun hermes-chat--turn-tool-effect (event)
  "Return a (TYPE . PAYLOAD) active-tool delta for tool-like EVENT, or nil.
`tool-put' carries (KEY . SUMMARY) and `tool-remove' carries KEY.  Pure."
  (and-let* ((summary (hermes-chat--header-tool-summary event)))
    (let ((key (or (hermes-chat--header-tool-key event) summary)))
      (if (hermes-chat--finished-status-p
           (hermes-chat--transport-entry-status event))
          (cons 'tool-remove key)
        (cons 'tool-put (cons key summary))))))

(defun hermes-chat--turn-status-state (state event now)
  "Return the merged :status-state for header EVENT at NOW, given turn-state STATE."
  (apply #'hermes-chat--entry-with
         (hermes-chat--turn-state-get state :status-state)
         (append (hermes-chat--turn-header-props event)
                 (list :updated now))))

(defun hermes-chat--transcript-event-p (event)
  "Return non-nil when EVENT should render a compact transcript entry."
  (pcase (plist-get event :type)
    ('status (not (hermes-chat--session-info-event-p event)))
    ((or 'progress 'tool 'commentary 'diff 'unknown) t)))

(defun hermes-chat--turn-entry-effect (event)
  "Return an (upsert-entry . EVENT) transcript effect for EVENT, or nil.  Pure."
  (and (hermes-chat--transcript-event-p event)
       (cons 'upsert-entry event)))

(defun hermes-chat--turn-done-effects (event status)
  "Return the ordered effect list for a `done' EVENT with header STATUS.
`refresh-header' precedes the lifecycle so the header settles before `drain'
re-submits any queued turn."
  (list '(clear-tools)
        (cons 'refresh-header status)
        (cons 'clear-prompts event)
        (cons 'mark-done (plist-get event :content))
        '(drop-thinking)
        '(settle . done)
        '(finish)
        '(clear-pending)
        '(drain)))

(defun hermes-chat--turn-error-effects (event status)
  "Return the ordered effect list for an `error' EVENT with header STATUS."
  (let ((estatus (hermes-chat--error-status event))
        (content (let ((c (or (plist-get event :content) "")))
                   (if (string-empty-p c) "Transport error" c))))
    (list '(clear-tools)
          (cons 'refresh-header status)
          (cons 'clear-prompts event)
          (cons 'append-error (cons content estatus))
          (cons 'settle estatus)
          '(finish)
          '(clear-pending)
          '(drain))))

(defun hermes-chat--turn-reduce (state event now)
  "Return (NEW-STATE . EFFECTS) for domain EVENT applied to STATE at time NOW.
Pure: no buffer, EWOC, process, header, or message side effects.  EFFECTS is an
ordered list the boundary replays: a header change leads with `refresh-header',
`done'/`error' append the turn lifecycle, and tool/transcript events emit deltas
and `upsert-entry'.  Other types return (STATE)."
  (pcase (plist-get event :type)
    ((or 'status 'commentary 'thinking 'diff)
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (delq nil (list (cons 'refresh-header status)
                             (hermes-chat--turn-entry-effect event))))))
    ('unknown
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (list (cons 'refresh-header status)
                   (cons 'message (hermes-chat--unknown-event-content event))
                   (cons 'upsert-entry event)))))
    ('done
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (hermes-chat--turn-done-effects event status))))
    ('error
     (let ((status (hermes-chat--turn-status-state state event now)))
       (cons (hermes-chat--turn-state-put state :status-state status)
             (hermes-chat--turn-error-effects event status))))
    ((or 'progress 'tool)
     (cons state (delq nil (list (hermes-chat--turn-tool-effect event)
                                 (hermes-chat--turn-entry-effect event)))))
    ('delta
     (cons state (list (cons 'append-delta (or (plist-get event :content) "")))))
    (_ (cons state nil))))

(defun hermes-chat--apply-turn-effect (assistant-id effect)
  "Apply one boundary EFFECT for ASSISTANT-ID.
Header and tool effects always apply; transcript, message, and turn-lifecycle
effects apply only when ASSISTANT-ID is non-nil, so a header-only reduction
stays side-effect-light."
  (pcase effect
    (`(refresh-header . ,status)
     (setq hermes-chat--status-state status)
     (force-mode-line-update)
     (hermes-chat--notify-state-change))
    ('(clear-tools) (hermes-chat--clear-active-tools))
    (`(tool-put ,key . ,summary)
     (puthash key summary (hermes-chat--active-tools-table)))
    (`(tool-remove . ,key) (remhash key (hermes-chat--active-tools-table)))
    ((guard (null assistant-id)) nil)
    (`(upsert-entry . ,event)
     (hermes-chat--upsert-transport-entry assistant-id event))
    (`(message . ,text) (message "%s" text))
    (`(clear-prompts . ,event) (hermes-chat--clear-terminal-prompts event))
    (`(mark-done . ,content)
     (hermes-chat--mark-assistant
      assistant-id 'done
      (hermes-chat--assistant-done-content assistant-id content) t))
    (`(append-error ,content . ,status)
     (hermes-chat--append-assistant-content assistant-id content status))
    ('(drop-thinking) (hermes-chat--drop-duplicate-thinking assistant-id))
    (`(settle . ,status)
     (hermes-chat--settle-transport-entries assistant-id status))
    ('(finish) (hermes-chat--dashboard-finish-assistant assistant-id))
    ('(clear-pending)
     (setq hermes-chat--pending-assistant-id nil
           hermes-chat--process nil))
    ('(drain) (hermes-chat--drain-queued-message))
    (`(append-delta . ,content)
     (unless (hermes-chat--thinking-echo-delta-p assistant-id content)
       (hermes-chat--append-assistant-content assistant-id content 'streaming)))))

(defun hermes-chat--run-turn-reducer (assistant-id event)
  "Reduce EVENT and apply its effects in order for ASSISTANT-ID.
Captures session identity first.  The reducer puts `refresh-header' in sequence
so the boundary only replays effects, with no separate write-back."
  (hermes-chat--capture-session-identity event)
  (let ((result (hermes-chat--turn-reduce
                 (hermes-chat--turn-state :status-state hermes-chat--status-state)
                 event (current-time))))
    (dolist (effect (cdr result))
      (hermes-chat--apply-turn-effect assistant-id effect))))

(defun hermes-chat--update-header-for-event (event)
  "Update only header and tool state from transport EVENT, leaving the transcript.
Used where a synthesized header event must not insert a transcript entry."
  (hermes-chat--run-turn-reducer nil event))

(defun hermes-chat--render-turn-event (assistant-id event)
  "Render transport EVENT for ASSISTANT-ID: header, tools, and transcript entry."
  (hermes-chat--run-turn-reducer assistant-id event))

(defun hermes-chat--header-agent-name ()
  "Return the agent/profile name shown in the chat header."
  (or (hermes-chat--nonempty-string hermes-chat--agent-name)
      (hermes-chat--nonempty-string hermes-chat--profile)
      "Hermes"))

(defun hermes-chat--header-detail (label)
  "Return the live detail to append after LABEL in the header, or nil.
The activity is used, with a leading copy of LABEL stripped so a label-prefixed
activity is not shown twice.  Tool commands are deliberately not surfaced here:
the header keeps the kawaii thinking status as its only live detail, while the
transcript carries the full tool detail."
  (when-let* ((activity (hermes-chat--nonempty-string
                         (plist-get hermes-chat--status-state :activity))))
    (if (string-prefix-p (downcase label) (downcase activity))
        (hermes-chat--nonempty-string
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
        (propertize (or (hermes-chat--nonempty-string
                         (plist-get hermes-chat--status-state :activity))
                        "Thinking")
                    'face (hermes-chat--header-status-face 'running))
      (let* ((label (hermes-chat--header-status-label status))
             (detail (hermes-chat--header-detail label)))
        (propertize
         (format "%s %s" (hermes-chat--status-icon status)
                 (if detail (format "%s: %s" label detail) label))
         'face (hermes-chat--header-status-face status))))))

(defun hermes-chat--header-line ()
  "Return the chat buffer header line: agent, status, model, and context."
  (let* ((parts (delq nil
                      (list (propertize (hermes-chat--header-agent-name)
                                        'face 'mode-line-emphasis)
                            (hermes-chat--header-status-segment)
                            (hermes-chat--nonempty-string hermes-chat--model)
                            (hermes-chat--format-context hermes-chat--context))))
         (text (concat " " (string-join parts "  |  ") " "))
         (width (max 20 (window-total-width))))
    ;; Double % so the context percentage is not read as a mode-line %-spec.
    (string-replace "%" "%%" (truncate-string-to-width text width nil nil "…"))))


(require 'hermes-chat-buffer)
(require 'hermes-chat-prompts)
(require 'hermes-chat-dashboard)

(declare-function hermes-chat--cleanup-buffer "hermes-chat-dashboard" ())
(declare-function hermes-chat--dashboard-bootstrap-error "hermes-chat-dashboard" (message &optional content))
(declare-function hermes-chat--dashboard-client-live-p "hermes-chat-dashboard" (client))
(declare-function hermes-chat--dashboard-control-client "hermes-chat-dashboard" ())
(declare-function hermes-chat--dashboard-default-transport-p "hermes-chat-dashboard" ())
(declare-function hermes-chat--dashboard-ensure-session-action "hermes-chat-dashboard" (client buffer action &optional reject))
(declare-function hermes-chat--dashboard-queue-or-submit "hermes-chat-dashboard" (content buffer &optional display))
(declare-function hermes-chat--dashboard-record-session "hermes-chat-dashboard" (client result))
(declare-function hermes-chat--dashboard-session-attached-p "hermes-chat-dashboard" ())
(declare-function hermes-chat--dashboard-start "hermes-chat-dashboard" (callback))
(declare-function hermes-chat--handle-transport-event "hermes-chat-dashboard" (assistant-id event))
(declare-function hermes-chat--next-transport-generation "hermes-chat-dashboard" ())
(declare-function hermes-chat--send-prompt "hermes-chat-dashboard" (prompt callback))
(declare-function hermes-chat--transport-callback "hermes-chat-dashboard" (buffer assistant-id dashboard-p generation))

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

(defun hermes-chat--queue-content (content &optional note display)
  "Queue CONTENT for the next turn, inserting NOTE when non-nil.
DISPLAY is the compact user-turn text shown when the queued message is sent."
  (when hermes-chat--queued-message
    (user-error "A Hermes message is already queued"))
  (setq hermes-chat--queued-message content
        hermes-chat--queued-display display)
  (hermes-chat--insert-local-status
   (or note (format "Queued next message: %s"
                    (hermes-chat--preview (or display content))))
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
(defun hermes-chat--queue-or-submit-content (content &optional display)
  "Queue CONTENT during an active turn, otherwise submit it now.
DISPLAY is the compact user-turn text to show instead of CONTENT."
  (if (hermes-chat--active-turn-p)
      (hermes-chat--queue-content content nil display)
    (hermes-chat--submit-content content display)))
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
  "Send skill MESSAGE to the agent, echoing a compact loading line for NAME.
The dispatch returns the full skill payload (the agent needs it); the
transcript shows only \"loading skill: NAME\", not the whole skill."
  (unless (hermes-chat--nonempty-string message)
    (user-error "Skill returned no content to load"))
  (let ((display (format "⚡ loading skill: %s"
                         (or (hermes-chat--nonempty-string name) "skill"))))
    (hermes-chat--dashboard-queue-or-submit message (current-buffer) display)))

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

(defun hermes-chat--command-name (value)
  "Return VALUE as a bare slash command name, or nil."
  (and-let* ((name (hermes-chat--scalar-string value)))
    (hermes-chat--nonempty-string (string-remove-prefix "/" name))))

(defun hermes-chat--catalog-pairs-candidates (pairs)
  "Return (NAME . DESCRIPTION) cells for catalog PAIRS."
  (delq nil
        (mapcar
         (lambda (pair)
           (when-let* ((name (hermes-chat--command-name
                              (hermes-chat--pair-command pair))))
             (cons name (hermes-chat--scalar-string
                         (hermes-chat--pair-description pair)))))
         (hermes-chat--listify pairs))))

(defun hermes-chat--catalog-candidates (result)
  "Return an alist of (NAME . DESCRIPTION) slash commands from catalog RESULT."
  (let ((candidates
         (append
          (mapcan (lambda (category)
                    (hermes-chat--catalog-pairs-candidates
                     (hermes-transport--get category 'pairs)))
                  (hermes-chat--listify
                   (hermes-transport--get result 'categories)))
          (hermes-chat--catalog-pairs-candidates
           (hermes-transport--get result 'pairs)))))
    (cl-delete-duplicates candidates :key #'car :test #'equal :from-end t)))

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
    (let ((content hermes-chat--queued-message)
          (display hermes-chat--queued-display))
      (setq hermes-chat--queued-message nil
            hermes-chat--queued-display nil
            hermes-chat--draining-queued-message-p t)
      (unwind-protect
          (hermes-chat--submit-content content display)
        (setq hermes-chat--draining-queued-message-p nil)))))

(defun hermes-chat--submit-content (content &optional display)
  "Submit CONTENT as a new user turn, echoing DISPLAY when non-nil.
DISPLAY lets a slash skill send its full payload while showing a compact line."
  (when (hermes-chat--active-turn-p)
    (user-error "%s" (hermes-chat--busy-message)))
  (let* ((user-entry (hermes-chat--make-entry 'user (or display content) 'done))
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

(defun hermes-chat--fetch-commands-catalog ()
  "Fetch the slash command catalog into the buffer cache, when connected."
  (when (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (let ((buffer (current-buffer)))
      (hermes-dashboard-transport-commands-catalog
       hermes-chat--dashboard-client
       :resolve (lambda (result)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (setq hermes-chat--commands-cache
                            (hermes-chat--catalog-candidates result)))))))))

(defun hermes-chat--command-candidates ()
  "Return cached slash command candidates, fetching the catalog if needed."
  (unless hermes-chat--commands-cache
    (hermes-chat--fetch-commands-catalog))
  hermes-chat--commands-cache)

(defun hermes-chat-refresh-commands ()
  "Refresh the cached slash command catalog from the dashboard."
  (interactive)
  (setq hermes-chat--commands-cache nil)
  (hermes-chat--fetch-commands-catalog))

(defun hermes-chat--slash-completion-bounds ()
  "Return (START . END) of the slash command name at point, or nil.
Only matches while typing the /command word in the writable input tail."
  (let ((input (hermes-chat--input-position)))
    (and input
         (hermes-chat--point-in-input-p)
         (> (point) input)
         (eq (char-after input) ?/)
         (let ((name-start (1+ input)))
           (and (>= (point) name-start)
                (not (string-match-p
                      "[ \t\n]"
                      (buffer-substring-no-properties name-start (point))))
                (cons name-start (point)))))))

(defun hermes-chat--slash-capf ()
  "Completion-at-point for Hermes slash commands in the input tail."
  (when-let* ((bounds (hermes-chat--slash-completion-bounds))
              (candidates (hermes-chat--command-candidates)))
    (list (car bounds) (cdr bounds)
          (mapcar #'car candidates)
          :exclusive 'no
          :annotation-function
          (lambda (cand)
            (when-let* ((desc (cdr (assoc cand candidates))))
              (concat "  " desc))))))

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
      (hermes-transport--scalar-string
       (hermes-transport--get-any model '(id model name)))))

(defun hermes-chat--model-price (provider model)
  "Return a compact price string for MODEL in PROVIDER row, or nil."
  (when-let* ((prices (hermes-transport--get
                      (hermes-transport--get provider 'pricing) model)))
    (string-join
     (delq nil
           (list (hermes-transport--scalar-string
                  (hermes-transport--get prices 'input))
                 (hermes-transport--scalar-string
                  (hermes-transport--get prices 'output))))
     "/")))

(defun hermes-chat--model-capability-labels (provider model)
  "Return display labels for MODEL capabilities in PROVIDER row."
  (when-let* ((capabilities (hermes-transport--get
                            (hermes-transport--get provider 'capabilities)
                            model)))
    (delq nil
          (list (and (hermes-transport--get capabilities 'reasoning)
                     "reasoning")
                (and (hermes-transport--get capabilities 'fast) "fast")
                (when-let* ((context (hermes-transport--get-any
                                      capabilities '(context_window context))))
                  (format "%sk ctx" (/ (or (and (numberp context) context)
                                           (string-to-number
                                            (format "%s" context)))
                                       1000)))))))

(defun hermes-chat--model-provider-label (provider)
  "Return a readable, provider-identity-preserving label for PROVIDER."
  (let ((name (hermes-transport--scalar-string
               (hermes-transport--get provider 'name)))
        (slug (hermes-transport--scalar-string
               (hermes-transport--get provider 'slug))))
    (cond
     ((and name slug (not (equal name slug))) (format "%s (%s)" name slug))
     (name name)
     (slug slug)
     (t "provider"))))

(defun hermes-chat--model-label (provider model)
  "Return completion label for MODEL in PROVIDER row."
  (string-join
   (delq nil
         (append (list (hermes-chat--model-provider-label provider)
                       model
                       (hermes-chat--model-price provider model))
                 (hermes-chat--model-capability-labels provider model)))
   " · "))

(defun hermes-chat--model-candidate (provider model)
  "Return one completion candidate for MODEL in PROVIDER row."
  (when-let* ((model-id (hermes-chat--model-id model)))
    (let* ((provider-slug (hermes-transport--scalar-string
                          (hermes-transport--get provider 'slug)))
           (label (hermes-chat--model-label provider model-id)))
      (cons label (list :model model-id
                        :provider provider-slug
                        :label label
                        :authenticated (eq (hermes-transport--get
                                            provider 'authenticated)
                                           t))))))

(defun hermes-chat--model-candidates (payload)
  "Return completion candidates from `model.options' PAYLOAD.
Each candidate is (LABEL . PLIST).  Authenticated provider rows are listed
first; model ids are not de-duplicated across providers because provider
identity is part of the selection."
  (let (authed other seen)
    (dolist (provider (hermes-transport--get payload 'providers))
      (dolist (model (hermes-transport--get provider 'models))
        (when-let* ((candidate (hermes-chat--model-candidate provider model))
                    (data (cdr candidate))
                    (key (list (plist-get data :provider)
                               (plist-get data :model))))
          (unless (member key seen)
            (push key seen)
            (if (plist-get data :authenticated)
                (push candidate authed)
              (push candidate other))))))
    (append (nreverse authed) (nreverse other))))

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

(defun hermes-chat--model-display-name (candidate)
  "Return a compact display name for CANDIDATE."
  (if (stringp candidate)
      candidate
    (or (plist-get candidate :model)
        (hermes-chat--model-config-value candidate))))

(defun hermes-chat--apply-model (buffer client candidate confirm)
  "Set CANDIDATE on BUFFER's session via CLIENT.
CONFIRM acknowledges an expensive-model confirmation prompt."
  (with-current-buffer buffer
    (hermes-dashboard-transport-config-set
     client "model" (hermes-chat--model-config-value candidate)
     :session-id hermes-chat--dashboard-active-session-id
     :confirm-expensive-model confirm
     :resolve (lambda (result)
                (hermes-chat--model-set-result
                 buffer client candidate result confirm))
     :reject (lambda (message)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (hermes-chat--command-error message)))))))

(defun hermes-chat--model-set-result (buffer client candidate result confirmed)
  "Report CANDIDATE switch RESULT for BUFFER, re-confirming through CLIENT.
CONFIRMED is non-nil after the user has already accepted an expensive-model
confirmation prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (hermes-transport--get result 'confirm_required)
          (if confirmed
              (hermes-chat--command-error
               (format "Model switch still requires confirmation: %s"
                       (or (hermes-transport--scalar-string
                            (hermes-transport--get result 'confirm_message))
                           "backend repeated confirmation request")))
            (if (yes-or-no-p
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get result 'confirm_message))
                     "Confirm switching to this model? "))
                (hermes-chat--apply-model buffer client candidate t)
              (hermes-chat--insert-local-status "Model switch cancelled" 'ready)))
        (hermes-chat--insert-local-status
         (format "Model set to %s"
                 (hermes-chat--model-display-name candidate))
         'ready)))))

(defun hermes-chat--prompt-and-set-model (buffer client result)
  "Prompt for a model from RESULT and apply it to BUFFER's session via CLIENT."
  (when (buffer-live-p buffer)
    (let* ((candidates (hermes-chat--model-candidates result))
           (labels (mapcar #'car candidates))
           (current (hermes-transport--scalar-string
                     (hermes-transport--get result 'model))))
      (if (null candidates)
          (message "Hermes: no models available to switch to")
        (let* ((choice (completing-read
                        (format "Switch model (current %s): " (or current "?"))
                        labels nil t))
               (candidate (cdr (assoc choice candidates))))
          (unless (or (string-empty-p choice) (null candidate))
            (hermes-chat--apply-model buffer client candidate nil)))))))

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

(defun hermes-chat--profile-name (profile)
  "Return PROFILE's non-empty profile name, or nil."
  (and-let* ((name (hermes-transport--scalar-string
                    (hermes-transport--get profile 'name)))
             (trimmed (string-trim name))
             ((not (string-empty-p trimmed))))
    trimmed))

(defun hermes-chat--profile-default-p (profile)
  "Return non-nil when PROFILE denotes the dashboard default profile."
  (or (hermes-transport--get profile 'is_default)
      (equal (hermes-chat--profile-name profile) "default")))

(defun hermes-chat--profile-model-label (profile)
  "Return provider/model label for PROFILE, or nil."
  (let ((provider (hermes-transport--scalar-string
                   (hermes-transport--get profile 'provider)))
        (model (hermes-transport--scalar-string
                (hermes-transport--get profile 'model))))
    (cond
     ((and provider model) (format "%s/%s" provider model))
     (model model))))

(defun hermes-chat--profile-label (profile)
  "Return completion label for dashboard PROFILE metadata."
  (let ((name (hermes-chat--profile-name profile)))
    (string-join
     (delq nil
           (list name
                 (and (hermes-chat--profile-default-p profile) "default")
                 (and (hermes-transport--get profile 'has_alias) "alias")
                 (and (hermes-transport--get profile 'gateway_running)
                      "gateway")
                 (hermes-chat--profile-model-label profile)
                 (hermes-transport--scalar-string
                  (hermes-transport--get profile 'description))))
     " · ")))

(defun hermes-chat--profile-less-p (left right)
  "Return non-nil when LEFT dashboard profile should sort before RIGHT."
  (let ((left-default (hermes-chat--profile-default-p left))
        (right-default (hermes-chat--profile-default-p right)))
    (cond
     ((and left-default (not right-default)) t)
     ((and right-default (not left-default)) nil)
     (t (string-lessp (downcase (hermes-chat--profile-name left))
                      (downcase (hermes-chat--profile-name right)))))))

(defun hermes-chat--profile-candidates (payload)
  "Return sorted completion candidates from dashboard profiles PAYLOAD."
  (mapcar (lambda (profile)
            (cons (hermes-chat--profile-label profile)
                  (hermes-chat--profile-name profile)))
          (sort (cl-remove-if-not
                 #'hermes-chat--profile-name
                 (or (hermes-transport--get payload 'profiles) '()))
                #'hermes-chat--profile-less-p)))

(defun hermes-chat--existing-dashboard-client ()
  "Return a live dashboard client from any Hermes chat buffer, or nil."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'hermes-chat-mode)
                    (hermes-chat--dashboard-client-live-p
                     hermes-chat--dashboard-client)
                    hermes-chat--dashboard-client)))
           (buffer-list)))

(defun hermes-chat--profile-list-payload ()
  "Return dashboard profile-list payload using live or transient client auth."
  (let* ((existing (hermes-chat--existing-dashboard-client))
         (client existing))
    (unwind-protect
        (progn
          (unless client
            (setq client (hermes-dashboard-transport-start :callback #'ignore)))
          (hermes-dashboard-transport-profile-list client))
      (unless existing
        (when client
          (hermes-dashboard-transport-stop client))))))

(defun hermes-chat--read-raw-profile (&optional notice)
  "Read a raw Hermes profile name with the default-profile prompt.
When NOTICE is non-nil, include it in the prompt so fallback context remains
visible while reading."
  (read-string (if notice
                   (format "%s; profile (blank for default): " notice)
                 "Profile (blank for default): ")))

(defun hermes-chat--read-profile ()
  "Read a Hermes profile name, using dashboard metadata when available."
  (condition-case err
      (let* ((candidates (hermes-chat--profile-candidates
                          (hermes-chat--profile-list-payload)))
             (labels (mapcar #'car candidates)))
        (if candidates
            (let* ((choice (completing-read
                            "Profile (blank for default): " labels nil nil))
                   (profile (or (cdr (assoc choice candidates)) choice)))
              (and profile
                   (not (string-empty-p (string-trim profile)))
                   (string-trim profile)))
          (let ((notice "No dashboard profiles available"))
            (message "Hermes: %s; enter a profile name manually" notice)
            (hermes-chat--read-raw-profile notice))))
    (error
     (let ((notice (format "Profile list unavailable: %s"
                           (error-message-string err))))
       (message "Hermes: %s" notice)
       (hermes-chat--read-raw-profile notice)))))

(defun hermes-chat-new-profile-session (profile)
  "Open a new Hermes chat buffer for a dashboard session under PROFILE.
A blank PROFILE keeps the dashboard's default profile.  The profile applies
when the session is created on the first interaction."
  (interactive (list (hermes-chat--read-profile)))
  (let ((buffer (hermes-chat-new-session))
        (profile (and profile (string-trim profile))))
    (with-current-buffer buffer
      (setq hermes-chat--profile
            (and profile (not (string-empty-p profile)) profile)))
    buffer))

(defun hermes-chat--history-entry (message)
  "Return a chat entry for a resumed history MESSAGE, or nil to skip it."
  (let ((role (hermes-transport--scalar-string
               (hermes-transport--get message 'role)))
        (text (hermes-transport--scalar-string
               (hermes-transport--get message 'text))))
    (pcase role
      ("user" (and text (hermes-chat--make-entry 'user text 'done)))
      ("assistant" (and text (hermes-chat--make-entry 'assistant text 'done)))
      ("tool"
       (hermes-chat--make-entry
        'tool
        (hermes-chat--tool-head
         (or (hermes-transport--scalar-string
              (hermes-transport--get message 'name))
             "tool")
         (hermes-transport--scalar-string
          (hermes-transport--get message 'context)))
        'done)))))

(defun hermes-chat--render-history (messages)
  "Insert prior MESSAGES (from `session.resume') into the transcript."
  (dolist (message messages)
    (when-let* ((entry (hermes-chat--history-entry message)))
      (hermes-chat--insert-entry entry))))

(defun hermes-chat--load-session-history (buffer)
  "Resume BUFFER's session over the dashboard and render its prior messages."
  (with-current-buffer buffer
    (let ((client (hermes-chat--dashboard-start #'ignore)))
      (hermes-dashboard-transport-session-resume
       client hermes-chat--session-id
       :cols (hermes-chat--dashboard-cols)
       :resolve (lambda (result)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (hermes-chat--dashboard-record-session client result)
                      (hermes-chat--render-history
                       (hermes-transport--get result 'messages)))))
       :reject (lambda (message)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (hermes-chat--insert-local-status
                      (format "Could not load Hermes session history: %s" message)
                      'error))))))))

(defun hermes-chat-resume-session (session-id &optional title)
  "Open a Hermes chat buffer that resumes dashboard SESSION-ID.
TITLE, when given, names the buffer.  Over the dashboard transport the prior
messages are fetched and rendered; the durable session continues on send."
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
    (when (hermes-chat--dashboard-default-transport-p)
      (hermes-chat--load-session-history buffer))
    (with-current-buffer buffer
      (goto-char (or (hermes-chat--input-position) (point-max))))
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
      (cond
       ((and (hermes-chat--active-turn-p) hermes-chat--queued-message)
        (user-error "A Hermes message is already queued"))
       ((hermes-chat--active-turn-p)
        (hermes-chat--delete-input-tail)
        (hermes-chat--queue-content content))
       (t
        (hermes-chat--delete-input-tail)
        (hermes-chat--submit-content content))))))

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

;;; Renaming and switching chat buffers

(defun hermes-chat--buffer-name-for-title (title)
  "Return a chat buffer name derived from TITLE."
  (format "*Hermes: %s*" title))

(defun hermes-chat--push-session-title (title)
  "Push TITLE to the server with `session.title' when a session is attached.
With no live session the rename stays buffer-local; report that instead."
  (if (and (hermes-chat--dashboard-session-attached-p)
           hermes-chat--dashboard-active-session-id)
      (let ((buffer (current-buffer)))
        (hermes-dashboard-transport-session-title
         hermes-chat--dashboard-client
         :session-id hermes-chat--dashboard-active-session-id
         :title title
         :resolve (lambda (result)
                    (when (and (buffer-live-p buffer)
                               (eq (hermes-transport--get result 'pending) t))
                      (message "Title queued; applies once the session is saved")))
         :reject (lambda (message)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (hermes-chat--command-error message))))))
    (message "Renamed buffer; no live session to update on the server")))

(defun hermes-chat-rename (title)
  "Rename this chat session to TITLE.
Always rename the buffer to a TITLE-derived name; when a live dashboard session
is attached, also update the server title via `session.title' so the dashboard
and web reflect it."
  (interactive
   (list (read-string "Hermes chat title: " (or hermes-chat--title ""))))
  (let ((title (string-trim title)))
    (when (string-empty-p title)
      (user-error "Title must not be empty"))
    (setq hermes-chat--title title)
    (let ((newname (hermes-chat--buffer-name-for-title title)))
      (unless (equal (buffer-name) newname)
        (rename-buffer newname t)))
    (hermes-chat--push-session-title title)
    (force-mode-line-update)))

(defun hermes-chat--live-buffers ()
  "Return all live Hermes chat buffers in `buffer-list' order."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer (derived-mode-p 'hermes-chat-mode)))
   (buffer-list)))

(defun hermes-chat--switch-annotation (name)
  "Return a shadowed status annotation for chat buffer NAME in the switcher."
  (when-let* ((buffer (get-buffer name)))
    (with-current-buffer buffer
      (let ((detail (string-join
                     (delq nil
                           (list (hermes-chat--dashboard-connection-label)
                                 (hermes-chat--nonempty-string
                                  (plist-get hermes-chat--status-state :activity))))
                     " · ")))
        (and (not (string-empty-p detail))
             (concat "  " (propertize detail 'face 'shadow)))))))

(defun hermes-switch-to-chat (buffer)
  "Switch to a Hermes chat BUFFER chosen with completion."
  (interactive
   (let ((buffers (hermes-chat--live-buffers)))
     (unless buffers
       (user-error "No Hermes chat buffers"))
     (let ((completion-extra-properties
            (list :annotation-function #'hermes-chat--switch-annotation)))
       (list (get-buffer
              (completing-read "Hermes chat: "
                               (mapcar #'buffer-name buffers) nil t))))))
  (pop-to-buffer-same-window buffer))

(declare-function hermes-list-sessions "hermes-sessions")

(defvar hermes-chat-actions-map)

(keymap-popup-define hermes-chat-actions-map
  "In-chat action menu for `hermes-chat-mode'."
  :description "Hermes Chat Actions"
  :group "Turn"
  "s" ("Steer" hermes-chat-steer-message)
  "i" ("Interrupt" hermes-chat-interrupt)
  "k" ("Interrupt + send" hermes-chat-interrupt-and-send)
  "q" ("Queue message" hermes-chat-queue-message)
  :group "Prompt"
  "a" ("Answer prompt" hermes-chat-respond-to-prompt)
  "d" ("Cancel prompt" hermes-chat-cancel-prompt)
  :group "Session"
  "n" ("New session" hermes-chat-new-session)
  "N" ("New profile session" hermes-chat-new-profile-session)
  "m" ("Switch model" hermes-chat-switch-model)
  "R" ("Rename session" hermes-chat-rename)
  "b" ("Switch chat buffer" hermes-switch-to-chat)
  "S" ("Sessions" hermes-list-sessions)
  :group "Commands"
  "c" ("Show commands" hermes-chat-show-commands)
  "r" ("Refresh commands" hermes-chat-refresh-commands))

(defvar-keymap hermes-chat-mode-map
  :doc "Keymap for `hermes-chat-mode'."
  "RET" #'hermes-chat-send
  "C-j" #'hermes-chat-newline
  "S-<return>" #'hermes-chat-newline
  "TAB" #'completion-at-point
  "C-c C-i" #'hermes-chat-interrupt
  "C-c C-k" #'hermes-chat-interrupt-and-send
  "C-c C-q" #'hermes-chat-queue-message
  "C-c C-s" #'hermes-chat-steer-message
  "C-c C-a" #'hermes-chat-respond-to-prompt
  "C-c C-d" #'hermes-chat-cancel-prompt
  "C-c C-o" #'hermes-chat-actions-map-popup
  "C-c C-/" #'hermes-chat-show-commands
  "C-c C-l" #'hermes-chat-view-attachments
  "C-c C-n" #'hermes-chat-new-session
  "C-c C-r" #'hermes-chat-rename
  "C-c C-b" #'hermes-switch-to-chat)

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
  (add-hook 'completion-at-point-functions #'hermes-chat--slash-capf nil t)
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
