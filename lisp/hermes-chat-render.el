;;; hermes-chat-render.el --- Transcript rendering for Hermes chat  -*- lexical-binding: t; -*-

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

;; Transcript rendering effects for `hermes-chat', between the pure
;; `hermes-chat-format' helpers and the `hermes-chat-buffer' EWOC: markdown
;; and shadow insertion, diff View Diff links and the dedicated diff/
;; background result buffers, and entry-expansion metadata.  Splitting it
;; out lets `hermes-chat-buffer''s print function require these downward
;; instead of reaching into `hermes-chat' via declare-function.

;;; Code:

(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-chat-format)

(defface hermes-chat-background
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the one-line background-task result notice in the transcript.
Distinguishes a `/btw' result that arrives out of band from ordinary turns."
  :group 'hermes)

(defvar-keymap hermes-chat-background-mode-map
  :parent markdown-mode-map
  "q" #'quit-window)

(define-derived-mode hermes-chat-background-mode markdown-mode "Hermes Background"
  "Major mode for a rendered Hermes background-task result."
  :interactive nil
  (read-only-mode 1))

(defun hermes-chat--entry-expanded-p (entry)
  "Return non-nil when ENTRY's detail view is expanded."
  (plist-get (plist-get entry :metadata) :expanded))

(defun hermes-chat--metadata-preserve-expanded (entry metadata)
  "Return METADATA preserving ENTRY's explicit expansion state."
  (if-let* ((tail (plist-member (plist-get entry :metadata) :expanded)))
      (plist-put metadata :expanded (cadr tail))
    metadata))

(defun hermes-chat--copy-table-button (button)
  "Copy the exact original Markdown table stored on BUTTON."
  (kill-new (button-get button 'hermes-chat-table))
  (message "Copied table source"))

(defun hermes-chat--table-window-width ()
  "Return usable fixed-pitch columns in the narrowest displaying window.
A hidden buffer uses 78 columns until it becomes visible.  Reserve two
columns for the continuation glyph and rounding on graphical displays."
  ;; Selecting an unselected chat window adopts its draft point.  Restore
  ;; the caller's insertion point before returning to the EWOC printer.
  (save-excursion
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (if windows
          (apply #'min
                 (mapcar (lambda (window)
                           (with-selected-window window
                             ;; Body width excludes margins and fringes, but
                             ;; includes the line-number gutter.  Its native pixel
                             ;; width includes padding and the line-number face.
                             (let ((pixels (- (window-body-width window t)
                                              (if display-line-numbers
                                                  (line-number-display-width t)
                                                0))))
                               (max 6 (- (/ pixels
                                            (max 1 (window-font-width
                                                    window 'fixed-pitch)))
                                         2)))))
                         windows))
        78))))

(defun hermes-chat--insert-table (source width)
  "Insert SOURCE as a navigable grid within WIDTH, with a source-copy button."
  (let ((start (point)))
    (insert (hermes-chat--format-table source width))
    (insert-text-button
     "[Copy source]" 'face '(fixed-pitch link) 'follow-link t
     'help-echo "Copy the original Markdown, not the wrapped presentation"
     'hermes-chat-table source 'action #'hermes-chat--copy-table-button)
    (insert "\n")
    (add-text-properties start (point)
                         (list 'hermes-chat-inline-table source
                               'hermes-chat-table-width width
                               'rear-nonsticky t))))

(defun hermes-chat--reflow-tables (limit width)
  "Reflow inline tables before marker LIMIT to WIDTH, without changing source.
The caller owns transcript protection and draft undo.  Native diff-based
replacement preserves markers in unchanged text, including EWOC boundaries."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) limit)
      (let* ((start (point))
             (end (next-single-property-change
                   start 'hermes-chat-inline-table nil limit))
             (source (get-text-property start 'hermes-chat-inline-table)))
        (if (and source
                 (not (equal width (get-text-property start 'hermes-chat-table-width))))
            (let ((text (with-temp-buffer
                          (hermes-chat--insert-table source width)
                          (buffer-string))))
              (replace-region-contents start end (lambda () text))
              ;; Native replacement diffs characters, not their properties.
              (let ((offset 0))
                (while (< offset (length text))
                  (let ((next (next-property-change offset text (length text))))
                    (set-text-properties (+ start offset) (+ start next)
                                         (text-properties-at offset text))
                    (setq offset next))))
              (goto-char (+ start (length text))))
          (goto-char end))))))

(defun hermes-chat--insert-markdown (text)
  "Insert fontified TEXT with wrapped, navigable inline tables.
Ordinary region copying copies the presentation; each table also offers
an explicit button to copy its exact original Markdown source."
  (let* ((text (hermes-chat--fontify-markdown-string text))
         (width (hermes-chat--table-window-width))
         (end (length text))
         (start 0))
    (while (< start end)
      (let ((next (next-single-property-change
                   start 'hermes-chat-table text end))
            (table (get-text-property start 'hermes-chat-table text)))
        (if table
            (hermes-chat--insert-table table width)
          (insert (substring text start next)))
        (setq start next)))))

(defun hermes-chat--insert-shadow (text)
  "Insert TEXT with the `shadow' face when it is non-empty."
  (unless (string-empty-p text)
    (insert (propertize text 'face 'shadow))))

(defun hermes-chat--show-diff (diff &optional buffer-name)
  "Show DIFF in a dedicated `diff-mode' buffer.
BUFFER-NAME overrides the default \"*Hermes Diff*\" buffer.  The buffer is made
read-only so `diff-mode' installs its navigation keymap (n/p hunks, q quits)
instead of `view-mode' shadowing those keys.  Each opening uses the source
buffer's `default-directory' for native file navigation."
  (let ((directory default-directory)
        (buffer (get-buffer-create (or buffer-name "*Hermes Diff*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff)
        (unless (string-suffix-p "\n" diff) (insert "\n")))
      (goto-char (point-min))
      (delay-mode-hooks (diff-mode))
      (setq default-directory directory)
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
      (hermes-transport--non-empty-string
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

(defun hermes-chat--show-background-result (number content)
  "Show background task NUMBER's CONTENT in a dedicated markdown buffer.
The buffer renders CONTENT as markdown with diffs swapped for View Diff links,
mirroring `hermes-chat--show-diff'.  `hermes-chat-background-mode' keeps the
rendered buffer read-only and binds `q' to `quit-window'."
  (let ((buffer (get-buffer-create (format "*hermes-bg #%d*" number))))
    (with-current-buffer buffer
      (hermes-chat-background-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (hermes-chat--insert-diffed content #'hermes-chat--insert-markdown)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun hermes-chat--view-background-button (button)
  "Open the full background result stored on BUTTON in its own buffer."
  (hermes-chat--show-background-result
   (button-get button 'hermes-chat-background-number)
   (button-get button 'hermes-chat-background-content)))

(defun hermes-chat--insert-background-entry (entry)
  "Insert ENTRY as a one-line background result notice with a View Result link.
ENTRY's metadata supplies the task `:number' and prompt `:preview'; its
`:content' is the full response opened by the link."
  (let* ((meta (plist-get entry :metadata))
         (number (or (plist-get meta :number) 0))
         (preview (or (plist-get meta :preview) ""))
         (content (or (plist-get entry :content) "")))
    (insert (propertize (format "⚕ Background #%d done" number)
                        'face 'hermes-chat-background))
    (unless (string-empty-p preview)
      (insert (propertize (format "  %s" preview) 'face 'shadow)))
    (insert "  ")
    (insert-text-button
     "[View Result]"
     'face 'link
     'mouse-face 'highlight
     'follow-link t
     'help-echo "Open this background task's full result in a separate buffer"
     'hermes-chat-background-number number
     'hermes-chat-background-content content
     'action #'hermes-chat--view-background-button)
    (insert "\n")))

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

(defun hermes-chat--insert-image-url (url)
  "Insert image URL as an inline image when possible.
Fail soft with a shadow placeholder when image creation fails."
  (condition-case nil
      (let ((image (and (display-images-p)
                        (hermes-chat--create-image-from-url url))))
        (if image
            (progn
              (insert-image image "[image]")
              (insert "\n"))
          (insert (propertize "[image]\n" 'face 'shadow))))
    (error
     (insert (propertize "[image unavailable]\n" 'face 'shadow)))))

(defun hermes-chat--create-image-from-url (url)
  "Return an image descriptor for data:image URL, or nil.
Reject oversized payloads before and after base64 decode using
`hermes-chat--max-embedded-image-base64' and
`hermes-chat--max-embedded-image-decoded-bytes'.  Never fetches remote URLs."
  (when (and (stringp url)
             (string-prefix-p hermes-chat--data-image-prefix url))
    (when-let* ((marker (string-match ";base64," url))
                (payload-start (+ marker (length ";base64,")))
                ((<= (- (length url) payload-start)
                     hermes-chat--max-embedded-image-base64))
                (payload (substring url payload-start))
                (data (ignore-errors (base64-decode-string payload)))
                ((and (stringp data)
                      (> (length data) 0)
                      (<= (length data)
                          hermes-chat--max-embedded-image-decoded-bytes))))
      (create-image data nil t :max-width 640))))

(defun hermes-chat--insert-content-with-images (content insert-text &optional blocks)
  "Insert CONTENT via INSERT-TEXT, lifting images outside diff blocks.
BLOCKS is optional precomputed diff metadata for the original CONTENT."
  (let (images)
    (hermes-chat--insert-diffed
     (or content "")
     (lambda (segment)
       (pcase-let ((`(,text . ,segment-images)
                    (hermes-chat--extract-embedded-images segment)))
         (funcall insert-text (or text ""))
         (setq images (append images segment-images))))
     blocks)
    (mapc #'hermes-chat--insert-image-url images)))

(provide 'hermes-chat-render)
;;; hermes-chat-render.el ends here
