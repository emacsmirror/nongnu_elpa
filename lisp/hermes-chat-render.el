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
  "Insert CONTENT via INSERT-TEXT after lifting embedded image data URLs.
BLOCKS is optional precomputed diff-block metadata for the cleaned text."
  (pcase-let ((`(,text . ,images) (hermes-chat--extract-embedded-images content)))
    (hermes-chat--insert-diffed (or text "") insert-text blocks)
    (mapc #'hermes-chat--insert-image-url images)))

(provide 'hermes-chat-render)
;;; hermes-chat-render.el ends here
