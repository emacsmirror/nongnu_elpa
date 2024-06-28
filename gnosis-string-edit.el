;;; gnosis-string-edit.el --- gnosis edit strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; A slightly modified version of string-edit.el.  Gnosis comes with a
;; modified version string-edit.el to make gnosis available to users
;; of Emacs versions < 29.

;;; Code:

(require 'cl-lib)

(defface gnosis-string-edit-prompt
  '((t (:inherit font-lock-comment-face)))
  "Face used on `gnosis-string-edit' help text."
  :group 'text)

(defvar gnosis-string-edit--success-callback)
(defvar gnosis-string-edit--abort-callback)

(defun gnosis-string-edit-ensure-empty-lines (&optional lines)
  "Ensure that there are LINES number of empty lines before point.
If LINES is nil or omitted, ensure that there is a single empty
line before point.

If called interactively, LINES is given by the prefix argument.

If there are more than LINES empty lines before point, the number
of empty lines is reduced to LINES.

If point is not at the beginning of a line, a newline character
is inserted before adjusting the number of empty lines."
  (interactive "p")
  (unless (bolp)
    (insert "\n"))
  (let ((lines (or lines 1))
        (start (save-excursion
                 (if (re-search-backward "[^\n]" nil t)
                     (+ (point) 2)
                   (point-min)))))
    (cond
     ((> (- (point) start) lines)
      (delete-region (point) (- (point) (- (point) start lines))))
     ((< (- (point) start) lines)
      (insert (make-string (- lines (- (point) start)) ?\n))))))

;;;###autoload
(cl-defun gnosis-string-edit (prompt string success-callback
                              &key abort-callback)
  "Switch to a new buffer to edit STRING.
When the user finishes editing (with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-done]), SUCCESS-CALLBACK
is called with the resulting string.

If the user aborts (with \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-abort]), ABORT-CALLBACK (if any) is
called with no parameters.

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If PROMPT is nil, no help text
will be inserted.

Also see `read-string-from-buffer'."
  (with-current-buffer (generate-new-buffer "*edit string*")
    (when prompt
      (let ((inhibit-read-only t))
        (insert prompt)
        (gnosis-string-edit-ensure-empty-lines 0)
        (add-text-properties (point-min) (point)
                             (list 'intangible t
                                   'face 'gnosis-string-edit-prompt
                                   'read-only t))
        (insert (propertize (make-separator-line) 'rear-nonsticky t))
        (add-text-properties (point-min) (point)
                             (list 'gnosis-string-edit--prompt t))))
    (let ((start (point)))
      (insert string)
      (goto-char start))

    ;; Use `fit-window-to-buffer' after the buffer is filled with text.
    (pop-to-buffer (current-buffer)
                   '(display-buffer-below-selected
                     (window-height . (lambda (window)
                                        (fit-window-to-buffer window nil 10)))))

    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (gnosis-string-edit-mode)
    (setq-local gnosis-string-edit--success-callback success-callback)
    (when abort-callback
      (setq-local gnosis-string-edit--abort-callback abort-callback))
    (setq-local header-line-format
                (substitute-command-keys
                 "Type \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-done] when you've finished editing or \\[gnosis-string-edit-abort] to abort"))
    (message "%s" (substitute-command-keys
                   "Type \\<gnosis-string-edit-mode-map>\\[gnosis-string-edit-done] when you've finished editing"))))

(defvar-keymap gnosis-string-edit-mode-map
  "C-c C-c" #'gnosis-string-edit-done
  "C-c C-k" #'gnosis-string-edit-abort)

(define-derived-mode gnosis-string-edit-mode text-mode "String"
  "Mode for editing strings."
  :interactive nil)

(defun gnosis-string-edit-done ()
  "Finish editing the string and call the callback function.
This will kill the current buffer."
  (interactive)
  (goto-char (point-min))
  ;; Skip past the help text.
  (text-property-search-forward 'gnosis-string-edit--prompt)
  (let ((string (buffer-substring (point) (point-max)))
        (callback gnosis-string-edit--success-callback))
    (quit-window 'kill)
    (funcall callback string)))

(defun gnosis-string-edit-abort ()
  "Abort editing the current string."
  (interactive)
  (let ((callback gnosis-string-edit--abort-callback))
    (quit-window 'kill)
    (when callback
      (funcall callback))))

(provide 'gnosis-string-edit)

;;; gnosis-string-edit.el ends here

