;;; typst-ts-compile.el --- Compile Typst Files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 The typst-ts-mode Project Contributors

;; This file is NOT part of GNU Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)
(require 'typst-ts-variables)

(defun typst-ts-compile-get-target-file (&optional buffer)
  "Get Typst file path to compile for BUFFER.
When `typst-main-file' is non-nil, use it as main file, otherwise use
`buffer-file-name'.  If BUFFER is nil, use current buffer.
Return absolute file path, or nil if there is no associated file."
  (with-current-buffer (or buffer (current-buffer))
    (and-let* ((current-file (buffer-file-name)))
      (if (stringp typst-main-file)
          (expand-file-name typst-main-file
                            (file-name-directory current-file))
        current-file))))

(defun typst-ts-compile-get-target-file-argument (&optional buffer)
  "Get compile target file argument for BUFFER.
If BUFFER is nil, use current buffer.  Return absolute file path or nil."
  (typst-ts-compile-get-target-file buffer))

;;;###autoload
(defun typst-ts-main-file-ask ()
  "Ask for Typst main file and persist it as a file-local variable.
The chosen file is saved to `typst-main-file' in Local Variables comments.
Choosing current file unsets `typst-main-file'."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let* ((modified-before (buffer-modified-p))
         (current-file (expand-file-name buffer-file-name))
         (current-dir (file-name-directory current-file))
         (existing-main-file (and (stringp typst-main-file)
                                  (expand-file-name typst-main-file current-dir)))
         (last-main-file (and (stringp typst-ts-main-file-last)
                              (file-exists-p typst-ts-main-file-last)
                              typst-ts-main-file-last))
         (default-main-file (or existing-main-file
                                last-main-file
                                current-file))
         (default-main-file-display
          (let ((path (file-relative-name default-main-file current-dir)))
            (if (or (string-prefix-p "." path)
                    (string-prefix-p "/" path))
                path
              (concat "./" path))))
         (chosen-file (expand-file-name
                       (read-file-name
                        (format "Main Typst file (default %s): "
                                default-main-file-display)
                        current-dir
                        default-main-file
                        t)))
         (new-main-file (unless (file-equal-p chosen-file current-file)
                          (file-relative-name chosen-file current-dir))))
    (unless (file-equal-p chosen-file current-file)
      (setq typst-ts-main-file-last chosen-file))
    (setq-local typst-main-file new-main-file)
    (if new-main-file
        (add-file-local-variable 'typst-main-file new-main-file)
      (delete-file-local-variable 'typst-main-file))
    (when (fboundp 'typst-ts-set-compile-command)
      (typst-ts-set-compile-command))
    (cond
     ((not (buffer-modified-p))
      (message "Updated main file: %s"
               (or typst-main-file "current buffer")))
     ((not modified-before)
      (save-buffer)
      (message "Updated main file and saved buffer: %s"
               (or typst-main-file "current buffer")))
     ((y-or-n-p "Save buffer now so typst-main-file persists on reopen? ")
      (save-buffer)
      (message "Updated main file and saved buffer: %s"
               (or typst-main-file "current buffer")))
     (t
      (message "Updated main file in buffer only (not saved yet): %s"
               (or typst-main-file "current buffer"))))))

(defun typst-ts-compile--compilation-finish-function (cur-buffer)
  "Compilation finish function.
For `typst-ts-compile-after-compilation-hook' and
`compilation-finish-functions'.  CUR-BUFFER: original typst buffer, in case
user set `display-buffer-alist' option for compilation buffer to switch to
compilation buffer before compilation."
  (lambda (compilation-buffer msg)
    (when (and
           typst-ts-compile-hide-compilation-buffer-if-success
           (string-match-p "finished" (string-trim msg)))
      (delete-windows-on compilation-buffer)
      (message "Execute typst compile successfully."))
    (unwind-protect
        (with-current-buffer cur-buffer
          (run-hook-with-args 'typst-ts-compile-after-compilation-hook compilation-buffer msg))
      (remove-hook 'compilation-finish-functions
                   (typst-ts-compile--compilation-finish-function cur-buffer)))))

(defun typst-ts-compile (&optional preview)
  "Compile current typst file.
When using a prefix argument or the optional argument PREVIEW,
 preview the document after compilation."
  (interactive "P")
  (when preview
    (add-hook 'compilation-finish-functions
              (typst-ts-compile-and-preview--compilation-finish-function
               (current-buffer))))
  (run-hooks typst-ts-compile-before-compilation-hook)

  ;; The reason to take such a awkward solution is that `compilation-finish-functions'
  ;; should be a global variable and also its functions. It doesn't work if we
  ;; define them inside a let binding.
  (add-hook 'compilation-finish-functions
            (typst-ts-compile--compilation-finish-function (current-buffer)))
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (and-let* ((target-file (typst-ts-compile-get-target-file-argument))
             (result-file (typst-ts-compile-get-result-pdf-filename)))
    (compile
     (format "%s compile %s %s %s"
             typst-ts-compile-executable-location
             (shell-quote-argument target-file)
             (shell-quote-argument result-file)
             typst-ts-compile-options)
     'typst-ts-compilation-mode)))


(defun typst-ts-compile-get-result-pdf-filename (&optional buffer check)
  "Get the result PDF filename based on the name of BUFFER.
If BUFFER is nil, it means use the current buffer.
CHECK: non-nil mean check the file existence.
Return nil if the BUFFER has not associated file or the there is
no compiled pdf file when CHECK is non-nil."
  (and-let* ((typst-file (typst-ts-compile-get-target-file buffer)))
    (let* ((output-dir (with-current-buffer (or buffer (current-buffer))
                         (if (string= typst-ts-output-directory "")
                             (file-name-directory typst-file)
                           (expand-file-name typst-ts-output-directory))))
           (res (expand-file-name
                 (concat (file-name-base typst-file) ".pdf")
                 output-dir)))
      (and (or (not check) (file-exists-p res))
           res))))


(defun typst-ts-compile-and-preview--compilation-finish-function (cur-buffer)
  "For `typst-ts-compile-and-preview' and `compilation-finish-functions'.
CUR-BUFFER: original typst buffer, in case user set
`display-buffer-alist' option for compilation buffer to switch to compilation
buffer before compilation."
  (lambda (_b _msg)
    (unwind-protect
        (typst-ts-preview cur-buffer)
      (remove-hook 'compilation-finish-functions
                   (typst-ts-compile-and-preview--compilation-finish-function cur-buffer)))))

;;;###autoload
(defun typst-ts-compile-and-preview ()
  "Compile & Preview.
Assuming the compile output file name is in default style."
  (interactive)
  (typst-ts-compile t))

;;;###autoload
(defun typst-ts-preview (&optional buffer)
  "Preview the typst document output.
If BUFFER is passed, preview its output, otherwise use current buffer."
  (interactive)
  (funcall typst-ts-preview-function (typst-ts-compile-get-result-pdf-filename buffer)))

(defvar typst-ts-compilation-mode-error
  (cons (rx bol "error:" (+ not-newline) "\n" (+ blank) "┌─ "
            (group (+ not-newline)) ":" ;; file
            (group (+ num)) ":"         ;; start-line
            (group (+ num)) "\n")       ;; start-col
        '(1 2 3))
  "Regexp for Error in compilation buffer.")

;;;###autoload
(define-compilation-mode typst-ts-compilation-mode "Typst Compilation"
  "Customized major mode for typst watch compilation."
  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'typst-error typst-ts-compilation-mode-error))
  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'typst-error))

(provide 'typst-ts-compile)

;;; typst-ts-compile.el ends here
