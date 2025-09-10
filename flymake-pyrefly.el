;;; flymake-pyrefly.el --- A Pyrefly Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Boris Shminke

;; Author: Boris Shminke <boris@shminke.com>
;; Maintainer: Boris Shminke <boris@shminke.com>
;; Created: 29 Jun 2025
;; Version: 0.1.7
;; Keywords: tools, languages
;; URL: https://github.com/inpefess/flymake-pyrefly
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Pyrefly (https://pyrefly.org/) is a fast Python type checker
;; written in Rust. flymake-pyrefly is a Pyrefly backend for Flymake
;; that helps you to see typing errors in your Python files.
;;
;; Usage:
;;
;;   (add-hook 'python-base-mode-hook #'pyrefly-setup-flymake-backend)
;;
;;   ;; alternatively, with use-package
;;   (use-package flymake-pyrefly
;;     :hook (python-base-mode . pyrefly-setup-flymake-backend))
;;
;;   ;; or if you use eglot
;;   (add-hook 'eglot-managed-mode-hook #'pyrefly-setup-flymake-backend)
;;
;;   ;; if you use eglot and use-package
;;   (use-package flymake-pyrefly
;;     :hook (eglot-managed-mode . pyrefly-setup-flymake-backend))
;;
;;   You can also specify the Pyrefly binary through Easy Customization
;;   variable flymake-pyrefly-binary-file

;;; Code:
(require 'cl-lib)
(defvar-local pyrefly--flymake-proc nil)

(define-error 'no-pyrefly-error "Cannot find pyrefly")

(defgroup flymake-pyrefly nil
  "A Pyrefly Flymake backend."
  :group 'tools)

;;;###autoload
(defcustom flymake-pyrefly-binary-file "pyrefly"
  "Pyrefly binary (with absolute or relative path)."
  :type 'file
  :group 'flymake-pyrefly)

(defun pyrefly-flymake-backend (report-fn &rest _args)
  "Report pyrefly diagnostic with REPORT-FN."
  ;; Not having pyrefly installed is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  (unless (executable-find flymake-pyrefly-binary-file)
    (signal 'no-pyrefly-error nil))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `flymake-pyrefly-proc' to a different value
  (when (process-live-p pyrefly--flymake-proc)
    (kill-process pyrefly--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let ((source (current-buffer)) (source-file-name buffer-file-name))
    (save-restriction
      (widen)
      ;; Reset the `pyrefly--flymake-proc' process to a new process
      ;; calling the pyrefly tool.
      (setq
       pyrefly--flymake-proc
       (make-process
        :name "flymake-pyrefly" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        :buffer (generate-new-buffer " *flymake-pyrefly*")
        :command
        `(,flymake-pyrefly-binary-file "check" "--output-format" "min-text" "--no-summary" ,source-file-name)
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `pyrefly--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc pyrefly--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's
                      ;; messages and locations, collect them in a list
                      ;; of objects, and call `report-fn'.
                      (cl-loop
                       while (search-forward-regexp
                              (rx
                               ;; diagnostic level (error, warn, etc)
                               (group (one-or-more upper-case)) " "
                               ;; file name
                               (one-or-more (not space)) ".py:"
                               ;; line number
                               (group (one-or-more digit)) ":"
                               ;; start column
                               (group (one-or-more digit)) "-"
                               ;; end column
                               (group (one-or-more digit)) ": "
                               ;; diagnostic message
                               (group (one-or-more not-newline)))
                              nil t)
                       for msg = (match-string 5)
                       for beg = (cons (string-to-number (match-string 2))
                                       (string-to-number (match-string 3)))
                       for end = (cons (string-to-number (match-string 2))
                                       (string-to-number (match-string 4)))
                       for type = (if (equal "ERROR" (match-string 1))
                                      :error
                                    :warning)
                       collect (flymake-make-diagnostic source-file-name
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Cancelling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      (process-send-region pyrefly--flymake-proc (point-min) (point-max))
      (process-send-eof pyrefly--flymake-proc))))

;;;###autoload
(defun pyrefly-setup-flymake-backend ()
  "Setup Pyrefly as Flymake backend."
  (add-hook 'flymake-diagnostic-functions #'pyrefly-flymake-backend nil t))

(provide 'flymake-pyrefly)
;;; flymake-pyrefly.el ends here
