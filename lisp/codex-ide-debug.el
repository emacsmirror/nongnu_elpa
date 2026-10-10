;;; codex-ide-debug.el --- Debug logging for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, debug
;; URL: https://git.thanosapollo.org/emacs-codex-ide

;; This file is not part of GNU Emacs.

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

;; Debug logging for `codex-ide'.  When `codex-ide-debug' is non-nil, the
;; `codex-ide-debug' macro writes timestamped lines into the
;; `*codex-ide-debug*' buffer without evaluating its arguments otherwise.

;;; Code:

(require 'project)

;;; Customization

(defcustom codex-ide-debug nil
  "When non-nil, enable debug logging for `codex-ide'."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-log-with-context t
  "When non-nil, include session context in log messages."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-debug-buffer "*codex-ide-debug*"
  "Buffer name for debug output."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-debug-max-size (* 256 1024)
  "Maximum characters retained in the debug buffer."
  :type 'natnum
  :group 'codex-ide)

;;; Functions

(defun codex-ide--get-session-context ()
  "Return a short project context string for logging."
  (if codex-ide-log-with-context
      (format "[%s]"
              (or (ignore-errors
                    (file-name-nondirectory
                     (directory-file-name
                      (project-root (project-current)))))
                  "no-project"))
    ""))

(defun codex-ide--debug-buffer ()
  "Return the debug buffer, initializing its read-only special mode."
  (let ((buffer (get-buffer-create codex-ide-debug-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode)))
    buffer))

(defun codex-ide--debug-insert (text)
  "Append TEXT to the debug buffer and retain only its bounded tail."
  (with-current-buffer (codex-ide--debug-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert text)
        (when (> (buffer-size) (max 0 codex-ide-debug-max-size))
          (delete-region (point-min)
                         (- (point-max) (max 0 codex-ide-debug-max-size)))))
      (set-buffer-modified-p nil))))

(defmacro codex-ide-debug (format-string &rest args)
  "Log FORMAT-STRING formatted with ARGS when `codex-ide-debug' is on.
The arguments are only evaluated when debugging is enabled."
  `(when codex-ide-debug
     (codex-ide--debug-insert
      (format "%s %s%s\n" (format-time-string "%Y-%m-%d %H:%M:%S")
              (codex-ide--get-session-context) (format ,format-string ,@args)))))

(defun codex-ide-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to `*Messages*'."
  (message "%s %s"
           (codex-ide--get-session-context)
           (apply #'format format-string args)))

;;;###autoload
(defun codex-ide-show-debug ()
  "Show the `codex-ide' debug buffer."
  (interactive)
  (display-buffer (codex-ide--debug-buffer)))

;;;###autoload
(defun codex-ide-clear-debug ()
  "Clear the `codex-ide' debug buffer."
  (interactive)
  (with-current-buffer (codex-ide--debug-buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (set-buffer-modified-p nil)
    (message "Debug buffer cleared")))

(provide 'codex-ide-debug)

;;; codex-ide-debug.el ends here
