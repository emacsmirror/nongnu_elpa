;;; codex-ide-debug.el --- Debug logging for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, debug
;; URL: https://git.thanosapollo.org/emacs-codex

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

(defmacro codex-ide-debug (format-string &rest args)
  "Log FORMAT-STRING formatted with ARGS when `codex-ide-debug' is on.
The arguments are only evaluated when debugging is enabled."
  `(when codex-ide-debug
     (let ((msg (format ,format-string ,@args))
           (ts (format-time-string "%Y-%m-%d %H:%M:%S"))
           (ctx (codex-ide--get-session-context)))
       (with-current-buffer (get-buffer-create codex-ide-debug-buffer)
         (goto-char (point-max))
         (insert (format "%s %s%s\n" ts ctx msg))))))

(defun codex-ide-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to `*Messages*'."
  (message "%s %s"
           (codex-ide--get-session-context)
           (apply #'format format-string args)))

;;;###autoload
(defun codex-ide-show-debug ()
  "Show the `codex-ide' debug buffer."
  (interactive)
  (display-buffer (get-buffer-create codex-ide-debug-buffer)))

;;;###autoload
(defun codex-ide-clear-debug ()
  "Clear the `codex-ide' debug buffer."
  (interactive)
  (with-current-buffer (get-buffer-create codex-ide-debug-buffer)
    (erase-buffer)
    (message "Debug buffer cleared")))

(provide 'codex-ide-debug)

;;; codex-ide-debug.el ends here
