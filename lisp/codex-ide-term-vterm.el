;;; codex-ide-term-vterm.el --- Optional vterm backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, terminal
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

;; Optional vterm session creation and input forwarding for `codex-ide'.

;;; Code:

(require 'subr-x)

(declare-function vterm-copy-mode "vterm" (&optional arg))
(declare-function vterm-mode "vterm" ())
(declare-function vterm-reset-cursor-point "vterm" ())
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))

(defvar vterm-copy-mode)
(defvar vterm-environment)
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-shell)

(defun codex-ide-term-vterm--available-p ()
  "Return non-nil when vterm can be loaded."
  (condition-case nil
      (require 'vterm nil t)
    (error nil)))

(defun codex-ide-term-vterm--command (program args)
  "Return a shell command to run PROGRAM with ARGS unchanged."
  (mapconcat #'shell-quote-argument (cons program args) " "))

(defun codex-ide-term-vterm--prepare-buffer (buffer-name working-dir)
  "Prepare and return a vterm BUFFER-NAME for WORKING-DIR."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq default-directory (or working-dir default-directory)))
    buffer))

(defun codex-ide-term-vterm--configure-buffer ()
  "Validate the current Codex vterm buffer."
  (unless (derived-mode-p 'vterm-mode)
    (error "Codex vterm buffer has not started")))

(defun codex-ide-term-vterm--make-process (buffer program args env)
  "Start PROGRAM with ARGS and ENV in prepared vterm BUFFER."
  (with-current-buffer buffer
    (let ((vterm-shell (codex-ide-term-vterm--command program args))
          (vterm-environment (append env vterm-environment))
          (vterm-kill-buffer-on-exit nil))
      (vterm-mode))
    (setq-local vterm-kill-buffer-on-exit nil)
    (or (get-buffer-process buffer)
        (error "Failed to create vterm process"))))

(defun codex-ide-term-vterm--send-string (string)
  "Send STRING to the current vterm terminal."
  (vterm-send-string string))

(defun codex-ide-term-vterm--send-return ()
  "Send RET to the current vterm terminal."
  (vterm-send-return))

(defun codex-ide-term-vterm--send-escape ()
  "Send ESC to the current vterm terminal."
  (vterm-send-escape))

(defun codex-ide-term-vterm--return-live ()
  "Leave vterm copy mode and follow the live cursor."
  (when vterm-copy-mode
    (vterm-copy-mode -1))
  (vterm-reset-cursor-point))

(provide 'codex-ide-term-vterm)

;;; codex-ide-term-vterm.el ends here
