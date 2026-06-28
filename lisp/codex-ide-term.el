;;; codex-ide-term.el --- vterm integration for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, terminal
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

;; vterm integration for `codex-ide'.  Codex runs as a real terminal process;
;; this file only owns vterm session creation, input forwarding, terminal color
;; query replies, and display-size synchronization.

;;; Code:

(require 'cl-lib)
(require 'codex-ide-debug)
(require 'subr-x)
(require 'vterm)

(defvar vterm-shell)
(defvar vterm-environment)

;;; Process lifecycle

(defun codex-ide-term--make-process (buffer-name program args env working-dir)
  "Start PROGRAM with ARGS in a vterm buffer named BUFFER-NAME.
ENV is a list of \"KEY=VALUE\" strings prepended to the vterm process
environment.  Returns the process object."
  ;; vterm runs a single command via `vterm-shell'; keep shell quoting
  ;; isolated to this boundary.
  (let ((default-directory (or working-dir default-directory))
        (vterm-shell (string-join
                      (mapcar #'shell-quote-argument (cons program args))
                      " "))
        (vterm-environment (append env vterm-environment)))
    (save-window-excursion
      (let ((buffer (vterm buffer-name)))
        (unless buffer
          (error "Failed to create vterm buffer"))
        (let ((process (get-buffer-process buffer)))
          (unless process
            (error "Failed to create vterm process"))
          (with-current-buffer buffer
            (codex-ide-term--vterm-configure-buffer process))
          process)))))

(defun codex-ide-term--send-string (string)
  "Send STRING to the current vterm buffer."
  (vterm-send-string string))

(defun codex-ide-term--send-return ()
  "Send RET to the current vterm buffer."
  (vterm-send-return))

(defun codex-ide-term--send-escape ()
  "Send ESC to the current vterm buffer."
  (vterm-send-escape))

(defun codex-ide-term--sync-dimensions (buffer window)
  "Sync BUFFER terminal dimensions to WINDOW through vterm's resize hook."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (with-current-buffer buffer
      (when-let* ((process (get-buffer-process buffer))
                  (adjust (process-get process 'adjust-window-size-function)))
        (when (functionp adjust)
          (funcall adjust process (list window)))))))

;;; Terminal color queries

(defconst codex-ide-term--osc-color-query-regexp
  (rx (seq ?\e "]"
           (group (or "10" "11"))
           ";?"
           (or ?\a (seq ?\e "\\"))))
  "Regexp matching OSC 10/11 default color queries.")

(defun codex-ide-term--osc-color-query-types (string)
  "Return color query types from OSC 10/11 queries in STRING."
  (when string
    (cl-loop with start = 0
             while (string-match codex-ide-term--osc-color-query-regexp
                                 string start)
             collect (if (equal (match-string 1 string) "10")
                         'foreground
                       'background)
             do (setq start (match-end 0)))))

(defun codex-ide-term--osc-color-code (type)
  "Return the OSC color code for TYPE."
  (pcase type
    ('foreground 10)
    ('background 11)
    (_ (error "Unknown OSC color type: %s" type))))

(defun codex-ide-term--osc-color-reply (type rgb)
  "Return an OSC color reply for TYPE and RGB values."
  (format "\e]%d;rgb:%04x/%04x/%04x\e\\"
          (codex-ide-term--osc-color-code type)
          (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))

(defun codex-ide-term--osc-color-query-replies (string foreground background)
  "Return OSC color replies for queries in STRING.
FOREGROUND and BACKGROUND are RGB value lists."
  (when-let* ((types (codex-ide-term--osc-color-query-types string)))
    (string-join
     (mapcar (lambda (type)
               (codex-ide-term--osc-color-reply
                type (if (eq type 'foreground) foreground background)))
             types)
     "")))

(defun codex-ide-term--color-values-or-fallback (color fallback)
  "Return COLOR values, or FALLBACK when COLOR is unavailable."
  (or (and color (color-values color)) fallback))

(defun codex-ide-term--default-face-color-values (type)
  "Return default face color values for TYPE."
  (pcase type
    ('foreground
     (codex-ide-term--color-values-or-fallback
      (face-foreground 'default) '(255 255 255)))
    ('background
     (codex-ide-term--color-values-or-fallback
      (face-background 'default) '(0 0 0)))
    (_ (error "Unknown default face color type: %s" type))))

(defun codex-ide-term--vterm-osc-color-replies (input)
  "Return vterm OSC color replies for INPUT."
  (codex-ide-term--osc-color-query-replies
   input
   (codex-ide-term--default-face-color-values 'foreground)
   (codex-ide-term--default-face-color-values 'background)))

;;; vterm output filter

(defun codex-ide-term--vterm-send-osc-replies (process input)
  "Send OSC color replies for PROCESS output INPUT."
  (when-let* ((reply (codex-ide-term--vterm-osc-color-replies input)))
    (process-send-string process reply)))

(defun codex-ide-term--vterm-call-original-filter (process input)
  "Call PROCESS's original vterm filter with INPUT."
  (when-let* ((filter (process-get process
                                   'codex-ide-term--vterm-original-filter)))
    (funcall filter process input)))

(defun codex-ide-term--vterm-output-filter (process input)
  "Wrap vterm PROCESS output INPUT with Codex terminal replies."
  (condition-case err
      (codex-ide-term--vterm-send-osc-replies process input)
    (error
     (codex-ide-debug "Could not answer vterm OSC query: %s"
                      (error-message-string err))))
  (codex-ide-term--vterm-call-original-filter process input))

(defun codex-ide-term--vterm-install-output-filter (process)
  "Install the Codex vterm output filter for PROCESS."
  (when (processp process)
    (unless (eq (process-filter process)
                #'codex-ide-term--vterm-output-filter)
      (unless (process-get process
                           'codex-ide-term--vterm-original-filter)
        (process-put process
                     'codex-ide-term--vterm-original-filter
                     (process-filter process)))
      (set-process-filter process #'codex-ide-term--vterm-output-filter))))

(defun codex-ide-term--vterm-configure-buffer (process)
  "Configure the current Codex vterm buffer for PROCESS."
  (setq-local truncate-lines t)
  (codex-ide-term--vterm-install-output-filter process))

(provide 'codex-ide-term)

;;; codex-ide-term.el ends here
