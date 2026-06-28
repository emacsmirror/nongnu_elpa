;;; codex-ide-term.el --- Terminal backend abstraction for codex-ide  -*- lexical-binding: t; -*-

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

;; Terminal backend abstraction for `codex-ide'.  A backend is a small plain
;; plist implementing four operations, dispatched through
;; `codex-ide-term--call'.  Adding or removing a backend is a localized change
;; to `codex-ide-term--backends' plus the backend descriptor.

;;; Code:

(require 'codex-ide-debug)
(require 'subr-x)

;; External declarations for backends (loaded lazily by :ensure).
(defvar vterm-shell)
(defvar vterm-environment)
(defvar eat-terminal)
(defvar eat--synchronize-scroll-function)
(defvar eat-very-visible-cursor-type)
(defvar eat-very-visible-vertical-bar-cursor-type)
(defvar eat-very-visible-horizontal-bar-cursor-type)

(defvar-local codex-ide-term--backend nil
  "Backend descriptor plist for the current Codex terminal buffer.
When nil, send operations fall back to the active global backend.")

(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile switches))
(declare-function eat-term-send-string "eat" (terminal string))

;;; User options

(defcustom codex-ide-term-blink-cursor nil
  "Non-nil lets the Codex TUI drive a blinking cursor in eat buffers.
When nil, the cursor stays steady even though Codex requests a blinking
one via its terminal cursor-style escape.  Only the eat backend honors
this; vterm renders the cursor itself."
  :type 'boolean
  :group 'codex-ide)

;;; Backend registry

(defconst codex-ide-term--backends
  '((vterm . codex-ide-term--vterm-backend)
    (eat   . codex-ide-term--eat-backend))
  "Alist mapping backend symbols to backend descriptor functions.
Each function returns a plist with operations:
  :ensure       ()            -> require feature or `user-error'
  :make-process (name prog args env cwd) -> process
  :send-string  (string)
  :send-return  ()
  :send-escape  ()")

(defun codex-ide-term--resolve-backend (&optional symbol)
  "Return the backend descriptor plist for SYMBOL.
SYMBOL defaults to `codex-ide-terminal-backend'.  Signals `user-error'
for an unknown backend."
  (let* ((sym (or symbol
                  (bound-and-true-p codex-ide-terminal-backend)
                  'vterm))
         (fn (alist-get sym codex-ide-term--backends)))
    (unless fn
      (user-error "Unknown terminal backend: %s.  Valid: vterm, eat" sym))
    (funcall fn)))

(defun codex-ide-term--call (backend op &rest args)
  "Dispatch operation OP with ARGS on BACKEND descriptor.
BACKEND is a plist of operation keywords to handler functions."
  (let ((handler (plist-get backend op)))
    (unless handler
      (error "Backend does not implement %s" op))
    (apply handler args)))

(defun codex-ide-term--current-backend ()
  "Return the buffer-local backend descriptor or the active backend."
  (or codex-ide-term--backend
      (codex-ide-term--resolve-backend)))

(defun codex-ide-term--ensure-backend ()
  "Ensure the active terminal backend feature is loaded, or `user-error'."
  (codex-ide-term--call (codex-ide-term--resolve-backend) :ensure))

(defun codex-ide-term--make-process (buffer-name program args env working-dir)
  "Start PROGRAM with ARGS in a terminal buffer named BUFFER-NAME.
ENV is a list of \"KEY=VALUE\" strings prepended to the process environment.
WORKING-DIR is the working directory.  Returns the process object."
  (let* ((backend (codex-ide-term--resolve-backend))
         (process (codex-ide-term--call backend
                                        :make-process
                                        buffer-name program args env working-dir)))
    (when-let ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        (setq-local codex-ide-term--backend backend)))
    process))

(defun codex-ide-term--send-string (string)
  "Send STRING to the terminal in the current buffer."
  (codex-ide-term--call (codex-ide-term--current-backend)
                        :send-string string))

(defun codex-ide-term--send-return ()
  "Send RET to the terminal in the current buffer."
  (codex-ide-term--call (codex-ide-term--current-backend) :send-return))

(defun codex-ide-term--send-escape ()
  "Send ESC to the terminal in the current buffer."
  (codex-ide-term--call (codex-ide-term--current-backend) :send-escape))

(defun codex-ide-term--sync-dimensions (buffer window)
  "Sync BUFFER process dimensions to WINDOW, when both are live."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (with-current-buffer buffer
      (when-let ((proc (get-buffer-process buffer)))
        (set-process-window-size proc
                                 (window-body-height window)
                                 (window-body-width window))))))

;;; vterm backend

(defun codex-ide-term--vterm-backend ()
  "Return the vterm backend descriptor."
  (list
   :ensure
   (lambda ()
     (unless (require 'vterm nil t)
       (user-error
        "Package vterm is not installed.  Install vterm or set `codex-ide-terminal-backend' to `eat'")))
   :make-process
   (lambda (buffer-name program args env _working-dir)
     ;; vterm runs a single command via `vterm-shell'; keep shell quoting
     ;; isolated to this boundary.
     (let ((vterm-shell (string-join
                         (mapcar #'shell-quote-argument (cons program args))
                         " "))
           (vterm-environment (append env vterm-environment)))
       (save-window-excursion
         (let ((buffer (vterm buffer-name)))
           (unless buffer
             (error "Failed to create vterm buffer"))
           (with-current-buffer buffer
             (setq-local vterm-scroll-to-bottom-on-output nil))
           (get-buffer-process buffer)))))
   :send-string
   (lambda (string) (vterm-send-string string))
   :send-return
   (lambda () (vterm-send-return))
   :send-escape
   (lambda () (vterm-send-escape))))

;;; eat backend

(defun codex-ide-term--steady-eat-cursor (cursor-shape)
  "Return CURSOR-SHAPE with Eat's blink frequency disabled."
  (list (car cursor-shape) nil (nth 2 cursor-shape)))

(defun codex-ide-term--eat-backend ()
  "Return the eat backend descriptor."
  (list
   :ensure
   (lambda ()
     (unless (require 'eat nil t)
       (user-error
        "Package eat is not installed.  Install eat or set `codex-ide-terminal-backend' to `vterm'")))
   :make-process
   ;; `eat-exec' takes program and args natively, so structured args pay off
   ;; here without re-parsing a shell command string.  SWITCHES is a single
   ;; list argument (eat 0.9+), so pass ARGS directly rather than spreading it.
   (lambda (buffer-name program args env working-dir)
     (let ((buffer (get-buffer-create buffer-name))
           (default-directory working-dir))
       (with-current-buffer buffer
         (unless (eq major-mode 'eat-mode)
           (eat-mode))
         (unless codex-ide-term-blink-cursor
           ;; Codex requests a blinking cursor; render it steady by
           ;; preserving user cursor shapes while clearing blink frequency.
           (setq-local eat-very-visible-cursor-type
                       (codex-ide-term--steady-eat-cursor
                        eat-very-visible-cursor-type)
                       eat-very-visible-vertical-bar-cursor-type
                       (codex-ide-term--steady-eat-cursor
                        eat-very-visible-vertical-bar-cursor-type)
                       eat-very-visible-horizontal-bar-cursor-type
                       (codex-ide-term--steady-eat-cursor
                        eat-very-visible-horizontal-bar-cursor-type)))
         (setq-local process-environment
                     (append env process-environment))
         (eat-exec buffer buffer-name program nil args))
       (let ((proc (get-buffer-process buffer)))
         (unless proc
           (error "Failed to create eat process"))
         proc)))
   :send-string
   (lambda (string)
     (when eat-terminal
       (eat-term-send-string eat-terminal string)))
   :send-return
   (lambda ()
     (when eat-terminal
       (eat-term-send-string eat-terminal "\r")))
   :send-escape
   (lambda ()
     (when eat-terminal
       (eat-term-send-string eat-terminal "\e")))))

(provide 'codex-ide-term)

;;; codex-ide-term.el ends here
