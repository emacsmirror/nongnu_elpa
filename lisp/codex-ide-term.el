;;; codex-ide-term.el --- Terminal backends for codex-ide  -*- lexical-binding: t; -*-

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

;; Dispatch to Eat, optional vterm, or the native Neomacs terminal.

;;; Code:

(require 'codex-ide-term-neo)
(require 'subr-x)

;;; User options

(defcustom codex-ide-terminal-backend 'eat
  "Terminal backend used for new Codex sessions.
Eat is the GNU Emacs default.  In Neomacs an uncustomized Eat default
selects `neo-term' automatically, including after upgrading a loaded copy.
Use Customize to explicitly select Eat in Neomacs.  Selecting `vterm'
requires vterm separately.  Existing sessions retain their backend."
  :type '(choice (const :tag "Eat" eat)
                 (const :tag "vterm" vterm)
                 (const :tag "Native Neomacs terminal" neo-term))
  :group 'codex-ide)

;;; Backend dispatch

(defconst codex-ide-term--backends
  '((neo-term
     :feature codex-ide-term-neo
     :available codex-ide-term-neo--available-p
     :prepare codex-ide-term-neo--prepare-buffer
     :configure codex-ide-term-neo--configure-buffer
     :make-process codex-ide-term-neo--make-process
     :send-string codex-ide-term-neo--send-string
     :send-return codex-ide-term-neo--send-return
     :send-escape codex-ide-term-neo--send-escape
     :return-live codex-ide-term-neo--return-live)
    (eat
     :feature codex-ide-term-eat
     :available codex-ide-term-eat--available-p
     :prepare codex-ide-term-eat--prepare-buffer
     :configure codex-ide-term-eat--configure-buffer
     :make-process codex-ide-term-eat--make-process
     :send-string codex-ide-term-eat--send-string
     :send-return codex-ide-term-eat--send-return
     :send-escape codex-ide-term-eat--send-escape
     :return-live codex-ide-term-eat--return-live)
    (vterm
     :feature codex-ide-term-vterm
     :available codex-ide-term-vterm--available-p
     :prepare codex-ide-term-vterm--prepare-buffer
     :configure codex-ide-term-vterm--configure-buffer
     :make-process codex-ide-term-vterm--make-process
     :send-string codex-ide-term-vterm--send-string
     :send-return codex-ide-term-vterm--send-return
     :send-escape codex-ide-term-vterm--send-escape
     :return-live codex-ide-term-vterm--return-live))
  "Operations for the supported terminal backends.")

(defvar-local codex-ide-term--backend nil
  "Terminal backend that owns the current Codex buffer.")

(defun codex-ide-term--backend-spec (backend)
  "Return the fixed operation spec for BACKEND."
  (or (assq backend codex-ide-term--backends)
      (error "Unsupported terminal backend: %s" backend)))

(defun codex-ide-term--load-backend (backend)
  "Load BACKEND and return its operation spec."
  (let* ((spec (codex-ide-term--backend-spec backend))
         (feature (plist-get (cdr spec) :feature)))
    (unless (require feature nil t)
      (user-error "Terminal backend `%s' is not installed" backend))
    (let ((available (plist-get (cdr spec) :available)))
      (unless (and available (funcall available))
        (if (eq backend 'eat)
            (user-error "Terminal backend `eat' is not available.  Install Eat or select an installed backend")
          (user-error "Terminal backend `%s' is not available" backend))))
    spec))

(defun codex-ide-term--operation (backend operation)
  "Return BACKEND function for OPERATION."
  (or (plist-get (cdr (codex-ide-term--load-backend backend)) operation)
      (error "Terminal backend `%s' has no %s operation" backend operation)))

(defun codex-ide-term--selected-backend ()
  "Return the backend for a new session, respecting explicit customization."
  (if (and (featurep 'neomacs) (eq codex-ide-terminal-backend 'eat)
           (not (get 'codex-ide-terminal-backend 'customized-value))
           (not (get 'codex-ide-terminal-backend 'saved-value)))
      'neo-term
    codex-ide-terminal-backend))

(defun codex-ide-term--process (buffer)
  "Return BUFFER's terminal resource, native or Emacs-owned."
  (if (and (buffer-live-p buffer)
           (eq (buffer-local-value 'codex-ide-term--backend buffer) 'neo-term))
      (buffer-local-value 'codex-ide-term-neo--resource buffer)
    (get-buffer-process buffer)))

(defun codex-ide-term--process-buffer (resource)
  "Return the buffer owned by terminal RESOURCE."
  (if (processp resource) (process-buffer resource) (plist-get resource :buffer)))

(defun codex-ide-term--process-live-p (resource buffer)
  "Return non-nil if terminal RESOURCE still owns live BUFFER."
  (and (buffer-live-p buffer)
       (if (processp resource)
           (and (process-live-p resource) (eq resource (get-buffer-process buffer)))
         (codex-ide-term-neo--live-p resource buffer))))

(defun codex-ide-term--stop-process (resource buffer)
  "Stop terminal RESOURCE without touching a successor in BUFFER."
  (if (processp resource)
      (when (process-live-p resource) (delete-process resource))
    (when resource (codex-ide-term-neo--stop resource buffer))))

(defun codex-ide-term--current-backend ()
  "Return the backend that owns the current terminal buffer."
  (or codex-ide-term--backend
      (cond
       ((derived-mode-p 'neo-term-mode) 'neo-term)
       ((derived-mode-p 'vterm-mode) 'vterm)
       ((derived-mode-p 'eat-mode) 'eat)
       (t (codex-ide-term--selected-backend)))))

(defun codex-ide-term--call (operation &rest args)
  "Call the current backend OPERATION with ARGS."
  (let ((backend (codex-ide-term--current-backend)))
    (apply (codex-ide-term--operation backend operation) args)))

;;; Terminal operations

(defun codex-ide-term--prepare-buffer (buffer-name working-dir)
  "Prepare a terminal BUFFER-NAME for WORKING-DIR."
  (let* ((backend (codex-ide-term--selected-backend))
         (prepare (codex-ide-term--operation backend :prepare))
         (buffer (funcall prepare buffer-name working-dir)))
    (with-current-buffer buffer
      (setq-local codex-ide-term--backend backend))
    buffer))

(defun codex-ide-term--configure-buffer ()
  "Configure the current terminal buffer for its owning backend."
  (let ((backend (codex-ide-term--current-backend)))
    (setq-local codex-ide-term--backend backend)
    (funcall (codex-ide-term--operation backend :configure))))

(defun codex-ide-term--make-process (buffer program args env)
  "Start PROGRAM with ARGS and ENV in prepared terminal BUFFER.
Return an Emacs process or an exact native Neomacs resource."
  (with-current-buffer buffer
    (let* ((backend (codex-ide-term--current-backend))
           (make-process (codex-ide-term--operation backend :make-process))
           (process (funcall make-process buffer program args env)))
      (with-current-buffer (codex-ide-term--process-buffer process)
        (setq-local codex-ide-term--backend backend))
      process)))

(defun codex-ide-term--send-string (string)
  "Send STRING to the current terminal."
  (codex-ide-term--call :send-string string))

(defun codex-ide-term--validate-draft (text)
  "Reject unsafe or oversized draft TEXT before terminal input."
  (when (> (string-bytes (encode-coding-string text 'utf-8)) (* 1024 1024))
    (user-error "Terminal input exceeds 1 MiB of UTF-8 text"))
  (when (string-match-p "[\0-\10\13-\37\u007f-\u009f]" text)
    (user-error "Terminal input contains terminal control characters")))

(defun codex-ide-term--paste-draft (process text)
  "Insert literal draft TEXT into the current terminal's captured PROCESS.
Use one bracketed paste, without a Return key.  This operation targets the
Codex TUI, whose input parser handles bracketed paste on all backends."
  (codex-ide-term--validate-draft text)
  (unless (and (memq (codex-ide-term--current-backend) '(eat vterm neo-term))
               (codex-ide-term--process-live-p process (current-buffer)))
    (user-error "Codex terminal process is no longer current"))
  ;; Backend yank functions can fall back to unframed input before the
  ;; terminal has seen the mode-enable sequence, turning LF into key events.
  (let ((paste (concat "\e[200~" text "\e[201~")))
    (if (processp process)
        (process-send-string process paste)
      (codex-ide-term-neo--send-string paste))))

(defun codex-ide-term--send-return ()
  "Send RET to the current terminal."
  (codex-ide-term--call :send-return))

(defun codex-ide-term--send-escape ()
  "Send ESC to the current terminal."
  (codex-ide-term--call :send-escape))

(defun codex-ide-term--return-live ()
  "Restore terminal input and follow the live cursor."
  (codex-ide-term--call :return-live))

(defun codex-ide-term--sync-dimensions (buffer window)
  "Sync BUFFER terminal dimensions to WINDOW through its resize hook."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (with-current-buffer buffer
      (if (eq (codex-ide-term--current-backend) 'neo-term)
          (codex-ide-term-neo--sync-dimensions window)
        (when-let* ((process (get-buffer-process buffer))
                    (adjust (process-get process 'adjust-window-size-function)))
          (when (functionp adjust)
            (funcall adjust process (list window))))))))

(provide 'codex-ide-term)

;; Local Variables:
;; package-lint-main-file: "codex-ide.el"
;; End:

;;; codex-ide-term.el ends here
