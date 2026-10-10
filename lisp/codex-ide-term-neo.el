;;; codex-ide-term-neo.el --- Native Neomacs terminal backend -*- lexical-binding: t; -*-

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

;; Use neo-term's exact-command entry point.  The native host owns the PTY;
;; the session resource is an identity-bearing plist, not an Emacs process.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function neo-term-exec "neo-term" (executable argv directory environment &optional origin))
(declare-function neo-term--write "neo-term" (terminal-id string))
(declare-function neo-term--resize "neo-term" (terminal-id cols rows))
(declare-function neo-term--kill-buffer-terminal "neo-term" ())
(declare-function codex-ide--finish-session "codex-ide"
                  (root id buffer process success event))
(defvar neo-term--id)
(defvar neo-term--terminals)
(defvar neo-term-settled-functions)
(defvar neo-term-create-failed-functions)
(defvar neo-term-exit-functions)
(defvar neo-term-title-changed-functions)
(defvar codex-ide-term--backend)
(defvar codex-ide--session-root)
(defvar codex-ide--session-id)

(defvar-local codex-ide-term-neo--resource nil
  "Exact native terminal resource owned by this buffer.")

(defun codex-ide-term-neo--available-p ()
  "Return non-nil when exact-command native terminals are available."
  (and (featurep 'neomacs)
       (require 'neo-term nil t)
       (fboundp 'neo-term-exec)
       (fboundp 'neomacs-terminal-spawn)
       (boundp 'neo-term-settled-functions)
       (boundp 'neo-term-create-failed-functions)))

(defun codex-ide-term-neo--prepare-buffer (name directory)
  "Prepare a placeholder named NAME in DIRECTORY before native launch."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq-local default-directory directory))
    buffer))

(defun codex-ide-term-neo--configure-buffer ()
  "Keep the native terminal's own input and display configuration."
  nil)

(defun codex-ide-term-neo--live-p (resource buffer)
  "Return non-nil if RESOURCE still owns BUFFER's native terminal."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq resource codex-ide-term-neo--resource)
              (eq major-mode 'neo-term-mode)
              (eql (plist-get resource :id) neo-term--id)
              (eq buffer (plist-get (gethash neo-term--id neo-term--terminals)
                                    :buffer))))))

(defun codex-ide-term-neo--remove-hooks (resource)
  "Remove only the native completion hooks belonging to RESOURCE."
  (remove-hook 'neo-term-settled-functions (plist-get resource :settled))
  (remove-hook 'neo-term-create-failed-functions (plist-get resource :failed))
  (remove-hook 'neo-term-exit-functions (plist-get resource :exited))
  (remove-hook 'neo-term-title-changed-functions (plist-get resource :title)))

(defun codex-ide-term-neo--stop (resource buffer)
  "Retire RESOURCE and destroy its native terminal in BUFFER."
  (codex-ide-term-neo--remove-hooks resource)
  (when (and (buffer-live-p buffer)
             (eq resource (buffer-local-value 'codex-ide-term-neo--resource buffer)))
    (with-current-buffer buffer
      (when (eql neo-term--id (plist-get resource :id))
        (neo-term--kill-buffer-terminal))
      (setq codex-ide-term-neo--resource nil))))

(defun codex-ide-term-neo--finished (resource terminal-id success event)
  "Retire RESOURCE after TERMINAL-ID finishes with SUCCESS and EVENT."
  (when (eql terminal-id (plist-get resource :id))
    (codex-ide-term-neo--remove-hooks resource)
    (let ((buffer (plist-get resource :buffer)))
      (when (and (buffer-live-p buffer)
                 (eq resource (buffer-local-value 'codex-ide-term-neo--resource buffer)))
        (with-current-buffer buffer
          (codex-ide--finish-session codex-ide--session-root codex-ide--session-id
                                     buffer resource success event))))))

(defun codex-ide-term-neo--watch (resource)
  "Install native completion callbacks for RESOURCE."
  (let ((settled (lambda (id code signal drained wait-error read-error)
                   (codex-ide-term-neo--finished
                    resource id (and (eql code 0) (null signal) drained
                                     (null wait-error) (null read-error))
                    (format "%s%s%s%s" (or signal code "unknown status")
                            (if drained "" "; output not drained")
                            (if wait-error (format "; %s" wait-error) "")
                            (if read-error (format "; %s" read-error) "")))))
        (failed (lambda (id error)
                  (codex-ide-term-neo--finished resource id nil (format "%s" error))))
        (exited (lambda (id)
                  (codex-ide-term-neo--finished resource id nil "unknown status")))
        (title (lambda (id _title)
                 (let ((buffer (plist-get resource :buffer)))
                   (when (and (eql id (plist-get resource :id))
                              (codex-ide-term-neo--live-p resource buffer))
                     ;; Native OSC titles must not replace project/session labels.
                     (with-current-buffer buffer
                       (rename-buffer (plist-get resource :name) t)))))))
    (setf (plist-get resource :settled) settled
          (plist-get resource :failed) failed
          (plist-get resource :exited) exited
          (plist-get resource :title) title)
    ;; Let neo-term finish its own native teardown and output snapshot first.
    (add-hook 'neo-term-settled-functions settled t)
    (add-hook 'neo-term-create-failed-functions failed t)
    (add-hook 'neo-term-exit-functions exited t)
    (add-hook 'neo-term-title-changed-functions title t)))

(defun codex-ide-term-neo--make-process (placeholder program args env)
  "Launch exact PROGRAM and ARGS with ENV, replacing PLACEHOLDER.
Return a native resource, not an Emacs process or a shell launcher."
  (let* ((executable (or (executable-find program)
                         (user-error "Executable not found: %s" program)))
         (name (buffer-name placeholder))
         (directory (buffer-local-value 'default-directory placeholder))
         (buffer (neo-term-exec executable args directory
                                (append env process-environment)))
         (resource (list :id (buffer-local-value 'neo-term--id buffer)
                         :buffer buffer :command (cons program args)
                         :settled nil :failed nil :exited nil :title nil :name name))
         complete)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local codex-ide-term--backend 'neo-term)
            (setq-local codex-ide-term-neo--resource resource)
            (codex-ide-term-neo--watch resource))
          (kill-buffer placeholder)
          (with-current-buffer buffer (rename-buffer name t))
          (setq complete t)
          resource)
      (unless complete
        (codex-ide-term-neo--stop resource buffer)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun codex-ide-term-neo--send-string (string)
  "Send literal STRING through the current native PTY."
  (unless (codex-ide-term-neo--live-p codex-ide-term-neo--resource (current-buffer))
    (user-error "Native terminal is no longer current"))
  (neo-term--write neo-term--id string))

(defun codex-ide-term-neo--send-return ()
  "Send Return through the current native PTY."
  (codex-ide-term-neo--send-string "\r"))

(defun codex-ide-term-neo--send-escape ()
  "Send Escape through the current native PTY."
  (codex-ide-term-neo--send-string "\e"))

(defun codex-ide-term-neo--return-live ()
  "Display the current native terminal's live frame."
  ;; Native rendering follows its own cursor, independently of buffer point.
  (codex-ide-term-neo--send-string ""))

(defun codex-ide-term-neo--sync-dimensions (window)
  "Resize the current native terminal to WINDOW's text dimensions."
  (when (codex-ide-term-neo--live-p codex-ide-term-neo--resource (current-buffer))
    (neo-term--resize neo-term--id (max 1 (window-body-width window))
                      (max 1 (window-body-height window)))))

(provide 'codex-ide-term-neo)

;; Local Variables:
;; package-lint-main-file: "codex-ide.el"
;; End:

;;; codex-ide-term-neo.el ends here
