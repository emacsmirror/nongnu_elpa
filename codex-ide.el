;;; codex-ide.el --- Run Codex CLI in an Emacs terminal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (keymap-popup "0.3.1"))
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

;; Run the Codex CLI inside Emacs through `eat' or `vterm'.  This is a
;; terminal-first integration: one Codex session per project root, displayed
;; in a configurable side window, with prompt sending, toggling, and resume.
;;
;; Usage:
;;   M-x codex-ide              Start Codex for the current project
;;   M-x codex-ide-resume-last  Resume the most recent Codex session
;;   M-x codex-ide-toggle       Show/hide the Codex window
;;   M-x codex-ide-send-prompt  Send a prompt from the minibuffer
;;   M-x codex-ide-stop         Stop the session for the current project
;;   M-x codex-ide-menu         Popup menu of all commands

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'codex-ide-debug)
(require 'codex-ide-mcp)
(require 'codex-ide-term)

(autoload 'codex-ide-menu "codex-ide-menu" nil t)

;;; Customization

(defgroup codex-ide nil
  "Run Codex CLI inside Emacs through `eat' or `vterm'."
  :group 'tools
  :prefix "codex-ide-")

(defcustom codex-ide-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-terminal-backend 'vterm
  "Terminal backend for Codex sessions.
`vterm' is the default and most capable; `eat' is an alternative."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat))
  :group 'codex-ide)

(defcustom codex-ide-window-side 'right
  "Side of the frame where the Codex window appears."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'codex-ide)

(defcustom codex-ide-window-width 100
  "Body width of the Codex side window on the left or right."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-window-height 20
  "Height of the Codex side window on the top or bottom."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-use-side-window t
  "When non-nil, display Codex in a dedicated side window.
When nil, follow standard `display-buffer' behavior."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-focus-on-open t
  "When non-nil, select the Codex window when it opens."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-ask-for-approval nil
  "When to ask for human approval before Codex executes commands.
When nil (default), do not pass `--ask-for-approval' so that Codex's
own `config.toml' policy decides.  Otherwise pass the chosen policy."
  :type '(choice (const :tag "Config decides (nil)" nil)
                 (const :tag "untrusted" untrusted)
                 (const :tag "on-request" on-request)
                 (const :tag "never" never))
  :group 'codex-ide)

(defcustom codex-ide-config-overrides nil
  "Alist of Codex TOML config overrides, emitted as `-c key=value' pairs.
Keys are dotted TOML paths, values are strings.
Example: ((\"model\" . \"o3\")
          (\"sandbox_permissions\" . \"[\\\"disk-full-read-access\\\"]\"))"
  :type '(alist :key-type (string :tag "Key")
                :value-type (string :tag "Value"))
  :group 'codex-ide)

(defcustom codex-ide-cli-extra-args nil
  "Extra arguments appended verbatim to the Codex command.
Escape hatch for flags not yet modeled by a defcustom."
  :type '(repeat string)
  :group 'codex-ide)

(defcustom codex-ide-no-alt-screen nil
  "When non-nil, pass `--no-alt-screen' for inline TUI mode."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-buffer-name-function #'codex-ide--default-buffer-name
  "Function called with the working directory to produce a buffer name."
  :type 'function
  :group 'codex-ide)

;;; Variables

(defvar codex-ide--cli-available nil
  "Whether the Codex CLI was detected.")

(defvar codex-ide--processes (make-hash-table :test 'equal)
  "Hash table mapping project roots to their Codex processes.")

(defvar codex-ide--last-accessed-buffer nil
  "The most recently displayed Codex buffer.")

(defvar codex-ide--cleanup-in-progress nil
  "Reentrancy guard for `codex-ide--cleanup-on-exit'.")

;;; Helpers (pure / mostly pure)

(defun codex-ide--get-working-directory ()
  "Return the working directory for a Codex session.
Prefers the current project root; falls back to `default-directory'."
  (expand-file-name
   (if-let ((project (project-current)))
       (project-root project)
     default-directory)))

(defun codex-ide--default-buffer-name (directory)
  "Return the buffer name for DIRECTORY, as `*codex[<basename>]*'."
  (format "*codex[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun codex-ide--get-buffer-name (&optional directory)
  "Return the buffer name for DIRECTORY, defaulting to the current project."
  (funcall codex-ide-buffer-name-function
           (or directory (codex-ide--get-working-directory))))

(defun codex-ide--get-process (&optional directory)
  "Return the Codex process for DIRECTORY, defaulting to current project."
  (gethash (or directory (codex-ide--get-working-directory))
           codex-ide--processes))

(defun codex-ide--build-command (&optional resume-last session-id)
  "Return (PROGRAM . ARGS) for invoking the Codex CLI.
RESUME-LAST non-nil adds \"resume\" \"--last\".
SESSION-ID non-nil adds \"resume\" SESSION-ID and takes precedence over
RESUME-LAST when both are non-nil.  The result is always a cons; argument
folding is pure and does not touch the shell."
  (let ((args nil))
    ;; Config overrides come first, before any subcommand, matching how
    ;; `codex' parses top-level `-c' pairs.
    (dolist (pair codex-ide-config-overrides)
      (setq args (nconc args (list "-c"
                                   (format "%s=%s" (car pair) (cdr pair))))))
    ;; Resume subcommand (mutually exclusive shapes).
    (cond
     (session-id
      (setq args (nconc args (list "resume" session-id))))
     (resume-last
      (setq args (nconc args (list "resume" "--last")))))
    (when codex-ide-ask-for-approval
      (setq args (nconc args (list "--ask-for-approval"
                                   (symbol-name codex-ide-ask-for-approval)))))
    (when codex-ide-no-alt-screen
      (setq args (nconc args (list "--no-alt-screen"))))
    (when codex-ide-cli-extra-args
      (setq args (nconc args codex-ide-cli-extra-args)))
    (cons codex-ide-cli-path args)))

(defun codex-ide--session-config-overrides ()
  "Return Codex config overrides for a new session.
Includes user-provided `codex-ide-config-overrides' and any
session-local overrides needed by enabled integration helpers."
  (append codex-ide-config-overrides
          (when codex-ide-mcp-enabled
            (codex-ide-mcp-config-overrides
             (codex-ide-mcp-ensure-server)))))

;;; CLI detection

(defun codex-ide--detect-cli ()
  "Detect whether the Codex CLI is available and cache the result."
  (setq codex-ide--cli-available
        (condition-case nil
            (eq (call-process codex-ide-cli-path nil nil nil "--version") 0)
          (error nil))))

(defun codex-ide--ensure-cli ()
  "Return non-nil if the Codex CLI is available, detecting if needed."
  (unless codex-ide--cli-available
    (codex-ide--detect-cli))
  codex-ide--cli-available)

;;; Process lifecycle

(defun codex-ide--cleanup-dead-processes ()
  "Remove entries for dead processes from the process table."
  (maphash (lambda (directory process)
             (unless (process-live-p process)
               (remhash directory codex-ide--processes)))
           codex-ide--processes))

(defun codex-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER according to the window customization.
Returns the window.  Updates `codex-ide--last-accessed-buffer'."
  (let ((window
         (if codex-ide-use-side-window
             (let* ((side codex-ide-window-side)
                    (params '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . 0)
                        ,@(when (memq side '(left right))
                            `((window-width
                               . ,(lambda (win)
                                    (let ((delta (- codex-ide-window-width
                                                    (window-body-width win))))
                                      (unless (zerop delta)
                                        (window-resize win delta t)))))))
                        ,@(when (memq side '(top bottom))
                            `((window-height . ,codex-ide-window-height)))
                        (window-parameters . ,params)))))
               (display-buffer buffer))
           (display-buffer buffer))))
    (setq codex-ide--last-accessed-buffer buffer)
    (when (and window codex-ide-focus-on-open)
      (select-window window))
    (when (and window codex-ide-use-side-window
               (memq codex-ide-window-side '(top bottom)))
      (set-window-text-height window codex-ide-window-height)
      (set-window-dedicated-p window t))
    (when window
      (codex-ide-term--sync-dimensions buffer window))
    window))

(defun codex-ide--toggle-existing-window (buffer)
  "Show or hide the window showing BUFFER.
Used when a session is already running."
  (let ((window (get-buffer-window buffer)))
    (if window
        (progn
          (setq codex-ide--last-accessed-buffer buffer)
          (delete-window window)
          (codex-ide-debug "Codex window hidden"))
      (codex-ide--display-buffer-in-side-window buffer)
      (codex-ide-debug "Codex window shown"))))

(defun codex-ide--cleanup-on-exit (directory)
  "Clean up the Codex session state for DIRECTORY.
Reentrancy-guarded: sentinels and `kill-buffer-hook' can both fire."
  (unless codex-ide--cleanup-in-progress
    (let ((codex-ide--cleanup-in-progress t))
      (remhash directory codex-ide--processes)
      (when-let ((buffer (get-buffer (codex-ide--get-buffer-name directory))))
        (when (buffer-live-p buffer)
          (let ((kill-buffer-hook nil)
                (kill-buffer-query-functions nil))
            (kill-buffer buffer))))
      (codex-ide-debug "Cleaned up Codex session for %s"
                       (file-name-nondirectory (directory-file-name directory))))))

(defun codex-ide--make-env ()
  "Return the list of \"KEY=VALUE\" env vars for a Codex session."
  (list "TERM_PROGRAM=emacs"))

(defun codex-ide--create-session (&optional resume-last session-id)
  "Create a Codex terminal session for the current project.
RESUME-LAST and SESSION-ID are forwarded to `codex-ide--build-command'.
Returns (BUFFER . PROCESS)."
  (codex-ide-term--ensure-backend)
  (let* ((working-dir (codex-ide--get-working-directory))
         (buffer-name (codex-ide--get-buffer-name working-dir))
         (codex-ide-config-overrides (codex-ide--session-config-overrides))
         (cmd (codex-ide--build-command resume-last session-id))
         (program (car cmd))
         (args (cdr cmd))
         (env (codex-ide--make-env))
         (default-directory working-dir))
    (codex-ide-debug "Starting Codex: %s %s"
                     program (string-join args " "))
    (codex-ide-debug "Working directory: %s" working-dir)
    (let ((process (codex-ide-term--make-process
                    buffer-name program args env working-dir)))
      (cons (get-buffer buffer-name) process))))

(defun codex-ide--start-session (&optional resume-last session-id)
  "Start or focus a Codex session for the current project.
If a live session exists, toggle its window instead of starting a new one."
  (unless (codex-ide--ensure-cli)
    (user-error "Codex CLI not available.  Install it and ensure it is in PATH"))
  (codex-ide--cleanup-dead-processes)
  (let* ((working-dir (codex-ide--get-working-directory))
         (buffer-name (codex-ide--get-buffer-name working-dir))
         (existing-buffer (get-buffer buffer-name))
         (existing-process (codex-ide--get-process working-dir)))
    (if (and existing-buffer (buffer-live-p existing-buffer) existing-process)
        (codex-ide--toggle-existing-window existing-buffer)
      (let ((result (codex-ide--create-session resume-last session-id)))
        (let ((buffer (car result))
              (process (cdr result))
              (dir working-dir))
          (puthash working-dir process codex-ide--processes)
          (set-process-query-on-exit-flag process nil)
          (set-process-sentinel
           process
           (lambda (_proc event)
             (codex-ide-debug "Codex process event: %s" (string-trim event))
             (when (string-match-p
                    (rx (or "finished" "exited" "killed" "terminated"))
                    event)
               (codex-ide--cleanup-on-exit dir))))
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (codex-ide--cleanup-on-exit dir))
                      nil t)
            (local-set-key (kbd "S-<return>") #'codex-ide-insert-newline)
            (local-set-key (kbd "C-<escape>") #'codex-ide-send-escape))
          (codex-ide--display-buffer-in-side-window buffer)
          (codex-ide-log "Codex started in %s"
                         (file-name-nondirectory
                          (directory-file-name working-dir))))))))

;;; Commands

;;;###autoload
(defun codex-ide ()
  "Start Codex for the current project, or toggle its window if running."
  (interactive)
  (codex-ide--start-session))

;;;###autoload
(defun codex-ide-resume-last ()
  "Resume the most recent Codex session for the current project."
  (interactive)
  (codex-ide--start-session t))

;;;###autoload
(defun codex-ide-resume ()
  "Resume a Codex session for the current project.
In the MVP this resumes the most recent session via `codex resume --last'.
A session picker is deferred to a later phase."
  (interactive)
  (codex-ide--start-session t))

;;;###autoload
(defun codex-ide-stop ()
  "Stop the Codex session for the current project."
  (interactive)
  (let ((buffer-name (codex-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          (kill-buffer buffer)
          (codex-ide-log "Stopping Codex in %s..."
                         (file-name-nondirectory
                          (directory-file-name
                           (codex-ide--get-working-directory)))))
      (codex-ide-log "No Codex session is running in this directory"))))

;;;###autoload
(defun codex-ide-toggle ()
  "Toggle visibility of the Codex window for the current project."
  (interactive)
  (let ((buffer (get-buffer (codex-ide--get-buffer-name))))
    (if buffer
        (codex-ide--toggle-existing-window buffer)
      (user-error "No Codex session for this project"))))

;;;###autoload
(defun codex-ide-switch-to-buffer ()
  "Switch to the Codex buffer for the current project.
If it is not visible, display it in the configured side window."
  (interactive)
  (if-let ((buffer (get-buffer (codex-ide--get-buffer-name))))
      (if-let ((window (get-buffer-window buffer)))
          (select-window window)
        (codex-ide--display-buffer-in-side-window buffer))
    (user-error
     "No Codex session for this project.  Use M-x codex-ide to start one")))

;;;###autoload
(defun codex-ide-send-prompt (&optional prompt)
  "Send PROMPT to the Codex terminal for the current project.
Interactively, read PROMPT from the minibuffer."
  (interactive)
  (if-let ((buffer (get-buffer (codex-ide--get-buffer-name))))
      (let ((text (or prompt (read-string "Codex prompt: "))))
        (unless (string-empty-p text)
          (with-current-buffer buffer
            (codex-ide-term--send-string text)
            (sit-for 0.1)
            (codex-ide-term--send-return))
          (codex-ide-debug "Sent prompt: %s" text)))
    (user-error "No Codex session for this project")))

;;;###autoload
(defun codex-ide-send-escape ()
  "Send ESC to the Codex terminal for the current project."
  (interactive)
  (if-let ((buffer (get-buffer (codex-ide--get-buffer-name))))
      (with-current-buffer buffer
        (codex-ide-term--send-escape))
    (user-error "No Codex session for this project")))

;;;###autoload
(defun codex-ide-insert-newline ()
  "Insert a literal newline into the Codex prompt.
Sends backslash followed by RET, which Codex interprets as a newline."
  (interactive)
  (if-let ((buffer (get-buffer (codex-ide--get-buffer-name))))
      (with-current-buffer buffer
        (codex-ide-term--send-string "\\")
        (sit-for 0.1)
        (codex-ide-term--send-return))
    (user-error "No Codex session for this project")))

;;;###autoload
(defun codex-ide-check-status ()
  "Check whether the Codex CLI is available and report its version."
  (interactive)
  (codex-ide--detect-cli)
  (if codex-ide--cli-available
      (let ((version (with-temp-buffer
                       (call-process codex-ide-cli-path nil t nil "--version")
                       (string-trim (buffer-string)))))
        (codex-ide-log "Codex CLI version: %s" version))
    (codex-ide-log "Codex CLI is not installed.")))

(provide 'codex-ide)

;;; codex-ide.el ends here
