;;; codex-ide-menu.el --- Keymap-popup menus for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, keymap-popup, menu, tools
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

;; Popup menus for `codex-ide', reachable via `M-x codex-ide-menu'.

;;; Code:

(require 'keymap-popup)
(require 'codex-ide)
(require 'codex-ide-debug)
(require 'codex-ide-mcp)
(require 'codex-ide-sessions)

(declare-function custom-file "cus-edit" (&optional no-error))
(declare-function custom-save-all "cus-edit" ())

;;; Suffixes

(defconst codex-ide-menu--saved-config-symbols
  '(codex-ide-cli-path
    codex-ide-terminal-backend
    codex-ide-display-buffer-function
    codex-ide-ask-for-approval
    codex-ide-no-alt-screen
    codex-ide-cli-extra-args
    codex-ide-config-overrides
    codex-ide-debug
    codex-ide-yolo
    codex-ide-mcp-enabled
    codex-ide-mcp-host
    codex-ide-mcp-port
    codex-ide-context-auto-start)
  "Symbols persisted by `codex-ide-menu--save-config'.")

(defun codex-ide-menu--set-cli-path (path)
  "Set `codex-ide-cli-path' to PATH."
  (interactive
   (list (read-file-name "Codex CLI path: " nil
                         (or (executable-find "codex") codex-ide-cli-path)
                         nil)))
  (setq codex-ide-cli-path path)
  (codex-ide--invalidate-cli-cache)
  (codex-ide-log "CLI path set to %s" path))

(defun codex-ide-menu--set-approval (policy)
  "Set `codex-ide-ask-for-approval' to POLICY."
  (interactive
   (list (intern (completing-read
                  "Approval policy: "
                  '("nil" "on-request" "never")
                  nil t nil nil
                  (if codex-ide-ask-for-approval
                      (symbol-name codex-ide-ask-for-approval)
                    "nil")))))
  (setq codex-ide-ask-for-approval (unless (eq policy 'nil) policy))
  (codex-ide-log "Approval policy set to %s" policy))

(defun codex-ide-menu--set-terminal-backend (backend)
  "Use BACKEND for new Codex sessions."
  (interactive
   (list (intern (completing-read
                  "Terminal backend: " '("eat" "vterm" "neo-term") nil t nil nil
                  (symbol-name (codex-ide-term--selected-backend))))))
  (customize-set-variable 'codex-ide-terminal-backend backend)
  (codex-ide-log "Terminal backend set to %s" backend))


(defun codex-ide-menu--save-config ()
  "Save nondefault options and remove saved overrides restored to defaults.
Consider only `codex-ide-menu--saved-config-symbols'."
  (interactive)
  (require 'cus-edit)
  (let (reset)
    (dolist (symbol codex-ide-menu--saved-config-symbols)
      (if (equal (symbol-value symbol)
                 (eval (car (get symbol 'standard-value)) t))
          (when (or (get symbol 'saved-value)
                    (get symbol 'saved-variable-comment))
            (put symbol 'saved-value nil)
            (put symbol 'saved-variable-comment nil)
            ;; Retain explicit runtime choices, such as Eat on Neomacs,
            ;; while omitting the default value from future startup files.
            (customize-set-variable symbol (symbol-value symbol))
            (setq reset t))
        (customize-save-variable symbol (symbol-value symbol))))
    (when (and reset (custom-file t)) (custom-save-all)))
  (codex-ide-log "Configuration saved"))

(defun codex-ide-menu--value (label value)
  "Return LABEL with a compact, propertized VALUE and full hover text."
  (let ((text (format "%s" value)))
    (concat label ": "
            (propertize (truncate-string-to-width text 22 nil nil t)
                        'face 'keymap-popup-value 'help-echo text))))

(defun codex-ide-menu--no-project-session-p ()
  "Return non-nil when the current project has no live session."
  (null (codex-ide--project-sessions (codex-ide--get-working-directory))))

(defun codex-ide-menu--no-session-p ()
  "Return non-nil when no registered session is live."
  (not (cl-some #'codex-ide--session-live-p (codex-ide--raw-sessions))))

(defun codex-ide-menu--sandbox-description ()
  "Describe configured sandbox overrides for new sessions."
  (codex-ide-menu--value
   "Sandbox"
   (if codex-ide-yolo "disabled (YOLO)"
     (or (cdr (assoc "sandbox_mode" codex-ide-config-overrides))
         (cdr (assoc "sandbox_permissions" codex-ide-config-overrides))
         "CLI default"))))

(defun codex-ide-menu--configure-sandbox ()
  "Customize CLI configuration overrides, including sandbox settings."
  (interactive)
  (customize-variable 'codex-ide-config-overrides))

;;; Menus

(defvar codex-ide-config-map)
(defvar codex-ide-debug-map)
(defvar codex-ide-mcp-map)
(defvar codex-ide-map)

(keymap-popup-define codex-ide-config-map
  "codex-ide Configuration"
  :popup-key "?"
  :description "codex-ide Configuration"
  :group "CLI"
  "p" ("Set CLI path" codex-ide-menu--set-cli-path)
  "t" ((lambda () (codex-ide-menu--value
                    "Backend" (codex-ide-term--selected-backend)))
       codex-ide-menu--set-terminal-backend)
  "a" ((lambda () (codex-ide-menu--value
                    "Approval" (if codex-ide-yolo "never (YOLO)"
                                 (or codex-ide-ask-for-approval "CLI default"))))
       codex-ide-menu--set-approval)
  "s" (#'codex-ide-menu--sandbox-description codex-ide-menu--configure-sandbox)
  :group "Options"
  "y" ("YOLO full control" :switch codex-ide-yolo)
  "A" ("No-alt-screen" :switch codex-ide-no-alt-screen)
  "S" ("Save configuration" codex-ide-menu--save-config))

(keymap-popup-define codex-ide-debug-map
  "codex-ide Debug"
  :popup-key "?"
  :description "codex-ide Debug"
  :group "Status"
  "S" ("Check CLI status" codex-ide-check-status)
  :group "Settings"
  "d" ("Debug mode" :switch codex-ide-debug)
  :group "Logs"
  "l" ("Show debug log" codex-ide-show-debug)
  "c" ("Clear debug log" codex-ide-clear-debug))

(keymap-popup-define codex-ide-mcp-map
  "codex-ide MCP"
  :popup-key "?"
  :description "codex-ide MCP"
  :group "Server"
  "s" ("Start MCP server" codex-ide-mcp-start
       :inapt-if (lambda () (process-live-p codex-ide-mcp--server)))
  "K" ("Stop MCP server" codex-ide-mcp-stop
       :inapt-if (lambda () (not (process-live-p codex-ide-mcp--server))))
  "S" ("MCP status" codex-ide-mcp-status)
  "i" ("Install Codex MCP config" codex-ide-mcp-install-codex-config))

(keymap-popup-define codex-ide-map
  "codex-ide"
  :popup-key "?"
  :description "codex-ide Menu"
  :group "Session"
  "s" ("Start" codex-ide :c-u "C-u: new session")
  "r" ("Resume last" codex-ide-resume-last)
  "R" ("Resume saved session" codex-ide-resume)
  "k" ("Kill session" codex-ide-stop
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "N" ("Rename session" codex-ide-rename-session
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "B" ("Session list" codex-ide-list-sessions)
  :group "Switch"
  "b" ("Project buffer" codex-ide-switch-to-buffer
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "l" ("Project session" codex-ide-switch-project-session
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "L" ("Any session" codex-ide-switch-session
       :inapt-if #'codex-ide-menu--no-session-p)
  "w" ("Cycle sessions" codex-ide-cycle-session
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "W" ("Toggle project panel" codex-ide-toggle-panel
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "A" ("Show project panels" codex-ide-show-project-sessions
       :inapt-if #'codex-ide-menu--no-project-session-p)
  :row
  :group "Send"
  "p" ("Send prompt" codex-ide-send-prompt
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "e" ("Send escape" codex-ide-send-escape
       :inapt-if #'codex-ide-menu--no-project-session-p)
  :group "Context"
  "a" ("Attach source to draft" codex-ide-attach-source
       :inapt-if #'codex-ide-menu--no-project-session-p)
  "c" ("Start context server" codex-ide-context-start
       :inapt-if (lambda () (process-live-p codex-ide-context--server)))
  "x" ("Stop context server" codex-ide-context-stop
       :inapt-if (lambda () (not (process-live-p codex-ide-context--server))))
  "S" ("Context status" codex-ide-context-status)
  :row
  :group "Config"
  "C" ("Configuration" :keymap codex-ide-config-map)
  "m" ("MCP" :keymap codex-ide-mcp-map)
  "d" ("Debug" :keymap codex-ide-debug-map))

;;;###autoload
(defun codex-ide-menu ()
  "Open the codex-ide popup menu."
  (interactive)
  (keymap-popup codex-ide-map))

(provide 'codex-ide-menu)

;;; codex-ide-menu.el ends here
