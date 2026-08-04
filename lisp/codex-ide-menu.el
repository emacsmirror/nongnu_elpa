;;; codex-ide-menu.el --- Keymap-popup menus for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, keymap-popup, menu
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

;;; Suffixes

(defconst codex-ide-menu--saved-config-symbols
  '(codex-ide-cli-path
    codex-ide-display-buffer-function
    codex-ide-ask-for-approval
    codex-ide-no-alt-screen
    codex-ide-cli-extra-args
    codex-ide-config-overrides
    codex-ide-debug
    codex-ide-mcp-enabled
    codex-ide-mcp-host
    codex-ide-mcp-port
    codex-ide-context-auto-start)
  "Symbols persisted by `codex-ide-menu--save-config'.")

(defun codex-ide-menu--set-cli-path (path)
  "Set `codex-ide-cli-path' to PATH."
  (interactive
   (list (read-file-name "Codex CLI path: " nil codex-ide-cli-path t)))
  (setq codex-ide-cli-path path)
  (codex-ide--invalidate-cli-cache)
  (codex-ide-log "CLI path set to %s" path))

(defun codex-ide-menu--set-approval (policy)
  "Set `codex-ide-ask-for-approval' to POLICY."
  (interactive
   (list (intern (completing-read
                  "Approval policy: "
                  '("nil" "untrusted" "on-request" "never")
                  nil t nil nil
                  (if codex-ide-ask-for-approval
                      (symbol-name codex-ide-ask-for-approval)
                    "nil")))))
  (setq codex-ide-ask-for-approval (unless (eq policy 'nil) policy))
  (codex-ide-log "Approval policy set to %s" policy))

(defun codex-ide-menu--toggle-no-alt-screen ()
  "Toggle `codex-ide-no-alt-screen'."
  (interactive)
  (setq codex-ide-no-alt-screen (not codex-ide-no-alt-screen))
  (codex-ide-log "No-alt-screen %s"
                 (if codex-ide-no-alt-screen "enabled" "disabled")))

(defun codex-ide-menu--toggle-debug-mode ()
  "Toggle `codex-ide-debug'."
  (interactive)
  (setq codex-ide-debug (not codex-ide-debug))
  (codex-ide-log "Debug mode %s"
                 (if codex-ide-debug "enabled" "disabled")))

(defun codex-ide-menu--save-config ()
  "Save the documented configuration symbols to the custom file.
Persists `codex-ide-menu--saved-config-symbols' only: CLI path, display
function, approval, no-alt-screen, extra args, config overrides, debug,
MCP enable/host/port, and context auto-start."
  (interactive)
  (mapc (lambda (symbol)
          (customize-save-variable symbol (symbol-value symbol)))
        codex-ide-menu--saved-config-symbols)
  (codex-ide-log "Configuration saved"))

(defun codex-ide-menu--on-off (value)
  "Return \"ON\" or \"OFF\" for VALUE."
  (if value "ON" "OFF"))

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
  "a" ("Set approval policy" codex-ide-menu--set-approval)
  "A" ((lambda () (format "No-alt-screen (%s)"
                          (codex-ide-menu--on-off codex-ide-no-alt-screen)))
       codex-ide-menu--toggle-no-alt-screen)
  :group "Save"
  "S" ("Save configuration" codex-ide-menu--save-config))

(keymap-popup-define codex-ide-debug-map
  "codex-ide Debug"
  :popup-key "?"
  :description "codex-ide Debug"
  :group "Status"
  "S" ("Check CLI status" codex-ide-check-status)
  :group "Settings"
  "d" ((lambda () (format "Debug mode (%s)"
                          (codex-ide-menu--on-off codex-ide-debug)))
       codex-ide-menu--toggle-debug-mode)
  :group "Logs"
  "l" ("Show debug log" codex-ide-show-debug)
  "c" ("Clear debug log" codex-ide-clear-debug))

(keymap-popup-define codex-ide-mcp-map
  "codex-ide MCP"
  :popup-key "?"
  :description "codex-ide MCP"
  :group "Server"
  "s" ("Start MCP server" codex-ide-mcp-start)
  "q" ("Stop MCP server" codex-ide-mcp-stop)
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
  "q" ("Stop active session" codex-ide-stop)
  :group "Navigation"
  "b" ("Switch to buffer" codex-ide-switch-to-buffer)
  "C-l" ("Switch project session" codex-ide-list-project-sessions)
  "l" ("Switch any session" codex-ide-list-sessions)
  "w" ("Cycle sessions" codex-ide-toggle)
  :group "Interaction"
  "p" ("Send prompt" codex-ide-send-prompt)
  "e" ("Send escape" codex-ide-send-escape)
  "n" ("Insert newline" codex-ide-insert-newline)
  :group "Submenus"
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
