;;; codex-ide-menu.el --- Keymap-popup menus for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, keymap-popup, menu
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

;; Popup menus for `codex-ide', reachable via `M-x codex-ide-menu'.

;;; Code:

(require 'keymap-popup)
(require 'codex-ide)
(require 'codex-ide-debug)

;;; Suffixes

(defun codex-ide-menu--set-cli-path (path)
  "Set `codex-ide-cli-path' to PATH."
  (interactive
   (list (read-file-name "Codex CLI path: " nil codex-ide-cli-path t)))
  (setq codex-ide-cli-path path)
  (codex-ide-log "CLI path set to %s" path))

(defun codex-ide-menu--set-window-side (side)
  "Set `codex-ide-window-side' to SIDE."
  (interactive
   (list (intern (completing-read
                 "Window side: "
                 '("left" "right" "top" "bottom")
                 nil t nil nil
                 (symbol-name codex-ide-window-side)))))
  (setq codex-ide-window-side side)
  (codex-ide-log "Window side set to %s" side))

(defun codex-ide-menu--set-window-width (width)
  "Set `codex-ide-window-width' to WIDTH."
  (interactive
   (list (read-number "Window width: " codex-ide-window-width)))
  (setq codex-ide-window-width width)
  (codex-ide-log "Window width set to %d" width))

(defun codex-ide-menu--set-window-height (height)
  "Set `codex-ide-window-height' to HEIGHT."
  (interactive
   (list (read-number "Window height: " codex-ide-window-height)))
  (setq codex-ide-window-height height)
  (codex-ide-log "Window height set to %d" height))

(defun codex-ide-menu--set-backend (backend)
  "Set `codex-ide-terminal-backend' to BACKEND."
  (interactive
   (list (intern (completing-read
                 "Terminal backend: "
                 '("vterm" "eat") nil t nil nil
                 (symbol-name codex-ide-terminal-backend)))))
  (setq codex-ide-terminal-backend backend)
  (codex-ide-log "Terminal backend set to %s" backend))

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

(defun codex-ide-menu--toggle-use-side-window ()
  "Toggle `codex-ide-use-side-window'."
  (interactive)
  (setq codex-ide-use-side-window (not codex-ide-use-side-window))
  (codex-ide-log "Side window %s"
                 (if codex-ide-use-side-window "enabled" "disabled")))

(defun codex-ide-menu--toggle-focus-on-open ()
  "Toggle `codex-ide-focus-on-open'."
  (interactive)
  (setq codex-ide-focus-on-open (not codex-ide-focus-on-open))
  (codex-ide-log "Focus on open %s"
                 (if codex-ide-focus-on-open "enabled" "disabled")))

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
  "Save the current configuration to the custom file."
  (interactive)
  (mapc (lambda (symbol)
          (customize-save-variable symbol (symbol-value symbol)))
        '(codex-ide-cli-path
          codex-ide-terminal-backend
          codex-ide-window-side
          codex-ide-window-width
          codex-ide-window-height
          codex-ide-use-side-window
          codex-ide-focus-on-open
          codex-ide-ask-for-approval
          codex-ide-no-alt-screen))
  (codex-ide-log "Configuration saved"))

(defun codex-ide-menu--on-off (value)
  "Return \"ON\" or \"OFF\" for VALUE."
  (if value "ON" "OFF"))

;;; Menus

(defvar codex-ide-config-map)
(defvar codex-ide-debug-map)
(defvar codex-ide-map)

(keymap-popup-define codex-ide-config-map
  "codex-ide Configuration"
  :popup-key "?"
  :description "codex-ide Configuration"
  :group "Window"
  "s" ("Set window side" codex-ide-menu--set-window-side)
  "w" ("Set window width" codex-ide-menu--set-window-width)
  "h" ("Set window height" codex-ide-menu--set-window-height)
  "u" ((lambda () (format "Use side window (%s)"
                          (codex-ide-menu--on-off codex-ide-use-side-window)))
       codex-ide-menu--toggle-use-side-window)
  "f" ((lambda () (format "Focus on open (%s)"
                          (codex-ide-menu--on-off codex-ide-focus-on-open)))
       codex-ide-menu--toggle-focus-on-open)
  :group "CLI"
  "p" ("Set CLI path" codex-ide-menu--set-cli-path)
  "b" ("Set backend" codex-ide-menu--set-backend)
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

(keymap-popup-define codex-ide-map
  "codex-ide"
  :popup-key "?"
  :description "codex-ide Menu"
  :group "Session"
  "s" ("Start" codex-ide)
  "r" ("Resume last" codex-ide-resume-last)
  "R" ("Resume" codex-ide-resume)
  "q" ("Stop" codex-ide-stop)
  :group "Navigation"
  "b" ("Switch to buffer" codex-ide-switch-to-buffer)
  "l" ("List sessions" codex-ide-list-sessions)
  "w" ("Toggle window" codex-ide-toggle)
  :group "Interaction"
  "p" ("Send prompt" codex-ide-send-prompt)
  "e" ("Send escape" codex-ide-send-escape)
  "n" ("Insert newline" codex-ide-insert-newline)
  :group "Submenus"
  "C" ("Configuration" :keymap codex-ide-config-map)
  "d" ("Debug" :keymap codex-ide-debug-map))

;;;###autoload
(defun codex-ide-menu ()
  "Open the codex-ide popup menu."
  (interactive)
  (keymap-popup codex-ide-map))

(provide 'codex-ide-menu)

;;; codex-ide-menu.el ends here
