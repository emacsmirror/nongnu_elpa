;;; codex-ide-transient.el --- Transient menus for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, transient, menu
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

;; Transient menus for `codex-ide', reachable via `M-x codex-ide-menu'.

;;; Code:

(require 'transient)
(require 'codex-ide)
(require 'codex-ide-debug)

;;; Suffixes

(transient-define-suffix codex-ide--set-cli-path (path)
  "Set `codex-ide-cli-path'."
  :description "Set CLI path"
  (interactive (list (read-file-name "Codex CLI path: "
                                     nil codex-ide-cli-path t)))
  (setq codex-ide-cli-path path)
  (codex-ide-log "CLI path set to %s" path))

(transient-define-suffix codex-ide--set-window-side (side)
  "Set `codex-ide-window-side'."
  :description "Set window side"
  (interactive (list (intern (completing-read
                              "Window side: "
                              '("left" "right" "top" "bottom")
                              nil t nil nil
                              (symbol-name codex-ide-window-side)))))
  (setq codex-ide-window-side side)
  (codex-ide-log "Window side set to %s" side))

(transient-define-suffix codex-ide--set-window-width (width)
  "Set `codex-ide-window-width'."
  :description "Set window width"
  (interactive (list (read-number "Window width: "
                                  codex-ide-window-width)))
  (setq codex-ide-window-width width)
  (codex-ide-log "Window width set to %d" width))

(transient-define-suffix codex-ide--set-window-height (height)
  "Set `codex-ide-window-height'."
  :description "Set window height"
  (interactive (list (read-number "Window height: "
                                  codex-ide-window-height)))
  (setq codex-ide-window-height height)
  (codex-ide-log "Window height set to %d" height))

(transient-define-suffix codex-ide--set-backend (backend)
  "Set `codex-ide-terminal-backend'."
  :description "Set terminal backend"
  (interactive (list (intern (completing-read
                              "Terminal backend: "
                              '("vterm" "eat") nil t nil nil
                              (symbol-name codex-ide-terminal-backend)))))
  (setq codex-ide-terminal-backend backend)
  (codex-ide-log "Terminal backend set to %s" backend))

(transient-define-suffix codex-ide--set-approval (policy)
  "Set `codex-ide-ask-for-approval'."
  :description "Set approval policy"
  (interactive (list (intern (completing-read
                              "Approval policy: "
                              '("nil" "untrusted" "on-request" "never")
                              nil t nil nil
                              (if codex-ide-ask-for-approval
                                  (symbol-name codex-ide-ask-for-approval)
                                "nil")))))
  (setq codex-ide-ask-for-approval (unless (eq policy 'nil) policy))
  (codex-ide-log "Approval policy set to %s" policy))

(transient-define-suffix codex-ide--toggle-use-side-window ()
  "Toggle `codex-ide-use-side-window'."
  :description "Toggle side window"
  (interactive)
  (setq codex-ide-use-side-window (not codex-ide-use-side-window))
  (codex-ide-log "Side window %s"
                 (if codex-ide-use-side-window "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-focus-on-open ()
  "Toggle `codex-ide-focus-on-open'."
  :description "Toggle focus on open"
  (interactive)
  (setq codex-ide-focus-on-open (not codex-ide-focus-on-open))
  (codex-ide-log "Focus on open %s"
                 (if codex-ide-focus-on-open "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-no-alt-screen ()
  "Toggle `codex-ide-no-alt-screen'."
  :description "Toggle no-alt-screen"
  (interactive)
  (setq codex-ide-no-alt-screen (not codex-ide-no-alt-screen))
  (codex-ide-log "No-alt-screen %s"
                 (if codex-ide-no-alt-screen "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-debug-mode ()
  "Toggle `codex-ide-debug'."
  :description "Toggle debug mode"
  (interactive)
  (setq codex-ide-debug (not codex-ide-debug))
  (codex-ide-log "Debug mode %s"
                 (if codex-ide-debug "enabled" "disabled")))

(defun codex-ide--save-config ()
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

;;; Menus

;;;###autoload
(transient-define-prefix codex-ide-menu ()
  "codex-ide main menu."
  ["codex-ide"
   ["Session"
    ("s" "Start" codex-ide)
    ("r" "Resume last" codex-ide-resume-last)
    ("R" "Resume" codex-ide-resume)
    ("q" "Stop" codex-ide-stop)]
   ["Navigation"
    ("b" "Switch to buffer" codex-ide-switch-to-buffer)
    ("w" "Toggle window" codex-ide-toggle)]
   ["Interaction"
    ("p" "Send prompt" codex-ide-send-prompt)
    ("e" "Send escape" codex-ide-send-escape)
    ("n" "Insert newline" codex-ide-insert-newline)]
   ["Submenus"
    ("C" "Configuration" codex-ide-config-menu)
    ("d" "Debug" codex-ide-debug-menu)]])

(transient-define-prefix codex-ide-config-menu ()
  "codex-ide configuration menu."
  ["codex-ide Configuration"
   ["Window"
    ("s" codex-ide--set-window-side)
    ("w" codex-ide--set-window-width)
    ("h" codex-ide--set-window-height)
    ("u" codex-ide--toggle-use-side-window
     :description (lambda () (format "Use side window (%s)"
                                     (if codex-ide-use-side-window "ON" "OFF"))))
    ("f" codex-ide--toggle-focus-on-open
     :description (lambda () (format "Focus on open (%s)"
                                     (if codex-ide-focus-on-open "ON" "OFF"))))]
   ["CLI"
    ("p" codex-ide--set-cli-path)
    ("b" codex-ide--set-backend)
    ("a" codex-ide--set-approval)
    ("A" codex-ide--toggle-no-alt-screen
     :description (lambda () (format "No-alt-screen (%s)"
                                     (if codex-ide-no-alt-screen "ON" "OFF"))))]]
  ["Save"
   ("S" "Save configuration" codex-ide--save-config)])

(transient-define-prefix codex-ide-debug-menu ()
  "codex-ide debug menu."
  ["codex-ide Debug"
   ["Status"
    ("S" "Check CLI status" codex-ide-check-status)]
   ["Settings"
    ("d" codex-ide--toggle-debug-mode
     :description (lambda () (format "Debug mode (%s)"
                                     (if codex-ide-debug "ON" "OFF"))))]
   ["Logs"
    ("l" "Show debug log" codex-ide-show-debug)
    ("c" "Clear debug log" codex-ide-clear-debug)]])

(provide 'codex-ide-transient)

;;; codex-ide-transient.el ends here
