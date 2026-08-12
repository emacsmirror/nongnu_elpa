;;; hermes-command-palette.el --- Unified Hermes command palette  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; One completing-read entry point for the primary Hermes surfaces.

;;; Code:

(require 'seq)

(defconst hermes-command-palette-commands
  '(("New chat" . hermes-chat)
    ("Switch chat" . hermes-switch-to-chat)
    ("Sessions" . hermes-list-sessions)
    ("Profiles" . hermes-list-profiles)
    ("Messaging" . hermes-list-messaging-platforms)
    ("Cron jobs" . hermes-list-crons)
    ("Kanban" . hermes-list-kanban)
    ("MCP servers" . hermes-list-mcp)
    ("Configuration" . hermes-config)
    ("Gateway status" . hermes-system-status)
    ("Gateway logs" . hermes-system-logs)
    ("Connect provider" . hermes-onboarding-connect-provider))
  "Primary Hermes commands exposed by `hermes-command-palette'.")

(defun hermes-command-palette--dispatch (command)
  "Invoke interactive COMMAND selected from the Hermes palette."
  (unless (commandp command)
    (user-error "Hermes command is unavailable: %s" command))
  (call-interactively command))

;;;###autoload
(defun hermes-command-palette ()
  "Select and invoke a primary Hermes command."
  (interactive)
  (let* ((available
          (seq-filter (lambda (entry) (commandp (cdr entry)))
                      hermes-command-palette-commands))
         (choice (completing-read "Hermes command: " available nil t)))
    (hermes-command-palette--dispatch (cdr (assoc choice available)))))

(provide 'hermes-command-palette)
;;; hermes-command-palette.el ends here
