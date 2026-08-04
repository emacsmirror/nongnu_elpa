;;; hermes-command-palette.el --- Unified Hermes command palette  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

;;; Commentary:

;; One completing-read entry point for the primary Hermes surfaces.

;;; Code:

(require 'seq)

(defconst hermes-command-palette-commands
  '(("New chat" . hermes-chat)
    ("Switch chat" . hermes-switch-to-chat)
    ("Sessions" . hermes-list-sessions)
    ("Profiles" . hermes-list-profiles)
    ("Cron jobs" . hermes-list-crons)
    ("Kanban" . hermes-list-kanban)
    ("MCP servers" . hermes-list-mcp)
    ("Configuration" . hermes-config)
    ("Gateway status" . hermes-system-status)
    ("Gateway logs" . hermes-system-logs)
    ("Connect provider" . hermes-onboarding-connect-provider)
    ("Connect OAuth provider" . hermes-onboarding-connect-oauth))
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
