;;; codex-ide-mcp.el --- Local MCP tools bridge for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, codex, tools, mcp
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

;; Local streamable-HTTP MCP endpoint for Codex sessions.  The bridge exposes
;; Emacs tools for buffer metadata, navigation, diagnostics, tree-sitter
;; inspection, and, unless disabled, Emacs Lisp evaluation.
;; Codex sessions register it by adding a transient
;; `-c mcp_servers.emacs_tools.url' override; this module never writes to
;; `~/.codex/config.toml' unless `codex-ide-mcp-install-codex-config' is run.
;;
;; Usage:
;;   M-x codex-ide
;;   (setq codex-ide-mcp-enabled nil) ; disable automatic registration
;;   (setq codex-ide-mcp-enable-execute nil) ; hide emacs_execute
;;
;; Commands:
;;   M-x codex-ide-mcp-start
;;   M-x codex-ide-mcp-stop
;;   M-x codex-ide-mcp-status
;;   M-x codex-ide-mcp-install-codex-config

;;; Code:

(require 'subr-x)
(require 'codex-ide-debug)
(require 'codex-ide-mcp-core)
(require 'codex-ide-mcp-treesit)
(require 'codex-ide-mcp-tools)
(require 'codex-ide-mcp-protocol)
(require 'codex-ide-mcp-server)

;;; Public commands

(defun codex-ide-mcp--setup-message (url)
  "Return user-facing persistent setup text for URL."
  (string-join
   (delq nil
         (list (format "Persistent setup command: %s"
                       (codex-ide-mcp--install-command url))
               (codex-ide-mcp--persistent-warning)))
   "\n"))

(defun codex-ide-mcp--status-message ()
  "Return user-facing MCP server status text."
  (let* ((running (codex-ide-mcp--running-p))
         (url (and running (codex-ide-mcp--url))))
    (string-join
     (delq nil
           (list (if running
                     "Codex MCP tools server: running"
                   "Codex MCP tools server: stopped")
                 (when url (format "URL: %s" url))
                 (format "Port: %s"
                         (if (codex-ide-mcp--ephemeral-port-p)
                             "ephemeral"
                           "fixed"))
                 (format "Server key: %s" codex-ide-mcp--server-name)
                 (format "Codex namespace: %s"
                         (codex-ide-mcp--namespace))
                 (format "Example tool: %s"
                         (codex-ide-mcp--callable-tool-name
                          "emacs_current_buffer"))
                 (format "emacs_execute: %s"
                         (if codex-ide-mcp-enable-execute
                             "enabled"
                           "disabled"))
                 (when url (codex-ide-mcp--setup-message url))))
     "\n")))

(defun codex-ide-mcp--run-install-command (args)
  "Run the Codex MCP add command described by ARGS."
  (with-current-buffer (get-buffer-create "*codex-ide-mcp-install*")
    (erase-buffer)
    (let ((status (apply #'call-process (car args) nil t nil (cdr args))))
      (unless (eq status 0)
        (error "Codex MCP config command failed with status %s" status))
      status)))

;;;###autoload
(defun codex-ide-mcp-start ()
  "Start the local Codex MCP tools server."
  (interactive)
  (codex-ide-mcp-ensure-server)
  (codex-ide-log "Codex MCP tools server started on %s"
                 (codex-ide-mcp--url)))

;;;###autoload
(defun codex-ide-mcp-stop ()
  "Stop the local Codex MCP tools server."
  (interactive)
  (codex-ide-mcp--stop-server)
  (codex-ide-log "Codex MCP tools server stopped"))

;;;###autoload
(defun codex-ide-mcp-status ()
  "Report the local Codex MCP tools server status."
  (interactive)
  (let ((status (codex-ide-mcp--status-message)))
    (codex-ide-log "%s" status)
    status))

;;;###autoload
(defun codex-ide-mcp-install-codex-config ()
  "Add the running Emacs MCP server to Codex config after confirmation."
  (interactive)
  (let* ((url (codex-ide-mcp-ensure-server))
         (args (codex-ide-mcp--install-command-args url))
         (command (codex-ide-mcp--install-command url))
         (setup (codex-ide-mcp--setup-message url)))
    (codex-ide-log "%s" setup)
    (if (y-or-n-p (format "Run `%s'? " command))
        (progn
          (codex-ide-mcp--run-install-command args)
          (codex-ide-log "Installed Codex MCP config with `%s'" command)
          command)
      command)))

(provide 'codex-ide-mcp)

;;; codex-ide-mcp.el ends here
