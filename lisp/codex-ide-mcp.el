;;; codex-ide-mcp.el --- Local MCP tools bridge for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, mcp
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

;; GNU Emacs uses a local streamable-HTTP endpoint.  Neomacs instead uses
;; its native tool catalog and stdio relay.  The GNU bridge exposes
;; Emacs tools for harness context, live-buffer editing, async jobs, recent
;; events, and Emacs Lisp evaluation.
;; Codex sessions register it by adding a transient
;; `-c mcp_servers.emacs_tools.url' override; this module never writes to
;; `~/.codex/config.toml' unless `codex-ide-mcp-install-codex-config' is run.
;;
;; Usage:
;;   M-x codex-ide
;;   (setq codex-ide-mcp-enabled nil) ; disable automatic registration
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
(require 'codex-ide-mcp-neo)

;;; Public commands

(defun codex-ide-mcp--setup-message (url)
  "Return user-facing persistent setup text for URL."
  (ignore url)
  "GNU MCP requires a session bearer token; start Codex with M-x codex-ide.  Persistent HTTP installation is unavailable.")

(defun codex-ide-mcp--status-message ()
  "Return user-facing MCP server status text."
  (if (featurep 'neomacs)
      (codex-ide-mcp-neo--status-message)
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
                            "emacs_context"))
                   (format "Harness tools: %s"
                           (string-join (codex-ide-mcp-tool-names) ", "))
                   (when url (codex-ide-mcp--setup-message url))))
       "\n"))))

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
  "Start GNU MCP, or verify the existing native Neomacs endpoint."
  (interactive)
  (codex-ide-log "Codex MCP endpoint: %s" (codex-ide-mcp-ensure-server)))

;;;###autoload
(defun codex-ide-mcp-stop ()
  "Stop the GNU MCP server; refuse to stop Neomacs' shared listener."
  (interactive)
  (when (featurep 'neomacs)
    (user-error "Native MCP is shared; use neomacs-mcp-stop explicitly to stop it"))
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
  "Add the running Emacs MCP server to Codex config after confirmation.
Native setup can replace an existing entry and discard its tool policy."
  (interactive)
  (unless (featurep 'neomacs)
    (user-error "GNU MCP requires a session bearer token; start Codex with M-x codex-ide"))
  (let* ((url (codex-ide-mcp-ensure-server))
         (args (if (featurep 'neomacs)
                   (codex-ide-mcp-neo--install-command-args url)
                 (codex-ide-mcp--install-command-args url)))
         (command (if (featurep 'neomacs)
                      (mapconcat #'shell-quote-argument args " ")
                    (codex-ide-mcp--install-command url)))
         (setup (if (featurep 'neomacs)
                    (concat command "\nPersistent setup targets this exact native socket.\n"
                            "This can replace mcp_servers.neomacs and discard enabled, enabled_tools and disabled_tools policy.")
                  (codex-ide-mcp--setup-message url))))
    (codex-ide-log "%s" setup)
    (if (y-or-n-p (if (featurep 'neomacs)
                     (format "Run `%s' (can replace neomacs and discard enabled, enabled_tools and disabled_tools policy)? " command)
                   (format "Run `%s'? " command)))
        (progn
          (codex-ide-mcp--run-install-command args)
          (codex-ide-log "Installed Codex MCP config with `%s'" command)
          command)
      command)))

(provide 'codex-ide-mcp)

;;; codex-ide-mcp.el ends here
