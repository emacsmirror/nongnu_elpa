;;; codex-ide-mcp-neo.el --- Native Neomacs MCP registration -*- lexical-binding: t; -*-

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

;; Register the existing owner-controlled native endpoint via its stdio relay.
;; Do not start a second GNU MCP server or replace the native tool catalog.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-mcp-core)

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-args)

(defvar neomacs-mcp--listener)
(defvar neomacs-mcp--socket)
(defvar neomacs-mcp-tools)

(defcustom codex-ide-neomacs-mcp-command "neomacs-mcp"
  "Native MCP stdio relay executable.
Resolve it on the variable `exec-path', or set an absolute filename."
  :type 'string
  :group 'codex-ide)

(defun codex-ide-mcp-neo--socket ()
  "Return the active native MCP socket, or nil when it is not listening."
  (when (and (require 'neomacs-mcp nil t)
             (require 'neomacs-mcp-editor nil t)
             (require 'neomacs-mcp-companion nil t)
             (process-live-p neomacs-mcp--listener)
             (stringp neomacs-mcp--socket)
             (file-name-absolute-p neomacs-mcp--socket)
             (not (file-remote-p neomacs-mcp--socket))
             (equal neomacs-mcp--socket
                    (process-contact neomacs-mcp--listener :service)))
    neomacs-mcp--socket))

(defun codex-ide-mcp-neo--ensure-server ()
  "Return the active owner-controlled native MCP endpoint.
Refuse missing native support rather than silently using GNU MCP."
  (or (codex-ide-mcp-neo--socket)
      (user-error "Start the native endpoint with neomacs-mcp-start before launching Codex")))

(defun codex-ide-mcp-neo--command ()
  "Return the resolved native MCP stdio relay executable."
  (or (executable-find codex-ide-neomacs-mcp-command)
      (user-error "Native MCP relay not found: %s" codex-ide-neomacs-mcp-command)))

(defvar codex-ide-mcp-neo--config-cache nil
  "Last native config inspection key and server entries.")

(defun codex-ide-mcp-neo--config-key (overrides)
  "Return the identities affecting native config inspection with OVERRIDES."
  (let ((home (or (getenv "CODEX_HOME") (expand-file-name "~/.codex"))))
    (list codex-ide-cli-path (executable-find codex-ide-cli-path)
          overrides default-directory home
          (mapcar (lambda (path)
                    (list path (file-attribute-modification-time
                                (file-attributes path))))
                  (list (expand-file-name "config.toml" home)
                        (expand-file-name ".codex/config.toml" default-directory))))))

(defun codex-ide-mcp-neo--configured-servers (overrides)
  "Read effective MCP entries, caching by config files and OVERRIDES."
  (let ((key (codex-ide-mcp-neo--config-key overrides)))
    (unless (equal key (car codex-ide-mcp-neo--config-cache))
      (setq codex-ide-mcp-neo--config-cache
            (cons (copy-tree key) (codex-ide-mcp-neo--read-servers overrides))))
    (copy-tree (cdr codex-ide-mcp-neo--config-cache))))

(defun codex-ide-mcp-neo--read-servers (overrides)
  "Read effective Codex MCP entries with user OVERRIDES, without config writes.
Ask the CLI to merge its own config layers rather than parsing TOML here."
  (with-temp-buffer
    (let* ((args (append
                  (mapcan (lambda (pair)
                            (list "-c" (format "%s=%s" (car pair) (cdr pair))))
                          overrides)
                  '("mcp" "list" "--json")))
           (status (apply #'call-process codex-ide-cli-path nil
                          (list (current-buffer) nil) nil args)))
      (unless (eq status 0)
        (user-error "Cannot inspect Codex MCP config; run `%s mcp list --json' or set codex-ide-mcp-enabled to nil"
                    codex-ide-cli-path))
      (goto-char (point-min))
      (condition-case nil
          (json-parse-buffer :object-type 'alist :array-type 'list)
        (error (user-error "Codex MCP config inspection returned invalid JSON"))))))

(defun codex-ide-mcp-neo--config-overrides (socket &optional overrides)
  "Return transient Codex stdio config overrides for native SOCKET.
Inspect effective user OVERRIDES first.  Refuse a conflicting HTTP entry;
retain inherited native enablement and tool policy.  Disable any inherited
GNU emacs_tools entry only for this session, unless OVERRIDES explicitly
sets its enabled key.  Refuse extra CLI config selectors and remote routes
that the local config inspection cannot account for."
  (when (seq-some
         (lambda (arg)
           (string-match-p
            "\\`\\(?:-[pcC]\\|--\\(?:profile\\|config\\|cd\\|remote\\|remote-auth-token-env\\)\\(?:=\\|\\'\\)\\)"
            arg))
         codex-ide-cli-extra-args)
    (user-error "Native MCP cannot inspect profile/config/cwd or remote options in codex-ide-cli-extra-args; move session settings to codex-ide-config-overrides and select the project directory in Emacs, or set codex-ide-mcp-enabled to nil"))
  (let* ((command (codex-ide-mcp-neo--command))
         (servers (codex-ide-mcp-neo--configured-servers overrides))
         (native (seq-find (lambda (entry)
                             (equal (alist-get 'name entry) "neomacs")) servers)))
    (when (equal (alist-get 'type (alist-get 'transport native)) "streamable_http")
      (user-error "Codex mcp_servers.neomacs uses HTTP; rename that entry or explicitly replace it with stdio, or set codex-ide-mcp-enabled to nil; no config was changed"))
    (append
     (list (cons "mcp_servers.neomacs.command"
                 (codex-ide-mcp--toml-quote-string command))
           (cons "mcp_servers.neomacs.args"
                 (format "[\"--socket\", %s]"
                         (codex-ide-mcp--toml-quote-string socket))))
     (when (and (not (assoc "mcp_servers.emacs_tools.enabled" overrides))
                (seq-some (lambda (entry)
                            (equal (alist-get 'name entry) "emacs_tools")) servers))
       '(("mcp_servers.emacs_tools.enabled" . "false"))))))

(defun codex-ide-mcp-neo--install-command-args (socket)
  "Return the explicit persistent Codex setup argv for native SOCKET."
  (list codex-ide-cli-path "mcp" "add" "neomacs" "--"
        (codex-ide-mcp-neo--command) "--socket" socket))

(defun codex-ide-mcp-neo--status-message ()
  "Return the native endpoint status and its actual tool catalog."
  (let ((socket (codex-ide-mcp-neo--socket)))
    (string-join
     (delq nil
           (list (if socket "Native Neomacs MCP: running" "Native Neomacs MCP: stopped")
                 (when socket (concat "Socket: " socket))
                 "Codex namespace: mcp__neomacs"
                 (when socket (concat "Native tools: "
                                      (string-join (mapcar #'car neomacs-mcp-tools) ", ")))
                 "The native listener is shared and owner-controlled; Codex IDE does not stop it."))
     "\n")))

(provide 'codex-ide-mcp-neo)

;; Local Variables:
;; package-lint-main-file: "codex-ide.el"
;; End:

;;; codex-ide-mcp-neo.el ends here
