;;; codex-ide-mcp-core.el --- Shared MCP helpers for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Shared customization and data builders used by the local MCP bridge.

;;; Code:

(require 'json)
(require 'project)
(require 'subr-x)

;;; Customization

(defcustom codex-ide-mcp-enabled t
  "When non-nil, start and register the local Emacs MCP tools endpoint.
The endpoint listens on `codex-ide-mcp-host' and `codex-ide-mcp-port',
and Codex receives it through a transient CLI config override."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-mcp-host "127.0.0.1"
  "Loopback host address for the local MCP HTTP server."
  :type 'string
  :group 'codex-ide)

(defcustom codex-ide-mcp-port 0
  "Port for the local MCP HTTP server.
Zero means ask the operating system for an ephemeral port."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-mcp-selection-content-limit 65536
  "Maximum number of characters included by the selection MCP tool."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-mcp-max-request-bytes (* 1024 1024)
  "Maximum accepted MCP HTTP request body size in bytes.
Also bounds retained incomplete request pending data per client."
  :type 'integer
  :group 'codex-ide)

(defcustom codex-ide-mcp-max-clients 16
  "Maximum simultaneous MCP client connections."
  :type 'integer
  :group 'codex-ide)

;;; Constants

(defconst codex-ide-mcp--server-name "emacs_tools"
  "Codex config key name for the Emacs MCP server.")

(defconst codex-ide-mcp--protocol-version "2025-06-18"
  "MCP protocol version advertised by the local server.")

;;; Small builders

(defun codex-ide-mcp--json-false (value)
  "Return JSON true for VALUE or JSON false for nil."
  (if value t :json-false))

(defun codex-ide-mcp--truthy-p (value)
  "Return non-nil when VALUE is a JSON-style true value."
  (and value (not (eq value :json-false))))

(defun codex-ide-mcp--toml-quote-string (string)
  "Return STRING encoded as a basic TOML string literal."
  (concat
   "\""
   (mapconcat
    (lambda (char)
      (pcase char
        (?\" "\\\"")
        (?\\ "\\\\")
        (?\b "\\b")
        (?\t "\\t")
        (?\n "\\n")
        (?\f "\\f")
        (?\r "\\r")
        (_ (if (< char 32)
               (format "\\u%04x" char)
             (char-to-string char)))))
    string "")
   "\""))

(defun codex-ide-mcp-config-overrides (url)
  "Return Codex CLI config overrides registering the MCP server at URL."
  (list (cons (format "mcp_servers.%s.url" codex-ide-mcp--server-name)
              (codex-ide-mcp--toml-quote-string url))))

(defun codex-ide-mcp--namespace ()
  "Return the Codex-visible namespace for the Emacs MCP tools."
  (format "mcp__%s" codex-ide-mcp--server-name))

(defun codex-ide-mcp--callable-tool-name (tool-name)
  "Return the Codex-visible callable name for TOOL-NAME."
  (format "%s__%s" (codex-ide-mcp--namespace) tool-name))

(defun codex-ide-mcp--ephemeral-port-p ()
  "Return non-nil for an operating-system-chosen MCP port."
  (zerop codex-ide-mcp-port))

(defun codex-ide-mcp--install-command-args (url)
  "Return Codex CLI arguments for persistently adding URL."
  (list "codex" "mcp" "add" codex-ide-mcp--server-name "--url" url))

(defun codex-ide-mcp--install-command (url)
  "Return the persistent Codex MCP setup command for URL."
  (format "codex mcp add %s --url %s" codex-ide-mcp--server-name url))

(defun codex-ide-mcp--persistent-warning ()
  "Return an ephemeral-port warning for persistent Codex setup."
  (when (codex-ide-mcp--ephemeral-port-p)
    (concat "Warning: persistent setup is only reliable with a fixed "
            "`codex-ide-mcp-port'; the current port is ephemeral.")))

(defun codex-ide-mcp--line-column (&optional pos)
  "Return line/column alist for POS, defaulting to point.
Lines are one-based and columns are zero-based."
  (save-excursion
    (save-restriction
      (widen)
      (when pos
        (goto-char pos))
      (list (cons "line" (line-number-at-pos))
            (cons "column" (current-column))))))

(defun codex-ide-mcp--range (beg end)
  "Return a range alist for BEG to END."
  (list (cons "start" (codex-ide-mcp--line-column beg))
        (cons "end" (codex-ide-mcp--line-column end))))

(defun codex-ide-mcp--point-range (beg end)
  "Return a point range alist for BEG to END."
  (list (cons "start" beg)
        (cons "end" end)))

(defun codex-ide-mcp--byte-offset (pos)
  "Return zero-based byte offset for buffer position POS."
  (save-restriction
    (widen)
    (when-let* ((base (position-bytes (point-min)))
                (byte (position-bytes pos)))
      (- byte base))))

(defun codex-ide-mcp--byte-range (beg end)
  "Return a zero-based byte range alist for BEG to END."
  (when-let* ((start (codex-ide-mcp--byte-offset beg))
              (end (codex-ide-mcp--byte-offset end)))
    (list (cons "start" start)
          (cons "end" end))))

(defun codex-ide-mcp--bounded-integer (value default minimum)
  "Return VALUE when it is at least MINIMUM, otherwise DEFAULT."
  (if (and (integerp value) (>= value minimum))
      value
    default))

(defun codex-ide-mcp--buffer-project-root (&optional buffer)
  "Return project root for BUFFER, or nil when no project is known."
  (with-current-buffer (or buffer (current-buffer))
    (and-let* ((project (project-current nil)))
      (expand-file-name (project-root project)))))

(defun codex-ide-mcp--buffer-for-path (path)
  "Return open buffer visiting PATH, or signal `user-error'."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "Tool requires a non-empty path argument"))
  (let ((buffer (find-buffer-visiting (expand-file-name path))))
    (unless (buffer-live-p buffer)
      (user-error "No open buffer for %s" path))
    buffer))

(defun codex-ide-mcp--json-text-result (value)
  "Return a MCP text content result containing VALUE encoded as JSON."
  (list (cons "content"
              (vector
               (list (cons "type" "text")
                     (cons "text" (json-encode value)))))
        (cons "isError" :json-false)))

(defun codex-ide-mcp--text-error-result (message)
  "Return a MCP tool error result with MESSAGE."
  (list (cons "content"
              (vector
               (list (cons "type" "text")
                     (cons "text" message))))
        (cons "isError" t)))

(defun codex-ide-mcp--make-response (id result)
  "Build a JSON-RPC success response for ID and RESULT."
  (list (cons "jsonrpc" "2.0")
        (cons "id" id)
        (cons "result" result)))

(defun codex-ide-mcp--make-error-response (id code message)
  "Build a JSON-RPC error response for ID with CODE and MESSAGE."
  (list (cons "jsonrpc" "2.0")
        (cons "id" id)
        (cons "error" (list (cons "code" code)
                            (cons "message" message)))))

;;; Object access

(defun codex-ide-mcp--keyword (name)
  "Return keyword symbol for JSON field NAME."
  (intern (concat ":" name)))

(defun codex-ide-mcp--object-get (object name)
  "Return field NAME from OBJECT, accepting plists, alists, or hash tables."
  (cond
   ((hash-table-p object)
    (gethash name object))
   ((and (listp object) (keywordp (car object)))
    (plist-get object (codex-ide-mcp--keyword name)))
   ((listp object)
    (or (cdr (assoc name object))
        (cdr (assq (intern-soft name) object))
        (plist-get object (codex-ide-mcp--keyword name))))))

(defun codex-ide-mcp--object-has-key-p (object name)
  "Return non-nil when OBJECT has a member named NAME."
  (cond
   ((hash-table-p object)
    (let ((sentinel (make-symbol "missing")))
      (not (eq (gethash name object sentinel) sentinel))))
   ((and (listp object) (keywordp (car object)))
    (not (null (memq (codex-ide-mcp--keyword name) object))))
   ((listp object)
    (or (assoc name object)
        (assq (intern-soft name) object)
        (memq (codex-ide-mcp--keyword name) object)))))

(provide 'codex-ide-mcp-core)

;;; codex-ide-mcp-core.el ends here
