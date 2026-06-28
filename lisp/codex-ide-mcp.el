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

;; Local streamable-HTTP MCP endpoint for Codex sessions.  The bridge exposes a
;; deliberately narrow set of Emacs tools: buffer metadata, the active
;; selection, opening a file in Emacs, and already-available diagnostics.
;; Codex sessions opt in by adding a transient `-c mcp_servers.emacs_tools.url'
;; override; this module never writes to `~/.codex/config.toml'.
;;
;; Usage:
;;   (setq codex-ide-mcp-enabled t)
;;   M-x codex-ide
;;
;; Commands:
;;   M-x codex-ide-mcp-start
;;   M-x codex-ide-mcp-stop
;;   M-x codex-ide-mcp-status
;;   M-x codex-ide-mcp-install-codex-config

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'json)
(require 'project)
(require 'subr-x)
(require 'xref)
(require 'codex-ide-debug)

;;; Customization

(defcustom codex-ide-mcp-enabled nil
  "When non-nil, start and register the local Emacs MCP tools endpoint.
The endpoint listens on `codex-ide-mcp-host' and `codex-ide-mcp-port',
and Codex receives it through a transient CLI config override."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-mcp-host "127.0.0.1"
  "Host address for the local MCP HTTP server."
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

(defcustom codex-ide-mcp-enable-execute nil
  "Expose the dangerous `emacs_execute' MCP tool when non-nil.
This enables full Emacs Lisp eval from local MCP clients.  The default
is nil because Emacs has no sandbox for arbitrary Elisp."
  :type 'boolean
  :group 'codex-ide)

;;; Constants and variables

(defconst codex-ide-mcp--server-name "emacs_tools"
  "Codex config key name for the Emacs MCP server.")

(defconst codex-ide-mcp--protocol-version "2025-06-18"
  "MCP protocol version advertised by the local server.")

(defvar codex-ide-mcp--server nil
  "The listening MCP server process, or nil when stopped.")

(defvar codex-ide-mcp--port nil
  "The effective MCP server port once the server is listening.")

(defvar codex-ide-mcp--clients (make-hash-table :test 'eq)
  "Connection state keyed by client process.")

;;; Small builders

(defun codex-ide-mcp--json-false (value)
  "Return JSON true for VALUE or JSON false for nil."
  (if value t :json-false))

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

(defun codex-ide-mcp--url ()
  "Return the URL for the running MCP endpoint."
  (format "http://%s:%d/mcp" codex-ide-mcp-host codex-ide-mcp--port))

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

(defun codex-ide-mcp--buffer-project-root (&optional buffer)
  "Return project root for BUFFER, or nil when no project is known."
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((project (project-current nil)))
      (expand-file-name (project-root project)))))

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

;;; Tool helpers

(defun codex-ide-mcp--xref-location-line (location)
  "Return one-based line number for xref LOCATION, or nil."
  (condition-case nil
      (or (xref-location-line location)
          (when-let ((marker (xref-location-marker location)))
            (and (markerp marker)
                 (marker-buffer marker)
                 (with-current-buffer (marker-buffer marker)
                   (line-number-at-pos marker)))))
    (error nil)))

(defun codex-ide-mcp--xref-item->entry (item)
  "Return a JSON-ready entry for xref ITEM, or nil."
  (condition-case nil
      (let ((location (xref-item-location item)))
        (list (cons "file" (or (condition-case nil
                                   (xref-location-group location)
                                 (error nil))
                               ""))
              (cons "line" (or (codex-ide-mcp--xref-location-line
                                 location)
                                0))
              (cons "summary" (or (xref-item-summary item) ""))))
    (error nil)))

(defun codex-ide-mcp--xref-items->entries (items)
  "Return JSON-ready entries for xref ITEMS."
  (delq nil (mapcar #'codex-ide-mcp--xref-item->entry items)))

(defun codex-ide-mcp--imenu-position-line (position)
  "Return one-based line number for imenu POSITION, or nil."
  (cond
   ((markerp position)
    (when (marker-buffer position)
      (with-current-buffer (marker-buffer position)
        (line-number-at-pos position))))
   ((integerp position)
    (line-number-at-pos position))
   (t nil)))

(defun codex-ide-mcp--imenu-flatten (index &optional category)
  "Return flat JSON-ready imenu entries from INDEX.
CATEGORY names the parent group when recursing into sublists."
  (delq nil
        (mapcan
         (lambda (item)
           (let ((name (car item))
                 (value (cdr item)))
             (cond
              ((not (stringp name)) nil)
              ((string-prefix-p "*" name) nil)
              ((imenu--subalist-p item)
               (codex-ide-mcp--imenu-flatten value name))
              ((or (markerp value) (integerp value))
               (when-let ((line (codex-ide-mcp--imenu-position-line
                                  value)))
                 (list (list (cons "name" name)
                             (cons "category" (or category ""))
                             (cons "line" line)))))
              (t nil))))
         (or index nil))))

(defun codex-ide-mcp--buffer-for-path (path)
  "Return open buffer visiting PATH, or signal `user-error'."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "Tool requires a non-empty path argument"))
  (let ((buffer (find-buffer-visiting (expand-file-name path))))
    (unless (buffer-live-p buffer)
      (user-error "No open buffer for %s" path))
    buffer))

;;; Tool implementations

(defun codex-ide-mcp--tool-current-buffer (_args)
  "Return metadata for the current Emacs buffer."
  (let ((file (buffer-file-name))
        (root (codex-ide-mcp--buffer-project-root)))
    (codex-ide-mcp--json-text-result
     (delq nil
           (list (cons "buffer" (buffer-name))
                 (when file (cons "path" (expand-file-name file)))
                 (when root (cons "projectRoot" root))
                 (cons "majorMode" (symbol-name major-mode))
                 (cons "modified" (codex-ide-mcp--json-false
                                    (buffer-modified-p)))
                 (cons "readOnly" (codex-ide-mcp--json-false
                                    buffer-read-only))
                 (cons "point" (codex-ide-mcp--line-column)))))))

(defun codex-ide-mcp--tool-selection (_args)
  "Return the active region in the current Emacs buffer."
  (let* ((active (use-region-p))
         (beg (if active (region-beginning) (point)))
         (end (if active (region-end) (point)))
         (text (if active (buffer-substring-no-properties beg end) ""))
         (truncated (> (length text) codex-ide-mcp-selection-content-limit))
         (content (if truncated
                      (substring text 0 codex-ide-mcp-selection-content-limit)
                    text)))
    (codex-ide-mcp--json-text-result
     (list (cons "active" (codex-ide-mcp--json-false active))
           (cons "range" (codex-ide-mcp--range beg end))
           (cons "text" content)
           (cons "truncated" (codex-ide-mcp--json-false truncated))))))

(defun codex-ide-mcp--tool-open-file (args)
  "Open a file described by ARGS in Emacs and return the destination."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (line (codex-ide-mcp--object-get args "line"))
         (column (or (codex-ide-mcp--object-get args "column") 0)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "Emacs_open_file requires a non-empty path"))
    (let ((expanded (expand-file-name path)))
      (unless (file-readable-p expanded)
        (user-error "File is not readable: %s" expanded))
      (let* ((enable-local-variables nil)
             (enable-local-eval nil)
             (buffer (find-file-noselect expanded)))
        (pop-to-buffer buffer)
        (with-current-buffer buffer
          (when line
            (goto-char (point-min))
            (forward-line (max 0 (1- line)))
            (move-to-column column))
          (codex-ide-mcp--json-text-result
           (list (cons "path" (expand-file-name expanded))
                 (cons "buffer" (buffer-name buffer))
                 (cons "point" (codex-ide-mcp--line-column)))))))))

(defun codex-ide-mcp--flymake-diagnostics ()
  "Return available Flymake diagnostics for the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics)
             (fboundp 'flymake-diagnostic-beg)
             (fboundp 'flymake-diagnostic-end)
             (fboundp 'flymake-diagnostic-type)
             (fboundp 'flymake-diagnostic-text))
    (mapcar
     (lambda (diag)
       (let ((beg (funcall (symbol-function 'flymake-diagnostic-beg) diag))
             (end (funcall (symbol-function 'flymake-diagnostic-end) diag))
             (type (funcall (symbol-function 'flymake-diagnostic-type) diag))
             (text (funcall (symbol-function 'flymake-diagnostic-text) diag)))
         (list (cons "source" "flymake")
               (cons "type" (format "%s" type))
               (cons "message" text)
               (cons "range" (codex-ide-mcp--range beg end)))))
     (funcall (symbol-function 'flymake-diagnostics)
              (point-min) (point-max)))))

(defun codex-ide-mcp--flycheck-diagnostics ()
  "Return available Flycheck diagnostics for the current buffer."
  (when (and (boundp 'flycheck-current-errors)
             (fboundp 'flycheck-error-line)
             (fboundp 'flycheck-error-column)
             (fboundp 'flycheck-error-level)
             (fboundp 'flycheck-error-message)
             (fboundp 'flycheck-error-filename))
    (let ((file (buffer-file-name)))
      (cl-loop for err in (symbol-value 'flycheck-current-errors)
               for filename = (funcall (symbol-function 'flycheck-error-filename)
                                       err)
               when (or (not file)
                        (not filename)
                        (equal (file-truename file)
                               (file-truename filename)))
               collect
               (let* ((line (or (funcall (symbol-function 'flycheck-error-line)
                                         err)
                                1))
                      (column (or (funcall (symbol-function 'flycheck-error-column)
                                           err)
                                  0))
                      (pos (save-excursion
                             (goto-char (point-min))
                             (forward-line (max 0 (1- line)))
                             (move-to-column column)
                             (point))))
                 (list (cons "source" "flycheck")
                       (cons "type" (format "%s" (funcall
                                                   (symbol-function
                                                    'flycheck-error-level)
                                                   err)))
                       (cons "message" (funcall (symbol-function
                                                  'flycheck-error-message)
                                                 err))
                       (cons "range" (codex-ide-mcp--range pos pos))))))))

(defun codex-ide-mcp--tool-diagnostics (args)
  "Return already-known diagnostics for ARGS or the current buffer."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (if (and (stringp path) (not (string-empty-p path)))
                     (codex-ide-mcp--buffer-for-path path)
                   (current-buffer))))
    (with-current-buffer buffer
      (codex-ide-mcp--json-text-result
       (vconcat (append (codex-ide-mcp--flymake-diagnostics)
                        (codex-ide-mcp--flycheck-diagnostics)))))))

(defun codex-ide-mcp--tool-xref-references (args)
  "Return xref references described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (identifier (codex-ide-mcp--object-get args "identifier"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (unless (and (stringp identifier) (not (string-empty-p identifier)))
      (user-error "Tool emacs_xref_references requires a non-empty identifier"))
    (with-current-buffer buffer
      (condition-case err
          (if-let* ((backend (xref-find-backend)))
              (codex-ide-mcp--json-text-result
               (vconcat
                (codex-ide-mcp--xref-items->entries
                 (xref-backend-references backend identifier))))
            (codex-ide-mcp--text-error-result
             (format "No xref backend available for %s" path)))
        (error
         (codex-ide-mcp--text-error-result
          (format "Xref references error: %s"
                  (error-message-string err))))))))

(defun codex-ide-mcp--tool-xref-apropos (args)
  "Return xref apropos matches described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (pattern (codex-ide-mcp--object-get args "pattern"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (unless (and (stringp pattern) (not (string-empty-p pattern)))
      (user-error "Tool emacs_xref_apropos requires a non-empty pattern"))
    (with-current-buffer buffer
      (condition-case err
          (if-let* ((backend (xref-find-backend)))
              (codex-ide-mcp--json-text-result
               (vconcat
                (codex-ide-mcp--xref-items->entries
                 (xref-backend-apropos backend pattern))))
            (codex-ide-mcp--text-error-result
             (format "No xref backend available for %s" path)))
        (error
         (codex-ide-mcp--text-error-result
          (format "Xref apropos error: %s"
                  (error-message-string err))))))))

(defun codex-ide-mcp--tool-project-info (_args)
  "Return project metadata for the current buffer."
  (let* ((buffer (current-buffer))
         (root (codex-ide-mcp--buffer-project-root buffer))
         (file-count
          (if root
              (condition-case nil
                  (if-let* ((project (project-current nil root)))
                      (length (project-files project))
                    0)
                (error 0))
            0)))
    (codex-ide-mcp--json-text-result
     (list (cons "root" root)
           (cons "fileCount" file-count)
           (cons "activeBuffer" (buffer-name buffer))
           (cons "majorMode" (symbol-name major-mode))))))

(defun codex-ide-mcp--tool-imenu-symbols (args)
  "Return imenu symbols for the open buffer described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (buffer (codex-ide-mcp--buffer-for-path path)))
    (with-current-buffer buffer
      (condition-case err
          (codex-ide-mcp--json-text-result
           (vconcat
            (codex-ide-mcp--imenu-flatten
             (imenu--make-index-alist t))))
        (error
         (codex-ide-mcp--text-error-result
          (format "Imenu error: %s" (error-message-string err))))))))

(defun codex-ide-mcp--tool-close-buffer (args)
  "Close an Emacs buffer described by ARGS."
  (let* ((path (codex-ide-mcp--object-get args "path"))
         (name (codex-ide-mcp--object-get args "buffer"))
         (path-p (and (stringp path) (not (string-empty-p path))))
         (name-p (and (stringp name) (not (string-empty-p name))))
         (label (cond (path-p path) (name-p name) (t "buffer")))
         (buffer (cond (path-p (find-buffer-visiting
                                (expand-file-name path)))
                       (name-p (get-buffer name)))))
    (cond
     ((not (buffer-live-p buffer))
      (codex-ide-mcp--text-error-result
       (format "No open buffer for %s" label)))
     ((with-current-buffer buffer
        (and buffer-file-name (buffer-modified-p)))
      (codex-ide-mcp--text-error-result
       (format "Buffer %s has unsaved changes; not closing"
               (buffer-name buffer))))
     (t
      (let ((closed (buffer-name buffer)))
        (if (kill-buffer buffer)
            (codex-ide-mcp--json-text-result
             (list (cons "closed" closed)))
          (codex-ide-mcp--text-error-result
           (format "Could not close buffer %s" closed))))))))

(defun codex-ide-mcp--tool-execute (args)
  "Evaluate the Emacs Lisp expression in ARGS when enabled."
  (if (not codex-ide-mcp-enable-execute)
      (codex-ide-mcp--text-error-result
       "emacs_execute is disabled; set codex-ide-mcp-enable-execute to enable")
    (let ((code (codex-ide-mcp--object-get args "code")))
      (if (not (and (stringp code)
                    (not (string-empty-p (string-trim code)))))
          (codex-ide-mcp--text-error-result
           "emacs_execute requires a non-empty code argument")
        (condition-case err
            (codex-ide-mcp--json-text-result
             (list (cons "value"
                         (prin1-to-string
                          (eval (car (read-from-string code)) t)
                          t))))
          (error
           (codex-ide-mcp--text-error-result
            (format "Evaluation error: %s"
                    (error-message-string err)))))))))

;;; Tool registry and schema

(defconst codex-ide-mcp--tools
  (list
   (list :name "emacs_current_buffer"
         :description "Return metadata for the current Emacs buffer."
         :args nil
         :function #'codex-ide-mcp--tool-current-buffer)
   (list :name "emacs_selection"
         :description "Return the active Emacs region, if any."
         :args nil
         :function #'codex-ide-mcp--tool-selection)
   (list :name "emacs_open_file"
         :description "Open a file in Emacs and optionally move point."
         :args (list (list :name "path"
                           :type 'string
                           :description "File path to open.")
                     (list :name "line"
                           :type 'integer
                           :description "Optional one-based line number."
                           :optional t)
                     (list :name "column"
                           :type 'integer
                           :description "Optional zero-based column."
                           :optional t))
         :function #'codex-ide-mcp--tool-open-file)
   (list :name "emacs_diagnostics"
         :description "Return already-available Flymake/Flycheck diagnostics."
         :args (list (list :name "path"
                           :type 'string
                           :description "Optional path of an open buffer."
                           :optional t))
         :function #'codex-ide-mcp--tool-diagnostics)
   (list :name "emacs_xref_references"
         :description "Find references to an identifier using xref."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer.")
                     (list :name "identifier"
                           :type 'string
                           :description "Symbol name to find references for."))
         :function #'codex-ide-mcp--tool-xref-references)
   (list :name "emacs_xref_apropos"
         :description "Find symbols matching a pattern using xref."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer.")
                     (list :name "pattern"
                           :type 'string
                           :description "Regexp or substring pattern."))
         :function #'codex-ide-mcp--tool-xref-apropos)
   (list :name "emacs_project_info"
         :description "Return project root, file count, and active buffer."
         :args nil
         :function #'codex-ide-mcp--tool-project-info)
   (list :name "emacs_imenu_symbols"
         :description "Return imenu symbols for an open buffer."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of an open buffer."))
         :function #'codex-ide-mcp--tool-imenu-symbols)
   (list :name "emacs_close_buffer"
         :description "Close an open buffer by path or name."
         :args (list (list :name "path"
                           :type 'string
                           :description "Path of the buffer to close."
                           :optional t)
                     (list :name "buffer"
                           :type 'string
                           :description "Buffer name to close."
                           :optional t))
         :function #'codex-ide-mcp--tool-close-buffer)
   (list :name "emacs_execute"
         :description "Evaluate an Emacs Lisp expression. Disabled by default."
         :args (list (list :name "code"
                           :type 'string
                           :description "Emacs Lisp expression to evaluate."))
         :enabled-when 'codex-ide-mcp-enable-execute
         :function #'codex-ide-mcp--tool-execute))
  "Registered MCP tools.")

(defun codex-ide-mcp-tool-names ()
  "Return the names of the local Emacs MCP tools."
  (mapcar (lambda (tool) (plist-get tool :name)) codex-ide-mcp--tools))

(defun codex-ide-mcp--tool-by-name (name)
  "Return registered tool named NAME, or nil."
  (cl-find name codex-ide-mcp--tools
           :key (lambda (tool) (plist-get tool :name))
           :test #'equal))

(defun codex-ide-mcp--tool-enabled-p (tool)
  "Return non-nil when TOOL is currently enabled."
  (let ((gate (plist-get tool :enabled-when)))
    (or (not gate)
        (and (symbolp gate)
             (boundp gate)
             (symbol-value gate)))))

(defun codex-ide-mcp--arg-type-name (type)
  "Return JSON schema type name for TYPE."
  (pcase type
    ('integer "integer")
    ('number "number")
    ('boolean "boolean")
    (_ "string")))

(defun codex-ide-mcp--arg->schema-property (arg)
  "Return (NAME . SCHEMA) for ARG."
  (cons (plist-get arg :name)
        (delq nil
              (list (cons "type" (codex-ide-mcp--arg-type-name
                                   (plist-get arg :type)))
                    (when-let ((description (plist-get arg :description)))
                      (cons "description" description))))))

(defun codex-ide-mcp--tool->mcp (tool)
  "Return MCP tool schema for TOOL."
  (let* ((args (plist-get tool :args))
         (properties (mapcar #'codex-ide-mcp--arg->schema-property args))
         (required (cl-loop for arg in args
                            unless (plist-get arg :optional)
                            collect (plist-get arg :name))))
    (list (cons "name" (plist-get tool :name))
          (cons "description" (plist-get tool :description))
          (cons "inputSchema"
                (list (cons "type" "object")
                      (cons "properties"
                            (or properties (make-hash-table :test 'equal)))
                      (cons "required" (vconcat required))
                      (cons "additionalProperties" :json-false))))))

(defun codex-ide-mcp--validate-required-args (tool args)
  "Signal `user-error' when TOOL required arguments are absent from ARGS."
  (dolist (arg (plist-get tool :args))
    (when (and (not (plist-get arg :optional))
               (not (codex-ide-mcp--object-has-key-p
                     args (plist-get arg :name))))
      (user-error "Tool %s requires argument %s"
                  (plist-get tool :name)
                  (plist-get arg :name)))))

;;; JSON-RPC dispatch

(defun codex-ide-mcp--handle-initialize (_params)
  "Return MCP initialize result."
  (list (cons "protocolVersion" codex-ide-mcp--protocol-version)
        (cons "capabilities"
              (list (cons "tools"
                          (list (cons "listChanged" :json-false)))))
        (cons "serverInfo"
              (list (cons "name" "emacs-codex")
                    (cons "version" "0.1.0")))))

(defun codex-ide-mcp--handle-tools-list (_params)
  "Return MCP tools/list result."
  (list (cons "tools"
              (vconcat
               (mapcar #'codex-ide-mcp--tool->mcp
                       (cl-remove-if-not
                        #'codex-ide-mcp--tool-enabled-p
                        codex-ide-mcp--tools))))))

(defun codex-ide-mcp--handle-tools-call (params)
  "Call an Emacs MCP tool described by PARAMS."
  (let* ((name (codex-ide-mcp--object-get params "name"))
         (args (or (codex-ide-mcp--object-get params "arguments") nil))
         (tool (and (stringp name) (codex-ide-mcp--tool-by-name name))))
    (unless tool
      (user-error "Unknown MCP tool: %s" name))
    (condition-case err
        (progn
          (unless (codex-ide-mcp--tool-enabled-p tool)
            (user-error "Tool %s is disabled" name))
          (codex-ide-mcp--validate-required-args tool args)
          (funcall (plist-get tool :function) args))
      (error
       (codex-ide-mcp--text-error-result (error-message-string err))))))

(defun codex-ide-mcp--dispatch (method params)
  "Dispatch JSON-RPC METHOD with PARAMS and return its result object."
  (pcase method
    ("initialize" (codex-ide-mcp--handle-initialize params))
    ("tools/list" (codex-ide-mcp--handle-tools-list params))
    ("tools/call" (codex-ide-mcp--handle-tools-call params))
    (_ (user-error "Unsupported MCP method: %s" method))))

(defun codex-ide-mcp--handle-message (message)
  "Handle decoded JSON-RPC MESSAGE.
Returns a response alist, or nil for notifications."
  (let ((id (codex-ide-mcp--object-get message "id"))
        (method (codex-ide-mcp--object-get message "method"))
        (params (codex-ide-mcp--object-get message "params")))
    (if (not (codex-ide-mcp--object-has-key-p message "id"))
        nil
      (condition-case err
          (codex-ide-mcp--make-response
           id (codex-ide-mcp--dispatch method params))
        (user-error
         (codex-ide-mcp--make-error-response
          id -32601 (error-message-string err)))
        (error
         (codex-ide-mcp--make-error-response
          id -32603 (error-message-string err)))))))

;;; HTTP server boundary

(defun codex-ide-mcp--json-read (bytes)
  "Decode JSON BYTES into a plist."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string
     (decode-coding-string bytes 'utf-8))))

(defun codex-ide-mcp--status-text (status)
  "Return HTTP reason phrase for STATUS."
  (pcase status
    (200 "OK")
    (204 "No Content")
    (400 "Bad Request")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (415 "Unsupported Media Type")
    (_ "Internal Server Error")))

(defun codex-ide-mcp--send-json (proc status body)
  "Send HTTP STATUS and JSON BODY to PROC, then close the connection."
  (let* ((payload (if body
                      (encode-coding-string (json-encode body) 'utf-8)
                    ""))
         (headers (concat
                   (format "HTTP/1.1 %d %s\r\n"
                           status (codex-ide-mcp--status-text status))
                   "Content-Type: application/json\r\n"
                   "Connection: close\r\n"
                   (format "Content-Length: %d\r\n\r\n"
                           (length payload)))))
    (when (process-live-p proc)
      (process-send-string proc (concat headers payload))
      (delete-process proc))))

(defun codex-ide-mcp--parse-headers (header-text)
  "Parse HTTP HEADER-TEXT into a plist."
  (let* ((lines (split-string header-text "\r?\n" t))
         (request-line (car lines))
         (parts (and request-line (split-string request-line " " t)))
         (headers nil))
    (dolist (line (cdr lines))
      (when (string-match (rx string-start
                              (group (+ (not ?:))) ":"
                              (* blank)
                              (group (* anything))
                              string-end)
                          line)
        (push (cons (downcase (match-string 1 line))
                    (match-string 2 line))
              headers)))
    (list :method (car parts)
          :path (cadr parts)
          :headers headers)))

(defun codex-ide-mcp--header (request name)
  "Return header NAME from parsed REQUEST, or nil."
  (cdr (assoc (downcase name) (plist-get request :headers))))

(defun codex-ide-mcp--host-name (host)
  "Return normalized host name from HOST, dropping an optional port."
  (when (stringp host)
    (let ((host (string-trim host)))
      (cond
       ((string-empty-p host) nil)
       ((string-match (rx string-start
                          "[" (group (+ (not (any "]")))) "]"
                          (? ":" (+ digit))
                          string-end)
                      host)
        (downcase (match-string 1 host)))
       ((string-match (rx string-start
                          (group (+ (not (any ":"))))
                          (? ":" (+ digit))
                          string-end)
                      host)
        (downcase (match-string 1 host)))
       (t (downcase host))))))

(defun codex-ide-mcp--local-host-p (host)
  "Return non-nil when HOST is absent or names a loopback host."
  (let ((name (codex-ide-mcp--host-name host)))
    (or (not name)
        (member name '("127.0.0.1" "localhost" "::1")))))

(defun codex-ide-mcp--local-origin-p (origin)
  "Return non-nil when ORIGIN is absent or names a loopback origin."
  (or (not origin)
      (string-empty-p (string-trim origin))
      (and (string-match (rx string-start
                             (or "http" "https") "://"
                             (group (+ (not (any "/"))))
                             string-end)
                         origin)
           (codex-ide-mcp--local-host-p (match-string 1 origin)))))

(defun codex-ide-mcp--json-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE is application/json."
  (and (stringp content-type)
       (string-match-p (rx string-start
                           (* blank) "application/json" (* blank)
                           (? ";" (* anything))
                           string-end)
                       (downcase content-type))))

(defun codex-ide-mcp--request-error (request)
  "Return (STATUS . MESSAGE) when REQUEST should be rejected, else nil."
  (cond
   ((not (equal (plist-get request :path) "/mcp"))
    '(404 . "Only /mcp is supported"))
   ((not (codex-ide-mcp--local-host-p
          (codex-ide-mcp--header request "host")))
    '(403 . "Host must be loopback"))
   ((not (codex-ide-mcp--local-origin-p
          (codex-ide-mcp--header request "origin")))
    '(403 . "Origin must be loopback"))
   ((not (codex-ide-mcp--json-content-type-p
          (codex-ide-mcp--header request "content-type")))
    '(415 . "Content-Type must be application/json"))))

(defun codex-ide-mcp--content-length (request)
  "Return Content-Length for parsed REQUEST."
  (if-let* ((value (cdr (assoc "content-length"
                               (plist-get request :headers)))))
      (string-to-number value)
    0))

(defun codex-ide-mcp--split-request (pending)
  "Return (REQUEST . REST) from PENDING, or nil when incomplete."
  (when (string-match "\r\n\r\n" pending)
    (let* ((header-end (match-beginning 0))
           (body-start (match-end 0))
           (request (codex-ide-mcp--parse-headers
                     (substring pending 0 header-end)))
           (length (codex-ide-mcp--content-length request))
           (total (+ body-start length)))
      (when (<= total (length pending))
        (setf (plist-get request :body)
              (substring pending body-start total))
        (cons request (substring pending total))))))

(defun codex-ide-mcp--selected-buffer ()
  "Return the current UI buffer used for MCP tool execution."
  (if-let* ((window (selected-window)))
      (window-buffer window)
    (current-buffer)))

(defun codex-ide-mcp--handle-http-request (proc request)
  "Handle parsed HTTP REQUEST from PROC."
  (if (not (equal (plist-get request :method) "POST"))
      (codex-ide-mcp--send-json
       proc 405 (codex-ide-mcp--make-error-response
                 nil -32600 "Only POST is supported"))
    (if-let* ((request-error (codex-ide-mcp--request-error request)))
        (codex-ide-mcp--send-json
         proc (car request-error)
         (codex-ide-mcp--make-error-response
          nil -32600 (cdr request-error)))
      (condition-case err
          (let* ((message (codex-ide-mcp--json-read (plist-get request :body)))
                 (response (with-current-buffer (codex-ide-mcp--selected-buffer)
                             (codex-ide-mcp--handle-message message))))
            (if response
                (codex-ide-mcp--send-json proc 200 response)
              (codex-ide-mcp--send-json proc 204 nil)))
        (error
         (codex-ide-mcp--send-json
          proc 400 (codex-ide-mcp--make-error-response
                    nil -32700 (error-message-string err))))))))

(defun codex-ide-mcp--client-state (proc)
  "Return accumulated state for client PROC, creating it if needed."
  (or (gethash proc codex-ide-mcp--clients)
      (let ((state (list :pending "")))
        (puthash proc state codex-ide-mcp--clients)
        state)))

(defun codex-ide-mcp--filter (proc string)
  "Process filter for MCP HTTP connection PROC receiving STRING."
  (set-process-coding-system proc 'binary 'binary)
  (let* ((state (codex-ide-mcp--client-state proc))
         (pending (concat (plist-get state :pending) string)))
    (while (and (process-live-p proc)
                (if-let* ((parsed (codex-ide-mcp--split-request pending)))
                    (let ((request (car parsed)))
                      (setq pending (cdr parsed))
                      (codex-ide-mcp--handle-http-request proc request)
                      t)
                  nil)))
    (plist-put state :pending pending)))

(defun codex-ide-mcp--sentinel (proc event)
  "Clean client state for PROC on EVENT."
  (codex-ide-debug "Codex MCP client event: %s" (string-trim event))
  (unless (process-live-p proc)
    (remhash proc codex-ide-mcp--clients)))

(defun codex-ide-mcp--contact-port (process)
  "Return network port for PROCESS."
  (let* ((full (ignore-errors (process-contact process t)))
         (short (ignore-errors (process-contact process)))
         (service (or (and (listp full) (plist-get full :service))
                      (and (listp short) (plist-get short :service))
                      (and (listp short) (cadr short))
                      (and (listp full)
                           (cl-find-if #'integerp full)))))
    (unless (integerp service)
      (error "Could not determine MCP server port from process contact"))
    service))

(defun codex-ide-mcp--running-p ()
  "Return non-nil when the MCP server is listening."
  (and codex-ide-mcp--server
       (process-live-p codex-ide-mcp--server)))

(defun codex-ide-mcp--start-server ()
  "Start the local MCP HTTP server and return its process."
  (when (codex-ide-mcp--running-p)
    (user-error "Codex MCP tools server is already running"))
  (let ((server (make-network-process
                 :name "codex-ide-mcp"
                 :buffer nil
                 :host codex-ide-mcp-host
                 :service codex-ide-mcp-port
                 :server t
                 :noquery t
                 :filter #'codex-ide-mcp--filter
                 :sentinel #'codex-ide-mcp--sentinel)))
    (set-process-coding-system server 'binary 'binary)
    (setq codex-ide-mcp--server server
          codex-ide-mcp--port (codex-ide-mcp--contact-port server))
    (codex-ide-debug "Codex MCP listening on %s" (codex-ide-mcp--url))
    server))

(defun codex-ide-mcp--stop-server ()
  "Stop the local MCP HTTP server."
  (when codex-ide-mcp--server
    (ignore-errors (delete-process codex-ide-mcp--server))
    (setq codex-ide-mcp--server nil
          codex-ide-mcp--port nil))
  (maphash (lambda (proc _state)
             (ignore-errors (delete-process proc)))
           codex-ide-mcp--clients)
  (clrhash codex-ide-mcp--clients)
  (codex-ide-debug "Codex MCP tools server stopped"))

;;; Public commands

(defun codex-ide-mcp-ensure-server ()
  "Ensure the local MCP HTTP server is running and return its URL."
  (unless (codex-ide-mcp--running-p)
    (codex-ide-mcp--start-server))
  (codex-ide-mcp--url))

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
