;;; codex-ide-mcp-tests.el --- ERT tests for codex-ide-mcp  -*- lexical-binding: t; -*-

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

;; Unit tests for the narrow local MCP bridge.  These tests exercise pure
;; schema, dispatch, and command-builder integration without opening a network
;; listener or starting Codex.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'xref)
(require 'codex-ide)
(require 'codex-ide-mcp)

(defun codex-ide-mcp-test--json-read (string)
  "Decode JSON STRING as an alist for assertions."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (json-false nil)
        (json-null nil))
    (json-read-from-string string)))

(defun codex-ide-mcp-test--result-text (result)
  "Return the first MCP text content string from RESULT."
  (cdr (assoc "text" (aref (cdr (assoc "content" result)) 0))))

(defun codex-ide-mcp-test--decoded-result (result)
  "Decode the first MCP JSON text content from RESULT."
  (codex-ide-mcp-test--json-read
   (codex-ide-mcp-test--result-text result)))

(defun codex-ide-mcp-test--line-position (line)
  "Return buffer position at one-based LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun codex-ide-mcp-test--request (&rest headers)
  "Return a normal parsed MCP request with HEADERS."
  (list :method "POST"
        :path "/mcp"
        :headers (append headers
                         '(("host" . "127.0.0.1:43210")
                           ("content-type" . "application/json")))
        :body "{}"))

(ert-deftest codex-ide-mcp-split-request-exact ()
  "HTTP request splitting returns a complete request and no rest."
  (let* ((body "{\"jsonrpc\":\"2.0\"}")
         (pending (format "POST /mcp HTTP/1.1\r\nHost: 127.0.0.1:43210\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                          (string-bytes body) body))
         (parsed (codex-ide-mcp--split-request pending))
         (request (car parsed)))
    (should parsed)
    (should (equal (plist-get request :method) "POST"))
    (should (equal (plist-get request :path) "/mcp"))
    (should (equal (plist-get request :body) body))
    (should (equal (cdr parsed) ""))))

(ert-deftest codex-ide-mcp-split-request-partial-body ()
  "HTTP request splitting waits when Content-Length is incomplete."
  (let* ((body "{\"jsonrpc\":\"2.0\"}")
         (pending (format "POST /mcp HTTP/1.1\r\nContent-Length: %d\r\n\r\n%s"
                          (1+ (string-bytes body)) body)))
    (should-not (codex-ide-mcp--split-request pending))))

(ert-deftest codex-ide-mcp-split-request-preserves-rest ()
  "HTTP request splitting preserves bytes after the first request."
  (let* ((body "{}")
         (rest "POST /mcp HTTP/1.1\r\n")
         (pending (format "POST /mcp HTTP/1.1\r\nContent-Length: %d\r\n\r\n%s%s"
                          (string-bytes body) body rest))
         (parsed (codex-ide-mcp--split-request pending)))
    (should parsed)
    (should (equal (plist-get (car parsed) :body) body))
    (should (equal (cdr parsed) rest))))

(ert-deftest codex-ide-mcp-request-error-accepts-local-json-post ()
  "Request validation accepts normal localhost JSON POST requests."
  (should-not (codex-ide-mcp--request-error
               (codex-ide-mcp-test--request))))

(ert-deftest codex-ide-mcp-request-error-rejects-hostile-origin ()
  "Request validation rejects browser requests from non-local origins."
  (should (equal (codex-ide-mcp--request-error
                  (codex-ide-mcp-test--request
                   '("origin" . "https://example.test")))
                 '(403 . "Origin must be loopback"))))

(ert-deftest codex-ide-mcp-request-error-rejects-dns-rebinding-host ()
  "Request validation rejects non-loopback Host headers."
  (should (equal (codex-ide-mcp--request-error
                  (codex-ide-mcp-test--request
                   '("host" . "evil.example:43210")))
                 '(403 . "Host must be loopback"))))

(ert-deftest codex-ide-mcp-request-error-rejects-non-json-content-type ()
  "Request validation rejects non-JSON content types."
  (should (equal (codex-ide-mcp--request-error
                  (codex-ide-mcp-test--request
                   '("content-type" . "text/plain")))
                 '(415 . "Content-Type must be application/json"))))

(ert-deftest codex-ide-mcp-config-overrides-url ()
  "MCP URL override is emitted as a TOML string."
  (should (equal (codex-ide-mcp-config-overrides
                  "http://127.0.0.1:43210/mcp")
                 '(("mcp_servers.emacs_tools.url"
                    . "\"http://127.0.0.1:43210/mcp\"")))))

(ert-deftest codex-ide-mcp-install-command-fixed-port ()
  "Fixed-port persistent setup produces a stable Codex command."
  (let ((codex-ide-mcp-port 43210))
    (should (equal (codex-ide-mcp--install-command
                    "http://127.0.0.1:43210/mcp")
                   "codex mcp add emacs_tools --url http://127.0.0.1:43210/mcp"))
    (should-not (codex-ide-mcp--persistent-warning))))

(ert-deftest codex-ide-mcp-setup-message-warns-for-ephemeral-port ()
  "Ephemeral-port persistent setup reports that reuse is unreliable."
  (let ((codex-ide-mcp-port 0))
    (should (string-match-p
             "Persistent setup command: codex mcp add emacs_tools --url http://127.0.0.1:43210/mcp"
             (codex-ide-mcp--setup-message
              "http://127.0.0.1:43210/mcp")))
    (should (string-match-p
             "ephemeral"
             (codex-ide-mcp--setup-message
              "http://127.0.0.1:43210/mcp")))))

(ert-deftest codex-ide-mcp-status-message-running ()
  "Running status reports server key, namespace, URL, and execute state."
  (let ((codex-ide-mcp-port 43210)
        (codex-ide-mcp-enable-execute t))
    (cl-letf (((symbol-function 'codex-ide-mcp--running-p)
               (lambda () t))
              ((symbol-function 'codex-ide-mcp--url)
               (lambda () "http://127.0.0.1:43210/mcp")))
      (let ((status (codex-ide-mcp--status-message)))
        (should (string-match-p "running" status))
        (should (string-match-p "URL: http://127.0.0.1:43210/mcp" status))
        (should (string-match-p "Port: fixed" status))
        (should (string-match-p "Server key: emacs_tools" status))
        (should (string-match-p "Codex namespace: mcp__emacs_tools" status))
        (should (string-match-p
                 "Example tool: mcp__emacs_tools__emacs_current_buffer"
                 status))
        (should (string-match-p "emacs_execute: enabled" status))))))

(ert-deftest codex-ide-mcp-status-message-stopped-ephemeral ()
  "Stopped status reports ephemeral port setup and disabled execute."
  (let ((codex-ide-mcp-port 0)
        (codex-ide-mcp-enable-execute nil))
    (cl-letf (((symbol-function 'codex-ide-mcp--running-p)
               (lambda () nil)))
      (let ((status (codex-ide-mcp--status-message)))
        (should (string-match-p "stopped" status))
        (should (string-match-p "Port: ephemeral" status))
        (should (string-match-p "emacs_execute: disabled" status))
        (should-not (string-match-p "Persistent setup command" status))))))

(ert-deftest codex-ide-mcp-session-overrides-disabled ()
  "Disabled MCP integration leaves session overrides unchanged."
  (let ((codex-ide-config-overrides '(("model" . "o3")))
        (codex-ide-mcp-enabled nil))
    (cl-letf (((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda () (error "should not start MCP server"))))
      (should (equal (codex-ide--session-config-overrides)
                     '(("model" . "o3")))))))

(ert-deftest codex-ide-mcp-session-overrides-enabled ()
  "Enabled MCP integration appends the transient server URL override."
  (let ((codex-ide-config-overrides '(("model" . "o3")))
        (codex-ide-mcp-enabled t))
    (cl-letf (((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda () "http://127.0.0.1:43210/mcp")))
      (should (equal (codex-ide--session-config-overrides)
                     '(("model" . "o3")
                       ("mcp_servers.emacs_tools.url"
                        . "\"http://127.0.0.1:43210/mcp\"")))))))

(ert-deftest codex-ide-mcp-build-command-session-overrides ()
  "Session-local MCP overrides are visible to command construction."
  (let ((codex-ide-config-overrides nil)
        (codex-ide-mcp-enabled t)
        (codex-ide-cli-path "codex")
        (codex-ide-cli-extra-args nil)
        (codex-ide-ask-for-approval nil)
        (codex-ide-no-alt-screen nil))
    (cl-letf (((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda () "http://127.0.0.1:43210/mcp")))
      (let* ((codex-ide-config-overrides
              (codex-ide--session-config-overrides))
             (command (codex-ide--build-command t)))
        (should (equal command
                       '("codex"
                         "-c"
                         "mcp_servers.emacs_tools.url=\"http://127.0.0.1:43210/mcp\""
                         "resume"
                         "--last")))))))

(ert-deftest codex-ide-mcp-tool-to-mcp-schema-required-args ()
  "Tool schema marks required args and omits optional args from required."
  (let* ((tool (codex-ide-mcp--tool-by-name "emacs_open_file"))
         (schema (codex-ide-mcp--tool->mcp tool))
         (input (cdr (assoc "inputSchema" schema)))
         (properties (cdr (assoc "properties" input))))
    (should (equal (cdr (assoc "name" schema)) "emacs_open_file"))
    (should (equal (cdr (assoc "type" input)) "object"))
    (should (equal (cdr (assoc "required" input)) ["path"]))
    (should (equal (cdr (assoc "type" (cdr (assoc "path" properties))))
                   "string"))
    (should (equal (cdr (assoc "type" (cdr (assoc "line" properties))))
                   "integer"))))

(ert-deftest codex-ide-mcp-xref-item-to-entry-file-location ()
  "Xref file locations become plain JSON-ready entries."
  (let* ((item (xref-make
                "summary"
                (xref-make-file-location "/tmp/f.el" 12 0)))
         (entry (codex-ide-mcp--xref-item->entry item)))
    (should (equal (cdr (assoc "file" entry)) "/tmp/f.el"))
    (should (equal (cdr (assoc "line" entry)) 12))
    (should (equal (cdr (assoc "summary" entry)) "summary"))))

(ert-deftest codex-ide-mcp-xref-items-to-entries-skips-failures ()
  "Bad xref items are skipped when building JSON-ready entries."
  (let* ((item (xref-make
                "summary"
                (xref-make-file-location "/tmp/f.el" 12 0)))
         (entries (codex-ide-mcp--xref-items->entries
                   (list item "not an xref item"))))
    (should (equal (length entries) 1))
    (should (equal (cdr (assoc "summary" (car entries))) "summary"))))

(ert-deftest codex-ide-mcp-imenu-flatten-flat ()
  "Flat imenu indexes become flat JSON-ready entries."
  (with-temp-buffer
    (dotimes (_ 12) (insert "x\n"))
    (let* ((entries (codex-ide-mcp--imenu-flatten
                     `(("foo" . ,(codex-ide-mcp-test--line-position 5))
                       ("bar" . ,(codex-ide-mcp-test--line-position 10)))))
           (first (car entries))
           (second (cadr entries)))
      (should (equal (mapcar (lambda (entry) (cdr (assoc "name" entry)))
                             entries)
                     '("foo" "bar")))
      (should (equal (cdr (assoc "category" first)) ""))
      (should (equal (cdr (assoc "line" first)) 5))
      (should (equal (cdr (assoc "line" second)) 10)))))

(ert-deftest codex-ide-mcp-imenu-flatten-nested ()
  "Nested imenu indexes include their parent category."
  (with-temp-buffer
    (dotimes (_ 8) (insert "x\n"))
    (let* ((entries (codex-ide-mcp--imenu-flatten
                     `(("Functions"
                        ("alpha" . ,(codex-ide-mcp-test--line-position 3)))
                       ("beta" . ,(codex-ide-mcp-test--line-position 7)))))
           (alpha (car entries))
           (beta (cadr entries)))
      (should (equal (cdr (assoc "name" alpha)) "alpha"))
      (should (equal (cdr (assoc "category" alpha)) "Functions"))
      (should (equal (cdr (assoc "line" alpha)) 3))
      (should (equal (cdr (assoc "name" beta)) "beta"))
      (should (equal (cdr (assoc "category" beta)) ""))
      (should (equal (cdr (assoc "line" beta)) 7)))))

(ert-deftest codex-ide-mcp-imenu-flatten-skips-rescan ()
  "Imenu rescan entries are omitted from flattened output."
  (with-temp-buffer
    (dotimes (_ 2) (insert "x\n"))
    (let ((entries (codex-ide-mcp--imenu-flatten
                    `(("*Rescan*" . ,(point-min))
                      ("real" . ,(codex-ide-mcp-test--line-position 2))))))
      (should (equal (length entries) 1))
      (should (equal (cdr (assoc "name" (car entries))) "real")))))

(ert-deftest codex-ide-mcp-tool-to-mcp-schema-xref-references ()
  "Xref references schema includes required path and identifier strings."
  (let* ((tool (codex-ide-mcp--tool-by-name "emacs_xref_references"))
         (schema (codex-ide-mcp--tool->mcp tool))
         (input (cdr (assoc "inputSchema" schema)))
         (properties (cdr (assoc "properties" input))))
    (should (equal (cdr (assoc "name" schema)) "emacs_xref_references"))
    (should (equal (cdr (assoc "required" input)) ["path" "identifier"]))
    (should (equal (cdr (assoc "type" (cdr (assoc "path" properties))))
                   "string"))
    (should (equal (cdr (assoc "type" (cdr (assoc "identifier" properties))))
                   "string"))))

(ert-deftest codex-ide-mcp-tools-list-shape-default ()
  "tools/list returns the default Emacs tool schemas."
  (let* ((codex-ide-mcp-enable-execute nil)
         (result (codex-ide-mcp--handle-tools-list nil))
         (tools (cdr (assoc "tools" result)))
         (names (mapcar (lambda (tool) (cdr (assoc "name" tool)))
                        (append tools nil))))
    (should (vectorp tools))
    (should (equal names
                   '("emacs_current_buffer"
                     "emacs_selection"
                     "emacs_open_file"
                     "emacs_diagnostics"
                     "emacs_xref_references"
                     "emacs_xref_apropos"
                     "emacs_project_info"
                     "emacs_imenu_symbols"
                     "emacs_close_buffer")))))

(ert-deftest codex-ide-mcp-callable-name-display-only ()
  "Codex callable names are display-only and raw MCP names stay unchanged."
  (let ((names (codex-ide-mcp-tool-names)))
    (should (equal (codex-ide-mcp--callable-tool-name "emacs_selection")
                   "mcp__emacs_tools__emacs_selection"))
    (should (member "emacs_selection" names))
    (should-not (member "mcp__emacs_tools__emacs_selection" names))))

(ert-deftest codex-ide-mcp-tools-list-hides-disabled-execute ()
  "Disabled execute tool is omitted from tools/list."
  (let* ((codex-ide-mcp-enable-execute nil)
         (result (codex-ide-mcp--handle-tools-list nil))
         (tools (cdr (assoc "tools" result)))
         (names (mapcar (lambda (tool) (cdr (assoc "name" tool)))
                        (append tools nil))))
    (should (equal (length names) 9))
    (should-not (member "emacs_execute" names))))

(ert-deftest codex-ide-mcp-tools-list-shows-enabled-execute ()
  "Enabled execute tool is included in tools/list."
  (let* ((codex-ide-mcp-enable-execute t)
         (result (codex-ide-mcp--handle-tools-list nil))
         (tools (cdr (assoc "tools" result)))
         (names (mapcar (lambda (tool) (cdr (assoc "name" tool)))
                        (append tools nil))))
    (should (equal (length names) 10))
    (should (member "emacs_execute" names))))

(ert-deftest codex-ide-mcp-tools-call-execute-disabled ()
  "Disabled execute tool returns an MCP tool error."
  (let* ((codex-ide-mcp-enable-execute nil)
         (result (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_execute"
                    :arguments (:code "(+ 1 2)")))))
    (should (eq (cdr (assoc "isError" result)) t))
    (should (string-match-p "disabled"
                            (codex-ide-mcp-test--result-text result)))))

(ert-deftest codex-ide-mcp-tools-call-execute-enabled-evals ()
  "Enabled execute tool evaluates one Elisp expression."
  (let* ((codex-ide-mcp-enable-execute t)
         (result (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_execute"
                    :arguments (:code "(+ 1 2)"))))
         (decoded (codex-ide-mcp-test--decoded-result result)))
    (should (eq (cdr (assoc "isError" result)) :json-false))
    (should (equal (cdr (assoc "value" decoded)) "3"))))

(ert-deftest codex-ide-mcp-tools-call-xref-references-no-buffer ()
  "Xref references tool requires an already-open buffer."
  (let ((path (make-temp-file "codex-ide-mcp-xref")))
    (unwind-protect
        (let* ((result (codex-ide-mcp--handle-tools-call
                        `(:name "emacs_xref_references"
                          :arguments (:path ,path :identifier "foo")))))
          (should (eq (cdr (assoc "isError" result)) t))
          (should (string-match-p "No open buffer"
                                  (codex-ide-mcp-test--result-text result))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest codex-ide-mcp-tools-call-close-buffer-by-name ()
  "Close-buffer tool kills a named unmodified buffer."
  (let ((buffer (generate-new-buffer "codex-ide-mcp-close-test")))
    (unwind-protect
        (let* ((name (buffer-name buffer))
               (result (codex-ide-mcp--handle-tools-call
                        `(:name "emacs_close_buffer"
                          :arguments (:buffer ,name))))
               (decoded (codex-ide-mcp-test--decoded-result result)))
          (should (eq (cdr (assoc "isError" result)) :json-false))
          (should (equal (cdr (assoc "closed" decoded)) name))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-mcp-tools-call-close-buffer-refuses-modified ()
  "Close-buffer tool refuses modified file-visiting buffers."
  (let* ((file (make-temp-file "codex-ide-mcp-close"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "changed"))
          (let ((result (codex-ide-mcp--handle-tools-call
                         `(:name "emacs_close_buffer"
                           :arguments (:path ,file)))))
            (should (eq (cdr (assoc "isError" result)) t))
            (should (string-match-p "unsaved changes"
                                    (codex-ide-mcp-test--result-text result)))
            (should (buffer-live-p buffer))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest codex-ide-mcp-tools-call-project-info-no-project ()
  "Project info reports nil root outside a project."
  (let ((dir (make-temp-file "codex-ide-mcp-no-project" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir))
              (project-find-functions nil))
          (with-temp-buffer
            (rename-buffer "codex-ide-mcp-no-project" t)
            (let* ((result (codex-ide-mcp--handle-tools-call
                            '(:name "emacs_project_info"
                              :arguments nil)))
                   (decoded (codex-ide-mcp-test--decoded-result result)))
              (should (eq (cdr (assoc "isError" result)) :json-false))
              (should-not (cdr (assoc "root" decoded)))
              (should (equal (cdr (assoc "fileCount" decoded)) 0))
              (should (equal (cdr (assoc "activeBuffer" decoded))
                             "codex-ide-mcp-no-project"))
              (should (equal (cdr (assoc "majorMode" decoded))
                             "fundamental-mode")))))
      (delete-directory dir t))))

(ert-deftest codex-ide-mcp-tools-call-current-buffer ()
  "current-buffer tool returns normal MCP text content."
  (with-temp-buffer
    (rename-buffer "codex-ide-mcp-test" t)
    (let* ((result (codex-ide-mcp--handle-tools-call
                    '(:name "emacs_current_buffer" :arguments nil)))
           (content (aref (cdr (assoc "content" result)) 0))
           (decoded (codex-ide-mcp-test--json-read
                     (cdr (assoc "text" content)))))
      (should (eq (cdr (assoc "isError" result)) :json-false))
      (should (equal (cdr (assoc "type" content)) "text"))
      (should (equal (cdr (assoc "buffer" decoded))
                     "codex-ide-mcp-test")))))

(ert-deftest codex-ide-mcp-tools-call-open-file-validates-path ()
  "open-file tool reports an MCP tool error when path is absent."
  (let* ((result (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_open_file" :arguments nil)))
         (content (aref (cdr (assoc "content" result)) 0)))
    (should (eq (cdr (assoc "isError" result)) t))
    (should (string-match-p "requires argument path"
                            (cdr (assoc "text" content))))))

(ert-deftest codex-ide-mcp-handle-message-wraps-tools-list ()
  "JSON-RPC messages are wrapped in a success response."
  (let* ((response (codex-ide-mcp--handle-message
                    '(:jsonrpc "2.0" :id 7 :method "tools/list")))
         (result (cdr (assoc "result" response))))
    (should (equal (cdr (assoc "jsonrpc" response)) "2.0"))
    (should (equal (cdr (assoc "id" response)) 7))
    (should (vectorp (cdr (assoc "tools" result))))))

(provide 'codex-ide-mcp-tests)

;;; codex-ide-mcp-tests.el ends here
