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
                 '(("mcp_servers.emacs-tools.url"
                    . "\"http://127.0.0.1:43210/mcp\"")))))

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
                       ("mcp_servers.emacs-tools.url"
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
                         "mcp_servers.emacs-tools.url=\"http://127.0.0.1:43210/mcp\""
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

(ert-deftest codex-ide-mcp-tools-list-shape ()
  "tools/list returns the four narrow Emacs tool schemas."
  (let* ((result (codex-ide-mcp--handle-tools-list nil))
         (tools (cdr (assoc "tools" result)))
         (names (mapcar (lambda (tool) (cdr (assoc "name" tool)))
                        (append tools nil))))
    (should (vectorp tools))
    (should (equal names
                   '("emacs_current_buffer"
                     "emacs_selection"
                     "emacs_open_file"
                     "emacs_diagnostics")))))

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
