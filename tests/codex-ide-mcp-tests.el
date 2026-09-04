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
;; schema, dispatch, command-builder integration, and disposable loopback
;; listeners without starting Codex.

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

(cl-defstruct codex-ide-mcp-test-node
  type start end named field parent children)

(defun codex-ide-mcp-test--node
    (type start end &optional named field children)
  "Return a stub tree-sitter node."
  (let ((node (make-codex-ide-mcp-test-node
               :type type
               :start start
               :end end
               :named named
               :field field
               :children children)))
    (dolist (child children)
      (setf (codex-ide-mcp-test-node-parent child) node))
    node))

(defun codex-ide-mcp-test--node-at (node pos)
  "Return the deepest stub NODE covering POS."
  (or (cl-some
       (lambda (child)
         (when (and (<= (codex-ide-mcp-test-node-start child) pos)
                    (< pos (codex-ide-mcp-test-node-end child)))
           (codex-ide-mcp-test--node-at child pos)))
       (codex-ide-mcp-test-node-children node))
      node))

(defmacro codex-ide-mcp-test--with-treesit (root &rest body)
  "Run BODY with tree-sitter functions stubbed around ROOT."
  (declare (indent 1))
  `(let ((root-node ,root)
         (parser 'codex-ide-mcp-test-parser))
     (cl-letf (((symbol-function 'treesit-available-p)
                (lambda () t))
               ((symbol-function 'treesit-parser-list)
                (lambda (&rest _args) (list parser)))
               ((symbol-function 'treesit-parser-language)
                (lambda (_parser) 'elisp))
               ((symbol-function 'treesit-parser-root-node)
                (lambda (_parser) root-node))
               ((symbol-function 'treesit-node-at)
                (lambda (pos &optional _parser-or-lang _named)
                  (codex-ide-mcp-test--node-at root-node pos)))
               ((symbol-function 'treesit-node-check)
                (lambda (node property)
                  (and (eq property 'named)
                       (codex-ide-mcp-test-node-named node))))
               ((symbol-function 'treesit-node-child)
                (lambda (node n &optional _named)
                  (nth n (codex-ide-mcp-test-node-children node))))
               ((symbol-function 'treesit-node-child-count)
                (lambda (node &optional _named)
                  (length (codex-ide-mcp-test-node-children node))))
               ((symbol-function 'treesit-node-end)
                #'codex-ide-mcp-test-node-end)
               ((symbol-function 'treesit-node-field-name)
                #'codex-ide-mcp-test-node-field)
               ((symbol-function 'treesit-node-parent)
                #'codex-ide-mcp-test-node-parent)
               ((symbol-function 'treesit-node-start)
                #'codex-ide-mcp-test-node-start)
               ((symbol-function 'treesit-node-type)
                #'codex-ide-mcp-test-node-type))
       ,@body)))

(defun codex-ide-mcp-test--sample-tree ()
  "Insert sample text and return stub tree metadata."
  (insert "(message \"hi\")\n(+ 1 2)\n")
  (let* ((first-start (point-min))
         (first-end (save-excursion
                      (goto-char first-start)
                      (line-end-position)))
         (second-start (save-excursion
                         (goto-char first-end)
                         (forward-line 1)
                         (point)))
         (second-end (save-excursion
                       (goto-char second-start)
                       (line-end-position)))
         (symbol-start (save-excursion
                         (goto-char (point-min))
                         (search-forward "message")
                         (match-beginning 0)))
         (symbol-end (save-excursion
                       (goto-char symbol-start)
                       (search-forward "message")
                       (match-end 0)))
         (string-start (save-excursion
                         (goto-char (point-min))
                         (search-forward "\"hi\"")
                         (match-beginning 0)))
         (string-end (save-excursion
                       (goto-char string-start)
                       (search-forward "\"hi\"")
                       (match-end 0)))
         (symbol (codex-ide-mcp-test--node
                  "symbol" symbol-start symbol-end t "function"))
         (string (codex-ide-mcp-test--node
                  "string" string-start string-end t "argument"))
         (first (codex-ide-mcp-test--node
                 "list" first-start first-end t nil
                 (list symbol string)))
         (second (codex-ide-mcp-test--node
                  "list" second-start second-end t))
         (root (codex-ide-mcp-test--node
                "source_file" (point-min) (point-max) t nil
                (list first second))))
    (list :root root
          :first-start first-start
          :symbol-start symbol-start
          :symbol-end symbol-end)))

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

(ert-deftest codex-ide-mcp-start-server-rejects-non-loopback-host ()
  "Server startup rejects a non-loopback bind address."
  (let ((codex-ide-mcp-host "0.0.0.0")
        called)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest _args)
                 (setq called t))))
      (should-error (codex-ide-mcp--start-server) :type 'user-error)
      (should-not called))))

(ert-deftest codex-ide-mcp-start-server-rolls-back-port-failure ()
  "Server startup deletes its listener when port discovery fails."
  (let ((codex-ide-mcp--server nil)
        (codex-ide-mcp--port nil)
        deleted)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest _) 'listener))
              ((symbol-function 'set-process-coding-system)
               (lambda (&rest _) nil))
              ((symbol-function 'codex-ide-mcp--contact-port)
               (lambda (_) (error "port lookup failed")))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (should-error (codex-ide-mcp--start-server) :type 'error)
      (should (eq deleted 'listener))
      (should-not codex-ide-mcp--server)
      (should-not codex-ide-mcp--port))))

(ert-deftest codex-ide-mcp-url-brackets-ipv6-loopback ()
  "MCP URLs bracket an IPv6 loopback address."
  (let ((codex-ide-mcp-host "::1")
        (codex-ide-mcp--port 43210))
    (should (equal (codex-ide-mcp--url)
                   "http://[::1]:43210/mcp"))))

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
  "Running status reports server key, namespace, URL, and harness tools."
  (let ((codex-ide-mcp-port 43210))
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
                 "Example tool: mcp__emacs_tools__emacs_context"
                 status))
        (should (string-match-p "Harness tools: emacs_execute" status))
        (should-not (string-match-p "disabled" status))))))

(ert-deftest codex-ide-mcp-status-message-stopped-ephemeral ()
  "Stopped status reports ephemeral port setup without execute gating."
  (let ((codex-ide-mcp-port 0))
    (cl-letf (((symbol-function 'codex-ide-mcp--running-p)
               (lambda () nil)))
      (let ((status (codex-ide-mcp--status-message)))
        (should (string-match-p "stopped" status))
        (should (string-match-p "Port: ephemeral" status))
        (should (string-match-p "Harness tools: emacs_execute" status))
        (should-not (string-match-p "Persistent setup command" status))
        (should-not (string-match-p "disabled" status))))))

(ert-deftest codex-ide-mcp-session-overrides-disabled ()
  "Disabled MCP integration leaves session overrides unchanged."
  (let ((codex-ide-config-overrides '(("model" . "o3")))
        (codex-ide-mcp-enabled nil))
    (cl-letf (((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda () (error "should not start MCP server"))))
      (should (equal (codex-ide--session-config-overrides)
                     '(("model" . "o3")))))))

(ert-deftest codex-ide-mcp-session-overrides-default-enabled ()
  "Default MCP integration appends the transient server URL override."
  (let ((codex-ide-config-overrides '(("model" . "o3")))
        (started nil))
    (cl-letf (((symbol-function 'codex-ide-mcp-ensure-server)
               (lambda ()
                 (setq started t)
                 "http://127.0.0.1:43210/mcp")))
      (should (equal (codex-ide--session-config-overrides)
                     '(("model" . "o3")
                       ("mcp_servers.emacs_tools.url"
                        . "\"http://127.0.0.1:43210/mcp\""))))
      (should started))))

(ert-deftest codex-ide-mcp-build-command-session-overrides ()
  "Session-local MCP overrides are visible to command construction."
  (let ((codex-ide-config-overrides nil)
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
  (let* ((tool (codex-ide-mcp--tool-by-name "emacs_execute"))
         (schema (codex-ide-mcp--tool->mcp tool))
         (input (cdr (assoc "inputSchema" schema)))
         (properties (cdr (assoc "properties" input))))
    (should (equal (cdr (assoc "name" schema)) "emacs_execute"))
    (should (equal (cdr (assoc "type" input)) "object"))
    (should (equal (cdr (assoc "required" input)) ["code"]))
    (should (equal (cdr (assoc "type" (cdr (assoc "code" properties))))
                   "string"))
    (should (equal (cdr (assoc "type" (cdr (assoc "buffer" properties))))
                   "string"))))

(ert-deftest codex-ide-mcp-tool-to-mcp-schema-annotations ()
  "Tool schemas carry effect annotations for the Codex approval gate."
  (let* ((context (codex-ide-mcp--tool->mcp
                   (codex-ide-mcp--tool-by-name "emacs_context")))
         (execute (codex-ide-mcp--tool->mcp
                   (codex-ide-mcp--tool-by-name "emacs_execute")))
         (read-only (cdr (assoc "annotations" context)))
         (mutating (cdr (assoc "annotations" execute))))
    (should (eq (cdr (assoc "readOnlyHint" read-only)) t))
    (should (eq (cdr (assoc "idempotentHint" read-only)) t))
    (should (eq (cdr (assoc "openWorldHint" read-only)) :json-false))
    (should (eq (cdr (assoc "destructiveHint" mutating)) t))))

(ert-deftest codex-ide-mcp-tools-list-includes-annotations ()
  "Every tool in tools/list carries an annotations object."
  (let* ((result (codex-ide-mcp--handle-tools-list nil))
         (tools (append (cdr (assoc "tools" result)) nil)))
    (dolist (tool tools)
      (should (consp (cdr (assoc "annotations" tool)))))))

(ert-deftest codex-ide-mcp-control-tools-available-by-default ()
  "The MCP bridge exposes execute, edit, and job control by default."
  (dolist (name '("emacs_execute" "emacs_edit" "emacs_job"))
    (should (member name (codex-ide-mcp-tool-names)))
    (should (codex-ide-mcp--tool-by-name name))))

(ert-deftest codex-ide-mcp-client-limit-rejects-new-connection ()
  "A full MCP client table rejects and closes a new connection."
  (let ((codex-ide-mcp-max-clients 1)
        (codex-ide-mcp--clients (make-hash-table :test 'eq))
        deleted)
    (puthash 'existing '(:pending "") codex-ide-mcp--clients)
    (cl-letf (((symbol-function 'delete-process)
               (lambda (proc) (setq deleted proc))))
      (should-error (codex-ide-mcp--client-state 'new) :type 'user-error)
      (should (eq deleted 'new))
      (should (= (hash-table-count codex-ide-mcp--clients) 1)))))

(ert-deftest codex-ide-mcp-client-limit-covers-idle-connections ()
  "Idle TCP clients are admitted, capped, and closed by server stop."
  (let ((codex-ide-mcp-max-clients 1)
        (codex-ide-mcp-port 0)
        (codex-ide-mcp--server nil)
        (codex-ide-mcp--port nil)
        (codex-ide-mcp--clients (make-hash-table :test 'eq))
        outgoing admitted)
    (unwind-protect
        (progn
          (codex-ide-mcp--start-server)
          (dotimes (index 2)
            (push (make-network-process
                   :name (format "codex-ide-mcp-idle-%d" index)
                   :host "127.0.0.1" :service codex-ide-mcp--port
                   :noquery t)
                  outgoing))
          (let ((deadline (+ (float-time) 2)))
            (while (and (< (float-time) deadline)
                        (< (hash-table-count codex-ide-mcp--clients) 1))
              (accept-process-output nil 0.02)))
          (should (= (hash-table-count codex-ide-mcp--clients) 1))
          (maphash (lambda (proc _state) (push proc admitted))
                   codex-ide-mcp--clients)
          (codex-ide-mcp--stop-server)
          (should (cl-every (lambda (proc) (not (process-live-p proc)))
                            admitted)))
      (codex-ide-mcp--stop-server)
      (dolist (proc outgoing)
        (when (process-live-p proc) (delete-process proc))))))

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

(ert-deftest codex-ide-mcp-tool-to-mcp-schema-edit ()
  "Edit schema requires operation and keeps edit coordinates optional."
  (let* ((tool (codex-ide-mcp--tool-by-name "emacs_edit"))
         (schema (codex-ide-mcp--tool->mcp tool))
         (input (cdr (assoc "inputSchema" schema)))
         (properties (cdr (assoc "properties" input))))
    (should (equal (cdr (assoc "name" schema)) "emacs_edit"))
    (should (equal (cdr (assoc "required" input)) ["operation"]))
    (should (equal (cdr (assoc "type" (cdr (assoc "start" properties))))
                   "integer"))
    (should (equal (cdr (assoc "type" (cdr (assoc "indent" properties))))
                   "boolean"))))

(ert-deftest codex-ide-mcp-tools-list-shape-default ()
  "tools/list returns the core harness schemas."
  (let* ((result (codex-ide-mcp--handle-tools-list nil))
         (tools (cdr (assoc "tools" result)))
         (names (mapcar (lambda (tool) (cdr (assoc "name" tool)))
                        (append tools nil))))
    (should (vectorp tools))
    (should (equal names
                   '("emacs_execute"
                     "emacs_context"
                     "emacs_edit"
                     "emacs_job"
                     "emacs_events")))))

(ert-deftest codex-ide-mcp-callable-name-display-only ()
  "Codex callable names are display-only and raw MCP names stay unchanged."
  (let ((names (codex-ide-mcp-tool-names)))
    (should (equal (codex-ide-mcp--callable-tool-name "emacs_context")
                   "mcp__emacs_tools__emacs_context"))
    (should (member "emacs_context" names))
    (should-not (member "mcp__emacs_tools__emacs_context" names))))

(ert-deftest codex-ide-mcp-tools-call-execute-evals-multiple-forms ()
  "Execute evaluates every readable form and returns the final value."
  (unwind-protect
      (let* ((result (codex-ide-mcp--handle-tools-call
                      '(:name "emacs_execute"
                        :arguments
                        (:code "(put 'codex-ide-mcp-test 'value 4)
(+ (get 'codex-ide-mcp-test 'value) 5)"))))
             (decoded (codex-ide-mcp-test--decoded-result result)))
        (should (eq (cdr (assoc "isError" result)) :json-false))
        (should (eq (cdr (assoc "ok" decoded)) t))
        (should (equal (cdr (assoc "value" decoded)) "9")))
    (put 'codex-ide-mcp-test 'value nil)))

(ert-deftest codex-ide-mcp-tools-call-execute-captures-output-messages ()
  "Execute returns printed output and messages emitted during execution."
  (let* ((result (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_execute"
                    :arguments
                    (:code "(princ \"printed\")
(let ((inhibit-message t))
  (message \"hello %s\" \"world\"))
42"))))
         (decoded (codex-ide-mcp-test--decoded-result result))
         (messages (cdr (assoc "messages" decoded))))
    (should (eq (cdr (assoc "isError" result)) :json-false))
    (should (equal (cdr (assoc "value" decoded)) "42"))
    (should (equal (cdr (assoc "output" decoded)) "printed"))
    (should (member "hello world" messages))))

(ert-deftest codex-ide-mcp-tools-call-execute-returns-structured-error ()
  "Execute returns structured errors instead of failing the tool call."
  (let* ((result (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_execute"
                    :arguments (:code "(let ((inhibit-message t))
  (message \"before boom\"))
(error \"boom\")"))))
         (decoded (codex-ide-mcp-test--decoded-result result))
         (error-data (cdr (assoc "error" decoded))))
    (should (eq (cdr (assoc "isError" result)) :json-false))
    (should-not (cdr (assoc "ok" decoded)))
    (should (equal (cdr (assoc "message" error-data)) "boom"))
    (should (stringp (cdr (assoc "backtrace" error-data))))))

(ert-deftest codex-ide-mcp-tools-call-execute-selects-context-args ()
  "Execute can run in a buffer/path/directory context."
  (let* ((dir (make-temp-file "codex-ide-mcp-execute-dir" t))
         (file (expand-file-name "context.el" dir))
         (buffer nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert ";; context\n"))
          (setq buffer (find-file-noselect file))
          (let* ((result (codex-ide-mcp--handle-tools-call
                          `(:name "emacs_execute"
                            :arguments
                            (:code "(list (buffer-name) default-directory)"
                             :path ,file
                             :directory ,dir))))
                 (decoded (codex-ide-mcp-test--decoded-result result))
                 (current (cdr (assoc "currentBuffer" decoded))))
            (should (eq (cdr (assoc "isError" result)) :json-false))
            (should (string-match-p "context.el"
                                    (cdr (assoc "value" decoded))))
            (should (equal (cdr (assoc "path" current))
                           (expand-file-name file)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest codex-ide-mcp-tools-call-edit-insert-indents ()
  "Edit insert applies text to a live buffer and calls indent-region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (indented)
      (cl-letf (((symbol-function 'indent-region)
                 (lambda (beg end &rest _args)
                   (setq indented (list beg end)))))
        (let* ((result (codex-ide-mcp--handle-tools-call
                        `(:name "emacs_edit"
                          :arguments
                          (:operation "insert"
                           :text "(message \"x\")"
                           :buffer ,(buffer-name)
                           :start ,(point-min)))))
               (decoded (codex-ide-mcp-test--decoded-result result)))
          (should (eq (cdr (assoc "isError" result)) :json-false))
          (should (equal (buffer-string) "(message \"x\")"))
          (should (equal (cdr (assoc "operation" decoded)) "insert"))
          (should indented))))))

(ert-deftest codex-ide-mcp-tools-call-edit-replace-indents ()
  "Edit replace applies text to a live buffer and calls indent-region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(old)\n")
    (let (indented)
      (cl-letf (((symbol-function 'indent-region)
                 (lambda (beg end &rest _args)
                   (setq indented (list beg end)))))
        (let* ((result (codex-ide-mcp--handle-tools-call
                        `(:name "emacs_edit"
                          :arguments
                          (:operation "replace"
                           :text "(new)\n"
                           :buffer ,(buffer-name)
                           :start ,(point-min)
                           :end ,(point-max)))))
               (decoded (codex-ide-mcp-test--decoded-result result)))
          (should (eq (cdr (assoc "isError" result)) :json-false))
          (should (equal (buffer-string) "(new)\n"))
          (should (equal (cdr (assoc "operation" decoded)) "replace"))
          (should indented))))))

(ert-deftest codex-ide-mcp-edit-rejects-line-past-buffer ()
  "Line-based edits reject a line that does not exist."
  (with-temp-buffer
    (insert "abc\ndef")
    (should-error
     (codex-ide-harness-edit
      `(:operation "insert" :text "x" :buffer ,(buffer-name) :line 99))
     :type 'user-error)))

(ert-deftest codex-ide-mcp-edit-rejects-column-past-eol ()
  "Line-based edits reject a column beyond the line ending."
  (with-temp-buffer
    (insert "abc\ndef")
    (should-error
     (codex-ide-harness-edit
      `(:operation "insert" :text "x" :buffer ,(buffer-name)
        :line 1 :column 99))
     :type 'user-error)))

(ert-deftest codex-ide-mcp-edit-delete-requires-positions ()
  "Delete without positions signals a clear user-error."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error
                (codex-ide-harness-edit
                 `(:operation "delete" :buffer ,(buffer-name)))
                :type 'user-error)))
      (should (string-match-p "Delete requires start or line"
                              (error-message-string err))))))

(ert-deftest codex-ide-mcp-edit-replace-requires-end-position ()
  "Replace without an end position signals a clear user-error."
  (with-temp-buffer
    (insert "abc")
    (let ((err (should-error
                (codex-ide-harness-edit
                 `(:operation "replace"
                   :text "x"
                   :buffer ,(buffer-name)
                   :start ,(point-min)))
                :type 'user-error)))
      (should (string-match-p "Replace requires end or end_line"
                              (error-message-string err))))))

(defun codex-ide-mcp-test--wait-for-job (job-id)
  "Poll harness JOB-ID until it is no longer running."
  (let ((deadline (+ (float-time) 2))
        result)
    (while (and (< (float-time) deadline)
                (or (not result)
                    (equal (cdr (assoc "status" result)) "running")))
      (accept-process-output nil 0.05)
      (setq result
            (codex-ide-mcp-test--decoded-result
             (codex-ide-mcp--handle-tools-call
              `(:name "emacs_job"
                :arguments (:action "poll" :job_id ,job-id))))))
    result))

(ert-deftest codex-ide-mcp-tools-call-job-start-poll-read ()
  "Job tool starts, polls, and reads async process output."
  (let* ((start (codex-ide-mcp-test--decoded-result
                 (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_job"
                    :arguments (:action "start"
                                :command "printf harness-job")))))
         (job-id (cdr (assoc "id" start)))
         (done (codex-ide-mcp-test--wait-for-job job-id))
         (read (codex-ide-mcp-test--decoded-result
                (codex-ide-mcp--handle-tools-call
                 `(:name "emacs_job"
                   :arguments (:action "read"
                               :job_id ,job-id
                               :since 0)))))
         (output (cdr (assoc "output" read))))
    (should (member (cdr (assoc "status" done)) '("done" "failed")))
    (should (equal (cdr (assoc "text" output)) "harness-job"))))

(ert-deftest codex-ide-mcp-tools-call-job-cancel ()
  "Job tool cancels a running async process."
  (let* ((start (codex-ide-mcp-test--decoded-result
                 (codex-ide-mcp--handle-tools-call
                  '(:name "emacs_job"
                    :arguments (:action "start"
                                :command "sleep 5")))))
         (job-id (cdr (assoc "id" start)))
         (cancel (codex-ide-mcp-test--decoded-result
                  (codex-ide-mcp--handle-tools-call
                   `(:name "emacs_job"
                     :arguments (:action "cancel" :job_id ,job-id))))))
    (should (equal (cdr (assoc "status" cancel)) "canceled"))))

(ert-deftest codex-ide-mcp-tools-call-events-after-cursor ()
  "Events tool returns harness events after a cursor."
  (let ((cursor codex-ide-harness--event-cursor))
    (codex-ide-mcp--handle-tools-call
     '(:name "emacs_execute" :arguments (:code "(+ 1 2)")))
    (let* ((result (codex-ide-mcp--handle-tools-call
                    `(:name "emacs_events"
                      :arguments (:since ,cursor))))
           (decoded (codex-ide-mcp-test--decoded-result result))
           (events (cdr (assoc "events" decoded))))
      (should (eq (cdr (assoc "isError" result)) :json-false))
      (should events)
      (should (equal (cdr (assoc "type" (car events))) "execute")))))

(ert-deftest codex-ide-mcp-events-since-preserves-event-log ()
  "Reading events with a stale cursor must not mutate the global log."
  (let ((codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0))
    (codex-ide-harness--record-event "one" nil)
    (codex-ide-harness--record-event "two" nil)
    (codex-ide-harness--record-event "three" nil)
    (let ((first (codex-ide-harness--events-since 0 100))
          (second (codex-ide-harness--events-since 0 100)))
      (should (equal first second))
      (should (equal (mapcar (lambda (event) (cdr (assoc "type" event)))
                             codex-ide-harness--events)
                     '("three" "two" "one"))))))

(ert-deftest codex-ide-mcp-tools-call-tree-sitter-unavailable ()
  "Tree-sitter helper signals when treesit is absent."
  (with-temp-buffer
    (cl-letf (((symbol-function 'treesit-available-p)
               (lambda () nil)))
      (should-error (codex-ide-harness-tree-sitter nil)
                    :type 'user-error))))

(ert-deftest codex-ide-mcp-tools-call-tree-sitter-no-parser ()
  "Tree-sitter helper signals without a parser."
  (with-temp-buffer
    (let ((tree (codex-ide-mcp-test--sample-tree)))
      (codex-ide-mcp-test--with-treesit (plist-get tree :root)
        (cl-letf (((symbol-function 'treesit-parser-list)
                   (lambda (&rest _args) nil)))
          (should-error (codex-ide-harness-tree-sitter nil)
                        :type 'user-error))))))

(ert-deftest codex-ide-mcp-tools-call-tree-sitter-node-at-point ()
  "Tree-sitter info returns structured node data at point."
  (with-temp-buffer
    (let* ((tree (codex-ide-mcp-test--sample-tree))
           (symbol-start (plist-get tree :symbol-start))
           (symbol-end (plist-get tree :symbol-end)))
      (goto-char symbol-start)
      (codex-ide-mcp-test--with-treesit (plist-get tree :root)
        (let* ((decoded (codex-ide-harness-tree-sitter nil))
               (parser (cdr (assoc "parser" decoded)))
               (node (cdr (assoc "node" decoded)))
               (point-range (cdr (assoc "pointRange" node)))
               (byte-range (cdr (assoc "byteRange" node)))
               (range (cdr (assoc "range" node)))
               (range-start (cdr (assoc "start" range))))
          (should (equal (cdr (assoc "language" parser)) "elisp"))
          (should (equal (cdr (assoc "type" node)) "symbol"))
          (should (eq (cdr (assoc "named" node)) t))
          (should (equal (cdr (assoc "fieldName" node)) "function"))
          (should (equal (cdr (assoc "text" node)) "message"))
          (should (equal (cdr (assoc "start" point-range)) symbol-start))
          (should (equal (cdr (assoc "end" point-range)) symbol-end))
          (should (equal (cdr (assoc "start" byte-range))
                         (- (position-bytes symbol-start)
                            (position-bytes (point-min)))))
          (should (equal (cdr (assoc "line" range-start)) 1))
          (should (equal (cdr (assoc "column" range-start)) 1)))))))

(ert-deftest codex-ide-mcp-tools-call-tree-sitter-ancestors-children ()
  "Tree-sitter info can include ancestors and bounded children."
  (with-temp-buffer
    (let ((tree (codex-ide-mcp-test--sample-tree)))
      (goto-char (plist-get tree :first-start))
      (codex-ide-mcp-test--with-treesit (plist-get tree :root)
        (let* ((decoded (codex-ide-harness-tree-sitter
                         '(:include_ancestors t
                           :include_children t
                           :max_children 1)))
               (node (cdr (assoc "node" decoded)))
               (ancestors (cdr (assoc "ancestors" decoded)))
               (children (cdr (assoc "children" decoded)))
               (first-child (aref children 0)))
          (should (equal (cdr (assoc "type" node)) "list"))
          (should (equal (length ancestors) 1))
          (should (equal (cdr (assoc "type" (aref ancestors 0)))
                         "source_file"))
          (should (equal (length children) 1))
          (should (equal (cdr (assoc "type" first-child)) "symbol"))
          (should (eq (cdr (assoc "childrenTruncated" decoded)) t)))))))

(ert-deftest codex-ide-mcp-tools-call-tree-sitter-whole-file ()
  "Tree-sitter info can return a bounded whole-file tree."
  (with-temp-buffer
    (let ((tree (codex-ide-mcp-test--sample-tree)))
      (codex-ide-mcp-test--with-treesit (plist-get tree :root)
        (let* ((decoded (codex-ide-harness-tree-sitter
                         '(:whole_file t
                           :max_depth 1
                           :max_children 1)))
               (root (cdr (assoc "tree" decoded)))
               (children (cdr (assoc "children" root)))
               (first-child (aref children 0)))
          (should (equal (cdr (assoc "type" root)) "source_file"))
          (should (equal (length children) 1))
          (should (equal (cdr (assoc "type" first-child)) "list"))
          (should-not (assoc "children" first-child))
          (should (eq (cdr (assoc "childrenTruncated" root)) t)))))))

(ert-deftest codex-ide-mcp-tools-call-xref-references-no-buffer ()
  "Xref helper requires an already-open buffer."
  (let ((path (make-temp-file "codex-ide-mcp-xref")))
    (unwind-protect
        (should-error
         (codex-ide-harness-xref
          `(:path ,path :identifier "foo"))
         :type 'user-error)
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest codex-ide-mcp-tools-call-project-info-no-project ()
  "Context reports nil project root outside a project."
  (let ((dir (make-temp-file "codex-ide-mcp-no-project" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir))
              (project-find-functions nil))
          (with-temp-buffer
            (rename-buffer "codex-ide-mcp-no-project" t)
            (let* ((result (codex-ide-mcp--handle-tools-call
                            '(:name "emacs_context"
                              :arguments nil)))
                   (decoded (codex-ide-mcp-test--decoded-result result))
                   (project (cdr (assoc "project" decoded)))
                   (buffer (cdr (assoc "buffer" decoded))))
              (should (eq (cdr (assoc "isError" result)) :json-false))
              (should-not (cdr (assoc "root" project)))
              (should (equal (cdr (assoc "fileCount" project)) 0))
              (should (equal (cdr (assoc "buffer" buffer))
                             "codex-ide-mcp-no-project"))
              (should (equal (cdr (assoc "majorMode" buffer))
                             "fundamental-mode")))))
      (delete-directory dir t))))

(ert-deftest codex-ide-mcp-tools-call-context-current-buffer ()
  "Context tool returns normal MCP text content."
  (with-temp-buffer
    (rename-buffer "codex-ide-mcp-test" t)
    (let* ((result (codex-ide-mcp--handle-tools-call
                    '(:name "emacs_context" :arguments nil)))
           (content (aref (cdr (assoc "content" result)) 0))
           (decoded (codex-ide-mcp-test--json-read
                     (cdr (assoc "text" content))))
           (buffer (cdr (assoc "buffer" decoded))))
      (should (eq (cdr (assoc "isError" result)) :json-false))
      (should (equal (cdr (assoc "type" content)) "text"))
      (should (equal (cdr (assoc "buffer" buffer))
                     "codex-ide-mcp-test")))))

(ert-deftest codex-ide-mcp-tools-call-edit-path-requires-live-buffer ()
  "Edit by path reports an MCP error unless the buffer is live."
  (let ((path (make-temp-file "codex-ide-mcp-edit")))
    (unwind-protect
        (let* ((result (codex-ide-mcp--handle-tools-call
                        `(:name "emacs_edit"
                          :arguments (:operation "insert"
                                      :text "x"
                                      :path ,path))))
               (content (aref (cdr (assoc "content" result)) 0)))
          (should (eq (cdr (assoc "isError" result)) t))
          (should (string-match-p "No open buffer"
                                  (cdr (assoc "text" content)))))
      (delete-file path))))

(ert-deftest codex-ide-mcp-handle-message-accepts-notification ()
  "Valid JSON-RPC notifications are accepted without a response body."
  (should (eq (codex-ide-mcp--handle-message
               '(:jsonrpc "2.0" :method "notifications/initialized"))
              'accepted)))

(ert-deftest codex-ide-mcp-handle-message-accepts-client-response ()
  "Valid JSON-RPC client responses are accepted without dispatch."
  (dolist (id '(7 7.5 "request-7"))
    (should (eq (codex-ide-mcp--handle-message
                 (list :jsonrpc "2.0" :id id :result '(:ok t)))
                'accepted))))

(ert-deftest codex-ide-mcp-handle-message-rejects-invalid-shapes ()
  "Malformed JSON-RPC objects return invalid-request errors."
  (dolist (message '((:jsonrpc "1.0" :id 7 :method "tools/list")
                     (:jsonrpc "2.0")
                     (:jsonrpc "2.0" :id nil :method "tools/list")))
    (let ((response (codex-ide-mcp--handle-message message)))
      (should (= (cdr (assoc "code" (cdr (assoc "error" response))))
                 -32600)))))

(defun codex-ide-mcp-test--modern-request (method &optional params)
  "Build a modern HTTP request for METHOD with PARAMS."
  (list :method "POST" :path "/mcp"
        :headers (append (copy-tree '(("host" . "127.0.0.1")
                           ("content-type" . "application/json")
                           ("mcp-protocol-version" . "2026-07-28")))
                         (list (cons "mcp-method" method))
                         (when (equal method "tools/call")
                           (list (cons "mcp-name" (plist-get params :name)))))
        :body (json-encode
               (list :jsonrpc "2.0" :id 42 :method method
                     :params (append params
                                     (list :_meta
                                           (list :io.modelcontextprotocol/protocolVersion
                                                 "2026-07-28"
                                                 :io.modelcontextprotocol/clientCapabilities
                                                 (make-hash-table))))))))

(defun codex-ide-mcp-test--http-response (request)
  "Return (STATUS BODY) produced by HTTP REQUEST."
  (let (sent)
    (cl-letf (((symbol-function 'codex-ide-mcp--send-json)
               (lambda (_proc status body) (setq sent (list status body))))
              ((symbol-function 'codex-ide-mcp--selected-buffer)
               #'current-buffer))
      (codex-ide-mcp--handle-http-request 'client request))
    sent))

(ert-deftest codex-ide-mcp-modern-discovery-and-inline-tools ()
  "Modern tools work before discovery, with complete cacheable results."
  (dolist (method '("tools/list" "server/discover"))
    (pcase-let* ((`(,status ,body) (codex-ide-mcp-test--http-response
                                  (codex-ide-mcp-test--modern-request method)))
                 (result (alist-get "result" body nil nil #'equal)))
      (should (= status 200))
      (should (= (alist-get "id" body nil nil #'equal) 42))
      (should (equal (alist-get "resultType" result nil nil #'equal) "complete"))
      (should (equal (alist-get "ttlMs" result nil nil #'equal) 0))
      (should (equal (alist-get "cacheScope" result nil nil #'equal) "private"))
      (should (codex-ide-mcp--object-get
               (codex-ide-mcp--object-get result "_meta")
               "io.modelcontextprotocol/serverInfo"))
      (if (equal method "tools/list")
          (should (vectorp (alist-get "tools" result nil nil #'equal)))
        (should (equal (alist-get "supportedVersions" result nil nil #'equal)
                       ["2026-07-28" "2025-06-18"]))))))

(ert-deftest codex-ide-mcp-modern-rejections-do-not-call-tools ()
  "Invalid modern metadata and routing never execute a tool."
  (let ((calls 0))
    (cl-letf (((symbol-function 'codex-ide-mcp--handle-tools-call)
               (lambda (_) (cl-incf calls) '(("content" . [])))))
      (dolist (case '((header "mcp-method" nil -32020)
                      (header "mcp-method" "tools/list" -32020)
                      (header "mcp-name" nil -32020)
                      (header "mcp-name" "other" -32020)
                      (header "mcp-name" "=?base64?!!!?=" -32020)
                      (header "mcp-protocol-version" nil -32020)
                      (header "mcp-protocol-version" "2025-06-18" -32020)
                      (meta :io.modelcontextprotocol/protocolVersion nil -32602)
                      (meta :io.modelcontextprotocol/clientCapabilities nil -32602)
                      (meta :io.modelcontextprotocol/clientCapabilities [] -32602)))
        (let* ((request (codex-ide-mcp-test--modern-request
                         "tools/call" '(:name "test")))
               (field (nth 1 case))
               (value (nth 2 case)))
          (if (eq (car case) 'header)
              (setf (alist-get field (plist-get request :headers) nil nil #'equal)
                    value)
            (let* ((message (let ((json-object-type 'plist))
                              (json-read-from-string (plist-get request :body))))
                   (meta (plist-get (plist-get message :params) :_meta)))
              (setf (plist-get meta field) value)
              (setf (plist-get request :body) (json-encode message))))
          (pcase-let ((`(,status ,body) (codex-ide-mcp-test--http-response request)))
            (should (= status 400))
            (should (= (codex-ide-mcp--object-get
                        (codex-ide-mcp--object-get body "error") "code")
                       (nth 3 case))))))
      (should (= calls 0)))))

(ert-deftest codex-ide-mcp-modern-malformed-body-does-not-call-tools ()
  "Malformed IDs and trailing JSON never execute tools."
  (let ((calls 0))
    (cl-letf (((symbol-function 'codex-ide-mcp--handle-tools-call)
               (lambda (_) (cl-incf calls))))
      (dolist (suffix '(" {}" " garbage"))
        (let ((request (codex-ide-mcp-test--modern-request "tools/call" '(:name "test"))))
          (setf (plist-get request :body) (concat (plist-get request :body) suffix))
          (pcase-let ((`(,status ,body) (codex-ide-mcp-test--http-response request)))
            (should (= status 400))
            (should-not (codex-ide-mcp--object-has-key-p body "id"))
            (should (= (codex-ide-mcp--object-get
                        (codex-ide-mcp--object-get body "error") "code") -32700)))))
      (dolist (replacement '("null" "1.5" "[]"))
        (let ((request (codex-ide-mcp-test--modern-request "tools/call" '(:name "test"))))
          (setf (plist-get request :body)
                (replace-regexp-in-string "\"id\":42" (concat "\"id\":" replacement)
                                          (plist-get request :body)))
          (pcase-let ((`(,status ,body) (codex-ide-mcp-test--http-response request)))
            (should (= status 400))
            (should (= (codex-ide-mcp--object-get
                        (codex-ide-mcp--object-get body "error") "code") -32600)))))
      (should (= calls 0)))))

(ert-deftest codex-ide-mcp-modern-version-and-method-errors ()
  "Modern errors retain request identity and expose supported versions."
  (let ((request (codex-ide-mcp-test--modern-request "tools/list")))
    (setf (alist-get "mcp-protocol-version" (plist-get request :headers)
                     nil nil #'equal) "2099-01-01")
    (setf (plist-get request :body)
          (replace-regexp-in-string "2026-07-28" "2099-01-01"
                                    (plist-get request :body)))
    (pcase-let* ((`(,status ,body) (codex-ide-mcp-test--http-response request))
                 (error (codex-ide-mcp--object-get body "error"))
                 (data (codex-ide-mcp--object-get error "data")))
      (should (= status 400))
      (should (= (codex-ide-mcp--object-get body "id") 42))
      (should (= (codex-ide-mcp--object-get error "code") -32022))
      (should (equal (codex-ide-mcp--object-get data "requested") "2099-01-01"))
      (should (equal (codex-ide-mcp--object-get data "supported")
                     ["2026-07-28" "2025-06-18"]))))
  (dolist (method '("unknown" "initialize"))
    (pcase-let ((`(,status ,body) (codex-ide-mcp-test--http-response
                                 (codex-ide-mcp-test--modern-request method))))
      (should (= status 404))
      (should (= (codex-ide-mcp--object-get
                  (codex-ide-mcp--object-get body "error") "code") -32601)))))

(ert-deftest codex-ide-mcp-modern-base64-name-and-legacy-interleave ()
  "Encoded names work and modern requests never change legacy behavior."
  (let ((request (codex-ide-mcp-test--modern-request "tools/call" '(:name "test"))))
    (setf (alist-get "mcp-name" (plist-get request :headers) nil nil #'equal)
          "=?base64?dGVzdA==?=")
    (cl-letf (((symbol-function 'codex-ide-mcp--handle-tools-call)
               (lambda (_) (codex-ide-mcp--text-error-result "Tool failed"))))
      (pcase-let* ((`(,status ,body) (codex-ide-mcp-test--http-response request))
                   (result (codex-ide-mcp--object-get body "result")))
        (should (= status 200))
        (should (equal (codex-ide-mcp--object-get result "resultType") "complete"))
        (should (eq (codex-ide-mcp--object-get result "isError") t))))
    (let* ((legacy (codex-ide-mcp-test--request))
           (_ (setf (plist-get legacy :body)
                    "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"initialize\"}"))
           (response (codex-ide-mcp-test--http-response legacy))
           (result (codex-ide-mcp--object-get (cadr response) "result")))
      (should (= (car response) 200))
      (should (equal (codex-ide-mcp--object-get result "protocolVersion") "2025-06-18"))
      (should-not (codex-ide-mcp--object-get result "resultType")))))

(ert-deftest codex-ide-mcp-http-notification-returns-accepted ()
  "HTTP notifications receive 202 Accepted and no JSON body."
  (let (sent)
    (cl-letf (((symbol-function 'codex-ide-mcp--send-json)
               (lambda (_proc status body) (setq sent (list status body))))
              ((symbol-function 'codex-ide-mcp--selected-buffer)
               #'current-buffer))
      (codex-ide-mcp--handle-http-request
       'client
       (list :method "POST" :path "/mcp"
             :headers '(("host" . "127.0.0.1")
                        ("content-type" . "application/json"))
             :body "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))
    (should (equal sent '(202 nil)))))

(ert-deftest codex-ide-mcp-handle-message-wraps-tools-list ()
  "JSON-RPC messages are wrapped in a success response."
  (let* ((response (codex-ide-mcp--handle-message
                    '(:jsonrpc "2.0" :id 7 :method "tools/list")))
         (result (cdr (assoc "result" response))))
    (should (equal (cdr (assoc "jsonrpc" response)) "2.0"))
    (should (equal (cdr (assoc "id" response)) 7))
    (should (vectorp (cdr (assoc "tools" result))))))

(ert-deftest codex-ide-mcp-content-length-rejects-invalid ()
  "Invalid Content-Length values fail closed."
  (should (equal (codex-ide-mcp--content-length
                  (list :headers '(("content-length" . "12"))))
                 12))
  (should (equal (codex-ide-mcp--content-length
                  (list :headers nil))
                 0))
  (should-not (codex-ide-mcp--content-length
               (list :headers '(("content-length" . "abc")))))
  (should-not (codex-ide-mcp--content-length
               (list :headers '(("content-length" . "-1"))))))

(ert-deftest codex-ide-mcp-split-request-invalid-content-length ()
  "Split returns invalid for non-numeric Content-Length."
  (let ((pending "POST /mcp HTTP/1.1\r\nContent-Length: abc\r\n\r\n{}"))
    (should (eq (codex-ide-mcp--split-request pending) 'invalid))))

(ert-deftest codex-ide-mcp-split-request-negative-content-length ()
  "Split returns invalid for negative Content-Length."
  (let ((pending "POST /mcp HTTP/1.1\r\nContent-Length: -1\r\n\r\n"))
    (should (eq (codex-ide-mcp--split-request pending) 'invalid))))

(ert-deftest codex-ide-mcp-split-request-too-large-body ()
  "Split rejects bodies over the configured max."
  (let* ((codex-ide-mcp-max-request-bytes 8)
         (body "{\"x\":12345}")
         (pending (format "POST /mcp HTTP/1.1\r\nContent-Length: %d\r\n\r\n%s"
                          (string-bytes body) body)))
    (should (eq (codex-ide-mcp--split-request pending) 'too-large))))

(ert-deftest codex-ide-harness-job-output-is-capped ()
  "Retained output preserves absolute cursors after truncation."
  (let* ((codex-ide-harness-job-output-limit 8)
         (job '(:output "" :output-start 0 :output-next 0))
         (first (codex-ide-harness--append-job-output job "abcdef"))
         (second (codex-ide-harness--append-job-output first "ghij"))
         (summary (codex-ide-harness--job-summary second))
         (old-read (codex-ide-harness--job-output second 0))
         (new-read (codex-ide-harness--job-output second 6)))
    (should (equal (plist-get second :output) "cdefghij"))
    (should (= (plist-get second :output-start) 2))
    (should (= (plist-get second :output-next) 10))
    (should (equal (cdr (assoc "outputLength" summary)) 10))
    (should (equal (cdr (assoc "offset" old-read)) 2))
    (should (eq (cdr (assoc "truncated" old-read)) t))
    (should (equal (cdr (assoc "text" old-read)) "cdefghij"))
    (should (equal (cdr (assoc "offset" new-read)) 6))
    (should (equal (cdr (assoc "nextOffset" new-read)) 10))
    (should (equal (cdr (assoc "text" new-read)) "ghij"))))

(ert-deftest codex-ide-harness-cancel-emits-single-terminal-event ()
  "Live cancel emits job-canceled and not job-finished."
  (let ((codex-ide-harness--jobs (make-hash-table :test 'equal))
        (codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0)
        (codex-ide-harness--next-job-id 0))
    (let* ((start (codex-ide-harness-start-job
                   '(:command "sleep 5")))
           (job-id (cdr (assoc "id" start)))
           (cancel (codex-ide-harness--cancel-job
                    (gethash job-id codex-ide-harness--jobs))))
      (accept-process-output nil 0.1)
      (should (equal (cdr (assoc "status" cancel)) "canceled"))
      (should (equal (plist-get (gethash job-id codex-ide-harness--jobs) :status)
                     "canceled"))
      (let ((types (mapcar (lambda (event) (cdr (assoc "type" event)))
                           codex-ide-harness--events)))
        (should (member "job-canceled" types))
        (should-not (member "job-finished" types))))))

(ert-deftest codex-ide-harness-cancel-noop-on-terminal-job ()
  "Cancel leaves finished jobs unchanged."
  (let ((codex-ide-harness--jobs (make-hash-table :test 'equal))
        (codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0)
        (codex-ide-harness--next-job-id 0))
    (let* ((start (codex-ide-harness-start-job
                   '(:command "true")))
           (job-id (cdr (assoc "id" start)))
           (done (progn
                   (let ((deadline (+ (float-time) 2)))
                     (while (and (< (float-time) deadline)
                                 (equal (plist-get
                                         (gethash job-id codex-ide-harness--jobs)
                                         :status)
                                        "running"))
                       (accept-process-output nil 0.05)))
                   (gethash job-id codex-ide-harness--jobs)))
           (before-cursor codex-ide-harness--event-cursor)
           (cancel (codex-ide-harness--cancel-job done)))
      (should (equal (plist-get done :status) "done"))
      (should (equal (cdr (assoc "status" cancel)) "done"))
      (should (equal (plist-get (gethash job-id codex-ide-harness--jobs) :status)
                     "done"))
      (should (= before-cursor codex-ide-harness--event-cursor))
      (should-not (cl-find "job-canceled" codex-ide-harness--events
                           :key (lambda (event) (cdr (assoc "type" event)))
                           :test #'equal)))))

(ert-deftest codex-ide-harness-rejects-job-over-live-cap ()
  "Starting a job at the live-process cap signals `user-error'."
  (let ((codex-ide-harness-job-limit 1)
        (codex-ide-harness--jobs (make-hash-table :test 'equal))
        (codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0)
        (codex-ide-harness--next-job-id 0))
    (unwind-protect
        (progn
          (codex-ide-harness-start-job '(:command "sleep 5"))
          (should-error
           (codex-ide-harness-start-job '(:command "sleep 5"))
           :type 'user-error))
      (codex-ide-harness-reset))))

(ert-deftest codex-ide-harness-reset-clears-jobs-and-cancels-live ()
  "Harness reset cancels live processes and clears tables."
  (let ((codex-ide-harness--jobs (make-hash-table :test 'equal))
        (codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0)
        (codex-ide-harness--next-job-id 0))
    (let* ((start (codex-ide-harness-start-job
                   '(:command "sleep 5")))
           (job-id (cdr (assoc "id" start)))
           (process (plist-get (gethash job-id codex-ide-harness--jobs)
                               :process)))
      (should (process-live-p process))
      (codex-ide-harness-reset)
      (should (= (hash-table-count codex-ide-harness--jobs) 0))
      (should-not codex-ide-harness--events)
      (should (= codex-ide-harness--event-cursor 0))
      (should-not (process-live-p process)))))

(ert-deftest codex-ide-harness-event-fields-are-capped ()
  "Retained event payloads truncate oversized string fields."
  (let ((codex-ide-harness-event-field-limit 40)
        (codex-ide-harness--events nil)
        (codex-ide-harness--event-cursor 0)
        (payload (make-string 200 ?x)))
    (codex-ide-harness--record-event
     "execute" (list (cons "output" payload)
                     (cons "ok" t)))
    (let* ((event (car codex-ide-harness--events))
           (data (cdr (assoc "data" event)))
           (output (cdr (assoc "output" data))))
      (should (< (length output) 200))
      (should (string-match-p "truncated" output)))))

(ert-deftest codex-ide-mcp-edit-rolls-back-indentation-error ()
  "Failed indentation leaves insert and replace targets unchanged."
  (dolist (operation '("insert" "replace"))
    (with-temp-buffer
      (insert "original")
      (set-buffer-modified-p nil)
      (setq-local indent-region-function
                  (lambda (&rest _) (error "Indentation failed")))
      (let ((result (codex-ide-mcp--handle-tools-call
                     `(:name "emacs_edit" :arguments
                       (:operation ,operation :buffer ,(buffer-name)
                        :start 1 :end 9 :text "replacement")))))
        (should (eq (cdr (assoc "isError" result)) t))
        (should (equal (buffer-string) "original"))
        (should-not (buffer-modified-p))))))

(ert-deftest codex-ide-mcp-edit-rolls-back-delete-hook-error ()
  "A change hook failure does not leave a partially applied deletion."
  (with-temp-buffer
    (insert "original")
    (add-hook 'after-change-functions
              (lambda (&rest _) (error "Change hook failed")) nil t)
    (let ((result (codex-ide-mcp--handle-tools-call
                   `(:name "emacs_edit" :arguments
                     (:operation "delete" :buffer ,(buffer-name)
                      :start 1 :end 9)))))
      (should (eq (cdr (assoc "isError" result)) t))
      (should (equal (buffer-string) "original")))))

(ert-deftest codex-ide-mcp-execute-empty-does-not-leak-buffer ()
  "Rejected empty scripts leave no output buffers behind."
  (dolist (code '("" " " "; comment only"))
    (let ((before (buffer-list)))
      (unwind-protect
          (progn
            (should-error (codex-ide-harness-execute `(:code ,code))
                          :type 'user-error)
            (should (equal (buffer-list) before)))
        (dolist (buffer (seq-difference (buffer-list) before))
          (kill-buffer buffer))))))

(ert-deftest codex-ide-mcp-edit-invalid-args-preserve-buffer ()
  "Malformed edit fields and missing targets never change a buffer."
  (dolist (modern '(nil t))
    (dolist (fields '((:start "1") (:line "1") (:column "0")
                      (:end 1.5) (:end_line "1") (:end_column "0")
                      (:indent "false") (:indent nil) (:buffer 12)
                      (:path 12) (:directory 12) (:text 12)
                      (:buffer "") (:buffer nil) (:buffer "   ")))
      (with-temp-buffer
        (insert "original")
        (let* ((args (append fields
                             (unless (plist-member fields :buffer)
                               (list :buffer (buffer-name)))
                             (unless (plist-member fields :text) '(:text "x"))
                             '(:operation "insert")))
               (params (list :name "emacs_edit" :arguments args))
               (request (if modern
                            (codex-ide-mcp-test--modern-request "tools/call" params)
                          (let ((legacy (codex-ide-mcp-test--request)))
                            (setf (plist-get legacy :body)
                                  (json-encode (list :jsonrpc "2.0" :id 42
                                                     :method "tools/call"
                                                     :params params)))
                            legacy)))
               (reply (cadr (codex-ide-mcp-test--http-response request)))
               (result (codex-ide-mcp--object-get reply "result")))
          (should (eq (codex-ide-mcp--object-get result "isError") t))
          (should (equal (buffer-string) "original")))))))

(ert-deftest codex-ide-mcp-types-checked-before-handler ()
  "Reject mistyped fields across all tools before invoking their handlers."
  (let* ((calls 0)
         (codex-ide-mcp--tools
          (mapcar (lambda (tool)
                    (plist-put (copy-sequence tool) :function
                               (lambda (_) (setq calls (1+ calls)))))
                  codex-ide-mcp--tools)))
    (dolist (params '((:name "emacs_execute" :arguments (:code 12))
                      (:name "emacs_context" :arguments (:messages "1"))
                      (:name "emacs_job" :arguments (:action "start" :command 12))
                      (:name "emacs_events" :arguments (:since "1"))))
      (should (eq (codex-ide-mcp--object-get
                   (codex-ide-mcp--handle-tools-call params) "isError") t)))
    (should (= calls 0))))

(ert-deftest codex-ide-mcp-edit-requires-explicit-target ()
  "An untargeted edit fails before changing the selected buffer."
  (with-temp-buffer
    (insert "original")
    (should-error (codex-ide-harness-edit
                   '(:operation "insert" :text "x" :indent :json-false))
                  :type 'user-error)
    (should (equal (buffer-string) "original"))))

(ert-deftest codex-ide-mcp-edit-explicit-target-and-false ()
  "An explicit target wins over current buffer, and false skips indentation."
  (let ((target (generate-new-buffer " *codex-target-test*")))
    (unwind-protect
        (with-temp-buffer
          (insert "selected")
          (with-current-buffer target
            (setq-local indent-region-function
                        (lambda (&rest _) (error "Must not indent"))))
          (let ((result (codex-ide-mcp--handle-tools-call
                         (list :name "emacs_edit" :arguments
                               (list :operation "insert" :text "target"
                                     :buffer (buffer-name target)
                                     :indent :json-false)))))
            (should (eq (codex-ide-mcp--object-get result "isError") :json-false))
            (should (equal (buffer-string) "selected"))
            (should (equal (with-current-buffer target (buffer-string)) "target"))))
      (kill-buffer target))))

(ert-deftest codex-ide-mcp-edit-schema-advertises-target ()
  "Edit discovery requires either a buffer or path."
  (let* ((tool (codex-ide-mcp--tool->mcp (codex-ide-mcp--tool-by-name "emacs_edit")))
         (schema (codex-ide-mcp--object-get tool "inputSchema")))
    (should (equal (codex-ide-mcp--object-get schema "anyOf")
                   [(("required" . ["buffer"])) (("required" . ["path"]))]))))

(ert-deftest codex-ide-mcp-custom-registration-atomic ()
  "Reject invalid registrations without changing either catalog."
  (let ((codex-ide-mcp--custom-tools nil)
        (builtins (copy-tree codex-ide-mcp--tools)))
    (codex-ide-mcp-register-tool "custom" "Example" nil #'list)
    (dolist (spec '(("custom" "Duplicate" nil list)
                    ("emacs_execute" "Builtin" nil list)
                    ("bad name" "Invalid" nil list)
                    ("" "Invalid" nil list)
                    ("new" nil nil list)
                    ("new" "Invalid" nil missing-custom-handler)
                    ("new" "Invalid" ((:name "x" :type array)) list)
                    ("new" "Invalid" ((:name "x" :type string :enum ("x"))) list)
                    ("new" "Invalid" ((:name "x" :type string :optional maybe)) list)
                    ("new" "Invalid" ((:name "x" :type string)
                                       (:name "x" :type integer)) list)))
      (should-error (apply #'codex-ide-mcp-register-tool spec) :type 'user-error)
      (should (equal (codex-ide-mcp-tool-names)
                     (append (mapcar (lambda (tool) (plist-get tool :name)) builtins)
                             '("custom")))))
    (should-error (codex-ide-mcp-unregister-tool "emacs_execute") :type 'user-error)
    (should (codex-ide-mcp-unregister-tool "custom"))
    (should-not (codex-ide-mcp-unregister-tool "custom"))
    (should (equal builtins codex-ide-mcp--tools))))

(ert-deftest codex-ide-mcp-custom-explicit-arguments ()
  "Both dispatch modes validate primitives before calling explicit handlers."
  (let ((codex-ide-mcp--custom-tools nil)
        (calls 0) received)
    (codex-ide-mcp-register-tool
     "custom" "Example"
     '((:name "s" :type string) (:name "i" :type integer)
       (:name "n" :type number) (:name "b" :type boolean)
       (:name "optional" :type string :optional t))
     (lambda (s i n b optional)
       (setq received (list s i n b optional) calls (1+ calls))
       "done"))
    (dolist (dispatch '(codex-ide-mcp--dispatch codex-ide-mcp--modern-dispatch))
      (let ((result (funcall dispatch "tools/call"
                             '(:name "custom" :arguments
                               (:b :json-false :n 1.5 :i 2 :s "hello")))))
        (should (equal received '("hello" 2 1.5 :json-false nil)))
        (should (equal (codex-ide-mcp-test--decoded-result result) "done")))
      (dolist (bad '((:s 4 :i 2 :n 1 :b t) (:s "x" :i 2.5 :n 1 :b t)
                     (:s "x" :i 2 :n "1" :b t) (:s "x" :i 2 :n 1 :b nil)
                     (:s "x" :i 2 :n 1) (:s "x" :i 2 :n 1 :b t :extra 1)))
        (should (eq t (codex-ide-mcp--object-get
                       (funcall dispatch "tools/call"
                                (list :name "custom" :arguments bad)) "isError")))))
    (should (= calls 2))))

(ert-deftest codex-ide-mcp-custom-schema-snapshot-and-errors ()
  "Registration snapshots caller-owned schema and reports handler errors."
  (let ((codex-ide-mcp--custom-tools nil)
        (name (copy-sequence "custom"))
        (field (copy-sequence "value")))
    (let ((args (list (list :name field :type 'string))))
      (codex-ide-mcp-register-tool name "Example" args
                                   (lambda (value) (error "Handler refused %s" value)))
      (aset name 0 ?x)
      (aset field 0 ?x)
      (setf (plist-get (car args) :type) 'integer))
    (let ((names (codex-ide-mcp-tool-names)))
      (aset (car (last names)) 0 ?z))
    (let* ((tool (codex-ide-mcp--tool-by-name "custom"))
           (result (codex-ide-mcp--handle-tools-call
                    '(:name "custom" :arguments (:value "hello")))))
      (should (equal (plist-get (car (plist-get tool :args)) :name) "value"))
      (should (eq (codex-ide-mcp--object-get result "isError") t))
      (should (string-match-p "Handler refused hello"
                              (codex-ide-mcp-test--result-text result))))))

(defun codex-ide-mcp-test--wire-request (request)
  "Send REQUEST over a real loopback socket and return its JSON response."
  (let ((body (encode-coding-string (plist-get request :body) 'utf-8))
        (response "") client)
    (unwind-protect
        (progn
          (setq client
                (make-network-process
                 :name "codex-custom-wire" :host "127.0.0.1"
                 :service codex-ide-mcp--port :noquery t :coding 'binary
                 :filter (lambda (_proc text) (setq response (concat response text)))))
          (process-send-string
           client (concat "POST /mcp HTTP/1.1\r\n"
                          (mapconcat (lambda (header)
                                       (format "%s: %s\r\n" (car header) (cdr header)))
                                     (plist-get request :headers) "")
                          (format "Content-Length: %d\r\n\r\n" (length body)) body))
          (let ((deadline (+ (float-time) 3)))
            (while (and (process-live-p client) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (string-prefix-p "HTTP/1.1 200 " response))
          (should (string-match "\r\n\r\n" response))
          (codex-ide-mcp-test--json-read
           (decode-coding-string (substring response (match-end 0)) 'utf-8)))
      (when (process-live-p client) (delete-process client)))))

(ert-deftest codex-ide-mcp-custom-http-discovery-call-remove ()
  "Discover and call custom tools over both HTTP protocol modes."
  (let ((codex-ide-mcp--custom-tools nil)
        (codex-ide-mcp-port 0) (codex-ide-mcp--server nil)
        (codex-ide-mcp--port nil)
        (codex-ide-mcp--clients (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (codex-ide-mcp-register-tool
           "greet" "Greet" '((:name "name" :type string))
           (lambda (name) (concat "Hello, " name)))
          (codex-ide-mcp--start-server)
          (dolist (modern '(nil t))
            (dolist (method '("tools/list" "tools/call"))
              (let* ((params '(:name "greet" :arguments (:name "κόσμε")))
                     (request (if modern
                                  (codex-ide-mcp-test--modern-request method params)
                                (let ((legacy (codex-ide-mcp-test--request)))
                                  (setf (plist-get legacy :body)
                                        (json-encode (list :jsonrpc "2.0" :id 1
                                                           :method method :params params)))
                                  legacy)))
                     (result (cdr (assoc "result"
                                         (codex-ide-mcp-test--wire-request request)))))
                (if (equal method "tools/list")
                    (should (cl-find "greet" (cdr (assoc "tools" result))
                                     :key (lambda (tool) (cdr (assoc "name" tool)))
                                     :test #'equal))
                  (should (equal
                           (json-read-from-string
                            (cdr (assoc "text" (car (cdr (assoc "content" result))))))
                           "Hello, κόσμε"))))))
          (codex-ide-mcp-unregister-tool "greet")
          (should-not (member "greet" (codex-ide-mcp-tool-names)))
          (should-error (codex-ide-mcp--handle-tools-call '(:name "greet"))))
      (codex-ide-mcp--stop-server))))

(provide 'codex-ide-mcp-tests)

;;; codex-ide-mcp-tests.el ends here
