;;; hermes-mcp-tests.el --- mcp tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-mcp-rows-parse-read-only-server-response ()
  "MCP rows show backend name, type, enabled state, status, and tool count."
  (let ((rows (hermes-mcp--rows
               '((servers . (((name . "ctx") (transport . "stdio")
                              (enabled . t) (tool_count . 7))
                             ((name . "http-srv") (type . "http")
                              (enabled . nil) (tools . ("a" "b")))
                             ((name . "unknown") (status . "connecting"))))))))
    (should (equal (mapcar #'car rows) '("ctx" "http-srv" "unknown")))
    (should (equal (aref (cadr (car rows)) 1) "stdio"))
    (should (equal (aref (cadr (car rows)) 2) "on"))
    (should (equal (aref (cadr (car rows)) 3) "configured"))
    (should (equal (aref (cadr (car rows)) 4) "7"))
    (should (equal (aref (cadr (nth 1 rows)) 1) "http"))
    (should (equal (aref (cadr (nth 1 rows)) 2) "off"))
    (should (equal (aref (cadr (nth 1 rows)) 3) "disabled"))
    (should (equal (aref (cadr (nth 1 rows)) 4) "2"))
    (should (equal (aref (cadr (nth 2 rows)) 2) "?"))
    (should (equal (aref (cadr (nth 2 rows)) 3) "connecting"))))

(ert-deftest hermes-mcp-rows-face-every-column ()
  "MCP rows give every column its own face."
  (let* ((row (car (hermes-mcp--rows
                    '((servers . (((name . "ctx") (transport . "stdio")
                                    (enabled . t)
                                    (status . "connecting")
                                    (tool_count . 7))))))))
         (entry (cadr row)))
    (should (eq (get-text-property 0 'face (aref entry 0))
                'hermes-browser-name))
    (should (eq (get-text-property 0 'face (aref entry 1))
                'hermes-browser-type))
    (should (equal (get-text-property 0 'face (aref entry 2))
                   '(hermes-browser-success hermes-browser-enabled)))
    (should (equal (get-text-property 0 'face (aref entry 3))
                   '(hermes-browser-pending hermes-browser-status)))
    (should (eq (get-text-property 0 'face (aref entry 4))
                'hermes-browser-tool-count))))

(ert-deftest hermes-mcp-rows-redact-secret-shaped-display-values ()
  "MCP row display cells do not leak secret-shaped backend values."
  (let* ((secret "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (rows (hermes-mcp--rows
                `((servers . (((name . ,(concat "srv-" secret))
                               (transport . "stdio")
                               (enabled . t)
                               (status . ,(concat "failed token=" secret))
                               (tool_count . 1)))))))
         (entry (cadr (car rows)))
         (display (string-join (mapcar (lambda (cell) (format "%s" cell))
                                       (append entry nil))
                               " ")))
    (should-not (string-match-p (regexp-quote secret) display))
    (should (string-match-p "<redacted>" display))))

(ert-deftest hermes-mcp-revert-refreshes-without-display ()
  "Reverting the MCP list refreshes rows in place; the command displays."
  (let (displayed)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (let ((p (funcall make-promise 'fake-client)))
                   (if on-success (hermes--promise-then p on-success) p))))
              ((symbol-function 'hermes-mcp--api)
               (lambda (&rest _)
                 (hermes--promise-resolved
                  '((servers . (((name . "ctx") (transport . "stdio")
                                 (enabled . t) (tool_count . 0))))))))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (setq displayed t))))
      (unwind-protect
          (progn
            (hermes-mcp--revert)
            (should-not displayed)
            (with-current-buffer hermes-mcp-buffer-name
              (should (equal (mapcar #'car tabulated-list-entries) '("ctx"))))
            (hermes-list-mcp)
            (should displayed))
        (when (get-buffer hermes-mcp-buffer-name)
          (kill-buffer hermes-mcp-buffer-name))))))

(ert-deftest hermes-mcp-test-and-toggle-dispatch-rest-actions ()
  "Testing and toggling dispatch to MCP dashboard REST endpoints."
  (let (calls messages)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (let ((p (funcall make-promise 'fake-client)))
                   (if on-success (hermes--promise-then p on-success) p))))
              ((symbol-function 'hermes-mcp--api)
               (lambda (method path &optional body query &rest _args)
                 (push (list method path body query) calls)
                 (hermes--promise-resolved (cond
					    ((equal path "/servers")
					     '((servers . (((name . "ctx") (transport . "stdio")
							    (enabled . t) (tool_count . 0))))))
					    ((equal path "/servers/ctx/test")
					     '((ok . t) (tools . (((name . "read")
								   (description . "Read"))))))
					    ((equal path "/servers/ctx/enabled")
					     (should (equal body '((enabled . :false))))
					     '((ok . t) (name . "ctx") (enabled . nil)))
					    (t (error "unexpected MCP API call %S" path))))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-mcp)
            (with-current-buffer "*Hermes MCP Servers*"
              (should (derived-mode-p 'hermes-mcp-mode))
              (should (eq (keymap-lookup hermes-mcp-mode-map "RET")
                          #'hermes-mcp-test))
              (should (eq (keymap-lookup hermes-mcp-mode-map "e")
                          #'hermes-mcp-toggle))
              (should (equal (caar tabulated-list-entries) "ctx"))
              (goto-char (point-min))
              (hermes-mcp-test)
              (should (equal (hermes-mcp--test-tool-count "ctx") "1"))
              (should (equal (aref (cadr (car tabulated-list-entries)) 3)
                             "ok"))
              (should (equal (aref (cadr (car tabulated-list-entries)) 4)
                             "1"))
              (goto-char (point-min))
              (hermes-mcp-toggle))
            (should (member '("POST" "/servers/ctx/test" nil nil) calls))
            (should (member '("PUT" "/servers/ctx/enabled"
                              ((enabled . :false)) nil)
                            calls))
            (should (= (cl-count-if (lambda (call)
                                      (equal (cadr call) "/servers"))
                                    calls)
                       3))
            (should (cl-some (lambda (message)
                               (string-match-p "ctx has 1 MCP tool" message))
                             messages))
            (should (cl-some (lambda (message)
                               (string-match-p "disabled ctx" message))
                             messages)))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-test-failure-message-redacts-secret ()
  "MCP test failure messages redact secret-shaped backend errors."
  (let ((secret "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        messages)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (let ((p (funcall make-promise 'fake-client)))
                   (if on-success (hermes--promise-then p on-success) p))))
              ((symbol-function 'hermes-mcp--api)
               (lambda (_method path &optional _body _query &rest _args)
                 (hermes--promise-resolved (cond
					    ((equal path "/servers")
					     '((servers . (((name . "ctx") (transport . "stdio")
							    (enabled . t))))))
					    ((equal path "/servers/ctx/test")
					     `((ok . nil) (error . ,(concat "failed token=" secret))
					       (tools . nil)))
					    (t (error "unexpected MCP API call %S" path))))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-mcp)
            (with-current-buffer "*Hermes MCP Servers*"
              (goto-char (point-min))
              (hermes-mcp-test))
            (let ((joined (string-join messages "\n")))
              (should-not (string-match-p (regexp-quote secret) joined))
              (should (string-match-p "test failed" joined))
              (should (string-match-p "<redacted>" joined))))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-action-reports-unsupported-backend ()
  "MCP actions surface unsupported REST backends as a Hermes message."
  (let (called messages)
    (cl-letf (((symbol-function 'hermes-browser--run-on-client)
               (lambda (make-promise &optional on-success)
                 (hermes--promise-catch
                  (hermes--promise-then (funcall make-promise 'fake-client)
                                        on-success)
                  (lambda (m) (message "Hermes: %s" m)))))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (method path &rest _args)
                 (setq called (list method path))
                 (hermes--promise-rejected
                  "Hermes dashboard request failed at /api/mcp/servers/ctx/test (HTTP 404)")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-mcp--render
             '((servers . (((name . "ctx") (transport . "stdio")
                            (enabled . t))))))
            (with-current-buffer "*Hermes MCP Servers*"
              (goto-char (point-min))
              (hermes-mcp-test))
            (should (equal called '("POST" "/api/mcp/servers/ctx/test")))
            (should (cl-some (lambda (m)
                               (string-match-p "MCP REST API is unavailable" m))
                             messages)))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-api-uses-live-client-session-token ()
  "MCP REST requests use a live dashboard client's session token when present."
  (let ((client (make-hermes-dashboard-transport-client
                 :host "127.0.0.1" :port 32123 :token "session-token"))
        seen-url seen-method seen-headers seen-secrets result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--http-json-async)
               (cl-function
                (lambda (url &key method headers body secrets)
                  (ignore body)
                  (setq seen-url url
                        seen-method method
                        seen-headers headers
                        seen-secrets secrets)
                  (hermes--promise-resolved '(:body ((servers . nil))))))))
      (hermes--promise-then
       (hermes-mcp--api "GET" "/servers" nil '((profile . "work")) :client client)
       (lambda (body) (setq result body)))
      (should (equal result '((servers . nil))))
      (should (equal seen-method "GET"))
      (should (string-match-p (regexp-quote "/api/mcp/servers?profile=work")
                              seen-url))
      (should (equal (cdr (assoc "X-Hermes-Session-Token" seen-headers))
                     "session-token"))
      (should (member "session-token" seen-secrets)))))

(ert-deftest hermes-mcp-api-redacts-secret-shaped-errors ()
  "MCP API errors do not leak token, ticket, internal, or env secrets."
  (let ((secret "sk-test-secret") reason)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (_method _path &rest args)
                 (hermes--promise-rejected
                  (hermes-dashboard-transport--redact-secret
                   (format "bad token=%s env SECRET=%s" secret secret)
                   (plist-get args :secrets))))))
      (hermes--promise-catch
       (hermes-mcp--api "GET" "/servers" nil nil :secrets (list secret))
       (lambda (r) (setq reason r)))
      (should reason)
      (should-not (string-match-p (regexp-quote secret) reason))
      (should (string-match-p "<redacted>" reason)))))

(ert-deftest hermes-mcp-api-reports-unsupported-backend ()
  "A missing MCP REST endpoint is reported as an unsupported backend."
  (let (reason)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-rejected
                  "Hermes dashboard request failed at /api/mcp/servers (HTTP 404)"))))
      (hermes--promise-catch
       (hermes-mcp--api "GET" "/servers")
       (lambda (r) (setq reason r)))
      (should (string-match-p "MCP REST API is unavailable" reason)))))

(provide 'hermes-mcp-tests)
;;; hermes-mcp-tests.el ends here
