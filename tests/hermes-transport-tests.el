;;; hermes-transport-tests.el --- transport tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-transport-dashboard-shows-review-summary ()
  "`review.summary' becomes a status event carrying its text, not an Unknown event."
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "review.summary")
                            (session_id . "sid")
                            (payload . ((text . "Self-improvement review: profile updated")))))))))
    (let ((event (car events)))
      (should event)
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :content)
                     "Self-improvement review: profile updated")))))

(ert-deftest hermes-dashboard-rpc-session-requests-identify-emacs-source ()
  "Session create and resume requests identify the Emacs client surface."
  (let (requests)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method params &rest _)
                 (push (cons method params) requests))))
      (hermes-dashboard-transport-session-create 'client)
      (hermes-dashboard-transport-session-resume 'client "stored"))
    (should
     (equal (nreverse requests)
            '(("session.create" (source . "emacs"))
              ("session.resume" (session_id . "stored")
               (source . "emacs")))))))

(ert-deftest hermes-dashboard-rpc-config-set-sends-reasoning-scope ()
  "Config writes preserve the Dashboard session and optional global scope."
  (let (request)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method params &rest _)
                 (setq request (cons method params)))))
      (hermes-dashboard-transport-config-set
       'client "reasoning" "ultra"
       :session-id "sid" :scope "global"))
    (should
     (equal request
            '("config.set" (key . "reasoning") (value . "ultra")
              (session_id . "sid") (scope . "global"))))))

(ert-deftest hermes-transport-dashboard-approval-respond-payload ()
  (let ((client (hermes-test--dashboard-client))
        (hermes-dashboard-transport-request-timeout nil)
        sent-frame)
    (let ((hermes-dashboard-transport-websocket-send-function
           (lambda (_websocket text)
             (setq sent-frame (hermes-dashboard-transport--decode-frame text)))))
      (hermes-dashboard-transport-approval-respond
       client :session-id "sid-approval" :choice "session" :all t))
    (should (equal (alist-get 'method sent-frame) "approval.respond"))
    (should (equal (alist-get 'session_id (alist-get 'params sent-frame))
                   "sid-approval"))
    (should (equal (alist-get 'choice (alist-get 'params sent-frame))
                   "session"))
    (should (eq (alist-get 'all (alist-get 'params sent-frame)) t))))

(ert-deftest hermes-transport-dashboard-terminal-read-respond-payload ()
  (let ((client (hermes-test--dashboard-client))
        (hermes-dashboard-transport-request-timeout nil)
        sent-frame)
    (let ((hermes-dashboard-transport-websocket-send-function
           (lambda (_websocket text)
             (setq sent-frame (hermes-dashboard-transport--decode-frame text)))))
      (hermes-dashboard-transport-terminal-read-respond
       client "req-term" "terminal output line"))
    (should (equal (alist-get 'method sent-frame) "terminal.read.respond"))
    (should (equal (alist-get 'request_id (alist-get 'params sent-frame))
                   "req-term"))
    (should (equal (alist-get 'text (alist-get 'params sent-frame))
                   "terminal output line"))))

(ert-deftest hermes-transport-field-nil-on-absent ()
  (should (equal (hermes-transport--field '((name . "x")) 'name) "x"))
  (should (null (hermes-transport--field '((name . "x")) 'missing)))
  (should (equal (hermes-transport--field '(:name "x") 'name) "x"))
  (let ((h (make-hash-table :test #'equal)))
    (puthash "name" "x" h)
    (should (equal (hermes-transport--field h 'name) "x"))
    (should (null (hermes-transport--field h 'missing)))))

(ert-deftest hermes-transport-display-field-empty-on-absent ()
  (should (equal (hermes-transport--display-field '((name . "x")) 'name) "x"))
  (should (equal (hermes-transport--display-field '((name . "x")) 'missing) ""))
  (should (equal (hermes-transport--display-field nil 'name) "")))

(ert-deftest hermes-transport-non-empty-string ()
  (should (equal (hermes-transport--non-empty-string "x") "x"))
  (should (null (hermes-transport--non-empty-string "")))
  (should (equal (hermes-transport--non-empty-string "  ") "  "))
  (should (null (hermes-transport--non-empty-string nil))))

(ert-deftest hermes-transport-non-blank-string-trims ()
  (should (equal (hermes-transport--non-blank-string "  x  ") "x"))
  (should (null (hermes-transport--non-blank-string "   ")))
  (should (null (hermes-transport--non-blank-string nil))))

(ert-deftest hermes-transport-field-present-detects-nil-value ()
  (should (hermes-transport--field-present-p '((enabled . nil)) 'enabled))
  (should-not (hermes-transport--field-present-p '((enabled . nil)) 'other))
  (should (hermes-transport--field-present-p '(:enabled nil) 'enabled))
  (let ((h (make-hash-table :test #'equal)))
    (puthash "enabled" nil h)
    (should (hermes-transport--field-present-p h 'enabled))
    (should-not (hermes-transport--field-present-p h 'other))))

(ert-deftest hermes-transport-get-matches-key-candidates ()
  (should (equal (hermes-transport--get '(("session_id" . "s")) 'session_id) "s"))
  (should (equal (hermes-transport--get-any '((type . "t")) '(event type)) "t")))

(ert-deftest hermes-transport-send-emits-start-status ()
  (let (events)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _plist) 'fake-process)))
      (should (eq (hermes-transport-send
                   "hello"
                   (lambda (event) (push event events)))
                  'fake-process)))
    (should (equal (nreverse events)
                   '((:type status
			    :event "run.started"
			    :status "running"
			    :content "Starting Hermes"))))))

(ert-deftest hermes-transport-send-cleans-buffer-when-process-start-fails ()
  "A failed CLI process start does not leave its hidden output buffer alive."
  (let (buffer)
    (cl-letf (((symbol-function 'generate-new-buffer)
               (lambda (_name)
                 (setq buffer (get-buffer-create " *hermes-failed-start*"))))
              ((symbol-function 'make-process)
               (lambda (&rest _plist) (error "cannot start"))))
      (should-error (hermes-transport-send "hello" #'ignore))
      (should-not (buffer-live-p buffer)))))

(ert-deftest hermes-transport-builds-quiet-chat-command ()
  (cl-letf (((symbol-function 'executable-find) #'ignore)
            ((symbol-function 'file-executable-p) #'ignore))
    (let ((hermes-command "hermes"))
      (should (equal (hermes-transport--command "hello")
                     '("hermes" "chat" "-Q" "-q" "hello"))))))

(ert-deftest hermes-transport-dashboard-builds-command ()
  (let* ((hermes-dashboard-transport-command "hermes")
         (env (hermes-dashboard-transport--environment
               "secret-token" '("PATH=/bin")))
         (start-event (hermes-dashboard-transport--start-event
                       "127.0.0.1" 4567 "secret-token")))
    (cl-letf (((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'file-executable-p) #'ignore))
      (should (equal (hermes-dashboard-transport--command "127.0.0.1" 4567)
                     '("hermes" "dashboard" "--no-open" "--tui" "--isolated"
                       "--host" "127.0.0.1" "--port" "4567"))))
    (should (member "PATH=/bin" env))
    (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token" env))
    (should (member "HERMES_DASHBOARD_TUI=1" env))
    (should-not (string-match-p "secret-token" (format "%S" start-event)))
    (should (equal (plist-get start-event :content)
                   "Starting Hermes dashboard on 127.0.0.1:4567"))))

(ert-deftest hermes-transport-dashboard-builds-websocket-url ()
  (should (equal (hermes-dashboard-transport--websocket-url
                  "127.0.0.1" 4567 "secret-token")
                 "ws://127.0.0.1:4567/api/ws?token=secret-token"))
  (should (equal (hermes-dashboard-transport--redacted-websocket-url
                  "127.0.0.1" 4567)
                 "ws://127.0.0.1:4567/api/ws?token=<redacted>")))

(ert-deftest hermes-transport-dashboard-builds-prefixed-remote-urls ()
  (should (equal (hermes-dashboard-transport--api-url
                  "https://dash.example/hermes/" "/api/status")
                 "https://dash.example/hermes/api/status"))
  (should (equal (hermes-dashboard-transport--websocket-url
                  "ignored" nil "ticket-secret"
                  "https://dash.example/hermes/" "ticket")
                 "wss://dash.example/hermes/api/ws?ticket=ticket-secret"))
  (should (equal (hermes-dashboard-transport--redacted-websocket-url
                  "ignored" nil "https://dash.example/hermes/" "ticket")
                 "wss://dash.example/hermes/api/ws?ticket=<redacted>")))

(ert-deftest hermes-transport-dashboard-rejects-remote-url-credentials ()
  (dolist (url '("https://user:password@dash.example/hermes"
                 "https://dash.example/hermes?token=secret-token"
                 "https://dash.example/hermes#secret-fragment"))
    (let ((message (condition-case error
                       (progn
                         (hermes-dashboard-transport--base-url
                          "ignored" nil url)
                         nil)
                     (user-error (error-message-string error)))))
      (should message)
      (should-not (string-match-p "secret-token" message))
      (should-not (string-match-p "secret-fragment" message)))))

(ert-deftest hermes-transport-dashboard-normalize-base-url-empty-is-nil ()
  "Empty or whitespace-only URLs normalize to nil, not \"\"."
  (should-not (hermes-dashboard-transport--normalize-base-url nil))
  (should-not (hermes-dashboard-transport--normalize-base-url ""))
  (should-not (hermes-dashboard-transport--normalize-base-url "   "))
  (should (equal (hermes-dashboard-transport--normalize-base-url
                  "https://dash.example/hermes/")
                 "https://dash.example/hermes")))

(ert-deftest hermes-transport-dashboard-parses-set-cookie-headers ()
  (let ((buffer (generate-new-buffer " *hermes-test-http*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "HTTP/1.1 200 OK\r\n"
                  "Set-Cookie: access=access-cookie; Path=/; HttpOnly\r\n"
                  "Set-Cookie: refresh=refresh-cookie; Path=/; HttpOnly\r\n"
                  "Content-Type: application/json\r\n\r\n"
                  "{\"ok\": true}")
          (let ((response (hermes-dashboard-transport--parse-http-response-buffer
                           buffer)))
            (should (= (plist-get response :status) 200))
            (should (equal (hermes-dashboard-transport--response-cookie-header
                            response)
                           "access=access-cookie; refresh=refresh-cookie"))
            (should (string-match-p "\"ok\""
                                    (plist-get response :body-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-transport-dashboard-http-error-skips-json-body-parse ()
  (let (buffer message)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq buffer (generate-new-buffer " *hermes-test-http*"))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 401 Unauthorized\r\n\r\nnot json secret-token"))
                 buffer)))
      (setq message
            (condition-case error
                (progn
                  (hermes-dashboard-transport--default-http-request
                   "http://dash.example/api/status?token=secret-token"
                   :secrets '("secret-token"))
                  nil)
              (user-error (error-message-string error))))
      (should (string-match-p "HTTP 401" message))
      (should (string-match-p "token=<redacted>" message))
      (should-not (string-match-p "secret-token" message))
      (should-not (buffer-live-p buffer)))))

(ert-deftest hermes-transport-dashboard-http-error-includes-json-detail ()
  "REST errors include backend JSON detail and still redact secrets."
  (let (buffer message)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq buffer (generate-new-buffer " *hermes-test-http*"))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 400 Bad Request\r\n"
                           "Content-Type: application/json\r\n\r\n"
                           "{\"detail\": \"the 'default' board cannot be removed secret-token\"}"))
                 buffer)))
      (setq message
            (condition-case error
                (progn
                  (hermes-dashboard-transport--default-http-request
                   "http://dash.example/api/plugins/kanban/boards/default?token=secret-token"
                   :secrets '("secret-token"))
                  nil)
              (user-error (error-message-string error))))
      (should (string-match-p "HTTP 400" message))
      (should (string-match-p "default.*cannot be removed" message))
      (should (string-match-p "token=<redacted>" message))
      (should-not (string-match-p "secret-token" message))
      (should-not (buffer-live-p buffer)))))

(ert-deftest hermes-transport-dashboard-http-result-ok-on-2xx ()
  (let ((response (list :status 200 :headers nil
                        :body-text "{\"ok\": true}")))
    (pcase (hermes-dashboard-transport--http-result response "http://x" nil)
      (`(ok . ,r) (should (equal (hermes-transport--get (plist-get r :body) 'ok) t)))
      (other (ert-fail (format "expected ok, got %S" other))))))

(ert-deftest hermes-transport-dashboard-http-result-error-redacts ()
  (let ((response (list :status 404 :headers nil
                        :body-text "{\"detail\": \"no board secret-token\"}")))
    (pcase (hermes-dashboard-transport--http-result
            response "http://x?token=<redacted>" '("secret-token"))
      (`(error . ,message)
       (should (string-match-p "HTTP 404" message))
       (should (string-match-p "no board <redacted>" message))
       (should-not (string-match-p "secret-token" message)))
      (other (ert-fail (format "expected error, got %S" other))))))

(ert-deftest hermes-transport-dashboard-http-result-error-on-non-json-2xx ()
  "A 2xx status with a non-JSON body yields an error result, not a signal."
  (let ((response (list :status 200 :headers nil
                        :body-text "<html>proxy login secret-token</html>")))
    (pcase (hermes-dashboard-transport--http-result
            response "http://x?token=<redacted>" '("secret-token"))
      (`(error . ,message)
       (should (string-match-p "non-JSON body" message))
       (should (string-match-p "HTTP 200" message))
       (should-not (string-match-p "secret-token" message)))
      (other (ert-fail (format "expected error, got %S" other))))))

(ert-deftest hermes-transport-dashboard-basic-auth-request-descriptor ()
  "The pure builder yields the password-login request plist."
  (let ((request (hermes-dashboard-transport--basic-auth-request
                  "http://dash.example" "basic" "admin" "hunter2")))
    (should (equal (plist-get request :url)
                   "http://dash.example/auth/password-login"))
    (should (equal (plist-get request :method) "POST"))
    (should (equal (plist-get request :headers)
                   '(("Content-Type" . "application/json"))))
    (should (equal (plist-get request :body)
                   '((provider . "basic")
                     (username . "admin")
                     (password . "hunter2")
                     (next . ""))))
    (should (equal (plist-get request :secrets) '("hunter2")))))

(ert-deftest hermes-transport-dashboard-http-json-async-returns-promise ()
  (let* ((captured nil)
         (hermes-dashboard-transport-http-request-async-function
          (lambda (url &rest args)
            (setq captured (cons url args))
            (hermes--promise-resolved (list :status 200 :body '((ok . t))))))
         (result nil))
    (hermes--promise-then
     (hermes-dashboard-transport--http-json-async
      "http://x" :method "POST" :body '((a . 1)))
     (lambda (response) (setq result response)))
    (should (equal (plist-get result :body) '((ok . t))))
    (should (equal (plist-get (cdr captured) :method) "POST"))
    (should (assoc "Accept" (plist-get (cdr captured) :headers)))))

(ert-deftest hermes-transport-dashboard-api-request-async-token-auth ()
  (let* ((hermes-dashboard-transport--api-auth nil)
         (hermes-dashboard-transport-remote-auth-method 'token)
         (calls nil)
         (hermes-dashboard-transport-http-request-async-function
          (lambda (url &rest args)
            (push (cons url args) calls)
            (hermes--promise-resolved (list :status 200 :body '((ok . t))))))
         result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-token-secret)
               (lambda (&rest _) "tok"))
              ((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes--promise-then
       (hermes-dashboard-transport-api-request-async "GET" "/api/profiles")
       (lambda (body) (setq result body))))
    (should (equal result '((ok . t))))
    (should-not (cl-find-if (lambda (c) (string-match-p "/api/status" (car c)))
                            calls))
    (let ((headers (plist-get (cdr (car calls)) :headers)))
      (should (equal (cdr (assoc "X-Hermes-Session-Token" headers)) "tok")))))

(ert-deftest hermes-transport-dashboard-api-request-async-with-client ()
  (let* ((client (make-hermes-dashboard-transport-client
                  :host "127.0.0.1" :port 9119 :token "ctok"
                  :base-url "http://127.0.0.1:9119"))
         (calls nil)
         (hermes-dashboard-transport-http-request-async-function
          (lambda (url &rest args)
            (push (cons url args) calls)
            (hermes--promise-resolved (list :status 200 :body '((ok . t))))))
         result)
    (hermes--promise-then
     (hermes-dashboard-transport-api-request-async
      "GET" "/api/profiles" :client client)
     (lambda (body) (setq result body)))
    (should (equal result '((ok . t))))
    (let ((headers (plist-get (cdr (car calls)) :headers)))
      (should (equal (cdr (assoc "X-Hermes-Session-Token" headers)) "ctok")))))

(ert-deftest hermes-transport-dashboard-api-request-async-retries-get-once ()
  (let* ((hermes-dashboard-transport--api-auth nil)
         (hermes-dashboard-transport-remote-auth-method 'token)
         (n 0)
         (auth-count 0)
         (hermes-dashboard-transport-http-request-async-function
          (lambda (&rest _)
            (setq n (1+ n))
            (if (= n 1)
                (hermes--promise-rejected
                 "Hermes dashboard request failed at x (HTTP 401)")
              (hermes--promise-resolved (list :status 200 :body '((ok . t)))))))
         result reason)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-token-secret)
               (lambda (&rest _) (setq auth-count (1+ auth-count)) "tok"))
              ((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes--promise-then
       (hermes-dashboard-transport-api-request-async "GET" "/x")
       (lambda (b) (setq result b))
       (lambda (r) (setq reason r))))
    (should (equal result '((ok . t))))
    (should (null reason))
    (should (= n 2))
    (should (>= auth-count 2))))

(ert-deftest hermes-transport-dashboard-api-request-async-no-retry-on-404 ()
  "A non-auth failure is not retried through re-authentication."
  (let* ((hermes-dashboard-transport--api-auth nil)
         (hermes-dashboard-transport-remote-auth-method 'token)
         (n 0)
         (hermes-dashboard-transport-http-request-async-function
          (lambda (&rest _)
            (setq n (1+ n))
            (hermes--promise-rejected
             "Hermes dashboard request failed at x (HTTP 404)")))
         reason)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-token-secret)
               (lambda (&rest _) "tok"))
              ((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes--promise-catch
       (hermes-dashboard-transport-api-request-async "GET" "/x")
       (lambda (r) (setq reason r))))
    (should (= n 1))
    (should (string-match-p "HTTP 404" reason))))

(ert-deftest hermes-transport-dashboard-api-auth-invalidates-on-url-change ()
  "Cached REST auth is dropped when the configured dashboard URL changes."
  (let ((hermes-dashboard-transport--api-auth
         '(:base-url "http://old.example" :headers nil :secrets nil)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://new.example")))
      (should (hermes-dashboard-transport--api-auth-stale-p)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://old.example")))
      (should-not (hermes-dashboard-transport--api-auth-stale-p)))))

(ert-deftest hermes-transport-dashboard-cached-profile-list-serves-current-url ()
  "A stored profile payload is served only while the dashboard URL matches."
  (let ((hermes-dashboard-transport--profile-cache nil))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default"))))))
      (should (equal (hermes-dashboard-transport-cached-profile-list)
                     '((profiles . (((name . "default"))))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://other.example")))
      (should-not (hermes-dashboard-transport-cached-profile-list)))))

(ert-deftest hermes-transport-dashboard-profile-list-async-warms-cache ()
  "Resolving the async profile list stores the payload in the cache."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (payload '((profiles . (((name . "default")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-resolved payload))))
      (let (resolved)
        (hermes--promise-then
         (hermes-dashboard-transport-profile-list-async)
         (lambda (value) (setq resolved value)))
        (should (equal resolved payload))
        (should (equal (hermes-dashboard-transport-cached-profile-list)
                       payload))))))

(ert-deftest hermes-transport-profile-cache-uses-explicit-client-endpoint ()
  "An explicit client's profiles are cached under that client's endpoint."
  (let* ((hermes-dashboard-transport--profile-cache nil)
         (hermes-dashboard-transport-url "http://configured.example:9119")
         (client (make-hermes-dashboard-transport-client
                  :base-url "http://client.example:8123" :token "token"))
         (payload '((profiles . (((name . "client-profile")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-resolved payload))))
      (hermes-dashboard-transport-profile-list-async client)
      (should (equal (hermes-dashboard-transport-cached-profile-list client)
                     payload))
      (should-not (hermes-dashboard-transport-cached-profile-list)))))

(ert-deftest hermes-transport-profile-cache-keeps-request-endpoint-after-url-change ()
  "A late profile response stays associated with its captured request endpoint."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (hermes-dashboard-transport-url "http://a.example:9119")
        (request (hermes--promise-make))
        (payload '((profiles . (((name . "from-a")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) request)))
      (hermes-dashboard-transport-profile-list-async)
      (setq hermes-dashboard-transport-url "http://b.example:9119")
      (hermes--promise-resolve request payload)
      (should-not (hermes-dashboard-transport-cached-profile-list))
      (setq hermes-dashboard-transport-url "http://a.example:9119")
      (should (equal (hermes-dashboard-transport-cached-profile-list)
                     payload)))))

(ert-deftest hermes-transport-profile-cache-is-independent-per-client ()
  "Two clients retain independent profile lists under their own endpoints."
  (let* ((hermes-dashboard-transport--profile-cache nil)
         (client-a (make-hermes-dashboard-transport-client
                    :base-url "http://a.example:9119" :token "a"))
         (client-b (make-hermes-dashboard-transport-client
                    :base-url "http://b.example:9119" :token "b"))
         (payload-a '((profiles . (((name . "a"))))))
         (payload-b '((profiles . (((name . "b")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest args)
                 (hermes--promise-resolved
                  (if (eq (plist-get args :client) client-a)
                      payload-a payload-b)))))
      (hermes-dashboard-transport-profile-list-async client-a)
      (hermes-dashboard-transport-profile-list-async client-b)
      (should (equal (hermes-dashboard-transport-cached-profile-list client-a)
                     payload-a))
      (should (equal (hermes-dashboard-transport-cached-profile-list client-b)
                     payload-b)))))

(ert-deftest hermes-transport-cached-model-options-serves-current-url ()
  "A stored model-options payload is served only while the dashboard URL matches."
  (let ((hermes-dashboard-transport--model-options-cache nil)
        (payload '((providers . (((slug . "p1")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes-dashboard-transport--store-model-options payload)
      (should (equal (hermes-dashboard-transport-cached-model-options) payload)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://other.example")))
      (should-not (hermes-dashboard-transport-cached-model-options)))))

(ert-deftest hermes-transport-invalidate-model-options-clears-cache ()
  "Invalidation discards a stored model-options payload."
  (let ((hermes-dashboard-transport--model-options-cache nil))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example")))
      (hermes-dashboard-transport--store-model-options '((providers . nil)))
      (should (hermes-dashboard-transport-cached-model-options))
      (hermes-dashboard-transport-invalidate-model-options)
      (should-not (hermes-dashboard-transport-cached-model-options)))))

(ert-deftest hermes-transport-model-options-cached-serves-cache-without-fetch ()
  "A cache hit resolves from the cache and never calls the RPC."
  (let ((hermes-dashboard-transport--model-options-cache nil)
        (payload '((providers . (((slug . "cached"))))))
        fetched resolved)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (&rest _) (setq fetched t))))
      (hermes-dashboard-transport--store-model-options payload)
      (hermes-dashboard-transport-model-options-cached
       'client :resolve (lambda (result) (setq resolved result)))
      (should (equal resolved payload))
      (should-not fetched))))

(ert-deftest hermes-transport-model-options-cached-force-refetches ()
  "FORCE bypasses a populated cache, refetches, and stores the fresh payload."
  (let ((hermes-dashboard-transport--model-options-cache nil)
        (fresh '((providers . (((slug . "fresh"))))))
        fetched)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (setq fetched t)
                 (funcall (plist-get args :resolve) fresh))))
      (hermes-dashboard-transport--store-model-options '((providers . (((slug . "stale"))))))
      (hermes-dashboard-transport-model-options-cached 'client :force t)
      (should fetched)
      (should (equal (hermes-dashboard-transport-cached-model-options) fresh)))))

(ert-deftest hermes-transport-model-options-cached-stores-on-fetch ()
  "A cache miss fetches over the RPC and stores the resolved payload."
  (let ((hermes-dashboard-transport--model-options-cache nil)
        (payload '((providers . (((slug . "p1"))))))
        resolved)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve) payload))))
      (hermes-dashboard-transport-model-options-cached
       'client :resolve (lambda (result) (setq resolved result)))
      (should (equal resolved payload))
      (should (equal (hermes-dashboard-transport-cached-model-options) payload)))))

(ert-deftest hermes-transport-model-cache-is-independent-per-client ()
  "Two clients retain independent model catalogs under their own endpoints."
  (let* ((hermes-dashboard-transport--model-options-cache nil)
         (client-a (make-hermes-dashboard-transport-client
                    :base-url "http://a.example:9119" :token "a"))
         (client-b (make-hermes-dashboard-transport-client
                    :base-url "http://b.example:9119" :token "b"))
         (payload-a '((providers . (((slug . "a"))))))
         (payload-b '((providers . (((slug . "b")))))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (client &rest args)
                 (funcall (plist-get args :resolve)
                          (if (eq client client-a) payload-a payload-b)))))
      (hermes-dashboard-transport-model-options-cached client-a)
      (hermes-dashboard-transport-model-options-cached client-b)
      (should (equal (hermes-dashboard-transport-cached-model-options client-a)
                     payload-a))
      (should (equal (hermes-dashboard-transport-cached-model-options client-b)
                     payload-b)))))

(ert-deftest hermes-transport-dashboard-start-auto-localhost-spawns ()
  (let (process-plist opened-url events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-url "http://127.0.0.1:9119")
            (hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (setq process-plist plist)
               'fake-process))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (setq opened-url url)
               'fake-websocket)))
        (let ((client (hermes-dashboard-transport-start
                       :callback (lambda (event) (push event events)))))
          (should (eq (hermes-dashboard-transport-client-process client)
                      'fake-process))
          (should (equal (plist-get process-plist :name) "hermes-dashboard"))
          (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token"
                          (plist-get process-plist :env)))
          (should (equal opened-url
                         "ws://127.0.0.1:9119/api/ws?token=secret-token"))
          (should (string-match-p "Starting Hermes dashboard"
                                  (format "%S" events)))
          (should-not (string-match-p "secret-token" (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-auto-remote-does-not-spawn ()
  (let (opened-url events)
    (let ((hermes-dashboard-transport-start-mode 'auto)
          (hermes-dashboard-transport-ready-timeout nil)
          (hermes-dashboard-transport-make-process-function
           (lambda (&rest _plist) (error "remote attach must not spawn")))
          (hermes-dashboard-transport-websocket-open-function
           (lambda (url _client)
             (setq opened-url url)
             'fake-websocket)))
      (let ((client (hermes-dashboard-transport-start
                     :host "100.64.0.10"
                     :port 9119
                     :token "remote-token"
                     :remote-auth-method 'token
                     :callback (lambda (event) (push event events)))))
        (should-not (hermes-dashboard-transport-client-process client))
        (should (equal opened-url
                       "ws://100.64.0.10:9119/api/ws?token=remote-token"))
        (should-not (string-match-p "remote-token" (format "%S" events)))
        (should (string-match-p "token=<redacted>" (format "%S" events)))))))

(ert-deftest hermes-transport-dashboard-token-auth-source-and-env-fallback ()
  (let (searches)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (push args searches)
                 (when (equal (plist-get args :host)
                              "http://100.64.0.10:9119")
                   (list (list :secret (lambda () "auth-token")))))))
      (should (equal (hermes-dashboard-transport--remote-token-secret
                      "http://100.64.0.10:9119")
                     "auth-token"))
      (should (plist-get (car searches) :user))
      (should (equal (plist-get (car searches) :port)
                     "hermes-dashboard-token"))))
  (let ((process-environment
         '("HERMES_DASHBOARD_SESSION_TOKEN=env-token")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should (equal (hermes-dashboard-transport--remote-token-secret
                      "http://100.64.0.10:9119")
                     "env-token"))))
  (let ((process-environment
         '("HERMES_DASHBOARD_SESSION_TOKEN=")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should-error (hermes-dashboard-transport--remote-token-secret
                     "http://100.64.0.10:9119" "")
                    :type 'user-error))))

(ert-deftest hermes-transport-dashboard-normalized-error-redacts-remote-secrets ()
  (let* ((client (make-hermes-dashboard-transport-client
                  :secrets '("cookie-secret" "ticket-secret")))
         (message (hermes-dashboard-transport--normalized-error-message
                   client "failed with cookie-secret and ticket-secret")))
    (should (string-match-p "<redacted>" message))
    (should-not (string-match-p "cookie-secret" message))
    (should-not (string-match-p "ticket-secret" message))))

(ert-deftest hermes-transport-dashboard-missing-token-error-actionable ()
  (let ((process-environment nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (let ((message (condition-case error
                         (progn
                           (hermes-dashboard-transport--remote-token-secret
                            "http://100.64.0.10:9119")
                           nil)
                       (user-error (error-message-string error)))))
        (should (string-match-p "hermes-dashboard-token" message))
        (should (string-match-p "HERMES_DASHBOARD_SESSION_TOKEN" message))))))

(ert-deftest hermes-transport-dashboard-basic-auth-uses-ticket-and-redacts ()
  (let ((password "basic-password-secret")
        (cookie-a "access=access-cookie-secret")
        (cookie-b "refresh=refresh-cookie-secret")
        (ticket "ticket-secret-abc")
        requests opened-url events)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (when (equal (plist-get args :port)
                              "hermes-dashboard-basic")
                   (list (list :user "admin"
                               :secret (lambda () password)))))))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (setq opened-url url)
               'fake-websocket))
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) (error "remote attach must not spawn")))
            (hermes-dashboard-transport-http-request-async-function
             (lambda (url &rest args)
               (push (list :url url
                           :method (plist-get args :method)
                           :headers (plist-get args :headers)
                           :data (plist-get args :data))
                     requests)
               (hermes--promise-resolved
                (cond
                 ((string-suffix-p "/api/status" url)
                  '(:status 200 :headers nil
                    :body ((auth_required . t)
                           (auth_providers . ("basic")))))
                 ((string-suffix-p "/auth/password-login" url)
                  `(:status 200
                    :headers (("set-cookie" . ,(concat cookie-a "; Path=/"))
                              ("set-cookie" . ,(concat cookie-b "; Path=/")))
                    :body ((ok . t))))
                 ((string-suffix-p "/api/auth/ws-ticket" url)
                  `(:status 200 :headers nil
                    :body ((ticket . ,ticket) (ttl_seconds . 30)))))))))
        (hermes-dashboard-transport-start
         :host "100.64.0.10"
         :port 9119
         :callback (lambda (event) (push event events)))
        (setq requests (nreverse requests))
        (should (equal opened-url
                       "ws://100.64.0.10:9119/api/ws?ticket=ticket-secret-abc"))
        (let* ((login (nth 1 requests))
               (ticket-request (nth 2 requests))
               (login-body (json-parse-string (plist-get login :data)
                                              :object-type 'alist)))
          (should (equal (hermes-transport--get login-body 'username) "admin"))
          (should (equal (hermes-transport--get login-body 'password) password))
          (should-not (string-match-p password (format "%S" ticket-request)))
          (should (equal (alist-get "Cookie" (plist-get ticket-request :headers)
                                    nil nil #'equal)
                         (concat cookie-a "; " cookie-b))))
        (let ((visible (format "%S" events)))
          (dolist (secret (list password cookie-a cookie-b ticket))
            (should-not (string-match-p (regexp-quote secret) visible)))
          (should (string-match-p "ticket=<redacted>" visible)))))))

(ert-deftest hermes-transport-dashboard-oauth-only-remote-is-unsupported ()
  "Gated OAuth without native_pkce still rejects when no basic provider exists."
  (let (requests auth-source-called reason)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) (setq auth-source-called t) nil)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-http-request-async-function
             (lambda (url &rest _args)
               (push url requests)
               (hermes--promise-resolved
                '(:status 200 :headers nil
                  :body ((auth_required . t)
                         (auth_providers . ("oauth"))
                         (auth_flows . ("cookie")))))))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (&rest _args) (error "must not open websocket"))))
        (let ((client (hermes-dashboard-transport-start
                       :host "100.64.0.10" :port 9119 :callback #'ignore)))
          ;; The fake request resolves synchronously, so the auth chain has
          ;; already rejected the ready promise by the time we subscribe.
          (hermes--promise-catch
           (hermes-dashboard-transport-client-ready-promise client)
           (lambda (r) (setq reason r))))
        (should (string-match-p "OAuth-only remote attach" reason))
        (should-not (string-match-p "token=" reason))
        (should-not auth-source-called)
        (should (equal (nreverse requests)
                       '("http://100.64.0.10:9119/api/status")))))))

(ert-deftest hermes-transport-dashboard-native-pkce-happy-path ()
  "Native PKCE attach stores tokens, mints a ticket, and opens the WS."
  (let* ((base "http://100.64.0.10:9119")
         (access "native-access-token")
         (refresh "native-refresh-token")
         (ticket "native-ticket-secret")
         (store (make-hash-table :test #'equal))
         requests browsed opened-url events
         loopback-filter loopback-process)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--native-token-load)
               (lambda (url) (gethash url store)))
              ((symbol-function 'hermes-dashboard-transport--native-token-store)
               (lambda (url tokens)
                 (if tokens
                     (puthash url tokens store)
                   (remhash url store))))
              ((symbol-function 'hermes-dashboard-transport--random-bytes)
               (lambda (n) (apply #'unibyte-string (make-list n ?A))))
              ((symbol-function 'hermes-dashboard-transport--browse-url)
               (lambda (url) (setq browsed url)))
              ((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq loopback-filter (plist-get plist :filter))
                 (setq loopback-process
                       (list 'fake-server
                             :service 54321
                             :filter loopback-filter))
                 loopback-process))
              ((symbol-function 'process-contact)
               (lambda (proc &optional key &rest _)
                 (pcase key
                   (:service 54321)
                   (:local '(127 0 0 1 54321))
                   (_ proc))))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-live-p)
               (lambda (_proc) t)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (setq opened-url url)
               'fake-websocket))
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) (error "remote attach must not spawn")))
            (hermes-dashboard-transport-http-request-async-function
             (lambda (url &rest args)
               (push (list :url url
                           :method (plist-get args :method)
                           :headers (plist-get args :headers)
                           :data (plist-get args :data))
                     requests)
               (cond
                ((string-suffix-p "/api/status" url)
                 (hermes--promise-resolved
                  '(:status 200 :headers nil
                    :body ((auth_required . t)
                           (auth_providers . ("oauth"))
                           (auth_flows . ("cookie" "native_pkce"))))))
                ((string-suffix-p "/auth/native/token" url)
                 (hermes--promise-resolved
                  `(:status 200 :headers nil
                    :body ((access_token . ,access)
                           (refresh_token . ,refresh)
                           (expires_at . 4102444800)
                           (provider . "oauth")
                           (user_id . "user-1")))))
                ((string-suffix-p "/api/auth/ws-ticket" url)
                 (hermes--promise-resolved
                  `(:status 200 :headers nil
                    :body ((ticket . ,ticket) (ttl_seconds . 30)))))
                (t (hermes--promise-rejected
                    (format "unexpected request %s" url)))))))
        (hermes-dashboard-transport-start
         :host "100.64.0.10" :port 9119
         :callback (lambda (event) (push event events)))
        (should (stringp browsed))
        (should (string-match-p "/auth/native/authorize" browsed))
        (should (string-match-p "code_challenge_method=S256" browsed))
        (should (string-match-p "redirect_uri=" browsed))
        (should (functionp loopback-filter))
        (let* ((state (and (string-match "state=\\([^&]+\\)" browsed)
                           (url-unhex-string (match-string 1 browsed))))
               (req (format "GET /callback?code=gw-code&state=%s HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
                            (url-hexify-string state))))
          (funcall loopback-filter loopback-process req))
        (setq requests (nreverse requests))
        (should (equal opened-url
                       (format "ws://100.64.0.10:9119/api/ws?ticket=%s" ticket)))
        (let* ((token-req (seq-find (lambda (r)
                                      (string-suffix-p "/auth/native/token"
                                                       (plist-get r :url)))
                                    requests))
               (ticket-req (seq-find (lambda (r)
                                       (string-suffix-p "/api/auth/ws-ticket"
                                                        (plist-get r :url)))
                                     requests))
               (token-body (json-parse-string (plist-get token-req :data)
                                              :object-type 'alist))
               (stored (gethash base store)))
          (should (equal (hermes-transport--get token-body 'code) "gw-code"))
          (should (hermes-transport--get token-body 'code_verifier))
          (should (equal (alist-get "Authorization"
                                    (plist-get ticket-req :headers)
                                    nil nil #'equal)
                         (concat "Bearer " access)))
          (should (equal (plist-get stored :access-token) access))
          (should (equal (plist-get stored :refresh-token) refresh)))
        (let ((visible (format "%S" events)))
          (dolist (secret (list access refresh ticket "gw-code"))
            (should-not (string-match-p (regexp-quote secret) visible)))
          (should (string-match-p "ticket=<redacted>" visible)))))))

(ert-deftest hermes-transport-dashboard-native-pkce-failure-preserves-prior-tokens ()
  "A failed native re-login leaves previously stored tokens untouched."
  (let* ((base "http://100.64.0.10:9119")
         (prior (list :access-token "prior-access"
                      :refresh-token "prior-refresh"
                      :expires-at 1
                      :provider "oauth"
                      :user-id "user-1"))
         (store (make-hash-table :test #'equal))
         browsed reason loopback-filter loopback-process)
    (puthash base prior store)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--native-token-load)
               (lambda (url) (gethash url store)))
              ((symbol-function 'hermes-dashboard-transport--native-token-store)
               (lambda (url tokens)
                 (if tokens
                     (puthash url tokens store)
                   (remhash url store))))
              ((symbol-function 'hermes-dashboard-transport--random-bytes)
               (lambda (n) (apply #'unibyte-string (make-list n ?B))))
              ((symbol-function 'hermes-dashboard-transport--browse-url)
               (lambda (url) (setq browsed url)))
              ((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq loopback-filter (plist-get plist :filter))
                 (setq loopback-process
                       (list 'fake-server :service 54322 :filter loopback-filter))
                 loopback-process))
              ((symbol-function 'process-contact)
               (lambda (proc &optional key &rest _)
                 (pcase key
                   (:service 54322)
                   (:local '(127 0 0 1 54322))
                   (_ proc))))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-live-p)
               (lambda (_proc) t)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (&rest _args) (error "must not open websocket")))
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) (error "remote attach must not spawn")))
            (hermes-dashboard-transport-http-request-async-function
             (lambda (url &rest _args)
               (cond
                ((string-suffix-p "/api/status" url)
                 (hermes--promise-resolved
                  '(:status 200 :headers nil
                    :body ((auth_required . t)
                           (auth_providers . ("oauth"))
                           (auth_flows . ("cookie" "native_pkce"))))))
                ((string-suffix-p "/auth/native/refresh" url)
                 (hermes--promise-rejected
                  "Hermes dashboard request failed at /auth/native/refresh (HTTP 401)"))
                ((string-suffix-p "/auth/native/token" url)
                 (hermes--promise-rejected
                  "Hermes dashboard request failed at /auth/native/token (HTTP 400)"))
                (t (hermes--promise-rejected
                    (format "unexpected request %s" url)))))))
        (let ((client (hermes-dashboard-transport-start
                       :host "100.64.0.10" :port 9119 :callback #'ignore)))
          (should (stringp browsed))
          (let* ((state (and (string-match "state=\\([^&]+\\)" browsed)
                             (url-unhex-string (match-string 1 browsed))))
                 (req (format "GET /callback?code=bad-code&state=%s HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
                              (url-hexify-string state))))
            (funcall loopback-filter loopback-process req))
          (hermes--promise-catch
           (hermes-dashboard-transport-client-ready-promise client)
           (lambda (r) (setq reason r))))
        (should (string-match-p "native\\|/auth/native/token\\|HTTP 400"
                                (format "%s" reason)))
        (should (equal (gethash base store) prior))))))

(ert-deftest hermes-transport-dashboard-native-refresh-preserves-refresh-token ()
  "Access-only refresh responses keep the prior refresh credential."
  (let* ((prior (list :access-token "old-access"
                      :refresh-token "keep-refresh"
                      :expires-at 1
                      :provider "oauth"
                      :user-id "user-1"))
         (next nil))
    (setq next
          (hermes-dashboard-transport--native-token-plist
           '((access_token . "new-access")
             (expires_at . 4102444800)
             (provider . "oauth")
             (user_id . "user-1"))
           prior))
    (should (equal (plist-get next :access-token) "new-access"))
    (should (equal (plist-get next :refresh-token) "keep-refresh"))))

(ert-deftest hermes-transport-dashboard-native-token-store-restores-prior-on-failure ()
  "A failed durable write restores the previous native token set."
  (let* ((base "http://100.64.0.10:9119")
         (prior (list :access-token "prior-access"
                      :refresh-token "prior-refresh"
                      :expires-at 4102444800
                      :provider "oauth"
                      :user-id "user-1"))
         (fresh (list :access-token "fresh-access"
                      :refresh-token "fresh-refresh"
                      :expires-at 4102444800
                      :provider "oauth"
                      :user-id "user-1"))
         (writes 0)
         (restored nil))
    (clrhash hermes-dashboard-transport--native-token-memory)
    (puthash (hermes-dashboard-transport--native-token-memory-key base)
             prior
             hermes-dashboard-transport--native-token-memory)
    (cl-letf (((symbol-function
                'hermes-dashboard-transport--require-auth-source)
               #'ignore)
              ((symbol-function
                'hermes-dashboard-transport--native-token-load-disk)
               (lambda (_url) prior))
              ((symbol-function
                'hermes-dashboard-transport--native-token-delete-disk)
               #'ignore)
              ((symbol-function
                'hermes-dashboard-transport--native-token-write-disk)
               (lambda (_url tokens)
                 (setq writes (1+ writes))
                 (setq restored tokens)
                 (if (= writes 1)
                     nil
                   t))))
      (should-error
       (hermes-dashboard-transport--native-token-store base fresh))
      (should (equal (gethash
                      (hermes-dashboard-transport--native-token-memory-key base)
                      hermes-dashboard-transport--native-token-memory)
                     prior))
      (should (equal restored prior)))))

(ert-deftest hermes-transport-dashboard-redacts-websocket-process-name ()
  (let* ((token-url "ws://127.0.0.1:4567/api/ws?token=secret-token")
         (safe-url "ws://127.0.0.1:4567/api/ws?token=<redacted>")
         (token-name (format "websocket to %s" token-url))
         process-name websocket-url)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq process-name (plist-get plist :name))
                 'fake-process))
              ((symbol-function 'websocket-inner-create)
               (lambda (&rest plist)
                 (setq websocket-url (plist-get plist :url))
                 'fake-websocket)))
      (should (eq (hermes-dashboard-transport--call-with-redacted-websocket-state
                   token-url safe-url
                   (lambda ()
                     (let ((conn (make-network-process
                                  :name token-name
                                  :buffer nil
                                  :host "127.0.0.1"
                                  :service 4567)))
                       (websocket-inner-create :conn conn :url token-url))))
                  'fake-websocket)))
    (should (equal process-name (format "websocket to %s" safe-url)))
    (should (equal websocket-url safe-url))
    (should-not (string-match-p "secret-token" process-name))
    (should-not (string-match-p "secret-token" websocket-url))))

(ert-deftest hermes-transport-dashboard-redacts-ticket-websocket-name ()
  (let* ((ticket-url "wss://dash.example/hermes/api/ws?ticket=ticket-secret")
         (safe-url "wss://dash.example/hermes/api/ws?ticket=<redacted>")
         (ticket-name (format "websocket to %s" ticket-url))
         process-name websocket-url)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq process-name (plist-get plist :name))
                 'fake-process))
              ((symbol-function 'websocket-inner-create)
               (lambda (&rest plist)
                 (setq websocket-url (plist-get plist :url))
                 'fake-websocket)))
      (should (eq (hermes-dashboard-transport--call-with-redacted-websocket-state
                   ticket-url safe-url
                   (lambda ()
                     (let ((conn (make-network-process
                                  :name ticket-name
                                  :buffer nil
                                  :host "dash.example"
                                  :service 443)))
                       (websocket-inner-create :conn conn :url ticket-url))))
                  'fake-websocket)))
    (should (equal process-name (format "websocket to %s" safe-url)))
    (should (equal websocket-url safe-url))
    (should-not (string-match-p "ticket-secret" process-name))
    (should-not (string-match-p "ticket-secret" websocket-url))))

(ert-deftest hermes-transport-dashboard-close-marks-client-not-live ()
  (let (on-close events rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-close (plist-get args :on-close))
                 'fake-websocket)))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :host "127.0.0.1"
                      :port 4567
                      :token "secret-token"
                      :websocket 'fake-websocket
                      :ready-p t
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "prompt.submit"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (should (eq (hermes-dashboard-transport--default-websocket-open
                     "ws://127.0.0.1:4567/api/ws?token=secret-token"
                     client)
                    'fake-websocket))
        (should (functionp on-close))
        (funcall on-close 'fake-websocket)
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "closed" rejected))
        (should (equal (plist-get (car events) :status) "closed"))))))

(ert-deftest hermes-transport-dashboard-error-marks-client-not-live ()
  (let (on-error events rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :host "127.0.0.1"
                      :port 4567
                      :token "secret-token"
                      :websocket 'fake-websocket
                      :ready-p t
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "prompt.submit"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (should (eq (hermes-dashboard-transport--default-websocket-open
                     "ws://127.0.0.1:4567/api/ws?token=secret-token"
                     client)
                    'fake-websocket))
        (should (functionp on-error))
        (funcall on-error 'fake-websocket 'error "socket died secret-token")
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "Hermes dashboard WebSocket error" rejected))
        (should (string-match-p "<redacted>" rejected))
        (should-not (string-match-p "secret-token" rejected))
        (should (equal (plist-get (car events) :type) 'status))
        (should (equal (plist-get (car events) :status) "closed"))
        (should (string-match-p "<redacted>"
                                (plist-get (car events) :content)))
        (should-not (string-match-p "secret-token"
                                    (plist-get (car events) :content)))))))

(ert-deftest hermes-transport-dashboard-close-rejects-pending-requests ()
  (let (on-close rejects events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-close (plist-get args :on-close))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-command-dispatch
           client "queue" "next"
           :reject (lambda (message)
                     (push (cons 'control message) rejects)))
          (hermes-dashboard-transport-approval-respond
           client :choice "deny"
           :reject (lambda (message)
                     (push (cons 'prompt message) rejects))))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   2))
        (funcall on-close 'fake-websocket)
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (dolist (kind '(control prompt))
          (should (string-match-p "WebSocket closed"
                                  (alist-get kind rejects))))
        (should (equal (plist-get (car events) :status) "closed"))))))

(ert-deftest hermes-transport-dashboard-request-timeout-rejects-pending ()
  "An unanswered request is rejected once its timeout timer fires."
  (let ((client (make-hermes-dashboard-transport-client
                 :token "secret-token"
                 :websocket 'fake-websocket
                 :ready-p t
                 :pending (make-hash-table :test #'equal)))
        (hermes-dashboard-transport-websocket-send-function #'ignore)
        (hermes-dashboard-transport-request-timeout 30)
        timer-callback rejected)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timer-callback (cons fn args))
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (hermes-dashboard-transport-request
       client "session.create" nil nil
       (lambda (message) (setq rejected message)))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 1))
      (apply (car timer-callback) (cdr timer-callback))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (should (string-match-p "timed out" rejected))
      (should (string-match-p "session.create" rejected)))))

(ert-deftest hermes-transport-dashboard-request-timeout-varies-by-method ()
  "A prompt submission gets longer than the customizable RPC timeout."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'fake-websocket
                 :ready-p t
                 :pending (make-hash-table :test #'equal)))
        (hermes-dashboard-transport-websocket-send-function #'ignore)
        (hermes-dashboard-transport-request-timeout 30)
        timeouts)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seconds _repeat _function &rest _args)
                 (push seconds timeouts)
                 'fake-timer)))
      (hermes-dashboard-transport-request client "session.create")
      (hermes-dashboard-transport-request client "prompt.submit")
      (should (equal (nreverse timeouts) '(30 1800))))))

(ert-deftest hermes-transport-dashboard-model-save-key-sends-key-never-logs-it ()
  "`model.save_key' sends slug/api_key/session_id; a rejection never echoes the key."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'fake-websocket
                 :ready-p t
                 :pending (make-hash-table :test #'equal)))
        (hermes-dashboard-transport-request-timeout 30)
        sent timer-callback rejected)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timer-callback (cons fn args))
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((hermes-dashboard-transport-websocket-send-function
             (lambda (_ws text) (setq sent text))))
        (hermes-dashboard-transport-model-save-key
         client "deepseek" "sk-super-secret" :session-id "s1"
         :reject (lambda (message) (setq rejected message))))
      (let ((params (alist-get 'params (json-parse-string sent :object-type 'alist))))
        (should (equal (alist-get 'slug params) "deepseek"))
        (should (equal (alist-get 'api_key params) "sk-super-secret"))
        (should (equal (alist-get 'session_id params) "s1")))
      ;; The transport does not auto-redact api_key, so the error path must not
      ;; interpolate request params: a rejection names the method, never the key.
      (apply (car timer-callback) (cdr timer-callback))
      (should (string-match-p "model.save_key" rejected))
      (should-not (string-match-p "sk-super-secret" rejected)))))

(ert-deftest hermes-transport-dashboard-error-rejects-pending-requests ()
  (let (on-error rejected events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-session-interrupt
           client
           :reject (lambda (message) (setq rejected message))))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   1))
        (funcall on-error 'fake-websocket 'error "socket died")
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "socket died" rejected))
        (should (= (cl-count "closed" events
                             :key (lambda (event) (plist-get event :status))
                             :test #'equal)
                   1))))))

(ert-deftest hermes-transport-dashboard-error-with-unhandled-pending-emits-once ()
  (let (on-error events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-prompt-submit client "hello"))
        (funcall on-error 'fake-websocket 'error "socket died")
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (= (cl-count 'error events
                             :key (lambda (event) (plist-get event :type)))
                   1))
        (let ((event (cl-find 'error events
                              :key (lambda (e) (plist-get e :type)))))
          (should (equal (plist-get event :method) "prompt.submit"))
          (should (string-match-p "socket died"
                                  (plist-get event :content))))))))

(ert-deftest hermes-transport-dashboard-resume-does-not-store-session-id ()
  "A `session.resume' response must not mutate the shared client.
Session identity is buffer-local; the shared transport client stays
transport-only so two chat buffers sharing one socket cannot clobber each
other's session."
  :tags '(shared-socket-isolation)
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'fake-websocket
                 :pending (make-hash-table :test #'equal)
                 :callback #'ignore))
        (hermes-dashboard-transport-websocket-send-function #'ignore))
    (hermes-dashboard-transport-request client "session.resume" nil)
    (hermes-dashboard-transport--handle-frame
     client (hermes-dashboard-transport--encode-frame
             '((jsonrpc . "2.0")
               (id . "hermes-el-1")
               (result . ((session_id . "sid-live")
                          (resumed . "sid-stored"))))))
    (should-not (hermes-dashboard-transport-client-session-id client))
    (should-not (hermes-dashboard-transport-client-stored-session-id client))))

(ert-deftest hermes-transport-dashboard-connect-error-redacts-token ()
  (let (events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 1)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (error "connect failed: %s" url))))
        (hermes-dashboard-transport-start
         :callback (lambda (event) (push event events)))
        (let ((text (format "%S" events)))
          (should (string-match-p "<redacted>" text))
          (should-not (string-match-p "secret-token" text)))))))

(ert-deftest hermes-transport-dashboard-user-error-redacts-token ()
  (let ((open-attempts 0)
        (token "leaky-dashboard-token-abc123")
        events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () token))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 3)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (cl-incf open-attempts)
               (user-error "bad websocket url %s" url))))
        (hermes-dashboard-transport-start
         :callback (lambda (event) (push event events)))
        (should (= open-attempts 1))
        (let ((text (format "%S" events)))
          (should (string-match-p "<redacted>" text))
          (should-not (string-match-p token text)))))))

(ert-deftest hermes-transport-dashboard-start-process-error-redacts-token ()
  (let ((token "leaky-dashboard-token-abc123")
        events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () token))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (error "spawn failed with env %S" (plist-get plist :env)))))
        (let ((message (condition-case error
                           (progn
                             (hermes-dashboard-transport-start
                              :callback (lambda (event) (push event events)))
                             nil)
                         (user-error (error-message-string error))
                         (error (error-message-string error)))))
          (should message)
          (should (string-match-p "<redacted>" message))
          (should-not (string-match-p token message))
          (should-not (string-match-p token (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-start-cleans-process-on-connect-failure ()
  (let (deleted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 1)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client) (error "dashboard not ready"))))
        (hermes-dashboard-transport-start)
        (should (eq deleted 'fake-process))))))

(ert-deftest hermes-transport-dashboard-does-not-retry-user-errors ()
  (let ((open-attempts 0)
        scheduled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 3)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-schedule-function
             (lambda (delay fn &rest args) (push delay scheduled) (apply fn args)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client)
               (cl-incf open-attempts)
               (user-error "Install websocket.el"))))
        (hermes-dashboard-transport-start)
        (should (= open-attempts 1))
        (should-not scheduled)))))

(ert-deftest hermes-transport-dashboard-readiness-window-is-bounded ()
  "The async connect retry window covers a cold start without the old 45s budget."
  (let ((window (* (1- hermes-dashboard-transport-connect-retries)
                   hermes-dashboard-transport-connect-retry-delay)))
    (should (>= window 5))
    (should (<= window 20))))

(ert-deftest hermes-transport-dashboard-start-resolves-on-gateway-ready ()
  "Start returns a not-yet-ready client; `gateway.ready' marks it ready."
  (let (events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client) 'fake-websocket)))
        (let ((client (hermes-dashboard-transport-start
                       :callback (lambda (event) (push event events)))))
          (should-not (hermes-dashboard-transport-client-ready-p client))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   '((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . "gateway.ready"))))))
          (should (hermes-dashboard-transport-client-ready-p client))
          (should (cl-find "gateway.ready" events
                           :key (lambda (event) (plist-get event :event))
                           :test #'equal)))))))

(ert-deftest hermes-transport-dashboard-start-timeout-cleans-websocket ()
  (let (closed deleted opened-client)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567))
              ((symbol-function 'websocket-close)
               (lambda (websocket) (setq closed websocket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-ready-timeout 1)
            (hermes-dashboard-transport-schedule-function
             (lambda (_delay fn &rest args) (apply fn args)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url client)
               (setq opened-client client)
               'fake-websocket)))
        (hermes-dashboard-transport-start)
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should opened-client)
        (should-not (hermes-dashboard-transport-client-websocket opened-client))
        (should-not (hermes-dashboard-transport-client-ready-p opened-client))
        (should-not (hermes-dashboard-transport-client-process opened-client))))))

(ert-deftest hermes-transport-dashboard-stop-releases-resources-and-rejects-pending ()
  (let (closed deleted rejected events)
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (websocket) (setq closed websocket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :process 'fake-process
                      :websocket 'fake-websocket
                      :ready-p t
                      :token "secret-token"
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "session.create"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (hermes-dashboard-transport-stop client "stopped secret-token")
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-process client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "stopped" rejected))
        (should (string-match-p "<redacted>" rejected))
        (should-not (string-match-p "secret-token" rejected))
        (should-not events)))))

(ert-deftest hermes-transport-dashboard-request-sends-immediately-when-ready ()
  "A client without a readiness promise sends the frame at once."
  (let* ((client (make-hermes-dashboard-transport-client :token "t"))
         (sent nil)
         (hermes-dashboard-transport-request-timeout nil)
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text) (push text sent))))
    (hermes-dashboard-transport-request client "ping")
    (should (= (length sent) 1))))

(ert-deftest hermes-transport-dashboard-request-defers-until-ready ()
  "A request waits for the client's readiness promise before sending."
  (let* ((client (make-hermes-dashboard-transport-client :token "t"))
         (ready (hermes--promise-make))
         (sent nil)
         (rejected nil)
         (hermes-dashboard-transport-request-timeout nil)
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text) (push text sent))))
    (setf (hermes-dashboard-transport-client-ready-promise client) ready)
    (hermes-dashboard-transport-request client "ping" nil #'ignore
                                        (lambda (_m) (setq rejected t)))
    (should (null sent))
    (hermes--promise-resolve ready client)
    (should (= (length sent) 1))
    (should-not rejected)))

(ert-deftest hermes-transport-dashboard-request-rejected-when-readiness-fails ()
  "A rejected readiness promise rejects the deferred request without sending."
  (let* ((client (make-hermes-dashboard-transport-client :token "session-secret"))
         (ready (hermes--promise-make))
         (sent nil)
         (reason nil)
         (hermes-dashboard-transport-request-timeout nil)
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text) (push text sent))))
    (setf (hermes-dashboard-transport-client-ready-promise client) ready)
    (hermes-dashboard-transport-request client "ping" nil #'ignore
                                        (lambda (m) (setq reason m)))
    (hermes--promise-reject ready "connect failed")
    (should (null sent))
    (should (string-match-p "connect failed" reason))))

(ert-deftest hermes-transport-dashboard-jsonrpc-correlates-responses ()
  (let* ((sent nil)
         (first-result nil)
         (second-result nil)
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :pending (make-hash-table :test #'equal)
                  :callback #'ignore))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_websocket text)
            (push (hermes-dashboard-transport--decode-frame text) sent))))
    (hermes-dashboard-transport-request
     client "session.create" '((cols . 80))
     (lambda (result) (setq first-result result)))
    (hermes-dashboard-transport-request
     client "prompt.submit" '((session_id . "sid") (text . "hello"))
     (lambda (result) (setq second-result result)))
    (let ((ids (mapcar (lambda (frame) (alist-get 'id frame)) sent)))
      (should (equal ids '("hermes-el-2" "hermes-el-1")))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 2)))
    (hermes-dashboard-transport--handle-frame
     client "{\"jsonrpc\":\"2.0\",\"id\":\"hermes-el-2\",\"result\":{\"ok\":true}}")
    (hermes-dashboard-transport--handle-frame
     client "{\"jsonrpc\":\"2.0\",\"id\":\"hermes-el-1\",\"result\":{\"session_id\":\"sid\"}}")
    (should (equal second-result '((ok . t))))
    (should (equal first-result '((session_id . "sid"))))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               0))))

(ert-deftest hermes-transport-dashboard-jsonrpc-error-rejects-pending-request ()
  (let* ((sent nil)
         rejected
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :pending (make-hash-table :test #'equal)
                  :callback #'ignore))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_websocket text)
            (push (hermes-dashboard-transport--decode-frame text) sent))))
    (hermes-dashboard-transport-command-dispatch
     client "nope" "arg"
     :reject (lambda (message) (setq rejected message)))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               1))
    (hermes-dashboard-transport--handle-frame
     client (hermes-dashboard-transport--encode-frame
             '((jsonrpc . "2.0")
               (id . "hermes-el-1")
               (error . ((code . -32601)
                         (message . "unknown command"))))))
    (should (equal rejected "unknown command"))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               0))))

(ert-deftest hermes-transport-dashboard-jsonrpc-send-failure-clears-pending ()
  (let (rejected events)
    (let* ((client (make-hermes-dashboard-transport-client
                    :websocket 'fake-websocket
                    :pending (make-hash-table :test #'equal)
                    :token "secret-token"
                    :callback (lambda (event) (push event events))))
           (hermes-dashboard-transport-websocket-send-function
            (lambda (_websocket _text)
              (error "send failed for secret-token"))))
      (hermes-dashboard-transport-request
       client "session.create" nil nil
       (lambda (message) (setq rejected message)))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (should (string-match-p "send failed" rejected))
      (should (string-match-p "<redacted>" rejected))
      (should-not (string-match-p "secret-token" rejected))
      (should-not events)
      (hermes-dashboard-transport-request client "prompt.submit" nil)
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (let ((event (car events)))
        (should (eq (plist-get event :type) 'error))
        (should (equal (plist-get event :method) "prompt.submit"))
        (should (string-match-p "send failed" (plist-get event :content)))
        (should (string-match-p "<redacted>" (plist-get event :content)))
        (should-not (string-match-p "secret-token"
                                    (plist-get event :content)))))))

(ert-deftest hermes-transport-dashboard-connects-with-fakes ()
  (let (process-plist opened-url sent events sleeps
                      (open-attempts 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567))
              ((symbol-function 'executable-find)
               (lambda (_program) nil))
              ((symbol-function 'file-executable-p)
               (lambda (_file) nil)))
      (let ((hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-url "http://127.0.0.1:4567")
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (setq process-plist plist)
               'fake-process))
            (hermes-dashboard-transport-connect-retries 2)
            (hermes-dashboard-transport-connect-retry-delay 0.05)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-schedule-function
             (lambda (delay fn &rest args) (push delay sleeps) (apply fn args)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url client)
               (cl-incf open-attempts)
               (setq opened-url url)
               (should (hermes-dashboard-transport-client-p client))
               (if (= open-attempts 1)
                   (error "dashboard not ready")
                 (hermes-dashboard-transport--handle-frame
                  client (hermes-dashboard-transport--encode-frame
                          '((jsonrpc . "2.0")
                            (method . "event")
                            (params . ((type . "gateway.ready")
                                       (session_id . "sid"))))))
                 'fake-websocket)))
            (hermes-dashboard-transport-websocket-send-function
             (lambda (_websocket text)
               (push (hermes-dashboard-transport--decode-frame text) sent))))
        (let ((client (hermes-dashboard-transport-start
                       :callback (lambda (event) (push event events)))))
          (should (eq (hermes-dashboard-transport-client-process client)
                      'fake-process))
          (should (eq (hermes-dashboard-transport-client-websocket client)
                      'fake-websocket))
          (should (= open-attempts 2))
          (should (equal sleeps '(0.05)))
          (should (equal (plist-get process-plist :command)
                         '("hermes" "dashboard" "--no-open" "--tui" "--isolated"
                           "--host" "127.0.0.1" "--port" "4567")))
          (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token"
                          (plist-get process-plist :env)))
          (should (equal opened-url
                         "ws://127.0.0.1:4567/api/ws?token=secret-token"))
          (should-not (string-match-p "secret-token" (format "%S" events)))
          (should (hermes-dashboard-transport-client-ready-p client))
          (hermes-dashboard-transport-session-create client :cols 90 :title "Chat")
          (hermes-dashboard-transport-session-resume client "sid" :cols 90)
          (hermes-dashboard-transport-prompt-submit client "hello")
          (hermes-dashboard-transport-session-interrupt client)
          (hermes-dashboard-transport-session-steer client "cite files")
          (hermes-dashboard-transport-commands-catalog client)
          (hermes-dashboard-transport-command-dispatch client "queue" "next")
          (hermes-dashboard-transport-slash-exec client "queue next")
          (hermes-dashboard-transport-approval-respond client :choice "approve")
          (hermes-dashboard-transport-clarify-respond client "req-1" "answer")
          (hermes-dashboard-transport-sudo-respond client "req-2" "password")
          (hermes-dashboard-transport-secret-respond client "req-3" "value")
          (should (equal (mapcar (lambda (frame) (alist-get 'method frame))
                                 (nreverse sent))
                         '("session.create" "session.resume" "prompt.submit"
                           "session.interrupt" "session.steer"
                           "commands.catalog" "command.dispatch" "slash.exec"
                           "approval.respond" "clarify.respond" "sudo.respond"
                           "secret.respond")))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   '((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . "message.delta")
                                (session_id . "sid")
                                (payload . ((text . "hi"))))))))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   '((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . "message.complete")
                                (session_id . "sid")
                                (payload . ((text . "done"))))))))
          (should (equal (mapcar (lambda (event) (plist-get event :type))
                                 (nreverse events))
                         '(status status delta done))))))))

(ert-deftest hermes-transport-dashboard-complete-status-is-preserved ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.complete")
                            (session_id . "sid")
                            (payload . ((text . "Stopped")
                                        (status . "interrupted")))))))))
    (let ((event (car events)))
      (should (equal (plist-get event :type) 'error))
      (should (equal (plist-get event :event) "message.complete"))
      (should (equal (plist-get event :status) "interrupted"))
      (should (equal (plist-get event :content) "Stopped")))))

(ert-deftest hermes-transport-dashboard-normalizes-message-interim ()
  "An interim assistant boundary preserves text and stream provenance."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "message.interim")
                             (session_id . "sid")
                             (payload . ((text . "candidate")
                                         (already_streamed . t)))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (eq (plist-get event :type) 'interim))
    (should (equal (plist-get event :event) "message.interim"))
    (should (equal (plist-get event :content) "candidate"))
    (should (eq (plist-get event :already-streamed) t))))

(ert-deftest hermes-transport-dashboard-complete-status-done-is-terminal ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.complete")
                            (session_id . "sid")
                            (payload . ((text . "Done")
                                        (status . "done")))))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'done))
      (should (equal (plist-get event :event) "message.complete"))
      (should (equal (plist-get event :status) "done"))
      (should (equal (plist-get event :content) "Done")))))

(ert-deftest hermes-transport-dashboard-normalizes-session-info ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "session.info")
                            (session_id . "sid")
                            (payload . ((model . "gpt-5.5")
                                        (provider . "openai-codex")
                                        (profile_name . "planner")
                                        (running . t)
                                        (reasoning_effort . "high")
                                        (fast . t)
                                        (yolo . :false)
                                        (goal . ((status . "active")
                                                 (running . t)
                                                 (turns_used . 3)
                                                 (max_turns . 20)))
                                        (usage . ((context_used . 45000)
                                                  (context_max . 200000)
                                                  (context_percent . 22)))))))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :event) "session.info"))
      (should (equal (plist-get event :session-id) "sid"))
      (should (equal (plist-get event :status) "running"))
      (should (eq (plist-get event :running) t))
      (should (equal (plist-get event :model) "gpt-5.5"))
      (should (equal (plist-get event :agent-name) "planner"))
      (should (equal (plist-get event :reasoning-effort) "high"))
      (should (eq (plist-get event :fast) t))
      (should (plist-member event :yolo))
      (should-not (plist-get event :yolo))
      (should (equal (plist-get event :goal)
                     '(:status "active" :running t
                               :turns-used 3 :max-turns 20)))
      (should (equal (plist-get event :context) '(:used 45000 :max 200000 :percent 22)))
      (should (equal (plist-get event :content)
                     "Session ready: gpt-5.5 via openai-codex")))))

(ert-deftest hermes-transport-dashboard-normalizes-goal-change ()
  "Structured goal changes stay out of generic transcript status handling."
  (let ((event (car (hermes-test--dashboard-events
                     '("goal.changed" .
                       ((goal . ((status . "active")
                                 (running . t)
                                 (turns_used . 4)
                                 (max_turns . 20)))))))))
    (should (eq (plist-get event :type) 'goal))
    (should (equal (plist-get event :goal)
                   '(:status "active" :running t
                             :turns-used 4 :max-turns 20)))))

(ert-deftest hermes-transport-dashboard-session-info-preserves-idle-state ()
  "A false running field remains distinguishable from an absent field."
  (let ((event (car (hermes-test--dashboard-events
                     '("session.info" . ((running . :false)))))))
    (should (equal (plist-get event :status) "ready"))
    (should (plist-member event :running))
    (should-not (plist-get event :running))))

(ert-deftest hermes-transport-dashboard-normalizes-reasoning-events ()
  "`reasoning.delta' becomes commentary; `thinking.delta' becomes a `thinking' event."
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (dolist (type '("reasoning.delta" "thinking.delta"))
        (hermes-dashboard-transport--handle-frame
         client (hermes-dashboard-transport--encode-frame
                 `((jsonrpc . "2.0")
                   (method . "event")
                   (params . ((type . ,type)
                              (session_id . "sid")
                              (payload . ((text . "inspect first"))))))))))
    (let ((events (nreverse events)))
      (should (equal (mapcar (lambda (event) (plist-get event :type)) events)
                     '(commentary thinking)))
      (should (equal (mapcar (lambda (event) (plist-get event :event)) events)
                     '("reasoning.delta" "thinking.delta")))
      (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                             events)
                     '("sid" "sid")))
      (should (equal (mapcar (lambda (event) (plist-get event :content))
                             events)
                     '("inspect first" "inspect first"))))))

(ert-deftest hermes-transport-dashboard-normalizes-subagent-events ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (dolist (spec '(("subagent.thinking"
                       ((subagent_id . "sa-1")
                        (text . "(°ロ°) brainstorming...")))
                      ("subagent.tool"
                       ((subagent_id . "sa-1")
                        (tool_name . "terminal")
                        (tool_preview . "git status")
                        (text . "git status")))
                      ("subagent.progress"
                       ((subagent_id . "sa-1")
                        (text . "🔀 terminal, read_file")))
                      ("subagent.complete"
                       ((subagent_id . "sa-1")
                        (status . "completed")
                        (summary . "no merge recommended")))))
        (pcase-let ((`(,type ,payload) spec))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   `((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . ,type)
                                (session_id . "sid")
                                (payload . ,payload)))))))))
    (pcase-let ((`(,thinking ,tool ,progress ,complete) (nreverse events)))
      (should (eq (plist-get thinking :type) 'commentary))
      (should (equal (plist-get thinking :event) "subagent.thinking"))
      (should (equal (plist-get thinking :subagent-id) "sa-1"))
      (should (equal (plist-get thinking :content)
                     "(°ロ°) brainstorming..."))
      (should (eq (plist-get tool :type) 'tool))
      (should (equal (plist-get tool :event) "subagent.tool"))
      (should (equal (plist-get tool :name) "terminal"))
      (should (equal (plist-get tool :status) "running"))
      (should (equal (plist-get tool :preview) "git status"))
      (should (equal (plist-get tool :subagent-id) "sa-1"))
      (should (eq (plist-get progress :type) 'progress))
      (should (equal (plist-get progress :content)
                     "🔀 terminal, read_file"))
      (should (equal (plist-get progress :subagent-id) "sa-1"))
      (should (eq (plist-get complete :type) 'status))
      (should (equal (plist-get complete :status) "completed"))
      (should (equal (plist-get complete :content)
                     "no merge recommended")))))

(ert-deftest hermes-transport-dashboard-normalizes-tool-payloads-and-inline-diff ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                  :callback (lambda (event) (push event events))))
          (inline-diff "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1 @@\n-old\n+new\n"))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "tool.start")
                            (session_id . "sid")
                            (payload . ((tool_id . "tool-1")
                                        (name . "terminal")
                                        (context . "running make test")
                                        (args_text . "make test"))))))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               `((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "tool.complete")
                            (session_id . "sid")
                            (payload . ((tool_id . "tool-1")
                                        (name . "terminal")
                                        (summary . "updated file")
                                        (result_text . "ok")
                                        (inline_diff . ,inline-diff)
                                        (duration_s . 1.2))))))))
      (pcase-let ((`(,start ,complete ,diff) (nreverse events)))
        (should (eq (plist-get start :type) 'tool))
        (should (equal (plist-get start :preview) "running make test"))
        (should (equal (plist-get start :args) "make test"))
        (should (eq (plist-get complete :type) 'tool))
        (should (equal (plist-get complete :preview) "updated file"))
        (should (equal (plist-get complete :summary) "updated file"))
        (should (equal (plist-get complete :result-text) "ok"))
        (should (equal (plist-get complete :duration) 1.2))
        (should (eq (plist-get diff :type) 'diff))
        (should (equal (plist-get diff :session-id) "sid"))
        (should (equal (plist-get diff :content) inline-diff))))))

(ert-deftest hermes-transport-dashboard-tool-generating-is-header-thinking ()
  "`tool.generating' becomes a header-only thinking hint naming the tool."
  (let ((event (car (hermes-test--dashboard-events
                     '("tool.generating" . ((name . "skill_view")))))))
    (should (eq (plist-get event :type) 'thinking))
    (should (equal (plist-get event :content) "Calling skill_view"))))

(ert-deftest hermes-transport-dashboard-progress-events ()
  "`browser.progress' and `preview.restart.progress' become progress events."
  (pcase-let ((`(,browser ,preview)
               (hermes-test--dashboard-events
                '("browser.progress" . ((message . "Navigating to example.com")
                                        (level . "info")))
                '("preview.restart.progress" . ((task_id . "t1")
                                                (text . "Restarting preview"))))))
    (should (eq (plist-get browser :type) 'progress))
    (should (equal (plist-get browser :content) "Navigating to example.com"))
    (should (eq (plist-get preview :type) 'progress))
    (should (equal (plist-get preview :content) "Restarting preview"))))

(ert-deftest hermes-transport-dashboard-status-fallback-uses-text-then-label ()
  "Unclassified events become status lines: payload text, else a derived label."
  (pcase-let ((`(,complete ,unknown)
               (hermes-test--dashboard-events
                '("preview.restart.complete" . ((task_id . "t1")
                                                (text . "Preview ready")))
                '("unknown.widget.request" . ((request_id . "r1"))))))
    (should (eq (plist-get complete :type) 'status))
    (should (equal (plist-get complete :content) "Preview ready"))
    ;; No text payload, so the event name is prettified into the body.
    (should (eq (plist-get unknown :type) 'status))
    (should (equal (plist-get unknown :content) "Unknown Widget Request"))))

(ert-deftest hermes-transport-dashboard-background-complete-event ()
  "`background.complete' becomes a `background' event with task id and response."
  (let ((event (car (hermes-test--dashboard-events
                     '("background.complete" . ((task_id . "bg_abc123")
                                                (text . "Yes, x_search is available.")))))))
    (should (eq (plist-get event :type) 'background))
    (should (equal (plist-get event :task-id) "bg_abc123"))
    (should (equal (plist-get event :content) "Yes, x_search is available."))))

(ert-deftest hermes-transport-dashboard-unmapped-event-renders-not-unknown ()
  "A gateway event the client never heard of still renders as a labelled line.
This is the contract that replaces hand-mirroring every event name: an invented
`future.thing' is displayed, never surfaced as an Unknown error."
  (let ((event (car (hermes-test--dashboard-events
                     '("future.thing" . ((text . "hello from the future")))))))
    (should (eq (plist-get event :type) 'status))
    (should-not (eq (plist-get event :type) 'unknown))
    (should (equal (plist-get event :content) "hello from the future"))))

(ert-deftest hermes-transport-dashboard-drops-voice-and-skin-events ()
  "Voice and skin events are dropped, not surfaced as Unknown events."
  (should-not (hermes-test--dashboard-events
               '("voice.status" . ((state . "listening")))
               '("voice.transcript" . ((text . "hello")))
               '("skin.changed" . ((name . "dark"))))))

(ert-deftest hermes-transport-normalizes-legacy-events ()
  (should (equal (hermes-transport-normalize-event
                  '(:type delta :content "hello"))
                 '(:type delta :content "hello")))
  (should (equal (hermes-transport-normalize-event '(:type done))
                 '(:type done)))
  (should (equal (hermes-transport-normalize-event
                  '(:type error :content "boom"))
                 '(:type error :content "boom"))))

(ert-deftest hermes-transport-normalizes-progress-status-tool-commentary ()
  (let ((progress (hermes-transport-normalize-event
                   '((event . "tool.progress")
                     (tool_name . "terminal")
                     (delta . "running make test"))))
        (status (car (hermes-transport-parse-events
                      "event: run.started\ndata: {\"run_id\":\"r1\",\"status\":\"running\"}\n\n")))
        (tool (hermes-transport-normalize-event
               '((event . "tool.started")
                 (tool . "read_file")
                 (preview . "AGENTS.md")
                 (args . ((path . "AGENTS.md"))))))
        (hermes-tool (car (hermes-transport-parse-events
                           (concat "event: hermes.tool.progress\n"
                                   "data: {\"tool\":\"terminal\","
                                   "\"label\":\"Running make test\","
                                   "\"toolCallId\":\"call-1\","
                                   "\"status\":\"running\"}\n\n"))))
        (commentary (hermes-transport-normalize-event
                     '((event . "reasoning.available")
                       (text . "I'll inspect the repo first.")))))
    (should (eq (plist-get progress :type) 'progress))
    (should (equal (plist-get progress :event) "tool.progress"))
    (should (equal (plist-get progress :name) "terminal"))
    (should (equal (plist-get progress :content) "running make test"))
    (should (eq (plist-get status :type) 'status))
    (should (equal (plist-get status :event) "run.started"))
    (should (equal (plist-get status :run-id) "r1"))
    (should (equal (plist-get status :status) "running"))
    (should (eq (plist-get tool :type) 'tool))
    (should (equal (plist-get tool :name) "read_file"))
    (should (equal (plist-get tool :status) "started"))
    (should (equal (plist-get tool :preview) "AGENTS.md"))
    (should (equal (plist-get tool :args) '((path . "AGENTS.md"))))
    (should (eq (plist-get hermes-tool :type) 'tool))
    (should (equal (plist-get hermes-tool :event) "hermes.tool.progress"))
    (should (equal (plist-get hermes-tool :name) "terminal"))
    (should (equal (plist-get hermes-tool :status) "running"))
    (should (equal (plist-get hermes-tool :preview) "Running make test"))
    (should (equal (plist-get hermes-tool :tool-call-id) "call-1"))
    (should (eq (plist-get commentary :type) 'commentary))
    (should (equal (plist-get commentary :content)
                   "I'll inspect the repo first."))))

(ert-deftest hermes-transport-normalizes-hermes-agent-tool-events ()
  (let ((chunk (hermes-transport-normalize-event
                '((type . "ToolCallChunk")
                  (tool_name . "read_file")
                  (preview . "AGENTS.md")
                  (index . 2)
                  (args . ((path . "AGENTS.md"))))))
        (finished (hermes-transport-normalize-event
                   '((type . "ToolCallFinished")
                     (tool_name . "read_file")
                     (duration . 1.2)
                     (ok . t)
                     (index . 2))))
        (failed (hermes-transport-normalize-event
                 '((type . "ToolCallFinished")
                   (tool_name . "terminal")
                   (duration . 0.1)
                   (ok . nil)
                   (index . 3)))))
    (should (eq (plist-get chunk :type) 'tool))
    (should (equal (plist-get chunk :event) "ToolCallChunk"))
    (should (equal (plist-get chunk :name) "read_file"))
    (should (equal (plist-get chunk :status) "running"))
    (should (equal (plist-get chunk :preview) "AGENTS.md"))
    (should (equal (plist-get chunk :index) 2))
    (should (equal (plist-get chunk :args) '((path . "AGENTS.md"))))
    (should (equal (plist-get finished :status) "completed"))
    (should (equal (plist-get finished :duration) 1.2))
    (should (equal (plist-get finished :index) 2))
    (should (equal (plist-get failed :status) "failed"))))

(ert-deftest hermes-transport-parses-sse-and-preserves-plain-text ()
  (let ((events (hermes-transport-parse-events
                 (concat "event: assistant.delta\n"
                         "data: {\"delta\":\"hello\"}\n\n"
                         "data: {\"event\":\"done\"}\n\n"))))
    (should (equal (mapcar (lambda (event) (plist-get event :type)) events)
                   '(delta done)))
    (should (equal (plist-get (car events) :content) "hello")))
  (should (equal (hermes-transport-parse-events "plain CLI output")
                 '((:type delta :content "plain CLI output"))))
  (should (equal (hermes-transport-parse-events "running" "tool.progress")
                 '((:type progress :event "tool.progress" :content "running"))))
  (should (equal (hermes-transport-parse-events ": keepalive\n\n")
                 nil))
  (should (equal (hermes-transport-parse-events "{\"answer\":42}")
                 '((:type delta :content "{\"answer\":42}")))))

(ert-deftest hermes-transport-handles-unknown-and-invalid-events ()
  (let* ((raw '((event . "alien.signal") (payload . 1)))
         (unknown (hermes-transport-normalize-event raw))
         (invalid (hermes-transport-normalize-event '(:content "missing type")))
         (message-error (hermes-transport-normalize-event
                         '((event . "error") (message . "boom"))))
         (response-error (hermes-transport-normalize-event
                          '((type . "response.failed")
                            (response . ((error . ((message . "bad request"))))))))
         (bad-json (car (hermes-transport-parse-events
                         "{\"type\": \"status\""))))
    (should (eq (plist-get unknown :type) 'unknown))
    (should (equal (plist-get unknown :event) "alien.signal"))
    (should (equal (plist-get unknown :raw) raw))
    (should (eq (plist-get invalid :type) 'error))
    (should (string-match-p "Invalid Hermes transport event"
                            (plist-get invalid :content)))
    (should (equal (plist-get message-error :content) "boom"))
    (should (equal (plist-get response-error :content) "bad request"))
    (should (eq (plist-get bad-json :type) 'error))
    (should (string-match-p "Invalid Hermes transport JSON"
                            (plist-get bad-json :content)))))

(ert-deftest hermes-transport-normalizes-message-start-as-status ()
  (let ((event (hermes-transport-normalize-event
                '((event . "message.start")
                  (session_id . "sid-live")))))
    (should (eq (plist-get event :type) 'status))
    (should (equal (plist-get event :event) "message.start"))
    (should (equal (plist-get event :session-id) "sid-live"))
    (should (equal (plist-get event :status) "started"))
    (should (hermes-chat--active-status-p (plist-get event :status)))))

(ert-deftest hermes-transport-normalizes-message-start-underscore-as-status ()
  (let ((event (hermes-transport-normalize-event
                '((event . "message_start")
                  (session_id . "sid-live")))))
    (should (eq (plist-get event :type) 'status))
    (should (equal (plist-get event :event) "message_start"))
    (should (equal (plist-get event :session-id) "sid-live"))
    (should (equal (plist-get event :status) "started"))))

(ert-deftest hermes-transport-dashboard-normalizes-message-start-as-status ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.start")
                            (session_id . "sid-live")))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :event) "message.start"))
      (should (equal (plist-get event :session-id) "sid-live"))
      (should (equal (plist-get event :status) "started"))
      (should (hermes-chat--active-status-p (plist-get event :status))))))

(ert-deftest hermes-transport-dashboard-secret-list-tolerates-malformed ()
  "Secret collection filters strings and never aborts on a malformed slot."
  (should (equal '("a" "b")
                 (hermes-dashboard-transport--secret-list '("a" "" nil 7 "b"))))
  ;; An improper list is what `append' builds when the secrets slot holds a
  ;; bare string (e.g. a stale struct); it must not signal.
  (should (equal '("tok" "sid")
                 (hermes-dashboard-transport--secret-list '("tok" . "sid"))))
  (should (equal '("tok") (hermes-dashboard-transport--secret-list "tok")))
  (should-not (hermes-dashboard-transport--secret-list nil)))

(ert-deftest hermes-transport-dashboard-stop-tolerates-teardown-errors ()
  "Stop never throws and still closes resources when a teardown step errors."
  (let (closed deleted)
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (ws) (setq closed ws)))
              ((symbol-function 'delete-process)
               (lambda (p) (setq deleted p)))
              ((symbol-function 'hermes-dashboard-transport--reject-pending-requests)
               (lambda (&rest _) (error "boom"))))
      (let ((client (make-hermes-dashboard-transport-client
                     :process 'fake-process
                     :websocket 'fake-websocket
                     :pending (make-hash-table :test #'equal))))
        (should (hermes-dashboard-transport-stop client))
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-process client))))))

(ert-deftest hermes-transport-dashboard-parse-url-host-and-port ()
  "Dashboard URL parsing yields host and effective port."
  (should (equal '(:host "127.0.0.1" :port 9119)
                 (hermes-dashboard-transport--parse-url "http://127.0.0.1:9119")))
  (should (equal '(:host "example.test" :port 443)
                 (hermes-dashboard-transport--parse-url "https://example.test/hermes"))))

(ert-deftest hermes-transport-dashboard-url-drives-remote-attach ()
  "A non-loopback `hermes-dashboard-transport-url' attaches remotely."
  (let (opened-url)
    (let ((hermes-dashboard-transport-url "http://100.64.0.10:9119")
          (hermes-dashboard-transport-start-mode 'auto)
          (hermes-dashboard-transport-remote-auth-method 'token)
          (hermes-dashboard-transport-ready-timeout nil)
          (hermes-dashboard-transport-make-process-function
           (lambda (&rest _) (error "remote attach must not spawn")))
          (hermes-dashboard-transport-websocket-open-function
           (lambda (url _client) (setq opened-url url) 'fake-websocket)))
      (hermes-dashboard-transport-start :token "remote-token" :callback #'ignore)
      (should (equal opened-url
                     "ws://100.64.0.10:9119/api/ws?token=remote-token")))))

(ert-deftest hermes-transport-dashboard-message-complete-carries-usage ()
  "A `message.complete' event carries input/output token usage."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "message.complete")
                             (payload . ((status . "complete")
                                         (usage . ((input . 1200)
                                                   (output . 340)))))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (eq (plist-get event :type) 'done))
    (should (equal (plist-get event :usage) '(:input 1200 :output 340)))))

(ert-deftest hermes-transport-dashboard-message-complete-carries-warning ()
  "A `message.complete' event carries its history-desync warning."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "message.complete")
                             (payload . ((status . "complete")
                                         (warning . "not saved to history")))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (equal (plist-get event :warning) "not saved to history"))))

(ert-deftest hermes-transport-normalizes-terminal-read-request ()
  "A `terminal.read.request' becomes a prompt-request status event."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "terminal.read.request")
                             (session_id . "sid")
                             (payload . ((request_id . "req-tr")
                                         (start . 0)
                                         (count . 10)))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (eq (plist-get event :type) 'status))
    (should (plist-get event :prompt-request-p))
    (should (equal (plist-get event :prompt-type) "terminal"))
    (should (equal (plist-get event :status) "requested"))
    (should (equal (plist-get event :request-id) "req-tr"))
    (should (equal (plist-get event :start) 0))
    (should (equal (plist-get event :count) 10))))

(ert-deftest hermes-transport-normalizes-terminal-read-request-no-params ()
  "A `terminal.read.request' without start/count omits those fields."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "terminal.read.request")
                             (session_id . "sid")
                             (payload . ((request_id . "req-tr2")))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (eq (plist-get event :type) 'status))
    (should (plist-get event :prompt-request-p))
    (should (equal (plist-get event :prompt-type) "terminal"))
    (should (equal (plist-get event :request-id) "req-tr2"))
    (should-not (plist-member event :start))
    (should-not (plist-member event :count))))

(ert-deftest hermes-transport-dashboard-normalizes-notification-metadata ()
  "notification.show carries level/kind/ttl/key/id; warning levels prefix the text."
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0") (method . "event")
                 (params . ((type . "notification.show")
                            (session_id . "sid")
                            (payload . ((text . "credits low")
                                        (level . "warning")
                                        (kind . "credits")
                                        (ttl_ms . 5000)
                                        (key . "credits")
                                        (id . "n1"))))))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0") (method . "event")
                 (params . ((type . "notification.clear")
                            (session_id . "sid")
                            (payload . ((key . "credits")))))))))
    (let ((show (cadr events))
          (clear (car events)))
      (should (equal (plist-get show :content) "[warning] credits low"))
      (should (equal (plist-get show :level) "warning"))
      (should (equal (plist-get show :kind) "credits"))
      (should (equal (plist-get show :ttl-ms) 5000))
      (should (equal (plist-get show :notification-key) "credits"))
      (should (equal (plist-get show :notification-id) "n1"))
      (should (equal (plist-get clear :event) "notification.clear"))
      (should (equal (plist-get clear :notification-key) "credits")))))

(ert-deftest hermes-transport-dashboard-async-http-error-surfaces-detail ()
  "An url.el `http' status error rejects with the backend JSON detail.
url.el flags every 4xx/5xx via the callback status; the useless
\"peculiar error: N\" must not mask the body's detail message."
  (let (rejection)
    (with-temp-buffer
      (insert "HTTP/1.1 409 Conflict\r\n"
              "Content-Type: application/json\r\n\r\n"
              "{\"detail\": \"cannot reclaim t_x: not in a claimable state\"}")
      (let ((promise (hermes--promise-make)))
        (hermes-dashboard-transport--settle-http-response
         promise (list :error '(error http 409)) (current-buffer)
         "http://safe.test/api" nil)
        (hermes--promise-then promise #'ignore
                              (lambda (reason) (setq rejection reason)))))
    (should (string-match-p "not in a claimable state" rejection))
    (should (string-match-p "HTTP 409" rejection))
    (should-not (string-match-p "peculiar" rejection))))

(ert-deftest hermes-transport-dashboard-async-connection-error-still-rejects ()
  "A non-http url.el error keeps the direct rejection path."
  (let (rejection)
    (with-temp-buffer
      (let ((promise (hermes--promise-make)))
        (hermes-dashboard-transport--settle-http-response
         promise (list :error '(error connection-failed "refused"))
         (current-buffer) "http://safe.test/api" nil)
        (hermes--promise-then promise #'ignore
                              (lambda (reason) (setq rejection reason)))))
    (should (string-match-p "request failed at http://safe.test/api" rejection))))

(ert-deftest hermes-transport-dashboard-stop-cancels-cold-start-retry ()
  "A retry captured before stop cannot open another WebSocket afterward."
  (let (scheduled client (opens 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-connect-retries 2)
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _args) 'process))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (&rest _args)
               (cl-incf opens)
               (error "not ready")))
            (hermes-dashboard-transport-schedule-function
             (lambda (_delay fn &rest args)
               (setq scheduled (cons fn args)))))
        (setq client (hermes-dashboard-transport-start))
        (should scheduled)
        (hermes-dashboard-transport-stop client)
        (setf (hermes-dashboard-transport-client-stopping-p client) nil)
        (apply (car scheduled) (cdr scheduled))
        (should (= opens 1))
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-process client))))))

(ert-deftest hermes-transport-dashboard-stop-ignores-late-remote-auth ()
  "Remote authentication completing after stop cannot open or arm a socket."
  (let ((auth-promise (hermes--promise-make)) opened scheduled client)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-auth-async)
               (lambda (&rest _args) auth-promise)))
      (let ((hermes-dashboard-transport-ready-timeout 5)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (&rest _args) (setq opened t) 'websocket))
            (hermes-dashboard-transport-schedule-function
             (lambda (&rest args) (push args scheduled))))
        (setq client (hermes-dashboard-transport-start
                      :host "dash.example" :port 9119 :start-mode 'remote))
        (hermes-dashboard-transport-stop client)
        (hermes--promise-resolve
         auth-promise
         '(:token "token" :url "ws://dash.example/api/ws?token=token"
           :redacted-url "ws://dash.example/api/ws?token=<redacted>"
           :secrets ("token")))
        (should-not opened)
        (should-not scheduled)))))

(ert-deftest hermes-transport-dashboard-reconnect-exhaustion-finalizes-client ()
  "Exhausted reconnects reject readiness and release all owned resources."
  (let* ((ready (hermes--promise-make)) rejected request-rejected closed deleted
         (key '(spawn "127.0.0.1" 9119))
         (pending (make-hash-table :test #'equal))
         (client (make-hermes-dashboard-transport-client
                  :endpoint-key key :refcount 1 :reconnecting-p t
                  :ready-promise ready :websocket 'websocket :process 'process
                  :pending pending :callback #'ignore))
         (hermes-dashboard-transport-reconnect-max-attempts 1)
         (hermes-dashboard-transport-ready-timeout nil))
    (puthash "request"
             (list :method "prompt.submit"
                   :reject (lambda (reason) (setq request-rejected reason)))
             pending)
    (puthash key client hermes-dashboard-transport--clients)
    (hermes--promise-catch ready (lambda (reason) (setq rejected reason)))
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (socket) (setq closed socket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (hermes-dashboard-transport--reconnect-attempt client 1))
    (should rejected)
    (should request-rejected)
    (should (= (hash-table-count pending) 0))
    (should (eq closed 'websocket))
    (should (eq deleted 'process))
    (should-not (gethash key hermes-dashboard-transport--clients))))

(ert-deftest hermes-transport-dashboard-reconnect-abandonment-finalizes-client ()
  "A reconnect with no owners rejects readiness and releases its resources."
  (let* ((ready (hermes--promise-make)) rejected deleted
         (client (make-hermes-dashboard-transport-client
                  :refcount 0 :reconnecting-p t :ready-promise ready
                  :process 'process :pending (make-hash-table :test #'equal)
                  :callback #'ignore))
         (hermes-dashboard-transport-reconnect-max-attempts 3)
         (hermes-dashboard-transport-ready-timeout nil))
    (hermes--promise-catch ready (lambda (reason) (setq rejected reason)))
    (cl-letf (((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (hermes-dashboard-transport--reconnect-attempt client 0))
    (should rejected)
    (should (eq deleted 'process))))

(ert-deftest hermes-transport-dashboard-session-routing-keeps-surviving-owner ()
  "Removing one duplicate session owner never broadcasts to another session."
  (let* ((client (make-hermes-dashboard-transport-client :callback #'ignore))
         a1-events a2-events b-events
         (a1 (hermes-dashboard-transport-subscribe
              client (lambda (event) (push event a1-events))))
         (a2 (hermes-dashboard-transport-subscribe
              client (lambda (event) (push event a2-events))))
         (b (hermes-dashboard-transport-subscribe
             client (lambda (event) (push event b-events)))))
    (hermes-dashboard-transport-subscribe-session client a1 "A")
    (hermes-dashboard-transport-subscribe-session client a2 "A")
    (hermes-dashboard-transport-subscribe-session client b "B")
    (hermes-dashboard-transport-unsubscribe client a2)
    (hermes-dashboard-transport--dispatch-event client '(:session-id "A"))
    (should (= (length a1-events) 1))
    (should-not a2-events)
    (should-not b-events)
    (hermes-dashboard-transport-unsubscribe client a1)
    (hermes-dashboard-transport--dispatch-event client '(:session-id "A"))
    (should-not b-events)))

(ert-deftest hermes-transport-dashboard-spawn-key-includes-endpoint ()
  "Distinct loopback dashboard endpoints never share a spawned client key."
  (should-not
   (equal (hermes-dashboard-transport--endpoint-key
           :host "127.0.0.1" :port 9119 :start-mode 'spawn)
          (hermes-dashboard-transport--endpoint-key
           :host "127.0.0.1" :port 9229 :start-mode 'spawn))))

(provide 'hermes-transport-tests)
;;; hermes-transport-tests.el ends here
