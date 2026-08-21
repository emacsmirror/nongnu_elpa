;;; hermes-dashboard-api.el --- HTTP and URL layer for the Hermes dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; The HTTP/REST bottom of the dashboard stack, split out of
;; `hermes-dashboard-transport' (which requires this file): the dashboard
;; URL options, URL/WebSocket endpoint builders, secret redaction, the
;; promise-based url.el HTTP plumbing, the client struct (the data model;
;; the connection lifecycle stays in the transport), the REST API and its
;; auth, the profile/model caches, and remote credential resolution.
;; Symbol names keep the `hermes-dashboard-transport-' prefix so callers
;; were unaffected by the move.  Everything WebSocket- and process-shaped
;; -- spawn, connect, reconnect, JSON-RPC correlation, event dispatch --
;; belongs in `hermes-dashboard-transport', not here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-promise)

;;; Options

(define-obsolete-variable-alias 'hermes-dashboard-transport-remote-url
  'hermes-dashboard-transport-url "0.1.0")

(defcustom hermes-dashboard-transport-url "http://127.0.0.1:9119"
  "Address of the Hermes dashboard used for chat sessions.
One URL covers both local and remote dashboards:

- A loopback URL (the default `http://127.0.0.1:9119', the Hermes dashboard's
  standard port) makes `auto' start mode spawn a local dashboard bound to that
  host and port.
- A non-loopback URL makes `auto' attach to that already-running dashboard.
  `https://' and a reverse-proxy path prefix are supported, for example
  `https://example.test/hermes'.

Use `hermes-dashboard-transport-start-mode' to force spawn or remote attach."
  :type 'string
  :group 'hermes-dashboard-transport)

(defcustom hermes-instances nil
  "Named Hermes dashboard instances.
Each entry is (NAME . URL).  When nil, the existing
`hermes-dashboard-transport-url' is available as the instance named
\=`default'."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Dashboard URL"))
  :group 'hermes-dashboard-transport)

(defvar-local hermes-instance nil
  "Hermes instance owned by the current buffer, as (NAME . URL).")

(defun hermes-instance--valid-p (instance)
  "Return non-nil when INSTANCE is a valid (NAME . URL) pair."
  (and (consp instance)
       (stringp (car instance))
       (not (string-empty-p (string-trim (car instance))))
       (stringp (cdr instance))
       (not (string-empty-p (string-trim (cdr instance))))))

(defun hermes-instance-configured ()
  "Return configured Hermes instances as (NAME . URL) pairs.
Fall back to the existing dashboard URL as the instance named `default'."
  (let* ((instances (or hermes-instances
                        (list (cons "default"
                                    hermes-dashboard-transport-url))))
         (invalid (seq-find (lambda (instance)
                              (not (hermes-instance--valid-p instance)))
                            instances))
         (names (mapcar #'car instances)))
    (when invalid
      (user-error "Invalid Hermes instance: %S" invalid))
    (unless (= (length names) (length (delete-dups (copy-sequence names))))
      (user-error "Hermes instance names must be unique"))
    (copy-tree instances)))

(defun hermes-instance-name (instance)
  "Return INSTANCE's display name."
  (car instance))

(defun hermes-instance-url (instance)
  "Return INSTANCE's dashboard URL."
  (cdr instance))

(defun hermes-instance-multiple-p ()
  "Return non-nil when more than one Hermes instance is configured."
  (> (length (hermes-instance-configured)) 1))

(defun hermes-instance-context ()
  "Return the current unambiguous Hermes instance, or nil.
Unlike `hermes-instance-resolve', this never prompts."
  (let ((instances (hermes-instance-configured)))
    (if hermes-instances
        (or (and (hermes-instance--valid-p hermes-instance)
                 hermes-instance)
            (and (= (length instances) 1) (car instances)))
      (car instances))))

(defun hermes-instance-resolve ()
  "Return the Hermes instance for the current operation.
Use the current buffer's instance first.  Select the sole configured instance
without prompting, or prompt when multiple configured instances are available."
  (or (hermes-instance-context)
      (let ((instances (hermes-instance-configured)))
        (assoc (completing-read "Hermes instance: "
                                (mapcar #'car instances) nil t)
               instances))))

(defcustom hermes-dashboard-transport-http-timeout 30
  "Seconds before a dashboard REST/HTTP request gives up.
Bounds both the synchronous fallback and the asynchronous request path so a
slow or unreachable dashboard cannot hang a chat or list buffer forever."
  :type 'number
  :group 'hermes-dashboard-transport)

;;; URLs and WebSocket endpoints

(defun hermes-dashboard-transport--host-for-url (host)
  "Return HOST formatted for inclusion in a URL authority."
  (if (and (stringp host)
           (string-match-p ":" host)
           (not (string-prefix-p "[" host)))
      (format "[%s]" host)
    host))

(defun hermes-dashboard-transport--normalize-base-url (base-url)
  "Return normalized dashboard BASE-URL, or nil when BASE-URL is empty."
  (and-let* ((url (and (stringp base-url) (string-trim base-url)))
             ((not (string-empty-p url))))
    (let* ((parsed (url-generic-parse-url url))
           (path-and-query (url-path-and-query parsed)))
      (unless (member (url-type parsed) '("http" "https"))
        (user-error "Hermes remote dashboard URL must start with http:// or https://"))
      (unless (url-host parsed)
        (user-error "Hermes remote dashboard URL must include a host"))
      (when (or (url-user parsed) (url-password parsed))
        (user-error "Hermes remote dashboard URL must not include username or password"))
      (when (or (cdr path-and-query) (url-target parsed))
        (user-error "Hermes remote dashboard URL must not include query string or fragment")))
    (replace-regexp-in-string "/+\\'" "" url)))

(defun hermes-dashboard-transport--base-url (host port &optional remote-url)
  "Return dashboard HTTP base URL from HOST, PORT, or REMOTE-URL."
  (or (hermes-dashboard-transport--normalize-base-url remote-url)
      (progn
        (unless port
          (user-error
           "Set `hermes-dashboard-transport-url' for remote dashboard attach"))
        (format "http://%s:%d"
                (hermes-dashboard-transport--host-for-url host) port))))

(defun hermes-dashboard-transport--api-url (base-url path)
  "Return dashboard API URL by appending PATH to BASE-URL."
  (concat (hermes-dashboard-transport--normalize-base-url base-url) path))

(defun hermes-dashboard-transport--parse-url (url)
  "Return a plist of :host and :port parsed from dashboard URL."
  (let ((normalized (hermes-dashboard-transport--normalize-base-url url)))
    (unless normalized
      (user-error "Set `hermes-dashboard-transport-url' to an http(s) dashboard URL"))
    (let ((parsed (url-generic-parse-url normalized)))
      (list :host (url-host parsed) :port (url-port parsed)))))

(defun hermes-dashboard-transport--websocket-endpoint-for
    (host port path &optional remote-url)
  "Return the dashboard WebSocket endpoint for PATH from HOST, PORT, or REMOTE-URL."
  (let ((base-url (hermes-dashboard-transport--base-url host port remote-url)))
    (concat (cond
             ((string-prefix-p "https://" base-url)
              (concat "wss://" (substring base-url 8)))
             ((string-prefix-p "http://" base-url)
              (concat "ws://" (substring base-url 7)))
             (t (user-error "Hermes remote dashboard URL must use http or https")))
            path)))

(defun hermes-dashboard-transport--websocket-endpoint
    (host port &optional remote-url)
  "Return the JSON-RPC dashboard WebSocket endpoint from HOST, PORT, or REMOTE-URL."
  (hermes-dashboard-transport--websocket-endpoint-for
   host port "/api/ws" remote-url))

(defun hermes-dashboard-transport--websocket-url
    (host port secret &optional remote-url query-param)
  "Return authenticated dashboard WebSocket URL for SECRET.
HOST and PORT derive the default base URL.  REMOTE-URL overrides that base.
QUERY-PARAM defaults to `token'."
  (format "%s?%s=%s"
          (hermes-dashboard-transport--websocket-endpoint host port remote-url)
          (or query-param "token")
          (url-hexify-string secret)))

(defun hermes-dashboard-transport--redacted-websocket-url
    (host port &optional remote-url query-param)
  "Return a safe WebSocket URL for HOST, PORT, REMOTE-URL, and QUERY-PARAM."
  (format "%s?%s=<redacted>"
          (hermes-dashboard-transport--websocket-endpoint host port remote-url)
          (or query-param "token")))

(defconst hermes-dashboard-transport--kanban-events-path
  "/api/plugins/kanban/events"
  "Dashboard WebSocket path for the kanban live-events tail.")

(defun hermes-dashboard-transport--swap-websocket-path (url path)
  "Return URL with its `/api/ws' endpoint segment replaced by PATH.
The credential query string after the endpoint is preserved untouched, so the
token or ticket is never reconstructed."
  (replace-regexp-in-string "/api/ws\\(\\?\\|\\'\\)"
                            (concat path "\\1") url t))

(defun hermes-dashboard-transport--query-pair (key value)
  "Return KEY=VALUE percent-encoded for a URL query string."
  (format "%s=%s"
          (url-hexify-string (format "%s" key))
          (url-hexify-string (format "%s" value))))

(defun hermes-dashboard-transport--append-url-query (url params)
  "Return URL with PARAMS, an alist, appended as `&key=value' query pairs.
Pairs whose value is nil are dropped; keys and values are percent-encoded."
  (concat url
          (mapconcat
           (lambda (kv)
             (concat "&" (hermes-dashboard-transport--query-pair
                          (car kv) (cdr kv))))
           (seq-filter #'cdr params) "")))

;;; Secret redaction

(defun hermes-dashboard-transport--secret-list (secrets)
  "Return the non-empty string secrets contained in SECRETS.
SECRETS may be a proper list, an improper list, or a single value, so a
malformed slot never aborts the teardown-path redaction this guards."
  (let (result)
    (while (consp secrets)
      (let ((secret (car secrets)))
        (when (and (stringp secret) (not (string-empty-p secret)))
          (push secret result)))
      (setq secrets (cdr secrets)))
    (when (and (stringp secrets) (not (string-empty-p secrets)))
      (push secrets result))
    (nreverse result)))

(defun hermes-dashboard-transport--redact-secret (text &optional secrets)
  "Return TEXT with dashboard URL credentials and SECRETS redacted.
Each secret is also matched in its JSON-escaped spelling, since request
bodies travel JSON-serialized and an error may echo the escaped form."
  (let ((message (if (stringp text) text (format "%s" text))))
    (setq message
          (replace-regexp-in-string
           "\\([?&]\\(?:token\\|ticket\\|internal\\)=\\)[^&[:space:])\"']+"
           "\\1<redacted>" message t nil))
    (setq message
          (replace-regexp-in-string
           "\\(HERMES_DASHBOARD_SESSION_TOKEN=\\)[^[:space:])\"']+"
           "\\1<redacted>" message t nil))
    (dolist (secret (hermes-dashboard-transport--secret-list secrets))
      (setq message (string-replace secret "<redacted>" message))
      (let ((escaped (substring (json-encode secret) 1 -1)))
        (unless (equal escaped secret)
          (setq message (string-replace escaped "<redacted>" message)))))
    message))


;;; HTTP requests

(defun hermes-dashboard-transport--json-body (text)
  "Return JSON object parsed from TEXT, or nil for an empty body."
  (unless (string-empty-p (string-trim (or text "")))
    (hermes-transport-json-parse text)))

(defun hermes-dashboard-transport--json-error-message (value)
  "Return a human-facing error message extracted from JSON VALUE."
  (cond
   ((hermes-transport--scalar-string value))
   ((hermes-transport--object-p value)
    (hermes-dashboard-transport--json-error-message
     (hermes-transport--get-any value '(detail message error msg title))))
   ((consp value)
    (when-let* ((messages (delq nil
                                (mapcar #'hermes-dashboard-transport--json-error-message
                                        value))))
      (string-join messages "; ")))))

(defun hermes-dashboard-transport--http-error-detail (body-text)
  "Return backend error detail parsed from HTTP BODY-TEXT, or nil."
  (when-let* ((body (condition-case nil
                        (hermes-dashboard-transport--json-body body-text)
                      (error nil)))
              (message (hermes-dashboard-transport--json-error-message body))
              (trimmed (string-trim message)))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun hermes-dashboard-transport--parse-http-response-buffer (buffer)
  "Return plist parsed from url.el response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                       (string-to-number (match-string 1))))
          headers header-end body-start body)
      (if (re-search-forward "\r?\n\r?\n" nil t)
          (setq header-end (match-beginning 0)
                body-start (point))
        (setq header-end (point-max)
              body-start (point-max)))
      (dolist (line (split-string
                     (buffer-substring-no-properties (point-min) header-end)
                     "\r?\n" t))
        (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
          (push (cons (downcase (match-string 1 line))
                      (match-string 2 line))
                headers)))
      (setq body (buffer-substring-no-properties body-start (point-max)))
      (list :status status
            :headers (nreverse headers)
            :body-text body))))

(defun hermes-dashboard-transport--http-result (response safe-url secrets)
  "Interpret parsed RESPONSE from SAFE-URL, redacting SECRETS in errors.
Return (ok . RESPONSE) with the JSON `:body' filled in on a 2xx status, or
\(error . MESSAGE) otherwise."
  (let ((status (plist-get response :status)))
    (if (and status (<= 200 status 299))
        (condition-case err
            (cons 'ok (plist-put response :body
                                 (hermes-dashboard-transport--json-body
                                  (plist-get response :body-text))))
          (error
           (cons 'error
                 (format "Hermes dashboard returned a non-JSON body at %s (HTTP %s): %s"
                         safe-url status
                         (hermes-dashboard-transport--redact-secret
                          (error-message-string err) secrets)))))
      (let ((detail (hermes-dashboard-transport--http-error-detail
                     (plist-get response :body-text))))
        (cons 'error
              (if detail
                  (format "Hermes dashboard request failed at %s (HTTP %s): %s"
                          safe-url (or status "unknown")
                          (hermes-dashboard-transport--redact-secret
                           detail secrets))
                (format "Hermes dashboard request failed at %s (HTTP %s)"
                        safe-url (or status "unknown"))))))))

(defun hermes-dashboard-transport--settle-http-response
    (promise status buffer safe-url secrets)
  "Settle PROMISE from url.el STATUS and response BUFFER for SAFE-URL.
SECRETS are redacted from any error message.  An `http' STATUS error is
NOT taken at face value: url.el reports every 4xx/5xx that way (its
`error-message-string' is the useless \"peculiar error: N\"), while the
response buffer still holds the backend's JSON error detail -- so those
fall through to the body parser and reject with the real message."
  (if-let* ((error-data (plist-get status :error))
            ((not (eq (nth 1 error-data) 'http))))
      (hermes--promise-reject
       promise (format "Hermes dashboard request failed at %s: %s"
                       safe-url
                       (hermes-dashboard-transport--redact-secret
                        (error-message-string error-data) secrets)))
    (pcase (hermes-dashboard-transport--http-result
            (hermes-dashboard-transport--parse-http-response-buffer buffer)
            safe-url secrets)
      (`(ok . ,response) (hermes--promise-resolve promise response))
      (`(error . ,message) (hermes--promise-reject promise message)))))

(cl-defun hermes-dashboard-transport--default-http-request
    (url &key (method "GET") headers data secrets)
  "Fetch URL with METHOD, HEADERS, and DATA using url.el synchronously.
SECRETS are redacted from any user-visible error.
Legacy: new callers must use
`hermes-dashboard-transport--default-http-request-async'; synchronous
network on the main thread is banned by AGENTS.md."
  (let ((safe-url (hermes-dashboard-transport--redact-secret url secrets))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data))
    (let ((buffer (url-retrieve-synchronously
                   url t t hermes-dashboard-transport-http-timeout)))
      (unless buffer
        (user-error "Hermes dashboard request failed at %s" safe-url))
      (unwind-protect
          (pcase (hermes-dashboard-transport--http-result
                  (hermes-dashboard-transport--parse-http-response-buffer buffer)
                  safe-url secrets)
            (`(ok . ,response) response)
            (`(error . ,message) (user-error "%s" message)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun hermes-dashboard-transport--close-http-buffer (buffer)
  "Quietly close BUFFER and its live process."
  (when (buffer-live-p buffer)
    (when-let* ((process (ignore-errors (get-buffer-process buffer))))
      (when (ignore-errors (process-live-p process))
        (ignore-errors (delete-process process))))
    (ignore-errors (kill-buffer buffer))))

(cl-defun hermes-dashboard-transport--default-http-request-async
    (url &key (method "GET") headers data secrets timeout
         cancel-setter cancel-expected)
  "Fetch URL with METHOD, HEADERS, and DATA asynchronously using url.el.
Return a promise of the response plist; SECRETS are redacted from any error.
TIMEOUT overrides `hermes-dashboard-transport-http-timeout' when non-nil.
CANCEL-SETTER replaces CANCEL-EXPECTED with this request's cancellation owner."
  (let ((safe-url (hermes-dashboard-transport--redact-secret url secrets))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data)
        (request-timeout (or timeout hermes-dashboard-transport-http-timeout))
        (promise (hermes--promise-make))
        timer request-buffer cancel registered settled)
    (cl-labels
        ((release-owner ()
           (when timer
             (ignore-errors (cancel-timer timer))
             (setq timer nil))
           (when (and registered cancel-setter)
             (setq registered nil)
             (ignore-errors (funcall cancel-setter cancel nil))))
         (reject (reason)
           (unless settled
             (setq settled t)
             (release-owner)
             (hermes-dashboard-transport--close-http-buffer request-buffer)
             (hermes--promise-reject promise reason))))
      (setq cancel
            (lambda ()
              (reject "Hermes dashboard request was superseded")))
      (condition-case err
          (progn
            (when cancel-setter
              (unless (funcall cancel-setter cancel-expected cancel)
                (user-error "Hermes dashboard request was superseded"))
              (setq registered t))
            (setq timer
                  (run-at-time
                   request-timeout nil
                   (lambda ()
                     (reject
                      (format "Hermes dashboard request timed out at %s"
                              safe-url)))))
            (setq request-buffer
                  (url-retrieve
                   url
                   (lambda (status)
                     (let ((buffer (current-buffer)))
                       (if settled
                           (hermes-dashboard-transport--close-http-buffer buffer)
                         (setq settled t)
                         (release-owner)
                         (unwind-protect
                             (condition-case response-error
                                 (hermes-dashboard-transport--settle-http-response
                                  promise status buffer safe-url secrets)
                               (error
                                (hermes--promise-reject
                                 promise
                                 (format
                                  "Hermes dashboard response error at %s: %s"
                                  safe-url
                                  (hermes-dashboard-transport--redact-secret
                                   (error-message-string response-error)
                                   secrets)))))
                           (hermes-dashboard-transport--close-http-buffer
                            buffer)))))
                   nil t t))
            (when settled
              (hermes-dashboard-transport--close-http-buffer request-buffer)))
        (error
         (reject (hermes-dashboard-transport--redact-secret
                  (error-message-string err) secrets))))
      promise)))

(defvar hermes-dashboard-transport-http-request-function
  #'hermes-dashboard-transport--default-http-request
  "Function used for remote dashboard HTTP requests.
It is called with URL and keyword arguments :method, :headers, :data, and
:secrets, and returns a plist with :status, :headers, and :body.")

(defvar hermes-dashboard-transport-http-request-async-function
  #'hermes-dashboard-transport--default-http-request-async
  "Function used for asynchronous remote dashboard HTTP requests.
Called with URL and keyword arguments :method, :headers, :data, and :secrets.
Caller-specific overrides add :timeout or the paired :cancel-setter and
:cancel-expected keywords.  The function returns a promise of the response
plist.")

(cl-defun hermes-dashboard-transport--http-json
    (url &key (method "GET") headers body secrets)
  "Request URL as JSON using METHOD, HEADERS, BODY, and SECRETS."
  (funcall hermes-dashboard-transport-http-request-function
           url
           :method method
           :headers (append '(("Accept" . "application/json")) headers)
           :data (and body (json-serialize body))
           :secrets secrets))

(cl-defun hermes-dashboard-transport--http-json-async
    (url &key (method "GET") headers body secrets timeout
         cancel-setter cancel-expected)
  "Request URL as JSON asynchronously using METHOD, HEADERS, BODY, and SECRETS.
Return a promise of the response plist.  TIMEOUT overrides the default.
CANCEL-SETTER replaces CANCEL-EXPECTED while this request owns its slot."
  (apply hermes-dashboard-transport-http-request-async-function
         url
         (append
          (list :method method
                :headers (append '(("Accept" . "application/json")) headers)
                :data (and body (json-serialize body))
                :secrets secrets)
          (and timeout (list :timeout timeout))
          (and cancel-setter
               (list :cancel-setter cancel-setter
                     :cancel-expected cancel-expected)))))

(defun hermes-dashboard-transport--http-json-request (request)
  "Send REQUEST, a (:url :method :headers :body :secrets) plist, synchronously."
  (hermes-dashboard-transport--http-json
   (plist-get request :url)
   :method (plist-get request :method)
   :headers (plist-get request :headers)
   :body (plist-get request :body)
   :secrets (plist-get request :secrets)))

(defun hermes-dashboard-transport--http-json-request-async (request)
  "Send REQUEST, a REST request plist, asynchronously.
Return a promise of the response plist."
  (let ((timeout (plist-get request :timeout)))
    (apply #'hermes-dashboard-transport--http-json-async
           (plist-get request :url)
           (append
            (list :method (plist-get request :method)
                  :headers (plist-get request :headers)
                  :body (plist-get request :body)
                  :secrets (plist-get request :secrets))
            (and timeout (list :timeout timeout))))))




;;; Client struct

(cl-defstruct hermes-dashboard-transport-client
  "State for one dashboard/TUI JSON-RPC WebSocket connection."
  process
  websocket
  (host "127.0.0.1")
  port
  token
  base-url
  auth-method
  auth-token
  credential-kind
  credential-reusable-p
  websocket-url
  redacted-websocket-url
  secrets
  ready-p
  ready-promise
  (next-id 0)
  (pending (make-hash-table :test #'equal))
  ;; Deprecated: session identity is buffer-local.  These slots are retained
  ;; for incremental source compatibility only; chat and control paths no
  ;; longer read or write them.  Do not add new dependencies on them.
  session-id
  stored-session-id
  (callback #'ignore)
  (subscribers (make-hash-table :test #'eq))
  (session-index (make-hash-table :test #'equal))
  (refcount 0)
  endpoint-key
  idle-timer
  heartbeat-timer
  stopping-p
  reconnecting-p
  (reconnect-attempts 0)
  ;; Bumped by `hermes-dashboard-transport-stop' and manual reconnect so
  ;; scheduled reconnect attempts and ready timeouts armed for an earlier
  ;; connection lifetime become no-ops instead of racing the replacement.
  (generation 0)
  startup-cancel)

(defcustom hermes-dashboard-transport-remote-auth-method 'auto
  "Authentication method for remote dashboard attach.
`auto' probes /api/status.  An ungated dashboard uses a legacy session token.
A gated dashboard prefers RFC 8252 native PKCE when `/api/status' advertises
`native_pkce', otherwise username/password login with a WebSocket ticket when a
basic provider is available.  `token' forces the legacy /api/ws?token= path.
`basic' forces username/password login and a single-use WebSocket ticket.
`native' forces the cookieless native PKCE attach path."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Legacy session token" token)
                 (const :tag "Basic/password gated auth" basic)
                 (const :tag "Native PKCE OAuth" native))
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-native-login-timeout 300
  "Seconds to wait for the system-browser native PKCE loopback callback."
  :type 'number
  :group 'hermes-dashboard-transport)

(defconst hermes-dashboard-transport--native-auth-user
  "hermes-dashboard-native"
  "Auth-source login used for native dashboard OAuth tokens.")

(defconst hermes-dashboard-transport--native-auth-port
  "hermes-dashboard-native"
  "Auth-source port used for native dashboard OAuth tokens.")

(defconst hermes-dashboard-transport--native-done-html
  (concat
   "<!doctype html><meta charset=\"utf-8\"><title>Signed in</title>"
   "<body style=\"font:15px system-ui;margin:3rem;text-align:center\">"
   "<h2>Signed in to Hermes</h2>"
   "<p>You can close this window and return to Emacs.</p>")
  "HTML served on the native PKCE loopback after the browser returns.")

(defvar hermes-dashboard-transport-browse-url-function #'browse-url
  "Function used to open the native PKCE authorize URL.
Called with one URL string argument.")

(defun hermes-dashboard-transport--browse-url (url)
  "Open URL with `hermes-dashboard-transport-browse-url-function'."
  (funcall hermes-dashboard-transport-browse-url-function url))

(defun hermes-dashboard-transport--random-bytes (n)
  "Return N cryptographically strong random bytes.
Linux Emacs cannot seek `/dev/urandom`, so this prefers `openssl rand` and
falls back to `head -c` from `/dev/urandom`.  Never uses Emacs `random`."
  (unless (and (integerp n) (> n 0))
    (error "Random byte count must be a positive integer"))
  (let ((bytes
         (or (hermes-dashboard-transport--random-bytes-command
              n "openssl" nil "rand" (number-to-string n))
             (hermes-dashboard-transport--random-bytes-command
              n "head" "/dev/urandom" "-c" (number-to-string n)))))
    (unless (and (stringp bytes) (= (length bytes) n))
      (error "Failed to read %d cryptographically strong random bytes" n))
    bytes))

(defun hermes-dashboard-transport--random-bytes-command
    (n program infile &rest args)
  "Return N bytes from PROGRAM with ARGS, or nil on failure.
INFILE is the optional stdin file for PROGRAM."
  (when (executable-find program)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let* ((coding-system-for-read 'binary)
             (coding-system-for-write 'binary)
             (status (apply #'call-process program infile t nil args)))
        (and (eq status 0)
             (= (buffer-size) n)
             (buffer-string))))))

(defun hermes-dashboard-transport--base64url-no-pad (raw)
  "Return base64url encoding of RAW without padding."
  (let ((encoded (base64-encode-string raw t)))
    (setq encoded (replace-regexp-in-string "\\+" "-" encoded t t))
    (setq encoded (replace-regexp-in-string "/" "_" encoded t t))
    (replace-regexp-in-string "=+\\'" "" encoded t t)))

(defun hermes-dashboard-transport--pkce-pair ()
  "Return a native PKCE plist with :verifier, :challenge, and :method."
  (let* ((verifier (hermes-dashboard-transport--base64url-no-pad
                    (hermes-dashboard-transport--random-bytes 32)))
         (challenge (hermes-dashboard-transport--base64url-no-pad
                     (secure-hash 'sha256 verifier nil nil t))))
    (list :verifier verifier :challenge challenge :method "S256")))

(defun hermes-dashboard-transport--native-state ()
  "Return a high-entropy CSRF state for native PKCE."
  (hermes-dashboard-transport--base64url-no-pad
   (hermes-dashboard-transport--random-bytes 24)))

(defun hermes-dashboard-transport--status-auth-flows (status)
  "Return auth flow names from STATUS as strings."
  (mapcar #'hermes-transport--scalar-string
          (or (hermes-transport--get status 'auth_flows) '())))

(defun hermes-dashboard-transport--status-supports-native-pkce-p (status)
  "Return non-nil when STATUS advertises the native_pkce auth flow."
  (member "native_pkce"
          (hermes-dashboard-transport--status-auth-flows status)))

(defun hermes-dashboard-transport--status-oauth-provider (status)
  "Return the first non-basic auth provider name from STATUS, or nil."
  (cl-find-if (lambda (name)
                (and (stringp name)
                     (not (string-empty-p name))
                     (not (equal name "basic"))))
              (hermes-dashboard-transport--status-auth-providers status)))

(defun hermes-dashboard-transport--native-token-plist (body &optional previous)
  "Return a native token plist parsed from JSON BODY, or nil.
When BODY omits refresh_token, keep PREVIOUS's refresh token if present."
  (let ((access (hermes-transport--scalar-string
                 (hermes-transport--get body 'access_token)))
        (refresh (hermes-transport--scalar-string
                  (hermes-transport--get body 'refresh_token)))
        (expires (hermes-transport--get body 'expires_at))
        (provider (or (hermes-transport--scalar-string
                       (hermes-transport--get body 'provider))
                      (plist-get previous :provider)
                      ""))
        (user-id (or (hermes-transport--scalar-string
                      (hermes-transport--get body 'user_id))
                     (plist-get previous :user-id)
                     ""))
        (prior-refresh (plist-get previous :refresh-token)))
    (when (and access (not (string-empty-p access)))
      (list :access-token access
            :refresh-token (if (and (stringp refresh)
                                    (not (string-empty-p refresh)))
                               refresh
                             (or prior-refresh ""))
            :expires-at (if (numberp expires) expires 0)
            :provider provider
            :user-id user-id))))

(defun hermes-dashboard-transport--native-token-encode (tokens)
  "Return the auth-source secret string for TOKENS."
  (json-serialize
   `((access_token . ,(plist-get tokens :access-token))
     (refresh_token . ,(plist-get tokens :refresh-token))
     (expires_at . ,(or (plist-get tokens :expires-at) 0))
     (provider . ,(or (plist-get tokens :provider) ""))
     (user_id . ,(or (plist-get tokens :user-id) "")))))

(defun hermes-dashboard-transport--native-token-decode (secret)
  "Return a native token plist decoded from SECRET, or nil."
  (when (and (stringp secret) (not (string-empty-p secret)))
    (condition-case nil
        (hermes-dashboard-transport--native-token-plist
         (hermes-transport-json-parse secret))
      (error nil))))

(defvar hermes-dashboard-transport--native-token-memory
  (make-hash-table :test #'equal)
  "Process-local native token cache keyed by normalized base URL.")

(defun hermes-dashboard-transport--native-token-memory-key (base-url)
  "Return the memory-cache key for BASE-URL."
  (or (hermes-dashboard-transport--normalize-base-url base-url) base-url))

(defun hermes-dashboard-transport--native-token-netrc-files ()
  "Return file-backed netrc/authinfo sources configured in `auth-sources'."
  (delq nil
        (mapcar
         (lambda (source)
           (condition-case nil
               (let ((backend (auth-source-backend-parse source)))
                 (when (and backend
                            (eq (slot-value backend 'type) 'netrc))
                   (let ((file (slot-value backend 'source)))
                     (and (stringp file) (expand-file-name file)))))
             (error nil)))
         auth-sources)))

(defun hermes-dashboard-transport--native-token-netrc-line-entry (line file)
  "Return the auth-source entry represented by netrc LINE from FILE."
  (condition-case nil
      (with-temp-buffer
        (insert line "\n")
        (goto-char (point-min))
        (car (auth-source-netrc-normalize
              (auth-source-netrc-parse-entries (lambda (_entry) t) 1)
              file)))
    (error nil)))

(defun hermes-dashboard-transport--native-token-netrc-entry-p (entry hosts)
  "Return non-nil when ENTRY is this client's native token for HOSTS."
  (and (member (plist-get entry :host) hosts)
       (equal (plist-get entry :user)
              hermes-dashboard-transport--native-auth-user)
       (equal (plist-get entry :port)
              hermes-dashboard-transport--native-auth-port)))

(defun hermes-dashboard-transport--native-token-delete-netrc-file (file hosts)
  "Delete this client's one-line native token entries for HOSTS from FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((changed nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((start (point))
                 (end (line-beginning-position 2))
                 (entry
                  (hermes-dashboard-transport--native-token-netrc-line-entry
                   (buffer-substring-no-properties start end) file)))
            (if (hermes-dashboard-transport--native-token-netrc-entry-p
                 entry hosts)
                (progn
                  (delete-region start end)
                  (setq changed t))
              (goto-char end))))
        (when changed
          (when auth-source-gpg-encrypt-to
            (make-local-variable 'epa-file-encrypt-to)
            (when (listp auth-source-gpg-encrypt-to)
              (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))
          (write-region (point-min) (point-max) file nil 'silent))))))

(defun hermes-dashboard-transport--native-token-delete-disk (base-url)
  "Delete auth-source native token entries for BASE-URL."
  (let ((hosts (hermes-dashboard-transport--auth-source-hosts base-url)))
    (when (fboundp 'auth-source-forget-all-cached)
      (auth-source-forget-all-cached))
    (dolist (host hosts)
      (ignore-errors
        (auth-source-delete
         :host host
         :user hermes-dashboard-transport--native-auth-user
         :port hermes-dashboard-transport--native-auth-port)))
    ;; The netrc backend documents that `auth-source-delete' is search-only.
    ;; Remove the one-line records produced by its own save function directly.
    (dolist (file (hermes-dashboard-transport--native-token-netrc-files))
      (ignore-errors
        (hermes-dashboard-transport--native-token-delete-netrc-file
         file hosts))))
  (when (fboundp 'auth-source-forget-all-cached)
    (auth-source-forget-all-cached)))

(defun hermes-dashboard-transport--native-token-write-disk (base-url tokens)
  "Write TOKENS for BASE-URL to auth-source and return non-nil on success."
  (when (fboundp 'auth-source-forget-all-cached)
    (auth-source-forget-all-cached))
  (let* ((secret (hermes-dashboard-transport--native-token-encode tokens))
         (auth-source-creation-defaults
          `((user . ,hermes-dashboard-transport--native-auth-user)
            (port . ,hermes-dashboard-transport--native-auth-port)
            (secret . ,secret)))
         (host (car (hermes-dashboard-transport--auth-source-hosts base-url)))
         (entry (car (and host
                          (auth-source-search
                           :host host
                           :user hermes-dashboard-transport--native-auth-user
                           :port hermes-dashboard-transport--native-auth-port
                           :max 1
                           :create t
                           :require '(:secret))))))
    (when-let* ((save (and entry (plist-get entry :save-function))))
      (funcall save))
    (when (fboundp 'auth-source-forget-all-cached)
      (auth-source-forget-all-cached))
    (let ((loaded (hermes-dashboard-transport--native-token-load-disk base-url)))
      (and loaded
           (equal (plist-get loaded :access-token)
                  (plist-get tokens :access-token))
           (equal (plist-get loaded :refresh-token)
                  (plist-get tokens :refresh-token))))))

(defun hermes-dashboard-transport--native-token-load-disk (base-url)
  "Return native tokens for BASE-URL from auth-source only, or nil."
  (when-let* ((entry (hermes-dashboard-transport--auth-source-entry
                      base-url
                      :user hermes-dashboard-transport--native-auth-user
                      :port hermes-dashboard-transport--native-auth-port
                      :require '(:secret)))
              (secret (hermes-dashboard-transport--auth-source-secret entry)))
    (hermes-dashboard-transport--native-token-decode secret)))

(defun hermes-dashboard-transport--native-token-load (base-url)
  "Return stored native tokens for BASE-URL, or nil."
  (let ((key (hermes-dashboard-transport--native-token-memory-key base-url)))
    (or (gethash key hermes-dashboard-transport--native-token-memory)
        (when-let* ((disk (hermes-dashboard-transport--native-token-load-disk
                           base-url)))
          (puthash key disk hermes-dashboard-transport--native-token-memory)
          disk))))

(defun hermes-dashboard-transport--native-token-store (base-url tokens)
  "Persist TOKENS for BASE-URL, or delete the stored entry when TOKENS is nil.
Failed writes restore any previously stored tokens."
  (hermes-dashboard-transport--require-auth-source)
  (let* ((key (hermes-dashboard-transport--native-token-memory-key base-url))
         (prior-memory
          (gethash key hermes-dashboard-transport--native-token-memory))
         (prior-disk
          (hermes-dashboard-transport--native-token-load-disk base-url))
         (prior (or prior-memory prior-disk)))
    (cond
     ((null tokens)
      (remhash key hermes-dashboard-transport--native-token-memory)
      (hermes-dashboard-transport--native-token-delete-disk base-url)
      nil)
     (t
      (puthash key tokens hermes-dashboard-transport--native-token-memory)
      (condition-case err
          (progn
            ;; Replace disk only after the new secret is in hand.  Delete then
            ;; write, and restore PRIOR on any failure so durable state cannot
            ;; disappear because create/save failed mid-update.
            (hermes-dashboard-transport--native-token-delete-disk base-url)
            (unless (hermes-dashboard-transport--native-token-write-disk
                     base-url tokens)
              (error "Native token store verification failed"))
            tokens)
        (error
         (if prior
             (progn
               (puthash key prior
                        hermes-dashboard-transport--native-token-memory)
               (ignore-errors
                 (hermes-dashboard-transport--native-token-delete-disk base-url)
                 (hermes-dashboard-transport--native-token-write-disk
                  base-url prior)))
           (remhash key hermes-dashboard-transport--native-token-memory)
           (ignore-errors
             (hermes-dashboard-transport--native-token-delete-disk base-url)))
         (signal (car err) (cdr err))))))))

(defun hermes-dashboard-transport--native-token-needs-refresh-p
    (tokens &optional now skew)
  "Return non-nil when TOKENS should be refreshed.
NOW defaults to the current unix time; SKEW defaults to 60 seconds."
  (let* ((expires (plist-get tokens :expires-at))
         (now (or now (time-convert nil 'integer)))
         (skew (or skew 60)))
    (or (not (numberp expires))
        (<= expires 0)
        (>= now (- expires skew)))))

(defun hermes-dashboard-transport--native-authorize-url
    (base-url challenge redirect-uri state &optional provider)
  "Return the native authorize URL for BASE-URL.
CHALLENGE, REDIRECT-URI, STATE, and optional PROVIDER become query parameters."
  (let ((query `((code_challenge . ,challenge)
                 (code_challenge_method . "S256")
                 (redirect_uri . ,redirect-uri)
                 (state . ,state))))
    (when (and (stringp provider) (not (string-empty-p provider)))
      (push (cons 'provider provider) query))
    (concat (hermes-dashboard-transport--api-url
             base-url "/auth/native/authorize")
            (hermes-dashboard-transport--query-string (nreverse query)))))

(defun hermes-dashboard-transport--query-alist (query)
  "Return QUERY string as an alist of decoded (KEY . VALUE) pairs.
Signals when any key appears more than once."
  (let ((params (url-parse-query-string (or query "") nil t))
        (seen (make-hash-table :test #'equal))
        out)
    (dolist (pair params)
      (let ((key (car pair))
            (values (cdr pair)))
        (when (or (gethash key seen)
                  (/= (length values) 1))
          (user-error "Loopback callback has duplicate or empty parameters"))
        (puthash key t seen)
        (push (cons key (car values)) out)))
    (nreverse out)))

(defun hermes-dashboard-transport--native-parse-loopback (request expected-state)
  "Parse loopback REQUEST path/query and return (:code CODE).
Only a single GET /callback is accepted.  EXPECTED-STATE is validated before
honoring code or OAuth error parameters."
  (let* ((line (car (split-string request "\r\n" t)))
         (parts (and (stringp line) (split-string line " " t)))
         (method (nth 0 parts))
         (target (nth 1 parts))
         (version (nth 2 parts))
         (path (and (stringp target) (car (split-string target "?"))))
         (query (and (stringp target) (cadr (split-string target "?"))))
         (params (hermes-dashboard-transport--query-alist query))
         (error (cdr (assoc "error" params)))
         (code (cdr (assoc "code" params)))
         (state (cdr (assoc "state" params))))
    (unless (and (equal method "GET")
                 (equal path "/callback")
                 (stringp version)
                 (string-match-p "\\`HTTP/1\\.[01]\\'" version)
                 (= (length parts) 3))
      (user-error "Loopback callback is not a GET /callback request"))
    (unless (and (stringp expected-state)
                 (stringp state)
                 (equal state expected-state))
      (user-error "Loopback callback state mismatch (possible CSRF)"))
    (when error
      (user-error "Gateway rejected native login"))
    (unless (and (stringp code) (not (string-empty-p code)))
      (user-error "Loopback callback missing authorization code"))
    (when (and error code)
      (user-error "Loopback callback is ambiguous"))
    (list :code code)))

(defun hermes-dashboard-transport--native-loopback-reply (process &optional ok)
  "Write a completion page to PROCESS and close it.
OK non-nil means the callback was accepted."
  (when (process-live-p process)
    (let* ((body (if ok
                     hermes-dashboard-transport--native-done-html
                   "<!doctype html><title>Hermes</title><p>Sign-in could not be completed.</p>"))
           (payload
            (concat "HTTP/1.1 "
                    (if ok "200 OK" "400 Bad Request")
                    "\r\n"
                    "Content-Type: text/html; charset=utf-8\r\n"
                    "Connection: close\r\n"
                    "Content-Length: "
                    (number-to-string (string-bytes body))
                    "\r\n\r\n"
                    body)))
      (ignore-errors (process-send-string process payload))
      (ignore-errors (delete-process process)))))

(defconst hermes-dashboard-transport--native-loopback-max-bytes 16384
  "Maximum accepted bytes for one native loopback HTTP request.")

(defun hermes-dashboard-transport--native-loopback-listen (on-request)
  "Start a loopback HTTP listener and call ON-REQUEST with the first request.
ON-REQUEST receives (REQUEST CLIENT).  Return a plist with :process, :port,
:redirect-uri, and :children."
  (let* ((children nil)
         (acc (make-hash-table :test #'eq))
         process
         port
         (closed nil)
         (deliver
          (lambda (request client)
            (unless closed
              (setq closed t)
              (funcall on-request request client)))))
    (setq process
          (make-network-process
           :name "hermes-native-loopback"
           :buffer nil
           :host "127.0.0.1"
           :service t
           :server t
           :noquery t
           :coding 'binary
           :log
           (lambda (_server client _message)
             (push client children)
             (set-process-query-on-exit-flag client nil))
           :filter
           (lambda (client chunk)
             (unless (processp client)
               (setq client process))
             (let* ((prev (or (gethash client acc) ""))
                    (next (concat prev (if (stringp chunk) chunk ""))))
               (cond
                ((> (length next)
                    hermes-dashboard-transport--native-loopback-max-bytes)
                 (remhash client acc)
                 (hermes-dashboard-transport--native-loopback-reply client nil))
                ((string-match "\r?\n\r?\n" next)
                 (remhash client acc)
                 (funcall deliver (substring next 0 (match-end 0)) client))
                (t
                 (puthash client next acc)))))
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(closed failed exit signal))
               (setq children (delq proc children))
               (remhash proc acc)))))
    (unless (process-live-p process)
      (user-error "Failed to bind loopback listener for native login"))
    (setq port
          (or (process-contact process :service)
              (car (last (process-contact process :local)))))
    (unless (integerp port)
      (delete-process process)
      (user-error "Failed to determine native loopback port"))
    (list :process process
          :port port
          :children (lambda () children)
          :redirect-uri (format "http://127.0.0.1:%d/callback" port))))

(defun hermes-dashboard-transport--native-loopback-close (server)
  "Close SERVER listener and any accepted child connections."
  (when server
    (dolist (child (ignore-errors
                     (and (functionp (plist-get server :children))
                          (funcall (plist-get server :children)))))
      (when (ignore-errors (process-live-p child))
        (ignore-errors (delete-process child))))
    (when-let* ((proc (plist-get server :process)))
      (when (ignore-errors (process-live-p proc))
        (ignore-errors (delete-process proc))))))

(defun hermes-dashboard-transport--native-login-async
    (base-url &optional provider startup-cancel-setter)
  "Return a promise of native tokens for BASE-URL after browser PKCE login.
Optional PROVIDER is forwarded to the authorize URL when non-empty.
STARTUP-CANCEL-SETTER registers the exact login cancellation owner when non-nil."
  (let* ((promise (hermes--promise-make))
         (pkce (hermes-dashboard-transport--pkce-pair))
         (state (hermes-dashboard-transport--native-state))
         (settled nil)
         (server nil)
         (timer nil)
         (cancel nil)
         (cancel-registered nil)
         (cleanup
          (lambda ()
            (let ((owned-timer timer)
                  (owned-server server)
                  (registered cancel-registered))
              (setq timer nil server nil cancel-registered nil)
              (when owned-timer
                (ignore-errors (cancel-timer owned-timer)))
              (ignore-errors
                (hermes-dashboard-transport--native-loopback-close owned-server))
              (when (and registered startup-cancel-setter cancel)
                (ignore-errors
                  (funcall startup-cancel-setter cancel nil))))))
         (fail
          (lambda (reason)
            (unless settled
              (setq settled t)
              (funcall cleanup)
              (hermes--promise-reject promise reason))))
         (finish
          (lambda (tokens)
            (unless settled
              (setq settled t)
              (funcall cleanup)
              (hermes--promise-resolve promise tokens)))))
    (setq cancel
          (lambda ()
            (funcall fail "Native dashboard sign-in was superseded")))
    (condition-case err
        (progn
          (when startup-cancel-setter
            (unless (funcall startup-cancel-setter nil cancel)
              (user-error "Native dashboard sign-in was superseded"))
            (setq cancel-registered t))
          (setq server
                (hermes-dashboard-transport--native-loopback-listen
                 (lambda (request client)
                   (unless settled
                     (condition-case request-err
                         (let* ((parsed
                                 (hermes-dashboard-transport--native-parse-loopback
                                  request state))
                                (code (plist-get parsed :code)))
                           ;; Accept only after state/code validation, then close
                           ;; the listener before the token exchange starts.
                           (hermes-dashboard-transport--native-loopback-reply
                            client t)
                           (hermes-dashboard-transport--native-loopback-close
                            server)
                           (setq server nil)
                           (hermes--promise-then
                            (hermes-dashboard-transport--http-json-async
                             (hermes-dashboard-transport--api-url
                              base-url "/auth/native/token")
                             :method "POST"
                             :headers '(("Content-Type" . "application/json"))
                             :body `((code . ,code)
                                     (code_verifier
                                      . ,(plist-get pkce :verifier)))
                             :secrets (list code (plist-get pkce :verifier))
                             :cancel-setter startup-cancel-setter
                             :cancel-expected cancel)
                            (lambda (response)
                              (condition-case response-err
                                  (if-let* ((tokens
                                             (hermes-dashboard-transport--native-token-plist
                                              (plist-get response :body))))
                                      (funcall finish tokens)
                                    (funcall fail
                                             "Gateway token response missing access_token"))
                                (error
                                 (funcall fail
                                          (error-message-string response-err)))))
                            (lambda (reason)
                              (funcall fail reason))))
                       (error
                        (hermes-dashboard-transport--native-loopback-reply
                         client nil)
                        (funcall fail (error-message-string request-err))))))))
          (setq timer
                (run-at-time
                 hermes-dashboard-transport-native-login-timeout
                 nil
                 (lambda ()
                   (funcall fail
                            "Native sign-in timed out before the browser returned"))))
          (hermes-dashboard-transport--browse-url
           (hermes-dashboard-transport--native-authorize-url
            base-url
            (plist-get pkce :challenge)
            (plist-get server :redirect-uri)
            state
            provider)))
      (error (funcall fail (error-message-string err))))
    promise))

(defun hermes-dashboard-transport--native-ticket-auth
    (host port base-url tokens ticket-response)
  "Return ticket WebSocket auth from TOKENS and TICKET-RESPONSE.
HOST, PORT, and BASE-URL build the authenticated WebSocket URL."
  (let ((ticket (hermes-transport--scalar-string
                 (hermes-transport--get (plist-get ticket-response :body)
                                        'ticket)))
        (access (plist-get tokens :access-token))
        (refresh (plist-get tokens :refresh-token)))
    (unless (and ticket (not (string-empty-p ticket)))
      (user-error "Hermes dashboard did not return a WebSocket ticket"))
    (list :url (hermes-dashboard-transport--websocket-url
                host port ticket base-url "ticket")
          :redacted-url (hermes-dashboard-transport--redacted-websocket-url
                         host port base-url "ticket")
          :kind 'ticket :reusable-p nil
          :secrets (delq nil (list access refresh ticket)))))

(defun hermes-dashboard-transport--native-ticket-async
    (host port base-url tokens &optional cancel-setter)
  "Return a promise of ticket WebSocket auth using native TOKENS.
HOST, PORT, and BASE-URL build the authenticated WebSocket URL.
CANCEL-SETTER owns the in-flight request when non-nil."
  (let* ((access (plist-get tokens :access-token))
         (refresh (plist-get tokens :refresh-token))
         (secrets (delq nil (list access refresh))))
    (hermes--promise-then
     (hermes-dashboard-transport--http-json-async
      (hermes-dashboard-transport--api-url base-url "/api/auth/ws-ticket")
      :method "POST"
      :headers `(("Authorization" . ,(concat "Bearer " access)))
      :secrets secrets
      :cancel-setter cancel-setter)
     (lambda (ticket-response)
       (hermes-dashboard-transport--native-ticket-auth
        host port base-url tokens ticket-response))
     (lambda (reason)
       (hermes--promise-rejected reason)))))

(defun hermes-dashboard-transport--native-refresh-async
    (base-url tokens &optional cancel-setter)
  "Return a promise of refreshed native tokens for BASE-URL.
TOKENS must include a refresh token; successful rotation is stored.
CANCEL-SETTER owns the in-flight request when non-nil."
  (let ((refresh (plist-get tokens :refresh-token))
        (provider (plist-get tokens :provider)))
    (if (or (not (stringp refresh)) (string-empty-p refresh))
        (hermes--promise-rejected
         "Native dashboard tokens expired and no refresh token is stored")
      (hermes--promise-then
       (hermes-dashboard-transport--http-json-async
        (hermes-dashboard-transport--api-url base-url "/auth/native/refresh")
        :method "POST"
        :headers '(("Content-Type" . "application/json"))
        :body (append `((refresh_token . ,refresh))
                      (and (stringp provider)
                           (not (string-empty-p provider))
                           `((provider . ,provider))))
        :secrets (list refresh)
        :cancel-setter cancel-setter)
       (lambda (response)
         (if-let* ((next (hermes-dashboard-transport--native-token-plist
                          (plist-get response :body)
                          tokens)))
             (progn
               (hermes-dashboard-transport--native-token-store base-url next)
               next)
           (hermes--promise-rejected
            "Gateway refresh response missing access_token")))
       (lambda (reason)
         (hermes--promise-rejected reason))))))

(defun hermes-dashboard-transport--native-ensure-tokens-async
    (base-url &optional provider force-login interactive cancel-setter)
  "Return a promise of usable native tokens for BASE-URL.
Optional PROVIDER is used for authorize/refresh.  When FORCE-LOGIN is non-nil,
skip stored tokens.  INTERACTIVE permits login; CANCEL-SETTER owns refresh or
interactive login work."
  (let ((stored (and (not force-login)
                     (hermes-dashboard-transport--native-token-load base-url))))
    (cond
     ((and stored
           (not (hermes-dashboard-transport--native-token-needs-refresh-p
                 stored)))
      (hermes--promise-resolved stored))
     ((and stored (hermes-transport--non-empty-string
                   (plist-get stored :refresh-token)))
      (hermes--promise-catch
       (hermes-dashboard-transport--native-refresh-async
        base-url stored cancel-setter)
       (lambda (reason)
         ;; A dead refresh token may force a fresh login only when the caller
         ;; explicitly owns browser interaction; preserve the old store until
         ;; the new login succeeds.
         (if (and interactive (stringp reason)
                  (string-match-p "(HTTP 401)" reason))
             (hermes--promise-then
              (hermes-dashboard-transport--native-login-async
               base-url provider cancel-setter)
              (lambda (tokens)
                (hermes-dashboard-transport--native-token-store base-url tokens)
                tokens))
           (hermes--promise-rejected reason)))))
     (interactive
      (hermes--promise-then
       (hermes-dashboard-transport--native-login-async
        base-url provider cancel-setter)
       (lambda (tokens)
         (hermes-dashboard-transport--native-token-store base-url tokens)
         tokens)))
     (t
      (hermes--promise-rejected
       "Native dashboard sign-in requires explicit interactive authorization")))))

(defun hermes-dashboard-transport--remote-native-auth-async
    (host port base-url &optional status force-login interactive cancel-setter)
  "Return a promise of native PKCE WebSocket auth for HOST, PORT, BASE-URL.
Optional STATUS supplies the OAuth provider name.  FORCE-LOGIN skips stored
tokens.  INTERACTIVE permits login; CANCEL-SETTER owns native startup work."
  (let ((provider (and status
                       (hermes-dashboard-transport--status-oauth-provider
                        status))))
    (hermes--promise-then
     (hermes-dashboard-transport--native-ensure-tokens-async
      base-url provider force-login interactive cancel-setter)
     (lambda (tokens)
       (hermes-dashboard-transport--native-ticket-async
        host port base-url tokens cancel-setter)))))

(defun hermes-dashboard-transport--api-native-auth-async
    (base-url &optional status)
  "Return a promise of REST bearer auth for BASE-URL using native tokens.
Optional STATUS supplies the OAuth provider name."
  (let ((provider (and status
                       (hermes-dashboard-transport--status-oauth-provider
                        status))))
    (hermes--promise-map
     (hermes-dashboard-transport--native-ensure-tokens-async
      base-url provider)
     (lambda (tokens)
       (let ((access (plist-get tokens :access-token))
             (refresh (plist-get tokens :refresh-token)))
         (list :headers
               (list (cons "Authorization" (concat "Bearer " access)))
               :secrets (delq nil (list access refresh))))))))

(defun hermes-dashboard-transport--loopback-host-p (host)
  "Return non-nil when HOST names a loopback dashboard bind."
  (member (downcase (or host "")) '("localhost" "127.0.0.1" "::1" "[::1]")))

;;; Client secrets and redacted URLs

(defun hermes-dashboard-transport--client-secrets (client)
  "Return all known secret strings currently associated with CLIENT."
  (hermes-dashboard-transport--secret-list
   (append (list (hermes-dashboard-transport-client-token client)
                 (hermes-dashboard-transport-client-auth-token client))
           (hermes-dashboard-transport-client-secrets client))))

(defun hermes-dashboard-transport--client-redacted-websocket-url (client)
  "Return a safe display WebSocket URL for CLIENT."
  (or (hermes-dashboard-transport-client-redacted-websocket-url client)
      (hermes-dashboard-transport--redacted-websocket-url
       (hermes-dashboard-transport-client-host client)
       (hermes-dashboard-transport-client-port client))))

(defun hermes-dashboard-transport--client-websocket-url (client)
  "Return the tokenized WebSocket URL for CLIENT."
  (or (hermes-dashboard-transport-client-websocket-url client)
      (hermes-dashboard-transport--websocket-url
       (hermes-dashboard-transport-client-host client)
       (hermes-dashboard-transport-client-port client)
       (hermes-dashboard-transport-client-token client))))

(defun hermes-dashboard-transport--condition-message (client condition)
  "Return a user-displayable CONDITION message for CLIENT."
  (hermes-dashboard-transport--redact-secret
   (error-message-string condition)
   (hermes-dashboard-transport--client-secrets client)))


;;; REST API and authentication

(defvar hermes-dashboard-transport--api-auth nil
  "Cached dashboard REST auth plist.
The value is `(:base-url URL :headers HEADERS :secrets SECRETS)'.")

(defun hermes-dashboard-transport--api-base-url ()
  "Return the configured dashboard REST base URL."
  (or (hermes-dashboard-transport--normalize-base-url
       hermes-dashboard-transport-url)
      (user-error "Set `hermes-dashboard-transport-url' to a Hermes dashboard URL")))

(defun hermes-dashboard-transport--api-token-auth (base-url)
  "Return REST token auth for dashboard BASE-URL."
  (let ((token (hermes-dashboard-transport--remote-token-secret base-url)))
    (list :headers (list (cons "X-Hermes-Session-Token" token))
          :secrets (list token))))

(defun hermes-dashboard-transport--basic-auth-request
    (base-url provider username password &optional next)
  "Return the password-login request plist for dashboard BASE-URL.
PROVIDER, USERNAME, PASSWORD, and NEXT (default \"\") form the JSON body.
The result carries :url, :method, :headers, :body, and :secrets, ready for
`hermes-dashboard-transport--http-json-request' or its async variant;
:secrets holds PASSWORD for redaction."
  (list :url (hermes-dashboard-transport--api-url
              base-url "/auth/password-login")
        :method "POST"
        :headers '(("Content-Type" . "application/json"))
        :body `((provider . ,provider)
                (username . ,username)
                (password . ,password)
                (next . ,(or next "")))
        :secrets (list password)))

(defun hermes-dashboard-transport--basic-login-request (base-url status)
  "Return the password-login request plist for BASE-URL described by STATUS.
Signals when STATUS lacks a basic provider or auth-source has no credentials.
The password is the sole entry of the request's :secrets list."
  (let ((provider (hermes-dashboard-transport--status-basic-provider status)))
    (unless provider
      (hermes-dashboard-transport--unsupported-remote-auth base-url))
    (let ((credentials (hermes-dashboard-transport--remote-basic-credentials
                        base-url)))
      (hermes-dashboard-transport--basic-auth-request
       base-url provider
       (plist-get credentials :username)
       (plist-get credentials :password)))))

(defun hermes-dashboard-transport--api-basic-auth (base-url status)
  "Return REST cookie auth for dashboard BASE-URL described by STATUS."
  (let* ((request (hermes-dashboard-transport--basic-login-request
                   base-url status))
         (password (car (plist-get request :secrets)))
         (response (hermes-dashboard-transport--http-json-request request))
         (cookies (hermes-dashboard-transport--response-cookie-header response)))
    (unless cookies
      (user-error "Hermes dashboard basic login returned no session cookies"))
    (list :headers (list (cons "Cookie" cookies))
          :secrets (list password cookies))))

(defun hermes-dashboard-transport--api-authenticate ()
  "Resolve dashboard REST auth for `hermes-dashboard-transport-url'.
Native PKCE requires the async path; this legacy synchronous resolver rejects
native-gated dashboards instead of silently falling back to basic/token."
  (let ((base-url (hermes-dashboard-transport--api-base-url)))
    (append
     (list :base-url base-url)
     (pcase hermes-dashboard-transport-remote-auth-method
       ('token (hermes-dashboard-transport--api-token-auth base-url))
       ('basic (hermes-dashboard-transport--api-basic-auth
                base-url (hermes-dashboard-transport--remote-status base-url)))
       ('native
        (user-error
         "Native PKCE dashboard auth requires the asynchronous request path"))
       (_ (let ((status (hermes-dashboard-transport--remote-status base-url)))
            (cond
             ((not (hermes-dashboard-transport--status-auth-required-p status))
              (hermes-dashboard-transport--api-token-auth base-url))
             ((hermes-dashboard-transport--status-supports-native-pkce-p status)
              (user-error
               "Native PKCE dashboard auth requires the asynchronous request path"))
             ((hermes-dashboard-transport--status-basic-provider status)
              (hermes-dashboard-transport--api-basic-auth base-url status))
             (t (hermes-dashboard-transport--unsupported-remote-auth
                 base-url)))))))))

(defun hermes-dashboard-transport--auth-error-p (reason)
  "Return non-nil when REASON is an HTTP 401/403 authentication failure.
Only an expired or rejected credential justifies dropping cached auth and
retrying; other failures (404, 5xx, network) must not loop through
re-authentication."
  (and (stringp reason)
       (string-match-p "(HTTP 40[13])" reason)))

(defun hermes-dashboard-transport--api-auth-stale-p ()
  "Return non-nil when cached REST auth no longer matches the configured URL.
Keeps `hermes-dashboard-transport--api-auth' from serving credentials for a
previous `hermes-dashboard-transport-url' after the user switches dashboards."
  (and hermes-dashboard-transport--api-auth
       (not (equal (plist-get hermes-dashboard-transport--api-auth :base-url)
                   (ignore-errors (hermes-dashboard-transport--api-base-url))))))

(defun hermes-dashboard-transport-api-auth (&optional refresh)
  "Return dashboard REST auth, resolving it when REFRESH is non-nil.
Cached auth is also re-resolved when `hermes-dashboard-transport-url' changes."
  (when (or refresh (hermes-dashboard-transport--api-auth-stale-p))
    (setq hermes-dashboard-transport--api-auth nil))
  (or hermes-dashboard-transport--api-auth
      (setq hermes-dashboard-transport--api-auth
            (hermes-dashboard-transport--api-authenticate))))

(defun hermes-dashboard-transport--query-string (query)
  "Return a URL query string for QUERY, an alist of (KEY . VALUE)."
  (if query
      (concat "?" (mapconcat (lambda (entry)
                               (hermes-dashboard-transport--query-pair
                                (car entry) (cdr entry)))
                             query "&"))
    ""))

(defun hermes-dashboard-transport--api-client-token (client)
  "Return CLIENT's dashboard session token, or nil."
  (and (hermes-dashboard-transport-client-p client)
       (hermes-transport--non-empty-string
        (hermes-dashboard-transport-client-token client))))

(defun hermes-dashboard-transport--api-client-base-url (client)
  "Return CLIENT's dashboard HTTP base URL, or the configured REST URL."
  (or (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport-client-base-url client))
      (and (hermes-dashboard-transport-client-p client)
           (hermes-dashboard-transport-client-port client)
           (hermes-dashboard-transport--base-url
            (hermes-dashboard-transport-client-host client)
            (hermes-dashboard-transport-client-port client)))
      (hermes-dashboard-transport--api-base-url)))

(cl-defun hermes-dashboard-transport--api-request-plist
    (auth method path &key body query headers secrets timeout)
  "Return the REST request plist for METHOD PATH under resolved AUTH.
BODY, QUERY, HEADERS, SECRETS, and TIMEOUT extend the request; AUTH supplies
the base URL plus its own headers and secrets.  Pure: shared by the synchronous
and asynchronous request executors."
  (list :url (concat (hermes-dashboard-transport--api-url
		      (plist-get auth :base-url) path)
		     (hermes-dashboard-transport--query-string query))
	:method method
	:headers (append (plist-get auth :headers)
			 headers
			 (and body '(("Content-Type" . "application/json"))))
	:body body
	:timeout timeout
	:secrets (append secrets (plist-get auth :secrets))))

(defun hermes-dashboard-transport--api-client-auth (client)
  "Return a REST auth plist derived from CLIENT's live session token."
  (let ((token (hermes-dashboard-transport--api-client-token client)))
    (list :base-url (hermes-dashboard-transport--api-client-base-url client)
          :headers (and token (list (cons "X-Hermes-Session-Token" token)))
          :secrets (and token (list token)))))

(cl-defun hermes-dashboard-transport--api-request-with-client
    (client method path &key body query headers secrets)
  "Call dashboard REST METHOD PATH using CLIENT's live session token.
Legacy synchronous path; see `hermes-dashboard-transport-api-request'."
  (let ((request (hermes-dashboard-transport--api-request-plist
		  (hermes-dashboard-transport--api-client-auth client)
		  method path :body body :query query :headers headers
		  :secrets secrets)))
    (condition-case err
	(plist-get (hermes-dashboard-transport--http-json-request request)
                   :body)
      (error
       (signal (car err)
               (list (hermes-dashboard-transport--redact-secret
		      (error-message-string err)
		      (plist-get request :secrets))))))))

(cl-defun hermes-dashboard-transport--api-request-1
    (method path &key body query headers secrets retry)
  "Call dashboard REST METHOD PATH with BODY and QUERY.
HEADERS and SECRETS extend the resolved dashboard auth.  RETRY refreshes auth
once when the request fails."
  (let ((request (hermes-dashboard-transport--api-request-plist
		  (hermes-dashboard-transport-api-auth)
		  method path :body body :query query :headers headers
		  :secrets secrets)))
    (condition-case err
	(plist-get (hermes-dashboard-transport--http-json-request request)
                   :body)
      (error
       (if (and retry (hermes-dashboard-transport--auth-error-p
                       (error-message-string err)))
           (progn
             (hermes-dashboard-transport-api-auth t)
             (hermes-dashboard-transport--api-request-1
              method path :body body :query query :headers headers
              :secrets secrets :retry nil))
         (signal (car err)
                 (list (hermes-dashboard-transport--redact-secret
			(error-message-string err)
			(plist-get request :secrets)))))))))

(cl-defun hermes-dashboard-transport-api-request
    (method path &key body query headers secrets client)
  "Call authenticated dashboard REST METHOD PATH.
PATH is appended to `hermes-dashboard-transport-url'.  BODY is JSON-encoded,
QUERY is an alist encoded as a query string, and HEADERS/SECRETS extend the
authenticated request.  CLIENT, when it has a live session token, supplies the
spawned dashboard base URL and `X-Hermes-Session-Token'.  GET requests using
cached auth retry once with refreshed auth.
Legacy: new callers must use
`hermes-dashboard-transport-api-request-async'; synchronous network on the
main thread is banned by AGENTS.md."
  (if (hermes-dashboard-transport--api-client-token client)
      (hermes-dashboard-transport--api-request-with-client
       client method path :body body :query query :headers headers
       :secrets secrets)
    (hermes-dashboard-transport--api-request-1
     method path :body body :query query :headers headers :secrets secrets
     :retry (equal method "GET"))))

(defun hermes-dashboard-transport--remote-status-async
    (base-url &optional cancel-setter)
  "Return a promise of the /api/status object from dashboard BASE-URL.
CANCEL-SETTER owns the in-flight request when non-nil."
  (hermes--promise-map
   (hermes-dashboard-transport--http-json-async
    (hermes-dashboard-transport--api-url base-url "/api/status")
    :cancel-setter cancel-setter)
   (lambda (response) (plist-get response :body))))

(defun hermes-dashboard-transport--api-token-auth-async (base-url)
  "Return a promise of REST token auth for dashboard BASE-URL.
Token resolution is local (auth-source or environment) and never blocks on the
network; a missing token rejects the promise."
  (condition-case err
      (hermes--promise-resolved
       (hermes-dashboard-transport--api-token-auth base-url))
    (error (hermes--promise-rejected (error-message-string err)))))

(defun hermes-dashboard-transport--api-basic-auth-async (base-url status)
  "Return a promise of REST cookie auth for BASE-URL described by STATUS."
  (condition-case err
      (let* ((request (hermes-dashboard-transport--basic-login-request
                       base-url status))
             (password (car (plist-get request :secrets))))
        (hermes--promise-then
         (hermes-dashboard-transport--http-json-request-async request)
         (lambda (response)
           (let ((cookies (hermes-dashboard-transport--response-cookie-header
                           response)))
             (if cookies
                 (list :headers (list (cons "Cookie" cookies))
                       :secrets (list password cookies))
               (hermes--promise-rejected
                "Hermes dashboard basic login returned no session cookies"))))))
    (error (hermes--promise-rejected (error-message-string err)))))

(defun hermes-dashboard-transport--api-authenticate-async ()
  "Return a promise of dashboard REST auth for `hermes-dashboard-transport-url'."
  (condition-case err
      (let ((base-url (hermes-dashboard-transport--api-base-url)))
        (hermes--promise-map
         (pcase hermes-dashboard-transport-remote-auth-method
           ('token (hermes-dashboard-transport--api-token-auth-async base-url))
           ('basic (hermes--promise-then
                    (hermes-dashboard-transport--remote-status-async base-url)
                    (lambda (status)
                      (hermes-dashboard-transport--api-basic-auth-async
                       base-url status))))
           ('native (hermes--promise-then
                     (hermes-dashboard-transport--remote-status-async base-url)
                     (lambda (status)
                       (hermes-dashboard-transport--api-native-auth-async
                        base-url status))))
           (_ (hermes--promise-then
               (hermes-dashboard-transport--remote-status-async base-url)
               (lambda (status)
                 (cond
                  ((not (hermes-dashboard-transport--status-auth-required-p
                         status))
                   (hermes-dashboard-transport--api-token-auth-async base-url))
                  ((hermes-dashboard-transport--status-supports-native-pkce-p
                    status)
                   (hermes-dashboard-transport--api-native-auth-async
                    base-url status))
                  ((hermes-dashboard-transport--status-basic-provider status)
                   (hermes-dashboard-transport--api-basic-auth-async
                    base-url status))
                  (t (hermes-dashboard-transport--unsupported-remote-auth
                      base-url)))))))
         (lambda (auth) (append (list :base-url base-url) auth))))
    (error (hermes--promise-rejected (error-message-string err)))))

(defun hermes-dashboard-transport-api-auth-async (&optional refresh)
  "Return a promise of dashboard REST auth, re-resolving when REFRESH is non-nil.
The resolved auth is cached in `hermes-dashboard-transport--api-auth', shared
with the synchronous path, and re-resolved when the configured URL changes."
  (when (or refresh (hermes-dashboard-transport--api-auth-stale-p))
    (setq hermes-dashboard-transport--api-auth nil))
  (if hermes-dashboard-transport--api-auth
      (hermes--promise-resolved hermes-dashboard-transport--api-auth)
    (hermes--promise-map
     (hermes-dashboard-transport--api-authenticate-async)
     (lambda (auth)
       (setq hermes-dashboard-transport--api-auth auth)
       auth))))

(cl-defun hermes-dashboard-transport--api-request-1-async
    (method path &key body query headers secrets timeout retry base-url)
  "Return a promise of dashboard REST METHOD PATH using resolved auth.
BODY, QUERY, HEADERS, SECRETS, and TIMEOUT extend the request; RETRY refreshes
auth and retries once when the request fails.  BASE-URL, when non-nil, pins
authentication and retries to that dashboard endpoint."
  (let* ((base-url (or base-url
                       (hermes-dashboard-transport--api-base-url)))
         (hermes-dashboard-transport-url base-url))
    (hermes--promise-then
     (hermes-dashboard-transport-api-auth-async)
     (lambda (auth)
       (let ((request (hermes-dashboard-transport--api-request-plist
		       auth method path :body body :query query
		       :headers headers :secrets secrets :timeout timeout)))
         (hermes--promise-catch
          (hermes--promise-map
	   (hermes-dashboard-transport--http-json-request-async request)
           (lambda (response) (plist-get response :body)))
          (lambda (reason)
            (if (and retry (hermes-dashboard-transport--auth-error-p reason))
                (progn
                  (setq hermes-dashboard-transport--api-auth nil)
                  (hermes-dashboard-transport--api-request-1-async
                   method path :body body :query query :headers headers
                   :secrets secrets :timeout timeout :retry nil
                   :base-url base-url))
              (hermes--promise-rejected
	       (hermes-dashboard-transport--redact-secret
	        reason (plist-get request :secrets)))))))))))

(cl-defun hermes-dashboard-transport--api-request-with-client-async
    (client method path &key body query headers secrets timeout)
  "Return a promise of dashboard REST METHOD PATH using CLIENT's session token.
BODY, QUERY, HEADERS, SECRETS, and TIMEOUT extend the request."
  (hermes--promise-map
   (hermes-dashboard-transport--http-json-request-async
    (hermes-dashboard-transport--api-request-plist
     (hermes-dashboard-transport--api-client-auth client)
     method path :body body :query query :headers headers :secrets secrets
     :timeout timeout))
   (lambda (response) (plist-get response :body))))

(cl-defun hermes-dashboard-transport-api-request-async
    (method path &key body query headers secrets client timeout)
  "Return a promise of authenticated dashboard REST METHOD PATH.
Mirrors `hermes-dashboard-transport-api-request' but resolves asynchronously so
callers never block Emacs.  BODY, QUERY, HEADERS, SECRETS, and TIMEOUT extend
the request.  CLIENT pins the dashboard base URL.  Its live session token is
used when present; otherwise REST auth is resolved for that endpoint."
  (cond
   ((hermes-dashboard-transport--api-client-token client)
    (hermes-dashboard-transport--api-request-with-client-async
     client method path :body body :query query :headers headers
     :secrets secrets :timeout timeout))
   ((hermes-dashboard-transport-client-p client)
    (hermes-dashboard-transport--api-request-1-async
     method path :body body :query query :headers headers :secrets secrets
     :timeout timeout :retry (equal method "GET")
     :base-url (hermes-dashboard-transport--api-client-base-url client)))
   (t
    (hermes-dashboard-transport--api-request-1-async
     method path :body body :query query :headers headers :secrets secrets
     :timeout timeout :retry (equal method "GET")))))

;;; Profile and model caches

(defvar hermes-dashboard-transport--profile-cache nil
  "Cached `/api/profiles' payloads as an alist of (BASE-URL . PAYLOAD).")

(defun hermes-dashboard-transport--cache-base-url (&optional client)
  "Return the normalized endpoint identity for CLIENT or the configured URL."
  (ignore-errors
    (hermes-dashboard-transport--normalize-base-url
     (cond
      (client (hermes-dashboard-transport--api-client-base-url client))
      ((and hermes-instances (hermes-instance--valid-p hermes-instance))
       (hermes-instance-url hermes-instance))
      (hermes-instances
       (and-let* ((instance (hermes-instance-context)))
         (hermes-instance-url instance)))
      (t (hermes-dashboard-transport--api-client-base-url nil))))))

(defun hermes-dashboard-transport--endpoint-cache-get (cache base-url)
  "Return the payload in endpoint CACHE for BASE-URL.
Also accepts the single-entry cache shape used before endpoint isolation."
  (if (keywordp (car-safe cache))
      (and (equal (plist-get cache :base-url) base-url)
           (plist-get cache :payload))
    (cdr (assoc base-url cache))))

(defun hermes-dashboard-transport--endpoint-cache-put
    (cache base-url payload)
  "Return CACHE with PAYLOAD stored for BASE-URL."
  (let ((entries (unless (keywordp (car-safe cache)) cache)))
    (cons (cons base-url payload)
          (cl-remove base-url entries :key #'car :test #'equal))))

(defun hermes-dashboard-transport--store-profile-cache (payload &optional base-url)
  "Cache PAYLOAD for BASE-URL and return it.
BASE-URL defaults to the currently configured dashboard endpoint."
  (setq hermes-dashboard-transport--profile-cache
        (hermes-dashboard-transport--endpoint-cache-put
         hermes-dashboard-transport--profile-cache
         (or base-url (hermes-dashboard-transport--cache-base-url)) payload))
  payload)

(defun hermes-dashboard-transport-cached-profile-list (&optional client)
  "Return cached `/api/profiles' for CLIENT's endpoint, or nil.
The cache is warmed by `hermes-dashboard-transport-profile-list-async'."
  (hermes-dashboard-transport--endpoint-cache-get
   hermes-dashboard-transport--profile-cache
   (hermes-dashboard-transport--cache-base-url client)))

(defvar hermes-dashboard-transport--model-options-cache nil
  "Cached `model.options' payloads as an alist of (BASE-URL . PAYLOAD).
The provider/model catalog is dashboard-global -- disk config plus the curated
model list -- so it is shared across sessions for the same endpoint.  A saved
API key invalidates it;
see `hermes-dashboard-transport-invalidate-model-options'.")

(defun hermes-dashboard-transport--store-model-options (payload &optional base-url)
  "Cache PAYLOAD for BASE-URL and return it.
BASE-URL defaults to the currently configured dashboard endpoint."
  (setq hermes-dashboard-transport--model-options-cache
        (hermes-dashboard-transport--endpoint-cache-put
         hermes-dashboard-transport--model-options-cache
         (or base-url (hermes-dashboard-transport--cache-base-url)) payload))
  payload)

(defun hermes-dashboard-transport-cached-model-options (&optional client)
  "Return cached `model.options' for CLIENT's endpoint, or nil.
The cache is warmed by `hermes-dashboard-transport-model-options-cached' and is
discarded when `hermes-dashboard-transport-invalidate-model-options' is called."
  (hermes-dashboard-transport--endpoint-cache-get
   hermes-dashboard-transport--model-options-cache
   (hermes-dashboard-transport--cache-base-url client)))

(defun hermes-dashboard-transport-invalidate-model-options ()
  "Discard any cached `model.options' payload.
Callers that change provider authentication -- for example after saving an API
key -- call this so the next picker refetches the full list."
  (setq hermes-dashboard-transport--model-options-cache nil))

(defun hermes-dashboard-transport-profile-list (&optional client)
  "Return dashboard profile metadata from REST `/api/profiles'.
When CLIENT is non-nil, authenticate with its live dashboard session token.
The payload is cached for CLIENT's endpoint so subsequent profile prompts can
read it without blocking (see
`hermes-dashboard-transport-cached-profile-list')."
  (let ((base-url (hermes-dashboard-transport--cache-base-url client)))
    (hermes-dashboard-transport--store-profile-cache
     (hermes-dashboard-transport-api-request
      "GET" "/api/profiles" :client client)
     base-url)))

(defun hermes-dashboard-transport-profile-list-async (&optional client)
  "Return a promise of `/api/profiles', warming the profile cache on success.
When CLIENT is non-nil, authenticate with its live dashboard session token.
Resolves without blocking Emacs, so callers can warm the cache eagerly (for
example when the dashboard opens)."
  (let ((base-url (hermes-dashboard-transport--cache-base-url client)))
    (hermes--promise-map
     (hermes-dashboard-transport-api-request-async
      "GET" "/api/profiles" :client client)
     (lambda (payload)
       (hermes-dashboard-transport--store-profile-cache payload base-url)))))

(defun hermes-dashboard-transport-active-profile (&optional client)
  "Return dashboard active-profile metadata from REST `/api/profiles/active'.
When CLIENT is non-nil, authenticate with its live dashboard session token."
  (hermes-dashboard-transport-api-request
   "GET" "/api/profiles/active" :client client))

;;; Remote authentication

(defun hermes-dashboard-transport--response-header-values (response name)
  "Return all header values named NAME in RESPONSE."
  (let ((needle (downcase name)))
    (delq nil
          (mapcar (lambda (header)
                    (and (equal (car header) needle) (cdr header)))
                  (plist-get response :headers)))))

(defun hermes-dashboard-transport--response-cookie-header (response)
  "Return Cookie header value assembled from RESPONSE Set-Cookie headers."
  (let ((cookies (delq nil
                       (mapcar (lambda (header)
                                 (and-let* ((pair (car (split-string
                                                        header ";" t))))
                                   (string-trim pair)))
                               (hermes-dashboard-transport--response-header-values
                                response "set-cookie")))))
    (and cookies (string-join cookies "; "))))

(defun hermes-dashboard-transport--remote-status (base-url)
  "Return /api/status object from dashboard BASE-URL."
  (plist-get (hermes-dashboard-transport--http-json
              (hermes-dashboard-transport--api-url base-url "/api/status"))
             :body))

(defun hermes-dashboard-transport--status-auth-required-p (status)
  "Return non-nil when STATUS reports a gated dashboard."
  (eq (hermes-transport--get status 'auth_required) t))

(defun hermes-dashboard-transport--status-auth-providers (status)
  "Return auth provider names from STATUS as strings."
  (mapcar #'hermes-transport--scalar-string
          (or (hermes-transport--get status 'auth_providers) '())))

(defun hermes-dashboard-transport--status-basic-provider (status)
  "Return the basic/password auth provider name from STATUS, if present."
  (cl-find "basic"
           (hermes-dashboard-transport--status-auth-providers status)
           :test #'equal))

(defun hermes-dashboard-transport--auth-source-hosts (base-url)
  "Return auth-source host aliases for dashboard BASE-URL."
  (let* ((parsed (url-generic-parse-url base-url))
         (scheme (url-type parsed))
         (host (url-host parsed))
         (port (url-port parsed))
         (origin (and scheme host
                      (format "%s://%s%s"
                              scheme
                              (hermes-dashboard-transport--host-for-url host)
                              (if port (format ":%d" port) "")))))
    (cl-remove-duplicates
     (delq nil (list base-url origin
                     (and host port (format "%s:%d" host port))
                     host))
     :test #'equal)))

(defun hermes-dashboard-transport--require-auth-source ()
  "Ensure auth-source is available for remote credential lookup."
  (unless (or (fboundp 'auth-source-search)
              (require 'auth-source nil t))
    (user-error "Remote Hermes dashboard credentials require auth-source")))

(defun hermes-dashboard-transport--auth-source-entry (base-url &rest args)
  "Return first auth-source entry for BASE-URL using ARGS."
  (hermes-dashboard-transport--require-auth-source)
  (catch 'entry
    (dolist (host (hermes-dashboard-transport--auth-source-hosts base-url))
      (when-let* ((entries (apply #'auth-source-search
                                  :host host :max 1 args)))
        (throw 'entry (car entries))))
    nil))

(defun hermes-dashboard-transport--auth-source-secret (entry)
  "Return secret string from auth-source ENTRY."
  (and-let* ((secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun hermes-dashboard-transport--remote-token-secret (base-url &optional token)
  "Return legacy dashboard session token for BASE-URL, preferring TOKEN."
  (or (hermes-transport--non-empty-string token)
      (when-let* ((entry (hermes-dashboard-transport--auth-source-entry
                          base-url
                          :user "hermes-dashboard-token"
                          :port "hermes-dashboard-token"
                          :require '(:secret))))
        (hermes-transport--non-empty-string
         (hermes-dashboard-transport--auth-source-secret entry)))
      (hermes-transport--non-empty-string
       (getenv "HERMES_DASHBOARD_SESSION_TOKEN"))
      (user-error
       "No Hermes dashboard session token found; add auth-source login hermes-dashboard-token with port hermes-dashboard-token, or set HERMES_DASHBOARD_SESSION_TOKEN for legacy token attach")))

(defun hermes-dashboard-transport--remote-basic-credentials (base-url)
  "Return plist with username and password from auth-source for BASE-URL."
  (let* ((entry (hermes-dashboard-transport--auth-source-entry
                 base-url :port "hermes-dashboard-basic"
                 :require '(:user :secret)))
         (username (and entry (plist-get entry :user)))
         (password (and entry
                        (hermes-dashboard-transport--auth-source-secret entry))))
    (unless (and (stringp username) (not (string-empty-p username))
                 (stringp password) (not (string-empty-p password)))
      (user-error
       "No Hermes dashboard basic credentials found; add auth-source port hermes-dashboard-basic with login and password"))
    (list :username username :password password)))

(defun hermes-dashboard-transport--unsupported-remote-auth (base-url)
  "Signal an actionable unsupported gated auth error for BASE-URL."
  (user-error
   (concat "Hermes dashboard at %s requires gated auth, but this Emacs client "
           "currently supports native PKCE OAuth, basic/password gated "
           "dashboards, or legacy session tokens only; OAuth-only remote attach "
           "is not implemented without the native_pkce auth flow")
   base-url))

(defun hermes-dashboard-transport--remote-token-auth
    (host port base-url &optional token)
  "Return legacy-token auth plist for HOST, PORT, BASE-URL, and TOKEN."
  (let* ((token (hermes-dashboard-transport--remote-token-secret base-url token))
         (url (hermes-dashboard-transport--websocket-url
               host port token base-url "token"))
         (redacted-url (hermes-dashboard-transport--redacted-websocket-url
                        host port base-url "token")))
    (list :token token :url url :redacted-url redacted-url
          :kind 'legacy-token :reusable-p t
          :secrets (list token))))

(defun hermes-dashboard-transport--basic-ticket-auth
    (host port base-url password cookies ticket-response)
  "Return ticket WebSocket auth plist built from TICKET-RESPONSE.
HOST, PORT, and BASE-URL build the URL; PASSWORD and COOKIES extend the
redacted secret list."
  (let ((ticket (hermes-transport--scalar-string
                 (hermes-transport--get (plist-get ticket-response :body)
                                        'ticket))))
    (unless (and ticket (not (string-empty-p ticket)))
      (user-error "Hermes dashboard did not return a WebSocket ticket"))
    (list :url (hermes-dashboard-transport--websocket-url
                host port ticket base-url "ticket")
          :redacted-url (hermes-dashboard-transport--redacted-websocket-url
                         host port base-url "ticket")
          :kind 'ticket :reusable-p nil
          :secrets (list password cookies ticket))))

(defun hermes-dashboard-transport--remote-basic-ticket-async
    (host port base-url password login-response)
  "Return a promise of ticket WebSocket auth from LOGIN-RESPONSE cookies.
HOST, PORT, and BASE-URL build the URL; PASSWORD is redacted from errors."
  (let ((cookies (hermes-dashboard-transport--response-cookie-header
                  login-response)))
    (if (not cookies)
        (hermes--promise-rejected
         "Hermes dashboard basic login did not return session cookies")
      (hermes--promise-map
       (hermes-dashboard-transport--http-json-async
        (hermes-dashboard-transport--api-url base-url "/api/auth/ws-ticket")
        :method "POST"
        :headers `(("Cookie" . ,cookies))
        :secrets (list password cookies))
       (lambda (ticket-response)
         (hermes-dashboard-transport--basic-ticket-auth
          host port base-url password cookies ticket-response))))))

(defun hermes-dashboard-transport--remote-basic-auth-async
    (host port base-url status)
  "Return a promise of basic-auth WebSocket auth for HOST, PORT, BASE-URL, STATUS.
A rejected promise reports any failure, so the password login and WebSocket
ticket round-trips never block Emacs."
  (condition-case err
      (let* ((request (hermes-dashboard-transport--basic-login-request
                       base-url status))
             (password (car (plist-get request :secrets))))
        (hermes--promise-then
         (hermes-dashboard-transport--http-json-request-async request)
         (lambda (login-response)
           (hermes-dashboard-transport--remote-basic-ticket-async
            host port base-url password login-response))))
    (error (hermes--promise-rejected (error-message-string err)))))

(defun hermes-dashboard-transport--remote-token-auth-async
    (host port base-url &optional token)
  "Return a promise of legacy-token WebSocket auth for HOST, PORT, BASE-URL, TOKEN.
Token resolution is local (auth-source or environment) and never blocks on the
network; a missing token rejects the promise."
  (condition-case err
      (hermes--promise-resolved
       (hermes-dashboard-transport--remote-token-auth host port base-url token))
    (error (hermes--promise-rejected (error-message-string err)))))

(defun hermes-dashboard-transport--remote-auth-async
    (host port base-url method &optional token interactive cancel-setter)
  "Return a promise of WebSocket auth for HOST, PORT, BASE-URL, METHOD, and TOKEN.
Mirrors the previous synchronous resolution without blocking.  INTERACTIVE
permits native login; CANCEL-SETTER owns native startup work."
  (pcase method
    ('token (hermes-dashboard-transport--remote-token-auth-async
             host port base-url token))
    ('basic (hermes--promise-then
             (hermes-dashboard-transport--remote-status-async base-url)
             (lambda (status)
               (hermes-dashboard-transport--remote-basic-auth-async
                host port base-url status))))
    ('native (hermes--promise-then
              (hermes-dashboard-transport--remote-status-async
               base-url cancel-setter)
              (lambda (status)
                (hermes-dashboard-transport--remote-native-auth-async
                 host port base-url status nil interactive cancel-setter))))
    ('auto (hermes--promise-then
            (hermes-dashboard-transport--remote-status-async
             base-url cancel-setter)
            (lambda (status)
              (cond
               ((not (hermes-dashboard-transport--status-auth-required-p
                      status))
                (hermes-dashboard-transport--remote-token-auth-async
                 host port base-url token))
               ((hermes-dashboard-transport--status-supports-native-pkce-p
                 status)
                (hermes-dashboard-transport--remote-native-auth-async
                 host port base-url status nil interactive cancel-setter))
               ((hermes-dashboard-transport--status-basic-provider status)
                (hermes-dashboard-transport--remote-basic-auth-async
                 host port base-url status))
               (t (condition-case err
                      (hermes-dashboard-transport--unsupported-remote-auth
                       base-url)
                    (error (hermes--promise-rejected
                            (error-message-string err)))))))))
    (_ (hermes--promise-rejected
        (format "Unknown Hermes dashboard remote auth method: %S" method)))))

(defun hermes-dashboard-transport--kanban-events-plist (auth since board)
  "Return the kanban events URL plist derived from AUTH for SINCE and BOARD.
AUTH is a (:url :redacted-url :secrets) plist; the events path is swapped onto
both URLs and SINCE/BOARD are appended as query parameters."
  (let* ((params `((since . ,since) (board . ,board)))
         (path hermes-dashboard-transport--kanban-events-path)
         (events-url (lambda (key)
                       (hermes-dashboard-transport--append-url-query
                        (hermes-dashboard-transport--swap-websocket-path
                         (plist-get auth key) path)
                        params))))
    (list :url (funcall events-url :url)
          :redacted-url (funcall events-url :redacted-url)
          :secrets (plist-get auth :secrets))))

(defun hermes-dashboard-transport--client-auth-plist (client)
  "Return reusable active socket auth from CLIENT, or nil.
Only explicitly reusable spawn, internal, and legacy-token credentials qualify."
  (when-let* (((hermes-dashboard-transport-client-credential-reusable-p client))
              ((memq (hermes-dashboard-transport-client-credential-kind client)
                     '(spawn internal legacy-token)))
              (url (hermes-dashboard-transport--client-websocket-url client)))
    (list :url url
          :redacted-url
          (or (hermes-dashboard-transport-client-redacted-websocket-url client)
              (hermes-dashboard-transport--client-redacted-websocket-url client))
          :secrets (hermes-dashboard-transport--client-secrets client)
          :kind (hermes-dashboard-transport-client-credential-kind client)
          :reusable-p t)))

(defun hermes-dashboard-transport--auth-plist-async (client)
  "Return a promise of a WebSocket auth (:url :redacted-url :secrets) plist.
When CLIENT already has a resolved WebSocket URL its credential is reused;
otherwise auth resolves against `hermes-dashboard-transport-url' exactly as
the chat client does."
  (if-let* ((auth (and client
                       (hermes-dashboard-transport--client-auth-plist client))))
      (hermes--promise-resolved auth)
    (let* ((target (and (not client)
                        (hermes-dashboard-transport--parse-url
                         hermes-dashboard-transport-url)))
           (host (or (plist-get target :host) "127.0.0.1"))
           (port (plist-get target :port))
           (remote-url (and (not (hermes-dashboard-transport--loopback-host-p host))
                            hermes-dashboard-transport-url))
           (host (or (and client
                          (hermes-dashboard-transport-client-host client))
                     host))
           (port (or (and client
                          (hermes-dashboard-transport-client-port client))
                     port))
           (base-url (or (and client
                              (hermes-dashboard-transport-client-base-url client))
                         (hermes-dashboard-transport--base-url host port remote-url)))
           (method (or (and client
                            (hermes-dashboard-transport-client-auth-method client))
                       hermes-dashboard-transport-remote-auth-method 'auto))
           (token (and client
                       (hermes-dashboard-transport-client-auth-token client))))
      (hermes-dashboard-transport--remote-auth-async
       host port base-url method token nil))))

(cl-defun hermes-dashboard-transport-kanban-events-url-async
    (&key since board client)
  "Return a promise of the kanban live-events WebSocket URL plist.
The plist is (:url :redacted-url :secrets).  When CLIENT already has a resolved
WebSocket URL it is reused; otherwise auth resolves against
`hermes-dashboard-transport-url' exactly as the chat client does.  The events
path is swapped onto the resolved URL -- the credential is never rebuilt -- and
SINCE and BOARD are appended as query parameters."
  (hermes--promise-map
   (hermes-dashboard-transport--auth-plist-async client)
   (lambda (auth)
     (hermes-dashboard-transport--kanban-events-plist auth since board))))

(cl-defun hermes-dashboard-transport-capability-url-async
    (&key client)
  "Return a promise of the capability provider WebSocket URL plist.
The plist is (:url :redacted-url :secrets).  When CLIENT already has a resolved
WebSocket URL it is reused; otherwise auth resolves against
`hermes-dashboard-transport-url' exactly as the chat client does.  The
capability provider uses the same `/api/ws' JSON-RPC endpoint as chat, so unlike
the kanban events tail no path swap is performed -- only the credential is
reused."
  (hermes-dashboard-transport--auth-plist-async client))

(provide 'hermes-dashboard-api)
;;; hermes-dashboard-api.el ends here
