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

(cl-defun hermes-dashboard-transport--default-http-request-async
    (url &key (method "GET") headers data secrets timeout)
  "Fetch URL with METHOD, HEADERS, and DATA asynchronously using url.el.
Return a promise of the response plist; SECRETS are redacted from any error.
TIMEOUT overrides `hermes-dashboard-transport-http-timeout' when non-nil."
  (let ((safe-url (hermes-dashboard-transport--redact-secret url secrets))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data)
        (request-timeout (or timeout hermes-dashboard-transport-http-timeout))
        (promise (hermes--promise-make))
        timer request-buffer)
    (setq timer (run-at-time
                 request-timeout nil
                 (lambda ()
                   (hermes--promise-reject
                    promise (format "Hermes dashboard request timed out at %s"
                                    safe-url))
                   ;; Also drop the abandoned connection: url.el would keep
                   ;; the process and its buffer alive until its own cleanup.
                   (when (buffer-live-p request-buffer)
                     (kill-buffer request-buffer)))))
    (condition-case err
        (setq request-buffer
              (url-retrieve
               url
               (lambda (status)
                 (cancel-timer timer)
                 (let ((buffer (current-buffer)))
                   (unwind-protect
                       ;; No signal may escape: the timeout timer is already
                       ;; cancelled, so an error here would strand the promise.
                       (condition-case err
                           (hermes-dashboard-transport--settle-http-response
                            promise status buffer safe-url secrets)
                         (error
                          (hermes--promise-reject
                           promise
                           (format "Hermes dashboard response error at %s: %s"
                                   safe-url
                                   (hermes-dashboard-transport--redact-secret
                                    (error-message-string err) secrets)))))
                     (when (buffer-live-p buffer)
                       (kill-buffer buffer)))))
               nil t t))
      (error
       (cancel-timer timer)
       (hermes--promise-reject
        promise (hermes-dashboard-transport--redact-secret
                 (error-message-string err) secrets))))
    promise))

(defvar hermes-dashboard-transport-http-request-function
  #'hermes-dashboard-transport--default-http-request
  "Function used for remote dashboard HTTP requests.
It is called with URL and keyword arguments :method, :headers, :data, and
:secrets, and returns a plist with :status, :headers, and :body.")

(defvar hermes-dashboard-transport-http-request-async-function
  #'hermes-dashboard-transport--default-http-request-async
  "Function used for asynchronous remote dashboard HTTP requests.
Called with URL and keyword arguments :method, :headers, :data, and :secrets.
A caller-specific override adds :timeout.  The function returns a promise of
the response plist.")

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
    (url &key (method "GET") headers body secrets timeout)
  "Request URL as JSON asynchronously using METHOD, HEADERS, BODY, and SECRETS.
Return a promise of the response plist.  TIMEOUT overrides the default."
  (apply hermes-dashboard-transport-http-request-async-function
         url
         (append
          (list :method method
                :headers (append '(("Accept" . "application/json")) headers)
                :data (and body (json-serialize body))
                :secrets secrets)
          (and timeout (list :timeout timeout)))))

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
  (generation 0))

(defcustom hermes-dashboard-transport-remote-auth-method 'auto
  "Authentication method for remote dashboard attach.
`auto' probes /api/status, using a legacy session token when the dashboard is
not gated and username/password login with a WebSocket ticket when a basic
provider is available.  `token' forces the legacy /api/ws?token= path.  `basic'
forces username/password login and a single-use WebSocket ticket."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Legacy session token" token)
                 (const :tag "Basic/password gated auth" basic))
  :group 'hermes-dashboard-transport)

(defun hermes-dashboard-transport--loopback-host-p (host)
  "Return non-nil when HOST names a loopback dashboard bind."
  (member (downcase (or host "")) '("localhost" "127.0.0.1" "::1" "[::1]")))

;;; Client secrets and redacted URLs

(defun hermes-dashboard-transport--client-secrets (client)
  "Return all known secret strings currently associated with CLIENT."
  (hermes-dashboard-transport--secret-list
   (append (list (hermes-dashboard-transport-client-token client))
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
  "Resolve dashboard REST auth for `hermes-dashboard-transport-url'."
  (let ((base-url (hermes-dashboard-transport--api-base-url)))
    (append
     (list :base-url base-url)
     (pcase hermes-dashboard-transport-remote-auth-method
       ('token (hermes-dashboard-transport--api-token-auth base-url))
       ('basic (hermes-dashboard-transport--api-basic-auth
                base-url (hermes-dashboard-transport--remote-status base-url)))
       (_ (let ((status (hermes-dashboard-transport--remote-status base-url)))
            (if (hermes-dashboard-transport--status-auth-required-p status)
                (hermes-dashboard-transport--api-basic-auth base-url status)
              (hermes-dashboard-transport--api-token-auth base-url))))))))

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

(defun hermes-dashboard-transport--remote-status-async (base-url)
  "Return a promise of the /api/status object from dashboard BASE-URL."
  (hermes--promise-map
   (hermes-dashboard-transport--http-json-async
    (hermes-dashboard-transport--api-url base-url "/api/status"))
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
           (_ (hermes--promise-then
               (hermes-dashboard-transport--remote-status-async base-url)
               (lambda (status)
                 (if (hermes-dashboard-transport--status-auth-required-p status)
                     (hermes-dashboard-transport--api-basic-auth-async
                      base-url status)
                   (hermes-dashboard-transport--api-token-auth-async
                    base-url))))))
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
    (method path &key body query headers secrets timeout retry)
  "Return a promise of dashboard REST METHOD PATH using resolved auth.
BODY, QUERY, HEADERS, SECRETS, and TIMEOUT extend the request; RETRY refreshes
auth and retries once when the request fails."
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
                 :secrets secrets :timeout timeout :retry nil))
            (hermes--promise-rejected
	     (hermes-dashboard-transport--redact-secret
	      reason (plist-get request :secrets))))))))))

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
the request.  CLIENT, when it carries a live session token, supplies the
spawned dashboard base URL and `X-Hermes-Session-Token'."
  (if (hermes-dashboard-transport--api-client-token client)
      (hermes-dashboard-transport--api-request-with-client-async
       client method path :body body :query query :headers headers
       :secrets secrets :timeout timeout)
    (hermes-dashboard-transport--api-request-1-async
     method path :body body :query query :headers headers :secrets secrets
     :timeout timeout :retry (equal method "GET"))))

;;; Profile and model caches

(defvar hermes-dashboard-transport--profile-cache nil
  "Cached `/api/profiles' payloads as an alist of (BASE-URL . PAYLOAD).")

(defun hermes-dashboard-transport--cache-base-url (&optional client)
  "Return the normalized endpoint identity for CLIENT or the configured URL."
  (ignore-errors
    (hermes-dashboard-transport--normalize-base-url
     (hermes-dashboard-transport--api-client-base-url client))))

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
           "currently supports basic/password gated dashboards or legacy "
           "session tokens only; OAuth-only remote attach is not implemented")
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
    (host port base-url method &optional token)
  "Return a promise of WebSocket auth for HOST, PORT, BASE-URL, METHOD, and TOKEN.
Mirrors the previous synchronous resolution without blocking: the status probe
and the basic password/ticket exchange resolve through promises."
  (pcase method
    ('token (hermes-dashboard-transport--remote-token-auth-async
             host port base-url token))
    ('basic (hermes--promise-then
             (hermes-dashboard-transport--remote-status-async base-url)
             (lambda (status)
               (hermes-dashboard-transport--remote-basic-auth-async
                host port base-url status))))
    ('auto (hermes--promise-then
            (hermes-dashboard-transport--remote-status-async base-url)
            (lambda (status)
              (if (hermes-dashboard-transport--status-auth-required-p status)
                  (hermes-dashboard-transport--remote-basic-auth-async
                   host port base-url status)
                (hermes-dashboard-transport--remote-token-auth-async
                 host port base-url token)))))
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
  "Return a (:url :redacted-url :secrets) auth plist from live CLIENT, or nil."
  (when-let* ((url (hermes-dashboard-transport-client-websocket-url client)))
    (list :url url
          :redacted-url
          (or (hermes-dashboard-transport-client-redacted-websocket-url client)
              url)
          :secrets (hermes-dashboard-transport-client-secrets client))))

(defun hermes-dashboard-transport--auth-plist-async (client)
  "Return a promise of a WebSocket auth (:url :redacted-url :secrets) plist.
When CLIENT already has a resolved WebSocket URL its credential is reused;
otherwise auth resolves against `hermes-dashboard-transport-url' exactly as
the chat client does."
  (if-let* ((auth (and client
                       (hermes-dashboard-transport--client-auth-plist client))))
      (hermes--promise-resolved auth)
    (let* ((target (hermes-dashboard-transport--parse-url
                    hermes-dashboard-transport-url))
           (host (or (plist-get target :host) "127.0.0.1"))
           (port (plist-get target :port))
           (remote-url (and (not (hermes-dashboard-transport--loopback-host-p host))
                            hermes-dashboard-transport-url))
           (base-url (hermes-dashboard-transport--base-url host port remote-url))
           (method (or hermes-dashboard-transport-remote-auth-method 'auto)))
      (hermes-dashboard-transport--remote-auth-async
       host port base-url method))))

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
