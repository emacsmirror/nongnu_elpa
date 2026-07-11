;;; hermes-dashboard-api.el --- HTTP and URL layer for the Hermes dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; The client-struct-free bottom of the dashboard stack, split out of
;; `hermes-dashboard-transport' (which requires this file): the dashboard
;; URL options, URL/WebSocket endpoint builders, secret redaction on plain
;; strings, and the promise-based url.el HTTP plumbing.  Symbol names keep
;; the `hermes-dashboard-transport-' prefix so callers were unaffected by
;; the move.  The REST API helpers, credential lookup, and caches still
;; live in the transport; they migrate here as they shed their client
;; struct dependencies.

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
SECRETS are redacted from any error message."
  (if-let* ((error-data (plist-get status :error)))
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
    (url &key (method "GET") headers data secrets)
  "Fetch URL with METHOD, HEADERS, and DATA asynchronously using url.el.
Return a promise of the response plist; SECRETS are redacted from any error."
  (let ((safe-url (hermes-dashboard-transport--redact-secret url secrets))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data)
        (promise (hermes--promise-make))
        timer request-buffer)
    (setq timer (run-at-time
                 hermes-dashboard-transport-http-timeout nil
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
Called like `hermes-dashboard-transport-http-request-function' but returns a
promise of the response plist instead of blocking.")

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
    (url &key (method "GET") headers body secrets)
  "Request URL as JSON asynchronously using METHOD, HEADERS, BODY, and SECRETS.
Return a promise of the response plist."
  (funcall hermes-dashboard-transport-http-request-async-function
           url
           :method method
           :headers (append '(("Accept" . "application/json")) headers)
           :data (and body (json-serialize body))
           :secrets secrets))

(defun hermes-dashboard-transport--http-json-request (request)
  "Send REQUEST, a (:url :method :headers :body :secrets) plist, synchronously."
  (hermes-dashboard-transport--http-json
   (plist-get request :url)
   :method (plist-get request :method)
   :headers (plist-get request :headers)
   :body (plist-get request :body)
   :secrets (plist-get request :secrets)))

(defun hermes-dashboard-transport--http-json-request-async (request)
  "Send REQUEST, a (:url :method :headers :body :secrets) plist, asynchronously.
Return a promise of the response plist."
  (hermes-dashboard-transport--http-json-async
   (plist-get request :url)
   :method (plist-get request :method)
   :headers (plist-get request :headers)
   :body (plist-get request :body)
   :secrets (plist-get request :secrets)))



(provide 'hermes-dashboard-api)
;;; hermes-dashboard-api.el ends here
