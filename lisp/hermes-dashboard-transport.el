;;; hermes-dashboard-transport.el --- Dashboard transport for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (websocket "1.15"))

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

;; Spawn-owned dashboard/TUI JSON-RPC WebSocket transport skeleton.  This
;; module owns process/WebSocket/request state and emits only normalized
;; `hermes-transport' events to its callback.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'hermes-transport)

(declare-function websocket-open "ext:websocket")
(declare-function websocket-send-text "ext:websocket")
(declare-function websocket-frame-text "ext:websocket")
(declare-function websocket-close "ext:websocket")

(defgroup hermes-dashboard-transport nil
  "Dashboard/TUI transport for Hermes Agent."
  :group 'hermes)

(defcustom hermes-dashboard-transport-command (hermes-transport-default-command)
  "Hermes Agent command used to start the dashboard transport."
  :type 'string
  :group 'hermes-dashboard-transport)

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

(defcustom hermes-dashboard-transport-start-mode 'auto
  "How dashboard transport startup chooses between spawn and remote attach.
`auto' spawns when `hermes-dashboard-transport-url' is a loopback address and
attaches remotely otherwise.  `spawn' always starts a local dashboard process.
`remote' always attaches to an externally managed dashboard."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Spawn local dashboard" spawn)
                 (const :tag "Attach to remote dashboard" remote))
  :group 'hermes-dashboard-transport)

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

(defcustom hermes-dashboard-transport-connect-retries 451
  "Number of attempts to open the dashboard WebSocket after spawning."
  :type 'integer
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-connect-retry-delay 0.1
  "Seconds to wait between dashboard WebSocket connection attempts."
  :type 'number
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-ready-timeout 15
  "Seconds to wait for `gateway.ready' after the WebSocket opens.
Use nil to skip this wait.  The dashboard accepts the WebSocket before the TUI
gateway is ready to process JSON-RPC requests, so callers must not submit the
first request until the ready event arrives."
  :type '(choice (const :tag "Do not wait" nil) number)
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-request-timeout 30
  "Seconds before an unanswered dashboard request is rejected.
A pending JSON-RPC request whose response never arrives would otherwise leak
its callbacks forever, so it is rejected once this many seconds elapse.  Use
nil to disable the per-request timeout."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-ready-wait-interval 0.05
  "Seconds to wait between dashboard `gateway.ready' checks."
  :type 'number
  :group 'hermes-dashboard-transport)

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
  (next-id 0)
  (pending (make-hash-table :test #'equal))
  session-id
  stored-session-id
  (callback #'ignore))

(defun hermes-dashboard-transport--command (host port &optional command)
  "Return dashboard startup argv for HOST, PORT, and optional COMMAND."
  (list (or command hermes-dashboard-transport-command)
        "dashboard" "--no-open" "--tui" "--isolated"
        "--host" host "--port" (number-to-string port)))

(defun hermes-dashboard-transport--env-name (entry)
  "Return ENTRY's environment variable name."
  (car (split-string entry "=" t)))

(defun hermes-dashboard-transport--without-dashboard-env (environment)
  "Return ENVIRONMENT without dashboard transport overrides."
  (cl-remove-if
   (lambda (entry)
     (member (hermes-dashboard-transport--env-name entry)
             '("HERMES_DASHBOARD_SESSION_TOKEN" "HERMES_DASHBOARD_TUI")))
   environment))

(defun hermes-dashboard-transport--environment (token &optional base-environment)
  "Return dashboard process environment with TOKEN injected.
Use BASE-ENVIRONMENT when non-nil, otherwise start from `process-environment'."
  (append (hermes-dashboard-transport--without-dashboard-env
           (or base-environment process-environment))
          (list (concat "HERMES_DASHBOARD_SESSION_TOKEN=" token)
                "HERMES_DASHBOARD_TUI=1")))

(defun hermes-dashboard-transport--loopback-host-p (host)
  "Return non-nil when HOST names a loopback dashboard bind."
  (member (downcase (or host "")) '("localhost" "127.0.0.1" "::1" "[::1]")))

(defun hermes-dashboard-transport--resolved-start-mode
    (mode host remote-url)
  "Return concrete start mode for MODE, HOST, and REMOTE-URL."
  (pcase (or mode hermes-dashboard-transport-start-mode)
    ('spawn 'spawn)
    ('remote 'remote)
    ('auto (if (or (and (stringp remote-url)
                        (not (string-empty-p (string-trim remote-url))))
                   (not (hermes-dashboard-transport--loopback-host-p host)))
               'remote
             'spawn))
    (_ (user-error "Unknown Hermes dashboard start mode: %S" mode))))

(defun hermes-dashboard-transport--host-for-url (host)
  "Return HOST formatted for inclusion in a URL authority."
  (if (and (stringp host)
           (string-match-p ":" host)
           (not (string-prefix-p "[" host)))
      (format "[%s]" host)
    host))

(defun hermes-dashboard-transport--normalize-base-url (base-url)
  "Return normalized dashboard BASE-URL, or nil when BASE-URL is empty."
  (and-let* ((url (and (stringp base-url) (string-trim base-url))))
    (unless (string-empty-p url)
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
      (replace-regexp-in-string "/+\\'" "" url))))

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

(defun hermes-dashboard-transport--websocket-endpoint
    (host port &optional remote-url)
  "Return dashboard WebSocket endpoint from HOST, PORT, or REMOTE-URL."
  (let ((base-url (hermes-dashboard-transport--base-url host port remote-url)))
    (concat (cond
             ((string-prefix-p "https://" base-url)
              (concat "wss://" (substring base-url 8)))
             ((string-prefix-p "http://" base-url)
              (concat "ws://" (substring base-url 7)))
             (t (user-error "Hermes remote dashboard URL must use http or https")))
            "/api/ws")))

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

(defun hermes-dashboard-transport--non-empty-string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun hermes-dashboard-transport--redact-secret (text &optional secrets)
  "Return TEXT with dashboard URL credentials and SECRETS redacted."
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
      (setq message (string-replace secret "<redacted>" message)))
    message))

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

(defun hermes-dashboard-transport--start-event (host port _token)
  "Return a redacted dashboard startup status event for HOST and PORT."
  (list :type 'status
        :event "dashboard.starting"
        :status "starting"
        :content (format "Starting Hermes dashboard on %s:%d" host port)
        :url (hermes-dashboard-transport--redacted-websocket-url host port)))

(defun hermes-dashboard-transport--remote-connect-event (redacted-url)
  "Return remote dashboard connecting status event for REDACTED-URL."
  (list :type 'status
        :event "dashboard.connecting"
        :status "connecting"
        :content (format "Connecting to Hermes dashboard at %s" redacted-url)
        :url redacted-url))

(defun hermes-dashboard-transport--remote-connected-event (redacted-url)
  "Return remote dashboard connected status event for REDACTED-URL."
  (list :type 'status
        :event "dashboard.connected"
        :status "connected"
        :content (format "Connected to Hermes dashboard at %s" redacted-url)
        :url redacted-url))

(defun hermes-dashboard-transport--generate-token ()
  "Return a fresh dashboard session token."
  (secure-hash 'sha256
               (format "%S:%S:%S:%S" (current-time) (emacs-pid)
                       (user-uid) (random))))

(defun hermes-dashboard-transport--pick-port ()
  "Return an available loopback TCP port."
  (let ((server (make-network-process
                 :name "hermes-dashboard-port-probe"
                 :server t
                 :host "127.0.0.1"
                 :service t
                 :noquery t)))
    (unwind-protect
        (cadr (process-contact server))
      (delete-process server))))

(defun hermes-dashboard-transport--plist-remove (plist property)
  "Return PLIST with PROPERTY removed."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key property)
          (setq result (plist-put result key value)))))
    result))

(defun hermes-dashboard-transport--default-make-process (&rest plist)
  "Start a process from PLIST, honoring the private :env key."
  (let ((process-environment (or (plist-get plist :env) process-environment)))
    (apply #'make-process
           (hermes-dashboard-transport--plist-remove plist :env))))

(defun hermes-dashboard-transport--require-websocket ()
  "Load websocket.el or signal a clear user error."
  (unless (require 'websocket nil t)
    (user-error "Install websocket.el to use Hermes dashboard transport")))

(defun hermes-dashboard-transport--redacted-websocket-name (url redacted-url)
  "Return process name for URL redacted as REDACTED-URL."
  (cons (format "websocket to %s" url)
        (format "websocket to %s" redacted-url)))

(defun hermes-dashboard-transport--call-with-redacted-websocket-state
    (url redacted-url thunk)
  "Call THUNK while redacting URL as REDACTED-URL in websocket.el-visible state."
  (let* ((names (hermes-dashboard-transport--redacted-websocket-name
                 url redacted-url))
         (token-name (car names))
         (safe-name (cdr names))
         (make-network-process-function (symbol-function 'make-network-process))
         (open-network-stream-function (symbol-function 'open-network-stream))
         (websocket-inner-create-function (symbol-function 'websocket-inner-create)))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (when (equal (plist-get plist :name) token-name)
                   (setq plist (plist-put plist :name safe-name)))
                 (apply make-network-process-function plist)))
              ((symbol-function 'open-network-stream)
               (lambda (name buffer host service &rest args)
                 (apply open-network-stream-function
                        (if (equal name token-name) safe-name name)
                        buffer host service args)))
              ((symbol-function 'websocket-inner-create)
               (lambda (&rest plist)
                 (when (equal (plist-get plist :url) url)
                   (setq plist (plist-put plist :url redacted-url)))
                 (apply websocket-inner-create-function plist))))
      (funcall thunk))))

(defun hermes-dashboard-transport--mark-websocket-closed (client)
  "Mark CLIENT's WebSocket connection closed."
  (setf (hermes-dashboard-transport-client-websocket client) nil
        (hermes-dashboard-transport-client-ready-p client) nil))

(defun hermes-dashboard-transport--close-websocket (client)
  "Close CLIENT's WebSocket resource and clear its live fields."
  (when-let* ((websocket (hermes-dashboard-transport-client-websocket client)))
    (when (fboundp 'websocket-close)
      (ignore-errors (websocket-close websocket))))
  (hermes-dashboard-transport--mark-websocket-closed client))

(defun hermes-dashboard-transport--delete-process (client)
  "Delete CLIENT's spawned dashboard process and clear the field."
  (when-let* ((process (hermes-dashboard-transport-client-process client)))
    (ignore-errors (delete-process process)))
  (setf (hermes-dashboard-transport-client-process client) nil))

(defun hermes-dashboard-transport--normalized-error-message (client message)
  "Return redacted dashboard error MESSAGE for CLIENT."
  (hermes-dashboard-transport--redact-secret
   (or message "Hermes dashboard request failed")
   (hermes-dashboard-transport--client-secrets client)))

(defun hermes-dashboard-transport--safe-reject (client reject message method)
  "Call REJECT with MESSAGE, reporting callback failures for METHOD on CLIENT."
  (condition-case err
      (funcall reject message)
    (error
     (hermes-dashboard-transport--emit-error
      client
      (format "Hermes dashboard reject callback failed: %s"
              (hermes-dashboard-transport--condition-message client err))
      method))))

(defun hermes-dashboard-transport--reject-pending-request
    (client request message)
  "Reject one pending REQUEST on CLIENT with normalized MESSAGE."
  (let ((method (plist-get request :method))
        (reject (plist-get request :reject)))
    (if reject
        (hermes-dashboard-transport--safe-reject client reject message method)
      (hermes-dashboard-transport--emit-error client message method))))

(defun hermes-dashboard-transport--pending-requests (client)
  "Return CLIENT's pending request plists."
  (let (requests)
    (when-let* ((pending (hermes-dashboard-transport-client-pending client)))
      (maphash (lambda (_id request) (push request requests)) pending))
    (nreverse requests)))

(defun hermes-dashboard-transport--reject-pending-requests
    (client message)
  "Reject and clear every pending request on CLIENT with MESSAGE.
Return non-nil when any pending request had no reject callback and therefore
emitted its own transport error event."
  (let ((message (hermes-dashboard-transport--normalized-error-message
                  client message))
        (pending (hermes-dashboard-transport-client-pending client))
        (requests (hermes-dashboard-transport--pending-requests client))
        emitted-unhandled)
    (when (hash-table-p pending)
      (clrhash pending))
    (dolist (request requests)
      (hermes-dashboard-transport--cancel-request-timer request)
      (unless (plist-get request :reject)
        (setq emitted-unhandled t))
      (hermes-dashboard-transport--reject-pending-request
       client request message))
    emitted-unhandled))

(defun hermes-dashboard-transport-stop (client &optional message)
  "Release CLIENT's dashboard WebSocket, process, and pending requests.
Teardown is best effort: a stale or corrupt CLIENT (for example one left over
from a reload after a struct change) still has its socket and process closed
and its session ended, so the caller can always start a new session.
MESSAGE is reported to pending request reject callbacks, or as a normalized
transport error when a pending request has no reject callback."
  (when (hermes-dashboard-transport-client-p client)
    (ignore-errors
      (hermes-dashboard-transport--reject-pending-requests
       client (or message "Hermes dashboard transport stopped")))
    (ignore-errors
      (setf (hermes-dashboard-transport-client-callback client) #'ignore))
    (ignore-errors (hermes-dashboard-transport--close-websocket client))
    (ignore-errors (hermes-dashboard-transport--delete-process client))
    (ignore-errors
      (setf (hermes-dashboard-transport-client-session-id client) nil
            (hermes-dashboard-transport-client-stored-session-id client) nil))
    client))

(defun hermes-dashboard-transport--default-websocket-open (url client)
  "Open URL for CLIENT using websocket.el."
  (hermes-dashboard-transport--require-websocket)
  (let ((redacted-url
         (hermes-dashboard-transport--client-redacted-websocket-url client)))
    (hermes-dashboard-transport--call-with-redacted-websocket-state
     url redacted-url
     (lambda ()
       (websocket-open
        url
        :on-message (lambda (_websocket frame)
                      (hermes-dashboard-transport--handle-frame
                       client (websocket-frame-text frame)))
        :on-error (lambda (_websocket _type error)
                    (let ((message
                           (format "Hermes dashboard WebSocket error: %s"
                                   (hermes-dashboard-transport--redact-secret
                                    (format "%s" error)
                                    (hermes-dashboard-transport--client-secrets
                                     client)))))
                      (hermes-dashboard-transport--mark-websocket-closed client)
                      (unless (hermes-dashboard-transport--reject-pending-requests
                               client message)
                        (hermes-dashboard-transport--emit-error
                         client message))))
        :on-close (lambda (_websocket)
                    (let ((message "Hermes dashboard WebSocket closed"))
                      (hermes-dashboard-transport--mark-websocket-closed client)
                      (hermes-dashboard-transport--reject-pending-requests
                       client message)
                      (hermes-dashboard-transport--emit-status
                       client "closed" message))))))))

(defun hermes-dashboard-transport--default-websocket-send (websocket text)
  "Send TEXT on WEBSOCKET using websocket.el."
  (hermes-dashboard-transport--require-websocket)
  (websocket-send-text websocket text))

(defvar hermes-dashboard-transport-make-process-function
  #'hermes-dashboard-transport--default-make-process
  "Function used to start the dashboard process.
It is called with a `make-process'-like plist plus private key :env.")

(defvar hermes-dashboard-transport-websocket-open-function
  #'hermes-dashboard-transport--default-websocket-open
  "Function used to open the dashboard WebSocket.
It is called with the tokenized URL and the dashboard client.")

(defvar hermes-dashboard-transport-websocket-send-function
  #'hermes-dashboard-transport--default-websocket-send
  "Function used to send an encoded JSON-RPC frame over WebSocket.")

(defvar hermes-dashboard-transport-sleep-function #'sleep-for
  "Function used to wait between dashboard connection attempts.")

(defun hermes-dashboard-transport--default-ready-wait (_client seconds)
  "Wait SECONDS for dashboard WebSocket input."
  (accept-process-output nil seconds)
  (sit-for 0))

(defvar hermes-dashboard-transport-ready-wait-function
  #'hermes-dashboard-transport--default-ready-wait
  "Function used to wait for dashboard `gateway.ready'.
It receives the dashboard client and a number of seconds.")

(defun hermes-dashboard-transport--json-body (text)
  "Return JSON object parsed from TEXT, or nil for an empty body."
  (unless (string-empty-p (string-trim (or text "")))
    (json-parse-string text
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

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

(cl-defun hermes-dashboard-transport--default-http-request
    (url &key (method "GET") headers data secrets)
  "Fetch URL with METHOD, HEADERS, and DATA using url.el.
SECRETS are redacted from any user-visible error."
  (let ((safe-url (hermes-dashboard-transport--redact-secret url secrets))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data data))
    (let ((buffer (url-retrieve-synchronously url t t 30)))
      (unless buffer
        (user-error "Hermes dashboard request failed at %s" safe-url))
      (unwind-protect
          (let* ((response (hermes-dashboard-transport--parse-http-response-buffer
                            buffer))
                 (status (plist-get response :status)))
            (unless (and status (<= 200 status 299))
              (user-error "Hermes dashboard request failed at %s (HTTP %s)"
                          safe-url (or status "unknown")))
            (plist-put response :body
                       (hermes-dashboard-transport--json-body
                        (plist-get response :body-text))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defvar hermes-dashboard-transport-http-request-function
  #'hermes-dashboard-transport--default-http-request
  "Function used for remote dashboard HTTP requests.
It is called with URL and keyword arguments :method, :headers, :data, and
:secrets, and returns a plist with :status, :headers, and :body.")

(cl-defun hermes-dashboard-transport--http-json
    (url &key (method "GET") headers body secrets)
  "Request URL as JSON using METHOD, HEADERS, BODY, and SECRETS."
  (funcall hermes-dashboard-transport-http-request-function
           url
           :method method
           :headers (append '(("Accept" . "application/json")) headers)
           :data (and body (json-serialize body))
           :secrets secrets))

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
  (or (hermes-dashboard-transport--non-empty-string token)
      (when-let* ((entry (hermes-dashboard-transport--auth-source-entry
                         base-url
                         :user "hermes-dashboard-token"
                         :port "hermes-dashboard-token"
                         :require '(:secret))))
        (hermes-dashboard-transport--non-empty-string
         (hermes-dashboard-transport--auth-source-secret entry)))
      (hermes-dashboard-transport--non-empty-string
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

(defun hermes-dashboard-transport--remote-basic-auth
    (host port base-url status)
  "Return basic-auth plist for HOST, PORT, BASE-URL, and STATUS."
  (let ((provider (hermes-dashboard-transport--status-basic-provider status)))
    (unless provider
      (hermes-dashboard-transport--unsupported-remote-auth base-url))
    (let* ((credentials (hermes-dashboard-transport--remote-basic-credentials
                         base-url))
           (username (plist-get credentials :username))
           (password (plist-get credentials :password))
           (login-response
            (hermes-dashboard-transport--http-json
             (hermes-dashboard-transport--api-url base-url "/auth/password-login")
             :method "POST"
             :headers '(("Content-Type" . "application/json"))
             :body `((provider . ,provider)
                     (username . ,username)
                     (password . ,password)
                     (next . ""))
             :secrets (list password)))
           (cookies (hermes-dashboard-transport--response-cookie-header
                     login-response)))
      (unless cookies
        (user-error "Hermes dashboard basic login did not return session cookies"))
      (let* ((ticket-response
              (hermes-dashboard-transport--http-json
               (hermes-dashboard-transport--api-url base-url
                                                    "/api/auth/ws-ticket")
               :method "POST"
               :headers `(("Cookie" . ,cookies))
               :secrets (list password cookies)))
             (ticket (hermes-transport--scalar-string
                      (hermes-transport--get (plist-get ticket-response :body)
                                             'ticket))))
        (unless (and ticket (not (string-empty-p ticket)))
          (user-error "Hermes dashboard did not return a WebSocket ticket"))
        (list :url (hermes-dashboard-transport--websocket-url
                    host port ticket base-url "ticket")
              :redacted-url (hermes-dashboard-transport--redacted-websocket-url
                             host port base-url "ticket")
              :secrets (list password cookies ticket))))))

(defun hermes-dashboard-transport--remote-auth
    (host port base-url method &optional token)
  "Return auth plist for HOST, PORT, BASE-URL, METHOD, and TOKEN."
  (pcase method
    ('token (hermes-dashboard-transport--remote-token-auth
             host port base-url token))
    ('basic (hermes-dashboard-transport--remote-basic-auth
             host port base-url
             (hermes-dashboard-transport--remote-status base-url)))
    ('auto (let ((status (hermes-dashboard-transport--remote-status base-url)))
             (if (hermes-dashboard-transport--status-auth-required-p status)
                 (hermes-dashboard-transport--remote-basic-auth
                  host port base-url status)
               (hermes-dashboard-transport--remote-token-auth
                host port base-url token))))
    (_ (user-error "Unknown Hermes dashboard remote auth method: %S" method))))

(defun hermes-dashboard-transport--encode-frame (frame)
  "Encode JSON-RPC FRAME as a JSON string."
  (json-serialize frame))

(defun hermes-dashboard-transport--decode-frame (text)
  "Decode JSON-RPC TEXT into an alist frame."
  (if (stringp text)
      (json-parse-string text
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    text))

(defun hermes-dashboard-transport--jsonrpc-request (id method params)
  "Return a JSON-RPC request frame for ID, METHOD, and PARAMS."
  `((jsonrpc . "2.0")
    (id . ,id)
    (method . ,method)
    (params . ,(or params '()))))

(defun hermes-dashboard-transport--next-id (client)
  "Return CLIENT's next monotonically distinct JSON-RPC id."
  (let ((next (1+ (or (hermes-dashboard-transport-client-next-id client) 0))))
    (setf (hermes-dashboard-transport-client-next-id client) next)
    (format "hermes-el-%d" next)))

(defun hermes-dashboard-transport--ensure-pending (client)
  "Return CLIENT's pending request table, creating it if needed."
  (or (hermes-dashboard-transport-client-pending client)
      (setf (hermes-dashboard-transport-client-pending client)
            (make-hash-table :test #'equal))))

(defun hermes-dashboard-transport--send-failure-message
    (client method condition)
  "Return redacted send failure text for CLIENT, METHOD, and CONDITION."
  (hermes-dashboard-transport--normalized-error-message
   client
   (format "Hermes dashboard request %s failed before send: %s"
           method (error-message-string condition))))

(defun hermes-dashboard-transport--cancel-request-timer (request)
  "Cancel the timeout timer stored in pending REQUEST, if any."
  (when-let* ((timer (plist-get request :timer)))
    (cancel-timer timer)))

(defun hermes-dashboard-transport--take-pending (client id)
  "Remove and return CLIENT's pending request ID, cancelling its timer."
  (and-let* ((pending (hermes-dashboard-transport-client-pending client))
             (request (gethash id pending)))
    (remhash id pending)
    (hermes-dashboard-transport--cancel-request-timer request)
    request))

(defun hermes-dashboard-transport--on-request-timeout (client id)
  "Reject CLIENT's pending request ID after its timeout elapses."
  (when-let* ((request (hermes-dashboard-transport--take-pending client id)))
    (hermes-dashboard-transport--reject-pending-request
     client request
     (hermes-dashboard-transport--normalized-error-message
      client
      (format "Hermes dashboard request %s timed out"
              (plist-get request :method))))))

(defun hermes-dashboard-transport--arm-request-timer (client id)
  "Return a timeout timer for CLIENT's request ID, or nil when disabled."
  (and hermes-dashboard-transport-request-timeout
       (run-at-time hermes-dashboard-transport-request-timeout nil
                    #'hermes-dashboard-transport--on-request-timeout
                    client id)))

(defun hermes-dashboard-transport-request (client method &optional params resolve reject)
  "Send METHOD with PARAMS for CLIENT and correlate response callbacks.
RESOLVE is called with the JSON-RPC result.  REJECT is called with the error
message when provided.  Return the request id."
  (let* ((id (hermes-dashboard-transport--next-id client))
         (pending (hermes-dashboard-transport--ensure-pending client))
         (frame (hermes-dashboard-transport--jsonrpc-request id method params))
         (timer (hermes-dashboard-transport--arm-request-timer client id)))
    (puthash id (list :method method :resolve resolve :reject reject :timer timer)
             pending)
    (condition-case err
        (funcall hermes-dashboard-transport-websocket-send-function
                 (hermes-dashboard-transport-client-websocket client)
                 (hermes-dashboard-transport--encode-frame frame))
      (error
       (hermes-dashboard-transport--take-pending client id)
       (hermes-dashboard-transport--reject-pending-request
        client (list :method method :reject reject)
        (hermes-dashboard-transport--send-failure-message
         client method err))))
    id))

(defun hermes-dashboard-transport--alist-without-nil (alist)
  "Return ALIST without nil-valued cells."
  (cl-remove-if (lambda (cell) (null (cdr cell))) alist))

(defun hermes-dashboard-transport--session-param (client session-id)
  "Return explicit SESSION-ID or CLIENT's active session id."
  (or session-id (hermes-dashboard-transport-client-session-id client)))

(cl-defun hermes-dashboard-transport-session-create
    (client &key cols messages title profile cwd resolve reject)
  "Send a `session.create' request for CLIENT.
COLS, MESSAGES, TITLE, PROFILE, and CWD become request parameters.  RESOLVE
and REJECT receive the asynchronous result or error."
  (hermes-dashboard-transport-request
   client "session.create"
   (hermes-dashboard-transport--alist-without-nil
    `((cols . ,cols) (messages . ,messages) (title . ,title)
      (profile . ,profile) (cwd . ,cwd)))
   resolve reject))

(cl-defun hermes-dashboard-transport-session-resume
    (client session-id &key cols profile resolve reject)
  "Send a `session.resume' request for SESSION-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "session.resume"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,session-id) (cols . ,cols) (profile . ,profile)))
   resolve reject))

(cl-defun hermes-dashboard-transport-session-list
    (client &key limit resolve reject)
  "Send a `session.list' request for CLIENT.
LIMIT caps the number of sessions returned.  RESOLVE and REJECT receive the
asynchronous result or error."
  (hermes-dashboard-transport-request
   client "session.list"
   (hermes-dashboard-transport--alist-without-nil `((limit . ,limit)))
   resolve reject))

(cl-defun hermes-dashboard-transport-session-history
    (client session-id &key offset limit resolve reject)
  "Send a `session.history' request for SESSION-ID on CLIENT.
OFFSET and LIMIT page the returned messages."
  (hermes-dashboard-transport-request
   client "session.history"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,session-id) (offset . ,offset) (limit . ,limit)))
   resolve reject))

(cl-defun hermes-dashboard-transport-prompt-submit
    (client text &key session-id resolve reject)
  "Send TEXT through `prompt.submit' on CLIENT."
  (hermes-dashboard-transport-request
   client "prompt.submit"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))
      (text . ,text)))
   resolve reject))

(cl-defun hermes-dashboard-transport-session-interrupt
    (client &key session-id resolve reject)
  "Send `session.interrupt' for CLIENT's SESSION-ID or active session.
RESOLVE and REJECT receive the asynchronous result or error."
  (hermes-dashboard-transport-request
   client "session.interrupt"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))))
   resolve reject))

(cl-defun hermes-dashboard-transport-session-steer
    (client text &key session-id resolve reject)
  "Send TEXT through `session.steer' for CLIENT's active session."
  (hermes-dashboard-transport-request
   client "session.steer"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))
      (text . ,text)))
   resolve reject))

(cl-defun hermes-dashboard-transport-commands-catalog
    (client &key resolve reject)
  "Request the dashboard `commands.catalog' for CLIENT.
RESOLVE and REJECT receive the asynchronous result or error."
  (hermes-dashboard-transport-request
   client "commands.catalog" nil resolve reject))

(cl-defun hermes-dashboard-transport-command-dispatch
    (client name arg &key session-id resolve reject)
  "Dispatch slash command NAME with ARG through CLIENT's `command.dispatch'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  (hermes-dashboard-transport-request
   client "command.dispatch"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))
      (name . ,name)
      (arg . ,arg)))
   resolve reject))

(cl-defun hermes-dashboard-transport-slash-exec
    (client command &key session-id resolve reject)
  "Run COMMAND through CLIENT's dashboard `slash.exec'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  (hermes-dashboard-transport-request
   client "slash.exec"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))
      (command . ,command)))
   resolve reject))

(cl-defun hermes-dashboard-transport-approval-respond
    (client &key session-id choice all resolve reject)
  "Send an `approval.respond' CHOICE for CLIENT.
SESSION-ID selects the live dashboard session.  ALL applies CHOICE broadly when
non-nil.  RESOLVE and REJECT receive the asynchronous result or error."
  (hermes-dashboard-transport-request
   client "approval.respond"
   (hermes-dashboard-transport--alist-without-nil
    `((session_id . ,(hermes-dashboard-transport--session-param client session-id))
      (choice . ,choice) (all . ,all)))
   resolve reject))

(defun hermes-dashboard-transport-clarify-respond
    (client request-id answer &optional resolve reject)
  "Send ANSWER for clarify REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "clarify.respond" `((request_id . ,request-id) (answer . ,answer))
   resolve reject))

(defun hermes-dashboard-transport-sudo-respond
    (client request-id password &optional resolve reject)
  "Send PASSWORD for sudo REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "sudo.respond" `((request_id . ,request-id) (password . ,password))
   resolve reject))

(defun hermes-dashboard-transport-secret-respond
    (client request-id value &optional resolve reject)
  "Send VALUE for secret REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "secret.respond" `((request_id . ,request-id) (value . ,value))
   resolve reject))

(defun hermes-dashboard-transport--start-process (_client command env)
  "Start dashboard process using COMMAND and ENV."
  (funcall hermes-dashboard-transport-make-process-function
           :name "hermes-dashboard"
           :buffer " *hermes-dashboard*"
           :command command
           :env env
           :connection-type 'pipe
           :noquery t
           :sentinel (lambda (_process _event))))

(defun hermes-dashboard-transport--connection-error (client)
  "Return a redacted connection failure message for CLIENT."
  (format "Hermes dashboard WebSocket did not become ready at %s"
          (hermes-dashboard-transport--client-redacted-websocket-url client)))

(defun hermes-dashboard-transport--connection-error-message
    (client &optional condition)
  "Return a redacted connection error message for CLIENT and CONDITION."
  (let ((message (hermes-dashboard-transport--connection-error client)))
    (if condition
        (format "%s (%s)"
                message
                (hermes-dashboard-transport--condition-message client condition))
      message)))

(defun hermes-dashboard-transport--open-websocket-once (client url)
  "Open CLIENT's WebSocket at URL once."
  (funcall hermes-dashboard-transport-websocket-open-function url client))

(defun hermes-dashboard-transport--open-websocket-with-retries (client url)
  "Open CLIENT's WebSocket at URL, retrying dashboard cold-start races."
  (let ((attempts (max 1 hermes-dashboard-transport-connect-retries))
        last-error)
    (catch 'connected
      (dotimes (attempt attempts)
        (condition-case err
            (throw 'connected
                   (hermes-dashboard-transport--open-websocket-once
                    client url))
          (user-error
           (signal 'user-error
                   (list (hermes-dashboard-transport--condition-message
                          client err))))
          (error
           (setq last-error err)
           (when (< (1+ attempt) attempts)
             (funcall hermes-dashboard-transport-sleep-function
                      hermes-dashboard-transport-connect-retry-delay)))))
      (let ((message (hermes-dashboard-transport--connection-error-message
                      client last-error)))
        (hermes-dashboard-transport--emit-error client message)
        (signal 'user-error (list message))))))

(defun hermes-dashboard-transport-connect (client)
  "Open CLIENT's dashboard WebSocket and return the WebSocket object."
  (let ((websocket (hermes-dashboard-transport--open-websocket-with-retries
                    client
                    (hermes-dashboard-transport--client-websocket-url client))))
    (setf (hermes-dashboard-transport-client-websocket client) websocket)
    websocket))

(defun hermes-dashboard-transport--ready-timeout-error (client)
  "Return a redacted `gateway.ready' timeout message for CLIENT."
  (format "Hermes dashboard did not become ready at %s"
          (hermes-dashboard-transport--client-redacted-websocket-url client)))

(defun hermes-dashboard-transport--await-ready (client)
  "Wait for CLIENT to receive `gateway.ready', or signal `user-error'."
  (when hermes-dashboard-transport-ready-timeout
    (let ((deadline (+ (float-time) hermes-dashboard-transport-ready-timeout)))
      (while (and (not (hermes-dashboard-transport-client-ready-p client))
                  (< (float-time) deadline))
        (funcall hermes-dashboard-transport-ready-wait-function
                 client hermes-dashboard-transport-ready-wait-interval))
      (unless (hermes-dashboard-transport-client-ready-p client)
        (let ((message (hermes-dashboard-transport--ready-timeout-error client)))
          (hermes-dashboard-transport--emit-error client message)
          (signal 'user-error (list message)))))))

(defun hermes-dashboard-transport--cleanup-start-failure (client)
  "Release CLIENT resources after a failed dashboard start."
  (hermes-dashboard-transport-stop
   client "Hermes dashboard transport stopped during startup"))

(cl-defun hermes-dashboard-transport--start-spawn
    (&key callback host port command token base-environment)
  "Start spawn-owned dashboard with CALLBACK and override settings.
HOST, PORT, COMMAND, TOKEN, and BASE-ENVIRONMENT override defaults."
  (let* ((host (or host "127.0.0.1"))
         (port (or port (hermes-dashboard-transport--pick-port)))
         (token (or token (hermes-dashboard-transport--generate-token)))
         (client (make-hermes-dashboard-transport-client
                  :host host :port port :token token
                  :callback (or callback #'ignore)))
         (argv (hermes-dashboard-transport--command host port command))
         (env (hermes-dashboard-transport--environment token base-environment)))
    (funcall (hermes-dashboard-transport-client-callback client)
             (hermes-dashboard-transport--start-event host port token))
    (condition-case err
        (progn
          (setf (hermes-dashboard-transport-client-process client)
                (hermes-dashboard-transport--start-process client argv env))
          (hermes-dashboard-transport-connect client)
          (hermes-dashboard-transport--await-ready client)
          client)
      (error
       (hermes-dashboard-transport--cleanup-start-failure client)
       (signal 'user-error
               (list (hermes-dashboard-transport--condition-message
                      client err)))))))

(cl-defun hermes-dashboard-transport--start-remote
    (&key callback host port token remote-url remote-auth-method)
  "Attach to a remote dashboard with CALLBACK and override settings.
HOST, PORT, TOKEN, REMOTE-URL, and REMOTE-AUTH-METHOD override defaults."
  (let* ((host (or host "127.0.0.1"))
         (base-url (hermes-dashboard-transport--base-url host port remote-url))
         (auth (hermes-dashboard-transport--remote-auth
                host port base-url
                (or remote-auth-method
                    hermes-dashboard-transport-remote-auth-method)
                token))
         (client (make-hermes-dashboard-transport-client
                  :host host
                  :port port
                  :token (plist-get auth :token)
                  :base-url base-url
                  :websocket-url (plist-get auth :url)
                  :redacted-websocket-url (plist-get auth :redacted-url)
                  :secrets (plist-get auth :secrets)
                  :callback (or callback #'ignore))))
    (funcall (hermes-dashboard-transport-client-callback client)
             (hermes-dashboard-transport--remote-connect-event
              (plist-get auth :redacted-url)))
    (condition-case err
        (progn
          (hermes-dashboard-transport-connect client)
          (hermes-dashboard-transport--await-ready client)
          (funcall (hermes-dashboard-transport-client-callback client)
                   (hermes-dashboard-transport--remote-connected-event
                    (plist-get auth :redacted-url)))
          client)
      (error
       (hermes-dashboard-transport--cleanup-start-failure client)
       (signal 'user-error
               (list (hermes-dashboard-transport--condition-message
                      client err)))))))

(cl-defun hermes-dashboard-transport-start
    (&key callback host port command token base-environment
          start-mode remote-url remote-auth-method)
  "Start or attach to a dashboard transport and connect its WebSocket.
CALLBACK receives normalized `hermes-transport' events.  By default the target
is `hermes-dashboard-transport-url'; HOST, PORT, COMMAND, TOKEN,
BASE-ENVIRONMENT, START-MODE, REMOTE-URL, and REMOTE-AUTH-METHOD override it."
  (let* ((from-url (not (or host port remote-url)))
         (target (and from-url
                      (hermes-dashboard-transport--parse-url
                       hermes-dashboard-transport-url)))
         (host (or host (plist-get target :host)))
         (port (or port (plist-get target :port)))
         (remote-url (or remote-url
                         (and from-url
                              (not (hermes-dashboard-transport--loopback-host-p host))
                              hermes-dashboard-transport-url)))
         (mode (hermes-dashboard-transport--resolved-start-mode
                start-mode host remote-url)))
    (pcase mode
      ('spawn (hermes-dashboard-transport--start-spawn
               :callback callback :host host :port port :command command
               :token token :base-environment base-environment))
      ('remote (hermes-dashboard-transport--start-remote
                :callback callback :host host :port port :token token
                :remote-url remote-url
                :remote-auth-method remote-auth-method)))))

(defun hermes-dashboard-transport--emit-status (client status content)
  "Emit a status event with STATUS and CONTENT for CLIENT."
  (funcall (hermes-dashboard-transport-client-callback client)
           (list :type 'status :status status :content content)))

(defun hermes-dashboard-transport--emit-error (client message &optional method code)
  "Emit a normalized dashboard error MESSAGE for CLIENT."
  (let ((event (list :type 'error :event "jsonrpc.error" :content message)))
    (when method
      (setq event (plist-put event :method method)))
    (when code
      (setq event (plist-put event :code code)))
    (funcall (hermes-dashboard-transport-client-callback client) event)))

(defun hermes-dashboard-transport--frame-id (frame)
  "Return FRAME's JSON-RPC id as a string, or nil."
  (hermes-transport--scalar-string (hermes-transport--get frame 'id)))

(defun hermes-dashboard-transport--frame-kind (frame)
  "Return FRAME kind: response, error-response, event, or unknown."
  (cond
   ((and (hermes-dashboard-transport--frame-id frame)
         (hermes-transport--get frame 'error))
    'error-response)
   ((hermes-dashboard-transport--frame-id frame) 'response)
   ((equal (hermes-transport--get frame 'method) "event") 'event)
   (t 'unknown)))

(defun hermes-dashboard-transport--response-error-message (frame)
  "Return JSON-RPC error message from FRAME."
  (let ((error (hermes-transport--get frame 'error)))
    (or (and (hermes-transport--object-p error)
             (hermes-transport--scalar-string
              (hermes-transport--get error 'message)))
        (hermes-transport--scalar-string error)
        "Hermes dashboard request failed")))

(defun hermes-dashboard-transport--response-error-code (frame)
  "Return JSON-RPC error code from FRAME, if present."
  (let ((error (hermes-transport--get frame 'error)))
    (and (hermes-transport--object-p error)
         (hermes-transport--get error 'code))))

(defun hermes-dashboard-transport--store-session-result (client method result)
  "Store CLIENT session identifiers from METHOD RESULT when present."
  (when (member method '("session.create" "session.resume"))
    (let ((session-id (hermes-transport--get result 'session_id)))
      (when session-id
        (setf (hermes-dashboard-transport-client-session-id client) session-id))
      (when-let* ((stored-id (or (hermes-transport--get result 'stored_session_id)
                                (hermes-transport--get result 'resumed)
                                (hermes-transport--get result 'session_key)
                                (and (equal method "session.create")
                                     session-id))))
        (setf (hermes-dashboard-transport-client-stored-session-id client)
              stored-id)))))

(defun hermes-dashboard-transport--resolve-response (client frame)
  "Resolve CLIENT's pending request represented by response FRAME."
  (let* ((id (hermes-dashboard-transport--frame-id frame))
         (pending (and id (hermes-dashboard-transport--take-pending client id)))
         (method (plist-get pending :method)))
    (when pending
      (let ((result (hermes-transport--get frame 'result))
            (resolve (plist-get pending :resolve)))
        (hermes-dashboard-transport--store-session-result client method result)
        (when resolve
          (funcall resolve result))))))

(defun hermes-dashboard-transport--reject-response (client frame)
  "Reject CLIENT's pending request represented by error response FRAME."
  (let* ((id (hermes-dashboard-transport--frame-id frame))
         (pending (and id (hermes-dashboard-transport--take-pending client id)))
         (method (plist-get pending :method))
         (message (hermes-dashboard-transport--response-error-message frame))
         (code (hermes-dashboard-transport--response-error-code frame))
         handled)
    (when pending
      (when-let* ((reject (plist-get pending :reject)))
        (setq handled t)
        (funcall reject message)))
    (unless handled
      (hermes-dashboard-transport--emit-error client message method code))))

(defun hermes-dashboard-transport--payload-text (payload)
  "Return PAYLOAD's best display text, or nil."
  (hermes-transport--scalar-string
   (hermes-transport--get-any payload
                              '(text rendered content delta message context
                                question prompt description command env_var
                                summary result_text result preview))))

(defun hermes-dashboard-transport--event-base (type params payload)
  "Return base event plist for TYPE, PARAMS, and PAYLOAD."
  (let ((event (list :event type))
        (session-id (hermes-transport--get params 'session_id)))
    (when session-id
      (setq event (plist-put event :session-id session-id)))
    (when-let* ((request-id (hermes-transport--get payload 'request_id)))
      (setq event (plist-put event :request-id request-id)))
    event))

(defun hermes-dashboard-transport--status-event (type params payload status content)
  "Return a status event for TYPE, PARAMS, PAYLOAD, STATUS, and CONTENT."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'status)))
    (when status
      (setq event (plist-put event :status status)))
    (when content
      (setq event (plist-put event :content content)))
    event))

(defun hermes-dashboard-transport--tool-event (type params payload status)
  "Return a tool event for TYPE, PARAMS, PAYLOAD, and STATUS."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type 'tool))
        (preview (hermes-dashboard-transport--payload-text payload)))
    (dolist (field '((tool_id . :tool-call-id) (name . :name)
                     (args . :args) (args_text . :args)
                     (context . :context) (summary . :summary)
                     (result_text . :result-text) (result . :result)
                     (duration_s . :duration) (duration . :duration)))
      (when-let* ((value (hermes-transport--get payload (car field))))
        (setq event (plist-put event (cdr field) value))))
    (when preview
      (setq event (plist-put event :preview preview)))
    (plist-put event :status status)))

(defun hermes-dashboard-transport--inline-diff-event (type params payload)
  "Return a normalized inline diff event for TYPE/PARAMS/PAYLOAD, if any."
  (when-let* ((content (hermes-transport--scalar-string
                       (hermes-transport--get payload 'inline_diff))))
    (plist-put
     (plist-put (hermes-dashboard-transport--event-base type params payload)
                :type 'diff)
     :content content)))

(defun hermes-dashboard-transport--tool-complete-events (type params payload)
  "Return normalized `tool.complete' events for TYPE/PARAMS/PAYLOAD."
  (let ((events (list (hermes-dashboard-transport--tool-event
                       type params payload "completed"))))
    (if-let* ((diff (hermes-dashboard-transport--inline-diff-event
                    type params payload)))
        (append events (list diff))
      events)))

(defun hermes-dashboard-transport--payload-event (type params payload kind)
  "Return a single transport event of KIND for TYPE/PARAMS/PAYLOAD."
  (let ((event (plist-put
                (hermes-dashboard-transport--event-base type params payload)
                :type kind))
        (content (hermes-dashboard-transport--payload-text payload)))
    (if content
        (plist-put event :content content)
      event)))

(defun hermes-dashboard-transport--message-complete-kind (payload)
  "Return the transport kind for a `message.complete' PAYLOAD."
  (let ((status (downcase
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get payload 'status))
                     "complete"))))
    (if (member status '("complete" "completed" "done" "success" "ok"))
        'done
      'error)))

(defun hermes-dashboard-transport--message-complete-event (type params payload)
  "Return a normalized `message.complete' event for TYPE/PARAMS/PAYLOAD."
  (let* ((status (hermes-transport--scalar-string
                  (hermes-transport--get payload 'status)))
         (event (hermes-dashboard-transport--payload-event
                 type params payload
                 (hermes-dashboard-transport--message-complete-kind payload))))
    (if status
        (plist-put event :status status)
      event)))

(defun hermes-dashboard-transport--prompt-title (prompt-type)
  "Return human title for PROMPT-TYPE."
  (pcase prompt-type
    ("approval" "Approval requested")
    ("clarify" "Clarification requested")
    ("sudo" "Sudo password requested")
    ("secret" "Secret requested")
    (_ (format "%s requested" prompt-type))))

(defun hermes-dashboard-transport--prompt-content (prompt-type payload)
  "Return redacted display content for PROMPT-TYPE and PAYLOAD."
  (let ((title (hermes-dashboard-transport--prompt-title prompt-type)))
    (pcase prompt-type
      ("approval"
       (string-join
        (delq nil (list title
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'description))
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'command))))
        ": "))
      ("secret"
       (string-join
        (delq nil (list title
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'prompt))
                        (hermes-transport--scalar-string
                         (hermes-transport--get payload 'env_var))))
        ": "))
      (_
       (or (hermes-dashboard-transport--payload-text payload) title)))))

(defun hermes-dashboard-transport--copy-prompt-fields (event payload)
  "Copy safe prompt request fields from PAYLOAD into EVENT."
  (dolist (field '((question . :question) (choices . :choices)
                   (prompt . :prompt) (env_var . :env-var)
                   (command . :command) (description . :description)
                   (pattern_key . :pattern-key)
                   (pattern_keys . :pattern-keys)))
    (when-let* ((value (hermes-transport--get payload (car field))))
      (setq event (plist-put event (cdr field) value))))
  event)

(defun hermes-dashboard-transport--prompt-request-event (type params payload)
  "Return a redacted prompt request status event for TYPE/PARAMS/PAYLOAD."
  (let* ((prompt-type (car (split-string type "\\." t)))
         (event (hermes-dashboard-transport--status-event
                 type params payload "requested"
                 (hermes-dashboard-transport--prompt-content
                  prompt-type payload))))
    (setq event (plist-put event :prompt-type prompt-type))
    (setq event (plist-put event :prompt-request-p t))
    (hermes-dashboard-transport--copy-prompt-fields event payload)))

(defun hermes-dashboard-transport--payload-object (payload)
  "Return PAYLOAD as an object suitable for normalization."
  (cond
   ((hermes-transport--object-p payload) payload)
   ((null payload) nil)
   (t `((content . ,payload)))))

(defun hermes-dashboard-transport--session-info-content (payload)
  "Return a compact display string for a `session.info' PAYLOAD."
  (let ((model (hermes-transport--scalar-string
                (hermes-transport--get payload 'model)))
        (provider (hermes-transport--scalar-string
                   (hermes-transport--get payload 'provider)))
        (warning (hermes-transport--scalar-string
                  (hermes-transport--get payload 'config_warning))))
    (string-join
     (delq nil
           (list (cond
                  ((and model provider)
                   (format "Session ready: %s via %s" model provider))
                  (model (format "Session ready: %s" model))
                  (provider (format "Session ready via %s" provider))
                  (t "Session ready"))
                 warning))
     " — ")))

(defun hermes-dashboard-transport--session-info-event (type params payload)
  "Return a normalized `session.info' status event for TYPE/PARAMS/PAYLOAD."
  (hermes-dashboard-transport--status-event
   type params payload "ready"
   (hermes-dashboard-transport--session-info-content payload)))

(defun hermes-dashboard-transport--generic-event (type params payload)
  "Return generic normalized event for TYPE/PARAMS/PAYLOAD."
  (let* ((object (or (hermes-dashboard-transport--payload-object payload) '()))
         (session-id (hermes-transport--get params 'session_id))
         (raw (if session-id
                  (append object `((session_id . ,session-id)))
                object)))
    (list (hermes-transport-normalize-event raw type))))

(defun hermes-dashboard-transport--normalize-event-frame (frame)
  "Return normalized transport events for JSON-RPC event FRAME."
  (let* ((params (hermes-transport--get frame 'params))
         (type (hermes-transport--scalar-string
                (hermes-transport--get params 'type)))
         (payload (or (hermes-transport--get params 'payload) '())))
    (pcase type
      ("gateway.ready"
       (list (hermes-dashboard-transport--status-event
              type params payload "ready" "Hermes dashboard connected")))
      ("session.info"
       (list (hermes-dashboard-transport--session-info-event
              type params payload)))
      ("message.delta"
       (list (hermes-dashboard-transport--payload-event type params payload 'delta)))
      ("message.complete"
       (list (hermes-dashboard-transport--message-complete-event
              type params payload)))
      ("error"
       (list (hermes-dashboard-transport--payload-event type params payload 'error)))
      ("status.update"
       (let ((status (hermes-transport--scalar-string
                      (or (hermes-transport--get payload 'kind)
                          (hermes-transport--get payload 'status)))))
         (list (hermes-dashboard-transport--status-event
                type params payload status
                (hermes-dashboard-transport--payload-text payload)))))
      ("tool.start"
       (list (hermes-dashboard-transport--tool-event
              type params payload "running")))
      ("tool.complete"
       (hermes-dashboard-transport--tool-complete-events
        type params payload))
      ((or "reasoning.delta" "thinking.delta")
       (list (hermes-dashboard-transport--payload-event
              type params payload 'commentary)))
      ((or "approval.request" "clarify.request" "sudo.request" "secret.request")
       (list (hermes-dashboard-transport--prompt-request-event
              type params payload)))
      (_
       (if (and type (string-prefix-p "notification." type))
           (list (hermes-dashboard-transport--status-event
                  type params payload "notification"
                  (hermes-dashboard-transport--payload-text payload)))
         (hermes-dashboard-transport--generic-event type params payload))))))

(defun hermes-dashboard-transport--handle-event-frame (client frame)
  "Dispatch JSON-RPC event FRAME to CLIENT's callback."
  (let ((params (hermes-transport--get frame 'params)))
    (when (equal (hermes-transport--get params 'type) "gateway.ready")
      (setf (hermes-dashboard-transport-client-ready-p client) t))
    (dolist (event (hermes-dashboard-transport--normalize-event-frame frame))
      (funcall (hermes-dashboard-transport-client-callback client) event))))

(defun hermes-dashboard-transport--handle-frame (client text)
  "Handle inbound JSON-RPC TEXT or frame alist for CLIENT."
  (condition-case err
      (let ((frame (hermes-dashboard-transport--decode-frame text)))
        (pcase (hermes-dashboard-transport--frame-kind frame)
          ('response (hermes-dashboard-transport--resolve-response client frame))
          ('error-response (hermes-dashboard-transport--reject-response client frame))
          ('event (hermes-dashboard-transport--handle-event-frame client frame))
          (_ (hermes-dashboard-transport--emit-error
              client "Unknown Hermes dashboard frame"))))
    (error
     (hermes-dashboard-transport--emit-error
      client
      (format "Invalid Hermes dashboard frame: %s"
              (hermes-dashboard-transport--condition-message client err))))))

(provide 'hermes-dashboard-transport)
;;; hermes-dashboard-transport.el ends here
