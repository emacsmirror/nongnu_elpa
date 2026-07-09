;;; hermes-dashboard-transport.el --- Dashboard transport for Hermes  -*- lexical-binding: t; -*-

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
(require 'hermes-promise)

(declare-function websocket-open "ext:websocket")
(declare-function websocket-send-text "ext:websocket")
(declare-function websocket-send "ext:websocket")
(declare-function make-websocket-frame "ext:websocket")
(declare-function websocket-frame-text "ext:websocket")
(declare-function websocket-close "ext:websocket")

(defgroup hermes-dashboard-transport nil
  "Dashboard/TUI transport for Hermes Agent."
  :group 'hermes)

;;; Options

(defcustom hermes-dashboard-transport-command "hermes"
  "Hermes Agent command used to start the dashboard transport.
Resolved at spawn time: a bare name is searched on variable `exec-path',
with ~/.local/bin/hermes as a fallback."
  :type 'string)

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
  :type 'string)

(defcustom hermes-dashboard-transport-start-mode 'auto
  "How dashboard transport startup chooses between spawn and remote attach.
`auto' spawns when `hermes-dashboard-transport-url' is a loopback address and
attaches remotely otherwise.  `spawn' always starts a local dashboard process.
`remote' always attaches to an externally managed dashboard."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Spawn local dashboard" spawn)
                 (const :tag "Attach to remote dashboard" remote)))

(defcustom hermes-dashboard-transport-remote-auth-method 'auto
  "Authentication method for remote dashboard attach.
`auto' probes /api/status, using a legacy session token when the dashboard is
not gated and username/password login with a WebSocket ticket when a basic
provider is available.  `token' forces the legacy /api/ws?token= path.  `basic'
forces username/password login and a single-use WebSocket ticket."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Legacy session token" token)
                 (const :tag "Basic/password gated auth" basic)))

(defcustom hermes-dashboard-transport-connect-retries 100
  "Attempts to open the dashboard WebSocket while a spawn cold-starts.
Retries are scheduled asynchronously, so this budget never blocks Emacs; it
only bounds how long a never-arriving dashboard is tried before its readiness
fails.  With the default `hermes-dashboard-transport-connect-retry-delay' of
0.1s this is a 10-second window, ample for a local dashboard to bind its port
while surfacing a dead dashboard far sooner than the old 45-second budget."
  :type 'integer)

(defcustom hermes-dashboard-transport-connect-retry-delay 0.1
  "Seconds to wait between dashboard WebSocket connection attempts."
  :type 'number)

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

(defcustom hermes-dashboard-transport-http-timeout 30
  "Seconds before a dashboard REST/HTTP request gives up.
Bounds both the synchronous fallback and the asynchronous request path so a
slow or unreachable dashboard cannot hang a chat or list buffer forever."
  :type 'number
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-idle-close-delay nil
  "Seconds to keep a shared dashboard client alive after its last reference.
When the last chat buffer detaches, the shared WebSocket and any spawned
dashboard are kept warm for this many seconds so reopening a chat reuses the
connection instead of reconnecting and re-authenticating.  nil closes the
client immediately on the last release."
  :type '(choice (const :tag "Close immediately" nil) number)
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-heartbeat-interval nil
  "Seconds between keepalive pings on the shared dashboard WebSocket.
websocket.el answers server pings but never initiates one, so a long-lived
shared socket can be dropped by an idle proxy or load balancer.  A number sends
a WebSocket ping frame this often once the gateway is ready; nil disables the
heartbeat (a dropped socket is then rebuilt lazily on the next request)."
  :type '(choice (const :tag "No heartbeat" nil) number)
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-reconnect-max-attempts 10
  "How many times to reconnect the shared socket after an unexpected close.
While at least one chat buffer is attached, a dropped socket is reopened with
exponential backoff up to this many times before the client is given up and torn
down.  nil or 0 disables proactive reconnect; a dropped socket is then rebuilt
lazily on the next request instead."
  :type '(choice (const :tag "No proactive reconnect" nil) integer)
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-reconnect-base-delay 1.0
  "Seconds before the first shared-socket reconnect attempt.
Each further attempt doubles the delay up to
`hermes-dashboard-transport-reconnect-max-delay'."
  :type 'number
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-reconnect-max-delay 30
  "Maximum backoff in seconds between shared-socket reconnect attempts."
  :type 'number
  :group 'hermes-dashboard-transport)

;;; Client struct and subscribers

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

(defun hermes-dashboard-transport-subscribe (client fn)
  "Register FN as an event subscriber on CLIENT and return an opaque token.
A new subscriber owns no session, so it receives broadcast events -- untagged
connection-level events and tagged events with no owner -- until
`hermes-dashboard-transport-subscribe-session' binds the token to a live session
id."
  (let ((token (gensym "hermes-dashboard-sub-")))
    (puthash token (list :fn fn :session-id nil)
             (hermes-dashboard-transport-client-subscribers client))
    token))

(defun hermes-dashboard-transport-subscribe-session (client token session-id)
  "Bind subscriber TOKEN on CLIENT to live SESSION-ID.
Events tagged with SESSION-ID then route to TOKEN's function alone; other tokens
stop receiving them.  Re-binding moves TOKEN to the new SESSION-ID."
  (when-let* ((record (gethash token
                               (hermes-dashboard-transport-client-subscribers
                                client))))
    (let ((index (hermes-dashboard-transport-client-session-index client)))
      (when-let* ((previous (plist-get record :session-id)))
        (when (eq (gethash previous index) token)
          (remhash previous index)))
      (plist-put record :session-id session-id)
      (when session-id
        (puthash session-id token index)))))

(defun hermes-dashboard-transport-unsubscribe (client token)
  "Remove subscriber TOKEN from CLIENT."
  (let ((subscribers (hermes-dashboard-transport-client-subscribers client))
        (index (hermes-dashboard-transport-client-session-index client)))
    (when-let* ((record (gethash token subscribers)))
      (when-let* ((session-id (plist-get record :session-id)))
        (when (eq (gethash session-id index) token)
          (remhash session-id index)))
      (remhash token subscribers))))

(defun hermes-dashboard-transport-set-subscriber-fn (client token fn)
  "Replace subscriber TOKEN's function with FN on CLIENT, keeping its session.
Return TOKEN on success, or nil when TOKEN is not registered on CLIENT."
  (when-let* ((record (gethash token
                               (hermes-dashboard-transport-client-subscribers
                                client))))
    (plist-put record :fn fn)
    token))

(defun hermes-dashboard-transport--event-session-id (event)
  "Return EVENT's session id, or nil."
  (and (listp event) (plist-get event :session-id)))

(defun hermes-dashboard-transport--session-subscriber-fn (client session-id)
  "Return the subscriber function bound to SESSION-ID on CLIENT, or nil."
  (when-let* ((token (gethash session-id
                              (hermes-dashboard-transport-client-session-index
                               client))))
    (plist-get (gethash token
                         (hermes-dashboard-transport-client-subscribers client))
               :fn)))

(defun hermes-dashboard-transport--deliver (fn event)
  "Call subscriber FN with EVENT, demoting any error so delivery continues.
A throwing subscriber must not starve the other buffers sharing the socket or
perturb shared transport state from inside a status broadcast."
  (with-demoted-errors "Hermes dashboard subscriber error: %S"
    (funcall fn event)))

(defun hermes-dashboard-transport--broadcast-event (client event)
  "Send EVENT to every subscriber function on CLIENT."
  (maphash (lambda (_token record)
             (hermes-dashboard-transport--deliver (plist-get record :fn) event))
           (hermes-dashboard-transport-client-subscribers client)))

(defun hermes-dashboard-transport--dispatch-event (client event)
  "Route EVENT to CLIENT's subscribers by session id, else broadcast.
A tagged event whose session id owns a subscriber goes to that subscriber alone;
untagged or unowned events broadcast to every subscriber.  With no subscribers
registered, fall back to CLIENT's legacy callback so single-callback callers
keep working unchanged."
  (let ((subscribers (hermes-dashboard-transport-client-subscribers client)))
    (if (and (hash-table-p subscribers)
             (> (hash-table-count subscribers) 0))
        (let* ((session-id (hermes-dashboard-transport--event-session-id event))
               (fn (and session-id
                        (hermes-dashboard-transport--session-subscriber-fn
                         client session-id))))
          (if fn
              (hermes-dashboard-transport--deliver fn event)
            (hermes-dashboard-transport--broadcast-event client event)))
      (funcall (hermes-dashboard-transport-client-callback client) event))))

(defvar hermes-dashboard-transport--clients (make-hash-table :test #'equal)
  "Hash of endpoint key to the shared dashboard client serving that endpoint.
One client -- one WebSocket and one authentication -- is shared by every chat
buffer attached to the same dashboard endpoint.")

(defun hermes-dashboard-transport--unregister-client (client)
  "Remove CLIENT from the shared registry when it is still the registered one."
  (when-let* ((key (hermes-dashboard-transport-client-endpoint-key client)))
    (when (eq (gethash key hermes-dashboard-transport--clients) client)
      (remhash key hermes-dashboard-transport--clients))))

;;; Process spawn and environment

(defun hermes-dashboard-transport--command (host port &optional command)
  "Return dashboard startup argv for HOST, PORT, and optional COMMAND."
  (let ((program (or command hermes-dashboard-transport-command)))
    (list (or (executable-find program)
              (let ((local (expand-file-name "~/.local/bin/hermes")))
                (and (equal program "hermes") (file-executable-p local) local))
              program)
          "dashboard" "--no-open" "--tui" "--isolated"
          "--host" host "--port" (number-to-string port))))

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

(defun hermes-dashboard-transport--append-url-query (url params)
  "Return URL with PARAMS, an alist, appended as `&key=value' query pairs.
Pairs whose value is nil are dropped; values are percent-encoded."
  (concat url
          (mapconcat
           (lambda (kv)
             (format "&%s=%s" (car kv)
                     (url-hexify-string (format "%s" (cdr kv)))))
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

;;; Startup helpers

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

;;; WebSocket lifecycle

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
  "Call THUNK while redacting URL as REDACTED-URL in websocket.el-visible state.
The session token must ride in URL's query string -- the gateway only reads it
there, since browsers cannot set WebSocket request headers -- and websocket.el
derives both the connection's process name and the websocket object's stored
URL from it.  Redaction must therefore happen at creation time: Emacs process
names are immutable once created, and the websocket struct's URL slot is
read-only, so neither can be rewritten after the fact.  That is why all three
creation points (`make-network-process', `open-network-stream', and
websocket.el's `websocket-inner-create') are interposed for the duration of
THUNK rather than cleaned up afterward."
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
  "Mark CLIENT's WebSocket connection closed and stop its heartbeat.
This is pure state: the close handler decides whether to reconnect the client in
place or finalize it, so this neither rejects pending requests nor unregisters
CLIENT from the shared registry."
  (setf (hermes-dashboard-transport-client-websocket client) nil
        (hermes-dashboard-transport-client-ready-p client) nil)
  (hermes-dashboard-transport--cancel-heartbeat client))

(defun hermes-dashboard-transport--current-websocket-p (client websocket)
  "Return non-nil when WEBSOCKET is CLIENT's current WebSocket."
  (eq websocket (hermes-dashboard-transport-client-websocket client)))

(defun hermes-dashboard-transport--reset-readiness (client)
  "Install a fresh pending readiness promise on CLIENT.
After the first connection reaches `gateway.ready' the readiness promise is
resolved.  An unexpected socket loss clears `ready-p' but leaves that promise
resolved, so `hermes-dashboard-transport--when-ready' would fire immediately
for requests issued during reconnect -- sending against a closed socket.
Replacing the promise with a pending one ensures those requests wait for the
replacement socket's `gateway.ready'."
  (setf (hermes-dashboard-transport-client-ready-promise client)
        (hermes--promise-make)))

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
    (cl-incf (hermes-dashboard-transport-client-generation client))
    (setf (hermes-dashboard-transport-client-stopping-p client) t)
    (ignore-errors
      (hermes-dashboard-transport--reject-pending-requests
       client (or message "Hermes dashboard transport stopped")))
    (ignore-errors
      (when-let* ((promise (hermes-dashboard-transport-client-ready-promise
                            client)))
        (hermes--promise-reject
         promise (or message "Hermes dashboard transport stopped"))))
    (ignore-errors (hermes-dashboard-transport--unregister-client client))
    (ignore-errors (hermes-dashboard-transport--cancel-idle-timer client))
    (ignore-errors (hermes-dashboard-transport--cancel-heartbeat client))
    (ignore-errors
      (setf (hermes-dashboard-transport-client-callback client) #'ignore)
      (clrhash (hermes-dashboard-transport-client-subscribers client))
      (clrhash (hermes-dashboard-transport-client-session-index client)))
    (ignore-errors (hermes-dashboard-transport--close-websocket client))
    (ignore-errors (hermes-dashboard-transport--delete-process client))
    (ignore-errors
      (setf (hermes-dashboard-transport-client-session-id client) nil
            (hermes-dashboard-transport-client-stored-session-id client) nil))
    client))

;;; Reconnect

(defun hermes-dashboard-transport-reconnect (client &optional message)
  "Restart CLIENT's dashboard WebSocket in place.
Subscribers, reference counts, and the shared-client registry are preserved so
attached chat buffers can resume their durable sessions after the replacement
socket emits `gateway.ready'.  Pending requests are rejected because their
responses belonged to the old socket.  MESSAGE describes the restart in local
status UI and reject callbacks."
  (unless (hermes-dashboard-transport-client-p client)
    (user-error "No Hermes dashboard transport client to reconnect"))
  (unless (> (or (hermes-dashboard-transport-client-refcount client) 0) 0)
    (user-error "No attached Hermes chat buffers to reconnect"))
  (let ((message (or message "Hermes dashboard socket reconnecting"))
        (hermes-dashboard-transport-reconnect-max-attempts
         (if (hermes-dashboard-transport--reconnect-enabled-p)
             hermes-dashboard-transport-reconnect-max-attempts
           1)))
    (cl-incf (hermes-dashboard-transport-client-generation client))
    (hermes-dashboard-transport--cancel-idle-timer client)
    (setf (hermes-dashboard-transport-client-stopping-p client) t)
    (ignore-errors (hermes-dashboard-transport--close-websocket client))
    (setf (hermes-dashboard-transport-client-stopping-p client) nil
          (hermes-dashboard-transport-client-reconnecting-p client) t
          (hermes-dashboard-transport-client-reconnect-attempts client) 0)
    (hermes-dashboard-transport--reset-readiness client)
    (hermes-dashboard-transport--reject-pending-requests client message)
    (hermes-dashboard-transport--arm-ready-timeout client)
    (hermes-dashboard-transport--emit-status client "reconnecting" message)
    (hermes-dashboard-transport--reconnect-attempt client 0)
    client))

(defun hermes-dashboard-transport--reconnect-enabled-p ()
  "Return non-nil when proactive shared-socket reconnect is configured."
  (and (numberp hermes-dashboard-transport-reconnect-max-attempts)
       (> hermes-dashboard-transport-reconnect-max-attempts 0)))

(defun hermes-dashboard-transport--should-reconnect-p (client)
  "Return non-nil when CLIENT should reconnect after an unexpected close.
Reconnect only while at least one buffer is attached and reconnect is enabled."
  (and (hermes-dashboard-transport--reconnect-enabled-p)
       (> (or (hermes-dashboard-transport-client-refcount client) 0) 0)))

(defun hermes-dashboard-transport--reconnect-backoff (attempt)
  "Return the backoff delay in seconds for reconnect ATTEMPT, 0-based."
  (min hermes-dashboard-transport-reconnect-max-delay
       (* hermes-dashboard-transport-reconnect-base-delay
          (expt 2 attempt))))

(defun hermes-dashboard-transport--schedule-reconnect (client attempt)
  "Schedule CLIENT's reconnect ATTEMPT after its backoff delay.
The attempt is dropped when CLIENT's generation moves on before the timer
fires: a manual reconnect or stop in the meantime owns the socket now, and a
stale attempt would open a second WebSocket and orphan the replacement's."
  (let ((generation (hermes-dashboard-transport-client-generation client)))
    (hermes-dashboard-transport--schedule
     (hermes-dashboard-transport--reconnect-backoff attempt)
     (lambda ()
       (when (= generation
                (hermes-dashboard-transport-client-generation client))
         (hermes-dashboard-transport--reconnect-attempt client attempt))))))

(defun hermes-dashboard-transport--reconnect-attempt (client attempt)
  "Reopen CLIENT's WebSocket for reconnect ATTEMPT, backing off on failure.
After the configured maximum attempts CLIENT is finalized so the next request
rebuilds it.  A reopened socket's `gateway.ready' clears the reconnect state and
broadcasts `reconnected'; another drop before then re-enters the backoff."
  (cond
   ((hermes-dashboard-transport-client-stopping-p client) nil)
   ((not (hermes-dashboard-transport-client-reconnecting-p client)) nil)
   ((not (hermes-dashboard-transport--should-reconnect-p client))
    (setf (hermes-dashboard-transport-client-reconnecting-p client) nil)
    (hermes-dashboard-transport--unregister-client client)
    (hermes-dashboard-transport--emit-status
     client "closed" "Hermes dashboard reconnect abandoned"))
   ((>= attempt hermes-dashboard-transport-reconnect-max-attempts)
    (setf (hermes-dashboard-transport-client-reconnecting-p client) nil)
    (hermes-dashboard-transport--unregister-client client)
    (hermes-dashboard-transport--emit-status
     client "closed" "Hermes dashboard reconnect failed"))
   (t
    (condition-case _err
        (setf (hermes-dashboard-transport-client-websocket client)
              (hermes-dashboard-transport--open-websocket-once
               client
               (hermes-dashboard-transport--client-websocket-url client)))
      (error
       (setf (hermes-dashboard-transport-client-reconnect-attempts client)
             (1+ attempt))
       (hermes-dashboard-transport--schedule-reconnect client (1+ attempt)))))))

(defun hermes-dashboard-transport--handle-socket-down (client message &optional websocket)
  "React to CLIENT's WebSocket closing with MESSAGE.
An intentional stop only marks the socket closed.  An unexpected loss rejects
pending requests, reports `closed', and either reconnects a still-referenced
client in place or finalizes it so the next request rebuilds it.  A reopened
socket that drops before becoming ready continues the existing backoff.
When WEBSOCKET is non-nil, ignore the event unless it still names CLIENT's
current socket; delayed close/error callbacks from an old socket must not tear
down a replacement opened by manual reconnect."
  (when (or (null websocket)
            (hermes-dashboard-transport--current-websocket-p client websocket))
    (hermes-dashboard-transport--mark-websocket-closed client)
    (unless (hermes-dashboard-transport-client-stopping-p client)
      (hermes-dashboard-transport--reject-pending-requests client message)
      (cond
       ((hermes-dashboard-transport-client-reconnecting-p client)
        ;; Already reconnecting: keep the fresh promise installed at the first
        ;; socket-down so requests stay deferred for the next `gateway.ready'.
        (hermes-dashboard-transport--schedule-reconnect
         client
         (cl-incf (hermes-dashboard-transport-client-reconnect-attempts client))))
       ((hermes-dashboard-transport--should-reconnect-p client)
        ;; Starting reconnect: install a fresh pending readiness promise so a
        ;; request issued before the replacement socket emits `gateway.ready' is
        ;; deferred instead of sent against the now-closed socket, then arm the
        ;; ready timeout for this reconnect generation.
        (setf (hermes-dashboard-transport-client-reconnecting-p client) t
              (hermes-dashboard-transport-client-reconnect-attempts client) 0)
        (hermes-dashboard-transport--reset-readiness client)
        (hermes-dashboard-transport--arm-ready-timeout client)
        (hermes-dashboard-transport--emit-status client "closed" message)
        (hermes-dashboard-transport--schedule-reconnect client 0))
       (t
        (hermes-dashboard-transport--unregister-client client)
        (hermes-dashboard-transport--emit-status client "closed" message))))))

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
        :on-message (lambda (websocket frame)
                      (when (hermes-dashboard-transport--current-websocket-p
                             client websocket)
                        (hermes-dashboard-transport--handle-frame
                         client (websocket-frame-text frame))))
        :on-error (lambda (websocket _type error)
                    (hermes-dashboard-transport--handle-socket-down
                     client
                     (format "Hermes dashboard WebSocket error: %s"
                             (hermes-dashboard-transport--redact-secret
                              (format "%s" error)
                              (hermes-dashboard-transport--client-secrets
                               client)))
                     websocket))
        :on-close (lambda (websocket)
                    (hermes-dashboard-transport--handle-socket-down
                     client "Hermes dashboard WebSocket closed" websocket)))))))

(cl-defun hermes-dashboard-transport-open-websocket
    (url redacted-url secrets &key on-message on-close on-error)
  "Open a raw dashboard WebSocket to URL with credential redaction.
REDACTED-URL hides the credential in the process name and the websocket's stored
URL; SECRETS are scrubbed from any error text.  ON-MESSAGE is called with each
frame's text, ON-CLOSE with no arguments, and ON-ERROR with a redacted message.
Return the websocket object.  This is the generic counterpart to the chat
client's own connect path, for callers (e.g. the kanban events tail) that own a
separate socket carrying plain JSON frames."
  (hermes-dashboard-transport--require-websocket)
  (hermes-dashboard-transport--call-with-redacted-websocket-state
   url redacted-url
   (lambda ()
     (websocket-open
      url
      :on-message (lambda (_ws frame)
                    (when on-message
                      (funcall on-message (websocket-frame-text frame))))
      :on-error (lambda (_ws _type err)
                  (when on-error
                    (funcall on-error
                             (hermes-dashboard-transport--redact-secret
                              (format "%s" err) secrets))))
      :on-close (lambda (_ws)
                  (when on-close (funcall on-close)))))))

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

;;; HTTP requests

(defun hermes-dashboard-transport--json-body (text)
  "Return JSON object parsed from TEXT, or nil for an empty body."
  (unless (string-empty-p (string-trim (or text "")))
    (json-parse-string text
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

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
      (concat "?" (string-join
                   (mapcar (lambda (entry)
                             (format "%s=%s"
                                     (url-hexify-string
                                      (format "%s" (car entry)))
                                     (url-hexify-string
                                      (format "%s" (cdr entry)))))
                           query)
                   "&"))
    ""))

(defun hermes-dashboard-transport--api-client-token (client)
  "Return CLIENT's dashboard session token, or nil."
  (and (hermes-dashboard-transport-client-p client)
       (hermes-dashboard-transport--non-empty-string
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
    (auth method path &key body query headers secrets)
  "Return the REST request plist for METHOD PATH under resolved AUTH.
BODY, QUERY, HEADERS, and SECRETS extend the request; AUTH supplies the base
URL plus its own headers and secrets.  Pure: shared by the synchronous and
asynchronous request executors."
  (list :url (concat (hermes-dashboard-transport--api-url
		      (plist-get auth :base-url) path)
		     (hermes-dashboard-transport--query-string query))
	:method method
	:headers (append (plist-get auth :headers)
			 headers
			 (and body '(("Content-Type" . "application/json"))))
	:body body
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
    (method path &key body query headers secrets retry)
  "Return a promise of dashboard REST METHOD PATH using resolved auth.
BODY, QUERY, HEADERS, and SECRETS extend the request; RETRY refreshes auth and
retries once when the request fails."
  (hermes--promise-then
   (hermes-dashboard-transport-api-auth-async)
   (lambda (auth)
     (let ((request (hermes-dashboard-transport--api-request-plist
		     auth method path :body body :query query
		     :headers headers :secrets secrets)))
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
                 :secrets secrets :retry nil))
            (hermes--promise-rejected
	     (hermes-dashboard-transport--redact-secret
	      reason (plist-get request :secrets))))))))))

(cl-defun hermes-dashboard-transport--api-request-with-client-async
    (client method path &key body query headers secrets)
  "Return a promise of dashboard REST METHOD PATH using CLIENT's session token.
BODY, QUERY, HEADERS, and SECRETS extend the request."
  (hermes--promise-map
   (hermes-dashboard-transport--http-json-request-async
    (hermes-dashboard-transport--api-request-plist
     (hermes-dashboard-transport--api-client-auth client)
     method path :body body :query query :headers headers :secrets secrets))
   (lambda (response) (plist-get response :body))))

(cl-defun hermes-dashboard-transport-api-request-async
    (method path &key body query headers secrets client)
  "Return a promise of authenticated dashboard REST METHOD PATH.
Mirrors `hermes-dashboard-transport-api-request' but resolves asynchronously so
callers never block Emacs.  BODY, QUERY, HEADERS, and SECRETS extend the
request.  CLIENT, when it carries a live session token, supplies the spawned
dashboard base URL and `X-Hermes-Session-Token'."
  (if (hermes-dashboard-transport--api-client-token client)
      (hermes-dashboard-transport--api-request-with-client-async
       client method path :body body :query query :headers headers
       :secrets secrets)
    (hermes-dashboard-transport--api-request-1-async
     method path :body body :query query :headers headers :secrets secrets
     :retry (equal method "GET"))))

;;; Profile and model caches

(defvar hermes-dashboard-transport--profile-cache nil
  "Cached `/api/profiles' payload as `(:base-url URL :payload PAYLOAD)'.
Keyed by base URL so switching dashboards re-fetches instead of serving a
previous dashboard's profiles.")

(defun hermes-dashboard-transport--profile-cache-stale-p ()
  "Return non-nil when the cached profile list is for a different dashboard URL."
  (and hermes-dashboard-transport--profile-cache
       (not (equal (plist-get hermes-dashboard-transport--profile-cache :base-url)
                   (ignore-errors (hermes-dashboard-transport--api-base-url))))))

(defun hermes-dashboard-transport--store-profile-cache (payload)
  "Cache PAYLOAD as the current dashboard's profile list and return it."
  (setq hermes-dashboard-transport--profile-cache
        (list :base-url (ignore-errors (hermes-dashboard-transport--api-base-url))
              :payload payload))
  payload)

(defun hermes-dashboard-transport-cached-profile-list ()
  "Return the cached `/api/profiles' payload for the current dashboard, or nil.
The cache is warmed by `hermes-dashboard-transport-profile-list-async' and is
discarded once `hermes-dashboard-transport-url' changes."
  (unless (hermes-dashboard-transport--profile-cache-stale-p)
    (plist-get hermes-dashboard-transport--profile-cache :payload)))

(defvar hermes-dashboard-transport--model-options-cache nil
  "Cached `model.options' payload as `(:base-url URL :payload PAYLOAD)'.
The provider/model catalog is dashboard-global -- disk config plus the curated
model list -- so it is keyed only by base URL and shared across every chat
buffer and model picker.  A base-URL change or a saved API key invalidates it;
see `hermes-dashboard-transport-invalidate-model-options'.")

(defun hermes-dashboard-transport--model-options-cache-stale-p ()
  "Return non-nil when cached model options are for a different dashboard URL."
  (and hermes-dashboard-transport--model-options-cache
       (not (equal (plist-get hermes-dashboard-transport--model-options-cache
                              :base-url)
                   (ignore-errors (hermes-dashboard-transport--api-base-url))))))

(defun hermes-dashboard-transport--store-model-options (payload)
  "Cache PAYLOAD as the current dashboard's model options and return it."
  (setq hermes-dashboard-transport--model-options-cache
        (list :base-url (ignore-errors (hermes-dashboard-transport--api-base-url))
              :payload payload))
  payload)

(defun hermes-dashboard-transport-cached-model-options ()
  "Return the cached `model.options' payload for the current dashboard, or nil.
The cache is warmed by `hermes-dashboard-transport-model-options-cached' and is
discarded once the dashboard URL changes or
`hermes-dashboard-transport-invalidate-model-options' is called."
  (unless (hermes-dashboard-transport--model-options-cache-stale-p)
    (plist-get hermes-dashboard-transport--model-options-cache :payload)))

(defun hermes-dashboard-transport-invalidate-model-options ()
  "Discard any cached `model.options' payload.
Callers that change provider authentication -- for example after saving an API
key -- call this so the next picker refetches the full list."
  (setq hermes-dashboard-transport--model-options-cache nil))

(defun hermes-dashboard-transport-profile-list (&optional client)
  "Return dashboard profile metadata from REST `/api/profiles'.
When CLIENT is non-nil, authenticate with its live dashboard session token.
The payload is cached for the current dashboard URL so subsequent profile
prompts can read it without blocking (see
`hermes-dashboard-transport-cached-profile-list')."
  (hermes-dashboard-transport--store-profile-cache
   (hermes-dashboard-transport-api-request
    "GET" "/api/profiles" :client client)))

(defun hermes-dashboard-transport-profile-list-async (&optional client)
  "Return a promise of `/api/profiles', warming the profile cache on success.
When CLIENT is non-nil, authenticate with its live dashboard session token.
Resolves without blocking Emacs, so callers can warm the cache eagerly (for
example when the dashboard opens)."
  (hermes--promise-map
   (hermes-dashboard-transport-api-request-async
    "GET" "/api/profiles" :client client)
   #'hermes-dashboard-transport--store-profile-cache))

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

;;; JSON-RPC plumbing

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

(defun hermes-dashboard-transport--when-ready (client on-ready on-fail)
  "Run ON-READY once CLIENT can send, or ON-FAIL with the failure reason.
Sends immediately when CLIENT is already ready or carries no readiness promise
\(as in tests); otherwise defers until the readiness promise settles."
  (let ((promise (hermes-dashboard-transport-client-ready-promise client)))
    (if (or (hermes-dashboard-transport-client-ready-p client)
            (not (hermes--promise-p promise)))
        (funcall on-ready)
      (hermes--promise-subscribe
       promise
       (lambda (_value) (funcall on-ready))
       (lambda (reason) (funcall on-fail reason))))))

(defun hermes-dashboard-transport--send-frame (client id method frame reject)
  "Send FRAME for pending request ID/METHOD on CLIENT.
Reject the pending request through REJECT when the WebSocket send fails."
  (condition-case err
      (funcall hermes-dashboard-transport-websocket-send-function
               (hermes-dashboard-transport-client-websocket client)
               (hermes-dashboard-transport--encode-frame frame))
    (error
     (hermes-dashboard-transport--take-pending client id)
     (hermes-dashboard-transport--reject-pending-request
      client (list :method method :reject reject)
      (hermes-dashboard-transport--send-failure-message client method err)))))

(defun hermes-dashboard-transport-request (client method &optional params resolve reject)
  "Send METHOD with PARAMS for CLIENT and correlate response callbacks.
RESOLVE is called with the JSON-RPC result.  REJECT is called with the error
message when provided.  The frame is deferred until CLIENT becomes ready, so
callers never wait on the connection handshake themselves.  Return the request
id."
  (let* ((id (hermes-dashboard-transport--next-id client))
         (pending (hermes-dashboard-transport--ensure-pending client))
         (frame (hermes-dashboard-transport--jsonrpc-request id method params))
         (timer (hermes-dashboard-transport--arm-request-timer client id)))
    (puthash id (list :method method :resolve resolve :reject reject :timer timer)
             pending)
    (hermes-dashboard-transport--when-ready
     client
     (lambda ()
       (hermes-dashboard-transport--send-frame client id method frame reject))
     (lambda (reason)
       (when (hermes-dashboard-transport--take-pending client id)
         (hermes-dashboard-transport--reject-pending-request
          client (list :method method :reject reject)
          (hermes-dashboard-transport--normalized-error-message client reason)))))
    id))

(defun hermes-dashboard-transport-call (client method &optional params)
  "Send METHOD with PARAMS for CLIENT and return a promise of its response.
The promise resolves with the JSON-RPC result and rejects with the error
message, adapting `hermes-dashboard-transport-request' callbacks so callers can
compose with `hermes--promise-then' instead of nesting RESOLVE/REJECT.

This is the low-level promise primitive for a raw method/params call; callers
with a typed wrapper (`hermes-dashboard-transport-session-*' and friends) use
`hermes-dashboard-transport-call-fn' to reuse the wrapper's parameter building."
  (let ((promise (hermes--promise-make)))
    (hermes-dashboard-transport-request
     client method params
     (lambda (result) (hermes--promise-resolve promise result))
     (lambda (reason) (hermes--promise-reject promise reason)))
    promise))

(defun hermes-dashboard-transport-call-fn (fn &rest args)
  "Call RPC wrapper FN with ARGS and return a promise of its result.
FN must accept trailing :resolve/:reject keywords, as the typed
`hermes-dashboard-transport-*' wrappers do, letting callers compose with
`hermes--promise-then' instead of nesting RESOLVE/REJECT."
  (let ((promise (hermes--promise-make)))
    (apply fn (append args
                      (list :resolve
                            (lambda (result)
                              (hermes--promise-resolve promise result))
                            :reject
                            (lambda (message)
                              (hermes--promise-reject promise message)))))
    promise))

(defun hermes-dashboard-transport--alist-without-nil (alist)
  "Return ALIST without nil-valued cells."
  (cl-remove-if (lambda (cell) (null (cdr cell))) alist))

(defun hermes-dashboard-transport--session-param (_client session-id)
  "Return the explicit SESSION-ID, or nil when the caller omitted it.
The shared dashboard client carries no ambient session identity: every
session-scoped RPC must pass its own live `:session-id'.  This keeps two
chat buffers sharing one socket from leaking session state into each other."
  session-id)

;;; RPC method wrappers

(defmacro hermes-dashboard-transport-define-rpc (name method docstring &rest spec)
  "Define NAME as a wrapper sending METHOD over the dashboard WebSocket.
DOCSTRING documents the generated `cl-defun'.  SPEC is a plist: :args lists
positional arguments after CLIENT, :keys lists `&key' parameters, :session when
non-nil sends the resolved session id, and :params adds extra
\(REQUEST-KEY . VALUE-FORM) cells.  Each :args and :keys symbol contributes a
request parameter keyed by its snake_case name with the symbol as the value;
nil values are dropped.  RESOLVE and REJECT keys are always added."
  (declare (indent 2))
  (let* ((args (plist-get spec :args))
         (keys (plist-get spec :keys))
         (session (plist-get spec :session))
         (extra (plist-get spec :params))
         (snake (lambda (sym)
                  (intern (replace-regexp-in-string "-" "_" (symbol-name sym)))))
         (cells (append
                 (and session
                      (list `(cons 'session_id
                                   (hermes-dashboard-transport--session-param
                                    client session-id))))
                 (mapcar (lambda (s) `(cons ',(funcall snake s) ,s))
                         (append args keys))
                 (mapcar (lambda (c) `(cons ',(car c) ,(cdr c))) extra)))
         (params (and cells
                      `(hermes-dashboard-transport--alist-without-nil
                        (list ,@cells)))))
    `(cl-defun ,name (client ,@args &key ,@keys
                             ,@(and session '(session-id)) resolve reject)
       ,docstring
       (hermes-dashboard-transport-request
        client ,method ,params resolve reject))))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-create "session.create"
  "Send a `session.create' request for CLIENT.
COLS, MESSAGES, TITLE, PROFILE, CWD, MODEL, PROVIDER, REASONING-EFFORT, and
FAST become request parameters; the backend persists them as per-session
runtime overrides.  RESOLVE and REJECT receive the asynchronous result or
error."
  :keys (cols messages title profile cwd model provider reasoning-effort fast))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-resume "session.resume"
  "Send a `session.resume' request for SESSION-ID on CLIENT.
COLS and PROFILE are optional; RESOLVE and REJECT receive the result or error."
  :args (session-id) :keys (cols profile))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-list "session.list"
  "Send a `session.list' request for CLIENT.
LIMIT caps the number of sessions returned.  RESOLVE and REJECT receive the
asynchronous result or error."
  :keys (limit))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-history "session.history"
  "Send a `session.history' request for SESSION-ID on CLIENT.
OFFSET and LIMIT page the returned messages; RESOLVE and REJECT receive the
result or error."
  :args (session-id) :keys (offset limit))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-delete "session.delete"
  "Send a `session.delete' request for SESSION-ID on CLIENT.
RESOLVE and REJECT receive the asynchronous result or error."
  :args (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-model-options "model.options"
  "Send a `model.options' request for CLIENT.
SESSION-ID scopes the current-model hints to that session.  RESOLVE and REJECT
receive the asynchronous result or error."
  :keys (session-id))

(cl-defun hermes-dashboard-transport-model-options-cached
    (client &key session-id force resolve reject)
  "Resolve `model.options' for CLIENT, serving the shared cache when possible.
With FORCE non-nil, bypass the cache and refetch.  SESSION-ID is forwarded on a
live fetch but does not key the cache: the provider/model catalog is
dashboard-global, so it is shared across sessions and chat buffers.  RESOLVE and
REJECT receive the payload or an error message, matching the plain RPC wrapper.

The underlying request defers until CLIENT is ready, so callers may warm the
cache immediately after starting a client."
  (let ((cached (and (not force)
                     (hermes-dashboard-transport-cached-model-options))))
    (if cached
        (when resolve (funcall resolve cached))
      (hermes-dashboard-transport-model-options
       client
       :session-id session-id
       :resolve (lambda (result)
                  (hermes-dashboard-transport--store-model-options result)
                  (when resolve (funcall resolve result)))
       :reject (or reject #'ignore)))))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-config-set "config.set"
  "Send a `config.set' request setting KEY to VALUE on CLIENT.
SESSION-ID scopes the change; CONFIRM-EXPENSIVE-MODEL acknowledges a pricier
model when `config.set' asks for confirmation.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (key value) :keys (session-id confirm-expensive-model))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-config-get "config.get"
  "Send a `config.get' request reading KEY on CLIENT.
CWD scopes the `project' key; SESSION-ID scopes the `fast' key.  RESOLVE and
REJECT receive the asynchronous result or error."
  :args (key) :keys (cwd session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-setup-status "setup.status"
  "Send a `setup.status' request for CLIENT.
The result carries `provider_configured'.  RESOLVE and REJECT receive the
asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-setup-runtime-check "setup.runtime_check"
  "Send a `setup.runtime_check' request for CLIENT.
The result carries `ok' (nil on a credential failure -- never a JSON-RPC error)
with provider/model/source, or an `error' string.  RESOLVE and REJECT receive
the asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-model-save-key "model.save_key"
  "Send a `model.save_key' request saving API-KEY for provider SLUG on CLIENT.
SESSION-ID scopes the live agent update.  The result carries the updated
provider object on success, or an error whose code is 4002 (unknown provider),
4003 (wrong auth type), 4006 (managed install), etc.  RESOLVE and REJECT receive
the asynchronous result or error."
  :args (slug api-key) :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-tools-configure "tools.configure"
  "Send a `tools.configure' request for NAMES and ACTION on CLIENT.
ACTION is `enable' or `disable'.  SESSION-ID scopes a live session reset when
the dashboard backend supports it.  RESOLVE and REJECT receive the result or
error."
  :args (names action) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-skills-reload "skills.reload"
  "Send a `skills.reload' request for CLIENT.
RESOLVE and REJECT receive the result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-list "rollback.list"
  "Send a `rollback.list' request for CLIENT.
SESSION-ID scopes the checkpoints.  RESOLVE and REJECT receive the result
or error."
  :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-diff "rollback.diff"
  "Send a `rollback.diff' request for checkpoint HASH on CLIENT.
SESSION-ID scopes the checkpoint.  RESOLVE and REJECT receive the result
or error."
  :args (hash) :keys (session-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-rollback-restore "rollback.restore"
  "Send a `rollback.restore' request for checkpoint HASH on CLIENT.
FILE-PATH restores a single file; SESSION-ID scopes the checkpoint.  RESOLVE
and REJECT receive the result or error."
  :args (hash) :keys (session-id file-path))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-delegation-status "delegation.status"
  "Send a `delegation.status' request for CLIENT.
RESOLVE and REJECT receive the result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-subagent-interrupt "subagent.interrupt"
  "Send a `subagent.interrupt' request for SUBAGENT-ID on CLIENT.
RESOLVE and REJECT receive the result or error."
  :args (subagent-id))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-cron-manage "cron.manage"
  "Send a `cron.manage' request for CLIENT.
ACTION is one of list, add, remove, pause, or resume; NAME identifies the job;
SCHEDULE and PROMPT are used by add.  RESOLVE and REJECT receive the result
or error."
  :keys (action name schedule prompt))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-prompt-submit "prompt.submit"
  "Send TEXT through `prompt.submit' on CLIENT.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-prompt-background "prompt.background"
  "Run TEXT as a background task on CLIENT's SESSION-ID via `prompt.background'.
The task runs in its own session; its answer arrives later as a
`background.complete' event rather than in the resolve RESULT, which only
carries the assigned task id.  RESOLVE and REJECT receive the result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-interrupt "session.interrupt"
  "Send `session.interrupt' for CLIENT's SESSION-ID or active session.
RESOLVE and REJECT receive the asynchronous result or error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-process-stop "process.stop"
  "Send `process.stop' for CLIENT to terminate running background processes.
RESOLVE and REJECT receive the asynchronous result or error.  This stops
background/tool processes; it does not interrupt the current model turn -- use
`hermes-dashboard-transport-session-interrupt' for that.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-title "session.title"
  "Set CLIENT's SESSION-ID title to TITLE via `session.title'.
RESOLVE and REJECT receive the asynchronous result or error.  The gateway
resolves the session from the live SESSION-ID and may reply with a pending
title when the session row does not exist yet."
  :keys (title) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-title-fetch "session.title"
  "Fetch CLIENT's current SESSION-ID title via `session.title' without setting it.
Omitting the title makes the gateway return the stored or auto-generated title.
RESOLVE and REJECT receive the asynchronous result or error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-session-steer "session.steer"
  "Send TEXT through `session.steer' for CLIENT's active session.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
result or error."
  :args (text) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-request "handoff.request"
  "Queue a handoff of CLIENT's SESSION-ID to PLATFORM via `handoff.request'.
The gateway validates the platform and its home channel, marks the session
pending, and a separate watcher performs the transfer; poll
`hermes-dashboard-transport-handoff-state' for the terminal result.  RESOLVE
and REJECT receive the asynchronous result or error."
  :args (platform) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-state "handoff.state"
  "Poll the handoff state for CLIENT's SESSION-ID via `handoff.state'.
RESOLVE receives a result whose state is one of pending, running, completed, or
failed, and is empty when no handoff record exists; REJECT receives any error."
  :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-handoff-fail "handoff.fail"
  "Mark CLIENT's SESSION-ID handoff failed via `handoff.fail' with reason ERROR.
Called when a bounded client poll times out so the session is not left pending;
a late gateway success is not clobbered.  RESOLVE and REJECT receive the
asynchronous result or error."
  :keys (error) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-commands-catalog "commands.catalog"
  "Request the dashboard `commands.catalog' for CLIENT.
RESOLVE and REJECT receive the asynchronous result or error.")

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-command-dispatch "command.dispatch"
  "Dispatch slash command NAME with ARG through CLIENT's `command.dispatch'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (name arg) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-complete-slash "complete.slash"
  "Request slash-command completions for TEXT via `complete.slash'.
TEXT is the partial command line, for example \"/handoff \" to list the
gateway's connected handoff platforms.  RESOLVE receives a result whose `items'
each carry text/display/meta; REJECT receives any error."
  :args (text))

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-slash-exec "slash.exec"
  "Run COMMAND through CLIENT's dashboard `slash.exec'.
SESSION-ID selects the live dashboard session.  RESOLVE and REJECT receive the
asynchronous result or error."
  :args (command) :session t)

(hermes-dashboard-transport-define-rpc
    hermes-dashboard-transport-approval-respond "approval.respond"
  "Send an `approval.respond' CHOICE for CLIENT.
SESSION-ID selects the live dashboard session.  ALL applies CHOICE broadly when
non-nil.  RESOLVE and REJECT receive the asynchronous result or error."
  :keys (choice all) :session t)

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

(defun hermes-dashboard-transport-terminal-read-respond
    (client request-id text &optional resolve reject)
  "Send TEXT for terminal-read REQUEST-ID on CLIENT."
  (hermes-dashboard-transport-request
   client "terminal.read.respond"
   `((request_id . ,request-id) (text . ,text))
   resolve reject))

;;; Connection startup and readiness

(defun hermes-dashboard-transport--start-process (_client command env)
  "Start dashboard process using COMMAND and ENV."
  (funcall hermes-dashboard-transport-make-process-function
           :name "hermes-dashboard"
           :buffer " *hermes-dashboard*"
           :command command
           :env env
           :connection-type 'pipe
           :noquery t
           :sentinel #'ignore))

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

(defvar hermes-dashboard-transport-schedule-function
  (lambda (delay fn &rest args) (apply #'run-at-time delay nil fn args))
  "Function used to schedule deferred dashboard connection work.
Called with DELAY seconds, a FUNCTION, and its ARGS.  Tests rebind it to drive
the timer-based connect and readiness flow without real timers.")

(defun hermes-dashboard-transport--schedule (delay fn &rest args)
  "Schedule FN with ARGS after DELAY seconds via the schedule function."
  (apply hermes-dashboard-transport-schedule-function delay fn args))

(defun hermes-dashboard-transport--fail-ready (client message)
  "Report MESSAGE for CLIENT, reject its readiness, and release its resources.
Used when the connection or `gateway.ready' handshake fails asynchronously."
  (hermes-dashboard-transport--emit-error client message)
  (hermes-dashboard-transport-stop client message))

(defun hermes-dashboard-transport--connect-async (client &optional attempt)
  "Open CLIENT's WebSocket, retrying dashboard cold-start races asynchronously.
ATTEMPT counts retries.  This never blocks: a transient failure reschedules the
next attempt with `hermes-dashboard-transport--schedule', a `user-error' fails
fast, and exhausting the retries fails CLIENT's readiness.  Success leaves the
gateway readiness flow to resolve the readiness promise."
  (let ((attempt (or attempt 0))
        (url (hermes-dashboard-transport--client-websocket-url client))
        (max-attempts (max 1 hermes-dashboard-transport-connect-retries)))
    (condition-case err
        (setf (hermes-dashboard-transport-client-websocket client)
              (hermes-dashboard-transport--open-websocket-once client url))
      (user-error
       (hermes-dashboard-transport--fail-ready
        client (hermes-dashboard-transport--condition-message client err)))
      (error
       (if (< (1+ attempt) max-attempts)
           (hermes-dashboard-transport--schedule
            hermes-dashboard-transport-connect-retry-delay
            #'hermes-dashboard-transport--connect-async client (1+ attempt))
         (hermes-dashboard-transport--fail-ready
          client (hermes-dashboard-transport--connection-error-message
                  client err)))))))

(defun hermes-dashboard-transport--ready-timeout-error (client)
  "Return a redacted `gateway.ready' timeout message for CLIENT."
  (format "Hermes dashboard did not become ready at %s"
          (hermes-dashboard-transport--client-redacted-websocket-url client)))

(defun hermes-dashboard-transport--arm-ready-timeout (client)
  "Fail CLIENT's readiness when `gateway.ready' does not arrive in time.
Scheduled without blocking; a no-op when CLIENT is already ready or the timeout
is disabled."
  (when hermes-dashboard-transport-ready-timeout
    (let ((generation (hermes-dashboard-transport-client-generation client)))
      (hermes-dashboard-transport--schedule
       hermes-dashboard-transport-ready-timeout
       (lambda ()
         ;; A timeout armed for an earlier connection lifetime must not tear
         ;; down a later reconnect whose own timeout window is still open.
         (when (and (= generation
                       (hermes-dashboard-transport-client-generation client))
                    (not (hermes-dashboard-transport-client-ready-p client)))
           (hermes-dashboard-transport--fail-ready
            client (hermes-dashboard-transport--ready-timeout-error client))))))))

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
                  :ready-promise (hermes--promise-make)
                  :callback (or callback #'ignore)))
         (argv (hermes-dashboard-transport--command host port command))
         (env (hermes-dashboard-transport--environment token base-environment)))
    (hermes-dashboard-transport--dispatch-event
     client (hermes-dashboard-transport--start-event host port token))
    (condition-case err
        (setf (hermes-dashboard-transport-client-process client)
              (hermes-dashboard-transport--start-process client argv env))
      (error
       (hermes-dashboard-transport--cleanup-start-failure client)
       (signal 'user-error
               (list (hermes-dashboard-transport--condition-message
                      client err)))))
    (hermes-dashboard-transport--connect-async client)
    (hermes-dashboard-transport--arm-ready-timeout client)
    client))

(defun hermes-dashboard-transport--remote-connect (client auth)
  "Store AUTH on CLIENT, announce connecting, and open its WebSocket.
AUTH is the plist resolved by `hermes-dashboard-transport--remote-auth-async'."
  (setf (hermes-dashboard-transport-client-token client)
        (plist-get auth :token)
        (hermes-dashboard-transport-client-websocket-url client)
        (plist-get auth :url)
        (hermes-dashboard-transport-client-redacted-websocket-url client)
        (plist-get auth :redacted-url)
        (hermes-dashboard-transport-client-secrets client)
        (plist-get auth :secrets))
  (hermes-dashboard-transport--dispatch-event
   client (hermes-dashboard-transport--remote-connect-event
           (plist-get auth :redacted-url)))
  (hermes-dashboard-transport--connect-async client)
  (hermes-dashboard-transport--arm-ready-timeout client))

(cl-defun hermes-dashboard-transport--start-remote
    (&key callback host port token remote-url remote-auth-method)
  "Attach to a remote dashboard with CALLBACK and override settings.
HOST, PORT, TOKEN, REMOTE-URL, and REMOTE-AUTH-METHOD override defaults.  The
auth handshake -- the status probe and any password/ticket exchange -- resolves
asynchronously, so this returns before the WebSocket opens and never blocks
Emacs."
  (let* ((host (or host "127.0.0.1"))
         (base-url (hermes-dashboard-transport--base-url host port remote-url))
         (method (or remote-auth-method
                     hermes-dashboard-transport-remote-auth-method))
         (client (make-hermes-dashboard-transport-client
                  :host host :port port :base-url base-url
                  :ready-promise (hermes--promise-make)
                  :callback (or callback #'ignore))))
    (hermes--promise-then
     (hermes-dashboard-transport-client-ready-promise client)
     (lambda (_value)
       (hermes-dashboard-transport--dispatch-event
        client (hermes-dashboard-transport--remote-connected-event
                (hermes-dashboard-transport--client-redacted-websocket-url
                 client)))))
    (hermes--promise-then
     (hermes-dashboard-transport--remote-auth-async host port base-url method
                                                     token)
     (lambda (auth) (hermes-dashboard-transport--remote-connect client auth))
     (lambda (reason)
       (hermes-dashboard-transport--fail-ready
        client (hermes-dashboard-transport--redact-secret reason))))
    client))

(cl-defun hermes-dashboard-transport--resolve-target
    (&key host port start-mode remote-url)
  "Resolve the dashboard target from HOST, PORT, START-MODE, and REMOTE-URL.
Return a plist of :mode, :host, :port, and :remote-url, defaulting unset values
from `hermes-dashboard-transport-url'.  Shared by the start and endpoint-key
paths so they always agree on the resolved target."
  (let* ((from-url (not (or host port remote-url)))
         (target (and from-url
                      (hermes-dashboard-transport--parse-url
                       hermes-dashboard-transport-url)))
         (host (or host (plist-get target :host)))
         (port (or port (plist-get target :port)))
         (remote-url (or remote-url
                         (and from-url
                              (not (hermes-dashboard-transport--loopback-host-p host))
                              hermes-dashboard-transport-url))))
    (list :mode (hermes-dashboard-transport--resolved-start-mode
                 start-mode host remote-url)
          :host host :port port :remote-url remote-url)))

(cl-defun hermes-dashboard-transport-start
    (&key callback host port command token base-environment
          start-mode remote-url remote-auth-method)
  "Start or attach to a dashboard transport and connect its WebSocket.
CALLBACK receives normalized `hermes-transport' events.  By default the target
is `hermes-dashboard-transport-url'; HOST, PORT, COMMAND, TOKEN,
BASE-ENVIRONMENT, START-MODE, REMOTE-URL, and REMOTE-AUTH-METHOD override it."
  (let* ((target (hermes-dashboard-transport--resolve-target
                  :host host :port port :start-mode start-mode
                  :remote-url remote-url))
         (host (plist-get target :host))
         (port (plist-get target :port))
         (remote-url (plist-get target :remote-url)))
    (pcase (plist-get target :mode)
      ('spawn (hermes-dashboard-transport--start-spawn
               :callback callback :host host :port port :command command
               :token token :base-environment base-environment))
      ('remote (hermes-dashboard-transport--start-remote
                :callback callback :host host :port port :token token
                :remote-url remote-url
                :remote-auth-method remote-auth-method)))))

(cl-defun hermes-dashboard-transport--endpoint-key
    (&key host port start-mode remote-url)
  "Return the registry key identifying the resolved dashboard endpoint.
HOST, PORT, START-MODE, and REMOTE-URL select the target.  Spawn-mode targets
share the single `local-spawn' key; remote targets key on their normalized base
URL."
  (let ((target (hermes-dashboard-transport--resolve-target
                 :host host :port port :start-mode start-mode
                 :remote-url remote-url)))
    (pcase (plist-get target :mode)
      ('spawn 'local-spawn)
      ('remote (hermes-dashboard-transport--base-url
                (plist-get target :host)
                (plist-get target :port)
                (plist-get target :remote-url))))))

(cl-defun hermes-dashboard-transport-acquire
    (&key callback host port command token base-environment
          start-mode remote-url remote-auth-method)
  "Return a shared dashboard client for the resolved endpoint, refcounted.
A live client already serving the endpoint is reused and its reference count
incremented; otherwise a fresh client is started, registered under the endpoint
key, and returned with a reference count of 1.  CALLBACK, HOST, PORT, COMMAND,
TOKEN, BASE-ENVIRONMENT, START-MODE, REMOTE-URL, and REMOTE-AUTH-METHOD match
`hermes-dashboard-transport-start'; CALLBACK is ignored when an existing client
is reused, since attached buffers subscribe rather than seize the callback."
  (let* ((key (hermes-dashboard-transport--endpoint-key
               :host host :port port :start-mode start-mode
               :remote-url remote-url))
         (existing (gethash key hermes-dashboard-transport--clients)))
    (if existing
        (progn
          (hermes-dashboard-transport--cancel-idle-timer existing)
          (cl-incf (hermes-dashboard-transport-client-refcount existing))
          existing)
      (let ((client (hermes-dashboard-transport-start
                     :callback callback :host host :port port :command command
                     :token token :base-environment base-environment
                     :start-mode start-mode :remote-url remote-url
                     :remote-auth-method remote-auth-method)))
        (setf (hermes-dashboard-transport-client-refcount client) 1
              (hermes-dashboard-transport-client-endpoint-key client) key)
        (puthash key client hermes-dashboard-transport--clients)
        client))))

;;; Idle close and release

(defun hermes-dashboard-transport--cancel-idle-timer (client)
  "Cancel CLIENT's pending idle-close timer, if any."
  (when-let* ((timer (hermes-dashboard-transport-client-idle-timer client)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (hermes-dashboard-transport-client-idle-timer client) nil)))

(defun hermes-dashboard-transport--idle-close (client)
  "Stop CLIENT when it is still idle after the idle-close delay elapses.
A re-acquire before the timer fires cancels it, so CLIENT is torn down only when
no buffer reattached."
  (when (and (hermes-dashboard-transport-client-p client)
             (zerop (or (hermes-dashboard-transport-client-refcount client) 0)))
    (setf (hermes-dashboard-transport-client-idle-timer client) nil)
    (hermes-dashboard-transport-stop
     client "Hermes dashboard transport stopped")))

(defun hermes-dashboard-transport--release-idle-client (client)
  "Stop CLIENT now, or after the idle-close delay when one is configured."
  (if (and (numberp hermes-dashboard-transport-idle-close-delay)
           (> hermes-dashboard-transport-idle-close-delay 0))
      (progn
        (hermes-dashboard-transport--cancel-idle-timer client)
        (setf (hermes-dashboard-transport-client-idle-timer client)
              (hermes-dashboard-transport--schedule
               hermes-dashboard-transport-idle-close-delay
               #'hermes-dashboard-transport--idle-close client)))
    (hermes-dashboard-transport-stop
     client "Hermes dashboard transport stopped")))

(defun hermes-dashboard-transport-release (client)
  "Drop one reference to shared CLIENT; tear it down at zero references.
With `hermes-dashboard-transport-idle-close-delay' set, the last release keeps
CLIENT warm for that delay instead of stopping immediately.  Return the
remaining reference count, or nil when CLIENT is not a client."
  (when (hermes-dashboard-transport-client-p client)
    (let ((count (max 0 (1- (or (hermes-dashboard-transport-client-refcount client)
                                0)))))
      (setf (hermes-dashboard-transport-client-refcount client) count)
      (when (zerop count)
        (hermes-dashboard-transport--release-idle-client client))
      count)))

;;; Heartbeat

(defvar hermes-dashboard-transport-ping-function
  (lambda (websocket)
    (hermes-dashboard-transport--require-websocket)
    (websocket-send websocket (make-websocket-frame :opcode 'ping :completep t)))
  "Function sending a keepalive ping frame on a websocket.
Called with the websocket object.  Tests rebind it to capture pings without a
live socket.")

(defun hermes-dashboard-transport--send-ping (client)
  "Send a keepalive ping frame on CLIENT's websocket, ignoring send errors."
  (when-let* ((websocket (hermes-dashboard-transport-client-websocket client)))
    (ignore-errors (funcall hermes-dashboard-transport-ping-function websocket))))

(defun hermes-dashboard-transport--cancel-heartbeat (client)
  "Cancel CLIENT's pending heartbeat timer, if any."
  (when-let* ((timer (hermes-dashboard-transport-client-heartbeat-timer client)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (hermes-dashboard-transport-client-heartbeat-timer client) nil)))

(defun hermes-dashboard-transport--arm-heartbeat (client)
  "Schedule CLIENT's next keepalive ping when heartbeats are enabled.
A no-op unless `hermes-dashboard-transport-heartbeat-interval' is a positive
number and CLIENT still has an open WebSocket."
  (hermes-dashboard-transport--cancel-heartbeat client)
  (when (and (numberp hermes-dashboard-transport-heartbeat-interval)
             (> hermes-dashboard-transport-heartbeat-interval 0)
             (hermes-dashboard-transport-client-websocket client))
    (setf (hermes-dashboard-transport-client-heartbeat-timer client)
          (hermes-dashboard-transport--schedule
           hermes-dashboard-transport-heartbeat-interval
           #'hermes-dashboard-transport--heartbeat-tick client))))

(defun hermes-dashboard-transport--heartbeat-tick (client)
  "Send a keepalive ping on CLIENT and schedule the next one.
Self-terminating: once CLIENT's WebSocket is gone the chain stops re-arming."
  (when (hermes-dashboard-transport-client-websocket client)
    (hermes-dashboard-transport--send-ping client)
    (hermes-dashboard-transport--arm-heartbeat client)))

;;; Event dispatch and frame handling

(defun hermes-dashboard-transport--emit-status (client status content)
  "Emit a status event with STATUS and CONTENT for CLIENT."
  (hermes-dashboard-transport--dispatch-event
   client (list :type 'status :status status :content content)))

(defun hermes-dashboard-transport--emit-error (client message &optional method code)
  "Emit a normalized dashboard error MESSAGE for CLIENT."
  (let ((event (list :type 'error :event "jsonrpc.error" :content message)))
    (when method
      (setq event (plist-put event :method method)))
    (when code
      (setq event (plist-put event :code code)))
    (hermes-dashboard-transport--dispatch-event client event)))

(defun hermes-dashboard-transport--store-session-result (_client _method _result)
  "No-op retained for the response path.
Session identifiers live buffer-locally in each chat buffer; the shared
client is transport-only and must not accumulate session identity from
`session.create'/`session.resume' responses, or two buffers sharing one
socket would clobber each other's session state."
  nil)

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

(defun hermes-dashboard-transport--handle-event-frame (client frame)
  "Dispatch JSON-RPC event FRAME to CLIENT's callback."
  (let ((params (hermes-transport--get frame 'params)))
    (when (equal (hermes-transport--get params 'type) "gateway.ready")
      (setf (hermes-dashboard-transport-client-ready-p client) t)
      (hermes-dashboard-transport--arm-heartbeat client)
      (when (hermes-dashboard-transport-client-reconnecting-p client)
        (setf (hermes-dashboard-transport-client-reconnecting-p client) nil
              (hermes-dashboard-transport-client-reconnect-attempts client) 0)
        (hermes-dashboard-transport--emit-status
         client "reconnected" "Hermes dashboard reconnected"))
      (when-let* ((promise (hermes-dashboard-transport-client-ready-promise
                            client)))
        (hermes--promise-resolve promise client)))
    (dolist (event (hermes-dashboard-transport--normalize-event-frame frame))
      (hermes-dashboard-transport--dispatch-event client event))))

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
