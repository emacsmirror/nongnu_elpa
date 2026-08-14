;;; hermes-dashboard-transport.el --- Dashboard transport for Hermes  -*- lexical-binding: t; -*-

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
(require 'hermes-dashboard-api)

(declare-function websocket-open "ext:websocket")
(declare-function websocket-send-text "ext:websocket")
(declare-function websocket-send "ext:websocket")
(declare-function make-websocket-frame "ext:websocket" t t)
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

(defcustom hermes-dashboard-transport-start-mode 'auto
  "How dashboard transport startup chooses between spawn and remote attach.
`auto' spawns when `hermes-dashboard-transport-url' is a loopback address and
attaches remotely otherwise.  `spawn' always starts a local dashboard process.
`remote' always attaches to an externally managed dashboard."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Spawn local dashboard" spawn)
                 (const :tag "Attach to remote dashboard" remote)))

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
its callbacks forever.  Ordinary requests use this timeout; `prompt.submit'
uses at least 1800 seconds because its response can span a full agent turn.
Use nil to disable per-request timeouts."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'hermes-dashboard-transport)

(defvar hermes-dashboard-transport-request-owner nil
  "Identity attached to requests for scoped cancellation by their caller.")

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

;;; Subscribers

(defun hermes-dashboard-transport-subscribe (client fn)
  "Register FN as an event subscriber on CLIENT and return an opaque token.
A new subscriber owns no session, so it receives untagged connection-level
broadcast events until
`hermes-dashboard-transport-subscribe-session' binds the token to a live session
id."
  (let ((token (gensym "hermes-dashboard-sub-")))
    (puthash token (list :fn fn :session-id nil)
             (hermes-dashboard-transport-client-subscribers client))
    token))

(defun hermes-dashboard-transport--session-owner-token
    (client session-id &optional excluded)
  "Return a live token owning SESSION-ID on CLIENT, except EXCLUDED."
  (cl-loop for token being the hash-keys of
           (hermes-dashboard-transport-client-subscribers client)
           for record = (gethash token
                                 (hermes-dashboard-transport-client-subscribers
                                  client))
           when (and (not (eq token excluded))
                     (equal (plist-get record :session-id) session-id))
           return token))

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
          (if-let* ((owner (hermes-dashboard-transport--session-owner-token
                            client previous token)))
              (puthash previous owner index)
            (remhash previous index))))
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
          (if-let* ((owner (hermes-dashboard-transport--session-owner-token
                            client session-id token)))
              (puthash session-id owner index)
            (remhash session-id index))))
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

(defun hermes-dashboard-transport--session-subscriber-fns (client session-id)
  "Return live subscriber functions bound to SESSION-ID on CLIENT."
  (when-let* ((token (gethash
                      session-id
                      (hermes-dashboard-transport-client-session-index client)))
              (record (gethash
                       token
                       (hermes-dashboard-transport-client-subscribers client))))
    (list (plist-get record :fn))))

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

(defun hermes-dashboard-transport--sole-subscriber-fn (client)
  "Return CLIENT's only unbound subscriber function, or nil."
  (let ((subscribers (hermes-dashboard-transport-client-subscribers client)))
    (when (= (hash-table-count subscribers) 1)
      (let ((record (car (hash-table-values subscribers))))
        (unless (plist-get record :session-id)
          (plist-get record :fn))))))

(defun hermes-dashboard-transport--dispatch-event (client event)
  "Route EVENT to CLIENT's subscribers by session id.
Tagged events go only to their live session owners and are dropped when no
owner remains.  A sole unbound subscriber is the legacy bootstrap case and may
receive the tagged event; with multiple subscribers an unowned tag is dropped.
Untagged events broadcast.  With no subscribers registered, fall back to
CLIENT's legacy callback for single-callback callers."
  (let ((subscribers (hermes-dashboard-transport-client-subscribers client)))
    (if (and (hash-table-p subscribers)
             (> (hash-table-count subscribers) 0))
        (if-let* ((session-id
                   (hermes-dashboard-transport--event-session-id event)))
            (let ((fns (hermes-dashboard-transport--session-subscriber-fns
                        client session-id)))
              (if fns
                  (mapc (lambda (fn)
                          (hermes-dashboard-transport--deliver fn event))
                        fns)
                (when-let* ((fn
                             (hermes-dashboard-transport--sole-subscriber-fn
                              client)))
                  (hermes-dashboard-transport--deliver fn event))))
          (hermes-dashboard-transport--broadcast-event client event))
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

(defun hermes-dashboard-transport-stop-all (&optional message)
  "Stop every shared dashboard client and return the number stopped.
MESSAGE is forwarded to `hermes-dashboard-transport-stop'."
  (let ((clients
         (delete-dups
          (cl-remove-if-not
           #'hermes-dashboard-transport-client-p
           (hash-table-values hermes-dashboard-transport--clients)))))
    (mapc (lambda (client)
            (hermes-dashboard-transport-stop client message))
          clients)
    (clrhash hermes-dashboard-transport--clients)
    (length clients)))

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
    (hermes-dashboard-transport--finalize-reconnect
     client "Hermes dashboard reconnect abandoned"))
   ((>= attempt hermes-dashboard-transport-reconnect-max-attempts)
    (hermes-dashboard-transport--finalize-reconnect
     client "Hermes dashboard reconnect failed"))
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

(defun hermes-dashboard-transport--finalize-reconnect (client message)
  "Report terminal reconnect MESSAGE and stop CLIENT."
  (setf (hermes-dashboard-transport-client-reconnecting-p client) nil)
  (hermes-dashboard-transport--emit-status client "closed" message)
  (hermes-dashboard-transport-stop client message))

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
        ;; Starting reconnect: move to a fresh connection generation before
        ;; arming its readiness timeout.  The startup timeout remains scheduled,
        ;; but its captured generation can no longer stop this reconnect early.
        ;; Install a fresh pending readiness promise so a request issued before
        ;; the replacement socket emits `gateway.ready' is deferred instead of
        ;; sent against the now-closed socket.
        (cl-incf (hermes-dashboard-transport-client-generation client))
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

;;; JSON-RPC plumbing

(defun hermes-dashboard-transport--encode-frame (frame)
  "Encode JSON-RPC FRAME as a JSON string."
  (json-serialize frame))

(defun hermes-dashboard-transport--decode-frame (text)
  "Decode JSON-RPC TEXT into an alist frame."
  (if (stringp text)
      (hermes-transport-json-parse text)
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

(defun hermes-dashboard-transport-cancel-owner-requests (client owner)
  "Silently cancel pending CLIENT requests belonging to OWNER.
Return the number cancelled.  This releases callbacks and timeout timers
without reporting teardown as a request failure."
  (let (ids)
    (when-let* (((hermes-dashboard-transport-client-p client))
                (pending (hermes-dashboard-transport-client-pending client)))
      (maphash (lambda (id request)
                 (when (eq owner (plist-get request :owner))
                   (push id ids)))
               pending))
    (mapc (lambda (id)
            (hermes-dashboard-transport--take-pending client id))
          ids)
    (length ids)))

(defun hermes-dashboard-transport--on-request-timeout (client id)
  "Reject CLIENT's pending request ID after its timeout elapses."
  (when-let* ((request (hermes-dashboard-transport--take-pending client id)))
    (hermes-dashboard-transport--reject-pending-request
     client request
     (hermes-dashboard-transport--normalized-error-message
      client
      (format "Hermes dashboard request %s timed out"
              (plist-get request :method))))))

(defun hermes-dashboard-transport--request-timeout (method)
  "Return the request timeout for METHOD, or nil when disabled."
  (and hermes-dashboard-transport-request-timeout
       (if (equal method "prompt.submit")
           (max 1800 hermes-dashboard-transport-request-timeout)
         hermes-dashboard-transport-request-timeout)))

(defun hermes-dashboard-transport--arm-request-timer (client id method)
  "Return a timeout timer for CLIENT's request ID and METHOD."
  (and-let* ((timeout (hermes-dashboard-transport--request-timeout method)))
    (run-at-time timeout nil
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
         (timer (hermes-dashboard-transport--arm-request-timer client id method)))
    (puthash id (list :method method :resolve resolve :reject reject :timer timer
                      :owner hermes-dashboard-transport-request-owner)
             pending)
    (hermes-dashboard-transport--when-ready
     client
     (lambda ()
       (when (gethash id pending)
         (hermes-dashboard-transport--send-frame client id method frame reject)))
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

(defun hermes-dashboard-transport--generation-live-p (client generation)
  "Return non-nil when GENERATION still owns CLIENT startup work."
  (and (not (hermes-dashboard-transport-client-stopping-p client))
       (= generation (hermes-dashboard-transport-client-generation client))))

(defun hermes-dashboard-transport--connect-async
    (client &optional attempt generation)
  "Open CLIENT's WebSocket, retrying dashboard cold-start races asynchronously.
ATTEMPT counts retries and GENERATION identifies their startup lifetime.  This
never blocks: a transient failure reschedules the next attempt with
`hermes-dashboard-transport--schedule', a `user-error' fails fast, and
exhausting the retries fails CLIENT's readiness.  Success leaves the gateway
readiness flow to resolve the readiness promise."
  (let ((attempt (or attempt 0))
        (generation (or generation
                        (hermes-dashboard-transport-client-generation client)))
        (url (hermes-dashboard-transport--client-websocket-url client))
        (max-attempts (max 1 hermes-dashboard-transport-connect-retries)))
    (when (hermes-dashboard-transport--generation-live-p client generation)
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
              #'hermes-dashboard-transport--connect-async
              client (1+ attempt) generation)
           (hermes-dashboard-transport--fail-ready
            client (hermes-dashboard-transport--connection-error-message
                    client err))))))))

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
                  :callback (or callback #'ignore)))
         (generation (hermes-dashboard-transport-client-generation client)))
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
     (lambda (auth)
       (when (hermes-dashboard-transport--generation-live-p client generation)
         (hermes-dashboard-transport--remote-connect client auth)))
     (lambda (reason)
       (when (hermes-dashboard-transport--generation-live-p client generation)
         (hermes-dashboard-transport--fail-ready
          client (hermes-dashboard-transport--redact-secret reason)))))
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
key on their resolved host and port; remote targets key on their normalized
base URL."
  (let ((target (hermes-dashboard-transport--resolve-target
                 :host host :port port :start-mode start-mode
                 :remote-url remote-url)))
    (pcase (plist-get target :mode)
      ('spawn (list 'spawn (plist-get target :host) (plist-get target :port)))
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
