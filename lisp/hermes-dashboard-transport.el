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

(defcustom hermes-dashboard-transport-host "127.0.0.1"
  "Host used for spawn-owned dashboard connections."
  :type 'string
  :group 'hermes-dashboard-transport)

(defcustom hermes-dashboard-transport-port nil
  "Port used for spawn-owned dashboard connections, or nil to pick one."
  :type '(choice (const :tag "Pick an available port" nil) integer)
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

(defcustom hermes-dashboard-transport-ready-wait-interval 0.05
  "Seconds to wait between dashboard `gateway.ready' checks."
  :type 'number
  :group 'hermes-dashboard-transport)

(cl-defstruct hermes-dashboard-transport-client
  "State for one dashboard/TUI JSON-RPC WebSocket connection."
  process
  websocket
  (host hermes-dashboard-transport-host)
  port
  token
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

(defun hermes-dashboard-transport--websocket-url (host port token)
  "Return authenticated dashboard WebSocket URL for HOST, PORT, and TOKEN."
  (format "ws://%s:%d/api/ws?token=%s" host port token))

(defun hermes-dashboard-transport--redacted-websocket-url (host port)
  "Return a user-displayable dashboard WebSocket URL for HOST and PORT."
  (format "ws://%s:%d/api/ws?token=<redacted>" host port))

(defun hermes-dashboard-transport--redact-secret (text &optional token)
  "Return TEXT with dashboard URL tokens and TOKEN redacted."
  (let ((message (if (stringp text) text (format "%s" text))))
    (setq message
          (replace-regexp-in-string
           "\\([?&]token=\\)[^&[:space:])\"']+" "\\1<redacted>"
           message t nil))
    (setq message
          (replace-regexp-in-string
           "\\(HERMES_DASHBOARD_SESSION_TOKEN=\\)[^[:space:])\"']+"
           "\\1<redacted>" message t nil))
    (if (and (stringp token) (not (string-empty-p token)))
        (string-replace token "<redacted>" message)
      message)))

(defun hermes-dashboard-transport--condition-message (client condition)
  "Return a user-displayable CONDITION message for CLIENT."
  (hermes-dashboard-transport--redact-secret
   (error-message-string condition)
   (hermes-dashboard-transport-client-token client)))

(defun hermes-dashboard-transport--start-event (host port _token)
  "Return a redacted dashboard startup status event for HOST and PORT."
  (list :type 'status
        :event "dashboard.starting"
        :status "starting"
        :content (format "Starting Hermes dashboard on %s:%d" host port)
        :url (hermes-dashboard-transport--redacted-websocket-url host port)))

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
   (hermes-dashboard-transport-client-token client)))

(defun hermes-dashboard-transport--safe-reject (client reject message method)
  "Call REJECT with MESSAGE, reporting callback failures for METHOD on CLIENT."
  (condition-case error
      (funcall reject message)
    (error
     (hermes-dashboard-transport--emit-error
      client
      (format "Hermes dashboard reject callback failed: %s"
              (hermes-dashboard-transport--condition-message client error))
      method))))

(defun hermes-dashboard-transport--reject-pending-request
    (client request message)
  "Reject one pending REQUEST on CLIENT with normalized MESSAGE."
  (let ((method (plist-get request :method))
        (reject (plist-get request :reject)))
    (if reject
        (hermes-dashboard-transport--safe-reject client reject message method)
      (hermes-dashboard-transport--emit-error client message method))))

(defun hermes-dashboard-transport--reject-pending-requests
    (client message)
  "Reject and clear every pending request on CLIENT with MESSAGE."
  (let ((message (hermes-dashboard-transport--normalized-error-message
                  client message))
        (pending (hermes-dashboard-transport-client-pending client)))
    (when (hash-table-p pending)
      (maphash
       (lambda (_id request)
         (hermes-dashboard-transport--reject-pending-request
          client request message))
       pending)
      (clrhash pending))))

(defun hermes-dashboard-transport-stop (client &optional message)
  "Release CLIENT's dashboard WebSocket, process, and pending requests.
MESSAGE is reported to pending request reject callbacks, or as a normalized
transport error when a pending request has no reject callback."
  (when (hermes-dashboard-transport-client-p client)
    (hermes-dashboard-transport--reject-pending-requests
     client (or message "Hermes dashboard transport stopped"))
    (setf (hermes-dashboard-transport-client-callback client) #'ignore)
    (hermes-dashboard-transport--close-websocket client)
    (hermes-dashboard-transport--delete-process client)
    (setf (hermes-dashboard-transport-client-session-id client) nil
          (hermes-dashboard-transport-client-stored-session-id client) nil)
    client))

(defun hermes-dashboard-transport--default-websocket-open (url client)
  "Open URL for CLIENT using websocket.el."
  (hermes-dashboard-transport--require-websocket)
  (let ((redacted-url (hermes-dashboard-transport--redacted-websocket-url
                       (hermes-dashboard-transport-client-host client)
                       (hermes-dashboard-transport-client-port client))))
    (hermes-dashboard-transport--call-with-redacted-websocket-state
     url redacted-url
     (lambda ()
       (websocket-open
        url
        :on-message (lambda (_websocket frame)
                      (hermes-dashboard-transport--handle-frame
                       client (websocket-frame-text frame)))
        :on-error (lambda (_websocket _type error)
                    (hermes-dashboard-transport--mark-websocket-closed client)
                    (hermes-dashboard-transport--emit-error
                     client (format "Hermes dashboard WebSocket error: %s"
                                    (hermes-dashboard-transport--redact-secret
                                     (format "%s" error)
                                     (hermes-dashboard-transport-client-token
                                      client)))))
        :on-close (lambda (_websocket)
                    (hermes-dashboard-transport--mark-websocket-closed client)
                    (hermes-dashboard-transport--emit-status
                     client "closed"
                     "Hermes dashboard WebSocket closed")))))))

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

(defun hermes-dashboard-transport-request (client method &optional params resolve reject)
  "Send METHOD with PARAMS for CLIENT and correlate response callbacks.
RESOLVE is called with the JSON-RPC result.  REJECT is called with the error
message when provided.  Return the request id."
  (let* ((id (hermes-dashboard-transport--next-id client))
         (pending (hermes-dashboard-transport--ensure-pending client))
         (frame (hermes-dashboard-transport--jsonrpc-request id method params)))
    (puthash id (list :method method :resolve resolve :reject reject) pending)
    (condition-case error
        (funcall hermes-dashboard-transport-websocket-send-function
                 (hermes-dashboard-transport-client-websocket client)
                 (hermes-dashboard-transport--encode-frame frame))
      (error
       (remhash id pending)
       (hermes-dashboard-transport--reject-pending-request
        client (list :method method :reject reject)
        (hermes-dashboard-transport--send-failure-message
         client method error))))
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
          (hermes-dashboard-transport--redacted-websocket-url
           (hermes-dashboard-transport-client-host client)
           (hermes-dashboard-transport-client-port client))))

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
        (condition-case error
            (throw 'connected
                   (hermes-dashboard-transport--open-websocket-once
                    client url))
          (user-error
           (signal 'user-error
                   (list (hermes-dashboard-transport--condition-message
                          client error))))
          (error
           (setq last-error error)
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
                    (hermes-dashboard-transport--websocket-url
                     (hermes-dashboard-transport-client-host client)
                     (hermes-dashboard-transport-client-port client)
                     (hermes-dashboard-transport-client-token client)))))
    (setf (hermes-dashboard-transport-client-websocket client) websocket)
    websocket))

(defun hermes-dashboard-transport--ready-timeout-error (client)
  "Return a redacted `gateway.ready' timeout message for CLIENT."
  (format "Hermes dashboard did not become ready at %s"
          (hermes-dashboard-transport--redacted-websocket-url
           (hermes-dashboard-transport-client-host client)
           (hermes-dashboard-transport-client-port client))))

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

(cl-defun hermes-dashboard-transport-start
    (&key callback host port command token base-environment)
  "Start a spawn-owned dashboard process and connect its WebSocket.
CALLBACK receives normalized `hermes-transport' events.  HOST, PORT, COMMAND,
TOKEN, and BASE-ENVIRONMENT override the default spawn settings."
  (let* ((host (or host hermes-dashboard-transport-host))
         (port (or port hermes-dashboard-transport-port
                   (hermes-dashboard-transport--pick-port)))
         (token (or token (hermes-dashboard-transport--generate-token)))
         (client (make-hermes-dashboard-transport-client
                  :host host :port port :token token
                  :callback (or callback #'ignore)))
         (argv (hermes-dashboard-transport--command host port command))
         (env (hermes-dashboard-transport--environment token base-environment)))
    (funcall (hermes-dashboard-transport-client-callback client)
             (hermes-dashboard-transport--start-event host port token))
    (condition-case error
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
                      client error)))))))

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
         (pending (and id (gethash id (hermes-dashboard-transport-client-pending client))))
         (method (plist-get pending :method)))
    (when pending
      (remhash id (hermes-dashboard-transport-client-pending client))
      (let ((result (hermes-transport--get frame 'result))
            (resolve (plist-get pending :resolve)))
        (hermes-dashboard-transport--store-session-result client method result)
        (when resolve
          (funcall resolve result))))))

(defun hermes-dashboard-transport--reject-response (client frame)
  "Reject CLIENT's pending request represented by error response FRAME."
  (let* ((id (hermes-dashboard-transport--frame-id frame))
         (pending (and id (gethash id (hermes-dashboard-transport-client-pending client))))
         (method (plist-get pending :method))
         (message (hermes-dashboard-transport--response-error-message frame))
         (code (hermes-dashboard-transport--response-error-code frame))
         handled)
    (when pending
      (remhash id (hermes-dashboard-transport-client-pending client))
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
  (condition-case error
      (let ((frame (hermes-dashboard-transport--decode-frame text)))
        (pcase (hermes-dashboard-transport--frame-kind frame)
          ('response (hermes-dashboard-transport--resolve-response client frame))
          ('error-response (hermes-dashboard-transport--reject-response client frame))
          ('event (hermes-dashboard-transport--handle-event-frame client frame))
          (_ (hermes-dashboard-transport--emit-error
              client "Unknown Hermes dashboard frame"))))
    (error
     (hermes-dashboard-transport--emit-error
      client (format "Invalid Hermes dashboard frame: %s" error)))))

(provide 'hermes-dashboard-transport)
;;; hermes-dashboard-transport.el ends here
