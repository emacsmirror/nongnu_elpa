;;; hermes-capabilities.el --- Native capability provider for Hermes  -*- lexical-binding: t; -*-

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

;; Native Emacs capability provider for the Hermes dashboard gateway.  Emacs
;; registers as a reactive capability provider over a dedicated second
;; `/api/ws' connection, independent from chat buffers and sessions.  The
;; backend sends true JSON-RPC `emacs.request' frames; Emacs dispatches them
;; through a typed method registry and replies with normal JSON-RPC
;; `result'/`error' frames using the same id.
;;
;; This module owns the provider skeleton: the method registry, the
;; `hermes-capabilities-define' macro, the inbound request normalizer and
;; dispatcher, the JSON-RPC response writer, and the dedicated connection
;; lifecycle.  The registry is empty by default; read-only method handlers are
;; seeded by the C5 card, and write/eval methods by C8.  Backend policy is
;; authoritative; this card implements no write or eval surface.
;;
;; Wire contract: see `native-emacs-capability-implementation-roadmap.md' §2.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'map)
(require 'project)
(require 'hermes-transport)
(require 'hermes-promise)
(require 'hermes-dashboard-transport)

(declare-function websocket-close "ext:websocket")


;;;; Customization

(defgroup hermes-capabilities nil
  "Native Emacs capability provider for the Hermes dashboard gateway."
  :group 'hermes)

(defcustom hermes-capabilities-enabled nil
  "Master switch for the native capability provider.
When nil `hermes-capabilities-start' refuses to start, so the provider can
never come up implicitly.  Set it to t before calling
`hermes-capabilities-start'."
  :type 'boolean)

(defcustom hermes-capabilities-target "emacs-pair"
  "Stable target id this provider registers as.
The backend routes `emacs_call' requests by target; `pair' targets are the
visible interactive Emacs and `worker' targets are background sessions.  See
the capability roadmap §7.1."
  :type 'string)

(defcustom hermes-capabilities-role 'pair
  "Role advertised during registration.
`pair' is the visible interactive Emacs; `worker' is a background session."
  :type '(choice (const :tag "Pair (interactive)" pair)
                 (const :tag "Worker (background)" worker)))

(defcustom hermes-capabilities-display-name nil
  "Human-readable label advertised during registration.
When nil a default derived from the target and Emacs version is used."
  :type '(choice (const :tag "Derive from target" nil) string))

(defcustom hermes-capabilities-backoff-base 1.0
  "Seconds before the first reconnect attempt of the capability socket."
  :type 'number)

(defcustom hermes-capabilities-backoff-max 30
  "Maximum backoff in seconds between capability socket reconnect attempts."
  :type 'number)

(defcustom hermes-capabilities-protocol-version 1
  "Capability protocol version advertised during registration."
  :type 'integer)

(defcustom hermes-capabilities-client-name "emacs-hermes"
  "Client name advertised during registration."
  :type 'string)

(defcustom hermes-capabilities-buffer-list-max 50
  "Maximum number of buffer entries returned by the `buffer.list' method.
When more live listable buffers exist, the list is truncated and the response
reports `truncated' plus the untruncated `total'."
  :type 'integer)

(defcustom hermes-capabilities-buffer-read-max-lines 1000
  "Maximum number of lines the `buffer.read' method returns in one call.
When a request asks for a larger range, the response is capped and reports
`truncated'.  Defaults to the roadmap §R1 pagination bound."
  :type 'integer)

(defcustom hermes-capabilities-buffer-read-max-chars 20000
  "Maximum number of characters the `buffer.read' method returns in one call.
A char cap keeps the JSON-RPC response bounded even for very long lines."
  :type 'integer)



;;;; Method registry
(defvar hermes-capabilities--registry nil
  "Alist of registered capability methods.
Each entry is (METHOD-STRING . ENTRY) where ENTRY is a plist
\(:handler FN :doc DOCSTRING :params-version VERSION).  Methods are added with
`hermes-capabilities-define' or `hermes-capabilities--register'.")

(cl-defun hermes-capabilities--register
    (method handler &key doc (params-version 1) replace)
  "Register capability METHOD with HANDLER.
HANDLER is a function of one argument, the request params (an alist or plist),
returning a JSON-serializable result.  DOC is a human-readable description.
PARAMS-VERSION is the advertised params schema version (default 1).  When
METHOD is already registered, signal an error unless REPLACE is non-nil."
  (declare (indent defun))
  (let ((entry (list :handler handler
                     :doc (or doc (documentation handler))
                     :params-version params-version)))
    (if (and (not replace)
             (assoc method hermes-capabilities--registry))
        (error "Hermes capability method already registered: %s" method)
      (setf (map-elt hermes-capabilities--registry method) entry)
      method)))

(defun hermes-capabilities--lookup (method)
  "Return the registry entry for METHOD, or nil when unregistered."
  (map-elt hermes-capabilities--registry method))

(defun hermes-capabilities--methods ()
  "Return a vector of registered method-name strings for JSON serialization."
  (vconcat (mapcar #'car hermes-capabilities--registry)))

(defun hermes-capabilities--method-descriptors ()
  "Return a hash table of method descriptors for registration/catalog.
The table is keyed by method-name string; each value is the alist
\((params_schema_version . N)).  This serializes as the JSON object keyed by
method name required by the wire contract (roadmap §2.1) and by the backend,
which stores `methods=dict(params.get(\"methods\") or {})'."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (cell hermes-capabilities--registry)
      (let ((method (car cell))
            (entry (cdr cell)))
        (puthash method
                 `((params_schema_version . ,(plist-get entry :params-version)))
                 table)))
    table))

(defmacro hermes-capabilities-define (name method docstring handler)
  "Define a capability method NAME responding to wire METHOD.
NAME is a symbol naming the generated declaration; METHOD is the wire string
sent by the backend.  HANDLER is a function of one argument (the request
params) returning a JSON-serializable result.  DOCSTRING documents the method.
The registration runs when the expansion is evaluated (load time), and NAME
is defined as a constant
holding METHOD so callers can reference the wire name symbolically."
  (declare (indent defun))
  `(progn
     (defconst ,name ,method
       ,(format "Wire method name for the `%s' capability." method))
     (hermes-capabilities--register
       ,method ,handler :doc ,docstring :replace t)))


;;;; Request normalization and dispatch

(defun hermes-capabilities--normalize-request (frame)
  "Normalize an inbound JSON-RPC FRAME into a request plist.
Return (:id :request-id :target :instance-id :method :params :timeout-ms), or
nil when FRAME is not an `emacs.request'.  Uses the shared
`hermes-transport--get' accessors so alist, plist, and hash frames all work."
  (let* ((method (hermes-transport--get frame 'method))
         (params (or (hermes-transport--get frame 'params) '()))
         (id (hermes-transport--get frame 'id)))
    (and (equal method "emacs.request")
         (list :id id
               :request-id (hermes-transport--get params 'request_id)
               :target (hermes-transport--get params 'target)
               :instance-id (hermes-transport--get params 'instance_id)
               :method (hermes-transport--get params 'method)
               :params (or (hermes-transport--get params 'params) '())
               :timeout-ms (hermes-transport--get params 'timeout_ms)))))

(defun hermes-capabilities--make-error (code message &optional method)
  "Return a structured error plist for CODE, MESSAGE, and optional METHOD.
Shape matches the wire contract: (:code :message :data (:method))."
  (let ((error `(:code ,code :message ,message)))
    (if method
        (plist-put error :data `(:method ,method))
      error)))

(cl-defun hermes-capabilities--dispatch (request)
  "Dispatch REQUEST plist to its registered handler.
Return (ok . RESULT) when the handler succeeds, or
\(error . ERROR-PLIST) when the method is unknown or the handler signals.
ERROR-PLIST has the shape produced by `hermes-capabilities--make-error'."
  (let ((method (plist-get request :method))
        (params (plist-get request :params)))
    (if-let* ((entry (hermes-capabilities--lookup method))
              (handler (plist-get entry :handler)))
        (condition-case err
            (cons 'ok (funcall handler params))
          (error
           (cons 'error
                 (hermes-capabilities--make-error
                  'invalid_params
                  (error-message-string err)
                  method))))
      (cons 'error
            (hermes-capabilities--make-error
             'method_not_supported
             (format "Capability method not supported: %s" method)
             method)))))


;;;; JSON-RPC response writer

(defun hermes-capabilities--plist-to-alist (plist)
  "Convert PLIST into an alist with keyword names as symbols.
Keys keep their names without the leading colon so the JSON output uses bare
field names (e.g. `ok', `content', `method')."
  (cl-loop for (key value) on plist by #'cddr
           when (keywordp key)
           collect (cons (intern (substring (symbol-name key) 1)) value)))

(defun hermes-capabilities--code-number (code)
  "Return a JSON-RPC integer error code for symbolic CODE."
  (pcase code
    ('method_not_supported -32601)
    ('invalid_params -32602)
    ('target_disconnected 409)
    ('timeout 408)
    ('approval_denied 403)
    ('response_too_large 413)
    (_ (if (numberp code) code -32603))))

(defun hermes-capabilities--envelope-alist-p (object)
  "Return non-nil when OBJECT is an alist envelope with an `ok' key.
Handlers that build their own roadmap §2.3 envelope return such an alist so the
response writer passes it through unchanged instead of wrapping it again."
  (and (listp object)
       (consp (car object))
       (eq (car (car object)) 'ok)))

(defun hermes-capabilities--result-frame (id result)
  "Return a JSON-RPC result frame alist for ID and RESULT.
RESULT is wrapped in the wire-contract envelope.  A result that is already a
roadmap §2.3 envelope (an alist starting with `ok', or a plist with `:ok') is
used as-is; otherwise it is treated as the capability content and wrapped as
\(:ok t :content RESULT)."
  (let ((envelope (cond
                   ((hermes-capabilities--envelope-alist-p result) result)
                   ((and (listp result) (plist-member result :ok))
                    (hermes-capabilities--plist-to-alist result))
                   (t (list :ok t :content result)))))
    `((jsonrpc . "2.0")
      (id . ,id)
      (result . ,(if (hermes-capabilities--envelope-alist-p envelope)
                     envelope
                   (hermes-capabilities--plist-to-alist envelope))))))

(defun hermes-capabilities--error-frame (id error)
  "Return a JSON-RPC error frame alist for ID and ERROR plist.
ERROR has the shape (:code :message :data (:method))."
  (let* ((data (plist-get error :data))
         (error-alist `((code . ,(hermes-capabilities--code-number
                                  (plist-get error :code)))
                        (message . ,(plist-get error :message))
                        ,@(and data
                               (list (cons 'data
                                           (hermes-capabilities--plist-to-alist
                                            data)))))))
    `((jsonrpc . "2.0")
      (id . ,id)
      (error . ,error-alist))))

(defun hermes-capabilities--response-for (request)
  "Build the JSON-RPC response frame alist for REQUEST.
Dispatches REQUEST and wraps the outcome in a result or error frame using the
request's :id so the backend can correlate the reply."
  (let ((outcome (hermes-capabilities--dispatch request))
        (id (plist-get request :id)))
    (if (eq (car outcome) 'ok)
        (hermes-capabilities--result-frame id (cdr outcome))
      (hermes-capabilities--error-frame id (cdr outcome)))))


;;;; Provider identity

(defun hermes-capabilities--client-version ()
  "Return the advertised client version string."
  (or (and (featurep 'hermes)
           (boundp 'hermes-version)
           (stringp hermes-version)
           hermes-version)
      (format "emacs-%s" emacs-version)))

(defun hermes-capabilities--instance-id (target)
  "Return a volatile per-process instance id for TARGET.
Changes on every Emacs restart so the backend can reject stale late responses
from a replaced instance.  Shape: `emacs-<ver>:<host>:<pid>:<fp>'."
  (let ((fp (substring (md5 (format "%s:%s:%d:%s"
                                    target (system-name) (emacs-pid)
                                    (current-time-string)))
                       0 8)))
    (format "emacs-%s:%s:%d:%s"
            emacs-version (system-name) (emacs-pid) fp)))

(defun hermes-capabilities--display-name (target)
  "Return the display name for TARGET, honoring the defcustom when set."
  (or hermes-capabilities-display-name
      (format "%s (%s)" target emacs-version)))

(defun hermes-capabilities--registration-params
    (target instance-id display-name role)
  "Build the `emacs.register' params alist for TARGET and the provider identity.
INSTANCE-ID, DISPLAY-NAME, and ROLE complete the advertised identity.
Capabilities list and method descriptors come from the live registry, so C5
methods are advertised automatically once registered."
  `((target . ,target)
    (instance_id . ,instance-id)
    (display_name . ,display-name)
    (role . ,(if (symbolp role) (symbol-name role) role))
    (capabilities . ,(hermes-capabilities--methods))
    (methods . ,(hermes-capabilities--method-descriptors))
    (client . ((name . ,hermes-capabilities-client-name)
               (version . ,(hermes-capabilities--client-version))))
    (protocol_version . ,hermes-capabilities-protocol-version)))

(defun hermes-capabilities--register-request (id params)
  "Return a JSON-RPC `emacs.register' request frame alist for ID and PARAMS."
  `((jsonrpc . "2.0")
    (id . ,id)
    (method . "emacs.register")
    (params . ,params)))


;;;; Provider transport (dedicated capability connection)

(cl-defstruct (hermes-capabilities--provider
               (:constructor hermes-capabilities--provider-create))
  "State for one dedicated capability provider WebSocket connection.
This connection is independent from chat buffers and sessions: it owns its own
socket, message handler, and reconnect loop.  No session identity is stored
here or on the shared chat client."
  socket buffer target instance-id display-name role
  (seq 0)
  (backoff 1)
  reconnect-timer
  (generation 0)
  (active t))

(defvar hermes-capabilities--provider nil
  "The active capability provider, or nil when none is connected.
Bound by `hermes-capabilities-start'; cleared by
`hermes-capabilities--teardown'.")

(defvar hermes-capabilities--send-function
  #'hermes-capabilities--default-send
  "Function used to send a JSON string on the provider socket.
Called with (SOCKET TEXT).  Tests rebind it to capture outbound frames.")

(defvar hermes-capabilities--url-function
  #'hermes-dashboard-transport-capability-url-async
  "Function used to resolve the capability WebSocket URL promise.
Called with keyword args (currently just :client).  Tests rebind it to inject a
fixed URL plist without touching auth or the network.")

(defvar hermes-capabilities--open-function
  #'hermes-dashboard-transport-open-websocket
  "Function used to open the capability WebSocket.
Called with (URL REDACTED-URL SECRETS :on-message :on-close :on-error).  Tests
rebind it to return a fake socket object.")

(defvar hermes-capabilities--request-owner-buffer nil
  "Owning provider buffer for the capability request being dispatched.")

(defun hermes-capabilities--default-send (socket text)
  "Send TEXT on the provider SOCKET using websocket.el."
  (when (and socket (fboundp 'websocket-send-text))
    (websocket-send-text socket text)))

(defun hermes-capabilities--next-request-id (provider)
  "Return a fresh JSON-RPC id for PROVIDER's next outbound request."
  (let ((next (1+ (hermes-capabilities--provider-seq provider))))
    (setf (hermes-capabilities--provider-seq provider) next)
    (format "hermes-cap-%d" next)))

(defun hermes-capabilities--pending-request-id (provider)
  "Return the JSON-RPC id of the last outbound registration for PROVIDER, or nil.
Used to correlate inbound registration responses."
  (let ((seq (hermes-capabilities--provider-seq provider)))
    (and (> seq 0)
         (format "hermes-cap-%d" seq))))

(defun hermes-capabilities--send-frame (provider frame)
  "Send FRAME on PROVIDER's socket.
No-op when the socket is gone or the provider is inactive."
  (when (and (hermes-capabilities--provider-active provider)
             (hermes-capabilities--provider-socket provider))
    (funcall hermes-capabilities--send-function
             (hermes-capabilities--provider-socket provider)
             (json-serialize frame))))

(defun hermes-capabilities--send-registration (provider)
  "Send the `emacs.register' request for PROVIDER."
  (let ((frame (hermes-capabilities--register-request
                (hermes-capabilities--next-request-id provider)
                (hermes-capabilities--registration-params
                 (hermes-capabilities--provider-target provider)
                 (hermes-capabilities--provider-instance-id provider)
                 (hermes-capabilities--provider-display-name provider)
                 (hermes-capabilities--provider-role provider)))))
    (hermes-capabilities--send-frame provider frame)))

(defun hermes-capabilities--response-error-message (frame)
  "Return the error message from a JSON-RPC error response FRAME."
  (let ((error (hermes-transport--get frame 'error)))
    (or (and (hermes-transport--object-p error)
             (hermes-transport--scalar-string
              (hermes-transport--get error 'message)))
        (hermes-transport--scalar-string error)
        "Hermes capability request failed")))

(defun hermes-capabilities--method-not-found-p (frame)
  "Return non-nil when FRAME rejects with a JSON-RPC `method not found' error."
  (let ((message (hermes-capabilities--response-error-message frame)))
    (string-match-p (rx (or "method not found"
                            "method_not_found"
                            "Method not found"
                            "-32601"))
                    (or message ""))))

(defun hermes-capabilities--handle-registration-response (provider frame)
  "Handle an inbound registration response FRAME for PROVIDER.
On a `method not found' rejection (unsupported backend), deactivate the
provider gracefully without reconnect spam."
  (cond
   ((hermes-capabilities--method-not-found-p frame)
    (hermes-capabilities--on-unsupported provider))
   ((hermes-transport--get frame 'error)
    (message "Hermes capabilities: registration rejected: %s"
             (hermes-capabilities--response-error-message frame)))))

(defun hermes-capabilities--on-unsupported (provider)
  "Deactivate PROVIDER after the backend reports the capability unsupported."
  (message "Hermes capabilities: backend does not support `emacs.register'; staying offline")
  (setf (hermes-capabilities--provider-active provider) nil)
  (hermes-capabilities--teardown provider))

(defun hermes-capabilities--decode (text)
  "Decode JSON TEXT into an alist frame, or nil."
  (condition-case nil
      (json-parse-string text
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error nil)))

(defun hermes-capabilities--frame-id (frame)
  "Return FRAME's JSON-RPC id, or nil."
  (hermes-transport--get frame 'id))

(defun hermes-capabilities--handle-message (provider text)
  "Handle inbound socket TEXT for PROVIDER.
Parses the frame, routes registration responses to the registration handler,
and dispatches `emacs.request' frames through the registry, sending the
JSON-RPC reply back over the socket.  On a `gateway.ready' event the provider
re-registers."
  (when (hermes-capabilities--provider-active provider)
    (when-let* ((frame (hermes-capabilities--decode text)))
      (let ((id (hermes-capabilities--frame-id frame)))
        (cond
         ;; Backend-initiated capability request.
         ((equal (hermes-transport--get frame 'method) "emacs.request")
          (when-let* ((request (hermes-capabilities--normalize-request frame)))
            (let* ((hermes-capabilities--request-owner-buffer
                    (hermes-capabilities--provider-buffer provider))
                   (response (hermes-capabilities--response-for request)))
              (hermes-capabilities--send-frame provider response))))
         ;; Backend response to our registration (result or error).
         ((and id (or (hermes-transport--get frame 'result)
                      (hermes-transport--get frame 'error)))
          (when (equal id (hermes-capabilities--pending-request-id provider))
            (hermes-capabilities--handle-registration-response provider frame)
            (setf (hermes-capabilities--provider-backoff provider) 1)))
         ;; gateway.ready event: (re-)register on every ready.
         ((equal (hermes-transport--get frame 'method) "event")
          (let* ((params (or (hermes-transport--get frame 'params) '()))
                 (type (hermes-transport--get params 'type)))
            (when (equal type "gateway.ready")
              (hermes-capabilities--send-registration provider)))))))))

(defun hermes-capabilities--current-generation-p (provider generation)
  "Return non-nil when GENERATION still owns PROVIDER's connection."
  (and (hermes-capabilities--provider-active provider)
       (= generation (hermes-capabilities--provider-generation provider))))

(defun hermes-capabilities--on-down (provider generation &optional message)
  "Drop PROVIDER's GENERATION, report MESSAGE, and reconnect with backoff."
  (when (hermes-capabilities--current-generation-p provider generation)
    (when message
      (message "Hermes capabilities: %s" message))
    (setf (hermes-capabilities--provider-socket provider) nil)
    (hermes-capabilities--reconnect provider)))

(defun hermes-capabilities--reconnect (provider)
  "Schedule a bounded-backoff reconnect for PROVIDER."
  (when (and (hermes-capabilities--provider-active provider)
             (hermes-capabilities--provider-buffer provider)
             (buffer-live-p (hermes-capabilities--provider-buffer provider))
             (not (hermes-capabilities--provider-reconnect-timer provider)))
    (let ((delay (min hermes-capabilities-backoff-max
                      (* hermes-capabilities-backoff-base
                         (hermes-capabilities--provider-backoff provider)))))
      (setf (hermes-capabilities--provider-backoff provider)
            (max 2 (* 2 (hermes-capabilities--provider-backoff provider)))
            (hermes-capabilities--provider-reconnect-timer provider)
            (run-at-time delay nil
                         #'hermes-capabilities--do-reconnect provider)))))

(defun hermes-capabilities--do-reconnect (provider)
  "Clear PROVIDER's reconnect timer and reconnect when still active."
  (setf (hermes-capabilities--provider-reconnect-timer provider) nil)
  (when (and (hermes-capabilities--provider-active provider)
             (buffer-live-p (hermes-capabilities--provider-buffer provider)))
    (hermes-capabilities--connect provider)))

(defun hermes-capabilities--connect (provider)
  "Resolve the capability `/api/ws' URL and open PROVIDER's socket."
  (let* ((generation (1+ (hermes-capabilities--provider-generation provider)))
         (buffer (hermes-capabilities--provider-buffer provider))
         (instance (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (hermes-instance-context))))
         (hermes-dashboard-transport-url
          (or (and instance (hermes-instance-url instance))
              hermes-dashboard-transport-url)))
    (setf (hermes-capabilities--provider-generation provider) generation)
    (hermes--promise-then
     (funcall hermes-capabilities--url-function)
     (lambda (auth)
       (when (hermes-capabilities--current-generation-p provider generation)
         (setf (hermes-capabilities--provider-socket provider)
               (funcall hermes-capabilities--open-function
                        (plist-get auth :url)
                        (plist-get auth :redacted-url)
                        (plist-get auth :secrets)
                        :on-message
                        (lambda (text)
                          (when (hermes-capabilities--current-generation-p
                                 provider generation)
                            (hermes-capabilities--handle-message provider text)))
                        :on-close
                        (lambda ()
                          (hermes-capabilities--on-down provider generation))
                        :on-error
                        (lambda (msg)
                          (hermes-capabilities--on-down
                           provider generation msg))))))
     (lambda (reason)
       (when (hermes-capabilities--current-generation-p provider generation)
         (message "Hermes capabilities: connect failed: %s" reason)
         (hermes-capabilities--reconnect provider))))))

(defun hermes-capabilities--teardown (provider)
  "Tear down PROVIDER: stop reconnecting, close the socket, drop the kill hook.
Removing the owning buffer's `kill-buffer-hook' entry keeps a replaced
provider's old buffer from tearing down a newer provider later."
  (setf (hermes-capabilities--provider-active provider) nil
        (hermes-capabilities--provider-generation provider)
        (1+ (hermes-capabilities--provider-generation provider)))
  (when-let* ((timer (hermes-capabilities--provider-reconnect-timer provider)))
    (cancel-timer timer))
  (setf (hermes-capabilities--provider-reconnect-timer provider) nil)
  (when-let* ((socket (hermes-capabilities--provider-socket provider)))
    (when (fboundp 'websocket-close)
      (ignore-errors (websocket-close socket))))
  (setf (hermes-capabilities--provider-socket provider) nil)
  (when-let* ((buffer (hermes-capabilities--provider-buffer provider)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook
                     #'hermes-capabilities--teardown-hook t)))))



;;;; Read-only capability methods (C5)
;;
;; Bounded read-only handlers seeded into the registry.  Each handler is a
;; function of one argument (the request params, an alist/plist/hash read
;; through `hermes-transport--get') and returns a JSON-serializable alist.
;; `json-serialize' renders alist value nil as the empty object, so JSON
;; booleans use :false/:null-style keywords where the distinction matters.

(defun hermes-capabilities--json-bool (value)
  "Return a JSON-serializable boolean for VALUE.
A non-nil VALUE maps to JSON true; nil maps to the `:false' keyword
\(json-serialize renders nil as an empty object rather than false, so the
keyword is required)."
  (if value t :false))

(defun hermes-capabilities--listable-buffer-p (name)
  "Return non-nil when a buffer named NAME should be listed.
Excludes internal buffers whose names begin with a space."
  (and (stringp name)
       (not (string-empty-p name))
       ;; Space-prefixed names are internal/temp buffers by Emacs convention.
       (not (eq (aref name 0) ?\s))))

(defun hermes-capabilities--remote-path-p (path)
  "Return non-nil when PATH is a TRAMP/remote file name.
Used by `buffer.read' to reject remote buffers by default.  `file-remote-p'
returns the remote identification prefix (a string) for remote names and nil
otherwise, so any non-nil value counts as remote."
  (and (stringp path)
       (when (fboundp 'file-remote-p)
         (file-remote-p path))))

(defun hermes-capabilities--remote-buffer-p (buffer)
  "Return non-nil when BUFFER visits or is rooted in a remote path."
  (with-current-buffer buffer
    (or (hermes-capabilities--remote-path-p buffer-file-name)
        (hermes-capabilities--remote-path-p default-directory))))

(defun hermes-capabilities--buffer-entry (buffer)
  "Return a JSON-serializable alist describing BUFFER.
Shape: ((name . S) (mode . S) (point . N) (file . PATH/:null))."
  (with-current-buffer buffer
    (let ((file (buffer-file-name)))
      `((name . ,(buffer-name buffer))
        (mode . ,(symbol-name major-mode))
        (point . ,(point))
        (file . ,(if file file :null))))))

(defun hermes-capabilities--listable-buffers ()
  "Return the list of listable live buffer objects."
  (seq-filter (lambda (b) (hermes-capabilities--listable-buffer-p
                           (buffer-name b)))
              (buffer-list)))

(defun hermes-capabilities--context-buffer ()
  "Return the user context buffer for the current capability request."
  (let ((selected (and (window-live-p (selected-window))
                       (window-buffer (selected-window)))))
    (cond ((buffer-live-p selected) selected)
          ((buffer-live-p hermes-capabilities--request-owner-buffer)
           hermes-capabilities--request-owner-buffer)
          (t (current-buffer)))))

(defun hermes-capabilities--truncate-by-chars (string max)
  "Return (TRUNCATED . STRING) capping STRING at MAX characters.
When MAX is nil, no cap is applied."
  (if (or (null max) (<= (length string) max))
      (cons nil string)
    (cons t (substring string 0 max))))

(defun hermes-capabilities--project-entry (project)
  "Return the `project.current' alist for PROJECT, or nulls when PROJECT is nil."
  (if project
      (let ((root (expand-file-name (project-root project))))
        `((root . ,root)
          (name . ,(file-name-nondirectory (directory-file-name root)))))
    `((root . :null)
      (name . :null))))

(defun hermes-capabilities--buffer-slice (buffer start-line end-line)
  "Return a plist slicing BUFFER from START-LINE to END-LINE (1-based, inclusive).
Shape: (:content STRING :start-line N :end-line N :total-lines N
:truncated-by-lines BOOL).  START-LINE defaults to 1; END-LINE defaults to
the line cap.  Honors `hermes-capabilities-buffer-read-max-lines'.  When
START-LINE exceeds the buffer's line count, returns empty content."
  (let* ((max hermes-capabilities-buffer-read-max-lines)
         (total (with-current-buffer buffer
                  (line-number-at-pos (point-max))))
         (start (max 1 (or start-line 1))))
    (if (> start total)
        (list :content ""
              :start-line start
              :end-line (1- start)
              :total-lines total
              :truncated-by-lines nil)
      (let* ((effective-end (min (or end-line total) (+ start max -1) total))
             (truncated-by-lines (< effective-end (or end-line total)))
             (content
              (with-current-buffer buffer
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (forward-line (1- start))
                    (let ((beg (point)))
                      (forward-line (- effective-end start))
                      (end-of-line)
                      (buffer-substring-no-properties beg (point))))))))
        (list :content content
              :start-line start
              :end-line effective-end
              :total-lines total
              :truncated-by-lines truncated-by-lines)))))

(defun hermes-capabilities--handle-buffer-list (_params)
  "Return the `buffer.list' result alist.
Excludes internal buffers, caps at `hermes-capabilities-buffer-list-max', and
reports truncation metadata."
  (let* ((buffers (hermes-capabilities--listable-buffers))
         (total (length buffers))
         (cap hermes-capabilities-buffer-list-max)
         (capped (seq-take buffers cap))
         (entries (mapcar #'hermes-capabilities--buffer-entry capped)))
    `((buffers . ,(vconcat entries))
      (count . ,(length entries))
      (total . ,total)
      (truncated . ,(hermes-capabilities--json-bool (> total cap))))))

(defun hermes-capabilities--handle-buffer-current (_params)
  "Return the `buffer.current' result alist for the user context buffer."
  (hermes-capabilities--buffer-entry (hermes-capabilities--context-buffer)))

(defun hermes-capabilities--handle-project-current (_params)
  "Return the `project.current' result alist.
Safe when no project is active: returns null root and name."
  (with-current-buffer (hermes-capabilities--context-buffer)
    (hermes-capabilities--project-entry
     (project-current nil default-directory))))

(defun hermes-capabilities--handle-buffer-read (params)
  "Return the `buffer.read' envelope alist for PARAMS.
PARAMS keys: `buffer' (required), `start' and `end' (roadmap §2.2 wire names);
`start_line'/`end_line' are accepted as backward-compatible aliases.  Rejects
buffers visiting remote files and unknown buffers.  Enforces line and char caps.

Returns the roadmap §2.3 envelope shape
\((ok . t) (content . STRING) (metadata . ALIST)), where metadata carries
`buffer', `truncated', `line_count', `total_lines', `start_line', and
`end_line'."
  (let ((name (hermes-transport--get params 'buffer)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (error "Buffer.read: missing or invalid `buffer' parameter"))
    (let ((buffer (get-buffer name)))
      (unless buffer
        (error "Buffer.read: no buffer named %S" name))
      (when (hermes-capabilities--remote-buffer-p buffer)
        (error "Buffer.read: remote/TRAMP buffers rejected by default"))
      (let* ((start (hermes-transport--get-any
                     params '(start start_line)))
             (end (hermes-transport--get-any
                   params '(end end_line)))
             (slice (hermes-capabilities--buffer-slice buffer start end))
             (content (plist-get slice :content))
             (truncated (plist-get slice :truncated-by-lines))
             (char-trunc (hermes-capabilities--truncate-by-chars
                          content hermes-capabilities-buffer-read-max-chars)))
        (when (car char-trunc)
          (setq truncated t))
        `((ok . t)
          (content . ,(cdr char-trunc))
          (metadata
           . ((buffer . ,name)
              (truncated . ,(hermes-capabilities--json-bool truncated))
              (line_count . ,(1+ (- (plist-get slice :end-line)
                                    (plist-get slice :start-line))))
              (total_lines . ,(plist-get slice :total-lines))
              (start_line . ,(plist-get slice :start-line))
              (end_line . ,(plist-get slice :end-line)))))))))

(defun hermes-capabilities--handle-capabilities-list (_params)
  "Return the `capabilities.list' result alist for runtime discovery.
Reports registered method descriptors and a count."
  (let ((methods (hermes-capabilities--method-descriptors)))
    `((methods . ,methods)
      (count . ,(hash-table-count methods)))))


;;;; Method registration (C5)

(hermes-capabilities-define hermes-capabilities-method-buffer-list
  "buffer.list" "List live non-internal buffers with mode, point, and file."
  #'hermes-capabilities--handle-buffer-list)

(hermes-capabilities-define hermes-capabilities-method-buffer-current
  "buffer.current" "Return the current buffer's name, mode, point, and file."
  #'hermes-capabilities--handle-buffer-current)

(hermes-capabilities-define hermes-capabilities-method-project-current
  "project.current" "Return the active project root and name, or nulls."
  #'hermes-capabilities--handle-project-current)

(hermes-capabilities-define hermes-capabilities-method-buffer-read
  "buffer.read"
  "Read a bounded slice of a buffer.  Params: buffer (required), start, end."
  #'hermes-capabilities--handle-buffer-read)

(hermes-capabilities-define hermes-capabilities-method-capabilities-list
  "capabilities.list" "List registered capability methods for runtime discovery."
  #'hermes-capabilities--handle-capabilities-list)



;;;; Lifecycle entry points
(defun hermes-capabilities--teardown-hook ()
  "Disconnect the active provider when its owning buffer is killed."
  (when hermes-capabilities--provider
    (hermes-capabilities--teardown hermes-capabilities--provider)
    (setq hermes-capabilities--provider nil)))

;;;###autoload
(defun hermes-capabilities-start (&optional buffer)
  "Start the capability provider in BUFFER (default: current buffer).
Register as `hermes-capabilities-target' over a dedicated `/api/ws' connection.
Refuse when `hermes-capabilities-enabled' is nil.  The provider's lifecycle is
tied to BUFFER: killing it tears the connection down.  No session identity is
stored on the shared chat client."
  (interactive)
  (unless hermes-capabilities-enabled
    (user-error "Set `hermes-capabilities-enabled' to t before starting"))
  (when hermes-capabilities--provider
    (hermes-capabilities--teardown hermes-capabilities--provider))
  (let* ((home (or buffer (current-buffer)))
         (target hermes-capabilities-target)
         (provider (hermes-capabilities--provider-create
                    :buffer home
                    :target target
                    :instance-id (hermes-capabilities--instance-id target)
                    :display-name (hermes-capabilities--display-name target)
                    :role hermes-capabilities-role)))
    (setq hermes-capabilities--provider provider)
    (with-current-buffer home
      (add-hook 'kill-buffer-hook #'hermes-capabilities--teardown-hook nil t))
    (hermes-capabilities--connect provider)
    (message "Hermes capabilities provider starting as %s" target)
    provider))

(defun hermes-capabilities-stop ()
  "Stop the active capability provider and close its connection."
  (interactive)
  (when hermes-capabilities--provider
    (hermes-capabilities--teardown hermes-capabilities--provider)
    (setq hermes-capabilities--provider nil)
    (message "Hermes capabilities provider stopped")))

(provide 'hermes-capabilities)
;;; hermes-capabilities.el ends here
