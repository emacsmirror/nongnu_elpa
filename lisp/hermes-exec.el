;;; hermes-exec.el --- HTTP-JSON eval endpoint for the Hermes bridge  -*- lexical-binding: t; -*-

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

;; A small, auditable HTTP-JSON endpoint that a remote Hermes FastMCP
;; bridge POSTs Emacs Lisp source to.  The MCP protocol lives entirely in
;; the Python bridge; this file speaks plain HTTP-JSON only.
;;
;; Contract:
;;   POST /eval HTTP/1.1 with `Content-Type: application/json' and body
;;   {"code":"<elisp source>"}.  Evaluation replies use HTTP 200 with a JSON
;;   body of either {"ok":true,"result":"..."} or {"ok":false,"error":"..."}.
;;   Authentication, framing, size, capacity and timeout refusals use HTTP
;;   error statuses.  One Content-Length-framed request is served per socket;
;;   subsequent bytes are discarded and the response closes the connection.
;;
;; The endpoint reuses `hermes-dashboard-transport.el' for URL parsing, the
;; loopback-host predicate, and secret redaction so it never binds a public
;; interface by accident and never echoes credentials back to the bridge.

;;; Code:

(require 'hermes-buffer)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'hermes-dashboard-transport)

(defgroup hermes-exec nil
  "HTTP-JSON Emacs Lisp eval endpoint for the Hermes bridge."
  :group 'hermes)

(defcustom hermes-exec-enabled nil
  "Master switch for the Hermes eval endpoint.
`hermes-exec-start' refuses to start while this is nil, and a running endpoint
refuses to evaluate, so the server can never come up or run implicitly."
  :type 'boolean)

(defcustom hermes-exec-require-approval t
  "Policy controlling whether an incoming eval request is confirmed.
Modeled on `org-confirm-babel-evaluate'.  The value is one of:

  t         always ask before evaluating (the default);
  nil       evaluate without asking;
  function  a predicate called with the request code string that returns
            `deny' to refuse the request outright, nil to evaluate it
            without asking, or any other non-nil value to prompt.

The prompt is shown in a dedicated buffer answered with a key and no longer
blocks Emacs while it waits.  `hermes-exec-confirm-by-risk' is a ready-made
predicate; `hermes-exec-trust', `hermes-exec-yolo', and `hermes-exec-untrust'
switch between risk-based, never-ask, and always-ask policies."
  :type '(choice (const :tag "Always ask" t)
                 (const :tag "Never ask (run unsupervised)" nil)
                 (function :tag "Predicate of the code")))
;; Like `org-confirm-babel-evaluate', only the always-ask value may be set from
;; a file-local or dir-local variable, so a checked-out file cannot quietly
;; relax the gate.
(put 'hermes-exec-require-approval 'safe-local-variable (lambda (x) (eq x t)))

(defcustom hermes-exec-host nil
  "Interface the eval endpoint binds to, or nil to auto-resolve.
When nil and `hermes-dashboard-transport-url' names a loopback dashboard, the
endpoint binds \"127.0.0.1\".  When nil and the dashboard is remote,
`hermes-exec-start' errors and asks the user to set this to their Tailscale IP.
Only IPv4/IPv6 loopback and addresses reported by local Tailscale are accepted."
  :type '(choice (const :tag "Auto-resolve from dashboard" nil)
                 (string :tag "Bind address")))

(defcustom hermes-exec-port 8237
  "TCP port the Hermes eval endpoint listens on."
  :type 'integer)

(defcustom hermes-exec-max-output 100000
  "Maximum number of characters returned for an eval result.
Longer printed results are truncated to this length before being sent back."
  :type 'integer)

(defcustom hermes-exec-max-request-bytes 1048576
  "Maximum size in bytes of a single incoming HTTP request.
Once a connection's accumulated bytes exceed this the endpoint answers 413 and
closes, so a partial or oversized request cannot grow the per-connection buffer
without bound."
  :type 'integer)

(defcustom hermes-exec-timeout 120
  "Seconds an evaluation may run before `with-timeout' aborts it.
`with-timeout' fires from a timer, so it only interrupts code that waits or
checks input; a CPU-bound loop that never yields runs to completion anyway."
  :type 'number)

(defcustom hermes-exec-max-pending 16
  "Maximum number of eval requests that may await approval at once.
A request that would exceed this is declined immediately rather than queued, so
a misbehaving bridge cannot grow the approval queue without bound.
This does not bound incomplete requests; see `hermes-exec-max-connections'."
  :type 'integer)

(defcustom hermes-exec-max-connections 32
  "Maximum live accepted connections per eval listener.
Reject excess accepts with HTTP 503, independently of the approval queue.
This bounds retained clients, not OS backlog or connection-attempt rate."
  :type 'natnum)

(defcustom hermes-exec-request-timeout 120
  "Seconds from accept until an eval connection expires with HTTP 408.
Input progress does not extend this budget, which includes approval wait.
Timers run only when Emacs services its event loop: this is not a hard CPU
limit, and closing a socket cannot undo an evaluation already in progress."
  :type 'number)

(defcustom hermes-exec-idle-timeout 15
  "Seconds without input before an incomplete eval request expires.
Reset on received bytes; cancel once the first request is complete.
The independent `hermes-exec-request-timeout' still bounds slow trickles and
approval wait.  Both timeout options must be positive numbers."
  :type 'number)

(defcustom hermes-exec-token nil
  "Shared bearer token the eval endpoint requires, or nil for loopback-only.
When nil the endpoint trusts its bind host: `hermes-exec-start' refuses to bind
anything but a loopback interface, and existing non-loopback connections
refuse requests if the token is removed.  When set to a
string, the endpoint may bind an address reported by local Tailscale and each
request must present a matching `Authorization: Bearer' header.  Falls back to
the EMACS_EXEC_TOKEN environment variable when nil, so the Python bridge and
this endpoint can share one secret without duplicating it in config."
  :type '(choice (const :tag "Loopback-only, no token" nil)
                 (string :tag "Required bearer token")))

(defvar hermes-exec--process nil
  "The live eval endpoint server process, or nil when stopped.")

(defvar hermes-exec--connection nil
  "Connection served in the current filter call, or nil.
Bound by `hermes-exec--filter' so the eval path can skip evaluation when the
client has already disconnected -- e.g. when a slow approval outlives the
bridge timeout -- rather than running the side effect on a dead socket.")

(defvar hermes-exec--pending nil
  "FIFO list of (:id ID :proc PROC :code CODE) requests awaiting approval.
ID is a unique symbol retained across display metadata updates.")

(defvar hermes-exec--active nil
  "The request owning approval construction or display, or nil.
Its :buffer is nil until construction finishes for the same live owner.")

;;; Host resolution

(defun hermes-exec--dashboard-loopback-p ()
  "Return non-nil when `hermes-dashboard-transport-url' names a loopback host."
  (hermes-dashboard-transport--loopback-host-p
   (plist-get (hermes-dashboard-transport--parse-url
               hermes-dashboard-transport-url)
              :host)))

(defun hermes-exec--resolve-host ()
  "Return the bind host for the endpoint, or nil when it cannot be resolved.
Prefer `hermes-exec-host'; otherwise bind loopback only when the configured
dashboard is itself loopback.  A remote dashboard with no explicit host returns
nil so the caller can refuse to bind a public interface."
  (cond
   ((and (stringp hermes-exec-host)
         (not (string-empty-p (string-trim hermes-exec-host))))
    (string-trim hermes-exec-host))
   ((ignore-errors (hermes-exec--dashboard-loopback-p)) "127.0.0.1")
   (t nil)))

(defun hermes-exec--ipv4-octets (host)
  "Return HOST's four IPv4 octets, or nil when HOST is not an IPv4 address."
  (let ((parts (split-string host "\\.")))
    (when (and (= (length parts) 4)
               (cl-every (lambda (part)
                           (and (string-match-p "\\`[0-9]+\\'" part)
                                (<= (string-to-number part) 255)))
                         parts))
      (mapcar #'string-to-number parts))))

(defun hermes-exec--tailscale-ipv4-addresses ()
  "Return IPv4 addresses assigned to this host by Tailscale."
  (condition-case nil
      (cl-remove-if-not #'hermes-exec--ipv4-octets
                        (process-lines "tailscale" "ip" "-4"))
    (error nil)))

(defun hermes-exec--loopback-host-p (host)
  "Return non-nil when HOST names an IPv4 or IPv6 loopback address."
  (or (hermes-dashboard-transport--loopback-host-p host)
      (eq (car (hermes-exec--ipv4-octets host)) 127)))

(defun hermes-exec--allowed-bind-host-p (host)
  "Return non-nil when HOST is safe for the eval endpoint to bind."
  (or (hermes-exec--loopback-host-p host)
      (member host (hermes-exec--tailscale-ipv4-addresses))))

;;; HTTP request parsing (pure)

(defun hermes-exec--parse-request-line (line)
  "Return a plist of :method and :path parsed from request LINE."
  (let ((parts (split-string (string-trim line) " " t)))
    (list :method (nth 0 parts) :path (nth 1 parts))))

(defun hermes-exec--parse-headers (header-block)
  "Return an alist of lowercased header names to values from HEADER-BLOCK."
  (cl-loop for line in (split-string header-block "\r?\n" t)
           when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
           collect (cons (downcase (string-trim (match-string 1 line)))
                         (string-trim (match-string 2 line)))))

(defun hermes-exec--content-length (headers)
  "Return the unique nonnegative Content-Length in HEADERS, or nil.
Signal an error for ambiguous framing or unsupported transfer encoding."
  (let ((lengths (cl-remove-if-not
                  (lambda (header) (equal (car header) "content-length"))
                  headers)))
    (when (or (assoc "transfer-encoding" headers) (cdr lengths))
      (error "Unsupported HTTP framing"))
    (when-let* ((value (cdar lengths)))
      (unless (string-match-p (rx string-start (+ digit) string-end) value)
        (error "Invalid Content-Length"))
      (string-to-number value))))

(defun hermes-exec--parse-request (raw)
  "Return the first request parsed from RAW, or nil when incomplete.
The plist has :method, :path, :headers, and UTF-8 decoded :body.  POST needs
one valid Content-Length; transfer encoding is unsupported.  Signal an
error for invalid framing.  Ignore bytes beyond the declared first body:
this endpoint serves exactly one request per connection, never pipelines."
  (let ((bytes (if (multibyte-string-p raw)
                   (encode-coding-string raw 'utf-8-unix) raw)))
    (when-let* ((split (string-search "\r\n\r\n" bytes)))
      (let* ((head (decode-coding-string (substring bytes 0 split) 'utf-8-unix))
             (body-start (+ split 4))
             (lines (split-string head "\r?\n"))
             (request-line (hermes-exec--parse-request-line (car lines)))
             (headers (hermes-exec--parse-headers
                       (string-join (cdr lines) "\n")))
             (length (hermes-exec--content-length headers)))
        (when (and (equal (plist-get request-line :method) "POST") (null length))
          (error "POST requires Content-Length"))
        (when (>= (- (length bytes) body-start) (or length 0))
          (append request-line
                  (list :headers headers
                        :body (decode-coding-string
                               (substring bytes body-start
                                          (+ body-start (or length 0)))
                               'utf-8-unix))))))))

;;; Authentication
;;
;; Network reachability is the primary control: the endpoint binds loopback or
;; an address Tailscale reports as assigned locally.  A shared bearer token is
;; the enforced second layer for the non-loopback case, checked here against the
;; parsed request before evaluation.  Over plain HTTP the token authenticates but
;; does not encrypt.  WireGuard protects Tailscale traffic in transit, not
;; application authorization among reachable peers.  The bearer token also
;; restricts other local users on loopback, but does not isolate code running
;; as this Emacs user.  Approval permits arbitrary Lisp, not sandboxed code.

(defun hermes-exec--expected-token ()
  "Return the configured bearer token as a non-empty string, or nil.
Prefer `hermes-exec-token'; fall back to the EMACS_EXEC_TOKEN environment
variable so the bridge and endpoint can share one secret."
  (let ((token (or (and (stringp hermes-exec-token) hermes-exec-token)
                   (getenv "EMACS_EXEC_TOKEN"))))
    (and (stringp token)
         (not (string-empty-p (string-trim token)))
         (string-trim token))))

(defun hermes-exec--secure-equal (a b)
  "Return non-nil when strings A and B have equal SHA-256 digests.
Hash UTF-8 bytes first, then scan all 32 digest bytes without prefix or token
length short-circuiting.  Hash cost still depends on input length; Emacs Lisp
and its runtime provide no constant-time guarantee.  This reduces obvious
comparison leakage rather than proving resistance to timing attacks."
  (and (stringp a) (stringp b)
       (let ((left (secure-hash 'sha256 (encode-coding-string a 'utf-8-unix)
                                nil nil t))
             (right (secure-hash 'sha256 (encode-coding-string b 'utf-8-unix)
                                 nil nil t))
             (diff 0))
         (dotimes (i 32)
           (setq diff (logior diff (logxor (aref left i) (aref right i)))))
         (zerop diff))))

(defun hermes-exec--request-bearer (request)
  "Return the bearer token from REQUEST's Authorization header, or nil."
  (and-let* ((value (cdr (assoc "authorization" (plist-get request :headers))))
             (trimmed (string-trim value))
             ((string-match "\\`[Bb]earer[ \t]+\\(.+\\)\\'" trimmed)))
    (string-trim (match-string 1 trimmed))))

(defun hermes-exec--loopback-process-p (proc)
  "Return non-nil when PROC's actual local socket address is loopback.
Unknown addresses fail closed; never consult mutable bind configuration."
  (let ((address (and (processp proc)
                      (ignore-errors (process-contact proc :local)))))
    (and (vectorp address)
         (or (and (= (length address) 5) (eql (aref address 0) 127))
             (and (= (length address) 9)
                  (equal (substring address 0 8) [0 0 0 0 0 0 0 1]))))))

(defun hermes-exec--loopback-request-p ()
  "Return non-nil when the current request has a loopback socket boundary.
For accepted connections check both the socket and its exact listener, even
when that listener has died.  A missing socket identity fails closed."
  (let* ((proc (or hermes-exec--connection hermes-exec--process))
         (server (and (processp proc)
                      (process-get proc 'hermes-exec-connection))))
    (and (hermes-exec--loopback-process-p proc)
         (or (not hermes-exec--connection)
             (hermes-exec--loopback-process-p server)))))

(defun hermes-exec--request-authorized-p (request)
  "Return non-nil when REQUEST may run.
Without a token require an actual loopback socket boundary.  Otherwise REQUEST
must carry a matching bearer authorization header."
  (let ((expected (hermes-exec--expected-token)))
    (if expected
        (and-let* ((presented (hermes-exec--request-bearer request)))
          (hermes-exec--secure-equal expected presented))
      (hermes-exec--loopback-request-p))))

;;; Trust policy and risk classification
;;
;; The approval policy mirrors `org-confirm-babel-evaluate': t always asks, nil
;; never asks, and a function decides per request.  The classifier is an honest
;; triage aid, not a security boundary -- it reads the request's top-level forms
;; and flags the function symbols it can see.  Dynamic dispatch (`eval',
;; `funcall', `apply') defeats static reading, so it is always treated as
;; sensitive, and unreadable code fails closed to a prompt.

(defcustom hermes-exec-confirm-functions
  '(delete-file delete-directory rename-file copy-file make-symbolic-link
                write-region append-to-file set-file-modes set-file-times
                shell-command shell-command-to-string async-shell-command
                call-process call-process-region start-process make-process
                start-process-shell-command call-process-shell-command
                url-retrieve url-retrieve-synchronously kill-emacs)
  "Function symbols whose presence makes `hermes-exec-confirm-by-risk' prompt."
  :type '(repeat symbol))

(defcustom hermes-exec-forbidden-functions nil
  "Function symbols `hermes-exec-confirm-by-risk' refuses to evaluate at all.
Empty by default; add symbols you never want run through the endpoint."
  :type '(repeat symbol))

(defconst hermes-exec--dynamic-dispatch
  '(eval funcall funcall-interactively apply macroexpand macroexpand-all)
  "Symbols that defeat static reading and so are always treated as sensitive.")

(defun hermes-exec--code-symbols (code)
  "Return the symbols appearing in CODE's top-level forms.
Signal a reader error when CODE cannot be parsed, so callers fail closed."
  (let ((forms (car (read-from-string (format "(progn %s\n)" code))))
        (seen '()))
    (cl-labels ((walk (form)
                  (cond
                   ((and form (symbolp form)) (cl-pushnew form seen))
                   ((consp form) (walk (car form)) (walk (cdr form))))))
               (walk forms))
    seen))

(defun hermes-exec--classify-code (code)
  "Classify CODE as `forbidden', `sensitive', or `ordinary'.
This is a triage aid, not a security boundary: it flags every symbol it sees,
so quoted data or shadowing bindings can over-classify.  Indirect effects
of apparently ordinary functions can escape classification.
Unreadable, dynamic-dispatch, and too-deeply-nested code all fail closed to
`sensitive' via the surrounding handler."
  (condition-case nil
      (let ((symbols (hermes-exec--code-symbols code)))
        (cond
         ((cl-intersection symbols hermes-exec-forbidden-functions) 'forbidden)
         ((or (cl-intersection symbols hermes-exec-confirm-functions)
              (cl-intersection symbols hermes-exec--dynamic-dispatch))
          'sensitive)
         (t 'ordinary)))
    (error 'sensitive)))

(defun hermes-exec-confirm-by-risk (code)
  "Approval predicate for `hermes-exec-require-approval' keyed on CODE's risk.
Return `deny' for forbidden forms, non-nil to prompt for sensitive forms, and
nil to evaluate ordinary forms without asking."
  (pcase (hermes-exec--classify-code code)
    ('forbidden 'deny)
    ('sensitive t)
    (_ nil)))

(defun hermes-exec--approval-decision (code)
  "Return how to handle CODE: `run', `ask', or `deny'.
Follow the `org-confirm-babel-evaluate' convention -- t asks, nil runs, and a
function returns nil to run or non-nil to prompt -- extended with a `deny'
return value that refuses the request outright.  Org expresses a hard block
through a source block's `:eval' header rather than the confirm variable."
  (let ((policy hermes-exec-require-approval))
    (cond
     ((functionp policy)
      (pcase (funcall policy code)
        ('deny 'deny)
        ('nil 'run)
        (_ 'ask)))
     (policy 'ask)
     (t 'run))))

;;;###autoload
(defun hermes-exec-trust ()
  "Trust the agent for ordinary forms; still prompt for sensitive ones.
Set `hermes-exec-require-approval' to `hermes-exec-confirm-by-risk'."
  (interactive)
  (setq hermes-exec-require-approval #'hermes-exec-confirm-by-risk)
  (message "Hermes eval: trust mode (ordinary forms run, sensitive ones prompt)"))

;;;###autoload
(defun hermes-exec-yolo ()
  "Trust the agent to evaluate every authorized request without prompting."
  (interactive)
  (setq hermes-exec-require-approval nil)
  (message "Hermes eval: yolo mode (requests run without approval)"))

;;;###autoload
(defun hermes-exec-untrust ()
  "Require approval for every eval request, the default policy."
  (interactive)
  (setq hermes-exec-require-approval t)
  (message "Hermes eval: every request requires approval"))

;;; Eval path

(defun hermes-exec--format-result (value)
  "Return VALUE printed, truncated, and redacted for transport."
  (let ((printed (format "%S" value)))
    (hermes-dashboard-transport--redact-secret
     (if (> (length printed) hermes-exec-max-output)
         (substring printed 0 hermes-exec-max-output)
       printed))))

(defun hermes-exec--eval-code (code)
  "Read and evaluate every top-level form in CODE under a timeout.
CODE may carry more than one form; all run in order and the last value is
returned.  Wrapping in `progn' avoids silently dropping every form after the
first the way a single `read-from-string' would.  The timeout is best-effort:
it cannot interrupt a CPU-bound form that never waits or checks input (see
`hermes-exec-timeout')."
  (with-timeout (hermes-exec-timeout
                 (error "Hermes eval timed out after %s seconds"
                        hermes-exec-timeout))
    (eval (car (read-from-string (format "(progn %s\n)" code))) t)))

(defun hermes-exec--evaluate (code)
  "Evaluate CODE and return a result plist.
The plist is (:ok t :result STRING) on success or (:ok nil :error STRING) when
reading or evaluation signals.  Errors are captured, never thrown."
  (condition-case err
      (list :ok t :result (hermes-exec--format-result
                           (hermes-exec--eval-code code)))
    (error (list :ok nil
                 :error (hermes-dashboard-transport--redact-secret
                         (error-message-string err))))))

(defun hermes-exec--evaluate-guarded (code)
  "Evaluate CODE only while endpoint and connection policy still permit it.
Recheck the master switch and non-loopback token requirement after approval
and policy callbacks, without retaining the request's bearer token."
  (cond
   ((not hermes-exec-enabled)
    (list :ok nil :error "Hermes eval endpoint is disabled"))
   ((and hermes-exec--connection
         (not (process-live-p hermes-exec--connection)))
    (list :ok nil :error "Client disconnected before evaluation"))
   ((and hermes-exec--connection
         (not (hermes-exec--expected-token))
         (not (hermes-exec--loopback-request-p)))
    (list :ok nil :error "Non-loopback evaluation requires a token"))
   (t (hermes-exec--evaluate code))))

;;; Asynchronous approval
;;
;; A request that needs confirmation is queued rather than answered inside the
;; network filter, so Emacs does not block while it waits for the user.  The
;; request's connection stays open; the response is written from the approval
;; command once the user acts.  Evaluation itself still runs synchronously on
;; the main thread once approved, bounded by `hermes-exec-timeout' -- only the
;; human wait moved off the filter.  The bridge's own request timeout closes the
;; socket if the user is too slow, which `hermes-exec--evaluate-guarded' and
;; `hermes-exec--send-response' both detect before touching it.

(defconst hermes-exec--approval-buffer-name "*Hermes Eval Request*"
  "Name of the transient buffer that shows code awaiting eval approval.")

(defun hermes-exec--fontify-elisp (code)
  "Return CODE fontified as Emacs Lisp via a throwaway buffer."
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (emacs-lisp-mode))
    (ignore-errors (font-lock-ensure))
    (buffer-string)))

(defun hermes-exec--peer-info (proc)
  "Return a \"HOST:PORT\" contact string for PROC, or nil when unavailable.
Network connections expose the endpoint via `process-contact'; pipe and
dead or not-yet-connected processes yield nil so the caller can show a generic
fallback."
  (when (and (processp proc) (process-live-p proc))
    (ignore-errors
      (let ((contact (process-contact proc nil t)))
        (and (consp contact)
             (format "%s:%s" (or (car contact) "?")
                     (or (cadr contact) "?")))))))

(defun hermes-exec--risk-label (risk)
  "Return a capitalized label for risk class RISK."
  (pcase risk
    ('forbidden "Forbidden")
    ('sensitive "Sensitive")
    (_ "Ordinary")))

(defun hermes-exec--format-metadata (request)
  "Return a list of (LABEL . VALUE) string pairs describing REQUEST."
  (let ((pairs `(("Risk" . ,(hermes-exec--risk-label
                             (or (plist-get request :risk) 'ordinary)))
                 ("Requester" . ,(or (plist-get request :peer) "local"))
                 ("Origin" . ,(let ((buf (plist-get request :origin-buffer)))
                                (if (buffer-live-p buf)
                                    (buffer-name buf)
                                  "(dead)")))
                 ("Window" . ,(let ((win (plist-get request :origin-window)))
                                (if (window-live-p win)
                                    (format "live: %s" (buffer-name
                                                        (window-buffer win)))
                                  "(dead)")))
                 ("Timeout" . ,(format "%ss" hermes-exec-timeout)))))
    (let ((total (plist-get request :queue-total)))
      (if (and (integerp total) (> total 1))
          (append pairs (list (cons "Queue" (format "1 of %d" total))))
        pairs))))

(defun hermes-exec--insert-metadata (request)
  "Insert a formatted metadata header for REQUEST into the current buffer."
  (let* ((pairs (hermes-exec--format-metadata request))
         (width (cl-loop for p in pairs maximize (length (car p)))))
    (dolist (pair pairs)
      (insert (propertize
               (concat (car pair)
                       (make-string (max 0 (- width (length (car pair)))) ?\s)
                       ": ")
               'face 'minibuffer-prompt)
              (cdr pair)
              "\n"))
    (insert "\n")))

(defvar-keymap hermes-exec-approval-mode-map
  :doc "Keymap for `hermes-exec-approval-mode'."
  "RET" #'hermes-exec-decide
  "a" #'hermes-exec-approve
  "y" #'hermes-exec-approve
  "d" #'hermes-exec-deny
  "n" #'hermes-exec-deny
  "q" #'hermes-exec-deny
  "c" #'hermes-exec-decide)

(define-derived-mode hermes-exec-approval-mode special-mode "Hermes-Eval"
  "Major mode for confirming a single Hermes eval request.
\\<hermes-exec-approval-mode-map>\\[hermes-exec-decide] shows the approval menu, \
\\[hermes-exec-approve] evaluates the request, \\[hermes-exec-deny] declines it."
  (setq-local header-line-format
              (substitute-command-keys
               "Hermes eval request: \\[hermes-exec-decide] decide, \
\\[hermes-exec-approve] approve, \\[hermes-exec-deny] deny")))

(defvar-local hermes-exec--approval-id nil
  "Exact request displayed by this approval buffer.")

(defun hermes-exec--approval-current-p (buffer id)
  "Return non-nil when BUFFER still owns active approval request ID."
  (and id (buffer-live-p buffer)
       (eq id (plist-get hermes-exec--active :id))
       (eq buffer (plist-get hermes-exec--active :buffer))
       (with-current-buffer buffer
         (and (hermes-buffer--owned-p 'hermes-exec-approval-mode)
              (eq id hermes-exec--approval-id)))))

(defun hermes-exec--render-approval (buffer request &optional queued)
  "Render REQUEST in approval BUFFER while its exact claim remains current.
When QUEUED, require REQUEST to remain the exact active queue occurrence.
Stage text before editing, then notify change hooks once around the replacement.
A before-change hook may retire the owner or render a newer snapshot; abandon
this edit in either case.  Never roll back text belonging to a new owner."
  (when (buffer-live-p buffer)
    (let ((claim (buffer-local-value 'hermes-buffer--owner buffer))
          (id (plist-get request :id))
          (tick (buffer-chars-modified-tick buffer))
          (retired (make-symbol "retired-render")))
      (cl-labels
          ((current-p ()
             (and (buffer-live-p buffer)
                  (or (not queued) (eq request hermes-exec--active))
                  (with-current-buffer buffer
                    (and (eq claim hermes-buffer--owner)
                         (hermes-buffer--owned-p 'hermes-exec-approval-mode)
                         (eq id hermes-exec--approval-id)))))
           (check ()
             (unless (and (current-p)
                          (= tick (buffer-chars-modified-tick buffer)))
               (throw retired nil))))
        (catch retired
          (check)
          (let ((text (with-temp-buffer
                        (hermes-exec--insert-metadata request)
                        (insert (hermes-exec--fontify-elisp
                                 (plist-get request :code)))
                        (buffer-string))))
            (check)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (combine-change-calls (point-min) (point-max)
                  ;; Native before-change hooks have now returned.  No callback
                  ;; may run between this check and the complete text edit.
                  (unless (eq (current-buffer) buffer) (throw retired nil))
                  (check)
                  (let ((inhibit-modification-hooks t)
                        (inhibit-quit t))
                    (erase-buffer)
                    (insert text))))
              ;; After-change hooks can also transfer or kill this buffer.
              (when (current-p)
                (with-current-buffer buffer (goto-char (point-min))))))))))
  buffer)

(defun hermes-exec--approval-buffer (request)
  "Return a read-only buffer showing REQUEST's code and metadata for approval.
REQUEST is a plist with at least :code; optional keys :risk, :peer,
:origin-buffer, and :queue-total drive the metadata header.  When called
interactively the buffer is backed by `hermes-exec-approval-mode'."
  (let* ((queued (eq request hermes-exec--active))
         (buffer (hermes-buffer--get hermes-exec--approval-buffer-name
                                     #'hermes-exec-approval-mode t)))
    (with-current-buffer buffer
      (setq hermes-exec--approval-id (plist-get request :id)))
    (hermes-exec--render-approval buffer request queued)))

(defun hermes-exec--close-approval-window (&optional buffer id)
  "Close owned approval BUFFER, defaulting to the active request's buffer.
When ID is non-nil, close only that request's view, never a replacement.
Remove the named reuse claim before window or buffer teardown can run hooks."
  (let ((buffer (or buffer (plist-get hermes-exec--active :buffer)))
        (claim (cons t 'hermes-exec-approval-mode)))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (hermes-buffer--owned-p 'hermes-exec-approval-mode)
                      (or (null id) (eq id hermes-exec--approval-id)))))
      ;; Keep an exact, non-reusable claim until this teardown completes.
      (with-current-buffer buffer (setq hermes-buffer--owner claim))
      (let ((window (get-buffer-window buffer)))
        (when window
          (quit-restore-window window)))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (eq hermes-buffer--owner claim)
                        (hermes-buffer--owned-p 'hermes-exec-approval-mode)
                        (or (null id) (eq id hermes-exec--approval-id)))))
        (kill-buffer buffer)))))

(defun hermes-exec--display-approval (buffer)
  "Display approval BUFFER in a bottom side window without stealing focus.
Uses `display-buffer-in-side-window' so the user's working window is preserved."
  (let* ((selected (selected-window))
         (window (display-buffer
                  buffer
                  '(display-buffer-in-side-window
                    . ((side . bottom) (window-height . fit-window-to-buffer)
                       (slot . 0) (preserve-size . (nil . t)))))))
    (when (window-live-p selected)
      (select-window selected))
    window))

(defun hermes-exec--approval-choices ()
  "Return the `read-multiple-choice' choice list for the active request.
Offers approve once / deny / view, plus trust when risk-based policy is not
already active."
  (let ((choices '((?a "approve once" "approve once - evaluate this request")
                   (?d "deny" "deny - decline this request")
                   (?v "view" "view - inspect the full request"))))
    (if (eq hermes-exec-require-approval #'hermes-exec-confirm-by-risk)
        choices
      (append choices
              '((?t "trust for this session"
                   "trust for this session - ordinary forms run, sensitive ones still prompt"))))))

(defun hermes-exec--prompt-choice (&optional buffer id)
  "Prompt for approval BUFFER and request ID, defaulting to the active pair.
Maps `read-multiple-choice' to `hermes-exec--resolve-active'.  The \"view\"
choice selects the approval buffer for inspection; the user can press
\<hermes-exec-approval-mode-map>\[hermes-exec-decide] there to decide later.
Ignore answers when the displayed request was replaced while waiting."
  (when-let* (((not noninteractive))
              (buffer (or buffer (plist-get hermes-exec--active :buffer)))
              (id (or id (plist-get hermes-exec--active :id)))
              ((hermes-exec--approval-current-p buffer id)))
    (let (done)
      (while (not done)
        (let ((choice (read-multiple-choice
                       "Hermes eval approval" (hermes-exec--approval-choices))))
          ;; Input waits run sentinels and timers.  Display metadata may change,
          ;; but only the exact queued request can own the answer.
          (pcase (and (hermes-exec--approval-current-p buffer id)
                      (car choice))
            (?a
             (hermes-exec--resolve-active t)
             (setq done t))
            (?d
             (hermes-exec--resolve-active nil)
             (setq done t))
            (?t
             (hermes-exec-trust)
             (if (eq (plist-get hermes-exec--active :risk) 'ordinary)
                 (progn
                   (hermes-exec--resolve-active t)
                   (setq done t))
               (message "Trust enabled; this request still needs approve/deny")))
            (?v
             (let ((buffer (plist-get hermes-exec--active :buffer)))
               (when (buffer-live-p buffer)
                 (pop-to-buffer buffer)
                 (message "Inspect request, then press %s to decide"
                          (substitute-command-keys
                           "\\[hermes-exec-decide]"))))
             (setq done t))
            (_ (setq done t))))))))

(defun hermes-exec--maybe-prompt ()
  "Defer `hermes-exec--prompt-choice' until after the network filter completes.
The timer fires after the current event loop iteration, so the HTTP filter
that enqueued the request completes before the modal prompt blocks Emacs."
  (unless noninteractive
    (run-with-timer 0 nil #'hermes-exec--prompt-choice
                    (plist-get hermes-exec--active :buffer)
                    (plist-get hermes-exec--active :id))))

(defun hermes-exec--finish-active ()
  "Close the approval window and clear the active slot."
  (when-let* ((active hermes-exec--active))
    (setq hermes-exec--active nil)
    (when (plist-get active :buffer)
      (hermes-exec--close-approval-window
       (plist-get active :buffer) (plist-get active :id)))))

(defun hermes-exec--queue-total ()
  "Return the total count of active plus pending requests."
  (+ (length hermes-exec--pending) (if hermes-exec--active 1 0)))

(defun hermes-exec--refresh-active-buffer ()
  "Refresh the active approval buffer's queue metadata."
  (when hermes-exec--active
    (setq hermes-exec--active
          (plist-put hermes-exec--active :queue-total
                     (hermes-exec--queue-total)))
    (let ((buffer (plist-get hermes-exec--active :buffer)))
      (when (hermes-exec--approval-current-p
             buffer (plist-get hermes-exec--active :id))
        (hermes-exec--render-approval buffer hermes-exec--active t)))))

(defun hermes-exec--show-next ()
  "Display the next queued request when none is currently shown.
Reserve its active slot through construction and recheck after mode hooks.
Skip disconnected requests without taking a replacement owner's display."
  (when (and (null hermes-exec--active) hermes-exec--pending)
    (let* ((next (pop hermes-exec--pending))
           (id (plist-get next :id))
           (proc (plist-get next :proc))
           (server hermes-exec--process)
           (owner (process-get proc 'hermes-exec-connection)))
      (if (not (process-live-p proc))
          (hermes-exec--show-next)
        (let ((request (append next (list :buffer nil :queue-total
                                         (1+ (hermes-exec--queue-total)))))
              buffer published)
          (setq hermes-exec--active request)
          (unwind-protect
              (progn
                (setq buffer (hermes-exec--approval-buffer request))
                (when (and (eq id (plist-get hermes-exec--active :id))
                           (eq proc (plist-get hermes-exec--active :proc))
                           (eq server hermes-exec--process)
                           (eq owner (process-get proc 'hermes-exec-connection))
                           (process-live-p proc))
                  (setf (plist-get hermes-exec--active :buffer) buffer)
                  (when (hermes-exec--approval-current-p buffer id)
                    (setq published t)
                    (hermes-exec--display-approval buffer)
                    (when (hermes-exec--approval-current-p buffer id)
                      (hermes-exec--maybe-prompt)))))
            (unless published
              (when (eq id (plist-get hermes-exec--active :id))
                (setq hermes-exec--active nil))
              (when buffer (hermes-exec--close-approval-window buffer id))
              (when (eq server hermes-exec--process)
                (hermes-exec--show-next)))))))))

(defun hermes-exec--enqueue-approval (proc code &rest metadata)
  "Queue CODE from PROC for approval and show it when nothing else is pending.
METADATA is a plist appended to the queued request for the approval display
and eval path: :origin-buffer, :origin-window, :peer, and :risk.  Decline
immediately when the queue is already at `hermes-exec-max-pending', counting
the request currently shown, so the queue cannot grow without bound."
  (if (>= (+ (length hermes-exec--pending) (if hermes-exec--active 1 0))
          hermes-exec-max-pending)
      (hermes-exec--send-response
       proc (hermes-exec--http-response
             200 "OK" (hermes-exec--result-json
                       (list :ok nil :error "Too many pending eval requests"))))
    (setq hermes-exec--pending
          (append hermes-exec--pending
                  (list (append (list :id (make-symbol "eval-request")
                                      :proc proc :code code)
                                metadata))))
    (if hermes-exec--active
        (hermes-exec--refresh-active-buffer)
      (hermes-exec--show-next))))

(defun hermes-exec--eval-with-origin (code origin-buffer origin-window)
  "Evaluate CODE with ORIGIN-WINDOW or ORIGIN-BUFFER current when live.
Prefer the captured live window so selected-window-sensitive forms behave as if
run from the original cockpit.  Fall back to the origin buffer, then the current
buffer, when that window or buffer has died."
  (cond
   ((window-live-p origin-window)
    (with-selected-window origin-window
      (hermes-exec--evaluate-guarded code)))
   ((buffer-live-p origin-buffer)
    (with-current-buffer origin-buffer
      (hermes-exec--evaluate-guarded code)))
   (t (hermes-exec--evaluate-guarded code))))

(defun hermes-exec--resolve-active (approve)
  "Evaluate the active request when APPROVE, else decline it, then respond.
Consume the active slot before evaluating, so a sentinel that fires during the
eval -- for instance when the approved code closes its own connection -- cannot
re-enter and advance the queue twice.  Evaluation runs in the origin buffer
captured at enqueue time when it is still live, so current-buffer/region
operations do not accidentally run in the approval buffer.  Advance to the next
request afterwards, even on quit, but never advance a replacement listener."
  (when hermes-exec--active
    (let* ((active hermes-exec--active)
           (proc (plist-get active :proc))
           (server hermes-exec--process)
           (code (plist-get active :code))
           (origin-buffer (plist-get active :origin-buffer))
           (origin-window (plist-get active :origin-window)))
      (setq hermes-exec--active nil)
      (unwind-protect
          (progn
            (hermes-exec--close-approval-window
             (plist-get active :buffer) (plist-get active :id))
            (let ((result (if approve
                              (let ((hermes-exec--connection proc))
                                (hermes-exec--eval-with-origin
                                 code origin-buffer origin-window))
                            (list :ok nil :error "Evaluation declined by user"))))
              (hermes-exec--send-response
               proc (hermes-exec--http-response
                     200 "OK" (hermes-exec--result-json result)))))
        ;; Preserve nonlocal exits, but settle only the captured connection.
        (when (process-live-p proc)
          (ignore-errors (delete-process proc)))
        (when (eq server hermes-exec--process)
          (hermes-exec--show-next))))))

(defun hermes-exec--drop-pending (proc)
  "Drop PROC from the approval queue, advancing the display if it was active."
  (setq hermes-exec--pending
        (cl-remove proc hermes-exec--pending
                   :key (lambda (e) (plist-get e :proc))))
  (when (and hermes-exec--active
             (eq proc (plist-get hermes-exec--active :proc)))
    (hermes-exec--finish-active)
    (hermes-exec--show-next)))

(defun hermes-exec-approve ()
  "Approve the eval request shown in the current approval buffer."
  (interactive nil hermes-exec-approval-mode)
  (when (hermes-exec--approval-current-p (current-buffer) hermes-exec--approval-id)
    (hermes-exec--resolve-active t)))

(defun hermes-exec-deny ()
  "Decline the eval request shown in the current approval buffer."
  (interactive nil hermes-exec-approval-mode)
  (when (hermes-exec--approval-current-p (current-buffer) hermes-exec--approval-id)
    (hermes-exec--resolve-active nil)))

(defun hermes-exec-decide ()
  "Read an Emacs-native multiple-choice decision for the active eval request."
  (interactive nil hermes-exec-approval-mode)
  (when (hermes-exec--approval-current-p (current-buffer) hermes-exec--approval-id)
    (hermes-exec--prompt-choice (current-buffer) hermes-exec--approval-id)))

(defun hermes-exec--eval-outcome (code)
  "Return the eval result plist for CODE, or the symbol `defer'.
Refuse when the endpoint is disabled or policy denies the code, evaluate when
policy runs it, and return `defer' when it must wait for asynchronous approval."
  (cond
   ((not hermes-exec-enabled)
    (list :ok nil :error "Hermes eval endpoint is disabled"))
   (t (pcase (hermes-exec--approval-decision code)
        ('run (hermes-exec--evaluate-guarded code))
        ('deny (list :ok nil :error "Evaluation declined by policy"))
        ('ask 'defer)))))

;;; JSON request/response

(defun hermes-exec--code-from-body (body)
  "Return the \"code\" string from JSON BODY, or signal a clear error."
  (let* ((object (json-parse-string body :object-type 'alist))
         (code (cdr (assq 'code object))))
    (unless (stringp code)
      (error "Request JSON missing string \"code\" field"))
    code))

(defun hermes-exec--result-json (result)
  "Return RESULT plist serialized as a JSON response body."
  (json-serialize
   (if (plist-get result :ok)
       `((ok . t) (result . ,(plist-get result :result)))
     `((ok . :false) (error . ,(plist-get result :error))))))

(defun hermes-exec--eval-response-body (body)
  "Return the JSON response body for an /eval request BODY, or a defer signal.
The value is a JSON string for the disabled, denied, and run-now paths, and the
list (:defer CODE) when the request must wait for asynchronous approval."
  (condition-case err
      (let* ((code (hermes-exec--code-from-body body))
             (outcome (hermes-exec--eval-outcome code)))
        (if (eq outcome 'defer)
            (list :defer code)
          (hermes-exec--result-json outcome)))
    (error (json-serialize
            `((ok . :false)
              (error . ,(hermes-dashboard-transport--redact-secret
                         (error-message-string err))))))))

;;; HTTP response building (pure)

(defun hermes-exec--http-response (status reason body)
  "Return a full HTTP/1.1 response string for STATUS, REASON, and JSON BODY."
  (let ((bytes (string-bytes body)))
    (concat (format "HTTP/1.1 %d %s\r\n" status reason)
            "Content-Type: application/json\r\n"
            (format "Content-Length: %d\r\n" bytes)
            "Connection: close\r\n"
            "\r\n"
            body)))

(defun hermes-exec--dispatch (request)
  "Return the HTTP response string for parsed REQUEST."
  (cond
   ((not (hermes-exec--request-authorized-p request))
    (hermes-exec--http-response
     401 "Unauthorized" (json-serialize '((ok . :false) (error . "unauthorized")))))
   ((and (equal (plist-get request :method) "POST")
         (equal (plist-get request :path) "/eval"))
    (let ((outcome (hermes-exec--eval-response-body (plist-get request :body))))
      ;; A (:defer CODE) outcome is passed up to the filter, which queues the
      ;; request for approval and responds later; everything else is a JSON
      ;; body wrapped here and sent immediately.
      (if (eq (car-safe outcome) :defer)
          outcome
        (hermes-exec--http-response 200 "OK" outcome))))
   (t (hermes-exec--http-response
       404 "Not Found"
       (json-serialize '((ok . :false) (error . "not found")))))))

;;; Server IO

(defun hermes-exec--send-response (proc response)
  "Send RESPONSE on PROC and close the connection."
  (when (process-live-p proc)
    (ignore-errors (process-send-string proc response))
    (ignore-errors (delete-process proc))))

(defun hermes-exec--cancel-connection-timer (proc property)
  "Retire the timer in PROC's PROPERTY before cancelling it."
  (when-let* ((timer (process-get proc property)))
    (process-put proc property nil)
    (cancel-timer timer)))

(defun hermes-exec--arm-connection-timer (proc property seconds)
  "Set PROC's PROPERTY to a fresh timeout after SECONDS for its exact owner."
  (hermes-exec--cancel-connection-timer proc property)
  (let ((server (process-get proc 'hermes-exec-connection))
        timer)
    (when server
      (setq timer
            (run-at-time
             seconds nil
             (lambda ()
               (when (and (process-live-p proc)
                          (eq server (process-get proc 'hermes-exec-connection))
                          (eq timer (process-get proc property)))
                 (hermes-exec--send-response
                  proc (hermes-exec--http-response
                        408 "Request Timeout"
                        "{\"ok\":false,\"error\":\"request timed out\"}"))))))
      (process-put proc property timer))))

(defun hermes-exec--consume-request ()
  "Retire input before dispatch can yield or run arbitrary evaluation code."
  (when hermes-exec--connection
    (process-put hermes-exec--connection 'hermes-exec-consumed t)
    (process-put hermes-exec--connection 'hermes-buffer nil)
    (hermes-exec--cancel-connection-timer
     hermes-exec--connection 'hermes-exec-idle-timer)))

(defun hermes-exec--request-response (buffer)
  "Return a response for accumulated BUFFER, or nil for more bytes.
Reject oversized input with 413 and invalid framing with 400.  Dispatch only
the first request; retire its input before any policy callback or evaluation."
  (if (> (string-bytes buffer) hermes-exec-max-request-bytes)
      (hermes-exec--http-response
       413 "Payload Too Large"
       "{\"ok\":false,\"error\":\"request too large\"}")
    (let ((request (condition-case nil (hermes-exec--parse-request buffer)
                     (error 'invalid))))
      (cond
       ((eq request 'invalid)
        (hermes-exec--http-response
         400 "Bad Request" "{\"ok\":false,\"error\":\"invalid HTTP framing\"}"))
       (request
        (hermes-exec--consume-request)
        (hermes-exec--dispatch request))))))

(defun hermes-exec--deliver-outcome (proc outcome origin-buffer origin-window)
  "Send OUTCOME to PROC or queue it under ORIGIN-BUFFER and ORIGIN-WINDOW."
  (if (eq (car-safe outcome) :defer)
      (let ((code (cadr outcome)))
        (when (process-live-p proc)
          (hermes-exec--enqueue-approval
           proc code :origin-buffer origin-buffer :origin-window origin-window
           :peer (hermes-exec--peer-info proc)
           :risk (hermes-exec--classify-code code))))
    (hermes-exec--send-response proc outcome)))

(defun hermes-exec--filter (proc chunk)
  "Accumulate CHUNK on PROC and dispatch at most one request.
Discard trailing input, whether coalesced with the first body or received
later during approval/evaluation.  Never retain it or create another request.
The receive-size cap applies before parsing, including coalesced trailing
bytes; over-limit input is rejected even if its first request would fit."
  (unless (process-get proc 'hermes-exec-consumed)
    (let* ((origin-window (selected-window))
           (origin-buffer (and (window-live-p origin-window)
                               (window-buffer origin-window)))
           (buffer (concat (process-get proc 'hermes-buffer) chunk))
           (hermes-exec--connection proc))
      (process-put proc 'hermes-buffer buffer)
      (hermes-exec--arm-connection-timer
       proc 'hermes-exec-idle-timer hermes-exec-idle-timeout)
      (condition-case err
          (when-let* ((outcome (hermes-exec--request-response buffer)))
            (process-put proc 'hermes-buffer nil)
            (hermes-exec--deliver-outcome proc outcome origin-buffer origin-window))
        (quit
         (when (process-live-p proc)
           (ignore-errors (delete-process proc)))
         (signal (car err) (cdr err)))))))

(defun hermes-exec--sentinel (proc _event)
  "Retire PROC's timers, buffered input and approval when its connection ends."
  (unless (process-live-p proc)
    (hermes-exec--cancel-connection-timer proc 'hermes-exec-request-timer)
    (hermes-exec--cancel-connection-timer proc 'hermes-exec-idle-timer)
    (process-put proc 'hermes-buffer nil)
    (hermes-exec--drop-pending proc)))

(defun hermes-exec--accept (server connection _message)
  "Admit CONNECTION for SERVER within its connection and time budgets.
Tag the exact listener owner before checking the cap, including this accept.
Excess clients receive 503 and close without joining the approval queue."
  (process-put connection 'hermes-exec-connection server)
  (if (> (length (hermes-exec--live-connections server)) hermes-exec-max-connections)
      (hermes-exec--send-response
       connection (hermes-exec--http-response
                   503 "Service Unavailable"
                   "{\"ok\":false,\"error\":\"too many connections\"}"))
    (hermes-exec--arm-connection-timer
     connection 'hermes-exec-request-timer hermes-exec-request-timeout)
    (hermes-exec--arm-connection-timer
     connection 'hermes-exec-idle-timer hermes-exec-idle-timeout)))

(defun hermes-exec--start-server (host)
  "Return a new eval endpoint server process bound to HOST."
  (make-network-process
   :name "hermes-exec"
   :server t
   :host host
   :service hermes-exec-port
   :family (if (string-search ":" host) 'ipv6 'ipv4)
   :log #'hermes-exec--accept
   ;; Frame raw bytes before decoding the declared UTF-8 body.  A split UTF-8
   ;; sequence must not defer input accounting or change Content-Length units.
   :coding '(binary . utf-8-unix)
   :noquery t
   :filter #'hermes-exec--filter
   :sentinel #'hermes-exec--sentinel))

;;;###autoload
(defun hermes-exec-start ()
  "Start the Hermes eval endpoint.
Return the existing listener when already running.  Otherwise refuse to start
while `hermes-exec-enabled' is nil or the host would bind a public interface,
and store the listening process for `hermes-exec-stop'."
  (interactive)
  (if (process-live-p hermes-exec--process)
      hermes-exec--process
    (unless hermes-exec-enabled
      (user-error "Set `hermes-exec-enabled' to enable the Hermes eval endpoint"))
    (unless (and (natnump hermes-exec-max-connections)
                 (numberp hermes-exec-request-timeout)
                 (> hermes-exec-request-timeout 0)
                 (numberp hermes-exec-idle-timeout)
                 (> hermes-exec-idle-timeout 0))
      (user-error "Eval connection cap must be nonnegative and timeouts positive"))
    (let ((host (hermes-exec--resolve-host)))
      (unless host
        (user-error
         "Set `hermes-exec-host' to your Tailscale IP; refusing to bind a public interface for a remote dashboard"))
      (unless (hermes-exec--allowed-bind-host-p host)
        (user-error "Refusing to bind eval endpoint to unsafe host %s" host))
      (when (and (not (hermes-exec--loopback-host-p host))
                 (not (hermes-exec--expected-token)))
        (user-error
         "Refusing to bind non-loopback host %s without a token; set `hermes-exec-token' or EMACS_EXEC_TOKEN"
         host))
      (when hermes-exec--process
        (hermes-exec-stop))
      (setq hermes-exec--process (hermes-exec--start-server host))
      (message "Hermes eval endpoint listening on %s:%d" host hermes-exec-port))))

(defun hermes-exec--live-connections (server)
  "Return live connection processes accepted by SERVER.
Connections are tagged at accept time by `hermes-exec--accept', so match that
process property rather than the inherited filter, excluding SERVER itself."
  (and server
       (cl-remove-if-not
        (lambda (conn)
          (and (process-live-p conn)
               (eq server (process-get conn 'hermes-exec-connection))))
        (process-list))))

(defun hermes-exec-stop ()
  "Stop the Hermes eval endpoint and release any open connections."
  (interactive)
  (let ((server hermes-exec--process))
    ;; Retire the queue before delete-process can run connection sentinels.
    (setq hermes-exec--process nil
          hermes-exec--pending nil)
    (hermes-exec--finish-active)
    (dolist (conn (hermes-exec--live-connections server))
      (ignore-errors (delete-process conn)))
    (when (process-live-p server)
      (ignore-errors (delete-process server))))
  (message "Hermes eval endpoint stopped"))

(defun hermes-exec--bound-host ()
  "Return the host the endpoint is actually bound to.
While the listener is live, read its bound host from `process-contact' rather
than re-resolving, which could disagree if the config changed after start.  Fall
back to `hermes-exec--resolve-host' once the process is gone."
  (or (and (process-live-p hermes-exec--process)
           (process-contact hermes-exec--process :host))
      (hermes-exec--resolve-host)
      "?"))

(defun hermes-exec-status ()
  "Report whether the eval endpoint is running, and on which host and port."
  (interactive)
  (if (process-live-p hermes-exec--process)
      (message "Hermes eval endpoint running on %s:%d"
               (hermes-exec--bound-host) hermes-exec-port)
    (message "Hermes eval endpoint not running")))

;;; Bridge registration helper

(defun hermes-exec--detect-host ()
  "Return the bridge host: resolved host, a Tailscale IP, or a placeholder."
  (or (hermes-exec--resolve-host)
      (car (hermes-exec--tailscale-ipv4-addresses))
      "<your-host>"))

(defun hermes-exec-show-bridge-command ()
  "Copy the ready-to-paste `hermes mcp add' line registering this endpoint.
Includes EMACS_EXEC_TOKEN when a token is configured, since the bridge needs the
same secret the endpoint enforces.  Interactively the full command lands only on
the kill ring; the echoed line redacts the token so the secret does not persist
in `*Messages*'."
  (interactive)
  (let* ((token (hermes-exec--expected-token))
         (line "hermes mcp add emacs --command hermes-emacs-mcp --env EMACS_EXEC_HOST=%s EMACS_EXEC_PORT=%d%s")
         (host (hermes-exec--detect-host))
         (command (format line host hermes-exec-port
                          (if token (format " EMACS_EXEC_TOKEN=%s" token) ""))))
    (if (called-interactively-p 'interactive)
        (progn
          (kill-new command)
          (message "Copied: %s"
                   (format line host hermes-exec-port
                           (if token " EMACS_EXEC_TOKEN=<redacted>" ""))))
      command)))

(provide 'hermes-exec)
;;; hermes-exec.el ends here
