;;; hermes-exec.el --- HTTP-JSON eval endpoint for the Hermes bridge  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1"))

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
;;   {"code":"<elisp source>"}.  The reply is always HTTP 200 with a JSON
;;   body of either {"ok":true,"result":"..."} or {"ok":false,"error":"..."},
;;   so evaluation errors are reported in-band rather than as HTTP failures.
;;
;; The endpoint reuses `hermes-dashboard-transport.el' for URL parsing, the
;; loopback-host predicate, and secret redaction so it never binds a public
;; interface by accident and never echoes credentials back to the bridge.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'hermes-dashboard-transport)

(defgroup hermes-exec nil
  "HTTP-JSON Emacs Lisp eval endpoint for the Hermes bridge."
  :group 'hermes)

(defcustom hermes-exec-enabled nil
  "Master switch for the Hermes eval endpoint.
Nothing listens unless this is non-nil and `hermes-exec-start' has run, so the
server can never come up implicitly."
  :type 'boolean)

(defcustom hermes-exec-require-approval t
  "When non-nil, prompt before evaluating each request.
The prompt shows the incoming code and blocks Emacs on `y-or-n-p' until the user
answers, which is intentional: a human gate is the only protection while the
dangerous-form blocklist is deferred.  Set to nil only for full-execution
testing where every request should run unprompted."
  :type 'boolean)

(defcustom hermes-exec-host nil
  "Interface the eval endpoint binds to, or nil to auto-resolve.
When nil and `hermes-dashboard-transport-url' names a loopback dashboard, the
endpoint binds \"127.0.0.1\".  When nil and the dashboard is remote,
`hermes-exec-start' errors and asks the user to set this to their Tailscale IP.
The endpoint never binds \"0.0.0.0\" or all interfaces."
  :type '(choice (const :tag "Auto-resolve from dashboard" nil)
                 (string :tag "Bind address")))

(defcustom hermes-exec-port 8237
  "TCP port the Hermes eval endpoint listens on."
  :type 'integer)

(defcustom hermes-exec-max-output 100000
  "Maximum number of characters returned for an eval result.
Longer printed results are truncated to this length before being sent back."
  :type 'integer)

(defcustom hermes-exec-timeout 30
  "Seconds an evaluation may run before `with-timeout' aborts it."
  :type 'number)

(defvar hermes-exec--process nil
  "The live eval endpoint server process, or nil when stopped.")

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

;;; HTTP request parsing (pure)

(defun hermes-exec--parse-request-line (line)
  "Return a plist of :method and :path parsed from request LINE."
  (let ((parts (split-string (string-trim line) " " t)))
    (list :method (nth 0 parts) :path (nth 1 parts))))

(defun hermes-exec--parse-headers (header-block)
  "Return an alist of lowercased header names to values from HEADER-BLOCK."
  (let (headers)
    (dolist (line (split-string header-block "\r?\n" t))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (push (cons (downcase (string-trim (match-string 1 line)))
                    (string-trim (match-string 2 line)))
              headers)))
    (nreverse headers)))

(defun hermes-exec--content-length (headers)
  "Return the Content-Length value from HEADERS as a number, or nil."
  (and-let* ((value (cdr (assoc "content-length" headers))))
    (string-to-number value)))

(defun hermes-exec--parse-request (raw)
  "Return a plist parsed from the RAW HTTP request, or nil when incomplete.
The plist has :method, :path, :headers, and :body.  Nil means more bytes are
needed: the header terminator or the full Content-Length body is still missing."
  (and-let* ((split (string-search "\r\n\r\n" raw)))
    (let* ((head (substring raw 0 split))
           (body (substring raw (+ split 4)))
           (lines (split-string head "\r?\n"))
           (request-line (hermes-exec--parse-request-line (car lines)))
           (headers (hermes-exec--parse-headers
                     (string-join (cdr lines) "\n")))
           (length (hermes-exec--content-length headers)))
      (when (or (null length) (>= (string-bytes body) length))
        (append request-line (list :headers headers :body body))))))

;;; Authentication seam
;;
;; Bearer-token auth is deliberately not implemented yet.  When it lands it
;; belongs here, checked against the parsed request before evaluation.  This
;; predicate is the single seam the IO path consults so the rest of the file
;; stays unchanged when auth is added.

(defun hermes-exec--request-authorized-p (_request)
  "Return non-nil when REQUEST is authorized to run.
Always true for now; this is the seam for future bearer-token validation."
  t)

;;; Eval path (pure-ish)
;;
;; Tightening security with a dangerous-function blocklist or a static form
;; walker is a deferred phase.  See /tmp/emacs-mcp-server/mcp-server-security.el
;; for the intended reference when that work begins.

(defun hermes-exec--format-result (value)
  "Return VALUE printed, truncated, and redacted for transport."
  (let ((printed (format "%S" value)))
    (hermes-dashboard-transport--redact-secret
     (if (> (length printed) hermes-exec-max-output)
         (substring printed 0 hermes-exec-max-output)
       printed))))

(defun hermes-exec--eval-code (code)
  "Read and evaluate CODE under a timeout, returning the value."
  (with-timeout (hermes-exec-timeout
                 (error "Hermes eval timed out after %s seconds"
                        hermes-exec-timeout))
    (eval (car (read-from-string code)) t)))

(defun hermes-exec--evaluate (code)
  "Evaluate CODE and return a result plist.
The plist is (:ok t :result STRING) on success or (:ok nil :error STRING) when
reading or evaluation signals.  Errors are captured, never thrown."
  (condition-case err
      (list :ok t :result (hermes-exec--format-result
                           (hermes-exec--eval-code code)))
    (error (list :ok nil :error (error-message-string err)))))

(defun hermes-exec--approval-prompt (code)
  "Return a single-line prompt asking whether to evaluate CODE."
  (let ((shown (if (> (length code) 200) (substring code 0 200) code)))
    (format "Hermes eval request:\n%s\nEvaluate? " shown)))

(defun hermes-exec--maybe-evaluate (code)
  "Evaluate CODE, gating on `hermes-exec-require-approval' first.
When approval is required and the user declines, return a declined result plist
without evaluating anything."
  (if (and hermes-exec-require-approval
           (not (y-or-n-p (hermes-exec--approval-prompt code))))
      (list :ok nil :error "Evaluation declined by user")
    (hermes-exec--evaluate code)))

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
  "Return the JSON response body for an /eval request BODY."
  (condition-case err
      (hermes-exec--result-json
       (hermes-exec--maybe-evaluate (hermes-exec--code-from-body body)))
    (error (json-serialize `((ok . :false)
                             (error . ,(error-message-string err)))))))

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
    (hermes-exec--http-response
     200 "OK" (hermes-exec--eval-response-body (plist-get request :body))))
   (t (hermes-exec--http-response
       404 "Not Found"
       (json-serialize '((ok . :false) (error . "not found")))))))

;;; Server IO

(defun hermes-exec--send-response (proc response)
  "Send RESPONSE on PROC and close the connection."
  (when (process-live-p proc)
    (ignore-errors (process-send-string proc response))
    (ignore-errors (delete-process proc))))

;; Input may arrive in chunks, so each connection accumulates bytes in its
;; `hermes-buffer' process property.  After every chunk the buffer is reparsed;
;; `hermes-exec--parse-request' returns nil until both the header terminator and
;; the full Content-Length body are present, and only then is the request
;; dispatched.  This keeps partial reads from triggering a premature eval.
(defun hermes-exec--filter (proc chunk)
  "Accumulate CHUNK on PROC and dispatch once a full request has arrived."
  (let ((buffer (concat (process-get proc 'hermes-buffer) chunk)))
    (process-put proc 'hermes-buffer buffer)
    (when-let* ((request (hermes-exec--parse-request buffer)))
      (hermes-exec--send-response proc (hermes-exec--dispatch request)))))

(defun hermes-exec--sentinel (proc _event)
  "Drop PROC's accumulated input buffer when the connection ends."
  (unless (process-live-p proc)
    (process-put proc 'hermes-buffer nil)))

(defun hermes-exec--start-server (host)
  "Return a new eval endpoint server process bound to HOST."
  (make-network-process
   :name "hermes-exec"
   :server t
   :host host
   :service hermes-exec-port
   :family 'ipv4
   ;; utf-8-unix, not plain utf-8: a bare coding system auto-detects EOL and
   ;; rewrites CRLF to LF on read, which would strip the "\r\n\r\n" header
   ;; terminator the parser looks for.  -unix decodes UTF-8 without touching
   ;; line endings, so the HTTP framing survives intact.
   :coding 'utf-8-unix
   :noquery t
   :filter #'hermes-exec--filter
   :sentinel #'hermes-exec--sentinel))

;;;###autoload
(defun hermes-exec-start ()
  "Start the Hermes eval endpoint.
Set `hermes-exec-enabled' when nil, refuse to bind a public interface, and store
the listening process for `hermes-exec-stop'."
  (interactive)
  (when (process-live-p hermes-exec--process)
    (user-error "Hermes eval endpoint already running"))
  (setq hermes-exec-enabled t)
  (let ((host (hermes-exec--resolve-host)))
    (unless host
      (user-error
       "Set `hermes-exec-host' to your Tailscale IP; refusing to bind a public interface for a remote dashboard"))
    (setq hermes-exec--process (hermes-exec--start-server host))
    (message "Hermes eval endpoint listening on %s:%d" host hermes-exec-port)))

(defun hermes-exec--live-connections (server)
  "Return live connection processes accepted by SERVER.
Emacs sets no back-pointer to the listener on accepted connections, so match
them by the filter they inherit from SERVER instead, excluding SERVER itself."
  (and (process-live-p server)
       (cl-remove-if-not
        (lambda (conn)
          (and (not (eq conn server))
               (eq (process-filter conn) #'hermes-exec--filter)))
        (process-list))))

(defun hermes-exec-stop ()
  "Stop the Hermes eval endpoint and release any open connections."
  (interactive)
  (dolist (conn (hermes-exec--live-connections hermes-exec--process))
    (ignore-errors (delete-process conn)))
  (when (process-live-p hermes-exec--process)
    (ignore-errors (delete-process hermes-exec--process)))
  (setq hermes-exec--process nil)
  (message "Hermes eval endpoint stopped"))

(defun hermes-exec-status ()
  "Report whether the eval endpoint is running, and on which host and port."
  (interactive)
  (if (process-live-p hermes-exec--process)
      (message "Hermes eval endpoint running on %s:%d"
               (or (hermes-exec--resolve-host) "?") hermes-exec-port)
    (message "Hermes eval endpoint not running")))

;;; Bridge registration helper

(defun hermes-exec--detect-host ()
  "Return the bridge host: resolved host, a Tailscale IP, or a placeholder."
  (or (hermes-exec--resolve-host)
      (ignore-errors (car (process-lines "tailscale" "ip" "-4")))
      "<your-host>"))

(defun hermes-exec-show-bridge-command ()
  "Show the ready-to-paste `hermes mcp add' line registering this endpoint."
  (interactive)
  (let ((command (format
                  "hermes mcp add emacs --command <venv>/bin/python --args server.py --env EMACS_EXEC_HOST=%s EMACS_EXEC_PORT=%d"
                  (hermes-exec--detect-host) hermes-exec-port)))
    (if (called-interactively-p 'interactive)
        (progn (kill-new command) (message "%s" command))
      command)))

(provide 'hermes-exec)
;;; hermes-exec.el ends here
