;;; codex-ide-mcp-server.el --- HTTP MCP server for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; HTTP boundary and lifecycle state for the local MCP bridge.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'codex-ide-debug)
(require 'codex-ide-mcp-core)
(require 'codex-ide-mcp-protocol)

;;; Server state

(defvar codex-ide-mcp--server nil
  "The listening MCP server process, or nil when stopped.")

(defvar codex-ide-mcp--port nil
  "The effective MCP server port once the server is listening.")

(defvar codex-ide-mcp--clients (make-hash-table :test 'eq)
  "Connection state keyed by client process.")

;;; HTTP server boundary

(defun codex-ide-mcp--url ()
  "Return the URL for the running MCP endpoint."
  (let ((host (if (string-match-p ":" codex-ide-mcp-host)
                  (format "[%s]" codex-ide-mcp-host)
                codex-ide-mcp-host)))
    (format "http://%s:%d/mcp" host codex-ide-mcp--port)))

(defun codex-ide-mcp--json-read (bytes)
  "Decode JSON BYTES into a plist."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string
     (decode-coding-string bytes 'utf-8))))

(defun codex-ide-mcp--status-text (status)
  "Return HTTP reason phrase for STATUS."
  (pcase status
    (200 "OK")
    (204 "No Content")
    (400 "Bad Request")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (415 "Unsupported Media Type")
    (_ "Internal Server Error")))

(defun codex-ide-mcp--send-json (proc status body)
  "Send HTTP STATUS and JSON BODY to PROC, then close the connection."
  (let* ((payload (if body
                      (encode-coding-string (json-encode body) 'utf-8)
                    ""))
         (headers (concat
                   (format "HTTP/1.1 %d %s\r\n"
                           status (codex-ide-mcp--status-text status))
                   "Content-Type: application/json\r\n"
                   "Connection: close\r\n"
                   (format "Content-Length: %d\r\n\r\n"
                           (length payload)))))
    (when (process-live-p proc)
      (process-send-string proc (concat headers payload))
      (delete-process proc))))

(defun codex-ide-mcp--parse-headers (header-text)
  "Parse HTTP HEADER-TEXT into a plist."
  (let* ((lines (split-string header-text "\r?\n" t))
         (request-line (car lines))
         (parts (and request-line (split-string request-line " " t)))
         (headers nil))
    (dolist (line (cdr lines))
      (when (string-match (rx string-start
                              (group (+ (not ?:))) ":"
                              (* blank)
                              (group (* anything))
                              string-end)
                          line)
        (push (cons (downcase (match-string 1 line))
                    (match-string 2 line))
              headers)))
    (list :method (car parts)
          :path (cadr parts)
          :headers headers)))

(defun codex-ide-mcp--header (request name)
  "Return header NAME from parsed REQUEST, or nil."
  (cdr (assoc (downcase name) (plist-get request :headers))))

(defun codex-ide-mcp--host-name (host)
  "Return normalized host name from HOST, dropping an optional port."
  (when (stringp host)
    (let ((host (string-trim host)))
      (cond
       ((string-empty-p host) nil)
       ((string-match (rx string-start
                          "[" (group (+ (not (any "]")))) "]"
                          (? ":" (+ digit))
                          string-end)
                      host)
        (downcase (match-string 1 host)))
       ((string-match (rx string-start
                          (group (+ (not (any ":"))))
                          (? ":" (+ digit))
                          string-end)
                      host)
        (downcase (match-string 1 host)))
       (t (downcase host))))))

(defun codex-ide-mcp--local-host-p (host)
  "Return non-nil when HOST is absent or names a loopback host."
  (let ((name (codex-ide-mcp--host-name host)))
    (or (not name)
        (member name '("127.0.0.1" "localhost" "::1")))))

(defun codex-ide-mcp--local-origin-p (origin)
  "Return non-nil when ORIGIN is absent or names a loopback origin."
  (or (not origin)
      (string-empty-p (string-trim origin))
      (and (string-match (rx string-start
                             (or "http" "https") "://"
                             (group (+ (not (any "/"))))
                             string-end)
                         origin)
           (codex-ide-mcp--local-host-p (match-string 1 origin)))))

(defun codex-ide-mcp--json-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE is application/json."
  (and (stringp content-type)
       (string-match-p (rx string-start
                           (* blank) "application/json" (* blank)
                           (? ";" (* anything))
                           string-end)
                       (downcase content-type))))

(defun codex-ide-mcp--request-error (request)
  "Return (STATUS . MESSAGE) when REQUEST should be rejected, else nil."
  (cond
   ((not (equal (plist-get request :path) "/mcp"))
    '(404 . "Only /mcp is supported"))
   ((not (codex-ide-mcp--local-host-p
          (codex-ide-mcp--header request "host")))
    '(403 . "Host must be loopback"))
   ((not (codex-ide-mcp--local-origin-p
          (codex-ide-mcp--header request "origin")))
    '(403 . "Origin must be loopback"))
   ((not (codex-ide-mcp--json-content-type-p
          (codex-ide-mcp--header request "content-type")))
    '(415 . "Content-Type must be application/json"))))

(defun codex-ide-mcp--content-length (request)
  "Return non-negative Content-Length for REQUEST, or nil when invalid.
Missing Content-Length is treated as 0.  Non-numeric and negative values
are invalid."
  (if-let* ((value (cdr (assoc "content-length"
                               (plist-get request :headers)))))
      (let ((trimmed (string-trim value)))
        (if (string-match-p "\\`[0-9]+\\'" trimmed)
            (string-to-number trimmed)
          nil))
    0))

(defun codex-ide-mcp--split-request (pending)
  "Return parse result for PENDING HTTP bytes.
Complete requests return (REQUEST . REST).  Incomplete input returns nil.
Invalid Content-Length returns the symbol `invalid'.  Bodies larger than
`codex-ide-mcp-max-request-bytes' or pending buffers that already exceed
that limit return the symbol `too-large'."
  (let ((max-bytes (if (and (integerp codex-ide-mcp-max-request-bytes)
                            (> codex-ide-mcp-max-request-bytes 0))
                       codex-ide-mcp-max-request-bytes
                     (* 1024 1024))))
    (cond
     ((> (length pending) (+ max-bytes 8192))
      'too-large)
     ((not (string-match "\r\n\r\n" pending))
      nil)
     (t
      (let* ((header-end (match-beginning 0))
             (body-start (match-end 0))
             (request (codex-ide-mcp--parse-headers
                       (substring pending 0 header-end)))
             (length (codex-ide-mcp--content-length request)))
        (cond
         ((null length) 'invalid)
         ((> length max-bytes) 'too-large)
         (t
          (let ((total (+ body-start length)))
            (cond
             ((> total (length pending))
              (when (> (length pending) (+ max-bytes 8192))
                'too-large))
             (t
              (setf (plist-get request :body)
                    (substring pending body-start total))
              (cons request (substring pending total))))))))))))

(defun codex-ide-mcp--reject-client (proc status message)
  "Send STATUS/MESSAGE to PROC and drop the client connection."
  (ignore-errors
    (codex-ide-mcp--send-json
     proc status
     (codex-ide-mcp--make-error-response nil -32600 message)))
  (ignore-errors (delete-process proc))
  (remhash proc codex-ide-mcp--clients))

(defun codex-ide-mcp--selected-buffer ()
  "Return the current UI buffer used for MCP tool execution."
  (if-let* ((window (selected-window)))
      (window-buffer window)
    (current-buffer)))

(defun codex-ide-mcp--handle-http-request (proc request)
  "Handle parsed HTTP REQUEST from PROC."
  (if (not (equal (plist-get request :method) "POST"))
      (codex-ide-mcp--send-json
       proc 405 (codex-ide-mcp--make-error-response
                 nil -32600 "Only POST is supported"))
    (if-let* ((request-error (codex-ide-mcp--request-error request)))
        (codex-ide-mcp--send-json
         proc (car request-error)
         (codex-ide-mcp--make-error-response
          nil -32600 (cdr request-error)))
      (condition-case err
          (let* ((message (codex-ide-mcp--json-read (plist-get request :body)))
                 (response (with-current-buffer (codex-ide-mcp--selected-buffer)
                             (codex-ide-mcp--handle-message message))))
            (if response
                (codex-ide-mcp--send-json proc 200 response)
              (codex-ide-mcp--send-json proc 204 nil)))
        (error
         (codex-ide-mcp--send-json
          proc 400 (codex-ide-mcp--make-error-response
                    nil -32700 (error-message-string err))))))))

(defun codex-ide-mcp--client-state (proc)
  "Return accumulated state for client PROC, creating it if needed."
  (or (gethash proc codex-ide-mcp--clients)
      (let ((state (list :pending "")))
        (puthash proc state codex-ide-mcp--clients)
        state)))

(defun codex-ide-mcp--filter (proc string)
  "Process filter for MCP HTTP connection PROC receiving STRING."
  (set-process-coding-system proc 'binary 'binary)
  (let* ((state (codex-ide-mcp--client-state proc))
         (pending (concat (plist-get state :pending) string))
         (done nil))
    (while (and (not done) (process-live-p proc))
      (pcase (codex-ide-mcp--split-request pending)
        ('invalid
         (codex-ide-mcp--reject-client
          proc 400 "Invalid Content-Length")
         (setq done t
               pending ""))
        ('too-large
         (codex-ide-mcp--reject-client
          proc 400 "Request exceeds codex-ide-mcp-max-request-bytes")
         (setq done t
               pending ""))
        (`(,request . ,rest)
         (setq pending rest)
         (codex-ide-mcp--handle-http-request proc request))
        (_
         (setq done t))))
    (when (process-live-p proc)
      (plist-put state :pending pending))))

(defun codex-ide-mcp--sentinel (proc event)
  "Clean client state for PROC on EVENT."
  (codex-ide-debug "Codex MCP client event: %s" (string-trim event))
  (unless (process-live-p proc)
    (remhash proc codex-ide-mcp--clients)))

(defun codex-ide-mcp--contact-port (process)
  "Return network port for PROCESS."
  (let* ((full (ignore-errors (process-contact process t)))
         (short (ignore-errors (process-contact process)))
         (service (or (and (listp full) (plist-get full :service))
                      (and (listp short) (plist-get short :service))
                      (and (listp short) (cadr short))
                      (and (listp full)
                           (cl-find-if #'integerp full)))))
    (unless (integerp service)
      (error "Could not determine MCP server port from process contact"))
    service))

(defun codex-ide-mcp--running-p ()
  "Return non-nil when the MCP server is listening."
  (and codex-ide-mcp--server
       (process-live-p codex-ide-mcp--server)))

(defun codex-ide-mcp--start-server ()
  "Start the local MCP HTTP server and return its process."
  (when (codex-ide-mcp--running-p)
    (user-error "Codex MCP tools server is already running"))
  (unless (and (codex-ide-mcp--host-name codex-ide-mcp-host)
               (codex-ide-mcp--local-host-p codex-ide-mcp-host))
    (user-error "Codex MCP host must be loopback"))
  (let ((server (make-network-process
                 :name "codex-ide-mcp"
                 :buffer nil
                 :host codex-ide-mcp-host
                 :service codex-ide-mcp-port
                 :server t
                 :noquery t
                 :filter #'codex-ide-mcp--filter
                 :sentinel #'codex-ide-mcp--sentinel)))
    (set-process-coding-system server 'binary 'binary)
    (setq codex-ide-mcp--server server
          codex-ide-mcp--port (codex-ide-mcp--contact-port server))
    (codex-ide-debug "Codex MCP listening on %s" (codex-ide-mcp--url))
    server))

(defun codex-ide-mcp--stop-server ()
  "Stop the local MCP HTTP server and clear harness job state."
  (when codex-ide-mcp--server
    (ignore-errors (delete-process codex-ide-mcp--server))
    (setq codex-ide-mcp--server nil
          codex-ide-mcp--port nil))
  (maphash (lambda (proc _state)
             (ignore-errors (delete-process proc)))
           codex-ide-mcp--clients)
  (clrhash codex-ide-mcp--clients)
  (when (fboundp 'codex-ide-harness-reset)
    (codex-ide-harness-reset))
  (codex-ide-debug "Codex MCP tools server stopped"))

;;; Public server entry

(defun codex-ide-mcp-ensure-server ()
  "Ensure the local MCP HTTP server is running and return its URL."
  (unless (codex-ide-mcp--running-p)
    (codex-ide-mcp--start-server))
  (codex-ide-mcp--url))

(provide 'codex-ide-mcp-server)

;;; codex-ide-mcp-server.el ends here
