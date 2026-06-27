;;; codex-ide-appserver.el --- Native Codex app-server backend (spike)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, codex, tools, app-server
;; URL: https://git.thanosapollo.org/emacs-codex

;; This file is not part of GNU Emacs.

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

;; Native Codex app-server backend.  Communicates with bare `codex app-server'
;; over stdio using JSON-RPC-style newline-delimited JSON (currently without
;; the "jsonrpc":"2.0" header).  This is a Phase 4 spike: it proves the
;; initialize -> thread/start -> turn/start -> streaming -> turn/completed
;; path with agent-message rendering, plus auto-deny of approval requests.
;;
;; The module is intentionally isolated from the terminal MVP
;; (`codex-ide.el').  It has its own process, buffer, and state.
;;
;; Usage:
;;   M-x codex-ide-appserver-start      Start the app-server and initialize
;;   M-x codex-ide-appserver-send-message  Send a prompt and stream the reply
;;   M-x codex-ide-appserver-stop       Stop the app-server

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

;;; Customization

(defgroup codex-ide-appserver nil
  "Native Codex app-server backend (spike)."
  :group 'tools
  :prefix "codex-ide-appserver-")

(defcustom codex-ide-appserver-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide-appserver)

(defcustom codex-ide-appserver-buffer-name "*codex-ide-appserver*"
  "Name of the buffer where agent messages are rendered."
  :type 'string
  :group 'codex-ide-appserver)

(defcustom codex-ide-appserver-client-name "emacs-codex"
  "Client name reported to the app-server during initialization."
  :type 'string
  :group 'codex-ide-appserver)

(defcustom codex-ide-appserver-client-version "0.1.0"
  "Client version reported to the app-server during initialization."
  :type 'string
  :group 'codex-ide-appserver)

;;; Variables

(defvar codex-ide-appserver--process nil
  "The app-server stdio process, or nil when not running.")

(defvar codex-ide-appserver--next-id 0
  "Next JSON-RPC request id to use.")

(defvar codex-ide-appserver--pending (make-hash-table :test 'eql)
  "Hash table mapping request ids to their callback functions.")

(defvar codex-ide-appserver--thread-id nil
  "Current thread id after `thread/start'.")

(defvar codex-ide-appserver--output-buffer nil
  "Live output buffer, captured for async insertion.")

(defvar codex-ide-appserver--send-function nil
  "Override for `codex-ide-appserver--send' in tests.
When non-nil, this function is called instead of `process-send-string'.")

;;; JSON conventions
;;
;; All JSON encoding and decoding in this module uses string-keyed alists
;; and lists.  We bind `json-object-type', `json-array-type', and
;; `json-key-type' explicitly so the behavior is deterministic regardless
;; of the user's customizations.

(defmacro codex-ide-appserver--with-json-conventions (&rest body)
  "Execute BODY with standard JSON encoding/decoding conventions."
  (declare (indent 0))
  `(let ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-false :false)
         (json-null nil))
     ,@body))

;;; Pure helpers (no side effects, testable without a process)

(defun codex-ide-appserver--build-request (id method params)
  "Return a JSON-RPC request string for METHOD with PARAMS and numeric ID.
The string ends with a newline, ready for stdio.  PARAMS may be nil."
  (codex-ide-appserver--with-json-conventions
    (let ((obj `(("id" . ,id) ("method" . ,method))))
      (when params
        (nconc obj (list (cons "params" params))))
      (concat (json-encode obj) "\n"))))

(defun codex-ide-appserver--build-notification (method params)
  "Return a JSON-RPC notification string for METHOD with PARAMS.
Unlike a request, a notification carries no id.  The string ends with a
newline."
  (codex-ide-appserver--with-json-conventions
    (let ((obj `(("method" . ,method))))
      (when params
        (nconc obj (list (cons "params" params))))
      (concat (json-encode obj) "\n"))))

(defun codex-ide-appserver--parse-message (string)
  "Parse a single JSON-RPC message from STRING.
Return a plist (:type RESPONSE|NOTIFICATION|REQUEST :method M :id I
:result R :error E :params P), or nil if STRING is not valid JSON."
  (condition-case nil
      (codex-ide-appserver--with-json-conventions
        (let ((msg (json-read-from-string string)))
          (cond
           ((null msg) nil)
           ;; Response: has "id" and either "result" or "error".
           ((and (assoc "id" msg)
                 (or (assoc "result" msg) (assoc "error" msg)))
            `(:type response
              :id ,(cdr (assoc "id" msg))
              :result ,(cdr (assoc "result" msg))
              :error ,(cdr (assoc "error" msg))))
           ;; Request or notification: has "method".
           ((assoc "method" msg)
            (let ((id (cdr (assoc "id" msg))))
              `(:type ,(if id 'request 'notification)
                :method ,(cdr (assoc "method" msg))
                :id ,id
                :params ,(cdr (assoc "params" msg)))))
           (t nil))))
    (error nil)))

(defun codex-ide-appserver--make-input (text)
  "Return the params \"input\" array for a text user message TEXT."
  `((("type" . "text") ("text" . ,text))))

(defun codex-ide-appserver--approval-response (decision)
  "Return the result alist for an approval response with DECISION.
DECISION is a string like \"denied\"."
  `(("decision" . ,decision)))

(defun codex-ide-appserver--next-id ()
  "Return the next incrementing request id and advance the counter."
  (cl-incf codex-ide-appserver--next-id))

;;; Process communication

(defun codex-ide-appserver--send (string)
  "Send STRING to the app-server process.
When `codex-ide-appserver--send-function' is non-nil (test override),
call that instead."
  (if codex-ide-appserver--send-function
      (funcall codex-ide-appserver--send-function string)
    (when (process-live-p codex-ide-appserver--process)
      (process-send-string codex-ide-appserver--process string))))

(defun codex-ide-appserver--send-request (method params &optional callback)
  "Send a JSON-RPC request with METHOD and PARAMS.
Register CALLBACK (if non-nil) to receive the response.  Return the id
used."
  (let ((id (codex-ide-appserver--next-id)))
    (when callback
      (puthash id callback codex-ide-appserver--pending))
    (codex-ide-appserver--send
     (codex-ide-appserver--build-request id method params))
    id))

(defun codex-ide-appserver--send-notification (method params)
  "Send a JSON-RPC notification with METHOD and PARAMS."
  (codex-ide-appserver--send
   (codex-ide-appserver--build-notification method params)))

(defun codex-ide-appserver--send-response (id result)
  "Send a JSON-RPC response with numeric ID and RESULT."
  (codex-ide-appserver--with-json-conventions
    (let ((obj `(("id" . ,id) ("result" . ,result))))
      (codex-ide-appserver--send (concat (json-encode obj) "\n")))))

;;; Message dispatch

(defun codex-ide-appserver--dispatch (msg)
  "Dispatch a parsed MSG plist to the appropriate handler."
  (pcase (plist-get msg :type)
    ('response
     (let ((callback (gethash (plist-get msg :id)
                              codex-ide-appserver--pending)))
       (remhash (plist-get msg :id) codex-ide-appserver--pending)
       (when callback
         (funcall callback (plist-get msg :result)
                  (plist-get msg :error)))))
    ('notification
     (codex-ide-appserver--handle-notification
      (plist-get msg :method)
      (plist-get msg :params)))
    ('request
     (codex-ide-appserver--handle-request
      (plist-get msg :method)
      (plist-get msg :id)))))

(defun codex-ide-appserver--handle-notification (method params)
  "Handle a server notification with METHOD and PARAMS."
  (pcase method
    ("thread/started"
     (setq codex-ide-appserver--thread-id
           (cdr (assoc "id" (or (cdr (assoc "thread" params))
                                params)))))
    ("turn/started"
     (codex-ide-appserver--render-separator))
    ("item/agentMessage/delta"
     (codex-ide-appserver--render-delta
      (cdr (assoc "delta" params))))
    ("item/completed"
     (codex-ide-appserver--render-item-completed
      (cdr (assoc "item" params))))
    ("turn/completed"
     (codex-ide-appserver--render-separator))
    ("error"
     (codex-ide-appserver--render-error
      (cdr (assoc "error" params))))))

(defun codex-ide-appserver--handle-request (method id)
  "Handle a server-initiated request with METHOD and numeric ID.
The spike auto-denies approval requests."
  (pcase method
    ("item/commandExecution/requestApproval"
     (codex-ide-appserver--send-response
      id (codex-ide-appserver--approval-response "denied")))
    ("item/fileChange/requestApproval"
     (codex-ide-appserver--send-response
      id (codex-ide-appserver--approval-response "denied")))
    (_
     ;; Unknown server request: respond with method-not-found error.
     (codex-ide-appserver--send-response
      id `(("error" . (("code" . -32601)
                       ("message" . "Method not found"))))))))

;;; Process filter (line accumulation + dispatch)

(defun codex-ide-appserver--filter (proc string)
  "Process filter: accumulate STRING into the process buffer and dispatch lines.
Lines are newline-delimited JSON messages."
  (let ((proc-buffer (process-buffer proc)))
    (when (buffer-live-p proc-buffer)
      (with-current-buffer proc-buffer
        (goto-char (point-max))
        (insert string)
        (codex-ide-appserver--process-pending-lines)))))

(defun codex-ide-appserver--process-pending-lines ()
  "Extract and dispatch complete newline-delimited lines from point-min.
This is called from within the process buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (let* ((line (buffer-substring-no-properties
                    (point-min) (match-beginning 0)))
             (parsed (codex-ide-appserver--parse-message line)))
        (delete-region (point-min) (match-end 0))
        (when parsed
          (codex-ide-appserver--dispatch parsed))))))

;;; Buffer rendering

(defun codex-ide-appserver--get-output-buffer ()
  "Return the output buffer, creating it if needed."
  (or (and (buffer-live-p codex-ide-appserver--output-buffer)
           codex-ide-appserver--output-buffer)
      (let ((buf (get-buffer-create codex-ide-appserver-buffer-name)))
        (setq codex-ide-appserver--output-buffer buf)
        buf)))

(defun codex-ide-appserver--render-delta (delta)
  "Insert DELTA text at the end of the output buffer."
  (when (and delta codex-ide-appserver--output-buffer)
    (with-current-buffer codex-ide-appserver--output-buffer
      (goto-char (point-max))
      (insert delta))))

(defun codex-ide-appserver--render-separator ()
  "Insert a blank-line separator in the output buffer."
  (when codex-ide-appserver--output-buffer
    (with-current-buffer codex-ide-appserver--output-buffer
      (goto-char (point-max))
      (insert "\n\n"))))

(defun codex-ide-appserver--render-item-completed (item)
  "Render a completed ITEM in the output buffer.
For agentMessage items, a trailing newline is added."
  (when (and item codex-ide-appserver--output-buffer)
    (let ((type (cdr (assoc "type" item))))
      (when (equal type "agentMessage")
        (with-current-buffer codex-ide-appserver--output-buffer
          (goto-char (point-max))
          (insert "\n"))))))

(defun codex-ide-appserver--render-error (error)
  "Render an ERROR object in the output buffer."
  (when (and error codex-ide-appserver--output-buffer)
    (let ((msg (or (cdr (assoc "message" error))
                   (prin1-to-string error))))
      (with-current-buffer codex-ide-appserver--output-buffer
        (goto-char (point-max))
        (insert (format "[error] %s\n" msg))))))

;;; Lifecycle

(defun codex-ide-appserver--sentinel (_proc event)
  "Process sentinel for the app-server process and EVENT string."
  (codex-ide-appserver--debug "app-server sentinel: %s" (string-trim event))
  (cond
   ((string-match-p
     (rx (or "finished" "exited" "killed" "terminated"))
     event)
    (setq codex-ide-appserver--process nil)
    (setq codex-ide-appserver--thread-id nil)
    (clrhash codex-ide-appserver--pending)
    (codex-ide-appserver--debug "app-server stopped"))))

(defun codex-ide-appserver--debug (format-string &rest args)
  "Minimal debug logger for the spike.
Appends to a `*codex-ide-appserver-debug*' buffer when that buffer exists."
  (when (buffer-live-p (get-buffer "*codex-ide-appserver-debug*"))
    (with-current-buffer (get-buffer "*codex-ide-appserver-debug*")
      (goto-char (point-max))
      (insert (apply #'format format-string args) "\n"))))

(defun codex-ide-appserver--initialize-params ()
  "Return the params alist for the initialize request."
  `(("clientInfo" . ((("name" . ,codex-ide-appserver-client-name)
                      ("title" . "Emacs Codex")
                      ("version" . ,codex-ide-appserver-client-version))))
    ("capabilities" . ((("experimentalApi" . :false)
                        ("optOutNotificationMethods" . nil))))))

(defun codex-ide-appserver--on-initialized (_result _error)
  "Callback after initialize response.
Sends `initialized' notification and starts a thread."
  (codex-ide-appserver--send-notification "initialized" nil)
  (let ((cwd (expand-file-name default-directory)))
    (codex-ide-appserver--send-request
     "thread/start"
     `((("cwd" . ,cwd)))
     #'codex-ide-appserver--on-thread-started)))

(defun codex-ide-appserver--on-thread-started (result _error)
  "Callback after thread/start response.
Stores the thread id from RESULT and displays the output buffer."
  (let ((thread (cdr (assoc "thread" result))))
    (setq codex-ide-appserver--thread-id
          (cdr (assoc "id" (or thread result)))))
  (codex-ide-appserver--debug "thread started: %s"
                              codex-ide-appserver--thread-id)
  (display-buffer (codex-ide-appserver--get-output-buffer)))

(defun codex-ide-appserver--on-turn-started (_result _error)
  "Callback after turn/start response.
The streaming output arrives via notifications."
  (codex-ide-appserver--debug "turn started"))

;;;###autoload
(defun codex-ide-appserver-start ()
  "Start the app-server, initialize, and open a thread.
Returns the process on success, nil on failure."
  (interactive)
  (codex-ide-appserver-stop)
  (setq codex-ide-appserver--next-id 0)
  (clrhash codex-ide-appserver--pending)
  (get-buffer-create "*codex-ide-appserver-debug*")
  (codex-ide-appserver--get-output-buffer)
  (let* ((buf (get-buffer-create " *codex-ide-appserver-proc*"))
         (proc (make-process
                :name "codex-appserver"
                :buffer buf
                :command (list codex-ide-appserver-cli-path
                               "app-server")
                :connection-type 'pipe
                :filter #'codex-ide-appserver--filter
                :sentinel #'codex-ide-appserver--sentinel
                :noquery t)))
    (setq codex-ide-appserver--process proc)
    (codex-ide-appserver--debug "starting app-server: %s app-server"
                                codex-ide-appserver-cli-path)
    (codex-ide-appserver--send-request
     "initialize"
     (codex-ide-appserver--initialize-params)
     #'codex-ide-appserver--on-initialized)
    proc))

;;;###autoload
(defun codex-ide-appserver-send-message (prompt)
  "Send PROMPT to the current app-server thread as a new turn.
Requires an active session (`codex-ide-appserver-start')."
  (interactive
   (list (read-string "Codex prompt: ")))
  (unless (and codex-ide-appserver--process
               codex-ide-appserver--thread-id)
    (user-error "No active app-server thread.  Run M-x codex-ide-appserver-start"))
  (with-current-buffer (codex-ide-appserver--get-output-buffer)
    (goto-char (point-max))
    (insert (format ">>> %s\n\n" prompt)))
  (codex-ide-appserver--send-request
   "turn/start"
   `(("threadId" . ,codex-ide-appserver--thread-id)
     ("input" . ,(codex-ide-appserver--make-input prompt)))
   #'codex-ide-appserver--on-turn-started))

;;;###autoload
(defun codex-ide-appserver-stop ()
  "Stop the app-server process and clean up state."
  (interactive)
  (when (process-live-p codex-ide-appserver--process)
    (delete-process codex-ide-appserver--process))
  (setq codex-ide-appserver--process nil)
  (setq codex-ide-appserver--thread-id nil)
  (clrhash codex-ide-appserver--pending))

(provide 'codex-ide-appserver)

;;; codex-ide-appserver.el ends here
