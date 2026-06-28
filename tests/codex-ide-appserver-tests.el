;;; codex-ide-appserver-tests.el --- Tests for codex-ide-appserver.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide-appserver)

(defun codex-ide-appserver-tests--last-captured-json (captured)
  "Return the final JSON object encoded in CAPTURED output."
  (should captured)
  (codex-ide-appserver--with-json-conventions
    (json-read-from-string
     (car (last (split-string captured "\n" t))))))

(defun codex-ide-appserver-tests--dispatch-captured (msg)
  "Dispatch MSG and return the final JSON object sent in response."
  (let ((captured nil)
        (codex-ide-appserver--send-function nil))
    (setq codex-ide-appserver--send-function
          (lambda (str) (setq captured (concat captured str))))
    (codex-ide-appserver--dispatch msg)
    (codex-ide-appserver-tests--last-captured-json captured)))

(defun codex-ide-appserver-tests--response-decision (obj)
  "Return the approval decision from response OBJ."
  (cdr (assoc "decision" (cdr (assoc "result" obj)))))

(defun codex-ide-appserver-tests--file-change-approval-params (&optional overrides)
  "Return schema-real file-change approval params with OVERRIDES."
  (append overrides
          '(("itemId" . "item-file-1")
            ("startedAtMs" . 123)
            ("threadId" . "thread-1")
            ("turnId" . "turn-1")
            ("grantRoot" . "/tmp/codex-project")
            ("reason" . "write access requested"))))

;;; Pure helper tests

(ert-deftest codex-ide-appserver-build-request ()
  "Building a request produces valid JSON with id, method, and params."
  (let ((json (codex-ide-appserver--build-request
               1 "thread/start"
               `(("cwd" . "/tmp")))))
    (should (string-suffix-p "\n" json))
    (codex-ide-appserver--with-json-conventions
      (let ((obj (json-read-from-string json)))
        (should (equal 1 (cdr (assoc "id" obj))))
        (should (equal "thread/start" (cdr (assoc "method" obj))))
        (should (equal "/tmp"
                       (cdr (assoc "cwd"
                                   (cdr (assoc "params" obj))))))))))

(ert-deftest codex-ide-appserver-build-request-no-params ()
  "Building a request with nil params omits the params key."
  (let ((json (codex-ide-appserver--build-request 5 "initialize" nil)))
    (codex-ide-appserver--with-json-conventions
      (let ((obj (json-read-from-string json)))
        (should (equal 5 (cdr (assoc "id" obj))))
        (should (equal "initialize" (cdr (assoc "method" obj))))
        (should-not (assoc "params" obj))))))

(ert-deftest codex-ide-appserver-build-notification ()
  "Building a notification omits id."
  (let ((json (codex-ide-appserver--build-notification "initialized" nil)))
    (codex-ide-appserver--with-json-conventions
      (let ((obj (json-read-from-string json)))
        (should (equal "initialized" (cdr (assoc "method" obj))))
        (should-not (assoc "id" obj))))))

(ert-deftest codex-ide-appserver-parse-message-response ()
  "Parsing a response with id and result returns :type response."
  (let ((parsed (codex-ide-appserver--parse-message
                 "{\"id\":1,\"result\":{\"thread\":{\"id\":\"t-abc\"}}}")))
    (should (eq 'response (plist-get parsed :type)))
    (should (equal 1 (plist-get parsed :id)))
    (should (equal "t-abc"
                   (cdr (assoc "id"
                               (cdr (assoc "thread"
                                           (plist-get parsed :result)))))))))

(ert-deftest codex-ide-appserver-parse-message-error-response ()
  "Parsing an error response populates :error."
  (let ((parsed (codex-ide-appserver--parse-message
                 "{\"id\":2,\"error\":{\"code\":-32001,\"message\":\"busy\"}}")))
    (should (eq 'response (plist-get parsed :type)))
    (should (equal 2 (plist-get parsed :id)))
    (should (equal -32001
                   (cdr (assoc "code" (plist-get parsed :error)))))))

(ert-deftest codex-ide-appserver-parse-message-notification ()
  "Parsing a notification (method, no id) returns :type notification."
  (let ((parsed (codex-ide-appserver--parse-message
                 "{\"method\":\"item/agentMessage/delta\",\"params\":{\"delta\":\"hello\"}}")))
    (should (eq 'notification (plist-get parsed :type)))
    (should (equal "item/agentMessage/delta" (plist-get parsed :method)))
    (should (equal "hello"
                   (cdr (assoc "delta" (plist-get parsed :params)))))))

(ert-deftest codex-ide-appserver-parse-message-request ()
  "Parsing a server request (method + id) returns :type request."
  (let ((parsed (codex-ide-appserver--parse-message
                 "{\"method\":\"item/commandExecution/requestApproval\",\"id\":3,\"params\":{\"command\":\"ls\"}}")))
    (should (eq 'request (plist-get parsed :type)))
    (should (equal "item/commandExecution/requestApproval"
                   (plist-get parsed :method)))
    (should (equal 3 (plist-get parsed :id)))
    (should (equal "ls"
                   (cdr (assoc "command" (plist-get parsed :params)))))))

(ert-deftest codex-ide-appserver-parse-message-garbage ()
  "Parsing invalid JSON returns nil."
  (should (null (codex-ide-appserver--parse-message "not json")))
  (should (null (codex-ide-appserver--parse-message "")))
  (should (null (codex-ide-appserver--parse-message "  "))))

(ert-deftest codex-ide-appserver-make-input ()
  "Input builder wraps text in the UserInput Text variant."
  (let ((input (codex-ide-appserver--make-input "hello world")))
    (should (equal 1 (length input)))
    (let ((entry (car input)))
      (should (equal "text" (cdr (assoc "type" entry))))
      (should (equal "hello world" (cdr (assoc "text" entry)))))))

(ert-deftest codex-ide-appserver-approval-response ()
  "Approval response builder produces the expected decision."
  (let ((resp (codex-ide-appserver--approval-response "decline")))
    (should (equal "decline" (cdr (assoc "decision" resp))))))

(ert-deftest codex-ide-appserver-approval-policy-default-auto-deny ()
  "The app-server approval policy defaults to denying approvals."
  (should (eq codex-ide-approval-policy 'auto-deny)))

(ert-deftest codex-ide-appserver-make-approval-decision-auto-deny ()
  "Auto-deny returns an immediate denied response for approval requests."
  (let ((command (codex-ide-appserver--make-approval-decision
                  'auto-deny "item/commandExecution/requestApproval"
                  '(("command" . "make test"))))
        (file-change (codex-ide-appserver--make-approval-decision
                      'auto-deny "item/fileChange/requestApproval"
                      (codex-ide-appserver-tests--file-change-approval-params))))
    (should (eq 'respond (plist-get command :action)))
    (should (equal "decline" (plist-get command :decision)))
    (should (eq 'command-execution (plist-get command :kind)))
    (should (eq 'respond (plist-get file-change :action)))
    (should (equal "decline" (plist-get file-change :decision)))
    (should (eq 'file-change (plist-get file-change :kind)))))

(ert-deftest codex-ide-appserver-make-approval-decision-auto-approve ()
  "Auto-approve returns an immediate approved response for approvals."
  (let ((command (codex-ide-appserver--make-approval-decision
                  'auto-approve "item/commandExecution/requestApproval"
                  '(("command" . "make test"))))
        (file-change (codex-ide-appserver--make-approval-decision
                      'auto-approve "item/fileChange/requestApproval"
                      (codex-ide-appserver-tests--file-change-approval-params))))
    (should (eq 'respond (plist-get command :action)))
    (should (equal "accept" (plist-get command :decision)))
    (should (eq 'respond (plist-get file-change :action)))
    (should (equal "accept" (plist-get file-change :decision)))))

(ert-deftest codex-ide-appserver-make-approval-decision-ask-command ()
  "Ask policy prompts for command-execution approvals."
  (let ((action (codex-ide-appserver--make-approval-decision
                 'ask "item/commandExecution/requestApproval"
                 '(("command" . ("make" "test"))))))
    (should (eq 'prompt (plist-get action :action)))
    (should (eq 'command-execution (plist-get action :kind)))
    (should (string-match-p "make test" (plist-get action :prompt)))))

(ert-deftest codex-ide-appserver-make-approval-decision-ask-filechange ()
  "Ask policy prompts for file-change approvals."
  (let ((action (codex-ide-appserver--make-approval-decision
                 'ask "item/fileChange/requestApproval"
                 (codex-ide-appserver-tests--file-change-approval-params))))
    (should (eq 'prompt (plist-get action :action)))
    (should (eq 'file-change (plist-get action :kind)))
    (should (string-match-p "/tmp/codex-project" (plist-get action :prompt)))
    (should (string-match-p "write access requested"
                            (plist-get action :prompt)))))

(ert-deftest codex-ide-appserver-make-approval-decision-ask-with-diff-command ()
  "Ask-with-diff still prompts for command-execution approvals."
  (let ((action (codex-ide-appserver--make-approval-decision
                 'ask-with-diff "item/commandExecution/requestApproval"
                 '(("command" . "make test")))))
    (should (eq 'prompt (plist-get action :action)))
    (should (eq 'command-execution (plist-get action :kind)))
    (should (string-match-p "make test" (plist-get action :prompt)))))

(ert-deftest codex-ide-appserver-make-approval-decision-ask-with-diff-filechange ()
  "Ask-with-diff prompts for schema-real file-change approvals."
  (let ((action (codex-ide-appserver--make-approval-decision
                 'ask-with-diff "item/fileChange/requestApproval"
                 (codex-ide-appserver-tests--file-change-approval-params))))
    (should (eq 'prompt (plist-get action :action)))
    (should (eq 'file-change (plist-get action :kind)))
    (should (string-match-p "item-file-1" (plist-get action :prompt)))
    (should (eq 'schema-file-change-metadata
                (plist-get action :reason)))))

(ert-deftest codex-ide-appserver-make-approval-decision-malformed-filechange-denied ()
  "Ask-with-diff denies file changes without required schema fields."
  (let ((missing-item (codex-ide-appserver--make-approval-decision
                       'ask-with-diff "item/fileChange/requestApproval"
                       '(("startedAtMs" . 123)
                         ("threadId" . "thread-1")
                         ("turnId" . "turn-1"))))
        (missing-started (codex-ide-appserver--make-approval-decision
                          'ask-with-diff "item/fileChange/requestApproval"
                          '(("itemId" . "item-file-1")
                            ("threadId" . "thread-1")
                            ("turnId" . "turn-1")))))
    (should (eq 'respond (plist-get missing-item :action)))
    (should (equal "decline" (plist-get missing-item :decision)))
    (should (eq 'malformed-file-change
                (plist-get missing-item :reason)))
    (should (eq 'respond (plist-get missing-started :action)))
    (should (equal "decline" (plist-get missing-started :decision)))
    (should (eq 'malformed-file-change
                (plist-get missing-started :reason)))))

(ert-deftest codex-ide-appserver-make-approval-decision-invalid-policy-denied ()
  "Unknown approval policies deny safely."
  (let ((action (codex-ide-appserver--make-approval-decision
                 'surprise "item/commandExecution/requestApproval"
                 '(("command" . "make test")))))
    (should (eq 'respond (plist-get action :action)))
    (should (equal "decline" (plist-get action :decision)))
    (should (eq 'invalid-policy (plist-get action :reason)))))

(ert-deftest codex-ide-appserver-next-id-increments ()
  "Each call to next-id returns an incrementing integer."
  (let ((codex-ide-appserver--next-id 0))
    (should (= 1 (codex-ide-appserver--next-id)))
    (should (= 2 (codex-ide-appserver--next-id)))
    (should (= 3 (codex-ide-appserver--next-id)))))

;;; Request/response correlation tests

(ert-deftest codex-ide-appserver-response-fires-callback ()
  "A response with a matching id fires the registered callback."
  (let ((codex-ide-appserver--pending (make-hash-table :test 'eql))
        (result nil))
    (puthash 42 (lambda (res _err) (setq result res))
             codex-ide-appserver--pending)
    (codex-ide-appserver--dispatch
     `(:type response :id 42 :result (("ok" . t)) :error nil))
    (should (equal '(("ok" . t)) result))
    ;; Callback is removed after firing.
    (should-not (gethash 42 codex-ide-appserver--pending))))

(ert-deftest codex-ide-appserver-response-no-callback ()
  "A response with no registered callback is silently ignored."
  (let ((codex-ide-appserver--pending (make-hash-table :test 'eql)))
    ;; Should not error.
    (codex-ide-appserver--dispatch
     `(:type response :id 99 :result nil :error nil))))

;;; Notification dispatch tests

(ert-deftest codex-ide-appserver-notification-delta-renders ()
  "Dispatching an agentMessage/delta notification appends delta to output."
  (let ((codex-ide-appserver--output-buffer
         (get-buffer-create " *test-appserver-delta*")))
    (unwind-protect
        (progn
          (with-current-buffer codex-ide-appserver--output-buffer
            (erase-buffer))
          (codex-ide-appserver--dispatch
           `(:type notification
             :method "item/agentMessage/delta"
             :params (("delta" . "streamed text"))))
          (should (string-match-p
                   "streamed text"
                   (with-current-buffer codex-ide-appserver--output-buffer
                     (buffer-string)))))
      (kill-buffer codex-ide-appserver--output-buffer))))

(ert-deftest codex-ide-appserver-notification-thread-started ()
  "Dispatching thread/started stores the thread id."
  (let ((codex-ide-appserver--thread-id nil))
    (codex-ide-appserver--dispatch
     `(:type notification
       :method "thread/started"
       :params (("thread" . (("id" . "thread-xyz"))))))
    (should (equal "thread-xyz" codex-ide-appserver--thread-id))))

;;; Approval auto-deny tests

(ert-deftest codex-ide-appserver-command-approval-auto-deny ()
  "A command-execution approval request is auto-denied."
  (let* ((codex-ide-approval-policy 'auto-deny)
         (obj (codex-ide-appserver-tests--dispatch-captured
               `(:type request
                 :method "item/commandExecution/requestApproval"
                 :id 7
                 :params (("command" . "make test"))))))
    (should (equal 7 (cdr (assoc "id" obj))))
    (should (equal "decline"
                   (codex-ide-appserver-tests--response-decision obj)))))

(ert-deftest codex-ide-appserver-filechange-approval-auto-deny ()
  "A file-change approval request is auto-denied."
  (let* ((codex-ide-approval-policy 'auto-deny)
         (obj (codex-ide-appserver-tests--dispatch-captured
               `(:type request
                 :method "item/fileChange/requestApproval"
                 :id 8
                 :params ,(codex-ide-appserver-tests--file-change-approval-params)))))
    (should (equal 8 (cdr (assoc "id" obj))))
    (should (equal "decline"
                   (codex-ide-appserver-tests--response-decision obj)))))

(ert-deftest codex-ide-appserver-command-approval-ask-yes ()
  "Ask policy approves command-execution requests when the user says yes."
  (let ((codex-ide-approval-policy 'ask)
        (prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (text) (setq prompt text) t)))
      (let ((obj (codex-ide-appserver-tests--dispatch-captured
                  `(:type request
                    :method "item/commandExecution/requestApproval"
                    :id 9
                    :params (("command" . "make test"))))))
        (should (string-match-p "make test" prompt))
        (should (equal "accept"
                       (codex-ide-appserver-tests--response-decision obj)))))))

(ert-deftest codex-ide-appserver-command-approval-ask-no ()
  "Ask policy denies command-execution requests when the user says no."
  (let ((codex-ide-approval-policy 'ask)
        (prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (text) (setq prompt text) nil)))
      (let ((obj (codex-ide-appserver-tests--dispatch-captured
                  `(:type request
                    :method "item/commandExecution/requestApproval"
                    :id 10
                    :params (("command" . ("make" "test")))))))
        (should (string-match-p "make test" prompt))
        (should (equal "decline"
                       (codex-ide-appserver-tests--response-decision obj)))))))

(ert-deftest codex-ide-appserver-filechange-approval-ask-with-diff-accepts ()
  "Ask-with-diff accepts schema-real file changes after confirmation."
  (let ((codex-ide-approval-policy 'ask-with-diff)
        (prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (text) (setq prompt text) t)))
      (let ((obj (codex-ide-appserver-tests--dispatch-captured
                  `(:type request
                    :method "item/fileChange/requestApproval"
                    :id 11
                    :params ,(codex-ide-appserver-tests--file-change-approval-params)))))
        (should (string-match-p "item-file-1" prompt))
        (should (equal "accept"
                       (codex-ide-appserver-tests--response-decision obj)))))))

(ert-deftest codex-ide-appserver-filechange-approval-ask-with-diff-rejects ()
  "Ask-with-diff declines schema-real file changes after rejection."
  (let ((codex-ide-approval-policy 'ask-with-diff)
        (prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (text) (setq prompt text) nil)))
      (let ((obj (codex-ide-appserver-tests--dispatch-captured
                  `(:type request
                    :method "item/fileChange/requestApproval"
                    :id 12
                    :params ,(codex-ide-appserver-tests--file-change-approval-params)))))
        (should (string-match-p "item-file-1" prompt))
        (should (equal "decline"
                       (codex-ide-appserver-tests--response-decision obj)))))))

(ert-deftest codex-ide-appserver-filechange-approval-prompt-failure-denies ()
  "Ask-with-diff declines safely when the prompt errors or quits."
  (let ((codex-ide-approval-policy 'ask-with-diff))
    (dolist (failure '(error quit))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_text)
                   (pcase failure
                     ('error (error "prompt failed"))
                     ('quit (signal 'quit nil))))))
        (let ((obj (codex-ide-appserver-tests--dispatch-captured
                    `(:type request
                      :method "item/fileChange/requestApproval"
                      :id 13
                      :params ,(codex-ide-appserver-tests--file-change-approval-params)))))
          (should (equal "decline"
                         (codex-ide-appserver-tests--response-decision obj))))))))

;;; Line accumulation / filter tests
;;
;; These tests exercise `codex-ide-appserver--process-pending-lines' directly
;; against a buffer, simulating what the process filter does.  We use a mock
;; process whose buffer holds the accumulated partial data.

(ert-deftest codex-ide-appserver-filter-complete-line ()
  "A complete line is dispatched immediately."
  (let* ((output-buf (get-buffer-create " *test-filter-out-1*"))
         (proc (make-pipe-process
                :name "test-appserver-1"
                :buffer (generate-new-buffer " *test-proc-1*")
                :noquery t))
         (codex-ide-appserver--process proc)
         (codex-ide-appserver--output-buffer output-buf))
    (unwind-protect
        (progn
          (with-current-buffer output-buf (erase-buffer))
          (with-current-buffer (process-buffer proc)
            (insert "{\"method\":\"item/agentMessage/delta\",\"params\":{\"delta\":\"hi\"}}\n")
            (codex-ide-appserver--process-pending-lines))
          (should (string-match-p
                   "hi"
                   (with-current-buffer output-buf
                     (buffer-string)))))
      (delete-process proc)
      (kill-buffer output-buf))))

(ert-deftest codex-ide-appserver-filter-partial-then-complete ()
  "A partial line followed by its remainder is dispatched after completion."
  (let* ((output-buf (get-buffer-create " *test-filter-out-2*"))
         (proc (make-pipe-process
                :name "test-appserver-2"
                :buffer (generate-new-buffer " *test-proc-2*")
                :noquery t))
         (codex-ide-appserver--process proc)
         (codex-ide-appserver--output-buffer output-buf))
    (unwind-protect
        (progn
          (with-current-buffer output-buf (erase-buffer))
          (with-current-buffer (process-buffer proc)
            (insert "{\"method\":\"item/agentMessage/delta\",\"par")
            (codex-ide-appserver--process-pending-lines)
            ;; Nothing dispatched yet: no complete line.
            (should (string= ""
                             (with-current-buffer output-buf
                               (buffer-string))))
            (insert "ams\":{\"delta\":\"world\"}}\n")
            (codex-ide-appserver--process-pending-lines))
          (should (string-match-p
                   "world"
                   (with-current-buffer output-buf
                     (buffer-string)))))
      (delete-process proc)
      (kill-buffer output-buf))))

(ert-deftest codex-ide-appserver-filter-multiple-lines ()
  "Multiple lines in one chunk are dispatched separately and in order."
  (let* ((output-buf (get-buffer-create " *test-filter-out-3*"))
         (proc (make-pipe-process
                :name "test-appserver-3"
                :buffer (generate-new-buffer " *test-proc-3*")
                :noquery t))
         (codex-ide-appserver--process proc)
         (codex-ide-appserver--output-buffer output-buf))
    (unwind-protect
        (progn
          (with-current-buffer output-buf (erase-buffer))
          (with-current-buffer (process-buffer proc)
            (insert "{\"method\":\"item/agentMessage/delta\",\"params\":{\"delta\":\"A\"}}\n")
            (insert "{\"method\":\"item/agentMessage/delta\",\"params\":{\"delta\":\"B\"}}\n")
            (codex-ide-appserver--process-pending-lines))
          (should (string= "AB"
                           (with-current-buffer output-buf
                             (buffer-string)))))
      (delete-process proc)
      (kill-buffer output-buf))))

(provide 'codex-ide-appserver-tests)

;;; codex-ide-appserver-tests.el ends here
