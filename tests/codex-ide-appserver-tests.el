;;; codex-ide-appserver-tests.el --- Tests for codex-ide-appserver.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'ert)
(require 'codex-ide-appserver)

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
    (should (equal 3 (plist-get parsed :id)))))

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
  (let ((resp (codex-ide-appserver--approval-response "denied")))
    (should (equal "denied" (cdr (assoc "decision" resp))))))

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
  (let ((codex-ide-appserver--next-id 0)
        (codex-ide-appserver--send-function nil)
        (captured nil))
    (setq codex-ide-appserver--send-function
          (lambda (str) (setq captured (concat captured str))))
    (codex-ide-appserver--dispatch
     `(:type request
       :method "item/commandExecution/requestApproval"
       :id 7))
    (should captured)
    (codex-ide-appserver--with-json-conventions
      (let* ((lines (split-string captured "\n" t))
             (last-line (car (last lines)))
             (obj (json-read-from-string last-line)))
        (should (equal 7 (cdr (assoc "id" obj))))
        (should (equal "denied"
                       (cdr (assoc "decision"
                                   (cdr (assoc "result" obj))))))))))

(ert-deftest codex-ide-appserver-filechange-approval-auto-deny ()
  "A file-change approval request is auto-denied."
  (let ((codex-ide-appserver--next-id 0)
        (codex-ide-appserver--send-function nil)
        (captured nil))
    (setq codex-ide-appserver--send-function
          (lambda (str) (setq captured (concat captured str))))
    (codex-ide-appserver--dispatch
     `(:type request
       :method "item/fileChange/requestApproval"
       :id 8))
    (should captured)
    (codex-ide-appserver--with-json-conventions
      (let* ((lines (split-string captured "\n" t))
             (last-line (car (last lines)))
             (obj (json-read-from-string last-line)))
        (should (equal 8 (cdr (assoc "id" obj))))
        (should (equal "denied"
                       (cdr (assoc "decision"
                                   (cdr (assoc "result" obj))))))))))

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
