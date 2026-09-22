;;; hermes-request-tests.el --- Headless request tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hermes-request)
(require 'hermes-test-helpers)

(defmacro hermes-request-test--with-client (&rest body)
  "Run BODY with an isolated wire-level dashboard fixture."
  (declare (indent 0) (debug t))
  `(let* ((h-client (make-hermes-dashboard-transport-client
                    :websocket 'fixture :ready-p t :callback #'ignore
                    :subscribers (make-hash-table :test #'eq)
                    :session-index (make-hash-table :test #'equal)))
          (h-catalogue (hermes--promise-make))
          h-frames h-results h-errors h-cancel
          (h-releases 0)
          (hermes-dashboard-transport-websocket-send-function
           (lambda (_socket text) (push (json-parse-string text :object-type 'alist) h-frames))))
     (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                (lambda (&rest _) h-client))
               ((symbol-function 'hermes-dashboard-transport-release)
                (lambda (client) (should (eq client h-client)) (cl-incf h-releases)))
               ((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method path &rest args)
                  (should (equal (list method path) '("GET" "/api/profiles")))
                  (should (eq (plist-get args :client) h-client))
                  h-catalogue)))
       (unwind-protect
           (progn ,@body)
         (when h-cancel (funcall h-cancel))
         (hermes-dashboard-transport--reject-pending-requests h-client "Fixture retired")
         (hermes-test--event-loop-barrier)
         (should (zerop (hash-table-count
                         (hermes-dashboard-transport-client-pending h-client))))))))

(defmacro hermes-request-test--start ()
  "Start the public API against the fixture."
  '(setq h-cancel
         (hermes-request '(:prompt "Question α\nline two" :profile "study-eval")
                         (lambda (text) (push text h-results))
                         (lambda (reason) (push reason h-errors)))))

(defun hermes-request-test--catalogue (promise)
  "Resolve PROMISE with an exact backend profile catalogue."
  (hermes--promise-resolve
   promise '((profiles . [((name . "study-eval")
                          (model . "remote/Exact-Model")
                          (provider . "remote-provider"))]))))

(ert-deftest hermes-request-profile-model-overrides-launch-at-wire ()
  (dolist (choices '(("remote/Exact-Model" "remote-provider")
                     ("remote/Μοντέλο" "πάροχος")
                     ("\u200bmodel\u200b" "\u200bprovider\u200b")))
    (hermes-request-test--with-client
      (let ((process-environment (copy-sequence process-environment)))
        (setenv "HERMES_MODEL" "launch-model")
        (setenv "HERMES_INFERENCE_MODEL" "other-launch-model")
        (hermes-request-test--start)
        (hermes--promise-resolve
         h-catalogue `((profiles . [((name . "study-eval")
                                    (model . ,(car choices))
                                    (provider . ,(cadr choices)))])))
        (should (equal (alist-get 'method (car h-frames)) "session.create"))
        (let ((params (alist-get 'params (car h-frames))))
          ;; The backend gives these create overrides precedence over launch env.
          ;; Inspect actual typed-RPC serialization, not a mocked create helper.
          (should (equal (alist-get 'model params) (car choices)))
          (should (equal (alist-get 'provider params) (cadr choices)))
          (should-not (alist-get 'api_key params))
          (should-not (alist-get 'fallback_model params)))))))

(ert-deftest hermes-request-profile-model-and-provider-required ()
  (dolist (field '(model provider))
    (dolist (value (append '(nil "" " " " model" "model " "model\n" 42 t ((default . "model")))
                          (mapcan (lambda (space)
                                    (list space (concat space "model") (concat "model" space)))
                                  '("\u00a0" "\u2003" "\u0085" "\u1680" "\u2028" "\u2029"))))
      (hermes-request-test--with-client
        (hermes-request-test--start)
        (let ((row (list (cons 'name "study-eval")
                         (cons 'model "remote/Exact-Model")
                         (cons 'provider "remote-provider"))))
          (setf (alist-get field row) value)
          (hermes--promise-resolve h-catalogue `((profiles . [,row]))))
        (should-not h-errors)
        (hermes-test--wait-until (lambda () h-errors) nil "profile rejection")
        (should (= (length h-errors) 1))
        (funcall h-cancel)
        (funcall h-cancel)
        (hermes-test--event-loop-barrier)
        (should (= (length h-errors) 1))
        (should-not h-results)
        (should-not h-frames)
        (should (= h-releases 1))))))

(defun hermes-request-test--reply (client frame result &optional failure)
  "Deliver RESULT for FRAME on CLIENT, or FAILURE instead."
  (hermes-dashboard-transport--handle-frame
   client (json-encode `((jsonrpc . "2.0") (id . ,(alist-get 'id frame))
                         (,(if failure 'error 'result) . ,(or failure result))))))

(defun hermes-request-test--created (client frame &optional profile count)
  "Reply to create FRAME on CLIENT with PROFILE and COUNT."
  (hermes-request-test--reply
   client frame `((session_id . "fresh") (message_count . ,(or count 0))
                  (info . ((profile_name . ,(or profile "study-eval")))))))

(defun hermes-request-test--event (client type payload &optional session)
  "Deliver TYPE and PAYLOAD to CLIENT for SESSION."
  (hermes-dashboard-transport--handle-frame
   client (json-encode `((jsonrpc . "2.0") (method . "event")
                         (params . ((type . ,type) (session_id . ,(or session "fresh"))
                                    (payload . ,payload)))))))

(ert-deftest hermes-request-public-success-and-early-terminal ()
  (dolist (early '(nil t))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (should (functionp h-cancel))
      (should-not h-frames)
      (hermes-request-test--catalogue h-catalogue)
      (let ((create (car h-frames)))
        (should (equal (alist-get 'method create) "session.create"))
        (let ((params (alist-get 'params create)))
          (should (equal (alist-get 'profile params) "study-eval"))
          (should (equal (alist-get 'messages params) []))
          (should (eq (alist-get 'hidden params) t))
          (should (eq (alist-get 'close_on_disconnect params) t)))
        ;; A create-time notification cannot cause submission or completion.
        (hermes-request-test--event h-client "message.complete" '((status . "complete") (text . "old")))
        (should (= (length h-frames) 1))
        (hermes-request-test--created h-client create)
        (let ((submit (car h-frames)))
          (should (equal (alist-get 'method submit) "prompt.submit"))
          (should (equal (alist-get 'params submit)
                         '((session_id . "fresh") (text . "Question α\nline two"))))
          (hermes-request-test--event h-client "reasoning.delta" '((text . "private thinking")))
          (hermes-request-test--event h-client "message.delta" '((text . "partial")))
          (hermes-request-test--event h-client "message.interim" '((text . "interim")))
          (hermes-request-test--event h-client "message.complete"
                                     '((status . "complete") (text . "wrong")) "other")
          (unless early (hermes-request-test--reply h-client submit '((status . "streaming"))))
          (hermes-request-test--event h-client "message.complete"
                                     '((status . "complete") (text . "Final α\n  preserved")))
          (should-not h-results)
          (when early
            (should (equal (alist-get 'method (car h-frames)) "prompt.submit"))
            (hermes-request-test--reply h-client submit '((status . "streaming"))))
          (should (equal (alist-get 'method (car h-frames)) "session.close"))
          (hermes-request-test--reply h-client (car h-frames) '((closed . t)))
          ;; Duplicate RPC and stale events cannot submit or settle again.
          (hermes-request-test--created h-client create)
          (hermes-request-test--event h-client "message.complete" '((status . "complete") (text . "stale")))
          (hermes-test--wait-until (lambda () h-results) nil "final reply")
          (hermes-test--event-loop-barrier)
          (should (equal h-results '("Final α\n  preserved")))
          (should-not h-errors)
          (should (= h-releases 1)))))))

(ert-deftest hermes-request-cancel-during-create-closes-late-handle ()
  (hermes-request-test--with-client
    (hermes-request-test--start)
    (hermes-request-test--catalogue h-catalogue)
    (let ((create (car h-frames)))
      (funcall h-cancel)
      (funcall h-cancel)
      (hermes-test--wait-until (lambda () h-errors) nil "cancel notification")
      (should (= (length h-errors) 1))
      (should (= h-releases 0))
      (hermes-request-test--created h-client create)
      (should (equal (mapcar (lambda (frame) (alist-get 'method frame)) h-frames)
                     '("session.close" "session.create")))
      (should (equal (alist-get 'params (car h-frames)) '((session_id . "fresh"))))
      (hermes-request-test--reply h-client (car h-frames) '((closed . t)))
      (should (= h-releases 1))
      (should-not h-results))))

(ert-deftest hermes-request-cancel-before-catalogue-never-creates ()
  (hermes-request-test--with-client
    (hermes-request-test--start)
    (funcall h-cancel)
    (hermes-request-test--catalogue h-catalogue)
    (hermes-test--wait-until (lambda () h-errors) nil "cancel notification")
    (should-not h-frames)
    (should-not h-results)
    (should (= (length h-errors) 1))
    (should (= h-releases 1))))

(ert-deftest hermes-request-cancel-before-ready-never-sends ()
  (hermes-request-test--with-client
    (setf (hermes-dashboard-transport-client-ready-p h-client) nil
          (hermes-dashboard-transport-client-ready-promise h-client) (hermes--promise-make))
    (hermes-request-test--start)
    (hermes-request-test--catalogue h-catalogue)
    (funcall h-cancel)
    (hermes--promise-resolve (hermes-dashboard-transport-client-ready-promise h-client) t)
    (hermes-test--wait-until (lambda () h-errors) nil "cancel notification")
    (should-not h-frames)
    (should (= h-releases 1))
    (should (= (length h-errors) 1))))

(ert-deftest hermes-request-profile-absent-or-catalogue-failure ()
  (dolist (catalogue '(nil ((profiles . [])) ((profiles . [((name . "default"))]))))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (hermes--promise-resolve h-catalogue catalogue)
      (hermes-test--wait-until (lambda () h-errors) nil "catalogue rejection")
      (should (= (length h-errors) 1))
      (should-not h-results)
      (should-not h-frames)
      (should (= h-releases 1))))
  (hermes-request-test--with-client
    (hermes-request-test--start)
    (hermes--promise-reject h-catalogue "No catalogue")
    (hermes-test--wait-until (lambda () h-errors) nil "catalogue rejection")
    (should (= (length h-errors) 1))
    (should-not h-frames)))

(ert-deftest hermes-request-profile-readback-and-empty-session-required ()
  (dolist (params '(("default" 0) ("study-eval" 1)))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (hermes-request-test--catalogue h-catalogue)
      (apply #'hermes-request-test--created h-client (car h-frames) params)
      (hermes-test--wait-until (lambda () h-errors) nil "session rejection")
      (should (= (length h-errors) 1))
      (should (equal (alist-get 'method (car h-frames)) "session.close"))
      (should-not (seq-find (lambda (frame) (equal (alist-get 'method frame) "prompt.submit")) h-frames)))))

(ert-deftest hermes-request-invalid-arguments-do-not-acquire ()
  (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
             (lambda (&rest _) (ert-fail "Invalid request acquired a client"))))
    (dolist (profile '(nil "" " Study-eval" "study-eval " "../x" "UPPER" "-name" "éval"))
      (should-error (hermes-request (list :prompt "Q" :profile profile) #'ignore #'ignore)))
    (should-error (hermes-request '(:prompt 42 :profile "default") #'ignore #'ignore))))

(ert-deftest hermes-request-failure-terminal-malformation-and-interaction ()
  (dolist (event '(("message.complete" ((status . "error") (text . "not success")))
                   ("message.complete" ((status . "interrupted") (text . "partial")))
                   ("message.complete" ((text . "missing status")))
                   ("message.complete" ((status . "complete") (text . 42)))
                   ("message.complete" ((status . "complete") (text . "conflict") (error . "failed")))
                   ("message.complete" ((status . "complete") (content . "not text")))
                   ("message.complete" ((status . "complete") (text . "")))
                   ("error" ((message . "failed")))
                   ("clarify.request" ((question . "More?")))))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (hermes-request-test--catalogue h-catalogue)
      (hermes-request-test--created h-client (car h-frames))
      (hermes-request-test--reply h-client (car h-frames) '((status . "streaming")))
      (apply #'hermes-request-test--event h-client event)
      (hermes-test--wait-until (lambda () h-errors) nil "terminal rejection")
      (should-not h-results)
      (should (= (length h-errors) 1))
      (should (equal (alist-get 'method (car h-frames)) "session.close")))))

(ert-deftest hermes-request-admission-failure-overrides-early-terminal ()
  (dolist (reply '("error" "queued" "steered" "complete"))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (hermes-request-test--catalogue h-catalogue)
      (hermes-request-test--created h-client (car h-frames))
      (let ((submit (car h-frames)))
        (hermes-request-test--event h-client "message.complete" '((status . "complete") (text . "early")))
        (hermes-request-test--reply h-client submit `((status . ,reply))
                                    (and (equal reply "error") '((code . 4000) (message . "failed"))))
        (hermes-test--wait-until (lambda () h-errors) nil "admission rejection")
        (should-not h-results)
        (should (= (length h-errors) 1))))))

(ert-deftest hermes-request-timeout-and-disconnect-fail-closed ()
  (dolist (cause '(timeout disconnect cancel))
    (hermes-request-test--with-client
      (let ((hermes-request-timeout (if (eq cause 'timeout) 0.01 180)))
        (hermes-request-test--start))
      (hermes-request-test--catalogue h-catalogue)
      (hermes-request-test--created h-client (car h-frames))
      (let ((submit (car h-frames)))
        (pcase cause
          ('timeout (hermes-test--wait-until (lambda () h-errors) nil "real request deadline"))
          ('disconnect
           (let ((record (car (hash-table-values (hermes-dashboard-transport-client-subscribers h-client)))))
             (funcall (plist-get record :retire))))
          ('cancel (funcall h-cancel)))
        (hermes-request-test--reply h-client submit '((status . "streaming")))
        (hermes-request-test--event h-client "message.complete" '((status . "complete") (text . "late")))
        (hermes-test--event-loop-barrier)
        (should-not h-results)
        (should (= (length h-errors) 1))))))

(ert-deftest hermes-request-create-failure-or-missing-handle-releases ()
  (dolist (failure '(nil t))
    (hermes-request-test--with-client
      (hermes-request-test--start)
      (hermes-request-test--catalogue h-catalogue)
      (hermes-request-test--reply h-client (car h-frames) '((info))
                                  (and failure '((code . 5000) (message . "create failed"))))
      (hermes-test--wait-until (lambda () h-errors) nil "create rejection")
      (should (= (length h-errors) 1))
      (should (= h-releases 1))
      (should (= (length h-frames) 1)))))

(ert-deftest hermes-request-cold-start-waits-before-http-catalogue ()
  (hermes-request-test--with-client
    (let ((reads 0)
          (waits 0)
          (when-ready (symbol-function 'hermes-dashboard-transport-when-ready))
          (ready (hermes--promise-make)))
      (setf (hermes-dashboard-transport-client-ready-p h-client) nil
            (hermes-dashboard-transport-client-ready-promise h-client) ready)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) (cl-incf reads) h-catalogue))
                ((symbol-function 'hermes-dashboard-transport-when-ready)
                 (lambda (&rest args)
                   (cl-incf waits)
                   (apply when-ready args))))
        (hermes-request-test--start)
        (should (= waits 1))
        (should (= reads 0))
        (setf (hermes-dashboard-transport-client-ready-p h-client) t)
        (hermes--promise-resolve ready t)
        (should (= reads 1))
        (hermes-request-test--catalogue h-catalogue)
        (should (equal (alist-get 'method (car h-frames)) "session.create"))))))

(ert-deftest hermes-request-readiness-failure-does-not-read-catalogue ()
  (hermes-request-test--with-client
    (let ((ready (hermes--promise-make)) (reads 0))
      (setf (hermes-dashboard-transport-client-ready-p h-client) nil
            (hermes-dashboard-transport-client-ready-promise h-client) ready)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) (cl-incf reads) h-catalogue)))
        (hermes-request-test--start)
        (hermes--promise-reject ready "Connection failed")
        (sleep-for 0.01)
        (should (= reads 0))
        (should (= (length h-errors) 1))
        (should (= h-releases 1))
        (should-not h-results)
        (should-not h-frames)))))

(ert-deftest hermes-request-cancelled-create-timeout-releases ()
  (hermes-request-test--with-client
    (hermes-request-test--start)
    (hermes-request-test--catalogue h-catalogue)
    (let ((create (car h-frames)))
      (funcall h-cancel)
      (hermes-dashboard-transport--on-request-timeout h-client (alist-get 'id create))
      (hermes-test--wait-until (lambda () (and h-errors (= h-releases 1)))
                              nil "cancelled create release")
      (should (= h-releases 1))
      (should (= (length h-errors) 1))
      (should (= (hash-table-count (hermes-dashboard-transport-client-pending h-client)) 0)))))

(ert-deftest hermes-request-retirement-does-not-close-on-successor-connection ()
  (hermes-request-test--with-client
    (hermes-request-test--start)
    (hermes-request-test--catalogue h-catalogue)
    (hermes-request-test--created h-client (car h-frames))
    (let ((record (car (hash-table-values (hermes-dashboard-transport-client-subscribers h-client)))))
      (cl-incf (hermes-dashboard-transport-client-generation h-client))
      (funcall (plist-get record :retire))
      (hermes-test--wait-until (lambda () h-errors) nil "retirement notification")
      (should (= (length h-errors) 1))
      (should (= h-releases 1))
      (should-not (seq-find (lambda (frame) (equal (alist-get 'method frame) "session.close")) h-frames)))))

(ert-deftest hermes-request-keeps-other-chat-owner-and-events ()
  (hermes-request-test--with-client
    (let* (other-events
           (other-token (hermes-dashboard-transport-subscribe
                         h-client (lambda (event) (push event other-events)))))
      (hermes-dashboard-transport-subscribe-session h-client other-token "chat")
      (hermes-request-test--start)
      (hermes-request-test--catalogue h-catalogue)
      (hermes-request-test--event h-client "session.info" '((profile_name . "study-eval")))
      (should-not other-events)
      (hermes-request-test--created h-client (car h-frames))
      (hermes-request-test--event h-client "message.delta" '((text . "chat text")) "chat")
      (should (= (length other-events) 1))
      (should (equal (plist-get (car other-events) :content) "chat text"))
      (funcall h-cancel)
      (hermes-request-test--reply h-client (car h-frames) '((closed . t)))
      (should (gethash other-token (hermes-dashboard-transport-client-subscribers h-client)))
      (should (eq (gethash "chat" (hermes-dashboard-transport-client-session-index h-client)) other-token))
      (hermes-request-test--event h-client "message.delta" '((text . "still running")) "chat")
      (should (= (length other-events) 2)))))

(provide 'hermes-request-tests)
;;; hermes-request-tests.el ends here
