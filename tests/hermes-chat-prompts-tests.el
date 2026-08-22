;;; hermes-chat-prompts-tests.el --- prompt flow tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-prompts': approval/clarify/sudo/secret/terminal
;; prompt requests, auto-prompting, FIFO approval queueing, response
;; dispatch, and secret redaction in responses and errors.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defun hermes-test--auto-prompt-calls (calls)
  "Return automatic prompt timer CALLS in scheduling order."
  (cl-remove-if-not (lambda (call)
                      (eq (car call) #'hermes-chat--run-auto-prompt))
                    calls))

(defun hermes-test--last-auto-prompt-call (calls)
  "Return the last automatic prompt timer call from CALLS."
  (car (last (hermes-test--auto-prompt-calls calls))))

(cl-defmacro hermes-test-with-auto-prompt-session
    ((client calls prompted) &rest body)
  "Run BODY in an automatic prompt session with captured timer CALLS."
  (declare (indent 1))
  `(let (,calls (,prompted 0))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_secs _repeat function &rest args)
                  (setq ,calls (append ,calls (list (cons function args))))
                  'fake-timer))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'get-buffer-window)
                (lambda (&rest _args) (selected-window)))
               ((symbol-function 'completing-read)
                (lambda (&rest _args) (cl-incf ,prompted) "Deny")))
       (let ((noninteractive nil)
             (hermes-chat-auto-prompt-requests t))
         (hermes-test-with-dashboard-prompt-session (,client)
           (setq ,calls nil)
           ,@body)))))

(ert-deftest hermes-chat-prompt-notification-keeps-sensitive-content-generic ()
  "A secret request notifies without copying its command or prompt contents."
  (let (notice)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (setq notice arguments))))
      (let ((hermes-chat-auto-prompt-requests nil))
        (hermes-test-with-dashboard-prompt-session (client)
          (hermes-test--emit-dashboard-prompt
           client "secret.request"
           '((command . "publish-private-token")
             (description . "enter production credential")
             (env_var . "PRIVATE_TOKEN")))
          (should (eq (car notice) 'prompt))
          (should (string-match-p "Secret" (nth 2 notice)))
          (should-not (string-match-p "publish-private-token" (nth 2 notice)))
          (should-not (string-match-p "production credential" (nth 2 notice))))))))

(ert-deftest hermes-chat-handles-approval-request ()
  (let (respond-client respond-session respond-choice respond-all)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (client &rest args)
                 (setq respond-client client
                       respond-session (plist-get args :session-id)
                       respond-choice (plist-get args :choice)
                       respond-all (plist-get args :all))
                 (funcall (plist-get args :resolve)
                          '((resolved . 1))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm -rf /tmp/demo")
           (description . "dangerous delete")
           (pattern_key . "rm-rf")))
        (should (gethash "approval:sid-prompt" hermes-chat--pending-prompts))
        (should (string-match-p "dangerous delete" (buffer-string)))
        (should (string-match-p "Approval requested"
                                (hermes-test--header-line-string)))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (should (eq respond-client client))
        (should (equal respond-session "sid-prompt"))
        (should (equal respond-choice "once"))
        (should-not respond-all)
        (should-not (gethash "approval:sid-prompt"
                             hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-auto-prompts-visible-approval-request ()
  (let (timer-calls respond-choice seen-default)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (push (cons function args) timer-calls)
                 'fake-timer))
              ((symbol-function 'get-buffer-window)
               (lambda (_buffer &optional _all-frames) (selected-window)))
              ((symbol-function 'completing-read)
               (lambda (_prompt _candidates &rest args)
                 (setq seen-default (nth 4 args))
                 "Approve once"))
              ((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (setq respond-choice (plist-get args :choice))
                 (funcall (plist-get args :resolve)
                          '((resolved . 1))))))
      (let ((noninteractive nil)
            (hermes-chat-auto-prompt-requests t))
        (hermes-test-with-dashboard-prompt-session (client)
          (setq timer-calls nil)
          (hermes-test--emit-dashboard-prompt
           client "approval.request"
           '((command . "rm -rf /tmp/demo")
             (description . "dangerous delete")
             (pattern_key . "rm-rf")))
          (should (gethash "approval:sid-prompt" hermes-chat--pending-prompts))
          (let ((call (cl-find #'hermes-chat--run-auto-prompt timer-calls
                               :key #'car :test #'eq)))
            (should call)
            (apply (car call) (cdr call)))
          (should (equal seen-default "Cancel / ignore"))
          (should (equal respond-choice "once"))
          (should-not (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))))))

(ert-deftest hermes-chat-auto-prompt-does-not-open-for-hidden-buffer ()
  (let (timer-calls)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (push (cons function args) timer-calls)
                 'fake-timer))
              ((symbol-function 'get-buffer-window)
               (lambda (_buffer &optional _all-frames) nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (error "hidden buffer should not prompt"))))
      (let ((noninteractive nil)
            (hermes-chat-auto-prompt-requests t))
        (hermes-test-with-dashboard-prompt-session (client)
          (setq timer-calls nil)
          (hermes-test--emit-dashboard-prompt
           client "approval.request"
           '((command . "rm -rf /tmp/demo")
             (description . "dangerous delete")
             (pattern_key . "rm-rf")))
          (should (gethash "approval:sid-prompt" hermes-chat--pending-prompts))
          (should-not (cl-find #'hermes-chat--run-auto-prompt timer-calls
                               :key #'car :test #'eq)))))))

(ert-deftest hermes-chat-clarify-does-not-auto-open-minibuffer ()
  "A visible clarification waits for the chat input or `C-c C-a'."
  (let (timer-calls)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (push (cons function args) timer-calls)
                 'fake-timer))
              ((symbol-function 'get-buffer-window)
               (lambda (_buffer &optional _all-frames) (selected-window))))
      (let ((noninteractive nil)
            (hermes-chat-auto-prompt-requests t))
        (hermes-test-with-dashboard-prompt-session (client)
          (setq timer-calls nil)
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           '((request_id . "req-input")
             (question . "Which branch should I use?")))
          (should-not (cl-find #'hermes-chat--run-auto-prompt timer-calls
                               :key #'car :test #'eq)))))))

(ert-deftest hermes-chat-auto-prompt-defers-while-minibuffer-active ()
  (let (timer-calls prompted
        (depth 1))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat function &rest args)
                 (push (cons function args) timer-calls)
                 'fake-timer))
              ((symbol-function 'get-buffer-window)
               (lambda (_buffer &optional _all-frames) (selected-window)))
              ((symbol-function 'minibuffer-depth)
               (lambda () depth))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq prompted t)
                 "Deny"))
              ((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((resolved . 1))))))
      (let ((noninteractive nil)
            (hermes-chat-auto-prompt-requests t))
        (hermes-test-with-dashboard-prompt-session (client)
          (setq timer-calls nil)
          (hermes-test--emit-dashboard-prompt
           client "approval.request"
           '((command . "rm -rf /tmp/demo")
             (description . "dangerous delete")
             (pattern_key . "rm-rf")))
          (let ((call (cl-find #'hermes-chat--run-auto-prompt timer-calls
                               :key #'car :test #'eq)))
            (should call)
            (setq timer-calls nil)
            (apply (car call) (cdr call)))
          (should-not prompted)
          (let ((call (cl-find #'hermes-chat--run-auto-prompt timer-calls
                               :key #'car :test #'eq)))
            (should call)
            (setq depth 0)
            (apply (car call) (cdr call)))
          (should prompted)
          (should-not (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))))))

(ert-deftest hermes-chat-prompt-lifecycle-disconnect-invalidates-auto-prompt ()
  "An automatic prompt scheduled by an old chat lifecycle cannot open later."
  (hermes-test-with-auto-prompt-session (client timer-calls prompted)
    (let (sent)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
                 (lambda (&rest _args) (setq sent t))))
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "first")))
        (let ((call (hermes-test--last-auto-prompt-call timer-calls)))
          (hermes-chat-disconnect)
          (apply (car call) (cdr call)))
        (should (zerop prompted))
        (should-not sent)
        (should (zerop (hash-table-count hermes-chat--auto-prompt-keys)))))))

(ert-deftest hermes-chat-auto-prompt-removal-does-not-claim-successor ()
  "A removed prompt's timer cannot claim a same-key successor."
  (hermes-test-with-auto-prompt-session (client timer-calls prompted)
    (let ((sent 0))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
                 (lambda (_client &rest args)
                   (cl-incf sent)
                   (funcall (plist-get args :resolve) '((resolved . 1))))))
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "first")))
        (hermes-chat--clear-pending-prompts "sid-prompt")
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "second")))
        (pcase-let* ((`(,old-call ,new-call)
                      (hermes-test--auto-prompt-calls timer-calls))
                     (new-context (nth 2 (cdr new-call))))
          (apply (car old-call) (cdr old-call))
          (should (zerop prompted))
          (should (zerop sent))
          (should (eq (gethash "approval:sid-prompt"
                               hermes-chat--auto-prompt-keys)
                      (plist-get new-context :claim)))
          (apply (car new-call) (cdr new-call)))
        (should (= prompted 1))
        (should (= sent 1))))))

(defun hermes-test--exercise-auto-prompt-response-race (reject-p)
  "Prove a same-key successor survives response completion or REJECT-P."
  (hermes-test-with-auto-prompt-session (client timer-calls prompted)
    (let (callback (sent 0))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
                 (lambda (_client &rest args)
                   (cl-incf sent)
                   (if (= sent 1)
                       (setq callback (plist-get args
                                                 (if reject-p :reject :resolve)))
                     (funcall (plist-get args :resolve) '((resolved . 1)))))))
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "first")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (setq timer-calls nil)
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "second")))
        (let ((stale-call (hermes-test--last-auto-prompt-call timer-calls)))
          (funcall callback (if reject-p "transport failure" '((resolved . 1))))
          (let* ((fresh-call (hermes-test--last-auto-prompt-call timer-calls))
                 (fresh-context (nth 2 (cdr fresh-call))))
            (apply (car stale-call) (cdr stale-call))
            (should (zerop prompted))
            (should (= sent 1))
            (should (eq (gethash "approval:sid-prompt"
                                 hermes-chat--auto-prompt-keys)
                        (plist-get fresh-context :claim)))
            (apply (car fresh-call) (cdr fresh-call))))
        (should (= prompted 1))
        (should (= sent 2))
        (unless reject-p
          (hermes-test--emit-dashboard-prompt
           client "approval.request" '((command . "third")))
          (let ((call (hermes-test--last-auto-prompt-call timer-calls)))
            (apply (car call) (cdr call)))
          (should (= prompted 2))
          (should (= sent 3)))))))

(ert-deftest hermes-chat-auto-prompt-completion-refreshes-successor-claim ()
  "Response completion refreshes a same-key successor's prompt claim."
  (hermes-test--exercise-auto-prompt-response-race nil))

(ert-deftest hermes-chat-auto-prompt-rejection-refreshes-owned-claim ()
  "Response rejection refreshes only an existing automatic prompt claim."
  (hermes-test--exercise-auto-prompt-response-race t))

(ert-deftest hermes-chat-approval-candidates-follow-backend-choices ()
  (let* ((prompt '(:prompt-type "approval"
                   :choices ["once" "deny"]))
         (candidates (hermes-chat--approval-response-candidates prompt)))
    (should (equal (mapcar #'cdr candidates) '("once" "deny" nil)))
    (should (equal (mapcar #'car candidates)
                   '("Approve once" "Deny" "Cancel / ignore")))))

(ert-deftest hermes-chat-read-approval-response-offers-full-default-vocabulary ()
  "Without explicit choices the full once/session/always/deny set is offered.
The backend never gates \"always\", so it must not be filtered locally."
  (let (seen-candidates)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq seen-candidates candidates)
                 "Deny")))
      (should (equal (hermes-chat--read-prompt-response
                      '(:prompt-type "approval"))
                     "deny"))
      (should (member "Approve once" seen-candidates))
      (should (member "Approve for session" seen-candidates))
      (should (member "Always approve" seen-candidates))
      (should (member "Deny" seen-candidates))
      (should (member "Cancel / ignore" seen-candidates)))))

(ert-deftest hermes-chat-read-approval-response-can-cancel ()
  (let (seen-candidates cancelled)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq seen-candidates candidates)
                 "Cancel / ignore")))
      (condition-case nil
          (hermes-chat--read-prompt-response '(:prompt-type "approval"))
        (quit (setq cancelled t)))
      (should cancelled)
      (should (member "Always approve" seen-candidates))
      (should (member "Cancel / ignore" seen-candidates)))))

(ert-deftest hermes-chat-read-clarify-allows-free-text-answer ()
  "Clarify choices are suggestions: completion does not require a match."
  (let (require-match)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices &optional _pred match &rest _)
                 (setq require-match match)
                 "my own answer")))
      (should (equal (hermes-chat--read-prompt-response
                      '(:prompt-type "clarify" :choices ["a" "b"]))
                     "my own answer"))
      (should-not require-match))))

(ert-deftest hermes-chat-approval-ignores-allow-permanent-field ()
  "The gateway approval payload never carries `allow_permanent'.
A payload that does is not normalized into the prompt, and \"always\"
stays available."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "approval.request"
     '((command . "python risky.py")
       (description . "execute_code script execution")
       (allow_permanent . nil)))
    (let ((prompt (gethash "approval:sid-prompt" hermes-chat--pending-prompts)))
      (should prompt)
      (should-not (plist-member prompt :allow-permanent))
      (should (member "always"
                      (mapcar #'cdr
                              (hermes-chat--approval-response-candidates
                               prompt)))))))

(ert-deftest hermes-chat-handles-clarify-request ()
  (let (respond-client respond-request respond-answer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (client request-id answer &optional resolve _reject)
                 (setq respond-client client
                       respond-request request-id
                       respond-answer answer)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-clarify")
           (question . "Which branch should I use?")
           (choices . ["master" "feature"])))
        (should (gethash "req-clarify" hermes-chat--pending-prompts))
        (should (string-match-p "Which branch should I use\\?"
                                (buffer-string)))
        (hermes-chat--insert-local-status "Later activity")
        (hermes-chat-respond-to-prompt "req-clarify" "feature")
        (should (eq respond-client client))
        (should (equal respond-request "req-clarify"))
        (should (equal respond-answer "feature"))
        (let* ((contents (mapcar (lambda (entry) (plist-get entry :content))
                                 (hermes-chat--entries)))
               (prompt-index (seq-position
                              contents "Which branch should I use?"))
               (response-index (seq-position
                                contents "Clarify response sent"))
               (later-index (seq-position contents "Later activity")))
          (should prompt-index)
          (should (= response-index (1+ prompt-index)))
          (should (< response-index later-index)))
        (should-not (gethash "req-clarify" hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-send-answers-pending-clarify-from-input ()
  "RET sends chat input as the pending clarification response."
  (let (respond-request respond-answer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client request-id answer &optional resolve _reject)
                 (setq respond-request request-id
                       respond-answer answer)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-input")
           (question . "Which branch should I use?")
           (choices . ["master" "feature"])))
        (insert "feature")
        (hermes-chat-send)
        (should (equal respond-request "req-input"))
        (should (equal respond-answer "feature"))
        (should-not (gethash "req-input" hermes-chat--pending-prompts))
        (should-not (hermes-test--queued-contents))
        (should (string-empty-p (hermes-chat-input-string)))))))

(ert-deftest hermes-chat-send-treats-slash-as-clarify-answer ()
  "A pending clarification owns slash-leading chat input."
  (let (respond-answer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request-id answer &optional resolve _reject)
                 (setq respond-answer answer)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-path")
           (question . "Which path should I use?")))
        (insert "/tmp/project")
        (hermes-chat-send)
        (should (equal respond-answer "/tmp/project"))))))

(ert-deftest hermes-chat-send-restores-rejected-clarify-answer ()
  "A rejected chat-tail clarification keeps its answer recoverable."
  (let (reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request-id _answer &optional _resolve reject-fn)
                 (setq reject reject-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-reject")
           (question . "Which branch should I use?")))
        (insert "feature")
        (hermes-chat-send)
        (funcall reject "clarify failed")
        (should (equal (hermes-chat-input-string) "feature"))
        (should-not (hermes-chat--prompt-response-in-flight-p
                     "req-reject"))))))

(ert-deftest hermes-chat-clarify-failures-restore-before-presentation ()
  "Clarify rejection, error, and quit restore the exact submitted tail once."
  (dolist (mode '(async-reject sync-error sync-quit))
    (ert-info ((format "failure mode: %s" mode))
      (let (reject caught input-at-presentation (presentations 0))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
                   (lambda (&rest args)
                     (pcase mode
                       ('async-reject (setq reject (car (last args))))
                       ('sync-error (error "clarify failed"))
                       ('sync-quit (signal 'quit '(p1a))))))
                  ((symbol-function 'hermes-chat--command-error)
                   (lambda (_message)
                     (cl-incf presentations)
                     (should-not (hermes-chat--prompt-response-in-flight-p
                                  "req-failure"))
                     (setq input-at-presentation (hermes-chat-input-string))
                     (when (eq mode 'async-reject)
                       (error "presentation failed")))))
          (hermes-test-with-dashboard-prompt-session (client)
            (hermes-test--emit-dashboard-prompt
             client "clarify.request"
             '((request_id . "req-failure")
               (question . "Which branch should I use?")))
            (insert "exact clarify answer")
            (condition-case err
                (hermes-chat-send)
              (quit (setq caught err)))
            (when reject
              (should-error (funcall reject "clarify failed") :type 'error))
            (should (equal (hermes-chat-input-string) "exact clarify answer"))
            (should-not (hermes-chat--prompt-response-in-flight-p
                         "req-failure"))
            (should-not hermes-chat--retained-clarify-owners)
            (if (eq mode 'sync-quit)
                (progn
                  (should (equal caught '(quit p1a)))
                  (should (zerop presentations)))
              (should (= presentations 1))
              (should (equal input-at-presentation
                             "exact clarify answer")))))))))

(ert-deftest hermes-chat-genuine-missing-clarify-survives-response-overlap ()
  "Exact backend missing evidence wins when clarification text overlaps it."
  (let (reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request _answer &optional _resolve reject-fn)
                 (setq reject reject-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-missing") (question . "Answer?")))
        (hermes-chat-respond-to-prompt "req-missing" "no pending" nil t)
        (funcall reject "no pending answer request")
        (should-not (gethash "req-missing" hermes-chat--pending-prompts))
        (should (equal (hermes-chat-input-string) "no pending"))))))

(defun hermes-test--nonclarify-failure-spec (type)
  "Return real prompt failure fixture data for TYPE."
  (pcase type
    ('approval
     '(:event "approval.request" :key "approval:sid-prompt"
       :payload ((command . "first approval"))
       :transport hermes-dashboard-transport-approval-respond))
    ('sudo
     '(:event "sudo.request" :key "req-sudo-failure"
       :payload ((request_id . "req-sudo-failure"))
       :transport hermes-dashboard-transport-sudo-respond))
    ('secret
     '(:event "secret.request" :key "req-secret-failure"
       :payload ((request_id . "req-secret-failure")
                 (prompt . "Enter secret"))
       :transport hermes-dashboard-transport-secret-respond))
    ('terminal
     '(:event "terminal.read.request" :key "req-terminal-failure"
       :payload ((request_id . "req-terminal-failure") (start . 0) (count . 1))
       :transport hermes-dashboard-transport-terminal-read-respond))))

(ert-deftest hermes-chat-nonclarify-failures-never-preserve-response ()
  "Nonclarify failures stay retryable without exposing their raw response."
  (dolist (type '(approval sudo secret terminal))
    (dolist (mode '(async-reject sync-error sync-quit inline-success-error))
      (ert-info ((format "%s %s" type mode))
        (let* ((spec (hermes-test--nonclarify-failure-spec type))
               (transport (plist-get spec :transport))
               (response (format "P1A-RAW-no pending-%s-%s" type mode))
               (error-value (if (eq type 'terminal)
                                (json-encode-string response)
                              response))
               (present (symbol-function 'hermes-chat--command-error))
               reject caught presented approval-order approval-count)
          (cl-letf (((symbol-function transport)
                     (lambda (&rest args)
                       (pcase mode
                         ('async-reject
                          (setq reject
                                (if (eq type 'approval)
                                    (plist-get (cdr args) :reject)
                                  (car (last args)))))
                         ('sync-error (error "failed response %s" error-value))
                         ('inline-success-error
                          (funcall (if (eq type 'approval)
                                       (plist-get (cdr args) :resolve)
                                     (nth 3 args))
                                   '((status . "ok")))
                          (error "failed response %s" error-value))
                         ('sync-quit (signal 'quit '(p1a))))))
                    ((symbol-function 'hermes-chat--command-error)
                     (lambda (message)
                       (setq presented t)
                       (should-not (hermes-chat--prompt-response-in-flight-p
                                    (plist-get spec :key)))
                       (funcall present message))))
            (hermes-test-with-dashboard-prompt-session (client)
              (hermes-test--emit-dashboard-prompt
               client (plist-get spec :event) (plist-get spec :payload))
              (when (eq type 'approval)
                (hermes-test--emit-dashboard-prompt
                 client "approval.request" '((command . "second approval")))
                (let ((prompt (gethash (plist-get spec :key)
                                       hermes-chat--pending-prompts)))
                  (setq approval-count (plist-get prompt :prompt-count)
                        approval-order
                        (mapcar (lambda (item) (plist-get item :command))
                                (plist-get prompt :prompt-queue)))))
              (condition-case err
                  (hermes-chat-respond-to-prompt
                   (plist-get spec :key) response nil t)
                ((error quit) (setq caught err)))
              (when reject
                (funcall reject (format "failed response %s" error-value)))
              (let ((prompt (gethash (plist-get spec :key)
                                     hermes-chat--pending-prompts)))
                (if (eq mode 'inline-success-error)
                    (if (eq type 'approval)
                        (progn
                          (should (= (plist-get prompt :prompt-count) 1))
                          (should (equal
                                   (mapcar (lambda (item)
                                             (plist-get item :command))
                                           (plist-get prompt :prompt-queue))
                                   (cdr approval-order))))
                      (should-not prompt))
                  (should prompt)
                  (should-not (plist-get prompt :response-token))
                  (should-not (string-match-p
                               (regexp-quote response) (prin1-to-string prompt)))
                  (when (eq type 'approval)
                    (should (= (plist-get prompt :prompt-count) approval-count))
                    (should (equal
                             (mapcar (lambda (item) (plist-get item :command))
                                     (plist-get prompt :prompt-queue))
                             approval-order)))))
              (cond
               ((eq mode 'sync-quit)
                (should (equal caught '(quit p1a))))
               ((eq mode 'inline-success-error)
                (should (eq (car caught) 'error))
                (should-not (string-match-p
                             (regexp-quote response)
                             (error-message-string caught)))
                (with-current-buffer (messages-buffer)
                  (should-not (string-match-p
                               (regexp-quote response) (buffer-string)))))
               (t
                (should-not caught)
                (should presented)
                (should (string-match-p "<redacted>" (buffer-string)))))
              (should (string-empty-p (hermes-chat-input-string)))
              (should-not hermes-chat--retained-clarify-owners)
              (should-not (string-match-p (regexp-quote response)
                                          (buffer-string))))))))))

(ert-deftest hermes-chat-send-rejects-second-pending-clarify-answer ()
  "A second RET cannot lose text while the first response is in flight."
  (let (requests first-resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request-id answer &optional resolve _reject)
                 (push answer requests)
                 (setq first-resolve (or first-resolve resolve)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-double")
           (question . "Which branch should I use?")))
        (insert "first")
        (hermes-chat-send)
        (insert "second")
        (should-error (hermes-chat-send) :type 'user-error)
        (should (equal requests '("first")))
        (should (equal (hermes-chat-input-string) "second"))
        (funcall first-resolve '((status . "ok")))
        (should-not (gethash "req-double" hermes-chat--pending-prompts))
        (should (equal (hermes-chat-input-string) "second"))))))

(ert-deftest hermes-chat-rejected-clarify-does-not-queue-over-new-draft ()
  "A late clarification rejection appends its answer after a new draft."
  (let (reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request-id _answer &optional _resolve reject-fn)
                 (setq reject reject-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-late")
           (question . "Which branch should I use?")))
        (insert "feature")
        (hermes-chat-send)
        (insert "new draft")
        (funcall reject "clarify failed")
        (should (equal (hermes-chat-input-string) "new draft\nfeature"))
        (should-not (hermes-test--queued-contents))))))

(ert-deftest hermes-chat-stale-clarify-rejection-ignores-reset-buffer ()
  "A clarification rejection cannot mutate a replacement chat lifecycle."
  (let (reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request-id _answer &optional _resolve reject-fn)
                 (setq reject reject-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-stale")
           (question . "Which branch should I use?")))
        (insert "feature")
        (hermes-chat-send)
        (hermes-chat--reset-transcript)
        (should-not hermes-chat--retained-clarify-owners)
        (insert "replacement draft")
        (let ((before (buffer-string)))
          (funcall reject "clarify failed")
          (should (equal (buffer-string) before)))
        (should (equal (hermes-chat-input-string) "replacement draft"))
        (should-not (hermes-test--queued-contents))))))

(ert-deftest hermes-chat-send-restores-clarify-answer-after-signal ()
  "A synchronous clarification failure restores the chat-tail answer."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
             (lambda (&rest _args) (error "clarify failed"))))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "clarify.request"
       '((request_id . "req-signal")
         (question . "Which branch should I use?")))
      (insert "feature")
      (hermes-chat-send)
      (should (equal (hermes-chat-input-string) "feature"))
      (should-not (hermes-chat--prompt-response-in-flight-p "req-signal")))))

(ert-deftest hermes-chat-handles-sudo-request ()
  (let (respond-request respond-password)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-sudo-respond)
               (lambda (_client request-id password &optional resolve _reject)
                 (setq respond-request request-id
                       respond-password password)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "sudo.request" '((request_id . "req-sudo")))
        (should (gethash "req-sudo" hermes-chat--pending-prompts))
        (should (string-match-p "Sudo password requested" (buffer-string)))
        (hermes-chat-respond-to-prompt "req-sudo" "sudo password 123")
        (should (equal respond-request "req-sudo"))
        (should (equal respond-password "sudo password 123"))
        (should-not (string-match-p "sudo password 123" (buffer-string)))
        (should-not (gethash "req-sudo" hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-handles-secret-request ()
  (let (respond-request respond-value)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-secret-respond)
               (lambda (_client request-id value &optional resolve _reject)
                 (setq respond-request request-id
                       respond-value value)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "secret.request"
         '((request_id . "req-secret")
           (prompt . "Enter API token")
           (env_var . "API_TOKEN")))
        (should (gethash "req-secret" hermes-chat--pending-prompts))
        (should (string-match-p "Enter API token" (buffer-string)))
        (should (string-match-p "API_TOKEN" (buffer-string)))
        (hermes-chat-respond-to-prompt "req-secret" "secret-token-abc")
        (should (equal respond-request "req-secret"))
        (should (equal respond-value "secret-token-abc"))
        (should-not (string-match-p "secret-token-abc" (buffer-string)))
        (should-not (gethash "req-secret" hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-expires-secret-request-without-clearing-newer-one ()
  "A secret expiry removes only its exact pending request."
  (let ((hermes-chat-auto-prompt-requests nil))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-expired") (prompt . "Old secret")))
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-current") (prompt . "Current secret")))
      (puthash "req-expired" t (hermes-chat--ensure-auto-prompt-keys))
      (hermes-test--emit-dashboard-prompt
       client "secret.expire" '((request_id . "req-expired")))
      (should-not (gethash "req-expired" hermes-chat--pending-prompts))
      (should-not (gethash "req-expired" hermes-chat--auto-prompt-keys))
      (should (gethash "req-current" hermes-chat--pending-prompts))
      (hermes-test--emit-dashboard-prompt
       client "secret.expire" '((request_id . "req-expired")))
      (should (gethash "req-current" hermes-chat--pending-prompts))
      (let ((header (hermes-test--header-line-string)))
        (should (string-match-p "Secret requested" header))
        (should (string-match-p "Current secret" header))
        (should-not (string-match-p "expired" header)))
      (should (string-match-p "Secret request expired" (buffer-string))))))

(ert-deftest hermes-chat-does-not-claim-expired-secret-response-succeeded ()
  "An expired response result must not render a false success."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-secret-respond)
             (lambda (_client _request-id _value &optional resolve _reject)
               (funcall resolve '((status . "expired"))))))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-expired-result") (prompt . "Enter secret")))
      (hermes-chat-respond-to-prompt "req-expired-result" "secret-value")
      (should-not (gethash "req-expired-result" hermes-chat--pending-prompts))
      (should (string-match-p "Secret request no longer pending"
                              (buffer-string)))
      (should-not (string-match-p "Secret response sent" (buffer-string)))
      (should-not (string-match-p "secret-value" (buffer-string))))))

(ert-deftest hermes-chat-does-not-send-secret-expired-while-reading ()
  "Expiry during minibuffer input invalidates the captured prompt."
  (let (sent)
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-read-expire") (prompt . "Enter secret")))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _)
                   (hermes-test--emit-dashboard-prompt
                    client "secret.expire"
                    '((request_id . "req-read-expire")))
                   "secret-value"))
                ((symbol-function 'hermes-dashboard-transport-secret-respond)
                 (lambda (&rest _args) (setq sent t))))
        (should-error
         (hermes-chat-respond-to-prompt "req-read-expire")
         :type 'user-error))
      (should-not sent)
      (should-not (gethash "req-read-expire" hermes-chat--pending-prompts))
      (should-not (string-match-p "secret-value" (buffer-string))))))

(ert-deftest hermes-chat-secret-read-rejects-owner-loss ()
  "A secret read cannot cross disconnect or resurrect a stale client."
  (dolist (mode '(disconnect stale-client))
    (let (acquired sent)
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "secret.request"
         '((request_id . "req-owner-loss") (prompt . "Enter secret")))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _args)
                     (if (eq mode 'disconnect)
                         (hermes-chat-disconnect)
                       (setf (hermes-dashboard-transport-client-websocket
                              client) nil))
                     "secret-value"))
                  ((symbol-function 'hermes-dashboard-transport-acquire)
                   (lambda (&rest _args)
                     (setq acquired t)
                     (hermes-test--dashboard-client)))
                  ((symbol-function 'hermes-dashboard-transport-secret-respond)
                   (lambda (&rest _args) (setq sent t))))
          (should-error (hermes-chat-respond-to-prompt "req-owner-loss")
                        :type 'user-error))
        (should-not acquired)
        (should-not sent)
        (should-not (string-match-p "secret-value" (buffer-string)))))))

(ert-deftest hermes-chat-prompt-disconnect-releases-response-claim ()
  "Disconnect keeps an in-flight prompt recoverable by a successor owner."
  (let (resolves (sent 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (cl-incf sent)
                 (push (plist-get args :resolve) resolves))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "first")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (should (hermes-chat--prompt-response-in-flight-p
                 "approval:sid-prompt"))
        (hermes-chat-disconnect)
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))
          (should prompt)
          (should-not (plist-get prompt :response-token)))
        (funcall (car resolves) '((resolved . 1)))
        (should (gethash "approval:sid-prompt" hermes-chat--pending-prompts))
        (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
              hermes-chat--dashboard-active-session-id "sid-prompt")
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "deny")
        (should (= sent 2))))))

(ert-deftest hermes-chat-auto-prompt-defers-behind-in-flight-response ()
  "A successor timer remains recoverable while its predecessor is in flight."
  (hermes-test-with-auto-prompt-session (client timer-calls prompted)
    (let (reject-first (sent 0))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (cl-incf sent)
                 (if (= sent 1)
                     (setq reject-first (plist-get args :reject))
                   (funcall (plist-get args :resolve) '((resolved . 1)))))))
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "first")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (setq timer-calls nil)
        (hermes-test--emit-dashboard-prompt
         client "approval.request" '((command . "second")))
        (let ((first-call (hermes-test--last-auto-prompt-call timer-calls)))
          (apply (car first-call) (cdr first-call))
          (let ((deferred-call (hermes-test--last-auto-prompt-call timer-calls)))
            (should-not (eq deferred-call first-call))
            (funcall reject-first "transport failure")
            (let ((fresh-call (hermes-test--last-auto-prompt-call timer-calls)))
              (should-not (eq fresh-call deferred-call))
              (apply (car deferred-call) (cdr deferred-call))
              (should (zerop prompted))
              (apply (car fresh-call) (cdr fresh-call)))))
        (should (= prompted 1))
        (should (= sent 2))))))

(ert-deftest hermes-chat-handles-terminal-read-request ()
  (let (respond-request respond-text)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-terminal-read-respond)
               (lambda (_client request-id text &optional resolve _reject)
                 (setq respond-request request-id
                       respond-text text)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "terminal.read.request"
         '((request_id . "req-tr")
           (start . 0)
           (count . 10)))
        (should (gethash "req-tr" hermes-chat--pending-prompts))
        (should (string-match-p "Terminal read" (buffer-string)))
        (hermes-chat-respond-to-prompt "req-tr")
        (should (equal respond-request "req-tr"))
        (let ((snapshot (json-read-from-string respond-text)))
          (should (equal (alist-get 'start snapshot) 0))
          (should (<= (alist-get 'end snapshot) 10))
          (should (string-match-p "trigger prompt"
                                  (alist-get 'text snapshot))))
        (should-not (gethash "req-tr" hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-redacts-secret-response ()
  (cl-letf (((symbol-function 'hermes-dashboard-transport-secret-respond)
             (lambda (_client _request-id value &optional _resolve reject)
               (funcall reject (format "rejected value %s" value)))))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-secret")
         (prompt . "Enter API token")
         (env_var . "API_TOKEN")))
      (hermes-chat-respond-to-prompt "req-secret" "secret-token-abc")
      (should (gethash "req-secret" hermes-chat--pending-prompts))
      (should (string-match-p "<redacted>" (buffer-string)))
      (should-not (string-match-p "secret-token-abc" (buffer-string))))))

(ert-deftest hermes-chat-cancels-clarify-request ()
  (let (respond-request respond-answer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client request-id answer &optional resolve _reject)
                 (setq respond-request request-id
                       respond-answer answer)
                 (funcall resolve '((status . "ok"))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-cancel")
           (question . "Continue?")))
        (hermes-chat-cancel-prompt "req-cancel")
        (should (equal respond-request "req-cancel"))
        (should (equal respond-answer ""))
        (should-not (gethash "req-cancel" hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-keeps-approval-requests-fifo ()
  (let (choices)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (push (plist-get args :choice) choices)
                 (funcall (plist-get args :resolve)
                          '((resolved . 1))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm first")
           (description . "first approval")))
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm second")
           (description . "second approval")))
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))
          (should (equal (plist-get prompt :prompt-count) 2))
          (should (string-match-p "first approval" (plist-get prompt :content)))
          (should-not (string-match-p "second approval"
                                      (plist-get prompt :content))))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))
          (should prompt)
          (should (equal (plist-get prompt :prompt-count) 1))
          (should (string-match-p "second approval" (plist-get prompt :content))))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "deny")
        (should (equal (nreverse choices) '("once" "deny")))
        (should-not (gethash "approval:sid-prompt"
                             hermes-chat--pending-prompts))))))

(ert-deftest hermes-chat-keeps-new-approval-while-response-pending ()
  (let (resolve-first)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (setq resolve-first (plist-get args :resolve)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm first")
           (description . "first approval")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm second")
           (description . "second approval")))
        (funcall resolve-first '((resolved . 1)))
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts))
              (header (hermes-test--header-line-string)))
          (should prompt)
          (should (equal (plist-get prompt :prompt-count) 1))
          (should (string-match-p "second approval"
                                  (plist-get prompt :content)))
          (should (string-match-p "Approval requested" header))
          (should (string-match-p "second approval" header))
          (should-not (string-match-p "Approval response sent" header)))))))

(ert-deftest hermes-chat-keeps-new-approval-when-all-response-resolves-one ()
  (let (resolve-first)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (setq resolve-first (plist-get args :resolve)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm first")
           (description . "first approval")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once" t)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm second")
           (description . "second approval")))
        (funcall resolve-first '((resolved . 1)))
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts))
              (header (hermes-test--header-line-string)))
          (should prompt)
          (should (equal (plist-get prompt :prompt-count) 1))
          (should (string-match-p "second approval"
                                  (plist-get prompt :content)))
          (should (string-match-p "Approval requested" header))
          (should (string-match-p "second approval" header))
          (should-not (string-match-p "Approval response sent" header)))))))

(ert-deftest hermes-chat-treats-unresolved-approval-response-as-stale ()
  (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve) '((resolved . 0))))))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "approval.request"
       '((command . "rm stale")
         (description . "stale approval")))
      (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
      (should-not (gethash "approval:sid-prompt" hermes-chat--pending-prompts))
      (should (string-match-p "Approval request no longer pending"
                              (buffer-string)))
      (should-not (string-match-p "Approval response sent" (buffer-string)))
      (should (string-match-p "Approval request no longer pending"
                              (hermes-test--header-line-string))))))

(ert-deftest hermes-chat-stale-approval-response-keeps-new-request ()
  (let (resolve-first)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (setq resolve-first (plist-get args :resolve)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm first")
           (description . "first approval")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm second")
           (description . "second approval")))
        (funcall resolve-first '((resolved . 0)))
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))
          (should prompt)
          (should (equal (plist-get prompt :prompt-count) 1))
          (should (string-match-p "second approval"
                                  (plist-get prompt :content)))
          (should-not (plist-get prompt :response-token)))))))

(ert-deftest hermes-chat-missing-approval-rejection-keeps-new-request ()
  (let (reject-first)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-approval-respond)
               (lambda (_client &rest args)
                 (setq reject-first (plist-get args :reject)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm first")
           (description . "first approval")))
        (hermes-chat-respond-to-prompt "approval:sid-prompt" "once")
        (hermes-test--emit-dashboard-prompt
         client "approval.request"
         '((command . "rm second")
           (description . "second approval")))
        (funcall reject-first "no pending approval")
        (let ((prompt (gethash "approval:sid-prompt"
                               hermes-chat--pending-prompts)))
          (should prompt)
          (should (equal (plist-get prompt :prompt-count) 1))
          (should (string-match-p "second approval"
                                  (plist-get prompt :content)))
          (should-not (plist-get prompt :response-token)))))))

(ert-deftest hermes-chat-clears-prompt-request-on-terminal-event ()
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "secret.request"
     '((request_id . "req-timeout")
       (prompt . "Enter API token")
       (env_var . "API_TOKEN")))
    (should (gethash "req-timeout" hermes-chat--pending-prompts))
    (hermes-dashboard-transport--dispatch-event client
             '(:type done :session-id "sid-prompt"))
    (should-not (gethash "req-timeout" hermes-chat--pending-prompts))))

(ert-deftest hermes-chat-redacts-synchronous-secret-response-error ()
  (cl-letf (((symbol-function 'hermes-dashboard-transport-secret-respond)
             (lambda (_client _request-id value &optional _resolve _reject)
               (error "encoded frame contained %s" value))))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "secret.request"
       '((request_id . "req-secret")
         (prompt . "Enter API token")
         (env_var . "API_TOKEN")))
      (hermes-chat-respond-to-prompt "req-secret" "secret-token-abc")
      (should (gethash "req-secret" hermes-chat--pending-prompts))
      (should (string-match-p "<redacted>" (buffer-string)))
      (should-not (string-match-p "secret-token-abc" (buffer-string))))))

(ert-deftest hermes-chat-redacts-encoded-secret-response-error ()
  (let* ((secret "secret token with \\\"quotes\\\" and newline\nnext")
         (encoded-secret (json-encode-string secret)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-secret-respond)
               (lambda (_client _request-id value &optional _resolve _reject)
                 (error "encoded frame contained %s"
                        (json-encode-string value)))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "secret.request"
         '((request_id . "req-secret")
           (prompt . "Enter API token")
           (env_var . "API_TOKEN")))
        (hermes-chat-respond-to-prompt "req-secret" secret)
        (should (string-match-p "<redacted>" (buffer-string)))
        (should-not (string-match-p (regexp-quote secret) (buffer-string)))
        (should-not (string-match-p (regexp-quote encoded-secret)
                                    (buffer-string)))))))

(defun hermes-test--record-local-clarify (request question)
  "Record a normalized local clarification for REQUEST and QUESTION."
  (hermes-chat--record-prompt-request
   (hermes-dashboard-transport--prompt-request-event
    "clarify.request" '((session_id . "sid-prompt"))
    `((request_id . ,request) (question . ,question)))
   nil))

(ert-deftest hermes-chat-p1b-equivalent-replay-keeps-clarify-owner ()
  "Only an equivalent nonapproval replay inherits retained response authority."
  (let (resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request _answer &optional resolve-fn _reject)
                 (setq resolve resolve-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-replay") (question . "Branch?")))
        (hermes-chat-respond-to-prompt "req-replay" "feature" nil t)
        (let* ((owner (car hermes-chat--retained-clarify-owners))
               (token (plist-get owner :response-token)))
          (should (eq (plist-get owner :buffer) (current-buffer)))
          (should (eql (plist-get owner :generation)
                       hermes-chat--lifecycle-generation))
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           '((request_id . "req-replay") (question . "Branch?")))
          (should (eq token (plist-get (gethash "req-replay"
                                                hermes-chat--pending-prompts)
                                       :response-token)))
          (should (eq owner (car hermes-chat--retained-clarify-owners)))
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           '((request_id . "req-replay") (question . "Changed branch?")))
          (should-not (plist-get (gethash "req-replay"
                                         hermes-chat--pending-prompts)
                                 :response-token))
          (funcall resolve '((status . "ok")))
          (should (gethash "req-replay" hermes-chat--pending-prompts))
          (should (eq owner (car hermes-chat--retained-clarify-owners))))))))

(ert-deftest hermes-chat-p1b-approval-never-inherits-nonapproval-token ()
  "A same-key approval cannot inherit a nonapproval response claim."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
             (lambda (&rest _args) 'pending)))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "clarify.request"
       '((request_id . "shared-key") (question . "Branch?")))
      (hermes-chat-respond-to-prompt "shared-key" "feature" nil t)
      (hermes-chat--record-prompt-request
       '(:prompt-type "approval" :request-id "shared-key"
         :session-id "sid-prompt" :content "Approve?") nil)
      (let ((prompt (gethash "shared-key" hermes-chat--pending-prompts)))
        (should (hermes-chat--approval-prompt-p prompt))
        (should-not (plist-get prompt :response-token))
        (should (= (length hermes-chat--retained-clarify-owners) 1))))))

(ert-deftest hermes-chat-p1b-ordinary-clarify-owners-settle-exactly ()
  "Concurrent duplicate answers retain FIFO occurrences and settle by identity."
  (let ((callbacks (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client request _answer &optional resolve reject)
                 (puthash request (cons resolve reject) callbacks))))
      (hermes-test-with-dashboard-prompt-session (client)
        (dolist (request '("req-one" "req-two" "req-three"))
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           `((request_id . ,request) (question . "Same?")))
          (hermes-chat-respond-to-prompt request "duplicate" nil t))
        (let ((owners (copy-sequence hermes-chat--retained-clarify-owners)))
          (should (equal (mapcar (lambda (owner) (plist-get owner :text)) owners)
                         '("duplicate" "duplicate" "duplicate")))
          (should-not (eq (plist-get (car owners) :text) "duplicate"))
          (funcall (car (gethash "req-one" callbacks)) '((status . "ok")))
          (should (equal hermes-chat--retained-clarify-owners (cdr owners)))
          (funcall (cdr (gethash "req-two" callbacks)) "clarify failed")
          (should (equal hermes-chat--retained-clarify-owners (cddr owners)))
          (should (equal (hermes-chat-input-string) "duplicate"))
          (funcall (car (gethash "req-three" callbacks)) '((status . "ok")))
          (should-not hermes-chat--retained-clarify-owners))))))

(ert-deftest hermes-chat-p1b-lifecycle-keeps-current-reentry-owner ()
  "Invalidation drops old owners without dropping a hook-created replacement."
  (dolist (action '(invalidate disconnect mode))
    (ert-info ((format "lifecycle action: %s" action))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
                 (lambda (&rest _args) 'pending)))
        (hermes-test-with-dashboard-prompt-session (client)
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           '((request_id . "req-old") (question . "Old?")))
          (hermes-chat-respond-to-prompt "req-old" "old" nil t)
          (unless (eq action 'mode)
            (add-hook
             'hermes-chat-lifecycle-invalidation-hook
             (lambda ()
               (unless (gethash "req-current" hermes-chat--pending-prompts)
                 (hermes-test--record-local-clarify "req-current" "Current?")
                 (hermes-chat-respond-to-prompt
                  "req-current" "current" nil t)))
             nil t))
          (pcase action
            ('invalidate (hermes-chat--invalidate-transport-state))
            ('disconnect (hermes-chat-disconnect))
            ('mode (fundamental-mode)))
          (if (eq action 'mode)
              (should-not hermes-chat--retained-clarify-owners)
            (should (equal
                     (mapcar (lambda (owner) (plist-get owner :text))
                             hermes-chat--retained-clarify-owners)
                     '("current")))))))))

(defun hermes-test--p1b-reset-clarify (mode)
  "Exercise reset-hook clarification settlement MODE."
  (let (resolve reject holder caught doomed-input nested ran)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request _answer &optional resolve-fn reject-fn)
                 (setq resolve resolve-fn reject reject-fn)
                 (pcase mode
                   ('sync-error (error "clarify failed"))
                   ('sync-quit (signal 'quit '(p1b)))
                   ('inline-reject (funcall reject-fn "clarify failed"))
                   ('inline-success (funcall resolve-fn '((status . "ok"))))))))
      (hermes-test-with-dashboard-prompt-session (client)
        (add-hook
         'hermes-chat-lifecycle-invalidation-hook
         (lambda ()
           (when (and (eq mode 'nested) (not nested))
             (setq nested t)
             (hermes-chat--reset-transcript))
           (unless ran
             (setq ran t
                   holder hermes-chat--reset-clarify-owner-sink)
             (hermes-test--record-local-clarify "req-reset" "Reset answer?")
             (insert "reset answer")
             (condition-case err
                 (hermes-chat-send)
               (quit (setq caught err)))
             (when (eq mode 'async-reject)
               (funcall reject "clarify failed"))
             (setq doomed-input (hermes-chat-input-string))))
         nil t)
        (hermes-chat--reset-transcript)
        (should (equal doomed-input ""))
        (should (equal caught (and (eq mode 'sync-quit) '(quit p1b))))
        (should (equal (hermes-chat-input-string)
                       (if (eq mode 'inline-success) "" "reset answer")))
        (should-not hermes-chat--retained-clarify-owners)
        (should-not (cadr holder))
        (should-not hermes-chat--reset-clarify-owner-sink)
        (should (zerop (hash-table-count hermes-chat--pending-prompts)))
        (should-not hermes-chat--queued-messages)
        (let ((before (buffer-string)))
          (when resolve (funcall resolve '((status . "ok"))))
          (when reject (funcall reject "late rejection"))
          (should (equal (buffer-string) before)))))))

(ert-deftest hermes-chat-p1b-reset-hook-settles-clarify-owners ()
  "Reset recovers failed or pending local answers and drops exact successes."
  (dolist (mode '(pending sync-error sync-quit inline-reject
                          async-reject inline-success nested))
    (ert-info ((format "reset settlement: %s" mode))
      (hermes-test--p1b-reset-clarify mode))))

(ert-deftest hermes-chat-p1b-reset-sink-rejects-foreign-buffer-owner ()
  "Reset cannot capture a clarification accepted in another chat buffer."
  (let (foreign-hook other reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (_client _request _answer &optional _resolve reject-fn)
                 (setq reject reject-fn))))
      (hermes-test-with-dashboard-prompt-session (client)
        (setq other (generate-new-buffer " *Hermes foreign clarify*"))
        (unwind-protect
            (progn
              (with-current-buffer other
                (hermes-chat-mode)
                (setq hermes-chat--dashboard-client client
                      hermes-chat--dashboard-active-session-id "sid-prompt"))
              (setq foreign-hook
                    (lambda ()
                      (with-current-buffer other
                        (hermes-test--record-local-clarify "req-other" "Other?")
                        (hermes-chat-respond-to-prompt
                         "req-other" "other answer" nil t))))
              (add-hook 'hermes-chat-lifecycle-invalidation-hook
                        foreign-hook nil t)
              (hermes-chat--reset-transcript)
              (should (string-empty-p (hermes-chat-input-string)))
              (with-current-buffer other
                (should (equal (mapcar (lambda (owner) (plist-get owner :text))
                                       hermes-chat--retained-clarify-owners)
                               '("other answer")))
                (funcall reject "clarify failed")
                (should (equal (hermes-chat-input-string) "other answer"))
                (should-not hermes-chat--retained-clarify-owners)))
          (remove-hook 'hermes-chat-lifecycle-invalidation-hook foreign-hook t)
          (when (buffer-live-p other) (kill-buffer other)))))))

(ert-deftest hermes-chat-p1b-reset-preserves-duplicate-clarify-order ()
  "Reset drains duplicate clarification occurrences oldest first."
  (let (accepted holder ran)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
               (lambda (&rest _args) 'pending)))
      (hermes-test-with-dashboard-prompt-session (client)
        (add-hook
         'hermes-chat-lifecycle-invalidation-hook
         (lambda ()
           (unless ran
             (setq ran t
                   holder hermes-chat--reset-clarify-owner-sink)
             (dolist (request '("req-one" "req-two"))
               (hermes-test--record-local-clarify request "Same?")
               (hermes-chat-respond-to-prompt request "same" nil t))
             (setq accepted (copy-sequence (cadr holder)))))
         nil t)
        (hermes-chat--reset-transcript)
        (should (equal
                 (mapcar (lambda (owner)
                           (car (plist-get owner :response-token)))
                         accepted)
                 '("req-one" "req-two")))
        (should (equal (mapcar (lambda (owner) (plist-get owner :text)) accepted)
                       '("same" "same")))
        (should (equal (hermes-chat-input-string) "same\nsame"))
        (should-not (cadr holder))
        (should-not hermes-chat--retained-clarify-owners)))))

(ert-deftest hermes-chat-p2-takes-equivalent-clarify-claim-dormantly ()
  "Terminal take follows an exact token through replay and restores once."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
             (lambda (&rest _args) 'pending)))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "clarify.request"
       '((request_id . "req-terminal") (question . "Branch?")))
      (insert "feature")
      (hermes-chat-send)
      (let* ((snapshot (hermes-chat--capture-terminal-prompts))
             (owner (car hermes-chat--retained-clarify-owners))
             (token (plist-get owner :response-token)))
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         '((request_id . "req-terminal") (question . "Branch?")))
        (should (eq token (plist-get (gethash "req-terminal"
                                              hermes-chat--pending-prompts)
                                     :response-token)))
        (let ((effects (hermes-chat--take-terminal-prompts snapshot)))
          (should (= (length effects) 1))
          (should (string-empty-p (hermes-chat-input-string)))
          (should-not (gethash "req-terminal" hermes-chat--pending-prompts))
          (should-not hermes-chat--retained-clarify-owners)
          (funcall (car effects))
          (should (equal (hermes-chat-input-string) "feature"))
          (funcall (car effects))
          (should (equal (hermes-chat-input-string) "feature")))))))

(ert-deftest hermes-chat-p2-stale-take-is-total-no-op-for-prompt-matrix ()
  "Invalidation makes claimed and unclaimed prompt authority wholly stale."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
             (lambda (&rest _args) 'pending))
            ((symbol-function 'hermes-dashboard-transport-approval-respond)
             (lambda (&rest _args) 'pending)))
    (hermes-test-with-dashboard-prompt-session (client)
      (hermes-test--emit-dashboard-prompt
       client "clarify.request"
       '((request_id . "claimed-clarify") (question . "Branch?")))
      (hermes-chat-respond-to-prompt "claimed-clarify" "feature" nil t)
      (dolist (prompt '((:prompt-type "sudo" :request-id "open-sudo")
                        (:prompt-type "approval" :request-id "claimed-approval"
                         :session-id nil :content "Claimed?")
                        (:prompt-type "approval" :request-id "open-approval"
                         :session-id "other" :content "Open?")))
        (hermes-chat--record-prompt-request prompt nil))
      (hermes-chat-respond-to-prompt "claimed-approval" "once")
      (dolist (key '("claimed-clarify" "open-sudo"
                     "claimed-approval" "open-approval"))
        (puthash key (list key) (hermes-chat--ensure-auto-prompt-keys)))
      (let ((snapshot (hermes-chat--capture-terminal-prompts)))
        (hermes-chat--invalidate-transport-state)
        (let ((prompts (mapcar (lambda (key)
                                (cons key (gethash key hermes-chat--pending-prompts)))
                              '("claimed-clarify" "open-sudo"
                                "claimed-approval" "open-approval")))
              (owners (copy-sequence hermes-chat--retained-clarify-owners))
              (auto-count (hash-table-count hermes-chat--auto-prompt-keys)))
          (should-not (hermes-chat--take-terminal-prompts snapshot))
          (dolist (entry prompts)
            (should (eq (cdr entry)
                        (gethash (car entry) hermes-chat--pending-prompts))))
          (should (equal owners hermes-chat--retained-clarify-owners))
          (should (= auto-count
                     (hash-table-count hermes-chat--auto-prompt-keys))))))))

(ert-deftest hermes-chat-p2-effects-consume-on-stale-lifecycle ()
  "Taken restoration effects cannot cross invalidation, kill, mode, or reset."
  (dolist (action '(invalidate kill mode reset))
    (ert-info ((format "effect action: %s" action))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
                 (lambda (&rest _args) 'pending)))
        (hermes-test-with-dashboard-prompt-session (client)
          (hermes-test--emit-dashboard-prompt
           client "clarify.request"
           '((request_id . "req-effect") (question . "Branch?")))
          (insert "stale answer")
          (hermes-chat-send)
          (let* ((snapshot (hermes-chat--capture-terminal-prompts))
                 (effect (car (hermes-chat--take-terminal-prompts snapshot))))
            (should effect)
            (pcase action
              ('invalidate (hermes-chat--invalidate-transport-state))
              ('mode (fundamental-mode))
              ('reset (hermes-chat--reset-transcript))
              ('kill (kill-buffer buffer)))
            (if (eq action 'kill)
                (let ((successor (generate-new-buffer " *Hermes successor*")))
                  (unwind-protect
                      (with-current-buffer successor
                        (hermes-chat-mode)
                        (setq hermes-chat--lifecycle-generation
                              (plist-get snapshot :generation))
                        (funcall effect)
                        (should (string-empty-p (hermes-chat-input-string))))
                    (kill-buffer successor)))
              (let ((before (buffer-string)))
                (funcall effect)
                (funcall effect)
                (should (equal (buffer-string) before))))))))))

(ert-deftest hermes-chat-p2-approval-authority-is-session-tagged ()
  "Approval take handles nil sessions and preserves different-session queues."
  (hermes-test-with-chat-buffer
    (let ((key "approval-key"))
      (dolist (content '("nil-one" "nil-two"))
        (hermes-chat--record-prompt-request
         `(:prompt-type "approval" :request-id ,key
           :session-id nil :content ,content) nil))
      (let* ((snapshot (hermes-chat--capture-terminal-prompts))
             (entry (car (plist-get snapshot :entries))))
        (should (plist-get entry :approval-p))
        (should (plist-member entry :session-id))
        (should-not (plist-get entry :session-id))
        (hermes-chat--record-prompt-request
         `(:prompt-type "approval" :request-id ,key
           :session-id nil :content "nil-three") nil)
        (puthash key (list key (gethash key hermes-chat--pending-prompts))
                 (hermes-chat--ensure-auto-prompt-keys))
        (should-not (hermes-chat--take-terminal-prompts snapshot))
        (should-not (gethash key hermes-chat--pending-prompts))
        (should-not (gethash key hermes-chat--auto-prompt-keys)))
      (hermes-chat--record-prompt-request
       `(:prompt-type "approval" :request-id ,key
         :session-id "same" :content "captured") nil)
      (let ((snapshot (hermes-chat--capture-terminal-prompts)))
        (hermes-chat--clear-pending-prompts "same")
        (let* ((successor
                (hermes-chat--record-prompt-request
                 `(:prompt-type "approval" :request-id ,key
                   :session-id "same" :content "successor") nil))
               (claim (list key successor)))
          (puthash key claim (hermes-chat--ensure-auto-prompt-keys))
          (hermes-chat--take-terminal-prompts snapshot)
          (should (eq successor (gethash key hermes-chat--pending-prompts)))
          (should (eq claim (gethash key hermes-chat--auto-prompt-keys)))
          (hermes-chat--clear-pending-prompts "same")))
      (hermes-chat--record-prompt-request
       `(:prompt-type "approval" :request-id ,key
         :session-id "old" :content "old") nil)
      (let ((snapshot (hermes-chat--capture-terminal-prompts)))
        (dolist (content '("new-one" "new-one" "new-two"))
          (hermes-chat--record-prompt-request
           `(:prompt-type "approval" :request-id ,key
             :session-id "new" :content ,content) nil))
        (let* ((prompt (gethash key hermes-chat--pending-prompts))
               (new-owner (cadr (plist-get prompt :prompt-queue)))
               (claim (list key new-owner)))
          (puthash key claim (hermes-chat--ensure-auto-prompt-keys))
          (should-not (hermes-chat--take-terminal-prompts snapshot))
          (let ((survivor (gethash key hermes-chat--pending-prompts)))
            (should (= (plist-get survivor :prompt-count) 3))
            (should (equal (mapcar (lambda (item) (plist-get item :content))
                                   (plist-get survivor :prompt-queue))
                           '("new-one" "new-one" "new-two")))
            (should (eq (gethash key hermes-chat--auto-prompt-keys) claim))))))))

(ert-deftest hermes-chat-p2-retained-effects-preserve-order-and-successors ()
  "Take preserves duplicate effect order and does not claim replacements."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-clarify-respond)
             (lambda (&rest _args) 'pending)))
    (hermes-test-with-dashboard-prompt-session (client)
      (dolist (spec '(("req-one" "same") ("req-two" "same")
                      ("req-three" "third")))
        (hermes-test--emit-dashboard-prompt
         client "clarify.request"
         `((request_id . ,(car spec)) (question . "Answer?")))
        (hermes-chat-respond-to-prompt (car spec) (cadr spec) nil t))
      (let* ((snapshot (hermes-chat--capture-terminal-prompts))
             (effects (hermes-chat--take-terminal-prompts snapshot)))
        (should (= (length effects) 3))
        (should-not (hermes-chat--take-terminal-prompts snapshot))
        (mapc #'funcall effects)
        (should (equal (hermes-chat-input-string) "same\nsame\nthird")))
      (hermes-chat--record-prompt-request
       '(:prompt-type "sudo" :request-id "successor") nil)
      (hermes-chat--record-prompt-request
       '(:prompt-type "secret" :request-id "unclaimed") nil)
      (let ((claimed (gethash "successor" hermes-chat--pending-prompts)))
        (puthash "successor"
                 (plist-put (copy-sequence claimed)
                            :response-token '(captured token))
                 hermes-chat--pending-prompts))
      (let* ((snapshot (hermes-chat--capture-terminal-prompts))
             (old-table hermes-chat--pending-prompts)
             (old-auto hermes-chat--auto-prompt-keys)
             (successor '(:prompt-type "sudo" :request-id "successor"
                          :content "new" :response-token (new token)))
             (new-table (make-hash-table :test #'equal))
             (new-auto (make-hash-table :test #'equal))
             (claim (list "successor" successor)))
        (puthash "successor" successor new-table)
        (puthash "successor" claim new-auto)
        (setq hermes-chat--pending-prompts new-table
              hermes-chat--auto-prompt-keys new-auto)
        (should-not (hermes-chat--take-terminal-prompts snapshot))
        (should (eq (gethash "successor" new-table) successor))
        (should (eq (gethash "successor" new-auto) claim))
        (setq hermes-chat--pending-prompts old-table
              hermes-chat--auto-prompt-keys old-auto)
        (let* ((old (gethash "successor" old-table))
               (different (plist-put (copy-sequence old)
                                     :response-token '(different token)))
               (unclaimed (copy-sequence (gethash "unclaimed" old-table))))
          (puthash "successor" different old-table)
          (puthash "unclaimed" unclaimed old-table)
          (should-not (hermes-chat--take-terminal-prompts snapshot))
          (should (eq (gethash "successor" old-table) different))
          (should (eq (gethash "unclaimed" old-table) unclaimed)))))))

(provide 'hermes-chat-prompts-tests)
;;; hermes-chat-prompts-tests.el ends here
