;;; hermes-chat-prompts-tests.el --- prompt flow tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-prompts': approval/clarify/sudo/secret/terminal
;; prompt requests, auto-prompting, FIFO approval queueing, response
;; dispatch, and secret redaction in responses and errors.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

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

(provide 'hermes-chat-prompts-tests)
;;; hermes-chat-prompts-tests.el ends here
