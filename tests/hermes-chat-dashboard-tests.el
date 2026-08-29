;;; hermes-chat-dashboard-tests.el --- shared-socket isolation tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-dashboard' shared-socket runtime isolation: two
;; chat buffers on one dashboard client must keep their session identity,
;; subscriber tokens, create-time overrides, and reconnect resumes strictly
;; buffer-local.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

;;; Shared-socket runtime isolation

(defun hermes-test--resolve-new-dashboard-session (_client &rest args)
  "Resolve ARGS as a fresh dashboard session."
  (funcall (plist-get args :resolve) '((session_id . "sid-new"))))

(defun hermes-test--confirming-config-set (record)
  "Return a `config.set' stub that calls RECORD and requires confirmation."
  (lambda (_client key value &rest args)
    (funcall record key value args)
    (funcall (plist-get args :resolve)
             (cond
              ((not (equal key "model")) `((key . ,key)))
              ((plist-get args :confirm-expensive-model) '((key . "model")))
              (t '((confirm_required . t) (confirm_message . "Expensive model")))))))

(ert-deftest hermes-chat-dashboard-acquires-client-for-buffer-instance ()
  "Each chat acquires its dashboard client under its pinned instance URL."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        urls buffers)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _)
                 (push hermes-dashboard-transport-url urls)
                 (make-hermes-dashboard-transport-client))))
      (unwind-protect
          (dolist (instance '(("local" . "http://127.0.0.1:9119")
                              ("remote" . "https://hermes.example.test")))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance instance)
                (hermes-chat--dashboard-ensure-client))))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              buffers)))
    (should (equal (nreverse urls)
                   '("http://127.0.0.1:9119"
                     "https://hermes.example.test")))))

(ert-deftest hermes-chat-dashboard-parses-vanilla-goal-status ()
  "Vanilla `/goal status' output becomes compact header state."
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "⊙ Goal (active, 3/20 turns): Ship it")
          '(:goal (:status "active" :running t :turns-used 3 :max-turns 20))))
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "⏳ Goal (parked 8s — timer, 3/20 turns): Ship it")
          '(:goal (:status "inactive" :running nil :turns-used 3 :max-turns 20))))
  (should
   (equal (hermes-chat--dashboard-goal-status-projection
           "No active goal. Set one with /goal <text>.")
          '(:goal nil)))
  (should-not
   (hermes-chat--dashboard-goal-status-projection "Unrecognized response")))

(ert-deftest hermes-chat-dashboard-refreshes-goal-from-vanilla-hermes ()
  "Goal refresh queries the owning vanilla session and updates its header state."
  (let ((client (hermes-test--dashboard-client)) dispatch)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch (list name arg args)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-live")
       (hermes-chat--dashboard-refresh-goal)
       (should (equal (car dispatch) "goal"))
       (should (equal (cadr dispatch) "status"))
       (should (equal (plist-get (caddr dispatch) :session-id) "sid-live"))
       (funcall (plist-get (caddr dispatch) :resolve)
                '((type . "exec")
                  (output . "⊙ Goal (active, 4/20 turns): Ship it")))
       (should (equal hermes-chat--goal
                      '(:status "active" :running t
                        :turns-used 4 :max-turns 20)))))))

(ert-deftest hermes-chat-dashboard-ignores-stale-goal-refresh ()
  "A goal status response cannot update a successor session."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client _name _arg &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--goal '(:running nil))
       (hermes-chat--dashboard-refresh-goal)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve
                '((type . "exec")
                  (output . "⊙ Goal (active, 4/20 turns): Ship it")))
       (should (equal hermes-chat--goal '(:running nil)))))))

(ert-deftest hermes-chat-dashboard-record-session-refreshes-vanilla-goal ()
  "Attaching a session requests vanilla Hermes goal state once."
  (let ((client (hermes-test--dashboard-client)) (refreshes 0))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal)
               (lambda () (setq refreshes (1+ refreshes)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "sid-live")))
       (should (= refreshes 1))))))

(ert-deftest hermes-chat-dashboard-record-session-does-not-mutate-shared-client ()
  "Recording a session result updates buffer-local vars only, never the client."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (hermes-chat--dashboard-set-subscriber
      client (lambda (_event)))
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-live")
               (stored_session_id . "sid-stored")))
     (should (equal hermes-chat--dashboard-active-session-id "sid-live"))
     (should (equal hermes-chat--session-id "sid-stored"))
     (should hermes-chat--dashboard-session-ready-p)
     (should-not (hermes-dashboard-transport-client-session-id client))
     (should-not (hermes-dashboard-transport-client-stored-session-id client)))))

(ert-deftest hermes-chat-dashboard-record-session-binds-subscriber-token ()
  "Recording a session binds the buffer's subscriber token to its session id."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-route")))
     (should (eq (gethash "sid-route"
                          (hermes-dashboard-transport-client-session-index
                           client))
                 hermes-chat--dashboard-token)))))

(ert-deftest hermes-chat-dashboard-shared-client-keeps-buffer-local-session-ids ()
  "Two buffers sharing one client keep independent buffer-local session ids."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback)))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-a"))))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-b"))))
            ;; Buffer B's later record must not clobber buffer A.
            (with-current-buffer buf-a
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-b")))
            ;; The shared client holds no ambient session identity.
            (should-not (hermes-dashboard-transport-client-session-id
                         shared)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-model-switch-targets-current-buffer-session ()
  "A model switch from buffer A targets A's session, not the last recorded one."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        config-session shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((ok . t)))))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers .
                             (((slug . "p")
                               (authenticated . t)
                               (models . ("m")))))))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-a"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-b"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-a
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_items &rest _) "p · m")))
                (hermes-chat-switch-model)))
            ;; Buffer A's switch must carry A's session id.
            (should (equal config-session "sid-a")))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-create-seeds-runtime-before-config-gate ()
  "Fresh creation seeds customized runtime before the post-create gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (push 'config order)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high")
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :model) "gpt-5"))
         (should (equal (plist-get create-args :provider) "openai"))
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (equal config-calls
                        '(("reasoning" "high" "sid-new")
                          ("model" "gpt-5 --provider openai" "sid-new"))))
         (should (equal (reverse order) '(config config submit)))
         (should-not hermes-chat--dashboard-create-model)
         (should-not hermes-chat--dashboard-create-provider))))))

(ert-deftest hermes-chat-dashboard-first-build-uses-draft-model ()
  "The deferred agent build snapshots the draft model during `session.create'."
  (let ((client (hermes-test--dashboard-client)) built-model prompt-model)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq built-model (or (plist-get args :model) "profile-default"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key _value &rest args)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) (setq prompt-model built-model))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "grok-4.6"
               hermes-chat--dashboard-create-provider "xai-oauth")
         (insert "hello")
         (hermes-chat-send)
         (should (equal prompt-model "grok-4.6")))))))


(ert-deftest hermes-chat-dashboard-create-confirms-model-before-first-prompt ()
  "An expensive pre-session model is confirmed before `prompt.submit'."
  (let ((client (hermes-test--dashboard-client)) calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               #'hermes-test--resolve-new-dashboard-session)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (key value args)
                  (push (list key value (plist-get args :session-id)
                              (plist-get args :confirm-expensive-model)) calls)
                  (push 'config order))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-expensive"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (reverse calls)
                        '(("model" "gpt-expensive --provider openai" "sid-new" nil)
                          ("model" "gpt-expensive --provider openai" "sid-new" t)
                          ("reasoning" "high" "sid-new" nil)
                          ("fast" "fast" "sid-new" nil))))
         (should (equal (reverse order)
                        '(config config config config submit))))))))

(ert-deftest hermes-chat-dashboard-control-decline-retries-on-next-action ()
  "A control-action decline preserves the model for an owned retry."
  (let ((client (hermes-test--dashboard-client))
        (answers '(nil t)) action rejected calls create-model)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-model (plist-get args :model))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (_key _value args)
                  (push (plist-get args :confirm-expensive-model) calls)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t))
        (lambda (message) (setq rejected message)))
       (should-not action)
       (should rejected)
       (should (equal create-model "gpt-expensive"))
       (should hermes-chat--dashboard-create-model)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t)))
       (should action)
       (should (equal (reverse calls) '(nil nil t)))
       (should-not hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-retry-overrides-are-non-replacing ()
  "Concurrent retry callers preserve the first owner and send one batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid"
             hermes-chat--dashboard-create-fast-p t
             hermes-chat--create-overrides-retry-session-id "sid")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (string-match-p "configuration is in progress" second-reject))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)
       (should-not hermes-chat--create-overrides-retry-session-id)))))

(ert-deftest hermes-chat-dashboard-fresh-overrides-reject-late-control ()
  "A control caller cannot bypass an attached fresh-session override batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-fast-p t)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (should owner)
         (should-not hermes-chat--create-overrides-retry-session-id)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should hermes-chat--dashboard-create-fast-p)
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (equal second-reject
                      "Pre-session runtime configuration is in progress"))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not first-reject)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)))))

(ert-deftest hermes-chat-dashboard-stale-model-result-does-not-prompt ()
  "A model result for a replaced session rejects without prompting."
  (let ((client (hermes-test--dashboard-client)) resolve prompted continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq prompted t) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((confirm_required . t)))
       (should-not prompted)
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-model-confirmation-revalidates-after-prompt ()
  "Replacing the session during confirmation prevents the confirmed retry."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-active-session-id "sid-new")
                 t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 1))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-repeated-model-confirmation-rejects ()
  "A confirmed retry that still requires confirmation rejects without continuing."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 2))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-confirmed-retry-sync-failure-settles-origin ()
  "A synchronous confirmed-retry error or quit releases exact ownership."
  (dolist (signal '(error quit))
    (let ((client (hermes-test--dashboard-client)) calls resolve rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client _key _value &rest args)
                   (setq calls (1+ (or calls 0)))
                   (if (= calls 1)
                       (setq resolve (plist-get args :resolve))
                     (signal signal nil))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old"
               hermes-chat--dashboard-create-model "gpt-expensive")
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (funcall resolve '((confirm_required . t)))
         (should (= calls 2))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should hermes-chat--dashboard-create-model))))))

(ert-deftest hermes-chat-dashboard-create-uses-chat-working-directory ()
  "A fresh session starts in its gateway working directory."
  (let ((client (hermes-test--dashboard-client)) create-cwd)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-cwd (plist-get args :cwd))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory "/srv/remote-project")
         (insert "hi")
         (hermes-chat-send)
         (should (equal create-cwd "/srv/remote-project")))))))

(ert-deftest hermes-chat-dashboard-remote-create-omits-unknown-cwd ()
  "A detached remote chat never submits its editor directory as gateway cwd."
  (let ((client (hermes-test--dashboard-client)) create-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--remote-filesystem-p t)
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory nil)
         (insert "hi")
         (hermes-chat-send)
         (should create-args)
         (should-not (plist-get create-args :cwd))
         (should (equal default-directory "/tmp/local-editor/")))))))

(ert-deftest hermes-chat-remote-set-directory-preserves-editor-directory ()
  "A remote cwd change applies the backend path without changing editor cwd."
  (let ((client (hermes-test--dashboard-client)) request)
    (hermes-test-with-chat-buffer
     (setq-local hermes-chat--remote-filesystem-p t)
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv/old"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq request (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/mnt/c/translated"))))))
       (hermes-chat-set-directory "C:/project")
       (should (equal request '("C:/project" "sid")))
       (should (equal hermes-chat--working-directory "/mnt/c/translated"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-unknown-remote-directory-starts-with-manual-entry ()
  "A detached remote chat asks for a gateway path without listing a local path."
  (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
    (hermes-test-with-chat-buffer
     (setq-local hermes-chat--remote-filesystem-p t)
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory nil
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (ert-fail "unknown cwd must not be listed")))
               ((symbol-function 'read-string)
                (lambda (_prompt initial &rest _)
                  (setq prompt-default initial)
                  "/srv/manual"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd cwd)
                  (funcall (plist-get args :resolve) `((cwd . ,cwd))))))
       (hermes-chat-set-directory)
       (should (equal prompt-default ""))
       (should (equal set-cwd "/srv/manual"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-set-directory-uses-authoritative-backend-path ()
  "Changing directory applies the authoritative backend path to the chat."
  (let ((client (hermes-test--dashboard-client))
        request)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv/old"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq request (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/mnt/c/translated"))))))
       (hermes-chat-set-directory "C:/project")
       (should (equal request '("C:/project" "sid")))
       (should (equal hermes-chat--working-directory "/mnt/c/translated"))
       (should (equal default-directory "/mnt/c/translated/"))
       (should (string-match-p "\[translated\]" (buffer-name)))))))

(ert-deftest hermes-chat-directory-parent-handles-instance-path-syntax ()
  "Parent navigation is lexical for Unix and Windows instance paths."
  (should (equal (hermes-chat--directory-parent "/srv/project/") "/srv"))
  (should (equal (hermes-chat--directory-parent "/srv") "/"))
  (should (equal (hermes-chat--directory-parent "/") "/"))
  (should (equal (hermes-chat--directory-parent "C:\\Users\\Thanos")
                 "C:\\Users"))
  (should (equal (hermes-chat--directory-parent "C:\\Users") "C:\\"))
  (should (equal (hermes-chat--directory-parent "C:\\") "C:\\")))

(ert-deftest hermes-chat-set-directory-ignores-stale-rejection ()
  "A directory rejection cannot surface in a successor session."
  (let ((client (hermes-test--dashboard-client)) reject surfaced)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq reject (plist-get args :reject))))
               ((symbol-function 'hermes-chat--command-error)
                (lambda (message) (setq surfaced message))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall reject "old session rejected")
       (should-not surfaced)
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-set-directory-ignores-stale-resolution ()
  "A directory response cannot change a successor session's local context."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq resolve (plist-get args :resolve)))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((cwd . "/srv/new")))
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-dashboard-record-session-records-authoritative-cwd ()
  "Session creation records nested backend cwd without changing editor cwd."
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/")
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid") (info . ((cwd . "/srv/project")))))
     (should (equal hermes-chat--working-directory "/srv/project"))
     (should (equal default-directory "/tmp/local-editor/")))))

(ert-deftest hermes-chat-set-directory-browses-instance-filesystem ()
  "Interactive directory selection walks `/api/fs/list' on the owning instance."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (method path &rest args)
                  (let ((directory (cdr (assq 'path (plist-get args :query)))))
                    (push (list method path directory (plist-get args :client))
                          requests)
                    (hermes--promise-resolved
                     (if (equal directory "/srv")
                         '((entries . (((name . "project")
                                        (path . "/srv/project")
                                        (isDirectory . t))
                                       ((name . "README")
                                        (path . "/srv/README")
                                        (isDirectory . :false)))))
                       '((entries . ())))))))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _)
                  (car (seq-find
                        (lambda (candidate)
                          (let ((choice (cdr candidate)))
                            (if (= (length requests) 1)
                                (and (eq (plist-get choice :action) 'browse)
                                     (equal (plist-get choice :path)
                                            "/srv/project"))
                              (eq (plist-get choice :action) 'select))))
                        candidates))))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/srv/project"))))))
       (hermes-chat-set-directory)
       (should (equal (reverse requests)
                      (list (list "GET" "/api/fs/list" "/srv" client)
                            (list "GET" "/api/fs/list" "/srv/project" client))))
       (should (equal set-cwd '("/srv/project" "sid")))
       (should (equal hermes-chat--working-directory "/srv/project"))
       (should (equal default-directory "/srv/project/"))))))

(ert-deftest hermes-chat-directory-browser-falls-back-to-manual-path ()
  "An unavailable listing endpoint still accepts an instance-native path."
  (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (hermes--promise-rejected "404 not found")))
               ((symbol-function 'read-string)
                (lambda (_prompt initial &rest _)
                  (setq prompt-default initial)
                  "/opt/manual"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/opt/manual"))))))
       (hermes-chat-set-directory)
       (should (equal prompt-default "/srv"))
       (should (equal set-cwd '("/opt/manual" "sid")))
       (should (equal hermes-chat--working-directory "/opt/manual"))))))

(ert-deftest hermes-chat-directory-browser-ignores-stale-response ()
  "A directory listing cannot prompt or mutate a successor session."
  (let ((client (hermes-test--dashboard-client))
        (promise (hermes--promise-make)) prompted)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) promise))
               ((symbol-function 'completing-read)
                (lambda (&rest _) (setq prompted t))))
       (hermes-chat-set-directory)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (hermes--promise-resolve promise '((entries . ())))
       (should-not prompted)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-completion ()
  "A completion owned by an old session cannot browse or set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (_method _path &rest args)
                  (push (cdr (assq 'path (plist-get args :query))) requests)
                  (hermes--promise-resolved
                   '((entries . (((name . "project")
                                  (path . "/srv/project")
                                  (isDirectory . t))))))))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  (car (seq-find
                        (lambda (candidate)
                          (eq (plist-get (cdr candidate) :action) 'browse))
                        candidates))))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (equal requests '("/srv")))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-manual-read ()
  "A manual path owned by an old session cannot set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _)
                  (setq requests (1+ (or requests 0)))
                  (hermes--promise-rejected "listing unavailable")))
               ((symbol-function 'read-string)
                (lambda (&rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  "/opt/project"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (= requests 1))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-set-directory-requires-chat-buffer ()
  "The globally autoloaded directory command rejects non-chat buffers."
  (with-temp-buffer
    (should-error (hermes-chat-set-directory temporary-file-directory)
                  :type 'user-error)))

(ert-deftest hermes-chat-set-directory-rejects-active-turn ()
  "Changing directory is unavailable while the current turn is active."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-running-p t)
   (should-error (hermes-chat-set-directory "/tmp/") :type 'user-error)))

(ert-deftest hermes-chat-dashboard-create-seeds-reasoning-fast-before-config-set ()
  "Pre-session reasoning and fast picks seed creation before their config gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (eq (plist-get create-args :fast) t))
         (should (equal (reverse config-calls)
                        '(("reasoning" "high" "sid-new")
                          ("fast" "fast" "sid-new"))))
         (should-not hermes-chat--dashboard-create-reasoning-effort)
         (should-not hermes-chat--dashboard-create-fast-p))))))

(ert-deftest hermes-chat-dashboard-sync-override-failure-settles-origin ()
  "Synchronous model and non-model failures release exact override ownership."
  (dolist (spec '((model error) (reasoning quit)))
    (let ((client (hermes-test--dashboard-client)) rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (&rest _) (signal (cadr spec) nil))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car spec)
           ('model (setq hermes-chat--dashboard-create-model "gpt"))
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort "high")))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-discards-unowned-create-overrides ()
  "A stored session never receives create overrides without retry provenance."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        resume-model resume-provider resume-reasoning resume-fast configs action)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resume-model (plist-get args :model)
                       resume-provider (plist-get args :provider)
                       resume-reasoning (plist-get args :reasoning-effort)
                       resume-fast (plist-get args :fast))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq configs (1+ (or configs 0)))
                 (funcall (plist-get args :resolve) '((key . "unexpected")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "resume me")
         (hermes-chat-send)
         (should-not resume-model)
         (should-not resume-provider)
         (should-not resume-reasoning)
         (should-not resume-fast)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))
        (setq action nil configs nil)
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-client client
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (setq action nil)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-restores-pending-batch-clarify ()
  "Session resume renders locked answers and sends only the unanswered question."
  (let ((client (hermes-test--dashboard-client)) resume sent reads)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve))))
              ((symbol-function
                'hermes-dashboard-transport-clarify-question-respond)
               (lambda (_client request question answer &optional resolve _reject)
                 (setq sent (list request question answer))
                 (funcall resolve '((status . "ok")))))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt reads)
                 "Remaining")))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (funcall
        resume
        '((session_id . "live") (stored_session_id . "stored")
          (messages . (((role . "assistant") (text . "History"))))
          (pending_clarify
           . ((request_id . "req-batch")
              (questions . [((qid . "q0") (question . "Locked"))
                            ((qid . "q1") (question . "Open"))])
              (answers . ((q0 . "Accepted")))))))
       (should (string-match-p "Answered: Accepted" (buffer-string)))
       (hermes-chat-respond-to-prompt "req-batch")
       (should (equal reads '("Open: ")))
       (should (equal sent '("req-batch" "q1" "Remaining")))))))

(ert-deftest hermes-chat-dashboard-stale-resume-cannot-install-clarify ()
  "A late session-resume result cannot install a prompt after reset."
  (let ((client (hermes-test--dashboard-client)) resume)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (hermes-chat--reset-transcript)
       (funcall resume
                '((session_id . "stale")
                  (pending_clarify
                   . ((request_id . "req-stale")
                      (questions . [((qid . "q0") (question . "Stale"))])))))
       (should-not (hermes-chat--pending-prompt-p))
       (should-not (string-match-p "Stale" (buffer-string)))))))

(ert-deftest hermes-chat-dashboard-stale-non-model-override-stays-session-bound ()
  "Late reasoning and fast results settle without mutating their successor."
  (dolist (cell '((reasoning . "high") ("fast" . "fast")))
    (let ((client (hermes-test--dashboard-client)) resolve continued aborted action requests)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client key _value &rest args)
                   (push (list key (plist-get args :session-id)) requests)
                   (unless resolve (setq resolve (plist-get args :resolve))))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car cell)
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort (cdr cell)))
           ("fast" (setq hermes-chat--dashboard-create-fast-p t)))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (setq continued t))
          hermes-chat--lifecycle-generation (lambda (message) (setq aborted message)))
         (setq hermes-chat--dashboard-active-session-id "sid-new")
         (funcall resolve '((key . "stale")))
         (should-not continued)
         (should aborted)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should (= (length requests) 1))
         (should-not (hermes-chat--dashboard-create-config-cells))
         (should-not hermes-chat--create-overrides-retry-session-id))))))

(ert-deftest hermes-chat-dashboard-reconnected-keeps-each-buffer-detached ()
  "A shared reconnect leaves each buffer's durable session lazy."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        resumed shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest _args)
                 (push session-id resumed))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-a"))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-b"))
            ;; Simulate reconnect for each buffer.
            (with-current-buffer buf-a
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-a" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (with-current-buffer buf-b
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-b" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (should-not resumed)
            (with-current-buffer buf-a
              (should (equal hermes-chat--session-id "stored-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--session-id "stored-b"))))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(provide 'hermes-chat-dashboard-tests)
;;; hermes-chat-dashboard-tests.el ends here
