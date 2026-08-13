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

(ert-deftest hermes-chat-dashboard-create-applies-model-override-via-config-set ()
  "A pre-session model/provider pick applies via `config.set' after create.
The `session.create' request must not carry the override parameters (the
handler ignores them); the `config.set' must precede `prompt.submit'."
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
               hermes-chat--dashboard-create-provider "openai")
         (insert "hi")
         (hermes-chat-send)
         (should-not (plist-member create-args :model))
         (should-not (plist-member create-args :provider))
         (should (equal config-calls
                        '(("model" "gpt-5 --provider openai" "sid-new"))))
         (should (equal (reverse order) '(config submit)))
         (should-not hermes-chat--dashboard-create-model)
         (should-not hermes-chat--dashboard-create-provider))))))

(ert-deftest hermes-chat-dashboard-create-applies-reasoning-fast-via-config-set ()
  "Pre-session reasoning/fast picks apply via `config.set' after create."
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
         (should-not (plist-member create-args :reasoning-effort))
         (should-not (plist-member create-args :fast))
         (should (equal (reverse config-calls)
                        '(("reasoning" "high" "sid-new")
                          ("fast" "fast" "sid-new"))))
         (should-not hermes-chat--dashboard-create-reasoning-effort)
         (should-not hermes-chat--dashboard-create-fast-p))))))

(ert-deftest hermes-chat-dashboard-resume-does-not-send-create-runtime-overrides ()
  "Resuming a stored session does not forward create-time runtime overrides."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        resume-model resume-provider resume-reasoning resume-fast)
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
         (should-not resume-fast))))))

(ert-deftest hermes-chat-dashboard-reconnect-resumes-each-buffer-own-session ()
  "On reconnect, each buffer resumes its own stored session id."
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
            (should (member "stored-a" resumed))
            (should (member "stored-b" resumed)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(provide 'hermes-chat-dashboard-tests)
;;; hermes-chat-dashboard-tests.el ends here
