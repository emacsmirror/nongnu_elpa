;;; hermes-chat-history-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-resume-running-restores-stream-and-draft-undo ()
  "A busy resume owns deltas and completion without a new message.start."
  (dolist (busy '(((running . t))
                  ((inflight . ((user . "Question"))))))
    (let ((client (hermes-test--dashboard-client)) resolve buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'hermes-notifications-notify) #'ignore)
                ((symbol-function 'pop-to-buffer-same-window)
                 (lambda (buffer &rest _) buffer))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _sid &rest args)
                   (setq resolve (plist-get args :resolve)))))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (buffer-enable-undo)
                (insert "draft")
                (undo-boundary))
              (funcall resolve
                       (append busy
                               '((session_id . "live")
                                 (messages . (((role . "user")
                                               (text . "Question")))))))
              (with-current-buffer buffer
                (should (hermes-chat--active-turn-p))
                (should hermes-chat--pending-assistant-id)
                (should (eq hermes-chat--process client)))
              (dolist (text '("Remaining " "reply"))
                (hermes-dashboard-transport--dispatch-event
                 client (list :type 'delta :content text :session-id "live")))
              (hermes-dashboard-transport--dispatch-event
               client '(:type done :session-id "live"))
              (with-current-buffer buffer
                (let ((entries
                       (cl-remove-if
                        (lambda (entry) (eq (plist-get entry :role) 'status))
                        (hermes-chat--entries))))
                  (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                         entries)
                                 '(user assistant)))
                  (should (equal (plist-get (cadr entries) :content)
                                 "Remaining reply"))
                  (should (eq (plist-get (cadr entries) :status) 'done)))
                (should-not (hermes-chat--active-turn-p))
                (should-not hermes-chat--dashboard-running-p)
                (should-not hermes-chat--pending-assistant-id)
                (should-not hermes-chat--process)
                (undo 1)
                (should (equal (hermes-chat-input-string) ""))
                (should (string-match-p "Question" (buffer-string)))
                (should (string-match-p "Remaining reply" (buffer-string)))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-resume-stale-history-cannot-restore-turn ()
  "A replaced transport or request cannot restore old history or busy state."
  (dolist (replacement '(client request lifetime))
    (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          (hermes-dashboard-transport-idle-close-delay nil)
          (client (hermes-test--dashboard-client)) resolve reject buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'pop-to-buffer-same-window)
                 (lambda (buffer &rest _) buffer))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _sid &rest args)
                   (setq resolve (plist-get args :resolve)
                         reject (plist-get args :reject)))))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (should (eq hermes-chat--dashboard-client client))
                (should (= (hash-table-count hermes-dashboard-transport--clients) 1))
                (should (= (hermes-dashboard-transport-client-refcount client) 1))
                (should (= (hash-table-count
                            (hermes-dashboard-transport-client-subscribers client))
                           1))
                (pcase replacement
                  ('client (setq hermes-chat--dashboard-client
                                 (hermes-test--dashboard-client)))
                  ('request (hermes-chat--next-transport-generation))
                  ('lifetime (hermes-chat--invalidate-transport-state))))
              (funcall resolve
                       '((session_id . "old") (running . t)
                         (messages . (((role . "user") (text . "Old"))))))
              (funcall reject "Old error")
              (with-current-buffer buffer
                (should-not (hermes-chat--entries))
                (should-not hermes-chat--dashboard-active-session-id)
                (should-not (hermes-chat--active-turn-p))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          ;; Replacing the buffer-local client deliberately orphans this owner.
          (when (eq replacement 'client)
            (hermes-dashboard-transport-stop client)))
        (should (zerop (hash-table-count hermes-dashboard-transport--clients)))
        (should (zerop (hermes-dashboard-transport-client-refcount client)))
        (should (hermes-dashboard-transport-client-stopping-p client))
        (should (zerop (hash-table-count
                        (hermes-dashboard-transport-client-subscribers client))))))))

(ert-deftest hermes-chat-resume-stale-history-preserves-ambient-client ()
  "The stale-history fixture neither borrows nor disposes an ambient client."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client)))
      (unwind-protect
          (progn
            (should (eq (hermes-dashboard-transport-acquire) client))
            (funcall (ert-test-body
                      (ert-get-test
                       'hermes-chat-resume-stale-history-cannot-restore-turn)))
            (should (= (hash-table-count hermes-dashboard-transport--clients) 1))
            (should (eq (gethash (hermes-dashboard-transport-client-endpoint-key client)
                                 hermes-dashboard-transport--clients)
                        client))
            (should (= (hermes-dashboard-transport-client-refcount client) 1))
            (should (hermes-dashboard-transport--client-viable-p client)))
        (hermes-dashboard-transport-stop client)))))

(ert-deftest hermes-chat-resume-renders-prior-messages ()
  "Resuming renders history and later backend-owned turns without duplicates."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _sid &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "live-1") (running . nil)
                            (messages . (((role . "user") (text . "hi there"))
                                         ((role . "assistant") (text . "hello back"))
                                         ((role . "tool") (name . "terminal")
                                          (context . "make test")))))))))
      (let ((buffer (hermes-chat-resume-session "sid-stored" "My Session")))
        (unwind-protect
            (with-current-buffer buffer
              (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                     (hermes-chat--entries))
                             '(user assistant tool)))
              (should-not (hermes-chat--active-turn-p))
              (should-not hermes-chat--pending-assistant-id)
              (dolist (content '("wake one" "wake two"))
                (hermes-dashboard-transport--dispatch-event
                 client '(:type status :event "message.start" :status "started"
                          :session-id "live-1"))
                (hermes-dashboard-transport--dispatch-event
                 client (list :type 'delta :content content
                              :session-id "live-1"))
                (hermes-dashboard-transport--dispatch-event
                 client '(:type done :session-id "live-1")))
              (let ((assistants
                     (cl-remove-if-not
                      (lambda (entry) (eq (plist-get entry :role) 'assistant))
                      (hermes-chat--entries))))
                (should (equal (mapcar (lambda (entry)
                                         (plist-get entry :content))
                                       assistants)
                               '("hello back" "wake one" "wake two")))
                (should (cl-every (lambda (entry)
                                    (eq (plist-get entry :status) 'done))
                                  assistants)))
              (should-not hermes-chat--pending-assistant-id)
              (should (string-match-p "terminal: make test" (buffer-string)))
              (should (equal hermes-chat--dashboard-active-session-id "live-1")))
          (kill-buffer buffer))))))

(ert-deftest hermes-chat-dashboard-resume-running-restores-inflight-guard ()
  (let* ((client-1 (hermes-test--dashboard-client))
         (client-2 (hermes-test--dashboard-client))
         (clients (list client-1 client-2))
         callback first-callback second-callback
         resumed-session submit-sessions first-assistant-id second-assistant-id)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 (pop clients)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-1")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (setq resumed-session session-id)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-1")
                            (resumed . "sid-stored")
                            (running . t)
                            (inflight . ((turn_id . "old-turn")))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (push (plist-get args :session-id) submit-sessions))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (setq first-callback callback
               first-assistant-id hermes-chat--pending-assistant-id)
         (should (equal submit-sessions '("sid-live-1")))
         (setf (hermes-dashboard-transport-client-websocket client-1) nil
               (hermes-dashboard-transport-client-ready-p client-1) nil)
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (should-not hermes-chat--pending-assistant-id)
         (insert "second")
         (hermes-chat-send)
         (setq second-callback callback
               second-assistant-id (plist-get (nth 3 (hermes-chat--entries))
                                              :id))
         (should (equal resumed-session "sid-stored"))
         (should (equal submit-sessions '("sid-live-1")))
         (should (equal hermes-chat--pending-assistant-id first-assistant-id))
         ;; restore-inflight stream branch: reattach to the live turn, no suppress.
         (should (equal hermes-chat--dashboard-stream-assistant-id
                        first-assistant-id))
         (should-not hermes-chat--dashboard-suppress-stream-p)
         (funcall second-callback
                  '(:type delta
                    :session-id "sid-live-1"
                    :content "old inflight"))
         (let* ((entries (hermes-chat--entries))
                (first-assistant (nth 2 entries))
                (second-assistant (nth 3 entries)))
           (should (equal (plist-get first-assistant :content)
                          "old inflight"))
           (should (equal (plist-get first-assistant :status) 'streaming))
           (should (equal (plist-get second-assistant :id)
                          second-assistant-id))
           (should-not (string-match-p
                        "old inflight"
                        (plist-get second-assistant :content))))
         (insert "third")
         (hermes-chat-send)
         (should-not (hermes-test--queued-contents))
         (should (equal (hermes-chat-input-string) ""))
         (should (equal submit-sessions '("sid-live-1" "sid-live-1"))))))))

(ert-deftest hermes-chat-dashboard-resume-running-without-detached-guards-retry ()
  (let ((client (hermes-test--dashboard-client))
        callback resumed-session submit-sessions assistant-id)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (&rest _args)
                 (error "session.create should not run for stored chat")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (setq resumed-session session-id)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "sid-stored")
                            (running . t)
                            (inflight . ((turn_id . "old-turn")))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (push (plist-get args :session-id) submit-sessions))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored")
         (insert "new prompt")
         (hermes-chat-send)
         (setq assistant-id hermes-chat--pending-assistant-id)
         (should (equal resumed-session "sid-stored"))
         (should-not submit-sessions)
         (should hermes-chat--pending-assistant-id)
         ;; restore-inflight retry branch: suppress the live stream, no stream id.
         (should hermes-chat--dashboard-suppress-stream-p)
         (should-not hermes-chat--dashboard-stream-assistant-id)
         (funcall callback
                  '(:type delta
                    :session-id "sid-live"
                    :content "old inflight"))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :id) assistant-id))
           (should-not (string-match-p
                        "old inflight"
                        (plist-get assistant :content))))
         (funcall callback '(:type done :session-id "sid-live"))
         (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-dashboard-resume-running-without-detached-suppresses-terminal-content ()
  (dolist (terminal '((:type done :session-id "sid-live"
                       :content "old final answer")
                      (:type error :session-id "sid-live"
                       :content "old error answer")))
    (let ((client (hermes-test--dashboard-client))
          callback resumed-session submit-sessions assistant-id)
      (cl-letf (((symbol-function 'hermes-transport-send)
                 (lambda (&rest _args) (error "CLI fallback should not run")))
                ((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest args)
                   (setq callback (plist-get args :callback))
                   client))
                ((symbol-function 'hermes-dashboard-transport-session-create)
                 (lambda (&rest _args)
                   (error "session.create should not run for stored chat")))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client session-id &rest args)
                   (setq resumed-session session-id)
                   (funcall (plist-get args :resolve)
                            '((session_id . "sid-live")
                              (resumed . "sid-stored")
                              (running . t)
                              (inflight . ((turn_id . "old-turn")))))))
                ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                 (lambda (_client _text &rest args)
                   (push (plist-get args :session-id) submit-sessions))))
        (let ((hermes-transport-send-function #'hermes-transport-send))
          (hermes-test-with-chat-buffer
           (setq hermes-chat--session-id "sid-stored")
           (insert "new prompt")
           (hermes-chat-send)
           (setq assistant-id hermes-chat--pending-assistant-id)
           (should (equal resumed-session "sid-stored"))
           (should-not submit-sessions)
           (funcall callback
                    '(:type delta
                      :session-id "sid-live"
                      :content "old inflight"))
           (let ((assistant (hermes-test--assistant-entry)))
             (should (equal (plist-get assistant :id) assistant-id))
             (should-not (string-match-p
                          "old inflight"
                          (plist-get assistant :content))))
           (funcall callback terminal)
           (let ((assistant (hermes-test--assistant-entry)))
             (should-not (string-match-p
                          (regexp-quote (plist-get terminal :content))
                          (plist-get assistant :content))))
           (should-not hermes-chat--pending-assistant-id)))))))

(ert-deftest hermes-chat-finish-assistant-clears-only-matching-bookkeeping ()
  "Finishing an assistant clears stream/suppress/detached only when they match it."
  (with-temp-buffer
    (setq hermes-chat--dashboard-stream-assistant-id "a1"
          hermes-chat--dashboard-suppress-stream-p t
          hermes-chat--dashboard-detached-assistant-id "a1"
          hermes-chat--pending-assistant-id "a1")
    (hermes-chat--dashboard-finish-assistant "other")
    (should (equal hermes-chat--dashboard-stream-assistant-id "a1"))
    (should hermes-chat--dashboard-suppress-stream-p)
    (should (equal hermes-chat--dashboard-detached-assistant-id "a1"))
    (hermes-chat--dashboard-finish-assistant "a1")
    (should-not hermes-chat--dashboard-stream-assistant-id)
    (should-not hermes-chat--dashboard-suppress-stream-p)
    (should-not hermes-chat--dashboard-detached-assistant-id)))

(ert-deftest hermes-chat-forget-live-session-preserves-durable-key ()
  "Forgetting the live session clears ready/active id but keeps the durable key."
  (with-temp-buffer
    (setq hermes-chat--dashboard-session-ready-p t
          hermes-chat--dashboard-active-session-id "live-1"
          hermes-chat--session-id "durable-1")
    (hermes-chat--forget-live-dashboard-session)
    (should-not hermes-chat--dashboard-session-ready-p)
    (should-not hermes-chat--dashboard-active-session-id)
    (should (equal hermes-chat--session-id "durable-1"))))

(ert-deftest hermes-chat-restore-inflight-fresh-branch-binds-live-stream ()
  "Restoring with no prior turn inserts an assistant and binds the live stream."
  (hermes-test-with-chat-buffer
   (hermes-chat--dashboard-restore-inflight-turn 'fake-client)
   (should hermes-chat--pending-assistant-id)
   (should (eq hermes-chat--process 'fake-client))
   (should (equal hermes-chat--dashboard-stream-assistant-id
                  hermes-chat--pending-assistant-id))
   (should-not hermes-chat--dashboard-suppress-stream-p)))

(ert-deftest hermes-chat-dashboard-ignores-stale-no-session-close-after-settled ()
  (let* ((client-1 (hermes-test--dashboard-client))
         (client-2 (hermes-test--dashboard-client))
         (clients (list client-1 client-2))
         callback first-callback second-callback
         resumed-session submit-sessions)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 (pop clients)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-1")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (setq resumed-session session-id)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live-2")
                            (resumed . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (push (plist-get args :session-id) submit-sessions))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (setq first-callback callback)
         (setf (hermes-dashboard-transport-client-websocket client-1) nil
               (hermes-dashboard-transport-client-ready-p client-1) nil)
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (insert "second")
         (hermes-chat-send)
         (setq second-callback callback)
         (should (equal resumed-session "sid-stored"))
         (should (equal submit-sessions '("sid-live-2" "sid-live-1")))
         (funcall second-callback
                  '(:type delta
                    :session-id "sid-live-2"
                    :content "retry ok"))
         (funcall second-callback '(:type done :session-id "sid-live-2"))
         (should-not hermes-chat--pending-assistant-id)
         (should (equal hermes-chat--dashboard-active-session-id "sid-live-2"))
         (funcall first-callback '(:type error :content "late old error"))
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "late old close"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-live-2"))
         (let* ((entries (hermes-chat--entries))
                (first-assistant (nth 1 entries))
                (second-assistant (nth 3 entries)))
           (should-not (string-match-p
                        "late old"
                        (plist-get first-assistant :content)))
           (should (equal (plist-get second-assistant :content) "retry ok"))
           (should (equal (plist-get second-assistant :status) 'done))))))))

(ert-deftest hermes-chat-resume-session-presets-session-id ()
  "Resuming a session keeps its durable id and owning profile."
  (let ((default-directory "/tmp/emacs-hermes/") resume-args)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
             (lambda (&rest _) (hermes-test--dashboard-client)))
            ((symbol-function 'hermes-dashboard-transport-session-resume)
             (lambda (_client _sid &rest args) (setq resume-args args))))
      (let ((buffer (hermes-chat-resume-session "sid-42" "My chat" "work")))
        (unwind-protect
            (with-current-buffer buffer
              (should (derived-mode-p 'hermes-chat-mode))
              (should (equal hermes-chat--session-id "sid-42"))
              (should (equal hermes-chat--profile "work"))
              (should (equal hermes-chat--title "My chat"))
              (should (equal (plist-get resume-args :profile) "work"))
              (should (equal (buffer-name)
                             "*Hermes@work: [emacs-hermes]*")))
          (kill-buffer buffer))))))

(ert-deftest hermes-chat-send-during-history-hydrates-before-followup ()
  (dolist (reverse-p '(nil t))
    (let ((client (hermes-test--dashboard-client)) callbacks submits buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start) (lambda (&rest _) client))
                ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
                ((symbol-function 'hermes-chat--dashboard-client-live-p)
                 (lambda (candidate) (eq candidate client)))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _sid &rest args)
                   (setq callbacks (append callbacks (list (plist-get args :resolve))))))
                ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                 (lambda (_client text &rest args)
                   (push text submits)
                   (funcall (plist-get args :resolve) '((status . "streaming"))))))
        (unwind-protect
            (let ((hermes-transport-send-function #'hermes-transport-send))
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer (insert "followup") (hermes-chat-send))
              (dolist (callback (if reverse-p (reverse callbacks) callbacks))
                (funcall callback '((session_id . "live") (stored_session_id . "stored")
                                    (messages . (((role . "user") (text . "prior question"))
                                                 ((role . "assistant") (text . "prior answer")))))))
              (with-current-buffer buffer
                (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                                      (seq-filter (lambda (entry)
                                                    (memq (plist-get entry :role) '(user assistant)))
                                                  (hermes-chat--entries)))
                               '("prior question" "prior answer" "followup" "")))
                (should (equal submits '("followup")))
                (should (= (length callbacks) 1))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-history-callback-failure-retains-input-real-rpc ()
  "Delayed restoration errors and quits need explicit retry, not endless queueing."
  (dolist (fault '((render error) (render quit) (printer error) (record error)
                   (clarify quit) (render error t) (clarify quit t)))
    (let* ((stage (car fault))
           (condition (cadr fault))
           (client (hermes-test--dashboard-client))
           (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
           (hermes-dashboard-transport-request-timeout nil)
           (hermes-dashboard-transport-idle-close-delay nil)
           (hermes-transport-send-function #'hermes-transport-send)
           (render-history (symbol-function 'hermes-chat--render-history))
           (print-entry (symbol-function 'hermes-chat--print-entry))
           (fail-p t) frames buffer
           (hermes-dashboard-transport-websocket-send-function
            (lambda (_socket text)
              (push (hermes-dashboard-transport--decode-frame text) frames))))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'hermes-chat--dashboard-refresh-goal)
                 (lambda ()
                   (when (and fail-p (eq stage 'record))
                     (signal condition '("private callback details")))))
                ((symbol-function 'hermes-chat--dashboard-restore-pending-clarify)
                 (lambda (_result)
                   (when (and fail-p (eq stage 'clarify))
                     (signal condition '("private callback details")))))
                ((symbol-function 'hermes-chat--print-entry)
                 (lambda (entry)
                   (if (and fail-p (eq stage 'printer)
                            (equal (plist-get entry :content) "prior"))
                       (progn
                         (insert "prior")
                         (signal condition '("private callback details")))
                     (funcall print-entry entry))))
                ((symbol-function 'hermes-chat--render-history)
                 (lambda (messages)
                   (if (and fail-p (eq stage 'render))
                       (progn
                         (funcall render-history (list (car messages)))
                         (signal condition '("private callback details")))
                     (funcall render-history messages)))))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (insert "queued followup") (hermes-chat-send)
                (insert "newer draft")
                (let* ((id (hermes-transport--get (car frames) 'id))
                       (request (gethash id (hermes-dashboard-transport-client-pending client)))
                       (result `((session_id . "sid-prompt") (stored_session_id . "canonical")
                                 (running . ,(nth 2 fault))
                                 (messages . [((role . "user") (text . "prior"))
                                              ((role . "assistant") (text . "answer"))]))))
                  (hermes-dashboard-transport--handle-frame
                   client (hermes-dashboard-transport--encode-frame
                           `((jsonrpc . "2.0") (id . ,id) (result . ,result))))
                  (should (= 0 (hash-table-count (hermes-dashboard-transport-client-pending client))))
                  (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
                  (should (equal (hermes-chat-input-string) "newer draft"))
                  (unless (memq stage '(record printer))
                    (should (string-match-p "prior" (buffer-string))))
                  (should-not (string-match-p "private callback details" (buffer-string)))
                  (should (equal (hermes-test--queued-contents) '("queued followup")))
                  (should (= 1 (length frames)))
                  ;; A late success or terminal cannot clear the failed read.
                  (funcall (plist-get request :resolve) result)
                  (hermes-test--emit-dashboard-prompt client "message.complete"
                    '((text . "late output") (status . "complete")))
                  (should (= 1 (length frames)))
                  (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
                  ;; Explicit Send retries only the read, retaining FIFO order.
                  (setq fail-p nil)
                  (hermes-chat-send)
                  (should (= 2 (length frames)))
                  (should (equal (hermes-transport--get (car frames) 'method) "session.resume"))
                  (should (equal (hermes-test--queued-contents) '("queued followup" "newer draft")))
                  (insert "latest draft")
                  (hermes-dashboard-transport--handle-frame
                   client (hermes-dashboard-transport--encode-frame
                           `((jsonrpc . "2.0") (id . ,(hermes-transport--get (car frames) 'id))
                             (result . ,result))))
                  (should-not hermes-chat--session-bootstrap)
                  (when (nth 2 fault)
                    (should (= 2 (length frames)))
                    (should (equal (hermes-test--queued-contents)
                                   '("queued followup" "newer draft")))
                    (hermes-test--emit-dashboard-prompt client "message.complete"
                      '((text . "resumed completion") (status . "complete"))))
                  (should (equal
                           (mapcar (lambda (entry) (plist-get entry :content))
                                   (seq-filter
                                    (lambda (entry)
                                      (memq (plist-get entry :role) '(user assistant)))
                                    (hermes-chat--entries)))
                           (append
                            (when (and (nth 2 fault) (eq stage 'clarify))
                              '("late output"))
                            '("prior" "answer")
                            (when (nth 2 fault) '("resumed completion"))
                            '("queued followup" ""))))
                  (should (equal (hermes-transport--get (car frames) 'method) "prompt.submit"))
                  (hermes-dashboard-transport--handle-frame
                   client (hermes-dashboard-transport--encode-frame
                           `((jsonrpc . "2.0") (id . ,(hermes-transport--get (car frames) 'id))
                             (result . ((status . "streaming"))))))
                  (should (equal (hermes-test--queued-contents) '("newer draft")))
                  (should (equal (hermes-chat-input-string) "latest draft"))
                  (should (hermes-chat--point-in-input-p))
                  (dolist (text '("prior" "answer"))
                    (save-excursion
                      (goto-char (point-min))
                      (should (search-forward text (hermes-chat--input-position) t))
                      (should-not (search-forward text (hermes-chat--input-position) t))))
                  (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'text)
                                 "queued followup")))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-history-callback-failure-cannot-settle-successor ()
  "An exception after owner replacement must not publish into the successor."
  (dolist (replacement '(session client lifetime request turn))
    (let ((client (hermes-test--dashboard-client))
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          resolve buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _sid &rest args)
                   (setq resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-chat--render-history)
                 (lambda (_messages)
                   (pcase replacement
                     ('session (setq hermes-chat--session-id "successor"))
                     ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
                     ('lifetime (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token)))
                     ('request (setq hermes-chat--session-bootstrap (copy-sequence hermes-chat--session-bootstrap)))
                     ('turn (hermes-chat--next-transport-generation)))
                   (error "retired restoration"))))
        (unwind-protect
            (let ((hermes-transport-send-function #'hermes-transport-send))
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (insert "retained draft")
                (let ((before (buffer-string)))
                  (should-error
                   (funcall resolve '((session_id . "live") (stored_session_id . "stored"))))
                  (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'preflight))
                  (should (equal before (buffer-string))))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (hermes-dashboard-transport-stop client))))))

(ert-deftest hermes-chat-history-rejection-retains-input-for-explicit-retry ()
  (let ((client (hermes-test--dashboard-client)) requests submits buffer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start) (lambda (&rest _) client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _sid &rest args) (push args requests)))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (funcall (plist-get args :resolve) '((status . "streaming"))))))
      (unwind-protect
          (let ((hermes-transport-send-function #'hermes-transport-send))
            (setq buffer (hermes-chat-resume-session "stored"))
            (with-current-buffer buffer
              (insert "first queued") (hermes-chat-send)
              (should-not submits)
              (should (= 1 (length requests)))
              (funcall (plist-get (car requests) :reject) "read failed")
              (should (equal (hermes-test--queued-contents) '("first queued")))
              (should-not submits)
              ;; Empty Send retries the history read, not the queued prompt.
              (hermes-chat-send)
              (should (= 2 (length requests)))
              (should-not submits)
              (insert "newer composer")
              (funcall (plist-get (cadr requests) :resolve)
                       '((session_id . "stale") (messages . (((role . "user") (text . "STALE"))))))
              (should-not (string-match-p "STALE" (buffer-string)))
              (funcall (plist-get (car requests) :resolve)
                       '((session_id . "live") (stored_session_id . "stored")
                         (messages . (((role . "user") (text . "prior"))))))
              (should (equal submits '("first queued")))
              (should (equal (hermes-chat-input-string) "newer composer"))
              (should-not hermes-chat--session-bootstrap)
              (should (= 1 (cl-count "prior" (hermes-chat--entries)
                                      :test #'equal :key (lambda (entry) (plist-get entry :content)))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-history-overlap-running-result-waits-for-terminal ()
  (let ((client (hermes-test--dashboard-client)) resolve submits buffer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start) (lambda (&rest _) client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _sid &rest args) (setq resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (funcall (plist-get args :resolve) '((status . "streaming"))))))
      (unwind-protect
          (let ((hermes-transport-send-function #'hermes-transport-send))
            (setq buffer (hermes-chat-resume-session "stored"))
            (with-current-buffer buffer
              (insert "followup") (hermes-chat-send)
              (funcall resolve '((session_id . "sid-prompt") (stored_session_id . "stored")
                                 (running . t) (messages . (((role . "user") (text . "prior"))))))
              (should-not submits)
              (should (equal (hermes-test--queued-contents) '("followup")))
              (hermes-test--emit-dashboard-prompt client "message.complete"
                '((text . "prior result") (status . "complete")))
              (should (equal submits '("followup")))
              (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                                    (seq-filter (lambda (entry) (memq (plist-get entry :role) '(user assistant)))
                                                (hermes-chat--entries)))
                             '("prior" "prior result" "followup" "")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-history-rejects-session-and-connection-replacement ()
  (dolist (replacement '(session connection client lifetime request turn))
    (let ((client (hermes-test--dashboard-client))
          (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
          resolve reject buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start) (lambda (&rest _) client))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _sid &rest args)
                   (setq resolve (plist-get args :resolve) reject (plist-get args :reject)))))
        (unwind-protect
            (let ((hermes-transport-send-function #'hermes-transport-send))
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (insert "retained followup") (hermes-chat-send)
                (pcase replacement
                  ('session (setq hermes-chat--session-id "replacement"))
                  ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
                  ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
                  ('lifetime (setq hermes-chat--lifecycle-generation (hermes-chat--next-lifetime-token)))
                  ('request (setq hermes-chat--session-bootstrap (copy-sequence hermes-chat--session-bootstrap)))
                  ('turn (hermes-chat--next-transport-generation)))
                (let ((before (hermes-chat--entries))
                      (owner hermes-chat--session-bootstrap))
                  (funcall resolve '((session_id . "old") (running . t)
                                     (messages . (((role . "user") (text . "stale history"))))))
                  (should (equal before (hermes-chat--entries)))
                  (funcall reject "read failure")
                  (if (eq replacement 'connection)
                      ;; A retired connection cannot publish success, but the
                      ;; exact still-owned read must release its failed phase.
                      (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
                    (should (eq (plist-get owner :phase) 'preflight))
                    (should (equal before (hermes-chat--entries))))
                  (should (eq owner hermes-chat--session-bootstrap))
                  (should-not hermes-chat--dashboard-active-session-id)
                  (should (equal (hermes-test--queued-contents) '("retained followup"))))))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (hermes-dashboard-transport-stop client))))))

(ert-deftest hermes-chat-history-transport-loss-retries-only-on-send-real-rpc ()
  (dolist (loss '(stop reconnect))
    (let* ((client (hermes-test--dashboard-client))
           (replacement (hermes-test--dashboard-client))
           (hermes-dashboard-transport--clients (make-hash-table :test #'equal))
           (hermes-dashboard-transport-request-timeout nil)
           (hermes-dashboard-transport-heartbeat-interval nil)
           (hermes-dashboard-transport-idle-close-delay nil)
           (hermes-transport-send-function #'hermes-transport-send)
           (starts 0) frames buffer
           (hermes-dashboard-transport-websocket-send-function
            (lambda (_socket text)
              (let ((frame (hermes-dashboard-transport--decode-frame text)))
                (when (equal (hermes-transport--get frame 'method) "prompt.submit")
                  ;; Hydration must be visible before the prompt reaches the wire.
                  (should (string-match-p "prior history" (buffer-string))))
                (push frame frames)))))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _)
                   (if (= (cl-incf starts) 1) client replacement)))
                ((symbol-function 'websocket-close) #'ignore)
                ((symbol-function 'hermes-dashboard-transport--reconnect-attempt)
                 (lambda (owner &rest _)
                   ;; Substitute only the new connection's acquisition.
                   (setf (hermes-dashboard-transport-client-websocket owner) 'new-socket))))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat-resume-session "stored"))
              (with-current-buffer buffer
                (insert "accepted behind history") (hermes-chat-send)
                (should (= 1 (length frames)))
                (should (equal (hermes-transport--get (car frames) 'method) "session.resume"))
                (let* ((old-id (hermes-transport--get (car frames) 'id))
                       (old-request (gethash old-id (hermes-dashboard-transport-client-pending client)))
                       (connection (hermes-dashboard-transport-client-generation client)))
                  (pcase loss
                    ('stop (hermes-dashboard-transport-stop client "Socket lost" '(:type status :status "closed")))
                    ('reconnect (hermes-dashboard-transport-reconnect client)))
                  (should (> (hermes-dashboard-transport-client-generation client) connection))
                  (should (= 0 (hash-table-count (hermes-dashboard-transport-client-pending client))))
                  (should (eq (plist-get hermes-chat--session-bootstrap :phase) 'failed))
                  (should (equal (hermes-test--queued-contents) '("accepted behind history")))
                  ;; Connection readiness alone must not replay the resume or input.
                  (when (eq loss 'reconnect)
                    (hermes-dashboard-transport--complete-ready client '((method . "ready"))))
                  (should (= 1 (length frames)))
                  (let ((before (buffer-string)))
                    (funcall (plist-get old-request :resolve)
                             '((session_id . "stale") (messages . (((role . "user") (text . "stale history"))))))
                    (should (equal before (buffer-string))))
                  ;; Empty public Send explicitly starts one replacement read.
                  (hermes-chat-send)
                  (should (= (if (eq loss 'stop) 2 1) starts))
                  (should (eq hermes-chat--dashboard-client
                              (if (eq loss 'stop) replacement client)))
                  (should (= 2 (length frames)))
                  (should (equal (hermes-transport--get (car frames) 'method) "session.resume"))
                  (should (equal (hermes-transport--get (hermes-transport--get (car frames) 'params) 'session_id)
                                 "stored"))
                  (let ((owner hermes-chat--session-bootstrap)
                        (before (buffer-string)))
                    (funcall (plist-get old-request :reject) "late failure")
                    (should (eq owner hermes-chat--session-bootstrap))
                    (should (eq (plist-get owner :phase) 'preflight))
                    (should (equal before (buffer-string))))
                  (let* ((active hermes-chat--dashboard-client)
                         (id (hermes-transport--get (car frames) 'id))
                         (reply (hermes-dashboard-transport--encode-frame
                                 `((jsonrpc . "2.0") (id . ,id)
                                   (result . ((session_id . "live") (stored_session_id . "stored")
                                              (messages . [((role . "user") (text . "prior history"))])))))))
                    (hermes-dashboard-transport--handle-frame active reply)
                    (should-not hermes-chat--session-bootstrap)
                    (let ((prompts (seq-filter (lambda (frame)
                                                 (equal (hermes-transport--get frame 'method) "prompt.submit"))
                                               frames)))
                      (should (= 1 (length prompts)))
                      (should (equal (hermes-transport--get (hermes-transport--get (car prompts) 'params) 'text)
                                     "accepted behind history"))
                      (hermes-dashboard-transport--handle-frame
                       active (hermes-dashboard-transport--encode-frame
                               `((jsonrpc . "2.0") (id . ,(hermes-transport--get (car prompts) 'id))
                                 (result . ((status . "streaming")))))))
                    (should-not hermes-chat--queued-messages)
                    (let ((before (buffer-string)) (count (length frames)))
                      (hermes-dashboard-transport--handle-frame active reply)
                      (funcall (plist-get old-request :reject) "late failure")
                      (should (= count (length frames)))
                      (should (equal before (buffer-string))))))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-history-acquisition-failure-retains-exact-retry ()
  ;; Exercise real acquire/start/auth, readiness, RPC and composer boundaries.
  (dolist (failure '(rejected error quit session lifetime turn request client))
    (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
           (hermes-dashboard-transport-url "http://history.invalid:9999")
           (hermes-dashboard-transport-start-mode 'remote)
           (hermes-instances nil)
           (hermes-instance nil)
           (hermes-dashboard-transport-request-timeout nil)
           (hermes-dashboard-transport-ready-timeout nil)
           (hermes-dashboard-transport-heartbeat-interval nil)
           (hermes-dashboard-transport-idle-close-delay nil)
           (hermes-transport-send-function #'hermes-transport-send)
           fail-p buffer frames successor caught
           (dials 0)
           (hermes-dashboard-transport-websocket-open-function
            (lambda (_url _client) (list 'socket (cl-incf dials))))
           (hermes-dashboard-transport-websocket-send-function
            (lambda (_socket text)
              (let ((frame (hermes-dashboard-transport--decode-frame text)))
                (when (equal (hermes-transport--get frame 'method) "prompt.submit")
                  (should (string-match-p "historic literal" (buffer-string))))
                (when (member (hermes-transport--get frame 'method)
                              '("session.resume" "prompt.submit"))
                  (push frame frames))))))
      (cl-letf (((symbol-function 'hermes-dashboard-transport--remote-auth-async)
                 (lambda (&rest _)
                   (if (not fail-p)
                       (hermes--promise-resolved
                        '(:url "ws://history.invalid:9999/tui"
                          :redacted-url "ws://history.invalid:9999/tui"
                          :kind remote :reusable-p nil))
                     (insert "newer composer")
                     (pcase failure
                       ('session (setq hermes-chat--session-id "successor"))
                       ('lifetime (setq hermes-chat--lifecycle-generation
                                        (hermes-chat--next-lifetime-token)))
                       ('turn (hermes-chat--next-transport-generation))
                       ('request (setq hermes-chat--session-bootstrap
                                       (list :kind 'successor)))
                       ('client (setq hermes-chat--dashboard-client
                                      (hermes-test--dashboard-client))))
                     (setq successor hermes-chat--session-bootstrap)
                     (pcase failure
                       ('rejected (hermes--promise-rejected "Authentication unavailable"))
                       ('quit (signal 'quit '("Authentication cancelled")))
                       (_ (error "Authentication unavailable"))))))
                ((symbol-function 'websocket-close) #'ignore)
                ((symbol-function 'hermes-chat--warm-model-options) #'ignore)
                ((symbol-function 'hermes-notifications-notify) #'ignore))
        (cl-labels
            ((ready (client)
               (hermes-dashboard-transport--handle-frame
                client "{\"jsonrpc\":\"2.0\",\"method\":\"event\",\"params\":{\"type\":\"gateway.ready\",\"payload\":{}}}"))
             (reply (client frame result)
               (hermes-dashboard-transport--handle-frame
                client (hermes-dashboard-transport--encode-frame
                        `((jsonrpc . "2.0")
                          (id . ,(hermes-transport--get frame 'id))
                          (result . ,result))))))
          (unwind-protect
              (progn
                (setq buffer (hermes-chat-resume-session "stored"))
                (with-current-buffer buffer
                  (let ((client hermes-chat--dashboard-client))
                    (ready client)
                    (insert "first FIFO") (hermes-chat-send)
                    (insert "second FIFO") (hermes-chat-send)
                    (let ((old-request
                           (gethash (hermes-transport--get (car frames) 'id)
                                    (hermes-dashboard-transport-client-pending client))))
                      (hermes-dashboard-transport-stop
                       client "Lost" '(:type status :status "closed"))
                      (let ((owner hermes-chat--session-bootstrap))
                        (should (eq (plist-get owner :phase) 'failed))
                        (setq fail-p t)
                        (condition-case err (hermes-chat-send)
                          ((error quit) (setq caught err)))
                        (should caught)
                        (should (eq (car caught)
                                    (pcase failure
                                      ('quit 'quit) ('rejected 'user-error) (_ 'error))))
                        (should (equal (hermes-test--queued-contents)
                                       '("first FIFO" "second FIFO")))
                        (should (equal (hermes-chat-input-string) "newer composer"))
                        (should (= 1 (length frames)))
                        (if (memq failure '(session lifetime turn request client))
                            (should (eq successor hermes-chat--session-bootstrap))
                          (should (eq owner hermes-chat--session-bootstrap))
                          (should (eq (plist-get owner :phase) 'failed))
                          (setq fail-p nil)
                          ;; User clears the newer draft; empty Send retries only
                          ;; hydration, never bypassing it to replay the FIFO.
                          (hermes-chat--delete-input-tail)
                          (hermes-chat-send)
                          (should (= 1 (length frames)))
                          (let ((replacement hermes-chat--dashboard-client))
                            (should-not (eq replacement client))
                            (should (= 2 dials))
                            (ready replacement)
                            (should (= 2 (length frames)))
                            (should (equal (hermes-transport--get (car frames) 'method)
                                           "session.resume"))
                            (should (equal (hermes-transport--get
                                            (hermes-transport--get (car frames) 'params)
                                            'session_id) "stored"))
                            (let ((current hermes-chat--session-bootstrap)
                                  (before (buffer-string)))
                              (funcall (plist-get old-request :reject) "Late failure")
                              (funcall (plist-get old-request :resolve)
                                       '((session_id . "stale")))
                              (should (eq current hermes-chat--session-bootstrap))
                              (should (equal before (buffer-string))))
                            (insert "draft during hydration")
                            (reply replacement (car frames)
                                   '((session_id . "live") (stored_session_id . "stored")
                                     (messages . [((role . "user") (text . "historic literal"))])))
                            (should (= 3 (length frames)))
                            (should (equal (hermes-transport--get (car frames) 'method)
                                           "prompt.submit"))
                            (should (equal (hermes-transport--get
                                            (hermes-transport--get (car frames) 'params)
                                            'text) "first FIFO"))
                            (reply replacement (car frames) '((status . "streaming")))
                            (should (equal (hermes-test--queued-contents) '("second FIFO")))
                            (should (equal (hermes-chat-input-string) "draft during hydration"))
                            (should-not hermes-chat--session-bootstrap)
                            (should (= 1 (cl-count
                                          "historic literal" (hermes-chat--entries)
                                          :key (lambda (entry) (plist-get entry :content))
                                          :test #'equal))))))))))
            (when (buffer-live-p buffer) (kill-buffer buffer))
            (hermes-dashboard-transport-stop-all)))))))

(provide 'hermes-chat-history-tests)
;;; hermes-chat-history-tests.el ends here
