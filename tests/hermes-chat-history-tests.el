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

(ert-deftest hermes-chat-resume-history-uses-display-projection ()
  "Resumed history shows display fields, skips hidden rows, folds reasoning."
  (hermes-test-with-chat-buffer
   (hermes-chat--render-history hermes-test-display-history)
   (let ((entries (hermes-chat--entries)))
     (hermes-test-assert-display-history
      (mapconcat (lambda (entry) (or (plist-get entry :content) "")) entries "\n")
      t)
     (should (equal (mapcar (lambda (entry) (plist-get entry :role)) entries)
                    '(user commentary assistant commentary assistant)))
     (should-not (string-match-p "Visible reasoning" (buffer-string))))))

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
                 (setq args (hermes-test--released-create-args args))
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
                 (setq args (hermes-test--released-create-args args))
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
                (second-assistant (nth 4 entries)))
           (should (string-match-p "Offline — input kept"
                                   (plist-get (nth 2 entries) :content)))
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
                  (should (= (if (eq loss 'reconnect) 2 1) (length frames)))
                  (when (eq loss 'reconnect)
                    (should (equal (hermes-transport--get (car frames) 'method)
                                   "client.capabilities"))
                    (should (equal (hermes-transport--get (car frames) 'params)
                                   '((server_requests . t)))))
                  (let ((before (buffer-string)))
                    (funcall (plist-get old-request :resolve)
                             '((session_id . "stale") (messages . (((role . "user") (text . "stale history"))))))
                    (should (equal before (buffer-string))))
                  ;; Empty public Send explicitly starts one replacement read.
                  (hermes-chat-send)
                  (should (= (if (eq loss 'stop) 2 1) starts))
                  (should (eq hermes-chat--dashboard-client
                              (if (eq loss 'stop) replacement client)))
                  (should (= (if (eq loss 'reconnect) 3 2) (length frames)))
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

;;;; Session restore (desktop.el and warm-restart)

(defvar warm-restart-passive)
(defvar warm-restart-activate-functions)

(defmacro hermes-test--with-resume-stub (resumes &rest body)
  "Run BODY counting `session.resume' calls in RESUMES, a list variable."
  (declare (indent 1) (debug t))
  `(let ((default-directory "/tmp/emacs-hermes/"))
     (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                (lambda (&rest _) (hermes-test--dashboard-client)))
               ((symbol-function 'hermes-chat--dashboard-default-transport-p)
                (lambda () t))
               ((symbol-function 'hermes-dashboard-transport-session-resume)
                (lambda (_client sid &rest args)
                  (push (cons sid (plist-get args :profile)) ,resumes))))
       ,@body)))

(ert-deftest hermes-chat-desktop-save-carries-identity-and-draft ()
  "Desktop data holds the durable id, routing and draft, never the transcript."
  (let (resumes buffer)
    (hermes-test--with-resume-stub resumes
      (unwind-protect
          (with-current-buffer
              (setq buffer (hermes-chat-resume-session "sid-7" "Plans" "work"))
            (setq hermes-chat--title-manual-p t)
            (goto-char (point-max))
            (insert "half written")
            (backward-char 3)
            (should (eq desktop-save-buffer #'hermes-chat--desktop-save))
            (let ((data (hermes-chat--desktop-save "/tmp/")))
              (should (equal (car (read-from-string (prin1-to-string data))) data))
              (should (equal (plist-get data :session-id) "sid-7"))
              (should (equal (plist-get data :title) "Plans"))
              (should (plist-get data :title-manual-p))
              (should (equal (plist-get data :profile) "work"))
              (should (equal (plist-get data :draft) "half written"))
              (should (= (plist-get data :point-offset) 9))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-desktop-restore-passive-attaches-on-activation ()
  "Passive restore builds the chat offline; activation resumes it once."
  (let (resumes buffer)
    (hermes-test--with-resume-stub resumes
      (unwind-protect
          (let ((warm-restart-passive t)
                (warm-restart-activate-functions nil))
            (setq buffer (hermes-chat-desktop-restore
                          nil "*my chat*"
                          '(:version 1 :session-id "sid-9" :title "T"
                            :title-manual-p t :profile "work"
                            :draft "next question" :point-offset 4)))
            (with-current-buffer buffer
              (should (equal (buffer-name) "*my chat*"))
              (should (equal hermes-chat--session-id "sid-9"))
              (should (equal hermes-chat--profile "work"))
              (should hermes-chat--title-manual-p)
              (should (equal (hermes-chat-input-string) "next question"))
              (should-not hermes-chat--dashboard-client))
            (should-not resumes)
            (should (= (length warm-restart-activate-functions) 1))
            (mapc #'funcall warm-restart-activate-functions)
            (should (equal resumes '(("sid-9" . "work"))))
            (with-current-buffer buffer
              (should (equal (hermes-chat-input-string) "next question"))
              (should (= (- (point) (hermes-chat--input-position)) 4))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-desktop-restore-without-session-starts-fresh-chat ()
  "A chat that never created a session restores as a draft with no resume."
  (let (resumes buffer)
    (hermes-test--with-resume-stub resumes
      (unwind-protect
          (let ((warm-restart-passive t)
                (warm-restart-activate-functions nil))
            (setq buffer (hermes-chat-desktop-restore
                          nil "*fresh*" '(:version 1 :draft "hello")))
            (mapc #'funcall warm-restart-activate-functions)
            (should-not resumes)
            (with-current-buffer buffer
              (should-not hermes-chat--session-id)
              (should (equal (hermes-chat-input-string) "hello"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-warm-restart-blocks-unsettled-chats ()
  "Active turns and queued input block a hand-over; an idle chat does not."
  (hermes-test-with-chat-buffer
    (should-not (hermes-chat--warm-restart-blocker))
    (should (memq #'hermes-chat--warm-restart-blocker
                  warm-restart-blocker-functions))
    (let ((hermes-chat--pending-assistant-id "a1"))
      (should (equal (hermes-chat--warm-restart-blocker)
                     "Hermes turn in progress")))
    (let ((hermes-chat--queued-messages '((:content "x"))))
      (should (hermes-chat--warm-restart-blocker)))))

(defvar warm-restart-regenerable)
(defvar warm-restart-blocker-functions)
(require 'hermes-kanban)
(require 'hermes-profiles)
(require 'hermes-endpoints)

(ert-deftest hermes-buffer-claim-marks-views-regenerable ()
  "Claimed views are marked regenerable per buffer, never per mode.
Chats, with their own desktop data, are not marked at all."
  (with-temp-buffer
    (hermes-dashboard-mode)
    (hermes-buffer--claim 'hermes-dashboard-mode)
    (should (local-variable-p 'warm-restart-regenerable))
    (should (stringp warm-restart-regenerable))
    (should-not (get 'hermes-dashboard-mode 'warm-restart-regenerable)))
  (hermes-test-with-chat-buffer
    (should-not (local-variable-p 'warm-restart-regenerable))
    (should-not (get 'hermes-chat-mode 'warm-restart-regenerable))))

(ert-deftest hermes-buffer-claim-never-marks-a-reused-generic-mode ()
  "A Hermes diff view does not make a user's own diff buffer droppable."
  (let ((view (hermes-buffer--get "*Hermes Diff test*" #'diff-mode t)))
    (unwind-protect
        (with-temp-buffer
          (diff-mode)
          (insert "my own patch")
          (should (buffer-local-value 'warm-restart-regenerable view))
          (should-not (local-variable-p 'warm-restart-regenerable))
          (should-not (get 'diff-mode 'warm-restart-regenerable)))
      (kill-buffer view))))

(ert-deftest hermes-buffer-editors-block-warm-restart-with-unsaved-edits ()
  "Edited Kanban bodies and SOUL drafts block a warm restart; drafts always."
  (dolist (mode '(hermes-kanban-body-mode hermes-profiles-soul-mode))
    (with-temp-buffer
      (funcall mode)
      (hermes-buffer--claim mode)
      (should-not (get mode 'warm-restart-regenerable))
      (should (memq #'hermes-buffer--warm-restart-blocker warm-restart-blocker-functions))
      (should-not (hermes-buffer--warm-restart-blocker))
      (insert "unsaved user text")
      (should (string-match-p "unsaved" (hermes-buffer--warm-restart-blocker)))))
  (with-temp-buffer
    (hermes-endpoint-edit-mode)
    (hermes-buffer--claim 'hermes-endpoint-edit-mode)
    (should (string-match-p "draft" (hermes-buffer--warm-restart-blocker)))))

;;; Editing earlier turns and rewind reconciliation

(defconst hermes-test-rewind-history
  '(((role . "user") (text . "First question") (row_id . 11))
    ((role . "assistant") (text . "First answer") (row_id . 12))
    ((role . "user") (text . "Second question") (row_id . 13))
    ((role . "assistant") (text . "Second answer") (row_id . 14))
    ((role . "user") (text . "Third question") (row_id . 15))
    ((role . "assistant") (text . "Third answer") (row_id . 16)))
  "Released `session.history' rows with durable row ids.")

(defmacro hermes-test-with-rewind-chat (spec &rest body)
  "Run BODY in a chat attached to a fake session showing rewind history.
SPEC is (SUBMITS HISTORY-READS PROMPTS): variables collecting prompt.submit
argument lists, `session.history' resolvers and y-or-n-p prompts."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,submits ,reads ,prompts) spec))
    `(let ((client (hermes-test--dashboard-client)) callback
           ,submits ,reads ,prompts (answer t))
       (ignore callback)
       (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                  (lambda (&rest args)
                    (setq callback (plist-get args :callback))
                    client))
                 ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                  (lambda (_client text &rest args)
                    (push (cons text args) ,submits)
                    (funcall (plist-get args :resolve)
                             '((status . "streaming") (user_row_id . 21)))))
                 ((symbol-function 'hermes-dashboard-transport-session-history)
                  (lambda (_client session &rest args)
                    (should (equal session "sid-rewind"))
                    (push args ,reads)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (prompt) (push prompt ,prompts) answer)))
         (hermes-test-with-chat-buffer
           (setq hermes-chat--dashboard-client client
                 hermes-chat--dashboard-active-session-id "sid-rewind"
                 hermes-chat--dashboard-session-ready-p t)
           (hermes-chat--render-history hermes-test-rewind-history)
           ,@body)))))

(defun hermes-test--goto-entry (text)
  "Move point onto the transcript entry showing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (backward-char 2))

(defun hermes-test--finish-turn (_callback client)
  "Complete the active turn and report CLIENT's session idle."
  (hermes-chat--handle-transport-event
   hermes-chat--pending-assistant-id '(:type done :session-id "sid-rewind"))
  (hermes-test--emit-dashboard-idle client "sid-rewind")
  (should-not (hermes-chat--active-turn-p)))

(defun hermes-test--user-row-ids ()
  "Return the row ids of user entries in transcript order."
  (mapcar (lambda (entry) (plist-get entry :row-id))
          (ewoc-collect hermes-chat--ewoc
                        (lambda (entry) (eq (plist-get entry :role) 'user)))))

(ert-deftest hermes-transport-history-row-id-addresses-plain-user-rows ()
  "Only plain user rows with a positive integer row id are addressable."
  (should (eql (hermes-transport-history-row-id
                '((role . "user") (text . "hi") (row_id . 7)))
               7))
  (dolist (message '(((role . "assistant") (text . "hi") (row_id . 7))
                     ((role . "user") (text . "/demo") (row_id . 7)
                      (display_kind . "skill_invocation"))
                     ((role . "user") (text . "raw") (row_id . 7)
                      (display_content . "shown"))
                     ((role . "user") (text . "hi") (row_id . "7"))
                     ((role . "user") (text . "hi") (row_id . 0))
                     ((role . "user") (text . "hi"))))
    (should-not (hermes-transport-history-row-id message))))

(ert-deftest hermes-dashboard-prompt-submit-sends-truncation-wire-params ()
  "Truncation keys reach the wire in snake case; ordinary submits omit them."
  (let (params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client _method request &rest _) (push request params))))
      (hermes-dashboard-transport-prompt-submit
       'client "edited" :session-id "sid" :truncate-before-row-id 13
       :confirm-truncate t :confirm-deep-truncate t)
      (hermes-dashboard-transport-prompt-submit 'client "plain" :session-id "sid"))
    (should (equal (sort (mapcar #'car (cadr params)) #'string<)
                   '(confirm_deep_truncate confirm_truncate session_id text
                     truncate_before_row_id)))
    (should (eql (alist-get 'truncate_before_row_id (cadr params)) 13))
    (should (eq (alist-get 'confirm_truncate (cadr params)) t))
    (should (equal (sort (mapcar #'car (car params)) #'string<)
                   '(session_id text)))))

(ert-deftest hermes-chat-rewind-keeps-history-and-live-row-ids ()
  "History and accepted live user turns keep their durable identity."
  (hermes-test-with-rewind-chat (submits _reads _prompts)
    (should (equal (hermes-test--user-row-ids) '(11 13 15)))
    (goto-char (point-max))
    (insert "Fourth question")
    (hermes-chat-send)
    (should (equal (caar submits) "Fourth question"))
    ;; An ordinary submit never carries truncation parameters.
    (dolist (key '(:truncate-before-row-id :confirm-truncate
                   :confirm-deep-truncate :confirm-empty-truncate))
      (should-not (plist-get (cdar submits) key)))
    (should (equal (hermes-test--user-row-ids) '(11 13 15 21)))))

(ert-deftest hermes-chat-edit-turn-resends-with-confirmed-truncation ()
  "Editing a turn sends its row id and the confirmations the user gave."
  (hermes-test-with-rewind-chat (submits _reads prompts)
    (goto-char (point-max))
    (insert "my draft")
    (hermes-test--goto-entry "Second question")
    (call-interactively (key-binding (kbd "e")))
    (should (equal (hermes-chat-input-string) "Second question"))
    (should (string-match-p "editing" (hermes-chat--mode-line-process)))
    (goto-char (point-max))
    (insert " (revised)")
    (call-interactively (key-binding (kbd "RET")))
    (should (equal (car prompts)
                   "Replace this message, removing 1 later message? "))
    (pcase-let ((`(,text . ,args) (car submits)))
      (should (equal text "Second question (revised)"))
      (should (eql (plist-get args :truncate-before-row-id) 13))
      (should (eq (plist-get args :confirm-truncate) t))
      (should (eq (plist-get args :confirm-deep-truncate) t))
      (should-not (plist-get args :confirm-empty-truncate)))
    (should-not hermes-chat--edit-turn)
    (should (string-empty-p (hermes-chat-input-string)))))

(ert-deftest hermes-chat-edit-turn-confirms-scope ()
  "The first turn needs empty consent; the last needs neither deep nor empty."
  (dolist (case '(("First question" 11 "removing 2 later messages" nil t)
                  ("Third question" 15 nil nil nil)))
    (pcase-let ((`(,text ,row-id ,phrase ,deep ,empty) case))
      (hermes-test-with-rewind-chat (submits _reads prompts)
        (hermes-test--goto-entry text)
        (hermes-chat-edit-turn)
        (hermes-chat-send)
        (should (equal (car prompts)
                       (format "Replace this message%s? "
                               (if phrase (concat ", " phrase) ""))))
        (let ((args (cdar submits)))
          (should (eql (plist-get args :truncate-before-row-id) row-id))
          (should (eq (plist-get args :confirm-deep-truncate) (or deep (and phrase t))))
          (should (eq (plist-get args :confirm-empty-truncate) empty)))))))

(ert-deftest hermes-chat-edit-turn-declined-keeps-edit ()
  "Declining the confirmation sends nothing and keeps the edit open."
  (hermes-test-with-rewind-chat (submits _reads _prompts)
    (setq answer nil)
    (hermes-test--goto-entry "Second question")
    (hermes-chat-edit-turn)
    (hermes-chat-send)
    (should-not submits)
    (should hermes-chat--edit-turn)
    (should (equal (hermes-chat-input-string) "Second question"))))

(ert-deftest hermes-chat-edit-turn-cancel-restores-draft ()
  "Escape cancels editing and restores the earlier draft."
  (hermes-test-with-rewind-chat (_submits _reads _prompts)
    (goto-char (point-max))
    (insert "my draft")
    (should-not (key-binding (kbd "<escape>")))
    (hermes-test--goto-entry "Third question")
    (hermes-chat-edit-turn)
    ;; Choosing another turn keeps the original draft.
    (hermes-test--goto-entry "First question")
    (hermes-chat-edit-turn)
    (should (equal (hermes-chat-input-string) "First question"))
    (call-interactively (key-binding (kbd "<escape>")))
    (should-not hermes-chat--edit-turn)
    (should (equal (hermes-chat-input-string) "my draft"))
    (should-not (hermes-chat--mode-line-process))))

(ert-deftest hermes-chat-edit-turn-popup-group-follows-point ()
  "The Message group appears only on your own message or while editing."
  (hermes-test-with-rewind-chat (_submits _reads _prompts)
    (let ((message-keys
           (lambda ()
             (mapcar (lambda (entry) (plist-get entry :key))
                     (plist-get (seq-find
                                 (lambda (group) (equal (plist-get group :name) "Message"))
                                 (apply #'append
                                        (keymap-popup--meta hermes-chat-actions-map
                                                            'descriptions)))
                                :entries))))
          (visible-p (lambda ()
                       (string-match-p "Edit and resend"
                                       (keymap-popup--render
                                        (keymap-popup--meta hermes-chat-actions-map
                                                            'descriptions))))))
      (should (equal (funcall message-keys) '("C" "U" "Z")))
      (goto-char (point-max))
      (should-not (funcall visible-p))
      (hermes-test--goto-entry "Second answer")
      (should-not (funcall visible-p))
      (hermes-test--goto-entry "Second question")
      (should (funcall visible-p))
      (should (eq (keymap-lookup hermes-chat-actions-map "C") #'hermes-chat-edit-turn))
      (should-not (string-match-p "Cancel edit"
                                  (keymap-popup--render
                                   (keymap-popup--meta hermes-chat-actions-map
                                                       'descriptions))))
      (hermes-chat-edit-turn)
      (goto-char (point-max))
      (should (string-match-p "Cancel edit"
                              (keymap-popup--render
                               (keymap-popup--meta hermes-chat-actions-map
                                                   'descriptions)))))))

(ert-deftest hermes-chat-edit-turn-refusals ()
  "Editing is refused while busy, off a user turn, or without an address."
  (hermes-test-with-rewind-chat (submits _reads _prompts)
    (hermes-test--goto-entry "Second answer")
    (should-error (hermes-chat-edit-turn) :type 'user-error)
    (goto-char (point-max))
    (should-error (hermes-chat-edit-turn) :type 'user-error)
    (hermes-test--goto-entry "Second question")
    (setq hermes-chat--dashboard-running-p t)
    (should (string-match-p
             "wait for this turn"
             (cadr (should-error (hermes-chat-edit-turn) :type 'user-error))))
    (should (hermes-chat--rewind-unavailable-p))
    (setq hermes-chat--dashboard-running-p nil)
    (should-not (hermes-chat--rewind-unavailable-p))
    (hermes-chat-edit-turn)
    ;; A turn that starts after editing began also refuses the resend.
    (setq hermes-chat--dashboard-running-p t)
    (goto-char (point-max))
    (should-error (hermes-chat-send) :type 'user-error)
    (setq hermes-chat--dashboard-running-p nil)
    (hermes-chat--render-history
     '(((role . "user") (text . "/demo") (display_kind . "skill_invocation")
        (row_id . 31))))
    (hermes-test--goto-entry "/demo")
    (should-error (hermes-chat-edit-turn) :type 'user-error)
    (should-not submits)))

(ert-deftest hermes-chat-edit-turn-stale-session-refuses ()
  "An edit begun in one session never truncates its successor."
  (hermes-test-with-rewind-chat (submits _reads _prompts)
    (hermes-test--goto-entry "Second question")
    (hermes-chat-edit-turn)
    (setq hermes-chat--dashboard-active-session-id "successor")
    (goto-char (point-max))
    (should-error (hermes-chat-send) :type 'user-error)
    (should-not submits)
    (should-not hermes-chat--edit-turn)
    (should (equal (hermes-chat-input-string) "Second question"))))

(ert-deftest hermes-chat-restore-turn-reruns-unchanged-text ()
  "Rerun from here resends the turn's own text with truncation."
  (hermes-test-with-rewind-chat (submits _reads prompts)
    (hermes-test--goto-entry "Second question")
    (hermes-chat-restore-turn)
    (should (equal (car prompts)
                   "Rerun from this message, removing 1 later message? "))
    (should (equal (caar submits) "Second question"))
    (should (eql (plist-get (cdar submits) :truncate-before-row-id) 13))))

(ert-deftest hermes-chat-edit-turn-reconciles-after-turn ()
  "A truncating submit repaints from history once its turn ends."
  (hermes-test-with-rewind-chat (_submits reads _prompts)
    (hermes-test--goto-entry "Second question")
    (hermes-chat-edit-turn)
    (goto-char (point-max))
    (delete-region (hermes-chat--input-position) (point-max))
    (insert "Better question")
    (hermes-chat-send)
    ;; The turn is running: the stale history is not read yet.
    (should-not reads)
    (hermes-test--finish-turn callback client)
    (should (= (length reads) 1))
    (insert "next draft")
    (backward-char 3)
    (funcall (plist-get (car reads) :resolve)
             `((messages . ,(append (seq-take hermes-test-rewind-history 2)
                                    '(((role . "user") (text . "Better question")
                                       (row_id . 21))
                                      ((role . "assistant") (text . "Better answer")))))))
    (let ((text (buffer-string)))
      (should-not (string-match-p "Second question\\|Third question" text))
      (should (string-match-p "Better answer" text)))
    (should (equal (hermes-test--user-row-ids) '(11 21)))
    (should (equal (hermes-chat-input-string) "next draft"))
    (should (= (- (point) (hermes-chat--input-position)) 7))))

(ert-deftest hermes-chat-undo-reconciles-transcript ()
  "After /undo the transcript repaints from history, keeping the notice."
  (hermes-test-with-rewind-chat (_submits reads _prompts)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :resolve)
                          '((type . "prefill") (message . "Third question")
                            (notice . "Undid 1 turn"))))))
      (goto-char (point-max))
      (insert "/undo")
      (hermes-chat-send)
      (should (= (length reads) 1))
      (funcall (plist-get (car reads) :resolve)
               `((messages . ,(seq-take hermes-test-rewind-history 4))))
      (let ((text (buffer-string)))
        (should (string-match-p "Second answer" text))
        (should-not (string-match-p "Third answer" text))
        (should (string-match-p "Undid 1 turn" text)))
      (should (equal (hermes-chat-input-string) "Third question")))))

(ert-deftest hermes-chat-retry-reconciles-after-resent-turn ()
  "After /retry the resent turn runs, then history replaces the old copy."
  (hermes-test-with-rewind-chat (submits reads _prompts)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :resolve)
                          '((type . "send") (message . "Third question"))))))
      (goto-char (point-max))
      (insert "/retry")
      (hermes-chat-send)
      (should (equal (caar submits) "Third question"))
      ;; Any read that raced the resent turn is stale and is read again.
      (when reads
        (funcall (plist-get (pop reads) :resolve)
                 `((messages . ,(seq-take hermes-test-rewind-history 4)))))
      (should (string-match-p "Third answer" (buffer-string)))
      (should-not reads)
      (hermes-test--finish-turn callback client)
      (should (= (length reads) 1))
      (funcall (plist-get (car reads) :resolve)
               `((messages . ,(append (seq-take hermes-test-rewind-history 4)
                                      '(((role . "user") (text . "Third question")
                                         (row_id . 21))
                                        ((role . "assistant") (text . "New answer")))))))
      (let ((text (buffer-string)))
        (should-not (string-match-p "Third answer" text))
        (should (string-match-p "New answer" text)))
      (should (equal (hermes-test--user-row-ids) '(11 13 21))))))

(ert-deftest hermes-chat-rewind-reconcile-stale-owners-stay-quiet ()
  "A history read cannot repaint a successor session, lifetime or client."
  (dolist (change '(session lifetime client))
    (hermes-test-with-rewind-chat (_submits reads _prompts)
      (hermes-chat--request-history-reconcile)
      (should (= (length reads) 1))
      (pcase change
        ('session (setq hermes-chat--dashboard-active-session-id "successor"))
        ('lifetime (setq hermes-chat--lifecycle-generation
                         (hermes-chat--next-lifetime-token)))
        ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))))
      (let ((before (buffer-string)))
        (funcall (plist-get (car reads) :resolve) '((messages)))
        (funcall (plist-get (car reads) :reject) "late failure")
        (should (equal (buffer-string) before))))))

(ert-deftest hermes-chat-rewind-reconcile-failure-keeps-transcript ()
  "A failed read reports itself and deletes nothing."
  (hermes-test-with-rewind-chat (_submits reads _prompts)
    (hermes-chat--request-history-reconcile)
    (funcall (plist-get (car reads) :reject) "socket closed")
    (let ((text (buffer-string)))
      (should (string-match-p "Third answer" text))
      (should (string-match-p "Could not refresh the transcript: socket closed" text)))
    (should-not hermes-chat--history-reconcile)))

(ert-deftest hermes-chat-edit-turn-refuses-staged-images ()
  "Send with staged images while editing refuses and keeps the edit."
  (hermes-test-with-rewind-chat (submits _reads _prompts)
    (hermes-test--goto-entry "Second question")
    (hermes-chat-edit-turn)
    (goto-char (point-max))
    (insert " revised")
    (setq hermes-chat--draft-images '(image-fixture))
    (let ((hermes-chat--image-prior-submits (make-hash-table :test #'equal))
          (hermes-chat--image-session-blocks (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'hermes-chat--queue-image-draft)
                 (lambda () (error "Images must not be sent while editing"))))
        (should (string-match-p
                 "images"
                 (cadr (should-error (hermes-chat-send) :type 'user-error))))))
    (should-not submits)
    (should hermes-chat--edit-turn)
    (should (equal hermes-chat--draft-images '(image-fixture)))
    (should (equal (hermes-chat-input-string) "Second question revised"))))

(provide 'hermes-chat-history-tests)
;;; hermes-chat-history-tests.el ends here
