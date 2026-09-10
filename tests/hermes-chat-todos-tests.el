;;; hermes-chat-todos-tests.el --- Live todo projection tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hermes-chat)
(require 'hermes-chat-todos)
(require 'hermes-test-helpers)

(defun hermes-chat-todos-test--events (type payload &optional session)
  "Normalize TYPE and PAYLOAD for SESSION through the real transport."
  (hermes-dashboard-transport--normalize-event-frame
   `(:method "event" :params (:type ,type :session_id ,(or session "runtime")
                                  :payload ,payload))))

(defun hermes-chat-todos-test--send (payload &optional type session)
  "Apply normalized PAYLOAD of TYPE in SESSION to the current chat."
  (dolist (event (hermes-chat-todos-test--events
                  (or type "todo.updated") payload session))
    (hermes-chat--handle-transport-event hermes-chat--pending-assistant-id event)))

(defun hermes-chat-todos-test--start (&optional id)
  "Start an isolated chat turn with ID."
  (let* ((user (hermes-chat--make-entry 'user "Question" 'done))
         (assistant (hermes-chat--make-entry 'assistant "" 'pending))
         (id (or id (plist-get assistant :id))))
    (setq assistant (plist-put assistant :id id))
    (hermes-chat--begin-pending-turn user assistant (list :assistant-id id))
    (setq hermes-chat--dashboard-active-session-id "runtime")
    id))

(defconst hermes-chat-todos-test--list
  '(:todos [(:id "a" :content "Inspect" :status "completed")
            (:id "b" :content "Verify" :status "in_progress")
            (:id "c" :content "Publish" :status "pending")
            (:id "d" :content "Abandoned" :status "cancelled")]
    :revision 1))

(defmacro hermes-chat-todos-test--chat (&rest body)
  "Run BODY with an isolated chat and clean up its owned panel."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (hermes-chat-mode)
     (hermes-chat-todos-test--start)
     ,@body))

(ert-deftest hermes-chat-todos-normalize-reduce-visible-panel ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (should-not hermes-chat--todos-panel)
      (hermes-chat-todos-test--send hermes-chat-todos-test--list)
      (should-not hermes-chat--todos-panel)
      (let ((chat (current-buffer)))
        (call-interactively #'hermes-chat-show-todos)
        (with-current-buffer (buffer-local-value 'hermes-chat--todos-panel chat)
          (should (derived-mode-p 'hermes-chat-todos-mode))
          (should buffer-read-only)
          (should (string-match-p "Active.*1/3" (buffer-string)))
          (should (string-match-p "\\[X\\] Inspect" (buffer-string)))
          (should (string-match-p "\\[-\\] Verify" (buffer-string)))
          (should (string-match-p "\\[ \\] Publish" (buffer-string)))
          (should (string-match-p "\\[/\\] Abandoned" (buffer-string)))
          (goto-char (point-min))
          (call-interactively (key-binding (kbd "n")))
          (should (> (point) (point-min)))
          (call-interactively (key-binding (kbd "p")))
          (should (= (point) (point-min))))))))

(ert-deftest hermes-chat-todos-empty-clears-and-old-revision-stays-cleared ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (hermes-chat-todos-test--send '(:todos [] :revision 2))
    (should (eq 'empty (plist-get hermes-chat--todos :status)))
    (should-not (plist-get hermes-chat--todos :items))
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (should-not (plist-get hermes-chat--todos :items))))

(ert-deftest hermes-chat-todos-malformed-errors-and-revisions-are-inert ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (let ((before hermes-chat--todos))
      (dolist (bad '((:todos [] :revision "2") (:todos [] :revision -1)
                     (:todos [] :revision 1.5) (:todos [] :revision nil)
                     (:todos [(:id "x" :content "Broken" :status "mystery")] :revision 9)
                     (:todos [] :revision 9 :error "failed")
                     (:todos nil :revision 9) (:revision 9)))
        (hermes-chat-todos-test--send bad)
        (should (equal before hermes-chat--todos))))))

(ert-deftest hermes-chat-todos-legacy-result-not-args-or-prose ()
  (hermes-chat-todos-test--chat
    (dolist (name '("todo" "todo_list"))
      (hermes-chat-todos-test--send
       `(:name ,name :args (:todos []) :result "{\"todos\":[{\"id\":\"a\",\"content\":\"Legacy\",\"status\":\"pending\"}]}")
       "tool.complete")
      (should (equal "Legacy" (plist-get (car (plist-get hermes-chat--todos :items)) :content))))
    (let ((before hermes-chat--todos))
      (dolist (payload '((:name "todo" :args (:todos []) :result "Updated tasks")
                          (:name "terminal" :result (:todos []))
                          (:name "todo" :result (:todos [] :error "failed"))))
        (hermes-chat-todos-test--send payload "tool.complete")
        (should (equal before hermes-chat--todos))))))

(ert-deftest hermes-chat-todos-terminal-and-next-turn-preserve-unfinished ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (hermes-chat-todos-test--send '(:status "complete") "message.complete")
    (should (eq 'settled (plist-get hermes-chat--todos :status)))
    (should (equal "pending" (plist-get (nth 2 (plist-get hermes-chat--todos :items)) :status)))
    (hermes-chat-todos-test--start "next")
    (should (eq 'settled (plist-get hermes-chat--todos :status)))
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (should (eq 'settled (plist-get hermes-chat--todos :status)))
    (hermes-chat-todos-test--send '(:todos [(:id "next" :content "Next task" :status "pending")] :revision 2))
    (should (eq 'active (plist-get hermes-chat--todos :status)))))

(ert-deftest hermes-chat-todos-session-and-late-turn-events-are-inert ()
  (hermes-chat-todos-test--chat
    (let ((old hermes-chat--pending-assistant-id))
      (hermes-chat-todos-test--send hermes-chat-todos-test--list)
      (let ((before hermes-chat--todos))
        (hermes-chat-todos-test--send '(:todos [] :revision 20) nil "other")
        (should (equal before hermes-chat--todos))
        (should (equal "runtime" hermes-chat--dashboard-active-session-id)))
      (hermes-chat-todos-test--start "next")
      (let ((before hermes-chat--todos))
        (dolist (event (hermes-chat-todos-test--events "todo.updated" '(:todos [] :revision 20)))
          (hermes-chat--handle-transport-event old event))
        (should (equal before hermes-chat--todos))))))

(ert-deftest hermes-chat-todos-suppressed-resume-routes-only-task-snapshots ()
  "Public submit/resume keeps tasks current without adopting the old transcript."
  (save-window-excursion
    (dolist (case '(("todo.updated" nil done)
                    ("tool.complete" "todo" error)
                    ("tool.complete" "todo_list" done)))
      (let ((client (hermes-test--dashboard-client))
            (hermes-transport-send-function #'hermes-transport-send)
            callback resume)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                   (lambda (&rest args)
                     (setq callback (plist-get args :callback))
                     client))
                  ((symbol-function 'hermes-dashboard-transport-session-resume)
                   (lambda (_client session &rest args)
                     (should (equal session "stored"))
                     (setq resume (plist-get args :resolve))))
                  ((symbol-function 'hermes-dashboard-transport-session-create)
                   (lambda (&rest _) (ert-fail "Unexpected session.create")))
                  ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (&rest _) (ert-fail "Retry must remain unsubmitted")))
                  ((symbol-function 'hermes-transport-send)
                   (lambda (&rest _) (ert-fail "Unexpected CLI fallback"))))
          (hermes-test-with-chat-buffer
           (setq hermes-chat--session-id "stored")
           (insert "Unsubmitted retry")
           (hermes-chat-send)
           ;; Resolve after public submission returns, through its real owner.
           (should resume)
           (funcall resume `(:session_id "runtime" :running t
                            :todo_state ,hermes-chat-todos-test--list))
           (should hermes-chat--dashboard-suppress-stream-p)
           (should-not hermes-chat--dashboard-stream-assistant-id)
           (should (= 1 (plist-get hermes-chat--todos :revision)))
           (hermes-chat-show-todos)
           (buffer-enable-undo)
           (insert "Unsent draft")
           (let* ((text (buffer-string))
                  (point (point))
                  (undo buffer-undo-list)
                  (window (selected-window))
                  (clear (if (cadr case)
                             `(:name ,(cadr case)
                               :result "{\"todos\":[],\"revision\":2}")
                           '(:todos [] :revision 2)))
                  (late (if (cadr case)
                            `(:name ,(cadr case) :result (:todos [] :revision 99))
                          '(:todos [] :revision 99)))
                  (update (lambda (payload &optional runtime)
                            (dolist (event (hermes-chat-todos-test--events
                                           (car case) payload runtime))
                              (funcall callback event)))))
             ;; Both snapshot sources retain the runtime fence.
             (funcall update clear "other")
             (should (= 1 (plist-get hermes-chat--todos :revision)))
             (funcall update clear)
             (should (= 2 (plist-get hermes-chat--todos :revision)))
             (should-not (plist-get hermes-chat--todos :items))
             (should (string-match-p
                      "No task list"
                      (with-current-buffer hermes-chat--todos-panel (buffer-string))))
             ;; Tool completion's sibling tool event must still be withheld.
             (dolist (event '((:type delta :content "Old assistant text")
                              (:type interim :content "Old interim text")
                              (:type tool :name "terminal" :content "Old tool text")))
               (funcall callback (append event '(:session-id "runtime"))))
             (should (equal text (buffer-string)))
             (should (equal "Unsent draft" (hermes-chat-input-string)))
             (should hermes-chat--dashboard-suppress-stream-p)
             ;; A newer unfinished snapshot settles without completing its items.
             (funcall update
                      (if (cadr case)
                          `(:name ,(cadr case)
                            :result (:todos [(:id "p" :content "Still pending"
                                              :status "pending")] :revision 3))
                        '(:todos [(:id "p" :content "Still pending"
                                   :status "pending")] :revision 3)))
             (should (= 3 (plist-get hermes-chat--todos :revision)))
             (should (eq 'active (plist-get hermes-chat--todos :status)))
             (should (string-match-p
                      (regexp-quote "[ ] Still pending")
                      (with-current-buffer hermes-chat--todos-panel (buffer-string))))
             (should (equal text (buffer-string)))
             (should (= point (point)))
             (should (eq undo buffer-undo-list))
             (should (eq window (selected-window)))
             (should (eq buffer (current-buffer)))
             (funcall callback (list :type (nth 2 case) :session-id "runtime"
                                    :content "Old terminal text"))
             (should-not hermes-chat--pending-assistant-id)
             (should-not hermes-chat--dashboard-suppress-stream-p)
             (should-not (plist-get hermes-chat--todos :accepting))
             (should (eq 'settled (plist-get hermes-chat--todos :status)))
             (should (equal "pending" (plist-get
                                       (car (plist-get hermes-chat--todos :items))
                                       :status)))
             (should-not (string-match-p "Old terminal text" (buffer-string)))
             (should (equal "Unsent draft" (hermes-chat-input-string)))
             (let ((settled hermes-chat--todos))
               (funcall update late)
               (should (equal settled hermes-chat--todos)))
             ;; A replaced callback cannot mutate a newly accepting successor.
             (hermes-chat--next-transport-generation)
             (hermes-chat-todos-test--start "successor")
             (let ((before hermes-chat--todos) (text (buffer-string)))
               (funcall update late)
               (funcall callback '(:type done :session-id "runtime"))
               (should (equal before hermes-chat--todos))
               (should (equal text (buffer-string)))
               (should (equal "successor" hermes-chat--pending-assistant-id))))))))))

(ert-deftest hermes-chat-todos-popup-command-is-discoverable ()
  (hermes-chat-todos-test--chat
    (should (eq #'hermes-chat-show-todos (lookup-key hermes-chat-work-map (kbd "T"))))
    (should (eq #'hermes-chat-show-todos (lookup-key hermes-chat-actions-map (kbd "T"))))))

(ert-deftest hermes-chat-todos-wire-empty-null-and-false-are-distinct ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (let ((before hermes-chat--todos))
      (dolist (value '("null" "false"))
        (dolist (event (hermes-dashboard-transport--normalize-event-frame
                       (hermes-dashboard-transport--decode-frame
                        (format "{\"method\":\"event\",\"params\":{\"type\":\"todo.updated\",\"session_id\":\"runtime\",\"payload\":{\"todos\":%s,\"revision\":2}}}" value))))
          (hermes-chat--handle-transport-event hermes-chat--pending-assistant-id event))
        (should (equal before hermes-chat--todos))))
    (dolist (event (hermes-dashboard-transport--normalize-event-frame
                   (hermes-dashboard-transport--decode-frame
                    "{\"method\":\"event\",\"params\":{\"type\":\"tool.complete\",\"session_id\":\"runtime\",\"payload\":{\"name\":\"todo_list\",\"todos\":[],\"revision\":2}}}")))
      (hermes-chat--handle-transport-event hermes-chat--pending-assistant-id event))
    (should (eq 'empty (plist-get hermes-chat--todos :status)))))

(ert-deftest hermes-chat-todos-updates-preserve-composer-and-transcript ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (switch-to-buffer (current-buffer))
      (let ((chat (current-buffer)))
        (hermes-chat-show-todos)
        (goto-char (point-max))
        (insert "Unsent α draft")
        (backward-char 3)
        (let ((text (buffer-string)) (point (point)) (undo buffer-undo-list)
              (window (selected-window)))
          (hermes-chat-todos-test--send hermes-chat-todos-test--list)
          (should (eq chat (current-buffer)))
          (should (eq window (selected-window)))
          (should (= point (point)))
          (should (eq undo buffer-undo-list))
          (should (equal text (buffer-string)))
          (should (equal "Unsent α draft" (hermes-chat-input-string)))
          ;; Repeat while another window is selected and the chat is visible.
          (select-window (get-buffer-window hermes-chat--todos-panel))
          (with-current-buffer chat
            (hermes-chat-todos-test--send '(:todos [] :revision 2))
            (should (equal text (buffer-string)))
            (should (eq undo buffer-undo-list))
            (should (= point (point)))))))))

(ert-deftest hermes-chat-todos-disconnect-and-same-runtime-reconnect ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (hermes-chat-show-todos)
      (hermes-chat-todos-test--send hermes-chat-todos-test--list)
      (let ((turn hermes-chat--pending-assistant-id))
        (hermes-chat--handle-transport-event turn '(:type status :status "closed"))
        (should (eq 'stale (plist-get hermes-chat--todos :status)))
        (should (string-match-p "Disconnected / stale"
                                (with-current-buffer hermes-chat--todos-panel (buffer-string))))
        (setq hermes-chat--dashboard-active-session-id "runtime")
        (hermes-chat--dashboard-restore-inflight-turn nil)
        (should (equal turn (plist-get hermes-chat--todos :turn)))
        (should (= 1 (plist-get hermes-chat--todos :revision)))
        (hermes-chat-todos-test--send hermes-chat-todos-test--list)
        (should (eq 'stale (plist-get hermes-chat--todos :status)))
        (hermes-chat-todos-test--send '(:todos [(:id "b" :content "Fresh" :status "pending")] :revision 2))
        (should (eq 'active (plist-get hermes-chat--todos :status)))))))

(ert-deftest hermes-chat-todos-reset-kill-and-mode-change-retire-panel ()
  (save-window-excursion
    (dolist (retire '(hermes-chat--reset-transcript fundamental-mode kill-buffer))
      (let ((chat (generate-new-buffer " *todos-owner*")) panel callback)
        (unwind-protect
            (with-current-buffer chat
              (hermes-chat-mode)
              (hermes-chat-todos-test--start)
              (setq callback (hermes-chat--transport-callback
                              chat hermes-chat--pending-assistant-id nil
                              hermes-chat--transport-generation)
                    panel (hermes-chat-show-todos))
              (hermes-chat-todos-test--send hermes-chat-todos-test--list)
              (funcall retire)
              (should-not (buffer-live-p panel))
              (funcall callback (car (hermes-chat-todos-test--events
                                     "todo.updated" hermes-chat-todos-test--list)))
              (when (buffer-live-p chat)
                (with-current-buffer chat (should-not hermes-chat--todos))))
          (when (buffer-live-p chat) (kill-buffer chat)))))))

(ert-deftest hermes-chat-todos-panel-repurpose-is-not-erased-or-killed ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (let ((panel (hermes-chat-show-todos)))
        (unwind-protect
            (progn
              (with-current-buffer panel
                (fundamental-mode)
                (setq buffer-read-only nil)
                (erase-buffer)
                (insert "Unrelated editable content"))
              (should-not hermes-chat--todos-panel)
              (hermes-chat-todos-test--send hermes-chat-todos-test--list)
              (hermes-chat--reset-transcript)
              (should (buffer-live-p panel))
              (should (equal "Unrelated editable content"
                             (with-current-buffer panel (buffer-string)))))
          (when (buffer-live-p panel) (kill-buffer panel)))))))

(ert-deftest hermes-chat-todos-two-chats-and-stale-generation-are-isolated ()
  (save-window-excursion
    (let ((a (generate-new-buffer " *todos-a*"))
          (b (generate-new-buffer " *todos-b*")) callback panel-a panel-b before)
      (unwind-protect
          (progn
            (dolist (buffer (list a b))
              (with-current-buffer buffer
                (hermes-chat-mode)
                (hermes-chat-todos-test--start)
                (hermes-chat-show-todos)))
            (with-current-buffer a
              (setq panel-a hermes-chat--todos-panel
                    callback (hermes-chat--transport-callback
                              a hermes-chat--pending-assistant-id nil hermes-chat--transport-generation))
              (hermes-chat-todos-test--send hermes-chat-todos-test--list))
            (with-current-buffer b
              (setq panel-b hermes-chat--todos-panel)
              (should-not (plist-get hermes-chat--todos :items)))
            (should-not (eq panel-a panel-b))
            (with-current-buffer a
              (hermes-chat--next-transport-generation)
              (setq before hermes-chat--todos))
            (with-current-buffer b
              (funcall callback (car (hermes-chat-todos-test--events "todo.updated" '(:todos [] :revision 20))))
              (should-not (plist-get hermes-chat--todos :items)))
            (should (equal before (buffer-local-value 'hermes-chat--todos a))))
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) (list a b))))))

(ert-deftest hermes-chat-todos-button-returns-to-exact-owner ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (let* ((chat (current-buffer)) (panel (hermes-chat-show-todos)))
        (switch-to-buffer panel)
        (goto-char (point-min))
        (button-activate (button-at (point)))
        (should (eq (current-buffer) chat))))))

(ert-deftest hermes-chat-todos-interim-rotation-keeps-list-current ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (hermes-chat-todos-test--send '(:text "Interim answer") "message.interim")
    (should (equal hermes-chat--pending-assistant-id (plist-get hermes-chat--todos :turn)))
    (hermes-chat-todos-test--send '(:todos [] :revision 2))
    (should (eq 'empty (plist-get hermes-chat--todos :status)))))

(ert-deftest hermes-chat-todos-resume-snapshot-empty-and-absent ()
  (save-window-excursion
    (hermes-chat-todos-test--chat
      (hermes-chat-show-todos)
      (let* ((frame (hermes-dashboard-transport--decode-frame
                     "{\"id\":\"resume\",\"result\":{\"session_id\":\"runtime\",\"running\":true,\"todo_state\":{\"todos\":[{\"id\":\"a\",\"content\":\"Resumed\",\"status\":\"pending\"}],\"revision\":7}}}"))
             (result (hermes-transport--get frame 'result)))
        (hermes-chat--dashboard-record-session nil result)
        (should (eq 'active (plist-get hermes-chat--todos :status)))
        (should (string-match-p "Resumed" (with-current-buffer hermes-chat--todos-panel (buffer-string))))
        ;; No snapshot means no claim, not a clear.
        (hermes-chat--dashboard-record-session nil '(:session_id "runtime" :running t))
        (should (= 7 (plist-get hermes-chat--todos :revision)))
        (should (plist-get hermes-chat--todos :items))
        ;; An idle restoration never advertises an unfinished old plan as active.
        (setf (alist-get 'running result) nil)
        (hermes-chat--dashboard-record-session nil result)
        (should (eq 'settled (plist-get hermes-chat--todos :status))))
      (let ((frame (hermes-dashboard-transport--decode-frame
                    "{\"id\":\"resume\",\"result\":{\"session_id\":\"runtime\",\"running\":false,\"todo_state\":{\"todos\":[],\"revision\":8}}}")))
        (hermes-chat--dashboard-record-session nil (hermes-transport--get frame 'result)))
      (should (= 8 (plist-get hermes-chat--todos :revision)))
      (should-not (plist-get hermes-chat--todos :items))
      (should-not (string-match-p "Resumed" (with-current-buffer hermes-chat--todos-panel (buffer-string)))))))

(ert-deftest hermes-chat-todos-new-runtime-retires-old-watermark ()
  (hermes-chat-todos-test--chat
    (hermes-chat-todos-test--send hermes-chat-todos-test--list)
    (hermes-chat--dashboard-record-session nil '(:session_id "successor" :running t))
    (should-not (plist-get hermes-chat--todos :items))
    (should-not (plist-get hermes-chat--todos :revision))
    (let ((before hermes-chat--todos))
      (hermes-chat-todos-test--send '(:todos [] :revision 99) nil "runtime")
      (should (equal before hermes-chat--todos)))
    (hermes-chat-todos-test--send hermes-chat-todos-test--list nil "successor")
    (should (eq 'active (plist-get hermes-chat--todos :status)))
    (should (= 1 (plist-get hermes-chat--todos :revision)))))

(ert-deftest hermes-chat-todos-shared-wire-routes-to-visible-owner ()
  (save-window-excursion
    (let ((a (generate-new-buffer " *todo-wire-a*"))
          (b (generate-new-buffer " *todo-wire-b*"))
          (client (make-hermes-dashboard-transport-client :refcount 2)))
      (unwind-protect
          (progn
            (dolist (buffer (list a b))
              (with-current-buffer buffer
                (hermes-chat-mode)
                (hermes-chat-todos-test--start)
                (setq hermes-chat--dashboard-client client
                      hermes-chat--dashboard-active-session-id (if (eq buffer a) "a" "b")
                      hermes-chat--dashboard-token
                      (hermes-dashboard-transport-subscribe
                       client (hermes-chat--transport-callback
                               buffer hermes-chat--pending-assistant-id t hermes-chat--transport-generation)))
                (hermes-dashboard-transport-subscribe-session
                 client hermes-chat--dashboard-token hermes-chat--dashboard-active-session-id)
                (hermes-chat-show-todos)))
            (hermes-dashboard-transport--handle-frame
             client "{\"method\":\"event\",\"params\":{\"type\":\"todo.updated\",\"session_id\":\"a\",\"payload\":{\"todos\":[{\"id\":\"a\",\"content\":\"Wire task\",\"status\":\"pending\"}],\"revision\":1}}}")
            (with-current-buffer a
              (should (eq 'active (plist-get hermes-chat--todos :status)))
              (should (string-match-p "Wire task" (with-current-buffer hermes-chat--todos-panel (buffer-string)))))
            (with-current-buffer b (should-not (plist-get hermes-chat--todos :items))))
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) (list a b))))))

(provide 'hermes-chat-todos-tests)
;;; hermes-chat-todos-tests.el ends here
