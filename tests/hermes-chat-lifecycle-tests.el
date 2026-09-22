;;; hermes-chat-lifecycle-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-stop-confirmation-scope-and-ownership ()
  "Global stop requires consent for the exact attachment, including callbacks."
  (dolist (change '(cancel accept client session connection lifetime))
    (let ((client (hermes-test--dashboard-client)) frames prompt)
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "chat-A"
             hermes-chat--dashboard-session-ready-p t)
       (let ((hermes-dashboard-transport-websocket-send-function
              (lambda (_socket text)
                (push (hermes-transport-json-parse text) frames))))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (text)
                      (setq prompt text)
                      (pcase change
                        ('client (setq hermes-chat--dashboard-client
                                       (hermes-test--dashboard-client)))
                        ('session (setq hermes-chat--dashboard-active-session-id "chat-B"))
                        ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
                        ('lifetime (setq hermes-chat--lifecycle-generation (list 'replacement))))
                      (not (eq change 'cancel)))))
           (hermes-chat-stop-processes)))
       (should (string-match-p "all chats" (or prompt "")))
       (should (string-match-p "connected Hermes instance" prompt))
       (if (eq change 'accept)
           (progn
             (should (= (length frames) 1))
             (should (equal (hermes-transport--get (car frames) 'method) "process.stop"))
             (should-not (hermes-transport--get
                          (hermes-transport--get (car frames) 'params) 'session_id)))
         (should-not frames))))))

(ert-deftest hermes-chat-stop-shared-instance-consent ()
  "One chat's confirmed stop is explicitly shared by both attached owners."
  (let ((client (hermes-test--dashboard-client)) frames consent)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client client
           hermes-chat--dashboard-active-session-id "chat-A"
           hermes-chat--dashboard-session-ready-p t)
     (let ((origin (current-buffer)))
       (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-client client
              hermes-chat--dashboard-active-session-id "chat-B"
              hermes-chat--dashboard-session-ready-p t)
        (let ((peer (current-buffer))
              (hermes-dashboard-transport-websocket-send-function
               (lambda (_socket text)
                 (push (hermes-transport-json-parse text) frames))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (should (string-match-p "all chats" prompt))
                       consent)))
            (with-current-buffer origin
              (hermes-chat-stop-processes)
              (should-not frames)
              (setq consent t)
              (hermes-chat-stop-processes)))
          (should (= (length frames) 1))
          (should (equal (hermes-transport--get (car frames) 'method) "process.stop"))
          (should (eq client (buffer-local-value 'hermes-chat--dashboard-client peer)))
          (should (equal (buffer-local-value 'hermes-chat--dashboard-active-session-id peer)
                         "chat-B"))))))))

(ert-deftest hermes-chat-stop-stale-completions ()
  "Both stop completions leave a successor attachment's transcript untouched."
  (dolist (change '(client session connection lifetime))
    (let ((client (hermes-test--dashboard-client)) callbacks)
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "chat-A"
             hermes-chat--dashboard-session-ready-p t)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                 ((symbol-function 'hermes-dashboard-transport-process-stop)
                  (lambda (target &rest args)
                    (should (eq target client))
                    (setq callbacks args))))
         (hermes-chat-stop-processes))
       (pcase change
         ('client (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)))
         ('session (setq hermes-chat--dashboard-active-session-id "chat-B"))
         ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
         ('lifetime (setq hermes-chat--lifecycle-generation (list 'replacement))))
       (let ((before (buffer-string)))
         (funcall (plist-get callbacks :resolve) '((killed . 2)))
         (funcall (plist-get callbacks :reject) "old failure")
         (should (equal before (buffer-string))))))))

(ert-deftest hermes-chat-stop-help-names-instance-scope ()
  "Catalog help and completion must not inherit misleading backend wording."
  (let ((pair '("/stop" "Stop processes for this chat")))
    (should (string-match-p "all.*connected Hermes instance"
                            (hermes-chat--format-command-pair pair)))
    (should (string-match-p "all.*connected Hermes instance"
                            (cdar (hermes-chat--catalog-pairs-candidates (list pair)))))))

(ert-deftest hermes-chat-idle-automatic-close-retires-work ()
  "Real socket loss retires idle observation; readiness does not resume it."
  (let ((client (hermes-test--dashboard-client)) cancelled resumed)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _) (setq resumed t)))
              ((symbol-function 'hermes-dashboard-transport-cancel-owner-requests)
               (lambda (_client owner) (push owner cancelled))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "runtime-A") (session_key . "durable-A")))
       (let ((owner hermes-chat--work-owner)
             (hermes-dashboard-transport-schedule-function (lambda (&rest _) nil)))
         (should owner)
         (should (hermes-chat--work-current-p owner))
         (should-not hermes-chat--pending-assistant-id)
         (setf (hermes-dashboard-transport-client-refcount client) 1)
         (hermes-dashboard-transport--handle-socket-down client "Socket closed")
         (should (memq owner cancelled))
         (should-not hermes-chat--work-owner)
         (should-not (plist-get owner :timer))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (should (equal hermes-chat--session-id "durable-A"))
         (setf (hermes-dashboard-transport-client-websocket client) 'replacement-socket)
         (hermes-dashboard-transport--handle-frame
          client "{\"jsonrpc\":\"2.0\",\"method\":\"event\",\"params\":{\"type\":\"gateway.ready\"}}")
         (should-not resumed)
         (should-not hermes-chat--work-owner)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (should (equal hermes-chat--session-id "durable-A"))
         (hermes-chat--dashboard-record-session
          client '((session_id . "runtime-B") (session_key . "durable-A")))
         (should-not (eq owner hermes-chat--work-owner))
         (should (hermes-chat--work-current-p hermes-chat--work-owner)))))))

(ert-deftest hermes-chat-idle-process-exit-retires-work ()
  "A real terminal process-exit event also retires a nil-assistant attachment."
  (let ((client (hermes-test--dashboard-client)) cancelled)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-cancel-owner-requests)
               (lambda (_client owner) (push owner cancelled))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "runtime-A") (session_key . "durable-A")))
       (let ((owner hermes-chat--work-owner)
             (hermes-dashboard-transport-process-live-p-function (lambda (_) nil)))
         (setf (hermes-dashboard-transport-client-process client) 'exited-child)
         (hermes-dashboard-transport--handle-process-exit
          client 'exited-child
          (hermes-dashboard-transport-client-process-generation client))
         (should (hermes-dashboard-transport-client-stopping-p client))
         (should (memq owner cancelled))
         (should-not hermes-chat--work-owner)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (should (equal hermes-chat--session-id "durable-A")))))))

(ert-deftest hermes-chat-kill-stops-dashboard-client ()
  (let (closed deleted rejected)
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (websocket) (setq closed websocket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :process 'fake-process
                      :websocket 'fake-websocket
                      :ready-p t
                      :pending pending
                      :callback #'ignore))
             (buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
        (puthash "req-1"
                 (list :method "prompt.submit"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-chat--dashboard-client client))
              (kill-buffer buffer)
              (should (eq closed 'fake-websocket))
              (should (eq deleted 'fake-process))
              (should (string-match-p "stopped" rejected))
              (should-not (hermes-dashboard-transport-client-websocket client))
              (should-not (hermes-dashboard-transport-client-process client))
              (should (= (hash-table-count
                          (hermes-dashboard-transport-client-pending client))
                         0)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest hermes-chat-dashboard-start-stops-stale-client-before-replacing ()
  (let ((old-client (make-hermes-dashboard-transport-client
                     :process 'old-process
                     :websocket nil
                     :ready-p nil
                     :pending (make-hash-table :test #'equal)
                     :callback #'ignore))
        (new-client (make-hermes-dashboard-transport-client
                     :websocket 'new-websocket
                     :pending (make-hash-table :test #'equal)
                     :callback #'ignore))
        deleted cleared-before-cancel)
    (cl-letf (((symbol-function 'delete-process)
               (lambda (process) (setq deleted process)))
              ((symbol-function 'hermes-dashboard-transport-cancel-owner-requests)
               (lambda (&rest _)
                 (setq cleared-before-cancel
                       (and (null hermes-chat--session-bootstrap)
                            (null hermes-chat--create-override-owner)))))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) new-client)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client old-client
             hermes-chat--session-bootstrap 'old-bootstrap
             hermes-chat--create-override-owner 'old-overrides)
       (should (eq (hermes-chat--dashboard-start #'ignore) new-client))
       (should cleared-before-cancel)
       (should (eq deleted 'old-process))
       (should (eq hermes-chat--dashboard-client new-client))
       (should-not hermes-chat--session-bootstrap)
       (should-not hermes-chat--create-override-owner)
       (should-not (hermes-dashboard-transport-client-process old-client))))))

(ert-deftest hermes-chat-dashboard-complete-interrupted-preserves-status ()
  (let ((client (hermes-test--dashboard-client)) callback submit-text)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 (setf (hermes-dashboard-transport-client-callback client)
                       callback)
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submit-text text))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long prompt")
         (hermes-chat-send)
         (should (equal submit-text "long prompt"))
         (hermes-dashboard-transport--handle-frame
          client (hermes-dashboard-transport--encode-frame
                  '((jsonrpc . "2.0")
                    (method . "event")
                    (params . ((type . "message.complete")
                               (session_id . "sid-active")
                               (payload . ((text . "Stopped")
                                           (status . "interrupted"))))))))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :status) "interrupted"))
           (should (equal (plist-get assistant :content) "")))
         (should-not hermes-chat--pending-assistant-id)
         (should (string-match-p "Interrupted"
                                 (hermes-test--header-line-string)))
         (should-not (string-match-p "Error"
                                     (hermes-test--header-line-string))))))))

(ert-deftest hermes-chat-disconnect-stops-dashboard-session ()
  "Disconnect tears down the live client and marks the chat disconnected."
  (let ((client (hermes-test--dashboard-client))
        stopped)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) nil))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (c &rest _args) (setq stopped c))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "hello")
         (hermes-chat-send)
         (should hermes-chat--dashboard-client)
         (hermes-chat-disconnect)
         (should (eq stopped client))
         (should-not hermes-chat--dashboard-client)
         (should (eq (plist-get hermes-chat--status-state :status) 'disconnected))
         (should (string-match-p "Session disconnected" (buffer-string))))))))

(ert-deftest hermes-chat-disconnect-recovers-full-input ()
  "Public disconnect preserves exact occurrences without submitting anything."
  (let ((body (concat "  λ\n" (make-string 400 ?x) "\nend  "))
        (display " display\nnot the body \n")
        (draft "  newer\ndraft \t")
        (real-display (symbol-function 'display-buffer)) recovery shown)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest args)
                     (when (string-prefix-p "*Hermes recovery" (buffer-name buffer))
                       (setq shown buffer))
                     (apply real-display buffer args)))
                  ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (&rest _) (ert-fail "Unexpected submit")))
                  ((symbol-function 'hermes-chat-resume-session)
                   (lambda (&rest _) (ert-fail "Unexpected resume"))))
          (hermes-test-with-chat-buffer
           (setq hermes-chat--dashboard-active-session-id "live"
                 hermes-chat--session-id "stored"
                 hermes-chat--profile "test-profile"
                 hermes-chat--input-history '("accepted")
                 hermes-chat--queued-messages
                 (list (hermes-chat--make-queue-entry body display)
                       (hermes-chat--make-queue-entry body nil)
                       (hermes-chat--make-queue-entry "rejected" "")))
           (setf (plist-get (nth 2 hermes-chat--queued-messages) :rejected-p) t)
           (setq hermes-chat--queued-submit-id (hermes-chat--queue-head-id)
                 hermes-chat--unsettled-submit-context
                 (list :queue-id hermes-chat--queued-submit-id :content body)
                 hermes-chat--busy-submit-context
                 (list :content "separate unresolved" :display "busy display"))
           (insert draft)
           (hermes-chat-disconnect)
           (setq recovery shown)
           (should (buffer-live-p recovery))
           (should (string-match-p (regexp-quote (buffer-name recovery))
                                   (buffer-string)))
           (should (equal (hermes-chat-input-string) draft))
           (should (equal hermes-chat--input-history '("accepted")))
           (should (equal hermes-chat--session-id "stored"))
           (should-not hermes-chat--dashboard-active-session-id)
           (should-not hermes-chat--queued-messages)
           (should-not hermes-chat--unsettled-submit-context)
           (with-current-buffer recovery
             (should-not buffer-read-only)
             (goto-char (point-min))
             (dolist (text (list "stored" "test-profile" "Sessions"
                                 "hermes-chat-resume-session"
                                 "Delivery uncertain" body display "Never sent" body
                                 "Rejected" "rejected" "Display:\n\n"
                                 "Delivery uncertain" "separate unresolved"
                                 "busy display" "Draft" draft))
               (should (search-forward text nil t)))
             (should (= 2 (how-many (regexp-quote body) (point-min) (point-max))))
             (goto-char (point-max))
             (insert "manual edit"))))
      (when (buffer-live-p recovery)
        ;; The source fixture has already been killed.
        (with-current-buffer recovery
          (should (string-suffix-p "manual edit" (buffer-string))))
        (kill-buffer recovery)))))

(ert-deftest hermes-chat-disconnect-recovers-narrowed-draft ()
  "Disconnect copies the full draft without changing its restricted view."
  (dolist (bounds '((0 . 5) (6 . 12)))
    (let ((draft "first\nSECOND-HALF\n  λ tail \t") recovery source)
      (unwind-protect
          (progn
            (hermes-test-with-chat-buffer
             (setq source (current-buffer)
                   hermes-chat--dashboard-active-session-id "live")
             (insert draft)
             ;; The second view excludes the input marker as well as the suffix.
             (narrow-to-region (+ hermes-chat--input-marker (car bounds))
                               (+ hermes-chat--input-marker (cdr bounds)))
             (goto-char (1+ (point-min)))
             (let ((start (copy-marker (point-min)))
                   (end (copy-marker (point-max)))
                   (cursor (copy-marker (point)))
                   (visible (buffer-string)))
               (hermes-chat-disconnect)
               (setq recovery hermes-chat--recovery-buffer)
               (should (buffer-live-p recovery))
               (should (buffer-narrowed-p))
               (should (= (point-min) start))
               (should (= (point-max) end))
               (should (= (point) cursor))
               (should (equal (buffer-string) visible))
               (should (equal (save-restriction
                                (widen)
                                (hermes-chat-input-string))
                              draft))
               (with-current-buffer recovery
                 (goto-char (point-min))
                 (should (search-forward "\nContent:\n" nil t))
                 (should (equal (buffer-substring-no-properties
                                 (point) (point-max))
                                draft)))))
            (should-not (buffer-live-p source))
            (with-current-buffer recovery
              (should (string-suffix-p draft (buffer-string)))))
        (when (buffer-live-p recovery) (kill-buffer recovery))))))

(ert-deftest hermes-chat-disconnect-captures-hook-input-and-revisions ()
  "Retry preserves edits, revisions and hook-added equal occurrences."
  (let (recovery)
    (unwind-protect
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-active-session-id "live"
               hermes-chat--queued-messages
               (list (hermes-chat--make-queue-entry (copy-sequence "same") nil)))
         (insert "draft")
         (let ((hermes-chat-cleanup-functions (list (lambda () (error "Stop")))))
           (should-error (hermes-chat-disconnect))
           (setq recovery hermes-chat--recovery-buffer)
           (with-current-buffer recovery (goto-char (point-max)) (insert "USER EDIT"))
           (let ((text (with-current-buffer recovery (buffer-string))))
             (should-error (hermes-chat-disconnect))
             (should (equal text (with-current-buffer recovery (buffer-string))))))
         ;; Mutate a string in place: a retained pointer is not a revision snapshot.
         (aset (plist-get (car hermes-chat--queued-messages) :content) 0 ?S)
         (goto-char (point-max)) (insert " changed")
         (let ((hermes-chat-cleanup-functions
                (list (lambda ()
                        (should-error (hermes-chat-disconnect) :type 'user-error)
                        (hermes-chat--queue-content "same"))))
               (hermes-chat-lifecycle-invalidation-hook
                (list (lambda () (hermes-chat--queue-content "same")))))
           (hermes-chat-disconnect))
         (should-not hermes-chat--queued-messages)
         (should (equal (hermes-chat-input-string) "draft changed"))
         (with-current-buffer recovery
           (goto-char (point-min))
           (dolist (text '("same" "draft" "USER EDIT" "later revision" "Same"
                           "Draft" "later revision" "draft changed"
                           "Never sent" "same" "Never sent" "same"))
             (should (search-forward text nil t)))))
      (when (buffer-live-p recovery) (kill-buffer recovery)))))

(ert-deftest hermes-chat-disconnect-capture-failures-retain-owners ()
  "Creation and partial insertion errors/quits abort before input release."
  (dolist (condition '(error quit))
    (dolist (phase '(creation initial final))
      (let (recovery released marked cleanup-ran caught)
        (unwind-protect
            (hermes-test-with-chat-buffer
             (setq hermes-chat--dashboard-active-session-id "live"
                   hermes-chat--queued-messages
                   (list (hermes-chat--make-queue-entry "original" nil))
                   hermes-chat--unsettled-submit-context (list :content "unresolved")
                   hermes-chat--busy-submit-context (list :content "busy")
                   hermes-chat--pending-assistant-id "assistant")
             (insert "draft")
             (let ((real-activate (symbol-function 'activate-change-group))
                   (real-create (symbol-function 'generate-new-buffer))
                   (hermes-chat-cleanup-functions (list (lambda () (setq cleanup-ran t))))
                   (hermes-chat-lifecycle-invalidation-hook
                    (list (lambda () (hermes-chat--queue-content "hook input")))))
               (cl-letf (((symbol-function 'hermes-chat--stop-dashboard-client)
                          (lambda () (setq released t)))
                         ((symbol-function 'hermes-chat--mark-assistant)
                          (lambda (&rest _) (setq marked t)))
                         ((symbol-function 'generate-new-buffer)
                          (lambda (name &rest args)
                            (if (and (eq phase 'creation)
                                     (string-prefix-p "*Hermes recovery" name))
                                (signal condition '("Capture failed"))
                              (apply real-create name args))))
                         ((symbol-function 'activate-change-group)
                          (lambda (handle)
                            (funcall real-activate handle)
                            (when (and (string-prefix-p "*Hermes recovery" (buffer-name))
                                     (or (eq phase 'initial)
                                         (and (eq phase 'final) cleanup-ran)))
                                (insert "PARTIAL")
                              (signal condition '("Capture failed"))))))
                 (condition-case err (hermes-chat-disconnect)
                   ((error quit) (setq caught (car err))))))
             (should (eq caught condition))
             (should-not released)
             (should (eq marked (eq phase 'final)))
             (should (eq cleanup-ran (eq phase 'final)))
             (should-not hermes-chat--disconnect-in-progress)
             (should hermes-chat--unsettled-submit-context)
             (should hermes-chat--busy-submit-context)
             (should (equal (hermes-chat-input-string) "draft"))
             (should (equal (mapcar (lambda (e) (plist-get e :content))
                                   hermes-chat--queued-messages)
                            (if (eq phase 'final) '("original" "hook input")
                              '("original"))))
             (setq recovery hermes-chat--recovery-buffer)
             (when (buffer-live-p recovery)
               (with-current-buffer recovery
                 (should-not (string-match-p "PARTIAL" (buffer-string)))))
             (setq hermes-chat--pending-assistant-id nil)
             (hermes-chat-disconnect)
             (setq recovery hermes-chat--recovery-buffer)
             (with-current-buffer recovery
               (should-not (string-match-p "PARTIAL" (buffer-string)))
               (should (= 1 (how-many "Content:\noriginal" (point-min) (point-max))))))
          (when (buffer-live-p recovery) (kill-buffer recovery)))))))

(ert-deftest hermes-chat-recovery-formatting-preserves-literal-revisions ()
  "Recovery formatting preserves literal text and exact occurrence identity."
  (let* ((identity (list 'occurrence))
         (other (list 'occurrence))
         (records (list (list identity "Never sent" "  λ\n\t" "shown\n")
                        (list other "Delivery uncertain" "same" nil)))
         (before (copy-tree records))
         (text (hermes-chat--recovery-text records (list (cons identity nil)))))
    (should (equal text
                   (concat "\n\nNever sent (later revision)\nContent:\n  λ\n\t"
                           "\nDisplay:\nshown\n"
                           "\n\nDelivery uncertain\nContent:\nsame")))
    (should (equal records before))
    (should (equal (hermes-chat--recovery-document text "sid" "profile" "instance")
                   (concat
                    "Hermes recovery\nSession: sid\nProfile: profile\nInstance: instance\n"
                    "Open Sessions in this instance/profile and resume this session,\n"
                    "or use M-x hermes-chat-resume-session in that instance.\n"
                    "Inspect history before copying selected text and explicitly sending.\n"
                    "Never automatically resend Delivery uncertain text.\n"
                    "This editable buffer is in-memory only; save it if needed."
                    text)))))

(ert-deftest hermes-chat-disconnect-recreates-killed-recovery ()
  "A killed document cannot suppress still-owned input on retry."
  (let (recovery)
    (unwind-protect
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-active-session-id "live"
               hermes-chat--queued-messages
               (list (hermes-chat--make-queue-entry "retained" nil)))
         (let ((hermes-chat-cleanup-functions (list (lambda () (error "Stop")))))
           (should-error (hermes-chat-disconnect)))
         (kill-buffer hermes-chat--recovery-buffer)
         (hermes-chat-disconnect)
         (setq recovery hermes-chat--recovery-buffer)
         (with-current-buffer recovery
           (should (= 1 (how-many "Content:\nretained" (point-min) (point-max))))))
      (when (buffer-live-p recovery) (kill-buffer recovery)))))

(ert-deftest hermes-chat-disconnect-empty-and-hook-only-input ()
  "Empty input creates nothing; hooks can introduce the first recovery input."
  (dolist (hook-input '(nil t))
    (let (recovery)
      (unwind-protect
          (hermes-test-with-chat-buffer
           (setq hermes-chat--dashboard-active-session-id "live")
           (let ((hermes-chat-lifecycle-invalidation-hook
                  (when hook-input (list (lambda () (hermes-chat--queue-content "late"))))))
             (hermes-chat-disconnect))
           (setq recovery hermes-chat--recovery-buffer)
           (should (eq (buffer-live-p recovery) hook-input))
           (when hook-input
             (should (get-buffer-window recovery))
             (should (string-match-p (regexp-quote (buffer-name recovery)) (buffer-string)))
             (with-current-buffer recovery
               (should (string-match-p "Content:\nlate" (buffer-string))))))
        (when (buffer-live-p recovery) (kill-buffer recovery))))))

(ert-deftest hermes-chat-disconnect-without-session-errors ()
  "Disconnect signals a user error when there is no live session."
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-disconnect) :type 'user-error)))

(defun hermes-test--with-dashboard-restart (function)
  "Call FUNCTION with a restart fixture, faking only process and wire edges."
  (let* ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
         (hermes-dashboard-transport-request-timeout nil)
         (hermes-dashboard-transport-idle-close-delay nil)
         (old (make-hermes-dashboard-transport-client
               :host "127.0.0.1" :port 9123 :endpoint-key '(spawn "127.0.0.1" 9123)
               :process 'old-process :websocket 'old-ws :ready-p t :refcount 3
               :ready-promise (hermes--promise-resolved t)))
         (state (list :old old :starts 0 :frames nil :deleted nil
                      :new nil :start-error nil :buffers nil))
         (buffers (mapcar (lambda (_)
                            (generate-new-buffer (hermes-test--chat-buffer-name)))
                          '(a b blank)))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_ws text)
            (push (hermes-dashboard-transport--decode-frame text)
                  (plist-get state :frames)))))
    (puthash '(spawn "127.0.0.1" 9123) old hermes-dashboard-transport--clients)
    (setf (plist-get state :buffers) buffers)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'delete-process)
               (lambda (process) (push process (plist-get state :deleted))))
              ((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (cl-incf (plist-get state :starts))
                 (should (eq (plist-get args :start-mode) 'spawn))
                 (should (equal (plist-get args :host) "127.0.0.1"))
                 (should (= (plist-get args :port) 9123))
                 (when (plist-get state :start-error) (error "Spawn failed"))
                 (setf (plist-get state :new)
                       (make-hermes-dashboard-transport-client
                        :host "127.0.0.1" :port 9123 :process 'new-process
                        :websocket 'new-ws :ready-promise (hermes--promise-make))))))
      (unwind-protect
          (progn
            (cl-loop for buffer in buffers for stored in '("stored-a" "stored-b" nil)
                     do (with-current-buffer buffer
                          (hermes-chat-mode)
                          (setq hermes-chat--dashboard-client old
                                hermes-chat--resolved-start-mode 'spawn
                                hermes-chat--session-id stored
                                hermes-chat--dashboard-active-session-id
                                (and stored (concat "old-" stored))
                                hermes-chat--dashboard-session-ready-p (and stored t))
                          (hermes-chat--ensure-idle-listener old buffer)))
            (funcall function state))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (buffer-live-p hermes-chat--recovery-buffer)
                (kill-buffer hermes-chat--recovery-buffer)))
            (kill-buffer buffer)))))))

(defun hermes-test--restart-ready (state)
  "Resolve replacement readiness in restart fixture STATE."
  (let ((client (plist-get state :new)))
    (setf (hermes-dashboard-transport-client-ready-p client) t)
    (hermes--promise-resolve
     (hermes-dashboard-transport-client-ready-promise client) client)))

(defun hermes-test--restart-reply (state frame &optional failure running)
  "Reply to FRAME in STATE with optional FAILURE or RUNNING turn."
  (let ((stored (hermes-transport--get (alist-get 'params frame) 'session_id)))
    (hermes-dashboard-transport--handle-frame
     (plist-get state :new)
     `((jsonrpc . "2.0") (id . ,(alist-get 'id frame))
       ,(if failure `(error . ((message . ,failure)))
          `(result . ((session_id . ,(concat "new-" stored))
                      (resumed . ,stored) (running . ,(and running t))
                      (inflight . ,(and running '((text . "Recovered work")))))))))))

(ert-deftest hermes-chat-dashboard-restart-reattaches-without-resending ()
  "Restart once, eagerly resume both durable chats, and leave blank input alone."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (let* ((buffers (plist-get state :buffers))
            (old (plist-get state :old))
            (caller (car buffers)) old-callback lifetime position)
       (with-current-buffer caller
         (insert "draft α\nsecond line")
         (backward-char 3)
         (setq position (- (point) hermes-chat--input-marker)
               lifetime hermes-chat--lifecycle-generation
               old-callback (hermes-chat--transport-callback
                             caller "interrupted" t hermes-chat--transport-generation)
               hermes-chat--pending-assistant-id "interrupted"
               hermes-chat--dashboard-running-p t
               hermes-chat--queued-messages '((:id queued :content "queued exact")))
         (hermes-chat--insert-entry
          '(:id "interrupted" :role assistant :content "partial" :status streaming))
         (hermes-dashboard-restart)
         (should (equal (plist-get state :deleted) '(old-process)))
         (should (= (plist-get state :starts) 1))
         (should-not (eq lifetime hermes-chat--lifecycle-generation))
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--queued-messages)
         (should-not hermes-chat--dashboard-running-p)
         (should (equal (hermes-chat-input-string) "draft α\nsecond line"))
         (should (= position (- (point) hermes-chat--input-marker)))
         (should (string-match-p "queued exact"
                                 (with-current-buffer hermes-chat--recovery-buffer
                                   (buffer-string))))
         (should-error (hermes-dashboard-restart) :type 'user-error)
         (funcall old-callback '(:type delta :content "STALE"))
         (should-not (string-match-p "STALE" (buffer-string))))
       (should (hermes-dashboard-transport-client-stopping-p old))
       (should-not (plist-get state :frames))
       (hermes-test--restart-ready state)
       (let ((frames (plist-get state :frames)))
         (should (= (length frames) 2))
         (should (equal (sort (mapcar (lambda (frame)
                                       (should (equal (alist-get 'method frame) "session.resume"))
                                       (hermes-transport--get (alist-get 'params frame) 'session_id))
                                     frames) #'string<)
                        '("stored-a" "stored-b")))
         (dolist (frame frames) (hermes-test--restart-reply state frame)))
       (cl-loop for buffer in buffers for stored in '("stored-a" "stored-b" nil)
                do (with-current-buffer buffer
                     (should (eq hermes-chat--dashboard-client (plist-get state :new)))
                     (should (equal hermes-chat--session-id stored))
                     (should (equal hermes-chat--dashboard-active-session-id
                                    (and stored (concat "new-" stored))))
                     (should-not hermes-chat--session-bootstrap)))
       (should (= (hermes-dashboard-transport-client-refcount (plist-get state :new)) 3))
       (should (= (length (plist-get state :frames)) 2))))))

(ert-deftest hermes-chat-dashboard-restart-isolates-resume-failure-and-running ()
  "One failed resume leaves its peer attached to backend-recovered work."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
     (hermes-test--restart-ready state)
     (dolist (frame (plist-get state :frames))
       (hermes-test--restart-reply
        state frame
        (and (equal (hermes-transport--get (alist-get 'params frame) 'session_id)
                    "stored-a") "Missing session") t))
     (with-current-buffer (car (plist-get state :buffers))
       (should-not hermes-chat--dashboard-session-ready-p)
       (should-not hermes-chat--session-bootstrap)
       (should (equal hermes-chat--session-id "stored-a"))
       (should (eq (plist-get hermes-chat--status-state :status) 'error)))
     (with-current-buffer (cadr (plist-get state :buffers))
       (should hermes-chat--dashboard-session-ready-p)
       (should hermes-chat--pending-assistant-id)
       (should (hermes-chat--active-turn-p)))
     (should (= (length (plist-get state :frames)) 2)))))

(ert-deftest hermes-chat-dashboard-restart-stale-responses-and-killed-buffer ()
  "Killed and replaced chats ignore late resume results and errors."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
     (hermes-test--restart-ready state)
     (let ((frames (plist-get state :frames)))
       (kill-buffer (cadr (plist-get state :buffers)))
       (with-current-buffer (car (plist-get state :buffers))
         (hermes-chat--invalidate-transport-state)
         (setq hermes-chat--session-id "replacement")
         (dolist (frame frames)
           (hermes-test--restart-reply state frame)
           (hermes-test--restart-reply state frame "late error"))
         (should (equal hermes-chat--session-id "replacement"))
         (should-not hermes-chat--dashboard-active-session-id))))))

(ert-deftest hermes-chat-dashboard-restart-startup-failures-are-retryable ()
  "Both synchronous spawn errors and terminal readiness failure settle owners."
  (dolist (synchronous '(nil t))
    (hermes-test--with-dashboard-restart
     (lambda (state)
       (setf (plist-get state :start-error) synchronous)
       (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
       (unless synchronous
         (hermes-dashboard-transport-stop (plist-get state :new) "Startup timed out"))
       (dolist (buffer (plist-get state :buffers))
         (with-current-buffer buffer
           (should-not hermes-chat--session-bootstrap)
           (should (eq (plist-get hermes-chat--status-state :status) 'error))))
       (should-not (plist-get state :frames))
       (setf (plist-get state :start-error) nil)
       (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
       (should (= (plist-get state :starts) 2))))))

(ert-deftest hermes-chat-dashboard-restart-post-ready-failure-is-retryable ()
  "Socket loss and stop settle pending resumes without submitting drafts."
  (dolist (stop '(nil t))
    (hermes-test--with-dashboard-restart
     (lambda (state)
       (let* ((buffers (plist-get state :buffers))
              (hermes-dashboard-transport-reconnect-max-attempts 3)
              (hermes-dashboard-transport-schedule-function (lambda (&rest _) nil)))
         (dolist (buffer buffers)
           (with-current-buffer buffer (insert "original draft α")))
         (with-current-buffer (car buffers) (hermes-dashboard-restart))
         (hermes-test--restart-ready state)
         (should (= (length (plist-get state :frames)) 2))
         (let ((client (plist-get state :new)))
           (if stop
               (hermes-dashboard-transport-stop client "Stopped during resume")
             (hermes-dashboard-transport--handle-socket-down
              client "Lost during resume" 'new-ws))
           (cl-loop for buffer in buffers for stored in '("stored-a" "stored-b" nil)
                    do (with-current-buffer buffer
                         (should-not hermes-chat--session-bootstrap)
                         (should (equal hermes-chat--session-id stored))
                         (should-not hermes-chat--dashboard-active-session-id)
                         (should (equal (hermes-chat-input-string) "original draft α"))))
           (unless stop (hermes-test--restart-ready state))
           (should (= (length (plist-get state :frames)) 2))
           ;; A manual restart must be allowed, never an automatic input retry.
           (with-current-buffer (car buffers) (hermes-dashboard-restart))
           (should (= (plist-get state :starts) 2))
           (hermes-test--restart-ready state)
           (dolist (frame (seq-take (plist-get state :frames) 2))
             (hermes-test--restart-reply state frame))
           (cl-loop for buffer in buffers for stored in '("stored-a" "stored-b" nil)
                    do (with-current-buffer buffer
                         (should-not hermes-chat--session-bootstrap)
                         (should (equal hermes-chat--session-id stored))
                         (should (equal (hermes-chat-input-string) "original draft α"))))
           (should (= (length (plist-get state :frames)) 4))
           (dolist (frame (plist-get state :frames))
             (should (equal (alist-get 'method frame) "session.resume")))))))))

(ert-deftest hermes-chat-dashboard-restart-post-ready-reject-preserves-successor ()
  "A retired resume cannot settle a successor reservation on socket loss."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (let ((caller (car (plist-get state :buffers)))
           (hermes-dashboard-transport-reconnect-max-attempts 3)
           (hermes-dashboard-transport-schedule-function (lambda (&rest _) nil)))
       (with-current-buffer caller (hermes-dashboard-restart))
       (hermes-test--restart-ready state)
       (with-current-buffer caller
         (let ((successor (copy-sequence hermes-chat--session-bootstrap)))
           ;; Identical context is insufficient: only the exact owner may settle.
           (setq hermes-chat--session-bootstrap successor)
           (hermes-dashboard-transport--handle-socket-down
            (plist-get state :new) "Lost during resume" 'new-ws)
           (hermes-test--restart-ready state)
           (should (eq hermes-chat--session-bootstrap successor))
           (should (equal hermes-chat--session-id "stored-a"))
           (should-not hermes-chat--dashboard-active-session-id)))))))

(ert-deftest hermes-chat-dashboard-restart-refuses-remote-and-decline ()
  "Remote and declined restarts never stop a client or spawn anything."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
         (hermes-dashboard-restart))
       (setf (hermes-dashboard-transport-client-endpoint-key (plist-get state :old))
             "https://remote.example.test")
       (should-error (hermes-dashboard-restart) :type 'user-error)
       (should-not (plist-get state :deleted))
       (should (zerop (plist-get state :starts)))))))

(ert-deftest hermes-chat-dashboard-restart-observes-auto-continue-without-submit ()
  "A cold recovery response gets one live readback, never a prompt submission."
  (dolist (event-first '(nil t))
    (hermes-test--with-dashboard-restart
     (lambda (state)
       (let* ((caller (car (plist-get state :buffers)))
              (client nil))
         (with-current-buffer caller (hermes-dashboard-restart))
         (hermes-test--restart-ready state)
         (setq client (plist-get state :new))
         (let ((frame (seq-find
                       (lambda (frame)
                         (equal (hermes-transport--get (alist-get 'params frame) 'session_id)
                                "stored-a"))
                       (plist-get state :frames))))
           (hermes-dashboard-transport--handle-frame
            client `((jsonrpc . "2.0") (id . ,(alist-get 'id frame))
                     (result . ((session_id . "new-stored-a") (resumed . "stored-a")
                                (running . nil) (auto_continue . ((attempt . 1))))))))
         (should (= (length (plist-get state :frames)) 3))
         (when event-first
           (hermes-dashboard-transport--dispatch-event
            client '(:type status :event "message.start" :status "started"
                     :session-id "new-stored-a")))
         (hermes-test--restart-reply state (car (plist-get state :frames)) nil (not event-first))
         (with-current-buffer caller
           (should (hermes-chat--active-turn-p))
           (should hermes-chat--pending-assistant-id)
           (should-not hermes-chat--session-bootstrap)
           (should-not (eq (plist-get hermes-chat--status-state :status) 'ready)))
         (should (seq-every-p
                  (lambda (frame) (equal (alist-get 'method frame) "session.resume"))
                  (plist-get state :frames))))))))

(ert-deftest hermes-chat-dashboard-restart-readiness-skips-retired-owners ()
  "Readiness cannot resume killed or repurposed chat buffers."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (let ((buffers (plist-get state :buffers)))
       (with-current-buffer (car buffers) (hermes-dashboard-restart))
       (kill-buffer (cadr buffers))
       (with-current-buffer (car buffers) (fundamental-mode))
       (hermes-test--restart-ready state)
       (should-not (plist-get state :frames))
       (with-current-buffer (car buffers) (should (eq major-mode 'fundamental-mode)))
       (with-current-buffer (caddr buffers) (should-not hermes-chat--session-bootstrap))
       (should (= (hermes-dashboard-transport-client-refcount (plist-get state :new)) 1))))))

(ert-deftest hermes-chat-dashboard-restart-confirmation-revalidates-target ()
  "Changing the shared target during confirmation never stops either owner."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (_prompt)
                    (cl-incf (hermes-dashboard-transport-client-generation
                              (plist-get state :old)))
                    t)))
         (should-error (hermes-dashboard-restart) :type 'user-error)))
     (should-not (plist-get state :deleted))
     (should (zerop (plist-get state :starts))))))

(ert-deftest hermes-chat-dashboard-restart-preserves-unrelated-client ()
  "A chat detached to another endpoint is excluded from the shared restart."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (let ((peer (cadr (plist-get state :buffers)))
           (other (make-hermes-dashboard-transport-client :refcount 1)))
       (with-current-buffer peer (setq hermes-chat--dashboard-client other))
       (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
       (hermes-test--restart-ready state)
       (should (= (length (plist-get state :frames)) 1))
       (with-current-buffer peer
         (should (eq hermes-chat--dashboard-client other))
         (should (equal hermes-chat--dashboard-active-session-id "old-stored-b")))
       (should-not (hermes-dashboard-transport-client-stopping-p other))))))

(ert-deftest hermes-chat-dashboard-restart-preservation-failure-does-not-stop ()
  "Failing input capture prevents any process stop or ownership destruction."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers))
       (setq hermes-chat--queued-messages '((:id q :content "Keep me")))
       (cl-letf (((symbol-function 'hermes-chat--capture-recovery)
                  (lambda () (error "Cannot preserve input"))))
         (should-error (hermes-dashboard-restart)))
       (should (equal hermes-chat--queued-messages '((:id q :content "Keep me"))))
       (should (equal hermes-chat--dashboard-active-session-id "old-stored-a")))
     (should-not (plist-get state :deleted))
     (should (zerop (plist-get state :starts))))))

(ert-deftest hermes-chat-dashboard-restart-preparation-failure-retries ()
  "Errors and quits release earlier and partially prepared reservations."
  (dolist (failure '(error quit))
    (dolist (boundary '(cleanup display))
      (hermes-test--with-dashboard-restart
       (lambda (state)
         (let* ((buffers (hermes-chat--dashboard-buffers (plist-get state :old)))
                (second (cadr buffers))
                (header (symbol-function 'hermes-chat--set-header-state)))
           (dolist (buffer buffers)
             (with-current-buffer buffer
               (insert "draft retained")
               (setq hermes-chat--queued-messages '((:id q :content "queued retained")))))
           (unwind-protect
               (cl-letf (((symbol-function 'hermes-chat--set-header-state)
                          (lambda (&rest args)
                            (if (and (eq boundary 'display) (eq (current-buffer) second)
                                     (eq (plist-get args :status) 'reconnecting))
                                (signal failure '("Preparation failed"))
                              (apply header args)))))
                 (when (eq boundary 'cleanup)
                   (with-current-buffer second
                     (setq-local hermes-chat-cleanup-functions
                                 (list (lambda () (signal failure '("Cleanup failed")))))))
                 (with-current-buffer (car buffers)
                   (should (eq failure
                               (condition-case err
                                   (progn (hermes-dashboard-restart) nil)
                                 ((error quit) (car err)))))))
             (with-current-buffer second
               (kill-local-variable 'hermes-chat-cleanup-functions)))
           (should-not (plist-get state :deleted))
           (should (zerop (plist-get state :starts)))
           (dolist (buffer buffers)
             (with-current-buffer buffer
               (should-not hermes-chat--session-bootstrap)
               (should (equal (hermes-chat-input-string) "draft retained"))
               (should (string-match-p "queued retained"
                                       (with-current-buffer hermes-chat--recovery-buffer
                                         (buffer-string))))))
           (with-current-buffer (car buffers) (hermes-dashboard-restart))
           (hermes-test--restart-ready state)
           (dolist (frame (plist-get state :frames))
             (hermes-test--restart-reply state frame))
           (should (= (plist-get state :starts) 1))
           (should (= (length (plist-get state :frames)) 2))
           (dolist (buffer buffers)
             (with-current-buffer buffer
               (should-not hermes-chat--session-bootstrap)
               (should (eq hermes-chat--dashboard-client (plist-get state :new)))
               (should (equal (hermes-chat-input-string) "draft retained"))))))))))

(ert-deftest hermes-chat-dashboard-restart-cleanup-preserves-successors ()
  "Cleanup cannot seize a replacement client, session, mode, or killed peer."
  (dolist (change '(client session mode kill-peer retarget-peer invalidation))
    (hermes-test--with-dashboard-restart
     (lambda (state)
       (let* ((buffers (hermes-chat--dashboard-buffers (plist-get state :old)))
              (caller (car buffers))
              (target (if (memq change '(kill-peer retarget-peer)) (cadr buffers) caller))
              (other (make-hermes-dashboard-transport-client :refcount 1))
              after-hook)
         (unwind-protect
             (progn
               (with-current-buffer caller
                 (set (make-local-variable
                       (if (eq change 'invalidation)
                           'hermes-chat-lifecycle-invalidation-hook
                         'hermes-chat-cleanup-functions))
                      (list
                       (lambda ()
                         (with-current-buffer target
                           (pcase change
                             ('kill-peer (kill-buffer target))
                             ('mode (fundamental-mode) (insert "Successor mode"))
                             (_
                              (when (memq change '(client retarget-peer invalidation))
                                (setq hermes-chat--dashboard-client other))
                              (setq hermes-chat--session-id "successor"
                                    hermes-chat--dashboard-active-session-id "successor-live"
                                    hermes-chat--queued-messages '((:content "successor queue")))))))
                       (lambda () (setq after-hook t))))
                 (hermes-dashboard-restart))
               (hermes-test--restart-ready state)
               (should (= (hermes-dashboard-transport-client-refcount
                           (plist-get state :new)) 2))
               ;; Mode teardown runs cleanup itself; only the outer restart
               ;; hook traversal must stop after a plain attachment replacement.
               (when (memq change '(client session)) (should-not after-hook))
               (pcase change
                 ('kill-peer (should-not (buffer-live-p target)))
                 ('mode
                  (with-current-buffer target
                    (should (eq major-mode 'fundamental-mode))
                    (should (string-match-p "Successor mode" (buffer-string)))))
                 (_
                  (with-current-buffer target
                    (should (eq hermes-chat--dashboard-client
                                (if (eq change 'session) (plist-get state :old) other)))
                    (should (equal hermes-chat--session-id "successor"))
                    (should (equal hermes-chat--dashboard-active-session-id "successor-live"))
                    (should (equal hermes-chat--queued-messages '((:content "successor queue"))))
                    (should-not hermes-chat--session-bootstrap))))
               (should-not (hermes-dashboard-transport-client-stopping-p other)))
           (when (buffer-live-p caller)
             (with-current-buffer caller
               (kill-local-variable 'hermes-chat-cleanup-functions)
               (kill-local-variable 'hermes-chat-lifecycle-invalidation-hook)))))))))

(ert-deftest hermes-chat-dashboard-restart-adopts-canonical-resume ()
  "Canonical durable IDs settle idle and recovering sessions without sending."
  (dolist (auto-continue '(nil t))
    (hermes-test--with-dashboard-restart
     (lambda (state)
       (let ((caller (car (plist-get state :buffers))))
         (with-current-buffer caller (hermes-dashboard-restart))
         (hermes-test--restart-ready state)
         (let ((frame (seq-find
                       (lambda (frame)
                         (equal (hermes-transport--get (alist-get 'params frame) 'session_id)
                                "stored-a")) (plist-get state :frames))))
           (hermes-dashboard-transport--handle-frame
            (plist-get state :new)
            `((jsonrpc . "2.0") (id . ,(alist-get 'id frame))
              (result . ((session_id . "canonical-live") (resumed . "compression-tip")
                         (running . nil) (auto_continue . ,auto-continue))))))
         (when auto-continue
           (let ((frame (car (plist-get state :frames))))
             (should (equal (hermes-transport--get (alist-get 'params frame) 'session_id)
                            "compression-tip"))
             (hermes-dashboard-transport--handle-frame
              (plist-get state :new)
              `((jsonrpc . "2.0") (id . ,(alist-get 'id frame))
                (result . ((session_id . "canonical-live") (resumed . "compression-tip")
                           (running . t) (inflight . ((text . "Recovered work")))))))))
         (with-current-buffer caller
           (should (equal hermes-chat--session-id "compression-tip"))
           (should (equal hermes-chat--dashboard-active-session-id "canonical-live"))
           (should-not hermes-chat--session-bootstrap)
           (should (eq (and (hermes-chat--active-turn-p) t) auto-continue)))
         (hermes-dashboard-transport--dispatch-event
          (plist-get state :new)
          '(:type status :event "message.start" :status "started" :session-id "canonical-live"))
         (with-current-buffer caller (should hermes-chat--pending-assistant-id))
         (should (= (length (plist-get state :frames)) (if auto-continue 3 2)))
         (should (seq-every-p
                  (lambda (frame) (equal (alist-get 'method frame) "session.resume"))
                  (plist-get state :frames))))))))

(ert-deftest hermes-chat-dashboard-restart-survives-pre-ready-socket-close ()
  "A replacement handshake retry retains reservations until real readiness."
  (hermes-test--with-dashboard-restart
   (lambda (state)
     (with-current-buffer (car (plist-get state :buffers)) (hermes-dashboard-restart))
     (let* ((client (plist-get state :new))
            (ready (hermes-dashboard-transport-client-ready-promise client))
            (hermes-dashboard-transport-reconnect-max-attempts 3)
            (hermes-dashboard-transport-schedule-function (lambda (&rest _) nil)))
       (hermes-dashboard-transport--handle-socket-down client "Handshake closed" 'new-ws)
       (should (eq ready (hermes-dashboard-transport-client-ready-promise client)))
       (should (zerop (hermes-dashboard-transport-client-reconnect-attempts client)))
       (hermes-dashboard-transport--handle-socket-down client "Retry handshake closed")
       (should (= (hermes-dashboard-transport-client-reconnect-attempts client) 1))
       (should (eq ready (hermes-dashboard-transport-client-ready-promise client)))
       (dolist (buffer (plist-get state :buffers))
         (with-current-buffer buffer (should hermes-chat--session-bootstrap)))
       (hermes-test--restart-ready state)
       (dolist (frame (plist-get state :frames)) (hermes-test--restart-reply state frame))
       (should (= (length (plist-get state :frames)) 2))
       (dolist (buffer (plist-get state :buffers))
         (with-current-buffer buffer
           (should-not hermes-chat--session-bootstrap)
           (when hermes-chat--session-id (should hermes-chat--dashboard-session-ready-p))))))))

(ert-deftest hermes-chat-dashboard-reconnect-requires-live-client ()
  "Manual reconnect rejects a chat without a live dashboard client."
  (hermes-test-with-chat-buffer
   (let ((error (should-error (hermes-dashboard-reconnect) :type 'user-error)))
     (should (string-match-p "live dashboard" (error-message-string error))))))

(ert-deftest hermes-chat-dashboard-reconnect-restarts-exact-client ()
  "Manual reconnect restarts the exact client without losing local chat state."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws :refcount 1))
        called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (seen &rest _)
                 (setq called seen))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--session-id "stored"
             hermes-chat--dashboard-active-session-id "sid-live"
             hermes-chat--queued-messages '((:content "queued")))
       (insert "draft")
       (hermes-dashboard-reconnect)
       (should (eq called client))
       (should (equal hermes-chat--session-id "stored"))
       (should (equal hermes-chat--dashboard-active-session-id "sid-live"))
       (should (equal (hermes-chat-input-string) "draft"))
       (should (equal hermes-chat--queued-messages '((:content "queued"))))))))

(ert-deftest hermes-chat-dashboard-reconnect-refuses-current-active-turn ()
  "Manual reconnect refuses to disrupt the invoking chat's active turn."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws :refcount 1))
        called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (&rest _) (setq called t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-running-p t)
       (let ((error (should-error (hermes-dashboard-reconnect) :type 'user-error)))
         (should (string-match-p "active turn" (error-message-string error))))
       (should-not called)))))

(ert-deftest hermes-chat-dashboard-reconnect-refuses-shared-active-turn ()
  "Manual reconnect refuses while another chat sharing the client is active."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws :refcount 2))
        caller peer called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (&rest _) (setq called t))))
      (unwind-protect
          (progn
            (setq caller (generate-new-buffer (hermes-test--chat-buffer-name))
                  peer (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer caller
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client client))
            (with-current-buffer peer
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client client
                    hermes-chat--dashboard-running-p t))
            (with-current-buffer caller
              (let ((error (should-error
                            (hermes-dashboard-reconnect) :type 'user-error)))
                (should (string-match-p "active turn"
                                        (error-message-string error)))))
            (should-not called))
        (when (buffer-live-p caller) (kill-buffer caller))
        (when (buffer-live-p peer) (kill-buffer peer))))))

(ert-deftest hermes-chat-dashboard-reconnect-ignores-unrelated-active-turn ()
  "An active chat on another client does not block manual reconnect."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws-a :refcount 1))
        (other-client (make-hermes-dashboard-transport-client
                       :websocket 'ws-b :refcount 1))
        caller peer called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (seen &rest _) (setq called seen))))
      (unwind-protect
          (progn
            (setq caller (generate-new-buffer (hermes-test--chat-buffer-name))
                  peer (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer caller
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client client))
            (with-current-buffer peer
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client other-client
                    hermes-chat--dashboard-running-p t))
            (with-current-buffer caller (hermes-dashboard-reconnect))
            (should (eq called client)))
        (when (buffer-live-p caller) (kill-buffer caller))
        (when (buffer-live-p peer) (kill-buffer peer))))))

(provide 'hermes-chat-lifecycle-tests)
;;; hermes-chat-lifecycle-tests.el ends here
