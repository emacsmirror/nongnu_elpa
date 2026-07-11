;;; hermes-chat-tests.el --- chat tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-opens-ewoc-buffer-with-writable-input-tail ()
  (hermes-test-with-chat-buffer
   (should (eq major-mode 'hermes-chat-mode))
   (should hermes-chat--ewoc)
   (should (markerp hermes-chat--input-marker))
   (should (= (marker-position hermes-chat--input-marker) (point-max)))
   (goto-char (point-min))
   (should-error (insert "not writable"))
   (goto-char hermes-chat--input-marker)
   (insert "draft")
   (should (equal (hermes-chat-input-string) "draft"))))

(ert-deftest hermes-chat-protect-transcript-covers-inserted-and-updated-entries ()
  "Entries inserted mid-transcript and invalidated nodes end up read-only."
  (hermes-test-with-chat-buffer
   (let ((node (hermes-chat--insert-entry
                '(:id "a1" :role assistant :content "reply" :status streaming))))
     (hermes-chat--insert-entry '(:id "s1" :role status :content "tooling") node)
     (hermes-chat--update-entry
      "a1" (lambda (entry) (plist-put entry :content "reply grew"))))
   (let ((pos (hermes-chat--input-position)))
     (should-not (text-property-not-all (point-min) pos 'read-only t))
     (should-not (text-property-not-all pos (point-max) 'read-only nil)))))

(ert-deftest hermes-chat-in-buffer-runs-only-when-live ()
  (let ((buffer (generate-new-buffer " *hermes-in-buffer-test*"))
        ran)
    (unwind-protect
        (progn
          (hermes-chat--in-buffer buffer
            (setq ran (current-buffer)))
          (should (eq ran buffer)))
      (kill-buffer buffer))
    (setq ran 'untouched)
    (hermes-chat--in-buffer buffer
      (setq ran 'should-not-run))
    (should (eq ran 'untouched))))

(ert-deftest hermes-chat-mode-map-sends-and-inserts-newlines ()
  (should (eq (keymap-lookup hermes-chat-mode-map "RET") #'hermes-chat-send))
  (should (eq (keymap-lookup hermes-chat-mode-map "C-j") #'hermes-chat-newline))
  (should (eq (keymap-lookup hermes-chat-mode-map "S-<return>") #'hermes-chat-newline)))

(ert-deftest hermes-chat-parses-slash-commands-with-arguments ()
  (should (equal (hermes-chat--parse-slash "/QUEUE next message")
                 '("queue" . "next message")))
  (should (equal (hermes-chat--parse-slash "/Goal\nstatus")
                 '("goal" . "status")))
  (should (equal (hermes-chat--parse-slash "/commands")
                 '("commands" . "")))
  (should-not (hermes-chat--parse-slash " /queue not-a-command")))

(ert-deftest hermes-chat-status-helpers-classify-parity-states ()
  (dolist (case '(("in_progress" "Running" "·" shadow t nil)
                  ("busy" "Running" "·" shadow t nil)
                  ("approval-requested" "Approval requested" "·"
                   shadow t nil)
                  ("queued" "Queued" "·" shadow t nil)
                  ("succeeded" "Ready" "✓" success nil t)
                  ("interrupted" "Interrupted" "!" error nil t)
                  ("cancelled" "Cancelled" "!" error nil t)
                  ("closed" "Disconnected" "!" warning nil t)))
    (pcase-let ((`(,status ,label ,icon ,face ,active ,finished) case))
      (should (equal (hermes-chat--header-status-label status) label))
      (should (equal (hermes-chat--status-icon status) icon))
      (should (eq (hermes-chat--status-face status) face))
      (should (eq (hermes-chat--header-status-face status) face))
      (should (eq (not (null (hermes-chat--active-status-p status))) active))
      (should (eq (not (null (hermes-chat--finished-status-p status)))
                  finished)))))

(ert-deftest hermes-chat-transient-status-marker-uses-status-icon-faces ()
  "Transient markers show the status icon (dot while active, check when done)."
  (dolist (case '(("running" "·" shadow)
                  ("completed" "✓" success)))
    (pcase-let ((`(,status ,icon ,face) case))
      (with-temp-buffer
        (hermes-chat--insert-transient-content
         (list :id status :role 'progress :status status :content "doing work"))
        (goto-char (point-min))
        (search-forward icon)
        (should (eq (get-text-property (1- (point)) 'face) face))))))

(ert-deftest hermes-chat-renders-user-entry-with-prompt-prefix ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry (hermes-chat--make-entry 'user "hello" 'done))
   (goto-char (point-min))
   (should (search-forward "> hello" nil t))
   (let ((text (buffer-string)))
     (should-not (string-match-p "User:" text))
     (should-not (string-match-p "Assistant:" text)))))

(ert-deftest hermes-chat-send-uses-transport-and-creates-pending-assistant ()
  (let (sent callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt cb)
              (setq sent prompt
                    callback cb)
              'fake-process)))
       (insert "hello Hermes")
       (hermes-chat-send)
       (should (equal sent "hello Hermes"))
       (should (functionp callback))
       (should (equal (hermes-chat-input-string) ""))
       (pcase-let ((`(,user ,assistant) (hermes-chat--entries)))
         (should (equal (plist-get user :role) 'user))
         (should (equal (plist-get user :content) "hello Hermes"))
         (should (equal (plist-get assistant :role) 'assistant))
         (should (equal (plist-get assistant :status) 'pending))
         (should (equal (plist-get assistant :content) "")))))))

(ert-deftest hermes-chat-transport-updates-preserve-draft-input ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback '(:type delta :content "hello"))
       (insert "draft survives")
       (funcall callback '(:type delta :content " there"))
       (should (equal (hermes-chat-input-string) "draft survives"))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :status) 'streaming))
         (should (equal (plist-get assistant :content) "hello there")))
       (funcall callback '(:type done))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :status) 'done))
         (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-transport-updates-do-not-record-transcript-undo ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (setq buffer-undo-list nil)
       (dotimes (_ 3)
         (funcall callback '(:type delta :content "streamed chunk ")))
       (funcall callback '(:type status
                           :status-key "lifecycle"
                           :status "running"
                           :content "Thinking…"))
       (funcall callback '(:type done))
       (should-not buffer-undo-list)
       (insert "draft")
       (should buffer-undo-list)))))

(ert-deftest hermes-chat-renders-status-and-progress-events ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type status
			:status-key "lifecycle"
			:status "running"
			:content "Thinking…"))
       (funcall callback
                '(:type progress
			:tool-call-id "tool-1"
			:name "terminal"
			:status "running"
			:content "running make test"))
       (let ((text (buffer-string)))
         (should (string-match-p "Thinking" text))
         (should (string-match-p "terminal: running make test" text)))
       (funcall callback
                '(:type progress
			:tool-call-id "tool-1"
			:name "terminal"
			:status "running"
			:content "13/13 passed"))
       (funcall callback
                '(:type tool
			:tool-call-id "tool-1"
			:name "terminal"
			:status "completed"
			:args ((command . "make test"))
			:duration 1.2))
       (let* ((entries (hermes-chat--entries))
              (roles (mapcar (lambda (entry) (plist-get entry :role)) entries))
              (text (buffer-string)))
         (should (equal roles '(user status tool assistant)))
         (should-not (string-match-p "running make test" text))
         ;; The command survives completion; the status icon shows done.
         (should (string-match-p "terminal: make test" text))
         (should (string-match-p "1.2s" text))
         (should (string-match-p "💻" text))
         (should (equal (plist-get (nth 1 entries) :content) "Thinking…"))
         (should (equal (plist-get (nth 2 entries) :status) "completed")))))))

(ert-deftest hermes-chat-collapses-multiline-transient-output ()
  "A multiline tool/status entry collapses to a one-line toggle, like thinking."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_p cb) (setq callback cb) 'fake-process)))
       (insert "run script")
       (hermes-chat-send)
       (funcall callback
                '(:type tool :tool-call-id "t1" :name "terminal" :status "running"
                        :context "set -e\ncd /repo\ngit status"))
       (let ((text (buffer-string)))
         (should (string-match-p "▸" text))
         (should (string-match-p "terminal: set -e" text))
         (should-not (string-match-p "git status" text)))
       (hermes-test--push-button-labeled "terminal: set -e")
       (let ((text (buffer-string)))
         (should (string-match-p "▾" text))
         (should (string-match-p "git status" text)))))))

(ert-deftest hermes-chat-collapses-and-toggles-commentary-events ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type commentary
                  :event "reasoning.delta"
                  :content "I"))
       (funcall callback
                '(:type commentary
                  :event "reasoning.delta"
                  :content " need"))
       (let ((text (buffer-string))
             (entries (hermes-chat--entries)))
         (should (string-match-p "▸ Reasoning" text))
         (should-not (string-match-p "I need" text))
         (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                entries)
                        '(user commentary assistant)))
         (should (equal (plist-get (nth 1 entries) :content) "I need")))
       (hermes-test--should-have-face "Reasoning" 'shadow)
       (hermes-test--push-button-labeled "Reasoning")
       (let ((text (buffer-string)))
         (should (string-match-p "▾ Reasoning" text))
         (should (string-match-p "I need" text)))
       (hermes-test--should-have-face "I need" 'shadow)
       (hermes-test--push-button-labeled "Reasoning")
       (should-not (string-match-p "I need" (buffer-string)))))))

(ert-deftest hermes-chat-cleans-commentary-token-newline-noise ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (dolist (chunk '("I\n" " need\n" " to\n" " respond\n" " to\n" " \"hello.\""))
         (funcall callback
                  (list :type 'commentary
                        :event "reasoning.delta"
                        :content chunk)))
       (hermes-test--push-button-labeled "Reasoning")
       (let ((text (buffer-string)))
         (should (string-match-p "I need to respond to \\\"hello\.\\\"" text))
         (should-not (string-match-p "I\n need\n to" text)))))))

(ert-deftest hermes-chat-empty-thinking-delta-clears-to-running ()
  "An empty `thinking.delta' clears the spinner instead of showing \"Thinking\"."
  (should (equal (hermes-chat--turn-header-props
                  '(:type thinking :event "thinking.delta" :content ""))
                 '(:status running :activity nil)))
  (should (equal (hermes-chat--turn-header-props
                  '(:type thinking :event "thinking.delta"
                          :content "(◔_◔) pondering..."))
                 '(:status thinking :activity "(◔_◔) Pondering"))))

(ert-deftest hermes-chat-commentary-header-labels-reasoning ()
  "Streamed reasoning drives a \"Reasoning\" header activity, not \"Thinking\"."
  (should (equal (hermes-chat--turn-header-props
                  '(:type commentary :event "reasoning.delta" :content "x"))
                 '(:status running :activity "Reasoning"))))

(ert-deftest hermes-chat-reasoning-available-keeps-streamed-reasoning ()
  "A `reasoning.available' preview never shrinks already-streamed reasoning."
  (let ((entry '(:role commentary :content "Step A. Step B. full reasoning"))
        (event '(:type commentary :event "reasoning.available")))
    (should (equal (hermes-chat--updated-transport-content entry event "short")
                   "Step A. Step B. full reasoning")))
  (let ((entry '(:role commentary :content ""))
        (event '(:type commentary :event "reasoning.available")))
    (should (equal (hermes-chat--updated-transport-content entry event "preview")
                   "preview"))))

(ert-deftest hermes-chat-renders-indexed-tool-events-as-ewoc-entries ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                (hermes-transport-normalize-event
                 '((type . "ToolCallChunk")
                   (tool_name . "read_file")
                   (preview . "AGENTS.md")
                   (index . 0))))
       (funcall callback
                (hermes-transport-normalize-event
                 '((type . "ToolCallChunk")
                   (tool_name . "read_file")
                   (preview . "Makefile")
                   (index . 1))))
       (funcall callback
                (hermes-transport-normalize-event
                 '((type . "ToolCallFinished")
                   (tool_name . "read_file")
                   (duration . 0.4)
                   (ok . t)
                   (index . 0))))
       (let ((tools (cl-remove-if-not
                     (lambda (entry) (eq (plist-get entry :role) 'tool))
                     (hermes-chat--entries))))
         (should (= (length tools) 2))
         (should (equal (plist-get (car tools) :status) "completed"))
         (should (equal (plist-get (cadr tools) :status) "running"))
         (should (string-match-p "📖 read_file  0.4s"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-header-shows-status-and-omits-tool-activity ()
  "The header keeps the status detail and never surfaces tool commands."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (should (string-match-p "Waiting" (hermes-test--header-line-string)))
       (funcall callback
                '(:type status
			:status-key "lifecycle"
			:status "running"
			:content "Thinking"))
       (should (string-match-p "Thinking" (hermes-test--header-line-string)))
       (funcall callback
                '(:type tool
			:tool-call-id "tool-1"
			:name "terminal"
			:status "running"
			:preview "make test"))
       (let ((header (hermes-test--header-line-string)))
         (should (string-match-p "Thinking" header))
         (should-not (string-match-p "terminal: make test" header)))
       ;; The tool stays out of the header but is still tracked for the
       ;; dashboard's per-session tool list.
       (should (hermes-chat--active-tool-summaries))
       (funcall callback '(:type done))
       (let ((header (hermes-test--header-line-string)))
         (should (string-match-p "Ready" header))
         (should-not (string-match-p "terminal: make test" header)))))))

(ert-deftest hermes-chat-rename-updates-buffer-and-title ()
  "Renaming sets the title and the profile-qualified buffer name, trimming space."
  (hermes-test-with-chat-buffer
   (hermes-chat-rename "  My Project  ")
   (should (equal hermes-chat--title "My Project"))
   (should hermes-chat--title-manual-p)
   (should (equal (buffer-name) "*Hermes: default: My Project*"))))

(ert-deftest hermes-chat-rename-rejects-empty-title ()
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-rename "   ") :type 'user-error)))

(ert-deftest hermes-chat-rename-pushes-server-title-when-attached ()
  "An attached session pushes `session.title' with the live session id."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-1")
   (let (sent)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-dashboard-transport-session-title)
                (lambda (_client &rest args) (setq sent args))))
       (hermes-chat-rename "Renamed"))
     (should (equal (plist-get sent :session-id) "sid-1"))
     (should (equal (plist-get sent :title) "Renamed")))))

(ert-deftest hermes-chat-buffer-name-for-title-formats ()
  "Buffer names carry the profile, plus the title once present."
  (should (equal (hermes-chat--buffer-name-for-title "coder" nil)
                 "*Hermes: coder*"))
  (should (equal (hermes-chat--buffer-name-for-title "coder" "Fix bug")
                 "*Hermes: coder: Fix bug*"))
  (should (equal (hermes-chat--buffer-name-for-title nil "Fix bug")
                 "*Hermes: default: Fix bug*"))
  (should (equal (hermes-chat--buffer-name-for-title nil "")
                 "*Hermes: default*")))

(ert-deftest hermes-chat-prompts-profile-and-names-buffer ()
  "M-x hermes-chat reads a profile and names the buffer after it."
  (cl-letf (((symbol-function 'hermes-chat--read-profile)
             (lambda () "coder")))
    (let ((buffer (call-interactively #'hermes-chat)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal hermes-chat--profile "coder"))
            (should (string-prefix-p "*Hermes: coder" (buffer-name))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-blank-profile-names-buffer-default ()
  "A blank profile yields the default profile name and no stored profile."
  (let ((buffer (hermes-chat "")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not hermes-chat--profile)
          (should (string-prefix-p "*Hermes: default" (buffer-name))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-should-apply-title-p-rules ()
  "A fetched title applies only when non-empty, changed, and not manual."
  (should (hermes-chat--should-apply-title-p "New" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "New" "Old" t))
  (should-not (hermes-chat--should-apply-title-p "" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "Same" "Same" nil))
  (should-not (hermes-chat--should-apply-title-p nil "Old" nil)))

(ert-deftest hermes-chat-done-refreshes-session-title ()
  "A completed turn fetches the server title and renames the buffer, no push."
  (let ((client (hermes-test--dashboard-client))
        callback (pushes 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _a) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_c _t &rest _a) nil))
              ((symbol-function 'hermes-dashboard-transport-session-title-fetch)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve) '((title . "Auto Title")))))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _a) (setq pushes (1+ pushes)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "hi")
         (hermes-chat-send)
         (funcall callback '(:type done))
         ;; The title fetch is deferred off the event handler; let it run.
         (sit-for 0.05)
         (should (equal (buffer-name) "*Hermes: default: Auto Title*"))
         (should (= pushes 0)))))))

(ert-deftest hermes-chat-manual-title-survives-refresh ()
  "A manually set title is not overwritten by the automatic refresh."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
               (lambda () t))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _a) nil))
              ((symbol-function 'hermes-dashboard-transport-session-title-fetch)
               (lambda (&rest _a) (setq fetches (1+ fetches)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-active-session-id "sid")
       (hermes-chat-rename "Pinned")
       (should hermes-chat--title-manual-p)
       (hermes-chat--maybe-refresh-session-title)
       (should (= fetches 0))
       (should (equal hermes-chat--title "Pinned"))))))

(ert-deftest hermes-chat-snapshot-prefers-title ()
  "The dashboard snapshot uses the chat title over the buffer name."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--title "Pinned")
   (should (equal (plist-get (hermes-chat--dashboard-snapshot) :title)
                  "Pinned"))))

(ert-deftest hermes-chat-switch-offers-and-selects-live-buffer ()
  "The switcher lists live chat buffers and switches to the chosen one."
  (hermes-test-with-chat-buffer
   (let ((target (current-buffer)))
     (should (memq target (hermes-chat--live-buffers)))
     (with-temp-buffer
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) (buffer-name target))))
         (call-interactively #'hermes-switch-to-chat))
       (should (eq (current-buffer) target))))))

(ert-deftest hermes-chat-session-info-updates-header-without-entry ()
  "`session.info' sets the header model but adds no transcript entry."
  (hermes-test-with-chat-buffer
   (let ((before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (hermes-chat--handle-transport-event
      "a1" '(:type status :event "session.info" :status "ready"
             :model "gpt-5.5" :agent-name "openai-codex"))
     (should (string-match-p "gpt-5.5" (hermes-test--header-line-string)))
     (should (= before (length (ewoc-collect hermes-chat--ewoc #'identity)))))))

(ert-deftest hermes-chat-message-start-status-adds-no-entry ()
  "Low-value `message.start' status updates do not enter the transcript."
  (hermes-test-with-chat-buffer
   (let ((before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (dolist (event '(("message.start")
                     ("message_start")
                     ("message.start" "started")
                     ("message_start" "message_start: started")))
       (pcase-let ((`(,name ,content) event))
         (hermes-chat--handle-transport-event
          "a1" `(:type status :event ,name :status "started"
                         ,@(and content (list :content content))))))
     (should (= before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (should-not (string-match-p "message[ _]start"
                                 (downcase (buffer-string))))
     (should-not (string-match-p "message start"
                                 (downcase (hermes-test--header-line-string)))))))

(ert-deftest hermes-chat-progress-updates-preserve-draft-and-streaming ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type progress
			:tool-call-id "read-1"
			:name "read_file"
			:status "running"
			:content "reading AGENTS.md"))
       (insert "draft survives")
       (funcall callback '(:type delta :content "answer"))
       (funcall callback
                '(:type progress
			:tool-call-id "read-1"
			:name "read_file"
			:status "running"
			:content "read 40 lines"))
       (should (equal (hermes-chat-input-string) "draft survives"))
       (insert " more")
       (should (equal (hermes-chat-input-string) "draft survives more"))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :status) 'streaming))
         (should (equal (plist-get assistant :content) "answer")))
       (funcall callback '(:type done))
       (let* ((entries (hermes-chat--entries))
              (progress (cl-find 'progress entries
                                 :key (lambda (entry)
                                        (plist-get entry :role)))))
         (should (equal (plist-get progress :status) 'done))
         (should (string-match-p "✓ 📖 read_file: read 40 lines"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-progress-keys-are-turn-local ()
  (let (callback first-callback second-callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (setq first-callback callback)
       (funcall first-callback
                '(:type progress
			:tool-call-id "same-tool"
			:name "read_file"
			:status "running"
			:content "first turn"))
       (funcall first-callback '(:type done))
       (insert "second")
       (hermes-chat-send)
       (setq second-callback callback)
       (funcall second-callback
                '(:type progress
			:tool-call-id "same-tool"
			:name "read_file"
			:status "running"
			:content "second turn"))
       (let* ((entries (hermes-chat--entries))
              (progress-entries
               (cl-remove-if-not
                (lambda (entry) (eq (plist-get entry :role) 'progress))
                entries))
              (text (buffer-string)))
         (should (= (length progress-entries) 2))
         (should (string-match-p "read_file: first turn" text))
         (should (string-match-p "read_file: second turn" text))
         (should-not (equal (hermes-chat--entry-assistant-id
                             (car progress-entries))
                            (hermes-chat--entry-assistant-id
                             (cadr progress-entries)))))))))

(ert-deftest hermes-chat-error-settles-active-progress ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type progress
			:tool-call-id "tool-1"
			:name "terminal"
			:status "running"
			:content "running"))
       (funcall callback '(:type error :content "boom"))
       (let* ((entries (hermes-chat--entries))
              (assistant (cadr entries))
              (progress (cl-find 'progress entries
                                 :key (lambda (entry)
                                        (plist-get entry :role)))))
         (should (equal (plist-get assistant :status) 'error))
         (should (equal (plist-get progress :status) 'error))
         (should (string-match-p "! 💻 terminal: running" (buffer-string))))))))

(ert-deftest hermes-chat-done-settles-active-progress-and-clears-process ()
  "A done event settles active transport entries and clears the process handle."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (should (eq hermes-chat--process 'fake-process))
       (funcall callback
                '(:type progress :tool-call-id "tool-1" :name "terminal"
                        :status "running" :content "running"))
       (funcall callback '(:type done))
       (let* ((entries (hermes-chat--entries))
              (assistant (cadr entries))
              (progress (cl-find 'progress entries
                                 :key (lambda (entry) (plist-get entry :role)))))
         (should (equal (plist-get assistant :status) 'done))
         (should (equal (plist-get progress :status) 'done)))
       (should-not hermes-chat--process)
       (should-not hermes-chat--pending-assistant-id)))))

(ert-deftest hermes-chat-error-clears-process-handle ()
  "An error event clears the transport process handle and the pending id."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (should (eq hermes-chat--process 'fake-process))
       (funcall callback '(:type error :content "boom"))
       (should-not hermes-chat--process)
       (should-not hermes-chat--pending-assistant-id)))))

(ert-deftest hermes-chat-transport-removes-control-bytes-from-assistant-output ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback (list :type 'delta
                               :content (concat "\r\0hello"
                                                (string #x85)
                                                "\nλ\r")))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :content) "hello\nλ"))
         (should-not (string-match-p "\r" (buffer-string))))))))

(ert-deftest hermes-chat-transport-removes-session-id-lines ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                (list :type 'delta
                      :content "session_id: 20260614_223306_254697\nhello"))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :content) "hello"))
         (should-not (string-match-p "session_id:" (buffer-string))))))))

(ert-deftest hermes-chat-transport-removes-final-session-id-line ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback '(:type delta :content "session_id: trailing"))
       (funcall callback '(:type done))
       (let ((assistant (hermes-test--assistant-entry)))
         (should (equal (plist-get assistant :content) ""))
         (should-not (string-match-p "session_id:" (buffer-string))))))))

(ert-deftest hermes-chat-actions-popup-bound ()
  "C-c C-o opens the in-chat actions popup, which lists turn actions."
  (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-o")
              #'hermes-chat-actions-map-popup))
  (should (fboundp 'hermes-chat-actions-map-popup))
  (should (eq (keymap-lookup hermes-chat-actions-map "s")
              #'hermes-chat-steer-message))
  (should (eq (keymap-lookup hermes-chat-actions-map "i")
              #'hermes-chat-interrupt))
  (should (eq (keymap-lookup hermes-chat-actions-map "m")
              #'hermes-chat-switch-model))
  (should (eq (keymap-lookup hermes-chat-actions-map "n")
              #'hermes-chat))
  (should (eq (keymap-lookup hermes-chat-actions-map "H")
              #'hermes-chat-handoff))
  (should (eq (keymap-lookup hermes-chat-actions-map "x")
              #'hermes-dashboard-reconnect))
  (let* ((rows (keymap-popup--meta hermes-chat-actions-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows)))
    (dolist (key '("n" "m" "x"))
      (should (cl-find key entries :key (lambda (entry)
                                         (plist-get entry :key))
                       :test #'equal)))))

(ert-deftest hermes-chat-catalog-candidates-extracts-names ()
  "Catalog candidates extract bare command names and descriptions."
  (let ((cands (hermes-chat--catalog-candidates
                '((categories . (((name . "Session")
                                  (pairs . (("/steer" "Steer the run")
                                            ("/model" "Switch model"))))))))))
    (should (equal (assoc "steer" cands) '("steer" . "Steer the run")))
    (should (assoc "model" cands))))

(ert-deftest hermes-chat-slash-capf-completes-in-input ()
  "The slash capf offers command names while typing /cmd in the input."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--commands-cache '(("steer" . "Steer") ("model" . "Switch")))
   (goto-char (point-max))
   (insert "/st")
   (let ((capf (hermes-chat--slash-capf)))
     (should capf)
     (should (member "steer" (nth 2 capf)))
     (should (= (nth 0 capf) (1+ (hermes-chat--input-position))))
     (should (= (nth 1 capf) (point))))))

(ert-deftest hermes-chat-slash-capf-inactive-off-command ()
  "The slash capf is inactive for non-slash input or inside arguments."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--commands-cache '(("steer" . "Steer")))
   (goto-char (point-max))
   (insert "hello")
   (should-not (hermes-chat--slash-capf))
   (hermes-chat--delete-input-tail)
   (insert "/steer now")
   (should-not (hermes-chat--slash-capf))))

(ert-deftest hermes-chat-input-uses-separator-not-prompt ()
  "The input area sits below a separator rule, with no `> ' prompt prefix."
  (hermes-test-with-chat-buffer
   (let ((footer (buffer-substring-no-properties
                  (point-min) (hermes-chat--input-position))))
     (should-not (string-match-p "> \\'" footer)))
   (should (eq (get-text-property (- (hermes-chat--input-position) 2) 'face)
               'hermes-chat-separator))
   (goto-char (point-max))
   (insert "hello")
   (should (equal (hermes-chat-input-string) "hello"))))

(ert-deftest hermes-chat-markdown-keeps-markup-visible ()
  "Markdown markers keep their faces but are never hidden, for easy copying."
  (let ((s (hermes-chat--fontify-markdown-string "say *hello* and `code`")))
    (should (string-match-p "\\*hello\\*" s))
    (should (string-match-p "`code`" s))
    (dotimes (i (length s))
      (should-not (get-text-property i 'invisible s)))))

(ert-deftest hermes-chat-shows-inline-diff-as-view-diff-link ()
  "An inline unified diff is replaced by a View Diff link that opens the diff."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Changed:\n"
             "--- a/file.txt\n"
             "+++ b/file.txt\n"
             "@@ -1,2 +1,2 @@\n"
             " keep\n"
             "-old-inline\n"
             "+new-inline\n"
             "Done.")
     'done))
   (should-not (string-match-p "-old-inline" (buffer-string)))
   (should (string-match-p "Done." (buffer-string)))
   (hermes-test--should-have-face "View Diff" 'link)
   (let ((diff (hermes-test--view-diff-content)))
     (should (string-match-p "-old-inline" diff))
     (should (string-match-p "+new-inline" diff)))))

(ert-deftest hermes-chat-background-complete-renders-view-result-link ()
  "A `background' event renders a persistent #N notice with a View Result link."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--background-counter 1
         hermes-chat--background-tasks
         (list (cons "bg_x" (list :number 1 :preview "do you have x_search?"))))
   (hermes-chat--handle-background-complete
    (list :type 'background :task-id "bg_x"
          :content "Yes, x_search is available."))
   ;; The launching task is consumed once its result is delivered.
   (should-not (assoc "bg_x" hermes-chat--background-tasks))
   (let ((entry (cl-find-if (lambda (e) (eq (plist-get e :role) 'background))
                            (hermes-chat--entries))))
     (should entry)
     (should (equal (plist-get (plist-get entry :metadata) :number) 1))
     (should (equal (plist-get (plist-get entry :metadata) :preview)
                    "do you have x_search?")))
   (should (string-match-p "Background #1 done" (buffer-string)))
   ;; The full answer is not inline; it opens in a dedicated buffer.
   (should-not (string-match-p "x_search is available" (buffer-string)))
   (hermes-test--should-have-face "View Result" 'link)
   (unwind-protect
       (progn
         (hermes-test--push-button-labeled "View Result")
         (should (get-buffer "*hermes-bg #1*"))
         (with-current-buffer "*hermes-bg #1*"
           (should (string-match-p
                    "x_search is available"
                    (buffer-substring-no-properties (point-min) (point-max))))))
     (when (get-buffer "*hermes-bg #1*")
       (kill-buffer "*hermes-bg #1*")))))

(ert-deftest hermes-chat-background-complete-without-record-still-renders ()
  "A background result with no recorded task still renders with the counter value."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--background-counter 1
         hermes-chat--background-tasks nil)
   (hermes-chat--handle-background-complete
    (list :type 'background :task-id "bg_unknown" :content "done"))
   (should (string-match-p "Background #1 done" (buffer-string)))))

(ert-deftest hermes-chat-background-complete-stays-above-pending-reply ()
  "A result arriving mid-turn is inserted above the live assistant reply."
  (hermes-test-with-chat-buffer
   (let* ((assistant (hermes-chat--make-entry 'assistant "" 'pending))
          (assistant-id (plist-get assistant :id)))
     (hermes-chat--insert-entry assistant)
     (setq hermes-chat--pending-assistant-id assistant-id)
     (hermes-chat--handle-background-complete
      (list :type 'background :task-id "bg_z" :content "side result"))
     (let ((roles (mapcar (lambda (e) (plist-get e :role))
                          (hermes-chat--entries))))
       (should (< (cl-position 'background roles)
                  (cl-position 'assistant roles)))))))

(ert-deftest hermes-chat-streaming-content-skips-markdown-and-diff ()
  "A streaming entry stays raw; only a settled entry renders diffs/markdown."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1 @@\n-old\n+new\n")
     'streaming))
   (should (string-match-p "-old" (buffer-string)))
   (should-not (string-match-p "View Diff" (buffer-string)))))

(ert-deftest hermes-chat-shows-inline-diff-without-final-newline-as-link ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Changed:\n"
             "@@ -1 +1 @@\n"
             "-old-no-final-newline\n"
             "+new-no-final-newline")
     'done))
   (should-not (string-match-p "-old-no-final-newline" (buffer-string)))
   (let ((diff (hermes-test--view-diff-content)))
     (should (string-match-p "-old-no-final-newline" diff))
     (should (string-match-p "+new-no-final-newline" diff)))))

(ert-deftest hermes-chat-stops-inline-diff-link-at-hunk-counts ()
  "The trailing non-diff line stays in the transcript, out of the diff."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Changed:\n"
             "@@ -1 +1 @@\n"
             "-old-counted\n"
             "+new-counted\n"
             "+ ordinary follow-up")
     'done))
   (should (string-match-p "ordinary follow-up" (buffer-string)))
   (let ((diff (hermes-test--view-diff-content)))
     (should (string-match-p "-old-counted" diff))
     (should-not (string-match-p "ordinary follow-up" diff)))))

(ert-deftest hermes-chat-shows-fenced-diffs-as-view-diff-links ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Fenced changes:\n"
             "```diff\n"
             "-old-diff-fence\n"
             "+new-diff-fence\n"
             "```\n"
             "```patch\n"
             "-old-patch-fence\n"
             "+new-patch-fence\n"
             "```\n"
             "after fences")
     'done))
   (should-not (string-match-p "-old-diff-fence" (buffer-string)))
   (should-not (string-match-p "```" (buffer-string)))
   (should (string-match-p "after fences" (buffer-string)))
   (should (= 2 (hermes-test--count-buttons-labeled "View Diff")))
   (let ((diff (hermes-test--view-diff-content)))
     (should (string-match-p "-old-diff-fence" diff)))))

(ert-deftest hermes-chat-does-not-linkify-ordinary-plus-minus-lines ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     "Ordinary chat:\n- remove clutter\n+ add clarity\nNo hunk header."
     'done))
   (should-not (string-match-p "View Diff" (buffer-string)))
   (should (string-match-p "remove clutter" (buffer-string)))
   (should (string-match-p "add clarity" (buffer-string)))))

(ert-deftest hermes-chat-shows-structured-diff-and-status-events-as-links ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "show a patch")
       (hermes-chat-send)
       (funcall callback
                '(:type diff
			:content "--- a/diff-event\n+++ b/diff-event\n@@ -1 +1 @@\n-diff-event-old\n+diff-event-new\n"))
       (funcall callback
                '(:type status
			:status-key "patch-preview"
			:status "running"
			:content "--- a/status-event\n+++ b/status-event\n@@ -1 +1 @@\n-status-event-old\n+status-event-new\n"))
       (should-not (string-match-p "diff-event-old" (buffer-string)))
       (should-not (string-match-p "status-event-old" (buffer-string)))
       (should (= 2 (hermes-test--count-buttons-labeled "View Diff")))
       (let ((diff (hermes-test--view-diff-content)))
         (should (string-match-p "diff-event-old" diff)))))))

(ert-deftest hermes-chat-strips-ansi-before-diff-link ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Diff:\n"
             "\e[38;2;218;165;32m--- a/ansi.txt\e[0m\n"
             "\e[38;2;218;165;32m+++ b/ansi.txt\e[0m\n"
             "\e[38;2;139;134;130m@@ -1 +1,2 @@\e[0m\n"
             "\e[38;2;184;134;11m existing line\e[0m\n"
             "\e[38;2;255;255;255;48;2;19;87;20m+ansi-added\e[0m")
     'done))
   (should-not (string-match-p "38;2" (buffer-string)))
   (should-not (string-match-p "\\[0m" (buffer-string)))
   (let ((diff (hermes-test--view-diff-content)))
     (should (string-match-p "+ansi-added" diff))
     (should-not (string-match-p "38;2" diff)))))

(ert-deftest hermes-chat-strips-split-ansi-before-diff-link ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "show split ansi patch")
       (hermes-chat-send)
       (funcall callback
                '(:type delta
                  :content "Diff:\n\e[38;2;218;165;32m--- a/split.txt\e[0m\n\e[38;2;218;165;32m+++ b/split.txt\e[0m\n\e[38;2;139;134;130m@@ -1 +1,2 @@\e[0m\n\e[38;2;184;134;11m existing line\e[0m\n\e[38;2;255;255"))
       (funcall callback
                '(:type delta
                  :content ";255;48;2;19;87;20m+split-ansi-added\e[0m"))
       (funcall callback '(:type done))
       (should-not (string-match-p "38;2" (buffer-string)))
       (should-not (string-match-p "\\[0m" (buffer-string)))
       (let ((diff (hermes-test--view-diff-content)))
         (should (string-match-p "+split-ansi-added" diff)))))))

(ert-deftest hermes-chat-scopes-split-ansi-to-assistant-stream ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "show interleaved ansi patch")
       (hermes-chat-send)
       (funcall callback
                '(:type delta
                  :content "Diff:\n--- a/interleaved.txt\n+++ b/interleaved.txt\n@@ -1 +1,2 @@\n existing line\n\e[38;2;255;255"))
       (funcall callback
                '(:type commentary
                  :event "reasoning.delta"
                  :content "Thinking"))
       (funcall callback
                '(:type delta
                  :content ";255;48;2;19;87;20m+interleaved-added\e[0m"))
       (let ((assistant (hermes-test--assistant-entry))
             (commentary (cl-find-if
                          (lambda (entry)
                            (eq (plist-get entry :role) 'commentary))
                          (hermes-chat--entries))))
         (should (string-match-p "+interleaved-added"
                                 (plist-get assistant :content)))
         (should-not (string-match-p "38;2" (plist-get assistant :content)))
         (should (equal (plist-get commentary :content) "Thinking")))
       (funcall callback '(:type done))
       (let ((diff (hermes-test--view-diff-content)))
         (should (string-match-p "+interleaved-added" diff)))))))

(ert-deftest hermes-chat-clears-split-ansi-before-terminal-event ()
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_prompt cb)
              (setq callback cb)
              'fake-process)))
       (insert "finish after partial ansi")
       (hermes-chat-send)
       (funcall callback '(:type delta :content "\e[38;2;255;255"))
       (funcall callback '(:type done :content "hello"))
       (should (equal (plist-get (hermes-test--assistant-entry) :content)
                      "hello"))))))

(ert-deftest hermes-chat-resume-renders-prior-messages ()
  "Resuming a session renders its prior user/assistant/tool messages."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
             (lambda (&rest _) (hermes-test--dashboard-client)))
            ((symbol-function 'hermes-dashboard-transport-session-resume)
             (lambda (_client _sid &rest args)
               (funcall (plist-get args :resolve)
                        '((session_id . "live-1")
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
            (should (string-match-p "hi there" (buffer-string)))
            (should (string-match-p "hello back" (buffer-string)))
            (should (string-match-p "terminal: make test" (buffer-string)))
            (should (equal hermes-chat--dashboard-active-session-id "live-1")))
        (kill-buffer buffer)))))

(ert-deftest hermes-chat-send-queues-while-busy ()
  (let (sent callbacks)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt cb)
              (push prompt sent)
              (push cb callbacks)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (insert "second")
       (hermes-chat-send)
       (should (equal sent '("first")))
       (should (equal hermes-chat--queued-message "second"))
       (should (equal (hermes-chat-input-string) ""))
       (funcall (car (last callbacks)) '(:type done))
       (should (equal sent '("second" "first")))))))

(ert-deftest hermes-chat-send-preserves-input-when-queue-full ()
  (let (sent)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt _callback)
              (push prompt sent)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (insert "second")
       (hermes-chat-send)
       (should (equal hermes-chat--queued-message "second"))
       (insert "third")
       (let ((error (should-error (hermes-chat-send) :type 'user-error)))
         (should (string-match-p "already queued" (error-message-string error))))
       (should (equal sent '("first")))
       (should (equal hermes-chat--queued-message "second"))
       (should (equal (hermes-chat-input-string) "third"))))))

(ert-deftest hermes-chat-busy-controls-remain-available ()
  (hermes-test-with-chat-buffer
   (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
     (insert "first")
     (hermes-chat-send)
     (let ((message (hermes-chat--busy-message)))
       (should (string-match-p "interrupt" message))
       (should (string-match-p "queue" message))
       (should (string-match-p "steer" message))
       (should (string-match-p "new chat" message)))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-i")
                 #'hermes-chat-interrupt))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-q")
                 #'hermes-chat-queue-message))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-s")
                 #'hermes-chat-steer-message))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-n")
                 #'hermes-chat)))))

(ert-deftest hermes-chat-queues-message-while-pending ()
  (let (sent callbacks)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt cb)
              (push prompt sent)
              (push cb callbacks)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (hermes-chat-queue-message "second")
       (should (equal sent '("first")))
       (funcall (car (last callbacks)) '(:type done))
       (should (equal sent '("second" "first")))
       (should hermes-chat--pending-assistant-id)
       (let ((roles (mapcar (lambda (entry) (plist-get entry :role))
                            (hermes-chat--entries))))
         (should (equal roles '(user assistant status user assistant))))))))

(ert-deftest hermes-chat-new-buffer-while-pending ()
  (let (original new)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
       (setq original (current-buffer))
       (insert "first")
       (hermes-chat-send)
       (setq new (hermes-chat--new-buffer))
       (unwind-protect
           (progn
             (should (buffer-live-p new))
             (with-current-buffer original
               (should hermes-chat--pending-assistant-id))
             (with-current-buffer new
               (should (derived-mode-p 'hermes-chat-mode))
               (should-not hermes-chat--pending-assistant-id)
               (should-not hermes-chat--session-id)
               (should (equal (hermes-chat-input-string) ""))))
         (when (buffer-live-p new)
           (kill-buffer new)))))))

(ert-deftest hermes-chat-keeps-pending-reply-guard ()
  (let (sent)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt _cb)
              (push prompt sent)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (should-error (hermes-chat--submit-content "second") :type 'user-error)
       (should (equal sent '("first")))))))

(ert-deftest hermes-chat-steers-active-turn-through-dashboard ()
  (let ((client (hermes-test--dashboard-client))
        callback submits steer-session steer-text)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits)))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client text &rest args)
                 (setq steer-session (plist-get args :session-id)
                       steer-text text)
                 (funcall (plist-get args :resolve)
                          '((status . "queued"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (should (functionp callback))
         (insert "/steer cite files")
         (hermes-chat-send)
         (should (equal submits '("first")))
         (should (equal steer-session "sid-active"))
         (should (equal steer-text "cite files"))
         (should (string-match-p "Steering: cite files" (buffer-string)))
         (should-not (string-match-p "Steer queued" (buffer-string)))
         ;; The steer line lands above the pending assistant reply.
         (let ((roles (mapcar (lambda (e) (plist-get e :role))
                              (hermes-chat--entries))))
           (should (< (cl-position 'status roles)
                      (cl-position 'assistant roles)))))))))

(ert-deftest hermes-chat-steer-shows-immediate-pending-before-ack ()
  "Steering shows an instant pending line before the gateway acknowledges it."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) nil))
              ;; Leave the steer RPC in flight: never resolve or reject.
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (&rest _) nil)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "/steer cite files")
         (hermes-chat-send)
         (should (string-match-p "Steering… cite files" (buffer-string)))
         (should-not (string-match-p "Steer queued" (buffer-string))))))))

(ert-deftest hermes-chat-steer-rejected-result-queues-message ()
  (let ((client (hermes-test--dashboard-client))
        submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits)))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client _text &rest args)
                 (funcall (plist-get args :resolve)
                          '((status . "rejected"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "/steer cite files")
         (hermes-chat-send)
         (should (equal submits '("first")))
         (should (equal hermes-chat--queued-message "cite files"))
         (should (string-match-p "Steer unavailable" (buffer-string)))
         (should-not (string-match-p "Steering" (buffer-string))))))))

(ert-deftest hermes-chat-interrupt-requests-dashboard-session-interrupt ()
  (let ((client (hermes-test--dashboard-client))
        interrupt-session submit-text)
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
               (lambda (_client text &rest _args)
                 (setq submit-text text)))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((status . "ok"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long running prompt")
         (hermes-chat-send)
         (should (equal submit-text "long running prompt"))
         (hermes-chat-interrupt)
         (should (equal interrupt-session "sid-active"))
         (should (eq (plist-get hermes-chat--status-state :status)
                     'interrupted))
         (should (string-match-p "Interrupt requested" (buffer-string)))
         (should (string-match-p "Interrupted"
                                 (hermes-test--header-line-string))))))))

(ert-deftest hermes-chat-interrupt-and-send-interrupts-even-when-empty ()
  "Empty input still interrupts; the interrupt must fire before any queue check."
  (let ((client (hermes-test--dashboard-client))
        (interrupts 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest _args) nil))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupts (1+ interrupts))
                 (funcall (plist-get args :resolve) '((status . "ok"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long task")
         (hermes-chat-send)
         (hermes-chat-interrupt-and-send)
         (should (= interrupts 1))
         (should-not hermes-chat--queued-message))))))

(ert-deftest hermes-chat-interrupt-and-send-queues-text-after-interrupt ()
  "With input text, the interrupt fires and the text is queued for the next turn."
  (let ((client (hermes-test--dashboard-client))
        (interrupts 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest _args) nil))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupts (1+ interrupts))
                 (funcall (plist-get args :resolve) '((status . "ok"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long task")
         (hermes-chat-send)
         (insert "follow up")
         (hermes-chat-interrupt-and-send)
         (should (= interrupts 1))
         (should (equal hermes-chat--queued-message "follow up")))))))

(ert-deftest hermes-chat-native-slash-handler-resolves-names-and-aliases ()
  "The resolver maps known names and aliases to handlers and others to nil."
  (should (functionp (hermes-chat--native-slash-handler "stop")))
  (should (functionp (hermes-chat--native-slash-handler "interrupt")))
  (should (functionp (hermes-chat--native-slash-handler "int")))
  (should (functionp (hermes-chat--native-slash-handler "clear")))
  (should (functionp (hermes-chat--native-slash-handler "reset")))
  (should-not (hermes-chat--native-slash-handler "definitely-not-a-command"))
  (should-not (hermes-chat--native-slash-handler nil)))

(ert-deftest hermes-chat-slash-stop-calls-process-stop ()
  "/stop runs the process.stop RPC rather than forwarding to the agent."
  (let ((client (hermes-test--dashboard-client))
        (stopped 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _a) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _a) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_c _t &rest _a) nil))
              ((symbol-function 'hermes-dashboard-transport-process-stop)
               (lambda (_c &rest args)
                 (setq stopped (1+ stopped))
                 (funcall (plist-get args :resolve) '((killed . 2))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "go")
         (hermes-chat-send)
         (insert "/stop")
         (hermes-chat-send)
         (should (= stopped 1)))))))

(ert-deftest hermes-chat-slash-clear-resets-transcript-and-session ()
  "/clear empties the transcript and forgets the live and durable session ids."
  (let ((client (hermes-test--dashboard-client))
        (stopped 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _a) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _a) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_c _t &rest _a) nil))
              ((symbol-function 'hermes-chat--stop-dashboard-client)
               (lambda () (setq stopped (1+ stopped))))
              ((symbol-function 'y-or-n-p) (lambda (&rest _a) t)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "remember this")
         (hermes-chat-send)
         (should hermes-chat--dashboard-active-session-id)
         (setq stopped 0)
         (insert "/clear")
         (hermes-chat-send)
         (should (= stopped 1))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--session-id)
         (should (equal (mapcar (lambda (e) (plist-get e :role))
                                (hermes-chat--entries))
                        '(status))))))))

(ert-deftest hermes-chat-unknown-slash-falls-through-to-gateway ()
  "An unknown slash command dispatches to the gateway, not a native handler."
  (let ((client (hermes-test--dashboard-client))
        slash-name)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _a) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _a) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_c &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_c _t &rest _a) nil))
              ((symbol-function 'hermes-chat--dashboard-slash-exec)
               (lambda (name _arg _raw) (setq slash-name name))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "go")
         (hermes-chat-send)
         (insert "/somethingcustom foo")
         (hermes-chat-send)
         (should (equal slash-name "somethingcustom")))))))

(ert-deftest hermes-chat-control-error-keeps-active-turn ()
  (let ((client (hermes-test--dashboard-client))
        callback last-frame submits first-assistant)
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
                 (push text submits))))
      (let ((hermes-transport-send-function #'hermes-transport-send)
            (hermes-dashboard-transport-websocket-send-function
             (lambda (_websocket text)
               (setq last-frame
                     (hermes-dashboard-transport--decode-frame text)))))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (setq first-assistant hermes-chat--pending-assistant-id)
         (insert "/steer cite files")
         (hermes-chat-send)
         (hermes-dashboard-transport--handle-frame
          client
          `((jsonrpc . "2.0")
            (id . ,(alist-get 'id last-frame))
            (error . ((message . "agent does not support steer")))))
         (should (equal hermes-chat--pending-assistant-id first-assistant))
         (should (equal hermes-chat--queued-message "cite files"))
         (should (equal submits '("first")))
         (funcall callback '(:type done :session-id "sid-active"))
         (should (equal submits '("cite files" "first"))))))))

(ert-deftest hermes-chat-slash-queue-drains-once ()
  (let ((client (hermes-test--dashboard-client))
        first-callback submits dispatch-name dispatch-arg)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq first-callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits)))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch-name name
                       dispatch-arg arg)
                 (funcall (plist-get args :resolve)
                          '((type . "send") (message . "second"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "/queue second")
         (hermes-chat-send)
         (should (equal dispatch-name "queue"))
         (should (equal dispatch-arg "second"))
         (should (equal submits '("first")))
         (funcall first-callback '(:type done :session-id "sid-active"))
         (should (equal submits '("second" "first")))
         (funcall first-callback '(:type done :session-id "sid-active"))
         (should (equal submits '("second" "first"))))))))

(ert-deftest hermes-chat-commands-catalog-rendering ()
  (let ((client (hermes-test--dashboard-client))
        catalog-requested
        (catalog '((categories
                    . (((name . "Session")
                        (pairs . (("/queue" "Queue next message")
                                  ("/steer" "Steer active run"))))))
                   (sub . (("goal" . ("status" "pause" "resume"))
                           ("config" . ("get" "set"))))
                   (warning . "skill discovery unavailable"))))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-commands-catalog)
               (lambda (_client &rest args)
                 (setq catalog-requested t)
                 (funcall (plist-get args :resolve) catalog))))
      (hermes-test-with-chat-buffer
       (insert "/commands")
       (hermes-chat-send)
       (should catalog-requested)
       (let ((text (buffer-string)))
         (should (string-match-p "Session" text))
         (should (string-match-p "/queue — Queue next message" text))
         (should (string-match-p "/steer — Steer active run" text))
         (should (string-match-p "skill discovery unavailable" text))
         (should (string-match-p "Subcommands" text))
         (should (string-match-p "/goal status" text))
         (should (string-match-p "/goal pause" text))
         (should (string-match-p "/config get" text)))))))

(ert-deftest hermes-chat-command-skill-sends-message ()
  (let ((client (hermes-test--dashboard-client))
        submit-text slash-command dispatch-name dispatch-arg)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submit-text text)))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client command &rest args)
                 (setq slash-command command)
                 (funcall (plist-get args :reject)
                          "skill command uses command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch-name name
                       dispatch-arg arg)
                 (funcall (plist-get args :resolve)
                          '((type . "skill")
                            (name . "demo")
                            (message . "use demo skill"))))))
      (hermes-test-with-chat-buffer
       (insert "/demo now")
       (hermes-chat-send)
       (should (equal slash-command "demo now"))
       (should (equal dispatch-name "demo"))
       (should (equal dispatch-arg "now"))
       (should (equal submit-text "use demo skill"))
       (let ((text (buffer-string)))
         (should (string-match-p "loading skill: demo" text))
         ;; the full skill payload is sent to the agent but never echoed
         (should-not (string-match-p "use demo skill" text)))))))

(ert-deftest hermes-chat-command-skill-queues-while-active ()
  (let ((client (hermes-test--dashboard-client)) callback submits)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits)))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :reject)
                          "skill command uses command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client _name _arg &rest args)
                 (funcall (plist-get args :resolve)
                          '((type . "skill")
                            (name . "demo")
                            (message . "use demo while busy"))))))
      (hermes-test-with-chat-buffer
       (insert "first")
       (hermes-chat-send)
       (insert "/demo now")
       (hermes-chat-send)
       (should (equal submits '("first")))
       (should (equal hermes-chat--queued-message "use demo while busy"))
       (should (string-match-p "loading skill: demo" (buffer-string)))
       (funcall callback '(:type done :session-id "sid-active"))
       (should (equal submits '("use demo while busy" "first")))))))

(ert-deftest hermes-chat-command-prefill-renders-notice ()
  (hermes-test-with-chat-buffer
   (hermes-chat--handle-command-result
    '((type . "prefill")
      (message . "edited prompt")
      (notice . "Undid one turn")))
   (should (equal (hermes-chat-input-string) "edited prompt"))
   (should (string-match-p "Undid one turn" (buffer-string)))))

(ert-deftest hermes-chat-slash-resume-running-skill-queues ()
  (let ((client (hermes-test--dashboard-client)) submit-text queued-result)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "stored-session")
                            (running . t)
                            (inflight . ((user . "remote prompt")
                                         (assistant . "")
                                         (streaming . t)))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submit-text text)))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :reject)
                          "skill command uses command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client _name _arg &rest args)
                 (funcall (plist-get args :resolve)
                          '((type . "skill")
                            (name . "demo")
                            (message . "use demo skill while remote turn runs"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/demo now")
       (hermes-chat-send)
       (setq queued-result hermes-chat--queued-message)
       (should-not submit-text)
       (should (equal queued-result "use demo skill while remote turn runs"))
       (should hermes-chat--pending-assistant-id)
       (should (string-match-p "Hermes session is still running"
                               (buffer-string)))
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (should (equal submit-text
                      "use demo skill while remote turn runs"))))))

(ert-deftest hermes-chat-slash-queue-resume-running-queues ()
  (let ((client (hermes-test--dashboard-client))
        dispatch-name dispatch-arg dispatch-session submits)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "stored-session")
                            (running . t)
                            (inflight . ((user . "remote prompt")
                                         (assistant . "")
                                         (streaming . t)))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits)))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch-name name
                       dispatch-arg arg
                       dispatch-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve)
                          '((type . "send")
                            (message . "queued prompt"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/queue queued prompt")
       (hermes-chat-send)
       (should (equal dispatch-name "queue"))
       (should (equal dispatch-arg "queued prompt"))
       (should (equal dispatch-session "sid-live"))
       (should-not submits)
       (should (equal hermes-chat--queued-message "queued prompt"))
       (should hermes-chat--pending-assistant-id)
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued prompt")))
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued prompt")))))))

(ert-deftest hermes-chat-queue-message-resume-running-queues ()
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "stored-session")
                            (running . t)
                            (inflight . ((user . "remote prompt")
                                         (assistant . "")
                                         (streaming . t)))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (push text submits))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (hermes-chat-queue-message "queued via key")
       (should-not submits)
       (should (equal hermes-chat--queued-message "queued via key"))
       (should hermes-chat--pending-assistant-id)
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued via key")))
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued via key")))))))

(ert-deftest hermes-chat-queue-message-resume-error-preserves-text ()
  (let ((client (hermes-test--dashboard-client)) submitted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (when-let* ((reject (plist-get args :reject)))
                   (funcall reject "resume failed"))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (setq submitted t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "queued via key")
       (hermes-chat-queue-message)
       (should-not submitted)
       (should (hermes-test--control-content-preserved-p "queued via key"))
       (should (string-match-p "resume failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-queue-resume-error-preserves-text ()
  (let ((client (hermes-test--dashboard-client)) dispatched submitted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (when-let* ((reject (plist-get args :reject)))
                   (funcall reject "resume failed"))))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (&rest _args)
                 (setq dispatched t)))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (setq submitted t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/queue queued prompt")
       (hermes-chat-send)
       (should-not dispatched)
       (should-not submitted)
       (should (hermes-test--control-content-preserved-p
                "queued prompt" "/queue queued prompt"))
       (should (string-match-p "resume failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-queue-start-error-preserves-text ()
  (let (dispatched signaled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args)
                 (user-error "dashboard failed")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (&rest _args)
                 (setq dispatched t))))
      (hermes-test-with-chat-buffer
       (insert "/queue queued prompt")
       (condition-case err
           (hermes-chat-send)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should-not dispatched)
       (should (hermes-test--control-content-preserved-p
                "queued prompt" "/queue queued prompt"))
       (should (string-match-p "dashboard failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-queue-create-error-preserves-text ()
  (let ((client (hermes-test--dashboard-client)) dispatched signaled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (when-let* ((reject (plist-get args :reject)))
                   (funcall reject "create failed"))))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (&rest _args)
                 (setq dispatched t))))
      (hermes-test-with-chat-buffer
       (insert "/queue queued prompt")
       (condition-case err
           (hermes-chat-send)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should-not dispatched)
       (should (hermes-test--control-content-preserved-p
                "queued prompt" "/queue queued prompt"))
       (should (string-match-p "create failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-steer-resume-error-preserves-text ()
  (let ((client (hermes-test--dashboard-client)) steered submitted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (when-let* ((reject (plist-get args :reject)))
                   (funcall reject "resume failed"))))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (&rest _args)
                 (setq steered t)))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (setq submitted t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/steer adjust current run")
       (hermes-chat-send)
       (should-not steered)
       (should-not submitted)
       (should (hermes-test--control-content-preserved-p
                "adjust current run" "/steer adjust current run"))
       (should (string-match-p "resume failed" (buffer-string)))))))

(ert-deftest hermes-chat-queue-message-start-error-preserves-text ()
  (let (signaled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args)
                 (user-error "dashboard failed"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "queued before start")
       (condition-case err
           (hermes-chat-queue-message)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should (hermes-test--control-content-preserved-p
                "queued before start"))
       (should (string-match-p "dashboard failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-start-error-preserves-text ()
  (let (signaled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args)
                 (user-error "dashboard failed"))))
      (hermes-test-with-chat-buffer
       (insert "/demo arg")
       (condition-case err
           (hermes-chat-send)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should (hermes-test--control-content-preserved-p "/demo arg"))
       (should (string-match-p "dashboard failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-create-error-preserves-text ()
  (let ((client (hermes-test--dashboard-client)) signaled slash-ran)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (when-let* ((reject (plist-get args :reject)))
                   (funcall reject "create failed"))))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _args)
                 (setq slash-ran t))))
      (hermes-test-with-chat-buffer
       (insert "/demo arg")
       (condition-case err
           (hermes-chat-send)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should-not slash-ran)
       (should (hermes-test--control-content-preserved-p "/demo arg"))
       (should (string-match-p "create failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-steer-start-error-preserves-text ()
  (let (signaled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args)
                 (user-error "dashboard failed"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/steer adjust before start")
       (condition-case err
           (hermes-chat-send)
         (error (setq signaled (error-message-string err))))
       (should-not signaled)
       (should (hermes-test--control-content-preserved-p
                "adjust before start" "/steer adjust before start"))
       (should (string-match-p "dashboard failed" (buffer-string)))))))

(ert-deftest hermes-chat-slash-steer-resume-running-steers ()
  (let ((client (hermes-test--dashboard-client))
        steer-session steer-text submit-text)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "stored-session"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "stored-session")
                            (running . t)
                            (inflight . ((user . "remote prompt")
                                         (assistant . "")
                                         (streaming . t)))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submit-text text)))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client text &rest args)
                 (setq steer-session (plist-get args :session-id)
                       steer-text text)
                 (funcall (plist-get args :resolve)
                          '((status . "queued"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (insert "/steer adjust current run")
       (hermes-chat-send)
       (should-not submit-text)
       (should (equal steer-session "sid-live"))
       (should (equal steer-text "adjust current run"))
       (should (string-match-p "Steering: adjust current run" (buffer-string)))))))

(ert-deftest hermes-chat-command-alias-dispatches-target ()
  (let ((client (hermes-test--dashboard-client)) dispatches)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :reject)
                          "use command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (push (cons name arg) dispatches)
                 (funcall (plist-get args :resolve)
                          (if (equal name "short")
                              '((type . "alias") (target . "demo"))
                            '((type . "exec") (output . "alias target ran")))))))
      (hermes-test-with-chat-buffer
       (insert "/short now")
       (hermes-chat-send)
       (should (equal (nreverse dispatches)
                      '(("short" . "now") ("demo" . "now"))))
       (should (string-match-p "alias target ran" (buffer-string)))))))

(ert-deftest hermes-chat-command-output-renders-warning ()
  (hermes-test-with-chat-buffer
   (hermes-chat--handle-command-result
    '((type . "exec")
      (output . "command output")
      (warning . "command warning")))
   (let ((text (buffer-string)))
     (should (string-match-p "warning: command warning" text))
     (should (string-match-p "command output" text)))))

(ert-deftest hermes-chat-command-output-ignores-empty-body-fields ()
  (hermes-test-with-chat-buffer
   (hermes-chat--handle-command-result
    '((type . "exec")
      (output . "")
      (notice . "fallback notice")))
   (should (string-match-p "fallback notice" (buffer-string)))))

(ert-deftest hermes-chat-command-dispatch-output-renders ()
  (let ((client (hermes-test--dashboard-client)) slash-command dispatch-name dispatch-arg)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client command &rest args)
                 (setq slash-command command)
                 (funcall (plist-get args :reject)
                          "pending-input command: use command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch-name name
                       dispatch-arg arg)
                 (funcall (plist-get args :resolve)
                          '((type . "exec")
                            (output . "dispatch output"))))))
      (hermes-test-with-chat-buffer
       (insert "/foo dispatch output")
       (hermes-chat-send)
       (should (equal slash-command "foo dispatch output"))
       (should (equal dispatch-name "foo"))
       (should (equal dispatch-arg "dispatch output"))
       (should (string-match-p "dispatch output" (buffer-string)))))))

(ert-deftest hermes-chat-command-dispatch-rejection-renders-error ()
  (let ((client (hermes-test--dashboard-client)) dispatch-name dispatch-arg)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :reject)
                          "pending-input command: use command.dispatch")))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (setq dispatch-name name
                       dispatch-arg arg)
                 (funcall (plist-get args :reject)
                          "unknown command: nope"))))
      (hermes-test-with-chat-buffer
       (insert "/nope argument")
       (hermes-chat-send)
       (should (equal dispatch-name "nope"))
       (should (equal dispatch-arg "argument"))
       (should (equal (plist-get hermes-chat--status-state :status) 'error))
       (should (string-match-p "unknown command: nope" (buffer-string)))))))

(ert-deftest hermes-chat-dashboard-creates-session ()
  (let ((client (hermes-test--dashboard-client))
        start-callback create-resolve submit-client submit-text submit-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq start-callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (sent-client &rest args)
                 (should (eq sent-client client))
                 (setq create-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (sent-client text &rest args)
                 (setq submit-client sent-client
                       submit-text text
                       submit-args args))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (let ((chat-buffer (current-buffer)))
           (insert "hello dashboard")
           (hermes-chat-send)
           (should (functionp start-callback))
           (should (functionp create-resolve))
           (should-not submit-client)
           (with-temp-buffer
             (funcall create-resolve
                      '((session_id . "sid-live")
                        (stored_session_id . "sid-stored"))))
           (with-current-buffer chat-buffer
             (should (equal hermes-chat--session-id "sid-stored"))
             (should (equal (bound-and-true-p
                             hermes-chat--dashboard-active-session-id)
                            "sid-live"))
             (should hermes-chat--dashboard-session-ready-p)
             (should-not (hermes-dashboard-transport-client-session-id
                          client))
             (should-not (hermes-dashboard-transport-client-stored-session-id
                          client))
             (should (eq hermes-chat--process client))
             (should (eq submit-client client))
             (should (equal submit-text "hello dashboard"))
             (should (equal (plist-get submit-args :session-id)
                            "sid-live")))))))))

(ert-deftest hermes-chat-dashboard-submits-prompt ()
  (let ((client (hermes-test--dashboard-client))
        resumed-session resume-resolve submit-text submit-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (&rest _args)
                 (error "session.create should not run for resumed chat")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (sent-client session-id &rest args)
                 (should (eq sent-client client))
                 (setq resumed-session session-id
                       resume-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (setq submit-text text
                       submit-args args))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (let ((chat-buffer (current-buffer)))
           (setq hermes-chat--session-id "sid-stored")
           (insert "resume me")
           (hermes-chat-send)
           (should (equal resumed-session "sid-stored"))
           (should (functionp resume-resolve))
           (should-not submit-text)
           (with-temp-buffer
             (funcall resume-resolve
                      '((session_id . "sid-live")
                        (resumed . "sid-stored"))))
           (with-current-buffer chat-buffer
             (should (equal hermes-chat--session-id "sid-stored"))
             (should (equal (bound-and-true-p
                             hermes-chat--dashboard-active-session-id)
                            "sid-live"))
             (should hermes-chat--dashboard-session-ready-p)
             (should-not (hermes-dashboard-transport-client-session-id
                          client))
             (should-not (hermes-dashboard-transport-client-stored-session-id
                          client))
             (should (equal submit-text "resume me"))
             (should (equal (plist-get submit-args :session-id)
                            "sid-live")))))))))

(ert-deftest hermes-chat-dashboard-streams-events-into-ewoc ()
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-stream")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "stream please")
         (hermes-chat-send)
         (funcall callback
                  '(:type delta
                    :session-id "sid-other"
                    :content "ignored"))
         (funcall callback
                  '(:type status
                    :session-id "sid-other"
                    :status-key "run"
                    :status "running"
                    :content "Ignore me"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-other"
                    :tool-call-id "tool-other"
                    :name "terminal"
                    :status "running"
                    :preview "ignored"))
         (funcall callback
                  '(:type delta
                    :session-id "sid-stream"
                    :content "hello"))
         (funcall callback
                  '(:type status
                    :session-id "sid-stream"
                    :status-key "run"
                    :status "running"
                    :content "Thinking"))
         (funcall callback
                  '(:type status
                    :session-id "sid-stream"
                    :status-key "run"
                    :status "running"
                    :content "Still thinking"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-stream"
                    :tool-call-id "tool-1"
                    :name "terminal"
                    :status "running"
                    :preview "make test"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-stream"
                    :tool-call-id "tool-1"
                    :name "terminal"
                    :status "completed"
                    :duration 0.5))
         (funcall callback
                  '(:type delta
                    :session-id "sid-stream"
                    :content " world"))
         (funcall callback '(:type done :session-id "sid-stream"))
         (let* ((entries (hermes-chat--entries))
                (roles (mapcar (lambda (entry) (plist-get entry :role))
                               entries))
                (assistant (nth 3 entries))
                (status (nth 1 entries))
                (tool (nth 2 entries)))
           (should (equal roles '(user status tool assistant)))
           (should (equal (plist-get assistant :content) "hello world"))
           (should (equal (plist-get assistant :status) 'done))
           (should (equal (plist-get status :content) "Still thinking"))
           (should (equal (plist-get tool :status) "completed"))
           (should-not hermes-chat--pending-assistant-id)))))))

(ert-deftest hermes-chat-dashboard-collapses-reasoning-into-toggle ()
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-thinking")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "think first")
         (hermes-chat-send)
         (funcall callback
                  '(:type commentary
                    :session-id "sid-other"
                    :event "reasoning.delta"
                    :content "ignore this"))
         (dolist (chunk '("I\\n" " need\\n" " to inspect^J" " repo"))
           (funcall callback
                    (list :type 'commentary
                          :session-id "sid-thinking"
                          :event "reasoning.delta"
                          :content chunk)))
         (let ((collapsed (buffer-string)))
           (should (string-match-p "▸ Reasoning" collapsed))
           (should-not (string-match-p "inspect repo" collapsed)))
         (hermes-test--push-button-labeled "Reasoning")
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Reasoning" expanded))
           (should (string-match-p "I need to inspect repo" expanded))
           (should-not (string-match-p "\\\\n\|\\^J" expanded)))
         (funcall callback
                  '(:type commentary
                    :session-id "sid-thinking"
                    :event "reasoning.delta"
                    :content " and cite files"))
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Reasoning" expanded))
           (should (string-match-p "I need to inspect repo and cite files"
                                   expanded)))
         (funcall callback
                  '(:type delta
                    :session-id "sid-thinking"
                    :content "Clean answer"))
         (funcall callback '(:type done :session-id "sid-thinking"))
         (let* ((entries (hermes-chat--entries))
                (roles (mapcar (lambda (entry) (plist-get entry :role))
                               entries))
                (assistant (nth 2 entries))
                (commentary (nth 1 entries)))
           (should (equal roles '(user commentary assistant)))
           (should (= (cl-count 'commentary roles) 1))
           (should (equal (plist-get assistant :content) "Clean answer"))
           (should-not (string-match-p "inspect repo" (plist-get assistant :content)))
           (should (equal (plist-get commentary :content)
                          "I\\n need\\n to inspect^J repo and cite files"))))))))

(ert-deftest hermes-chat-queues-input-tail-during-stream ()
  (let ((client (hermes-test--dashboard-client))
        callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-tail")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "keep tail")
         (hermes-chat-send)
         (funcall callback '(:type delta :content "answer"))
         (insert "draft survives")
         (funcall callback
                  '(:type status
                    :status-key "run"
                    :status "running"
                    :content "Thinking"))
         (funcall callback '(:type delta :content " continues"))
         (should (equal (hermes-chat-input-string) "draft survives"))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :content)
                          "answer continues"))
           (should (equal (plist-get assistant :status) 'streaming)))
         (should hermes-chat--pending-assistant-id)
         (hermes-chat-send)
         (should (equal hermes-chat--queued-message "draft survives"))
         (should (equal (hermes-chat-input-string) "")))))))

(ert-deftest hermes-chat-dashboard-close-clears-pending-for-retry ()
  (let* ((client-1 (hermes-test--dashboard-client))
         (client-2 (hermes-test--dashboard-client))
         (clients (list client-1 client-2))
         callback first-callback second-callback
         resumed-session submit-sessions second-assistant-id)
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
         (should (equal submit-sessions '("sid-live-1")))
         (setf (hermes-dashboard-transport-client-websocket client-1) nil
               (hermes-dashboard-transport-client-ready-p client-1) nil)
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :status) 'error))
           (should (string-match-p "WebSocket closed"
                                   (plist-get assistant :content))))
         (should-not hermes-chat--pending-assistant-id)
         (insert "second")
         (hermes-chat-send)
         (setq second-callback callback
               second-assistant-id hermes-chat--pending-assistant-id)
         (should (equal resumed-session "sid-stored"))
         (should (equal submit-sessions '("sid-live-2" "sid-live-1")))
         (funcall first-callback '(:type error :content "late old error"))
         (should (equal hermes-chat--pending-assistant-id second-assistant-id))
         (funcall first-callback
                  '(:type status
                    :status "closed"
                    :content "late old close"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-live-2"))
         (funcall second-callback
                  '(:type delta
                    :session-id "sid-live-2"
                    :content "retry ok"))
         (funcall second-callback '(:type done :session-id "sid-live-2"))
         (let ((assistant (nth 3 (hermes-chat--entries))))
           (should (equal (plist-get assistant :content) "retry ok"))
           (should (equal (plist-get assistant :status) 'done)))
         (should-not hermes-chat--pending-assistant-id))))))

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
         (should (equal hermes-chat--queued-message "third"))
         (should (equal (hermes-chat-input-string) ""))
         (should (equal submit-sessions '("sid-live-1"))))))))

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
                   :choices ["once" "deny"]
                   :allow-permanent nil))
         (candidates (hermes-chat--approval-response-candidates prompt)))
    (should (equal (mapcar #'cdr candidates) '("once" "deny" nil)))
    (should (equal (mapcar #'car candidates)
                   '("Approve once" "Deny" "Cancel / ignore")))))

(ert-deftest hermes-chat-read-approval-response-omits-unavailable-always ()
  (let (seen-candidates)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq seen-candidates candidates)
                 "Deny")))
      (should (equal (hermes-chat--read-prompt-response
                      '(:prompt-type "approval" :allow-permanent nil))
                     "deny"))
      (should (member "Approve once" seen-candidates))
      (should (member "Approve for session" seen-candidates))
      (should (member "Deny" seen-candidates))
      (should (member "Cancel / ignore" seen-candidates))
      (should-not (member "Always approve" seen-candidates)))))

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

(ert-deftest hermes-chat-parses-approval-allow-permanent-flag ()
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt
     client "approval.request"
     '((command . "python risky.py")
       (description . "execute_code script execution")
       (allow_permanent . nil)))
    (let ((prompt (gethash "approval:sid-prompt" hermes-chat--pending-prompts)))
      (should prompt)
      (should (plist-member prompt :allow-permanent))
      (should-not (plist-get prompt :allow-permanent))
      (should-not (member "always"
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
        (hermes-chat-respond-to-prompt "req-clarify" "feature")
        (should (eq respond-client client))
        (should (equal respond-request "req-clarify"))
        (should (equal respond-answer "feature"))
        (should-not (gethash "req-clarify" hermes-chat--pending-prompts))))))

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
        deleted)
    (cl-letf (((symbol-function 'delete-process)
               (lambda (process) (setq deleted process)))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) new-client)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client old-client)
       (should (eq (hermes-chat--dashboard-start #'ignore) new-client))
       (should (eq deleted 'old-process))
       (should (eq hermes-chat--dashboard-client new-client))
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
           (should (equal (plist-get assistant :content) "Stopped")))
         (should-not hermes-chat--pending-assistant-id)
         (should (string-match-p "Interrupted"
                                 (hermes-test--header-line-string)))
         (should-not (string-match-p "Error"
                                     (hermes-test--header-line-string))))))))

(ert-deftest hermes-chat-thinking-activity-keeps-face-titlecases-verb ()
  "`thinking.delta' content keeps the kawaii face, drops dots, title-cases the verb."
  (should (equal (hermes-chat--thinking-activity "(◔_◔) pondering...")
                 "(◔_◔) Pondering"))
  (should (equal (hermes-chat--thinking-activity "( ͡° ͜ʖ ͡°) cogitating…")
                 "( ͡° ͜ʖ ͡°) Cogitating"))
  (should (equal (hermes-chat--thinking-activity "reasoning") "Reasoning"))
  (should (equal (hermes-chat--thinking-activity "") "Thinking"))
  (should (equal (hermes-chat--thinking-activity nil) "Thinking")))

(ert-deftest hermes-chat-thinking-event-updates-header-without-entry ()
  "A `thinking' event shows the face plus verb bare and adds no transcript entry."
  (hermes-test-with-chat-buffer
   (let ((before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (hermes-chat--handle-transport-event
      "a1" '(:type thinking :content "(◔_◔) musing..."))
     (let ((header (hermes-test--header-line-string)))
       (should (string-match-p "(◔_◔) Musing" header))
       (should-not (string-match-p "Running" header)))
     (should (= before (length (ewoc-collect hermes-chat--ewoc #'identity)))))))

(ert-deftest hermes-chat-renders-subagent-events-without-unknown-log ()
  (let (callback messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format-message format-string args) messages))))
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "inspect branches")
         (hermes-chat-send)
         (dolist (event '((:type commentary
                           :event "subagent.thinking"
                           :subagent-id "sa-1"
                           :content "(⌐■_■) synthesizing...")
                          (:type tool
                           :event "subagent.tool"
                           :subagent-id "sa-1"
                           :name "terminal"
                           :status "running"
                           :preview "git status")))
           (funcall callback event))
         (let ((entries (hermes-chat--entries)))
           (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                  entries)
                          '(user commentary tool assistant)))
           (should (equal (plist-get (nth 1 entries) :content)
                          "(⌐■_■) synthesizing..."))
           (should (equal (plist-get (nth 2 entries) :content)
                          "💻 terminal: git status")))
         (should-not (cl-some (lambda (line)
                                (string-match-p "Unknown Hermes transport event"
                                                line))
                              messages)))))))

(ert-deftest hermes-chat-surfaces-unknown-transport-events ()
  (let (callback messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format-message format-string args) messages))))
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "inspect")
         (hermes-chat-send)
         (funcall callback '(:type unknown
                             :event "alien.signal"
                             :raw ((payload . 1))))
         (let ((entries (hermes-chat--entries))
               (header (hermes-test--header-line-string)))
           (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                  entries)
                          '(user status assistant)))
           (should (string-match-p "Unknown Hermes transport event: alien.signal"
                                   (plist-get (nth 1 entries) :content)))
           (should (eq (plist-get (nth 1 entries) :status) 'error))
           (should (string-match-p "Error" header))
           (should (string-match-p "alien.signal" header)))
         (should (cl-some (lambda (line)
                            (string-match-p "Unknown Hermes transport event: alien.signal"
                                            line))
                          messages)))))))

(ert-deftest hermes-chat-collect-urls-extracts-in-order ()
  "URLs are returned in transcript order across entries."
  (should (equal '("https://a.example" "https://b.example")
                 (hermes-chat--collect-urls
                  (list '(:content "see https://a.example now")
                        '(:content "then https://b.example end"))))))

(ert-deftest hermes-chat-collect-urls-dedupes ()
  "Repeated URLs collapse to a single entry."
  (should (equal '("https://a.example")
                 (hermes-chat--collect-urls
                  (list '(:content "https://a.example")
                        '(:content "again https://a.example"))))))

(ert-deftest hermes-chat-collect-urls-handles-empty-and-nil-content ()
  "Entries without links or with nil content yield no URLs and no error."
  (should-not (hermes-chat--collect-urls
               (list '(:content "no links here") '(:content nil)))))

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

(ert-deftest hermes-chat-disconnect-without-session-errors ()
  "Disconnect signals a user error when there is no live session."
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-disconnect) :type 'user-error)))

(ert-deftest hermes-chat-dashboard-reconnect-restarts-current-client ()
  "`hermes-dashboard-reconnect' restarts the current chat's dashboard socket."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws
                 :refcount 1))
        called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (c &optional message)
                 (setq called (list c message)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-dashboard-reconnect)
       (should (equal called
                      (list client "Hermes dashboard socket reconnecting")))))))

(ert-deftest hermes-chat-dashboard-reconnect-refuses-active-turn ()
  "`hermes-dashboard-reconnect' refuses while a turn is active."
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'ws
                 :refcount 1))
        called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-reconnect)
               (lambda (&rest _args) (setq called t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--pending-assistant-id "assistant-1")
       (should-error (hermes-dashboard-reconnect) :type 'user-error)
       (should-not called)))))

(ert-deftest hermes-chat-resume-session-presets-session-id ()
  "Resuming a session opens a chat buffer bound to that durable id."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
             (lambda (&rest _) (hermes-test--dashboard-client)))
            ((symbol-function 'hermes-dashboard-transport-session-resume)
             (lambda (_client _sid &rest _args) nil)))
    (let ((buffer (hermes-chat-resume-session "sid-42" "My chat")))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'hermes-chat-mode))
            (should (equal hermes-chat--session-id "sid-42")))
        (kill-buffer buffer)))))

(ert-deftest hermes-chat-format-usage ()
  "Usage formatting is compact and omits empty counts."
  (should (equal (hermes-chat--format-usage '(:input 1200 :output 340))
                 "1200↑ 340↓ tok"))
  (should (equal (hermes-chat--format-usage '(:input 5 :output 0)) "5↑ 0↓ tok"))
  (should-not (hermes-chat--format-usage '(:input 0 :output 0)))
  (should-not (hermes-chat--format-usage nil)))

(ert-deftest hermes-chat-drops-thinking-that-echoes-reply ()
  "A reasoning block identical to the final reply is dropped on completion."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_p cb) (setq callback cb) 'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback '(:type delta :content "ok"))
       (funcall callback '(:type commentary :event "reasoning.available" :content "ok"))
       (funcall callback '(:type done :content "ok"))
       (let ((roles (mapcar (lambda (e) (plist-get e :role)) (hermes-chat--entries))))
         (should-not (memq 'commentary roles))
         (should (memq 'assistant roles)))))))

(ert-deftest hermes-chat-suppresses-thinking-echo-delta ()
  "Streaming content that only echoes thinking is not shown as assistant text."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_p cb) (setq callback cb) 'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type commentary :event "reasoning.delta"
                        :content "I will inspect the repo first."))
       (funcall callback
                '(:type delta :content "I will inspect the repo first."))
       (let ((assistant (hermes-test--assistant-entry)))
         (should assistant)
         (should (string-empty-p (or (plist-get assistant :content) ""))))))))

(ert-deftest hermes-chat-suppresses-thinking-only-final-message ()
  "Final content that only echoes thinking is not promoted to assistant text."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_p cb) (setq callback cb) 'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type commentary :event "reasoning.available"
                        :content "I will inspect the repo first."))
       (funcall callback
                '(:type done :content "I will inspect the repo first.\nsession_id: sid"))
       (let* ((entries (hermes-chat--entries))
              (assistant (cl-find-if
                          (lambda (entry)
                            (eq (plist-get entry :role) 'assistant))
                          entries))
              (commentary (cl-find-if
                           (lambda (entry)
                             (eq (plist-get entry :role) 'commentary))
                           entries)))
         (should assistant)
         (should (string-empty-p (or (plist-get assistant :content) "")))
         (should commentary)
         (should (equal (plist-get commentary :status) 'done)))))))

(ert-deftest hermes-chat-keeps-thinking-that-differs-from-reply ()
  "Reasoning that genuinely differs from the reply is retained."
  (let (callback)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (_p cb) (setq callback cb) 'fake-process)))
       (insert "hi")
       (hermes-chat-send)
       (funcall callback
                '(:type commentary :event "reasoning.delta"
                        :content "Let me weigh the options first."))
       (funcall callback '(:type delta :content "The answer is 42."))
       (funcall callback '(:type done :content "The answer is 42."))
       (let ((roles (mapcar (lambda (e) (plist-get e :role)) (hermes-chat--entries))))
         (should (memq 'commentary roles))
         (should (memq 'assistant roles)))))))

(ert-deftest hermes-chat-header-shows-agent-status-model ()
  "The header renders agent name, status, and model from `session.info'."
  (hermes-test-with-chat-buffer
   (hermes-chat--update-header-for-event
    '(:type status :event "session.info" :status "ready"
            :model "claude-opus-4-8" :agent-name "planner"))
   (let ((header (hermes-test--header-line-string)))
     (should (string-match-p "planner" header))
     (should (string-match-p "claude-opus-4-8" header))
     (should (string-match-p "Ready" header))
     (should-not (string-match-p "Hermes" header))
     (should-not (string-match-p "session " header)))))

(ert-deftest hermes-chat-header-falls-back-to-hermes-without-agent ()
  "Without an agent name the header still shows Hermes."
  (hermes-test-with-chat-buffer
   (should (string-match-p "Hermes" (hermes-test--header-line-string)))))

(ert-deftest hermes-chat-format-tool-event-keeps-detail-and-emoji ()
  "Tool lines keep the command/skill detail and carry the tool emoji."
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "terminal" :status "running"
                          :context "make test"))
                 "💻 terminal: make test"))
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "terminal" :status "completed"
                          :args ((command . "make test")) :duration 0.2))
                 "💻 terminal: make test  0.2s"))
  (should (equal (hermes-chat--format-tool-event
                  '(:type tool :name "skill_view" :status "completed"
                          :args ((name . "elisp-review")) :duration 0.1))
                 "📚 skill_view: elisp-review  0.1s"))
  (should (string-prefix-p "⚡ mystery"
                           (hermes-chat--format-tool-event
                            '(:type tool :name "mystery" :status "running")))))

(ert-deftest hermes-chat-format-context ()
  "Context usage renders abbreviated tokens and a percentage."
  (should (equal (hermes-chat--format-context '(:used 45000 :max 200000 :percent 22))
                 "45k/200k ctx (22%)"))
  (should-not (hermes-chat--format-context '(:used 0 :max 0 :percent 0)))
  (should-not (hermes-chat--format-context nil)))

(ert-deftest hermes-chat-header-shows-context-window ()
  "The header surfaces context-window usage from `session.info'."
  (hermes-test-with-chat-buffer
   (hermes-chat--update-header-for-event
    '(:type status :event "session.info" :status "ready"
            :model "gpt-5.5" :agent-name "planner"
            :context (:used 45000 :max 200000 :percent 22)))
   ;; The header doubles % so the redisplay engine renders a literal "22%"
   ;; instead of eating "%)" as a mode-line spec.
   (should (string-match-p "45k/200k ctx (22%%)" (hermes-chat--header-line)))))

(ert-deftest hermes-chat-done-event-records-usage ()
  "A done event records usage in header state; the compact header omits the gauge."
  (hermes-test-with-chat-buffer
   (hermes-chat--update-header-for-event
    '(:type done :usage (:input 1200 :output 340)))
   (should (equal (plist-get hermes-chat--status-state :usage) '(:input 1200 :output 340)))
   (should-not (string-match-p "1200↑ 340↓ tok" (hermes-chat--header-line)))))

(ert-deftest hermes-chat-turn-reduce-status-family-stamps-and-refreshes ()
  "Status-family events stamp :status-state and lead with a refresh-header effect."
  (let ((now '(100 200)))
    (dolist (case
             (list
              (list '(:status-state (:status running :activity "old"))
                    '(:type thinking :content "pondering...")
                    '(:status thinking :activity "Pondering" :updated (100 200))
                    '(refresh-header))
              (list '(:status-state (:status thinking :activity "x"))
                    '(:type commentary)
                    '(:status running :activity "Reasoning" :updated (100 200))
                    '(refresh-header upsert-entry))
              (list '(:status-state nil)
                    '(:type diff)
                    '(:status running :activity "Reviewing diff" :updated (100 200))
                    '(refresh-header upsert-entry))))
      (cl-destructuring-bind (state event expected-status effect-types) case
        (let ((result (hermes-chat--turn-reduce state event now)))
          (should (equal (plist-get (car result) :status-state) expected-status))
          (should (equal (mapcar #'car (cdr result)) effect-types))
          ;; refresh-header carries the same status-state threaded into NEW-STATE.
          (should (equal (cdr (assq 'refresh-header (cdr result)))
                         expected-status)))))))

(ert-deftest hermes-chat-turn-reduce-suppressed-terminal-settles-without-content ()
  "A suppressed terminal event runs the done lifecycle minus content copying."
  (let* ((original '(:type done :content "final text"))
         (event (list :type 'suppressed-terminal
                      :settle-status 'done
                      :header '(:type done)
                      :original original))
         (result (hermes-chat--turn-reduce
                  '(:status-state (:status running)) event '(1 2)))
         (effects (cdr result)))
    (should (equal (mapcar #'car effects)
                   '(clear-tools refresh-header clear-prompts mark-status
                     drop-thinking settle finish clear-pending drain)))
    (should (eq (cdr (assq 'mark-status effects)) 'done))
    (should (eq (cdr (assq 'clear-prompts effects)) original))
    (should-not (assq 'mark-done effects))
    (should (eq (plist-get (plist-get (car result) :status-state) :status)
                'ready))))

(ert-deftest hermes-chat-turn-reduce-status-stamps-clock-and-upserts ()
  "A non-session-info status stamps NOW, refreshes, and upserts; info skips upsert."
  (let* ((now '(7 7))
         (state '(:status-state (:status idle :activity "x")))
         (event '(:type status :status "running" :content "Searching"))
         (result (hermes-chat--turn-reduce state event now))
         (status-state (plist-get (car result) :status-state)))
    (should (equal (mapcar #'car (cdr result)) '(refresh-header upsert-entry)))
    (should (equal (cdr (assq 'upsert-entry (cdr result))) event))
    (should (equal (plist-get status-state :updated) now))
    (should (equal status-state
                   (apply #'hermes-chat--entry-with
                          '(:status idle :activity "x")
                          (append (hermes-chat--turn-header-props event)
                                  (list :updated now)))))
    (let ((r (hermes-chat--turn-reduce
              state '(:type status :event "session.info" :status "ready") now)))
      (should (equal (mapcar #'car (cdr r)) '(refresh-header))))))

(ert-deftest hermes-chat-turn-reduce-terminal-events ()
  "done/error reduce to the ordered turn-lifecycle effects; unknown adds a message."
  (let ((now '(5 5))
        (state '(:status-state (:status running :activity "x"))))
    (let* ((event '(:type done :usage (:input 1 :output 2)))
           (r (hermes-chat--turn-reduce state event now)))
      (should (equal (plist-get (car r) :status-state)
                     '(:status ready :activity "Ready"
                               :usage (:input 1 :output 2) :updated (5 5))))
      ;; refresh-header precedes drain so the header settles before re-submit.
      (should (equal (mapcar #'car (cdr r))
                     '(clear-tools refresh-header clear-prompts mark-done
                       drop-thinking settle finish clear-pending drain)))
      (should (eq (cdr (assq 'settle (cdr r))) 'done)))
    (let* ((event '(:type error :content "boom"))
           (estatus (hermes-chat--error-status event))
           (r (hermes-chat--turn-reduce state event now)))
      (should (equal (mapcar #'car (cdr r))
                     '(clear-tools refresh-header clear-prompts append-error
                       settle finish clear-pending drain)))
      (should (equal (cdr (assq 'append-error (cdr r))) (cons "boom" estatus)))
      (should (eq (cdr (assq 'settle (cdr r))) estatus)))
    (let* ((event '(:type unknown :event "weird"))
           (r (hermes-chat--turn-reduce state event now)))
      (should (eq (plist-get (plist-get (car r) :status-state) :status) 'error))
      (should (equal (mapcar #'car (cdr r))
                     '(refresh-header message upsert-entry))))))

(ert-deftest hermes-chat-turn-reduce-done-surfaces-warning ()
  "A done event with a warning emits a warning effect after mark-done."
  (let* ((state '(:status-state (:status running)))
         (event '(:type done :warning "not saved to history"))
         (r (hermes-chat--turn-reduce state event '(5 5))))
    (should (equal (mapcar #'car (cdr r))
                   '(clear-tools refresh-header clear-prompts mark-done
                     warning drop-thinking settle finish clear-pending drain)))
    (should (equal (cdr (assq 'warning (cdr r))) "not saved to history"))))

(ert-deftest hermes-chat-turn-reduce-delta-emits-append-effect ()
  "A delta event leaves the state and emits append-delta carrying its content."
  (let ((state '(:status-state (:status running))))
    (let ((r (hermes-chat--turn-reduce state '(:type delta :content "hi") '(0 0))))
      (should (eq (car r) state))
      (should (equal (cdr r) '((append-delta . "hi")))))
    ;; Missing content becomes the empty string.
    (let ((r (hermes-chat--turn-reduce state '(:type delta) '(0 0))))
      (should (equal (cdr r) '((append-delta . "")))))))

(ert-deftest hermes-chat-turn-reduce-tool-family-delta-and-transcript ()
  "Tool-like events leave the state and emit a tool delta plus an upsert-entry."
  (let ((running '(:type tool :name "terminal" :status "running" :context "make test"))
        (done '(:type tool :name "terminal" :status "completed" :context "make test"))
        (state '(:status-state (:status running))))
    (let ((result (hermes-chat--turn-reduce state running '(0 0))))
      (should (eq (car result) state))
      (should (equal (cdr result)
                     (list (cons 'tool-put
                                 (cons (hermes-chat--header-tool-key running)
                                       (hermes-chat--header-tool-summary running)))
                           (cons 'upsert-entry running)))))
    (let ((result (hermes-chat--turn-reduce state done '(0 0))))
      (should (equal (cdr result)
                     (list (cons 'tool-remove (hermes-chat--header-tool-key done))
                           (cons 'upsert-entry done)))))
    ;; No summary -> no tool delta.
    (should-not (hermes-chat--turn-tool-effect '(:type status)))))

(ert-deftest hermes-chat-turn-reduce-out-of-scope-is-noop ()
  "An unhandled event type returns the same state object and no effects."
  (let ((state '(:status-state (:status running))))
    (should (equal (hermes-chat--turn-reduce state '(:type bogus) '(0 0))
                   (cons state nil)))))

(ert-deftest hermes-chat-model-candidates-auth-first-dedup ()
  "Model candidates list authenticated providers first and keep provider identity."
  (let* ((cands (hermes-chat--model-candidates
                 '((providers
                    . (((slug . "openai") (name . "OpenAI")
                        (authenticated . nil) (models . ("gpt")))
                       ((slug . "anthropic") (name . "Anthropic")
                        (authenticated . t)
                        (models . ("claude"))
                        (pricing . ((claude . ((input . "$3") (output . "$15")))))
                        (capabilities . ((claude . ((reasoning . t)
                                                    (fast . t)
                                                    (context_window . 200000))))))
                       ((slug . "openrouter") (name . "OpenRouter")
                        (authenticated . t) (models . ("claude" ((id . "gemini"))))))))))
         (labels (mapcar #'car cands))
         (providers (mapcar (lambda (cand)
                              (plist-get (cdr cand) :provider))
                            cands)))
    (should (equal providers '("anthropic" "openrouter" "openrouter" "openai")))
    (should (string-match-p "Anthropic" (car labels)))
    (should (string-match-p "(anthropic)" (car labels)))
    (should (string-match-p "claude" (car labels)))
    (should (string-match-p "\\$3" (car labels)))
    (should-not (string-match-p "reasoning" (car labels)))
    (should-not (string-match-p "fast" (car labels)))
    (should-not (string-match-p "ctx" (car labels)))
    (should (equal (hermes-chat--model-config-value (cdar cands))
                   "claude --provider anthropic"))))

(ert-deftest hermes-chat-switch-model-sets-chosen-model ()
  "Switching prompts from model.options and applies the choice via config.set."
  (let (set-key set-value set-session set-confirm)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("alpha" "beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (let ((choice (cl-find "beta" coll :test #'string-match-p)))
                   (should choice)
                   choice)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq set-key key set-value value
                       set-session (plist-get args :session-id)
                       set-confirm (plist-get args :confirm-expensive-model))
                 (funcall (plist-get args :resolve)
                          '((key . "model") (value . "beta --provider p1"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal set-key "model"))
       (should (equal set-value "beta --provider p1"))
       (should (equal set-session "sid-1"))
       (should-not set-confirm)
       (should (string-match-p "Model set to beta" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-confirms-expensive-choice ()
  "Expensive model confirmation retries config.set with confirmation enabled."
  (let ((calls 0)
        confirms prompt)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p)
               (lambda (text)
                 (setq prompt text)
                 t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (push (plist-get args :confirm-expensive-model) confirms)
                 (funcall (plist-get args :resolve)
                          (if (= calls 1)
                              '((confirm_required . t)
                                (confirm_message . "This model may be expensive"))
                            '((key . "model")
                              (value . "beta --provider p1")))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 2))
       (should (equal (nreverse confirms) '(nil t)))
       (should (equal prompt "This model may be expensive"))
       (should (string-match-p "Model set to beta" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-stops-repeated-expensive-confirmation ()
  "A repeated confirmation request after consent reports an error instead of looping."
  (let ((calls 0)
        confirms)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (push (plist-get args :confirm-expensive-model) confirms)
                 (funcall (plist-get args :resolve)
                          '((confirm_required . t)
                            (confirm_message . "Still expensive"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 2))
       (should (equal (nreverse confirms) '(nil t)))
       (should (string-match-p "still requires confirmation"
                               (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-cancelled-expensive-choice-stops ()
  "Declining an expensive-model confirmation does not retry config.set."
  (let ((calls 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((model . "old-model")
                            (providers
                             . (((slug . "p1") (authenticated . t)
                                 (name . "Provider One")
                                 (models . ("beta")))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (cl-find "beta" coll :test #'string-match-p)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key value &rest args)
                 (should (equal value "beta --provider p1"))
                 (setq calls (1+ calls))
                 (funcall (plist-get args :resolve)
                          '((confirm_required . t)
                            (confirm_message . "This model may be expensive"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-active-session-id "sid-1"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat-switch-model)
       (should (equal calls 1))
       (should (string-match-p "Model switch cancelled" (buffer-string)))))))

(ert-deftest hermes-chat-switch-model-renders-config-set-rejection ()
  "A config.set rejection from the dashboard is rendered in the chat buffer."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-model-options)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve)
                        '((model . "old-model")
                          (providers
                           . (((slug . "p1") (authenticated . t)
                               (name . "Provider One")
                               (models . ("beta")))))))))
            ((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _)
               (cl-find "beta" coll :test #'string-match-p)))
            ((symbol-function 'hermes-dashboard-transport-config-set)
             (lambda (_client _key _value &rest args)
               (funcall (plist-get args :reject) "backend denied"))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
           hermes-chat--dashboard-active-session-id "sid-1"
           hermes-chat--dashboard-session-ready-p t)
     (hermes-chat-switch-model)
     (should (string-match-p "backend denied" (buffer-string))))))

(ert-deftest hermes-chat-warm-model-options-fetches-after-ready ()
  "Warming defers the fetch until the client's readiness promise resolves."
  (let* ((hermes-dashboard-transport--model-options-cache nil)
         (ready (hermes--promise-make))
         (client (make-hermes-dashboard-transport-client :ready-promise ready))
         fetched)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-dashboard-transport-model-options)
               (lambda (_client &rest args)
                 (setq fetched t)
                 (funcall (plist-get args :resolve) '((providers . nil))))))
      (hermes-chat--warm-model-options client)
      (should-not fetched)
      (hermes--promise-resolve ready t)
      (should fetched)
      (should (hermes-dashboard-transport-cached-model-options)))))

(ert-deftest hermes-chat-new-buffer-sets-profile ()
  "A profile chat records the profile; a blank one stays nil."
  (let ((buffer (hermes-chat--new-buffer "work")))
    (unwind-protect
        (with-current-buffer buffer (should (equal hermes-chat--profile "work")))
      (kill-buffer buffer)))
  (let ((buffer (hermes-chat--new-buffer "")))
    (unwind-protect
        (with-current-buffer buffer (should-not hermes-chat--profile))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-new-buffer-names-after-profile-and-title ()
  "The buffer name reflects the profile and a pinned title, never the bare name."
  (let ((buffer (hermes-chat--new-buffer nil nil)))
    (unwind-protect
        (with-current-buffer buffer (should (equal (buffer-name) "*Hermes: default*")))
      (kill-buffer buffer)))
  (let ((buffer (hermes-chat--new-buffer "work" "deploy")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name) "*Hermes: work: deploy*"))
          (should hermes-chat--title-manual-p))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-profile-candidates-describe-dashboard-profiles ()
  "Profile candidates parse and sort to (NAME . MODEL-LABEL) pairs."
  (let ((cands (hermes-chat--profile-candidates
                '((profiles
                   . (((name . "zeta"))
                      ((name . "") (description . "ignored"))
                      ((name . "elisp-dev") (is_default . nil)
                       (provider . "anthropic") (model . "claude-sonnet")
                       (description . "Emacs Lisp work"))
                      ((name . "default") (is_default . t)
                       (provider . "openai") (model . "gpt-5.5")
                       (description . "Main profile")
                       (gateway_running . t))
                      ((name . "alpha") (has_alias . t))))))))
    (should (equal (mapcar #'car cands)
                   '("default" "alpha" "elisp-dev" "zeta")))
    (should (equal (cdr (assoc "default" cands)) "openai/gpt-5.5"))
    (should (equal (cdr (assoc "elisp-dev" cands)) "anthropic/claude-sonnet"))
    (should-not (cdr (assoc "alpha" cands)))))

(ert-deftest hermes-chat-profile-annotation-shows-model ()
  "The profile annotation shows the model, and nothing when none is known."
  (let* ((cands '(("default" . "openai/gpt-5.5") ("alpha" . nil)))
         (annotate (hermes-chat--profile-annotation-function cands)))
    (should (string-match-p "openai/gpt-5.5" (funcall annotate "default")))
    (should-not (funcall annotate "alpha"))
    (should-not (funcall annotate "unknown"))))

(ert-deftest hermes-chat-read-profile-falls-back-when-dashboard-unavailable ()
  "The profile chooser falls back to a raw prompt when dashboard data is missing."
  (let (prompt messages)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (_client) (user-error "404 not found")))
              ((symbol-function 'read-string)
               (lambda (text &rest _)
                 (setq prompt text)
                 "manual-profile"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should (string-match-p "blank for default" prompt))
      (should (string-match-p "Profile list unavailable: 404 not found"
                              prompt))
      (should (string-match-p "Profile list unavailable: 404 not found"
                              (car messages))))))

(ert-deftest hermes-chat-read-profile-falls-back-when-list-empty ()
  "An empty dashboard profile list falls back with a helpful message."
  (let (prompt messages)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (_client) '((profiles . nil))))
              ((symbol-function 'read-string)
               (lambda (text &rest _)
                 (setq prompt text)
                 "manual-profile"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should (string-match-p "blank for default" prompt))
      (should (string-match-p "No dashboard profiles available" prompt))
      (should (string-match-p "No dashboard profiles available" (car messages))))))

(ert-deftest hermes-chat-read-profile-skips-spawn-without-client ()
  "With no live chat client the profile chooser prompts raw, never spawning."
  (let (prompt spawned)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq spawned t) 'transient-client))
              ((symbol-function 'read-string)
               (lambda (text &rest _) (setq prompt text) "manual-profile"))
              ((symbol-function 'message) #'ignore))
      (should (equal (hermes-chat--read-profile) "manual-profile"))
      (should-not spawned)
      (should (string-match-p "blank for default" prompt)))))

(ert-deftest hermes-chat-profile-list-payload-prefers-warm-cache ()
  "A warmed profile cache feeds completion without touching a chat client."
  (let ((hermes-dashboard-transport--profile-cache nil)
        touched)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () (setq touched 'client) nil))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (&rest _) (setq touched 'fetch) nil)))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default") (is_default . t))
                      ((name . "elisp-dev"))))))
      (should (equal (hermes-chat--profile-candidates
                      (hermes-chat--profile-list-payload))
                     '(("default" . nil) ("elisp-dev" . nil))))
      (should-not touched))))

(ert-deftest hermes-chat-read-profile-completes-from-cache-without-client ()
  "With a warm cache and no live client the picker completes, never spawning."
  (let ((hermes-dashboard-transport--profile-cache nil)
        spawned)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (setq spawned t) 'transient-client))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default") (is_default . t))
                      ((name . "elisp-dev"))))))
      (should (equal (hermes-chat--read-profile) "elisp-dev"))
      (should-not spawned))))

(ert-deftest hermes-chat-completes-dashboard-profile ()
  "Interactively creating a chat chooses from dashboard profiles."
  (let (choices)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (_client)
                 '((profiles . (((name . "default") (is_default . t))
                                ((name . "elisp-dev")
                                 (description . "Emacs Lisp work")))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (let ((buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (progn
              (should (cl-find "default" choices :test #'string-match-p))
              (with-current-buffer buffer
                (should (equal hermes-chat--profile "elisp-dev"))))
          (kill-buffer buffer))))))

(ert-deftest hermes-chat-send-passes-profile-to-session-create ()
  "The buffer's profile is threaded into session.create."
  (let (create-profile)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-profile (plist-get args :profile))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid") (stored_session_id . "stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) nil)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--profile "work")
         (insert "hello")
         (hermes-chat-send)
         (should (equal create-profile "work")))))))

(ert-deftest hermes-chat-blank-profile-fallback-uses-default-session-profile ()
  "Blank raw fallback input leaves session.create profile omitted."
  (let (create-profile)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (_client) (user-error "404 not found")))
              ((symbol-function 'read-string)
               (lambda (&rest _) "   "))
              ((symbol-function 'message)
               (lambda (&rest _) nil))
              ((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq create-profile (plist-get args :profile))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid") (stored_session_id . "stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) nil)))
      (let ((hermes-transport-send-function #'hermes-transport-send)
            (buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (with-current-buffer buffer
              (should-not hermes-chat--profile)
              (insert "hello")
              (hermes-chat-send)
              (should-not create-profile))
          (kill-buffer buffer))))))

;;; Group: provider onboarding from chat

(ert-deftest hermes-chat-find-provider-matches-slug ()
  "`hermes-chat--find-provider' returns the provider row for a slug, or nil."
  (let ((result '((providers . (((slug . "openai"))
                                ((slug . "deepseek") (name . "DeepSeek")))))))
    (should (equal (hermes-transport--get
                    (hermes-chat--find-provider result "deepseek") 'name)
                   "DeepSeek"))
    (should-not (hermes-chat--find-provider result "nope"))))

(ert-deftest hermes-chat-connect-provider-candidate-saves-then-runs-on-connected ()
  "Connecting reads a key and saves it scoped to the session, then continues."
  (let (saved on-ran)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "sk-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (_client slug key &rest args)
                 (setq saved (list slug key (plist-get args :session-id)))
                 (funcall (plist-get args :resolve)
                          '((provider . ((slug . "deepseek"))))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-active-session-id "sid-1"
              hermes-chat--dashboard-session-ready-p t)
        (hermes-chat--connect-provider-candidate
         (current-buffer) 'fake-client
         '((slug . "deepseek") (name . "DeepSeek")
           (auth_type . "api_key") (key_env . "DEEPSEEK_API_KEY"))
         (lambda () (setq on-ran t)))))
    (should (equal saved '("deepseek" "sk-secret" "sid-1")))
    (should on-ran)))

(ert-deftest hermes-chat-model-picker-connects-unauthed-then-applies ()
  "Picking an unauthenticated provider's model connects it, then applies it."
  (let (saved applied)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt labels &rest _) (car labels)))
              ((symbol-function 'read-passwd) (lambda (&rest _) "sk-secret"))
              ((symbol-function 'hermes-dashboard-transport-model-save-key)
               (lambda (_client slug _key &rest args)
                 (setq saved slug)
                 (funcall (plist-get args :resolve)
                          '((provider . ((slug . "deepseek")))))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq applied (cons key value))
                 (funcall (plist-get args :resolve) '((ok . t))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-active-session-id "sid-1"
              hermes-chat--dashboard-session-ready-p t)
        (hermes-chat--prompt-and-set-model
         (current-buffer) 'fake-client
         '((providers . (((slug . "deepseek") (name . "DeepSeek")
                          (auth_type . "api_key") (key_env . "DEEPSEEK_API_KEY")
                          (models . ("deepseek-chat")))))))))
    (should (equal saved "deepseek"))
    (should (equal (car applied) "model"))))

;;; Group: session handoff

(ert-deftest hermes-chat-handoff-status-classifies-as-active ()
  "The `handoff' status reads as active with a distinct header label."
  (should (hermes-chat--active-status-p 'handoff))
  (should-not (hermes-chat--finished-status-p 'handoff))
  (should (equal (hermes-chat--header-status-label 'handoff) "Handing off")))

(ert-deftest hermes-chat-handoff-requires-attached-session ()
  "Handoff errors when the chat has no attached dashboard session."
  (hermes-test-with-chat-buffer
   (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
              (lambda () nil)))
     (should-error (hermes-chat-handoff "telegram") :type 'user-error))))

(ert-deftest hermes-chat-handoff-refuses-during-active-turn ()
  "Handoff errors while a turn is still active."
  (hermes-test-with-chat-buffer
   (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
              (lambda () t))
             ((symbol-function 'hermes-chat--active-turn-p) (lambda () t)))
     (should-error (hermes-chat-handoff "telegram") :type 'user-error))))

(ert-deftest hermes-chat-handoff-sends-platform-and-session ()
  "Handoff submits the lowercased platform and live session id."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-9")
   (let (args)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-chat--handoff-start-poll) #'ignore)
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (_client platform &rest rest)
                  (setq args (cons platform rest)))))
       (hermes-chat-handoff "Telegram"))
     (should (equal (car args) "telegram"))
     (should (equal (plist-get (cdr args) :session-id) "sid-9")))))

(ert-deftest hermes-chat-handoff-handle-state-completed-stops ()
  "A completed handoff state stops the poll and reports success."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 1))
    (let (stopped reported)
      (cl-letf (((symbol-function 'hermes-chat--handoff-stop)
                 (lambda () (setq stopped t)))
                ((symbol-function 'hermes-chat--insert-local-status)
                 (lambda (&rest _) (setq reported t)))
                ((symbol-function 'hermes-chat--set-header-state) #'ignore)
                ((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (error "should not reschedule on completed"))))
        (hermes-chat--handoff-handle-state
         (current-buffer) '((state . "completed"))))
      (should stopped)
      (should reported))))

(ert-deftest hermes-chat-handoff-handle-state-pending-reschedules ()
  "A non-terminal handoff state reschedules another poll."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 1))
    (let (rescheduled)
      (cl-letf (((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (setq rescheduled t))))
        (hermes-chat--handoff-handle-state
         (current-buffer) '((state . "pending"))))
      (should rescheduled))))

(ert-deftest hermes-chat-handoff-reschedule-doubles-backoff-capped ()
  "Reschedule doubles the poll backoff up to the configured ceiling."
  (with-temp-buffer
    (let (delays)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay &rest _) (push delay delays) 'timer)))
        (setq hermes-chat--handoff-poll (list :backoff 1))
        (hermes-chat--handoff-reschedule (current-buffer))
        (should (equal (plist-get hermes-chat--handoff-poll :backoff) 2))
        (setq hermes-chat--handoff-poll (list :backoff 8))
        (hermes-chat--handoff-reschedule (current-buffer))
        (should (equal (plist-get hermes-chat--handoff-poll :backoff)
                       hermes-chat--handoff-poll-max-delay)))
      (should (equal (car (last delays)) 2)))))

(ert-deftest hermes-chat-handoff-stop-cancels-timer ()
  "Stopping the handoff cancels its timer and clears poll state."
  (with-temp-buffer
    (let (cancelled)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_timer) (setq cancelled t))))
        (setq hermes-chat--handoff-poll (list :timer 'the-timer))
        (hermes-chat--handoff-stop))
      (should cancelled)
      (should-not hermes-chat--handoff-poll))))

(ert-deftest hermes-chat-handoff-poll-tick-times-out-past-deadline ()
  "A poll tick past the deadline routes to the timeout path, not a poll."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll
          (list :platform "telegram" :backoff 1
                :deadline (time-subtract (current-time) 5)))
    (let (timed-out)
      (cl-letf (((symbol-function 'hermes-chat--handoff-timeout)
                 (lambda (_b) (setq timed-out t)))
                ((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (&rest _) (error "should not poll past deadline"))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should timed-out))))

(ert-deftest hermes-chat-handoff-poll-tick-reject-reschedules ()
  "A failed `handoff.state' poll reschedules rather than aborting."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll
          (list :platform "telegram" :backoff 1
                :deadline (time-add (current-time) 60)))
    (let (rescheduled)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (funcall (plist-get args :reject) "boom")))
                ((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (setq rescheduled t))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should rescheduled))))

(ert-deftest hermes-chat-handoff-poll-tick-resolve-handles-state ()
  "A successful poll routes its result into the state handler."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll
          (list :platform "telegram" :backoff 1
                :deadline (time-add (current-time) 60)))
    (let (handled)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (funcall (plist-get args :resolve) '((state . "running")))))
                ((symbol-function 'hermes-chat--handoff-handle-state)
                 (lambda (_b result) (setq handled result))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should (equal (cdr (assq 'state handled)) "running")))))

(ert-deftest hermes-chat-handoff-slash-routes-to-command ()
  "The /handoff slash command dispatches its argument to `hermes-chat-handoff'."
  (let (called)
    (cl-letf (((symbol-function 'hermes-chat-handoff)
               (lambda (&optional arg) (setq called (or arg 'interactive)))))
      (let ((handler (hermes-chat--native-slash-handler "handoff")))
        (should handler)
        (funcall handler "telegram")
        (should (equal called "telegram"))))))

(ert-deftest hermes-chat-handoff-timeout-fails-and-reports ()
  "A timed-out handoff fires `handoff.fail' and reports the error."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 8))
    (let (failed reported)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-fail)
                 (lambda (_client &rest args)
                   (setq failed (plist-get args :error))))
                ((symbol-function 'hermes-chat--command-error)
                 (lambda (msg) (setq reported msg))))
        (hermes-chat--handoff-timeout (current-buffer)))
      (should (equal failed "client poll timed out"))
      (should (string-match-p "timed out" reported))
      (should-not hermes-chat--handoff-poll))))

(ert-deftest hermes-chat-handoff-targets-parse-completion-items ()
  "Completion items become (PLATFORM . META) cells, vector or list."
  (should (equal (hermes-chat--handoff-targets
                  '((items . [((text . "telegram") (meta . "→ Home"))
                              ((text . "discord") (meta . ""))])))
                 '(("telegram" . "→ Home") ("discord" . "")))))

(ert-deftest hermes-chat-handoff-prompt-uses-live-targets ()
  "The platform prompt fetches live targets via complete.slash."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid")
   (let (slash-text offered)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (_client text &rest args)
                  (setq slash-text text)
                  (funcall (plist-get args :resolve)
                           '((items . [((text . "telegram") (meta . "→ H"))])))))
               ((symbol-function 'completing-read)
                (lambda (_prompt coll &rest _) (setq offered coll) ""))
               ((symbol-function 'hermes-chat--handoff-begin) #'ignore))
       (hermes-chat-handoff))
     (should (equal slash-text "/handoff "))
     (should (member "telegram" offered)))))

(ert-deftest hermes-chat-handoff-given-platform-skips-prompt ()
  "A platform argument begins the handoff without fetching live targets."
  (hermes-test-with-chat-buffer
   (let (began)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (&rest _) (error "should not fetch targets when given")))
               ((symbol-function 'hermes-chat--handoff-begin)
                (lambda (_buffer platform) (setq began platform))))
       (hermes-chat-handoff "Telegram"))
     (should (equal began "telegram")))))

(ert-deftest hermes-chat-handoff-read-target-falls-back-without-targets ()
  "With no live targets the picker reads a free-form platform name."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "  IRC ")))
    (should (equal (hermes-chat--handoff-read-target '((items . []))) "irc"))))

(ert-deftest hermes-chat-handoff-read-target-skips-blank-text ()
  "Items without a text field are dropped rather than erroring."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x")))
    (should (equal (hermes-chat--handoff-read-target '((items . [((meta . "m"))])))
                   "x"))))

(ert-deftest hermes-chat-dashboard-shares-one-client-across-buffers ()
  "Two chat buffers attach to one shared client and route events by session.
Killing one buffer releases its reference without tearing down the other."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (made 0) a-events b-events buf-a buf-b shared)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (cl-incf made)
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
                            (lambda (event) (push event a-events))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-a"))))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start
               (lambda (event) (push event b-events)))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-b"))))
            (should (= made 1))
            (should (= (hermes-dashboard-transport-client-refcount shared) 2))
            (hermes-dashboard-transport--dispatch-event
             shared '(:type delta :session-id "sid-a" :content "x"))
            (should (= (length a-events) 1))
            (should-not b-events)
            (kill-buffer buf-a)
            (should (hermes-dashboard-transport-client-websocket shared))
            (should (= (hermes-dashboard-transport-client-refcount shared) 1))
            (hermes-dashboard-transport--dispatch-event
             shared '(:type delta :session-id "sid-b" :content "y"))
            (should (= (length b-events) 1)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-reconnected-resumes-session ()
  "A reconnected status re-resumes the buffer's stored dashboard session."
  (let ((client (hermes-test--dashboard-client)) resumed)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest _args) (setq resumed session-id))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "stored-session"
               hermes-chat--dashboard-client client)
         ;; Drive the real subscriber closure (generation + session filter),
         ;; not the handler directly, so a future guard regression is caught.
         (funcall (hermes-chat--transport-callback
                   (current-buffer) "asst-reconnect" t
                   (hermes-chat--next-transport-generation))
                  '(:type status :status "reconnected"))
         (should (equal resumed "stored-session")))))))


;;; Shared-socket runtime isolation

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

(provide 'hermes-chat-tests)
;;; hermes-chat-tests.el ends here
