;;; hermes-chat-render-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'hermes-chat-render)
(require 'delsel)
(require 'hermes-test-helpers)

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
              (roles (hermes-test--transcript-roles entries))
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
         (should (equal (hermes-test--transcript-roles entries)
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
                 '(:status running :activity "Working"))))

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

(ert-deftest hermes-chat-session-info-updates-header-and-working-directory ()
  "`session.info' updates header state but adds no transcript entry."
  (hermes-test-with-chat-buffer
   (setq default-directory "/tmp/local-editor/")
   (let ((before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (hermes-chat--handle-transport-event
       "a1" '(:type status :event "session.info" :status "ready"
              :model "gpt-5.5" :agent-name "openai-codex" :cwd "/srv/project"
              :goal (:status "active" :running t
                             :turns-used 1 :max-turns 20)))
      (should (string-match-p "gpt-5.5" (hermes-test--header-line-string)))
      (should-not (string-match-p "Goal" (hermes-test--header-line-string)))
      (should (equal (plist-get hermes-chat--goal :turns-used) 1))
      (should (equal hermes-chat--working-directory "/srv/project"))
      (should (equal default-directory "/tmp/local-editor/"))
      (should (= before (length (ewoc-collect hermes-chat--ewoc #'identity)))))))

(ert-deftest hermes-chat-goal-status-preserves-turn-header ()
  "Goal notices preserve turn status and refresh vanilla goal state."
  (let ((refreshes 0))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal)
               (lambda () (setq refreshes (1+ refreshes)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--status-state '(:status ready :activity "Ready"))
       (hermes-chat--run-turn-reducer
        "a1" '(:type status :event "status.update" :status "goal"
                     :content "Continuing toward goal (1/20)"))
       (should-not (string-match-p "Ready" (hermes-test--header-line-string)))
       (should (string-match-p "Continuing toward goal" (buffer-string)))
       (should (= refreshes 1))))))

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

(ert-deftest hermes-chat-warn-status-reuses-one-transcript-line ()
  "Repeated compression-blocked warnings replace one status line."
  (hermes-test-with-chat-buffer
   (hermes-chat--run-turn-reducer
    "a1" '(:type status :event "status.update" :status "warn"
                 :session-id "sid"
                 :content "Context over threshold (~477,446 tokens >= 425,000) blocked (cooldown:60)."))
   (hermes-chat--run-turn-reducer
    "a2" '(:type status :event "status.update" :status "warn"
                 :session-id "sid"
                 :content "Context over threshold (~528,669 tokens >= 425,000) blocked (cooldown:60)."))
   (hermes-chat--run-turn-reducer
    "a2" '(:type status :event "status.update" :status "loop"
                 :session-id "sid" :content "Loop set (every 30m)"))
   (let ((text (buffer-substring-no-properties
                (point-min) (hermes-chat--input-position)))
         (warns (ewoc-collect
                 hermes-chat--ewoc
                 (lambda (entry)
                   (and (eq (plist-get entry :role) 'status)
                        (string-match-p "over threshold"
                                        (or (plist-get entry :content) "")))))))
     (should (= 1 (length warns)))
     (should (string-match-p "528,669" (plist-get (car warns) :content)))
     (should-not (string-match-p "477,446" text))
     (should (string-match-p "Loop set" text)))))

(ert-deftest hermes-chat-compressing-status-reuses-one-line ()
  "Manual compress progress upserts one line; the ready bar-clear is header-only."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--status-state '(:status ready :activity "Ready"))
   (hermes-chat--run-turn-reducer
    "a1" '(:type status :event "status.update" :status "compressing"
                 :session-id "sid" :content "compressing 40 messages…"))
   (hermes-chat--run-turn-reducer
    "a1" '(:type status :event "status.update" :status "compressing"
                 :session-id "sid"
                 :content "compressing 40 messages (~9,000 tok)…"))
   (hermes-chat--run-turn-reducer
    "a1" '(:type status :event "status.update" :status "status"
                 :session-id "sid" :content "ready"))
   (let ((lines (ewoc-collect
                 hermes-chat--ewoc
                 (lambda (entry)
                   (eq (plist-get entry :role) 'status)))))
     (should (= 1 (length lines)))
     (should (string-match-p "9,000" (plist-get (car lines) :content)))
     (should-not (string-match-p "ready"
                                 (buffer-substring-no-properties
                                  (point-min) (hermes-chat--input-position))))
     (should (eq (plist-get hermes-chat--status-state :status) 'ready))
     (should (equal (plist-get hermes-chat--status-state :activity) "Ready"))
     (should-not (string-match-p "Ready" (hermes-test--header-line-string)))
     (should-not (string-match-p "Idle" (hermes-test--header-line-string))))))

(ert-deftest hermes-chat-control-session-renders-server-originated-turn ()
  "A slash-created idle session renders a later backend-owned turn."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "sid-loop")
                 (stored_session_id . "stored-loop")))
       (should hermes-chat--dashboard-token)
       (hermes-dashboard-transport--dispatch-event
        client '(:type status :event "message.start" :status "started"
                 :session-id "sid-loop"))
       (hermes-dashboard-transport--dispatch-event
        client '(:type delta :content "Hello from the loop"
                 :session-id "sid-loop"))
       (hermes-dashboard-transport--dispatch-event
        client '(:type done :session-id "sid-loop"))
       (hermes-dashboard-transport--dispatch-event
        client '(:type status :event "message.start" :status "started"
                 :session-id "sid-loop"))
       (hermes-dashboard-transport--dispatch-event
        client '(:type delta :content "Hello again"
                 :session-id "sid-loop"))
       (hermes-dashboard-transport--dispatch-event
        client '(:type done :session-id "sid-loop"))
       (let ((assistants
              (cl-remove-if-not
               (lambda (entry) (eq (plist-get entry :role) 'assistant))
               (hermes-chat--entries))))
         (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                                assistants)
                        '("Hello from the loop" "Hello again")))
         (should (cl-every (lambda (entry)
                            (eq (plist-get entry :status) 'done))
                          assistants)))
       (should-not hermes-chat--pending-assistant-id)))))

(ert-deftest hermes-chat-idle-session-renders-background-completion ()
  "An idle subscriber renders a session-owned `/btw' completion."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--background-tasks
             '(("bg-idle" :number 1 :preview "idle task")))
       (hermes-chat--dashboard-record-session
        client '((session_id . "sid-idle")
                 (stored_session_id . "stored-idle")))
       (hermes-dashboard-transport--dispatch-event
        client '(:type background :task-id "bg-idle"
                 :content "Idle task finished" :session-id "sid-idle"))
       (should-not (assoc "bg-idle" hermes-chat--background-tasks))
       (let ((entry (cl-find-if
                     (lambda (item) (eq (plist-get item :role) 'background))
                     (hermes-chat--entries))))
         (should entry)
         (should (equal (plist-get entry :content) "Idle task finished")))))))

(ert-deftest hermes-chat-idle-session-reconnected-remains-detached ()
  "A reconnected status leaves the durable session lazy and detached."
  (let ((client (hermes-test--dashboard-client)) resumed)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client stored-id &rest _args)
                 (setq resumed stored-id))))
      (let ((hermes-chat-use-dashboard-transport t)
            (hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client)
         (hermes-chat--dashboard-record-session
          client '((session_id . "sid-old")
                   (stored_session_id . "stored-idle")))
         (hermes-dashboard-transport--dispatch-event
          client '(:type status :status reconnecting))
         (should (equal hermes-chat--session-id "stored-idle"))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (hermes-dashboard-transport--dispatch-event
          client '(:type status :status reconnected))
         (should-not resumed)
         (should (equal hermes-chat--session-id "stored-idle"))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (should (eq (plist-get hermes-chat--status-state :status) 'ready))
         (should (string-match-p "Dashboard socket reconnected"
                                 (buffer-string))))))))

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

(ert-deftest hermes-chat-table-ragged-insertion-and-reflow ()
  "Narrow insertion and reflow preserve ragged source, EWOC and draft undo."
  (dolist (initial-width '(12 80))
    (hermes-test-with-chat-buffer
     (let ((raw "| A | B | C |\n|---|---|---|\n| x |\n|\n| y | z | q | r |\n")
           (width initial-width)
           (kill-ring nil))
       (cl-letf (((symbol-function 'hermes-chat--table-window-width)
                  (lambda () width)))
         (insert "Draft")
         (undo-boundary)
         (insert " suffix")
         (undo-boundary)
         (let* ((node (hermes-chat--insert-entry
                       (hermes-chat--make-entry 'assistant raw 'done)))
                (entry (ewoc-data node)))
           (dolist (next-width '(12 80 12))
             (setq width next-width)
             (hermes-chat--reflow-table-windows)
             (should (eq node (ewoc-nth hermes-chat--ewoc 0)))
             (should (eq entry (ewoc-data node)))
             (should (equal (plist-get entry :content) raw))
             (should (equal (hermes-chat-input-string) "Draft suffix"))
             (let* ((start (text-property-not-all
                            (point-min) (point-max) 'hermes-chat-inline-table nil))
                    (end (button-start (next-button start)))
                    (grid (buffer-substring-no-properties start end)))
               (should (= (get-text-property start 'hermes-chat-table-width) width))
               (dolist (cell '("A" "B" "C" "x" "y" "z" "q" "r"))
                 (should (string-match-p cell grid)))
               (dolist (line (split-string grid "\n" t))
                 (should (<= (string-width line) width))))
             (button-activate (next-button (point-min)))
             (should (equal (current-kill 0 t) raw)))
           (goto-char (point-max))
           (undo 1)
           (should (equal (hermes-chat-input-string) "Draft"))))))))

(ert-deftest hermes-chat-visible-unselected-reply-preserves-input ()
  "Rendering in an unselected window must not insert at its draft point."
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (hermes-chat-mode)
      (let ((window (split-window-right)))
        (set-window-buffer window (current-buffer))
        (insert "Draft")
        (set-window-point window (point-max))
        (hermes-chat--insert-entry
         (hermes-chat--make-entry 'assistant "Reply" 'done "reply"))
        (should (equal (hermes-chat-input-string) "Draft"))
        (should (equal (buffer-substring-no-properties
                        (point-min) hermes-chat--input-marker)
                       "Reply\n\n \n"))
        (hermes-chat--update-entry
         "reply" (lambda (entry)
                   (hermes-chat--entry-with entry :content "Updated reply")))
        (should (equal (hermes-chat-input-string) "Draft"))
        (should (equal (buffer-substring-no-properties
                        (point-min) hermes-chat--input-marker)
                       "Updated reply\n\n \n"))))))

(ert-deftest hermes-chat-table-window-width-excludes-number-gutter ()
  "Use each window's number gutter and fixed-pitch metrics, not frame columns."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((wide (selected-window))
             (narrow (split-window-right 30))
             (body 1404)
             (gutter 44)
             (font 11))
        (set-window-buffer narrow (current-buffer))
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (window pixelwise)
                     (should pixelwise)
                     (if (eq window wide) 2808 body)))
                  ((symbol-function 'window-font-width)
                   (lambda (window face)
                     (should (memq window (list wide narrow)))
                     (should (eq face 'fixed-pitch))
                     font))
                  ((symbol-function 'line-number-display-width)
                   (lambda (pixelwise)
                     (should pixelwise)
                     (if (eq (selected-window) narrow) gutter 22))))
          (setq-local display-line-numbers nil)
          (should (= (hermes-chat--table-window-width) 125))
          (setq-local display-line-numbers 'relative)
          (should (= (hermes-chat--table-window-width) 121))
          (setq gutter 66)
          (should (= (hermes-chat--table-window-width) 119))
          (setq body 682)
          (should (= (hermes-chat--table-window-width) 54))
          (setq font 14)
          (should (= (hermes-chat--table-window-width) 42))
          (should (eq (selected-window) wide)))))))

(ert-deftest hermes-chat-table-window-width-real-number-geometry ()
  "Graphical line-number gutters leave the rendered grid inside the text area."
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert (make-string 150 ?\n))
      (let ((window (selected-window))
            (raw (concat "| A | B |\n|---|---|\n| "
                         (make-string 200 ?x) " | value |\n")))
        ;; Body width already excludes these; do not subtract them twice.
        (set-window-margins window 2 3)
        (set-window-fringes window 8 8)
        (dolist (columns '(nil 3 6))
          (setq-local display-line-numbers (and columns 'relative))
          (setq-local display-line-numbers-width columns)
          (redisplay t)
          (let* ((gutter (if columns (line-number-display-width t) 0))
                 (body (window-body-width window t))
                 (font (window-font-width window 'fixed-pitch))
                 (width (hermes-chat--table-window-width)))
            (when columns (should (> gutter 0)))
            (should (= width (max 6 (- (/ (- body gutter) font) 2))))
            (dolist (line (split-string (hermes-chat--format-table raw width) "\n" t))
              (should (<= (string-pixel-width
                           (propertize line 'face 'fixed-pitch))
                          (- body gutter))))))))))

(ert-deftest hermes-chat-table-inline-copy-and-navigation ()
  "Tables are real text; source-copy does not switch buffers or touch drafts."
  (dolist (hide '(nil t))
    (let ((markdown-hide-markup hide)
          (raw "| **bold** | B |\n|---|---|\n| one | two |\n")
          (kill-ring nil))
      (hermes-test-with-chat-buffer
       (insert "Unsent draft")
       (let* ((node (hermes-chat--insert-entry
                     (hermes-chat--make-entry 'assistant raw 'done)))
              (entry (ewoc-data node))
              (buffer (current-buffer)))
         (should (equal (plist-get entry :content) raw))
         (should-not (string-match-p "View Table" (buffer-string)))
         (goto-char (point-min))
         (search-forward "one")
         (backward-char 3)
         (let ((start (point)))
           (forward-char 1)
           (should (= (point) (1+ start)))
           (should (equal (filter-buffer-substring start (+ start 3)) "one")))
         (let ((button (next-button (point-min))))
           (should (equal (button-label button) "[Copy source]"))
           (button-activate button))
         (should (eq (current-buffer) buffer))
         (should (equal (current-kill 0 t) raw))
         (should (eq entry (ewoc-data node)))
         (should (equal (hermes-chat-input-string) "Unsent draft"))
         (should visual-line-mode)
         (should-not truncate-lines))))))

(ert-deftest hermes-chat-table-resize-preserves-ewoc-draft-and-undo ()
  "Hidden insertion reflows for the narrowest window without mutating entries."
  (save-window-excursion
    (hermes-test-with-chat-buffer
     (let* ((raw (concat "| A | B |\n|---|---|\n| one | "
                         (make-string 160 ?x) " |\n"))
            (node (hermes-chat--insert-entry
                   (hermes-chat--make-entry 'assistant raw 'done)))
            (entry (ewoc-data node)))
       (insert "Draft")
       (undo-boundary)
       (insert " suffix")
       (undo-boundary)
       (set-window-buffer (selected-window) (current-buffer))
       (let* ((other (split-window-right 30))
              (expected (hermes-chat--table-window-width)))
         (set-window-buffer other (current-buffer))
         (setq expected (hermes-chat--table-window-width))
         (narrow-to-region hermes-chat--input-marker (point-max))
         (hermes-chat--reflow-table-windows)
         (should (buffer-narrowed-p))
         (should (equal (buffer-string) "Draft suffix"))
         (should (= (point) (point-max)))
         (widen)
         (should (eq entry (ewoc-data node)))
         (should (equal (plist-get entry :content) raw))
         (goto-char (point-min))
         (let ((start (text-property-not-all (point-min) (point-max)
                                             'hermes-chat-inline-table nil)))
           (should (= (get-text-property start 'hermes-chat-table-width) expected)))
         (goto-char (point-max))
         (undo 1)
         (should (equal (hermes-chat-input-string) "Draft"))
         (let ((before (buffer-chars-modified-tick)))
           (hermes-chat--reflow-table-windows)
           (should (= before (buffer-chars-modified-tick))))
         (should (memq #'hermes-chat--reflow-table-windows
                       window-configuration-change-hook))
         (fundamental-mode)
         (should-not (memq #'hermes-chat--reflow-table-windows
                           window-configuration-change-hook)))))))

(ert-deftest hermes-chat-table-reflow-preserves-window-anchors-and-node-boundaries ()
  "Table replacement preserves both reader windows and later EWOC updates."
  (save-window-excursion
    (hermes-test-with-chat-buffer
     (let* ((table (concat "| A | B |\n|---|---|\n| first | "
                           (make-string 100 ?x) " |\n"))
            (first (hermes-chat--insert-entry
                    (hermes-chat--make-entry 'assistant (concat table "\nAfter one\n") 'done)))
            (second (hermes-chat--insert-entry
                     (hermes-chat--make-entry 'assistant (concat table "\nAfter two\n") 'done)))
            (window (selected-window))
            (other (split-window-right 30)))
       (set-window-buffer window (current-buffer))
       (set-window-buffer other (current-buffer))
       (goto-char (point-min))
       (search-forward "After one")
       (beginning-of-line)
       (set-window-start window (point))
       (set-window-point window (point))
       (search-forward "After two")
       (beginning-of-line)
       (set-window-start other (point))
       (set-window-point other (point))
       (goto-char (window-start window))
       (hermes-chat--reflow-table-windows)
       (dolist (pair (list (cons window "After one") (cons other "After two")))
         (dolist (position (list (window-start (car pair)) (window-point (car pair))))
           (should (equal (buffer-substring-no-properties position (+ position 9))
                          (cdr pair)))))
       (should (= (length (hermes-chat--entries)) 2))
       (let ((inhibit-read-only t))
         (hermes-chat--preserve-input-point
          (ewoc-invalidate hermes-chat--ewoc first second)))
       (should (= (how-many "After one" (point-min) (point-max)) 1))
       (should (= (how-many "After two" (point-min) (point-max)) 1))))))

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

(ert-deftest hermes-chat-diff-reuse-adopts-opening-source-directory ()
  "Each chat opening the shared viewer supplies its own directory and diff."
  (let (viewer)
    (save-window-excursion
      (unwind-protect
          ;; Preserve remote directory spelling without invoking TRAMP handlers.
          (let ((file-name-handler-alist nil))
            (when (get-buffer "*Hermes Diff*")
              (kill-buffer "*Hermes Diff*"))
            (dolist (directory '("/tmp/project-a/" "/tmp/project-b/"
                                 "/ssh:example.invalid:/project-c/"))
              (hermes-test-with-chat-buffer
               (setq default-directory directory)
               (let ((diff (concat "--- a/source.el\n+++ b/source.el\n"
                                   "@@ -1 +1 @@\n-old\n+" directory "\n")))
                 (hermes-chat--insert-entry
                  (hermes-chat--make-entry 'assistant diff 'done))
                 (let ((source (current-buffer))
                       (text (buffer-string)))
                   (hermes-test--push-button-labeled "View Diff")
                   (let ((opened (get-buffer "*Hermes Diff*")))
                     (when viewer (should (eq opened viewer)))
                     (setq viewer opened)
                     (with-current-buffer opened
                       (should (equal default-directory directory))
                       (should (equal (buffer-string) diff))
                       (should (derived-mode-p 'diff-mode))
                       (should buffer-read-only)))
                   (with-current-buffer source
                     (should (equal default-directory directory))
                     (should (equal (buffer-string) text))))))))
        (when (buffer-live-p viewer) (kill-buffer viewer))))))

(ert-deftest hermes-chat-diff-preserves-embedded-data-image-url ()
  "Diff buttons retain data URLs without lifting them as transcript images."
  (let ((url (concat "data:image/png;base64," (make-string 80 ?A))))
    (hermes-test-with-chat-buffer
     (hermes-chat--insert-entry
      (hermes-chat--make-entry
       'assistant
       (concat "Changed:\n"
               "--- a/style.css\n"
               "+++ b/style.css\n"
               "@@ -0,0 +1 @@\n"
               "+background: url(" url ");\n")
       'done))
     (should (string-match-p (regexp-quote url)
                             (hermes-test--view-diff-content)))
     (should-not (string-match-p "\\[image\\]" (buffer-string))))))

(ert-deftest hermes-chat-background-owner-rejects-before-draft-mutation ()
  "A session owner blocks background submission without consuming its draft."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'command-owner)
   (let (bootstrapped)
     (cl-letf (((symbol-function 'hermes-chat--with-dashboard-session)
                (lambda (&rest _) (setq bootstrapped t))))
       (insert "valuable background draft")
       (should-error (hermes-chat-background) :type 'user-error)
       (should (equal (hermes-chat-input-string) "valuable background draft"))
       (should-not bootstrapped)))))

(ert-deftest hermes-chat-background-reject-restores-input ()
  "A rejected background launch restores the consumed draft."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-background)
               (lambda (_client _text &rest args)
                 (funcall (plist-get args :reject) "session busy"))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-client client
              hermes-chat--dashboard-active-session-id "sid-bg"
              hermes-chat--dashboard-session-ready-p t)
        (insert "valuable background draft")
        (hermes-chat-background)
        (should (equal (hermes-chat-input-string) "valuable background draft"))
        (should-not hermes-chat--background-tasks)))))

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
           (should (derived-mode-p 'hermes-chat-background-mode))
           (should (derived-mode-p 'markdown-mode))
           (should buffer-read-only)
           (should (eq (keymap-lookup (current-local-map) "q") 'quit-window))
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

(ert-deftest hermes-chat-background-notification-uses-launch-preview ()
  "A background result notifies with its prompt preview, not result contents."
  (let (notice)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (setq notice arguments))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--background-counter 1
             hermes-chat--background-tasks
             (list (cons "bg_private"
                         (list :number 1 :preview "check the build"))))
       (hermes-chat--handle-background-complete
        (list :type 'background :task-id "bg_private"
              :content "private result contents"))
       (should (eq (car notice) 'background))
       (should (string-match-p "check the build" (nth 2 notice)))
       (should-not (string-match-p "private result" (nth 2 notice)))))))

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
           (should (equal (hermes-test--transcript-roles entries)
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
           (should (equal (hermes-test--transcript-roles entries)
                          '(user status assistant)))
           (should (string-match-p "Unknown Hermes transport event: alien.signal"
                                   (plist-get (nth 1 entries) :content)))
           (should (eq (plist-get (nth 1 entries) :status) 'error))
           (should (string-match-p "Error" header))
           (should (string-match-p "alien.signal" (hermes-chat--session-details-text))))
         (should (cl-some (lambda (line)
                            (string-match-p "Unknown Hermes transport event: alien.signal"
                                            line))
                          messages)))))))

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

(ert-deftest hermes-chat-insert-content-with-images-fail-soft ()
  "Bad image data still inserts cleaned text without signaling."
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-images-p) (lambda () t))
              ((symbol-function 'create-image)
               (lambda (&rest _) (error "bad image"))))
      (hermes-chat--insert-content-with-images
       (concat "hello "
               "data:image/png;base64," (make-string 80 ?B)
               " world")
       #'insert))
    (should (string-match-p "hello" (buffer-string)))
    (should (string-match-p "image unavailable\\|\\[image\\]" (buffer-string)))
    (should-not (string-match-p "data:image" (buffer-string)))))

(ert-deftest hermes-chat-create-image-from-url-decodes-base64-payload ()
  "Data URLs are decoded and passed to create-image as raw data."
  (let* ((payload (base64-encode-string "PNGDATA" t))
         (url (concat "data:image/png;base64," payload))
         args)
    (cl-letf (((symbol-function 'create-image)
               (lambda (data &optional type data-p &rest props)
                 (setq args (list data type data-p props))
                 '(image dummy))))
      (should (equal (hermes-chat--create-image-from-url url) '(image dummy)))
      (should (equal (nth 0 args) "PNGDATA"))
      (should-not (nth 1 args))
      (should (eq (nth 2 args) t)))))

(ert-deftest hermes-chat-create-image-from-url-rejects-malformed-base64 ()
  "Malformed base64 fails soft without calling create-image."
  (let ((url (concat "data:image/png;base64," (make-string 80 ??)))
        (called nil))
    (cl-letf (((symbol-function 'create-image)
               (lambda (&rest _)
                 (setq called t)
                 '(image dummy)))
              ((symbol-function 'base64-decode-string)
               (lambda (&rest _)
                 (error "bad base64"))))
      (should-not (hermes-chat--create-image-from-url url))
      (should-not called))))

(ert-deftest hermes-chat-create-image-from-url-rejects-oversize-base64 ()
  "Encoded payload above the hard cap is rejected before decode."
  (let* ((hermes-chat--max-embedded-image-base64 80)
         (hermes-chat--max-embedded-image-decoded-bytes 1024)
         (url (concat "data:image/png;base64," (make-string 81 ?A)))
         (decoded nil)
         (called nil))
    (cl-letf (((symbol-function 'base64-decode-string)
               (lambda (payload &optional _ignore)
                 (setq decoded payload)
                 "x"))
              ((symbol-function 'create-image)
               (lambda (&rest _)
                 (setq called t)
                 '(image dummy))))
      (should-not (hermes-chat--create-image-from-url url))
      (should-not decoded)
      (should-not called))))

(ert-deftest hermes-chat-create-image-from-url-accepts-exact-base64-limit ()
  "Encoded payload exactly at the hard cap may still decode."
  (let* ((hermes-chat--max-embedded-image-base64 80)
         (hermes-chat--max-embedded-image-decoded-bytes 1024)
         (payload (make-string 80 ?A))
         (url (concat "data:image/png;base64," payload))
         args)
    (cl-letf (((symbol-function 'base64-decode-string)
               (lambda (data &optional _ignore)
                 (should (equal data payload))
                 "PNGDATA"))
              ((symbol-function 'create-image)
               (lambda (data &optional type data-p &rest props)
                 (setq args (list data type data-p props))
                 '(image dummy))))
      (should (equal (hermes-chat--create-image-from-url url) '(image dummy)))
      (should (equal (nth 0 args) "PNGDATA")))))

(ert-deftest hermes-chat-create-image-from-url-rejects-oversize-decoded ()
  "Decoded byte count above the hard cap is rejected before create-image."
  (let* ((hermes-chat--max-embedded-image-base64 256)
         (hermes-chat--max-embedded-image-decoded-bytes 4)
         (url (concat "data:image/png;base64,"
                      (base64-encode-string "ABCDE" t)))
         (called nil))
    (cl-letf (((symbol-function 'create-image)
               (lambda (&rest _)
                 (setq called t)
                 '(image dummy))))
      (should-not (hermes-chat--create-image-from-url url))
      (should-not called))))

(ert-deftest hermes-chat-create-image-from-url-accepts-exact-decoded-limit ()
  "Decoded payload exactly at the hard cap is accepted."
  (let* ((hermes-chat--max-embedded-image-base64 256)
         (hermes-chat--max-embedded-image-decoded-bytes 4)
         (raw "ABCD")
         (url (concat "data:image/png;base64,"
                      (base64-encode-string raw t)))
         args)
    (cl-letf (((symbol-function 'create-image)
               (lambda (data &optional type data-p &rest props)
                 (setq args (list data type data-p props))
                 '(image dummy))))
      (should (equal (hermes-chat--create-image-from-url url) '(image dummy)))
      (should (equal (nth 0 args) raw)))))

(ert-deftest hermes-chat-render-expansion-metadata-preserves-explicit-state ()
  "An absent choice differs from explicit collapse, without losing metadata."
  (dolist (state '(absent nil t))
    (let* ((entry (list :metadata (unless (eq state 'absent)
                                   (list :expanded state))))
           (metadata (list :expanded 'incoming :detail "literal α" :other 42))
           (result (hermes-chat--metadata-preserve-expanded entry metadata)))
      (should (eq (plist-get result :expanded)
                  (if (eq state 'absent) 'incoming state)))
      (should (equal (plist-get result :detail) "literal α"))
      (should (= (plist-get result :other) 42)))))

(ert-deftest hermes-chat-render-diff-label-prefers-standard-headers ()
  "Prefer +++ then Git then gateway headers; retain literal target paths."
  (dolist (case '(("a/old → b/arrow α\ndiff --git a/old b/git α\n+++ b/space name α\n"
                   "space name α")
                  ("a/old → b/arrow α\ndiff --git a/old b/git α\n" "git α")
                  ("a/old → b/arrow α\n" "arrow α")
                  ("+++ a/path α\n" "path α")
                  ("+++ c/path α\n" "c/path α")
                  ("@@ -1 +1 @@\n-old\n+new\n" nil)))
    (should (equal (hermes-chat--diff-label (car case)) (cadr case)))))

(ert-deftest hermes-chat-settled-diff-eof-requires-complete-counts ()
  "Settled truncated hunks stay literal; complete hunks still disclose."
  (dolist (complete '(nil t))
    (dolist (ending '("" "\n" "\n\\ No newline at end of file\n"))
      (let ((text (concat (if complete "@@ -1 +1 @@" "@@ -1,2 +1,2 @@")
                          "\n-old α\n+new β" ending)))
        (hermes-test-with-chat-buffer
         (insert "DRAFT")
         (hermes-chat--insert-entry (hermes-chat--make-entry 'assistant text 'done))
         (should (equal (hermes-chat-input-string) "DRAFT"))
         (if complete
             (progn
               (should (= 1 (hermes-test--count-buttons-labeled "View Diff")))
               (should (equal (hermes-test--view-diff-content)
                              (if (string-suffix-p "\n" text) text
                                (concat text "\n")))))
           (should (zerop (hermes-test--count-buttons-labeled "View Diff")))
           (goto-char (point-min))
           (search-forward text)
           (should (equal (filter-buffer-substring (- (point) (length text))
                                                  (point))
                          text))))))))

(provide 'hermes-chat-render-tests)
;;; hermes-chat-render-tests.el ends here
