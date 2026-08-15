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
  (should (eq (keymap-lookup hermes-chat-mode-map "S-<return>") #'hermes-chat-newline))
  (should (eq (keymap-lookup hermes-chat-mode-map "M-p")
              #'hermes-chat-input-history-previous))
  (should (eq (keymap-lookup hermes-chat-mode-map "M-n")
              #'hermes-chat-input-history-next)))

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

(ert-deftest hermes-chat-notification-follows-completed-reply ()
  "A completed turn notifies with the settled assistant text and owning buffer."
  (let (callback notice)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (setq notice arguments))))
      (hermes-test-with-chat-buffer
       (let ((buffer (current-buffer))
             (hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "notify me")
         (hermes-chat-send)
         (funcall callback '(:type delta :content "Finished\ncleanly"))
         (funcall callback '(:type done))
         (should (eq (car notice) 'chat-reply))
         (should (equal (nth 1 notice)
                        (format "%s: Finished cleanly" (buffer-name buffer))))
         (should (equal (nth 2 notice) "Finished cleanly"))
         (should (eq (plist-get (nthcdr 3 notice) :buffer) buffer)))))))

(ert-deftest hermes-chat-notification-reports-terminal-error-not-interrupt ()
  "A real terminal error notifies, while an intentional interrupt does not."
  (let (callback notices)
    (cl-letf (((symbol-function 'hermes-notifications-notify)
               (lambda (&rest arguments) (push arguments notices))))
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "fail")
         (hermes-chat-send)
         (funcall callback '(:type error :content "backend failed"))
         (should (equal (mapcar #'car notices) '(chat-error)))
         (should (equal (nth 2 (car notices)) "backend failed"))))
      (setq notices nil)
      (hermes-test-with-chat-buffer
       (let ((hermes-transport-send-function
              (lambda (_prompt cb)
                (setq callback cb)
                'fake-process)))
         (insert "interrupt")
         (hermes-chat-send)
         (funcall callback '(:type error :status interrupted))
         (should-not notices))))))

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

(ert-deftest hermes-chat-rename-updates-title-not-project-identity ()
  "Renaming stores a canonical title without changing project identity."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time 45 30 18 7 8 2026 t))))
    (hermes-test-with-chat-buffer
     (let ((project-name (buffer-name)))
       (hermes-chat-rename "  My Project  ")
       (should (equal hermes-chat--title
                      "My Project--20260807T183045.000000Z--emacs"))
       (should hermes-chat--title-manual-p)
       (should (equal (buffer-name) project-name))))))

(ert-deftest hermes-chat-rename-rejects-empty-title ()
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-rename "   ") :type 'user-error)))

(ert-deftest hermes-chat-rename-pushes-server-title-when-attached ()
  "An attached session pushes `session.title' with the live session id."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time 45 30 18 7 8 2026 t))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-active-session-id "sid-1")
     (let (sent)
       (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                  (lambda () t))
                 ((symbol-function 'hermes-dashboard-transport-session-title)
                  (lambda (_client &rest args) (setq sent args))))
         (hermes-chat-rename "Renamed"))
       (should (equal (plist-get sent :session-id) "sid-1"))
       (should (equal (plist-get sent :title)
                      "Renamed--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-rename-preserves-canonical-session-timestamp ()
  "Renaming an identified session changes its label, not its timestamp."
  (hermes-test-with-chat-buffer
   (let ((project-name (buffer-name)))
     (setq hermes-chat--title "Old--20260102T030405.123456Z--emacs")
     (hermes-chat-rename "New")
     (should (equal hermes-chat--title
                    "New--20260102T030405.123456Z--emacs"))
     (should (equal (buffer-name) project-name)))))

(ert-deftest hermes-chat-buffer-name-formats-project-identity ()
  "Buffer names carry profile and gateway directory, never session title."
  (should (equal (hermes-chat--buffer-name
                  "coder" nil "/tmp/emacs-hermes/")
                 "*Hermes@coder: [emacs-hermes]*"))
  (should (equal (hermes-chat--buffer-name
                  nil nil "C:\\Users\\Thanos\\project\\")
                 "*Hermes@default: [project]*")))

(ert-deftest hermes-chat-buffer-name-identifies-instance-when-multiple ()
  "Named-instance chat buffers use the instance before the profile."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote)))
    (should (equal (hermes-chat--buffer-name
                    nil local "/tmp/emacs-hermes/")
                   "*local@default: [emacs-hermes]*"))
    (should (equal
             (hermes-chat--buffer-name
              "coder" remote "/tmp/emacs-hermes/")
             "*remote@coder: [emacs-hermes]*"))))

(ert-deftest hermes-chat-buffer-name-uses-named-single-instance ()
  "A named single instance remains part of the project identity."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list local)))
    (should (equal (hermes-chat--buffer-name
                    "coder" local "/tmp/emacs-hermes/")
                   "*local@coder: [emacs-hermes]*"))))

(ert-deftest hermes-chat-create-uses-project-canonical-title ()
  "Fresh dashboard sessions use a canonical title from the current project."
  (let ((client (hermes-test--dashboard-client)) created-title)
    (cl-letf (((symbol-function 'current-time)
               (lambda () (encode-time 45 30 18 7 8 2026 t)))
              ((symbol-function 'project-current) (lambda (&rest _) 'project))
              ((symbol-function 'project-root)
               (lambda (_project) "/tmp/emacs-hermes/"))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq created-title (plist-get args :title))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) nil)))
      (hermes-test-with-chat-buffer
       (insert "hello")
       (hermes-chat-send)
       (should (equal created-title
                      "emacs-hermes--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-prompts-profile-and-names-buffer ()
  "M-x hermes-chat reads a profile and names the buffer after it."
  (cl-letf (((symbol-function 'hermes-chat--read-profile)
             (lambda () "coder")))
    (let ((buffer (call-interactively #'hermes-chat)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal hermes-chat--profile "coder"))
            (should (string-prefix-p "*Hermes@coder" (buffer-name))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-uses-current-default-directory ()
  "A new chat keeps the directory of the buffer that launched it."
  (let ((origin (generate-new-buffer " *hermes-chat-origin*")) chat)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer origin
            (setq default-directory
                  (file-name-as-directory temporary-file-directory))
            (setq chat (hermes-chat nil)))
          (with-current-buffer chat
            (should (equal default-directory
                           (file-name-as-directory temporary-file-directory)))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (kill-buffer origin))))

(ert-deftest hermes-chat-selects-instance-before-profile ()
  "Interactive chat selection pins the instance used to read its profile."
  (let ((instance '("remote" . "https://hermes.example.test"))
        profile-instance)
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () instance))
              ((symbol-function 'hermes-chat--read-profile)
               (lambda ()
                 (setq profile-instance hermes-instance)
                 "coder")))
      (let ((buffer (call-interactively #'hermes-chat)))
        (unwind-protect
            (with-current-buffer buffer
              (should (equal profile-instance instance))
              (should (equal hermes-instance instance))
              (should (equal hermes-chat--profile "coder")))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-blank-profile-names-buffer-default ()
  "A blank profile yields the default profile name and no stored profile."
  (let ((buffer (hermes-chat "")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not hermes-chat--profile)
          (should (string-prefix-p "*Hermes@default" (buffer-name))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-should-apply-title-p-rules ()
  "A fetched title applies only when non-empty, changed, and not manual."
  (should (hermes-chat--should-apply-title-p "New" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "New" "Old" t))
  (should-not (hermes-chat--should-apply-title-p "" "Old" nil))
  (should-not (hermes-chat--should-apply-title-p "Same" "Same" nil))
  (should-not (hermes-chat--should-apply-title-p nil "Old" nil)))

(ert-deftest hermes-chat-done-refreshes-session-title ()
  "A completed turn fetches title metadata without renaming or pushing."
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
         (let ((project-name (buffer-name)))
           (insert "hi")
           (hermes-chat-send)
           (funcall callback '(:type done))
           ;; The title fetch is deferred off the event handler; let it run.
           (sit-for 0.05)
           (should (equal hermes-chat--title "Auto Title"))
           (should (equal (buffer-name) project-name))
           (should (= pushes 0))))))))

(ert-deftest hermes-chat-manual-title-survives-refresh ()
  "A manually set title is not overwritten by the automatic refresh."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'current-time)
               (lambda () (encode-time 45 30 18 7 8 2026 t)))
              ((symbol-function 'hermes-chat--dashboard-session-attached-p)
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
       (should (equal hermes-chat--title
                      "Pinned--20260807T183045.000000Z--emacs"))))))

(ert-deftest hermes-chat-snapshot-prefers-title ()
  "The dashboard snapshot uses the chat title over the buffer name."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--title "Pinned--20260807T183045.123456Z--emacs")
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
      (should (string-match-p "Goal 1/20" (hermes-test--header-line-string)))
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
       (should (string-match-p "✓ Ready" (hermes-test--header-line-string)))
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

(ert-deftest hermes-chat-idle-session-reconnects-before-server-turn ()
  "An idle control session resumes and renders after a socket reconnect."
  (let ((client (hermes-test--dashboard-client)) resumed)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client stored-id &rest args)
                 (setq resumed stored-id)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-resumed")
                            (resumed . "stored-idle"))))))
      (let ((hermes-chat-use-dashboard-transport t)
            (hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client)
         (hermes-chat--dashboard-record-session
          client '((session_id . "sid-old")
                   (stored_session_id . "stored-idle")))
         (hermes-dashboard-transport--dispatch-event
          client '(:type status :status reconnecting))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-session-ready-p)
         (hermes-dashboard-transport--dispatch-event
          client '(:type status :status reconnected))
         (should (equal resumed "stored-idle"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-resumed"))
         (hermes-dashboard-transport--dispatch-event
          client '(:type status :event "message.start" :status "started"
                   :session-id "sid-resumed"))
         (hermes-dashboard-transport--dispatch-event
          client '(:type delta :content "Hello after reconnect"
                   :session-id "sid-resumed"))
         (hermes-dashboard-transport--dispatch-event
          client '(:type done :session-id "sid-resumed"))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :content)
                          "Hello after reconnect"))
           (should (eq (plist-get assistant :status) 'done))))))))

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
  (should (eq (keymap-lookup hermes-chat-actions-map "w")
              #'hermes-chat-set-directory))
  (let* ((rows (keymap-popup--meta hermes-chat-actions-map 'descriptions))
         (groups (apply #'append rows))
         (group-names (mapcar (lambda (group) (plist-get group :name)) groups))
         (entries (apply #'append
                         (mapcar (lambda (group)
                                   (plist-get group :entries))
                                 groups))))
    (should (equal group-names
                   '("Turn" "Prompt" "Session" "Connection" "Workspace"
                     "Browse" "Commands")))
    (should (equal (mapcar (lambda (row)
                             (mapcar (lambda (group)
                                       (plist-get group :name))
                                     row))
                           rows)
                   '(("Turn" "Prompt" "Session" "Connection")
                     ("Workspace" "Browse" "Commands"))))
    (dolist (group groups)
      (should (<= (length (plist-get group :entries)) 4)))
    (let ((directory-entry
           (cl-find "w" entries :key (lambda (entry)
                                       (plist-get entry :key))
                    :test #'equal)))
      (should directory-entry)
      (should (eq (plist-get directory-entry :inapt-if)
                  #'hermes-chat--active-turn-p)))
    (dolist (key '("n" "m" "x" "b" "t"))
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

(ert-deftest hermes-chat-sanitize-fragment-state-is-explicit ()
  "ANSI fragments are pure values that can be carried by independent streams."
  (let* ((first (hermes-chat--sanitize-content-with-fragment
                 "left\e[38;2;255" nil))
         (other (hermes-chat--sanitize-content-with-fragment "right" nil))
         (continued (hermes-chat--sanitize-content-with-fragment
                     ";0;0m!" (cdr first))))
    (should (equal (car first) "left"))
    (should (equal (car other) "right"))
    (should-not (cdr other))
    (should (equal (car continued) "!"))
    (should-not (cdr continued))))

(ert-deftest hermes-chat-resume-renders-prior-messages ()
  "Resuming renders history and later backend-owned turns without duplicates."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-notifications-notify) #'ignore)
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
       (should (equal (hermes-test--queued-contents) '("second")))
       (should (equal (hermes-chat-input-string) ""))
       (funcall (car (last callbacks)) '(:type done))
       (should (equal sent '("second" "first")))))))

(ert-deftest hermes-chat-dashboard-send-defers-busy-policy-to-backend ()
  "A normal busy send reaches `prompt.submit' without a client interrupt."
  (let ((client (hermes-test--dashboard-client)) submits interrupts)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (funcall (plist-get args :resolve)
                          `((status . ,(if (equal text "first")
                                           "streaming"
                                         "queued"))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (&rest _args) (setq interrupts t))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (should (equal submits '("second" "first")))
         (should-not interrupts)
         (should-not (hermes-test--queued-contents)))))))

(ert-deftest hermes-chat-dashboard-busy-send-preserves-local-fifo-order ()
  "A normal busy send stays behind messages already queued explicitly."
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (funcall (plist-get args :resolve)
                          '((status . "streaming"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-queue-message "queued-first")
         (insert "busy-second")
         (hermes-chat-send)
         (should (equal submits '("first")))
         (should (equal (hermes-test--queued-contents)
                        '("queued-first" "busy-second"))))))))

(ert-deftest hermes-chat-dashboard-busy-submit-signal-preserves-input ()
  "A synchronous busy-submit failure restores the deleted input."
  (let ((client (hermes-test--dashboard-client)) signaled)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (if (equal text "first")
                     (funcall (plist-get args :resolve)
                              '((status . "streaming")))
                   (error "submit failed")))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (condition-case err
             (hermes-chat-send)
           (error (setq signaled (error-message-string err))))
         (should-not signaled)
         (should (equal (hermes-chat-input-string) "second")))))))

(ert-deftest hermes-chat-dashboard-streaming-busy-result-settles-old-turn ()
  "A busy send that finds the backend idle starts a clean new local turn."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (funcall (plist-get args :resolve)
                          '((status . "streaming"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (let ((first-id hermes-chat--pending-assistant-id))
           (insert "second")
           (hermes-chat-send)
           (should (eq (plist-get (ewoc-data (gethash first-id hermes-chat--nodes))
                                  :status)
                       'done))
           (hermes-test--emit-dashboard-event
            client "message.delta" '((text . "second answer")))
           (should (equal
                    (plist-get (hermes-test--last-assistant-entry) :content)
                    "second answer"))))))))

(ert-deftest hermes-chat-invalidates-interim-assistant-state ()
  (hermes-test-with-chat-buffer
    (setq hermes-chat--dashboard-interim-assistant-id "sealed")
    (hermes-chat--invalidate-transport-state)
    (should-not hermes-chat--dashboard-interim-assistant-id)))

(ert-deftest hermes-chat-dashboard-preserves-interim-assistant-message ()
  "A verification candidate remains visible beside the later final response."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (funcall (plist-get args :resolve)
                          '((status . "streaming"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "verify")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "candidate")))
         (hermes-test--emit-dashboard-event
          client "message.interim"
          '((text . "candidate") (already_streamed . t)))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "verified")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "verified") (status . "complete")))
         (let ((assistants
                (cl-remove-if-not
                 (lambda (entry) (eq (plist-get entry :role) 'assistant))
                 (hermes-chat--entries))))
           (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                                  assistants)
                          '("candidate" "verified")))
           (should (equal (mapcar (lambda (entry) (plist-get entry :status))
                                  assistants)
                          '(done done)))))))))

(ert-deftest hermes-chat-dashboard-settles-previewed-final-on-interim ()
  "A reused verification candidate settles once rather than duplicating."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (funcall (plist-get args :resolve)
                          '((status . "streaming"))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "verify")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "candidate")))
         (hermes-test--emit-dashboard-event
          client "message.interim"
          '((text . "candidate") (already_streamed . t)))
         (hermes-test--emit-dashboard-event
          client "message.complete"
          '((text . "candidate") (status . "complete") (response_previewed . t)))
         (let ((assistants
                (cl-remove-if-not
                 (lambda (entry) (eq (plist-get entry :role) 'assistant))
                 (hermes-chat--entries))))
           (should (= (length assistants) 1))
           (should (equal (plist-get (car assistants) :content) "candidate"))
           (should (eq (plist-get (car assistants) :status) 'done))))))))

(ert-deftest hermes-chat-dashboard-buffers-new-turn-before-streaming-ack ()
  "A new turn cannot capture events until its busy-submit result is known."
  (let ((client (hermes-test--dashboard-client)) second-resolve)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (if (equal text "first")
                     (funcall (plist-get args :resolve)
                              '((status . "streaming")))
                   (setq second-resolve (plist-get args :resolve))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (let ((first-id hermes-chat--pending-assistant-id))
           (insert "second")
           (hermes-chat-send)
           (hermes-test--emit-dashboard-event client "message.start" nil)
           (hermes-test--emit-dashboard-event
            client "message.delta" '((text . "early second answer")))
           (funcall second-resolve '((status . "streaming")))
           (should (string-empty-p
                    (plist-get (ewoc-data (gethash first-id hermes-chat--nodes))
                               :content)))
           (should (equal
                    (plist-get (hermes-test--last-assistant-entry) :content)
                    "early second answer"))))))))

(ert-deftest hermes-chat-dashboard-buffers-direct-handoff-before-queued-ack ()
  "Direct queued handoff stays ordered when events beat the RPC response."
  (let ((client (hermes-test--dashboard-client)) second-resolve)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (if (equal text "first")
                     (funcall (plist-get args :resolve)
                              '((status . "streaming")))
                   (setq second-resolve (plist-get args :resolve))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "first answer")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "done") (status . "done")))
         (hermes-test--emit-dashboard-event client "message.start" nil)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "early second answer")))
         (funcall second-resolve '((status . "queued")))
         (should (equal (plist-get (hermes-test--assistant-entry) :content)
                        "done"))
         (should (eq (plist-get (hermes-test--assistant-entry) :status) 'done))
         (should-not hermes-chat--server-queued-prior-terminal-p)
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "early second answer")))))))

(ert-deftest hermes-chat-dashboard-close-clears-backend-queued-turn ()
  "Session loss cannot leave a backend-queued placeholder active."
  (let ((client (hermes-test--dashboard-client)) callback)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 (setf (hermes-dashboard-transport-client-callback client) callback)
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (funcall (plist-get args :resolve)
                          `((status . ,(if (equal text "first")
                                           "streaming"
                                         "queued")))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (funcall callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (should-not hermes-chat--server-queued-assistant-id)
         (should-not hermes-chat--server-queued-user-id)
         (should-not (hermes-chat--active-turn-p)))))))

(ert-deftest hermes-chat-send-queues-multiple-messages-in-fifo-order ()
  (let (sent callbacks)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (prompt callback)
              (push prompt sent)
              (push callback callbacks)
              'fake-process)))
       (insert "first")
       (hermes-chat-send)
       (insert "second")
       (hermes-chat-send)
       (insert "third")
       (hermes-chat-send)
       (should (equal sent '("first")))
       (should (equal (hermes-test--queued-contents) '("second" "third")))
       (funcall (car callbacks) '(:type done))
       (should (equal sent '("second" "first")))
       (should (equal (hermes-test--queued-contents) '("third")))
       (funcall (car callbacks) '(:type done))
       (should (equal sent '("third" "second" "first")))
       (should-not (hermes-test--queued-contents))))))

(ert-deftest hermes-chat-dashboard-queued-send-keeps-current-stream ()
  "A backend-queued send takes ownership only at the next message start."
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve
                            `((status . ,(if (equal text "first")
                                             "streaming"
                                           "queued"))))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (should (equal submits '("second" "first")))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "still working")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "done") (status . "done")))
         (hermes-test--emit-dashboard-event
          client "session.info" '((running . :false)))
         (hermes-test--emit-dashboard-event client "message.start" nil)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "second answer")))
         (should (equal (plist-get (hermes-test--assistant-entry) :content)
                        "done"))
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "second answer")))))))

(ert-deftest hermes-chat-backend-queued-ack-is-settled-status ()
  "Backend queue acceptance is an acknowledgement, not live progress."
  (hermes-test-with-chat-buffer
   (let* ((user (hermes-chat--make-entry 'user "queued" 'done))
          (assistant (hermes-chat--make-entry 'assistant "" 'pending))
          (context (list :user-id (plist-get user :id)
                         :assistant-id (plist-get assistant :id)
                         :generation hermes-chat--transport-generation
                         :idle-count hermes-chat--dashboard-idle-count)))
     (hermes-chat--insert-entry user)
     (hermes-chat--insert-entry assistant)
     (hermes-chat--busy-submit-queued context)
     (let ((ack (cl-find-if
                 (lambda (entry)
                   (and (eq (plist-get entry :role) 'status)
                        (equal (plist-get entry :content) "Queued by Hermes")))
                 (hermes-chat--entries))))
       (should ack)
       (should (eq (plist-get ack :status) 'done))))))

(ert-deftest hermes-chat-dashboard-queued-send-starts-after-terminal-without-idle ()
  "A direct backend queue handoff does not require `session.info' idle."
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve
                            `((status . ,(if (equal text "first")
                                             "streaming"
                                           "queued"))))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "done") (status . "interrupted")))
         (should hermes-chat--server-queued-prior-terminal-p)
         (hermes-test--emit-dashboard-event client "message.start" nil)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "second answer")))
         (should-not hermes-chat--server-queued-assistant-id)
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "second answer")))))))

(ert-deftest hermes-chat-dashboard-rejected-busy-submit-preserves-input ()
  "A rejected busy submit restores its text without a fake turn."
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (if (equal text "second")
                     (when-let* ((reject (plist-get args :reject)))
                       (funcall reject "session busy"))
                   (when-let* ((resolve (plist-get args :resolve)))
                     (funcall resolve '((status . "streaming"))))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "done") (status . "done")))
         (hermes-test--emit-dashboard-event
          client "session.info" '((running . :false)))
         (should (equal submits '("second" "first")))
         (should-not (hermes-test--queued-contents))
         (should (equal (hermes-chat-input-string) "second"))
         (should (= (cl-count 'user (hermes-chat--entries)
                              :key (lambda (entry) (plist-get entry :role)))
                    1))
         (should (= (cl-count 'assistant (hermes-chat--entries)
                              :key (lambda (entry) (plist-get entry :role)))
                    1))
         (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-dashboard-error-settles-without-idle-event ()
  "A terminal backend error releases local busy state without session.info."
  (let ((client (hermes-test--dashboard-client)) submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (&rest _args)
                 (ert-fail "Terminal events should not require reconciliation"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "error" '((message . "agent initialization failed")))
         (should (equal submits '("second" "first")))
         (should-not hermes-chat--dashboard-running-p)
         (should-not (hermes-test--queued-contents)))))))

(ert-deftest hermes-chat-dashboard-busy-queued-result-stays-server-owned ()
  "A busy result waits for the accepted backend queue without resubmitting."
  (let ((client (hermes-test--dashboard-client)) submits interrupts)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve
                            `((status . ,(if (= (length submits) 2)
                                             "queued"
                                           "streaming")))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (push args interrupts))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "first done") (status . "done")))
         (hermes-test--emit-dashboard-idle client)
         (should-not interrupts)
         (should-not (hermes-test--queued-contents))
         (should (= (cl-count 'user (hermes-chat--entries)
                              :key (lambda (entry) (plist-get entry :role)))
                    2))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "other run output")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "interrupted")))
         (hermes-test--emit-dashboard-idle client)
         (hermes-test--emit-dashboard-event client "message.start" nil)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "second answer")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "done")))
         (should (equal submits '("second" "first")))
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "second answer")))))))

(ert-deftest hermes-chat-dashboard-busy-redirected-result-keeps-current-turn ()
  "A busy redirect keeps the current assistant without a fake user turn."
  (let ((client (hermes-test--dashboard-client))
        submits interrupts redirect-resolve assistant-id)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (if (= (length submits) 2)
                       (setq redirect-resolve resolve)
                     (funcall resolve '((status . "streaming")))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (&rest _args) (setq interrupts t))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (setq assistant-id hermes-chat--pending-assistant-id)
         (insert "second")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "continued run")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "done")))
         (funcall redirect-resolve '((status . "redirected")))
         (should (equal submits '("second" "first")))
         (should-not interrupts)
         (should-not (hermes-test--queued-contents))
         (should (= (cl-count 'user (hermes-chat--entries)
                              :key (lambda (entry) (plist-get entry :role)))
                    1))
         (should (= (cl-count 'assistant (hermes-chat--entries)
                              :key (lambda (entry) (plist-get entry :role)))
                    1))
         (should (cl-find-if
                  (lambda (entry)
                    (and (eq (plist-get entry :role) 'status)
                         (equal (plist-get entry :content)
                                "Redirected: second")))
                  (hermes-chat--entries)))
         (should (eq (plist-get (car (last (hermes-chat--entries))) :role)
                     'assistant))
         (should (equal (plist-get (hermes-test--last-assistant-entry) :id)
                        assistant-id))
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "continued run")))))))

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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
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
         (should (equal (hermes-test--queued-contents) '("cite files")))
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

(ert-deftest hermes-chat-interrupt-freezes-partial-reply ()
  "Interrupt keeps streamed text and ignores later cancellation payloads."
  (let ((client (hermes-test--dashboard-client)) interrupt-resolve)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client _text &rest args)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long prompt")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "partial")))
         (hermes-chat-interrupt)
         (should (eq (plist-get hermes-chat--status-state :status)
                     'interrupted))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . " late")))
         (hermes-test--emit-dashboard-event
          client "message.complete"
          '((text . "Operation interrupted by user")
            (status . "interrupted")))
         (let ((assistant (hermes-test--assistant-entry)))
           (should (equal (plist-get assistant :content) "partial"))
           (should (equal (hermes-chat--status-name
                           (plist-get assistant :status))
                          "interrupted")))
         (should-not (string-match-p "Transport error" (buffer-string)))
         (should-not (string-match-p "Operation interrupted" (buffer-string)))
         (should (functionp interrupt-resolve)))))))

(ert-deftest hermes-chat-interrupt-signal-restores-live-turn ()
  "A synchronous interrupt failure must resume rendering the current turn."
  (let ((client (hermes-test--dashboard-client)) signaled)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) nil))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (&rest _args)
                 (user-error "interrupt failed"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long prompt")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "partial")))
         (condition-case err
             (hermes-chat-interrupt)
           (error (setq signaled (error-message-string err))))
         (should-not signaled)
         (should-not hermes-chat--interrupted-assistant-id)
         (should hermes-chat--pending-assistant-id)
         (should (equal (plist-get (hermes-test--assistant-entry) :status)
                        'streaming))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . " continues")))
         (should (equal (plist-get (hermes-test--assistant-entry) :content)
                        "partial continues")))))))

(ert-deftest hermes-chat-interrupt-rejection-replays-buffered-output ()
  "Output held while interrupt is pending is restored when the request fails."
  (let ((client (hermes-test--dashboard-client)) interrupt-reject)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) nil))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-reject (plist-get args :reject)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "long prompt")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "partial")))
         (hermes-chat-interrupt)
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . " retained")))
         (funcall interrupt-reject "not interruptible")
         (should-not hermes-chat--interrupted-assistant-id)
         (should (equal (plist-get (hermes-test--assistant-entry) :content)
                        "partial retained")))))))

(ert-deftest hermes-chat-interrupt-ack-reconciles-silent-startup-cancel ()
  "An accepted early interrupt drains after resume confirms backend idle."
  (let ((client (hermes-test--dashboard-client))
        interrupt-resolve scheduled submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")
                            (stored_session_id . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (should (equal (cons name arg) '("goal" . "status")))
                 (funcall (plist-get args :resolve)
                          '((type . "exec")
                            (output . "No active goal.")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (should (equal session-id "sid-stored"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active") (running . nil)))))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'fake-timer)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-interrupt-and-send "second")
         (funcall interrupt-resolve '((status . "interrupted")))
         (should scheduled)
         (apply (car scheduled) (cdr scheduled))
         (should (equal submits '("second" "first")))
         (should-not (hermes-test--queued-contents)))))))

(ert-deftest hermes-chat-late-interrupt-ack-does-not-touch-new-turn ()
  "An interrupt callback is scoped to the turn that issued it."
  (let ((client (hermes-test--dashboard-client)) interrupt-resolve submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-interrupt)
         (funcall interrupt-resolve '((status . "interrupted")))
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "interrupted")))
         (hermes-test--emit-dashboard-event
          client "session.info" '((running . :false)))
         (insert "second")
         (hermes-chat-send)
         (should (eq (plist-get hermes-chat--status-state :status) 'pending))
         (funcall interrupt-resolve '((status . "interrupted")))
         (should (equal submits '("second" "first")))
         (should (eq (plist-get hermes-chat--status-state :status) 'pending)))))))

(ert-deftest hermes-chat-late-steer-rejection-submits-when-idle ()
  "A steer rejected after settlement does not strand its text in the queue."
  (let ((client (hermes-test--dashboard-client)) steer-reject submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client _text &rest args)
                 (setq steer-reject (plist-get args :reject)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-steer-message "second")
         (hermes-test--emit-dashboard-event
          client "message.complete" '((text . "done") (status . "done")))
         (hermes-test--emit-dashboard-event
          client "session.info" '((running . :false)))
         (funcall steer-reject "steer unavailable")
         (should (equal submits '("second" "first")))
         (should-not (hermes-test--queued-contents)))))))

(ert-deftest hermes-chat-stale-steer-rejection-queues-during-new-turn ()
  "A definitive steer rejection keeps its text after a newer turn starts."
  (let ((client (hermes-test--dashboard-client)) steer-reject submits)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client _text &rest args)
                 (setq steer-reject (plist-get args :reject)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-steer-message "second")
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "done")))
         (hermes-test--emit-dashboard-idle client)
         (insert "third")
         (hermes-chat-send)
         (funcall steer-reject "rejected")
         (should (equal submits '("third" "first")))
         (should (equal (hermes-test--queued-contents) '("second")))
         (should-not (cl-find-if
                      (lambda (entry)
                        (string-prefix-p
                         "Steering" (or (plist-get entry :content) "")))
                      (hermes-chat--entries))))))))

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
         (should-not (hermes-test--queued-contents)))))))

(ert-deftest hermes-chat-interrupt-clears-backend-queued-turn ()
  "An accepted interrupt settles the queued prompt discarded by Hermes."
  (let ((client (hermes-test--dashboard-client)) interrupt-resolve)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (funcall (plist-get args :resolve)
                          `((status . ,(if (equal text "first")
                                           "streaming"
                                         "queued"))))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (hermes-chat-interrupt)
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "interrupted")))
         (funcall interrupt-resolve '((status . "ok")))
         (hermes-test--emit-dashboard-idle client)
         (should-not hermes-chat--server-queued-assistant-id)
         (should-not hermes-chat--server-queued-user-id)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--dashboard-running-p)
         (should-not (hermes-chat--active-turn-p)))))))

(ert-deftest hermes-chat-interrupt-waits-for-busy-submit-result ()
  "Interrupt cannot race an unresolved busy submission acknowledgement."
  (let ((client (hermes-test--dashboard-client)) busy-resolve (interrupts 0))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (if (equal text "first")
                     (funcall (plist-get args :resolve)
                              '((status . "streaming")))
                   (setq busy-resolve (plist-get args :resolve)))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (&rest _args) (cl-incf interrupts))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (should-error (hermes-chat-interrupt) :type 'user-error)
         (should (zerop interrupts))
         (funcall busy-resolve '((status . "queued")))
         (hermes-chat-interrupt)
         (should (= interrupts 1)))))))

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
         (should (equal (hermes-test--queued-contents) '("follow up"))))))))

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
         (setq stopped 0
               hermes-chat--model "stale-model"
               hermes-chat--agent-name "stale-agent"
               hermes-chat--context '(:used 45000 :max 200000 :percent 22)
               hermes-chat--goal '(:running t :turns-used 2 :max-turns 20)
               hermes-chat--runtime-flags '(:reasoning-effort "high" :fast t :yolo t))
         (insert "/clear")
         (hermes-chat-send)
         (should (= stopped 1))
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--session-id)
         (should-not hermes-chat--model)
         (should-not hermes-chat--agent-name)
         (should-not hermes-chat--context)
         (should-not hermes-chat--goal)
         (should-not hermes-chat--runtime-flags)
         (should-not (string-match-p "Goal\\|ctx\\|stale-model\\|high\\|fast\\|YOLO"
                                     (hermes-test--header-line-string)))
         (should (equal (mapcar (lambda (e) (plist-get e :role))
                                (hermes-chat--entries))
                        '(status))))))))

(ert-deftest hermes-chat-reset-runs-buffer-cleanup-before-reinitializing ()
  "Reset cancels per-buffer resources before constructing the new transcript."
  (let ((timer 'handoff-timer) cancelled cleanup-ran)
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (value) (push value cancelled)))
              ((symbol-function 'hermes-chat--stop-dashboard-client) #'ignore))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--handoff-poll (list :timer timer :id 'old)
             hermes-chat-cleanup-functions
             (list (lambda ()
                     (setq cleanup-ran t)
                     (hermes-chat--handoff-stop))))
       (hermes-chat--reset-transcript)
       (should cleanup-ran)
       (should (memq timer cancelled))
       (should-not hermes-chat--handoff-poll)))))

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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming")))))))
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
         (should (equal (hermes-test--queued-contents) '("cite files")))
         (should (equal submits '("first")))
         (funcall callback '(:type done :session-id "sid-active"))
         (hermes-test--emit-dashboard-idle client)
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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
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
         (hermes-test--emit-dashboard-idle client)
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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
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
       (should (equal (hermes-test--queued-contents)
                      '("use demo while busy")))
       (should (string-match-p "loading skill: demo" (buffer-string)))
       (funcall callback '(:type done :session-id "sid-active"))
       (hermes-test--emit-dashboard-idle client)
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
               (lambda (_client text &rest args)
                 (setq submit-text text)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
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
       (setq queued-result (car (hermes-test--queued-contents)))
       (should-not submit-text)
       (should (equal queued-result "use demo skill while remote turn runs"))
       (should hermes-chat--pending-assistant-id)
       (should (string-match-p "Hermes session is still running"
                               (buffer-string)))
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (hermes-test--emit-dashboard-idle client "sid-live")
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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
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
       (should (equal (hermes-test--queued-contents) '("queued prompt")))
       (should hermes-chat--pending-assistant-id)
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (hermes-test--emit-dashboard-idle client "sid-live")
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
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming")))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored-session")
       (hermes-chat-queue-message "queued via key")
       (should-not submits)
       (should (equal (hermes-test--queued-contents) '("queued via key")))
       (should hermes-chat--pending-assistant-id)
       (hermes-dashboard-transport--dispatch-event client
                '(:type done :session-id "sid-live"))
       (hermes-test--emit-dashboard-idle client "sid-live")
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
                      '(("goal" . "status")
                        ("short" . "now")
                        ("demo" . "now"))))
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

(ert-deftest hermes-chat-goal-command-refreshes-vanilla-state ()
  "A successful `/goal' command refreshes state through vanilla status output."
  (let ((client (hermes-test--dashboard-client)) status-queries)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (_client _command &rest args)
                 (funcall (plist-get args :resolve)
                          '((type . "exec") (output . "Goal paused")))))
              ((symbol-function 'hermes-dashboard-transport-command-dispatch)
               (lambda (_client name arg &rest args)
                 (push (cons name arg) status-queries)
                 (funcall (plist-get args :resolve)
                          '((type . "exec")
                            (output . "⏸ Goal (paused, 2/20 turns): Ship it"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/goal pause")
       (hermes-chat-send)
       (should (equal status-queries '(("goal" . "status"))))
       (should-not (plist-get hermes-chat--goal :running))))))

(ert-deftest hermes-chat-reasoning-command-mutates-live-session ()
  "`/reasoning' sets and reads back the owned dashboard session."
  (let ((client (hermes-test--dashboard-client)) set-args get-args)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _args)
                 (ert-fail "Reasoning must not run in the isolated slash worker")))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq set-args (list key value
                                      (plist-get args :session-id)))
                 (funcall (plist-get args :resolve) '((value . "ultra")))))
              ((symbol-function 'hermes-dashboard-transport-config-get)
               (lambda (_client key &rest args)
                 (setq get-args (list key (plist-get args :session-id)))
                 (funcall (plist-get args :resolve) '((value . "ultra"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--runtime-flags '(:reasoning-effort "high"))
       (insert "/reasoning ultra")
       (hermes-chat-send)
       (should (equal set-args '("reasoning" "ultra" "sid-active")))
       (should (equal get-args '("reasoning" "sid-active")))
       (should-not
        (cl-find-if (lambda (entry)
                      (and (eq (plist-get entry :role) 'status)
                           (string-match-p "Reasoning set"
                                           (or (plist-get entry :content) ""))))
                    (hermes-chat--entries)))
       (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                      "ultra"))
       (should (string-match-p "ultra" (hermes-test--header-line-string)))
       (should-not (string-match-p "high" (hermes-test--header-line-string)))))))

(ert-deftest hermes-chat-reasoning-request-projects-scope ()
  "Reasoning arguments project to one value and optional global scope."
  (should (equal (hermes-chat--reasoning-request "ultra --session")
                 '("ultra")))
  (should (equal (hermes-chat--reasoning-request "--global ultra")
                 '("ultra" . "global"))))

(ert-deftest hermes-chat-global-reasoning-omits-live-session ()
  "Global reasoning uses `config.set' without scoping it to the live session."
  (let ((client (hermes-test--dashboard-client)) request)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq request
                       (list key value (plist-get args :session-id)))
                 (funcall (plist-get args :resolve) nil)))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _args)
                 (ert-fail "Global reasoning must use config.set"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/reasoning ultra --global")
       (hermes-chat-send)
       (should (equal request '("reasoning" "ultra" nil)))))))

(ert-deftest hermes-chat-reasoning-settlement-ignores-stale-session ()
  "A late reasoning setter cannot read into a replacement session."
  (let ((client (hermes-test--dashboard-client)) resolve-set config-gets)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq resolve-set (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-config-get)
               (lambda (&rest _args) (cl-incf config-gets))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--runtime-flags '(:reasoning-effort "low")
             config-gets 0)
       (insert "/reasoning high")
       (hermes-chat-send)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve-set '((value . "high")))
       (should (= config-gets 0))
       (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                      "low"))))))

(ert-deftest hermes-chat-reasoning-readback-ignores-stale-session ()
  "A late reasoning readback cannot overwrite a replacement session."
  (let ((client (hermes-test--dashboard-client)) resolve-get)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (funcall (plist-get args :resolve) '((value . "high")))))
              ((symbol-function 'hermes-dashboard-transport-config-get)
               (lambda (_client _key &rest args)
                 (setq resolve-get (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--runtime-flags '(:reasoning-effort "low"))
       (insert "/reasoning high")
       (hermes-chat-send)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve-get '((value . "high")))
       (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                      "low"))))))

(ert-deftest hermes-chat-reasoning-rejection-preserves-effective-state ()
  "A rejected reasoning setter preserves the previous effective effort."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (funcall (plist-get args :reject) "reasoning rejected")))
              ((symbol-function 'hermes-dashboard-transport-config-get)
               (lambda (&rest _args)
                 (ert-fail "Rejected setter must not read reasoning"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--runtime-flags '(:reasoning-effort "low"))
       (insert "/reasoning high")
       (hermes-chat-send)
       (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                      "low"))
       (should
        (cl-find-if (lambda (entry)
                      (and (eq (plist-get entry :role) 'status)
                           (eq (plist-get entry :status) 'error)
                           (equal (plist-get entry :content)
                                  "reasoning rejected")))
                    (hermes-chat--entries)))))))

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

(ert-deftest hermes-chat-dashboard-reset-ignores-stale-resume-result ()
  "A pre-reset session callback must not repopulate the cleared chat."
  (let ((client (hermes-test--dashboard-client)) resume-resolve submitted)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resume-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest _args)
                 (setq submitted text))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored")
         (insert "stale prompt")
         (hermes-chat-send)
         (should (functionp resume-resolve))
         (hermes-chat--reset-transcript)
         (funcall resume-resolve
                  '((session_id . "old-live")
                    (resumed . "sid-stored")
                    (running . :false)))
         (should-not submitted)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not hermes-chat--dashboard-running-p)
         (should-not (hermes-chat--entries)))))))

(ert-deftest hermes-chat-dashboard-submit-signal-clears-running-state ()
  "A synchronous prompt failure must not leave the session locally busy."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (user-error "submit failed"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "fail synchronously")
         (hermes-chat-send)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--dashboard-running-p)
         (should (string-match-p "submit failed" (buffer-string))))))))

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

(ert-deftest hermes-chat-dashboard-drops-late-settled-turn-events ()
  "Late fallback events must not appear after a newer turn's final reply."
  (let ((client (hermes-test--dashboard-client))
        callback interrupt-resolve)
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
               (lambda (&rest _args) 'prompt-request))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve)))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-interrupt)
         (funcall callback
                  '(:type done :session-id "sid-active" :status "interrupted"))
         (funcall interrupt-resolve '((status . "ok")))
         (hermes-chat--activate-backend-turn "second")
         (funcall callback
                  '(:type done :session-id "sid-active" :content "second reply"))
         (funcall callback
                  '(:type tool
                    :session-id "sid-active"
                    :tool-call-id "late-tool"
                    :name "terminal"
                    :status "completed"
                    :preview "late output"))
         (let ((entries (hermes-chat--entries)))
           (should (equal
                    (mapcar (lambda (entry) (plist-get entry :role)) entries)
                    '(user assistant status user assistant)))
           (should (equal
                    (plist-get (hermes-test--last-assistant-entry) :content)
                    "second reply"))))))))

(ert-deftest hermes-chat-dashboard-handles-close-after-settled-turn ()
  "A current transport close must clear session state after reply settlement."
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
                          '((session_id . "sid-active")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (funcall callback
                  '(:type done :session-id "sid-active" :content "reply"))
         (should (equal hermes-chat--dashboard-active-session-id "sid-active"))
         (funcall callback
                  '(:type status
                    :status "closed"
                    :content "Hermes dashboard WebSocket closed"))
         (should-not hermes-chat--dashboard-active-session-id)
         (should (equal
                  (hermes-chat--status-name
                   (plist-get hermes-chat--status-state :status))
                  "closed")))))))

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

(ert-deftest hermes-chat-explicit-queue-keeps-input-tail-during-stream ()
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
         (hermes-chat-queue-message)
         (should (equal (hermes-test--queued-contents) '("draft survives")))
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

(ert-deftest hermes-chat-idle-reconciliation-settles-missing-finished-session ()
  "A missing durable row cannot keep a locally finished chat busy forever."
  (let ((client (hermes-test--dashboard-client)) idle rescheduled)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args) (setq rescheduled t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-live"
             hermes-chat--session-id "sid-missing"
             hermes-chat--dashboard-running-p t)
       (setq rescheduled nil)
       (hermes-chat--dashboard-reconcile-idle
        (hermes-chat--dashboard-idle-context (lambda () (setq idle t))))
       (should idle)
       (should-not rescheduled)
       (should-not hermes-chat--dashboard-running-p)))))

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

(ert-deftest hermes-chat-queued-drain-resume-running-retains-head ()
  "A resume race restores the live turn without consuming queued input."
  (let ((client (hermes-test--dashboard-client)))
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _args) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _args) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "sid-stored")
                            (running . t)
                            (inflight . ((turn_id . "remote")))))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args)
                 (ert-fail "prompt.submit must wait for the live turn"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored")
         (hermes-chat--queue-content "queued")
         (hermes-chat--drain-queued-message)
         (should (equal (hermes-test--queued-contents) '("queued")))
         (should-not hermes-chat--queued-submit-id)
         (should hermes-chat--dashboard-running-p)
         (should hermes-chat--pending-assistant-id))))))

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
           (should (equal (plist-get assistant :content) "")))
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

(ert-deftest hermes-chat-header-shows-directory-status-model ()
  "The header renders directory, status, and model from chat state."
  (hermes-test-with-chat-buffer
   (setq default-directory "/tmp/emacs-hermes/"
         hermes-chat--working-directory "/tmp/emacs-hermes/"
         hermes-chat--profile "coder")
   (hermes-chat--run-turn-reducer nil
    '(:type status :event "session.info" :status "ready"
            :model "claude-opus-4-8" :agent-name "planner"))
   (cl-letf (((symbol-function 'window-total-width) (lambda (&rest _) 200)))
     (let ((header (hermes-test--header-line-string)))
       (should (string-prefix-p " emacs-hermes  | " header))
       (should-not (string-match-p "coder" header))
       (should-not (string-match-p "planner" header))
       (should (string-match-p "claude-opus-4-8" header))
       (should (string-match-p "Ready" header))
       (should-not (string-match-p "session " header))))))

(ert-deftest hermes-chat-header-omits-buffer-identity ()
  "The header omits instance and profile already present in the buffer name."
  (let ((hermes-instances '(("local" . "http://127.0.0.1:9119")
                            ("remote" . "https://hermes.example.test"))))
    (cl-letf (((symbol-function 'hermes-instance-resolve)
               (lambda () (cadr hermes-instances))))
      (hermes-test-with-chat-buffer
       (setq default-directory "/tmp/project/"
             hermes-chat--working-directory "/tmp/project/"
             hermes-chat--profile "coder")
       (let ((header (hermes-test--header-line-string)))
         (should (string-prefix-p " project  | " header))
         (should-not (string-match-p "remote" header))
         (should-not (string-match-p "coder" header)))))))

(ert-deftest hermes-chat-header-uses-directory-basename ()
  "The header directory segment handles Unix and Windows instance paths."
  (should (equal (hermes-chat--directory-basename
                  "/tmp/Projects/emacs-lisp/emacs-hermes/")
                 "emacs-hermes"))
  (should (equal (hermes-chat--directory-basename
                  "C:\\Users\\Thanos\\Projects\\hermes-el\\")
                 "hermes-el")))

(ert-deftest hermes-chat-header-separates-runtime-flags-from-model ()
  "Reasoning effort, fast tier, and yolo render as separate segments."
  (hermes-test-with-chat-buffer
   (hermes-chat--run-turn-reducer nil
    '(:type status :event "session.info" :status "ready"
            :model "gpt-5.5" :reasoning-effort "high" :fast t :yolo t))
   (should (equal (substring-no-properties (hermes-chat--header-model-segment))
                  "gpt-5.5"))
   (should (equal (mapcar #'substring-no-properties
                          (hermes-chat--header-runtime-segments))
                  '("high" "fast" "YOLO")))
   ;; A later session.info clearing fast/yolo updates the captured flags.
   (hermes-chat--run-turn-reducer nil
    '(:type status :event "session.info" :status "ready"
            :model "gpt-5.5" :fast nil :yolo nil))
   (should (equal (mapcar #'substring-no-properties
                          (hermes-chat--header-runtime-segments))
                  '("high")))))

(ert-deftest hermes-chat-header-model-segment-without-flags-is-bare ()
  "Without runtime flags the model segment is the bare model id."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--model "gpt-5.5")
   (should (equal (substring-no-properties (hermes-chat--header-model-segment))
                  "gpt-5.5"))
   (setq hermes-chat--model nil)
   (should-not (hermes-chat--header-model-segment))))

(ert-deftest hermes-chat-header-uses-compact-semantic-layout ()
  "The compact header orders directory, activity, runtime, and context metadata."
  (hermes-test-with-chat-buffer
   (setq default-directory "/tmp/emacs-hermes/"
         hermes-chat--working-directory "/tmp/emacs-hermes/"
         hermes-chat--profile "scout"
         hermes-chat--agent-name "default"
         hermes-chat--model "grok-4.5"
         hermes-chat--runtime-flags
         '(:reasoning-effort "medium" :fast t :yolo t)
         hermes-chat--context '(:used 24705 :max 500000 :percent 5)
         hermes-chat--status-state '(:status ready :activity "Ready"))
   (cl-letf (((symbol-function 'window-total-width) (lambda (&rest _) 200)))
     (should (equal (substring-no-properties (hermes-chat--header-line))
                    (concat " emacs-hermes  |  ✓ Ready  |  grok-4.5  |  medium"
                            "  |  fast  |  YOLO  |  25k/500k "))))))

(ert-deftest hermes-chat-header-segments-carry-semantic-faces ()
  "Directory, model, runtime flags, and context values use distinct faces."
  (hermes-test-with-chat-buffer
   (setq default-directory "/tmp/emacs-hermes/"
         hermes-chat--working-directory "/tmp/emacs-hermes/"
         hermes-chat--profile "scout"
         hermes-chat--model "grok-4.5"
         hermes-chat--runtime-flags
         '(:reasoning-effort "medium" :fast t :yolo t)
         hermes-chat--context '(:used 24705 :max 500000 :percent 5))
   (cl-letf (((symbol-function 'window-total-width) (lambda (&rest _) 200)))
     (let ((header (hermes-chat--header-line)))
       (dolist (case '(("emacs-hermes" . hermes-chat-header-directory)
                       ("grok-4.5" . hermes-chat-header-model)
                       ("medium" . hermes-chat-header-reasoning)
                       ("fast" . hermes-chat-header-tier)
                       ("YOLO" . hermes-chat-header-warning)
                       ("25k/500k" . hermes-chat-header-context)))
         (let ((position (string-match-p (regexp-quote (car case)) header)))
           (should position)
           (should (eq (get-text-property position 'face header)
                       (cdr case)))))))))

(ert-deftest hermes-chat-header-truncates-to-narrow-window ()
  "A narrow header fits its window and preserves the leading directory face."
  (hermes-test-with-chat-buffer
   (setq default-directory "/tmp/emacs-hermes/"
         hermes-chat--working-directory "/tmp/emacs-hermes/"
         hermes-chat--profile "scout"
         hermes-chat--model "grok-4.5")
   (cl-letf (((symbol-function 'window-total-width) (lambda (&rest _) 10)))
     (let* ((header (hermes-chat--header-line))
            (directory-position (string-match-p "emacs" header)))
       (should (<= (string-width header) 10))
       (should (string-suffix-p "…" header))
       (should directory-position)
       (should (eq (get-text-property directory-position 'face header)
                   'hermes-chat-header-directory))))))

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
  "Context usage renders only abbreviated used and limit tokens."
  (should (equal (hermes-chat--format-context '(:used 45000 :max 200000 :percent 22))
                  "45k/200k"))
  (should-not (hermes-chat--format-context '(:used 0 :max 0 :percent 0)))
  (should-not (hermes-chat--format-context nil)))

(ert-deftest hermes-chat-header-shows-context-window ()
  "The header surfaces context-window usage from `session.info'."
  (hermes-test-with-chat-buffer
   (hermes-chat--run-turn-reducer nil
    '(:type status :event "session.info" :status "ready"
            :model "gpt-5.5" :agent-name "planner"
            :context (:used 45000 :max 200000 :percent 22)))
   (should (string-match-p "45k/200k" (hermes-chat--header-line)))
   (should-not (string-match-p "ctx\\|%" (hermes-chat--header-line)))))

(ert-deftest hermes-chat-header-shows-only-running-goal ()
  "The compact goal counter is visible only while goal work is running."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--goal
         '(:status "active" :running t :turns-used 3 :max-turns 20))
   (should (string-match-p "Goal 3/20" (hermes-chat--header-line)))
   (setq hermes-chat--goal
         '(:status "paused" :running nil :turns-used 3 :max-turns 20))
   (should-not (string-match-p "Goal" (hermes-chat--header-line)))
   (setq hermes-chat--goal nil)
   (should-not (string-match-p "Goal" (hermes-chat--header-line)))))

(ert-deftest hermes-chat-done-event-records-usage ()
  "A done event records usage in header state; the compact header omits the gauge."
  (hermes-test-with-chat-buffer
   (hermes-chat--run-turn-reducer nil
    '(:type done :usage (:input 1200 :output 340)))
   (should (equal (plist-get hermes-chat--status-state :usage) '(:input 1200 :output 340)))
   (should-not (string-match-p "1200↑ 340↓ tok" (hermes-chat--header-line)))))

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

(ert-deftest hermes-chat-new-buffer-pins-instance ()
  "A new chat owns the resolved Hermes instance for its lifetime."
  (let* ((instance '("remote" . "https://hermes.example.test"))
         (buffer (hermes-chat--new-buffer "work" nil instance)))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal hermes-instance instance)))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-legacy-url-change-before-connect-is-honored ()
  "Unconfigured chats keep following the legacy dashboard URL until connect."
  (let ((hermes-instances nil)
        (hermes-dashboard-transport-url "http://127.0.0.1:9119")
        acquired-url buffer)
    (unwind-protect
        (progn
          (setq buffer (hermes-chat--new-buffer "work"))
          (setq hermes-dashboard-transport-url "https://hermes.example.test")
          (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                     (lambda (&rest _)
                       (setq acquired-url hermes-dashboard-transport-url)
                       (hermes-test--dashboard-client))))
            (with-current-buffer buffer
              (hermes-chat--dashboard-ensure-client))
            (should (equal acquired-url "https://hermes.example.test"))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-existing-dashboard-client-matches-instance ()
  "The profile picker reuses only a client for its selected instance."
  (let ((local '("local" . "http://127.0.0.1:9119"))
        (remote '("remote" . "https://hermes.example.test"))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        (local-client (hermes-test--dashboard-client))
        (remote-client (hermes-test--dashboard-client))
        buffers)
    (unwind-protect
        (progn
          (dolist (pair (list (cons local local-client)
                              (cons remote remote-client)))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (car pair)
                      hermes-chat--dashboard-client (cdr pair)))))
          (with-temp-buffer
            (setq hermes-instance local)
            (should (eq (hermes-chat--existing-dashboard-client)
                        local-client))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest hermes-chat-new-buffer-uses-project-identity ()
  "Fresh buffer names reflect instance, profile, and launching project."
  (let* ((default-directory "/tmp/emacs-hermes/")
         (buffer (hermes-chat--new-buffer nil nil)))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name) "*Hermes@default: [emacs-hermes]*")))
      (kill-buffer buffer)))
  (let* ((default-directory "/tmp/emacs-hermes/")
         (buffer (hermes-chat--new-buffer "work" "deploy")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (buffer-name)
                         "*Hermes@work: [emacs-hermes]*"))
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
  "A cold profile chooser falls back while its asynchronous warmup fails."
  (let (prompt messages)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (_client) (hermes--promise-rejected "404 not found")))
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
      (should (string-match-p "No dashboard profiles available"
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

(ert-deftest hermes-chat-profile-list-payload-serves-cache-and-revalidates ()
  "A warm profile cache is returned while an existing client refreshes it async."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (client (hermes-test--dashboard-client))
        refreshed)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (value)
                 (setq refreshed value)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (&rest _) (error "synchronous profile fetch"))))
      (let ((cached
             '((profiles . (((name . "default") (is_default . t))
                            ((name . "elisp-dev")))))))
        (hermes-dashboard-transport--store-profile-cache cached)
        (should (equal (hermes-chat--profile-list-payload) cached))
        (should (eq refreshed client))))))

(ert-deftest hermes-chat-profile-list-cache-miss-warms-asynchronously ()
  "A cold profile picker starts a warmup but never calls synchronous HTTP."
  (let ((hermes-dashboard-transport--profile-cache nil)
        (client (hermes-test--dashboard-client)) warmed)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () client))
              ((symbol-function 'hermes-dashboard-transport-profile-list-async)
               (lambda (value)
                 (setq warmed value)
                 (hermes--promise-resolved nil)))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (&rest _) (error "synchronous profile fetch")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "synchronous HTTP"))))
      (should-not (hermes-chat--profile-list-payload))
      (should (eq warmed client)))))

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
  "Interactively creating a chat chooses from the warmed profile cache."
  (let ((hermes-dashboard-transport--profile-cache nil) choices)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--api-base-url)
               (lambda () "http://dash.example"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (hermes-dashboard-transport--store-profile-cache
       '((profiles . (((name . "default") (is_default . t))
                      ((name . "elisp-dev")
                       (description . "Emacs Lisp work"))))))
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

(ert-deftest hermes-chat-usage-content-formats-counts-and-credits ()
  "Usage text carries the four counters and appends credit lines."
  (should (equal (hermes-chat--usage-content
                  '((calls . 3) (input . 100) (output . 50) (total . 150)
                    (credits_lines . ("Nous: 1.2 credits left"))))
                 "Usage: 3 calls — input 100, output 50, total 150 tokens\nNous: 1.2 credits left"))
  (should (equal (hermes-chat--usage-content '())
                 "Usage: 0 calls — input 0, output 0, total 0 tokens")))

(ert-deftest hermes-chat-show-usage-inserts-panel-for-session ()
  "The usage command fetches `session.usage' for the attached session."
  (let (seen-session)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-usage)
               (lambda (_client &rest args)
                 (setq seen-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve)
                          '((calls . 2) (input . 10) (output . 5) (total . 15))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client
             (hermes-test--dashboard-client)
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-usage")
       (cl-letf (((symbol-function 'hermes-chat--dashboard-control-client)
                  (lambda () hermes-chat--dashboard-client)))
         (hermes-chat-show-usage))
       (should (equal seen-session "sid-usage"))
       (should (string-match-p "Usage: 2 calls" (buffer-string)))))))

(ert-deftest hermes-chat-notification-clear-adds-no-transcript-entry ()
  "notification.clear retracts a keyed notice; it must not render an entry."
  (should-not (hermes-chat--transcript-event-p
               '(:type status :event "notification.clear"
                       :notification-key "credits")))
  (should (hermes-chat--transcript-event-p
           '(:type status :event "notification.show"
                   :content "[warning] credits low"))))

(ert-deftest hermes-chat-load-populates-registry-functions ()
  "Loading `hermes-chat' wires the buffer/dashboard registry variables."
  (should (eq hermes-chat--submit-function #'hermes-chat--submit-content))
  (should (eq hermes-chat--turn-event-function #'hermes-chat--run-turn-reducer))
  (should (memq #'hermes-chat--handoff-stop hermes-chat-cleanup-functions)))

(ert-deftest hermes-chat-input-history-restores-draft ()
  "Input history navigates newest-first and restores the unsent draft."
  (hermes-test-with-chat-buffer
   (should-error (hermes-chat-input-history-previous) :type 'user-error)
   (hermes-chat--record-input-history "first")
   (hermes-chat--record-input-history "second")
   (goto-char (point-max))
   (insert "draft")
   (hermes-chat-input-history-previous)
   (should (equal (hermes-chat-input-string) "second"))
   (hermes-chat-input-history-previous)
   (should (equal (hermes-chat-input-string) "first"))
   (hermes-chat-input-history-previous)
   (should (equal (hermes-chat-input-string) "first"))
   (hermes-chat-input-history-next)
   (should (equal (hermes-chat-input-string) "second"))
   (hermes-chat-input-history-next)
   (should (equal (hermes-chat-input-string) "draft"))
   (should-error (hermes-chat-input-history-next) :type 'user-error)))

(ert-deftest hermes-chat-input-history-records-only-successful-sends-per-buffer ()
  "Successful sends enter only their owning buffer's input history."
  (let (first-history)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function (lambda (&rest _) 'fake-process)))
       (insert "sent")
       (hermes-chat-send)
       (setq first-history hermes-chat--input-history)))
    (should (equal first-history '("sent")))
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function
            (lambda (&rest _) (error "Send rejected"))))
       (insert "not sent")
       (hermes-chat-send)
       (should-not hermes-chat--input-history)))))



(ert-deftest hermes-chat-queue-panel-renders-and-reorders-fifo ()
  "The side panel renders queue entries and can change their send order."
  (hermes-test-with-chat-buffer
   (hermes-chat--queue-content "first")
   (hermes-chat--queue-content "second")
   (hermes-chat--queue-content "third")
   (let ((owner (current-buffer))
         (third-id (plist-get (nth 2 hermes-chat--queued-messages) :id)))
     (with-temp-buffer
       (hermes-chat-queue-panel-mode)
       (setq hermes-chat-queue-panel--owner owner)
       (hermes-chat-queue-panel-refresh)
       (should (string-match-p "1. first" (buffer-string)))
       (should (string-match-p "2. second" (buffer-string)))
       (should (string-match-p "3. third" (buffer-string)))
       (hermes-chat--queue-panel-move-entry owner third-id -1)
       (should (equal (with-current-buffer owner
                        (hermes-test--queued-contents))
                      '("first" "third" "second")))
       (hermes-chat-queue-panel-refresh)
       (search-forward "third")
       (cl-letf (((symbol-function 'read-string-from-buffer)
                  (lambda (&rest _) "third edited")))
         (hermes-chat-queue-panel-edit))
       (goto-char (point-min))
       (search-forward "second")
       (hermes-chat-queue-panel-remove)
       (should (equal (with-current-buffer owner
                        (hermes-test--queued-contents))
                      '("first" "third edited")))))))

(ert-deftest hermes-chat-queue-panel-blocks-swap-with-inflight-head ()
  "Reorder refuses to displace the currently submitted queue head."
  (hermes-test-with-chat-buffer
   (hermes-chat--queue-content "first")
   (hermes-chat--queue-content "second")
   (let* ((head-id (plist-get (car hermes-chat--queued-messages) :id))
          (second-id (plist-get (nth 1 hermes-chat--queued-messages) :id)))
     (setq hermes-chat--queued-submit-id head-id)
     (should-error (hermes-chat--queue-panel-move-entry
                    (current-buffer) second-id -1)
                   :type 'user-error)
     (should (equal (hermes-test--queued-contents) '("first" "second")))
     (should (equal hermes-chat--queued-submit-id head-id)))))

(ert-deftest hermes-chat-file-ref-capf-inserts-project-relative-path ()
  "An @ prefix completes project files while retaining the reference marker."
  (hermes-test-with-chat-buffer
   (goto-char (point-max))
   (insert "See @lisp/her")
   (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'project))
             ((symbol-function 'project-root) (lambda (_) "/tmp/project/"))
             ((symbol-function 'project-files)
              (lambda (_) '("/tmp/project/lisp/hermes.el"))))
     (pcase-let ((`(,begin ,end ,candidates . ,_) (hermes-chat--file-ref-capf)))
       (should (member "lisp/hermes.el" candidates))
       (delete-region begin end)
       (insert "lisp/hermes.el")
       (should (equal (hermes-chat-input-string)
                      "See @lisp/hermes.el"))))))


(ert-deftest hermes-chat-extract-embedded-images-lifts-data-url ()
  "Embedded data:image URLs leave cleaned prose and an image list."
  (let* ((png (concat "data:image/png;base64," (make-string 80 ?A)))
         (result (hermes-chat--extract-embedded-images
                  (format "see this %s please" png))))
    (should (equal (car result) "see this  please"))
    (should (equal (cdr result) (list png)))))

(ert-deftest hermes-chat-extract-embedded-images-rejects-oversize-boundedly ()
  "Oversized image data is rejected with bounded validation work."
  (let* ((hermes-chat--max-embedded-image-base64 80)
         (text (concat "data:image/png;base64," (make-string 100000 ?A)))
         (real-bounded-run
          (symbol-function 'hermes-chat--bounded-valid-run-length))
         limits
         result)
    (cl-letf (((symbol-function 'hermes-chat--bounded-valid-run-length)
               (lambda (&rest args)
                 (push (nth 3 args) limits)
                 (apply real-bounded-run args))))
      (setq result (hermes-chat--extract-embedded-images text)))
    (should (equal (car result) text))
    (should-not (cdr result))
    (should (equal (nreverse limits)
                   (list hermes-chat--max-embedded-image-mime-length
                         hermes-chat--max-embedded-image-base64)))))

(ert-deftest hermes-chat-extract-embedded-images-ignores-short-payload ()
  "Short base64 payloads are left in prose."
  (let* ((short "data:image/png;base64,AAAA")
         (text (format "keep %s text" short))
         (result (hermes-chat--extract-embedded-images text)))
    (should (equal (car result) text))
    (should-not (cdr result))))

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

(provide 'hermes-chat-tests)
;;; hermes-chat-tests.el ends here
