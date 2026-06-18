;;; hermes-tests.el --- Tests for hermes-el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'timer)
(require 'auth-source)

(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

;; The project targets Emacs 29.1+ through keymap-popup.  CI/developer
;; machines should use that.  These tiny shims let the tests still exercise the
;; pure buffer logic on Debian's Emacs 28 when no newer Emacs is available.
(eval-and-compile
  (unless (fboundp 'keymap-set)
    (defun keymap-set (keymap key definition)
      (define-key keymap (kbd key) definition)))
  (unless (fboundp 'keymap-lookup)
    (defun keymap-lookup (keymap key &optional accept-default)
      (lookup-key keymap (kbd key) accept-default)))
  (unless (fboundp 'defvar-keymap)
    (defmacro defvar-keymap (name &rest args)
      (declare (indent 1))
      (let (doc parent bindings)
        (while (keywordp (car args))
          (pcase (pop args)
            (:doc (setq doc (pop args)))
            (:parent (setq parent (pop args)))
            (_ (pop args))))
        (while args
          (let ((key (pop args))
                (definition (pop args)))
            (push `(define-key map (kbd ,key) ,definition) bindings)))
        `(defvar ,name
           (let ((map (make-sparse-keymap)))
             ,@(when parent `((set-keymap-parent map ,parent)))
             ,@(nreverse bindings)
             map)
           ,doc)))))

(require 'keymap-popup)
(require 'hermes)
(require 'hermes-chat)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)

(defun hermes-test--chat-buffer-name ()
  "Return a fresh chat buffer name for tests."
  (generate-new-buffer-name "*Hermes Chat Test*"))

(defun hermes-test--dashboard-buffer-name ()
  "Return a fresh dashboard buffer name for tests."
  (generate-new-buffer-name "*Hermes Dashboard Test*"))

(defmacro hermes-test-with-chat-buffer (&rest body)
  "Create a fresh Hermes chat buffer and run BODY in it."
  (declare (indent 0) (debug t))
  `(let ((hermes-chat-buffer-name (hermes-test--chat-buffer-name)))
     (unwind-protect
         (progn
           (hermes-chat)
           (with-current-buffer hermes-chat-buffer-name
             ,@body))
       (when-let* ((buffer (get-buffer hermes-chat-buffer-name)))
         (kill-buffer buffer)))))

(defmacro hermes-test-with-dashboard-buffer (&rest body)
  "Create a fresh Hermes dashboard buffer and run BODY in it."
  (declare (indent 0) (debug t))
  `(let ((hermes-dashboard-buffer-name (hermes-test--dashboard-buffer-name)))
     (unwind-protect
         (with-current-buffer (get-buffer-create hermes-dashboard-buffer-name)
           (hermes-dashboard-mode)
           (hermes-dashboard--render)
           ,@body)
       (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
         (kill-buffer buffer)))))

(defun hermes-test--dashboard-node-data (id)
  "Return dashboard node data for ID in the current dashboard buffer."
  (when-let* ((node (gethash id hermes-dashboard--nodes)))
    (ewoc-data node)))

(defun hermes-test--dashboard-stale-refresh-timers (&optional buffer)
  "Return dashboard stale-refresh timers, optionally for BUFFER."
  (let (timers)
    (dolist (timer timer-list (nreverse timers))
      (when (and (timerp timer)
                 (eq (timer--function timer)
                     #'hermes-dashboard--stale-refresh)
                 (or (null buffer)
                     (equal (timer--args timer) (list buffer))))
        (push timer timers)))))

(defun hermes-test--face-includes-p (value face)
  "Return non-nil if text face VALUE includes FACE."
  (or (eq value face)
      (and (listp value) (memq face value))))

(defun hermes-test--face-at-end-of (needle)
  "Return the face on the final character of NEEDLE in the current buffer."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (1- (point)) 'face))

(defun hermes-test--should-have-face (needle face)
  "Assert that NEEDLE has FACE on its final character."
  (should (hermes-test--face-includes-p
           (hermes-test--face-at-end-of needle) face)))

(defun hermes-test--should-not-have-face (needle face)
  "Assert that NEEDLE does not have FACE on its final character."
  (should-not (hermes-test--face-includes-p
               (hermes-test--face-at-end-of needle) face)))

(defun hermes-test--push-button-labeled (label)
  "Activate the text button ending at LABEL in the current buffer."
  (goto-char (point-min))
  (search-forward label)
  (let ((button (button-at (1- (point)))))
    (should button)
    (button-activate button)))

(defun hermes-test--header-line-string ()
  "Return the current chat header line as plain text."
  (substring-no-properties (hermes-chat--header-line)))

(defun hermes-test--dashboard-client ()
  "Return a fake dashboard transport client for chat integration tests."
  (make-hermes-dashboard-transport-client
   :websocket 'fake-websocket
   :pending (make-hash-table :test #'equal)
   :callback #'ignore))

(defun hermes-test--control-content-preserved-p (&rest candidates)
  "Return non-nil when a busy-control CANDIDATE is still recoverable."
  (or (member hermes-chat--queued-message candidates)
      (member (hermes-chat-input-string) candidates)))

(defmacro hermes-test-with-dashboard-prompt-session (spec &rest body)
  "Create a chat using fake dashboard SPEC's client, then run BODY."
  (declare (indent 1) (debug t))
  (let ((client (car spec)))
    `(let ((,client (hermes-test--dashboard-client)))
     (cl-letf (((symbol-function 'hermes-transport-send)
                (lambda (&rest _args) (error "CLI fallback should not run")))
               ((symbol-function 'hermes-dashboard-transport-start)
                (lambda (&rest args)
                  (setf (hermes-dashboard-transport-client-callback ,client)
                        (plist-get args :callback))
                  ,client))
               ((symbol-function 'hermes-dashboard-transport-session-create)
                (lambda (_client &rest args)
                  (funcall (plist-get args :resolve)
                           '((session_id . "sid-prompt")
                             (stored_session_id . "sid-stored")))))
               ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                (lambda (&rest _args) 'prompt-submitted)))
       (let ((hermes-transport-send-function #'hermes-transport-send))
         (hermes-test-with-chat-buffer
          (insert "trigger prompt")
          (hermes-chat-send)
          ,@body))))))

(defun hermes-test--emit-dashboard-prompt (client type payload)
  "Emit dashboard prompt event TYPE with PAYLOAD through CLIENT."
  (hermes-dashboard-transport--handle-frame
   client
   (hermes-dashboard-transport--encode-frame
    `((jsonrpc . "2.0")
      (method . "event")
      (params . ((type . ,type)
                 (session_id . "sid-prompt")
                 (payload . ,payload)))))))

(ert-deftest hermes-dashboard-opens-special-mode-buffer-and-popup ()
  (let (shown-map)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (keymap) (setq shown-map keymap))))
      (unwind-protect
          (progn
            (hermes)
            (should (eq major-mode 'hermes-dashboard-mode))
            (should (eq shown-map hermes-dashboard-mode-map))
            (should hermes-dashboard--ewoc)
            (let ((text (buffer-string)))
              (should (string-match-p "Hermes" text))
              (should (string-match-p "Chat" text))
              (should (string-match-p "New session" text))))
        (when-let* ((buffer (get-buffer hermes-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-chat-action-is-keymap-popup-binding ()
  (should (eq (keymap-lookup hermes-dashboard-mode-map "c") #'hermes-chat))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "N") #'hermes-chat-new-session))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "g") #'hermes-dashboard-refresh))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "n") #'hermes-dashboard-next))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "p") #'hermes-dashboard-previous))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "RET") #'hermes-dashboard-open))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "i") #'hermes-dashboard-interrupt))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "s") #'hermes-dashboard-steer))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "a") #'hermes-dashboard-respond))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "?") #'hermes-dashboard-popup))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "h") #'hermes-dashboard-popup))
  (let* ((rows (keymap-popup--meta hermes-dashboard-mode-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows)))
    (should (cl-find "c" entries :key (lambda (entry) (plist-get entry :key))
                     :test #'equal))))

(ert-deftest hermes-dashboard-status-symbol-does-not-intern-unknown-strings ()
  (let* ((normalized "hermes-unknown-status-from-test")
         (status (replace-regexp-in-string "-" " " normalized)))
    (should-not (intern-soft normalized))
    (should-not (hermes-dashboard--status-symbol status))
    (should-not (intern-soft normalized))
    (should (eq (hermes-dashboard--status-symbol "input requested")
                'input-requested))
    (should (eq (hermes-dashboard--status-symbol "In_Progress")
                'in-progress))))

(ert-deftest hermes-dashboard-repeated-open-cleans-stale-refresh-timers ()
  (let ((hermes-dashboard-buffer-name (hermes-test--dashboard-buffer-name))
        (hermes-dashboard-stale-refresh-interval 3600)
        buffer)
    (cl-letf (((symbol-function 'keymap-popup)
               (lambda (&rest _args) nil)))
      (unwind-protect
          (progn
            (dotimes (_ 3)
              (hermes))
            (setq buffer (get-buffer hermes-dashboard-buffer-name))
            (should (buffer-live-p buffer))
            (should (= 1 (length (hermes-test--dashboard-stale-refresh-timers
                                  buffer))))
            (kill-buffer buffer)
            (should (= 0 (length (hermes-test--dashboard-stale-refresh-timers
                                  buffer)))))
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))))))

(ert-deftest hermes-dashboard-renders-ewoc-actions-and-empty-state ()
  (hermes-test-with-dashboard-buffer
   (should (eq major-mode 'hermes-dashboard-mode))
   (should hermes-dashboard--ewoc)
   (should (equal (hermes-dashboard--current-ids)
                  '("action:chat" "action:new-session" "empty:chats")))
   (let ((text (buffer-string)))
     (should (string-match-p "Chat" text))
     (should (string-match-p "New session" text))
     (should (string-match-p "No live Hermes chat buffers" text)))
   (should (eq (plist-get (hermes-test--dashboard-node-data "action:chat") :action)
               #'hermes-chat))
   (goto-char (point-min))
   (search-forward "Chat")
   (should (equal (get-text-property (point) 'hermes-dashboard-node-id)
                  "action:chat"))))

(ert-deftest hermes-dashboard-lists-open-chat-buffers-with-status ()
  (let (chat-buffer chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-buffer (current-buffer)
           chat-name (buffer-name))
     (setq hermes-chat--session-id "sid-dashboard-test")
     (puthash "tool-1" "terminal: make check" hermes-chat--active-tools)
     (puthash "prompt-1" '(:prompt-type "approval") hermes-chat--pending-prompts)
     (hermes-chat--set-header-state
      :status 'running :activity "terminal: make check")
     (hermes-test-with-dashboard-buffer
      (let ((id (format "chat:%s" chat-name))
            (text (buffer-string)))
        (should (member id (hermes-dashboard--current-ids)))
        (should (string-match-p (regexp-quote chat-name) text))
        (should (string-match-p "Running" text))
        (should (string-match-p "terminal: make check" text))
        (should (string-match-p "1 pending prompt" text))
        (should (string-match-p "session sid-dashboard-test" text))
        (should (eq (plist-get (hermes-test--dashboard-node-data id) :buffer)
                    chat-buffer)))))))

(ert-deftest hermes-dashboard-refresh-updates-chat-node ()
  (let (chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-name (buffer-name))
     (hermes-test-with-dashboard-buffer
      (should (string-match-p "Ready" (buffer-string)))
      (with-current-buffer chat-name
        (hermes-chat--set-header-state :status 'error :activity "boom"))
      (hermes-dashboard-refresh)
      (let ((text (buffer-string))
            (chat-id (format "chat:%s" chat-name)))
        (should (string-match-p "Error" text))
        (should (string-match-p "boom" text))
        (should (= 1 (cl-count chat-id (hermes-dashboard--current-ids)
                               :test #'equal))))))))

(ert-deftest hermes-dashboard-open-at-point-switches-to-chat-buffer ()
  (let (chat-buffer chat-name)
    (hermes-test-with-chat-buffer
     (setq chat-buffer (current-buffer)
           chat-name (buffer-name))
     (hermes-test-with-dashboard-buffer
      (search-forward chat-name)
      (hermes-dashboard-open)
      (should (eq (current-buffer) chat-buffer))
      (should (= (point) hermes-chat--input-marker))))))

(ert-deftest hermes-dashboard-selected-chat-actions-error-without-chat-node ()
  (hermes-test-with-dashboard-buffer
   (goto-char (point-min))
   (search-forward "Chat")
   (should-error (hermes-dashboard-interrupt) :type 'user-error)
   (should-error (hermes-dashboard-steer) :type 'user-error)
   (should-error (hermes-dashboard-respond) :type 'user-error)))

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
  (dolist (case '(("in_progress" "Running" "…" font-lock-keyword-face t nil)
                  ("busy" "Running" "…" font-lock-keyword-face t nil)
                  ("approval-requested" "Approval requested" "…"
                   font-lock-keyword-face t nil)
                  ("queued" "Queued" "…" font-lock-keyword-face t nil)
                  ("succeeded" "Ready" "✓" success nil t)
                  ("interrupted" "Interrupted" "!" error nil t)
                  ("cancelled" "Cancelled" "!" error nil t)
                  ("closed" "Disconnected" "!" warning nil t)))
    (pcase-let ((`(,status ,label ,icon ,face ,active ,finished) case))
      (should (equal (hermes-chat--header-status-label status) label))
      (should (equal (hermes-chat--status-icon status) icon))
      (should (eq (hermes-chat--header-status-face status) face))
      (should (eq (not (null (hermes-chat--active-status-p status))) active))
      (should (eq (not (null (hermes-chat--finished-status-p status)))
                  finished)))))

(ert-deftest hermes-dashboard-status-helpers-classify-parity-states ()
  (dolist (case '(("in_progress" "Running" hermes-dashboard-status-running)
                  ("busy" "Running" hermes-dashboard-status-running)
                  ("approval requested" "Approval requested"
                   hermes-dashboard-status-waiting)
                  ("input.requested" "Input requested"
                   hermes-dashboard-status-waiting)
                  ("succeeded" "Ready" hermes-dashboard-status-ready)
                  ("interrupted" "Interrupted" hermes-dashboard-status-error)
                  ("disconnected" "Disconnected"
                   hermes-dashboard-status-error)
                  ("backend paused" "Backend Paused" hermes-dashboard-muted)))
    (pcase-let ((`(,status ,label ,face) case))
      (should (equal (hermes-dashboard--status-label status) label))
      (should (eq (hermes-dashboard--status-face status) face)))))

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
       (let ((assistant (cadr (hermes-chat--entries))))
         (should (equal (plist-get assistant :status) 'streaming))
         (should (equal (plist-get assistant :content) "hello there")))
       (funcall callback '(:type done))
       (let ((assistant (cadr (hermes-chat--entries))))
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
			:duration 1.2))
       (let* ((entries (hermes-chat--entries))
              (roles (mapcar (lambda (entry) (plist-get entry :role)) entries))
              (text (buffer-string)))
         (should (equal roles '(user assistant status tool)))
         (should-not (string-match-p "running make test" text))
         (should (string-match-p "terminal completed (1.2s)" text))
         (should (equal (plist-get (nth 2 entries) :content) "Thinking…"))
         (should (equal (plist-get (nth 3 entries) :status) "completed")))))))

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
         (should (string-match-p "▸ Thinking\.\.\." text))
         (should-not (string-match-p "I need" text))
         (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                entries)
                        '(user assistant commentary)))
         (should (equal (plist-get (nth 2 entries) :content) "I need")))
       (hermes-test--should-have-face "Thinking..." 'shadow)
       (hermes-test--push-button-labeled "Thinking...")
       (let ((text (buffer-string)))
         (should (string-match-p "▾ Thinking\.\.\." text))
         (should (string-match-p "I need" text)))
       (hermes-test--should-have-face "I need" 'shadow)
       (hermes-test--push-button-labeled "Thinking...")
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
                        :event "thinking.delta"
                        :content chunk)))
       (hermes-test--push-button-labeled "Thinking...")
       (let ((text (buffer-string)))
         (should (string-match-p "I need to respond to \\\"hello\.\\\"" text))
         (should-not (string-match-p "I\n need\n to" text)))))))

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
         (should (string-match-p "read_file completed (0.4s)"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-header-shows-status-and-tool-activity ()
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
       (should (string-match-p "terminal: make test"
                               (hermes-test--header-line-string)))
       (funcall callback
                '(:type tool
			:tool-call-id "tool-1"
			:name "terminal"
			:status "completed"
			:duration 1.0))
       (should (string-match-p "terminal completed (1.0s)"
                               (hermes-test--header-line-string)))
       (funcall callback '(:type done))
       (let ((header (hermes-test--header-line-string)))
         (should (string-match-p "Ready" header))
         (should (string-match-p "last tool: terminal completed" header)))))))

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
       (let ((assistant (cadr (hermes-chat--entries))))
         (should (equal (plist-get assistant :status) 'streaming))
         (should (equal (plist-get assistant :content) "answer")))
       (funcall callback '(:type done))
       (let* ((entries (hermes-chat--entries))
              (progress (cl-find 'progress entries
                                 :key (lambda (entry)
                                        (plist-get entry :role)))))
         (should (equal (plist-get progress :status) 'done))
         (should (string-match-p "✓ read_file: read 40 lines"
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
         (should (string-match-p "! terminal: running" (buffer-string))))))))

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
       (let ((assistant (cadr (hermes-chat--entries))))
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
       (let ((assistant (cadr (hermes-chat--entries))))
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
       (let ((assistant (cadr (hermes-chat--entries))))
         (should (equal (plist-get assistant :content) ""))
         (should-not (string-match-p "session_id:" (buffer-string))))))))

(ert-deftest hermes-chat-fontifies-inline-diff-with-diff-mode-faces ()
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
   (hermes-test--should-have-face "-old-inline" 'diff-removed)
   (hermes-test--should-have-face "+new-inline" 'diff-added)
   (hermes-test--should-not-have-face "Done." 'diff-added)
   (hermes-test--should-not-have-face "Done." 'diff-removed)))

(ert-deftest hermes-chat-fontifies-inline-diff-without-final-newline ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     (concat "Changed:\n"
             "@@ -1 +1 @@\n"
             "-old-no-final-newline\n"
             "+new-no-final-newline")
     'done))
   (hermes-test--should-have-face "-old-no-final-newline" 'diff-removed)
   (hermes-test--should-have-face "+new-no-final-newline" 'diff-added)))

(ert-deftest hermes-chat-stops-inline-diff-at-hunk-counts ()
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
   (hermes-test--should-have-face "-old-counted" 'diff-removed)
   (hermes-test--should-have-face "+new-counted" 'diff-added)
   (hermes-test--should-not-have-face "+ ordinary follow-up" 'diff-added)))

(ert-deftest hermes-chat-fontifies-markdown-diff-and-patch-fences ()
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
   (hermes-test--should-have-face "-old-diff-fence" 'diff-removed)
   (hermes-test--should-have-face "+new-diff-fence" 'diff-added)
   (hermes-test--should-have-face "-old-patch-fence" 'diff-removed)
   (hermes-test--should-have-face "+new-patch-fence" 'diff-added)
   (hermes-test--should-not-have-face "after fences" 'diff-added)
   (hermes-test--should-not-have-face "after fences" 'diff-removed)))

(ert-deftest hermes-chat-does-not-fontify-ordinary-plus-minus-lines ()
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry
     'assistant
     "Ordinary chat:\n- remove clutter\n+ add clarity\nNo hunk header."
     'done))
   (hermes-test--should-not-have-face "- remove clutter" 'diff-removed)
   (hermes-test--should-not-have-face "+ add clarity" 'diff-added)))

(ert-deftest hermes-chat-fontifies-structured-diff-and-status-events ()
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
       (hermes-test--should-have-face "-diff-event-old" 'diff-removed)
       (hermes-test--should-have-face "+diff-event-new" 'diff-added)
       (hermes-test--should-have-face "-status-event-old" 'diff-removed)
       (hermes-test--should-have-face "+status-event-new" 'diff-added)))))

(ert-deftest hermes-chat-strips-ansi-before-diff-fontification ()
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
   (hermes-test--should-have-face "+ansi-added" 'diff-added)))

(ert-deftest hermes-chat-strips-split-ansi-before-diff-fontification ()
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
       (should-not (string-match-p "38;2" (buffer-string)))
       (should-not (string-match-p "\\[0m" (buffer-string)))
       (hermes-test--should-have-face "+split-ansi-added" 'diff-added)))))

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
                  :event "thinking.delta"
                  :content "Thinking"))
       (funcall callback
                '(:type delta
                  :content ";255;48;2;19;87;20m+interleaved-added\e[0m"))
       (let ((assistant (cadr (hermes-chat--entries)))
             (commentary (cl-find-if
                          (lambda (entry)
                            (eq (plist-get entry :role) 'commentary))
                          (hermes-chat--entries))))
         (should (string-match-p "+interleaved-added"
                                 (plist-get assistant :content)))
         (should-not (string-match-p "38;2" (plist-get assistant :content)))
         (should (equal (plist-get commentary :content) "Thinking")))
       (hermes-test--should-have-face "+interleaved-added" 'diff-added)))))

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
       (should (equal (plist-get (cadr (hermes-chat--entries)) :content)
                      "hello"))))))

(ert-deftest hermes-chat-rejects-concurrent-send-in-same-buffer ()
  (hermes-test-with-chat-buffer
   (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
     (insert "first")
     (hermes-chat-send)
     (insert "second")
     (should-error (hermes-chat-send) :type 'user-error))))

(ert-deftest hermes-chat-busy-offers-interrupt ()
  (hermes-test-with-chat-buffer
   (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
     (insert "first")
     (hermes-chat-send)
     (insert "second")
     (let ((message (condition-case error
                        (progn (hermes-chat-send) nil)
                      (user-error (error-message-string error)))))
       (should (string-match-p "A Hermes reply is still pending" message))
       (should (string-match-p "interrupt" message))
       (should (string-match-p "queue" message))
       (should (string-match-p "steer" message))
       (should (string-match-p "new session" message)))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-i")
                 #'hermes-chat-interrupt))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-q")
                 #'hermes-chat-queue-message))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-s")
                 #'hermes-chat-steer-message))
     (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-n")
                 #'hermes-chat-new-session)))))

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

(ert-deftest hermes-chat-new-session-while-pending ()
  (let (original new)
    (hermes-test-with-chat-buffer
     (let ((hermes-transport-send-function (lambda (_prompt _cb) 'fake-process)))
       (setq original (current-buffer))
       (insert "first")
       (hermes-chat-send)
       (setq new (hermes-chat-new-session))
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
       (insert "second")
       (should-error (hermes-chat-send) :type 'user-error)
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
         (should (string-match-p "Steer queued" (buffer-string))))))))

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
         (should (string-match-p "Steer unavailable" (buffer-string))))))))

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
         (should (string-match-p "use demo skill" text)))))))

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
       (funcall (hermes-dashboard-transport-client-callback client)
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
       (funcall (hermes-dashboard-transport-client-callback client)
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued prompt")))
       (funcall (hermes-dashboard-transport-client-callback client)
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
       (funcall (hermes-dashboard-transport-client-callback client)
                '(:type done :session-id "sid-live"))
       (should (equal submits '("queued via key")))
       (funcall (hermes-dashboard-transport-client-callback client)
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
       (should (string-match-p "Steer queued" (buffer-string)))))))

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
             (should (equal (hermes-dashboard-transport-client-session-id
                             client)
                            "sid-live"))
             (should (equal (hermes-dashboard-transport-client-stored-session-id
                             client)
                            "sid-stored"))
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
             (should (equal (hermes-dashboard-transport-client-session-id
                             client)
                            "sid-live"))
             (should (equal (hermes-dashboard-transport-client-stored-session-id
                             client)
                            "sid-stored"))
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
                (assistant (cadr entries))
                (status (nth 2 entries))
                (tool (nth 3 entries)))
           (should (equal roles '(user assistant status tool)))
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
           (should (string-match-p "▸ Thinking\.\.\." collapsed))
           (should-not (string-match-p "inspect repo" collapsed)))
         (hermes-test--push-button-labeled "Thinking...")
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Thinking\.\.\." expanded))
           (should (string-match-p "I need to inspect repo" expanded))
           (should-not (string-match-p "\\\\n\|\\^J" expanded)))
         (funcall callback
                  '(:type commentary
                    :session-id "sid-thinking"
                    :event "reasoning.delta"
                    :content " and cite files"))
         (let ((expanded (buffer-string)))
           (should (string-match-p "▾ Thinking\.\.\." expanded))
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
                (assistant (cadr entries))
                (commentary (nth 2 entries)))
           (should (equal roles '(user assistant commentary)))
           (should (= (cl-count 'commentary roles) 1))
           (should (equal (plist-get assistant :content) "Clean answer"))
           (should-not (string-match-p "inspect repo" (plist-get assistant :content)))
           (should (equal (plist-get commentary :content)
                          "I\\n need\\n to inspect^J repo and cite files"))))))))

(ert-deftest hermes-chat-preserves-input-tail-during-stream ()
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
         (let ((assistant (cadr (hermes-chat--entries))))
           (should (equal (plist-get assistant :content)
                          "answer continues"))
           (should (equal (plist-get assistant :status) 'streaming)))
         (should hermes-chat--pending-assistant-id)
         (should-error (hermes-chat-send) :type 'user-error))))))

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
         (let ((assistant (cadr (hermes-chat--entries))))
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
         (funcall second-callback
                  '(:type delta
                    :session-id "sid-live-1"
                    :content "old inflight"))
         (let* ((entries (hermes-chat--entries))
                (first-assistant (nth 1 entries))
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
         (should-error (hermes-chat-send) :type 'user-error))))))

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
         (funcall callback
                  '(:type delta
                    :session-id "sid-live"
                    :content "old inflight"))
         (let ((assistant (cadr (hermes-chat--entries))))
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
           (let ((assistant (cadr (hermes-chat--entries))))
             (should (equal (plist-get assistant :id) assistant-id))
             (should-not (string-match-p
                          "old inflight"
                          (plist-get assistant :content))))
           (funcall callback terminal)
           (let ((assistant (cadr (hermes-chat--entries))))
             (should-not (string-match-p
                          (regexp-quote (plist-get terminal :content))
                          (plist-get assistant :content))))
           (should-not hermes-chat--pending-assistant-id)))))))

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
    (funcall (hermes-dashboard-transport-client-callback client)
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

(ert-deftest hermes-transport-send-emits-start-status ()
  (let (events)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _plist) 'fake-process)))
      (should (eq (hermes-transport-send
                   "hello"
                   (lambda (event) (push event events)))
                  'fake-process)))
    (should (equal (nreverse events)
                   '((:type status
			    :event "run.started"
			    :status "running"
			    :content "Starting Hermes"))))))

(ert-deftest hermes-transport-builds-quiet-chat-command ()
  (let ((hermes-command "hermes"))
    (should (equal (hermes-transport--command "hello")
                   '("hermes" "chat" "-Q" "-q" "hello")))))

(ert-deftest hermes-transport-dashboard-builds-command ()
  (let* ((hermes-dashboard-transport-command "hermes")
         (env (hermes-dashboard-transport--environment
               "secret-token" '("PATH=/bin")))
         (start-event (hermes-dashboard-transport--start-event
                       "127.0.0.1" 4567 "secret-token")))
    (should (equal (hermes-dashboard-transport--command "127.0.0.1" 4567)
                   '("hermes" "dashboard" "--no-open" "--tui" "--isolated"
                     "--host" "127.0.0.1" "--port" "4567")))
    (should (member "PATH=/bin" env))
    (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token" env))
    (should (member "HERMES_DASHBOARD_TUI=1" env))
    (should-not (string-match-p "secret-token" (format "%S" start-event)))
    (should (equal (plist-get start-event :content)
                   "Starting Hermes dashboard on 127.0.0.1:4567"))))

(ert-deftest hermes-transport-dashboard-builds-websocket-url ()
  (should (equal (hermes-dashboard-transport--websocket-url
                  "127.0.0.1" 4567 "secret-token")
                 "ws://127.0.0.1:4567/api/ws?token=secret-token"))
  (should (equal (hermes-dashboard-transport--redacted-websocket-url
                  "127.0.0.1" 4567)
                 "ws://127.0.0.1:4567/api/ws?token=<redacted>")))

(ert-deftest hermes-transport-dashboard-builds-prefixed-remote-urls ()
  (should (equal (hermes-dashboard-transport--api-url
                  "https://dash.example/hermes/" "/api/status")
                 "https://dash.example/hermes/api/status"))
  (should (equal (hermes-dashboard-transport--websocket-url
                  "ignored" nil "ticket-secret"
                  "https://dash.example/hermes/" "ticket")
                 "wss://dash.example/hermes/api/ws?ticket=ticket-secret"))
  (should (equal (hermes-dashboard-transport--redacted-websocket-url
                  "ignored" nil "https://dash.example/hermes/" "ticket")
                 "wss://dash.example/hermes/api/ws?ticket=<redacted>")))

(ert-deftest hermes-transport-dashboard-rejects-remote-url-credentials ()
  (dolist (url '("https://user:password@dash.example/hermes"
                 "https://dash.example/hermes?token=secret-token"
                 "https://dash.example/hermes#secret-fragment"))
    (let ((message (condition-case error
                       (progn
                         (hermes-dashboard-transport--base-url
                          "ignored" nil url)
                         nil)
                     (user-error (error-message-string error)))))
      (should message)
      (should-not (string-match-p "secret-token" message))
      (should-not (string-match-p "secret-fragment" message)))))

(ert-deftest hermes-transport-dashboard-parses-set-cookie-headers ()
  (let ((buffer (generate-new-buffer " *hermes-test-http*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "HTTP/1.1 200 OK\r\n"
                  "Set-Cookie: access=access-cookie; Path=/; HttpOnly\r\n"
                  "Set-Cookie: refresh=refresh-cookie; Path=/; HttpOnly\r\n"
                  "Content-Type: application/json\r\n\r\n"
                  "{\"ok\": true}")
          (let ((response (hermes-dashboard-transport--parse-http-response-buffer
                           buffer)))
            (should (= (plist-get response :status) 200))
            (should (equal (hermes-dashboard-transport--response-cookie-header
                            response)
                           "access=access-cookie; refresh=refresh-cookie"))
            (should (string-match-p "\"ok\""
                                    (plist-get response :body-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-transport-dashboard-http-error-skips-json-body-parse ()
  (let (buffer message)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq buffer (generate-new-buffer " *hermes-test-http*"))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 401 Unauthorized\r\n\r\nnot json secret-token"))
                 buffer)))
      (setq message
            (condition-case error
                (progn
                  (hermes-dashboard-transport--default-http-request
                   "http://dash.example/api/status?token=secret-token"
                   :secrets '("secret-token"))
                  nil)
              (user-error (error-message-string error))))
      (should (string-match-p "HTTP 401" message))
      (should (string-match-p "token=<redacted>" message))
      (should-not (string-match-p "secret-token" message))
      (should-not (buffer-live-p buffer)))))

(ert-deftest hermes-transport-dashboard-start-auto-localhost-spawns ()
  (let (process-plist opened-url events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-url "http://127.0.0.1:9119")
            (hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (setq process-plist plist)
               'fake-process))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (setq opened-url url)
               'fake-websocket)))
        (let ((client (hermes-dashboard-transport-start
                       :callback (lambda (event) (push event events)))))
          (should (eq (hermes-dashboard-transport-client-process client)
                      'fake-process))
          (should (equal (plist-get process-plist :name) "hermes-dashboard"))
          (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token"
                          (plist-get process-plist :env)))
          (should (equal opened-url
                         "ws://127.0.0.1:9119/api/ws?token=secret-token"))
          (should (string-match-p "Starting Hermes dashboard"
                                  (format "%S" events)))
          (should-not (string-match-p "secret-token" (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-auto-remote-does-not-spawn ()
  (let (opened-url events)
    (let ((hermes-dashboard-transport-start-mode 'auto)
          (hermes-dashboard-transport-ready-timeout nil)
          (hermes-dashboard-transport-make-process-function
           (lambda (&rest _plist) (error "remote attach must not spawn")))
          (hermes-dashboard-transport-websocket-open-function
           (lambda (url _client)
             (setq opened-url url)
             'fake-websocket)))
      (let ((client (hermes-dashboard-transport-start
                     :host "100.64.0.10"
                     :port 9119
                     :token "remote-token"
                     :remote-auth-method 'token
                     :callback (lambda (event) (push event events)))))
        (should-not (hermes-dashboard-transport-client-process client))
        (should (equal opened-url
                       "ws://100.64.0.10:9119/api/ws?token=remote-token"))
        (should-not (string-match-p "remote-token" (format "%S" events)))
        (should (string-match-p "token=<redacted>" (format "%S" events)))))))

(ert-deftest hermes-transport-dashboard-token-auth-source-and-env-fallback ()
  (let (searches)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (push args searches)
                 (when (equal (plist-get args :host)
                              "http://100.64.0.10:9119")
                   (list (list :secret (lambda () "auth-token")))))))
      (should (equal (hermes-dashboard-transport--remote-token-secret
                      "http://100.64.0.10:9119")
                     "auth-token"))
      (should (plist-get (car searches) :user))
      (should (equal (plist-get (car searches) :port)
                     "hermes-dashboard-token"))))
  (let ((process-environment
         '("HERMES_DASHBOARD_SESSION_TOKEN=env-token")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should (equal (hermes-dashboard-transport--remote-token-secret
                      "http://100.64.0.10:9119")
                     "env-token"))))
  (let ((process-environment
         '("HERMES_DASHBOARD_SESSION_TOKEN=")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should-error (hermes-dashboard-transport--remote-token-secret
                     "http://100.64.0.10:9119" "")
                    :type 'user-error))))

(ert-deftest hermes-transport-dashboard-normalized-error-redacts-remote-secrets ()
  (let* ((client (make-hermes-dashboard-transport-client
                  :secrets '("cookie-secret" "ticket-secret")))
         (message (hermes-dashboard-transport--normalized-error-message
                   client "failed with cookie-secret and ticket-secret")))
    (should (string-match-p "<redacted>" message))
    (should-not (string-match-p "cookie-secret" message))
    (should-not (string-match-p "ticket-secret" message))))

(ert-deftest hermes-transport-dashboard-missing-token-error-actionable ()
  (let ((process-environment nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (let ((message (condition-case error
                         (progn
                           (hermes-dashboard-transport--remote-token-secret
                            "http://100.64.0.10:9119")
                           nil)
                       (user-error (error-message-string error)))))
        (should (string-match-p "hermes-dashboard-token" message))
        (should (string-match-p "HERMES_DASHBOARD_SESSION_TOKEN" message))))))

(ert-deftest hermes-transport-dashboard-basic-auth-uses-ticket-and-redacts ()
  (let ((password "basic-password-secret")
        (cookie-a "access=access-cookie-secret")
        (cookie-b "refresh=refresh-cookie-secret")
        (ticket "ticket-secret-abc")
        requests opened-url events)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (when (equal (plist-get args :port)
                              "hermes-dashboard-basic")
                   (list (list :user "admin"
                               :secret (lambda () password)))))))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-ready-timeout nil)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (setq opened-url url)
               'fake-websocket))
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) (error "remote attach must not spawn")))
            (hermes-dashboard-transport-http-request-function
             (lambda (url &rest args)
               (push (list :url url
                           :method (plist-get args :method)
                           :headers (plist-get args :headers)
                           :data (plist-get args :data))
                     requests)
               (cond
                ((string-suffix-p "/api/status" url)
                 '(:status 200 :headers nil
                   :body ((auth_required . t)
                          (auth_providers . ("basic")))))
                ((string-suffix-p "/auth/password-login" url)
                 `(:status 200
                   :headers (("set-cookie" . ,(concat cookie-a "; Path=/"))
                             ("set-cookie" . ,(concat cookie-b "; Path=/")))
                   :body ((ok . t))))
                ((string-suffix-p "/api/auth/ws-ticket" url)
                 `(:status 200 :headers nil
                   :body ((ticket . ,ticket) (ttl_seconds . 30))))))))
        (hermes-dashboard-transport-start
         :host "100.64.0.10"
         :port 9119
         :callback (lambda (event) (push event events)))
        (setq requests (nreverse requests))
        (should (equal opened-url
                       "ws://100.64.0.10:9119/api/ws?ticket=ticket-secret-abc"))
        (let* ((login (nth 1 requests))
               (ticket-request (nth 2 requests))
               (login-body (json-parse-string (plist-get login :data)
                                              :object-type 'alist)))
          (should (equal (hermes-transport--get login-body 'username) "admin"))
          (should (equal (hermes-transport--get login-body 'password) password))
          (should-not (string-match-p password (format "%S" ticket-request)))
          (should (equal (alist-get "Cookie" (plist-get ticket-request :headers)
                                    nil nil #'equal)
                         (concat cookie-a "; " cookie-b))))
        (let ((visible (format "%S" events)))
          (dolist (secret (list password cookie-a cookie-b ticket))
            (should-not (string-match-p (regexp-quote secret) visible)))
          (should (string-match-p "ticket=<redacted>" visible)))))))

(ert-deftest hermes-transport-dashboard-oauth-only-remote-is-unsupported ()
  (let (requests auth-source-called)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) (setq auth-source-called t) nil)))
      (let ((hermes-dashboard-transport-start-mode 'auto)
            (hermes-dashboard-transport-http-request-function
             (lambda (url &rest _args)
               (push url requests)
               '(:status 200 :headers nil
                 :body ((auth_required . t)
                        (auth_providers . ("oauth"))))))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (&rest _args) (error "must not open websocket"))))
        (let ((message (condition-case error
                           (progn
                             (hermes-dashboard-transport-start
                              :host "100.64.0.10" :port 9119)
                             nil)
                         (user-error (error-message-string error)))))
          (should (string-match-p "OAuth-only remote attach" message))
          (should-not (string-match-p "token=" message))
          (should-not auth-source-called)
          (should (equal (nreverse requests)
                         '("http://100.64.0.10:9119/api/status"))))))))

(ert-deftest hermes-transport-dashboard-redacts-websocket-process-name ()
  (let* ((token-url "ws://127.0.0.1:4567/api/ws?token=secret-token")
         (safe-url "ws://127.0.0.1:4567/api/ws?token=<redacted>")
         (token-name (format "websocket to %s" token-url))
         process-name websocket-url)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq process-name (plist-get plist :name))
                 'fake-process))
              ((symbol-function 'websocket-inner-create)
               (lambda (&rest plist)
                 (setq websocket-url (plist-get plist :url))
                 'fake-websocket)))
      (should (eq (hermes-dashboard-transport--call-with-redacted-websocket-state
                   token-url safe-url
                   (lambda ()
                     (let ((conn (make-network-process
                                  :name token-name
                                  :buffer nil
                                  :host "127.0.0.1"
                                  :service 4567)))
                       (websocket-inner-create :conn conn :url token-url))))
                  'fake-websocket)))
    (should (equal process-name (format "websocket to %s" safe-url)))
    (should (equal websocket-url safe-url))
    (should-not (string-match-p "secret-token" process-name))
    (should-not (string-match-p "secret-token" websocket-url))))

(ert-deftest hermes-transport-dashboard-redacts-ticket-websocket-name ()
  (let* ((ticket-url "wss://dash.example/hermes/api/ws?ticket=ticket-secret")
         (safe-url "wss://dash.example/hermes/api/ws?ticket=<redacted>")
         (ticket-name (format "websocket to %s" ticket-url))
         process-name websocket-url)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest plist)
                 (setq process-name (plist-get plist :name))
                 'fake-process))
              ((symbol-function 'websocket-inner-create)
               (lambda (&rest plist)
                 (setq websocket-url (plist-get plist :url))
                 'fake-websocket)))
      (should (eq (hermes-dashboard-transport--call-with-redacted-websocket-state
                   ticket-url safe-url
                   (lambda ()
                     (let ((conn (make-network-process
                                  :name ticket-name
                                  :buffer nil
                                  :host "dash.example"
                                  :service 443)))
                       (websocket-inner-create :conn conn :url ticket-url))))
                  'fake-websocket)))
    (should (equal process-name (format "websocket to %s" safe-url)))
    (should (equal websocket-url safe-url))
    (should-not (string-match-p "ticket-secret" process-name))
    (should-not (string-match-p "ticket-secret" websocket-url))))

(ert-deftest hermes-transport-dashboard-close-marks-client-not-live ()
  (let (on-close events rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-close (plist-get args :on-close))
                 'fake-websocket)))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :host "127.0.0.1"
                      :port 4567
                      :token "secret-token"
                      :websocket 'fake-websocket
                      :ready-p t
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "prompt.submit"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (should (eq (hermes-dashboard-transport--default-websocket-open
                     "ws://127.0.0.1:4567/api/ws?token=secret-token"
                     client)
                    'fake-websocket))
        (should (functionp on-close))
        (funcall on-close 'fake-websocket)
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "closed" rejected))
        (should (equal (plist-get (car events) :status) "closed"))))))

(ert-deftest hermes-transport-dashboard-error-marks-client-not-live ()
  (let (on-error events rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :host "127.0.0.1"
                      :port 4567
                      :token "secret-token"
                      :websocket 'fake-websocket
                      :ready-p t
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "prompt.submit"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (should (eq (hermes-dashboard-transport--default-websocket-open
                     "ws://127.0.0.1:4567/api/ws?token=secret-token"
                     client)
                    'fake-websocket))
        (should (functionp on-error))
        (funcall on-error 'fake-websocket 'error "socket died secret-token")
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "Hermes dashboard WebSocket error" rejected))
        (should (string-match-p "<redacted>" rejected))
        (should-not (string-match-p "secret-token" rejected))
        (should (equal (plist-get (car events) :type) 'error))
        (should (string-match-p "<redacted>"
                                (plist-get (car events) :content)))
        (should-not (string-match-p "secret-token"
                                    (plist-get (car events) :content)))))))

(ert-deftest hermes-transport-dashboard-close-rejects-pending-requests ()
  (let (on-close rejects events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-close (plist-get args :on-close))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-command-dispatch
           client "queue" "next"
           :reject (lambda (message)
                     (push (cons 'control message) rejects)))
          (hermes-dashboard-transport-approval-respond
           client :choice "deny"
           :reject (lambda (message)
                     (push (cons 'prompt message) rejects))))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   2))
        (funcall on-close 'fake-websocket)
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (dolist (kind '(control prompt))
          (should (string-match-p "WebSocket closed"
                                  (alist-get kind rejects))))
        (should (equal (plist-get (car events) :status) "closed"))))))

(ert-deftest hermes-transport-dashboard-request-timeout-rejects-pending ()
  "An unanswered request is rejected once its timeout timer fires."
  (let ((client (make-hermes-dashboard-transport-client
                 :token "secret-token"
                 :websocket 'fake-websocket
                 :ready-p t
                 :pending (make-hash-table :test #'equal)))
        (hermes-dashboard-transport-websocket-send-function #'ignore)
        (hermes-dashboard-transport-request-timeout 30)
        timer-callback rejected)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (setq timer-callback (cons fn args))
                 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (hermes-dashboard-transport-request
       client "session.create" nil nil
       (lambda (message) (setq rejected message)))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 1))
      (apply (car timer-callback) (cdr timer-callback))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (should (string-match-p "timed out" rejected))
      (should (string-match-p "session.create" rejected)))))

(ert-deftest hermes-transport-dashboard-error-rejects-pending-requests ()
  (let (on-error rejected events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-session-interrupt
           client
           :reject (lambda (message) (setq rejected message))))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   1))
        (funcall on-error 'fake-websocket 'error "socket died")
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "socket died" rejected))
        (should (= (cl-count 'error events
                             :key (lambda (event) (plist-get event :type)))
                   1))))))

(ert-deftest hermes-transport-dashboard-error-with-unhandled-pending-emits-once ()
  (let (on-error events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--require-websocket)
               #'ignore)
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error))
                 'fake-websocket)))
      (let ((client (make-hermes-dashboard-transport-client
                     :host "127.0.0.1"
                     :port 4567
                     :token "secret-token"
                     :websocket 'fake-websocket
                     :ready-p t
                     :pending (make-hash-table :test #'equal)
                     :callback (lambda (event) (push event events)))))
        (hermes-dashboard-transport--default-websocket-open
         "ws://127.0.0.1:4567/api/ws?token=secret-token" client)
        (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
          (hermes-dashboard-transport-prompt-submit client "hello"))
        (funcall on-error 'fake-websocket 'error "socket died")
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (= (cl-count 'error events
                             :key (lambda (event) (plist-get event :type)))
                   1))
        (let ((event (car events)))
          (should (equal (plist-get event :method) "prompt.submit"))
          (should (string-match-p "socket died"
                                  (plist-get event :content))))))))

(ert-deftest hermes-transport-dashboard-resume-stores-durable-session-id ()
  (let ((client (make-hermes-dashboard-transport-client
                 :websocket 'fake-websocket
                 :pending (make-hash-table :test #'equal)
                 :callback #'ignore))
        (hermes-dashboard-transport-websocket-send-function #'ignore))
    (hermes-dashboard-transport-request client "session.resume" nil)
    (hermes-dashboard-transport--handle-frame
     client (hermes-dashboard-transport--encode-frame
             '((jsonrpc . "2.0")
               (id . "hermes-el-1")
               (result . ((session_id . "sid-live")
                          (resumed . "sid-stored"))))))
    (should (equal (hermes-dashboard-transport-client-session-id client)
                   "sid-live"))
    (should (equal (hermes-dashboard-transport-client-stored-session-id client)
                   "sid-stored"))))

(ert-deftest hermes-transport-dashboard-connect-error-redacts-token ()
  (let (events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 1)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (error "connect failed: %s" url))))
        (let ((message (condition-case error
                           (progn
                             (hermes-dashboard-transport-start
                              :callback (lambda (event) (push event events)))
                             nil)
                         (user-error (error-message-string error)))))
          (should message)
          (should (string-match-p "<redacted>" message))
          (should-not (string-match-p "secret-token" message))
          (should-not (string-match-p "secret-token" (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-user-error-redacts-token ()
  (let ((open-attempts 0)
        (token "leaky-dashboard-token-abc123")
        events
        sleeps)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () token))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 3)
            (hermes-dashboard-transport-sleep-function
             (lambda (seconds) (push seconds sleeps)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url _client)
               (cl-incf open-attempts)
               (user-error "bad websocket url %s" url))))
        (let ((message (condition-case error
                           (progn
                             (hermes-dashboard-transport-start
                              :callback (lambda (event) (push event events)))
                             nil)
                         (user-error (error-message-string error)))))
          (should message)
          (should (= open-attempts 1))
          (should-not sleeps)
          (should (string-match-p "<redacted>" message))
          (should-not (string-match-p token message))
          (should-not (string-match-p token (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-start-process-error-redacts-token ()
  (let ((token "leaky-dashboard-token-abc123")
        events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () token))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (error "spawn failed with env %S" (plist-get plist :env)))))
        (let ((message (condition-case error
                           (progn
                             (hermes-dashboard-transport-start
                              :callback (lambda (event) (push event events)))
                             nil)
                         (user-error (error-message-string error))
                         (error (error-message-string error)))))
          (should message)
          (should (string-match-p "<redacted>" message))
          (should-not (string-match-p token message))
          (should-not (string-match-p token (format "%S" events))))))))

(ert-deftest hermes-transport-dashboard-start-cleans-process-on-connect-failure ()
  (let (deleted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 1)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client) (error "dashboard not ready"))))
        (should-error (hermes-dashboard-transport-start) :type 'user-error)
        (should (eq deleted 'fake-process))))))

(ert-deftest hermes-transport-dashboard-does-not-retry-user-errors ()
  (let ((open-attempts 0)
        sleeps)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-connect-retries 3)
            (hermes-dashboard-transport-sleep-function
             (lambda (seconds) (push seconds sleeps)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client)
               (cl-incf open-attempts)
               (user-error "Install websocket.el"))))
        (should-error (hermes-dashboard-transport-start) :type 'user-error)
        (should (= open-attempts 1))
        (should-not sleeps)))))

(ert-deftest hermes-transport-dashboard-readiness-window-is-practical ()
  (should (>= (* (1- hermes-dashboard-transport-connect-retries)
                 hermes-dashboard-transport-connect-retry-delay)
              45)))

(ert-deftest hermes-transport-dashboard-start-waits-for-gateway-ready ()
  (let (waits events)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-ready-timeout 1)
            (hermes-dashboard-transport-ready-wait-interval 0.01)
            (hermes-dashboard-transport-ready-wait-function
             (lambda (client seconds)
               (push seconds waits)
               (hermes-dashboard-transport--handle-frame
                client (hermes-dashboard-transport--encode-frame
                        '((jsonrpc . "2.0")
                          (method . "event")
                          (params . ((type . "gateway.ready"))))))))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url _client) 'fake-websocket)))
        (let ((client (hermes-dashboard-transport-start
                      :callback (lambda (event) (push event events)))))
          (should (hermes-dashboard-transport-client-ready-p client))
          (should (equal waits '(0.01)))
          (should (cl-find "gateway.ready" events
                           :key (lambda (event) (plist-get event :event))
                           :test #'equal)))))))

(ert-deftest hermes-transport-dashboard-start-timeout-cleans-websocket ()
  (let (closed deleted opened-client)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567))
              ((symbol-function 'websocket-close)
               (lambda (websocket) (setq closed websocket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let ((hermes-dashboard-transport-make-process-function
             (lambda (&rest _plist) 'fake-process))
            (hermes-dashboard-transport-ready-timeout 0)
            (hermes-dashboard-transport-websocket-open-function
             (lambda (_url client)
               (setq opened-client client)
               'fake-websocket)))
        (should-error (hermes-dashboard-transport-start) :type 'user-error)
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should opened-client)
        (should-not (hermes-dashboard-transport-client-websocket opened-client))
        (should-not (hermes-dashboard-transport-client-ready-p opened-client))
        (should-not (hermes-dashboard-transport-client-process opened-client))))))

(ert-deftest hermes-transport-dashboard-stop-releases-resources-and-rejects-pending ()
  (let (closed deleted rejected events)
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (websocket) (setq closed websocket)))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (let* ((pending (make-hash-table :test #'equal))
             (client (make-hermes-dashboard-transport-client
                      :process 'fake-process
                      :websocket 'fake-websocket
                      :ready-p t
                      :token "secret-token"
                      :pending pending
                      :callback (lambda (event) (push event events)))))
        (puthash "req-1"
                 (list :method "session.create"
                       :reject (lambda (message) (setq rejected message)))
                 pending)
        (hermes-dashboard-transport-stop client "stopped secret-token")
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-process client))
        (should-not (hermes-dashboard-transport-client-ready-p client))
        (should (= (hash-table-count
                    (hermes-dashboard-transport-client-pending client))
                   0))
        (should (string-match-p "stopped" rejected))
        (should (string-match-p "<redacted>" rejected))
        (should-not (string-match-p "secret-token" rejected))
        (should-not events)))))

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

(ert-deftest hermes-transport-dashboard-jsonrpc-correlates-responses ()
  (let* ((sent nil)
         (first-result nil)
         (second-result nil)
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :pending (make-hash-table :test #'equal)
                  :callback #'ignore))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_websocket text)
            (push (hermes-dashboard-transport--decode-frame text) sent))))
    (hermes-dashboard-transport-request
     client "session.create" '((cols . 80))
     (lambda (result) (setq first-result result)))
    (hermes-dashboard-transport-request
     client "prompt.submit" '((session_id . "sid") (text . "hello"))
     (lambda (result) (setq second-result result)))
    (let ((ids (mapcar (lambda (frame) (alist-get 'id frame)) sent)))
      (should (equal ids '("hermes-el-2" "hermes-el-1")))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 2)))
    (hermes-dashboard-transport--handle-frame
     client "{\"jsonrpc\":\"2.0\",\"id\":\"hermes-el-2\",\"result\":{\"ok\":true}}")
    (hermes-dashboard-transport--handle-frame
     client "{\"jsonrpc\":\"2.0\",\"id\":\"hermes-el-1\",\"result\":{\"session_id\":\"sid\"}}")
    (should (equal second-result '((ok . t))))
    (should (equal first-result '((session_id . "sid"))))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               0))))

(ert-deftest hermes-transport-dashboard-jsonrpc-error-rejects-pending-request ()
  (let* ((sent nil)
         rejected
         (client (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :pending (make-hash-table :test #'equal)
                  :callback #'ignore))
         (hermes-dashboard-transport-websocket-send-function
          (lambda (_websocket text)
            (push (hermes-dashboard-transport--decode-frame text) sent))))
    (hermes-dashboard-transport-command-dispatch
     client "nope" "arg"
     :reject (lambda (message) (setq rejected message)))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               1))
    (hermes-dashboard-transport--handle-frame
     client (hermes-dashboard-transport--encode-frame
             '((jsonrpc . "2.0")
               (id . "hermes-el-1")
               (error . ((code . -32601)
                         (message . "unknown command"))))))
    (should (equal rejected "unknown command"))
    (should (= (hash-table-count
                (hermes-dashboard-transport-client-pending client))
               0))))

(ert-deftest hermes-transport-dashboard-jsonrpc-send-failure-clears-pending ()
  (let (rejected events)
    (let* ((client (make-hermes-dashboard-transport-client
                    :websocket 'fake-websocket
                    :pending (make-hash-table :test #'equal)
                    :token "secret-token"
                    :callback (lambda (event) (push event events))))
           (hermes-dashboard-transport-websocket-send-function
            (lambda (_websocket _text)
              (error "send failed for secret-token"))))
      (hermes-dashboard-transport-request
       client "session.create" nil nil
       (lambda (message) (setq rejected message)))
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (should (string-match-p "send failed" rejected))
      (should (string-match-p "<redacted>" rejected))
      (should-not (string-match-p "secret-token" rejected))
      (should-not events)
      (hermes-dashboard-transport-request client "prompt.submit" nil)
      (should (= (hash-table-count
                  (hermes-dashboard-transport-client-pending client))
                 0))
      (let ((event (car events)))
        (should (eq (plist-get event :type) 'error))
        (should (equal (plist-get event :method) "prompt.submit"))
        (should (string-match-p "send failed" (plist-get event :content)))
        (should (string-match-p "<redacted>" (plist-get event :content)))
        (should-not (string-match-p "secret-token"
                                    (plist-get event :content)))))))

(ert-deftest hermes-transport-dashboard-connects-with-fakes ()
  (let (process-plist opened-url sent events sleeps
                      (open-attempts 0))
    (cl-letf (((symbol-function 'hermes-dashboard-transport--generate-token)
               (lambda () "secret-token"))
              ((symbol-function 'hermes-dashboard-transport--pick-port)
               (lambda () 4567)))
      (let ((hermes-dashboard-transport-command "hermes")
            (hermes-dashboard-transport-url "http://127.0.0.1:4567")
            (hermes-dashboard-transport-make-process-function
             (lambda (&rest plist)
               (setq process-plist plist)
               'fake-process))
            (hermes-dashboard-transport-connect-retries 2)
            (hermes-dashboard-transport-connect-retry-delay 0.05)
            (hermes-dashboard-transport-sleep-function
             (lambda (seconds) (push seconds sleeps)))
            (hermes-dashboard-transport-websocket-open-function
             (lambda (url client)
               (cl-incf open-attempts)
               (setq opened-url url)
               (should (hermes-dashboard-transport-client-p client))
               (if (= open-attempts 1)
                   (error "dashboard not ready")
                 (hermes-dashboard-transport--handle-frame
                  client (hermes-dashboard-transport--encode-frame
                          '((jsonrpc . "2.0")
                            (method . "event")
                            (params . ((type . "gateway.ready")
                                       (session_id . "sid"))))))
                 'fake-websocket)))
            (hermes-dashboard-transport-websocket-send-function
             (lambda (_websocket text)
               (push (hermes-dashboard-transport--decode-frame text) sent))))
        (let ((client (hermes-dashboard-transport-start
                       :callback (lambda (event) (push event events)))))
          (should (eq (hermes-dashboard-transport-client-process client)
                      'fake-process))
          (should (eq (hermes-dashboard-transport-client-websocket client)
                      'fake-websocket))
          (should (= open-attempts 2))
          (should (equal sleeps '(0.05)))
          (should (equal (plist-get process-plist :command)
                         '("hermes" "dashboard" "--no-open" "--tui" "--isolated"
                           "--host" "127.0.0.1" "--port" "4567")))
          (should (member "HERMES_DASHBOARD_SESSION_TOKEN=secret-token"
                          (plist-get process-plist :env)))
          (should (equal opened-url
                         "ws://127.0.0.1:4567/api/ws?token=secret-token"))
          (should-not (string-match-p "secret-token" (format "%S" events)))
          (should (hermes-dashboard-transport-client-ready-p client))
          (hermes-dashboard-transport-session-create client :cols 90 :title "Chat")
          (hermes-dashboard-transport-session-resume client "sid" :cols 90)
          (hermes-dashboard-transport-prompt-submit client "hello")
          (hermes-dashboard-transport-session-interrupt client)
          (hermes-dashboard-transport-session-steer client "cite files")
          (hermes-dashboard-transport-commands-catalog client)
          (hermes-dashboard-transport-command-dispatch client "queue" "next")
          (hermes-dashboard-transport-slash-exec client "queue next")
          (hermes-dashboard-transport-approval-respond client :choice "approve")
          (hermes-dashboard-transport-clarify-respond client "req-1" "answer")
          (hermes-dashboard-transport-sudo-respond client "req-2" "password")
          (hermes-dashboard-transport-secret-respond client "req-3" "value")
          (should (equal (mapcar (lambda (frame) (alist-get 'method frame))
                                 (nreverse sent))
                         '("session.create" "session.resume" "prompt.submit"
                           "session.interrupt" "session.steer"
                           "commands.catalog" "command.dispatch" "slash.exec"
                           "approval.respond" "clarify.respond" "sudo.respond"
                           "secret.respond")))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   '((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . "message.delta")
                                (session_id . "sid")
                                (payload . ((text . "hi"))))))))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   '((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . "message.complete")
                                (session_id . "sid")
                                (payload . ((text . "done"))))))))
          (should (equal (mapcar (lambda (event) (plist-get event :type))
                                 (nreverse events))
                         '(status status delta done))))))))

(ert-deftest hermes-transport-dashboard-complete-status-is-preserved ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.complete")
                            (session_id . "sid")
                            (payload . ((text . "Stopped")
                                        (status . "interrupted")))))))))
    (let ((event (car events)))
      (should (equal (plist-get event :type) 'error))
      (should (equal (plist-get event :event) "message.complete"))
      (should (equal (plist-get event :status) "interrupted"))
      (should (equal (plist-get event :content) "Stopped")))))

(ert-deftest hermes-transport-dashboard-complete-status-done-is-terminal ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.complete")
                            (session_id . "sid")
                            (payload . ((text . "Done")
                                        (status . "done")))))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'done))
      (should (equal (plist-get event :event) "message.complete"))
      (should (equal (plist-get event :status) "done"))
      (should (equal (plist-get event :content) "Done")))))

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
         (let ((assistant (cadr (hermes-chat--entries))))
           (should (equal (plist-get assistant :status) "interrupted"))
           (should (equal (plist-get assistant :content) "Stopped")))
         (should-not hermes-chat--pending-assistant-id)
         (should (string-match-p "Interrupted"
                                 (hermes-test--header-line-string)))
         (should-not (string-match-p "Error"
                                     (hermes-test--header-line-string))))))))

(ert-deftest hermes-transport-dashboard-normalizes-session-info ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "session.info")
                            (session_id . "sid")
                            (payload . ((model . "gpt-5.5")
                                        (provider . "openai-codex")))))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :event) "session.info"))
      (should (equal (plist-get event :session-id) "sid"))
      (should (equal (plist-get event :status) "ready"))
      (should (equal (plist-get event :content)
                     "Session ready: gpt-5.5 via openai-codex")))))

(ert-deftest hermes-transport-dashboard-normalizes-reasoning-events ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (dolist (type '("reasoning.delta" "thinking.delta"))
        (hermes-dashboard-transport--handle-frame
         client (hermes-dashboard-transport--encode-frame
                 `((jsonrpc . "2.0")
                   (method . "event")
                   (params . ((type . ,type)
                              (session_id . "sid")
                              (payload . ((text . "inspect first"))))))))))
    (let ((events (nreverse events)))
      (should (equal (mapcar (lambda (event) (plist-get event :type)) events)
                     '(commentary commentary)))
      (should (equal (mapcar (lambda (event) (plist-get event :event)) events)
                     '("reasoning.delta" "thinking.delta")))
      (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                             events)
                     '("sid" "sid")))
      (should (equal (mapcar (lambda (event) (plist-get event :content))
                             events)
                     '("inspect first" "inspect first"))))))

(ert-deftest hermes-transport-dashboard-normalizes-subagent-events ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (dolist (spec '(("subagent.thinking"
                       ((subagent_id . "sa-1")
                        (text . "(°ロ°) brainstorming...")))
                      ("subagent.tool"
                       ((subagent_id . "sa-1")
                        (tool_name . "terminal")
                        (tool_preview . "git status")
                        (text . "git status")))
                      ("subagent.progress"
                       ((subagent_id . "sa-1")
                        (text . "🔀 terminal, read_file")))
                      ("subagent.complete"
                       ((subagent_id . "sa-1")
                        (status . "completed")
                        (summary . "no merge recommended")))))
        (pcase-let ((`(,type ,payload) spec))
          (hermes-dashboard-transport--handle-frame
           client (hermes-dashboard-transport--encode-frame
                   `((jsonrpc . "2.0")
                     (method . "event")
                     (params . ((type . ,type)
                                (session_id . "sid")
                                (payload . ,payload)))))))))
    (pcase-let ((`(,thinking ,tool ,progress ,complete) (nreverse events)))
      (should (eq (plist-get thinking :type) 'commentary))
      (should (equal (plist-get thinking :event) "subagent.thinking"))
      (should (equal (plist-get thinking :subagent-id) "sa-1"))
      (should (equal (plist-get thinking :content)
                     "(°ロ°) brainstorming..."))
      (should (eq (plist-get tool :type) 'tool))
      (should (equal (plist-get tool :event) "subagent.tool"))
      (should (equal (plist-get tool :name) "terminal"))
      (should (equal (plist-get tool :status) "running"))
      (should (equal (plist-get tool :preview) "git status"))
      (should (equal (plist-get tool :subagent-id) "sa-1"))
      (should (eq (plist-get progress :type) 'progress))
      (should (equal (plist-get progress :content)
                     "🔀 terminal, read_file"))
      (should (equal (plist-get progress :subagent-id) "sa-1"))
      (should (eq (plist-get complete :type) 'status))
      (should (equal (plist-get complete :status) "completed"))
      (should (equal (plist-get complete :content)
                     "no merge recommended")))))

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
                          '(user assistant commentary tool)))
           (should (equal (plist-get (nth 2 entries) :content)
                          "(⌐■_■) synthesizing..."))
           (should (equal (plist-get (nth 3 entries) :content)
                          "terminal: git status")))
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
                          '(user assistant status)))
           (should (string-match-p "Unknown Hermes transport event: alien.signal"
                                   (plist-get (nth 2 entries) :content)))
           (should (eq (plist-get (nth 2 entries) :status) 'error))
           (should (string-match-p "Error" header))
           (should (string-match-p "alien.signal" header)))
         (should (cl-some (lambda (line)
                            (string-match-p "Unknown Hermes transport event: alien.signal"
                                            line))
                          messages)))))))

(ert-deftest hermes-transport-dashboard-normalizes-tool-payloads-and-inline-diff ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                  :callback (lambda (event) (push event events))))
          (inline-diff "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1 @@\n-old\n+new\n"))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "tool.start")
                            (session_id . "sid")
                            (payload . ((tool_id . "tool-1")
                                        (name . "terminal")
                                        (context . "running make test")
                                        (args_text . "make test"))))))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               `((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "tool.complete")
                            (session_id . "sid")
                            (payload . ((tool_id . "tool-1")
                                        (name . "terminal")
                                        (summary . "updated file")
                                        (result_text . "ok")
                                        (inline_diff . ,inline-diff)
                                        (duration_s . 1.2))))))))
      (pcase-let ((`(,start ,complete ,diff) (nreverse events)))
        (should (eq (plist-get start :type) 'tool))
        (should (equal (plist-get start :preview) "running make test"))
        (should (equal (plist-get start :args) "make test"))
        (should (eq (plist-get complete :type) 'tool))
        (should (equal (plist-get complete :preview) "updated file"))
        (should (equal (plist-get complete :summary) "updated file"))
        (should (equal (plist-get complete :result-text) "ok"))
        (should (equal (plist-get complete :duration) 1.2))
        (should (eq (plist-get diff :type) 'diff))
        (should (equal (plist-get diff :session-id) "sid"))
        (should (equal (plist-get diff :content) inline-diff))))))

(ert-deftest hermes-transport-normalizes-legacy-events ()
  (should (equal (hermes-transport-normalize-event
                  '(:type delta :content "hello"))
                 '(:type delta :content "hello")))
  (should (equal (hermes-transport-normalize-event '(:type done))
                 '(:type done)))
  (should (equal (hermes-transport-normalize-event
                  '(:type error :content "boom"))
                 '(:type error :content "boom"))))

(ert-deftest hermes-transport-normalizes-progress-status-tool-commentary ()
  (let ((progress (hermes-transport-normalize-event
                   '((event . "tool.progress")
                     (tool_name . "terminal")
                     (delta . "running make test"))))
        (status (car (hermes-transport-parse-events
                      "event: run.started\ndata: {\"run_id\":\"r1\",\"status\":\"running\"}\n\n")))
        (tool (hermes-transport-normalize-event
               '((event . "tool.started")
                 (tool . "read_file")
                 (preview . "AGENTS.md")
                 (args . ((path . "AGENTS.md"))))))
        (hermes-tool (car (hermes-transport-parse-events
                           (concat "event: hermes.tool.progress\n"
                                   "data: {\"tool\":\"terminal\","
                                   "\"label\":\"Running make test\","
                                   "\"toolCallId\":\"call-1\","
                                   "\"status\":\"running\"}\n\n"))))
        (commentary (hermes-transport-normalize-event
                     '((event . "reasoning.available")
                       (text . "I'll inspect the repo first.")))))
    (should (eq (plist-get progress :type) 'progress))
    (should (equal (plist-get progress :event) "tool.progress"))
    (should (equal (plist-get progress :name) "terminal"))
    (should (equal (plist-get progress :content) "running make test"))
    (should (eq (plist-get status :type) 'status))
    (should (equal (plist-get status :event) "run.started"))
    (should (equal (plist-get status :run-id) "r1"))
    (should (equal (plist-get status :status) "running"))
    (should (eq (plist-get tool :type) 'tool))
    (should (equal (plist-get tool :name) "read_file"))
    (should (equal (plist-get tool :status) "started"))
    (should (equal (plist-get tool :preview) "AGENTS.md"))
    (should (equal (plist-get tool :args) '((path . "AGENTS.md"))))
    (should (eq (plist-get hermes-tool :type) 'tool))
    (should (equal (plist-get hermes-tool :event) "hermes.tool.progress"))
    (should (equal (plist-get hermes-tool :name) "terminal"))
    (should (equal (plist-get hermes-tool :status) "running"))
    (should (equal (plist-get hermes-tool :preview) "Running make test"))
    (should (equal (plist-get hermes-tool :tool-call-id) "call-1"))
    (should (eq (plist-get commentary :type) 'commentary))
    (should (equal (plist-get commentary :content)
                   "I'll inspect the repo first."))))

(ert-deftest hermes-transport-normalizes-hermes-agent-tool-events ()
  (let ((chunk (hermes-transport-normalize-event
                '((type . "ToolCallChunk")
                  (tool_name . "read_file")
                  (preview . "AGENTS.md")
                  (index . 2)
                  (args . ((path . "AGENTS.md"))))))
        (finished (hermes-transport-normalize-event
                   '((type . "ToolCallFinished")
                     (tool_name . "read_file")
                     (duration . 1.2)
                     (ok . t)
                     (index . 2))))
        (failed (hermes-transport-normalize-event
                 '((type . "ToolCallFinished")
                   (tool_name . "terminal")
                   (duration . 0.1)
                   (ok . nil)
                   (index . 3)))))
    (should (eq (plist-get chunk :type) 'tool))
    (should (equal (plist-get chunk :event) "ToolCallChunk"))
    (should (equal (plist-get chunk :name) "read_file"))
    (should (equal (plist-get chunk :status) "running"))
    (should (equal (plist-get chunk :preview) "AGENTS.md"))
    (should (equal (plist-get chunk :index) 2))
    (should (equal (plist-get chunk :args) '((path . "AGENTS.md"))))
    (should (equal (plist-get finished :status) "completed"))
    (should (equal (plist-get finished :duration) 1.2))
    (should (equal (plist-get finished :index) 2))
    (should (equal (plist-get failed :status) "failed"))))

(ert-deftest hermes-transport-parses-sse-and-preserves-plain-text ()
  (let ((events (hermes-transport-parse-events
                 (concat "event: assistant.delta\n"
                         "data: {\"delta\":\"hello\"}\n\n"
                         "data: {\"event\":\"done\"}\n\n"))))
    (should (equal (mapcar (lambda (event) (plist-get event :type)) events)
                   '(delta done)))
    (should (equal (plist-get (car events) :content) "hello")))
  (should (equal (hermes-transport-parse-events "plain CLI output")
                 '((:type delta :content "plain CLI output"))))
  (should (equal (hermes-transport-parse-events "running" "tool.progress")
                 '((:type progress :event "tool.progress" :content "running"))))
  (should (equal (hermes-transport-parse-events ": keepalive\n\n")
                 nil))
  (should (equal (hermes-transport-parse-events "{\"answer\":42}")
                 '((:type delta :content "{\"answer\":42}")))))

(ert-deftest hermes-transport-handles-unknown-and-invalid-events ()
  (let* ((raw '((event . "alien.signal") (payload . 1)))
         (unknown (hermes-transport-normalize-event raw))
         (invalid (hermes-transport-normalize-event '(:content "missing type")))
         (message-error (hermes-transport-normalize-event
                         '((event . "error") (message . "boom"))))
         (response-error (hermes-transport-normalize-event
                          '((type . "response.failed")
                            (response . ((error . ((message . "bad request"))))))))
         (bad-json (car (hermes-transport-parse-events
                         "{\"type\": \"status\""))))
    (should (eq (plist-get unknown :type) 'unknown))
    (should (equal (plist-get unknown :event) "alien.signal"))
    (should (equal (plist-get unknown :raw) raw))
    (should (eq (plist-get invalid :type) 'error))
    (should (string-match-p "Invalid Hermes transport event"
                            (plist-get invalid :content)))
    (should (equal (plist-get message-error :content) "boom"))
    (should (equal (plist-get response-error :content) "bad request"))
    (should (eq (plist-get bad-json :type) 'error))
    (should (string-match-p "Invalid Hermes transport JSON"
                            (plist-get bad-json :content)))))

(ert-deftest hermes-transport-normalizes-message-start-as-status ()
  (let ((event (hermes-transport-normalize-event
                '((event . "message.start")
                  (session_id . "sid-live")))))
    (should (eq (plist-get event :type) 'status))
    (should (equal (plist-get event :event) "message.start"))
    (should (equal (plist-get event :session-id) "sid-live"))
    (should (equal (plist-get event :status) "started"))
    (should (hermes-chat--active-status-p (plist-get event :status)))))

(ert-deftest hermes-transport-dashboard-normalizes-message-start-as-status ()
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "message.start")
                            (session_id . "sid-live")))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :event) "message.start"))
      (should (equal (plist-get event :session-id) "sid-live"))
      (should (equal (plist-get event :status) "started"))
      (should (hermes-chat--active-status-p (plist-get event :status))))))

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

(ert-deftest hermes-transport-dashboard-secret-list-tolerates-malformed ()
  "Secret collection filters strings and never aborts on a malformed slot."
  (should (equal '("a" "b")
                 (hermes-dashboard-transport--secret-list '("a" "" nil 7 "b"))))
  ;; An improper list is what `append' builds when the secrets slot holds a
  ;; bare string (e.g. a stale struct); it must not signal.
  (should (equal '("tok" "sid")
                 (hermes-dashboard-transport--secret-list '("tok" . "sid"))))
  (should (equal '("tok") (hermes-dashboard-transport--secret-list "tok")))
  (should-not (hermes-dashboard-transport--secret-list nil)))

(ert-deftest hermes-transport-dashboard-stop-tolerates-teardown-errors ()
  "Stop never throws and still closes resources when a teardown step errors."
  (let (closed deleted)
    (cl-letf (((symbol-function 'websocket-close)
               (lambda (ws) (setq closed ws)))
              ((symbol-function 'delete-process)
               (lambda (p) (setq deleted p)))
              ((symbol-function 'hermes-dashboard-transport--reject-pending-requests)
               (lambda (&rest _) (error "boom"))))
      (let ((client (make-hermes-dashboard-transport-client
                     :process 'fake-process
                     :websocket 'fake-websocket
                     :pending (make-hash-table :test #'equal))))
        (should (hermes-dashboard-transport-stop client))
        (should (eq closed 'fake-websocket))
        (should (eq deleted 'fake-process))
        (should-not (hermes-dashboard-transport-client-websocket client))
        (should-not (hermes-dashboard-transport-client-process client))))))

(ert-deftest hermes-transport-dashboard-parse-url-host-and-port ()
  "Dashboard URL parsing yields host and effective port."
  (should (equal '(:host "127.0.0.1" :port 9119)
                 (hermes-dashboard-transport--parse-url "http://127.0.0.1:9119")))
  (should (equal '(:host "example.test" :port 443)
                 (hermes-dashboard-transport--parse-url "https://example.test/hermes"))))

(ert-deftest hermes-transport-dashboard-url-drives-remote-attach ()
  "A non-loopback `hermes-dashboard-transport-url' attaches remotely."
  (let (opened-url)
    (let ((hermes-dashboard-transport-url "http://100.64.0.10:9119")
          (hermes-dashboard-transport-start-mode 'auto)
          (hermes-dashboard-transport-remote-auth-method 'token)
          (hermes-dashboard-transport-ready-timeout nil)
          (hermes-dashboard-transport-make-process-function
           (lambda (&rest _) (error "remote attach must not spawn")))
          (hermes-dashboard-transport-websocket-open-function
           (lambda (url _client) (setq opened-url url) 'fake-websocket)))
      (hermes-dashboard-transport-start :token "remote-token" :callback #'ignore)
      (should (equal opened-url
                     "ws://100.64.0.10:9119/api/ws?token=remote-token")))))

(provide 'hermes-tests)
;;; hermes-tests.el ends here
