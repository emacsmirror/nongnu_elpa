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
(require 'hermes-mcp)

(defun hermes-test--chat-buffer-name ()
  "Return a fresh chat buffer name for tests."
  (generate-new-buffer-name "*Hermes Chat Test*"))

(defun hermes-test--dashboard-buffer-name ()
  "Return a fresh dashboard buffer name for tests."
  (generate-new-buffer-name "*Hermes Dashboard Test*"))

(defmacro hermes-test-with-chat-buffer (&rest body)
  "Create a fresh Hermes chat buffer and run BODY in it.
The buffer is captured by object so teardown still kills it after a rename."
  (declare (indent 0) (debug t))
  `(let* ((hermes-chat-buffer-name (hermes-test--chat-buffer-name))
          (buffer (progn (hermes-chat)
                         (get-buffer hermes-chat-buffer-name))))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer)
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

(defun hermes-test--count-buttons-labeled (label)
  "Return the number of buttons whose text is LABEL in the current buffer."
  (let ((count 0)
        (search (concat "[" label "]")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward search nil t)
        (when (button-at (1- (point)))
          (setq count (1+ count)))))
    count))

(defun hermes-test--view-diff-content ()
  "Push the first View Diff link and return the diff buffer text."
  (hermes-test--push-button-labeled "View Diff")
  (with-current-buffer "*Hermes Diff*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hermes-test--assistant-entry ()
  "Return the chat entry whose role is `assistant' (the agent reply)."
  (cl-find-if (lambda (entry) (eq (plist-get entry :role) 'assistant))
              (hermes-chat--entries)))

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
  (should (eq (keymap-lookup hermes-dashboard-mode-map "P") #'hermes-chat-new-profile-session))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "m") #'hermes-dashboard-switch-model))
  (should (eq (keymap-lookup hermes-dashboard-mode-map "X") #'hermes-list-mcp))
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
    (dolist (key '("c" "P" "m"))
      (should (cl-find key entries :key (lambda (entry)
                                         (plist-get entry :key))
                       :test #'equal)))))

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
  (dolist (case '(("in_progress" "Running" "✓" shadow t nil)
                  ("busy" "Running" "✓" shadow t nil)
                  ("approval-requested" "Approval requested" "✓"
                   shadow t nil)
                  ("queued" "Queued" "✓" shadow t nil)
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

(ert-deftest hermes-chat-transient-status-marker-uses-check-faces ()
  "Transient execution markers use a checkmark with pending/done faces."
  (dolist (case '(("running" shadow)
                  ("completed" success)))
    (pcase-let ((`(,status ,face) case))
      (with-temp-buffer
        (hermes-chat--insert-transient-content
         (list :id status :role 'progress :status status :content "doing work"))
        (goto-char (point-min))
        (search-forward "✓")
        (should (eq (get-text-property (1- (point)) 'face) face))))))

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
         (should (string-match-p "▸ Thinking\.\.\." text))
         (should-not (string-match-p "I need" text))
         (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                                entries)
                        '(user commentary assistant)))
         (should (equal (plist-get (nth 1 entries) :content) "I need")))
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
                        :event "reasoning.delta"
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
  "Renaming sets the title and the buffer name, trimming whitespace."
  (hermes-test-with-chat-buffer
   (hermes-chat-rename "  My Project  ")
   (should (equal hermes-chat--title "My Project"))
   (should (equal (buffer-name) "*Hermes: My Project*"))))

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

(ert-deftest hermes-transport-dashboard-shows-review-summary ()
  "`review.summary' becomes a status event carrying its text, not an Unknown event."
  (let (events)
    (let ((client (make-hermes-dashboard-transport-client
                   :callback (lambda (event) (push event events)))))
      (hermes-dashboard-transport--handle-frame
       client (hermes-dashboard-transport--encode-frame
               '((jsonrpc . "2.0")
                 (method . "event")
                 (params . ((type . "review.summary")
                            (session_id . "sid")
                            (payload . ((text . "Self-improvement review: profile updated")))))))))
    (let ((event (car events)))
      (should event)
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :content)
                     "Self-improvement review: profile updated")))))

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
  (should (eq (keymap-lookup hermes-chat-actions-map "N")
              #'hermes-chat-new-profile-session))
  (let* ((rows (keymap-popup--meta hermes-chat-actions-map 'descriptions))
         (entries (mapcan (lambda (row)
                            (mapcan (lambda (group)
                                      (plist-get group :entries))
                                    row))
                          rows)))
    (dolist (key '("N" "m"))
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

(ert-deftest hermes-transport-dashboard-approval-respond-payload ()
  (let ((client (hermes-test--dashboard-client))
        (hermes-dashboard-transport-request-timeout nil)
        sent-frame)
    (let ((hermes-dashboard-transport-websocket-send-function
           (lambda (_websocket text)
             (setq sent-frame (hermes-dashboard-transport--decode-frame text)))))
      (hermes-dashboard-transport-approval-respond
       client :session-id "sid-approval" :choice "session" :all t))
    (should (equal (alist-get 'method sent-frame) "approval.respond"))
    (should (equal (alist-get 'session_id (alist-get 'params sent-frame))
                   "sid-approval"))
    (should (equal (alist-get 'choice (alist-get 'params sent-frame))
                   "session"))
    (should (eq (alist-get 'all (alist-get 'params sent-frame)) t))))

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

(ert-deftest hermes-transport-dashboard-http-error-includes-json-detail ()
  "REST errors include backend JSON detail and still redact secrets."
  (let (buffer message)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq buffer (generate-new-buffer " *hermes-test-http*"))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 400 Bad Request\r\n"
                           "Content-Type: application/json\r\n\r\n"
                           "{\"detail\": \"the 'default' board cannot be removed secret-token\"}"))
                 buffer)))
      (setq message
            (condition-case error
                (progn
                  (hermes-dashboard-transport--default-http-request
                   "http://dash.example/api/plugins/kanban/boards/default?token=secret-token"
                   :secrets '("secret-token"))
                  nil)
              (user-error (error-message-string error))))
      (should (string-match-p "HTTP 400" message))
      (should (string-match-p "default.*cannot be removed" message))
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
         (let ((assistant (hermes-test--assistant-entry)))
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
                                        (provider . "openai-codex")
                                        (profile_name . "planner")
                                        (usage . ((context_used . 45000)
                                                  (context_max . 200000)
                                                  (context_percent . 22)))))))))))
    (let ((event (car events)))
      (should (eq (plist-get event :type) 'status))
      (should (equal (plist-get event :event) "session.info"))
      (should (equal (plist-get event :session-id) "sid"))
      (should (equal (plist-get event :status) "ready"))
      (should (equal (plist-get event :model) "gpt-5.5"))
      (should (equal (plist-get event :agent-name) "planner"))
      (should (equal (plist-get event :context) '(:used 45000 :max 200000 :percent 22)))
      (should (equal (plist-get event :content)
                     "Session ready: gpt-5.5 via openai-codex")))))

(ert-deftest hermes-transport-dashboard-normalizes-reasoning-events ()
  "`reasoning.delta' becomes commentary; `thinking.delta' becomes a `thinking' event."
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
                     '(commentary thinking)))
      (should (equal (mapcar (lambda (event) (plist-get event :event)) events)
                     '("reasoning.delta" "thinking.delta")))
      (should (equal (mapcar (lambda (event) (plist-get event :session-id))
                             events)
                     '("sid" "sid")))
      (should (equal (mapcar (lambda (event) (plist-get event :content))
                             events)
                     '("inspect first" "inspect first"))))))

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

(ert-deftest hermes-transport-normalizes-message-start-underscore-as-status ()
  (let ((event (hermes-transport-normalize-event
                '((event . "message_start")
                  (session_id . "sid-live")))))
    (should (eq (plist-get event :type) 'status))
    (should (equal (plist-get event :event) "message_start"))
    (should (equal (plist-get event :session-id) "sid-live"))
    (should (equal (plist-get event :status) "started"))))

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

(ert-deftest hermes-sessions-rows-from-session-list ()
  "Session rows map the `session.list' result fields to columns."
  (let* ((rows (hermes-sessions--rows
                '(((id . "s1") (title . "First") (message_count . 3) (source . "tui")))))
         (entry (cadr (car rows))))
    (should (equal (caar rows) "s1"))
    (should (equal (aref entry 0) "s1"))
    (should (equal (aref entry 1) "First"))
    (should (equal (aref entry 2) "3"))
    (should (equal (aref entry 3) "tui"))))

(ert-deftest hermes-sessions-mode-keymap-keeps-ret-and-adds-actions ()
  "The browser keeps RET resume and exposes native history/actions."
  (should (eq (keymap-lookup hermes-sessions-mode-map "RET")
              #'hermes-sessions-open))
  (should (eq (keymap-lookup hermes-sessions-mode-map "v")
              #'hermes-sessions-view))
  (should (eq (keymap-lookup hermes-sessions-mode-map "r")
              #'hermes-sessions-rename))
  (should (eq (keymap-lookup hermes-sessions-mode-map "d")
              #'hermes-sessions-delete)))

(ert-deftest hermes-sessions-detail-renders-history-messages ()
  "The detail buffer renders user, assistant, and tool history readably."
  (let ((buffer (hermes-sessions--render-detail
                 '((id . "s1") (title . "First") (source . "tui"))
                 '(((role . "user") (text . "hi there"))
                   ((role . "assistant") (text . "hello back"))
                   ((role . "tool") (name . "terminal")
                    (context . "make test")))
                 3)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'hermes-session-detail-mode))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Session: First" text))
            (should (string-match-p "ID: s1" text))
            (should (string-match-p "Messages: 3" text))
            (should (string-match-p "\\[user\\]" text))
            (should (string-match-p "hi there" text))
            (should (string-match-p "\\[assistant\\]" text))
            (should (string-match-p "hello back" text))
            (should (string-match-p "\\[tool: terminal\\]" text))
            (should (string-match-p "make test" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-sessions-display-string-renders-structured-fallbacks ()
  "Structured history values render type or object fallback text."
  (should (equal (hermes-sessions--display-string '((type . "image_url")))
                 "[image_url]"))
  (should (string-match-p "foo"
                          (hermes-sessions--display-string
                           '((foo . "bar"))))))

(ert-deftest hermes-sessions-detail-renders-tool-name-fallbacks ()
  "Tool messages and tool calls use alternate name fields."
  (let ((buffer (hermes-sessions--render-detail
                 '((id . "s1") (title . "First"))
                 '(((role . "tool") (tool_name . "terminal")
                    (output . "done"))
                   ((role . "assistant") (text . "running")
                    (tool_calls . [((id . "call-1")
                                    (name . "terminal")
                                    (arguments . "make test"))])))
                 2)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "\\[tool: terminal\\]" text))
            (should (string-match-p "tool-call: terminal" text))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest hermes-sessions-view-fetches-history-and-renders-detail ()
  "Viewing a row requests `session.history' and renders the result."
  (let ((history-result '((count . 2)
                          (messages . (((role . "user") (text . "question"))
                                       ((role . "assistant")
                                        (text . "answer"))))))
        history-session stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq history-session session-id)
                 (funcall (plist-get args :resolve) history-result))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal history-session "s1"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: s1*"
              (should (string-match-p "question" (buffer-string)))
              (should (string-match-p "answer" (buffer-string)))))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest hermes-sessions-view-stale-detail-live-id-resumes-durable-id ()
  "Detail refresh resumes the durable id after stale live history fails."
  (let ((history-result '((count . 1)
                          (messages . (((role . "assistant")
                                        (text . "resumed"))))))
        history-session resume-session stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq history-session session-id)
                 (funcall (plist-get args :reject) "session not found")))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (client session-id &rest args)
                 (should (eq client 'fake-client))
                 (setq resume-session session-id)
                 (funcall (plist-get args :resolve) history-result))))
      (unwind-protect
          (progn
            (hermes-sessions--render-detail
             '((id . "durable-1") (live_session_id . "dead-live")
               (title . "Stored"))
             nil 0)
            (with-current-buffer "*Hermes Session: durable-1*"
              (hermes-sessions-view))
            (should (equal history-session "dead-live"))
            (should (equal resume-session "durable-1"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Session: durable-1*"
              (should (string-match-p "resumed" (buffer-string)))))
        (when (get-buffer "*Hermes Session: durable-1*")
          (kill-buffer "*Hermes Session: durable-1*"))))))

(ert-deftest hermes-sessions-view-rejects-with-message ()
  "A failed history request reports the gateway error without rendering detail."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-history)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "history failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-view))
            (should (equal shown "Hermes: history failed"))
            (should-not (get-buffer "*Hermes Session: s1*")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-rename-prompts-and-dispatches-title ()
  "Renaming a selected row prompts and dispatches `session.title'."
  (let (sent stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "Renamed"))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (client &rest args)
                 (setq sent (cons client args))
                 (funcall (plist-get args :resolve)
                          '((pending . :json-false) (title . "Renamed"))))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-rename))
            (should (eq (car sent) 'fake-client))
            (should (equal (plist-get (cdr sent) :session-id) "s1"))
            (should (equal (plist-get (cdr sent) :title) "Renamed"))
            (should (eq stopped 'fake-client)))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-rename-from-detail-updates-open-browser ()
  "Renaming from detail keeps an open browser row in sync."
  (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
            ((symbol-function 'hermes-dashboard-transport-start)
             (lambda (&rest _) 'fake-client))
            ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
            ((symbol-function 'read-string)
             (lambda (&rest _) "Renamed"))
            ((symbol-function 'hermes-dashboard-transport-session-title)
             (lambda (_client &rest args)
               (funcall (plist-get args :resolve)
                        '((pending . :json-false) (title . "Renamed"))))))
    (unwind-protect
        (progn
          (hermes-sessions--render
           '(((id . "s1") (title . "First") (message_count . 2))))
          (hermes-sessions--render-detail
           '((id . "s1") (title . "First") (message_count . 2))
           '(((role . "user") (text . "question")))
           1)
          (with-current-buffer "*Hermes Session: s1*"
            (hermes-sessions-rename))
          (with-current-buffer "*Hermes Sessions*"
            (should (equal (aref (cadr (assoc "s1" tabulated-list-entries)) 1)
                           "Renamed")))
          (with-current-buffer "*Hermes Session: s1*"
            (should (string-match-p "Session: Renamed" (buffer-string)))))
      (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
        (when (get-buffer name)
          (kill-buffer name))))))

(ert-deftest hermes-sessions-rename-rejects-empty-title-before-dispatch ()
  "Empty rename input is rejected locally before an RPC is sent."
  (let (sent)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  "))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (&rest _) (setq sent t))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (should-error (hermes-sessions-rename) :type 'user-error))
            (should-not sent))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-rename-rejects-with-message ()
  "A failed rename request reports the gateway error without updating the row."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
              ((symbol-function 'hermes-dashboard-transport-session-title)
               (lambda (_client &rest args)
                 (funcall (plist-get args :reject) "rename failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-rename)
              (should (equal (aref (cadr (assoc "s1" tabulated-list-entries)) 1)
                             "First")))
            (should (equal shown "Hermes: rename failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-delete-prompts-and-dispatches-delete ()
  "Deleting a selected row asks for confirmation before `session.delete'."
  (let (deleted stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (client session-id &rest args)
                 (setq deleted (list client session-id))
                 (funcall (plist-get args :resolve)
                          '((deleted . "s1"))))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (hermes-sessions--render-detail
             '((id . "s1") (title . "First") (message_count . 2))
             '(((role . "user") (text . "question")))
             1)
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete)
              (should-not (assoc "s1" tabulated-list-entries)))
            (should-not (get-buffer "*Hermes Session: s1*"))
            (should (equal deleted '(fake-client "s1")))
            (should (eq stopped 'fake-client)))
        (dolist (name '("*Hermes Sessions*" "*Hermes Session: s1*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest hermes-sessions-delete-cancel-does-not-dispatch ()
  "Answering no to the delete prompt leaves the session untouched."
  (let (deleted)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (&rest _) (setq deleted t))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete))
            (should-not deleted))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-sessions-delete-rejects-with-message ()
  "A failed delete request reports the gateway error."
  (let (shown)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-session-delete)
               (lambda (_client _session-id &rest args)
                 (funcall (plist-get args :reject) "delete failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq shown (apply #'format fmt args)))))
      (unwind-protect
          (progn
            (hermes-sessions--render
             '(((id . "s1") (title . "First") (message_count . 2))))
            (with-current-buffer "*Hermes Sessions*"
              (goto-char (point-min))
              (search-forward "s1")
              (beginning-of-line)
              (hermes-sessions-delete))
            (should (equal shown "Hermes: delete failed")))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

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

(ert-deftest hermes-sessions-list-renders-and-stops-transient-client ()
  "Listing connects a transient client, renders rows, then stops it."
  (let (listed stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-session-list)
               (lambda (client &rest args)
                 (setq listed client)
                 (funcall (plist-get args :resolve)
                          '((sessions
                             . (((id . "s1") (title . "First") (message_count . 3))
                                ((id . "s2") (title . "Second") (message_count . 0)))))))))
      (unwind-protect
          (progn
            (hermes-list-sessions)
            (should (eq listed 'fake-client))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Sessions*"
              (should (derived-mode-p 'hermes-sessions-mode))
              (should (equal (sort (mapcar #'car tabulated-list-entries) #'string<)
                             '("s1" "s2")))))
        (when (get-buffer "*Hermes Sessions*")
          (kill-buffer "*Hermes Sessions*"))))))

(ert-deftest hermes-transport-dashboard-message-complete-carries-usage ()
  "A `message.complete' event carries input/output token usage."
  (let* ((frame '((jsonrpc . "2.0") (method . "event")
                  (params . ((type . "message.complete")
                             (payload . ((status . "complete")
                                         (input_tokens . 1200)
                                         (output_tokens . 340)))))))
         (event (car (hermes-dashboard-transport--normalize-event-frame frame))))
    (should (eq (plist-get event :type) 'done))
    (should (equal (plist-get event :usage) '(:input 1200 :output 340)))))

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

(ert-deftest hermes-chat-turn-reduce-status-family-stamps-state-no-effects ()
  "Status-family events thread a NOW-stamped :status-state and emit no effects."
  (let ((now '(100 200)))
    (dolist (case
             (list
              (list '(:status-state (:status running :activity "old"))
                    '(:type thinking :content "pondering...")
                    '(:status-state (:status thinking :activity "Pondering"
                                             :updated (100 200))))
              (list '(:status-state (:status thinking :activity "x"))
                    '(:type commentary)
                    '(:status-state (:status running :activity "Thinking..."
                                             :updated (100 200))))
              (list '(:status-state nil)
                    '(:type diff)
                    '(:status-state (:status running :activity "Reviewing diff"
                                             :updated (100 200))))))
      (cl-destructuring-bind (state event expected-state) case
        (let ((result (hermes-chat--turn-reduce state event now)))
          (should (equal (car result) expected-state))
          (should-not (cdr result)))))))

(ert-deftest hermes-chat-turn-reduce-status-stamps-clock-purely ()
  "A status event bakes the injected NOW into the threaded :status-state."
  (let* ((now '(7 7))
         (state '(:status-state (:status idle :activity "x")))
         (event '(:type status :status "running" :content "Searching"))
         (result (hermes-chat--turn-reduce state event now))
         (status-state (plist-get (car result) :status-state)))
    (should-not (cdr result))
    (should (equal (plist-get status-state :updated) now))
    (should (equal status-state
                   (apply #'hermes-chat--entry-with
                          '(:status idle :activity "x")
                          (append (hermes-chat--turn-status-props event)
                                  (list :updated now)))))))

(ert-deftest hermes-chat-turn-reduce-tool-family-emits-pure-delta ()
  "Tool-like events leave the state untouched and emit a pure tool delta."
  (let ((running '(:type tool :name "terminal" :status "running" :context "make test"))
        (done '(:type tool :name "terminal" :status "completed" :context "make test"))
        (state '(:status-state (:status running))))
    (let ((result (hermes-chat--turn-reduce state running '(0 0))))
      (should (eq (car result) state))
      (should (equal (cdr result)
                     (list (cons 'tool-put
                                 (cons (hermes-chat--header-tool-key running)
                                       (hermes-chat--header-tool-summary running)))))))
    (let ((result (hermes-chat--turn-reduce state done '(0 0))))
      (should (equal (cdr result)
                     (list (cons 'tool-remove
                                 (hermes-chat--header-tool-key done))))))
    ;; No summary -> no delta.
    (should-not (hermes-chat--turn-tool-effect '(:type status)))))

(ert-deftest hermes-chat-turn-reduce-out-of-scope-is-noop ()
  "Out-of-scope events return the same state object and no effects."
  (let ((state '(:status-state (:status running))))
    (should (equal (hermes-chat--turn-reduce state '(:type done) '(0 0))
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
    (should (string-match-p "\$3" (car labels)))
    (should (string-match-p "reasoning" (car labels)))
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
             hermes-chat--dashboard-active-session-id "sid-1")
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
             hermes-chat--dashboard-active-session-id "sid-1")
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
             hermes-chat--dashboard-active-session-id "sid-1")
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
             hermes-chat--dashboard-active-session-id "sid-1")
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
           hermes-chat--dashboard-active-session-id "sid-1")
     (hermes-chat-switch-model)
     (should (string-match-p "backend denied" (buffer-string))))))

(ert-deftest hermes-inventory-toolset-rows ()
  "Toolset rows map name/enabled/count/description."
  (let ((rows (hermes-inventory--toolset-rows
               '((toolsets . (((name . "files") (enabled . t) (tool_count . 5)
                               (description . "File ops"))))))))
    (should (equal (caar rows) "files"))
    (should (equal (aref (cadr (car rows)) 1) "on"))
    (should (equal (aref (cadr (car rows)) 2) "5"))
    (should (equal (aref (cadr (car rows)) 3) "File ops"))))

(ert-deftest hermes-inventory-skill-rows-flattens-categories ()
  "Skill rows flatten the category->names map into per-skill rows."
  (let ((rows (hermes-inventory--skill-rows
               '((skills . ((coding . ("refactor" "review")) (writing . ("draft"))))))))
    (should (equal (mapcar (lambda (r) (aref (cadr r) 1)) rows)
                   '("refactor" "review" "draft")))
    (should (equal (aref (cadr (car rows)) 0) "coding"))))

(ert-deftest hermes-inventory-agent-and-plugin-rows ()
  "Agent and plugin rows map their fields."
  (let ((agents (hermes-inventory--agent-rows
                 '((processes . (((session_id . "a1") (status . "running")
                                  (uptime . 42) (command . "do x")))))))
        (plugins (hermes-inventory--plugin-rows
                  '((plugins . (((name . "p1") (version . "1.2") (enabled . nil))))))))
    (should (equal (aref (cadr (car agents)) 0) "a1"))
    (should (equal (aref (cadr (car agents)) 2) "42"))
    (should (equal (aref (cadr (car plugins)) 1) "1.2"))
    (should (equal (aref (cadr (car plugins)) 2) "off"))))

(ert-deftest hermes-inventory-list-fetches-and-renders ()
  "Choosing a category fetches its method and renders the rows."
  (let (requested-method stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'completing-read) (lambda (&rest _) "Toolsets"))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method _params resolve _reject)
                 (setq requested-method method)
                 (funcall resolve '((toolsets . (((name . "files") (enabled . t)
                                                  (tool_count . 5)))))))))
      (unwind-protect
          (progn
            (hermes-list-inventory)
            (should (equal requested-method "tools.list"))
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Toolsets*"
              (should (derived-mode-p 'hermes-inventory-mode))
              (should (equal (caar tabulated-list-entries) "files"))))
        (when (get-buffer "*Hermes Toolsets*") (kill-buffer "*Hermes Toolsets*"))))))

(ert-deftest hermes-inventory-skill-rows-map-dashboard-skill-list ()
  "Skill rows map dashboard REST skill metadata, including enabled state."
  (let ((rows (hermes-inventory--skill-rows
               '((skills . (((name . "review") (category . "coding")
                              (description . "Review code") (enabled . t))
                             ((name . "draft") (category . "writing")
                              (description . "Draft text") (enabled . nil))))))))
    (should (equal (mapcar #'car rows) '("review" "draft")))
    (should (equal (aref (cadr (car rows)) 0) "coding"))
    (should (equal (aref (cadr (car rows)) 2) "on"))
    (should (equal (aref (cadr (nth 1 rows)) 2) "off"))
    (should (equal (aref (cadr (car rows)) 3) "Review code"))))

(ert-deftest hermes-inventory-skills-result-normalizes-rest-shapes ()
  "Skill REST payloads normalize raw-list and object response shapes."
  (let ((raw '(((name . "review") (enabled . t))))
        (wrapped '((skills . (((name . "draft") (enabled . nil)))))))
    (should (equal (hermes-inventory--skills-result raw)
                   `((skills . ,raw))))
    (should (equal (hermes-inventory--skills-result wrapped) wrapped))))

(ert-deftest hermes-inventory-fetch-skills-prefers-rest ()
  "Skill inventory fetch uses dashboard REST when `/api/skills' is available."
  (let (method path requested-client rendered done-called)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       requested-client (plist-get args :client))
                 '(((name . "review") (enabled . t)))))
              ((symbol-function 'hermes-inventory--render-result)
               (lambda (_spec result) (setq rendered result))))
      (hermes-inventory--fetch-skills
       'fake-client (lambda () (setq done-called t))
       (assoc "Skills" hermes-inventory--specs))
      (should done-called)
      (should (equal method "GET"))
      (should (equal path "/api/skills"))
      (should (eq requested-client 'fake-client))
      (should (equal rendered '((skills . (((name . "review") (enabled . t))))))))))

(ert-deftest hermes-inventory-fetch-skills-falls-back-to-jsonrpc ()
  "Skill inventory fetch falls back to read-only JSON-RPC when REST fails."
  (let (fallback-spec message-text)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (&rest _) (user-error "missing endpoint")))
              ((symbol-function 'hermes-inventory--fetch-via-jsonrpc)
               (lambda (_client _done spec) (setq fallback-spec spec)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (let ((spec (assoc "Skills" hermes-inventory--specs)))
        (hermes-inventory--fetch-skills 'fake-client #'ignore spec)
        (should (eq fallback-spec spec))
        (should (string-match-p "using read-only list" message-text))))))

(ert-deftest hermes-dashboard-transport-tools-configure-sends-action-payload ()
  "The transport wrapper sends `tools.configure' names/action/session_id."
  (let (method params)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((ok . t))))))
      (let ((client (hermes-test--dashboard-client)))
        (setf (hermes-dashboard-transport-client-session-id client) "sid-1")
        (hermes-dashboard-transport-tools-configure
         client '("terminal") "disable" :resolve #'ignore :reject #'ignore))
      (should (equal method "tools.configure"))
      (should (equal (cdr (assq 'names params)) '("terminal")))
      (should (equal (cdr (assq 'action params)) "disable"))
      (should (equal (cdr (assq 'session_id params)) "sid-1")))))

(ert-deftest hermes-dashboard-transport-skills-reload-sends-rpc ()
  "The transport wrapper sends `skills.reload' without shelling out."
  (let (method params resolved)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client m p resolve _reject)
                 (setq method m params p)
                 (funcall resolve '((output . "ok"))))))
      (hermes-dashboard-transport-skills-reload
       'fake-client
       :resolve (lambda (result) (setq resolved result))
       :reject #'ignore)
      (should (equal method "skills.reload"))
      (should-not params)
      (should (equal resolved '((output . "ok")))))))

(ert-deftest hermes-inventory-toolset-toggle-sends-tools-configure ()
  "Inventory toolset actions go through `tools.configure' with safe actions."
  (let (names action session done-called reverted)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (let ((client (hermes-test--dashboard-client)))
                   (setf (hermes-dashboard-transport-client-session-id client) "sid-2")
                   (funcall fn client (lambda () (setq done-called t))))))
              ((symbol-function 'hermes-dashboard-transport-tools-configure)
               (lambda (_client ns act &rest args)
                 (setq names ns action act session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((reset . t)))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) #'ignore))
      (hermes-inventory--set-toolset-enabled "terminal" nil)
      (should done-called)
      (should reverted)
      (should (equal names '("terminal")))
      (should (equal action "disable"))
      (should (equal session "sid-2")))))

(ert-deftest hermes-inventory-skill-toggle-posts-rest-json-boolean ()
  "Inventory skill actions use the dashboard REST toggle endpoint, no CLI shellout."
  (let (method path body requested-client done-called reverted)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       body (plist-get args :body)
                       requested-client (plist-get args :client))
                 '((ok . t))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message) #'ignore))
      (hermes-inventory--set-skill-enabled "review" nil)
      (should done-called)
      (should reverted)
      (should (equal method "PUT"))
      (should (equal path "/api/skills/toggle"))
      (should (eq requested-client 'fake-client))
      (should (equal body '((name . "review") (enabled . :false)))))))

(ert-deftest hermes-inventory-skill-toggle-cleans-up-on-rest-error ()
  "Skill toggle stops transient clients when REST toggle fails."
  (let (done-called message-text reverted)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (&rest _) (user-error "token missing")))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (hermes-inventory--set-skill-enabled "review" t)
      (should done-called)
      (should-not reverted)
      (should (equal message-text "Hermes: token missing")))))

(ert-deftest hermes-inventory-reload-skills-dispatches-rpc-and-refreshes ()
  "Skill reload uses dashboard RPC and refreshes skill inventory buffers."
  (let (done-called reloaded-client message-text reverted)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-skills-reload)
               (lambda (client &rest args)
                 (setq reloaded-client client)
                 (funcall (plist-get args :resolve) '((output . "Reloaded skills")))))
              ((symbol-function 'hermes-inventory--revert)
               (lambda (&rest _) (setq reverted t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (let ((hermes-inventory--spec (assoc "Skills" hermes-inventory--specs)))
        (hermes-inventory-reload-skills))
      (should done-called)
      (should (eq reloaded-client 'fake-client))
      (should (equal message-text "Hermes: Reloaded skills"))
      (should reverted))))

(ert-deftest hermes-inventory-memory-status-redacts-secrets-and-contents ()
  "Memory status displays only provider names/sizes, never contents or secrets."
  (let* ((secret "token-secret-value-that-is-long-enough-to-redact-1234567890")
         (text (hermes-inventory--memory-status-text
                `((active . ,secret)
                  (providers . (((name . "built-in")
                                 (description . "contains-private-detail")
                                 (configured . t))
                                ((name . ,secret)
                                 (description . "contains-private-token")
                                 (configured . nil))))
                  (builtin_files . ((memory . 12) (user . 34)))
                  (memory_contents . "do not show this")))))
    (should (string-match-p "Active provider: <redacted>" text))
    (should (string-match-p "MEMORY.md: 12 bytes" text))
    (should (string-match-p "USER.md: 34 bytes" text))
    (should (string-match-p "<redacted>" text))
    (should-not (string-match-p (regexp-quote secret) text))
    (should-not (string-match-p "External providers" text))
    (should-not (string-match-p "built-in (configured)" text))
    (should-not (string-match-p "contains-private" text))
    (should-not (string-match-p "do not show this" text))))

(ert-deftest hermes-memory-status-fetches-rest-with-client ()
  "Memory status passes the live dashboard client to REST."
  (let (method path requested-client rendered done-called)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       requested-client (plist-get args :client))
                 '((active . "built-in") (builtin_files . ((memory . 1))))))
              ((symbol-function 'hermes-inventory--render-memory-status)
               (lambda (status) (setq rendered status))))
      (hermes-memory-status)
      (should done-called)
      (should (equal method "GET"))
      (should (equal path "/api/memory"))
      (should (eq requested-client 'fake-client))
      (should (equal rendered '((active . "built-in")
                                (builtin_files . ((memory . 1)))))))))

(ert-deftest hermes-memory-reset-confirms-and-posts-target ()
  "Memory reset is gated by yes-or-no-p and posts the chosen target to REST."
  (let (prompt method path body requested-client done-called refreshed)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t))
              ((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (m p &rest args)
                 (setq method m
                       path p
                       body (plist-get args :body)
                       requested-client (plist-get args :client))
                 '((ok . t) (deleted . ("USER.md")))))
              ((symbol-function 'hermes-memory-status)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message) #'ignore))
      (hermes-memory-reset "user")
      (should (string-match-p "Erase built-in Hermes user memory" prompt))
      (should (equal method "POST"))
      (should (equal path "/api/memory/reset"))
      (should (equal body '((target . "user"))))
      (should (eq requested-client 'fake-client))
      (should done-called)
      (should refreshed))))

(ert-deftest hermes-memory-reset-cancel-skips-client-and-rest ()
  "Declining memory reset stops before client startup or REST calls."
  (let (with-client-called request-called)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'hermes-sessions--with-client)
               (lambda (&rest _)
                 (setq with-client-called t)))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (&rest _)
                 (setq request-called t))))
      (hermes-memory-reset "all")
      (should-not with-client-called)
      (should-not request-called))))

(ert-deftest hermes-memory-status-reports-rest-errors ()
  "Memory status reports REST errors."
  (let (message-text requested-client done-called)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client (lambda () (setq done-called t)))))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (&rest args)
                 (setq requested-client (plist-get args :client))
                 (user-error "backend unavailable")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (hermes-memory-status)
      (should done-called)
      (should (eq requested-client 'fake-client))
      (should (equal message-text "Hermes: backend unavailable")))))

(ert-deftest hermes-rollback-rows-from-list ()
  "Rollback rows abbreviate the hash and map timestamp/message."
  (let ((rows (hermes-rollback--rows
               '((checkpoints . (((hash . "abcdef1234567890")
                                  (timestamp . "2026-01-01") (message . "edit foo"))))))))
    (should (equal (caar rows) "abcdef1234567890"))
    (should (equal (aref (cadr (car rows)) 0) "abcdef12"))
    (should (equal (aref (cadr (car rows)) 1) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 2) "edit foo"))))

(ert-deftest hermes-rollback-list-fetches-and-renders ()
  "Listing fetches rollback.list and renders the checkpoints."
  (let (stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-rollback-list)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((checkpoints . (((hash . "h1") (message . "m1")))))))))
      (unwind-protect
          (progn
            (hermes-list-rollbacks)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Rollbacks*"
              (should (derived-mode-p 'hermes-rollback-mode))
              (should (equal (caar tabulated-list-entries) "h1"))))
        (when (get-buffer "*Hermes Rollbacks*") (kill-buffer "*Hermes Rollbacks*"))))))

(ert-deftest hermes-rollback-display-diff-fontifies ()
  "The diff view renders the unified diff through diff-mode."
  (unwind-protect
      (progn
        (hermes-rollback--display-diff
         "abc1234567"
         '((diff . "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n")))
        (with-current-buffer "*Hermes Rollback Diff*"
          (should (derived-mode-p 'diff-mode))
          (should (string-match-p "\\+new" (buffer-string)))))
    (when (get-buffer "*Hermes Rollback Diff*")
      (kill-buffer "*Hermes Rollback Diff*"))))

(ert-deftest hermes-subagents-rows-indents-by-depth ()
  "Subagent rows indent the goal by spawn depth."
  (let ((rows (hermes-subagents--rows
               '((active . (((subagent_id . "s0") (depth . 0) (goal . "root")
                             (status . "running") (model . "m") (tool_count . 2))
                            ((subagent_id . "s1") (depth . 2) (goal . "child")
                             (status . "running") (model . "m") (tool_count . 0))))))))
    (should (equal (caar rows) "s0"))
    (should (equal (aref (cadr (car rows)) 0) "root"))
    (should (equal (aref (cadr (nth 1 rows)) 0) "    child"))
    (should (equal (aref (cadr (car rows)) 3) "2"))))

(ert-deftest hermes-subagents-list-fetches-and-renders ()
  "Listing fetches delegation.status and renders active subagents."
  (let (stopped)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client &rest _) (setq stopped client)))
              ((symbol-function 'hermes-dashboard-transport-delegation-status)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((active . (((subagent_id . "s0") (depth . 0)
                                        (goal . "root")))))))))
      (unwind-protect
          (progn
            (hermes-list-subagents)
            (should (eq stopped 'fake-client))
            (with-current-buffer "*Hermes Subagents*"
              (should (derived-mode-p 'hermes-subagents-mode))
              (should (equal (caar tabulated-list-entries) "s0"))))
        (when (get-buffer "*Hermes Subagents*")
          (kill-buffer "*Hermes Subagents*"))))))

(defmacro hermes-test-with-cron-buffer (entries &rest body)
  "Create a cron buffer with ENTRIES and run BODY on its first row."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (with-current-buffer (get-buffer-create "*Hermes Cron*")
         (hermes-cron-mode)
         (setq tabulated-list-entries ,entries)
         (tabulated-list-print t)
         (goto-char (point-min))
         (search-forward "nightly" nil t)
         (beginning-of-line)
         ,@body)
     (when (get-buffer "*Hermes Cron*")
       (kill-buffer "*Hermes Cron*"))))

(defun hermes-test--cron-entry (&optional state)
  "Return one rich cron tabulated-list entry with optional STATE."
  (list "j1" (vector "nightly" "0 0 * * *" (or state "scheduled")
                     "work" "telegram" "2026-01-01" "2026-01-02"
                     "do it")))

(ert-deftest hermes-cron-rows-from-list ()
  "Cron rows map rich job fields into the tabulated list."
  (let ((rows (hermes-cron--rows
               '((jobs . (((id . "j1") (name . "nightly")
                           (schedule . ((expr . "0 0 * * *") (display . "daily")))
                           (state . "scheduled") (profile . "work")
                           (deliver . "telegram") (last_run_at . "2026-01-01")
                           (next_run_at . "2026-01-02") (prompt . "do it"))))))))
    (should (equal (caar rows) "j1"))
    (should (equal (aref (cadr (car rows)) 0) "nightly"))
    (should (equal (aref (cadr (car rows)) 1) "daily"))
    (should (equal (aref (cadr (car rows)) 2) "scheduled"))
    (should (equal (aref (cadr (car rows)) 3) "work"))
    (should (equal (aref (cadr (car rows)) 4) "telegram"))
    (should (equal (aref (cadr (car rows)) 5) "2026-01-01"))
    (should (equal (aref (cadr (car rows)) 6) "2026-01-02"))
    (should (equal (aref (cadr (car rows)) 7) "do it"))
    (should (equal (aref (cadr (car (hermes-cron--rows '((jobs . (((id . "j2")))))))) 2)
                   "scheduled"))
    (should (equal (aref (cadr (car (hermes-cron--rows
                                     '((jobs . (((id . "j3") (enabled . nil)))))))) 2)
                   "disabled"))))

(ert-deftest hermes-cron-client-api-sends-session-token ()
  "Cron REST calls authenticate with a live dashboard client's session token."
  (let ((client (make-hermes-dashboard-transport-client
                 :host "127.0.0.1" :port 9119 :token "[REDACTED]"))
        request)
    (let ((hermes-dashboard-transport-http-request-function
           (lambda (url &rest args)
             (setq request (list :url url :headers (plist-get args :headers)
                                 :method (plist-get args :method)
                                 :data (plist-get args :data)))
             '(:status 200 :headers nil :body ((ok . t))))))
      (should (equal (hermes-cron--client-api
                      client "POST" "/jobs/j1/trigger" nil '((profile . "work")))
                     '((ok . t))))
      (should (equal (plist-get request :url)
                     "http://127.0.0.1:9119/api/cron/jobs/j1/trigger?profile=work"))
      (should (equal (alist-get "X-Hermes-Session-Token"
                                (plist-get request :headers) nil nil #'equal)
                     "[REDACTED]"))
      (should (equal (plist-get request :method) "POST")))))

(ert-deftest hermes-cron-list-fetches-and-renders ()
  "Listing fetches cron.manage list and renders the jobs."
  (let (action)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-cron-manage)
               (lambda (_client &rest args)
                 (setq action (plist-get args :action))
                 (funcall (plist-get args :resolve)
                          '((jobs . (((job_id . "j1") (name . "nightly")))))))))
      (unwind-protect
          (progn
            (hermes-list-crons)
            (should (equal action "list"))
            (with-current-buffer "*Hermes Cron*"
              (should (derived-mode-p 'hermes-cron-mode))
              (should (equal (caar tabulated-list-entries) "j1"))))
        (when (get-buffer "*Hermes Cron*") (kill-buffer "*Hermes Cron*"))))))

(ert-deftest hermes-cron-toggle-resumes-paused-job ()
  "Toggling a paused or disabled job sends the resume action."
  (let (actions)
    (cl-letf (((symbol-function 'hermes-sessions--existing-client) (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'fake-client))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-cron-manage)
               (lambda (_client &rest args)
                 (push (plist-get args :action) actions)
                 (funcall (plist-get args :resolve)
                          (if (equal (plist-get args :action) "list")
                              '((jobs . (((job_id . "j1") (name . "n") (state . "paused")))))
                            '((ok . t)))))))
      (unwind-protect
          (progn
            (hermes-list-crons)
            (with-current-buffer "*Hermes Cron*"
              (goto-char (point-min))
              (hermes-cron-toggle))
            (should (member "resume" actions)))
        (when (get-buffer "*Hermes Cron*") (kill-buffer "*Hermes Cron*")))))
  (let (actions)
    (cl-letf (((symbol-function 'hermes-cron--act)
               (lambda (action _id _message)
                 (push action actions))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry "disabled"))
        (hermes-cron-toggle))
      (should (equal actions '("resume"))))))

(ert-deftest hermes-cron-edit-updates-job-at-point ()
  "Editing sends the update payload for the selected cron job."
  (let (calls refreshed messages)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--client-api)
               (lambda (_client method path &optional body query)
                 (push (list method path body query) calls)
                 (cond
                  ((and (equal method "GET") (equal path "/jobs/j1"))
                   '((id . "j1") (profile . "work") (name . "nightly")
                     (schedule . ((expr . "0 0 * * *"))) (prompt . "old")
                     (deliver . "local") (skills . ("old-skill"))))
                  ((equal method "PUT") '((id . "j1"))))))
              ((symbol-function 'read-string)
               (lambda (prompt &optional _initial &rest _)
                 (cond
                  ((string-prefix-p "Name" prompt) "edited")
                  ((string-prefix-p "Schedule" prompt) "*/5 * * * *")
                  ((string-prefix-p "Prompt" prompt) "new prompt")
                  ((string-prefix-p "Deliver" prompt) "telegram")
                  ((string-prefix-p "Skills" prompt) "emacs, cron")
                  (t ""))))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-edit))
      (let* ((put (car (last (nreverse calls))))
             (updates (hermes-transport--get (nth 2 put) 'updates)))
        (should (equal (nth 0 put) "PUT"))
        (should (equal (nth 1 put) "/jobs/j1"))
        (should (equal (cdr (assq 'profile (nth 3 put))) "work"))
        (should (equal (hermes-transport--get updates 'name) "edited"))
        (should (equal (hermes-transport--get updates 'schedule) "*/5 * * * *"))
        (should (equal (hermes-transport--get updates 'prompt) "new prompt"))
        (should (equal (hermes-transport--get updates 'deliver) "telegram"))
        (should (equal (hermes-transport--get updates 'skills) '("emacs" "cron")))
        (should refreshed)
        (should (member "Hermes: updated j1" messages))))))

(ert-deftest hermes-cron-trigger-posts-job-at-point ()
  "Trigger-now posts to the selected cron job endpoint."
  (let (call refreshed)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--client-api)
               (lambda (_client method path &optional body query)
                 (setq call (list method path body query))
                 '((id . "j1"))))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (equal (nth 0 call) "POST"))
      (should (equal (nth 1 call) "/jobs/j1/trigger"))
      (should-not (nth 2 call))
      (should (equal (cdr (assq 'profile (nth 3 call))) "work"))
      (should refreshed))))

(ert-deftest hermes-cron-trigger-refreshes-after-transient-client-cleanup ()
  "Trigger-now cleans up a transient client before refreshing the list."
  (let (events)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn)
                 (funcall fn 'fake-client
                          (lambda ()
                            (setq events (append events '(done)))))))
              ((symbol-function 'hermes-cron--client-api)
               (lambda (&rest _)
                 (setq events (append events '(trigger)))
                 '((id . "j1"))))
              ((symbol-function 'hermes-list-crons)
               (lambda ()
                 (setq events (append events '(refresh)))))
              ((symbol-function 'message) #'ignore))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (equal events '(trigger done refresh))))))

(ert-deftest hermes-cron-show-fetches-job-and-run-history ()
  "Detail view fetches the job and run history, then renders both."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--client-api)
               (lambda (_client method path &optional _body query)
                 (push (list method path query) calls)
                 (cond
                  ((equal path "/jobs/j1")
                   '((id . "j1") (profile . "work") (name . "nightly")
                     (schedule . ((display . "Daily at midnight")))
                     (state . "scheduled") (deliver . "telegram")
                     (prompt . "do it")))
                  ((equal path "/jobs/j1/runs")
                   '((runs . (((id . "cron_j1_1") (title . "Run one")
                               (message_count . 3) (source . "cron")
                               (started_at . 1780000000) (ended_at . 1780000030))))))))))
      (unwind-protect
          (progn
            (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
              (hermes-cron-show))
            (with-current-buffer "*Hermes Cron Job*"
              (let ((text (buffer-string)))
                (should (string-match-p "nightly" text))
                (should (string-match-p "Daily at midnight" text))
                (should (string-match-p "Runs:" text))
                (should (string-match-p "Run one" text))))
            (should (member '("GET" "/jobs/j1" ((profile . "work"))) calls))
            (should (member '("GET" "/jobs/j1/runs" ((profile . "work") (limit . 20)))
                            calls)))
        (when (get-buffer "*Hermes Cron Job*")
          (kill-buffer "*Hermes Cron Job*"))))))

(ert-deftest hermes-cron-trigger-reports-api-errors ()
  "Trigger-now reports REST failures without refreshing the list."
  (let (messages refreshed)
    (cl-letf (((symbol-function 'hermes-sessions--with-client)
               (lambda (fn) (funcall fn 'fake-client #'ignore)))
              ((symbol-function 'hermes-cron--client-api)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'hermes-list-crons)
               (lambda () (setq refreshed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (hermes-test-with-cron-buffer (list (hermes-test--cron-entry))
        (hermes-cron-trigger))
      (should (cl-some (lambda (message) (string-match-p "boom" message)) messages))
      (should-not refreshed))))

(ert-deftest hermes-chat-new-profile-session-sets-profile ()
  "A profile session records the profile; a blank one stays nil."
  (let ((buffer (hermes-chat-new-profile-session "work")))
    (unwind-protect
        (with-current-buffer buffer (should (equal hermes-chat--profile "work")))
      (kill-buffer buffer)))
  (let ((buffer (hermes-chat-new-profile-session "")))
    (unwind-protect
        (with-current-buffer buffer (should-not hermes-chat--profile))
      (kill-buffer buffer))))

(ert-deftest hermes-chat-profile-candidates-describe-dashboard-profiles ()
  "Profile candidates parse, sort, and display dashboard profile data."
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
    (should (equal (mapcar #'cdr cands)
                   '("default" "alpha" "elisp-dev" "zeta")))
    (should (string-match-p "default" (caar cands)))
    (should (string-match-p "openai/gpt-5.5" (caar cands)))
    (should (string-match-p "gateway" (caar cands)))
    (should (string-match-p "alias" (caadr cands)))
    (should (string-match-p "Emacs Lisp work" (car (nth 2 cands))))))

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

(ert-deftest hermes-chat-read-profile-uses-transient-client ()
  "The profile chooser can fetch profiles through a transient dashboard client."
  (let (listed-client stopped-client choices)
    (cl-letf (((symbol-function 'hermes-chat--existing-dashboard-client)
               (lambda () nil))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) 'transient-client))
              ((symbol-function 'hermes-dashboard-transport-stop)
               (lambda (client) (setq stopped-client client)))
              ((symbol-function 'hermes-dashboard-transport-profile-list)
               (lambda (client)
                 (setq listed-client client)
                 '((profiles . (((name . "default") (is_default . t))
                                ((name . "elisp-dev")))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 (cl-find "elisp-dev" collection :test #'string-match-p))))
      (should (equal (hermes-chat--read-profile) "elisp-dev"))
      (should (eq listed-client 'transient-client))
      (should (eq stopped-client 'transient-client))
      (should (cl-find "default" choices :test #'string-match-p)))))

(ert-deftest hermes-chat-new-profile-session-completes-dashboard-profile ()
  "Interactively creating a profile session chooses from dashboard profiles."
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
      (let ((buffer (call-interactively #'hermes-chat-new-profile-session)))
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
            (buffer (call-interactively #'hermes-chat-new-profile-session)))
        (unwind-protect
            (with-current-buffer buffer
              (should-not hermes-chat--profile)
              (insert "hello")
              (hermes-chat-send)
              (should-not create-profile))
          (kill-buffer buffer))))))

(ert-deftest hermes-kanban-status-display-uses-shared-icons ()
  "Status display helpers share icons, labels, and raw status properties."
  (should (equal hermes-kanban--current-board-marker "📍"))
  (should (equal hermes-kanban--board-count-statuses
                 '("todo" "ready" "running" "blocked" "done" "archived")))
  (dolist (spec '(("todo" "📝")
                  ("ready" "✅")
                  ("running" "⚙️")
                  ("blocked" "⛔")
                  ("done" "🏁")
                  ("archived" "🗄️")))
    (pcase-let ((`(,status ,icon) spec))
      (should (equal (hermes-kanban--status-icon status) icon))
      (let ((formatted (hermes-kanban--format-status status)))
        (should (equal (substring-no-properties formatted)
                       (format "%s %s" icon status)))
        (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                       status)))
      (let ((indicator (hermes-kanban--format-status-indicator status)))
        (should (equal (substring-no-properties indicator) icon))
        (should (equal (get-text-property 0 'hermes-kanban-status indicator)
                       status)))))
  (let ((running (hermes-kanban--format-status "running")))
    (should (equal (hermes-kanban--entry-status
                    (vector running "2" "elisp-dev" "Do thing"))
                   "running"))
    (should (equal (hermes-kanban--entry-status
                    (vector "⚙️ running" "2" "elisp-dev" "Do thing"))
                   "running")))
  (should (equal (hermes-kanban--format-status-count
                  '((ready . 2)) "ready")
                 "2"))
  (let* ((raw (copy-sequence "done"))
         (formatted (hermes-kanban--format-status raw)))
    (should (equal (substring-no-properties formatted) "🏁 done"))
    (should (equal (get-text-property 0 'hermes-kanban-status formatted)
                   "done"))
    (should-not (text-properties-at 0 raw))))

(defun hermes-test--tabulated-list-format-total-width (format)
  "Return FORMAT's display width including inter-column padding."
  (let ((total (max 0 (1- (length format)))))
    (dotimes (i (length format) total)
      (setq total (+ total (cadr (aref format i)))))))

(ert-deftest hermes-kanban-tabulated-list-formats-scale-with-width ()
  "Kanban tabulated-list formats fit and flex by display width."
  (dolist (width '(30 40 50 80 120))
    (let ((boards (hermes-kanban--boards-tabulated-list-format width))
          (tasks (hermes-kanban--tasks-tabulated-list-format width)))
      (should (= (hermes-test--tabulated-list-format-total-width boards)
                 width))
      (should (<= (hermes-test--tabulated-list-format-total-width tasks)
                  width))
      (should (equal (car (aref boards 0)) ""))
      (should (equal (car (aref boards 1)) "📋"))
      (should (equal (car (aref boards 3)) "📝"))
      (should (>= (cadr (aref boards 0)) 1))
      (should (>= (cadr (aref boards 1)) 1))
      (should (>= (cadr (aref tasks 0)) 1))
      (should (>= (cadr (aref tasks 3)) 1))
      (should (<= (cadr (aref tasks 3))
                  hermes-kanban--task-title-column-max-width))
      (when (>= width 50)
        (should (>= (cadr (aref boards 1)) 12))
        (should (>= (cadr (aref boards 3)) 4))
        (should (>= (cadr (aref tasks 0)) 6))
        (should (>= (cadr (aref tasks 2)) 10))
        (should (>= (cadr (aref tasks 3)) 20)))))
  (let ((narrow-boards (hermes-kanban--boards-tabulated-list-format 50))
        (wide-boards (hermes-kanban--boards-tabulated-list-format 120))
        (narrow-tasks (hermes-kanban--tasks-tabulated-list-format 50))
        (wide-tasks (hermes-kanban--tasks-tabulated-list-format 120)))
    (should (< (cadr (aref narrow-boards 1))
               (cadr (aref wide-boards 1))))
    (should (< (cadr (aref narrow-boards 3))
               (cadr (aref wide-boards 3))))
    (should (< (cadr (aref narrow-tasks 2))
               (cadr (aref wide-tasks 2))))
    (should (< (cadr (aref narrow-tasks 3))
               (cadr (aref wide-tasks 3))))
    (should (= (cadr (aref (hermes-kanban--tasks-tabulated-list-format 200) 3))
               hermes-kanban--task-title-column-max-width))))

(ert-deftest hermes-kanban-window-size-change-recomputes-format ()
  "Kanban tabulated-list modes recompute widths when their window resizes."
  (dolist (mode '(hermes-kanban-boards-mode hermes-kanban-mode))
    (with-temp-buffer
      (funcall mode)
      (let (printed)
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (_window &optional _pixelwise) 120))
                  ((symbol-function 'tabulated-list-print)
                   (lambda (&rest _) (setq printed t))))
          (setq tabulated-list-format
                (if (derived-mode-p 'hermes-kanban-boards-mode)
                    (hermes-kanban--boards-tabulated-list-format 50)
                  (hermes-kanban--tasks-tabulated-list-format 50)))
          (hermes-kanban--window-size-change 'fake-window)
          (should printed)
          (let ((total (hermes-test--tabulated-list-format-total-width
                        tabulated-list-format)))
            (if (derived-mode-p 'hermes-kanban-boards-mode)
                (should (= total 120))
              (should (<= total 120))
              (should (<= (cadr (aref tabulated-list-format 3))
                          hermes-kanban--task-title-column-max-width)))))))))

(ert-deftest hermes-kanban-board-rows-from-boards ()
  "Board rows map name/total/per-status counts and mark the current board."
  (cl-labels ((column-for (status)
                (+ 3 (cl-position status hermes-kanban--board-count-statuses
                                  :test #'equal))))
    (let* ((rows (hermes-kanban--board-rows
                  '(((slug . "emacs-lisp") (name . "Emacs Lisp")
                     (is_current . t) (total . 4)
                     (counts . ((todo . 1) (running . 2)
                                (archived . 1)))))))
           (entry (cadr (car rows))))
      (should (equal (caar rows) (cons "emacs-lisp" "Emacs Lisp")))
      (should (equal (aref entry 0) "📍"))
      (should (equal (aref entry 1) "Emacs Lisp"))
      (should (equal (aref entry 2) "4"))
      (should (equal (aref entry (column-for "todo")) "1"))
      (should (equal (aref entry (column-for "running")) "2"))
      (should (equal (aref entry (column-for "archived")) "1")))))

(ert-deftest hermes-kanban-task-rows-from-columns ()
  "Task rows flatten dashboard status columns into status/pri/assignee/title."
  (let* ((title "Do thing with a long title that tabulated-list truncates by column")
         (rows (hermes-kanban--task-rows
                `(((name . "todo")
                   (tasks . (((id . "t1") (status . "todo") (priority . 2)
                              (assignee . "elisp-dev") (title . ,title)))))
                  ((name . "running") (tasks . nil))))))
    (should (equal (caar rows) "t1"))
    (should (= (length rows) 1))
    (let ((status (aref (cadr (car rows)) 0)))
      (should (equal (substring-no-properties status) "📝"))
      (should (equal (get-text-property 0 'hermes-kanban-status status)
                     "todo")))
    (should (equal (aref (cadr (car rows)) 1) "2"))
    (should (equal (aref (cadr (car rows)) 2) "elisp-dev"))
    (should (equal (aref (cadr (car rows)) 3) title))))

(ert-deftest hermes-kanban-render-boards-lists-boards ()
  "The boards overview fetches /boards and renders one row per board."
  (cl-letf (((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body _query)
               (should (equal method "GET"))
               (should (equal path "/boards"))
               '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                             (is_current . t) (total . 1)
                             (counts . ((ready . 1))))))))))
    (unwind-protect
        (progn
          (hermes-list-kanban)
          (with-current-buffer "*Hermes Kanban Boards*"
            (should (derived-mode-p 'hermes-kanban-boards-mode))
            (should (equal (car (aref tabulated-list-format 1)) "📋"))
            (should (equal (caar tabulated-list-entries)
                           (cons "emacs-lisp" "Emacs Lisp")))))
      (when (get-buffer "*Hermes Kanban Boards*")
        (kill-buffer "*Hermes Kanban Boards*")))))

(ert-deftest hermes-kanban-board-actions-dispatch-rest-calls ()
  "Board overview actions use REST endpoints, safe archive, and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (pcase path
                   ("/boards"
                    '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                                  (is_current . t) (total . 1)
                                  (counts . ((ready . 1))))))))
                   ("/boards/emacs-lisp/switch" '((current . "emacs-lisp")))
                   ("/boards/emacs-lisp" '((board . ((slug . "emacs-lisp")
                                                      (name . "Renamed")))))
                   (_ (error "unexpected path: %s" path)))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (hermes-kanban-switch-board)
              (goto-char (point-min))
              (hermes-kanban-rename-board " Renamed ")
              (goto-char (point-min))
              (hermes-kanban-archive-board))
            (should (member '("POST" "/boards/emacs-lisp/switch" nil nil)
                            calls))
            (should (member '("PATCH" "/boards/emacs-lisp"
                              ((name . "Renamed")) nil)
                            calls))
            (should (member '("DELETE" "/boards/emacs-lisp" nil nil)
                            calls))
            (should (= (cl-count-if (lambda (call)
                                      (equal (cadr call) "/boards"))
                                    calls)
                       4))
            (should (= (length prompts) 2))
            (should (cl-some (lambda (prompt)
                               (string-match-p "current board" prompt))
                             prompts))
            (should (cl-some (lambda (prompt)
                               (string-match-p "hard delete" prompt))
                             prompts)))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-rename-board-rejects-blank-name ()
  "Whitespace-only board renames signal before PATCH or refresh."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                               (is_current . t) (total . 1)))))))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-rename-board "   ")
                            :type 'user-error))
            (should-not calls))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-current-board-cancel-stops-before-delete ()
  "Declining the current-board archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                               (is_current . t) (total . 1)))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-error (hermes-kanban-archive-board)
                            :type 'user-error))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "current board" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-default-board-stops-before-prompt ()
  "The protected default board is rejected before prompts or DELETE."
  (let (calls prompted)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 '((boards . (((slug . "default") (name . "Default")
                               (is_current . t) (total . 1)))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq prompted t)
                 t))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (let ((err (should-error (hermes-kanban-archive-board)
                                       :type 'user-error)))
                (should (string-match-p "protected.*cannot be archived"
                                        (error-message-string err)))))
            (should-not calls)
            (should-not prompted))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-archive-board-cancel-skips-delete ()
  "Declining the normal archive prompt skips DELETE and refresh."
  (let (calls prompts)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional body query)
                 (push (list method path body query) calls)
                 (should (equal path "/boards"))
                 '((boards . (((slug . "emacs-lisp") (name . "Emacs Lisp")
                               (total . 1)))))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (hermes-kanban--render-boards)
            (setq calls nil)
            (with-current-buffer "*Hermes Kanban Boards*"
              (goto-char (point-min))
              (should-not (hermes-kanban-archive-board)))
            (should-not calls)
            (should (= (length prompts) 1))
            (should (string-match-p "hard delete" (car prompts))))
        (when (get-buffer "*Hermes Kanban Boards*")
          (kill-buffer "*Hermes Kanban Boards*"))))))

(ert-deftest hermes-kanban-open-board-renders-tasks ()
  "Opening a board fetches /board with its slug and flattens the columns."
  (cl-letf (((symbol-function 'window-body-width)
             (lambda (&optional _window _pixelwise) 80))
            ((symbol-function 'hermes-kanban--api)
             (lambda (method path &optional _body query)
               (should (equal method "GET"))
               (should (equal path "/board"))
               (should (equal (cdr (assq 'board query)) "emacs-lisp"))
               '((columns . (((name . "todo")
                              (tasks . (((id . "t1") (status . "todo")
                                         (title . "Do thing")))))))
                 (assignees . ("elisp-dev"))))))
    (unwind-protect
        (progn
          (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
          (with-current-buffer "*Hermes Kanban*"
            (should (derived-mode-p 'hermes-kanban-mode))
            (should (equal hermes-kanban--slug "emacs-lisp"))
            (should (equal hermes-kanban--assignees '("elisp-dev")))
            (should (= (hermes-test--tabulated-list-format-total-width
                        tabulated-list-format)
                       80))
            (should (>= (cadr (aref tabulated-list-format 0)) 6))
            (should (>= (cadr (aref tabulated-list-format 3)) 20))
            (should (equal (caar tabulated-list-entries) "t1"))))
      (when (get-buffer "*Hermes Kanban*") (kill-buffer "*Hermes Kanban*")))))

(ert-deftest hermes-kanban-show-fetches-task-at-point ()
  "Showing fetches the task on the current row and renders its body."
  (let (show-path)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (_method path &optional _body _query)
                 (cond
                  ((equal path "/board")
                   '((columns . (((name . "todo")
                                  (tasks . (((id . "t1") (status . "todo")
                                             (title . "Do thing")))))))
                     (assignees)))
                  (t (setq show-path path)
                     '((task . ((id . "t1") (title . "Do thing") (status . "todo")
                                (body . "details here")))))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show))
            (should (equal show-path "/tasks/t1"))
            (with-current-buffer "*Hermes Kanban Task*"
              (should (derived-mode-p 'hermes-kanban-task-mode))
              (when (require 'markdown-mode nil t)
                (should (derived-mode-p 'markdown-mode)))
              (should buffer-read-only)
              (should (equal hermes-kanban-task--task-id "t1"))
              (should (string-match-p "## Description" (buffer-string)))
              (should (string-match-p "details here" (buffer-string)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Task*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-format-task-detail-renders-markdown-sections ()
  "Task detail formatting includes Markdown task sections and rows."
  (let* ((payload
          '((task . ((id . "t1") (title . "Do thing") (status . "running")
                     (priority . 5) (assignee . "elisp-dev")
                     (created_at . 1700000000)
                     (body . "details here")
                     (diagnostics . (((kind . "stale_running")
                                      (severity . "error")
                                      (title . "Stale worker")
                                      (detail . "No heartbeat")
                                      (count . 2)
                                      (run_id . 7)
                                      (actions . (((kind . "reclaim")
                                                   (label . "Reclaim")
                                                   (suggested . t)))))))))
            (comments . (((id . 1) (author . "thanos")
                          (body . "needs eyes") (created_at . 1700000010))))
            (events . (((id . 2) (kind . "claimed")
                        (created_at . 1700000020)
                        (payload . ((run_id . 7))))))
            (attachments . (((id . 5) (filename . "report.txt")
                             (content_type . "text/plain") (size . 42)
                             (uploaded_by . "dashboard")
                             (stored_path . "/tmp/report.txt"))))
            (runs . (((id . 7) (profile . "elisp-dev")
                      (status . "finished") (outcome . "blocked")
                      (worker_pid . 1234)
                      (started_at . 1700000000) (ended_at . 1700000060)
                      (summary . "needs review")
                      (metadata . ((tests . 3)))
                      (error . "review-required"))))))
         (text (hermes-kanban--format-task-detail payload)))
    (should (string-match-p (regexp-quote "# Do thing") text))
    (should (string-match-p (regexp-quote "- Status: `⚙️ running`") text))
    (should (string-match-p "## Description" text))
    (should (string-match-p "## Run history (1)" text))
    (should (string-match-p (regexp-quote "### Run #7 — blocked @elisp-dev") text))
    (should (string-match-p "needs review" text))
    (should (string-match-p "tests" text))
    (should (string-match-p "## Diagnostics (1)" text))
    (should (string-match-p (regexp-quote "### [error] stale_running: Stale worker") text))
    (should (string-match-p "Reclaim" text))
    (should (string-match-p "## Attachments (1)" text))
    (should (string-match-p (regexp-quote "### report.txt (#5) (42 B)") text))
    (should (string-match-p "/tmp/report.txt" text))
    (should (string-match-p "## Comments (1)" text))
    (should (string-match-p (regexp-quote "— thanos") text))
    (should (string-match-p "needs eyes" text))
    (should (string-match-p "## Events (1)" text))
    (should (string-match-p (regexp-quote "— claimed") text))
    (should (string-match-p "Payload:" text))))

(ert-deftest hermes-kanban-format-task-detail-renders-empty-states ()
  "Task detail formatting names empty Markdown sections instead of omitting them."
  (let ((text (hermes-kanban--format-task-detail
               '((task . ((id . "t-empty") (title . "Empty task")
                          (status . "todo") (body . "")))
                 (comments) (events) (attachments) (runs)))))
    (should (string-match-p "## Diagnostics (0)" text))
    (should (string-match-p "— no diagnostics —" text))
    (should (string-match-p "## Attachments (0)" text))
    (should (string-match-p "— no attachments —" text))
    (should (string-match-p "## Comments (0)" text))
    (should (string-match-p "— no comments —" text))
    (should (string-match-p "## Events (0)" text))
    (should (string-match-p "— no events —" text))
    (should (string-match-p "## Run history (0)" text))
    (should (string-match-p "— no runs —" text))))

(ert-deftest hermes-kanban-show-log-fetches-selected-task-log ()
  "Log viewing goes through the dashboard REST endpoint for the selected task."
  (let (log-path log-query)
    (cl-letf (((symbol-function 'hermes-kanban--api)
               (lambda (method path &optional _body query)
                 (should (equal method "GET"))
                 (cond
                  ((equal path "/board")
                   '((columns . (((name . "running")
                                  (tasks . (((id . "t1") (status . "running")
                                             (title . "Do thing")))))))
                     (assignees . ("elisp-dev"))))
                  ((equal path "/tasks/t1/log")
                   (setq log-path path
                         log-query query)
                   '((task_id . "t1") (path . "/logs/t1.log")
                     (exists . t) (size_bytes . 12)
                     (content . "hello from worker\n")
                     (truncated . :json-false)))
                  (t (error "unexpected path: %s" path))))))
      (unwind-protect
          (progn
            (hermes-kanban--render-board "emacs-lisp" "Emacs Lisp")
            (with-current-buffer "*Hermes Kanban*"
              (goto-char (point-min))
              (hermes-kanban-show-log))
            (should (equal log-path "/tasks/t1/log"))
            (should (equal (cdr (assq 'board log-query)) "emacs-lisp"))
            (should (equal (cdr (assq 'tail log-query)) 100000))
            (with-current-buffer "*Hermes Kanban Log*"
              (should (derived-mode-p 'hermes-kanban-log-mode))
              (should (equal hermes-kanban-log--task-id "t1"))
              (should (equal hermes-kanban-log--board-slug "emacs-lisp"))
              (let ((text (buffer-string)))
                (should (string-match-p "Worker log for t1" text))
                (should (string-match-p "/logs/t1.log" text))
                (should (string-match-p "hello from worker" text)))))
        (dolist (b '("*Hermes Kanban*" "*Hermes Kanban Log*"))
          (when (get-buffer b) (kill-buffer b)))))))

(ert-deftest hermes-kanban-format-log-renders-empty-and-error-states ()
  "Worker log formatting is explicit when the backend reports no log or an error."
  (should (string-match-p "no worker log"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (exists . :json-false)
                             (content . "")))))
  (should (string-match-p "failed to load worker log: boom"
                          (hermes-kanban--format-log
                           '((task_id . "t1") (error . "boom"))))))

(ert-deftest hermes-kanban-format-log-sanitizes-control-output ()
  "Worker log formatting renders CR and ANSI control output readably."
  (let* ((text (hermes-kanban--format-log
                `((task_id . "t1") (exists . t)
                  (content . ,(concat "start\rprogress\r\ndone\n"
                                      "\33[31merror\33[0m\n")))))
         (plain (substring-no-properties text)))
    (should-not (string-match-p "\r" plain))
    (should-not (string-match-p (regexp-quote "\33[") plain))
    (should (string-match-p "start\nprogress\ndone" plain))
    (should (string-match-p "error" plain))))

(ert-deftest hermes-mcp-rows-parse-read-only-server-response ()
  "MCP rows show backend name, type, enabled state, status, and tool count."
  (let ((rows (hermes-mcp--rows
               '((servers . (((name . "ctx") (transport . "stdio")
                              (enabled . t) (tool_count . 7))
                             ((name . "http-srv") (type . "http")
                              (enabled . nil) (tools . ("a" "b")))
                             ((name . "unknown") (status . "connecting"))))))))
    (should (equal (mapcar #'car rows) '("ctx" "http-srv" "unknown")))
    (should (equal (aref (cadr (car rows)) 1) "stdio"))
    (should (equal (aref (cadr (car rows)) 2) "on"))
    (should (equal (aref (cadr (car rows)) 3) "configured"))
    (should (equal (aref (cadr (car rows)) 4) "7"))
    (should (equal (aref (cadr (nth 1 rows)) 1) "http"))
    (should (equal (aref (cadr (nth 1 rows)) 2) "off"))
    (should (equal (aref (cadr (nth 1 rows)) 3) "disabled"))
    (should (equal (aref (cadr (nth 1 rows)) 4) "2"))
    (should (equal (aref (cadr (nth 2 rows)) 2) "?"))
    (should (equal (aref (cadr (nth 2 rows)) 3) "connecting"))))

(ert-deftest hermes-mcp-rows-redact-secret-shaped-display-values ()
  "MCP row display cells do not leak secret-shaped backend values."
  (let* ((secret "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (rows (hermes-mcp--rows
                `((servers . (((name . ,(concat "srv-" secret))
                               (transport . "stdio")
                               (enabled . t)
                               (status . ,(concat "failed token=" secret))
                               (tool_count . 1)))))))
         (entry (cadr (car rows)))
         (display (string-join (mapcar (lambda (cell) (format "%s" cell))
                                       (append entry nil))
                               " ")))
    (should-not (string-match-p (regexp-quote secret) display))
    (should (string-match-p "<redacted>" display))))

(ert-deftest hermes-mcp-test-and-toggle-dispatch-rest-actions ()
  "Testing and toggling dispatch to MCP dashboard REST endpoints."
  (let (calls messages)
    (cl-letf (((symbol-function 'hermes-mcp--with-client)
               (lambda (fn) (funcall fn 'fake-client)))
              ((symbol-function 'hermes-mcp--api)
               (lambda (method path &optional body query &rest _args)
                 (push (list method path body query) calls)
                 (cond
                  ((equal path "/servers")
                   '((servers . (((name . "ctx") (transport . "stdio")
                                  (enabled . t) (tool_count . 0))))))
                  ((equal path "/servers/ctx/test")
                   '((ok . t) (tools . (((name . "read")
                                         (description . "Read"))))))
                  ((equal path "/servers/ctx/enabled")
                   (should (equal body '((enabled . :false))))
                   '((ok . t) (name . "ctx") (enabled . nil)))
                  (t (error "unexpected MCP API call %S" path)))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-mcp)
            (with-current-buffer "*Hermes MCP Servers*"
              (should (derived-mode-p 'hermes-mcp-mode))
              (should (eq (keymap-lookup hermes-mcp-mode-map "RET")
                          #'hermes-mcp-test))
              (should (eq (keymap-lookup hermes-mcp-mode-map "e")
                          #'hermes-mcp-toggle))
              (should (equal (caar tabulated-list-entries) "ctx"))
              (goto-char (point-min))
              (hermes-mcp-test)
              (should (equal (hermes-mcp--test-tool-count "ctx") "1"))
              (should (equal (aref (cadr (car tabulated-list-entries)) 3)
                             "ok"))
              (should (equal (aref (cadr (car tabulated-list-entries)) 4)
                             "1"))
              (goto-char (point-min))
              (hermes-mcp-toggle))
            (should (member '("POST" "/servers/ctx/test" nil nil) calls))
            (should (member '("PUT" "/servers/ctx/enabled"
                              ((enabled . :false)) nil)
                            calls))
            (should (= (cl-count-if (lambda (call)
                                      (equal (cadr call) "/servers"))
                                    calls)
                       3))
            (should (cl-some (lambda (message)
                               (string-match-p "ctx has 1 MCP tool" message))
                             messages))
            (should (cl-some (lambda (message)
                               (string-match-p "disabled ctx" message))
                             messages)))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-test-failure-message-redacts-secret ()
  "MCP test failure messages redact secret-shaped backend errors."
  (let ((secret "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        messages)
    (cl-letf (((symbol-function 'hermes-mcp--with-client)
               (lambda (fn) (funcall fn 'fake-client)))
              ((symbol-function 'hermes-mcp--api)
               (lambda (_method path &optional _body _query &rest _args)
                 (cond
                  ((equal path "/servers")
                   '((servers . (((name . "ctx") (transport . "stdio")
                                  (enabled . t))))))
                  ((equal path "/servers/ctx/test")
                   `((ok . nil) (error . ,(concat "failed token=" secret))
                     (tools . nil)))
                  (t (error "unexpected MCP API call %S" path)))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (unwind-protect
          (progn
            (hermes-list-mcp)
            (with-current-buffer "*Hermes MCP Servers*"
              (goto-char (point-min))
              (hermes-mcp-test))
            (let ((joined (string-join messages "\n")))
              (should-not (string-match-p (regexp-quote secret) joined))
              (should (string-match-p "test failed" joined))
              (should (string-match-p "<redacted>" joined))))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-action-reports-unsupported-backend ()
  "MCP actions surface unsupported REST backends clearly."
  (let (called)
    (cl-letf (((symbol-function 'hermes-mcp--with-client)
               (lambda (fn) (funcall fn 'fake-client)))
              ((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (method path &rest _args)
                 (setq called (list method path))
                 (user-error
                  "Hermes dashboard request failed at /api/mcp/servers/ctx/test (HTTP 404)"))))
      (unwind-protect
          (progn
            (hermes-mcp--render
             '((servers . (((name . "ctx") (transport . "stdio")
                            (enabled . t))))))
            (with-current-buffer "*Hermes MCP Servers*"
              (goto-char (point-min))
              (let ((error (should-error (hermes-mcp-test) :type 'user-error)))
                (should (equal called '("POST" "/api/mcp/servers/ctx/test")))
                (should (string-match-p "MCP REST API is unavailable"
                                        (error-message-string error))))))
        (when (get-buffer "*Hermes MCP Servers*")
          (kill-buffer "*Hermes MCP Servers*"))))))

(ert-deftest hermes-mcp-api-uses-live-client-session-token ()
  "MCP REST requests use a live dashboard client's session token when present."
  (let ((client (make-hermes-dashboard-transport-client
                 :host "127.0.0.1" :port 32123 :token "session-token"))
        seen-url seen-method seen-headers seen-secrets)
    (cl-letf (((symbol-function 'hermes-dashboard-transport--http-json)
               (cl-function
                (lambda (url &key method headers body secrets)
                 (ignore body)
                 (setq seen-url url
                       seen-method method
                       seen-headers headers
                       seen-secrets secrets)
                 '(:body ((servers . nil)))))))
      (should (equal (hermes-mcp--api "GET" "/servers" nil '((profile . "work"))
                                      :client client)
                     '((servers . nil))))
      (should (equal seen-method "GET"))
      (should (string-match-p (regexp-quote "/api/mcp/servers?profile=work")
                              seen-url))
      (should (equal (cdr (assoc "X-Hermes-Session-Token" seen-headers))
                     "session-token"))
      (should (member "session-token" seen-secrets)))))

(ert-deftest hermes-mcp-api-redacts-secret-shaped-errors ()
  "MCP API errors do not leak token, ticket, internal, or env secrets."
  (let ((secret "sk-test-secret"))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request)
               (lambda (_method _path &rest args)
                 (signal 'user-error
                         (list (hermes-dashboard-transport--redact-secret
                                (format "bad token=%s env SECRET=%s" secret secret)
                                (plist-get args :secrets)))))))
      (should-error (hermes-mcp--api "GET" "/servers" nil nil
                                      :secrets (list secret))
                    :type 'user-error)
      (condition-case err
          (hermes-mcp--api "GET" "/servers" nil nil :secrets (list secret))
        (user-error
         (let ((message (error-message-string err)))
           (should-not (string-match-p (regexp-quote secret) message))
           (should (string-match-p "<redacted>" message))))))))

(ert-deftest hermes-mcp-api-reports-unsupported-backend ()
  "A missing MCP REST endpoint is reported as an unsupported backend."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request)
             (lambda (&rest _)
               (user-error "Hermes dashboard request failed at /api/mcp/servers (HTTP 404)"))))
    (should-error (hermes-mcp--api "GET" "/servers") :type 'user-error)
    (condition-case err
        (hermes-mcp--api "GET" "/servers")
      (user-error
       (should (string-match-p "MCP REST API is unavailable"
                               (error-message-string err)))))))

(provide 'hermes-tests)
;;; hermes-tests.el ends here
