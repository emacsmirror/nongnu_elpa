;;; hermes-chat-tests.el --- chat tests for hermes-el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'delsel)
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

(ert-deftest hermes-chat-quote-region-preserves-text-draft-and-busy-state ()
  (dolist (draft '("" "draft" "draft\n" "draft\n\n"))
    (let ((transient-mark-mode t))
      (hermes-test-with-chat-buffer
       (insert draft)
       (save-excursion
         (goto-char (point-min))
         (let ((inhibit-read-only t) (buffer-undo-list t))
           (insert-before-markers
            (propertize "α\n\n```elisp\n  (+ 1 2)\n```\n" 'face 'bold
                        'display "hidden" 'keymap (make-sparse-keymap))))
         (hermes-chat--protect-transcript))
       (goto-char (point-min))
       (search-forward "α")
       (let ((start (1- (point))))
         (search-forward "```elisp\n  (+ 1 2)\n```\n")
         (push-mark start t t))
       (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker))
             (hermes-chat--dashboard-running-p t)
             (hermes-chat--queued-messages '(existing)))
         (cl-letf (((symbol-function 'hermes-chat-send) (lambda (&rest _) (ert-fail "Sent quote")))
                   ((symbol-function 'hermes-chat-queue-message) (lambda (&rest _) (ert-fail "Queued quote"))))
           (call-interactively #'hermes-chat-quote-region))
         (should (equal (hermes-chat-input-string)
                        (concat (if (string-empty-p draft) "" "draft\n\n")
                                "> α\n> \n> ```elisp\n>   (+ 1 2)\n> ```\n> \n\n")))
         (should (= (point) (point-max)))
         (should-not mark-active)
         (should (equal-including-properties
                  (buffer-substring hermes-chat--input-marker (point-max))
                  (hermes-chat-input-string)))
         (should-not (text-property-not-all hermes-chat--input-marker (point-max) 'read-only nil))
         (should (equal hermes-chat--queued-messages '(existing)))
         (should hermes-chat--dashboard-running-p)
         (should (equal-including-properties transcript
                                             (buffer-substring (point-min) hermes-chat--input-marker))))))))

(ert-deftest hermes-chat-quote-region-rejects-invalid-selection ()
  (dolist (selection '(inactive empty composer crossing boundary-crossing missing-marker foreign-marker narrowed))
    (let ((transient-mark-mode t))
      (hermes-test-with-chat-buffer
       (insert "draft")
       (hermes-chat--insert-entry '(:id "quote" :role assistant :content "reply"))
       (goto-char (point-min))
       (push-mark (1+ (point)) t t)
       (pcase selection
         ('inactive (deactivate-mark))
         ('empty (set-mark (point)))
         ('composer (goto-char (point-max)) (set-mark hermes-chat--input-marker))
         ('crossing (set-mark (point-max)))
         ('boundary-crossing (goto-char hermes-chat--input-marker) (set-mark (point-max)))
         ('missing-marker (setq hermes-chat--input-marker nil))
         ('foreign-marker (setq hermes-chat--input-marker (with-temp-buffer (copy-marker 1))))
         ('narrowed (narrow-to-region 2 (point-max))))
       (let ((before (buffer-string)) (position (point)) (undo buffer-undo-list))
         (should-error (call-interactively #'hermes-chat-quote-region) :type 'user-error)
         (should (equal-including-properties before (buffer-string)))
         (should (= position (point)))
         (should (eq undo buffer-undo-list)))))))

(ert-deftest hermes-chat-quote-region-key-and-popup-edit-and-undo ()
  (dolist (keys '("r" "C-c C-o Q"))
    (let ((transient-mark-mode t)
          (undo-in-region nil)
          (last-command nil))
      (save-window-excursion
        (hermes-test-with-chat-buffer
         (switch-to-buffer (current-buffer))
         (insert "draft")
         (undo-boundary)
         (hermes-chat--insert-entry '(:id "quote" :role assistant :content "reply"))
         (goto-char (point-min))
         (search-forward "reply")
         (push-mark (- (point) 5) t t)
         (execute-kbd-macro (kbd keys))
         (undo-boundary)
         (should (equal (hermes-chat-input-string) "draft\n\n> reply\n\n"))
         (should (= (point) (point-max)))
         (should-not mark-active)
         (execute-kbd-macro (kbd "r"))
         (should (equal (hermes-chat-input-string) "draft\n\n> reply\n\nr"))
         (undo-boundary)
         (hermes-chat--insert-entry '(:id "later" :role assistant :content "stream"))
         (setq last-command nil)
         (hermes-test--draft-undo-command #'undo-only)
         (should (equal (hermes-chat-input-string) "draft\n\n> reply\n\n"))
         (hermes-test--draft-undo-command #'undo-only)
         (should (equal (hermes-chat-input-string) "draft")))))))

(ert-deftest hermes-chat-quote-region-accepts-end-at-input-boundary ()
  (let ((transient-mark-mode t))
    (hermes-test-with-chat-buffer
     (insert "draft")
     (hermes-chat--insert-entry '(:id "quote" :role assistant :content "reply"))
     (goto-char (point-min))
     (search-forward "reply")
     (let* ((start (- (point) 5))
            (end (marker-position hermes-chat--input-marker))
            (text (buffer-substring-no-properties start end)))
       (goto-char start)
       (push-mark end t t)
       (narrow-to-region start end)
       (call-interactively #'hermes-chat-quote-region)
       (should (= (point) (point-max)))
       (should (equal (hermes-chat-input-string)
                      (concat "draft\n\n"
                              (mapconcat (lambda (line) (concat "> " line))
                                         (split-string text "\n" nil) "\n")
                              "\n\n")))))))

(ert-deftest hermes-chat-quote-region-r-retains-native-selection-editing ()
  (let ((transient-mark-mode t))
    (save-window-excursion
      (hermes-test-with-chat-buffer
       (switch-to-buffer (current-buffer))
       (insert "draft")
       (goto-char hermes-chat--input-marker)
       (should (eq (key-binding (kbd "r")) #'self-insert-command))
       (execute-kbd-macro (kbd "r"))
       (should (equal (hermes-chat-input-string) "rdraft"))
       (goto-char (point-max))
       (push-mark hermes-chat--input-marker t t)
       (let ((delete-selection-mode t))
         (add-hook 'pre-command-hook #'delete-selection-pre-hook nil t)
         (execute-kbd-macro (kbd "r")))
       (should (equal (hermes-chat-input-string) "r"))
       (should (eq (lookup-key hermes-chat-actions-map (kbd "r"))
                   #'hermes-chat-refresh-commands))))))

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

(ert-deftest hermes-chat-old-lifetime-bare-callbacks-ignore-reentered-buffer ()
  "Bare callbacks from lifetime A cannot mutate same-buffer lifetime B."
  (let ((client (hermes-test--dashboard-client))
        catalog-resolve stop-resolve background-resolve model-rejects model-resolves provider-candidate)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'hermes-dashboard-transport-commands-catalog)
               (lambda (_client &rest args) (setq catalog-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-process-stop)
               (lambda (_client &rest args) (setq stop-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-prompt-background)
               (lambda (_client _content &rest args) (setq background-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (push (plist-get args :resolve) model-resolves) (push (plist-get args :reject) model-rejects)))
              ((symbol-function 'hermes-onboarding--choose-provider)
               (lambda (_result) (fundamental-mode) (hermes-chat-mode) '((slug . "old"))))
              ((symbol-function 'hermes-chat--connect-provider-candidate)
               (lambda (&rest _) (setq provider-candidate t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "old-session" hermes-chat--dashboard-session-ready-p t
             hermes-chat--lifecycle-generation 2
             hermes-chat--lifetime-sequence 0)
       (hermes-chat--fetch-commands-catalog)
       (hermes-chat-stop-processes)
       (hermes-chat--background-submit "old task" (current-buffer))
       (hermes-chat-switch-model)
       (hermes-chat-connect-provider)
       (should (cl-every #'functionp
                         (append (list catalog-resolve stop-resolve background-resolve)
                                 model-rejects model-resolves)))
       (funcall (car model-resolves) nil)
       (should-not provider-candidate)
       (fundamental-mode)
       (hermes-chat-mode)
       (setq hermes-chat--commands-cache '(("new" . "current"))
             hermes-chat--dashboard-client 'new-client
             hermes-chat--dashboard-active-session-id "new-session"
             hermes-chat--queued-messages '((:id "new-queue"))
             hermes-chat--background-counter 7)
       (funcall catalog-resolve '((pairs . (("/old" "stale")))))
       (funcall stop-resolve '((killed . 3)))
       (funcall background-resolve '((task_id . "old-task")))
       (mapc (lambda (reject) (funcall reject "stale-model-error")) model-rejects)
       (mapc (lambda (resolve) (funcall resolve nil)) model-resolves)
       (should (equal hermes-chat--commands-cache '(("new" . "current"))))
       (should (eq hermes-chat--dashboard-client 'new-client))
       (should (equal hermes-chat--dashboard-active-session-id "new-session"))
       (should (equal hermes-chat--queued-messages '((:id "new-queue"))))
       (should (and (= hermes-chat--background-counter 7) (null (hermes-chat--entries))))))))

(ert-deftest hermes-chat-mode-exit-and-kill-release-resources-once ()
  "Mode exit and kill each release requests, subscriber, and client once."
  (dolist (exit '(mode-change kill))
    (let ((buffer (generate-new-buffer " *hermes-chat-cleanup*"))
          (client 'owned-client)
          (token 'owned-subscriber)
          cancel unsubscribe release lifetime-at-cancel)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-cancel-owner-requests)
                 (lambda (_client _owner)
                   (setq cancel (1+ (or cancel 0))
                         lifetime-at-cancel hermes-chat--lifecycle-generation)))
                ((symbol-function 'hermes-dashboard-transport-unsubscribe)
                 (lambda (_client _token)
                   (setq unsubscribe (1+ (or unsubscribe 0)))))
                ((symbol-function 'hermes-dashboard-transport-release)
                 (lambda (_client)
                   (setq release (1+ (or release 0))))))
        (unwind-protect
            (with-current-buffer buffer
              (hermes-chat-mode)
              (let ((owned-lifetime hermes-chat--lifecycle-generation))
                (setq hermes-chat--dashboard-client client
                      hermes-chat--dashboard-token token
                      hermes-chat--process client)
                (if (eq exit 'kill)
                    (kill-buffer buffer)
                  (fundamental-mode)
                  (kill-buffer buffer))
                (should-not (equal lifetime-at-cancel owned-lifetime))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (should (equal (list cancel unsubscribe release) '(1 1 1)))))))

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
      (should (eq (hermes-chat--header-status-face status)
                  (cond ((equal label "Running") 'font-lock-keyword-face)
                        ((equal label "Approval requested") 'warning)
                        (t face))))
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

(defun hermes-test--draft-undo-command (command)
  "Run undo COMMAND with the relevant command-loop bookkeeping."
  (let ((this-command command))
    (funcall command 1)
    (setq last-command this-command))
  (undo-boundary))

(ert-deftest hermes-chat-draft-undo-survives-transcript-mutations ()
  "Transcript changes preserve draft edits and native pending undo/redo."
  (dolist (mutation '(insert grow shrink remove expand collapse))
    (dolist (pending '(nil undo redo))
      (ert-info ((format "mutation=%s pending=%s" mutation pending))
        (let ((undo-in-region nil)
              (last-command nil)
              (pending-undo-list nil)
              (undo-equiv-table (make-hash-table :test #'eq)))
          (hermes-test-with-chat-buffer
           (hermes-chat--insert-entry
            (hermes-chat--make-entry 'tool "first\nsecond\nthird" 'done "entry"))
           (when (eq mutation 'collapse)
             (hermes-chat--toggle-entry-expanded "entry"))
           (setq buffer-undo-list nil)
           (insert "draft α\n")
           (undo-boundary)
           (insert "tail")
           (undo-boundary)
           ;; A deletion exercises signed string-position records as well
           ;; as insertion ranges and native redo equivalence links.
           (delete-char -2)
           (undo-boundary)
           (when pending
             (hermes-test--draft-undo-command #'undo-only)
             (should (equal (hermes-chat-input-string) "draft α\ntail")))
           (when (eq pending 'redo)
             (hermes-test--draft-undo-command #'undo-redo)
             (should (equal (hermes-chat-input-string) "draft α\nta")))
           (pcase mutation
             ('insert
              (hermes-chat--insert-entry
               (hermes-chat--make-entry 'assistant "New streamed answer" 'streaming "new")))
             ('grow
              (hermes-chat--update-entry
               "entry" (lambda (entry)
                         (hermes-chat--entry-with entry :content "A much longer first line\nmore"))))
             ('shrink
              (hermes-chat--update-entry
               "entry" (lambda (entry)
                         (hermes-chat--entry-with entry :content "x"))))
             ('remove (hermes-chat--remove-entry "entry"))
             (_ (hermes-chat--toggle-entry-expanded "entry")))
           (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker))
                 (entries (copy-tree (hermes-chat--entries))))
             (unless (eq pending 'undo)
               (hermes-test--draft-undo-command #'undo-only)
               (should (equal (hermes-chat-input-string) "draft α\ntail")))
             (hermes-test--draft-undo-command #'undo-only)
             (should (equal (hermes-chat-input-string) "draft α\n"))
             (hermes-test--draft-undo-command #'undo-only)
             (should (equal (hermes-chat-input-string) ""))
             (dolist (draft '("draft α\n" "draft α\ntail" "draft α\nta"))
               (hermes-test--draft-undo-command #'undo-redo)
               (should (equal (hermes-chat-input-string) draft)))
             (should (equal-including-properties
                      transcript (buffer-substring (point-min) hermes-chat--input-marker)))
             (should (equal entries (hermes-chat--entries)))
             (should-not (text-property-not-all (point-min) hermes-chat--input-marker 'read-only t))
             (should-not (get-text-property hermes-chat--input-marker 'read-only))
             (should (<= hermes-chat--input-marker (point)))
             (insert "!")
             (should (equal (hermes-chat-input-string) "draft α\nta!")))))))))

(ert-deftest hermes-chat-narrowed-transcript-mutations ()
  "Transcript edits preserve a narrowed draft, point, protection and undo."
  (dolist (bounds '((0 . 7) (1 . 6)))
    (dolist (mutation '(insert grow shrink remove))
      (ert-info ((format "bounds=%S mutation=%s" bounds mutation))
        (let ((last-command nil)
              (undo-in-region nil)
              (undo-equiv-table (make-hash-table :test #'eq)))
          (hermes-test-with-chat-buffer
           (hermes-chat--insert-entry
            (hermes-chat--make-entry 'assistant "old transcript" 'streaming "reply"))
           (setq buffer-undo-list nil)
           (insert "draft α")
           (undo-boundary)
           (narrow-to-region (+ hermes-chat--input-marker (car bounds))
                             (+ hermes-chat--input-marker (cdr bounds)))
           (goto-char (+ hermes-chat--input-marker 3))
           (pcase mutation
             ('insert
              (hermes-chat--insert-entry
               (hermes-chat--make-entry 'assistant "new" 'streaming "new")
               (gethash "reply" hermes-chat--nodes)))
             ('grow
              (hermes-chat--update-entry
               "reply" (lambda (entry)
                         (hermes-chat--entry-with entry :content "a longer streamed transcript"))))
             ('shrink
              (hermes-chat--update-entry
               "reply" (lambda (entry) (hermes-chat--entry-with entry :content "x"))))
             ('remove (hermes-chat--remove-entry "reply")))
           (should (buffer-narrowed-p))
           (should (= (point-min) (+ hermes-chat--input-marker (car bounds))))
           (should (= (point-max) (+ hermes-chat--input-marker (cdr bounds))))
           (should (= (point) (+ hermes-chat--input-marker 3)))
           (save-restriction
             (widen)
             (should (equal (hermes-chat-input-string) "draft α"))
             (let* ((contents (mapcar (lambda (entry) (plist-get entry :content))
                                      (hermes-chat--entries)))
                    (expected (pcase mutation
                                ('insert '("new" "old transcript"))
                                ('grow '("a longer streamed transcript"))
                                ('shrink '("x")))))
               (should (equal contents expected))
               (should (= (hash-table-count hermes-chat--nodes) (length expected)))
               (should (equal (buffer-substring-no-properties
                               (point-min) hermes-chat--input-marker)
                              (concat (mapconcat (lambda (text) (concat text "\n"))
                                                 expected "")
                                      "\n \n"))))
             (should-not (text-property-not-all
                          (point-min) hermes-chat--input-marker 'read-only t))
             (should-not (get-text-property hermes-chat--input-marker 'read-only)))
           ;; Undo the whole draft with its exact composer boundary accessible.
           (widen)
           (narrow-to-region hermes-chat--input-marker (point-max))
           (hermes-test--draft-undo-command #'undo-only)
           (should (equal (buffer-string) ""))
           (hermes-test--draft-undo-command #'undo-redo)
           (should (equal (buffer-string) "draft α"))))))))

(ert-deftest hermes-chat-narrowed-empty-composer-streaming ()
  "An empty composer remains empty as its preceding transcript is replaced."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry
    (hermes-chat--make-entry 'assistant "old" 'streaming "reply"))
   (narrow-to-region hermes-chat--input-marker hermes-chat--input-marker)
   (dolist (content '("a longer answer" "x" ""))
     (hermes-chat--update-entry
      "reply" (lambda (entry) (hermes-chat--entry-with entry :content content)))
     (should (buffer-narrowed-p))
     (should (= (point-min) hermes-chat--input-marker))
     (should (= (point-max) hermes-chat--input-marker))
     (should (= (point) hermes-chat--input-marker)))
   (hermes-chat--remove-entry "reply")
   (hermes-chat--insert-entry
    (hermes-chat--make-entry 'assistant "new" 'streaming "new"))
   (should (= (point-min) hermes-chat--input-marker))
   (should (= (point-max) hermes-chat--input-marker))
   (insert "draft")
   (should (equal (buffer-string) "draft"))))

(ert-deftest hermes-chat-narrowed-mutation-error-restores-restriction ()
  "An error after transcript mutation still restores draft bounds and point."
  (hermes-test-with-chat-buffer
   (insert "draft")
   (narrow-to-region hermes-chat--input-marker (point-max))
   (goto-char (+ (point-min) 2))
   (should-error
    (hermes-chat--preserve-input-point
      (hermes-chat--insert-entry
       (hermes-chat--make-entry 'assistant "answer" 'streaming "reply"))
      (error "Mutation interrupted"))
    :type 'error)
   (should (buffer-narrowed-p))
   (should (= (point-min) hermes-chat--input-marker))
   (should (= (point) (+ (point-min) 2)))
   (should (equal (buffer-string) "draft"))
   (save-restriction
     (widen)
     (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                           (hermes-chat--entries))
                    '("answer")))
     (should-not (text-property-not-all
                  (point-min) hermes-chat--input-marker 'read-only t)))))

(ert-deftest hermes-chat-draft-grouped-undo-survives-streaming ()
  "Native combined edits retain their nested undo records across streaming."
  (let ((undo-in-region nil)
        (last-command nil)
        (pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq)))
    (hermes-test-with-chat-buffer
     (setq buffer-undo-list nil)
     (combine-change-calls (point) (point)
       (insert "draft")
       (put-text-property (- (point) 5) (point) 'face 'bold))
     (undo-boundary)
     (hermes-chat--insert-entry
      (hermes-chat--make-entry 'assistant "streamed" 'streaming "reply"))
     (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker)))
       (hermes-test--draft-undo-command #'undo-only)
       (should (equal (hermes-chat-input-string) ""))
       (should (equal-including-properties
                transcript (buffer-substring (point-min) hermes-chat--input-marker)))
       (hermes-chat--update-entry
        "reply" (lambda (entry) (hermes-chat--entry-with entry :content "x")))
       (setq transcript (buffer-substring (point-min) hermes-chat--input-marker))
       (hermes-test--draft-undo-command #'undo-redo)
       (should (equal (hermes-chat-input-string) "draft"))
       (should (eq (get-text-property hermes-chat--input-marker 'face) 'bold))
       (should (equal-including-properties
                transcript (buffer-substring (point-min) hermes-chat--input-marker)))))))

(ert-deftest hermes-chat-draft-selective-undo-survives-streaming ()
  "A pending native selective undo list follows the same moving draft."
  (let ((undo-in-region nil)
        (last-command nil)
        (pending-undo-list nil)
        (undo-equiv-table (make-hash-table :test #'eq))
        (transient-mark-mode t))
    (hermes-test-with-chat-buffer
     (setq buffer-undo-list nil)
     (insert "one")
     (undo-boundary)
     (insert "two")
     (undo-boundary)
     (push-mark hermes-chat--input-marker t t)
     (hermes-test--draft-undo-command #'undo-only)
     (should (equal (hermes-chat-input-string) "one"))
     (hermes-chat--insert-entry
      (hermes-chat--make-entry 'assistant "streamed answer" 'streaming "reply"))
     (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker)))
       (hermes-test--draft-undo-command #'undo-only)
       (should (equal (hermes-chat-input-string) ""))
       (should (equal-including-properties
                transcript (buffer-substring (point-min) hermes-chat--input-marker)))))))

(ert-deftest hermes-chat-draft-undo-reset-is-buffer-local ()
  "Reset drops stale draft history without touching another buffer's undo."
  (let ((undo-in-region nil)
        (last-command nil)
        (pending-undo-list (list '(10 . 20)))
        (undo-equiv-table (make-hash-table :test #'eq)))
    (hermes-test-with-chat-buffer
     (insert "old draft")
     (undo-boundary)
     (hermes-test--draft-undo-command #'undo-only)
     (hermes-chat--setup-buffer)
     (should-not buffer-undo-list)
     (should-not pending-undo-list)
     (insert "new draft")
     (undo-boundary)
     (setq last-command nil)
     (hermes-chat--insert-entry
      (hermes-chat--make-entry 'assistant "new session reply" 'streaming "reply"))
     (hermes-test--draft-undo-command #'undo-only)
     (should (equal (hermes-chat-input-string) "")))
    (should (equal pending-undo-list '((10 . 20))))))

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
       (should-not (string-match-p "Waiting" (hermes-test--header-line-string)))
       (funcall callback
                '(:type status
			:status-key "lifecycle"
			:status "running"
			:content "Thinking"))
       (should-not (string-match-p "Thinking" (hermes-test--header-line-string)))
       (funcall callback
                '(:type tool
			:tool-call-id "tool-1"
			:name "terminal"
			:status "running"
			:preview "make test"))
       (let ((header (hermes-test--header-line-string)))
         (should-not (string-match-p "Thinking" header))
         (should-not (string-match-p "terminal: make test" header)))
       ;; The tool stays out of the header but is still tracked for the
       ;; dashboard's per-session tool list.
       (should (hermes-chat--active-tool-summaries))
       (funcall callback '(:type done))
       (let ((header (hermes-test--header-line-string)))
         (should-not (string-match-p "Ready" header))
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

(ert-deftest hermes-chat-rename-prompt-rejects-replaced-owner ()
  "A real rename answer cannot mutate any replacement owner."
  (dolist (changed '(lifetime transport session fresh-session client mode killed))
    (hermes-test-with-chat-buffer
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (insert "Exact draft")
        (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
              hermes-chat--dashboard-session-ready-p t
              hermes-chat--dashboard-active-session-id
              (unless (eq changed 'fresh-session) "session-original"))
        (let* ((owner (current-buffer)) (noninteractive nil) sent
               (minibuffer-setup-hook
                (cons (lambda ()
                        (with-current-buffer owner
                          (pcase changed
                            ('lifetime
                             (setq hermes-chat--lifecycle-generation
                                   (hermes-chat--next-lifetime-token)))
                            ('transport (cl-incf hermes-chat--transport-generation))
                            ((or 'session 'fresh-session)
                             (setq hermes-chat--dashboard-active-session-id
                                   "session-successor"))
                            ('client
                             (setq hermes-chat--dashboard-client
                                   (hermes-test--dashboard-client)))
                            ('mode (fundamental-mode))
                            ('killed (kill-buffer owner)))
                          (when (buffer-live-p owner)
                            (setq hermes-chat--title "Successor"
                                  hermes-chat--title-manual-p nil))))
                      minibuffer-setup-hook)))
          (cl-letf (((symbol-function 'hermes-chat--dashboard-client-live-p)
                     (lambda (_client) t))
                    ((symbol-function 'hermes-dashboard-transport-session-title)
                     (lambda (_client &rest args) (setq sent args))))
            (unwind-protect
                (progn
                  (should (equal
                           (should-error
                            (execute-kbd-macro
                             (kbd "C-c C-o S R r e n a m e d RET"))
                            :type 'user-error)
                           '(user-error "Hermes rename prompt is no longer current")))
                  (should-not sent)
                  (when (buffer-live-p owner)
                    (with-current-buffer owner
                      (should (equal hermes-chat--title "Successor"))
                      (should-not hermes-chat--title-manual-p)
                      (when (derived-mode-p 'hermes-chat-mode)
                        (should (equal (hermes-chat-input-string) "Exact draft"))))))
              (keymap-popup-dismiss)
              (when (buffer-live-p owner)
                (with-current-buffer owner
                  (setq hermes-chat--dashboard-client nil))))))))))

(ert-deftest hermes-chat-rename-prompt-uses-original-session ()
  "An unchanged real prompt renames its original session, preserving the draft."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (setq hermes-chat--dashboard-active-session-id "session-original")
      (let ((owner (current-buffer)) (noninteractive nil)
            (before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) sent)
        (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                   (lambda () t))
                  ((symbol-function 'hermes-dashboard-transport-session-title)
                   (lambda (_client &rest args) (setq sent args))))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "C-c C-o S R r e n a m e d RET"))
                (should (equal (plist-get sent :session-id) "session-original"))
                (should (equal (plist-get sent :title) hermes-chat--title))
                (should (equal (hermes-session-title-chat-display hermes-chat--title)
                               "renamed"))
                (should hermes-chat--title-manual-p)
                (should (equal before (buffer-string)))
                (should (= position (point)))
                (should (equal undo buffer-undo-list))
                (should (eq owner (current-buffer))))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-rename-prompt-preserves-reader-error ()
  "An unrelated reader failure is not laundered into stale-owner refusal."
  (hermes-test-with-chat-buffer
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (signal 'file-error '("Reader failure")))))
      (should (equal (should-error (call-interactively #'hermes-chat-rename)
                                   :type 'file-error)
                     '(file-error "Reader failure"))))))

(ert-deftest hermes-chat-rename-prompt-cancel-preserves-editor ()
  "Cancelling a real rename preserves the title, draft, undo, point and focus."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (setq hermes-chat--title "Original")
      (let ((before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) (owner (current-buffer))
            (noninteractive nil) sent)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-session-title)
                   (lambda (&rest _) (setq sent t))))
          (unwind-protect
              (progn
                (condition-case nil
                    (execute-kbd-macro (kbd "C-c C-o S R C-g"))
                  (quit nil))
                (should-not sent)
                (should (equal hermes-chat--title "Original"))
                (should-not hermes-chat--title-manual-p)
                (should (equal before (buffer-string)))
                (should (= position (point)))
                (should (equal undo buffer-undo-list))
                (should (eq owner (current-buffer)))
                (should (eq owner (window-buffer (selected-window)))))
            (keymap-popup-dismiss)))))))

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

(ert-deftest hermes-chat-configured-instance-uses-resolved-filesystem-mode ()
  "A named instance derives filesystem ownership from its transport target."
  (dolist (spec '(("http://127.0.0.1:9119" auto t)
                  ("http://127.0.0.1:9119" spawn t)
                  ("http://127.0.0.1:9119" remote nil)
                  ("https://hermes.example.test" auto nil)))
    (let* ((instance (cons "named" (nth 0 spec)))
           (hermes-instances (list instance))
           (hermes-dashboard-transport-start-mode (nth 1 spec))
           (launch-directory (file-name-as-directory temporary-file-directory))
           buffer)
      (unwind-protect
          (let ((default-directory launch-directory))
            (setq buffer (hermes-chat nil instance))
            (with-current-buffer buffer
              (should (eq hermes-chat--resolved-start-mode
                          (if (nth 2 spec) 'spawn 'remote)))
              (should (equal default-directory launch-directory))
              (if (nth 2 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-configured-instance-pins-resolved-start-mode ()
  "A named chat keeps one transport mode for cwd locality and acquisition."
  (dolist (spec '((auto remote spawn t)
                  (remote spawn remote nil)))
    (let* ((instance '(:id "named" :name "Named"
                       :url "http://127.0.0.1:9119"))
           (hermes-instances (list instance))
           (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
           (launch-directory (file-name-as-directory temporary-file-directory))
           (client (hermes-test--dashboard-client))
           buffer acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 0 spec))
            (let ((default-directory launch-directory))
              (setq buffer (hermes-chat nil instance)))
            (set-default 'hermes-dashboard-transport-start-mode (nth 1 spec))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           client)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq acquired-mode (nth 2 spec)))
              (should (eq hermes-chat--resolved-start-mode (nth 2 spec)))
              (should-not
               (local-variable-p 'hermes-dashboard-transport-start-mode))
              (if (nth 3 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory)))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-adopts-attached-client-start-mode ()
  "A live legacy client wins over changed configuration before reacquisition."
  (dolist (spec '((remote "https://hermes.example.test" spawn
                          ("named" . "http://127.0.0.1:9119"))
                  (spawn (spawn "127.0.0.1" 9119) remote
                         ("named" . "https://hermes.example.test"))))
    (let* ((expected (nth 0 spec))
           (client (make-hermes-dashboard-transport-client
                    :websocket 'attached-websocket
                    :endpoint-key (nth 1 spec)))
           (replacement (hermes-test--dashboard-client))
           (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
           (buffer (generate-new-buffer " *hermes-legacy-mode*"))
           acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 2 spec))
            (with-current-buffer buffer
              (hermes-chat-mode)
              (setq hermes-instance (nth 3 spec)
                    hermes-chat--dashboard-client client)
              (kill-local-variable 'hermes-dashboard-transport-start-mode)
              (should (eq (hermes-chat--dashboard-ensure-client) client))
              (should (eq hermes-chat--resolved-start-mode expected))
              (setq hermes-chat--dashboard-client nil)
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           replacement)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq acquired-mode expected))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-adopts-old-local-start-mode ()
  "A legacy concrete pin wins over changed configuration during acquisition."
  (dolist (spec '((spawn remote) (remote spawn)))
    (let ((saved-mode (default-value 'hermes-dashboard-transport-start-mode))
          (client (hermes-test--dashboard-client))
          (buffer (generate-new-buffer " *hermes-legacy-local-mode*"))
          acquired-mode)
      (unwind-protect
          (progn
            (set-default 'hermes-dashboard-transport-start-mode (nth 1 spec))
            (with-current-buffer buffer
              (hermes-chat-mode)
              (setq hermes-instance
                    '("named" . "http://127.0.0.1:9119"))
              (setq-local hermes-dashboard-transport-start-mode (nth 0 spec))
              (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                         (lambda (&rest args)
                           (setq acquired-mode (plist-get args :start-mode))
                           client)))
                (hermes-chat--dashboard-ensure-client))
              (should (eq hermes-chat--resolved-start-mode (nth 0 spec)))
              (should (eq acquired-mode (nth 0 spec)))))
        (set-default 'hermes-dashboard-transport-start-mode saved-mode)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-legacy-buffer-resolves-instance-once-for-acquisition ()
  "One selected instance supplies both fallback mode and acquisition URL."
  (let* ((remote '(:id "remote" :name "Remote"
                   :url "https://hermes.example.test"))
         (local '("local" . "http://127.0.0.1:9119"))
         (choices (list remote local))
         (client (hermes-test--dashboard-client))
         (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
         (buffer (generate-new-buffer " *hermes-legacy-instance*"))
         prompts acquired-mode acquired-url)
    (unwind-protect
        (progn
          (set-default 'hermes-dashboard-transport-start-mode 'auto)
          (with-current-buffer buffer
            (hermes-chat-mode)
            (kill-local-variable 'hermes-dashboard-transport-start-mode)
            (cl-letf (((symbol-function 'hermes-instance-resolve)
                       (lambda ()
                         (setq prompts (1+ (or prompts 0)))
                         (pop choices)))
                      ((symbol-function 'hermes-dashboard-transport-acquire)
                       (lambda (&rest args)
                         (setq acquired-mode (plist-get args :start-mode)
                               acquired-url hermes-dashboard-transport-url)
                         client)))
              (hermes-chat--dashboard-ensure-client))
            (should (= prompts 1))
            (should (equal hermes-instance remote))
            (should (eq acquired-mode 'remote))
            (should (equal acquired-url "https://hermes.example.test"))))
      (set-default 'hermes-dashboard-transport-start-mode saved-mode)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest hermes-chat-lifetime-reset-adopts-attached-client-start-mode ()
  "Disconnect and reset preserve an unpinned attached client's endpoint mode."
  (dolist (action '(disconnect reset))
    (dolist (spec '((remote "https://hermes.example.test" spawn
                            ("named" . "https://hermes.example.test"))
                    (spawn (spawn "127.0.0.1" 9119) remote
                           ("named" . "http://127.0.0.1:9119"))))
      (let* ((expected (nth 0 spec))
             (client (make-hermes-dashboard-transport-client
                      :websocket 'attached-websocket
                      :endpoint-key (nth 1 spec)))
             (replacement (hermes-test--dashboard-client))
             (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
             (buffer (generate-new-buffer " *hermes-lifetime-mode*"))
             acquired-mode)
        (unwind-protect
            (progn
              (set-default 'hermes-dashboard-transport-start-mode (nth 2 spec))
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance (nth 3 spec)
                      hermes-chat--dashboard-client client
                      hermes-chat--dashboard-active-session-id "sid")
                (kill-local-variable 'hermes-dashboard-transport-start-mode)
                (let ((hermes-chat-cleanup-functions nil))
                  (pcase action
                    ('disconnect (hermes-chat-disconnect))
                    ('reset (hermes-chat--reset-transcript))))
                (should (eq hermes-chat--resolved-start-mode expected))
                (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
                           (lambda (&rest args)
                             (setq acquired-mode (plist-get args :start-mode))
                             replacement)))
                  (hermes-chat--dashboard-ensure-client))
                (should (eq acquired-mode expected))))
          (set-default 'hermes-dashboard-transport-start-mode saved-mode)
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-new-instance-does-not-inherit-source-start-mode ()
  "A new chat resolves its instance without inheriting another chat's mode."
  (let* ((local '("local" . "http://127.0.0.1:9119"))
         (remote '("remote" . "https://hermes.example.test"))
         (hermes-instances (list local remote))
         (saved-mode (default-value 'hermes-dashboard-transport-start-mode))
         (launch-directory (file-name-as-directory temporary-file-directory))
         local-buffer remote-buffer)
    (unwind-protect
        (progn
          (set-default 'hermes-dashboard-transport-start-mode 'auto)
          (let ((default-directory launch-directory))
            (setq local-buffer (hermes-chat nil local)))
          (with-current-buffer local-buffer
            (setq remote-buffer (hermes-chat nil remote)))
          (with-current-buffer remote-buffer
            (should (eq hermes-chat--resolved-start-mode 'remote))
            (should-not
             (local-variable-p 'hermes-dashboard-transport-start-mode))
            (should-not (hermes-chat--current-working-directory))))
      (set-default 'hermes-dashboard-transport-start-mode saved-mode)
      (when (buffer-live-p local-buffer) (kill-buffer local-buffer))
      (when (buffer-live-p remote-buffer) (kill-buffer remote-buffer)))))

(ert-deftest hermes-chat-legacy-instance-uses-resolved-filesystem-mode ()
  "The unnamed singleton seeds cwd only when its resolved mode is spawn."
  (dolist (spec '(("http://127.0.0.1:9119" auto t)
                  ("http://127.0.0.1:9119" spawn t)
                  ("http://127.0.0.1:9119" remote nil)
                  ("https://hermes.example.test" auto nil)))
    (let ((hermes-instances nil)
          (hermes-dashboard-transport-url (nth 0 spec))
          (hermes-dashboard-transport-start-mode (nth 1 spec))
          (launch-directory (file-name-as-directory temporary-file-directory))
          buffer)
      (unwind-protect
          (let ((default-directory launch-directory))
            (setq buffer (hermes-chat nil))
            (with-current-buffer buffer
              (should (eq hermes-chat--resolved-start-mode
                          (if (nth 2 spec) 'spawn 'remote)))
              (should (equal default-directory launch-directory))
              (if (nth 2 spec)
                  (should (equal (hermes-chat--current-working-directory)
                                 launch-directory))
                (should-not (hermes-chat--current-working-directory))
                (should (string-match-p
                         (format "\\[%s\\]"
                                 (regexp-quote
                                  (hermes-chat--directory-basename
                                   launch-directory)))
                         (buffer-name)))
                (should (string-match-p
                         "detached" (hermes-test--header-line-string))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-remote-resume-hydrates-only-gateway-cwd ()
  "A remote resume uses editor identity while gateway execution stays detached."
  (dolist (outcome '(resolve reject))
    (let* ((instance '("remote" . "https://hermes.example.test"))
           (hermes-instances (list instance))
           (launch-directory (file-name-as-directory temporary-file-directory))
           resolve reject buffer)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) (hermes-test--dashboard-client)))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _session-id &rest args)
                   (setq resolve (plist-get args :resolve)
                         reject (plist-get args :reject)))))
        (unwind-protect
            (let ((default-directory launch-directory))
              (setq buffer
                    (hermes-chat-resume-session "stored" nil nil instance))
              (with-current-buffer buffer
                (should-not hermes-chat--working-directory)
                (should (string-match-p
                         (format "\\[%s\\]"
                                 (regexp-quote
                                  (hermes-chat--directory-basename
                                   launch-directory)))
                         (buffer-name)))
                (should (string-match-p "detached"
                                        (hermes-test--header-line-string)))
                (pcase outcome
                  ('resolve
                   (funcall resolve
                            '((session_id . "live")
                              (info . ((cwd . "/srv/repo")))))
                   (should (equal hermes-chat--working-directory "/srv/repo"))
                   (should (string-match-p "\\[repo\\]" (buffer-name))))
                  ('reject
                   (funcall reject "resume failed")
                   (should (string-match-p
                            (format "\\[%s\\]"
                                    (regexp-quote
                                     (hermes-chat--directory-basename
                                      launch-directory)))
                            (buffer-name)))
                   (should-not hermes-chat--working-directory)))
                (should (equal default-directory launch-directory))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-resume-is-explicitly-unowned-by-launch-project ()
  "A resumed chat cannot be mistaken for a legacy project chat."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-resume-project-" t)))
         (instance '("remote" . "https://hermes.example.test"))
         (hermes-instances (list instance))
         resolve buffer)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) (hermes-test--dashboard-client)))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (unwind-protect
          (let ((default-directory root))
            (setq buffer
                  (hermes-chat-resume-session "stored" nil nil instance))
            (with-current-buffer buffer
              (should (local-variable-p 'hermes-chat--launch-project-root))
              (should-not hermes-chat--launch-project-root)
              (funcall resolve
                       '((session_id . "live")
                         (info . ((cwd . "/srv/resumed")))))
              (should (string-match-p "\\[resumed\\]" (buffer-name)))
              (should-not
               (hermes-chat--project-buffers root (list buffer)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))
    (delete-directory root t)))

(ert-deftest hermes-chat-fresh-override-failure-settles-prompt ()
  "A post-attachment override failure settles the creating prompt exactly once."
  (let ((client (hermes-test--dashboard-client)) config-reject prompt-submits)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (hermes--promise-resolved '((cwd . "/srv/default")))))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-reject (plist-get args :reject))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (setq prompt-submits (1+ (or prompt-submits 0)))))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--dashboard-create-fast-p t)
         (insert "hello")
         (hermes-chat-send)
         (should (bound-and-true-p hermes-chat--session-bootstrap))
         (should hermes-chat--pending-assistant-id)
         (should hermes-chat--unsettled-submit-context)
         (funcall config-reject "config boom")
         (should-not prompt-submits)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--unsettled-submit-context)
         (should-not hermes-chat--dashboard-running-p)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should (string-match-p "Pre-session override failed: config boom"
                                 (buffer-string))))))))

(ert-deftest hermes-chat-sync-create-failure-retains-queued-entry ()
  "A queued origin remains queued when fresh session creation signals."
  (let ((client (hermes-test--dashboard-client)) api-calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq api-calls (1+ (or api-calls 0)))
                 (hermes--promise-resolved '((cwd . "/srv/default")))))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (&rest _) (error "sync create boom"))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--working-directory nil)
         (hermes-chat--queue-content "queued")
         (let ((entry (car hermes-chat--queued-messages)))
           (setq hermes-chat--queued-submit-id (plist-get entry :id))
           (hermes-chat--submit-content "queued" nil entry))
         (should (= api-calls 1))
         (should (equal (hermes-test--queued-contents) '("queued")))
         (should-not hermes-chat--queued-submit-id)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--unsettled-submit-context)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should (string-match-p "Queued message retained: sync create boom"
                                 (buffer-string))))))))

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

(ert-deftest hermes-chat-project-root-prefers-project-and-falls-back-to-directory ()
  "Project identity uses the project root or normalized directory fallback."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-root-" t)))
         (nested (expand-file-name "src/lib/" root))
         (outside (file-name-as-directory
                   (make-temp-file "hermes-project-fallback-" t))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (should (equal (file-truename (hermes-chat--project-root nested))
                         (file-truename root)))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
            (should (equal (hermes-chat--project-root outside) outside))))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest hermes-project-chat-switches-single-exact-root-match ()
  "Project chat switches directly to the sole exact-root chat."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-chat-" t)))
         (other-root (file-name-as-directory
                      (make-temp-file "hermes-project-other-" t)))
         (nested (expand-file-name "src/" root))
         (target (generate-new-buffer " *hermes-project-target*"))
         (other (generate-new-buffer " *hermes-project-other*"))
         shown)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (dolist (entry `((,target . ,root) (,other . ,other-root)))
            (with-current-buffer (car entry)
              (setq default-directory (cdr entry))
              (hermes-chat-mode)))
          (with-temp-buffer
            (setq default-directory nested)
            (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (eq shown target)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list target other))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest hermes-project-chat-completes-among-same-root-siblings ()
  "Multiple project chats offer only exact-root siblings."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-siblings-" t)))
         (other-root (file-name-as-directory
                      (make-temp-file "hermes-project-excluded-" t)))
         (first (generate-new-buffer " *hermes-project-first*"))
         (second (generate-new-buffer " *hermes-project-second*"))
         (other (generate-new-buffer " *hermes-project-excluded*"))
         offered shown)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (dolist (entry `((,first . ,root) (,second . ,root)
                           (,other . ,other-root)))
            (with-current-buffer (car entry)
              (setq default-directory (cdr entry))
              (hermes-chat-mode)))
          (with-temp-buffer
            (setq default-directory root)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _)
                         (setq offered candidates)
                         (buffer-name second)))
                      ((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (equal (sort (copy-sequence offered) #'string<)
                         (sort (mapcar #'buffer-name (list first second))
                               #'string<)))
          (should (eq shown second)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list first second other))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest hermes-project-chat-adopts-and-renames-legacy-buffer ()
  "Project switching repairs a pre-project-identity chat name."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-adopt-" t)))
         (target (generate-new-buffer "*local@default: [emacs-hermes]*"))
         shown)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (with-current-buffer target
            (setq default-directory root)
            (hermes-chat-mode)
            (setq hermes-chat--resolved-start-mode 'remote
                  hermes-chat--working-directory "/srv/emacs-hermes"))
          (with-temp-buffer
            (setq default-directory root)
            (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                       (lambda (buffer &rest _) (setq shown buffer))))
              (hermes-project-chat)))
          (should (eq shown target))
          (with-current-buffer target
            (should (equal (file-truename hermes-chat--launch-project-root)
                           (file-truename root)))
            (should (string-match-p
                     (format "\\[%s\\]" (file-name-nondirectory
                                          (directory-file-name root)))
                     (buffer-name)))))
      (when (buffer-live-p target) (kill-buffer target))
      (delete-directory root t))))

(ert-deftest hermes-chat-direct-new-does-not-inherit-project-identity ()
  "A direct new chat from a project chat remains cwd-named and unowned."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-direct-from-project-" t)))
         (instance '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list instance))
         (source (generate-new-buffer " *hermes-project-source*"))
         direct-a direct-b)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer source
            (setq default-directory root)
            (hermes-chat-mode)
            (setq hermes-instance instance
                  hermes-chat--launch-project-root root
                  hermes-chat--resolved-start-mode 'remote
                  hermes-chat--working-directory "/srv/source")
            (setq direct-a (hermes-chat nil instance)
                  direct-b (hermes-chat nil instance)))
          (dolist (buffer (list direct-a direct-b))
            (with-current-buffer buffer
              (hermes-chat--record-working-directory "/srv/second")
              (should-not hermes-chat--launch-project-root)
              (should (string-match-p "\\[second\\]" (buffer-name)))))
          (should-not (memq direct-a
                            (hermes-chat--project-buffers
                             root (list direct-a direct-b))))
          (should (string-suffix-p "<2>" (buffer-name direct-b))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list source direct-a direct-b))
      (delete-directory root t))))

(ert-deftest hermes-chat-buffer-name-function-is-customizable ()
  "A custom naming function owns the complete chat buffer name."
  (let ((hermes-chat-buffer-name-function
         (lambda (profile instance directory)
           (format "*Custom: %s/%s/%s*"
                   (hermes-instance-name instance) profile
                   (file-name-nondirectory (directory-file-name directory))))))
    (dolist (instance '(("local" . "http://127.0.0.1:9119")
                        (:id "local-id" :name "local"
                         :url "http://127.0.0.1:9119")))
      (should (equal (hermes-chat--buffer-name
                      "coder" instance "/tmp/nema/")
                     "*Custom: local/coder/nema*")))))

(ert-deftest hermes-chat-buffer-name-function-receives-display-directory ()
  "A custom name receives editor fallback, then hydrated gateway cwd."
  (let* (captured
         (hermes-chat-buffer-name-function
          (lambda (_profile _instance directory)
            (setq captured directory)
            "*Captured Hermes*")))
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--launch-project-root nil
           hermes-chat--working-directory nil)
     (hermes-chat--refresh-buffer-name)
     (should (equal captured "/tmp/local-editor/"))
     (hermes-chat--record-working-directory "/srv/project")
     (should (equal captured "/srv/project")))))

(ert-deftest hermes-project-chat-keeps-launch-project-in-buffer-name ()
  "A project chat name stays anchored to its launching project."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-name-" t)))
         (nested (expand-file-name "src/" root))
         (instance '("local" . "http://127.0.0.1:9119"))
         (hermes-instances (list instance))
         buffer)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (save-window-excursion
            (with-temp-buffer
              (setq default-directory nested)
              (cl-letf (((symbol-function 'hermes-chat--read-profile)
                         (lambda () nil)))
                (setq buffer (hermes-project-chat t)))))
          (with-current-buffer buffer
            (hermes-chat--record-working-directory
             "/srv/emacs-hermes")
            (should (equal (file-truename hermes-chat--launch-project-root)
                           (file-truename root)))
            (should (string-match-p
                     (format "\\[%s\\]" (file-name-nondirectory
                                          (directory-file-name root)))
                     (buffer-name)))
            (should (string-match-p "emacs-hermes"
                                    (hermes-test--header-line-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest hermes-project-chat-routes-from-adopted-project-buffer ()
  "A project chat routes from its launch root after explicit cwd adoption."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-route-" t)))
         (gateway (file-name-as-directory
                   (make-temp-file "hermes-gateway-route-" t)))
         (buffer (generate-new-buffer " *Hermes project route*"))
         selected normal-created prefix-created prefix-directory prefix-phase)
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory root)
          (hermes-chat-mode)
          (setq hermes-chat--launch-project-root root
                hermes-chat--resolved-start-mode 'remote)
          (hermes-chat--apply-directory gateway)
          (cl-letf (((symbol-function 'hermes-chat--live-buffers)
                     (lambda () (list buffer)))
                    ((symbol-function 'pop-to-buffer-same-window)
                     (lambda (target &rest _) (setq selected target)))
                    ((symbol-function 'call-interactively)
                     (lambda (command &rest _)
                       (if prefix-phase
                           (setq prefix-created command
                                 prefix-directory default-directory)
                         (setq normal-created command)))))
            (hermes-project-chat)
            (should (eq selected buffer))
            (should-not normal-created)
            (setq prefix-phase t)
            (hermes-project-chat t)
            (should (eq prefix-created #'hermes-chat))
            (should (equal (file-truename prefix-directory)
                           (file-truename root)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)
      (delete-directory gateway t))))

(ert-deftest hermes-project-chat-prefix-creates-at-project-root ()
  "Prefix always creates a sibling chat rooted at the current project."
  (let* ((root (file-name-as-directory
                (make-temp-file "hermes-project-new-" t)))
         (nested (expand-file-name "src/" root))
         called directory)
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory (expand-file-name ".git" root))
          (with-temp-buffer
            (setq default-directory nested)
            (cl-letf (((symbol-function 'call-interactively)
                       (lambda (command &rest _)
                         (setq called command
                               directory default-directory))))
              (hermes-project-chat t)))
          (should (eq called #'hermes-chat))
          (should (equal (file-truename directory) (file-truename root))))
      (delete-directory root t))))

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

(ert-deftest hermes-chat-actions-popup-bound ()
  "Chat actions use shallow native menus with two columns per row."
  (should (eq (keymap-lookup hermes-chat-mode-map "C-c C-o")
              #'hermes-chat-actions-map-popup))
  (let ((rows (keymap-popup--meta hermes-chat-actions-map 'descriptions)))
    (should (equal (mapcar (lambda (row)
                            (mapcar (lambda (group) (plist-get group :name)) row))
                          rows)
                   '(("Turn" "Compose") ("Configure" "Browse"))))))

(ert-deftest hermes-chat-actions-popup-paths ()
  "Actual popup wrappers dispatch every advertised path in the owner buffer."
  (hermes-test-with-chat-buffer
    (let ((owner (current-buffer)))
      (dolist (path '(("s" hermes-chat-steer-message)
                      ("i" hermes-chat-interrupt)
                      ("k" hermes-chat-interrupt-and-send)
                      ("q" hermes-chat-queue-message)
                      ("a" hermes-chat-respond-to-prompt)
                      ("d" hermes-chat-cancel-prompt)
                      ("j" hermes-chat-go-to-composer)
                      ("f" hermes-chat-attach-image-file)
                      ("v" hermes-chat-paste-image)
                      ("I V" hermes-chat-preview-images)
                      ("I D" hermes-chat-remove-image)
                      ("c" hermes-chat-show-commands)
                      ("r" hermes-chat-refresh-commands)
                      ("S n" hermes-chat)
                      ("S R" hermes-chat-rename)
                      ("S H" hermes-chat-handoff)
                      ("S S" hermes-list-sessions)
                      ("M m" hermes-chat-switch-model)
                      ("M e" hermes-chat-set-reasoning)
                      ("M K" hermes-chat-connect-provider)
                      ("w w" hermes-chat-set-directory)
                      ("w b" hermes-switch-to-chat)
                      ("w P" hermes-chat-queue-panel)
                      ("X W" hermes-chat-work)
                      ("X h" hermes-chat-session-details)
                      ("X x" hermes-dashboard-reconnect)
                      ("X u" hermes-chat-show-usage)
                      ("X t" hermes-chat-show-status)))
        (let (called)
          (cl-letf (((symbol-function (cadr path))
                     (lambda () (interactive) (setq called (current-buffer)))))
            (unwind-protect
                (save-window-excursion
                  (switch-to-buffer owner)
                  (execute-kbd-macro (kbd (concat "C-c C-o " (car path))))
                  (should (eq called owner)))
              (keymap-popup-dismiss))))))))

(ert-deftest hermes-chat-actions-popup-shortcut-aliases ()
  "Unclaimed old suffix keys remain hidden aliases, not a second menu."
  (dolist (map (list hermes-chat-images-map hermes-chat-sess-map
                     hermes-chat-model-map hermes-chat-work-map
                     hermes-chat-info-map))
    (map-keymap
     (lambda (event binding)
       (when (and (commandp binding)
                  (not (eq binding #'hermes-chat--submenu-root-key))
                  (not (memq event '(?? ?S ?w))))
         (should (eq (lookup-key hermes-chat-actions-map (vector event))
                     binding))))
     map)))

(ert-deftest hermes-chat-actions-popup-back-and-cancel ()
  "Native q/C-g back navigation and dismissal preserve the chat draft."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (insert "Unsent draft")
      (let ((owner (current-buffer)) (before (buffer-string)))
        (unwind-protect
            (progn
              (hermes-chat-actions-map-popup)
              (execute-kbd-macro (kbd "S q"))
              (let ((popup (get-buffer keymap-popup--buffer-name)))
                (should popup)
                (should (eq (keymap-popup--active-get popup :keymap)
                            hermes-chat-actions-map)))
              (execute-kbd-macro (kbd "M C-g"))
              (should (get-buffer keymap-popup--buffer-name))
              (execute-kbd-macro (kbd "C-g"))
              (should-not (get-buffer keymap-popup--buffer-name))
              (should (eq (current-buffer) owner))
              (should (equal (buffer-string) before)))
          (keymap-popup-dismiss))))))

(ert-deftest hermes-chat-actions-popup-directory-guard ()
  "The nested directory action refuses busy turns in the actual wrapper."
  (hermes-test-with-chat-buffer
    (let (called)
      (cl-letf (((symbol-function 'hermes-chat--active-turn-p) (lambda () t))
                ((symbol-function 'hermes-chat-set-directory)
                 (lambda () (interactive) (setq called t))))
        (unwind-protect
            (save-window-excursion
              (hermes-chat-actions-map-popup)
              (call-interactively (key-binding (kbd "w")))
              (call-interactively (key-binding (kbd "w")))
              (should-not called)
              (should (eq (keymap-popup--active-get
                           (get-buffer keymap-popup--buffer-name) :keymap)
                          hermes-chat-work-map)))
          (keymap-popup-dismiss))))))

(ert-deftest hermes-chat-actions-popup-minibuffer-owner ()
  "A nested prompting suffix reads text without stealing the draft owner."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (insert "Keep this draft")
      (let ((owner (current-buffer)) (before (buffer-string)) answer called)
        (cl-letf (((symbol-function 'hermes-chat-rename)
                   (lambda (name)
                     (interactive (list (read-string "Session name: ")))
                     (setq answer name called (current-buffer)))))
          (unwind-protect
              (let ((noninteractive nil))
                (hermes-chat-actions-map-popup)
                (execute-kbd-macro (kbd "S R n e w SPC n a m e RET"))
                (should (equal answer "new name"))
                (should (eq called owner))
                (should (equal (buffer-string) before))
                (should-not (get-buffer keymap-popup--buffer-name)))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-actions-popup-inherited-launchers-safe ()
  "Root launchers inside children refuse safely through the command loop."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (buffer-enable-undo)
      (insert "Exact draft")
      (undo-boundary)
      (let ((before (buffer-string)) (position (point))
            (undo (copy-tree buffer-undo-list)) (owner (current-buffer))
            (children '("S" "M" "w" "I" "X")) paths)
        (dolist (child children)
          (dolist (target children)
            ;; S S and w w are child actions, not ancestor launchers.
            (unless (member (list child target) '(("S" "S") ("w" "w")))
              (let ((path (concat "C-c C-o " child " " target)))
                (unwind-protect
                    (progn
                      (should (equal
                               (should-error (execute-kbd-macro (kbd path))
                                             :type 'user-error)
                               '(user-error
                                 "Reopen chat actions to choose another menu")))
                      (push path paths)
                      (should-not (get-buffer keymap-popup--buffer-name))
                      ;; The supported child -> root -> child route still works.
                      (execute-kbd-macro
                       (kbd (concat "C-c C-o " child " q " target " q C-g")))
                      (should-not (get-buffer keymap-popup--buffer-name))
                      (should (eq owner (current-buffer)))
                      (should (eq owner (window-buffer (selected-window))))
                      (should (equal before (buffer-string)))
                      (should (= position (point)))
                      (should (equal undo buffer-undo-list)))
                  (keymap-popup-dismiss))))))
        (should (= (length paths) 23))))))

(ert-deftest hermes-chat-actions-popup-model-busy-availability ()
  "Busy model and reasoning actions remain visible but cannot prompt."
  (hermes-test-with-chat-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (setq hermes-chat--dashboard-running-p t
            hermes-chat--model "Current model"
            hermes-chat--runtime-flags '(:reasoning-effort "low"))
      (let (prompted)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (setq prompted t) "high")))
          (unwind-protect
              (progn
                (execute-kbd-macro (kbd "C-c C-o M"))
                (with-current-buffer (get-buffer keymap-popup--buffer-name)
                  (dolist (text '("Switch model: Current model"
                                  "Set reasoning: low"))
                    (goto-char (point-min))
                    (search-forward text)
                    (should (eq (get-text-property (line-beginning-position) 'face)
                                'keymap-popup-inapt))))
                (execute-kbd-macro (kbd "m e"))
                (should-not prompted)
                (should-not hermes-chat--dashboard-create-reasoning-effort)
                (should (eq (keymap-popup--active-get
                             (get-buffer keymap-popup--buffer-name) :keymap)
                            hermes-chat-model-map)))
            (keymap-popup-dismiss)))))))

(ert-deftest hermes-chat-set-reasoning-busy-before-prompt ()
  "Direct reasoning invocation refuses an already-busy chat before asking."
  (hermes-test-with-chat-buffer
    (setq hermes-chat--dashboard-running-p t)
    (let (prompted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq prompted t) "high")))
        (should-error (call-interactively #'hermes-chat-set-reasoning)
                      :type 'user-error)
        (should-not prompted)
        (should-not hermes-chat--dashboard-create-reasoning-effort)))))

(ert-deftest hermes-chat-actions-popup-attach-image-label ()
  "The actual root advertises the image-only attachment command accurately."
  (hermes-test-with-chat-buffer
    (unwind-protect
        (progn
          (hermes-chat-actions-map-popup)
          (with-current-buffer (get-buffer keymap-popup--buffer-name)
            (should (string-match-p "Attach image" (buffer-string)))
            (should-not (string-match-p "Attach file" (buffer-string)))))
      (keymap-popup-dismiss))))

(ert-deftest hermes-chat-set-reasoning-before-session-stores-override ()
  "A fresh buffer stores reasoning effort without opening a session."
  (hermes-test-with-chat-buffer
   (hermes-chat-set-reasoning "high")
   (should (equal hermes-chat--dashboard-create-reasoning-effort "high"))
   (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                  "high"))
   (should-not hermes-chat--dashboard-client)
   (should (string-match-p "applies to next session" (buffer-string)))))

(ert-deftest hermes-chat-loads-cached-profile-model-for-fresh-draft ()
  "A fresh draft projects the selected profile's configured model."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "default") (is_default . t)
                                (model . "gpt-default"))
                               ((name . "coder") (model . "gpt-coder"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--profile "coder")
     (hermes-chat--restore-draft-runtime)
     (should (equal hermes-chat--model "gpt-coder")))))

(ert-deftest hermes-chat-draft-model-cache-does-not-clobber-selection ()
  "Cached profile metadata cannot replace an explicit draft model choice."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "coder") (model . "default"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--profile "coder"
           hermes-chat--dashboard-create-model "chosen"
           hermes-chat--model "chosen")
     (hermes-chat--restore-draft-runtime)
     (should (equal hermes-chat--model "chosen")))))

(ert-deftest hermes-chat-new-and-cleared-buffers-load-profile-model ()
  "New buffers and `/clear' restore the selected profile's configured model."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "coder") (model . "gpt-coder"))))))))
    (let ((buffer (hermes-chat--new-buffer "coder")))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal hermes-chat--model "gpt-coder"))
            (setq hermes-chat--model "stale")
            (hermes-chat--reset-transcript)
            (should (equal hermes-chat--model "gpt-coder")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest hermes-chat-clear-reprojects-pending-draft-runtime ()
  "`/clear' keeps surviving create overrides visible and ready for creation."
  (cl-letf (((symbol-function 'hermes-dashboard-transport-cached-profile-list)
             (lambda (&optional _client)
               '((profiles . (((name . "default")
                                (model . "profile-default"))))))))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-create-model "grok-4.6"
           hermes-chat--dashboard-create-provider "xai-oauth"
           hermes-chat--dashboard-create-reasoning-effort "high"
           hermes-chat--dashboard-create-fast-p t
           hermes-chat--model "grok-4.6"
           hermes-chat--runtime-flags '(:reasoning-effort "high" :fast t))
     (hermes-chat--reset-transcript)
     (should (equal hermes-chat--model "grok-4.6"))
     (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                    "high"))
     (should (eq (plist-get hermes-chat--runtime-flags :fast) t))
     (let ((params (hermes-chat--dashboard-create-params)))
       (should (equal (plist-get params :model) "grok-4.6"))
       (should (equal (plist-get params :provider) "xai-oauth"))
       (should (equal (plist-get params :reasoning-effort) "high"))
       (should (eq (plist-get params :fast) t))))))

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

(ert-deftest hermes-chat-markdown-marks-original-tables ()
  "Recognizing tables does not align or otherwise change their source."
  (let* ((raw "| A | B |\n|---|---|\n| one | two |\n")
         (text (hermes-chat--fontify-markdown-string raw)))
    (should (equal (substring-no-properties text) raw))
    (should (equal (get-text-property 0 'hermes-chat-table text) raw))))

(ert-deftest hermes-chat-table-wrap-preserves-literals ()
  "Wrapping preserves every character, native face and combining sequence."
  (dolist (text (list "path\\ followed by text" "a\\|b **bold** `code`"
                      "界é界é" (make-string 160 ?x)))
    (dolist (width '(2 3 7 22))
      (let ((lines (hermes-chat--table-cell-lines
                    (propertize text 'face 'bold) width)))
        (should (equal (apply #'concat lines) text))
        (dolist (line lines)
          (should (<= (string-width line) width))
          (when (> (length line) 0)
            (should (eq (get-text-property 0 'face line) 'bold))
            (should-not (= (aref line 0) #x301))))))))

(ert-deftest hermes-chat-table-grid-bounded-with-native-cells ()
  "Long tokens, escaped pipes and Unicode remain visible in bounded grids."
  (let ((source (concat "| Kind | Description |\n|---|---|\n"
                        "| 界é **bold** | path\\ followed a\\|b `x|y` "
                        (make-string 160 ?x) " |\n")))
    (dolist (width '(12 28 78))
      (let ((grid (hermes-chat--format-table source width)))
        (dolist (line (split-string grid "\n" t))
          (should (<= (string-width line) width)))
        (should-not (text-property-not-all 0 (length grid) 'display nil grid))
        (should-not (text-property-not-all 0 (length grid) 'invisible nil grid))
        (should (eq 'fixed-pitch (get-text-property 0 'face grid)))))
    ;; The native parser keeps escaped pipes and fontified code spans together.
    (let* ((line (hermes-chat--fontify-markdown-string
                  "| a\\|b | `x|y` |\n"))
           (cells (markdown--table-line-to-columns (string-trim-right line))))
      (should (equal cells '("a\\|b" "`x|y`"))))))

(ert-deftest hermes-chat-table-degenerate-and-optional-trailing-pipes ()
  "Degenerate tables stay literal; missing closing pipes never lose cells."
  (dolist (raw '("|---|---|\n" "|\n"))
    (should (equal (substring-no-properties (hermes-chat--format-table raw 28)) raw)))
  (let* ((raw "| A | B\n|---|---|\n| x | y\n")
         (grid (hermes-chat--format-table raw 28)))
    (dolist (cell '("A" "B" "x" "y"))
      (should (string-match-p cell grid)))))

(ert-deftest hermes-chat-table-narrow-column-panels ()
  "Narrow windows retain every header and data cell in column panels."
  (let* ((source "| A | B | C | D |\n|---|---|---|---|\n| a | b | c | d |\n")
         (grid (hermes-chat--format-table source 12)))
    (dolist (cell '("A" "B" "C" "D" "a" "b" "c" "d"))
      (should (string-match-p cell grid)))
    (dolist (line (split-string grid "\n" t))
      (should (<= (string-width line) 12)))))

(ert-deftest hermes-chat-table-ragged-panels-preserve-all-cells ()
  "Missing cells render empty, including entire rows and header panel slices."
  (let ((rows '(("A" "B" "C") ("x") nil ("y" "z" "q" "r")))
        (padded '(("A" "B" "C" "") ("x" "" "" "")
                  ("" "" "" "") ("y" "z" "q" "r"))))
    (dolist (width '(6 12 80))
      (should (equal (hermes-chat--table-grid rows width)
                     (hermes-chat--table-grid padded width)))))
  (dolist (body '("| x |\n" "|\n| x |\n| y | z | q | r |\n"))
    (let* ((raw (concat "| A | B | C |\n|---|---|---|\n" body))
           (grid (hermes-chat--format-table raw 12)))
      (dolist (cell (if (string-match-p "y" body)
                        '("A" "B" "C" "x" "y" "z" "q" "r")
                      '("A" "B" "C" "x")))
        (should (string-match-p (regexp-quote cell) grid)))
      (dolist (line (split-string grid "\n" t))
        (should (<= (string-width line) 12))))))

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

(ert-deftest hermes-chat-markdown-unclosed-fenced-tables-stay-inline ()
  "Settled replies retain table-like code in either unclosed fence."
  (dolist (fence '("```" "~~~"))
    (let* ((raw (concat fence "\n| A | B |\n|---|---|\n| one | two |\n"))
           (formatted (hermes-chat--fontify-markdown-string raw)))
      (should (equal (substring-no-properties formatted) raw))
      (should-not (text-property-not-all
                   0 (length formatted) 'hermes-chat-table nil formatted))
      (hermes-test-with-chat-buffer
       (hermes-chat--insert-entry
        (hermes-chat--make-entry 'assistant raw 'done))
       (should (string-match-p (regexp-quote raw) (buffer-string)))
       (should-not (next-button (point-min)))))))

(ert-deftest hermes-chat-markdown-leaves-fenced-tables-alone ()
  "A table inside either closed fence stays inline, unlike a table after it."
  (dolist (fence '("```" "~~~"))
    (let* ((table "| short | x |\n|---|---|\n| much longer cell | y |\n")
           (raw (concat fence "\n" table fence "\n"))
           (s (hermes-chat--fontify-markdown-string raw)))
      (should (equal (substring-no-properties s) raw))
      (should-not (text-property-not-all 0 (length s) 'hermes-chat-table nil s))
      (with-temp-buffer
        (hermes-chat--insert-markdown (concat raw "\n" table))
        (should (string-prefix-p raw (buffer-string)))
        (let ((button (next-button (point-min))))
          (should button)
          (should (> (button-start button) (length raw)))
          (should (equal (button-get button 'hermes-chat-table) table))
          (should-not (next-button (button-end button))))))))

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

(ert-deftest hermes-chat-late-submit-result-cannot-settle-newer-turn ()
  "A stale prompt acceptance cannot mutate its successor turn."
  (let ((client (hermes-test--dashboard-client)) callback resolves)
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
                 (push (cons text (plist-get args :resolve)) resolves))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-test--emit-dashboard-event client "message.start" nil)
         (hermes-test--emit-dashboard-event
          client "message.complete" '((status . "done")))
         ;; Terminal output alone does not acknowledge the submit request.
         (funcall (cdr (assoc "first" resolves)) '((status . "accepted")))
         (insert "second")
         (hermes-chat-send)
         (should (assoc "second" resolves))
         (let ((before-entries (copy-tree (hermes-chat--entries)))
               (before-queue (copy-tree hermes-chat--queued-messages))
               (before-assistant hermes-chat--pending-assistant-id)
               (before-header (copy-tree hermes-chat--status-state)))
           (funcall (cdr (assoc "first" resolves)) '((status . "queued")))
           (should (equal (hermes-chat--entries) before-entries))
           (should (equal hermes-chat--queued-messages before-queue))
           (should (equal hermes-chat--pending-assistant-id before-assistant))
           (should (equal hermes-chat--status-state before-header))))))))

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
  "A busy redirect preserves the question, full user input and subsequent reply."
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
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . "Which approach?")))
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
         (should (equal
                  (mapcar (lambda (entry)
                            (list (plist-get entry :role) (plist-get entry :content)))
                          (seq-filter (lambda (entry)
                                        (memq (plist-get entry :role) '(user assistant)))
                                      (hermes-chat--entries)))
                  '((user "first") (assistant "Which approach?")
                    (user "second") (assistant "continued run"))))
         (should (eq (plist-get (car (last (hermes-chat--entries))) :role)
                     'assistant))
         (should-not (equal (plist-get (hermes-test--last-assistant-entry) :id)
                            assistant-id))
         (should (equal (plist-get (hermes-test--last-assistant-entry) :content)
                        "continued run")))))))

(ert-deftest hermes-chat-dashboard-idle-submit-steered-preserves-user ()
  "An authoritative steer receipt keeps the full optimistic user turn once."
  (dolist (terminal-first '(nil t))
    (hermes-test-with-dashboard-prompt-session (client)
      (funcall (hermes-chat--submit-resolve-callback
                (current-buffer) hermes-chat--unsettled-submit-context)
               '((status . "streaming")))
      (hermes-test--emit-dashboard-prompt
       client "message.complete" '((text . "First reply")))
      (let ((answer (concat "Accepted correction α\n" (make-string 220 ?λ) "\n終わり"))
            resolve submitted)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (_client text &rest args)
                     (setq submitted text
                           resolve (plist-get args :resolve)))))
          (should-not hermes-chat--pending-assistant-id)
          (should-not hermes-chat--dashboard-running-p)
          (insert answer)
          (hermes-chat-send)
          (should (equal submitted answer))
          (let ((assistant-id hermes-chat--pending-assistant-id)
                (generation hermes-chat--transport-generation)
                (receipt resolve))
            (insert "draft stays")
            (hermes-test--emit-dashboard-prompt
             client "message.delta" '((text . "Live output")))
            (when terminal-first
              (hermes-test--emit-dashboard-prompt
               client "message.complete" '((text . "Live output"))))
            (funcall receipt '((status . "steered")))
            (should (equal
                     (mapcar (lambda (entry)
                               (list (plist-get entry :role) (plist-get entry :content)))
                             (seq-filter (lambda (entry)
                                           (memq (plist-get entry :role) '(user assistant)))
                                         (hermes-chat--entries)))
                     `((user "trigger prompt") (assistant "First reply")
                       (user ,answer) (assistant "Live output"))))
            (should (string-match-p (regexp-quote answer) (buffer-string)))
            (should-not (string-match-p "Steered:" (buffer-string)))
            (should (equal (hermes-chat-input-string) "draft stays"))
            (should (= generation hermes-chat--transport-generation))
            (should (equal hermes-chat--pending-assistant-id
                           (unless terminal-first assistant-id)))
            (should-not hermes-chat--unsettled-submit-context)
            (let ((entries (copy-tree (hermes-chat--entries))))
              (funcall receipt '((status . "steered")))
              (should (equal entries (hermes-chat--entries))))
            (unless terminal-first
              (hermes-test--emit-dashboard-prompt
               client "message.complete" '((text . "Live output"))))
            (should (eq (plist-get (hermes-test--last-assistant-entry) :status) 'done))
            (hermes-chat-send)
            (let ((context hermes-chat--unsettled-submit-context)
                  (pending hermes-chat--pending-assistant-id)
                  (entries (copy-tree (hermes-chat--entries))))
              (should context)
              (funcall receipt '((status . "steered")))
              (should (eq context hermes-chat--unsettled-submit-context))
              (should (equal pending hermes-chat--pending-assistant-id))
              (should (equal entries (hermes-chat--entries))))))))))

(ert-deftest hermes-chat-redirected-segments-preserve-cumulative-finals ()
  "Repeated redirects split one stream, including corrected cumulative finals."
  (dolist (final '("Answer" "Q1.Q2.Answer" "Corrected answer"))
    (hermes-test-with-dashboard-prompt-session (client)
      (funcall (hermes-chat--submit-resolve-callback
                (current-buffer) hermes-chat--unsettled-submit-context)
               '((status . "streaming")))
      (let ((generation hermes-chat--transport-generation)
            (answer (concat "Custom α answer\n" (make-string 220 ?x)))
            resolve)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (_client _text &rest args)
                     (setq resolve (plist-get args :resolve)))))
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Q1.")))
          (insert answer)
          (hermes-chat-send)
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Q2.")))
          (funcall resolve '((status . "redirected")))
          (insert "Another answer")
          (hermes-chat-send)
          (insert "draft stays")
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Answer")))
          (hermes-test--emit-dashboard-prompt client "message.complete"
                                             `((text . ,final) (status . "done")))
          (funcall resolve '((status . "steered")))
          (let ((entries (seq-filter
                          (lambda (entry) (memq (plist-get entry :role) '(user assistant)))
                          (hermes-chat--entries))))
            (should (equal (mapcar (lambda (entry) (plist-get entry :content)) entries)
                           (list "trigger prompt" "Q1." answer "Q2." "Another answer"
                                 (if (equal final "Corrected answer") final "Answer")))))
          (should (string-match-p (regexp-quote answer) (buffer-string)))
          (should (equal (hermes-chat-input-string) "draft stays"))
          (should (= generation hermes-chat--transport-generation))
          (should-not hermes-chat--pending-assistant-id))))))

(ert-deftest hermes-chat-redirected-interim-preview-is-not-duplicated ()
  "Real interim boundaries reset local prefixes and previewed finals settle once."
  (dolist (boundary '(before after))
    (hermes-test-with-dashboard-prompt-session (client)
      (funcall (hermes-chat--submit-resolve-callback
                (current-buffer) hermes-chat--unsettled-submit-context)
               '((status . "streaming")))
      (let (resolve)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (_client _text &rest args)
                     (setq resolve (plist-get args :resolve)))))
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Question")))
          (when (eq boundary 'before)
            (hermes-test--emit-dashboard-prompt client "message.interim" '((text . "Question"))))
          (insert "User answer")
          (hermes-chat-send)
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Reply")))
          (let ((preview (if (eq boundary 'after) "QuestionReply" "Reply")))
            (hermes-test--emit-dashboard-prompt client "message.interim" `((text . ,preview)))
            (hermes-test--emit-dashboard-prompt
             client "message.complete" `((text . ,preview) (status . "done")
                                         (response_previewed . t))))
          (funcall resolve '((status . "redirected")))
          (should (equal
                   (mapcar (lambda (entry) (plist-get entry :content))
                           (seq-filter (lambda (entry)
                                         (memq (plist-get entry :role) '(user assistant)))
                                       (hermes-chat--entries)))
                   '("trigger prompt" "Question" "User answer" "Reply"))))))))

(ert-deftest hermes-chat-redirected-tool-completion-keeps-one-row ()
  "Live tool ownership follows a redirected stream without a stranded row."
  (hermes-test-with-dashboard-prompt-session (client)
    (let ((assistant-id hermes-chat--pending-assistant-id) resolve)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                 (lambda (_client _text &rest args)
                   (setq resolve (plist-get args :resolve)))))
        (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Question")))
        (hermes-chat--handle-transport-event
         assistant-id '(:type tool :event "tool.start" :tool-call-id "call-1"
                       :name "terminal" :content "Inspect" :status "running"))
        (hermes-chat--submit-busy-dashboard-content "Answer")
        (funcall resolve '((status . "redirected")))
        (hermes-chat--handle-transport-event
         hermes-chat--pending-assistant-id
         '(:type tool :event "tool.complete" :tool-call-id "call-1"
           :name "terminal" :content "Complete" :status "done"))
        (let ((tools (seq-filter (lambda (entry) (eq (plist-get entry :role) 'tool))
                                 (hermes-chat--entries))))
          (should (= (length tools) 1))
          (should (equal (hermes-chat--status-name (plist-get (car tools) :status)) "done")))))))

(ert-deftest hermes-chat-redirected-rejection-and-disconnect-preserve-order ()
  "Unaccepted input remains recoverable; stale receipts cannot insert it."
  (dolist (outcome '(reject disconnect))
    (hermes-test-with-dashboard-prompt-session (client)
      (let (resolve reject)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (_client _text &rest args)
                     (setq resolve (plist-get args :resolve)
                           reject (plist-get args :reject)))))
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Before")))
          (hermes-chat--submit-busy-dashboard-content "Unaccepted answer")
          (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "After")))
          (if (eq outcome 'reject)
              (funcall reject "Rejected")
            (hermes-dashboard-transport--dispatch-event
             client '(:type status :status "closed" :content "Disconnected")))
          (should (string-prefix-p "BeforeAfter"
                                   (plist-get (hermes-test--last-assistant-entry) :content)))
          (should (hermes-test--control-content-preserved-p "Unaccepted answer"))
          (let ((entries (copy-tree (hermes-chat--entries)))
                (draft (hermes-chat-input-string)))
            (funcall resolve '((status . "redirected")))
            (should (equal entries (hermes-chat--entries)))
            (should (equal draft (hermes-chat-input-string)))))))))

(ert-deftest hermes-chat-previewed-final-preserves-later-stream ()
  "A final receipt for an earlier preview cannot overwrite later assistant text."
  (hermes-test-with-dashboard-prompt-session (client)
    (hermes-test--emit-dashboard-prompt client "message.interim" '((text . "Preview")))
    (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Later response")))
    (hermes-test--emit-dashboard-prompt
     client "message.complete" '((text . "Preview") (status . "done") (response_previewed . t)))
    (should (equal (plist-get (hermes-test--last-assistant-entry) :content) "Later response"))))

(ert-deftest hermes-chat-redirected-visible-unselected-preserves-draft ()
  "Redirect receipt and held output leave an unselected chat's editable tail intact."
  (save-window-excursion
    (hermes-test-with-dashboard-prompt-session (client)
      (let ((chat (current-buffer)) resolve)
        (set-window-buffer (split-window-right) chat)
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (with-current-buffer chat
          (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                     (lambda (_client _text &rest args)
                       (setq resolve (plist-get args :resolve)))))
            (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Question")))
            (insert "Custom answer")
            (hermes-chat-send)
            (insert "draft stays")
            (let ((offset (- (point) (marker-position hermes-chat--input-marker))))
              (hermes-test--emit-dashboard-prompt client "message.delta" '((text . "Reply")))
              (hermes-test--emit-dashboard-prompt client "message.complete" '((text . "Reply")))
              (funcall resolve '((status . "redirected")))
              (should (equal (hermes-chat-input-string) "draft stays"))
              (should (= (- (point) (marker-position hermes-chat--input-marker)) offset))
              (should (string-match-p "Question" (buffer-substring-no-properties
                                                  (point-min) hermes-chat--input-marker)))
              (should (string-match-p "> Custom answer" (buffer-string)))
              (should-not (eq (window-buffer (selected-window)) chat)))))))))

(ert-deftest hermes-chat-redirected-final-correction-does-not-guess-prefix ()
  "Without a matching live suffix, a final-only correction is kept verbatim."
  (dolist (stream '("" "Old draft" "Question restated"))
    (hermes-test-with-chat-buffer
     (hermes-chat--insert-entry
      (list :id "a1" :role 'assistant :status 'streaming
            :stream-prefix "Question" :content stream))
     (hermes-chat--handle-transport-event "a1" '(:type done :content "Question restated"))
     (should (equal (hermes-chat--entry-content-by-id "a1") "Question restated")))))

(ert-deftest hermes-chat-previewed-final-accepts-live-legacy-interim ()
  "Reloaded code accepts interim entries retained from the previous layout."
  (dolist (stream '("" "Later response"))
    (hermes-test-with-chat-buffer
     (hermes-chat--insert-entry '(:id "old" :role assistant :content "Preview" :status done))
     (hermes-chat--insert-entry (list :id "live" :role 'assistant :content stream :status 'streaming))
     (setq hermes-chat--dashboard-interim-assistant-id "old"
           hermes-chat--pending-assistant-id "live")
     (hermes-chat--handle-transport-event
      "live" '(:type done :content "Preview" :response-previewed t))
     (should (equal
              (mapcar (lambda (entry) (plist-get entry :content)) (hermes-chat--entries))
              (if (string-empty-p stream) '("Preview") (list "Preview" stream)))))))

(ert-deftest hermes-chat-provider-wait-is-not-reasoning ()
  "Provider notices remain neutral activity, never an inferred reasoning state."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
   (setq hermes-chat--pending-assistant-id "a1"
         hermes-chat--dashboard-running-p t)
   (hermes-chat--handle-transport-event
    "a1" '(:type thinking :event "thinking.delta" :content "Rate limited; waiting 60s"))
   (should (equal (plist-get hermes-chat--status-state :activity) "Working"))
   (should (eq (plist-get hermes-chat--status-state :status) 'running))
   (should hermes-chat--dashboard-running-p)
   (should (string-match-p "Working…" (buffer-string)))
   (should-not (string-match-p "Thinking\\|Rate limited" (buffer-string)))))

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
       (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                              (hermes-chat--entries))
                      '(user status assistant)))
       (funcall (car (last callbacks)) '(:type done))
       (should (equal sent '("second" "first")))
       (should hermes-chat--pending-assistant-id)
       (let ((roles (mapcar (lambda (entry) (plist-get entry :role))
                            (hermes-chat--entries))))
         (should (equal roles '(user status assistant user assistant))))))))

(ert-deftest hermes-chat-local-status-preserves-newer-queued-turn-order ()
  (hermes-test-with-chat-buffer
   (let* ((first (hermes-chat--insert-backend-turn "first"))
          (first-assistant-id (cdr first)))
     (setq hermes-chat--pending-assistant-id first-assistant-id)
     (hermes-chat--record-server-queued-content "second")
     (insert "draft")
     (let ((input-offset (- (point) (marker-position hermes-chat--input-marker))))
       (hermes-chat--insert-local-status "one")
       (hermes-chat--insert-local-status "two")
       (let ((entries (hermes-chat--entries)))
         (should (equal (mapcar (lambda (entry) (plist-get entry :role)) entries)
                        '(user assistant user status status assistant)))
         (should (equal (mapcar (lambda (entry) (plist-get entry :content))
                                (cl-remove-if-not
                                 (lambda (entry)
                                   (eq (plist-get entry :role) 'status))
                                 entries))
                        '("one" "two"))))
       (should (equal (hermes-chat-input-string) "draft"))
       (should (= (- (point) (marker-position hermes-chat--input-marker))
                  input-offset))))))

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
         (hermes-test--emit-dashboard-event client "thinking.delta" '((text . "pondering")))
         (let ((id (concat hermes-chat--pending-assistant-id ":activity")))
           (should (gethash id hermes-chat--nodes))
           (hermes-chat-interrupt)
           (should-not (gethash id hermes-chat--nodes)))
         (hermes-test--emit-dashboard-event client "thinking.delta" '((text . "pondering")))
         (funcall interrupt-reject "not interruptible")
         (should (gethash (concat hermes-chat--pending-assistant-id ":activity") hermes-chat--nodes))
         (hermes-test--emit-dashboard-event
          client "message.delta" '((text . " retained")))
         (should-not (gethash (concat hermes-chat--pending-assistant-id ":activity") hermes-chat--nodes))
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

(ert-deftest hermes-chat-interrupt-missing-session-settles-once ()
  "A missing session cannot let old reconciliation affect its successor."
  (let ((client (hermes-test--dashboard-client))
        interrupt-resolve idle-resolve idle-reject scheduled submits
        (resume-count 0))
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
                            (stored_session_id . "sid-missing")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (funcall (plist-get args :resolve)
                          '((status . "streaming")))))
              ((symbol-function 'hermes-dashboard-transport-session-interrupt)
               (lambda (_client &rest args)
                 (setq interrupt-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (cl-incf resume-count)
                 (if (= resume-count 1)
                     (setq idle-resolve (plist-get args :resolve)
                           idle-reject (plist-get args :reject))
                   (error "late reconciliation failure"))))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (when (eq function #'hermes-chat--dashboard-reconcile-idle)
                   (push (cons function args) scheduled))
                 (timer-create))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-interrupt-and-send "second")
         (funcall interrupt-resolve '((status . "interrupted")))
         (should (= (length scheduled) 1))
         (let ((old-reconciliation (car scheduled)))
           (apply (car old-reconciliation) (cdr old-reconciliation))
           (funcall idle-reject "session not found")
           (should (plist-get (cadr old-reconciliation) :terminal-p))
           (should (equal submits '("second" "first")))
           (should (= (length scheduled) 1))
           (should hermes-chat--dashboard-running-p)
           (let ((successor hermes-chat--pending-assistant-id))
             (apply (car old-reconciliation) (cdr old-reconciliation))
             (funcall idle-reject "temporary failure")
             (funcall idle-resolve '((running . nil)))
             (should (= resume-count 1))
             (should (= (length scheduled) 1))
             (should hermes-chat--dashboard-running-p)
             (should (equal hermes-chat--pending-assistant-id successor))
             (should hermes-chat--process)
             (should-not (hermes-test--queued-contents)))))))))

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

(ert-deftest hermes-chat-steer-rejection-after-disconnect-does-not-resubmit ()
  "A stale steer rejection cannot submit into a disconnected chat."
  (let ((client (hermes-test--dashboard-client))
        steer-reject submits (resumes 0))
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
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resumes (1+ resumes))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-replacement")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (_client text &rest args)
                 (push text submits)
                 (when-let* ((resolve (plist-get args :resolve)))
                   (funcall resolve '((status . "streaming"))))))
              ((symbol-function 'hermes-dashboard-transport-session-steer)
               (lambda (_client _text &rest args)
                 (setq steer-reject (plist-get args :reject))))
              ((symbol-function 'hermes-dashboard-transport-stop) #'ignore))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (insert "first")
         (hermes-chat-send)
         (hermes-chat-steer-message "second")
         (hermes-chat-disconnect)
         (funcall steer-reject "late rejection")
         (should (= resumes 0))
         (should (equal submits '("first")))
         (should-not (hermes-test--queued-contents))
         (should-not hermes-chat--dashboard-client))))))

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
  (should (functionp (hermes-chat--native-slash-handler "compact")))
  (should (functionp (hermes-chat--native-slash-handler "compress")))
  (should-not (hermes-chat--native-slash-handler "definitely-not-a-command"))
  (should-not (hermes-chat--native-slash-handler nil)))

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
                 (funcall (plist-get args :resolve) '((killed . 2)))))
              ((symbol-function 'yes-or-no-p) (lambda (_) t)))
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

(ert-deftest hermes-chat-compact-uses-session-compress ()
  "/compact uses session.compress and settles one progress line."
  (let ((client (hermes-test--dashboard-client))
        compress-args resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _)
                 (ert-fail "compact must not use slash.exec")))
              ((symbol-function 'hermes-dashboard-transport-session-compress)
               (lambda (_client &rest args)
                 (setq compress-args args
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (hermes-chat--insert-entry (hermes-chat--make-entry 'user "hi"))
       (hermes-chat--insert-entry
        (hermes-chat--make-entry 'assistant "settled reply" 'done))
       (insert "/compact keep blobs")
       (hermes-chat-send)
       (should (equal (plist-get compress-args :session-id) "sid-active"))
       (should (equal (plist-get compress-args :focus-topic) "keep blobs"))
       (should (equal (mapcar (lambda (entry) (plist-get entry :role))
                             (hermes-chat--entries))
                      '(user assistant status)))
       (should (string-match-p "Compressing" (buffer-string)))
       (hermes-chat--run-turn-reducer
        "a1" '(:type status :event "status.update" :status "compressing"
                     :session-id "sid-active"
                     :content "compressing 12 messages (~4,000 tok)…"))
       (let ((lines (ewoc-collect
                     hermes-chat--ewoc
                     (lambda (entry)
                       (eq (plist-get entry :role) 'status)))))
         (should (= 1 (length lines)))
         (should (string-match-p "4,000" (plist-get (car lines) :content))))
       (funcall resolve
                '((status . "compressed")
                  (summary . ((headline . "Compressed: 40 → 12 messages")
                              (token_line . "Approx request size: ~120,000 → ~40,000 tokens")))))
       (should (string-match-p "Compressed: 40 → 12" (buffer-string)))
       (should (string-match-p "120,000" (buffer-string)))
       (should-not (string-match-p "Compressing" (buffer-string)))
       (should (= 1 (length
                     (ewoc-collect
                      hermes-chat--ewoc
                      (lambda (entry)
                        (eq (plist-get entry :role) 'status))))))))))

(ert-deftest hermes-chat-compact-reject-settles-progress-line ()
  "A rejected session.compress replaces the pending compress line."
  (let ((client (hermes-test--dashboard-client)) reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _)
                 (ert-fail "compact must not use slash.exec")))
              ((symbol-function 'hermes-dashboard-transport-session-compress)
               (lambda (_client &rest args)
                 (setq reject (plist-get args :reject)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/compress")
       (hermes-chat-send)
       (should (string-match-p "Compressing" (buffer-string)))
       (funcall reject "session busy — /interrupt the current turn before /compress")
       (should (string-match-p "session busy" (buffer-string)))
       (should-not (string-match-p "Compressing" (buffer-string)))))))

(ert-deftest hermes-chat-compact-ignores-stale-session-result ()
  "A late session.compress result does not settle a successor chat."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _)
                 (ert-fail "compact must not use slash.exec")))
              ((symbol-function 'hermes-dashboard-transport-session-compress)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/compact")
       (hermes-chat-send)
       (setq hermes-chat--lifecycle-generation
             (hermes-chat--next-lifetime-token))
       (funcall resolve
                '((summary . ((headline . "Compressed: 40 → 12 messages")))))
       (should (string-match-p "Compressing" (buffer-string)))
       (should-not (string-match-p "Compressed: 40" (buffer-string)))))))

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
       (should-not (string-match-p "ultra" (hermes-test--header-line-string)))
       (should (string-match-p "ultra" (hermes-chat--session-details-text)))
       (should-not (string-match-p "high" (hermes-test--header-line-string)))))))

(ert-deftest hermes-chat-reasoning-query-follows-public-setter-readback ()
  "A public effort pick, readback, and bare query share one session authority."
  (let ((client (hermes-test--dashboard-client))
        (efforts (make-hash-table :test #'equal))
        (undo-in-region nil) (last-command nil)
        (undo-equiv-table (make-hash-table :test #'eq)) requests query-resolve)
    ;; Keep the public setter, typed RPCs, command owner, and renderer real.
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (sent-client method params resolve &optional _reject)
                 (push method requests)
                 (should (eq sent-client client))
                 (should (equal (alist-get 'key params) "reasoning"))
                 (should (equal (alist-get 'session_id params) "sid-active"))
                 (pcase method
                   ("config.set"
                    (puthash (alist-get 'session_id params)
                             (alist-get 'value params) efforts)
                    (funcall resolve `((value . ,(alist-get 'value params)))))
                   ("config.get"
                    (if hermes-chat--command-owner
                        (setq query-resolve resolve)
                      (funcall resolve
                               `((value . ,(gethash "sid-active" efforts))
                                 (display . "hide")))))
                   (_ (ert-fail "Bare reasoning must not use slash.exec"))))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (dolist (effort '("max" "xhigh"))
         (hermes-chat-set-reasoning effort)
         (should (equal (gethash "sid-active" efforts) effort))
         (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                        effort))
         (should-not hermes-chat--command-owner)
         (insert "/reasoning")
         (hermes-chat-send)
         (should-not (member "slash.exec" requests))
         (should hermes-chat--command-owner)
         (should (functionp query-resolve))
         (undo-boundary)
         (insert "new draft α")
         (undo-boundary)
         (let ((undo buffer-undo-list)
               (point-offset (- (point) (hermes-chat--input-position))))
           (funcall query-resolve `((value . ,(gethash "sid-active" efforts))
                                   (display . "hide")))
           (should-not hermes-chat--command-owner)
           (should (equal (hermes-chat-input-string) "new draft α"))
           (should (eq buffer-undo-list undo))
           (should (= (- (point) (hermes-chat--input-position)) point-offset))
           (let ((transcript (buffer-substring (point-min) hermes-chat--input-marker)))
             (hermes-test--draft-undo-command #'undo-only)
             (should (equal (hermes-chat-input-string) ""))
             (hermes-test--draft-undo-command #'undo-redo)
             (should (equal (hermes-chat-input-string) "new draft α"))
             (should (equal (buffer-substring (point-min) hermes-chat--input-marker)
                            transcript)))
           (should (string-match-p (concat "Reasoning effort:  " effort)
                                   (buffer-string)))
           (should (string-match-p "Reasoning display: off" (buffer-string)))
           (should-not (string-match-p "clamped to\\|medium" (buffer-string))))
         (hermes-chat--delete-input-tail))
       (should (equal (nreverse requests)
                      '("config.set" "config.get" "config.get"
                        "config.set" "config.get" "config.get")))))))

(ert-deftest hermes-chat-reasoning-query-reports-only-returned-fields ()
  "Reasoning reports distinguish missing values, disabled effort, and display."
  (dolist (case '((nil "unknown" "unknown")
                  (((value . "max")) "max" "unknown")
                  (((display . "show")) "unknown" "on")
                  (((value . "none") (display . "hide")) "none (disabled)" "off")
                  (((value . "") (display . "")) "unknown" "unknown")
                  (((value . "future-effort") (display . "future-display"))
                   "future-effort" "future-display")))
    (let ((expected (format "Reasoning effort:  %s\nReasoning display: %s"
                            (nth 1 case) (nth 2 case))))
      (should (equal (hermes-chat--reasoning-report (car case)) expected))))
  (should (equal (hermes-chat--reasoning-report
                  '(:value "medium" :display "show"))
                 "Reasoning effort:  medium\nReasoning display: on"))
  (let ((result (make-hash-table :test #'equal)))
    (puthash "value" "xhigh" result)
    (puthash "display" "hide" result)
    (puthash "reasoning_full" t result)
    (should (equal (hermes-chat--reasoning-report result)
                   "Reasoning effort:  xhigh\nReasoning display: off"))))

(ert-deftest hermes-chat-reasoning-query-settles-rejection-and-stale-callbacks ()
  "A query settles once and cannot paint or release a successor's owner."
  (dolist (terminal '(reject session disconnect kill))
    (let ((client (hermes-test--dashboard-client)) resolve reject)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                 (lambda (_client method params on-success on-error)
                   (should (equal method "config.get"))
                   (should (equal params '((key . "reasoning")
                                          (session_id . "sid-active"))))
                   (setq resolve on-success reject on-error))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-active-session-id "sid-active"
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--runtime-flags '(:reasoning-effort "low"))
         (insert "/reasoning")
         (hermes-chat-send)
         (should hermes-chat--command-owner)
         (pcase terminal
           ('reject (funcall reject "Reasoning query rejected"))
           ('session
            (setq hermes-chat--dashboard-active-session-id "sid-new")
            (funcall resolve '((value . "max") (display . "hide"))))
           ('disconnect (hermes-chat-disconnect))
           ('kill (kill-buffer (current-buffer))))
         (if (eq terminal 'kill)
             (with-temp-buffer
               (insert "Unrelated buffer")
               (funcall resolve '((value . "max") (display . "show")))
               (funcall reject "late failure")
               (should (equal (buffer-string) "Unrelated buffer")))
           (should-not hermes-chat--command-owner)
           (should (equal (plist-get hermes-chat--runtime-flags :reasoning-effort)
                          "low"))
           (when (eq terminal 'reject)
             (should (string-match-p "Reasoning query rejected" (buffer-string)))
             (should (eq (plist-get hermes-chat--status-state :status) 'error)))
           (let ((owner (hermes-chat--command-start))
                 (text (buffer-string))
                 (undo (copy-tree buffer-undo-list)))
             (funcall resolve '((value . "max") (display . "show")))
             (funcall reject "late failure")
             (should (eq hermes-chat--command-owner owner))
             (should (equal (buffer-string) text))
             (should (equal buffer-undo-list undo)))))))))

(ert-deftest hermes-chat-reasoning-query-transport-error-and-timeout ()
  "Real RPC rejection and timeout release the query without a worker fallback."
  (dolist (failure '(error timeout stop))
    (let ((client (hermes-test--dashboard-client)) frames
          (hermes-dashboard-transport-websocket-close-function #'ignore))
      (let ((hermes-dashboard-transport-websocket-send-function
             (lambda (_socket frame) (push frame frames))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-active-session-id "sid-active"
               hermes-chat--dashboard-session-ready-p t)
         (insert "/reasoning")
         (hermes-chat-send)
         (let* ((pending (hermes-dashboard-transport-client-pending client))
                (id (car (hash-table-keys pending)))
                (request (gethash id pending)))
           (should (= (length frames) 1))
           (should (equal (plist-get request :method) "config.get"))
           (should hermes-chat--command-owner)
           (pcase failure
             ('error
              (hermes-dashboard-transport--handle-frame
               client (hermes-dashboard-transport--encode-frame
                       `((jsonrpc . "2.0") (id . ,id)
                         (error . ((code . 4002) (message . "Reasoning unavailable")))))))
             ('timeout (hermes-dashboard-transport--on-request-timeout client id))
             ('stop (hermes-dashboard-transport-stop client "Socket closed")))
           (should (= (hash-table-count pending) 0))
           (should-not hermes-chat--command-owner)
           (should-not hermes-chat--runtime-flags)
           (should (eq (plist-get hermes-chat--status-state :status) 'error))
           (should (string-match-p
                    (pcase failure
                      ('error "Reasoning unavailable")
                      ('timeout "[Tt]ime")
                      ('stop "Socket closed"))
                    (buffer-string)))
           (should (= (length frames) 1))))))))

(ert-deftest hermes-chat-reasoning-query-keeps-busy-guards ()
  "Existing exclusive command, handoff, and creation guards refuse a bare query."
  (dolist (busy '(handoff creation command))
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (&rest _) (ert-fail "Busy query must not dispatch"))))
      (hermes-test-with-chat-buffer
       (pcase busy
         ('handoff (setq hermes-chat--handoff-owner 'handoff))
         ('creation (setq hermes-chat--create-override-owner 'creation))
         ('command (hermes-chat--command-start)))
       (insert "/reasoning")
       (let ((text (buffer-string)) (undo (copy-tree buffer-undo-list)))
         (should-error (hermes-chat-send) :type 'user-error)
         (should (equal (buffer-string) text))
         (should (equal buffer-undo-list undo)))))))

(ert-deftest hermes-chat-reasoning-query-preserves-argument-routing ()
  "Only a bare query changes route; display, effort, and flag handling stays put."
  (dolist (case '(("max --session" "config.set" "max" "sid-active")
                  ("xhigh --global" "config.set" "xhigh" nil)
                  ("show" "config.set" "show" "sid-active")
                  ("hide" "config.set" "hide" "sid-active")
                  ("full" "config.set" "full" "sid-active")
                  ("clamp" "config.set" "clamp" "sid-active")
                  ("unknown" "config.set" "unknown" "sid-active")
                  ("--global" "slash.exec" nil "sid-active")
                  ("--session" "slash.exec" nil "sid-active")))
    (let ((client (hermes-test--dashboard-client)) request)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                 (lambda (_client method params _resolve _reject)
                   (setq request (list method params)))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-active-session-id "sid-active"
               hermes-chat--dashboard-session-ready-p t)
         (insert (concat "/reasoning " (car case)))
         (hermes-chat-send)
         (should (equal (car request) (nth 1 case)))
         (should (equal (alist-get 'value (cadr request)) (nth 2 case)))
         (should (equal (alist-get 'session_id (cadr request)) (nth 3 case)))
         (when (equal (car request) "slash.exec")
           (should (equal (alist-get 'command (cadr request))
                          (concat "reasoning " (car case))))))))))

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

(ert-deftest hermes-chat-model-command-sets-explicit-model-on-live-session ()
  "`/model MODEL' uses typed config.set without opening the model picker."
  (let ((client (hermes-test--dashboard-client)) request)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (setq request
                       (list key value (plist-get args :session-id)
                             (plist-get args :confirm-expensive-model)))
                 (funcall (plist-get args :resolve)
                          `((value . ,value)))))
              ((symbol-function 'hermes-chat-switch-model)
               (lambda (&rest _args)
                 (ert-fail "Explicit /model must not open completing-read")))
              ((symbol-function 'hermes-dashboard-transport-slash-exec)
               (lambda (&rest _args)
                 (ert-fail "Explicit /model must not use slash.exec"))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/model gpt-5.6 --provider openai-codex")
       (hermes-chat-send)
       (should (equal request
                      '("model" "gpt-5.6 --provider openai-codex"
                        "sid-active" nil)))
       (should-not hermes-chat--command-owner)
       (should (string-match-p "Model set to gpt-5.6"
                               (buffer-string)))))))

(ert-deftest hermes-chat-model-command-confirms-expensive-model ()
  "`/model MODEL' retries an expensive switch only after confirmation."
  (let ((client (hermes-test--dashboard-client))
        (calls 0)
        confirms)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (cl-incf calls)
                 (push (plist-get args :confirm-expensive-model) confirms)
                 (funcall (plist-get args :resolve)
                          (if (= calls 1)
                              '((confirm_required . t)
                                (confirm_message . "Expensive model"))
                            '((value . "gpt-5.6"))))))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-active"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/model gpt-5.6 --provider openai-codex")
       (hermes-chat-send)
       (should (= calls 2))
       (should (equal (nreverse confirms) '(nil t)))
       (should-not hermes-chat--command-owner)))))

(ert-deftest hermes-chat-model-command-does-not-confirm-into-replacement-session ()
  "An expensive confirmation cannot redirect its retry to a replacement session."
  (let ((client (hermes-test--dashboard-client)) calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (push (plist-get args :session-id) calls)
                 (funcall (plist-get args :resolve)
                          '((confirm_required . t)
                            (confirm_message . "Expensive model")))))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (setq hermes-chat--dashboard-active-session-id "sid-new")
                 t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/model gpt-5.6 --provider openai-codex")
       (hermes-chat-send)
       (should (equal (nreverse calls) '("sid-old")))
       (should-not hermes-chat--command-owner)))))

(ert-deftest hermes-chat-model-command-stale-confirmation-releases-owner ()
  "A confirmation response for a replaced session cannot leave ownership stuck."
  (let ((client (hermes-test--dashboard-client)) resolve prompted)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) (setq prompted t))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-session-ready-p t)
       (insert "/model gpt-5.6 --provider openai-codex")
       (hermes-chat-send)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((confirm_required . t)))
       (should-not prompted)
       (should-not hermes-chat--command-owner)))))

(ert-deftest hermes-chat-model-command-without-argument-keeps-picker ()
  "Bare `/model' retains the interactive cached model picker."
  (let (picked)
    (cl-letf (((symbol-function 'hermes-chat-switch-model)
               (lambda (&rest _args) (setq picked t))))
      (hermes-test-with-chat-buffer
       (insert "/model")
       (hermes-chat-send)
       (should picked)))))

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
                    '(user status assistant user assistant)))
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

(ert-deftest hermes-chat-dashboard-queue-drain-requires-attached-session ()
  "A detached dashboard preserves queued occurrences until it is attached."
  (let ((hermes-transport-send-function #'hermes-transport-send)
        submitted)
    (hermes-test-with-chat-buffer
     (let ((hermes-chat--submit-function
            (lambda (content &rest _args) (push content submitted))))
       (setq hermes-chat--session-id "stored-session")
       (hermes-chat--queue-content "duplicate")
       (hermes-chat--queue-content "duplicate")
       (insert "draft survives")
       (let* ((queue hermes-chat--queued-messages)
              (ids (mapcar (lambda (entry) (plist-get entry :id)) queue)))
         (hermes-chat--drain-queued-message)
         (should-not submitted)
         (should-not hermes-chat--queued-submit-id)
         (should (eq hermes-chat--queued-messages queue))
         (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                                hermes-chat--queued-messages)
                        ids))
         (should (equal (hermes-test--queued-contents)
                        '("duplicate" "duplicate")))
         (should (equal (hermes-chat-input-string) "draft survives"))
         (should (equal hermes-chat--session-id "stored-session")))
       (setq hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "live-session")
       (hermes-chat--drain-queued-message)
       (should-not submitted)
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client))
       (hermes-chat--drain-queued-message)
       (should (equal submitted '("duplicate")))
       (should (equal hermes-chat--queued-submit-id
                      (plist-get (car hermes-chat--queued-messages) :id)))))))

(ert-deftest hermes-chat-non-dashboard-queue-drain-remains-ready ()
  "A non-Dashboard queue still drains when the transport is idle."
  (let ((hermes-transport-send-function (lambda (&rest _args)))
        submitted)
    (hermes-test-with-chat-buffer
     (let ((hermes-chat--submit-function
            (lambda (content &rest _args) (setq submitted content))))
       (hermes-chat--queue-content "queued")
       (hermes-chat--drain-queued-message)
       (should (equal submitted "queued"))))))

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

(ert-deftest hermes-chat-header-brand-is-optional-first-cell ()
  "Branding inherits the header face and never displaces existing information."
  (hermes-test-with-chat-buffer
    (setq hermes-chat--status-state '(:status error)
          hermes-chat--runtime-flags '(:yolo t))
    (dolist (width '(1 8 12 20 30 50 120 240))
      (let ((plain (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
                     (hermes-chat--header-line width))))
        (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
          (let ((branded (hermes-chat--header-line width)))
            (should (equal (substring-no-properties branded)
                           (if (<= (+ (string-width plain) (string-width "⚕ | ")) width)
                               (concat "⚕ | " (substring-no-properties plain))
                             (substring-no-properties plain))))
            (should (<= (string-width branded) width))
            (when (string-prefix-p "⚕ | " branded)
              (should-not (get-text-property 0 'face branded))
              (should (eq (get-text-property 1 'face branded) 'shadow)))))))))

(ert-deftest hermes-chat-header-thinking-does-not-repeat-reasoning ()
  "The rendered thinking header omits synonymous activity, not effort settings."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--runtime-flags '(:reasoning-effort "medium"))
   (dolist (content '("reasoning" "(◔_◔) reasoning..." "thinking" "(◔_◔) thinking..."))
     (hermes-chat--handle-transport-event "a1" `(:type thinking :content ,content))
     (let* ((label (hermes-chat--header-status-label 'thinking))
            (activity (hermes-chat--thinking-activity content))
            (header (substring-no-properties (hermes-chat--header-line 240)))
            (cells (mapcar #'string-trim (split-string header "|"))))
       (should-not (string-match-p (regexp-quote label) header))
       (should-not (member activity cells))
       (should-not (member "medium" cells))))
   ;; Meaningful distinct activity is not a duplicate of the state label.
   (hermes-chat--handle-transport-event
    "a1" '(:type thinking :content "Inspecting the failing test"))
   (should-not (string-match-p "Inspecting The Failing Test"
                           (hermes-chat--header-line 240)))))

(ert-deftest hermes-chat-thinking-activity-is-neutral ()
  "Provider notices are activity, not evidence of reasoning."
  (dolist (content '("(◔_◔) pondering..." "reasoning" "Rate limited; waiting"))
    (should (equal (hermes-chat--thinking-activity content) "Working")))
  (should-not (hermes-chat--thinking-activity ""))
  (should-not (hermes-chat--thinking-activity nil)))

(ert-deftest hermes-chat-reasoning-row-public-lifecycle ()
  "Public callbacks clear only the transient row, never actual commentary."
  (dolist (terminal '((:type done :content "Answer")
                      (:type error :content "Failure")
                      (:type thinking :event "thinking.delta" :content "")
                      (:type thinking :event "tool.generating" :content "Calling terminal")
                      (:type tool :event "tool.start" :name "terminal" :status "running")
                      (:type progress :content "Working")
                      (:type delta :content "Answer")
                      (:type interim :content "Interim")
                      (:type status :status "reconnecting")
                      (:type status :status "closed")))
    (hermes-test-with-chat-buffer
     (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
     (setq hermes-chat--pending-assistant-id "a1")
     (let ((callback (hermes-chat--transport-callback (current-buffer) "a1" nil hermes-chat--transport-generation)))
       (funcall callback '(:type commentary :event "reasoning.delta" :content "Actual reasoning"))
       (funcall callback '(:type thinking :event "thinking.delta" :content "hidden"))
       (let ((node (gethash "a1:activity" hermes-chat--nodes)))
         (should node)
         (funcall callback '(:type thinking :event "thinking.delta" :content "changed"))
         (should (eq node (gethash "a1:activity" hermes-chat--nodes))))
       (funcall callback terminal)
       (should-not (gethash "a1:activity" hermes-chat--nodes))
       (should (equal (plist-get (ewoc-data (gethash "a1:commentary:thinking" hermes-chat--nodes))
                                 :content) "Actual reasoning"))
       (should-not (string-match-p "hidden\\|changed" (buffer-string)))))))

(ert-deftest hermes-chat-reasoning-row-history-windows-and-draft ()
  "Activity updates preserve two actual history windows and narrowed draft input."
  (save-window-excursion
    (delete-other-windows)
    (hermes-test-with-chat-buffer
     (dotimes (i 60)
       (hermes-chat--insert-entry (list :id (format "history-%s" i) :role 'assistant
                                      :content (format "History %s\nMore history" i) :status 'done)))
     (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
     (setq hermes-chat--pending-assistant-id "a1")
     (goto-char (point-max))
     (insert "draft preserved")
     (let* ((chat (current-buffer)) (one (selected-window))
            (two (split-window one nil 'right))
            (callback (hermes-chat--transport-callback chat "a1" nil hermes-chat--transport-generation)))
       (set-window-buffer one chat)
       (set-window-buffer two chat)
       (goto-char (point-min))
       (forward-line 15)
       (set-window-start one (line-beginning-position))
       (set-window-point one (point))
       (forward-line 20)
       (set-window-start two (line-beginning-position))
       (set-window-point two (point))
       (select-window one)
       (goto-char (window-start one))
       (forward-line 2)
       (redisplay t)
       (let ((p (point)) (starts (mapcar #'window-start (list one two)))
             (points (mapcar #'window-point (list one two))))
         (funcall callback '(:type thinking :event "thinking.delta" :content "spinner"))
         (funcall callback '(:type thinking :event "thinking.delta" :content "spinner"))
         (funcall callback '(:type thinking :event "thinking.delta" :content ""))
         (redisplay t)
         (should (= p (point)))
         (should (equal starts (mapcar #'window-start (list one two))))
         (should (equal points (mapcar #'window-point (list one two)))))
       (goto-char (point-max))
       (backward-char 4)
       (narrow-to-region (hermes-chat--input-position) (point-max))
       (let ((offset (- (point) (point-min))))
         (funcall callback '(:type thinking :event "thinking.delta" :content "spinner"))
         (funcall callback '(:type thinking :event "thinking.delta" :content ""))
         (should (buffer-narrowed-p))
         (should (= offset (- (point) (point-min))))
         (should (equal (buffer-string) "draft preserved")))))))

(ert-deftest hermes-chat-reasoning-row-invalidation-and-stale-owner ()
  "Old callbacks cannot resurrect a row after replacement or disconnect."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
   (setq hermes-chat--pending-assistant-id "a1")
   (let ((callback (hermes-chat--transport-callback (current-buffer) "a1" nil hermes-chat--transport-generation)))
     (funcall callback '(:type thinking :event "thinking.delta" :content "spinner"))
     (should (gethash "a1:activity" hermes-chat--nodes))
     (hermes-chat--invalidate-transport-state)
     (should-not (gethash "a1:activity" hermes-chat--nodes))
     (funcall callback '(:type thinking :event "thinking.delta" :content "late"))
     (should-not (gethash "a1:activity" hermes-chat--nodes)))))

(ert-deftest hermes-chat-quiet-header-and-owned-reasoning ()
  "Activity is one static owner row, not header prose or hidden content."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
   (setq hermes-chat--pending-assistant-id "a1")
   (dolist (text '("private spinner" "another spinner"))
     (hermes-chat--handle-transport-event
      "a1" (list :type 'thinking :event "thinking.delta" :content text)))
   (let ((rows (seq-filter (lambda (entry) (eq (plist-get entry :role) 'activity))
                           (hermes-chat--entries))))
     (should (= 1 (length rows)))
     (should (string-match-p (regexp-quote "Working…") (buffer-string)))
     (should-not (string-match-p "spinner" (buffer-string)))
     (should (equal (plist-get (car (last (hermes-chat--entries))) :id) "a1")))
   (should-not (string-match-p "Reasoning\\|Thinking\\|Running" (hermes-chat--header-line 240)))
   (hermes-chat--handle-transport-event "a1" '(:type delta :content "Answer"))
   (should-not (seq-find (lambda (entry) (eq (plist-get entry :role) 'activity))
                        (hermes-chat--entries)))))

(ert-deftest hermes-chat-quiet-header-ready-settings ()
  (hermes-test-with-chat-buffer
   (setq hermes-chat--status-state '(:status ready)
         hermes-chat--runtime-flags '(:reasoning-effort "high" :fast t :yolo t))
   (let ((text (hermes-chat--header-line 240)))
     (should-not (string-match-p "Ready\\|Fast\\|high" text))
     (should (string-match-p "YOLO" text))
     (should-not (string-prefix-p " | " text)))))

(ert-deftest hermes-chat-thinking-event-updates-header-without-entry ()
  "Header-only provider activity stays quiet and adds no transcript entry."
  (hermes-test-with-chat-buffer
   (let ((before (length (ewoc-collect hermes-chat--ewoc #'identity))))
     (hermes-chat--handle-transport-event
      "a1" '(:type thinking :content "(◔_◔) musing..."))
     (let ((header (hermes-test--header-line-string)))
       (should-not (string-match-p "Musing" header))
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
           (should (string-match-p "alien.signal" (hermes-chat--session-details-text))))
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
   (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 200)))
     (let ((header (hermes-test--header-line-string)))
       (should (string-prefix-p
                (concat (and (char-displayable-p ?⚕) "⚕ | ") "emacs-hermes | ")
                header))
       (should-not (string-match-p "coder" header))
       (should-not (string-match-p "planner" header))
       (should (string-match-p "claude-opus-4-8" header))
       (should-not (string-match-p "Ready" header))
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
         (should (string-prefix-p
                  (concat (and (char-displayable-p ?⚕) "⚕ | ") "project") header))
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

(ert-deftest hermes-chat-disconnect-header-is-warning ()
  "Public disconnect retains identity and never presents idle readiness."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-session-id "retained-session"
         hermes-chat--dashboard-active-session-id "live-session")
   (hermes-chat-disconnect)
   (should (equal hermes-chat--dashboard-session-id "retained-session"))
   (should (string-match-p "Disconnected" (hermes-chat--header-line)))
   (should-not (string-match-p "Idle" (hermes-chat--header-line)))
   (should (equal (hermes-chat--status-icon 'disconnected) "!"))
   (should (eq (hermes-chat--header-status-face 'disconnected) 'warning))))

(ert-deftest hermes-chat-native-navigation-preserves-draft ()
  "Transcript keys traverse buttons; composer motion leaves the draft intact."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "nav-tool" :role tool :content "one\ntwo"))
   (goto-char (point-max))
   (insert "draft\nsecond line")
   (let ((draft (hermes-chat-input-string))
         (undo buffer-undo-list))
     (goto-char (point-min))
     (call-interactively (key-binding (kbd "TAB")))
     (should (button-at (point)))
     (should (equal (button-get (button-at (point)) 'help-echo) "Expand output"))
     (call-interactively (key-binding (kbd "RET")))
     (should (equal (button-get (button-at (point)) 'help-echo) "Collapse output"))
     (call-interactively (key-binding (kbd "C-c C-j")))
     (should (= (point) (point-max)))
     (should (equal draft (hermes-chat-input-string)))
     (should (equal undo buffer-undo-list))
     (let (completed)
       (cl-letf (((symbol-function 'completion-at-point)
                  (lambda () (interactive) (setq completed t))))
         (call-interactively (key-binding (kbd "TAB"))))
       (should completed)))))

(ert-deftest hermes-chat-header-window-details-and-literal-percent ()
  "Each window budgets its display; full inert details remain discoverable."
  (save-window-excursion
    (delete-other-windows)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/remote/界%project-with-a-long-name/"
           hermes-chat--model "model%with-a-long-name"
           hermes-chat--runtime-flags '(:yolo t :fast t)
           hermes-chat--status-state '(:status running))
     (let* ((owner (current-buffer))
            (wide (selected-window))
            (narrow (split-window wide 24 'right)))
       (set-window-buffer wide owner)
       (set-window-buffer narrow owner)
       (dolist (window (list wide narrow))
         (let* ((width (window-body-width window))
                (header (with-selected-window window (hermes-chat--header-line)))
                (display (string-replace "%%" "%" header)))
           (should (<= (string-width display) width))
           (should-not (string-match-p "Running" display))
           (should (string-match-p "YOLO" display))
           (should (string-match-p (regexp-quote hermes-chat--working-directory)
                                   (get-text-property 0 'help-echo header)))))
       (should (string-match-p "界%%project" (hermes-chat--header-line 120)))
       (should (eq (keymap-lookup hermes-chat-info-map "h")
                   #'hermes-chat-session-details))
       (should-not (keymap-lookup hermes-chat-mode-map "C-c C-h"))
       (unwind-protect
           (progn
             (call-interactively (keymap-lookup hermes-chat-info-map "h"))
             (with-current-buffer "*Hermes Session Details*"
               (should (derived-mode-p 'special-mode))
               (should buffer-read-only)
               (should (string-match-p "model%with-a-long-name" (buffer-string)))
               (should-not (next-button (point-min)))))
         (when (get-buffer "*Hermes Session Details*")
           (kill-buffer "*Hermes Session Details*")))))))

(ert-deftest hermes-chat-navigation-boundaries-and-reasoning ()
  "Reverse traversal and narrowing never wrap or change the multiline draft."
  (hermes-test-with-chat-buffer
   (hermes-chat--insert-entry '(:id "reason" :role commentary :content "why\nhow"))
   (hermes-chat--insert-entry '(:id "tool" :role tool :content "one\ntwo"))
   (goto-char (point-max))
   (insert "draft\nuntouched")
   (let ((draft (hermes-chat-input-string)))
     (call-interactively (key-binding (kbd "<backtab>")))
     (should (equal (button-get (button-at (point)) 'hermes-chat-entry-id) "tool"))
     (let ((position (point)))
       (should-error (call-interactively (key-binding (kbd "TAB"))) :type 'user-error)
       (should (= position (point))))
     (call-interactively (key-binding (kbd "<backtab>")))
     (should (equal (button-get (button-at (point)) 'help-echo) "Expand reasoning"))
     (call-interactively (key-binding (kbd "RET")))
     (should (equal (button-get (button-at (point)) 'help-echo) "Collapse reasoning"))
     (save-restriction
       (narrow-to-region hermes-chat--input-marker (point-max))
       (goto-char (point-min))
       (should-error (call-interactively (key-binding (kbd "<backtab>"))) :type 'user-error))
     (narrow-to-region (point-min) hermes-chat--input-marker)
     (call-interactively (key-binding (kbd "C-c C-j")))
     (should-not (buffer-narrowed-p))
     (should (= (point) (point-max)))
     (should (equal draft (hermes-chat-input-string))))))

(ert-deftest hermes-chat-header-priority-widths ()
  "Long identity never hides state or the active risk flag."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--working-directory (concat "/remote/" (make-string 100 ?界))
         hermes-chat--model (make-string 100 ?m)
         hermes-chat--runtime-flags '(:yolo t))
   (dolist (width '(1 2 8 12 20 30 40 50 80 120))
     (dolist (case '((ready "Ready" "RDY") (running "Running" "RUN")
                    (error "Error" "ERR") (approval-requested "Approval" "AP?")
                    (requested "Input" "IN?") (disconnected "Offline" "OFF")))
       (setq hermes-chat--status-state (list :status (car case)))
       (let ((header (hermes-chat--header-line width)))
         (should (<= (string-width header) width))
         (should (string-match-p (if (< width 20) "!\\|YOLO" "YOLO") header))
         (when (and (>= width 8)
                    (memq (car case) '(error approval-requested requested disconnected)))
           (should (string-match-p
                    (regexp-quote
                     (cond ((< width 12) (nth 2 case))
                           ((and (>= width 30) (eq (car case) 'disconnected))
                            "Disconnected")
                           (t (nth 1 case)))) header))))))))

(ert-deftest hermes-chat-header-all-states-narrow-budget ()
  "Keep state and risk together across consecutive abbreviation boundaries."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--working-directory "/remote/界%long-directory/"
         hermes-chat--model (make-string 100 ?m)
         hermes-chat--goal '(:running t :turns-used 1 :max-turns 20)
         hermes-chat--context '(:used 100 :max 200 :percent 50))
   (dolist (yolo '(nil t))
     (setq hermes-chat--runtime-flags (list :yolo yolo :fast t))
     (dolist (case '((ready "Ready" "RDY") (running "Running" "RUN")
                    (error "Error" "ERR") (approval-requested "Approval" "AP?")
                    (requested "Input" "IN?") (disconnected "Offline" "OFF")
                    (thinking "Thinking" "THK") (waiting "Waiting" "WAIT")
                    (loading "Loading" "LOAD") (connecting "Connecting" "CON")
                    (streaming "Streaming" "STR") (queued "Queued" "Q")
                    (handoff "Handing off" "HAND") (interrupted "Interrupted" "INT")
                    (cancelled "Cancelled" "CAN") (idle "Idle" "IDL")))
       (setq hermes-chat--status-state (list :status (car case)))
       (dolist (width '(8 9 10 11 12 13 14 15 16 17 18 19 20))
         (let* ((header (hermes-chat--header-line width))
                (display (string-replace "%%" "%" header))
                (state-pos (string-match-p
                            (regexp-opt (cdr case)) display))
                (risk-pos (string-match-p "YOLO\\|Y!" display)))
           (should (<= (string-width display) width))
           (if (memq (car case) '(error approval-requested requested disconnected interrupted cancelled))
               (progn
                 (should state-pos)
                 (should (eq (get-text-property state-pos 'face display)
                             (hermes-chat--header-status-face (car case)))))
             (should-not state-pos))
           (if yolo
               (progn
                 (should risk-pos)
                 (should (eq (get-text-property risk-pos 'face display)
                             'hermes-chat-header-warning)))
             (should-not risk-pos))))))))

(ert-deftest hermes-chat-details-ignore-ambient-printer-limits ()
  "Details and tooltip retain nested values under finite printer limits."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--runtime-flags
         '(:reasoning-effort "high" :fast t :yolo t :extra (:nested ("runtime-end")))
         hermes-chat--goal '(:running t :extra (:nested ("goal-end")))
         hermes-chat--context '(:used 100 :max 200 :extra (:nested ("context-end"))))
   (let* ((print-length nil)
          (print-level nil)
          (expected (mapcar (lambda (value) (format "%S" value))
                            (list hermes-chat--runtime-flags hermes-chat--goal
                                  hermes-chat--context))))
     (dolist (limits '((2 nil) (nil 2) (2 2)))
       (let* ((print-length (car limits))
              (print-level (cadr limits))
              (details (hermes-chat--session-details-text))
              (tooltip (get-text-property 0 'help-echo (hermes-chat--header-line 12))))
         (dolist (value expected)
           (should (string-match-p (regexp-quote value) details))
           (should (string-match-p (regexp-quote value) tooltip)))
         (should (equal print-length (car limits)))
         (should (equal print-level (cadr limits))))))))

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
   (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 200)))
     (should (equal (substring-no-properties (hermes-chat--header-line))
                    (concat (and (char-displayable-p ?⚕) "⚕ | ")
                            "emacs-hermes | YOLO | grok-4.5 | 25k/500k"))))))

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
   (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 200)))
     (let ((header (hermes-chat--header-line)))
       (dolist (case '(("emacs-hermes" . hermes-chat-header-directory)
                       ("grok-4.5" . hermes-chat-header-model)
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
   (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 10)))
     (let ((header (hermes-chat--header-line)))
       (should (<= (string-width header) 10))
       (should (string-prefix-p "emacs" header))
       (should-not (string-match-p "RDY" header))))))

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
   (should-not (string-match-p "Goal" (hermes-chat--header-line)))
   (should (string-match-p ":turns-used 3" (hermes-chat--session-details-text)))
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

(ert-deftest hermes-chat-show-usage-ignores-stale-session ()
  "A late usage result cannot render into a replacement session."
  (let (resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-usage)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-chat--dashboard-control-client)
               (lambda () hermes-chat--dashboard-client)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old")
       (hermes-chat-show-usage)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((calls . 2) (input . 10) (output . 5) (total . 15)))
       (should-not (string-match-p "Usage:" (buffer-string)))))))

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

(ert-deftest hermes-chat-command-terminal-take-clears-only-exact-owner ()
  "Command terminal take cannot clear a replacement operation."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'old)
   (let ((snapshot (hermes-chat--capture-command-terminal-owner)))
     (should (eq (plist-get snapshot :owner) 'old))
     (setq hermes-chat--command-owner 'new)
     (should-not (hermes-chat--take-command-terminal-owner snapshot))
     (should (eq hermes-chat--command-owner 'new)))
   (let ((snapshot (hermes-chat--capture-command-terminal-owner)))
     (should-not (hermes-chat--take-command-terminal-owner snapshot))
     (should-not hermes-chat--command-owner))))

(ert-deftest hermes-chat-terminal-owner-registry-takes-current-dormantly-in-order ()
  "Combined take clears exact owners and returns ordered dormant effects."
  (hermes-test-with-chat-buffer
   (let* ((token (list 'response-token))
          (timer (timer-create))
          (poll (list :id 'handoff :timer timer))
          (prompt (list :prompt-type "clarify" :request-id "request"
                        :response-token token))
          (retained (list :buffer (current-buffer)
                          :generation hermes-chat--lifecycle-generation
                          :response-token token :text "answer"))
          observed)
     (puthash "request" prompt hermes-chat--pending-prompts)
     (setq hermes-chat--retained-clarify-owners (list retained)
           hermes-chat--command-owner 'command
           hermes-chat--handoff-owner 'handoff
           hermes-chat--handoff-poll poll)
     (let* ((snapshot (hermes-chat--capture-terminal-owners))
            (effects (hermes-chat--take-terminal-owners snapshot)))
       (should (= (length effects) 2))
       (should-not observed)
       (should-not hermes-chat--command-owner)
       (should-not hermes-chat--handoff-owner)
       (should-not hermes-chat--handoff-poll)
       (should-not (gethash "request" hermes-chat--pending-prompts))
       (cl-letf (((symbol-function 'hermes-chat--restore-prompt-response)
                  (lambda (_text) (setq observed (append observed '(prompt)))))
                 ((symbol-function 'cancel-timer)
                  (lambda (_timer) (setq observed (append observed '(timer))))))
         (mapc #'funcall effects))
       (should (equal observed '(prompt timer)))))))

(ert-deftest hermes-chat-terminal-owner-registry-preserves-all-replacements ()
  "Combined stale take leaves every successor authority intact."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'old-command
         hermes-chat--handoff-owner 'old-handoff
         hermes-chat--handoff-poll
         (list :id 'old-handoff :timer (timer-create)))
   (let ((snapshot (hermes-chat--capture-terminal-owners))
         (prompt-table (make-hash-table :test #'equal))
         (auto-table (make-hash-table :test #'equal))
         (poll (list :id 'new-handoff :timer (timer-create))))
     (puthash "successor" '(:prompt-type "sudo") prompt-table)
     (setq hermes-chat--pending-prompts prompt-table
           hermes-chat--auto-prompt-keys auto-table
           hermes-chat--command-owner 'new-command
           hermes-chat--handoff-owner 'new-handoff
           hermes-chat--handoff-poll poll)
     (should-not (hermes-chat--take-terminal-owners snapshot))
     (should (eq hermes-chat--pending-prompts prompt-table))
     (should (eq hermes-chat--auto-prompt-keys auto-table))
     (should (eq hermes-chat--command-owner 'new-command))
     (should (eq hermes-chat--handoff-owner 'new-handoff))
     (should (eq hermes-chat--handoff-poll poll)))))

;;; Terminal fingerprint schema

(ert-deftest hermes-chat-terminal-clear-fields-are-exactly-ephemeral ()
  "Terminal clear fields contain ephemeral authority, never durable state."
  (should
   (equal hermes-chat--terminal-clear-fields
          '(hermes-chat--dashboard-client
            hermes-chat--dashboard-token
            hermes-chat--process
            hermes-chat--dashboard-active-session-id
            hermes-chat--dashboard-session-ready-p
            hermes-chat--dashboard-running-p
            hermes-chat--pending-assistant-id
            hermes-chat--dashboard-stream-assistant-id
            hermes-chat--dashboard-interim-assistant-id
            hermes-chat--dashboard-detached-assistant-id
            hermes-chat--dashboard-suppress-stream-p
            hermes-chat--dashboard-last-start-idle-count
            hermes-chat--server-queued-assistant-id
            hermes-chat--server-queued-user-id
            hermes-chat--server-queued-after-idle-count
            hermes-chat--server-queued-prior-terminal-p
            hermes-chat--busy-submit-context
            hermes-chat--unsettled-submit-context
            hermes-chat--prepared-submit-assistant-id
            hermes-chat--queued-submit-id
            hermes-chat--interrupted-assistant-id
            hermes-chat--interrupted-events
            hermes-chat--interrupt-request-pending-p
            hermes-dashboard-transport-request-owner
            hermes-chat--active-tools)))
  (dolist (field '(hermes-chat--queued-messages
                   hermes-chat--dashboard-idle-count
                   hermes-chat--ewoc hermes-chat--nodes
                   hermes-chat--input-history-draft default-directory
                   hermes-chat--working-directory hermes-chat--profile
                   hermes-chat--model hermes-chat--agent-name
                   hermes-chat--context hermes-chat--goal
                   hermes-chat--runtime-flags hermes-chat--session-id
                   hermes-chat--status-state hermes-chat--title
                   hermes-chat--transport-generation
                   hermes-chat--lifecycle-generation))
    (should-not (memq field hermes-chat--terminal-clear-fields))))

(defun hermes-chat-test--fingerprint-under (value hostile callback)
  "Fingerprint VALUE under one HOSTILE printer ambience using CALLBACK."
  (let ((print-length (if hostile 1 nil))
        (print-level (if hostile 1 nil))
        (print-circle (not hostile))
        (print-gensym (not hostile))
        (print-quoted (not hostile))
        (print-continuous-numbering hostile)
        (print-number-table (and hostile (make-vector 67 nil)))
        (print-escape-newlines (not hostile))
        (print-escape-control-characters (not hostile))
        (print-escape-nonascii (not hostile))
        (print-escape-multibyte (not hostile))
        (print-charset-text-property (not hostile))
        (print-unreadable-function callback)
        (print-integers-as-characters hostile)
        (print-symbols-bare hostile)
        (float-output-format (and hostile "%.2f")))
    (hermes-chat--terminal-fingerprint value)))

(ert-deftest hermes-chat-terminal-fingerprint-binds-every-printer-control ()
  "Fingerprinting overrides every supported ambient printer control."
  (let ((print-length 1) (print-level 1) (print-circle nil)
        (print-gensym nil) (print-quoted nil)
        (print-continuous-numbering t) (print-number-table (make-vector 67 nil))
        (print-escape-newlines nil) (print-escape-control-characters nil)
        (print-escape-nonascii nil) (print-escape-multibyte nil)
        (print-charset-text-property nil) (print-unreadable-function #'ignore)
        (print-integers-as-characters t) (print-symbols-bare t)
        (float-output-format "%.2f") observed)
    (cl-letf (((symbol-function 'prin1-to-string)
               (lambda (&rest _)
                 (setq observed
                       (list print-length print-level print-circle print-gensym
                             print-quoted print-continuous-numbering
                             print-number-table print-escape-newlines
                             print-escape-control-characters print-escape-nonascii
                             print-escape-multibyte print-charset-text-property
                             print-unreadable-function
                             print-integers-as-characters print-symbols-bare
                             float-output-format))
                 "private")))
      (hermes-chat--terminal-fingerprint nil))
    (should (equal observed
                   '(nil nil t t t nil nil t t t t t nil nil nil nil)))))

(ert-deftest hermes-chat-terminal-fingerprint-is-private-and-deterministic ()
  "Fingerprinting defeats hostile printer state for complete structured values."
  (let* ((shared (list "shared-queue-secret"))
         (cycle (list 'cycle-secret))
         (charset (propertize "charset-λ-secret" 'charset 'greek-iso8859-7))
         (value (list :nested (list (list "token-ZZ" "prompt-YY"))
                      :shared shared shared :cycle cycle :charset charset
                      :positioned (position-symbol 'positioned-secret 19)
                      :gensym (make-symbol "gensym-secret")
                      :unreadable (current-buffer) :quoted '(function quoted-secret)
                      :response "response-XX"
                      :input "input-WW\n\x1f" :held "held-VV"
                      :nonascii (unibyte-string 200)
                      :integer 9876543210123456789 :float 12345.6789))
         (calls 0)
         (state 'untouched)
         (callback (lambda (&rest _)
                     (setq calls (1+ calls) state 'mutated)
                     "callback-secret")))
    (setcdr cycle cycle)
    (let ((hostile (hermes-chat-test--fingerprint-under value t callback))
          (opposite (hermes-chat-test--fingerprint-under value nil nil)))
      (should (equal hostile opposite))
      (should (equal hostile
                     (hermes-chat-test--fingerprint-under value t callback)))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" hostile))
      (dolist (plaintext '("token-ZZ" "prompt-YY" "response-XX"
                           "input-WW" "held-VV" "shared-queue-secret"
                           "charset-λ-secret" "9876543210123456789"
                           "12345.6789" "positioned-secret" "gensym-secret"
                           "\n"))
        (should-not (string-match-p (regexp-quote plaintext) hostile)))
      (should (= calls 0))
      (should (eq state 'untouched)))))

(ert-deftest hermes-chat-terminal-field-record-schema-is-exact ()
  "Only one exact three-element record per catalog field passes schema."
  (let* ((exact-value (list :exact (car hermes-chat--terminal-clear-fields)
                            "value-secret"))
         (records
          (cons (hermes-chat--terminal-field-record
                 (car hermes-chat--terminal-clear-fields) exact-value)
                (mapcar (lambda (field)
                          (hermes-chat--terminal-field-record
                           field (list :exact field "value-secret")))
                        (cdr hermes-chat--terminal-clear-fields))))
         (first (car records))
         (second (cadr records)))
    (should (equal first
                   (list (car first) (cadr first)
                         (hermes-chat--terminal-fingerprint (cadr first)))))
    (should (eq exact-value (cadr first)))
    (should (hermes-chat--terminal-fields-schema-p records))
    (dolist (malformed
             (list nil
                   (butlast records)
                   (cons first records)
                   (cons (cons 'unknown-field (cdr first)) (cdr records))
                   (cons second (cons first (cddr records)))
                   (cons (cons 'hermes-chat--session-id (cdr first))
                         (cdr records))
                   (cons first 'dotted-tail)
                   (cons (butlast first) (cdr records))
                   (cons (append first '(extra)) (cdr records))
                   (cons (list (car first) (cadr first) "not-a-digest")
                         (cdr records))))
      (should-not (hermes-chat--terminal-fields-schema-p malformed)))))

(ert-deftest hermes-chat-terminal-fingerprint-schema-is-inert ()
  "Catalog, fingerprint, record, and schema calls do not take live authority."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-client 'client
         hermes-chat--pending-assistant-id "assistant"
         hermes-chat--queued-messages '((:id "queue" :content "durable")))
   (let ((before (mapcar #'symbol-value hermes-chat--terminal-clear-fields))
         (route hermes-chat--turn-event-function)
         (calls 0))
     (let ((hermes-chat--busy-submit-event-function
            (lambda (&rest _) (setq calls (1+ calls))))
           (hermes-chat--busy-submit-abandon-function
            (lambda (&rest _) (setq calls (1+ calls)))))
       (let ((record (hermes-chat--terminal-field-record
                      'hermes-chat--dashboard-client
                      hermes-chat--dashboard-client)))
         (hermes-chat--terminal-fingerprint record)
         (hermes-chat--terminal-fields-schema-p
          (mapcar (lambda (field)
                    (hermes-chat--terminal-field-record field (symbol-value field)))
                  hermes-chat--terminal-clear-fields))))
     (should (equal before
                    (mapcar #'symbol-value hermes-chat--terminal-clear-fields)))
     (should (eq route hermes-chat--turn-event-function))
     (should (= calls 0))
     (should (equal hermes-chat--queued-messages
                    '((:id "queue" :content "durable")))))))

;;; Integrated terminal owner authority

(defun hermes-chat-test--terminal-owner-fixture ()
  "Install real prompt, command, and handoff owners and return their leaves."
  (let* ((token (list 'response-token))
         (prompt (list :prompt-type "clarify" :request-id "clarify"
                       :response-token token))
         (member (list :prompt-type "approval" :session-id "session"))
         (approval (list :prompt-type "approval" :prompt-queue (list member)))
         (claim (list "clarify" prompt))
         (retained (list :buffer (current-buffer)
                         :generation hermes-chat--lifecycle-generation
                         :response-token token :text "answer"))
         (timer (timer-create))
         (meta (list "poll-metadata"))
         (poll (list :id 'handoff :timer timer :meta meta)))
    (puthash "clarify" prompt hermes-chat--pending-prompts)
    (puthash "approval" approval hermes-chat--pending-prompts)
    (puthash "clarify" claim hermes-chat--auto-prompt-keys)
    (setq hermes-chat--retained-clarify-owners (list retained)
          hermes-chat--command-owner (list 'command)
          hermes-chat--handoff-owner (list 'handoff)
          hermes-chat--handoff-poll poll)
    (list :prompt prompt :approval approval :token token :member member
          :claim claim :retained retained :timer timer :poll poll :meta meta)))

(defun hermes-chat-test--replace-terminal-owner-leaf (case fixture)
  "Replace CASE authority in FIXTURE with an equal successor."
  (pcase case
    ('registry (setq hermes-chat--terminal-owner-functions
                     (copy-sequence hermes-chat--terminal-owner-functions)))
    ('lifecycle (setq hermes-chat--lifecycle-generation
                      (list 'replacement-lifecycle)))
    ('mode (fundamental-mode))
    ('take (let ((registry (copy-tree hermes-chat--terminal-owner-functions)))
             (setcdr (car registry) #'ignore)
             (setq hermes-chat--terminal-owner-functions registry)))
    ('command (setq hermes-chat--command-owner
                    (copy-tree hermes-chat--command-owner)))
    ('handoff-owner (setq hermes-chat--handoff-owner
                          (copy-tree hermes-chat--handoff-owner)))
    ('handoff-poll (setq hermes-chat--handoff-poll
                         (copy-tree hermes-chat--handoff-poll)))
    ('handoff-timer (setf (plist-get hermes-chat--handoff-poll :timer)
                          (timer-create)))
    ('prompt-table (setq hermes-chat--pending-prompts
                         (copy-hash-table hermes-chat--pending-prompts)))
    ('auto-table (setq hermes-chat--auto-prompt-keys
                       (copy-hash-table hermes-chat--auto-prompt-keys)))
    ('prompt (puthash "approval" (copy-sequence (plist-get fixture :approval))
                      hermes-chat--pending-prompts))
    ('token (setf (plist-get (plist-get fixture :prompt) :response-token)
                  (copy-tree (plist-get fixture :token))))
    ('retained (setq hermes-chat--retained-clarify-owners
                     (list (copy-tree (plist-get fixture :retained)))))
    ('member (setf (plist-get (plist-get fixture :approval) :prompt-queue)
                   (list (copy-tree (plist-get fixture :member)))))
    ('claim (puthash "clarify" (copy-sequence (plist-get fixture :claim))
                     hermes-chat--auto-prompt-keys))))

(ert-deftest hermes-chat-terminal-owner-authority-is-integrated-and-inert ()
  "Real registered owners validate in capture order without taking authority."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (prompt-capture (symbol-function 'hermes-chat--capture-terminal-prompts))
          (command-capture (symbol-function 'hermes-chat--capture-command-terminal-owner))
          (handoff-capture (symbol-function 'hermes-chat--capture-handoff-terminal-owner))
          observed authority)
     (cl-letf (((symbol-function 'hermes-chat--capture-terminal-prompts)
                (lambda () (setq observed (append observed '(prompt)))
                  (funcall prompt-capture)))
               ((symbol-function 'hermes-chat--capture-command-terminal-owner)
                (lambda () (setq observed (append observed '(command)))
                  (funcall command-capture)))
               ((symbol-function 'hermes-chat--capture-handoff-terminal-owner)
                (lambda () (setq observed (append observed '(handoff)))
                  (funcall handoff-capture)))
               ((symbol-function 'hermes-chat--take-terminal-prompts) #'ert-fail)
               ((symbol-function 'hermes-chat--take-command-terminal-owner) #'ert-fail)
               ((symbol-function 'hermes-chat--take-handoff-terminal-owner) #'ert-fail))
       (setq authority (hermes-chat--capture-terminal-owner-authority))
       (should (hermes-chat--terminal-owner-authority-current-p authority)))
     (should (eq (plist-get authority :registry)
                 hermes-chat--terminal-owner-functions))
     (should (equal (mapcar #'car (plist-get authority :owners))
                    '(hermes-chat--take-terminal-prompts
                      hermes-chat--take-command-terminal-owner
                      hermes-chat--take-handoff-terminal-owner)))
     (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                             (plist-get authority :digest)))
     (should (equal observed '(prompt command handoff prompt command handoff)))
     (should (eq (gethash "clarify" hermes-chat--auto-prompt-keys)
                 (plist-get fixture :claim))))))

(ert-deftest hermes-chat-terminal-owner-authority-rejects-equal-successors ()
  "Every exact registry and owner leaf rejects a structurally equal successor."
  (dolist (case '(registry lifecycle mode take command
                  handoff-owner handoff-poll handoff-timer
                  prompt-table auto-table prompt token retained member claim))
    (let ((hermes-chat--terminal-owner-functions
           hermes-chat--terminal-owner-functions))
      (hermes-test-with-chat-buffer
       (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
              (authority (hermes-chat--capture-terminal-owner-authority))
              (table hermes-chat--auto-prompt-keys)
              (prompt (plist-get fixture :prompt)))
         (hermes-chat-test--replace-terminal-owner-leaf case fixture)
         (cl-letf (((symbol-function 'remhash) #'ert-fail)
                   ((symbol-function 'cancel-timer) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-terminal-prompts) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-command-terminal-owner) #'ert-fail)
                   ((symbol-function 'hermes-chat--take-handoff-terminal-owner) #'ert-fail))
           (should-not (hermes-chat--terminal-owner-authority-current-p authority)))
         (when (eq case 'claim)
           (should (eq table hermes-chat--auto-prompt-keys))
           (should (eq prompt (cadr (gethash "clarify" table))))))))))

(ert-deftest hermes-chat-terminal-owner-authority-digest-detects-nested-mutation ()
  "Saved digest rejects shared nested mutation under hostile printer settings."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (authority (hermes-chat--capture-terminal-owner-authority)))
     (setcar (plist-get fixture :meta) "mutated-poll-metadata")
     (let ((print-length 1) (print-level 1) (print-circle nil)
           (print-quoted nil) (print-escape-newlines nil)
           (print-symbols-bare t) (float-output-format "%.1f"))
       (should-not (hermes-chat--terminal-owner-authority-current-p authority))))))

(ert-deftest hermes-chat-terminal-owner-authority-malformed-fails-closed ()
  "Unknown schemas and invalid A3a claims neither signal nor run effects."
  (hermes-test-with-chat-buffer
   (hermes-chat-test--terminal-owner-fixture)
   (let* ((authority (hermes-chat--capture-terminal-owner-authority))
          (owners-bad (plist-put (copy-tree authority) :owners '(malformed)))
          (unknown '((hermes-chat-test--unexpected . ignore)))
          (registries (list nil unknown
                            (reverse hermes-chat--terminal-owner-functions)
                            (cons (car hermes-chat--terminal-owner-functions)
                                  hermes-chat--terminal-owner-functions)
                            (cons (car hermes-chat--terminal-owner-functions) 'dotted)))
          (calls 0))
     (cl-letf (((symbol-function 'hermes-chat-test--unexpected)
                (lambda () (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--take-terminal-prompts)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'remhash) (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'cancel-timer) (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--restore-prompt-response)
                (lambda (&rest _) (setq calls (1+ calls)))))
       (dolist (malformed (list nil '(dotted . authority) owners-bad))
         (should-not (hermes-chat--terminal-owner-authority-current-p malformed)))
       (dolist (registry registries)
         (let ((hermes-chat--terminal-owner-functions registry))
           (should-not (hermes-chat--terminal-owner-authority-current-p authority))))
       (let ((hermes-chat--auto-prompt-keys 'invalid-a3a-state))
         (should-not (hermes-chat--terminal-owner-authority-current-p authority)))
       (should (= calls 0))))))

(ert-deftest hermes-chat-terminal-owner-authority-contains-capture-quit ()
  "Capture and validation fail closed when an exact owner capture quits."
  (hermes-test-with-chat-buffer
   (let* ((fixture (hermes-chat-test--terminal-owner-fixture))
          (authority (hermes-chat--capture-terminal-owner-authority))
          (table hermes-chat--auto-prompt-keys)
          (claim (plist-get fixture :claim))
          (calls 0))
     (cl-letf (((symbol-function 'hermes-chat--capture-terminal-prompts)
                (lambda () (signal 'quit nil)))
               ((symbol-function 'hermes-chat--take-terminal-prompts)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'remhash)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'cancel-timer)
                (lambda (&rest _) (setq calls (1+ calls))))
               ((symbol-function 'hermes-chat--restore-prompt-response)
                (lambda (&rest _) (setq calls (1+ calls)))))
       (should-not
        (condition-case nil
            (hermes-chat--capture-terminal-owner-authority)
          (quit 'signaled)))
       (should-not
        (condition-case nil
            (hermes-chat--terminal-owner-authority-current-p authority)
          (quit 'signaled))))
     (should (= calls 0))
     (should (eq table hermes-chat--auto-prompt-keys))
     (should (eq claim (gethash "clarify" table))))))

(provide 'hermes-chat-tests)
;;; hermes-chat-tests.el ends here
