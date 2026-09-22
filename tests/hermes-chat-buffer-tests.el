;;; hermes-chat-buffer-tests.el --- Chat topic tests -*- lexical-binding: t; -*-

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

(provide 'hermes-chat-buffer-tests)
;;; hermes-chat-buffer-tests.el ends here
