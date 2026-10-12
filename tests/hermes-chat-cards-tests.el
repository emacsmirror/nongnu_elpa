;;; hermes-chat-cards-tests.el --- Live worker cards in the transcript -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-cards': a `delegate_task' row grows into a live
;; tree of its workers and folds once the batch settles, a background
;; terminal row shows its process's last line and exit verdict, and a
;; global segment lists the hidden chats with work.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defvar hermes-test--card-now nil
  "Fake clock for card tests.")

(defun hermes-test--card-owner ()
  "Return a minimal attached work owner for the current chat."
  (list :buffer (current-buffer) :runtime "sid" :events nil
        :delegates nil :processes nil :current-p (lambda (_) t)))

(defmacro hermes-test--with-card-chat (&rest body)
  "Run BODY in a displayed chat with a streaming turn and a fake owner."
  (declare (indent 0) (debug t))
  `(let ((hermes-test--card-now (float-time))
         (real-float-time (symbol-function 'float-time))
         (timers nil))
     (cl-letf (((symbol-function 'float-time)
                (lambda (&optional time)
                  (if time (funcall real-float-time time) hermes-test--card-now)))
               ((symbol-function 'run-at-time)
                (lambda (&rest args) (push args timers) (timer-create)))
               ((symbol-function 'cancel-timer) #'ignore))
       (save-window-excursion
         (hermes-test-with-chat-buffer
           (switch-to-buffer (current-buffer))
           (delete-other-windows)
           (hermes-chat--insert-entry
            '(:id "a1" :role assistant :content "" :status streaming))
           (setq hermes-chat--pending-assistant-id "a1"
                 hermes-chat--work-owner (hermes-test--card-owner))
           ,@body)))))

(defun hermes-test--card-tool (event)
  "Apply tool EVENT to the streaming turn as the reducer would."
  (hermes-chat--apply-turn-effect "a1" (cons 'upsert-entry event)))

(defun hermes-test--card-events (&rest events)
  "Merge subagent EVENTS into the chat's roster, announcing each change."
  (let ((owner hermes-chat--work-owner))
    (dolist (event events)
      (let ((old (plist-get owner :events)))
        (setf (plist-get owner :events)
              (hermes-chat--work-apply-event old event hermes-test--card-now))
        (unless (eq old (plist-get owner :events))
          (hermes-chat--work-changed owner))))))

(defun hermes-test--card-processes (rows)
  "Give the chat a process snapshot of ROWS and announce the change."
  (let ((owner hermes-chat--work-owner))
    (setf (plist-get owner :processes) (list :sid "sid" :rows rows))
    (hermes-chat--work-changed owner)))

(defun hermes-test--transcript ()
  "Return the transcript text above the composer, without properties."
  (buffer-substring-no-properties (point-min) (hermes-chat--input-position)))

(defun hermes-test--card-line (needle)
  "Return the whole transcript line containing NEEDLE, or nil."
  (seq-find (lambda (line) (string-search needle line))
            (split-string (hermes-test--transcript) "\n")))

(defconst hermes-test--card-delegate-start
  '(:type tool :event "tool.start" :name "delegate_task" :tool-call-id "call-1"
          :status "running"
          :args ((tasks . [((goal . "Map the agents panel"))
                           ((goal . "Inventory process fields"))])))
  "A two-task `delegate_task' start.")

(defun hermes-test--card-spawn ()
  "Start the delegation and its workers: two roots and one nested child."
  (hermes-test--card-tool hermes-test--card-delegate-start)
  (hermes-test--card-events
   '(:event "subagent.spawn_requested" :subagent-id "sa-1" :task-index 0
            :content "Map the agents panel")
   '(:event "subagent.spawn_requested" :subagent-id "sa-2" :task-index 1
            :content "Inventory process fields")
   '(:event "subagent.start" :subagent-id "sa-1" :task-index 0
            :content "Map the agents panel")
   '(:event "subagent.spawn_requested" :subagent-id "sa-3" :parent-id "sa-1"
            :task-index 0 :content "Extract depth caps")
   '(:event "subagent.start" :subagent-id "sa-3" :parent-id "sa-1"
            :task-index 0 :content "Extract depth caps")))

;;;; Pure presentation

(ert-deftest hermes-chat-card-text-is-pure-and-nested ()
  "Rows become a nested tree with glyphs, activity and right-aligned times."
  (let* ((entry (list :id "e" :role 'tool :status 'running
                      :metadata '(:event (:name "delegate_task"))
                      :card (list :kind 'delegate :tasks 2 :now 100.0
                                  :rows '((:id "a" :state running :goal "Root A" :started 40.0
                                                :tools (("read_file" . "x.el"))
                                                :tool-count 3)
                                          (:id "c" :parent "a" :state queued :goal "Child C")
                                          (:id "b" :state completed :goal "Root B"
                                                :started 50.0 :finished 80.0
                                                :summary "All fields listed\nmore")))))
         (text (hermes-chat--card-text entry)))
    (should (equal-including-properties text (hermes-chat--card-text entry)))
    (should (equal (split-string (substring-no-properties text) "\n" t)
                   '("  ● 🧠 delegate_task · 2 tasks · 1 running · 1 queued · 1 done 1:00"
                     "  ├ ● Root A 1:00"
                     "  │   ↳ 📖 read_file x.el · 3 tools"
                     "  │   └ ◌ Child C queued"
                     "  └ ✓ Root B 0:30"
                     "      ↳ All fields listed")))
    (let ((pos (string-search "0:30" text)))
      (should (eq (get-text-property pos 'face text) 'shadow))
      (should (eq (car (get-text-property (1- pos) 'display text)) 'space)))
    (should (eq (get-text-property (string-search "✓" text) 'face text)
                'hermes-work-done))
    (should (eq (get-text-property (string-search "◌" text) 'face text)
                'hermes-work-queued))))

;;;; Live tree in place

(ert-deftest hermes-chat-card-updates-in-place-preserving-readers ()
  "The card repaints in place; draft, undo, point and reader windows hold."
  (hermes-test--with-card-chat
    (hermes-chat--insert-entry
     (hermes-chat--make-entry 'assistant
                              (mapconcat (lambda (n) (format "older line %d" n))
                                         (number-sequence 1 40) "\n")
                              'done "older"))
    (hermes-test--card-spawn)
    (should (hermes-test--card-line "● 🧠 delegate_task · 2 tasks · 2 running · 1 queued"))
    (should (hermes-test--card-line "├ ● Map the agents panel"))
    (should (hermes-test--card-line "│   └ ● Extract depth caps"))
    (should (hermes-test--card-line "└ ◌ Inventory process fields"))
    ;; No per-child transcript rows: only the card owns the workers.
    (should (= 1 (cl-count-if (lambda (entry) (eq (plist-get entry :role) 'tool))
                              (hermes-chat--entries))))
    (let* ((reader (split-window (selected-window) nil 'right))
           (older (save-excursion
                    (goto-char (point-min))
                    (search-forward "older line 20")
                    (line-beginning-position))))
      (set-window-buffer reader (current-buffer))
      (set-window-start reader older t)
      (set-window-point reader (+ older 3))
      (goto-char (point-max))
      (setq buffer-undo-list nil)
      (insert "draft λ")
      (undo-boundary)
      (let ((offset (- (point) (hermes-chat--input-position)))
            (start-text (buffer-substring-no-properties older (+ older 13))))
        (setq hermes-test--card-now (+ hermes-test--card-now 5))
        (hermes-test--card-events
         '(:event "subagent.tool" :subagent-id "sa-1" :name "read_file"
                  :preview "agentsPanel.tsx" :tool-count 14))
        (should (hermes-test--card-line "↳ 📖 read_file agentsPanel.tsx · 14 tools"))
        (should (hermes-test--card-line "Map the agents panel"))
        (should (equal (hermes-chat-input-string) "draft λ"))
        (should (= (- (point) (hermes-chat--input-position)) offset))
        (should (equal (buffer-substring-no-properties
                        (window-start reader) (+ (window-start reader) 13))
                       start-text))
        (should (= (window-point reader) (+ (window-start reader) 3)))
        ;; Transcript stays read-only; the draft's undo still removes it.
        (should-not (text-property-not-all
                     (point-min) (hermes-chat--input-position) 'read-only t))
        (hermes-test--draft-undo-command #'undo)
        (should (equal (hermes-chat-input-string) ""))))))

(ert-deftest hermes-chat-card-collapses-when-the-batch-settles ()
  "A settled batch folds to one line; the fold button unfolds the results."
  (hermes-test--with-card-chat
    (hermes-test--card-spawn)
    (setq hermes-test--card-now (+ hermes-test--card-now 62))
    (hermes-test--card-events
     '(:event "subagent.start" :subagent-id "sa-2")
     '(:event "subagent.complete" :subagent-id "sa-3" :status "completed"
              :content "caps are 3 deep")
     '(:event "subagent.complete" :subagent-id "sa-1" :status "completed"
              :content "panel budget is 6 rows")
     '(:event "subagent.complete" :subagent-id "sa-2" :status "failed"
              :content "process.list timed out"))
    ;; The spawning row is still running, so the tree stays open.
    (should (hermes-test--card-line "Map the agents panel"))
    (hermes-test--card-tool
     (append '(:type tool :event "tool.complete" :status "completed" :duration 62.0)
             (cl-subseq hermes-test--card-delegate-start 4)))
    (let ((transcript (hermes-test--transcript)))
      (should (string-match-p
               "^  ✗ 🧠 delegate_task · 2 tasks · 2 done · 1 failed … +1:02$"
               transcript))
      (should-not (string-search "Map the agents panel" transcript)))
    (let ((head (save-excursion (goto-char (point-min))
                                (search-forward "delegate_task")
                                (1- (point)))))
      (should (eq (get-text-property head 'action) #'hermes-chat--toggle-entry-button))
      (push-button head)
      (should (hermes-test--card-line "├ ✓ Map the agents panel"))
      (should (hermes-test--card-line "↳ panel budget is 6 rows"))
      (should (hermes-test--card-line "└ ✗ Inventory process fields"))
      (should (hermes-test--card-line "↳ process.list timed out"))
      (should-not (string-search " …" (hermes-test--card-line "delegate_task")))
      (push-button head)
      (should-not (hermes-test--card-line "Map the agents panel")))
    ;; Settled cards stop ticking.
    (should-not hermes-chat--card-timer)))

;;;; Background process

(ert-deftest hermes-chat-card-process-shows-last-line-then-exit ()
  "A background terminal row shows the live last line, then the verdict."
  (hermes-test--with-card-chat
    (let ((start '(:type tool :event "tool.start" :name "terminal" :tool-call-id "call-t"
                         :status "running"
                         :args ((command . "make check") (background . t)))))
      (hermes-test--card-tool start)
      (hermes-test--card-tool
       (append '(:type tool :event "tool.complete" :status "completed"
                        :result ((output . "Background process started")
                                 (session_id . "proc_1")))
               (cl-subseq start 4)))
      (hermes-test--card-processes
       '((:id "proc_9" :kind process :state running :command "make check" :uptime 1)
         (:id "proc_1" :kind process :state running :command "make check"
              :last-line "Ran 1412/2672 tests" :uptime 220)))
      (should (string-match-p "^  ⚙ 💻 terminal: make check (background) +3:40$"
                              (hermes-test--card-line "terminal")))
      (should (equal (hermes-test--card-line "↳") "    ↳ Ran 1412/2672 tests"))
      (setq hermes-test--card-now (+ hermes-test--card-now 30))
      (hermes-test--card-processes
       `((:id "proc_1" :kind process :state done :exit-code 0 :command "make check"
              :last-line "Ran 2672 tests" :exited-at ,(- hermes-test--card-now 12))))
      (let ((line (hermes-test--card-line "terminal")))
        (should (string-match-p "^  ✓ 💻 terminal: make check (background) +exit 0 · 12s ago$"
                                line))
        (should-not (hermes-test--card-line "↳"))
        (let ((pos (save-excursion (goto-char (point-min)) (search-forward "exit 0"))))
          (should (eq (get-text-property (1- pos) 'face) 'hermes-work-done))))
      ;; After the linger, the verdict stays as the durable record.
      (setq hermes-test--card-now (+ hermes-test--card-now 120))
      (hermes-chat--cards-refresh)
      (should (string-match-p "(background) +exit 0$" (hermes-test--card-line "terminal")))
      (should-not hermes-chat--card-timer))))

;;;; Repaint economy

(ert-deftest hermes-chat-card-skips-repaint-when-unchanged ()
  "Refreshing with unchanged derived text never edits the buffer."
  (hermes-test--with-card-chat
    (hermes-test--card-spawn)
    (let ((tick (buffer-chars-modified-tick))
          (edits 0))
      (add-hook 'after-change-functions (lambda (&rest _) (cl-incf edits)) nil t)
      (hermes-chat--cards-refresh)
      (hermes-test--card-events '(:event "subagent.progress" :subagent-id "ghost"))
      (should (= edits 0))
      (should (= tick (buffer-chars-modified-tick)))
      ;; Elapsed time is part of the text: a later second repaints once.
      (setq hermes-test--card-now (+ hermes-test--card-now 1))
      (hermes-chat--cards-refresh)
      (should (> edits 0)))))

;;;; Cross-chat segment

(ert-deftest hermes-chat-work-indicator-lists-only-hidden-chats ()
  "The segment names hidden chats with live or unseen failed work only."
  (cl-letf (((symbol-function 'hermes-chat--switch-project)
             (lambda (buffer) (buffer-name (get-buffer buffer))))
            ((symbol-function 'char-displayable-p) (lambda (_) t)))
    (let ((hermes-chat-work-indicator t)
          (hermes-chat--card-indicator-buffers nil)
          (global-mode-string nil))
      (save-window-excursion
        (hermes-test-with-chat-buffer
          (let ((shown (current-buffer))
                (rows (lambda (n)
                        (list :sid "sid"
                              :rows (mapcar (lambda (i)
                                              (list :id (format "w%d" i) :kind 'delegate
                                                    :state 'running :goal "g"))
                                            (number-sequence 1 n))))))
            (setq hermes-chat--work-owner (hermes-test--card-owner))
            (setf (plist-get hermes-chat--work-owner :delegates) (funcall rows 1))
            (delete-other-windows)
            (set-window-buffer (selected-window) shown)
            (hermes-chat--card-indicator-sync)
            (hermes-test-with-chat-buffer
              (let ((hidden (current-buffer)))
                (setq hermes-chat--work-owner (hermes-test--card-owner))
                (setf (plist-get hermes-chat--work-owner :delegates) (funcall rows 2)
                      (plist-get hermes-chat--work-owner :processes)
                      '(:sid "sid" :rows ((:id "p" :kind process :state failed
                                                :exit-code 2 :exited-at 1.0e12))))
                (set-window-buffer (selected-window) shown)
                (hermes-chat--card-indicator-sync)
                (should (memq 'hermes-chat-work-indicator-mode-line
                              (default-value 'global-mode-string)))
                (let ((segment (hermes-chat--work-indicator-string)))
                  (should (equal (substring-no-properties segment)
                                 (format " [Hermes 🤖 %s 2 ✗1]" (buffer-name hidden))))
                  (should-not (string-search (concat (buffer-name shown) " ") segment))
                  (should (functionp (lookup-key (get-text-property
                                                  (string-search (buffer-name hidden) segment)
                                                  'local-map segment)
                                                 [mode-line mouse-1]))))
                ;; Displaying the chat drops it and acknowledges its failure.
                (set-window-buffer (selected-window) hidden)
                (hermes-chat--card-indicator-sync)
                (should (equal (substring-no-properties
                                (hermes-chat--work-indicator-string))
                               (format " [Hermes 🤖 %s 1]" (buffer-name shown))))
                (set-window-buffer (selected-window) shown)
                (setf (plist-get hermes-chat--work-owner :delegates) nil)
                (hermes-chat--card-indicator-sync)
                (should-not (memq hidden hermes-chat--card-indicator-buffers))
                (should-not (hermes-chat--work-indicator-string))))))))))

(provide 'hermes-chat-cards-tests)
;;; hermes-chat-cards-tests.el ends here
