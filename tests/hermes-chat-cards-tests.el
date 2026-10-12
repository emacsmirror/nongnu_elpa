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
  "Start the delegation and its workers: two roots and one nested child.
As in the gateway, a spawn request precedes the delegation id, which every
later event of the worker carries; sa-2 stays queued."
  (hermes-test--card-tool hermes-test--card-delegate-start)
  (hermes-test--card-events
   '(:event "subagent.spawn_requested" :subagent-id "sa-1" :task-index 0
            :content "Map the agents panel")
   '(:event "subagent.spawn_requested" :subagent-id "sa-2" :task-index 1
            :content "Inventory process fields")
   '(:event "subagent.start" :subagent-id "sa-1" :task-index 0
            :delegation-id "d1" :content "Map the agents panel")
   '(:event "subagent.spawn_requested" :subagent-id "sa-3" :parent-id "sa-1"
            :task-index 0 :content "Extract depth caps")
   '(:event "subagent.start" :subagent-id "sa-3" :parent-id "sa-1"
            :delegation-id "d2" :task-index 0 :content "Extract depth caps")))

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
                   '("  ● 🧠 delegate_task · 2 tasks · 1 running · 1 done · +1 nested"
                     "  ├ ● Root A 1:00"
                     "  │   ↳ 📖 read_file x.el · 3 tools"
                     "  │   └ ◌ Child C queued"
                     "  └ ✓ Root B 30s"
                     "      ↳ All fields listed")))
    (let ((pos (string-search "30s" text)))
      (should (eq (get-text-property pos 'face text) 'shadow))
      (should (eq (car (get-text-property (1- pos) 'display text)) 'space)))
    (should (eq (get-text-property (string-search "✓" text) 'face text)
                'hermes-work-done))
    (should (eq (get-text-property (string-search "◌" text) 'face text)
                'hermes-work-queued))
    ;; A result's arrow stays quiet; only a failure's text is red.
    (let ((failed (hermes-chat--card-text (hermes-test--card-sample 90 nil))))
      (should (eq (get-text-property (string-search "↳ Error:" failed) 'face failed) 'shadow))
      (should (eq (get-text-property (string-search "Error:" failed) 'face failed)
                  'hermes-work-failed)))))

;;;; Live tree in place

(ert-deftest hermes-work-plain-line-strips-inline-markdown ()
  "Result lines drop Markdown marks, and the card shows the first real line."
  (should (equal (hermes-work-plain-line "## **Verdict:** use `foo-bar`") "Verdict: use foo-bar"))
  (should (equal (hermes-work-plain-line "> - __Done__: all green ") "Done: all green"))
  (should (equal (hermes-work-plain-line "-1 offset, a-b * c") "-1 offset, a-b * c"))
  ;; Unpaired marks are text, not emphasis.
  (should (equal (hermes-work-plain-line "make: *** [Makefile:88: test] Error 1")
                 "make: *** [Makefile:88: test] Error 1"))
  (should (equal (hermes-work-plain-line "**`a * b`** and __c__") "a * b and c"))
  (should (equal (hermes-chat--card-first-line "\n**\n# `Findings`\nmore") "Findings"))
  (let ((text (hermes-chat--card-text
               (list :id "e" :role 'tool :status 'running
                     :metadata '(:event (:name "delegate_task"))
                     :card (list :kind 'delegate :tasks 1 :now 100.0
                                 :rows '((:id "a" :state completed :goal "Review"
                                               :started 40.0 :finished 90.0
                                               :summary "**Accept.** No `MUST` findings")))))))
    (should (string-search "↳ Accept. No MUST findings" text))))

(ert-deftest hermes-chat-card-updates-in-place-preserving-readers ()
  "The card repaints in place; draft, undo, point and reader windows hold."
  (hermes-test--with-card-chat
    (hermes-chat--insert-entry
     (hermes-chat--make-entry 'assistant
                              (mapconcat (lambda (n) (format "older line %d" n))
                                         (number-sequence 1 40) "\n")
                              'done "older"))
    (hermes-test--card-spawn)
    (should (equal (hermes-test--card-line "delegate_task")
                   "  ● 🧠 delegate_task · 2 tasks · 1 running · 1 queued · +1 nested"))
    (should (hermes-test--card-line "└ ● Map the agents panel"))
    (should (hermes-test--card-line "    └ ● Extract depth caps"))
    ;; A task no worker has picked up yet is only counted.
    (should-not (hermes-test--card-line "Inventory process fields"))
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
        ;; The split halves the width; the card refits to the narrower window.
        (should (string-search "↳ 📖 read_file agents"
                               (hermes-test--card-line "read_file")))
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
     '(:event "subagent.start" :subagent-id "sa-2" :delegation-id "d1")
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
               "^  ✗ ▸ 🧠 delegate_task · 2 tasks · 1 done · 1 failed · \\+1 nested +1:02$"
               transcript))
      (should-not (string-search "Map the agents panel" transcript)))
    (let ((head (save-excursion (goto-char (point-min))
                                (search-forward "delegate_task")
                                (1- (point)))))
      (should (eq (get-text-property head 'action) #'hermes-chat--toggle-entry-button))
      ;; The fold glyph is quiet like other toggles, not a link.
      (let ((fold (save-excursion (goto-char (point-min)) (search-forward "▸") (1- (point)))))
        (should (eq (get-text-property fold 'face) 'shadow)))
      (push-button head)
      (should (hermes-test--card-line "  ✗ ▾ 🧠 delegate_task"))
      (should (hermes-test--card-line "├ ✓ Map the agents panel"))
      (should (hermes-test--card-line "↳ panel budget is 6 rows"))
      (should (hermes-test--card-line "└ ✗ Inventory process fields"))
      (should (hermes-test--card-line "↳ process.list timed out"))
      (push-button head)
      (should-not (hermes-test--card-line "Map the agents panel")))
    ;; Settled cards stop ticking.
    (should-not hermes-chat--card-timer)))

;;;; Background process

(ert-deftest hermes-work-ago-reads-the-same-everywhere ()
  "Ages under a minute count seconds; longer ones use the worker clock."
  (should (equal (hermes-work-ago 12.7) "12s ago"))
  (should (equal (hermes-work-ago -3) "0s ago"))
  (should (equal (hermes-work-ago 65) "1:05 ago")))

(ert-deftest hermes-chat-clock-is-one-vocabulary ()
  "Tool rows, cards, the outline and the status rule share one clock.
Seconds below a minute, then m:ss and h:mm:ss; only tool rows keep
tenths, and only below ten seconds."
  (should (equal (mapcar #'hermes-chat--clock '(0 12.7 59.9 73 3725))
                 '("0s" "12s" "59s" "1:13" "1:02:05")))
  (should (equal (mapcar #'hermes-work-duration '(12 266)) '("12s" "4:26")))
  (should (equal (mapcar #'hermes-chat--format-duration '(1.83 9.96 73.4 "3 min"))
                 '("1.8s" "9s" "1:13" "3 min"))))

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

;;;; Attribution

(defun hermes-test--card-delegate (id goals &optional args)
  "Return a running `delegate_task' start with call ID for GOALS and ARGS."
  (list :type 'tool :event "tool.start" :name "delegate_task" :tool-call-id id
        :status "running"
        :args (or args
                  `((tasks . ,(vconcat (mapcar (lambda (goal) `((goal . ,goal)))
                                               goals)))))))

(defun hermes-test--card-of (id)
  "Return the card of the tool row whose call id is ID."
  (plist-get (seq-find (lambda (entry)
                         (equal (plist-get (plist-get (plist-get entry :metadata) :event)
                                           :tool-call-id)
                                id))
                       (hermes-chat--entries))
             :card))

(defun hermes-test--card-ids (id)
  "Return the worker ids on the card of tool call ID."
  (mapcar (lambda (row) (plist-get row :id))
          (plist-get (hermes-test--card-of id) :rows)))

(defun hermes-test--card-start (id delegation goal &optional parent)
  "Return a start event of worker ID in DELEGATION for GOAL under PARENT."
  (append (list :event "subagent.start" :subagent-id id :delegation-id delegation
                :content goal)
          (and parent (list :parent-id parent))))

(ert-deftest hermes-chat-card-non-spawn-delegate-calls-stay-plain ()
  "`delegate_task' list, steer and stop calls never become cards."
  (hermes-test--with-card-chat
    (hermes-test--card-events (hermes-test--card-start "sa-1" "d1" "Running goal"))
    (dolist (action '("list" "steer" "stop"))
      (hermes-test--card-tool
       (hermes-test--card-delegate (concat "call-" action) nil
                                   `((action . ,action) (subagent_id . "sa-1")))))
    (should-not hermes-chat--card-nodes)
    (should-not (seq-some (lambda (entry) (plist-get entry :card)) (hermes-chat--entries)))
    (should-not (hermes-test--card-line "Running goal"))))

(ert-deftest hermes-chat-card-overlapping-batches-stay-apart ()
  "Two live batches each get their own workers, never the newest row all."
  (hermes-test--with-card-chat
    (hermes-test--card-tool (hermes-test--card-delegate "call-a" '("Audit A" "Audit B")))
    (hermes-test--card-tool (hermes-test--card-delegate "call-b" '("Build C")))
    (hermes-test--card-events
     (hermes-test--card-start "w-c" "d2" "Build C")
     (hermes-test--card-start "w-a" "d1" "Audit A")
     (hermes-test--card-start "w-n" "d3" "Nested under C" "w-c")
     (hermes-test--card-start "w-b" "d1" "Audit B"))
    (should (equal (hermes-test--card-ids "call-a") '("w-a" "w-b")))
    (should (equal (hermes-test--card-ids "call-b") '("w-c" "w-n")))
    (should (equal (plist-get (hermes-test--card-of "call-a") :delegation) "d1"))
    (should (equal (plist-get (hermes-test--card-of "call-b") :delegation) "d2"))))

(ert-deftest hermes-chat-card-ambiguous-batches-wait-for-their-results ()
  "Batches with the same goals stay unattributed until a result names one."
  (hermes-test--with-card-chat
    (hermes-test--card-tool (hermes-test--card-delegate "call-a" '("Same goal")))
    (hermes-test--card-tool (hermes-test--card-delegate "call-b" '("Same goal")))
    (hermes-test--card-events (hermes-test--card-start "w-1" "d1" "Same goal")
                              (hermes-test--card-start "w-2" "d2" "Same goal"))
    ;; Both rows show their head and count, but claim no worker.
    (should-not (hermes-test--card-ids "call-a"))
    (should-not (hermes-test--card-ids "call-b"))
    (should (= 2 (cl-count-if (lambda (line)
                                (string-search "delegate_task · 1 task · 1 queued" line))
                              (split-string (hermes-test--transcript) "\n"))))
    ;; A finished batch names its live transcripts' delegation directory.
    (hermes-test--card-tool
     (append '(:type tool :event "tool.complete" :status "completed"
                     :result "{\"results\":[{\"task_index\":0,\"status\":\"completed\",\"live_transcript\":\"/h/cache/delegation/live/d2/task-0.log\"}]}")
             (cl-subseq (hermes-test--card-delegate "call-a" '("Same goal")) 4)))
    (should (equal (hermes-test--card-ids "call-a") '("w-2")))
    ;; The other batch is now the only match for the other row.
    (should (equal (hermes-test--card-ids "call-b") '("w-1")))))

(ert-deftest hermes-chat-card-result-overrides-a-goal-match ()
  "A result naming another delegation drops the goal-matched workers and
their nested workers, then follows the named batch."
  (hermes-test--with-card-chat
    (let ((start (hermes-test--card-delegate "call-a" '("Goal"))))
      (hermes-test--card-tool start)
      (hermes-test--card-events (hermes-test--card-start "old" "d-old" "Goal")
                                (hermes-test--card-start "old-kid" "d-x" "Stale nested" "old"))
      (should (equal (hermes-test--card-ids "call-a") '("old" "old-kid")))
      (should (equal (plist-get (hermes-test--card-of "call-a") :delegation) "d-old"))
      (hermes-test--card-tool
       (append '(:type tool :event "tool.complete" :status "completed"
                       :result ((delegation_id . "d-new") (subagent_ids . ["new"])))
               (cl-subseq start 4)))
      (hermes-test--card-events (hermes-test--card-start "new" "d-new" "Goal")
                                (hermes-test--card-start "new-kid" "d-y" "Fresh nested" "new"))
      (should (equal (hermes-test--card-ids "call-a") '("new" "new-kid")))
      (should (equal (plist-get (hermes-test--card-of "call-a") :delegation) "d-new"))
      (should (equal (plist-get (hermes-test--card-of "call-a") :roots) '("new")))
      (let ((transcript (hermes-test--transcript)))
        (should (string-search "delegate_task · 1 task · 1 running · +1 nested" transcript))
        (should-not (string-search "Stale nested" transcript)))
      ;; A confirmed batch keeps its rows on later refreshes.
      (hermes-chat--cards-refresh)
      (should (equal (hermes-test--card-ids "call-a") '("new" "new-kid"))))))

(ert-deftest hermes-chat-card-confirmed-rows-is-pure ()
  "Only roots the result names or owns survive, each with its subtree."
  (let ((rows '((:id "a" :delegation-id "d1") (:id "a1" :parent "a")
                (:id "b" :delegation-id "d2") (:id "b1" :parent "b")
                (:id "b2" :parent "b1") (:id "c"))))
    (should (equal (mapcar (lambda (row) (plist-get row :id))
                           (hermes-chat--card-confirmed-rows rows "d1" '("c")))
                   '("a" "a1" "c")))
    (should-not (hermes-chat--card-confirmed-rows rows nil nil))))

(ert-deftest hermes-chat-card-dispatch-result-names-roots-once ()
  "A background dispatch's subagent ids attach workers; refreshes never grow them."
  (hermes-test--with-card-chat
    (let ((start (hermes-test--card-delegate "call-a" '("Goal one"))))
      (hermes-test--card-tool start)
      (hermes-test--card-tool
       (append '(:type tool :event "tool.complete" :status "completed"
                       :result ((status . "dispatched") (delegation_id . "d9")
                                (subagent_ids . ["sa-9"])))
               (cl-subseq start 4)))
      (hermes-test--card-events
       '(:event "subagent.spawn_requested" :subagent-id "sa-9" :content "Goal one"))
      (should (equal (hermes-test--card-ids "call-a") '("sa-9")))
      (dotimes (_ 3) (hermes-chat--cards-refresh))
      (should (equal (plist-get (hermes-test--card-of "call-a") :roots) '("sa-9"))))))

(ert-deftest hermes-chat-card-process-rebinds-to-the-result-session ()
  "Identical commands match provisionally, then follow their own session ids."
  (hermes-test--with-card-chat
    (let ((starts (mapcar (lambda (id)
                            (list :type 'tool :event "tool.start" :name "terminal"
                                  :tool-call-id id :status "running"
                                  :args '((command . "make check") (background . t))))
                          '("call-1" "call-2"))))
      (mapc #'hermes-test--card-tool starts)
      (hermes-test--card-processes
       '((:id "proc_a" :kind process :state running :command "make check" :uptime 9)
         (:id "proc_b" :kind process :state running :command "make check" :uptime 8)))
      (should (equal (plist-get (hermes-test--card-of "call-1") :id) "proc_a"))
      (should (equal (plist-get (hermes-test--card-of "call-2") :id) "proc_b"))
      ;; The results say the reverse; each card follows its own session.
      (cl-mapc (lambda (start session)
                 (hermes-test--card-tool
                  (append `(:type tool :event "tool.complete" :status "completed"
                                  :result ((session_id . ,session)))
                          (cl-subseq start 4))))
               starts '("proc_b" "proc_a"))
      (should (equal (plist-get (hermes-test--card-of "call-1") :id) "proc_b"))
      (should (equal (plist-get (hermes-test--card-of "call-2") :id) "proc_a"))
      (should (= (plist-get (hermes-test--card-of "call-1") :started)
                 (- hermes-test--card-now 8))))))

(ert-deftest hermes-chat-card-failed-process-keeps-its-reason ()
  "A failed process keeps its last line, in the failed face."
  (hermes-test--with-card-chat
    (let ((start '(:type tool :event "tool.start" :name "terminal" :tool-call-id "call-t"
                         :status "running"
                         :args ((command . "make check") (background . t)))))
      (hermes-test--card-tool start)
      (hermes-test--card-processes
       `((:id "proc_1" :kind process :state failed :exit-code 2 :command "make check"
              :last-line "make: *** [check] Error 2" :exited-at ,hermes-test--card-now)))
      (should (equal (hermes-test--card-line "↳") "    ↳ make: *** [check] Error 2"))
      (let ((face-at (lambda (needle)
                       (get-text-property
                        (save-excursion (goto-char (point-min))
                                        (- (search-forward needle) (length needle)))
                        'face))))
        ;; Only the reason and verdict are red; the command stays readable.
        (should (eq (funcall face-at "↳") 'shadow))
        (should (eq (funcall face-at "make: ***") 'hermes-work-failed))
        (should (eq (funcall face-at "exit 2") 'hermes-work-failed))
        (should-not (funcall face-at "terminal: make check"))))))

(ert-deftest hermes-chat-card-process-keeps-the-command-over-its-marker ()
  "A narrow process card drops `(background)' rather than cut the command."
  (let ((text (lambda (width)
                (substring-no-properties
                 (hermes-chat--card-text
                  (list :id "t" :role 'tool :status 'completed
                        :metadata '(:event (:name "terminal"
                                                  :args ((command . "nix build .#hermes --no-link")
                                                         (background . t))))
                        :card (list :kind 'process :now 500.0 :started 300.0 :width width
                                    :row '(:id "p" :state running
                                                :command "nix build .#hermes --no-link"))))))))
    (should (string-search "nix build .#hermes --no-link (background)" (funcall text 90)))
    (let ((narrow (funcall text 54)))
      (should (string-search "nix build .#hermes --no-link" narrow))
      (should-not (string-search "(background)" narrow)))
    (should-not (string-search "(background)" (funcall text 40)))))

;;;; Width

(defun hermes-test--card-overflows (text width)
  "Return the lines of card TEXT that overflow WIDTH columns.
A right-aligned part needs its own width plus two columns of gap."
  (seq-remove
   (lambda (line)
     (let ((split (text-property-not-all 0 (length line) 'display nil line)))
       (<= (if split
               (+ (string-width (substring line 0 split))
                  2 (string-width (substring line (1+ split))))
             (string-width line))
           width)))
   (split-string text "\n" t)))

(defun hermes-test--card-aligned-p (text)
  "Return non-nil when card TEXT right-aligns anything."
  (text-property-not-all 0 (length text) 'display nil text))

(defun hermes-test--card-sample (width settled)
  "Return a delegate card entry WIDTH columns wide, SETTLED or live."
  (let ((long "Audit the subagent outline, header, mode line and inline cards for overflow"))
    (list :id "e" :role 'tool :status (if settled 'completed 'running)
          :metadata '(:event (:name "delegate_task"))
          :expanded settled
          :card (list :kind 'delegate :tasks 3 :now 400.0 :width width
                      :rows `((:id "a" :state ,(if settled 'completed 'running) :goal ,long
                                   :started 100.0 ,@(and settled '(:finished 380.0))
                                   :tools (("read_file" . "lisp/hermes-chat-cards.el and much more text"))
                                   :tool-count 12
                                   :summary "Everything fits within the window at every width we tried")
                              (:id "n" :parent "a" :state completed :goal ,long
                                   :started 120.0 :finished 200.0 :summary ,long)
                              (:id "b" :state failed :goal "Short goal" :started 110.0
                                   :finished 300.0
                                   :error ,(concat "Error: " long))
                              (:id "c" :state queued :goal ,long))))))

(ert-deftest hermes-chat-card-fits-the-window-width ()
  "No card line overflows at 60, 90 or 140 columns; narrow cards drop times."
  (dolist (width '(60 90 140 36))
    (dolist (settled '(nil t))
      (let ((text (hermes-chat--card-text (hermes-test--card-sample width settled))))
        (should-not (hermes-test--card-overflows text width))
        (should (eq (>= width 40) (and (hermes-test--card-aligned-p text) t)))))
    (let* ((row '(:id "p" :state running :command "make check" :last-line
                      "a very long output line that goes on and on and on and on and on and on and on and on"))
           (entry (list :id "t" :role 'tool :status 'completed
                        :metadata '(:event (:name "terminal"
                                                  :args ((command . "make -j8 check TESTS='a very long list of test files that never ends'")
                                                         (background . t))))
                        :card (list :kind 'process :row row :now 500.0 :started 300.0
                                    :width width))))
      (should-not (hermes-test--card-overflows (hermes-chat--card-text entry) width)))))

(ert-deftest hermes-chat-card-head-drops-tasks-then-nested ()
  "A narrowing head drops the task total first, then the nested total;
the state counts stay to the last."
  (let ((ranks
         (mapcar
          (lambda (width)
            (let* ((text (hermes-chat--card-text (hermes-test--card-sample width nil)))
                   (head (car (split-string (substring-no-properties text) "\n")))
                   (tasks (string-search "3 tasks" head))
                   (nested (string-search "+1 nested" head)))
              (should (string-search "1 running" head))
              (should-not (and tasks (not nested)))
              (cond (tasks 2) (nested 1) (t 0))))
          (number-sequence 40 100))))
    ;; Wider never shows less, and every stage occurs.
    (should (equal ranks (sort (copy-sequence ranks) #'<)))
    (should (equal (seq-uniq ranks) '(0 1 2)))))

(ert-deftest hermes-chat-card-width-follows-the-window ()
  "Resizing the chat window refits the card."
  (hermes-test--with-card-chat
    (hermes-test--card-spawn)
    (let ((wide (plist-get (hermes-test--card-of "call-1") :width)))
      (split-window-right)
      (run-hooks 'window-configuration-change-hook)
      (should (< (plist-get (hermes-test--card-of "call-1") :width) wide)))))

(ert-deftest hermes-chat-card-width-caps-and-times-follow-the-card ()
  "Wide windows cap cards, and right-hand times align to the card's edge."
  (let ((hermes-chat-card-max-width 100))
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (cl-letf (((symbol-function 'hermes-chat--table-window-width) (lambda () 170)))
        (should (= (hermes-chat--card-width) 100)))
      (cl-letf (((symbol-function 'hermes-chat--table-window-width) (lambda () 60)))
        (should (= (hermes-chat--card-width) 60)))))
  (let* ((line (hermes-chat--card-line "  ✓ Root" "1:13" 100))
         (pos (string-search "1:13" line)))
    (should (equal (get-text-property (1- pos) 'display line) '(space :align-to 97)))))

;;;; Timers

(ert-deftest hermes-chat-card-timer-stops-when-hidden-or-retired ()
  "Hidden cards never tick; a mode change cancels the timer and the cards."
  (hermes-test--with-card-chat
    (hermes-test--card-spawn)
    (should hermes-chat--card-timer)
    (let ((chat (current-buffer)))
      (set-window-buffer (selected-window) (get-buffer-create " *card hidden*"))
      (with-current-buffer chat
        (hermes-chat--cards-tick chat)
        (should-not hermes-chat--card-timer)
        ;; Work changes while hidden repaint but never rearm the clock.
        (setq hermes-test--card-now (+ hermes-test--card-now 5))
        (hermes-test--card-events
         '(:event "subagent.tool" :subagent-id "sa-1" :name "read_file" :preview "x"))
        (should-not hermes-chat--card-timer))
      (set-window-buffer (selected-window) chat)
      (with-current-buffer chat
        (run-hooks 'window-configuration-change-hook)
        (should hermes-chat--card-timer)
        (run-hooks 'change-major-mode-hook)
        (should-not hermes-chat--card-timer)
        (should-not hermes-chat--card-nodes))
      (kill-buffer " *card hidden*"))))

;;;; Cross-chat segment

(ert-deftest hermes-chat-work-indicator-toggle-drives-hidden-polling ()
  "Turning the segment on polls hidden chats slowly; turning it off stops."
  (save-window-excursion
    (hermes-test-with-chat-buffer
      (let ((chat (current-buffer))
            (client (hermes-test--dashboard-client))
            (hermes-chat--work-kinds '(:delegates))
            (hermes-chat-work-indicator nil)
            (hermes-chat-work-hidden-cadence 30)
            (global-mode-string nil)
            (setter (get 'hermes-chat-work-indicator 'custom-set))
            calls)
        (setf (hermes-dashboard-transport-client-ready-p client) t)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-subagent-list)
                   (lambda (_client &rest args) (push args calls) (length calls)))
                  ((symbol-function 'run-at-time)
                   (lambda (delay _repeat fn &rest args) (list delay fn args)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
          (unwind-protect
              (with-current-buffer chat
                (setq hermes-chat--dashboard-client client
                      hermes-chat--dashboard-active-session-id "runtime"
                      hermes-chat--dashboard-session-ready-p t)
                (hermes-chat--work-bind client "runtime")
                (let* ((owner hermes-chat--work-owner)
                       (bind-read (plist-get owner :timer)))
                  ;; Off: the hidden chat gets its one bind read, then rests.
                  (apply (cadr bind-read) (nth 2 bind-read))
                  (should (= (length calls) 1))
                  (funcall (plist-get (car calls) :resolve)
                           (hermes-transport-json-parse-lossless "{\"subagents\":[]}"))
                  (should-not (plist-get owner :timer))
                  (funcall setter 'hermes-chat-work-indicator t)
                  (should (memq 'hermes-chat-work-indicator-mode-line
                                (default-value 'global-mode-string)))
                  (let ((timer (plist-get owner :timer)))
                    (should (= (car timer) 30))
                    (should (plist-get owner :hidden))
                    (apply (cadr timer) (nth 2 timer))
                    (should (= (length calls) 2)))
                  (funcall setter 'hermes-chat-work-indicator nil)
                  (should-not (plist-get owner :timer))
                  (should-not (memq 'hermes-chat-work-indicator-mode-line
                                    (default-value 'global-mode-string)))))
            (kill-buffer " *work hidden*")))))))

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
                                 (format " [Hermes 🤖 %s 2 ✗ 1]" (buffer-name hidden))))
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
