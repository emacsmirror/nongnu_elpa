;;; hermes-session-control-tests.el --- Goal, loop and heartbeat control tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Released `session.control' snapshots: ordering, header, view and actions.

;;; Code:

(require 'hermes-test-helpers)

(defun hermes-test--control (revision &rest overrides)
  "Return a released control snapshot at REVISION.
OVERRIDES is an alist of top-level sections replacing the defaults."
  (let ((control
         (copy-tree
         `((revision . ,revision) (updated_at . 100)
           (goal . ((title . "Ship the release") (status . "active")
                    (turns_used . 3) (max_turns . 20)
                    (contract . ((outcome . "A working release") (verification . "")
                                 (constraints . "") (boundaries . "") (stop_when . "")))
                    (subgoals . ["Test recovery" "Review changes"]) (gates . [])
                    (created_at . 100)))
           (loop . ((status . "active") (prompt . "Check the build") (mode . "interval")
                    (interval_seconds . 300) (current_delay . 300) (times . 5)
                    (until . "") (max_ticks . 100) (ticks_fired . 2)
                    (created_at . 100) (next_due_at . 0)
                    (awaiting_response . :false) (deferred_by_goal . :false)))
           (heartbeat . :null)))))
    (dolist (override (car overrides) control)
      (setf (alist-get (car override) control) (cdr override)))))

(defun hermes-test--control-wire (revision &rest overrides)
  "Return `hermes-test--control' REVISION OVERRIDES as the client parses it."
  (hermes-transport-json-parse
   (json-serialize (apply #'hermes-test--control revision overrides))))

(defun hermes-test--control-snapshot (revision &rest overrides)
  "Return the normalized snapshot for `hermes-test--control' REVISION OVERRIDES."
  (hermes-transport-control-snapshot
   (apply #'hermes-test--control-wire revision overrides)))

(defmacro hermes-test--with-control-chat (&rest body)
  "Run BODY in a chat attached to live session \"sid\"."
  (declare (indent 0) (debug t))
  `(hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
           hermes-chat--dashboard-active-session-id "sid"
           hermes-chat--dashboard-session-ready-p t)
     ,@body))

;;;; Reducer

(ert-deftest hermes-control-reduce-orders-replies-by-epoch ()
  "A reply applies only when no push or detach happened since its request."
  (let* ((a (hermes-test--control-snapshot "a"))
         (b (hermes-test--control-snapshot "b"))
         (state (hermes-session-control-reduce nil (list :push a)))
         (epoch (plist-get state :epoch)))
    (should (eq (plist-get state :snapshot) a))
    (setq state (hermes-session-control-reduce state '(:request nil)))
    (setq state (hermes-session-control-reduce state (list :push b)))
    (setq state (hermes-session-control-reduce state (list :reply epoch a nil)))
    (should (eq (plist-get state :snapshot) b))
    (setq state (hermes-session-control-reduce
                 state (list :reply (plist-get state :epoch) a nil)))
    (should (eq (plist-get state :snapshot) a))
    (should-not (plist-get (hermes-session-control-reduce state '(:forget)) :snapshot))))

(ert-deftest hermes-control-reduce-keeps-identical-revision ()
  "An equal revision is a no-op that keeps the current snapshot object."
  (let* ((a (hermes-test--control-snapshot "a"))
         (state (hermes-session-control-reduce nil (list :push a))))
    (should (eq (plist-get (hermes-session-control-reduce
                            state (list :push (hermes-test--control-snapshot "a")))
                           :snapshot)
                a))))

(ert-deftest hermes-control-reduce-tracks-one-pending-action ()
  "An action is pending until its own reply or failure settles it."
  (let ((state (hermes-session-control-reduce nil '(:request "loop.pause"))))
    (should (equal (plist-get state :pending) "loop.pause"))
    (should (equal (plist-get (hermes-session-control-reduce state '(:reply 0 nil nil))
                              :pending)
                   "loop.pause"))
    (let ((failed (hermes-session-control-reduce state '(:fail "boom" "loop.pause"))))
      (should-not (plist-get failed :pending))
      (should (equal (plist-get failed :error) "boom"))
      (should-not (plist-get (hermes-session-control-reduce failed '(:request nil))
                             :error)))))

;;;; Wire

(ert-deftest hermes-control-push-updates-only-its-own-session ()
  "A pushed snapshot reaches its session's header, never the transcript."
  (hermes-test--with-control-chat
    (let ((client hermes-chat--dashboard-client)
          (callback (hermes-chat--transport-callback
                     (current-buffer) nil t hermes-chat--transport-generation)))
      (setf (hermes-dashboard-transport-client-callback client) callback
            hermes-chat--dashboard-active-session-id "sid-active")
      (hermes-test--emit-dashboard-event
       client "session.control.update" `((control . ,(hermes-test--control "a"))))
      (should (equal (plist-get (hermes-session-control-snapshot) :revision) "a"))
      (should-not hermes-chat--pending-assistant-id)
      (should-not (hermes-chat--entries))
      (funcall callback (list :type 'session-control :session-id "other"
                              :control '(:revision "wrong")))
      (should (equal (plist-get (hermes-session-control-snapshot) :revision) "a")))))

(ert-deftest hermes-control-read-cannot-overwrite-push-or-successor ()
  "A read reply loses to a later push and to a new attachment."
  (hermes-test--with-control-chat
    (let (callbacks)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control-read)
                 (lambda (_client &rest args) (push (plist-get args :resolve) callbacks))))
        (hermes-session-control-refresh)
        (hermes-session-control-accept
         (list :type 'session-control :session-id "sid"
               :control (hermes-test--control-snapshot "new")))
        (funcall (car callbacks) `((control . ,(hermes-test--control-wire "old"))))
        (should (equal (plist-get (hermes-session-control-snapshot) :revision) "new"))
        (hermes-session-control-refresh)
        (setq hermes-chat--dashboard-active-session-id "successor")
        (funcall (car callbacks) `((control . ,(hermes-test--control-wire "wrong"))))
        (should (equal (plist-get (hermes-session-control-snapshot) :revision) "new"))))))

(ert-deftest hermes-control-rpc-serializes-exact-session-action-and-args ()
  "The typed RPCs send the backend's exact method and params."
  (let (calls)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method params &rest _)
                 (push (cons method params) calls))))
      (hermes-dashboard-transport-session-control-read 'client :session-id "session")
      (hermes-dashboard-transport-session-control
       'client "subgoal.remove" :session-id "session" :args '((index . 2))))
    (should (equal (car calls) '("session.control" (session_id . "session")
                                 (action . "subgoal.remove") (args (index . 2)))))
    (should (equal (cadr calls) '("session.control.read" (session_id . "session"))))))

;;;; Actions

(ert-deftest hermes-control-actions-follow-released-state ()
  "Only actions the backend accepts in the current state are offered."
  (should (equal (hermes-session-control-actions (hermes-test--control-snapshot "a"))
                 '("goal.pause" "subgoal.add" "subgoal.remove" "subgoal.clear"
                   "goal.clear" "loop.pause" "loop.stop")))
  (should (equal (hermes-session-control-actions
                  (hermes-test--control-snapshot
                   "b" '((goal . ((status . "paused") (subgoals . [])
                                  (wait_barrier . ((type . "pid") (target . 7)))))
                         (loop . :null)
                         (heartbeat . ((status . "paused") (interval_seconds . 60))))))
                 '("goal.resume" "goal.unwait" "subgoal.add" "goal.clear"
                   "heartbeat.resume" "heartbeat.clear")))
  (should (equal (hermes-session-control-actions
                  (hermes-test--control-snapshot
                   "c" '((goal . ((status . "done"))) (loop . ((status . "done"))))))
                 '("goal.clear" "loop.stop")))
  (should-not (hermes-session-control-actions nil)))

(defmacro hermes-test--with-control-view (&rest body)
  "Run BODY in a control view of a chat holding snapshot \"a\"."
  (declare (indent 0) (debug t))
  `(hermes-test--with-control-chat
     (let ((chat (current-buffer))
           (view (hermes-buffer--get "*Hermes Controls: test*"
                                     #'hermes-session-control-mode)))
       (unwind-protect
           (progn
             (hermes-session-control-accept
              (list :type 'session-control :session-id "sid"
                    :control (hermes-test--control-snapshot "a")))
             (setq hermes-session-control--view view)
             (with-current-buffer view
               (setq hermes-session-control--chat chat)
               (hermes-session-control--redisplay)
               ,@body))
         (kill-buffer view)))))

(ert-deftest hermes-control-action-is-pending-until-the-backend-answers ()
  "An action waits for the backend's snapshot and blocks a second action."
  (hermes-test--with-control-view
    (let (sent)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control)
                 (lambda (_client action &rest args) (push (cons action args) sent))))
        (hermes-session-control-loop-pause)
        (should (equal (caar sent) "loop.pause"))
        (should (equal (plist-get (cdar sent) :session-id) "sid"))
        (should (string-match-p "Applying loop.pause" (buffer-string)))
        ;; No optimistic state: the loop still reads active.
        (should (equal (plist-get (plist-get (with-current-buffer hermes-session-control--chat
                                               (hermes-session-control-snapshot))
                                             :loop)
                                  :status)
                       "active"))
        (should-error (hermes-session-control-loop-stop) :type 'user-error)
        (funcall (plist-get (cdar sent) :resolve)
                 `((control . ,(hermes-test--control-wire
                                "b" '((loop . ((status . "paused") (interval_seconds . 300))))))))
        (should-not (string-match-p "Applying" (buffer-string)))
        (should (string-match-p "Paused 0 ticks · every 5m" (buffer-string)))))))

(ert-deftest hermes-control-clear-asks-and-subgoal-remove-sends-index ()
  "Clearing needs confirmation; removing a criterion sends its 1-based index."
  (hermes-test--with-control-view
    (let (sent)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control)
                 (lambda (_client action &rest args) (push (cons action args) sent)))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car (nth 1 choices)))))
        (hermes-session-control-goal-clear)
        (should-not sent)
        (hermes-session-control-subgoal-remove)
        (should (equal (caar sent) "subgoal.remove"))
        (should (equal (plist-get (cdar sent) :args) '((index . 2))))))))

(defun hermes-test--goal-resume-envelope ()
  "Return the released `session.control' reply to goal.resume."
  `((control . ,(hermes-test--control "resumed"))
    (dispatch . ((type . "send")
                 (output . :null)
                 (notice . "▶ Goal resumed: Ship the release\nContinuing now — taking the next step.")
                 (message . "[Continuing toward your standing goal]\nGoal: Ship the release")
                 (display . "/goal resume")))))

(ert-deftest hermes-control-goal-resume-sends-the-backend-continuation ()
  "Goal Resume submits the backend's continuation like /goal resume does.
The draft stays in the composer, and a busy chat queues the continuation."
  (dolist (busy '(nil t))
    (hermes-test--with-control-view
      (let ((chat hermes-session-control--chat) submitted queued)
        (with-current-buffer chat
          (hermes-session-control-accept
           (list :type 'session-control :session-id "sid"
                 :control (hermes-test--control-snapshot
                           "paused" '((goal . ((title . "Ship the release")
                                               (status . "paused")))))))
          (goto-char (point-max))
          (insert "my draft")
          (when busy (setq hermes-chat--dashboard-running-p t)))
        (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control)
                   (lambda (_client _action &rest args)
                     (funcall (plist-get args :resolve)
                              (hermes-transport-json-parse
                               (json-serialize (hermes-test--goal-resume-envelope))))))
                  ((symbol-function 'hermes-chat--queue-content)
                   (lambda (content &rest _) (push content queued)))
                  ((symbol-function 'hermes-chat--drain-queued-message) #'ignore))
          (with-current-buffer chat
            (setq-local hermes-chat--submit-function
                        (lambda (content &rest _) (push content submitted))))
          (hermes-session-control-goal-resume))
        (with-current-buffer chat
          (should (equal (hermes-chat-input-string) "my draft"))
          (should (string-match-p "Goal resumed" (buffer-string)))
          (should-not hermes-chat--command-owner)
          (should (equal (if busy queued submitted)
                         '("[Continuing toward your standing goal]\nGoal: Ship the release")))
          (should-not (if busy submitted queued)))))))

;;;; Reconciliation

(defmacro hermes-test--with-control-wire (reads &rest body)
  "Run BODY in a chat whose dashboard callback and reads are captured.
Each `session.control.read' pushes its resolve function onto READS."
  (declare (indent 1) (debug t))
  `(hermes-test--with-control-chat
     (let* ((client hermes-chat--dashboard-client)
            (callback (hermes-chat--transport-callback
                       (current-buffer) nil t hermes-chat--transport-generation))
            (,reads nil))
       (setf (hermes-dashboard-transport-client-callback client) callback
             hermes-chat--dashboard-active-session-id "sid-active")
       (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control-read)
                  (lambda (_client &rest args)
                    (should (equal (plist-get args :session-id) "sid-active"))
                    (push (plist-get args :resolve) ,reads))))
         (unwind-protect (progn ,@body)
           (when (bound-and-true-p hermes-session-control--timer)
             (cancel-timer hermes-session-control--timer)))))))

(defun hermes-test--loop-control (revision ticks &rest loop)
  "Return a released snapshot at REVISION whose loop fired TICKS of 5.
LOOP overrides the loop's fields."
  (hermes-test--control
   revision
   `((goal . :null)
     (loop . ,(append loop
                      `((status . "active") (prompt . "Check the build")
                        (mode . "interval") (interval_seconds . 300)
                        (current_delay . 300) (times . 5) (ticks_fired . ,ticks)
                        (awaiting_response . :false) (deferred_by_goal . :false)))))))

(defun hermes-test--control-header ()
  "Return this chat's plain control header."
  (when-let* ((header (hermes-session-control-header)))
    (substring-no-properties header)))

(ert-deftest hermes-control-model-tick-rereads-at-fire-and-turn-end ()
  "A model-backed tick advances the header without a backend push."
  (hermes-test--with-control-wire reads
    (let ((due (+ (float-time) 300)))
      (hermes-test--emit-dashboard-event
       client "session.control.update"
       `((control . ,(hermes-test--loop-control "a" 2 `(next_due_at . ,due)))))
      (should-not reads)
      ;; The tick fires: a status notice, then the turn; no snapshot yet.
      (hermes-test--emit-dashboard-event
       client "status.update" '((kind . "loop") (text . "↻ /loop wakeup #3 firing…")))
      (should (= (length reads) 1))
      (funcall (pop reads)
               `((control . ,(hermes-transport-json-parse
                              (json-serialize
                               (hermes-test--loop-control
                                "b" 3 '(awaiting_response . t)
                                `(next_due_at . ,(+ due 300))))))))
      (should (equal (hermes-test--control-header) "Loop waiting 3/5 · every 5m"))
      (hermes-test--emit-dashboard-event
       client "message.complete" '((text . "Build is green") (status . "complete")))
      (should (= (length reads) 1))
      (funcall (pop reads)
               `((control . ,(hermes-transport-json-parse
                              (json-serialize
                               (hermes-test--loop-control
                                "c" 3 `(next_due_at . ,(+ due 600))))))))
      (should (string-prefix-p "Loop 3/5 · every 5m · next "
                               (hermes-test--control-header)))
      ;; The last tick's turn ends the loop.
      (hermes-test--emit-dashboard-event
       client "message.complete" '((text . "Done") (status . "complete")))
      (funcall (pop reads)
               `((control . ,(hermes-transport-json-parse
                              (json-serialize
                               (hermes-test--loop-control "d" 5 '(status . "done")))))))
      (should-not (hermes-test--control-header))
      (should-not hermes-session-control--timer)
      ;; Nothing running: turns no longer reread.
      (hermes-test--emit-dashboard-event
       client "message.complete" '((text . "Hi") (status . "complete")))
      (should-not reads))))

(ert-deftest hermes-control-slash-tick-rereads-once-after-due-time ()
  "A slash-only tick is reconciled by its notice and one timer after due."
  (hermes-test--with-control-wire reads
    (let ((due (+ (float-time) 300)))
      (hermes-test--emit-dashboard-event
       client "session.control.update"
       `((control . ,(hermes-test--loop-control "a" 2 `(next_due_at . ,due)))))
      (let ((timer hermes-session-control--timer))
        (should (timerp timer))
        (should (< 300 (float-time (time-subtract (timer--time timer) nil)) 306))
        ;; A slash tick reports only its output.
        (hermes-test--emit-dashboard-event
         client "status.update" '((kind . "loop") (text . "Recap: 2 PRs open")))
        (funcall (pop reads)
                 `((control . ,(hermes-transport-json-parse
                                (json-serialize
                                 (hermes-test--loop-control
                                  "b" 3 `(next_due_at . ,(+ due 300))))))))
        (should (string-prefix-p "Loop 3/5" (hermes-test--control-header)))
        ;; The new snapshot replaced the timer with one for its due time.
        (should-not (memq timer timer-list))
        (setq timer hermes-session-control--timer)
        (should (< 600 (float-time (time-subtract (timer--time timer) nil)) 606))
        (should-not reads)
        ;; With no notice at all, the timer rereads once.
        (funcall (timer--function timer))
        (should-not hermes-session-control--timer)
        (should (= (length reads) 1))
        (funcall (pop reads)
                 `((control . ,(hermes-transport-json-parse
                                (json-serialize
                                 (hermes-test--loop-control
                                  "c" 4 '(status . "paused")
                                  `(next_due_at . ,(+ due 600))))))))
        (should (string-prefix-p "Loop paused 4/5" (hermes-test--control-header)))
        (should-not hermes-session-control--timer)
        ;; A fired timer never rereads twice.
        (funcall (timer--function timer))
        (should-not reads)))))

(ert-deftest hermes-control-detach-retires-the-tick-timer ()
  "Detaching cancels the reread, and its stale callback stays inert."
  (hermes-test--with-control-wire reads
    (hermes-test--emit-dashboard-event
     client "session.control.update"
     `((control . ,(hermes-test--loop-control
                    "a" 2 `(next_due_at . ,(+ (float-time) 60))))))
    (let ((timer hermes-session-control--timer))
      (should (memq timer timer-list))
      (hermes-chat--forget-live-dashboard-session)
      (should-not hermes-session-control--timer)
      (should-not (memq timer timer-list))
      (funcall (timer--function timer))
      (should-not reads))))

;;;; Presentation

(ert-deftest hermes-control-header-summarizes-running-automation ()
  "The header shows loop and goal budgets, compactly when narrow."
  (hermes-test--with-control-chat
    (should-not (hermes-session-control-header))
    (hermes-session-control-accept
     (list :type 'session-control :session-id "sid"
           :control (hermes-test--control-snapshot "a")))
    (should (equal (substring-no-properties (hermes-session-control-header))
                   "Loop 2/5 · every 5m · Goal 3/20 turns"))
    (should (eq (get-text-property 0 'face (hermes-session-control-header)) 'shadow))
    (should (equal (substring-no-properties (hermes-session-control-header t))
                   "Loop 2/5 · every 5m · Goal 3/20"))
    (hermes-session-control-accept
     (list :type 'session-control :session-id "sid"
           :control (hermes-test--control-snapshot
                     "b" '((goal . ((status . "active") (turns_used . 3) (max_turns . 20)
                                    (wait_barrier . ((type . "until") (until_at . 1)))))
                           (loop . ((status . "active") (mode . "self_paced")
                                    (current_delay . 5400) (ticks_fired . 1)
                                    (deferred_by_goal . t)))))))
    (should (equal (substring-no-properties (hermes-session-control-header))
                   "Loop parked 1 tick · self-paced 1h30m · Goal waiting 3/20 turns"))
    (hermes-session-control-accept
     (list :type 'session-control :session-id "sid"
           :control (hermes-test--control-snapshot
                     "c" '((goal . ((status . "done"))) (loop . :null)))))
    (should-not (hermes-session-control-header))
    (should (string-match-p "Loop 2/5"
                            (progn (hermes-session-control-accept
                                    (list :type 'session-control :session-id "sid"
                                          :control (hermes-test--control-snapshot "a")))
                                   (hermes-chat--header-line 120))))))

(defun hermes-test--face-at (string text)
  "Return the face of the first occurrence of TEXT in STRING."
  (get-text-property (string-search text string) 'face string))

(ert-deftest hermes-control-header-mutes-labels-and-marks-attention ()
  "Labels are muted, values are plain, and paused or parked states stand out."
  (hermes-test--with-control-chat
    (hermes-session-control-accept
     (list :type 'session-control :session-id "sid"
           :control (hermes-test--control-snapshot
                     "a" '((goal . ((status . "paused") (turns_used . 3) (max_turns . 20)))
                           (loop . ((status . "active") (interval_seconds . 300)
                                    (ticks_fired . 1) (deferred_by_goal . t)))))))
    (let ((header (hermes-session-control-header)))
      (should (equal (substring-no-properties header)
                     "Loop parked 1 tick · every 5m · Goal paused 3/20 turns"))
      (should (eq (hermes-test--face-at header "Loop") 'shadow))
      (should (eq (hermes-test--face-at header " · ") 'shadow))
      (should (eq (hermes-test--face-at header "every") 'shadow))
      (should (eq (hermes-test--face-at header "parked") 'warning))
      (should (eq (hermes-test--face-at header "paused") 'warning))
      (should-not (hermes-test--face-at header "5m"))
      (should-not (hermes-test--face-at header "3/20")))))

(ert-deftest hermes-control-view-says-each-state-once ()
  "A waiting goal and a parked loop are explained once, and stand out."
  (hermes-test--with-control-view
    (with-current-buffer hermes-session-control--chat
      (hermes-session-control-accept
       (list :type 'session-control :session-id "sid"
             :control (hermes-test--control-snapshot
                       "b" '((goal . ((title . "Ship it") (status . "active")
                                      (turns_used . 3) (max_turns . 20)
                                      (wait_barrier . ((type . "pid") (target . 7)))))
                             (loop . ((status . "active") (prompt . "Poll")
                                      (interval_seconds . 300) (ticks_fired . 1)
                                      (deferred_by_goal . t))))))))
    (let ((text (buffer-string)))
      (should (= 1 (cl-count-if (lambda (line) (string-match-p "[Ww]ait" line))
                                (split-string text "\n"))))
      (should (= 1 (cl-count-if (lambda (line) (string-match-p "[Pp]ark" line))
                                (split-string text "\n"))))
      (should (memq 'warning (ensure-list (hermes-test--face-at text "Parked")))))
    (with-current-buffer hermes-session-control--chat
      (hermes-session-control-accept
       (list :type 'session-control :session-id "sid"
             :control (hermes-test--control-snapshot
                       "c" '((goal . ((title . "Ship it") (status . "paused")
                                      (turns_used . 3) (max_turns . 20)
                                      (paused_reason . "budget check")))
                             (loop . :null))))))
    (should (equal (seq-filter (lambda (line) (string-match-p "[Pp]aused" line))
                               (split-string (buffer-string) "\n"))
                   '("  Paused 3/20 turns · budget check")))))

(ert-deftest hermes-control-view-shows-only-present-sections ()
  "The view renders real fields and omits absent sections and empty fields."
  (hermes-test--with-control-view
    (let ((text (buffer-string)))
      (dolist (expected '("Goal\n" "Ship the release" "  3/20 turns"
                          "Outcome: A working release" "Criteria\n"
                          "1. Test recovery" "2. Review changes" "Loop\n"
                          "Check the build" "Tick cap: 100"))
        (should (string-match-p (regexp-quote expected) text)))
      (should-not (string-match-p "Heartbeat\\|Verification\\|Until" text)))
    (with-current-buffer hermes-session-control--chat
      (hermes-session-control-accept
       (list :type 'session-control :session-id "sid"
             :control (hermes-transport-control-snapshot
                       '((revision . "") (updated_at . 0)
                         (goal) (loop) (heartbeat))))))
    (should (string-match-p "No goal, loop or heartbeat" (buffer-string)))))

(ert-deftest hermes-control-push-keeps-the-reading-position ()
  "A pushed snapshot keeps point and the window start on their text.
That holds inside unnavigable text, and when a note is added above it."
  (hermes-test--with-control-view
    (let ((window (selected-window)))
      (set-window-buffer window (current-buffer))
      (goto-char (point-min))
      (search-forward "Check the b")
      (set-window-point window (point))
      (set-window-start window (save-excursion (search-backward "Loop") (point)) t)
      (with-current-buffer hermes-session-control--chat
        (hermes-session-control--update '(:request "goal.pause"))
        (hermes-session-control-accept
         (list :type 'session-control :session-id "sid"
               :control (hermes-test--control-snapshot "next"))))
      (should (string-match-p "Applying goal.pause" (buffer-string)))
      (should (looking-at-p "uild"))
      (should (= (window-point window) (point)))
      (should (save-excursion (goto-char (window-start window))
                              (looking-at-p "Loop$"))))))

(ert-deftest hermes-control-navigation-and-refresh-keep-the-item ()
  "n/p walk sections and criteria; a re-render keeps point on its item."
  (hermes-test--with-control-view
    (hermes-session-control-next)
    (should (eq (get-text-property (point) 'hermes-session-control-item) :goal))
    (hermes-session-control-next 3)
    (should (equal (get-text-property (point) 'hermes-session-control-item) '(:subgoal . 2)))
    (hermes-session-control-previous)
    (should (equal (get-text-property (point) 'hermes-session-control-item) '(:subgoal . 1)))
    (with-current-buffer hermes-session-control--chat
      (hermes-session-control-accept
       (list :type 'session-control :session-id "sid"
             :control (hermes-test--control-snapshot
                       "b" '((goal . ((title . "A new title that is quite long")
                                      (status . "active")
                                      (subgoals . ["Test recovery"]))))))))
    (should (equal (get-text-property (point) 'hermes-session-control-item) '(:subgoal . 1)))
    (should-error (hermes-session-control-previous 9) :type 'user-error)))

;;;; Reconnect

(ert-deftest hermes-control-view-refresh-reconnects-a-paused-session ()
  "After a reconnect, g in an open view resumes the session and reads it."
  (hermes-test--with-control-view
    (let ((chat hermes-session-control--chat) resumed reads)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client session-id &rest args)
                   (push session-id resumed)
                   (funcall (plist-get args :resolve)
                            '((session_id . "sid-2") (resumed . "stored-session")))))
                ((symbol-function 'hermes-dashboard-transport-session-control-read)
                 (lambda (_client &rest args)
                   (push (cons (plist-get args :session-id) (plist-get args :resolve))
                         reads))))
        (with-current-buffer chat
          (setq hermes-chat--session-id "stored-session")
          (hermes-session-control-accept
           (list :type 'session-control :session-id "sid"
                 :control (hermes-test--control-snapshot
                           "paused" '((goal . :null)
                                      (loop . ((status . "paused")
                                               (interval_seconds . 300)))))))
          (hermes-chat--handle-reconnecting-status '(:type status :status "reconnecting"))
          (hermes-chat--dashboard-handle-reconnected '(:type status :status "reconnected")))
        ;; A paused loop stays lazy, and the view says why it is empty.
        (should-not resumed)
        (should (string-match-p "Disconnected" (buffer-string)))
        (should-not (hermes-session-control--offers-p "loop.resume"))
        (hermes-session-control-refresh)
        (should (equal resumed '("stored-session")))
        (should (equal (mapcar #'car reads) '("sid-2")))
        (should (string-match-p "Reading session state" (buffer-string)))
        (funcall (cdar reads)
                 `((control . ,(hermes-test--control-wire
                                "paused" '((goal . :null)
                                           (loop . ((status . "paused")
                                                    (interval_seconds . 300))))))))
        (should-not (string-match-p "Disconnected\\|Reading" (buffer-string)))
        (should (hermes-session-control--offers-p "loop.resume"))))))

(ert-deftest hermes-control-reconnect-retries-a-settling-session ()
  "A briefly refused automation resume retries on the same connection.
Retries are bounded, and detaching retires a pending retry."
  (dolist (case '(success teardown exhausted))
    (let ((client (hermes-test--dashboard-client)) (calls 0))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'hermes-dashboard-transport-session-control-read)
                 #'ignore)
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client _session-id &rest args)
                   (cl-incf calls)
                   (if (and (eq case 'success) (= calls 2))
                       (funcall (plist-get args :resolve)
                                '((session_id . "sid-2") (resumed . "stored-session")))
                     (funcall (plist-get args :reject)
                              (propertize "session disconnect interrupt settling"
                                          'hermes-rpc-code 4009))))))
        (hermes-test-with-chat-buffer
          (setq hermes-chat--session-id "stored-session"
                hermes-chat--dashboard-client client
                hermes-session-control--resume-p t)
          (hermes-chat--dashboard-handle-reconnected '(:type status :status "reconnected"))
          (should (= calls 1))
          (should-not (string-match-p "Dashboard session failed" (buffer-string)))
          (let ((timer hermes-chat--automation-resume-timer))
            (should (memq timer timer-list))
            (pcase case
              ('success
               (funcall (timer--function timer))
               (should (= calls 2))
               (should (equal hermes-chat--dashboard-active-session-id "sid-2"))
               (should-not hermes-chat--automation-resume-timer))
              ('teardown
               (hermes-chat--forget-live-dashboard-session)
               (should-not hermes-chat--automation-resume-timer)
               (should-not (memq timer timer-list))
               (funcall (timer--function timer))
               (should (= calls 1)))
              ('exhausted
               (while hermes-chat--automation-resume-timer
                 (funcall (timer--function hermes-chat--automation-resume-timer)))
               (should (= calls 4))
               (should (string-match-p "Dashboard session failed: session disconnect"
                                       (buffer-string)))))))))))

;; Session resume in tests resolves through the captured callbacks.
(defun hermes-test--resume-receipt (running)
  "Return a resume receipt for the stored session with RUNNING."
  `((session_id . "sid") (resumed . "stored-session") (running . ,running)))

(ert-deftest hermes-control-refresh-supersedes-a-pending-resume-retry ()
  "An explicit refresh during a pending automation retry owns the resume.
The retry is retired, and a second refresh cannot start another resume."
  (let ((client (hermes-test--dashboard-client)) calls requests)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-control-read)
               #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest args)
                 (push session-id calls)
                 (if (= (length calls) 1)
                     (funcall (plist-get args :reject)
                              (propertize "settling" 'hermes-rpc-code 4009))
                   (setq requests (append requests (list args)))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--session-id "stored-session"
              hermes-chat--dashboard-client client
              hermes-session-control--resume-p t)
        (hermes-chat--dashboard-handle-reconnected nil)
        (let ((timer hermes-chat--automation-resume-timer))
          (should timer)
          (hermes-session-control-refresh)
          (should (= (length calls) 2))
          (should-not hermes-chat--automation-resume-timer)
          (should-not (memq timer timer-list))
          ;; A retry already queued for delivery stays inert.
          (funcall (timer--function timer))
          (hermes-session-control-refresh)
          (should (= (length calls) 2))
          (funcall (plist-get (car requests) :resolve)
                   (hermes-test--resume-receipt t))
          (should (equal hermes-chat--dashboard-active-session-id "sid"))
          (should hermes-chat--dashboard-running-p)
          (should-not hermes-chat--session-bootstrap))))))

(ert-deftest hermes-control-stale-resume-receipt-cannot-overwrite-successor ()
  "A resume receipt from a retired connection leaves its successor alone."
  (let ((client (hermes-test--dashboard-client)) requests)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-control-read)
               #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq requests (append requests (list args))))))
      (hermes-test-with-chat-buffer
        (setq hermes-chat--session-id "stored-session"
              hermes-chat--dashboard-client client)
        (hermes-session-control-refresh)
        (should (= (length requests) 1))
        ;; The socket was replaced without delivering the old rejection.
        (cl-incf (hermes-dashboard-transport-client-generation client))
        (hermes-session-control-refresh)
        (should (= (length requests) 2))
        (funcall (plist-get (nth 1 requests) :resolve)
                 (hermes-test--resume-receipt t))
        (should hermes-chat--dashboard-running-p)
        (funcall (plist-get (nth 0 requests) :resolve)
                 (hermes-test--resume-receipt nil))
        (should hermes-chat--dashboard-running-p)))))

(ert-deftest hermes-control-reconnect-resumes-only-running-automation ()
  "A reconnect stays lazy unless the detached session was running automation."
  (dolist (case '((nil . nil) (t . "stored-session")))
    (let ((client (hermes-test--dashboard-client)) resumed)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                 (lambda (&rest _) client))
                ((symbol-function 'hermes-dashboard-transport-session-resume)
                 (lambda (_client session-id &rest _args) (setq resumed session-id))))
        (hermes-test-with-chat-buffer
          (setq hermes-chat--session-id "stored-session"
                hermes-chat--dashboard-client client
                hermes-chat--dashboard-active-session-id "sid")
          (when (car case)
            (hermes-session-control-accept
             (list :type 'session-control :session-id "sid"
                   :control (hermes-test--control-snapshot "a"))))
          (let ((callback (hermes-chat--transport-callback
                           (current-buffer) "asst" t
                           (hermes-chat--next-transport-generation))))
            (funcall callback '(:type status :status "reconnecting"))
            (should-not (hermes-session-control-header))
            (funcall callback '(:type status :status "reconnected")))
          (should (equal resumed (cdr case))))))))

(provide 'hermes-session-control-tests)
;;; hermes-session-control-tests.el ends here
