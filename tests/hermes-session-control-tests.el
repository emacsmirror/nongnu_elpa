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
           hermes-chat--dashboard-active-session-id "sid")
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

(provide 'hermes-session-control-tests)
;;; hermes-session-control-tests.el ends here
