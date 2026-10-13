;;; hermes-session-control.el --- Goal, loop and heartbeat controls -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The backend owns each session's goal, loop and heartbeat.  It pushes a
;; `session.control.update' snapshot whenever one changes, and answers
;; `session.control.read' and `session.control' with the same snapshot.
;; This file keeps that snapshot per chat, shows a compact header summary,
;; and offers a session view whose actions are exactly those the released
;; backend accepts for the current state.  There is no optimistic state:
;; an action shows as pending until the backend's snapshot arrives.
;;
;; Revisions are content hashes, so they cannot be ordered.  Pushes arrive
;; in order on the socket; every accepted push or detach bumps an epoch, and
;; a read or action reply applies only if the epoch has not moved since its
;; request began.  A snapshot with the current revision is a no-op.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'keymap-popup)
(require 'hermes-buffer)
(require 'hermes-dashboard-rpc)
(require 'hermes-transport)

(defvar hermes-chat--dashboard-client)
(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--lifecycle-generation)
(declare-function hermes-chat--dashboard-client-live-p "hermes-chat-dashboard")
(declare-function hermes-chat--dashboard-context-current-p "hermes-chat-dashboard")
(declare-function hermes-chat--with-dashboard-session "hermes-chat-dashboard")
(declare-function hermes-chat--dashboard-session-attached-p "hermes-chat-dashboard")
(declare-function hermes-chat--command-run-owned "hermes-chat-slash")
(declare-function hermes-chat--command-context "hermes-chat-slash")
(declare-function hermes-chat--command-context-current-p "hermes-chat-slash")
(declare-function hermes-chat--command-stop "hermes-chat-slash")
(declare-function hermes-chat--handle-command-result "hermes-chat-slash")

;;;; State

(defvar-local hermes-session-control--state nil
  "This chat's control state: (:snapshot S :epoch N :pending A :error E).")

(defvar-local hermes-session-control--resume-p nil
  "Non-nil when the detached session was running automation.
A reconnect resumes such a session instead of leaving it lazy.")

(defvar-local hermes-session-control--reading nil
  "The outstanding snapshot read, (:again FLAG), or nil.
FLAG is non-nil when another read was asked for meanwhile.")

(defvar-local hermes-session-control--view nil
  "The control view showing this chat.")

(defvar-local hermes-session-control--chat nil
  "The chat buffer this control view shows.")

(defun hermes-session-control-reduce (state message)
  "Return STATE after MESSAGE.
MESSAGE is one of (:push SNAPSHOT), (:request ACTION),
\(:reply EPOCH SNAPSHOT ACTION), (:fail ERROR ACTION) or (:forget)."
  (let ((epoch (or (plist-get state :epoch) 0))
        (current (plist-get state :snapshot))
        (pending (plist-get state :pending)))
    (cl-flet ((settle (action) (if (equal action pending) nil pending))
              (keep (snapshot) (if (and current
                                        (equal (plist-get snapshot :revision)
                                               (plist-get current :revision)))
                                   current snapshot)))
      (pcase message
        (`(:push ,snapshot)
         (list :snapshot (keep snapshot) :epoch (1+ epoch) :pending pending))
        (`(:request ,action)
         (list :snapshot current :epoch epoch :pending (or action pending)))
        (`(:reply ,at ,snapshot ,action)
         (list :snapshot (if (eql at epoch) (keep snapshot) current)
               :epoch epoch :pending (settle action)))
        (`(:fail ,error ,action)
         (list :snapshot current :epoch epoch :pending (settle action)
               :error error))
        (`(:forget) (list :epoch (1+ epoch)))
        (_ state)))))

(defun hermes-session-control-snapshot ()
  "Return this chat's current control snapshot."
  (plist-get hermes-session-control--state :snapshot))

(defun hermes-session-control-active-p (snapshot)
  "Return non-nil when SNAPSHOT has a goal, loop or heartbeat running."
  (cl-some (lambda (kind)
             (equal (plist-get (plist-get snapshot kind) :status) "active"))
           '(:goal :loop :heartbeat)))

(defun hermes-session-control--update (message)
  "Reduce MESSAGE into this chat's state and redisplay it."
  (setq hermes-session-control--state
        (hermes-session-control-reduce hermes-session-control--state message))
  (pcase (car message)
    ((or :push :reply) (hermes-session-control--schedule))
    ;; A detached read cannot block the next attachment's.
    (:forget (hermes-session-control--cancel)
             (setq hermes-session-control--reading nil)))
  (force-mode-line-update)
  (let ((chat (current-buffer)))
    (when (buffer-live-p hermes-session-control--view)
      (with-current-buffer hermes-session-control--view
        (when (and (hermes-buffer--owned-p 'hermes-session-control-mode)
                   (eq hermes-session-control--chat chat))
          (hermes-session-control--redisplay))))))

(defun hermes-session-control-accept (event)
  "Accept a pushed `session-control' EVENT for this chat's attached session."
  (when (and (eq (plist-get event :type) 'session-control)
             (equal (plist-get event :session-id)
                    hermes-chat--dashboard-active-session-id))
    (hermes-session-control--update (list :push (plist-get event :control)))))

(defun hermes-session-control-forget ()
  "Drop the detached session's snapshot, remembering whether it ran."
  (when-let* ((snapshot (hermes-session-control-snapshot)))
    (setq hermes-session-control--resume-p
          (hermes-session-control-active-p snapshot)))
  (hermes-session-control--update '(:forget)))

;;;; Requests

(defun hermes-session-control--send (&optional action args context done)
  "Read this chat's snapshot, or apply ACTION with ARGS under command CONTEXT.
Call DONE in the chat once the request settles, whatever its outcome.
The reply applies only to the same attachment and epoch.  An action's
reply releases CONTEXT's command owner and then handles the backend's
`dispatch' exactly as the matching slash command's result: a resumed
goal's continuation is submitted, or queued behind a running turn."
  (let* ((buffer (current-buffer))
         (client hermes-chat--dashboard-client)
         (session hermes-chat--dashboard-active-session-id)
         (generation hermes-chat--lifecycle-generation)
         (owner (plist-get context :owner))
         (epoch (or (plist-get hermes-session-control--state :epoch) 0))
         (settle
          (lambda (message &optional dispatch)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (unwind-protect
                    (let ((current (if context
                                       (hermes-chat--command-context-current-p context)
                                     (hermes-chat--dashboard-context-current-p
                                      client generation session))))
                      (when owner (hermes-chat--command-stop owner))
                      (if current
                          (progn
                            (hermes-session-control--update message)
                            (when (consp dispatch)
                              (hermes-chat--handle-command-result
                               dispatch nil (plist-get owner :input-edited))))
                        ;; A successor attachment owns the snapshot; free the action.
                        (when action
                          (hermes-session-control--update (list :fail nil action)))))
                  (when done (funcall done)))))))
         (resolve
          (lambda (result)
            (funcall settle
                     (if-let* ((snapshot (hermes-transport-control-snapshot
                                          (hermes-transport--get result 'control))))
                         (list :reply epoch snapshot action)
                       (list :fail "invalid control snapshot" action))
                     (and action (hermes-transport--get result 'dispatch)))))
         (reject
          (lambda (err)
            (funcall settle (list :fail (format "%s" err) action)))))
    (hermes-session-control--update (list :request action))
    (condition-case err
        (if action
            (hermes-dashboard-transport-session-control
             client action :args args :session-id session
             :resolve resolve :reject reject)
          (hermes-dashboard-transport-session-control-read
           client :session-id session :resolve resolve :reject reject))
      (error (funcall reject (error-message-string err))))))

(defun hermes-session-control--in-chat (function)
  "Call FUNCTION in this control view's live chat buffer."
  (unless (and (hermes-buffer--owned-p 'hermes-session-control-mode)
               (buffer-live-p hermes-session-control--chat))
    (user-error "The chat for these controls is gone"))
  (with-current-buffer hermes-session-control--chat
    (funcall function)))

(defun hermes-session-control--read ()
  "Read a fresh snapshot when this chat is attached; never attach for it.
Keep at most one read outstanding: asking during one schedules a single
follow-up read of whichever session is then attached."
  (when (and (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
             hermes-chat--dashboard-active-session-id)
    (if hermes-session-control--reading
        (setf (plist-get hermes-session-control--reading :again) t)
      (let ((reading (list :again nil)))
        (setq hermes-session-control--reading reading)
        (hermes-session-control--send
         nil nil nil
         (lambda ()
           (when (eq reading hermes-session-control--reading)
             (setq hermes-session-control--reading nil)
             (when (plist-get reading :again)
               (hermes-session-control--read)))))))))

(defun hermes-session-control-refresh ()
  "Read a fresh control snapshot for this chat or control view.
A detached chat resumes its session, as sending would; attaching reads
the snapshot itself."
  (interactive)
  (if (derived-mode-p 'hermes-session-control-mode)
      (hermes-session-control--in-chat #'hermes-session-control-refresh)
    (let ((attached (hermes-chat--dashboard-session-attached-p)))
      (hermes-chat--with-dashboard-session
       nil (current-buffer)
       (lambda (_client) (when attached (hermes-session-control--read)))))))

;;;; Reconciliation

;; The backend pushes a snapshot after an action, a slash command and a
;; model turn's post-turn hooks, but not when a loop tick fires, when a
;; slash-only tick completes, or while a turn runs.  So the chat rereads
;; at those boundaries: after a turn ends, after a loop, goal, heartbeat
;; or process notice, and once just after the loop's next tick is due.

(defconst hermes-session-control--due-grace 5
  "Seconds after a loop tick is due before rereading the session state.")

(defvar-local hermes-session-control--timer nil
  "The one-shot reread timed after the active loop's next tick, or nil.")

(defun hermes-session-control--cancel ()
  "Cancel this chat's pending tick reread."
  (when hermes-session-control--timer
    (cancel-timer hermes-session-control--timer)
    (setq hermes-session-control--timer nil)))

(defun hermes-session-control--schedule ()
  "Replace the reread with one just after the active loop's next tick.
The timer belongs to this attachment and fires at most once."
  (hermes-session-control--cancel)
  (let* ((loop (plist-get (hermes-session-control-snapshot) :loop))
         (due (plist-get loop :next-due-at))
         (delay (and (equal (plist-get loop :status) "active")
                     (not (plist-get loop :deferred-by-goal))
                     (numberp due)
                     (- due (float-time)))))
    (when (and delay (> delay 0))
      (let* ((buffer (current-buffer))
             (client hermes-chat--dashboard-client)
             (generation hermes-chat--lifecycle-generation)
             (session hermes-chat--dashboard-active-session-id)
             (timer (timer-create)))
        (timer-set-time timer (time-add nil (+ delay hermes-session-control--due-grace)))
        (timer-set-function
         timer
         (lambda ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (eq timer hermes-session-control--timer)
                          (hermes-chat--dashboard-context-current-p
                           client generation session))
                 (setq hermes-session-control--timer nil)
                 (hermes-session-control--read))))))
        (timer-activate timer)
        (setq hermes-session-control--timer timer)))))

(defun hermes-session-control-observe (event)
  "Reread automation state when EVENT may have changed it unannounced.
That is a turn's end or a loop, goal, heartbeat or process notice for
this session, while a goal, loop or heartbeat is running."
  (when (and (hermes-session-control-active-p (hermes-session-control-snapshot))
             (member (plist-get event :session-id)
                     (list nil hermes-chat--dashboard-active-session-id))
             (or (memq (plist-get event :type) '(done error))
                 (and (eq (plist-get event :type) 'status)
                      (member (plist-get event :status)
                              '("loop" "goal" "heartbeat" "process")))))
    (hermes-session-control--read)))

;;;; Actions

(defconst hermes-session-control--destructive
  '(("goal.clear" . "Clear the goal")
    ("loop.stop" . "Stop the loop")
    ("heartbeat.clear" . "Clear the heartbeat")
    ("subgoal.clear" . "Clear all criteria"))
  "Actions that discard state, with their confirmation prompts.")

(defun hermes-session-control-actions (snapshot)
  "Return the released backend actions applicable to SNAPSHOT."
  (let ((goal (plist-get snapshot :goal))
        (loop (plist-get snapshot :loop))
        (heartbeat (plist-get snapshot :heartbeat)))
    (cl-flet ((toggle (prefix section)
                (pcase (plist-get section :status)
                  ("active" (list (concat prefix ".pause")))
                  ("paused" (list (concat prefix ".resume"))))))
      (append
       (when goal
         (append (toggle "goal" goal)
                 (when (plist-get goal :wait-barrier) '("goal.unwait"))
                 (when (member (plist-get goal :status) '("active" "paused"))
                   (cons "subgoal.add"
                         (when (plist-get goal :subgoals)
                           '("subgoal.remove" "subgoal.clear"))))
                 '("goal.clear")))
       (when loop (append (toggle "loop" loop) '("loop.stop")))
       (when heartbeat
         (append (toggle "heartbeat" heartbeat) '("heartbeat.clear")))))))

(defun hermes-session-control--offers-p (action)
  "Return non-nil when this control view can apply ACTION now."
  (and (hermes-buffer--owned-p 'hermes-session-control-mode)
       (buffer-live-p hermes-session-control--chat)
       (with-current-buffer hermes-session-control--chat
         (and (not (plist-get hermes-session-control--state :pending))
              (member action (hermes-session-control-actions
                              (hermes-session-control-snapshot)))))))

(defun hermes-session-control--arguments (action snapshot)
  "Read the arguments ACTION needs, given SNAPSHOT."
  (pcase action
    ("subgoal.add"
     (let ((text (string-trim (read-string "Add criterion: "))))
       (when (string-empty-p text) (user-error "Empty criterion"))
       `((text . ,text))))
    ("subgoal.remove"
     (let ((choices (cl-loop for text in (plist-get (plist-get snapshot :goal) :subgoals)
                             for n from 1
                             collect (cons (format "%d. %s" n text) n))))
       `((index . ,(cdr (assoc (completing-read "Remove criterion: " choices nil t)
                               choices))))))))

(defun hermes-session-control--act (action)
  "Confirm and apply ACTION to this control view's session."
  (unless (hermes-session-control--offers-p action)
    (user-error "Not available now; press g to refresh"))
  (let* ((chat hermes-session-control--chat)
         (state (buffer-local-value 'hermes-session-control--state chat))
         (args (hermes-session-control--arguments
                action (plist-get state :snapshot)))
         (prompt (cdr (assoc action hermes-session-control--destructive))))
    (when (or (not prompt) (y-or-n-p (concat prompt " for this session? ")))
      ;; The prompt may have outlived the state it asked about.
      (unless (and (eq state (buffer-local-value 'hermes-session-control--state chat))
                   (hermes-session-control--offers-p action))
        (user-error "Session state changed; review it and try again"))
      (with-current-buffer chat
        ;; Hold the chat's command owner, as the slash command would, so
        ;; its result cannot race a send and lands only on this attachment.
        (hermes-chat--command-run-owned
         nil (lambda (client owner)
               (hermes-session-control--send
                action args (hermes-chat--command-context client owner))))))))

(defmacro hermes-session-control--define-actions (&rest specs)
  "Define one view command per (NAME ACTION DOC) in SPECS."
  `(progn
     ,@(cl-loop for (name action doc) in specs
                collect `(defun ,name ()
                           ,doc
                           (interactive nil hermes-session-control-mode)
                           (hermes-session-control--act ,action)))))

(hermes-session-control--define-actions
 (hermes-session-control-goal-pause "goal.pause" "Pause the goal.")
 (hermes-session-control-goal-resume "goal.resume" "Resume the goal.")
 (hermes-session-control-goal-unwait "goal.unwait" "Release the goal's wait.")
 (hermes-session-control-goal-clear "goal.clear" "Clear the goal.")
 (hermes-session-control-subgoal-add "subgoal.add" "Add a completion criterion.")
 (hermes-session-control-subgoal-remove "subgoal.remove" "Remove a completion criterion.")
 (hermes-session-control-subgoal-clear "subgoal.clear" "Clear all completion criteria.")
 (hermes-session-control-loop-pause "loop.pause" "Pause the loop.")
 (hermes-session-control-loop-resume "loop.resume" "Resume the loop.")
 (hermes-session-control-loop-stop "loop.stop" "Stop the loop.")
 (hermes-session-control-heartbeat-pause "heartbeat.pause" "Pause the heartbeat.")
 (hermes-session-control-heartbeat-resume "heartbeat.resume" "Resume the heartbeat.")
 (hermes-session-control-heartbeat-clear "heartbeat.clear" "Clear the heartbeat."))

;;;; Summaries

(defun hermes-session-control--duration (seconds)
  "Return SECONDS as a compact duration such as 45s, 5m or 1h30m."
  (let* ((total (round (or seconds 0)))
         (hours (/ total 3600))
         (minutes (/ (% total 3600) 60))
         (rest (% total 60)))
    (cond ((> hours 0) (if (> minutes 0) (format "%dh%dm" hours minutes)
                         (format "%dh" hours)))
          ((> minutes 0) (if (> rest 0) (format "%dm%ds" minutes rest)
                           (format "%dm" minutes)))
          (t (format "%ds" rest)))))

(defun hermes-session-control--clock (value)
  "Return epoch VALUE as a local clock time, or nil when unset."
  (when (and (numberp value) (> value 0))
    (format-time-string "%H:%M" (seconds-to-time value))))

(defun hermes-session-control--words (&rest words)
  "Return the non-nil WORDS joined by spaces."
  (string-join (delq nil words) " "))

(defun hermes-session-control--label (text)
  "Return label TEXT, muted so the values beside it stand out."
  (and text (propertize text 'face 'shadow)))

(defconst hermes-session-control--separator (propertize " · " 'face 'shadow)
  "The muted separator between summary fields.")

(defun hermes-session-control--state (state)
  "Return STATE, marked when it needs the user's eye."
  (and state
       (propertize state 'face (pcase state
                                 ((or "paused" "waiting" "parked") 'warning)
                                 ("done" 'success)
                                 (_ nil)))))

(defun hermes-session-control--goal-summary (goal &optional name compact)
  "Return GOAL's state and turn budget after NAME, without units when COMPACT."
  (hermes-session-control--words
   (hermes-session-control--label name)
   (hermes-session-control--state
    (cond ((plist-get goal :wait-barrier) "waiting")
          ((not (equal (plist-get goal :status) "active")) (plist-get goal :status))))
   (concat (format "%d/%d" (or (plist-get goal :turns-used) 0)
                   (or (plist-get goal :max-turns) 0))
           (if compact "" (hermes-session-control--label " turns")))))

(defun hermes-session-control--loop-summary (loop &optional name compact)
  "Return LOOP's state, ticks and cadence after NAME.
COMPACT omits the next tick."
  (let* ((status (plist-get loop :status))
         (state (cond ((not (equal status "active")) status)
                      ((plist-get loop :deferred-by-goal) "parked")
                      ((plist-get loop :awaiting-response) "waiting")))
         (times (plist-get loop :times))
         (ticks (or (plist-get loop :ticks-fired) 0))
         (next (and (not compact) (not state)
                    (hermes-session-control--clock (plist-get loop :next-due-at)))))
    (concat (hermes-session-control--words
             (hermes-session-control--label name)
             (hermes-session-control--state state)
             (if (and times (> times 0)) (format "%d/%d" ticks times)
               (concat (number-to-string ticks)
                       (hermes-session-control--label
                        (if (= ticks 1) " tick" " ticks")))))
            hermes-session-control--separator
            (if (equal (plist-get loop :mode) "self_paced")
                (concat (hermes-session-control--label "self-paced ")
                        (hermes-session-control--duration (plist-get loop :current-delay)))
              (concat (hermes-session-control--label "every ")
                      (hermes-session-control--duration (plist-get loop :interval-seconds))))
            (if next
                (concat hermes-session-control--separator
                        (hermes-session-control--label "next ") next)
              ""))))

(defun hermes-session-control--heartbeat-summary (heartbeat &optional name)
  "Return HEARTBEAT's state and cadence after NAME."
  (hermes-session-control--words
   (hermes-session-control--label name)
   (unless (equal (plist-get heartbeat :status) "active")
     (hermes-session-control--state (plist-get heartbeat :status)))
   (hermes-session-control--label "every")
   (hermes-session-control--duration (plist-get heartbeat :interval-seconds))))

(defun hermes-session-control-header (&optional compact)
  "Return this chat's goal, loop and heartbeat for the header, or nil.
Finished ones are omitted.  COMPACT drops units, the next tick and the
heartbeat.  Labels are muted; values and states needing attention are not."
  (let* ((snapshot (hermes-session-control-snapshot))
         (goal (plist-get snapshot :goal))
         (loop (plist-get snapshot :loop))
         (heartbeat (plist-get snapshot :heartbeat))
         (parts (delq nil
                      (list (when (member (plist-get loop :status) '("active" "paused"))
                              (hermes-session-control--loop-summary loop "Loop" compact))
                            (when (member (plist-get goal :status) '("active" "paused"))
                              (hermes-session-control--goal-summary goal "Goal" compact))
                            (when (and heartbeat (not compact))
                              (hermes-session-control--heartbeat-summary
                               heartbeat "Heartbeat"))))))
    (when parts (string-join parts hermes-session-control--separator))))

;;;; View

(defun hermes-session-control--insert-heading (title item)
  "Insert section TITLE, a navigation target identified by ITEM."
  (insert "\n" (propertize title 'face 'bold 'hermes-session-control-item item) "\n"))

(defun hermes-session-control--insert-line (text &optional face item)
  "Insert indented, filled TEXT with FACE, a navigation target if ITEM."
  (let ((start (point)))
    (insert "  " text "\n")
    (let ((fill-prefix "    ")) (fill-region start (point)))
    (when face (add-face-text-property start (point) face t))
    (when item
      (put-text-property (+ start 2) (1- (point)) 'hermes-session-control-item item))))

(defun hermes-session-control--sentence (text)
  "Return TEXT with its first letter upcased, keeping its faces."
  (if (string-empty-p text) text
    (let ((text (copy-sequence text)))
      (aset text 0 (upcase (aref text 0)))
      text)))

(defun hermes-session-control--status-line (summary section)
  "Return SUMMARY as a sentence, followed by SECTION's reason for pausing."
  (concat (hermes-session-control--sentence summary)
          (if-let* ((reason (plist-get section :paused-reason)))
              (concat hermes-session-control--separator reason)
            "")))

(defun hermes-session-control--insert-fields (value fields)
  "Insert VALUE's FIELDS that are set, each (KEY . LABEL), as secondary lines."
  (pcase-dolist (`(,key . ,label) fields)
    (when-let* ((field (plist-get value key)))
      (hermes-session-control--insert-line (format "%s: %s" label field) 'shadow))))

(defun hermes-session-control--wait-text (wait)
  "Return a description of goal barrier WAIT."
  (concat "Waiting for "
          (pcase (plist-get wait :type)
            ("until" (or (hermes-session-control--clock (plist-get wait :until-at))
                         "a time"))
            ("pid" (format "process %s" (plist-get wait :target)))
            (_ (format "session %s" (plist-get wait :target))))
          (if-let* ((reason (plist-get wait :reason))) (concat " · " reason) "")))

(defun hermes-session-control--insert-goal (goal)
  "Insert the GOAL section."
  (hermes-session-control--insert-heading "Goal" :goal)
  (hermes-session-control--insert-line (or (plist-get goal :title) ""))
  ;; The wait line below says it is waiting, and for what.
  (hermes-session-control--insert-line
   (hermes-session-control--status-line
    (hermes-session-control--goal-summary (plist-put (copy-sequence goal)
                                                     :wait-barrier nil))
    goal)
   'shadow)
  (when-let* ((wait (plist-get goal :wait-barrier)))
    (hermes-session-control--insert-line
     (hermes-session-control--wait-text wait) 'warning))
  (hermes-session-control--insert-fields
   goal '((:last-verdict . "Verdict")
          (:last-reason . "Reason")))
  (hermes-session-control--insert-fields
   (plist-get goal :contract)
   '((:outcome . "Outcome") (:verification . "Verification")
     (:constraints . "Constraints") (:boundaries . "Boundaries")
     (:stop-when . "Stop when")))
  (dolist (gate (plist-get goal :gates))
    (hermes-session-control--insert-line
     (format "Gate: %s · %d/%d tries%s" (plist-get gate :command)
             (or (plist-get gate :attempts) 0)
             (1+ (or (plist-get gate :max-retries) 0))
             (if-let* ((code (plist-get gate :last-exit-code)))
                 (format " · exit %d" code)
               ""))
     'shadow))
  (when-let* ((subgoals (plist-get goal :subgoals)))
    (hermes-session-control--insert-heading "Criteria" :subgoals)
    (cl-loop for text in subgoals for n from 1
             do (hermes-session-control--insert-line
                 (format "%d. %s" n text) nil (cons :subgoal n)))))

(defun hermes-session-control--insert-loop (loop)
  "Insert the LOOP section."
  (hermes-session-control--insert-heading "Loop" :loop)
  (hermes-session-control--insert-line (or (plist-get loop :prompt) ""))
  (hermes-session-control--insert-line
   (hermes-session-control--status-line
    (hermes-session-control--loop-summary loop) loop)
   'shadow)
  (hermes-session-control--insert-fields
   loop '((:until . "Until") (:max-ticks . "Tick cap")
          (:last-stop-reason . "Stopped")))
  (when-let* ((last (hermes-session-control--clock (plist-get loop :last-fired-at))))
    (hermes-session-control--insert-line (concat "Last tick: " last) 'shadow)))

(defun hermes-session-control--insert-heartbeat (heartbeat)
  "Insert the HEARTBEAT section."
  (hermes-session-control--insert-heading "Heartbeat" :heartbeat)
  (hermes-session-control--insert-line (or (plist-get heartbeat :prompt) ""))
  (hermes-session-control--insert-line
   (format "%s · %d fired"
           (hermes-session-control--sentence
            (hermes-session-control--heartbeat-summary heartbeat))
           (or (plist-get heartbeat :fire-count) 0))
   'shadow))

(defun hermes-session-control-render (state &optional detached)
  "Insert the control view for chat STATE at point.
DETACHED means the chat has no live session to read."
  (let ((snapshot (plist-get state :snapshot)))
    (when-let* ((error (plist-get state :error)))
      (hermes-session-control--insert-line (concat "Failed: " error) 'error))
    (when-let* ((pending (plist-get state :pending)))
      (hermes-session-control--insert-line (format "Applying %s…" pending) 'shadow))
    (cond
     ((and (null snapshot) detached)
      (hermes-session-control--insert-line
       "Disconnected · press g to reconnect" 'warning))
     ((null snapshot)
      (hermes-session-control--insert-line "Reading session state…" 'shadow))
     ((not (cl-some (lambda (kind) (plist-get snapshot kind)) '(:goal :loop :heartbeat)))
      (hermes-session-control--insert-line
       "No goal, loop or heartbeat.  Start one with /goal or /loop." 'shadow))
     (t
      (when-let* ((goal (plist-get snapshot :goal)))
        (hermes-session-control--insert-goal goal))
      (when-let* ((loop (plist-get snapshot :loop)))
        (hermes-session-control--insert-loop loop))
      (when-let* ((heartbeat (plist-get snapshot :heartbeat)))
        (hermes-session-control--insert-heartbeat heartbeat))))))

(defun hermes-session-control--item-starts ()
  "Return (POSITION . ITEM) for each navigation target, in buffer order."
  (let ((pos (point-min)) starts)
    (while (< pos (point-max))
      (when-let* ((item (get-text-property pos 'hermes-session-control-item)))
        (push (cons pos item) starts))
      (setq pos (next-single-property-change pos 'hermes-session-control-item
                                             nil (point-max))))
    (nreverse starts)))

(defun hermes-session-control--item-position (item)
  "Return the start of navigation target ITEM, or nil."
  (car (rassoc item (hermes-session-control--item-starts))))

(defun hermes-session-control--anchor (position)
  "Return POSITION as (ITEM LINES COLUMN) relative to the item before it.
ITEM is the last navigation target starting at or before POSITION, or
nil for the top; LINES and COLUMN place POSITION from that start.  The
anchor survives a repaint that changes other sections or adds notes."
  (let ((start (car (last (seq-filter (lambda (entry) (<= (car entry) position))
                                      (hermes-session-control--item-starts))))))
    (save-excursion
      (goto-char position)
      (list (cdr start)
            (- (line-number-at-pos) (line-number-at-pos (or (car start) (point-min))))
            (current-column)))))

(defun hermes-session-control--place (anchor)
  "Return the position of ANCHOR from `hermes-session-control--anchor'.
An item that is gone places it at the top."
  (pcase-let ((`(,item ,lines ,column) anchor))
    (save-excursion
      (goto-char (or (and item (hermes-session-control--item-position item))
                     (point-min)))
      (forward-line lines)
      (move-to-column column)
      (point))))

(defun hermes-session-control--redisplay ()
  "Re-render this control view, keeping point and each window on its text."
  (let* ((inhibit-read-only t)
         (chat hermes-session-control--chat)
         (point (hermes-session-control--anchor (point)))
         (windows (mapcar (lambda (window)
                            (list window
                                  (hermes-session-control--anchor (window-start window))
                                  (hermes-session-control--anchor (window-point window))))
                          (get-buffer-window-list nil nil t))))
    (erase-buffer)
    (setq header-line-format
          (concat " " (propertize (hermes-buffer--mode-line-literal
                                   (if (buffer-live-p chat) (buffer-name chat)
                                     "closed chat"))
                                  'face 'bold)
                  (propertize "   ? actions  g refresh  n/p move" 'face 'shadow)))
    (when (buffer-live-p chat)
      (hermes-session-control-render
       (buffer-local-value 'hermes-session-control--state chat)
       (not (with-current-buffer chat (hermes-chat--dashboard-session-attached-p)))))
    (goto-char (hermes-session-control--place point))
    (pcase-dolist (`(,window ,start ,place) windows)
      (set-window-start window (save-excursion
                                 (goto-char (hermes-session-control--place start))
                                 (line-beginning-position))
                        t)
      (set-window-point window (hermes-session-control--place place)))))

(defun hermes-session-control-next (&optional n)
  "Move to the Nth next section or criterion; backward when N is negative."
  (interactive "p" hermes-session-control-mode)
  (let* ((n (or n 1))
         (starts (mapcar #'car (hermes-session-control--item-starts)))
         (here (if (get-text-property (point) 'hermes-session-control-item)
                   (or (previous-single-property-change
                        (1+ (point)) 'hermes-session-control-item)
                       (point-min))
                 (point)))
         (candidates (if (> n 0)
                         (seq-filter (lambda (pos) (> pos here)) starts)
                       (reverse (seq-filter (lambda (pos) (< pos here)) starts))))
         (target (nth (1- (abs n)) candidates)))
    (unless target
      (user-error (if (> n 0) "No next item" "No previous item")))
    (goto-char target)))

(defun hermes-session-control-previous (&optional n)
  "Move to the Nth previous section or criterion."
  (interactive "p" hermes-session-control-mode)
  (hermes-session-control-next (- (or n 1))))

(keymap-popup-define hermes-session-control-actions-map
  "Act on the session's goal, loop and heartbeat."
  :description "Session controls"
  :popup-key "?"
  :exit-key "q"
  :group "Goal"
  "p" ("Pause" hermes-session-control-goal-pause
       :if (lambda () (hermes-session-control--offers-p "goal.pause")))
  "r" ("Resume" hermes-session-control-goal-resume
       :if (lambda () (hermes-session-control--offers-p "goal.resume")))
  "w" ("Release wait" hermes-session-control-goal-unwait
       :if (lambda () (hermes-session-control--offers-p "goal.unwait")))
  "a" ("Add criterion" hermes-session-control-subgoal-add
       :if (lambda () (hermes-session-control--offers-p "subgoal.add")))
  "d" ("Remove criterion" hermes-session-control-subgoal-remove
       :if (lambda () (hermes-session-control--offers-p "subgoal.remove")))
  "D" ("Clear criteria" hermes-session-control-subgoal-clear
       :if (lambda () (hermes-session-control--offers-p "subgoal.clear")))
  "C" ("Clear goal" hermes-session-control-goal-clear
       :if (lambda () (hermes-session-control--offers-p "goal.clear")))
  :group "Loop"
  "l" ("Pause" hermes-session-control-loop-pause
       :if (lambda () (hermes-session-control--offers-p "loop.pause")))
  "L" ("Resume" hermes-session-control-loop-resume
       :if (lambda () (hermes-session-control--offers-p "loop.resume")))
  "s" ("Stop" hermes-session-control-loop-stop
       :if (lambda () (hermes-session-control--offers-p "loop.stop")))
  :group "Heartbeat"
  "h" ("Pause" hermes-session-control-heartbeat-pause
       :if (lambda () (hermes-session-control--offers-p "heartbeat.pause")))
  "H" ("Resume" hermes-session-control-heartbeat-resume
       :if (lambda () (hermes-session-control--offers-p "heartbeat.resume")))
  "x" ("Clear" hermes-session-control-heartbeat-clear
       :if (lambda () (hermes-session-control--offers-p "heartbeat.clear"))))

(defvar-keymap hermes-session-control-mode-map
  :parent special-mode-map
  "n" #'hermes-session-control-next
  "p" #'hermes-session-control-previous
  "g" #'hermes-session-control-refresh
  "?" #'hermes-session-control-actions-map-popup)

(define-derived-mode hermes-session-control-mode special-mode "Hermes Controls"
  "View and control one chat session's goal, loop and heartbeat.
\\{hermes-session-control-mode-map}"
  (setq-local truncate-lines nil
              fill-column 72))

;;;###autoload
(defun hermes-session-control ()
  "Show the goal, loop and heartbeat of this chat's session."
  (interactive nil hermes-chat-mode)
  (let ((chat (current-buffer)))
    (hermes-chat--with-dashboard-session
     nil chat
     (lambda (_client)
       (let ((view (hermes-buffer--get (format "*Hermes Controls: %s*" (buffer-name chat))
                                       #'hermes-session-control-mode)))
         (with-current-buffer chat
           (setq hermes-session-control--view view))
         (with-current-buffer view
           (setq hermes-session-control--chat chat)
           (hermes-session-control--redisplay))
         (pop-to-buffer view)
         (with-current-buffer chat
           (hermes-session-control--read)))))))

(provide 'hermes-session-control)
;;; hermes-session-control.el ends here
