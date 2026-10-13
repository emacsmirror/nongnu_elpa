;;; hermes-chat-dashboard-tests.el --- shared-socket isolation tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-dashboard' shared-socket runtime isolation: two
;; chat buffers on one dashboard client must keep their session identity,
;; subscriber tokens, create-time overrides, and reconnect resumes strictly
;; buffer-local.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'hermes-test-helpers)

(require 'hermes-subagents)

(ert-deftest hermes-chat-reasoning-cleanup-rejects-reentrant-old-event ()
  (hermes-test-with-chat-buffer
    (hermes-chat--insert-entry '(:id "a1" :role assistant :content "" :status streaming))
    (setq hermes-chat--pending-assistant-id "a1")
    (let ((callback (hermes-chat--transport-callback (current-buffer) "a1" nil hermes-chat--transport-generation))
          called)
      (funcall callback '(:type thinking :event "thinking.delta" :content "first"))
      (let ((hermes-chat-state-change-hook
             (list (lambda ()
                     (unless called
                       (setq called t)
                       (funcall callback '(:type thinking :event "thinking.delta" :content "late")))))))
        (hermes-chat--cleanup-buffer))
      (should called)
      (should-not hermes-chat--pending-assistant-id)
      (should-not (gethash "a1:activity" hermes-chat--nodes)))))

;;; Observed work

(defvar hermes-chat-work-indicator)

(defvar hermes-test--work-processes-p nil
  "Non-nil to read both roster kinds rather than workers alone.")

(defmacro hermes-test--with-process-wire (&rest body)
  "Run BODY through both real typed roster wrappers and raw frames."
  (declare (indent 0) (debug t))
  `(let ((hermes-test--work-processes-p t))
     (hermes-test--with-work-wire ,@body)))

(defun hermes-test--work-request-id (owner &optional kind)
  "Return the pending request ID of OWNER's KIND read, default the first."
  (let* ((inflight (plist-get owner :inflight))
         (kind (or kind (seq-find (lambda (k) (alist-get k inflight))
                                  '(:delegates :processes))))
         (context (alist-get kind inflight))
         found)
    (when context
      (maphash (lambda (id entry)
                 (when (eq (plist-get entry :owner) context) (setq found id)))
               (hermes-dashboard-transport-client-pending
                (plist-get owner :client))))
    found))

(defun hermes-test--work-reply (client id json)
  "Deliver JSON result for request ID through CLIENT's raw frame handler."
  (hermes-dashboard-transport--handle-frame
   client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"result\":%s}" id json)))

(defun hermes-test--work-answer (client json)
  "Answer the current chat's in-flight read matching JSON's roster kind."
  (hermes-test--work-reply
   client (hermes-test--work-request-id
           hermes-chat--work-owner
           (if (string-match-p "processes" json) :processes :delegates))
   json))

(defun hermes-test--work-error (client id code)
  "Deliver JSON-RPC error CODE for request ID through CLIENT."
  (hermes-dashboard-transport--handle-frame
   client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":%s,\"message\":\"private error\"}}"
                  id code)))

(defun hermes-test--work-text (label)
  "Return LABEL without properties, or nil."
  (and label (substring-no-properties label)))

(defmacro hermes-test--with-work (&rest body)
  "Run BODY with a visible, bound chat and captured worker reads."
  (declare (indent 0) (debug t))
  `(let ((client (hermes-test--dashboard-client))
         (hermes-chat--work-kinds (if hermes-test--work-processes-p
                                      '(:delegates :processes)
                                    '(:delegates)))
         (hermes-chat-work-indicator nil)
         calls timers)
     (setf (hermes-dashboard-transport-client-ready-p client) t)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-subagent-list)
                (lambda (_client &rest args)
                  (push args calls)
                  (should (= hermes-dashboard-transport-request-timeout 10))
                  (should hermes-dashboard-transport-request-lossless-result)
                  (length calls)))
               ((symbol-function 'run-at-time)
                (lambda (delay _repeat fn &rest args)
                  (let ((timer (list delay fn args)))
                    (push timer timers) timer)))
               ((symbol-function 'cancel-timer) #'ignore))
       (save-window-excursion
         (hermes-test-with-chat-buffer
          (switch-to-buffer (current-buffer))
          (setq hermes-chat--dashboard-client client
                hermes-chat--dashboard-active-session-id "runtime"
                hermes-chat--dashboard-session-ready-p t)
          (hermes-chat--work-bind client "runtime")
          ,@body)))))

(defmacro hermes-test--with-work-wire (&rest body)
  "Run BODY with real typed requests/raw responses and fake socket/timers."
  (declare (indent 0) (debug t))
  `(let ((rpc (symbol-function 'hermes-dashboard-transport-subagent-list))
         (hermes-dashboard-transport-request-timeout nil)
         frames events)
     (hermes-test--with-work
       (let ((hermes-dashboard-transport-websocket-send-function
              (lambda (_socket text) (push text frames))))
         (setf (hermes-dashboard-transport-client-callback client)
               (lambda (event) (push event events)))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-subagent-list) rpc))
           ,@body)))))

(defun hermes-test--work-set (agents processes)
  "Give the current chat snapshots of AGENTS and PROCESSES rows."
  (let ((owner hermes-chat--work-owner))
    (setf (plist-get owner :delegates) (list :sid "runtime" :rows agents)
          (plist-get owner :processes) (list :sid "runtime" :rows processes))))

;;;; Pure roster core

(ert-deftest hermes-chat-work-label-is-a-pure-count ()
  "Counts join with a slash, fall back without emoji, and never show `?'."
  (dolist (displayable '(t nil))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) displayable)))
      (should-not (hermes-chat--work-label-text 0 0 nil))
      (should-not (hermes-chat--work-label-text 0 0 t))
      (should (equal (hermes-test--work-text (hermes-chat--work-label-text 2 1 nil))
                     (if displayable "🤖 2 / ⚙ 1" "Agents 2 / 1 process")))
      (should (equal (hermes-test--work-text (hermes-chat--work-label-text 0 2 nil))
                     (if displayable "⚙ 2" "2 processes")))
      (should (equal (hermes-test--work-text (hermes-chat--work-label-text 2 1 t)) "2a / 1p"))
      (should (equal (hermes-test--work-text (hermes-chat--work-label-text 3 0 t)) "3a"))
      ;; Only the numbers are faced; emoji and the separator stay plain.
      (let ((label (hermes-chat--work-label-text 2 1 nil)))
        (dotimes (i (length label))
          (should (eq (get-text-property i 'face label)
                      (and (<= ?0 (aref label i) ?9) 'hermes-work-running))))))))

(ert-deftest hermes-chat-work-events-win-over-snapshots ()
  "Event status overrides an older snapshot; event-only workers are listed."
  (let* ((now 1000.0)
         (rows '((:key (delegate . "a") :id "a" :kind delegate :state running)
                 (:key (delegate . "b") :id "b" :kind delegate :state running)))
         (events (seq-reduce (lambda (events event)
                               (hermes-chat--work-apply-event events event now))
                             '((:event "subagent.complete" :subagent-id "a" :status "failed")
                               (:event "subagent.spawn_requested" :subagent-id "c"
                                       :content "New goal")
                               (:event "subagent.start" :subagent-id "c")
                               (:event "subagent.progress" :subagent-id "ghost")
                               (:event "message.delta" :subagent-id "b"))
                             nil))
         (merged (hermes-chat--work-merge-agents events rows now)))
    (should (equal (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                           merged)
                   '(("a" failed) ("b" running) ("c" running))))
    ;; Unchanged rows keep identity; the event-only row carries its goal.
    (should (eq (nth 1 merged) (nth 1 rows)))
    (should (equal (plist-get (nth 2 merged) :goal) "New goal"))
    (should (= (car (hermes-chat--work-counts (list :agents merged))) 2))
    ;; A finished worker stays finished, and queueing never demotes a snapshot.
    (should (eq (plist-get (cdr (assoc "a" (hermes-chat--work-apply-event
                                           events '(:event "subagent.start"
                                                           :subagent-id "a")
                                           now)))
                           :state)
                'failed))
    (should (eq (plist-get (car (hermes-chat--work-merge-agents
                                 '(("b" :state queued)) rows now))
                           :state)
                'running))))

(ert-deftest hermes-chat-work-finished-workers-are-retained ()
  "A worker leaving the roster is done for sixty seconds; its event status wins."
  (let* ((now 1000.0)
         (previous '((:key (delegate . "a") :id "a" :kind delegate :state running
                           :goal "A goal")
                     (:key (delegate . "b") :id "b" :kind delegate :state running)
                     (:key (delegate . "c") :id "c" :kind delegate :state running)))
         (events (hermes-chat--work-apply-event
                  nil '(:event "subagent.complete" :subagent-id "b" :status "error")
                  (- now 10)))
         (retired (hermes-chat--work-retire events previous (last previous) now now))
         (state (lambda (at)
                  (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                          (hermes-chat--work-merge-agents retired (last previous) at)))))
    (should (equal (funcall state now) '(("c" running) ("a" completed) ("b" error))))
    (should (equal (plist-get (car (hermes-chat--work-merge-agents retired nil now)) :goal)
                   "A goal"))
    (should (equal (funcall state (+ now 55)) '(("c" running) ("a" completed))))
    (should (equal (funcall state (+ now 61)) '(("c" running))))
    (should-not (hermes-chat--work-retire retired (last previous) (last previous)
                                          (+ now 61) (+ now 61)))
    (should (= 1 (car (hermes-chat--work-counts
                       (list :agents (hermes-chat--work-merge-agents retired (last previous) now))))))))

(ert-deftest hermes-chat-work-read-older-than-activity-never-ends-a-worker ()
  "Any event after a read was sent keeps its worker running, in both orders.
Start at 100, read sent at 110, thinking at 120, empty reply at 130: the
thinking proves the reply stale, whether or not an earlier read listed the
worker.  Without newer activity the reply finishes it, keeping its tools."
  (let* ((thinking '(:event "subagent.thinking" :subagent-id "w"))
         (tool '(:event "subagent.tool" :subagent-id "w" :name "read_file"
                        :preview "a.el" :tool-count 3))
         (start (hermes-chat--work-apply-event
                 nil '(:event "subagent.start" :subagent-id "w" :content "Goal") 100.0))
         (listed '((:key (delegate . "w") :id "w" :kind delegate :state running
                         :goal "Goal")))
         (state (lambda (events)
                  (plist-get (car (hermes-chat--work-merge-agents events nil 130.0))
                             :state))))
    (dolist (previous (list nil listed))
      ;; Activity after the read was sent: the reply is stale.
      (let* ((events (hermes-chat--work-apply-event
                      (hermes-chat--work-apply-event start tool 105.0)
                      thinking 120.0))
             (retired (hermes-chat--work-retire events previous nil 110.0 130.0)))
        (should (= (plist-get (cdr (assoc "w" events)) :seen) 120.0))
        (should (eq (funcall state retired) 'running))
        ;; A later read sent after that activity does finish it.
        (should (eq (funcall state (hermes-chat--work-retire retired nil nil 125.0 140.0))
                    'completed)))
      ;; Activity before the read was sent: the reply ends the worker but
      ;; keeps what events taught about it.
      (let* ((events (hermes-chat--work-apply-event
                      (hermes-chat--work-apply-event start tool 105.0)
                      thinking 108.0))
             (row (car (hermes-chat--work-merge-agents
                        (hermes-chat--work-retire events previous nil 110.0 130.0)
                        nil 130.0))))
        (should (eq (plist-get row :state) 'completed))
        (should (= (plist-get row :finished) 130.0))
        (should (= (plist-get row :tool-count) 3))
        (should (equal (plist-get row :tools) '(("read_file" . "a.el"))))
        (should (= (plist-get row :started) 100.0))))))

(ert-deftest hermes-chat-work-read-retires-silent-event-only-workers ()
  "A complete read sent after a worker's last event finishes it if absent.
Workers whose events are newer than the read keep running."
  (let* ((events (seq-reduce (lambda (events event)
                               (hermes-chat--work-apply-event
                                events (cdr event) (car event)))
                             '((100.0 :event "subagent.start" :subagent-id "old")
                               (200.0 :event "subagent.start" :subagent-id "new"))
                             nil))
         (retired (hermes-chat--work-retire events nil nil 150.0 210.0))
         (states (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                         (hermes-chat--work-merge-agents retired nil 210.0))))
    (should (equal (sort states (lambda (a b) (string< (car a) (car b))))
                   '(("new" running) ("old" completed))))
    (should (= (plist-get (cdr (assoc "old" retired)) :finished) 210.0))
    ;; A worker the read lists is not touched.
    (should (equal (hermes-chat--work-retire
                    events nil '((:id "old") (:id "new")) 300.0 310.0)
                   events))))

(ert-deftest hermes-chat-work-wire-read-reconciles-event-only-workers ()
  "The roster read's send time decides which event-only workers it can end."
  (let ((clock 1000.0))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _) (setq clock (+ clock 1.0)))))
      (hermes-test--with-work-wire
        (hermes-chat--work-note-event '(:event "subagent.start" :subagent-id "missed"))
        (hermes-chat-work-refresh)
        (hermes-chat--work-note-event '(:event "subagent.start" :subagent-id "fresh"))
        (hermes-test--work-answer client "{\"subagents\":[]}")
        (should (equal (sort (mapcar (lambda (row)
                                       (list (plist-get row :id) (plist-get row :state)))
                                     (plist-get (hermes-chat--work-roster
                                                 hermes-chat--work-owner clock)
                                                :agents))
                             (lambda (a b) (string< (car a) (car b))))
                       '(("fresh" running) ("missed" completed))))))))

(ert-deftest hermes-chat-work-tick-publishes-expiry-without-reads ()
  "The poll tick republishes and prunes expired work when reads cannot run."
  (let ((clock 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock)))
      (hermes-test--with-work
        (let ((owner hermes-chat--work-owner)
              (changes 0))
          (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
                     (lambda () (cl-incf changes))))
            (push :delegates (plist-get owner :unsupported))
            (hermes-test--work-set
             nil '((:key (process . "p") :id "p" :kind process :state done :exited-at 990.0)))
            (hermes-chat--work-note-event '(:event "subagent.start" :subagent-id "a"))
            (hermes-chat--work-note-event '(:event "subagent.complete" :subagent-id "a"))
            (should (= changes 2))
            (should (assoc "a" (plist-get owner :events)))
            ;; Nothing expired yet: no read, no publication.
            (setq clock 1030.0)
            (hermes-chat--work-tick owner)
            (should (= changes 2))
            (setq clock 1061.0)
            (hermes-chat--work-tick owner)
            (should (= changes 3))
            (should-not (plist-get owner :events))
            (should-not (plist-get (hermes-chat--work-roster owner clock) :agents))
            (should-not (plist-get (hermes-chat--work-roster owner clock) :processes))
            (should-not calls)
            (hermes-chat--work-tick owner)
            (should (= changes 3))))))))

(ert-deftest hermes-chat-work-tree-order ()
  "Parents precede their children depth first; orphans are roots."
  (let ((rows '((:id "c1" :parent "p") (:id "o" :parent "missing") (:id "p")
                (:id "g" :parent "c1") (:id "c2" :parent "p"))))
    (should (equal (mapcar (lambda (row) (plist-get row :id)) (hermes-chat--work-tree rows))
                   '("o" "p" "c1" "g" "c2")))))

(ert-deftest hermes-chat-work-process-retention-window ()
  "Running processes lead, longest first; exits stay sixty seconds."
  (let* ((now 1000.0)
         (rows (list '(:id "short" :state running :uptime 5)
                     '(:id "old" :state done :exited-at 900.0)
                     '(:id "recent" :state failed :exited-at 990.0)
                     '(:id "undated" :state killed)
                     '(:id "long" :state running :uptime 50)
                     '(:id "edge" :state done :exited-at 940.0))))
    (should (equal (mapcar (lambda (row) (plist-get row :id))
                           (hermes-chat--work-process-rows rows now))
                   '("long" "short" "recent" "edge")))
    (should (equal (mapcar (lambda (row) (plist-get row :id))
                           (hermes-chat--work-process-rows rows (+ now 31)))
                   '("long" "short" "recent")))))

(ert-deftest hermes-chat-work-process-notice-refreshes ()
  "A background process notice reads the rosters immediately."
  (hermes-test--with-work
    (let ((event (car (hermes-dashboard-transport--normalize-event-frame
                       '((method . "event")
                         (params . ((type . "status.update") (session_id . "runtime")
                                    (payload . ((kind . "process")
                                                (text . "Process finished"))))))))))
      (should (hermes-chat--work-process-notice-p event))
      (hermes-chat--work-note-event event)
      (should (= (length calls) 1))
      (hermes-chat--work-note-event '(:event "status.update" :status "thinking"))
      (should (= (length calls) 1)))))

(ert-deftest hermes-chat-work-snapshot-is-session-scoped ()
  "A snapshot read for another session contributes nothing."
  (should-not (hermes-chat--work-snapshot-rows '(:sid "old" :rows ((:id "x"))) "new"))
  (should (equal (hermes-chat--work-snapshot-rows '(:sid "new" :rows ((:id "x"))) "new")
                 '((:id "x")))))

;;;; Owner lifecycle

;; The owner shape built by release 0.9.0, which older live sessions keep
;; across an ordered reload.
(defun hermes-test--legacy-work-owner (client timer request)
  "Return a 0.9.0-shaped work owner on CLIENT with TIMER and REQUEST."
  (list :buffer (current-buffer) :generation hermes-chat--work-generation
        :lifetime hermes-chat--lifecycle-generation
        :instance (copy-tree hermes-instance) :profile hermes-chat--profile
        :client client :connection (hermes-dashboard-transport-client-generation client)
        :runtime "runtime" :key nil :request request :timer (cons nil timer)
        :cycle nil :adoption nil
        :delegates '(:coverage stale :rows nil) :processes nil :controls nil
        :visible t :view nil :view-valid-p nil :render nil :refresh nil
        :settle (lambda (_) (error "Legacy settle must not run"))
        :current-p #'hermes-chat--work-current-p))

(ert-deftest hermes-chat-work-reload-replaces-legacy-owner ()
  "Ordered reload retires an older release's owner and binds a fresh one.
The old timer and request are released without running old closures, the
session is not restarted, and the new owner polls with the new signature."
  (hermes-test--with-work
    (let* ((legacy-timer (timer-create))
           (request (list :id nil :cycle (list nil) :source :delegates))
           (pending (hermes-dashboard-transport-client-pending client))
           cancelled)
      (timer-set-function legacy-timer (lambda () (error "Legacy timer must not run")))
      (puthash 99 (list :owner request :method "subagent.list") pending)
      (setq hermes-chat--work-owner
            (hermes-test--legacy-work-owner client legacy-timer request))
      (should (hermes-chat--work-legacy-p hermes-chat--work-owner))
      (setq calls nil)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'hermes-chat--dashboard-start)
                 (lambda (&rest _) (error "Reload must not restart the session"))))
        (hermes-chat--work-activate))
      (let ((owner hermes-chat--work-owner))
        (should owner)
        (should-not (hermes-chat--work-legacy-p owner))
        (should (memq legacy-timer cancelled))
        (should-not (gethash 99 pending))
        (should (eq (plist-get owner :client) client))
        (should (equal (plist-get owner :runtime) "runtime"))
        (should (hermes-chat--work-current-p owner))
        (should (equal (hermes-chat--work-counts (hermes-chat--work-current-roster))
                       '(0 . 0)))
        ;; The fresh owner reads through the current two-argument request.
        (hermes-chat-work-refresh)
        (should (= (length calls) 1))
        (should (equal (plist-get (car calls) :session-id) "runtime"))
        ;; A compatible owner is kept on the next reload.
        (hermes-chat--work-activate)
        (should (eq owner hermes-chat--work-owner))))))

(ert-deftest hermes-chat-work-repeated-bind-keeps-one-focus-advice ()
  "Native add-function deduplicates the named observer across attachments."
  (hermes-test-with-chat-buffer
   (let ((client (hermes-test--dashboard-client))
         (after-focus-change-function #'ignore)
         (pre-redisplay-functions nil)
         (delete-frame-functions nil)
         (calls 0))
     (cl-letf (((symbol-function 'hermes-chat--work-visibility)
                (lambda (&rest _) (cl-incf calls))))
       (dotimes (_ 3) (hermes-chat--work-bind client "runtime"))
       (setq calls 0)
       (funcall after-focus-change-function)
       (should (= calls 1))
       (should (= (cl-count #'hermes-chat--work-window-change
                            pre-redisplay-functions) 1))
       (should (= (cl-count #'hermes-chat--work-visibility
                            delete-frame-functions) 1))))))

(ert-deftest hermes-chat-workers-session-roster-wire ()
  "Public refresh asks the captured runtime for its session roster."
  (hermes-test--with-work-wire
    (hermes-chat-work-refresh)
    (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
           (pending (gethash (hermes-test--work-request-id hermes-chat--work-owner)
                             (hermes-dashboard-transport-client-pending client))))
      (should (equal (hermes-transport--get frame 'method) "subagent.list"))
      (should (equal (hermes-transport--get frame 'params)
                     '((session_id . "runtime"))))
      (should (plist-get pending :lossless-result))
      (should (= (car (plist-get pending :timer)) 10)))))

(ert-deftest hermes-chat-work-process-wire-and-exits ()
  "Both kinds read the runtime; a reply replaces the snapshot wholesale."
  (hermes-test--with-process-wire
    (hermes-chat-work-refresh)
    (should (equal (sort (mapcar (lambda (frame)
                                   (hermes-transport--get
                                    (hermes-dashboard-transport--decode-frame frame) 'method))
                                 frames)
                         #'string<)
                   '("process.list" "subagent.list")))
    (hermes-test--work-answer client "{\"subagents\":[]}")
    (hermes-test--work-answer
     client (format "{\"processes\":[{\"session_id\":\"run\",\"status\":\"running\"},{\"session_id\":\"ok\",\"status\":\"exited\",\"exit_code\":0,\"exited_at\":%s},{\"session_id\":\"bad\",\"status\":\"exited\",\"exit_code\":7,\"exited_at\":%s},{\"session_id\":\"old\",\"status\":\"exited\",\"exit_code\":0,\"exited_at\":1}]}"
                    (- (float-time) 5) (float-time)))
    (let ((owner hermes-chat--work-owner))
      (should (equal (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                             (plist-get (hermes-chat--work-roster owner (float-time))
                                        :processes))
                     '(("run" running) ("bad" failed) ("ok" done))))
      (should (equal (hermes-test--work-text (hermes-chat--work-label nil))
                     (if (char-displayable-p ?⚙) "⚙ 1" "1 process")))
      (should-not (plist-get owner :inflight))
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"subagents\":[]}")
      (hermes-test--work-answer client "{\"processes\":[]}")
      (should-not (plist-get (plist-get owner :processes) :rows))
      (should-not (hermes-chat--work-label nil))
      (should-not events)
      (should-not hermes-chat--pending-assistant-id))))

(ert-deftest hermes-chat-work-failed-refresh-keeps-last-known-counts ()
  "Invalid replies, errors and timeouts leave the snapshot and label alone."
  (hermes-test--with-work-wire
    (hermes-chat-work-refresh)
    (hermes-test--work-answer
     client "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"},{\"subagent_id\":\"b\",\"status\":\"queued\"}]}")
    (let* ((owner hermes-chat--work-owner)
           (snapshot (plist-get owner :delegates))
           (label (if (char-displayable-p ?🤖) "🤖 2" "Agents 2")))
      (should (equal (hermes-test--work-text (hermes-chat--work-label nil)) label))
      (dolist (failure '("{}" "{\"subagents\":null}" "{\"subagents\":7}"
                         "{\"subagents\":[{\"subagent_id\":\"a\"},null]}"
                         "{\"subagents\":[{\"subagent_id\":\"a\"},{\"status\":\"running\"}]}"
                         error timeout))
        (hermes-chat-work-refresh)
        (let ((id (hermes-test--work-request-id owner)))
          (pcase failure
            ('error (hermes-test--work-error client id -32603))
            ('timeout (let ((timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client))
                                              :timer)))
                        (apply (cadr timer) (nth 2 timer))))
            (_ (hermes-test--work-reply client id failure))))
        (should (eq snapshot (plist-get owner :delegates)))
        (should-not (plist-get owner :inflight))
        (should-not (plist-get owner :unsupported))
        (should (equal (hermes-test--work-text (hermes-chat--work-label nil)) label))
        (should-not (string-match-p "private error" (hermes-chat--work-details))))
      (should (= 0 (hash-table-count (hermes-dashboard-transport-client-pending client))))
      (should-not events))))

(ert-deftest hermes-chat-work-malformed-roster-never-completes-workers ()
  "A partly malformed roster is a failed read: no worker leaves or finishes."
  (hermes-test--with-work-wire
    (hermes-chat-work-refresh)
    (hermes-test--work-answer
     client "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"},{\"subagent_id\":\"b\",\"status\":\"running\"}]}")
    (let* ((owner hermes-chat--work-owner)
           (snapshot (plist-get owner :delegates))
           (states (lambda ()
                     (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                             (plist-get (hermes-chat--work-roster owner (float-time)) :agents)))))
      (hermes-chat-work-refresh)
      (hermes-test--work-answer
       client "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"},{\"status\":\"running\"}]}")
      (should (eq snapshot (plist-get owner :delegates)))
      (should-not (plist-get owner :events))
      (should (equal (funcall states) '(("a" running) ("b" running))))
      (should-not (plist-get owner :inflight)))))

(ert-deftest hermes-chat-work-method-not-found-stops-that-kind ()
  "Only method-not-found stops polling a kind; explicit refresh retries it."
  (hermes-test--with-work-wire
    (dolist (code '(-32603 -32601))
      (hermes-chat-work-refresh)
      (hermes-test--work-error client (hermes-test--work-request-id hermes-chat--work-owner) code)
      (should (eq (and (memq :delegates (plist-get hermes-chat--work-owner :unsupported)) t)
                  (eql code -32601))))
    (let ((sent (length frames)))
      (hermes-chat--work-tick hermes-chat--work-owner)
      (should (= sent (length frames)))
      (should-not (hermes-chat--work-label nil))
      (hermes-chat-work-refresh)
      (should (= (1+ sent) (length frames))))))

(ert-deftest hermes-chat-work-session-switch-drops-late-replies ()
  "A new session starts empty, and the old session's late reply is dropped."
  (hermes-test--with-work-wire
    (hermes-chat-work-refresh)
    (hermes-test--work-answer client "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}")
    (hermes-chat-work-refresh)
    (let* ((old hermes-chat--work-owner)
           (id (hermes-test--work-request-id old)))
      (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
                ((symbol-function 'hermes-dashboard-transport-subscribe-session) #'ignore))
        ;; Recording the same runtime keeps the attachment and its counts.
        (hermes-chat--dashboard-record-session client '((session_id . "runtime")))
        (should (eq old hermes-chat--work-owner))
        (should (hermes-chat--work-label nil))
        (hermes-chat--dashboard-record-session client '((session_id . "other"))))
      (let ((new hermes-chat--work-owner))
        (should-not (eq old new))
        (should-not (hermes-chat--work-label nil))
        (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
        (hermes-test--work-reply client id "{\"subagents\":[{\"subagent_id\":\"late\",\"status\":\"running\"}]}")
        (should-not (plist-get new :delegates))
        (should-not (hermes-chat--work-label nil))))))

(ert-deftest hermes-chat-work-live-events-update-the-header ()
  "Admitted subagent events change counts before the next poll."
  (hermes-test--with-work
    (hermes-test--work-set '((:key (delegate . "a") :id "a" :kind delegate :state running)) nil)
    (let ((label (lambda () (hermes-test--work-text (hermes-chat--work-label t))))
          (changes 0))
      (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
                 (lambda () (cl-incf changes))))
        (should (equal (funcall label) "1a"))
        (hermes-chat--work-note-event '(:event "subagent.start" :subagent-id "b"))
        (should (equal (funcall label) "2a"))
        (hermes-chat--work-note-event '(:event "subagent.tool" :subagent-id "b"))
        (should (= changes 1))
        (hermes-chat--work-note-event '(:event "subagent.complete" :subagent-id "a"
                                               :status "completed"))
        (hermes-chat--work-note-event '(:event "subagent.complete" :subagent-id "b"))
        (should-not (funcall label))
        (should (= changes 3))))))

(ert-deftest hermes-chat-work-refinement-popup-workers ()
  "The root renders active worker counts and preserves child Workers shortcuts."
  (hermes-test--with-work
    (should (eq (keymap-lookup hermes-chat-actions-map "W") #'hermes-chat-work))
    (should (eq (keymap-lookup hermes-chat-jobs-map "W") #'hermes-chat-work))
    (let* ((groups (apply #'append (keymap-popup--meta hermes-chat-actions-map 'descriptions)))
           (work (seq-find (lambda (g) (equal (plist-get g :name) "Browse")) groups))
           (entry (seq-find (lambda (e) (equal (plist-get e :key) "W"))
                            (plist-get work :entries)))
           (description (plist-get entry :description)))
      (should entry)
      (should (functionp description))
      (should (= 1 (seq-count
                    (lambda (e) (equal (plist-get e :key) "W"))
                    (apply #'append (mapcar (lambda (g) (plist-get g :entries)) groups)))))
      (should (equal (funcall description) "Workers 0"))
      (hermes-test--work-set '((:key (delegate . "a") :id "a" :kind delegate :state running)
                               (:key (delegate . "b") :id "b" :kind delegate :state running))
                             '((:key (process . "p") :id "p" :kind process :state running)))
      (should (equal (funcall description) "Workers 2"))
      (let* ((rendered (keymap-popup--render-entry entry))
             (number (string-match "2" rendered)))
        (should number)
        (should (eq (get-text-property number 'face rendered) 'keymap-popup-value))
        (should-not (get-text-property 0 'face (funcall description))))
      (let ((chat (current-buffer)) (owner hermes-chat--work-owner))
        (unwind-protect
            (progn
              (call-interactively (keymap-lookup hermes-chat-info-map "W"))
              (should (eq owner hermes-work--owner))
              (should (equal (funcall description) "Workers 2"))
              (hermes-chat-work)
              (should (eq (current-buffer) (plist-get owner :view)))
              (with-current-buffer chat
                (hermes-chat--work-bind client "runtime"))
              (should-error (hermes-chat-work) :type 'user-error)
              (should (equal (funcall description) "Workers 0")))
          (when (buffer-live-p (plist-get owner :view))
            (kill-buffer (plist-get owner :view)))
          (set-buffer chat))))))

;; A synthetic header-line click on WINDOW.
(defun hermes-test--header-click (window)
  "Return a mouse-1 event on WINDOW's header line."
  (list 'mouse-1 (list window 'header-line '(0 . 0) 0)))

(ert-deftest hermes-chat-work-header-count-is-clickable ()
  "The header work count advertises and binds a click; other segments keep details."
  (hermes-test--with-work
    (hermes-test--work-set '((:key (delegate . "a") :id "a" :kind delegate :state running))
                           '((:key (process . "p") :id "p" :kind process :state running)))
    (let* ((label (hermes-chat--work-label nil))
           (map (get-text-property 0 'local-map label)))
      (should (eq (get-text-property (string-match "[0-9]" label) 'face label)
                  'hermes-work-running))
      (should (eq (get-text-property 0 'mouse-face label) 'highlight))
      (should (equal (get-text-property 0 'help-echo label)
                     "mouse-1: show observed workers and processes"))
      (should (eq (lookup-key map [header-line mouse-1])
                  #'hermes-chat-work-at-mouse)))
    (let* ((header (hermes-chat--header-line 120))
           (start (string-search (hermes-test--work-text (hermes-chat--work-label nil))
                                 header)))
      (should start)
      (should (keymapp (get-text-property start 'local-map header)))
      (should (string-prefix-p "mouse-1:" (get-text-property start 'help-echo header)))
      ;; The rest of the header still explains the session.
      (should (string-prefix-p "Directory:" (get-text-property 0 'help-echo header)))
      (should-not (get-text-property 0 'local-map header)))
    (dolist (width '(1 8 12 20 30 40 50 80 120))
      (should (<= (string-width (hermes-chat--header-line width)) width)))))

(ert-deftest hermes-chat-work-header-click-opens-the-clicked-chat ()
  "A click on an unselected chat's count opens that chat's work, not the selected one's."
  (let (opened)
    (save-window-excursion
      (hermes-test-with-chat-buffer
        (let ((clicked (current-buffer)))
          (hermes-test-with-chat-buffer
            (let* ((selected (current-buffer))
                   (_ (progn (delete-other-windows)
                             (switch-to-buffer selected)))
                   (other (split-window)))
              (set-window-buffer other clicked)
              (should (eq (window-buffer (selected-window)) selected))
              (cl-letf (((symbol-function 'hermes-chat-work)
                         (lambda () (interactive) (push (current-buffer) opened))))
                (hermes-chat-work-at-mouse (hermes-test--header-click other)))
              (should (equal opened (list clicked)))
              (should (eq (selected-window) other)))))))))

(ert-deftest hermes-chat-work-refinement-rows-first ()
  "The work list puts rows before scope prose; help retains the full caveat."
  (hermes-test--with-work
    (hermes-test--work-set '((:kind delegate :id "a" :key (delegate . "a")
                                    :goal "Find the bug" :state running))
                           nil)
    (let ((owner hermes-chat--work-owner))
      (unwind-protect
          (progn
            (hermes-chat-work)
            (goto-char (point-min))
            (search-forward "Find the bug")
            (should (<= (line-number-at-pos) 4))
            (should (eq (key-binding (kbd "i")) #'hermes-work-instance-subagents))
            (should (eq (key-binding (kbd "h")) #'hermes-work-scope-details)))
        (when (buffer-live-p (plist-get owner :view))
          (kill-buffer (plist-get owner :view)))))))

(ert-deftest hermes-chat-reasoning-mode-exit-releases-exact-work ()
  "Error and quit in presentation cannot strand work or damage a successor."
  (dolist (condition '(error quit))
    (hermes-test--with-work-wire
      ;; Another subscriber keeps the shared client available for a successor.
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (hermes-chat--reasoning-row "a1" t)
      (hermes-chat-work-refresh)
      (let* ((owner hermes-chat--work-owner)
             (generation hermes-chat--transport-generation)
             (callback (hermes-chat--transport-callback
                        (current-buffer) "a1" nil generation))
             (id (hermes-test--work-request-id owner))
             (pending (hermes-dashboard-transport-client-pending client))
             (timer (plist-get (gethash id pending) :timer))
             cancelled caught)
        (should (plist-get owner :timer))
        (cl-letf (((symbol-function 'cancel-timer)
                   (lambda (target) (push target cancelled))))
          (let ((hermes-chat-state-change-hook
                 (list (lambda () (signal condition '("Display hook failed"))))))
            (condition-case err (fundamental-mode)
              ((error quit) (setq caught err))))
          (should (eq (car caught) condition))
          (should hermes-chat--cleanup-done-p)
          (should (> hermes-chat--transport-generation generation))
          (should-not (hermes-chat--work-current-p owner))
          (should-not (gethash "a1:activity" hermes-chat--nodes))
          (should-not hermes-chat--dashboard-client)
          (should (= (hash-table-count pending) 0))
          (should (= (cl-count timer cancelled :test #'eq) 1))
          (should-not (plist-get owner :timer))
          (hermes-chat--cleanup-buffer)
          (should (= (cl-count timer cancelled :test #'eq) 1))
          (should-not (plist-get owner :timer))
          ;; Rebind in the same buffer; late old events and deadline callbacks
          ;; must neither recreate the row nor settle the successor request.
          (setq hermes-chat--dashboard-client client
                hermes-chat--dashboard-active-session-id "runtime"
                hermes-chat--dashboard-session-ready-p t)
          (hermes-chat--work-bind client "runtime")
          (hermes-chat-work-refresh)
          (let* ((successor hermes-chat--work-owner)
                 (request (plist-get successor :inflight))
                 (next-id (hermes-test--work-request-id successor))
                 (next-timer (plist-get (gethash next-id pending) :timer)))
            (should request)
            (funcall callback '(:type thinking :event "thinking.delta" :content "late"))
            (hermes-test--work-reply client id "{\"subagents\":[]}")
            (apply (cadr timer) (nth 2 timer))
            (hermes-chat--cleanup-buffer)
            (should (eq successor hermes-chat--work-owner))
            (should (eq request (plist-get successor :inflight)))
            (should (eq next-timer (plist-get (gethash next-id pending) :timer)))
            (should (= (hash-table-count pending) 1))
            (should-not (memq next-timer cancelled))
            (should-not (gethash "a1:activity" hermes-chat--nodes))
            (hermes-chat--work-stop)))))))

(ert-deftest hermes-work-activity-mutation-is-local ()
  "Activity insertion/deletion is inert; ordinary input still runs change hooks."
  (dolist (condition '(error quit replace nil))
    (hermes-test--with-work-wire
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (setq hermes-chat--dashboard-token
            (hermes-dashboard-transport-subscribe client #'ignore))
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (let* ((owner hermes-chat--work-owner)
             (token hermes-chat--dashboard-token)
             (nodes hermes-chat--nodes)
             (pending (hermes-dashboard-transport-client-pending client))
             fired caught cancelled)
        (hermes-chat-work-refresh)
        (hermes-test--work-answer client "{\"subagents\":[]}")
        (hermes-test--work-answer client "{\"processes\":[]}")
        (progn
          (should (plist-get owner :timer))
          (hermes-chat-work-refresh)
          (let* ((id (hermes-test--work-request-id owner))
                 (deadline (plist-get (gethash id pending) :timer))
                 (hook (lambda (&rest _)
                         (setq fired t)
                         (pcase condition
                           ((or 'error 'quit) (signal condition '("Chat change")))
                           ('replace (fundamental-mode))))))
            (add-hook 'after-change-functions hook nil t)
            (unwind-protect
                (cl-letf (((symbol-function 'cancel-timer)
                           (lambda (timer) (push timer cancelled))))
                  (condition-case err
                      (progn (hermes-chat--reasoning-row "a1" t)
                             (fundamental-mode))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (should-not fired)
                  (should-not (gethash "a1:activity" nodes))
                  (should-not hermes-chat--dashboard-client)
                  (should-not (gethash token (hermes-dashboard-transport-client-subscribers client)))
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (hash-table-count pending) 0))
                  (should-not (hermes-chat--work-current-p owner))
                  (should-not (gethash (hermes-test--work-request-id owner) pending))
                  (should-not (plist-get owner :timer))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should-not (plist-get owner :timer))
                  (hermes-chat--cleanup-buffer)
                  (hermes-chat--cleanup-buffer)
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should-not (plist-get owner :timer))
                  ;; The binding ended: ordinary buffer edits retain native hooks.
                  (add-hook 'after-change-functions hook nil t)
                  (condition-case err
                      (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "Input"))
                    ((error quit) (setq caught (car err))))
                  (should fired)
                  (should (eq caught (and (memq condition '(error quit)) condition))))
              (setq after-change-functions nil))))))))

(ert-deftest hermes-work-terminal-mutation-settles ()
  "List change hooks cannot interrupt terminal settlement or replace its owner."
  (dolist (event '((:type done :content "Finished") (:type error :content "Failed")))
    (dolist (condition '(error quit replace nil))
      (save-window-excursion
        (hermes-test--with-work-wire
          (hermes-chat--insert-entry
           '(:id "a1" :role assistant :content "" :status streaming))
          (setq hermes-chat--pending-assistant-id "a1"
                hermes-chat--dashboard-stream-assistant-id "a1")
          (hermes-chat--insert-entry
           '(:id "comment" :role commentary :content "Real commentary" :status done))
          (hermes-chat--reasoning-row "a1" t)
          (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
                (refs (hermes-dashboard-transport-client-refcount client))
                view fired caught)
            (unwind-protect
                (progn
                  (hermes-chat-work)
                  (setq view (current-buffer))
                  (add-hook 'after-change-functions
                            (lambda (&rest _)
                              (setq fired t)
                              (pcase condition
                                ((or 'error 'quit) (signal condition '("List change")))
                                ('replace
                                 (with-current-buffer chat
                                   (hermes-chat--work-bind client "successor"))))) nil t)
                  (with-current-buffer chat
                    (condition-case err (hermes-chat--handle-transport-event "a1" event)
                      ((error quit) (setq caught (car err))))
                    (should-not caught)
                    (should-not fired)
                    (should-not hermes-chat--pending-assistant-id)
                    (should-not hermes-chat--dashboard-stream-assistant-id)
                    (should-not (gethash "a1:activity" hermes-chat--nodes))
                    (should (equal (plist-get (ewoc-data (gethash "comment" hermes-chat--nodes)) :content)
                                   "Real commentary"))
                    (should (eq owner hermes-chat--work-owner))
                    (should (eq client hermes-chat--dashboard-client))
                    (should (= refs (hermes-dashboard-transport-client-refcount client))))
                  (should (eq owner (buffer-local-value 'hermes-work--owner view))))
              (when (buffer-live-p view)
                (with-current-buffer view (setq after-change-functions nil))
                (kill-buffer view)))))))))

(ert-deftest hermes-work-deferred-mutation-protects-successor ()
  "A detached render cannot run change hooks mid-print or write a replaced view."
  (dolist (condition '(error quit replace nil))
    (save-window-excursion
      (hermes-test--with-work-wire
        (setf (hermes-dashboard-transport-client-refcount client) 2)
        (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
              view fired caught)
          (unwind-protect
              (progn
                (hermes-chat-work)
                (setq view (current-buffer))
                (with-current-buffer chat (hermes-chat--stop-dashboard-client))
                (let ((publication
                       (seq-find (lambda (timer)
                                   (and (eq (cadr timer) #'hermes-work--render)
                                        (eq (car (nth 2 timer)) owner))) timers)))
                  (should publication)
                  (with-current-buffer chat
                    (setq hermes-chat--dashboard-client client
                          hermes-chat--dashboard-active-session-id "successor"
                          hermes-chat--dashboard-session-ready-p t)
                    (hermes-chat--work-bind client "successor")
                    (hermes-chat-work-refresh))
                  (let* ((successor (buffer-local-value 'hermes-chat--work-owner chat))
                         (request (plist-get successor :inflight))
                         (pending (hermes-dashboard-transport-client-pending client))
                         (deadline (plist-get (gethash (plist-get request :id) pending) :timer))
                         (transcript (with-current-buffer chat (buffer-string))))
                    (with-current-buffer view
                      (add-hook 'after-change-functions
                                (lambda (&rest _)
                                  (setq fired t)
                                  (pcase condition
                                    ((or 'error 'quit) (signal condition '("List change")))
                                    ('replace
                                     (setq after-change-functions nil)
                                     (fundamental-mode)
                                     (let ((inhibit-read-only t))
                                       (erase-buffer) (insert "Successor view content"))))) nil t))
                    (condition-case err (apply (cadr publication) (nth 2 publication))
                      ((error quit) (setq caught (car err))))
                    (should-not caught)
                    (should-not fired)
                    (should (hermes-work--view-p owner))
                    (should (string-match-p "chat detached"
                                            (with-current-buffer view
                                              (format "%s" header-line-format))))
                    ;; Prove the hook is live outside package-owned rendering.
                    (with-current-buffer view
                      (condition-case err
                          (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "User edit"))
                        ((error quit) (setq caught (car err))))
                      (should fired)
                      (should (eq caught (and (memq condition '(error quit)) condition)))
                      (setq after-change-functions nil)
                      (unless (eq condition 'replace)
                        (fundamental-mode)
                        (let ((inhibit-read-only t))
                          (erase-buffer) (insert "Successor view content"))))
                    (apply (cadr publication) (nth 2 publication))
                    (should (eq (buffer-local-value 'major-mode view) 'fundamental-mode))
                    (should-not (buffer-local-value 'hermes-work--owner view))
                    (should (equal (with-current-buffer view (buffer-string)) "Successor view content"))
                    (should (eq successor (buffer-local-value 'hermes-chat--work-owner chat)))
                    (should (eq request (plist-get successor :inflight)))
                    (should (eq deadline (plist-get (gethash (plist-get request :id) pending) :timer)))
                    (should (= (hash-table-count pending) 1))
                    (should (= (hermes-dashboard-transport-client-refcount client) 1))
                    (should (equal transcript (with-current-buffer chat (buffer-string)))))))
            (when (buffer-live-p view)
              (with-current-buffer view (setq after-change-functions nil))
              (kill-buffer view))))))))

(defun hermes-test--work-view-teardown (action condition)
  "Exercise ACTION with an actual work list signalling CONDITION on repaint."
  (save-window-excursion
    (hermes-test--with-work-wire
      (setf (hermes-dashboard-transport-client-refcount client) 2)
      (setq hermes-chat--dashboard-token
            (hermes-dashboard-transport-subscribe client #'ignore))
      (hermes-chat--insert-entry
       '(:id "a1" :role assistant :content "" :status streaming))
      (setq hermes-chat--pending-assistant-id "a1")
      (hermes-chat--reasoning-row "a1" t)
      (hermes-chat-work-refresh)
      (let* ((chat (current-buffer))
             (owner hermes-chat--work-owner)
             (nodes hermes-chat--nodes)
             (token hermes-chat--dashboard-token)
             (callback (hermes-chat--transport-callback
                        chat "a1" nil hermes-chat--transport-generation))
             (id (hermes-test--work-request-id owner))
             (pending (hermes-dashboard-transport-client-pending client))
             (deadline (plist-get (gethash id pending) :timer))
             (cadence (plist-get owner :timer))
             view publication cancelled caught)
        ;; A cadence can already be queued when a manual refresh starts.
        (hermes-test--work-answer client "{\"subagents\":[]}")
        (hermes-test--work-answer client "{\"processes\":[]}")
        (setq cadence (plist-get owner :timer))
        (hermes-chat-work-refresh)
        (setq id (hermes-test--work-request-id owner)
              deadline (plist-get (gethash id pending) :timer))
        (unwind-protect
            (progn
              (hermes-chat-work)
              (setq view (current-buffer))
              (when condition
                (add-hook 'after-change-functions
                          (lambda (&rest _) (signal condition '("Work list display hook")))
                          nil t))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'cancel-timer)
                           (lambda (timer) (push timer cancelled)))
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (condition-case err (funcall action)
                    ((error quit) (setq caught (car err))))
                  (should-not hermes-chat--dashboard-client)
                  (should-not caught)
                  (should-not (gethash token (hermes-dashboard-transport-client-subscribers client)))
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should-not (gethash "a1:activity" nodes))
                  (should-not (hermes-chat--work-current-p owner))
                  (should (= (hash-table-count pending) 0))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should-not (plist-get owner :timer))
                  (setq publication
                        (seq-find (lambda (timer)
                                    (and (eq (cadr timer) #'hermes-work--render)
                                         (eq (car (nth 2 timer)) owner))) timers))
                  (should publication)
                  ;; Deferred presentation may fail, but exact release is complete.
                  (condition-case err
                      (apply (cadr publication) (nth 2 publication))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (with-current-buffer view
                    (setq after-change-functions nil)
                    (unless condition
                      (should (string-match-p "chat detached"
                                              (format "%s" header-line-format)))))
                  (hermes-chat--cleanup-buffer)
                  (hermes-chat--cleanup-buffer)
                  (should (= (hermes-dashboard-transport-client-refcount client) 1))
                  (should (= (cl-count deadline cancelled :test #'eq) 1))
                  (should-not (plist-get owner :timer))
                  ;; Reuse this buffer, then deliver every captured old continuation.
                  (unless (derived-mode-p 'hermes-chat-mode) (hermes-chat-mode))
                  (setq hermes-chat--dashboard-client client
                        hermes-chat--dashboard-active-session-id "runtime"
                        hermes-chat--dashboard-session-ready-p t)
                  (hermes-chat--work-bind client "runtime")
                  (hermes-chat-work-refresh)
                  (let* ((successor hermes-chat--work-owner)
                         (request (plist-get successor :inflight))
                         (next-id (hermes-test--work-request-id successor))
                         (next-timer (plist-get (gethash next-id pending) :timer))
                         (transcript (buffer-string)))
                    (should request)
                    (funcall callback '(:type thinking :event "thinking.delta" :content "late"))
                    (hermes-test--work-reply client id "{\"subagents\":[]}")
                    (apply (cadr deadline) (nth 2 deadline))
                    (apply (cadr cadence) (nth 2 cadence))
                    (apply (cadr publication) (nth 2 publication))
                    (should (eq successor hermes-chat--work-owner))
                    (should (eq request (plist-get successor :inflight)))
                    (should (eq next-timer (plist-get (gethash next-id pending) :timer)))
                    (should (= (hash-table-count pending) 1))
                    (should-not (memq next-timer cancelled))
                    (should (equal transcript (buffer-string)))
                    ;; A delayed repaint cannot write into a reused list buffer.
                    (with-current-buffer view
                      (fundamental-mode)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "Successor view content")))
                    (apply (cadr publication) (nth 2 publication))
                    (should (equal (with-current-buffer view (buffer-string))
                                   "Successor view content"))))))
          (when (buffer-live-p view)
            (with-current-buffer view (setq after-change-functions nil))
            (kill-buffer view))
          (when (buffer-live-p chat)
            (with-current-buffer chat (hermes-chat--stop-dashboard-client))))))))

(ert-deftest hermes-work-list-mode-exit-teardown ()
  "Mode exit releases resources before error, quit, or successful list repaint."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'fundamental-mode condition)))

(ert-deftest hermes-work-list-disconnect-teardown ()
  "Disconnect releases resources before arbitrary work-list display hooks."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'hermes-chat-disconnect condition)))

(ert-deftest hermes-work-list-clear-teardown ()
  "Public clear resets resources before arbitrary work-list display hooks."
  (dolist (condition '(error quit nil))
    (hermes-test--work-view-teardown #'hermes-chat-clear condition)))

(ert-deftest hermes-work-list-reconnect-teardown ()
  "Reconnect forgets the old owner before work-list display hooks can fail."
  (dolist (condition '(error quit nil))
    (save-window-excursion
      (hermes-test--with-work-wire
        (hermes-chat--insert-entry
         '(:id "a1" :role assistant :content "" :status streaming))
        (setq hermes-chat--pending-assistant-id "a1")
        (hermes-chat--reasoning-row "a1" t)
        (hermes-chat-work-refresh)
        (let ((chat (current-buffer))
              (owner hermes-chat--work-owner)
              view caught)
          (unwind-protect
              (progn
                (hermes-chat-work)
                (setq view (current-buffer))
                (when condition
                  (add-hook 'after-change-functions
                            (lambda (&rest _) (signal condition '("Work list display hook")))
                            nil t))
                (with-current-buffer chat
                  (condition-case err
                      (hermes-chat--handle-transport-event
                       "a1" '(:type status :status "reconnecting"))
                    ((error quit) (setq caught (car err))))
                  (should-not caught)
                  (should-not hermes-chat--work-owner)
                  (should-not hermes-chat--dashboard-active-session-id)
                  (should-not (gethash "a1:activity" hermes-chat--nodes))
                  (should (= (hash-table-count
                              (hermes-dashboard-transport-client-pending client)) 0))
                  ;; Reconnect retains the shared client, unlike terminal detach.
                  (should (eq client hermes-chat--dashboard-client))
                  (let ((publication
                         (seq-find (lambda (timer)
                                     (and (eq (cadr timer) #'hermes-work--render)
                                          (eq (car (nth 2 timer)) owner))) timers)))
                    (should publication)
                    (condition-case err
                        (apply (cadr publication) (nth 2 publication))
                      ((error quit) (setq caught (car err))))
                    (should-not caught))))
            (when (buffer-live-p view)
              (with-current-buffer view (setq after-change-functions nil))
              (kill-buffer view))))))))


(ert-deftest hermes-chat-work-readiness-deadline-and-drop ()
  "Held readiness remains bounded; dropping an owner before ready suppresses send."
  (dolist (drop '(nil t))
    (hermes-test--with-work-wire
      (let (ready)
        (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                   (lambda (_client resolve _reject) (setq ready resolve))))
          (hermes-chat-work-refresh))
        (let* ((owner hermes-chat--work-owner)
               (id (hermes-test--work-request-id owner))
               (timer (plist-get (gethash id (hermes-dashboard-transport-client-pending client))
                                 :timer)))
          (should (= (car timer) 10))
          (if drop (hermes-chat--work-stop)
            (apply (cadr timer) (nth 2 timer)))
          (funcall ready)
          (should-not frames)
          (should-not (gethash id (hermes-dashboard-transport-client-pending client))))))))

(ert-deftest hermes-chat-work-synchronous-failures ()
  "Synchronous readiness rejection and send errors leave no wedged read."
  (dolist (failure '(ready send signal))
    (hermes-test--with-work-wire
      (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                 (lambda (_client resolve reject)
                   (pcase failure
                     ('ready (funcall reject "not ready"))
                     ('signal (error "registration failed"))
                     (_ (funcall resolve))))))
        (let ((hermes-dashboard-transport-websocket-send-function
               (lambda (&rest _) (error "send failed"))))
          (hermes-chat-work-refresh)))
      (should-not (plist-get hermes-chat--work-owner :inflight))
      (should-not (plist-get hermes-chat--work-owner :delegates))
      (should (plist-get hermes-chat--work-owner :timer))
      (should-not events))))

(ert-deftest hermes-chat-work-ui-reentry-and-generation ()
  "An old reply cannot publish after a UI hook replaces its owner."
  (hermes-test--with-work
    (hermes-chat-work-refresh)
    (let ((resolve (plist-get (car calls) :resolve)))
      (cl-letf (((symbol-function 'hermes-chat--notify-state-change)
                 (lambda ()
                   (hermes-chat--work-bind client "runtime")
                   (hermes-chat-work-refresh))))
        (funcall resolve (hermes-transport-json-parse-lossless "{\"subagents\":[]}")))
      (should (plist-get hermes-chat--work-owner :inflight))
      (should-not (plist-get hermes-chat--work-owner :delegates))
      (cl-incf hermes-chat--work-generation)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless "{\"subagents\":[]}"))
      (should-not (plist-get hermes-chat--work-owner :delegates)))))

(ert-deftest hermes-chat-work-synchronous-ready-drops-owner ()
  "A readiness callback can destroy the owner before the wrapper returns."
  (hermes-test--with-work-wire
    (let ((owner hermes-chat--work-owner))
      (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                 (lambda (_client resolve _reject)
                   (hermes-chat--work-stop)
                   (funcall resolve))))
        (hermes-chat--work-poll owner))
      (should-not frames)
      (should-not hermes-chat--work-owner)
      (should (= (hash-table-count (hermes-dashboard-transport-client-pending client)) 0)))))

(ert-deftest hermes-chat-work-two-chats-one-client ()
  "A and B share one client but request and receive separate runtime rosters."
  (hermes-test--with-work-wire
    (let ((a (current-buffer)) (owner-a hermes-chat--work-owner)
          (b (generate-new-buffer " *work B*"))
          (wire-a "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}")
          (wire-b "{\"subagents\":[{\"subagent_id\":\"b\",\"status\":\"running\",\"goal\":\"private B\"}]}"))
      (unwind-protect
          (progn
            (hermes-chat-work-refresh)
            (set-window-buffer (split-window-right) b)
            (with-current-buffer b
              (hermes-chat-mode)
              (setq hermes-chat--dashboard-client client
                    hermes-chat--dashboard-active-session-id "runtime-B"
                    hermes-chat--dashboard-session-ready-p t)
              (hermes-chat--work-bind client "runtime-B")
              (hermes-chat-work-refresh)
              (let ((owner-b hermes-chat--work-owner))
                (hermes-test--work-reply client (hermes-test--work-request-id owner-a) wire-a)
                (hermes-test--work-reply client (hermes-test--work-request-id owner-b) wire-b)
                (should (equal (mapcar (lambda (row) (plist-get row :id))
                                      (plist-get (plist-get owner-b :delegates) :rows)) '("b")))
                (with-current-buffer a
                  (should (equal (mapcar (lambda (row) (plist-get row :id))
                                        (plist-get (plist-get owner-a :delegates) :rows)) '("a")))
                  (should-not (string-match-p "private B"
                                              (prin1-to-string (plist-get owner-a :delegates))))))))
        (kill-buffer b)))))

(ert-deftest hermes-chat-work-hiding-keeps-counts ()
  "Hiding stops polling without touching snapshots; reentry rearms the timer."
  (hermes-test--with-work
    (let ((chat (current-buffer)) (owner hermes-chat--work-owner))
      (hermes-chat-work-refresh)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless
                "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}"))
      (let ((snapshot (plist-get owner :delegates))
            (label (hermes-test--work-text (hermes-chat--work-label t)))
            (old-timer (plist-get owner :timer))
            (other (split-window-right)))
        (should (equal label "1a"))
        (set-window-buffer other chat)
        (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
        (delete-window other)
        ;; Run the same default hook dispatched by window-state redisplay.
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (should-not (plist-get owner :timer))
        (should (eq snapshot (plist-get owner :delegates)))
        (should (equal (with-current-buffer chat
                         (hermes-test--work-text (hermes-chat--work-label t)))
                       label))
        ;; A consumed timer from before hiding sends nothing.
        (apply (cadr old-timer) (nth 2 old-timer))
        (should (= (length calls) 1))
        (set-window-buffer (selected-window) chat)
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (should (= (length calls) 1))
        (let ((timer (plist-get owner :timer)))
          (should (= (car timer) 0))
          (apply (cadr timer) (nth 2 timer)))
        (should (= (length calls) 2))
        ;; A failure while hidden still keeps the counts.
        (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
        (run-hook-with-args 'pre-redisplay-functions (selected-window))
        (funcall (plist-get (car calls) :reject) "failed while hidden")
        (should (eq snapshot (plist-get owner :delegates)))
        (should (equal (with-current-buffer chat
                         (hermes-test--work-text (hermes-chat--work-label t)))
                       label)))
      (kill-buffer " *work hidden*"))))

(ert-deftest hermes-chat-work-hidden-chats-get-observed ()
  "Binding a hidden chat reads at once; it then polls slowly while the
indicator is on, and not at all when it is off."
  (dolist (indicator '(t nil))
    (save-window-excursion
      (hermes-test-with-chat-buffer
       (let ((chat (current-buffer))
             (client (hermes-test--dashboard-client))
             (hermes-chat--work-kinds '(:delegates))
             (hermes-chat-work-indicator indicator)
             (hermes-chat-work-hidden-cadence 30)
             calls)
         (setf (hermes-dashboard-transport-client-ready-p client) t)
         (cl-letf (((symbol-function 'hermes-dashboard-transport-subagent-list)
                    (lambda (_client &rest args) (push args calls) (length calls)))
                   ((symbol-function 'run-at-time)
                    (lambda (delay _repeat fn &rest args) (list delay fn args)))
                   ((symbol-function 'cancel-timer) #'ignore))
           (set-window-buffer (selected-window) (get-buffer-create " *work hidden*"))
           (with-current-buffer chat
             (setq hermes-chat--dashboard-client client
                   hermes-chat--dashboard-active-session-id "runtime"
                   hermes-chat--dashboard-session-ready-p t)
             (hermes-chat--work-bind client "runtime")
             (let ((owner hermes-chat--work-owner))
               (should-not (hermes-chat--work-visible-p owner))
               ;; Redisplay keeps the bind read; it runs from its timer.
               (run-hook-with-args 'pre-redisplay-functions (selected-window))
               (let ((timer (plist-get owner :timer)))
                 (should (= (car timer) 0))
                 (apply (cadr timer) (nth 2 timer)))
               (should (= (length calls) 1))
               (funcall (plist-get (car calls) :resolve)
                        (hermes-transport-json-parse-lossless
                         "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}"))
               (should (equal (hermes-test--work-text (hermes-chat--work-label t)) "1a"))
               (if (not indicator)
                   (should-not (plist-get owner :timer))
                 (let ((timer (plist-get owner :timer)))
                   (should (= (car timer) 30))
                   (should (plist-get owner :hidden))
                   (run-hook-with-args 'pre-redisplay-functions (selected-window))
                   (should (eq timer (plist-get owner :timer)))
                   (apply (cadr timer) (nth 2 timer))
                   (should (= (length calls) 2))
                   (should (= (car (plist-get owner :timer)) 30))
                   ;; Showing the chat switches to the fast cadence at once.
                   (set-window-buffer (selected-window) chat)
                   (run-hook-with-args 'pre-redisplay-functions (selected-window))
                   (should (= (car (plist-get owner :timer)) 0))
                   (should-not (plist-get owner :hidden))))))
           (kill-buffer " *work hidden*")))))))

(ert-deftest hermes-chat-work-frame-visibility-notifications ()
  "Focus and redisplay notifications reconcile timers without sending RPCs."
  (dolist (notification '(focus redisplay))
    (hermes-test--with-work
      (let ((owner hermes-chat--work-owner)
            (visible t)
            (notify (if (eq notification 'focus)
                        (lambda () (funcall after-focus-change-function))
                      (lambda ()
                        (run-hook-with-args 'pre-redisplay-functions
                                            (selected-window))))))
        (hermes-chat-work-refresh)
        (funcall (plist-get (car calls) :resolve)
                 (hermes-transport-json-parse-lossless "{\"subagents\":[]}"))
        (let ((snapshot (plist-get owner :delegates)))
          (cl-letf (((symbol-function 'frame-visible-p) (lambda (&rest _) visible)))
            (setq visible 'icon)
            (funcall notify)
            (should-not (plist-get owner :timer))
            (should (eq snapshot (plist-get owner :delegates)))
            (setq visible t)
            (funcall notify)
            (should (= (length calls) 1))
            (let ((timer (plist-get owner :timer)))
              (should (= (car timer) 0))
              (funcall notify)
              (should (eq timer (plist-get owner :timer)))
              (apply (cadr timer) (nth 2 timer)))
            (should (= (length calls) 2))))))))

(ert-deftest hermes-chat-work-connection-turn-and-cleanup ()
  "Normal turns preserve snapshots, but disconnect/reset/mode exit release ownership."
  (dolist (end '(disconnect reset mode kill))
    (hermes-test--with-work-wire
      (hermes-chat-work-refresh)
      (let* ((owner hermes-chat--work-owner)
             (id (hermes-test--work-request-id owner))
             (foreign (list 'unrelated))
             (other-id (let ((hermes-dashboard-transport-request-owner foreign))
                         (hermes-dashboard-transport-request client "other" nil #'ignore #'ignore))))
        (rename-buffer (generate-new-buffer-name " *renamed owner*"))
        (cl-incf hermes-chat--transport-generation)
        (should (hermes-chat--work-current-p owner))
        (pcase end
          ('disconnect (hermes-chat--forget-live-dashboard-session))
          ('reset (hermes-chat--reset-transcript))
          ('mode (fundamental-mode))
          ('kill (kill-buffer (current-buffer))))
        (should-not (hermes-chat--work-current-p owner))
        (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
        ;; Work cleanup itself never cancels requests owned by another feature.
        ;; Full chat teardown may close an unshared client, so check disconnect here.
        (when (eq end 'disconnect)
          (should (gethash other-id (hermes-dashboard-transport-client-pending client))))
        (hermes-test--work-reply client id "{\"subagents\":[]}")
        (should-not (plist-get owner :delegates))
        (should-not (plist-get owner :timer))))))

(ert-deftest hermes-chat-work-connection-change-hides-counts ()
  "A connection generation mismatch detaches even previously positive counts."
  (hermes-test--with-work
    (hermes-chat-work-refresh)
    (funcall (plist-get (car calls) :resolve)
             (hermes-transport-json-parse-lossless
              "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}"))
    (should (equal (hermes-test--work-text (hermes-chat--work-label t)) "1a"))
    (cl-incf (hermes-dashboard-transport-client-generation client))
    (should-not (hermes-chat--work-label t))
    (hermes-chat--work-visibility)
    (should-not (plist-get hermes-chat--work-owner :timer))))

(ert-deftest hermes-chat-work-header-and-details-preserve-draft ()
  "Ready and work coexist without render I/O, point movement or draft changes."
  (hermes-test--with-work
    (hermes-chat--set-header-state :status 'ready)
    (insert "unsent draft")
    (let ((text (buffer-string)) (point (point))
          (chat (current-buffer))
          (window (selected-window))
          (help-map (copy-keymap help-mode-map)))
      (hermes-chat-work-refresh)
      (funcall (plist-get (car calls) :resolve)
               (hermes-transport-json-parse-lossless
                "{\"subagents\":[{\"subagent_id\":\"a\",\"status\":\"running\"}]}"))
      (dolist (width '(12 20 30 40 50 80 120))
        (let ((header (hermes-chat--header-line width)))
          (should (<= (string-width header) width))
          (should-not (string-match-p "Ready" header))
          (when (>= width 30)
            (should (string-match-p
                     (if (< width 50) "1a"
                       (if (char-displayable-p ?🤖) "🤖 1" "Agents 1")) header)))))
      (should (string-match-p "Workers: 1 active of 1 listed" (hermes-chat--work-details)))
      (should (equal text (buffer-string)))
      (should (= point (point)))
      (should (eq window (selected-window)))
      (should (= (length calls) 1))
      (unwind-protect
          (progn
            (hermes-chat-session-details)
            (should (equal help-map help-mode-map))
            ;; Help display may replace the sole chat window in batch Emacs.
            ;; Details alone must not authorize a request, even explicit g.
            (delete-other-windows window)
            (set-window-buffer window "*Hermes Session Details*")
            (should-not (hermes-chat--work-visible-p hermes-chat--work-owner))
            (with-current-buffer "*Hermes Session Details*"
              (call-interactively (key-binding (kbd "g"))))
            (should (= (length calls) 1))
            (set-window-buffer (split-window-right) chat)
            (should (hermes-chat--work-visible-p hermes-chat--work-owner))
            (with-current-buffer "*Hermes Session Details*"
              (call-interactively (key-binding (kbd "g"))))
            (should (= (length calls) 2))
            (with-current-buffer chat
              (should (equal text (buffer-string)))
              (should (= point (point)))))
        (when (get-buffer "*Hermes Session Details*")
          (kill-buffer "*Hermes Session Details*"))))))

;;; Shared-socket runtime isolation

(defun hermes-test--resolve-new-dashboard-session (_client &rest args)
  "Resolve ARGS as a fresh dashboard session."
  (funcall (plist-get (hermes-test--released-create-args args) :resolve)
           '((session_id . "sid-new"))))

(defun hermes-test--confirming-config-set (record)
  "Return a `config.set' stub that calls RECORD and requires confirmation."
  (lambda (_client key value &rest args)
    (funcall record key value args)
    (funcall (plist-get args :resolve)
             (cond
              ((not (equal key "model")) `((key . ,key)))
              ((plist-get args :confirm-expensive-model) '((key . "model")))
              (t '((confirm_required . t) (confirm_message . "Expensive model")))))))

(ert-deftest hermes-chat-dashboard-acquires-client-for-buffer-instance ()
  "Each chat acquires its dashboard client under its pinned instance URL."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (hermes-instances
         '(("local" . "http://127.0.0.1:9119")
           ("remote" . "https://hermes.example.test")))
        urls buffers)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-acquire)
               (lambda (&rest _)
                 (push hermes-dashboard-transport-url urls)
                 (make-hermes-dashboard-transport-client))))
      (unwind-protect
          (dolist (instance '(("local" . "http://127.0.0.1:9119")
                              ("remote" . "https://hermes.example.test")))
            (let ((buffer (generate-new-buffer (hermes-test--chat-buffer-name))))
              (push buffer buffers)
              (with-current-buffer buffer
                (hermes-chat-mode)
                (setq hermes-instance instance)
                (hermes-chat--dashboard-ensure-client))))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              buffers)))
    (should (equal (nreverse urls)
                   '("http://127.0.0.1:9119"
                     "https://hermes.example.test")))))

(defmacro hermes-test--with-submit-wire (&rest body)
  "Run BODY with an attached chat and captured production RPC frames."
  (declare (indent 0) (debug t))
  `(let* ((client (hermes-test--dashboard-client))
          (hermes-transport-send-function #'hermes-transport-send)
          (hermes-dashboard-transport-request-timeout nil)
          frames
          (hermes-dashboard-transport-websocket-send-function
           (lambda (_socket text)
             (push (hermes-dashboard-transport--decode-frame text) frames))))
     (setf (hermes-dashboard-transport-client-ready-p client) t)
     (cl-letf (((symbol-function 'hermes-chat--maybe-refresh-session-title) #'ignore)
               ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
       (hermes-test-with-chat-buffer
        (setq hermes-chat--dashboard-client client
              hermes-chat--dashboard-active-session-id "sid"
              hermes-chat--dashboard-session-ready-p t)
        ,@body))))

(defun hermes-test--submit-wire-event (client type &optional payload)
  "Deliver TYPE and PAYLOAD through CLIENT's production JSON parser."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (method . "event")
             (params . ((type . ,type) (session_id . "sid")
                        (payload . ,payload)))))))

(defun hermes-test--submit-wire-reply (client request &optional reject status)
  "Deliver CLIENT's response to REQUEST, with optional REJECT or STATUS."
  (hermes-dashboard-transport--handle-frame
   client (hermes-dashboard-transport--encode-frame
           `((jsonrpc . "2.0") (id . ,(alist-get 'id request))
             ,(if reject
                  '(error . ((code . -32000) (message . "submit rejected")))
                `(result . ((status . ,(or status "accepted")))))))))

(defun hermes-test--terminal-before-submit-ack (queued)
  "Prove terminal-before-ack settlement for a normal or QUEUED submission."
  (dolist (terminal '("message.complete" "error"))
    (hermes-test--with-submit-wire
     (if queued
         (progn
           (hermes-chat--queue-content "first\nfull body")
           (hermes-chat--drain-queued-message))
       (insert "first\nfull body")
       (hermes-chat-send))
     (let ((request (car frames))
           (context hermes-chat--unsettled-submit-context))
       (should (equal (alist-get 'method request) "prompt.submit"))
       (should (eq (not (null (plist-get context :queue-id))) queued))
       (hermes-chat-queue-message "second\nfull body")
       (hermes-chat-queue-message "third\nfull body")
       (let ((suffix (last hermes-chat--queued-messages 2)))
         (hermes-test--submit-wire-event client "message.start")
         (hermes-test--submit-wire-event client terminal '((text . "finished")))
         (hermes-test--submit-wire-event client terminal '((text . "duplicate")))
         (should-not hermes-chat--pending-assistant-id)
         (should (eq context hermes-chat--unsettled-submit-context))
         (hermes-test--submit-wire-reply client request)
         (should-not (eq context hermes-chat--unsettled-submit-context))
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (should (eq suffix hermes-chat--queued-messages))
         (should (equal (hermes-test--queued-contents)
                        '("second\nfull body" "third\nfull body")))
         (should (= (length frames) 2))
         (let ((successor hermes-chat--unsettled-submit-context))
           (should successor)
           (should (equal (plist-get successor :content) "second\nfull body"))
           (hermes-test--submit-wire-reply client request)
           (should (eq successor hermes-chat--unsettled-submit-context))
           (should (eq suffix hermes-chat--queued-messages)))
         (hermes-test--submit-wire-reply client (car frames))
         (should (equal (hermes-test--queued-contents) '("third\nfull body")))
         (hermes-test--submit-wire-event client "message.complete")
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (should (= (length frames) 3))
         (should (equal (mapcar (lambda (frame)
                                  (alist-get 'text (alist-get 'params frame)))
                                (reverse frames))
                        '("first\nfull body" "second\nfull body" "third\nfull body"))))))))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-ack-normal ()
  "Normal terminal-before-ack releases the request and advances FIFO once."
  (hermes-test--terminal-before-submit-ack nil))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-ack-queued ()
  "Queued terminal-before-ack accepts the exact FIFO head once."
  (hermes-test--terminal-before-submit-ack t))

(ert-deftest hermes-chat-dashboard-submit-terminal-orderings ()
  "Acceptance, terminal and idle settle once in each valid finite ordering."
  (dolist (order '((ack terminal idle) (terminal ack idle)
                   (terminal idle ack)))
    (dolist (status '("accepted" "queued"))
      (hermes-test--with-submit-wire
       (hermes-chat--queue-content "first")
       (hermes-chat--drain-queued-message)
       (let ((request (car frames))
             (assistant hermes-chat--pending-assistant-id))
         ;; Idle then start is the existing authoritative boundary separating a
         ;; server-queued turn from the prior turn's output.
         (hermes-test--submit-wire-event client "session.info" '((running . :false)))
         (hermes-test--submit-wire-event client "message.start")
         (dolist (step order)
           (pcase step
             ('ack (hermes-test--submit-wire-reply client request nil status))
             ('terminal (hermes-test--submit-wire-event client "message.complete"))
             ('idle (hermes-test--submit-wire-event
                     client "session.info" '((running . :false))))))
         (should-not hermes-chat--unsettled-submit-context)
         (should-not hermes-chat--queued-messages)
         (should-not hermes-chat--queued-submit-id)
         (should-not hermes-chat--pending-assistant-id)
         (should-not hermes-chat--server-queued-assistant-id)
         (should (eq (plist-get (ewoc-data (gethash assistant hermes-chat--nodes))
                               :status) 'done))
         (hermes-test--submit-wire-reply client request nil status)
         (should-not (hermes-chat--active-turn-p))
         (should (= (length frames) 1)))))))

(ert-deftest hermes-chat-dashboard-terminal-before-submit-reject ()
  "Late rejection settles its request without sending or losing retained input."
  (dolist (queued '(nil t))
    (hermes-test--with-submit-wire
     (if queued
         (progn
           (hermes-chat--queue-content "first")
           (hermes-chat--drain-queued-message))
       (insert "first") (hermes-chat-send))
     (let* ((request (car frames))
            (reject (plist-get
                     (gethash (alist-get 'id request)
                              (hermes-dashboard-transport-client-pending client))
                     :reject))
            (context hermes-chat--unsettled-submit-context))
       (hermes-chat-queue-message "second")
       (hermes-test--submit-wire-event client "message.complete")
       (hermes-test--submit-wire-event client "session.info" '((running . :false)))
       (hermes-test--submit-wire-reply client request t)
       (should-not (eq context hermes-chat--unsettled-submit-context))
       (if queued
           (progn
             (should (equal (hermes-test--queued-contents) '("first" "second")))
             (should-not hermes-chat--queued-submit-id)
             (should-not hermes-chat--pending-assistant-id)
             (should (= (length frames) 1)))
         ;; The next accepted input may progress, but old rejection must not
         ;; clear the new turn's running state after dispatching it.
         (should (= (length frames) 2))
         (should hermes-chat--dashboard-running-p)
         (should hermes-chat--pending-assistant-id)
         (should (equal (plist-get hermes-chat--unsettled-submit-context :content)
                        "second")))
       (let ((before (copy-tree (hermes-chat--entries)))
             (owner hermes-chat--unsettled-submit-context))
         (funcall reject "late duplicate rejection")
         (should (equal before (hermes-chat--entries)))
         (should (eq owner hermes-chat--unsettled-submit-context)))))))

(defun hermes-test--rejected-head-retry (terminal)
  "Prove explicit retry after rejection, with optional preceding TERMINAL."
    (hermes-test--with-submit-wire
     (hermes-chat--queue-content "first\n  exact body  " nil "compact")
     (hermes-chat--drain-queued-message)
     (hermes-chat-queue-message "second\nbody")
     (insert "newer draft\n  intact  ")
     (let ((request (car frames))
           (head (car hermes-chat--queued-messages))
           (suffix (cdr hermes-chat--queued-messages))
           (draft (buffer-substring-no-properties
                   hermes-chat--input-marker (point-max))))
       (when terminal
         (hermes-test--submit-wire-event client "message.complete"))
       (hermes-test--submit-wire-reply client request t)
       (dotimes (_ 3)
         (hermes-test--submit-wire-event client "session.info" '((running . :false))))
       (should (= (length frames) 1))
       (should (eq head (car hermes-chat--queued-messages)))
       (should (eq suffix (cdr hermes-chat--queued-messages)))
       (should (plist-get head :rejected-p))
       (should-not hermes-chat--unsettled-submit-context)
       (let ((panel (save-window-excursion (hermes-chat-queue-panel))))
         (unwind-protect
             (progn
               (with-current-buffer panel
                 (should (string-match-p "Retry queued message" (buffer-string)))
                 (call-interactively (keymap-lookup hermes-chat-queue-panel-mode-map "r")))
               (should (= (length frames) 2))
               (should-not (plist-get head :rejected-p))
               (should (equal (alist-get 'text (alist-get 'params (car frames)))
                              "first\n  exact body  "))
               (with-current-buffer panel
                 (should-error (hermes-chat-queue-panel-retry) :type 'user-error))
               (should (= (length frames) 2))
               (should (equal draft (buffer-substring-no-properties
                                     hermes-chat--input-marker (point-max))))
               (hermes-test--submit-wire-reply client (car frames))
               (should (eq suffix hermes-chat--queued-messages)))
           (kill-buffer panel))))))

(ert-deftest hermes-chat-dashboard-rejected-head-requires-explicit-retry ()
  "Reject then idle never sends again until an explicit panel retry."
  (hermes-test--rejected-head-retry nil))

(ert-deftest hermes-chat-dashboard-terminal-rejected-head-requires-explicit-retry ()
  "Terminal, reject then idle never sends again until an explicit panel retry."
  (hermes-test--rejected-head-retry t))

(ert-deftest hermes-chat-dashboard-rejected-retry-refuses-invalid-owner ()
  "Busy, detached and stale panels preserve the rejected entry and draft."
  (dolist (change '(busy detached lifetime client session panel mode))
    (hermes-test--with-submit-wire
     (hermes-chat--queue-content "first\nexact")
     (hermes-chat--drain-queued-message)
     (hermes-test--submit-wire-reply client (car frames) t)
     (insert "draft")
     (let ((panel (save-window-excursion (hermes-chat-queue-panel)))
           (head (car hermes-chat--queued-messages)))
       (unwind-protect
           (progn
             (pcase change
               ('busy (setq hermes-chat--dashboard-running-p t))
               ('detached (setq hermes-chat--dashboard-session-ready-p nil))
               ('lifetime (setq hermes-chat--lifecycle-generation 'changed))
               ('client (setq hermes-chat--dashboard-client
                              (hermes-test--dashboard-client)))
               ('session (setq hermes-chat--dashboard-active-session-id "other"))
               ('panel (setq hermes-chat--queue-panel-buffer nil))
               ('mode (setq major-mode 'fundamental-mode)))
             (with-current-buffer panel
               (should-error (call-interactively
                              (keymap-lookup hermes-chat-queue-panel-mode-map "r"))
                             :type 'user-error))
             (should (eq head (car hermes-chat--queued-messages)))
             (should (plist-get head :rejected-p))
             (should (equal "first\nexact" (plist-get head :content)))
             (should (equal "draft" (hermes-chat-input-string)))
             (should (= (length frames) 1)))
         (kill-buffer panel))))))

(ert-deftest hermes-chat-dashboard-rejected-edit-and-remove-preserve-pause ()
  "Editing and removing a suffix cannot silently resend the rejected head."
  (hermes-test--with-submit-wire
   (hermes-chat--queue-content "first")
   (hermes-chat--drain-queued-message)
   (hermes-chat-queue-message "second")
   (hermes-test--submit-wire-reply client (car frames) t)
   (let ((panel (save-window-excursion (hermes-chat-queue-panel))))
     (unwind-protect
         (progn
           (with-current-buffer panel
             (cl-letf (((symbol-function 'read-string-from-buffer)
                        (lambda (&rest _) "edited\nbody")))
               (call-interactively #'hermes-chat-queue-panel-edit))
             (forward-line 1)
             (call-interactively #'hermes-chat-queue-panel-remove))
           (hermes-test--submit-wire-event client "session.info" '((running . :false)))
           (should (= (length frames) 1))
           (should (equal (hermes-test--queued-contents) '("edited\nbody")))
           (should (plist-get (car hermes-chat--queued-messages) :rejected-p))
           (with-current-buffer panel
             (call-interactively #'hermes-chat-queue-panel-remove))
           (hermes-test--submit-wire-event client "session.info" '((running . :false)))
           (should-not hermes-chat--queued-messages)
           (should (= (length frames) 1)))
       (kill-buffer panel)))))

(ert-deftest hermes-chat-dashboard-submit-replaced-owner ()
  "Old wire responses cannot settle changed context, lifetime or connection."
  (dolist (replacement '(context lifetime generation client session))
    (dolist (reject '(nil t))
      (hermes-test--with-submit-wire
       (hermes-chat--queue-content "owned head")
       (hermes-chat--drain-queued-message)
       (hermes-chat-queue-message "untouched suffix")
       (let ((request (car frames)))
         (hermes-test--submit-wire-event client "message.complete")
         (pcase replacement
           ('context (setq hermes-chat--unsettled-submit-context
                           (copy-sequence hermes-chat--unsettled-submit-context)))
           ('lifetime (setq hermes-chat--lifecycle-generation (list 'replacement)))
           ('generation (hermes-chat--next-transport-generation))
           ('client (setq hermes-chat--dashboard-client
                          (hermes-test--dashboard-client)))
           ('session (setq hermes-chat--dashboard-active-session-id "replacement")))
         (let ((owner hermes-chat--unsettled-submit-context)
               (queue hermes-chat--queued-messages)
               (before (copy-tree (hermes-chat--entries)))
               (header (copy-tree hermes-chat--status-state)))
           (hermes-test--submit-wire-reply client request reject "queued")
           (should (eq owner hermes-chat--unsettled-submit-context))
           (should (eq queue hermes-chat--queued-messages))
           (should (equal (hermes-test--queued-contents)
                          '("owned head" "untouched suffix")))
           (should (equal before (hermes-chat--entries)))
           (should (equal header hermes-chat--status-state))
           (should-not hermes-chat--pending-assistant-id)
           (should (= (length frames) 1))))))))

(ert-deftest hermes-chat-dashboard-refreshes-structured-controls ()
  "Control refresh queries the owning session and accepts its snapshot."
  (let ((client (hermes-test--dashboard-client)) dispatch)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control-read)
               (lambda (_client &rest args)
                 (setq dispatch args))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-live")
       (hermes-chat--dashboard-refresh-goal)
       (should (equal (plist-get dispatch :session-id) "sid-live"))
       (funcall (plist-get dispatch :resolve)
                '((control . ((revision . "hash") (updated_at . 0)
                              (goal . ((status . "active") (turns_used . 4) (max_turns . 20)))
                              (loop) (heartbeat)))))
       (should (= (plist-get (plist-get (hermes-session-control-snapshot) :goal)
                             :turns-used)
                  4))))))

(ert-deftest hermes-chat-dashboard-ignores-stale-goal-refresh ()
  "A goal status response cannot update a successor session."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-control-read)
               (lambda (_client &rest args)
                 (setq resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid-old")
       (setq hermes-session-control--state '(:snapshot (:revision "kept") :epoch 0))
       (hermes-chat--dashboard-refresh-goal)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve
                '((control . ((revision . "stale") (updated_at . 0)
                              (goal . :null) (loop . :null) (heartbeat . :null)))))
       (should (equal (plist-get (hermes-session-control-snapshot) :revision) "kept"))))))

(ert-deftest hermes-chat-dashboard-record-session-refreshes-controls ()
  "Attaching a session requests structured control state once."
  (let ((client (hermes-test--dashboard-client)) (refreshes 0))
    (cl-letf (((symbol-function 'hermes-chat--dashboard-refresh-goal)
               (lambda () (setq refreshes (1+ refreshes)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client)
       (hermes-chat--dashboard-record-session
        client '((session_id . "sid-live")))
       (should (= refreshes 1))))))

(ert-deftest hermes-chat-dashboard-record-session-does-not-mutate-shared-client ()
  "Recording a session result updates buffer-local vars only, never the client."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (hermes-chat--dashboard-set-subscriber
      client (lambda (_event)))
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-live")
               (stored_session_id . "sid-stored")))
     (should (equal hermes-chat--dashboard-active-session-id "sid-live"))
     (should (equal hermes-chat--session-id "sid-stored"))
     (should hermes-chat--dashboard-session-ready-p)
     (should-not (hermes-dashboard-transport-client-session-id client))
     (should-not (hermes-dashboard-transport-client-stored-session-id client)))))

(ert-deftest hermes-chat-dashboard-record-session-binds-subscriber-token ()
  "Recording a session binds the buffer's subscriber token to its session id."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-token
           (hermes-dashboard-transport-subscribe
            client (lambda (_event))))
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid-route")))
     (should (eq (gethash "sid-route"
                          (hermes-dashboard-transport-client-session-index
                           client))
                 hermes-chat--dashboard-token)))))

(ert-deftest hermes-chat-dashboard-shared-client-keeps-buffer-local-session-ids ()
  "Two buffers sharing one client keep independent buffer-local session ids."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback)))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-a"))))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-token
                    (hermes-dashboard-transport-subscribe
                     shared (lambda (_event))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-b"))))
            ;; Buffer B's later record must not clobber buffer A.
            (with-current-buffer buf-a
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--dashboard-active-session-id
                             "sid-b")))
            ;; The shared client holds no ambient session identity.
            (should-not (hermes-dashboard-transport-client-session-id
                         shared)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-model-switch-targets-current-buffer-session ()
  "A model switch from buffer A targets A's session, not the last recorded one."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        config-session shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-session (plist-get args :session-id))
                 (funcall (plist-get args :resolve) '((ok . t)))))
              ((symbol-function 'hermes-dashboard-transport-model-options-cached)
               (lambda (_client &rest args)
                 (funcall (plist-get args :resolve)
                          '((providers .
                             (((slug . "p")
                               (authenticated . t)
                               (models . ("m")))))))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-a"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--dashboard-active-session-id "sid-b"
                    hermes-chat--dashboard-session-ready-p t))
            (with-current-buffer buf-a
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_items &rest _) "m · p")))
                (hermes-chat-switch-model)))
            ;; Buffer A's switch must carry A's session id.
            (should (equal config-session "sid-a")))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-create-seeds-runtime-before-config-gate ()
  "Fresh creation seeds customized runtime before the post-create gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (push 'config order)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high")
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :model) "gpt-5"))
         (should (equal (plist-get create-args :provider) "openai"))
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (equal config-calls
                        '(("reasoning" "high" "sid-new")
                          ("model" "gpt-5 --provider openai" "sid-new"))))
         (should (equal (reverse order) '(config config submit)))
         (should-not hermes-chat--dashboard-create-model)
         (should-not hermes-chat--dashboard-create-provider))))))

(ert-deftest hermes-chat-dashboard-first-build-uses-draft-model ()
  "The deferred agent build snapshots the draft model during `session.create'."
  (let ((client (hermes-test--dashboard-client)) built-model prompt-model)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq built-model (or (plist-get args :model) "profile-default"))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key _value &rest args)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _args) (setq prompt-model built-model))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "grok-4.6"
               hermes-chat--dashboard-create-provider "xai-oauth")
         (insert "hello")
         (hermes-chat-send)
         (should (equal prompt-model "grok-4.6")))))))


(ert-deftest hermes-chat-dashboard-create-confirms-model-before-first-prompt ()
  "An expensive pre-session model is confirmed before `prompt.submit'."
  (let ((client (hermes-test--dashboard-client)) calls order)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               #'hermes-test--resolve-new-dashboard-session)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (key value args)
                  (push (list key value (plist-get args :session-id)
                              (plist-get args :confirm-expensive-model)) calls)
                  (push 'config order))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) (push 'submit order))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-model "gpt-expensive"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (reverse calls)
                        '(("model" "gpt-expensive --provider openai" "sid-new" nil)
                          ("model" "gpt-expensive --provider openai" "sid-new" t)
                          ("reasoning" "high" "sid-new" nil)
                          ("fast" "fast" "sid-new" nil))))
         (should (equal (reverse order)
                        '(config config config config submit))))))))

(ert-deftest hermes-chat-dashboard-control-decline-retries-on-next-action ()
  "A control-action decline preserves the model for an owned retry."
  (let ((client (hermes-test--dashboard-client))
        (answers '(nil t)) action rejected calls create-model)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-model (plist-get args :model))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (hermes-test--confirming-config-set
                (lambda (_key _value args)
                  (push (plist-get args :confirm-expensive-model) calls)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t))
        (lambda (message) (setq rejected message)))
       (should-not action)
       (should rejected)
       (should (equal create-model "gpt-expensive"))
       (should hermes-chat--dashboard-create-model)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq action t)))
       (should action)
       (should (equal (reverse calls) '(nil nil t)))
       (should-not hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-retry-overrides-are-non-replacing ()
  "Concurrent retry callers preserve the first owner and send one batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid"
             hermes-chat--dashboard-create-fast-p t
             hermes-chat--create-overrides-retry-session-id "sid")
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (string-match-p "configuration is in progress" second-reject))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)
       (should-not hermes-chat--create-overrides-retry-session-id)))))

(ert-deftest hermes-chat-dashboard-fresh-overrides-reject-late-control ()
  "A control caller cannot bypass an attached fresh-session override batch."
  (let ((client (hermes-test--dashboard-client))
        calls resolve first-action second-action first-reject second-reject)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0))
                       resolve (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-create-fast-p t)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_client) (setq first-action t))
        (lambda (message) (setq first-reject message)))
       (let ((owner hermes-chat--create-override-owner))
         (should owner)
         (should-not hermes-chat--create-overrides-retry-session-id)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq second-action t))
          (lambda (message) (setq second-reject message)))
         (should (eq hermes-chat--create-override-owner owner)))
       (should (= calls 1))
       (should hermes-chat--dashboard-create-fast-p)
       (should-not first-action)
       (should-not first-reject)
       (should-not second-action)
       (should (equal second-reject "Session setup is in progress"))
       (funcall resolve '((key . "fast")))
       (should first-action)
       (should-not first-reject)
       (should-not second-action)
       (should-not hermes-chat--create-override-owner)
       (should-not hermes-chat--dashboard-create-fast-p)))))

(ert-deftest hermes-chat-dashboard-remote-create-keeps-profile-default ()
  "Prompt and control creation omit launch defaults even when available."
  (dolist (path '(prompt control))
    (dolist (outcome '(success reject signal malformed))
      (let ((client (hermes-test--dashboard-client))
            api-calls create-cwd action)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                   (lambda (&rest _) client))
                  ((symbol-function 'hermes-dashboard-transport-api-request-async)
                   (lambda (&rest _)
                     (setq api-calls (1+ (or api-calls 0)))
                     (pcase outcome
                       ('success (hermes--promise-resolved
                                  '((cwd . "/srv/default"))))
                       ('reject (hermes--promise-rejected "missing"))
                       ('signal (error "missing"))
                       ('malformed (hermes--promise-resolved '((cwd . "")))))))
                  ((symbol-function 'hermes-dashboard-transport-session-create)
                   (lambda (_client &rest args)
                     (setq args (hermes-test--released-create-args args))
                     (setq create-cwd (plist-get args :cwd))
                     (funcall (plist-get args :resolve) '((session_id . "sid")))))
                  ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                   (lambda (&rest _) (setq action t)))
                  ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
          (let ((hermes-transport-send-function #'hermes-transport-send))
            (hermes-test-with-chat-buffer
             (setq-local hermes-chat--resolved-start-mode 'remote)
             (setq default-directory "/tmp/editor/"
                   hermes-chat--working-directory nil)
             (pcase path
               ('prompt (insert "hi") (hermes-chat-send))
               ('control
                (setq hermes-chat--dashboard-client client)
                (hermes-chat--dashboard-ensure-session-action
                 client (current-buffer) (lambda (_client) (setq action t)))))
             (should-not api-calls)
             (should-not create-cwd)
             (should (equal hermes-chat--working-directory create-cwd))
             (should (equal default-directory "/tmp/editor/"))
             (should action))))))))

(ert-deftest hermes-chat-dashboard-fresh-reservation-rejects-each-phase ()
  "One control owner spans preflight, create, and override settlement."
  (let ((client (hermes-test--dashboard-client))
        (preflight (hermes--promise-make))
        api-calls create-calls config-calls create-resolve config-resolve
        actions rejections)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _)
                 (setq api-calls (1+ (or api-calls 0)))
                 preflight))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-calls (1+ (or create-calls 0))
                       create-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq config-calls (1+ (or config-calls 0))
                       config-resolve (plist-get args :resolve))))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq hermes-chat--dashboard-client client
             hermes-chat--working-directory nil
             hermes-chat--dashboard-create-fast-p t)
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer)
        (lambda (_client)
          (setq actions (1+ (or actions 0)))
          (error "continuation boom"))
        (lambda (message) (push message rejections)))
       (should-not api-calls)
       (hermes-chat--dashboard-ensure-session
        client "late prompt" (current-buffer) nil
        (lambda (message) (push message rejections)))
       (hermes--promise-resolve preflight '((cwd . "/srv/default")))
       (should (= create-calls 1))
       (hermes-chat--dashboard-ensure-session
        client "late queued" (current-buffer) nil
        (lambda (message) (push message rejections)) t)
       (funcall create-resolve '((session_id . "sid")))
       (should (= config-calls 1))
       (hermes-chat--dashboard-ensure-session
        client "late attached prompt" (current-buffer) nil
        (lambda (message) (push message rejections)))
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) #'ignore (lambda (message) (push message rejections)))
       (should (= (length rejections) 4))
       (should (cl-every (lambda (message)
                           (equal message "Session setup is in progress"))
                         rejections))
       (should (= create-calls 1))
       (should (= config-calls 1))
       (should-not actions)
       (funcall config-resolve '((key . "fast")))
       (should (= actions 1))
       (should (equal (car rejections) "continuation boom"))
       (should-not (bound-and-true-p hermes-chat--session-bootstrap))
       (should-not hermes-chat--create-override-owner)))))

(ert-deftest hermes-chat-dashboard-create-failure-settles-origin ()
  "Sync and async create failures settle prompt and control origins once."
  (dolist (path '(prompt control))
    (dolist (preflight '(resolve reject))
      (dolist (failure '(sync async))
        (let ((client (hermes-test--dashboard-client))
              action rejected create-reject)
          (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
                     (lambda (&rest _) client))
                    ((symbol-function 'hermes-dashboard-transport-api-request-async)
                     (lambda (&rest _)
                       (if (eq preflight 'resolve)
                           (hermes--promise-resolved '((cwd . "/srv/default")))
                         (hermes--promise-rejected "missing"))))
                    ((symbol-function 'hermes-dashboard-transport-session-create)
                     (lambda (_client &rest args)
                       (setq args (hermes-test--released-create-args args))
                       (if (eq failure 'sync)
                           (error "create boom")
                         (setq create-reject (plist-get args :reject)))))
                    ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
            (let ((hermes-transport-send-function #'hermes-transport-send))
              (hermes-test-with-chat-buffer
               (setq-local hermes-chat--resolved-start-mode 'remote)
               (setq hermes-chat--dashboard-client client
                     hermes-chat--working-directory nil)
               (pcase path
                 ('prompt (insert "recover me") (hermes-chat-send))
                 ('control
                  (hermes-chat--dashboard-ensure-session-action
                   client (current-buffer) (lambda (_client) (setq action t))
                   (lambda (message) (push message rejected)))))
               (when (eq failure 'async)
                 (funcall create-reject "create boom")
                 (funcall create-reject "duplicate boom"))
               (pcase path
                 ('prompt
                  (should (string-match-p "create boom" (buffer-string)))
                  (should-not hermes-chat--pending-assistant-id)
                  (should-not hermes-chat--unsettled-submit-context))
                 ('control
                  (should (equal rejected '("create boom")))
                  (should-not action)))
               (should-not (bound-and-true-p hermes-chat--session-bootstrap))))))))))

(ert-deftest hermes-chat-dashboard-reset-cancels-each-bootstrap-phase ()
  "Reset cancels preflight, create, and override owners before late callbacks."
  (dolist (phase '(create override))
    (let ((client (hermes-test--dashboard-client))
          (preflight (hermes--promise-make))
          create-resolve config-resolve action rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                 (lambda (&rest _) preflight))
                ((symbol-function 'hermes-dashboard-transport-session-create)
                 (lambda (_client &rest args)
                   (setq args (hermes-test--released-create-args args))
                   (setq create-resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client _key _value &rest args)
                   (setq config-resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq hermes-chat--dashboard-client client
               hermes-chat--working-directory nil
               hermes-chat--dashboard-create-fast-p t)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t))
          (lambda (message) (setq rejected message)))
         (when (memq phase '(create override))
           (hermes--promise-resolve preflight '((cwd . "/srv/default"))))
         (when (eq phase 'override)
           (funcall create-resolve '((session_id . "sid"))))
         (should (bound-and-true-p hermes-chat--session-bootstrap))
         (should (eq (plist-get hermes-chat--session-bootstrap :phase) phase))
         (hermes-chat--reset-transcript)
         (pcase phase
           ('preflight
            (hermes--promise-resolve preflight '((cwd . "/srv/late"))))
           ('create (funcall create-resolve '((session_id . "late"))))
           ('override (funcall config-resolve '((key . "fast")))))
         (should-not action)
         (should-not rejected)
         (should-not hermes-chat--dashboard-active-session-id)
         (should-not (bound-and-true-p hermes-chat--session-bootstrap))
         (should-not hermes-chat--create-override-owner))))))

(ert-deftest hermes-chat-dashboard-stale-model-result-does-not-prompt ()
  "A model result for a replaced session rejects without prompting."
  (let ((client (hermes-test--dashboard-client)) resolve prompted continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq resolve (plist-get args :resolve))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq prompted t) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((confirm_required . t)))
       (should-not prompted)
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-model-confirmation-revalidates-after-prompt ()
  "Replacing the session during confirmation prevents the confirmed retry."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-active-session-id "sid-new")
                 t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 1))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-repeated-model-confirmation-rejects ()
  "A confirmed retry that still requires confirmation rejects without continuing."
  (let ((client (hermes-test--dashboard-client)) calls continued rejected)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq calls (1+ (or calls 0)))
                 (funcall (plist-get args :resolve) '((confirm_required . t)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old"
             hermes-chat--dashboard-create-model "gpt-expensive")
       (hermes-chat--dashboard-apply-create-overrides
        client (lambda () (setq continued t))
        hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
       (should (= calls 2))
       (should-not continued)
       (should rejected)
       (should-not hermes-chat--create-override-owner)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
       (should hermes-chat--dashboard-create-model)))))

(ert-deftest hermes-chat-dashboard-confirmed-retry-sync-failure-settles-origin ()
  "A synchronous confirmed-retry error or quit releases exact ownership."
  (dolist (signal '(error quit))
    (let ((client (hermes-test--dashboard-client)) calls resolve rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client _key _value &rest args)
                   (setq calls (1+ (or calls 0)))
                   (if (= calls 1)
                       (setq resolve (plist-get args :resolve))
                     (signal signal nil))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old"
               hermes-chat--dashboard-create-model "gpt-expensive")
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (funcall resolve '((confirm_required . t)))
         (should (= calls 2))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should hermes-chat--dashboard-create-model))))))

(ert-deftest hermes-chat-dashboard-create-uses-chat-working-directory ()
  "A fresh session starts in its gateway working directory."
  (let ((client (hermes-test--dashboard-client)) create-cwd)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-cwd (plist-get args :cwd))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory "/srv/remote-project"
               hermes-chat--cwd-explicit-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal create-cwd "/srv/remote-project")))))))

(ert-deftest hermes-chat-dashboard-workspace-reconciles-before-first-prompt ()
  "A deliberate cwd is accepted before runtime overrides and the first prompt."
  (let ((client (hermes-test--dashboard-client)) requests cwd-resolve)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client client
           hermes-chat--resolved-start-mode 'remote
           hermes-chat--working-directory "/chosen"
           hermes-chat--cwd-explicit-p t
           hermes-chat--dashboard-create-reasoning-effort "high")
     (let ((editor-directory default-directory))
       (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                  (lambda (_client method params resolve _reject)
                    (push (cons method params) requests)
                    (pcase method
                      ("session.create"
                       (funcall resolve '((session_id . "sid")
                                          (info . ((cwd . "/profile-default"))))))
                      ("session.cwd.set" (setq cwd-resolve resolve))
                      ("config.set" (funcall resolve '((ok . t)))))))
                 ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
                 ((symbol-function 'hermes-chat--dashboard-submit-prompt)
                  (lambda (&rest _) (push '(prompt) requests))))
         (hermes-chat--dashboard-ensure-session client "hello" (current-buffer))
         (should (equal (mapcar #'car (reverse requests))
                        '("session.create" "session.cwd.set")))
         (should (equal (cdr (car requests))
                        '((session_id . "sid") (cwd . "/chosen"))))
         (should hermes-chat--session-bootstrap)
         (should-not (equal hermes-chat--working-directory "/chosen"))
         (funcall cwd-resolve '((cwd . "/canonical/chosen")))
         (should (equal (mapcar #'car (reverse requests))
                        '("session.create" "session.cwd.set" "config.set" prompt)))
         (should (equal hermes-chat--working-directory "/canonical/chosen"))
         (should (equal default-directory editor-directory))
         (should-not hermes-chat--session-bootstrap))))))

(ert-deftest hermes-chat-dashboard-workspace-failure-retries-the-same-session ()
  "A failed workspace receipt retains intent without sending or recreating."
  (let ((client (hermes-test--dashboard-client)) requests cwd-reject cwd-resolve)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client client
           hermes-chat--working-directory "/chosen"
           hermes-chat--cwd-explicit-p t)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method _params resolve reject)
                  (push method requests)
                  (pcase method
                    ("session.create"
                     (funcall resolve '((session_id . "sid")
                                        (info . ((cwd . "/profile-default"))))))
                    ("session.cwd.set"
                     (setq cwd-resolve resolve cwd-reject reject)))))
               ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
               ((symbol-function 'hermes-chat--dashboard-submit-prompt)
                (lambda (&rest _) (push 'prompt requests))))
       (hermes-chat--dashboard-ensure-session
        client "first" (current-buffer) nil #'ignore)
       (should cwd-reject)
       (funcall cwd-reject "unavailable")
       (should-not hermes-chat--session-bootstrap)
       (should (equal hermes-chat--create-overrides-retry-session-id "sid"))
       (hermes-chat--dashboard-ensure-session
        client "retry" (current-buffer) nil #'ignore)
       (funcall cwd-resolve '((cwd . "/chosen")))
       (should (equal (reverse requests)
                      '("session.create" "session.cwd.set" "session.cwd.set" prompt)))
       (should-not hermes-chat--create-overrides-retry-session-id)))))

(ert-deftest hermes-chat-dashboard-workspace-missing-receipt-cannot-release ()
  "Malformed acceptance never releases pending deliberate workspace intent."
  (dolist (receipt '(nil ((cwd . ""))))
    (let ((client (hermes-test--dashboard-client)) continued rejected)
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid"
             hermes-chat--dashboard-create-cwd "/chosen")
       (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                  (lambda (_client method _params resolve _reject)
                    (should (equal method "session.cwd.set"))
                    (funcall resolve receipt))))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (setq continued t)) hermes-chat--lifecycle-generation
          (lambda (message) (setq rejected message)))
         (should rejected)
         (should-not continued)
         (should (equal hermes-chat--dashboard-create-cwd "/chosen"))
         (should (equal hermes-chat--create-overrides-retry-session-id "sid")))))))

(ert-deftest hermes-chat-dashboard-workspace-public-send-keeps-fifo ()
  "Send reserves workspace acceptance while a second input stays in FIFO."
  (let ((client (hermes-test--dashboard-client)) cwd-resolve prompts)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (setf (hermes-dashboard-transport-client-callback client)
                       (plist-get args :callback))
                 client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-request)
               (lambda (_client method params resolve _reject)
                 (pcase method
                   ("session.create"
                    (funcall resolve '((session_id . "sid")
                                       (info . ((cwd . "/profile"))))))
                   ("session.cwd.set" (setq cwd-resolve resolve))
                   ("prompt.submit"
                    (push (hermes-transport--get params 'text) prompts)
                    (funcall resolve '((accepted . t))))))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--working-directory "/chosen"
               hermes-chat--cwd-explicit-p t)
         (insert "first")
         (hermes-chat-send)
         (insert "second")
         (hermes-chat-send)
         (should-not prompts)
         (should (equal (hermes-test--queued-contents) '("second")))
         (funcall cwd-resolve '((cwd . "/chosen")))
         (should (equal prompts '("first")))
         (hermes-dashboard-transport--handle-frame
          client (hermes-dashboard-transport--encode-frame
                  '((jsonrpc . "2.0") (method . "event")
                    (params . ((type . "message.complete") (session_id . "sid")
                               (payload . ((text . "done") (status . "completed"))))))))
         (should (equal (reverse prompts) '("first" "second")))
         (should-not hermes-chat--queued-messages))))))

(ert-deftest hermes-chat-dashboard-workspace-control-waits-for-acceptance ()
  "Fresh control actions cannot pass a pending workspace reservation."
  (let ((client (hermes-test--dashboard-client)) cwd-resolve action)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client client
           hermes-chat--working-directory "/chosen"
           hermes-chat--cwd-explicit-p t)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client method _params resolve _reject)
                  (pcase method
                    ("session.create"
                     (funcall resolve '((session_id . "sid")
                                        (info . ((cwd . "/profile"))))))
                    ("session.cwd.set" (setq cwd-resolve resolve)))))
               ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore))
       (hermes-chat--dashboard-ensure-session-action
        client (current-buffer) (lambda (_) (setq action t)))
       (should-not action)
       (should (eq (plist-get hermes-chat--session-bootstrap :kind) 'control))
       (funcall cwd-resolve '((cwd . "/chosen")))
       (should action)
       (should-not hermes-chat--session-bootstrap)))))

(ert-deftest hermes-chat-dashboard-workspace-native-readiness-and-receipts ()
  "Native RPCs fence held dispatch and late readback against retired owners."
  (dolist (schedule '(current connection session lifetime owner))
    (let ((client (hermes-test--dashboard-client)) ready frames continued)
      (hermes-test-with-chat-buffer
       (setq hermes-chat--dashboard-client client
             hermes-chat--dashboard-active-session-id "sid"
             hermes-chat--dashboard-create-cwd "/chosen")
       (let ((hermes-dashboard-transport-request-owner (current-buffer))
             (hermes-dashboard-transport-websocket-send-function
              (lambda (_socket text)
                (push (hermes-dashboard-transport--decode-frame text) frames))))
         (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                    (lambda (_client resolve _reject) (setq ready resolve))))
           (hermes-chat--dashboard-apply-create-overrides
            client (lambda () (setq continued t))
            hermes-chat--lifecycle-generation #'ignore))
         (let* ((pending (hermes-dashboard-transport-client-pending client))
                (id (car (hash-table-keys pending))))
           (should (eq (plist-get (gethash id pending) :owner) (current-buffer)))
           (pcase schedule
             ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
             ('session (setq hermes-chat--dashboard-active-session-id "successor"))
             ('lifetime (setq hermes-chat--lifecycle-generation
                              (hermes-chat--next-lifetime-token)))
             ('owner (setq hermes-chat--create-override-owner nil)))
           (funcall ready)
           (if (eq schedule 'current)
               (progn
                 (should (equal (hermes-transport--get (car frames) 'method)
                                "session.cwd.set"))
                 (should-not continued)
                 (hermes-dashboard-transport--handle-frame
                  client (hermes-dashboard-transport--encode-frame
                          `((jsonrpc . "2.0") (id . ,id)
                            (result . ((cwd . "/accepted"))))))
                 (should continued)
                 (should (equal hermes-chat--working-directory "/accepted")))
             (should-not frames)
             (should-not continued))
           (should-not (gethash id pending))))))))

(ert-deftest hermes-chat-dashboard-workspace-native-late-receipt ()
  "A dispatched old workspace reply cannot rename or release its successor."
  (let ((client (hermes-test--dashboard-client)) frames continued)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--dashboard-client client
           hermes-chat--dashboard-active-session-id "sid"
           hermes-chat--dashboard-create-cwd "/chosen")
     (let ((hermes-dashboard-transport-websocket-send-function
            (lambda (_socket text)
              (push (hermes-dashboard-transport--decode-frame text) frames))))
       (cl-letf (((symbol-function 'hermes-dashboard-transport--when-ready)
                  (lambda (_client resolve _reject) (funcall resolve))))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (setq continued t))
          hermes-chat--lifecycle-generation #'ignore))
       (let ((id (hermes-transport--get (car frames) 'id))
             (successor (list :successor t)))
         (setq hermes-chat--create-override-owner successor
               hermes-chat--working-directory "/successor")
         (hermes-dashboard-transport--handle-frame
          client (hermes-dashboard-transport--encode-frame
                  `((jsonrpc . "2.0") (id . ,id)
                    (result . ((cwd . "/old"))))))
         (should (eq hermes-chat--create-override-owner successor))
         (should (equal hermes-chat--working-directory "/successor"))
         (should-not continued))))))

(ert-deftest hermes-chat-dashboard-remote-create-omits-unknown-cwd ()
  "A detached remote chat never submits its editor directory as gateway cwd."
  (let ((client (hermes-test--dashboard-client)) create-args)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-api-request-async)
               (lambda (&rest _) (hermes--promise-rejected "unavailable")))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-args args)
                 (funcall (plist-get args :resolve) '((session_id . "sid")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq default-directory "/tmp/local-editor/"
               hermes-chat--working-directory nil)
         (insert "hi")
         (hermes-chat-send)
         (should create-args)
         (should-not (plist-get create-args :cwd))
         (should (equal default-directory "/tmp/local-editor/")))))))

(ert-deftest hermes-chat-set-directory-preserves-editor-directory ()
  "Gateway-only paths must not replace or probe the editor directory."
  (let* ((local-directory (make-temp-file "hermes-directory-" t))
         (local-file (make-temp-file "hermes-directory-file-")))
    (unwind-protect
        (dolist (directory (list (concat local-directory "/missing")
                                 local-file "relative/path" "~/"
                                 "/ssh:example.org:/srv/project"))
          (hermes-test-with-chat-buffer
           (setq default-directory (file-name-as-directory local-directory)
                 hermes-chat--dashboard-client (hermes-test--dashboard-client)
                 hermes-chat--dashboard-session-ready-p t
                 hermes-chat--dashboard-active-session-id "sid")
           (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                      (lambda (_client _cwd &rest args)
                        (funcall (plist-get args :resolve)
                                 `((cwd . ,directory))))))
             (let ((file-name-handler-alist
                    (cons (cons (rx string-start "/ssh:example.org:")
                                (lambda (&rest _)
                                  (ert-fail "Gateway path invoked a file handler")))
                          file-name-handler-alist)))
               (hermes-chat-set-directory directory))
             (should (equal hermes-chat--working-directory directory))
             (should (equal default-directory
                            (file-name-as-directory local-directory))))))
      (delete-directory local-directory)
      (delete-file local-file))))

(ert-deftest hermes-chat-set-directory-converges-after-session-info ()
  "The current response adopts cwd after passive gateway projection."
  (ert-with-temp-directory directory
    :prefix "translated"
    (dolist (project-root '(nil "/tmp/project-a/"))
      (let* ((client (hermes-test--dashboard-client))
             (editor-directory (or project-root "/tmp/local-editor/"))
             resolve request)
        (hermes-test-with-chat-buffer
         (setq-local hermes-chat--resolved-start-mode 'remote)
         (setq default-directory editor-directory
               hermes-chat--launch-project-root project-root
               hermes-chat--title "Saved session title"
               hermes-chat--working-directory "/srv/old"
               hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid")
         (hermes-chat--refresh-buffer-name)
         (let ((initial-name (buffer-name))
               (event-callback
                (hermes-chat--transport-callback
                 (current-buffer) nil t hermes-chat--transport-generation)))
           (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                      (lambda (_client cwd &rest args)
                        (setq request (list cwd (plist-get args :session-id))
                              resolve (plist-get args :resolve))))
                     ;; Keep the temp cwd's name independent of any VCS
                     ;; checkout enclosing TMPDIR.
                     ((symbol-function 'project-current) #'ignore))
             (hermes-chat-set-directory "C:/project")
             (should (equal request '("C:/project" "sid")))
             (should (equal (buffer-name) initial-name))
             (funcall event-callback
                      `(:type status :event "session.info" :status "ready"
                              :session-id "sid" :cwd ,directory))
             (should (equal hermes-chat--working-directory
                            directory))
             (should (equal default-directory editor-directory))
             ;; A launch project keeps its name and shows the moved cwd;
             ;; otherwise the first recorded cwd names the chat.
             (let ((name (if project-root "\\[project-a\\]"
                           "\\[translated.*\\]")))
               (should (string-match-p name (buffer-name)))
               (if project-root
                   (should (string-match-p
                            "translated" (hermes-test--header-line-string)))
                 (should-not (string-match-p
                              "translated" (hermes-test--header-line-string))))
               (funcall resolve `((cwd . ,directory)))
               (should (equal default-directory directory))
               (should (equal hermes-chat--working-directory
                              directory))
               (should (string-match-p name (buffer-name))))
             (should (equal hermes-chat--launch-project-root project-root))
             (should (equal hermes-chat--title "Saved session title"))
             (when project-root
               (should (equal
                        (hermes-chat--project-buffers
                         project-root (list (current-buffer)))
                        (list (current-buffer))))))))))))

(ert-deftest hermes-chat-set-directory-separates-event-response-ownership ()
  "A replacement blocks stale event or response effects at its own boundary."
  (dolist (schedule '(before-event before-response))
    (let ((client (hermes-test--dashboard-client)) resolve)
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq default-directory "/tmp/local-editor/"
             hermes-chat--working-directory "/srv/old"
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid-old")
       (hermes-chat--refresh-buffer-name)
       (let ((initial-name (buffer-name))
             (event-callback
              (hermes-chat--transport-callback
               (current-buffer) nil t hermes-chat--transport-generation)))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                    (lambda (_client _cwd &rest args)
                      (setq resolve (plist-get args :resolve)))))
           (hermes-chat-set-directory "C:/project")
           (when (eq schedule 'before-event)
             (setq hermes-chat--dashboard-active-session-id "sid-new"))
           (funcall event-callback
                    '(:type status :event "session.info" :status "ready"
                            :session-id "sid-old" :cwd "/mnt/c/translated"))
           (when (eq schedule 'before-response)
             (setq hermes-chat--dashboard-active-session-id "sid-new"))
           (funcall resolve '((cwd . "/mnt/c/translated")))
           (should (equal default-directory "/tmp/local-editor/"))
           (pcase schedule
             ('before-event
              (should (equal hermes-chat--working-directory "/srv/old"))
              (should (equal (buffer-name) initial-name)))
             ('before-response
              (should (equal hermes-chat--working-directory
                             "/mnt/c/translated"))
              (should (string-match-p
                       "\\[translated\\]" (buffer-name)))))))))))

(ert-deftest hermes-chat-unknown-remote-directory-starts-with-manual-entry ()
  "A detached remote chat asks for a gateway path without listing a local path."
  (ert-with-temp-directory directory
    :prefix "translated"
    (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
      (hermes-test-with-chat-buffer
       (setq-local hermes-chat--resolved-start-mode 'remote)
       (setq default-directory "/tmp/local-editor/"
             hermes-chat--working-directory nil
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid")
       (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (&rest _) (ert-fail "unknown cwd must not be listed")))
                 ((symbol-function 'read-string)
                  (lambda (_prompt initial &rest _)
                    (setq prompt-default initial)
                    directory))
                 ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                  (lambda (_client cwd &rest args)
                    (setq set-cwd cwd)
                    (funcall (plist-get args :resolve) `((cwd . ,cwd))))))
         (hermes-chat-set-directory)
         (should (equal prompt-default ""))
         (should (equal set-cwd directory))
         (should (equal default-directory directory)))))))

(ert-deftest hermes-chat-set-directory-uses-authoritative-backend-path ()
  "Changing directory applies the authoritative backend path to the chat."
  (ert-with-temp-directory directory
    :prefix "translated"
    (let ((client (hermes-test--dashboard-client))
          request)
      (hermes-test-with-chat-buffer
       (setq default-directory "/tmp/local-editor/"
             hermes-chat--working-directory "/srv/old"
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid")
       (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                  (lambda (_client cwd &rest args)
                    (setq request (list cwd (plist-get args :session-id)))
                    (funcall (plist-get args :resolve)
                             `((cwd . ,directory))))))
         (hermes-chat-set-directory "C:/project")
         (should (equal request '("C:/project" "sid")))
         (should (equal hermes-chat--working-directory directory))
         (should (equal default-directory directory))
         (should (string-match-p (rx "[translated" (* nonl) "]")
                                 (buffer-name))))))))

(ert-deftest hermes-chat-directory-parent-handles-instance-path-syntax ()
  "Parent navigation is lexical for Unix and Windows instance paths."
  (should (equal (hermes-chat--directory-parent "/srv/project/") "/srv"))
  (should (equal (hermes-chat--directory-parent "/srv") "/"))
  (should (equal (hermes-chat--directory-parent "/") "/"))
  (should (equal (hermes-chat--directory-parent "C:\\Users\\Thanos")
                 "C:\\Users"))
  (should (equal (hermes-chat--directory-parent "C:\\Users") "C:\\"))
  (should (equal (hermes-chat--directory-parent "C:\\") "C:\\")))

(ert-deftest hermes-chat-set-directory-ignores-stale-rejection ()
  "A directory rejection cannot surface in a successor session."
  (let ((client (hermes-test--dashboard-client)) reject surfaced)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq reject (plist-get args :reject))))
               ((symbol-function 'hermes-chat--command-error)
                (lambda (message) (setq surfaced message))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall reject "old session rejected")
       (should-not surfaced)
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-set-directory-ignores-stale-resolution ()
  "A directory response cannot change a successor session's local context."
  (let ((client (hermes-test--dashboard-client)) resolve)
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/"
           hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client _cwd &rest args)
                  (setq resolve (plist-get args :resolve)))))
       (hermes-chat-set-directory "/srv/new")
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (funcall resolve '((cwd . "/srv/new")))
       (should (equal hermes-chat--working-directory "/srv"))
       (should (equal default-directory "/tmp/local-editor/"))))))

(ert-deftest hermes-chat-dashboard-record-session-records-authoritative-cwd ()
  "Session creation records nested backend cwd without changing editor cwd."
  (let ((client (hermes-test--dashboard-client)))
    (hermes-test-with-chat-buffer
     (setq default-directory "/tmp/local-editor/")
     (hermes-chat--dashboard-record-session
      client '((session_id . "sid") (info . ((cwd . "/srv/project")))))
     (should (equal hermes-chat--working-directory "/srv/project"))
     (should (equal default-directory "/tmp/local-editor/")))))

(ert-deftest hermes-chat-set-directory-browses-instance-filesystem ()
  "Interactive directory selection walks `/api/fs/list' on the owning instance."
  (ert-with-temp-directory local-directory
    (let ((client (hermes-test--dashboard-client)) requests set-cwd)
      (hermes-test-with-chat-buffer
       (setq default-directory "/tmp/local-editor/"
             hermes-chat--working-directory "/srv"
             hermes-chat--dashboard-client client
             hermes-chat--dashboard-session-ready-p t
             hermes-chat--dashboard-active-session-id "sid")
       (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                  (lambda (method path &rest args)
                    (let ((directory (cdr (assq 'path (plist-get args :query)))))
                      (push (list method path directory (plist-get args :client))
                            requests)
                      (hermes--promise-resolved
                       (if (equal directory "/srv")
                           '((entries . (((name . "project")
                                          (path . "/srv/project")
                                          (isDirectory . t))
                                         ((name . "README")
                                          (path . "/srv/README")
                                          (isDirectory . :false)))))
                         '((entries . ())))))))
                 ((symbol-function 'completing-read)
                  (lambda (_prompt candidates &rest _)
                    (car (seq-find
                          (lambda (candidate)
                            (let ((choice (cdr candidate)))
                              (if (= (length requests) 1)
                                  (and (eq (plist-get choice :action) 'browse)
                                       (equal (plist-get choice :path)
                                              "/srv/project"))
                                (eq (plist-get choice :action) 'select))))
                          candidates))))
                 ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                  (lambda (_client cwd &rest args)
                    (setq set-cwd (list cwd (plist-get args :session-id)))
                    (funcall (plist-get args :resolve)
                             `((cwd . ,local-directory))))))
         (hermes-chat-set-directory)
         (should (equal (reverse requests)
                        (list (list "GET" "/api/fs/list" "/srv" client)
                              (list "GET" "/api/fs/list" "/srv/project" client))))
         (should (equal set-cwd '("/srv/project" "sid")))
         (should (equal hermes-chat--working-directory local-directory))
         (should (equal default-directory local-directory)))))))

(ert-deftest hermes-chat-directory-browser-falls-back-to-manual-path ()
  "An unavailable listing endpoint still accepts an instance-native path."
  (let ((client (hermes-test--dashboard-client)) prompt-default set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) (hermes--promise-rejected "404 not found")))
               ((symbol-function 'read-string)
                (lambda (_prompt initial &rest _)
                  (setq prompt-default initial)
                  "/opt/manual"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (_client cwd &rest args)
                  (setq set-cwd (list cwd (plist-get args :session-id)))
                  (funcall (plist-get args :resolve)
                           '((cwd . "/opt/manual"))))))
       (hermes-chat-set-directory)
       (should (equal prompt-default "/srv"))
       (should (equal set-cwd '("/opt/manual" "sid")))
       (should (equal hermes-chat--working-directory "/opt/manual"))))))

(ert-deftest hermes-chat-directory-browser-ignores-stale-response ()
  "A directory listing cannot prompt or mutate a successor session."
  (let ((client (hermes-test--dashboard-client))
        (promise (hermes--promise-make)) prompted)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _) promise))
               ((symbol-function 'completing-read)
                (lambda (&rest _) (setq prompted t))))
       (hermes-chat-set-directory)
       (setq hermes-chat--dashboard-active-session-id "sid-new")
       (hermes--promise-resolve promise '((entries . ())))
       (should-not prompted)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-completion ()
  "A completion owned by an old session cannot browse or set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (_method _path &rest args)
                  (push (cdr (assq 'path (plist-get args :query))) requests)
                  (hermes--promise-resolved
                   '((entries . (((name . "project")
                                  (path . "/srv/project")
                                  (isDirectory . t))))))))
               ((symbol-function 'completing-read)
                (lambda (_prompt candidates &rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  (car (seq-find
                        (lambda (candidate)
                          (eq (plist-get (cdr candidate) :action) 'browse))
                        candidates))))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (equal requests '("/srv")))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-directory-browser-ignores-session-change-in-manual-read ()
  "A manual path owned by an old session cannot set its successor."
  (let ((client (hermes-test--dashboard-client)) requests set-cwd)
    (hermes-test-with-chat-buffer
     (setq hermes-chat--working-directory "/srv"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid-old")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (&rest _)
                  (setq requests (1+ (or requests 0)))
                  (hermes--promise-rejected "listing unavailable")))
               ((symbol-function 'read-string)
                (lambda (&rest _)
                  (setq hermes-chat--dashboard-active-session-id "sid-new")
                  "/opt/project"))
               ((symbol-function 'hermes-dashboard-transport-session-cwd-set)
                (lambda (&rest _) (setq set-cwd t))))
       (hermes-chat-set-directory)
       (should (= requests 1))
       (should-not set-cwd)
       (should (equal hermes-chat--working-directory "/srv"))))))

(ert-deftest hermes-chat-set-directory-requires-chat-buffer ()
  "The globally autoloaded directory command rejects non-chat buffers."
  (with-temp-buffer
    (should-error (hermes-chat-set-directory temporary-file-directory)
                  :type 'user-error)))

(ert-deftest hermes-chat-set-directory-rejects-active-turn ()
  "Changing directory is unavailable while the current turn is active."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-running-p t)
   (should-error (hermes-chat-set-directory "/tmp/") :type 'user-error)))

(ert-deftest hermes-chat-dashboard-create-seeds-reasoning-fast-before-config-set ()
  "Pre-session reasoning and fast picks seed creation before their config gate."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        create-args config-calls)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-create)
               (lambda (_client &rest args)
                 (setq args (hermes-test--released-create-args args))
                 (setq create-args args)
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-new")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client key value &rest args)
                 (push (list key value (plist-get args :session-id))
                       config-calls)
                 (funcall (plist-get args :resolve) `((key . ,key)))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "hi")
         (hermes-chat-send)
         (should (equal (plist-get create-args :reasoning-effort) "high"))
         (should (eq (plist-get create-args :fast) t))
         (should (equal (reverse config-calls)
                        '(("reasoning" "high" "sid-new")
                          ("fast" "fast" "sid-new"))))
         (should-not hermes-chat--dashboard-create-reasoning-effort)
         (should-not hermes-chat--dashboard-create-fast-p))))))

(ert-deftest hermes-chat-dashboard-sync-override-failure-settles-origin ()
  "Synchronous model and non-model failures release exact override ownership."
  (dolist (spec '((model error) (reasoning quit)))
    (let ((client (hermes-test--dashboard-client)) rejected)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (&rest _) (signal (cadr spec) nil))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car spec)
           ('model (setq hermes-chat--dashboard-create-model "gpt"))
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort "high")))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (ert-fail "failure must not continue"))
          hermes-chat--lifecycle-generation (lambda (message) (setq rejected message)))
         (should rejected)
         (should-not hermes-chat--create-override-owner)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (should (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-discards-unowned-create-overrides ()
  "A stored session never receives create overrides without retry provenance."
  :tags '(shared-socket-isolation)
  (let ((client (hermes-test--dashboard-client))
        resume-model resume-provider resume-reasoning resume-fast configs action)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session-id &rest args)
                 (setq resume-model (plist-get args :model)
                       resume-provider (plist-get args :provider)
                       resume-reasoning (plist-get args :reasoning-effort)
                       resume-fast (plist-get args :fast))
                 (funcall (plist-get args :resolve)
                          '((session_id . "sid-live")
                            (resumed . "sid-stored")))))
              ((symbol-function 'hermes-dashboard-transport-config-set)
               (lambda (_client _key _value &rest args)
                 (setq configs (1+ (or configs 0)))
                 (funcall (plist-get args :resolve) '((key . "unexpected")))))
              ((symbol-function 'hermes-dashboard-transport-prompt-submit)
               (lambda (&rest _) 'prompt-request)))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-provider "openai"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (insert "resume me")
         (hermes-chat-send)
         (should-not resume-model)
         (should-not resume-provider)
         (should-not resume-reasoning)
         (should-not resume-fast)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))
        (setq action nil configs nil)
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "sid-stored"
               hermes-chat--dashboard-client client
               hermes-chat--dashboard-create-model "gpt-5"
               hermes-chat--dashboard-create-reasoning-effort "high"
               hermes-chat--dashboard-create-fast-p t)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (setq action nil)
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should-not configs)
         (should-not (hermes-chat--dashboard-create-config-cells)))))))

(ert-deftest hermes-chat-dashboard-resume-restores-pending-batch-clarify ()
  "Session resume renders locked answers and sends only the unanswered question."
  (let ((client (hermes-test--dashboard-client)) resume sent reads)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
              ((symbol-function 'hermes-notifications-notify) #'ignore)
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve))))
              ((symbol-function
                'hermes-dashboard-transport-clarify-question-respond)
               (lambda (_client request question answer &optional resolve _reject)
                 (setq sent (list request question answer))
                 (funcall resolve '((status . "ok")))))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt reads)
                 "Remaining")))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (funcall
        resume
        '((session_id . "live") (stored_session_id . "stored")
          (messages . (((role . "assistant") (text . "History"))))
          (pending_clarify
           . ((request_id . "req-batch")
              (questions . [((qid . "q0") (question . "Locked"))
                            ((qid . "q1") (question . "Open"))])
              (answers . ((q0 . "Accepted")))))))
       (should (string-match-p "Answered: Accepted" (buffer-string)))
       (hermes-chat-respond-to-prompt "req-batch")
       (should (equal reads '("Open: ")))
       (should (equal sent '("req-batch" "q1" "Remaining")))))))

(ert-deftest hermes-chat-dashboard-stale-resume-cannot-install-clarify ()
  "A late session-resume result cannot install a prompt after reset."
  (let ((client (hermes-test--dashboard-client)) resume)
    (cl-letf (((symbol-function 'hermes-chat--dashboard-start)
               (lambda (&rest _)
                 (setq hermes-chat--dashboard-client client)
                 client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client _session &rest args)
                 (setq resume (plist-get args :resolve)))))
      (hermes-test-with-chat-buffer
       (setq hermes-chat--session-id "stored")
       (hermes-chat--load-session-history (current-buffer))
       (hermes-chat--reset-transcript)
       (funcall resume
                '((session_id . "stale")
                  (pending_clarify
                   . ((request_id . "req-stale")
                      (questions . [((qid . "q0") (question . "Stale"))])))))
       (should-not (hermes-chat--pending-prompt-p))
       (should-not (string-match-p "Stale" (buffer-string)))))))

(ert-deftest hermes-chat-dashboard-stale-non-model-override-stays-session-bound ()
  "Late reasoning and fast results settle without mutating their successor."
  (dolist (cell '((reasoning . "high") ("fast" . "fast")))
    (let ((client (hermes-test--dashboard-client)) resolve continued aborted action requests)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-config-set)
                 (lambda (_client key _value &rest args)
                   (push (list key (plist-get args :session-id)) requests)
                   (unless resolve (setq resolve (plist-get args :resolve))))))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--dashboard-client client
               hermes-chat--dashboard-session-ready-p t
               hermes-chat--dashboard-active-session-id "sid-old")
         (pcase (car cell)
           ('reasoning
            (setq hermes-chat--dashboard-create-reasoning-effort (cdr cell)))
           ("fast" (setq hermes-chat--dashboard-create-fast-p t)))
         (hermes-chat--dashboard-apply-create-overrides
          client (lambda () (setq continued t))
          hermes-chat--lifecycle-generation (lambda (message) (setq aborted message)))
         (setq hermes-chat--dashboard-active-session-id "sid-new")
         (funcall resolve '((key . "stale")))
         (should-not continued)
         (should aborted)
         (should (equal hermes-chat--create-overrides-retry-session-id "sid-old"))
         (hermes-chat--dashboard-ensure-session-action
          client (current-buffer) (lambda (_client) (setq action t)))
         (should action)
         (should (= (length requests) 1))
         (should-not (hermes-chat--dashboard-create-config-cells))
         (should-not hermes-chat--create-overrides-retry-session-id))))))

(ert-deftest hermes-chat-dashboard-reconnected-keeps-each-buffer-detached ()
  "A shared reconnect leaves each buffer's durable session lazy."
  :tags '(shared-socket-isolation)
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        resumed shared buf-a buf-b)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (make-hermes-dashboard-transport-client
                  :websocket 'fake-websocket
                  :callback (plist-get args :callback))))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest _args)
                 (push session-id resumed))))
      (unwind-protect
          (progn
            (setq buf-a (generate-new-buffer (hermes-test--chat-buffer-name))
                  buf-b (generate-new-buffer (hermes-test--chat-buffer-name)))
            (with-current-buffer buf-a
              (hermes-chat-mode)
              (setq shared (hermes-chat--dashboard-start
                            (lambda (_event)))
                    hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-a"))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start (lambda (_event)))
              (setq hermes-chat--dashboard-client shared
                    hermes-chat--session-id "stored-b"))
            ;; Simulate reconnect for each buffer.
            (with-current-buffer buf-a
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-a" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (with-current-buffer buf-b
              (funcall (hermes-chat--transport-callback
                        (current-buffer) "asst-b" t
                        (hermes-chat--next-transport-generation))
                       '(:type status :status "reconnected")))
            (should-not resumed)
            (with-current-buffer buf-a
              (should (equal hermes-chat--session-id "stored-a")))
            (with-current-buffer buf-b
              (should (equal hermes-chat--session-id "stored-b"))))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-work-browser-exact-owner-keys-and-snapshots ()
  "The outline consumes real owner snapshots and never acquires a client."
  (save-window-excursion
    (hermes-test--with-process-wire
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"subagents\":[{\"subagent_id\":\"same\",\"owner_agent_session_id\":\"A\",\"status\":\"running\",\"goal\":\"界 50% goal\"}]}")
      (hermes-test--work-answer client (format "{\"processes\":[{\"session_id\":\"same\",\"status\":\"exited\",\"exit_code\":7,\"exited_at\":%s,\"command\":\"printf done\",\"cwd\":\"/remote/inert\",\"output_tail\":\"last output\"}]}" (float-time)))
      (let ((chat (current-buffer)) (owner hermes-chat--work-owner)
            (sent (length frames)) view details instance)
        (insert "draft text")
        (backward-char 3)
        (let ((offset (- (point) (hermes-chat--input-position))))
          (unwind-protect
              (cl-letf (((symbol-function 'hermes-browser--run-on-client)
                         (lambda (&rest _) (ert-fail "Work view acquired a client"))))
                (call-interactively (keymap-lookup hermes-chat-mode-map "C-c C-w"))
                (setq view (current-buffer))
                (should (derived-mode-p 'hermes-work-mode))
                (should (eq owner hermes-work--owner))
                (should (equal hermes-work--order
                               '((section . agents) (delegate . "same")
                                 (section . processes) (process . "same"))))
                (should (string-match-p "Roster visibility" (hermes-work--scope-text owner)))
                (goto-char (hermes-work--find-line '(delegate . "same")))
                (should (eq (get-text-property (point) 'face) 'hermes-work-running))
                (rename-buffer "*renamed work view*" t)
                (with-current-buffer chat (hermes-chat-work))
                (should (eq view (current-buffer)))
                (goto-char (point-min))
                (search-forward "printf done")
                (call-interactively (keymap-lookup hermes-work-mode-map "RET"))
                (setq details (current-buffer))
                (should (derived-mode-p 'hermes-work-detail-mode))
                (should buffer-read-only)
                (should (string-match-p "Output tail\nlast output" (buffer-string)))
                (should (string-match-p "^Cwd +/remote/inert$" (buffer-string)))
                (should-not (next-button (point-min)))
                (should (= sent (length frames)))
                (call-interactively (keymap-lookup (current-local-map) "q"))
                (with-current-buffer view
                  (cl-letf (((symbol-function 'hermes-list-subagents)
                             (lambda () (interactive) (setq instance hermes-instance))))
                    (call-interactively (keymap-lookup hermes-work-mode-map "i")))
                  (should (equal instance (plist-get owner :instance)))
                  (call-interactively (keymap-lookup hermes-work-mode-map "g")))
                (should (= (+ 2 sent) (length frames)))
                (with-current-buffer chat
                  (should (equal (buffer-substring-no-properties (hermes-chat--input-position) (point-max)) "draft text"))
                  (should (= offset (- (point) (hermes-chat--input-position))))
                  (hermes-test--work-answer client "{\"subagents\":[]}")
                  (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"same\",\"status\":\"running\"}]}"))
                (with-current-buffer view
                  (should (equal (hermes-work--key-at-point) '(process . "same")))
                  ;; The worker that left the roster stays listed as done.
                  (should (eq 'completed
                              (plist-get (hermes-work--find '(delegate . "same")
                                                            (hermes-work--observations owner))
                                         :state)))
                  (should (hermes-work--find-line '(delegate . "same"))))
                ;; Replacement must detach, not retarget, even after renaming.
                (with-current-buffer chat (hermes-chat--work-bind client "runtime"))
                (let ((publication
                       (seq-find (lambda (timer)
                                   (and (eq (cadr timer) #'hermes-work--render)
                                        (eq (car (nth 2 timer)) owner))) timers)))
                  (should publication)
                  (apply (cadr publication) (nth 2 publication)))
                (with-current-buffer view
                  (should (string-match-p "chat detached" (format "%s" header-line-format)))
                  (should-error (hermes-work-refresh) :type 'user-error)
                  (should (eq (keymap-lookup hermes-work-mode-map "k") #'hermes-work-interrupt))))
            (dolist (buffer (list view details))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest hermes-work-list-only-visibility-and-mode-replacement ()
  "Only the exact list, not details or a reused name/mode, keeps cadence visible."
  (save-window-excursion
    (hermes-test--with-process-wire
      (let ((owner hermes-chat--work-owner) view replacement)
        (unwind-protect
            (progn
              (hermes-chat-work)
              (setq view (current-buffer))
              (delete-other-windows)
              (should (hermes-chat--work-visible-p owner))
              (setq replacement (generate-new-buffer "*work-details-only*"))
              (with-current-buffer replacement (special-mode))
              (set-window-buffer (selected-window) replacement)
              (should-not (hermes-chat--work-visible-p owner))
              (hermes-chat--work-visibility)
              (should-not (plist-get owner :timer))
              (set-window-buffer (selected-window) view)
              (with-current-buffer view (fundamental-mode))
              (should-not (plist-get owner :view))
              (should-not (hermes-chat--work-visible-p owner))
              (with-current-buffer view
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "unrelated replacement")))
              (hermes-work--render owner)
              (should (equal (with-current-buffer view (buffer-string)) "unrelated replacement")))
          (dolist (buffer (list view replacement))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-work-details-link-and-widths ()
  "Details open the same owner view; rows fit real window widths."
  (save-window-excursion
    (hermes-test--with-process-wire
      (hermes-chat-work-refresh)
      (hermes-test--work-answer client "{\"subagents\":[{\"subagent_id\":\"w\",\"status\":\"running\",\"model\":\"claude-opus-5-5\",\"tool_count\":12,\"started_at\":1,\"goal\":\"界% a very long worker goal that must truncate before its metadata\"}]}")
      (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"p\",\"status\":\"running\",\"command\":\"界% very long command with long description\",\"uptime_seconds\":100}]}")
      (let ((owner hermes-chat--work-owner) view details)
        (unwind-protect
            (progn
              (hermes-chat-session-details)
              (setq details (get-buffer "*Hermes Session Details*"))
              (with-current-buffer details
                (goto-char (point-min))
                (search-forward "Browse observed work")
                (let ((button (button-at (1- (point)))))
                  (should (equal (button-label button) "Browse observed work"))
                  (button-activate button)))
              (setq view (plist-get owner :view))
              (should (buffer-live-p view))
              (should (eq (window-buffer (selected-window)) view))
              (with-current-buffer view
                (goto-char (hermes-work--find-line '(process . "p")))
                (dolist (width '(30 40 50 80 120))
                  (cl-letf (((symbol-function 'window-body-width)
                             (lambda (&rest _) width)))
                    (hermes-work--resize (selected-window)))
                  (should (equal (hermes-work--key-at-point) '(process . "p")))
                  ;; Each heading is goal, an aligning space, then metadata.
                  (dolist (key '((delegate . "w") (process . "p")))
                    (save-excursion
                      (goto-char (hermes-work--find-line key))
                      (let* ((end (line-end-position))
                             (space (text-property-search-forward 'display nil
                                                                  (lambda (_ v) (consp v)) t))
                             (before (buffer-substring-no-properties
                                      (line-beginning-position) (prop-match-beginning space)))
                             (after (buffer-substring-no-properties (prop-match-end space) end)))
                        (should (<= (+ (string-width before) 1 (string-width after)) width))
                        (should (get-text-property (line-beginning-position 1) 'hermes-work-key)))))))
              ;; Old list render must not write into a successor that steals its slot.
              (with-current-buffer view
                (setq hermes-work--owner nil)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "Successor view content")))
              (let ((before (with-current-buffer view (buffer-string))))
                (hermes-work--render owner)
                (should (equal before (with-current-buffer view (buffer-string))))))
          (dolist (buffer (list view details))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest hermes-chat-background-early-completion-reconciles-launch ()
  (dolist (early '(t nil))
    (let (resolve)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-background)
                 (lambda (_client _content &rest args)
                   (setq resolve (plist-get args :resolve)))))
        (hermes-test-with-dashboard-prompt-session (client)
          (hermes-chat-background "side investigation")
          (unless early (funcall resolve '((task_id . "task-a"))))
          (hermes-test--emit-dashboard-prompt client "background.complete"
            '((task_id . "task-a") (text . "literal result")))
          (when early (funcall resolve '((task_id . "task-a"))))
          (should-not (assoc "task-a" hermes-chat--background-tasks))
          (let* ((results (seq-filter (lambda (entry) (eq (plist-get entry :role) 'background))
                                      (hermes-chat--entries)))
                 (entry (car results)))
            (should (= 1 (length results)))
            (should (equal (plist-get entry :content) "literal result"))
            (should (= 1 (plist-get (plist-get entry :metadata) :number)))
            (should (equal (plist-get (plist-get entry :metadata) :preview) "side investigation")))
          (when early
            (should-not (string-match-p "Background #1 started" (buffer-string)))))))))

(ert-deftest hermes-chat-background-early-results-match-exact-launches ()
  (let (requests)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-background)
               (lambda (_client content &rest args) (push (cons content args) requests))))
      (hermes-test-with-dashboard-prompt-session (client)
        (hermes-chat-background "first launch")
        (hermes-chat-background "second launch")
        (hermes-test--emit-dashboard-prompt client "background.complete"
          '((task_id . "second") (text . "second result")))
        (hermes-test--emit-dashboard-prompt client "background.complete"
          '((task_id . "unrelated") (text . "unknown result")))
        (hermes-test--emit-dashboard-prompt client "background.complete"
          '((task_id . "first") (text . "first result")))
        (funcall (plist-get (cdr (assoc "first launch" requests)) :resolve)
                 '((task_id . "first")))
        (funcall (plist-get (cdr (assoc "second launch" requests)) :resolve)
                 '((task_id . "second")))
        (should-not hermes-chat--background-tasks)
        (should (equal
                 (mapcar (lambda (entry)
                           (let ((meta (plist-get entry :metadata)))
                             (list (plist-get entry :content)
                                   (plist-get meta :number) (plist-get meta :preview))))
                         (seq-filter (lambda (entry) (eq (plist-get entry :role) 'background))
                                     (hermes-chat--entries)))
                 '(("second result" 2 "second launch")
                   ("unknown result" 0 "") ("first result" 1 "first launch"))))))))

(ert-deftest hermes-chat-background-late-receipt-preserves-successor ()
  (let (resolve reject recovery)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-background)
               (lambda (_client _content &rest args)
                 (setq resolve (plist-get args :resolve) reject (plist-get args :reject))))
              ((symbol-function 'display-buffer) #'ignore))
      (unwind-protect
          (hermes-test-with-dashboard-prompt-session (client)
            (hermes-chat-background "old launch")
            (hermes-chat-disconnect)
            (setq recovery hermes-chat--recovery-buffer)
            (insert "successor draft")
            (let ((before (hermes-chat--entries)))
              (funcall resolve '((task_id . "old")))
              (funcall reject "late failure")
              (should (equal before (hermes-chat--entries)))
              (should-not hermes-chat--background-tasks)
              (should (equal (hermes-chat-input-string) "successor draft"))))
        (when (buffer-live-p recovery) (kill-buffer recovery))))))

(ert-deftest hermes-chat-btw-wire-and-owned-completion ()
  "Real BTW frames settle once, independent of launch receipt and main turn."
  (dolist (early '(t nil))
    (hermes-test-with-dashboard-prompt-session (client)
      (let ((assistant hermes-chat--pending-assistant-id) requests)
        (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                   (lambda (_client method params resolve reject)
                     (push (list method params resolve reject) requests))))
          (insert "/btw same question") (hermes-chat-send)
          (insert "/btw same question") (hermes-chat-send)
          (insert "/bg independent") (hermes-chat-send)
          (setq requests (reverse requests))
          (should (equal (mapcar #'car requests)
                         '("prompt.btw" "prompt.btw" "prompt.background")))
          (should (equal (alist-get 'text (nth 1 (car requests))) "same question"))
          (should (equal (alist-get 'session_id (nth 1 (car requests))) "sid-prompt"))
          (insert "new draft")
          (cl-loop for task in '("btw-one" "btw-two") for request in requests do
                   (unless early (funcall (nth 2 request) `((task_id . ,task))))
                   (dotimes (_ 2)
                     (hermes-test--emit-dashboard-prompt
                      client "btw.complete" `((task_id . ,task) (text . "prior fact answer"))))
                   (when early (funcall (nth 2 request) `((task_id . ,task)))))
          (should (= 2 (length (seq-filter
                               (lambda (entry) (eq (plist-get entry :role) 'background))
                               (hermes-chat--entries)))))
          (should (equal assistant hermes-chat--pending-assistant-id))
          (should (equal (hermes-chat-input-string) "new draft")))))))

(defun hermes-test--side-question-documents ()
  "Return manual side-question recovery documents in this isolated test."
  (seq-filter (lambda (buffer)
                (string-prefix-p "*Hermes side question*" (buffer-name buffer)))
              (buffer-list)))

(ert-deftest hermes-chat-btw-failure-never-replays-on-main-settlement ()
  "Real admission failures retain literal text outside the ordinary FIFO."
  (dolist (failure '(rejected timeout malformed completed-before-rejection))
    (hermes-test-with-dashboard-prompt-session (client)
      (let* ((question "retain α\nsecond line")
             (history (copy-tree hermes-chat--input-history))
             (assistant hermes-chat--pending-assistant-id)
             frames main-sends
             (hermes-dashboard-transport-websocket-send-function
              (lambda (_socket frame) (push frame frames))))
        (unwind-protect
            (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-submit)
                       (lambda (&rest args) (push args main-sends))))
              (insert "/btw " question) (hermes-chat-send)
              (should (= 1 (hash-table-count (hermes-dashboard-transport-client-pending client))))
              (insert "newer draft")
              (let* ((id (car (hash-table-keys (hermes-dashboard-transport-client-pending client))))
                     (request (gethash id (hermes-dashboard-transport-client-pending client))))
                (when (eq failure 'completed-before-rejection)
                  (hermes-test--emit-dashboard-prompt client "btw.complete"
                    '((task_id . "early") (text . "side answer"))))
                (if (eq failure 'timeout)
                    (hermes-dashboard-transport--on-request-timeout client id)
                  (hermes-dashboard-transport--handle-frame
                   client (hermes-dashboard-transport--encode-frame
                           `((jsonrpc . "2.0") (id . ,id)
                             ,(if (eq failure 'malformed) '(result . ())
                                '(error . ((code . -32000) (message . "rejected"))))))))
                ;; A duplicate or conflicting receipt cannot create another copy.
                (funcall (plist-get request :reject) "late")
                (funcall (plist-get request :resolve) '((task_id . "late"))))
              (should (equal assistant hermes-chat--pending-assistant-id))
              (should-not hermes-chat--queued-messages)
              (should (= 1 (length (hermes-test--side-question-documents))))
              (with-current-buffer (car (hermes-test--side-question-documents))
                (should (equal question (buffer-string))))
              (should (string-search "manual copying only" (buffer-string)))
              (funcall (hermes-chat--submit-resolve-callback
                        (current-buffer) hermes-chat--unsettled-submit-context)
                       '((status . "streaming")))
              (hermes-test--emit-dashboard-prompt client "session.info" '((running . :false)))
              (hermes-test--emit-dashboard-prompt client "message.complete" '((text . "main answer")))
              (should-not hermes-chat--pending-assistant-id)
              (should-not main-sends)
              (should (equal (cons (concat "/btw " question) history)
                             hermes-chat--input-history))
              (should-not hermes-chat--queued-messages)
              (should (equal "newer draft" (hermes-chat-input-string)))
              (should (= 1 (length frames)))
              (should (equal "prompt.btw" (hermes-transport--get
                                           (hermes-dashboard-transport--decode-frame (car frames)) 'method)))
              (should (= (if (eq failure 'completed-before-rejection) 1 0)
                         (length (seq-filter (lambda (entry) (eq (plist-get entry :role) 'background))
                                             (hermes-chat--entries))))))
          (mapc #'kill-buffer (hermes-test--side-question-documents)))))))

(ert-deftest hermes-chat-btw-native-stop-recovery-and-replacement ()
  "Closed status precedes RPC rejection; recovery never needs live admission."
  (dolist (replacement '(nil newer session mode lifetime))
    (hermes-test-with-dashboard-prompt-session (client)
      (let ((hermes-dashboard-transport-websocket-send-function #'ignore))
        (unwind-protect
            (progn
              (insert "/btw exact side question") (hermes-chat-send)
              (let* ((pending (hermes-dashboard-transport-client-pending client))
                     (request (gethash (car (hash-table-keys pending)) pending)))
                (should (= 1 (hash-table-count pending)))
                (pcase replacement
                  ('session (setq hermes-chat--dashboard-active-session-id "successor"))
                  ('mode (fundamental-mode) (let ((inhibit-read-only t)) (erase-buffer)))
                  ('lifetime (hermes-chat--reset-transcript)))
                (when replacement (insert "successor draft"))
                ;; Replacement assertions concern the pending BTW callback, not
                ;; the old main-turn listener retained by this fixture.
                (when (memq replacement '(session mode lifetime))
                  (setf (hermes-dashboard-transport-client-callback client) #'ignore))
                (hermes-dashboard-transport-stop client "fixture closed" '(:type status :status "closed"))
                (funcall (plist-get request :reject) "late rejection")
                (should (= 0 (hash-table-count pending)))
                (if (memq replacement '(session mode lifetime))
                    (progn
                      (should-not (hermes-test--side-question-documents))
                      (should (string-suffix-p "successor draft" (buffer-string))))
                  (should-not hermes-chat--dashboard-active-session-id)
                  (should (= 1 (length (hermes-test--side-question-documents))))
                  (with-current-buffer (car (hermes-test--side-question-documents))
                    (should (equal "exact side question" (buffer-string))))
                  (should (equal (if replacement "successor draft" "")
                                 (hermes-chat-input-string))))
                (should-not hermes-chat--queued-messages)))
          (mapc #'kill-buffer (hermes-test--side-question-documents)))))))

(ert-deftest hermes-chat-profile-cwd-wire-intent ()
  "Omit unknown cwd and unsupported explicitness from released requests."
  (hermes-test-with-chat-buffer
   (let ((client (hermes-test--dashboard-client)) wire)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                (lambda (_client _method params &rest _) (setq wire params))))
       (dolist (choice '(unknown inherited explicit))
         (setq hermes-chat--profile "selected"
               hermes-chat--resolved-start-mode 'remote
               hermes-chat--working-directory (unless (eq choice 'unknown) "/selected")
               hermes-chat--cwd-explicit-p (eq choice 'explicit))
         (apply #'hermes-dashboard-transport-session-create client
                (hermes-chat--dashboard-create-params))
         (should (equal (alist-get 'profile wire) "selected"))
         (should-not (assq 'cwd_explicit wire))
         (if (eq choice 'explicit)
             (should (equal (alist-get 'cwd wire) "/selected"))
           (should-not (assq 'cwd wire))))))))

(ert-deftest hermes-chat-profile-cwd-inherited-create-paths ()
  "Prompt, control and queued bootstrap omit inherited cwd on the real wire."
  (dolist (path '(prompt control queued))
    (let ((client (hermes-test--dashboard-client)) wire action)
      (hermes-test-with-chat-buffer
       (setq hermes-chat--resolved-start-mode 'remote
             hermes-chat--profile "selected"
             hermes-chat--working-directory "/launch/inherited"
             hermes-chat--cwd-explicit-p nil
             hermes-chat--dashboard-client client)
       (let ((editor-directory default-directory))
         (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                    (lambda (_client method params resolve _reject)
                      (should (equal method "session.create"))
                      (setq wire params)
                      (funcall resolve
                               '((session_id . "selected-session")
                                 (info . ((cwd . "/selected/workspace")))))))
                   ((symbol-function 'hermes-dashboard-transport-api-request-async)
                    (lambda (&rest _) (ert-fail "No launch cwd HTTP lookup")))
                   ((symbol-function 'hermes-chat--dashboard-refresh-goal) #'ignore)
                   ((symbol-function 'hermes-dashboard-transport-prompt-submit)
                    (lambda (&rest _) (setq action t))))
           (if (eq path 'control)
               (hermes-chat--dashboard-ensure-session-action
                client (current-buffer) (lambda (_client) (setq action t)))
             (hermes-chat--dashboard-ensure-session
              client "literal prompt" (current-buffer) nil nil (eq path 'queued)))
           (should action)
           (should (equal (alist-get 'profile wire) "selected"))
           (should-not (assq 'cwd wire))
           (should-not (assq 'cwd_explicit wire))
           (should (equal hermes-chat--working-directory "/selected/workspace"))
           (should (equal default-directory editor-directory))
           (should (string-match-p "workspace" (buffer-name)))
           (should (equal (hermes-chat--directory-basename) "workspace"))))))))

(ert-deftest hermes-chat-profile-cwd-local-launch-is-deliberate ()
  "The public constructor retains deliberate local placement, not remote defaults."
  (dolist (mode '(spawn remote))
    (let ((default-directory "/deliberate/local/") buffer)
      (cl-letf (((symbol-function 'hermes-chat--instance-start-mode)
                 (lambda (_) mode))
                ((symbol-function 'pop-to-buffer-same-window) #'set-buffer))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat--new-buffer "selected" nil '("test" . "http://test")))
              (with-current-buffer buffer
                (should (eq hermes-chat--cwd-explicit-p (eq mode 'spawn)))
                (should (equal (plist-get (hermes-chat--dashboard-create-params) :cwd)
                               (and (eq mode 'spawn) "/deliberate/local/")))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-project-chat-sends-project-root-cwd ()
  "Project launches request their root only from same-host gateways."
  (pcase-dolist (`(,mode ,url ,expected)
                 '((spawn "http://test" "/project/root")
                   (remote "http://127.0.0.1:9119" "/project/root")
                   (remote "http://localhost:9119" "/project/root")
                   (remote "http://[::1]:9119" "/project/root")
                   (remote "http://100.64.0.2:9119" nil)))
    (let ((default-directory "/project/root/src/")
          (hermes-chat--project-chat-root "/project/root/")
          buffer)
      (cl-letf (((symbol-function 'hermes-chat--instance-start-mode)
                 (lambda (_) mode))
                ((symbol-function 'pop-to-buffer-same-window) #'set-buffer))
        (unwind-protect
            (progn
              (setq buffer (hermes-chat--new-buffer nil nil (cons "test" url)))
              (with-current-buffer buffer
                (should hermes-chat--cwd-explicit-p)
                (should (equal (plist-get (hermes-chat--dashboard-create-params) :cwd)
                               expected))
                (should (string-match-p "\\[root\\]" (buffer-name)))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest hermes-chat-btw-foreign-and-retired-frames-stay-quiet ()
  (hermes-test-with-dashboard-prompt-session (client)
    (let ((parent (current-buffer)) resolve)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-prompt-btw)
                 (lambda (_client _text &rest args) (setq resolve (plist-get args :resolve)))))
        (hermes-chat-btw "same question")
        (hermes-dashboard-transport--handle-frame
         client (hermes-dashboard-transport--encode-frame
                 '((jsonrpc . "2.0") (method . "event")
                   (params . ((type . "btw.complete") (session_id . "foreign")
                              (payload . ((task_id . "foreign") (text . "wrong"))))))))
        (should-not (seq-some (lambda (entry) (eq (plist-get entry :role) 'background))
                             (hermes-chat--entries)))
        (hermes-chat--reset-transcript)
        (setf (hermes-dashboard-transport-client-callback client) #'ignore)
        (insert "successor draft")
        (funcall resolve '((task_id . "old")))
        (hermes-test--emit-dashboard-prompt client "btw.complete"
          '((task_id . "old") (text . "retired")))
        (should (eq parent (current-buffer)))
        (should (equal (hermes-chat-input-string) "successor draft"))
        (should-not (seq-some (lambda (entry) (eq (plist-get entry :role) 'background))
                             (hermes-chat--entries)))))))

(defmacro hermes-test--with-worker-view (&rest body)
  "Run BODY in the public worker view with real wire dispatch and one worker."
  (declare (indent 0) (debug t))
  `(hermes-test--with-work-wire
     (hermes-chat-work-refresh)
     (hermes-test--work-answer
      client "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":1,\"goal\":\"Worker goal\"}]}")
     (let ((chat (current-buffer)) (owner hermes-chat--work-owner) tail-view)
       (unwind-protect
           (progn
             (call-interactively (keymap-lookup hermes-chat-actions-map "W"))
             (goto-char (point-min))
             (search-forward "Worker goal")
             (setq frames nil)
             ,@body)
         (when (buffer-live-p tail-view) (kill-buffer tail-view))
         (when (buffer-live-p (plist-get owner :view))
           (kill-buffer (plist-get owner :view)))
         (set-buffer chat)))))

(ert-deftest hermes-work-session-interrupt-native-key-and-receipt ()
  "The public key sends session and child once; found false is not success."
  (dolist (found '(t :false))
    (hermes-test--with-worker-view
      (let (messages)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (execute-kbd-macro "k")
          (should (= (length frames) 1))
          (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
            (should (equal (hermes-transport--get frame 'method) "subagent.interrupt"))
            (should (equal (hermes-transport--get frame 'params)
                           '((session_id . "runtime") (subagent_id . "child"))))
            (hermes-test--work-reply
             client (hermes-transport--get frame 'id)
             (if (eq found t) "{\"found\":true}" "{\"found\":false}")))
          (should (seq-some (lambda (text)
                              (string-match-p (if (eq found t) "accepted" "not found") text))
                            messages)))))))

(ert-deftest hermes-work-session-controls-refuse-retired-consent ()
  "Recursive confirmation/input cannot lend a replaced row, view or session."
  (dolist (command '(hermes-work-interrupt hermes-work-steer))
    (dolist (change '(row session connection mode association cancel))
      (hermes-test--with-worker-view
        (let* ((view (current-buffer)) entered
               (read (lambda (&rest _)
                       (setq entered t)
                       (pcase change
                         ('row (setf (plist-get (plist-get owner :delegates) :rows)
                                     (copy-tree (plist-get (plist-get owner :delegates) :rows))))
                         ('session (with-current-buffer chat
                                     (setq hermes-chat--dashboard-active-session-id "foreign")))
                         ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
                         ('mode (fundamental-mode))
                         ('association (set-visited-file-name
                                        (expand-file-name "worker-notes" temporary-file-directory) t)))
                       (if (eq command 'hermes-work-steer)
                           (if (eq change 'cancel) "" "Literal steering")
                         (not (eq change 'cancel))))))
          (cl-letf (((symbol-function 'read-string) read)
                    ((symbol-function 'yes-or-no-p) read))
            (call-interactively command))
          (should entered)
          (should-not frames)
          (with-current-buffer view (set-buffer-modified-p nil)))))))

(ert-deftest hermes-work-session-controls-recheck-authentication ()
  "A readiness wait rechecks the exact row and connection before serialization."
  (dolist (change '(nil row connection))
    (hermes-test--with-worker-view
      (let ((ready (hermes--promise-make)))
        (setf (hermes-dashboard-transport-client-ready-p client) nil
              (hermes-dashboard-transport-client-ready-promise client) ready)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Exact λ\ntext")))
          (execute-kbd-macro "s"))
        (should-not frames)
        (pcase change
          ('row (setf (plist-get (plist-get owner :delegates) :rows) nil))
          ('connection (cl-incf (hermes-dashboard-transport-client-generation client))))
        (setf (hermes-dashboard-transport-client-ready-p client) t)
        (hermes--promise-resolve ready t)
        (if change (should-not frames)
          (should (= (length frames) 1))
          (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
            (should (equal (hermes-transport--get frame 'method) "subagent.steer"))
            (should (equal (hermes-transport--get frame 'params)
                           '((session_id . "runtime") (subagent_id . "child")
                             (text . "Exact λ\ntext"))))))))))

(ert-deftest hermes-work-session-steering-queued-not-delivered ()
  "Display queued versus rejected receipts without implying delivery."
  (dolist (queued '(t nil))
    (hermes-test--with-worker-view
      (let (messages)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Please check"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (execute-kbd-macro "s")
          (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
            (hermes-test--work-reply client (hermes-transport--get frame 'id)
                                     (if queued "{\"status\":\"queued\"}" "{\"status\":\"rejected\"}")))
          (should (seq-some (lambda (text)
                              (string-match-p (if queued "queued (delivery not confirmed)"
                                                "rejected") text)) messages)))))))

(ert-deftest hermes-work-session-missing-authority-refuses-before-input ()
  "Absent session, finished or closed-steering rows never prompt or send."
  (dolist (change '(session finished steer))
    (hermes-test--with-worker-view
      (pcase change
        ('session (setf (plist-get owner :runtime) nil))
        ('finished (setf (plist-get (car (plist-get (plist-get owner :delegates) :rows)) :state) 'done))
        ('steer (setf (plist-get (car (plist-get (plist-get owner :delegates) :rows)) :accepting-steer) nil)))
      (let (entered)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (setq entered t) "text"))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq entered t))))
          (should-error (call-interactively (if (eq change 'steer) #'hermes-work-steer
                                               #'hermes-work-interrupt)) :type 'user-error))
        (should-not entered)
        (should-not frames)))))

(ert-deftest hermes-work-session-tail-native-open-refresh-and-flags ()
  "The native tail uses session/child, bounded receipts, and no guessed path."
  (hermes-test--with-worker-view
    (execute-kbd-macro "t")
    (setq tail-view (current-buffer))
    (should (derived-mode-p 'hermes-work-log-mode))
    (should-error (hermes-work-log-refresh) :type 'user-error)
    (dolist (result '("{\"subagent_id\":\"child\",\"available\":true,\"truncated\":true,\"text\":\"λ tail\"}"
                      "{\"subagent_id\":\"child\",\"available\":false,\"truncated\":false,\"text\":\"\"}"))
      (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
        (should (equal (hermes-transport--get frame 'method) "subagent.tail"))
        (should (equal (hermes-transport--get frame 'params)
                       '((session_id . "runtime") (subagent_id . "child"))))
        (hermes-test--work-reply client (hermes-transport--get frame 'id) result))
      (if (= (length frames) 1)
          (progn
            (should (equal (buffer-string) "λ tail"))
            (should (string-match-p "available: yes · truncated: yes" header-line-format))
            (execute-kbd-macro "g"))
        (should (string-match-p "Tail unavailable" (buffer-string)))
        (should (string-match-p "available: no · truncated: no" header-line-format))))))

(ert-deftest hermes-work-instance-inventory-is-read-only ()
  "Global inventory cannot supply the missing runtime control authority."
  (with-temp-buffer
    (hermes-subagents-mode)
    (let (sent prompted)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-request)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq prompted t))))
        (should-error (call-interactively #'hermes-subagents-interrupt) :type 'user-error))
      (should-not prompted)
      (should-not sent))))

(ert-deftest hermes-work-session-tail-retires-during-auth-and-reply ()
  "The tail cannot dispatch or repaint after row or native buffer retirement."
  (dolist (phase '(auth reply))
    (dolist (change '(row connection association))
      (hermes-test--with-worker-view
        (let ((ready (hermes--promise-make)))
          (when (eq phase 'auth)
            (setf (hermes-dashboard-transport-client-ready-p client) nil
                  (hermes-dashboard-transport-client-ready-promise client) ready))
          (execute-kbd-macro "t")
          (setq tail-view (current-buffer))
          (pcase change
            ('row (setf (plist-get (plist-get owner :delegates) :rows) nil))
            ('connection (cl-incf (hermes-dashboard-transport-client-generation client)))
            ('association
             (set-visited-file-name (expand-file-name "tail-notes" temporary-file-directory) t)
             (set-visited-file-name nil t)
             (setq header-line-format "User notes")
             (let ((inhibit-read-only t)) (insert "User draft"))))
          (let ((text (buffer-string)))
            (if (eq phase 'auth)
                (progn
                  (setf (hermes-dashboard-transport-client-ready-p client) t)
                  (hermes--promise-resolve ready t)
                  (should-not frames))
              (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
                (hermes-test--work-reply
                 client (hermes-transport--get frame 'id)
                 "{\"subagent_id\":\"child\",\"available\":true,\"truncated\":false,\"text\":\"Late tail\"}")))
            (should (equal text (buffer-string)))
            (should-not hermes-work-log--request)
            (should (string-match-p (if (eq change 'association) "User notes" "Retired.*reopen")
                                    header-line-format))
            (set-buffer-modified-p nil)))))))

(ert-deftest hermes-work-session-roster-preserves-unchanged-occurrence ()
  "Cadence retains unchanged rows but retires replaced worker occurrences."
  (hermes-test--with-worker-view
    (let* ((row (car (plist-get (plist-get owner :delegates) :rows)))
           (key (plist-get row :key)))
      (with-current-buffer chat
        (hermes-chat-work-refresh)
        (hermes-test--work-answer
         client "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":1,\"goal\":\"Worker goal\"}]}"))
      (should (eq row (car (plist-get (plist-get owner :delegates) :rows))))
      (with-current-buffer chat
        (hermes-chat-work-refresh)
        (hermes-test--work-answer
         client "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":2,\"goal\":\"Worker goal\"}]}"))
      (should (equal key (plist-get (car (plist-get (plist-get owner :delegates) :rows)) :key)))
      (should-not (hermes-work--action-current-p owner row)))))

(ert-deftest hermes-work-session-late-control-receipt-is-quiet ()
  "Late control receipts cannot report success into a successor view or owner."
  (dolist (change '(nil row connection mode))
    (hermes-test--with-worker-view
      (let (messages)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (execute-kbd-macro "k")
          ;; Emacs 32 clears the key echo with (message "").  Attribute it
          ;; to keyboard dispatch, never to a later operation receipt.
          (should (seq-every-p #'string-empty-p messages))
          (setq messages nil)
          (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
            (pcase change
              ('row (with-current-buffer chat
                      (hermes-chat-work-refresh)
                      (hermes-test--work-answer client "{\"subagents\":[]}")))
              ('connection (hermes-dashboard-transport-stop client))
              ('mode (fundamental-mode)))
            (when change
              (should (= (length messages) 1))
              (should (string-match-p "outcome unknown" (car messages))))
            (setq messages nil)
            (hermes-test--work-reply client (hermes-transport--get frame 'id)
                                     "{\"found\":true}"))
          (if change (should-not messages)
            (should (equal messages '("Hermes: worker interrupt accepted")))))))))

(ert-deftest hermes-work-session-tail-only-visibility-settles ()
  "Hiding the roster does not revoke an explicitly opened tail or its retry."
  (dolist (failure '(nil t))
    (hermes-test--with-worker-view
      (execute-kbd-macro "t")
      (setq tail-view (current-buffer))
      (execute-kbd-macro (kbd "C-x 1"))
      (hermes-chat--work-window-change (selected-window))
      (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
             (id (hermes-transport--get frame 'id)))
        (should (plist-get (plist-get owner :delegates) :rows))
        (if failure
            (hermes-dashboard-transport--on-request-timeout client id)
          (hermes-test--work-reply
           client id "{\"subagent_id\":\"child\",\"available\":true,\"truncated\":false,\"text\":\"Visible tail\"}"))
        (should-not hermes-work-log--request)
        (should (string-match-p (if failure "Failed" "available: yes") header-line-format))
        (unless failure (should (equal (buffer-string) "Visible tail")))
        (execute-kbd-macro "g")
        (should hermes-work-log--request)
        (should (= (length frames) 2))))))

(ert-deftest hermes-work-session-tail-retirement-settles-pending ()
  "Row replacement/removal and disconnect settle before any delayed reply."
  (dolist (change '(removed replaced disconnect))
    (hermes-test--with-worker-view
      (execute-kbd-macro "t")
      (setq tail-view (current-buffer))
      (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
             (id (hermes-transport--get frame 'id)))
        (with-current-buffer chat
          (if (eq change 'disconnect)
              (hermes-chat--forget-live-dashboard-session)
            (hermes-chat-work-refresh)
            (hermes-test--work-answer
             client (if (eq change 'removed) "{\"subagents\":[]}"
                      "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"started_at\":2}]}"))))
        (should-not hermes-work-log--request)
        (should-not (gethash id (hermes-dashboard-transport-client-pending client)))
        (should (string-match-p "Retired.*reopen" header-line-format))
        (let ((header header-line-format) (body (buffer-string)))
          (hermes-test--work-reply
           client id "{\"subagent_id\":\"child\",\"available\":true,\"truncated\":false,\"text\":\"Obsolete tail\"}")
          (should (equal header header-line-format))
          (should (equal body (buffer-string))))
        (should-error (hermes-work-log-refresh) :type 'user-error)))))

(ert-deftest hermes-work-session-poll-failure-preserves-user-requests ()
  "Observation timeout cannot consume a control/tail receipt or its deadline."
  (dolist (source '(:delegates :processes))
    (dolist (action '(interrupt steer tail))
      (dolist (outcome '(success refusal error))
	(hermes-test--with-worker-view
         (let ((hermes-chat--work-kinds (if (eq source :processes)
                                            '(:delegates :processes)
                                          '(:delegates)))
               messages)
           (with-current-buffer chat
             (hermes-chat-work-refresh)
             (when (eq source :processes)
               (hermes-test--work-answer
		client "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":1,\"goal\":\"Worker goal\"}]}")))
           (let* ((poll (hermes-test--work-request-id owner))
                  (pending (hermes-dashboard-transport-client-pending client)))
             (should (equal (plist-get (gethash poll pending) :method)
                            (if (eq source :processes) "process.list" "subagent.list")))
             (setq frames nil)
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'read-string) (lambda (&rest _) "Exact steering"))
                       ((symbol-function 'message)
			(lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
		      (execute-kbd-macro (pcase action ('interrupt "k") ('steer "s") ('tail "t")))
		      (when (eq action 'tail) (setq tail-view (current-buffer)))
		      (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
			     (id (hermes-transport--get frame 'id))
			     (request (gethash id pending))
			     (timer (plist-get request :timer)))
			(should timer)
			(hermes-dashboard-transport--on-request-timeout client poll)
			(should (eq request (gethash id pending)))
			(should (eq timer (plist-get (gethash id pending) :timer)))
			(if (eq outcome 'error)
			    (hermes-dashboard-transport--handle-frame
			     client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":4001,\"message\":\"Control failed\"}}" id))
			  (hermes-test--work-reply
			   client id
			   (pcase action
			     ('interrupt (if (eq outcome 'success) "{\"found\":true}" "{\"found\":false}"))
			     ('steer (if (eq outcome 'success) "{\"status\":\"queued\"}" "{\"status\":\"rejected\"}"))
			     ('tail (if (eq outcome 'success)
					"{\"subagent_id\":\"child\",\"available\":true,\"truncated\":false,\"text\":\"Independent tail\"}"
				      "{\"subagent_id\":\"child\",\"available\":false,\"truncated\":false,\"text\":\"\"}")))))
			(should-not (gethash id pending))
			(should (= (length frames) 1))
			(if (eq action 'tail)
			    (progn
			      (should-not hermes-work-log--request)
			      (should (string-match-p
				       (pcase outcome ('success "available: yes") ('refusal "available: no") ('error "Failed"))
				       header-line-format)))
			  (should (seq-some
				   (lambda (text)
				     (string-match-p
				      (if (eq outcome 'error) "Control failed"
					(pcase action
					  ('interrupt (if (eq outcome 'success) "accepted" "not found"))
					  ('steer (if (eq outcome 'success) "queued (delivery not confirmed)" "rejected"))))
				      text)) messages))))))))))))

(ert-deftest hermes-work-session-control-teardown-reports-uncertainty ()
  "Attachment teardown reports uncertainty once and cannot touch successors."
  (hermes-test--with-worker-view
    (let (messages)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (execute-kbd-macro "k")
        (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
               (id (hermes-transport--get frame 'id)))
          (with-current-buffer chat
            (hermes-chat--work-stop)
            (should (seq-some (lambda (text) (string-match-p "outcome unknown" text)) messages))
            (hermes-chat--work-bind client "runtime")
            (hermes-chat-work-refresh)
            (let* ((successor hermes-chat--work-owner)
                   (request (plist-get successor :inflight))
                   (before messages))
              (hermes-test--work-reply client id "{\"found\":true}")
              (hermes-dashboard-transport--on-request-timeout client id)
              (should (eq before messages))
              (should (eq request (plist-get successor :inflight)))
              (should (gethash (hermes-test--work-request-id successor)
                               (hermes-dashboard-transport-client-pending client))))))))))

(defun hermes-test--retire-worker-control (change chat client)
  "Apply native CHANGE to a control's view or CHAT and CLIENT authority."
  (pcase change
    ((or 'changed-row 'removed-row 'unchanged)
     (with-current-buffer chat
       (hermes-chat-work-refresh)
       (hermes-test--work-answer
        client
        (pcase change
          ('removed-row "{\"subagents\":[]}")
          ('changed-row "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":false,\"started_at\":1,\"goal\":\"Worker goal\"}]}")
          (_ "{\"subagents\":[{\"subagent_id\":\"child\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":1,\"goal\":\"Worker goal\"}]}")))))
    ('mode (fundamental-mode))
    ('kill (kill-buffer (current-buffer)))
    ('association
     (set-visited-file-name (expand-file-name "control-notes" temporary-file-directory) t)
     (set-visited-file-name nil t))
    ('connection (hermes-dashboard-transport-stop client))))

(ert-deftest hermes-work-session-control-terminal-ownership ()
  "Sent controls settle once despite roster/view retirement, never by replay."
  (dolist (action '(interrupt steer))
    (dolist (change '(unchanged changed-row removed-row mode kill association connection))
      (dolist (outcome '(success refusal error timeout))
        (hermes-test--with-worker-view
          (let ((view (current-buffer)) messages)
            (unwind-protect
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'read-string) (lambda (&rest _) "Exact steering"))
                          ((symbol-function 'message)
                           (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
                  (execute-kbd-macro (if (eq action 'interrupt) "k" "s"))
                  (should (seq-every-p #'string-empty-p messages))
                  (setq messages nil)
                  (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
                         (id (hermes-transport--get frame 'id))
                         (pending (hermes-dashboard-transport-client-pending client))
                         (request (gethash id pending)))
                    (should request)
                    (should (plist-get request :timer))
                    (hermes-test--retire-worker-control change chat client)
                    ;; Retirement has its own truthful outcome, before late delivery.
                    (if (eq change 'unchanged)
                        (progn (should-not messages) (should (eq request (gethash id pending))))
                      (should-not (gethash id pending))
                      (should (= (length messages) 1))
                      (should (string-match-p "outcome unknown" (car messages)))
                      (should (string-match-p "child.*runtime" (car messages)))
                      (should (string-match-p (symbol-name action) (car messages))))
                    (when (eq change 'changed-row)
                      (with-current-buffer view (should (hermes-work--selected-worker))))
                    (when (memq change '(mode association))
                      (with-current-buffer view
                        (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor draft"))
                        (setq header-line-format "Successor header")))
                    (setq messages nil)
                    (let ((text (and (buffer-live-p view) (with-current-buffer view (buffer-string))))
                          (header (and (buffer-live-p view) (buffer-local-value 'header-line-format view)))
                          (poll (plist-get owner :inflight)))
                      (pcase outcome
                        ('timeout (hermes-dashboard-transport--on-request-timeout client id))
                        ('error (hermes-dashboard-transport--handle-frame
                                 client (format "{\"jsonrpc\":\"2.0\",\"id\":%S,\"error\":{\"code\":4001,\"message\":\"Control failed\"}}" id)))
                        (_ (hermes-test--work-reply
                            client id (if (eq action 'interrupt)
                                          (if (eq outcome 'success) "{\"found\":true}" "{\"found\":false}")
                                        (if (eq outcome 'success) "{\"status\":\"queued\"}" "{\"status\":\"rejected\"}")))))
                      (should-not (gethash id pending))
                      (if (eq change 'unchanged)
                          (progn
                            (should (= (length messages) 1))
                            (should (string-match-p
                                     (pcase outcome
                                       ('error "Control failed") ('timeout "[Tt]ime")
                                       ('refusal (if (eq action 'interrupt) "not found" "rejected"))
                                       (_ (if (eq action 'interrupt) "accepted" "queued (delivery not confirmed)")))
                                     (car messages))))
                        (should-not messages))
                      (setq messages nil)
                      (hermes-test--work-reply client id "{\"found\":true}")
                      (hermes-dashboard-transport--on-request-timeout client id)
                      (should-not messages)
                      (should (eq poll (plist-get owner :inflight)))
                      (when (buffer-live-p view)
                        (should (equal text (with-current-buffer view (buffer-string))))
                        (should (equal header (buffer-local-value 'header-line-format view)))))
                    (should (= 1 (seq-count
                                  (lambda (wire)
                                    (equal (hermes-transport--get
                                            (hermes-dashboard-transport--decode-frame wire) 'method)
                                           (if (eq action 'interrupt) "subagent.interrupt" "subagent.steer")))
                                  frames)))))
              (when (buffer-live-p view)
                (with-current-buffer view (set-buffer-modified-p nil))
                (kill-buffer view)))))))))

(ert-deftest hermes-work-session-control-unsent-retirement ()
  "An authentication-retired control is not a dispatched uncertain operation."
  (dolist (change '(changed-row removed-row mode kill association connection))
    (hermes-test--with-worker-view
      (let ((view (current-buffer)) (ready (hermes--promise-make)) messages)
        (unwind-protect
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (setf (hermes-dashboard-transport-client-ready-p client) nil
                    (hermes-dashboard-transport-client-ready-promise client) ready)
              (execute-kbd-macro "k")
              (should-not frames)
              (should (seq-every-p #'string-empty-p messages))
              (setq messages nil)
              ;; Permit the roster read without releasing the held auth waiter.
              (setf (hermes-dashboard-transport-client-ready-p client) t)
              (hermes-test--retire-worker-control change chat client)
              (should (= (length messages) 1))
              (should (string-match-p "not sent" (car messages)))
              (should-not (string-match-p "outcome unknown" (car messages)))
              (setq messages nil)
              (hermes--promise-resolve ready t)
              (should-not messages)
              (should-not (seq-some
                           (lambda (wire) (equal "subagent.interrupt"
                                                 (hermes-transport--get
                                                  (hermes-dashboard-transport--decode-frame wire) 'method)))
                           frames)))
          (when (buffer-live-p view)
            (with-current-buffer view (set-buffer-modified-p nil))
            (kill-buffer view)))))))

(ert-deftest hermes-chat-dashboard-ready-startup-row-survives-turn-outcome ()
  "A startup row settled by readiness keeps its status when the turn fails."
  (hermes-test-with-dashboard-prompt-session (client)
    (let ((ready (hermes--promise-make))
          (start (hermes-dashboard-transport--start-event "127.0.0.1" 4567 nil))
          (row-status
           (lambda ()
             (plist-get (cl-find "Starting Hermes dashboard on 127.0.0.1:4567"
                                 (hermes-chat--entries)
                                 :key (lambda (entry) (plist-get entry :content))
                                 :test #'equal)
                        :status))))
      (setf (hermes-dashboard-transport-client-ready-promise client) ready)
      (hermes-dashboard-transport--dispatch-event client start)
      (hermes-dashboard-transport--settle-on-ready client start)
      (should (equal (funcall row-status) "starting"))
      (hermes--promise-resolve ready client)
      (should (equal (funcall row-status) "ready"))
      (hermes-dashboard-transport--dispatch-event
       client '(:type error :status "interrupted" :content "Interrupted"))
      (should-not hermes-chat--pending-assistant-id)
      (should (equal (funcall row-status) "ready")))))

;;;; Outline roster

(defun hermes-test--outline-keys (lines)
  "Return (KEY KIND DEPTH) for each outline line in LINES."
  (mapcar (lambda (line) (list (plist-get line :key) (plist-get line :kind) (plist-get line :depth)))
          lines))

(ert-deftest hermes-work-outline-tree-order-and-folds ()
  "Workers render depth first under their parents; folds hide subtrees."
  (let* ((rows (list (list :key '(delegate . "c") :id "c" :kind 'delegate :state 'running :parent "b" :goal "C"
                           :tools '(("read_file" . "x.el")) :tool-count 3)
                     (list :key '(delegate . "a") :id "a" :kind 'delegate :state 'running :goal "A")
                     (list :key '(delegate . "d") :id "d" :kind 'delegate :state 'queued :parent "a" :goal "D")
                     (list :key '(delegate . "b") :id "b" :kind 'delegate :state 'running :parent "a" :goal "B")
                     (list :key '(delegate . "e") :id "e" :kind 'delegate :state 'completed :parent "gone" :goal "E")))
         (roster (list :agents (hermes-chat--work-tree rows)
                       :processes (list (list :key '(process . "p") :id "p" :kind 'process
                                              :state 'running :command "make" :last-line "ok")))))
    (should (equal (hermes-test--outline-keys
                    (hermes-work--outline roster '(:observed t) 100 80))
                   '(((section . agents) section 0)
                     ((delegate . "a") heading 0)
                     ((delegate . "d") heading 1)
                     ((delegate . "b") heading 1)
                     ((delegate . "c") heading 2) ((delegate . "c") activity 3)
                     ((delegate . "e") heading 0)
                     (nil blank 0)
                     ((section . processes) section 0)
                     ((process . "p") heading 0) ((process . "p") activity 1))))
    (let ((lines (hermes-work--outline roster '(:folded ((delegate . "b"))) 100 80)))
      (should-not (assoc '(delegate . "c") (hermes-test--outline-keys lines)))
      (should (string-match-p "B \\+1" (substring-no-properties
                                       (plist-get (seq-find (lambda (l) (equal (plist-get l :key) '(delegate . "b"))) lines)
                                                  :text)))))
    (let ((lines (hermes-work--outline roster '(:folded ((section . agents))) 100 80)))
      (should (equal (mapcar #'car (hermes-test--outline-keys lines))
                     '((section . agents) nil (section . processes) (process . "p") (process . "p")))))
    (should (equal (substring-no-properties
                    (plist-get (car (hermes-work--outline nil '(:observed t) 0 80)) :text))
                   "No workers in this chat yet."))))

(ert-deftest hermes-work-outline-activity-folds-tool-events ()
  "One fold records tools and outcomes, also of snapshot-only workers."
  (let* ((a (hermes-chat--work-apply-event
             nil '(:event "subagent.tool" :subagent-id "w" :name "read_file"
                          :preview "x.el" :tool-count 2)
             10))
         (a (hermes-chat--work-apply-event
             a '(:event "subagent.complete" :subagent-id "w" :status "failed" :content "Boom")
             20)))
    (should (equal (cdr (assoc "w" a))
                   '(:seen 20 :state failed :finished 20 :row nil :goal nil
                           :tool-count 2 :tools (("read_file" . "x.el")) :error "Boom")))
    (should (eq a (hermes-chat--work-apply-event a '(:event "message.delta" :content "x") 30)))
    ;; A stateless entry keeps the snapshot's state and adds its tools.
    (let* ((b (hermes-chat--work-apply-event
               nil '(:event "subagent.tool" :subagent-id "s" :name "grep") 10))
           (row (car (hermes-chat--work-merge-agents
                      b '((:id "s" :state running :last-tool "old")) 10))))
      (should-not (plist-get (cdr (assoc "s" b)) :state))
      (should (eq (plist-get row :state) 'running))
      (should (equal (hermes-chat--work-last-tool row) '("grep")))
      ;; It is not an event-only worker, and it lives while listed.
      (should-not (hermes-chat--work-merge-agents b nil 10))
      (should (hermes-chat--work-prune b '((:id "s")) 500))
      (should-not (hermes-chat--work-prune b nil 500)))
    (should (equal (hermes-chat--work-last-tool '(:last-tool "terminal")) '("terminal")))
    (dotimes (i 10)
      (setq a (hermes-chat--work-apply-event
               a (list :event "subagent.tool" :subagent-id "w" :name (format "t%d" i)) 30)))
    (should (= 8 (length (plist-get (cdr (assoc "w" a)) :tools))))
    (should (equal "t9" (car (car (last (plist-get (cdr (assoc "w" a)) :tools))))))))

(defconst hermes-test--two-workers
  "{\"subagents\":[{\"subagent_id\":\"parent\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":1,\"goal\":\"Parent goal\"},{\"subagent_id\":\"child\",\"parent_id\":\"parent\",\"status\":\"running\",\"accepting_steer\":true,\"started_at\":2,\"goal\":\"Child goal\"},{\"subagent_id\":\"other\",\"status\":\"running\",\"started_at\":3,\"goal\":\"Other goal\"}]}"
  "A parent with one child, then a second root.")

(defmacro hermes-test--with-outline (&rest body)
  "Run BODY in the work view showing `hermes-test--two-workers' and one process."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (hermes-test--with-process-wire
       (hermes-chat-work-refresh)
       (hermes-test--work-answer client hermes-test--two-workers)
       (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"proc\",\"status\":\"running\",\"command\":\"make test\",\"uptime_seconds\":5,\"output_preview\":\"line one\\nline two\"}]}")
       (let ((chat (current-buffer)) (owner hermes-chat--work-owner) view)
         (unwind-protect
             (progn
               (hermes-chat-work)
               (setq view (current-buffer)
                     frames nil)
               (delete-other-windows)
               ,@body)
           (dolist (buffer (buffer-list))
             (when (eq owner (buffer-local-value 'hermes-work--owner buffer))
               (kill-buffer buffer)))
           (set-buffer chat))))))

(defun hermes-test--outline-repoll (chat client)
  "Answer a fresh roster read in CHAT through CLIENT, unchanged."
  (with-current-buffer chat
    (hermes-chat-work-refresh)
    (hermes-test--work-answer client hermes-test--two-workers)
    (hermes-test--work-answer client "{\"processes\":[{\"session_id\":\"proc\",\"status\":\"running\",\"command\":\"make test\",\"uptime_seconds\":9,\"output_preview\":\"line three\"}]}")))

(ert-deftest hermes-work-outline-navigation-keys ()
  "n/p walk headings, f/b siblings and ^ the parent."
  (hermes-test--with-outline
    (goto-char (point-min))
    (execute-kbd-macro "n")
    (should (equal (hermes-work--key-at-point) '(delegate . "parent")))
    (execute-kbd-macro "f")
    (should (equal (hermes-work--key-at-point) '(delegate . "other")))
    (execute-kbd-macro "b")
    (should (equal (hermes-work--key-at-point) '(delegate . "parent")))
    (execute-kbd-macro "n")
    (should (equal (hermes-work--key-at-point) '(delegate . "child")))
    (execute-kbd-macro "^")
    (should (equal (hermes-work--key-at-point) '(delegate . "parent")))
    (execute-kbd-macro "nnn")
    (should (equal (hermes-work--key-at-point) '(section . processes)))
    (should (string-match-p "↳ line two\\|line two" (buffer-string)))))

(ert-deftest hermes-work-outline-card-and-fold-survive-refresh ()
  "TAB cycles a parent through card and folded subtree across live repaints."
  (hermes-test--with-outline
    (goto-char (hermes-work--find-line '(delegate . "parent")))
    (execute-kbd-macro (kbd "TAB"))
    (should (hermes-work--find-line '(delegate . "parent") 'card 1))
    (hermes-test--outline-repoll chat client)
    (with-current-buffer view
      (should (hermes-work--find-line '(delegate . "parent") 'card 1))
      ;; The heading shows the whole goal, so the card does not repeat it.
      (should-not (string-match-p "Goal +Parent goal" (buffer-string)))
      (should (string-match-p "Id +parent" (buffer-string)))
      (execute-kbd-macro (kbd "TAB")))
    (should-not (hermes-work--find-line '(delegate . "parent") 'card 1))
    (should-not (hermes-work--find-line '(delegate . "child")))
    (should (string-match-p "Parent goal \\+1" (buffer-string)))
    (hermes-test--outline-repoll chat client)
    (with-current-buffer view
      (should-not (hermes-work--find-line '(delegate . "child")))
      (should (equal (hermes-work--key-at-point) '(delegate . "parent")))
      (execute-kbd-macro (kbd "TAB"))
      (should (hermes-work--find-line '(delegate . "child")))
      ;; A process card skips the command its heading shows whole; a section folds.
      (goto-char (hermes-work--find-line '(process . "proc")))
      (execute-kbd-macro (kbd "TAB"))
      (should (string-match-p "Id +proc" (buffer-string)))
      (should-not (string-match-p "Command" (buffer-string)))
      (goto-char (hermes-work--find-line '(section . agents)))
      (execute-kbd-macro (kbd "TAB"))
      (should-not (hermes-work--find-line '(delegate . "parent")))
      (hermes-test--outline-repoll chat client)
      (with-current-buffer view
        (should-not (hermes-work--find-line '(delegate . "parent")))
        (should (hermes-work--find-line '(process . "proc") 'card 1))))))

(ert-deftest hermes-work-outline-preserves-point-and-window-start ()
  "A repaint that inserts rows above keeps point and each window's first row."
  (hermes-test--with-outline
    (let ((window (selected-window)))
      (goto-char (hermes-work--find-line '(delegate . "other")))
      (execute-kbd-macro (kbd "TAB"))
      (goto-char (hermes-work--find-line '(delegate . "other") 'card 1))
      (forward-char 4)
      (set-window-start window (hermes-work--find-line '(delegate . "child")))
      (set-window-point window (point))
      (with-current-buffer chat
        (hermes-chat-work-refresh)
        (hermes-test--work-answer
         client (concat "{\"subagents\":["
                        (mapconcat (lambda (i)
                                     (format "{\"subagent_id\":\"new%d\",\"status\":\"running\",\"goal\":\"New %d\"}" i i))
                                   '(1 2 3) ",")
                        "," (substring hermes-test--two-workers (length "{\"subagents\":[")))))
      (with-current-buffer view
        (should (equal (car hermes-work--order) '(section . agents)))
        (should (equal (nth 1 hermes-work--order) '(delegate . "new1")))
        (should (equal (hermes-work--line-position (point))
                       (list '(delegate . "other") 'card 1 4
                             (line-number-at-pos (point) t))))
        (should (= (window-point window) (point)))
        (should (equal (get-text-property (window-start window) 'hermes-work-key)
                       '(delegate . "child")))
        (should (= (window-start window)
                   (hermes-work--find-line '(delegate . "child"))))))))

(ert-deftest hermes-work-outline-open-reuses-one-detail-buffer ()
  "RET opens one buffer per row, reused on every press and refreshed live."
  (hermes-test--with-outline
    (let (first second)
      (goto-char (hermes-work--find-line '(delegate . "child")))
      (execute-kbd-macro (kbd "RET"))
      (setq first (current-buffer))
      (should (derived-mode-p 'hermes-work-detail-mode))
      (should (string-match-p "\\`. Child goal" (buffer-string)))
      (should (string-match-p "Parent +Parent goal" (buffer-string)))
      (set-buffer view)
      (dotimes (_ 3)
        (goto-char (hermes-work--find-line '(delegate . "child")))
        (call-interactively #'hermes-work-open)
        (should (eq first (current-buffer)))
        (set-buffer view))
      (should (= 1 (seq-count (lambda (buffer)
                                (hermes-browser--buffer-mode-p buffer 'hermes-work-detail-mode))
                              (buffer-list))))
      (goto-char (hermes-work--find-line '(delegate . "parent")))
      (call-interactively #'hermes-work-open)
      (setq second (current-buffer))
      (should-not (eq first second))
      (should (string-match-p "Children (1)" (buffer-string)))
      ;; Live activity reaches the open detail without another RET.
      (with-current-buffer chat
        (hermes-chat--work-note-event
         '(:type tool :event "subagent.tool" :subagent-id "child" :name "grep" :preview "needle")))
      (with-current-buffer first
        (should (string-match-p "grep needle" (buffer-string)))))))

(ert-deftest hermes-work-outline-detail-controls-dispatch ()
  "Steer, interrupt and tail from a detail buffer name that exact worker."
  (dolist (key '("s" "k" "t"))
    (hermes-test--with-outline
      (goto-char (hermes-work--find-line '(delegate . "child")))
      (execute-kbd-macro (kbd "RET"))
      ;; Moving the roster's point must not retarget the detail's controls.
      (with-current-buffer view (goto-char (hermes-work--find-line '(delegate . "other"))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'read-string) (lambda (&rest _) "Exact")))
        (execute-kbd-macro key))
      (should (= (length frames) 1))
      (let ((frame (hermes-dashboard-transport--decode-frame (car frames))))
        (should (equal (hermes-transport--get frame 'method)
                       (pcase key ("s" "subagent.steer") ("k" "subagent.interrupt")
                              ("t" "subagent.tail"))))
        (should (equal (hermes-transport--get (hermes-transport--get frame 'params) 'subagent_id)
                       "child"))))))

(ert-deftest hermes-work-outline-process-kill-confirms ()
  "k on a process asks first, then sends process.kill and reports the receipt."
  (dolist (case '((nil nil nil)
                  (t "{\"status\":\"killed\"}" "process killed")
                  (t "{\"status\":\"already_exited\"}" "already exited")
                  (t 4044 "not owned by this chat")))
    (hermes-test--with-outline
      (let (prompt messages)
        (goto-char (hermes-work--find-line '(process . "proc")))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (text) (setq prompt text) (car case)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (execute-kbd-macro "k")
          (should (string-match-p "Kill process `make test'" prompt))
          (if (not (car case))
              (should-not frames)
            (should (= (length frames) 1))
            (let* ((frame (hermes-dashboard-transport--decode-frame (car frames)))
                   (id (hermes-transport--get frame 'id)))
              (should (equal (hermes-transport--get frame 'method) "process.kill"))
              (should (equal (hermes-transport--get frame 'params)
                             '((session_id . "runtime") (process_id . "proc"))))
              (if (numberp (nth 1 case))
                  (hermes-test--work-error client id (nth 1 case))
                (hermes-test--work-reply client id (nth 1 case))))
            (should (seq-some (lambda (text) (string-match-p (nth 2 case) text)) messages))))))))

(ert-deftest hermes-work-outline-empty-state-is-quiet ()
  "Loading is one shadowed line; an empty read adds a shadowed hint."
  (save-window-excursion
    (hermes-test--with-process-wire
      (let ((owner hermes-chat--work-owner))
        (unwind-protect
            (progn
              (hermes-chat-work)
              (should (equal (buffer-string) "Loading workers…\n"))
              (with-current-buffer (plist-get owner :buffer)
                (hermes-chat-work-refresh)
                (hermes-test--work-answer client "{\"subagents\":[]}")
                (hermes-test--work-answer client "{\"processes\":[]}"))
              (with-current-buffer (plist-get owner :view)
                (should (equal (buffer-string)
                               (concat "No workers in this chat yet.\n"
                                       "Delegated agents and background processes appear here as they start.\n")))
                (should (eq (get-text-property (point-min) 'face) 'shadow))
                (should (eq (get-text-property (- (point-max) 2) 'face) 'shadow))))
          (when (buffer-live-p (plist-get owner :view))
            (kill-buffer (plist-get owner :view))))))))

(defun hermes-test--line-text (lines key kind &optional n)
  "Return the text of KEY's line of KIND numbered N in outline LINES."
  (plist-get (seq-find (lambda (line)
                         (and (equal (plist-get line :key) key)
                              (eq (plist-get line :kind) kind)
                              (eql (plist-get line :n) (or n 0))))
                       lines)
             :text))

(defun hermes-test--face-of (text needle)
  "Return the face of the first NEEDLE in TEXT."
  (get-text-property (string-search needle text) 'face text))

(ert-deftest hermes-work-outline-calm-failures ()
  "Failures carry red in the glyph, an error line and an exit verdict only."
  (let* ((agent (list :key '(delegate . "w") :id "w" :kind 'delegate :state 'failed
                      :goal "Fix the parser" :model "claude-opus-5-5" :tool-count 3
                      :started 0 :finished 70 :error "Boom: parse failed\nTraceback …"))
         (process (list :key '(process . "p") :id "p" :kind 'process :state 'failed
                        :command "make check" :exit-code 7 :exited-at 90))
         (clean (list :key '(process . "q") :id "q" :kind 'process :state 'done
                      :command "make lint" :exit-code 0 :exited-at 95))
         (lines (hermes-work--outline (list :agents (list agent) :processes (list process clean))
                                      '(:observed t) 100 90))
         (heading (hermes-test--line-text lines '(delegate . "w") 'heading))
         (activity (hermes-test--line-text lines '(delegate . "w") 'activity))
         (failed (hermes-test--line-text lines '(process . "p") 'heading))
         (done (hermes-test--line-text lines '(process . "q") 'heading)))
    ;; D7: the glyph says failed; the meta stays quiet and aligned.
    (should (string-prefix-p "✗ Fix the parser" heading))
    (should (eq (get-text-property 0 'face heading) 'hermes-work-failed))
    (should (string-match-p "opus · 3 tools · 1:10\\'" heading))
    (should-not (string-match-p "failed" heading))
    (should (eq (hermes-test--face-of heading "opus") 'hermes-work-meta))
    ;; D3: the first error line, in the failed face.
    (should (equal (substring-no-properties activity) "↳ Boom: parse failed"))
    (should (eq (hermes-test--face-of activity "Boom") 'hermes-work-failed))
    ;; D8: a failed command stays readable; only its verdict is red.
    (should (eq (hermes-test--face-of failed "make check") 'default))
    (should (eq (hermes-test--face-of failed "exit 7") 'hermes-work-failed))
    (should (eq (hermes-test--face-of failed "10s ago") 'hermes-work-meta))
    (should (eq (hermes-test--face-of done "make lint") 'hermes-work-fading))
    (should (eq (hermes-test--face-of done "exit 0") 'hermes-work-meta))))

(ert-deftest hermes-work-outline-card-error-adds-to-the-arrow ()
  "A failed worker's card shows only the error lines its arrow line lacks."
  (let* ((card (lambda (error width)
                 (let ((lines (hermes-work--outline
                               (list :agents (list (list :key '(delegate . "w") :id "w"
                                                         :kind 'delegate :state 'failed
                                                         :goal "G" :error error)))
                               '(:observed t :cards ((delegate . "w"))) 0 width)))
                   (mapcar (lambda (line) (substring-no-properties (plist-get line :text)))
                           (seq-filter (lambda (line) (eq (plist-get line :kind) 'card))
                                       lines)))))
         (long "make: *** [Makefile:88: test] Error 1 — 2 unexpected results in the suite"))
    ;; One line shown whole on the arrow: no Error field.
    (should (equal (funcall card "Boom: parse failed" 80) '("Id       w")))
    ;; Multiline: the arrow's line is not repeated, the rest stays.
    (should (equal (funcall card "\n**Boom**: parse failed\n  at x.el:3\nTraceback …" 80)
                   '("Error    at x.el:3" "         Traceback …" "Id       w")))
    ;; The arrow cut the line short: the card keeps the whole error.
    (should (equal (car (funcall card long 60)) "Error    make: *** [Makefile:88: test] Error 1 — 2"))
    (should (equal (hermes-work--error-rest long 200) nil))))

(ert-deftest hermes-work-outline-shared-state-vocabulary ()
  "Rows use the shared glyphs, faces and m:ss clock."
  (dolist (case '((queued delegate "◌" hermes-work-queued)
                  (interrupted delegate "⊘" hermes-work-interrupted)
                  (mystery delegate "?" hermes-work-unknown)
                  (lost process "✗" hermes-work-failed)
                  (killed process "⊘" hermes-work-interrupted)
                  (running process "⚙" hermes-work-running)))
    (let* ((kind (nth 1 case))
           (row (list :key (cons kind "x") :id "x" :kind kind :state (car case)
                      :goal "G" :command "c" :uptime 75))
           (lines (hermes-work--outline
                   (if (eq kind 'process) (list :processes (list row)) (list :agents (list row)))
                   '(:observed t) 0 80))
           (text (hermes-test--line-text lines (cons kind "x") 'heading)))
      (should (string-prefix-p (nth 2 case) text))
      (should (eq (get-text-property 0 'face text) (nth 3 case)))))
  (should (string-match-p "1:15\\'" (hermes-test--line-text
                                    (hermes-work--outline
                                     (list :processes (list (list :key '(process . "p") :id "p" :kind 'process
                                                                  :state 'running :command "c" :uptime 75)))
                                     '(:observed t) 0 80)
                                    '(process . "p") 'heading))))

(ert-deftest hermes-work-outline-card-and-narrow-meta ()
  "Cards repeat a goal only when truncated; narrow rows drop the model first."
  (let* ((goal "Audit every worker surface for state vocabulary drift")
         (row (list :key '(delegate . "w") :id "w" :kind 'delegate :state 'running
                    :goal goal :model "claude-opus-5-5" :tool-count 9 :started 0
                    :tools '(("read_file" . "a") ("search_files" . "b") ("terminal" . "c"))))
         (roster (list :agents (list row)))
         (wide (hermes-work--outline roster '(:observed t :cards ((delegate . "w"))) 72 140))
         (narrow (hermes-work--outline roster '(:observed t :cards ((delegate . "w"))) 72 48)))
    (should (string-match-p "opus · 9 tools · 1:12\\'" (hermes-test--line-text wide '(delegate . "w") 'heading)))
    (should-not (seq-some (lambda (l) (string-match-p "\\`Goal" (plist-get l :text))) wide))
    (should (equal (substring-no-properties (hermes-test--line-text wide '(delegate . "w") 'card 1))
                   "Recent   read_file · search_files · terminal  (9 tools)"))
    (should (equal (substring-no-properties (hermes-test--line-text wide '(delegate . "w") 'card 2))
                   "Id       w"))
    ;; D14: room for the goal wins over the model, which goes first.
    (let ((heading (hermes-test--line-text narrow '(delegate . "w") 'heading)))
      (should-not (string-match-p "opus" heading))
      (should (string-match-p "9 tools · 1:12\\'" heading)))
    (should (string-match-p "\\`Goal +Audit" (hermes-test--line-text narrow '(delegate . "w") 'card 1)))))

(ert-deftest hermes-work-header-names-project-and-counts ()
  "The header names the project, mirrors the chat's count and adds failures."
  (let* ((owner (list :current-p #'always))
         (roster (list :agents (list '(:id "a" :kind delegate :state running)
                                     '(:id "b" :kind delegate :state queued)
                                     '(:id "c" :kind delegate :state failed)
                                     '(:id "d" :kind delegate :state completed))
                       :processes (list '(:id "p" :kind process :state running)
                                        '(:id "q" :kind process :state failed)
                                        '(:id "r" :kind process :state lost))))
         (header (hermes-work--header owner roster "emacs-hermes")))
    ;; Running and queued agents count alike, as in the chat header.
    (should (equal (substring-no-properties header)
                   (concat " emacs-hermes  "
                           (substring-no-properties (hermes-chat--work-label-text 2 1 nil))
                           "  ✗ 3   ? actions")))
    (should (string-match-p "  🤖 2 / ⚙ 1  \\|Agents 2" header))
    (should (eq (hermes-test--face-of header "emacs-hermes") 'font-lock-constant-face))
    (should (eq (hermes-test--face-of header "2") 'hermes-work-running))
    (should (eq (hermes-test--face-of header "3") 'hermes-work-failed))
    (should (eq (hermes-test--face-of header "?") 'help-key-binding))
    ;; The chat's link back to this view does not follow it here.
    (should-not (text-property-not-all 0 (length header) 'local-map nil header))
    (should-not (text-property-not-all 0 (length header) 'mouse-face nil header))
    (should (equal (substring-no-properties
                    (hermes-work--header owner (list :agents (list '(:id "c" :kind delegate
                                                                       :state failed)))
                                         "p"))
                   " p  ✗ 1   ? actions"))
    (should (string-match-p "\\` emacs-hermes  last known"
                            (hermes-work--header (list :current-p #'ignore) roster "emacs-hermes")))))

(ert-deftest hermes-work-outline-names-buffer-after-project ()
  "The Workers buffer and header carry the chat's project name."
  (hermes-test--with-outline
    (let ((project (with-current-buffer chat
                     (cdr (hermes-chat--project-identity (hermes-chat--naming-directory))))))
      (should (equal (buffer-name view) (format "*Hermes Workers: %s*" project)))
      (should (string-prefix-p (concat " " project "  ") (format "%s" header-line-format))))))

(ert-deftest hermes-work-detail-offers-only-applicable-actions ()
  "Detail headers list keys for actions that apply, derived from the map."
  (let ((header (lambda (row &optional log)
                  (substring-no-properties
                   (hermes-work--detail-header (plist-get row :key) row t log)))))
    (should (string-suffix-p "running   s steer  k interrupt  t tail  ? more"
                             (funcall header '(:key (delegate . "w") :kind delegate :state running
                                                    :accepting-steer t))))
    (should (string-suffix-p "running   k interrupt  t tail  ? more"
                             (funcall header '(:key (delegate . "w") :kind delegate :state running))))
    (should (string-suffix-p "done   L log  ? more"
                             (funcall header '(:key (delegate . "w") :kind delegate :state completed) t)))
    (should (string-suffix-p "failed   ? more"
                             (funcall header '(:key (delegate . "w") :kind delegate :state failed))))
    (should (string-suffix-p "running   k kill  ? more"
                             (funcall header '(:key (process . "p") :kind process :state running))))
    (should (string-suffix-p "done   ? more"
                             (funcall header '(:key (process . "p") :kind process :state done))))
    (let ((keymap (copy-keymap hermes-work-detail-mode-map)))
      (cl-letf (((symbol-value 'hermes-work-detail-mode-map) keymap))
        (keymap-set keymap "S" #'hermes-work-steer)
        (keymap-unset keymap "s" t)
        (should (string-match-p "S steer" (funcall header '(:key (delegate . "w") :kind delegate
                                                                  :state running :accepting-steer t))))))))

(ert-deftest hermes-work-detail-fields-are-calm ()
  "Details drop depth, the root parent and closed steering of ended workers."
  (let ((root '(:key (delegate . "r") :id "r" :kind delegate :state completed :depth 0
                     :goal "Root" :accepting-steer nil))
        (running '(:key (delegate . "s") :id "s" :kind delegate :state running :depth 1
                        :parent "r" :goal "Sub" :accepting-steer nil))
        (process '(:key (process . "p") :id "p" :kind process :state failed :exit-code 2
                        :command "make" :cwd "/remote/dir")))
    (let ((text (hermes-work--detail-text root (list root running) 100)))
      (should-not (string-match-p "depth\\|Parent\\|Steering" text))
      (should (string-match-p "1 child\\b" text)))
    (let ((text (hermes-work--detail-text running (list root running) 100)))
      (should (string-match-p "^Parent +Root$" text))
      (should (string-match-p "^Steering +closed$" text))
      (should-not (string-match-p "depth" text)))
    (let ((text (hermes-work--detail-text process nil 100)))
      (should (string-match-p "^Cwd +/remote/dir$" text))
      (should (string-match-p "failed · exit 2" text)))))

(ert-deftest hermes-work-detail-detached-is-last-known ()
  "A detail of a torn-down chat says last known and offers no actions."
  (hermes-test--with-outline
    (goto-char (hermes-work--find-line '(delegate . "child")))
    (execute-kbd-macro (kbd "RET"))
    (let ((detail (current-buffer)))
      (should (string-suffix-p "running   s steer  k interrupt  t tail  ? more"
                               (substring-no-properties header-line-format)))
      ;; The list need not survive for its details to learn of teardown.
      (kill-buffer view)
      (with-current-buffer chat (hermes-chat--work-bind client "runtime"))
      (hermes-work--render owner)
      (with-current-buffer detail
        (should (string-suffix-p "Worker child · last known · chat detached"
                                 (substring-no-properties (format "%s" header-line-format))))
        (should (eq (hermes-test--face-of header-line-format "last known") 'warning))
        (should-not (string-match-p "steer\\|interrupt\\|more" (format "%s" header-line-format)))
        (should (string-match-p "Child goal" (buffer-string)))))))

(defun hermes-test--retire-detail (change)
  "Apply native CHANGE to the current detail buffer."
  (pcase change
    ('mode (fundamental-mode))
    ('kill (kill-buffer (current-buffer)))
    ('association
     (set-visited-file-name (expand-file-name "detail-notes" temporary-file-directory) t)
     (set-buffer-modified-p nil)
     (set-visited-file-name nil t))))

(ert-deftest hermes-work-detail-controls-retire-with-presentation ()
  "Detail steer, interrupt and kill never dispatch after their detail retires.
Retirement during input refuses; during an authentication wait it settles
the operation unsent."
  (dolist (action '(steer interrupt kill))
    (dolist (change '(mode kill association))
      (dolist (phase '(input auth))
        (hermes-test--with-outline
          (goto-char (hermes-work--find-line (if (eq action 'kill) '(process . "proc")
                                               '(delegate . "child"))))
          (execute-kbd-macro (kbd "RET"))
          (let* ((detail (current-buffer)) (ready (hermes--promise-make)) entered messages
                 (method (pcase action ('steer "subagent.steer") ('interrupt "subagent.interrupt")
                                ('kill "process.kill")))
                 (read (lambda (&rest _)
                         (setq entered t)
                         (when (eq phase 'input) (hermes-test--retire-detail change))
                         (if (eq action 'steer) "Exact" t))))
            (when (eq phase 'auth)
              (setf (hermes-dashboard-transport-client-ready-p client) nil
                    (hermes-dashboard-transport-client-ready-promise client) ready))
            (cl-letf (((symbol-function 'yes-or-no-p) read)
                      ((symbol-function 'read-string) read)
                      ((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (execute-kbd-macro (if (eq action 'steer) "s" "k"))
              (should entered)
              (when (eq phase 'auth)
                (should-not frames)
                (with-current-buffer detail (hermes-test--retire-detail change))
                (should (seq-some (lambda (text) (string-match-p "not sent" text)) messages))
                (setf (hermes-dashboard-transport-client-ready-p client) t)
                (hermes--promise-resolve ready t)))
            (should-not (seq-some (lambda (wire)
                                    (equal method (hermes-transport--get
                                                   (hermes-dashboard-transport--decode-frame wire)
                                                   'method)))
                                  frames))
            (should-not (plist-get owner :controls))
            (when (buffer-live-p detail)
              (with-current-buffer detail (set-buffer-modified-p nil)))))))))

(ert-deftest hermes-work-detail-controls-survive-list-retirement ()
  "A detail's own control does not depend on the list it was opened from."
  (hermes-test--with-outline
    (goto-char (hermes-work--find-line '(delegate . "child")))
    (execute-kbd-macro (kbd "RET"))
    (kill-buffer view)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Exact")))
      (execute-kbd-macro "s"))
    (should (= (length frames) 1))
    (should (equal (hermes-transport--get (hermes-dashboard-transport--decode-frame (car frames))
                                          'method)
                   "subagent.steer"))))

(ert-deftest hermes-work-first-line-is-plain-text ()
  "Failure lines drop inline Markdown markup but keep the words."
  (should (equal (hermes-work--first-line "\n## **Build failed:** `make test` exited 2\nmore")
                 "Build failed: make test exited 2"))
  (should (equal (hermes-work--first-line "> - __Boom__ in `parse`") "Boom in parse"))
  (should (equal (hermes-work--first-line "a*b and 2 - 1") "a*b and 2 - 1"))
  (should-not (hermes-work--first-line " \n "))
  (should (equal (hermes-work--first-line "**\nBuild failed") "Build failed"))
  (let ((activity (hermes-test--line-text
                   (hermes-work--outline
                    (list :agents (list (list :key '(delegate . "w") :id "w" :kind 'delegate
                                              :state 'failed :goal "G" :error "**Boom**")))
                    '(:observed t) 0 80)
                   '(delegate . "w") 'activity)))
    (should (equal (substring-no-properties activity) "↳ Boom"))))

(ert-deftest hermes-work-queued-row-is-one-line ()
  "A queued worker says queued in its time column and adds no line."
  (let* ((lines (hermes-work--outline
                 (list :agents (list (list :key '(delegate . "q") :id "q" :kind 'delegate
                                           :state 'queued :goal "Wait" :model "claude-haiku-4-5")))
                 '(:observed t) 0 80))
         (heading (hermes-test--line-text lines '(delegate . "q") 'heading)))
    (should (string-match-p "haiku · queued\\'" heading))
    (should (eq (hermes-test--face-of heading "queued") 'hermes-work-meta))
    (should-not (hermes-test--line-text lines '(delegate . "q") 'activity))))

(ert-deftest hermes-work-failed-process-shows-its-last-line ()
  "A failed process shows its last output line in red, once; cards skip
a command the heading shows whole."
  (let* ((long "nix build .#hermes --no-link --print-build-logs --keep-going -L")
         (rows (list (list :key '(process . "f") :id "f" :kind 'process :state 'failed
                           :command "make" :exit-code 2 :last-line "error: boom")
                     (list :key '(process . "d") :id "d" :kind 'process :state 'done
                           :command long :exit-code 0 :last-line "built")))
         (lines (hermes-work--outline (list :processes rows)
                                      '(:observed t :cards ((process . "f") (process . "d")))
                                      0 40))
         (activity (hermes-test--line-text lines '(process . "f") 'activity))
         (cards (lambda (key)
                  (mapconcat (lambda (line) (substring-no-properties (plist-get line :text)))
                             (seq-filter (lambda (line) (and (equal (plist-get line :key) key)
                                                             (eq (plist-get line :kind) 'card)))
                                         lines)
                             "\n"))))
    (should (equal (substring-no-properties activity) "↳ error: boom"))
    (should (eq (hermes-test--face-of activity "error") 'hermes-work-failed))
    (should-not (string-match-p "Output\\|Command" (funcall cards '(process . "f"))))
    ;; A clean exit keeps its output in the card and repeats a truncated command.
    (should-not (hermes-test--line-text lines '(process . "d") 'activity))
    (should (string-match-p "^Command +nix build" (funcall cards '(process . "d"))))
    (should (string-match-p "^Output +built" (funcall cards '(process . "d"))))))

(ert-deftest hermes-work-card-keeps-text-lines ()
  "Card text keeps its own line breaks and fills each line separately."
  (should (equal (hermes-work--wrap "First line.\n\n  Second line.\n" 40)
                 '("First line." "Second line.")))
  (should (equal (hermes-work--wrap (concat (make-string 30 ?a) " " (make-string 30 ?b) "\nend")
                                    40)
                 (list (make-string 30 ?a) (make-string 30 ?b) "end")))
  (should (equal (length (hermes-work--wrap "a\nb\nc\nd\ne" 40)) 4)))

(ert-deftest hermes-work-detail-meta-and-sections-are-lean ()
  "Detail meta reads model, tools, time; no section repeats the latest tool."
  (let* ((row '(:key (delegate . "w") :id "w" :kind delegate :state running :model "claude-opus-5-5"
                     :tool-count 3 :started 0 :accepting-steer t
                     :tools (("read_file" . "a.el") ("grep" . "needle"))))
         (text (hermes-work--detail-text row (list row) 72)))
    (should (string-match-p "^running · opus · 3 tools · 1:12$" text))
    (should-not (string-match-p "^Now$" text))
    (should (string-match-p "^Tools (3)\n· grep needle\n· read_file a.el" text))))

(ert-deftest hermes-work-detached-outline-says-so-once ()
  "A detached outline says so in its header only."
  (hermes-test--with-outline
    (with-current-buffer chat (hermes-chat--work-bind client "runtime"))
    (hermes-work--render owner)
    (with-current-buffer view
      (should (string-match-p "last known" (format "%s" header-line-format)))
      (should-not (string-match-p "Detached\\|last known" (buffer-string)))
      (should (string-match-p "Parent goal" (buffer-string))))))

(ert-deftest hermes-work-detail-fills-to-the-window-with-hanging-indent ()
  "Long goals, fields and errors fill to the width, hanging under their text.
Each source line keeps its own indentation, and nothing exceeds the width."
  (let* ((goal "Audit hermes-subagents.el for stale-owner races in the control path and report")
         (parent (list :id "p" :kind 'delegate :state 'running :goal goal))
         (row (list :key '(delegate . "w") :id "w" :kind 'delegate :state 'failed
                    :parent "p" :goal goal
                    :error "make: *** [Makefile:88: test] Error 1 — 2 unexpected results in the suite\n  at x.el:3"))
         (lines (split-string (substring-no-properties
                               (hermes-work--detail-text row (list parent row) 100 50))
                              "\n")))
    (should (seq-every-p (lambda (line) (<= (string-width line) 50)) lines))
    (should (equal (seq-take lines 2)
                   '("✗ Audit hermes-subagents.el for stale-owner races"
                     "  in the control path and report")))
    (should (member "Parent    Audit hermes-subagents.el for" lines))
    (should (member "          stale-owner races in the control path" lines))
    (should (member "make: *** [Makefile:88: test] Error 1 — 2" lines))
    (should (member "  at x.el:3" lines))
    ;; The goal's continuation keeps the bold face of its first line.
    (let ((text (hermes-work--detail-text row (list parent row) 100 50)))
      (should (eq (get-text-property (string-search "in the control" text) 'face text)
                  'bold)))))

(ert-deftest hermes-work-detail-resize-spares-a-file-associated-buffer ()
  "A detail retired by a visited file keeps its text and header on resize."
  (let* ((row (list :key '(delegate . "w") :id "w" :kind 'delegate :state 'running
                    :goal "Original worker goal" :accepting-steer t))
         (roster (list :agents (list row)))
         (buffer (hermes-work--detail-buffer (list :runtime "r") '(delegate . "w")))
         (file (make-temp-file "hermes-detail-")))
    (unwind-protect
        (cl-letf (((symbol-function 'hermes-chat--work-roster) (lambda (&rest _) roster)))
          (with-current-buffer buffer
            (hermes-work--detail-render roster 0)
            (set-visited-file-name file t)
            (should-not (hermes-buffer--owned-p))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "UNSAVED FILE DRAFT\n"))
            (setq header-line-format "Draft header"
                  hermes-work--width 1)
            (hermes-work--resize (selected-window))
            (should (equal (buffer-string) "UNSAVED FILE DRAFT\n"))
            (should (equal header-line-format "Draft header"))
            (should-not (memq #'hermes-work--resize window-size-change-functions))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest hermes-work-detail-reflow-keeps-each-reader-on-its-text ()
  "Narrowing and widening refill a long goal, yet each window stays on its text."
  (let* ((row (list :key '(delegate . "w") :id "w" :kind 'delegate :state 'failed
                    :goal (mapconcat #'identity (make-list 24 "lengthy") " ")
                    :error "First failure line\nERROR-ANCHOR details follow"
                    :summary "SUMMARY-ANCHOR closing words"))
         (roster (list :agents (list row)))
         (buffer (hermes-work--detail-buffer (list :runtime "r") '(delegate . "w")))
         (width 80))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buffer)
          (let ((other (split-window)))
            (set-window-buffer other buffer)
            (cl-letf (((symbol-function 'hermes-chat--work-roster) (lambda (&rest _) roster))
                      ((symbol-function 'window-body-width) (lambda (&rest _) width)))
              (hermes-work--detail-render roster 0)
              (goto-char (+ (string-search "ERROR-ANCHOR" (buffer-string)) 7))
              (set-window-point other (+ (string-search "SUMMARY-ANCHOR" (buffer-string)) 4))
              (set-window-start other (+ (string-search "Summary\n" (buffer-string)) 1))
              (dolist (next '(40 100 30 80))
                (setq width next)
                (hermes-work--resize other)
                (should (eql hermes-work--width next))
                (should (looking-at-p "ANCHOR details"))
                (should (save-excursion (goto-char (window-point other))
                                        (looking-at-p "MARY-ANCHOR closing")))
                (should (save-excursion (goto-char (window-start other))
                                        (looking-at-p "Summary$")))))))
      (kill-buffer buffer))))

(defun hermes-test--offered (rows key &optional detail log current)
  "Return the actions the popup offers for KEY among ROWS.
DETAIL asks from a detail buffer; LOG names a published log; CURRENT
defaults to an attached chat."
  (cl-letf (((symbol-function 'hermes-work--observations) (lambda (_) rows))
            ((symbol-function 'hermes-work--log-path) (lambda (_ _) log)))
    (with-temp-buffer
      (if detail (hermes-work-detail-mode) (hermes-work-mode))
      (setq hermes-work--owner (list :current-p (if (eq current 'detached) #'ignore #'always)))
      (if detail (setq hermes-work--detail-key key)
        (let ((inhibit-read-only t))
          (hermes-work--insert (hermes-work--outline
                                (list :agents (seq-filter (lambda (row) (eq (plist-get row :kind) 'delegate)) rows)
                                      :processes (seq-filter (lambda (row) (eq (plist-get row :kind) 'process)) rows))
                                '(:observed t) 0 80)))
        (goto-char (hermes-work--find-line key)))
      (seq-filter #'hermes-work--offers-p
                  '(hermes-work-steer hermes-work-interrupt hermes-work-tail hermes-work-log)))))

(ert-deftest hermes-work-popup-offers-only-applicable-actions ()
  "Both popups offer exactly the actions the row's state allows."
  (let ((rows (list '(:key (delegate . "r") :id "r" :kind delegate :state running :goal "R"
                           :accepting-steer t)
                    '(:key (delegate . "c") :id "c" :kind delegate :state running :goal "C")
                    '(:key (delegate . "q") :id "q" :kind delegate :state queued :goal "Q")
                    '(:key (delegate . "d") :id "d" :kind delegate :state completed :goal "D")
                    '(:key (delegate . "f") :id "f" :kind delegate :state failed :goal "F")
                    '(:key (process . "p") :id "p" :kind process :state running :command "make")
                    '(:key (process . "x") :id "x" :kind process :state failed :command "make"
                           :exit-code 2))))
    (dolist (detail '(nil t))
      (should (equal (hermes-test--offered rows '(delegate . "r") detail "/log")
                     '(hermes-work-steer hermes-work-interrupt hermes-work-tail)))
      (should (equal (hermes-test--offered rows '(delegate . "c") detail)
                     '(hermes-work-interrupt hermes-work-tail)))
      (should-not (hermes-test--offered rows '(delegate . "q") detail))
      (should-not (hermes-test--offered rows '(delegate . "d") detail))
      (should (equal (hermes-test--offered rows '(delegate . "f") detail "/log")
                     '(hermes-work-log)))
      (should (equal (hermes-test--offered rows '(process . "p") detail)
                     '(hermes-work-interrupt)))
      (should-not (hermes-test--offered rows '(process . "x") detail))
      (should-not (hermes-test--offered rows '(delegate . "r") detail nil 'detached)))
    (should-not (hermes-test--offered rows '(section . agents)))
    (should-not (hermes-test--offered rows '(section . processes)))))

(ert-deftest hermes-chat-directory-browser-uses-captured-profile ()
  "File listing uses the chat profile rather than the launch default."
  (hermes-test-with-chat-buffer
   (let ((client (hermes-test--dashboard-client)) query)
     (setq hermes-chat--profile "remote-work"
           hermes-chat--dashboard-client client
           hermes-chat--dashboard-session-ready-p t
           hermes-chat--dashboard-active-session-id "sid")
     (cl-letf (((symbol-function 'hermes-dashboard-transport-api-request-async)
                (lambda (_method _path &rest args)
                  (setq query (plist-get args :query))
                  (hermes--promise-make))))
       (hermes-chat--browse-instance-directory client "/srv")
       (should (equal query '((path . "/srv") (profile . "remote-work"))))))))

(ert-deftest hermes-chat-work-persisted-failures-are-read-only ()
  "Failed tasks survive fresh roster reads and cannot send live controls."
  (hermes-test--with-worker-view
    (with-current-buffer chat
      (hermes-chat-work-refresh)
      (hermes-test--work-answer
       client "{\"subagents\":[],\"delegations\":[{\"delegation_id\":\"batch\",\"task_index\":0,\"status\":\"failed\",\"goal\":\"Check compatibility\",\"error\":\"Cannot read module\",\"dispatched_at\":10,\"completed_at\":20},{\"delegation_id\":\"batch\",\"task_index\":1,\"status\":\"error\",\"goal\":\"Check rendering\",\"error\":\"Render failed\",\"dispatched_at\":10,\"completed_at\":25}]}")
      (let* ((rows (plist-get (hermes-chat--work-roster owner (float-time)) :agents))
             (failures (seq-filter (lambda (row) (eq (plist-get row :state) 'failed)) rows)))
        (should (= (length failures) 2))
        (should-not (equal (plist-get (car failures) :key) (plist-get (cadr failures) :key)))
        (should (equal (mapcar (lambda (row) (plist-get row :finished)) failures) '(20 25)))
        (should (= (plist-get (car failures) :started) 10))))
    (goto-char (point-min))
    (search-forward "Check compatibility")
    (should (string-match-p "Cannot read module" (buffer-string)))
    (let ((before (length frames)))
      (dolist (command '(hermes-work-steer hermes-work-interrupt hermes-work-tail))
        (should-error (call-interactively command) :type 'user-error))
      (should (= before (length frames))))))

(ert-deftest hermes-chat-work-persisted-failures-follow-live-workers ()
  "A persisted failure follows live workers unless one already shows its task."
  (let* ((now 1000.0)
         (event (lambda (name status index)
                  (list :type 'event :event name :subagent-id (format "sa-%d" index)
                        :delegation-id "batch" :task-index index :status status
                        :content "Goal")))
         (events (seq-reduce (lambda (events args)
                               (hermes-chat--work-apply-event
                                events (apply event args) now))
                             '(("subagent.start" nil 0)
                               ("subagent.complete" "failed" 0))
                             nil))
         (failure (lambda (index)
                    (list :key (cons 'delegate (format "delegation:batch:%d" index))
                          :id (format "delegation:batch:%d" index) :kind 'delegate
                          :persisted t :state 'failed :delegation-id "batch"
                          :task-index index :goal "Goal")))
         (owner (list :runtime "sid" :events events
                      :delegates (list :sid "sid" :rows nil
                                       :failures (list (funcall failure 0)
                                                       (funcall failure 1)))))
         (agents (plist-get (hermes-chat--work-roster owner now) :agents)))
    (should (equal (mapcar (lambda (row) (plist-get row :id)) agents)
                   '("sa-0" "delegation:batch:1")))
    ;; Once the live worker leaves the roster, its persisted failure remains.
    (should (equal (mapcar (lambda (row) (plist-get row :id))
                           (plist-get (hermes-chat--work-roster owner (+ now 3600)) :agents))
                   '("delegation:batch:0" "delegation:batch:1")))
    ;; A persisted failure from another runtime is not shown.
    (setf (plist-get (plist-get owner :delegates) :sid) "old")
    (should (equal (mapcar (lambda (row) (plist-get row :id))
                           (plist-get (hermes-chat--work-roster owner now) :agents))
                   '("sa-0")))))

(ert-deftest hermes-chat-work-persisted-failure-matches-live-snapshot-worker ()
  "A live worker's task index from its events dedupes its persisted failure.
The live roster omits task indexes, so only the worker's events link it
to the persisted task."
  (let* ((now 1000.0)
         (all (hermes-transport-work-subagents
               (hermes-transport-json-parse-lossless
                "{\"subagents\":[{\"subagent_id\":\"sa-0\",\"delegation_id\":\"batch\",\"status\":\"running\",\"goal\":\"Check compatibility\",\"started_at\":10}],\"delegations\":[{\"delegation_id\":\"batch\",\"task_index\":0,\"status\":\"failed\",\"goal\":\"Check compatibility\",\"error\":\"Cannot read module\",\"dispatched_at\":10,\"completed_at\":20}]}")))
         (events (seq-reduce
                  (lambda (events event) (hermes-chat--work-apply-event events event now))
                  (list (list :type 'event :event "subagent.start" :subagent-id "sa-0"
                              :delegation-id "batch" :task-index 0
                              :content "Check compatibility")
                        (list :type 'event :event "subagent.complete" :subagent-id "sa-0"
                              :delegation-id "batch" :task-index 0 :status "failed"
                              :content "Check compatibility"))
                  nil))
         (owner (list :runtime "sid" :events events
                      :delegates (list :sid "sid"
                                       :rows (seq-remove (lambda (row) (plist-get row :persisted)) all)
                                       :failures (seq-filter (lambda (row) (plist-get row :persisted)) all)))))
    (should (equal (mapcar (lambda (row) (list (plist-get row :id) (plist-get row :state)))
                           (plist-get (hermes-chat--work-roster owner now) :agents))
                   '(("sa-0" failed))))))

(ert-deftest hermes-work-persisted-failure-meta-says-when ()
  "A persisted failure reports its run time and how long ago it ended."
  (let ((row (list :kind 'delegate :state 'failed :started 100 :finished 400)))
    (should (equal (mapcar #'substring-no-properties
                           (hermes-work--agent-meta (append '(:persisted t) row) 4000))
                   (list (hermes-work-duration 300) (hermes-work-ago 3600))))
    (should (equal (mapcar #'substring-no-properties (hermes-work--agent-meta row 4000))
                   (list (hermes-work-duration 300))))))

(provide 'hermes-chat-dashboard-tests)
;;; hermes-chat-dashboard-tests.el ends here
