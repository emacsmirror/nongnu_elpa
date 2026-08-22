;;; hermes-chat-handoff-tests.el --- session handoff tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-handoff': the handoff command's guards, the
;; target prompt, and the backoff-polled `handoff.state' watcher.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(defun hermes-test--own-handoff-poll (poll)
  "Install POLL with a matching handoff owner in the current buffer."
  (let ((id (or (plist-get poll :id) (gensym "test-handoff-"))))
    (setq hermes-chat--handoff-owner id
          hermes-chat--handoff-poll
          (plist-put (copy-sequence poll) :id id))))

(ert-deftest hermes-chat-handoff-status-classifies-as-active ()
  "The `handoff' status reads as active with a distinct header label."
  (should (hermes-chat--active-status-p 'handoff))
  (should-not (hermes-chat--finished-status-p 'handoff))
  (should (equal (hermes-chat--header-status-label 'handoff) "Handing off")))

(ert-deftest hermes-chat-handoff-requires-attached-session ()
  "Handoff errors when the chat has no attached dashboard session."
  (hermes-test-with-chat-buffer
   (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
              (lambda () nil)))
     (should-error (hermes-chat-handoff "telegram") :type 'user-error))))

(ert-deftest hermes-chat-handoff-refuses-during-active-turn ()
  "Handoff errors while a turn is still active."
  (hermes-test-with-chat-buffer
   (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
              (lambda () t))
             ((symbol-function 'hermes-chat--active-turn-p) (lambda () t)))
     (should-error (hermes-chat-handoff "telegram") :type 'user-error))))

(ert-deftest hermes-chat-handoff-sends-platform-and-session ()
  "Handoff submits the lowercased platform and live session id."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-9")
   (let (args)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-chat--handoff-start-poll) #'ignore)
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (_client platform &rest rest)
                  (setq args (cons platform rest)))))
       (hermes-chat-handoff "Telegram"))
     (should (equal (car args) "telegram"))
     (should (equal (plist-get (cdr args) :session-id) "sid-9")))))

(ert-deftest hermes-chat-handoff-request-exclusively-owns-submission ()
  "An in-flight handoff request blocks every submission path without losing input."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-owner")
   (let (submitted slash-executed request-reject)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (_client _platform &rest args)
                  (setq request-reject (plist-get args :reject))))
               ((symbol-function 'hermes-chat--submit-through-transport)
                (lambda (&rest _) (setq submitted t)))
               ((symbol-function 'hermes-chat--dashboard-slash-exec)
                (lambda (&rest _) (setq slash-executed t))))
       (insert "draft")
       (hermes-chat-handoff "telegram")
       (should (hermes-chat--active-turn-p))
       (should-error (hermes-chat-send) :type 'user-error)
       (should-error (hermes-chat-queue-message) :type 'user-error)
       (should-error (hermes-chat--submit-content "programmatic")
                     :type 'user-error)
       (should-error (hermes-chat--handle-slash-content "/models")
                     :type 'user-error)
       (should-error (hermes-chat-handoff "discord") :type 'user-error)
       (should (equal (hermes-chat-input-string) "draft"))
       (should-not submitted)
       (should-not slash-executed)
       (funcall request-reject "rejected")
       (should-not (hermes-chat--active-turn-p))))))

(ert-deftest hermes-chat-handoff-blocked-steer-preserves-draft ()
  "A session owner rejects steering before deleting the writable draft."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--command-owner 'command-owner)
   (let (queued submitted)
     (cl-letf (((symbol-function 'hermes-chat--queue-content)
                (lambda (&rest _) (setq queued t)))
               ((symbol-function 'hermes-chat--submit-through-transport)
                (lambda (&rest _) (setq submitted t))))
       (insert "valuable draft")
       (should-error (hermes-chat-steer-message) :type 'user-error)
       (should (equal (hermes-chat-input-string) "valuable draft"))
       (should-not queued)
       (should-not submitted)))))

(ert-deftest hermes-chat-handoff-refuses-in-flight-slash-operation ()
  "An earlier slash RPC owns the session until its callback settles."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
         hermes-chat--dashboard-active-session-id "sid-command"
         hermes-chat--dashboard-session-ready-p t)
   (let (slash-resolve handoff-requested)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
                (lambda (_client _raw &rest args)
                  (setq slash-resolve (plist-get args :resolve))))
               ((symbol-function 'hermes-chat--handle-command-result) #'ignore)
               ((symbol-function 'hermes-chat--refresh-state-after-command) #'ignore)
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (&rest _) (setq handoff-requested t))))
       (hermes-chat--dashboard-slash-exec "foo" "" "foo")
       (should-error (hermes-chat-handoff "telegram") :type 'user-error)
       (should-not handoff-requested)
       (funcall slash-resolve '((status . "ok")))
       (hermes-chat-handoff "telegram")
       (should handoff-requested)))))

(ert-deftest hermes-chat-handoff-slash-fallback-throw-releases-operation ()
  "A synchronous command fallback failure releases exact session ownership."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
         hermes-chat--dashboard-active-session-id "sid-fallback"
         hermes-chat--dashboard-session-ready-p t)
   (let (slash-reject handoff-requested)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-slash-exec)
                (lambda (_client _raw &rest args)
                  (setq slash-reject (plist-get args :reject))))
               ((symbol-function 'hermes-dashboard-transport-command-dispatch)
                (lambda (&rest _) (error "timer setup failed synchronously")))
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (&rest _) (setq handoff-requested t))))
       (hermes-chat--dashboard-slash-exec "foo" "" "foo")
       (should hermes-chat--command-owner)
       (should-error (funcall slash-reject "unsupported"))
       (should-not hermes-chat--command-owner)
       (should-not (hermes-chat--submit-inhibit-reason))
       (hermes-chat-handoff "telegram")
       (should handoff-requested)))))

(ert-deftest hermes-chat-handoff-disconnect-clears-owner-and-stales-callbacks ()
  "Disconnect releases handoff ownership before transport teardown."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-client (hermes-test--dashboard-client)
         hermes-chat--dashboard-active-session-id "sid-disconnect"
         hermes-chat--dashboard-session-ready-p t)
   (let (resolve reject)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (_client _platform &rest args)
                  (setq resolve (plist-get args :resolve)
                        reject (plist-get args :reject))))
               ((symbol-function 'hermes-dashboard-transport-cancel-owner-requests)
                #'ignore)
               ((symbol-function 'hermes-dashboard-transport-release) #'ignore))
       (insert "draft")
       (hermes-chat-handoff "telegram")
       (should hermes-chat--handoff-owner)
       (hermes-chat-disconnect)
       (should-not hermes-chat--handoff-owner)
       (should-not hermes-chat--handoff-poll)
       (should-not hermes-chat--dashboard-active-session-id)
       (should (equal (hermes-chat-input-string) "draft"))
       (should-not (hermes-chat--submit-inhibit-reason))
       (funcall resolve '((queued . t)))
       (funcall reject "late rejection")
       (should-not hermes-chat--handoff-owner)
       (should-not hermes-chat--handoff-poll)))))

(ert-deftest hermes-chat-handoff-poll-owns-until-terminal-state ()
  "A queued handoff blocks submission until its terminal state settles."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-poll")
   (let (submitted)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (_client _platform &rest args)
                  (funcall (plist-get args :resolve) '((queued . t)))))
               ((symbol-function 'run-at-time) (lambda (&rest _) (timer-create)))
               ((symbol-function 'hermes-chat--submit-through-transport)
                (lambda (&rest _) (setq submitted t))))
       (hermes-chat-handoff "telegram")
       (should hermes-chat--handoff-poll)
       (should (hermes-chat--active-turn-p))
       (should-error (hermes-chat--submit-content "blocked") :type 'user-error)
       (hermes-chat--handoff-handle-state
        (current-buffer) '((state . "completed")))
       (should-not (hermes-chat--active-turn-p))
       (should (hermes-chat--submit-content "allowed"))
       (should submitted)))))

(ert-deftest hermes-chat-handoff-handle-state-completed-stops ()
  "A completed handoff state stops the poll and reports success."
  (with-temp-buffer
    (hermes-test--own-handoff-poll (list :platform "telegram" :backoff 1))
    (let (stopped reported)
      (cl-letf (((symbol-function 'hermes-chat--handoff-stop)
                 (lambda () (setq stopped t)))
                ((symbol-function 'hermes-chat--insert-local-status)
                 (lambda (&rest _) (setq reported t)))
                ((symbol-function 'hermes-chat--set-header-state) #'ignore)
                ((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (error "should not reschedule on completed"))))
        (hermes-chat--handoff-handle-state
         (current-buffer) '((state . "completed"))))
      (should stopped)
      (should reported))))

(ert-deftest hermes-chat-handoff-handle-state-pending-reschedules ()
  "A non-terminal handoff state reschedules another poll."
  (with-temp-buffer
    (hermes-test--own-handoff-poll (list :platform "telegram" :backoff 1))
    (let (rescheduled)
      (cl-letf (((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (setq rescheduled t))))
        (hermes-chat--handoff-handle-state
         (current-buffer) '((state . "pending"))))
      (should rescheduled))))

(ert-deftest hermes-chat-handoff-reschedule-doubles-backoff-capped ()
  "Reschedule doubles the poll backoff up to the configured ceiling."
  (with-temp-buffer
    (let (delays)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay &rest _) (push delay delays) 'timer)))
        (hermes-test--own-handoff-poll (list :backoff 1))
        (hermes-chat--handoff-reschedule (current-buffer))
        (should (equal (plist-get hermes-chat--handoff-poll :backoff) 2))
        (hermes-test--own-handoff-poll (list :backoff 8))
        (hermes-chat--handoff-reschedule (current-buffer))
        (should (equal (plist-get hermes-chat--handoff-poll :backoff)
                       hermes-chat--handoff-poll-max-delay)))
      (should (equal (car (last delays)) 2)))))

(ert-deftest hermes-chat-handoff-stop-cancels-timer ()
  "Stopping the handoff cancels its timer and clears poll state."
  (with-temp-buffer
    (let (cancelled)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_timer) (setq cancelled t))))
        (hermes-test--own-handoff-poll (list :timer 'the-timer))
        (hermes-chat--handoff-stop))
      (should cancelled)
      (should-not hermes-chat--handoff-poll)
      (should-not hermes-chat--handoff-owner))))

(ert-deftest hermes-chat-handoff-poll-tick-times-out-past-deadline ()
  "A poll tick past the deadline routes to the timeout path, not a poll."
  (with-temp-buffer
    (hermes-test--own-handoff-poll
     (list :platform "telegram" :backoff 1
           :deadline (time-subtract (current-time) 5)))
    (let (timed-out)
      (cl-letf (((symbol-function 'hermes-chat--handoff-timeout)
                 (lambda (_b &optional _id) (setq timed-out t)))
                ((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (&rest _) (error "should not poll past deadline"))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should timed-out))))

(ert-deftest hermes-chat-handoff-poll-tick-reject-reschedules ()
  "A failed `handoff.state' poll reschedules rather than aborting."
  (with-temp-buffer
    (hermes-test--own-handoff-poll
     (list :platform "telegram" :backoff 1
           :deadline (time-add (current-time) 60)))
    (let (rescheduled)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (funcall (plist-get args :reject) "boom")))
                ((symbol-function 'hermes-chat--handoff-reschedule)
                 (lambda (_b) (setq rescheduled t))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should rescheduled))))

(ert-deftest hermes-chat-handoff-poll-tick-resolve-handles-state ()
  "A successful poll routes its result into the state handler."
  (with-temp-buffer
    (hermes-test--own-handoff-poll
     (list :platform "telegram" :backoff 1
           :deadline (time-add (current-time) 60)))
    (let (handled)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (funcall (plist-get args :resolve) '((state . "running")))))
                ((symbol-function 'hermes-chat--handoff-handle-state)
                 (lambda (_b result) (setq handled result))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should (equal (cdr (assq 'state handled)) "running")))))

(ert-deftest hermes-chat-handoff-stale-poll-result-does-not-settle-replacement ()
  "A result captured for poll A cannot settle a replacement poll B."
  (with-temp-buffer
    (let (resolve handled)
      (hermes-test--own-handoff-poll
       (list :id 'poll-a :platform "telegram" :backoff 1
             :deadline (time-add (current-time) 60)))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (setq resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-chat--handoff-handle-state)
                 (lambda (&rest _args) (setq handled t))))
        (hermes-chat--handoff-poll-tick (current-buffer))
        (hermes-test--own-handoff-poll
         (list :id 'poll-b :platform "discord" :backoff 1
               :deadline (time-add (current-time) 60)))
        (funcall resolve '((state . "completed"))))
      (should (eq (plist-get hermes-chat--handoff-poll :id) 'poll-b))
      (should-not handled))))

(ert-deftest hermes-chat-handoff-start-replaces-poll-with-new-identity ()
  "Starting a second handoff replaces the first with a distinct identity."
  (with-temp-buffer
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (hermes-chat--handoff-start-poll "telegram")
      (let ((first (plist-get hermes-chat--handoff-poll :id)))
        (hermes-chat--handoff-start-poll "discord")
        (should first)
        (should-not (eq first (plist-get hermes-chat--handoff-poll :id)))))))

(ert-deftest hermes-chat-handoff-slash-routes-to-command ()
  "The /handoff slash command dispatches its argument to `hermes-chat-handoff'."
  (let (called)
    (cl-letf (((symbol-function 'hermes-chat-handoff)
               (lambda (&optional arg) (setq called (or arg 'interactive)))))
      (let ((handler (hermes-chat--native-slash-handler "handoff")))
        (should handler)
        (funcall handler "telegram")
        (should (equal called "telegram"))))))

(ert-deftest hermes-chat-handoff-timeout-retains-owner-until-terminal ()
  "An overdue watcher keeps exact ownership until a terminal poll result."
  (hermes-test-with-chat-buffer
   (hermes-test--own-handoff-poll
    (list :platform "telegram" :backoff 8
          :deadline (time-subtract (current-time) 5)))
   (let ((owner hermes-chat--handoff-owner)
         failed reported rescheduled submitted slash-executed)
     (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-fail)
                (lambda (&rest _) (setq failed t)))
               ((symbol-function 'hermes-chat--command-error)
                (lambda (msg) (setq reported msg)))
               ((symbol-function 'hermes-chat--handoff-reschedule)
                (lambda (&rest _) (setq rescheduled t)))
               ((symbol-function 'hermes-chat--submit-through-transport)
                (lambda (&rest _) (setq submitted t)))
               ((symbol-function 'hermes-chat--dashboard-slash-exec)
                (lambda (&rest _) (setq slash-executed t))))
       (insert "valuable draft")
       (hermes-chat--handoff-timeout (current-buffer) owner)
       (should-not failed)
       (should (string-match-p "timed out" reported))
       (should rescheduled)
       (should (eq hermes-chat--handoff-owner owner))
       (should-not (plist-get hermes-chat--handoff-poll :deadline))
       (should-error (hermes-chat-send) :type 'user-error)
       (should-error (hermes-chat-queue-message) :type 'user-error)
       (should-error (hermes-chat--submit-content "programmatic")
                     :type 'user-error)
       (should-error (hermes-chat--handle-slash-content "/models")
                     :type 'user-error)
       (should-error (hermes-chat-steer-message) :type 'user-error)
       (should (equal (hermes-chat-input-string) "valuable draft"))
       (should-not submitted)
       (should-not slash-executed)
       (setq rescheduled nil)
       (hermes-chat--handoff-handle-state
        (current-buffer) '((state . "running")) owner)
       (should rescheduled)
       (should (eq hermes-chat--handoff-owner owner))
       (hermes-chat--handoff-handle-state
        (current-buffer) '((state . "completed")) owner)
       (should-not hermes-chat--handoff-owner)
       (hermes-test--own-handoff-poll
        (list :id 'replacement :platform "discord" :backoff 1))
       (hermes-chat--handoff-handle-state
        (current-buffer) '((state . "completed")) owner)
       (should (eq hermes-chat--handoff-owner 'replacement))))))

(ert-deftest hermes-chat-handoff-targets-parse-completion-items ()
  "Completion items become (PLATFORM . META) cells, vector or list."
  (should (equal (hermes-chat--handoff-targets
                  '((items . [((text . "telegram") (meta . "→ Home"))
                              ((text . "discord") (meta . ""))])))
                 '(("telegram" . "→ Home") ("discord" . "")))))

(ert-deftest hermes-chat-handoff-prompt-uses-live-targets ()
  "The platform prompt fetches live targets via complete.slash."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid")
   (let (slash-text offered)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (_client text &rest args)
                  (setq slash-text text)
                  (funcall (plist-get args :resolve)
                           '((items . [((text . "telegram") (meta . "→ H"))])))))
               ((symbol-function 'completing-read)
                (lambda (_prompt coll &rest _) (setq offered coll) ""))
               ((symbol-function 'hermes-chat--handoff-begin) #'ignore))
       (hermes-chat-handoff))
     (should (equal slash-text "/handoff "))
     (should (member "telegram" offered)))))

(ert-deftest hermes-chat-handoff-prompt-refuses-after-turn-starts ()
  "A delayed target completion cannot hand off after a turn starts."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-original")
   (let (resolve requested active)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p)
                (lambda () active))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (_client _text &rest args)
                  (setq resolve (plist-get args :resolve))))
               ((symbol-function 'completing-read)
                (lambda (&rest _) "telegram"))
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (&rest _) (setq requested t))))
       (hermes-chat-handoff)
       (setq active t)
       (funcall resolve '((items . [((text . "telegram"))])))
       (should-not requested)
       (should-not hermes-chat--handoff-owner)))))

(ert-deftest hermes-chat-handoff-prompt-refuses-after-session-replacement ()
  "A delayed target completion cannot hand off a replacement session."
  (hermes-test-with-chat-buffer
   (setq hermes-chat--dashboard-active-session-id "sid-original")
   (let (resolve requested)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (_client _text &rest args)
                  (setq resolve (plist-get args :resolve))))
               ((symbol-function 'completing-read)
                (lambda (&rest _) "telegram"))
               ((symbol-function 'hermes-dashboard-transport-handoff-request)
                (lambda (&rest _) (setq requested t))))
       (hermes-chat-handoff)
       (setq hermes-chat--dashboard-active-session-id "sid-replacement")
       (funcall resolve '((items . [((text . "telegram"))])))
       (should-not requested)
       (should-not hermes-chat--handoff-owner)))))

(ert-deftest hermes-chat-handoff-given-platform-skips-prompt ()
  "A platform argument begins the handoff without fetching live targets."
  (hermes-test-with-chat-buffer
   (let (began)
     (cl-letf (((symbol-function 'hermes-chat--dashboard-session-attached-p)
                (lambda () t))
               ((symbol-function 'hermes-chat--active-turn-p) (lambda () nil))
               ((symbol-function 'hermes-dashboard-transport-complete-slash)
                (lambda (&rest _) (error "should not fetch targets when given")))
               ((symbol-function 'hermes-chat--handoff-begin)
                (lambda (_buffer platform) (setq began platform))))
       (hermes-chat-handoff "Telegram"))
     (should (equal began "telegram")))))

(ert-deftest hermes-chat-handoff-read-target-falls-back-without-targets ()
  "With no live targets the picker reads a free-form platform name."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "  IRC ")))
    (should (equal (hermes-chat--handoff-read-target '((items . []))) "irc"))))

(ert-deftest hermes-chat-handoff-read-target-skips-blank-text ()
  "Items without a text field are dropped rather than erroring."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x")))
    (should (equal (hermes-chat--handoff-read-target '((items . [((meta . "m"))])))
                   "x"))))

(ert-deftest hermes-chat-dashboard-shares-one-client-across-buffers ()
  "Two chat buffers attach to one shared client and route events by session.
Killing one buffer releases its reference without tearing down the other."
  (let ((hermes-dashboard-transport--clients (make-hash-table :test #'equal))
        (made 0) a-events b-events buf-a buf-b shared)
    (cl-letf (((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest args)
                 (cl-incf made)
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
                            (lambda (event) (push event a-events))))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-a"))))
            (with-current-buffer buf-b
              (hermes-chat-mode)
              (hermes-chat--dashboard-start
               (lambda (event) (push event b-events)))
              (hermes-chat--dashboard-record-session
               shared '((session_id . "sid-b"))))
            (should (= made 1))
            (should (= (hermes-dashboard-transport-client-refcount shared) 2))
            (hermes-dashboard-transport--dispatch-event
             shared '(:type delta :session-id "sid-a" :content "x"))
            (should (= (length a-events) 1))
            (should-not b-events)
            (kill-buffer buf-a)
            (should (hermes-dashboard-transport-client-websocket shared))
            (should (= (hermes-dashboard-transport-client-refcount shared) 1))
            (hermes-dashboard-transport--dispatch-event
             shared '(:type delta :session-id "sid-b" :content "y"))
            (should (= (length b-events) 1)))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest hermes-chat-dashboard-reconnected-keeps-session-lazy ()
  "A reconnected status preserves the durable id without resuming it."
  (let ((client (hermes-test--dashboard-client)) resumed)
    (cl-letf (((symbol-function 'hermes-transport-send)
               (lambda (&rest _) (error "CLI fallback should not run")))
              ((symbol-function 'hermes-dashboard-transport-start)
               (lambda (&rest _) client))
              ((symbol-function 'hermes-dashboard-transport-session-resume)
               (lambda (_client session-id &rest _args) (setq resumed session-id))))
      (let ((hermes-transport-send-function #'hermes-transport-send))
        (hermes-test-with-chat-buffer
         (setq hermes-chat--session-id "stored-session"
               hermes-chat--dashboard-client client)
         ;; Drive the real subscriber closure (generation + session filter),
         ;; not the handler directly, so a future guard regression is caught.
         (funcall (hermes-chat--transport-callback
                   (current-buffer) "asst-reconnect" t
                   (hermes-chat--next-transport-generation))
                  '(:type status :status "reconnected"))
         (should-not resumed)
         (should (equal hermes-chat--session-id "stored-session")))))))

(ert-deftest hermes-chat-handoff-terminal-take-is-coherent-across-replacement ()
  "Terminal take clears only one exact owner, poll, and timer lease."
  (dolist (replacement '(owner poll timer split))
    (with-temp-buffer
      (let* ((old-timer (list 'old-timer))
             (new-timer (list 'new-timer))
             (old-poll (list :id 'old :timer old-timer))
             (new-poll (list :id 'new :timer new-timer)))
        (setq hermes-chat--handoff-owner 'old
              hermes-chat--handoff-poll old-poll)
        (let ((snapshot (hermes-chat--capture-handoff-terminal-owner)))
          (pcase replacement
            ('owner (setq hermes-chat--handoff-owner 'new))
            ('poll (setq hermes-chat--handoff-poll new-poll))
            ('timer
             (should (eq old-poll (plist-put old-poll :timer new-timer))))
            ('split
             (setq hermes-chat--handoff-owner 'new
                   hermes-chat--handoff-poll new-poll)))
          (should-not (hermes-chat--take-handoff-terminal-owner snapshot))
          (should (eq hermes-chat--handoff-owner
                      (if (memq replacement '(owner split)) 'new 'old)))
          (should (eq hermes-chat--handoff-poll
                      (if (memq replacement '(poll split)) new-poll old-poll)))
          (should (eq (plist-get hermes-chat--handoff-poll :timer)
                      (if (memq replacement '(poll timer split))
                          new-timer old-timer))))))))

(ert-deftest hermes-chat-handoff-terminal-cancel-clears-first-and-runs-once ()
  "Timer cancellation signals only after local authority is gone, once."
  (dolist (condition '(error quit))
    (with-temp-buffer
      (let* ((timer (list condition))
             (poll (list :id 'owner :timer timer))
             calls)
        (setq hermes-chat--handoff-owner 'owner
              hermes-chat--handoff-poll poll)
        (let* ((snapshot (hermes-chat--capture-handoff-terminal-owner))
               (effects (hermes-chat--take-handoff-terminal-owner snapshot)))
          (should (= (length effects) 1))
          (should-not hermes-chat--handoff-owner)
          (should-not hermes-chat--handoff-poll)
          (should-not calls)
          (cl-letf (((symbol-function 'cancel-timer)
                     (lambda (captured)
                       (push captured calls)
                       (should-not hermes-chat--handoff-owner)
                       (should-not hermes-chat--handoff-poll)
                       (signal condition nil))))
            (pcase condition
              ('error (should-error (funcall (car effects)) :type 'error))
              ('quit
               (let (signalled)
                 (condition-case nil
                     (funcall (car effects))
                   (quit (setq signalled t)))
                 (should signalled))))
            (funcall (car effects)))
          (should (equal calls (list timer))))))))


(provide 'hermes-chat-handoff-tests)
;;; hermes-chat-handoff-tests.el ends here
