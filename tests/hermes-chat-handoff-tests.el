;;; hermes-chat-handoff-tests.el --- session handoff tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `hermes-chat-handoff': the handoff command's guards, the
;; target prompt, and the backoff-polled `handoff.state' watcher.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

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

(ert-deftest hermes-chat-handoff-handle-state-completed-stops ()
  "A completed handoff state stops the poll and reports success."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 1))
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
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 1))
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
        (setq hermes-chat--handoff-poll (list :backoff 1))
        (hermes-chat--handoff-reschedule (current-buffer))
        (should (equal (plist-get hermes-chat--handoff-poll :backoff) 2))
        (setq hermes-chat--handoff-poll (list :backoff 8))
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
        (setq hermes-chat--handoff-poll (list :timer 'the-timer))
        (hermes-chat--handoff-stop))
      (should cancelled)
      (should-not hermes-chat--handoff-poll))))

(ert-deftest hermes-chat-handoff-poll-tick-times-out-past-deadline ()
  "A poll tick past the deadline routes to the timeout path, not a poll."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll
          (list :platform "telegram" :backoff 1
                :deadline (time-subtract (current-time) 5)))
    (let (timed-out)
      (cl-letf (((symbol-function 'hermes-chat--handoff-timeout)
                 (lambda (_b) (setq timed-out t)))
                ((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (&rest _) (error "should not poll past deadline"))))
        (hermes-chat--handoff-poll-tick (current-buffer)))
      (should timed-out))))

(ert-deftest hermes-chat-handoff-poll-tick-reject-reschedules ()
  "A failed `handoff.state' poll reschedules rather than aborting."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll
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
    (setq hermes-chat--handoff-poll
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
      (setq hermes-chat--handoff-poll
            (list :id 'poll-a :platform "telegram" :backoff 1
                  :deadline (time-add (current-time) 60)))
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-state)
                 (lambda (_client &rest args)
                   (setq resolve (plist-get args :resolve))))
                ((symbol-function 'hermes-chat--handoff-handle-state)
                 (lambda (&rest _args) (setq handled t))))
        (hermes-chat--handoff-poll-tick (current-buffer))
        (setq hermes-chat--handoff-poll
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

(ert-deftest hermes-chat-handoff-timeout-fails-and-reports ()
  "A timed-out handoff fires `handoff.fail' and reports the error."
  (with-temp-buffer
    (setq hermes-chat--handoff-poll (list :platform "telegram" :backoff 8))
    (let (failed reported)
      (cl-letf (((symbol-function 'hermes-dashboard-transport-handoff-fail)
                 (lambda (_client &rest args)
                   (setq failed (plist-get args :error))))
                ((symbol-function 'hermes-chat--command-error)
                 (lambda (msg) (setq reported msg))))
        (hermes-chat--handoff-timeout (current-buffer)))
      (should (equal failed "client poll timed out"))
      (should (string-match-p "timed out" reported))
      (should-not hermes-chat--handoff-poll))))

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

(ert-deftest hermes-chat-dashboard-reconnected-resumes-session ()
  "A reconnected status re-resumes the buffer's stored dashboard session."
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
         (should (equal resumed "stored-session")))))))


(provide 'hermes-chat-handoff-tests)
;;; hermes-chat-handoff-tests.el ends here
