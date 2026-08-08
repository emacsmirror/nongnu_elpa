;;; hermes-chat-reducer-tests.el --- turn reducer tests for hermes-el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the pure `hermes-chat--turn-reduce' reducer: status stamping,
;; terminal settlement, tool/delta effects, and out-of-scope no-ops.

;;; Code:

(require 'ert)
(require 'hermes-test-helpers)

(ert-deftest hermes-chat-turn-reduce-status-family-stamps-and-refreshes ()
  "Status-family events stamp :status-state and lead with a refresh-header effect."
  (let ((now '(100 200)))
    (dolist (case
             (list
              (list '(:status-state (:status running :activity "old"))
                    '(:type thinking :content "pondering...")
                    '(:status thinking :activity "Pondering" :updated (100 200))
                    '(refresh-header))
              (list '(:status-state (:status thinking :activity "x"))
                    '(:type commentary)
                    '(:status running :activity "Reasoning" :updated (100 200))
                    '(refresh-header upsert-entry))
              (list '(:status-state nil)
                    '(:type diff)
                    '(:status running :activity "Reviewing diff" :updated (100 200))
                    '(refresh-header upsert-entry))))
      (cl-destructuring-bind (state event expected-status effect-types) case
        (let ((result (hermes-chat--turn-reduce state event now)))
          (should (equal (plist-get (car result) :status-state) expected-status))
          (should (equal (mapcar #'car (cdr result)) effect-types))
          ;; refresh-header carries the same status-state threaded into NEW-STATE.
          (should (equal (cdr (assq 'refresh-header (cdr result)))
                         expected-status)))))))

(ert-deftest hermes-chat-turn-reduce-suppressed-terminal-settles-without-content ()
  "A suppressed terminal event runs the done lifecycle minus content copying."
  (let* ((original '(:type done :content "final text"))
         (event (list :type 'suppressed-terminal
                      :settle-status 'done
                      :header '(:type done)
                      :original original))
         (result (hermes-chat--turn-reduce
                  '(:status-state (:status running)) event '(1 2)))
         (effects (cdr result)))
    (should (equal (mapcar #'car effects)
                   '(clear-tools refresh-header clear-prompts mark-status
                     drop-thinking settle finish clear-pending
                     set-dashboard-running drain)))
    (should (eq (cdr (assq 'mark-status effects)) 'done))
    (should (eq (cdr (assq 'clear-prompts effects)) original))
    (should-not (assq 'mark-done effects))
    (should (eq (plist-get (plist-get (car result) :status-state) :status)
                'ready))))

(ert-deftest hermes-chat-turn-reduce-status-stamps-clock-and-upserts ()
  "A non-session-info status stamps NOW, refreshes, and upserts; info skips upsert."
  (let* ((now '(7 7))
         (state '(:status-state (:status idle :activity "x")))
         (event '(:type status :status "running" :content "Searching"))
         (result (hermes-chat--turn-reduce state event now))
         (status-state (plist-get (car result) :status-state)))
    (should (equal (mapcar #'car (cdr result)) '(refresh-header upsert-entry)))
    (should (equal (cdr (assq 'upsert-entry (cdr result))) event))
    (should (equal (plist-get status-state :updated) now))
    (should (equal status-state
                   (apply #'hermes-chat--entry-with
                          '(:status idle :activity "x")
                          (append (hermes-chat--turn-header-props event)
                                  (list :updated now)))))
    (let ((r (hermes-chat--turn-reduce
              state '(:type status :event "session.info" :status "ready") now)))
      (should (equal (mapcar #'car (cdr r)) '(refresh-header))))))

(ert-deftest hermes-chat-turn-reduce-goal-updates-header-state-only ()
  "Structured goal state is reduced without creating transcript content."
  (let* ((goal '(:status "active" :running t :turns-used 2 :max-turns 20))
         (state '(:status-state (:status ready) :goal nil))
         (result (hermes-chat--turn-reduce
                  state (list :type 'goal :goal goal) '(7 7))))
    (should (equal (plist-get (car result) :goal) goal))
    (should (equal (cdr result) '((refresh-header))))
    (let ((cleared (hermes-chat--turn-reduce
                    (car result) '(:type goal :goal nil) '(8 8))))
      (should-not (plist-get (car cleared) :goal))
      (should (equal (cdr cleared) '((refresh-header)))))))

(ert-deftest hermes-chat-turn-reduce-terminal-events ()
  "done/error reduce to the ordered turn-lifecycle effects; unknown adds a message."
  (let ((now '(5 5))
        (state '(:status-state (:status running :activity "x"))))
    (let* ((event '(:type done :usage (:input 1 :output 2)))
           (r (hermes-chat--turn-reduce state event now)))
      (should (equal (plist-get (car r) :status-state)
                     '(:status ready :activity "Ready"
                               :usage (:input 1 :output 2) :updated (5 5))))
      ;; refresh-header precedes drain so the header settles before re-submit.
      (should (equal (mapcar #'car (cdr r))
                     '(clear-tools refresh-header clear-prompts mark-done
                       drop-thinking settle finish clear-pending
                       set-dashboard-running drain)))
      (should (eq (cdr (assq 'settle (cdr r))) 'done)))
    (let* ((event '(:type error :content "boom"))
           (estatus (hermes-chat--error-status event))
           (r (hermes-chat--turn-reduce state event now)))
      (should (equal (mapcar #'car (cdr r))
                     '(clear-tools refresh-header clear-prompts append-error
                       settle finish clear-pending set-dashboard-running
                       drain)))
      (should (equal (cdr (assq 'append-error (cdr r))) (cons "boom" estatus)))
      (should (eq (cdr (assq 'settle (cdr r))) estatus)))
    (let* ((event '(:type unknown :event "weird"))
           (r (hermes-chat--turn-reduce state event now)))
      (should (eq (plist-get (plist-get (car r) :status-state) :status) 'error))
      (should (equal (mapcar #'car (cdr r))
                     '(refresh-header message upsert-entry))))))

(ert-deftest hermes-chat-turn-reduce-done-surfaces-warning ()
  "A done event with a warning emits a warning effect after mark-done."
  (let* ((state '(:status-state (:status running)))
         (event '(:type done :warning "not saved to history"))
         (r (hermes-chat--turn-reduce state event '(5 5))))
    (should (equal (mapcar #'car (cdr r))
                   '(clear-tools refresh-header clear-prompts mark-done
                     warning drop-thinking settle finish clear-pending
                     set-dashboard-running drain)))
    (should (equal (cdr (assq 'warning (cdr r))) "not saved to history"))))

(ert-deftest hermes-chat-turn-reduce-delta-emits-append-effect ()
  "A delta event leaves the state and emits append-delta carrying its content."
  (let ((state '(:status-state (:status running))))
    (let ((r (hermes-chat--turn-reduce state '(:type delta :content "hi") '(0 0))))
      (should (eq (car r) state))
      (should (equal (cdr r) '((append-delta . "hi")))))
    ;; Missing content becomes the empty string.
    (let ((r (hermes-chat--turn-reduce state '(:type delta) '(0 0))))
      (should (equal (cdr r) '((append-delta . "")))))))

(ert-deftest hermes-chat-turn-reduce-tool-family-delta-and-transcript ()
  "Tool-like events leave the state and emit a tool delta plus an upsert-entry."
  (let ((running '(:type tool :name "terminal" :status "running" :context "make test"))
        (done '(:type tool :name "terminal" :status "completed" :context "make test"))
        (state '(:status-state (:status running))))
    (let ((result (hermes-chat--turn-reduce state running '(0 0))))
      (should (eq (car result) state))
      (should (equal (cdr result)
                     (list (cons 'tool-put
                                 (cons (hermes-chat--header-tool-key running)
                                       (hermes-chat--header-tool-summary running)))
                           (cons 'upsert-entry running)))))
    (let ((result (hermes-chat--turn-reduce state done '(0 0))))
      (should (equal (cdr result)
                     (list (cons 'tool-remove (hermes-chat--header-tool-key done))
                           (cons 'upsert-entry done)))))
    ;; No summary -> no tool delta.
    (should-not (hermes-chat--turn-tool-effect '(:type status)))))

(ert-deftest hermes-chat-turn-reduce-out-of-scope-is-noop ()
  "An unhandled event type returns the same state object and no effects."
  (let ((state '(:status-state (:status running))))
    (should (equal (hermes-chat--turn-reduce state '(:type bogus) '(0 0))
                   (cons state nil)))))

(provide 'hermes-chat-reducer-tests)
;;; hermes-chat-reducer-tests.el ends here
