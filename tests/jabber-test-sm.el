;;; jabber-test-sm.el --- Tests for jabber-sm  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0198 Stream Management.

;;; Code:

(require 'ert)
(require 'jabber-xml)
(require 'jabber-sm)
(require 'jabber-sm-runtime)
(require 'jabber-core)

(defvar jabber-connections)
(defvar jabber-debug-keep-process-buffers)

;;; Counter arithmetic

(ert-deftest jabber-test-sm-inc-counter ()
  "Incrementing a counter adds one."
  (should (= (jabber-sm--inc-counter 0) 1))
  (should (= (jabber-sm--inc-counter 41) 42)))

(ert-deftest jabber-test-sm-inc-counter-wraps ()
  "Counter wraps at 2^32."
  (should (= (jabber-sm--inc-counter (1- (expt 2 32))) 0)))

(ert-deftest jabber-test-sm-counter-delta ()
  "Forward distance between counters."
  (should (= (jabber-sm--counter-delta 5 3) 2))
  (should (= (jabber-sm--counter-delta 0 (1- (expt 2 32))) 1)))

(ert-deftest jabber-test-sm-counter-<= ()
  "Counter comparison with wraparound."
  (should (jabber-sm--counter-<= 3 5))
  (should (jabber-sm--counter-<= 3 3))
  (should-not (jabber-sm--counter-<= 5 3))
  ;; Wraparound: counter near max is "before" counter near 0
  (should (jabber-sm--counter-<= (- (expt 2 32) 2) 1)))

;;; Predicates

(ert-deftest jabber-test-sm-r-p ()
  "Detect SM <r/> element."
  (should (jabber-sm--r-p '(r ((xmlns . "urn:xmpp:sm:3")))))
  (should-not (jabber-sm--r-p '(r ((xmlns . "wrong")))))
  (should-not (jabber-sm--r-p '(a ((xmlns . "urn:xmpp:sm:3"))))))

(ert-deftest jabber-test-sm-a-p ()
  "Detect SM <a/> element."
  (should (jabber-sm--a-p '(a ((xmlns . "urn:xmpp:sm:3") (h . "5")))))
  (should-not (jabber-sm--a-p '(r ((xmlns . "urn:xmpp:sm:3"))))))

(ert-deftest jabber-test-sm-enabled-p ()
  "Detect SM <enabled/> element."
  (should (jabber-sm--enabled-p
           '(enabled ((xmlns . "urn:xmpp:sm:3") (id . "abc") (resume . "true"))))))

(ert-deftest jabber-test-sm-resumed-p ()
  "Detect SM <resumed/> element."
  (should (jabber-sm--resumed-p
           '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "5") (previd . "abc"))))))

(ert-deftest jabber-test-sm-failed-p ()
  "Detect SM <failed/> element."
  (should (jabber-sm--failed-p
           '(failed ((xmlns . "urn:xmpp:sm:3"))))))

;;; State-data reset

(ert-deftest jabber-test-sm-reset ()
  "Reset clears all SM keys to defaults."
  (let* ((sd (list :username "test" :sm-enabled t :sm-outbound-count 42))
         (result (jabber-sm--reset sd)))
    (should-not (plist-get result :sm-enabled))
    (should (= (plist-get result :sm-outbound-count) 0))
    (should (= (plist-get result :sm-inbound-count) 0))
    (should (null (plist-get result :sm-outbound-queue)))
    ;; Non-SM keys preserved
    (should (equal (plist-get result :username) "test"))))

;;; Features check

(ert-deftest jabber-test-sm-features-have-sm ()
  "Detect SM in stream features."
  (let ((sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind")))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (jabber-sm--features-have-sm-p sd))))

(ert-deftest jabber-test-sm-features-no-sm ()
  "No SM in stream features."
  (let ((sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind")))))))
    (should-not (jabber-sm--features-have-sm-p sd))))

(ert-deftest jabber-test-sm-features-nil ()
  "Nil stream features."
  (should-not (jabber-sm--features-have-sm-p '(:stream-features nil))))

;;; Stanza counting

(ert-deftest jabber-test-sm-count-outbound-message ()
  "Outbound message increments counter and queues."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 0 :sm-outbound-queue nil))
         (msg '(message ((to . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-outbound sd msg)))
    (should (= (plist-get result :sm-outbound-count) 1))
    (should (= (length (plist-get result :sm-outbound-queue)) 1))
    (should (equal (cdar (plist-get result :sm-outbound-queue)) msg))))

(ert-deftest jabber-test-sm-count-outbound-iq ()
  "Outbound iq increments counter."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 5 :sm-outbound-queue nil))
         (iq '(iq ((type . "get") (id . "1")) (query ((xmlns . "test")))))
         (result (jabber-sm--count-outbound sd iq)))
    (should (= (plist-get result :sm-outbound-count) 6))))

(ert-deftest jabber-test-sm-count-outbound-disabled ()
  "No counting when SM is disabled."
  (let* ((sd (list :sm-enabled nil :sm-outbound-count 0 :sm-outbound-queue nil))
         (msg '(message ((to . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-outbound sd msg)))
    (should (= (plist-get result :sm-outbound-count) 0))
    (should (null (plist-get result :sm-outbound-queue)))))

(ert-deftest jabber-test-sm-count-outbound-non-stanza ()
  "SM elements are not counted."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 0 :sm-outbound-queue nil))
         (r '(r ((xmlns . "urn:xmpp:sm:3"))))
         (result (jabber-sm--count-outbound sd r)))
    (should (= (plist-get result :sm-outbound-count) 0))))

(ert-deftest jabber-test-sm-count-inbound ()
  "Inbound stanza increments counter."
  (let* ((sd (list :sm-enabled t :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-inbound nil sd msg)))
    (should (= (plist-get result :sm-inbound-count) 1))))

(ert-deftest jabber-test-sm-count-inbound-disabled ()
  "No counting when SM is disabled."
  (let* ((sd (list :sm-enabled nil :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-inbound nil sd msg)))
    (should (= (plist-get result :sm-inbound-count) 0))))

(ert-deftest jabber-test-sm-proactive-ack ()
  "Proactive ack is sent when inbound counter hits the interval."
  (let* ((jabber-sm-ack-interval 3)
         (sd (list :sm-enabled t :sm-inbound-count 2))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (ack-sent nil))
    (cl-letf (((symbol-function 'jabber-sm--send-ack)
               (lambda (_jc _sd) (setq ack-sent t))))
      (jabber-sm--count-inbound 'fake-jc sd msg))
    (should ack-sent)))

(ert-deftest jabber-test-sm-proactive-ack-not-at-interval ()
  "No proactive ack when counter is not at the interval boundary."
  (let* ((jabber-sm-ack-interval 3)
         (sd (list :sm-enabled t :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (ack-sent nil))
    (cl-letf (((symbol-function 'jabber-sm--send-ack)
               (lambda (_jc _sd) (setq ack-sent t))))
      (jabber-sm--count-inbound 'fake-jc sd msg))
    (should-not ack-sent)))

(ert-deftest jabber-test-sm-proactive-ack-disabled ()
  "No proactive ack when jabber-sm-ack-interval is nil."
  (let* ((jabber-sm-ack-interval nil)
         (sd (list :sm-enabled t :sm-inbound-count 2))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (ack-sent nil))
    (cl-letf (((symbol-function 'jabber-sm--send-ack)
               (lambda (_jc _sd) (setq ack-sent t))))
      (jabber-sm--count-inbound 'fake-jc sd msg))
    (should-not ack-sent)))

;;; Queue pruning and ack processing

(ert-deftest jabber-test-sm-prune-queue ()
  "Prune removes entries with count <= h."
  (let* ((queue (list (cons 1 'a) (cons 2 'b) (cons 3 'c) (cons 4 'd)))
         (result (jabber-sm--prune-queue queue 2)))
    (should (= (length result) 2))
    (should (= (caar result) 3))))

(ert-deftest jabber-test-sm-prune-queue-empty ()
  "Prune on empty queue returns empty."
  (should (null (jabber-sm--prune-queue nil 5))))

(ert-deftest jabber-test-sm-process-ack ()
  "Processing <a/> prunes queue and updates last-acked."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-count 3
                   :sm-outbound-queue (list (cons 1 'a) (cons 2 'b) (cons 3 'c))
                   :sm-last-acked 0))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "2"))))
         (result (jabber-sm--process-ack sd ack)))
    (should (= (plist-get result :sm-last-acked) 2))
    (should (= (length (plist-get result :sm-outbound-queue)) 1))
    (should (= (caar (plist-get result :sm-outbound-queue)) 3))))

(ert-deftest jabber-test-sm-process-ack-ahead-signals-protocol-error ()
  "An acknowledgement beyond the sent count is a protocol error."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-count 11501
                   :sm-outbound-queue (list (cons 11501 'a))
                   :sm-last-acked 11501
                   :sm-stall-since 1.0))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "11502"))))
         (before (copy-tree sd)))
    (should-error (jabber-sm--process-ack sd ack)
                  :type 'jabber-sm-handled-count-too-high)
    (should (equal before sd)))
  (dolist (h '("11502" "11501"))
    (let* ((sd (list :sm-enabled t
                     :sm-outbound-count 11502
                     :sm-outbound-queue (list (cons 11502 'a))
                     :sm-last-acked 11502
                     :sm-stall-since 1.0))
           (before (copy-tree sd))
           (ack `(a ((xmlns . "urn:xmpp:sm:3") (h . ,h))))
           (result (jabber-sm--process-ack sd ack)))
      (should (equal result before)))))

(ert-deftest jabber-test-sm-stall-reconnects-with-unacked-state ()
  "An acknowledgement stall reconnects without fabricating an ack."
  (let* ((outbound '((1 . first)))
         (pending '((0 message nil (body nil "second"))))
         (connection 'fake-process)
         (state-data (list :connection connection
                           :sm-outbound-count 1
                           :sm-last-acked 0
                           :sm-outbound-queue outbound
                           :sm-pending-queue pending
                           :sm-stall-since 1.0))
         deleted)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (process) (setq deleted process))))
      (jabber-sm--recover-stall 'fake-jc state-data))
    (should (eq connection deleted))
    (should (equal outbound (plist-get state-data :sm-outbound-queue)))
    (should (equal pending (plist-get state-data :sm-pending-queue)))
    (should (equal "Stream Management acknowledgement timeout"
                   (plist-get state-data :disconnection-reason)))))

(ert-deftest jabber-test-sm-stall-reports-exact-virtual-transport ()
  "A non-process stall reports the exact transport to the FSM."
  (let* ((connection (make-symbol "virtual-transport"))
         (state-data (list :connection connection
                           :sm-outbound-queue '((1 . first))))
         sent)
    (cl-letf (((symbol-function 'processp) (lambda (_) nil))
              ((symbol-function 'fsm-send)
               (lambda (jc event) (setq sent (list jc event)))))
      (jabber-sm--recover-stall 'fake-jc state-data))
    (should (equal sent
                   (list 'fake-jc
                         (list :connection-dead connection
                               "Stream Management acknowledgement timeout"))))))

;;; FSM routing helper

(ert-deftest jabber-test-sm-maybe-enable-with-sm ()
  "Route to :sm-enable when SM is available."
  (let ((jabber-sm-enable t)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :sm-enable))))

(ert-deftest jabber-test-sm-maybe-enable-without-sm ()
  "Route to :session-established when SM is not in features."
  (let ((jabber-sm-enable t)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :session-established))))

(ert-deftest jabber-test-sm-maybe-enable-disabled ()
  "Route to :session-established when SM is disabled by user."
  (let ((jabber-sm-enable nil)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :session-established))))

;;; Enable/resume XML generation

(ert-deftest jabber-test-sm-make-enable-xml ()
  "Enable XML matches expected format."
  (should (string-match-p "enable" (jabber-sm--make-enable-xml)))
  (should (string-match-p "resume='true'" (jabber-sm--make-enable-xml))))

(ert-deftest jabber-test-sm-make-resume-xml ()
  "Resume XML includes h and previd."
  (let ((xml (jabber-sm--make-resume-xml 42 "session-123")))
    (should (string-match-p "h='42'" xml))
    (should (string-match-p "previd='session-123'" xml))))

(ert-deftest jabber-test-sm-parse-enabled ()
  "Parse <enabled/> stanza with resume=true."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "abc-123")
                          (resume . "true")
                          (max . "300"))))))
    (should (equal (plist-get info :id) "abc-123"))
    (should (plist-get info :resume))
    (should (= (plist-get info :max) 300))))

(ert-deftest jabber-test-sm-parse-enabled-resume-1 ()
  "Parse <enabled/> stanza with resume=1."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "xyz")
                          (resume . "1"))))))
    (should (plist-get info :resume))))

(ert-deftest jabber-test-sm-parse-enabled-no-resume ()
  "Parse <enabled/> stanza without resume attribute."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "xyz-456"))))))
    (should (equal (plist-get info :id) "xyz-456"))
    (should-not (plist-get info :resume))
    (should-not (plist-get info :max))))

(ert-deftest jabber-test-sm-apply-enabled-with-resume ()
  "Apply enabled info with resume granted."
  (let* ((sd (list :sm-enabled nil :sm-id nil :sm-resume-max nil))
         (info (list :id "abc" :resume '("true") :max 300))
         (result (jabber-sm--apply-enabled sd info)))
    (should (eq (plist-get result :sm-enabled) t))
    (should (equal (plist-get result :sm-id) "abc"))
    (should (= (plist-get result :sm-resume-max) 300))))

(ert-deftest jabber-test-sm-apply-enabled-no-resume ()
  "Apply enabled info without resume: sm-id stays nil."
  (let* ((sd (list :sm-enabled nil :sm-id nil :sm-resume-max nil))
         (info (list :id "abc" :resume nil :max nil))
         (result (jabber-sm--apply-enabled sd info)))
    (should (eq (plist-get result :sm-enabled) t))
    (should-not (plist-get result :sm-id))))

;;; Resume handling

(ert-deftest jabber-test-sm-handle-resumed ()
  "Handle <resumed/> prunes queue and returns stanzas to resend."
  (let* ((msg-a '(message ((to . "a@x")) (body () "a")))
         (msg-b '(message ((to . "b@x")) (body () "b")))
         (msg-c '(message ((to . "c@x")) (body () "c")))
         (sd (list :sm-enabled t
                   :sm-id "abc"
                   :sm-outbound-count 3
                   :sm-outbound-queue (list (cons 1 msg-a)
                                            (cons 2 msg-b)
                                            (cons 3 msg-c))
                   :sm-last-acked 0
                   :sm-resumed nil
                   :sm-resuming t))
         (resumed '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "1") (previd . "abc"))))
         (result (jabber-sm--handle-resumed sd resumed)))
    ;; state-data updated
    (should (= (plist-get (car result) :sm-last-acked) 1))
    (should (= (plist-get (car result) :sm-outbound-count) 1))
    (should (null (plist-get (car result) :sm-outbound-queue)))
    (should (eq (plist-get (car result) :sm-resumed) t))
    (should-not (plist-get (car result) :sm-resuming))
    ;; stanzas to resend: entries 2 and 3
    (should (= (length (cdr result)) 2))
    (should (equal (car (cdr result)) msg-b))
    (should (equal (cadr (cdr result)) msg-c))))

(ert-deftest jabber-test-sm-handle-resumed-all-acked ()
  "All stanzas acked means nothing to resend."
  (let* ((sd (list :sm-enabled t
                   :sm-id "abc"
                   :sm-outbound-count 2
                   :sm-outbound-queue (list (cons 1 'a) (cons 2 'b))
                   :sm-last-acked 0
                   :sm-resumed nil
                   :sm-resuming t))
         (resumed '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "2") (previd . "abc"))))
         (result (jabber-sm--handle-resumed sd resumed)))
    (should (null (cdr result)))))

(ert-deftest jabber-test-sm-handle-resumed-counter-reset ()
  "Outbound counter resets to server h, preventing drift on resend."
  (let* ((msg-a '(message ((to . "a@x")) (body () "a")))
         (msg-b '(message ((to . "b@x")) (body () "b")))
         (msg-c '(message ((to . "c@x")) (body () "c")))
         (sd (list :sm-enabled t
                   :sm-id "s1"
                   :sm-outbound-count 10
                   :sm-outbound-queue (list (cons 8 msg-a)
                                            (cons 9 msg-b)
                                            (cons 10 msg-c))
                   :sm-last-acked 7
                   :sm-resumed nil
                   :sm-resuming t))
         (resumed '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "8") (previd . "s1"))))
         (result (jabber-sm--handle-resumed sd resumed))
         (new-sd (car result))
         (to-resend (cdr result)))
    ;; Counter must reset to server's h so resent stanzas start from 8
    (should (= (plist-get new-sd :sm-outbound-count) 8))
    ;; Two stanzas to resend (9 and 10 were unacked)
    (should (= (length to-resend) 2))
    ;; After resending, count-outbound increments from 8 to 9, 10
    ;; rather than from 10 to 11, 12 (the old drift bug)
    (let ((after-resend new-sd))
      (dolist (sexp to-resend)
        (setq after-resend (jabber-sm--count-outbound after-resend sexp)))
      (should (= (plist-get after-resend :sm-outbound-count) 10)))))

(ert-deftest jabber-test-sm-handle-failed-resume-preserves-unacked ()
  "A failed resume carries unacknowledged stanzas into the new session."
  (let* ((msg-a '(message ((to . "a@x")) (body () "a")))
         (msg-b '(message ((to . "b@x")) (body () "b")))
         (msg-c '(message ((to . "c@x")) (body () "c")))
         (pending (list (cons 0 msg-c)))
         (sd (list :sm-enabled t
                   :sm-id "expired"
                   :sm-outbound-count 2
                   :sm-outbound-queue (list (cons 1 msg-a)
                                            (cons 2 msg-b))
                   :sm-pending-queue pending
                   :sm-last-acked 0
                   :sm-resuming t))
         (failed '(failed ((xmlns . "urn:xmpp:sm:3") (h . "1"))))
         (result (jabber-sm--handle-failed-resume sd failed)))
    (should-not (plist-get result :sm-enabled))
    (should-not (plist-get result :sm-id))
    (should-not (plist-get result :sm-resuming))
    (should
     (equal (mapcar #'jabber-sm--pending-stanza
                    (plist-get result :sm-pending-queue))
            (list msg-b msg-c)))))

(ert-deftest jabber-test-sm-resume-failure-binds-without-reauthentication ()
  "A failed post-SASL resume continues with resource binding."
  (let* ((msg '(message ((to . "a@x")) (body () "unacked")))
         (fsm 'fake-jc)
         (state-data
          (list :resource "emacs"
                :sm-enabled t
                :sm-id "expired"
                :sm-resuming t
                :sm-outbound-count 1
                :sm-outbound-queue (list (cons 1 msg))
                :sm-last-acked 0
                :stream-features
                `(features ()
                           (bind ((xmlns . ,jabber-bind-xmlns)))
                           (sm ((xmlns . ,jabber-sm-xmlns))))))
         (event `(:stanza (failed ((xmlns . ,jabber-sm-xmlns)))))
         (handler (gethash :sm-resume
                           (get 'jabber-connection :fsm-event)))
         (enter-bind (gethash :bind
                              (get 'jabber-connection :fsm-enter)))
         sent-iq
         stream-header-sent)
    (cl-letf (((symbol-function 'jabber-lifecycle-dispatch-session-reset)
               #'ignore)
              ((symbol-function 'jabber-send-stream-header)
               (lambda (_jc) (setq stream-header-sent t)))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent-iq args))))
      (pcase-let* ((`(,next-state ,next-data)
                     (funcall handler fsm state-data event #'ignore))
                   (`(,entered-data nil)
                    (funcall enter-bind fsm next-data)))
        (should (eq next-state :bind))
        (should sent-iq)
        (should-not stream-header-sent)
        (should-not (plist-get entered-data :sm-resuming))
        (should
         (equal (mapcar #'jabber-sm--pending-stanza
                        (plist-get entered-data :sm-pending-queue))
                (list msg)))))))

(ert-deftest jabber-test-sm-new-session-drains-recovered-stanzas ()
  "A newly established session sends stanzas recovered from failed resume."
  (let* ((msg '(message ((to . "a@x")) (body () "unacked")))
         (state-data (jabber-sm--reset nil))
         (enter (gethash :session-established
                         (get 'jabber-connection :fsm-enter)))
         sent)
    (setq state-data
          (plist-put state-data :sm-pending-queue (list (cons 0 msg))))
    (cl-letf (((symbol-function 'jabber-lifecycle-dispatch-session-bootstrap)
               #'ignore)
              ((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (pcase-let ((`(,result nil) (funcall enter 'fake-jc state-data)))
        (should (equal sent (list msg)))
        (should-not (plist-get result :sm-pending-queue))))))

;;; Ack XML generation

(ert-deftest jabber-test-sm-make-ack-xml ()
  "Ack XML includes h value."
  (should (string-match-p "h='7'" (jabber-sm--make-ack-xml 7))))

(ert-deftest jabber-test-sm-make-request-xml ()
  "Request XML is well-formed."
  (should (string-match-p "<r xmlns=" (jabber-sm--make-request-xml))))

;;; Ack timer

(ert-deftest jabber-test-sm-r-timer-reports-errors ()
  "Ack timer errors are reported without escaping the timer."
  (let ((jabber-connections '(fake-jc))
        messages)
    (cl-letf (((symbol-function 'jabber-sm--request-ack)
               (lambda (_jc) (error "send failed")))
              ((symbol-function 'jabber-sm--check-stall)
               (lambda (_jc) (error "should not run")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (jabber-sm--r-timer-function 'fake-jc)
      (should (equal '("SM: ack timer failed: send failed") messages)))))

;;; Queue operations across 2^32 boundary

(ert-deftest jabber-test-sm-prune-queue-wraparound ()
  "Prune works when counters span the 2^32 wraparound."
  (let* ((near-max (- (expt 2 32) 1))
         (queue (list (cons near-max 'a) (cons 0 'b) (cons 1 'c)))
         (result (jabber-sm--prune-queue queue 0)))
    ;; near-max and 0 are both <= 0 (with wraparound), so pruned
    (should (= (length result) 1))
    (should (= (caar result) 1))))

;;; h-count validation

(ert-deftest jabber-test-sm-parse-handled-count-valid ()
  "Parse every supported XML Schema unsignedInt lexical shape."
  (dolist (case '(("0" . 0)
                  ("0001" . 1)
                  ("+1" . 1)
                  (" 1 " . 1)
                  ("4294967295" . 4294967295)))
    (should (= (jabber-sm--parse-handled-count
                `(a ((h . ,(car case)))))
               (cdr case))))
  (should-not
   (jabber-sm--parse-handled-count
    '(failed ((xmlns . "urn:xmpp:sm:3"))) t)))

(ert-deftest jabber-test-sm-parse-handled-count-rejects-invalid ()
  "Reject missing, malformed, negative, and overflowing handled counts."
  (dolist (h '(nil "" "1oops" "-1" "1.0" "4294967296"
                    "\v1" "\u00a01"))
    (should-error
     (jabber-sm--parse-handled-count
      `(a (,@(when h `((h . ,h))))))
     :type 'jabber-sm-invalid-acknowledgement)))

(ert-deftest jabber-test-sm-classifies-handled-count-intervals ()
  "Classify current, forward, stale, and over-high counts across wrap."
  (let ((ordinary '(:sm-last-acked 7 :sm-outbound-count 10))
        (wrapped `(:sm-last-acked ,(1- jabber-sm--counter-max)
                                  :sm-outbound-count 1)))
    (should (eq (jabber-sm--handled-count-status ordinary 7) :current))
    (should (eq (jabber-sm--handled-count-status ordinary 8) :forward))
    (should (eq (jabber-sm--handled-count-status ordinary 10) :forward))
    (should (eq (jabber-sm--handled-count-status ordinary 6) :stale))
    (should-error (jabber-sm--handled-count-status ordinary 11)
                  :type 'jabber-sm-handled-count-too-high)
    (should (eq (jabber-sm--handled-count-status
                 wrapped (1- jabber-sm--counter-max))
                :current))
    (should (eq (jabber-sm--handled-count-status wrapped 0) :forward))
    (should (eq (jabber-sm--handled-count-status wrapped 1) :forward))
    (should (eq (jabber-sm--handled-count-status
                 wrapped (- jabber-sm--counter-max 2))
                :stale))
    (should-error (jabber-sm--handled-count-status wrapped 2)
                  :type 'jabber-sm-handled-count-too-high)))

(ert-deftest jabber-test-sm-classifies-generated-valid-intervals ()
  "Every generated count inside a bounded modular interval is accepted."
  (dolist (last `(0 1 ,(- jabber-sm--counter-max 2)
                       ,(1- jabber-sm--counter-max)))
    (dotimes (span 5)
      (let ((state-data
             (list :sm-last-acked last
                   :sm-outbound-count
                   (mod (+ last span) jabber-sm--counter-max))))
        (dotimes (step (1+ span))
          (should
           (eq (jabber-sm--handled-count-status
                state-data (mod (+ last step) jabber-sm--counter-max))
               (if (zerop step) :current :forward))))))))

(ert-deftest jabber-test-sm-invalid-a-preserves-state ()
  "Malformed ordinary acknowledgements cannot mutate SM state."
  (dolist (h '(nil "" "1oops" "-1" "4294967296"))
    (let* ((sd (list :sm-outbound-count 2
                     :sm-last-acked 0
                     :sm-outbound-queue '((1 . first) (2 . second))
                     :sm-pending-queue '((0 . pending))))
           (before (copy-tree sd)))
      (should-error
       (jabber-sm--process-ack sd `(a (,@(when h `((h . ,h))))))
       :type 'jabber-sm-invalid-acknowledgement)
      (should (equal sd before)))))

(ert-deftest jabber-test-sm-invalid-resumed-preserves-state ()
  "Invalid resumption evidence cannot mutate queues or counters."
  (dolist (stanza
           '((resumed ((xmlns . "urn:xmpp:sm:3") (previd . "s1")))
             (resumed ((xmlns . "urn:xmpp:sm:3")
                       (h . "1oops") (previd . "s1")))
             (resumed ((xmlns . "urn:xmpp:sm:3") (h . "2")))
             (resumed ((xmlns . "urn:xmpp:sm:3")
                       (h . "2") (previd . "other")))
             (resumed ((xmlns . "urn:xmpp:sm:3")
                       (h . "0") (previd . "s1")))))
    (let* ((sd (list :sm-id "s1"
                     :sm-outbound-count 2
                     :sm-last-acked 1
                     :sm-outbound-queue '((2 . second))
                     :sm-pending-queue '((0 . pending))
                     :sm-resuming t))
           (before (copy-tree sd)))
      (should-error (jabber-sm--handle-resumed sd stanza)
                    :type 'jabber-sm-invalid-acknowledgement)
      (should (equal sd before))))
  (let* ((sd (list :sm-id "s1"
                   :sm-outbound-count 2
                   :sm-last-acked 1
                   :sm-outbound-queue '((2 . second))))
         (before (copy-tree sd)))
    (should-error
     (jabber-sm--handle-resumed
      sd '(resumed ((h . "3") (previd . "s1"))))
     :type 'jabber-sm-handled-count-too-high)
    (should (equal sd before))))

(ert-deftest jabber-test-sm-invalid-failed-h-preserves-state ()
  "Malformed optional failed h evidence cannot reset or prune SM state."
  (dolist (h '("" "1oops" "-1" "4294967296"))
    (let* ((sd (list :sm-id "s1"
                     :sm-outbound-count 2
                     :sm-last-acked 0
                     :sm-outbound-queue '((1 . first) (2 . second))
                     :sm-pending-queue '((0 . pending))
                     :sm-resuming t))
           (before (copy-tree sd)))
      (should-error
       (jabber-sm--handle-failed-resume sd `(failed ((h . ,h))))
       :type 'jabber-sm-invalid-acknowledgement)
      (should (equal sd before)))))

(ert-deftest jabber-test-sm-session-rejects-ack-h-too-high ()
  "An impossible acknowledgement closes the stream with the XEP error."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-count 3
                   :sm-outbound-queue (list (cons 1 'a) (cons 2 'b) (cons 3 'c))
                   :sm-last-acked 0))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "99"))))
         (handler (gethash :session-established
                           (get 'jabber-connection :fsm-event)))
         sent)
    (cl-letf (((symbol-function 'jabber-send-string)
               (lambda (_jc string) (setq sent string))))
      (let ((result (funcall handler 'fake-jc sd (list :stanza ack) nil)))
        (should-not (car result))
        (should (string-match-p "handled-count-too-high" sent))
        (should (string-match-p "h='99'" sent))
        (should (string-match-p "send-count='3'" sent))
        (should (string-match-p "acknowledged 99 stanzas"
                                (plist-get (cadr result)
                                           :disconnection-reason)))))))

(ert-deftest jabber-test-sm-too-high-send-failure-still-closes ()
  "Failure to emit the over-ack stream error cannot leave the stream open."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-count 2
                   :sm-last-acked 0
                   :sm-outbound-queue '((1 . first) (2 . second))))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "3"))))
         (handler (gethash :session-established
                           (get 'jabber-connection :fsm-event)))
         result)
    (cl-letf (((symbol-function 'jabber-send-string)
               (lambda (&rest _) (error "transport closed"))))
      (setq result
            (condition-case nil
                (funcall handler 'fake-jc sd (list :stanza ack) nil)
              (error 'escaped))))
    (should (listp result))
    (should-not (car result))
    (should (string-match-p "acknowledged 3 stanzas"
                            (plist-get (cadr result)
                                       :disconnection-reason)))))

(ert-deftest jabber-test-sm-session-rejects-malformed-ack ()
  "A malformed ordinary acknowledgement closes without pruning queues."
  (let* ((outbound '((1 . first) (2 . second)))
         (pending '((0 . pending)))
         (sd (list :sm-enabled t
                   :sm-outbound-count 2
                   :sm-last-acked 0
                   :sm-outbound-queue outbound
                   :sm-pending-queue pending))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "1oops"))))
         (handler (gethash :session-established
                           (get 'jabber-connection :fsm-event)))
         sent drained)
    (cl-letf (((symbol-function 'jabber-send-string)
               (lambda (_jc string) (setq sent string)))
              ((symbol-function 'jabber-sm--drain-pending)
               (lambda (&rest _) (setq drained t))))
      (let ((result (funcall handler 'fake-jc sd (list :stanza ack) nil)))
        (should-not (car result))
        (should (equal (plist-get (cadr result) :sm-outbound-queue)
                       outbound))
        (should (equal (plist-get (cadr result) :sm-pending-queue)
                       pending))
        (should-not sent)
        (should-not drained)
        (should (string-match-p
                 "Invalid Stream Management acknowledgement"
                 (plist-get (cadr result) :disconnection-reason)))))))

(ert-deftest jabber-test-sm-resume-rejects-invalid-evidence ()
  "Invalid resumed and failed evidence closes without resetting session state."
  (dolist (stanza
           '((resumed ((xmlns . "urn:xmpp:sm:3")
                       (h . "3") (previd . "s1")))
             (failed ((xmlns . "urn:xmpp:sm:3") (h . "3")))
             (failed ((xmlns . "urn:xmpp:sm:3") (h . "1oops")))))
    (let* ((outbound '((1 . first) (2 . second)))
           (pending '((0 . pending)))
           (sd (list :sm-enabled t
                     :sm-id "s1"
                     :sm-outbound-count 2
                     :sm-last-acked 0
                     :sm-outbound-queue outbound
                     :sm-pending-queue pending
                     :sm-resuming t
                     :stream-features
                     `(features ()
                                (bind ((xmlns . ,jabber-bind-xmlns)))
                                (sm ((xmlns . ,jabber-sm-xmlns))))))
           (handler (gethash :sm-resume
                             (get 'jabber-connection :fsm-event)))
           sent reset)
      (cl-letf (((symbol-function 'jabber-send-string)
                 (lambda (_jc string) (setq sent string)))
                ((symbol-function 'jabber-lifecycle-dispatch-session-reset)
                 (lambda (_) (setq reset t))))
        (let ((result (funcall handler 'fake-jc sd
                               (list :stanza stanza) nil)))
          (should-not (car result))
          (should (equal (plist-get (cadr result) :sm-outbound-queue)
                         outbound))
          (should (equal (plist-get (cadr result) :sm-pending-queue)
                         pending))
          (should-not reset)
          (if (equal (jabber-xml-get-attribute stanza 'h) "3")
              (should (string-match-p "handled-count-too-high" sent))
            (should-not sent)))))))

;;; Back-pressure

(ert-deftest jabber-test-sm-in-flight-count-normal ()
  "In-flight count is delta between outbound and last-acked."
  (let ((sd (list :sm-outbound-count 7 :sm-last-acked 3)))
    (should (= (jabber-sm--in-flight-count sd) 4))))

(ert-deftest jabber-test-sm-in-flight-count-zero ()
  "In-flight count is zero when fully acked."
  (let ((sd (list :sm-outbound-count 5 :sm-last-acked 5)))
    (should (= (jabber-sm--in-flight-count sd) 0))))

(ert-deftest jabber-test-sm-in-flight-count-wraparound ()
  "In-flight count handles 2^32 wraparound."
  (let ((sd (list :sm-outbound-count 2
                  :sm-last-acked (- (expt 2 32) 3))))
    (should (= (jabber-sm--in-flight-count sd) 5))))

(ert-deftest jabber-test-sm-should-queue-p-at-limit ()
  "Should queue when in-flight equals the cap."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should (jabber-sm--should-queue-p
             sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-test-sm-should-queue-p-below-limit ()
  "Should not queue when in-flight is below the cap."
  (let ((jabber-sm-max-in-flight 10)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-test-sm-should-queue-p-disabled ()
  "Should not queue when back-pressure is disabled."
  (let ((jabber-sm-max-in-flight nil)
        (sd (list :sm-enabled t :sm-outbound-count 100 :sm-last-acked 0)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-test-sm-should-queue-p-sm-off ()
  "Should not queue when SM is not enabled."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled nil :sm-outbound-count 5 :sm-last-acked 0)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-test-sm-should-queue-p-non-stanza ()
  "Should not queue non-countable elements."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should-not (jabber-sm--should-queue-p
                 sd '(r ((xmlns . "urn:xmpp:sm:3")))))))

(ert-deftest jabber-test-sm-enqueue-pending ()
  "Enqueue appends to pending queue as (priority . sexp) pairs."
  (let* ((sd (list :sm-pending-queue nil))
         (msg1 '(message ((to . "a@b")) (body () "1")))
         (msg2 '(message ((to . "a@b")) (body () "2"))))
    (setq sd (jabber-sm--enqueue-pending sd msg1))
    (setq sd (jabber-sm--enqueue-pending sd msg2))
    (should (= (length (plist-get sd :sm-pending-queue)) 2))
    (should (equal (cdar (plist-get sd :sm-pending-queue)) msg1))
    (should (equal (cdadr (plist-get sd :sm-pending-queue)) msg2))
    ;; Both are messages, priority 0
    (should (= (caar (plist-get sd :sm-pending-queue)) 0))
    (should (= (caadr (plist-get sd :sm-pending-queue)) 0))))

(ert-deftest jabber-test-sm-drain-pending-partial ()
  "Drain sends stanzas up to the cap, leaving the rest queued."
  (let* ((jabber-sm-max-in-flight 2)
         (msg1 '(message ((to . "a@b")) (body () "1")))
         (msg2 '(message ((to . "a@b")) (body () "2")))
         (msg3 '(message ((to . "a@b")) (body () "3")))
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-inbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue (list (cons 0 msg1)
                                           (cons 0 msg2)
                                           (cons 0 msg3))))
         (sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    ;; Should have sent exactly 2 (the cap)
    (should (= (length sent) 2))
    ;; One remains in pending queue
    (should (= (length (plist-get sd :sm-pending-queue)) 1))
    (should (equal (cdar (plist-get sd :sm-pending-queue)) msg3))
    ;; Outbound count incremented for sent stanzas
    (should (= (plist-get sd :sm-outbound-count) 2))))

(ert-deftest jabber-test-sm-drain-pending-counts-once ()
  "Draining counts each transmitted stanza exactly once."
  (let* ((jabber-sm-max-in-flight 2)
         (msg '(message ((to . "a@b")) (body () "1")))
         (sd (list :sm-enabled t
                   :sm-outbound-count 10
                   :sm-last-acked 9
                   :sm-outbound-queue nil
                   :sm-pending-queue (list (cons 0 msg))))
         (sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (should (equal sent (list msg)))
    (should (= (plist-get sd :sm-outbound-count) 11))
    (should (= (length (plist-get sd :sm-outbound-queue)) 1))
    (should (= (caar (plist-get sd :sm-outbound-queue)) 11))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-test-sm-drain-pending-empty ()
  "Drain with empty queue is a no-op."
  (let* ((jabber-sm-max-in-flight 10)
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue nil))
         (sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (should (null sent))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-test-sm-reset-clears-pending-queue ()
  "Reset clears the pending queue."
  (let* ((sd (list :sm-enabled t :sm-pending-queue '((0 . a) (1 . b) (2 . c))))
         (result (jabber-sm--reset sd)))
    (should (null (plist-get result :sm-pending-queue)))))

(ert-deftest jabber-test-sm-drain-runs-transport-success-callback ()
  "A queued stanza commits only after queue drain writes it."
  (let* ((jabber-sm-max-in-flight nil)
         (successes 0)
         (failures 0)
         (msg '(message ((to . "a@b")) (body () "queued")))
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg
           (lambda () (cl-incf successes))
           (lambda (_reason) (cl-incf failures))))
    (should (= 0 successes))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (&rest _) nil)))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (should (= 1 successes))
    (should (= 0 failures))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-test-sm-drain-runs-transport-failure-callback ()
  "A failed queue-drain write fails once and removes that entry."
  (let* ((jabber-sm-max-in-flight nil)
         (successes 0)
         (failures 0)
         (msg '(message ((to . "a@b")) (body () "queued")))
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg
           (lambda () (cl-incf successes))
           (lambda (_reason) (cl-incf failures))))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (&rest _) (error "transport failed"))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (should (= 0 successes))
    (should (= 1 failures))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-test-sm-discard-pending-isolates-failure-callbacks ()
  "Discard runs every failure callback even when one callback errors."
  (let* ((called nil)
         (msg '(message ((to . "a@b")) (body () "queued")))
         (sd (list :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg nil
           (lambda (_reason)
             (push 'first called)
             (error "restoration failed"))))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg nil
           (lambda (_reason) (push 'second called))))
    (setq sd (jabber-sm--discard-pending sd "session reset"))
    (should (equal called '(second first)))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-test-lifecycle-reset-isolates-error-and-quit ()
  "A failing reset hook cannot skip later reset effects."
  (dolist (condition '(error quit))
    (let* ((calls nil)
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_jc)
                    (push 'first calls)
                    (signal condition nil))
                  (lambda (_jc) (push 'second calls)))))
      (should-not
       (condition-case nil
           (progn
             (jabber-lifecycle-dispatch-session-reset 'fake-jc)
             nil)
         ((error quit) t)))
      (should (equal calls '(second first))))))

(ert-deftest jabber-test-lifecycle-reset-preserves-local-hook-semantics ()
  "Reset dispatch honors local hooks that include global functions."
  (let ((calls nil)
        (saved (default-value
                'jabber-lifecycle-session-reset-functions)))
    (unwind-protect
        (progn
          (setq-default jabber-lifecycle-session-reset-functions
                        (list (lambda (_jc) (push 'global calls))))
          (with-temp-buffer
            (setq-local jabber-lifecycle-session-reset-functions
                        (list (lambda (_jc) (push 'local calls)) t))
            (jabber-lifecycle-dispatch-session-reset 'fake-jc))
          (should (equal calls '(global local))))
      (setq-default jabber-lifecycle-session-reset-functions saved))))

(ert-deftest jabber-test-sm-drain-isolates-success-callback-quit ()
  "A quitting success callback cannot skip later queued work."
  (let* ((jabber-sm-max-in-flight nil)
         (sent nil)
         (later 0)
         (first '(message ((to . "first@example.org"))))
         (second '(message ((to . "second@example.org"))))
         (sd (jabber-sm--reset nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd first (lambda () (signal 'quit nil)) nil))
    (setq sd
          (jabber-sm--enqueue-pending
           sd second (lambda () (cl-incf later)) nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc stanza) (push stanza sent))))
      (should-not
       (condition-case nil
           (progn
             (setq sd (jabber-sm--drain-pending 'fake-jc sd))
             nil)
         (quit t))))
    (should (= later 1))
    (should (equal (nreverse sent) (list first second)))
    (should-not (plist-get sd :sm-pending-queue))))

(ert-deftest jabber-test-sm-discard-isolates-failure-callback-quit ()
  "A quitting failure callback cannot skip later settlements."
  (let* ((later 0)
         (msg '(message ((to . "a@example.org"))))
         (sd (jabber-sm--reset nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg nil (lambda (_reason) (signal 'quit nil))))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg nil (lambda (_reason) (cl-incf later))))
    (should-not
     (condition-case nil
         (progn
           (setq sd (jabber-sm--discard-pending sd "terminal"))
           nil)
       (quit t)))
    (should (= later 1))
    (should-not (plist-get sd :sm-pending-queue))))

(ert-deftest jabber-test-sm-discard-detaches-before-callbacks ()
  "Discard removes live queue ownership before failure callbacks."
  (let* ((observed 'unset)
         (msg '(message ((to . "a@example.org"))))
         (sd (jabber-sm--reset nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd msg nil
           (lambda (_reason)
             (setq observed (plist-get sd :sm-pending-queue)))))
    (setq sd (jabber-sm--discard-pending sd "terminal"))
    (should-not observed)
    (should-not (plist-get sd :sm-pending-queue))))

(ert-deftest jabber-test-lifecycle-list-change-isolates-error-and-quit ()
  "A failing list-change hook cannot skip later notifications."
  (dolist (condition '(error quit))
    (let* ((calls nil)
           (jabber-lifecycle-connection-list-changed-functions
            (list (lambda ()
                    (push 'first calls)
                    (signal condition nil))
                  (lambda () (push 'second calls)))))
      (should-not
       (condition-case nil
           (progn
             (jabber-lifecycle-dispatch-connection-list-changed)
             nil)
         ((error quit) t)))
      (should (equal calls '(second first))))))

(ert-deftest jabber-test-sm-terminal-loss-detaches-before-callbacks ()
  "Terminal loss retires the old connection before reset and failure effects."
  (let* ((jc (make-symbol "terminal-loss"))
         (replacement (make-symbol "replacement"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect nil)
         (jabber-connections (list jc))
         (jabber-lost-connection-hooks nil)
         (reset-observation nil)
         (failure-observation nil)
         (failures 0)
         (sd (list :username "user" :server "example.org" :resource "emacs"
                   :connection transport :ever-session-established t
                   :sm-enabled nil :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd '(message ((to . "peer@example.org"))) nil
           (lambda (_reason)
             (cl-incf failures)
             (setq failure-observation
                   (list (memq jc jabber-connections)
                         (plist-get (fsm-get-state-data jc) :connection)
                         (plist-get (fsm-get-state-data jc)
                                    :sm-pending-queue))))))
    (let ((jabber-lifecycle-session-reset-functions
           (list (lambda (_connection)
                   (setq reset-observation
                         (list (memq jc jabber-connections)
                               (plist-get (fsm-get-state-data jc) :connection)
                               (plist-get (fsm-get-state-data jc)
                                          :sm-pending-queue)))
                   (push replacement jabber-connections)))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data sd)
      (fsm-send-sync jc (list :connection-dead transport "lost")))
    (should (equal reset-observation '(nil nil nil)))
    (should (equal failure-observation '(nil nil nil)))
    (should (= failures 1))
    (should-not (memq jc jabber-connections))
    (should (memq replacement jabber-connections))
    (should-not (plist-get (fsm-get-state-data jc) :connection))
    (should-not (plist-get (fsm-get-state-data jc) :sm-pending-queue))))

(ert-deftest jabber-test-sm-terminal-resumable-loss-without-retry-discards ()
  "Resumable work is terminally settled when no retry owner exists."
  (let* ((jc (make-symbol "terminal-resumable"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect nil)
         (jabber-connections (list jc))
         (jabber-lost-connection-hooks nil)
         (jabber-lifecycle-session-reset-functions nil)
         (failures 0)
         (sd (list :username "user" :server "example.org" :resource "emacs"
                   :connection transport :ever-session-established t
                   :sm-enabled t :sm-id "session" :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd '(message ((to . "peer@example.org"))) nil
           (lambda (_reason) (cl-incf failures))))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data sd)
    (fsm-send-sync jc (list :connection-dead transport "lost"))
    (should (= failures 1))
    (should-not (plist-get (fsm-get-state-data jc) :sm-enabled))
    (should-not (plist-get (fsm-get-state-data jc) :sm-pending-queue))
    (should-not (memq jc jabber-connections))
    (should-not (get jc :timeout))))

(ert-deftest jabber-test-sm-cancel-retry-detaches-before-callbacks ()
  "Cancelling a retry retires old ownership before arbitrary effects."
  (let* ((jc (make-symbol "cancel-retry"))
         (replacement (make-symbol "replacement"))
         (transport (make-symbol "stale-transport"))
         (timer (run-with-timer 3600 nil #'ignore))
         (sm-timer (run-with-timer 3600 nil #'ignore))
         (jabber-connections (list jc))
         (jabber-lost-connection-hooks nil)
         (reset-observation nil)
         (failure-observation nil)
         (failures 0)
         (sd (list :username "user" :server "example.org" :resource "emacs"
                   :connection transport :disconnection-expected nil
                   :sm-enabled nil :sm-r-timer sm-timer
                   :sm-pending-queue nil)))
    (unwind-protect
        (progn
          (setq sd
                (jabber-sm--enqueue-pending
                 sd '(message ((to . "peer@example.org"))) nil
                 (lambda (_reason)
                   (cl-incf failures)
                   (setq failure-observation
                         (list (memq jc jabber-connections)
                               (get jc :timeout)
                               (plist-get (fsm-get-state-data jc) :sm-r-timer)
                               (memq sm-timer timer-list)
                               (plist-get (fsm-get-state-data jc) :connection)
                               (plist-get (fsm-get-state-data jc)
                                          :sm-pending-queue))))))
          (put jc :name 'jabber-connection)
          (put jc :state nil)
          (put jc :state-data sd)
          (put jc :timeout timer)
          (let ((jabber-lifecycle-session-reset-functions
                 (list (lambda (_connection)
                         (setq reset-observation
                               (list (memq jc jabber-connections)
                                     (get jc :timeout)
                                     (plist-get (fsm-get-state-data jc)
                                                :sm-r-timer)
                                     (memq sm-timer timer-list)
                                     (plist-get (fsm-get-state-data jc)
                                                :connection)
                                     (plist-get (fsm-get-state-data jc)
                                                :sm-pending-queue)))
                         (push replacement jabber-connections)))))
            (jabber-disconnect-one jc))
          (should (equal reset-observation '(nil nil nil nil nil nil)))
          (should (equal failure-observation '(nil nil nil nil nil nil)))
          (should (= failures 1))
          (should-not (memq jc jabber-connections))
          (should (memq replacement jabber-connections))
          (should-not (get jc :timeout))
          (should-not (plist-get (fsm-get-state-data jc) :sm-r-timer))
          (should-not (memq sm-timer timer-list))
          (should-not (plist-get (fsm-get-state-data jc) :connection))
          (should-not (plist-get (fsm-get-state-data jc)
                                 :sm-pending-queue)))
      (when (timerp timer)
        (cancel-timer timer))
      (when (timerp sm-timer)
        (cancel-timer sm-timer)))))

(ert-deftest jabber-test-sm-terminal-lost-hooks-isolate-error-and-quit ()
  "A failing lost-connection hook cannot skip later loss effects."
  (dolist (condition '(error quit))
    (let* ((jc (make-symbol "terminal-lost-hook"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect nil)
           (jabber-connections (list jc))
           (jabber-lifecycle-session-reset-functions nil)
           (later-loss 0)
           (sd (list :username "user" :server "example.org" :resource "emacs"
                     :connection transport :ever-session-established t
                     :sm-enabled nil :sm-pending-queue nil))
           (jabber-lost-connection-hooks
            (list (lambda (_connection) (signal condition nil))
                  (lambda (_connection) (cl-incf later-loss)))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data sd)
      (should-not
       (condition-case nil
           (progn
             (fsm-send-sync jc (list :connection-dead transport "lost"))
             nil)
         ((error quit) t)))
      (should (= later-loss 1))
      (should-not (memq jc jabber-connections)))))

(ert-deftest jabber-test-sm-terminal-transport-cleanup-cannot-abort ()
  "Transport cleanup error or quit cannot interrupt terminal settlement."
  (dolist (case '((delete error) (delete quit)
                  (buffer error) (buffer quit)))
    (pcase-let* ((`(,source ,condition) case)
                 (jc (make-symbol "terminal-transport-cleanup"))
                 (transport (make-symbol "transport"))
                 (buffer (generate-new-buffer " *jabber-terminal-cleanup*"))
                 (original-kill-buffer (symbol-function 'kill-buffer))
                 (jabber-auto-reconnect nil)
                 (jabber-debug-keep-process-buffers nil)
                 (jabber-connections (list jc))
                 (jabber-lost-connection-hooks nil)
                 (resets 0)
                 (failures 0)
                 (buffer-cleanups 0)
                 (jabber-lifecycle-session-reset-functions
                  (list (lambda (_connection) (cl-incf resets))))
                 (sd (list :username "user" :server "example.org"
                           :resource "emacs" :connection transport
                           :ever-session-established t :sm-enabled nil
                           :sm-pending-queue nil)))
      (setq sd
            (jabber-sm--enqueue-pending
             sd '(message ((to . "peer@example.org"))) nil
             (lambda (_reason) (cl-incf failures))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data sd)
      (cl-letf (((symbol-function 'processp)
                 (lambda (object) (eq object transport)))
                ((symbol-function 'process-buffer)
                 (lambda (_process) buffer))
                ((symbol-function 'delete-process)
                 (lambda (_process)
                   (when (eq source 'delete)
                     (signal condition nil))))
                ((symbol-function 'kill-buffer)
                 (lambda (target)
                   (if (eq target buffer)
                       (progn
                         (cl-incf buffer-cleanups)
                         (when (eq source 'buffer)
                           (signal condition nil)))
                     (funcall original-kill-buffer target)))))
        (should-not
         (condition-case nil
             (progn
               (fsm-send-sync jc (list :connection-dead transport "lost"))
               nil)
           ((error quit) t))))
      (should (= buffer-cleanups 1))
      (should (= resets 1))
      (should (= failures 1))
      (should-not (memq jc jabber-connections))
      (should-not (plist-get (fsm-get-state-data jc) :connection))
      (should-not (plist-get (fsm-get-state-data jc)
                             :sm-pending-queue))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-reentrant-cancel-prevents-orphan-retry ()
  "Cancelling from retry callbacks cannot re-arm an orphan FSM."
  (dolist (source '(reset pending lost))
    (let* ((jc (make-symbol "reentrant-cancel"))
           (replacement (make-symbol "replacement"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect t)
           (jabber-reconnect-delay 300)
           (jabber-connections (list jc))
           (cancelled nil)
           (resets 0)
           (failures 0)
           (losses 0)
           (list-changes 0)
           (cancel
            (lambda ()
              (unless cancelled
                (setq cancelled t)
                (push replacement jabber-connections)
                (jabber-disconnect-one jc))))
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_connection)
                    (cl-incf resets)
                    (when (eq source 'reset)
                      (funcall cancel)))))
           (jabber-lifecycle-connection-list-changed-functions
            (list (lambda () (cl-incf list-changes))))
           (jabber-lost-connection-hooks
            (list (lambda (_connection)
                    (cl-incf losses)
                    (when (eq source 'lost)
                      (funcall cancel)))))
           (sd (list :username "user" :server "example.org" :resource "emacs"
                     :connection transport :ever-session-established t
                     :disconnection-expected nil
                     :sm-enabled (eq source 'lost)
                     :sm-id (and (eq source 'lost) "session")
                     :sm-pending-queue nil)))
      (setq sd
            (jabber-sm--enqueue-pending
             sd '(message ((to . "peer@example.org"))) nil
             (lambda (_reason)
               (cl-incf failures)
               (when (eq source 'pending)
                 (funcall cancel)))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data sd)
      (unwind-protect
          (progn
            (fsm-send-sync jc (list :connection-dead transport "lost"))
            (should-not (get jc :state))
            (should-not (get jc :timeout))
            (should-not (memq jc jabber-connections))
            (should (memq replacement jabber-connections))
            (should (plist-get (fsm-get-state-data jc) :terminalized))
            (should (plist-get (fsm-get-state-data jc)
                               :disconnection-expected))
            (should (= failures 1))
            (should (= resets 1))
            (should (= losses 1))
            (should (= list-changes 1)))
        (when (timerp (get jc :timeout))
          (cancel-timer (get jc :timeout)))))))

(ert-deftest jabber-test-sm-terminal-reentry-is-once-only ()
  "Terminal callbacks cannot repeat retirement effects."
  (dolist (source '(reset pending list lost))
    (let* ((jc (make-symbol "terminal-reentry"))
           (replacement (make-symbol "replacement"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect nil)
           (jabber-connections (list jc))
           (triggered nil)
           (resets 0)
           (failures 0)
           (losses 0)
           (list-changes 0)
           (reenter
            (lambda ()
              (unless triggered
                (setq triggered t)
                (push replacement jabber-connections)
                (jabber-disconnect-one jc))))
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_connection)
                    (cl-incf resets)
                    (when (eq source 'reset)
                      (funcall reenter)))))
           (jabber-lifecycle-connection-list-changed-functions
            (list (lambda ()
                    (cl-incf list-changes)
                    (when (eq source 'list)
                      (funcall reenter)))))
           (jabber-lost-connection-hooks
            (list (lambda (_connection)
                    (cl-incf losses)
                    (when (eq source 'lost)
                      (funcall reenter)))))
           (sd (list :username "user" :server "example.org" :resource "emacs"
                     :connection transport :ever-session-established t
                     :disconnection-expected nil :sm-enabled nil
                     :sm-pending-queue nil)))
      (setq sd
            (jabber-sm--enqueue-pending
             sd '(message ((to . "peer@example.org"))) nil
             (lambda (_reason)
               (cl-incf failures)
               (when (eq source 'pending)
                 (funcall reenter)))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data sd)
      (fsm-send-sync jc (list :connection-dead transport "lost"))
      (should-not (get jc :state))
      (should-not (get jc :timeout))
      (should-not (memq jc jabber-connections))
      (should (memq replacement jabber-connections))
      (should (plist-get (fsm-get-state-data jc) :terminalized))
      (should (= resets 1))
      (should (= failures 1))
      (should (= losses 1))
      (should (= list-changes 1)))))

(ert-deftest jabber-test-sm-terminalization-does-not-mark-input-plist ()
  "Terminal retirement does not mark or detach an external plist alias."
  (dolist (entry '(active-loss nil-disconnect))
    (let* ((jc (make-symbol "terminal-state-copy"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect nil)
           (jabber-connections (list jc))
           (jabber-lifecycle-session-reset-functions nil)
           (jabber-lifecycle-connection-list-changed-functions nil)
           (jabber-lost-connection-hooks nil)
           (state-data
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection transport :ever-session-established t
                  :disconnection-expected nil :sm-enabled nil
                  :sm-pending-queue nil)))
      (put jc :name 'jabber-connection)
      (put jc :state (and (eq entry 'active-loss) :session-established))
      (put jc :state-data state-data)
      (if (eq entry 'active-loss)
          (fsm-send-sync jc (list :connection-dead transport "lost"))
        (jabber-disconnect-one jc))
      (should-not (plist-get state-data :terminalized))
      (should-not (plist-get state-data :disconnection-expected))
      (should (eq (plist-get state-data :connection) transport))
      (should (plist-get (fsm-get-state-data jc) :terminalized)))))

(defun jabber-test-sm--assert-unowned-resume-terminalized (source)
  "Assert resumable loss from unowned SOURCE is terminally settled."
  (let* ((jc (make-symbol "unowned-resume"))
         (replacement (make-symbol "replacement"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-connections
          (if (eq source 'entry) (list replacement) (list jc)))
         (resets 0)
         (failures 0)
         (list-changes 0)
         (lost-observation nil)
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection) (cl-incf resets))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda () (cl-incf list-changes))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection)
                  (when (eq source 'entry)
                    (let ((current (fsm-get-state-data jc)))
                      (setq lost-observation
                            (list (plist-get current :terminalized)
                                  (plist-get current :sm-enabled)
                                  (plist-get current :sm-pending-queue)
                                  failures resets list-changes))))
                  (when (eq source 'lost-hook)
                    (setq jabber-connections
                          (cons replacement
                                (delq jc jabber-connections)))))))
         (sd (list :username "user" :server "example.org" :resource "emacs"
                   :connection transport :ever-session-established t
                   :disconnection-expected nil
                   :sm-enabled t :sm-id "session"
                   :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd '(message ((to . "peer@example.org"))) nil
           (lambda (_reason) (cl-incf failures))))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data sd)
    (unwind-protect
        (progn
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (let ((current (fsm-get-state-data jc)))
            (should (plist-get current :terminalized))
            (should-not (plist-get current :sm-enabled))
            (should-not (plist-get current :sm-id))
            (should-not (plist-get current :sm-resuming))
            (should-not (plist-get current :sm-pending-queue)))
          (should (= resets 1))
          (should (= failures 1))
          (should (= list-changes 1))
          (should-not (get jc :timeout))
          (should-not (memq jc jabber-connections))
          (should (memq replacement jabber-connections)))
      (when (eq source 'entry)
        (should (equal lost-observation '(t nil nil 1 1 1))))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout))))))

(ert-deftest jabber-test-sm-resumable-retry-requires-owner-at-entry ()
  "Resumable state without an entry retry owner is terminally settled."
  (jabber-test-sm--assert-unowned-resume-terminalized 'entry))

(ert-deftest jabber-test-sm-resumable-retry-settles-lost-owner ()
  "Resumable state whose retry owner is lost before return is settled."
  (jabber-test-sm--assert-unowned-resume-terminalized 'lost-hook))

(ert-deftest jabber-test-sm-nonresumable-lost-owner-does-not-reset-twice ()
  "Late terminalization reuses an already completed session reset."
  (let* ((jc (make-symbol "lost-fresh-owner"))
         (replacement (make-symbol "replacement"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-connections (list jc))
         (resets 0)
         (failures 0)
         (list-changes 0)
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection) (cl-incf resets))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda () (cl-incf list-changes))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection)
                  (setq jabber-connections (list replacement)))))
         (sd (list :username "user" :server "example.org" :resource "emacs"
                   :connection transport :ever-session-established t
                   :disconnection-expected nil :sm-enabled nil
                   :sm-pending-queue nil)))
    (setq sd
          (jabber-sm--enqueue-pending
           sd '(message ((to . "peer@example.org"))) nil
           (lambda (_reason) (cl-incf failures))))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data sd)
    (fsm-send-sync jc (list :connection-dead transport "lost"))
    (should (= resets 1))
    (should (= failures 1))
    (should (= list-changes 1))
    (should-not (get jc :timeout))
    (should-not (memq jc jabber-connections))
    (should (memq replacement jabber-connections))
    (should (plist-get (fsm-get-state-data jc) :terminalized))))

(ert-deftest jabber-test-sm-transport-cleanup-cancel-is-once-only ()
  "Cancellation from transport cleanup retires old ownership once."
  (let* ((jc (make-symbol "cleanup-cancel"))
         (replacement (make-symbol "replacement"))
         (buffer (generate-new-buffer " *jabber-cleanup-cancel*"))
         (transport (make-pipe-process :name "jabber-cleanup-cancel"
                                       :buffer buffer :noquery t))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-debug-keep-process-buffers nil)
         (jabber-connections (list jc))
         (triggered nil)
         (resets 0)
         (failures 0)
         (losses 0)
         (list-changes 0)
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection) (cl-incf resets))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda () (cl-incf list-changes))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection) (cl-incf losses))))
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :ever-session-established t
                :disconnection-expected nil :sm-enabled t :sm-id "session"
                :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((to . "peer@example.org"))) nil
           (lambda (_reason) (cl-incf failures))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (unless triggered
                    (setq triggered t)
                    (push replacement jabber-connections)
                    (jabber-disconnect-one jc)))
                nil t))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (progn
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (let ((current (fsm-get-state-data jc)))
            (should (plist-get current :terminalized))
            (should (plist-get current :disconnection-expected))
            (should-not (plist-get current :sm-pending-queue))
            (should-not (plist-get current :nil-entry-pending)))
          (should (= resets 1))
          (should (= failures 1))
          (should (= losses 1))
          (should (= list-changes 1))
          (should-not (memq jc jabber-connections))
          (should (memq replacement jabber-connections))
          (should-not (get jc :timeout))
          (should-not (buffer-live-p buffer)))
      (when (process-live-p transport)
        (delete-process transport))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-transport-cleanup-timeout-waits-for-retry-lease ()
  "Transport cleanup cannot reconnect before the retry lease is armed."
  (let* ((jc (make-symbol "cleanup-retry"))
         (buffer (generate-new-buffer " *jabber-cleanup-retry*"))
         (transport (make-pipe-process :name "jabber-cleanup-retry"
                                       :buffer buffer :noquery t))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-debug-keep-process-buffers nil)
         (jabber-connections (list jc))
         (triggered nil)
         (reconnects 0)
         (resets 0)
         (failures 0)
         (losses 0)
         (list-changes 0)
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection) (cl-incf resets))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda () (cl-incf list-changes))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection) (cl-incf losses))))
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :connection-type 'network
                :network-server "example.org" :port 5222
                :ever-session-established t :disconnection-expected nil
                :sm-enabled nil :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((to . "peer@example.org"))) nil
           (lambda (_reason) (cl-incf failures))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (unless triggered
                    (setq triggered t)
                    (fsm-send-sync jc :timeout)))
                nil t))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-get-connect-function)
                   (lambda (_type)
                     (lambda (&rest _) (cl-incf reconnects)))))
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (should-not (get jc :state))
          (should (= reconnects 0))
          (should (= resets 1))
          (should (= failures 1))
          (should (= losses 1))
          (should (= list-changes 0))
          (should (memq jc jabber-connections))
          (should-not (plist-get (fsm-get-state-data jc)
                                 :sm-pending-queue))
          (should (timerp (get jc :timeout)))
          (fsm-send-sync jc :timeout)
          (should (eq (get jc :state) :connecting))
          (should (= reconnects 1))
          (should-not (get jc :timeout))
          (should-not (buffer-live-p buffer)))
      (when (process-live-p transport)
        (delete-process transport))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-session-establishment-clears-reset-marker ()
  "A genuine session permits one later logical-session reset."
  (let* ((jc (make-symbol "fresh-session"))
         (token (cons nil nil))
         (held (list (make-symbol "held")))
         (state-data
          (list :session-reset-done t :nil-entry-token token
                :nil-entry-pending held
                :sm-resumed nil :sm-enabled nil :sm-pending-queue nil))
         (enter (gethash :session-established
                         (get 'jabber-connection :fsm-enter))))
    (put jc :state-data state-data)
    (cl-letf (((symbol-function 'jabber-lifecycle-dispatch-session-bootstrap)
               #'ignore)
              ((symbol-function 'jabber-sm--drain-pending)
               (lambda (_jc current) current)))
      (setq state-data (car (funcall enter jc state-data))))
    (should-not (plist-get state-data :session-reset-done))
    (should-not (plist-get state-data :nil-entry-token))
    (should-not (plist-get state-data :nil-entry-pending))
    (should-not (plist-get (fsm-get-state-data jc) :session-reset-done))
    (should-not (plist-get (fsm-get-state-data jc) :nil-entry-token))
    (should-not (plist-get (fsm-get-state-data jc) :nil-entry-pending))))

(ert-deftest jabber-test-sm-terminal-cleanup-keeps-cancellation ()
  "Terminal cleanup reentry preserves explicit cancellation authority."
  (dolist (mode '(no-retry lost-owner))
    (let* ((jc (make-symbol "terminal-cleanup"))
           (replacement (make-symbol "replacement"))
           (buffer (generate-new-buffer " *jabber-terminal-cleanup*"))
           (process (make-pipe-process
                     :name "jabber-terminal-cleanup" :buffer buffer
                     :noquery t))
           (jabber-auto-reconnect (not (eq mode 'no-retry)))
           (jabber-debug-keep-process-buffers nil)
           (jabber-connections
            (if (eq mode 'lost-owner) (list replacement) (list jc)))
           (triggered nil)
           (resets 0)
           (failures 0)
           (list-changes 0)
           (lost 0)
           reset-observations
           (jabber-lost-connection-hooks
            (list (lambda (_connection) (cl-incf lost))))
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_connection)
                    (cl-incf resets)
                    (push (plist-get (fsm-get-state-data jc)
                                     :disconnection-expected)
                          reset-observations))))
           (jabber-lifecycle-connection-list-changed-functions
            (list (lambda () (cl-incf list-changes))))
           (input
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection process :disconnection-expected nil
                  :ever-session-established t :sm-enabled nil
                  :sm-pending-queue nil))
           state-data)
      (setq state-data
            (jabber-sm--enqueue-pending
             input '(message ((to . "peer@example.org"))) nil
             (lambda (_reason) (cl-incf failures))))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (unless triggered
                      (setq triggered t)
                      (cl-pushnew replacement jabber-connections)
                      (jabber-disconnect-one jc)))
                  nil t))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state-data)
      (unwind-protect
          (progn
            (fsm-send-sync jc (list :connection-dead process "lost"))
            (let ((current (fsm-get-state-data jc)))
              (should triggered)
              (should (equal reset-observations '(t)))
              (should (plist-get current :disconnection-expected))
              (should (plist-get current :terminalized))
              (should-not (get jc :state))
              (should-not (memq jc jabber-connections))
              (should (memq replacement jabber-connections))
              (should (= resets 1))
              (should (= failures 1))
              (should (= list-changes 1))
              (should (= lost 1))))
        (when (timerp (get jc :timeout))
          (cancel-timer (get jc :timeout)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun jabber-test-sm--terminal-timeout-case (source)
  "Assert terminal timeout reentry from SOURCE stays inert."
  (let* ((jc (make-symbol "terminal-timeout"))
         (replacement (make-symbol "replacement"))
         (buffer (and (eq source 'cleanup)
                      (generate-new-buffer " *jabber-timeout-cleanup*")))
         (process (and buffer
                       (make-pipe-process
                        :name "jabber-timeout-cleanup" :buffer buffer
                        :noquery t)))
         (transport (or process (make-symbol "transport")))
         (jabber-auto-reconnect nil)
         (jabber-debug-keep-process-buffers nil)
         (jabber-connections (list jc replacement))
         (triggered 0)
         (reconnects 0)
         (resets 0)
         (failures 0)
         (list-changes 0)
         (lost 0)
         trigger
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection)
                  (cl-incf resets)
                  (when (eq source 'reset) (funcall trigger)))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda ()
                  (cl-incf list-changes)
                  (when (eq source 'list) (funcall trigger)))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection)
                  (cl-incf lost)
                  (when (eq source 'lost) (funcall trigger)))))
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :disconnection-expected nil
                :ever-session-established t :sm-enabled nil
                :sm-pending-queue nil)))
    (setq trigger
          (lambda ()
            (cl-incf triggered)
            (fsm-send-sync jc :timeout)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((to . "peer@example.org"))) nil
           (lambda (_reason)
             (cl-incf failures)
             (when (eq source 'pending) (funcall trigger)))))
    (when buffer
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  (lambda () (funcall trigger)) nil t)))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-get-connect-function)
                   (lambda (_connection-type)
                     (lambda (&rest _) (cl-incf reconnects)))))
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (when (eq source 'delayed) (funcall trigger))
          (let ((current (fsm-get-state-data jc)))
            (should (= triggered 1))
            (should (= reconnects 0))
            (should-not (get jc :state))
            (should (plist-get current :terminalized))
            (should-not (memq jc jabber-connections))
            (should (memq replacement jabber-connections))
            (should-not (get jc :timeout))))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-terminal-timeout-is-inert ()
  "Terminal reset, failure, list, lost, cleanup, and delayed timeouts are inert."
  (dolist (source '(reset pending list lost cleanup delayed))
    (jabber-test-sm--terminal-timeout-case source)))

(ert-deftest jabber-test-sm-timeout-requires-complete-retry-lease ()
  "Only a registered, tokened, nonterminal nil-state retry may reconnect."
  (dolist (case '(unregistered terminal expected missing-token
                  missing-timer live))
    (let* ((jc (make-symbol "retry-lease"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect t)
           (jabber-reconnect-delay 300)
           (jabber-connections (list jc))
           (jabber-lifecycle-session-reset-functions nil)
           (jabber-lifecycle-connection-list-changed-functions nil)
           (jabber-lost-connection-hooks nil)
           (reconnects 0)
           (state-data
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection transport :connection-type 'network
                  :network-server "example.org" :port 5222
                  :disconnection-expected nil :ever-session-established t
                  :sm-enabled nil :sm-pending-queue nil)))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state-data)
      (unwind-protect
          (cl-letf (((symbol-function 'jabber-get-connect-function)
                     (lambda (_connection-type)
                       (lambda (&rest _) (cl-incf reconnects)))))
            (fsm-send-sync jc (list :connection-dead transport "lost"))
            (pcase case
              ('unregistered
               (setq jabber-connections (delq jc jabber-connections)))
              ('terminal
               (plist-put (fsm-get-state-data jc) :terminalized t))
              ('expected
               (plist-put (fsm-get-state-data jc)
                          :disconnection-expected t))
              ('missing-token
               (plist-put (fsm-get-state-data jc) :nil-entry-token nil)))
            (when (eq case 'missing-timer)
              (fsm-stop-timer jc))
            (fsm-send-sync jc :timeout)
            (if (eq case 'live)
                (progn
                  (should (eq (get jc :state) :connecting))
                  (should (= reconnects 1)))
              (should-not (get jc :state))
              (should (= reconnects 0))))
        (when (timerp (get jc :timeout))
          (cancel-timer (get jc :timeout)))))))

(ert-deftest jabber-test-sm-reset-reentry-keeps-successor-queue ()
  "Reset and pending callbacks cannot erase same-plist successor work."
  (dolist (source '(reset pending))
    (let* ((jc (make-symbol "reset-successor"))
           (replacement (make-symbol "replacement"))
           (transport (make-symbol "transport"))
           (jabber-auto-reconnect t)
           (jabber-connections (list jc replacement))
           (resets 0)
           (old-failures 0)
           (successor-failures 0)
           (reconnects 0)
           (advanced nil)
           (same-object nil)
           successor-entries
           advance
           (jabber-lost-connection-hooks nil)
           (jabber-lifecycle-connection-list-changed-functions nil)
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_connection)
                    (cl-incf resets)
                    (when (eq source 'reset) (funcall advance)))))
           (state-data
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection transport :disconnection-expected nil
                  :ever-session-established t :sm-enabled nil
                  :sm-pending-queue nil)))
      (setq advance
            (lambda ()
              (unless advanced
                (setq advanced t)
                (fsm-send-sync jc :timeout)
                (let* ((current (fsm-get-state-data jc))
                       (first
                        (jabber-sm--enqueue-pending
                         current '(message ((id . "successor"))) nil
                         (lambda (_reason) (cl-incf successor-failures))))
                       (second
                        (jabber-sm--enqueue-pending
                         first '(message ((id . "successor"))) nil
                         (lambda (_reason) (cl-incf successor-failures)))))
                  (setq same-object (and (eq current first)
                                         (eq first second)))
                  (setq successor-entries
                        (copy-sequence
                         (plist-get second :sm-pending-queue)))
                  (put jc :state-data second)))))
      (setq state-data
            (jabber-sm--enqueue-pending
             state-data '(message ((id . "old"))) nil
             (lambda (_reason)
               (cl-incf old-failures)
               (when (eq source 'pending) (funcall advance)))))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state-data)
      (cl-letf (((symbol-function 'jabber-get-connect-function)
                 (lambda (_connection-type)
                   (lambda (&rest _) (cl-incf reconnects)))))
        (fsm-send-sync jc (list :connection-dead transport "lost"))
        (let ((current (fsm-get-state-data jc)))
          (should advanced)
          (should same-object)
          (should-not (get jc :state))
          (should (memq jc jabber-connections))
          (should (memq replacement jabber-connections))
          (should (= reconnects 0))
          (should (= resets 1))
          (should (= old-failures 1))
          (should (= successor-failures 0))
          (let ((pending (plist-get current :sm-pending-queue)))
            (should (= (length pending) 2))
            (should (eq (nth 0 pending) (nth 0 successor-entries)))
            (should (eq (nth 1 pending) (nth 1 successor-entries)))
            (should
             (equal (mapcar #'jabber-sm--pending-stanza pending)
                    '((message ((id . "successor")))
                      (message ((id . "successor")))))))
          (should (timerp (get jc :timeout)))
          (fsm-send-sync jc :timeout)
          (should (eq (get jc :state) :connecting))
          (should (= reconnects 1)))))))

(ert-deftest jabber-test-sm-expected-loss-does-not-retry ()
  "An expected active loss is terminal despite automatic reconnect."
  (let* ((jc (make-symbol "expected-loss"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect t)
         (jabber-connections (list jc))
         (jabber-lifecycle-session-reset-functions nil)
         (jabber-lifecycle-connection-list-changed-functions nil)
         (jabber-lost-connection-hooks nil)
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :disconnection-expected t
                :ever-session-established t :sm-enabled nil
                :sm-pending-queue nil)))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (fsm-send-sync jc (list :connection-dead transport "closed"))
    (should (plist-get (fsm-get-state-data jc) :terminalized))
    (should-not (memq jc jabber-connections))
    (should-not (get jc :timeout))))

(ert-deftest jabber-test-sm-initial-failure-does-not-retry ()
  "A never-established FSM is terminal despite automatic reconnect."
  (let* ((jc (make-symbol "initial-failure"))
         (jabber-auto-reconnect t)
         (jabber-connections (list jc))
         (jabber-lifecycle-session-reset-functions nil)
         (jabber-lifecycle-connection-list-changed-functions nil)
         (jabber-lost-connection-hooks nil)
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection nil :disconnection-expected nil
                :ever-session-established nil :sm-enabled nil
                :sm-pending-queue nil)))
    (put jc :name 'jabber-connection)
    (put jc :state :connecting)
    (put jc :state-data state-data)
    (fsm-send-sync jc '(:connection-failed ("no route")))
    (should (plist-get (fsm-get-state-data jc) :terminalized))
    (should-not (memq jc jabber-connections))
    (should-not (get jc :timeout))))

(ert-deftest jabber-test-sm-nested-nil-entry-preserves-new-retry-timer ()
  "A stale outer nil entry cannot replace a nested retry timer."
  (let* ((jc (make-symbol "nested-nil-entry"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-connections (list jc))
         (jabber-lost-connection-hooks nil)
         (nested nil)
         (resets 0)
         (failures 0)
         (reconnects 0)
         old-token new-token nested-timer
         (jabber-lifecycle-session-reset-functions
          (list
           (lambda (_connection)
             (cl-incf resets)
             (unless nested
               (setq nested t)
               (setq old-token
                     (plist-get (fsm-get-state-data jc) :nil-entry-token))
               (fsm-start-timer jc 300)
               (fsm-send-sync jc :timeout)
               (fsm-send-sync jc '(:connection-failed ("nested")))
               (setq new-token
                     (plist-get (fsm-get-state-data jc) :nil-entry-token))
               (setq nested-timer (get jc :timeout))))))
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :connection-type 'network
                :network-server "example.org" :port 5222
                :disconnection-expected nil :ever-session-established t
                :sm-enabled nil :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((id . "old"))) nil
           (lambda (_reason) (cl-incf failures))))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-get-connect-function)
                   (lambda (_connection-type)
                     (lambda (&rest _) (cl-incf reconnects)))))
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (should nested)
          (should old-token)
          (should new-token)
          (should-not (eq old-token new-token))
          (should (eq (get jc :state) nil))
          (should (eq (get jc :timeout) nested-timer))
          (should (timerp nested-timer))
          (should (memq jc jabber-connections))
          (should (= reconnects 1))
          (should (= resets 1))
          (should (= failures 1)))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout))))))

(ert-deftest jabber-test-sm-cleanup-added-work-survives-session-reset ()
  "Cleanup-created successor occurrences survive old-session settlement."
  (dolist (mode '(retry terminal))
    (let* ((jc (make-symbol "cleanup-successor-work"))
           (buffer (generate-new-buffer " *jabber-cleanup-successor*"))
           (transport (make-pipe-process
                       :name "jabber-cleanup-successor" :buffer buffer
                       :noquery t))
           (jabber-auto-reconnect (eq mode 'retry))
           (jabber-reconnect-delay 300)
           (jabber-debug-keep-process-buffers nil)
           (jabber-connections (list jc))
           (jabber-lifecycle-session-reset-functions nil)
           (jabber-lifecycle-connection-list-changed-functions nil)
           (jabber-lost-connection-hooks nil)
           (old-failures 0)
           (successor-failures 0)
           successor-entries
           (state-data
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection transport :ever-session-established t
                  :disconnection-expected nil :sm-enabled nil
                  :sm-pending-queue nil)))
      (setq state-data
            (jabber-sm--enqueue-pending
             state-data '(message ((id . "old"))) nil
             (lambda (_reason) (cl-incf old-failures))))
      (with-current-buffer buffer
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (let* ((current (fsm-get-state-data jc))
                  (first
                   (jabber-sm--enqueue-pending
                    current '(message ((id . "successor"))) nil
                    (lambda (_reason) (cl-incf successor-failures))))
                  (second
                   (jabber-sm--enqueue-pending
                    first '(message ((id . "successor"))) nil
                    (lambda (_reason) (cl-incf successor-failures)))))
             (setq successor-entries
                   (copy-sequence (plist-get second :sm-pending-queue)))
             (put jc :state-data second)))
         nil t))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state-data)
      (unwind-protect
          (progn
            (fsm-send-sync jc (list :connection-dead transport "lost"))
            (should (= old-failures 1))
            (should (= successor-failures 0))
            (let ((pending
                   (plist-get (fsm-get-state-data jc) :sm-pending-queue)))
              (should (= (length pending) 2))
              (should (eq (nth 0 pending) (nth 0 successor-entries)))
              (should (eq (nth 1 pending) (nth 1 successor-entries)))
              (should
               (equal (mapcar #'jabber-sm--pending-stanza pending)
                      '((message ((id . "successor")))
                        (message ((id . "successor")))))))
            (if (eq mode 'retry)
                (progn
                  (should (memq jc jabber-connections))
                  (should (timerp (get jc :timeout))))
              (should-not (memq jc jabber-connections))
              (should-not (get jc :timeout))))
        (when (timerp (get jc :timeout))
          (cancel-timer (get jc :timeout)))
        (when (process-live-p transport)
          (delete-process transport))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-sm-resumable-cleanup-restores-old-before-successors ()
  "A surviving resumable retry restores old work before cleanup additions."
  (let* ((jc (make-symbol "resumable-cleanup-order"))
         (buffer (generate-new-buffer " *jabber-resumable-cleanup-order*"))
         (transport (make-pipe-process
                     :name "jabber-resumable-cleanup-order" :buffer buffer
                     :noquery t))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-debug-keep-process-buffers nil)
         (jabber-connections (list jc))
         (jabber-lifecycle-session-reset-functions nil)
         (jabber-lifecycle-connection-list-changed-functions nil)
         (jabber-lost-connection-hooks nil)
         (old-failures 0)
         (successor-failures 0)
         old-queue
         successor-entries
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :ever-session-established t
                :disconnection-expected nil :sm-enabled t :sm-id "session"
                :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((id . "old"))) nil
           (lambda (_reason) (cl-incf old-failures))))
    (setq old-queue (plist-get state-data :sm-pending-queue))
    (with-current-buffer buffer
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (let* ((current (fsm-get-state-data jc))
                (first
                 (jabber-sm--enqueue-pending
                  current '(message ((id . "successor-1"))) nil
                  (lambda (_reason) (cl-incf successor-failures))))
                (second
                 (jabber-sm--enqueue-pending
                  first '(message ((id . "successor-2"))) nil
                  (lambda (_reason) (cl-incf successor-failures)))))
           (setq successor-entries
                 (copy-sequence (plist-get second :sm-pending-queue)))
           (put jc :state-data second)))
       nil t))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (progn
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (let ((pending
                 (plist-get (fsm-get-state-data jc) :sm-pending-queue)))
            (should (= old-failures 0))
            (should (= successor-failures 0))
            (should (= (length old-queue) 1))
            (should (= (length pending) 3))
            (should (eq (nth 0 pending) (nth 0 old-queue)))
            (should (eq (nth 1 pending) (nth 0 successor-entries)))
            (should (eq (nth 2 pending) (nth 1 successor-entries)))
            (should
             (equal (mapcar #'jabber-sm--pending-stanza pending)
                    '((message ((id . "old")))
                      (message ((id . "successor-1")))
                      (message ((id . "successor-2")))))))
          (should (plist-get (fsm-get-state-data jc) :sm-resuming))
          (should (memq jc jabber-connections))
          (should (timerp (get jc :timeout)))
          (should-not (plist-get (fsm-get-state-data jc)
                                 :nil-entry-pending)))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout)))
      (when (process-live-p transport)
        (delete-process transport))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-resumable-callback-timeout-promotes-held-work ()
  "A callback-armed retry promotes held work before connecting."
  (let* ((jc (make-symbol "resumable-callback-timeout"))
         (buffer (generate-new-buffer " *jabber-resumable-timeout*"))
         (transport (make-pipe-process
                     :name "jabber-resumable-timeout" :buffer buffer
                     :noquery t))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-debug-keep-process-buffers nil)
         (jabber-connections (list jc))
         (jabber-lost-connection-hooks nil)
         (jabber-post-resume-hooks nil)
         (jabber-lifecycle-session-reset-functions nil)
         (jabber-lifecycle-connection-list-changed-functions nil)
         (old-failures 0)
         (successor-failures 0)
         (reconnects 0)
         old-queue old-entry successor-entries
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :connection-type 'network
                :network-server "example.org" :port 5222
                :disconnection-expected nil :ever-session-established t
                :sm-enabled t :sm-id "session" :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((id . "old"))) nil
           (lambda (_reason) (cl-incf old-failures))))
    (setq old-queue (plist-get state-data :sm-pending-queue))
    (setq old-entry (car old-queue))
    (with-current-buffer buffer
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (let* ((current (fsm-get-state-data jc))
                (first
                 (jabber-sm--enqueue-pending
                  current '(message ((id . "successor-1"))) nil
                  (lambda (_reason) (cl-incf successor-failures))))
                (second
                 (jabber-sm--enqueue-pending
                  first '(message ((id . "successor-2"))) nil
                  (lambda (_reason) (cl-incf successor-failures)))))
           (setq successor-entries
                 (copy-sequence (plist-get second :sm-pending-queue)))
           (put jc :state-data second)
           (fsm-start-timer jc 300)
           (fsm-send-sync jc :timeout)))
       nil t))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-get-connect-function)
                   (lambda (_connection-type)
                     (lambda (&rest _) (cl-incf reconnects)))))
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (let* ((current (fsm-get-state-data jc))
                 (pending (plist-get current :sm-pending-queue)))
            (should (eq (get jc :state) :connecting))
            (should (= reconnects 1))
            (should (= old-failures 0))
            (should (= successor-failures 0))
            (should (= (length old-queue) 1))
            (should (= (length pending) 3))
            (should (eq (nth 0 pending) old-entry))
            (should (eq (nth 1 pending) (nth 0 successor-entries)))
            (should (eq (nth 2 pending) (nth 1 successor-entries)))
            (should-not (plist-get current :nil-entry-pending))
            (setq current (copy-sequence current))
            (setq current (plist-put current :sm-resumed t))
            (fsm-update jc :session-established current nil))
          (let ((pending
                 (plist-get (fsm-get-state-data jc) :sm-pending-queue)))
            (should (= (length pending) 3))
            (should (eq (nth 0 pending) old-entry))
            (should (eq (nth 1 pending) (nth 0 successor-entries)))
            (should (eq (nth 2 pending) (nth 1 successor-entries)))
            (should-not (plist-get (fsm-get-state-data jc)
                                   :nil-entry-pending))))
      (let ((timer (plist-get (fsm-get-state-data jc) :sm-r-timer)))
        (when (timerp timer)
          (cancel-timer timer)))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout)))
      (when (process-live-p transport)
        (delete-process transport))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest jabber-test-sm-late-owner-loss-preserves-cleanup-work ()
  "Late retry-owner loss cannot consume cleanup-created successor work."
  (dolist (mode '(nonresumable resumable))
    (let* ((jc (make-symbol "late-owner-cleanup"))
           (replacement (make-symbol "replacement"))
           (buffer (generate-new-buffer " *jabber-late-owner-cleanup*"))
           (transport (make-pipe-process
                       :name "jabber-late-owner-cleanup" :buffer buffer
                       :noquery t))
           (jabber-auto-reconnect t)
           (jabber-reconnect-delay 300)
           (jabber-debug-keep-process-buffers nil)
           (jabber-connections (list jc))
           (resets 0)
           (list-changes 0)
           (losses 0)
           (old-failures 0)
           (successor-failures 0)
           successor-entries
           (jabber-lifecycle-session-reset-functions
            (list (lambda (_connection) (cl-incf resets))))
           (jabber-lifecycle-connection-list-changed-functions
            (list (lambda () (cl-incf list-changes))))
           (jabber-lost-connection-hooks
            (list (lambda (_connection)
                    (cl-incf losses)
                    (setq jabber-connections
                          (cons replacement (delq jc jabber-connections))))))
           (state-data
            (list :username "user" :server "example.org" :resource "emacs"
                  :connection transport :ever-session-established t
                  :disconnection-expected nil
                  :sm-enabled (eq mode 'resumable)
                  :sm-id (and (eq mode 'resumable) "session")
                  :sm-pending-queue nil)))
      (setq state-data
            (jabber-sm--enqueue-pending
             state-data '(message ((id . "old"))) nil
             (lambda (_reason) (cl-incf old-failures))))
      (with-current-buffer buffer
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (let* ((current (fsm-get-state-data jc))
                  (first
                   (jabber-sm--enqueue-pending
                    current '(message ((id . "successor"))) nil
                    (lambda (_reason) (cl-incf successor-failures))))
                  (second
                   (jabber-sm--enqueue-pending
                    first '(message ((id . "successor"))) nil
                    (lambda (_reason) (cl-incf successor-failures)))))
             (setq successor-entries
                   (copy-sequence (plist-get second :sm-pending-queue)))
             (put jc :state-data second)))
         nil t))
      (put jc :name 'jabber-connection)
      (put jc :state :session-established)
      (put jc :state-data state-data)
      (unwind-protect
          (progn
            (fsm-send-sync jc (list :connection-dead transport "lost"))
            (let* ((current (fsm-get-state-data jc))
                   (pending (plist-get current :sm-pending-queue)))
              (should (= old-failures 1))
              (should (= successor-failures 0))
              (should (= (length pending) 2))
              (should (eq (nth 0 pending) (nth 0 successor-entries)))
              (should (eq (nth 1 pending) (nth 1 successor-entries)))
              (should
               (equal (mapcar #'jabber-sm--pending-stanza pending)
                      '((message ((id . "successor")))
                        (message ((id . "successor"))))))
              (should (= resets 1))
              (should (= list-changes 1))
              (should (= losses 1))
              (should (plist-get current :terminalized))
              (should-not (plist-get current :sm-enabled))
              (should-not (plist-get current :sm-id)))
            (should-not (get jc :state))
            (should-not (get jc :timeout))
            (should-not (memq jc jabber-connections))
            (should (memq replacement jabber-connections)))
        (when (timerp (get jc :timeout))
          (cancel-timer (get jc :timeout)))
        (when (process-live-p transport)
          (delete-process transport))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-sm-late-list-cancellation-keeps-current-state ()
  "Late terminal list reentry preserves explicit cancellation state."
  (let* ((jc (make-symbol "late-list-cancel"))
         (replacement (make-symbol "replacement"))
         (transport (make-symbol "transport"))
         (jabber-auto-reconnect t)
         (jabber-reconnect-delay 300)
         (jabber-connections (list jc))
         (triggered nil)
         (resets 0)
         (failures 0)
         (list-changes 0)
         (losses 0)
         (jabber-lifecycle-session-reset-functions
          (list (lambda (_connection) (cl-incf resets))))
         (jabber-lifecycle-connection-list-changed-functions
          (list (lambda ()
                  (cl-incf list-changes)
                  (unless triggered
                    (setq triggered t)
                    (jabber-disconnect-one jc)))))
         (jabber-lost-connection-hooks
          (list (lambda (_connection)
                  (cl-incf losses)
                  (setq jabber-connections
                        (cons replacement (delq jc jabber-connections))))))
         (state-data
          (list :username "user" :server "example.org" :resource "emacs"
                :connection transport :ever-session-established t
                :disconnection-expected nil
                :sm-enabled t :sm-id "session"
                :sm-pending-queue nil)))
    (setq state-data
          (jabber-sm--enqueue-pending
           state-data '(message ((id . "old"))) nil
           (lambda (_reason) (cl-incf failures))))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data state-data)
    (unwind-protect
        (progn
          (fsm-send-sync jc (list :connection-dead transport "lost"))
          (let ((current (fsm-get-state-data jc)))
            (should triggered)
            (should (plist-get current :terminalized))
            (should (plist-get current :disconnection-expected))
            (should-not (plist-get current :sm-pending-queue)))
          (should (= resets 1))
          (should (= failures 1))
          (should (= list-changes 1))
          (should (= losses 1))
          (should-not (get jc :state))
          (should-not (get jc :timeout))
          (should-not (memq jc jabber-connections))
          (should (memq replacement jabber-connections)))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout))))))

;;; Priority queue

(ert-deftest jabber-test-sm-stanza-priority-message ()
  "Messages have priority 0."
  (should (= (jabber-sm--stanza-priority '(message ((to . "a@b")) (body () "hi"))) 0)))

(ert-deftest jabber-test-sm-stanza-priority-iq ()
  "IQs have priority 1."
  (should (= (jabber-sm--stanza-priority '(iq ((type . "get") (id . "1")))) 1)))

(ert-deftest jabber-test-sm-stanza-priority-presence ()
  "Presence has priority 2."
  (should (= (jabber-sm--stanza-priority '(presence ((to . "room@muc/nick")))) 2)))

(ert-deftest jabber-test-sm-drain-pending-priority-order ()
  "Drain sends messages before presence, preserving FIFO within class."
  (let* ((jabber-sm-max-in-flight nil)
         (pres1 '(presence ((to . "r1@muc/nick"))))
         (msg1 '(message ((to . "a@b")) (body () "1")))
         (pres2 '(presence ((to . "r2@muc/nick"))))
         (iq1 '(iq ((type . "get") (id . "1"))))
         (msg2 '(message ((to . "c@d")) (body () "2")))
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-inbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue (list (cons 2 pres1)
                                           (cons 0 msg1)
                                           (cons 2 pres2)
                                           (cons 1 iq1)
                                           (cons 0 msg2))))
         (sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (setq sent (nreverse sent))
    ;; Messages first (FIFO), then IQ, then presences (FIFO)
    (should (= (length sent) 5))
    (should (eq (car-safe (nth 0 sent)) 'message))
    (should (eq (car-safe (nth 1 sent)) 'message))
    (should (eq (car-safe (nth 2 sent)) 'iq))
    (should (eq (car-safe (nth 3 sent)) 'presence))
    (should (eq (car-safe (nth 4 sent)) 'presence))
    ;; FIFO within messages
    (should (equal (nth 0 sent) msg1))
    (should (equal (nth 1 sent) msg2))
    ;; FIFO within presence
    (should (equal (nth 3 sent) pres1))
    (should (equal (nth 4 sent) pres2))))

(ert-deftest jabber-test-sm-drain-pending-priority-partial ()
  "With a cap, messages drain first even if presence was enqueued first."
  (let* ((jabber-sm-max-in-flight 2)
         (pres1 '(presence ((to . "r1@muc/nick"))))
         (msg1 '(message ((to . "a@b")) (body () "urgent")))
         (pres2 '(presence ((to . "r2@muc/nick"))))
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-inbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue (list (cons 2 pres1)
                                           (cons 0 msg1)
                                           (cons 2 pres2))))
         (sent nil))
    (cl-letf (((symbol-function 'jabber-send-sexp--raw)
               (lambda (_jc sexp) (push sexp sent))))
      (setq sd (jabber-sm--drain-pending 'fake-jc sd)))
    (setq sent (nreverse sent))
    ;; Message sent first, then one presence
    (should (= (length sent) 2))
    (should (equal (nth 0 sent) msg1))
    (should (equal (nth 1 sent) pres1))
    ;; One presence remains
    (should (= (length (plist-get sd :sm-pending-queue)) 1))
    (should (equal (cdar (plist-get sd :sm-pending-queue)) pres2))))

;;; Transport logging

(ert-deftest jabber-test-stanza-log-calls-configured-sink ()
  "Forward enabled XML logs to the configured sink."
  (let ((jabber-debug-log-xml t)
        (jabber-stanza-log-function
         (lambda (jc direction data)
           (list jc direction data))))
    (should (equal (jabber-log-xml 'jc "sending" '(message))
                   '(jc "sending" (message))))))

(ert-deftest jabber-test-stanza-log-skips-disabled-sink ()
  "Do not call the stanza log sink when XML logging is disabled."
  (let* ((jabber-debug-log-xml nil)
         (called nil)
         (jabber-stanza-log-function (lambda (&rest _) (setq called t))))
    (jabber-log-xml 'jc "sending" '(message))
    (should-not called)))

(provide 'jabber-test-sm)

;;; jabber-test-sm.el ends here
