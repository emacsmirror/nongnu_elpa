;;; jabber-sm-tests.el --- Tests for jabber-sm  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-xml)
(require 'jabber-sm)

;;; Counter arithmetic

(ert-deftest jabber-sm-test-inc-counter ()
  "Incrementing a counter adds one."
  (should (= (jabber-sm--inc-counter 0) 1))
  (should (= (jabber-sm--inc-counter 41) 42)))

(ert-deftest jabber-sm-test-inc-counter-wraps ()
  "Counter wraps at 2^32."
  (should (= (jabber-sm--inc-counter (1- (expt 2 32))) 0)))

(ert-deftest jabber-sm-test-counter-delta ()
  "Forward distance between counters."
  (should (= (jabber-sm--counter-delta 5 3) 2))
  (should (= (jabber-sm--counter-delta 0 (1- (expt 2 32))) 1)))

(ert-deftest jabber-sm-test-counter-<= ()
  "Counter comparison with wraparound."
  (should (jabber-sm--counter-<= 3 5))
  (should (jabber-sm--counter-<= 3 3))
  (should-not (jabber-sm--counter-<= 5 3))
  ;; Wraparound: counter near max is "before" counter near 0
  (should (jabber-sm--counter-<= (- (expt 2 32) 2) 1)))

;;; Predicates

(ert-deftest jabber-sm-test-r-p ()
  "Detect SM <r/> element."
  (should (jabber-sm--r-p '(r ((xmlns . "urn:xmpp:sm:3")))))
  (should-not (jabber-sm--r-p '(r ((xmlns . "wrong")))))
  (should-not (jabber-sm--r-p '(a ((xmlns . "urn:xmpp:sm:3"))))))

(ert-deftest jabber-sm-test-a-p ()
  "Detect SM <a/> element."
  (should (jabber-sm--a-p '(a ((xmlns . "urn:xmpp:sm:3") (h . "5")))))
  (should-not (jabber-sm--a-p '(r ((xmlns . "urn:xmpp:sm:3"))))))

(ert-deftest jabber-sm-test-enabled-p ()
  "Detect SM <enabled/> element."
  (should (jabber-sm--enabled-p
           '(enabled ((xmlns . "urn:xmpp:sm:3") (id . "abc") (resume . "true"))))))

(ert-deftest jabber-sm-test-resumed-p ()
  "Detect SM <resumed/> element."
  (should (jabber-sm--resumed-p
           '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "5") (previd . "abc"))))))

(ert-deftest jabber-sm-test-failed-p ()
  "Detect SM <failed/> element."
  (should (jabber-sm--failed-p
           '(failed ((xmlns . "urn:xmpp:sm:3"))))))

;;; State-data reset

(ert-deftest jabber-sm-test-reset ()
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

(ert-deftest jabber-sm-test-features-have-sm ()
  "Detect SM in stream features."
  (let ((sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind")))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (jabber-sm--features-have-sm-p sd))))

(ert-deftest jabber-sm-test-features-no-sm ()
  "No SM in stream features."
  (let ((sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind")))))))
    (should-not (jabber-sm--features-have-sm-p sd))))

(ert-deftest jabber-sm-test-features-nil ()
  "Nil stream features."
  (should-not (jabber-sm--features-have-sm-p '(:stream-features nil))))

;;; Stanza counting

(ert-deftest jabber-sm-test-count-outbound-message ()
  "Outbound message increments counter and queues."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 0 :sm-outbound-queue nil))
         (msg '(message ((to . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-outbound sd msg)))
    (should (= (plist-get result :sm-outbound-count) 1))
    (should (= (length (plist-get result :sm-outbound-queue)) 1))
    (should (equal (cdar (plist-get result :sm-outbound-queue)) msg))))

(ert-deftest jabber-sm-test-count-outbound-iq ()
  "Outbound iq increments counter."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 5 :sm-outbound-queue nil))
         (iq '(iq ((type . "get") (id . "1")) (query ((xmlns . "test")))))
         (result (jabber-sm--count-outbound sd iq)))
    (should (= (plist-get result :sm-outbound-count) 6))))

(ert-deftest jabber-sm-test-count-outbound-disabled ()
  "No counting when SM is disabled."
  (let* ((sd (list :sm-enabled nil :sm-outbound-count 0 :sm-outbound-queue nil))
         (msg '(message ((to . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-outbound sd msg)))
    (should (= (plist-get result :sm-outbound-count) 0))
    (should (null (plist-get result :sm-outbound-queue)))))

(ert-deftest jabber-sm-test-count-outbound-non-stanza ()
  "SM elements are not counted."
  (let* ((sd (list :sm-enabled t :sm-outbound-count 0 :sm-outbound-queue nil))
         (r '(r ((xmlns . "urn:xmpp:sm:3"))))
         (result (jabber-sm--count-outbound sd r)))
    (should (= (plist-get result :sm-outbound-count) 0))))

(ert-deftest jabber-sm-test-count-inbound ()
  "Inbound stanza increments counter."
  (let* ((sd (list :sm-enabled t :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-inbound nil sd msg)))
    (should (= (plist-get result :sm-inbound-count) 1))))

(ert-deftest jabber-sm-test-count-inbound-disabled ()
  "No counting when SM is disabled."
  (let* ((sd (list :sm-enabled nil :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-inbound nil sd msg)))
    (should (= (plist-get result :sm-inbound-count) 0))))

(ert-deftest jabber-sm-test-proactive-ack ()
  "Proactive ack is sent when inbound counter hits the interval."
  (let* ((jabber-sm-ack-interval 3)
         (sd (list :sm-enabled t :sm-inbound-count 2))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (ack-sent nil))
    (cl-letf (((symbol-function 'jabber-sm--send-ack)
               (lambda (_jc _sd) (setq ack-sent t))))
      (jabber-sm--count-inbound 'fake-jc sd msg))
    (should ack-sent)))

(ert-deftest jabber-sm-test-proactive-ack-not-at-interval ()
  "No proactive ack when counter is not at the interval boundary."
  (let* ((jabber-sm-ack-interval 3)
         (sd (list :sm-enabled t :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (ack-sent nil))
    (cl-letf (((symbol-function 'jabber-sm--send-ack)
               (lambda (_jc _sd) (setq ack-sent t))))
      (jabber-sm--count-inbound 'fake-jc sd msg))
    (should-not ack-sent)))

(ert-deftest jabber-sm-test-proactive-ack-disabled ()
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

(ert-deftest jabber-sm-test-prune-queue ()
  "Prune removes entries with count <= h."
  (let* ((queue (list (cons 1 'a) (cons 2 'b) (cons 3 'c) (cons 4 'd)))
         (result (jabber-sm--prune-queue queue 2)))
    (should (= (length result) 2))
    (should (= (caar result) 3))))

(ert-deftest jabber-sm-test-prune-queue-empty ()
  "Prune on empty queue returns empty."
  (should (null (jabber-sm--prune-queue nil 5))))

(ert-deftest jabber-sm-test-process-ack ()
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

;;; FSM routing helper

(ert-deftest jabber-sm-test-maybe-enable-with-sm ()
  "Route to :sm-enable when SM is available."
  (let ((jabber-sm-enable t)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :sm-enable))))

(ert-deftest jabber-sm-test-maybe-enable-without-sm ()
  "Route to :session-established when SM is not in features."
  (let ((jabber-sm-enable t)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :session-established))))

(ert-deftest jabber-sm-test-maybe-enable-disabled ()
  "Route to :session-established when SM is disabled by user."
  (let ((jabber-sm-enable nil)
        (sd (list :stream-features
                  '(features ((xmlns . "http://etherx.jabber.org/streams"))
                             (sm ((xmlns . "urn:xmpp:sm:3")))))))
    (should (eq (car (jabber-sm--maybe-enable-or-establish sd))
                :session-established))))

;;; Enable/resume XML generation

(ert-deftest jabber-sm-test-make-enable-xml ()
  "Enable XML matches expected format."
  (should (string-match-p "enable" (jabber-sm--make-enable-xml)))
  (should (string-match-p "resume='true'" (jabber-sm--make-enable-xml))))

(ert-deftest jabber-sm-test-make-resume-xml ()
  "Resume XML includes h and previd."
  (let ((xml (jabber-sm--make-resume-xml 42 "session-123")))
    (should (string-match-p "h='42'" xml))
    (should (string-match-p "previd='session-123'" xml))))

(ert-deftest jabber-sm-test-parse-enabled ()
  "Parse <enabled/> stanza with resume=true."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "abc-123")
                          (resume . "true")
                          (max . "300"))))))
    (should (equal (plist-get info :id) "abc-123"))
    (should (plist-get info :resume))
    (should (= (plist-get info :max) 300))))

(ert-deftest jabber-sm-test-parse-enabled-resume-1 ()
  "Parse <enabled/> stanza with resume=1."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "xyz")
                          (resume . "1"))))))
    (should (plist-get info :resume))))

(ert-deftest jabber-sm-test-parse-enabled-no-resume ()
  "Parse <enabled/> stanza without resume attribute."
  (let ((info (jabber-sm--parse-enabled
               '(enabled ((xmlns . "urn:xmpp:sm:3")
                          (id . "xyz-456"))))))
    (should (equal (plist-get info :id) "xyz-456"))
    (should-not (plist-get info :resume))
    (should-not (plist-get info :max))))

(ert-deftest jabber-sm-test-apply-enabled-with-resume ()
  "Apply enabled info with resume granted."
  (let* ((sd (list :sm-enabled nil :sm-id nil :sm-resume-max nil))
         (info (list :id "abc" :resume '("true") :max 300))
         (result (jabber-sm--apply-enabled sd info)))
    (should (eq (plist-get result :sm-enabled) t))
    (should (equal (plist-get result :sm-id) "abc"))
    (should (= (plist-get result :sm-resume-max) 300))))

(ert-deftest jabber-sm-test-apply-enabled-no-resume ()
  "Apply enabled info without resume: sm-id stays nil."
  (let* ((sd (list :sm-enabled nil :sm-id nil :sm-resume-max nil))
         (info (list :id "abc" :resume nil :max nil))
         (result (jabber-sm--apply-enabled sd info)))
    (should (eq (plist-get result :sm-enabled) t))
    (should-not (plist-get result :sm-id))))

;;; Resume handling

(ert-deftest jabber-sm-test-handle-resumed ()
  "Handle <resumed/> prunes queue and returns stanzas to resend."
  (let* ((msg-a '(message ((to . "a@x")) (body () "a")))
         (msg-b '(message ((to . "b@x")) (body () "b")))
         (msg-c '(message ((to . "c@x")) (body () "c")))
         (sd (list :sm-enabled t
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

(ert-deftest jabber-sm-test-handle-resumed-all-acked ()
  "All stanzas acked means nothing to resend."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-queue (list (cons 1 'a) (cons 2 'b))
                   :sm-last-acked 0
                   :sm-resumed nil
                   :sm-resuming t))
         (resumed '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "5") (previd . "abc"))))
         (result (jabber-sm--handle-resumed sd resumed)))
    (should (null (cdr result)))))

(ert-deftest jabber-sm-test-handle-resumed-counter-reset ()
  "Outbound counter resets to server h, preventing drift on resend."
  (let* ((msg-a '(message ((to . "a@x")) (body () "a")))
         (msg-b '(message ((to . "b@x")) (body () "b")))
         (msg-c '(message ((to . "c@x")) (body () "c")))
         (sd (list :sm-enabled t
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

;;; Ack XML generation

(ert-deftest jabber-sm-test-make-ack-xml ()
  "Ack XML includes h value."
  (should (string-match-p "h='7'" (jabber-sm--make-ack-xml 7))))

(ert-deftest jabber-sm-test-make-request-xml ()
  "Request XML is well-formed."
  (should (string-match-p "<r xmlns=" (jabber-sm--make-request-xml))))

;;; Queue operations across 2^32 boundary

(ert-deftest jabber-sm-test-prune-queue-wraparound ()
  "Prune works when counters span the 2^32 wraparound."
  (let* ((near-max (- (expt 2 32) 1))
         (queue (list (cons near-max 'a) (cons 0 'b) (cons 1 'c)))
         (result (jabber-sm--prune-queue queue 0)))
    ;; near-max and 0 are both <= 0 (with wraparound), so pruned
    (should (= (length result) 1))
    (should (= (caar result) 1))))

;;; h-count validation

(ert-deftest jabber-sm-test-process-ack-h-too-high ()
  "Warning when server acks more stanzas than sent."
  (let* ((sd (list :sm-enabled t
                   :sm-outbound-count 3
                   :sm-outbound-queue (list (cons 1 'a) (cons 2 'b) (cons 3 'c))
                   :sm-last-acked 0))
         (ack '(a ((xmlns . "urn:xmpp:sm:3") (h . "99"))))
         (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (jabber-sm--process-ack sd ack))
    (should (cl-some (lambda (m) (string-match-p "more stanzas than sent" m))
                     messages))))

;;; Back-pressure

(ert-deftest jabber-sm-test-in-flight-count-normal ()
  "In-flight count is delta between outbound and last-acked."
  (let ((sd (list :sm-outbound-count 7 :sm-last-acked 3)))
    (should (= (jabber-sm--in-flight-count sd) 4))))

(ert-deftest jabber-sm-test-in-flight-count-zero ()
  "In-flight count is zero when fully acked."
  (let ((sd (list :sm-outbound-count 5 :sm-last-acked 5)))
    (should (= (jabber-sm--in-flight-count sd) 0))))

(ert-deftest jabber-sm-test-in-flight-count-wraparound ()
  "In-flight count handles 2^32 wraparound."
  (let ((sd (list :sm-outbound-count 2
                  :sm-last-acked (- (expt 2 32) 3))))
    (should (= (jabber-sm--in-flight-count sd) 5))))

(ert-deftest jabber-sm-test-should-queue-p-at-limit ()
  "Should queue when in-flight equals the cap."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should (jabber-sm--should-queue-p
             sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-sm-test-should-queue-p-below-limit ()
  "Should not queue when in-flight is below the cap."
  (let ((jabber-sm-max-in-flight 10)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-sm-test-should-queue-p-disabled ()
  "Should not queue when back-pressure is disabled."
  (let ((jabber-sm-max-in-flight nil)
        (sd (list :sm-enabled t :sm-outbound-count 100 :sm-last-acked 0)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-sm-test-should-queue-p-sm-off ()
  "Should not queue when SM is not enabled."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled nil :sm-outbound-count 5 :sm-last-acked 0)))
    (should-not (jabber-sm--should-queue-p
                 sd '(message ((to . "a@b")) (body () "hi"))))))

(ert-deftest jabber-sm-test-should-queue-p-non-stanza ()
  "Should not queue non-countable elements."
  (let ((jabber-sm-max-in-flight 3)
        (sd (list :sm-enabled t :sm-outbound-count 5 :sm-last-acked 2)))
    (should-not (jabber-sm--should-queue-p
                 sd '(r ((xmlns . "urn:xmpp:sm:3")))))))

(ert-deftest jabber-sm-test-enqueue-pending ()
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

(ert-deftest jabber-sm-test-drain-pending-partial ()
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
    (setq sd (jabber-sm--drain-pending
              'fake-jc sd
              (lambda (_jc sexp) (push sexp sent))))
    ;; Should have sent exactly 2 (the cap)
    (should (= (length sent) 2))
    ;; One remains in pending queue
    (should (= (length (plist-get sd :sm-pending-queue)) 1))
    (should (equal (cdar (plist-get sd :sm-pending-queue)) msg3))
    ;; Outbound count incremented for sent stanzas
    (should (= (plist-get sd :sm-outbound-count) 2))))

(ert-deftest jabber-sm-test-drain-pending-empty ()
  "Drain with empty queue is a no-op."
  (let* ((jabber-sm-max-in-flight 10)
         (sd (list :sm-enabled t
                   :sm-outbound-count 0
                   :sm-last-acked 0
                   :sm-outbound-queue nil
                   :sm-pending-queue nil))
         (sent nil))
    (setq sd (jabber-sm--drain-pending
              'fake-jc sd
              (lambda (_jc sexp) (push sexp sent))))
    (should (null sent))
    (should (null (plist-get sd :sm-pending-queue)))))

(ert-deftest jabber-sm-test-reset-clears-pending-queue ()
  "Reset clears the pending queue."
  (let* ((sd (list :sm-enabled t :sm-pending-queue '((0 . a) (1 . b) (2 . c))))
         (result (jabber-sm--reset sd)))
    (should (null (plist-get result :sm-pending-queue)))))

;;; Priority queue

(ert-deftest jabber-sm-test-stanza-priority-message ()
  "Messages have priority 0."
  (should (= (jabber-sm--stanza-priority '(message ((to . "a@b")) (body () "hi"))) 0)))

(ert-deftest jabber-sm-test-stanza-priority-iq ()
  "IQs have priority 1."
  (should (= (jabber-sm--stanza-priority '(iq ((type . "get") (id . "1")))) 1)))

(ert-deftest jabber-sm-test-stanza-priority-presence ()
  "Presence has priority 2."
  (should (= (jabber-sm--stanza-priority '(presence ((to . "room@muc/nick")))) 2)))

(ert-deftest jabber-sm-test-drain-pending-priority-order ()
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
    (setq sd (jabber-sm--drain-pending
              'fake-jc sd
              (lambda (_jc sexp) (push sexp sent))))
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

(ert-deftest jabber-sm-test-drain-pending-priority-partial ()
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
    (setq sd (jabber-sm--drain-pending
              'fake-jc sd
              (lambda (_jc sexp) (push sexp sent))))
    (setq sent (nreverse sent))
    ;; Message sent first, then one presence
    (should (= (length sent) 2))
    (should (equal (nth 0 sent) msg1))
    (should (equal (nth 1 sent) pres1))
    ;; One presence remains
    (should (= (length (plist-get sd :sm-pending-queue)) 1))
    (should (equal (cdar (plist-get sd :sm-pending-queue)) pres2))))

(provide 'jabber-sm-tests)

;;; jabber-sm-tests.el ends here
