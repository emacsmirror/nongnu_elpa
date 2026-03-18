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
         (result (jabber-sm--count-inbound sd msg)))
    (should (= (plist-get result :sm-inbound-count) 1))))

(ert-deftest jabber-sm-test-count-inbound-disabled ()
  "No counting when SM is disabled."
  (let* ((sd (list :sm-enabled nil :sm-inbound-count 0))
         (msg '(message ((from . "bob@example.com")) (body () "hi")))
         (result (jabber-sm--count-inbound sd msg)))
    (should (= (plist-get result :sm-inbound-count) 0))))

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

(provide 'jabber-sm-tests)

;;; jabber-sm-tests.el ends here
