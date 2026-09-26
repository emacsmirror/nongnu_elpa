;;; jabber-test-disco-owner.el --- Owned discovery regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise production IQ registration, dispatch, discovery and caps together.
;; Only the network write is replaced; all state is disposable.

;;; Code:

(require 'ert)
(require 'jabber-disco)
(require 'jabber-moderation)
(require 'jabber-disco-test-helpers)
(require 'jabber-sm)

(ert-deftest jabber-test-disco-owner-info-and-items ()
  "Identical destinations retain separate answers and force invalidation."
  (dolist (kind '(info items))
    (jabber-test-disco-owner--with-state
      (let* ((getter (if (eq kind 'info)
                         #'jabber-disco-get-info #'jabber-disco-get-items))
             (cache (if (eq kind 'info)
                        jabber-disco-info-cache jabber-disco-items-cache))
             (payload-a (if (eq kind 'info)
                            '((feature ((var . "A"))))
                          '((item ((name . "A") (jid . "a.example"))))))
             (payload-b (if (eq kind 'info)
                            '((feature ((var . "B"))))
                          '((item ((name . "B") (jid . "b.example"))))))
             delivered)
        (funcall getter a "same.example" "node" nil nil)
        (jabber-test-disco-owner--reply (car wire) payload-a)
        (funcall getter b "same.example" "node"
                 (lambda (jc ctx value) (push (list jc ctx value) delivered)) 'b)
        (should (= (length wire) 2))
        (jabber-test-disco-owner--reply (car wire) payload-b)
        (should (eq (caar delivered) b))
        (let ((answer-b (gethash (jabber-disco--cache-key b "same.example" "node") cache)))
          (should answer-b)
          (should-not (equal answer-b
                             (gethash (jabber-disco--cache-key a "same.example" "node") cache)))
          (funcall getter b "same.example" "node" nil nil)
          (should (= (length wire) 2))
          (funcall getter a "same.example" "node" nil nil t)
          (should (= (length wire) 3))
          (should (eq answer-b
                      (gethash (jabber-disco--cache-key b "same.example" "node") cache))))))))

(ert-deftest jabber-test-disco-owner-callback-retirement ()
  "Ignore foreign and retired replies but accept ordinary FSM state copies."
  (dolist (getter (list #'jabber-disco-get-info #'jabber-disco-get-items))
    (jabber-test-disco-owner--with-state
      (let (delivered)
        (funcall getter a "same.example" "node"
                 (lambda (&rest args) (push args delivered)) nil)
        (let* ((sent (car wire))
               (entry (car jabber-open-info-queries))
               (success (nth 1 entry))
               (xml '(iq ((from . "same.example"))
                         (query ((node . "node")) (feature ((var . "A")))))))
          (funcall (car success) b xml (cdr success))
          (should-not delivered)
          (put a :state-data (copy-sequence (fsm-get-state-data a)))
          (jabber-test-disco-owner--reply sent '((feature ((var . "A")))))
          (should (= (length delivered) 1))
          (setf (plist-get (get a :state-data) :connection) (make-symbol "reconnect"))
          (funcall (car success) a xml (cdr success))
          (should (= (length delivered) 1))
          (should-not (jabber-disco-get-info-immediately "same.example" "node" a))
          (should-not (gethash (jabber-disco--cache-key a "same.example" "node")
                              jabber-disco-items-cache)))))))

(ert-deftest jabber-test-disco-owner-cached-callback-retirement ()
  "A deferred cache hit cannot deliver into a retired connection."
  (jabber-test-disco-owner--with-state
    (jabber-disco-get-info a "same.example" "node" nil nil)
    (jabber-test-disco-owner--reply (car wire) '((feature ((var . "A")))))
    (let (delivered)
      (let ((timer (jabber-disco-get-info
                    a "same.example" "node"
                    (lambda (&rest args) (setq delivered args)) nil)))
        (unwind-protect
            (progn
              (setq jabber-connections (delq a jabber-connections))
              (apply (timer--function timer) (timer--args timer))
              (should-not delivered))
          (cancel-timer timer))))))

(ert-deftest jabber-test-disco-owner-predicate-bypasses-cache ()
  "Predicate-bearing queries require admitted wire replies even on cache hit."
  (jabber-test-disco-owner--with-state
    (jabber-disco-get-info a "same.example" "node" nil nil)
    (jabber-test-disco-owner--reply (car wire) '((feature ((var . "cached")))))
    (let (admit delivered)
      (jabber-disco-get-info
       a "same.example" "node" (lambda (&rest args) (setq delivered args)) nil
       nil (lambda (_jc _xml) admit))
      (should (= (length wire) 2))
      (jabber-test-disco-owner--reply (car wire) '((feature ((var . "fresh")))))
      (should-not delivered)
      (should (= (length jabber-open-info-queries) 1))
      (setq admit t)
      (jabber-test-disco-owner--reply (car wire) '((feature ((var . "fresh")))))
      (should (equal (cadr (nth 2 delivered)) '("fresh")))
      (should-not jabber-open-info-queries))))

(ert-deftest jabber-test-disco-owner-moderation-reads ()
  "The native moderation predicate uses the caller's account, not ambient state."
  (jabber-test-disco-owner--with-state
    (jabber-disco-get-info a "room.example" nil nil nil)
    (jabber-test-disco-owner--reply
     (car wire) `((feature ((var . ,jabber-moderation-occupant-id-xmlns)))))
    (jabber-disco-get-info b "room.example" nil nil nil)
    (jabber-test-disco-owner--reply (car wire) '((feature ((var . "other")))))
    (should (jabber-moderation--room-supports-occupant-id-p "room.example" a))
    (should-not (jabber-moderation--room-supports-occupant-id-p "room.example" b))
    (should-not (jabber-disco-get-info-immediately "room.example" nil))
    (let (queried)
      (cl-letf (((symbol-function 'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (room jc) (setq queried (list room jc)) nil)))
        (jabber-moderation--handle-author-retraction
         b '(message ((from . "room.example/nick"))) '(retract ((id . "x"))))
        (should (equal queried (list "room.example" b)))))))

(ert-deftest jabber-test-disco-owner-caps-fallback-owned-node ()
  "Identical full JIDs on different accounts retain distinct owned candidates."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "unknown")))
      (jabber-caps--query-if-needed a "same.example/res" "sha-1" "node-a" "unknown" key nil)
      (jabber-caps--query-if-needed b "same.example/res" "sha-1" "node-b" "unknown"
                                    key (gethash key jabber-caps-cache))
      (jabber-caps--query-if-needed b "same.example/res" "sha-1" "node-b" "unknown"
                                    key (gethash key jabber-caps-cache))
      (should (= (length wire) 1))
      (jabber-test-disco-owner--reply (car wire) nil t)
      (should (= (length wire) 2))
      (should (eq (caar wire) b))
      (should (equal (jabber-xml-get-attribute (jabber-iq-query (cdar wire)) 'node)
                     "node-b#unknown"))
      (jabber-test-disco-owner--reply (car wire) nil t)
      (should-not (gethash key jabber-caps--pending))
      (should-not (gethash key jabber-caps-cache)))))

(ert-deftest jabber-test-disco-owner-caps-timeout-and-stale-result ()
  "Timeout advances candidates; stale errors, results and timers cannot settle successors."
  (jabber-test-disco-owner--with-state
    (let* ((children '((feature ((var . "verified")))))
           (ver (jabber-caps-ver-string `(query nil ,@children) "sha-1"))
           (key (cons "sha-1" ver)))
      (jabber-caps--query-if-needed a "same.example/r" "sha-1" "a" ver key nil)
      (let* ((first (car wire))
             (first-iq (car jabber-open-info-queries))
             (pending (gethash key jabber-caps--pending))
             (timer (plist-get pending :timer)))
        (jabber-caps--query-if-needed b "same.example/r" "sha-1" "b" ver key nil)
        (jabber-test-disco-owner--expire pending)
        (should (eq (caar wire) b))
        (apply (timer--function timer) (timer--args timer))
        (dolist (callback (list (nth 1 first-iq) (nth 2 first-iq)))
          (funcall (car callback) a `(iq nil (query nil ,@children)) (cdr callback)))
        (should-not (memq first-iq jabber-open-info-queries))
        (should (= (length jabber-open-info-queries) 1))
        (should (= (length wire) 2))
        (jabber-test-disco-owner--reply first children)
        (should-not (gethash key jabber-caps-cache))
        (should (eq pending (gethash key jabber-caps--pending)))
        (jabber-test-disco-owner--reply (car wire) children)
        (should (equal (cadr (gethash key jabber-caps-cache)) '("verified")))
        (should-not (gethash key jabber-caps--pending))
        (jabber-caps--query-if-needed c "other.example/r" "sha-1" "c" ver key
                                      (gethash key jabber-caps-cache))
        (should (= (length wire) 2))
        (should (eq (gethash key jabber-caps-cache)
                    (jabber-disco-get-info-immediately "other.example/r" nil c)))))))

(ert-deftest jabber-test-disco-owner-caps-retired-and-failed-candidates ()
  "Discard retired queued owners, reject bad hashes and settle a silent last peer."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "unknown")))
      (jabber-caps--query-if-needed a "a.example/r" "sha-1" "a" "unknown" key nil)
      (jabber-caps--query-if-needed c "c.example/r" "sha-1" "c" "unknown" key nil)
      (jabber-caps--query-if-needed b "b.example/r" "sha-1" "b" "unknown" key nil)
      (setf (plist-get (get b :state-data) :connection) (make-symbol "reconnected"))
      (jabber-test-disco-owner--reply (car wire) '((feature ((var . "wrong-hash")))))
      (should (= (length wire) 2))
      (should (eq (caar wire) c))
      (jabber-test-disco-owner--expire (gethash key jabber-caps--pending))
      (should-not jabber-open-info-queries)
      (should-not (gethash key jabber-caps--pending))
      (should-not (gethash key jabber-caps-cache))
      (jabber-caps--query-if-needed a "a.example/r" "sha-1" "a" "unknown" key nil)
      (should (= (length wire) 3)))))

(ert-deftest jabber-test-disco-owner-caps-active-retirement ()
  "A retired active owner cannot install even hash-verified content."
  (jabber-test-disco-owner--with-state
    (let* ((children '((feature ((var . "verified")))))
           (ver (jabber-caps-ver-string `(query nil ,@children) "sha-1"))
           (key (cons "sha-1" ver)))
      (jabber-caps--query-if-needed a "same.example/r" "sha-1" "a" ver key nil)
      (jabber-caps--query-if-needed b "same.example/r" "sha-1" "b" ver key nil)
      (setq jabber-connections (delq a jabber-connections))
      (jabber-test-disco-owner--reply (car wire) children)
      (should-not (gethash key jabber-caps-cache))
      (should (eq (caar wire) b))
      (jabber-test-disco-owner--reply (car wire) children)
      (should (gethash key jabber-caps-cache)))))

(ert-deftest jabber-test-disco-owner-caps-contact-and-aliases ()
  "Scope resource observations and aliases without duplicating verified content."
  (jabber-test-disco-owner--with-state
    (let* ((peer "same.example/r")
           (children '((feature ((var . "verified")))))
           (ver (jabber-caps-ver-string `(query nil ,@children) "sha-1"))
           (key (cons "sha-1" ver)))
      (jabber-process-caps-modern a peer "sha-1" "node-a" ver)
      (jabber-process-caps-modern b peer "sha-1" "node-b" ver)
      (should (= (length wire) 1))
      (jabber-test-disco-owner--reply (car wire) nil t)
      (should (eq (caar wire) b))
      (should (equal (jabber-xml-get-attribute (jabber-iq-query (cdar wire)) 'node)
                     (concat "node-b#" ver)))
      (jabber-test-disco-owner--reply (car wire) children)
      (let ((info (gethash key jabber-caps-cache)))
        (should info)
        (should (eq (jabber-caps-get-cached peer a) info))
        (should (eq (jabber-caps-get-cached peer b) info))
        (should-not (jabber-caps-get-cached peer c))
        (should-not (jabber-caps-get-cached peer))
        (jabber-process-caps-modern a peer "sha-1" "node-a" ver)
        (jabber-process-caps-modern b peer "sha-1" "node-b" ver)
        (should (= (length wire) 2))
        (should (eq (gethash (jabber-disco--cache-key a peer nil) jabber-disco-info-cache) info))
        (should (eq (gethash (jabber-disco--cache-key b peer nil) jabber-disco-info-cache) info))
        (jabber-process-caps-modern a peer "sha-1" "changed" "unknown")
        (should-not (jabber-disco-get-info-immediately peer nil a))
        (should (eq (jabber-disco-get-info-immediately peer nil b) info))
        (setf (plist-get (get b :state-data) :session-id) "new-stream")
        (should-not (jabber-caps-get-cached peer b))
        (should-not (jabber-disco-get-info-immediately peer nil b))))))

(ert-deftest jabber-test-disco-owner-persisted-caps ()
  "Reuse persisted verified payloads, creating aliases only for the observer."
  (jabber-test-disco-owner--with-state
    (let ((jabber-db-path (make-temp-file "jabber-disco-db-"))
          (jabber-db--connection nil)
          (info '((["client" "client" "pc"]) ("feature"))))
      (unwind-protect
          (progn
            (jabber-db-caps-store "sha-1" "stored" (car info) (cadr info))
            (jabber-process-caps-modern a "same.example/r" "sha-1" "node" "stored")
            (should-not wire)
            (should (equal (jabber-disco-get-info-immediately "same.example/r" nil a) info))
            (should-not (jabber-disco-get-info-immediately "same.example/r" nil b))
            (jabber-process-caps-modern b "same.example/r" "sha-1" "other" "stored")
            (should-not wire)
            (should (eq (jabber-caps-get-cached "same.example/r" a)
                        (jabber-caps-get-cached "same.example/r" b))))
        (jabber-db-close)
        (delete-file jabber-db-path)))))

(ert-deftest jabber-test-disco-owner-caps-send-failure ()
  "Settle failed transport admission and allow a later attempt."
  (jabber-test-disco-owner--with-state
    (let ((key '("sha-1" . "unknown")))
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (&rest _) (error "Transport closed"))))
        (jabber-caps--query-if-needed a "same.example/r" "sha-1" "a" "unknown" key nil))
      (should-not (gethash key jabber-caps--pending))
      (should-not jabber-open-info-queries)
      (jabber-caps--query-if-needed b "same.example/r" "sha-1" "b" "unknown" key nil)
      (should (eq (caar wire) b)))))

(ert-deftest jabber-test-disco-owner-retired-observations-bounded ()
  "Reclaim retired observations without clearing live or verified content."
  (jabber-test-disco-owner--with-state
    (let* ((children '((feature ((var . "verified")))))
           (ver (jabber-caps-ver-string `(query nil ,@children) "sha-1"))
           (key (cons "sha-1" ver)))
      ;; Populate verified global data through the native IQ/hash check.
      (jabber-process-caps-modern b "peer.example/r" "sha-1" "node" ver)
      (jabber-test-disco-owner--reply (car wire) children)
      (jabber-process-caps-modern b "peer.example/r" "sha-1" "node" ver)
      (jabber-disco-get-items b "service.example" nil nil nil)
      (jabber-test-disco-owner--reply
       (car wire) '((item ((jid . "live.example")))))
      (let ((verified (gethash key jabber-caps-cache))
            (live-info (gethash (jabber-disco--cache-key b "peer.example/r" nil)
                                jabber-disco-info-cache))
            (live-items (gethash (jabber-disco--cache-key b "service.example" nil)
                                 jabber-disco-items-cache)))
        (dotimes (generation 12)
          ;; Replace transports, streams and entire connections in turn.
          (pcase (% generation 3)
            (0 (setf (plist-get (get a :state-data) :connection)
                     (make-symbol "replacement")))
            (1 (setf (plist-get (get a :state-data) :session-id)
                     (format "stream-%s" generation)))
            (2 (setq jabber-connections (delq a jabber-connections)
                     a (jabber-test-disco-owner--connection "a"))
               (push a jabber-connections)))
          ;; Exercise each insertion boundary as the first cleanup after loss.
          (pcase (% generation 3)
            (0 (jabber-disco-get-info a "service.example" nil nil nil)
               (jabber-test-disco-owner--reply (car wire) children))
            (1 (jabber-disco-get-items a "service.example" nil nil nil)
               (jabber-test-disco-owner--reply
                (car wire) '((item ((jid . "current.example"))))))
            (2 (jabber-process-caps-modern a "peer.example/r" "sha-1" "node" ver)))
          (dolist (cache (list jabber-disco-info-cache jabber-disco-items-cache))
            (maphash (lambda (observation _)
                       (should (jabber-disco--owner-current-p (car observation))))
                     cache))
          (jabber-disco-get-info a "service.example" nil nil nil t)
          (jabber-test-disco-owner--reply (car wire) children)
          (jabber-disco-get-items a "service.example" nil nil nil t)
          (jabber-test-disco-owner--reply
           (car wire) '((item ((jid . "current.example")))))
          (jabber-process-caps-modern a "peer.example/r" "sha-1" "node" ver)
          (should (= (hash-table-count jabber-disco-info-cache) 3))
          (should (= (hash-table-count jabber-disco-items-cache) 2))
          (dolist (cache (list jabber-disco-info-cache jabber-disco-items-cache))
            (maphash (lambda (observation _)
                       (should (jabber-disco--owner-current-p (car observation))))
                     cache))
          (should (eq verified (gethash key jabber-caps-cache)))
          (should (eq live-info (jabber-disco-get-info-immediately "peer.example/r" nil b)))
          (should (eq live-items
                      (gethash (jabber-disco--cache-key b "service.example" nil)
                               jabber-disco-items-cache))))
        ;; A successful native SM resume preserves the other live owner's
        ;; observations and contact namespace across ordinary state copies.
        (let ((contact (jabber-jid-symbol "peer.example" b))
              (state (append (list :sm-enabled t :sm-id "resumable"
                                   :sm-resuming t :sm-outbound-count 0
                                   :sm-last-acked 0)
                             (copy-sequence (fsm-get-state-data b)))))
          (put b :state-data
               (jabber-sm--handle-resumed
                state '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "0")
                                 (previd . "resumable")))))
          (should (plist-get (fsm-get-state-data b) :sm-resumed))
          (jabber-disco-get-info a "after-resume.example" nil nil nil)
          (jabber-test-disco-owner--reply (car wire) children)
          (should (eq contact (jabber-jid-symbol "peer.example" b)))
          (should (eq live-info (jabber-disco-get-info-immediately "peer.example/r" nil b)))
          (should (eq verified (jabber-caps-get-cached "peer.example/r" b))))))))

(ert-deftest jabber-test-disco-owner-resume-reclaims-old-transport ()
  "Retire transport observations on resume, retaining verified caps and contacts."
  (jabber-test-disco-owner--with-state
    (let* ((children '((feature ((var . "verified")))))
           (ver (jabber-caps-ver-string `(query nil ,@children) "sha-1"))
           (key (cons "sha-1" ver)))
      (jabber-process-caps-modern a "peer.example/r" "sha-1" "node" ver)
      (jabber-test-disco-owner--reply (car wire) children)
      (jabber-process-caps-modern a "peer.example/r" "sha-1" "node" ver)
      (let* ((contact (jabber-jid-symbol "peer.example" a))
             (verified (gethash key jabber-caps-cache))
             (old-key (jabber-disco--cache-key a "peer.example/r" nil))
             (state (copy-sequence (fsm-get-state-data a))))
        (setf (plist-get state :connection) (make-symbol "resumed-transport")
              (plist-get state :session-id) "resumed-stream")
        (put a :state-data
             (jabber-sm--handle-resumed
              (append (list :sm-id "resumable" :sm-enabled t :sm-resuming t
                            :sm-outbound-count 0 :sm-last-acked 0) state)
              '(resumed ((xmlns . "urn:xmpp:sm:3") (h . "0")
                         (previd . "resumable")))))
        (should (plist-get (fsm-get-state-data a) :sm-resumed))
        (should (eq contact (jabber-jid-symbol "peer.example" a)))
        (should-not (jabber-caps-get-cached "peer.example/r" a))
        (jabber-process-caps-modern a "peer.example/r" "sha-1" "node" ver)
        (should-not (gethash old-key jabber-disco-info-cache))
        (should (= (hash-table-count jabber-disco-info-cache) 1))
        (should (= (length wire) 1))
        (should (eq verified (gethash key jabber-caps-cache)))
        (should (eq verified (jabber-disco-get-info-immediately "peer.example/r" nil a)))))))

(provide 'jabber-test-disco-owner)
;;; jabber-test-disco-owner.el ends here
