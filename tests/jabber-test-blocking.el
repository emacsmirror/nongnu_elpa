;;; jabber-test-blocking.el --- Tests for XEP-0191 blocking  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-blocking)

(ert-deftest jabber-test-blocking-apply-block-push ()
  "A block push adds its JIDs without duplicates."
  (let ((query `(block ((xmlns . ,jabber-blocking-xmlns))
                       (item ((jid . "a@example.org")))
                       (item ((jid . "b@example.org"))))))
    (should (equal (jabber-blocking--apply-push
                    '("a@example.org" "old@example.org") query)
                   '("a@example.org" "old@example.org" "b@example.org")))))

(ert-deftest jabber-test-blocking-apply-unblock-push ()
  "An unblock push removes listed JIDs or clears the whole list."
  (let ((current '("a@example.org" "b@example.org")))
    (should
     (equal
      (jabber-blocking--apply-push
       current
       `(unblock ((xmlns . ,jabber-blocking-xmlns))
                 (item ((jid . "a@example.org")))))
      '("b@example.org")))
    (should-not
     (jabber-blocking--apply-push
      current `(unblock ((xmlns . ,jabber-blocking-xmlns)))))))

(ert-deftest jabber-test-blocking-processes-valid-push ()
  "A valid server push updates state and receives an IQ result."
  (let* ((state-data (list :username "me"
                           :server "example.org"
                           :resource "emacs"
                           :blocking-list '("old@example.org")))
         (xml `(iq ((type . "set") (id . "push-1"))
                   (block ((xmlns . ,jabber-blocking-xmlns))
                          (item ((jid . "new@example.org"))))))
         sent)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) state-data))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent args))))
      (jabber-blocking--process-push 'fake-jc xml))
    (should (equal (plist-get state-data :blocking-list)
                   '("old@example.org" "new@example.org")))
    (should (equal (nth 2 sent) "result"))
    (should (equal (car (last sent)) "push-1"))))

(ert-deftest jabber-test-blocking-rejects-foreign-push ()
  "A foreign entity cannot change the local blocklist state."
  (let* ((state-data (list :username "me"
                           :server "example.org"
                           :resource "emacs"
                           :blocking-list '("old@example.org")))
         (xml `(iq ((type . "set") (id . "push-1")
                    (from . "attacker@example.net"))
                   (block ((xmlns . ,jabber-blocking-xmlns))
                          (item ((jid . "new@example.org"))))))
         sent)
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (_jc) state-data))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq sent args))))
      (jabber-blocking--process-push 'fake-jc xml))
    (should (equal (plist-get state-data :blocking-list)
                   '("old@example.org")))
    (should-not sent)))

(ert-deftest jabber-test-blocking-jid-matching ()
  "Match XEP-0191 scopes without broadening resources or domains."
  (let ((state (list :username "me" :server "example.org")))
    (cl-letf (((symbol-function 'fsm-get-state-data)
               (lambda (jc) (and (eq jc 'account) state))))
      (dolist (case '(("peer@example.net" "peer@example.net/Phone" t)
                      ("peer@example.net/Phone" "peer@example.net" nil)
                      ("peer@example.net/Phone" "peer@example.net/phone" nil)
                      ("example.net/Phone" "peer@example.net/Phone" t)
                      ("example.net/Phone" "peer@example.net/Other" nil)
                      ("example.net" "peer@example.net/Phone" t)
                      ("example.net" "peer@sub.example.net" nil)
                      ("example.net" "peer@notexample.net" nil)
                      ("example.org" "me@example.org/Other" nil)
                      ("PEER@EXAMPLE.NET" "peer@example.net" t)
                      ("peer@example.net" "room@conference.example.net/peer" nil)))
        (plist-put state :blocking-list (list (nth 0 case)))
        (should (eq (not (null (jabber-blocking-blocked-p 'account (nth 1 case))))
                    (nth 2 case))))
      (should-not (jabber-blocking-blocked-p 'other "peer@example.net")))))

(ert-deftest jabber-test-blocking-malformed-unblock-and-authority ()
  "Malformed unblocks must not clear state; resources cannot push it."
  (should (equal (jabber-blocking--apply-push
                  '("peer@example.net") '(unblock nil (item nil)))
                 '("peer@example.net")))
  (let ((state '(:username "me" :server "example.org" :resource "emacs")))
    (should (jabber-blocking--valid-push-p nil state))
    (should (jabber-blocking--valid-push-p "me@example.org" state))
    (should-not (jabber-blocking--valid-push-p "me@example.org/emacs" state))
    (should-not (jabber-blocking--valid-push-p "example.org" state))))

(ert-deftest jabber-test-blocking-startup-snapshot-and-push-race ()
  "Startup subscribes to pushes and never overwrites one with an old snapshot."
  (let ((state (list :username "me" :server "example.org"))
        disco requests
        (jabber-blocking-ready-hook nil))
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (_jc _jid _node callback _ctx &optional _force _predicate)
                 (setq disco callback)))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (push args requests))))
      (jabber-blocking--on-connect 'account)
      (should-not (jabber-blocking-ready-p 'account "peer@example.net"))
      (should (jabber-blocking-ready-p 'account "me@example.org/Other"))
      (funcall disco 'account nil '(nil ("urn:xmpp:blocking")))
      (let ((snapshot (nth 4 (car requests))))
        (should-not (nth 1 (car requests)))
        (jabber-blocking--process-push
         'account '(iq ((type . "set") (id . "p"))
                       (block ((xmlns . "urn:xmpp:blocking"))
                              (item ((jid . "peer@example.net"))))))
        (funcall snapshot 'account
                 '(iq nil (blocklist ((xmlns . "urn:xmpp:blocking")))) nil)
        (should (jabber-blocking-blocked-p 'account "peer@example.net"))
        (should-not (jabber-blocking-ready-p 'account "peer@example.net"))
        (funcall (nth 4 (car requests)) 'account
                 '(iq nil (blocklist ((xmlns . "urn:xmpp:blocking"))
                                     (item ((jid . "peer@example.net"))))) nil)
        (should (jabber-blocking-ready-p 'account "peer@example.net"))
        (should (jabber-blocking-blocked-p 'account "peer@example.net"))))))

(ert-deftest jabber-test-blocking-discovery-unsupported-or-failed ()
  "Unsupported servers permit work; discovery failures do not mean empty."
  (dolist (info '((nil nil) (error nil)))
    (let ((state (list :username "me" :server "example.org"))
          (jabber-blocking-ready-hook nil))
      (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
                ((symbol-function 'jabber-disco-get-info)
                 (lambda (jc _jid _node callback _ctx &optional _force _predicate)
                   (funcall callback jc nil info)))
                ((symbol-function 'jabber-send-iq)
                 (lambda (&rest _) (ert-fail "Unexpected blocklist request"))))
        (jabber-blocking--on-connect 'account)
        (should (eq (not (null (jabber-blocking-ready-p 'account "peer@example.net")))
                    (not (eq (car info) 'error))))))))

(ert-deftest jabber-test-blocking-old-session-snapshot ()
  "A snapshot from an old connection generation cannot replace current state."
  (let ((state (list :username "me" :server "example.org")) response)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq response (nth 4 args)))))
      (jabber-blocking--fetch 'account)
      (plist-put state :blocking-session (list nil))
      (funcall response 'account
               '(iq nil (blocklist ((xmlns . "urn:xmpp:blocking"))
                                   (item ((jid . "peer@example.net"))))) nil)
      (should-not (plist-member state :blocking-list)))))

(require 'jabber-omemo)
(require 'jabber-muc)

(ert-deftest jabber-test-blocking-omemo-no-requests-or-events ()
  "Known blocks settle fetches without requests, cache changes or warnings."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-list '("peer@example.net")))
        (jabber-omemo--device-lists (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-pubsub-request)
               (lambda (&rest _) (ert-fail "Blocked peer was queried")))
              ((symbol-function 'warn)
               (lambda (&rest _) (ert-fail "Blocked peer produced warning")))
              ((symbol-function 'jabber-omemo-store-save-device)
               (lambda (&rest _) (ert-fail "Blocked event was persisted"))))
      (let ((done (lambda (result) (should-not result) (cl-incf calls))))
        (jabber-omemo--fetch-device-list 'account "peer@example.net" done)
        (jabber-omemo--fetch-bundle 'account "peer@example.net" 7 done)
        (jabber-omemo--ensure-sessions 'account "peer@example.net" done))
      (jabber-omemo--handle-device-list 'account "peer@example.net" nil nil)
      (should (= calls 3))
      (should (= (hash-table-count jabber-omemo--device-lists) 0)))))

(ert-deftest jabber-test-blocking-omemo-inflight-block-and-errors ()
  "Late replies settle after a block; ordinary errors remain visible."
  (dolist (blocked '(nil t))
    (let ((state (list :username "me" :server "example.org"))
          success failure warned result)
      (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
                ((symbol-function 'jabber-pubsub-request)
                 (lambda (_jc _jid _node ok err)
                   (setq success ok failure err)))
                ((symbol-function 'warn) (lambda (&rest _) (setq warned t)))
                ((symbol-function 'jabber-omemo--parse-bundle-xml)
                 (lambda (_) (ert-fail "Blocked reply parsed"))))
        (jabber-omemo--fetch-bundle
         'account "peer@example.net" 7 (lambda (value) (setq result (list value))))
        (when blocked
          (jabber-blocking--set-list state '("peer@example.net"))
          (funcall success 'account
                   '(iq nil (pubsub nil (items nil (item nil (bundle nil))))) nil)
          (should (equal result '(nil))))
        (funcall failure 'account
                 '(iq nil (error ((type . "cancel"))
                                 (service-unavailable ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))) nil)
        (should (equal result '(nil)))
        (should (eq warned (not blocked)))))))

(ert-deftest jabber-test-blocking-omemo-muc-no-partial-success ()
  "A blocked real recipient fails aggregation rather than silently omitting them."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-list '("peer@example.net")))
        result)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-omemo--ensure-sessions)
               (lambda (_jc jid callback)
                 (funcall callback (unless (equal jid "peer@example.net")
                                     '((8 . session)))))))
      (jabber-omemo--ensure-sessions-multi
       'account '("peer@example.net" "other@example.net")
       (lambda (value) (setq result (list value))))
      (should (equal result '(nil))))))

(ert-deftest jabber-test-blocking-delayed-command-result ()
  "A delayed block result refreshes state rather than replaying old intent."
  (let ((state (list :username "me" :server "example.org"))
        requests (jabber-blocking-ready-hook nil))
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (push args requests))))
      (jabber-blocking-block-jid 'account "peer@example.net")
      (let ((ack (nth 4 (car requests))))
        (jabber-blocking--set-list state nil)
        (funcall ack 'account '(iq ((type . "result"))) nil)
        (should (equal (nth 2 (car requests)) "get"))
        (should-not (jabber-blocking-blocked-p 'account "peer@example.net"))
        (funcall (nth 4 (car requests)) 'account
                 '(iq nil (blocklist ((xmlns . "urn:xmpp:blocking")))) nil)
        (should-not (jabber-blocking-blocked-p 'account "peer@example.net"))
        (setq requests nil)
        (plist-put state :blocking-session (list nil))
        (funcall ack 'account '(iq ((type . "result"))) nil)
        (should-not requests)))))

(ert-deftest jabber-test-blocking-open-chat-waits-for-snapshot ()
  "Open-chat prefetch waits for discovery, then resumes only unblocked peers."
  (let ((state (list :username "me" :server "example.org" :blocking-status 'pending))
        (jabber-omemo--device-lists (make-hash-table :test #'equal))
        (jabber-blocking-ready-hook '(jabber-omemo--prefetch-open-chats))
        requested)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-omemo--load-device-list-from-db)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-omemo--fetch-device-list)
               (lambda (_jc jid callback)
                 (push jid requested) (funcall callback nil))))
      (with-temp-buffer
        (setq-local major-mode 'jabber-chat-mode
                    jabber-buffer-connection 'account
                    jabber-chat-encryption 'omemo
                    jabber-chatting-with "peer@example.net")
        (jabber-omemo--prefetch-open-chats 'account)
        (should-not requested)
        (jabber-blocking--ready 'account 'ready)
        (should (equal requested '("peer@example.net")))
        (setq requested nil)
        (jabber-blocking--set-list state '("peer@example.net"))
        (jabber-blocking--ready 'account 'ready)
        (should-not requested)))))

(ert-deftest jabber-test-blocking-omemo-own-work-proceeds ()
  "Discovery and a domain block do not suppress own-device requests."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-status 'pending :blocking-list '("example.org")))
        targets)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-pubsub-request)
               (lambda (_jc jid &rest _) (push jid targets))))
      (jabber-omemo--fetch-bundle 'account "me@example.org" 7 #'ignore)
      (jabber-omemo--fetch-device-list 'account "me@example.org" #'ignore)
      (should (equal targets '("me@example.org" "me@example.org"))))))

(ert-deftest jabber-test-blocking-inflight-devicelist-no-persistence ()
  "A block arriving before a device-list response prevents cache and DB writes."
  (let ((state (list :username "me" :server "example.org"))
        (jabber-omemo--device-lists (make-hash-table :test #'equal))
        response result)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-pubsub-request)
               (lambda (_jc _jid _node success _failure) (setq response success)))
              ((symbol-function 'jabber-omemo-store-save-device)
               (lambda (&rest _) (ert-fail "Blocked device persisted"))))
      (jabber-omemo--fetch-device-list
       'account "peer@example.net" (lambda (value) (setq result (list value))))
      (jabber-blocking--set-list state '("peer@example.net"))
      (funcall response 'account
               `(iq nil (pubsub nil (items nil (item nil
                         (list ((xmlns . ,jabber-omemo-xmlns))
                               (device ((id . "7")))))))) nil)
      (should (equal result '(nil)))
      (should (= (hash-table-count jabber-omemo--device-lists) 0)))))

(ert-deftest jabber-test-blocking-send-refuses-blocked-recipients ()
  "Direct and room send paths refuse before encryption, not just fetch."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-list '("peer@example.net")))
        (jabber-muc-participants
         '(("room@conference.example.org"
            ("nick" jid "peer@example.net/Phone")))))
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-omemo-encrypt-message)
               (lambda (&rest _) (ert-fail "Blocked send reached encryption"))))
      (should-error
       (jabber-omemo--send-encrypted 'account "text" "peer@example.net" nil)
       :type 'user-error)
      (should-error
       (jabber-omemo--send-encrypted-muc 'account "text" "room@conference.example.org" nil)
       :type 'user-error))))

(ert-deftest jabber-test-blocking-invalid-snapshot-retains-known-state ()
  "Malformed snapshots and failed refreshes never manufacture an empty list."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-status 'ready :blocking-list '("peer@example.net")))
        request (jabber-blocking-ready-hook nil))
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-send-iq)
               (lambda (&rest args) (setq request args))))
      (jabber-blocking--fetch 'account)
      (funcall (nth 4 request) 'account
               '(iq nil (blocklist ((xmlns . "urn:xmpp:blocking")) (item nil))) nil)
      (should (jabber-blocking-blocked-p 'account "peer@example.net"))
      (funcall (nth 6 request) 'account '(iq nil (error ((type . "cancel"))
                                  (service-unavailable ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))) nil)
      (should (jabber-blocking-blocked-p 'account "peer@example.net"))
      (should (jabber-blocking-ready-p 'account "other@example.net")))))

(ert-deftest jabber-test-blocking-wire-response-admission ()
  "Reject foreign IQ results/errors without consuming the legitimate request."
  (dolist (kind '(disco snapshot block unblock))
    (dolist (type '("result" "error"))
      (let* ((state (list :username "me" :server "example.org"
                          :blocking-status 'pending
                          :blocking-list '("peer@example.net")))
             (other (list :username "other" :server "example.org"))
             (jabber-open-info-queries nil)
             (jabber-disco-info-cache (make-hash-table :test #'equal))
             (ready 0)
             (jabber-blocking-ready-hook (list (lambda (_) (cl-incf ready))))
             sent messages)
        ;; A cached unsupported answer must not bypass wire admission.
        (puthash '("example.org" . nil) '(nil nil) jabber-disco-info-cache)
        (cl-letf (((symbol-function 'fsm-get-state-data)
                   (lambda (jc) (if (eq jc 'account) state other)))
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc xml &rest _) (push xml sent)))
                  ((symbol-function 'message)
                   (lambda (&rest args) (push args messages))))
          (pcase kind
            ('disco (jabber-blocking--on-connect 'account))
            ('snapshot (jabber-blocking--fetch 'account))
            (_ (jabber-blocking--change 'account "peer@example.net" kind)))
          (should (= (length sent) 1))
          (let* ((id (jabber-xml-get-attribute (car sent) 'id))
                 (pending (assoc id jabber-open-info-queries))
                 (payload
                  (if (equal type "error")
                      '(error ((type . "cancel"))
                              (service-unavailable
                               ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))
                    (pcase kind
                      ('disco '(query ((xmlns . "http://jabber.org/protocol/disco#info"))))
                      ('snapshot '(blocklist ((xmlns . "urn:xmpp:blocking")))))))
                 (reply (lambda (from)
                          `(iq ((type . ,type) (id . ,id)
                                ,@(when from `((from . ,from)))) ,payload))))
            (should pending)
            (dolist (attack '((account "attacker@example.net")
                              (other nil) (other "example.org")
                              (account "me@example.org/Other")))
              (jabber-process-iq (car attack) (funcall reply (cadr attack)))
              (should (eq pending (assoc id jabber-open-info-queries)))
              (should (equal (plist-get state :blocking-list) '("peer@example.net")))
              (should (eq (plist-get state :blocking-status) 'pending))
              (should (= ready 0))
              (should (= (length sent) 1))
              (should-not messages))
            ;; The real response still settles the original pending request.
            (jabber-process-iq 'account (funcall reply "example.org"))
            (should-not (assoc id jabber-open-info-queries))
            (cond
             ((equal type "error")
              (should (equal (plist-get state :blocking-list) '("peer@example.net")))
              (should (= ready 0))
              (should (= (length sent) 1))
              (if (eq kind 'disco)
                  (should (eq (plist-get state :blocking-status) 'failed))
                (should messages)))
             ((memq kind '(disco snapshot))
              (should (= ready 1))
              (should-not (plist-get state :blocking-list))
              (should (eq (plist-get state :blocking-status)
                          (if (eq kind 'disco) 'unsupported 'ready))))
             (t
              (should (= (length sent) 2))
              (should (eq (car (jabber-iq-query (car sent))) 'blocklist))
              (should messages)))))))))

(ert-deftest jabber-test-blocking-wire-malformed-then-valid-snapshot ()
  "Ignore malformed snapshots while retaining the request for a valid reply."
  (let ((state (list :username "me" :server "example.org"
                     :blocking-list '("peer@example.net")))
        (jabber-open-info-queries nil)
        (jabber-blocking-ready-hook nil)
        sent)
    (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc xml &rest _) (setq sent xml))))
      (jabber-blocking--fetch 'account)
      (let* ((id (jabber-xml-get-attribute sent 'id))
             (pending (assoc id jabber-open-info-queries)))
        (jabber-process-iq
         'account `(iq ((type . "result") (id . ,id))
                       (blocklist ((xmlns . "urn:xmpp:blocking")) (item nil))))
        (should (eq pending (assoc id jabber-open-info-queries)))
        (should (equal (plist-get state :blocking-list) '("peer@example.net")))
        (jabber-process-iq
         'account `(iq ((type . "result") (from . "me@example.org") (id . ,id))
                       (blocklist ((xmlns . "urn:xmpp:blocking")))))
        (should-not jabber-open-info-queries)
        (should-not (plist-get state :blocking-list))))))

(require 'jabber-core)

(defun jabber-test-blocking--connection ()
  "Return a disposable established FSM with Stream Management state."
  (let ((jc (make-symbol "blocking-test")))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data
         (jabber-sm--reset
          (list :username "me" :server "example.org"
                :connection (list 'transport) :session-id "stream"
                :blocking-session (list nil) :blocking-status 'pending)))
    jc))

(defun jabber-test-blocking--drain (jc)
  "Run the real SM drain that replaces JC's state between replies."
  (let ((old (fsm-get-state-data jc)))
    (jabber-sm--drain-pending jc old)
    (should-not (eq old (fsm-get-state-data jc)))
    (should (eq (plist-get old :connection)
                (plist-get (fsm-get-state-data jc) :connection)))))

(ert-deftest jabber-test-blocking-fsm-replaced-state-responses ()
  "Real FSM replies survive the ordinary SM drain's plist replacement."
  (dolist (kind '(disco snapshot block))
    (dolist (failure '(nil t))
      (let* ((jc (jabber-test-blocking--connection))
             (jabber-connections (list jc))
             (jabber-open-info-queries nil)
             (jabber-blocking-ready-hook nil)
             (jabber-disco-info-cache (make-hash-table :test #'equal))
             sent)
        (cl-letf (((symbol-function 'jabber-send-sexp)
                   (lambda (_jc xml &rest _) (push xml sent))))
          (pcase kind
            ('disco (jabber-blocking--on-connect jc))
            ('snapshot (jabber-blocking--fetch jc))
            (_ (jabber-blocking--change jc "peer@example.net" kind)))
          (let* ((old (fsm-get-state-data jc))
                 (id (jabber-xml-get-attribute (car sent) 'id))
                 (payload
                  (if failure
                      '(error ((type . "cancel"))
                              (service-unavailable
                               ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))
                    (pcase kind
                      ('disco '(query ((xmlns . "http://jabber.org/protocol/disco#info"))))
                      ('snapshot '(blocklist ((xmlns . "urn:xmpp:blocking"))
                                            (item ((jid . "peer@example.net")))))))))
            (jabber-test-blocking--drain jc)
            (fsm-send-sync jc
                           `(:stanza (iq ((type . ,(if failure "error" "result"))
                                          (id . ,id) (from . "example.org"))
                                         ,@(when payload (list payload)))))
            (should-not (assoc id jabber-open-info-queries))
            (should (eq (plist-get old :blocking-status) 'pending))
            (if (eq kind 'block)
                (should (= (length sent) (if failure 1 2)))
              (should (eq (plist-get (fsm-get-state-data jc) :blocking-status)
                          (if failure 'failed
                            (if (eq kind 'disco) 'unsupported 'ready)))))
            (when (and (eq kind 'snapshot) (not failure))
              (should (jabber-blocking-blocked-p jc "peer@example.net")))))))))

(ert-deftest jabber-test-blocking-fsm-retired-response ()
  "Replacement transport, stream, blocking session or account rejects old IQs."
  (dolist (change '((:connection . new-transport)
                     (:session-id . "new-stream")
                     (:blocking-session . new-session)
                     (:username . "other") (:server . "example.net")))
    (let* ((jc (jabber-test-blocking--connection))
           (jabber-connections (list jc))
           (jabber-open-info-queries nil)
           (jabber-blocking-ready-hook nil)
           sent)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (_jc xml &rest _) (setq sent xml))))
        (jabber-blocking--fetch jc)
        (jabber-test-blocking--drain jc)
        (plist-put (fsm-get-state-data jc) (car change) (cdr change))
        (let* ((id (jabber-xml-get-attribute sent 'id))
               (pending (assoc id jabber-open-info-queries)))
          (fsm-send-sync jc
                         `(:stanza (iq ((type . "result") (id . ,id))
                                       (blocklist ((xmlns . "urn:xmpp:blocking"))))))
          (should (eq pending (assoc id jabber-open-info-queries)))
          (should (eq (plist-get (fsm-get-state-data jc) :blocking-status)
                      'pending)))))))

(ert-deftest jabber-test-blocking-omemo-replaced-state ()
  "Peer replies survive SM copies but not transport or session replacement."
  (dolist (change '(nil (:connection . replacement)
                        (:session-id . "replacement")
                        (:blocking-session . replacement)
                        (:blocking-list "peer@example.net")))
    (dolist (failure '(nil t))
      (let* ((jc (jabber-test-blocking--connection))
             (jabber-connections (list jc))
             success error-callback result)
        (plist-put (fsm-get-state-data jc) :blocking-status 'ready)
        (cl-letf (((symbol-function 'jabber-pubsub-request)
                   (lambda (_jc _jid _node ok err)
                     (setq success ok error-callback err))))
          (jabber-omemo--request-peer
           jc "peer@example.net" "test-node"
           (lambda (&rest _) (setq result 'success))
           (lambda (&rest _) (setq result 'failure))
           (lambda (value) (setq result (list value))))
          (jabber-test-blocking--drain jc)
          (when change
            (plist-put (fsm-get-state-data jc) (car change) (cdr change)))
          (funcall (if failure error-callback success) jc nil nil)
          (should (equal result (if change '(nil)
                                  (if failure 'failure 'success)))))))))

(provide 'jabber-test-blocking)

;;; jabber-test-blocking.el ends here
