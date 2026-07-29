;;; jabber-test-util.el --- Tests for jabber-util  -*- lexical-binding: t; -*-

;;; Commentary:

;; JID manipulation, time parsing, and utility functions.

;;; Code:

(require 'ert)
(require 'jabber-util)
(require 'jabber-uri)

(defvar jabber-jid-obarray (make-vector 127 0))

(ert-deftest jabber-test-util-connection-active-requires-session ()
  "Only a listed FSM with an established session is active."
  (let* ((connection (make-symbol "connection"))
         (jabber-connections (list connection)))
    (put connection :state :connecting)
    (should-not (jabber-connection-active-p connection))
    (put connection :state :session-established)
    (should (jabber-connection-active-p connection))))

;;; Group 1: JID functions

(ert-deftest jabber-test-util-jid-username-normal ()
  "Extract username from full JID."
  (should (string= (jabber-jid-username "alice@example.com/home")
                   "alice")))

(ert-deftest jabber-test-util-jid-username-bare ()
  "Extract username from bare JID."
  (should (string= (jabber-jid-username "alice@example.com")
                   "alice")))

(ert-deftest jabber-test-util-jid-username-no-at ()
  "Return nil for server-only JID."
  (should (null (jabber-jid-username "example.com"))))

(ert-deftest jabber-test-util-jid-user-full ()
  "Extract bare JID from full JID."
  (should (string= (jabber-jid-user "alice@example.com/home")
                   "alice@example.com")))

(ert-deftest jabber-test-util-jid-user-bare ()
  "Bare JID returned unchanged."
  (should (string= (jabber-jid-user "alice@example.com")
                   "alice@example.com")))

(ert-deftest jabber-test-util-jid-user-transport ()
  "Transport JID (no @) returns the server part."
  (should (string= (jabber-jid-user "transport.example.com")
                   "transport.example.com")))

(ert-deftest jabber-test-util-jid-server-normal ()
  "Extract server from normal JID."
  (should (string= (jabber-jid-server "alice@example.com/home")
                   "example.com")))

(ert-deftest jabber-test-util-jid-server-bare ()
  "Extract server from bare JID."
  (should (string= (jabber-jid-server "alice@example.com")
                   "example.com")))

(ert-deftest jabber-test-util-jid-server-no-at ()
  "Extract server from server-only JID."
  (should (string= (jabber-jid-server "example.com")
                   "example.com")))

(ert-deftest jabber-test-util-jid-resource-present ()
  "Extract resource from full JID."
  (should (string= (jabber-jid-resource "alice@example.com/home")
                   "home")))

(ert-deftest jabber-test-util-jid-resource-absent ()
  "Return nil when no resource."
  (should (null (jabber-jid-resource "alice@example.com"))))

(ert-deftest jabber-test-util-jid-resource-multiple-slashes ()
  "Resource can contain slashes."
  (should (string= (jabber-jid-resource "alice@example.com/home/desk")
                   "home/desk")))

(ert-deftest jabber-test-util-jid-symbol-string ()
  "Intern a JID string into jabber-jid-obarray."
  (let ((jabber-jid-obarray (make-vector 127 0)))
    (let ((sym (jabber-jid-symbol "Alice@Example.COM/Home")))
      (should (symbolp sym))
      (should (string= (symbol-name sym) "alice@example.com")))))

(ert-deftest jabber-test-util-jid-symbol-passthrough ()
  "Symbol input passes through unchanged."
  (let ((jabber-jid-obarray (make-vector 127 0)))
    (let ((sym 'already-a-symbol))
      (should (eq (jabber-jid-symbol sym) sym)))))

;;; Group 2: time functions

(ert-deftest jabber-test-util-encode-time-format ()
  "Encode a known time value to XEP-0082 string."
  (let ((time (encode-time '(0 0 0 1 1 2024 nil -1 0))))
    (should (string= (jabber-encode-time time) "2024-01-01T00:00:00Z"))))

(ert-deftest jabber-test-util-parse-legacy-time ()
  "Parse legacy ccyymmddThh:mm:ss format."
  (let ((result (jabber-parse-legacy-time "20240101T12:30:45")))
    (should result)
    ;; jabber-parse-legacy-time interprets as local time
    (let ((expected (encode-time '(45 30 12 1 1 2024 nil -1 nil))))
      (should (= (float-time result) (float-time expected))))))

(ert-deftest jabber-test-util-encode-legacy-time ()
  "Encode a time value to legacy format in UTC."
  (let ((time (encode-time '(45 30 12 1 1 2024 nil -1 0))))
    (should (string= (jabber-encode-legacy-time time) "20240101T12:30:45"))))

(ert-deftest jabber-test-util-legacy-time-encode-produces-utc ()
  "Legacy encode outputs UTC representation."
  ;; Encode a known UTC time and verify the output
  (let ((time (encode-time '(0 0 15 25 2 2024 nil -1 0))))
    (should (string= (jabber-encode-legacy-time time) "20240225T15:00:00"))))

(ert-deftest jabber-test-util-parse-time-encode-time-roundtrip ()
  "XEP-0082 parse then encode roundtrip."
  (let* ((stamp "2024-02-25T23:32:40Z")
         (parsed (jabber-parse-time stamp))
         (encoded (jabber-encode-time parsed)))
    (should (string= encoded "2024-02-25T23:32:40Z"))))

;;; Group 3: IQ helpers

(ert-deftest jabber-test-util-iq-query-normal ()
  "Extract query child from IQ stanza."
  (let ((iq '(iq ((type . "result"))
               (query ((xmlns . "jabber:iq:roster"))
                (item ((jid . "bob@example.com")))))))
    (should (eq (jabber-xml-node-name (jabber-iq-query iq)) 'query))))

(ert-deftest jabber-test-util-iq-query-skips-error ()
  "Query extraction skips error child."
  (let ((iq '(iq ((type . "error"))
               (query ((xmlns . "jabber:iq:roster")))
               (error ((type . "cancel"))))))
    (should (eq (jabber-xml-node-name (jabber-iq-query iq)) 'query))))

(ert-deftest jabber-test-util-iq-query-no-children ()
  "Return nil when IQ has no query child."
  (let ((iq '(iq ((type . "result")))))
    (should (null (jabber-iq-query iq)))))

(ert-deftest jabber-test-util-iq-error-present ()
  "Extract error child from IQ stanza."
  (let ((iq '(iq ((type . "error"))
               (query nil)
               (error ((type . "cancel"))
                (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))
    (should (eq (jabber-xml-node-name (jabber-iq-error iq)) 'error))))

(ert-deftest jabber-test-util-iq-error-absent ()
  "Return nil when IQ has no error."
  (let ((iq '(iq ((type . "result")) (query nil))))
    (should (null (jabber-iq-error iq)))))

(ert-deftest jabber-test-util-iq-xmlns ()
  "Extract namespace of query child."
  (let ((iq '(iq ((type . "get"))
               (query ((xmlns . "jabber:iq:roster"))))))
    (should (string= (jabber-iq-xmlns iq) "jabber:iq:roster"))))

;;; Group 4: jabber-x-delay

(ert-deftest jabber-test-util-x-delay-xep0203 ()
  "Parse XEP-0203 delay element."
  (let ((delay '(delay ((xmlns . "urn:xmpp:delay")
                        (stamp . "2024-02-25T23:32:40Z")))))
    (should (jabber-x-delay delay))))

(ert-deftest jabber-test-util-x-delay-xep0091 ()
  "Parse XEP-0091 legacy delay element."
  (let ((delay '(x ((xmlns . "jabber:x:delay")
                    (stamp . "20240225T23:32:40")))))
    (should (jabber-x-delay delay))))

(ert-deftest jabber-test-util-x-delay-none ()
  "Return nil when no delay info present."
  (let ((node '(body ((xmlns . "jabber:client")))))
    (should (null (jabber-x-delay node)))))

;;; Group 5: error parsing

(ert-deftest jabber-test-util-parse-error-new-style ()
  "Parse new-style error with type and condition."
  (let ((err '(error ((type . "cancel"))
               (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
    (should (string= (jabber-parse-error err) "Item not found"))))

(ert-deftest jabber-test-util-parse-error-new-style-with-text ()
  "Parse new-style error with text."
  (let ((err '(error ((type . "cancel"))
               (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))
               (text ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))
                "The item was not found"))))
    (should (string-match-p "Item not found" (jabber-parse-error err)))
    (should (string-match-p "The item was not found" (jabber-parse-error err)))))

(ert-deftest jabber-test-util-parse-error-legacy ()
  "Parse legacy error with code."
  (let ((err '(error ((code . "404"))
               "Not found")))
    (should (string-match-p "Not found" (jabber-parse-error err)))))

(ert-deftest jabber-test-util-error-condition ()
  "Extract condition symbol from error."
  (let ((err '(error ((type . "cancel"))
               (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
    (should (eq (jabber-error-condition err) 'item-not-found))))

(ert-deftest jabber-test-util-stream-error-condition ()
  "Extract condition from stream error."
  (let ((err '(stream:error nil
               (host-unknown ((xmlns . "urn:ietf:params:xml:ns:xmpp-streams"))))))
    (should (eq (jabber-stream-error-condition err) 'host-unknown))))

(ert-deftest jabber-test-util-parse-stream-error ()
  "Parse stream error to human-readable string."
  (let ((err '(stream:error nil
               (host-unknown ((xmlns . "urn:ietf:params:xml:ns:xmpp-streams"))))))
    (should (string= (jabber-parse-stream-error err) "Host unknown"))))

(ert-deftest jabber-test-util-parse-stream-error-with-text ()
  "Parse stream error with text child."
  (let ((err '(stream:error nil
               (host-unknown ((xmlns . "urn:ietf:params:xml:ns:xmpp-streams")))
               (text nil "No such host"))))
    (should (string-match-p "Host unknown" (jabber-parse-stream-error err)))
    (should (string-match-p "No such host" (jabber-parse-stream-error err)))))

;;; Group 6: other pure functions

(ert-deftest jabber-test-util-unhex ()
  "Decode hex-encoded UTF-8 string."
  (should (string= (jabber-unhex "hello%20world") "hello world")))

(ert-deftest jabber-test-util-jabber-string>-numerical-greater ()
  "Larger number returns t."
  (should (jabber-string>-numerical "200" "100")))

(ert-deftest jabber-test-util-jabber-string>-numerical-less ()
  "Smaller number returns nil."
  (should-not (jabber-string>-numerical "50" "100")))

(ert-deftest jabber-test-util-jabber-string>-numerical-equal ()
  "Equal numbers return nil."
  (should-not (jabber-string>-numerical "100" "100")))

(ert-deftest jabber-test-util-jabber-string>-numerical-longer ()
  "Longer string (more digits) is greater."
  (should (jabber-string>-numerical "1000" "999")))

(ert-deftest jabber-test-util-signal-error ()
  "jabber-signal-error signals jabber-error condition."
  (should-error (jabber-signal-error "Cancel" 'item-not-found "Not found")
                :type 'jabber-error))

(ert-deftest jabber-test-util-tree-map ()
  "Apply function to all atoms in a tree."
  (should (equal (jabber-tree-map #'1+ '(1 (2 3) 4))
                 '(2 (3 4) 5))))

;;; Group: Decrypt-failure placeholder predicate

(ert-deftest jabber-test-util-decrypt-failure-body-p-matches-placeholder ()
  (should (jabber--decrypt-failure-body-p "[OMEMO: could not decrypt]"))
  (should (jabber--decrypt-failure-body-p "[OpenPGP: could not decrypt]")))

(ert-deftest jabber-test-util-decrypt-failure-body-p-rejects-real-text ()
  (should-not (jabber--decrypt-failure-body-p nil))
  (should-not (jabber--decrypt-failure-body-p "hello there"))
  (should-not (jabber--decrypt-failure-body-p
               "quoting [OMEMO: could not decrypt] in a longer message"))
  (should-not (jabber--decrypt-failure-body-p
               "[OMEMO: could not decrypt] trailing text")))

;;; Group: Roster membership predicate

(defun jabber-test-util--make-jc-with-roster (&rest jids)
  "Create a fake connection whose roster contains JIDS."
  (let ((jc (gensym "jabber-test-util-jc-")))
    (put jc :state-data
         (list :roster (mapcar #'jabber-jid-symbol jids)))
    jc))

(ert-deftest jabber-test-util-roster-contact-p-bare-jid ()
  (let ((jc (jabber-test-util--make-jc-with-roster "alice@example.com")))
    (should (jabber-roster-contact-p jc "alice@example.com"))))

(ert-deftest jabber-test-util-roster-contact-p-full-jid ()
  "Resource is stripped before the roster lookup."
  (let ((jc (jabber-test-util--make-jc-with-roster "alice@example.com")))
    (should (jabber-roster-contact-p jc "alice@example.com/laptop"))))

(ert-deftest jabber-test-util-roster-contact-p-stranger ()
  (let ((jc (jabber-test-util--make-jc-with-roster "alice@example.com")))
    (should-not (jabber-roster-contact-p jc "mallory@example.com"))))

(ert-deftest jabber-test-util-roster-contact-p-nil-args ()
  (let ((jc (jabber-test-util--make-jc-with-roster "alice@example.com")))
    (should-not (jabber-roster-contact-p nil "alice@example.com"))
    (should-not (jabber-roster-contact-p jc nil))))

;;; Group: JID completion sorting

(defun jabber-test-util--contact (jid show &optional name)
  "Return an uninterned roster symbol JID with SHOW and NAME set."
  (let ((sym (make-symbol jid)))
    (put sym 'show show)
    (when name (put sym 'name name))
    sym))

(ert-deftest jabber-test-util-presence-rank-orders-shows ()
  "Available shows rank ahead of away shows, offline ranks last."
  (let ((online (jabber-test-util--contact "a@x.com" ""))
        (chatty (jabber-test-util--contact "b@x.com" "chat"))
        (away (jabber-test-util--contact "c@x.com" "away"))
        (offline (jabber-test-util--contact "d@x.com" nil)))
    (should (< (jabber--presence-rank chatty)
               (jabber--presence-rank online)))
    (should (< (jabber--presence-rank online)
               (jabber--presence-rank away)))
    (should (< (jabber--presence-rank away)
               (jabber--presence-rank offline)))))

(ert-deftest jabber-test-util-jid-candidate-lessp-availability-first ()
  "An online contact sorts before an offline one regardless of name."
  (let* ((online (jabber-test-util--contact "zed@x.com" ""))
         (offline (jabber-test-util--contact "ann@x.com" nil))
         (table (list (cons "zed@x.com" online)
                      (cons "ann@x.com" offline))))
    (should (jabber--jid-candidate-lessp "zed@x.com" "ann@x.com" table))
    (should-not (jabber--jid-candidate-lessp "ann@x.com" "zed@x.com" table))))

(ert-deftest jabber-test-util-jid-candidate-lessp-name-tiebreak ()
  "Same availability falls back to case-insensitive name order."
  (let* ((a (jabber-test-util--contact "ann@x.com" ""))
         (b (jabber-test-util--contact "Bob@x.com" ""))
         (table (list (cons "ann@x.com" a)
                      (cons "Bob@x.com" b))))
    (should (jabber--jid-candidate-lessp "ann@x.com" "Bob@x.com" table))
    (should-not (jabber--jid-candidate-lessp "Bob@x.com" "ann@x.com" table))))

(ert-deftest jabber-test-util-completion-metadata-sorts-by-availability ()
  "The completion table's display sort puts available contacts first."
  (let* ((jabber-jid-completion-display 'jid)
         (table (list (cons "off@x.com" (jabber-test-util--contact "off@x.com" nil))
                      (cons "away@x.com" (jabber-test-util--contact "away@x.com" "away"))
                      (cons "on@x.com" (jabber-test-util--contact "on@x.com" ""))))
         (collection (jabber--jid-completion-with-metadata table))
         (metadata (funcall collection "" nil 'metadata))
         (sorter (cdr (assq 'display-sort-function (cdr metadata)))))
    (should (functionp sorter))
    (should (equal '("on@x.com" "away@x.com" "off@x.com")
                   (funcall sorter '("off@x.com" "away@x.com" "on@x.com"))))))

;;; Group 7: XMPP URI parsing

(ert-deftest jabber-test-uri-parse-chat ()
  "Parse a plain chat URI without a query method."
  (should (equal (jabber-uri-parse "xmpp:alice@example.com")
                 '(:jid "alice@example.com" :method nil :args nil))))

(ert-deftest jabber-test-uri-parse-method-and-arguments ()
  "Parse and decode an XMPP URI method and its arguments."
  (should (equal
           (jabber-uri-parse
            "xmpp:room@example.com?command;node=hello%20world;action=next")
           '(:jid "room@example.com"
             :method "command"
             :args (("node" . "hello world") ("action" . "next"))))))

(ert-deftest jabber-test-uri-rejects-authority ()
  "Reject XMPP URIs containing an authority part."
  (should-error (jabber-uri-parse "xmpp://alice@example.com")))

(ert-deftest jabber-test-uri-rejects-trailing-text ()
  "Reject text after a complete XMPP URI."
  (should-error (jabber-uri-parse "xmpp:alice@example.com?join trailing")))

(provide 'jabber-test-util)
;;; jabber-test-util.el ends here
