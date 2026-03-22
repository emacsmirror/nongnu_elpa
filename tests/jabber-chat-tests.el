;;; jabber-chat-tests.el --- Tests for jabber-chat  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-chat)

;; jabber-chat uses this constant from jabber-muc, which has too many
;; dependencies to load in isolation.  Define it here for tests.
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")

;;; Group 1: jabber-chat--msg-plist-from-stanza

(ert-deftest jabber-chat-test-plist-from-stanza-basic ()
  "Basic chat message produces correct plist keys."
  (let* ((stanza '(message ((from . "alice@example.com/res")
                            (type . "chat"))
                           (body () "Hello!")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "alice@example.com/res" (plist-get plist :from)))
    (should (string= "Hello!" (plist-get plist :body)))
    (should-not (plist-get plist :subject))
    (should-not (plist-get plist :delayed))
    (should-not (plist-get plist :oob-url))
    (should-not (plist-get plist :error-text))
    (should (plist-get plist :timestamp))))

(ert-deftest jabber-chat-test-plist-from-stanza-nil-body ()
  "Message with no body produces nil :body."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (subject () "Topic")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :body))
    (should (string= "Topic" (plist-get plist :subject)))))

(ert-deftest jabber-chat-test-plist-from-stanza-muc ()
  "MUC message has room JID with nick as resource."
  (let* ((stanza '(message ((from . "room@conf.example.com/Alice")
                            (type . "groupchat"))
                           (body () "Hi room")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "room@conf.example.com/Alice" (plist-get plist :from)))
    (should (string= "Hi room" (plist-get plist :body)))))

(ert-deftest jabber-chat-test-plist-from-stanza-delay ()
  "Message with XEP-0203 delay element is marked delayed."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Old message")
                           (delay ((xmlns . "urn:xmpp:delay")
                                   (stamp . "2025-01-15T10:30:00Z")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (plist-get plist :delayed))
    (should (string= "Old message" (plist-get plist :body)))))

(ert-deftest jabber-chat-test-plist-from-stanza-forced-delay ()
  "Passing DELAYED arg forces :delayed to non-nil."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Backlog")))
         (plist (jabber-chat--msg-plist-from-stanza stanza t)))
    (should (plist-get plist :delayed))))

(ert-deftest jabber-chat-test-plist-from-stanza-oob ()
  "OOB URL and description are extracted."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Check this")
                           (x ((xmlns . "jabber:x:oob"))
                              (url () "https://example.com/file.png")
                              (desc () "A picture"))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (string= "https://example.com/file.png" (plist-get plist :oob-url)))
    (should (string= "A picture" (plist-get plist :oob-desc)))))

(ert-deftest jabber-chat-test-plist-from-stanza-error ()
  "Error node is parsed into :error-text."
  (let* ((stanza '(message ((from . "alice@example.com")
                            (type . "error"))
                           (body () "Bad request")
                           (error ((type . "modify") (code . "400"))
                                  (bad-request
                                   ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (stringp (plist-get plist :error-text)))))

(ert-deftest jabber-chat-test-plist-from-stanza-oob-no-url ()
  "OOB element with no url child yields nil :oob-url."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Check this")
                           (x ((xmlns . "jabber:x:oob")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :oob-url))))

(ert-deftest jabber-chat-test-plist-from-stanza-invite ()
  "MUC invitation preserves raw XML in :xml-data."
  (let* ((stanza '(message ((from . "room@conf.example.com"))
                           (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                              (invite ((from . "alice@example.com"))
                                      (reason () "Join us")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (plist-get plist :xml-data))
    (should (eq stanza (plist-get plist :xml-data)))))

(ert-deftest jabber-chat-test-plist-from-stanza-no-invite-no-xml ()
  "Non-invitation message does not include :xml-data."
  (let* ((stanza '(message ((from . "alice@example.com"))
                           (body () "Normal message")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :xml-data))))

;;; Group 2: jabber-chat--oob-field

(ert-deftest jabber-chat-test-oob-field-url ()
  "Extract URL from OOB node."
  (let ((oob '(x ((xmlns . "jabber:x:oob"))
                  (url () "https://example.com/file.png"))))
    (should (string= (jabber-chat--oob-field oob 'url)
                     "https://example.com/file.png"))))

(ert-deftest jabber-chat-test-oob-field-missing-child ()
  "Return nil when OOB child element is absent."
  (let ((oob '(x ((xmlns . "jabber:x:oob"))
                  (url () "https://example.com/file.png"))))
    (should-not (jabber-chat--oob-field oob 'desc))))

(ert-deftest jabber-chat-test-oob-field-nil-node ()
  "Return nil when OOB node is nil."
  (should-not (jabber-chat--oob-field nil 'url)))

;;; Group 3: jabber-chat--has-muc-invite-p

(ert-deftest jabber-chat-test-has-muc-invite-positive ()
  "Detect MUC invitation in stanza."
  (let ((stanza '(message ((from . "room@conf.example.com"))
                  (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (invite ((from . "alice@example.com")))))))
    (should (jabber-chat--has-muc-invite-p stanza))))

(ert-deftest jabber-chat-test-has-muc-invite-negative ()
  "Return nil for stanza without MUC invitation."
  (let ((stanza '(message ((from . "alice@example.com"))
                  (body () "Hello"))))
    (should-not (jabber-chat--has-muc-invite-p stanza))))

(ert-deftest jabber-chat-test-has-muc-invite-muc-user-no-invite ()
  "Return nil when muc#user element exists but has no invite child."
  (let ((stanza '(message ((from . "room@conf.example.com"))
                  (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (status ((code . "110")))))))
    (should-not (jabber-chat--has-muc-invite-p stanza))))

;;; Group 4: jabber-chat-entry-time

(ert-deftest jabber-chat-test-entry-time-plist ()
  "Entry time from a msg-plist entry."
  (let* ((ts (encode-time '(0 30 14 15 1 2025 nil nil 0)))
         (entry (list :foreign (list :from "alice" :timestamp ts))))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-chat-test-entry-time-rare-time ()
  "Entry time from a :rare-time entry."
  (let* ((ts (encode-time '(0 0 12 10 3 2025 nil nil 0)))
         (entry (list :rare-time ts)))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-chat-test-entry-time-string-notice ()
  "Entry time from a string :muc-notice with :time in cddr."
  (let* ((ts (current-time))
         (entry (list :muc-notice "user enters the room" :time ts)))
    (should (equal ts (jabber-chat-entry-time entry)))))

(ert-deftest jabber-chat-test-entry-time-string-no-time ()
  "String entry without :time returns nil."
  (let ((entry (list :notice "some notice")))
    (should-not (jabber-chat-entry-time entry))))

;;; Group 5: jabber-chat--decrypt-if-needed

(ert-deftest jabber-chat-test-decrypt-if-needed-returns-xml-unchanged ()
  "No-op decryption returns xml-data unchanged."
  (let ((xml '(message ((from . "alice@example.com") (type . "chat"))
                       (body () "Hello!"))))
    (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))))

(ert-deftest jabber-chat-test-decrypt-if-needed-preserves-complex-stanza ()
  "No-op decryption preserves a stanza with nested elements."
  (let ((xml '(message ((from . "bob@example.com"))
                       (body () "Encrypted?")
                       (x ((xmlns . "jabber:x:oob"))
                          (url () "https://example.com/file.png")))))
    (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))))

;;; Group 6: jabber-chat--set-body

(ert-deftest jabber-chat-test-set-body-replaces-existing ()
  "set-body replaces existing <body> text."
  (let ((xml '(message ((from . "alice@example.com"))
                       (body () "old text"))))
    (jabber-chat--set-body xml "new text")
    (should (string= "new text"
                      (car (jabber-xml-node-children
                            (car (jabber-xml-get-children xml 'body))))))))

(ert-deftest jabber-chat-test-set-body-creates-missing ()
  "set-body appends <body> when none exists."
  (let ((xml '(message ((from . "alice@example.com")))))
    (jabber-chat--set-body xml "created")
    (let ((body-el (car (jabber-xml-get-children xml 'body))))
      (should body-el)
      (should (string= "created"
                        (car (jabber-xml-node-children body-el)))))))

;;; Group 7: decrypt handler dispatch

(ert-deftest jabber-chat-test-register-decrypt-handler-adds-entry ()
  "Register a handler, assert it appears in the alist."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (should (assq 'test-handler jabber-chat-decrypt-handlers))))

(ert-deftest jabber-chat-test-unregister-decrypt-handler-removes-entry ()
  "Register then unregister, assert the alist is empty."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (jabber-chat-unregister-decrypt-handler 'test-handler)
    (should-not jabber-chat-decrypt-handlers)))

(ert-deftest jabber-chat-test-register-decrypt-handler-replaces-existing ()
  "Register a handler twice, assert only one entry with new priority."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 10 :error-label "Test")
    (jabber-chat-register-decrypt-handler
     'test-handler :detect #'ignore :decrypt #'ignore
     :priority 20 :error-label "Test")
    (should (= 1 (length jabber-chat-decrypt-handlers)))
    (should (= 20 (plist-get (cdr (assq 'test-handler
                                         jabber-chat-decrypt-handlers))
                              :priority)))))

(ert-deftest jabber-chat-test-decrypt-dispatches-to-matching-handler ()
  "Handler whose :detect matches gets its :decrypt called."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t)
        (called nil))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq called t) xml)
     :priority 10
     :error-label "Test")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should called))))

(ert-deftest jabber-chat-test-decrypt-skips-non-matching-handler ()
  "Handler whose :detect returns nil leaves xml-data unchanged."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) nil)
     :decrypt (lambda (_jc _xml _parsed) (error "Should not be called"))
     :priority 10
     :error-label "Test")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (should (eq xml (jabber-chat--decrypt-if-needed nil xml))))))

(ert-deftest jabber-chat-test-decrypt-priority-order ()
  "Lower-priority handler runs first when both match."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t)
        (winner nil))
    (jabber-chat-register-decrypt-handler
     'handler-20
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq winner 20) xml)
     :priority 20
     :error-label "H20")
    (jabber-chat-register-decrypt-handler
     'handler-10
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc xml _parsed) (setq winner 10) xml)
     :priority 10
     :error-label "H10")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should (= 10 winner)))))

(ert-deftest jabber-chat-test-decrypt-error-replaces-body ()
  "Handler that signals error gets body replaced with error label."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (jabber-chat-register-decrypt-handler
     'test-handler
     :detect (lambda (_xml) 'detected)
     :decrypt (lambda (_jc _xml _parsed) (error "Decrypt boom"))
     :priority 10
     :error-label "BOOM")
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "fallback"))))
      (jabber-chat--decrypt-if-needed nil xml)
      (should (string= "[BOOM: could not decrypt]"
                        (car (jabber-xml-node-children
                              (car (jabber-xml-get-children xml 'body)))))))))

(ert-deftest jabber-chat-test-decrypt-no-handlers-returns-unchanged ()
  "With empty handler alist, xml-data passes through."
  (let ((jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil)
        (jabber-chat--crypto-loaded t))
    (let ((xml '(message ((from . "alice@example.com"))
                         (body () "hello"))))
      (should (eq xml (jabber-chat--decrypt-if-needed nil xml))))))

(ert-deftest jabber-chat-test-decrypt-skips-nil-from ()
  "Stanza with no from attribute bypasses decrypt dispatch entirely."
  (let ((jabber-chat--crypto-loaded t)
        (called nil))
    (jabber-chat-register-decrypt-handler
     'test-nil-from
     :detect  (lambda (_xml) (setq called t) nil)
     :decrypt (lambda (_jc _xml _det) nil)
     :priority 1
     :error-label "test")
    (unwind-protect
        (let ((xml '(message () (body () "no from"))))
          (should (eq xml (jabber-chat--decrypt-if-needed nil xml)))
          (should-not called))
      (jabber-chat-unregister-decrypt-handler 'test-nil-from))))

(provide 'jabber-chat-tests)

;;; jabber-chat-tests.el ends here
