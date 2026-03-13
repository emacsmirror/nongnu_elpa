;;; jabber-chat-tests.el --- Tests for jabber-chat  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-chat)

;; jabber-chat uses this constant from jabber-muc, which has too many
;; dependencies to load in isolation.  Define it here for tests.
(defvar jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user")

;;; ---- Group 1: jabber-chat--msg-plist-from-stanza ----

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

;;; ---- Group 2: jabber-chat--oob-field ----

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

;;; ---- Group 3: jabber-chat--has-muc-invite-p ----

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

;;; ---- Group 4: jabber-chat-entry-time ----

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

(provide 'jabber-chat-tests)

;;; jabber-chat-tests.el ends here
