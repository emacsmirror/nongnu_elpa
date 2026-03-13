;;; jabber-muc-tests.el --- Tests for jabber-muc  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time
;; from jabber-core.el and jabber-chat.el:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-muc)

(defmacro jabber-muc-test-with-rooms (rooms &rest body)
  "Run BODY with ROOMS as active groupchats.
ROOMS is an alist of (group . nickname)."
  (declare (indent 1))
  `(let ((jabber-muc--rooms (make-hash-table :test #'equal)))
     (dolist (r ,rooms)
       (puthash (car r) (cons nil (cdr r)) jabber-muc--rooms))
     ,@body))

;;; ---- Group 1: jabber-muc-message-p ----

(ert-deftest jabber-test-muc-message-p-groupchat ()
  "Groupchat type message is a MUC message."
  (let ((msg '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Hello"))))
    (should (jabber-muc-message-p msg))))

(ert-deftest jabber-test-muc-message-p-error-from-room ()
  "Error from a pending groupchat is a MUC message."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (puthash (intern "room@conference.example.com" jabber-jid-obarray)
             "mynick" jabber-pending-groupchats)
    (let ((msg '(message ((from . "room@conference.example.com")
                          (type . "error"))
                 (error ((type . "cancel"))))))
      (should (jabber-muc-message-p msg)))))

(ert-deftest jabber-test-muc-message-p-chat ()
  "Normal chat message is not a MUC message."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (let ((msg '(message ((from . "alice@example.com/home")
                          (type . "chat"))
                 (body nil "Hi"))))
      (should-not (jabber-muc-message-p msg)))))

(ert-deftest jabber-test-muc-message-p-invite ()
  "MUC invite is a MUC message."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (let ((msg '(message ((from . "room@conference.example.com"))
                 (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                  (invite ((from . "alice@example.com"))
                   (reason nil "Join us"))))))
      (should (jabber-muc-message-p msg)))))

;;; ---- Group 2: jabber-muc-sender-p ----

(ert-deftest jabber-test-muc-sender-p-full-jid ()
  "Full JID from active groupchat is a MUC sender."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (should (jabber-muc-sender-p "room@conference.example.com/othernick"))))

(ert-deftest jabber-test-muc-sender-p-bare-jid ()
  "Bare JID (no resource) is not a MUC sender."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (should-not (jabber-muc-sender-p "room@conference.example.com"))))

(ert-deftest jabber-test-muc-sender-p-not-active ()
  "JID not in active groupchats is not a MUC sender."
  (jabber-muc-test-with-rooms nil
    (should-not (jabber-muc-sender-p "room@conference.example.com/nick"))))

;;; ---- Group 3: jabber-muc-private-message-p ----

(ert-deftest jabber-test-muc-private-message-p-private ()
  "Private message from MUC participant returns non-nil."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((msg '(message ((from . "room@conference.example.com/othernick")
                          (type . "chat"))
                 (body nil "Psst"))))
      (should (jabber-muc-private-message-p msg)))))

(ert-deftest jabber-test-muc-private-message-p-groupchat ()
  "Groupchat type message is not a private message."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((msg '(message ((from . "room@conference.example.com/nick")
                          (type . "groupchat"))
                 (body nil "Hello all"))))
      (should-not (jabber-muc-private-message-p msg)))))

;;; ---- Group 4: jabber-muc-presence-p ----

(ert-deftest jabber-test-muc-presence-p-with-marker ()
  "Presence with muc#user namespace is MUC presence."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (let ((pres '(presence ((from . "room@conference.example.com/nick"))
                  (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                   (item ((affiliation . "member") (role . "participant")))))))
      (should (jabber-muc-presence-p pres)))))

(ert-deftest jabber-test-muc-presence-p-without-marker ()
  "Presence without muc#user namespace is not MUC presence."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (let ((pres '(presence ((from . "alice@example.com/home")))))
      (should-not (jabber-muc-presence-p pres)))))

(ert-deftest jabber-test-muc-presence-p-error-pending ()
  "Error presence from pending groupchat is MUC presence."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (puthash (intern "room@conference.example.com" jabber-jid-obarray)
             "mynick" jabber-pending-groupchats)
    (let ((pres '(presence ((from . "room@conference.example.com/mynick")
                            (type . "error"))
                  (error ((type . "cancel"))))))
      (should (jabber-muc-presence-p pres)))))

;;; ---- Group 5: jabber-muc accessor functions ----

(ert-deftest jabber-test-muc-join-set-and-nickname ()
  "jabber-muc-join-set stores room; jabber-muc-nickname retrieves nick."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (jabber-muc-join-set "room@example.com" 'fake-jc "mynick")
    (should (string= (jabber-muc-nickname "room@example.com") "mynick"))))

(ert-deftest jabber-test-muc-join-set-and-connection ()
  "jabber-muc-connection retrieves the stored connection."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (jabber-muc-join-set "room@example.com" 'fake-jc "mynick")
    (should (eq (jabber-muc-connection "room@example.com") 'fake-jc))))

(ert-deftest jabber-test-muc-leave-remove ()
  "jabber-muc-leave-remove removes the room."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (jabber-muc-join-set "room@example.com" 'fake-jc "mynick")
    (jabber-muc-leave-remove "room@example.com")
    (should-not (jabber-muc-joined-p "room@example.com"))))

(ert-deftest jabber-test-muc-joined-p ()
  "jabber-muc-joined-p returns t for joined rooms, nil otherwise."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (should-not (jabber-muc-joined-p "room@example.com"))
    (jabber-muc-join-set "room@example.com" nil "mynick")
    (should (jabber-muc-joined-p "room@example.com"))))

(ert-deftest jabber-test-muc-active-rooms ()
  "jabber-muc-active-rooms returns list of joined room JIDs."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (jabber-muc-join-set "room1@example.com" nil "nick1")
    (jabber-muc-join-set "room2@example.com" nil "nick2")
    (let ((rooms (jabber-muc-active-rooms)))
      (should (= (length rooms) 2))
      (should (member "room1@example.com" rooms))
      (should (member "room2@example.com" rooms)))))

(ert-deftest jabber-test-muc-generation-increments ()
  "jabber-muc-generation increments on join and leave."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--generation 0)
)
    (should (= (jabber-muc-generation) 0))
    (jabber-muc-join-set "room@example.com" nil "mynick")
    (should (= (jabber-muc-generation) 1))
    (jabber-muc-leave-remove "room@example.com")
    (should (= (jabber-muc-generation) 2))))

(ert-deftest jabber-test-muc-nickname-unknown-room ()
  "jabber-muc-nickname returns nil for unknown rooms."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (should-not (jabber-muc-nickname "unknown@example.com"))))

(ert-deftest jabber-test-muc-connection-unknown-room ()
  "jabber-muc-connection returns nil for unknown rooms."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (should-not (jabber-muc-connection "unknown@example.com"))))

;;; ---- Group 6: jabber-muc--classify-message ----

(ert-deftest jabber-test-muc-classify-message-error ()
  "Stanza with error child is classified as :muc-error."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (error ((type . "cancel"))))))
      (should (eq :muc-error
                  (jabber-muc--classify-message
                   "room@conference.example.com" "othernick" xml))))))

(ert-deftest jabber-test-muc-classify-message-local ()
  "Message from our own nick is classified as :muc-local."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/mynick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-local
                  (jabber-muc--classify-message
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-foreign ()
  "Message from another nick is classified as :muc-foreign."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message
                   "room@conference.example.com" "othernick" xml))))))

(ert-deftest jabber-test-muc-classify-message-uncached-room ()
  "Room not in jabber-muc--rooms with non-nil nick returns :muc-foreign."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message
                   "room@conference.example.com" "othernick" xml))))))

;;; ---- Group 7: jabber-muc--history-message-p ----

(ert-deftest jabber-test-muc-history-message-p-delay ()
  "Message with urn:xmpp:delay child is detected as history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (delay ((xmlns . "urn:xmpp:delay")
                       (stamp . "2023-01-01T00:00:00Z"))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-legacy-delay ()
  "Message with jabber:x:delay child is detected as history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (x ((xmlns . "jabber:x:delay")
                   (stamp . "20230101T00:00:00"))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-live ()
  "Live message without delay element is not history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Live message"))))
    (should-not (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-mixed-children ()
  "Delay is detected among mixed sibling elements."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (delay ((xmlns . "urn:xmpp:delay")
                       (stamp . "2023-01-01T00:00:00Z")))
               (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                  (status ((code . "100")))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-classify-message-error-priority ()
  "Error classification takes priority over matching local nick."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/mynick")
                          (type . "groupchat"))
               (error ((type . "cancel"))))))
      (should (eq :muc-error
                  (jabber-muc--classify-message
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-nil-nick ()
  "Nil nick (bare JID) classifies as :muc-foreign, not crash."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com")
                          (type . "groupchat"))
               (body nil "Room announcement"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message
                   "room@conference.example.com" nil xml))))))

;;; ---- Group 8: jabber-muc--format-affiliation-change ----

(ert-deftest jabber-test-muc-affiliation-promote-member-to-admin ()
  "Promoting member to admin reports promotion."
  (should (string= "alice has been promoted to admin"
                    (jabber-muc--format-affiliation-change
                     "alice" "member" "admin" ""))))

(ert-deftest jabber-test-muc-affiliation-demote-admin-to-member ()
  "Demoting admin to member reports demotion."
  (should (string= "bob has been demoted to member by op: misconduct"
                    (jabber-muc--format-affiliation-change
                     "bob" "admin" "member" " by op: misconduct"))))

(ert-deftest jabber-test-muc-affiliation-grant-membership ()
  "Granting membership from outcast reports grant."
  (should (string= "carol has been granted membership"
                    (jabber-muc--format-affiliation-change
                     "carol" "outcast" "member" ""))))

(ert-deftest jabber-test-muc-affiliation-lose-membership ()
  "Losing membership from member to none reports deprivation."
  (should (string= "dave has been deprived of membership"
                    (jabber-muc--format-affiliation-change
                     "dave" "member" "none" ""))))

(ert-deftest jabber-test-muc-affiliation-owner-to-admin ()
  "Owner demoted to admin reports demotion."
  (should (string= "frank has been demoted to admin"
                    (jabber-muc--format-affiliation-change
                     "frank" "owner" "admin" ""))))

(ert-deftest jabber-test-muc-affiliation-no-match ()
  "Unrecognized affiliation transition returns nil."
  (should-not (jabber-muc--format-affiliation-change
               "eve" "admin" "outcast" "")))

;;; ---- Group 9: jabber-muc--format-role-change ----

(ert-deftest jabber-test-muc-role-change-to-moderator ()
  "Participant promoted to moderator reports grant."
  (should (string= "alice has been granted moderator privileges"
                    (jabber-muc--format-role-change
                     "alice" "participant" "moderator" ""))))

(ert-deftest jabber-test-muc-role-change-moderator-to-participant ()
  "Moderator demoted to participant reports revocation."
  (should (string= "bob had moderator privileges revoked by admin"
                    (jabber-muc--format-role-change
                     "bob" "moderator" "participant" " by admin"))))

(ert-deftest jabber-test-muc-role-change-to-visitor ()
  "Participant changed to visitor reports denied voice."
  (should (string= "carol has been denied voice"
                    (jabber-muc--format-role-change
                     "carol" "participant" "visitor" ""))))

(ert-deftest jabber-test-muc-role-change-to-participant ()
  "Visitor granted voice reports grant."
  (should (string= "dave has been granted voice"
                    (jabber-muc--format-role-change
                     "dave" "visitor" "participant" ""))))

(ert-deftest jabber-test-muc-role-change-visitor-to-moderator ()
  "Visitor promoted to moderator reports grant."
  (should (string= "eve has been granted moderator privileges"
                    (jabber-muc--format-role-change
                     "eve" "visitor" "moderator" ""))))

;;; ---- Group 10: jabber-muc-report-delta integration ----

(ert-deftest jabber-test-muc-report-delta-new-join ()
  "Nil old-plist produces an enters-room message."
  (let ((new-plist '(role "participant" affiliation "member")))
    (should (string= "nick enters the room (participant, member)"
                      (jabber-muc-report-delta "nick" nil new-plist nil nil)))))

(ert-deftest jabber-test-muc-report-delta-no-change ()
  "Same affiliation and role returns nil."
  (let ((old '(role "participant" affiliation "member"))
        (new '(role "participant" affiliation "member")))
    (should-not (jabber-muc-report-delta "nick" old new nil nil))))

(ert-deftest jabber-test-muc-report-delta-affiliation-change ()
  "Affiliation change delegates to affiliation helper."
  (let ((old '(role "participant" affiliation "member"))
        (new '(role "participant" affiliation "admin")))
    (should (string= "nick has been promoted to admin"
                      (jabber-muc-report-delta "nick" old new nil nil)))))

(ert-deftest jabber-test-muc-report-delta-role-change ()
  "Role change delegates to role helper."
  (let ((old '(role "participant" affiliation "member"))
        (new '(role "moderator" affiliation "member")))
    (should (string= "nick has been granted moderator privileges"
                      (jabber-muc-report-delta "nick" old new nil nil)))))

(provide 'jabber-muc-tests)
;;; jabber-muc-tests.el ends here
