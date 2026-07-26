;;; jabber-test-muc.el --- Tests for jabber-muc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Multi-user chat rooms.

;;; Code:

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time
;; from jabber-core.el and jabber-chat.el:
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-chatbuffer)
(require 'jabber-muc)
(require 'jabber-muc-nick-completion)

(defmacro jabber-test-muc-with-rooms (rooms &rest body)
  "Run BODY with ROOMS as active groupchats.
ROOMS is an alist of (group . nickname).  Each room gets a single
entry with JC=nil."
  (declare (indent 1))
  `(let ((jabber-muc--rooms (make-hash-table :test #'equal)))
     (dolist (r ,rooms)
       (puthash (car r) (list (cons nil (cdr r))) jabber-muc--rooms))
     ,@body))

;;; Group 1: jabber-muc-message-p

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

;;; Group 2: jabber-muc-sender-p

(ert-deftest jabber-test-muc-sender-p-full-jid ()
  "Full JID from active groupchat is a MUC sender."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (should (jabber-muc-sender-p "room@conference.example.com/othernick"))))

(ert-deftest jabber-test-muc-sender-p-bare-jid ()
  "Bare JID (no resource) is not a MUC sender."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (should-not (jabber-muc-sender-p "room@conference.example.com"))))

(ert-deftest jabber-test-muc-sender-p-not-active ()
  "JID not in active groupchats is not a MUC sender."
  (jabber-test-muc-with-rooms nil
    (should-not (jabber-muc-sender-p "room@conference.example.com/nick"))))

;;; Group 3: jabber-muc-private-message-p

(ert-deftest jabber-test-muc-private-message-p-private ()
  "Private message from MUC participant returns non-nil."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((msg '(message ((from . "room@conference.example.com/othernick")
                          (type . "chat"))
                 (body nil "Psst"))))
      (should (jabber-muc-private-message-p msg)))))

(ert-deftest jabber-test-muc-private-message-p-groupchat ()
  "Groupchat type message is not a private message."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((msg '(message ((from . "room@conference.example.com/nick")
                          (type . "groupchat"))
                 (body nil "Hello all"))))
      (should-not (jabber-muc-private-message-p msg)))))

;;; Group 4: jabber-muc-presence-p

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

;;; Group 5: jabber-muc accessor functions

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

;;; Group 6: jabber-muc--classify-message

(ert-deftest jabber-test-muc-classify-message-error ()
  "Stanza with error child is classified as :muc-error."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (error ((type . "cancel"))))))
      (should (eq :muc-error
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "othernick" xml))))))

(ert-deftest jabber-test-muc-classify-message-local ()
  "Message from our own nick is classified as :muc-local."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/mynick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-local
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-foreign ()
  "Message from another nick is classified as :muc-foreign."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "othernick" xml))))))

(ert-deftest jabber-test-muc-classify-message-uncached-room ()
  "Room not in jabber-muc--rooms with non-nil nick returns :muc-foreign."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "othernick" xml))))))

;;; Group 7: jabber-muc--history-message-p

(ert-deftest jabber-test-muc-history-message-p-delay ()
  "Delay from=room is detected as MUC history per XEP-0045."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (delay ((xmlns . "urn:xmpp:delay")
                       (from . "room@conference.example.com")
                       (stamp . "2023-01-01T00:00:00Z"))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-legacy-delay ()
  "Legacy jabber:x:delay from=room is detected as MUC history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (x ((xmlns . "jabber:x:delay")
                   (from . "room@conference.example.com")
                   (stamp . "20230101T00:00:00"))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-live ()
  "Live message without delay element is not history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Live message"))))
    (should-not (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-mixed-children ()
  "Delay from=room among mixed sibling elements is detected."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Old message")
               (delay ((xmlns . "urn:xmpp:delay")
                       (from . "room@conference.example.com")
                       (stamp . "2023-01-01T00:00:00Z")))
               (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                  (status ((code . "100")))))))
    (should (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-bridge-delay ()
  "Delay from a bridge gateway is not history (Matrix/slidge scenario)."
  (let ((xml '(message ((from . "!room@matrix.example.com/nick")
                        (type . "groupchat"))
               (body nil "Live bridged message")
               (delay ((xmlns . "urn:xmpp:delay")
                       (from . "matrix.example.com")
                       (stamp . "2026-04-06T06:09:55Z"))))))
    (should-not (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-history-message-p-delay-no-from ()
  "Delay without from attribute is not treated as history."
  (let ((xml '(message ((from . "room@conference.example.com/nick")
                        (type . "groupchat"))
               (body nil "Message with anonymous delay")
               (delay ((xmlns . "urn:xmpp:delay")
                       (stamp . "2023-01-01T00:00:00Z"))))))
    (should-not (jabber-muc--history-message-p xml))))

(ert-deftest jabber-test-muc-classify-message-error-priority ()
  "Error classification takes priority over matching local nick."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/mynick")
                          (type . "groupchat"))
               (error ((type . "cancel"))))))
      (should (eq :muc-error
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-nil-nick ()
  "Nil nick (bare JID) classifies as :muc-foreign, not crash."
  (jabber-test-muc-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com")
                          (type . "groupchat"))
               (body nil "Room announcement"))))
      (should (eq :muc-foreign
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" nil xml))))))

;;; Group 8: jabber-muc--format-affiliation-change

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

;;; Group 9: jabber-muc--format-role-change

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

;;; Group 10: jabber-muc-report-delta integration

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

;;; Group 11: jabber-muc-create

(ert-deftest jabber-test-muc-create-sets-auto-configure ()
  "jabber-muc-create sends join presence with auto-configure."
  (let ((join-args nil))
    (cl-letf (((symbol-function 'jabber-muc--send-join-presence)
               (lambda (jc group nickname password popup &optional auto-configure)
                 (setq join-args
                       (list jc group nickname password popup auto-configure))))
              ((symbol-function 'jabber-bookmarks--publish-one)
               #'ignore))
      (jabber-muc-create 'fake-jc "room@conference.example.com" "mynick"))
    (should join-args)
    ;; auto-configure (6th) should be t
    (should (nth 5 join-args))
    ;; popup (5th) should be t
    (should (nth 4 join-args))))

(ert-deftest jabber-test-muc-auto-configure-opens-config ()
  "Status 201 with auto-configure flag calls jabber-muc-get-config."
  (let ((config-called nil)
        (jabber-buffer-connection 'fake-jc)
        (jabber-group "room@conference.example.com")
        (jabber-muc--auto-configure t)
        (jabber-chat-ewoc nil))
    (cl-letf (((symbol-function 'jabber-muc-get-config)
               (lambda (jc group)
                 (setq config-called (cons jc group)))))
      (jabber-muc--enter-extra-notices
       "mynick" (list jabber-muc-status-room-created)))
    (should (equal config-called '(fake-jc . "room@conference.example.com")))
    (should-not jabber-muc--auto-configure)))

(ert-deftest jabber-test-muc-auto-configure-off-shows-notice ()
  "Status 201 without auto-configure flag inserts ewoc notice."
  (let ((notice-entered nil)
        (jabber-muc--auto-configure nil)
        (jabber-chat-ewoc 'fake-ewoc))
    (cl-letf (((symbol-function 'jabber-chat-ewoc-enter)
               (lambda (data)
                 (setq notice-entered data)))
              ((symbol-function 'jabber-muc--room-created-message)
               (lambda () "room created message")))
      (jabber-muc--enter-extra-notices
       "mynick" (list jabber-muc-status-room-created)))
    (should notice-entered)
    (should (eq :muc-notice (car notice-entered)))))

;;; Group 12: jabber-muc--validate-disco-result

(ert-deftest jabber-test-muc-validate-disco-ok ()
  "Conference identity returns :ok status with features."
  (let* ((identities (vector "Room" "conference" "text"))
         (features '("http://jabber.org/protocol/muc" "muc_open"))
         (result (list (list identities) features)))
    (let ((v (jabber-muc--validate-disco-result result)))
      (should (eq 'ok (plist-get v :status)))
      (should (equal features (plist-get v :features))))))

(ert-deftest jabber-test-muc-validate-disco-not-found ()
  "Item-not-found error returns :not-found status."
  (let ((result '(error ((type . "cancel"))
                  (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
    (cl-letf (((symbol-function 'jabber-error-condition)
               (lambda (_r) 'item-not-found)))
      (let ((v (jabber-muc--validate-disco-result result)))
        (should (eq 'not-found (plist-get v :status)))))))

(ert-deftest jabber-test-muc-validate-disco-not-conference ()
  "Non-conference identity returns :not-conference status."
  (let* ((identities (vector "Gateway" "gateway" "xmpp"))
         (result (list (list identities) '("some-feature"))))
    (let ((v (jabber-muc--validate-disco-result result)))
      (should (eq 'not-conference (plist-get v :status))))))

(ert-deftest jabber-test-muc-validate-disco-error ()
  "Generic error returns :error status with message."
  (let ((result '(error ((type . "cancel"))
                  (forbidden ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
    (cl-letf (((symbol-function 'jabber-error-condition)
               (lambda (_r) 'forbidden))
              ((symbol-function 'jabber-parse-error)
               (lambda (_r) "Forbidden")))
      (let ((v (jabber-muc--validate-disco-result result)))
        (should (eq 'error (plist-get v :status)))
        (should (string= "Forbidden" (plist-get v :error-msg)))))))

(ert-deftest jabber-test-muc-validate-disco-no-disco ()
  "Feature-not-implemented returns :no-disco status."
  (let ((result '(error ((type . "cancel"))
                  (feature-not-implemented
                   ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
    (cl-letf (((symbol-function 'jabber-error-condition)
               (lambda (_r) 'feature-not-implemented)))
      (let ((v (jabber-muc--validate-disco-result result)))
        (should (eq 'no-disco (plist-get v :status)))))))

;;; Group 13: jabber-muc-create skips disco

(ert-deftest jabber-test-muc-create-skips-disco ()
  "jabber-muc-create sends join presence directly without disco."
  (let ((join-called nil)
        (disco-called nil))
    (cl-letf (((symbol-function 'jabber-muc--send-join-presence)
               (lambda (&rest _args) (setq join-called t)))
              ((symbol-function 'jabber-disco-get-info)
               (lambda (&rest _args) (setq disco-called t)))
              ((symbol-function 'jabber-bookmarks--publish-one)
               #'ignore))
      (jabber-muc-create 'fake-jc "room@conference.example.com" "mynick"))
    (should join-called)
    (should-not disco-called)))

;;; Group 14: OMEMO session prefetch on participant join

(defun jabber-muc-test--make-fake-jc ()
  "Return a fake connection object for testing."
  'fake-jc)

(ert-deftest jabber-muc-test-omemo-prefetch-on-participant-join ()
  "OMEMO sessions are prefetched when a new participant with a real JID joins."
  (let* ((jc (jabber-muc-test--make-fake-jc))
         (group "room@conf.example.com")
         (prefetch-calls nil)
         (x-muc '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (item ((affiliation . "member")
                            (role . "participant")
                            (jid . "alice@example.com/res")))))
         (buf (generate-new-buffer " *test-muc-omemo*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local jabber-chat-encryption 'omemo))
          (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                     (lambda (_group &optional _jc) buf))
                    ((symbol-function 'jabber-omemo--prefetch-sessions)
                     (lambda (_ jid) (push jid prefetch-calls)))
                    ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-modify-participant) #'ignore)
                    ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-create-buffer) (lambda (&rest _) buf))
                    ((symbol-function 'jabber-maybe-print-rare-time) #'ignore)
                    ((symbol-function 'jabber-chat-ewoc-enter) #'ignore))
            (jabber-muc--process-enter
             jc group "alice"
             (jabber-jid-symbol "room@conf.example.com/alice")
             nil x-muc nil nil "me")
            (should (member "alice@example.com" prefetch-calls))))
      (kill-buffer buf))))

(ert-deftest jabber-muc-test-no-omemo-prefetch-when-plaintext ()
  "No OMEMO prefetch when the buffer uses plaintext encryption."
  (let* ((jc (jabber-muc-test--make-fake-jc))
         (group "room@conf.example.com")
         (prefetch-calls nil)
         (x-muc '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (item ((affiliation . "member")
                            (role . "participant")
                            (jid . "alice@example.com/res")))))
         (buf (generate-new-buffer " *test-muc-plain*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local jabber-chat-encryption 'plaintext))
          (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                     (lambda (_group &optional _jc) buf))
                    ((symbol-function 'jabber-omemo--prefetch-sessions)
                     (lambda (_ jid) (push jid prefetch-calls)))
                    ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-modify-participant) #'ignore)
                    ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-create-buffer) (lambda (&rest _) buf))
                    ((symbol-function 'jabber-maybe-print-rare-time) #'ignore)
                    ((symbol-function 'jabber-chat-ewoc-enter) #'ignore))
            (jabber-muc--process-enter
             jc group "alice"
             (jabber-jid-symbol "room@conf.example.com/alice")
             nil x-muc nil nil "me")
            (should (null prefetch-calls))))
      (kill-buffer buf))))

(ert-deftest jabber-muc-test-no-omemo-prefetch-for-self ()
  "OMEMO prefetch is not triggered for self-presence."
  (let* ((jc (jabber-muc-test--make-fake-jc))
         (group "room@conf.example.com")
         (prefetch-calls nil)
         (x-muc '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (item ((affiliation . "member")
                            (role . "participant")
                            (jid . "me@example.com/res")))))
         (buf (generate-new-buffer " *test-muc-self*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local jabber-chat-encryption 'omemo))
          (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                     (lambda (_group &optional _jc) buf))
                    ((symbol-function 'jabber-omemo--prefetch-sessions)
                     (lambda (_ jid) (push jid prefetch-calls)))
                    ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-modify-participant) #'ignore)
                    ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-create-buffer) (lambda (&rest _) buf))
                    ((symbol-function 'jabber-maybe-print-rare-time) #'ignore)
                    ((symbol-function 'jabber-chat-ewoc-enter) #'ignore)
                    ((symbol-function 'jabber-muc-add-groupchat) #'ignore)
                    ((symbol-function 'jabber-mam-muc-joined) #'ignore)
                    ((symbol-function 'jabber-bookmarks-auto-add-maybe) #'ignore))
            ;; "me" is self — status code 110 marks self-presence
            (jabber-muc--process-enter
             jc group "me"
             (jabber-jid-symbol "room@conf.example.com/me")
             (list jabber-muc-status-self-presence) x-muc nil nil "me")
            (should (null prefetch-calls))))
      (kill-buffer buf))))

(ert-deftest jabber-muc-test-no-omemo-prefetch-without-real-jid ()
  "No OMEMO prefetch when participant has no real JID (anonymous room)."
  (let* ((jc (jabber-muc-test--make-fake-jc))
         (group "room@conf.example.com")
         (prefetch-calls nil)
         ;; No jid attribute in item
         (x-muc '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
                     (item ((affiliation . "member") (role . "participant")))))
         (buf (generate-new-buffer " *test-muc-anon*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local jabber-chat-encryption 'omemo))
          (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                     (lambda (_group &optional _jc) buf))
                    ((symbol-function 'jabber-omemo--prefetch-sessions)
                     (lambda (_ jid) (push jid prefetch-calls)))
                    ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-modify-participant) #'ignore)
                    ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
                    ((symbol-function 'jabber-muc-create-buffer) (lambda (&rest _) buf))
                    ((symbol-function 'jabber-maybe-print-rare-time) #'ignore)
                    ((symbol-function 'jabber-chat-ewoc-enter) #'ignore))
            (jabber-muc--process-enter
             jc group "bob"
             (jabber-jid-symbol "room@conf.example.com/bob")
             nil x-muc nil nil "me")
            (should (null prefetch-calls))))
      (kill-buffer buf))))

;;; Group 15: jabber-muc--merge-plist

(ert-deftest jabber-test-muc-merge-plist-preserves-old-keys ()
  "Old keys not present in new plist are preserved."
  (let ((result (jabber-muc--merge-plist
                 '(jid "alice@example.com" role "participant")
                 '(role "moderator"))))
    (should (string= "alice@example.com" (plist-get result 'jid)))
    (should (string= "moderator" (plist-get result 'role)))))

(ert-deftest jabber-test-muc-merge-plist-overwrites-shared-keys ()
  "New values win on conflict."
  (let ((result (jabber-muc--merge-plist
                 '(role "participant" affiliation "member")
                 '(role "moderator" affiliation "admin"))))
    (should (string= "moderator" (plist-get result 'role)))
    (should (string= "admin" (plist-get result 'affiliation)))))

(ert-deftest jabber-test-muc-merge-plist-empty-old ()
  "Nil old plist returns new plist unchanged."
  (let ((result (jabber-muc--merge-plist nil '(role "participant"))))
    (should (string= "participant" (plist-get result 'role)))))

(ert-deftest jabber-test-muc-modify-participant-preserves-jid ()
  "Presence update without jid keeps the previously known jid."
  (let ((jabber-muc-participants nil))
    ;; Initial presence with full info including jid
    (jabber-muc-modify-participant "room@conf.example.com" "alice"
                                  '(role "participant" affiliation "member"
                                    jid "alice@example.com/res"))
    ;; Subsequent presence (e.g. role change) without jid attribute
    (jabber-muc-modify-participant "room@conf.example.com" "alice"
                                  '(role "moderator" affiliation "member"))
    (let ((plist (jabber-muc-participant-plist "room@conf.example.com" "alice")))
      (should (string= "moderator" (plist-get plist 'role)))
      (should (string= "alice@example.com/res" (plist-get plist 'jid))))))

;;; Group 16: XEP-0249 direct MUC invitations

(ert-deftest jabber-test-muc-message-p-direct-invite ()
  "XEP-0249 direct invite stanza is detected as a MUC message."
  (let ((jabber-pending-groupchats (make-hash-table))
        (jabber-jid-obarray (make-vector 127 0)))
    (let ((msg '(message ((from . "alice@example.com/home"))
               (x ((xmlns . "jabber:x:conference")
                   (jid . "room@conference.example.com"))))))
      (should (jabber-muc-message-p msg)))))

(ert-deftest jabber-test-muc-print-invite-direct ()
  "Direct invite renders with correct group and inviter."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (with-temp-buffer
      (let ((msg (list :xml-data
                       '(message ((from . "alice@example.com/home"))
                         (x ((xmlns . "jabber:x:conference")
                             (jid . "room@conference.example.com")))))))
        (jabber-muc-print-invite msg nil :insert)
        (let ((text (buffer-string)))
          (should (string-match-p "room@conference.example.com" text))
          (should (string-match-p "alice@example.com" text))
          (should (string-match-p "Accept" text)))))))

(ert-deftest jabber-test-muc-print-invite-direct-with-reason ()
  "Direct invite with reason attribute displays the reason."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal)))
    (with-temp-buffer
      (let ((msg (list :xml-data
                       '(message ((from . "alice@example.com/home"))
                         (x ((xmlns . "jabber:x:conference")
                             (jid . "room@conference.example.com")
                             (reason . "Join our discussion")))))))
        (jabber-muc-print-invite msg nil :insert)
        (let ((text (buffer-string)))
          (should (string-match-p "Join our discussion" text)))))))

;;; Group 17: disco-prioritized autojoin queue

(ert-deftest jabber-muc-test-autojoin-insert-sorted ()
  "Rooms are inserted in ascending order by occupant count."
  (let ((jabber-muc--autojoin-queue nil))
    (jabber-muc--autojoin-insert 'jc1 50 "big@muc" "nick1")
    (jabber-muc--autojoin-insert 'jc1 5 "small@muc" "nick2")
    (jabber-muc--autojoin-insert 'jc1 20 "mid@muc" "nick3")
    (let ((rooms (cdr (assq 'jc1 jabber-muc--autojoin-queue))))
      (should (= (length rooms) 3))
      ;; Sorted: 5, 20, 50
      (should (= (caar rooms) 5))
      (should (string= (cadar rooms) "small@muc"))
      (should (= (caadr rooms) 20))
      (should (= (caaddr rooms) 50)))))

(ert-deftest jabber-muc-test-autojoin-insert-and-next ()
  "Inserting rooms and popping them drains in count order."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer nil)
        (joined nil))
    (cl-letf (((symbol-function 'jabber-muc--send-join-presence)
               (lambda (_jc group nick _pw _popup)
                 (push (cons group nick) joined)))
              ((symbol-function 'jabber-get-conference-data)
               (lambda (&rest _) nil)))
      (jabber-muc--autojoin-insert 'jc1 100 "big@muc" "nick1")
      (jabber-muc--autojoin-insert 'jc1 3 "tiny@muc" "nick2")
      (jabber-muc--autojoin-insert 'jc1 30 "mid@muc" "nick3")
      ;; Pop first: smallest count
      (jabber-muc--autojoin-next 'jc1)
      (should (equal (car joined) '("tiny@muc" . "nick2")))
      ;; Pop second
      (jabber-muc--autojoin-next 'jc1)
      (should (equal (car joined) '("mid@muc" . "nick3")))
      ;; Pop third (last)
      (jabber-muc--autojoin-next 'jc1)
      (should (equal (car joined) '("big@muc" . "nick1")))
      ;; Queue entry removed
      (should-not (assq 'jc1 jabber-muc--autojoin-queue))
      ;; Extra pop is a no-op
      (let ((count (length joined)))
        (jabber-muc--autojoin-next 'jc1)
        (should (= (length joined) count))))))

(ert-deftest jabber-muc-test-autojoin-disco-callback-success ()
  "Disco callback inserts room with occupant count."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer nil)
        (jabber-muc--autojoin-disco-count nil))
    ;; Simulate disco result with 3 occupants
    (jabber-muc--autojoin-disco-callback
     'jc1 '("room@muc" . "nick1")
     '(["alice" "room@muc/alice" nil]
       ["bob" "room@muc/bob" nil]
       ["carol" "room@muc/carol" nil]))
    ;; Room should be in queue with count 3
    (should (jabber-muc--autojoin-queued-p 'jc1 "room@muc"))
    (let ((entry (car (cdr (assq 'jc1 jabber-muc--autojoin-queue)))))
      (should (= (car entry) 3)))))

(ert-deftest jabber-muc-test-autojoin-disco-callback-error ()
  "Disco error inserts room with most-positive-fixnum count."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer nil))
    ;; First insert a small room
    (jabber-muc--autojoin-insert 'jc1 2 "small@muc" "nick2")
    ;; Then disco error arrives for another room
    (cl-letf (((symbol-function 'jabber-muc--send-join-presence) #'ignore)
              ((symbol-function 'jabber-get-conference-data)
               (lambda (&rest _) nil)))
      (jabber-muc--autojoin-disco-callback
       'jc1 '("broken@muc" . "nick1")
       '(error ((type . "cancel")))))
    ;; Error room should be last (count = most-positive-fixnum)
    (let ((rooms (cdr (assq 'jc1 jabber-muc--autojoin-queue))))
      ;; After drain started, small@muc was popped, so only broken@muc remains
      ;; (or both if drain didn't fire because timer was set)
      (when rooms
        (should (= (caar (last rooms)) most-positive-fixnum))))))

(ert-deftest jabber-muc-test-autojoin-dequeue ()
  "Dequeue removes a specific room from the queue."
  (let ((jabber-muc--autojoin-queue nil))
    (jabber-muc--autojoin-insert 'jc1 5 "r1@muc" "n1")
    (jabber-muc--autojoin-insert 'jc1 10 "r2@muc" "n2")
    (jabber-muc--autojoin-insert 'jc1 15 "r3@muc" "n3")
    (jabber-muc--autojoin-dequeue 'jc1 "r2@muc")
    (let ((rooms (cdr (assq 'jc1 jabber-muc--autojoin-queue))))
      (should (= (length rooms) 2))
      (should-not (cl-find "r2@muc" rooms :key #'cadr :test #'string=)))))

(ert-deftest jabber-muc-test-autojoin-dequeue-last-cleans-entry ()
  "Dequeuing the last room removes the connection entry entirely."
  (let ((jabber-muc--autojoin-queue nil))
    (jabber-muc--autojoin-insert 'jc1 5 "r1@muc" "n1")
    (jabber-muc--autojoin-dequeue 'jc1 "r1@muc")
    (should-not (assq 'jc1 jabber-muc--autojoin-queue))))

(ert-deftest jabber-muc-test-autojoin-clear ()
  "Clearing the queue removes all entries for a connection."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer nil))
    (jabber-muc--autojoin-insert 'jc1 5 "r1@muc" "n1")
    (jabber-muc--autojoin-insert 'jc2 5 "r2@muc" "n2")
    (jabber-muc--autojoin-clear 'jc1)
    (should-not (assq 'jc1 jabber-muc--autojoin-queue))
    ;; Other connection unaffected
    (should (assq 'jc2 jabber-muc--autojoin-queue))))

(ert-deftest jabber-muc-test-autojoin-queued-p ()
  "Check if a room is already in the autojoin queue."
  (let ((jabber-muc--autojoin-queue nil))
    (jabber-muc--autojoin-insert 'jc1 5 "r1@muc" "n1")
    (should (jabber-muc--autojoin-queued-p 'jc1 "r1@muc"))
    (should-not (jabber-muc--autojoin-queued-p 'jc1 "r2@muc"))
    (should-not (jabber-muc--autojoin-queued-p 'jc2 "r1@muc"))))

(ert-deftest jabber-muc-test-autojoin-next-empty-is-noop ()
  "Calling next with no queue entries does nothing."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer nil)
        (joined nil))
    (cl-letf (((symbol-function 'jabber-muc--send-join-presence)
               (lambda (&rest _) (push t joined))))
      (jabber-muc--autojoin-next 'jc1)
      (should (null joined)))))

(ert-deftest jabber-muc-test-autojoin-disco-no-drain-while-inflight ()
  "Disco callback does not start drain when a join is in-flight."
  (let ((jabber-muc--autojoin-queue nil)
        (jabber-muc--autojoin-timer 'fake-timer)
        (next-called nil))
    (cl-letf (((symbol-function 'jabber-muc--autojoin-next)
               (lambda (_jc) (setq next-called t))))
      (jabber-muc--autojoin-disco-callback
       'jc1 '("room@muc" . "nick1")
       '(["alice" "room@muc/alice" nil]))
      ;; Should NOT have called next because timer was set (join in-flight)
      (should-not next-called)
      ;; But the room should be in the queue
      (should (jabber-muc--autojoin-queued-p 'jc1 "room@muc")))))

;;; Group 20: Participant list UI

(ert-deftest jabber-muc-test-names-revert-refreshes-participants ()
  "Reverting a MUC names buffer refreshes participant entries."
  (let ((jabber-muc-participants
         '(("room@muc"
            ("alice" role "participant" affiliation "member"
             jid "alice@example.com/res")))))
    (with-temp-buffer
      (jabber-muc-names-mode)
      (setq jabber-muc-names--group "room@muc")
      (jabber-muc-names--refresh)
      (should (equal ["alice" "participant" "member" "alice@example.com/res"]
                     (cadar tabulated-list-entries)))
      (setq jabber-muc-participants
            '(("room@muc"
               ("alice" role "moderator" affiliation "admin"
                jid "alice@example.com/res")
               ("bob" role "visitor" affiliation "none"
                jid "bob@example.com/res"))))
      (revert-buffer nil t)
      (should (= 2 (length tabulated-list-entries)))
      (should (equal ["alice" "moderator" "admin" "alice@example.com/res"]
                     (cadar tabulated-list-entries)))
      (should (equal ["bob" "visitor" "none" "bob@example.com/res"]
                     (cadr (cadr tabulated-list-entries)))))))

;;; Group 21: MUC status-code notices

(ert-deftest jabber-test-muc-status-notices-privacy-and-logging ()
  "Privacy and logging status codes produce user-visible notices."
  (should (equal
           '("This room exposes your real JID to other occupants"
             "This room is publicly logged"
             "This room is no longer publicly logged"
             "This room is now non-anonymous"
             "This room is now semi-anonymous")
           (jabber-muc--status-notices
            (list jabber-muc-status-nonanonymous
                  jabber-muc-status-logging-enabled
                  jabber-muc-status-logging-disabled
                  jabber-muc-status-now-nonanonymous
                  jabber-muc-status-now-semianonymous)))))

(ert-deftest jabber-test-muc-enter-extra-notices-inserts-status-notices ()
  "Entering a room inserts notices for privacy and logging status codes."
  (let (notices)
    (cl-letf (((symbol-function 'jabber-muc--insert-notice)
               (lambda (notice)
                 (push notice notices))))
      (jabber-muc--enter-extra-notices
       "me"
       (list jabber-muc-status-nonanonymous
             jabber-muc-status-logging-enabled
             jabber-muc-status-now-nonanonymous))
      (should (equal
               '("This room exposes your real JID to other occupants"
                 "This room is publicly logged"
                 "This room is now non-anonymous")
               (nreverse notices))))))

(ert-deftest jabber-test-muc-buffer-registry-is-account-scoped ()
  "Two accounts may hold distinct buffers for the same room."
  (let ((jabber-buffer-registry--buffers
         (make-hash-table :test #'equal))
        (a (generate-new-buffer " *muc-account-a*"))
        (b (generate-new-buffer " *muc-account-b*"))
        (group "room@conference.example.com"))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (jc)
                     (if (eq jc 'jc-a) "a@example.com" "b@example.com"))))
          (with-current-buffer a
            (setq-local jabber-group group)
            (jabber-buffer-registry-register
             'muc (jabber-muc--buffer-key 'jc-a group)))
          (with-current-buffer b
            (setq-local jabber-group group)
            (jabber-buffer-registry-register
             'muc (jabber-muc--buffer-key 'jc-b group)))
          (should (eq a (jabber-muc-find-buffer group 'jc-a)))
          (should (eq b (jabber-muc-find-buffer group 'jc-b)))
          (should-not (jabber-muc-find-buffer group)))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest jabber-muc-test-process-enter-schedules-next ()
  "Self-presence in process-enter schedules autojoin-next via timer."
  (let* ((jabber-muc--autojoin-queue nil)
         (jabber-muc--autojoin-timer nil)
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-muc--generation 0)
         (jabber-pending-groupchats (make-hash-table))
         (jabber-jid-obarray (make-vector 127 0))
         (timer-scheduled nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_secs _repeat fn &rest _args)
                 (when (eq fn #'jabber-muc--autojoin-next)
                   (setq timer-scheduled t))
                 'fake-timer))
              ((symbol-function 'jabber-mam-muc-joined) #'ignore)
              ((symbol-function 'jabber-bookmarks-auto-add-maybe) #'ignore)
              ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc-modify-participant) #'ignore)
              ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc-find-buffer)
               (lambda (_group &optional _jc) nil)))
      (jabber-muc--process-enter
       'fake-jc "room@muc" "me"
       (jabber-jid-symbol "room@muc/me")
       (list jabber-muc-status-self-presence)
       '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
           (item ((affiliation . "member") (role . "participant"))))
       nil nil "me"))
    (should timer-scheduled)))

;;; Group 22: MUC nick completion sorting

(ert-deftest jabber-test-muc-sort-nicks-default-delimiter ()
  "Nick sorting honors activity with the default completion delimiter."
  (let ((jabber-muc-completion-delimiter ": ")
        (jabber-muc-participant-last-speaking
         '(("room@muc" ("zoe" . 20) ("alice" . 10)))))
    (should
     (equal '("zoe: " "alice: " "bob: ")
            (jabber-sort-nicks '("alice: " "zoe: " "bob: ") "room@muc")))))

(ert-deftest jabber-test-muc-sort-nicks-custom-delimiters ()
  "Nick sorting honors activity with shorter and longer delimiters."
  (let ((jabber-muc-participant-last-speaking
         '(("room@muc" ("zoe" . 20) ("alice" . 10)))))
    (dolist (delimiter '(">" " -> "))
      (let ((jabber-muc-completion-delimiter delimiter))
        (should
         (equal (mapcar (lambda (nick) (concat nick delimiter))
                        '("zoe" "alice" "bob"))
                (jabber-sort-nicks
                 (mapcar (lambda (nick) (concat nick delimiter))
                         '("alice" "zoe" "bob"))
                 "room@muc")))))))

;;; Group 23: MUC nickname faces

(defun jabber-test-muc--relative-luminance (color)
  "Return relative luminance for COLOR."
  (let ((linear
         (lambda (component)
           (if (<= component 0.04045)
               (/ component 12.92)
             (expt (/ (+ component 0.055) 1.055) 2.4)))))
    (pcase-let ((`(,red ,green ,blue)
                 (let ((values (color-values-from-color-spec color)))
                   (if values
                       (mapcar (lambda (component) (/ component 65535.0))
                               values)
                     (color-name-to-rgb color)))))
      (+ (* 0.2126 (funcall linear red))
         (* 0.7152 (funcall linear green))
         (* 0.0722 (funcall linear blue))))))

(defun jabber-test-muc--contrast-ratio (first second)
  "Return the contrast ratio between FIRST and SECOND."
  (let ((a (jabber-test-muc--relative-luminance first))
        (b (jabber-test-muc--relative-luminance second)))
    (/ (+ (max a b) 0.05)
       (+ (min a b) 0.05))))

(defun jabber-test-muc--hue-distance (first second)
  "Return the circular distance between hue angles FIRST and SECOND."
  (min (mod (- first second) 360.0)
       (mod (- second first) 360.0)))

(ert-deftest jabber-test-muc-nick-colors-follow-theme-changes ()
  "Theme changes refresh direct nickname faces."
  (should (memq #'jabber-muc--refresh-nick-faces
                enable-theme-functions))
  (should (memq #'jabber-muc--refresh-nick-faces
                disable-theme-functions)))

(ert-deftest jabber-test-muc-refresh-nick-faces-refreshes-muc-ewoc ()
  "Theme changes redisplay existing MUC messages."
  (with-temp-buffer
    (setq major-mode 'jabber-chat-mode)
    (setq-local jabber-group "room@conference.example.com")
    (setq-local jabber-chat-ewoc 'ewoc)
    (let (refreshed)
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list (current-buffer))))
                ((symbol-function 'ewoc-refresh)
                 (lambda (ewoc) (setq refreshed ewoc))))
        (jabber-muc--refresh-nick-faces)
        (should (eq 'ewoc refreshed))))))

(ert-deftest jabber-test-muc-refresh-nick-faces-preserves-history-view ()
  "Theme changes preserve a MUC window reading history."
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq major-mode 'jabber-chat-mode)
      (setq-local jabber-group "room@conference.example.com")
      (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
      (setq-local jabber-chat-ewoc
                  (ewoc-create
                   (lambda (data)
                     (insert (plist-get (cadr data) :body) "\n"))))
      (dotimes (index 100)
        (ewoc-enter-last
         jabber-chat-ewoc
         (list :muc-foreign
               (list :server-id (format "message-%03d" index)
                     :body (format "Message %03d" index)))))
      (let* ((window (selected-window))
             (anchor-id "message-040")
             (anchor (jabber-chat-ewoc-find-by-id anchor-id))
             (position (ewoc-location anchor)))
        (set-window-start window position)
        (set-window-point window position)
        (redisplay t)
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (list (current-buffer)))))
          (jabber-muc--refresh-nick-faces))
        (setq anchor (jabber-chat-ewoc-find-by-id anchor-id)
              position (ewoc-location anchor))
        (should (= position (window-start window)))
        (should (= position (window-point window)))))))

(ert-deftest jabber-test-muc-nick-hue-matches-xep-0392 ()
  "Nickname hues match the XEP-0392 test vectors."
  (dolist (entry '(("Romeo" . 327.255249)
                   ("juliet@capulet.lit" . 209.410400)
                   ("😺" . 331.199341)
                   ("council" . 359.994507)
                   ("Board" . 171.430664)))
    (should (< (abs (- (jabber-muc--nick-hue (car entry))
                       (cdr entry)))
               0.0001))))

(ert-deftest jabber-test-muc-display-hue-stays-near-xep-hue ()
  "Display hue stays within eighteen degrees of the XEP hue."
  (dolist (nick '("wanderer000" "wanderer017" "wanderer178"
                  "wanderer001" "wanderer209" "willow205"))
    (pcase-let ((`(,base ,display . ,_rest)
                 (jabber-muc--nick-color-components nick)))
      (should (<= (min (mod (- base display) 360.0)
                       (mod (- display base) 360.0))
                  18.0)))))

(ert-deftest jabber-test-muc-nick-color-components-are-stable ()
  "Hash-derived nickname color components remain stable."
  (cl-mapc
   (lambda (actual expected)
     (should (< (abs (- actual expected)) 0.000001)))
   (jabber-muc--nick-color-components "wanderer000")
   '(21.5386962890625 36.36425671633333
     96.86274509803921 0.01568627450980392)))

(ert-deftest jabber-test-muc-color-matches-xep-0392 ()
  "Nickname colors match the XEP-0392 RGB test vectors."
  (dolist (entry '(("Romeo" . (0.865 0.000 0.686))
                   ("juliet@capulet.lit" . (0.000 0.515 0.573))
                   ("😺" . (0.872 0.000 0.659))
                   ("council" . (0.918 0.000 0.394))
                   ("Board" . (0.000 0.527 0.457))))
    (cl-mapc (lambda (actual expected)
               (should (< (abs (- actual expected)) 0.001)))
             (jabber-muc--hsluv-rgb
              (jabber-muc--nick-hue (car entry)) 100 50)
             (cdr entry))))

(ert-deftest jabber-test-muc-hsluv-matches-reference-vectors ()
  "HSLuv conversion handles varied saturation and lightness."
  (dolist (entry '(((76.4373779 75 70) . (0.7311863 0.6755247 0.3317136))
                   ((75.8715820 60 80) . (0.8374650 0.7814542 0.4900905))))
    (cl-mapc (lambda (actual expected)
               (should (< (abs (- actual expected)) 0.0001)))
             (apply #'jabber-muc--hsluv-rgb (car entry))
             (cdr entry))))

(ert-deftest jabber-test-muc-nick-colors-are-distinct-and-readable ()
  "Generated nickname colors stay distinct and readable on common backgrounds."
  (dolist (background '("#000000" "#202020" "#eeeeee" "#ffffff"
                        "black" "white"))
    (dolist (saturation '(60 80 100))
      (dolist (variation '(0 0.5 1))
        (let ((colors
               (mapcar (lambda (hue)
                         (jabber-muc--nick-color
                          hue saturation variation background))
                       '(15 60 105 150 195 240 285 330))))
          (should (>= (length (delete-dups (copy-sequence colors))) 6))
          (dolist (color colors)
            (should (>= (jabber-test-muc--contrast-ratio color background)
                        4.5))))))))

(ert-deftest jabber-test-muc-nick-colors-are-readable-on-midtones ()
  "Generated nickname colors choose the stronger contrast on midtones."
  (dolist (background '("#414141" "#777777" "#989898"))
    (dolist (saturation '(60 100))
      (dolist (variation '(0 1))
        (dolist (hue '(15 60 105 150 195 240 285 330))
          (should (>= (jabber-test-muc--contrast-ratio
                       (jabber-muc--nick-color
                        hue saturation variation background)
                       background)
                      4.5)))))))

(ert-deftest jabber-test-muc-local-nick-face-keeps-plaintext-style ()
  "The local MUC nickname retains its established face and weight."
  (should (eq 'jabber-chat-nick-plaintext
              (face-attribute 'jabber-muc-nick-local-face :inherit nil nil)))
  (should (eq 'semi-bold
              (face-attribute 'jabber-muc-nick-local-face :weight nil nil)))
  (should (eq 'unspecified
              (face-attribute 'jabber-muc-nick-local-face :slant nil nil))))

(ert-deftest jabber-test-muc-nick-face-stable ()
  "A known nickname gets a stable semi-bold foreground."
  (cl-letf (((symbol-function 'jabber-muc--default-background)
             (lambda () "#202020")))
    (let ((face (jabber-muc--nick-face "alice")))
      (should
       (equal (apply #'jabber-muc--nick-color
                     (append (cdr (jabber-muc--nick-color-components "alice"))
                             '("#202020")))
              (plist-get face :foreground)))
      (should (eq 'semi-bold (plist-get face :weight)))
      (should-not (plist-member face :slant)))))

(ert-deftest jabber-test-muc-nick-face-varies-clustered-hues ()
  "Later hash bytes distinguish synthetic clustered hues."
  (cl-letf (((symbol-function 'jabber-muc--default-background)
             (lambda () "#202020")))
    (dolist (nicks '(("wanderer000" "wanderer017" "wanderer178")
                     ("wanderer001" "wanderer209" "willow205")))
      (let ((faces (mapcar #'jabber-muc--nick-face nicks)))
        (should (= (length faces)
                   (length (delete-dups faces))))))))

(ert-deftest jabber-test-muc-display-hue-separates-clustered-hues ()
  "Display hue separates synthetic clustered pairs."
  (dolist (pair '(("wanderer000" "wanderer017")
                  ("wanderer000" "wanderer178")
                  ("wanderer001" "wanderer209")
                  ("wanderer001" "willow205")))
    (pcase-let* ((`(,base-a ,display-a . ,_)
                  (jabber-muc--nick-color-components (car pair)))
                 (`(,base-b ,display-b . ,_)
                  (jabber-muc--nick-color-components (cadr pair))))
      (should (> (jabber-test-muc--hue-distance display-a display-b)
                 (jabber-test-muc--hue-distance base-a base-b))))))

(ert-deftest jabber-test-muc-nick-face-disabled ()
  "Disabling nickname colors restores the existing face."
  (let ((jabber-muc-colorize-nicks nil))
    (should
     (equal '((:weight semi-bold) jabber-chat-nick-foreign-plaintext)
            (jabber-muc--nick-face "alice")))))

(ert-deftest jabber-test-muc-obsolete-palette-option-retains-toggle ()
  "Obsolete palette values retain enabled and disabled behavior."
  (with-suppressed-warnings ((obsolete jabber-muc-nick-color-faces))
    (let ((jabber-muc-nick-color-faces nil))
      (should-not jabber-muc-colorize-nicks)
      (should
       (equal '((:weight semi-bold) jabber-chat-nick-foreign-plaintext)
              (jabber-muc--nick-face "alice"))))
    (let ((jabber-muc-nick-color-faces '(font-lock-keyword-face)))
      (should jabber-muc-colorize-nicks)
      (should (plist-member (jabber-muc--nick-face "alice") :foreground)))))

(ert-deftest jabber-test-muc-print-prompt-colors-foreign-nick ()
  "A foreign MUC prompt receives its selected nickname face."
  (let (prompt-args)
    (cl-letf (((symbol-function 'jabber-chat--format-time)
               (lambda (&rest _args) "12:34"))
              ((symbol-function 'jabber-muc--default-background)
               (lambda () "#202020"))
              ((symbol-function 'jabber-chat--insert-prompt)
               (lambda (&rest args)
                 (setq prompt-args args))))
      (jabber-muc-print-prompt
       '(:from "room@conference.example.com/alice" :timestamp nil))
      (should
       (equal `("12:34" "alice"
                (:weight semi-bold
                 :foreground
                 ,(apply #'jabber-muc--nick-color
                         (append
                          (cdr (jabber-muc--nick-color-components "alice"))
                          '("#202020")))))
              prompt-args)))))

(ert-deftest jabber-test-muc-print-prompt-keeps-local-face ()
  "A local MUC prompt retains the existing nickname face."
  (let (prompt-args)
    (cl-letf (((symbol-function 'jabber-chat--format-time)
               (lambda (&rest _args) "12:34"))
              ((symbol-function 'jabber-chat--insert-prompt)
               (lambda (&rest args)
                 (setq prompt-args args))))
      (jabber-muc-print-prompt
       '(:from "room@conference.example.com/me" :timestamp nil) t)
      (should
       (equal '("12:34" "me"
                jabber-muc-nick-local-face)
              prompt-args)))))

(ert-deftest jabber-test-muc-private-print-prompt-is-semi-bold ()
  "A private MUC prompt uses a semi-bold nickname."
  (let (prompt-args)
    (cl-letf (((symbol-function 'jabber-chat--format-time)
               (lambda (&rest _args) "12:34"))
              ((symbol-function 'jabber-jid-rostername)
               (lambda (_jid) "Room"))
              ((symbol-function 'jabber-chat--insert-prompt)
               (lambda (&rest args)
                 (setq prompt-args args))))
      (jabber-muc-private-print-prompt
       '(:from "room@conference.example.com/alice" :timestamp nil))
      (should
       (equal '("12:34" "Room/alice"
                ((:weight semi-bold) jabber-chat-nick-foreign-plaintext))
              prompt-args)))))

(provide 'jabber-test-muc)
;;; jabber-test-muc.el ends here
