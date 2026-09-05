;;; jabber-test-muc.el --- Tests for jabber-muc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Multi-user chat rooms.

;;; Code:

(require 'ert)

;; Pre-define variables that jabber-muc.el expects at load time
;; from jabber-core.el and jabber-chat.el:
(defvar jabber-silent-mode nil)
(defvar jabber-current-status nil)
(defvar jabber-current-show nil)
(defvar jabber-current-priority nil)
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)
(defvar jabber-jid-obarray (make-vector 127 0))

(require 'jabber-chatbuffer)
(require 'jabber-core)
(require 'jabber-alert)
(require 'jabber-muc)
(require 'jabber-muc-nick-completion)

(defmacro jabber-test-muc-with-active-jc (account &rest body)
  "Run BODY with ACCOUNT active, restoring its symbol properties afterward."
  (declare (indent 1))
  `(let* ((account ,account)
          (properties (copy-sequence (symbol-plist account)))
          (jabber-connections (list account)))
     (unwind-protect
         (progn
           (put account :state :session-established)
           (put account :state-data '(:username "test" :server "example.org"))
           ,@body)
       (setplist account properties))))

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
  (jabber-test-muc-with-active-jc 'fake-jc
    (let ((join-args nil))
      (cl-letf (((symbol-function 'jabber-muc--send-join-presence)
		 (lambda (jc group nickname password popup &optional auto-configure _request)
                   (setq join-args
			 (list jc group nickname password popup auto-configure))))
		((symbol-function 'jabber-bookmarks--publish-one)
		 #'ignore))
	(jabber-muc-create 'fake-jc "room@conference.example.com" "mynick"))
      (should join-args)
      ;; auto-configure (6th) should be t
      (should (nth 5 join-args))
      ;; popup (5th) should be t
      (should (nth 4 join-args)))))

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
       'fake-jc "room@conference.example.com" "mynick" (list jabber-muc-status-room-created)))
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
       'fake-jc "room@conference.example.com" "mynick" (list jabber-muc-status-room-created)))
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
  (jabber-test-muc-with-active-jc 'fake-jc
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
      (should-not disco-called))))

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

(ert-deftest jabber-test-muc-direct-invite-password-reaches-join ()
  "Accepting a direct invite retains its password for the join."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        (jabber-buffer-connection 'jc)
        joined)
    (with-temp-buffer
      (setq-local jabber-buffer-connection 'jc)
      (cl-letf (((symbol-function 'jabber-muc-read-my-nickname)
                 (lambda (&rest _) "nick"))
                ((symbol-function 'jabber-muc-join)
                 (lambda (jc group nick &optional _popup)
                   (setq joined (list jc group nick
                                      (jabber-muc--session-password jc group))))))
        (jabber-muc-print-invite
         (list :xml-data
               '(message ((from . "alice@example.com/home"))
                 (x ((xmlns . "jabber:x:conference")
                     (jid . "room@conference.example.com")
                     (password . "secret")))))
         nil :insert)
        (button-activate (next-button (point-min))))
      (should (equal '(jc "room@conference.example.com" "nick" "secret")
                     joined)))))

(ert-deftest jabber-test-muc-direct-invite-without-password-reaches-join ()
  "Accepting a passwordless direct invite joins without storing a secret."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        joined)
    (with-temp-buffer
      (setq-local jabber-buffer-connection 'jc)
      (cl-letf (((symbol-function 'jabber-muc-read-my-nickname)
                 (lambda (&rest _) "nick"))
                ((symbol-function 'jabber-get-conference-data) #'ignore)
                ((symbol-function 'jabber-muc-join)
                 (lambda (jc group nick &optional _popup)
                   (setq joined (list jc group nick
                                      (jabber-muc--session-password jc group))))))
        (jabber-muc-print-invite
         (list :xml-data
               '(message ((from . "alice@example.com/home"))
                 (x ((xmlns . "jabber:x:conference")
                     (jid . "room@conference.example.com")))))
         nil :insert)
        (button-activate (next-button (point-min))))
      (should (equal '(jc "room@conference.example.com" "nick" nil)
                     joined))
      (should-not (gethash '(jc "room@conference.example.com")
                           jabber-muc--session-passwords)))))

(ert-deftest jabber-test-muc-direct-invite-includes-known-password ()
  "Outgoing direct invitations include a known session password."
  (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
        sent)
    (puthash '(jc "room@conference.example.com") "secret"
             jabber-muc--session-passwords)
    (cl-letf (((symbol-function 'jabber-send-iq) #'ignore)
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc stanza) (setq sent stanza))))
      (jabber-muc-invite 'jc "bob@example.com" "room@conference.example.com" ""))
    (let ((x (car (jabber-xml-get-children sent 'x))))
      (should (equal "secret" (jabber-xml-get-attribute x 'password))))))

(ert-deftest jabber-test-muc-direct-invite-omits-unknown-password ()
  "Outgoing direct invitations omit the password attribute when unknown."
  (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
        sent)
    (cl-letf (((symbol-function 'jabber-send-iq) #'ignore)
              ((symbol-function 'jabber-get-conference-data) #'ignore)
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc stanza) (setq sent stanza))))
      (jabber-muc-invite 'jc "bob@example.com" "room@conference.example.com" ""))
    (let ((x (car (jabber-xml-get-children sent 'x))))
      (should x)
      (should-not (jabber-xml-get-attribute x 'password)))))

(ert-deftest jabber-test-muc-disable-disco-uses-bookmarked-password ()
  "The no-disco join path falls back to a bookmarked password."
  (jabber-test-muc-with-active-jc 'jc
    (let ((jabber-muc-disable-disco-check t)
          (jabber-muc--session-passwords (make-hash-table :test #'equal))
          conference-args
          args)
      (cl-letf (((symbol-function 'jabber-muc-joined-p) (lambda (&rest _) nil))
		((symbol-function 'jabber-muc--autojoin-dequeue) #'ignore)
		((symbol-function 'jabber-get-conference-data)
		 (lambda (&rest values)
                   (setq conference-args values)
                   "secret"))
		((symbol-function 'read-passwd)
		 (lambda (&rest _) (ert-fail "password prompt called")))
		((symbol-function 'jabber-muc--send-join-presence)
		 (lambda (&rest values) (setq args values))))
	(jabber-muc-join 'jc "room@conference.example.com" "nick"))
      (should-not (gethash '(jc "room@conference.example.com")
                           jabber-muc--session-passwords))
      (should (equal '(jc "room@conference.example.com" nil :password)
                     conference-args))
      (should (equal '(jc "room@conference.example.com" "nick" "secret" nil)
                     (seq-take args 5))))))

(ert-deftest jabber-test-muc-prompted-password-survives-self-ping-rejoin ()
  "A prompted room password is retained for forced self-ping rejoin."
  (jabber-test-muc-with-active-jc 'jc
    (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
          (jabber-pending-groupchats (make-hash-table :test #'eq))
          (jabber-jid-obarray (make-vector 127 0))
          sent)
      (cl-letf (((symbol-function 'jabber-muc--validate-disco-result)
		 (lambda (_result)
                   '(:status ok :features ("muc_passwordprotected"))))
		((symbol-function 'read-passwd) (lambda (&rest _) "secret"))
		((symbol-function 'jabber-presence-children) (lambda (_jc) nil))
		((symbol-function 'jabber-send-sexp)
		 (lambda (_jc stanza) (push stanza sent)))
		((symbol-function 'jabber-muc-remove-groupchat) #'ignore)
		((symbol-function 'jabber-iq-error) (lambda (_xml) '(error nil)))
		((symbol-function 'jabber-error-condition)
		 (lambda (_error) 'not-acceptable)))
	(jabber-muc--disco-callback
	 'jc '("room@example.org" "nick" nil) '(iq nil))
	(should (equal "secret"
                       (gethash '(jc "room@example.org")
				jabber-muc--session-passwords)))
	(jabber-muc--self-ping-failed
	 'jc '(iq nil) '("room@example.org" . "nick")))
      (should (= 2 (length sent)))
      (dolist (presence sent)
	(should (equal "secret"
                       (jabber-xml-path presence '(x password ""))))))))

(ert-deftest jabber-test-muc-disable-disco-empty-password-stays-nil ()
  "An empty no-disco password prompt keeps the passwordless join shape."
  (jabber-test-muc-with-active-jc 'jc
    (let ((jabber-muc-disable-disco-check t)
          (jabber-muc--session-passwords (make-hash-table :test #'equal))
          sent-password)
      (cl-letf (((symbol-function 'jabber-muc-joined-p) (lambda (&rest _) nil))
		((symbol-function 'jabber-muc--autojoin-dequeue) #'ignore)
		((symbol-function 'jabber-get-conference-data) #'ignore)
		((symbol-function 'read-passwd) (lambda (&rest _) ""))
		((symbol-function 'jabber-muc--send-join-presence)
		 (lambda (_jc _group _nick password _popup &optional _configure _request)
                   (setq sent-password password))))
	(jabber-muc-join 'jc "room@example.org" "nick" t)
	(should-not sent-password)))))

(ert-deftest jabber-test-muc-session-password-cleared-on-leave ()
  "Leaving a room clears its in-memory password."
  (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
        (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
        (jabber-bookmarks-auto-add nil)
        dequeued)
    (puthash '(jc "room@conference.example.com") "secret"
             jabber-muc--session-passwords)
    (puthash "account@example.com"
             '(("room@conference.example.com" "nick" "secret")
               ("other@conference.example.com" "nick" nil))
             jabber-muc--rooms-before-disconnect)
    (cl-letf (((symbol-function 'jabber-muc-nickname) (lambda (&rest _) "nick"))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "account@example.com"))
              ((symbol-function 'jabber-send-sexp) #'ignore)
              ((symbol-function 'jabber-muc--autojoin-dequeue)
               (lambda (jc group) (setq dequeued (list jc group)))))
      (jabber-muc-leave 'jc "room@conference.example.com"))
    (should (equal dequeued '(jc "room@conference.example.com")))
    (should-not (gethash '(jc "room@conference.example.com")
                         jabber-muc--session-passwords))
    (should (equal (gethash "account@example.com"
                            jabber-muc--rooms-before-disconnect)
                   '(("other@conference.example.com" "nick" nil))))))

(ert-deftest jabber-test-muc-disco-preserves-invited-password ()
  "Disco joins use a direct invitation password even without a feature flag."
  (jabber-test-muc-with-active-jc 'jc
    (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
          sent-password)
      (jabber-muc--remember-password 'jc "room@example.org" "secret")
      (cl-letf (((symbol-function 'jabber-muc--validate-disco-result)
		 (lambda (_result) '(:status ok :features nil)))
		((symbol-function 'jabber-muc--send-join-presence)
		 (lambda (_jc _group _nick password _popup &optional _configure _request)
                   (setq sent-password password))))
	(jabber-muc--disco-callback
	 'jc '("room@example.org" "romeo" nil) nil)
	(should (equal sent-password "secret"))))))

(ert-deftest jabber-test-muc-session-password-is-account-scoped ()
  "Passwords for the same room do not cross account boundaries."
  (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
        (first (list :first))
        (second (list :second)))
    (cl-letf (((symbol-function 'jabber-get-conference-data) #'ignore))
      (jabber-muc--remember-password first "room@example.org" "secret")
      (should (equal (jabber-muc--session-password first "room@example.org")
                     "secret"))
      (should-not (jabber-muc--session-password second "room@example.org")))))

(ert-deftest jabber-test-muc-rejected-session-password-prompts-for-replacement ()
  "A rejected session password prompts without changing other cached secrets."
  (jabber-test-muc-with-active-jc 'jc
    (let ((jabber-muc-disable-disco-check nil)
          (jabber-muc--session-passwords (make-hash-table :test #'equal))
          (room "room@example.org")
          (bookmark-reads 0)
          prompt
          sent)
      (puthash (list 'jc room) "rejected" jabber-muc--session-passwords)
      (puthash '(jc "other@example.org") "other-room"
               jabber-muc--session-passwords)
      (puthash '(other-jc "room@example.org") "other-account"
               jabber-muc--session-passwords)
      (cl-letf (((symbol-function 'jabber-muc-remove-groupchat) #'ignore)
		((symbol-function 'jabber-muc-get-buffer)
		 (lambda (&rest _) " *missing-muc-test*"))
		((symbol-function 'run-with-timer) #'ignore)
		((symbol-function 'message) #'ignore)
		((symbol-function 'jabber-muc-joined-p) (lambda (&rest _) nil))
		((symbol-function 'jabber-muc--autojoin-dequeue) #'ignore)
		((symbol-function 'jabber-muc--validate-disco-result)
		 (lambda (_result) '(:status no-disco :features nil)))
		((symbol-function 'jabber-disco-get-info)
		 (lambda (jc _group _node callback closure)
                   (funcall callback jc closure nil)))
		((symbol-function 'jabber-get-conference-data)
		 (lambda (&rest _)
                   (setq bookmark-reads (1+ bookmark-reads))
                   "bookmarked"))
		((symbol-function 'read-passwd)
		 (lambda (&rest _)
                   (setq prompt t)
                   "replacement"))
		((symbol-function 'jabber-muc--send-join-presence)
		 (lambda (&rest args) (setq sent args))))
	(jabber-muc--process-self-leave
	 'jc room "error" nil
	 `(error ((code . "401"))
		 (not-authorized ((xmlns . ,jabber-stanzas-xmlns))))
	 nil nil)
	(should-not (gethash (list 'jc room) jabber-muc--session-passwords
                             'missing))
	(should (equal "other-room"
                       (gethash '(jc "other@example.org")
				jabber-muc--session-passwords)))
	(should (equal "other-account"
                       (gethash '(other-jc "room@example.org")
				jabber-muc--session-passwords)))
	(jabber-muc-join 'jc room "nick" t))
      (should prompt)
      (should (zerop bookmark-reads))
      (should (equal '(jc "room@example.org" "nick" "replacement" t)
                     (seq-take sent 5))))))

(ert-deftest jabber-test-muc-unrelated-error-preserves-session-password ()
  "A non-authorization MUC error preserves the cached session password."
  (let ((jabber-muc--session-passwords (make-hash-table :test #'equal))
        (room "room@example.org"))
    (puthash (list 'jc room) "valid" jabber-muc--session-passwords)
    (cl-letf (((symbol-function 'jabber-muc-remove-groupchat) #'ignore)
              ((symbol-function 'jabber-muc-get-buffer)
               (lambda (&rest _) " *missing-muc-test*"))
              ((symbol-function 'run-with-timer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (jabber-muc--process-self-leave
       'jc room "error" nil
       `(error ((code . "503"))
               (service-unavailable ((xmlns . ,jabber-stanzas-xmlns))))
       nil nil))
    (should (equal "valid"
                   (gethash (list 'jc room) jabber-muc--session-passwords)))))

(ert-deftest jabber-test-muc-rejoin-snapshots-are-account-scoped ()
  "Rejoining one account leaves another account's snapshot untouched."
  (let ((jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        enqueued)
    (puthash "first@example.com" '(("one@muc.example" "one" "secret"))
             jabber-muc--rooms-before-disconnect)
    (puthash "second@example.com" '(("two@muc.example" "two" nil))
             jabber-muc--rooms-before-disconnect)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (jc) (if (eq jc 'first)
                                "first@example.com"
                              "second@example.com")))
              ((symbol-function 'jabber-muc-joined-p) (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc--autojoin-queued-p)
               (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc--autojoin-enqueue-pending)
               (lambda (jc room nick) (setq enqueued (list jc room nick)))))
      (jabber-muc--rejoin-snapshot 'first))
    (should (equal enqueued '(first "one@muc.example" "one")))
    (should (equal (jabber-muc--session-password 'first "one@muc.example")
                   "secret"))
    (should-not (gethash "first@example.com"
                         jabber-muc--rooms-before-disconnect))
    (should (gethash "second@example.com"
                     jabber-muc--rooms-before-disconnect))))

(ert-deftest jabber-test-muc-session-reset-snapshots-transient-password ()
  "Unexpected reconnectable disconnect snapshots then clears room secrets."
  (let ((jabber-auto-reconnect t)
        (jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--room-jids (make-hash-table :test #'equal))
        (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
        (jabber-muc-participants nil))
    (puthash "room@example.org" '((jc . "nick")) jabber-muc--rooms)
    (jabber-muc--remember-password 'jc "room@example.org" "secret")
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "a@example.org"))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:ever-session-established t))))
      (jabber-muc--session-reset 'jc)
      (should-not (gethash '(jc "room@example.org")
                           jabber-muc--session-passwords))
      (should (equal (gethash "a@example.org"
                              jabber-muc--rooms-before-disconnect)
                     '(("room@example.org" "nick" "secret")))))))

(ert-deftest jabber-test-muc-session-reset-discards-terminal-password ()
  "Expected terminal disconnects discard secrets and reconnect snapshots."
  (let ((jabber-auto-reconnect t)
        (jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--room-jids (make-hash-table :test #'equal))
        (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
        (jabber-muc-participants nil))
    (puthash "room@example.org" '((jc . "nick")) jabber-muc--rooms)
    (jabber-muc--remember-password 'jc "room@example.org" "secret")
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "a@example.org"))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc) '(:disconnection-expected t
                               :ever-session-established t))))
      (jabber-muc--session-reset 'jc)
      (should-not (gethash '(jc "room@example.org")
                           jabber-muc--session-passwords))
      (should-not (gethash "a@example.org"
                           jabber-muc--rooms-before-disconnect)))))

(ert-deftest jabber-test-muc-disconnect-cancels-pending-secret-rejoin ()
  "Public disconnect cancels a pending reconnect and clears its room secret."
  (let* ((jc (make-symbol "jabber-test-pending-reconnect"))
         (jabber-connections (list jc))
         (jabber-auto-reconnect t)
         (jabber-lifecycle-session-reset-functions
          '(jabber-muc--session-reset))
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-muc--room-jids (make-hash-table :test #'equal))
         (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
         (jabber-muc--session-passwords (make-hash-table :test #'equal))
         (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
         (jabber-muc-participants nil)
         (room "room@conference.example.com")
         (account "a@example.org")
         enqueued)
    (put jc :name 'jabber-connection)
    (put jc :state nil)
    (put jc :state-data '(:username "a" :server "example.org"
                          :ever-session-established t))
    (put jc :deferred nil)
    (put jc :timeout (run-with-timer 60 nil #'ignore))
    (puthash account (list (list room "nick" "secret"))
             jabber-muc--rooms-before-disconnect)
    (jabber-muc--remember-password jc room "secret")
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-muc--autojoin-enqueue-pending)
                   (lambda (&rest args) (setq enqueued args))))
          (jabber-disconnect-one jc)
          (should (plist-get (fsm-get-state-data jc)
                             :disconnection-expected))
          (should-not (memq jc jabber-connections))
          (should-not (get jc :timeout))
          (should-not (gethash (list jc room)
                               jabber-muc--session-passwords))
          (should-not (gethash account jabber-muc--rooms-before-disconnect))
          (jabber-muc--rejoin-snapshot jc)
          (should-not enqueued))
      (when (timerp (get jc :timeout))
        (cancel-timer (get jc :timeout))))))

(ert-deftest jabber-test-muc-legacy-close-preserves-reconnect-snapshot ()
  "The legacy one-argument close call preserves rooms for reconnect."
  (let ((jabber-muc--rooms (make-hash-table :test #'equal))
        (jabber-muc--room-jids (make-hash-table :test #'equal))
        (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
        (jabber-muc--session-passwords (make-hash-table :test #'equal))
        (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
        (jabber-muc-participants nil))
    (puthash "room@example.org" '((jc . "nick")) jabber-muc--rooms)
    (jabber-muc--remember-password 'jc "room@example.org" "secret")
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "a@example.org")))
      (jabber-muc-connection-closed "a@example.org"))
    (should (equal (gethash "a@example.org"
                            jabber-muc--rooms-before-disconnect)
                   '(("room@example.org" "nick" "secret"))))))

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
       nil "room@example.org" "me"
       (list jabber-muc-status-nonanonymous
             jabber-muc-status-logging-enabled
             jabber-muc-status-now-nonanonymous))
      (should (equal
               '("This room exposes your real JID to other occupants"
                 "This room is publicly logged"
                 "This room is now non-anonymous")
               (nreverse notices))))))

(ert-deftest jabber-test-muc-personal-nick-is-literal ()
  "Regexp characters in a nickname are matched literally."
  (cl-letf (((symbol-function 'jabber-my-nick) (lambda (&optional _) "bob.m")))
    (should (jabber-muc-looks-like-personal-p "bob.m, hello"))
    (should (jabber-muc-looks-like-personal-p "bob.m: hello"))
    (should (jabber-muc-looks-like-personal-p "bob.m> hello"))
    (should-not (jabber-muc-looks-like-personal-p "bobXm: hello"))))

(ert-deftest jabber-test-muc-personal-plus-nick-matches ()
  "A plus sign in a nickname does not alter mention matching."
  (cl-letf (((symbol-function 'jabber-my-nick) (lambda (&optional _) "bob+m")))
    (should (jabber-muc-looks-like-personal-p "bob+m: hello"))))

(ert-deftest jabber-test-muc-personal-rejects-non-string-inputs ()
  "Personal mention detection ignores missing rooms and non-string messages."
  (jabber-test-muc-with-rooms nil
    (let ((jabber-muc-default-nicknames nil))
      (should-not
       (jabber-muc-looks-like-personal-p
        "hello" "unknown@conference.example.com"))
      (should-not (jabber-muc-looks-like-personal-p 'not-a-message)))))

(ert-deftest jabber-test-muc-unknown-room-personal-alert-is-ignored ()
  "An inbound alert from an unknown room does not signal or run its action."
  (jabber-test-muc-with-rooms nil
    (let ((jabber-muc-default-nicknames nil)
          called)
      (cl-letf (((symbol-function 'jabber-muc-beep)
                 (lambda (&rest _ignore) (setq called t))))
        (jabber-muc-beep-personal
         "sender" "unknown@conference.example.com" nil "hello" t))
      (should-not called))))

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

(ert-deftest jabber-test-muc-receive-config-opens-data-form-in-origin ()
  "A live originating buffer opens a dedicated configuration form."
  (let (shown-form shown-actions)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'jabber-xdata-form-open)
               (lambda (form actions)
                 (setq shown-form form
                       shown-actions actions))))
      (with-temp-buffer
        (jabber-muc--receive-config
         'fake-jc
         '(iq ((from . "room@conference.example.org") (type . "result"))
              (query ((xmlns . "http://jabber.org/protocol/muc#owner"))
                     (x ((xmlns . "jabber:x:data") (type . "form"))
                        (field ((var . "FORM_TYPE") (type . "hidden"))
                               (value nil
                                      "http://jabber.org/protocol/muc#roomconfig"))
                        (field ((var . "roles") (type . "list-multi"))
                               (value nil "moderator")
                               (option nil (value nil "moderator"))
                               (option nil (value nil "participant"))))))
         (list (current-buffer) "room@conference.example.org"))))
    (should (equal (plist-get
                    (jabber-xdata-field shown-form "roles") :values)
                   '("moderator")))
    (should (equal (plist-get shown-form :title)
                   "Configuration: room@conference.example.org"))
    (should (equal (mapcar (lambda (action)
                            (plist-get action :label))
                          shown-actions)
                   '("Submit" "Cancel")))))

(ert-deftest jabber-test-muc-overlapping-forms-keep-room-context ()
  "Submitting an older form uses its original room and connection."
  (let (opened sent)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'jabber-xdata-form-open)
               (lambda (_form actions)
                 (setq opened (append opened (list actions)))))
              ((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-send-iq)
               (lambda (jc to _type _query &rest _ignore)
                 (setq sent (list jc to)))))
      (with-temp-buffer
        (dolist (request '((first-jc "one@conference.example.org")
                           (second-jc "two@conference.example.org")))
          (jabber-muc--receive-config
           (nth 0 request)
           '(iq ((type . "result"))
                (query ((xmlns . "http://jabber.org/protocol/muc#owner"))
                       (x ((xmlns . "jabber:x:data") (type . "form")))))
           (list (current-buffer) (nth 1 request)))))
      (with-temp-buffer
        (setq-local jabber-xdata-form--form '(:fields nil))
        (let ((jabber-connections '(first-jc second-jc)))
          (call-interactively
           (plist-get (car (car opened)) :command)))))
    (should (equal sent '(first-jc "one@conference.example.org")))))

(ert-deftest jabber-test-muc-submit-config-preserves-form-values ()
  "MUC configuration submits hidden and ordered list-multi values."
  (let (sent-query)
    (cl-letf (((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-send-iq)
               (lambda (_jc to type query &rest _ignore)
                 (should (equal to "room@conference.example.org"))
                 (should (equal type "set"))
                 (setq sent-query query))))
      (with-temp-buffer
        (setq-local jabber-muc--config-connection 'fake-jc)
        (setq-local jabber-muc--config-group "room@conference.example.org")
        (setq-local jabber-xdata-form--form
                    (jabber-xdata-parse
                     '(x ((xmlns . "jabber:x:data") (type . "form"))
                         (field ((var . "FORM_TYPE") (type . "hidden"))
                                (value nil "urn:roomconfig"))
                         (field ((var . "roles") (type . "list-multi"))
                                (value nil "visitor")
                                (value nil "moderator")
                                (option nil (value nil "moderator"))
                                (option nil (value nil "visitor"))))))
        (let ((jabber-connections '(fake-jc)))
          (jabber-muc-submit-config))))
    (let* ((xdata (car (jabber-xml-get-children sent-query 'x)))
           (fields (jabber-xml-get-children xdata 'field)))
      (should (equal
               (mapcar (lambda (value)
                         (car (jabber-xml-node-children value)))
                       (jabber-xml-get-children (cadr fields) 'value))
               '("moderator" "visitor")))
      (should (equal
               (car (jabber-xml-node-children
                     (car (jabber-xml-get-children (car fields) 'value))))
               "urn:roomconfig")))))

(ert-deftest jabber-test-muc-cancel-config-sends-cancel-form ()
  "Canceling a room configuration sends XEP-0004 type cancel."
  (let (sent-query)
    (cl-letf (((symbol-function 'jabber-connection-active-p) #'identity)
              ((symbol-function 'jabber-send-iq)
               (lambda (_jc _to _type query &rest _ignore)
                 (setq sent-query query))))
      (with-temp-buffer
        (setq-local jabber-muc--config-connection 'fake-jc)
        (setq-local jabber-muc--config-group "room@conference.example.org")
        (let ((jabber-connections '(fake-jc)))
          (jabber-muc-cancel-config))))
    (let ((xdata (car (jabber-xml-get-children sent-query 'x))))
      (should (equal (jabber-xml-get-attribute xdata 'type) "cancel")))))

(ert-deftest jabber-test-muc-config-rejects-reconnecting-connection ()
  "MUC submit and cancel reject a reconnecting FSM before sending."
  (let ((reconnecting (make-symbol "reconnecting"))
        jabber-connections called)
    (setq jabber-connections (list reconnecting))
    (put reconnecting :state :connecting)
    (cl-letf (((symbol-function 'jabber-send-iq)
               (lambda (&rest _ignore) (setq called t))))
      (with-temp-buffer
        (setq-local jabber-muc--config-connection reconnecting)
        (setq-local jabber-muc--config-group
                    "room@conference.example.org")
        (should-error (jabber-muc-submit-config) :type 'user-error)
        (should-error (jabber-muc-cancel-config) :type 'user-error)))
    (should-not called)))

(ert-deftest jabber-test-muc-config-resolves-established-replacement ()
  "MUC configuration resolves an established replacement connection."
  (let ((jabber-muc--config-connection 'dead-jc))
    (cl-letf (((symbol-function 'jabber-find-active-connection)
               (lambda (_jc) 'live-jc))
              ((symbol-function 'jabber-connection-active-p)
               (lambda (jc) (eq jc 'live-jc))))
      (should (eq (jabber-muc--config-active-connection) 'live-jc)))))

(defmacro jabber-test-muc-with-native-intent (&rest body)
  "Run BODY with isolated native join state and captured synthetic sends."
  (declare (indent 0))
  `(let ((jc (make-symbol "native-jc"))
         (other (make-symbol "native-other"))
         (room "room@example.org")
         (jabber-db-path nil)
         (jabber-current-status nil)
         (jabber-current-show nil)
         (jabber-current-priority nil)
         (jabber-muc-disable-disco-check nil)
         (jabber-muc--rooms (make-hash-table :test #'equal))
         (jabber-muc--session-passwords (make-hash-table :test #'equal))
         (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
         (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
         (jabber-pending-groupchats (make-hash-table :test #'eq))
         (jabber-jid-obarray (make-vector 127 0))
         (jabber-bookmarks-auto-add nil)
         replies sent)
     (let ((jabber-connections (list jc other)))
       (dolist (account (list jc other))
	 (put account :state :session-established)
	 (put account :state-data
              (list :connection (make-symbol "transport") :session-id "session")))
       (cl-letf (((symbol-function 'jabber-disco-get-info)
                  (lambda (account _group _node callback closure)
                    (setq replies (append replies
                                          (list (lambda (result)
                                                  (funcall callback account closure result)))))))
		 ((symbol-function 'jabber-muc--validate-disco-result)
                  (lambda (result) (or result '(:status ok))))
		 ((symbol-function 'jabber-get-conference-data) #'ignore)
		 ((symbol-function 'jabber-muc--autojoin-dequeue) #'ignore)
		 ((symbol-function 'jabber-muc-create-buffer)
                  (lambda (&rest _) (current-buffer)))
		 ((symbol-function 'switch-to-buffer) #'ignore)
		 ((symbol-function 'jabber-presence-children) #'ignore)
		 ((symbol-function 'jabber-connection-bare-jid)
                  (lambda (account) (symbol-name account)))
		 ((symbol-function 'jabber-send-sexp)
                  (lambda (account stanza)
                    (push (list account (jabber-xml-get-attribute stanza 'to)
				(jabber-xml-get-attribute stanza 'type)) sent))))
	 ,@body))))

(ert-deftest jabber-test-muc-native-intent-latest-reply-orders ()
  "Only the latest native request sends, in either discovery reply order."
  (dolist (order '((0 1) (1 0)))
    (jabber-test-muc-with-native-intent
      (jabber-muc-join jc room "old")
      (jabber-muc-join jc room "new")
      (dolist (index order) (funcall (nth index replies) nil))
      (should (equal sent (list (list jc "room@example.org/new" nil)))))))

(ert-deftest jabber-test-muc-native-intent-prompt-reentry ()
  "Password and create prompts cannot revive a superseded request."
  (dolist (result '((:status ok :features ("muc_passwordprotected"))
                    (:status not-found)))
    (jabber-test-muc-with-native-intent
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _)
                   (jabber-muc-join jc room "new") "secret"))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (jabber-muc-join jc room "new") t)))
        (jabber-muc-join jc room "old")
        (funcall (car replies) result)
        (should-not sent)
        (funcall (cadr replies) nil)
        (should (equal sent (list (list jc "room@example.org/new" nil))))))))

(ert-deftest jabber-test-muc-native-intent-retirement ()
  "Direct send, leave and logical reset retire outstanding discovery."
  (dolist (action '(direct leave reset))
    (jabber-test-muc-with-native-intent
      (jabber-muc-join jc room "old")
      (pcase action
        ('direct (jabber-muc-join-3 jc room "direct" nil nil))
        ('leave (jabber-muc-leave jc room))
        ('reset (jabber-muc--session-reset jc)))
      (let ((before (copy-tree sent)))
        (funcall (car replies) nil)
        (should (equal sent before))))))

(ert-deftest jabber-test-muc-native-intent-context-replaced ()
  "A replaced transport or logical session blocks late and nested sends."
  (dolist (field '(:connection :session-id :nil-entry-token))
    (dolist (during-prompt '(nil t))
      (jabber-test-muc-with-native-intent
        (cl-flet ((replace-context ()
                    (put jc :state-data
                         (plist-put (fsm-get-state-data jc) field
                                    (make-symbol "successor")))))
          (cl-letf (((symbol-function 'read-passwd)
                     (lambda (&rest _) (replace-context) "secret")))
            (jabber-muc-join jc room "old")
            (unless during-prompt (replace-context))
            (funcall (car replies)
                     (when during-prompt
                       '(:status ok :features ("muc_passwordprotected"))))
            (should-not sent)))))))

(ert-deftest jabber-test-muc-native-intent-independent-scopes ()
  "A newer request leaves another room and another account untouched."
  (jabber-test-muc-with-native-intent
    (jabber-muc-join jc room "old")
    (jabber-muc-join other room "other")
    (jabber-muc-join jc "second@example.org" "second")
    (jabber-muc-join jc room "new")
    (dolist (reply replies) (funcall reply nil))
    (should (equal (reverse sent)
                   (list (list other "room@example.org/other" nil)
                         (list jc "second@example.org/second" nil)
                         (list jc "room@example.org/new" nil))))))

(ert-deftest jabber-test-muc-native-intent-joined-preserved ()
  "An already joined command opens, syncs and pings without new presence."
  (jabber-test-muc-with-native-intent
    (let (effects)
      (jabber-muc-join jc room "old")
      (jabber-muc-join-set room jc "joined")
      (cl-letf (((symbol-function 'jabber-muc-create-buffer)
                 (lambda (&rest _) (current-buffer)))
                ((symbol-function 'switch-to-buffer)
                 (lambda (&rest _) (push 'open effects)))
                ((symbol-function 'jabber-mam-muc-joined)
                 (lambda (&rest _) (push 'sync effects)))
                ((symbol-function 'jabber-muc--self-ping-one)
                 (lambda (&rest _) (push 'ping effects))))
        (jabber-muc-join jc room "joined" t))
      (funcall (car replies) nil)
      (should-not sent)
      (should (equal effects '(ping sync open)))
      (should (equal (jabber-muc-nickname room jc) "joined")))))

(ert-deftest jabber-test-muc-native-intent-legacy-context ()
  "Legacy callback aliases capture context before prompting."
  (jabber-test-muc-with-native-intent
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _)
                 (put jc :state-data '(:connection replacement)) "secret")))
      (jabber-muc-join-2 jc (list room "old" nil)
                         '(:status ok :features ("muc_passwordprotected")))
      (should-not sent))))

(ert-deftest jabber-test-muc-native-intent-no-disco-reentry ()
  "A no-disco password prompt cannot replace a nested newer join."
  (jabber-test-muc-with-native-intent
    (let ((jabber-muc-disable-disco-check t))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _)
                   (jabber-muc-join jc room "new") "old-secret")))
        (jabber-muc-join jc room "old" t))
      (should (equal sent (list (list jc "room@example.org/new" nil)))))))

(ert-deftest jabber-test-muc-native-intent-pending-reply-orders ()
  "Latest discovery also wins while an earlier join presence is pending."
  (dolist (order '((0 1) (1 0)))
    (jabber-test-muc-with-native-intent
      (jabber-muc-join-3 jc room "pending" nil nil)
      (setq sent nil)
      (jabber-muc-join jc room "old")
      (jabber-muc-join jc room "new")
      (dolist (index order) (funcall (nth index replies) nil))
      (should (equal sent (list (list jc "room@example.org/new" nil)))))))

(ert-deftest jabber-test-muc-native-intent-completion-retires-owner ()
  "Completion and cancellation release only the request that completed."
  (dolist (outcome '(success not-conference quit error))
    (jabber-test-muc-with-native-intent
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (signal outcome nil))))
        (jabber-muc-join jc room "nick")
        (condition-case nil
            (funcall (car replies)
                     (pcase outcome
                       ('success '(:status ok))
                       ('not-conference '(:status not-conference))
                       (_ '(:status ok :features ("muc_passwordprotected")))))
          ((error quit) nil)))
      (should-not (get jc 'jabber-muc--join-intents)))))

(ert-deftest jabber-test-muc-native-intent-construction-successor ()
  "Real presence hooks cannot hand off an old join after a newer one."
  (let ((dispatcher (symbol-function 'jabber-presence-children)))
    (dolist (entry '(disco no-disco direct))
      (dolist (complete '(nil t))
        (jabber-test-muc-with-native-intent
          (let ((jabber-presence-element-functions
                 (list (lambda (_account)
                         (let ((jabber-presence-element-functions nil)
                               (jabber-muc-disable-disco-check nil))
                           (jabber-muc-join jc room "new")
                           (when complete (funcall (car (last replies)) nil)))
                         nil))))
            (cl-letf (((symbol-function 'jabber-presence-children) dispatcher))
              (pcase entry
                ('disco
                 (jabber-muc-join jc room "old")
                 (funcall (car replies) nil))
                ('no-disco
                 (let ((jabber-muc-disable-disco-check t))
                   (jabber-muc-join jc room "old")))
                ('direct (jabber-muc-join-3 jc room "old" "old-secret" nil)))
              (unless complete
                (should-not sent)
                (let ((jabber-presence-element-functions nil))
                  (funcall (car (last replies)) nil)))))
          (should (equal sent (list (list jc "room@example.org/new" nil))))
          (should (equal (gethash (jabber-jid-symbol room)
                                  jabber-pending-groupchats) "new"))
          (should-not (jabber-muc--session-password jc room))
          (should-not (get jc 'jabber-muc--join-intents)))))))

(ert-deftest jabber-test-muc-native-intent-construction-cancelled ()
  "Context changes and cancellation in real presence hooks prevent writes."
  (let ((dispatcher (symbol-function 'jabber-presence-children)))
    (dolist (entry '(disco no-disco direct))
      (dolist (action '(:connection :session-id :nil-entry-token leave reset))
        (jabber-test-muc-with-native-intent
          (let ((jabber-presence-element-functions
                 (list (lambda (_account)
                         (pcase action
                           ('leave (jabber-muc-leave jc room))
                           ('reset (jabber-muc--session-reset jc))
                           (_ (put jc :state-data
                                   (plist-put (fsm-get-state-data jc) action
                                              (make-symbol "replacement")))))
                         nil))))
            (cl-letf (((symbol-function 'jabber-presence-children) dispatcher)
                      ((symbol-function 'read-passwd)
                       (lambda (&rest _) "old-secret")))
              (pcase entry
                ('disco
                 (jabber-muc-join jc room "old")
                 (funcall (car replies)
                          '(:status ok :features ("muc_passwordprotected"))))
                ('no-disco
                 (let ((jabber-muc-disable-disco-check t))
                   (jabber-muc-join jc room "old" t)))
                ('direct (jabber-muc-join-3 jc room "old" "old-secret" nil)))))
          (should-not (seq-find (lambda (send) (null (nth 2 send))) sent))
          (should-not (gethash (jabber-jid-symbol room) jabber-pending-groupchats))
          (should-not (jabber-muc--session-password jc room))
          (should-not (get jc 'jabber-muc--join-intents)))))))

(defun jabber-test-muc-native-create-completion (phase action)
  "Exercise native creation interrupted at PHASE by ACTION."
  (require 'jabber-bookmarks)
  (let ((dispatcher (symbol-function 'jabber-presence-children)))
    (jabber-test-muc-with-native-intent
      (with-temp-buffer
        (let ((buffer (current-buffer))
              (jabber-bookmarks (make-hash-table :test #'equal))
              publications callbacks opened created interrupted)
          (cl-labels
              ((interrupt (where)
                 (when (and (eq phase where) (not interrupted))
                   (setq interrupted t)
                   (pcase action
                     ('join (jabber-muc-join jc room "new"))
                     ('leave (jabber-muc-leave jc room))
                     ('reset (jabber-muc--session-reset jc))
                     ((or 'error 'quit)
                      (jabber-muc-join jc room "new")
                      (signal action nil))))))
            (let ((jabber-presence-element-functions
                   (list (lambda (_) (interrupt 'presence) nil))))
              (cl-letf (((symbol-function 'jabber-presence-children) dispatcher)
                        ((symbol-function 'jabber-bookmarks--legacy-p) (lambda (_) nil))
                        ((symbol-function 'jabber-bookmarks2--publish)
                         (lambda (_jc bookmark success _failure)
                           (push bookmark publications)
                           (push success callbacks)))
                        ((symbol-function 'jabber-bookmarks--refresh-buffer) #'ignore)
                        ((symbol-function 'jabber-send-sexp)
                         (lambda (account stanza)
                           (push (list account (jabber-xml-get-attribute stanza 'to)
                                       (jabber-xml-get-attribute stanza 'type)) sent)
                           (interrupt 'send)))
                        ((symbol-function 'jabber-muc-create-buffer)
                         (lambda (account target)
                           (setq created t)
                           (with-current-buffer buffer
                             (setq-local jabber-buffer-connection account)
                             (setq-local jabber-group target))
                           (interrupt 'buffer) buffer))
                        ((symbol-function 'switch-to-buffer)
                         (lambda (&rest _) (setq opened t) (interrupt 'switch))))
                (let ((caught nil))
                  (condition-case err
                      (jabber-muc-create jc room "old")
                    ((error quit) (setq caught (car err))))
                  (should (eq caught (and (memq action '(error quit)) action))))
                (if phase
                    (progn
                      (should-not publications)
                      (should-not jabber-muc--auto-configure)
                      (when (memq phase '(presence send buffer))
                        (should-not opened))
                      (when (eq phase 'presence) (should-not created)))
                  (should opened)
                  (should jabber-muc--auto-configure)
                  (should (equal (mapcar (lambda (bm) (plist-get bm :nick))
                                         publications) '("old"))))
                ;; Only real transport handoffs may have a success callback.
                (dolist (callback callbacks) (funcall callback jc nil nil))
                (let ((jabber-presence-element-functions nil))
                  (dolist (reply replies) (funcall reply nil)))
                (cond
                 ((or (eq action 'join) (memq action '(error quit)))
                  (should (equal (gethash (jabber-jid-symbol room)
                                          jabber-pending-groupchats) "new"))
                  (should (equal (car sent) (list jc "room@example.org/new" nil))))
                 ((null phase)
                  (should (equal (gethash (jabber-jid-symbol room)
                                          jabber-pending-groupchats) "old")))
                 ((eq phase 'presence)
                  (should-not (seq-find (lambda (send) (null (nth 2 send))) sent)))))
              (should-not (get jc 'jabber-muc--join-intents)))))))))

(ert-deftest jabber-test-muc-native-create-construction-cancelled ()
  "An obsolete create never publishes a bookmark that can revive it."
  (dolist (action '(join leave reset))
    (jabber-test-muc-native-create-completion 'presence action)))

(ert-deftest jabber-test-muc-native-create-post-callback-cancelled ()
  "Send and UI reentry suppress obsolete configuration and publication."
  (dolist (phase '(send buffer switch))
    (dolist (action '(join leave reset))
      (jabber-test-muc-native-create-completion phase action))))

(ert-deftest jabber-test-muc-native-create-errors-preserve-successor ()
  "Errors and quits propagate without retiring a nested successor."
  (dolist (phase '(presence send buffer switch))
    (dolist (action '(error quit))
      (jabber-test-muc-native-create-completion phase action))))

(ert-deftest jabber-test-muc-native-create-success ()
  "Successful creation opens, configures and publishes through real callbacks."
  (jabber-test-muc-native-create-completion nil nil))

(defmacro jabber-test-muc-with-native-room (&rest body)
  "Run BODY with real native buffers and isolated external effects."
  (declare (indent 0))
  `(let* ((jc (make-symbol "room-a"))
          (other (make-symbol "room-b"))
          (room "native@example.org")
          (jabber-connections (list jc other))
          (jabber-db-path nil)
          (jabber-groupchat-buffer-format " *native-%n-%a*")
          (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
          (jabber-muc--rooms (make-hash-table :test #'equal))
          (jabber-muc--session-passwords (make-hash-table :test #'equal))
          (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
          (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
          (jabber-muc-participants nil)
          (jabber-pending-groupchats (make-hash-table :test #'eq))
          (jabber-jid-obarray (make-vector 127 0))
          (jabber-bookmarks-auto-add nil)
          (jabber-presence-element-functions nil)
          (jabber-chat-mode-hook nil)
          (before (buffer-list))
          sent published config)
     (dolist (account jabber-connections)
       (put account :state :session-established)
       (put account :state-data
            (list :connection (make-symbol "transport") :session-id "session"
                  :username (symbol-name account) :server "example.org")))
     (save-window-excursion
       (unwind-protect
           (cl-letf (((symbol-function 'jabber-send-sexp)
                      (lambda (account stanza) (push (list account stanza) sent)))
                     ((symbol-function 'jabber-send-iq)
                      (lambda (account target type query callback context &rest _)
                        (when (equal (jabber-xml-get-attribute query 'xmlns)
                                     jabber-muc-xmlns-owner)
                          (push (list account target type callback context
                                      jabber-muc--auto-configure) config))))
                     ((symbol-function 'jabber-bookmarks--publish-one)
                      (lambda (&rest args) (push args published)))
                     ((symbol-function 'jabber-mam-muc-joined) #'ignore)
                     ((symbol-function 'run-with-timer) #'ignore))
             ,@body)
         (dolist (buffer (seq-difference (buffer-list) before))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun jabber-test-muc-native-201 (jc room)
  "Deliver real self-presence creating ROOM on JC."
  (jabber-muc-process-presence
   jc `(presence ((from . ,(concat room "/nick")))
                 (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                    (item ((affiliation . "owner") (role . "moderator")))
                    (status ((code . "110")))
                    (status ((code . "201")))))))

(ert-deftest jabber-test-muc-native-room-collision ()
  "Real constructor rejects a foreign arm before any rebind or handoff."
  (jabber-test-muc-with-native-room
    (let ((jabber-groupchat-buffer-format " *native-%n*"))
      (jabber-muc-create jc room "nick")
      (let* ((buffer (jabber-muc-find-buffer room jc))
             (arm (buffer-local-value 'jabber-muc--auto-configure buffer))
             (keys (buffer-local-value 'jabber-buffer-registry--keys buffer)))
        (should-error (jabber-muc-create other room "other") :type 'user-error)
        (should-error (jabber-muc-create-buffer other room) :type 'user-error)
        (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
        (should (eq jc (buffer-local-value 'jabber-buffer-connection buffer)))
        (should (equal keys (buffer-local-value 'jabber-buffer-registry--keys buffer)))
        (should (= (length sent) 1))
        (should (= (length published) 1))
        (should-not (get other 'jabber-muc--join-intents))
        ;; Historical aliases can route presence without constructing a buffer.
        (jabber-buffer-registry-register
         'muc (jabber-muc--buffer-key other room) buffer)
        (jabber-test-muc-native-201 other room)
        (should-not config)
        (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
        (jabber-test-muc-native-201 jc room)
        (should (= (length config) 1))
        (should (eq (caar config) jc))
        (should-not (nth 5 (car config)))
        (jabber-test-muc-native-201 jc room)
        (should (= (length config) 1))
        (jabber-muc-create other room "retry")
        (should (= (length published) 2))
        (jabber-test-muc-native-201 other room)
        (should (= (length config) 2))))))

(ert-deftest jabber-test-muc-native-room-progress ()
  "201 during send, constructor reentry or real switching is positive progress."
  (dolist (phase '(send constructor switch))
    (jabber-test-muc-with-native-room
      (let ((send (symbol-function 'jabber-send-sexp))
            fired)
        (cl-labels ((deliver ()
                      (unless fired
                        (setq fired t)
                        (when (eq phase 'constructor)
                          (jabber-muc-create-buffer jc room))
                        (jabber-test-muc-native-201 jc room))))
          (let ((buffer-list-update-hook
                 (when (eq phase 'switch)
                   (list (lambda ()
                           (when (and (equal jabber-group room)
                                      (eq jabber-buffer-connection jc)
                                      sent)
                             (deliver)))))))
            (cl-letf (((symbol-function 'jabber-send-sexp)
                       (lambda (account stanza)
                         (funcall send account stanza)
                         (unless (eq phase 'switch) (deliver)))))
              (jabber-muc-create jc room "nick")))
          (should fired)
          (should (= (length config) 1))
          (should-not (nth 5 (car config)))
          (should (= (length published) 1))
          (with-current-buffer (jabber-muc-find-buffer room jc)
            (should-not jabber-muc--auto-configure))
          (should-not (get jc 'jabber-muc--join-intents)))))))

(ert-deftest jabber-test-muc-native-room-origin ()
  "Notice reentry cannot change the account or room of a config request."
  (dolist (manual '(nil t manual))
    (jabber-test-muc-with-native-room
      (jabber-muc-create jc room "nick")
      (let* ((buffer (jabber-muc-find-buffer room jc))
             (insert (symbol-function 'jabber-chat-ewoc-enter)))
        (with-current-buffer buffer
          (when manual (setq jabber-muc--auto-configure manual)))
        (cl-letf (((symbol-function 'jabber-chat-ewoc-enter)
                   (lambda (&rest args)
                     (prog1 (apply insert args)
                       (setq jabber-buffer-connection other
                             jabber-group "another@example.org")))))
          (jabber-test-muc-native-201 jc room))
        (should (= (length config) 1))
        (should (eq (caar config) jc))
        (should (equal (cadar config) room))
        (should (equal (nth 4 (car config)) (list buffer room)))
        (should-not (nth 5 (car config)))))))

(ert-deftest jabber-test-muc-native-room-setup-collision ()
  "Setup reentry cannot install a foreign arm before a final rebind."
  (jabber-test-muc-with-native-room
    (let ((setup (symbol-function 'jabber-chat-mode-setup)) foreign)
      (cl-letf (((symbol-function 'jabber-chat-mode-setup)
                 (lambda (&rest args)
                   (apply setup args)
                   (setq foreign (list 'jabber-muc--config-arm other room))
                   (setq jabber-muc--auto-configure foreign
                         jabber-buffer-connection other))))
        (should-error (jabber-muc-create jc room "nick") :type 'user-error))
      (with-current-buffer (get-buffer (jabber-muc-get-buffer room jc))
        (should (eq jabber-buffer-connection other))
        (should (eq jabber-muc--auto-configure foreign))
        (should-not jabber-buffer-registry--keys))
      (should-not sent)
      (should-not published))))

(ert-deftest jabber-test-muc-native-intent-joined-constructor-reentry ()
  "A joined room's yielding constructor cannot open a superseded intent."
  (jabber-test-muc-with-native-intent
    (jabber-muc-join-set room jc "joined")
    (let (effects)
      (cl-letf (((symbol-function 'jabber-muc-create-buffer)
                 (lambda (&rest _)
                   (jabber-muc-leave jc room)
                   (current-buffer)))
                ((symbol-function 'switch-to-buffer)
                 (lambda (&rest _) (push 'open effects)))
                ((symbol-function 'jabber-mam-muc-joined)
                 (lambda (&rest _) (push 'sync effects)))
                ((symbol-function 'jabber-muc--self-ping-one)
                 (lambda (&rest _) (push 'ping effects))))
        (jabber-muc-join jc room "joined" t))
      (should-not effects))))

(ert-deftest jabber-test-muc-native-constructor-entry-effects ()
  "Reject foreign permission before mode callbacks, not merely final rebind."
  (jabber-test-muc-with-native-room
    (let* ((buffer (get-buffer-create (jabber-muc-get-buffer room jc)))
           (arm (list 'jabber-muc--config-arm other room))
           entered
           (jabber-chat-mode-hook (list (lambda () (setq entered t)))))
      (with-current-buffer buffer
        (setq-local jabber-muc--auto-configure arm)
        (setq-local jabber-buffer-connection other))
      (condition-case nil
          (jabber-muc-create-buffer jc room)
        (user-error nil))
      (should-not entered)
      (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
      (should (eq other (buffer-local-value 'jabber-buffer-connection buffer)))
      (should-not (buffer-local-value 'jabber-buffer-registry--keys buffer)))))

(ert-deftest jabber-test-muc-native-create-arm-unwind ()
  "Old cancellation, error and quit preserve a new create's exact permission."
  (dolist (successor '(nil completed))
    (dolist (condition '(nil error quit))
      (jabber-test-muc-with-native-room
        (let ((send (symbol-function 'jabber-send-sexp))
              fired new-arm new-owner caught)
          (cl-letf (((symbol-function 'jabber-send-sexp)
                     (lambda (account stanza)
                       (funcall send account stanza)
                       (unless fired
                         (setq fired t)
                         (when successor
                           (jabber-muc-create jc room "successor")
                           (setq new-arm
                                 (buffer-local-value 'jabber-muc--auto-configure
                                                     (jabber-muc-find-buffer room jc))))
                         (when condition (signal condition '("old-create" detail)))))))
            (condition-case err
                (jabber-muc-create jc room "old")
              ((error quit) (setq caught err))))
          (should (equal caught (and condition (list condition "old-create" 'detail))))
          (with-current-buffer (jabber-muc-find-buffer room jc)
            (if successor
                (should (eq jabber-muc--auto-configure new-arm))
              (should (eq (not jabber-muc--auto-configure) (and condition t)))))
          (should (eq (cdr (assoc room (get jc 'jabber-muc--join-intents))) new-owner))
          (should (= (length published) (if successor 1 (if condition 0 1)))))))))

(ert-deftest jabber-test-muc-native-consumed-unwind ()
  "Consumed permissions stay consumed across later native cancellation."
  (dolist (action '(join leave reset error quit))
    (jabber-test-muc-with-native-room
      (let ((send (symbol-function 'jabber-send-sexp)) fired caught)
        (cl-letf (((symbol-function 'jabber-send-sexp)
                   (lambda (account stanza)
                     (funcall send account stanza)
                     (unless fired
                       (setq fired t)
                       (jabber-test-muc-native-201 jc room)
                       (pcase action
                         ('join (let ((jabber-muc-disable-disco-check t))
                                  (jabber-muc-join jc room "new")))
                         ('leave (jabber-muc-leave jc room))
                         ('reset (jabber-muc--session-reset jc))
                         (_ (signal action '("consumed" detail))))))))
          (condition-case err
              (jabber-muc-create jc room "old")
            ((error quit) (setq caught err))))
        (should (equal caught (and (memq action '(error quit))
                                  (list action "consumed" 'detail))))
        (should (= (length config) 1))
        (should-not (nth 5 (car config)))
        (should-not published)
        (with-current-buffer (jabber-muc-find-buffer room jc)
          (should-not jabber-muc--auto-configure))
        (jabber-test-muc-native-201 jc room)
        (should (= (length config) 1))))))

(ert-deftest jabber-test-muc-native-permission-invalidation ()
  "Ordinary join, leave and reset clear only matching package permissions."
  (dolist (kind '(nil t manual owned foreign-account foreign-room))
    (dolist (action '(join leave reset))
      (jabber-test-muc-with-native-room
        (let* ((buffer (jabber-muc-create-buffer jc room))
               (arm (pcase kind
                      ('owned (list 'jabber-muc--config-arm jc room))
                      ('foreign-account (list 'jabber-muc--config-arm other room))
                      ('foreign-room (list 'jabber-muc--config-arm jc "else@example.org"))
                      (_ kind))))
          (with-current-buffer buffer (setq jabber-muc--auto-configure arm))
          (pcase action
            ('join
             (let ((jabber-muc-disable-disco-check t))
               (if (memq kind '(foreign-account foreign-room))
                   (should-error (jabber-muc-join jc room "new") :type 'user-error)
                 (jabber-muc-join jc room "new"))))
            ('leave (jabber-muc-leave jc room))
            ('reset (jabber-muc--session-reset jc)))
          (with-current-buffer buffer
            (should (eq jabber-muc--auto-configure
                        (if (or (eq kind 'owned)
                                (and (eq kind 'foreign-room) (eq action 'reset)))
                            nil arm)))))))))

(ert-deftest jabber-test-muc-native-scope-collisions ()
  "Rendered collisions reject ordinary popup joins without bookkeeping."
  (dolist (scope '(account equal-account room))
    (jabber-test-muc-with-native-room
      (let ((jabber-groupchat-buffer-format " *native-collision*"))
        (when (eq scope 'equal-account)
          (put other :state-data (copy-sequence (fsm-get-state-data jc))))
        (jabber-muc-create jc room "first")
        (let* ((target (if (eq scope 'room) "else@example.org" room))
               (account (if (eq scope 'room) jc other))
               (buffer (jabber-muc-find-buffer room jc))
               (arm (buffer-local-value 'jabber-muc--auto-configure buffer))
               (pending (copy-hash-table jabber-pending-groupchats))
               (passwords (copy-hash-table jabber-muc--session-passwords))
               opened)
          (cl-letf (((symbol-function 'switch-to-buffer)
                     (lambda (&rest _) (setq opened t))))
            (should-error
             (jabber-muc--send-join-presence account target "second" "secret" t)
             :type 'user-error)
            (should-error (jabber-muc-create account target "second") :type 'user-error))
          (should-not opened)
          (dolist (tables (list (cons pending jabber-pending-groupchats)
                                (cons passwords jabber-muc--session-passwords)))
            (should (= (hash-table-count (car tables)) (hash-table-count (cdr tables))))
            (maphash (lambda (key value)
                       (should (equal value (gethash key (cdr tables) :missing))))
                     (car tables)))
          (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
          (should-not (get account 'jabber-muc--join-intents))
          (should (= (length sent) 1))
          (should (= (length published) 1)))))))

(ert-deftest jabber-test-muc-native-default-scope-progress ()
  "Default names separate active accounts and ordinary popup joins still open."
  (jabber-test-muc-with-native-room
    (let ((jabber-groupchat-buffer-format (default-value 'jabber-groupchat-buffer-format)))
      (jabber-muc-create jc room "first")
      (jabber-muc-create other room "second")
      (should-not (eq (jabber-muc-find-buffer room jc) (jabber-muc-find-buffer room other)))
      (jabber-test-muc-native-201 jc room)
      (jabber-test-muc-native-201 other room)
      (should (equal (mapcar #'car (reverse config)) (list jc other)))
      (should (= (length published) 2))
      (jabber-muc--send-join-presence jc "ordinary@example.org" "nick" nil t)
      (should (eq (current-buffer) (jabber-muc-find-buffer "ordinary@example.org" jc))))))

(ert-deftest jabber-test-muc-native-setup-successor ()
  "Same-scope setup reentry leaves only the nested create's send and arm."
  (jabber-test-muc-with-native-room
    (let ((setup (symbol-function 'jabber-chat-mode-setup)) fired arm)
      (cl-letf (((symbol-function 'jabber-chat-mode-setup)
                 (lambda (&rest args)
                   (apply setup args)
                   (unless fired
                     (setq fired t)
                     (jabber-muc-create jc room "new")
                     (setq arm jabber-muc--auto-configure)))))
        (jabber-muc-create jc room "old"))
      (should fired)
      (should (= (length sent) 1))
      (should (equal (jabber-xml-get-attribute (cadar sent) 'to) (concat room "/new")))
      (should (= (length published) 1))
      (should-not (get jc 'jabber-muc--join-intents))
      (with-current-buffer (jabber-muc-find-buffer room jc)
        (should (eq arm jabber-muc--auto-configure))))))

(ert-deftest jabber-test-muc-native-config-callback-progress ()
  "Immediate config results retain authority when real form UI reenters."
  (dolist (outcome '(success error join create))
    (jabber-test-muc-with-native-room
      (let ((send (symbol-function 'jabber-send-sexp))
            (iq (symbol-function 'jabber-send-iq))
            (open (symbol-function 'jabber-xdata-form-open))
            fired form-opened effects)
        (cl-letf (((symbol-function 'jabber-send-sexp)
                   (lambda (account stanza)
                     (funcall send account stanza)
                     (push (list 'send account (jabber-xml-get-attribute stanza 'to)) effects)
                     (unless fired
                       (setq fired t)
                       (jabber-test-muc-native-201 jc room))))
                  ((symbol-function 'jabber-send-iq)
                   (lambda (account target type query callback context &rest rest)
                     (apply iq account target type query callback context rest)
                     (when (equal (jabber-xml-get-attribute query 'xmlns) jabber-muc-xmlns-owner)
                       (should-not jabber-muc--auto-configure)
                       (push (list 'config account target) effects)
                       (if (eq outcome 'error)
                           (funcall (car rest) account
                                    '(iq ((type . "error"))
                                         (error ((type . "cancel"))
                                                (forbidden ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))
                                    (cadr rest))
                         (funcall callback account
                                  `(iq ((type . "result"))
                                       (query ((xmlns . ,jabber-muc-xmlns-owner))
                                              (x ((xmlns . ,jabber-xdata-xmlns) (type . "form"))
                                                 (field ((var . "name") (type . "text-single"))
                                                        (value () "Room")))))
                                  context)))))
                  ((symbol-function 'jabber-muc--room-created-message)
                   (lambda () (ert-fail "Automatic configuration fell back to a manual notice")))
                  ((symbol-function 'jabber-xdata-form-open)
                   (lambda (&rest args)
                     (prog1 (apply open args)
                       (setq form-opened t)
                       (push (list 'form jc room) effects)
                       (pcase outcome
                         ('join (let ((jabber-muc-disable-disco-check t))
                                  (jabber-muc-join jc room "new")))
                         ('create (jabber-muc-create jc room "new")))))))
          (jabber-muc-create jc room "old"))
        (should (eq form-opened (not (eq outcome 'error))))
        (should (equal (seq-take (reverse effects) 2)
                       (list (list 'send jc (concat room "/old")) (list 'config jc room))))
        (should (= (length config) 1))
        (should (eq (caar config) jc))
        (should (equal (cadar config) room))
        (should (= (length published) (if (eq outcome 'join) 0 1)))
        (when (eq outcome 'create)
          (should (equal (nth 2 (car published)) "new")))
        (with-current-buffer (jabber-muc-find-buffer room jc)
          (if (eq outcome 'create)
              (should (eq (car-safe jabber-muc--auto-configure) 'jabber-muc--config-arm))
            (should-not jabber-muc--auto-configure)))))))

(ert-deftest jabber-test-muc-native-inverse-event-origin ()
  "Foreign events cannot steal an arm by rebinding locals during notices."
  (dolist (phase '(delta status))
    (dolist (manual '(nil t manual))
      (jabber-test-muc-with-native-room
        (jabber-muc-create jc room "old")
        (let* ((buffer (jabber-muc-find-buffer room jc))
               (arm (buffer-local-value 'jabber-muc--auto-configure buffer))
               (insert (symbol-function 'jabber-chat-ewoc-enter))
               (notice (symbol-function 'jabber-muc--insert-notice))
               rebound effects)
          (jabber-buffer-registry-register 'muc (jabber-muc--buffer-key other room) buffer)
          (with-current-buffer buffer
            (setq jabber-buffer-connection other)
            (when manual (setq jabber-muc--auto-configure manual)))
          (cl-labels ((rebind ()
                        (setq rebound t jabber-buffer-connection jc
                              jabber-group "wrong@example.org")
                        (push (list 'rebind other room) effects)))
            (cl-letf (((symbol-function 'jabber-chat-ewoc-enter)
                       (lambda (&rest args)
                         (prog1 (apply insert args)
                           (when (eq phase 'delta) (rebind)))))
                      ((symbol-function 'jabber-muc--insert-notice)
                       (lambda (&rest args)
                         (prog1 (apply notice args)
                           (when (eq phase 'status) (rebind))))))
              (jabber-muc-process-presence
               other `(presence ((from . ,(concat room "/nick")))
                                (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                                   (item ((affiliation . "owner") (role . "moderator")))
                                   (status ((code . "110")))
                                   (status ((code . "170")))
                                   (status ((code . "201"))))))))
          (should rebound)
          (should effects)
          (if manual
              (progn
                (should (= (length config) 1))
                (should (eq (caar config) other))
                (should (equal (cadar config) room))
                (should (equal (nth 4 (car config)) (list buffer room)))
                (should-not (nth 5 (car config))))
            (should-not config)
            (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))))))))

(ert-deftest jabber-test-muc-native-pending-create-arm-cleanup ()
  "Stale cleanup while a new create is still sending preserves its owner."
  (jabber-test-muc-with-native-room
    (let ((send (symbol-function 'jabber-send-sexp)) old checked)
      (cl-letf (((symbol-function 'jabber-send-sexp)
                 (lambda (account stanza)
                   (funcall send account stanza)
                   (if (null old)
                       (progn
                         (setq old (cdr (assoc room (get jc 'jabber-muc--join-intents))))
                         (jabber-muc-create jc room "new"))
                     (let* ((owner (cdr (assoc room (get jc 'jabber-muc--join-intents))))
                            (buffer (jabber-muc-find-buffer room jc))
                            (arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
                       (should-not (eq old owner))
                       (should-not (eq (aref old 6) arm))
                       ;; Exercise the actual stale cleanup boundary while the
                       ;; successor's public sender has not returned.
                       (should (eq (jabber-muc--with-intent old (ert-fail "Stale body"))
                                   :cancelled))
                       (should (eq owner (cdr (assoc room (get jc 'jabber-muc--join-intents)))))
                       (should (eq arm (buffer-local-value 'jabber-muc--auto-configure buffer)))
                       (setq checked t))))))
        (jabber-muc-create jc room "old"))
      (should checked)
      (should (= (length published) 1))
      (should-not (get jc 'jabber-muc--join-intents)))))

(ert-deftest jabber-test-muc-native-manual-error-unwind ()
  "Error and quit never claim or erase pre-existing manual permissions."
  (dolist (manual '(t manual))
    (dolist (condition '(error quit))
      (jabber-test-muc-with-native-room
        (let ((buffer (jabber-muc-create-buffer jc room)) caught)
          (with-current-buffer buffer (setq jabber-muc--auto-configure manual))
          (cl-letf (((symbol-function 'jabber-send-sexp)
                     (lambda (&rest _) (signal condition '("manual" detail)))))
            (condition-case err
                (jabber-muc-create jc room "old")
              ((error quit) (setq caught err))))
          (should (equal caught (list condition "manual" 'detail)))
          (should (eq manual (buffer-local-value 'jabber-muc--auto-configure buffer)))
          (should-not published)
          (should-not (get jc 'jabber-muc--join-intents)))))))

(ert-deftest jabber-test-muc-native-status-event-origin ()
  "Matching and manual permissions use event authority after status notices."
  (dolist (manual '(nil t manual))
    (jabber-test-muc-with-native-room
      (jabber-muc-create jc room "old")
      (let ((buffer (jabber-muc-find-buffer room jc))
            (insert (symbol-function 'jabber-muc--insert-notice))
            (iq (symbol-function 'jabber-send-iq))
            effects)
        (with-current-buffer buffer
          (when manual (setq jabber-muc--auto-configure manual)))
        (cl-letf (((symbol-function 'jabber-muc--insert-notice)
                   (lambda (&rest args)
                     (prog1 (apply insert args)
                       (setq jabber-buffer-connection other jabber-group "wrong@example.org")
                       (push (list 'notice jc room) effects))))
                  ((symbol-function 'jabber-send-iq)
                   (lambda (account target type query &rest args)
                     (when (equal (jabber-xml-get-attribute query 'xmlns) jabber-muc-xmlns-owner)
                       (push (list 'config account target) effects))
                     (apply iq account target type query args))))
          (jabber-muc-process-presence
           jc `(presence ((from . ,(concat room "/nick")))
                         (x ((xmlns . "http://jabber.org/protocol/muc#user"))
                            (item ((affiliation . "owner") (role . "moderator")))
                            (status ((code . "110")))
                            (status ((code . "170")))
                            (status ((code . "201")))))))
        (should (equal (reverse effects)
                       (list (list 'notice jc room) (list 'config jc room))))
        (should (= (length config) 1))
        (should (equal (nth 4 (car config)) (list buffer room)))
        (should-not (nth 5 (car config)))))))

(ert-deftest jabber-test-muc-native-discovery-submission-unwind ()
  "Submission failures propagate intact and cannot leave a live callback."
  (dolist (condition '(error quit))
    (jabber-test-muc-with-native-intent
      (let ((submit (symbol-function 'jabber-disco-get-info)) caught)
        (cl-letf (((symbol-function 'jabber-disco-get-info)
                   (lambda (&rest args)
                     (apply submit args)
                     (signal condition '("submission" detail)))))
          (condition-case err
              (jabber-muc-join jc room "old")
            ((error quit) (setq caught err))))
        (should (equal caught (list condition "submission" 'detail)))
        (should-not (get jc 'jabber-muc--join-intents))
        (funcall (car replies) nil)
        (should-not sent)
        (should-not (gethash (jabber-jid-symbol room) jabber-pending-groupchats))))))

(ert-deftest jabber-test-muc-native-repeated-discovery ()
  "A completed native closure stays inert even after a new reservation."
  (jabber-test-muc-with-native-intent
    (jabber-muc-join jc room "old")
    (funcall (car replies) nil)
    (funcall (car replies) nil)
    (should (= (length sent) 1))
    (jabber-muc-join jc room "new")
    (let ((owner (cdr (assoc room (get jc 'jabber-muc--join-intents)))))
      (funcall (car replies) nil)
      (should (eq owner (cdr (assoc room (get jc 'jabber-muc--join-intents))))))
    (funcall (cadr replies) nil)
    (funcall (cadr replies) nil)
    (should (equal (reverse sent)
                   (list (list jc "room@example.org/old" nil)
                         (list jc "room@example.org/new" nil))))))

(ert-deftest jabber-test-muc-native-eligibility-yields ()
  "Loss of active eligibility fences discovery, prompts and construction."
  (let ((dispatcher (symbol-function 'jabber-presence-children)))
    (dolist (phase '(discovery password create presence))
      (dolist (action '(remove terminal register))
        (jabber-test-muc-with-native-intent
          (cl-labels ((invalidate ()
                        (pcase action
                          ('remove (setq jabber-connections (delq jc jabber-connections)))
                          ('terminal (put jc :state :disconnected))
                          ('register (put jc :state-data
                                          (plist-put (fsm-get-state-data jc) :registerp t))))))
            (let ((jabber-silent-mode nil)
                  (jabber-presence-element-functions
                   (list (lambda (_) (invalidate) nil))))
              (cl-letf (((symbol-function 'read-passwd)
                         (lambda (&rest _) (invalidate) "secret"))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _) (invalidate) t))
                        ((symbol-function 'jabber-presence-children)
                         (if (eq phase 'presence) dispatcher #'ignore)))
                (jabber-muc-join jc room "old")
                (when (eq phase 'discovery) (invalidate))
                (funcall (car replies)
                         (pcase phase
                           ('password '(:status ok :features ("muc_passwordprotected" "muc_nonanonymous")))
                           ('create '(:status not-found))
                           (_ nil))))))
          (should-not sent)
          (should-not (get jc 'jabber-muc--join-intents))
          (should-not (gethash room jabber-muc--nonanonymous-rooms))
          (should-not (jabber-muc--session-password jc room))
          (should-not (gethash (jabber-jid-symbol room) jabber-pending-groupchats)))))))

(ert-deftest jabber-test-muc-native-create-prompt-outcomes ()
  "Silent creation bypasses prompting; decline and quit never hand off."
  (dolist (outcome '(silent accept decline error quit))
    (jabber-test-muc-with-native-intent
      (let ((jabber-silent-mode (eq outcome 'silent)) prompted caught)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq prompted t)
                     (pcase outcome
                       ((or 'error 'quit) (signal outcome '("create-prompt" detail)))
                       (_ (eq outcome 'accept))))))
          (jabber-muc-join jc room "nick")
          (condition-case err
              (funcall (car replies) '(:status not-found))
            ((error quit) (setq caught err))))
        (should (eq prompted (not (eq outcome 'silent))))
        (should (equal caught
                       (pcase outcome
                         ('decline '(error "Non-existent groupchat"))
                         ((or 'error 'quit) (list outcome "create-prompt" 'detail)))))
        (should (= (length sent) (if (memq outcome '(silent accept)) 1 0)))
        (should-not (get jc 'jabber-muc--join-intents))
        (let ((before (copy-tree sent)))
          (funcall (car replies) '(:status not-found))
          (should (equal sent before)))))))

(ert-deftest jabber-test-muc-native-legacy-publication ()
  "Legacy transport and success retain native ownership after publication."
  (require 'jabber-bookmarks)
  (let ((publish (symbol-function 'jabber-bookmarks--publish-one)))
    (dolist (phase '(nil presence send buffer switch publication))
      (jabber-test-muc-with-native-room
        (let ((jabber-bookmarks (make-hash-table :test #'equal))
              (jabber-bookmarks--legacy-accounts (make-hash-table :test #'equal))
              (jabber-muc-disable-disco-check t)
              (send (symbol-function 'jabber-send-sexp))
              (create (symbol-function 'jabber-muc-create-buffer))
              (switch (symbol-function 'switch-to-buffer))
              callbacks storage interrupted)
          (puthash (jabber-connection-bare-jid jc) t jabber-bookmarks--legacy-accounts)
          (cl-labels ((interrupt (where)
                        (when (and (eq where phase) (not interrupted))
                          (setq interrupted t)
                          (jabber-muc-join jc room "new"))))
            (let ((jabber-presence-element-functions
                   (list (lambda (_) (interrupt 'presence) nil))))
              (cl-letf (((symbol-function 'jabber-bookmarks--publish-one) publish)
                        ((symbol-function 'jabber-bookmarks--refresh-buffer) #'ignore)
                        ((symbol-function 'jabber-private-set)
                         (lambda (account xml callback context &rest _)
                           (push (list account xml) storage)
                           (push (lambda () (funcall callback account nil context)) callbacks)
                           (interrupt 'publication)))
                        ((symbol-function 'jabber-send-sexp)
                         (lambda (&rest args) (prog1 (apply send args) (interrupt 'send))))
                        ((symbol-function 'jabber-muc-create-buffer)
                         (lambda (&rest args) (prog1 (apply create args) (interrupt 'buffer))))
                        ((symbol-function 'switch-to-buffer)
                         (lambda (&rest args) (prog1 (apply switch args) (interrupt 'switch)))))
                (jabber-muc-create jc room "old")
                (should (= (length storage) (if (memq phase '(nil publication)) 1 0)))
                (when storage
                  (should (eq (caar storage) jc))
                  (should (equal (cadar storage)
                                 `(storage ((xmlns . ,jabber-bookmarks-xmlns))
                                           (conference ((jid . ,room) (autojoin . "1"))
                                                       (nick () "old"))))))
                ;; Complete the real legacy save-all/publish-one/maybe-join chain.
                (dolist (callback callbacks) (funcall callback))
                (should (equal (mapcar (lambda (entry)
                                         (jabber-xml-get-attribute (cadr entry) 'to))
                                       (reverse sent))
                               (pcase phase
                                 ('nil (list (concat room "/old") (concat room "/old")))
                                 ((or 'presence 'buffer) (list (concat room "/new")))
                                 ('publication (list (concat room "/old") (concat room "/new")
                                                     (concat room "/old")))
                                 (_ (list (concat room "/old") (concat room "/new")))))))))
          (should-not (get jc 'jabber-muc--join-intents)))))))

(provide 'jabber-test-muc)
;;; jabber-test-muc.el ends here
