;;; jabber-muc-tests.el --- Tests for jabber-muc  -*- lexical-binding: t; -*-

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

(defmacro jabber-muc-test-with-rooms (rooms &rest body)
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

;;; Group 3: jabber-muc-private-message-p

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
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/othernick")
                          (type . "groupchat"))
                 (error ((type . "cancel"))))))
      (should (eq :muc-error
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "othernick" xml))))))

(ert-deftest jabber-test-muc-classify-message-local ()
  "Message from our own nick is classified as :muc-local."
  (jabber-muc-test-with-rooms
      '(("room@conference.example.com" . "mynick"))
    (let ((xml '(message ((from . "room@conference.example.com/mynick")
                          (type . "groupchat"))
                 (body nil "Hello"))))
      (should (eq :muc-local
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-foreign ()
  "Message from another nick is classified as :muc-foreign."
  (jabber-muc-test-with-rooms
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
                  (jabber-muc--classify-message nil
                   "room@conference.example.com" "mynick" xml))))))

(ert-deftest jabber-test-muc-classify-message-nil-nick ()
  "Nil nick (bare JID) classifies as :muc-foreign, not crash."
  (jabber-muc-test-with-rooms
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
          (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (_) buf))
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
          (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (_) buf))
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
          (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (_) buf))
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
          (cl-letf (((symbol-function 'jabber-muc-find-buffer) (lambda (_) buf))
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
               (lambda (_secs _repeat fn &rest args)
                 (when (eq fn #'jabber-muc--autojoin-next)
                   (setq timer-scheduled t))
                 'fake-timer))
              ((symbol-function 'jabber-mam-muc-joined) #'ignore)
              ((symbol-function 'jabber-bookmarks-auto-add-maybe) #'ignore)
              ((symbol-function 'jabber-muc-participant-plist) (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc-modify-participant) #'ignore)
              ((symbol-function 'jabber-muc-report-delta) (lambda (&rest _) nil))
              ((symbol-function 'jabber-muc-find-buffer) (lambda (_) nil)))
      (jabber-muc--process-enter
       'fake-jc "room@muc" "me"
       (jabber-jid-symbol "room@muc/me")
       (list jabber-muc-status-self-presence)
       '(x ((xmlns . "http://jabber.org/protocol/muc#user"))
           (item ((affiliation . "member") (role . "participant"))))
       nil nil "me"))
    (should timer-scheduled)))

(provide 'jabber-muc-tests)
;;; jabber-muc-tests.el ends here
