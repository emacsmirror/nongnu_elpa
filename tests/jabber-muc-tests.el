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

(provide 'jabber-muc-tests)
;;; jabber-muc-tests.el ends here
