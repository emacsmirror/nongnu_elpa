;;; jabber-test-mam-room-identity.el --- Room archive identity tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Replay idless forwarded room messages against stable live room identities.

;;; Code:

(require 'jabber-archive-test-helpers)

(defun jabber-test-mam-room--message (&optional room-id transport-id)
  "Return a room message with optional ROOM-ID and TRANSPORT-ID."
  `(message ((from . "room@conference.example.com/nick")
             (type . "groupchat")
             ,@(when transport-id `((id . ,transport-id))))
            (body () "A repeatable message")
            ,@(when room-id
                `((stanza-id ((xmlns . "urn:xmpp:sid:0")
                              (by . "room@conference.example.com")
                              (id . ,room-id)))))))

(defun jabber-test-mam-room--live (jc uid &optional transport-id)
  "Store a live message on JC with room UID and TRANSPORT-ID."
  (let ((xml (jabber-test-mam-room--message uid transport-id)))
    (jabber-db-store-message
     (jabber-connection-bare-jid jc) "room@conference.example.com"
     "in" "groupchat" "A repeatable message" 1735689600 "nick"
     transport-id uid nil nil nil nil nil
     (jabber-db--message-identity xml))))

(defun jabber-test-mam-room--query (jc &optional archive)
  "Start an owned room query on JC against ARCHIVE."
  (jabber-mam--query jc nil nil nil nil
                     (or archive "room@conference.example.com")))

(defun jabber-test-mam-room--rows ()
  "Return stored room identity metadata, never message bodies."
  (sqlite-select jabber-db--connection
                 "SELECT account, room_id, server_id, stanza_id FROM message ORDER BY id"))

(ert-deftest jabber-test-mam-room-uid-live-first ()
  "An owned room UID reconciles an idless archive after live delivery."
  (jabber-test-archive--with-file
    (let* ((id (jabber-test-mam-room--live jc "room-uid" "transport"))
           (query (jabber-test-mam-room--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "room-uid"
                                     (jabber-test-mam-room--message)))
      (should (= 1 (length (jabber-test-mam-room--rows))))
      (should (equal (sqlite-select jabber-db--connection
                                   "SELECT message_id FROM message_archive")
                     (list (list id)))))))

(ert-deftest jabber-test-mam-room-uid-archive-first ()
  "An idless owned archive row acquires the later live transport ID."
  (jabber-test-archive--with-file
    (let ((query (jabber-test-mam-room--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "room-uid"
                                     (jabber-test-mam-room--message)))
      (jabber-test-mam-room--live jc "room-uid" "transport")
      (should (equal (jabber-test-mam-room--rows)
                     '(("me@example.com" "room-uid" "room-uid" "transport")))))))

(ert-deftest jabber-test-mam-room-uid-wrong-archive ()
  "A foreign room or personal archive UID is not the sender room's ID."
  (dolist (archive '("other@conference.example.com" "me@example.com"))
    (jabber-test-archive--with-file
      (jabber-test-mam-room--live jc "room-uid")
      (let ((query (jabber-test-mam-room--query jc archive)))
        (jabber-mam--process-message
         jc (jabber-test-archive--result query "room-uid"
                                       (jabber-test-mam-room--message)))
        (should (= 2 (length (jabber-test-mam-room--rows))))
        (should-not (nth 1 (cadr (jabber-test-mam-room--rows))))))))

(ert-deftest jabber-test-mam-room-uid-explicit-conflict ()
  "An explicit different room stanza ID is never replaced by the UID."
  (jabber-test-archive--with-file
    (jabber-test-mam-room--live jc "room-uid")
    (let ((query (jabber-test-mam-room--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "room-uid"
                                     (jabber-test-mam-room--message "explicit")))
      (should (= 2 (length (jabber-test-mam-room--rows))))
      (should (equal (nth 1 (cadr (jabber-test-mam-room--rows))) "explicit")))))

(ert-deftest jabber-test-mam-room-uid-transport-conflict ()
  "Matching room UID does not override conflicting transport evidence."
  (jabber-test-archive--with-file
    (jabber-test-mam-room--live jc "room-uid" "live-transport")
    (let ((query (jabber-test-mam-room--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result
           query "room-uid" (jabber-test-mam-room--message nil "archive-transport")))
      (should (= 2 (length (jabber-test-mam-room--rows)))))))

(ert-deftest jabber-test-mam-room-uid-legitimate-repeats ()
  "Same sender, body and timestamp with distinct UIDs remain distinct."
  (jabber-test-archive--with-file
    (let ((query (jabber-test-mam-room--query jc)))
      (dolist (uid '("first" "second"))
        (jabber-mam--process-message
         jc (jabber-test-archive--result query uid (jabber-test-mam-room--message)))
        (jabber-test-mam-room--live jc uid))
      (should (equal (jabber-test-mam-room--rows)
                     '(("me@example.com" "first" "first" nil)
                       ("me@example.com" "second" "second" nil)))))))

(ert-deftest jabber-test-mam-room-uid-account-scope ()
  "Identical room UIDs on different local accounts do not merge."
  (jabber-test-archive--with-file
    (jabber-test-mam-room--live jc "room-uid")
    (let* ((jc (jabber-test-mam--native-connection "other"))
           (query (jabber-test-mam-room--query jc)))
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "room-uid" (jabber-test-mam-room--message)))
      (should (equal (mapcar #'car (jabber-test-mam-room--rows))
                     '("me@example.com" "other@example.com"))))))

(ert-deftest jabber-test-mam-room-uid-unowned-result ()
  "Wrong sender and retired session results cannot install room identity."
  (jabber-test-archive--with-file
    (let* ((query (jabber-test-mam-room--query jc))
           (xml (jabber-test-archive--result query "room-uid"
                                           (jabber-test-mam-room--message))))
      (setcdr (assq 'from (cadr xml)) "other@conference.example.com")
      (jabber-mam--process-message jc xml)
      (should-not (jabber-test-mam-room--rows))
      (setcdr (assq 'from (cadr xml)) "room@conference.example.com")
      (plist-put (get jc :state-data) :session-id "retired")
      (jabber-mam--process-message jc xml)
      (should-not (jabber-test-mam-room--rows)))))

(provide 'jabber-test-mam-room-identity)
;;; jabber-test-mam-room-identity.el ends here
