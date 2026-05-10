;;; jabber-moderation-tests.el --- Tests for XEP-0425 moderation  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ewoc)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-moderation)

;;; Test helpers

(defmacro jabber-moderation-test-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

;;; Group 1: server-id indexing

(ert-deftest jabber-moderation-test-server-id-indexed ()
  "Ewoc hash stores and retrieves by :server-id."
  (jabber-moderation-test-with-ewoc
    (let* ((msg (list :id "client-1" :server-id "server-abc"
                      :body "hello" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :muc-foreign msg))))
      (should (eq node (jabber-chat-ewoc-find-by-id "client-1")))
      (should (eq node (jabber-chat-ewoc-find-by-id "server-abc"))))))

(ert-deftest jabber-moderation-test-server-id-nil-no-index ()
  "A nil :server-id does not pollute the hash table."
  (jabber-moderation-test-with-ewoc
    (let ((msg (list :id "client-2" :body "x" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    (should (= 1 (hash-table-count jabber-chat--msg-nodes)))
    (should (gethash "client-2" jabber-chat--msg-nodes))))

;;; Group 2: retraction handling

(ert-deftest jabber-moderation-test-retract-updates-ewoc ()
  "Retraction stanza sets :retracted on the original message."
  (jabber-moderation-test-with-ewoc
    ;; Insert a message with a server-id
    (let ((msg (list :id "msg-1" :server-id "stanza-id-1"
                     :from "room@muc.example.com/alice"
                     :body "spam" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    ;; Name the buffer so jabber-muc-find-buffer can find it
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) buf))
                ((symbol-function 'jabber-db-retract-message)
                 #'ignore))
        ;; Simulate retraction stanza; <reason> is a child of <retract>
        ;; per XEP-0425, not of <moderated>.
        (let ((retract-xml
               '(message ((from . "room@muc.example.com")
                          (type . "groupchat"))
                         (retract ((id . "stanza-id-1")
                                   (xmlns . "urn:xmpp:message-retract:1"))
                                  (moderated ((by . "room@muc.example.com/admin")
                                              (xmlns . "urn:xmpp:message-moderate:1")))
                                  (reason () "spam")))))
          (jabber-moderation--handle-message nil retract-xml)))
      ;; Verify the plist was mutated
      (let* ((node (jabber-chat-ewoc-find-by-id "stanza-id-1"))
             (data (ewoc-data node))
             (msg (cadr data)))
        (should (plist-get msg :retracted))
        (should (equal "room@muc.example.com/admin"
                       (plist-get msg :retracted-by)))
        (should (equal "spam" (plist-get msg :retraction-reason)))))))

;;; Group 3: stanza-id source validation

(ert-deftest jabber-moderation-test-rejects-client-id ()
  "Retraction targeting a client message-id (not server stanza-id) is ignored."
  (jabber-moderation-test-with-ewoc
    (let ((msg (list :id "client-id-1" :server-id "server-stanza-id-1"
                     :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) buf))
                ((symbol-function 'jabber-db-retract-message)
                 #'ignore))
        ;; Use client-id-1, not server-stanza-id-1 -- MUST be ignored
        (let ((retract-xml
               '(message ((from . "room@muc.example.com")
                          (type . "groupchat"))
                         (retract ((id . "client-id-1")
                                   (xmlns . "urn:xmpp:message-retract:1"))
                                  (moderated ((by . "room@muc.example.com/admin")
                                              (xmlns . "urn:xmpp:message-moderate:1")))))))
          ;; Returns t (consumed by chain) but must not mutate the message
          (jabber-moderation--handle-message nil retract-xml))))
    (let* ((node (jabber-chat-ewoc-find-by-id "client-id-1"))
           (msg (cadr (ewoc-data node))))
      (should-not (plist-get msg :retracted)))))

;;; Group 4: sender validation


(ert-deftest jabber-moderation-test-validates-sender ()
  "Retraction from a participant (not MUC service) is ignored."
  (jabber-moderation-test-with-ewoc
    (let ((msg (list :id "msg-2" :server-id "stanza-id-2"
                     :from "room@muc.example.com/alice"
                     :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    ;; Retraction from a full JID (has resource) should be rejected
    (let ((retract-xml
           '(message ((from . "room@muc.example.com/mallory")
                      (type . "groupchat"))
                     (retract ((id . "stanza-id-2")
                               (xmlns . "urn:xmpp:message-retract:1"))
                              (moderated ((by . "room@muc.example.com/mallory")
                                          (xmlns . "urn:xmpp:message-moderate:1")))))))
      (should-not (jabber-moderation--handle-message nil retract-xml)))
    ;; Original message should be untouched
    (let* ((node (jabber-chat-ewoc-find-by-id "stanza-id-2"))
           (msg (cadr (ewoc-data node))))
      (should-not (plist-get msg :retracted)))))

(ert-deftest jabber-moderation-test-non-groupchat-ignored ()
  "Retraction in a non-groupchat message is ignored."
  (let ((retract-xml
         '(message ((from . "room@muc.example.com")
                    (type . "chat"))
                   (retract ((id . "stanza-id-3")
                             (xmlns . "urn:xmpp:message-retract:1"))
                            (moderated ((by . "room@muc.example.com/admin")
                                        (xmlns . "urn:xmpp:message-moderate:1")))))))
    (should-not (jabber-moderation--handle-message nil retract-xml))))

;;; Group 5: missing message

(ert-deftest jabber-moderation-test-missing-message-ignored ()
  "Retraction for unknown stanza-id doesn't error."
  (jabber-moderation-test-with-ewoc
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group) buf))
                ((symbol-function 'jabber-db-retract-message)
                 #'ignore))
        (let ((retract-xml
               '(message ((from . "room@muc.example.com")
                          (type . "groupchat"))
                         (retract ((id . "nonexistent-id")
                                   (xmlns . "urn:xmpp:message-retract:1"))
                                  (moderated ((by . "room@muc.example.com/admin")
                                              (xmlns . "urn:xmpp:message-moderate:1")))))))
          ;; Should return t (consumed) but not error
          (should (jabber-moderation--handle-message nil retract-xml)))))))

;;; Group 6: tombstone rendering

(ert-deftest jabber-moderation-test-tombstone-rendering ()
  "Tombstone text is inserted for retracted messages."
  (with-temp-buffer
    (let ((msg (list :body "spam" :retracted t
                     :retracted-by "room@muc.example.com/admin"
                     :retraction-reason "spam")))
      (jabber-chat--insert-tombstone msg)
      (should (string-match-p
               "Message retracted by: admin reason: spam"
               (buffer-string))))))

(ert-deftest jabber-moderation-test-tombstone-no-reason ()
  "Tombstone without reason omits the reason part."
  (with-temp-buffer
    (let ((msg (list :body "x" :retracted t
                     :retracted-by "room@muc.example.com/mod")))
      (jabber-chat--insert-tombstone msg)
      (let ((text (buffer-string)))
        (should (string-match-p "Message retracted by: mod" text))
        (should-not (string-match-p "reason:" text))))))

;;; Group 7: build-msg-plist extracts server-id

(ert-deftest jabber-moderation-test-plist-extracts-server-id ()
  "jabber-chat--build-msg-plist extracts :server-id from stanza-id element."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (id . "client-id")
                            (type . "groupchat"))
                           (body () "hello")
                           (stanza-id ((id . "server-id-42")
                                       (by . "room@muc.example.com")
                                       (xmlns . "urn:xmpp:sid:0")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "server-id-42" (plist-get plist :server-id)))))

(ert-deftest jabber-moderation-test-plist-nil-server-id ()
  "jabber-chat--build-msg-plist returns nil :server-id when absent."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :server-id))))

;;; Group 8: retract command

(ert-deftest jabber-moderation-test-retract-sends-iq ()
  "jabber-moderation-retract sends correct IQ XML."
  (jabber-moderation-test-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let* ((msg (list :id "msg-r1" :server-id "sid-retract"
                      :body "spam" :timestamp (current-time)))
           (sent-iq nil))
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'jabber-send-iq)
                 (lambda (_jc to type query &rest _rest)
                   (setq sent-iq (list :to to :type type :query query))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "test reason"))
                ((symbol-function 'jabber-db-retract-message)
                 #'ignore))
        (jabber-moderation-retract))
      (should (equal "room@muc.example.com" (plist-get sent-iq :to)))
      (should (equal "set" (plist-get sent-iq :type)))
      (let ((query (plist-get sent-iq :query)))
        (should (eq 'moderate (car query)))
        (should (equal "sid-retract"
                       (cdr (assq 'id (cadr query)))))
        (should (equal jabber-moderation-xmlns
                       (cdr (assq 'xmlns (cadr query)))))
        ;; Check retract child
        (let ((retract (nth 2 query)))
          (should (eq 'retract (car retract)))
          (should (equal jabber-moderation-retract-xmlns
                         (cdr (assq 'xmlns (cadr retract))))))
        ;; Check reason child
        (let ((reason (nth 3 query)))
          (should (eq 'reason (car reason)))
          (should (equal "test reason" (nth 2 reason))))))))

(ert-deftest jabber-moderation-test-retract-errors-without-server-id ()
  "jabber-moderation-retract signals error when no server-id."
  (jabber-moderation-test-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((msg (list :id "msg-r2" :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (goto-char (point-min))
      (should-error (jabber-moderation-retract) :type 'user-error))))

(ert-deftest jabber-moderation-test-retract-errors-outside-muc ()
  "jabber-moderation-retract signals error outside MUC buffer."
  (with-temp-buffer
    (should-error (jabber-moderation-retract) :type 'user-error)))

(provide 'jabber-moderation-tests)

;;; jabber-moderation-tests.el ends here
