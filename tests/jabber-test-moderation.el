;;; jabber-test-moderation.el --- Tests for jabber-moderation  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0424/XEP-0425 Message Retraction and Moderation.

;;; Code:

(require 'ert)
(require 'ewoc)
(require 'jabber-chatbuffer)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-moderation)

;;; Test helpers

(defmacro jabber-test-moderation-with-ewoc (&rest body)
  "Set up a temp buffer with a chat ewoc and hash table, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((jabber-chat-ewoc (ewoc-create #'ignore nil nil 'nosep))
           (jabber-chat--msg-nodes (make-hash-table :test 'equal)))
       ,@body)))

;;; Group 1: server-id indexing

(ert-deftest jabber-test-moderation-server-id-indexed ()
  "Ewoc hash stores and retrieves by :server-id."
  (jabber-test-moderation-with-ewoc
    (let* ((msg (list :id "client-1" :server-id "server-abc"
                      :body "hello" :timestamp (current-time)))
           (node (jabber-chat-ewoc-enter (list :muc-foreign msg))))
      (should (eq node (jabber-chat-ewoc-find-by-id "client-1")))
      (should (eq node (jabber-chat-ewoc-find-by-id "server-abc"))))))

(ert-deftest jabber-test-moderation-server-id-nil-no-index ()
  "A nil :server-id does not pollute the hash table."
  (jabber-test-moderation-with-ewoc
    (let ((msg (list :id "client-2" :body "x" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    (should (= 1 (hash-table-count jabber-chat--msg-nodes)))
    (should (gethash "client-2" jabber-chat--msg-nodes))))

;;; Group 2: retraction handling

(ert-deftest jabber-test-moderation-retract-updates-ewoc ()
  "Retraction stanza sets :retracted on the original message."
  (jabber-test-moderation-with-ewoc
    ;; Insert a message with a server-id
    (let ((msg (list :id "msg-1" :server-id "stanza-id-1"
                     :from "room@muc.example.com/alice"
                     :body "spam" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    ;; Name the buffer so jabber-muc-find-buffer can find it
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group &optional _jc) buf))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@muc.example.com"))
                ((symbol-function 'jabber-db-retract-message-in-peer)
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
          (jabber-moderation--handle-message 'fake-jc retract-xml)))
      ;; Verify the plist was mutated
      (let* ((node (jabber-chat-ewoc-find-by-id "stanza-id-1"))
             (data (ewoc-data node))
             (msg (cadr data)))
        (should (plist-get msg :retracted))
        (should (equal "room@muc.example.com/admin"
                       (plist-get msg :retracted-by)))
        (should (equal "spam" (plist-get msg :retraction-reason)))))))

(ert-deftest jabber-test-moderation-author-retract-updates-exact-target ()
  "Author retraction updates its one occupant-matched target."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let* ((from "room@muc.example.com/alice")
           (msg (list :id "client-id-1" :server-id "server-id-1"
                      :from from :occupant-id "occupant-alice"
                      :body "mistake" :timestamp (current-time)))
           (buf (current-buffer))
           db-call)
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (cl-letf (((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) t))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-retraction-candidates)
                 (lambda (_account _room _server-id)
                   (list (list :row-id 42 :from from
                               :occupant-id "occupant-alice"))))
                ((symbol-function 'jabber-db-retract-message-row)
                 (lambda (&rest args) (setq db-call args)))
                ((symbol-function 'jabber-moderation--target-buffers-for-row)
                 (lambda (_jc _room _row-id) (list buf))))
        (should
         (jabber-moderation--handle-message
          'fake-jc
          `(message ((from . ,from) (type . "groupchat"))
                    (body () "Your contact attempted to retract a previous message, but it's unsupported by your client")
                    (retract ((id . "server-id-1")
                              (xmlns . ,jabber-moderation-retract-xmlns)))
                    (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                  (id . "occupant-alice")))))))
      (should (equal '(42 "room@muc.example.com/alice") db-call))
      (let ((updated (cadr (ewoc-data
                            (jabber-chat-ewoc-find-by-id "server-id-1")))))
        (should (plist-get updated :retracted))
        (should (equal from (plist-get updated :retracted-by)))))))

(ert-deftest jabber-test-moderation-author-retract-without-history ()
  "Live author retraction works when persistent history is disabled."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((jabber-db-path nil)
          (original
           '(message ((from . "room@muc.example.com/alice")
                      (id . "client-id-1")
                      (type . "groupchat"))
                     (body () "mistake")
                     (stanza-id ((id . "server-id-1")
                                 (by . "room@muc.example.com")
                                 (xmlns . "urn:xmpp:sid:0")))
                     (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                   (id . "occupant-alice")))))
          (retraction
           '(message ((from . "room@muc.example.com/alice")
                      (type . "groupchat"))
                     (body () "sender-controlled fallback")
                     (retract ((id . "server-id-1")
                               (xmlns . "urn:xmpp:message-retract:1")))
                     (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                   (id . "occupant-alice"))))))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_room &optional _jc) (current-buffer)))
                ((symbol-function 'jabber-muc-nickname)
                 (lambda (_room _jc) "me"))
                ((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) t)))
        (jabber-process-input 'fake-jc original)
        (jabber-process-input 'fake-jc retraction))
      (let ((msg (cadr (ewoc-data
                        (jabber-chat-ewoc-find-by-id "server-id-1")))))
        (should (equal "occupant-alice" (plist-get msg :occupant-id)))
        (should (plist-get msg :retracted)))
      (should
       (= 1
          (length
           (ewoc-collect jabber-chat-ewoc
                         (lambda (data)
                           (memq (car-safe data)
                                 '(:muc-local :muc-foreign))))))))))

(ert-deftest jabber-test-moderation-live-author-retract-updates-projections ()
  "One live target is tombstoned in its parent and thread views."
  (let ((parent (generate-new-buffer " *jabber-retract-parent*"))
        (thread (generate-new-buffer " *jabber-retract-thread*")))
    (unwind-protect
        (progn
          (dolist (entry (list (cons parent nil) (cons thread "thread-1")))
            (with-current-buffer (car entry)
              (setq-local jabber-group "room@muc.example.com")
              (setq-local jabber-buffer-connection 'fake-jc)
              (setq-local jabber-message-thread-id (cdr entry))
              (setq-local jabber-chat-ewoc
                          (ewoc-create #'ignore nil nil 'nosep))
              (setq-local jabber-chat--msg-nodes
                          (make-hash-table :test 'equal))
              (jabber-chat-ewoc-enter
               (list :muc-foreign
                     (list :server-id "server-id-1"
                           :from "room@muc.example.com/alice"
                           :occupant-id "occupant-alice"
                           :body "mistake" :timestamp (current-time))))))
          (let ((jabber-db-path nil))
            (cl-letf (((symbol-function
                        'jabber-moderation--room-supports-occupant-id-p)
                       (lambda (_room) t))
                      ((symbol-function 'jabber-connection-bare-jid)
                       (lambda (_jc) "me@example.com")))
              (should
               (jabber-moderation--handle-message
                'fake-jc
                '(message ((from . "room@muc.example.com/alice")
                           (type . "groupchat"))
                          (retract ((id . "server-id-1")
                                    (xmlns . "urn:xmpp:message-retract:1")))
                          (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                        (id . "occupant-alice"))))))))
          (dolist (buffer (list parent thread))
            (with-current-buffer buffer
              (should
               (plist-get
                (cadr (ewoc-data
                       (jabber-chat-ewoc-find-by-id "server-id-1")))
                :retracted)))))
      (dolist (buffer (list parent thread))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest jabber-test-moderation-author-retract-rejects-live-disagreement ()
  "Stored authorization cannot override conflicting live occupant identity."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (jabber-chat-ewoc-enter
     (list :muc-foreign
           (list :server-id "server-id-1"
                 :from "room@muc.example.com/alice"
                 :occupant-id "occupant-mallory"
                 :body "keep" :timestamp (current-time))))
    (let (db-call)
      (cl-letf (((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) t))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-retraction-candidates)
                 (lambda (_account _room _server-id)
                   (list (list :row-id 42
                               :from "room@muc.example.com/alice"
                               :occupant-id "occupant-alice"))))
                ((symbol-function 'jabber-db-retract-message-row)
                 (lambda (&rest args) (setq db-call args))))
        (should-not
         (jabber-moderation--handle-message
          'fake-jc
          '(message ((from . "room@muc.example.com/alice")
                     (type . "groupchat"))
                    (retract ((id . "server-id-1")
                              (xmlns . "urn:xmpp:message-retract:1")))
                    (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                  (id . "occupant-alice")))))))
      (should-not db-call)
      (should-not
       (plist-get
        (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "server-id-1")))
        :retracted)))))

(ert-deftest jabber-test-moderation-author-retract-rejects-missing-live-identity ()
  "Stored authorization cannot override missing live occupant identity."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (jabber-chat-ewoc-enter
     (list :muc-foreign
           (list :server-id "server-id-1"
                 :from "room@muc.example.com/alice"
                 :body "keep" :timestamp (current-time))))
    (let (db-call)
      (cl-letf (((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) t))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-retraction-candidates)
                 (lambda (_account _room _server-id)
                   (list (list :row-id 42
                               :from "room@muc.example.com/alice"
                               :occupant-id "occupant-alice"))))
                ((symbol-function 'jabber-db-retract-message-row)
                 (lambda (&rest args) (setq db-call args))))
        (should-not
         (jabber-moderation--handle-message
          'fake-jc
          '(message ((from . "room@muc.example.com/alice")
                     (type . "groupchat"))
                    (retract ((id . "server-id-1")
                              (xmlns . "urn:xmpp:message-retract:1")))
                    (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                  (id . "occupant-alice")))))))
      (should-not db-call)
      (should-not
       (plist-get
        (cadr (ewoc-data (jabber-chat-ewoc-find-by-id "server-id-1")))
        :retracted)))))

(ert-deftest jabber-test-moderation-author-retract-preserves-moderator-projections ()
  "A later author action cannot downgrade a moderator tombstone."
  (let* ((dir (make-temp-file "jabber-moderation-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" dir))
         (jabber-db--connection nil)
         (parent (generate-new-buffer " *jabber-moderated-parent*"))
         (thread (generate-new-buffer " *jabber-moderated-thread*"))
         (moderator "room@muc.example.com/admin"))
    (unwind-protect
        (progn
          (jabber-db-ensure-open)
          (jabber-db-store-message
           "me@example.com" "room@muc.example.com" "in" "groupchat"
           "spam" 1700000000 "alice" "client-id-1" "server-id-1"
           "occupant-alice")
          (dolist (entry (list (cons parent nil) (cons thread "thread-1")))
            (with-current-buffer (car entry)
              (setq-local jabber-group "room@muc.example.com")
              (setq-local jabber-buffer-connection 'fake-jc)
              (setq-local jabber-message-thread-id (cdr entry))
              (setq-local jabber-chat-ewoc
                          (ewoc-create #'ignore nil nil 'nosep))
              (setq-local jabber-chat--msg-nodes
                          (make-hash-table :test 'equal))
              (jabber-chat-ewoc-enter
               (list :muc-foreign
                     (list :server-id "server-id-1"
                           :from "room@muc.example.com/alice"
                           :occupant-id "occupant-alice"
                           :body "spam" :timestamp (current-time))))))
          (cl-letf (((symbol-function
                      'jabber-moderation--room-supports-occupant-id-p)
                     (lambda (_room) t))
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.com"))
                    ((symbol-function 'jabber-moderation--target-buffers)
                     (lambda (_jc _room _server-id)
                       (list parent thread)))
                    ((symbol-function
                      'jabber-moderation--target-buffers-for-row)
                     (lambda (_jc _room _row-id)
                       (list parent thread))))
            (should
             (jabber-moderation--handle-message
              'fake-jc
              `(message ((from . "room@muc.example.com")
                         (type . "groupchat"))
                        (retract ((id . "server-id-1")
                                  (xmlns . ,jabber-moderation-retract-xmlns))
                                 (moderated
                                  ((by . ,moderator)
                                   (xmlns . ,jabber-moderation-xmlns)))
                                 (reason () "spam")))))
            (should
             (jabber-moderation--handle-message
              'fake-jc
              '(message ((from . "room@muc.example.com/alice")
                         (type . "groupchat"))
                        (retract ((id . "server-id-1")
                                  (xmlns . "urn:xmpp:message-retract:1")))
                        (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                      (id . "occupant-alice")))))))
          (should
           (equal `((,moderator "spam"))
                  (sqlite-select
                   jabber-db--connection
                   "SELECT retracted_by, retraction_reason FROM message")))
          (dolist (buffer (list parent thread))
            (with-current-buffer buffer
              (let ((msg (cadr (ewoc-data
                                (jabber-chat-ewoc-find-by-id
                                 "server-id-1")))))
                (should (plist-get msg :retracted))
                (should (equal moderator (plist-get msg :retracted-by)))
                (should (equal "spam" (plist-get msg :retraction-reason)))))))
      (jabber-db-close)
      (dolist (buffer (list parent thread))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest jabber-test-moderation-live-author-retract-fails-closed ()
  "Live lookup rejects mismatched, missing, and duplicate target identities."
  (dolist (messages
           '(((:server-id "server-id-1"
              :from "room@muc.example.com/alice"
              :occupant-id "occupant-bob"))
             ((:server-id "other-id"
               :from "room@muc.example.com/alice"
               :occupant-id "occupant-alice"))
             ((:server-id "server-id-1"
               :from "room@muc.example.com/alice"
               :occupant-id "occupant-alice")
              (:server-id "server-id-1"
               :from "room@muc.example.com/alice"
               :occupant-id "occupant-alice"))))
    (jabber-test-moderation-with-ewoc
      (setq-local jabber-group "room@muc.example.com")
      (setq-local jabber-buffer-connection 'fake-jc)
      (dolist (msg messages)
        (ewoc-enter-last
         jabber-chat-ewoc
         (list :muc-foreign
               (append msg (list :body "keep" :timestamp (current-time))))))
      (when (cdr messages)
        (setq-local jabber-message-thread-id "same-view"))
      (let ((jabber-db-path nil))
        (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                   (lambda (_room &optional _jc) (current-buffer)))
                  ((symbol-function
                    'jabber-moderation--room-supports-occupant-id-p)
                   (lambda (_room) t)))
          (should-not
           (jabber-moderation--handle-message
            'fake-jc
            '(message ((from . "room@muc.example.com/alice")
                       (type . "groupchat"))
                      (retract ((id . "server-id-1")
                                (xmlns . "urn:xmpp:message-retract:1")))
                      (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                                    (id . "occupant-alice"))))))))
      (let ((node (ewoc-nth jabber-chat-ewoc 0)))
        (while node
          (should-not (plist-get (cadr (ewoc-data node)) :retracted))
          (setq node (ewoc-next jabber-chat-ewoc node)))))))

(ert-deftest jabber-test-moderation-author-retract-rejects-occupant-mismatch ()
  "Author retraction cannot retract another occupant's message."
  (let (db-call)
    (cl-letf (((symbol-function
                'jabber-moderation--room-supports-occupant-id-p)
               (lambda (_room) t))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-db-message-retraction-candidates)
               (lambda (_account _room _server-id)
                 (list (list :row-id 42
                             :from "room@muc.example.com/bob"
                             :occupant-id "occupant-bob"))))
              ((symbol-function 'jabber-db-retract-message-row)
               (lambda (&rest args) (setq db-call args))))
      (jabber-moderation--handle-message
       'fake-jc
       `(message ((from . "room@muc.example.com/alice")
                  (type . "groupchat"))
                 (retract ((id . "server-id-1")
                           (xmlns . ,jabber-moderation-retract-xmlns)))
                 (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                               (id . "occupant-alice"))))))
    (should-not db-call)))

(ert-deftest jabber-test-moderation-author-retract-rejects-ambiguous-target ()
  "Author retraction cannot choose among duplicate room stanza IDs."
  (cl-letf (((symbol-function
              'jabber-moderation--room-supports-occupant-id-p)
             (lambda (_room) t))
            ((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-db-message-retraction-candidates)
             (lambda (_account _room _server-id)
               (list (list :row-id 42
                           :from "room@muc.example.com/alice"
                           :occupant-id "occupant-alice")
                     (list :row-id 43
                           :from "room@muc.example.com/mallory"
                           :occupant-id "occupant-mallory"))))
            ((symbol-function 'jabber-db-retract-message-row)
             (lambda (&rest _args)
               (ert-fail "Retracted an ambiguous stanza ID"))))
    (should-not
     (jabber-moderation--handle-message
      'fake-jc
      `(message ((from . "room@muc.example.com/alice")
                 (type . "groupchat"))
                (retract ((id . "duplicate-server-id")
                          (xmlns . ,jabber-moderation-retract-xmlns)))
                (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                              (id . "occupant-alice"))))))))

(ert-deftest jabber-test-moderation-author-retract-requires-trusted-occupant-id ()
  "Unadvertised or ambiguous occupant IDs cannot authorize retraction."
  (dolist (case
           `((nil
              (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                            (id . "occupant-alice"))))
             (t
              (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                            (id . "occupant-alice")))
              (occupant-id ((xmlns . "urn:xmpp:occupant-id:0")
                            (id . "occupant-mallory"))))))
    (let ((advertised (car case))
          (occupant-elements (cdr case))
          db-call)
      (cl-letf (((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) advertised))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-message-retraction-candidates)
                 (lambda (_account _room _server-id)
                   (list (list :row-id 42
                               :from "room@muc.example.com/alice"
                               :occupant-id "occupant-alice"))))
                ((symbol-function 'jabber-db-retract-message-row)
                 (lambda (&rest args) (setq db-call args))))
        (jabber-moderation--handle-message
         'fake-jc
         `(message ((from . "room@muc.example.com/alice")
                    (type . "groupchat"))
                   (retract ((id . "server-id-1")
                             (xmlns . ,jabber-moderation-retract-xmlns)))
                   ,@occupant-elements)))
      (should-not db-call))))

(ert-deftest jabber-test-moderation-retract-fallback-is-protocol-only ()
  "Any XEP-0424 retract body is hidden and excluded from history."
  (dolist (stanza
           `((message ((from . "room@muc.example.com/alice")
                       (type . "groupchat"))
                      (body () "sender-controlled fallback")
                      (retract ((id . "missing-id")
                                (xmlns . ,jabber-moderation-retract-xmlns))))
             (message ((from . "room@muc.example.com/alice")
                       (type . "groupchat"))
                      (body () "ambiguous protocol action")
                      (retract ((id . "first")
                                (xmlns . ,jabber-moderation-retract-xmlns)))
                      (retract ((id . "second")
                                (xmlns . ,jabber-moderation-retract-xmlns))))))
    (should (jabber-moderation--muc-retraction-message-p stanza))
    (should (jabber-moderation--history-inhibit-p nil stanza)))
  (should-not
   (jabber-moderation--muc-retraction-message-p
    '(message ((type . "groupchat"))
              (retract ((id . "ordinary-extension")
                        (xmlns . "urn:example:not-retraction")))))))

(ert-deftest jabber-test-moderation-muc-does-not-render-retract-fallback ()
  "MUC processing never displays a XEP-0424 retract fallback body."
  (let ((displayed nil)
        (stanza
         `(message ((from . "room@muc.example.com/alice")
                    (type . "groupchat"))
                   (body () "sender-controlled fallback")
                   (retract ((id . "unknown-id")
                             (xmlns . ,jabber-moderation-retract-xmlns))))))
    (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc xml-data) xml-data))
              ((symbol-function 'jabber-muc--display-message)
               (lambda (&rest _args) (setq displayed t))))
      (jabber-muc-process-message 'fake-jc stanza))
    (should-not displayed)))

(ert-deftest jabber-test-moderation-tombstone-updates-ewoc ()
  "Archived tombstone sets :retracted using the MAM archive id."
  (jabber-test-moderation-with-ewoc
    (let ((msg (list :id "client-id-1" :server-id "stanza-id-1"
                     :from "room@muc.example.com/alice"
                     :body "spam" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    (let ((buf (current-buffer))
          db-call)
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group &optional _jc) buf))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@muc.example.com"))
                ((symbol-function 'jabber-db-retract-message-in-peer)
                 (lambda (account peer server-id moderator reason)
                   (setq db-call
                         (list account peer server-id moderator reason)))))
        (let ((tombstone-xml
               '(message ((from . "room@muc.example.com/alice")
                          (type . "groupchat")
                          (jabber-mam--origin . "t")
                          (jabber-mam--archive-id . "stanza-id-1"))
                         (retracted ((stamp . "2026-01-01T00:00:00Z")
                                     (xmlns . "urn:xmpp:message-retract:1"))
                                    (moderated ((by . "room@muc.example.com/admin")
                                                (xmlns . "urn:xmpp:message-moderate:1")))
                                    (reason () "spam")))))
          (jabber-moderation--handle-message 'fake-jc tombstone-xml)))
      (let* ((node (jabber-chat-ewoc-find-by-id "stanza-id-1"))
             (msg (cadr (ewoc-data node))))
        (should (equal '("me@muc.example.com" "room@muc.example.com"
                         "stanza-id-1" "room@muc.example.com/admin" "spam")
                       db-call))
        (should (plist-get msg :retracted))
        (should (equal "room@muc.example.com/admin"
                       (plist-get msg :retracted-by)))
        (should (equal "spam" (plist-get msg :retraction-reason)))))))

;;; Group 3: stanza-id source validation

(ert-deftest jabber-test-moderation-rejects-client-id ()
  "Retraction targeting a client message-id (not server stanza-id) is ignored."
  (jabber-test-moderation-with-ewoc
    (let ((msg (list :id "client-id-1" :server-id "server-stanza-id-1"
                     :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg)))
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group &optional _jc) buf))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@muc.example.com"))
                ((symbol-function 'jabber-db-retract-message-in-peer)
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
          (jabber-moderation--handle-message 'fake-jc retract-xml))))
    (let* ((node (jabber-chat-ewoc-find-by-id "client-id-1"))
           (msg (cadr (ewoc-data node))))
      (should-not (plist-get msg :retracted)))))

;;; Group 4: sender validation


(ert-deftest jabber-test-moderation-validates-sender ()
  "Retraction from a participant (not MUC service) is ignored."
  (jabber-test-moderation-with-ewoc
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

(ert-deftest jabber-test-moderation-non-groupchat-ignored ()
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

(ert-deftest jabber-test-moderation-missing-message-ignored ()
  "Retraction for unknown stanza-id doesn't error."
  (jabber-test-moderation-with-ewoc
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_group &optional _jc) buf))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@muc.example.com"))
                ((symbol-function 'jabber-db-retract-message-in-peer)
                 #'ignore))
        (let ((retract-xml
               '(message ((from . "room@muc.example.com")
                          (type . "groupchat"))
                         (retract ((id . "nonexistent-id")
                                   (xmlns . "urn:xmpp:message-retract:1"))
                                  (moderated ((by . "room@muc.example.com/admin")
                                              (xmlns . "urn:xmpp:message-moderate:1")))))))
          ;; Should return t (consumed) but not error
          (should (jabber-moderation--handle-message 'fake-jc retract-xml)))))))

;;; Group 6: tombstone rendering

(ert-deftest jabber-test-moderation-tombstone-rendering ()
  "Tombstone text is inserted for retracted messages."
  (with-temp-buffer
    (let ((msg (list :body "spam" :retracted t
                     :retracted-by "room@muc.example.com/admin"
                     :retraction-reason "spam")))
      (jabber-chat--insert-tombstone msg)
      (should (string-match-p
               "Message retracted by: admin reason: spam"
               (buffer-string))))))

(ert-deftest jabber-test-moderation-tombstone-no-reason ()
  "Tombstone without reason omits the reason part."
  (with-temp-buffer
    (let ((msg (list :body "x" :retracted t
                     :retracted-by "room@muc.example.com/mod")))
      (jabber-chat--insert-tombstone msg)
      (let ((text (buffer-string)))
        (should (string-match-p "Message retracted by: mod" text))
        (should-not (string-match-p "reason:" text))))))

;;; Group 7: build-msg-plist extracts server-id

(ert-deftest jabber-test-moderation-plist-extracts-server-id ()
  "Build message state with server and occupant IDs."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (id . "client-id")
                            (type . "groupchat"))
                           (body () "hello")
                           (stanza-id ((id . "server-id-42")
                                       (by . "room@muc.example.com")
                                       (xmlns . "urn:xmpp:sid:0")))
                           (occupant-id ((id . "occupant-alice")
                                         (xmlns . "urn:xmpp:occupant-id:0")))))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should (equal "server-id-42" (plist-get plist :server-id)))
    (should (equal "occupant-alice" (plist-get plist :occupant-id)))))

(ert-deftest jabber-test-moderation-plist-nil-server-id ()
  "jabber-chat--build-msg-plist returns nil :server-id when absent."
  (let* ((stanza '(message ((from . "room@muc.example.com/alice")
                            (type . "groupchat"))
                           (body () "hello")))
         (plist (jabber-chat--msg-plist-from-stanza stanza)))
    (should-not (plist-get plist :server-id))))

;;; Group 8: retract command

(ert-deftest jabber-test-moderation-retract-sends-iq ()
  "jabber-moderation-retract sends correct IQ XML."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let* ((msg (list :id "msg-r1" :server-id "sid-retract"
                      :body "spam" :timestamp (current-time)))
           (sent-iq nil)
           (db-call nil)
           (buf (current-buffer)))
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'jabber-send-iq)
                 (lambda (jc to type query success success-data
                             error error-data)
                   (setq sent-iq
                         (list :jc jc :to to :type type :query query
                               :success success :success-data success-data
                               :error error :error-data error-data))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "test reason"))
                ((symbol-function 'jabber-muc-nickname)
                 (lambda (_room _jc) "nick"))
                ((symbol-function 'jabber-muc-find-buffer)
                 (lambda (_room &optional _jc) buf))
                ((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-db-retract-message-in-peer)
                 (lambda (&rest args) (setq db-call args))))
        (jabber-moderation-retract)
        (should (equal "room@muc.example.com" (plist-get sent-iq :to)))
        (should (equal "set" (plist-get sent-iq :type)))
        (should-not db-call)
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
            (should (equal "test reason" (nth 2 reason)))))
        (funcall (plist-get sent-iq :success)
                 'fake-jc '(iq ((type . "result")))
                 (plist-get sent-iq :success-data))
        (should (equal '("me@example.com" "room@muc.example.com"
                         "sid-retract" "room@muc.example.com/nick"
                         "test reason")
                       db-call))
        (let* ((node (jabber-chat-ewoc-find-by-id "sid-retract"))
               (msg (cadr (ewoc-data node))))
          (should (plist-get msg :retracted)))))))

(ert-deftest jabber-test-moderation-retract-errors-without-server-id ()
  "jabber-moderation-retract signals error when no server-id."
  (jabber-test-moderation-with-ewoc
    (setq-local jabber-group "room@muc.example.com")
    (setq-local jabber-buffer-connection 'fake-jc)
    (let ((msg (list :id "msg-r2" :body "hello" :timestamp (current-time))))
      (jabber-chat-ewoc-enter (list :muc-foreign msg))
      (goto-char (point-min))
      (should-error (jabber-moderation-retract) :type 'user-error))))

(ert-deftest jabber-test-moderation-retract-errors-outside-muc ()
  "jabber-moderation-retract signals error outside MUC buffer."
  (with-temp-buffer
    (should-error (jabber-moderation-retract) :type 'user-error)))

(provide 'jabber-test-moderation)

;;; jabber-test-moderation.el ends here
