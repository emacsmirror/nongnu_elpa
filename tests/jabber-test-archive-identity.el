;;; jabber-test-archive-identity.el --- Persistent action identities -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise native ingestion, actions, SQLite readers and cold replay together.

;;; Code:

(require 'jabber-archive-test-helpers)

(defun jabber-test-archive-identity--direct (&optional origin)
  "Return a direct message, optionally carrying ORIGIN."
  `(message ((from . "friend@example.com/phone") (type . "chat") (id . "transport"))
            (body () "retained")
            ,@(when origin
                `((origin-id ((xmlns . "urn:xmpp:sid:0") (id . ,origin)))))))

(defun jabber-test-archive-identity--replay (jc inner archive uid)
  "Replay INNER on JC from ARCHIVE with UID through a complete MAM page."
  (let ((query (jabber-mam--query jc nil nil nil nil archive t 10)))
    (jabber-mam--process-message jc (jabber-test-archive--result query uid inner))
    (jabber-test-archive--fin jc query uid)))

(ert-deftest jabber-test-archive-identity-reaction-action-reopen ()
  "Select from cold history, send the real action, receive and reject stale updates."
  (jabber-test-archive--with-file
    (let ((xml (jabber-test-archive-identity--direct "origin"))
          (other (jabber-test-mam--native-connection "other")))
      (jabber-db--message-handler jc xml)
      (jabber-db--message-handler other xml)
      (jabber-db-close)
      (let ((stored (car (jabber-db-backlog "me@example.com" "friend@example.com")))
            sent)
        (with-temp-buffer
          (setq-local jabber-buffer-connection jc)
          (setq-local jabber-chatting-with "friend@example.com")
          (setq-local jabber-chat-ewoc (ewoc-create (lambda (_) (insert "message\n"))))
          (let ((node (ewoc-enter-last jabber-chat-ewoc (list :foreign stored))))
            (setq-local jabber-point-insert (copy-marker (point-max)))
            (goto-char (ewoc-location node))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "👍"))
                      ((symbol-function 'jabber-send-sexp)
                       (lambda (_jc xml &rest _) (setq sent xml))))
              (call-interactively #'jabber-reactions-react-at-point-or-insert))
            (should (equal "origin" (jabber-xml-get-attribute
                                     (car (jabber-xml-get-children sent 'reactions)) 'id)))))
        (let ((reader (sqlite-open jabber-db-path)))
          (unwind-protect
              (should (equal '(("me@example.com" "👍"))
                             (sqlite-select reader "SELECT m.account,r.reaction
FROM message m JOIN message_reaction r ON r.message_id=m.id")))
            (sqlite-close reader))))
      ;; Inbound source time and actor lookup use the same origin/alias policy.
      (dolist (entry '(("2025-01-02T00:00:00Z" "origin" "🔥")
                       ("2025-01-01T00:00:00Z" "transport" "stale")))
        (jabber-reactions--handle-message
         jc `(message ((from . "friend@example.com/laptop") (type . "chat"))
                      (delay ((xmlns . "urn:xmpp:delay") (stamp . ,(car entry))))
                      (reactions ((xmlns . "urn:xmpp:reactions:0") (id . ,(cadr entry)))
                                 (reaction () ,(nth 2 entry))))))
      (should (jabber-db-reaction-stale-p
               "me@example.com" "friend@example.com" "chat" "origin"
               "friend@example.com" 1))
      (should (jabber-db-reaction-stale-p
               "me@example.com" "friend@example.com" "chat" "transport"
               "friend@example.com" 1))
      (jabber-db-close)
      (let ((reactions (plist-get (car (jabber-db-backlog
                                        "me@example.com" "friend@example.com")) :reactions)))
        (should (equal '("👍") (cdr (assoc "me@example.com" reactions))))
        (should (equal '("🔥") (cdr (assoc "friend@example.com" reactions)))))
      (should-not (plist-get (car (jabber-db-backlog
                                  "other@example.com" "friend@example.com")) :reactions)))))

(ert-deftest jabber-test-archive-identity-historical-reply-enrichment ()
  "Retain transport references after replay adds origin; new replies prefer origin."
  (jabber-test-archive--with-file
    (jabber-db--message-handler jc (jabber-test-archive-identity--direct))
    (jabber-db--message-handler
     jc '(message ((from . "friend@example.com/phone") (type . "chat") (id . "reply"))
                  (body () "response")
                  (reply ((xmlns . "urn:xmpp:reply:0") (id . "transport")
                          (to . "friend@example.com/phone")))))
    (should (equal "retained" (jabber-db-reply-target-body
                              "me@example.com" "friend@example.com" "transport" nil)))
    (jabber-db-close)
    (jabber-test-archive-identity--replay
     jc (jabber-test-archive-identity--direct "origin") nil "archive-uid")
    (jabber-db-close)
    (let* ((rows (jabber-db-backlog "me@example.com" "friend@example.com"))
           (reply (seq-find (lambda (row) (plist-get row :reply-to-id)) rows))
           (target (seq-find (lambda (row) (equal "transport" (plist-get row :id))) rows)))
      (should (= (length rows) 2))
      (should (equal "transport" (plist-get reply :reply-to-id)))
      (should (equal "origin" (jabber-message-reply--select-id target nil)))
      (with-temp-buffer
        (setq-local jabber-buffer-connection jc)
        (setq-local jabber-chatting-with "friend@example.com")
        (should (equal "retained" (jabber-chat--reply-context-snippet reply))))
      (should-not (jabber-db-reply-target-body
                   "me@example.com" "friend@example.com" "archive-uid" nil)))))

(ert-deftest jabber-test-archive-identity-reference-collisions ()
  "Count origin/transport collisions across senders, not accounts or message kinds."
  (jabber-test-archive--with-file
    (jabber-db--message-handler jc (jabber-test-archive-identity--direct "origin"))
    ;; A distinct sender's transport equals the first sender's origin.
    (jabber-db--message-handler
     jc '(message ((from . "friend@example.com/other") (type . "chat") (id . "origin"))
                  (body () "collision")
                  (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "other-origin")))))
    (jabber-db-close)
    (should-not (jabber-db-replace-reactions
                 "me@example.com" "friend@example.com" "chat" "origin" "actor" '("bad")))
    (should-not (jabber-db-reply-target-body
                 "me@example.com" "friend@example.com" "origin" nil))
    (should (equal "retained" (jabber-db-reply-target-body
                              "me@example.com" "friend@example.com" "origin" nil
                              "friend@example.com/phone")))
    (should-not (jabber-db-reply-target-body
                 "me@example.com" "friend@example.com" "origin" nil "wrong@example.com"))
    (should-not (jabber-db-reply-target-body
                 "me@example.com" "friend@example.com" "origin" nil "friend@example.com"))
    (should-not (sqlite-select jabber-db--connection "SELECT * FROM message_reaction_actor"))
    ;; A visible subset must not override ambiguity found in durable history.
    (with-temp-buffer
      (setq-local jabber-buffer-connection jc)
      (setq-local jabber-chatting-with "friend@example.com")
      (setq-local jabber-chat-ewoc (ewoc-create (lambda (_) (insert "message\n"))))
      (let* ((stored (seq-find (lambda (msg) (equal "transport" (plist-get msg :id)))
                               (jabber-db-backlog "me@example.com" "friend@example.com")))
             (node (ewoc-enter-last jabber-chat-ewoc (list :foreign stored)))
             sent)
        (setq-local jabber-point-insert (copy-marker (point-max)))
        (goto-char (ewoc-location node))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bad"))
                  ((symbol-function 'jabber-send-sexp) (lambda (&rest _) (setq sent t))))
          (should-error (call-interactively #'jabber-reactions-react-at-point-or-insert)
                        :type 'user-error))
        (should-not sent)
        (jabber-reactions--handle-message
         jc '(message ((from . "friend@example.com/phone") (type . "chat"))
                      (reactions ((xmlns . "urn:xmpp:reactions:0") (id . "origin"))
                                 (reaction () "bad"))))
        (should-not (plist-get (cadr (ewoc-data node)) :reactions))))
    ;; Tombstones participate in ambiguity rather than selecting the survivor.
    (sqlite-execute jabber-db--connection "UPDATE message SET retracted=1 WHERE stanza_id='origin'")
    (should-not (jabber-db-reply-target-body
                 "me@example.com" "friend@example.com" "origin" nil))
    (should-not (jabber-db-replace-reactions
                 "me@example.com" "friend@example.com" "chat" "origin" "actor" '("bad")))
    ;; One row matching both aliases is not two candidates.
    (jabber-db--message-handler
     jc '(message ((from . "friend@example.com/phone") (type . "chat") (id . "same"))
                  (body () "one")
                  (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "same")))))
    (should (jabber-db-replace-reactions
             "me@example.com" "friend@example.com" "chat" "same" "actor" '("ok")))))

(defun jabber-test-archive-identity--room ()
  "Return a partial live room stanza with sender and transport evidence."
  '(message ((from . "room@example.com/nick") (type . "groupchat") (id . "transport"))
            (body () "retained")
            (delay ((xmlns . "urn:xmpp:delay") (stamp . "2025-01-01T00:00:00Z")))
            (occupant-id ((xmlns . "urn:xmpp:occupant-id:0") (id . "actor")))))

(ert-deftest jabber-test-archive-identity-partial-room-replay ()
  "Enrich one live partial room row, replay two archives and preserve its tombstone."
  (jabber-test-archive--with-file
    (let ((jabber-muc--rooms (make-hash-table :test #'equal))
          (jabber-muc--generation 0)
          (inner (jabber-test-archive-identity--room)))
      (jabber-muc-join-set "room@example.com" jc "me")
      (jabber-db--message-handler jc inner)
      (let ((id (caar (sqlite-select jabber-db--connection "SELECT id FROM message")))
            (full (append inner '((stanza-id ((xmlns . "urn:xmpp:sid:0")
                                             (by . "room@example.com") (id . "room-id")))))))
        (jabber-db-close)
        (jabber-test-archive-identity--replay jc full "room@example.com" "room-uid")
        (let ((reader (sqlite-open jabber-db-path)))
          (unwind-protect
              (should (equal (list (list id "transport" "room-id" "room-id"))
                             (sqlite-select reader "SELECT id,stanza_id,server_id,room_id FROM message")))
            (sqlite-close reader)))
        (jabber-db-retract-message-row id nil)
        (dotimes (_ 2)
          (jabber-db-close)
          (jabber-test-archive-identity--replay jc full nil "personal-uid")
          (jabber-test-archive-identity--replay jc full "room@example.com" "room-uid"))
        (jabber-db-close)
        (should (equal (list (list id 1 "retained"))
                       (sqlite-select (jabber-db-ensure-open) "SELECT id,retracted,body FROM message")))
        (should (equal '(("me@example.com" "personal-uid") ("room@example.com" "room-uid"))
                       (sqlite-select jabber-db--connection
                                      "SELECT archive,uid FROM message_archive ORDER BY archive")))))))

(ert-deftest jabber-test-archive-identity-partial-room-conflicts ()
  "Reject sender, kind, occupant, transport, origin, legacy-server and weak matches."
  (dolist (conflict '(sender type occupant transport origin server weak ambiguous))
    (jabber-test-archive--with-file
      (let ((jabber-muc--rooms (make-hash-table :test #'equal))
            (jabber-muc--generation 0)
            (inner (jabber-test-archive-identity--room)))
        (jabber-muc-join-set "room@example.com" jc "me")
        (jabber-db--message-handler jc inner)
        (pcase conflict
          ('sender (sqlite-execute jabber-db--connection "UPDATE message SET resource='other'"))
          ('type (sqlite-execute jabber-db--connection "UPDATE message SET type='chat'"))
          ('occupant (sqlite-execute jabber-db--connection "UPDATE message SET occupant_id='other'"))
          ('transport (sqlite-execute jabber-db--connection "UPDATE message SET stanza_id='other'"))
          ('origin (sqlite-execute jabber-db--connection "UPDATE message SET origin_id='other'"))
          ('server (sqlite-execute jabber-db--connection "UPDATE message SET server_id='room-id'"))
          ('weak (sqlite-execute jabber-db--connection "UPDATE message SET stanza_id=NULL"))
          ('ambiguous
           (sqlite-execute jabber-db--connection "INSERT INTO message
(account,peer,resource,occupant_id,direction,type,body,timestamp,stanza_id)
SELECT account,peer,resource,occupant_id,direction,type,body,timestamp,stanza_id FROM message")))
        (let ((before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id")))
          (jabber-db-close)
          (jabber-test-archive-identity--replay
           jc (append inner '((stanza-id ((xmlns . "urn:xmpp:sid:0")
                                         (by . "room@example.com") (id . "room-id")))
                              (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin")))))
           "room@example.com" "room-uid")
          (jabber-db-close)
          (should (equal before
                         (seq-take (sqlite-select (jabber-db-ensure-open)
                                                  "SELECT * FROM message ORDER BY id")
                                   (length before))))
          (should (= (1+ (length before))
                     (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message")))))))))

(ert-deftest jabber-test-archive-identity-thread-reaction-routing ()
  "Route a restored origin reaction to its native dedicated thread view."
  (jabber-test-archive--with-file
    (let ((jabber-buffer-registry--buffers (make-hash-table :test #'equal))
          (jabber-message-thread-use-buffers t))
      (jabber-db--message-handler
       jc (append (jabber-test-archive-identity--direct "origin")
                  '((thread () "thread"))))
      (jabber-db-register-message-thread
       "me@example.com" "friend@example.com" "chat" "thread" nil "transport" nil 1)
      (jabber-db-close)
      (let ((stored (car (jabber-db-backlog
                         "me@example.com" "friend@example.com" nil nil nil nil t))))
        (with-temp-buffer
          (setq-local jabber-buffer-connection jc)
          (setq-local jabber-chatting-with "friend@example.com")
          (setq-local jabber-chat-ewoc (ewoc-create (lambda (_) (insert "message\n"))))
          (let ((node (ewoc-enter-last jabber-chat-ewoc (list :foreign stored))))
            (jabber-buffer-registry-register
             'thread '("me@example.com" "friend@example.com" "chat" "thread"))
            (jabber-reactions--handle-message
             jc '(message ((from . "friend@example.com/phone") (type . "chat"))
                          (reactions ((xmlns . "urn:xmpp:reactions:0") (id . "origin"))
                                     (reaction () "👍"))))
            (should (equal '(("friend@example.com" "👍"))
                           (plist-get (cadr (ewoc-data node)) :reactions)))))))))

(ert-deftest jabber-test-archive-identity-visible-reaction-collisions ()
  "Do not let a cached generic ID or two visible aliases choose a wrong target."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (let* ((first (ewoc-enter-last jabber-chat-ewoc
                                  '(:foreign (:id "transport" :origin-id "origin"
                                               :server-id "archive"))))
           (second (ewoc-enter-last jabber-chat-ewoc
                                   '(:foreign (:id "origin" :origin-id "other"
                                                :server-id "room")))))
      (puthash "origin" first jabber-chat--msg-nodes)
      (should-not (jabber-reactions--find-target-node "origin" nil))
      (should-not (jabber-reactions--find-target-node "archive" nil))
      (should-not (jabber-reactions--find-target-node "origin" t))
      (should (eq second (jabber-reactions--find-target-node "room" t)))
      (should (eq first (jabber-reactions--find-target-node "transport" nil))))))

(provide 'jabber-test-archive-identity)
;;; jabber-test-archive-identity.el ends here
