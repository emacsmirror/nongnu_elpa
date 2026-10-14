;;; jabber-test-action-storage.el --- Reaction and reference storage -*- lexical-binding: t; -*-

;;; Commentary:
;; Atomic reaction snapshots and indexed reply/reaction target candidates,
;; exercised through native dispatch, real SQLite faults and reopened files.

;;; Code:

(require 'jabber-archive-test-helpers)

;;;; Reaction snapshots

(defconst jabber-test-action-storage--sender "friend@example.com"
  "Reaction actor used by direct-chat fixtures.")

(defun jabber-test-action-storage--state ()
  "Return reaction actor and snapshot rows from an independent SQLite reader."
  (let ((reader (sqlite-open jabber-db-path)))
    (unwind-protect
        (list (sqlite-select reader "SELECT message_id, sender, updated_at
FROM message_reaction_actor ORDER BY message_id, sender")
              (sqlite-select reader "SELECT message_id, sender, reaction, updated_at
FROM message_reaction ORDER BY message_id, sender, reaction"))
      (sqlite-close reader))))

(defun jabber-test-action-storage--seed (jc)
  "Store a direct target on JC with an older two-reaction snapshot.
Return the independently read state."
  (jabber-db--message-handler
   jc '(message ((from . "friend@example.com/phone") (type . "chat") (id . "target"))
                (body () "target")))
  (should (jabber-db-replace-reactions
           "me@example.com" "friend@example.com" "chat" "target"
           jabber-test-action-storage--sender '("old" "older") 100))
  (jabber-test-action-storage--state))

(defun jabber-test-action-storage--replace (reactions)
  "Replace the fixture actor's target REACTIONS at source time 200."
  (jabber-db-replace-reactions
   "me@example.com" "friend@example.com" "chat" "target"
   jabber-test-action-storage--sender reactions 200))

(defun jabber-test-action-storage--assert-retry (before reactions)
  "Assert BEFORE survives reopen and a same-source retry applies REACTIONS."
  (jabber-db-close)
  (jabber-db-ensure-open)
  (should (equal before (jabber-test-action-storage--state)))
  (should (jabber-test-action-storage--replace reactions))
  (should (equal (list (list (list 1 jabber-test-action-storage--sender 200))
                       (mapcar (lambda (reaction)
                                 (list 1 jabber-test-action-storage--sender reaction 200))
                               (sort (copy-sequence reactions) #'string<)))
                 (jabber-test-action-storage--state))))

(ert-deftest jabber-test-action-storage-reaction-sqlite-faults-roll-back ()
  "A SQLite abort at any snapshot write preserves the prior watermark and rows."
  (dolist (case '((actor "BEFORE UPDATE ON message_reaction_actor" ("new1" "new2"))
                  (delete "BEFORE DELETE ON message_reaction" ("new1" "new2"))
                  (empty "BEFORE DELETE ON message_reaction" nil)
                  (first "BEFORE INSERT ON message_reaction WHEN NEW.reaction = 'new1'"
                         ("new1" "new2"))
                  (second "BEFORE INSERT ON message_reaction WHEN NEW.reaction = 'new2'"
                          ("new1" "new2"))))
    (pcase-let ((`(,name ,trigger ,reactions) case))
      (ert-info ((symbol-name name) :prefix "Boundary: ")
        (jabber-test-archive--with-file
          (let ((before (jabber-test-action-storage--seed jc)))
            (sqlite-execute jabber-db--connection
                            (format "CREATE TRIGGER snapshot_fault %s
BEGIN SELECT RAISE(ABORT, 'injected snapshot fault'); END" trigger))
            (should-error (jabber-test-action-storage--replace reactions)
                          :type 'sqlite-error)
            (should (equal before (jabber-test-action-storage--state)))
            (sqlite-execute jabber-db--connection "DROP TRIGGER snapshot_fault")
            (jabber-test-action-storage--assert-retry before reactions)))))))

(ert-deftest jabber-test-action-storage-reaction-error-and-quit-roll-back ()
  "Error and quit after each completed snapshot write leave no partial state."
  (dolist (failure '(error quit))
    (dotimes (boundary 3)
      (ert-info ((format "%s after write %d" failure boundary) :prefix "Fault: ")
        (jabber-test-archive--with-file
          (let ((before (jabber-test-action-storage--seed jc))
                (execute (symbol-function 'sqlite-execute))
                (writes 0))
            (should
             (eq failure
                 (condition-case err
                     (cl-letf (((symbol-function 'sqlite-execute)
                                (lambda (db sql &rest args)
                                  (prog1 (apply execute db sql args)
                                    (when (and (string-match-p
                                                "\\`\\(?:INSERT INTO\\|DELETE FROM\\) message_reaction"
                                                sql)
                                               (= boundary (prog1 writes (cl-incf writes))))
                                      (signal failure '("Injected snapshot fault")))))))
                       (jabber-test-action-storage--replace '("new1" "new2"))
                       nil)
                   ((error quit) (car err)))))
            (should (equal before (jabber-test-action-storage--state)))
            (jabber-test-action-storage--assert-retry before '("new1" "new2"))))))))

(defconst jabber-test-action-storage--mam-chain
  (list #'jabber-mam--process-message #'jabber-reactions--handle-message)
  "Native message chain for archived reaction results.")

(defun jabber-test-action-storage--reaction-stanza ()
  "Return a delayed two-reaction update for the direct fixture target."
  '(message ((from . "friend@example.com/phone") (type . "chat"))
            (delay ((xmlns . "urn:xmpp:delay") (stamp . "2025-01-02T00:00:00Z")))
            (reactions ((xmlns . "urn:xmpp:reactions:0") (id . "target"))
                       (reaction () "good") (reaction () "bad"))))

(defun jabber-test-action-storage--dispatch (jc query uid)
  "Dispatch the reaction update natively on JC, through QUERY page UID if any."
  (let ((jabber-message-chain
         (if query jabber-test-action-storage--mam-chain
           (list #'jabber-reactions--handle-message)))
        (inner (jabber-test-action-storage--reaction-stanza)))
    (jabber-process-input jc (if query (jabber-test-archive--result query uid inner) inner))
    (when query (jabber-test-archive--fin jc query uid))))

(ert-deftest jabber-test-action-storage-reaction-native-live-and-mam-retry ()
  "Native live and MAM faults keep prior state; the same source then applies."
  (dolist (mode '(live mam))
    (ert-info ((symbol-name mode) :prefix "Input: ")
      (jabber-test-archive--with-file
        (let ((before (jabber-test-action-storage--seed jc))
              (applied '(((1 "friend@example.com" 1735776000))
                         ((1 "friend@example.com" "bad" 1735776000)
                          (1 "friend@example.com" "good" 1735776000)))))
          (sqlite-execute jabber-db--connection "CREATE TRIGGER snapshot_fault
BEFORE INSERT ON message_reaction WHEN NEW.reaction = 'bad'
BEGIN SELECT RAISE(ABORT, 'injected snapshot fault'); END")
          ;; Native dispatch contains the error; MAM records page failure.
          (jabber-test-action-storage--dispatch
           jc (and (eq mode 'mam) (jabber-mam--query jc)) "failed-uid")
          (should (equal before (jabber-test-action-storage--state)))
          (should-not (jabber-test-archive--progress))
          (sqlite-execute jabber-db--connection "DROP TRIGGER snapshot_fault")
          (jabber-db-close)
          (jabber-db-ensure-open)
          (should (equal before (jabber-test-action-storage--state)))
          (jabber-test-action-storage--dispatch
           jc (and (eq mode 'mam) (jabber-mam--query jc)) "retry-uid")
          (should (equal applied (jabber-test-action-storage--state)))
          (should (equal (and (eq mode 'mam) '("retry-uid" nil))
                         (jabber-test-archive--progress))))))))

(ert-deftest jabber-test-action-storage-reaction-fault-keeps-concurrent-mam-page ()
  "A failed snapshot inside a shared MAM transaction keeps other accepted pages."
  (jabber-test-archive--with-file
    (let* ((before (jabber-test-action-storage--seed jc))
           (failing (jabber-mam--query jc nil nil "friend@example.com"))
           (accepted (jabber-mam--query jc nil nil "other@example.com")))
      (sqlite-execute jabber-db--connection "CREATE TRIGGER snapshot_fault
BEFORE INSERT ON message_reaction WHEN NEW.reaction = 'bad'
BEGIN SELECT RAISE(ABORT, 'injected snapshot fault'); END")
      (let ((jabber-message-chain jabber-test-action-storage--mam-chain))
        (jabber-process-input
         jc (jabber-test-archive--result
             accepted "accepted-uid"
             '(message ((from . "other@example.com/desk") (type . "chat") (id . "kept"))
                       (body () "kept"))))
        (jabber-process-input
         jc (jabber-test-archive--result
             failing "failed-uid" (jabber-test-action-storage--reaction-stanza))))
      (jabber-test-archive--fin jc failing "failed-uid")
      (jabber-test-archive--fin jc accepted "accepted-uid")
      (sqlite-execute jabber-db--connection "DROP TRIGGER snapshot_fault")
      (jabber-db-close)
      (jabber-db-ensure-open)
      (should (equal before (jabber-test-action-storage--state)))
      (should-not (jabber-test-archive--progress nil nil "friend@example.com"))
      (should (equal '("accepted-uid" nil)
                     (jabber-test-archive--progress nil nil "other@example.com")))
      (should (equal '(("kept"))
                     (sqlite-select
                      jabber-db--connection
                      "SELECT body FROM message WHERE peer = 'other@example.com'"))))))

(ert-deftest jabber-test-action-storage-reaction-physical-rollback-control ()
  "A transaction-wide SQLite rollback fails the whole MAM commit closed."
  (jabber-test-archive--with-file
    (let ((before (jabber-test-action-storage--seed jc)))
      ;; Install outside the page transaction, which the fault discards.
      (sqlite-execute jabber-db--connection "CREATE TRIGGER snapshot_fault
BEFORE INSERT ON message_reaction WHEN NEW.reaction = 'bad'
BEGIN SELECT RAISE(ROLLBACK, 'injected transaction loss'); END")
      (let ((query (jabber-mam--query jc))
            (jabber-message-chain jabber-test-action-storage--mam-chain))
        (jabber-process-input
         jc (jabber-test-archive--result
             query "body-uid"
             '(message ((from . "friend@example.com/phone") (type . "chat") (id . "lost"))
                       (body () "lost"))))
        (jabber-process-input
         jc (jabber-test-archive--result
             query "reaction-uid" (jabber-test-action-storage--reaction-stanza)))
        ;; The lost transaction cannot be released, so settlement fails.
        (should-error (jabber-test-archive--fin jc query "reaction-uid")
                      :type 'sqlite-error))
      (sqlite-execute jabber-db--connection "DROP TRIGGER snapshot_fault")
      (jabber-db-close)
      (jabber-db-ensure-open)
      (should (equal before (jabber-test-action-storage--state)))
      (should-not (jabber-test-archive--progress))
      (should-not (sqlite-select jabber-db--connection
                                 "SELECT id FROM message WHERE body = 'lost'")))))

;;;; Reference candidates

(defconst jabber-test-action-storage--rows
  ;; (ACCOUNT PEER TYPE ROOM SERVER ORIGIN STANZA RETRACTED RESOURCE)
  '(("me@example.com" "room@example.com" "groupchat" "uid-1" "uid-1" nil nil 0 "nick")
    ("me@example.com" "room@example.com" "groupchat" nil "legacy" nil nil 0 "nick")
    ("me@example.com" "room@example.com" "groupchat" "shadow" "uid-2" nil nil 0 "nick")
    ("me@example.com" "room@example.com" "groupchat" "uid-2" nil nil nil 0 "nick")
    ("me@example.com" "room@example.com" "groupchat" "dup" "dup" nil nil 0 "nick")
    ("me@example.com" "room@example.com" "groupchat" "dup" nil nil nil 1 "other")
    ("me@example.com" "room@example.com" "groupchat" nil "dup" nil nil 0 "third")
    ("other@example.com" "room@example.com" "groupchat" "uid-1" "uid-1" nil nil 0 "nick")
    ("me@example.com" "elsewhere@example.com" "groupchat" "uid-1" "uid-1" nil nil 0 "nick")
    ("me@example.com" "room@example.com" "chat" "uid-1" "uid-1" "uid-1" "uid-1" 0 "nick")
    ("me@example.com" "friend@example.com" "chat" nil nil "o-1" "s-1" 0 "phone")
    ("me@example.com" "friend@example.com" "chat" nil nil "same" "same" 0 "phone")
    ("me@example.com" "friend@example.com" "chat" nil nil "x-1" "s-2" 0 "phone")
    ("me@example.com" "friend@example.com" "chat" nil nil nil "x-1" 1 "phone")
    ("me@example.com" "friend@example.com" "chat" "o-3" "o-3" nil nil 0 "phone")
    ("me@example.com" "friend@example.com" "groupchat" nil nil "o-1" "o-1" 0 "phone"))
  "Reference identity rows covering shadowing, fallback, aliases and scope.")

(defun jabber-test-action-storage--insert-rows (db)
  "Insert reference fixture rows into DB."
  (dolist (row jabber-test-action-storage--rows)
    (pcase-let ((`(,account ,peer ,type ,room ,server ,origin ,stanza ,retracted ,resource) row))
      (sqlite-execute db "INSERT INTO message
(account, peer, direction, type, body, timestamp, resource,
 room_id, server_id, origin_id, stanza_id, retracted)
VALUES (?, ?, 'in', ?, 'body', 1, ?, ?, ?, ?, ?, ?)"
                      (list account peer type resource room server origin stanza retracted)))))

(defun jabber-test-action-storage--oracle (db account peer type target)
  "Return the previous unindexed candidate rows for TARGET in DB, sorted."
  (sqlite-select db (format "SELECT id, body, retracted, direction, resource FROM message
WHERE account = ? AND peer = ? AND type = ? AND %s ORDER BY id"
                            (if (equal type "groupchat")
                                "COALESCE(room_id, server_id) = ?"
                              "(origin_id = ? OR stanza_id = ?)"))
                 (append (list account peer type target)
                         (unless (equal type "groupchat") (list target)))))

(ert-deftest jabber-test-action-storage-reference-candidates-equivalent ()
  "Indexed candidates equal the room-precedence and alias contracts exactly."
  (jabber-test-archive--with-file
    (ignore jc)
    (let ((db jabber-db--connection)
          (targets '("uid-1" "uid-2" "shadow" "legacy" "dup" "o-1" "s-1" "same"
                     "x-1" "s-2" "o-3" "absent")))
      (jabber-test-action-storage--insert-rows db)
      (dolist (scope '(("me@example.com" "room@example.com" "groupchat")
                       ("me@example.com" "room@example.com" "chat")
                       ("me@example.com" "friend@example.com" "chat")
                       ("me@example.com" "friend@example.com" "groupchat")
                       ("other@example.com" "room@example.com" "groupchat")
                       ("me@example.com" "elsewhere@example.com" "groupchat")))
        (dolist (target targets)
          (ert-info ((format "%S %s" scope target) :prefix "Lookup: ")
            (should (equal (apply #'jabber-test-action-storage--oracle db
                                  (append scope (list target)))
                           (sort (apply #'jabber-db--message-reference-candidates db
                                        (append scope (list target)))
                                 (lambda (a b) (< (car a) (car b))))))))))
    ;; Spot-check the contracts the oracle encodes.
    (let ((ids (lambda (target)
                 (mapcar #'car (jabber-db--message-reference-candidates
                                jabber-db--connection "me@example.com"
                                "room@example.com" "groupchat" target)))))
      (should (equal '(4) (funcall ids "uid-2")))
      (should (equal '(2) (funcall ids "legacy")))
      (should (equal '(5 6 7) (sort (funcall ids "dup") #'<))))))

(defun jabber-test-action-storage--plan (db type target)
  "Return native plan details of the production candidate SQL for TYPE TARGET."
  (let ((select (symbol-function 'sqlite-select)) captured)
    (cl-letf (((symbol-function 'sqlite-select)
               (lambda (connection sql &optional values &rest rest)
                 (unless captured (setq captured (list sql values)))
                 (apply select connection sql values rest))))
      (jabber-db--message-reference-candidates
       db "me@example.com" (if (equal type "groupchat") "room@example.com" "friend@example.com")
       type target))
    (mapcar (lambda (row) (nth 3 row))
            (funcall select db (concat "EXPLAIN QUERY PLAN " (car captured))
                     (cadr captured)))))

(ert-deftest jabber-test-action-storage-reference-candidates-seek-identity-indexes ()
  "Room and direct candidates seek identity indexes, never peer history."
  (jabber-test-archive--with-file
    (ignore jc)
    (let ((db jabber-db--connection))
      (dotimes (i 500)
        (sqlite-execute db "INSERT INTO message
(account, peer, direction, type, body, timestamp, room_id, server_id, origin_id, stanza_id)
VALUES ('me@example.com', ?, 'in', ?, 'history', ?, ?, ?, ?, ?)"
                        (if (cl-evenp i)
                            (list "room@example.com" "groupchat" i
                                  (format "r-%d" i) (format "r-%d" i) nil nil)
                          (list "friend@example.com" "chat" i
                                nil nil (format "o-%d" i) (format "s-%d" i)))))
      (dolist (analyzed '(nil t))
        (when analyzed (sqlite-execute db "ANALYZE"))
        (pcase-dolist (`(,type ,target ,indexes)
                       '(("groupchat" "r-2" ("idx_msg_room_id" "idx_msg_server_id"))
                         ("chat" "o-1" ("idx_msg_origin_id" "idx_msg_stanza_id"))))
          (ert-info ((format "%s analyzed=%s" type analyzed) :prefix "Plan: ")
            (let* ((plan (jabber-test-action-storage--plan db type target))
                   (searches (seq-filter (lambda (detail)
                                           (string-match-p "\\`\\(?:SEARCH\\|SCAN\\)" detail))
                                         plan)))
              (should (= 2 (length searches)))
              (dolist (detail searches)
                (should (string-prefix-p "SEARCH message USING INDEX" detail)))
              (dolist (index indexes)
                (should (seq-some (lambda (detail) (string-search (concat index " ") detail))
                                  searches))))))))))

(ert-deftest jabber-test-action-storage-reference-consumers-native ()
  "Native room/direct history serves reply previews and reactions with scope checks."
  (jabber-test-archive--with-file
    (let ((query (jabber-mam--query jc nil nil nil nil "room@example.com"))
          (other (jabber-test-mam--native-connection "other")))
      ;; Room archive row with an explicit room stanza ID.
      (let ((jabber-message-chain jabber-test-action-storage--mam-chain))
        (jabber-process-input
         jc (jabber-test-archive--result
             query "room-uid"
             '(message ((from . "room@example.com/alice") (type . "groupchat"))
                       (body () "room target")
                       (stanza-id ((xmlns . "urn:xmpp:sid:0") (by . "room@example.com")
                                   (id . "room-uid")))))))
      (jabber-test-archive--fin jc query "room-uid")
      ;; Historical null-room row and a row whose room ID shadows its server ID.
      (sqlite-execute jabber-db--connection "INSERT INTO message
(account, peer, direction, type, body, timestamp, resource, room_id, server_id) VALUES
('me@example.com', 'room@example.com', 'in', 'groupchat', 'legacy target', 1,
 'bob', NULL, 'legacy-uid'),
('me@example.com', 'room@example.com', 'in', 'groupchat', 'shadowing', 2,
 'bob', 'own-room', 'shadowed-uid')")
      ;; Direct row with origin and transport aliases.
      (let ((jabber-message-chain (list #'jabber-db--message-handler)))
        (jabber-process-input
         jc '(message ((from . "friend@example.com/phone") (type . "chat") (id . "transport"))
                      (body () "direct target")
                      (origin-id ((xmlns . "urn:xmpp:sid:0") (id . "origin"))))))
      (jabber-db-close)
      (let ((snippet
             (lambda (peer muc-p id from)
               (with-temp-buffer
                 (setq-local jabber-buffer-connection jc)
                 (if muc-p (setq-local jabber-group peer)
                   (setq-local jabber-chatting-with peer))
                 (jabber-chat--reply-context-snippet
                  (list :reply-to-id id :reply-to-jid from))))))
        (should (equal "room target"
                       (funcall snippet "room@example.com" t "room-uid" "room@example.com/alice")))
        (should-not (funcall snippet "room@example.com" t "room-uid" "room@example.com/mallory"))
        (should (equal "legacy target"
                       (funcall snippet "room@example.com" t "legacy-uid" "room@example.com/bob")))
        (should-not (funcall snippet "room@example.com" t "shadowed-uid" "room@example.com/bob"))
        (should (equal "shadowing"
                       (funcall snippet "room@example.com" t "own-room" "room@example.com/bob")))
        (dolist (alias '("origin" "transport"))
          (should (equal "direct target"
                         (funcall snippet "friend@example.com" nil alias
                                  "friend@example.com/phone")))
          (should-not (funcall snippet "friend@example.com" nil alias "friend@example.com/laptop")))
        (should-not (funcall snippet "room@example.com" nil "room-uid" "room@example.com/alice")))
      ;; Native reactions resolve the same candidates and respect account scope.
      (let ((react
             (lambda (connection from type target)
               (let ((jabber-message-chain (list #'jabber-reactions--handle-message)))
                 (jabber-process-input
                  connection
                  `(message ((from . ,from) (type . ,type))
                            (reactions ((xmlns . "urn:xmpp:reactions:0") (id . ,target))
                                       (reaction () "👍"))))))))
        (funcall react jc "room@example.com/carol" "groupchat" "room-uid")
        (funcall react jc "room@example.com/carol" "groupchat" "legacy-uid")
        (funcall react jc "room@example.com/carol" "groupchat" "shadowed-uid")
        (funcall react jc "friend@example.com/phone" "chat" "origin")
        (funcall react other "room@example.com/carol" "groupchat" "room-uid")
        (should (equal '(("direct target" "friend@example.com")
                         ("legacy target" "room@example.com/carol")
                         ("room target" "room@example.com/carol"))
                       (sqlite-select jabber-db--connection
                                      "SELECT body, sender FROM message_reaction
JOIN message ON message.id = message_reaction.message_id ORDER BY body")))))))

(provide 'jabber-test-action-storage)
;;; jabber-test-action-storage.el ends here
