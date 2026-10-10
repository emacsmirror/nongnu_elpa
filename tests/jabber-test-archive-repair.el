;;; jabber-test-archive-repair.el --- Historical room reconciliation -*- lexical-binding: t; -*-

;;; Commentary:
;; Schema 13 split rows must repair on open, without content-based discovery.

;;; Code:

(require 'jabber-archive-test-helpers)

(defun jabber-test-archive-repair--split (jc)
  "Return (OLD LIVE) historical split row IDs for JC."
  (let ((old (jabber-db-store-message
              "me@example.com" "room@conference.example.com" "in" "groupchat"
              "A repeatable message" 1735689600 "nick")))
    (sqlite-execute jabber-db--connection
                    "INSERT INTO message_archive VALUES (?, ?, ?, ?)"
                    (list "me@example.com" "room@conference.example.com" "room-uid" old))
    (list old
          (jabber-db-store-message
           (jabber-connection-bare-jid jc) "room@conference.example.com"
           "in" "groupchat" "A repeatable message" 1735689600 "nick"
           "transport" "room-uid" nil nil nil nil nil
           '(:room-id "room-uid")))))

(defun jabber-test-archive-repair--open ()
  "Reopen the fixture as an existing schema 13 database."
  (sqlite-execute jabber-db--connection "PRAGMA user_version=13")
  (jabber-db-close)
  (jabber-db-ensure-open))

(ert-deftest jabber-test-archive-repair-open-replay ()
  "Opening repairs a historical split and replay accepts canonical ownership."
  (jabber-test-archive--with-file
    (let* ((ids (jabber-test-archive-repair--split jc))
           (live (cadr ids)))
      (jabber-test-archive-repair--open)
      (should (equal (sqlite-select jabber-db--connection "SELECT id FROM message")
                     (list (list live))))
      (should (equal (sqlite-select jabber-db--connection "SELECT message_id FROM message_archive")
                     (list (list live))))
      (let ((query (jabber-mam--query jc nil nil nil nil "room@conference.example.com")))
        (jabber-mam--process-message jc
         (jabber-test-archive--result
          query "room-uid"
          '(message ((from . "room@conference.example.com/nick")
                     (type . "groupchat"))
                    (body () "A repeatable message"))))
        (jabber-test-archive--fin jc query "room-uid"))
      (should (equal '("room-uid" nil)
                     (jabber-test-archive--progress nil "room@conference.example.com")))
      (jabber-db-close)
      (jabber-db-ensure-open)
      (should (= 14 (caar (sqlite-select jabber-db--connection "PRAGMA user_version"))))
      (should (= 1 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message")))))))

(ert-deftest jabber-test-archive-repair-references ()
  "Preserve metadata, relational references, FTS and unrelated rows."
  (jabber-test-archive--with-file
    (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
      (sqlite-execute jabber-db--connection "UPDATE message SET delivered_at=7, thread_id='thread' WHERE id=?" (list old))
      (sqlite-execute jabber-db--connection "UPDATE message SET displayed_at=8 WHERE id=?" (list live))
      (sqlite-execute jabber-db--connection "INSERT INTO message_oob(message_id,url,desc) VALUES (?, 'url', 'description')" (list old))
      (sqlite-execute jabber-db--connection "INSERT INTO message_reaction VALUES (?, 'actor', 'like', 9)" (list old))
      (sqlite-execute jabber-db--connection "INSERT INTO message_reaction_actor VALUES (?, 'actor', 9)" (list old))
      (sqlite-execute jabber-db--connection "INSERT INTO message_thread(account,peer,type,thread_id,root_message_id,created_at,read_message_id,title) VALUES ('me@example.com','room@conference.example.com','groupchat','thread',?,1,?,'title')" (list old live))
      (let ((unrelated (jabber-db-store-message "me@example.com" "room@conference.example.com" "in" "groupchat" "A repeatable message" 1735689600 "nick")))
        (jabber-test-archive-repair--open)
        (should (equal (sqlite-select jabber-db--connection "SELECT id FROM message ORDER BY id") (list (list live) (list unrelated)))))
      (should (equal '((7 8 "thread")) (sqlite-select jabber-db--connection "SELECT delivered_at,displayed_at,thread_id FROM message WHERE id=?" (list live))))
      (dolist (table '("message_archive" "message_oob" "message_reaction" "message_reaction_actor"))
        (should (equal (list (list live)) (sqlite-select jabber-db--connection (format "SELECT message_id FROM %s" table)))))
      (should (equal (list (list live live "title")) (sqlite-select jabber-db--connection "SELECT root_message_id,read_message_id,title FROM message_thread")))
      (should-not (sqlite-select jabber-db--connection "PRAGMA foreign_key_check"))
      (should (equal '(("ok")) (sqlite-select jabber-db--connection "PRAGMA integrity_check")))
      (sqlite-execute jabber-db--connection "INSERT INTO message_fts(message_fts,rank) VALUES ('integrity-check',1)")
      (should (= 2 (length (sqlite-select jabber-db--connection "SELECT rowid FROM message_fts WHERE message_fts MATCH 'repeatable'")))))))

(ert-deftest jabber-test-archive-repair-conflicts ()
  "Conflicting content, sender, encryption or metadata fails closed."
  (dolist (assignment '("body='different'" "resource='other'" "encrypted=1"
                        "occupant_id='other'" "edited=1" "retracted=1"
                        "thread_id='different'" "server_id='conflicting'"))
    (jabber-test-archive--with-file
      (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
        (sqlite-execute jabber-db--connection "UPDATE message SET occupant_id='known',thread_id='thread' WHERE id=?" (list old))
        (sqlite-execute jabber-db--connection (concat "UPDATE message SET " assignment " WHERE id=?") (list live))
        (jabber-test-archive-repair--open)
        (should (= 2 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))
        (should (equal (list (list old)) (sqlite-select jabber-db--connection "SELECT message_id FROM message_archive")))))))

(ert-deftest jabber-test-archive-repair-ambiguity ()
  "A room ID collision remains ambiguous even with incompatible content."
  (jabber-test-archive--with-file
    (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
      (sqlite-execute jabber-db--connection "INSERT INTO message(account,peer,type,direction,timestamp,resource,body,room_id) SELECT account,peer,type,direction,timestamp,resource,'different',room_id FROM message WHERE id=?" (list live))
      (jabber-test-archive-repair--open)
      (should (= 3 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))
      (should (equal (list (list old)) (sqlite-select jabber-db--connection "SELECT message_id FROM message_archive"))))))

(ert-deftest jabber-test-archive-repair-rollback ()
  "A failure after reparenting restores all rows, references and version."
  (jabber-test-archive--with-file
    (let* ((ids (jabber-test-archive-repair--split jc))
           (old (car ids)))
      (sqlite-execute jabber-db--connection "PRAGMA user_version=13")
      (sqlite-execute jabber-db--connection "CREATE TRIGGER repair_failure BEFORE DELETE ON message BEGIN SELECT RAISE(ABORT, 'injected'); END")
      (should-error (jabber-db--migrate jabber-db--connection))
      (should (= 13 (caar (sqlite-select jabber-db--connection "PRAGMA user_version"))))
      (should (= 2 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))
      (should (equal (list (list old)) (sqlite-select jabber-db--connection "SELECT message_id FROM message_archive"))))))

(ert-deftest jabber-test-archive-repair-scopes ()
  "Personal archives, foreign accounts and foreign rooms cannot authorize repair."
  (dolist (assignment '("archive='me@example.com'" "account='other@example.com'"
                        "archive='other@conference.example.com'"))
    (jabber-test-archive--with-file
      (jabber-test-archive-repair--split jc)
      (sqlite-execute jabber-db--connection (concat "UPDATE message_archive SET " assignment))
      (jabber-test-archive-repair--open)
      (should (= 2 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message")))))))

(ert-deftest jabber-test-archive-repair-encrypted-and-timestamp ()
  "Keep identical decrypted text and encrypted state; prefer archive time."
  (jabber-test-archive--with-file
    (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
      (sqlite-execute jabber-db--connection "UPDATE message SET encrypted=1")
      (sqlite-execute jabber-db--connection "UPDATE message SET timestamp=timestamp+2 WHERE id=?" (list live))
      (jabber-test-archive-repair--open)
      (should (equal (list (list live "A repeatable message" 1 1735689600))
                     (sqlite-select jabber-db--connection "SELECT id,body,encrypted,timestamp FROM message")))
      (should-not (sqlite-select jabber-db--connection "SELECT id FROM message WHERE id=?" (list old))))))

(ert-deftest jabber-test-archive-repair-reaction-snapshots ()
  "Equal actor snapshots collapse; differing and empty snapshots fail closed."
  (dolist (snapshot '(same different empty))
    (jabber-test-archive--with-file
      (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
        (dolist (id (list old live))
          (sqlite-execute jabber-db--connection "INSERT INTO message_reaction_actor VALUES (?, 'actor', 9)" (list id)))
        (sqlite-execute jabber-db--connection "INSERT INTO message_reaction VALUES (?, 'actor', 'like', 9)" (list old))
        (unless (eq snapshot 'empty)
          (sqlite-execute jabber-db--connection "INSERT INTO message_reaction VALUES (?, 'actor', ?, 9)" (list live (if (eq snapshot 'same) "like" "other"))))
        (jabber-test-archive-repair--open)
        (should (= (if (eq snapshot 'same) 1 2) (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))
        (when (eq snapshot 'same)
          (should (equal (list (list live "actor" "like" 9)) (sqlite-select jabber-db--connection "SELECT * FROM message_reaction")))
          (should (equal (list (list live "actor" 9)) (sqlite-select jabber-db--connection "SELECT * FROM message_reaction_actor"))))))))

(ert-deftest jabber-test-archive-repair-foreign-references ()
  "References with conflicting ownership and room occurrences remain untouched."
  (dolist (kind '(thread archive occurrence))
    (jabber-test-archive--with-file
      (let ((old (car (jabber-test-archive-repair--split jc))))
        (pcase kind
          ('thread (sqlite-execute jabber-db--connection "INSERT INTO message_thread(account,peer,type,thread_id,root_message_id,created_at) VALUES ('other','foreign','chat','thread',?,1)" (list old)))
          ('archive (sqlite-execute jabber-db--connection "INSERT INTO message_archive VALUES ('other','personal','uid',?)" (list old)))
          ('occurrence (sqlite-execute jabber-db--connection "INSERT INTO message_archive VALUES ('me@example.com','room@conference.example.com','different',?)" (list old))))
        (jabber-test-archive-repair--open)
        (should (= 2 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))))))

(ert-deftest jabber-test-archive-repair-v14-faults ()
  "Every write boundary rolls back on error/quit; reopen and retry succeeds."
  (dolist (failure '(error quit))
    (dotimes (boundary 12)
      (jabber-test-archive--with-file
        (pcase-let* ((`(,old ,live) (jabber-test-archive-repair--split jc))
                     (execute (symbol-function 'sqlite-execute))
                     (count 0))
          (sqlite-execute jabber-db--connection "PRAGMA user_version=13")
          (sqlite-execute jabber-db--connection "DROP INDEX idx_archive_message_id")
          (let ((schema (sqlite-select jabber-db--connection "SELECT type,name,sql FROM sqlite_master ORDER BY name")))
            (cl-letf (((symbol-function 'sqlite-execute)
                       (lambda (db sql &rest args)
                         (prog1 (apply execute db sql args)
                           (unless (string-match-p "\\`\\(?:SAVEPOINT\\|RELEASE\\|ROLLBACK\\)" sql)
                             (when (= (prog1 count (cl-incf count)) boundary)
                               (signal failure '("injected"))))))))
              (should (eq failure (condition-case err
                                      (progn (jabber-db--migrate jabber-db--connection) nil)
                                    ((error quit) (car err))))))
            (should (equal schema (sqlite-select jabber-db--connection "SELECT type,name,sql FROM sqlite_master ORDER BY name"))))
          (should (= 13 (caar (sqlite-select jabber-db--connection "PRAGMA user_version"))))
          (should (= 2 (caar (sqlite-select jabber-db--connection "SELECT count(*) FROM message"))))
          (should (equal (list (list old)) (sqlite-select jabber-db--connection "SELECT message_id FROM message_archive")))
          (jabber-db-close)
          (jabber-db-ensure-open)
          (should (equal (list (list live)) (sqlite-select jabber-db--connection "SELECT id FROM message"))))))))

(ert-deftest jabber-test-archive-repair-read-boundaries ()
  "Never cross a read boundary, in either direction or with external watermarks."
  (dolist (reverse '(nil t))
    (dolist (watermark '(old live middle))
      (jabber-test-archive--with-file
        (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
          ;; Reserve an intervening unrelated reply and put the pair around it.
          (sqlite-execute jabber-db--connection "PRAGMA foreign_keys=OFF")
          (sqlite-execute jabber-db--connection "UPDATE message SET id=4 WHERE id=?" (list (if reverse old live)))
          (if reverse
              (progn
                (sqlite-execute jabber-db--connection "UPDATE message_archive SET message_id=4")
                (setq old 4))
            (setq live 4))
          (sqlite-execute jabber-db--connection "PRAGMA foreign_keys=ON")
          (sqlite-execute jabber-db--connection "UPDATE message SET thread_id='thread'")
          (sqlite-execute jabber-db--connection "INSERT INTO message(id,account,peer,type,direction,body,timestamp,thread_id) VALUES (3,'me@example.com','room@conference.example.com','groupchat','in','unrelated reply',1,'thread')")
          (sqlite-execute jabber-db--connection "INSERT INTO message_thread(account,peer,type,thread_id,created_at,read_message_id,dedicated) VALUES ('me@example.com','room@conference.example.com','groupchat','thread',1,?,1)"
                          (list (pcase watermark ('old old) ('live live) (_ 3))))
          (let ((before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id"))
                (thread (sqlite-select jabber-db--connection "SELECT * FROM message_thread"))
                (unread (plist-get (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread") :unread)))
            (jabber-test-archive-repair--open)
            ;; A watermark at the higher LIVE is already safe and is not remapped.
            (unless (and (not reverse) (eq watermark 'live))
              (should (equal before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id")))
              (should (equal thread (sqlite-select jabber-db--connection "SELECT * FROM message_thread"))))
            (should (eq unread (plist-get (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread") :unread)))))))))

(ert-deftest jabber-test-archive-repair-both-row-references ()
  "Check ownership and conflicting UIDs on both rows, including non-FK refs."
  (dolist (which '(old live))
    (dolist (kind '(root read foreign-account foreign-archive contradictory-uid))
      (jabber-test-archive--with-file
        (pcase-let* ((`(,old ,live) (jabber-test-archive-repair--split jc))
                     (id (if (eq which 'old) old live)))
          (pcase kind
            ((or 'root 'read)
             (sqlite-execute jabber-db--connection
                             (format "INSERT INTO message_thread(account,peer,type,thread_id,created_at,%s) VALUES ('other','foreign','chat','thread',1,?)"
                                     (if (eq kind 'root) "root_message_id" "read_message_id")) (list id)))
            ('foreign-account (sqlite-execute jabber-db--connection "INSERT INTO message_archive VALUES ('other','personal','uid',?)" (list id)))
            ('foreign-archive (sqlite-execute jabber-db--connection "INSERT INTO message_archive VALUES ('me@example.com','foreign','uid',?)" (list id)))
            ('contradictory-uid (sqlite-execute jabber-db--connection "INSERT INTO message_archive VALUES ('me@example.com','room@conference.example.com','different',?)" (list id))))
          (let ((messages (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id"))
                (archives (sqlite-select jabber-db--connection "SELECT * FROM message_archive ORDER BY account,archive,uid"))
                (threads (sqlite-select jabber-db--connection "SELECT * FROM message_thread")))
            (jabber-test-archive-repair--open)
            (should (equal messages (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id")))
            (should (equal archives (sqlite-select jabber-db--connection "SELECT * FROM message_archive ORDER BY account,archive,uid")))
            (should (equal threads (sqlite-select jabber-db--connection "SELECT * FROM message_thread")))))))))

(ert-deftest jabber-test-archive-repair-oob-lossless ()
  "Preserve attachment IDs, URLs and distinct descriptions without guesses."
  (jabber-test-archive--with-file
    (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
      (dolist (entry (list (list old "same-url" "archive description")
                          (list live "same-url" "live description")
                          (list old "same-url" nil)
                          (list live "other-url" "other")
                          (list old "same-url" "live description")))
        (sqlite-execute jabber-db--connection "INSERT INTO message_oob(message_id,url,desc) VALUES (?,?,?)" entry))
      (let ((before (sqlite-select jabber-db--connection "SELECT id,url,desc FROM message_oob ORDER BY id")))
        (jabber-test-archive-repair--open)
        (should (equal before (sqlite-select jabber-db--connection "SELECT id,url,desc FROM message_oob ORDER BY id")))
        (should (equal (list (list live)) (sqlite-select jabber-db--connection "SELECT DISTINCT message_id FROM message_oob")))))))

(ert-deftest jabber-test-archive-repair-root-unread ()
  "Do not erase an unread reply by collapsing it into a thread root."
  (dolist (root '(old live server stanza))
    (jabber-test-archive--with-file
      (pcase-let ((`(,old ,live) (jabber-test-archive-repair--split jc)))
        (sqlite-execute jabber-db--connection "UPDATE message SET thread_id='thread'")
        (sqlite-execute jabber-db--connection "INSERT INTO message_thread(account,peer,type,thread_id,created_at,dedicated,root_message_id,root_server_id,root_stanza_id) VALUES ('me@example.com','room@conference.example.com','groupchat','thread',1,1,?,?,?)"
                        (list (pcase root ('old old) ('live live))
                              (and (eq root 'server) "room-uid")
                              (and (eq root 'stanza) "transport")))
        (let ((before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id"))
              (summary (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread")))
          (jabber-test-archive-repair--open)
          (should (equal before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id")))
          (should (equal summary (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread"))))))))

(ert-deftest jabber-test-archive-repair-unmatched-implicit-root ()
  "Preserve NULL-sensitive unread state when thread metadata moves to LIVE."
  (dolist (kind '(server stanza))
    (jabber-test-archive--with-file
      (pcase-let ((`(,old ,_live) (jabber-test-archive-repair--split jc)))
        (sqlite-execute jabber-db--connection "UPDATE message SET thread_id='thread' WHERE id=?" (list old))
        (sqlite-execute jabber-db--connection
                        (format "INSERT INTO message_thread(account,peer,type,thread_id,created_at,dedicated,%s) VALUES ('me@example.com','room@conference.example.com','groupchat','thread',1,1,'unmatched')"
                                (if (eq kind 'server) "root_server_id" "root_stanza_id")))
        (let ((before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id"))
              (summary (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread")))
          (jabber-test-archive-repair--open)
          (should (equal before (sqlite-select jabber-db--connection "SELECT * FROM message ORDER BY id")))
          (should (equal summary (jabber-db-message-thread-summary "me@example.com" "room@conference.example.com" "groupchat" "thread"))))))))

(provide 'jabber-test-archive-repair)
;;; jabber-test-archive-repair.el ends here
