;;; jabber-test-archive-migration.el --- Archive schema migration proof -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'jabber-db)

(defconst jabber-test-archive-migration--fixture
  (expand-file-name "fixtures/archive-schema-v11.eld"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Literal historical schema, never synthesized from the candidate schema.")

(defun jabber-test-archive-migration--populate (db)
  "Create literal v11 tables and representative retained state in DB."
  (with-temp-buffer
    (insert-file-contents jabber-test-archive-migration--fixture)
    (dolist (ddl (read (current-buffer)))
      (sqlite-execute db ddl)))
  (dolist (sql '("PRAGMA user_version=11"
                "INSERT INTO message (id, account, peer, direction, type, stanza_id, server_id, body, timestamp, thread_id, retracted, retracted_by) VALUES (1,'me','peer','in','chat','transport','unknown-legacy','retained',1,'thread',1,'actor')"
                "INSERT INTO message_oob (message_id,url,desc) VALUES (1,'https://example.org/file','file')"
                "INSERT INTO message_reaction VALUES (1,'actor','λ',9)"
                "INSERT INTO message_reaction_actor VALUES (1,'actor',11)"
                "INSERT INTO message_thread (account,peer,type,thread_id,root_message_id,created_at,read_message_id,dedicated,title) VALUES ('me','peer','chat','thread',1,1,1,1,'local title')"
                "INSERT INTO chat_settings VALUES ('me','peer','omemo','thread')"
                "INSERT INTO omemo_store VALUES ('me',x'000102',123)"
                "INSERT INTO omemo_sessions VALUES ('me','peer',7,x'030405')"
                "INSERT INTO omemo_trust VALUES ('me','peer',7,x'0607',1,4)"
                "INSERT INTO omemo_skipped_keys VALUES ('me','peer',7,x'0809',3,x'1011',12)"
                "INSERT INTO omemo_devices VALUES ('me','peer',7,1,12)"
                "INSERT INTO omemo_device_id VALUES ('me',7)"
                "INSERT INTO caps_cache VALUES ('sha-1','ver','identities','features')"))
    (sqlite-execute db sql)))

(defun jabber-test-archive-migration--data (db &optional columns)
  "Snapshot DB's application data, using historical COLUMNS when supplied."
  (mapcar
   (lambda (row)
     (let* ((table (car row))
            (names (or (cadr (assoc table columns))
                       (mapcar #'cadr (sqlite-select db (format "PRAGMA table_info(%s)" table))))))
       (list table names
             (sqlite-select db (format "SELECT %s FROM %s ORDER BY rowid"
                                       (mapconcat #'identity names ",") table)))))
   (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table'
AND name NOT LIKE 'message_fts%' AND name NOT LIKE 'sqlite_%'
AND name NOT IN ('mam_progress','message_archive') ORDER BY name")))

(defun jabber-test-archive-migration--schema (db)
  "Return DB's complete schema and version for rollback comparisons."
  (list (sqlite-select db "PRAGMA user_version")
        (sqlite-select db "SELECT type,name,tbl_name,sql FROM sqlite_master ORDER BY type,name")))

(ert-deftest jabber-test-archive-migration-populated-upgrade-and-fresh ()
  "Preserve all retained state; fresh and migrated tables have equal columns."
  (let ((db (sqlite-open)) (fresh (sqlite-open)))
    (unwind-protect
        (progn
          (jabber-test-archive-migration--populate db)
          (let ((before (jabber-test-archive-migration--data db)))
            (jabber-db--migrate db)
            (should (equal before (jabber-test-archive-migration--data db before)))
            (should (equal '((nil nil "unknown-legacy"))
                           (sqlite-select db "SELECT origin_id,room_id,server_id FROM message")))
            (should-not (sqlite-select db "SELECT * FROM mam_progress"))
            (should-not (sqlite-select db "SELECT * FROM message_archive"))
            (should (equal '((1)) (sqlite-select db "SELECT rowid FROM message_fts WHERE message_fts MATCH 'retained'")))
            (should (equal '(("ok")) (sqlite-select db "PRAGMA integrity_check")))
            (should-not (sqlite-select db "PRAGMA foreign_key_check")))
          (jabber-db--migrate fresh)
          (should (equal (list (list jabber-db--schema-version)) (sqlite-select db "PRAGMA user_version")))
          (should (equal (sqlite-select db "SELECT type,name FROM sqlite_master ORDER BY type,name")
                         (sqlite-select fresh "SELECT type,name FROM sqlite_master ORDER BY type,name")))
          (dolist (table (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table'"))
            (let ((sql (format "PRAGMA table_info(%s)" (car table))))
              (should (equal (sqlite-select db sql) (sqlite-select fresh sql))))))
      (sqlite-close db)
      (sqlite-close fresh))))

(ert-deftest jabber-test-archive-migration-faults-reopen-retry ()
  "Roll back each DDL/version boundary on error/quit, close, reopen and retry."
  (dolist (failure '(error quit))
    (dotimes (boundary 5)
      (let* ((dir (make-temp-file "jabber-migration-" t))
             (jabber-db-path (expand-file-name "history.sqlite" dir))
             (jabber-db--connection nil)
             (raw (sqlite-open jabber-db-path))
             (execute (symbol-function 'sqlite-execute))
             (count 0) before schema)
        (unwind-protect
            (progn
              (jabber-test-archive-migration--populate raw)
              (setq before (jabber-test-archive-migration--data raw)
                    schema (jabber-test-archive-migration--schema raw))
              (sqlite-close raw)
              (cl-letf (((symbol-function 'sqlite-execute)
                         (lambda (db sql &rest args)
                           (prog1 (apply execute db sql args)
                             (when (or (string-prefix-p "ALTER TABLE message ADD COLUMN" sql)
                                       (string-prefix-p "CREATE TABLE IF NOT EXISTS mam_progress" sql)
                                       (string-prefix-p "CREATE TABLE IF NOT EXISTS message_archive" sql)
                                       (equal sql "PRAGMA user_version=12"))
                               (when (= (prog1 count (cl-incf count)) boundary)
                                 (signal failure '("Migration fault"))))))))
                (should (eq failure
                            (condition-case err
                                (progn (jabber-db-ensure-open) nil)
                              ((error quit) (car err))))))
              (should-not jabber-db--connection)
              (setq raw (sqlite-open jabber-db-path))
              (should (equal schema (jabber-test-archive-migration--schema raw)))
              (should (equal before (jabber-test-archive-migration--data raw)))
              (sqlite-close raw)
              (jabber-db-ensure-open)
              (should (equal before (jabber-test-archive-migration--data jabber-db--connection before)))
              (jabber-db-close)
              (jabber-db-ensure-open)
              (should (equal (list (list jabber-db--schema-version)) (sqlite-select jabber-db--connection "PRAGMA user_version"))))
          (jabber-db-close)
          (ignore-errors (sqlite-close raw))
          (delete-directory dir t))))))

(ert-deftest jabber-test-archive-migration-nonlocal-exit-preserves-owner ()
  "A nonlocal exit inside a caller transaction preserves its earlier work."
  (let ((db (sqlite-open)) (execute (symbol-function 'sqlite-execute)))
    (unwind-protect
        (progn
          (jabber-test-archive-migration--populate db)
          (sqlite-execute db "BEGIN")
          (sqlite-execute db "UPDATE message SET body='owner work' WHERE id=1")
          (should (eq 'stopped
                      (catch 'stop
                        (cl-letf (((symbol-function 'sqlite-execute)
                                   (lambda (connection sql &rest args)
                                     (prog1 (apply execute connection sql args)
                                       (when (equal sql "PRAGMA user_version=13")
                                         (throw 'stop 'stopped))))))
                          (jabber-db--migrate db)))))
          (should (equal '((11)) (sqlite-select db "PRAGMA user_version")))
          (should (equal '(("owner work")) (sqlite-select db "SELECT body FROM message")))
          (sqlite-execute db "COMMIT")
          (jabber-db--migrate db)
          (should (equal (list (list jabber-db--schema-version)) (sqlite-select db "PRAGMA user_version"))))
      (sqlite-close db))))

(defun jabber-test-archive-migration--v12 (db)
  "Populate historical v12 data in DB, including identities and archive state."
  (jabber-test-archive-migration--populate db)
  (jabber-db--migrate-v11-to-v12 db)
  (sqlite-execute db "UPDATE message SET origin_id='origin',room_id='room'")
  (sqlite-execute db "INSERT INTO message_archive VALUES ('me','archive','uid',1)")
  (sqlite-execute db "INSERT INTO mam_progress VALUES ('me','archive','','uid',NULL)"))

(ert-deftest jabber-test-archive-migration-v13-preserves-data-and-indexes ()
  "Add the same nonunique indexes as fresh schema without changing any data."
  (let ((db (sqlite-open)) (fresh (sqlite-open)))
    (unwind-protect
        (progn
          (jabber-test-archive-migration--v12 db)
          (let ((before (jabber-test-archive-migration--data db))
                (archive (sqlite-select db "SELECT * FROM message_archive"))
                (progress (sqlite-select db "SELECT * FROM mam_progress")))
            (jabber-db--migrate db)
            (should (equal before (jabber-test-archive-migration--data db)))
            (should (equal archive (sqlite-select db "SELECT * FROM message_archive")))
            (should (equal progress (sqlite-select db "SELECT * FROM mam_progress"))))
          (jabber-db--migrate fresh)
          (dolist (name '("idx_msg_origin_id" "idx_msg_room_id"))
            (let ((sql "SELECT sql FROM sqlite_master WHERE name=?"))
              (should (equal (sqlite-select db sql (list name))
                             (sqlite-select fresh sql (list name))))))
          ;; Identity collisions remain stored, never suppressed by a UNIQUE index.
          (sqlite-execute db "INSERT INTO message
(account,peer,direction,type,body,timestamp,origin_id,room_id)
VALUES ('me','peer','in','chat','collision',2,'origin','room')")
          (should (equal '((2)) (sqlite-select db "SELECT count(*) FROM message WHERE origin_id='origin'")))
          (should (equal '(("ok")) (sqlite-select db "PRAGMA integrity_check")))
          (should-not (sqlite-select db "PRAGMA foreign_key_check")))
      (sqlite-close db)
      (sqlite-close fresh))))

(ert-deftest jabber-test-archive-migration-v13-faults-reopen-retry ()
  "Roll back both indexes/version on error or quit, reopen and retry safely."
  (dolist (failure '(error quit))
    (dotimes (boundary 3)
      (let* ((dir (make-temp-file "jabber-v13-" t))
             (jabber-db-path (expand-file-name "history.sqlite" dir))
             (jabber-db--connection nil)
             (raw (sqlite-open jabber-db-path))
             (execute (symbol-function 'sqlite-execute))
             (count 0) before schema archive progress)
        (unwind-protect
            (progn
              (jabber-test-archive-migration--v12 raw)
              (setq before (jabber-test-archive-migration--data raw)
                    schema (jabber-test-archive-migration--schema raw)
                    archive (sqlite-select raw "SELECT * FROM message_archive")
                    progress (sqlite-select raw "SELECT * FROM mam_progress"))
              (sqlite-close raw)
              (cl-letf (((symbol-function 'sqlite-execute)
                         (lambda (db sql &rest args)
                           (prog1 (apply execute db sql args)
                             (when (or (member sql jabber-db--identity-index-ddl)
                                       (equal sql "PRAGMA user_version=13"))
                               (when (= (prog1 count (cl-incf count)) boundary)
                                 (signal failure '("Index migration fault"))))))))
                (should (eq failure
                            (condition-case err
                                (progn (jabber-db-ensure-open) nil)
                              ((error quit) (car err))))))
              (should-not jabber-db--connection)
              (setq raw (sqlite-open jabber-db-path))
              (should (equal schema (jabber-test-archive-migration--schema raw)))
              (should (equal before (jabber-test-archive-migration--data raw)))
              (should (equal archive (sqlite-select raw "SELECT * FROM message_archive")))
              (should (equal progress (sqlite-select raw "SELECT * FROM mam_progress")))
              (sqlite-close raw)
              (jabber-db-ensure-open)
              (should (equal before (jabber-test-archive-migration--data jabber-db--connection)))
              (should (equal '((13)) (sqlite-select jabber-db--connection "PRAGMA user_version"))))
          (jabber-db-close)
          (ignore-errors (sqlite-close raw))
          (delete-directory dir t))))))

(provide 'jabber-test-archive-migration)
;;; jabber-test-archive-migration.el ends here
