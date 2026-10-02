;;; jabber-db.el --- SQLite message storage for jabber.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-jabber contributors
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; SQLite-based message storage for jabber.el, replacing flat-file
;; history.  Requires Emacs 29.1+ built-in `sqlite' support.
;;
;; Provides:
;; - Persistent message storage with full-text search (FTS5)
;; - Backlog retrieval compatible with `jabber-chat-insert-backlog-entry'
;; - Paginated queries and FTS5 search
;; - XEP-0359 stanza-id / server-id columns for future MAM dedup
;; - One-time migration from flat-file history

;;; Code:

(require 'subr-x)
(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-message-thread-protocol)
(require 'jabber-muc-protocol)
(require 'jabber-muc-state)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-chat-send-hooks)        ; jabber-chat.el
(defvar jabber-chat-encryption)        ; jabber-chatbuffer.el
(defvar jabber-chat--sending-correction) ; jabber-chat.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-message-chain nil)       ; jabber-core.el
(defvar jabber-post-connect-hooks nil) ; jabber-core.el
(defvar jabber-pre-disconnect-hook nil) ; jabber-core.el
(defvar jabber-oob-xmlns)              ; jabber-xml.el

(defgroup jabber-db nil
  "SQLite message storage for jabber.el."
  :group 'jabber)

(defcustom jabber-db-path
  (expand-file-name "jabber/jabber.db" user-emacs-directory)
  "Path to the SQLite database file for message storage.
Set to nil to disable message storage entirely."
  :type '(choice (file :tag "Database file")
                 (const :tag "Disabled" nil)))

(defcustom jabber-backlog-days nil
  "Age limit on messages in chat buffer backlog, in days."
  :type '(choice (number :tag "Number of days")
                 (const :tag "No limit" nil)))

(defcustom jabber-backlog-number 30
  "Maximum number of messages in chat buffer backlog."
  :type 'integer)

(defvar jabber-history-inhibit-received-message-functions nil
  "Functions determining whether to log an incoming message stanza.
The functions in this list are called with two arguments,
the connection and the full message stanza.
If any of the functions returns non-nil, the stanza is not logged
in the message history.")

(defvar jabber-db-message-thread-stored-functions nil
  "Functions run after a threaded message is stored.
Each function receives ACCOUNT, PEER, TYPE, THREAD-ID, and TIMESTAMP.")

;;; Database connection

(defvar jabber-db--connection nil
  "Active SQLite database connection, or nil.")

(defconst jabber-db--schema-ddl
  '("CREATE TABLE IF NOT EXISTS message (
  id           INTEGER PRIMARY KEY,
  stanza_id    TEXT,
  server_id    TEXT,
  account      TEXT NOT NULL,
  peer         TEXT NOT NULL,
  resource     TEXT,
  occupant_id  TEXT,
  direction    TEXT NOT NULL CHECK(direction IN ('in','out')),
  type         TEXT CHECK(type IN ('chat','groupchat','headline')),
  body         TEXT,
  timestamp    INTEGER NOT NULL,
  encrypted    INTEGER DEFAULT 0,
  delivered_at INTEGER,
  displayed_at INTEGER,
  retracted_by TEXT,
  retraction_reason TEXT,
  edited       INTEGER DEFAULT 0,
  reply_to_id  TEXT,
  reply_to_jid TEXT,
  fallback_start INTEGER,
  fallback_end INTEGER,
  thread_id TEXT,
  thread_parent_id TEXT,
  retracted INTEGER NOT NULL DEFAULT 0,
  origin_id TEXT,
  room_id TEXT)"
    "CREATE INDEX IF NOT EXISTS idx_msg_peer_ts
  ON message(account, peer, timestamp)"
    "CREATE INDEX IF NOT EXISTS idx_msg_stanza_id
  ON message(account, stanza_id) WHERE stanza_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_server_id
  ON message(account, server_id) WHERE server_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_occupant_id
  ON message(account, peer, occupant_id) WHERE occupant_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_thread
  ON message(account, peer, type, thread_id, timestamp)
  WHERE thread_id IS NOT NULL"
    "CREATE TABLE IF NOT EXISTS message_thread (
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  type TEXT NOT NULL,
  thread_id TEXT NOT NULL,
  parent_thread_id TEXT,
  root_message_id INTEGER,
  root_stanza_id TEXT,
  root_server_id TEXT,
  created_at INTEGER NOT NULL,
  read_message_id INTEGER,
  dedicated INTEGER NOT NULL DEFAULT 0,
  title TEXT,
  PRIMARY KEY (account, peer, type, thread_id))"
    "CREATE VIRTUAL TABLE IF NOT EXISTS message_fts USING fts5(
  body, content='message', content_rowid='id')"
    "CREATE TRIGGER IF NOT EXISTS message_ai AFTER INSERT ON message BEGIN
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END"
    "CREATE TRIGGER IF NOT EXISTS message_ad AFTER DELETE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
END"
    "CREATE TRIGGER IF NOT EXISTS message_au AFTER UPDATE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END"
    "CREATE TABLE IF NOT EXISTS omemo_store (
  account TEXT PRIMARY KEY,
  store_blob BLOB NOT NULL,
  spk_rotated_at INTEGER)"
    "CREATE TABLE IF NOT EXISTS omemo_sessions (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  session_blob BLOB NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE TABLE IF NOT EXISTS omemo_trust (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  identity_key BLOB NOT NULL,
  trust INTEGER DEFAULT 0,
  first_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE TABLE IF NOT EXISTS omemo_skipped_keys (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  dh_key BLOB NOT NULL,
  message_number INTEGER NOT NULL,
  message_key BLOB NOT NULL,
  created_at INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id, dh_key, message_number))"
    "CREATE TABLE IF NOT EXISTS omemo_devices (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  active INTEGER DEFAULT 1,
  last_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE INDEX IF NOT EXISTS idx_omemo_trust_jid
  ON omemo_trust (account, jid)"
    "CREATE INDEX IF NOT EXISTS idx_omemo_devices_jid
  ON omemo_devices (account, jid)"
    "CREATE INDEX IF NOT EXISTS idx_omemo_sessions_jid
  ON omemo_sessions (account, jid)"
    "CREATE TABLE IF NOT EXISTS omemo_device_id (
  account TEXT PRIMARY KEY,
  device_id INTEGER NOT NULL)"
    "CREATE TABLE IF NOT EXISTS chat_settings (
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  encryption TEXT DEFAULT 'default',
  thread_id TEXT,
  PRIMARY KEY (account, peer))"
    "CREATE TABLE IF NOT EXISTS message_oob (
  id         INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  url        TEXT NOT NULL,
  desc       TEXT)"
    "CREATE INDEX IF NOT EXISTS idx_oob_message_id
  ON message_oob(message_id)"
    "CREATE TABLE IF NOT EXISTS message_reaction (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  reaction   TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender, reaction))"
    "CREATE INDEX IF NOT EXISTS idx_reaction_message_id
  ON message_reaction(message_id)"
    "CREATE TABLE IF NOT EXISTS message_reaction_actor (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender))"
    "CREATE TABLE IF NOT EXISTS caps_cache (
  hash       TEXT NOT NULL,
  ver        TEXT NOT NULL,
  identities TEXT NOT NULL,
  features   TEXT NOT NULL,
  PRIMARY KEY (hash, ver))")
  "DDL statements for the latest database schema.")

(defconst jabber-db--archive-ddl
  '("CREATE TABLE IF NOT EXISTS mam_progress (
  account TEXT NOT NULL,
  archive TEXT NOT NULL,
  scope TEXT NOT NULL,
  uid TEXT,
  start TEXT,
  PRIMARY KEY (account, archive, scope))"
    "CREATE TABLE IF NOT EXISTS message_archive (
  account TEXT NOT NULL,
  archive TEXT NOT NULL,
  uid TEXT NOT NULL,
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  PRIMARY KEY (account, archive, uid))")
  "Archive coverage and occurrence schema; empty scope means all peers.
An absent start records an unbounded scan, not an inferred history date.")

(defconst jabber-db--identity-index-ddl
  '("CREATE INDEX IF NOT EXISTS idx_msg_origin_id
  ON message(account, origin_id) WHERE origin_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_room_id
  ON message(account, room_id) WHERE room_id IS NOT NULL")
  "Nonunique identity indexes; collisions must remain visible to admission.")

(defun jabber-db--init-schema (db)
  "Initialize the database schema in DB."
  (dolist (ddl (append jabber-db--schema-ddl jabber-db--archive-ddl
                      jabber-db--identity-index-ddl))
    (sqlite-execute db ddl)))

(defun jabber-db--table-exists-p (db table)
  "Return non-nil when TABLE exists in DB."
  (not (null (sqlite-select db "\
SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?"
                           (list table)))))

(defun jabber-db--ensure-reaction-actor-table (db)
  "Create the reaction actor metadata table in DB when missing."
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS message_reaction_actor (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender))"))

(defun jabber-db--reaction-actors-current-p (db)
  "Return non-nil when reaction actor metadata in DB is current."
  (and (jabber-db--table-exists-p db "message_reaction_actor")
       (zerop (caar (sqlite-select db "\
SELECT count(*)
FROM (
  SELECT message_id, sender, MAX(updated_at) AS updated_at
  FROM message_reaction
  GROUP BY message_id, sender) AS reaction_actor
LEFT JOIN message_reaction_actor
  ON message_reaction_actor.message_id = reaction_actor.message_id
 AND message_reaction_actor.sender = reaction_actor.sender
WHERE message_reaction_actor.message_id IS NULL
   OR message_reaction_actor.updated_at < reaction_actor.updated_at")))))

(defun jabber-db--backfill-reaction-actors (db)
  "Backfill reaction actor metadata in DB from reaction rows."
  (sqlite-execute db "\
INSERT OR IGNORE INTO message_reaction_actor (message_id, sender, updated_at)
  SELECT message_id, sender, MAX(updated_at)
  FROM message_reaction
  GROUP BY message_id, sender")
  (sqlite-execute db "\
UPDATE message_reaction_actor
SET updated_at = (
  SELECT MAX(updated_at)
  FROM message_reaction
  WHERE message_reaction.message_id = message_reaction_actor.message_id
    AND message_reaction.sender = message_reaction_actor.sender)
WHERE updated_at < (
  SELECT MAX(updated_at)
  FROM message_reaction
  WHERE message_reaction.message_id = message_reaction_actor.message_id
    AND message_reaction.sender = message_reaction_actor.sender)"))

(defun jabber-db--repair-reaction-actors (db)
  "Repair reaction actor metadata in DB when missing or stale."
  (unless (jabber-db--reaction-actors-current-p db)
    (jabber-db--ensure-reaction-actor-table db)
    (jabber-db--backfill-reaction-actors db)))

(defconst jabber-db--schema-version 13
  "Current schema version.
Bump this when adding migrations.  A database whose version
exceeds this value is from a newer (or development) build and
cannot be used; the user is prompted to delete it.")

(defun jabber-db--handle-unknown-schema (db)
  "Detect a schema newer than `jabber-db--schema-version' in DB and offer reset.
Return non-nil if the database was deleted and the caller should
re-open it."
  (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
    (when (> version jabber-db--schema-version)
      (sqlite-close db)
      (if (y-or-n-p
           (format "Database schema v%d is newer than supported v%d at %s.\n\
Delete it and start fresh? "
                   version jabber-db--schema-version jabber-db-path))
          (progn
            (delete-file jabber-db-path)
            (message "Deleted incompatible database %s" jabber-db-path)
            t)
        (user-error "Cannot open database (v%d > supported v%d); \
delete %s manually to continue"
                    version jabber-db--schema-version jabber-db-path)))))

(defmacro jabber-db--with-savepoint (db &rest body)
  "Execute BODY atomically on DB, including on nonlocal exits.
Savepoints permit migration inside a caller-owned transaction."
  (declare (indent 1) (debug t))
  (let ((connection (make-symbol "connection"))
        (complete (make-symbol "complete")))
    `(let ((,connection ,db)
           (,complete nil))
       (sqlite-execute ,connection "SAVEPOINT jabber_schema")
       (unwind-protect
           (prog1 (progn ,@body)
             (sqlite-execute ,connection "RELEASE jabber_schema")
             (setq ,complete t))
         (unless ,complete
           (sqlite-execute ,connection "ROLLBACK TO jabber_schema")
           (sqlite-execute ,connection "RELEASE jabber_schema"))))))

(defun jabber-db--migrate-v1-to-v2 (db)
  "Migrate DB from schema version 1 to version 2."
  (jabber-db--with-savepoint db
    (sqlite-execute db "ALTER TABLE message ADD COLUMN occupant_id TEXT")
    (sqlite-execute db "ALTER TABLE message DROP COLUMN raw_xml")
    (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_occupant_id
  ON message(account, peer, occupant_id) WHERE occupant_id IS NOT NULL")
    (sqlite-execute db "PRAGMA user_version=2")))

(defun jabber-db--migrate-v2-to-v3 (db)
  "Migrate DB from schema version 2 to version 3."
  (jabber-db--with-savepoint db
    (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS message_oob (
  id         INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  url        TEXT NOT NULL,
  desc       TEXT)")
    (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_oob_message_id
  ON message_oob(message_id)")
    (sqlite-execute db "\
INSERT INTO message_oob (message_id, url, desc)
  SELECT id, oob_url, oob_desc FROM message WHERE oob_url IS NOT NULL")
    (sqlite-execute db "ALTER TABLE message DROP COLUMN oob_url")
    (sqlite-execute db "ALTER TABLE message DROP COLUMN oob_desc")
    (sqlite-execute db "PRAGMA user_version=3")))

(defun jabber-db--migrate-v3-to-v4 (db)
  "Migrate DB from schema version 3 to version 4."
  (jabber-db--with-savepoint db
    (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS caps_cache (
  hash       TEXT NOT NULL,
  ver        TEXT NOT NULL,
  identities TEXT NOT NULL,
  features   TEXT NOT NULL,
  PRIMARY KEY (hash, ver))")
    (sqlite-execute db "PRAGMA user_version=4")))

(defun jabber-db--migrate-v4-to-v5 (db)
  "Migrate DB from schema version 4 to version 5."
  (jabber-db--with-savepoint db
    (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS message_reaction (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  reaction   TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender, reaction))")
    (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_reaction_message_id
  ON message_reaction(message_id)")
    (jabber-db--ensure-reaction-actor-table db)
    (jabber-db--backfill-reaction-actors db)
    (sqlite-execute db "PRAGMA user_version=5")))

(defun jabber-db--migrate-v5-to-v6 (db)
  "Migrate DB from schema version 5 to version 6."
  (jabber-db--with-savepoint db
    (sqlite-execute db
                    "ALTER TABLE omemo_store ADD COLUMN spk_rotated_at INTEGER")
    (sqlite-execute db "PRAGMA user_version=6")))

(defun jabber-db--migrate-v6-to-v7 (db)
  "Migrate DB from schema version 6 to version 7."
  (jabber-db--with-savepoint db
    (dolist (column '("reply_to_id TEXT" "reply_to_jid TEXT"
                      "fallback_start INTEGER" "fallback_end INTEGER"))
      (sqlite-execute db (concat "ALTER TABLE message ADD COLUMN " column)))
    (sqlite-execute db "PRAGMA user_version=7")))

(defun jabber-db--migrate-v7-to-v8-steps (db)
  "Apply the version 7 to version 8 schema migration to DB."
  (dolist (column '("thread_id TEXT" "thread_parent_id TEXT"))
    (sqlite-execute db (concat "ALTER TABLE message ADD COLUMN " column)))
  (when (cl-every
         (lambda (column)
           (member column
                   (mapcar #'car
                           (sqlite-select
                            db "SELECT name FROM pragma_table_info('message')"))))
         '("account" "peer" "type" "timestamp"))
    (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_thread
  ON message(account, peer, type, thread_id, timestamp)
  WHERE thread_id IS NOT NULL"))
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS message_thread (
  account TEXT NOT NULL, peer TEXT NOT NULL, type TEXT NOT NULL,
  thread_id TEXT NOT NULL, parent_thread_id TEXT,
  root_message_id INTEGER, root_stanza_id TEXT, root_server_id TEXT,
  created_at INTEGER NOT NULL, read_message_id INTEGER,
  PRIMARY KEY (account, peer, type, thread_id))")
  (sqlite-execute db "PRAGMA user_version=8"))

(defun jabber-db--migrate-v7-to-v8 (db)
  "Migrate DB atomically from schema version 7 to version 8."
  (sqlite-execute db "SAVEPOINT jabber_schema_v8")
  (condition-case err
      (prog1
          (jabber-db--migrate-v7-to-v8-steps db)
        (sqlite-execute db "RELEASE jabber_schema_v8"))
    (error
     (ignore-errors
       (sqlite-execute db "ROLLBACK TO jabber_schema_v8"))
     (ignore-errors
       (sqlite-execute db "RELEASE jabber_schema_v8"))
     (signal (car err) (cdr err)))))

(defun jabber-db--migrate-v8-to-v9-steps (db)
  "Apply the version 8 to version 9 schema migration to DB."
  (sqlite-execute db "\
ALTER TABLE message_thread
ADD COLUMN dedicated INTEGER NOT NULL DEFAULT 0")
  (sqlite-execute db "\
UPDATE message_thread AS mt SET dedicated = 1
WHERE mt.type = 'groupchat'
OR mt.parent_thread_id IS NOT NULL
OR mt.read_message_id IS NOT NULL
OR EXISTS (
  SELECT 1 FROM message AS root
  WHERE root.id = mt.root_message_id
  AND (root.thread_id IS NULL OR root.thread_id != mt.thread_id))")
  (when (cl-every
         (lambda (column)
           (member column
                   (mapcar #'car
                           (sqlite-select
                            db "SELECT name FROM pragma_table_info('message')"))))
         '("account" "peer" "type" "thread_id" "direction" "reply_to_id"))
    (sqlite-execute db "\
UPDATE message_thread AS mt SET dedicated = 1
WHERE EXISTS (
  SELECT 1 FROM message AS m
  WHERE m.account = mt.account AND m.peer = mt.peer AND m.type = mt.type
  AND m.thread_id = mt.thread_id
  AND m.direction = 'out' AND m.reply_to_id IS NULL)"))
  (sqlite-execute db "PRAGMA user_version=9"))

(defun jabber-db--migrate-v8-to-v9 (db)
  "Migrate DB atomically from schema version 8 to version 9."
  (sqlite-execute db "SAVEPOINT jabber_schema_v9")
  (condition-case err
      (prog1
          (jabber-db--migrate-v8-to-v9-steps db)
        (sqlite-execute db "RELEASE jabber_schema_v9"))
    (error
     (ignore-errors
       (sqlite-execute db "ROLLBACK TO jabber_schema_v9"))
     (ignore-errors
       (sqlite-execute db "RELEASE jabber_schema_v9"))
     (signal (car err) (cdr err)))))

(defun jabber-db--migrate-v9-to-v10 (db)
  "Add local thread state atomically to schema version 9 in DB."
  (sqlite-execute db "SAVEPOINT jabber_schema_v10")
  (condition-case err
      (progn
        (sqlite-execute db
                        "ALTER TABLE message_thread ADD COLUMN title TEXT")
        (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS chat_settings (
  account TEXT NOT NULL, peer TEXT NOT NULL,
  encryption TEXT DEFAULT 'default', thread_id TEXT,
  PRIMARY KEY (account, peer))")
        (unless
            (member "thread_id"
                    (mapcar #'car
                            (sqlite-select
                             db
                             "SELECT name FROM pragma_table_info('chat_settings')")))
          (sqlite-execute
           db "ALTER TABLE chat_settings ADD COLUMN thread_id TEXT"))
        (sqlite-execute db "PRAGMA user_version=10")
        (sqlite-execute db "RELEASE jabber_schema_v10"))
    (error
     (ignore-errors
       (sqlite-execute db "ROLLBACK TO jabber_schema_v10"))
     (ignore-errors
       (sqlite-execute db "RELEASE jabber_schema_v10"))
     (signal (car err) (cdr err)))))

(defun jabber-db--migrate-v10-to-v11 (db)
  "Separate retraction state from optional attribution in DB."
  (jabber-db--with-savepoint db
    (sqlite-execute db "ALTER TABLE message
ADD COLUMN retracted INTEGER NOT NULL DEFAULT 0")
    (sqlite-execute db "UPDATE message SET retracted = 1
WHERE retracted_by IS NOT NULL")
    (sqlite-execute db "PRAGMA user_version=11")))

(defun jabber-db--migrate-v11-to-v12 (db)
  "Preserve identity and archive coverage independently in DB.
Never reinterpret historical server IDs or invent historical scan coverage."
  (jabber-db--with-savepoint db
    (sqlite-execute db "ALTER TABLE message ADD COLUMN origin_id TEXT")
    (sqlite-execute db "ALTER TABLE message ADD COLUMN room_id TEXT")
    (dolist (ddl jabber-db--archive-ddl)
      (sqlite-execute db ddl))
    (sqlite-execute db "PRAGMA user_version=12")))

(defun jabber-db--migrate-v12-to-v13 (db)
  "Index origin and room duplicate candidates atomically in DB."
  (jabber-db--with-savepoint db
    (dolist (ddl jabber-db--identity-index-ddl)
      (sqlite-execute db ddl))
    (sqlite-execute db "PRAGMA user_version=13")))

(defun jabber-db--migrate (db)
  "Check user_version and apply migrations to DB."
  (jabber-db--with-savepoint db
    (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
      (when (zerop version)
        (jabber-db--init-schema db)
        (sqlite-execute db
                        (format "PRAGMA user_version=%d"
                                jabber-db--schema-version))
        (setq version jabber-db--schema-version))
      (when (= version 1)
        (jabber-db--migrate-v1-to-v2 db)
        (setq version 2))
      (when (= version 2)
        (jabber-db--migrate-v2-to-v3 db)
        (setq version 3))
      (when (= version 3)
        (jabber-db--migrate-v3-to-v4 db)
        (setq version 4))
      (when (= version 4)
        (jabber-db--migrate-v4-to-v5 db)
        (setq version 5))
      (when (= version 5)
        (jabber-db--migrate-v5-to-v6 db)
        (setq version 6))
      (when (= version 6)
        (jabber-db--migrate-v6-to-v7 db)
        (setq version 7))
      (when (= version 7)
        (jabber-db--migrate-v7-to-v8 db)
        (setq version 8))
      (when (= version 8)
        (jabber-db--migrate-v8-to-v9 db)
        (setq version 9))
      (when (= version 9)
        (jabber-db--migrate-v9-to-v10 db)
        (setq version 10))
      (when (= version 10)
        (jabber-db--migrate-v10-to-v11 db)
        (setq version 11))
      (when (= version 11)
        (jabber-db--migrate-v11-to-v12 db)
        (setq version 12))
      (when (= version 12)
        (jabber-db--migrate-v12-to-v13 db)
        (setq version 13))
      (when (= version 13)
        (jabber-db--repair-reaction-actors db)))))

(defun jabber-db-ensure-open ()
  "Open the SQLite database, creating it if needed.  Idempotent.
Migrate an existing connection when the package schema has advanced.
Return the database connection, or nil if storage is disabled."
  (when jabber-db-path
    (let* ((live (and jabber-db--connection (sqlitep jabber-db--connection)))
           (db (and live jabber-db--connection))
           (complete nil))
      (unwind-protect
          (progn
            (unless live
              (when-let* ((dir (file-name-directory jabber-db-path)))
                (unless (file-directory-p dir)
                  (make-directory dir t)))
              (setq db (sqlite-open jabber-db-path))
              (when (jabber-db--handle-unknown-schema db)
                (setq db (sqlite-open jabber-db-path)))
              (sqlite-execute db "PRAGMA journal_mode=WAL")
              (sqlite-execute db "PRAGMA synchronous=NORMAL")
              (sqlite-execute db "PRAGMA foreign_keys=ON"))
            (when (or (not live)
                      (< (caar (sqlite-select db "PRAGMA user_version"))
                         jabber-db--schema-version))
              (jabber-db--migrate db))
            (setq jabber-db--connection db
                  complete t)
            db)
        (unless complete
          (when (and db (sqlitep db))
            (sqlite-close db))
          (setq jabber-db--connection nil))))))

(defun jabber-db-close ()
  "Close the database connection."
  (when (and jabber-db--connection
             (sqlitep jabber-db--connection))
    (sqlite-close jabber-db--connection)
    (setq jabber-db--connection nil)))

;;; Transactions

(defmacro jabber-db-with-transaction (&rest body)
  "Execute BODY inside a SQLite transaction.
Opens a BEGIN/COMMIT pair around BODY.  If BODY signals an error,
the transaction is still committed (partial data is better than
a stuck open transaction in single-threaded Emacs)."
  (declare (indent 0) (debug t))
  `(when-let* ((db (jabber-db-ensure-open)))
     (sqlite-execute db "BEGIN")
     (unwind-protect
         (progn ,@body)
       (sqlite-execute db "COMMIT"))))

;;; Chat settings

(defun jabber-db-set-chat-encryption (account peer encryption)
  "Store ENCRYPTION mode for ACCOUNT + PEER.
ENCRYPTION is a symbol: `omemo', `plaintext', or `default'."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
INSERT INTO chat_settings (account, peer, encryption) VALUES (?, ?, ?)
ON CONFLICT(account, peer) DO UPDATE SET encryption = excluded.encryption"
		    (list account peer (symbol-name encryption)))))

(defun jabber-db-get-chat-encryption (account peer)
  "Load encryption mode for ACCOUNT + PEER.
Returns a symbol (`omemo', `plaintext'), or nil if not set or `default'."
  (when-let* ((db (jabber-db-ensure-open)))
    (when-let* ((val (caar (sqlite-select db "\
SELECT encryption FROM chat_settings
  WHERE account = ? AND peer = ?"
					  (list account peer)))))
      (unless (string= val "default")
        (intern val)))))

(defun jabber-db-set-chat-thread (account peer thread-id)
  "Store the current parent chat THREAD-ID for ACCOUNT and PEER."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
INSERT INTO chat_settings (account, peer, thread_id) VALUES (?, ?, ?)
ON CONFLICT(account, peer) DO UPDATE SET thread_id = excluded.thread_id"
                    (list account peer thread-id))))

(defun jabber-db-get-chat-thread (account peer)
  "Return the current parent chat thread ID for ACCOUNT and PEER."
  (when-let* ((db (jabber-db-ensure-open)))
    (caar
     (sqlite-select db "\
SELECT thread_id FROM chat_settings WHERE account = ? AND peer = ?"
                    (list account peer)))))

;;; Caps cache

(defun jabber-db-caps-store (hash ver identities features)
  "Persist a caps cache entry for HASH and VER.
IDENTITIES is a list of vectors [name category type].
FEATURES is a list of feature strings."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db "\
INSERT OR REPLACE INTO caps_cache (hash, ver, identities, features)
  VALUES (?, ?, ?, ?)"
                    (list hash ver
                          (prin1-to-string identities)
                          (prin1-to-string features)))))

(defun jabber-db-caps-lookup (hash ver)
  "Look up a caps cache entry for HASH and VER.
Return (IDENTITIES FEATURES) or nil if not found."
  (when-let* ((db (jabber-db-ensure-open)))
    (when-let* ((row (car (sqlite-select db "\
SELECT identities, features FROM caps_cache
  WHERE hash = ? AND ver = ?"
                                         (list hash ver)))))
      (list (car (read-from-string (car row)))
            (car (read-from-string (cadr row)))))))

;;; Storage

(defun jabber-db--extract-reply-fields (xml-data)
  "Return shared reply metadata from XML-DATA."
  (jabber-xml-reply-fields xml-data))

(defun jabber-db--extract-thread-fields (xml-data)
  "Return valid XEP-0201 thread metadata from XML-DATA, or nil."
  (jabber-message-thread-protocol-fields xml-data))

(defun jabber-db--reply-fallback-range (xml-data)
  "Return shared reply metadata from XML-DATA."
  (jabber-xml-reply-fallback-range xml-data))

(defun jabber-db--fallback-range-cols (range)
  "Encode RANGE for storage as a (START . END) cons of column values.
RANGE is nil, `all', or a (START END) list; `all' is stored
as -1/-1, nil as NULL/NULL."
  (pcase range
    ('all '(-1 . -1))
    (`(,start ,end) (cons start end))
    (_ '(nil . nil))))

(defun jabber-db--decode-fallback-range (start end)
  "Decode fallback columns START and END back into a range value.
Inverse of `jabber-db--fallback-range-cols'."
  (cond ((and (eql start -1) (eql end -1)) 'all)
        ((and start end) (list start end))))

(defun jabber-db--detect-duplicate
    (db account peer timestamp body stanza-id server-id
        type direction resource occupant-id)
  "Return the unambiguous duplicate row id in DB, or nil.
ACCOUNT, PEER, TYPE and DIRECTION scope the message.  Populated STANZA-ID,
SERVER-ID and OCCUPANT-ID must not conflict.  A MUC row with all three
IDs equal and the same RESOURCE may retain its original direction when
nickname-based archive classification drifts.  MUC client IDs alone are
not unique.  Content matching requires one row with equal TIMESTAMP,
BODY, RESOURCE and OCCUPANT-ID, and missing IDs.  An ID-less outgoing
non-MUC row may lack the archive sender's resource.  A matching MUC client
ID may be upgraded only when the replay supplies a SERVER-ID."
  (let* ((scope "SELECT id FROM message
WHERE account = ? AND peer = ? AND type IS ?
AND (direction = ? OR (type = 'groupchat' AND server_id = ?
     AND stanza_id = ? AND occupant_id = ? AND resource IS ?))
AND (? IS NULL OR stanza_id IS NULL OR stanza_id = ?)
AND (? IS NULL OR server_id IS NULL OR server_id = ?)
AND (? IS NULL OR occupant_id IS NULL OR occupant_id = ?)
AND ")
         (parameters
          (list account peer type direction server-id stanza-id occupant-id resource
                stanza-id stanza-id server-id server-id occupant-id occupant-id))
         ;; Separate indexed branches avoid a peer-wide scan for the ID OR.
         ;; UNION counts one row matching both IDs only once; LIMIT is global.
         (matches
          (sqlite-select
           db (concat scope "server_id = ? UNION "
                      scope "? != 'groupchat' AND stanza_id = ? LIMIT 2")
           (append parameters (list server-id) parameters (list type stanza-id)))))
    (if matches
        (and (null (cdr matches)) (caar matches))
      ;; Without an ID, identical messages may be independent sends.
      ;; Never upgrade every equal-content row or pick an arbitrary one.
      (when (or stanza-id server-id)
        (let ((legacy
               (sqlite-select
                db "SELECT id FROM message
WHERE account = ? AND peer = ? AND type IS ? AND direction = ?
AND timestamp = ? AND body = ? AND occupant_id IS ?
AND (resource IS ? OR (direction = 'out' AND type != 'groupchat'
     AND resource IS NULL AND stanza_id IS NULL AND ? IS NOT NULL))
AND server_id IS NULL
AND (stanza_id IS NULL OR (? IS NOT NULL AND stanza_id = ?)) LIMIT 2"
                (list account peer type direction timestamp body
                      occupant-id resource server-id server-id stanza-id))))
          (and (null (cdr legacy)) (caar legacy)))))))

(defun jabber-db--insert-message (db account peer resource occupant-id
                                     direction type body timestamp
                                     stanza-id server-id encrypted
                                     oob-entries reply thread)
  "Insert a new message row into DB for ACCOUNT and attach OOB-ENTRIES.
PEER, RESOURCE, OCCUPANT-ID, DIRECTION, TYPE, BODY, TIMESTAMP,
STANZA-ID, SERVER-ID and ENCRYPTED fill the corresponding columns.
REPLY and THREAD contain parsed reply and thread metadata."
  (pcase-let ((`(,fb-start . ,fb-end)
               (jabber-db--fallback-range-cols
                (plist-get reply :fallback-range))))
    (sqlite-execute
     db
     "INSERT INTO message \
(account, peer, resource, occupant_id, direction, type, body, timestamp, \
stanza_id, server_id, encrypted, reply_to_id, reply_to_jid, \
fallback_start, fallback_end, thread_id, thread_parent_id) \
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list account peer resource occupant-id direction type body timestamp
           stanza-id server-id (if encrypted 1 0)
           (plist-get reply :reply-to-id)
           (plist-get reply :reply-to-jid)
           fb-start fb-end
           (plist-get thread :thread-id)
           (plist-get thread :thread-parent-id))))
  (let ((msg-id (caar (sqlite-select db "SELECT last_insert_rowid()"))))
    (when oob-entries
      (dolist (entry oob-entries)
        (sqlite-execute
         db
         "INSERT INTO message_oob (message_id, url, desc) VALUES (?, ?, ?)"
         (list msg-id (car entry) (cdr entry)))))
    msg-id))

(defun jabber-db--update-duplicate-message (db message-id timestamp body oob-entries)
  "Update the exact duplicate MESSAGE-ID in DB.
Normalize TIMESTAMP when non-nil and replace failed placeholders with BODY.
Skip retracted messages to prevent MAM replays from undoing retractions.
OOB-ENTRIES replaces the row's OOB metadata when BODY is upgraded."
  (unless (eql 1 (caar (sqlite-select
                       db "SELECT retracted FROM message WHERE id = ?"
                       (list message-id))))
    (when timestamp
      (sqlite-execute
       db "UPDATE message SET timestamp = ? WHERE id = ? AND timestamp != ?"
       (list timestamp message-id timestamp)))
    (when (and body
               (not (jabber--decrypt-failure-body-p body))
               (jabber--decrypt-failure-body-p
                (caar (sqlite-select db "SELECT body FROM message WHERE id = ?"
                                     (list message-id)))))
      (sqlite-execute db "UPDATE message SET body = ? WHERE id = ?"
                      (list body message-id))
      (sqlite-execute db "DELETE FROM message_oob WHERE message_id = ?"
                      (list message-id))
      (dolist (entry oob-entries)
        (sqlite-execute
         db "INSERT INTO message_oob (message_id, url, desc) VALUES (?, ?, ?)"
         (list message-id (car entry) (cdr entry)))))))

(defun jabber-db--backfill-message-ids (db message-id stanza-id server-id)
  "Fill missing STANZA-ID and SERVER-ID on exact MESSAGE-ID in DB."
  (sqlite-execute
   db "UPDATE message SET stanza_id = COALESCE(stanza_id, ?),
server_id = COALESCE(server_id, ?) WHERE id = ?"
   (list stanza-id server-id message-id)))

(defun jabber-db--backfill-reply-fields (db message-id reply)
  "Fill NULL reply columns in DB for exact MESSAGE-ID using REPLY.
Complete rows stored before the outgoing reply elements were attached."
  (pcase-let ((`(,fb-start . ,fb-end)
               (jabber-db--fallback-range-cols
                (plist-get reply :fallback-range))))
    (sqlite-execute
     db "UPDATE message SET reply_to_id = ?, reply_to_jid = ?,
fallback_start = ?, fallback_end = ? WHERE id = ? AND reply_to_id IS NULL"
     (list (plist-get reply :reply-to-id)
           (plist-get reply :reply-to-jid)
           fb-start fb-end message-id))))

(defun jabber-db--backfill-thread-fields (db message-id thread)
  "Fill missing THREAD columns for MESSAGE-ID in DB."
  (when (and thread message-id)
    (sqlite-execute
     db
     "UPDATE message SET thread_id = ?, thread_parent_id = ? \
WHERE id = ? AND thread_id IS NULL"
     (list (plist-get thread :thread-id)
           (plist-get thread :thread-parent-id)
           message-id))))

(defun jabber-db--register-message-thread
    (account peer type thread-id parent-thread-id root-stanza-id
             root-server-id created-at root-message-id dedicated-p)
  "Store THREAD-ID metadata and DEDICATED-P for ACCOUNT, PEER, and TYPE."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute
     db
     "INSERT INTO message_thread \
(account, peer, type, thread_id, parent_thread_id, root_message_id, \
root_stanza_id, root_server_id, created_at, dedicated) \
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
ON CONFLICT(account, peer, type, thread_id) DO UPDATE SET \
parent_thread_id = COALESCE(message_thread.parent_thread_id, excluded.parent_thread_id), \
root_message_id = COALESCE(message_thread.root_message_id, excluded.root_message_id), \
root_stanza_id = COALESCE(message_thread.root_stanza_id, excluded.root_stanza_id), \
root_server_id = COALESCE(message_thread.root_server_id, excluded.root_server_id), \
dedicated = MAX(message_thread.dedicated, excluded.dedicated)"
     (list account peer type thread-id parent-thread-id root-message-id
           root-stanza-id root-server-id created-at
           (if dedicated-p 1 0)))))

(defun jabber-db-register-message-thread
    (account peer type thread-id parent-thread-id root-stanza-id
             root-server-id created-at &optional root-message-id)
  "Register THREAD-ID as a dedicated thread for ACCOUNT and PEER.
TYPE, PARENT-THREAD-ID, ROOT-STANZA-ID, ROOT-SERVER-ID, CREATED-AT,
and ROOT-MESSAGE-ID describe its identity and root message."
  (jabber-db--register-message-thread
   account peer type thread-id parent-thread-id root-stanza-id
   root-server-id created-at root-message-id t))

(defun jabber-db--normalize-message-thread-title (title)
  "Return TITLE trimmed onto one line, or nil when it is empty."
  (when title
    (let ((trimmed
           (string-trim
            (replace-regexp-in-string
             "[[:space:][:cntrl:]]+" " " title))))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun jabber-db-set-message-thread-title
    (account peer type thread-id title)
  "Set dedicated THREAD-ID's local TITLE for ACCOUNT, PEER, and TYPE.
Whitespace around TITLE is removed; an empty title clears it."
  (let ((title (jabber-db--normalize-message-thread-title title)))
    (when-let* ((db (jabber-db-ensure-open)))
      (unless
          (equal
           1
           (caar
            (sqlite-select
             db
             "SELECT dedicated FROM message_thread
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ?"
             (list account peer type thread-id))))
        (user-error "Unknown dedicated message thread"))
      (sqlite-execute
       db
       "UPDATE message_thread SET title = ?
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ?
AND dedicated = 1"
       (list title account peer type thread-id))
      title)))

(defun jabber-db--message-thread-stored-p
    (db account peer type thread-id)
  "Return non-nil for THREAD-ID stored in DB for ACCOUNT, PEER, and TYPE."
  (caar
   (sqlite-select
    db
    "SELECT 1 FROM message_thread
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ?"
    (list account peer type thread-id))))

(defun jabber-db--ensure-message-thread
    (account peer type timestamp stanza-id server-id thread)
  "Register THREAD after storing a message for ACCOUNT, PEER, and TYPE.
TIMESTAMP, STANZA-ID, and SERVER-ID identify its first observed root."
  (when-let* ((thread-id (plist-get thread :thread-id))
              (db (jabber-db-ensure-open)))
    (let* ((new-p (not (jabber-db--message-thread-stored-p
                        db account peer type thread-id)))
           (parent-id (plist-get thread :thread-parent-id))
           (root-message-id
            (and
             new-p
             (caar
              (sqlite-select
               db
               "SELECT id FROM message \
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ? \
ORDER BY id DESC LIMIT 1"
               (list account peer type thread-id))))))
      (jabber-db--register-message-thread
       account peer type thread-id parent-id stanza-id server-id timestamp
       root-message-id
       (or (equal type "groupchat") parent-id)))))

(defun jabber-db-mam-progress (account archive with)
  "Return durable (UID START) for ACCOUNT, exact ARCHIVE and WITH filter.
Nil means unknown coverage.  A nil START in a recorded row means full scan."
  (when-let* ((db (jabber-db-ensure-open)))
    (car (sqlite-select db
          "SELECT uid, start FROM mam_progress
WHERE account = ? AND archive = ? AND scope = ?"
          (list account archive (or with ""))))))

(defun jabber-db--advance-mam-progress (db account archive with expected uid start)
  "Advance DB's ACCOUNT/ARCHIVE/WITH cursor from EXPECTED to UID.
START retains the safe scan lower bound.  The caller owns the transaction.
Return nil on a stale predecessor; opaque UIDs are never ordered."
  (if expected
      (sqlite-execute db
       "UPDATE mam_progress SET uid = ?
WHERE account = ? AND archive = ? AND scope = ? AND uid IS ?"
       (list uid account archive (or with "") expected))
    (sqlite-execute db
     "INSERT INTO mam_progress (account, archive, scope, uid, start)
VALUES (?, ?, ?, ?, ?)
ON CONFLICT(account, archive, scope) DO UPDATE SET uid = excluded.uid
WHERE mam_progress.uid IS NULL"
     (list account archive (or with "") uid start)))
  (= 1 (caar (sqlite-select db "SELECT changes()"))))

(defun jabber-db--message-identity (xml &optional archive uid)
  "Return operation-specific evidence in XML, plus ARCHIVE and UID.
Only a stanza ID assigned by the message's room supplies room identity."
  (let* ((origin (seq-find
                  (lambda (child)
                    (and (eq (jabber-xml-node-name child) 'origin-id)
                         (equal (jabber-xml-get-xmlns child) "urn:xmpp:sid:0")))
                  (jabber-xml-node-children xml)))
         (room (and (equal (jabber-xml-get-attribute xml 'type) "groupchat")
                    (jabber-xml-get-attribute xml 'from)
                    (jabber-jid-user (jabber-xml-get-attribute xml 'from))))
         (sid (and room (jabber-db--stanza-id-element xml room))))
    (list :origin-id (jabber-xml-get-attribute origin 'id)
          :room-id (jabber-xml-get-attribute sid 'id)
          :archive archive :archive-id uid)))

(defun jabber-db--identity-compatible-p
    (db id stanza-id server-id origin-id room-id resource occupant-id)
  "Check ID in DB against supplied operation-specific evidence.
STANZA-ID, SERVER-ID, ORIGIN-ID, ROOM-ID and OCCUPANT-ID may fill
unknown fields, but never replace conflicts.  RESOURCE identifies the sender."
  (pcase-let ((`(,stanza ,server ,origin ,room ,sender ,occupant ,direction ,type)
               (car (sqlite-select db
                     "SELECT stanza_id, server_id, origin_id, room_id, resource,
occupant_id, direction, type FROM message WHERE id = ?" (list id)))))
    (and (cl-every (lambda (pair)
                     (or (null (car pair)) (null (cdr pair))
                         (equal (car pair) (cdr pair))))
                   (list (cons stanza stanza-id) (cons server server-id)
                         (cons origin origin-id)
                         (cons room room-id) (cons occupant occupant-id)))
         (or (equal sender resource)
             (and (equal direction "out") (not (equal type "groupchat"))
                  (null sender))))))

(defun jabber-db--identity-duplicate
    (db account peer direction type stanza-id server-id resource occupant-id identity legacy)
  "Resolve IDENTITY to one compatible row in DB, or nil.
ACCOUNT, PEER, DIRECTION, TYPE, STANZA-ID, SERVER-ID, RESOURCE and
OCCUPANT-ID scope admission.  LEGACY is the former detector's candidate,
not authority.  An archive occurrence conflict signals an error instead
of rebinding its UID."
  (let* ((origin (plist-get identity :origin-id))
         (room (plist-get identity :room-id))
         (archive (plist-get identity :archive))
         (uid (plist-get identity :archive-id))
         (occurrence (and archive uid
                          (caar (sqlite-select db
                           "SELECT message_id FROM message_archive
WHERE account = ? AND archive = ? AND uid = ?" (list account archive uid)))))
         (scope "SELECT id FROM message WHERE account = ? AND peer = ?
AND type IS ? AND (direction = ? OR (type = 'groupchat' AND room_id = ?))
AND ")
         (parameters (list account peer type direction room))
         ;; Retain incompatible strong candidates for ambiguity/conflict checks.
         ;; Keep SQL's NULL type semantics in the non-MUC origin branch.
         (strong (and (or origin room occurrence)
                      (mapcar #'car
                              (sqlite-select
                               db (concat scope "type != 'groupchat' AND origin_id = ? UNION "
                                          scope "room_id = ? UNION " scope "id = ?")
                               (append parameters (list origin) parameters (list room)
                                       parameters (list occurrence))))))
         ;; A populated unknown legacy server_id is not room evidence.
         ;; A partial live row, however, can acquire its first room ID when
         ;; the scoped detector also proved an equal transport ID.  Body
         ;; and timestamp alone never authorize this identity upgrade.
         (candidates (delete-dups
                      (append strong
                              (when (and legacy
                                         (or (not (equal type "groupchat"))
                                             (caar (sqlite-select db
                                              "SELECT id FROM message WHERE id = ?
AND (room_id IS NOT NULL OR (server_id IS NULL AND stanza_id = ?))"
                                              (list legacy stanza-id)))))
                                (list legacy)))))
         (matches (seq-filter
                   (lambda (id)
                     (jabber-db--identity-compatible-p
                      db id stanza-id server-id origin room resource occupant-id))
                   candidates)))
    (when (and occurrence
               (not (and (equal matches (list occurrence))
                         (equal candidates (list occurrence)))))
      (error "Conflicting archive occurrence"))
    (and (= (length candidates) 1) (= (length matches) 1) (car matches))))

(defun jabber-db--enrich-message-identity (db id account identity)
  "Append compatible IDENTITY to exact ID in DB for ACCOUNT."
  (sqlite-execute db "UPDATE message SET origin_id = COALESCE(origin_id, ?),
room_id = COALESCE(room_id, ?) WHERE id = ?"
                  (list (plist-get identity :origin-id)
                        (plist-get identity :room-id) id))
  (when-let* ((archive (plist-get identity :archive))
              (uid (plist-get identity :archive-id)))
    (sqlite-execute db
     "INSERT INTO message_archive (account, archive, uid, message_id)
VALUES (?, ?, ?, ?) ON CONFLICT(account, archive, uid) DO NOTHING"
     (list account archive uid id))))

(defun jabber-db-store-message (account peer direction type body timestamp
                                        &optional resource stanza-id
                                        server-id occupant-id oob-entries
                                        encrypted reply thread identity)
  "Store a message in the database.
ACCOUNT is the bare JID of the local account.
PEER is the bare JID of the contact or room.
DIRECTION is \"in\" or \"out\".
TYPE is the message type (\"chat\", \"groupchat\", \"headline\").
BODY is the message text.  TIMESTAMP is a unix epoch integer, or
nil when the source has no authoritative timestamp.
Optional RESOURCE is the sender resource.
Optional STANZA-ID is the transport message/@id, not the origin ID.
Optional SERVER-ID is the XEP-0359 server-assigned id.
Optional OCCUPANT-ID is the XEP-0421 occupant id.
Optional OOB-ENTRIES is a list of (URL . DESC) cons cells for
jabber:x:oob elements.
Optional ENCRYPTED is non-nil if the message was OMEMO-encrypted.
Optional REPLY is a reply metadata plist from
`jabber-db--extract-reply-fields'.
Optional THREAD is a thread metadata plist from
`jabber-db--extract-thread-fields'.
Optional IDENTITY carries distinct origin, room and scoped archive evidence.
Return the canonical message row ID."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-db--with-savepoint db
      (let* ((stored-timestamp (or timestamp (floor (float-time))))
           (legacy-id (jabber-db--detect-duplicate
                       db account peer stored-timestamp body stanza-id
                       server-id type direction resource occupant-id))
           (duplicate-id
            (if identity
                (jabber-db--identity-duplicate
                 db account peer direction type stanza-id server-id resource occupant-id
                 identity legacy-id)
              legacy-id))
           (message-id
            (or duplicate-id
                (jabber-db--insert-message
                 db account peer resource occupant-id direction type body
                 stored-timestamp stanza-id server-id encrypted oob-entries
                 reply thread))))
      (when duplicate-id
        (jabber-db--update-duplicate-message db duplicate-id timestamp body oob-entries)
        (jabber-db--backfill-message-ids db duplicate-id stanza-id server-id)
        (when reply
          (jabber-db--backfill-reply-fields db duplicate-id reply)))
      (when identity
        (jabber-db--enrich-message-identity db message-id account identity))
      (jabber-db--backfill-thread-fields db message-id thread)
      (jabber-db--ensure-message-thread
       account peer type stored-timestamp stanza-id server-id thread)
      (when-let* ((thread-id (plist-get thread :thread-id)))
        (run-hook-with-args
         'jabber-db-message-thread-stored-functions
         account peer type thread-id stored-timestamp))
      message-id))))

(defun jabber-db-prune-empty-message-threads (account peer)
  "Delete thread metadata without a surviving message for ACCOUNT and PEER."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute
     db
     "DELETE FROM message_thread AS mt \
WHERE mt.account = ? AND mt.peer = ? AND NOT EXISTS ( \
SELECT 1 FROM message AS m WHERE m.account = mt.account AND m.peer = mt.peer \
AND m.type = mt.type AND (m.thread_id = mt.thread_id \
OR (mt.root_message_id IS NOT NULL AND m.id = mt.root_message_id) \
OR (mt.type = 'groupchat' AND mt.root_server_id IS NOT NULL \
AND m.server_id = mt.root_server_id) \
OR (mt.type != 'groupchat' AND mt.root_stanza_id IS NOT NULL \
AND m.stanza_id = mt.root_stanza_id)))"
     (list account peer))))

;;; Receipt updates

(defun jabber-db-update-receipt (account peer stanza-id column timestamp)
  "Set COLUMN to TIMESTAMP for outgoing message with STANZA-ID.
ACCOUNT and PEER scope the update to prevent cross-conversation
collision.  Only updates outgoing messages (direction=out).
COLUMN is \"delivered_at\" or \"displayed_at\".
The IS NULL guard prevents overwriting an earlier timestamp."
  (when (and jabber-db--connection stanza-id)
    (sqlite-execute jabber-db--connection
                    (format "UPDATE message SET %s = ? \
WHERE account = ? AND peer = ? AND stanza_id = ? \
AND direction = 'out' AND %s IS NULL"
                            column column)
                    (list timestamp account peer stanza-id))))

(defun jabber-db-cascade-displayed
    (account peer timestamp ref-timestamp &optional ref-id)
  "Mark delivered outgoing messages through the referenced row as displayed.
ACCOUNT and PEER identify the conversation.  TIMESTAMP is the time
stored in displayed_at.  REF-TIMESTAMP is the referenced message's
epoch; optional REF-ID is its stanza ID and takes precedence.
Resolve exactly one outgoing row, then order by its stored timestamp
and primary row ID to break equal-second ties within its message type.
Without REF-ID, accept only an unambiguous stored REF-TIMESTAMP.
Missing, ambiguous or older boundaries leave every row unchanged.
Preserve existing displayed_at values and skip undelivered messages."
  (when jabber-db--connection
    (sqlite-execute
     jabber-db--connection
     "WITH reference AS (\
SELECT id, timestamp, type FROM message \
WHERE account = ? AND peer = ? AND direction = 'out' \
AND CASE WHEN ? IS NOT NULL THEN stanza_id = ? ELSE timestamp = ? END) \
UPDATE message SET displayed_at = ? \
WHERE account = ? AND peer = ? AND direction = 'out' \
AND (SELECT count(*) FROM reference) = 1 \
AND type = (SELECT type FROM reference) \
AND (timestamp, id) <= (SELECT timestamp, id FROM reference) \
AND delivered_at IS NOT NULL AND displayed_at IS NULL \
AND NOT EXISTS (SELECT 1 FROM message AS newer \
WHERE newer.account = ? AND newer.peer = ? AND newer.direction = 'out' \
AND newer.type = (SELECT type FROM reference) \
AND newer.displayed_at IS NOT NULL \
AND (newer.timestamp, newer.id) > (SELECT timestamp, id FROM reference))"
     (list account peer ref-id ref-id ref-timestamp timestamp
           account peer account peer))))

(defun jabber-db-message-retraction-candidates (account peer server-id)
  "Return stored MUC retraction candidates for SERVER-ID.
ACCOUNT and PEER scope the lookup.  Each result contains identity and
existing retraction state for one exact row."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar
     (lambda (row)
       (seq-let (row-id resource occupant-id retracted-by
                        retraction-reason retracted)
           row
         (list :row-id row-id
               :retracted (eql retracted 1)
               :from (if resource (concat peer "/" resource) peer)
               :occupant-id occupant-id
               :retracted-by retracted-by
               :retraction-reason retraction-reason)))
     (sqlite-select
      db
      "SELECT id, resource, occupant_id, retracted_by, retraction_reason, retracted \
FROM message \
WHERE account = ? AND peer = ? AND type = 'groupchat' AND server_id = ?"
      (list account peer server-id)))))

(defun jabber-db-retract-message-row (row-id retracted-by &optional reason)
  "Mark primary message ROW-ID as retracted by RETRACTED-BY.
RETRACTED-BY may be nil when attribution is unavailable.
Optional REASON is the human-readable retraction reason string.
Return non-nil only when this call performed the first retraction."
  (when-let* ((db (and row-id (jabber-db-ensure-open))))
    (sqlite-execute db
                    "UPDATE message SET retracted = 1, retracted_by = ?, retraction_reason = ? \
WHERE id = ? AND retracted = 0"
                    (list retracted-by reason row-id))
    (= 1 (caar (sqlite-select db "SELECT changes()")))))

(defun jabber-db-retract-message (server-id retracted-by &optional reason)
  "Retract globally unambiguous SERVER-ID by RETRACTED-BY.
Optional REASON is the human-readable retraction reason string.
Conversation-aware callers should use
`jabber-db-retract-message-in-peer'."
  (when (and jabber-db--connection server-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET retracted = 1, retracted_by = ?, retraction_reason = ? \
WHERE server_id = ? AND \
(SELECT COUNT(*) FROM message WHERE server_id = ?) = 1"
                    (list retracted-by reason server-id server-id))))

(defun jabber-db-retract-message-in-peer (account peer server-id retracted-by
                                                  &optional reason)
  "Mark SERVER-ID as retracted in PEER on ACCOUNT.
RETRACTED-BY is the moderator or sender JID, or nil if anonymous.
Optional REASON is the human-readable retraction reason string."
  (when (and jabber-db--connection account peer server-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET retracted = 1, retracted_by = ?, retraction_reason = ? \
WHERE account = ? AND peer = ? AND server_id = ?"
                    (list retracted-by reason account peer server-id))))

(defun jabber-db-occupant-id-by-server-id (server-id)
  "Return occupant-id for globally unambiguous SERVER-ID, or nil."
  (when (and jabber-db--connection server-id)
    (caar (sqlite-select jabber-db--connection
                         "SELECT occupant_id FROM message \
WHERE server_id = ? GROUP BY server_id HAVING COUNT(*) = 1"
                         (list server-id)))))

(defun jabber-db-occupant-id-by-server-id-in-peer (account peer server-id)
  "Return occupant-id for SERVER-ID in PEER on ACCOUNT, or nil."
  (when (and jabber-db--connection account peer server-id)
    (caar (sqlite-select jabber-db--connection
                         "SELECT occupant_id FROM message \
WHERE account = ? AND peer = ? AND server_id = ? LIMIT 1"
                         (list account peer server-id)))))

(defun jabber-db-occupant-id-by-stanza-id (stanza-id)
  "Return the occupant ID for globally unique STANZA-ID, or nil."
  (when (and jabber-db--connection stanza-id)
    (caar (sqlite-select jabber-db--connection
                         "SELECT occupant_id FROM message \
WHERE stanza_id = ? GROUP BY stanza_id HAVING COUNT(*) = 1"
                         (list stanza-id)))))

(defun jabber-db-server-ids-by-occupant-id (account peer occupant-id)
  "Return server-ids for messages with OCCUPANT-ID in PEER on ACCOUNT.
Only returns non-retracted messages that have a server-id."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar #'car
            (sqlite-select db
                           "SELECT server_id FROM message \
WHERE account = ? AND peer = ? AND occupant_id = ? \
AND server_id IS NOT NULL AND retracted = 0"
                           (list account peer occupant-id)))))

(defun jabber-db-correct-message (stanza-id new-body)
  "Correct globally unique STANZA-ID with NEW-BODY.
Protocol handlers should use `jabber-db-correct-message-row' after a
conversation-scoped lookup."
  (when (and jabber-db--connection stanza-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET body = ?, edited = 1 WHERE id = (\
SELECT MIN(id) FROM message WHERE stanza_id = ? \
GROUP BY stanza_id HAVING COUNT(*) = 1)"
                    (list new-body stanza-id))))

(defun jabber-db-message-correction-candidates (account peer stanza-id)
  "Return correction candidates for STANZA-ID in ACCOUNT's PEER chat."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar
     (lambda (row)
       (seq-let (row-id direction row-peer resource row-account occupant-id
                        timestamp type)
           row
         (list :row-id row-id
               :from (if (or (string= direction "in")
                             (string= type "groupchat"))
                         (if resource
                             (concat row-peer "/" resource)
                           row-peer)
                       row-account)
               :occupant-id occupant-id
               :timestamp timestamp)))
     (sqlite-select
      db
      "SELECT id, direction, peer, resource, account, occupant_id, timestamp, type \
FROM message WHERE account = ? AND peer = ? AND stanza_id = ?"
      (list account peer stanza-id)))))

(defun jabber-db-correct-message-row (row-id new-body)
  "Replace the body of primary message ROW-ID with NEW-BODY."
  (when (and jabber-db--connection row-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET body = ?, edited = 1 WHERE id = ?"
                    (list new-body row-id))))

(defun jabber-db-delete-peer-messages (account peer)
  "Delete all messages for PEER on ACCOUNT."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db
		    "DELETE FROM message WHERE account = ? AND peer = ?"
		    (list account peer))
    (sqlite-execute db
                    "DELETE FROM message_thread WHERE account = ? AND peer = ?"
                    (list account peer))))

(defun jabber-db-message-sender-by-stanza-id (stanza-id)
  "Return the sender of globally unique STANZA-ID, or nil.
For incoming messages returns the full sender JID (peer/resource or peer).
For outgoing messages returns the account bare JID, enabling validation
of carbon copies of corrections sent from another device."
  (when (and jabber-db--connection stanza-id)
    (when-let* ((row (car (sqlite-select
                           jabber-db--connection
                           "SELECT direction, peer, resource, account \
FROM message WHERE stanza_id = ? \
GROUP BY stanza_id HAVING COUNT(*) = 1"
                           (list stanza-id)))))
      (seq-let (direction peer resource account) row
        (if (string= direction "in")
            (if resource (concat peer "/" resource) peer)
          account)))))

(defun jabber-db--message-reference-candidates (db account peer type target-id)
  "Return reply/reaction candidates for TARGET-ID in DB.
ACCOUNT, PEER and TYPE scope both aliases.  Direct messages retain their
transport alias after origin enrichment; room references use only room
identity, falling back to the historical server field.  Archive UIDs
never identify action targets.  Return all candidates to detect collisions."
  (when (and account peer type target-id)
    (sqlite-select
     db
     (format "SELECT id, body, retracted, direction, resource FROM message
WHERE account = ? AND peer = ? AND type = ? AND %s"
             (if (equal type "groupchat")
                 "COALESCE(room_id, server_id) = ?"
               "(origin_id = ? OR stanza_id = ?)"))
     (append (list account peer type target-id)
             (unless (equal type "groupchat") (list target-id))))))

(defun jabber-db-reply-target-body (account peer reply-id muc-p &optional sender)
  "Return the body of the unique message REPLY-ID references, or nil.
ACCOUNT and PEER scope the lookup.  MUC-P selects room identity; direct
chat accepts origin and historical transport aliases.  Optional SENDER
is the reply's author JID: a full JID limits known resources, a bare JID
limits the account or peer.  Historical outgoing direct messages may lack
resource evidence.  Ambiguous and retracted targets return nil."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((rows
           (seq-filter
            (lambda (row)
              (let* ((author (if (or muc-p (equal (nth 3 row) "in"))
                                 peer account))
                     (full (if (nth 4 row)
                               (concat author "/" (nth 4 row)) author)))
                (or (null sender)
                    (equal sender (if (jabber-jid-resource sender) full author))
                    (and (not muc-p) (equal (nth 3 row) "out")
                         (null (nth 4 row))
                         (equal (jabber-jid-user sender) author)))))
            (jabber-db--message-reference-candidates
             db account peer (if muc-p "groupchat" "chat") reply-id))))
      (when (and (= (length rows) 1) (eql (nth 2 (car rows)) 0))
        (nth 1 (car rows))))))

;;; Reactions

(defun jabber-db--message-id-for-reaction-target (db account peer type target-id)
  "Return the unique DB message id for reaction TARGET-ID, or nil.
ACCOUNT, PEER and TYPE scope the lookup.  Count all aliases together,
including retracted rows, so a collision cannot choose another message."
  (let ((rows (jabber-db--message-reference-candidates
               db account peer type target-id)))
    (when (= (length rows) 1) (caar rows))))

(defun jabber-db-reaction-target-row (account peer type target-id)
  "Resolve reaction TARGET-ID for ACCOUNT, PEER and TYPE.
Return its unique row ID, nil if absent, or `ambiguous' on collision."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((rows (jabber-db--message-reference-candidates
                 db account peer type target-id)))
      (if (cdr rows) 'ambiguous (caar rows)))))

(defun jabber-db--reaction-current-updated-at (db message-id sender)
  "Return actor reaction timestamp in DB for MESSAGE-ID and SENDER."
  (caar (sqlite-select db "SELECT updated_at FROM message_reaction_actor \
WHERE message_id = ? AND sender = ?"
                       (list message-id sender))))

(defun jabber-db--source-reaction-stale-p (db message-id sender updated-at)
  "Return non-nil when UPDATED-AT is stale for MESSAGE-ID and SENDER in DB."
  (when-let* ((current-updated-at (jabber-db--reaction-current-updated-at
                                   db message-id sender)))
    (<= updated-at current-updated-at)))

(defun jabber-db-reaction-stale-p (account peer type target-id sender updated-at)
  "Return non-nil when UPDATED-AT is stale for SENDER's target reactions.
ACCOUNT, PEER, TYPE and TARGET-ID identify the target message.  Return
nil when storage is disabled or the target is not stored."
  (when-let* ((db (jabber-db-ensure-open))
              (updated-at)
              (message-id (jabber-db--message-id-for-reaction-target
                           db account peer type target-id)))
    (jabber-db--source-reaction-stale-p db message-id sender updated-at)))

(defun jabber-db-replace-reactions (account peer type target-id sender reactions
                                    &optional updated-at)
  "Replace SENDER's REACTIONS for TARGET-ID in ACCOUNT/PEER conversation.
TYPE is the target message type.  Return non-nil when the target message
exists and the replacement was applied.  Empty REACTIONS deletes SENDER's
stored reactions for the target.  Non-nil UPDATED-AT is source ordered and
older or equal values are ignored.  Nil UPDATED-AT is a local replacement
and is always accepted with the current timestamp."
  (when-let* ((db (jabber-db-ensure-open))
              (message-id (jabber-db--message-id-for-reaction-target
                           db account peer type target-id)))
    (unless (and updated-at
                 (jabber-db--source-reaction-stale-p
                  db message-id sender updated-at))
      (let ((deduplicated (delete-dups (cl-remove-if-not #'stringp reactions)))
            (replacement-updated-at (or updated-at (floor (float-time)))))
        (sqlite-execute db "INSERT INTO message_reaction_actor \
(message_id, sender, updated_at) VALUES (?, ?, ?) \
ON CONFLICT(message_id, sender) DO UPDATE SET updated_at = excluded.updated_at"
                        (list message-id sender replacement-updated-at))
        (sqlite-execute db "DELETE FROM message_reaction \
WHERE message_id = ? AND sender = ?"
                        (list message-id sender))
        (dolist (reaction deduplicated)
          (unless (string-empty-p reaction)
            (sqlite-execute db "INSERT INTO message_reaction \
(message_id, sender, reaction, updated_at) VALUES (?, ?, ?, ?)"
                            (list message-id sender reaction replacement-updated-at))))
        t))))

(defun jabber-db-reactions-for-message-ids (message-ids)
  "Return reaction state for MESSAGE-IDS keyed by message DB id.
The returned hash table maps message ids to alists of (SENDER . REACTIONS)."
  (let ((grouped (make-hash-table :test #'eql)))
    (when-let* ((db (jabber-db-ensure-open))
                ((cl-some #'identity message-ids)))
      (dolist (row (sqlite-select
                    db
                    (format "SELECT message_id, sender, reaction \
FROM message_reaction WHERE message_id IN (%s) \
ORDER BY message_id, updated_at, rowid"
                            (mapconcat #'number-to-string message-ids ","))))
        (seq-let (message-id sender reaction) row
          (push reaction (alist-get sender (gethash message-id grouped)
                                    nil nil #'equal))))
      (maphash (lambda (message-id sender-state)
                 (puthash message-id
                          (mapcar (lambda (entry)
                                    (cons (car entry) (nreverse (cdr entry))))
                                  (nreverse sender-state))
                          grouped))
               grouped))
    grouped))

(defun jabber-db--attach-reactions (plists)
  "Batch-query reactions and attach them to PLISTS by :db-id."
  (let* ((ids (cl-loop for p in plists
                       for id = (plist-get p :db-id)
                       when id collect id))
         (reactions (jabber-db-reactions-for-message-ids ids)))
    (dolist (p plists)
      (when-let* ((db-id (plist-get p :db-id)))
        (plist-put p :reactions (gethash db-id reactions))))
    plists))

;;; Retrieval

(defconst jabber-db--backlog-columns
  "SELECT id, account, peer, direction, body, timestamp, \
resource, type, encrypted, stanza_id, delivered_at, displayed_at, \
server_id, retracted_by, retraction_reason, edited, occupant_id, \
reply_to_id, reply_to_jid, fallback_start, fallback_end, \
thread_id, thread_parent_id, retracted, origin_id, room_id FROM message"
  "Columns shared by parent and thread backlog queries.")

(defun jabber-db--row-to-plist (row)
  "Convert a backlog ROW to a message plist.
ROW columns match the SELECT in `jabber-db-backlog'.
The :oob-entries key is populated later by `jabber-db--attach-oob-entries'."
  (seq-let (id account peer direction body timestamp resource type
               encrypted stanza-id delivered-at
               displayed-at server-id retracted-by retraction-reason edited
               occupant-id reply-to-id reply-to-jid fallback-start fallback-end
               thread-id thread-parent-id retracted origin-id room-id)
      row
    (let ((from (cond
                 ;; Incoming: peer/resource (or just peer if no resource).
                 ((string= direction "in")
                  (if resource (concat peer "/" resource) peer))
                 ;; Outgoing groupchat: peer/resource so the nick renders.
                 ((and (equal type "groupchat") resource)
                  (concat peer "/" resource))
                 ;; Outgoing 1:1: account bare JID.
                 (t account))))
      (list :db-id id
            :id stanza-id
            :server-id (or room-id server-id)
            :origin-id origin-id
            :occupant-id occupant-id
            :from from
            :body (or body "")
            :subject nil
            :timestamp (seconds-to-time timestamp)
            :delayed t
            :encrypted (and encrypted (not (zerop encrypted)))
            :retracted (eql retracted 1)
            :retracted-by retracted-by
            :retraction-reason retraction-reason
            :edited (and edited (not (zerop edited)))
            :reply-to-id reply-to-id
            :reply-to-jid reply-to-jid
            :fallback-range (jabber-db--decode-fallback-range
                             fallback-start fallback-end)
            :thread-id thread-id
            :thread-parent-id thread-parent-id
            :direction direction
            :msg-type type
            :oob-entries nil
            :oob-url nil
            :oob-desc nil
            :error-text nil
            :status (cond
                     (displayed-at :displayed)
                     (delivered-at :delivered))))))

(defun jabber-db--attach-oob-entries (db plists)
  "Batch-query OOB entries and attach to PLISTS.
DB is the SQLite connection.  Each plist must have a :db-id key.
Sets :oob-entries, :oob-url, and :oob-desc on each plist."
  (when plists
    (let* ((ids (cl-loop for p in plists
                         for id = (plist-get p :db-id)
                         when id collect id))
           (oob-rows
            (when ids
              (sqlite-select
               db
               (format "SELECT message_id, url, desc FROM message_oob \
WHERE message_id IN (%s) ORDER BY message_id, id"
                       (mapconcat (lambda (id) (number-to-string id))
                                  ids ",")))))
           (grouped (make-hash-table :test #'eql)))
      (dolist (row oob-rows)
        (let ((msg-id (nth 0 row))
              (url (nth 1 row))
              (desc (nth 2 row)))
          (push (cons url desc) (gethash msg-id grouped))))
      (dolist (p plists)
        (when-let* ((db-id (plist-get p :db-id)))
          (let ((entries (nreverse (gethash db-id grouped))))
            (plist-put p :oob-entries entries)
            (plist-put p :oob-url (caar entries))
            (plist-put p :oob-desc (cdar entries)))))))
  plists)

(defun jabber-db--thread-summary (db account peer type thread-row)
  "Return THREAD-ROW's summary from DB for ACCOUNT, PEER, and TYPE."
  (seq-let (thread-id parent-id root-message-id root-stanza-id root-server-id
                      read-message-id title)
      thread-row
    (seq-let (reply-count latest-in-id local-reply-count)
        (car
         (sqlite-select
          db
          "SELECT count(*), MAX(CASE WHEN direction = 'in' THEN id END), \
COALESCE(SUM(CASE WHEN direction = 'out' THEN 1 ELSE 0 END), 0) \
FROM message WHERE account = ? AND peer = ? AND type = ? AND thread_id = ? \
AND NOT (CASE WHEN ? IS NOT NULL THEN id = ? \
WHEN ? = 'groupchat' AND ? IS NOT NULL THEN server_id = ? \
WHEN ? IS NOT NULL THEN stanza_id = ? ELSE 0 END)"
          (list account peer type thread-id
                root-message-id root-message-id
                type root-server-id root-server-id
                root-stanza-id root-stanza-id)))
      (list :thread-id thread-id
            :thread-type type
            :thread-parent-id parent-id
            :root-message-id root-message-id
            :root-stanza-id root-stanza-id
            :root-server-id root-server-id
            :title title
            :reply-count reply-count
            :local-reply-count local-reply-count
            :unread (and latest-in-id
                         (or (null read-message-id)
                             (> latest-in-id read-message-id)))))))

(defun jabber-db-message-thread-summary (account peer type thread-id)
  "Return THREAD-ID's summary for ACCOUNT, PEER, and TYPE, or nil."
  (when-let* ((db (jabber-db-ensure-open))
              (row
               (car (sqlite-select
                     db
                     "SELECT thread_id, parent_thread_id, root_message_id, \
root_stanza_id, root_server_id, read_message_id, title, type \
FROM message_thread \
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ? \
AND dedicated = 1"
                     (list account peer type thread-id)))))
    (jabber-db--thread-summary db account peer type row)))

(defconst jabber-db--message-threads-sql
  "SELECT mt.thread_id, mt.parent_thread_id, mt.created_at,
COALESCE(MAX(m.timestamp), mt.created_at) AS latest_at,
root.id, root.stanza_id, root.server_id, root.resource,
root.direction, root.body, root.timestamp,
root.retracted_by, root.retraction_reason, root.retracted,
COALESCE(SUM(CASE WHEN m.id IS NULL THEN 0
WHEN (CASE WHEN mt.root_message_id IS NOT NULL
THEN m.id = mt.root_message_id
WHEN mt.type = 'groupchat' AND mt.root_server_id IS NOT NULL
THEN m.server_id = mt.root_server_id
WHEN mt.root_stanza_id IS NOT NULL
THEN m.stanza_id = mt.root_stanza_id ELSE 0 END) THEN 0 ELSE 1 END), 0),
COALESCE(SUM(CASE WHEN m.id IS NULL OR m.direction != 'out' THEN 0
WHEN (CASE WHEN mt.root_message_id IS NOT NULL
THEN m.id = mt.root_message_id
WHEN mt.type = 'groupchat' AND mt.root_server_id IS NOT NULL
THEN m.server_id = mt.root_server_id
WHEN mt.root_stanza_id IS NOT NULL
THEN m.stanza_id = mt.root_stanza_id ELSE 0 END) THEN 0 ELSE 1 END), 0),
MAX(CASE WHEN m.direction = 'in' AND NOT (CASE
WHEN mt.root_message_id IS NOT NULL THEN m.id = mt.root_message_id
WHEN mt.type = 'groupchat' AND mt.root_server_id IS NOT NULL
THEN m.server_id = mt.root_server_id
WHEN mt.root_stanza_id IS NOT NULL
THEN m.stanza_id = mt.root_stanza_id ELSE 0 END) THEN m.id END),
mt.read_message_id, mt.title
FROM message_thread mt
LEFT JOIN message root ON
(mt.root_message_id IS NOT NULL AND root.id = mt.root_message_id
AND root.account = mt.account AND root.peer = mt.peer AND root.type = mt.type)
OR (mt.root_message_id IS NULL AND mt.type = 'groupchat'
AND mt.root_server_id IS NOT NULL AND root.server_id = mt.root_server_id
AND root.account = mt.account AND root.peer = mt.peer AND root.type = mt.type)
OR (mt.root_message_id IS NULL
AND NOT (mt.type = 'groupchat' AND mt.root_server_id IS NOT NULL)
AND mt.root_stanza_id IS NOT NULL AND root.stanza_id = mt.root_stanza_id
AND root.account = mt.account AND root.peer = mt.peer AND root.type = mt.type)
LEFT JOIN message m ON m.account = mt.account AND m.peer = mt.peer
AND m.type = mt.type AND m.thread_id = mt.thread_id
WHERE mt.account = ? AND mt.peer = ? AND mt.type = ? AND mt.dedicated = 1
GROUP BY mt.account, mt.peer, mt.type, mt.thread_id
ORDER BY latest_at DESC, mt.created_at DESC, mt.thread_id
LIMIT 50"
  "Query dedicated threads in one conversation by latest activity.")

(defun jabber-db--thread-root-from
    (account peer type resource direction)
  "Return a root sender from ACCOUNT, PEER, TYPE, RESOURCE, and DIRECTION."
  (cond
   ((equal direction "in")
    (if resource (concat peer "/" resource) peer))
   ((and (equal type "groupchat") resource)
    (concat peer "/" resource))
   (t account)))

(defun jabber-db--thread-list-row-to-plist (account peer type row)
  "Convert a thread listing ROW for ACCOUNT, PEER, and TYPE to a plist."
  (seq-let (thread-id parent-id created-at latest-at root-id root-stanza-id
                      root-server-id root-resource root-direction root-body
                      root-timestamp root-retracted-by root-retraction-reason
                      root-retracted
                      reply-count local-reply-count latest-in-id read-message-id
                      title)
      row
    (list :thread-id thread-id
          :thread-type type
          :thread-parent-id parent-id
          :title title
          :created-at (seconds-to-time created-at)
          :latest-at (seconds-to-time latest-at)
          :reply-count reply-count
          :local-reply-count local-reply-count
          :unread (and latest-in-id
                       (or (null read-message-id)
                           (> latest-in-id read-message-id)))
          :root-message
          (and root-id
               (list :db-id root-id :id root-stanza-id
                     :server-id root-server-id
                     :from (jabber-db--thread-root-from
                            account peer type root-resource root-direction)
                     :resource root-resource :body (or root-body "")
                     :timestamp (seconds-to-time root-timestamp)
                     :retracted (eql root-retracted 1)
                     :retracted-by root-retracted-by
                     :retraction-reason root-retraction-reason
                     :thread-id thread-id :thread-parent-id parent-id
                     :direction root-direction :msg-type type)))))

(defun jabber-db-message-threads (account peer type)
  "Return ACCOUNT and PEER's TYPE threads by latest activity."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar
     (lambda (row)
       (jabber-db--thread-list-row-to-plist account peer type row))
     (sqlite-select db jabber-db--message-threads-sql
                    (list account peer type)))))

(defun jabber-db-message-thread-known-p (account peer type thread-id)
  "Return non-nil when THREAD-ID is dedicated for ACCOUNT, PEER, and TYPE."
  (and thread-id
       (jabber-db-message-thread-summary account peer type thread-id)
       t))

(defun jabber-db-message-thread-root-p
    (account peer type thread-id stanza-id server-id &optional message-id)
  "Return non-nil when MESSAGE-ID identifies THREAD-ID's root.
ACCOUNT, PEER, and TYPE scope the lookup.  SERVER-ID and STANZA-ID
are fallback wire identifiers when the thread has no database row ID."
  (when-let* ((summary
               (jabber-db-message-thread-summary
                account peer type thread-id)))
    (cond
     ((plist-get summary :root-message-id)
      (and message-id
           (equal message-id (plist-get summary :root-message-id))))
     ((and (equal type "groupchat")
           (plist-get summary :root-server-id))
      (and server-id
           (equal server-id (plist-get summary :root-server-id))))
     ((plist-get summary :root-stanza-id)
      (and stanza-id
           (equal stanza-id (plist-get summary :root-stanza-id)))))))

(defun jabber-db--message-thread-location
    (db account peer type column value)
  "Return VALUE's thread location from DB.
ACCOUNT, PEER, and TYPE scope the trusted internal COLUMN."
  (when-let* ((row
               (car (sqlite-select
                     db
                     (format
                      "SELECT mt.thread_id, \
CASE WHEN mt.root_message_id IS NOT NULL THEN mt.root_message_id = m.id \
WHEN m.type = 'groupchat' AND mt.root_server_id IS NOT NULL \
THEN mt.root_server_id = m.server_id \
WHEN mt.root_stanza_id IS NOT NULL THEN mt.root_stanza_id = m.stanza_id \
ELSE 0 END \
FROM message m LEFT JOIN message_thread mt \
ON mt.account = m.account AND mt.peer = m.peer AND mt.type = m.type \
AND mt.dedicated = 1 \
AND (mt.thread_id = m.thread_id OR (m.thread_id IS NULL AND \
CASE WHEN mt.root_message_id IS NOT NULL THEN mt.root_message_id = m.id \
WHEN m.type = 'groupchat' AND mt.root_server_id IS NOT NULL \
THEN mt.root_server_id = m.server_id \
WHEN mt.root_stanza_id IS NOT NULL THEN mt.root_stanza_id = m.stanza_id \
ELSE 0 END)) \
WHERE m.account = ? AND m.peer = ? AND m.type = ? AND %s = ? \
AND mt.thread_id IS NOT NULL LIMIT 1"
                      column)
                     (list account peer type value)))))
    (list :thread-id (car row) :root (= (cadr row) 1))))

(defun jabber-db-message-thread-location
    (account peer type message-id server-id-p)
  "Return MESSAGE-ID's thread location for ACCOUNT, PEER, and TYPE.
The result contains `:thread-id' and non-nil `:root' when the
message is the thread root.  SERVER-ID-P selects server IDs."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-db--message-thread-location
     db account peer type
     (if server-id-p "m.server_id" "m.stanza_id")
     message-id)))

(defun jabber-db-message-thread-location-by-row
    (account peer type row-id)
  "Return ROW-ID's thread location for ACCOUNT, PEER, and TYPE."
  (when-let* ((db (jabber-db-ensure-open)))
    (jabber-db--message-thread-location
     db account peer type "m.id" row-id)))

(defun jabber-db-message-thread-for-message
    (account peer type message-id server-id-p)
  "Return MESSAGE-ID's reply thread for ACCOUNT, PEER, and TYPE.
Thread roots return nil so their canonical owner remains the parent
buffer.  SERVER-ID-P selects server IDs."
  (when-let* ((location
               (jabber-db-message-thread-location
                account peer type message-id server-id-p))
              ((not (plist-get location :root))))
    (plist-get location :thread-id)))

(defun jabber-db--attach-thread-summaries (db account peer plists)
  "Attach DB thread summaries for ACCOUNT and PEER to root PLISTS."
  (dolist (row (sqlite-select
                db
                "SELECT thread_id, parent_thread_id, root_message_id, \
root_stanza_id, root_server_id, read_message_id, title, type \
FROM message_thread \
WHERE account = ? AND peer = ? AND dedicated = 1"
                (list account peer)))
    (let* ((type (nth 7 row))
           (root (seq-find
                  (lambda (msg)
                    (and (equal (plist-get msg :msg-type) type)
                     (jabber-db--message-thread-root-p
                      msg type (nth 2 row) (nth 3 row) (nth 4 row))))
                  plists)))
      (when root
        (plist-put
         root :thread-summary
         (jabber-db--thread-summary db account peer type row)))))
  plists)

(defun jabber-db-backlog (account peer &optional count start-time resource
                                  msg-type include-thread-replies)
  "Return the last COUNT messages for PEER on ACCOUNT.
Messages are returned as plists with keys :from, :body, :timestamp,
:delayed, :direction, :msg-type, etc.
COUNT defaults to `jabber-backlog-number'.
START-TIME is a `float-time'; only messages after this time are returned.
If nil, `jabber-backlog-days' is used to compute the cutoff.
RESOURCE, when non-nil, filters to messages from that resource only.
This is used for MUC private message buffers.
MSG-TYPE, when non-nil, filters to messages of that type only
\(e.g. \"groupchat\" for MUC buffers).
INCLUDE-THREAD-REPLIES non-nil keeps replies in the result."
  (when-let* ((db (jabber-db-ensure-open)))
    (let* ((n (or count jabber-backlog-number))
           (cutoff (cond
                    (start-time (floor start-time))
                    (jabber-backlog-days
                     (floor (- (float-time) (* jabber-backlog-days 86400.0))))
                    (t 0)))
           (parent-clause
            (if include-thread-replies
                ""
              " AND (thread_id IS NULL OR NOT EXISTS (\
SELECT 1 FROM message_thread mt WHERE mt.account = message.account \
AND mt.peer = message.peer AND mt.type = message.type \
AND mt.thread_id = message.thread_id AND mt.dedicated = 1) OR EXISTS (\
SELECT 1 FROM message_thread mt WHERE mt.account = message.account \
AND mt.peer = message.peer AND mt.type = message.type \
AND mt.thread_id = message.thread_id AND mt.dedicated = 1 \
AND (CASE WHEN mt.root_message_id IS NOT NULL \
THEN mt.root_message_id = message.id \
WHEN message.type = 'groupchat' AND mt.root_server_id IS NOT NULL \
THEN mt.root_server_id = message.server_id \
WHEN mt.root_stanza_id IS NOT NULL \
THEN mt.root_stanza_id = message.stanza_id ELSE 0 END)))"))
           (sql (cond
                 (resource
                  (concat jabber-db--backlog-columns
                          " WHERE account = ? AND peer = ? \
AND type = 'chat' AND (resource = ? OR direction = 'out') \
AND timestamp >= ?" parent-clause
" ORDER BY timestamp DESC LIMIT ?"))
                 (msg-type
                  (concat jabber-db--backlog-columns
                          " WHERE account = ? AND peer = ? \
AND type = ? AND timestamp >= ?" parent-clause " \
ORDER BY timestamp DESC LIMIT ?"))
                 (t
                  (concat jabber-db--backlog-columns
                          " WHERE account = ? AND peer = ? \
AND timestamp >= ?" parent-clause
" ORDER BY timestamp DESC LIMIT ?"))))
           (params (cond
                    (resource
                     (list account peer resource cutoff
                           (if (eq n t) -1 n)))
                    (msg-type
                     (list account peer msg-type cutoff
                           (if (eq n t) -1 n)))
                    (t
                     (list account peer cutoff
                           (if (eq n t) -1 n)))))
           (rows (sqlite-select db sql params))
           (plists (mapcar #'jabber-db--row-to-plist rows)))
      (jabber-db--attach-thread-summaries
       db account peer
       (jabber-db--attach-reactions
        (jabber-db--attach-oob-entries db plists))))))

(defun jabber-db--message-thread-root-p
    (msg type root-message-id root-stanza-id root-server-id)
  "Return non-nil when MSG matches a supplied thread root ID.
TYPE selects groupchat server IDs.  ROOT-MESSAGE-ID is the database
row ID.  ROOT-STANZA-ID and ROOT-SERVER-ID are wire identifiers."
  (cond
   (root-message-id
    (equal root-message-id (plist-get msg :db-id)))
   ((and (equal type "groupchat") root-server-id)
    (equal root-server-id (plist-get msg :server-id)))
   (root-stanza-id
    (equal root-stanza-id (plist-get msg :id)))))

(defun jabber-db-thread-backlog
    (account peer type thread-id &optional count start-time)
  "Return THREAD-ID's root and replies for ACCOUNT, PEER, and TYPE.
Results are reverse chronological, limited by COUNT after START-TIME."
  (when-let* ((db (jabber-db-ensure-open))
              (thread-row
               (car (sqlite-select
                     db
                     "SELECT root_message_id, root_stanza_id, root_server_id \
FROM message_thread WHERE account = ? AND peer = ? AND type = ? \
AND thread_id = ?"
                     (list account peer type thread-id)))))
    (let* ((root-message-id (nth 0 thread-row))
           (root-stanza-id (nth 1 thread-row))
           (root-server-id (nth 2 thread-row))
           (cutoff (floor (or start-time 0)))
           (rows (sqlite-select
                  db
                  (concat jabber-db--backlog-columns "\
 WHERE account = ? AND peer = ? AND type = ? AND timestamp >= ? \
AND (thread_id = ? OR CASE WHEN ? IS NOT NULL THEN id = ? \
WHEN ? = 'groupchat' AND ? IS NOT NULL THEN server_id = ? \
WHEN ? IS NOT NULL THEN stanza_id = ? ELSE 0 END) \
ORDER BY timestamp DESC")
                  (list account peer type cutoff thread-id
                        root-message-id root-message-id
                        type root-server-id root-server-id
                        root-stanza-id root-stanza-id)))
           (plists (mapcar #'jabber-db--row-to-plist rows))
           (root (seq-find
                  (lambda (msg)
                    (jabber-db--message-thread-root-p
                     msg type root-message-id root-stanza-id root-server-id))
                  plists))
           (limit (or count jabber-backlog-number))
           (replies (seq-remove (lambda (msg) (eq msg root)) plists))
           (selected (if (eq limit t) replies
                       (seq-take replies (max 0 (1- limit)))))
           (result (append selected (and root (list root)))))
      (jabber-db--attach-reactions
       (jabber-db--attach-oob-entries db result)))))

(defun jabber-db-mark-message-thread-read
    (account peer type thread-id)
  "Mark stored THREAD-ID replies read for ACCOUNT, PEER, and TYPE."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute
     db
     "UPDATE message_thread SET read_message_id = COALESCE((\
SELECT MAX(id) FROM message WHERE account = ? AND peer = ? AND type = ? \
AND thread_id = ?), read_message_id) \
WHERE account = ? AND peer = ? AND type = ? AND thread_id = ?"
     (list account peer type thread-id account peer type thread-id))))

(defun jabber-db--raw-row-to-plist (row)
  "Convert a raw query ROW to a plist.
ROW columns: id, stanza_id, server_id, account, peer, resource,
occupant_id, direction, type, body, timestamp, encrypted."
  (seq-let (id stanza-id server-id account peer resource
               occupant-id direction type body timestamp encrypted)
      row
    (list :id id
          :stanza-id stanza-id
          :server-id server-id
          :account account
          :peer peer
          :resource resource
          :occupant-id occupant-id
          :direction direction
          :type type
          :body body
          :timestamp timestamp
          :encrypted encrypted)))

(defun jabber-db-query (account peer &optional start-time end-time limit offset)
  "Query messages for PEER on ACCOUNT with pagination.
Returns a list of plists with keys :id, :stanza-id, :server-id,
:account, :peer, :resource, :occupant-id, :direction, :type, :body,
:timestamp, :encrypted.
START-TIME and END-TIME are unix epoch integers.
LIMIT defaults to 50, OFFSET defaults to 0."
  (when-let* ((db (jabber-db-ensure-open)))
    (let* ((lim (or limit 50))
           (off (or offset 0))
           (st (or start-time 0))
           (et (or end-time (floor (float-time))))
           (rows (sqlite-select
                  db
                  "SELECT id, stanza_id, server_id, account, peer, resource, \
occupant_id, direction, type, body, timestamp, encrypted \
FROM message \
WHERE account = ? AND peer = ? AND timestamp >= ? AND timestamp <= ? \
ORDER BY timestamp ASC LIMIT ? OFFSET ?"
                  (list account peer st et lim off))))
      (mapcar #'jabber-db--raw-row-to-plist rows))))

(defun jabber-db-search (account query &optional peer limit)
  "Full-text search for QUERY in messages on ACCOUNT.
Optional PEER restricts to a specific contact.
LIMIT defaults to 50.
Returns matching messages as plists."
  (when-let* ((db (jabber-db-ensure-open)))
    (let* ((lim (or limit 50))
           (rows (if peer
                     (sqlite-select
                      db
                      "SELECT m.id, m.stanza_id, m.server_id, m.account, \
m.peer, m.resource, m.occupant_id, m.direction, m.type, m.body, m.timestamp, \
m.encrypted \
FROM message m \
JOIN message_fts f ON f.rowid = m.id \
WHERE f.body MATCH ? AND m.account = ? AND m.peer = ? \
ORDER BY m.timestamp DESC LIMIT ?"
                      (list query account peer lim))
                   (sqlite-select
                    db
                    "SELECT m.id, m.stanza_id, m.server_id, m.account, \
m.peer, m.resource, m.occupant_id, m.direction, m.type, m.body, m.timestamp, \
m.encrypted \
FROM message m \
JOIN message_fts f ON f.rowid = m.id \
WHERE f.body MATCH ? AND m.account = ? \
ORDER BY m.timestamp DESC LIMIT ?"
                    (list query account lim)))))
      (mapcar #'jabber-db--raw-row-to-plist rows))))

(defun jabber-db-last-timestamp (account peer)
  "Return the latest stored timestamp for PEER on ACCOUNT.
Returns a unix epoch integer, or nil if no messages exist."
  (when-let* ((db (jabber-db-ensure-open)))
    (caar (sqlite-select
           db
           "SELECT MAX(timestamp) FROM message \
WHERE account = ? AND peer = ?"
           (list account peer)))))

(defun jabber-db-last-server-id (account &optional peer)
  "Return the most recent server_id for ACCOUNT, or nil.
This is the XEP-0359 stanza-id assigned by the server, used as
the sync point for MAM catch-up queries.
When PEER is non-nil, scope to messages with that peer (for MUC MAM)."
  (when-let* ((db (jabber-db-ensure-open)))
    (if peer
        (caar (sqlite-select
               db
               "SELECT server_id FROM message \
WHERE account = ? AND peer = ? AND server_id IS NOT NULL \
ORDER BY id DESC LIMIT 1"
               (list account peer)))
      (caar (sqlite-select
             db
             "SELECT server_id FROM message \
WHERE account = ? AND server_id IS NOT NULL \
ORDER BY id DESC LIMIT 1"
             (list account))))))

;;; Message chain handlers

(defun jabber-db--extract-occupant-id (xml-data)
  "Extract XEP-0421 occupant-id from XML-DATA, or nil."
  (jabber-xml-get-attribute
   (jabber-xml-child-with-xmlns xml-data "urn:xmpp:occupant-id:0") 'id))

(defun jabber-db--extract-oob-entries (xml-data)
  "Extract all jabber:x:oob entries from XML-DATA.
Returns a list of (URL . DESC) cons cells, or nil."
  (let (entries)
    (dolist (child (jabber-xml-node-children xml-data))
      (when (and (listp child)
                 (string= (jabber-xml-get-attribute child 'xmlns)
                          jabber-oob-xmlns))
        (let ((url (car (jabber-xml-node-children
                         (car (jabber-xml-get-children child 'url)))))
              (desc (car (jabber-xml-node-children
                          (car (jabber-xml-get-children child 'desc))))))
          (when url
            (push (cons url desc) entries)))))
    (nreverse entries)))

(defun jabber-db--stanza-id-element (xml-data expected-by)
  "Return the <stanza-id/> child of XML-DATA whose `by' is EXPECTED-BY.
Matching on the node name matters: <origin-id/> shares the
urn:xmpp:sid:0 namespace, and occupants can inject stanza-id
elements with arbitrary `by' values."
  (seq-find
   (lambda (child)
     (and (eq (jabber-xml-node-name child) 'stanza-id)
          (string= (jabber-xml-get-xmlns child) "urn:xmpp:sid:0")
          (jabber-xml-get-attribute child 'id)
          (equal (jabber-xml-get-attribute child 'by) expected-by)))
   (jabber-xml-node-children xml-data)))

(defun jabber-db--message-handler (jc xml-data)
  "Store incoming message in the database.
JC is the Jabber connection.
XML-DATA is the parsed stanza."
  (unless (or (null (jabber-xml-get-attribute xml-data 'from))
              (run-hook-with-args-until-success
               'jabber-history-inhibit-received-message-functions
               jc xml-data))
    (let* ((from (jabber-xml-get-attribute xml-data 'from))
           (body (car (jabber-xml-node-children
                       (car (jabber-xml-get-children xml-data 'body)))))
           (timestamp (jabber-message-timestamp xml-data))
           (type (jabber-xml-get-attribute xml-data 'type))
           (peer (jabber-jid-user from))
           (resource (jabber-jid-resource from))
           (direction
            (if (and (equal type "groupchat")
                     resource
                     (equal resource (jabber-muc-nickname peer jc)))
                "out"
              "in"))
           (stanza-id (jabber-xml-get-attribute xml-data 'id))
           (server-id
            ;; Trust only the stanza-id assigned by our own server
            ;; (1:1) or by a joined room itself (MUC).
            (when-let* ((expected-by
                         (if (string= type "groupchat")
                             (and (jabber-muc-joined-p (jabber-jid-user from) jc)
                                  (jabber-jid-user from))
                           (jabber-connection-bare-jid jc)))
                        (sid-el (jabber-db--stanza-id-element
                                 xml-data expected-by)))
              (jabber-xml-get-attribute sid-el 'id)))
           (oob-entries (jabber-db--extract-oob-entries xml-data))
           (encrypted (jabber-xml-encrypted-p xml-data)))
      (when (and from body)
        (jabber-db-store-message
         (jabber-connection-bare-jid jc)
         peer
         direction
         (or type "chat")
         body
         (floor (float-time (or timestamp (current-time))))
         resource
         stanza-id
         server-id
         (jabber-db--extract-occupant-id xml-data)
         oob-entries
         encrypted
         (jabber-db--extract-reply-fields xml-data)
         (jabber-db--extract-thread-fields xml-data)
         (let ((identity (jabber-db--message-identity xml-data)))
           (when (equal type "groupchat")
             (plist-put identity :room-id server-id))
           identity))))))

(defun jabber-db--outgoing-handler (body id &optional reply thread)
  "Store outgoing chat message in the database.
BODY is the message text.  ID is the stanza id for dedup.
Called from `jabber-chat-send-hooks'.  Reply metadata is read from
`jabber-chat--send-hook-stanza' when the hooks that emit the reply
elements have already run.  Optional REPLY and THREAD supply metadata
for messages stored before those hooks run."
  (when (and jabber-chatting-with jabber-buffer-connection
             (not (bound-and-true-p jabber-chat--sending-correction)))
    (jabber-db-store-message
     (jabber-connection-bare-jid jabber-buffer-connection)
     (jabber-jid-user jabber-chatting-with)
     "out"
     "chat"
     body
     (floor (float-time))
     (when (jabber-muc-sender-p jabber-chatting-with)
       (jabber-jid-resource jabber-chatting-with))
     id
     nil nil nil
     (memq jabber-chat-encryption '(omemo openpgp openpgp-legacy))
     (or reply
         (and (bound-and-true-p jabber-chat--send-hook-stanza)
              (jabber-db--extract-reply-fields
               jabber-chat--send-hook-stanza)))
     (or thread
         (and (bound-and-true-p jabber-chat--send-hook-stanza)
              (jabber-db--extract-thread-fields
               jabber-chat--send-hook-stanza)))
     (and (bound-and-true-p jabber-chat--send-hook-stanza)
          (jabber-db--message-identity jabber-chat--send-hook-stanza))))
  nil)

(defun jabber-db--store-outgoing (jc to body type)
  "Store an outgoing message sent via `jabber-send-message'.
JC is the connection, TO is the recipient JID, BODY is the text,
TYPE is the message type."
  (when (and body (not (string= type "groupchat")))
    (jabber-db-store-message
     (jabber-connection-bare-jid jc)
     (jabber-jid-user to)
     "out"
     (or type "chat")
     body
     (floor (float-time)))))

;;; History import
;;
;; One-time migration from the legacy flat-file history format
;; (formerly in jabber-history.el) into the SQLite database.

(defcustom jabber-history-dir
  (locate-user-emacs-file "jabber-history" ".emacs-jabber")
  "Base directory where per-contact history files are stored.
Used only when `jabber-use-global-history' is nil."
  :type 'directory)

(defcustom jabber-global-history-filename
  (locate-user-emacs-file "jabber-global-message-log"
                          ".jabber_global_message_log")
  "Global file where all messages are logged.
Used when `jabber-use-global-history' is non-nil."
  :type 'file)

(defcustom jabber-use-global-history
  (file-exists-p jabber-global-history-filename)
  "Whether to use a global file for message history.
If non-nil, `jabber-global-history-filename' is used, otherwise,
messages are stored in per-user files under the
`jabber-history-dir' directory."
  :type 'boolean)

(defun jabber-db-import-history (account)
  "Import message history from flat files into the SQLite database.
ACCOUNT is the bare JID to associate with imported messages.
Reads from either the global history file or per-user history
files, depending on the value of `jabber-use-global-history'."
  (interactive (list (read-string "Account JID: ")))
  (jabber-db-ensure-open)
  (let ((files (if jabber-use-global-history
                   (when (file-readable-p jabber-global-history-filename)
                     (list jabber-global-history-filename))
                 (when (file-directory-p jabber-history-dir)
                   (directory-files jabber-history-dir t "\\`[^.]"))))
        (count 0))
    (unless files
      (user-error "No history files found"))
    (let ((progress (make-progress-reporter
                     "Importing history..." 0 (length files)))
          (file-idx 0))
      (jabber-db-with-transaction
        (dolist (file files)
          (when (file-readable-p file)
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8))
                (insert-file-contents file))
              (goto-char (point-min))
              (while (not (eobp))
                (condition-case nil
                    (let* ((entry (read (current-buffer)))
                           (time-str (aref entry 0))
                           (direction (aref entry 1))
                           (from (aref entry 2))
                           (to (aref entry 3))
                           (body (aref entry 4))
                           (peer (jabber-jid-user
                                  (if (string= from "me") to from)))
                           (timestamp (floor
                                       (float-time
                                        (jabber-parse-time time-str)))))
                      (jabber-db-store-message
                       account peer direction "chat" body timestamp)
                      (cl-incf count))
                  (error (forward-line 1))))))
          (cl-incf file-idx)
          (progress-reporter-update progress file-idx)))
      (progress-reporter-done progress))
    (message "Imported %d messages into database" count)))

;;; Lifecycle hooks

(defun jabber-db--on-connect (_jc)
  "Open the database on connect."
  (jabber-db-ensure-open))

(defun jabber-db--on-disconnect ()
  "Close the database on disconnect."
  (jabber-db-close))

;;; Registration

(jabber-chain-add 'jabber-message-chain #'jabber-db--message-handler 90)
;; Depth 90: run after the hooks that attach reply/receipt elements,
;; so the stored row sees the complete stanza.
(add-hook 'jabber-chat-send-hooks #'jabber-db--outgoing-handler 90)
(add-hook 'jabber-post-connect-hooks #'jabber-db--on-connect)
(add-hook 'jabber-pre-disconnect-hook #'jabber-db--on-disconnect)
(add-hook 'kill-emacs-hook #'jabber-db-close)

(provide 'jabber-db)

;;; jabber-db.el ends here
