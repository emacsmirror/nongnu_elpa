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

(require 'jabber-util)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

;; Global reference declarations
(declare-function jabber-xml-child-with-xmlns "jabber-xml.el"
                  (node xmlns))
(declare-function jabber-xml-get-attribute "jabber-xml.el" (node attribute))
(declare-function jabber-muc-joined-p "jabber-muc" (group &optional jc))
(declare-function jabber-muc-sender-p "jabber-muc" (jid))
(declare-function jabber-xml-encrypted-p "jabber-xml" (xml-data))
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-chat-send-hooks)        ; jabber-chat.el
(defvar jabber-chat-encryption)        ; jabber-chatbuffer.el
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
                 (const :tag "Disabled" nil))
  :group 'jabber-db)

(defcustom jabber-backlog-days nil
  "Age limit on messages in chat buffer backlog, in days."
  :group 'jabber-db
  :type '(choice (number :tag "Number of days")
                 (const :tag "No limit" nil)))

(defcustom jabber-backlog-number 30
  "Maximum number of messages in chat buffer backlog."
  :group 'jabber-db
  :type 'integer)

(defvar jabber-history-inhibit-received-message-functions nil
  "Functions determining whether to log an incoming message stanza.
The functions in this list are called with two arguments,
the connection and the full message stanza.
If any of the functions returns non-nil, the stanza is not logged
in the message history.")

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
  edited       INTEGER DEFAULT 0)"
    "CREATE INDEX IF NOT EXISTS idx_msg_peer_ts
  ON message(account, peer, timestamp)"
    "CREATE INDEX IF NOT EXISTS idx_msg_stanza_id
  ON message(account, stanza_id) WHERE stanza_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_server_id
  ON message(account, server_id) WHERE server_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_occupant_id
  ON message(account, peer, occupant_id) WHERE occupant_id IS NOT NULL"
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
  store_blob BLOB NOT NULL)"
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
  PRIMARY KEY (account, peer))"
    "CREATE TABLE IF NOT EXISTS message_oob (
  id         INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  url        TEXT NOT NULL,
  desc       TEXT)"
    "CREATE INDEX IF NOT EXISTS idx_oob_message_id
  ON message_oob(message_id)")
  "DDL statements for the latest database schema.")

(defun jabber-db--init-schema (db)
  "Initialize the database schema in DB."
  (dolist (ddl jabber-db--schema-ddl)
    (sqlite-execute db ddl)))

(defconst jabber-db--schema-version 3
  "Current schema version.
Bump this when adding migrations.  A database whose version
exceeds this value is from a newer (or development) build and
cannot be used; the user is prompted to delete it.")

(defun jabber-db--handle-unknown-schema (db)
  "Detect a schema newer than `jabber-db--schema-version' and offer to reset.
Returns non-nil if the database was deleted and the caller should
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

(defun jabber-db--migrate (db)
  "Check user_version and apply migrations to DB."
  (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
    (when (zerop version)
      (jabber-db--init-schema db)
      (sqlite-execute db
                      (format "PRAGMA user_version=%d"
                              jabber-db--schema-version))
      (setq version jabber-db--schema-version))
    (when (= version 1)
      (sqlite-execute db "ALTER TABLE message ADD COLUMN occupant_id TEXT")
      (sqlite-execute db "ALTER TABLE message DROP COLUMN raw_xml")
      (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_occupant_id
  ON message(account, peer, occupant_id) WHERE occupant_id IS NOT NULL")
      (sqlite-execute db "PRAGMA user_version=2")
      (setq version 2))
    (when (= version 2)
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
      (sqlite-execute db "PRAGMA user_version=3")
      (setq version 3))))

(defun jabber-db-ensure-open ()
  "Open the SQLite database, creating it if needed.  Idempotent.
Return the database connection, or nil if storage is disabled."
  (when jabber-db-path
    (unless (and jabber-db--connection
                 (sqlitep jabber-db--connection))
      (let ((dir (file-name-directory jabber-db-path)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (let ((db (sqlite-open jabber-db-path)))
        (when (jabber-db--handle-unknown-schema db)
          ;; Database was deleted; re-open fresh.
          (setq db (sqlite-open jabber-db-path)))
        (setq jabber-db--connection db))
      (sqlite-execute jabber-db--connection "PRAGMA journal_mode=WAL")
      (sqlite-execute jabber-db--connection "PRAGMA synchronous=NORMAL")
      (sqlite-execute jabber-db--connection "PRAGMA foreign_keys=ON")
      (jabber-db--migrate jabber-db--connection))
    jabber-db--connection))

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
INSERT OR REPLACE INTO chat_settings (account, peer, encryption)
  VALUES (?, ?, ?)"
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

;;; Storage

(defun jabber-db--detect-duplicate (db account peer timestamp body
                                       stanza-id server-id)
  "Check whether a message already exists in DB.
Return a symbol indicating the match type: `stanza_id', `server_id',
`content', or nil for no match."
  (cond
   ((and stanza-id
         (caar (sqlite-select
                db "SELECT 1 FROM message \
WHERE stanza_id = ? AND account = ? LIMIT 1"
                (list stanza-id account))))
    'stanza_id)
   ((and server-id
         (caar (sqlite-select
                db "SELECT 1 FROM message \
WHERE server_id = ? AND account = ? LIMIT 1"
                (list server-id account))))
    'server_id)
   ;; Content-based dedup: matches messages stored by the
   ;; live handler (nil IDs) against MAM replays (with IDs),
   ;; or MUC history replayed on every join.
   ((caar (sqlite-select
           db "SELECT 1 FROM message \
WHERE account = ? AND peer = ? AND timestamp = ? AND body = ? LIMIT 1"
           (list account peer timestamp body)))
    'content)))

(defun jabber-db--insert-message (db account peer resource occupant-id
                                     direction type body timestamp
                                     stanza-id server-id encrypted
                                     oob-entries)
  "Insert a new message row into DB and attach OOB entries."
  (sqlite-execute
   db
   "INSERT INTO message \
(account, peer, resource, occupant_id, direction, type, body, timestamp, \
stanza_id, server_id, encrypted) \
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (list account peer resource occupant-id direction type body timestamp
         stanza-id server-id (if encrypted 1 0)))
  (when oob-entries
    (let ((msg-id (caar (sqlite-select db "SELECT last_insert_rowid()"))))
      (dolist (entry oob-entries)
        (sqlite-execute
         db
         "INSERT INTO message_oob (message_id, url, desc) VALUES (?, ?, ?)"
         (list msg-id (car entry) (cdr entry)))))))

(defun jabber-db--update-duplicate-ids (db account _peer timestamp body
                                           stanza-id server-id oob-entries
                                           dup-id-col)
  "Update an existing duplicate matched by DUP-ID-COL.
Normalizes timestamp and replaces failed-decrypt placeholders.
Skips retracted messages to prevent MAM replays from undoing retractions."
  (let* ((id-val (if (eq dup-id-col 'stanza_id) stanza-id server-id))
         (retracted (caar (sqlite-select
                           db
                           (format "SELECT 1 FROM message \
WHERE %s = ? AND account = ? AND retracted_by IS NOT NULL LIMIT 1"
                                   dup-id-col)
                           (list id-val account)))))
    (unless retracted
      ;; Normalize timestamp only when it differs.
      (sqlite-execute
       db
       (format "UPDATE message SET timestamp = ? \
WHERE %s = ? AND account = ? AND timestamp != ?"
               dup-id-col)
       (list timestamp id-val account timestamp))
      ;; Replace failed-decrypt placeholder if new body is real text.
      (when (and body
                 (not (string-match-p "\\`: could not decrypt\\]" body)))
        (let ((msg-id
               (caar (sqlite-select
                      db
                      (format "SELECT id FROM message \
WHERE %s = ? AND account = ? AND body LIKE '%%: could not decrypt]' LIMIT 1"
                              dup-id-col)
                      (list id-val account)))))
          (when msg-id
            (sqlite-execute
             db "UPDATE message SET body = ? WHERE id = ?"
             (list body msg-id))
            (sqlite-execute
             db "DELETE FROM message_oob WHERE message_id = ?"
             (list msg-id))
            (dolist (entry oob-entries)
              (sqlite-execute
               db
               "INSERT INTO message_oob (message_id, url, desc) \
VALUES (?, ?, ?)"
               (list msg-id (car entry) (cdr entry))))))))))

(defun jabber-db--upgrade-content-match (db account peer timestamp body
                                            stanza-id server-id)
  "Upgrade a content-matched row with server-assigned IDs."
  (when (or stanza-id server-id)
    (sqlite-execute
     db
     "UPDATE message SET stanza_id = COALESCE(stanza_id, ?), \
server_id = COALESCE(server_id, ?) \
WHERE account = ? AND peer = ? AND timestamp = ? AND body = ? \
AND stanza_id IS NULL AND server_id IS NULL"
     (list stanza-id server-id account peer timestamp body))))

(defun jabber-db-store-message (account peer direction type body timestamp
                                        &optional resource stanza-id
                                        server-id occupant-id oob-entries
                                        encrypted)
  "Store a message in the database.
ACCOUNT is the bare JID of the local account.
PEER is the bare JID of the contact or room.
DIRECTION is \"in\" or \"out\".
TYPE is the message type (\"chat\", \"groupchat\", \"headline\").
BODY is the message text.
TIMESTAMP is a unix epoch integer.
Optional RESOURCE is the sender resource.
Optional STANZA-ID is the XEP-0359 origin id.
Optional SERVER-ID is the XEP-0359 server-assigned id.
Optional OCCUPANT-ID is the XEP-0421 occupant id.
Optional OOB-ENTRIES is a list of (URL . DESC) cons cells for
jabber:x:oob elements.
Optional ENCRYPTED is non-nil if the message was OMEMO-encrypted."
  (when-let* ((db (jabber-db-ensure-open)))
    (let ((dup-id-col (jabber-db--detect-duplicate
                       db account peer timestamp body stanza-id server-id)))
      (pcase dup-id-col
        ('nil
         (jabber-db--insert-message db account peer resource occupant-id
                                   direction type body timestamp
                                   stanza-id server-id encrypted oob-entries))
        ((or 'stanza_id 'server_id)
         (jabber-db--update-duplicate-ids db account peer timestamp body
                                         stanza-id server-id oob-entries
                                         dup-id-col))
        ('content
         (jabber-db--upgrade-content-match db account peer timestamp body
                                          stanza-id server-id))))))

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

(defun jabber-db-cascade-displayed (account peer timestamp ref-timestamp)
  "Mark all outgoing messages before REF-TIMESTAMP as displayed.
ACCOUNT and PEER identify the conversation.  TIMESTAMP is the
current time to store as displayed_at.  REF-TIMESTAMP is the
timestamp of the referenced message.  Only updates messages with
direction=out that have delivered_at set but displayed_at IS NULL."
  (when jabber-db--connection
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET displayed_at = ? \
WHERE account = ? AND peer = ? AND direction = 'out' \
AND timestamp <= ? AND delivered_at IS NOT NULL AND displayed_at IS NULL"
                    (list timestamp account peer ref-timestamp))))

(defun jabber-db-retract-message (server-id retracted-by &optional reason)
  "Mark the message with SERVER-ID as retracted by RETRACTED-BY.
Optional REASON is the human-readable retraction reason string."
  (when (and jabber-db--connection server-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET retracted_by = ?, retraction_reason = ? WHERE server_id = ?"
                    (list retracted-by reason server-id))))

(defun jabber-db-occupant-id-by-server-id (server-id)
  "Return the occupant-id for the message with SERVER-ID, or nil."
  (when (and jabber-db--connection server-id)
    (caar (sqlite-select jabber-db--connection
                         "SELECT occupant_id FROM message \
WHERE server_id = ? LIMIT 1"
                         (list server-id)))))

(defun jabber-db-server-ids-by-occupant-id (account peer occupant-id)
  "Return server-ids for messages with OCCUPANT-ID in PEER on ACCOUNT.
Only returns non-retracted messages that have a server-id."
  (when-let* ((db (jabber-db-ensure-open)))
    (mapcar #'car
            (sqlite-select db
                           "SELECT server_id FROM message \
WHERE account = ? AND peer = ? AND occupant_id = ? \
AND server_id IS NOT NULL AND retracted_by IS NULL"
                           (list account peer occupant-id)))))

(defun jabber-db-correct-message (stanza-id new-body)
  "Replace body of message with STANZA-ID with NEW-BODY and mark as edited."
  (when (and jabber-db--connection stanza-id)
    (sqlite-execute jabber-db--connection
                    "UPDATE message SET body = ?, edited = 1 WHERE stanza_id = ?"
                    (list new-body stanza-id))))

(defun jabber-db-delete-peer-messages (account peer)
  "Delete all messages for PEER on ACCOUNT."
  (when-let* ((db (jabber-db-ensure-open)))
    (sqlite-execute db
      "DELETE FROM message WHERE account = ? AND peer = ?"
      (list account peer))))

(defun jabber-db-message-sender-by-stanza-id (stanza-id)
  "Return the from-JID of the stored message with STANZA-ID, or nil.
For incoming messages returns the full sender JID (peer/resource or peer).
For outgoing messages returns the account bare JID, enabling validation
of carbon copies of corrections sent from another device."
  (when (and jabber-db--connection stanza-id)
    (when-let* ((row (car (sqlite-select
                           jabber-db--connection
                           "SELECT direction, peer, resource, account \
FROM message WHERE stanza_id = ? LIMIT 1"
                           (list stanza-id)))))
      (seq-let (direction peer resource account) row
        (if (string= direction "in")
            (if resource (concat peer "/" resource) peer)
          account)))))

;;; Retrieval

(defun jabber-db--row-to-plist (row)
  "Convert a backlog ROW to a message plist.
ROW columns match the SELECT in `jabber-db-backlog'.
The :oob-entries key is populated later by `jabber-db--attach-oob-entries'."
  (seq-let (id account peer direction body timestamp resource type
            encrypted stanza-id delivered-at
            displayed-at server-id retracted-by retraction-reason edited)
      row
    (let ((from (if (string= direction "in")
                    (if resource (concat peer "/" resource) peer)
                  account)))
      (list :db-id id
            :id stanza-id
            :server-id server-id
            :from from
            :body (or body "")
            :subject nil
            :timestamp (seconds-to-time timestamp)
            :delayed t
            :encrypted (and encrypted (not (zerop encrypted)))
            :retracted (and retracted-by t)
            :retracted-by retracted-by
            :retraction-reason retraction-reason
            :edited (and edited (not (zerop edited)))
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

(defun jabber-db-backlog (account peer &optional count start-time resource
                                  msg-type)
  "Return the last COUNT messages for PEER on ACCOUNT.
Messages are returned as plists with keys :from, :body, :timestamp,
:delayed, :direction, :msg-type, etc.
COUNT defaults to `jabber-backlog-number'.
START-TIME is a float-time; only messages after this time are returned.
If nil, `jabber-backlog-days' is used to compute the cutoff.
RESOURCE, when non-nil, filters to messages from that resource only.
This is used for MUC private message buffers.
MSG-TYPE, when non-nil, filters to messages of that type only
\(e.g. \"groupchat\" for MUC buffers)."
  (when-let* ((db (jabber-db-ensure-open)))
    (let* ((n (or count jabber-backlog-number))
           (cutoff (cond
                    (start-time (floor start-time))
                    (jabber-backlog-days
                     (floor (- (float-time) (* jabber-backlog-days 86400.0))))
                    (t 0)))
           (base-cols "SELECT id, account, peer, direction, body, timestamp, \
resource, type, encrypted, stanza_id, delivered_at, displayed_at, \
server_id, retracted_by, retraction_reason, edited FROM message")
           (sql (cond
                 (resource
                  (concat base-cols " WHERE account = ? AND peer = ? \
AND type = 'chat' AND (resource = ? OR direction = 'out') \
AND timestamp >= ? ORDER BY timestamp DESC LIMIT ?"))
                 (msg-type
                  (concat base-cols " WHERE account = ? AND peer = ? \
AND type = ? AND timestamp >= ? \
ORDER BY timestamp DESC LIMIT ?"))
                 (t
                  (concat base-cols " WHERE account = ? AND peer = ? \
AND timestamp >= ? ORDER BY timestamp DESC LIMIT ?"))))
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
      (jabber-db--attach-oob-entries db plists))))

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
           (stanza-id (jabber-xml-get-attribute xml-data 'id))
           (server-id
            (when-let* ((sid-el (jabber-xml-child-with-xmlns
                                 xml-data "urn:xmpp:sid:0"))
                        (by (jabber-xml-get-attribute sid-el 'by))
                        ;; Trust stanza-id from our bare JID (1:1)
                        ;; or from a room we've joined (MUC).
                        ((or (string= by (jabber-connection-bare-jid jc))
                             (and (string= type "groupchat")
                                  (jabber-muc-joined-p
                                   (jabber-jid-user from))))))
              (jabber-xml-get-attribute sid-el 'id)))
           (oob-entries (jabber-db--extract-oob-entries xml-data))
           (encrypted (jabber-xml-encrypted-p xml-data)))
      (when (and from body)
        (jabber-db-store-message
         (jabber-connection-bare-jid jc)
         (jabber-jid-user from)
         "in"
         (or type "chat")
         body
         (floor (float-time (or timestamp (current-time))))
         (jabber-jid-resource from)
         stanza-id
         server-id
         (jabber-db--extract-occupant-id xml-data)
         oob-entries
         encrypted)))))

(defun jabber-db--outgoing-handler (body id)
  "Store outgoing chat message in the database.
BODY is the message text.  ID is the stanza id for dedup.
Called from `jabber-chat-send-hooks'."
  (when (and jabber-chatting-with jabber-buffer-connection)
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
     (memq jabber-chat-encryption '(omemo openpgp openpgp-legacy))))
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
  :group 'jabber-db
  :type 'directory)

(defcustom jabber-global-history-filename
  (locate-user-emacs-file "jabber-global-message-log"
                          ".jabber_global_message_log")
  "Global file where all messages are logged.
Used when `jabber-use-global-history' is non-nil."
  :group 'jabber-db
  :type 'file)

(defcustom jabber-use-global-history
  (file-exists-p jabber-global-history-filename)
  "Whether to use a global file for message history.
If non-nil, `jabber-global-history-filename' is used, otherwise,
messages are stored in per-user files under the
`jabber-history-dir' directory."
  :group 'jabber-db
  :type 'boolean)

(defun jabber-db-import-history ()
  "Import message history from flat files into the SQLite database.
Reads from either the global history file or per-user history
files, depending on the value of `jabber-use-global-history'."
  (interactive)
  (jabber-db-ensure-open)
  (let ((files (if jabber-use-global-history
                   (when (file-readable-p jabber-global-history-filename)
                     (list jabber-global-history-filename))
                 (when (file-directory-p jabber-history-dir)
                   (directory-files jabber-history-dir t "^[^.]"))))
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
                           (peer (if (string= from "me") to from))
                           (timestamp (floor
                                       (float-time
                                        (jabber-parse-time time-str)))))
                      (jabber-db-store-message
                       "" peer direction "chat" body timestamp)
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

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-message-chain #'jabber-db--message-handler 90))
(add-hook 'jabber-chat-send-hooks #'jabber-db--outgoing-handler)
(add-hook 'jabber-post-connect-hooks #'jabber-db--on-connect)
(add-hook 'jabber-pre-disconnect-hook #'jabber-db--on-disconnect)
(add-hook 'kill-emacs-hook #'jabber-db-close)

(provide 'jabber-db)

;;; jabber-db.el ends here
