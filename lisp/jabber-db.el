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
(eval-when-compile (require 'cl-lib))

;; Global reference declarations
(declare-function jabber-xml-child-with-xmlns "jabber-xml.el"
                  (node xmlns))
(declare-function jabber-muc-joined-p "jabber-muc" (group))
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

(defun jabber-db--init-schema (db)
  "Initialize the database schema in DB."
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS message (
  id           INTEGER PRIMARY KEY,
  stanza_id    TEXT,
  server_id    TEXT,
  account      TEXT NOT NULL,
  peer         TEXT NOT NULL,
  resource     TEXT,
  direction    TEXT NOT NULL,
  type         TEXT,
  body         TEXT,
  timestamp    INTEGER NOT NULL,
  encrypted    INTEGER DEFAULT 0,
  raw_xml      TEXT,
  oob_url      TEXT,
  oob_desc     TEXT,
  delivered_at INTEGER,
  displayed_at INTEGER
)")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_peer_ts
  ON message(account, peer, timestamp)")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_stanza_id
  ON message(stanza_id) WHERE stanza_id IS NOT NULL")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_msg_server_id
  ON message(server_id) WHERE server_id IS NOT NULL")
  ;; FTS5 for full-text search
  (sqlite-execute db "\
CREATE VIRTUAL TABLE IF NOT EXISTS message_fts USING fts5(
  body,
  content='message',
  content_rowid='id'
)")
  ;; Triggers to keep FTS in sync
  (sqlite-execute db "\
CREATE TRIGGER IF NOT EXISTS message_ai AFTER INSERT ON message BEGIN
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END")
  (sqlite-execute db "\
CREATE TRIGGER IF NOT EXISTS message_ad AFTER DELETE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
END")
  (sqlite-execute db "\
CREATE TRIGGER IF NOT EXISTS message_au AFTER UPDATE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END")
  ;; OMEMO tables
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_store (
  account TEXT PRIMARY KEY,
  store_blob BLOB NOT NULL)")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_sessions (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  session_blob BLOB NOT NULL,
  PRIMARY KEY (account, jid, device_id))")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_trust (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  identity_key BLOB NOT NULL,
  trust INTEGER DEFAULT 0,
  first_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_skipped_keys (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  dh_key BLOB NOT NULL,
  message_number INTEGER NOT NULL,
  message_key BLOB NOT NULL,
  created_at INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id, dh_key, message_number))")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_devices (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  active INTEGER DEFAULT 1,
  last_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_omemo_trust_jid
  ON omemo_trust (account, jid)")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_omemo_devices_jid
  ON omemo_devices (account, jid)")
  (sqlite-execute db "\
CREATE INDEX IF NOT EXISTS idx_omemo_sessions_jid
  ON omemo_sessions (account, jid)")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS omemo_device_id (
  account TEXT PRIMARY KEY,
  device_id INTEGER NOT NULL)")
  (sqlite-execute db "\
CREATE TABLE IF NOT EXISTS chat_settings (
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  encryption TEXT DEFAULT 'default',
  PRIMARY KEY (account, peer))"))

(defun jabber-db--migrate (db)
  "Check user_version and apply migrations to DB."
  (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
    (when (zerop version)
      ;; Fresh database: create latest schema, skip intermediate migrations.
      (jabber-db--init-schema db)
      (sqlite-execute db "PRAGMA user_version=3")
      (setq version 3))
    (when (< version 2)
      ;; v1->v2: Remove duplicate messages that lack stanza_id and server_id.
      ;; These were created by MUC server history replayed on every join.
      (sqlite-execute db "\
DELETE FROM message WHERE id NOT IN (
  SELECT MIN(id) FROM message
  GROUP BY account, peer, timestamp, body, resource)")
      (sqlite-execute db "PRAGMA user_version=2"))
    (when (< version 3)
      ;; v2->v3: Add receipt timestamp columns for XEP-0184/XEP-0333.
      (sqlite-execute db "ALTER TABLE message ADD COLUMN delivered_at INTEGER")
      (sqlite-execute db "ALTER TABLE message ADD COLUMN displayed_at INTEGER")
      (sqlite-execute db "PRAGMA user_version=3"))))

(defun jabber-db-ensure-open ()
  "Open the SQLite database, creating it if needed.  Idempotent.
Return the database connection, or nil if storage is disabled."
  (when jabber-db-path
    (unless (and jabber-db--connection
                 (sqlitep jabber-db--connection))
      (let ((dir (file-name-directory jabber-db-path)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (setq jabber-db--connection (sqlite-open jabber-db-path))
      (sqlite-execute jabber-db--connection "PRAGMA journal_mode=WAL")
      (sqlite-execute jabber-db--connection "PRAGMA synchronous=NORMAL")
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

(defun jabber-db-store-message (account peer direction type body timestamp
                                        &optional resource stanza-id
                                        server-id raw-xml oob-url oob-desc
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
Optional RAW-XML is the full stanza as a string.
Optional OOB-URL is the jabber:x:oob URL.
Optional OOB-DESC is the jabber:x:oob description.
Optional ENCRYPTED is non-nil if the message was OMEMO-encrypted."
  (when-let* ((db (jabber-db-ensure-open)))
    ;; Dedup by stanza_id or server_id if present, otherwise by content
    (unless (or (and stanza-id
                     (caar (sqlite-select
                            db
                            "SELECT 1 FROM message WHERE stanza_id = ? LIMIT 1"
                            (list stanza-id))))
                (and server-id
                     (caar (sqlite-select
                            db
                            "SELECT 1 FROM message WHERE server_id = ? LIMIT 1"
                            (list server-id))))
                ;; Fallback: content-based dedup for messages without IDs
                ;; (e.g. MUC history replayed on every join)
                (and (not stanza-id) (not server-id)
                     (caar (sqlite-select
                            db
                            "SELECT 1 FROM message \
WHERE account = ? AND peer = ? AND timestamp = ? AND body = ? LIMIT 1"
                            (list account peer timestamp body)))))
      (sqlite-execute
       db
       "INSERT INTO message \
(account, peer, resource, direction, type, body, timestamp, \
stanza_id, server_id, raw_xml, oob_url, oob_desc, encrypted) \
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       (list account peer resource direction type body timestamp
             stanza-id server-id raw-xml oob-url oob-desc
             (if encrypted 1 0))))))

;;; Receipt updates

(defun jabber-db-update-receipt (stanza-id column timestamp)
  "Set COLUMN to TIMESTAMP for message with STANZA-ID.
COLUMN is \"delivered_at\" or \"displayed_at\".
The IS NULL guard prevents overwriting an earlier timestamp."
  (when (and jabber-db--connection stanza-id)
    (sqlite-execute jabber-db--connection
                    (format "UPDATE message SET %s = ? WHERE stanza_id = ? AND %s IS NULL"
                            column column)
                    (list timestamp stanza-id))))

;;; Retrieval

(defun jabber-db--row-to-plist (row)
  "Convert a database ROW to a message plist.
ROW is (account peer direction body timestamp resource type
oob_url oob_desc encrypted stanza_id delivered_at displayed_at)."
  (let* ((account (nth 0 row))
         (peer (nth 1 row))
         (direction (nth 2 row))
         (body (nth 3 row))
         (timestamp (nth 4 row))
         (resource (nth 5 row))
         (stanza-id (nth 10 row))
         (delivered-at (nth 11 row))
         (displayed-at (nth 12 row))
         (from (if (string= direction "in")
                   (if resource (concat peer "/" resource) peer)
                 account)))
    (list :id stanza-id
          :from from
          :body (or body "")
          :subject nil
          :timestamp (seconds-to-time timestamp)
          :delayed t
          :encrypted (and (nth 9 row) (not (zerop (nth 9 row))))
          :direction direction
          :msg-type (nth 6 row)
          :oob-url (nth 7 row)
          :oob-desc (nth 8 row)
          :error-text nil
          :status (cond
                   (displayed-at :displayed)
                   (delivered-at :delivered)
                   ((string= direction "out") :undelivered)))))

(defun jabber-db-backlog (account peer &optional count start-time)
  "Return the last COUNT messages for PEER on ACCOUNT.
Messages are returned as plists with keys :from, :body, :timestamp,
:delayed, :direction, :msg-type, etc.
COUNT defaults to `jabber-backlog-number'.
START-TIME is a float-time; only messages after this time are returned.
If nil, `jabber-backlog-days' is used to compute the cutoff."
  (when-let* ((db (jabber-db-ensure-open)))
    (let* ((n (or count jabber-backlog-number))
           (cutoff (cond
                    (start-time (floor start-time))
                    (jabber-backlog-days
                     (floor (- (float-time) (* jabber-backlog-days 86400.0))))
                    (t 0)))
           (rows (sqlite-select
                  db
                  "SELECT account, peer, direction, body, timestamp, resource, type, \
oob_url, oob_desc, encrypted, stanza_id, delivered_at, displayed_at \
FROM message \
WHERE account = ? AND peer = ? AND timestamp >= ? \
ORDER BY timestamp DESC LIMIT ?"
                  (list account peer cutoff
                        (if (eq n t) -1 n)))))
      (mapcar #'jabber-db--row-to-plist rows))))

(defun jabber-db-query (account peer &optional start-time end-time limit offset)
  "Query messages for PEER on ACCOUNT with pagination.
Returns a list of plists with keys :id, :stanza-id, :server-id,
:account, :peer, :resource, :direction, :type, :body, :timestamp,
:encrypted, :raw-xml.
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
direction, type, body, timestamp, encrypted, raw_xml \
FROM message \
WHERE account = ? AND peer = ? AND timestamp >= ? AND timestamp <= ? \
ORDER BY timestamp ASC LIMIT ? OFFSET ?"
                  (list account peer st et lim off))))
      (mapcar (lambda (row)
              (list :id (nth 0 row)
                    :stanza-id (nth 1 row)
                    :server-id (nth 2 row)
                    :account (nth 3 row)
                    :peer (nth 4 row)
                    :resource (nth 5 row)
                    :direction (nth 6 row)
                    :type (nth 7 row)
                    :body (nth 8 row)
                    :timestamp (nth 9 row)
                    :encrypted (nth 10 row)
                    :raw-xml (nth 11 row)))
            rows))))

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
m.peer, m.resource, m.direction, m.type, m.body, m.timestamp, \
m.encrypted, m.raw_xml \
FROM message m \
JOIN message_fts f ON f.rowid = m.id \
WHERE f.body MATCH ? AND m.account = ? AND m.peer = ? \
ORDER BY m.timestamp DESC LIMIT ?"
                    (list query account peer lim))
                 (sqlite-select
                  db
                  "SELECT m.id, m.stanza_id, m.server_id, m.account, \
m.peer, m.resource, m.direction, m.type, m.body, m.timestamp, \
m.encrypted, m.raw_xml \
FROM message m \
JOIN message_fts f ON f.rowid = m.id \
WHERE f.body MATCH ? AND m.account = ? \
ORDER BY m.timestamp DESC LIMIT ?"
                  (list query account lim)))))
      (mapcar (lambda (row)
                (list :id (nth 0 row)
                      :stanza-id (nth 1 row)
                      :server-id (nth 2 row)
                      :account (nth 3 row)
                      :peer (nth 4 row)
                      :resource (nth 5 row)
                      :direction (nth 6 row)
                      :type (nth 7 row)
                      :body (nth 8 row)
                      :timestamp (nth 9 row)
                      :encrypted (nth 10 row)
                      :raw-xml (nth 11 row)))
              rows))))

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

(defun jabber-db--message-handler (jc xml-data)
  "Store incoming message in the database.
JC is the Jabber connection.
XML-DATA is the parsed stanza."
  (unless (run-hook-with-args-until-success
           'jabber-history-inhibit-received-message-functions
           jc xml-data)
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
           (oob-x (cl-find-if
                    (lambda (x)
                      (and (listp x)
                           (string= (jabber-xml-get-attribute x 'xmlns)
                                    jabber-oob-xmlns)))
                    (jabber-xml-node-children xml-data)))
           (oob-url (when oob-x
                      (car (jabber-xml-node-children
                            (car (jabber-xml-get-children oob-x 'url))))))
           (oob-desc (when oob-x
                       (car (jabber-xml-node-children
                             (car (jabber-xml-get-children oob-x 'desc))))))
           (encrypted (or (jabber-xml-child-with-xmlns
                          xml-data "eu.siacs.conversations.axolotl")
                         (jabber-xml-child-with-xmlns
                          xml-data "jabber:x:encrypted")
                         (jabber-xml-child-with-xmlns
                          xml-data "urn:xmpp:openpgp:0"))))
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
         server-id nil
         oob-url oob-desc
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
     nil id
     nil nil nil nil
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
        (progress-reporter-update progress file-idx))
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

(add-to-list 'jabber-message-chain #'jabber-db--message-handler t)
(add-hook 'jabber-chat-send-hooks #'jabber-db--outgoing-handler)
(add-hook 'jabber-post-connect-hooks #'jabber-db--on-connect)
(add-hook 'jabber-pre-disconnect-hook #'jabber-db--on-disconnect)
(add-hook 'kill-emacs-hook #'jabber-db-close)

(provide 'jabber-db)

;;; jabber-db.el ends here
