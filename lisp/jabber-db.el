;;; jabber-db.el --- SQLite message storage for jabber.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 emacs-jabber contributors

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

;; Global reference declarations
(declare-function jabber-muc-message-p "jabber-muc.el" (message))
(declare-function jabber-history-backlog "jabber-history.el"
                  (jid &optional before))
(declare-function jabber-history-filename "jabber-history.el" (contact))
(defvar jabber-history-enabled)         ; jabber-history.el
(defvar jabber-history-muc-enabled)     ; jabber-history.el
(defvar jabber-history-dir)             ; jabber-history.el
(defvar jabber-use-global-history)      ; jabber-history.el
(defvar jabber-global-history-filename) ; jabber-history.el
(defvar jabber-backlog-days)            ; jabber-history.el
(defvar jabber-backlog-number)          ; jabber-history.el
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-chat-send-hooks)        ; jabber-chat.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-message-chain nil)       ; jabber-core.el
(defvar jabber-post-connect-hooks nil) ; jabber-core.el
(defvar jabber-pre-disconnect-hook nil) ; jabber-core.el
(defvar jabber-history-inhibit-received-message-functions) ; jabber-history.el

(defgroup jabber-db nil
  "SQLite message storage for jabber.el."
  :group 'jabber)

(defcustom jabber-db-path (locate-user-emacs-file "jabber-db.sqlite")
  "Path to the SQLite database file for message storage."
  :type 'string
  :group 'jabber-db)

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
  raw_xml      TEXT
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
END"))

(defun jabber-db--migrate (db)
  "Check user_version and apply migrations to DB."
  (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
    (when (< version 1)
      (jabber-db--init-schema db)
      (sqlite-execute db "PRAGMA user_version=1"))))

(defun jabber-db-ensure-open ()
  "Open the SQLite database, creating it if needed.  Idempotent.
Return the database connection."
  (unless (and jabber-db--connection
               (sqlitep jabber-db--connection))
    (let ((dir (file-name-directory jabber-db-path)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (setq jabber-db--connection (sqlite-open jabber-db-path))
    (sqlite-execute jabber-db--connection "PRAGMA journal_mode=WAL")
    (sqlite-execute jabber-db--connection "PRAGMA synchronous=NORMAL")
    (jabber-db--migrate jabber-db--connection))
  jabber-db--connection)

(defun jabber-db-close ()
  "Close the database connection."
  (when (and jabber-db--connection
             (sqlitep jabber-db--connection))
    (sqlite-close jabber-db--connection)
    (setq jabber-db--connection nil)))

;;; Storage

(defun jabber-db-store-message (account peer direction type body timestamp
                                        &optional resource stanza-id
                                        server-id raw-xml)
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
Optional RAW-XML is the full stanza as a string."
  (let ((db (jabber-db-ensure-open)))
    ;; Dedup by stanza_id if present
    (unless (and stanza-id
                 (caar (sqlite-select
                        db
                        "SELECT 1 FROM message WHERE stanza_id = ? LIMIT 1"
                        (list stanza-id))))
      (sqlite-execute
       db
       "INSERT INTO message \
(account, peer, resource, direction, type, body, timestamp, \
stanza_id, server_id, raw_xml) \
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       (list account peer resource direction type body timestamp
             stanza-id server-id raw-xml)))))

;;; Retrieval

(defun jabber-db--row-to-vector (row)
  "Convert a database ROW to the backlog vector format.
Returns [timestamp direction sender recipient body] where
timestamp is an XEP-0082 string."
  (let ((peer (nth 1 row))
        (direction (nth 2 row))
        (body (nth 3 row))
        (timestamp (nth 4 row)))
    (vector
     (jabber-encode-time (seconds-to-time timestamp))
     direction
     (if (string= direction "in") peer "me")
     (if (string= direction "in") "me" peer)
     (or body ""))))

(defun jabber-db-backlog (account peer &optional count start-time)
  "Return the last COUNT messages for PEER on ACCOUNT.
Messages are returned as vectors matching the format expected by
`jabber-chat-insert-backlog-entry': [timestamp direction sender recipient body].
COUNT defaults to `jabber-backlog-number'.
START-TIME is a float-time; only messages after this time are returned.
If nil, `jabber-backlog-days' is used to compute the cutoff."
  (let* ((db (jabber-db-ensure-open))
         (n (or count jabber-backlog-number))
         (cutoff (cond
                  (start-time (floor start-time))
                  (jabber-backlog-days
                   (floor (- (float-time) (* jabber-backlog-days 86400.0))))
                  (t 0)))
         (rows (sqlite-select
                db
                "SELECT account, peer, direction, body, timestamp \
FROM message \
WHERE account = ? AND peer = ? AND timestamp >= ? \
ORDER BY timestamp DESC LIMIT ?"
                (list account peer cutoff
                      (if (eq n t) -1 n)))))
    (mapcar #'jabber-db--row-to-vector rows)))

(defun jabber-db-query (account peer &optional start-time end-time limit offset)
  "Query messages for PEER on ACCOUNT with pagination.
Returns a list of plists with keys :id, :stanza-id, :server-id,
:account, :peer, :resource, :direction, :type, :body, :timestamp,
:encrypted, :raw-xml.
START-TIME and END-TIME are unix epoch integers.
LIMIT defaults to 50, OFFSET defaults to 0."
  (let* ((db (jabber-db-ensure-open))
         (lim (or limit 50))
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
            rows)))

(defun jabber-db-search (account query &optional peer limit)
  "Full-text search for QUERY in messages on ACCOUNT.
Optional PEER restricts to a specific contact.
LIMIT defaults to 50.
Returns matching messages as plists."
  (let* ((db (jabber-db-ensure-open))
         (lim (or limit 50))
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
            rows)))

(defun jabber-db-last-timestamp (account peer)
  "Return the latest stored timestamp for PEER on ACCOUNT.
Returns a unix epoch integer, or nil if no messages exist."
  (let ((db (jabber-db-ensure-open)))
    (caar (sqlite-select
           db
           "SELECT MAX(timestamp) FROM message \
WHERE account = ? AND peer = ?"
           (list account peer)))))

;;; Message chain handlers

(defun jabber-db--message-handler (jc xml-data)
  "Store incoming message in the database.
JC is the Jabber connection.
XML-DATA is the parsed stanza."
  (let ((is-muc (jabber-muc-message-p xml-data)))
    (when (or (not is-muc) jabber-history-muc-enabled)
      (unless (run-hook-with-args-until-success
               'jabber-history-inhibit-received-message-functions
               jc xml-data)
        (let* ((from (jabber-xml-get-attribute xml-data 'from))
               (body (car (jabber-xml-node-children
                           (car (jabber-xml-get-children xml-data 'body)))))
               (timestamp (jabber-message-timestamp xml-data))
               (type (jabber-xml-get-attribute xml-data 'type))
               (stanza-id (jabber-xml-get-attribute xml-data 'id)))
          (when (and from body)
            (jabber-db-store-message
             (jabber-connection-bare-jid jc)
             (jabber-jid-user from)
             "in"
             (or type "chat")
             body
             (floor (float-time (or timestamp (current-time))))
             (jabber-jid-resource from)
             stanza-id)))))))

(defun jabber-db--outgoing-handler (body _id)
  "Store outgoing chat message in the database.
BODY is the message text.  Called from `jabber-chat-send-hooks'."
  (when (and jabber-chatting-with jabber-buffer-connection)
    (jabber-db-store-message
     (jabber-connection-bare-jid jabber-buffer-connection)
     (jabber-jid-user jabber-chatting-with)
     "out"
     "chat"
     body
     (floor (float-time)))))

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

(defun jabber-db-import-history ()
  "Import message history from flat files into the SQLite database.
Reads from either the global history file or per-user history
files, depending on the value of `jabber-use-global-history'."
  (interactive)
  (require 'jabber-history)
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

(add-to-list 'jabber-message-chain #'jabber-db--message-handler)
(add-hook 'jabber-chat-send-hooks #'jabber-db--outgoing-handler)
(add-hook 'jabber-post-connect-hooks #'jabber-db--on-connect)
(add-hook 'jabber-pre-disconnect-hook #'jabber-db--on-disconnect)
(add-hook 'kill-emacs-hook #'jabber-db-close)

(provide 'jabber-db)

;;; jabber-db.el ends here
