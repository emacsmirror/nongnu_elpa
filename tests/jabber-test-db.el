;;; jabber-test-db.el --- Tests for jabber-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; SQLite message storage, schema, and queries.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-reactions)

(declare-function jabber-db-replace-reactions
                  "jabber-db" (account peer type target-id sender reactions
                                        &optional updated-at))

;;; Test infrastructure

(defmacro jabber-test-db-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Binds `jabber-db-path' to a temp file, ensures the DB is open,
and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-db--dir (make-temp-file "jabber-db-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-test-db--dir))
          (jabber-db--connection nil)
          (jabber-backlog-days 3.0)
          (jabber-backlog-number 10))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-test-db--dir)
         (delete-directory jabber-test-db--dir t)))))

;;; Group 1: Schema and lifecycle

(ert-deftest jabber-test-db-ensure-open-creates-db ()
  "Opening the database creates the file and returns a connection."
  (jabber-test-db-with-db
    (should (sqlitep jabber-db--connection))
    (should (file-exists-p jabber-db-path))))

(ert-deftest jabber-test-db-ensure-open-idempotent ()
  "Calling ensure-open twice returns the same connection."
  (jabber-test-db-with-db
    (let ((db1 jabber-db--connection)
          (db2 (jabber-db-ensure-open)))
      (should (eq db1 db2)))))

(ert-deftest jabber-test-db-close-and-reopen ()
  "Closing and reopening the database works."
  (jabber-test-db-with-db
    (jabber-db-close)
    (should (null jabber-db--connection))
    (let ((db (jabber-db-ensure-open)))
      (should (sqlitep db)))))

(ert-deftest jabber-test-db-schema-version ()
  "The user_version pragma matches `jabber-db--schema-version'."
  (jabber-test-db-with-db
    (should (= jabber-db--schema-version
               (caar (sqlite-select jabber-db--connection
                                    "PRAGMA user_version"))))))

(ert-deftest jabber-test-db-wal-mode ()
  "WAL journal mode is active."
  (jabber-test-db-with-db
    (should (string= "wal"
                     (caar (sqlite-select jabber-db--connection
                                         "PRAGMA journal_mode"))))))

(ert-deftest jabber-test-db-tables-exist ()
  "All expected tables and indexes exist."
  (jabber-test-db-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "message" tables))
      (should (member "message_fts" tables))
      (should (member "chat_settings" tables)))))

;;; Group 2: Store and retrieve

(ert-deftest jabber-test-db-store-and-query ()
  "Storing a message and querying it back returns matching fields."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello!" ts "laptop")
      (let* ((rows (jabber-db-query "me@example.com" "friend@example.com"))
             (row (car rows)))
        (should (= 1 (length rows)))
        (should (string= "me@example.com" (plist-get row :account)))
        (should (string= "friend@example.com" (plist-get row :peer)))
        (should (string= "in" (plist-get row :direction)))
        (should (string= "chat" (plist-get row :type)))
        (should (string= "Hello!" (plist-get row :body)))
        (should (= ts (plist-get row :timestamp)))
        (should (string= "laptop" (plist-get row :resource)))))))

(ert-deftest jabber-test-db-store-with-stanza-id ()
  "Storing a message with stanza-id and server-id preserves them."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Test" ts nil "origin-123" "server-456")
      (let* ((rows (jabber-db-query "me@example.com" "friend@example.com"))
             (row (car rows)))
        (should (string= "origin-123" (plist-get row :stanza-id)))
        (should (string= "server-456" (plist-get row :server-id)))))))

(ert-deftest jabber-test-db-store-unicode-body ()
  "Unicode text in message body is preserved."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time)))
          (body "Hej! Gruss Gott! Ελληνικά 日本語 🎉"))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" body ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (string= body (plist-get row :body)))))))

(ert-deftest jabber-test-db-store-nil-body ()
  "Storing a message with nil body succeeds."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" nil ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (null (plist-get row :body)))))))

(ert-deftest jabber-test-db-store-multiline-body ()
  "Newlines in message body are preserved."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time)))
          (body "Line one\nLine two\nLine three"))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" body ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (string= body (plist-get row :body)))))))

;;; Group 3: Backlog format and ordering

(ert-deftest jabber-test-db-backlog-plist-format ()
  "Backlog entries are plists with :from, :body, :timestamp, :delayed, :direction, :msg-type."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello!" ts "laptop")
      (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
             (entry (car entries)))
        (should (listp entry))
        (should (string= "friend@example.com/laptop" (plist-get entry :from)))
        (should (string= "Hello!" (plist-get entry :body)))
        (should (string= "in" (plist-get entry :direction)))
        (should (string= "chat" (plist-get entry :msg-type)))
        (should (plist-get entry :delayed))
        (should (plist-get entry :timestamp))))))

(ert-deftest jabber-test-db-backlog-chat-no-resource ()
  "Chat backlog sender is bare JID when no resource is stored."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello!" ts)
      (let ((entry (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
        (should (string= "friend@example.com" (plist-get entry :from)))))))

(ert-deftest jabber-test-db-backlog-outgoing-format ()
  "Outgoing backlog entries have account JID as :from."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "out" "chat"
       "Hi there" ts)
      (let ((entry (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
        (should (string= "out" (plist-get entry :direction)))
        (should (string= "me@example.com" (plist-get entry :from)))))))

(ert-deftest jabber-test-db-backlog-ordering ()
  "Backlog returns messages in reverse chronological order."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "First" (- now 200))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Second" (- now 100))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Third" now)
      (let ((entries (jabber-db-backlog "me@example.com" "friend@example.com")))
        (should (= 3 (length entries)))
        ;; DESC order: newest first
        (should (string= "Third" (plist-get (nth 0 entries) :body)))
        (should (string= "Second" (plist-get (nth 1 entries) :body)))
        (should (string= "First" (plist-get (nth 2 entries) :body)))))))

(ert-deftest jabber-test-db-backlog-respects-count ()
  "Backlog returns at most COUNT messages."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (dotimes (i 5)
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         (format "Message %d" i) (- now (* i 10))))
      (let ((entries (jabber-db-backlog
                      "me@example.com" "friend@example.com" 2)))
        (should (= 2 (length entries)))))))

(ert-deftest jabber-test-db-backlog-time-filter ()
  "Backlog respects the start-time parameter."
  (jabber-test-db-with-db
    (let* ((now (floor (float-time)))
           (old (- now 86400))    ; 1 day ago
           (very-old (- now 172800))) ; 2 days ago
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Recent" now)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Old" old)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Very old" very-old)
      ;; Only get messages from last 1.5 days
      (let* ((cutoff (- (float-time) (* 1.5 86400)))
             (entries (jabber-db-backlog
                       "me@example.com" "friend@example.com"
                       nil cutoff)))
        (should (= 2 (length entries)))
        (should (string= "Recent" (plist-get (nth 0 entries) :body)))
        (should (string= "Old" (plist-get (nth 1 entries) :body)))))))

(ert-deftest jabber-test-db-backlog-msg-type-filter ()
  "Backlog with msg-type filters by message type."
  (jabber-test-db-with-db
    (let ((jabber-backlog-days 3.0)
          (jabber-backlog-number 50)
          (now (floor (float-time))))
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "group msg" now "alice")
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "chat"
                               "private msg" (1+ now) "bob")
      ;; Without filter, both messages returned.
      (should (= 2 (length (jabber-db-backlog "me@x.com" "room@x.com"))))
      ;; With groupchat filter, only group message returned.
      (let ((entries (jabber-db-backlog "me@x.com" "room@x.com"
                                        nil nil nil "groupchat")))
        (should (= 1 (length entries)))
        (should (string= "group msg" (plist-get (car entries) :body))))
      ;; With chat filter, only private message returned.
      (let ((entries (jabber-db-backlog "me@x.com" "room@x.com"
                                        nil nil nil "chat")))
        (should (= 1 (length entries)))
        (should (string= "private msg" (plist-get (car entries) :body)))))))

;;; Group 4: FTS search

(ert-deftest jabber-test-db-fts-search ()
  "Full-text search finds messages by keyword."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Let's meet for coffee tomorrow" ts)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "The weather is nice today" (1+ ts))
      (let ((results (jabber-db-search "me@example.com" "coffee")))
        (should (= 1 (length results)))
        (should (string-match-p "coffee"
                                (plist-get (car results) :body)))))))

(ert-deftest jabber-test-db-fts-search-with-peer ()
  "FTS search scoped to a specific peer."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "alice@example.com" "in" "chat"
       "Hello from Alice" ts)
      (jabber-db-store-message
       "me@example.com" "bob@example.com" "in" "chat"
       "Hello from Bob" (1+ ts))
      ;; Search for "Hello" scoped to Alice
      (let ((results (jabber-db-search
                      "me@example.com" "Hello" "alice@example.com")))
        (should (= 1 (length results)))
        (should (string= "alice@example.com"
                         (plist-get (car results) :peer)))))))

(ert-deftest jabber-test-db-fts-search-no-match ()
  "FTS search returns nil when no messages match."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello world" ts)
      (should (null (jabber-db-search "me@example.com" "xyzzynonexistent"))))))

;;; Group 5: Dedup and last-timestamp

(ert-deftest jabber-test-db-dedup-stanza-id ()
  "Duplicate stanza_id keeps one row with body preserved and timestamp updated."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "First" ts nil "dup-id-123")
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Duplicate" (1+ ts) nil "dup-id-123")
      (let ((rows (sqlite-select (jabber-db-ensure-open)
                   "SELECT body, timestamp FROM message WHERE stanza_id = 'dup-id-123'")))
        (should (= 1 (length rows)))
        (should (string= "First" (caar rows)))
        (should (= (1+ ts) (cadar rows)))))))

(ert-deftest jabber-test-db-dedup-scoped-by-account ()
  "Same stanza_id from different accounts are stored as separate messages."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "alice@example.com" "friend@example.com" "in" "chat"
       "Alice got it" ts nil "shared-id-999")
      (jabber-db-store-message
       "bob@example.com" "friend@example.com" "in" "chat"
       "Bob got it" (1+ ts) nil "shared-id-999")
      ;; Both rows should exist
      (let ((rows (sqlite-select jabber-db--connection
                                 "SELECT account FROM message WHERE stanza_id='shared-id-999'")))
        (should (= 2 (length rows)))))))

(ert-deftest jabber-test-db-no-dedup-without-stanza-id ()
  "Messages without stanza_id are never deduped."
  (jabber-test-db-with-db
    (let ((ts (- (floor (float-time)) 10)))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Same body" ts)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Same body" (1+ ts))
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
        (should (= 2 (length rows)))))))

(ert-deftest jabber-test-db-last-timestamp ()
  "last-timestamp returns the latest timestamp for a peer."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Old" (- now 100))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "New" now)
      (should (= now (jabber-db-last-timestamp
                      "me@example.com" "friend@example.com"))))))

;;; Group 6: Account isolation

(ert-deftest jabber-test-db-account-isolation ()
  "Messages from different accounts are isolated."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "alice@example.com" "friend@example.com" "in" "chat"
       "Alice's message" ts)
      (jabber-db-store-message
       "bob@example.com" "friend@example.com" "in" "chat"
       "Bob's message" (1+ ts))
      (let ((alice-msgs (jabber-db-backlog "alice@example.com" "friend@example.com"))
            (bob-msgs (jabber-db-backlog "bob@example.com" "friend@example.com")))
        (should (= 1 (length alice-msgs)))
        (should (= 1 (length bob-msgs)))
        (should (string= "Alice's message" (plist-get (car alice-msgs) :body)))
        (should (string= "Bob's message" (plist-get (car bob-msgs) :body)))))))

(ert-deftest jabber-test-db-peer-isolation ()
  "Messages to different peers are isolated."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "alice@example.com" "out" "chat"
       "To Alice" ts)
      (jabber-db-store-message
       "me@example.com" "bob@example.com" "out" "chat"
       "To Bob" (1+ ts))
      (let ((alice-msgs (jabber-db-backlog "me@example.com" "alice@example.com"))
            (bob-msgs (jabber-db-backlog "me@example.com" "bob@example.com")))
        (should (= 1 (length alice-msgs)))
        (should (= 1 (length bob-msgs)))))))

;;; Group 7: Empty database

(ert-deftest jabber-test-db-empty-backlog ()
  "Backlog returns nil on an empty database."
  (jabber-test-db-with-db
    (should (null (jabber-db-backlog
                   "me@example.com" "friend@example.com")))))

(ert-deftest jabber-test-db-empty-search ()
  "Search returns nil on an empty database."
  (jabber-test-db-with-db
    (should (null (jabber-db-search "me@example.com" "anything")))))

(ert-deftest jabber-test-db-empty-last-timestamp ()
  "last-timestamp returns nil when no messages exist."
  (jabber-test-db-with-db
    (should (null (jabber-db-last-timestamp
                   "me@example.com" "friend@example.com")))))

;;; Group 8: Query pagination

(ert-deftest jabber-test-db-query-pagination ()
  "Query with limit and offset returns correct page."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (dotimes (i 5)
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         (format "Message %d" i) (+ now i)))
      ;; Page 1: first 2 messages
      (let ((page1 (jabber-db-query "me@example.com" "friend@example.com"
                                    now (+ now 10) 2 0)))
        (should (= 2 (length page1)))
        (should (string= "Message 0" (plist-get (car page1) :body))))
      ;; Page 2: next 2 messages
      (let ((page2 (jabber-db-query "me@example.com" "friend@example.com"
                                    now (+ now 10) 2 2)))
        (should (= 2 (length page2)))
        (should (string= "Message 2" (plist-get (car page2) :body)))))))

(ert-deftest jabber-test-db-query-time-range ()
  "Query with start-time and end-time filters correctly."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Before" (- now 100))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "During" now)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "After" (+ now 100))
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   (- now 10) (+ now 10))))
        (should (= 1 (length rows)))
        (should (string= "During" (plist-get (car rows) :body)))))))

;;; Group 9: Data persistence across close/reopen

(ert-deftest jabber-test-db-persistence ()
  "Data survives close and reopen."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Persistent message" ts)
      (jabber-db-close)
      (jabber-db-ensure-open)
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
        (should (= 1 (length rows)))
        (should (string= "Persistent message" (plist-get (car rows) :body)))))))

;;; Group 10: MUC backlog round-trip

(ert-deftest jabber-test-db-muc-backlog-sender-has-nickname ()
  "MUC backlog sender includes room JID and nickname as resource."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "in" "groupchat"
       "Hello everyone" ts "knighthk")
      (let ((entry (car (jabber-db-backlog
                         "me@example.com" "room@conference.example.com"))))
        (should (string= "room@conference.example.com/knighthk"
                         (plist-get entry :from)))
        (should (string= "in" (plist-get entry :direction)))
        (should (string= "groupchat" (plist-get entry :msg-type)))
        (should (string= "Hello everyone" (plist-get entry :body)))))))

(ert-deftest jabber-test-db-muc-backlog-multiple-senders ()
  "MUC backlog preserves distinct nicknames for different senders."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "in" "groupchat"
       "Hi from Alice" (- now 20) "alice")
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "in" "groupchat"
       "Hi from Bob" (- now 10) "bob")
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "out" "groupchat"
       "Hi from me" now)
      (let ((entries (jabber-db-backlog
                      "me@example.com" "room@conference.example.com")))
        (should (= 3 (length entries)))
        ;; DESC order: newest first
        (should (string= "me@example.com" (plist-get (nth 0 entries) :from)))
        (should (string= "room@conference.example.com/bob"
                         (plist-get (nth 1 entries) :from)))
        (should (string= "room@conference.example.com/alice"
                         (plist-get (nth 2 entries) :from)))))))

(ert-deftest jabber-test-db-muc-backlog-persistence ()
  "MUC messages survive close/reopen and retain nicknames."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "room@conference.example.com" "in" "groupchat"
       "Persistent MUC msg" ts "someuser")
      (jabber-db-close)
      (jabber-db-ensure-open)
      (let ((entry (car (jabber-db-backlog
                         "me@example.com" "room@conference.example.com"))))
        (should (string= "room@conference.example.com/someuser"
                         (plist-get entry :from)))
        (should (string= "Persistent MUC msg" (plist-get entry :body)))))))

(ert-deftest jabber-test-db-nil-path-disables-storage ()
  "Setting jabber-db-path to nil disables all DB operations."
  (let ((jabber-db-path nil)
        (jabber-db--connection nil))
    (should (null (jabber-db-ensure-open)))
    (should (null (jabber-db-store-message
                   "me@example.com" "friend@example.com" "in" "chat"
                   "Hello" (floor (float-time)))))
    (should (null (jabber-db-backlog "me@example.com" "friend@example.com")))))

;;; Group 11: Import from history

(ert-deftest jabber-test-db-import-history ()
  "Importing from flat-file history populates the database."
  (jabber-test-db-with-db
    (let* ((jabber-use-global-history nil)
           (jabber-history-dir
            (expand-file-name "history"
                              (file-name-directory jabber-db-path)))
           (history-file
            (expand-file-name "friend@example.com" jabber-history-dir)))
      ;; Create a fake history file
      (make-directory jabber-history-dir)
      (with-temp-file history-file
        (insert "[\"2024-01-15T10:00:00Z\" \"in\" \"friend@example.com\" \"me\" \"Hi there\"]\n")
        (insert "[\"2024-01-15T10:01:00Z\" \"out\" \"me\" \"friend@example.com\" \"Hey!\"]\n"))
      (jabber-db-import-history "me@example.com")
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (floor (float-time)))))
        (should (= 2 (length rows)))
        (should (string= "Hi there" (plist-get (car rows) :body)))
        (should (string= "in" (plist-get (car rows) :direction)))
        (should (string= "Hey!" (plist-get (cadr rows) :body)))
        (should (string= "out" (plist-get (cadr rows) :direction)))))))

(ert-deftest jabber-test-db-import-history-strips-resource ()
  "Imported messages with resource JIDs are stored under the bare JID."
  (jabber-test-db-with-db
    (let* ((jabber-use-global-history nil)
           (jabber-history-dir
            (expand-file-name "history"
                              (file-name-directory jabber-db-path)))
           (history-file
            (expand-file-name "friend@example.com" jabber-history-dir)))
      (make-directory jabber-history-dir)
      (with-temp-file history-file
        (insert "[\"2024-01-15T10:00:00Z\" \"in\" \"friend@example.com/Work PC\" \"me\" \"From work\"]\n")
        (insert "[\"2024-01-15T10:01:00Z\" \"out\" \"me\" \"friend@example.com/Work PC\" \"Reply\"]\n"))
      (jabber-db-import-history "me@example.com")
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (floor (float-time)))))
        (should (= 2 (length rows)))
        (should (string= "From work" (plist-get (car rows) :body)))
        (should (string= "Reply" (plist-get (cadr rows) :body)))))))

(ert-deftest jabber-test-db-import-global-history ()
  "Importing from a global history file works."
  (jabber-test-db-with-db
    (let* ((jabber-use-global-history t)
           (jabber-global-history-filename
            (expand-file-name "global-history"
                              (file-name-directory jabber-db-path))))
      (with-temp-file jabber-global-history-filename
        (insert "[\"2024-06-01T12:00:00Z\" \"in\" \"alice@example.com\" \"me\" \"Global msg\"]\n"))
      (jabber-db-import-history "me@example.com")
      (let ((rows (jabber-db-query "me@example.com" "alice@example.com"
                                   0 (floor (float-time)))))
        (should (= 1 (length rows)))
        (should (string= "Global msg" (plist-get (car rows) :body)))))))

;;; Group 12: jabber-db--row-to-plist

(ert-deftest jabber-test-db-row-to-plist-incoming-chat ()
  "Incoming chat message builds correct plist."
  ;;  id  account               peer                  dir  body     ts          resource  type
  (let* ((row '(1 "me@example.com" "alice@example.com" "in"
                "Hello!" 1700000000 "mobile" "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "alice@example.com/mobile" (plist-get plist :from)))
    (should (string= "Hello!" (plist-get plist :body)))
    (should (string= "in" (plist-get plist :direction)))
    (should (string= "chat" (plist-get plist :msg-type)))
    (should (plist-get plist :delayed))
    (should (equal (seconds-to-time 1700000000) (plist-get plist :timestamp)))))

(ert-deftest jabber-test-db-row-to-plist-incoming-no-resource ()
  "Incoming message without resource uses bare JID as :from."
  (let* ((row '(2 "me@example.com" "alice@example.com" "in"
                "Hi" 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "alice@example.com" (plist-get plist :from)))))

(ert-deftest jabber-test-db-row-to-plist-outgoing ()
  "Outgoing message uses account JID as :from."
  (let* ((row '(3 "me@example.com" "alice@example.com" "out"
                "Bye!" 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "me@example.com" (plist-get plist :from)))))

(ert-deftest jabber-test-db-row-to-plist-groupchat ()
  "Groupchat message has msg-type groupchat."
  (let* ((row '(4 "me@example.com" "room@conf.example.com" "in"
                "Hello room" 1700000000 "Alice" "groupchat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "groupchat" (plist-get plist :msg-type)))
    (should (string= "room@conf.example.com/Alice" (plist-get plist :from)))))

(ert-deftest jabber-test-db-row-to-plist-nil-body ()
  "Nil body is converted to empty string."
  (let* ((row '(5 "me@example.com" "alice@example.com" "in"
                nil 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "" (plist-get plist :body)))))

(ert-deftest jabber-test-db-row-to-plist-encrypted-flag ()
  "Encrypted flag is correctly converted to boolean."
  ;;  id  account               peer                  dir  body     ts          resource type encrypted
  (let* ((row '(6 "me@example.com" "alice@example.com" "in"
                "Secret" 1700000000 nil "chat" 1))
         (plist (jabber-db--row-to-plist row)))
    (should (eq t (plist-get plist :encrypted)))))

(ert-deftest jabber-test-db-row-to-plist-not-encrypted ()
  "Zero encrypted flag yields nil."
  (let* ((row '(7 "me@example.com" "alice@example.com" "in"
                "Plain" 1700000000 nil "chat" 0))
         (plist (jabber-db--row-to-plist row)))
    (should-not (plist-get plist :encrypted))))

;;; Group 13: Chat settings (encryption persistence)

(ert-deftest jabber-test-db-chat-settings-table-exists ()
  "The chat_settings table is created by the schema."
  (jabber-test-db-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "chat_settings" tables)))))

(ert-deftest jabber-test-db-set-and-get-encryption-omemo ()
  "Storing OMEMO encryption and reading it back returns the symbol."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-test-db-set-and-get-encryption-plaintext ()
  "Storing plaintext encryption and reading it back returns the symbol."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-test-db-get-encryption-default-returns-nil ()
  "Storing `default' encryption returns nil from get."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'default)
    (should (null (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-test-db-get-encryption-missing-returns-nil ()
  "Querying encryption for an unknown peer returns nil."
  (jabber-test-db-with-db
    (should (null (jabber-db-get-chat-encryption "me@example.com" "nobody@example.com")))))

(ert-deftest jabber-test-db-set-encryption-overwrites ()
  "Setting encryption twice overwrites the previous value."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-test-db-chat-settings-account-isolation ()
  "Encryption settings are isolated per account."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "alice@example.com" "bob@example.com" 'omemo)
    (jabber-db-set-chat-encryption "carol@example.com" "bob@example.com" 'plaintext)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "alice@example.com" "bob@example.com")))
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "carol@example.com" "bob@example.com")))))

(ert-deftest jabber-test-db-chat-settings-peer-isolation ()
  "Encryption settings are isolated per peer."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-set-chat-encryption "me@example.com" "bob@example.com" 'plaintext)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "bob@example.com")))))

(ert-deftest jabber-test-db-chat-settings-persist-across-reopen ()
  "Encryption settings survive close and reopen."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-close)
    (jabber-db-ensure-open)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-test-db-chat-settings-muc-peer ()
  "Encryption settings work with MUC room JIDs."
  (jabber-test-db-with-db
    (jabber-db-set-chat-encryption
     "me@example.com" "room@conference.example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption
                 "me@example.com" "room@conference.example.com")))))

(ert-deftest jabber-test-db-chat-settings-nil-path ()
  "Chat settings no-op when jabber-db-path is nil."
  (let ((jabber-db-path nil)
        (jabber-db--connection nil))
    (should (null (jabber-db-set-chat-encryption
                   "me@example.com" "alice@example.com" 'omemo)))
    (should (null (jabber-db-get-chat-encryption
                   "me@example.com" "alice@example.com")))))

;;; Group 14: Buffer encryption integration
;;
;; These tests verify that jabber-chat-mode-setup loads encryption
;; from the DB when jabber-chatting-with / jabber-group is set
;; BEFORE the setup call (the bug was calling setup before setting
;; the peer variable, so the DB lookup always returned nil).

(require 'jabber-chatbuffer)
(require 'fsm)

(defun jabber-test-db--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT (user@server)."
  (let ((jc (gensym "jabber-test-db-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

(defmacro jabber-test-db-with-chat-buffer (account peer &rest body)
  "Run BODY in a temp chat buffer with fake connection for ACCOUNT talking to PEER.
Sets up jabber-chatting-with before jabber-chat-mode-setup, mimicking
the corrected jabber-chat-create-buffer order."
  (declare (indent 2) (debug t))
  `(jabber-test-db-with-db
     (let* ((jc (jabber-test-db--make-fake-jc ,account))
            (buf (generate-new-buffer " *test-chat*"))
            (jabber-chat-default-encryption 'omemo)
            (jabber-chatting-with nil))
       (unwind-protect
           (with-current-buffer buf
             (jabber-chat-mode)
             (set (make-local-variable 'jabber-chatting-with) ,peer)
             (jabber-chat-mode-setup jc #'ignore)
             ,@body)
         (kill-buffer buf)))))

(defmacro jabber-test-db-with-muc-buffer (account group &rest body)
  "Run BODY in a temp MUC buffer with fake connection for ACCOUNT in GROUP.
Sets up jabber-group before jabber-chat-mode-setup, mimicking
the corrected jabber-muc-create-buffer order."
  (declare (indent 2) (debug t))
  `(jabber-test-db-with-db
     (let* ((jc (jabber-test-db--make-fake-jc ,account))
            (buf (generate-new-buffer " *test-muc*"))
            (jabber-chat-default-encryption 'omemo)
            (jabber-chatting-with nil))
       (unwind-protect
           (with-current-buffer buf
             (jabber-chat-mode)
             (set (make-local-variable 'jabber-group) ,group)
             (jabber-chat-mode-setup jc #'ignore)
             ,@body)
         (kill-buffer buf)))))

(ert-deftest jabber-test-db-chat-buffer-loads-encryption-from-db ()
  "1:1 chat buffer loads saved encryption from DB on setup."
  (jabber-test-db-with-chat-buffer "me@example.com" "alice@example.com"
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    ;; Reset and re-run setup to simulate fresh buffer
    (setq jabber-chat-encryption nil)
    (jabber-chat-mode-setup jc #'ignore)
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-test-db-chat-buffer-falls-back-to-default ()
  "1:1 chat buffer uses default when no DB setting exists."
  (jabber-test-db-with-chat-buffer "me@example.com" "bob@example.com"
    (should (eq 'omemo jabber-chat-encryption))))

(ert-deftest jabber-test-db-chat-buffer-default-plaintext ()
  "1:1 chat buffer respects jabber-chat-default-encryption when set to plaintext."
  (jabber-test-db-with-db
    (let* ((jc (jabber-test-db--make-fake-jc "me@example.com"))
           (buf (generate-new-buffer " *test-chat-plain*"))
           (jabber-chat-default-encryption 'plaintext)
           (jabber-chatting-with nil))
      (unwind-protect
          (with-current-buffer buf
            (jabber-chat-mode)
            (set (make-local-variable 'jabber-chatting-with) "carol@example.com")
            (jabber-chat-mode-setup jc #'ignore)
            (should (eq 'plaintext jabber-chat-encryption)))
        (kill-buffer buf)))))

(ert-deftest jabber-test-db-chat-buffer-db-overrides-default ()
  "DB setting overrides jabber-chat-default-encryption."
  (jabber-test-db-with-chat-buffer "me@example.com" "alice@example.com"
    ;; Default is omemo, but DB says plaintext
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (setq jabber-chat-encryption nil)
    (jabber-chat-mode-setup jc #'ignore)
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-test-db-muc-buffer-loads-encryption-from-db ()
  "MUC buffer loads saved encryption from DB on setup."
  (jabber-test-db-with-db
    (let* ((jc (jabber-test-db--make-fake-jc "me@example.com"))
           (buf (generate-new-buffer " *test-muc-load*"))
           (jabber-chat-default-encryption 'omemo)
           (jabber-chatting-with nil))
      ;; Store plaintext BEFORE creating the buffer
      (jabber-db-set-chat-encryption "me@example.com" "room@conference.example.com" 'plaintext)
      (unwind-protect
          (with-current-buffer buf
            (jabber-chat-mode)
            (set (make-local-variable 'jabber-group) "room@conference.example.com")
            (jabber-chat-mode-setup jc #'ignore)
            (should (eq 'plaintext jabber-chat-encryption)))
        (kill-buffer buf)))))

(ert-deftest jabber-test-db-muc-buffer-falls-back-to-plaintext ()
  "MUC buffer defaults to plaintext when no DB setting exists."
  (jabber-test-db-with-muc-buffer "me@example.com" "room@conference.example.com"
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-test-db-chat-buffer-without-peer-falls-back ()
  "Buffer without jabber-chatting-with or jabber-group falls back to default."
  (jabber-test-db-with-db
    (let* ((jc (jabber-test-db--make-fake-jc "me@example.com"))
           (buf (generate-new-buffer " *test-no-peer*"))
           (jabber-chat-default-encryption 'omemo)
           (jabber-chatting-with nil))
      (unwind-protect
          (with-current-buffer buf
            (jabber-chat-mode)
            ;; Deliberately not setting jabber-chatting-with or jabber-group
            (jabber-chat-mode-setup jc #'ignore)
            (should (eq 'omemo jabber-chat-encryption)))
        (kill-buffer buf)))))

(ert-deftest jabber-test-db-toggle-save-roundtrip ()
  "Toggling encryption saves to DB and reloading a fresh buffer picks it up."
  (jabber-test-db-with-db
    (let* ((jc (jabber-test-db--make-fake-jc "me@example.com"))
           (jabber-chat-default-encryption 'omemo)
           (jabber-chatting-with nil))
      ;; First buffer: toggle to plaintext
      (let ((buf1 (generate-new-buffer " *test-toggle-1*")))
        (unwind-protect
            (with-current-buffer buf1
              (jabber-chat-mode)
              (set (make-local-variable 'jabber-chatting-with) "alice@example.com")
              (jabber-chat-mode-setup jc #'ignore)
              (should (eq 'omemo jabber-chat-encryption))
              (jabber-chat-encryption-set-plaintext)
              (should (eq 'plaintext jabber-chat-encryption)))
          (kill-buffer buf1)))
      ;; Second buffer: should load plaintext from DB
      (let ((buf2 (generate-new-buffer " *test-toggle-2*")))
        (unwind-protect
            (with-current-buffer buf2
              (jabber-chat-mode)
              (set (make-local-variable 'jabber-chatting-with) "alice@example.com")
              (jabber-chat-mode-setup jc #'ignore)
              (should (eq 'plaintext jabber-chat-encryption)))
          (kill-buffer buf2))))))

(defvar jabber-chat-header-line-format)  ; jabber-chat.el

(ert-deftest jabber-test-db-redisplay-reloads-encryption ()
  "jabber-chat-redisplay reloads encryption from DB."
  (jabber-test-db-with-db
    (let* ((jc (jabber-test-db--make-fake-jc "me@example.com"))
           (buf (generate-new-buffer " *test-redisplay*"))
           (jabber-chat-default-encryption 'omemo)
           (jabber-chatting-with nil)
           (jabber-chat-header-line-format '("test")))
      (unwind-protect
          (with-current-buffer buf
            (jabber-chat-mode)
            (set (make-local-variable 'jabber-chatting-with) "alice@example.com")
            (jabber-chat-mode-setup jc #'ignore)
            (should (eq 'omemo jabber-chat-encryption))
            ;; Simulate external DB change
            (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
            ;; Redisplay should pick up the DB change
            (jabber-chat-redisplay)
            (should (eq 'plaintext jabber-chat-encryption)))
        (kill-buffer buf)))))

;;; Group 15: Receipt columns and updates

(ert-deftest jabber-test-db-receipt-columns ()
  "Message table has delivered_at and displayed_at columns."
  (jabber-test-db-with-db
    (sqlite-execute jabber-db--connection
                    "INSERT INTO message (account,peer,direction,timestamp)
                     VALUES ('a','b','out',1)")
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at, displayed_at FROM message"))))
      (should (equal row '(nil nil))))))

(ert-deftest jabber-test-db-update-receipt-delivered ()
  "Update delivered_at for an outgoing message by stanza_id."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-001")
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              "msg-001" "delivered_at" 1001)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message WHERE stanza_id='msg-001'"))))
      (should (equal row '(1001))))))

(ert-deftest jabber-test-db-update-receipt-displayed ()
  "Update displayed_at for an outgoing message by stanza_id."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-002")
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              "msg-002" "displayed_at" 1002)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT displayed_at FROM message WHERE stanza_id='msg-002'"))))
      (should (equal row '(1002))))))

(ert-deftest jabber-test-db-update-receipt-no-overwrite ()
  "Duplicate receipt does not overwrite earlier timestamp."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-003")
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              "msg-003" "delivered_at" 1001)
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              "msg-003" "delivered_at" 9999)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message WHERE stanza_id='msg-003'"))))
      (should (equal row '(1001))))))

(ert-deftest jabber-test-db-update-receipt-nil-stanza-id ()
  "Update with nil stanza_id is a no-op."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000)
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              nil "delivered_at" 1001)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message LIMIT 1"))))
      (should (equal row '(nil))))))

(ert-deftest jabber-test-db-update-receipt-scoped-by-peer ()
  "Receipt update only affects matching account+peer, not other conversations."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "alice@example.com"
                             "out" "chat" "hi alice" 1000 nil "msg-same-id")
    (jabber-db-store-message "me@example.com" "bob@example.com"
                             "out" "chat" "hi bob" 1001 nil "msg-same-id")
    (jabber-db-update-receipt "me@example.com" "alice@example.com"
                              "msg-same-id" "delivered_at" 2000)
    (let ((alice (caar (sqlite-select jabber-db--connection
                        "SELECT delivered_at FROM message WHERE peer='alice@example.com'")))
          (bob (caar (sqlite-select jabber-db--connection
                      "SELECT delivered_at FROM message WHERE peer='bob@example.com'"))))
      (should (equal alice 2000))
      (should (null bob)))))

(ert-deftest jabber-test-db-update-receipt-only-outgoing ()
  "Receipt update only affects outgoing messages, not incoming."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "in" "chat" "incoming" 1000 nil "msg-in")
    (jabber-db-update-receipt "me@example.com" "them@example.com"
                              "msg-in" "delivered_at" 2000)
    (let ((row (caar (sqlite-select jabber-db--connection
                      "SELECT delivered_at FROM message WHERE stanza_id='msg-in'"))))
      (should (null row)))))

;;; Group 16: Delete peer messages

(ert-deftest jabber-test-db-delete-peer-messages ()
  "Deleting peer messages removes all rows for that account+peer."
  (jabber-test-db-with-db
    (let ((ts (- (floor (float-time)) 10)))
      (jabber-db-store-message
       "me@example.com" "alice@example.com" "in" "chat" "Hello" ts)
      (jabber-db-store-message
       "me@example.com" "alice@example.com" "out" "chat" "Hi" (1+ ts))
      (jabber-db-store-message
       "me@example.com" "bob@example.com" "in" "chat" "Hey" (+ ts 2))
      ;; Delete alice's messages
      (jabber-db-delete-peer-messages "me@example.com" "alice@example.com")
      ;; Alice gone
      (should (null (jabber-db-query "me@example.com" "alice@example.com")))
      ;; Bob untouched
      (let ((rows (jabber-db-query "me@example.com" "bob@example.com")))
        (should (= 1 (length rows)))
        (should (string= "Hey" (plist-get (car rows) :body)))))))

(ert-deftest jabber-test-db-delete-peer-messages-empty ()
  "Deleting from a nonexistent peer is a no-op."
  (jabber-test-db-with-db
    (jabber-db-delete-peer-messages "me@example.com" "nobody@example.com")
    ;; No error, no rows affected
    (should t)))

;;; Group 17: Message retraction

(ert-deftest jabber-test-db-retract-with-reason ()
  "jabber-db-retract-message persists moderator and reason; backlog returns both."
  (skip-unless (fboundp 'sqlite-open))
  (let ((jabber-backlog-days 3.0)
        (jabber-backlog-number 10)
        (now (floor (float-time))))
    (jabber-test-db-with-db
      (jabber-db-store-message "me@x.com" "room@x.com"
                               "in" "groupchat" "offensive" now
                               nil nil "srv-retract-1")
      (jabber-db-retract-message "srv-retract-1" "room@x.com/mod" "spam")
      (let* ((entries (jabber-db-backlog "me@x.com" "room@x.com"))
             (entry (car entries)))
        (should entry)
        (should (plist-get entry :retracted))
        (should (equal "room@x.com/mod" (plist-get entry :retracted-by)))
        (should (equal "spam" (plist-get entry :retraction-reason)))))))

(ert-deftest jabber-test-db-retract-without-reason ()
  "jabber-db-retract-message with no reason leaves :retraction-reason nil."
  (skip-unless (fboundp 'sqlite-open))
  (let ((jabber-backlog-days 3.0)
        (jabber-backlog-number 10)
        (now (floor (float-time))))
    (jabber-test-db-with-db
      (jabber-db-store-message "me@x.com" "room@x.com"
                               "in" "groupchat" "msg" now
                               nil nil "srv-retract-2")
      (jabber-db-retract-message "srv-retract-2" "room@x.com/mod")
      (let* ((entries (jabber-db-backlog "me@x.com" "room@x.com"))
             (entry (car entries)))
        (should (plist-get entry :retracted))
        (should-not (plist-get entry :retraction-reason))))))

;;; Group: Failed-decrypt replacement

(ert-deftest jabber-test-db-store-replaces-failed-decrypt-by-stanza-id ()
  "Re-storing a message with real text replaces a decrypt-failure placeholder."
  (jabber-test-db-with-db
    ;; Store with failed-decrypt body
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" "stanza-1" "srv-1")
    ;; Re-store same stanza-id with decrypted body
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello there" 1700000000
                             "res" "stanza-1" "srv-1"
                             nil nil t)
    ;; Should have exactly one row with the decrypted body
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE stanza_id = ?"
                               '("stanza-1"))))
      (should (= 1 (length rows)))
      (should (string= "hello there" (caar rows))))))

(ert-deftest jabber-test-db-store-replaces-failed-decrypt-by-server-id ()
  "Re-storing by server-id replaces a decrypt-failure placeholder."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" nil "srv-2")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "decrypted text" 1700000000
                             "res" nil "srv-2"
                             nil nil t)
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE server_id = ?"
                               '("srv-2"))))
      (should (= 1 (length rows)))
      (should (string= "decrypted text" (caar rows))))))

(ert-deftest jabber-test-db-store-no-replace-when-still-undecryptable ()
  "Re-storing with another failed-decrypt body does not update."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" "stanza-3" "srv-3")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" "stanza-3" "srv-3")
    ;; Still one row, body unchanged
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE stanza_id = ?"
                               '("stanza-3"))))
      (should (= 1 (length rows)))
      (should (string= "[OMEMO: could not decrypt]" (caar rows))))))

(ert-deftest jabber-test-db-store-no-replace-when-already-decrypted ()
  "Re-storing does not overwrite an already-decrypted message."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "original text" 1700000000
                             "res" "stanza-4" "srv-4")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "different text" 1700000000
                             "res" "stanza-4" "srv-4")
    ;; Still one row, original body preserved
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE stanza_id = ?"
                               '("stanza-4"))))
      (should (= 1 (length rows)))
      (should (string= "original text" (caar rows))))))

(ert-deftest jabber-test-db-store-normalizes-timestamp-on-dedup ()
  "Re-storing a duplicate updates the timestamp to the server's value."
  (jabber-test-db-with-db
    ;; Store with local timestamp
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello" 1700000099
                             "res" "stanza-5" "srv-5")
    ;; Re-store same message with server's authoritative timestamp
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello" 1700000100
                             "res" "stanza-5" "srv-5")
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT timestamp FROM message WHERE stanza_id = ?"
                               '("stanza-5"))))
      (should (= 1 (length rows)))
      (should (= 1700000100 (caar rows))))))

(ert-deftest jabber-test-db-store-normalizes-timestamp-and-replaces-decrypt ()
  "Failed-decrypt replacement also normalizes the timestamp."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000099
                             "res" "stanza-6" "srv-6")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "decrypted" 1700000100
                             "res" "stanza-6" "srv-6"
                             nil nil t)
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body, timestamp FROM message WHERE stanza_id = ?"
                               '("stanza-6"))))
      (should (= 1 (length rows)))
      (should (string= "decrypted" (caar rows)))
      (should (= 1700000100 (cadar rows))))))

;;; Group: Schema v2 migration and constraints

(defconst jabber-test-db--v1-ddl
  '("CREATE TABLE IF NOT EXISTS message (
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
  displayed_at INTEGER,
  retracted_by TEXT,
  retraction_reason TEXT,
  edited       INTEGER DEFAULT 0)"
    "CREATE INDEX IF NOT EXISTS idx_msg_peer_ts
  ON message(account, peer, timestamp)"
    "CREATE INDEX IF NOT EXISTS idx_msg_stanza_id
  ON message(account, stanza_id) WHERE stanza_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_server_id
  ON message(account, server_id) WHERE server_id IS NOT NULL")
  "V1 schema DDL for migration tests.")

(defmacro jabber-test-db-with-v1-db (&rest body)
  "Run BODY with a v1 database (has raw_xml, no occupant_id)."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-db--dir (make-temp-file "jabber-db-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-test-db--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (let ((db (sqlite-open jabber-db-path)))
           (dolist (ddl jabber-test-db--v1-ddl)
             (sqlite-execute db ddl))
           (sqlite-execute db "PRAGMA user_version=1")
           (sqlite-close db)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-test-db--dir)
         (delete-directory jabber-test-db--dir t)))))

(ert-deftest jabber-test-db-v1-to-v2-migration ()
  "Migrating from v1 adds occupant_id, drops raw_xml, and runs through v3."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-v1-db
    ;; Insert a v1 row with raw_xml
    (let ((db (sqlite-open jabber-db-path)))
      (sqlite-execute db "\
INSERT INTO message (account, peer, direction, type, body, timestamp, raw_xml)
VALUES ('me@x.com', 'friend@x.com', 'in', 'chat', 'hello', 1000, '<msg/>')")
      (sqlite-close db))
    ;; Open via jabber-db which triggers migration
    (jabber-db-ensure-open)
    (let ((version (caar (sqlite-select jabber-db--connection "PRAGMA user_version"))))
      (should (= jabber-db--schema-version version)))
    ;; occupant_id column exists (NULL for old rows)
    (let ((rows (sqlite-select jabber-db--connection
                               "SELECT occupant_id FROM message LIMIT 1")))
      (should (= 1 (length rows)))
      (should (null (caar rows))))
    ;; raw_xml column is gone
    (should-error
     (sqlite-select jabber-db--connection
                    "SELECT raw_xml FROM message LIMIT 1"))))

(ert-deftest jabber-test-db-v1-migration-preserves-data ()
  "Migrating from v1 preserves existing message data."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-v1-db
    (let ((db (sqlite-open jabber-db-path)))
      (sqlite-execute db "\
INSERT INTO message (account, peer, direction, type, body, timestamp, resource)
VALUES ('me@x.com', 'friend@x.com', 'in', 'chat', 'preserved', 2000, 'laptop')")
      (sqlite-close db))
    (jabber-db-ensure-open)
    (let ((row (car (sqlite-select jabber-db--connection
                                   "SELECT body, resource FROM message LIMIT 1"))))
      (should (string= "preserved" (nth 0 row)))
      (should (string= "laptop" (nth 1 row))))))

(ert-deftest jabber-test-db-check-direction-on-fresh-db ()
  "CHECK constraint rejects invalid direction on fresh databases."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-db
    (should-error
     (sqlite-execute jabber-db--connection
                     "INSERT INTO message (account, peer, direction, type, body, timestamp)
                      VALUES ('a', 'b', 'bad', 'chat', 'x', 1000)"))))

(ert-deftest jabber-test-db-check-type-on-fresh-db ()
  "CHECK constraint rejects invalid message type on fresh databases."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-db
    (should-error
     (sqlite-execute jabber-db--connection
                     "INSERT INTO message (account, peer, direction, type, body, timestamp)
                      VALUES ('a', 'b', 'in', 'invalid', 'x', 1000)"))))

(ert-deftest jabber-test-db-occupant-id-round-trip ()
  "Storing and retrieving occupant_id works end-to-end."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "hello" (floor (float-time))
                             "nick" "sid-1" nil "occ-abc-123")
    (let* ((rows (jabber-db-query "me@x.com" "room@x.com"))
           (row (car rows)))
      (should row)
      (should (string= "occ-abc-123" (plist-get row :occupant-id))))))

(ert-deftest jabber-test-db-occupant-id-nil-when-absent ()
  "occupant_id is nil when not provided."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello" (floor (float-time)))
    (let* ((rows (jabber-db-query "me@x.com" "friend@x.com"))
           (row (car rows)))
      (should row)
      (should (null (plist-get row :occupant-id))))))

;;; Group: server-ids-by-occupant-id

(ert-deftest jabber-test-db-server-ids-by-occupant-id ()
  "Returns correct server-ids for an occupant-id."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg1" now "nick" nil "srv-a" "occ-1")
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg2" (1+ now) "nick" nil "srv-b" "occ-1")
      (let ((ids (jabber-db-server-ids-by-occupant-id
                  "me@x.com" "room@x.com" "occ-1")))
        (should (= 2 (length ids)))
        (should (member "srv-a" ids))
        (should (member "srv-b" ids))))))

(ert-deftest jabber-test-db-server-ids-by-occupant-id-excludes-retracted ()
  "Already-retracted messages are excluded."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg1" now "nick" nil "srv-c" "occ-2")
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg2" (1+ now) "nick" nil "srv-d" "occ-2")
      (jabber-db-retract-message "srv-c" "room@x.com/mod" "spam")
      (let ((ids (jabber-db-server-ids-by-occupant-id
                  "me@x.com" "room@x.com" "occ-2")))
        (should (= 1 (length ids)))
        (should (string= "srv-d" (car ids)))))))

(ert-deftest jabber-test-db-server-ids-by-occupant-id-excludes-nil-server-id ()
  "Messages without server-id are excluded."
  (jabber-test-db-with-db
    (let ((now (floor (float-time))))
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg1" now "nick" nil "srv-e" "occ-3")
      ;; Message with occupant-id but no server-id
      (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                               "msg2" (1+ now) "nick" nil nil "occ-3")
      (let ((ids (jabber-db-server-ids-by-occupant-id
                  "me@x.com" "room@x.com" "occ-3")))
        (should (= 1 (length ids)))
        (should (string= "srv-e" (car ids)))))))

(ert-deftest jabber-test-db-server-ids-by-occupant-id-unknown ()
  "Returns nil for an unknown occupant-id."
  (jabber-test-db-with-db
    (should (null (jabber-db-server-ids-by-occupant-id
                   "me@x.com" "room@x.com" "nonexistent")))))

(ert-deftest jabber-test-db-occupant-id-by-server-id ()
  "Returns occupant-id for a known server-id."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "hello" (floor (float-time))
                             "nick" nil "srv-occ-1" "occ-lookup")
    (should (string= "occ-lookup"
                      (jabber-db-occupant-id-by-server-id "srv-occ-1")))))

(ert-deftest jabber-test-db-occupant-id-by-server-id-nil ()
  "Returns nil for unknown server-id."
  (jabber-test-db-with-db
    (should (null (jabber-db-occupant-id-by-server-id "nonexistent")))))

(ert-deftest jabber-test-db-store-preserves-retraction-on-dedup ()
  "Re-storing a retracted message does not clear retracted_by."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "spam" 1000 "nick" nil "srv-pres-1")
    (jabber-db-retract-message "srv-pres-1" "room@x.com/mod" "spam")
    ;; MAM re-stores the same message
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "spam" 1000 "nick" nil "srv-pres-1")
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT retracted_by FROM message WHERE server_id = ?"
                     '("srv-pres-1")))))
      (should (string= "room@x.com/mod" (car row))))))

;;; Group: message_oob child table

(ert-deftest jabber-test-db-oob-table-exists ()
  "The message_oob table and index exist in fresh databases."
  (jabber-test-db-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "message_oob" tables)))
    (let ((indexes (mapcar #'car
                           (sqlite-select jabber-db--connection
                             "SELECT name FROM sqlite_master WHERE type='index'"))))
      (should (member "idx_oob_message_id" indexes)))))

(ert-deftest jabber-test-db-store-single-oob ()
  "Storing a message with one OOB entry creates a child row."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Check this out" ts nil "id-oob-1" nil nil
       '(("https://example.com/file.pdf" . "A PDF")))
      (let ((rows (sqlite-select jabber-db--connection
                   "SELECT url, desc FROM message_oob")))
        (should (= 1 (length rows)))
        (should (string= "https://example.com/file.pdf" (caar rows)))
        (should (string= "A PDF" (cadar rows)))))))

(ert-deftest jabber-test-db-store-multiple-oob ()
  "Storing a message with multiple OOB entries creates multiple child rows."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Attachments" ts nil "id-oob-multi" nil nil
       '(("https://example.com/a.jpg" . "Photo A")
         ("https://example.com/b.pdf" . nil)
         ("https://example.com/c.mp3" . "Audio")))
      (let ((rows (sqlite-select jabber-db--connection
                   "SELECT url, desc FROM message_oob ORDER BY id")))
        (should (= 3 (length rows)))
        (should (string= "https://example.com/a.jpg" (car (nth 0 rows))))
        (should (string= "Photo A" (cadr (nth 0 rows))))
        (should (string= "https://example.com/b.pdf" (car (nth 1 rows))))
        (should (null (cadr (nth 1 rows))))
        (should (string= "https://example.com/c.mp3" (car (nth 2 rows))))))))

(ert-deftest jabber-test-db-store-nil-oob ()
  "Storing a message with nil OOB creates no child rows."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Plain text" ts nil "id-no-oob")
      (let ((rows (sqlite-select jabber-db--connection
                   "SELECT count(*) FROM message_oob")))
        (should (= 0 (caar rows)))))))

(ert-deftest jabber-test-db-backlog-oob-entries ()
  "Backlog returns :oob-entries with correct data."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "File" ts nil "id-back-oob" nil nil
       '(("https://example.com/a.pdf" . "Doc A")
         ("https://example.com/b.png" . nil)))
      (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
             (entry (car entries))
             (oob (plist-get entry :oob-entries)))
        (should (= 2 (length oob)))
        (should (string= "https://example.com/a.pdf" (caar oob)))
        (should (string= "Doc A" (cdar oob)))
        (should (string= "https://example.com/b.png" (car (cadr oob))))
        (should (null (cdr (cadr oob))))))))

(ert-deftest jabber-test-db-backlog-oob-compat ()
  "Backlog sets :oob-url and :oob-desc from first entry for compat."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "File" ts nil "id-compat-oob" nil nil
       '(("https://example.com/first.pdf" . "First")
         ("https://example.com/second.pdf" . "Second")))
      (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
             (entry (car entries)))
        (should (string= "https://example.com/first.pdf"
                         (plist-get entry :oob-url)))
        (should (string= "First" (plist-get entry :oob-desc)))))))

(ert-deftest jabber-test-db-backlog-no-oob ()
  "Backlog returns nil :oob-entries for messages without OOB."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Plain" ts nil "id-nooob")
      (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
             (entry (car entries)))
        (should (null (plist-get entry :oob-entries)))
        (should (null (plist-get entry :oob-url)))))))

(ert-deftest jabber-test-db-oob-cascade-delete ()
  "Deleting a message cascades to message_oob rows."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "peer@example.com" "in" "chat"
       "File" ts nil "id-cascade" nil nil
       '(("https://example.com/x.pdf" . "X")))
      (jabber-db-delete-peer-messages "me@example.com" "peer@example.com")
      (should (= 0 (caar (sqlite-select jabber-db--connection
                           "SELECT count(*) FROM message_oob")))))))

(ert-deftest jabber-test-db-migration-v2-to-v4 ()
  "Migration v2->v4 applies the full chain: OOB child table, caps cache."
  (let* ((jabber-test-db--dir (make-temp-file "jabber-db-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" jabber-test-db--dir))
         (jabber-db--connection nil)
         (jabber-backlog-days 3.0)
         (jabber-backlog-number 10))
    (unwind-protect
        (progn
          ;; Create a v2 database manually.
          (let ((db (sqlite-open jabber-db-path)))
            (sqlite-execute db "PRAGMA journal_mode=WAL")
            (sqlite-execute db "\
CREATE TABLE message (
  id INTEGER PRIMARY KEY, account TEXT NOT NULL, peer TEXT NOT NULL,
  resource TEXT, occupant_id TEXT,
  direction TEXT NOT NULL, type TEXT, body TEXT,
  timestamp INTEGER NOT NULL, stanza_id TEXT, server_id TEXT,
  oob_url TEXT, oob_desc TEXT, encrypted INTEGER DEFAULT 0,
  delivered_at INTEGER, displayed_at INTEGER,
  retracted_by TEXT, retraction_reason TEXT, edited INTEGER DEFAULT 0)")
            ;; Insert a message with OOB data.
            (sqlite-execute db "\
INSERT INTO message (account, peer, direction, type, body, timestamp,
  stanza_id, oob_url, oob_desc)
VALUES ('me@x.com', 'peer@x.com', 'in', 'chat', 'file', 1000,
  'sid-1', 'https://example.com/f.pdf', 'A file')")
            ;; Insert a message without OOB data.
            (sqlite-execute db "\
INSERT INTO message (account, peer, direction, type, body, timestamp,
  stanza_id)
VALUES ('me@x.com', 'peer@x.com', 'in', 'chat', 'text', 1001,
  'sid-2')")
            (sqlite-execute db "PRAGMA user_version=2")
            (sqlite-close db))
          ;; Open with migration.
          (jabber-db-ensure-open)
          ;; Check version is current (full chain: v2 through latest).
          (should (= jabber-db--schema-version
                     (caar (sqlite-select jabber-db--connection
                                          "PRAGMA user_version"))))
          (let ((tables (mapcar #'car
                                (sqlite-select jabber-db--connection
                                  "SELECT name FROM sqlite_master WHERE type='table'"))))
            (should (member "message_reaction" tables))
            (should (member "message_reaction_actor" tables)))
          ;; OOB data migrated to child table.
          (let ((oob-rows (sqlite-select jabber-db--connection
                           "SELECT url, desc FROM message_oob")))
            (should (= 1 (length oob-rows)))
            (should (string= "https://example.com/f.pdf" (caar oob-rows)))
            (should (string= "A file" (cadar oob-rows))))
          ;; Old columns should be gone.
          (let ((cols (mapcar #'car
                              (sqlite-select jabber-db--connection
                                "SELECT name FROM pragma_table_info('message')"))))
            (should-not (member "oob_url" cols))
            (should-not (member "oob_desc" cols))))
      (jabber-db-close)
      (when (file-directory-p jabber-test-db--dir)
        (delete-directory jabber-test-db--dir t)))))

(ert-deftest jabber-test-db-oob-dedup-replacement ()
  "Failed-decrypt replacement updates OOB entries."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      ;; Store with failed decrypt body and OOB.
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "[me@example.com: could not decrypt]" ts nil "id-dec" nil nil
       '(("https://old.com/x.pdf" . "Old")))
      ;; Re-store with real body and new OOB entries.
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Decrypted text" ts nil "id-dec" nil nil
       '(("https://new.com/a.pdf" . "New A")
         ("https://new.com/b.pdf" . "New B")))
      ;; Body should be updated.
      (let ((body (caar (sqlite-select jabber-db--connection
                         "SELECT body FROM message WHERE stanza_id = 'id-dec'"))))
        (should (string= "Decrypted text" body)))
      ;; OOB entries should be replaced.
      (let ((oob (sqlite-select jabber-db--connection
                  "SELECT url, desc FROM message_oob ORDER BY id")))
        (should (= 2 (length oob)))
        (should (string= "https://new.com/a.pdf" (car (nth 0 oob))))
        (should (string= "https://new.com/b.pdf" (car (nth 1 oob))))))))

;;; Group: message_reaction child table

(ert-deftest jabber-test-db-reaction-table-exists ()
  "The reaction row and actor metadata tables exist in fresh databases."
  (jabber-test-db-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "message_reaction" tables))
      (should (member "message_reaction_actor" tables)))
    (let ((indexes (mapcar #'car
                           (sqlite-select jabber-db--connection
                             "SELECT name FROM sqlite_master WHERE type='index'"))))
      (should (member "idx_reaction_message_id" indexes)))))

(ert-deftest jabber-test-db-v5-repairs-missing-reaction-actor-table ()
  "A v5 DB missing message_reaction_actor is repaired without data loss."
  (skip-unless (fboundp 'sqlite-open))
  (let* ((jabber-test-db--dir (make-temp-file "jabber-db-test" t))
         (jabber-db-path (expand-file-name "test.sqlite" jabber-test-db--dir))
         (jabber-db--connection nil))
    (unwind-protect
        (progn
          (let ((db (sqlite-open jabber-db-path)))
            (sqlite-execute db "\
CREATE TABLE message (
  id INTEGER PRIMARY KEY,
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  direction TEXT NOT NULL,
  type TEXT,
  body TEXT,
  timestamp INTEGER NOT NULL,
  stanza_id TEXT)")
            (sqlite-execute db "\
CREATE TABLE message_reaction (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  reaction   TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender, reaction))")
            (sqlite-execute db "\
INSERT INTO message (id, account, peer, direction, type, body, timestamp, stanza_id)
VALUES (1, 'me@example.com', 'friend@example.com', 'in', 'chat', 'hello', 1000,
  'target-1')")
            (sqlite-execute db "\
INSERT INTO message_reaction (message_id, sender, reaction, updated_at)
VALUES (1, 'friend@example.com', '👍', 1001)")
            (sqlite-execute db "PRAGMA user_version=5")
            (sqlite-close db))
          (jabber-db-ensure-open)
          (let ((tables (mapcar #'car
                                (sqlite-select jabber-db--connection
                                  "SELECT name FROM sqlite_master WHERE type='table'"))))
            (should (member "message_reaction_actor" tables)))
          (should (= 1 (caar (sqlite-select jabber-db--connection
                              "SELECT count(*) FROM message_reaction"))))
          (should (equal '("friend@example.com" 1001)
                         (car (sqlite-select jabber-db--connection
                                "SELECT sender, updated_at FROM message_reaction_actor"))))
          (should-not (jabber-db-replace-reactions
                       "me@example.com" "friend@example.com" "chat" "target-1"
                       "friend@example.com" '("🎉") 1000))
          (should (equal '("👍" 1001)
                         (car (sqlite-select jabber-db--connection
                                "SELECT reaction, updated_at FROM message_reaction"))))
          (should (string= "hello"
                           (caar (sqlite-select jabber-db--connection
                                  "SELECT body FROM message WHERE id = 1"))))
          (jabber-db-close)
          (jabber-db-ensure-open)
          (should (= 1 (caar (sqlite-select jabber-db--connection
                              "SELECT count(*) FROM message_reaction_actor"))))
          (should (equal '("friend@example.com" 1001)
                         (car (sqlite-select jabber-db--connection
                                "SELECT sender, updated_at FROM message_reaction_actor"))))
          (should (equal '("👍" 1001)
                         (car (sqlite-select jabber-db--connection
                                "SELECT reaction, updated_at FROM message_reaction"))))
          (should (string= "hello"
                           (caar (sqlite-select jabber-db--connection
                                  "SELECT body FROM message WHERE id = 1")))))
      (jabber-db-close)
      (when (file-directory-p jabber-test-db--dir)
        (delete-directory jabber-test-db--dir t)))))

(ert-deftest jabber-test-db-reaction-fallback-body-not-stored ()
  "Reaction fallback body is not stored as a normal message body."
  (jabber-test-db-with-db
    (let ((xml `(message ((from . "friend@example.com/laptop")
                          (type . "chat")
                          (id . "reaction-1"))
                         (body nil "> quoted\n👍")
                         (reactions ((xmlns . ,jabber-reactions-xmlns)
                                     (id . "target-1"))
                                    (reaction nil "👍"))
                         (fallback ((xmlns . "urn:xmpp:fallback:0")
                                    (for . ,jabber-reactions-xmlns))
                                   (body ((start . "0") (end . "10")))))))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (jabber-db--message-handler 'fake-jc xml))
      (should (null (jabber-db-query "me@example.com" "friend@example.com"))))))

(ert-deftest jabber-test-db-replace-reactions-chat-by-stanza-id ()
  "Direct-chat reactions are stored against the target stanza id."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (should (jabber-db-replace-reactions
             "me@example.com" "friend@example.com" "chat" "stanza-1"
             "friend@example.com" '("👍" "👍" "🎉")))
    (let ((rows (sqlite-select jabber-db--connection
                 "SELECT sender, reaction FROM message_reaction")))
      (should (= 2 (length rows)))
      (should (member '("friend@example.com" "👍") rows))
      (should (member '("friend@example.com" "🎉") rows)))))

(ert-deftest jabber-test-db-replace-reactions-groupchat-by-server-id ()
  "MUC reactions are stored against the target server id."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "room@example.com"
                             "in" "groupchat" "hello" 1000
                             "alice" nil "server-1")
    (should (jabber-db-replace-reactions
             "me@example.com" "room@example.com" "groupchat" "server-1"
             "room@example.com/bob" '("❤️")))
    (let ((row (car (sqlite-select jabber-db--connection
                    "SELECT sender, reaction FROM message_reaction"))))
      (should (equal row '("room@example.com/bob" "❤️"))))))

(ert-deftest jabber-test-db-replace-reactions-stores-source-timestamp ()
  "Reaction replacement stores the supplied source timestamp."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (should (jabber-db-replace-reactions
             "me@example.com" "friend@example.com" "chat" "stanza-1"
             "alice" '("👍") 1234))
    (should (= 1234 (caar (sqlite-select jabber-db--connection
                            "SELECT updated_at FROM message_reaction"))))
    (should (= 1234 (caar (sqlite-select jabber-db--connection
                            "SELECT updated_at FROM message_reaction_actor"))))))

(ert-deftest jabber-test-db-replace-reactions-newer-overwrites ()
  "A newer reaction replacement overwrites existing sender state."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍") 1000)
    (should (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                         "chat" "stanza-1" "alice" '("🎉") 1001))
    (should (equal (sqlite-select jabber-db--connection
                     "SELECT reaction, updated_at FROM message_reaction")
                   '(("🎉" 1001))))))

(ert-deftest jabber-test-db-replace-reactions-stale-ignored ()
  "An older or equal reaction replacement does not overwrite sender state."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍") 1000)
    (should-not (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                             "chat" "stanza-1" "alice" '("🎉") 999))
    (should-not (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                             "chat" "stanza-1" "alice" '("❤️") 1000))
    (should (equal (sqlite-select jabber-db--connection
                     "SELECT reaction, updated_at FROM message_reaction")
                   '(("👍" 1000))))))

(ert-deftest jabber-test-db-replace-reactions-old-call-remains-compatible ()
  "Reaction replacement still works when callers omit UPDATED-AT."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (should (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                         "chat" "stanza-1" "alice" '("👍")))
    (should (equal (caar (sqlite-select jabber-db--connection
                           "SELECT reaction FROM message_reaction"))
                   "👍"))))

(ert-deftest jabber-test-db-replace-reactions-empty-removes-sender ()
  "An empty replacement removes only that sender's reactions."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍"))
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "bob" '("🎉"))
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" nil)
    (let ((rows (sqlite-select jabber-db--connection
                 "SELECT sender, reaction FROM message_reaction")))
      (should (equal rows '(("bob" "🎉")))))))

(ert-deftest jabber-test-db-replace-reactions-empty-preserves-actor-timestamp ()
  "Empty replacement records actor timestamp and blocks older replays."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍") 1000)
    (should (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                         "chat" "stanza-1" "alice" nil 1001))
    (should-not (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                             "chat" "stanza-1" "alice" '("🎉") 1000))
    (should (= 1001 (caar (sqlite-select jabber-db--connection
                            "SELECT updated_at FROM message_reaction_actor"))))
    (should (= 0 (caar (sqlite-select jabber-db--connection
                         "SELECT count(*) FROM message_reaction"))))))

(ert-deftest jabber-test-db-replace-reactions-local-same-second-updates ()
  "Local replacements without UPDATED-AT are accepted even in the same second."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 1234.9)))
      (should (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                           "chat" "stanza-1" "alice" '("👍")))
      (should (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                           "chat" "stanza-1" "alice" '("🎉"))))
    (should (equal (sqlite-select jabber-db--connection
                     "SELECT reaction, updated_at FROM message_reaction")
                   '(("🎉" 1234))))))

(ert-deftest jabber-test-db-replace-reactions-explicit-equal-rejected ()
  "Source-ordered replacement with equal timestamp is ignored."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍") 1000)
    (should-not (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                             "chat" "stanza-1" "alice" '("🎉") 1000))
    (should (equal (sqlite-select jabber-db--connection
                     "SELECT reaction, updated_at FROM message_reaction")
                   '(("👍" 1000))))))

(ert-deftest jabber-test-db-backlog-empty-reactions-after-removal ()
  "Empty replacement leaves no reactions in backlog."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍") 1000)
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" nil 1001)
    (let ((entry (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
      (should-not (plist-get entry :reactions)))))

(ert-deftest jabber-test-db-backlog-attaches-reactions ()
  "Backlog entries include persisted reaction state."
  (jabber-test-db-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message "me@example.com" "friend@example.com"
                               "in" "chat" "hello" ts nil "stanza-1"))
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍" "🎉"))
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "bob" '("👍"))
    (let* ((entry (car (jabber-db-backlog
                        "me@example.com" "friend@example.com")))
           (reactions (plist-get entry :reactions)))
      (should (equal (alist-get "alice" reactions nil nil #'equal)
                     '("👍" "🎉")))
      (should (equal (alist-get "bob" reactions nil nil #'equal)
                     '("👍"))))))

(ert-deftest jabber-test-db-reaction-cascade-delete ()
  "Deleting a message cascades to reaction rows and actor metadata."
  (jabber-test-db-with-db
    (jabber-db-store-message "me@example.com" "friend@example.com"
                             "in" "chat" "hello" 1000 nil "stanza-1")
    (jabber-db-replace-reactions "me@example.com" "friend@example.com"
                                 "chat" "stanza-1" "alice" '("👍"))
    (jabber-db-delete-peer-messages "me@example.com" "friend@example.com")
    (should (= 0 (caar (sqlite-select jabber-db--connection
                         "SELECT count(*) FROM message_reaction"))))
    (should (= 0 (caar (sqlite-select jabber-db--connection
                         "SELECT count(*) FROM message_reaction_actor"))))))

(provide 'jabber-test-db)

;;; jabber-test-db.el ends here
