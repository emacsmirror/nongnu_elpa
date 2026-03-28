;;; jabber-db-tests.el --- Tests for jabber-db  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-db)

;;; Test infrastructure

(defmacro jabber-db-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Binds `jabber-db-path' to a temp file, ensures the DB is open,
and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-db-test--dir (make-temp-file "jabber-db-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-db-test--dir))
          (jabber-db--connection nil)
          (jabber-backlog-days 3.0)
          (jabber-backlog-number 10))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-db-test--dir)
         (delete-directory jabber-db-test--dir t)))))

;;; Group 1: Schema and lifecycle

(ert-deftest jabber-db-test-ensure-open-creates-db ()
  "Opening the database creates the file and returns a connection."
  (jabber-db-test-with-db
    (should (sqlitep jabber-db--connection))
    (should (file-exists-p jabber-db-path))))

(ert-deftest jabber-db-test-ensure-open-idempotent ()
  "Calling ensure-open twice returns the same connection."
  (jabber-db-test-with-db
    (let ((db1 jabber-db--connection)
          (db2 (jabber-db-ensure-open)))
      (should (eq db1 db2)))))

(ert-deftest jabber-db-test-close-and-reopen ()
  "Closing and reopening the database works."
  (jabber-db-test-with-db
    (jabber-db-close)
    (should (null jabber-db--connection))
    (let ((db (jabber-db-ensure-open)))
      (should (sqlitep db)))))

(ert-deftest jabber-db-test-schema-version ()
  "The user_version pragma matches `jabber-db--schema-version'."
  (jabber-db-test-with-db
    (should (= jabber-db--schema-version
               (caar (sqlite-select jabber-db--connection
                                    "PRAGMA user_version"))))))

(ert-deftest jabber-db-test-wal-mode ()
  "WAL journal mode is active."
  (jabber-db-test-with-db
    (should (string= "wal"
                     (caar (sqlite-select jabber-db--connection
                                         "PRAGMA journal_mode"))))))

(ert-deftest jabber-db-test-tables-exist ()
  "All expected tables and indexes exist."
  (jabber-db-test-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "message" tables))
      (should (member "message_fts" tables))
      (should (member "chat_settings" tables)))))

;;; Group 2: Store and retrieve

(ert-deftest jabber-db-test-store-and-query ()
  "Storing a message and querying it back returns matching fields."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-store-with-stanza-id ()
  "Storing a message with stanza-id and server-id preserves them."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Test" ts nil "origin-123" "server-456")
      (let* ((rows (jabber-db-query "me@example.com" "friend@example.com"))
             (row (car rows)))
        (should (string= "origin-123" (plist-get row :stanza-id)))
        (should (string= "server-456" (plist-get row :server-id)))))))

(ert-deftest jabber-db-test-store-unicode-body ()
  "Unicode text in message body is preserved."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time)))
          (body "Hej! Gruss Gott! Ελληνικά 日本語 🎉"))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" body ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (string= body (plist-get row :body)))))))

(ert-deftest jabber-db-test-store-nil-body ()
  "Storing a message with nil body succeeds."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" nil ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (null (plist-get row :body)))))))

(ert-deftest jabber-db-test-store-multiline-body ()
  "Newlines in message body are preserved."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time)))
          (body "Line one\nLine two\nLine three"))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" body ts)
      (let ((row (car (jabber-db-query "me@example.com" "friend@example.com"))))
        (should (string= body (plist-get row :body)))))))

;;; Group 3: Backlog format and ordering

(ert-deftest jabber-db-test-backlog-plist-format ()
  "Backlog entries are plists with :from, :body, :timestamp, :delayed, :direction, :msg-type."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-backlog-chat-no-resource ()
  "Chat backlog sender is bare JID when no resource is stored."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello!" ts)
      (let ((entry (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
        (should (string= "friend@example.com" (plist-get entry :from)))))))

(ert-deftest jabber-db-test-backlog-outgoing-format ()
  "Outgoing backlog entries have account JID as :from."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "out" "chat"
       "Hi there" ts)
      (let ((entry (car (jabber-db-backlog "me@example.com" "friend@example.com"))))
        (should (string= "out" (plist-get entry :direction)))
        (should (string= "me@example.com" (plist-get entry :from)))))))

(ert-deftest jabber-db-test-backlog-ordering ()
  "Backlog returns messages in reverse chronological order."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-backlog-respects-count ()
  "Backlog returns at most COUNT messages."
  (jabber-db-test-with-db
    (let ((now (floor (float-time))))
      (dotimes (i 5)
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         (format "Message %d" i) (- now (* i 10))))
      (let ((entries (jabber-db-backlog
                      "me@example.com" "friend@example.com" 2)))
        (should (= 2 (length entries)))))))

(ert-deftest jabber-db-test-backlog-time-filter ()
  "Backlog respects the start-time parameter."
  (jabber-db-test-with-db
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

;;; Group 4: FTS search

(ert-deftest jabber-db-test-fts-search ()
  "Full-text search finds messages by keyword."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-fts-search-with-peer ()
  "FTS search scoped to a specific peer."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-fts-search-no-match ()
  "FTS search returns nil when no messages match."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Hello world" ts)
      (should (null (jabber-db-search "me@example.com" "xyzzynonexistent"))))))

;;; Group 5: Dedup and last-timestamp

(ert-deftest jabber-db-test-dedup-stanza-id ()
  "Duplicate stanza_id keeps one row with body preserved and timestamp updated."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-dedup-scoped-by-account ()
  "Same stanza_id from different accounts are stored as separate messages."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-no-dedup-without-stanza-id ()
  "Messages without stanza_id are never deduped."
  (jabber-db-test-with-db
    (let ((ts (- (floor (float-time)) 10)))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Same body" ts)
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Same body" (1+ ts))
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
        (should (= 2 (length rows)))))))

(ert-deftest jabber-db-test-last-timestamp ()
  "last-timestamp returns the latest timestamp for a peer."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-account-isolation ()
  "Messages from different accounts are isolated."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-peer-isolation ()
  "Messages to different peers are isolated."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-empty-backlog ()
  "Backlog returns nil on an empty database."
  (jabber-db-test-with-db
    (should (null (jabber-db-backlog
                   "me@example.com" "friend@example.com")))))

(ert-deftest jabber-db-test-empty-search ()
  "Search returns nil on an empty database."
  (jabber-db-test-with-db
    (should (null (jabber-db-search "me@example.com" "anything")))))

(ert-deftest jabber-db-test-empty-last-timestamp ()
  "last-timestamp returns nil when no messages exist."
  (jabber-db-test-with-db
    (should (null (jabber-db-last-timestamp
                   "me@example.com" "friend@example.com")))))

;;; Group 8: Query pagination

(ert-deftest jabber-db-test-query-pagination ()
  "Query with limit and offset returns correct page."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-query-time-range ()
  "Query with start-time and end-time filters correctly."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-persistence ()
  "Data survives close and reopen."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-muc-backlog-sender-has-nickname ()
  "MUC backlog sender includes room JID and nickname as resource."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-muc-backlog-multiple-senders ()
  "MUC backlog preserves distinct nicknames for different senders."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-muc-backlog-persistence ()
  "MUC messages survive close/reopen and retain nicknames."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-nil-path-disables-storage ()
  "Setting jabber-db-path to nil disables all DB operations."
  (let ((jabber-db-path nil)
        (jabber-db--connection nil))
    (should (null (jabber-db-ensure-open)))
    (should (null (jabber-db-store-message
                   "me@example.com" "friend@example.com" "in" "chat"
                   "Hello" (floor (float-time)))))
    (should (null (jabber-db-backlog "me@example.com" "friend@example.com")))))

;;; Group 11: Import from history

(ert-deftest jabber-db-test-import-history ()
  "Importing from flat-file history populates the database."
  (jabber-db-test-with-db
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
      (jabber-db-import-history)
      (let ((rows (jabber-db-query "" "friend@example.com"
                                   0 (floor (float-time)))))
        (should (= 2 (length rows)))
        (should (string= "Hi there" (plist-get (car rows) :body)))
        (should (string= "in" (plist-get (car rows) :direction)))
        (should (string= "Hey!" (plist-get (cadr rows) :body)))
        (should (string= "out" (plist-get (cadr rows) :direction)))))))

(ert-deftest jabber-db-test-import-global-history ()
  "Importing from a global history file works."
  (jabber-db-test-with-db
    (let* ((jabber-use-global-history t)
           (jabber-global-history-filename
            (expand-file-name "global-history"
                              (file-name-directory jabber-db-path))))
      (with-temp-file jabber-global-history-filename
        (insert "[\"2024-06-01T12:00:00Z\" \"in\" \"alice@example.com\" \"me\" \"Global msg\"]\n"))
      (jabber-db-import-history)
      (let ((rows (jabber-db-query "" "alice@example.com"
                                   0 (floor (float-time)))))
        (should (= 1 (length rows)))
        (should (string= "Global msg" (plist-get (car rows) :body)))))))

;;; Group 12: jabber-db--row-to-plist

(ert-deftest jabber-db-test-row-to-plist-incoming-chat ()
  "Incoming chat message builds correct plist."
  (let* ((row '("me@example.com" "alice@example.com" "in"
                "Hello!" 1700000000 "mobile" "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "alice@example.com/mobile" (plist-get plist :from)))
    (should (string= "Hello!" (plist-get plist :body)))
    (should (string= "in" (plist-get plist :direction)))
    (should (string= "chat" (plist-get plist :msg-type)))
    (should (plist-get plist :delayed))
    (should (equal (seconds-to-time 1700000000) (plist-get plist :timestamp)))))

(ert-deftest jabber-db-test-row-to-plist-incoming-no-resource ()
  "Incoming message without resource uses bare JID as :from."
  (let* ((row '("me@example.com" "alice@example.com" "in"
                "Hi" 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "alice@example.com" (plist-get plist :from)))))

(ert-deftest jabber-db-test-row-to-plist-outgoing ()
  "Outgoing message uses account JID as :from."
  (let* ((row '("me@example.com" "alice@example.com" "out"
                "Bye!" 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "me@example.com" (plist-get plist :from)))))

(ert-deftest jabber-db-test-row-to-plist-groupchat ()
  "Groupchat message has msg-type groupchat."
  (let* ((row '("me@example.com" "room@conf.example.com" "in"
                "Hello room" 1700000000 "Alice" "groupchat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "groupchat" (plist-get plist :msg-type)))
    (should (string= "room@conf.example.com/Alice" (plist-get plist :from)))))

(ert-deftest jabber-db-test-row-to-plist-nil-body ()
  "Nil body is converted to empty string."
  (let* ((row '("me@example.com" "alice@example.com" "in"
                nil 1700000000 nil "chat"))
         (plist (jabber-db--row-to-plist row)))
    (should (string= "" (plist-get plist :body)))))

(ert-deftest jabber-db-test-row-to-plist-encrypted-flag ()
  "Encrypted flag is correctly converted to boolean."
  (let* ((row '("me@example.com" "alice@example.com" "in"
                "Secret" 1700000000 nil "chat" nil nil 1))
         (plist (jabber-db--row-to-plist row)))
    (should (eq t (plist-get plist :encrypted)))))

(ert-deftest jabber-db-test-row-to-plist-not-encrypted ()
  "Zero encrypted flag yields nil."
  (let* ((row '("me@example.com" "alice@example.com" "in"
                "Plain" 1700000000 nil "chat" nil nil 0))
         (plist (jabber-db--row-to-plist row)))
    (should-not (plist-get plist :encrypted))))

;;; Group 13: Chat settings (encryption persistence)

(ert-deftest jabber-db-test-chat-settings-table-exists ()
  "The chat_settings table is created by the schema."
  (jabber-db-test-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master WHERE type='table'"))))
      (should (member "chat_settings" tables)))))

(ert-deftest jabber-db-test-set-and-get-encryption-omemo ()
  "Storing OMEMO encryption and reading it back returns the symbol."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-db-test-set-and-get-encryption-plaintext ()
  "Storing plaintext encryption and reading it back returns the symbol."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-db-test-get-encryption-default-returns-nil ()
  "Storing `default' encryption returns nil from get."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'default)
    (should (null (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-db-test-get-encryption-missing-returns-nil ()
  "Querying encryption for an unknown peer returns nil."
  (jabber-db-test-with-db
    (should (null (jabber-db-get-chat-encryption "me@example.com" "nobody@example.com")))))

(ert-deftest jabber-db-test-set-encryption-overwrites ()
  "Setting encryption twice overwrites the previous value."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-db-test-chat-settings-account-isolation ()
  "Encryption settings are isolated per account."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "alice@example.com" "bob@example.com" 'omemo)
    (jabber-db-set-chat-encryption "carol@example.com" "bob@example.com" 'plaintext)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "alice@example.com" "bob@example.com")))
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "carol@example.com" "bob@example.com")))))

(ert-deftest jabber-db-test-chat-settings-peer-isolation ()
  "Encryption settings are isolated per peer."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-set-chat-encryption "me@example.com" "bob@example.com" 'plaintext)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption "me@example.com" "bob@example.com")))))

(ert-deftest jabber-db-test-chat-settings-persist-across-reopen ()
  "Encryption settings survive close and reopen."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'omemo)
    (jabber-db-close)
    (jabber-db-ensure-open)
    (should (eq 'omemo
                (jabber-db-get-chat-encryption "me@example.com" "alice@example.com")))))

(ert-deftest jabber-db-test-chat-settings-muc-peer ()
  "Encryption settings work with MUC room JIDs."
  (jabber-db-test-with-db
    (jabber-db-set-chat-encryption
     "me@example.com" "room@conference.example.com" 'plaintext)
    (should (eq 'plaintext
                (jabber-db-get-chat-encryption
                 "me@example.com" "room@conference.example.com")))))

(ert-deftest jabber-db-test-chat-settings-nil-path ()
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

(defun jabber-db-test--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT (user@server)."
  (let ((jc (gensym "jabber-db-test-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

(defmacro jabber-db-test-with-chat-buffer (account peer &rest body)
  "Run BODY in a temp chat buffer with fake connection for ACCOUNT talking to PEER.
Sets up jabber-chatting-with before jabber-chat-mode-setup, mimicking
the corrected jabber-chat-create-buffer order."
  (declare (indent 2) (debug t))
  `(jabber-db-test-with-db
     (let* ((jc (jabber-db-test--make-fake-jc ,account))
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

(defmacro jabber-db-test-with-muc-buffer (account group &rest body)
  "Run BODY in a temp MUC buffer with fake connection for ACCOUNT in GROUP.
Sets up jabber-group before jabber-chat-mode-setup, mimicking
the corrected jabber-muc-create-buffer order."
  (declare (indent 2) (debug t))
  `(jabber-db-test-with-db
     (let* ((jc (jabber-db-test--make-fake-jc ,account))
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

(ert-deftest jabber-db-test-chat-buffer-loads-encryption-from-db ()
  "1:1 chat buffer loads saved encryption from DB on setup."
  (jabber-db-test-with-chat-buffer "me@example.com" "alice@example.com"
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    ;; Reset and re-run setup to simulate fresh buffer
    (setq jabber-chat-encryption nil)
    (jabber-chat-mode-setup jc #'ignore)
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-db-test-chat-buffer-falls-back-to-default ()
  "1:1 chat buffer uses default when no DB setting exists."
  (jabber-db-test-with-chat-buffer "me@example.com" "bob@example.com"
    (should (eq 'omemo jabber-chat-encryption))))

(ert-deftest jabber-db-test-chat-buffer-default-plaintext ()
  "1:1 chat buffer respects jabber-chat-default-encryption when set to plaintext."
  (jabber-db-test-with-db
    (let* ((jc (jabber-db-test--make-fake-jc "me@example.com"))
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

(ert-deftest jabber-db-test-chat-buffer-db-overrides-default ()
  "DB setting overrides jabber-chat-default-encryption."
  (jabber-db-test-with-chat-buffer "me@example.com" "alice@example.com"
    ;; Default is omemo, but DB says plaintext
    (jabber-db-set-chat-encryption "me@example.com" "alice@example.com" 'plaintext)
    (setq jabber-chat-encryption nil)
    (jabber-chat-mode-setup jc #'ignore)
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-db-test-muc-buffer-loads-encryption-from-db ()
  "MUC buffer loads saved encryption from DB on setup."
  (jabber-db-test-with-db
    (let* ((jc (jabber-db-test--make-fake-jc "me@example.com"))
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

(ert-deftest jabber-db-test-muc-buffer-falls-back-to-plaintext ()
  "MUC buffer defaults to plaintext when no DB setting exists."
  (jabber-db-test-with-muc-buffer "me@example.com" "room@conference.example.com"
    (should (eq 'plaintext jabber-chat-encryption))))

(ert-deftest jabber-db-test-chat-buffer-without-peer-falls-back ()
  "Buffer without jabber-chatting-with or jabber-group falls back to default."
  (jabber-db-test-with-db
    (let* ((jc (jabber-db-test--make-fake-jc "me@example.com"))
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

(ert-deftest jabber-db-test-toggle-save-roundtrip ()
  "Toggling encryption saves to DB and reloading a fresh buffer picks it up."
  (jabber-db-test-with-db
    (let* ((jc (jabber-db-test--make-fake-jc "me@example.com"))
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

(ert-deftest jabber-db-test-redisplay-reloads-encryption ()
  "jabber-chat-redisplay reloads encryption from DB."
  (jabber-db-test-with-db
    (let* ((jc (jabber-db-test--make-fake-jc "me@example.com"))
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

(ert-deftest jabber-db-test-receipt-columns ()
  "Message table has delivered_at and displayed_at columns."
  (jabber-db-test-with-db
    (sqlite-execute jabber-db--connection
                    "INSERT INTO message (account,peer,direction,timestamp)
                     VALUES ('a','b','out',1)")
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at, displayed_at FROM message"))))
      (should (equal row '(nil nil))))))

(ert-deftest jabber-db-test-update-receipt-delivered ()
  "Update delivered_at for a message by stanza_id."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-001")
    (jabber-db-update-receipt "msg-001" "delivered_at" 1001)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message WHERE stanza_id='msg-001'"))))
      (should (equal row '(1001))))))

(ert-deftest jabber-db-test-update-receipt-displayed ()
  "Update displayed_at for a message by stanza_id."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-002")
    (jabber-db-update-receipt "msg-002" "displayed_at" 1002)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT displayed_at FROM message WHERE stanza_id='msg-002'"))))
      (should (equal row '(1002))))))

(ert-deftest jabber-db-test-update-receipt-no-overwrite ()
  "Duplicate receipt does not overwrite earlier timestamp."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000
                             nil "msg-003")
    (jabber-db-update-receipt "msg-003" "delivered_at" 1001)
    (jabber-db-update-receipt "msg-003" "delivered_at" 9999)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message WHERE stanza_id='msg-003'"))))
      (should (equal row '(1001))))))

(ert-deftest jabber-db-test-update-receipt-nil-stanza-id ()
  "Update with nil stanza_id is a no-op."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@example.com" "them@example.com"
                             "out" "chat" "hello" 1000)
    (jabber-db-update-receipt nil "delivered_at" 1001)
    (let ((row (car (sqlite-select jabber-db--connection
                     "SELECT delivered_at FROM message LIMIT 1"))))
      (should (equal row '(nil))))))

;;; Group 16: Delete peer messages

(ert-deftest jabber-db-test-delete-peer-messages ()
  "Deleting peer messages removes all rows for that account+peer."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-delete-peer-messages-empty ()
  "Deleting from a nonexistent peer is a no-op."
  (jabber-db-test-with-db
    (jabber-db-delete-peer-messages "me@example.com" "nobody@example.com")
    ;; No error, no rows affected
    (should t)))

;;; Group 17: Message retraction

(ert-deftest jabber-db-test-retract-with-reason ()
  "jabber-db-retract-message persists moderator and reason; backlog returns both."
  (skip-unless (fboundp 'sqlite-open))
  (let ((jabber-backlog-days 3.0)
        (jabber-backlog-number 10)
        (now (floor (float-time))))
    (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-retract-without-reason ()
  "jabber-db-retract-message with no reason leaves :retraction-reason nil."
  (skip-unless (fboundp 'sqlite-open))
  (let ((jabber-backlog-days 3.0)
        (jabber-backlog-number 10)
        (now (floor (float-time))))
    (jabber-db-test-with-db
      (jabber-db-store-message "me@x.com" "room@x.com"
                               "in" "groupchat" "msg" now
                               nil nil "srv-retract-2")
      (jabber-db-retract-message "srv-retract-2" "room@x.com/mod")
      (let* ((entries (jabber-db-backlog "me@x.com" "room@x.com"))
             (entry (car entries)))
        (should (plist-get entry :retracted))
        (should-not (plist-get entry :retraction-reason))))))

;;; Group: Failed-decrypt replacement

(ert-deftest jabber-db-test-store-replaces-failed-decrypt-by-stanza-id ()
  "Re-storing a message with real text replaces a decrypt-failure placeholder."
  (jabber-db-test-with-db
    ;; Store with failed-decrypt body
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" "stanza-1" "srv-1")
    ;; Re-store same stanza-id with decrypted body
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello there" 1700000000
                             "res" "stanza-1" "srv-1"
                             nil nil nil t)
    ;; Should have exactly one row with the decrypted body
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE stanza_id = ?"
                               '("stanza-1"))))
      (should (= 1 (length rows)))
      (should (string= "hello there" (caar rows))))))

(ert-deftest jabber-db-test-store-replaces-failed-decrypt-by-server-id ()
  "Re-storing by server-id replaces a decrypt-failure placeholder."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000000
                             "res" nil "srv-2")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "decrypted text" 1700000000
                             "res" nil "srv-2"
                             nil nil nil t)
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body FROM message WHERE server_id = ?"
                               '("srv-2"))))
      (should (= 1 (length rows)))
      (should (string= "decrypted text" (caar rows))))))

(ert-deftest jabber-db-test-store-no-replace-when-still-undecryptable ()
  "Re-storing with another failed-decrypt body does not update."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-store-no-replace-when-already-decrypted ()
  "Re-storing does not overwrite an already-decrypted message."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-store-normalizes-timestamp-on-dedup ()
  "Re-storing a duplicate updates the timestamp to the server's value."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-store-normalizes-timestamp-and-replaces-decrypt ()
  "Failed-decrypt replacement also normalizes the timestamp."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "[OMEMO: could not decrypt]" 1700000099
                             "res" "stanza-6" "srv-6")
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "decrypted" 1700000100
                             "res" "stanza-6" "srv-6"
                             nil nil nil t)
    (let ((rows (sqlite-select (jabber-db-ensure-open)
                               "SELECT body, timestamp FROM message WHERE stanza_id = ?"
                               '("stanza-6"))))
      (should (= 1 (length rows)))
      (should (string= "decrypted" (caar rows)))
      (should (= 1700000100 (cadar rows))))))

;;; Group: Schema v2 migration and constraints

(defconst jabber-db-test--v1-ddl
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

(defmacro jabber-db-test-with-v1-db (&rest body)
  "Run BODY with a v1 database (has raw_xml, no occupant_id)."
  (declare (indent 0) (debug t))
  `(let* ((jabber-db-test--dir (make-temp-file "jabber-db-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-db-test--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (let ((db (sqlite-open jabber-db-path)))
           (dolist (ddl jabber-db-test--v1-ddl)
             (sqlite-execute db ddl))
           (sqlite-execute db "PRAGMA user_version=1")
           (sqlite-close db)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-db-test--dir)
         (delete-directory jabber-db-test--dir t)))))

(ert-deftest jabber-db-test-v1-to-v2-migration ()
  "Migrating from v1 adds occupant_id and drops raw_xml."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-v1-db
    ;; Insert a v1 row with raw_xml
    (let ((db (sqlite-open jabber-db-path)))
      (sqlite-execute db "\
INSERT INTO message (account, peer, direction, type, body, timestamp, raw_xml)
VALUES ('me@x.com', 'friend@x.com', 'in', 'chat', 'hello', 1000, '<msg/>')")
      (sqlite-close db))
    ;; Open via jabber-db which triggers migration
    (jabber-db-ensure-open)
    (let ((version (caar (sqlite-select jabber-db--connection "PRAGMA user_version"))))
      (should (= 2 version)))
    ;; occupant_id column exists (NULL for old rows)
    (let ((rows (sqlite-select jabber-db--connection
                               "SELECT occupant_id FROM message LIMIT 1")))
      (should (= 1 (length rows)))
      (should (null (caar rows))))
    ;; raw_xml column is gone
    (should-error
     (sqlite-select jabber-db--connection
                    "SELECT raw_xml FROM message LIMIT 1"))))

(ert-deftest jabber-db-test-v1-migration-preserves-data ()
  "Migrating from v1 preserves existing message data."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-v1-db
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

(ert-deftest jabber-db-test-check-direction-on-fresh-db ()
  "CHECK constraint rejects invalid direction on fresh databases."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-db
    (should-error
     (sqlite-execute jabber-db--connection
                     "INSERT INTO message (account, peer, direction, type, body, timestamp)
                      VALUES ('a', 'b', 'bad', 'chat', 'x', 1000)"))))

(ert-deftest jabber-db-test-check-type-on-fresh-db ()
  "CHECK constraint rejects invalid message type on fresh databases."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-db
    (should-error
     (sqlite-execute jabber-db--connection
                     "INSERT INTO message (account, peer, direction, type, body, timestamp)
                      VALUES ('a', 'b', 'in', 'invalid', 'x', 1000)"))))

(ert-deftest jabber-db-test-occupant-id-round-trip ()
  "Storing and retrieving occupant_id works end-to-end."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-db
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "hello" (floor (float-time))
                             "nick" "sid-1" nil "occ-abc-123")
    (let* ((rows (jabber-db-query "me@x.com" "room@x.com"))
           (row (car rows)))
      (should row)
      (should (string= "occ-abc-123" (plist-get row :occupant-id))))))

(ert-deftest jabber-db-test-occupant-id-nil-when-absent ()
  "occupant_id is nil when not provided."
  (skip-unless (fboundp 'sqlite-open))
  (jabber-db-test-with-db
    (jabber-db-store-message "me@x.com" "friend@x.com" "in" "chat"
                             "hello" (floor (float-time)))
    (let* ((rows (jabber-db-query "me@x.com" "friend@x.com"))
           (row (car rows)))
      (should row)
      (should (null (plist-get row :occupant-id))))))

;;; Group: server-ids-by-occupant-id

(ert-deftest jabber-db-test-server-ids-by-occupant-id ()
  "Returns correct server-ids for an occupant-id."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-server-ids-by-occupant-id-excludes-retracted ()
  "Already-retracted messages are excluded."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-server-ids-by-occupant-id-excludes-nil-server-id ()
  "Messages without server-id are excluded."
  (jabber-db-test-with-db
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

(ert-deftest jabber-db-test-server-ids-by-occupant-id-unknown ()
  "Returns nil for an unknown occupant-id."
  (jabber-db-test-with-db
    (should (null (jabber-db-server-ids-by-occupant-id
                   "me@x.com" "room@x.com" "nonexistent")))))

(ert-deftest jabber-db-test-occupant-id-by-server-id ()
  "Returns occupant-id for a known server-id."
  (jabber-db-test-with-db
    (jabber-db-store-message "me@x.com" "room@x.com" "in" "groupchat"
                             "hello" (floor (float-time))
                             "nick" nil "srv-occ-1" "occ-lookup")
    (should (string= "occ-lookup"
                      (jabber-db-occupant-id-by-server-id "srv-occ-1")))))

(ert-deftest jabber-db-test-occupant-id-by-server-id-nil ()
  "Returns nil for unknown server-id."
  (jabber-db-test-with-db
    (should (null (jabber-db-occupant-id-by-server-id "nonexistent")))))

(ert-deftest jabber-db-test-store-preserves-retraction-on-dedup ()
  "Re-storing a retracted message does not clear retracted_by."
  (jabber-db-test-with-db
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

(provide 'jabber-db-tests)

;;; jabber-db-tests.el ends here
