;;; jabber-db-tests.el --- Tests for jabber-db  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-db)
(require 'jabber-history)

;;; ---- Test infrastructure ----

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

;;; ---- Group 1: Schema and lifecycle ----

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
  "The user_version pragma is set to 1 after initialization."
  (jabber-db-test-with-db
    (should (= 1 (caar (sqlite-select jabber-db--connection
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
      (should (member "message_fts" tables)))))

;;; ---- Group 2: Store and retrieve ----

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

;;; ---- Group 3: Backlog format and ordering ----

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

;;; ---- Group 4: FTS search ----

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

;;; ---- Group 5: Dedup and last-timestamp ----

(ert-deftest jabber-db-test-dedup-stanza-id ()
  "Inserting a message with a duplicate stanza_id is silently ignored."
  (jabber-db-test-with-db
    (let ((ts (floor (float-time))))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "First" ts nil "dup-id-123")
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat"
       "Duplicate" (1+ ts) nil "dup-id-123")
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
        (should (= 1 (length rows)))
        (should (string= "First" (plist-get (car rows) :body)))))))

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

;;; ---- Group 6: Account isolation ----

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

;;; ---- Group 7: Empty database ----

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

;;; ---- Group 8: Query pagination ----

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

;;; ---- Group 9: Data persistence across close/reopen ----

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

;;; ---- Group 10: MUC backlog round-trip ----

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

;;; ---- Group 11: Import from history ----

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

;;; ---- Group 11: jabber-db--row-to-plist ----

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

(provide 'jabber-db-tests)

;;; jabber-db-tests.el ends here
