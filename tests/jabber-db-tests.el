;;; jabber-db-tests.el --- Tests for jabber-db  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-db)
(require 'jabber-history)

;;; ---- Test infrastructure ----

(defvar jabber-db-test-temp-dir nil
  "Temporary directory for test databases.")

(defun jabber-db-test-setup ()
  "Create a temporary directory and initialize a test database."
  (setq jabber-db-test-temp-dir (make-temp-file "jabber-db-test" t))
  (setq jabber-db-path
        (expand-file-name "test.sqlite" jabber-db-test-temp-dir))
  (setq jabber-db--connection nil)
  ;; Set defaults for backlog parameters
  (setq jabber-backlog-days 3.0)
  (setq jabber-backlog-number 10))

(defun jabber-db-test-teardown ()
  "Close the database and remove the temporary directory."
  (jabber-db-close)
  (when (and jabber-db-test-temp-dir
             (file-directory-p jabber-db-test-temp-dir))
    (delete-directory jabber-db-test-temp-dir t))
  (setq jabber-db-test-temp-dir nil))

;;; ---- Group 1: Schema and lifecycle ----

(ert-deftest jabber-db-test-ensure-open-creates-db ()
  "Opening the database creates the file and returns a connection."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((db (jabber-db-ensure-open)))
        (should (sqlitep db))
        (should (file-exists-p jabber-db-path)))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-ensure-open-idempotent ()
  "Calling ensure-open twice returns the same connection."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((db1 (jabber-db-ensure-open))
            (db2 (jabber-db-ensure-open)))
        (should (eq db1 db2)))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-close-and-reopen ()
  "Closing and reopening the database works."
  (jabber-db-test-setup)
  (unwind-protect
      (progn
        (jabber-db-ensure-open)
        (jabber-db-close)
        (should (null jabber-db--connection))
        (let ((db (jabber-db-ensure-open)))
          (should (sqlitep db))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-schema-version ()
  "The user_version pragma is set to 1 after initialization."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((db (jabber-db-ensure-open)))
        (should (= 1 (caar (sqlite-select db "PRAGMA user_version")))))
    (jabber-db-test-teardown)))

;;; ---- Group 2: Store and retrieve ----

(ert-deftest jabber-db-test-store-and-query ()
  "Storing a message and querying it back returns matching fields."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((ts (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "Hello!" ts "laptop")
        (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
          (should (= 1 (length rows)))
          (let ((row (car rows)))
            (should (string= "me@example.com" (plist-get row :account)))
            (should (string= "friend@example.com" (plist-get row :peer)))
            (should (string= "in" (plist-get row :direction)))
            (should (string= "chat" (plist-get row :type)))
            (should (string= "Hello!" (plist-get row :body)))
            (should (= ts (plist-get row :timestamp)))
            (should (string= "laptop" (plist-get row :resource))))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-store-with-stanza-id ()
  "Storing a message with stanza-id and server-id preserves them."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((ts (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "Test" ts nil "origin-123" "server-456")
        (let* ((rows (jabber-db-query "me@example.com" "friend@example.com"))
               (row (car rows)))
          (should (string= "origin-123" (plist-get row :stanza-id)))
          (should (string= "server-456" (plist-get row :server-id)))))
    (jabber-db-test-teardown)))

;;; ---- Group 3: Backlog format and ordering ----

(ert-deftest jabber-db-test-backlog-vector-format ()
  "Backlog entries are vectors of [timestamp direction sender recipient body]."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((ts (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "Hello!" ts)
        (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
               (entry (car entries)))
          (should (vectorp entry))
          (should (= 5 (length entry)))
          (should (stringp (aref entry 0)))      ; timestamp string
          (should (string= "in" (aref entry 1)))
          (should (string= "friend@example.com" (aref entry 2)))
          (should (string= "me" (aref entry 3)))
          (should (string= "Hello!" (aref entry 4)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-backlog-outgoing-format ()
  "Outgoing backlog entries have sender 'me' and recipient as peer."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((ts (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "out" "chat"
         "Hi there" ts)
        (let* ((entries (jabber-db-backlog "me@example.com" "friend@example.com"))
               (entry (car entries)))
          (should (string= "out" (aref entry 1)))
          (should (string= "me" (aref entry 2)))
          (should (string= "friend@example.com" (aref entry 3)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-backlog-ordering ()
  "Backlog returns messages in reverse chronological order."
  (jabber-db-test-setup)
  (unwind-protect
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
          (should (string= "Third" (aref (nth 0 entries) 4)))
          (should (string= "Second" (aref (nth 1 entries) 4)))
          (should (string= "First" (aref (nth 2 entries) 4)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-backlog-respects-count ()
  "Backlog returns at most COUNT messages."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((now (floor (float-time))))
        (dotimes (i 5)
          (jabber-db-store-message
           "me@example.com" "friend@example.com" "in" "chat"
           (format "Message %d" i) (- now (* i 10))))
        (let ((entries (jabber-db-backlog
                        "me@example.com" "friend@example.com" 2)))
          (should (= 2 (length entries)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-backlog-time-filter ()
  "Backlog respects the start-time parameter."
  (jabber-db-test-setup)
  (unwind-protect
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
          (should (string= "Recent" (aref (nth 0 entries) 4)))
          (should (string= "Old" (aref (nth 1 entries) 4)))))
    (jabber-db-test-teardown)))

;;; ---- Group 4: FTS search ----

(ert-deftest jabber-db-test-fts-search ()
  "Full-text search finds messages by keyword."
  (jabber-db-test-setup)
  (unwind-protect
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
                                  (plist-get (car results) :body)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-fts-search-with-peer ()
  "FTS search scoped to a specific peer."
  (jabber-db-test-setup)
  (unwind-protect
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
                           (plist-get (car results) :peer)))))
    (jabber-db-test-teardown)))

;;; ---- Group 5: Dedup and last-timestamp ----

(ert-deftest jabber-db-test-dedup-stanza-id ()
  "Inserting a message with a duplicate stanza_id is silently ignored."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((ts (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "First" ts nil "dup-id-123")
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "Duplicate" (1+ ts) nil "dup-id-123")
        (let ((rows (jabber-db-query "me@example.com" "friend@example.com")))
          (should (= 1 (length rows)))
          (should (string= "First" (plist-get (car rows) :body)))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-last-timestamp ()
  "last-timestamp returns the latest timestamp for a peer."
  (jabber-db-test-setup)
  (unwind-protect
      (let ((now (floor (float-time))))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "Old" (- now 100))
        (jabber-db-store-message
         "me@example.com" "friend@example.com" "in" "chat"
         "New" now)
        (should (= now (jabber-db-last-timestamp
                        "me@example.com" "friend@example.com"))))
    (jabber-db-test-teardown)))

;;; ---- Group 6: Empty database ----

(ert-deftest jabber-db-test-empty-backlog ()
  "Backlog returns nil on an empty database."
  (jabber-db-test-setup)
  (unwind-protect
      (progn
        (jabber-db-ensure-open)
        (should (null (jabber-db-backlog
                       "me@example.com" "friend@example.com"))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-empty-search ()
  "Search returns nil on an empty database."
  (jabber-db-test-setup)
  (unwind-protect
      (progn
        (jabber-db-ensure-open)
        (should (null (jabber-db-search "me@example.com" "anything"))))
    (jabber-db-test-teardown)))

(ert-deftest jabber-db-test-empty-last-timestamp ()
  "last-timestamp returns nil when no messages exist."
  (jabber-db-test-setup)
  (unwind-protect
      (progn
        (jabber-db-ensure-open)
        (should (null (jabber-db-last-timestamp
                       "me@example.com" "friend@example.com"))))
    (jabber-db-test-teardown)))

;;; ---- Group 7: Import from history ----

(ert-deftest jabber-db-test-import-history ()
  "Importing from flat-file history populates the database."
  (jabber-db-test-setup)
  (unwind-protect
      (let* ((jabber-use-global-history nil)
             (jabber-history-dir
              (expand-file-name "history" jabber-db-test-temp-dir))
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
          (should (string= "out" (plist-get (cadr rows) :direction)))))
    (jabber-db-test-teardown)))

(provide 'jabber-db-tests)

;;; jabber-db-tests.el ends here
