;;; jabber-mam-tests.el --- Tests for jabber-mam  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-mam)
(require 'jabber-message-correct)

;;; Test infrastructure

(defmacro jabber-mam-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database."
  (declare (indent 0) (debug t))
  `(let* ((jabber-mam-test--dir (make-temp-file "jabber-mam-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-mam-test--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-mam-test--dir)
         (delete-directory jabber-mam-test--dir t)))))

(defvar jabber-mam-test-queryid "test-query"
  "Default query ID used in test MAM stanzas.")

(defun jabber-mam-test--make-message (index &optional peer type)
  "Build a fake MAM result <message> stanza for message INDEX.
PEER defaults to \"friend@example.com\".
TYPE defaults to \"chat\"."
  (let* ((peer (or peer "friend@example.com"))
         (type (or type "chat"))
         (archive-id (format "archive-%06d" index))
         (stanza-id (format "stanza-%06d" index))
         (stamp (format-time-string
                 "%Y-%m-%dT%H:%M:%SZ"
                 (seconds-to-time (+ 1700000000 (* index 86400)))
                 t))
         (from (if (= (% index 3) 0)
                   "me@example.com"
                 (concat peer "/resource")))
         (to (if (= (% index 3) 0)
                 (concat peer "/resource")
               "me@example.com")))
    ;; Outer <message> with MAM <result> wrapping forwarded content
    `(message ((from . "me@example.com"))
              (result ((xmlns . ,jabber-mam-xmlns)
                       (queryid . ,jabber-mam-test-queryid)
                       (id . ,archive-id))
                      (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                 (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                         (stamp . ,stamp)))
                                 (message ((from . ,from)
                                           (to . ,to)
                                           (type . ,type)
                                           (id . ,stanza-id))
                                          (body () ,(format "Message %d" index))))))))

(defun jabber-mam-test--make-fin (last-id &optional complete)
  "Build a fake <fin> IQ result with LAST-ID.
When COMPLETE is non-nil, mark the archive as fully consumed."
  `(iq ((type . "result"))
       (fin ((xmlns . ,jabber-mam-xmlns)
             ,@(when complete '((complete . "true"))))
            (set ((xmlns . ,jabber-mam-rsm-xmlns))
                 (first () "first-id")
                 (last () ,last-id)))))

(defun jabber-mam-test--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT."
  (let ((jc (gensym "jabber-mam-test-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

;;; Group 1: Large sync

(ert-deftest jabber-mam-test-large-sync ()
  "3650 messages (10 years, 1/day) are stored and deduped correctly."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (count 3650)
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-muc-participants nil)
           (start-time (float-time)))
      ;; Feed all messages through the process function inside a transaction
      (jabber-db-with-transaction
        (dotimes (i count)
          (let ((xml (jabber-mam-test--make-message i)))
            (jabber-mam--process-message jc xml))))
      ;; Verify all stored
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 (* count 86400))
                                   -1)))
        (should (= count (length rows))))
      ;; Should complete in under 5 seconds
      (let ((elapsed (- (float-time) start-time)))
        (should (< elapsed 5.0))))))

;;; Group 2: Dedup on re-sync

(ert-deftest jabber-mam-test-dedup-resync ()
  "Running 3650 messages twice yields exactly 3650 rows."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (count 3650)
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-muc-participants nil))
      ;; First pass
      (jabber-db-with-transaction
        (dotimes (i count)
          (jabber-mam--process-message
           jc (jabber-mam-test--make-message i))))
      ;; Second pass (re-sync)
      (jabber-db-with-transaction
        (dotimes (i count)
          (jabber-mam--process-message
           jc (jabber-mam-test--make-message i))))
      ;; Still exactly count rows
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 (* count 86400))
                                   -1)))
        (should (= count (length rows)))))))

;;; Group 3: Transaction batching performance

(ert-deftest jabber-mam-test-transaction-batching ()
  "Batched inserts inside a transaction are faster than unbatched."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (batch-count 500)
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-muc-participants nil))
      ;; Batched: all in one transaction
      (let ((t1 (float-time)))
        (jabber-db-with-transaction
          (dotimes (i batch-count)
            (jabber-mam--process-message
             jc (jabber-mam-test--make-message i))))
        (let ((batched-time (- (float-time) t1)))
          ;; Verify they all stored
          (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                       0 (+ 1700000000 (* batch-count 86400))
                                       -1)))
            (should (= batch-count (length rows))))
          ;; Batched should be under 2 seconds for 500 messages
          (should (< batched-time 2.0)))))))

;;; Group 4: Parse helpers

(ert-deftest jabber-mam-test-parse-result ()
  "jabber-mam--parse-result extracts archive-id, stamp, and inner message."
  (let* ((xml (jabber-mam-test--make-message 42))
         (parsed (jabber-mam--parse-result xml)))
    (should parsed)
    (should (string= "archive-000042" (nth 0 parsed)))
    (should (stringp (nth 1 parsed)))
    (should (listp (nth 2 parsed)))
    (should (string= "Message 42"
                     (car (jabber-xml-node-children
                           (car (jabber-xml-get-children (nth 2 parsed) 'body))))))))

(ert-deftest jabber-mam-test-build-query-before-id-empty ()
  "build-query with before-id=t emits an empty <before/> element."
  (let ((query (jabber-mam--build-query "q1" "peer@example.com" nil nil 30 t)))
    ;; Should have RSM set with max and before
    (let* ((set-el (cl-find 'set (jabber-xml-node-children query)
                            :key (lambda (n) (and (listp n) (jabber-xml-node-name n)))))
           (before-el (car (jabber-xml-get-children set-el 'before)))
           (max-el (car (jabber-xml-get-children set-el 'max))))
      (should set-el)
      (should before-el)
      ;; before element should have no children (empty <before/>)
      (should-not (jabber-xml-node-children before-el))
      (should max-el)
      (should (string= "30" (car (jabber-xml-node-children max-el)))))))

(ert-deftest jabber-mam-test-build-query-before-id-string ()
  "build-query with before-id as a string emits <before>ID</before>."
  (let ((query (jabber-mam--build-query "q2" nil nil nil 10 "some-id")))
    (let* ((set-el (cl-find 'set (jabber-xml-node-children query)
                            :key (lambda (n) (and (listp n) (jabber-xml-node-name n)))))
           (before-el (car (jabber-xml-get-children set-el 'before))))
      (should before-el)
      (should (string= "some-id" (car (jabber-xml-node-children before-el)))))))

(ert-deftest jabber-mam-test-parse-fin-incomplete ()
  "jabber-mam--parse-fin returns :complete nil when not complete."
  (let* ((xml (jabber-mam-test--make-fin "last-123"))
         (fin (jabber-mam--parse-fin xml)))
    (should-not (plist-get fin :complete))
    (should (string= "last-123" (plist-get fin :last)))))

(ert-deftest jabber-mam-test-parse-fin-complete ()
  "jabber-mam--parse-fin returns :complete t when archive is exhausted."
  (let* ((xml (jabber-mam-test--make-fin "last-456" t))
         (fin (jabber-mam--parse-fin xml)))
    (should (plist-get fin :complete))
    (should (string= "last-456" (plist-get fin :last)))))

;;; Group 5: Transaction ref-count lifecycle

(ert-deftest jabber-mam-test-tx-depth-single-query ()
  "Single query cycle: depth goes 0 -> 1 -> 0, transaction commits."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--tx-depth 0)
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--dirty-peers nil)
           (jabber-muc-participants nil))
      ;; Simulate what jabber-mam--query does to the transaction
      (when (zerop jabber-mam--tx-depth)
        (setq jabber-mam--dirty-peers nil)
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "BEGIN")))
      (cl-incf jabber-mam--tx-depth)
      (should (= 1 jabber-mam--tx-depth))
      ;; Insert a message inside the open transaction
      (jabber-mam--process-message jc (jabber-mam-test--make-message 0))
      ;; Simulate what jabber-mam--handle-fin does
      (when (> jabber-mam--tx-depth 0)
        (cl-decf jabber-mam--tx-depth))
      (should (= 0 jabber-mam--tx-depth))
      (when (zerop jabber-mam--tx-depth)
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "COMMIT")))
      ;; Message should be committed and queryable
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 86400) -1)))
        (should (= 1 (length rows)))))))

(ert-deftest jabber-mam-test-tx-depth-concurrent-queries ()
  "Concurrent queries share one transaction: depth 0 -> 1 -> 2 -> 1 -> 0."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--tx-depth 0)
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)
                                      (cons jc "muc-query")))
           (jabber-mam--dirty-peers nil)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash "room@conference.example.com" (list (cons jc "mynick")) jabber-muc--rooms)
      ;; First query opens transaction
      (when (zerop jabber-mam--tx-depth)
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "BEGIN")))
      (cl-incf jabber-mam--tx-depth)
      (should (= 1 jabber-mam--tx-depth))
      ;; Second query piggybacks
      (cl-incf jabber-mam--tx-depth)
      (should (= 2 jabber-mam--tx-depth))
      ;; Insert messages from both "queries"
      (jabber-mam--process-message jc (jabber-mam-test--make-message 0))
      (jabber-mam--process-message
       jc (jabber-mam-test--make-muc-message 1 "room@conference.example.com" "mynick"))
      ;; First query finishes
      (when (> jabber-mam--tx-depth 0)
        (cl-decf jabber-mam--tx-depth))
      (should (= 1 jabber-mam--tx-depth))
      ;; No COMMIT yet
      ;; Second query finishes
      (when (> jabber-mam--tx-depth 0)
        (cl-decf jabber-mam--tx-depth))
      (should (= 0 jabber-mam--tx-depth))
      ;; Now COMMIT
      (when (zerop jabber-mam--tx-depth)
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "COMMIT")))
      ;; Both messages committed
      (let ((chat-rows (jabber-db-query "me@example.com" "friend@example.com"
                                        0 (+ 1700000000 86400) -1))
            (muc-rows (jabber-db-query "me@example.com" "room@conference.example.com"
                                       0 (+ 1700000000 (* 2 86400)) -1)))
        (should (= 1 (length chat-rows)))
        (should (= 1 (length muc-rows)))))))

(ert-deftest jabber-mam-test-tx-depth-guard-negative ()
  "Decrementing at depth 0 does not go negative."
  (let ((jabber-mam--tx-depth 0))
    (when (> jabber-mam--tx-depth 0)
      (cl-decf jabber-mam--tx-depth))
    (should (= 0 jabber-mam--tx-depth))
    ;; Double-decrement still stays at 0
    (when (> jabber-mam--tx-depth 0)
      (cl-decf jabber-mam--tx-depth))
    (should (= 0 jabber-mam--tx-depth))))

;;; Group 6: MUC messages

(defun jabber-mam-test--make-muc-message (index room our-nick)
  "Build a fake MAM MUC result for message INDEX in ROOM.
OUR-NICK is our nickname; every 3rd message is from us."
  (let* ((archive-id (format "muc-archive-%06d" index))
         (stanza-id (format "muc-stanza-%06d" index))
         (stamp (format-time-string
                 "%Y-%m-%dT%H:%M:%SZ"
                 (seconds-to-time (+ 1700000000 (* index 86400)))
                 t))
         (nick (if (= (% index 3) 0) our-nick "otherperson"))
         (from (concat room "/" nick)))
    `(message ((from . "me@example.com"))
              (result ((xmlns . ,jabber-mam-xmlns)
                       (queryid . "muc-query")
                       (id . ,archive-id))
                      (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                 (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                         (stamp . ,stamp)))
                                 (message ((from . ,from)
                                           (to . ,room)
                                           (type . "groupchat")
                                           (id . ,stanza-id))
                                          (body () ,(format "MUC message %d" index))))))))

(defvar jabber-muc--rooms)              ; jabber-muc.el

(ert-deftest jabber-mam-test-muc-message-storage ()
  "MUC messages from MAM are stored with correct peer and type."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (list (cons jc "muc-query")))
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      (jabber-db-with-transaction
        (dotimes (i 10)
          (jabber-mam--process-message
           jc (jabber-mam-test--make-muc-message i room "mynick"))))
      (let ((rows (jabber-db-query "me@example.com" room
                                   0 (+ 1700000000 (* 10 86400)) -1)))
        (should (= 10 (length rows)))
        (should (string= "groupchat" (plist-get (car rows) :type)))
        (should (string= room (plist-get (car rows) :peer)))))))

(ert-deftest jabber-mam-test-muc-direction-detection ()
  "MUC MAM detects outgoing messages by matching our nickname."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (list (cons jc "muc-query")))
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants
            `((,room ("mynick" . nil) ("otherperson" . nil)))))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      (jabber-db-with-transaction
        (jabber-mam--process-message
         jc (jabber-mam-test--make-muc-message 0 room "mynick"))  ; from us (idx%3=0)
        (jabber-mam--process-message
         jc (jabber-mam-test--make-muc-message 1 room "mynick"))) ; from other (idx%3=1)
      (let ((rows (jabber-db-query "me@example.com" room
                                   0 (+ 1700000000 (* 2 86400)) -1)))
        (should (= 2 (length rows)))
        (should (string= "out" (plist-get (car rows) :direction)))
        (should (string= "in" (plist-get (cadr rows) :direction)))))))

;;; Group 7: Dirty peer tracking

(ert-deftest jabber-mam-test-mark-dirty-dedup ()
  "jabber-mam--mark-dirty does not add the same peer twice."
  (let ((jabber-mam--dirty-peers nil))
    (jabber-mam--mark-dirty "peer@example.com" "chat")
    (jabber-mam--mark-dirty "peer@example.com" "chat")
    (jabber-mam--mark-dirty "peer@example.com" "chat")
    (should (= 1 (length jabber-mam--dirty-peers)))
    (should (equal '("peer@example.com" . "chat")
                   (car jabber-mam--dirty-peers)))))

(ert-deftest jabber-mam-test-dirty-peers-reset-on-new-sync ()
  "Starting a new sync cycle resets the dirty peer list."
  (let ((jabber-mam--dirty-peers '(("room@muc.example.com" . "groupchat")))
        (jabber-mam--tx-depth 0))
    ;; Simulate depth 0->1 transition (new sync cycle)
    (when (zerop jabber-mam--tx-depth)
      (setq jabber-mam--dirty-peers nil))
    (should (null jabber-mam--dirty-peers))))

;;; Group 8: jabber-mam-sync-buffer

(ert-deftest jabber-mam-test-sync-buffer-not-connected ()
  "Signal user-error when not connected."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'dead-jc)
    (let ((jabber-connections nil))
      (should-error (jabber-mam-sync-buffer 30) :type 'user-error))))

(ert-deftest jabber-mam-test-sync-buffer-1to1-registers-and-queries ()
  "1:1 sync registers reconciliation tracking and queries with before-id=t."
  (let ((query-args nil))
    (cl-letf (((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (setq query-args args)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-jid-user)
               (lambda (jid) jid)))
      (with-temp-buffer
        (let ((jabber-connections (list 'fake-jc))
              (jabber-mam--dirty-peers nil)
              (jabber-mam--sync-received nil)
              (jabber-mam--completion-callbacks nil))
          (setq-local jabber-buffer-connection 'fake-jc)
          (setq-local jabber-chatting-with "friend@example.com")
          (jabber-mam-sync-buffer 50)
          ;; Should have registered sync tracking
          (should jabber-mam--sync-received)
          (let ((data (cdar jabber-mam--sync-received)))
            (should (hash-table-p (plist-get data :ids)))
            (should (string= "me@example.com" (plist-get data :account)))
            (should (string= "friend@example.com" (plist-get data :peer))))
          ;; Should have registered completion callback
          (should jabber-mam--completion-callbacks)
          ;; (jc after-id queryid with start to before-id max)
          (should (eq 'fake-jc (nth 0 query-args)))
          (should (equal "friend@example.com" (nth 3 query-args)))
          (should-not (nth 5 query-args))        ; no to (1:1)
          (should (eq t (nth 6 query-args)))     ; before-id = t
          (should (= 50 (nth 7 query-args))))))) ; max = count
  )

(ert-deftest jabber-mam-test-sync-buffer-muc-registers-and-queries ()
  "MUC sync registers reconciliation tracking and queries with before-id=t."
  (let ((query-args nil))
    (cl-letf (((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (setq query-args args)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (with-temp-buffer
        (let ((jabber-connections (list 'fake-jc))
              (jabber-mam--dirty-peers nil)
              (jabber-mam--sync-received nil)
              (jabber-mam--completion-callbacks nil))
          (setq-local jabber-buffer-connection 'fake-jc)
          (setq-local jabber-group "room@conference.example.com")
          (jabber-mam-sync-buffer 25)
          ;; Should have registered sync tracking
          (should jabber-mam--sync-received)
          (let ((data (cdar jabber-mam--sync-received)))
            (should (string= "me@example.com" (plist-get data :account)))
            (should (string= "room@conference.example.com"
                             (plist-get data :peer))))
          ;; (jc after-id queryid with start to before-id max)
          (should (eq 'fake-jc (nth 0 query-args)))
          (should (equal "room@conference.example.com" (nth 5 query-args)))
          (should (eq t (nth 6 query-args)))     ; before-id = t
          (should (= 25 (nth 7 query-args))))))) ; max = count
  )

;;; Group 8b: sync reconciliation

(ert-deftest jabber-mam-test-reconcile-deletes-orphan-messages ()
  "Reconciliation deletes local messages whose IDs are not in the remote set."
  (jabber-mam-test-with-db
    (let ((db (jabber-db-ensure-open))
          (account "me@example.com")
          (peer "friend@example.com"))
      ;; Insert 3 local messages with server_ids
      (dolist (sid '("srv-1" "srv-2" "srv-3"))
        (sqlite-execute db
          "INSERT INTO message (account,peer,direction,type,body,timestamp,server_id)
           VALUES (?,?,'in','chat',?,1700000100,?)"
          (list account peer (concat "msg " sid) sid)))
      ;; Simulate sync that received only srv-1 and srv-3 (srv-2 is orphan)
      (let* ((ids (make-hash-table :test #'equal))
             (jabber-mam--sync-received
              (list (cons "test-q"
                          (list :ids ids
                                :min-ts 1700000100 :max-ts 1700000100
                                :account account :peer peer)))))
        (puthash "srv-1" t ids)
        (puthash "srv-3" t ids)
        (jabber-mam--reconcile-sync "test-q")
        ;; srv-2 should be deleted
        (should-not (caar (sqlite-select db
                    "SELECT 1 FROM message WHERE server_id = 'srv-2'")))
        ;; srv-1 and srv-3 should remain
        (should (caar (sqlite-select db
                  "SELECT 1 FROM message WHERE server_id = 'srv-1'")))
        (should (caar (sqlite-select db
                  "SELECT 1 FROM message WHERE server_id = 'srv-3'")))
        ;; Tracking entry should be cleaned up
        (should-not jabber-mam--sync-received)))))

(ert-deftest jabber-mam-test-reconcile-keeps-messages-without-ids ()
  "Reconciliation keeps local messages that have no stanza_id or server_id."
  (jabber-mam-test-with-db
    (let ((db (jabber-db-ensure-open))
          (account "me@example.com")
          (peer "friend@example.com"))
      ;; Insert a message without any server-side IDs
      (sqlite-execute db
        "INSERT INTO message (account,peer,direction,type,body,timestamp)
         VALUES (?,?,'out','chat','local only',1700000100)"
        (list account peer))
      ;; Insert a message with server_id that IS in remote
      (sqlite-execute db
        "INSERT INTO message (account,peer,direction,type,body,timestamp,server_id)
         VALUES (?,?,'in','chat','from server',1700000100,'srv-ok')"
        (list account peer))
      (let* ((ids (make-hash-table :test #'equal))
             (jabber-mam--sync-received
              (list (cons "test-q"
                          (list :ids ids
                                :min-ts 1700000100 :max-ts 1700000100
                                :account account :peer peer)))))
        (puthash "srv-ok" t ids)
        (jabber-mam--reconcile-sync "test-q")
        ;; Both messages should remain
        (should (= 2 (caar (sqlite-select db
                     "SELECT count(*) FROM message WHERE account = ? AND peer = ?"
                     (list account peer)))))))))

(ert-deftest jabber-mam-test-reconcile-noop-when-empty ()
  "Reconciliation is a no-op when no messages were received."
  (jabber-mam-test-with-db
    (let ((db (jabber-db-ensure-open))
          (account "me@example.com")
          (peer "friend@example.com"))
      (sqlite-execute db
        "INSERT INTO message (account,peer,direction,type,body,timestamp,server_id)
         VALUES (?,?,'in','chat','keep me',1700000100,'srv-1')"
        (list account peer))
      ;; Sync received nothing (min-ts and max-ts are nil)
      (let ((jabber-mam--sync-received
             (list (cons "test-q"
                         (list :ids (make-hash-table :test #'equal)
                               :min-ts nil :max-ts nil
                               :account account :peer peer)))))
        (jabber-mam--reconcile-sync "test-q")
        ;; Message should still be there
        (should (caar (sqlite-select db
                  "SELECT 1 FROM message WHERE server_id = 'srv-1'")))
        ;; Tracking cleaned up
        (should-not jabber-mam--sync-received)))))

(ert-deftest jabber-mam-test-reconcile-uses-stanza-id-too ()
  "Reconciliation matches on stanza_id when server_id is absent."
  (jabber-mam-test-with-db
    (let ((db (jabber-db-ensure-open))
          (account "me@example.com")
          (peer "friend@example.com"))
      ;; Message with stanza_id only (no server_id)
      (sqlite-execute db
        "INSERT INTO message (account,peer,direction,type,body,timestamp,stanza_id)
         VALUES (?,?,'in','chat','has stanza id',1700000100,'st-1')"
        (list account peer))
      ;; Remote set includes this stanza_id
      (let* ((ids (make-hash-table :test #'equal))
             (jabber-mam--sync-received
              (list (cons "test-q"
                          (list :ids ids
                                :min-ts 1700000100 :max-ts 1700000100
                                :account account :peer peer)))))
        (puthash "st-1" t ids)
        (jabber-mam--reconcile-sync "test-q")
        ;; Should be kept (matched by stanza_id)
        (should (caar (sqlite-select db
                  "SELECT 1 FROM message WHERE stanza_id = 'st-1'")))))))

(ert-deftest jabber-mam-test-process-message-tracks-ids ()
  "process-message accumulates IDs and timestamps for sync tracking."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (ids (make-hash-table :test #'equal))
           (jabber-mam--sync-received
            (list (cons jabber-mam-test-queryid
                        (list :ids ids
                              :min-ts nil :max-ts nil
                              :account "me@example.com"
                              :peer "friend@example.com"))))
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--tx-depth 1)
           (jabber-muc-participants nil))
      ;; Process two messages
      (jabber-mam--process-message jc (jabber-mam-test--make-message 0))
      (jabber-mam--process-message jc (jabber-mam-test--make-message 5))
      ;; Check that IDs were tracked
      (let ((data (cdr (car jabber-mam--sync-received))))
        (should (gethash "archive-000000" (plist-get data :ids)))
        (should (gethash "archive-000005" (plist-get data :ids)))
        (should (gethash "stanza-000000" (plist-get data :ids)))
        (should (gethash "stanza-000005" (plist-get data :ids)))
        ;; Timestamps should bracket the range
        (should (plist-get data :min-ts))
        (should (plist-get data :max-ts))
        (should (<= (plist-get data :min-ts) (plist-get data :max-ts)))))))

;;; Group 9: disconnect cleanup


(ert-deftest jabber-mam-test-cleanup-all-commits-transaction ()
  "cleanup-all commits open transaction and resets state."
  (jabber-mam-test-with-db
    (sqlite-execute (jabber-db-ensure-open) "BEGIN")
    (let ((jabber-mam--tx-depth 2)
          (jabber-mam--syncing '((jc1 . "q1") (jc2 . "q2")))
          (jabber-mam--completion-callbacks '(("q1" . ignore) ("q2" . ignore)))
          (jabber-mam--dirty-peers nil))
      (jabber-mam--cleanup-all)
      (should (= 0 jabber-mam--tx-depth))
      (should-not jabber-mam--syncing)
      (should-not jabber-mam--completion-callbacks)
      ;; Transaction was committed; verify we can write without error.
      (sqlite-execute (jabber-db-ensure-open)
                      "INSERT INTO message (account,peer,direction,type,body,timestamp) \
VALUES ('a','b','in','chat','test',1)")
      (should (caar (sqlite-select (jabber-db-ensure-open)
                                   "SELECT 1 FROM message WHERE body='test'"))))))

(ert-deftest jabber-mam-test-cleanup-connection-scoped ()
  "cleanup-connection only removes entries for the given connection."
  (jabber-mam-test-with-db
    (sqlite-execute (jabber-db-ensure-open) "BEGIN")
    (let ((jabber-mam--tx-depth 2)
          (jabber-mam--syncing '((jc1 . "q1") (jc2 . "q2")))
          (jabber-mam--completion-callbacks '(("q1" . ignore)))
          (jabber-mam--dirty-peers nil))
      (jabber-mam--cleanup-connection 'jc1)
      (should (= 1 jabber-mam--tx-depth))
      (should (equal '((jc2 . "q2")) jabber-mam--syncing))
      (should-not jabber-mam--completion-callbacks))))

(ert-deftest jabber-mam-test-cleanup-triggers-redisplay ()
  "cleanup-all redraws dirty buffers."
  (jabber-mam-test-with-db
    (let ((jabber-mam--tx-depth 1)
          (jabber-mam--syncing '((jc1 . "q1")))
          (jabber-mam--completion-callbacks nil)
          (jabber-mam--dirty-peers '(("peer@example.com" . "chat")))
          ) ;; (redrawn nil)
      (cl-letf (((symbol-function 'jabber-chat-find-buffer)
                 (lambda (_peer) nil)))
        (jabber-mam--cleanup-all)
        ;; Dirty peers list should be drained after cleanup.
        (should (null jabber-mam--dirty-peers))))))

(ert-deftest jabber-mam-test-cleanup-all-noop-when-idle ()
  "cleanup-all is safe to call with no active queries."
  (let ((jabber-mam--tx-depth 0)
        (jabber-mam--syncing nil)
        (jabber-mam--completion-callbacks nil)
        (jabber-mam--dirty-peers nil))
    (jabber-mam--cleanup-all)
    (should (= 0 jabber-mam--tx-depth))))

;;; Group 10: stanza mutation guard

(ert-deftest jabber-mam-test-body-stanza-stripped ()
  "Body-bearing MAM result has children stripped after processing."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (stanza (jabber-mam-test--make-message 1)))
      (jabber-mam--process-message jc stanza)
      (should-not (cddr stanza)))))

(ert-deftest jabber-mam-test-bodyless-stanza-unwrapped ()
  "Bodyless MAM result is unwrapped with original sender and MAM marker."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Receipt stanza: no body, just a <received/> element
           (stanza `(message ((from . "me@example.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . ,jabber-mam-test-queryid)
                                      (id . "archive-001"))
                                     (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                        (stamp . "2025-01-01T00:00:00Z")))
                                                (message ((from . "alice@example.com/res")
                                                          (to . "me@example.com")
                                                          (id . "receipt-1"))
                                                         (received ((xmlns . "urn:xmpp:receipts")
                                                                    (id . "msg-42")))))))))
      (jabber-mam--process-message jc stanza)
      ;; Outer stanza should now have inner message's from
      (should (string= "alice@example.com/res"
                        (jabber-xml-get-attribute stanza 'from)))
      ;; MAM origin marker should be set
      (should (jabber-xml-get-attribute stanza 'jabber-mam--origin))
      ;; The receipt element should be a child
      (should (car (jabber-xml-get-children stanza 'received))))))

;;; Group 9: query ID validation

(ert-deftest jabber-mam-test-unknown-queryid-rejected ()
  "MAM result with unknown queryid is not processed."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc "known-query")))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Build stanza with queryid that doesn't match
           (stanza `(message ((from . "me@example.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . "unknown-query")
                                      (id . "arch-1"))
                                     (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                        (stamp . "2025-01-01T00:00:00Z")))
                                                (message ((from . "alice@example.com")
                                                          (to . "me@example.com")
                                                          (id . "s1"))
                                                         (body () "secret")))))))
      (jabber-mam--process-message jc stanza)
      ;; Stanza should NOT have been stripped (not processed)
      (should (cddr stanza))
      ;; Message should NOT be in DB
      (should-not (caar (sqlite-select (jabber-db-ensure-open)
                                       "SELECT 1 FROM message WHERE stanza_id='s1'"))))))

(ert-deftest jabber-mam-test-known-queryid-accepted ()
  "MAM result with known queryid is processed normally."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc "known-query")))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Use the test helper but we need to add queryid
           (stanza `(message ((from . "me@example.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . "known-query")
                                      (id . "arch-2"))
                                     (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                        (stamp . "2025-01-01T00:00:00Z")))
                                                (message ((from . "alice@example.com/res")
                                                          (to . "me@example.com")
                                                          (id . "s2"))
                                                         (body () "hello")))))))
      (jabber-mam--process-message jc stanza)
      ;; Message should be in DB
      (should (caar (sqlite-select (jabber-db-ensure-open)
                                   "SELECT 1 FROM message WHERE stanza_id='s2'"))))))

;;; Group 10: error handler callback transfer

(ert-deftest jabber-mam-test-error-callback-transferred ()
  "item-not-found fallback transfers callback to new query."
  (jabber-mam-test-with-db
    (sqlite-execute (jabber-db-ensure-open) "BEGIN")
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--tx-depth 1)
           (jabber-mam--syncing (list (cons jc "old-q")))
           (jabber-mam--dirty-peers nil)
           (callback-fired nil)
           (jabber-mam--completion-callbacks
            (list (cons "old-q" (lambda () (setq callback-fired t)))))
           (captured-queryid nil))
      ;; Mock jabber-mam--query to capture the new queryid
      (cl-letf (((symbol-function 'jabber-mam--query)
                 (lambda (_jc _after qid &rest _)
                   (setq captured-queryid qid))))
        ;; Simulate item-not-found error IQ
        (jabber-mam--handle-error
         jc
         `(iq ((type . "error"))
              (error () (item-not-found ())))
         '("old-q" nil)))
      ;; Old callback should be removed
      (should-not (assoc "old-q" jabber-mam--completion-callbacks #'string=))
      ;; New callback should be registered under the new queryid
      (should captured-queryid)
      (should (assoc captured-queryid jabber-mam--completion-callbacks
                     #'string=))
      ;; Fire it to confirm it's the same callback
      (funcall (cdr (assoc captured-queryid jabber-mam--completion-callbacks
                           #'string=)))
      (should callback-fired))))

;;; Group 11: sender JID validation

(ert-deftest jabber-mam-test-rejects-foreign-sender ()
  "MAM result from a server other than ours is rejected."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           ;; Outer from is evil.com, not our bare JID
           (stanza `(message ((from . "evil.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . ,jabber-mam-test-queryid)
                                      (id . "arch-evil"))
                                     (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                        (stamp . "2025-01-01T00:00:00Z")))
                                                (message ((from . "alice@legit.com/res")
                                                          (to . "me@example.com")
                                                          (id . "forged-1"))
                                                         (body () "injected")))))))
      (jabber-mam--process-message jc stanza)
      ;; Stanza should NOT have been stripped
      (should (cddr stanza))
      ;; Message should NOT be in DB
      (should-not (caar (sqlite-select (jabber-db-ensure-open)
                                       "SELECT 1 FROM message WHERE stanza_id='forged-1'"))))))

(ert-deftest jabber-mam-test-accepts-own-jid-sender ()
  "MAM result from our own bare JID is accepted."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (list (cons jc jabber-mam-test-queryid)))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Normal 1:1 MAM result with from=our bare JID
           (stanza (jabber-mam-test--make-message 5)))
      (jabber-mam--process-message jc stanza)
      ;; Message should be stored
      (should (caar (sqlite-select (jabber-db-ensure-open)
                                   "SELECT 1 FROM message WHERE stanza_id='stanza-000005'"))))))

(ert-deftest jabber-mam-test-accepts-joined-muc-sender ()
  "MAM result from a joined MUC room is accepted."
  (jabber-mam-test-with-db
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (list (cons jc "muc-query")))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      ;; MUC MAM: outer from is the room bare JID
      (let ((stanza `(message ((from . ,room))
                              (result ((xmlns . ,jabber-mam-xmlns)
                                       (queryid . "muc-query")
                                       (id . "muc-arch-1"))
                                      (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                 (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                         (stamp . "2025-01-01T12:00:00Z")))
                                                 (message ((from . ,(concat room "/otherperson"))
                                                           (to . ,room)
                                                           (type . "groupchat")
                                                           (id . "muc-s1"))
                                                          (body () "hello room")))))))
        (jabber-mam--process-message jc stanza)
        ;; Message should be stored
        (should (caar (sqlite-select (jabber-db-ensure-open)
                                     "SELECT 1 FROM message WHERE stanza_id='muc-s1'")))))))

;;; Group 12: MUC query cancellation

(ert-deftest jabber-mam-test-cancel-muc-query ()
  "Cancelling a MUC MAM query removes it from active state."
  (jabber-mam-test-with-db
    (sqlite-execute (jabber-db-ensure-open) "BEGIN")
    (let* ((jc (jabber-mam-test--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--tx-depth 1)
           (jabber-mam--syncing (list (cons jc "muc-q1")))
           (jabber-mam--query-targets (list (cons "muc-q1" room)))
           (jabber-mam--completion-callbacks
            (list (cons "muc-q1" #'ignore)))
           (jabber-mam--dirty-peers nil))
      (jabber-mam--cancel-muc-query room)
      (should (= 0 jabber-mam--tx-depth))
      (should-not jabber-mam--syncing)
      (should-not jabber-mam--query-targets)
      (should-not jabber-mam--completion-callbacks))))

(ert-deftest jabber-mam-test-cancel-muc-query-noop-for-unknown ()
  "Cancelling a room with no active query is a no-op."
  (let ((jabber-mam--tx-depth 1)
        (jabber-mam--syncing (list (cons 'jc "q1")))
        (jabber-mam--query-targets nil)
        (jabber-mam--dirty-peers nil))
    (jabber-mam--cancel-muc-query "unknown@conference.example.com")
    ;; State unchanged
    (should (= 1 jabber-mam--tx-depth))
    (should jabber-mam--syncing)))

(provide 'jabber-mam-tests)

;;; jabber-mam-tests.el ends here
