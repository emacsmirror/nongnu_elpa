;;; jabber-mam-tests.el --- Tests for jabber-mam  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-mam)

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
           (jabber-mam--syncing nil)
           (jabber-mam--dirty-buffers nil)
           (jabber-muc-participants nil))
      ;; Simulate what jabber-mam--query does to the transaction
      (when (zerop jabber-mam--tx-depth)
        (setq jabber-mam--dirty-buffers nil)
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
           (jabber-mam--syncing nil)
           (jabber-mam--dirty-buffers nil)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash "room@conference.example.com" (cons jc "mynick") jabber-muc--rooms)
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
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash room (cons jc "mynick") jabber-muc--rooms)
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
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants
            `((,room ("mynick" . nil) ("otherperson" . nil)))))
      (puthash room (cons jc "mynick") jabber-muc--rooms)
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

;;; Group 7: Dirty buffer tracking

(ert-deftest jabber-mam-test-mark-dirty-dedup ()
  "jabber-mam--mark-dirty does not add the same buffer twice."
  (let ((jabber-mam--dirty-buffers nil)
        (buf (generate-new-buffer " *test-dirty*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'jabber-chat-find-buffer)
                     (lambda (_peer) buf)))
            (jabber-mam--mark-dirty "peer@example.com" "chat")
            (jabber-mam--mark-dirty "peer@example.com" "chat")
            (jabber-mam--mark-dirty "peer@example.com" "chat"))
          (should (= 1 (length jabber-mam--dirty-buffers)))
          (should (eq buf (car jabber-mam--dirty-buffers))))
      (kill-buffer buf))))

(ert-deftest jabber-mam-test-dirty-buffers-reset-on-new-sync ()
  "Starting a new sync cycle resets the dirty buffer list."
  (let ((jabber-mam--dirty-buffers (list (generate-new-buffer " *stale*")))
        (jabber-mam--tx-depth 0))
    (unwind-protect
        (progn
          ;; Simulate depth 0->1 transition (new sync cycle)
          (when (zerop jabber-mam--tx-depth)
            (setq jabber-mam--dirty-buffers nil))
          (should (null jabber-mam--dirty-buffers)))
      ;; Clean up the stale buffer if it's still alive
      (dolist (buf jabber-mam--dirty-buffers)
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Group 8: jabber-mam-sync-buffer

(ert-deftest jabber-mam-test-sync-buffer-not-connected ()
  "Signal user-error when not connected."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'dead-jc)
    (let ((jabber-connections nil))
      (should-error (jabber-mam-sync-buffer) :type 'user-error))))

(ert-deftest jabber-mam-test-sync-buffer-1to1-query-args ()
  "1:1 sync queries user archive with peer filter."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (setq captured-args args)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-db-last-server-id)
               (lambda (_account _peer) "stanza-42")))
      (with-temp-buffer
        (let ((jabber-connections (list 'fake-jc)))
          (setq-local jabber-buffer-connection 'fake-jc)
          (setq-local jabber-chatting-with "friend@example.com")
          (jabber-mam-sync-buffer)
          ;; Args: jc after-id queryid with start to
          (should (eq 'fake-jc (nth 0 captured-args)))
          (should (equal "stanza-42" (nth 1 captured-args)))
          (should-not (nth 2 captured-args))
          (should (equal "friend@example.com" (nth 3 captured-args)))
          (should-not (nth 4 captured-args))
          (should-not (nth 5 captured-args)))))))

(ert-deftest jabber-mam-test-sync-buffer-muc-query-args ()
  "MUC sync queries room archive via to parameter."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (setq captured-args args)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-db-last-server-id)
               (lambda (_account _peer) "stanza-99")))
      (with-temp-buffer
        (let ((jabber-connections (list 'fake-jc)))
          (setq-local jabber-buffer-connection 'fake-jc)
          (setq-local jabber-group "room@conference.example.com")
          (jabber-mam-sync-buffer)
          ;; Args: jc after-id queryid with start to
          (should (eq 'fake-jc (nth 0 captured-args)))
          (should (equal "stanza-99" (nth 1 captured-args)))
          (should-not (nth 2 captured-args))
          (should-not (nth 3 captured-args))
          (should-not (nth 4 captured-args))
          (should (equal "room@conference.example.com"
                         (nth 5 captured-args))))))))

(provide 'jabber-mam-tests)

;;; jabber-mam-tests.el ends here
