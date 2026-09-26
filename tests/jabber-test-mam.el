;;; jabber-test-mam.el --- Tests for jabber-mam  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0313 Message Archive Management.

;;; Code:

(require 'ert)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-chat)
(require 'jabber-muc)
(require 'jabber-mam)
(require 'jabber-core)
(require 'jabber-message-correct)
(require 'jabber-moderation)
(require 'jabber-omemo-store)

;;; Test infrastructure

(defmacro jabber-test-mam-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-mam--dir (make-temp-file "jabber-mam-test" t))
          (jabber-db-path (expand-file-name "test.sqlite" jabber-test-mam--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-test-mam--dir)
         (delete-directory jabber-test-mam--dir t)))))

(defvar jabber-test-mam-queryid "test-query"
  "Default query ID used in test MAM stanzas.")

(defun jabber-test-mam--make-message (index &optional peer type)
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
                       (queryid . ,jabber-test-mam-queryid)
                       (id . ,archive-id))
                      (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                 (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                         (stamp . ,stamp)))
                                 (message ((from . ,from)
                                           (to . ,to)
                                           (type . ,type)
                                           (id . ,stanza-id))
                                          (body () ,(format "Message %d" index))))))))

(defun jabber-test-mam--without-delay (stanza)
  "Return a copy of MAM result STANZA without its delay element."
  (let* ((copy (copy-tree stanza))
         (result (jabber-xml-child-with-xmlns copy jabber-mam-xmlns))
         (forwarded (car (jabber-xml-get-children result 'forwarded))))
    (setcdr (cdr forwarded)
            (cl-remove-if
             (lambda (child)
               (eq (jabber-xml-node-name child) 'delay))
             (jabber-xml-node-children forwarded)))
    copy))

(defun jabber-test-mam--make-correction-message
    (archive-id stanza-id replace-id from body)
  "Build a fake MAM correction stanza.
ARCHIVE-ID and STANZA-ID identify the archived correction.  REPLACE-ID is
the original message id.  FROM is the correction sender."
  `(message ((from . "me@example.com"))
            (result ((xmlns . ,jabber-mam-xmlns)
                     (queryid . ,jabber-test-mam-queryid)
                     (id . ,archive-id))
                    (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                               (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                       (stamp . "2025-01-01T00:00:00Z")))
                               (message ((from . ,from)
                                         (to . "me@example.com")
                                         (id . ,stanza-id))
                                        (body () ,body)
                                        (replace ((id . ,replace-id)
                                                  (xmlns . ,jabber-message-correct-xmlns))))))))

(defun jabber-test-mam--make-fin (last-id &optional complete)
  "Build a fake <fin> IQ result with LAST-ID.
When COMPLETE is non-nil, mark the archive as fully consumed."
  `(iq ((type . "result"))
       (fin ((xmlns . ,jabber-mam-xmlns)
             ,@(when complete '((complete . "true"))))
            (set ((xmlns . ,jabber-mam-rsm-xmlns))
                 (first () "first-id")
                 (last () ,last-id)))))

(defun jabber-test-mam--make-fake-jc (account)
  "Create a fake connection symbol for ACCOUNT."
  (let ((jc (gensym "jabber-test-mam-jc-"))
        (parts (split-string account "@")))
    (put jc :state-data (list :username (nth 0 parts)
                              :server (nth 1 parts)))
    jc))

(defun jabber-test-mam--queries (jc id)
  "Build an active message-only query fixture for JC and ID."
  (list (list :id id :jc jc :current-p (jabber-mam--session-predicate jc)
              :to nil :page (list nil) :transaction nil)))

;;; Group 0: Hook defaults

(ert-deftest jabber-test-mam-post-connect-hook-default ()
  "MAM catch-up is enabled for fresh connections by default."
  (should (memq 'jabber-mam-maybe-catchup jabber-post-connect-hooks))
  (should (memq 'jabber-mam-maybe-catchup
                (get 'jabber-post-connect-hooks 'custom-options)))
  (should (memq 'jabber-mam-maybe-catchup jabber-post-resume-hooks))
  (should (memq 'jabber-mam-maybe-catchup
                (get 'jabber-post-resume-hooks 'custom-options))))

(ert-deftest jabber-test-mam-chat-opened-coalesces-peer-sync ()
  "Repeated chat opens share one peer MAM catch-up."
  (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
         (peer "friend@example.com")
         (jabber-mam-enable t)
         (jabber-mam--peer-syncing nil)
         disco-requests
         query-requests)
    (cl-letf (((symbol-function 'jabber-disco-get-info)
               (lambda (_jc _jid _node callback closure &optional _force)
                 (push (cons callback closure) disco-requests)))
              ((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (push args query-requests))))
      (jabber-mam-chat-opened jc peer)
      (jabber-mam-chat-opened jc peer)
      (should (= 1 (length disco-requests)))
      (pcase-let ((`(,callback . ,closure) (car disco-requests)))
        (funcall callback jc closure (list nil (list jabber-mam-xmlns))))
      (should (= 1 (length query-requests)))
      (jabber-mam-chat-opened jc peer)
      (should (= 1 (length disco-requests)))
      (funcall (nth 8 (car query-requests)))
      (jabber-mam-chat-opened jc peer)
      (should (= 2 (length disco-requests))))))

(ert-deftest jabber-test-mam-stale-disco-callback-does-not-start-query ()
  "A disco callback from an abandoned peer sync cannot start a query."
  (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
         (peer "friend@example.com")
         (jabber-mam-enable t)
         (jabber-mam--peer-syncing nil)
         (jabber-mam--syncing nil)
         disco-requests
         query-requests)
    (cl-letf (((symbol-function 'jabber-disco-get-info)
               (lambda (_jc _jid _node callback closure &optional _force)
                 (push (cons callback closure) disco-requests)))
              ((symbol-function 'jabber-mam--query)
               (lambda (&rest args) (push args query-requests))))
      (jabber-mam-chat-opened jc peer)
      (let ((old-request (car disco-requests)))
        (jabber-mam--cleanup-connection jc)
        (jabber-mam-chat-opened jc peer)
        (pcase-let ((`(,callback . ,closure) old-request))
          (funcall callback jc closure (list nil (list jabber-mam-xmlns))))
        (should-not query-requests)
        (pcase-let ((`(,callback . ,closure) (car disco-requests)))
          (funcall callback jc closure (list nil (list jabber-mam-xmlns)))))
      (should (= 1 (length query-requests))))))

;;; Group 1: Large sync

(ert-deftest jabber-test-mam-large-sync ()
  "3650 messages (10 years, 1/day) are stored and deduped correctly."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (count 3650)
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (start-time (float-time)))
      ;; Feed all messages through the process function inside a transaction
      (jabber-db-with-transaction
        (dotimes (i count)
          (let ((xml (jabber-test-mam--make-message i)))
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

(ert-deftest jabber-test-mam-dedup-resync ()
  "Running 3650 messages twice yields exactly 3650 rows."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (count 3650)
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil))
      ;; First pass
      (jabber-db-with-transaction
        (dotimes (i count)
          (jabber-mam--process-message
           jc (jabber-test-mam--make-message i))))
      ;; Second pass (re-sync)
      (jabber-db-with-transaction
        (dotimes (i count)
          (jabber-mam--process-message
           jc (jabber-test-mam--make-message i))))
      ;; Still exactly count rows
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 (* count 86400))
                                   -1)))
        (should (= count (length rows)))))))

(ert-deftest jabber-test-mam-replay-without-delay-preserves-timestamp ()
  "A replay without a delay stamp does not make an old message new."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing
            (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (stanza (jabber-test-mam--make-message 1))
           (replay (jabber-test-mam--without-delay stanza)))
      (jabber-mam--process-message jc stanza)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (seconds-to-time 1800000000))))
        (jabber-mam--process-message jc replay))
      (should
       (equal '((1700086400))
              (sqlite-select jabber-db--connection
                             "SELECT timestamp FROM message"))))))

(ert-deftest jabber-test-mam-preserves-archived-thread-fields ()
  "A MAM result stores XEP-0201 metadata from the forwarded stanza."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (stanza (jabber-test-mam--make-message 1))
           (result (jabber-xml-child-with-xmlns stanza jabber-mam-xmlns))
           (forwarded (car (jabber-xml-get-children result 'forwarded)))
           (inner (car (jabber-xml-get-children forwarded 'message))))
      (nconc inner '((thread ((parent . "parent-1")) "thread-1")))
      (jabber-mam--process-message jc stanza)
      (let ((stored (car (jabber-db-thread-backlog
                          "me@example.com" "friend@example.com" "chat"
                          "thread-1" t))))
        (should (equal "thread-1" (plist-get stored :thread-id)))
        (should (equal "parent-1"
                       (plist-get stored :thread-parent-id)))))))

;;; Group 3: Transaction batching performance

(ert-deftest jabber-test-mam-transaction-batching ()
  "Batched inserts inside a transaction are faster than unbatched."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (batch-count 500)
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil))
      ;; Batched: all in one transaction
      (let ((t1 (float-time)))
        (jabber-db-with-transaction
          (dotimes (i batch-count)
            (jabber-mam--process-message
             jc (jabber-test-mam--make-message i))))
        (let ((batched-time (- (float-time) t1)))
          ;; Verify they all stored
          (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                       0 (+ 1700000000 (* batch-count 86400))
                                       -1)))
            (should (= batch-count (length rows))))
          ;; Batched should be under 2 seconds for 500 messages
          (should (< batched-time 2.0)))))))

(ert-deftest jabber-test-mam-encrypted-session-save-in-transaction ()
  "An encrypted MAM message can migrate its session inside the MAM transaction."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (stanza (jabber-test-mam--make-message 1))
           (inner (nth 2 (jabber-mam--parse-result stanza)))
           (envelope (unibyte-string ?J ?O ?M ?E ?M ?O 0 1 0))
           (body (car (jabber-xml-get-children inner 'body))))
      (setcdr (last inner) '((encrypted ((xmlns . "eu.siacs.conversations.axolotl")))))
      (sqlite-execute jabber-db--connection "\
INSERT INTO omemo_skipped_keys
  (account, jid, device_id, dh_key, message_number, message_key, created_at)
  VALUES ('me@example.com', 'friend@example.com', 7, 'dh', 3, 'mk', 0)")
      (sqlite-execute jabber-db--connection "BEGIN")
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc message)
                   (jabber-omemo-store-save-session-and-clear-legacy-keys
                    "me@example.com" "friend@example.com" 7 envelope)
                   (setcdr (cdr body) '("decrypted"))
                   message)))
        (jabber-mam--process-message jc stanza))
      (sqlite-execute jabber-db--connection "COMMIT")
      (should (equal envelope
                     (jabber-omemo-store-load-session
                      "me@example.com" "friend@example.com" 7)))
      (should-not (jabber-omemo-store-all-skipped-keys
                   "me@example.com" "friend@example.com" 7))
      (should (equal "decrypted"
                     (caar (sqlite-select jabber-db--connection "\
SELECT body FROM message WHERE stanza_id = 'stanza-000001'")))))))

(ert-deftest jabber-test-mam-decrypts-without-message-noise ()
  "Archived decryption does not write per-message diagnostics."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (stanza (jabber-test-mam--make-message 1))
           message-settings)
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc message)
                   (setq message-settings
                         (list inhibit-message message-log-max))
                   message)))
        (jabber-mam--process-message jc stanza))
      (should (equal '(t nil) message-settings)))))

;;; Group 4: Parse helpers

(ert-deftest jabber-test-mam-parse-result ()
  "jabber-mam--parse-result extracts archive-id, stamp, and inner message."
  (let* ((xml (jabber-test-mam--make-message 42))
         (parsed (jabber-mam--parse-result xml)))
    (should parsed)
    (should (string= "archive-000042" (nth 0 parsed)))
    (should (stringp (nth 1 parsed)))
    (should (listp (nth 2 parsed)))
    (should (string= "Message 42"
                     (car (jabber-xml-node-children
                           (car (jabber-xml-get-children (nth 2 parsed) 'body))))))))

(ert-deftest jabber-test-mam-build-query-before-id-empty ()
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

(ert-deftest jabber-test-mam-build-query-before-id-string ()
  "build-query with before-id as a string emits <before>ID</before>."
  (let ((query (jabber-mam--build-query "q2" nil nil nil 10 "some-id")))
    (let* ((set-el (cl-find 'set (jabber-xml-node-children query)
                            :key (lambda (n) (and (listp n) (jabber-xml-node-name n)))))
           (before-el (car (jabber-xml-get-children set-el 'before))))
      (should before-el)
      (should (string= "some-id" (car (jabber-xml-node-children before-el)))))))

(ert-deftest jabber-test-mam-parse-fin-incomplete ()
  "jabber-mam--parse-fin returns :complete nil when not complete."
  (let* ((xml (jabber-test-mam--make-fin "last-123"))
         (fin (jabber-mam--parse-fin xml)))
    (should-not (plist-get fin :complete))
    (should (string= "last-123" (plist-get fin :last)))))

(ert-deftest jabber-test-mam-parse-fin-complete ()
  "jabber-mam--parse-fin returns :complete t when archive is exhausted."
  (let* ((xml (jabber-test-mam--make-fin "last-456" t))
         (fin (jabber-mam--parse-fin xml)))
    (should (plist-get fin :complete))
    (should (string= "last-456" (plist-get fin :last)))))


;;; Group 6: MUC messages

(defun jabber-test-mam--make-muc-message (index room our-nick)
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

(ert-deftest jabber-test-mam-muc-message-storage ()
  "MUC messages from MAM are stored with correct peer and type."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (jabber-test-mam--queries jc "muc-query"))
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      (jabber-db-with-transaction
        (dotimes (i 10)
          (jabber-mam--process-message
           jc (jabber-test-mam--make-muc-message i room "mynick"))))
      (let ((rows (jabber-db-query "me@example.com" room
                                   0 (+ 1700000000 (* 10 86400)) -1)))
        (should (= 10 (length rows)))
        (should (string= "groupchat" (plist-get (car rows) :type)))
        (should (string= room (plist-get (car rows) :peer)))))))

(ert-deftest jabber-test-mam-muc-direction-detection ()
  "MUC MAM detects outgoing messages by matching our nickname."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (jabber-test-mam--queries jc "muc-query"))
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants
            `((,room ("mynick" . nil) ("otherperson" . nil)))))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      (jabber-db-with-transaction
        (jabber-mam--process-message
         jc (jabber-test-mam--make-muc-message 0 room "mynick"))  ; from us (idx%3=0)
        (jabber-mam--process-message
         jc (jabber-test-mam--make-muc-message 1 room "mynick"))) ; from other (idx%3=1)
      (let ((rows (jabber-db-query "me@example.com" room
                                   0 (+ 1700000000 (* 2 86400)) -1)))
        (should (= 2 (length rows)))
        (should (string= "out" (plist-get (car rows) :direction)))
        (should (string= "in" (plist-get (cadr rows) :direction)))))))

;;; Group 7: Dirty peer tracking

(ert-deftest jabber-test-mam-mark-dirty-dedup ()
  "jabber-mam--mark-dirty does not add the same peer twice."
  (let ((jabber-mam--dirty-peers nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (jc) (symbol-name jc))))
      (jabber-mam--mark-dirty 'account-a "peer@example.com" "chat")
      (jabber-mam--mark-dirty 'account-a "peer@example.com" "chat")
      (jabber-mam--mark-dirty 'account-b "peer@example.com" "chat")
      (should (= 2 (length jabber-mam--dirty-peers)))
      (should (member '("account-a" "peer@example.com" "chat")
                      jabber-mam--dirty-peers))
      (should (member '("account-b" "peer@example.com" "chat")
                      jabber-mam--dirty-peers)))))

;;; Group 8: jabber-mam-sync-buffer

(ert-deftest jabber-test-mam-sync-buffer-not-connected ()
  "Signal user-error when not connected."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'dead-jc)
    (let ((jabber-connections nil))
      (should-error (jabber-mam-sync-buffer) :type 'user-error))))


;;; Group 9: disconnect cleanup

(ert-deftest jabber-test-mam-correction-from-original-sender-updates-db ()
  "Archived XEP-0308 correction from the original sender updates storage."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-muc-participants nil))
      (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
      (jabber-mam--process-message
       jc
       (jabber-test-mam--make-correction-message
        "archive-correction-1" "correction-1" "stanza-000001"
        "friend@example.com/other-resource" "Corrected body"))
      (let ((row (car (sqlite-select (jabber-db-ensure-open)
                                     "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-000001'"))))
        (should (equal '("Corrected body" 1) row))))))

(ert-deftest jabber-test-mam-correction-from-wrong-sender-rejected ()
  "Archived XEP-0308 correction from another sender does not update storage."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-muc-participants nil))
      (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
      (jabber-mam--process-message
       jc
       (jabber-test-mam--make-correction-message
        "archive-correction-2" "correction-2" "stanza-000001"
        "mallory@example.com/resource" "Forged body"))
      (let ((row (car (sqlite-select (jabber-db-ensure-open)
                                     "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-000001'"))))
        (should (equal '("Message 1" 0) row))))))

(ert-deftest jabber-test-mam-undecryptable-correction-preserves-plaintext ()
  "An archived correction decrypt failure never replaces stored plaintext."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-muc-participants nil)
           (correction
            (jabber-test-mam--make-correction-message
             "archive-correction-failed" "correction-failed"
             "stanza-000001" "friend@example.com/phone"
             "OMEMO encrypted message"))
           messages)
      (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc inner)
                   (jabber-chat--set-body
                    inner "[OMEMO: could not decrypt]")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (jabber-mam--process-message jc correction))
      (should-not
       (cl-find-if
        (lambda (text) (string-prefix-p "XEP-0308:" text))
        messages))
      (should
       (equal '(("Message 1" 0))
              (sqlite-select
               (jabber-db-ensure-open)
               "SELECT body, edited FROM message \
WHERE stanza_id = 'stanza-000001'"))))))


;;; Group 10: stanza mutation guard

(ert-deftest jabber-test-mam-body-stanza-stripped ()
  "Body-bearing MAM result has children stripped after processing."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (stanza (jabber-test-mam--make-message 1)))
      (jabber-mam--process-message jc stanza)
      (should-not (cddr stanza)))))

(ert-deftest jabber-test-mam-reaction-reply-fallback-not-stored ()
  "Conversations reaction fallback from MAM is not stored as chat text."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (quote "Δύο άτομα δίνουν πόνο έξω")
           (body (concat "> " quote "\n👍"))
           (stanza `(message ((from . "me@example.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . ,jabber-test-mam-queryid)
                                      (id . "archive-reaction-1"))
                                     (forwarded ((xmlns . ,jabber-mam-forward-xmlns))
                                                (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                                        (stamp . "2026-06-07T06:25:31Z")))
                                                (message ((from . "me@example.com/Conversations")
                                                          (to . "som@yax.im")
                                                          (type . "chat"))
                                                         (reactions ((xmlns . ,jabber-reactions-xmlns)
                                                                     (id . "target-1"))
                                                                    (reaction nil "👍"))
                                                         (store ((xmlns . "urn:xmpp:hints")))
                                                         (reply ((xmlns . "urn:xmpp:reply:0")
                                                                 (to . "som@yax.im/Conversations")
                                                                 (id . "target-1")))
                                                         (fallback ((xmlns . "urn:xmpp:fallback:0")
                                                                    (for . "urn:xmpp:reply:0"))
                                                                   (body ((start . "0")
                                                                          (end . "30"))))
                                                         (fallback ((xmlns . "urn:xmpp:fallback:0")
                                                                    (for . ,jabber-reactions-xmlns))
                                                                   (body nil))
                                                         (body nil ,body)))))))
      (jabber-mam--process-message jc stanza)
      (should-not (sqlite-select (jabber-db-ensure-open)
                    "SELECT body FROM message WHERE server_id = ?"
                    '("archive-reaction-1")))
      (should (jabber-xml-get-attribute stanza 'jabber-mam--origin))
      (should (car (jabber-xml-get-children stanza 'reactions))))))

(ert-deftest jabber-test-mam-author-retract-fallback-unwrapped-not-stored ()
  "Archived author retraction is unwrapped but never stored as chat text."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))

           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (stanza
            `(message ((from . "room@conference.example"))
                      (result ((xmlns . ,jabber-mam-xmlns)
                               (queryid . ,jabber-test-mam-queryid)
                               (id . "archive-retract-1"))
                              (forwarded
                               ((xmlns . ,jabber-mam-forward-xmlns))
                               (delay ((xmlns . ,jabber-mam-delay-xmlns)
                                       (stamp . "2026-08-13T13:45:00Z")))
                               (message
                                ((from . "room@conference.example/alice")
                                 (to . "me@example.com")
                                 (type . "groupchat")
                                 (id . "retract-message-1"))
                                (body () "sender-controlled fallback")
                                (retract
                                 ((id . "target-server-id")
                                  (xmlns . ,jabber-moderation-retract-xmlns)))
                                (occupant-id
                                 ((xmlns . "urn:xmpp:occupant-id:0")
                                  (id . "occupant-alice")))))))))
      (plist-put (car jabber-mam--syncing) :to "room@conference.example")
      (jabber-db-store-message
       "me@example.com" "room@conference.example" "in" "groupchat"
       "original message" 1786628700 "alice" "original-client-id"
       "target-server-id" "occupant-alice")
      (cl-letf (((symbol-function
                  'jabber-moderation--room-supports-occupant-id-p)
                 (lambda (_room) t)))
        (jabber-mam--process-message jc stanza)
        (should (jabber-moderation--handle-message jc stanza)))
      (should
       (equal '(("original message" "room@conference.example/alice"))
              (sqlite-select
               (jabber-db-ensure-open)
               "SELECT body, retracted_by FROM message ORDER BY id")))
      (should (jabber-xml-get-attribute stanza 'jabber-mam--origin))
      (should (jabber-moderation--muc-retraction-message-p stanza)))))

(ert-deftest jabber-test-mam-bodyless-stanza-unwrapped ()
  "Bodyless MAM result is unwrapped with original sender and MAM marker."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Receipt stanza: no body, just a <received/> element
           (stanza `(message ((from . "me@example.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . ,jabber-test-mam-queryid)
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
      ;; Archive id is needed by downstream tombstone handlers.
      (should (equal "archive-001"
                     (jabber-xml-get-attribute stanza 'jabber-mam--archive-id)))
      ;; The receipt element should be a child
      (should (car (jabber-xml-get-children stanza 'received))))))

;;; Group 9: query ID validation

(ert-deftest jabber-test-mam-unknown-queryid-rejected ()
  "MAM result with unknown queryid is not processed."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc "known-query"))
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

(ert-deftest jabber-test-mam-known-queryid-accepted ()
  "MAM result with known queryid is processed normally."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc "known-query"))
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


;;; Group 11: sender JID validation

(ert-deftest jabber-test-mam-rejects-foreign-sender ()
  "MAM result from a server other than ours is rejected."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           ;; Outer from is evil.com, not our bare JID
           (stanza `(message ((from . "evil.com"))
                             (result ((xmlns . ,jabber-mam-xmlns)
                                      (queryid . ,jabber-test-mam-queryid)
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

(ert-deftest jabber-test-mam-accepts-own-jid-sender ()
  "MAM result from our own bare JID is accepted."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           ;; Normal 1:1 MAM result with from=our bare JID
           (stanza (jabber-test-mam--make-message 5)))
      (jabber-mam--process-message jc stanza)
      ;; Message should be stored
      (should (caar (sqlite-select (jabber-db-ensure-open)
                                   "SELECT 1 FROM message WHERE stanza_id='stanza-000005'"))))))

(ert-deftest jabber-test-mam-accepts-joined-muc-sender ()
  "MAM result from a joined MUC room is accepted."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (room "room@conference.example.com")
           (jabber-mam--syncing (jabber-test-mam--queries jc "muc-query"))

           (jabber-mam--tx-depth 1)
           (jabber-chat--crypto-loaded t)
           (jabber-muc--rooms (make-hash-table :test 'equal))
           (jabber-muc-participants nil))
      (puthash room (list (cons jc "mynick")) jabber-muc--rooms)
      (plist-put (car jabber-mam--syncing) :to room)
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


;;; Native admission and settlement regressions

(defun jabber-test-mam--native-connection (&optional username)
  "Return a disposable established native FSM for USERNAME, defaulting to me."
  (let ((jc (make-symbol "mam-native")))
    (put jc :name 'jabber-connection)
    (put jc :state :session-established)
    (put jc :state-data
         (jabber-sm--reset
          (list :username (or username "me") :server "example.com"
                :connection (list 'transport) :session-id "stream"
                :send-function #'ignore)))
    jc))

(defmacro jabber-test-mam--with-native (&rest body)
  "Run BODY with native IQ dispatch and isolated SQLite."
  (declare (indent 0) (debug t))
  `(jabber-test-mam-with-db
     (let* ((jc (jabber-test-mam--native-connection))
            (other (jabber-test-mam--native-connection "other"))
            (jabber-connections (list jc other))
            (jabber-open-info-queries nil)
            (jabber-mam--syncing nil)
            (jabber-mam--peer-syncing nil)
            (jabber-mam--dirty-peers nil)
            (jabber-mam--tx-depth 0)
            (jabber-mam-sync-complete-functions nil)
            (jabber-mam-peer-syncing-functions nil)
            (fsm-debug nil)
            sent)
       (cl-letf (((symbol-function 'jabber-send-sexp)
                  (lambda (_jc xml &rest _) (push xml sent))))
         (unwind-protect (progn ,@body)
           (jabber-mam--cleanup-all))))))

(defun jabber-test-mam--reply (jc request &optional from failure incomplete)
  "Deliver REQUEST's IQ reply through JC's native FSM.
FROM is the archive sender; FAILURE and INCOMPLETE select the response."
  (fsm-send-sync
   jc `(:stanza
        (iq ((type . ,(if failure "error" "result"))
             (id . ,(jabber-xml-get-attribute request 'id))
             ,@(when from `((from . ,from))))
            ,(if failure '(error () (service-unavailable ()))
               `(fin ((xmlns . ,jabber-mam-xmlns)
                      (complete . ,(if incomplete "false" "true")))
                     (set ((xmlns . ,jabber-mam-rsm-xmlns))
                          (last () "next-page"))))))))

(ert-deftest jabber-test-mam-native-archive-admission ()
  "Reject occupant, missing room sender and foreign connection before decrypt."
  (jabber-test-mam--with-native
    (let ((jabber-test-mam-queryid "owned") (decrypts 0))
      (jabber-mam--query jc nil "owned" nil nil "room@example.com")
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc xml) (cl-incf decrypts) xml)))
        (dolist (case `((,jc "room@example.com/occupant")
                        (,jc "other@example.com") (,jc nil)
                        (,other "room@example.com")))
          (let ((xml (jabber-test-mam--make-message 1)))
            (setf (cadr xml) (when (cadr case) `((from . ,(cadr case)))))
            (jabber-mam--process-message (car case) xml)
            (should (cddr xml))))
        (should (= 0 decrypts))
        (should-not (sqlite-select jabber-db--connection "SELECT id FROM message"))))))

(ert-deftest jabber-test-mam-native-session-admission ()
  "Accept native state copies, but reject retired transport before decrypt."
  (jabber-test-mam--with-native
    (let ((jabber-test-mam-queryid "owned") (decrypts 0))
      (jabber-mam--query jc nil "owned")
      (jabber-sm--drain-pending jc (fsm-get-state-data jc))
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc xml) (cl-incf decrypts) xml)))
        (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
        (should (= 1 decrypts))
        (plist-put (fsm-get-state-data jc) :connection (list 'replacement))
        (jabber-mam--process-message jc (jabber-test-mam--make-message 2))
        (should (= 1 decrypts))))))

(ert-deftest jabber-test-mam-native-cancel-late-replies ()
  "Late fin/error from a cancelled room cannot settle another query."
  (dolist (error-first '(nil t))
    (jabber-test-mam--with-native
      (jabber-mam--query jc nil "room-query" nil nil "room@example.com")
      (let ((room-request (car sent)))
        (jabber-mam--query jc nil "other-query")
        (let ((other-request (car sent)))
          (should (= 2 jabber-mam--tx-depth))
          (jabber-mam--cancel-muc-query "room@example.com")
          (should (= 1 jabber-mam--tx-depth))
          (jabber-test-mam--reply jc room-request "room@example.com" error-first)
          (jabber-test-mam--reply jc room-request "room@example.com" (not error-first))
          (should (= 1 jabber-mam--tx-depth))
          (should (jabber-mam-syncing-p))
          (jabber-test-mam--reply jc other-request)
          (should (= 0 jabber-mam--tx-depth))
          (should-not (jabber-mam-syncing-p)))))))

(ert-deftest jabber-test-mam-native-bounded-sync-preserves-absence ()
  "Limited success, partial failure and disconnect never delete absent rows."
  (dolist (ending '(success error disconnect))
    (jabber-test-mam--with-native
      (let ((stamp (+ 1700000000 86400)))
        (jabber-db-store-message "me@example.com" "friend@example.com"
                                 "in" "chat" "unfetched same second" stamp
                                 nil "unfetched" "unfetched-archive"))
      (with-temp-buffer
        (setq-local jabber-buffer-connection jc)
        (setq-local jabber-chatting-with "friend@example.com")
        (setq-local jabber-chat-buffer-msg-count 1)
        (jabber-mam-sync-buffer))
      (let* ((request (car sent))
             (jabber-test-mam-queryid
              (jabber-xml-get-attribute (car (jabber-xml-get-children request 'query))
                                        'queryid)))
        (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
        (pcase ending
          ('success (jabber-test-mam--reply jc request))
          ('error (jabber-test-mam--reply jc request nil t))
          ('disconnect (jabber-mam--cleanup-connection jc)))
        (should (= 2 (caar (sqlite-select jabber-db--connection
                                          "SELECT count(*) FROM message"))))
        (should (= 0 jabber-mam--tx-depth))))))

(ert-deftest jabber-test-mam-native-iq-admission ()
  "Forged IQs leave the real page pending; valid completion fires once."
  (dolist (room '(nil "room@example.com"))
    (dolist (failure '(nil t))
      (jabber-test-mam--with-native
        (let ((calls 0))
          (jabber-mam--query jc nil nil nil nil room nil nil
                             (lambda () (cl-incf calls)))
          (let* ((request (car sent))
                 (id (jabber-xml-get-attribute request 'id))
                 (pending (assoc id jabber-open-info-queries)))
            (dolist (case `((,other ,room)
                            (,jc "foreign@example.com")
                            (,jc ,(concat (or room "me@example.com") "/resource"))))
              (jabber-test-mam--reply (car case) request (cadr case) failure)
              (should (eq pending (assoc id jabber-open-info-queries)))
              (should (= 0 calls))
              (should (= 1 jabber-mam--tx-depth)))
            (when room
              (jabber-test-mam--reply jc request nil failure)
              (should (eq pending (assoc id jabber-open-info-queries))))
            (jabber-test-mam--reply jc request room failure)
            (jabber-test-mam--reply jc request room failure)
            (should (= 1 calls))
            (should (= 0 jabber-mam--tx-depth))
            (should-not jabber-open-info-queries)))))))

(ert-deftest jabber-test-mam-native-missing-from-and-retirement ()
  "Missing FROM requires personal ownership; each lifecycle replacement rejects."
  (dolist (change '(nil (:connection . replacement)
                       (:session-id . "new-stream")
                       (:username . "other") (:server . "other.example")))
    (jabber-test-mam--with-native
      (let ((jabber-test-mam-queryid "own") (decrypts 0))
        (jabber-mam--query jc nil "own")
        (jabber-sm--drain-pending jc (fsm-get-state-data jc))
        (when change
          (plist-put (fsm-get-state-data jc) (car change) (cdr change)))
        (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                   (lambda (_jc xml) (cl-incf decrypts) xml)))
          (let ((xml (jabber-test-mam--make-message 1)))
            (setf (cadr xml) nil)
            (jabber-mam--process-message jc xml)))
        (should (= (if change 0 1) decrypts))
        (should (= (if change 0 1)
                   (caar (sqlite-select jabber-db--connection
                                         "SELECT count(*) FROM message"))))))))

(ert-deftest jabber-test-mam-native-pagination-retirement ()
  "Pagination retains ownership across timers and retires safely at every exit."
  (dolist (ending '(success connection all room replacement))
    (jabber-test-mam--with-native
      (let ((calls 0))
        (jabber-mam--query jc nil "pages" nil nil "room@example.com" nil 7
                           (lambda () (cl-incf calls)))
        (let ((first (car sent))
              (query (car jabber-mam--syncing)))
          (jabber-test-mam--reply jc first "room@example.com" nil t)
          (should (= 0 jabber-mam--tx-depth))
          (should (jabber-mam-syncing-p))
          (let* ((timer (plist-get query :timer))
                 (function (timer--function timer))
                 (args (timer--args timer)))
            (should (timerp timer))
            ;; Deterministic delivery of the native timer, even if cancelled.
            (cancel-timer timer)
            (pcase ending
              ('connection (jabber-mam--cleanup-connection jc))
              ('all (jabber-mam--cleanup-all))
              ('room (jabber-mam--cancel-muc-query "room@example.com"))
              ('replacement
               (plist-put (fsm-get-state-data jc) :connection (list 'new))))
            (apply function args)
            (if (eq ending 'success)
                (progn
                  (should (= 2 (length sent)))
                  (should (= 1 jabber-mam--tx-depth))
                  (let* ((payload (car (jabber-xml-get-children (car sent) 'query)))
                         (rsm (car (jabber-xml-get-children payload 'set))))
                    (should (equal '(after nil "next-page")
                                   (car (jabber-xml-get-children rsm 'after))))
                    (should (equal '(max nil "7")
                                   (car (jabber-xml-get-children rsm 'max)))))
                  (jabber-test-mam--reply jc (car sent) "room@example.com"))
              (should (= 1 (length sent))))
            (should (= 1 calls))
            (should-not (jabber-mam-syncing-p))
            (should-not jabber-open-info-queries)
            (should (= 0 jabber-mam--tx-depth))))))))

(ert-deftest jabber-test-mam-native-cancelled-callbacks-preserve-transaction ()
  "Captured stale callbacks cannot commit a surviving query's SQLite writes."
  (jabber-test-mam--with-native
    (let ((calls 0) (jabber-test-mam-queryid "survivor"))
      (jabber-mam--query jc nil "cancelled" nil nil "room@example.com"
                         nil nil (lambda () (cl-incf calls)))
      (let ((callbacks (car jabber-open-info-queries)))
        (jabber-mam--query jc nil "survivor" nil nil nil nil nil
                           (lambda () (cl-incf calls)))
        (let ((request (car sent)))
          (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
          (jabber-mam--cancel-muc-query "room@example.com")
          (should (= 1 calls))
          (dolist (callback (list (nth 1 callbacks) (nth 2 callbacks)))
            (funcall (car callback) jc
                     '(iq ((from . "room@example.com"))
                          (fin ((xmlns . "urn:xmpp:mam:2") (complete . "true"))))
                     (cdr callback)))
          (should (= 1 calls))
          (should (= 1 jabber-mam--tx-depth))
          (let ((reader (sqlite-open jabber-db-path)))
            (unwind-protect
                (progn
                  (should-not (sqlite-select reader "SELECT body FROM message"))
                  (jabber-test-mam--reply jc request)
                  (should (equal '(("Message 1"))
                                 (sqlite-select reader "SELECT body FROM message"))))
              (sqlite-close reader)))
          (should (= 2 calls))
          (should-not jabber-open-info-queries))))))

(ert-deftest jabber-test-mam-native-query-start-failures ()
  "Real BEGIN failure and failed send settle only the owned contribution."
  (dolist (failure '(begin send quit))
    (jabber-test-mam--with-native
      (let ((calls 0))
        (when (eq failure 'begin)
          (sqlite-execute jabber-db--connection "BEGIN"))
        (cl-letf (((symbol-function 'jabber-send-sexp)
                   (lambda (&rest _)
                     (if (eq failure 'quit) (signal 'quit nil)
                       (error "Injected send failure")))))
          (condition-case nil
              (jabber-mam--query jc nil nil nil nil nil nil nil
                                 (lambda () (cl-incf calls)))
            (quit (should (eq failure 'quit)))))
        (should (= 1 calls))
        (should-not (jabber-mam-syncing-p))
        (should-not jabber-open-info-queries)
        (should (= 0 jabber-mam--tx-depth))
        ;; Failed BEGIN never releases somebody else's transaction.
        (when (eq failure 'begin)
          (sqlite-execute jabber-db--connection "ROLLBACK"))
        (sqlite-execute jabber-db--connection "BEGIN")
        (sqlite-execute jabber-db--connection "COMMIT")))))

(ert-deftest jabber-test-mam-native-stale-cursor-retry ()
  "A stale cursor retry retains filters and callback and cannot loop forever."
  (jabber-test-mam--with-native
    (let ((calls 0))
      (jabber-mam--query jc "expired" "retry" "friend@example.com"
                         "2020-01-01T00:00:00Z" nil nil 7
                         (lambda () (cl-incf calls)))
      (dotimes (_ 2)
        (fsm-send-sync
         jc `(:stanza
              (iq ((type . "error")
                   (id . ,(jabber-xml-get-attribute (car sent) 'id)))
                  (error () (item-not-found ()))))))
      (should (= 2 (length sent)))
      (should (= 1 calls))
      (should-not (jabber-mam-syncing-p))
      (should-not jabber-open-info-queries)
      (let ((xml (jabber-sexp2xml (car sent))))
        (should (string-match-p "friend@example.com" xml))
        (should (string-match-p "2020-01-01T00:00:00Z" xml))
        (should-not (string-match-p "<after>" xml))))))

(ert-deftest jabber-test-mam-native-reentrant-decrypt-retirement ()
  "Retirement during decryption prevents storage and unwrapping afterwards."
  (jabber-test-mam--with-native
    (let ((jabber-test-mam-queryid "retire"))
      (jabber-mam--query jc nil "retire")
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc xml) (jabber-mam--cleanup-connection jc) xml)))
        (let ((xml (jabber-test-mam--make-message 1)))
          (jabber-mam--process-message jc xml)
          (should (cddr xml))
          (should-not (jabber-xml-get-attribute xml 'jabber-mam--origin))))
      (should-not (sqlite-select jabber-db--connection "SELECT body FROM message")))))

(ert-deftest jabber-test-mam-native-buffer-sync-contract ()
  "Bounded personal and room sync preserve absent threads and settle indicators."
  (dolist (room '(nil "room@example.com"))
    (dolist (ending '(success error disconnect))
      (jabber-test-mam--with-native
        (let* ((peer (or room "friend@example.com"))
               (type (if room "groupchat" "chat"))
               (signals nil)
               (refreshes nil)
               (jabber-mam-peer-syncing-functions
                (list (lambda (&rest args) (push args signals))))
               (jabber-mam-sync-complete-functions
                (list (lambda (peers) (push peers refreshes)))))
          (jabber-db-store-message
           "me@example.com" peer "in" type "root" 1700086400
           "phone" "root" "archive-root" nil nil nil nil '(:thread-id "thread"))
          (jabber-db-register-message-thread
           "me@example.com" peer type "thread" nil "root" "archive-root" 1700086400)
          (with-temp-buffer
            (setq-local jabber-buffer-connection jc)
            (setq-local jabber-chatting-with peer)
            (setq-local jabber-group room)
            (setq-local jabber-chat-buffer-msg-count 1)
            (jabber-mam-sync-buffer))
          (let* ((request (car sent))
                 (payload (car (jabber-xml-get-children request 'query)))
                 (rsm (car (jabber-xml-get-children payload 'set)))
                 (jabber-test-mam-queryid (jabber-xml-get-attribute payload 'queryid))
                 (xml (jabber-test-mam--make-message 1 peer type)))
            (should (equal room (jabber-xml-get-attribute request 'to)))
            (should (equal '(before nil) (car (jabber-xml-get-children rsm 'before))))
            (should (equal '(max nil "1") (car (jabber-xml-get-children rsm 'max))))
            (setf (cadr xml) `((from . ,(or room "me@example.com"))))
            (jabber-mam--process-message jc xml)
            (pcase ending
              ;; A successful bounded page need not exhaust the archive.
              ('success (jabber-test-mam--reply jc request room nil t))
              ('error (jabber-test-mam--reply jc request room t))
              ('disconnect (jabber-mam--cleanup-connection jc)))
            (should (= 1 (length sent)))
            (should (equal (list (list peer type nil) (list peer type t)) signals))
            (should (equal (list (list (list "me@example.com" peer type))) refreshes))
            (should (jabber-db-message-thread-known-p "me@example.com" peer type "thread"))
            ;; Reopen the actual file, not just the writer's uncommitted view.
            (jabber-db-close)
            (should (= 2 (caar (sqlite-select (jabber-db-ensure-open)
                                              "SELECT count(*) FROM message"))))))))))

(ert-deftest jabber-test-mam-native-connection-cleanup-is-scoped ()
  "Disconnect settles only the selected connection, even with failing callbacks."
  (jabber-test-mam--with-native
    (let ((calls 0))
      (jabber-mam--query jc nil nil nil nil nil nil nil
                         (lambda () (cl-incf calls) (error "Callback failure")))
      (jabber-mam--query other nil nil nil nil nil nil nil
                         (lambda () (cl-incf calls)))
      (let ((request (car sent)))
        (jabber-mam--cleanup-connection jc)
        (jabber-mam--cleanup-connection jc)
        (should (= 1 calls))
        (should (= 1 jabber-mam--tx-depth))
        (should (= 1 (length jabber-open-info-queries)))
        (jabber-test-mam--reply other request)
        (should (= 2 calls))
        (should (= 0 jabber-mam--tx-depth))
        (should-not (jabber-mam-syncing-p))))))

;;; Native room departure and disconnect fault regressions

(defmacro jabber-test-mam--with-lifecycle (&rest body)
  "Run BODY with the registered MAM lifecycle hooks, without unrelated UI."
  (declare (indent 0) (debug t))
  `(let ((jabber-lifecycle-session-reset-functions
          (cl-remove-if-not
           (lambda (function) (eq function #'jabber-mam--cleanup-connection))
           jabber-lifecycle-session-reset-functions))
         (jabber-lost-connection-hooks
          (cl-remove-if-not
           (lambda (function) (eq function #'jabber-mam--cleanup-connection))
           jabber-lost-connection-hooks))
         (jabber-pre-disconnect-hook
          (cl-remove-if-not
           (lambda (function) (eq function #'jabber-mam--cleanup-all))
           jabber-pre-disconnect-hook))
         (jabber-lifecycle-connection-list-changed-functions nil)
         (jabber-post-disconnect-hook nil)
         (jabber-auto-reconnect nil))
     (cl-letf (((symbol-function 'jabber-clear-roster) #'ignore))
       ,@body)))

(defun jabber-test-mam--capture-page (query request &optional waiting)
  "Capture QUERY's native REQUEST callbacks, optionally enter WAITING state."
  (let ((callbacks (assoc (plist-get query :iq-id) jabber-open-info-queries)))
    (when waiting
      (jabber-test-mam--reply (plist-get query :jc) request
                             (plist-get query :to) nil t))
    (list query request callbacks (plist-get query :timer))))

(defun jabber-test-mam--deliver-late-page (capture)
  "Deliver both retired IQ callbacks and any pagination timer in CAPTURE."
  (pcase-let ((`(,query ,_request ,callbacks ,timer) capture))
    (dolist (callback (list (nth 1 callbacks) (nth 2 callbacks)))
      (funcall (car callback) (plist-get query :jc)
               `(iq ((from . ,(or (plist-get query :to)
                                  (jabber-connection-bare-jid
                                   (plist-get query :jc)))))
                    (fin ((xmlns . ,jabber-mam-xmlns) (complete . "true"))))
               (cdr callback)))
    (when timer
      (should-not (memq timer timer-list))
      (apply (timer--function timer) (timer--args timer)))))

(defun jabber-test-mam--assert-retired (capture)
  "Assert that CAPTURE owns no query, IQ, page, timer or contribution."
  (let ((query (car capture)))
    (should-not (memq query jabber-mam--syncing))
    (should-not (assoc (jabber-xml-get-attribute (nth 1 capture) 'id)
                       jabber-open-info-queries))
    (dolist (key '(:page :iq-id :timer :transaction))
      (should-not (plist-get query key)))))

(ert-deftest jabber-test-mam-native-room-departure-account-scope ()
  "Room departure retires every A query but preserves B in either order."
  (dolist (other-first '(nil t))
    (dolist (a-waiting '(nil t))
      (dolist (b-waiting '(nil t))
        (jabber-test-mam--with-native
          (let ((room "room@example.com")
                (jabber-muc--rooms (make-hash-table :test #'equal))
                (jabber-muc--room-jids (make-hash-table :test #'equal))
                (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
                (jabber-muc-participants nil)
                (a-calls 0) (b-calls 0) captures survivor)
            (jabber-muc-add-groupchat room "me" jc)
            (jabber-muc-add-groupchat room "other" other)
            (cl-labels
                ((start-a ()
                   (dotimes (i 3)
                     (let ((query (jabber-mam--query
                                   jc nil nil nil nil room nil nil
                                   (lambda () (cl-incf a-calls)))))
                       (push (jabber-test-mam--capture-page
                              query (car sent) (and a-waiting (zerop i)))
                             captures))))
                 (start-b ()
                   (let ((query (jabber-mam--query
                                 other nil "survivor" nil nil room nil nil
                                 (lambda () (cl-incf b-calls)))))
                     (setq survivor (jabber-test-mam--capture-page
                                     query (car sent) b-waiting)))))
              (if other-first (progn (start-b) (start-a))
                (start-a) (start-b)))
            (jabber-db-store-message "me@example.com" room "in" "groupchat"
                                     "A accepted" 1 nil "A-row" "A-archive")
            (jabber-muc-remove-groupchat room jc)
            (should-not (jabber-muc-joined-p room jc))
            (should (equal "other" (jabber-muc-nickname room other)))
            (should (= 3 a-calls))
            (should (= 0 b-calls))
            (should (equal (list (car survivor)) jabber-mam--syncing))
            (dolist (capture captures)
              (jabber-test-mam--assert-retired capture)
              (jabber-test-mam--deliver-late-page capture))
            (jabber-muc-remove-groupchat room jc)
            (should (= 3 a-calls))
            (should (= (if b-waiting 0 1) jabber-mam--tx-depth))
            (let ((reader (sqlite-open jabber-db-path)))
              (unwind-protect
                  (progn
                    (should (= (if b-waiting 1 0)
                               (caar (sqlite-select reader "SELECT count(*) FROM message"))))
                    (when b-waiting
                      (let ((timer (nth 3 survivor)))
                        (should (memq timer timer-list))
                        (cancel-timer timer)
                        (apply (timer--function timer) (timer--args timer))))
                    (should (= 1 jabber-mam--tx-depth))
                    (should (= 1 (length jabber-open-info-queries)))
                    (let* ((jabber-test-mam-queryid "survivor")
                           (xml (jabber-test-mam--make-message 1 room "groupchat")))
                      (setf (cadr xml) `((from . ,room)))
                      (jabber-mam--process-message other xml))
                    (jabber-test-mam--reply
                     other (if b-waiting (car sent) (nth 1 survivor)) room)
                    (should (= 1 b-calls))
                    (should (= 0 jabber-mam--tx-depth))
                    (should (equal '(("me@example.com" "A accepted")
                                     ("other@example.com" "Message 1"))
                                   (sqlite-select reader
                                                  "SELECT account,body FROM message ORDER BY account"))))
                (sqlite-close reader)))))))))

(ert-deftest jabber-test-mam-native-accountless-room-departure ()
  "An intentional accountless departure cancels that room on every account."
  (jabber-test-mam--with-native
    (let ((room "room@example.com")
          (jabber-muc--rooms (make-hash-table :test #'equal))
          (jabber-muc--room-jids (make-hash-table :test #'equal))
          (jabber-muc--nonanonymous-rooms (make-hash-table :test #'equal))
          (jabber-muc-participants nil)
          (calls 0))
      (jabber-mam--query other nil "foreign-room" nil nil "elsewhere@example.com")
      (let ((survivor (car jabber-mam--syncing)))
        (dolist (account (list jc other))
          (jabber-muc-add-groupchat room "nick" account)
          (jabber-mam--query account nil nil nil nil room nil nil
                             (lambda () (cl-incf calls))))
        (jabber-muc-remove-groupchat room)
        (should-not (jabber-muc-joined-p room))
        (should (= 2 calls))
        (should (equal (list survivor) jabber-mam--syncing))
        (should (= 1 jabber-mam--tx-depth))
        (should (= 1 (length jabber-open-info-queries)))))))

(ert-deftest jabber-test-mam-native-cleanup-callback-faults ()
  "Error or quit cannot strand selected pages, timers, IQs or physical writes."
  (dolist (ending '(lost room all))
    (dolist (failure '(error quit))
      (jabber-test-mam--with-native
        (jabber-test-mam--with-lifecycle
          (let ((room "room@example.com") (calls nil) captures survivor)
            (jabber-mam--query other nil "foreign" nil nil nil nil nil
                               (lambda () (push 'foreign calls)))
            (setq survivor (car sent))
            ;; The faulting page is selected first, before another active page
            ;; and a waiting timer.  Every callback faults independently.
            (dolist (id '(waiting active fault))
              (let ((query (jabber-mam--query
                            jc nil nil nil nil room nil nil
                            (lambda () (push id calls) (signal failure nil)))))
                (push (jabber-test-mam--capture-page
                       query (car sent) (eq id 'waiting)) captures)))
            (jabber-db-store-message "me@example.com" room "in" "groupchat"
                                     "accepted" 1 nil "row" "archive")
            (cl-labels ((cleanup ()
                         (pcase ending
                           ('lost
                            (fsm-send-sync
                             jc (list :connection-dead
                                      (plist-get (fsm-get-state-data jc) :connection)
                                      "Fixture loss")))
                           ('room (jabber-mam--cancel-muc-query room jc))
                           ('all (jabber-mam--cleanup-all)))))
              (cleanup)
              (dolist (id '(waiting active fault))
                (should (= 1 (cl-count id calls))))
              (dolist (capture captures)
                (jabber-test-mam--assert-retired capture)
                (jabber-test-mam--deliver-late-page capture))
              (unless (eq ending 'lost) (cleanup))
              (jabber-mam--cleanup-connection jc)
              (should (= 3 (length (remq 'foreign (copy-sequence calls))))))
            (let ((reader (sqlite-open jabber-db-path)))
              (unwind-protect
                  (progn
                    (if (eq ending 'all)
                        (should (= 0 jabber-mam--tx-depth))
                      (should (= 1 jabber-mam--tx-depth))
                      (should (= 1 (length jabber-open-info-queries)))
                      (should-not (sqlite-select reader "SELECT body FROM message"))
                      (jabber-test-mam--reply other survivor))
                    (should (equal '(("accepted"))
                                   (sqlite-select reader "SELECT body FROM message"))))
                (sqlite-close reader)))
            (should (= 1 (cl-count 'foreign calls)))
            (should-not jabber-mam--syncing)
            (should-not jabber-open-info-queries)
            (should (= 0 jabber-mam--tx-depth))))))))

(ert-deftest jabber-test-mam-native-cleanup-presentation-faults ()
  "Attempt every peer and redraw hook despite earlier errors or quits."
  (dolist (failure '(error quit))
    (jabber-test-mam--with-native
      (let* ((calls nil)
             (jabber-mam-peer-syncing-functions
              (list (lambda (&rest _) (push 'peer-fault calls) (signal failure nil))
                    (lambda (&rest _) (push 'peer-good calls))))
             (jabber-mam-sync-complete-functions
              (list (lambda (_) (push 'redraw-fault calls) (signal failure nil))
                    (lambda (_) (push 'redraw-good calls)))))
        ;; Peer catch-ups awaiting discovery own tokens but no page yet.
        (jabber-mam--begin-peer-sync other "foreign@example.com")
        (jabber-mam--begin-peer-sync jc "one@example.com")
        (jabber-mam--begin-peer-sync jc "two@example.com")
        (jabber-mam--cleanup-connection jc)
        (should (= 2 (cl-count 'peer-good calls)))
        (should (= 2 (cl-count 'peer-fault calls)))
        (should (= 1 (length jabber-mam--peer-syncing)))
        ;; Put waiting queries behind the final active page: redisplay failure
        ;; must not prevent their completion or the remaining peer cleanup.
        (dotimes (_ 2)
          (let ((query (jabber-mam--query
                        jc nil nil nil nil nil nil nil
                        (lambda () (push 'complete calls)))))
            (jabber-test-mam--capture-page query (car sent) t)))
        (jabber-mam--query jc)
        (jabber-mam--mark-dirty jc "friend@example.com" "chat")
        (jabber-mam--cleanup-all)
        (jabber-mam--cleanup-all)
        (should (= 2 (cl-count 'complete calls)))
        (should (= 3 (cl-count 'peer-good calls)))
        (should (= 3 (cl-count 'peer-fault calls)))
        (should (= 1 (cl-count 'redraw-fault calls)))
        (should (= 1 (cl-count 'redraw-good calls)))
        (should-not jabber-mam--syncing)
        (should-not jabber-mam--peer-syncing)
        (should-not jabber-open-info-queries)
        (should-not jabber-mam--dirty-peers)
        (should (= 0 jabber-mam--tx-depth))))))

(ert-deftest jabber-test-mam-native-public-disconnect-settlement ()
  "Real voluntary single/all-account and lost-session paths settle personal MAM."
  (should (memq #'jabber-mam--cleanup-connection
                jabber-lifecycle-session-reset-functions))
  (dolist (ending '(one all lost))
    (dolist (waiting '(nil t))
      (jabber-test-mam--with-native
        (jabber-test-mam--with-lifecycle
          (let ((a-calls 0) (b-calls 0))
            (jabber-mam--query other nil "foreign" nil nil nil nil nil
                               (lambda () (cl-incf b-calls)))
            (let* ((survivor (car sent))
                   (query (jabber-mam--query
                           jc nil "personal" nil nil nil nil nil
                           (lambda () (cl-incf a-calls))))
                   (capture (jabber-test-mam--capture-page query (car sent) waiting)))
              (jabber-db-store-message "me@example.com" "friend@example.com"
                                       "in" "chat" "accepted" 1 nil "row" "archive")
              (pcase ending
                ('one (jabber-disconnect-one jc t))
                ('all (jabber-disconnect))
                ('lost (fsm-send-sync
                        jc (list :connection-dead
                                 (plist-get (fsm-get-state-data jc) :connection)
                                 "Fixture loss"))))
              (should-not (get jc :state))
              (should (= 1 a-calls))
              (jabber-test-mam--assert-retired capture)
              (jabber-test-mam--deliver-late-page capture)
              (jabber-mam--cleanup-connection jc)
              (should (= 1 a-calls))
              (let ((reader (sqlite-open jabber-db-path)))
                (unwind-protect
                    (progn
                      (if (eq ending 'all)
                          (progn (should-not (get other :state))
                                 (should-not jabber-connections))
                        (should (= 0 b-calls))
                        (should (eq :session-established (get other :state)))
                        (should (memq other jabber-connections))
                        (should (= 1 jabber-mam--tx-depth))
                        (should (= 1 (length jabber-open-info-queries)))
                        (should-not (sqlite-select reader "SELECT body FROM message"))
                        (jabber-test-mam--reply other survivor))
                      (should (equal '(("accepted"))
                                     (sqlite-select reader "SELECT body FROM message"))))
                  (sqlite-close reader)))
              (should (= 1 b-calls))
              (should (= 0 jabber-mam--tx-depth))
              (should-not jabber-mam--syncing)
              (should-not jabber-open-info-queries))))))))

(provide 'jabber-test-mam)

;;; jabber-test-mam.el ends here
