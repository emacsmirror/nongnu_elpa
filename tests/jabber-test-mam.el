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
(require 'jabber-message-thread)
(require 'jabber-omemo-store)
(require 'jabber-archive-test-helpers)

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

;; Bodyless results reach downstream handlers as an unwrapped copy, while
;; live handlers see only the consumed envelope.
(defun jabber-test-mam--dispatched (jc stanza)
  "Process MAM STANZA on JC; return the unwrapped stanzas sent downstream."
  (let* ((seen nil)
         (jabber-message-chain (list (lambda (_jc xml) (push xml seen)))))
    (jabber-mam--process-message jc stanza)
    (should-not (cddr stanza))
    (nreverse seen)))

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
           (jabber-muc-participants nil))
      ;; Each message commits in its own short transaction.
      (dotimes (i count)
        (jabber-mam--process-message jc (jabber-test-mam--make-message i)))
      ;; Verify all stored
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 (* count 86400))
                                   -1)))
        (should (= count (length rows)))))))

;;; Group 2: Dedup on re-sync

(ert-deftest jabber-test-mam-dedup-resync ()
  "Running 3650 messages twice yields exactly 3650 rows."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (count 3650)
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil))
      ;; First pass, then a re-sync
      (dotimes (_pass 2)
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

;;; Group 3: Transaction batching

(ert-deftest jabber-test-mam-transaction-batching ()
  "Each processed message commits in its own transaction."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (batch-count 500)
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (execute (symbol-function 'sqlite-execute))
           (commits 0))
      (cl-letf (((symbol-function 'sqlite-execute)
                 (lambda (db sql &rest args)
                   (when (equal sql "COMMIT")
                     (cl-incf commits))
                   (apply execute db sql args))))
        (dotimes (i batch-count)
          (jabber-mam--process-message
           jc (jabber-test-mam--make-message i))
          (should (jabber-test-archive--idle-p))))
      (should (= batch-count commits))
      (let ((rows (jabber-db-query "me@example.com" "friend@example.com"
                                   0 (+ 1700000000 (* batch-count 86400))
                                   -1)))
        (should (= batch-count (length rows)))))))

(ert-deftest jabber-test-mam-encrypted-session-save-in-transaction ()
  "An encrypted MAM message can migrate its session inside the MAM transaction."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
           (jabber-muc-participants nil)
           (stanza (jabber-test-mam--make-message 1))
           (inner (nth 2 (jabber-mam--parse-result stanza)))
           (envelope (unibyte-string ?J ?O ?M ?E ?M ?O 0 1 0)))
      (setcdr (last inner) '((encrypted ((xmlns . "eu.siacs.conversations.axolotl")))))
      (sqlite-execute jabber-db--connection "\
INSERT INTO omemo_skipped_keys
  (account, jid, device_id, dh_key, message_number, message_key, created_at)
  VALUES ('me@example.com', 'friend@example.com', 7, 'dh', 3, 'mk', 0)")
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc message)
                   (should-not (jabber-test-archive--idle-p))
                   (jabber-omemo-store-save-session-and-clear-legacy-keys
                    "me@example.com" "friend@example.com" 7 envelope)
                   (jabber-chat--set-body message "decrypted"))))
        (jabber-mam--process-message jc stanza))
      (should (jabber-test-archive--idle-p))
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
      (dotimes (i 10)
        (jabber-mam--process-message
         jc (jabber-test-mam--make-muc-message i room "mynick")))
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
      (jabber-mam--process-message
       jc (jabber-test-mam--make-muc-message 0 room "mynick"))  ; from us (idx%3=0)
      (jabber-mam--process-message
       jc (jabber-test-mam--make-muc-message 1 room "mynick")) ; from other (idx%3=1)
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
           (jabber-chat--crypto-loaded t)
           (stanza (jabber-test-mam--make-message 1)))
      (jabber-mam--process-message jc stanza)
      (should-not (cddr stanza)))))

(ert-deftest jabber-test-mam-reaction-reply-fallback-not-stored ()
  "Conversations reaction fallback from MAM is not stored as chat text."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
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
      (let ((unwrapped (car (jabber-test-mam--dispatched jc stanza))))
        (should-not (sqlite-select (jabber-db-ensure-open)
                      "SELECT body FROM message WHERE server_id = ?"
                      '("archive-reaction-1")))
        (should (jabber-xml-get-attribute unwrapped 'jabber-mam--origin))
        (should (car (jabber-xml-get-children unwrapped 'reactions)))))))

(ert-deftest jabber-test-mam-author-retract-fallback-unwrapped-not-stored ()
  "Archived author retraction is unwrapped but never stored as chat text."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))

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
      (let ((unwrapped (car (jabber-test-mam--dispatched jc stanza))))
        (cl-letf (((symbol-function
                    'jabber-moderation--room-supports-occupant-id-p)
                   (lambda (_room owner) (eq owner jc))))
          (should (jabber-moderation--handle-message jc unwrapped)))
        (should
         (equal '(("original message" "room@conference.example/alice"))
                (sqlite-select
                 (jabber-db-ensure-open)
                 "SELECT body, retracted_by FROM message ORDER BY id")))
        (should (jabber-xml-get-attribute unwrapped 'jabber-mam--origin))
        (should (jabber-moderation--muc-retraction-message-p unwrapped))))))

(ert-deftest jabber-test-mam-bodyless-stanza-unwrapped ()
  "Bodyless MAM result is unwrapped with original sender and MAM marker."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc jabber-test-mam-queryid))
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
      (let ((unwrapped (car (jabber-test-mam--dispatched jc stanza))))
        ;; The dispatched stanza has the inner message's from
        (should (string= "alice@example.com/res"
                         (jabber-xml-get-attribute unwrapped 'from)))
        ;; MAM origin marker should be set
        (should (jabber-xml-get-attribute unwrapped 'jabber-mam--origin))
        ;; Archive id is needed by downstream tombstone handlers.
        (should (equal "archive-001"
                       (jabber-xml-get-attribute unwrapped 'jabber-mam--archive-id)))
        ;; The receipt element should be a child
        (should (car (jabber-xml-get-children unwrapped 'received)))))))

;;; Group 9: query ID validation

(ert-deftest jabber-test-mam-unknown-queryid-rejected ()
  "MAM result with unknown queryid is not processed."
  (jabber-test-mam-with-db
    (let* ((jc (jabber-test-mam--make-fake-jc "me@example.com"))
           (jabber-mam--syncing (jabber-test-mam--queries jc "known-query"))
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
            (jabber-mam--redraw-timer nil)
            (jabber-mam--redraw-peers nil)
            (jabber-mam-sync-complete-functions nil)
            (jabber-mam-peer-syncing-functions nil)
            (fsm-debug nil)
            sent)
       (cl-letf (((symbol-function 'jabber-send-sexp)
                  (lambda (_jc xml &rest _) (push xml sent))))
         (unwind-protect (progn ,@body)
           (jabber-mam--cleanup-all)
           (when jabber-mam--redraw-timer
             (cancel-timer jabber-mam--redraw-timer)))))))

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
          (should (jabber-test-archive--idle-p))
          (jabber-mam--cancel-muc-query "room@example.com")
          (should (jabber-test-archive--idle-p))
          (jabber-test-mam--reply jc room-request "room@example.com" error-first)
          (jabber-test-mam--reply jc room-request "room@example.com" (not error-first))
          (should (jabber-test-archive--idle-p))
          (should (jabber-mam-syncing-p))
          (jabber-test-mam--reply jc other-request)
          (should (jabber-test-archive--idle-p))
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
        (should (jabber-test-archive--idle-p))))))

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
              (should (jabber-test-archive--idle-p)))
            (when room
              (jabber-test-mam--reply jc request nil failure)
              (should (eq pending (assoc id jabber-open-info-queries))))
            (jabber-test-mam--reply jc request room failure)
            (jabber-test-mam--reply jc request room failure)
            (should (= 1 calls))
            (should (jabber-test-archive--idle-p))
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
      (let ((calls 0) outcomes)
        (jabber-mam--query jc nil "pages" nil nil "room@example.com" nil 7
                           (lambda () (cl-incf calls))
                           (lambda (outcome) (push outcome outcomes)))
        (let ((first (car sent))
              (query (car jabber-mam--syncing)))
          (jabber-test-mam--reply jc first "room@example.com" nil t)
          (should (= 0 calls))
          (should-not outcomes)
          (should (jabber-test-archive--idle-p))
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
                  (should (jabber-test-archive--idle-p))
                  (let* ((payload (car (jabber-xml-get-children (car sent) 'query)))
                         (rsm (car (jabber-xml-get-children payload 'set))))
                    (should (equal '(after nil "next-page")
                                   (car (jabber-xml-get-children rsm 'after))))
                    (should (equal '(max nil "7")
                                   (car (jabber-xml-get-children rsm 'max)))))
                  (jabber-test-mam--reply jc (car sent) "room@example.com"))
              (should (= 1 (length sent))))
            (should (= 1 calls))
            (should (equal (list (if (eq ending 'success) 'success 'cancelled))
                           outcomes))
            (apply function args)
            (should (= 1 (length outcomes)))
            (should-not (jabber-mam-syncing-p))
            (should-not jabber-open-info-queries)
            (should (jabber-test-archive--idle-p))))))))

(ert-deftest jabber-test-mam-native-cancelled-callbacks-preserve-transaction ()
  "Captured stale callbacks cannot settle a surviving query or its cursor."
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
          (should (jabber-test-archive--idle-p))
          (let ((reader (sqlite-open jabber-db-path)))
            (unwind-protect
                (progn
                  ;; The result committed on arrival; its cursor waits for fin.
                  (should (equal '(("Message 1"))
                                 (sqlite-select reader "SELECT body FROM message")))
                  (should-not (sqlite-select reader "SELECT uid FROM mam_progress"))
                  (jabber-test-mam--reply jc request)
                  (should (sqlite-select reader "SELECT uid FROM mam_progress")))
              (sqlite-close reader)))
          (should (= 2 calls))
          (should-not jabber-open-info-queries))))))

(ert-deftest jabber-test-mam-native-query-start-failures ()
  "Failed send settles only the owned query and touches no transaction."
  (dolist (failure '(send quit))
    (jabber-test-mam--with-native
      (let ((calls 0) outcomes returned quit-seen)
        ;; Another writer's open transaction is neither joined nor ended.
        (sqlite-execute jabber-db--connection "BEGIN")
        (cl-letf (((symbol-function 'jabber-send-sexp)
                   (lambda (&rest _)
                     (if (eq failure 'quit) (signal 'quit nil)
                       (error "Injected send failure")))))
          (condition-case nil
              (progn
                (jabber-mam--query
                 jc nil nil nil nil nil nil nil
                 (lambda () (cl-incf calls))
                 (lambda (outcome) (push (list outcome returned) outcomes)))
                (setq returned t))
            (quit (setq quit-seen t))))
        (should (= 1 calls))
        (should (eq quit-seen (eq failure 'quit)))
        (should (equal (list (list (if quit-seen 'cancelled 'failed) nil)) outcomes))
        (should-not (jabber-mam-syncing-p))
        (should-not jabber-open-info-queries)
        (should-not (jabber-test-archive--idle-p))
        (sqlite-execute jabber-db--connection "ROLLBACK")))))

(ert-deftest jabber-test-mam-native-stale-cursor-retry ()
  "A stale cursor retry retains filters and callback and cannot loop forever."
  (dolist (failure '(nil t))
    (jabber-test-mam--with-native
      (let ((calls 0) outcomes)
        (jabber-mam--query jc "expired" "retry" "friend@example.com"
                           "2020-01-01T00:00:00Z" nil nil 7
                           (lambda () (cl-incf calls))
                           (lambda (outcome) (push outcome outcomes)))
        (cl-labels ((stale ()
                     (fsm-send-sync
                      jc `(:stanza
                           (iq ((type . "error")
                                (id . ,(jabber-xml-get-attribute (car sent) 'id)))
                               (error () (item-not-found ())))))))
          (stale)
          (should (= 0 calls))
          (should-not outcomes)
          (should (jabber-test-archive--idle-p))
          (if failure (stale) (jabber-test-mam--reply jc (car sent))))
        (should (= 2 (length sent)))
        (should (= 1 calls))
        (should (equal (list (if failure 'failed 'success)) outcomes))
        (should-not (jabber-mam-syncing-p))
        (should-not jabber-open-info-queries)
        (let ((xml (jabber-sexp2xml (car sent))))
          (should (string-match-p "friend@example.com" xml))
          (should (string-match-p "2020-01-01T00:00:00Z" xml))
          (should-not (string-match-p "<after>" xml)))))))

(ert-deftest jabber-test-mam-native-reentrant-decrypt-retirement ()
  "Retirement during decryption prevents storage and downstream dispatch."
  (jabber-test-mam--with-native
    (let* ((jabber-test-mam-queryid "retire")
           (dispatched nil)
           (jabber-message-chain (list (lambda (_jc xml) (push xml dispatched))))
           ;; A bodyless receipt would otherwise be dispatched downstream.
           (receipt (jabber-test-mam--make-message 1)))
      (jabber-mam--query jc nil "retire")
      (setf (nthcdr 2 (nth 3 (nth 2 (nth 2 receipt))))
            '((received ((xmlns . "urn:xmpp:receipts") (id . "x")))))
      (cl-letf (((symbol-function 'jabber-chat--decrypt-if-needed)
                 (lambda (_jc xml) (jabber-mam--cleanup-connection jc) xml)))
        (jabber-mam--process-message jc (jabber-test-mam--make-message 2))
        (jabber-mam--process-message jc receipt))
      (should-not dispatched)
      (should (jabber-test-archive--idle-p))
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
                                              "SELECT count(*) FROM message"))))
            (should (jabber-db-message-thread-known-p
                     "me@example.com" peer type "thread"))))))))

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
        (should (jabber-test-archive--idle-p))
        (should (= 1 (length jabber-open-info-queries)))
        (jabber-test-mam--reply other request)
        (should (= 2 calls))
        (should (jabber-test-archive--idle-p))
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
            (should (jabber-test-archive--idle-p))
            (let ((reader (sqlite-open jabber-db-path)))
              (unwind-protect
                  (progn
                    ;; Writes outside a result are never held by a MAM page.
                    (should (= 1 (caar (sqlite-select reader "SELECT count(*) FROM message"))))
                    (when b-waiting
                      (let ((timer (nth 3 survivor)))
                        (should (memq timer timer-list))
                        (cancel-timer timer)
                        (apply (timer--function timer) (timer--args timer))))
                    (should (jabber-test-archive--idle-p))
                    (should (= 1 (length jabber-open-info-queries)))
                    (let* ((jabber-test-mam-queryid "survivor")
                           (xml (jabber-test-mam--make-message 1 room "groupchat")))
                      (setf (cadr xml) `((from . ,room)))
                      (jabber-mam--process-message other xml))
                    (jabber-test-mam--reply
                     other (if b-waiting (car sent) (nth 1 survivor)) room)
                    (should (= 1 b-calls))
                    (should (jabber-test-archive--idle-p))
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
        (should (jabber-test-archive--idle-p))
        (should (= 1 (length jabber-open-info-queries)))))))

(ert-deftest jabber-test-mam-native-cleanup-callback-faults ()
  "Error or quit cannot strand selected pages, timers, IQs or physical writes."
  (dolist (ending '(lost room all))
    (dolist (failure '(error quit))
      (jabber-test-mam--with-native
        (jabber-test-mam--with-lifecycle
          (let ((room "room@example.com") (calls nil) outcomes captures survivor)
            (jabber-mam--query other nil "foreign" nil nil nil nil nil
                               (lambda () (push 'foreign calls))
                               (lambda (outcome) (push (cons 'foreign outcome) outcomes)))
            (setq survivor (car sent))
            ;; The faulting page is selected first, before another active page
            ;; and a waiting timer.  Every callback faults independently.
            (dolist (id '(waiting active fault))
              (let ((query (jabber-mam--query
                            jc nil nil nil nil room nil nil
                            (lambda () (push id calls) (signal failure nil))
                            (lambda (outcome)
                              (push (cons id outcome) outcomes)
                              (signal failure nil)))))
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
                (should (= 1 (cl-count id calls)))
                (should (= 1 (cl-count (cons id 'cancelled) outcomes :test #'equal))))
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
                        (should (jabber-test-archive--idle-p))
                      (should (jabber-test-archive--idle-p))
                      (should (= 1 (length jabber-open-info-queries)))
                      (should (equal '(("accepted"))
                                     (sqlite-select reader "SELECT body FROM message")))
                      (jabber-test-mam--reply other survivor))
                    (should (equal '(("accepted"))
                                   (sqlite-select reader "SELECT body FROM message"))))
                (sqlite-close reader)))
            (should (= 1 (cl-count 'foreign calls)))
            (should (= 4 (length outcomes)))
            (should (equal (if (eq ending 'all) 'cancelled 'success)
                           (alist-get 'foreign outcomes)))
            (should-not jabber-mam--syncing)
            (should-not jabber-open-info-queries)
            (should (jabber-test-archive--idle-p))))))))

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
        (should (jabber-test-archive--idle-p))))))

(ert-deftest jabber-test-mam-native-public-disconnect-settlement ()
  "Real voluntary single/all-account and lost-session paths settle personal MAM."
  (should (memq #'jabber-mam--cleanup-connection
                jabber-lifecycle-session-reset-functions))
  (dolist (ending '(one all lost))
    (dolist (waiting '(nil t))
      (jabber-test-mam--with-native
        (jabber-test-mam--with-lifecycle
          (let ((a-calls 0) (b-calls 0) a-outcomes b-outcomes)
            (jabber-mam--query other nil "foreign" nil nil nil nil nil
                               (lambda () (cl-incf b-calls))
                               (lambda (outcome) (push outcome b-outcomes)))
            (let* ((survivor (car sent))
                   (query (jabber-mam--query
                           jc nil "personal" nil nil nil nil nil
                           (lambda () (cl-incf a-calls))
                           (lambda (outcome) (push outcome a-outcomes))))
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
              (should (equal '(cancelled) a-outcomes))
              (let ((reader (sqlite-open jabber-db-path)))
                (unwind-protect
                    (progn
                      (if (eq ending 'all)
                          (progn (should-not (get other :state))
                                 (should-not jabber-connections))
                        (should (= 0 b-calls))
                        (should-not b-outcomes)
                        (should (eq :session-established (get other :state)))
                        (should (memq other jabber-connections))
                        (should (jabber-test-archive--idle-p))
                        (should (= 1 (length jabber-open-info-queries)))
                        (should (equal '(("accepted"))
                                       (sqlite-select reader "SELECT body FROM message")))
                        (jabber-test-mam--reply other survivor))
                      (should (equal '(("accepted"))
                                     (sqlite-select reader "SELECT body FROM message"))))
                  (sqlite-close reader)))
              (should (= 1 b-calls))
              (should (jabber-test-archive--idle-p))
              (should (equal (list (if (eq ending 'all) 'cancelled 'success))
                             b-outcomes))
              (should-not jabber-mam--syncing)
              (should-not jabber-open-info-queries))))))))

;;; Explicit terminal outcome contract

(ert-deftest jabber-test-mam-native-terminal-outcomes ()
  "Report query success, failure and cancellation without changing legacy arity."
  (dolist (ending '(success bounded failed cancelled))
    (jabber-test-mam--with-native
      (let* ((calls 0) observations
             (query (jabber-mam--query
                     jc nil "outcome" nil nil "room@example.com"
                     (eq ending 'bounded) 1
                     (lambda () (cl-incf calls))
                     (lambda (outcome)
                       (push (list outcome calls (length jabber-mam--syncing)
                                   (jabber-test-archive--idle-p)
                                   (length jabber-open-info-queries))
                             observations))))
             (request (car sent))
             (jabber-test-mam-queryid "outcome")
             (xml (jabber-test-mam--make-message 1))
             (refreshes nil)
             (jabber-mam-sync-complete-functions
              (list (lambda (peers) (push peers refreshes)))))
        (setf (cadr xml) '((from . "room@example.com")))
        (jabber-mam--process-message jc xml)
        (pcase ending
          ('cancelled (jabber-mam--cancel-muc-query "room@example.com" jc))
          (_ (jabber-test-mam--reply jc request "room@example.com"
                                    (eq ending 'failed) (eq ending 'bounded))))
        (jabber-test-mam--reply jc request "room@example.com")
        (jabber-test-mam--reply jc request "room@example.com" t)
        (should (equal (list (list (if (eq ending 'bounded) 'success ending)
                                  1 0 t 0)) observations))
        (should-not (memq query jabber-mam--syncing))
        (should (= 1 calls))
        (should (= 1 (length refreshes)))
        (jabber-db-close)
        (should (equal '(("Message 1"))
                       (sqlite-select (jabber-db-ensure-open)
                                      "SELECT body FROM message")))))))

(ert-deftest jabber-test-mam-native-nonprogress-outcome ()
  "An incomplete forward page without a new cursor fails, not succeeds."
  (dolist (last '(nil "same"))
    (jabber-test-mam--with-native
      (let (outcomes)
        ;; Opt in to results without a legacy settlement callback.
        (jabber-mam--query jc "same" nil nil nil nil nil nil nil
                           (lambda (outcome) (push outcome outcomes)))
        (fsm-send-sync
         jc `(:stanza
              (iq ((type . "result")
                   (id . ,(jabber-xml-get-attribute (car sent) 'id)))
                  (fin ((xmlns . ,jabber-mam-xmlns) (complete . "false"))
                       (set ((xmlns . ,jabber-mam-rsm-xmlns))
                            ,@(when last `((last () ,last))))))))
        (should (equal '(failed) outcomes))
        (should-not jabber-mam--syncing)
        (should-not jabber-open-info-queries)
        (should (jabber-test-archive--idle-p))
        (should (= 1 (length sent)))))))

(ert-deftest jabber-test-mam-native-cancel-then-new-query-outcomes ()
  "Cancel a room, start unrelated work, then replay stale fin/error both ways."
  (dolist (error-first '(nil t))
    (jabber-test-mam--with-native
      (let ((a 0) (b 0) a-outcomes b-outcomes
            (jabber-test-mam-queryid "new-survivor"))
        (jabber-mam--query jc nil "cancel-first" nil nil "room@example.com"
                           nil nil (lambda () (cl-incf a))
                           (lambda (outcome) (push outcome a-outcomes)))
        (let ((captured (car jabber-open-info-queries)))
          (jabber-mam--cancel-muc-query "room@example.com" jc)
          (should (= 1 a))
          (should (equal '(cancelled) a-outcomes))
          (should (jabber-test-archive--idle-p))
          (let* ((survivor (jabber-mam--query
                            jc nil jabber-test-mam-queryid nil nil nil nil nil
                            (lambda () (cl-incf b))
                            (lambda (outcome) (push outcome b-outcomes))))
                 (request (car sent))
                 (pending (car jabber-open-info-queries))
                 (reader (sqlite-open jabber-db-path)))
            (unwind-protect
                (progn
                  (jabber-mam--process-message jc (jabber-test-mam--make-message 1))
                  (dolist (index (if error-first '(2 1) '(1 2)))
                    (let ((callback (nth index captured)))
                      (funcall (car callback) jc
                               '(iq ((from . "room@example.com"))
                                    (fin ((xmlns . "urn:xmpp:mam:2")
                                          (complete . "true"))))
                               (cdr callback))))
                  (should (= 1 a))
                  (should (= 0 b))
                  (should (equal '(cancelled) a-outcomes))
                  (should-not b-outcomes)
                  (should (jabber-test-archive--idle-p))
                  (should (equal (list survivor) jabber-mam--syncing))
                  (should (equal (list pending) jabber-open-info-queries))
                  ;; The survivor's result committed on arrival; stale
                  ;; callbacks could not settle its cursor.
                  (should (equal '(("Message 1"))
                                 (sqlite-select reader "SELECT body FROM message")))
                  (should-not (sqlite-select reader "SELECT uid FROM mam_progress"))
                  (jabber-test-mam--reply jc request)
                  (should (= 1 a))
                  (should (= 1 b))
                  (should (equal '(success) b-outcomes))
                  (should (jabber-test-archive--idle-p))
                  (should (equal '(("Message 1"))
                                 (sqlite-select reader "SELECT body FROM message"))))
              (sqlite-close reader))))))))

(ert-deftest jabber-test-mam-native-outcome-callback-reentry ()
  "Each consumer can reenter then error or quit without leaking query outcomes."
  (dolist (consumer '(legacy result))
    (dolist (failure '(error quit))
      (jabber-test-mam--with-native
        (let ((calls 0) outcomes nested-outcomes foreign-outcomes
              observations capture)
          (jabber-mam--query other nil "foreign" nil nil nil nil nil nil
                             (lambda (outcome) (push outcome foreign-outcomes)))
          (let ((foreign (car jabber-mam--syncing))
                (foreign-iq (car jabber-open-info-queries))
                (foreign-request (car sent)))
            (cl-labels
                ((reenter ()
                   ;; Retirement is observable before either consumer.  Replay
                   ;; the captured page and cleanup while the old stack exists.
                   (push (list (memq (car capture) jabber-mam--syncing)
                               (plist-get (car capture) :transaction)) observations)
                   (jabber-test-mam--deliver-late-page capture)
                   (jabber-mam--cleanup-connection jc)
                   (jabber-mam--query
                    jc nil "nested" nil nil nil nil nil nil
                    (lambda (outcome) (push outcome nested-outcomes)))
                   (jabber-test-mam--reply jc (car sent) nil t)
                   (signal failure nil)))
              (let ((query (jabber-mam--query
                            jc nil "outer" nil nil nil nil nil
                            (lambda ()
                              (cl-incf calls)
                              (when (eq consumer 'legacy) (reenter)))
                            (lambda (outcome)
                              (push outcome outcomes)
                              (when (eq consumer 'result) (reenter))))))
                (setq capture (jabber-test-mam--capture-page query (car sent)))
                (jabber-test-mam--reply jc (car sent))))
            (should (equal '((nil nil)) observations))
            (should (= 1 calls))
            (should (equal '(success) outcomes))
            (should (equal '(failed) nested-outcomes))
            (should-not foreign-outcomes)
            (should (equal (list foreign) jabber-mam--syncing))
            (should (equal (list foreign-iq) jabber-open-info-queries))
            (should (jabber-test-archive--idle-p))
            (jabber-test-mam--deliver-late-page capture)
            (should (= 1 (length outcomes)))
            (jabber-test-mam--reply other foreign-request)
            (should (equal '(success) foreign-outcomes))
            (should (jabber-test-archive--idle-p))))))))

;;; Progressive room catch-up display

(defun jabber-test-mam--room-message (jc request room index &optional body)
  "Deliver room archive message INDEX for REQUEST through JC.
ROOM is the archive JID.  BODY, when non-nil, replaces the message body."
  (let* ((jabber-test-mam-queryid
          (jabber-xml-get-attribute
           (car (jabber-xml-get-children request 'query)) 'queryid))
         (xml (jabber-test-mam--make-message index room "groupchat")))
    (setf (cadr xml) `((from . ,room)))
    (when body
      (let ((inner (nth 2 (jabber-mam--parse-result xml))))
        (setcar (cddr (car (jabber-xml-get-children inner 'body))) body)))
    (jabber-mam--process-message jc xml)))

(defun jabber-test-mam--room-page (jc request room index complete)
  "Deliver room archive message INDEX and REQUEST's fin through JC.
ROOM is the archive JID.  COMPLETE marks the last page."
  (jabber-test-mam--room-message jc request room index)
  (fsm-send-sync
   jc `(:stanza
        (iq ((type . "result") (from . ,room)
             (id . ,(jabber-xml-get-attribute request 'id)))
            (fin ((xmlns . ,jabber-mam-xmlns)
                  (complete . ,(if complete "true" "false")))
                 (set ((xmlns . ,jabber-mam-rsm-xmlns))
                      (last () ,(format "archive-%06d" index))))))))

(defun jabber-test-mam--wait-for (predicate)
  "Run timers until PREDICATE returns non-nil or two seconds pass."
  (let ((deadline (+ (float-time) 2)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest jabber-test-mam-room-catch-up-is-progressive ()
  "Committed room pages appear while catch-up continues, with an indicator."
  (jabber-test-mam--with-native
    (let* ((room "room@example.com")
           (jabber-mam-page-size 1)
           (jabber-mam--redraw-delay 0.05)
           (jabber-mam-sync-complete-functions
            (list #'jabber-chat--handle-mam-sync-complete))
           (jabber-chat-mode-hook nil)
           (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
           (jabber-jid-obarray (make-vector 127 0))
           (jabber-groupchat-buffer-format " *jabber-test-mam-%a-%j*")
           (buffer (jabber-muc-create-buffer jc room))
           (foreign (jabber-muc-create-buffer other room)))
      (unwind-protect
          (cl-letf (((symbol-function 'jabber-disco-get-info)
                     (lambda (jc _to _node callback closure)
                       (funcall callback jc closure
                                (list nil (list jabber-mam-xmlns))))))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "unsent draft"))
            (jabber-mam-muc-joined jc room)
            (should (buffer-local-value 'jabber-chat-mam-syncing buffer))
            (should-not (buffer-local-value 'jabber-chat-mam-syncing foreign))
            (jabber-test-mam--room-page jc (car sent) room 1 nil)
            ;; Page 1 is committed; the next page is still outstanding.
            (should (jabber-test-mam--wait-for
                     (lambda () (= 2 (length sent)))))
            (should (jabber-test-mam--wait-for
                     (lambda ()
                       (with-current-buffer buffer
                         (string-search "Message 1" (buffer-string))))))
            (should (jabber-mam-syncing-p))
            (should (buffer-local-value 'jabber-chat-mam-syncing buffer))
            (with-current-buffer foreign
              (should-not (string-search "Message 1" (buffer-string))))
            (jabber-test-mam--room-page jc (car sent) room 2 t)
            (should-not (jabber-mam-syncing-p))
            (should-not (buffer-local-value 'jabber-chat-mam-syncing buffer))
            (should (jabber-test-mam--wait-for
                     (lambda ()
                       (with-current-buffer buffer
                         (string-search "Message 2" (buffer-string))))))
            (with-current-buffer buffer
              (let ((text (buffer-string)))
                (should (= 1 (cl-count-if
                              (lambda (data)
                                (equal "Message 1" (plist-get (cadr data) :body)))
                              (ewoc-collect jabber-chat-ewoc #'identity))))
                (should (string-search "Message 1" text))
                (should (equal "unsent draft"
                               (buffer-substring-no-properties
                                jabber-point-insert (point-max)))))))
        (kill-buffer buffer)
        (kill-buffer foreign)))))

(ert-deftest jabber-test-mam-progressive-redraw-keeps-history-reader ()
  "A progressive redraw leaves a buffer alone while its reader reads history.
The refresh reloads only the newest backlog window, which can evict the
reader's anchor and move the window to the composer.  The buffer is left
for the completion refresh, which behaves as it did before progressive
display and so is not asserted to keep the anchor."
  (jabber-test-mam--with-native
    (save-window-excursion
      (let* ((room "room@example.com")
             (jabber-backlog-number 10)
             (jabber-mam-page-size 1)
             (jabber-mam--redraw-delay 0.05)
             (jabber-mam-sync-complete-functions
              (list #'jabber-chat--handle-mam-sync-complete))
             (jabber-chat-mode-hook nil)
             (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
             (jabber-jid-obarray (make-vector 127 0))
             (jabber-groupchat-buffer-format " *jabber-test-mam-%a-%j*")
             (buffer (jabber-muc-create-buffer jc room)))
        (unwind-protect
            (progn
              ;; Seed ten loaded messages from a completed page.
              (jabber-mam--query jc nil nil nil nil room nil 10)
              (let ((request (car sent)))
                (dolist (index '(1 2 4 5 7 8 10 11 13))
                  (jabber-test-mam--room-message jc request room index))
                (jabber-test-mam--room-page jc request room 14 t))
              (should (jabber-test-mam--wait-for
                       (lambda () (null jabber-mam--redraw-timer))))
              (with-current-buffer buffer
                (should (string-search "Message 1" (buffer-string)))
                (goto-char (point-max))
                (insert "draft survives"))
              (switch-to-buffer buffer)
              (let* ((window (selected-window))
                     (pos (with-current-buffer buffer
                            (ewoc-location
                             (jabber-chat-ewoc-find-by-id "archive-000001")))))
                (set-window-start window pos t)
                (set-window-point window pos)
                (with-current-buffer buffer
                  (should (equal '(msg . "archive-000001")
                                 (jabber-chat-buffer--window-anchor window))))
                ;; Page 16 commits; the catch-up continues.
                (jabber-mam--query jc "archive-000014" nil nil nil room)
                (jabber-test-mam--room-page jc (car sent) room 16 nil)
                (should jabber-mam--redraw-timer)
                (should (jabber-test-mam--wait-for
                         (lambda () (null jabber-mam--redraw-timer))))
                (should (jabber-mam-syncing-p))
                (with-current-buffer buffer
                  (should (equal '(msg . "archive-000001")
                                 (jabber-chat-buffer--window-anchor window)))
                  (should (equal "draft survives"
                                 (buffer-substring-no-properties
                                  jabber-point-insert (point-max))))
                  (should-not (string-search "Message 16" (buffer-string))))
                ;; Completion still refreshes the deferred buffer.
                (should (jabber-test-mam--wait-for
                         (lambda () (= 3 (length sent)))))
                (jabber-test-mam--room-page jc (car sent) room 17 t)
                (should-not (jabber-mam-syncing-p))
                (with-current-buffer buffer
                  (should (string-search "Message 16" (buffer-string)))
                  (should (string-search "Message 17" (buffer-string)))
                  (should (equal "draft survives"
                                 (buffer-substring-no-properties
                                  jabber-point-insert (point-max)))))))
          (kill-buffer buffer))))))

(ert-deftest jabber-test-mam-rollback-redraws-speculative-rows ()
  "A result whose transaction fails never shows its row in the buffer.
Each result commits in its own short transaction before any redraw can
read it.  A real SQLITE_FULL while storing page 2 leaves page 1 displayed,
page 2 nowhere and the draft intact."
  (jabber-test-mam--with-native
    (let* ((room "room@example.com")
           (jabber-mam-page-size 1)
           (jabber-mam--redraw-delay 0.05)
           (jabber-mam-sync-complete-functions
            (list #'jabber-chat--handle-mam-sync-complete))
           (jabber-chat-mode-hook nil)
           (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
           (jabber-jid-obarray (make-vector 127 0))
           (jabber-groupchat-buffer-format " *jabber-test-mam-%a-%j*")
           (buffer (jabber-muc-create-buffer jc room)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "unsent draft"))
            (sqlite-execute
             jabber-db--connection
             (format "PRAGMA max_page_count=%d"
                     (+ 20 (caar (sqlite-select jabber-db--connection
                                                "PRAGMA page_count")))))
            (jabber-mam--query jc nil nil nil nil room)
            (jabber-test-mam--room-page jc (car sent) room 1 nil)
            (should (jabber-test-mam--wait-for (lambda () (= 2 (length sent)))))
            (should (jabber-test-mam--wait-for
                     (lambda ()
                       (with-current-buffer buffer
                         (string-search "Message 1" (buffer-string))))))
            ;; Page 2's row arrives; it does not fit in the database.
            (should-error
             (jabber-test-mam--room-message
              jc (car sent) room 2
              (concat "Message 2 " (make-string (* 1024 1024) ?x)))
             :type 'sqlite-error)
            (should (jabber-test-archive--idle-p))
            (jabber-test-mam--reply jc (car sent) room)
            (should-not (jabber-mam-syncing-p))
            (should (equal '(("Message 1"))
                           (sqlite-select jabber-db--connection
                                          "SELECT body FROM message ORDER BY id")))
            (should (jabber-test-mam--wait-for
                     (lambda () (null jabber-mam--redraw-timer))))
            (with-current-buffer buffer
              (should (string-search "Message 1" (buffer-string)))
              (should-not (string-search "Message 2" (buffer-string)))
              (should (equal "unsent draft"
                             (buffer-substring-no-properties
                              jabber-point-insert (point-max))))))
        (kill-buffer buffer)))))

(ert-deftest jabber-test-mam-progressive-redraw-keeps-idless-history-reader ()
  "A progressive redraw leaves a history reader alone without message IDs.
Stored incoming chat messages need not carry message/@id or an XEP-0359
server-id, so the reader's window has no message anchor; it still does
not follow the composer and must not be moved there mid-sync."
  (jabber-test-mam--with-native
    (save-window-excursion
      (let* ((peer "friend@example.com")
             (jabber-backlog-number 10)
             (jabber-mam--redraw-delay 0.05)
             (jabber-chat-mode-hook nil)
             (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
             (jabber-chat-buffer-format " *jabber-test-mam-%a-%j*")
             (buffer (jabber-chat-create-buffer jc peer))
             (refreshes 0)
             (refresh (symbol-function 'jabber-chat-buffer-refresh)))
        (unwind-protect
            (progn
              (dotimes (index 10)
                (jabber-db-store-message "me@example.com" peer "in" "chat"
                                         (format "Existing message %d\n%s" index
                                                 (make-string 100 ?x))
                                         (+ 1700000000 index) "phone"))
              (with-current-buffer buffer (jabber-chat-buffer-refresh))
              (switch-to-buffer buffer)
              (let ((node (ewoc-nth jabber-chat-ewoc 0)))
                (while (eq (car (ewoc-data node)) :rare-time)
                  (setq node (ewoc-next jabber-chat-ewoc node)))
                (set-window-start (selected-window) (ewoc-location node) t)
                (set-window-point (selected-window) (ewoc-location node)))
              (should (< (window-point (selected-window)) jabber-point-insert))
              (should (jabber-chat-buffer--reading-history-p))
              (cl-letf (((symbol-function 'jabber-chat-buffer-refresh)
                         (lambda () (cl-incf refreshes) (funcall refresh))))
                (jabber-mam--query jc)
                (let* ((request (car sent))
                       (jabber-test-mam-queryid
                        (jabber-xml-get-attribute
                         (car (jabber-xml-get-children request 'query))
                         'queryid))
                       (xml (jabber-test-mam--make-message 1 peer "chat"))
                       (inner (nth 2 (jabber-mam--parse-result xml))))
                  (setf (cadr inner) (assq-delete-all 'id (cadr inner)))
                  (jabber-mam--process-message jc xml)
                  (jabber-test-mam--reply jc request nil nil t))
                (should (jabber-test-mam--wait-for
                         (lambda () (null jabber-mam--redraw-timer))))
                (should (jabber-mam-syncing-p))
                (should (= 0 refreshes))))
          (kill-buffer buffer))))))

(ert-deftest jabber-test-mam-progressive-redraw-thread-and-account-policy ()
  "Progressive redraw skips history readers in any window, per account.
A thread buffer read in one of two windows is skipped while its parent
refreshes; once both windows follow the composer it refreshes.  The
completion refresh covers a history reader.  Another account's buffers
for the same room are never touched."
  (jabber-test-mam--with-native
    (save-window-excursion
      (let* ((room "room@example.com")
             (jabber-chat-mode-hook nil)
             (jabber-message-thread-use-buffers t)
             (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
             (jabber-jid-obarray (make-vector 127 0))
             (jabber-groupchat-buffer-format " *jabber-test-mam-%a-%j*")
             (parent (jabber-muc-create-buffer jc room))
             (foreign-parent (jabber-muc-create-buffer other room))
             (thread (jabber-message-thread-create-buffer
                      jc room "groupchat" "thread-a" nil parent))
             (foreign-thread (jabber-message-thread-create-buffer
                              other room "groupchat" "thread-a" nil
                              foreign-parent))
             (peers (list (list "me@example.com" room "groupchat")))
             refreshed marked)
        (unwind-protect
            (progn
              (with-current-buffer thread
                (jabber-chat-ewoc-enter
                 (list :muc-foreign
                       (list :id "client-1" :server-id "room-1"
                             :body "Read me"
                             :from "room@example.com/occupant"
                             :msg-type "groupchat"
                             :timestamp (seconds-to-time 1700000000)))))
              (switch-to-buffer thread)
              (let* ((bottom (selected-window))
                     (history (split-window-below))
                     (insert (buffer-local-value 'jabber-point-insert thread))
                     (pos (with-current-buffer thread
                            (ewoc-location
                             (jabber-chat-ewoc-find-by-id "room-1")))))
                (set-window-buffer history thread)
                (set-window-point bottom insert)
                (set-window-start history pos t)
                (set-window-point history pos)
                (with-current-buffer thread
                  (should (jabber-chat-buffer--reading-history-p)))
                (cl-letf (((symbol-function 'jabber-chat-buffer-refresh)
                           (lambda () (push (current-buffer) refreshed)))
                          ((symbol-function
                            'jabber-db-mark-message-thread-read)
                           (lambda (&rest args) (push args marked))))
                  (jabber-chat--handle-mam-sync-complete peers t)
                  (should (memq parent refreshed))
                  (should-not (memq thread refreshed))
                  (should-not (memq foreign-parent refreshed))
                  (should-not (memq foreign-thread refreshed))
                  (should-not marked)
                  ;; Both windows follow the composer.
                  (set-window-point history insert)
                  (setq refreshed nil)
                  (jabber-chat--handle-mam-sync-complete peers t)
                  (should (memq thread refreshed))
                  (should (= 1 (length marked)))
                  (should-not (memq foreign-thread refreshed))
                  ;; Completion refreshes even a history reader.
                  (set-window-point history pos)
                  (setq refreshed nil)
                  (jabber-chat--handle-mam-sync-complete peers)
                  (should (memq thread refreshed)))))
          (dolist (buffer (list thread foreign-thread parent foreign-parent))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest jabber-test-mam-rollback-reconciles-history-reader ()
  "A history reader never sees a result whose transaction fails.
The reader scrolls into history before page 2 arrives; page 2's transaction
fails, so its row is never displayed and page 1 stays visible."
  (jabber-test-mam--with-native
    (save-window-excursion
      (let* ((room "room@example.com")
             (jabber-mam-page-size 1)
             (jabber-mam--redraw-delay 0.05)
             (jabber-chat-mode-hook nil)
             (jabber-mam-sync-complete-functions
              (list #'jabber-chat--handle-mam-sync-complete))
             (jabber-buffer-registry--buffers (make-hash-table :test #'equal))
             (jabber-jid-obarray (make-vector 127 0))
             (jabber-groupchat-buffer-format " *jabber-test-mam-%a-%j*")
             (buffer (jabber-muc-create-buffer jc room)))
        (unwind-protect
            (progn
              (sqlite-execute
               jabber-db--connection
               (format "PRAGMA max_page_count=%d"
                       (+ 20 (caar (sqlite-select jabber-db--connection
                                                  "PRAGMA page_count")))))
              (jabber-mam--query jc nil nil nil nil room)
              (jabber-test-mam--room-page jc (car sent) room 1 nil)
              (should (jabber-test-mam--wait-for
                       (lambda () (= 2 (length sent)))))
              (should (jabber-test-mam--wait-for
                       (lambda ()
                         (with-current-buffer buffer
                           (string-search "Message 1" (buffer-string))))))
              ;; The reader scrolls into history before page 2 settles.
              (switch-to-buffer buffer)
              (let ((pos (with-current-buffer buffer
                           (ewoc-location
                            (jabber-chat-ewoc-find-by-id "archive-000001")))))
                (set-window-start (selected-window) pos t)
                (set-window-point (selected-window) pos))
              (with-current-buffer buffer
                (should (jabber-chat-buffer--reading-history-p)))
              (should-error
               (jabber-test-mam--room-message
                jc (car sent) room 2
                (concat "Message 2 " (make-string (* 1024 1024) ?x)))
               :type 'sqlite-error)
              (jabber-test-mam--reply jc (car sent) room)
              (should-not (jabber-mam-syncing-p))
              (should (equal '(("Message 1"))
                             (sqlite-select jabber-db--connection
                                            "SELECT body FROM message ORDER BY id")))
              (should (jabber-test-mam--wait-for
                       (lambda () (null jabber-mam--redraw-timer))))
              (with-current-buffer buffer
                (should (string-search "Message 1" (buffer-string)))
                (should-not (string-search "Message 2" (buffer-string)))))
          (kill-buffer buffer))))))

(ert-deftest jabber-test-mam-peer-syncing-hook-keeps-three-arguments ()
  "Three-argument handlers still run; a failing one strands no catch-up."
  (jabber-test-mam--with-native
    (let* ((peer "friend@example.com")
           (calls nil)
           (jabber-mam-peer-syncing-functions
            (list (lambda (_peer _type _syncing-p) (error "Handler bug"))
                  (lambda (peer type syncing-p)
                    (push (list peer type syncing-p) calls)))))
      (cl-letf (((symbol-function 'jabber-disco-get-info)
                 (lambda (jc _to _node callback closure)
                   (funcall callback jc closure
                            (list nil (list jabber-mam-xmlns))))))
        (jabber-mam-chat-opened jc peer)
        (should (equal (list (list peer "chat" t)) calls))
        (should (= 1 (length sent)))
        (jabber-test-mam--reply jc (car sent))
        (should (equal (list peer "chat" nil) (car calls)))
        (should-not jabber-mam--peer-syncing)
        (should-not (jabber-mam-syncing-p))))))

(provide 'jabber-test-mam)

;;; jabber-test-mam.el ends here
