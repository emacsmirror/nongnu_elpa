;;; jabber-test-omemo-admission.el --- Native inbound admission -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real ratchets and SQLite, intercepting only external effects.

;;; Code:
(require 'ert)
(require 'jabber-chat)
(require 'jabber-omemo)
(require 'jabber-archive-test-helpers)
(require 'jabber-receipts)
(require 'jabber-chatstates)

(defun jabber-test-omemo-admission--store ()
  "Return a disposable native store."
  (jabber-omemo-deserialize-store (jabber-omemo-setup-store)))

(defun jabber-test-omemo-admission--initiate (sender receiver)
  "Initiate SENDER toward RECEIVER using their native stores."
  (let* ((bundle (jabber-omemo-get-bundle receiver))
         (pk (car (plist-get bundle :pre-keys))))
    (jabber-omemo-initiate-session
     sender (plist-get bundle :signature) (plist-get bundle :signed-pre-key)
     (plist-get bundle :identity-key) (cdr pk)
     (plist-get bundle :signed-pre-key-id) (car pk))))

(defun jabber-test-omemo-admission--wire (session text &optional count)
  "Return native encrypted TEXT from SESSION, advancing COUNT times."
  (let* ((payload (jabber-omemo-encrypt-message (encode-coding-string text 'utf-8)))
         encrypted)
    (dotimes (_ (or count 1))
      (setq encrypted (jabber-omemo-encrypt-key session (plist-get payload :key))))
    (list :sid 7 :iv (plist-get payload :iv) :payload (plist-get payload :ciphertext)
          :keys (list (cons 9 encrypted)))))

(defun jabber-test-omemo-admission--unwrap (session store parsed)
  "Decrypt PARSED's native wrapped key using SESSION and STORE."
  (let ((entry (cdar (plist-get parsed :keys))))
    (jabber-omemo-decrypt-key session store (plist-get entry :pre-key-p)
                             (plist-get entry :data))))

(defun jabber-test-omemo-admission--dispatch-rejected (parsed)
  "Prove refused PARSED never creates a successful dedup entry."
  (let ((jabber-chat--decrypt-cache (make-hash-table :test #'equal))
        (jabber-chat--crypto-loaded t)
        (jabber-chat-decrypt-handlers nil)
        (jabber-chat--sorted-decrypt-handlers-cache nil))
    (jabber-chat-register-decrypt-handler
     'omemo :detect #'jabber-omemo--detect-encrypted
     :decrypt #'jabber-omemo--decrypt-handler :priority 10 :error-label "OMEMO")
    (dolist (payload (list (plist-get parsed :payload) nil))
      (let ((stanza
             `(message ((from . "a@example.invalid/device") (type . "chat"))
                       (encrypted ((xmlns . ,jabber-omemo-xmlns))
                                  (header ((sid . "7"))
                                          ,@(mapcar
                                             (lambda (entry)
                                               `(key ((rid . ,(number-to-string (car entry)))
                                                      (prekey . "true"))
                                                     ,(base64-encode-string
                                                       (plist-get (cdr entry) :data) t)))
                                             (plist-get parsed :keys))
                                          (iv () ,(base64-encode-string
                                                   (plist-get parsed :iv) t)))
                                  ,@(when payload
                                      `((payload () ,(base64-encode-string payload t))))))))
        (dotimes (_ 2)
          (let ((result (jabber-chat--decrypt-if-needed 'jc (copy-tree stanza))))
            (if payload
                (should (equal (caddr (assq 'body (cddr result)))
                               "[OMEMO: could not decrypt]"))
              (should-not (assq 'body (cddr result))))))
        (should (zerop (hash-table-count jabber-chat--decrypt-cache)))))))

(defun jabber-test-omemo-admission--case (kind &optional trust no-session)
  "Exercise KIND with stored TRUST and optionally NO-SESSION."
  (let* ((jabber-db-path nil)
         (jabber-db--connection nil)
         (db (sqlite-open))
         (a (jabber-test-omemo-admission--store))
         (other (jabber-test-omemo-admission--store))
         (b (jabber-test-omemo-admission--store))
         (identity (plist-get (jabber-omemo-get-bundle a) :identity-key))
         (a-session (jabber-test-omemo-admission--initiate a b))
         (accepted (jabber-omemo-make-session))
         (jabber-omemo--sessions (make-hash-table :test #'equal))
         (jabber-omemo--stores (make-hash-table :test #'equal))
         (jabber-omemo--device-ids (make-hash-table :test #'equal))
         (jabber-omemo--pending-prekey-removals (make-hash-table :test #'equal))
         (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
         (jabber-chat--decrypt-consumed-p nil)
         (slot (jabber-omemo--session-key "b@example.invalid" "a@example.invalid" 7))
         (xml (copy-tree '(message ((from . "a@example.invalid/device") (type . "chat")))))
         sent scheduled recovered skipped persisted
         (persist-store (symbol-function 'jabber-omemo--persist-store)))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-db-ensure-open) (lambda () db))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "b@example.invalid"))
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_ stanza) (push stanza sent)))
                  ((symbol-function 'jabber-omemo--schedule-prekey-flush)
                   (lambda (_) (push t scheduled)))
                  ((symbol-function 'jabber-omemo--recover-prekey-failure)
                   (lambda (&rest _) (push t recovered)))
                  ((symbol-function 'jabber-omemo--persist-store)
                   (lambda (jc) (push jc persisted) (funcall persist-store jc))))
          (jabber-db--init-schema db)
          (jabber-test-omemo-admission--unwrap
           accepted b (jabber-test-omemo-admission--wire a-session "original"))
          (jabber-test-omemo-admission--unwrap
           a-session a (jabber-test-omemo-admission--wire accepted "ack"))
          (setq skipped (jabber-test-omemo-admission--wire a-session "skipped"))
          (jabber-test-omemo-admission--unwrap
           accepted b (jabber-test-omemo-admission--wire a-session "later"))
          (should (= 1 (length (jabber-omemo--session-skipped-keys accepted))))
          (puthash "b@example.invalid" b jabber-omemo--stores)
          (puthash "b@example.invalid" 9 jabber-omemo--device-ids)
          (unless no-session
            (jabber-omemo--save-session 'jc "a@example.invalid" 7 accepted))
          (jabber-omemo--persist-store 'jc)
          (setq persisted nil)
          (unless (eq kind 'unknown)
            (jabber-omemo-store-save-trust
             "b@example.invalid" "a@example.invalid" 7 identity (or trust 2)))
          (let* ((old-blob (jabber-omemo-serialize-session accepted))
                 (old-store (jabber-omemo-serialize-store b))
                 (old-trust (jabber-omemo-store-load-trust
                             "b@example.invalid" "a@example.invalid" 7))
                 (candidate (jabber-test-omemo-admission--initiate
                             (if (memq kind '(same-key same-key-heartbeat)) a other) b))
                 (wire (jabber-test-omemo-admission--wire
                        candidate "replacement" (if (memq kind '(heartbeat same-key-heartbeat)) 53 1)))
                 (reject (memq kind '(changed heartbeat corrupt)))
                 outcome)
            (when (eq kind 'corrupt)
              (let* ((entry (cdar (plist-get wire :keys)))
                     (bytes (copy-sequence (plist-get entry :data))))
                (aset bytes (1- (length bytes)) (logxor 1 (aref bytes (1- (length bytes)))))
                (plist-put entry :data bytes)))
            (if reject
                (should-error
                 (if (eq kind 'corrupt)
                     ;; Failed authentication remains recoverable, separately
                     ;; from identity rejection through the public handler.
                     (jabber-omemo--decrypt-stanza 'jc xml wire)
                   (jabber-omemo--decrypt-handler
                    'jc xml (list :type 'omemo :parsed wire)))
                 :type (if (eq kind 'corrupt) 'jabber-omemo-prekey-failed
                         'jabber-omemo-identity-changed))
              (setq outcome (jabber-omemo--decrypt-stanza 'jc xml wire)))
            (when (memq kind '(changed heartbeat))
              (jabber-test-omemo-admission--dispatch-rejected wire))
            (should (equal old-blob (jabber-omemo-serialize-session accepted)))
            (should (equal old-store (jabber-omemo-serialize-store b)))
            (should (equal old-trust (jabber-omemo-store-load-trust
                                     "b@example.invalid" "a@example.invalid" 7)))
            (if reject
                (progn
                  (should (eq (unless no-session accepted)
                              (gethash slot jabber-omemo--sessions)))
                  (should (equal (unless no-session old-blob)
                                 (jabber-omemo-store-load-session
                                  "b@example.invalid" "a@example.invalid" 7)))
                  (should (equal old-store (jabber-omemo-store-load "b@example.invalid")))
                  (should-not jabber-chat--decrypt-consumed-p)
                  (should-not sent)
                  (should-not scheduled)
                  (should-not recovered)
                  (should-not persisted)
                  (should (zerop (hash-table-count jabber-omemo--pending-prekey-removals)))
                  ;; Both the saved skipped key and subsequent native ratchet
                  ;; progress remain usable after refusing the replacement.
                  (should (jabber-test-omemo-admission--unwrap accepted b skipped))
                  (should (jabber-test-omemo-admission--unwrap
                           accepted b (jabber-test-omemo-admission--wire a-session "still original"))))
              (should (equal "replacement" (caddr (assq 'body (cddr outcome)))))
              (should jabber-chat--decrypt-consumed-p)
              (should (= 1 (length scheduled)))
              (should (= (length sent) (if (eq kind 'same-key-heartbeat) 1 0)))
              (should (equal (jabber-omemo-serialize-session (gethash slot jabber-omemo--sessions))
                             (jabber-omemo-store-load-session
                              "b@example.invalid" "a@example.invalid" 7))))))
      (sqlite-close db))))

(ert-deftest jabber-test-omemo-admission-changed-identity-all-trust-levels ()
  (dolist (trust '(-1 0 1 2))
    (jabber-test-omemo-admission--case 'changed trust)))

(ert-deftest jabber-test-omemo-admission-known-identity-without-session ()
  (dolist (trust '(-1 0 1 2))
    (jabber-test-omemo-admission--case 'changed trust t)))

(ert-deftest jabber-test-omemo-admission-no-heartbeat-on-rejection ()
  (jabber-test-omemo-admission--case 'heartbeat))

(ert-deftest jabber-test-omemo-admission-authentication-rollback ()
  (jabber-test-omemo-admission--case 'corrupt))

(ert-deftest jabber-test-omemo-admission-same-key-reset ()
  (jabber-test-omemo-admission--case 'same-key))

(ert-deftest jabber-test-omemo-admission-same-key-heartbeat-control ()
  (jabber-test-omemo-admission--case 'same-key-heartbeat))

(ert-deftest jabber-test-omemo-admission-unknown-policy-unchanged ()
  (jabber-test-omemo-admission--case 'unknown))

(ert-deftest jabber-test-omemo-admission-missing-export-fails-closed ()
  "An old module cannot ratchet, recover, or silently skip admission."
  (let ((original (symbol-function 'jabber-omemo--session-remote-identity))
        touched)
    (unwind-protect
        (progn
          (fmakunbound 'jabber-omemo--session-remote-identity)
          (cl-letf (((symbol-function 'jabber-omemo--get-session)
                     (lambda (&rest _) (setq touched t))))
            (let ((err (should-error
                        (jabber-omemo--decrypt-key-with-session
                         'jc "a@example.invalid" 7 nil t "unused")
                        :type 'jabber-omemo-error)))
              (should (string-match-p "Rebuild.*native module"
                                      (error-message-string err))))
            (should-not touched)))
      (fset 'jabber-omemo--session-remote-identity original))))

(ert-deftest jabber-test-omemo-admission-exact-account-jid-device ()
  "Known identities on other account/JID/device tuples do not block admission."
  (let* ((db (sqlite-open))
         (a (jabber-test-omemo-admission--store))
         (b (jabber-test-omemo-admission--store))
         (other (jabber-test-omemo-admission--store))
         (wrong-id (plist-get (jabber-omemo-get-bundle other) :identity-key))
         (jabber-omemo--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-db-ensure-open) (lambda () db))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "b@example.invalid")))
          (jabber-db--init-schema db)
          (dolist (tuple '(("other@example.invalid" "a@example.invalid" 7)
                           ("b@example.invalid" "other@example.invalid" 7)
                           ("b@example.invalid" "a@example.invalid" 8)))
            (apply #'jabber-omemo-store-save-trust
                   (append tuple (list wrong-id 2))))
          (let* ((sender (jabber-test-omemo-admission--initiate a b))
                 (wire (jabber-omemo-encrypt-key sender (make-string 32 ?x)))
                 (result (jabber-omemo--decrypt-key-with-session
                          'jc "a@example.invalid" 7 b t (plist-get wire :data))))
            (should (equal (cadr result) (make-string 32 ?x)))
            (should (nth 2 result))))
      (sqlite-close db))))

(ert-deftest jabber-test-omemo-admission-simultaneous-initiation ()
  "An authenticated same-identity prekey resolves simultaneous initiation."
  (let* ((db (sqlite-open))
         (a (jabber-test-omemo-admission--store))
         (b (jabber-test-omemo-admission--store))
         (outbound (jabber-test-omemo-admission--initiate b a))
         (inbound (jabber-test-omemo-admission--initiate a b))
         (identity (plist-get (jabber-omemo-get-bundle a) :identity-key))
         (jabber-omemo--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-db-ensure-open) (lambda () db))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "b@example.invalid")))
          (jabber-db--init-schema db)
          (jabber-omemo-store-save-trust
           "b@example.invalid" "a@example.invalid" 7 identity 2)
          (jabber-omemo--save-session 'jc "a@example.invalid" 7 outbound)
          (let* ((wire (jabber-omemo-encrypt-key inbound (make-string 32 ?x)))
                 (result (jabber-omemo--decrypt-key-with-session
                          'jc "a@example.invalid" 7 b t (plist-get wire :data))))
            (should (equal (cadr result) (make-string 32 ?x)))
            (should (equal identity
                           (jabber-omemo--session-remote-identity (car result))))
            (let ((reply (jabber-omemo-encrypt-key (car result) (make-string 32 ?y))))
              (should (equal (make-string 32 ?y)
                             (jabber-omemo-decrypt-key
                              inbound a (plist-get reply :pre-key-p)
                              (plist-get reply :data)))))))
      (sqlite-close db))))

(defun jabber-test-omemo-admission--stanza (parsed id)
  "Return a direct OMEMO stanza with ID carrying native PARSED keys."
  `(message ((from . "friend@example.com/phone") (to . "me@example.com")
             (type . "chat") (id . ,id))
            (encrypted ((xmlns . ,jabber-omemo-xmlns))
                       (header ((sid . "7"))
                               ,@(mapcar
                                  (lambda (entry)
                                    `(key ((rid . "9")
                                           (prekey . ,(if (plist-get (cdr entry) :pre-key-p)
                                                          "true" "false")))
                                          ,(base64-encode-string (plist-get (cdr entry) :data) t)))
                                  (plist-get parsed :keys))
                               (iv () ,(base64-encode-string (plist-get parsed :iv) t)))
                       (payload () ,(base64-encode-string (plist-get parsed :payload) t)))))

(defmacro jabber-test-omemo-admission--with-mam (&rest body)
  "Run BODY with a file database, native OMEMO receiver and MAM state.
BODY sees SENDER-STORE and SENDER, a peer session toward our store."
  (declare (indent 0) (debug t))
  `(jabber-test-archive--with-file
     (let* ((jabber-chat--crypto-loaded t)
            (jabber-chat--decrypt-cache (make-hash-table :test #'equal))
            (jabber-chat-decrypt-handlers nil)
            (jabber-chat--sorted-decrypt-handlers-cache nil)
            (jabber-omemo--sessions (make-hash-table :test #'equal))
            (jabber-omemo--stores (make-hash-table :test #'equal))
            (jabber-omemo--device-ids (make-hash-table :test #'equal))
            (jabber-omemo--pending-prekey-removals (make-hash-table :test #'equal))
            (sender-store (jabber-test-omemo-admission--store))
            (store (jabber-test-omemo-admission--store))
            (sender (jabber-test-omemo-admission--initiate sender-store store)))
       (cl-letf (((symbol-function 'jabber-omemo--schedule-prekey-flush) #'ignore)
                 ((symbol-function 'jabber-omemo--send-heartbeat) #'ignore))
         (jabber-chat-register-decrypt-handler
          'omemo :detect #'jabber-omemo--detect-encrypted
          :decrypt #'jabber-omemo--decrypt-handler :priority 10 :error-label "OMEMO")
         (puthash "me@example.com" store jabber-omemo--stores)
         (puthash "me@example.com" 9 jabber-omemo--device-ids)
         (jabber-omemo-store-save-device-id "me@example.com" 9)
         (jabber-omemo--persist-store jc)
         ,@body))))

(defun jabber-test-omemo-admission--session ()
  "Return our in-memory session with the fixture peer's device."
  (gethash (jabber-omemo--session-key "me@example.com" "friend@example.com" 7)
           jabber-omemo--sessions))

(defun jabber-test-omemo-admission--next-decrypts (jc sender sender-store)
  "Acknowledge to SENDER, then decrypt its next regular message after restart.
JC receives it.  SENDER-STORE is the peer's store."
  (jabber-test-omemo-admission--unwrap
   sender sender-store
   (jabber-test-omemo-admission--wire (jabber-test-omemo-admission--session) "ack"))
  (clrhash jabber-omemo--sessions)
  (clrhash jabber-omemo--stores)
  (clrhash jabber-chat--decrypt-cache)
  (jabber-db-close)
  (let ((next (jabber-test-omemo-admission--wire sender "next")))
    (should-not (plist-get (cdar (plist-get next :keys)) :pre-key-p))
    (should (equal "next"
                   (caddr (assq 'body (jabber-chat--decrypt-if-needed
                                       jc (jabber-test-omemo-admission--stanza
                                           next "next"))))))))

(ert-deftest jabber-test-omemo-admission-mam-page-rollback ()
  "A rejected result forgets its ratchet state; retry and restart decrypt."
  (jabber-test-omemo-admission--with-mam
    (let ((first (jabber-test-omemo-admission--stanza
                  (jabber-test-omemo-admission--wire sender "first") "first"))
          (execute (symbol-function 'sqlite-execute))
          (reject t))
      (cl-letf (((symbol-function 'sqlite-execute)
                 (lambda (db sql &rest args)
                   (when (and reject (equal sql "COMMIT"))
                     (funcall execute db "INSERT INTO control_child VALUES (999)"))
                   (apply execute db sql args))))
        (sqlite-execute jabber-db--connection "CREATE TABLE control_parent(id INTEGER PRIMARY KEY)")
        (sqlite-execute jabber-db--connection "CREATE TABLE control_child(parent INTEGER REFERENCES control_parent(id) DEFERRABLE INITIALLY DEFERRED)")
        (dolist (attempt '(rejected retry))
          (let* ((outcome nil)
                 (query (jabber-mam--query
                         jc nil nil nil nil nil nil nil nil
                         (lambda (value) (setq outcome value))))
                 (result (jabber-test-archive--result query "uid" (copy-tree first))))
            (if (eq attempt 'rejected)
                (should-error (jabber-mam--process-message jc result) :type 'sqlite-error)
              (jabber-mam--process-message jc result))
            (jabber-test-archive--fin jc query "uid")
            (should (eq outcome (if (eq attempt 'rejected) 'failed 'success))))
          (when (eq attempt 'rejected)
            ;; Nothing durable survived, and nothing in memory claims it did.
            (should-not (jabber-omemo-store-load-session "me@example.com" "friend@example.com" 7))
            (should (zerop (hash-table-count jabber-chat--decrypt-cache)))
            (should (zerop (hash-table-count jabber-omemo--sessions)))
            (should-not (gethash "me@example.com" jabber-omemo--stores))
            (should-not (gethash "me@example.com" jabber-omemo--pending-prekey-removals))
            (setq reject nil))))
      (jabber-db-close)
      (should (equal '(("first")) (sqlite-select (jabber-db-ensure-open) "SELECT body FROM message")))
      (should (equal '("uid" nil) (jabber-test-archive--progress)))
      (should (= 1 (length (gethash "me@example.com" jabber-omemo--pending-prekey-removals))))
      (jabber-test-omemo-admission--next-decrypts jc sender sender-store))))

;; A contained storage failure in one result must not leak into later
;; results.  Each result has its own transaction: SQLite's automatic
;; rollback of the failing one leaves nothing for later writes to join.
(ert-deftest jabber-test-omemo-admission-mam-full-action-then-ciphertext ()
  "SQLITE_FULL in an archived reaction keeps a later regular OMEMO result.
The page fails without a cursor.  After space returns, its retry succeeds
and reuses the committed plaintext without ratcheting the same ciphertext
again.  The next regular ciphertext still decrypts from durable state
after a restart."
  (jabber-test-omemo-admission--with-mam
    (let* ((receiver (jabber-omemo-make-session))
           (jabber-message-chain (list #'jabber-mam--process-message
                                       #'jabber-reactions--handle-message))
           (decrypts 0)
           (decrypt-key (symbol-function 'jabber-omemo-decrypt-key))
           regular budget)
      ;; Establish and acknowledge a session, then queue a regular message.
      (jabber-test-omemo-admission--unwrap
       receiver store (jabber-test-omemo-admission--wire sender "initial"))
      (jabber-test-omemo-admission--unwrap
       sender sender-store (jabber-test-omemo-admission--wire receiver "ack"))
      (setq regular (jabber-test-omemo-admission--stanza
                     (jabber-test-omemo-admission--wire sender "regular") "regular"))
      (jabber-omemo--save-session jc "friend@example.com" 7 receiver)
      (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                               "target" 1 "phone" "target")
      (setq budget (caar (sqlite-select jabber-db--connection "PRAGMA max_page_count")))
      (sqlite-execute jabber-db--connection
                      (format "PRAGMA max_page_count=%d"
                              (+ 2 (caar (sqlite-select jabber-db--connection
                                                        "PRAGMA page_count")))))
      (cl-letf (((symbol-function 'jabber-omemo-decrypt-key)
                 (lambda (&rest args) (cl-incf decrypts) (apply decrypt-key args))))
        (dolist (attempt '(full retry))
          (when (eq attempt 'retry)
            (sqlite-execute jabber-db--connection
                            (format "PRAGMA max_page_count=%d" budget)))
          (let* ((outcome nil)
                 (query (jabber-mam--query
                         jc nil nil nil nil nil nil nil nil
                         (lambda (value) (setq outcome value))))
                 (reaction
                  (jabber-test-archive--result
                   query "reaction"
                   `(message ((from . "friend@example.com/phone") (type . "chat"))
                             (reactions ((xmlns . "urn:xmpp:reactions:0") (id . "target"))
                                        (reaction () ,(make-string (* 16 1024) ?x)))))))
            ;; SQLITE_FULL rolls back the whole transaction; the error may
            ;; surface wherever the client settles it.
            (ignore-error sqlite-error
              (jabber-mam--process-message jc reaction))
            (ignore-error sqlite-error
              (jabber-mam--process-message
               jc (jabber-test-archive--result query "regular" (copy-tree regular))))
            (ignore-error sqlite-error
              (jabber-test-archive--fin jc query "regular"))
            (should (jabber-test-archive--idle-p))
            (should (eq outcome (if (eq attempt 'full) 'failed 'success)))
            (should (= 1 decrypts))
            (jabber-db-close)
            (should (equal '(("target") ("regular"))
                           (sqlite-select (jabber-db-ensure-open)
                                          "SELECT body FROM message ORDER BY id")))
            (should (equal (and (eq attempt 'retry) '("regular" nil))
                           (jabber-test-archive--progress)))))
        (should (= 1 (caar (sqlite-select jabber-db--connection
                                          "SELECT count(*) FROM message_reaction")))))
      (jabber-test-omemo-admission--next-decrypts jc sender sender-store))))

;; Rollback must restore every in-memory effect of a result's transaction.
;; Each case below rejects the result's real COMMIT, then retries it in the
;; same process.  The stored outcome must match a run that never failed.

(defun jabber-test-omemo-admission--echo-wire (jc text)
  "Send TEXT to the fixture room on JC through native OMEMO; return its echo.
The sender's own device has no key, so only the sent-plaintext cache can
recover TEXT."
  (let ((room "room@example.com")
        (outbound (jabber-test-omemo-admission--initiate
                   (gethash "me@example.com" jabber-omemo--stores)
                   (jabber-test-omemo-admission--store)))
        wire)
    (puthash room (list (cons jc "me")) jabber-muc--rooms)
    (jabber-omemo--save-session jc "friend@example.com" 7 outbound)
    (cl-letf (((symbol-function 'jabber-omemo--check-blocking) #'ignore)
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc stanza &rest _)
                 (when (eq (car stanza) 'message)
                   (setq wire (copy-tree stanza))))))
      (jabber-omemo--send-encrypted-muc
       jc text room
       (list (list 7 "me@example.com" "friend@example.com" outbound))
       nil "own-sent"))
    (should (= 1 (hash-table-count jabber-omemo--sent-muc-plaintexts)))
    (setf (cadr wire) '((from . "room@example.com/me") (to . "me@example.com")
                       (type . "groupchat") (id . "own-sent")))
    wire))

(defun jabber-test-omemo-admission--archive (jc room inner)
  "Archive INNER from ROOM (nil for the account archive) on JC.
Return the query outcome, re-signalling nothing."
  (let* ((outcome nil)
         (query (jabber-mam--query jc nil nil nil nil room nil nil nil
                                   (lambda (value) (setq outcome value)))))
    (ignore-error sqlite-error
      (jabber-mam--process-message
       jc (jabber-test-archive--result query "uid" (copy-tree inner))))
    (ignore-error sqlite-error
      (jabber-test-archive--fin jc query "uid"))
    (should (jabber-test-archive--idle-p))
    outcome))

(defun jabber-test-omemo-admission--chat-buffer (jc)
  "Return a minimal open chat buffer for the fixture peer on JC."
  (with-current-buffer (get-buffer-create
                        (jabber-chat-get-buffer "friend@example.com" jc))
    (setq-local jabber-chatting-with "friend@example.com")
    (setq-local jabber-buffer-connection jc)
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (current-buffer)))

(ert-deftest jabber-test-omemo-admission-mam-rollback-restores-memory ()
  "A rejected result's retry stores what an unfailed run stores.
Cover each kind of in-memory state a result transaction changes."
  (dolist (kind '(plain fresh existing heartbeat echo reaction correction
                        receipt gone gone-thread))
   (ert-info ((symbol-name kind) :prefix "Kind: ")
    (jabber-test-omemo-admission--with-mam
      (let* ((jabber-muc--rooms (make-hash-table :test #'equal))
             (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
             (jabber-message-chain
              (list #'jabber-mam--process-message
                    #'jabber-reactions--handle-message
                    #'jabber-receipts--handle-message
                    #'jabber-handle-incoming-message-chatstates))
             (execute (symbol-function 'sqlite-execute))
             (decrypt-key (symbol-function 'jabber-omemo-decrypt-key))
             (decrypts 0) (heartbeats 0) (reject nil)
             (room (and (eq kind 'echo) "room@example.com"))
             (chat (and (memq kind '(receipt gone gone-thread))
                        (jabber-test-omemo-admission--chat-buffer jc)))
             (from "friend@example.com/phone")
             inner)
        (unwind-protect
            (cl-letf (((symbol-function 'sqlite-execute)
                       (lambda (db sql &rest args)
                         (when (and reject (equal sql "COMMIT"))
                           (funcall execute db "INSERT INTO control_child VALUES (999)"))
                         (apply execute db sql args)))
                      ((symbol-function 'jabber-omemo-decrypt-key)
                       (lambda (&rest args) (cl-incf decrypts) (apply decrypt-key args)))
                      ((symbol-function 'jabber-omemo--send-heartbeat)
                       (lambda (&rest _) (cl-incf heartbeats)))
                      ((symbol-function 'jabber-omemo--resolve-sender-jid)
                       (lambda (xml &rest _)
                         (jabber-jid-user (jabber-xml-get-attribute xml 'from)))))
              (sqlite-execute jabber-db--connection "CREATE TABLE control_parent(id INTEGER PRIMARY KEY)")
              (sqlite-execute jabber-db--connection "CREATE TABLE control_child(parent INTEGER REFERENCES control_parent(id) DEFERRABLE INITIALLY DEFERRED)")
              (setq inner
                    (pcase kind
                      ('plain `(message ((from . ,from) (type . "chat") (id . "p"))
                                        (body () "plain")))
                      ('fresh (jabber-test-omemo-admission--stanza
                               (jabber-test-omemo-admission--wire sender "fresh") "f"))
                      ((or 'existing 'heartbeat)
                       (let ((receiver (jabber-omemo-make-session)))
                         (jabber-test-omemo-admission--unwrap
                          receiver store (jabber-test-omemo-admission--wire sender "initial"))
                         (jabber-test-omemo-admission--unwrap
                          sender sender-store (jabber-test-omemo-admission--wire receiver "ack"))
                         (jabber-omemo--save-session jc "friend@example.com" 7 receiver)
                         (jabber-test-omemo-admission--stanza
                          (jabber-test-omemo-admission--wire
                           sender "regular" (if (eq kind 'heartbeat) 53 1))
                          "r")))
                      ('echo (jabber-test-omemo-admission--echo-wire
                              jc (make-string 4096 ?x)))
                      ('reaction `(message ((from . ,from) (type . "chat"))
                                           (reactions ((xmlns . "urn:xmpp:reactions:0")
                                                       (id . "target"))
                                                      (reaction () "+1"))))
                      ('correction `(message ((from . ,from) (type . "chat") (id . "c"))
                                             (body () "fixed")
                                             (replace ((id . "target")
                                                       (xmlns . ,jabber-message-correct-xmlns)))))
                      ('receipt `(message ((from . ,from) (type . "chat"))
                                          (displayed ((xmlns . ,jabber-chat-markers-xmlns)
                                                      (id . "out-1")))))
                      ('gone `(message ((from . ,from) (type . "chat"))
                                       (gone ((xmlns . ,jabber-chatstates-xmlns)))))
                      ('gone-thread `(message ((from . ,from) (type . "chat"))
                                              (thread () "t1")
                                              (gone ((xmlns . ,jabber-chatstates-xmlns)))))))
              (jabber-db-store-message "me@example.com" "friend@example.com" "in" "chat"
                                       "target" 1 "phone" "target")
              (jabber-db-store-message "me@example.com" "friend@example.com" "out" "chat"
                                       "hello" 2 nil "out-1")
              (when chat
                (with-current-buffer chat
                  (puthash "out-1" (ewoc-enter-last
                                    jabber-chat-ewoc
                                    (list :local (list :id "out-1" :status :sent
                                                       :timestamp (seconds-to-time 2))))
                           jabber-chat--msg-nodes)
                  (if (eq kind 'gone-thread)
                      (progn
                        (setq-local jabber-message-thread-id "t1")
                        (setq-local jabber-message-thread-peer "friend@example.com")
                        (setq-local jabber-message-thread-type "chat")
                        (jabber-buffer-registry-register
                         'thread (jabber-message-thread--key
                                  "me@example.com" "friend@example.com" "chat" "t1")))
                    (setq-local jabber-message-thread-session-id "session")
                    (jabber-db-set-chat-thread "me@example.com" "friend@example.com"
                                               "session"))))
              (let ((rows (caar (sqlite-select jabber-db--connection
                                               "SELECT count(*) FROM message"))))
                (setq decrypts 0 reject t)
                (should (eq 'failed (jabber-test-omemo-admission--archive jc room inner)))
                (should-not (jabber-test-archive--progress nil room))
                (should (= heartbeats 0))
                (setq reject nil)
                (should (eq 'success (jabber-test-omemo-admission--archive jc room inner)))
                (should (equal '("uid" nil) (jabber-test-archive--progress nil room)))
                ;; A committed replay is served from memory: no third ratchet step.
                (let ((seen decrypts))
                  (should (eq 'success (jabber-test-omemo-admission--archive jc room inner)))
                  (should (= decrypts seen)))
                (should (= decrypts (if (memq kind '(fresh existing heartbeat)) 2 0)))
                (should (= heartbeats (if (eq kind 'heartbeat) 1 0)))
                (should (= (caar (sqlite-select jabber-db--connection
                                                "SELECT count(*) FROM message"))
                           (+ rows (if (memq kind '(plain fresh existing heartbeat echo))
                                       1 0)))))
              (pcase kind
                ((or 'plain 'fresh 'existing 'heartbeat 'echo)
                 (should (equal (pcase kind
                                  ('plain "plain") ('fresh "fresh")
                                  ('echo (make-string 4096 ?x)) (_ "regular"))
                                (caar (sqlite-select jabber-db--connection
                                                     "SELECT body FROM message ORDER BY id DESC LIMIT 1")))))
                ('reaction
                 (should (equal '(("+1")) (sqlite-select jabber-db--connection
                                                         "SELECT reaction FROM message_reaction"))))
                ('correction
                 (should (equal '(("fixed" 1))
                                (sqlite-select jabber-db--connection
                                               "SELECT body, edited FROM message WHERE stanza_id = 'target'"))))
                ('receipt
                 (should (caar (sqlite-select jabber-db--connection
                                              "SELECT displayed_at FROM message WHERE stanza_id = 'out-1'")))
                 (with-current-buffer chat
                   (should (eq :displayed
                               (plist-get (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0)))
                                          :status)))))
                ('gone
                 (should-not (jabber-db-get-chat-thread "me@example.com" "friend@example.com"))
                 (should-not (buffer-local-value 'jabber-message-thread-session-id chat)))
                ('gone-thread
                 ;; One renewal is durable and current; the buffer stays findable.
                 (with-current-buffer chat
                   (should (equal "t1" jabber-message-thread-parent-id))
                   (should (equal `((,jabber-message-thread-id "t1"))
                                  (sqlite-select jabber-db--connection
                                                 "SELECT thread_id, parent_thread_id FROM message_thread")))
                   (should (eq chat (jabber-message-thread-find-buffer
                                     "me@example.com" "friend@example.com" "chat"
                                     jabber-message-thread-id))))))
              (when (memq kind '(fresh existing))
                (should (= (if (eq kind 'fresh) 1 0)
                           (length (gethash "me@example.com"
                                            jabber-omemo--pending-prekey-removals))))
                (jabber-test-omemo-admission--next-decrypts jc sender sender-store)))
          (when chat (kill-buffer chat))))))))

(ert-deftest jabber-test-omemo-admission-mam-own-echo-store-full ()
  "SQLITE_FULL on an own room echo keeps its only plaintext for the retry.
Native outgoing encryption omits the sending device, so decryption cannot
recover it.  The retry stores the text and advances the cursor; a later
live echo still shows the text."
  (jabber-test-omemo-admission--with-mam
    (let* ((room "room@example.com")
           (jabber-muc--rooms (make-hash-table :test #'equal))
           (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
           (plaintext (make-string (* 16 1024) ?x))
           (budget (caar (sqlite-select jabber-db--connection "PRAGMA max_page_count")))
           (wire (jabber-test-omemo-admission--echo-wire jc plaintext)))
      (ignore sender)
      (cl-letf (((symbol-function 'jabber-omemo--resolve-sender-jid)
                 (lambda (&rest _) "me@example.com")))
        (dolist (attempt '(full retry))
          (sqlite-execute jabber-db--connection
                          (format "PRAGMA max_page_count=%d"
                                  (if (eq attempt 'full)
                                      (+ 2 (caar (sqlite-select jabber-db--connection
                                                                "PRAGMA page_count")))
                                    budget)))
          (should (eq (if (eq attempt 'full) 'failed 'success)
                      (jabber-test-omemo-admission--archive jc room wire)))
          (should (equal (and (eq attempt 'retry) '((16384 0)))
                         (sqlite-select jabber-db--connection
                                        "SELECT length(body), body LIKE '[OMEMO:%' FROM message")))
          (should (equal (and (eq attempt 'retry) '("uid" nil))
                         (jabber-test-archive--progress nil room))))
        (should (zerop (hash-table-count jabber-omemo--sent-muc-plaintexts)))
        (should (equal plaintext
                       (caddr (assq 'body (jabber-chat--decrypt-if-needed
                                           jc (copy-tree wire))))))))))

(provide 'jabber-test-omemo-admission)
;;; jabber-test-omemo-admission.el ends here
