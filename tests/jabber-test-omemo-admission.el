;;; jabber-test-omemo-admission.el --- Native inbound admission -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real ratchets and SQLite, intercepting only external effects.

;;; Code:
(require 'ert)
(require 'jabber-chat)
(require 'jabber-omemo)

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

(provide 'jabber-test-omemo-admission)
;;; jabber-test-omemo-admission.el ends here
