;;; jabber-test-omemo-protocol.el --- Tests for jabber-omemo-protocol  -*- lexical-binding: t; -*-

;;; Commentary:

;; OMEMO protocol session management.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-omemo)

;;; Test infrastructure

(defmacro jabber-test-omemo-protocol-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Clears OMEMO in-memory caches and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-omemo-protocol--dir
           (make-temp-file "jabber-omemo-proto-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-test-omemo-protocol--dir))
          (jabber-db--connection nil)
          (jabber-omemo--device-ids (make-hash-table :test 'equal))
          (jabber-omemo--stores (make-hash-table :test 'equal))
          (jabber-omemo--device-lists (make-hash-table :test 'equal))
          (jabber-omemo--sessions (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-test-omemo-protocol--dir)
         (delete-directory jabber-test-omemo-protocol--dir t)))))

;;; Group 1: Device list XML

(ert-deftest jabber-test-omemo-protocol-parse-device-list ()
  "parse-device-list extracts device IDs from XML items."
  (let ((items '((item ((id . "current"))
                       (list ((xmlns . "eu.siacs.conversations.axolotl"))
                             (device ((id . "123")))
                             (device ((id . "456")))
                             (device ((id . "789"))))))))
    (should (equal '(123 456 789)
                   (jabber-omemo--parse-device-list items)))))

(ert-deftest jabber-test-omemo-protocol-parse-device-list-empty ()
  "parse-device-list handles empty device list."
  (let ((items '((item ((id . "current"))
                       (list ((xmlns . "eu.siacs.conversations.axolotl")))))))
    (should (equal '() (jabber-omemo--parse-device-list items)))))

(ert-deftest jabber-test-omemo-protocol-parse-device-list-no-items ()
  "parse-device-list returns nil for no items."
  (should (equal '() (jabber-omemo--parse-device-list nil))))

(ert-deftest jabber-test-omemo-protocol-build-device-list-xml ()
  "build-device-list-xml produces correct sexp."
  (let ((xml (jabber-omemo--build-device-list-xml '(100 200))))
    (should (eq 'list (car xml)))
    (should (string= "eu.siacs.conversations.axolotl"
                     (cdr (assq 'xmlns (cadr xml)))))
    (let ((devices (cddr xml)))
      (should (= 2 (length devices)))
      (should (string= "100" (cdr (assq 'id (cadr (nth 0 devices))))))
      (should (string= "200" (cdr (assq 'id (cadr (nth 1 devices)))))))))

(ert-deftest jabber-test-omemo-protocol-build-parse-device-list-roundtrip ()
  "Building then parsing a device list round-trips the IDs."
  (let* ((ids '(111 222 333))
         (xml (jabber-omemo--build-device-list-xml ids))
         (wrapped `((item ((id . "current")) ,xml)))
         (parsed (jabber-omemo--parse-device-list wrapped)))
    (should (equal ids parsed))))

;;; Group 2: Bundle XML

(ert-deftest jabber-test-omemo-protocol-build-bundle-xml ()
  "build-bundle-xml produces valid sexp with base64 keys."
  (jabber-test-omemo-protocol-with-db
    (let* ((blob (jabber-omemo-setup-store))
           (store-ptr (jabber-omemo-deserialize-store blob))
           (xml (jabber-omemo--build-bundle-xml store-ptr)))
      (should (eq 'bundle (car xml)))
      (should (string= "eu.siacs.conversations.axolotl"
                       (cdr (assq 'xmlns (cadr xml)))))
      (let ((spk (car (jabber-xml-get-children xml 'signedPreKeyPublic)))
            (sig (car (jabber-xml-get-children xml 'signedPreKeySignature)))
            (ik (car (jabber-xml-get-children xml 'identityKey)))
            (pks (car (jabber-xml-get-children xml 'prekeys))))
        (should spk)
        (should sig)
        (should ik)
        (should pks)
        (should (jabber-xml-get-attribute spk 'signedPreKeyId))
        (should (> (length (jabber-xml-get-children pks 'preKeyPublic)) 0))))))

(ert-deftest jabber-test-omemo-protocol-parse-bundle-xml ()
  "parse-bundle-xml returns correct plist keys."
  (jabber-test-omemo-protocol-with-db
    (let* ((blob (jabber-omemo-setup-store))
           (store-ptr (jabber-omemo-deserialize-store blob))
           (xml (jabber-omemo--build-bundle-xml store-ptr))
           (parsed (jabber-omemo--parse-bundle-xml xml)))
      (should (plist-get parsed :signature))
      (should (plist-get parsed :signed-pre-key))
      (should (plist-get parsed :identity-key))
      (should (integerp (plist-get parsed :signed-pre-key-id)))
      (should (listp (plist-get parsed :pre-keys))))))

(ert-deftest jabber-test-omemo-protocol-bundle-xml-roundtrip ()
  "parse-bundle-xml round-trips with build-bundle-xml."
  (jabber-test-omemo-protocol-with-db
    (let* ((blob (jabber-omemo-setup-store))
           (store-ptr (jabber-omemo-deserialize-store blob))
           (bundle (jabber-omemo-get-bundle store-ptr))
           (xml (jabber-omemo--build-bundle-xml store-ptr))
           (parsed (jabber-omemo--parse-bundle-xml xml)))
      (should (string= (plist-get bundle :identity-key)
                       (plist-get parsed :identity-key)))
      (should (string= (plist-get bundle :signed-pre-key)
                       (plist-get parsed :signed-pre-key)))
      (should (string= (plist-get bundle :signature)
                       (plist-get parsed :signature)))
      (should (= (plist-get bundle :signed-pre-key-id)
                 (plist-get parsed :signed-pre-key-id)))
      (should (= (length (plist-get bundle :pre-keys))
                 (length (plist-get parsed :pre-keys)))))))

(ert-deftest jabber-test-omemo-protocol-parsed-bundle-key-lengths ()
  "Parsed bundle keys have correct byte lengths."
  (jabber-test-omemo-protocol-with-db
    (let* ((blob (jabber-omemo-setup-store))
           (store-ptr (jabber-omemo-deserialize-store blob))
           (xml (jabber-omemo--build-bundle-xml store-ptr))
           (parsed (jabber-omemo--parse-bundle-xml xml)))
      (should (= 33 (length (plist-get parsed :identity-key))))
      (should (= 33 (length (plist-get parsed :signed-pre-key))))
      (should (= 64 (length (plist-get parsed :signature))))
      (dolist (pk (plist-get parsed :pre-keys))
        (should (= 33 (length (cdr pk))))))))

;;; Group 3: Device ID persistence

(ert-deftest jabber-test-omemo-protocol-device-id-roundtrip ()
  "save and load device ID round-trips."
  (jabber-test-omemo-protocol-with-db
    (jabber-omemo-store-save-device-id "me@example.com" 42)
    (should (= 42 (jabber-omemo-store-load-device-id "me@example.com")))))

(ert-deftest jabber-test-omemo-protocol-device-id-unknown ()
  "load returns nil for unknown account."
  (jabber-test-omemo-protocol-with-db
    (should (null (jabber-omemo-store-load-device-id "nobody@example.com")))))

(ert-deftest jabber-test-omemo-protocol-device-id-upsert ()
  "save overwrites existing device ID."
  (jabber-test-omemo-protocol-with-db
    (jabber-omemo-store-save-device-id "me@example.com" 1)
    (jabber-omemo-store-save-device-id "me@example.com" 2)
    (should (= 2 (jabber-omemo-store-load-device-id "me@example.com")))))

;;; Group 4: Store cache

(ert-deftest jabber-test-omemo-protocol-get-store-creates-new ()
  "get-store creates new store on first call."
  (jabber-test-omemo-protocol-with-db
    (let ((jc (list :bare-jid "me@example.com")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (let ((ptr (jabber-omemo--get-store jc)))
          (should (user-ptrp ptr)))))))

(ert-deftest jabber-test-omemo-protocol-get-store-cached ()
  "get-store returns cached ptr on second call."
  (jabber-test-omemo-protocol-with-db
    (let ((jc (list :bare-jid "me@example.com")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (let ((ptr1 (jabber-omemo--get-store jc))
              (ptr2 (jabber-omemo--get-store jc)))
          (should (eq ptr1 ptr2)))))))

(ert-deftest jabber-test-omemo-protocol-get-store-from-db ()
  "get-store loads from DB on cold start."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((jc (list :bare-jid "me@example.com")))
        (let ((ptr1 (jabber-omemo--get-store jc)))
          (should (user-ptrp ptr1))
          ;; Clear cache to simulate cold start
          (clrhash jabber-omemo--stores)
          (let ((ptr2 (jabber-omemo--get-store jc)))
            (should (user-ptrp ptr2))
            ;; Different ptr but loaded from same DB blob
            (should-not (eq ptr1 ptr2))))))))

;;; Group 5: Session establishment (integration)

(ert-deftest jabber-test-omemo-protocol-establish-session ()
  "establish-session creates and persists a session."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (_store-ptr (jabber-omemo--get-store jc))
             (bundle-blob (jabber-omemo-setup-store))
             (remote-store (jabber-omemo-deserialize-store bundle-blob))
             (bundle-xml (jabber-omemo--build-bundle-xml remote-store))
             (parsed (jabber-omemo--parse-bundle-xml bundle-xml))
             (session (jabber-omemo--establish-session
                       jc "them@example.com" 999 parsed)))
        (should (user-ptrp session))
        ;; Session should be in cache
        (should (eq session (jabber-omemo--get-session
                             jc "them@example.com" 999)))))))

(ert-deftest jabber-test-omemo-protocol-establish-session-trust ()
  "establish-session stores trust record."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (_store-ptr (jabber-omemo--get-store jc))
             (bundle-blob (jabber-omemo-setup-store))
             (remote-store (jabber-omemo-deserialize-store bundle-blob))
             (bundle-xml (jabber-omemo--build-bundle-xml remote-store))
             (parsed (jabber-omemo--parse-bundle-xml bundle-xml)))
        (jabber-omemo--establish-session jc "them@example.com" 999 parsed)
        (let ((trust (jabber-omemo-store-load-trust
                      "me@example.com" "them@example.com" 999)))
          (should trust)
          (should (= 0 (plist-get trust :trust))))))))

(ert-deftest jabber-test-omemo-protocol-get-session-unknown ()
  "get-session returns nil for unknown device."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((jc (list :bare-jid "me@example.com")))
        (should (null (jabber-omemo--get-session
                       jc "them@example.com" 999)))))))

(ert-deftest jabber-test-omemo-protocol-get-session-from-db ()
  "get-session loads from DB when not cached."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (_store-ptr (jabber-omemo--get-store jc))
             (bundle-blob (jabber-omemo-setup-store))
             (remote-store (jabber-omemo-deserialize-store bundle-blob))
             (bundle-xml (jabber-omemo--build-bundle-xml remote-store))
             (parsed (jabber-omemo--parse-bundle-xml bundle-xml)))
        (jabber-omemo--establish-session jc "them@example.com" 999 parsed)
        ;; Clear session cache
        (clrhash jabber-omemo--sessions)
        (let ((loaded (jabber-omemo--get-session
                       jc "them@example.com" 999)))
          (should (user-ptrp loaded)))))))

;;; Group 6: Bundle publish-if-needed

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-nil-published ()
  "Republish required when no bundle has been published."
  (let ((local '(:identity-key "ik" :signed-pre-key "spk"
                 :signed-pre-key-id 1 :pre-keys (1 2 3))))
    (should (jabber-omemo--bundle-needs-republish-p local nil))))

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-identity-key-mismatch ()
  "Republish required when identity key differs."
  (let* ((pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (local `(:identity-key "ik-new" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik-old" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-spk-id-mismatch ()
  "Republish required when signed-pre-key-id differs."
  (let* ((pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 2 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-spk-data-mismatch ()
  "Republish required when signed-pre-key data differs."
  (let* ((pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk-new"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk-old"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-prekey-count-low ()
  "Republish required when published pre-key count is below threshold."
  (let* ((local-pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (published-pks (cl-loop for i from 1 to 5 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,local-pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,published-pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-test-omemo-protocol-bundle-needs-republish-up-to-date ()
  "No republish when published bundle matches local and has enough pre-keys."
  (let* ((pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should-not (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-test-omemo-protocol-publish-bundle-if-needed-skips-when-current ()
  "publish-bundle-if-needed does NOT publish when fetched bundle matches local."
  (let ((jabber-omemo--bundle-publishes-in-flight (make-hash-table :test 'equal))
        (publish-called nil)
        (persist-called nil)
        (refill-called nil)
        (pks (cl-loop for i from 1 to 100 collect (cons i "k"))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--get-device-id)
               (lambda (_jc) 42))
              ((symbol-function 'jabber-omemo--get-store)
               (lambda (_jc) 'fake-store-ptr))
              ((symbol-function 'jabber-omemo-get-bundle)
               (lambda (_store)
                 (list :identity-key "ik" :signed-pre-key "spk"
                       :signed-pre-key-id 1 :pre-keys pks)))
              ((symbol-function 'jabber-omemo--fetch-bundle)
               (lambda (_jc _jid _did callback)
                 (funcall callback
                          (list :identity-key "ik" :signed-pre-key "spk"
                                :signed-pre-key-id 1 :pre-keys pks))))
              ((symbol-function 'jabber-omemo-refill-pre-keys)
               (lambda (_store) (setq refill-called t)))
              ((symbol-function 'jabber-omemo--persist-store)
               (lambda (_jc) (setq persist-called t)))
              ((symbol-function 'jabber-omemo--publish-bundle)
               (lambda (_jc) (setq publish-called t))))
      (jabber-omemo--publish-bundle-if-needed 'fake-jc)
      (should-not publish-called)
      (should-not persist-called)
      (should-not refill-called)
      ;; In-flight key cleared after callback runs
      (should (zerop (hash-table-count
                      jabber-omemo--bundle-publishes-in-flight))))))

(ert-deftest jabber-test-omemo-protocol-publish-bundle-if-needed-publishes-when-stale ()
  "publish-bundle-if-needed refills, persists, and publishes when stale."
  (let ((jabber-omemo--bundle-publishes-in-flight (make-hash-table :test 'equal))
        (calls nil)
        (pks (cl-loop for i from 1 to 100 collect (cons i "k"))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--get-device-id)
               (lambda (_jc) 42))
              ((symbol-function 'jabber-omemo--get-store)
               (lambda (_jc) 'fake-store-ptr))
              ((symbol-function 'jabber-omemo-get-bundle)
               (lambda (_store)
                 (list :identity-key "ik-new" :signed-pre-key "spk"
                       :signed-pre-key-id 1 :pre-keys pks)))
              ((symbol-function 'jabber-omemo--fetch-bundle)
               (lambda (_jc _jid _did callback)
                 (funcall callback
                          (list :identity-key "ik-old" :signed-pre-key "spk"
                                :signed-pre-key-id 1 :pre-keys pks))))
              ((symbol-function 'jabber-omemo-refill-pre-keys)
               (lambda (store) (push (cons 'refill store) calls)))
              ((symbol-function 'jabber-omemo--persist-store)
               (lambda (jc) (push (cons 'persist jc) calls)))
              ((symbol-function 'jabber-omemo--publish-bundle)
               (lambda (jc) (push (cons 'publish jc) calls))))
      (jabber-omemo--publish-bundle-if-needed 'fake-jc)
      (should (equal (nreverse calls)
                     '((refill . fake-store-ptr)
                       (persist . fake-jc)
                       (publish . fake-jc))))
      ;; In-flight key cleared after callback runs
      (should (zerop (hash-table-count
                      jabber-omemo--bundle-publishes-in-flight))))))

(ert-deftest jabber-test-omemo-protocol-publish-bundle-if-needed-dedup ()
  "Second concurrent publish-bundle-if-needed call is a no-op while first is in flight."
  (let ((jabber-omemo--bundle-publishes-in-flight (make-hash-table :test 'equal))
        (fetch-count 0)
        ;; Hold the first fetch's callback so we can fire the second
        ;; call while the first is still in flight.
        (held-callback nil))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--get-device-id)
               (lambda (_jc) 42))
              ((symbol-function 'jabber-omemo--get-store)
               (lambda (_jc) 'fake-store-ptr))
              ((symbol-function 'jabber-omemo-get-bundle)
               (lambda (_store) nil))
              ((symbol-function 'jabber-omemo--fetch-bundle)
               (lambda (_jc _jid _did callback)
                 (cl-incf fetch-count)
                 (setq held-callback callback)))
              ((symbol-function 'jabber-omemo-refill-pre-keys) #'ignore)
              ((symbol-function 'jabber-omemo--persist-store) #'ignore)
              ((symbol-function 'jabber-omemo--publish-bundle) #'ignore))
      ;; First call: fires fetch, callback held.
      (jabber-omemo--publish-bundle-if-needed 'fake-jc)
      (should (= 1 fetch-count))
      (should (= 1 (hash-table-count
                    jabber-omemo--bundle-publishes-in-flight)))
      ;; Second call while first is in flight: should NOT fire fetch.
      (jabber-omemo--publish-bundle-if-needed 'fake-jc)
      (should (= 1 fetch-count))
      ;; Now release the held callback; in-flight slot frees.
      (funcall held-callback nil)
      (should (zerop (hash-table-count
                      jabber-omemo--bundle-publishes-in-flight)))
      ;; Third call after release: fetch fires again.
      (jabber-omemo--publish-bundle-if-needed 'fake-jc)
      (should (= 2 fetch-count)))))

;;; Group 7: Pre-key session selection

(defun jabber-test-omemo-protocol--initiate-toward (jc)
  "Create a peer store with a session initiated toward JC's store.
Returns (PEER-SESSION . PK-ID) where PEER-SESSION is the sending
side session and PK-ID the pre-key id it consumed."
  (let* ((our-bundle (jabber-omemo-get-bundle (jabber-omemo--get-store jc)))
         (peer-store (jabber-omemo-deserialize-store
                      (jabber-omemo-setup-store)))
         (pk (car (plist-get our-bundle :pre-keys))))
    (cons (jabber-omemo-initiate-session
           peer-store
           (plist-get our-bundle :signature)
           (plist-get our-bundle :signed-pre-key)
           (plist-get our-bundle :identity-key)
           (cdr pk)
           (plist-get our-bundle :signed-pre-key-id)
           (car pk))
          (car pk))))

(ert-deftest jabber-test-omemo-protocol-prekey-reuses-established-session ()
  "A second pre-key message decrypts via the saved session, not a fresh one."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc))
             (alice (car (jabber-test-omemo-protocol--initiate-toward jc)))
             (key-1 (make-string 32 ?A))
             (key-2 (make-string 32 ?B))
             (msg-1 (jabber-omemo-encrypt-key alice key-1))
             (msg-2 (jabber-omemo-encrypt-key alice key-2)))
        (should (plist-get msg-1 :pre-key-p))
        (should (plist-get msg-2 :pre-key-p))
        (pcase-let ((`(,session ,decrypted ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-1 :data))))
          (should (string= key-1 decrypted))
          (should fresh-p)
          (jabber-omemo--save-session jc "alice@example.com" 7 session))
        (pcase-let ((`(,_session ,decrypted ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-2 :data))))
          (should (string= key-2 decrypted))
          (should-not fresh-p))))))

(ert-deftest jabber-test-omemo-protocol-prekey-out-of-order-falls-back ()
  "An earlier pre-key message still decrypts after a later one.
The established session serves the older ratchet position from its
skipped message keys, so no fresh-session fallback is needed."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc))
             (alice (car (jabber-test-omemo-protocol--initiate-toward jc)))
             (key-1 (make-string 32 ?A))
             (key-2 (make-string 32 ?B))
             (msg-1 (jabber-omemo-encrypt-key alice key-1))
             (msg-2 (jabber-omemo-encrypt-key alice key-2)))
        (pcase-let ((`(,session ,decrypted ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-2 :data))))
          (should (string= key-2 decrypted))
          (should fresh-p)
          (jabber-omemo--save-session jc "alice@example.com" 7 session))
        (pcase-let ((`(,_session ,decrypted ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-1 :data))))
          (should (string= key-1 decrypted))
          (should-not fresh-p))))))

(ert-deftest jabber-test-omemo-protocol-prekey-falls-back-on-peer-reset ()
  "A pre-key message from a re-initialized peer session decrypts fresh."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc))
             (alice-1 (car (jabber-test-omemo-protocol--initiate-toward jc)))
             (msg-1 (jabber-omemo-encrypt-key alice-1 (make-string 32 ?A))))
        (pcase-let ((`(,session ,_decrypted ,_fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-1 :data))))
          (jabber-omemo--save-session jc "alice@example.com" 7 session))
        ;; Alice lost her state and initiates a brand new session.
        (let* ((alice-2 (car (jabber-test-omemo-protocol--initiate-toward jc)))
               (key-2 (make-string 32 ?B))
               (msg-2 (jabber-omemo-encrypt-key alice-2 key-2)))
          (pcase-let ((`(,_session ,decrypted ,fresh-p)
                       (jabber-omemo--decrypt-key-with-session
                        jc "alice@example.com" 7 store-ptr t
                        (plist-get msg-2 :data))))
            (should (string= key-2 decrypted))
            (should fresh-p)))))))

(ert-deftest jabber-test-omemo-protocol-prekey-failure-triggers-recovery ()
  "A pre-key decrypt failure drops the stale session and re-ensures."
  (jabber-test-omemo-protocol-with-db
    (let (deleted ensured)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-omemo--decrypt-stanza)
                 (lambda (_jc _xml _parsed)
                   (signal 'jabber-omemo-prekey-failed
                           (list "alice@example.com" 7 "stale pre-key"))))
                ((symbol-function 'jabber-omemo-store-delete-session)
                 (lambda (account jid did)
                   (setq deleted (list account jid did))))
                ((symbol-function 'jabber-omemo--ensure-sessions)
                 (lambda (_jc jid _callback) (setq ensured jid))))
        (puthash (jabber-omemo--session-key
                  "me@example.com" "alice@example.com" 7)
                 'stale-session jabber-omemo--sessions)
        (should-error (jabber-omemo--decrypt-handler
                       'fake-jc
                       '(message ((from . "alice@example.com/phone")))
                       '(:type omemo :parsed (:payload "ciphertext")))
                      :type 'jabber-omemo-prekey-failed)
        (should (equal '("me@example.com" "alice@example.com" 7) deleted))
        (should (equal "alice@example.com" ensured))
        (should-not (gethash (jabber-omemo--session-key
                              "me@example.com" "alice@example.com" 7)
                             jabber-omemo--sessions))))))

(ert-deftest jabber-test-omemo-protocol-regular-message-requires-session ()
  "A non-pre-key message without an established session signals no-session."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc)))
        (should-error (jabber-omemo--decrypt-key-with-session
                       jc "alice@example.com" 7 store-ptr nil "junk")
                      :type 'jabber-omemo-no-session)))))

;;; Group 8: One-time pre-key removal

(ert-deftest jabber-test-omemo-protocol-prekey-removal-deferred-until-flush ()
  "Both pre-key messages decrypt before the consumed key is removed."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--schedule-prekey-flush)
               #'ignore)
              ((symbol-function 'jabber-omemo--publish-bundle) #'ignore)
              ((symbol-function 'jabber-omemo--mam-syncing-p)
               (lambda () nil)))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc))
             (pair (jabber-test-omemo-protocol--initiate-toward jc))
             (alice (car pair))
             (victim (cdr pair))
             (key-2 (make-string 32 ?B))
             (msg-1 (jabber-omemo-encrypt-key alice (make-string 32 ?A)))
             (msg-2 (jabber-omemo-encrypt-key alice key-2))
             (jabber-omemo--pending-prekey-removals
              (make-hash-table :test #'equal)))
        (pcase-let ((`(,session ,_key ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-1 :data))))
          (should fresh-p)
          (jabber-omemo--save-session jc "alice@example.com" 7 session)
          (jabber-omemo--note-consumed-prekey jc session))
        (should (equal (list victim)
                       (gethash "me@example.com"
                                jabber-omemo--pending-prekey-removals)))
        ;; Removal is deferred, so the second pre-key message on the
        ;; same pre-key still decrypts (via the established session).
        (pcase-let ((`(,_session ,decrypted ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 7 store-ptr t
                      (plist-get msg-2 :data))))
          (should (string= key-2 decrypted))
          (should-not fresh-p))
        (jabber-omemo--flush-prekey-removals jc)
        (let ((after (plist-get (jabber-omemo-get-bundle store-ptr)
                                :pre-keys)))
          (should-not (assq victim after))
          (should (gethash "me@example.com" jabber-omemo--stores)))
        (should-not (gethash "me@example.com"
                             jabber-omemo--pending-prekey-removals))))))

(ert-deftest jabber-test-omemo-protocol-flush-noop-while-syncing ()
  "Pending removals survive a flush attempted during MAM catchup."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--mam-syncing-p)
               (lambda () t))
              ((symbol-function 'jabber-omemo--publish-bundle) #'ignore))
      (let ((jc (list :bare-jid "me@example.com"))
            (jabber-omemo--pending-prekey-removals
             (make-hash-table :test #'equal)))
        (puthash "me@example.com" '(42)
                 jabber-omemo--pending-prekey-removals)
        (jabber-omemo--flush-prekey-removals jc)
        (should (equal '(42)
                       (gethash "me@example.com"
                                jabber-omemo--pending-prekey-removals)))))))

(ert-deftest jabber-test-omemo-protocol-flush-republishes-bundle ()
  "A flush republishes the bundle unconditionally."
  (jabber-test-omemo-protocol-with-db
    (let (published)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-omemo--mam-syncing-p)
                 (lambda () nil))
                ((symbol-function 'jabber-omemo--publish-bundle)
                 (lambda (_jc) (setq published t))))
        (let* ((jc (list :bare-jid "me@example.com"))
               (store-ptr (jabber-omemo--get-store jc))
               (victim (car (car (plist-get
                                  (jabber-omemo-get-bundle store-ptr)
                                  :pre-keys))))
               (jabber-omemo--pending-prekey-removals
                (make-hash-table :test #'equal)))
          (puthash "me@example.com" (list victim)
                   jabber-omemo--pending-prekey-removals)
          (jabber-omemo--flush-prekey-removals jc)
          (should published))))))

(ert-deftest jabber-test-omemo-protocol-mam-sync-complete-flushes ()
  "The MAM sync-complete hook flushes all connections when idle."
  (let ((flushed nil)
        (jabber-connections '(jc-a jc-b)))
    (cl-letf (((symbol-function 'jabber-omemo--flush-prekey-removals)
               (lambda (jc) (push jc flushed))))
      (cl-letf (((symbol-function 'jabber-omemo--mam-syncing-p)
                 (lambda () t)))
        (jabber-omemo--on-mam-sync-complete nil)
        (should-not flushed))
      (cl-letf (((symbol-function 'jabber-omemo--mam-syncing-p)
                 (lambda () nil)))
        (jabber-omemo--on-mam-sync-complete nil)
        (should (equal '(jc-b jc-a) flushed))))))

;;; Group: Skipped message keys

(ert-deftest jabber-test-omemo-protocol-legacy-session-migrates-atomically ()
  "A raw session imports legacy keys and saves one self-contained blob."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc 'jc)
             (session (jabber-omemo-make-session))
             (envelope (jabber-omemo-serialize-session session))
             (raw-size (+ (ash (aref envelope 12) 24)
                          (ash (aref envelope 13) 16)
                          (ash (aref envelope 14) 8)
                          (aref envelope 15)))
             (raw (substring envelope 20 (+ 20 raw-size)))
             (dh (make-string 32 ?d))
             (mk (make-string 32 ?m)))
        (jabber-omemo-store-save-session
         "me@example.com" "alice@example.com" 111 raw)
        (sqlite-execute jabber-db--connection "\
INSERT INTO omemo_skipped_keys
  (account, jid, device_id, dh_key, message_number, message_key, created_at)
  VALUES (?, ?, ?, ?, ?, ?, ?)"
                        (list "me@example.com" "alice@example.com"
                              111 dh 7 mk 0))
        (let ((loaded (jabber-omemo--get-session
                       jc "alice@example.com" 111)))
          (should (equal (list (list 7 dh mk))
                         (jabber-omemo--session-skipped-keys loaded)))
          (jabber-omemo--save-session jc "alice@example.com" 111 loaded))
        (let ((blob (jabber-omemo-store-load-session
                     "me@example.com" "alice@example.com" 111)))
          (should-not (jabber-omemo--legacy-session-blob-p blob))
          (should (= 1 (length
                        (jabber-omemo--session-skipped-keys
                         (jabber-omemo-deserialize-session blob))))))
        (should-not (jabber-omemo-store-all-skipped-keys
                     "me@example.com" "alice@example.com" 111))))))

(ert-deftest jabber-test-omemo-protocol-skipped-keys-recover-across-restart ()
  "An out-of-order message decrypts after a session cache flush.
Skipped ratchet keys persist inside the session blob."
  (jabber-test-omemo-protocol-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let* ((jc (list :bare-jid "me@example.com"))
             (store-ptr (jabber-omemo--get-store jc))
             (my-bundle (jabber-omemo-get-bundle store-ptr))
             (alice (jabber-omemo-deserialize-store (jabber-omemo-setup-store)))
             (pk (car (plist-get my-bundle :pre-keys)))
             (alice-session (jabber-omemo-initiate-session
                             alice
                             (plist-get my-bundle :signature)
                             (plist-get my-bundle :signed-pre-key)
                             (plist-get my-bundle :identity-key)
                             (cdr pk)
                             (plist-get my-bundle :signed-pre-key-id)
                             (car pk)))
             (k1 (make-string 32 ?1))
             (k2 (make-string 32 ?2))
             (m1 (jabber-omemo-encrypt-key alice-session k1))
             (m2 (jabber-omemo-encrypt-key alice-session k2)))
        ;; Deliver message 2 first; its decrypt skips message 1's key.
        (pcase-let ((`(,session ,key ,fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 111 store-ptr
                      (plist-get m2 :pre-key-p) (plist-get m2 :data))))
          (should (string= k2 key))
          (should fresh-p)
          (jabber-omemo--save-session jc "alice@example.com" 111 session))
        (should-not (jabber-omemo-store-all-skipped-keys
                     "me@example.com" "alice@example.com" 111))
        ;; Simulate a restart: drop all in-memory session state.
        (clrhash jabber-omemo--sessions)
        ;; The late message decrypts from the reloaded skipped key.
        (pcase-let ((`(,_session ,key ,_fresh-p)
                     (jabber-omemo--decrypt-key-with-session
                      jc "alice@example.com" 111 store-ptr
                      (plist-get m1 :pre-key-p) (plist-get m1 :data))))
          (should (string= k1 key)))
        ;; No separate skipped-key rows are written or consumed.
        (should (null (jabber-omemo-store-all-skipped-keys
                       "me@example.com" "alice@example.com" 111)))))))

(provide 'jabber-test-omemo-protocol)
;;; jabber-test-omemo-protocol.el ends here
