;;; jabber-omemo-protocol-tests.el --- ERT tests for OMEMO protocol logic  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-chat)
(require 'jabber-omemo)

;;; Test infrastructure

(defmacro jabber-omemo-protocol-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Clears OMEMO in-memory caches and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-omemo-protocol-test--dir
           (make-temp-file "jabber-omemo-proto-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-omemo-protocol-test--dir))
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
       (when (file-directory-p jabber-omemo-protocol-test--dir)
         (delete-directory jabber-omemo-protocol-test--dir t)))))

;;; Group 1: Device list XML

(ert-deftest jabber-omemo-protocol-test-parse-device-list ()
  "parse-device-list extracts device IDs from XML items."
  (let ((items '((item ((id . "current"))
                       (list ((xmlns . "eu.siacs.conversations.axolotl"))
                             (device ((id . "123")))
                             (device ((id . "456")))
                             (device ((id . "789"))))))))
    (should (equal '(123 456 789)
                   (jabber-omemo--parse-device-list items)))))

(ert-deftest jabber-omemo-protocol-test-parse-device-list-empty ()
  "parse-device-list handles empty device list."
  (let ((items '((item ((id . "current"))
                       (list ((xmlns . "eu.siacs.conversations.axolotl")))))))
    (should (equal '() (jabber-omemo--parse-device-list items)))))

(ert-deftest jabber-omemo-protocol-test-parse-device-list-no-items ()
  "parse-device-list returns nil for no items."
  (should (equal '() (jabber-omemo--parse-device-list nil))))

(ert-deftest jabber-omemo-protocol-test-build-device-list-xml ()
  "build-device-list-xml produces correct sexp."
  (let ((xml (jabber-omemo--build-device-list-xml '(100 200))))
    (should (eq 'list (car xml)))
    (should (string= "eu.siacs.conversations.axolotl"
                     (cdr (assq 'xmlns (cadr xml)))))
    (let ((devices (cddr xml)))
      (should (= 2 (length devices)))
      (should (string= "100" (cdr (assq 'id (cadr (nth 0 devices))))))
      (should (string= "200" (cdr (assq 'id (cadr (nth 1 devices)))))))))

(ert-deftest jabber-omemo-protocol-test-build-parse-device-list-roundtrip ()
  "Building then parsing a device list round-trips the IDs."
  (let* ((ids '(111 222 333))
         (xml (jabber-omemo--build-device-list-xml ids))
         (wrapped `((item ((id . "current")) ,xml)))
         (parsed (jabber-omemo--parse-device-list wrapped)))
    (should (equal ids parsed))))

;;; Group 2: Bundle XML

(ert-deftest jabber-omemo-protocol-test-build-bundle-xml ()
  "build-bundle-xml produces valid sexp with base64 keys."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-parse-bundle-xml ()
  "parse-bundle-xml returns correct plist keys."
  (jabber-omemo-protocol-test-with-db
    (let* ((blob (jabber-omemo-setup-store))
           (store-ptr (jabber-omemo-deserialize-store blob))
           (xml (jabber-omemo--build-bundle-xml store-ptr))
           (parsed (jabber-omemo--parse-bundle-xml xml)))
      (should (plist-get parsed :signature))
      (should (plist-get parsed :signed-pre-key))
      (should (plist-get parsed :identity-key))
      (should (integerp (plist-get parsed :signed-pre-key-id)))
      (should (listp (plist-get parsed :pre-keys))))))

(ert-deftest jabber-omemo-protocol-test-bundle-xml-roundtrip ()
  "parse-bundle-xml round-trips with build-bundle-xml."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-parsed-bundle-key-lengths ()
  "Parsed bundle keys have correct byte lengths."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-device-id-roundtrip ()
  "save and load device ID round-trips."
  (jabber-omemo-protocol-test-with-db
    (jabber-omemo-store-save-device-id "me@example.com" 42)
    (should (= 42 (jabber-omemo-store-load-device-id "me@example.com")))))

(ert-deftest jabber-omemo-protocol-test-device-id-unknown ()
  "load returns nil for unknown account."
  (jabber-omemo-protocol-test-with-db
    (should (null (jabber-omemo-store-load-device-id "nobody@example.com")))))

(ert-deftest jabber-omemo-protocol-test-device-id-upsert ()
  "save overwrites existing device ID."
  (jabber-omemo-protocol-test-with-db
    (jabber-omemo-store-save-device-id "me@example.com" 1)
    (jabber-omemo-store-save-device-id "me@example.com" 2)
    (should (= 2 (jabber-omemo-store-load-device-id "me@example.com")))))

;;; Group 4: Store cache

(ert-deftest jabber-omemo-protocol-test-get-store-creates-new ()
  "get-store creates new store on first call."
  (jabber-omemo-protocol-test-with-db
    (let ((jc (list :bare-jid "me@example.com")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (let ((ptr (jabber-omemo--get-store jc)))
          (should (user-ptrp ptr)))))))

(ert-deftest jabber-omemo-protocol-test-get-store-cached ()
  "get-store returns cached ptr on second call."
  (jabber-omemo-protocol-test-with-db
    (let ((jc (list :bare-jid "me@example.com")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (let ((ptr1 (jabber-omemo--get-store jc))
              (ptr2 (jabber-omemo--get-store jc)))
          (should (eq ptr1 ptr2)))))))

(ert-deftest jabber-omemo-protocol-test-get-store-from-db ()
  "get-store loads from DB on cold start."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-establish-session ()
  "establish-session creates and persists a session."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-establish-session-trust ()
  "establish-session stores trust record."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-get-session-unknown ()
  "get-session returns nil for unknown device."
  (jabber-omemo-protocol-test-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((jc (list :bare-jid "me@example.com")))
        (should (null (jabber-omemo--get-session
                       jc "them@example.com" 999)))))))

(ert-deftest jabber-omemo-protocol-test-get-session-from-db ()
  "get-session loads from DB when not cached."
  (jabber-omemo-protocol-test-with-db
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

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-nil-published ()
  "Republish required when no bundle has been published."
  (let ((local '(:identity-key "ik" :signed-pre-key "spk"
                 :signed-pre-key-id 1 :pre-keys (1 2 3))))
    (should (jabber-omemo--bundle-needs-republish-p local nil))))

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-identity-key-mismatch ()
  "Republish required when identity key differs."
  (let* ((pks (cl-loop for i from 1 to 30 collect (cons i "k")))
         (local `(:identity-key "ik-new" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik-old" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-spk-id-mismatch ()
  "Republish required when signed-pre-key-id differs."
  (let* ((pks (cl-loop for i from 1 to 30 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 2 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-spk-data-mismatch ()
  "Republish required when signed-pre-key data differs."
  (let* ((pks (cl-loop for i from 1 to 30 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk-new"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk-old"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-prekey-count-low ()
  "Republish required when published pre-key count is below threshold."
  (let* ((local-pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (published-pks (cl-loop for i from 1 to 5 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,local-pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,published-pks)))
    (should (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-omemo-protocol-test-bundle-needs-republish-up-to-date ()
  "No republish when published bundle matches local and has enough pre-keys."
  (let* ((pks (cl-loop for i from 1 to 100 collect (cons i "k")))
         (local `(:identity-key "ik" :signed-pre-key "spk"
                  :signed-pre-key-id 1 :pre-keys ,pks))
         (published `(:identity-key "ik" :signed-pre-key "spk"
                      :signed-pre-key-id 1 :pre-keys ,pks)))
    (should-not (jabber-omemo--bundle-needs-republish-p local published))))

(ert-deftest jabber-omemo-protocol-test-publish-bundle-if-needed-skips-when-current ()
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

(ert-deftest jabber-omemo-protocol-test-publish-bundle-if-needed-publishes-when-stale ()
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

(ert-deftest jabber-omemo-protocol-test-publish-bundle-if-needed-dedup ()
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

(provide 'jabber-omemo-protocol-tests)
;;; jabber-omemo-protocol-tests.el ends here
