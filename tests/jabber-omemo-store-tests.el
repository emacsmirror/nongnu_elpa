;;; jabber-omemo-store-tests.el --- Tests for jabber-omemo-store  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-omemo-store)

;;; ---- Test infrastructure ----

(defmacro jabber-omemo-store-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database for OMEMO tests.
Binds `jabber-db-path' to a temp file and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-omemo-store-test--dir (make-temp-file "jabber-omemo-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-omemo-store-test--dir))
          (jabber-db--connection nil))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-omemo-store-test--dir)
         (delete-directory jabber-omemo-store-test--dir t)))))

;;; ---- Group 1: Schema creation ----

(ert-deftest jabber-omemo-store-test-schema-creates-tables ()
  "Migration v2 creates all 5 OMEMO tables."
  (jabber-omemo-store-test-with-db
    (let ((tables (mapcar #'car
                          (sqlite-select jabber-db--connection
                            "SELECT name FROM sqlite_master
                             WHERE type='table' AND name LIKE 'omemo_%'"))))
      (should (member "omemo_store" tables))
      (should (member "omemo_sessions" tables))
      (should (member "omemo_trust" tables))
      (should (member "omemo_skipped_keys" tables))
      (should (member "omemo_devices" tables)))))

(ert-deftest jabber-omemo-store-test-schema-idempotent ()
  "Calling schema init twice does not error."
  (jabber-omemo-store-test-with-db
    (jabber-db--init-schema jabber-db--connection)
    (should t)))

;;; ---- Group 2: Store blob CRUD ----

(ert-deftest jabber-omemo-store-test-save-load-roundtrip ()
  "Save + load round-trips a unibyte blob."
  (jabber-omemo-store-test-with-db
    (let ((blob (unibyte-string 7 42 0 1 127 200 255)))
      (jabber-omemo-store-save "me@example.com" blob)
      (should (equal blob (jabber-omemo-store-load "me@example.com"))))))

(ert-deftest jabber-omemo-store-test-load-unknown-account ()
  "Load returns nil for unknown account."
  (jabber-omemo-store-test-with-db
    (should (null (jabber-omemo-store-load "nobody@example.com")))))

(ert-deftest jabber-omemo-store-test-save-upsert ()
  "Save overwrites existing blob (upsert)."
  (jabber-omemo-store-test-with-db
    (let ((blob1 (string-as-unibyte "first"))
          (blob2 (string-as-unibyte "second")))
      (jabber-omemo-store-save "me@example.com" blob1)
      (jabber-omemo-store-save "me@example.com" blob2)
      (should (equal blob2 (jabber-omemo-store-load "me@example.com"))))))

(ert-deftest jabber-omemo-store-test-delete ()
  "Delete removes the store record."
  (jabber-omemo-store-test-with-db
    (jabber-omemo-store-save "me@example.com" (string-as-unibyte "data"))
    (jabber-omemo-store-delete "me@example.com")
    (should (null (jabber-omemo-store-load "me@example.com")))))

;;; ---- Group 3: Trust CRUD ----

(ert-deftest jabber-omemo-store-test-trust-save-load-roundtrip ()
  "save-trust + load-trust round-trips."
  (jabber-omemo-store-test-with-db
    (let ((key (string-as-unibyte "identity-key-bytes")))
      (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                     12345 key 1)
      (let ((rec (jabber-omemo-store-load-trust "me@example.com"
                                                "alice@example.com" 12345)))
        (should rec)
        (should (equal key (plist-get rec :identity-key)))
        (should (= 1 (plist-get rec :trust)))
        (should (integerp (plist-get rec :first-seen)))))))

(ert-deftest jabber-omemo-store-test-trust-load-unknown ()
  "load-trust returns nil for unknown device."
  (jabber-omemo-store-test-with-db
    (should (null (jabber-omemo-store-load-trust "me@example.com"
                                                 "alice@example.com" 99999)))))

(ert-deftest jabber-omemo-store-test-set-trust-updates-level ()
  "set-trust updates level without changing identity-key or first-seen."
  (jabber-omemo-store-test-with-db
    (let ((key (string-as-unibyte "key-data")))
      (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                     100 key 0)
      (let ((orig (jabber-omemo-store-load-trust "me@example.com"
                                                  "alice@example.com" 100)))
        (jabber-omemo-store-set-trust "me@example.com" "alice@example.com"
                                      100 2)
        (let ((updated (jabber-omemo-store-load-trust "me@example.com"
                                                       "alice@example.com" 100)))
          (should (= 2 (plist-get updated :trust)))
          (should (equal key (plist-get updated :identity-key)))
          (should (= (plist-get orig :first-seen)
                     (plist-get updated :first-seen))))))))

(ert-deftest jabber-omemo-store-test-all-trust-multiple ()
  "all-trust returns multiple devices for same JID."
  (jabber-omemo-store-test-with-db
    (let ((key1 (string-as-unibyte "key1"))
          (key2 (string-as-unibyte "key2")))
      (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                     100 key1 1)
      (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                     200 key2 0)
      (let ((all (jabber-omemo-store-all-trust "me@example.com"
                                               "alice@example.com")))
        (should (= 2 (length all)))
        (should (cl-find 100 all :key (lambda (p) (plist-get p :device-id))))
        (should (cl-find 200 all :key (lambda (p) (plist-get p :device-id))))))))

(ert-deftest jabber-omemo-store-test-trust-levels ()
  "Trust levels: 0 (undecided), 1 (tofu), 2 (verified), -1 (untrusted)."
  (jabber-omemo-store-test-with-db
    (let ((key (string-as-unibyte "k")))
      (dolist (level '(0 1 2 -1))
        (jabber-omemo-store-save-trust "me@example.com" "peer@example.com"
                                       (+ 1000 level) key level))
      (dolist (level '(0 1 2 -1))
        (let ((rec (jabber-omemo-store-load-trust "me@example.com"
                                                   "peer@example.com"
                                                   (+ 1000 level))))
          (should (= level (plist-get rec :trust))))))))

(ert-deftest jabber-omemo-store-test-trust-upsert-preserves-first-seen ()
  "Upserting trust preserves the original first_seen."
  (jabber-omemo-store-test-with-db
    (let ((key (string-as-unibyte "key-data")))
      (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                     100 key 0)
      (let ((first (plist-get
                    (jabber-omemo-store-load-trust "me@example.com"
                                                   "alice@example.com" 100)
                    :first-seen)))
        ;; Upsert with different trust level
        (jabber-omemo-store-save-trust "me@example.com" "alice@example.com"
                                       100 key 2)
        (let ((after (plist-get
                      (jabber-omemo-store-load-trust "me@example.com"
                                                     "alice@example.com" 100)
                      :first-seen)))
          (should (= first after)))))))

;;; ---- Group 4: Device list CRUD ----

(ert-deftest jabber-omemo-store-test-device-save-load-roundtrip ()
  "save-device + load-devices round-trips."
  (jabber-omemo-store-test-with-db
    (jabber-omemo-store-save-device "me@example.com" "alice@example.com" 42)
    (let ((devs (jabber-omemo-store-load-devices "me@example.com"
                                                  "alice@example.com")))
      (should (= 1 (length devs)))
      (let ((d (car devs)))
        (should (= 42 (plist-get d :device-id)))
        (should (eq t (plist-get d :active)))
        (should (integerp (plist-get d :last-seen)))))))

(ert-deftest jabber-omemo-store-test-device-load-unknown ()
  "load-devices returns empty list for unknown JID."
  (jabber-omemo-store-test-with-db
    (should (null (jabber-omemo-store-load-devices "me@example.com"
                                                    "nobody@example.com")))))

(ert-deftest jabber-omemo-store-test-device-set-active ()
  "set-device-active toggles the flag."
  (jabber-omemo-store-test-with-db
    (jabber-omemo-store-save-device "me@example.com" "alice@example.com" 42)
    (jabber-omemo-store-set-device-active "me@example.com" "alice@example.com"
                                          42 nil)
    (let ((d (car (jabber-omemo-store-load-devices "me@example.com"
                                                    "alice@example.com"))))
      (should-not (plist-get d :active)))
    (jabber-omemo-store-set-device-active "me@example.com" "alice@example.com"
                                          42 t)
    (let ((d (car (jabber-omemo-store-load-devices "me@example.com"
                                                    "alice@example.com"))))
      (should (eq t (plist-get d :active))))))

(ert-deftest jabber-omemo-store-test-device-delete ()
  "delete-device removes the record."
  (jabber-omemo-store-test-with-db
    (jabber-omemo-store-save-device "me@example.com" "alice@example.com" 42)
    (jabber-omemo-store-delete-device "me@example.com" "alice@example.com" 42)
    (should (null (jabber-omemo-store-load-devices "me@example.com"
                                                    "alice@example.com")))))

(ert-deftest jabber-omemo-store-test-device-last-seen ()
  "last-seen is set on save."
  (jabber-omemo-store-test-with-db
    (jabber-omemo-store-save-device "me@example.com" "alice@example.com" 42)
    (let* ((d (car (jabber-omemo-store-load-devices "me@example.com"
                                                     "alice@example.com")))
           (ts (plist-get d :last-seen))
           (now (truncate (float-time))))
      (should (<= (abs (- ts now)) 2)))))

;;; ---- Group 5: Session CRUD ----

(ert-deftest jabber-omemo-store-test-session-save-load-roundtrip ()
  "save-session + load-session round-trips."
  (jabber-omemo-store-test-with-db
    (let ((blob (string-as-unibyte "session-data\x00\x01")))
      (jabber-omemo-store-save-session "me@example.com" "alice@example.com"
                                       42 blob)
      (should (equal blob (jabber-omemo-store-load-session
                           "me@example.com" "alice@example.com" 42))))))

(ert-deftest jabber-omemo-store-test-session-load-unknown ()
  "load-session returns nil for unknown."
  (jabber-omemo-store-test-with-db
    (should (null (jabber-omemo-store-load-session "me@example.com"
                                                    "alice@example.com" 99)))))

(ert-deftest jabber-omemo-store-test-session-delete ()
  "delete-session removes the session."
  (jabber-omemo-store-test-with-db
    (let ((blob (string-as-unibyte "session")))
      (jabber-omemo-store-save-session "me@example.com" "alice@example.com"
                                       42 blob)
      (jabber-omemo-store-delete-session "me@example.com" "alice@example.com" 42)
      (should (null (jabber-omemo-store-load-session
                     "me@example.com" "alice@example.com" 42))))))

(ert-deftest jabber-omemo-store-test-session-all ()
  "all-sessions lists all devices for a peer."
  (jabber-omemo-store-test-with-db
    (let ((b1 (string-as-unibyte "s1"))
          (b2 (string-as-unibyte "s2")))
      (jabber-omemo-store-save-session "me@example.com" "alice@example.com"
                                       42 b1)
      (jabber-omemo-store-save-session "me@example.com" "alice@example.com"
                                       99 b2)
      (let ((all (jabber-omemo-store-all-sessions "me@example.com"
                                                   "alice@example.com")))
        (should (= 2 (length all)))
        (should (cl-find 42 all :key (lambda (p) (plist-get p :device-id))))
        (should (cl-find 99 all :key (lambda (p) (plist-get p :device-id))))))))

;;; ---- Group 6: Skipped key CRUD ----

(ert-deftest jabber-omemo-store-test-skipped-key-save-load-roundtrip ()
  "save + load round-trips a skipped key."
  (jabber-omemo-store-test-with-db
    (let ((dh (string-as-unibyte "dh-key-data"))
          (mk (string-as-unibyte "msg-key-data")))
      (jabber-omemo-store-save-skipped-key "me@example.com" "alice@example.com"
                                            42 dh 7 mk)
      (should (equal mk (jabber-omemo-store-load-skipped-key
                         "me@example.com" "alice@example.com" 42 dh 7))))))

(ert-deftest jabber-omemo-store-test-skipped-key-load-unknown ()
  "load returns nil for unknown skipped key."
  (jabber-omemo-store-test-with-db
    (should (null (jabber-omemo-store-load-skipped-key
                   "me@example.com" "alice@example.com" 42
                   (string-as-unibyte "x") 0)))))

(ert-deftest jabber-omemo-store-test-skipped-key-delete ()
  "delete removes a skipped key after use."
  (jabber-omemo-store-test-with-db
    (let ((dh (string-as-unibyte "dh"))
          (mk (string-as-unibyte "mk")))
      (jabber-omemo-store-save-skipped-key "me@example.com" "alice@example.com"
                                            42 dh 7 mk)
      (jabber-omemo-store-delete-skipped-key "me@example.com" "alice@example.com"
                                              42 dh 7)
      (should (null (jabber-omemo-store-load-skipped-key
                     "me@example.com" "alice@example.com" 42 dh 7))))))

(ert-deftest jabber-omemo-store-test-skipped-key-delete-old ()
  "delete-old-skipped-keys removes by age."
  (jabber-omemo-store-test-with-db
    (let ((dh (string-as-unibyte "dh"))
          (mk (string-as-unibyte "mk"))
          (now (truncate (float-time))))
      ;; Insert an old key by directly using SQL
      (sqlite-execute jabber-db--connection "\
INSERT INTO omemo_skipped_keys
  (account, jid, device_id, dh_key, message_number, message_key, created_at)
  VALUES (?, ?, ?, ?, ?, ?, ?)"
        (list "me@example.com" "alice@example.com" 42 dh 1 mk (- now 7200)))
      ;; Insert a recent key
      (jabber-omemo-store-save-skipped-key "me@example.com" "alice@example.com"
                                            42 dh 2 mk)
      ;; Delete keys older than 1 hour
      (jabber-omemo-store-delete-old-skipped-keys "me@example.com" 3600)
      ;; Old key gone, recent key remains
      (should (null (jabber-omemo-store-load-skipped-key
                     "me@example.com" "alice@example.com" 42 dh 1)))
      (should (jabber-omemo-store-load-skipped-key
               "me@example.com" "alice@example.com" 42 dh 2)))))

(provide 'jabber-omemo-store-tests)

;;; jabber-omemo-store-tests.el ends here
