;;; jabber-omemo-message-tests.el --- ERT tests for OMEMO message encrypt/decrypt  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-chat)
(require 'jabber-omemo)

;;; Test infrastructure

(defmacro jabber-omemo-message-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Clears OMEMO in-memory caches and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-omemo-message-test--dir
           (make-temp-file "jabber-omemo-msg-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-omemo-message-test--dir))
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
       (when (file-directory-p jabber-omemo-message-test--dir)
         (delete-directory jabber-omemo-message-test--dir t)))))

;;; Group 1: Fallback body

(ert-deftest jabber-omemo-message-test-fallback-body ()
  "Fallback body constant is a non-empty string."
  (should (stringp jabber-omemo-fallback-body))
  (should (> (length jabber-omemo-fallback-body) 0)))

;;; Group 2: Parse encrypted XML

(ert-deftest jabber-omemo-message-test-parse-encrypted-basic ()
  "parse-encrypted extracts sid, iv, payload, and keys from XML."
  (let* ((xml-data
          `(message ((from . "alice@example.com/phone")
                     (to . "bob@example.com/laptop")
                     (type . "chat"))
                    (body () "fallback text")
                    (encrypted ((xmlns . "eu.siacs.conversations.axolotl"))
                               (header ((sid . "12345"))
                                       (key ((rid . "67890") (prekey . "true"))
                                            ,(base64-encode-string "encrypted-key-data" t))
                                       (key ((rid . "11111"))
                                            ,(base64-encode-string "other-key-data" t))
                                       (iv () ,(base64-encode-string (make-string 12 ?x) t)))
                               (payload () ,(base64-encode-string "ciphertext-data" t)))))
         (parsed (jabber-omemo--parse-encrypted xml-data)))
    (should parsed)
    (should (= 12345 (plist-get parsed :sid)))
    (should (= 12 (length (plist-get parsed :iv))))
    (should (string= "ciphertext-data" (plist-get parsed :payload)))
    (let ((keys (plist-get parsed :keys)))
      (should (= 2 (length keys)))
      (should (= 67890 (car (nth 0 keys))))
      (should (plist-get (cdr (nth 0 keys)) :pre-key-p))
      (should (string= "encrypted-key-data"
                        (plist-get (cdr (nth 0 keys)) :data)))
      (should (= 11111 (car (nth 1 keys))))
      (should-not (plist-get (cdr (nth 1 keys)) :pre-key-p)))))

(ert-deftest jabber-omemo-message-test-parse-encrypted-no-element ()
  "parse-encrypted returns nil when no <encrypted> element."
  (let ((xml-data '(message ((from . "alice@example.com")
                             (type . "chat"))
                            (body () "hello"))))
    (should-not (jabber-omemo--parse-encrypted xml-data))))

(ert-deftest jabber-omemo-message-test-parse-encrypted-no-payload ()
  "parse-encrypted handles heartbeat messages (no payload)."
  (let* ((xml-data
          `(message ((from . "alice@example.com/phone")
                     (type . "chat"))
                    (encrypted ((xmlns . "eu.siacs.conversations.axolotl"))
                               (header ((sid . "999"))
                                       (key ((rid . "888"))
                                            ,(base64-encode-string "key-data" t))
                                       (iv () ,(base64-encode-string (make-string 12 0) t))))))
         (parsed (jabber-omemo--parse-encrypted xml-data)))
    (should parsed)
    (should (= 999 (plist-get parsed :sid)))
    (should-not (plist-get parsed :payload))
    (should (= 1 (length (plist-get parsed :keys))))))

;;; Group 3: Build encrypted XML

(ert-deftest jabber-omemo-message-test-build-encrypted-structure ()
  "build-encrypted-xml produces correct sexp structure."
  (jabber-omemo-message-test-with-db
    (let* ((store-blob-a (jabber-omemo-setup-store))
           (store-ptr-a (jabber-omemo-deserialize-store store-blob-a))
           (store-blob-b (jabber-omemo-setup-store))
           (store-ptr-b (jabber-omemo-deserialize-store store-blob-b))
           (account "alice@example.com")
           (peer "bob@example.com")
           (our-did 42)
           (peer-did 99))
      ;; Set up account state
      (puthash account store-ptr-a jabber-omemo--stores)
      (puthash account our-did jabber-omemo--device-ids)
      ;; Get bundle from B and establish session A->B
      (let* ((bundle-b (jabber-omemo-get-bundle store-ptr-b))
             (pre-keys (plist-get bundle-b :pre-keys))
             (pk (car pre-keys))
             (session-ptr (jabber-omemo-initiate-session
                           store-ptr-a
                           (plist-get bundle-b :signature)
                           (plist-get bundle-b :signed-pre-key)
                           (plist-get bundle-b :identity-key)
                           (cdr pk)
                           (plist-get bundle-b :signed-pre-key-id)
                           (car pk))))
        (jabber-omemo-store-save-session account peer peer-did
                                         (jabber-omemo-serialize-session session-ptr))
        (puthash (jabber-omemo--session-key account peer peer-did)
                 session-ptr jabber-omemo--sessions)
        ;; Build the encrypted XML using a mock jc
        (let* ((jc (list :mock-jc))
               (enc-result (jabber-omemo-encrypt-message
                            (encode-coding-string "Hello" 'utf-8))))
          (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) account)))
            (let ((xml (jabber-omemo--build-encrypted-xml
                        jc (list (cons peer-did session-ptr)) enc-result)))
              ;; Verify structure
              (should (eq 'encrypted (car xml)))
              (should (string= "eu.siacs.conversations.axolotl"
                               (cdr (assq 'xmlns (cadr xml)))))
              (let ((header (car (jabber-xml-get-children xml 'header))))
                (should header)
                (should (string= "42" (jabber-xml-get-attribute header 'sid)))
                ;; Should have one key element and an iv
                (should (jabber-xml-get-children header 'key))
                (should (jabber-xml-get-children header 'iv)))
              ;; Should have a payload
              (should (jabber-xml-get-children xml 'payload)))))))))

;;; Group 4: detect-encrypted

(ert-deftest jabber-omemo-message-test-detect-encrypted-returns-parsed ()
  "detect-encrypted returns (:type omemo :parsed ...) for OMEMO stanza."
  (let* ((xml-data
          `(message ((from . "alice@example.com/phone")
                     (type . "chat"))
                    (body () "fallback")
                    (encrypted ((xmlns . "eu.siacs.conversations.axolotl"))
                               (header ((sid . "12345"))
                                       (key ((rid . "67890") (prekey . "true"))
                                            ,(base64-encode-string "key-data" t))
                                       (iv () ,(base64-encode-string (make-string 12 ?x) t)))
                               (payload () ,(base64-encode-string "ciphertext" t)))))
         (result (jabber-omemo--detect-encrypted xml-data)))
    (should result)
    (should (eq 'omemo (plist-get result :type)))
    (should (plist-get result :parsed))
    (should (= 12345 (plist-get (plist-get result :parsed) :sid)))))

(ert-deftest jabber-omemo-message-test-detect-encrypted-returns-nil-for-plain ()
  "detect-encrypted returns nil for plain stanza."
  (let ((xml-data '(message ((from . "alice@example.com")
                             (type . "chat"))
                            (body () "hello plain"))))
    (should-not (jabber-omemo--detect-encrypted xml-data))))

(ert-deftest jabber-omemo-message-test-detect-encrypted-muc-echo ()
  "detect-encrypted returns muc-echo plist and consumes cache."
  (let ((jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal)))
    (puthash "msg-001" "secret text" jabber-omemo--sent-muc-plaintexts)
    (let* ((xml-data '(message ((from . "room@conf.example.com/me")
                                (id . "msg-001")
                                (type . "groupchat"))
                               (body () "fallback")))
           (result (jabber-omemo--detect-encrypted xml-data)))
      (should result)
      (should (eq 'muc-echo (plist-get result :type)))
      (should (string= "secret text" (plist-get result :cached)))
      ;; Cache entry consumed
      (should-not (gethash "msg-001" jabber-omemo--sent-muc-plaintexts)))))

;;; Group 5: Trust label formatting

(ert-deftest jabber-omemo-message-test-trust-labels ()
  "Trust labels map correctly."
  (should (string= "undecided" (jabber-omemo--trust-label 0)))
  (should (string= "TOFU" (jabber-omemo--trust-label 1)))
  (should (string= "verified" (jabber-omemo--trust-label 2)))
  (should (string= "UNTRUSTED" (jabber-omemo--trust-label -1))))

;;; Group 6: Fingerprint formatting

(ert-deftest jabber-omemo-message-test-format-fingerprint ()
  "format-fingerprint produces space-separated hex."
  (let ((key (unibyte-string #xDE #xAD #xBE #xEF)))
    (should (string= "DE AD BE EF"
                      (jabber-omemo--format-fingerprint key)))))

;;; Group 7: Full encrypt/decrypt round-trip

(ert-deftest jabber-omemo-message-test-encrypt-decrypt-roundtrip ()
  "Encrypt and decrypt a message round-trips the plaintext."
  (jabber-omemo-message-test-with-db
    (let* ((store-blob-a (jabber-omemo-setup-store))
           (store-ptr-a (jabber-omemo-deserialize-store store-blob-a))
           (store-blob-b (jabber-omemo-setup-store))
           (store-ptr-b (jabber-omemo-deserialize-store store-blob-b))
           (plaintext "Hello, OMEMO world!")
           (plaintext-bytes (encode-coding-string plaintext 'utf-8)))
      ;; A initiates session with B's bundle
      (let* ((bundle-b (jabber-omemo-get-bundle store-ptr-b))
             (pre-keys (plist-get bundle-b :pre-keys))
             (pk (car pre-keys))
             (session-a->b (jabber-omemo-initiate-session
                            store-ptr-a
                            (plist-get bundle-b :signature)
                            (plist-get bundle-b :signed-pre-key)
                            (plist-get bundle-b :identity-key)
                            (cdr pk)
                            (plist-get bundle-b :signed-pre-key-id)
                            (car pk))))
        ;; A encrypts message
        (let* ((enc-result (jabber-omemo-encrypt-message plaintext-bytes))
               (msg-key (plist-get enc-result :key))
               (iv (plist-get enc-result :iv))
               (ciphertext (plist-get enc-result :ciphertext))
               ;; A encrypts the key for B
               (encrypted-key (jabber-omemo-encrypt-key session-a->b msg-key))
               (key-data (plist-get encrypted-key :data))
               (pre-key-p (plist-get encrypted-key :pre-key-p)))
          ;; B decrypts the key
          (let* ((session-b (jabber-omemo-make-session))
                 (decrypted-key (jabber-omemo-decrypt-key
                                 session-b store-ptr-b pre-key-p key-data))
                 ;; B decrypts the message
                 (decrypted-bytes (jabber-omemo-decrypt-message
                                   decrypted-key iv ciphertext))
                 (decrypted-text (decode-coding-string decrypted-bytes 'utf-8)))
            (should (string= plaintext decrypted-text))))))))

;;; Group 8: aesgcm URL construction

(ert-deftest jabber-omemo-message-test-build-aesgcm-url ()
  "Build aesgcm:// URL from HTTPS URL, IV, and key."
  (let* ((iv (decode-hex-string "8c3d050e9386ec173861778f"))
         (key (decode-hex-string "68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587"))
         (url (jabber-omemo--build-aesgcm-url
               "https://download.example.org/file.jpg" iv key)))
    (should (string= url "aesgcm://download.example.org/file.jpg#8c3d050e9386ec173861778f68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587"))))

(ert-deftest jabber-omemo-message-test-aesgcm-url-round-trip ()
  "Build URL then parse it back, recovering same IV and key."
  (let* ((enc (jabber-omemo-aesgcm-encrypt (make-string 100 ?x)))
         (iv (plist-get enc :iv))
         (key (plist-get enc :key))
         (url (jabber-omemo--build-aesgcm-url "https://host/f.jpg" iv key))
         (parsed (jabber-chat--parse-aesgcm-url url)))
    (should (string= iv (plist-get parsed :iv)))
    (should (string= key (plist-get parsed :key)))
    (should (string= "https://host/f.jpg" (plist-get parsed :https-url)))))

(ert-deftest jabber-omemo-message-test-aesgcm-file-round-trip ()
  "Encrypt file contents, build URL, parse URL, decrypt, compare."
  (let* ((original "This is test file content with UTF-8: café")
         (plaintext (encode-coding-string original 'utf-8))
         (enc (jabber-omemo-aesgcm-encrypt plaintext))
         (url (jabber-omemo--build-aesgcm-url
               "https://upload.example.org/abc/test.txt"
               (plist-get enc :iv)
               (plist-get enc :key)))
         (parsed (jabber-chat--parse-aesgcm-url url))
         (decrypted (jabber-omemo-aesgcm-decrypt
                     (plist-get parsed :key)
                     (plist-get parsed :iv)
                     (plist-get enc :ciphertext))))
    (should (string= plaintext decrypted))
    (should (string-prefix-p "aesgcm://" url))
    (should (string= "https://upload.example.org/abc/test.txt"
                      (plist-get parsed :https-url)))))

;;; Group 9: aesgcm upload integration

(ert-deftest jabber-omemo-message-test-build-aesgcm-url-rejects-non-https ()
  "build-aesgcm-url signals error when given a non-https URL."
  (let* ((iv (decode-hex-string "8c3d050e9386ec173861778f"))
         (key (decode-hex-string "68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587")))
    (should-error (jabber-omemo--build-aesgcm-url
                   "aesgcm://host/path#oldfrag" iv key)
                  :type 'error)))

(ert-deftest jabber-omemo-message-test-httpupload-transform-nil-without-omemo ()
  "Transform returns nil when encryption is not OMEMO."
  (let ((jabber-chat-encryption 'plaintext))
    (should-not (jabber-omemo--httpupload-transform "/tmp/test.png" #'identity))))

(ert-deftest jabber-omemo-message-test-httpupload-transform-encrypts-with-omemo ()
  "Transform returns (filepath . callback) when OMEMO is active."
  (let* ((tmp (make-temp-file "omemo-test-" nil ".txt"))
         (jabber-chat-encryption 'omemo)
         result)
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "test content"))
          (setq result (jabber-omemo--httpupload-transform tmp #'identity))
          (should (consp result))
          (should (stringp (car result)))
          (should (functionp (cdr result))))
      (ignore-errors (delete-file tmp))
      (when (and result (stringp (car result)))
        (ignore-errors (delete-file (car result)))))))

(ert-deftest jabber-omemo-message-test-httpupload-send-url-handles-aesgcm ()
  "Send-url override returns non-nil for aesgcm:// URLs."
  (cl-letf (((symbol-function 'jabber-omemo--ensure-sessions)
             (lambda (_jc _jid callback) (funcall callback nil)))
            ((symbol-function 'jabber-omemo--send-encrypted)
             (lambda (&rest _) nil))
            ((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-jid-user)
             (lambda (jid) jid)))
    (should (jabber-omemo--httpupload-send-url
             'fake-jc "alice@example.com"
             "aesgcm://host/file#abc123"))))

(ert-deftest jabber-omemo-message-test-httpupload-send-url-skips-https ()
  "Send-url override returns nil for https:// URLs."
  (should-not (jabber-omemo--httpupload-send-url
               'fake-jc "alice@example.com"
               "https://host/file")))

(provide 'jabber-omemo-message-tests)
;;; jabber-omemo-message-tests.el ends here
