;;; jabber-test-omemo-message.el --- Tests for jabber-omemo-message  -*- lexical-binding: t; -*-

;;; Commentary:

;; OMEMO message encryption and decryption.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-message-reply)
(require 'jabber-omemo)

(defvar jabber-group nil)
(defvar jabber-muc-participants nil)

;;; Test infrastructure

(defmacro jabber-test-omemo-message-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database.
Clears OMEMO in-memory caches and tears down on exit."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-omemo-message--dir
           (make-temp-file "jabber-omemo-msg-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-test-omemo-message--dir))
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
       (when (file-directory-p jabber-test-omemo-message--dir)
         (delete-directory jabber-test-omemo-message--dir t)))))

;;; Group 1: Fallback body

(ert-deftest jabber-test-omemo-message-fallback-body ()
  "Fallback body constant is a non-empty string."
  (should (stringp jabber-omemo-fallback-body))
  (should (> (length jabber-omemo-fallback-body) 0)))

;;; Group 2: Parse encrypted XML

(ert-deftest jabber-test-omemo-message-parse-encrypted-basic ()
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

(ert-deftest jabber-test-omemo-message-parse-encrypted-no-element ()
  "parse-encrypted returns nil when no <encrypted> element."
  (let ((xml-data '(message ((from . "alice@example.com")
                             (type . "chat"))
                            (body () "hello"))))
    (should-not (jabber-omemo--parse-encrypted xml-data))))

(ert-deftest jabber-test-omemo-message-parse-encrypted-no-payload ()
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

(ert-deftest jabber-test-omemo-message-build-encrypted-structure ()
  "build-encrypted-xml produces correct sexp structure."
  (jabber-test-omemo-message-with-db
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

(ert-deftest jabber-test-omemo-message-detect-encrypted-returns-parsed ()
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

(ert-deftest jabber-test-omemo-message-detect-encrypted-returns-nil-for-plain ()
  "detect-encrypted returns nil for plain stanza."
  (let ((xml-data '(message ((from . "alice@example.com")
                             (type . "chat"))
                            (body () "hello plain"))))
    (should-not (jabber-omemo--detect-encrypted xml-data))))

(ert-deftest jabber-test-omemo-message-muc-echo-requires-exact-occupant ()
  "Only the exact local occupant echo may recover sent plaintext."
  (let* ((jc 'connection)
         (room "room@conf.example.com")
         (id "msg-001")
         (key (jabber-omemo--muc-echo-key
               jc room (concat room "/me") id))
         (jabber-omemo--sent-muc-plaintexts
          (make-hash-table :test #'equal))
         (xml-data `(message ((from . ,(concat room "/me"))
                              (id . ,id)
                              (type . "groupchat"))
                             (body () "fallback")))
         (detected '(:type omemo :parsed (:payload "ciphertext"))))
    (puthash key "secret text" jabber-omemo--sent-muc-plaintexts)
    (jabber-omemo--decrypt-handler jc xml-data detected)
    (should (string= "secret text"
                     (car (jabber-xml-node-children
                           (car (jabber-xml-get-children
                                 xml-data 'body))))))
    (should-not (gethash key jabber-omemo--sent-muc-plaintexts))))

(ert-deftest jabber-test-omemo-message-muc-echo-rejects-foreign-occupant ()
  "A foreign occupant reusing our message id cannot read cached plaintext."
  (let* ((jc 'connection)
         (room "room@conf.example.com")
         (id "msg-001")
         (key (jabber-omemo--muc-echo-key
               jc room (concat room "/me") id))
         (jabber-omemo--sent-muc-plaintexts
          (make-hash-table :test #'equal))
         (xml-data `(message ((from . ,(concat room "/mallory"))
                              (id . ,id)
                              (type . "groupchat"))
                             (body () "fallback")))
         (runs 0))
    (puthash key "secret text" jabber-omemo--sent-muc-plaintexts)
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (_jc xml _parsed)
                 (cl-incf runs)
                 (jabber-chat--set-body xml "decrypted foreign"))))
      (jabber-omemo--decrypt-handler
       jc xml-data '(:type omemo :parsed (:payload "ciphertext"))))
    (should (= 1 runs))
    (should (gethash key jabber-omemo--sent-muc-plaintexts))))

(ert-deftest jabber-test-omemo-message-muc-echo-rejects-other-room ()
  "Another room reusing our message id cannot read cached plaintext."
  (let* ((jc 'connection)
         (room "room@conf.example.com")
         (other "other@conf.example.com")
         (id "msg-001")
         (key (jabber-omemo--muc-echo-key
               jc room (concat room "/me") id))
         (jabber-omemo--sent-muc-plaintexts
          (make-hash-table :test #'equal))
         (xml-data `(message ((from . ,(concat other "/me"))
                              (id . ,id)
                              (type . "groupchat"))
                             (body () "fallback")))
         (runs 0))
    (puthash key "secret text" jabber-omemo--sent-muc-plaintexts)
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (_jc xml _parsed)
                 (cl-incf runs)
                 (jabber-chat--set-body xml "decrypted other"))))
      (jabber-omemo--decrypt-handler
       jc xml-data '(:type omemo :parsed (:payload "ciphertext"))))
    (should (= 1 runs))
    (should (gethash key jabber-omemo--sent-muc-plaintexts))))

;;; Group 5: Trust label formatting

(ert-deftest jabber-test-omemo-message-trust-labels ()
  "Trust labels map correctly."
  (should (string= "undecided" (jabber-omemo--trust-label 0)))
  (should (string= "TOFU" (jabber-omemo--trust-label 1)))
  (should (string= "verified" (jabber-omemo--trust-label 2)))
  (should (string= "UNTRUSTED" (jabber-omemo--trust-label -1))))

;;; Group 6: Fingerprint formatting

(ert-deftest jabber-test-omemo-message-format-fingerprint ()
  "format-fingerprint produces space-separated hex."
  (let ((key (unibyte-string #xDE #xAD #xBE #xEF)))
    (should (string= "DE AD BE EF"
                      (jabber-omemo--format-fingerprint key)))))

;;; Group 7: Full encrypt/decrypt round-trip

(ert-deftest jabber-test-omemo-message-encrypt-decrypt-roundtrip ()
  "Encrypt and decrypt a message round-trips the plaintext."
  (jabber-test-omemo-message-with-db
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

(ert-deftest jabber-test-omemo-message-build-aesgcm-url ()
  "Build aesgcm:// URL from HTTPS URL, IV, and key."
  (let* ((iv (decode-hex-string "8c3d050e9386ec173861778f"))
         (key (decode-hex-string "68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587"))
         (url (jabber-omemo--build-aesgcm-url
               "https://download.example.org/file.jpg" iv key)))
    (should (string= url "aesgcm://download.example.org/file.jpg#8c3d050e9386ec173861778f68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587"))))

(ert-deftest jabber-test-omemo-message-aesgcm-url-round-trip ()
  "Build URL then parse it back, recovering same IV and key."
  (let* ((enc (jabber-omemo-aesgcm-encrypt (make-string 100 ?x)))
         (iv (plist-get enc :iv))
         (key (plist-get enc :key))
         (url (jabber-omemo--build-aesgcm-url "https://host/f.jpg" iv key))
         (parsed (jabber-chat--parse-aesgcm-url url)))
    (should (string= iv (plist-get parsed :iv)))
    (should (string= key (plist-get parsed :key)))
    (should (string= "https://host/f.jpg" (plist-get parsed :https-url)))))

(ert-deftest jabber-test-omemo-message-aesgcm-file-round-trip ()
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

(ert-deftest jabber-test-omemo-message-build-aesgcm-url-rejects-non-https ()
  "build-aesgcm-url signals error when given a non-https URL."
  (let* ((iv (decode-hex-string "8c3d050e9386ec173861778f"))
         (key (decode-hex-string "68e9af38a97aaf82faa4063b4d0878a61261534410c8a84331eaac851759f587")))
    (should-error (jabber-omemo--build-aesgcm-url
                   "aesgcm://host/path#oldfrag" iv key)
                  :type 'error)))

(ert-deftest jabber-test-omemo-message-httpupload-transform-nil-without-omemo ()
  "Transform returns nil when encryption is not OMEMO."
  (let ((jabber-chat-encryption 'plaintext))
    (should-not (jabber-omemo--httpupload-transform "/tmp/test.png" #'identity))))

(ert-deftest jabber-test-omemo-message-httpupload-transform-encrypts-with-omemo ()
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

(ert-deftest jabber-test-omemo-message-httpupload-send-url-handles-aesgcm ()
  "Send-url override returns non-nil for aesgcm:// URLs."
  (let (sent)
    (cl-letf (((symbol-function 'jabber-muc-joined-p)
               (lambda (_group &optional _jc) nil))
              ((symbol-function 'jabber-chat-create-buffer)
               (lambda (_jc _jid) (current-buffer)))
              ((symbol-function 'jabber-omemo--send-chat)
               (lambda (jc body &rest _)
                 (setq sent (list jc body (current-buffer))))))
      (should (jabber-omemo--httpupload-send-url
               'fake-jc "alice@example.com"
               "aesgcm://host/file#abc123"))
      (should (equal (list 'fake-jc "aesgcm://host/file#abc123"
                           (current-buffer))
                     sent)))))

(ert-deftest jabber-test-omemo-message-httpupload-stall-fails-on-reset ()
  "A reset cancels a direct upload URL send before its late callback."
  (let ((chat-buffer (generate-new-buffer " *omemo-upload-chat-test*"))
        (jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (failures 0)
        (encrypted 0)
        continuation)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local jabber-chatting-with "alice@example.com"))
          (cl-letf (((symbol-function 'jabber-muc-joined-p)
                     (lambda (_group &optional _jc) nil))
                    ((symbol-function 'jabber-chat-create-buffer)
                     (lambda (_jc _jid) chat-buffer))
                    ((symbol-function 'jabber-jid-user) #'identity)
                    ((symbol-function 'jabber-omemo--display-pending)
                     (lambda (&rest _) 'pending-node))
                    ((symbol-function 'jabber-omemo--ensure-sessions)
                     (lambda (_jc _jid callback)
                       (setq continuation callback)))
                    ((symbol-function 'jabber-omemo--send-encrypted)
                     (lambda (&rest _) (cl-incf encrypted)))
                    ((symbol-function 'jabber-omemo--send-failed)
                     (lambda (&rest _) (cl-incf failures))))
            (jabber-omemo--httpupload-send-url
             'fake-jc "alice@example.com" "aesgcm://host/file#abc123")
            (jabber-omemo--session-reset 'fake-jc)
            (funcall continuation '((1 . session))))
          (should (= 1 failures))
          (should (= 0 encrypted)))
      (kill-buffer chat-buffer))))

(ert-deftest jabber-test-omemo-message-httpupload-send-url-muc-from-any-buffer ()
  "An aesgcm URL for a joined room is sent in that room's buffer.
The upload callback fires from a process sentinel where the current
buffer is arbitrary; the room must be derived from the JID, not from
buffer-local `jabber-group'."
  (let ((room "room@conference.example.com")
        (room-buffer (generate-new-buffer " *omemo-muc-upload-test*"))
        (sent-group nil))
    (unwind-protect
        (progn
          (with-current-buffer room-buffer
            (setq-local jabber-group room))
          (cl-letf (((symbol-function 'jabber-muc-joined-p)
                     (lambda (group &optional _jc) (equal group room)))
                    ((symbol-function 'jabber-muc-create-buffer)
                     (lambda (_jc _group) room-buffer))
                    ((symbol-function 'jabber-omemo--send-muc)
                     (lambda (_jc _body &optional _extra)
                       (setq sent-group (bound-and-true-p jabber-group)))))
            (with-temp-buffer
              (should (jabber-omemo--httpupload-send-url
                       'fake-jc room "aesgcm://host/file#abc123"))))
          (should (equal sent-group room)))
      (kill-buffer room-buffer))))

(ert-deftest jabber-test-omemo-message-httpupload-send-url-skips-https ()
  "Send-url override returns nil for https:// URLs."
  (should-not (jabber-omemo--httpupload-send-url
               'fake-jc "alice@example.com"
               "https://host/file")))

;;; Group 10: Trust filtering

(ert-deftest jabber-test-omemo-message-trusted-sessions-excludes-untrusted ()
  "trusted-sessions drops devices with trust = -1."
  (let ((sessions '((100 . fake-ptr-100) (200 . fake-ptr-200) (300 . fake-ptr-300))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--session-jid-for-did)
               (lambda (_jc did)
                 (format "peer%d@example.com" did)))
              ((symbol-function 'jabber-omemo-store-load-trust)
               (lambda (_account _jid did)
                 (pcase did
                   (100 (list :identity-key "k1" :trust 1 :first-seen 0))
                   (200 (list :identity-key "k2" :trust -1 :first-seen 0))
                   (300 (list :identity-key "k3" :trust 2 :first-seen 0))))))
      (let ((result (jabber-omemo--trusted-sessions 'fake-jc sessions)))
        (should (= 2 (length result)))
        (should (assq 100 result))
        (should-not (assq 200 result))
        (should (assq 300 result))))))

(ert-deftest jabber-test-omemo-message-trusted-sessions-keeps-undecided ()
  "trusted-sessions keeps devices with trust = 0 (undecided)."
  (let ((sessions '((100 . fake-ptr-100))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--session-jid-for-did)
               (lambda (_jc _did) "peer@example.com"))
              ((symbol-function 'jabber-omemo-store-load-trust)
               (lambda (_account _jid _did)
                 (list :identity-key "k" :trust 0 :first-seen 0))))
      (let ((result (jabber-omemo--trusted-sessions 'fake-jc sessions)))
        (should (= 1 (length result)))))))

(ert-deftest jabber-test-omemo-message-trusted-sessions-keeps-no-trust-record ()
  "trusted-sessions keeps devices with no trust record."
  (let ((sessions '((100 . fake-ptr-100))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--session-jid-for-did)
               (lambda (_jc _did) "peer@example.com"))
              ((symbol-function 'jabber-omemo-store-load-trust)
               (lambda (_account _jid _did) nil)))
      (let ((result (jabber-omemo--trusted-sessions 'fake-jc sessions)))
        (should (= 1 (length result)))))))

(ert-deftest jabber-test-omemo-message-build-encrypted-rejects-all-untrusted ()
  "build-encrypted-xml signals error when all devices are untrusted."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-omemo--session-jid-for-did)
             (lambda (_jc _did) "peer@example.com"))
            ((symbol-function 'jabber-omemo-store-load-trust)
             (lambda (_account _jid _did)
               (list :identity-key "k" :trust -1 :first-seen 0))))
    (should-error
     (jabber-omemo--build-encrypted-xml
      'fake-jc '((100 . fake-ptr)) '(:key "k" :iv "i" :ciphertext "c"))
     :type 'user-error)))

;;; Group 12: Structured decrypt errors

(ert-deftest jabber-test-omemo-message-decrypt-error-conditions ()
  "Decrypt error subtypes inherit from `jabber-omemo-error'."
  (should (memq 'jabber-omemo-error
                (get 'jabber-omemo-not-for-us 'error-conditions)))
  (should (memq 'jabber-omemo-error
                (get 'jabber-omemo-no-session 'error-conditions)))
  (should (memq 'jabber-omemo-error
                (get 'jabber-omemo-prekey-failed 'error-conditions))))

(ert-deftest jabber-test-omemo-message-decrypt-stanza-not-for-us ()
  "decrypt-stanza signals `jabber-omemo-not-for-us' when no key for our device."
  (let ((jabber-omemo--device-ids (make-hash-table :test 'equal)))
    (puthash "me@example.com" 42 jabber-omemo--device-ids)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))))
            (parsed (list :sid 12345
                          :iv (make-string 12 0)
                          :payload "ciphertext"
                          ;; Only a key for device 999, not for us (42).
                          :keys '((999 . (:data "k" :pre-key-p nil))))))
        (should-error
         (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed)
         :type 'jabber-omemo-not-for-us)))))

(ert-deftest jabber-test-omemo-message-decrypt-stanza-no-session ()
  "decrypt-stanza signals `jabber-omemo-no-session' for non-prekey with no session."
  (let ((jabber-omemo--device-ids (make-hash-table :test 'equal))
        (jabber-omemo--stores (make-hash-table :test 'equal))
        (jabber-omemo--sessions (make-hash-table :test 'equal)))
    (puthash "me@example.com" 42 jabber-omemo--device-ids)
    ;; Non-nil store entry to skip the lazy DB load path.
    (puthash "me@example.com" 'fake-store-ptr jabber-omemo--stores)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo-store-load-session)
               (lambda (_account _jid _did) nil)))
      (let ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))))
            (parsed (list :sid 999
                          :iv (make-string 12 0)
                          :payload "ciphertext"
                          ;; pre-key-p nil triggers session lookup.
                          :keys '((42 . (:data "k" :pre-key-p nil))))))
        (should-error
         (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed)
         :type 'jabber-omemo-no-session)))))

(ert-deftest jabber-test-omemo-message-decrypt-stanza-prekey-failed ()
  "decrypt-stanza re-signals C error as `jabber-omemo-prekey-failed' for prekey."
  (let ((jabber-omemo--device-ids (make-hash-table :test 'equal))
        (jabber-omemo--stores (make-hash-table :test 'equal)))
    (puthash "me@example.com" 42 jabber-omemo--device-ids)
    (puthash "me@example.com" 'fake-store-ptr jabber-omemo--stores)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo-make-session)
               (lambda () 'fake-session-ptr))
              ((symbol-function 'jabber-omemo-decrypt-key)
               (lambda (&rest _)
                 (signal 'jabber-omemo-error '("simulated decrypt failure")))))
      (let ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))))
            (parsed (list :sid 999
                          :iv (make-string 12 0)
                          :payload "ciphertext"
                          :keys '((42 . (:data "k" :pre-key-p t))))))
        (should-error
         (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed)
         :type 'jabber-omemo-prekey-failed)))))

(ert-deftest jabber-test-omemo-message-decrypt-stanza-non-prekey-error-propagates ()
  "decrypt-stanza propagates `jabber-omemo-error' verbatim for non-prekey messages."
  (let ((jabber-omemo--device-ids (make-hash-table :test 'equal))
        (jabber-omemo--stores (make-hash-table :test 'equal))
        (jabber-omemo--sessions (make-hash-table :test 'equal)))
    (puthash "me@example.com" 42 jabber-omemo--device-ids)
    (puthash "me@example.com" 'fake-store-ptr jabber-omemo--stores)
    ;; Use a real session pointer because the C module rejects placeholders.
    (puthash (jabber-omemo--session-key "me@example.com" "alice@example.com" 999)
             (jabber-omemo-make-session) jabber-omemo--sessions)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo-decrypt-key)
               (lambda (&rest _)
                 (signal 'jabber-omemo-error '("simulated decrypt failure")))))
      (let ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))))
            (parsed (list :sid 999
                          :iv (make-string 12 0)
                          :payload "ciphertext"
                          :keys '((42 . (:data "k" :pre-key-p nil))))))
        (let ((err (should-error
                    (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed)
                    :type 'jabber-omemo-error)))
          ;; Should be the parent error type, not the prekey-failed subtype.
          (should-not (eq (car err) 'jabber-omemo-prekey-failed)))))))

;;; Group 13: Decrypt handler error recovery

(ert-deftest jabber-test-omemo-message-decrypt-handler-swallows-bodyless-not-for-us ()
  "decrypt-handler leaves a bodyless stanza unchanged when it is not for us."
  (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
             (lambda (&rest _)
               (signal 'jabber-omemo-not-for-us '(42)))))
    (let* ((xml-data '(message ((from . "alice@example.com/phone")
                                 (type . "chat"))
                                (encrypted nil)))
           (detected (list :type 'omemo :parsed nil))
           (result (jabber-omemo--decrypt-handler 'fake-jc xml-data detected)))
      (should (eq result xml-data)))))

(ert-deftest jabber-test-omemo-message-empty-decrypt-failure-remains-bodyless ()
  "A failed empty OMEMO stanza stays bodyless and retryable."
  (let* ((detected (list :type 'omemo :parsed (list :payload nil)))
         (jabber-chat-decrypt-handlers
          (list
           (cons 'omemo
                 (list :detect (lambda (_xml) detected)
                       :decrypt #'jabber-omemo--decrypt-handler
                       :priority 10
                       :error-label "OMEMO"))))
         (jabber-chat--sorted-decrypt-handlers-cache nil)
         (jabber-chat--decrypt-cache (make-hash-table :test #'equal))
         (calls 0))
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (&rest _)
                 (cl-incf calls)
                 (signal 'jabber-omemo-no-session
                         '("alice@example.com" 999)))))
      (dotimes (_ 2)
        (let* ((xml-data '(message ((from . "alice@example.com/phone")
                                    (type . "chat"))
                                   (encrypted nil)))
               (result (jabber-chat--dispatch-decrypt
                        'fake-jc xml-data 'cache-key 'context)))
          (should-not (jabber-xml-get-children result 'body)))))
    (should (= calls 2))
    (should-not (gethash 'cache-key jabber-chat--decrypt-cache))))

(ert-deftest jabber-test-omemo-message-empty-generic-error-remains-bodyless ()
  "A generic failure on an empty OMEMO stanza stays bodyless."
  (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
             (lambda (&rest _) (error "Sender JID unknown"))))
    (let* ((xml-data '(message ((from . "room@example.com/nick")
                                (type . "groupchat"))
                               (encrypted nil)))
           (detected (list :type 'omemo :parsed (list :payload nil)))
           (props (list :decrypt #'jabber-omemo--decrypt-handler
                        :error-label "OMEMO"))
           (result (jabber-chat--try-decrypt
                    'fake-jc xml-data detected props)))
      (should-not (jabber-xml-get-children result 'body)))))

(ert-deftest jabber-test-omemo-message-empty-post-ratchet-failure-is-cached ()
  "An empty failure after ratchet consumption is cached as bodyless."
  (let* ((detected (list :type 'omemo :parsed (list :payload nil)))
         (jabber-chat-decrypt-handlers
          (list
           (cons 'omemo
                 (list :detect (lambda (_xml) detected)
                       :decrypt #'jabber-omemo--decrypt-handler
                       :priority 10
                       :error-label "OMEMO"))))
         (jabber-chat--sorted-decrypt-handlers-cache nil)
         (jabber-chat--decrypt-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (&rest _)
                 (setq jabber-chat--decrypt-consumed-p t)
                 (signal 'jabber-omemo-error
                         '("failed after ratchet consumption")))))
      (let* ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))
                                 (encrypted nil)))
             (result (jabber-chat--dispatch-decrypt
                      'fake-jc xml-data 'cache-key 'context)))
        (should-not (jabber-xml-get-children result 'body))
        (should
         (eq 'no-body
             (plist-get (gethash 'cache-key jabber-chat--decrypt-cache)
                        :outcome)))))))

(ert-deftest jabber-test-omemo-message-empty-prekey-recovery-error-is-bodyless ()
  "An empty pre-key failure stays bodyless when recovery also fails."
  (let* ((detected (list :type 'omemo :parsed (list :payload nil)))
         (props (list :decrypt #'jabber-omemo--decrypt-handler
                      :error-label "OMEMO")))
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (&rest _)
                 (signal 'jabber-omemo-prekey-failed
                         '("alice@example.com" 999 "bad pre-key"))))
              ((symbol-function 'jabber-omemo--recover-prekey-failure)
               (lambda (&rest _) (error "Recovery failed"))))
      (let* ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))
                                 (encrypted nil)))
             (result (jabber-chat--try-decrypt
                      'fake-jc xml-data detected props)))
        (should-not (jabber-xml-get-children result 'body))))))

(ert-deftest jabber-test-omemo-message-decrypt-handler-rejects-payload-not-for-us ()
  "decrypt-handler re-signals when a payload-bearing stanza is not for us."
  (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
             (lambda (&rest _)
               (signal 'jabber-omemo-not-for-us '(42)))))
    (let ((xml-data '(message ((from . "alice@example.com/phone")
                               (type . "chat"))
                              (body () "OMEMO encrypted message")
                              (encrypted nil)))
          (detected (list :type 'omemo
                          :parsed (list :payload "ciphertext"))))
      (should-error
       (jabber-omemo--decrypt-handler 'fake-jc xml-data detected)
       :type 'jabber-omemo-not-for-us))))

(ert-deftest jabber-test-omemo-message-decrypt-handler-no-publish-on-prekey-failure ()
  "decrypt-handler does NOT republish bundle on prekey failure (Dino-style)."
  (let ((publish-called nil))
    (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
               (lambda (&rest _)
                 (signal 'jabber-omemo-prekey-failed
                         (list "alice@example.com" 999 "boom"))))
              ((symbol-function 'jabber-omemo--publish-bundle)
               (lambda (&rest _) (setq publish-called t)))
              ((symbol-function 'jabber-omemo--publish-bundle-if-needed)
               (lambda (&rest _) (setq publish-called t)))
              ;; Session recovery is exercised elsewhere; keep this
              ;; test focused on bundle publishing.
              ((symbol-function 'jabber-omemo--recover-prekey-failure)
               (lambda (&rest _) nil)))
      (let ((xml-data '(message ((from . "alice@example.com/phone")
                                  (type . "chat"))))
            (detected (list :type 'omemo
                            :parsed (list :payload "ciphertext"))))
        (should-error
         (jabber-omemo--decrypt-handler 'fake-jc xml-data detected)
         :type 'jabber-omemo-prekey-failed)
        (should-not publish-called)))))

(ert-deftest jabber-test-omemo-message-decrypt-handler-propagates-other-errors ()
  "decrypt-handler propagates non-recoverable OMEMO errors unchanged."
  (cl-letf (((symbol-function 'jabber-omemo--decrypt-stanza)
             (lambda (&rest _)
               (signal 'jabber-omemo-no-session
                       '("alice@example.com" 999)))))
    (let ((xml-data '(message ((from . "alice@example.com/phone")
                                (type . "chat"))))
          (detected (list :type 'omemo
                          :parsed (list :payload "ciphertext"))))
      (should-error
       (jabber-omemo--decrypt-handler 'fake-jc xml-data detected)
       :type 'jabber-omemo-no-session))))

(ert-deftest jabber-test-omemo-message-decrypt-stanza-no-publish-on-prekey-success ()
  "decrypt-stanza does NOT republish bundle on successful prekey decrypt."
  (jabber-test-omemo-message-with-db
    (let* ((store-blob-a (jabber-omemo-setup-store))
           (store-ptr-a (jabber-omemo-deserialize-store store-blob-a))
           (store-blob-b (jabber-omemo-setup-store))
           (store-ptr-b (jabber-omemo-deserialize-store store-blob-b))
           (account "bob@example.com")
           (peer "alice@example.com")
           (our-did 42)
           (publish-called nil))
      (puthash account store-ptr-b jabber-omemo--stores)
      (puthash account our-did jabber-omemo--device-ids)
      ;; A initiates a session and encrypts a key for B, producing a
      ;; pre-key message that B will decrypt below.
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
                            (car pk)))
             (enc (jabber-omemo-encrypt-message
                   (encode-coding-string "hi" 'utf-8)))
             (msg-key (plist-get enc :key))
             (iv (plist-get enc :iv))
             (ciphertext (plist-get enc :ciphertext))
             (encrypted-key (jabber-omemo-encrypt-key session-a->b msg-key))
             (key-data (plist-get encrypted-key :data))
             (pre-key-p (plist-get encrypted-key :pre-key-p)))
        (should pre-key-p)
        (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) account))
                  ((symbol-function 'jabber-omemo--publish-bundle)
                   (lambda (&rest _) (setq publish-called t)))
                  ((symbol-function 'jabber-omemo--publish-bundle-if-needed)
                   (lambda (&rest _) (setq publish-called t))))
          (let ((xml-data `(message ((from . ,(concat peer "/phone"))
                                     (type . "chat"))))
                (parsed (list :sid 12345
                              :iv iv
                              :payload ciphertext
                              :keys (list (cons our-did
                                                (list :data key-data
                                                      :pre-key-p t))))))
            (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed)
            (should-not publish-called)))))))

;;; Group 14: MUC send hook buffer

(ert-deftest jabber-test-omemo-message-chat-stalled-device-list-fails-on-reset ()
  "A reset fails a chat send stalled before recipient sessions arrive."
  (let ((jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (jabber-chatting-with "friend@example.com")
        (successes 0)
        (failures 0)
        (encrypted 0)
        continuation)
    (cl-letf (((symbol-function 'jabber-jid-user) #'identity)
              ((symbol-function 'jabber-omemo--ensure-sessions)
               (lambda (_jc _jid callback)
                 (setq continuation callback)))
              ((symbol-function 'jabber-omemo--send-encrypted)
               (lambda (&rest _) (cl-incf encrypted)))
              ((symbol-function 'jabber-omemo--send-failed)
               (lambda (&rest _) nil)))
      (jabber-omemo--send-chat
       'fake-jc "corrected"
       '((replace ((xmlns . "urn:xmpp:message-correct:0")
                   (id . "old"))))
       (lambda () (cl-incf successes))
       (lambda (_reason) (cl-incf failures)))
      (jabber-omemo--session-reset 'fake-jc)
      (funcall continuation '((1 . session)))
      (should (= 0 successes))
      (should (= 1 failures))
      (should (= 0 encrypted))
      (should-not
       (gethash 'fake-jc jabber-omemo--pending-send-operations)))))

(ert-deftest jabber-test-omemo-message-chat-setup-error-finishes-operation ()
  "A synchronous session setup error fails and unregisters the send."
  (let ((jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (jabber-chatting-with "friend@example.com")
        (failures 0))
    (cl-letf (((symbol-function 'jabber-jid-user) #'identity)
              ((symbol-function 'jabber-omemo--ensure-sessions)
               (lambda (&rest _) (error "setup failed")))
              ((symbol-function 'jabber-omemo--send-failed)
               (lambda (&rest _) nil)))
      (jabber-omemo--send-chat
       'fake-jc "corrected"
       '((replace ((xmlns . "urn:xmpp:message-correct:0")
                   (id . "old"))))
       #'ignore
       (lambda (_reason) (cl-incf failures))))
    (should (= 1 failures))
    (should-not
     (gethash 'fake-jc jabber-omemo--pending-send-operations))))

(ert-deftest jabber-test-omemo-message-ordinary-chat-stall-fails-on-reset ()
  "A reset fails an ordinary chat send and makes its late callback inert."
  (let ((jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (jabber-chatting-with "friend@example.com")
        (failures 0)
        (encrypted 0)
        continuation)
    (cl-letf (((symbol-function 'jabber-jid-user) #'identity)
              ((symbol-function 'jabber-omemo--display-pending)
               (lambda (&rest _) 'pending-node))
              ((symbol-function 'jabber-omemo--ensure-sessions)
               (lambda (_jc _jid callback)
                 (setq continuation callback)))
              ((symbol-function 'jabber-omemo--send-encrypted)
               (lambda (&rest _) (cl-incf encrypted)))
              ((symbol-function 'jabber-omemo--send-failed)
               (lambda (&rest _) (cl-incf failures))))
      (jabber-omemo--send-chat 'fake-jc "hello")
      (jabber-omemo--session-reset 'fake-jc)
      (funcall continuation '((1 . session)))
      (should (= 1 failures))
      (should (= 0 encrypted))
      (should-not
       (gethash 'fake-jc jabber-omemo--pending-send-operations)))))

(ert-deftest jabber-test-omemo-message-parent-thread-reply-has-no-pending-echo ()
  "A pending encrypted thread reply never appears in its parent buffer."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id nil))
    (let ((jabber-chat-printers (list (lambda (&rest _) t))))
      (cl-letf (((symbol-function 'jabber-db--outgoing-handler) #'ignore))
        (should-not
         (jabber-omemo--display-pending
          (current-buffer) "reply" "reply-1"))))
    (should-not (ewoc-nth jabber-chat-ewoc 0))))

(ert-deftest jabber-test-omemo-message-pending-thread-reply-is-stored-threaded ()
  "A pending encrypted reply remains threaded if encryption later fails."
  (jabber-test-omemo-message-with-db
    (with-temp-buffer
      (setq-local jabber-chatting-with "friend@example.com")
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chat-encryption 'omemo)
      (setq-local jabber-message-reply--thread
                  '(:thread-id "thread-1" :thread-parent-id nil))
      (jabber-db-register-message-thread
       "me@example.com" "friend@example.com" "chat"
       "thread-1" nil "root-1" nil 1)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (jabber-omemo--display-pending
         (current-buffer) "reply" "pending-1"))
      (should
       (equal '("thread-1")
              (car
               (sqlite-select
                jabber-db--connection
                "SELECT thread_id FROM message WHERE stanza_id = ?"
                '("pending-1")))))
      (should-not
       (seq-find
        (lambda (msg) (equal "pending-1" (plist-get msg :id)))
        (jabber-db-backlog
         "me@example.com" "friend@example.com" t 0 nil "chat"))))))

(ert-deftest jabber-test-omemo-message-thread-buffer-keeps-pending-echo ()
  "A pending encrypted reply remains visible in its thread buffer."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (setq-local jabber-message-thread-id "thread-1")
    (setq-local jabber-message-reply--thread
                '(:thread-id "thread-1" :thread-parent-id nil))
    (let ((jabber-chat-printers (list (lambda (&rest _) t))))
      (cl-letf (((symbol-function 'jabber-db--outgoing-handler) #'ignore))
        (should
         (jabber-omemo--display-pending
          (current-buffer) "reply" "reply-1"))))
    (should (ewoc-nth jabber-chat-ewoc -1))))

(defun jabber-test-omemo-message--thread-send-result (source-kind outcome)
  "Run an OMEMO thread send from SOURCE-KIND through OUTCOME."
  (jabber-test-omemo-message-with-db
    (let ((parent (generate-new-buffer " *omemo-thread-parent*"))
          (thread (generate-new-buffer " *omemo-thread-buffer*"))
          (jabber-omemo--pending-send-operations
           (make-hash-table :test #'eq))
          sent continuation (session-calls 0))
      (unwind-protect
          (progn
            (dolist (buffer (list parent thread))
              (with-current-buffer buffer
                (setq-local jabber-buffer-connection 'fake-jc)
                (setq-local jabber-chatting-with "friend@example.com")
                (setq-local jabber-chat-encryption 'omemo)
                (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
                (setq-local jabber-chat--msg-nodes
                            (make-hash-table :test #'equal))))
            (with-current-buffer thread
              (setq-local jabber-message-thread-id "thread-1")
              (setq-local jabber-message-thread-parent-id nil)
              (setq-local jabber-chat-send-hooks
                          '(jabber-message-thread--send-hook
                            jabber-db--outgoing-handler)))
            (with-current-buffer parent
              (setq-local jabber-message-reply--id "root-1")
              (setq-local jabber-message-reply--jid "friend@example.com")
              (setq-local jabber-message-reply--thread
                          '(:thread-id "thread-1"
                            :thread-parent-id nil))
              (setq-local jabber-chat-send-hooks
                          '(jabber-message-reply--send-hook
                            jabber-db--outgoing-handler)))
            (jabber-db-store-message
             "me@example.com" "friend@example.com" "in" "chat"
             "root" 1 nil "root-1" nil nil nil nil nil
             '(:thread-id "thread-1" :thread-parent-id nil))
            (let ((root-id
                   (caar (sqlite-select
                          jabber-db--connection
                          "SELECT id FROM message WHERE stanza_id = ?"
                          '("root-1")))))
              (with-current-buffer thread
                (ewoc-enter-last
                 jabber-chat-ewoc
                 (list :foreign
                       (list :db-id root-id :id "root-1" :body "root"
                             :thread-id "thread-1")))))
            (let ((source-buffer
                   (if (eq source-kind 'parent) parent thread)))
              (with-current-buffer source-buffer
                (let ((jabber-chat-printers (list (lambda (&rest _) t))))
                  (cl-letf
                      (((symbol-function 'jabber-connection-bare-jid)
                        (lambda (_jc) "me@example.com"))
                       ((symbol-function 'jabber-message-thread-find-buffer)
                        (lambda (&rest _) thread))
                       ((symbol-function 'jabber-omemo--ensure-sessions)
                        (lambda (_jc _jid callback)
                          (setq session-calls (1+ session-calls))
                          (cond
                           ((and (eq outcome 'delayed-dead)
                                 (= session-calls 1))
                            (setq continuation callback))
                           ((eq outcome 'failure)
                            (funcall callback nil))
                           (t
                            (funcall callback '((1 . session)))))))
                       ((symbol-function 'jabber-omemo-encrypt-message)
                        (lambda (_plaintext)
                          '(:iv "iv" :key "key" :payload "payload")))
                       ((symbol-function 'jabber-omemo--build-encrypted-xml)
                        (lambda (&rest _)
                          '(encrypted
                            ((xmlns . "eu.siacs.conversations.axolotl")))))
                       ((symbol-function 'jabber-send-sexp)
                        (lambda (_jc _stanza &optional success _failure)
                          (setq sent t)
                          (when success (funcall success)))))
                    (jabber-omemo--send-chat 'fake-jc "reply")
                    (when (eq outcome 'delayed-dead)
                      (kill-buffer source-buffer)
                      (funcall continuation '((1 . session))))))))
            (let* ((row (car (sqlite-select
                              jabber-db--connection
                              "SELECT stanza_id, thread_id FROM message \
WHERE body = 'reply'")))
                   (node (and (buffer-live-p thread)
                              (with-current-buffer thread
                                (ewoc-nth jabber-chat-ewoc -1)))))
              (list
               :sent sent
               :active
               (gethash 'fake-jc jabber-omemo--pending-send-operations)
               :stored-thread (cadr row)
               :parent-empty
               (or (not (buffer-live-p parent))
                   (not (with-current-buffer parent
                          (ewoc-nth jabber-chat-ewoc 0))))
               :status (and node (plist-get (cadr (ewoc-data node)) :status))
               :live-thread
               (and node (plist-get (cadr (ewoc-data node)) :thread-id))
               :restored
               (when-let* ((source
                            (and (buffer-live-p
                                  (if (eq source-kind 'parent)
                                      parent thread))
                                 (if (eq source-kind 'parent)
                                     parent thread))))
                 (with-current-buffer source (buffer-string))))))
        (when (buffer-live-p parent) (kill-buffer parent))
        (when (buffer-live-p thread) (kill-buffer thread))))))

(ert-deftest jabber-test-omemo-message-thread-send-pending-lifecycle ()
  "Pending thread ownership survives success and failure from both views."
  (dolist (source '(parent thread))
    (dolist (outcome '(success failure))
      (let ((result
             (jabber-test-omemo-message--thread-send-result source outcome)))
        (should (equal "thread-1" (plist-get result :stored-thread)))
        (should (plist-get result :parent-empty))
        (should (equal "thread-1" (plist-get result :live-thread)))
        (should (eq (if (eq outcome 'success) :sent :undelivered)
                    (plist-get result :status)))
        (should (eq (eq outcome 'success) (plist-get result :sent)))
        (should-not (plist-get result :active))
        (should (eq (eq outcome 'failure)
                    (string-suffix-p "reply"
                                     (plist-get result :restored))))))))

(ert-deftest jabber-test-omemo-message-concurrent-sends-keep-thread-owner ()
  "Reverse OMEMO completion cannot move reply and thread metadata."
  (jabber-test-omemo-message-with-db
    (with-temp-buffer
      (setq-local jabber-buffer-connection 'fake-jc)
      (setq-local jabber-chatting-with "friend@example.com")
      (setq-local jabber-chat-encryption 'omemo)
      (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
      (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
      (setq-local jabber-chat-send-hooks
                  '(jabber-message-reply--send-hook
                    jabber-db--outgoing-handler))
      (jabber-db-store-message
       "me@example.com" "friend@example.com" "in" "chat" "root" 1
       "phone" "root-1" nil nil nil nil nil
       '(:thread-id "thread-1"))
      (let ((jabber-omemo--pending-send-operations
             (make-hash-table :test #'eq))
            callbacks sent
            (ticks 10))
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) (cl-incf ticks)))
                  ((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_) "me@example.com"))
                  ((symbol-function 'jabber-omemo--ensure-sessions)
                   (lambda (_jc jid callback)
                     (if (equal jid "friend@example.com")
                         (push callback callbacks)
                       (funcall callback '((2 . own-session))))))
                  ((symbol-function 'jabber-omemo-encrypt-message)
                   (lambda (_) '(:iv "iv" :key "key" :payload "payload")))
                  ((symbol-function 'jabber-omemo--build-encrypted-xml)
                   (lambda (&rest _) '(encrypted ())))
                  ((symbol-function 'jabber-chat--display-local-message)
                   #'ignore)
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc stanza &optional success _failure)
                     (push stanza sent)
                     (when success (funcall success)))))
          (setq-local jabber-message-reply--id "root-1")
          (setq-local jabber-message-reply--jid "friend@example.com")
          (setq-local jabber-message-reply--thread
                      '(:thread-id "thread-1"))
          (jabber-omemo--send-chat 'fake-jc "first")
          (jabber-omemo--send-chat 'fake-jc "second")
          (funcall (car callbacks) '((1 . peer-session)))
          (funcall (cadr callbacks) '((1 . peer-session)))
          (let ((first (car sent))
                (second (cadr sent)))
            (should (= 1 (length (jabber-xml-get-children first 'thread))))
            (should (equal "thread-1"
                           (car (jabber-xml-node-children
                                 (car (jabber-xml-get-children first 'thread))))))
            (should (jabber-xml-child-with-xmlns first "urn:xmpp:reply:0"))
            (should-not (jabber-xml-get-children second 'thread))
            (should-not
             (jabber-xml-child-with-xmlns second "urn:xmpp:reply:0")))
          (should
           (equal '(("first" "thread-1") ("second" nil))
                  (sqlite-select
                   jabber-db--connection
                   "SELECT body, thread_id FROM message \
WHERE body IN ('first', 'second') ORDER BY body"))))))))

(ert-deftest jabber-test-omemo-message-dead-thread-source-cancels-send ()
  "A delayed OMEMO thread send stops when its source buffer dies."
  (dolist (source '(parent thread))
    (let ((result
           (jabber-test-omemo-message--thread-send-result
            source 'delayed-dead)))
      (should-not (plist-get result :sent))
      (should-not (plist-get result :active))
      (should (equal "thread-1" (plist-get result :stored-thread)))
      (should (plist-get result :parent-empty))
      (when (eq source 'parent)
        (should (eq :undelivered (plist-get result :status)))))))

(ert-deftest jabber-test-omemo-message-muc-stalled-bundle-fails-on-reset ()
  "A reset fails a MUC send stalled while own sessions are fetched."
  (let ((jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (jabber-group "room@conf.example.com")
        (jabber-muc-participants nil)
        (successes 0)
        (failures 0)
        (encrypted 0)
        continuation)
    (cl-letf (((symbol-function 'jabber-omemo--muc-participant-jids)
               (lambda (&rest _) '("alice@example.com")))
              ((symbol-function 'jabber-omemo--ensure-sessions-multi)
               (lambda (_jc _jids callback)
                 (funcall callback '((1 . participant-session)))))
              ((symbol-function 'jabber-omemo--ensure-sessions)
               (lambda (_jc _jid callback)
                 (setq continuation callback)))
              ((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo--send-encrypted-muc)
               (lambda (&rest _) (cl-incf encrypted)))
              ((symbol-function 'jabber-omemo--send-failed)
               (lambda (&rest _) nil)))
      (jabber-omemo--send-muc
       'fake-jc "corrected"
       '((replace ((xmlns . "urn:xmpp:message-correct:0")
                   (id . "old"))))
       (lambda () (cl-incf successes))
       (lambda (_reason) (cl-incf failures)))
      (jabber-omemo--session-reset 'fake-jc)
      (funcall continuation '((2 . own-session)))
      (should (= 0 successes))
      (should (= 1 failures))
      (should (= 0 encrypted))
      (should-not
       (gethash 'fake-jc jabber-omemo--pending-send-operations)))))

(ert-deftest jabber-test-omemo-message-ordinary-muc-stall-fails-on-reset ()
  "A reset fails an ordinary MUC send and makes its late callback inert."
  (let ((jabber-omemo--pending-send-operations
         (make-hash-table :test #'eq))
        (jabber-group "room@conf.example.com")
        (jabber-muc-participants nil)
        (failures 0)
        (encrypted 0)
        continuation)
    (cl-letf (((symbol-function 'jabber-omemo--muc-participant-jids)
               (lambda (&rest _) '("alice@example.com")))
              ((symbol-function 'jabber-omemo--ensure-sessions-multi)
               (lambda (_jc _jids callback)
                 (setq continuation callback)))
              ((symbol-function 'jabber-omemo--send-encrypted-muc)
               (lambda (&rest _) (cl-incf encrypted)))
              ((symbol-function 'jabber-omemo--send-failed)
               (lambda (&rest _) (cl-incf failures))))
      (jabber-omemo--send-muc 'fake-jc "hello")
      (jabber-omemo--session-reset 'fake-jc)
      (funcall continuation '((1 . session)))
      (should (= 1 failures))
      (should (= 0 encrypted))
      (should-not
       (gethash 'fake-jc jabber-omemo--pending-send-operations)))))

(defmacro jabber-test-omemo-message--with-muc-send-stubs (sent-var &rest body)
  "Run BODY with the MUC encrypt/send path stubbed.
SENT-VAR is bound to the stanza passed to `jabber-send-sexp'."
  (declare (indent 1) (debug t))
  `(let ((,sent-var nil))
     (cl-letf (((symbol-function 'jabber-omemo-encrypt-message)
                (lambda (_plaintext) '(:iv "iv" :key "key" :payload "payload")))
               ((symbol-function 'jabber-omemo--build-encrypted-xml)
                (lambda (_jc _sessions _enc)
                  '(encrypted ((xmlns . "eu.siacs.conversations.axolotl")))))
               ((symbol-function 'jabber-send-sexp)
                (lambda (_jc stanza) (setq ,sent-var stanza))))
       ,@body)))

(ert-deftest jabber-test-omemo-message-muc-send-hooks-run-in-buffer ()
  "MUC send hooks run in the originating buffer, not the IQ callback's."
  (let* ((muc-buffer (generate-new-buffer "*test-omemo-muc*"))
         (hook-buffer nil)
         (jabber-chat-send-hooks
          (list (lambda (_body _id)
                  (setq hook-buffer (current-buffer))
                  '((probe ((xmlns . "test:probe"))))))))
    (unwind-protect
        (jabber-test-omemo-message--with-muc-send-stubs sent
          (with-temp-buffer
            (jabber-omemo--send-encrypted-muc
             'fake-jc "hello" "room@conf.example.com" nil muc-buffer))
          (should (eq hook-buffer muc-buffer))
          (should sent)
          (should (jabber-xml-get-children sent 'probe)))
      (kill-buffer muc-buffer))))

(ert-deftest jabber-test-omemo-message-muc-send-dead-buffer-still-sends ()
  "A dead originating buffer skips send hooks but the stanza still goes out."
  (let* ((muc-buffer (generate-new-buffer "*test-omemo-muc*"))
         (jabber-chat-send-hooks
          (list (lambda (_body _id) '((probe ((xmlns . "test:probe"))))))))
    (kill-buffer muc-buffer)
    (jabber-test-omemo-message--with-muc-send-stubs sent
      (jabber-omemo--send-encrypted-muc
       'fake-jc "hello" "room@conf.example.com" nil muc-buffer)
      (should sent)
      (should-not (jabber-xml-get-children sent 'probe)))))

(ert-deftest jabber-test-omemo-message-correction-handoff-skips-new-echo ()
  "A successful correction runs its callback without inserting a new node."
  (let ((entered nil)
        (successes 0)
        (jabber-chat-send-hooks nil))
    (cl-letf (((symbol-function 'jabber-omemo-encrypt-message)
               (lambda (_plaintext)
                 '(:iv "iv" :key "key" :payload "payload")))
              ((symbol-function 'jabber-omemo--build-encrypted-xml)
               (lambda (&rest _)
                 '(encrypted
                   ((xmlns . "eu.siacs.conversations.axolotl")))))
              ((symbol-function 'jabber-chat-ewoc-enter)
               (lambda (&rest _) (setq entered t)))
              ((symbol-function 'jabber-send-sexp)
               (lambda (_jc _stanza success _failure)
                 (funcall success))))
      (jabber-omemo--send-encrypted
       'fake-jc "corrected" "friend@example.com" nil
       (current-buffer) nil "correction-1"
       '((replace ((xmlns . "urn:xmpp:message-correct:0")
                   (id . "original-1"))))
       (lambda () (cl-incf successes))
       #'ignore))
    (should (= 1 successes))
    (should-not entered)))

;;; Group 12: Signed pre-key rotation

(defmacro jabber-test-omemo-message--with-rotation-stubs (rotated-var &rest body)
  "Run BODY with rotation collaborators stubbed.
ROTATED-VAR is bound to non-nil when a rotation was performed."
  (declare (indent 1) (debug t))
  `(jabber-test-omemo-message-with-db
     (let ((,rotated-var nil))
       (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                  (lambda (_jc) "me@example.com"))
                 ((symbol-function 'jabber-omemo--get-store)
                  (lambda (_jc) 'fake-store))
                 ((symbol-function 'jabber-omemo-rotate-signed-pre-key)
                  (lambda (_store) (setq ,rotated-var t)))
                 ((symbol-function 'jabber-omemo--persist-store)
                  (lambda (_jc) nil)))
         (jabber-omemo-store-save "me@example.com" (unibyte-string 1))
         ,@body))))

(ert-deftest jabber-test-omemo-message-spk-rotation-records-baseline ()
  "First rotation check records a timestamp without rotating."
  (jabber-test-omemo-message--with-rotation-stubs rotated
    (jabber-omemo--maybe-rotate-signed-pre-key 'fake-jc)
    (should-not rotated)
    (should (jabber-omemo-store-spk-rotated-at "me@example.com"))))

(ert-deftest jabber-test-omemo-message-spk-rotation-skips-when-fresh ()
  "A recent rotation timestamp is left alone."
  (jabber-test-omemo-message--with-rotation-stubs rotated
    (let ((now (time-convert nil 'integer)))
      (jabber-omemo-store-set-spk-rotated-at "me@example.com" now)
      (jabber-omemo--maybe-rotate-signed-pre-key 'fake-jc)
      (should-not rotated)
      (should (= now (jabber-omemo-store-spk-rotated-at "me@example.com"))))))

(ert-deftest jabber-test-omemo-message-spk-rotation-rotates-when-due ()
  "A timestamp older than the rotation period triggers a rotation."
  (jabber-test-omemo-message--with-rotation-stubs rotated
    (let* ((now (time-convert nil 'integer))
           (stale (- now jabber-omemo-signed-pre-key-rotation-period 10)))
      (jabber-omemo-store-set-spk-rotated-at "me@example.com" stale)
      (jabber-omemo--maybe-rotate-signed-pre-key 'fake-jc)
      (should rotated)
      (should (> (jabber-omemo-store-spk-rotated-at "me@example.com") stale)))))

(provide 'jabber-test-omemo-message)
;;; jabber-test-omemo-message.el ends here
