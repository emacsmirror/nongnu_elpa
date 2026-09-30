;;; jabber-test-omemo-message.el --- Tests for jabber-omemo-message  -*- lexical-binding: t; -*-

;;; Commentary:

;; OMEMO message encryption and decryption.

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-message-reply)
(require 'jabber-omemo)
(require 'jabber-receipts)

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
                        jc (list (list peer-did account peer session-ptr)) enc-result)))
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

(ert-deftest jabber-test-omemo-message-httpupload-preserves-literal-bytes ()
  "Upload ciphertext must decrypt to the literal source file bytes."
  (dolist (suffix '(".bin" ".targz" ".gz" ".tgz"))
    (let* ((source (make-temp-file "jabber-upload-bytes-" nil suffix))
           (jabber-chat-encryption 'omemo)
           (payload (unibyte-string 0 1 10 13 127 128 254 255))
           result)
      (unwind-protect
          (progn
            ;; For compressed suffixes, create a real compressed input file.
            (with-temp-file source
              (set-buffer-multibyte nil)
              (insert payload))
            (let ((original (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally source)
                              (buffer-string))))
              (setq result
                    (jabber-omemo--httpupload-transform source #'identity))
              (let* ((ciphertext (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally (car result))
                                   (buffer-string)))
                     (url (funcall (cdr result)
                                   (concat "https://example.org/file" suffix)))
                     (parsed (jabber-chat--parse-aesgcm-url url)))
                (should-not (file-exists-p (car result)))
                (should (equal original
                               (jabber-omemo-aesgcm-decrypt
                                (plist-get parsed :key)
                                (plist-get parsed :iv) ciphertext)))
                (should (= (length ciphertext) (+ 16 (length original)))))))
        (delete-file source)
        (when (and result (file-exists-p (car result)))
          (delete-file (car result)))))))

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
  (let ((sessions '((100 "me@example.com" "peer100@example.com" fake-ptr-100)
                    (200 "me@example.com" "peer200@example.com" fake-ptr-200)
                    (300 "me@example.com" "peer300@example.com" fake-ptr-300))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
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
  (let ((sessions '((100 "me@example.com" "peer@example.com" fake-ptr-100))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo-store-load-trust)
               (lambda (_account _jid _did)
                 (list :identity-key "k" :trust 0 :first-seen 0))))
      (let ((result (jabber-omemo--trusted-sessions 'fake-jc sessions)))
        (should (= 1 (length result)))))))

(ert-deftest jabber-test-omemo-message-trusted-sessions-keeps-no-trust-record ()
  "trusted-sessions keeps devices with no trust record."
  (let ((sessions '((100 "me@example.com" "peer@example.com" fake-ptr-100))))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-omemo-store-load-trust)
               (lambda (_account _jid _did) nil)))
      (let ((result (jabber-omemo--trusted-sessions 'fake-jc sessions)))
        (should (= 1 (length result)))))))

(ert-deftest jabber-test-omemo-message-build-encrypted-rejects-all-untrusted ()
  "build-encrypted-xml signals error when all devices are untrusted."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_jc) "me@example.com"))
            ((symbol-function 'jabber-omemo-store-load-trust)
             (lambda (_account _jid _did)
               (list :identity-key "k" :trust -1 :first-seen 0))))
    (should-error
     (jabber-omemo--build-encrypted-xml
      'fake-jc '((100 "me@example.com" "peer@example.com" fake-ptr)) '(:key "k" :iv "i" :ciphertext "c"))
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

(ert-deftest jabber-test-omemo-message-pending-clears-receipt-header ()
  "A pending OMEMO message immediately owns the receipt header."
  (with-temp-buffer
    (setq-local jabber-buffer-connection 'fake-jc)
    (setq-local jabber-chatting-with "friend@example.com")
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (setq-local jabber-chat-receipt-message " seen 10:00")
    (let ((jabber-chat-printers (list (lambda (&rest _) t)))
          (jabber-omemo--pending-send-operations
           (make-hash-table :test #'eq))
          continuation)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_) "me@example.com"))
                ((symbol-function 'jabber-db--outgoing-handler) #'ignore)
                ((symbol-function 'jabber-omemo--ensure-sessions)
                 (lambda (_jc _jid callback)
                   (setq continuation callback))))
        (jabber-omemo--send-chat 'fake-jc "hello")
        (let ((node (ewoc-nth jabber-chat-ewoc -1)))
          (should (eq :sending
                      (plist-get (cadr (ewoc-data node)) :status)))
          (should (equal "" jabber-chat-receipt-message))
          (funcall continuation nil)
          (should (eq :undelivered
                      (plist-get (cadr (ewoc-data node)) :status)))
          (should (equal "" jabber-chat-receipt-message)))))))

(ert-deftest jabber-test-omemo-message-reused-pending-clears-receipt-header ()
  "Reusing a pending OMEMO node still clears its stale receipt header."
  (with-temp-buffer
    (setq-local jabber-chat-ewoc (ewoc-create #'ignore))
    (setq-local jabber-chat--msg-nodes (make-hash-table :test #'equal))
    (setq-local jabber-chat-receipt-message " seen 10:00")
    (setq-local header-line-format '((:eval jabber-chat-receipt-message)))
    (let ((jabber-chat-printers (list (lambda (&rest _) t))))
      (cl-letf (((symbol-function 'jabber-db--outgoing-handler) #'ignore))
        (let* ((existing
                (jabber-chat-ewoc-enter
                 (list :local
                       (list :id "pending-1" :body "old"
                             :status :sent :timestamp (current-time)))))
               (pending
                (jabber-omemo--display-pending
                 (current-buffer) "retry" "pending-1")))
          (should (eq existing (plist-get pending :node)))
          (should (equal "" jabber-chat-receipt-message))
          (should (equal "" (format-mode-line header-line-format))))))))

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
            (should (= 1 (length (jabber-xml-get-children second 'thread))))
            (should-not
             (equal "thread-1"
                    (car (jabber-xml-node-children
                          (car (jabber-xml-get-children second 'thread))))))
            (should-not
             (jabber-xml-child-with-xmlns second "urn:xmpp:reply:0"))
            (should
             (equal
              `(("first" "thread-1")
                ("second"
                 ,(car (jabber-xml-node-children
                        (car (jabber-xml-get-children second 'thread))))))
              (sqlite-select
               jabber-db--connection
               "SELECT body, thread_id FROM message \
WHERE body IN ('first', 'second') ORDER BY body")))))))))

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

;;; Group 15: Heartbeat persist-before-send

(ert-deftest jabber-test-omemo-message-decrypt-stanza-persists-heartbeat ()
  "decrypt-stanza persists the post-heartbeat session before sending it.
After nr reaches 53, reloading the stored blob and calling
`jabber-omemo-heartbeat' again must return nil.  The in-memory
session and the database blob must serialize identically."
  (jabber-test-omemo-message-with-db
    (let* ((alice-store (jabber-omemo-deserialize-store
                         (jabber-omemo-setup-store)))
           (bob-store (jabber-omemo-deserialize-store
                       (jabber-omemo-setup-store)))
           (account "bob@example.com")
           (peer "alice@example.com")
           (our-did 42)
           (peer-did 99)
           (sent 0)
           (bundle (jabber-omemo-get-bundle bob-store))
           (pk (car (plist-get bundle :pre-keys)))
           (alice-session
            (jabber-omemo-initiate-session
             alice-store
             (plist-get bundle :signature)
             (plist-get bundle :signed-pre-key)
             (plist-get bundle :identity-key)
             (cdr pk)
             (plist-get bundle :signed-pre-key-id)
             (car pk))))
      (puthash account bob-store jabber-omemo--stores)
      (puthash account our-did jabber-omemo--device-ids)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) account))
                ((symbol-function 'jabber-omemo--note-consumed-prekey)
                 #'ignore)
                ((symbol-function 'jabber-omemo--send-heartbeat)
                 (lambda (&rest _) (cl-incf sent))))
        (dotimes (i 53)
          (let* ((enc (jabber-omemo-encrypt-message
                       (encode-coding-string (format "m%d" i) 'utf-8)))
                 (encrypted-key (jabber-omemo-encrypt-key
                                 alice-session (plist-get enc :key)))
                 (xml-data `(message ((from . ,(concat peer "/phone"))
                                      (type . "chat"))))
                 (parsed (list :sid peer-did
                               :iv (plist-get enc :iv)
                               :payload (plist-get enc :ciphertext)
                               :keys (list (cons our-did
                                                 (list :data (plist-get
                                                              encrypted-key
                                                              :data)
                                                       :pre-key-p
                                                       (plist-get
                                                        encrypted-key
                                                        :pre-key-p)))))))
            (jabber-omemo--decrypt-stanza 'fake-jc xml-data parsed))))
      (should (= 1 sent))
      (let* ((live (gethash (jabber-omemo--session-key account peer peer-did)
                            jabber-omemo--sessions))
             (blob (jabber-omemo-store-load-session account peer peer-did))
             (reloaded (jabber-omemo-deserialize-session blob)))
        (should (equal (jabber-omemo-serialize-session live) blob))
        (should-not (jabber-omemo-heartbeat reloaded bob-store))))))

(ert-deftest jabber-test-omemo-message-fresh-recovery-owns-ciphertext ()
  "Encrypted sends survive reset and two bind losses without reencryption."
  (dolist (source '(:sm-resume :bind))
    (dolist (later '(nil ack plain))
      (dolist (terminal '(nil t))
	(let* ((jc (make-symbol "encrypted-recovery"))
               (jabber-connections (list jc))
               (jabber-auto-reconnect t)
               (jabber-sm-max-in-flight 1)
               (jabber-lost-connection-hooks nil)
               (jabber-lifecycle-connection-list-changed-functions nil)
               (jabber-lifecycle-session-bootstrap-functions nil)
               (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
               (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
               (jabber-chatting-with "peer@example.org")
               (state (jabber-sm--reset nil))
               (successes 0) (failures 0) (abandoned 0) (encrypted 0) (resets 0)
               (jabber-lifecycle-session-reset-functions
		(list (lambda (connection)
			(cl-incf resets)
			(jabber-omemo--session-reset connection))))
               cipher sent)
          (dolist (pair `((:connection . transport) (:ever-session-established . t)
                          (:sm-enabled . t) (:sm-id . "old")
                          (:sm-outbound-count . 1) (:sm-resuming . t)
                          (:stream-features . (features ()
							(bind ((xmlns . ,jabber-bind-xmlns)))))))
            (setq state (plist-put state (car pair) (cdr pair))))
          (put jc :name 'jabber-connection)
          (put jc :state source)
          (put jc :state-data state)
          (unwind-protect
              (cl-letf (((symbol-function 'jabber-connection-bare-jid)
			 (lambda (_jc) "self@example.org"))
			((symbol-function 'jabber-omemo--ensure-sessions)
			 (lambda (_jc _jid callback) (funcall callback '((1 . session)))))
			((symbol-function 'jabber-omemo-encrypt-message)
			 (lambda (_body) (cl-incf encrypted) 'ciphertext))
			((symbol-function 'jabber-omemo--build-encrypted-xml)
			 (lambda (&rest _) '(encrypted ((xmlns . "eu.siacs.conversations.axolotl")))))
			((symbol-function 'jabber-chat--run-send-hooks) #'ignore)
			((symbol-function 'jabber-omemo--send-failed) #'ignore)
			((symbol-function 'jabber-send-sexp--raw)
			 (lambda (_jc stanza) (push stanza sent)))
			((symbol-function 'jabber--send-bind-request) #'ignore)
			((symbol-function 'jabber-send-stream-header) #'ignore)
			((symbol-function 'jabber-send-string) #'ignore)
			((symbol-function 'jabber-sm--schedule-drain) #'ignore))
		(jabber-omemo--send-chat
		 jc "synthetic" '((replace ((id . "old"))))
		 (lambda () (cl-incf successes))
		 (lambda (_reason) (cl-incf failures)))
		(setq cipher (jabber-sm--pending-stanza
                              (car (plist-get state :sm-pending-queue))))
		(should cipher)
		(jabber-omemo--send-operation-register
		 jc nil (lambda (_reason) (cl-incf abandoned)))
		(fsm-send-sync
		 jc (list :stanza
                          (if (eq source :bind) (plist-get state :stream-features)
                            '(failed ((xmlns . "urn:xmpp:sm:3"))))))
		(should (= failures 0))
		(should (= abandoned 1))
		(dotimes (_ 2)
                  (fsm-send-sync jc '(:bind-failure (iq ((type . "error"))
							(error ((type . "cancel"))
							       (not-allowed ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))
                  (should-not (get jc :state))
                  (should (= failures 0))
                  (should (plist-get (fsm-get-state-data jc) :sm-pending-queue))
                  (fsm-stop-timer jc)
                  ;; Model the next authenticated transport; binding remains real FSM.
                  (put jc :state :bind)
                  (plist-put (fsm-get-state-data jc) :connection 'next-transport))
		(should (= resets 1))
		(when later
                  (when (eq later 'ack)
                    (plist-put (fsm-get-state-data jc) :stream-features
                               `(features () (sm ((xmlns . ,jabber-sm-xmlns))))))
                  (fsm-send-sync jc '(:bind-success (iq () (bind () (jid () "self@example.org/test")))))
                  (when (eq later 'ack)
                    (cl-letf (((symbol-function 'jabber-send-string) #'ignore))
                      (fsm-send-sync jc '(:stanza (enabled ((xmlns . "urn:xmpp:sm:3")))))))
                  (fsm-send-sync jc '(:connection-dead next-transport "synthetic later loss"))
                  (should (= resets 2))
                  (should (= failures 0))
                  (should (= abandoned 1))
                  (fsm-stop-timer jc)
                  (put jc :state :bind)
                  (plist-put (fsm-get-state-data jc) :connection 'final-transport)
                  (plist-put (fsm-get-state-data jc) :stream-features nil))
		(if terminal
                    (progn
                      (jabber-disconnect-one jc)
                      (jabber-disconnect-one jc)
                      (should (= failures 1))
                      (should (= successes 0))
                      (should-not sent))
                  (fsm-send-sync jc '(:bind-success (iq () (bind () (jid () "self@example.org/test")))))
                  (let ((jabber-sm-max-in-flight nil))
                    (jabber-sm--drain-pending jc (fsm-get-state-data jc)))
                  (should (equal sent (list cipher)))
                  (should (= successes 1))
                  (should (= failures 0)))
		(should (= encrypted 1)))
            (fsm-stop-timer jc)))))))

(ert-deftest jabber-test-omemo-message-fresh-room-echo-ownership ()
  "Retain exact echoes for queued and already handed-off room ciphertext."
  (dolist (later '(nil ack plain))
    (dolist (queued '(nil t))
      (let* ((jc (make-symbol "room-owner"))
             (other (make-symbol "other"))
             (jabber-connections (list jc))
             (jabber-auto-reconnect t)
             (jabber-sm-max-in-flight (and queued 0))
             (jabber-group "room@example.org")
             (jabber-muc--rooms-before-disconnect (make-hash-table :test #'equal))
             (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
             (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
             (jabber-lifecycle-session-reset-functions '(jabber-omemo--session-reset))
             (jabber-lost-connection-hooks nil)
             (jabber-lifecycle-session-bootstrap-functions nil)
             (jabber-lifecycle-connection-list-changed-functions nil)
             (state (jabber-sm--reset nil))
             (successes 0) (failures 0) (encrypted 0)
             key stanza)
	(setq state (plist-put state :sm-enabled t))
	(setq state (plist-put state :connection 'transport))
	(setq state (plist-put state :ever-session-established t))
	(put jc :name 'jabber-connection)
	(put jc :state :session-established)
	(put jc :state-data state)
	(cl-letf (((symbol-function 'jabber-connection-bare-jid)
                   (lambda (_jc) "self@example.org"))
                  ((symbol-function 'jabber-muc-nickname) (lambda (&rest _) "self"))
                  ((symbol-function 'jabber-omemo--muc-participant-jids)
                   (lambda (&rest _) '("peer@example.org")))
                  ((symbol-function 'jabber-omemo--ensure-sessions-multi)
                   (lambda (_jc _jids callback) (funcall callback '((1 . session)))))
                  ((symbol-function 'jabber-omemo--ensure-sessions)
                   (lambda (_jc _jid callback) (funcall callback nil)))
                  ((symbol-function 'jabber-omemo-encrypt-message)
                   (lambda (_body) (cl-incf encrypted) 'ciphertext))
                  ((symbol-function 'jabber-omemo--build-encrypted-xml)
                   (lambda (&rest _) '(encrypted ((xmlns . "eu.siacs.conversations.axolotl")))))
                  ((symbol-function 'jabber-chat--run-send-hooks) #'ignore)
                  ((symbol-function 'jabber-omemo--send-failed) #'ignore)
                  ((symbol-function 'jabber-send-sexp--raw) #'ignore))
          (jabber-omemo--send-muc jc "synthetic" nil
				  (lambda () (cl-incf successes))
				  (lambda (_reason) (cl-incf failures)))
          (setq key (car (hash-table-keys jabber-omemo--sent-muc-plaintexts)))
          (setq stanza (if queued
                           (jabber-sm--pending-stanza
                            (car (plist-get state :sm-pending-queue)))
			 (cdar (plist-get state :sm-outbound-queue))))
          (should stanza)
          (dolist (foreign (list (list other jabber-group (nth 2 key) (nth 3 key))
				 (list jc jabber-group "room@example.org/foreign" (nth 3 key))
				 (list jc "elsewhere@example.org" (nth 2 key) (nth 3 key))
				 (list jc jabber-group (nth 2 key) "unretained")))
            (puthash foreign "synthetic" jabber-omemo--sent-muc-plaintexts))
          (put jc :state-data (jabber-sm--handle-failed-resume state '(failed ())))
          ;; MUC reset may run before OMEMO; use its existing exact-account snapshot.
          (puthash "self@example.org" (list (list jabber-group "self" nil))
                   jabber-muc--rooms-before-disconnect)
          (cl-letf (((symbol-function 'jabber-muc-nickname) (lambda (&rest _) nil)))
            (jabber-omemo--session-reset jc))
          (when later
            (put jc :state :bind)
            (plist-put (fsm-get-state-data jc) :stream-features
                       (when (eq later 'ack)
			 `(features () (sm ((xmlns . ,jabber-sm-xmlns))))))
            (cl-letf (((symbol-function 'jabber-send-string) #'ignore)
                      ((symbol-function 'jabber-sm--schedule-drain) #'ignore))
              (fsm-send-sync jc '(:bind-success (iq () (bind () (jid () "self@example.org/test")))))
              (when (eq later 'ack)
		(fsm-send-sync jc '(:stanza (enabled ((xmlns . "urn:xmpp:sm:3"))))))
              (fsm-send-sync jc '(:connection-dead transport "synthetic later loss")))
            (fsm-stop-timer jc))
          (should (equal (gethash key jabber-omemo--sent-muc-plaintexts) "synthetic"))
          (should (= (hash-table-count jabber-omemo--sent-muc-plaintexts) 2))
          (should (= failures 0))
          (should (= successes (if queued 0 1)))
          (let ((jabber-sm-max-in-flight nil))
            (jabber-sm--drain-pending jc (fsm-get-state-data jc)))
          (should (plist-get (fsm-get-state-data jc) :sm-pending-queue))
          (should (= encrypted 1))
          (plist-put (fsm-get-state-data jc) :session-reset-done t)
          (jabber-disconnect-one jc)
          (jabber-disconnect-one jc)
          (should-not (gethash key jabber-omemo--sent-muc-plaintexts))
          (should (= (hash-table-count jabber-omemo--sent-muc-plaintexts) 1))
          (should (= failures (if queued 1 0))))))))

(ert-deftest jabber-test-omemo-message-fresh-recovery-later-session-loss ()
  "A later nonresumable loss reconverts owned work before reset effects."
  (dolist (sm '(t nil))
    (let* ((jc (make-symbol "later-loss"))
           (jabber-connections (list jc))
           (jabber-auto-reconnect t)
           (jabber-sm-max-in-flight 1)
           (jabber-lost-connection-hooks nil)
           (jabber-lifecycle-session-bootstrap-functions nil)
           (jabber-lifecycle-connection-list-changed-functions nil)
           (jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
           (jabber-omemo--sent-muc-plaintexts (make-hash-table :test #'equal))
           (resets 0) (failed 0) sent
           (jabber-lifecycle-session-reset-functions
            (list (lambda (c)
                    (cl-incf resets)
                    (jabber-omemo--session-reset c))))
           (direct '(message ((id . "direct-old"))))
           (room '(message ((type . "groupchat") (to . "room@example.org")
                            (id . "room-new"))))
           (state (jabber-sm--reset nil)))
      (setq state (plist-put state :connection 'transport))
      (setq state (plist-put state :ever-session-established t))
      (setq state (plist-put state :stream-features
                            `(features () (bind ((xmlns . ,jabber-bind-xmlns)))
                                       ,@(when sm `((sm ((xmlns . ,jabber-sm-xmlns))))))))
      (setq state (jabber-sm--enqueue-pending state direct))
      (put jc :name 'jabber-connection)
      (put jc :state :sm-resume)
      (put jc :state-data state)
      (unwind-protect
          (cl-letf (((symbol-function 'jabber--send-bind-request) #'ignore)
                    ((symbol-function 'jabber-send-string) #'ignore)
                    ((symbol-function 'jabber-sm--schedule-drain) #'ignore)
                    ((symbol-function 'jabber-send-sexp--raw)
                     (lambda (_jc stanza) (push stanza sent))))
            (fsm-send-sync jc '(:stanza (failed ((xmlns . "urn:xmpp:sm:3")))))
            (fsm-send-sync jc '(:bind-success (iq () (bind () (jid () "self@example.org/test")))))
            (when sm
              (fsm-send-sync jc '(:stanza (enabled ((xmlns . "urn:xmpp:sm:3"))))))
            (should (eq (get jc :state) :session-established))
            (jabber-sm--drain-pending jc (fsm-get-state-data jc))
            (should (equal sent (list direct)))
            (jabber-omemo--send-operation-register jc nil (lambda (_) (cl-incf failed)))
            (if sm
                (jabber-send-sexp jc room)
              ;; No SM backpressure: model work already accepted but not drained.
              (put jc :state-data
                   (jabber-sm--enqueue-pending (fsm-get-state-data jc) room)))
            (fsm-send-sync jc '(:connection-dead transport "synthetic loss"))
            (should (= resets 2))
            (should (= failed 1))
            (should (= (plist-get (fsm-get-state-data jc) :sm-outbound-count) 0))
            (should-not (plist-get (fsm-get-state-data jc) :sm-outbound-queue))
            (should (equal (mapcar #'jabber-sm--pending-stanza
                                  (plist-get (fsm-get-state-data jc) :sm-pending-queue))
                           (if sm (list direct room) (list room))))
            (dotimes (_ 2)
              (fsm-stop-timer jc)
              (put jc :state :bind)
              (plist-put (fsm-get-state-data jc) :connection 'next-transport)
              (fsm-send-sync jc '(:bind-failure (iq ((type . "error"))
                    (error ((type . "cancel"))
                     (not-allowed ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))
              (should (= resets 2))
              (should (= failed 1))
              (should (plist-get (fsm-get-state-data jc) :sm-pending-queue)))
            (fsm-stop-timer jc)
            (put jc :state :bind)
            (plist-put (fsm-get-state-data jc) :connection 'next-transport)
            (fsm-send-sync jc '(:bind-success (iq () (bind () (jid () "self@example.org/test")))))
            (when sm
              (fsm-send-sync jc '(:stanza (enabled ((xmlns . "urn:xmpp:sm:3"))))))
            (setq sent nil)
            (jabber-sm--drain-pending jc (fsm-get-state-data jc))
            (should (equal sent (and sm (list direct))))
            (let ((jabber-sm-max-in-flight nil))
              (jabber-sm--drain-pending jc (fsm-get-state-data jc)))
            (should (equal sent (and sm (list direct))))
            (should (equal (mapcar #'jabber-sm--pending-stanza
                                  (plist-get (fsm-get-state-data jc) :sm-pending-queue))
                           (list room))))
        (fsm-stop-timer jc)))))

(ert-deftest jabber-test-omemo-message-upload-encryption-failure-stops-slot ()
  "Selected encryption must fail before slot allocation or plaintext upload."
  (require 'jabber-httpupload)
  (dolist (failure '(read encrypt write))
    (let* ((temporary-file-directory (make-temp-file "jabber-upload-failure-" t))
           (file (make-temp-file "attachment-" nil nil "private bytes"))
           (jabber-chat-encryption 'omemo)
           (jabber-httpupload-support '((fake-jc . "upload.example")))
           (jabber-httpupload-pre-upload-transform #'jabber-omemo--httpupload-transform)
           (native-comp-enable-subr-trampolines nil)
           (read-file (symbol-function 'insert-file-contents-literally))
           (encrypt (symbol-function 'jabber-omemo-aesgcm-encrypt))
           (write-file (symbol-function 'write-region))
           slots uploads)
      (unwind-protect
          (cl-letf (((symbol-function 'insert-file-contents-literally)
                     (lambda (&rest args)
                       (if (eq failure 'read) (error "Read failed")
                         (apply read-file args))))
                    ((symbol-function 'jabber-omemo-aesgcm-encrypt)
                     (lambda (bytes)
                       (if (eq failure 'encrypt) (error "Encryption/RNG failed")
                         (funcall encrypt bytes))))
                    ((symbol-function 'write-region)
                     (lambda (&rest args)
                       (if (eq failure 'write) (error "Write failed")
                         (apply write-file args))))
                    ((symbol-function 'jabber-send-iq)
                     (lambda (&rest _) (push t slots)))
                    ((symbol-function 'jabber-httpupload-put-file-curl)
                     (lambda (&rest _) (push t uploads))))
            (should-error (jabber-httpupload--upload 'fake-jc file #'ignore))
            (should-not slots)
            (should-not uploads)
            (should (equal (directory-files temporary-file-directory nil
                                            directory-files-no-dot-files-regexp)
                           (list (file-name-nondirectory file)))))
        (delete-directory temporary-file-directory t)))))

(ert-deftest jabber-test-omemo-message-colliding-device-trust ()
  "Peer identity must survive session acquisition and trust filtering."
  (jabber-test-omemo-message-with-db
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_) "me@example.com"))
              ((symbol-function 'jabber-omemo--get-device-id) (lambda (_) 99)))
      (dolist (peer '("good@example.com" "bad@example.com"))
        (puthash (jabber-omemo--session-key "me@example.com" peer 7)
                 (intern peer) jabber-omemo--sessions)
        (jabber-omemo-store-save-trust
         "me@example.com" peer 7 "identity"
         (if (equal peer "bad@example.com") -1 2)))
      (let (sessions)
        (jabber-omemo--ensure-sessions-for-ids
         'fake-jc "bad@example.com" '(7) (lambda (value) (setq sessions value)))
        (should sessions)
        (should-not (jabber-omemo--trusted-sessions 'fake-jc sessions))))))

(ert-deftest jabber-test-omemo-message-peer-device-may-match-own-id ()
  "Self exclusion requires the own JID as well as the numeric device ID."
  (cl-letf (((symbol-function 'jabber-connection-bare-jid)
             (lambda (_) "me@example.com"))
            ((symbol-function 'jabber-omemo--get-device-id) (lambda (_) 7))
            ((symbol-function 'jabber-omemo--get-session)
             (lambda (&rest _) 'session)))
    (let (peer own)
      (jabber-omemo--ensure-sessions-for-ids
       'fake-jc "peer@example.com" '(7) (lambda (value) (setq peer value)))
      (jabber-omemo--ensure-sessions-for-ids
       'fake-jc "me@example.com" '(7) (lambda (value) (setq own value)))
      (should peer)
      (should-not own))))

(ert-deftest jabber-test-omemo-message-colliding-multi-peer-native ()
  "Merged native sessions retain peer, account and persistence ownership."
  (jabber-test-omemo-message-with-db
    (let* ((account "me@example.com")
           (peers '("one@example.com" "two@example.com" "me@example.com"))
           (stores (mapcar (lambda (_) (jabber-omemo-deserialize-store
                                       (jabber-omemo-setup-store))) peers))
           sessions)
      (cl-letf (((symbol-function 'jabber-connection-bare-jid) (lambda (_) account))
                ((symbol-function 'jabber-blocking-ready-p) (lambda (&rest _) t))
                ((symbol-function 'jabber-blocking-blocked-p) (lambda (&rest _) nil)))
        (puthash account 99 jabber-omemo--device-ids)
        (cl-mapc
         (lambda (peer store)
           (jabber-omemo--establish-session
            'fake-jc peer 7 (jabber-omemo-get-bundle store))
           (puthash (jabber-omemo--device-list-key account peer) '(7)
                    jabber-omemo--device-lists))
         peers stores)
        (jabber-omemo--ensure-sessions-multi
         'fake-jc peers (lambda (value) (setq sessions value)))
        (should (= 3 (length sessions)))
        (let* ((enc (jabber-omemo-encrypt-message "hello"))
               (xml (jabber-omemo--build-encrypted-xml 'fake-jc sessions enc))
               (header (car (jabber-xml-get-children xml 'header)))
               (keys (jabber-xml-get-children header 'key)))
          (should (= 3 (length keys)))
          (cl-mapc
           (lambda (entry key)
             (let* ((peer (nth 2 entry))
                    (store (nth (cl-position peer peers :test #'equal) stores))
                    (receiver (jabber-omemo-make-session)))
               (should (equal (jabber-omemo-decrypt-key
                               receiver store t
                               (base64-decode-string
                                (car (jabber-xml-node-children key))))
                              (plist-get enc :key)))
               (should (equal (jabber-omemo-serialize-session (nth 3 entry))
                              (jabber-omemo-store-load-session account peer 7)))))
           sessions keys)
          (jabber-omemo-store-set-trust account "one@example.com" 7 -1)
          (let* ((before (jabber-omemo-store-load-session account "one@example.com" 7))
                 (filtered (jabber-omemo--build-encrypted-xml 'fake-jc sessions enc)))
            (should (= 2 (length (jabber-xml-get-children
                                  (car (jabber-xml-get-children filtered 'header)) 'key))))
            (should (equal before (jabber-omemo-store-load-session
                                   account "one@example.com" 7))))
          (should-error
           (jabber-omemo--build-encrypted-xml
            'other-jc (list (list 7 "other@example.com" "one@example.com"
                                 (nth 3 (car sessions)))) enc)
           :type 'user-error)
          ;; A pending send must not restore a device deleted in the meantime.
          (jabber-omemo--delete-session account (nth 2 (car sessions)) 7 t)
          (should-error (jabber-omemo--build-encrypted-xml 'fake-jc sessions enc)
                        :type 'user-error)
          (should-not (jabber-omemo-store-load-session
                       account (nth 2 (car sessions)) 7)))))))

(ert-deftest jabber-test-omemo-message-upload-native-and-plain ()
  "Upload uses ciphertext when selected and plaintext only when intentional."
  (require 'jabber-httpupload)
  (dolist (encryption '(omemo nil))
    (let* ((file (make-temp-file "jabber-upload-native-" nil nil "private bytes"))
           (jabber-chat-encryption encryption)
           (jabber-httpupload-support '((fake-jc . "upload.example")))
           (jabber-httpupload-max-file-size nil)
           (jabber-httpupload-pre-upload-transform #'jabber-omemo--httpupload-transform)
           uploaded-path uploaded-bytes url
           (jabber-httpupload-upload-function
            (lambda (path _headers _url callback arg &optional _ignore)
              (setq uploaded-path path
                    uploaded-bytes (with-temp-buffer
                                     (set-buffer-multibyte nil)
                                     (insert-file-contents-literally path)
                                     (buffer-string)))
              (funcall callback arg)
              t)))
      (unwind-protect
          (cl-letf (((symbol-function 'jabber-send-iq)
                     (lambda (jc _to _type _request success &rest _)
                       (funcall success jc
                                '(iq () (slot ((xmlns . "urn:xmpp:http:upload:0"))
                                          (put ((url . "https://upload.example/put")))
                                          (get ((url . "https://upload.example/get"))))) nil)))
                    ((symbol-function 'jabber-httpupload-ignore-certificate) #'ignore))
            (jabber-httpupload--upload 'fake-jc file (lambda (value) (setq url value)))
            (if encryption
                (progn
                  (should-not (equal uploaded-bytes "private bytes"))
                  (should-not (equal uploaded-path file))
                  (should (string-prefix-p "aesgcm://" url))
                  (should-not (file-exists-p uploaded-path)))
              (should (equal uploaded-path file))
              (should (equal uploaded-bytes "private bytes"))
              (should (equal url "https://upload.example/get")))
            (should (file-exists-p file)))
        (delete-file file)))))

(defun jabber-test-omemo-message--cold-upload (encryption change &optional failure)
  "Exercise cold upload with ENCRYPTION, then CHANGE the source chat.
Inject FAILURE after discovery starts, before the upload service replies."
  (require 'jabber-httpupload)
  (let* ((temporary-file-directory (make-temp-file "jabber-cold-upload-" t))
         (file (make-temp-file "attachment-" nil nil "private bytes"))
         (chat (generate-new-buffer " *jabber-upload-chat*"))
         (dispatch (generate-new-buffer " *jabber-upload-dispatch*"))
         (jc (make-symbol "upload"))
         (jabber-connections (list jc))
         (jabber-httpupload-support nil)
         (jabber-httpupload-max-file-size nil)
         (jabber-disco-info-cache (make-hash-table :test #'equal))
         (jabber-disco-items-cache (make-hash-table :test #'equal))
         (jabber-open-info-queries nil)
         (jabber-httpupload-pre-upload-transform nil)
         (native-comp-enable-subr-trampolines nil)
         (read-file (symbol-function 'insert-file-contents-literally))
         (encrypt (symbol-function 'jabber-omemo-aesgcm-encrypt))
         (write-file (symbol-function 'write-region))
         (transforms 0)
         sent uploaded-path uploaded-bytes url
         (jabber-httpupload-upload-function
          (lambda (path _headers _url callback arg &optional _ignore)
            (setq uploaded-path path
                  uploaded-bytes (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally path)
                                   (buffer-string)))
            (funcall callback arg)
            t)))
    (put jc :state :session-established)
    (put jc :state-data
         (list :server "example.com" :username "upload" :resource "test"
               :connection (make-symbol "transport") :session-id "upload-stream"))
    (unwind-protect
        (cl-letf (((symbol-function 'jabber-caps-get-cached) #'ignore)
                  ((symbol-function 'jabber-httpupload-ignore-certificate) #'ignore)
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc stanza &rest _) (push stanza sent))))
          (with-current-buffer chat
            (setq-local jabber-chat-encryption encryption)
            ;; Retain the existing two-argument hook contract, including a
            ;; buffer-local custom hook rather than only the OMEMO symbol.
            (setq-local jabber-httpupload-pre-upload-transform
                        (lambda (path callback)
                          (cl-incf transforms)
                          (jabber-omemo--httpupload-transform path callback)))
            (jabber-httpupload--upload
             jc file (lambda (value) (setq url value))))
          (should (= (length sent) 1))
          (should (= transforms 0))
          (pcase change
            ('close (kill-buffer chat))
            ('mode (with-current-buffer chat (fundamental-mode)))
            ('toggle (with-current-buffer chat
                       (setq jabber-chat-encryption
                             (if encryption nil 'omemo)))))
          (with-current-buffer dispatch
            ;; A callback buffer can even have the opposite selection.
            (setq-local jabber-chat-encryption (if encryption nil 'omemo))
            (cl-labels
                ((reply (from payload)
                   (jabber-process-iq
                    jc `(iq ((type . "result") (from . ,from)
                              (id . ,(jabber-xml-get-attribute (car sent) 'id)))
                             ,payload))))
              (reply "example.com"
                     '(query ((xmlns . "http://jabber.org/protocol/disco#items"))
                             (item ((jid . "upload.example")))))
              (should (= (length sent) 2))
              (cl-letf (((symbol-function 'insert-file-contents-literally)
                         (lambda (&rest args)
                           (if (eq failure 'read) (error "Read failed")
                             (apply read-file args))))
                        ((symbol-function 'jabber-omemo-aesgcm-encrypt)
                         (lambda (bytes)
                           (if (eq failure 'encrypt) (error "Encryption failed")
                             (funcall encrypt bytes))))
                        ((symbol-function 'write-region)
                         (lambda (&rest args)
                           (if (eq failure 'write) (error "Write failed")
                             (apply write-file args)))))
                (let ((info '(query ((xmlns . "http://jabber.org/protocol/disco#info"))
                                    (feature ((var . "urn:xmpp:http:upload:0"))))))
                  (if failure
                      (should-error (reply "upload.example" info))
                    (reply "upload.example" info))))
              (should (= transforms 1))
              (if failure
                  (progn
                    (should (= (length sent) 2))
                    (should-not uploaded-path)
                    (should-not url))
                (should (= (length sent) 3))
                (should (eq 'request (car (jabber-iq-query (car sent)))))
                (reply "upload.example"
                       '(slot ((xmlns . "urn:xmpp:http:upload:0"))
                              (put ((url . "https://upload.example/put")))
                              (get ((url . "https://upload.example/get")))))
                (if encryption
                    (progn
                      (should-not (equal uploaded-path file))
                      (should-not (equal uploaded-bytes "private bytes"))
                      (should (string-prefix-p "aesgcm://" url))
                      (should-not (file-exists-p uploaded-path))
                      (let ((parts (jabber-chat--parse-aesgcm-url url)))
                        (should (equal
                                 (jabber-omemo-aesgcm-decrypt
                                  (plist-get parts :key) (plist-get parts :iv)
                                  uploaded-bytes)
                                 "private bytes"))))
                  (should (equal uploaded-path file))
                  (should (equal uploaded-bytes "private bytes"))
                  (should (equal url "https://upload.example/get"))))))
          (unless failure (should-not jabber-open-info-queries))
          (should (equal (directory-files temporary-file-directory nil
                                          directory-files-no-dot-files-regexp)
                         (list (file-name-nondirectory file)))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (kill-buffer dispatch)
      (delete-directory temporary-file-directory t))))

(ert-deftest jabber-test-omemo-message-cold-upload-retains-selection ()
  "Cold native IQ discovery cannot downgrade an accepted encrypted upload."
  (dolist (change '(close mode toggle))
    (jabber-test-omemo-message--cold-upload 'omemo change)))

(ert-deftest jabber-test-omemo-message-cold-upload-plaintext-control ()
  "A deliberately plaintext upload keeps its original selection and hook."
  (dolist (change '(close mode toggle))
    (jabber-test-omemo-message--cold-upload nil change)))

(ert-deftest jabber-test-omemo-message-cold-upload-failures-stop-slot ()
  "Selected failures after held discovery must cause no slot or HTTP effects."
  (dolist (failure '(read encrypt write))
    (dolist (change '(close mode toggle))
      (jabber-test-omemo-message--cold-upload 'omemo change failure))))

(ert-deftest jabber-test-omemo-message-malformed-bundle-settles-send ()
  "A malformed asynchronous bundle fails the real send operation once."
  (let ((jabber-omemo--pending-send-operations (make-hash-table :test #'eq))
        (jabber-omemo--device-lists (make-hash-table :test #'equal))
        (jabber-chatting-with "peer@example.com")
        (failures 0) (encrypted 0) reply)
    (puthash (jabber-omemo--device-list-key "me@example.com" "peer@example.com")
             '(7) jabber-omemo--device-lists)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_) "me@example.com"))
              ((symbol-function 'jabber-blocking-ready-p) (lambda (&rest _) t))
              ((symbol-function 'jabber-blocking-blocked-p) (lambda (&rest _) nil))
              ((symbol-function 'jabber-omemo--get-device-id) (lambda (_) 99))
              ((symbol-function 'jabber-omemo--get-session) #'ignore)
              ((symbol-function 'jabber-omemo--display-pending) #'ignore)
              ((symbol-function 'jabber-omemo--send-failed) #'ignore)
              ((symbol-function 'jabber-omemo--send-encrypted)
               (lambda (&rest _) (cl-incf encrypted)))
              ((symbol-function 'jabber-omemo--request-peer)
               (lambda (_jc _jid _node success _failure _callback)
                 (setq reply success))))
      (jabber-omemo--send-chat 'fake-jc "hello" nil #'ignore
                             (lambda (_) (cl-incf failures)))
      (should (gethash 'fake-jc jabber-omemo--pending-send-operations))
      (funcall reply nil
               '(iq () (pubsub () (items () (item ()
                 (bundle () (signedPreKeyPublic () "%%%%")
                         (signedPreKeySignature () "YQ==")
                         (identityKey () "YQ==")))))) nil)
      (should (= failures 1))
      (should (= encrypted 0))
      (should-not (gethash 'fake-jc jabber-omemo--pending-send-operations)))))

;;; Interactive attachment settlement

(require 'jabber-chat-commands)

(defun jabber-test-omemo--attachment-reply (jc request payload)
  "Deliver PAYLOAD through native IQ dispatch for JC and REQUEST."
  (jabber-process-iq
   jc (with-temp-buffer
        (insert (format "<iq type='result' from='%s' id='%s'>%s</iq>"
                        (jabber-xml-get-attribute request 'to)
                        (jabber-xml-get-attribute request 'id) payload))
        (car (xml-parse-region (point-min) (point-max))))))
(defconst jabber-test-omemo--attachment-info "<query xmlns='http://jabber.org/protocol/disco#info'><feature var='urn:xmpp:http:upload:0'/></query>")
(defconst jabber-test-omemo--attachment-slot "<slot xmlns='urn:xmpp:http:upload:0'><put url='https://upload.example.invalid/put'/><get url='https://upload.example.invalid/get'/></slot>")
(defun jabber-test-omemo--attachment-slots (wire)
  "Count slot requests in WIRE."
  (cl-count-if (lambda (stanza) (eq (car (jabber-iq-query stanza)) 'request)) wire))
(defun jabber-test-omemo--attachment-case (cold failure &optional terminal-check replay)
  "Exercise COLD discovery and FAILURE with optional TERMINAL-CHECK or REPLAY."
  (let* ((temporary-file-directory (make-temp-file "jabber-test-omemo-attachment-" t))
         (file (make-temp-file "selected-" nil ".txt" "private attachment bytes"))
         (chat (generate-new-buffer " *jabber-test-omemo-attachment-chat*"))
         (dispatch (generate-new-buffer " *jabber-test-omemo-attachment-dispatch*"))
         (jc (make-symbol "attachment"))
         (jabber-connections (list jc))
         (jabber-httpupload-support (unless cold (list (cons jc "upload.example.invalid"))))
         (jabber-httpupload-max-file-size nil)
         (jabber-httpupload--discoveries nil)
         (jabber-disco-info-cache (make-hash-table :test #'equal))
         (jabber-disco-items-cache (make-hash-table :test #'equal))
         (jabber-open-info-queries nil)
         (jabber-chat-display-help-at-point nil)
         (jabber-chat-mode-hook nil)
         (jabber-httpupload-pre-upload-transform #'jabber-omemo--httpupload-transform)
         (command-history nil)
         (native-comp-enable-subr-trampolines nil)
         (read-original (symbol-function 'insert-file-contents-literally))
         (write-original (symbol-function 'write-region))
         (encrypt-original (symbol-function 'jabber-omemo-aesgcm-encrypt))
         (fault failure) (uploads 0) (selections 0)
         wire pending-error info-request uploaded-bytes uploaded-path
         (jabber-httpupload-upload-function
          (lambda (path _headers _put callback arg &optional _ignore)
            (cl-incf uploads)
            (setq uploaded-path path
                  uploaded-bytes (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally path)
                                   (buffer-string)))
            (funcall callback arg) t)))
    (put jc :state :session-established)
    (put jc :state-data
         (list :server "example.invalid" :username "attachment" :resource "test"
               :connection (make-symbol "transport") :session-id "attachment-stream"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _) (cl-incf selections) file))
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_owner stanza &rest _) (push stanza wire)))
                  ((symbol-function 'jabber-caps-get-cached) #'ignore)
                  ((symbol-function 'jabber-httpupload-ignore-certificate) #'ignore)
                  ((symbol-function 'make-network-process)
                   (lambda (&rest _) (error "Network forbidden")))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (error "HTTP forbidden")))
                  ((symbol-function 'insert-file-contents-literally)
                   (lambda (&rest args)
                     (if (and (eq fault 'read) (equal (car args) file))
                         (error "Injected attachment read failure")
                       (apply read-original args))))
                  ((symbol-function 'write-region)
                   (lambda (&rest args)
                     (if (eq fault 'write) (error "Injected ciphertext write failure")
                       (apply write-original args))))
                  ((symbol-function 'jabber-omemo-aesgcm-encrypt)
                   (lambda (bytes)
                     (pcase fault
                       ('encrypt (error "Injected encryption failure"))
                       ('rng (error "Injected random generation failure"))
                       (_ (funcall encrypt-original bytes))))))
          (with-current-buffer chat
            (jabber-chat-mode)
            (setq-local jabber-buffer-connection jc)
            (setq-local jabber-chat-encryption (if (eq failure 'plaintext) 'plaintext 'omemo))
            (setq-local jabber-point-insert (copy-marker (point-min)))
            (insert "Unicode draft λ with an earlier https://existing.invalid/file")
            (setq-local jabber-httpupload--pending-url "https://existing.invalid/file")
            (goto-char (+ (point-min) 5))
            (condition-case err
                ;; Force ordinary command history recording despite scripted minibuffer.
                (call-interactively #'jabber-chat-attach-file t)
              (error (setq pending-error (error-message-string err)))))
          (when cold
            (should-not pending-error)
            (should (= (length wire) 1))
            (with-current-buffer dispatch
              (setq-local jabber-chat-encryption 'plaintext)
              (jabber-test-omemo--attachment-reply jc (car wire)
                                                   "<query xmlns='http://jabber.org/protocol/disco#items'><item jid='upload.example.invalid'/></query>")
              (setq info-request (car wire))
              (condition-case err
                  (jabber-test-omemo--attachment-reply jc info-request jabber-test-omemo--attachment-info)
                (error (setq pending-error (error-message-string err))))))
          (if (memq failure '(read write encrypt rng))
              (progn
                (should (equal pending-error
                               (pcase failure
                                 ('read "Injected attachment read failure")
                                 ('write "Injected ciphertext write failure")
                                 ('encrypt "Injected encryption failure")
                                 ('rng "Injected random generation failure"))))
                (should (= (jabber-test-omemo--attachment-slots wire) 0))
                (should (= uploads 0))
                (with-current-buffer chat
                  (should (equal (buffer-string) "Unicode draft λ with an earlier https://existing.invalid/file"))
                  (should (= (point) (+ (point-min) 5)))
                  (should (equal jabber-httpupload--pending-url "https://existing.invalid/file")))
                (should (file-exists-p file))
                (should (equal (with-temp-buffer (funcall read-original file) (buffer-string))
                               "private attachment bytes"))
                (should (equal (car command-history) `(jabber-chat-attach-file ,file)))

                (when terminal-check
                  ;; A failed operation may not remain owned by an outstanding IQ.
                  (should-not jabber-open-info-queries))
                (should-not jabber-httpupload--discoveries)
                (setq fault nil)
                (if replay
                    (progn
                      (with-current-buffer dispatch (jabber-test-omemo--attachment-reply jc info-request jabber-test-omemo--attachment-info))
                      (when (> (jabber-test-omemo--attachment-slots wire) 0)
                        (with-current-buffer dispatch (jabber-test-omemo--attachment-reply jc (car wire) jabber-test-omemo--attachment-slot)))

                      ;; Closure acceptance: a repeated result may not restart failed work.
                      (should (= (jabber-test-omemo--attachment-slots wire) 0))
                      (should (= uploads 0))
                      (with-current-buffer chat
                        (should (equal (buffer-string)
                                       "Unicode draft λ with an earlier https://existing.invalid/file"))
                        (should (equal jabber-httpupload--pending-url
                                       "https://existing.invalid/file"))))
                  ;; Explicit retry uses the expression retained by command history.
                  (with-current-buffer chat (eval (car command-history) t))
                  (should (= selections 1))
                  (should (= (jabber-test-omemo--attachment-slots wire) 1))
                  (with-current-buffer dispatch (jabber-test-omemo--attachment-reply jc (car wire) jabber-test-omemo--attachment-slot))
                  (should (= uploads 1))
                  (should-not jabber-open-info-queries)))
            (should-not pending-error)
            (should (= (jabber-test-omemo--attachment-slots wire) 1))
            (with-current-buffer dispatch (jabber-test-omemo--attachment-reply jc (car wire) jabber-test-omemo--attachment-slot))
            (should (= uploads 1))
            (should-not jabber-open-info-queries))
          (unless (or terminal-check replay)
            (with-current-buffer chat
              (let ((url jabber-httpupload--pending-url))
                (should (string-suffix-p url (buffer-string)))
                (if (eq failure 'plaintext)
                    (progn (should (equal uploaded-path file))
                           (should (equal uploaded-bytes "private attachment bytes")))
                  (should-not (equal uploaded-path file))
                  (should-not (file-exists-p uploaded-path))
                  (should-not (equal uploaded-bytes "private attachment bytes"))
                  (let ((parts (jabber-chat--parse-aesgcm-url url)))
                    (should (equal (jabber-omemo-aesgcm-decrypt
                                    (plist-get parts :key) (plist-get parts :iv) uploaded-bytes)
                                   "private attachment bytes"))))
                ;; Native deferred-send OOB consumer attaches once, then retires URL.
                (should (equal (jabber-httpupload--send-hook (buffer-string) "probe")
                               `((x ((xmlns . ,jabber-oob-xmlns)) (url () ,url)))))
                (should-not jabber-httpupload--pending-url)
                (should-not (jabber-httpupload--send-hook (buffer-string) "probe2"))))))
      (kill-buffer chat)
      (kill-buffer dispatch)
      (delete-directory temporary-file-directory t))))

(dolist (cold '(nil t))
  (dolist (failure '(read write encrypt rng nil plaintext))
    (eval `(ert-deftest ,(intern (format "jabber-test-omemo-attachment-observe-%s-%s" (if cold "cold" "cached") failure)) ()
             (jabber-test-omemo--attachment-case ,cold ',failure)))))
(dolist (failure '(read write encrypt rng))
  (eval `(ert-deftest ,(intern (format "jabber-test-omemo-attachment-terminal-cold-%s" failure)) ()
           (jabber-test-omemo--attachment-case t ',failure t)))
  (eval `(ert-deftest ,(intern (format "jabber-test-omemo-attachment-replay-cold-%s" failure)) ()
           (jabber-test-omemo--attachment-case t ',failure nil t))))

(provide 'jabber-test-omemo-message)
;;; jabber-test-omemo-message.el ends here
