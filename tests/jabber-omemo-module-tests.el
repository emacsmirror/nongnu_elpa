;;; jabber-omemo-module-tests.el --- ERT tests for OMEMO dynamic module  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-omemo-core)

;;; Group 1: Module loading

(ert-deftest jabber-omemo-module-test-provides-feature ()
  "The module provides the `jabber-omemo-core' feature."
  (should (featurep 'jabber-omemo-core)))

(ert-deftest jabber-omemo-module-test-functions-bound ()
  "All internal functions are bound after loading."
  (should (fboundp 'jabber-omemo--setup-store))
  (should (fboundp 'jabber-omemo--deserialize-store))
  (should (fboundp 'jabber-omemo--serialize-store))
  (should (fboundp 'jabber-omemo--get-bundle))
  (should (fboundp 'jabber-omemo--rotate-signed-pre-key))
  (should (fboundp 'jabber-omemo--refill-pre-keys))
  (should (fboundp 'jabber-omemo--encrypt-message))
  (should (fboundp 'jabber-omemo--decrypt-message)))

;;; Group 2: Store lifecycle

(ert-deftest jabber-omemo-module-test-setup-store-returns-unibyte ()
  "setup-store returns a non-empty unibyte string."
  (let ((blob (jabber-omemo--setup-store)))
    (should (stringp blob))
    (should (not (multibyte-string-p blob)))
    (should (> (length blob) 0))))

(ert-deftest jabber-omemo-module-test-deserialize-store-returns-user-ptr ()
  "deserialize-store returns a user-ptr."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob)))
    (should (user-ptrp ptr))))

(ert-deftest jabber-omemo-module-test-store-round-trip ()
  "Serializing a deserialized store produces the same blob."
  (let* ((blob1 (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob1))
         (blob2 (jabber-omemo--serialize-store ptr)))
    (should (string= blob1 blob2))))

;;; Group 3: Bundle extraction

(ert-deftest jabber-omemo-module-test-get-bundle-plist-keys ()
  "get-bundle returns a plist with expected keys."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr)))
    (should (plist-get bundle :identity-key))
    (should (plist-get bundle :signed-pre-key))
    (should (plist-get bundle :signed-pre-key-id))
    (should (plist-get bundle :signature))
    (should (plist-get bundle :pre-keys))))

(ert-deftest jabber-omemo-module-test-identity-key-length ()
  "Identity key is 33 bytes (0x05 prefix + 32-byte key)."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr))
         (ik (plist-get bundle :identity-key)))
    (should (= (length ik) 33))
    (should (= (aref ik 0) #x05))))

(ert-deftest jabber-omemo-module-test-signed-pre-key-length ()
  "Signed pre-key is 33 bytes."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr))
         (spk (plist-get bundle :signed-pre-key)))
    (should (= (length spk) 33))))

(ert-deftest jabber-omemo-module-test-signature-length ()
  "Signature is 64 bytes."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr))
         (sig (plist-get bundle :signature)))
    (should (= (length sig) 64))))

(ert-deftest jabber-omemo-module-test-pre-keys-count ()
  "Pre-keys list has 100 entries."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr))
         (pks (plist-get bundle :pre-keys)))
    (should (= (length pks) 100))))

(ert-deftest jabber-omemo-module-test-pre-key-format ()
  "Each pre-key is (id . key) with integer id and 33-byte key."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (bundle (jabber-omemo--get-bundle ptr))
         (pks (plist-get bundle :pre-keys))
         (first-pk (car pks)))
    (should (consp first-pk))
    (should (integerp (car first-pk)))
    (should (= (length (cdr first-pk)) 33))))

;;; Group 4: Key rotation

(ert-deftest jabber-omemo-module-test-rotate-changes-spk-id ()
  "Rotating the signed pre-key changes its ID."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob))
         (id-before (plist-get (jabber-omemo--get-bundle ptr)
                               :signed-pre-key-id)))
    (jabber-omemo--rotate-signed-pre-key ptr)
    (let ((id-after (plist-get (jabber-omemo--get-bundle ptr)
                               :signed-pre-key-id)))
      (should-not (= id-before id-after)))))

(ert-deftest jabber-omemo-module-test-refill-pre-keys ()
  "refill-pre-keys does not error on a fresh store."
  (let* ((blob (jabber-omemo--setup-store))
         (ptr (jabber-omemo--deserialize-store blob)))
    (jabber-omemo--refill-pre-keys ptr)
    (should t)))

;;; Group 5: Message encrypt/decrypt

(ert-deftest jabber-omemo-module-test-encrypt-returns-plist ()
  "encrypt-message returns plist with :key, :iv, :ciphertext."
  (let ((result (jabber-omemo--encrypt-message
                 (encode-coding-string "hello" 'utf-8))))
    (should (plist-get result :key))
    (should (plist-get result :iv))
    (should (plist-get result :ciphertext))))

(ert-deftest jabber-omemo-module-test-encrypt-key-length ()
  "Encryption key is 32 bytes (16 AES + 16 auth tag)."
  (let* ((result (jabber-omemo--encrypt-message
                  (encode-coding-string "test" 'utf-8)))
         (key (plist-get result :key)))
    (should (= (length key) 32))))

(ert-deftest jabber-omemo-module-test-encrypt-iv-length ()
  "IV is 12 bytes."
  (let* ((result (jabber-omemo--encrypt-message
                  (encode-coding-string "test" 'utf-8)))
         (iv (plist-get result :iv)))
    (should (= (length iv) 12))))

(ert-deftest jabber-omemo-module-test-decrypt-recovers-plaintext ()
  "Decrypting an encrypted message recovers the original."
  (let* ((msg (encode-coding-string "Hello there!" 'utf-8))
         (enc (jabber-omemo--encrypt-message msg))
         (dec (jabber-omemo--decrypt-message
               (plist-get enc :key)
               (plist-get enc :iv)
               (plist-get enc :ciphertext))))
    (should (string= msg dec))))

(ert-deftest jabber-omemo-module-test-encrypt-decrypt-round-trip-utf8 ()
  "Round-trip works with UTF-8 content."
  (let* ((msg (encode-coding-string "Hallo Welt! \u00e4\u00f6\u00fc" 'utf-8))
         (enc (jabber-omemo--encrypt-message msg))
         (dec (jabber-omemo--decrypt-message
               (plist-get enc :key)
               (plist-get enc :iv)
               (plist-get enc :ciphertext))))
    (should (string= msg dec))))

(ert-deftest jabber-omemo-module-test-decrypt-wrong-key-signals-error ()
  "Decrypting with a wrong key signals jabber-omemo-error."
  (let* ((msg (encode-coding-string "secret" 'utf-8))
         (enc (jabber-omemo--encrypt-message msg))
         (bad-key (make-string 32 ?x)))
    (should-error
     (jabber-omemo--decrypt-message
      bad-key
      (plist-get enc :iv)
      (plist-get enc :ciphertext))
     :type 'jabber-omemo-error)))

(provide 'jabber-omemo-module-tests)
;;; jabber-omemo-module-tests.el ends here
