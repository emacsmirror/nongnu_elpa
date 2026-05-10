;;; jabber-omemo-trust-tests.el --- Tests for OMEMO trust UI  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-omemo-store)

;; Load the trust UI without triggering jabber-omemo's C module requirement.
;; We mock the declare-function targets instead.
(unless (fboundp 'jabber-omemo--format-fingerprint)
  (defun jabber-omemo--format-fingerprint (identity-key)
    "Test stub: format IDENTITY-KEY as hex pairs."
    (mapconcat (lambda (byte) (format "%02X" byte))
               identity-key " ")))

(unless (fboundp 'jabber-omemo--trust-label)
  (defun jabber-omemo--trust-label (level)
    "Test stub: return label for trust LEVEL."
    (pcase level
      (0 "undecided")
      (1 "TOFU")
      (2 "verified")
      (-1 "UNTRUSTED")
      (_ (format "unknown(%d)" level)))))

(unless (fboundp 'jabber-connection-bare-jid)
  (defun jabber-connection-bare-jid (_jc) "alice@example.com"))

(unless (fboundp 'jabber-jid-user)
  (defun jabber-jid-user (jid) (car (split-string jid "/"))))

(unless (fboundp 'jabber-read-account)
  (defun jabber-read-account () 'test-jc))

(require 'jabber-omemo-trust)

;;; Group 1: trust label mapping

(ert-deftest jabber-omemo-trust-test-label-undecided ()
  "Trust level 0 maps to undecided."
  (should (string= "undecided" (jabber-omemo--trust-label 0))))

(ert-deftest jabber-omemo-trust-test-label-tofu ()
  "Trust level 1 maps to TOFU."
  (should (string= "TOFU" (jabber-omemo--trust-label 1))))

(ert-deftest jabber-omemo-trust-test-label-verified ()
  "Trust level 2 maps to verified."
  (should (string= "verified" (jabber-omemo--trust-label 2))))

(ert-deftest jabber-omemo-trust-test-label-untrusted ()
  "Trust level -1 maps to UNTRUSTED."
  (should (string= "UNTRUSTED" (jabber-omemo--trust-label -1))))

(ert-deftest jabber-omemo-trust-test-label-unknown ()
  "Unknown trust level shows the number."
  (should (string= "unknown(99)" (jabber-omemo--trust-label 99))))

;;; Group 2: fingerprint formatting

(ert-deftest jabber-omemo-trust-test-format-fingerprint ()
  "Fingerprint formats as space-separated uppercase hex pairs."
  (let ((key (unibyte-string #xDE #xAD #xBE #xEF)))
    (should (string= "DE AD BE EF"
                      (jabber-omemo--format-fingerprint key)))))

(ert-deftest jabber-omemo-trust-test-format-fingerprint-empty ()
  "Empty key produces empty string."
  (should (string= "" (jabber-omemo--format-fingerprint ""))))

;;; Group 3: key type stripping

(ert-deftest jabber-omemo-trust-test-strip-key-type ()
  "Strip 0x05 prefix from identity key."
  (let ((key (unibyte-string #x05 #xAB #xCD)))
    (should (equal (unibyte-string #xAB #xCD)
                   (jabber-omemo-trust--strip-key-type key)))))

(ert-deftest jabber-omemo-trust-test-strip-key-type-no-prefix ()
  "Leave key unchanged when no 0x05 prefix."
  (let ((key (unibyte-string #xAB #xCD)))
    (should (equal key (jabber-omemo-trust--strip-key-type key)))))

;;; Group 4: entries function

(ert-deftest jabber-omemo-trust-test-entries-shape ()
  "Entries returns list of (ID VECTOR) from trust records."
  (cl-letf (((symbol-function 'jabber-omemo-store-all-trust)
             (lambda (_acct _jid)
               (list (list :device-id 12345
                           :identity-key (unibyte-string #x05 #xAB #xCD)
                           :trust 1
                           :first-seen 1710000000)
                     (list :device-id 67890
                           :identity-key (unibyte-string #x05 #xEF #x01)
                           :trust 2
                           :first-seen nil)))))
    (let ((jabber-omemo-trust--account "alice@example.com")
          (jabber-omemo-trust--peer "bob@example.com"))
      (let ((entries (jabber-omemo-trust--entries)))
        (should (= 2 (length entries)))
        ;; First entry: 05 stripped, fingerprint is just AB CD
        (let ((entry (car entries)))
          (should (= 12345 (car entry)))
          (should (vectorp (cadr entry)))
          (should (string= "12345" (aref (cadr entry) 0)))
          (should (string= "TOFU" (aref (cadr entry) 1)))
          (should (string= "AB CD" (aref (cadr entry) 2)))
          (should (not (string= "" (aref (cadr entry) 3)))))
        ;; Second entry
        (let ((entry (cadr entries)))
          (should (= 67890 (car entry)))
          (should (string= "verified" (aref (cadr entry) 1)))
          (should (string= "" (aref (cadr entry) 3))))))))

(ert-deftest jabber-omemo-trust-test-entries-empty ()
  "Entries returns nil for no trust records."
  (cl-letf (((symbol-function 'jabber-omemo-store-all-trust)
             (lambda (_acct _jid) nil)))
    (let ((jabber-omemo-trust--account "alice@example.com")
          (jabber-omemo-trust--peer "bob@example.com"))
      (should (null (jabber-omemo-trust--entries))))))

;;; Group 5: column format

(ert-deftest jabber-omemo-trust-test-column-format ()
  "Mode sets a 4-column tabulated-list-format."
  (with-temp-buffer
    (jabber-omemo-trust-mode)
    (should (= 4 (length tabulated-list-format)))
    (should (string= "Device ID" (car (aref tabulated-list-format 0))))
    (should (string= "Trust" (car (aref tabulated-list-format 1))))
    (should (string= "Fingerprint" (car (aref tabulated-list-format 2))))
    (should (string= "First Seen" (car (aref tabulated-list-format 3))))))

(provide 'jabber-omemo-trust-tests)
;;; jabber-omemo-trust-tests.el ends here
