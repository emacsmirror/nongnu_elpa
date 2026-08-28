;;; jabber-test-omemo-trust.el --- Tests for jabber-omemo-trust  -*- lexical-binding: t; -*-

;;; Commentary:

;; OMEMO trust management.

;;; Code:

(require 'ert)
(require 'jabber-omemo-trust)

;;; Group 1: trust label mapping

(ert-deftest jabber-test-omemo-trust-label-undecided ()
  "Trust level 0 maps to undecided."
  (should (string= "undecided" (jabber-omemo--trust-label 0))))

(ert-deftest jabber-test-omemo-trust-label-tofu ()
  "Trust level 1 maps to TOFU."
  (should (string= "TOFU" (jabber-omemo--trust-label 1))))

(ert-deftest jabber-test-omemo-trust-label-verified ()
  "Trust level 2 maps to verified."
  (should (string= "verified" (jabber-omemo--trust-label 2))))

(ert-deftest jabber-test-omemo-trust-label-untrusted ()
  "Trust level -1 maps to UNTRUSTED."
  (should (string= "UNTRUSTED" (jabber-omemo--trust-label -1))))

(ert-deftest jabber-test-omemo-trust-label-unknown ()
  "Unknown trust level shows the number."
  (should (string= "unknown(99)" (jabber-omemo--trust-label 99))))

;;; Group 2: fingerprint formatting

(ert-deftest jabber-test-omemo-trust-format-fingerprint ()
  "Fingerprint formats as space-separated uppercase hex pairs."
  (let ((key (unibyte-string #xDE #xAD #xBE #xEF)))
    (should (string= "DE AD BE EF"
                      (jabber-omemo--format-fingerprint key)))))

(ert-deftest jabber-test-omemo-trust-format-fingerprint-empty ()
  "Empty key produces empty string."
  (should (string= "" (jabber-omemo--format-fingerprint ""))))

;;; Group 3: key type stripping

(ert-deftest jabber-test-omemo-trust-strip-key-type ()
  "Strip 0x05 prefix from identity key."
  (let ((key (unibyte-string #x05 #xAB #xCD)))
    (should (equal (unibyte-string #xAB #xCD)
                   (jabber-omemo-trust--strip-key-type key)))))

(ert-deftest jabber-test-omemo-trust-strip-key-type-no-prefix ()
  "Leave key unchanged when no 0x05 prefix."
  (let ((key (unibyte-string #xAB #xCD)))
    (should (equal key (jabber-omemo-trust--strip-key-type key)))))

;;; Group 4: entries function

(ert-deftest jabber-test-omemo-trust-entries-shape ()
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

(ert-deftest jabber-test-omemo-trust-entries-empty ()
  "Entries returns nil for no trust records."
  (cl-letf (((symbol-function 'jabber-omemo-store-all-trust)
             (lambda (_acct _jid) nil)))
    (let ((jabber-omemo-trust--account "alice@example.com")
          (jabber-omemo-trust--peer "bob@example.com"))
      (should (null (jabber-omemo-trust--entries))))))

;;; Group 5: column format

(ert-deftest jabber-test-omemo-trust-column-format ()
  "Mode sets a 4-column tabulated-list-format."
  (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 20)))
    (with-temp-buffer
      (jabber-omemo-trust-mode)
      (should (= 4 (length tabulated-list-format)))
      (should (equal "Device ID" (car (aref tabulated-list-format 0))))
      (should (= 9 (cadr (aref tabulated-list-format 0))))
      (should (equal "Trust" (car (aref tabulated-list-format 1))))
      (should (= 8 (cadr (aref tabulated-list-format 1))))
      (should (equal "Fingerprint" (car (aref tabulated-list-format 2))))
      (should (= 32 (cadr (aref tabulated-list-format 2))))
      (should (equal "First Seen" (car (aref tabulated-list-format 3))))
      (should (= 16 (cadr (aref tabulated-list-format 3)))))))

;;; Group 6: session reset

(ert-deftest jabber-test-omemo-trust-reset-session-rebuilds-device ()
  "Reset command rebuilds the selected peer device session."
  (let (called callback messages)
    (cl-letf (((symbol-function 'jabber-omemo-trust--device-at-point)
               (lambda () 42))
              ((symbol-function 'jabber-connection-active-p)
               (lambda (jc) (eq jc 'live-jc)))
              ((symbol-function 'jabber-find-active-connection)
               (lambda (_jc) 'live-jc))
              ((symbol-function 'jabber-omemo--reset-session)
               (lambda (jc jid did cb)
                 (setq called (list jc jid did)
                       callback cb)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (let ((jabber-omemo-trust--jc 'stale-jc)
            (jabber-omemo-trust--peer "alice@example.com"))
        (jabber-omemo-reset-session)
        (should (equal '(live-jc "alice@example.com" 42) called))
        (funcall callback 'fresh-session)
        (should (equal '("OMEMO: rebuilt session for alice@example.com device 42"
                         "OMEMO: resetting session for alice@example.com device 42")
                       messages))))))

(ert-deftest jabber-test-omemo-trust-reset-session-reports-failure ()
  "Reset command reports a failed asynchronous rebuild."
  (let (callback messages)
    (cl-letf (((symbol-function 'jabber-omemo-trust--device-at-point)
               (lambda () 42))
              ((symbol-function 'jabber-connection-active-p)
               (lambda (_jc) t))
              ((symbol-function 'jabber-omemo--reset-session)
               (lambda (_jc _jid _did cb) (setq callback cb)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (let ((jabber-omemo-trust--jc 'fake-jc)
            (jabber-omemo-trust--peer "alice@example.com"))
        (jabber-omemo-reset-session)
        (funcall callback nil)
        (should (equal '("OMEMO: could not rebuild session for alice@example.com device 42"
                         "OMEMO: resetting session for alice@example.com device 42")
                       messages))))))

(ert-deftest jabber-test-omemo-trust-reset-session-requires-active-connection ()
  "Reset refuses to delete state when no active connection exists."
  (let (called)
    (cl-letf (((symbol-function 'jabber-omemo-trust--device-at-point)
               (lambda () 42))
              ((symbol-function 'jabber-connection-active-p)
               (lambda (_jc) nil))
              ((symbol-function 'jabber-find-active-connection)
               (lambda (_jc) nil))
              ((symbol-function 'jabber-omemo--reset-session)
               (lambda (&rest _) (setq called t))))
      (let ((jabber-omemo-trust--jc 'stale-jc)
            (jabber-omemo-trust--peer "alice@example.com"))
        (should-error (jabber-omemo-reset-session) :type 'user-error)
        (should-not called)))))

(ert-deftest jabber-test-omemo-trust-reset-session-key ()
  "The trust mode binds r to session reset."
  (should (eq #'jabber-omemo-reset-session
              (keymap-lookup jabber-omemo-trust-mode-map "r"))))

(provide 'jabber-test-omemo-trust)
;;; jabber-test-omemo-trust.el ends here
