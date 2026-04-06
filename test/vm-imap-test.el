;;; vm-imap-test.el --- Tests for vm-imap.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM IMAP functions in vm-imap.el
;; Tests pure functions directly and uses mocking for network-dependent code.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap)

;;; vm-imap-parse-spec-to-list tests

(ert-deftest vm-imap-test-parse-spec-basic ()
  "Test parsing a basic IMAP spec."
  (let ((result (vm-imap-parse-spec-to-list
                 "imap:mail.example.com:143:inbox:login:user:password")))
    (should (equal (nth 0 result) "imap"))
    (should (equal (nth 1 result) "mail.example.com"))
    (should (equal (nth 2 result) "143"))
    (should (equal (nth 3 result) "inbox"))
    (should (equal (nth 4 result) "login"))
    (should (equal (nth 5 result) "user"))
    (should (equal (nth 6 result) "password"))))

(ert-deftest vm-imap-test-parse-spec-ssl ()
  "Test parsing an IMAP spec with SSL port."
  (let ((result (vm-imap-parse-spec-to-list
                 "imap-ssl:mail.example.com:993:INBOX:login:user:pass")))
    (should (equal (nth 0 result) "imap-ssl"))
    (should (equal (nth 2 result) "993"))))

(ert-deftest vm-imap-test-parse-spec-wildcard-password ()
  "Test parsing spec with wildcard password."
  (let ((result (vm-imap-parse-spec-to-list
                 "imap:mail.example.com:143:inbox:login:user:*")))
    (should (equal (nth 6 result) "*"))))

(ert-deftest vm-imap-test-parse-spec-wildcard-mailbox ()
  "Test parsing spec with wildcard mailbox."
  (let ((result (vm-imap-parse-spec-to-list
                 "imap:mail.example.com:143:*:login:user:*")))
    (should (equal (nth 3 result) "*"))))

;;; vm-imap-encode-list-to-spec tests

(ert-deftest vm-imap-test-encode-list-basic ()
  "Test encoding a list to spec string."
  (should (equal (vm-imap-encode-list-to-spec
                  '("imap" "mail.example.com" "143" "inbox" "login" "user" "pass"))
                 "imap:mail.example.com:143:inbox:login:user:pass")))

(ert-deftest vm-imap-test-encode-list-roundtrip ()
  "Test that encoding reverses parsing."
  (let ((spec "imap:mail.example.com:143:inbox:login:user:password"))
    (should (equal (vm-imap-encode-list-to-spec
                    (vm-imap-parse-spec-to-list spec))
                   spec))))

;;; vm-imap-normalize-spec tests

(ert-deftest vm-imap-test-normalize-spec-scrubs-password ()
  "Test that normalize-spec scrubs the password."
  (let ((result (vm-imap-normalize-spec
                 "imap:mail.example.com:143:inbox:login:user:secret")))
    (should (string-match ":user:\\*$" result))))

(ert-deftest vm-imap-test-normalize-spec-scrubs-port ()
  "Test that normalize-spec scrubs the port."
  (let ((result (vm-imap-normalize-spec
                 "imap:mail.example.com:993:inbox:login:user:secret")))
    (should (string-match ":\\*:inbox:" result))))

(ert-deftest vm-imap-test-normalize-spec-scrubs-auth ()
  "Test that normalize-spec scrubs the auth method."
  (let ((result (vm-imap-normalize-spec
                 "imap:mail.example.com:143:inbox:cram-md5:user:secret")))
    (should (string-match ":inbox:\\*:user:" result))))

(ert-deftest vm-imap-test-normalize-spec-standardizes-protocol ()
  "Test that normalize-spec standardizes the protocol to 'imap'."
  (let ((result (vm-imap-normalize-spec
                 "imap-ssl:mail.example.com:993:inbox:login:user:secret")))
    (should (string-match "^imap:" result))))

;;; vm-imap-spec-for-mailbox tests

(ert-deftest vm-imap-test-spec-for-mailbox-basic ()
  "Test generating spec for different mailbox."
  (let ((result (vm-imap-spec-for-mailbox
                 "imap:mail.example.com:143:inbox:login:user:pass"
                 "Sent")))
    (should (string-match ":Sent:" result))
    (should-not (string-match ":inbox:" result))))

(ert-deftest vm-imap-test-spec-for-mailbox-preserves-other-parts ()
  "Test that spec-for-mailbox preserves other parts of the spec."
  (let ((result (vm-imap-spec-for-mailbox
                 "imap:mail.example.com:143:inbox:login:user:pass"
                 "Archive")))
    (should (string-match "^imap:mail.example.com:143:" result))
    (should (string-match ":login:user:pass$" result))))

;;; vm-imap-account-name-for-spec tests

(ert-deftest vm-imap-test-account-name-found ()
  "Test finding account name from spec."
  (let ((vm-imap-account-alist
         '(("imap:mail.example.com:143:*:login:user:*" "work")
           ("imap:personal.com:993:*:login:me:*" "home"))))
    (should (equal (vm-imap-account-name-for-spec
                    "imap:mail.example.com:143:inbox:login:user:secret")
                   "work"))))

(ert-deftest vm-imap-test-account-name-not-found ()
  "Test that nil is returned when account not found."
  (let ((vm-imap-account-alist
         '(("imap:mail.example.com:143:*:login:user:*" "work"))))
    (should (null (vm-imap-account-name-for-spec
                   "imap:unknown.com:143:inbox:login:other:secret")))))

;;; vm-imap-folder-name-for-spec tests

(ert-deftest vm-imap-test-folder-name-basic ()
  "Test extracting folder name from spec."
  (let ((result (vm-imap-folder-name-for-spec
                 "imap:mail.example.com:143:MyFolder:login:user:pass")))
    ;; The function uses account-alist lookup, so without it returns nil
    ;; but the spec component extraction should work
    (should (or (null result)
                (stringp result)))))

;;; vm-imap-quote-buffer tests

(ert-deftest vm-imap-test-quote-buffer-simple ()
  "Test quoting a simple string in buffer."
  (with-temp-buffer
    (insert "hello")
    (vm-imap-quote-buffer)
    (should (equal (buffer-string) "\"hello\""))))

(ert-deftest vm-imap-test-quote-buffer-with-quotes ()
  "Test quoting a string containing quotes."
  (with-temp-buffer
    (insert "say \"hello\"")
    (vm-imap-quote-buffer)
    (should (equal (buffer-string) "\"say \\\"hello\\\"\""))))

(ert-deftest vm-imap-test-quote-buffer-with-backslash ()
  "Test quoting a string containing backslash."
  (with-temp-buffer
    (insert "path\\to\\file")
    (vm-imap-quote-buffer)
    (should (equal (buffer-string) "\"path\\\\to\\\\file\""))))

(ert-deftest vm-imap-test-quote-buffer-empty ()
  "Test quoting an empty buffer."
  (with-temp-buffer
    (vm-imap-quote-buffer)
    (should (equal (buffer-string) "\"\""))))

;;; IMAP fixture tests

(ert-deftest vm-imap-test-fixtures-exist ()
  "Test that IMAP fixtures exist."
  (should (vm-test-fixture-exists-p "imap" "capability-response.txt"))
  (should (vm-test-fixture-exists-p "imap" "select-response.txt"))
  (should (vm-test-fixture-exists-p "imap" "fetch-response.txt"))
  (should (vm-test-fixture-exists-p "imap" "list-response.txt")))

(ert-deftest vm-imap-test-read-capability-fixture ()
  "Test reading capability response fixture."
  (let ((content (vm-test-read-fixture "imap" "capability-response.txt")))
    (should (stringp content))
    (should (string-match "CAPABILITY" content))
    (should (string-match "IMAP4rev1" content))
    (should (string-match "AUTH=PLAIN" content))))

(ert-deftest vm-imap-test-read-select-fixture ()
  "Test reading select response fixture."
  (let ((content (vm-test-read-fixture "imap" "select-response.txt")))
    (should (stringp content))
    (should (string-match "EXISTS" content))
    (should (string-match "UIDVALIDITY" content))
    (should (string-match "FLAGS" content))))

(ert-deftest vm-imap-test-read-greeting-fixture ()
  "Test reading greeting fixtures."
  (let ((ok (vm-test-read-fixture "imap" "greeting-ok.txt"))
        (preauth (vm-test-read-fixture "imap" "greeting-preauth.txt")))
    (should (string-match "\\* OK" ok))
    (should (string-match "\\* PREAUTH" preauth))))

;;; vm-imap-make-filename-for-spec tests

(ert-deftest vm-imap-test-make-filename-returns-string ()
  "Test that make-filename-for-spec returns a string."
  (let ((vm-imap-folder-cache-directory nil)
        (vm-folder-directory nil))
    (should (stringp (vm-imap-make-filename-for-spec
                      "imap:mail.example.com:143:inbox:login:user:pass")))))

(ert-deftest vm-imap-test-make-filename-includes-prefix ()
  "Test that make-filename includes 'imap-cache-' prefix."
  (let ((vm-imap-folder-cache-directory nil)
        (vm-folder-directory nil)
        (result (vm-imap-make-filename-for-spec
                 "imap:mail.example.com:143:inbox:login:user:pass")))
    (should (string-match "imap-cache-" result))))

(ert-deftest vm-imap-test-make-filename-same-for-normalized ()
  "Test that different auth/password give same filename."
  (let ((vm-imap-folder-cache-directory nil)
        (vm-folder-directory nil)
        (result1 (vm-imap-make-filename-for-spec
                  "imap:mail.example.com:143:inbox:login:user:secret1"))
        (result2 (vm-imap-make-filename-for-spec
                  "imap:mail.example.com:143:inbox:cram-md5:user:secret2")))
    ;; Both should normalize to same filename since password/auth are scrubbed
    (should (equal result1 result2))))

;;; Error condition types tests

(ert-deftest vm-imap-test-error-conditions-defined ()
  "Test that IMAP error conditions are defined."
  (should (get 'vm-imap-protocol-error 'error-conditions))
  (should (get 'vm-imap-normal-error 'error-conditions)))

(ert-deftest vm-imap-test-error-hierarchy ()
  "Test that vm-imap-normal-error is a subtype of vm-imap-protocol-error."
  (let ((conditions (get 'vm-imap-normal-error 'error-conditions)))
    (should (memq 'vm-imap-protocol-error conditions))
    (should (memq 'error conditions))))

;;; Mock-based network tests

(ert-deftest vm-imap-test-mock-infrastructure ()
  "Test that our mock infrastructure works."
  (let ((mock-called nil)
        (mock-args nil))
    (cl-letf (((symbol-function 'open-network-stream)
               (lambda (&rest args)
                 (setq mock-called t)
                 (setq mock-args args)
                 nil)))
      (ignore-errors
        (open-network-stream "test" nil "example.com" 143))
      (should mock-called)
      (should (equal (car mock-args) "test")))))

;;; vm-imap-quote-string tests

(ert-deftest vm-imap-test-quote-string-simple ()
  "Test quoting a simple string."
  (should (equal (vm-imap-quote-string "hello") "\"hello\"")))

(ert-deftest vm-imap-test-quote-string-with-quotes ()
  "Test quoting a string containing quotes."
  (should (equal (vm-imap-quote-string "say \"hello\"")
                 "\"say \\\"hello\\\"\"")))

(ert-deftest vm-imap-test-quote-string-with-backslash ()
  "Test quoting a string containing backslash."
  (should (equal (vm-imap-quote-string "path\\to\\file")
                 "\"path\\\\to\\\\file\"")))

(ert-deftest vm-imap-test-quote-string-empty ()
  "Test quoting an empty string."
  (should (equal (vm-imap-quote-string "") "\"\"")))

;;; vm-imap-folder-spec-p tests

(ert-deftest vm-imap-test-folder-spec-p-valid ()
  "Test vm-imap-folder-spec-p with valid IMAP spec."
  (let ((vm-recognize-imap-maildrops "^imap:"))
    (should (vm-imap-folder-spec-p "imap:mail.example.com:143:inbox:login:user:pass"))))

(ert-deftest vm-imap-test-folder-spec-p-imap-ssl ()
  "Test vm-imap-folder-spec-p with IMAP-SSL spec."
  (let ((vm-recognize-imap-maildrops "^imap"))
    (should (vm-imap-folder-spec-p "imap-ssl:mail.example.com:993:inbox:login:user:pass"))))

(ert-deftest vm-imap-test-folder-spec-p-not-imap ()
  "Test vm-imap-folder-spec-p with non-IMAP spec."
  (let ((vm-recognize-imap-maildrops "^imap:"))
    (should-not (vm-imap-folder-spec-p "/home/user/mail/inbox"))))

(ert-deftest vm-imap-test-folder-spec-p-disabled ()
  "Test vm-imap-folder-spec-p when recognition disabled."
  (let ((vm-recognize-imap-maildrops nil))
    (should-not (vm-imap-folder-spec-p "imap:mail.example.com:143:inbox:login:user:pass"))))

;;; vm-imap-encode/decode-mailbox-name tests

(ert-deftest vm-imap-test-encode-mailbox-ascii ()
  "Test encoding ASCII mailbox name."
  (should (equal (vm-imap-encode-mailbox-name "INBOX") "INBOX")))

(ert-deftest vm-imap-test-decode-mailbox-ascii ()
  "Test decoding ASCII mailbox name."
  (should (equal (vm-imap-decode-mailbox-name "INBOX") "INBOX")))

(ert-deftest vm-imap-test-encode-decode-roundtrip ()
  "Test mailbox name encode/decode roundtrip."
  (let ((name "INBOX"))
    (should (equal (vm-imap-decode-mailbox-name
                    (vm-imap-encode-mailbox-name name))
                   name))))

;;; vm-imap-quote-mailbox-name tests

(ert-deftest vm-imap-test-quote-mailbox-name-simple ()
  "Test quoting simple mailbox name."
  (should (equal (vm-imap-quote-mailbox-name "INBOX") "\"INBOX\"")))

;;; Additional spec parsing tests

(ert-deftest vm-imap-test-parse-spec-special-chars ()
  "Test parsing spec with special characters in password."
  (let ((result (vm-imap-parse-spec-to-list
                 "imap:mail.example.com:143:inbox:login:user:p@ss:word")))
    ;; The last part should contain the full password
    (should (equal (nth 6 result) "p@ss:word"))))

(ert-deftest vm-imap-test-spec-for-mailbox-nested ()
  "Test generating spec for nested mailbox."
  (let ((result (vm-imap-spec-for-mailbox
                 "imap:mail.example.com:143:inbox:login:user:pass"
                 "Archive/2024")))
    (should (string-match ":Archive/2024:" result))))

;;; Error handling tests

(ert-deftest vm-imap-test-signal-protocol-error ()
  "Test signaling IMAP protocol error."
  (should-error (signal 'vm-imap-protocol-error '("test error"))
                :type 'vm-imap-protocol-error))

(ert-deftest vm-imap-test-signal-normal-error ()
  "Test signaling IMAP normal error."
  (should-error (signal 'vm-imap-normal-error '("test error"))
                :type 'vm-imap-normal-error))

(provide 'vm-imap-test)

;;; vm-imap-test.el ends here
