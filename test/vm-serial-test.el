;;; vm-serial-test.el --- Tests for vm-serial.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM serial mail functions in vm-serial.el

;;; Code:

(require 'vm-test-init)
(require 'vm-serial)

;;; vm-serial-cookie tests

(ert-deftest vm-serial-test-default-cookie ()
  "Test that the default cookie is $."
  (should (stringp vm-serial-cookie))
  (should (string= vm-serial-cookie "$")))

;;; vm-serial-random-string tests

(ert-deftest vm-serial-test-random-string-returns-string ()
  "Test that vm-serial-random-string returns a string from the list."
  (let ((result (vm-serial-random-string '("a" "b" "c"))))
    (should (stringp result))
    (should (member result '("a" "b" "c")))))

(ert-deftest vm-serial-test-random-string-single-element ()
  "Test vm-serial-random-string with single element list."
  (let ((result (vm-serial-random-string '("only"))))
    (should (string= result "only"))))

(ert-deftest vm-serial-test-random-string-distribution ()
  "Test vm-serial-random-string returns varied results over many calls."
  (let ((results (make-hash-table :test 'equal))
        (choices '("a" "b" "c")))
    ;; Call many times and collect results
    (dotimes (_ 100)
      (let ((r (vm-serial-random-string choices)))
        (puthash r (1+ (gethash r results 0)) results)))
    ;; All choices should appear at least once (probabilistically)
    (should (>= (hash-table-count results) 2))))

;;; vm-serial-get-name tests

(ert-deftest vm-serial-test-get-name-full ()
  "Test vm-serial-get-name returns full name."
  (let ((vm-serial-to '("John Doe" "john@example.com")))
    (should (string= (vm-serial-get-name) "John Doe"))))

(ert-deftest vm-serial-test-get-name-first ()
  "Test vm-serial-get-name extracts first name."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'first "John Doe") "John"))))

(ert-deftest vm-serial-test-get-name-last ()
  "Test vm-serial-get-name extracts last name."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'last "John Doe") "Doe"))))

(ert-deftest vm-serial-test-get-name-first-multi-word ()
  "Test vm-serial-get-name with multi-word names."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'first "Mary Jane Watson") "Mary"))))

(ert-deftest vm-serial-test-get-name-last-multi-word ()
  "Test vm-serial-get-name last with multi-word names.
Returns everything after the first name, not just the last word."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'last "Mary Jane Watson") "Jane Watson"))))

;;; vm-serial-eval-token-value tests

(ert-deftest vm-serial-test-eval-token-value-string ()
  "Test vm-serial-eval-token-value with string input."
  (should (string= (vm-serial-eval-token-value "hello") "hello")))

(ert-deftest vm-serial-test-eval-token-value-function ()
  "Test vm-serial-eval-token-value with function."
  (should (string= (vm-serial-eval-token-value (lambda () "result")) "result")))

(ert-deftest vm-serial-test-eval-token-value-expression ()
  "Test vm-serial-eval-token-value with expression."
  (should (string= (vm-serial-eval-token-value '(concat "a" "b")) "ab")))

(ert-deftest vm-serial-test-eval-token-value-number ()
  "Test vm-serial-eval-token-value with numeric expression."
  (should (string= (vm-serial-eval-token-value '(number-to-string (+ 1 2))) "3")))

;;; vm-serial-token-alist tests

(ert-deftest vm-serial-test-default-tokens-exist ()
  "Test that default tokens are defined."
  (should (assoc "to" vm-serial-token-alist))
  (should (assoc "sir" vm-serial-token-alist))
  (should (assoc "you" vm-serial-token-alist))
  (should (assoc "mr" vm-serial-token-alist))
  (should (assoc "me" vm-serial-token-alist))
  (should (assoc "point" vm-serial-token-alist))
  (should (assoc "sig" vm-serial-token-alist)))

(ert-deftest vm-serial-test-token-structure ()
  "Test that tokens have proper structure (name . value)."
  (let ((token (assoc "me" vm-serial-token-alist)))
    (should token)
    (should (stringp (car token)))
    (should (cdr token))))  ; Value should be non-nil

;;; vm-serial-get-token tests

(ert-deftest vm-serial-test-get-token-existing ()
  "Test vm-serial-get-token for existing token."
  (let ((value (vm-serial-get-token "me")))
    ;; Should return a value (actual value depends on user settings)
    (should value)))

(ert-deftest vm-serial-test-get-token-nonexistent ()
  "Test vm-serial-get-token returns nil for nonexistent token."
  (should (null (vm-serial-get-token "nonexistent-token-xyz-12345"))))

;;; vm-serial-set-token tests

(ert-deftest vm-serial-test-set-token ()
  "Test vm-serial-set-token adds or updates token."
  (let ((vm-serial-token-alist (copy-alist vm-serial-token-alist)))
    (vm-serial-set-token "test-token" "test-value")
    (should (assoc "test-token" vm-serial-token-alist))
    (should (string= (vm-serial-get-token "test-token") "test-value"))))

(ert-deftest vm-serial-test-set-token-update ()
  "Test vm-serial-set-token updates existing token."
  (let ((vm-serial-token-alist (copy-alist vm-serial-token-alist)))
    (vm-serial-set-token "test-token" "value1")
    (vm-serial-set-token "test-token" "value2")
    (should (string= (vm-serial-get-token "test-token") "value2"))))

;;; vm-serial-mails-alist tests

(ert-deftest vm-serial-test-mails-alist-is-list ()
  "Test that vm-serial-mails-alist is a list."
  (should (listp vm-serial-mails-alist)))

;;; vm-serial-get-mail tests

(ert-deftest vm-serial-test-get-mail-nonexistent ()
  "Test vm-serial-get-mail returns nil for nonexistent template."
  (should (null (vm-serial-get-mail "nonexistent-mail-xyz-12345"))))

;;; Interactive command tests

(ert-deftest vm-serial-test-commands-interactive ()
  "Test that serial commands are interactive."
  (should (commandp 'vm-serial-expand-tokens))
  (should (commandp 'vm-serial-yank-mail))
  (should (commandp 'vm-serial-send-mail))
  (should (commandp 'vm-serial-set-token))
  (should (commandp 'vm-serial-get-token))
  (should (commandp 'vm-serial-insert-token)))

;;; Variable existence tests

(ert-deftest vm-serial-test-variables-exist ()
  "Test that serial-related variables exist."
  (should (boundp 'vm-serial-token-alist))
  (should (boundp 'vm-serial-mails-alist))
  (should (boundp 'vm-serial-cookie))
  (should (boundp 'vm-serial-fcc))
  (should (boundp 'vm-serial-mail-signature))
  (should (boundp 'vm-serial-unknown-to)))

(provide 'vm-serial-test)

;;; vm-serial-test.el ends here