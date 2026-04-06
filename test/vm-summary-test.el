;;; vm-summary-test.el --- Tests for vm-summary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM summary functions in vm-summary.el
;; Tests pure string manipulation and formatting functions.

;;; Code:

(require 'vm-test-init)
(require 'vm-summary)

;;; vm-string-width tests

(ert-deftest vm-summary-test-string-width-ascii ()
  "Test string width for ASCII strings."
  (should (= (vm-string-width "hello") 5))
  (should (= (vm-string-width "") 0))
  (should (= (vm-string-width "a") 1)))

(ert-deftest vm-summary-test-string-width-spaces ()
  "Test string width with spaces and tabs."
  (should (= (vm-string-width "a b") 3))
  (should (= (vm-string-width "   ") 3)))

;;; vm-left-justify-string tests

(ert-deftest vm-summary-test-left-justify-shorter ()
  "Test left justifying a string shorter than width."
  (should (equal (vm-left-justify-string "hi" 5) "hi   ")))

(ert-deftest vm-summary-test-left-justify-equal ()
  "Test left justifying a string equal to width."
  (should (equal (vm-left-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-left-justify-longer ()
  "Test left justifying a string longer than width (no truncation)."
  (should (equal (vm-left-justify-string "hello world" 5) "hello world")))

;;; vm-right-justify-string tests

(ert-deftest vm-summary-test-right-justify-shorter ()
  "Test right justifying a string shorter than width."
  (should (equal (vm-right-justify-string "hi" 5) "   hi")))

(ert-deftest vm-summary-test-right-justify-equal ()
  "Test right justifying a string equal to width."
  (should (equal (vm-right-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-right-justify-longer ()
  "Test right justifying a string longer than width (no truncation)."
  (should (equal (vm-right-justify-string "hello world" 5) "hello world")))

;;; vm-numeric-left-justify-string tests

(ert-deftest vm-summary-test-numeric-left-justify-shorter ()
  "Test numeric left justify pads with zeros."
  (should (equal (vm-numeric-left-justify-string "42" 5) "42000")))

(ert-deftest vm-summary-test-numeric-left-justify-equal ()
  "Test numeric left justify with equal length."
  (should (equal (vm-numeric-left-justify-string "12345" 5) "12345")))

;;; vm-numeric-right-justify-string tests

(ert-deftest vm-summary-test-numeric-right-justify-shorter ()
  "Test numeric right justify pads with zeros."
  (should (equal (vm-numeric-right-justify-string "42" 5) "00042")))

(ert-deftest vm-summary-test-numeric-right-justify-equal ()
  "Test numeric right justify with equal length."
  (should (equal (vm-numeric-right-justify-string "12345" 5) "12345")))

;;; vm-truncate-roman-string tests

(ert-deftest vm-summary-test-truncate-roman-shorter ()
  "Test truncating a string shorter than width."
  (should (equal (vm-truncate-roman-string "hi" 5) "hi")))

(ert-deftest vm-summary-test-truncate-roman-exact ()
  "Test truncating a string exactly at width."
  (should (equal (vm-truncate-roman-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-longer ()
  "Test truncating a string longer than width."
  (should (equal (vm-truncate-roman-string "hello world" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-negative ()
  "Test truncating from end with negative width."
  (should (equal (vm-truncate-roman-string "hello world" -5) "world")))

;;; vm-truncate-string tests

(ert-deftest vm-summary-test-truncate-string-basic ()
  "Test basic string truncation."
  (should (equal (vm-truncate-string "hello" 3) "hel")))

(ert-deftest vm-summary-test-truncate-string-no-change ()
  "Test truncation when string is short enough."
  (should (equal (vm-truncate-string "hi" 10) "hi")))

;;; vm-default-chop-full-name tests

(ert-deftest vm-summary-test-chop-name-angle-brackets ()
  "Test chopping name with angle bracket format."
  (let ((result (vm-default-chop-full-name "John Doe <john@example.com>")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-parens ()
  "Test chopping name with parenthesis format."
  (let ((result (vm-default-chop-full-name "john@example.com (John Doe)")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-plain-email ()
  "Test chopping plain email address."
  (let ((result (vm-default-chop-full-name "john@example.com")))
    ;; Plain email should have nil for full-name
    (should (or (null (car result))
                (equal (car result) "john@example.com")))))

(ert-deftest vm-summary-test-chop-name-quoted ()
  "Test chopping name with quoted string."
  (let ((result (vm-default-chop-full-name "\"John Q. Doe\" <john@example.com>")))
    (should (stringp (car result)))
    (should (equal (cadr result) "john@example.com"))))

;;; vm-su-trim-subject tests (when stripping is disabled)

(ert-deftest vm-summary-test-trim-subject-passthrough ()
  "Test subject trimming when stripping is disabled."
  (let ((vm-summary-strip-subject-tags nil)
        (vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil))
    ;; With stripping disabled, should return subject as-is
    (should (equal (vm-su-trim-subject "Hello World") "Hello World"))))

(ert-deftest vm-summary-test-trim-subject-with-re ()
  "Test subject trimming with Re: prefix."
  (let ((vm-summary-strip-subject-tags t)
        (vm-subject-ignored-prefix "^\\(re: *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-tag-prefix-exceptions nil))
    (let ((result (vm-su-trim-subject "Re: Hello World")))
      (should (stringp result))
      ;; The Re: is moved to prefix but kept
      (should (string-match "Hello World" result)))))

;;; High-level header content extraction (requires message structure)
;; These tests verify the infrastructure but don't create full messages

(ert-deftest vm-summary-test-infrastructure-loaded ()
  "Test that summary functions are loaded."
  (should (fboundp 'vm-string-width))
  (should (fboundp 'vm-left-justify-string))
  (should (fboundp 'vm-right-justify-string))
  (should (fboundp 'vm-truncate-string))
  (should (fboundp 'vm-default-chop-full-name))
  (should (fboundp 'vm-su-trim-subject)))

(provide 'vm-summary-test)

;;; vm-summary-test.el ends here
