;;; vm-sort-test.el --- Tests for vm-sort.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM sort functions in vm-sort.el

;;; Code:

(require 'vm-test-init)
(require 'vm-sort)

;;; vm-so-trim-subject tests

(ert-deftest vm-sort-test-trim-subject-plain ()
  "Test trim-subject with plain subject."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello World") "Hello World"))))

(ert-deftest vm-sort-test-trim-subject-re-prefix ()
  "Test trim-subject strips Re: prefix."
  (let ((vm-subject-ignored-prefix "^\\(re: *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Re: Hello") "Hello"))
    (should (equal (vm-so-trim-subject "Re: Re: Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-fwd-prefix ()
  "Test trim-subject strips Fwd: prefix."
  (let ((vm-subject-ignored-prefix "^\\(\\(re\\|fwd?\\): *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Fwd: Hello") "Hello"))
    (should (equal (vm-so-trim-subject "Fw: Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-suffix ()
  "Test trim-subject strips suffix."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix " *(fwd)$")
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello (fwd)") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-tag-prefix ()
  "Test trim-subject strips tag prefix like [list]."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix "^\\[[^]]+\\] *")
        (vm-subject-tag-prefix-exceptions nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "[list] Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-significant-chars ()
  "Test trim-subject respects significant chars limit."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars 5))
    (should (equal (vm-so-trim-subject "Hello World") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-whitespace ()
  "Test trim-subject collapses whitespace."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello   World") "Hello World"))))

;;; Sort comparison function existence tests

(ert-deftest vm-sort-test-compare-functions-exist ()
  "Test that sort comparison functions exist."
  (should (fboundp 'vm-sort-compare-author))
  (should (fboundp 'vm-sort-compare-author-r))
  (should (fboundp 'vm-sort-compare-date))
  (should (fboundp 'vm-sort-compare-date-r))
  (should (fboundp 'vm-sort-compare-subject))
  (should (fboundp 'vm-sort-compare-subject-r))
  (should (fboundp 'vm-sort-compare-recipients))
  (should (fboundp 'vm-sort-compare-recipients-r))
  (should (fboundp 'vm-sort-compare-line-count))
  (should (fboundp 'vm-sort-compare-line-count-r))
  (should (fboundp 'vm-sort-compare-byte-count))
  (should (fboundp 'vm-sort-compare-byte-count-r))
  (should (fboundp 'vm-sort-compare-physical-order))
  (should (fboundp 'vm-sort-compare-physical-order-r)))

;;; vm-supported-sort-keys tests

(ert-deftest vm-sort-test-keys-recognized ()
  "Test that sort keys are in vm-supported-sort-keys."
  (should (member "date" vm-supported-sort-keys))
  (should (member "reversed-date" vm-supported-sort-keys))
  (should (member "author" vm-supported-sort-keys))
  (should (member "subject" vm-supported-sort-keys))
  (should (member "recipients" vm-supported-sort-keys))
  (should (member "line-count" vm-supported-sort-keys))
  (should (member "byte-count" vm-supported-sort-keys))
  (should (member "physical-order" vm-supported-sort-keys)))

(provide 'vm-sort-test)

;;; vm-sort-test.el ends here
