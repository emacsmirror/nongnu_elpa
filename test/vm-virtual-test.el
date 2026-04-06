;;; vm-virtual-test.el --- Tests for vm-virtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM virtual folder functions in vm-virtual.el

;;; Code:

(require 'vm-test-init)
(require 'vm-virtual)

;;; vm-vs-any tests

(ert-deftest vm-virtual-test-vs-any ()
  "Test vm-vs-any always returns t."
  (should (eq (vm-vs-any nil) t))
  (should (eq (vm-vs-any 'anything) t)))

;;; vm-vs-not tests (with mock)

(ert-deftest vm-virtual-test-vs-not-negates ()
  "Test vm-vs-not negates selector result."
  (let ((vm-virtual-selector-function-alist
         '((any . vm-vs-any))))
    ;; not any = nil
    (should (null (vm-vs-not nil '(any))))))

;;; Selector function existence tests

(ert-deftest vm-virtual-test-selectors-exist ()
  "Test that virtual selector functions exist."
  (should (fboundp 'vm-vs-or))
  (should (fboundp 'vm-vs-and))
  (should (fboundp 'vm-vs-not))
  (should (fboundp 'vm-vs-any))
  (should (fboundp 'vm-vs-author))
  (should (fboundp 'vm-vs-recipient))
  (should (fboundp 'vm-vs-subject))
  (should (fboundp 'vm-vs-sent-before))
  (should (fboundp 'vm-vs-sent-after))
  (should (fboundp 'vm-vs-older-than))
  (should (fboundp 'vm-vs-newer-than))
  (should (fboundp 'vm-vs-outgoing))
  (should (fboundp 'vm-vs-attachment))
  (should (fboundp 'vm-vs-header))
  (should (fboundp 'vm-vs-text))
  (should (fboundp 'vm-vs-header-or-text)))

;;; vm-virtual-selector-function-alist tests

(ert-deftest vm-virtual-test-selector-alist-populated ()
  "Test that vm-virtual-selector-function-alist has entries."
  (should (assq 'and vm-virtual-selector-function-alist))
  (should (assq 'or vm-virtual-selector-function-alist))
  (should (assq 'not vm-virtual-selector-function-alist))
  (should (assq 'any vm-virtual-selector-function-alist))
  (should (assq 'author vm-virtual-selector-function-alist))
  (should (assq 'subject vm-virtual-selector-function-alist))
  (should (assq 'recipient vm-virtual-selector-function-alist)))

;;; Virtual folder creation function existence

(ert-deftest vm-virtual-test-creation-functions-exist ()
  "Test that virtual folder creation functions exist."
  (should (fboundp 'vm-create-virtual-folder))
  (should (fboundp 'vm-create-virtual-folder-same-subject))
  (should (fboundp 'vm-create-virtual-folder-same-author))
  (should (fboundp 'vm-apply-virtual-folder)))

;;; vm-vs-sexp tests

(ert-deftest vm-virtual-test-vs-sexp-delegates-to-and ()
  "Test vm-vs-sexp delegates to vm-vs-and."
  (let ((vm-virtual-selector-function-alist
         '((any . vm-vs-any))))
    ;; sexp with (any) should return t
    (should (vm-vs-sexp nil '((any))))))

(provide 'vm-virtual-test)

;;; vm-virtual-test.el ends here
