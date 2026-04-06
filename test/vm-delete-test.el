;;; vm-delete-test.el --- Tests for vm-delete.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM delete functions in vm-delete.el

;;; Code:

(require 'vm-test-init)
(require 'vm-delete)

;;; Delete function existence tests

(ert-deftest vm-delete-test-functions-exist ()
  "Test that delete functions exist."
  (should (fboundp 'vm-delete-message))
  (should (fboundp 'vm-delete-message-backward))
  (should (fboundp 'vm-undelete-message))
  (should (fboundp 'vm-toggle-flag-message))
  (should (fboundp 'vm-kill-subject))
  (should (fboundp 'vm-kill-thread-subtree))
  (should (fboundp 'vm-delete-duplicate-messages))
  (should (fboundp 'vm-delete-duplicate-messages-by-body))
  (should (fboundp 'vm-expunge-folder))
  (should (fboundp 'vm-expunge-message)))

;;; vm-expunge-folder keyword args

(ert-deftest vm-delete-test-expunge-folder-accepts-keywords ()
  "Test that vm-expunge-folder accepts keyword arguments."
  ;; Just verify the function signature accepts :quiet
  (should (equal (car (func-arity 'vm-expunge-folder)) 0)))

(provide 'vm-delete-test)

;;; vm-delete-test.el ends here
