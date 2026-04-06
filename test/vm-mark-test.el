;;; vm-mark-test.el --- Tests for vm-mark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM mark functions in vm-mark.el

;;; Code:

(require 'vm-test-init)
(require 'vm-mark)

;;; Mark function existence tests

(ert-deftest vm-mark-test-functions-exist ()
  "Test that mark functions exist."
  (should (fboundp 'vm-clear-all-marks))
  (should (fboundp 'vm-toggle-all-marks))
  (should (fboundp 'vm-mark-all-messages))
  (should (fboundp 'vm-mark-message))
  (should (fboundp 'vm-unmark-message))
  (should (fboundp 'vm-mark-summary-region))
  (should (fboundp 'vm-unmark-summary-region))
  (should (fboundp 'vm-mark-messages-by-selector))
  (should (fboundp 'vm-unmark-messages-by-selector))
  (should (fboundp 'vm-mark-thread-subtree))
  (should (fboundp 'vm-unmark-thread-subtree))
  (should (fboundp 'vm-mark-messages-same-subject))
  (should (fboundp 'vm-unmark-messages-same-subject))
  (should (fboundp 'vm-mark-messages-same-author))
  (should (fboundp 'vm-unmark-messages-same-author)))

;;; vm-mark-or-unmark helper tests

(ert-deftest vm-mark-test-or-unmark-functions-exist ()
  "Test that mark/unmark helper functions exist."
  (should (fboundp 'vm-mark-or-unmark-summary-region))
  (should (fboundp 'vm-mark-or-unmark-messages-by-selector))
  (should (fboundp 'vm-mark-or-unmark-thread-subtree))
  (should (fboundp 'vm-mark-or-unmark-messages-same-subject))
  (should (fboundp 'vm-mark-or-unmark-messages-same-author)))

(provide 'vm-mark-test)

;;; vm-mark-test.el ends here
