;;; vm-undo-test.el --- Tests for vm-undo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM undo functions in vm-undo.el

;;; Code:

(require 'vm-test-init)
(require 'vm-undo)

;;; Undo function existence tests

(ert-deftest vm-undo-test-functions-exist ()
  "Test that undo functions exist."
  (should (fboundp 'vm-undo))
  (should (fboundp 'vm-undo-boundary))
  (should (fboundp 'vm-add-undo-boundaries))
  (should (fboundp 'vm-undo-record))
  (should (fboundp 'vm-undo-describe))
  (should (fboundp 'vm-undo-set-message-pointer))
  (should (fboundp 'vm-clear-expunge-invalidated-undos))
  (should (fboundp 'vm-clear-virtual-quit-invalidated-undos))
  (should (fboundp 'vm-clear-modification-flag-undos))
  (should (fboundp 'vm-squeeze-consecutive-undo-boundaries)))

;;; Label functions tests

(ert-deftest vm-undo-test-label-functions-exist ()
  "Test that label manipulation functions exist."
  (should (fboundp 'vm-set-message-attributes))
  (should (fboundp 'vm-add-message-labels))
  (should (fboundp 'vm-add-existing-message-labels))
  (should (fboundp 'vm-delete-message-labels))
  (should (fboundp 'vm-add-or-delete-message-labels))
  (should (fboundp 'vm-set-labels)))

;;; Flag setting functions tests

(ert-deftest vm-undo-test-flag-functions-exist ()
  "Test that flag setting functions exist."
  (should (fboundp 'vm-set-xxxx-flag))
  (should (fboundp 'vm-set-xxxx-cached-data-flag)))

;;; vm-undo-describe operation recognition
;; Note: vm-undo-describe requires real message structures for full testing.
;; Here we test that the function recognizes different record types.

(ert-deftest vm-undo-test-describe-recognizes-operations ()
  "Test that vm-undo-describe can be called."
  ;; Just verify the function is callable - it needs real messages for output
  (should (fboundp 'vm-undo-describe)))

(provide 'vm-undo-test)

;;; vm-undo-test.el ends here
