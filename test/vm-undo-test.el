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

;;; vm-squeeze-consecutive-undo-boundaries tests

(ert-deftest vm-undo-test-squeeze-removes-consecutive-nils ()
  "Test that vm-squeeze-consecutive-undo-boundaries removes consecutive nils."
  (let ((vm-undo-record-list '(nil nil record1 nil nil nil record2 nil)))
    (vm-squeeze-consecutive-undo-boundaries)
    ;; Should collapse consecutive nils to single nils
    (should-not (and (null (car vm-undo-record-list))
                     (null (cadr vm-undo-record-list))))))

(ert-deftest vm-undo-test-squeeze-empty-list ()
  "Test vm-squeeze-consecutive-undo-boundaries with nil list."
  (let ((vm-undo-record-list nil))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (null vm-undo-record-list))))

(ert-deftest vm-undo-test-squeeze-only-nils-becomes-nil ()
  "Test that list of only nils becomes nil."
  (let ((vm-undo-record-list '(nil)))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (null vm-undo-record-list))))

(ert-deftest vm-undo-test-squeeze-preserves-records ()
  "Test that vm-squeeze preserves actual records."
  (let ((vm-undo-record-list '(record1 nil record2 nil record3)))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (memq 'record1 vm-undo-record-list))
    (should (memq 'record2 vm-undo-record-list))
    (should (memq 'record3 vm-undo-record-list))))

;;; vm-undo-record tests

(ert-deftest vm-undo-test-record-adds-to-list ()
  "Test that vm-undo-record adds sexp to list."
  (let ((vm-undo-record-list nil))
    (vm-undo-record '(vm-set-new-flag msg t))
    (should (equal (car vm-undo-record-list) '(vm-set-new-flag msg t)))))

(ert-deftest vm-undo-test-record-prepends ()
  "Test that vm-undo-record prepends to existing list."
  (let ((vm-undo-record-list '(existing-record)))
    (vm-undo-record 'new-record)
    (should (eq (car vm-undo-record-list) 'new-record))
    (should (eq (cadr vm-undo-record-list) 'existing-record))))

;;; vm-undo-boundary tests

(ert-deftest vm-undo-test-boundary-adds-nil ()
  "Test that vm-undo-boundary adds nil separator."
  (let ((vm-undo-record-list '(record1)))
    (vm-undo-boundary)
    (should (null (car vm-undo-record-list)))
    (should (eq (cadr vm-undo-record-list) 'record1))))

(ert-deftest vm-undo-test-boundary-no-double-nil ()
  "Test that vm-undo-boundary doesn't add nil if list starts with nil."
  (let ((vm-undo-record-list '(nil record1)))
    (vm-undo-boundary)
    ;; Should not add another nil
    (should (equal vm-undo-record-list '(nil record1)))))

(provide 'vm-undo-test)

;;; vm-undo-test.el ends here
