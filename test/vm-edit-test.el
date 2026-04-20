;;; vm-edit-test.el --- Tests for vm-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM edit functions in vm-edit.el

;;; Code:

(require 'vm-test-init)
(require 'vm-edit)

;;; Edit function existence tests

(ert-deftest vm-edit-test-functions-exist ()
  "Test that edit functions exist."
  (should (fboundp 'vm-edit-message))
  (should (fboundp 'vm-edit-message-other-frame))
  (should (fboundp 'vm-discard-cached-data))
  (should (fboundp 'vm-discard-cached-data-internal))
  (should (fboundp 'vm-edit-message-end))
  (should (fboundp 'vm-edit-message-abort)))

;;; vm-discard-cached-data-internal tests
;; Note: vm-discard-cached-data-internal requires full folder context.
;; These tests verify the underlying fillarray behavior on cached-data.

(ert-deftest vm-edit-test-fillarray-clears-cached-data ()
  "Test that fillarray clears cached data vector."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    ;; Set some cached values
    (vm-set-subject-of msg "Cached Subject")
    (vm-set-from-of msg "Cached From")
    (should (equal "Cached Subject" (vm-subject-of msg)))
    (should (equal "Cached From" (vm-from-of msg)))
    ;; Use fillarray like vm-discard-cached-data-internal does
    (fillarray (vm-cached-data-of msg) nil)
    ;; After fillarray, should be nil
    (should (null (vm-subject-of msg)))
    (should (null (vm-from-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-message-id ()
  "Test that fillarray clears cached message-id."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-message-id-of msg "<cached@example.com>")
    (should (equal "<cached@example.com>" (vm-message-id-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-message-id-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-byte-count ()
  "Test that fillarray clears cached byte count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-byte-count-of msg "12345")
    (should (equal "12345" (vm-byte-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-byte-count-of msg)))))

(ert-deftest vm-edit-test-fillarray-clears-line-count ()
  "Test that fillarray clears cached line count."
  (let* ((msg (vm-make-message))
         (cached-data (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of msg cached-data)
    (vm-set-line-count-of msg "100")
    (should (equal "100" (vm-line-count-of msg)))
    (fillarray (vm-cached-data-of msg) nil)
    (should (null (vm-line-count-of msg)))))

;;; Edit state tests

(ert-deftest vm-edit-test-edited-flag ()
  "Test that the edited flag can be set and read."
  (let* ((msg (vm-make-message))
         (attrs (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of msg attrs)
    ;; Initially not edited
    (should-not (vm-edited-flag msg))
    ;; Set edited flag directly in attributes vector
    (aset attrs 7 t)
    (should (vm-edited-flag msg))
    ;; Clear edited flag
    (aset attrs 7 nil)
    (should-not (vm-edited-flag msg))))

;;; Edit buffer tracking

(ert-deftest vm-edit-test-edit-buffer-of ()
  "Test vm-edit-buffer-of accessor."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message))
          (edit-buf (generate-new-buffer " *test-edit*")))
      (unwind-protect
          (progn
            ;; Initially no edit buffer
            (should (null (vm-edit-buffer-of msg)))
            ;; Set edit buffer
            (vm-set-edit-buffer-of msg edit-buf)
            (should (eq edit-buf (vm-edit-buffer-of msg)))
            ;; Clear edit buffer
            (vm-set-edit-buffer-of msg nil)
            (should (null (vm-edit-buffer-of msg))))
        (kill-buffer edit-buf)))))

(provide 'vm-edit-test)

;;; vm-edit-test.el ends here
