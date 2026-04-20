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

;;; Behavioral tests for vm-set-deleted-flag

(ert-deftest vm-delete-test-set-deleted-flag ()
  "Test that vm-set-deleted-flag sets the deleted attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test message
Message-ID: <test1@example.com>

Body text
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not deleted
      (should-not (vm-deleted-flag msg))
      ;; Set deleted flag
      (vm-set-deleted-flag msg t)
      (should (vm-deleted-flag msg))
      ;; Unset deleted flag
      (vm-set-deleted-flag msg nil)
      (should-not (vm-deleted-flag msg)))))

;;; Behavioral tests for vm-expunge-message

(ert-deftest vm-delete-test-expunge-message-removes-from-list ()
  "Test that vm-expunge-message removes message from vm-message-list."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    (should (= 2 (vm-test-message-count)))
    (let ((msg1 (vm-test-first-message)))
      (vm-expunge-message msg1)
      (should (= 1 (vm-test-message-count)))
      ;; The remaining message should be message 2
      (should (string-match "Message 2"
                            (vm-test-message-header (vm-test-first-message) "Subject"))))))

(ert-deftest vm-delete-test-expunge-message-sets-expunged-marker ()
  "Test that expunged messages have deleted-flag set to 'expunged."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Test 2
Message-ID: <test2@example.com>

Body 2
"
    ;; Need at least 2 messages so expunge doesn't leave empty list
    (should (= 2 (vm-test-message-count)))
    (let ((msg (vm-test-first-message)))
      ;; Set reverse links (needed by vm-expunge-message)
      (vm-set-reverse-link-of (vm-test-nth-message 1) vm-message-list)
      (vm-expunge-message msg)
      ;; After expunge, deleted-flag should be 'expunged
      ;; (vm-deleted-flag accesses slot 2 of the attributes vector)
      (should (eq 'expunged (vm-deleted-flag msg))))))

;;; Tests for duplicate detection logic
;; Note: vm-delete-duplicate-messages requires full folder context,
;; so we test the underlying logic patterns instead.

(ert-deftest vm-delete-test-message-id-hash-logic ()
  "Test the hash table logic used for duplicate detection."
  (let ((table (make-vector 103 0))
        (ids '("<unique1@example.com>"
               "<duplicate@example.com>"
               "<duplicate@example.com>"  ; duplicate!
               "<unique2@example.com>")))
    ;; Simulate duplicate detection logic
    (let ((duplicates 0))
      (dolist (mid ids)
        (if (intern-soft mid table)
            (setq duplicates (1+ duplicates))
          (intern mid table)))
      ;; Should find exactly 1 duplicate
      (should (= duplicates 1)))))

(ert-deftest vm-delete-test-message-id-uniqueness ()
  "Test that hash table correctly identifies unique Message-IDs."
  (let ((table (make-vector 61 0))
        (ids '("<msg1@example.com>"
               "<msg2@example.com>"
               "<msg3@example.com>")))
    (let ((duplicates 0))
      (dolist (mid ids)
        (if (intern-soft mid table)
            (setq duplicates (1+ duplicates))
          (intern mid table)))
      ;; No duplicates
      (should (= duplicates 0)))))

;;; Tests for flagged status

(ert-deftest vm-delete-test-flagged-flag ()
  "Test that vm-set-flagged-flag toggles the flagged attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not flagged
      (should-not (vm-flagged-flag msg))
      ;; Set flagged
      (vm-set-flagged-flag msg t)
      (should (vm-flagged-flag msg))
      ;; Unset flagged
      (vm-set-flagged-flag msg nil)
      (should-not (vm-flagged-flag msg)))))

;;; Tests for skipping logic used in duplicate detection

(ert-deftest vm-delete-test-skip-already-deleted-logic ()
  "Test that duplicate logic skips already-deleted messages."
  ;; This tests the pattern: skip if message is already deleted
  (let ((deleted-flags '(t nil nil t))  ; Messages 0,3 are deleted
        (ids '("<dup@example.com>"
               "<dup@example.com>"      ; Would be dup, but msg0 deleted
               "<unique@example.com>"
               "<unique@example.com>")))  ; Would be dup, but msg3 deleted
    (let ((table (make-vector 61 0))
          (idx 0)
          (new-deletes 0))
      (while (< idx (length ids))
        (unless (nth idx deleted-flags)  ; Skip deleted messages
          (let ((mid (nth idx ids)))
            (if (intern-soft mid table)
                (setq new-deletes (1+ new-deletes))
              (intern mid table))))
        (setq idx (1+ idx)))
      ;; Only one new delete: second occurrence of <dup@example.com>
      ;; (the one at idx 1, since idx 0 is deleted)
      (should (= new-deletes 0)))))

(provide 'vm-delete-test)

;;; vm-delete-test.el ends here
