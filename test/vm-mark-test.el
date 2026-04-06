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

;;; Tests using real messages

(defconst vm-mark-test-folder
  "From alice@example.com Mon Jan  1 00:00:00 2024
From: Alice <alice@example.com>
To: recipient@example.com
Subject: First Subject
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <mark1@example.com>

First message body.

From alice@example.com Tue Jan  2 00:00:00 2024
From: Alice <alice@example.com>
To: recipient@example.com
Subject: First Subject
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <mark2@example.com>

Second message body, same author and subject.

From bob@example.com Wed Jan  3 00:00:00 2024
From: Bob <bob@example.com>
To: recipient@example.com
Subject: Different Subject
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <mark3@example.com>

Third message from different author.

"
  "Test folder for mark and label tests.")

;;; vm-marked-messages tests

(ert-deftest vm-mark-test-marked-messages-empty ()
  "Test vm-marked-messages with no marks returns nil."
  (vm-test-with-folder vm-mark-test-folder
    (dolist (msg vm-message-list)
      (vm-set-mark-of msg nil))
    (should (null (vm-marked-messages)))))

(ert-deftest vm-mark-test-marked-messages-some ()
  "Test vm-marked-messages returns only marked messages."
  (vm-test-with-folder vm-mark-test-folder
    ;; Mark first and third messages
    (vm-set-mark-of (nth 0 vm-message-list) t)
    (vm-set-mark-of (nth 1 vm-message-list) nil)
    (vm-set-mark-of (nth 2 vm-message-list) t)
    (let ((marked (vm-marked-messages)))
      (should (= (length marked) 2))
      (should (memq (nth 0 vm-message-list) marked))
      (should-not (memq (nth 1 vm-message-list) marked))
      (should (memq (nth 2 vm-message-list) marked)))))

(ert-deftest vm-mark-test-marked-messages-all ()
  "Test vm-marked-messages when all are marked."
  (vm-test-with-folder vm-mark-test-folder
    (dolist (msg vm-message-list)
      (vm-set-mark-of msg t))
    (should (= (length (vm-marked-messages)) 3))))

;;; Label accessor tests

(ert-deftest vm-mark-test-labels-accessor ()
  "Test vm-labels-of accessor."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Initially no labels
      (vm-set-labels-of msg nil)
      (should (null (vm-labels-of msg)))
      ;; Set some labels
      (vm-set-labels-of msg '("work" "important"))
      (should (equal (vm-labels-of msg) '("work" "important"))))))

(ert-deftest vm-mark-test-label-string-of ()
  "Test vm-label-string-of returns labels as string."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Set labels via vm-set-labels-of
      (vm-set-labels-of msg '("work" "urgent"))
      ;; vm-label-string-of returns cached string from mirror-data
      ;; It must be explicitly set or retrieved via vm-su-labels
      ;; which computes it. Set it explicitly for this test.
      (vm-set-label-string-of msg "work urgent")
      (let ((label-str (vm-label-string-of msg)))
        (should (stringp label-str))
        (should (string-match "work" label-str))
        (should (string-match "urgent" label-str))))))

(ert-deftest vm-mark-test-label-string-empty ()
  "Test vm-label-string-of with no labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg nil)
      (let ((label-str (vm-label-string-of msg)))
        (should (or (null label-str) (equal label-str "")))))))

;;; Mark flag accessor tests

(ert-deftest vm-mark-test-mark-of-accessor ()
  "Test vm-mark-of and vm-set-mark-of."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg nil)
      (should (null (vm-mark-of msg)))
      (vm-set-mark-of msg t)
      (should (vm-mark-of msg)))))

;;; Mark same subject tests

(ert-deftest vm-mark-test-same-subject-found ()
  "Test finding messages with same subject."
  (vm-test-with-folder vm-mark-test-folder
    ;; First two messages have same subject
    (let ((msg1 (nth 0 vm-message-list))
          (msg2 (nth 1 vm-message-list)))
      (let ((subj1 (vm-su-subject msg1))
            (subj2 (vm-su-subject msg2)))
        (should (equal subj1 subj2))))))

;;; Mark same author tests

(ert-deftest vm-mark-test-same-author-found ()
  "Test finding messages with same author."
  (vm-test-with-folder vm-mark-test-folder
    ;; First two messages have same author (Alice)
    (let ((msg1 (nth 0 vm-message-list))
          (msg2 (nth 1 vm-message-list))
          (msg3 (nth 2 vm-message-list)))
      (let ((from1 (vm-su-from msg1))
            (from2 (vm-su-from msg2))
            (from3 (vm-su-from msg3)))
        (should (string-match "Alice" from1))
        (should (string-match "Alice" from2))
        (should (string-match "Bob" from3))))))

;;; Label manipulation tests

(ert-deftest vm-mark-test-add-label ()
  "Test adding a label to a message."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg nil)
      ;; Manually add a label
      (vm-set-labels-of msg (cons "new-label" (vm-labels-of msg)))
      (should (member "new-label" (vm-labels-of msg))))))

(ert-deftest vm-mark-test-multiple-labels ()
  "Test message can have multiple labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("label1" "label2" "label3"))
      (let ((labels (vm-labels-of msg)))
        (should (= (length labels) 3))
        (should (member "label1" labels))
        (should (member "label2" labels))
        (should (member "label3" labels))))))

(ert-deftest vm-mark-test-remove-label ()
  "Test removing a label from a message."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("keep" "remove" "also-keep"))
      (vm-set-labels-of msg (delete "remove" (vm-labels-of msg)))
      (should-not (member "remove" (vm-labels-of msg)))
      (should (member "keep" (vm-labels-of msg))))))

;;; vm-set-xxxx-flag accessor coverage
;; Note: flag accessors are named vm-xxx-flag not vm-xxx-flag-of

(ert-deftest vm-mark-test-flag-accessors ()
  "Test message flag accessors."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Test new flag
      (vm-set-new-flag-of msg t)
      (should (vm-new-flag msg))
      (vm-set-new-flag-of msg nil)
      (should-not (vm-new-flag msg))

      ;; Test deleted flag
      (vm-set-deleted-flag-of msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag-of msg nil)

      ;; Test replied flag
      (vm-set-replied-flag-of msg t)
      (should (vm-replied-flag msg))
      (vm-set-replied-flag-of msg nil)

      ;; Test forwarded flag
      (vm-set-forwarded-flag-of msg t)
      (should (vm-forwarded-flag msg))
      (vm-set-forwarded-flag-of msg nil)

      ;; Test flagged flag
      (vm-set-flagged-flag-of msg t)
      (should (vm-flagged-flag msg))
      (vm-set-flagged-flag-of msg nil))))

;;; Integration test with virtual selectors

(ert-deftest vm-mark-test-vs-label-integration ()
  "Test vm-vs-label selector with labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("important" "work"))
      (should (vm-vs-label msg "important"))
      (should (vm-vs-label msg "work"))
      (should-not (vm-vs-label msg "personal")))))

(provide 'vm-mark-test)

;;; vm-mark-test.el ends here
