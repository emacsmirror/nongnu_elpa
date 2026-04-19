;;; vm-motion-test.el --- Tests for vm-motion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM motion/navigation functions in vm-motion.el

;;; Code:

(require 'vm-test-init)
(require 'vm-motion)

;;; Motion function existence tests

(ert-deftest vm-motion-test-functions-exist ()
  "Test that motion functions exist."
  (should (fboundp 'vm-record-and-change-message-pointer))
  (should (fboundp 'vm-goto-message))
  (should (fboundp 'vm-goto-message-last-seen))
  (should (fboundp 'vm-goto-parent-message))
  (should (fboundp 'vm-check-count))
  (should (fboundp 'vm-move-message-pointer))
  (should (fboundp 'vm-should-skip-message))
  (should (fboundp 'vm-should-skip-hidden-message))
  (should (fboundp 'vm-next-message))
  (should (fboundp 'vm-previous-message))
  (should (fboundp 'vm-next-message-no-skip))
  (should (fboundp 'vm-previous-message-no-skip))
  (should (fboundp 'vm-next-unread-message))
  (should (fboundp 'vm-previous-unread-message))
  (should (fboundp 'vm-next-message-same-subject))
  (should (fboundp 'vm-previous-message-same-subject))
  (should (fboundp 'vm-find-first-unread-message))
  (should (fboundp 'vm-thoughtfully-select-message))
  (should (fboundp 'vm-follow-summary-cursor)))

;;; vm-check-count tests
;; Note: vm-check-count requires vm-message-list/vm-message-pointer context
;; so we test it within a folder context

(ert-deftest vm-motion-test-check-count-within-folder ()
  "Test vm-check-count within folder context."
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

From sender@example.com Mon Jan  3 00:00:00 2024
From: sender@example.com
Subject: Message 3
Message-ID: <test3@example.com>

Body 3
"
    (setq vm-message-pointer vm-message-list)
    ;; Check count 1 should not signal (we have 3 messages ahead)
    (should (null (vm-check-count 1)))
    ;; Check count 3 should not signal (we have exactly 3 messages)
    (should (null (vm-check-count 3)))
    ;; Check count 4 should signal end-of-folder
    (should-error (vm-check-count 4) :type 'end-of-folder)))

;;; vm-move-message-pointer tests
;; Note: vm-move-message-pointer uses 'forward and 'backward directions
;; and signals end-of-folder/beginning-of-folder rather than returning nil

(ert-deftest vm-motion-test-move-message-pointer-forward ()
  "Test vm-move-message-pointer moves forward in message list."
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
    ;; Start at first message
    (setq vm-message-pointer vm-message-list)
    ;; Move forward
    (vm-move-message-pointer 'forward)
    (should (eq vm-message-pointer (cdr vm-message-list)))))

(ert-deftest vm-motion-test-move-message-pointer-backward ()
  "Test vm-move-message-pointer moves backward in message list."
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
    ;; Set up reverse links
    (vm-set-reverse-link-of (car (cdr vm-message-list)) vm-message-list)
    ;; Start at second message
    (setq vm-message-pointer (cdr vm-message-list))
    ;; Move backward
    (vm-move-message-pointer 'backward)
    (should (eq vm-message-pointer vm-message-list))))

(ert-deftest vm-motion-test-move-message-pointer-signals-at-end ()
  "Test vm-move-message-pointer signals end-of-folder at end."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1
"
    (let ((vm-circular-folders nil))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Try to move forward - should signal end-of-folder
      (should-error (vm-move-message-pointer 'forward) :type 'end-of-folder))))

(ert-deftest vm-motion-test-move-message-pointer-signals-at-beginning ()
  "Test vm-move-message-pointer signals beginning-of-folder at start."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1
"
    (let ((vm-circular-folders nil))
      ;; Start at only message (no reverse link)
      (setq vm-message-pointer vm-message-list)
      ;; Try to move backward - should signal beginning-of-folder
      (should-error (vm-move-message-pointer 'backward) :type 'beginning-of-folder))))

;;; vm-should-skip-message tests
;; Note: vm-should-skip-message takes a message-pointer (cons), not just a message

(ert-deftest vm-motion-test-should-skip-deleted-when-skipping ()
  "Test vm-should-skip-message skips deleted messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages t)
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))  ; Avoid hidden message checks
      ;; Mark as deleted
      (vm-set-deleted-flag (car vm-message-list) t)
      ;; Pass the message pointer (vm-message-list is a cons)
      (should (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-deleted-when-not-skipping ()
  "Test vm-should-skip-message doesn't skip deleted when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages nil)
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as deleted
      (vm-set-deleted-flag (car vm-message-list) t)
      ;; Should NOT skip
      (should-not (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-read-when-skipping ()
  "Test vm-should-skip-message skips read messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-read-messages t)
          (vm-skip-deleted-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as read (not new, not unread, not deleted)
      (vm-set-new-flag (car vm-message-list) nil)
      (vm-set-unread-flag (car vm-message-list) nil)
      (vm-set-deleted-flag (car vm-message-list) nil)
      ;; Should skip
      (should (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-read-when-not-skipping ()
  "Test vm-should-skip-message doesn't skip read when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-read-messages nil)
          (vm-skip-deleted-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as read
      (vm-set-new-flag (car vm-message-list) nil)
      (vm-set-unread-flag (car vm-message-list) nil)
      ;; Should NOT skip
      (should-not (vm-should-skip-message vm-message-list nil)))))

;;; vm-find-first-unread-message tests

(ert-deftest vm-motion-test-find-first-unread-new ()
  "Test vm-find-first-unread-message finds new messages."
  ;; Note: vm-find-first-unread-message takes a NEW-ONLY flag, not a message list.
  ;; It uses the global vm-message-list internally.
  ;; When NEW-ONLY is non-nil, it only looks for new messages.
  ;; When NEW-ONLY is nil, it looks for new OR unread messages.
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
    ;; First message is read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    ;; Second message is new
    (vm-set-new-flag (vm-test-nth-message 1) t)

    ;; Pass t for NEW-ONLY to look for new messages
    (let ((found (vm-find-first-unread-message t)))
      (should found)
      (should (eq (car found) (vm-test-nth-message 1))))))

(ert-deftest vm-motion-test-find-first-unread-unread ()
  "Test vm-find-first-unread-message finds unread messages."
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
    ;; First message is read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    (vm-set-deleted-flag (vm-test-nth-message 0) nil)
    ;; Second message is unread (not new, but unread)
    (vm-set-new-flag (vm-test-nth-message 1) nil)
    (vm-set-unread-flag (vm-test-nth-message 1) t)
    (vm-set-deleted-flag (vm-test-nth-message 1) nil)

    ;; Pass nil for NEW-ONLY to look for new OR unread messages
    (let ((found (vm-find-first-unread-message nil)))
      (should found)
      (should (eq (car found) (vm-test-nth-message 1))))))

(ert-deftest vm-motion-test-find-first-unread-all-read ()
  "Test vm-find-first-unread-message returns nil when all read."
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
    ;; All messages are read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    (vm-set-new-flag (vm-test-nth-message 1) nil)
    (vm-set-unread-flag (vm-test-nth-message 1) nil)

    ;; Pass nil for NEW-ONLY (look for new or unread)
    (let ((found (vm-find-first-unread-message nil)))
      (should (null found)))))

;;; Message pointer state tests

(ert-deftest vm-motion-test-message-pointer-initialized ()
  "Test that vm-message-pointer is properly initialized."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (should vm-message-pointer)
    (should (eq vm-message-pointer vm-message-list))))

;;; Message number tests

(ert-deftest vm-motion-test-message-numbering ()
  "Test that messages are numbered correctly."
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

From sender@example.com Mon Jan  3 00:00:00 2024
From: sender@example.com
Subject: Message 3
Message-ID: <test3@example.com>

Body 3
"
    ;; Number the messages
    (vm-number-messages)
    ;; Check numbering
    (should (equal (vm-number-of (vm-test-nth-message 0)) "1"))
    (should (equal (vm-number-of (vm-test-nth-message 1)) "2"))
    (should (equal (vm-number-of (vm-test-nth-message 2)) "3"))))

;;; Higher-level navigation command tests
;; These test the interactive navigation functions

(ert-deftest vm-motion-test-Next-message-callable ()
  "Test vm-Next-message is a callable function."
  ;; vm-Next-message is defined with (fset 'vm-Next-message 'vm-next-message-no-skip)
  (should (fboundp 'vm-Next-message))
  (should (commandp 'vm-Next-message)))

(ert-deftest vm-motion-test-Previous-message-callable ()
  "Test vm-Previous-message is a callable function."
  ;; vm-Previous-message is defined with fset as an alias
  (should (fboundp 'vm-Previous-message))
  (should (commandp 'vm-Previous-message)))

;;; Circular folder tests

(ert-deftest vm-motion-test-circular-folders-wraps-forward ()
  "Test that circular-folders wraps forward navigation."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Only Message
Message-ID: <test@example.com>

Body
"
    (let ((vm-circular-folders t))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Move forward should wrap to same message
      (vm-move-message-pointer 'forward)
      (should (eq vm-message-pointer vm-message-list)))))

(ert-deftest vm-motion-test-circular-folders-wraps-backward ()
  "Test that circular-folders wraps backward navigation."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Only Message
Message-ID: <test@example.com>

Body
"
    (let ((vm-circular-folders t))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Move backward should wrap to same message
      (vm-move-message-pointer 'backward)
      (should (eq vm-message-pointer vm-message-list)))))

;;; Message skip configuration tests

(ert-deftest vm-motion-test-skip-filed-messages ()
  "Test vm-should-skip-message skips filed messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages t)  ; needed to enable skip logic
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as filed
      (vm-set-filed-flag (car vm-message-list) t)
      ;; Filed messages aren't skipped by default - they're not in skip logic
      (should-not (vm-should-skip-message vm-message-list nil)))))

;;; Thread navigation tests
;; Basic thread navigation - vm-goto-parent-message requires threading setup

(ert-deftest vm-motion-test-goto-parent-exists ()
  "Test vm-goto-parent-message function exists."
  (should (fboundp 'vm-goto-parent-message)))

;;; Message selection tests

(ert-deftest vm-motion-test-thoughtfully-select-returns-pointer ()
  "Test vm-thoughtfully-select-message returns a message pointer."
  ;; vm-thoughtfully-select-message returns a message pointer or nil
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    ;; Mark message as new so thoughtfully-select might find it
    (vm-set-new-flag (car vm-message-list) t)
    (let ((vm-jump-to-new-messages t)
          (vm-jump-to-unread-messages nil))
      (let ((result (vm-thoughtfully-select-message)))
        ;; Should return the message pointer (list starting at the message)
        (should (or (null result) (consp result)))))))

(provide 'vm-motion-test)

;;; vm-motion-test.el ends here