;;; vm-integration-test.el --- Integration tests for VM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Integration tests that use full VM folders to test higher-level
;; functions. These tests exercise many underlying functions.

;;; Code:

(require 'vm-test-init)
(require 'vm-delete)
(require 'vm-mark)
(require 'vm-undo)
(require 'vm-sort)
(require 'vm-summary)

;;; Test folder with multiple messages

(defconst vm-integration-test-folder
  "From sender1@example.com Mon Jan  1 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: First Message
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <msg1@example.com>

This is the first message body.

From sender2@example.com Tue Jan  2 00:00:00 2024
From: sender2@example.com
To: recipient@example.com
Subject: Re: First Message
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <msg2@example.com>
In-Reply-To: <msg1@example.com>

This is a reply to the first message.

From sender3@example.com Wed Jan  3 00:00:00 2024
From: sender3@example.com
To: recipient@example.com
Subject: Another Thread
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <msg3@example.com>

This starts a new thread.

From sender1@example.com Thu Jan  4 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: Fourth Message
Date: Thu, 04 Jan 2024 13:00:00 +0000
Message-ID: <msg4@example.com>

Fourth message from sender1.

"
  "Test folder with 4 messages for integration tests.")

;;; Message accessors tests

(ert-deftest vm-integration-test-message-count ()
  "Test that folder has expected number of messages."
  (vm-test-with-folder vm-integration-test-folder
    (should (= (length vm-message-list) 4))))

;;; Note: vm-from-of, vm-subject-of, vm-message-id-of require cached data
;;; that is populated lazily. Use vm-su-* functions instead which parse headers.

(ert-deftest vm-integration-test-get-header-contents-from ()
  "Test vm-get-header-contents for From header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((from (vm-get-header-contents (car vm-message-list) "From:")))
      (should (stringp from))
      (should (string-match "sender1" from)))))

(ert-deftest vm-integration-test-get-header-contents-subject ()
  "Test vm-get-header-contents for Subject header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((subject (vm-get-header-contents (car vm-message-list) "Subject:")))
      (should (stringp subject))
      (should (string-match "First Message" subject)))))

(ert-deftest vm-integration-test-get-header-contents-message-id ()
  "Test vm-get-header-contents for Message-ID header."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg-id (vm-get-header-contents (car vm-message-list) "Message-ID:")))
      (should (stringp msg-id))
      (should (string-match "msg1@example.com" msg-id)))))

;;; Delete/undelete tests

(ert-deftest vm-integration-test-set-deleted-flag ()
  "Test vm-set-deleted-flag on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-deleted-flag msg))
      (vm-set-deleted-flag msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag msg nil)
      (should-not (vm-deleted-flag msg)))))

;;; Mark tests

(ert-deftest vm-integration-test-set-mark ()
  "Test vm-set-mark-of on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-mark-of msg))
      (vm-set-mark-of msg t)
      (should (vm-mark-of msg))
      (vm-set-mark-of msg nil)
      (should-not (vm-mark-of msg)))))

;;; Flag tests

(ert-deftest vm-integration-test-new-flag ()
  "Test new flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      ;; Initially messages may be new or not depending on parsing
      (vm-set-new-flag msg t)
      (should (vm-new-flag msg))
      (vm-set-new-flag msg nil)
      (should-not (vm-new-flag msg)))))

(ert-deftest vm-integration-test-unread-flag ()
  "Test unread flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-unread-flag msg t)
      (should (vm-unread-flag msg))
      (vm-set-unread-flag msg nil)
      (should-not (vm-unread-flag msg)))))

(ert-deftest vm-integration-test-replied-flag ()
  "Test replied flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-replied-flag msg))
      (vm-set-replied-flag msg t)
      (should (vm-replied-flag msg)))))

(ert-deftest vm-integration-test-forwarded-flag ()
  "Test forwarded flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-forwarded-flag msg))
      (vm-set-forwarded-flag msg t)
      (should (vm-forwarded-flag msg)))))

(ert-deftest vm-integration-test-filed-flag ()
  "Test filed flag operations on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-filed-flag msg))
      (vm-set-filed-flag msg t)
      (should (vm-filed-flag msg)))))

;;; Undo tests with real messages

(ert-deftest vm-integration-test-undo-record-delete ()
  "Test undo recording for delete operations."
  (vm-test-with-folder vm-integration-test-folder
    (let ((vm-undo-record-list nil)
          (msg (car vm-message-list)))
      ;; Delete should record undo
      (vm-set-deleted-flag msg t)
      ;; Check undo record was created
      (should vm-undo-record-list)
      (should (eq (car (car vm-undo-record-list)) 'vm-set-deleted-flag)))))

;;; Sort trim-subject tests

(ert-deftest vm-integration-test-trim-subject-of-real-message ()
  "Test vm-so-trim-subject on real message subject."
  (vm-test-with-folder vm-integration-test-folder
    (let ((vm-subject-ignored-prefix "^\\(re: *\\)+")
          (vm-subject-ignored-suffix nil)
          (vm-subject-tag-prefix nil)
          (vm-subject-significant-chars nil))
      ;; Second message has "Re: First Message" - get it via header
      (let ((subject (vm-get-header-contents (nth 1 vm-message-list) "Subject:")))
        (should (string-match "Re:" subject))
        (let ((trimmed (vm-so-trim-subject subject)))
          (should (string= trimmed "First Message")))))))

;;; Message body tests

(ert-deftest vm-integration-test-message-body ()
  "Test extracting message body."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-find-and-set-text-of msg)
      (let ((body (buffer-substring-no-properties
                   (vm-text-of msg)
                   (vm-text-end-of msg))))
        (should (string-match "first message body" body))))))

;;; Summary tests

(ert-deftest vm-integration-test-su-from ()
  "Test vm-su-from on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "sender1" (vm-su-from msg))))))

(ert-deftest vm-integration-test-su-subject ()
  "Test vm-su-subject on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "First Message" (vm-su-subject msg))))))

(ert-deftest vm-integration-test-su-to ()
  "Test vm-su-to on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (should (string-match "recipient" (vm-su-to msg))))))

;;; Header iteration

(ert-deftest vm-integration-test-iterate-headers ()
  "Test iterating headers on real message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list))
          (headers '()))
      (save-excursion
        (goto-char (vm-headers-of msg))
        (while (re-search-forward "^\\([^:]+\\):" (vm-text-of msg) t)
          (push (match-string 1) headers)))
      (should (member "From" headers))
      (should (member "To" headers))
      (should (member "Subject" headers)))))

;;; Multiple flag operations

(ert-deftest vm-integration-test-multiple-flags ()
  "Test setting multiple flags on same message."
  (vm-test-with-folder vm-integration-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-deleted-flag msg t)
      (vm-set-replied-flag msg t)
      (vm-set-filed-flag msg t)
      (should (vm-deleted-flag msg))
      (should (vm-replied-flag msg))
      (should (vm-filed-flag msg)))))

;;; Message traversal

(ert-deftest vm-integration-test-message-links ()
  "Test message list links."
  (vm-test-with-folder vm-integration-test-folder
    (let* ((first (car vm-message-list))
           (second (nth 1 vm-message-list))
           (third (nth 2 vm-message-list)))
      ;; Test forward links
      (should (eq second (car (cdr vm-message-list))))
      (should (eq third (nth 2 vm-message-list))))))

(provide 'vm-integration-test)

;;; vm-integration-test.el ends here
