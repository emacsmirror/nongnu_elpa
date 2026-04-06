;;; vm-folder-test.el --- Tests for vm-folder.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM folder functions in vm-folder.el

;;; Code:

(require 'vm-test-init)
(require 'vm-folder)

;;; vm-get-folder-type tests

(ert-deftest vm-folder-test-type-empty-buffer ()
  "Test folder type detection on empty buffer."
  (with-temp-buffer
    (should (null (vm-get-folder-type)))))

(ert-deftest vm-folder-test-type-from-folder ()
  "Test From_ folder type detection."
  (let ((vm-default-From_-folder-type 'From_)
        (vm-trust-From_-with-Content-Length nil))
    (with-temp-buffer
      (insert "From VM Thu Jan  1 00:00:00 2024\n")
      (insert "From: sender@example.com\n")
      (insert "To: recipient@example.com\n")
      (insert "Subject: Test\n\n")
      (insert "Body\n")
      (should (eq (vm-get-folder-type) 'From_)))))

(ert-deftest vm-folder-test-type-mmdf-folder ()
  "Test MMDF folder type detection."
  (with-temp-buffer
    (insert "\001\001\001\001\n")
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert "Subject: Test\n\n")
    (insert "Body\n")
    (insert "\001\001\001\001\n")
    (should (eq (vm-get-folder-type) 'mmdf))))

(ert-deftest vm-folder-test-type-babyl-folder ()
  "Test BABYL folder type detection."
  (with-temp-buffer
    (insert "BABYL OPTIONS:\n")
    (insert "Version: 5\n")
    (should (eq (vm-get-folder-type) 'babyl))))

(ert-deftest vm-folder-test-type-unknown ()
  "Test unknown folder type detection."
  (with-temp-buffer
    (insert "Random content that doesn't match any folder type\n")
    (should (eq (vm-get-folder-type) 'unknown))))

;;; vm-leading-message-separator tests

(ert-deftest vm-folder-test-leading-separator-from ()
  "Test From_ leading separator."
  (let ((vm-folder-type 'From_))
    (let ((sep (vm-leading-message-separator)))
      (should (stringp sep))
      (should (string-match "^From VM " sep)))))

(ert-deftest vm-folder-test-leading-separator-mmdf ()
  "Test MMDF leading separator."
  (let ((vm-folder-type 'mmdf))
    (should (equal (vm-leading-message-separator) "\001\001\001\001\n"))))

(ert-deftest vm-folder-test-leading-separator-babyl-no-message ()
  "Test BABYL leading separator without message."
  (let ((vm-folder-type 'babyl))
    (let ((sep (vm-leading-message-separator)))
      (should (stringp sep))
      (should (string-match "^\014\n0," sep))
      (should (string-match "\\*\\*\\* EOOH \\*\\*\\*\n$" sep)))))

(ert-deftest vm-folder-test-leading-separator-explicit-type ()
  "Test leading separator with explicit folder type."
  (let ((vm-folder-type 'From_))  ; Current type is From_
    ;; But explicitly request mmdf separator
    (should (equal (vm-leading-message-separator 'mmdf) "\001\001\001\001\n"))))

;;; vm-trailing-message-separator tests

(ert-deftest vm-folder-test-trailing-separator-from ()
  "Test From_ trailing separator."
  (let ((vm-folder-type 'From_))
    (should (equal (vm-trailing-message-separator) "\n"))))

(ert-deftest vm-folder-test-trailing-separator-from-content-length ()
  "Test From_-with-Content-Length trailing separator."
  (let ((vm-folder-type 'From_-with-Content-Length))
    (should (equal (vm-trailing-message-separator) ""))))

(ert-deftest vm-folder-test-trailing-separator-bellFrom ()
  "Test BellFrom_ trailing separator."
  (let ((vm-folder-type 'BellFrom_))
    (should (equal (vm-trailing-message-separator) ""))))

(ert-deftest vm-folder-test-trailing-separator-mmdf ()
  "Test MMDF trailing separator."
  (let ((vm-folder-type 'mmdf))
    (should (equal (vm-trailing-message-separator) "\001\001\001\001\n"))))

(ert-deftest vm-folder-test-trailing-separator-babyl ()
  "Test BABYL trailing separator."
  (let ((vm-folder-type 'babyl))
    (should (equal (vm-trailing-message-separator) "\037"))))

(ert-deftest vm-folder-test-trailing-separator-explicit-type ()
  "Test trailing separator with explicit folder type."
  (let ((vm-folder-type 'From_))  ; Current type is From_
    ;; But explicitly request babyl separator
    (should (equal (vm-trailing-message-separator 'babyl) "\037"))))

;;; vm-match-header tests

(ert-deftest vm-folder-test-match-header-basic ()
  "Test basic header matching."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert "Subject: Test Subject\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (should (vm-match-header))
    (should (string= (vm-matched-header-name) "From"))))

(ert-deftest vm-folder-test-match-header-specific ()
  "Test matching a specific header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "Subject: Test Subject\n")
    (insert "\n")
    (goto-char (point-min))
    ;; Skip to Subject line
    (forward-line 1)
    (should (vm-match-header "Subject"))
    (should (string-match "Test Subject" (vm-matched-header-contents)))))

(ert-deftest vm-folder-test-match-header-no-match ()
  "Test header matching at non-header location."
  (with-temp-buffer
    (insert "\n")  ; Empty line marks end of headers
    (insert "Body text\n")
    (goto-char (point-min))
    (should-not (vm-match-header))))

;;; vm-matched-header accessors tests

(ert-deftest vm-folder-test-matched-header-accessors ()
  "Test header accessor functions after match."
  (with-temp-buffer
    (insert "Subject: Hello World\n")
    (insert "\n")
    (goto-char (point-min))
    (vm-match-header)
    (should (stringp (vm-matched-header)))
    (should (string= (vm-matched-header-name) "Subject"))
    (should (string-match "Hello World" (vm-matched-header-contents)))
    (should (numberp (vm-matched-header-start)))
    (should (numberp (vm-matched-header-end)))
    (should (numberp (vm-matched-header-name-start)))
    (should (numberp (vm-matched-header-name-end)))
    (should (numberp (vm-matched-header-contents-start)))
    (should (numberp (vm-matched-header-contents-end)))))

;;; vm-set-buffer-modified-p tests

(ert-deftest vm-folder-test-mark-modified ()
  "Test marking folder as modified."
  (with-temp-buffer
    (let ((vm-modification-counter 0)
          (vm-buffers-needing-display-update (make-vector 29 0))
          (vm-messages-not-on-disk 5))
      (vm-mark-folder-modified-p)
      (should (buffer-modified-p))
      (should (= vm-messages-not-on-disk 0))
      (should (> vm-modification-counter 0)))))

(ert-deftest vm-folder-test-unmark-modified ()
  "Test unmarking folder as modified."
  (with-temp-buffer
    (let ((vm-modification-counter 0)
          (vm-buffers-needing-display-update (make-vector 29 0)))
      (set-buffer-modified-p t)
      (vm-unmark-folder-modified-p (current-buffer))
      (should-not (buffer-modified-p)))))

;;; vm-compatible-folder-p tests

(ert-deftest vm-folder-test-compatible-folder-same-type ()
  "Test compatible folder detection for same type."
  (vm-test-with-temp-dir
    (let* ((test-file (expand-file-name "test-from.mbox" temp-dir))
           (vm-default-From_-folder-type 'From_)
           (vm-trust-From_-with-Content-Length nil)
           (vm-folder-type 'From_))
      (with-temp-file test-file
        (insert "From VM Thu Jan  1 00:00:00 2024\n")
        (insert "From: test@example.com\n\n")
        (insert "Test body\n"))
      (should (vm-compatible-folder-p test-file)))))

;;; Fixture-based folder tests

(ert-deftest vm-folder-test-simple-email-fixture ()
  "Test loading simple email fixture."
  (let ((content (vm-test-read-fixture "emails" "simple-plain.eml")))
    (should (stringp content))
    (should (string-match "From:" content))
    (should (string-match "Subject:" content))))

(ert-deftest vm-folder-test-multipart-email-fixture ()
  "Test loading multipart email fixture."
  (let ((content (vm-test-read-fixture "emails" "multipart-mixed.eml")))
    (should (string-match "multipart/mixed" content))
    (should (string-match "boundary=" content))))

;;; vm-message-position tests

(ert-deftest vm-folder-test-message-position-nil ()
  "Test message position with nil message list."
  (let ((vm-message-list nil))
    (should (null (vm-message-position 'some-message)))))

(ert-deftest vm-folder-test-message-position-found ()
  "Test message position when message is found."
  (let* ((m1 'msg1)
         (m2 'msg2)
         (m3 'msg3)
         (vm-message-list (list m1 m2 m3)))
    (let ((pos (vm-message-position m2)))
      (should pos)
      (should (eq (car pos) m2)))))

(ert-deftest vm-folder-test-message-position-not-found ()
  "Test message position when message is not found."
  (let ((vm-message-list '(msg1 msg2 msg3)))
    (should (null (vm-message-position 'msg4)))))

;;; vm-munge-message-separators tests
;; Note: vm-munge-message-separators only escapes lines that match the exact
;; folder separator patterns. For From_ folders, lines must match the regexp
;; "^From .*[0-9]$" (ending with a digit like date). For MMDF, lines must
;; start with the ^A^A^A^A sequence at beginning of line.

(ert-deftest vm-folder-test-munge-separators-from ()
  "Test munging From_ separators in message body."
  (with-temp-buffer
    (insert "Some text\n")
    ;; Must match "^From .*[0-9]$" pattern - needs to end with digit
    (insert "From fake.sender@example.com Thu Jan 1 00:00:00 2024\n")
    (insert "More text\n")
    (vm-munge-message-separators 'From_ (point-min) (point-max))
    ;; From at beginning of line matching separator pattern should be escaped
    (goto-char (point-min))
    (should (search-forward ">From fake.sender@example.com" nil t))))

(ert-deftest vm-folder-test-munge-separators-mmdf ()
  "Test munging MMDF separators in message body."
  (with-temp-buffer
    (insert "Some text\n")
    ;; MMDF separator - four ^A characters at line start
    (insert "\001\001\001\001\n")
    (insert "More text\n")
    (vm-munge-message-separators 'mmdf (point-min) (point-max))
    ;; Should be escaped with ">" prefix
    (goto-char (point-min))
    (should (search-forward ">\001\001\001\001" nil t))))

;;; Buffer state tests

(ert-deftest vm-folder-test-reset-buffer-modified ()
  "Test reset-buffer-modified-p."
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (vm-reset-buffer-modified-p t (current-buffer))
    (should (buffer-modified-p))
    (vm-reset-buffer-modified-p nil (current-buffer))
    (should-not (buffer-modified-p))))

(ert-deftest vm-folder-test-restore-buffer-modified ()
  "Test restore-buffer-modified-p."
  (with-temp-buffer
    ;; Save initial state
    (let ((saved-state (buffer-modified-p)))
      (set-buffer-modified-p (not saved-state))
      ;; Restore
      (vm-restore-buffer-modified-p saved-state (current-buffer))
      (should (eq (buffer-modified-p) saved-state)))))

;;; vm-skip-past-leading-message-separator tests

(ert-deftest vm-folder-test-skip-past-leading-from ()
  "Test skipping past From_ message separator."
  (with-temp-buffer
    (insert "From VM Thu Jan  1 00:00:00 2024\n")
    (insert "From: sender@example.com\n")
    (let ((vm-folder-type 'From_))
      (goto-char (point-min))
      (vm-skip-past-leading-message-separator)
      ;; Should be at the From: header line
      (should (looking-at "From:")))))

(ert-deftest vm-folder-test-skip-past-leading-mmdf ()
  "Test skipping past MMDF message separator."
  (with-temp-buffer
    (insert "\001\001\001\001\n")
    (insert "From: sender@example.com\n")
    (let ((vm-folder-type 'mmdf))
      (goto-char (point-min))
      (vm-skip-past-leading-message-separator)
      ;; Should be at the From: header line
      (should (looking-at "From:")))))

;;; vm-skip-past-trailing-message-separator tests

(ert-deftest vm-folder-test-skip-past-trailing-from ()
  "Test skipping past From_ trailing separator."
  (with-temp-buffer
    (insert "Body text\n")
    (insert "\n")
    (let ((vm-folder-type 'From_))
      (goto-char (point-min))
      (forward-line 1)
      (vm-skip-past-trailing-message-separator)
      ;; Should be at end
      (should (eobp)))))

(ert-deftest vm-folder-test-skip-past-trailing-mmdf ()
  "Test skipping past MMDF trailing separator."
  (with-temp-buffer
    (insert "Body text\n")
    (insert "\001\001\001\001\n")
    (let ((vm-folder-type 'mmdf))
      (goto-char (point-min))
      (forward-line 1)
      (vm-skip-past-trailing-message-separator)
      ;; Should be at end
      (should (eobp)))))

;;; vm-find-leading-message-separator tests

(ert-deftest vm-folder-test-find-leading-from ()
  "Test finding From_ leading separator."
  (with-temp-buffer
    ;; First message at start of buffer
    (insert "From VM Thu Jan  1 00:00:00 2024\n")
    (insert "From: sender@example.com\n")
    (insert "Body of first message\n")
    ;; Blank line separates messages in mbox format
    (insert "\n")
    ;; Second message
    (insert "From VM Thu Jan  2 00:00:00 2024\n")
    (insert "From: other@example.com\n")
    (let ((vm-folder-type 'From_))
      ;; Start after first message's From line
      (goto-char (point-min))
      (forward-line 1)
      ;; Should find the second From line
      (should (vm-find-leading-message-separator))
      ;; Should be positioned at the second From line
      (should (looking-at "From VM Thu Jan  2")))))

(ert-deftest vm-folder-test-find-leading-mmdf ()
  "Test finding MMDF leading separator."
  (with-temp-buffer
    (insert "\001\001\001\001\n")
    (insert "From: sender@example.com\n")
    (let ((vm-folder-type 'mmdf))
      (goto-char (point-min))
      (should (vm-find-leading-message-separator)))))

;;; vm-find-trailing-message-separator tests

(ert-deftest vm-folder-test-find-trailing-from ()
  "Test finding From_ trailing separator (blank line before next message)."
  (with-temp-buffer
    (insert "Body text\n")
    ;; Blank line before next message's From line
    (insert "\n")
    (insert "From VM Thu Jan  1 00:00:00 2024\n")
    (let ((vm-folder-type 'From_))
      (goto-char (point-min))
      ;; vm-find-trailing-message-separator for From_ calls
      ;; vm-find-leading-message-separator then backs up one char.
      ;; Note: function returns nil for From_ but positions point correctly
      (vm-find-trailing-message-separator)
      ;; After finding, point should be at the newline before the next From
      (should (looking-at "\nFrom VM")))))

(ert-deftest vm-folder-test-find-trailing-mmdf ()
  "Test finding MMDF trailing separator."
  (with-temp-buffer
    (insert "Body text\n")
    (insert "\001\001\001\001\n")
    (let ((vm-folder-type 'mmdf))
      (goto-char (point-min))
      (should (vm-find-trailing-message-separator)))))

;;; High-level buffer operations

(ert-deftest vm-folder-test-buffer-type-detection ()
  "Test that folder type detection works on various formats."
  ;; Test From_ detection
  (with-temp-buffer
    (insert "From VM Thu Jan  1 00:00:00 2024\n")
    (insert "From: test@example.com\n\nBody\n")
    (let ((vm-default-From_-folder-type 'From_)
          (vm-trust-From_-with-Content-Length nil))
      (should (eq (vm-get-folder-type) 'From_))))
  ;; Test MMDF detection
  (with-temp-buffer
    (insert "\001\001\001\001\n")
    (insert "From: test@example.com\n\nBody\n")
    (insert "\001\001\001\001\n")
    (should (eq (vm-get-folder-type) 'mmdf)))
  ;; Test BABYL detection
  (with-temp-buffer
    (insert "BABYL OPTIONS:\n")
    (should (eq (vm-get-folder-type) 'babyl))))

;;; vm-marker tests

(ert-deftest vm-folder-test-marker-basic ()
  "Test vm-marker creates a marker."
  (with-temp-buffer
    (insert "test")
    (let ((m (vm-marker (point))))
      (should (markerp m))
      (should (= (marker-position m) (point))))))

;;; Header iteration tests

(ert-deftest vm-folder-test-match-all-headers ()
  "Test iterating through headers with vm-match-header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert "Subject: Test\n")
    (insert "Date: Mon, 1 Jan 2024 10:00:00 +0000\n")
    (insert "X-Custom: value\n")
    (insert "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (let ((count 0))
      (while (vm-match-header)
        (setq count (1+ count))
        (goto-char (vm-matched-header-end)))
      ;; Should have matched 5 headers
      (should (= count 5)))))

(ert-deftest vm-folder-test-match-header-multiline ()
  "Test matching multiline (folded) header."
  (with-temp-buffer
    (insert "Subject: This is a very long subject line\n")
    (insert "\tthat continues on the next line\n")
    (insert "From: sender@example.com\n")
    (insert "\n")
    (goto-char (point-min))
    (should (vm-match-header))
    (should (string= (vm-matched-header-name) "Subject"))
    ;; The contents should include the continuation
    (let ((contents (vm-matched-header-contents)))
      (should (string-match "continues" contents)))))

;;; High-level folder tests using vm-test-with-folder

(defvar vm-test-simple-mbox
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
To: recipient@example.com
Subject: Test Message
Date: Mon, 1 Jan 2024 12:00:00 +0000

This is the body of the test message.
It has multiple lines.
"
  "A simple single-message mbox for testing.")

(defvar vm-test-multi-mbox
  "From sender1@example.com Mon Jan  1 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: First Message
Date: Mon, 1 Jan 2024 12:00:00 +0000

Body of first message.

From sender2@example.com Tue Jan  2 00:00:00 2024
From: sender2@example.com
To: recipient@example.com
Subject: Second Message
Date: Tue, 2 Jan 2024 12:00:00 +0000

Body of second message.

From sender3@example.com Wed Jan  3 00:00:00 2024
From: sender3@example.com
To: recipient@example.com
Subject: Third Message
Date: Wed, 3 Jan 2024 12:00:00 +0000

Body of third message.
"
  "A multi-message mbox for testing.")

(ert-deftest vm-folder-test-parse-single-message ()
  "Test parsing a folder with a single message."
  (vm-test-with-folder vm-test-simple-mbox
    (should (= (vm-test-message-count) 1))
    (should vm-message-list)
    (should vm-message-pointer)))

(ert-deftest vm-folder-test-parse-multiple-messages ()
  "Test parsing a folder with multiple messages."
  (vm-test-with-folder vm-test-multi-mbox
    (should (= (vm-test-message-count) 3))))

(ert-deftest vm-folder-test-message-markers ()
  "Test that message markers are properly set."
  (vm-test-with-folder vm-test-simple-mbox
    (let ((m (vm-test-first-message)))
      (should (markerp (vm-start-of m)))
      (should (markerp (vm-headers-of m)))
      (should (markerp (vm-text-end-of m)))
      (should (markerp (vm-end-of m)))
      ;; Markers should be in order
      (should (< (vm-start-of m) (vm-headers-of m)))
      (should (< (vm-headers-of m) (vm-text-end-of m)))
      (should (<= (vm-text-end-of m) (vm-end-of m))))))

(ert-deftest vm-folder-test-message-type ()
  "Test that message type is set correctly."
  (vm-test-with-folder vm-test-simple-mbox
    (let ((m (vm-test-first-message)))
      (should (eq (vm-message-type-of m) 'From_)))))

(ert-deftest vm-folder-test-extract-body ()
  "Test extracting message body text."
  (vm-test-with-folder vm-test-simple-mbox
    (let ((body (vm-test-message-body (vm-test-first-message))))
      (should (stringp body))
      (should (string-match "body of the test message" body)))))

(ert-deftest vm-folder-test-extract-header ()
  "Test extracting headers from parsed message."
  (vm-test-with-folder vm-test-simple-mbox
    (let ((m (vm-test-first-message)))
      (should (equal (vm-test-message-header m "From")
                     "sender@example.com"))
      (should (equal (vm-test-message-header m "Subject")
                     "Test Message"))
      (should (equal (vm-test-message-header m "To")
                     "recipient@example.com")))))

(ert-deftest vm-folder-test-nth-message ()
  "Test accessing messages by index."
  (vm-test-with-folder vm-test-multi-mbox
    (should (equal (vm-test-message-header (vm-test-nth-message 0) "Subject")
                   "First Message"))
    (should (equal (vm-test-message-header (vm-test-nth-message 1) "Subject")
                   "Second Message"))
    (should (equal (vm-test-message-header (vm-test-nth-message 2) "Subject")
                   "Third Message"))))

(ert-deftest vm-folder-test-reverse-links ()
  "Test that message reverse links are properly set."
  (vm-test-with-folder vm-test-multi-mbox
    (let ((m1 (vm-test-nth-message 0))
          (m2 (vm-test-nth-message 1))
          (m3 (vm-test-nth-message 2)))
      ;; First message has no reverse link
      (should (null (vm-reverse-link-of m1)))
      ;; Second message points back to first
      (should (eq (car (vm-reverse-link-of m2)) m1))
      ;; Third message points back to second
      (should (eq (car (vm-reverse-link-of m3)) m2)))))

(ert-deftest vm-folder-test-message-buffer ()
  "Test that messages know their buffer."
  (vm-test-with-folder vm-test-simple-mbox
    (let ((m (vm-test-first-message)))
      (should (bufferp (vm-buffer-of m)))
      (should (eq (vm-buffer-of m) (current-buffer))))))

;;; Test using fixture files

(ert-deftest vm-folder-test-fixture-simple-plain ()
  "Test parsing simple-plain.eml fixture."
  (vm-test-with-folder-fixture "emails" "simple-plain.eml"
    (should (>= (vm-test-message-count) 1))))

(ert-deftest vm-folder-test-fixture-multipart ()
  "Test parsing multipart-mixed.eml fixture."
  (vm-test-with-folder-fixture "emails" "multipart-mixed.eml"
    (should (>= (vm-test-message-count) 1))))

(provide 'vm-folder-test)

;;; vm-folder-test.el ends here
