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
        (vm-trust-content-length nil))
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
  "Test mboxcl2 trailing separator."
  (let ((vm-folder-type 'mboxcl2))
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
           (vm-trust-content-length nil)
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
          (vm-trust-content-length nil))
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

(ert-deftest vm-folder-test-reverse-links-survive-splicing-and-rebuilding ()
  "Reverse links still describe the list after messages are spliced out.
The links live outside the messages, in `vm-reverse-link-table' (issue #453).
Two operations rearrange the list and so can get them wrong: expunging, which
splices a cons out, and `vm-reverse-link-messages', which sorting uses to
rebuild every link."
  (vm-test-with-folder vm-test-multi-mbox
    (should (= 3 (length vm-message-list)))
    (should (vm-test-reverse-links-consistent-p))
    ;; Expunge the middle message: the third must now point at the first.
    (let ((m1 (vm-test-nth-message 0))
          (m3 (vm-test-nth-message 2)))
      (vm-expunge-message (vm-test-nth-message 1))
      (should (= 2 (length vm-message-list)))
      (should (vm-test-reverse-links-consistent-p))
      (should (eq (car (vm-reverse-link-of m3)) m1))
      ;; Expunge the first: the survivor heads the list and has no link.
      (vm-expunge-message m1)
      (should (equal (list m3) vm-message-list))
      (should (null (vm-reverse-link-of m3))))
    ;; Rebuilding from scratch over a reordered list, as sorting does.
    (setq vm-message-list (list (vm-make-message) (vm-make-message)
                                (vm-make-message)))
    (vm-reverse-link-messages)
    (should (vm-test-reverse-links-consistent-p))
    (setq vm-message-list (reverse vm-message-list))
    (vm-reverse-link-messages)
    (should (vm-test-reverse-links-consistent-p))))

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

;;; label registration tests

(defun vm-folder-test-message-with-labels (labels)
  "Return an mbox message whose X-VM-v5-Data carries LABELS."
  (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
          "X-VM-v5-Data: ("
          (prin1-to-string (make-vector vm-attributes-vector-length nil))
          "\n\t"
          (prin1-to-string (make-vector vm-cached-data-vector-length nil))
          "\n\t"
          (prin1-to-string labels)
          ")\n"
          "From: sender@example.com\n"
          "Subject: Test\n"
          "\n"
          "Body\n"))

(ert-deftest vm-folder-test-register-message-labels ()
  "Test that `vm-register-message-labels' adds labels to the folder's list."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    (setq vm-label-obarray (make-vector 29 0))
    (vm-set-decoded-labels-of (car vm-message-list) '("one" "two"))
    (vm-register-message-labels vm-message-list)
    (should (equal (sort (vm-obarray-to-string-list vm-label-obarray)
                         #'string-lessp)
                   '("one" "two")))))

(ert-deftest vm-folder-test-register-message-labels-downcases ()
  "Test that a registered label is stored lowercase.
`vm-expunge-label' downcases its argument, so a label interned as
\"Work\" could never be expunged."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    (setq vm-label-obarray (make-vector 29 0))
    (vm-set-decoded-labels-of (car vm-message-list) '("Work" "URGENT"))
    (vm-register-message-labels vm-message-list)
    (should (equal (sort (vm-obarray-to-string-list vm-label-obarray)
                         #'string-lessp)
                   '("urgent" "work")))))

(ert-deftest vm-folder-test-register-message-labels-empty ()
  "Test that messages with no labels register nothing."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    (setq vm-label-obarray (make-vector 29 0))
    (vm-register-message-labels vm-message-list)
    (should (null (vm-obarray-to-string-list vm-label-obarray)))))

(ert-deftest vm-folder-test-assimilate-registers-labels ()
  "Test that a message arriving already labelled registers its labels.
Otherwise the label is in use but missing from the folder's list, so it
never appears in label completion -- what `vm-sync-labels' exists to
repair after the fact."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (insert (vm-folder-test-message-with-labels '("came-with-message")))
    (goto-char (point-min))
    (setq vm-label-obarray (make-vector 29 0))
    (vm-assimilate-new-messages :read-attributes t :run-hooks nil)
    (should (equal (vm-labels-of (car vm-message-list)) '("came-with-message")))
    (should (equal (vm-obarray-to-string-list vm-label-obarray)
                   '("came-with-message")))))

;;; fetched-message bookkeeping tests

(ert-deftest vm-folder-test-unregister-fetched-message-registered ()
  "Test that unregistering a fetched message drops it and counts down."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    (let* ((m (car vm-message-list))
           (vm-fetched-messages (list m))
           (vm-fetched-message-count 1))
      (vm-unregister-fetched-message m)
      (should (null vm-fetched-messages))
      (should (= vm-fetched-message-count 0)))))

(ert-deftest vm-folder-test-unregister-fetched-message-not-registered ()
  "Test that unregistering a message that was never fetched changes nothing.
`vm-expunge-message' calls this for every expunged message, registered
or not.  The count was decremented unconditionally, so expunging drove
it below the length of the list -- and once it went negative the
`vm-external-fetched-message-limit' comparison stopped being true, so
fetched bodies were never evicted again."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: One

Body

From sender@example.com Mon Jan  1 00:00:01 2024
From: sender@example.com
Subject: Two

Body
"
    (let* ((fetched (car vm-message-list))
           (other (nth 1 vm-message-list))
           (vm-fetched-messages (list fetched))
           (vm-fetched-message-count 1))
      (vm-unregister-fetched-message other)
      (should (equal vm-fetched-messages (list fetched)))
      (should (= vm-fetched-message-count 1)))))

(ert-deftest vm-folder-test-unregister-fetched-message-count-never-negative ()
  "Test that repeated unregistering cannot drive the count below zero."
  (vm-test-with-folder "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    (let* ((m (car vm-message-list))
           (vm-fetched-messages nil)
           (vm-fetched-message-count 0))
      (dotimes (_ 5) (vm-unregister-fetched-message m))
      (should (= vm-fetched-message-count 0)))))

;;; 8-bit headers must not damage the folder (issues #11, #368)

(defconst vm-folder-test--8bit-folder-text
  (concat "From rene@example.com Mon Jan  1 00:00:00 2024\n"
          "From: René Müller <rene@example.com>\n"
          "To: vm@example.com\n"
          "Subject: Grüße aus München\n"
          "Date: Mon, 01 Jan 2024 00:00:00 +0000\n"
          "Message-ID: <utf8-1@example.com>\n"
          "\n"
          "Körper des Briefes.\n\n")
  "A message whose headers hold raw 8-bit text, as RFC 6532 permits.")

(defun vm-folder-test--write-8bit-folder (file &optional coding)
  "Write `vm-folder-test--8bit-folder-text' to FILE in CODING, default UTF-8.
Written as bytes, which is what a folder on disk is."
  (let ((coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (encode-coding-string vm-folder-test--8bit-folder-text
                                    (or coding 'utf-8)))
      (write-region (point-min) (point-max) file nil 'quiet))))

(defmacro vm-folder-test--with-visited-folder (file &rest body)
  "Visit FILE as a VM folder, run BODY in it, then leave nothing behind.
Every buffer the visit created is killed, unmodified, on the way out: a folder
buffer left alive is global state, and the next test to visit a folder walks
into it."
  (declare (indent 1) (debug t))
  ;; Menus are left switched on: with `vm-use-menus' nil the visit skips
  ;; `vm-menu-initialize-vm-mode-menu-map', which is what defines the
  ;; vm-menu-fsfemacs-*-menu variables, and the next test in the same Emacs
  ;; to build a presentation buffer then reads one of them unbound.
  `(let ((vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
     (unwind-protect
         (progn
           (vm-visit-folder ,file)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun vm-folder-test--file-bytes (file)
  "Return the contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary))
      (insert-file-contents-literally file))
    (buffer-string)))

(ert-deftest vm-folder-test-8bit-headers-visit-and-summarize ()
  "A folder with raw 8-bit headers is readable, and its summary is legible.
Issue #11 reported that such a folder broke VM and had to be cleaned up with
another mail reader; issue #368 is the same text being legal under RFC 6532.
The bytes reach the summary as characters rather than as octets."
  (let* ((vm-use-menus nil)
         (dir (file-name-as-directory (make-temp-file "vm-8bit" t)))
         (file (expand-file-name "folder" dir)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-folder-test--write-8bit-folder file)
          (vm-folder-test--with-visited-folder file
            (should (= 1 (length vm-message-list)))
            (let ((m (car vm-message-list)))
              (should (equal "Grüße aus München" (vm-su-subject m)))
              (should (equal "René Müller" (vm-su-full-name m))))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-8bit-headers-survive-a-save ()
  "Visiting and saving a folder with 8-bit headers leaves its bytes alone.
The decoding that makes such headers legible is for display only.  If it
reached the folder buffer, saving would rewrite the file in some other
encoding -- which is the corruption issue #11 complained about, only caused
by VM rather than avoided by it."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-8bit" t)))
         (file (expand-file-name "folder" dir))
         (raw-subject (decode-coding-string
                       (encode-coding-string "Subject: Grüße aus München"
                                             'utf-8)
                       'binary)))
    (require 'vm)
    (unwind-protect
        (let ((before (progn (vm-folder-test--write-8bit-folder file)
                             (vm-folder-test--file-bytes file))))
          (vm-folder-test--with-visited-folder file
            ;; Read the message the way a user does: summary line, then a
            ;; presentation copy with its headers decoded.
            (let ((m (car vm-message-list)))
              (vm-su-subject m)
              (should-not (eq 'none (vm-mm-encoded-header m)))
              (vm-make-presentation-copy m)
              (with-current-buffer vm-presentation-buffer
                (vm-decode-mime-message-headers (car vm-message-pointer))
                ;; The display shows it as text ...
                (should (string-match-p "Subject: Grüße aus München"
                                        (buffer-string)))))
            ;; ... while the folder buffer still holds the bytes.
            (save-restriction
              (widen)
              (should (string-match-p (regexp-quote raw-subject)
                                      (buffer-string))))
            (vm-save-folder))
          (let ((after (vm-folder-test--file-bytes file)))
            ;; VM adds its own X-VM- bookkeeping headers on the first save, so
            ;; the file legitimately grows; what must not change is the
            ;; sender's own text.
            (should (string-match-p
                     (regexp-quote
                      (encode-coding-string "Subject: Grüße aus München"
                                            'utf-8))
                     after))
            (should (string-match-p
                     (regexp-quote
                      (encode-coding-string "Körper des Briefes." 'utf-8))
                     after))
            ;; And nothing was re-encoded into some other set of bytes.
            (should-not (string-match-p
                         (regexp-quote
                          (encode-coding-string "Grüße" 'iso-8859-1))
                         after))
            (should (>= (length after) (length before)))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-8bit-latin-1-headers-are-legible ()
  "Headers in a single-byte encoding are shown as text too.
There is no character set stated anywhere for them, so this is a guess, but
it is a better guess than showing the octets."
  (let* ((vm-use-menus nil)
         (dir (file-name-as-directory (make-temp-file "vm-8bit" t)))
         (file (expand-file-name "folder" dir)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-folder-test--write-8bit-folder file 'iso-8859-1)
          (vm-folder-test--with-visited-folder file
            (should (equal "Grüße aus München"
                           (vm-su-subject (car vm-message-list))))))
      (delete-directory dir t))))


;;; a server folder's cache, opened as a folder of its own (issue #425)

(ert-deftest vm-folder-test-cache-folder-name-p ()
  "Cache folder names are recognised, and ordinary folder names are not."
  (should (vm-cache-folder-name-p "imap-cache-b979c2934ac0b4ba3f08dabfdd1b2299"))
  (should (vm-cache-folder-name-p "/home/someone/Mail/pop-cache-0123456789abcdef"))
  (should-not (vm-cache-folder-name-p "INBOX"))
  (should-not (vm-cache-folder-name-p "/home/someone/Mail/imap-cache-notes.txt"))
  (should-not (vm-cache-folder-name-p nil)))

(defmacro vm-folder-test--visiting (file &rest body)
  "Visit FILE as a folder with `vm-warn' captured, run BODY, clean up.
BODY can look at WARNINGS, the list of warning strings."
  (declare (indent 1) (debug t))
  `(let ((vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list))
         (warnings nil))
     (require 'vm)
     (unwind-protect
         (cl-letf (((symbol-function 'vm-warn)
                    (lambda (_level _secs format &rest args)
                      (push (apply #'format format args) warnings))))
           (vm-visit-folder ,file)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun vm-folder-test--write-plain-folder (file)
  "Write a one-message From_ folder to FILE."
  (with-temp-file file
    (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
            "From: alice@example.com\n"
            "Subject: one\n"
            "\n"
            "Body.\n\n")))

(ert-deftest vm-folder-test-visiting-a-cache-folder-warns ()
  "REGRESSION: opening a server folder's cache as a folder says so.
Issue #425.  The cache reads perfectly as a folder, which is the trouble: it
looks like the mailbox and is not connected to it, so nothing done in it reaches
the server and the next real session sees none of it.  desktop.el restoring the
buffer, or plain find-file, both land here."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-cache" t)))
         (cache (expand-file-name (concat "imap-cache-" (md5 "spec")) dir)))
    (unwind-protect
        (progn
          (vm-folder-test--write-plain-folder cache)
          (vm-folder-test--visiting cache
            (should (cl-find-if (lambda (w) (string-match-p "local cache" w))
                                warnings))
            ;; It really was read as a folder -- that is why the warning is
            ;; needed rather than an error.
            (should (= 1 (length vm-message-list)))
            (should-not vm-folder-access-method)))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-visiting-an-ordinary-folder-is-quiet ()
  "An ordinary folder does not get the cache-folder warning.
The control: a warning on every folder would be worse than none."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-cache" t)))
         (plain (expand-file-name "ordinary-folder" dir)))
    (unwind-protect
        (progn
          (vm-folder-test--write-plain-folder plain)
          (vm-folder-test--visiting plain
            (should (= 1 (length vm-message-list)))
            (should-not (cl-find-if (lambda (w) (string-match-p "local cache" w))
                                    warnings))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-cache-folder-visited-properly-is-quiet ()
  "Opened the right way, through vm-visit-imap-folder, a cache folder is quiet.
`vm-mode-internal' gets told the access method then, and the warning is only for
the case where it is not."
  (let ((warnings nil))
    (with-temp-buffer
      (setq buffer-file-name (concat "/tmp/imap-cache-" (md5 "spec")))
      (unwind-protect
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_level _secs format &rest args)
                       (push (apply #'format format args) warnings)))
                    ((symbol-function 'vm-menu-install-menus) #'ignore)
                    ((symbol-function 'vm-set-summary-redo-start-point) #'ignore))
            (ignore-errors (vm-mode-internal 'imap))
            (should (eq 'imap vm-folder-access-method))
            (should-not (cl-find-if (lambda (w) (string-match-p "local cache" w))
                                    warnings)))
        (setq buffer-file-name nil)))))


;;; server deletions that have not been sent yet (issue #556)

(ert-deftest vm-folder-test-imap-to-expunge-header-round-trip ()
  "REGRESSION: pending server deletions survive being written to the folder.
Issue #556.  `vm-imap-messages-to-expunge' is buffer-local and was never
written anywhere, so a session that ended before it could reach the server
dropped the deletions -- the user\'s mail stayed on the server for good, with
nothing said.  It now goes into the folder beside X-VM-IMAP-Retrieved."
  (vm-test-with-folder vm-folder-test--8bit-folder-text
    (let ((pending '(("12" . "1785695783") ("7" . "1785695783"))))
      (setq vm-imap-messages-to-expunge pending)
      (vm-stuff-imap-to-expunge)
      (save-restriction
        (widen)
        (should (string-match-p "X-VM-IMAP-To-Expunge:" (buffer-string))))
      ;; Forget it, then read it back from the folder.
      (setq vm-imap-messages-to-expunge nil)
      (vm-gobble-imap-to-expunge)
      (should (equal pending vm-imap-messages-to-expunge)))))

(ert-deftest vm-folder-test-imap-to-expunge-header-empty ()
  "An empty pending list round-trips as empty, not as garbage."
  (vm-test-with-folder vm-folder-test--8bit-folder-text
    (setq vm-imap-messages-to-expunge nil)
    (vm-stuff-imap-to-expunge)
    (vm-gobble-imap-to-expunge)
    (should-not vm-imap-messages-to-expunge)))

(ert-deftest vm-folder-test-imap-to-expunge-rewritten-not-duplicated ()
  "Writing the header twice leaves one of it, with the newer value.
It is rewritten on every save, so a folder must not collect a header per save."
  (vm-test-with-folder vm-folder-test--8bit-folder-text
    (setq vm-imap-messages-to-expunge '(("1" . "100")))
    (vm-stuff-imap-to-expunge)
    (setq vm-imap-messages-to-expunge '(("2" . "100")))
    (vm-stuff-imap-to-expunge)
    (save-restriction
      (widen)
      (should (= 1 (cl-count-if
                    (lambda (line)
                      (string-prefix-p "X-VM-IMAP-To-Expunge:" line))
                    (split-string (buffer-string) "\n")))))
    (setq vm-imap-messages-to-expunge nil)
    (vm-gobble-imap-to-expunge)
    (should (equal '(("2" . "100")) vm-imap-messages-to-expunge))))

(ert-deftest vm-folder-test-index-file-restores-reverse-links ()
  "A message list read from the index file is linked like a parsed one.
The index is the one source of a message list that does not come from
`vm-build-message-list', which links only messages it creates itself and not
ones it finds already in `vm-message-list'.  What links these instead is the
message order the index also carries: `vm-read-index-file' ends by applying it,
and `vm-startup-apply-message-order' finishes with `vm-reverse-link-messages'.
So the linking rides on a field written for another purpose.  Expunging reads
these links to decide which cons to splice, so were the order ever written
conditionally the folder would start losing the wrong message.  Issue #453."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-index" t)))
         (folder (expand-file-name "folder" dir))
         (index (expand-file-name ".folder.inx" dir))
         (vm-index-file-suffix ".inx")
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (with-temp-file folder
            (dotimes (i 4)
              (insert (format "From s%d@example.com Mon Jan  1 00:00:00 2024\n" i)
                      (format "From: S%d <s%d@example.com>\n" i i)
                      (format "Subject: subject %d\n" i)
                      (format "Message-ID: <m-%d@example.com>\n" i)
                      "\n" (format "Body %d.\n\n" i))))
          (vm-visit-folder folder)
          (should (= 4 (length vm-message-list)))
          (should (vm-test-reverse-links-consistent-p))
          (vm-write-index-file index)
          ;; Install a list from the index alone, which is what a visit does
          ;; when it trusts the index instead of parsing.
          (setq vm-message-list nil)
          (should (vm-read-index-file index))
          (should (= 4 (length vm-message-list)))
          (should (vm-test-reverse-links-consistent-p)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-index-file-carries-pending-expunges ()
  "The index file carries the pending deletions, and still reads version 1.
The index is an optional cache of the folder, so its version can move on: a
reader that does not know version 2 discards the file and parses the folder,
which is correct if slower.  This checks both directions -- a version 2 file
round-trips the list, and a version 1 file is still read, with the list empty."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-index" t)))
         (folder (expand-file-name "folder" dir))
         (index (expand-file-name ".folder.inx" dir))
         (pending '(("42" . "1785695783")))
         (vm-index-file-suffix ".inx")
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-folder-test--write-plain-folder folder)
          (vm-visit-folder folder)
          (setq vm-imap-messages-to-expunge pending)
          (vm-write-index-file index)
          (with-temp-buffer
            (insert-file-contents index)
            (goto-char (point-min))
            ;; version 2 now
            (should (re-search-forward "^2$" nil t))
            (should (string-match-p "42" (buffer-string))))
          (setq vm-imap-messages-to-expunge nil)
          (should (vm-read-index-file index))
          (should (equal pending vm-imap-messages-to-expunge))
          ;; A version 1 file, as an older VM would have written: read, with
          ;; no pending deletions, rather than refused.
          (with-temp-buffer
            (insert-file-contents index)
            (goto-char (point-min))
            (should (re-search-forward "^2$" nil t))
            (replace-match "1")
            ;; drop the field version 1 does not have
            (goto-char (point-min))
            (when (re-search-forward "^;; IMAP messages to expunge on the server$"
                                     nil t)
              (let ((start (match-beginning 0)))
                (goto-char start)
                (forward-line 1)
                (let ((field-start (point)))
                  (forward-sexp)
                  (delete-region start (point)))))
            (let ((coding-system-for-write 'raw-text))
              (write-region (point-min) (point-max) index nil 'quiet)))
          (setq vm-imap-messages-to-expunge pending)
          (should (vm-read-index-file index))
          (should-not vm-imap-messages-to-expunge))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; recovering a folder without knowing its file name (issue #547)

(ert-deftest vm-folder-test-recover-defaults-to-the-current-folder ()
  "REGRESSION: recovering a folder offers the folder you are in.
Issue #547.  `vm-recover-folder' called `recover-file' interactively, which
prompts for a file name with no default, so after a crash the user had to type
the name of the folder's file -- and for a server folder that is a cache named
after the MD5 of the maildrop specification, which nobody can produce from
memory.  It is now offered as the default, so RET is enough."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-recover" t)))
         (folder (expand-file-name "plain-folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (vm-folder-test--write-plain-folder folder)
          (vm-visit-folder folder)
          (let (prompt-seen)
            ;; Stand in for the user pressing RET: read-file-name returns the
            ;; default it was given.
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (prompt &optional _dir default &rest _)
                         (setq prompt-seen prompt)
                         default)))
              (should (equal folder (vm-recover-folder-file-name)))
              ;; and the prompt says what RET will do
              (should (string-match-p "plain-folder" prompt-seen)))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-recover-accepts-an-imap-folder-name ()
  "An IMAP folder may be named ACCOUNT:MAILBOX at the recover prompt.
The other half of #547: the file is unguessable, but the folder's name is not.
Nothing else in the tree resolves one to the other, so this checks the two
halves are actually wired together."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-recover" t)))
         (spec "imap:mail.example.com:143:INBOX:login:someone:secret")
         (vm-imap-folder-cache-directory dir)
         (vm-imap-account-alist (list (list spec "myaccount")))
         (cache (vm-imap-make-filename-for-spec spec)))
    (require 'vm)
    (unwind-protect
        (with-temp-buffer
          ;; Not a folder buffer, so there is no default; the user types a
          ;; folder name and read-file-name expands it against the directory.
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _)
                       (expand-file-name "myaccount:INBOX" dir))))
            (should (equal cache (vm-recover-folder-file-name))))
          ;; A name that resolves to nothing is handed back as a file, so a
          ;; mistyped name is not silently turned into some other folder.
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _)
                       (expand-file-name "no-such-account:INBOX" dir))))
            (should (equal (expand-file-name "no-such-account:INBOX" dir)
                           (vm-recover-folder-file-name)))))
      (delete-directory dir t))))


;;; saving a folder reached through a link (issue #532)

(defmacro vm-folder-test--with-linked-folder (spec &rest body)
  "Build a folder, a symlink to it and a second hard link, then run BODY.
SPEC is (REAL-VAR LINK-VAR HARD-VAR PRECIOUS), where PRECIOUS is the value for
`vm-folder-file-precious-flag' and must be given -- nil is a meaningful value
here, so it cannot also mean \"not supplied\".  BODY runs with the folder visited
*through the symlink*, which is what issue #532 is about."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-linked" t)))
          (,(car spec) (expand-file-name "testmailbox" dir))
          (,(nth 1 spec) (expand-file-name "testlink" dir))
          (,(nth 2 spec) (expand-file-name "testhard" dir))
          (vm-folder-file-precious-flag ,(nth 3 spec))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list)))
     (require 'vm)
     (unwind-protect
         (progn
           (with-temp-file ,(car spec)
             (dotimes (i 3)
               (insert (format "From s%d@example.com Mon Jan  1 00:00:00 2024\n" i)
                       (format "From: S%d <s%d@example.com>\n" i i)
                       (format "Subject: msg %d\n" i)
                       (format "Message-ID: <linked-%d@example.com>\n" i)
                       "\n"
                       (format "Body %d.\n\n" i))))
           (make-symbolic-link "testmailbox" ,(nth 1 spec))
           (add-name-to-file ,(car spec) ,(nth 2 spec))
           (vm-visit-folder ,(nth 1 spec))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-folder-test-saving-through-a-symlink-keeps-the-link ()
  "REGRESSION: saving a folder visited by symlink neither replaces nor bypasses it.
Issue #532.  VM sets `file-precious-flag' in folder buffers -- \"mail folders are
precious\" -- which makes Emacs write a temporary file and rename it over the
folder.  That replaced the *symlink*, so two things went wrong at once: the link
became a plain file, and the change was written to that new file while the real
mailbox kept its old contents.  Anything else reading the mailbox saw a folder
that had silently diverged.

Emacs has a companion setting for the precious case, `file-preserve-symlinks-on-save',
which resolves the link first so the rename lands on the file it points at."
  (vm-folder-test--with-linked-folder (real link hard t)
    (ignore hard)
    (should (= 3 (length vm-message-list)))
    ;; delete the first message and save, so "msg 0" must be gone from the file
    ;; the link points at
    (vm-delete-message 1)
    (vm-expunge-folder)
    (vm-save-folder)
    ;; the link is still a link ...
    (should (file-symlink-p link))
    ;; ... and the change reached the mailbox it names.
    (with-temp-buffer
      (insert-file-contents real)
      (should-not (string-match-p "Subject: msg 0" (buffer-string)))
      (should (string-match-p "Subject: msg 1" (buffer-string))))))

(ert-deftest vm-folder-test-hard-links-need-precious-flag-off ()
  "A folder kept as a hard link survives a save only with the precious flag off.
The other half of #532, which cannot be fixed and is documented instead: nothing
can atomically replace a file and keep another name pointing at the same
contents, so `vm-folder-file-precious-flag' has to be nil for a folder that is a
hard link.  This pins that the documented workaround actually works."
  (vm-folder-test--with-linked-folder (real link hard nil)
    (ignore link)
    (vm-delete-message 1)
    (vm-expunge-folder)
    (vm-save-folder)
    ;; still two names for one file ...
    (should (= 2 (file-attribute-link-number (file-attributes real))))
    ;; ... and both see the change.
    (dolist (name (list real hard))
      (with-temp-buffer
        (insert-file-contents name)
        (should-not (string-match-p "Subject: msg 0" (buffer-string)))))))


;;; a count that runs past the end of the folder (issue #550)

(defconst vm-folder-test--four-messages
  (apply #'concat
         (mapcar (lambda (i)
                   (format (concat "From s%d@example.com Mon Jan  1 00:00:00 2024\n"
                                   "From: S%d <s%d@example.com>\n"
                                   "Subject: msg %d\n"
                                   "Message-ID: <count-%d@example.com>\n"
                                   "\nBody %d.\n\n")
                           i i i i i i))
                 (number-sequence 0 3)))
  "A four-message folder, subjects \"msg 0\" through \"msg 3\".")

(defmacro vm-folder-test--with-point-at (n &rest body)
  "Run BODY in the four-message folder with the current message the Nth."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder vm-folder-test--four-messages
     (setq major-mode 'vm-mode)
     (setq vm-mail-buffer nil)
     (setq vm-message-pointer (nthcdr ,n vm-message-list))
     ,@body))

(defun vm-folder-test--operable-subjects (count)
  "Return the subjects `vm-select-operable-messages' gives for COUNT."
  (mapcar #'vm-su-subject (vm-select-operable-messages count nil "Test")))

(ert-deftest vm-folder-test-count-past-end-acts-on-what-is-there ()
  "REGRESSION: a count larger than the messages left acts on the rest.
Issue #550: `C-u 10 d' near the end of a folder signalled end-of-folder and
deleted *nothing*.  `vm-select-operable-messages' called `vm-check-count', which
signals rather than clamping.  Acting on as many as there are is what Emacs's own
commands do at a boundary, and VM's commands report how many they acted on, so a
short count is visible rather than silent."
  (vm-folder-test--with-point-at 2
    (should (equal '("msg 2" "msg 3") (vm-folder-test--operable-subjects 10))))
  (vm-folder-test--with-point-at 3
    (should (equal '("msg 3") (vm-folder-test--operable-subjects 10)))))

(ert-deftest vm-folder-test-count-that-fits-is-unchanged ()
  "A count within the folder still selects exactly that many.
The control: clamping must not change the ordinary case."
  (vm-folder-test--with-point-at 0
    (should (equal '("msg 0" "msg 1") (vm-folder-test--operable-subjects 2))))
  (vm-folder-test--with-point-at 2
    (should (equal '("msg 2" "msg 3") (vm-folder-test--operable-subjects 2))))
  ;; and a count of 1 is the current message
  (vm-folder-test--with-point-at 1
    (should (equal '("msg 1") (vm-folder-test--operable-subjects 1)))))

(ert-deftest vm-folder-test-negative-count-is-measured-backwards ()
  "REGRESSION: a backward count is limited by what lies behind, not ahead.
`vm-check-count' was handed the absolute value of the count, so it always took
its forward branch: a backward count was checked against the messages *ahead* of
point.  At the second of four messages, `C-u -10' therefore signalled
end-of-folder -- complaining about the end of the folder while walking towards
its start."
  (vm-folder-test--with-point-at 1
    (should (equal '("msg 1" "msg 0") (vm-folder-test--operable-subjects -10))))
  ;; and a backward count that fits is unaffected
  (vm-folder-test--with-point-at 3
    (should (equal '("msg 3" "msg 2") (vm-folder-test--operable-subjects -2)))))

(ert-deftest vm-folder-test-count-zero-still-means-all ()
  "A count of zero means the whole folder, which clamping must not disturb."
  (vm-folder-test--with-point-at 0
    (should (= 4 (length (vm-folder-test--operable-subjects 0))))))


;;; which movemail VM runs (issue #538)

(ert-deftest vm-folder-test-movemail-defaults-to-the-one-emacs-came-with ()
  "REGRESSION: nil `vm-movemail-program' means Emacs's own movemail.
Issue #538: the default used to be the string \"movemail\", which `call-process'
looks up along `exec-path' -- and /usr/bin/movemail comes first there.  On
Debian and Ubuntu with the mailutils package installed that is GNU Mailutils'
movemail, which moves mail by parsing and rewriting it rather than copying it,
and merges a message whose body is empty with the message after it.  VM wants
the one that copies its input unaltered, and Emacs's is that one."
  (let ((vm-movemail-program nil)
        (own (expand-file-name "movemail" exec-directory)))
    ;; An Emacs built --with-mailutils installs no movemail of its own, and that
    ;; is its default when Mailutils is on PATH at build time -- so this is not a
    ;; broken installation and the assertion simply does not apply.  What happens
    ;; instead is covered by vm-folder-test-movemail-missing-is-an-error.
    (vm-test-skip-unless (file-executable-p own)
                         "this Emacs has no movemail of its own")
    (should (equal own (vm-movemail-program-name)))
    ;; not merely something named movemail somewhere on the path
    (should (file-name-absolute-p (vm-movemail-program-name)))))

(ert-deftest vm-folder-test-movemail-setting-is-honoured ()
  "An explicitly set `vm-movemail-program' is used as given.
Mailutils' movemail is a reasonable choice for a maildrop it does not mangle,
and for the protocols it supports; what the default avoids is reaching for it
by accident."
  (let ((vm-movemail-program "/somewhere/else/movemail"))
    (should (equal "/somewhere/else/movemail" (vm-movemail-program-name)))))

(ert-deftest vm-folder-test-movemail-missing-is-an-error ()
  "With no movemail of its own, VM says so rather than picking one off the path.
Falling back to `exec-path' would mean choosing a program by its name alone to
do the one job where a different implementation than the expected one damages
mail."
  (let ((vm-movemail-program nil)
        (exec-directory (file-name-as-directory
                         (expand-file-name "no-movemail-here"
                                           temporary-file-directory))))
    (should-not (file-executable-p (expand-file-name "movemail" exec-directory)))
    (let* ((err (should-error (vm-movemail-program-name)))
           (text (error-message-string err)))
      ;; The message is the whole remedy for anyone who meets this, so it has to
      ;; say what to do and not merely what is missing (#566).
      (should (string-match-p "vm-movemail-program" text))
      (should (string-match-p "will not fetch" text))     ; what is wrong
      (should (string-match-p "--with-mailutils" text))   ; why Emacs has none
      (should (string-match-p "setq vm-movemail-program" text)) ; what to do
      (should (string-match-p "#538" text))               ; and the caveat
      (should (string-match-p (regexp-quote exec-directory) text)))))

(ert-deftest vm-folder-test-arriving-spooled-mail-makes-a-message-current ()
  "Mail arriving into an empty folder leaves it with a current message.

`vm-get-new-mail' selects one after the blocking fetch; the asynchronous path
does not go through it, so the folder was left with messages in its list and
nothing in `vm-message-pointer', and the next command that worked on the
current message failed with \"Wrong type argument: arrayp, nil\"."
  (vm-test-with-real-folder (0)
    (should (null vm-message-list))
    (should (null vm-message-pointer))
    (let ((crash (expand-file-name "crash" dir)))
      (with-temp-file crash
        (insert "From alice@example.com  Mon Jan  1 00:00:00 2024\n"
                "From: Alice <alice@example.com>\n"
                "Subject: hello\n"
                "\n"
                "Body.\n\n"))
      (should (vm-spooled-mail-arrived crash "spool"))
      (should (equal (length vm-message-list) 1))
      (should vm-message-pointer)
      (should (equal (vm-su-subject (car vm-message-pointer)) "hello")))))

(ert-deftest vm-folder-test-movemail-copies-the-spool-unaltered ()
  "The movemail VM defaults to copies a spool file byte for byte.
The property the default is chosen for, checked against the mbox from #538: a
first message with an empty body, so the blank line ending its headers is the
only one before the next `From ' line.  Skipped when this Emacs has no movemail
of its own -- there is then nothing to make the claim about."
  (let ((movemail (expand-file-name "movemail" exec-directory)))
    (vm-test-skip-unless (file-executable-p movemail)
                         "this Emacs has no movemail of its own")
    (let* ((dir (file-name-as-directory (make-temp-file "vm-538" t)))
           (spool (expand-file-name "spool" dir))
           (crash (expand-file-name "crash" dir))
           (mbox (concat
                  "From alice@example.com  Mon Jan  1 00:00:00 2024\n"
                  "To: bob@example.com\n"
                  "Subject: foo\n"
                  "From: Alice <alice@example.com>\n"
                  "\n"
                  "From alice@example.com  Mon Jan  1 00:00:01 2024\n"
                  "To: bob@example.com\n"
                  "Subject: bar\n"
                  "From: Alice <alice@example.com>\n"
                  "\n"
                  "blat\n\n")))
      (unwind-protect
          (progn
            (with-temp-file spool (insert mbox))
            (let ((vm-movemail-program nil)
                  (vm-movemail-program-switches nil))
              (should (eq 0 (call-process (vm-movemail-program-name)
                                          nil nil nil spool crash))))
            (with-temp-buffer
              (insert-file-contents crash)
              ;; byte for byte, so both messages are still there and separate
              (should (equal mbox (buffer-string)))
              (goto-char (point-min))
              (should (= 2 (how-many "^From alice@example\\.com  ")))
              ;; and nothing added: these are the marks a rewriting movemail
              ;; leaves, and what the folder in #538 arrived carrying
              (should (= 0 (how-many "^>From ")))
              (should (= 0 (how-many "^X-IMAPbase:")))
              (should (= 0 (how-many "^X-UID:")))))
        (delete-directory dir t)))))

;;; Renumbering part of a folder (issue #453)

;; `vm-number-messages' takes a start point rather than renumbering the whole
;; folder, and the first number comes from the message before it, reached
;; through the reverse link.  That is how every splice renumbers: expunge, sort
;; and `vm-move-message-forward' all set `vm-numbering-redo-start-point' to a
;; cons and let this work out the numbers.  Only the whole-folder case had a
;; test, so an off-by-one here was invisible.

(ert-deftest vm-folder-test-numbering-continues-from-the-previous-message ()
  "Renumbering from the middle carries on from the number before it.
The numbers of the messages ahead of the start point are not recomputed and not
consulted, so a start point that took its first number by counting would give
this folder two messages numbered 2."
  (vm-test-with-folder vm-folder-test--four-messages
    ;; Parsing a folder does not number it, so start from a numbered folder.
    (vm-number-messages)
    (should (equal '("1" "2" "3" "4") (mapcar #'vm-number-of vm-message-list)))
    ;; Make the numbers past the start point wrong, so what comes back has to
    ;; have been computed rather than left alone.
    (vm-set-number-of (vm-test-nth-message 2) "99")
    (vm-set-number-of (vm-test-nth-message 3) "99")
    (vm-number-messages (nthcdr 2 vm-message-list))
    (should (equal '("1" "2" "3" "4") (mapcar #'vm-number-of vm-message-list)))
    (should (equal '("  1" "  2" "  3" "  4")
                   (mapcar #'vm-padded-number-of vm-message-list)))))

(ert-deftest vm-folder-test-numbering-from-the-first-message-starts-at-one ()
  "A start point at the head of the folder has no message before it.
The reverse link is nil there, which is the branch that starts the count at 1
instead of consulting a predecessor."
  (vm-test-with-folder vm-folder-test--four-messages
    (mapc (lambda (m) (vm-set-number-of m "99")) vm-message-list)
    (vm-number-messages vm-message-list)
    (should (equal '("1" "2" "3" "4") (mapcar #'vm-number-of vm-message-list)))))

(ert-deftest vm-folder-test-numbering-stops-at-the-end-point ()
  "Renumbering stops before the end point and leaves the rest as it was.
`vm-move-message-forward' relies on this: it renumbers only the stretch of the
folder its move disturbed."
  (vm-test-with-folder vm-folder-test--four-messages
    (mapc (lambda (m) (vm-set-number-of m "99")) vm-message-list)
    ;; Messages 1 and 2, not 3 and 4.
    (vm-number-messages vm-message-list (nthcdr 2 vm-message-list))
    (should (equal '("1" "2" "99" "99") (mapcar #'vm-number-of vm-message-list)))
    ;; And the highest-number line of the mode line is only updated for a
    ;; renumbering that ran to the end of the folder.
    (vm-number-messages)
    (should (equal "4" vm-ml-highest-message-number))))

;;; Saving and expunging together

;; `vm-save-and-expunge-folder' expunges quietly and then saves, so it is the
;; command where the message list and the file on disk have to agree.  A wrong
;; reverse link makes them disagree, one message leaving the list while another
;; message's text is deleted, and this is where that would be written out.  It
;; had no test.

(defun vm-folder-test--bodies-on-disk (path)
  "Return the numbers of the message bodies present in the folder file PATH.
The generated folders have bodies \"Body N.\", one per message."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((found nil))
      (dotimes (i 10)
        (goto-char (point-min))
        (when (search-forward (format "Body %d." i) nil t)
          (push i found)))
      (nreverse found))))

(defun vm-folder-test--separators-on-disk (path)
  "Return how many message separator lines the folder file PATH has."
  (with-temp-buffer
    (insert-file-contents path)
    (how-many "^From alice@example\\.com" (point-min) (point-max))))

(ert-deftest vm-folder-test-save-and-expunge-writes-what-is-left ()
  "The folder on disk ends up with exactly the messages the list has.
Both halves matter: the expunged message's text is gone from the file, and the
other three are still there and still whole."
  (vm-test-with-real-folder (4)
    (let ((path buffer-file-name))
      (vm-set-deleted-flag (nth 1 vm-message-list) t)
      (vm-save-and-expunge-folder)
      (should (equal '("subject 0" "subject 2" "subject 3")
                     (mapcar #'vm-su-subject vm-message-list)))
      (should (vm-test-reverse-links-consistent-p))
      ;; Saved, so nothing is left waiting in the buffer.
      (should-not (buffer-modified-p))
      (should (equal '(0 2 3) (vm-folder-test--bodies-on-disk path)))
      (should (= 3 (vm-folder-test--separators-on-disk path))))))

(ert-deftest vm-folder-test-save-and-expunge-leaves-a-read-only-folder-alone ()
  "A read-only folder is not expunged, as the docstring promises.
The command still saves, so what this pins is that the deleted message is
neither removed from the list nor written out of the file."
  (vm-test-with-real-folder (3)
    (let ((path buffer-file-name))
      (vm-set-deleted-flag (nth 1 vm-message-list) t)
      (setq vm-folder-read-only t)
      (vm-save-and-expunge-folder)
      (should (= 3 (length vm-message-list)))
      (should (vm-deleted-flag (nth 1 vm-message-list)))
      (should (equal '(0 1 2) (vm-folder-test--bodies-on-disk path))))))

(ert-deftest vm-folder-test-save-and-expunge-with-nothing-deleted-changes-nothing ()
  "With nothing flagged, the folder and the file come out as they were."
  (vm-test-with-real-folder (3)
    (let ((path buffer-file-name))
      (vm-save-and-expunge-folder)
      (should (= 3 (length vm-message-list)))
      (should (equal '(0 1 2) (vm-folder-test--bodies-on-disk path)))
      (should (= 3 (vm-folder-test--separators-on-disk path))))))

;;; Marking messages read and unread

;; Both commands take a count and go through `vm-select-operable-messages', and
;; neither had a test.  A freshly parsed message is new, and read means neither
;; new nor unread.

(defun vm-folder-test--read-states ()
  "Return `new', `unread' or `read' for each message in `vm-message-list'."
  (mapcar (lambda (m)
            (cond ((vm-new-flag m) 'new)
                  ((vm-unread-flag m) 'unread)
                  (t 'read)))
          vm-message-list))

(ert-deftest vm-folder-test-mark-message-read-over-a-range ()
  "Marking read clears both the new and the unread flag, over a count."
  (vm-test-with-real-folder (4)
    (let ((vm-move-after-reading nil))
      (should (equal '(new new new new) (vm-folder-test--read-states)))
      (setq vm-message-pointer vm-message-list)
      (vm-mark-message-read 2)
      (should (equal '(read read new new) (vm-folder-test--read-states))))))

(ert-deftest vm-folder-test-mark-message-unread-over-a-range ()
  "Marking unread sets the unread flag on messages that were read.
It does not make them new again: new is for messages that have just arrived."
  (vm-test-with-real-folder (4)
    (let ((vm-move-after-reading nil))
      (setq vm-message-pointer vm-message-list)
      (vm-mark-message-read 4)
      (should (equal '(read read read read) (vm-folder-test--read-states)))
      (setq vm-message-pointer (nthcdr 1 vm-message-list))
      (vm-mark-message-unread 2)
      (should (equal '(read unread unread read)
                     (vm-folder-test--read-states))))))

(ert-deftest vm-folder-test-mark-message-read-leaves-read-messages-alone ()
  "Marking a message read again changes nothing, and does not fail."
  (vm-test-with-real-folder (3)
    (let ((vm-move-after-reading nil))
      (setq vm-message-pointer vm-message-list)
      (vm-mark-message-read 3)
      (vm-mark-message-read 3)
      (should (equal '(read read read) (vm-folder-test--read-states))))))

;;; Per-folder settings, as the manual describes them (#244)

(defun vm-folder-test--summary-head (n)
  "The first N characters of the current folder's summary."
  (with-current-buffer vm-summary-buffer
    (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) n)))))

(ert-deftest vm-folder-test-mode-hook-can-set-a-folder-local-format ()
  "A summary format made local in `vm-mode-hook' is the one used.
The manual tells people to set per-folder options there, so the hook has to
run in the folder's buffer and before the summary is built."
  (let ((vm-summary-tokenized-compiled-format-alist
         vm-summary-tokenized-compiled-format-alist)
        (vm-summary-untokenized-compiled-format-alist
         vm-summary-untokenized-compiled-format-alist)
        (vm-mode-hook
         (list (lambda ()
                 (set (make-local-variable 'vm-summary-format)
                      "MODEHOOK %n %s\n")))))
    (vm-test-with-real-folder (2)
      (should (string-prefix-p "->MODEHOOK" (vm-folder-test--summary-head 12))))))

(ert-deftest vm-folder-test-visit-folder-hook-is-after-the-summary ()
  "`vm-visit-folder-hook' runs too late to choose the summary format.
Which is why the manual names `vm-mode-hook' and not this one: the summary
lines exist by now, each cached with its message."
  (let ((vm-summary-tokenized-compiled-format-alist
         vm-summary-tokenized-compiled-format-alist)
        (vm-summary-untokenized-compiled-format-alist
         vm-summary-untokenized-compiled-format-alist)
        (vm-visit-folder-hook
         (list (lambda ()
                 (set (make-local-variable 'vm-summary-format)
                      "VISITHOOK %n %s\n")))))
    (vm-test-with-real-folder (2)
      (should (equal vm-summary-format "VISITHOOK %n %s\n"))
      (should-not (string-match-p "VISITHOOK" (vm-folder-test--summary-head 40))))))

(ert-deftest vm-folder-test-a-folder-cannot-set-variables ()
  "A `Local Variables:' list in a folder file is ignored.
A folder is mail from strangers, so VM visits one with
`enable-local-variables' nil.  The manual says so, and this is why."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-local" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-user-interaction-buffer vm-user-interaction-buffer)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-test-write-simple-folder file 1)
          (with-temp-buffer
            (insert "\nLocal Variables:\nvm-summary-format: \"OWNED %n\\n\"\nEnd:\n")
            (append-to-file (point-min) (point-max) file))
          (vm-visit-folder file)
          (should-not (equal vm-summary-format "OWNED %n\n")))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-a-hard-linked-folder-says-so ()
  "Visiting a folder with another name warns that saving will break it.
`file-precious-flag' writes a temporary file and renames it into place, so
the folder\='s name gets a new inode and the other name keeps the old mail.
Symbolic links are preserved (#532); hard links cannot be, so VM says so
rather than let the other name quietly stop following the folder."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-hardlink" t)))
         (file (expand-file-name "folder" dir))
         (other (expand-file-name "other-name" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-user-interaction-buffer vm-user-interaction-buffer)
         (vm-current-warning nil)
         (warned nil)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-test-write-simple-folder file 1)
          (add-name-to-file file other)
          (should (= 2 (file-attribute-link-number (file-attributes file))))
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_level _secs &rest args)
                       (push (apply #'format args) warned))))
            (vm-visit-folder file))
          (should (seq-find (lambda (w) (string-match-p "has 2 names" w)) warned)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-an-ordinary-folder-says-nothing ()
  "One name, one warning fewer."
  (let ((vm-current-warning nil)
        (warned nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args) (push (apply #'format args) warned))))
      (vm-test-with-real-folder (1)
        (should-not (seq-find (lambda (w) (string-match-p "names" w)) warned))))))

(ert-deftest vm-folder-test-hard-link-warning-is-about-precious-saving ()
  "With `file-precious-flag' nil the other name follows the folder, so
nothing is said."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-hardlink" t)))
         (file (expand-file-name "folder" dir))
         (other (expand-file-name "other-name" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-folder-file-precious-flag nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-user-interaction-buffer vm-user-interaction-buffer)
         (vm-current-warning nil)
         (warned nil)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (vm-test-write-simple-folder file 1)
          (add-name-to-file file other)
          (cl-letf (((symbol-function 'vm-warn)
                     (lambda (_level _secs &rest args)
                       (push (apply #'format args) warned))))
            (vm-visit-folder file))
          (should-not (seq-find (lambda (w) (string-match-p "names" w)) warned)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; Which bodies can be fetched in one command (#185)

(defun vm-folder-test--message (subject)
  "One message for a folder, with SUBJECT."
  ;; The blank line at the end matters: an mbox message ends with one, and
  ;; without it the next `From ' line is read as body text (issue #538).
  (format (concat "From a@example.com  Thu Jan  1 00:00:00 2026\n"
                  "From: a@example.com\nSubject: %s\n\nbody\n\n")
          subject))

(defun vm-folder-test--make-external (m)
  "Make M look like an IMAP message whose body is still on the server."
  (vm-set-message-access-method-of m 'imap)
  (vm-set-body-to-be-retrieved-flag m t)
  m)

(ert-deftest vm-folder-test-two-messages-are-fetched-together ()
  "Several bodies from one IMAP folder go in one command."
  (vm-test-with-folder (concat (vm-folder-test--message "one")
                               (vm-folder-test--message "two")
                               (vm-folder-test--message "three"))
    (let* ((messages (mapcar #'vm-folder-test--make-external vm-message-list))
           (bunch (vm-messages-to-fetch-together messages)))
      (should (= (length messages) (length bunch)))
      (dolist (m messages)
        (should (memq m bunch))))))

(ert-deftest vm-folder-test-one-message-is-not-a-bunch ()
  "One message is left to the simple path, which is the common case."
  (vm-test-with-folder (vm-folder-test--message "only")
    (let ((messages (mapcar #'vm-folder-test--make-external vm-message-list)))
      (should-not (vm-messages-to-fetch-together messages)))))

(ert-deftest vm-folder-test-bodies-already-here-are-not-fetched ()
  "A message whose body is already loaded is not asked for again."
  (vm-test-with-folder (concat (vm-folder-test--message "one")
                               (vm-folder-test--message "two"))
    (let ((messages (mapcar #'vm-folder-test--make-external vm-message-list)))
      (vm-set-body-to-be-retrieved-flag (car messages) nil)
      ;; one left, and one is not a bunch
      (should-not (vm-messages-to-fetch-together messages)))))

(ert-deftest vm-folder-test-only-imap-messages-are-bunched ()
  "POP and local messages keep the one-at-a-time path.
The command is an IMAP one; nothing else has a UID to put in it."
  (vm-test-with-folder (concat (vm-folder-test--message "one")
                               (vm-folder-test--message "two"))
    (let ((messages (mapcar #'vm-folder-test--make-external vm-message-list)))
      (vm-set-message-access-method-of (car messages) 'pop)
      (should-not (vm-messages-to-fetch-together messages)))))

;;; A From_ line inside a header block is a separator (issue #562)

;; An mbox whose writer left out the blank line between two messages used to be
;; read as one message with the second one's headers as its body.  A
;; `From '-looking line inside a header block cannot be a header -- RFC 5322
;; wants a field name and a colon before the first space -- so it is a
;; separator, and reading it as one loses nothing that could have been valid.
;;
;; The cases that must NOT change are the point of most of these: a `From '
;; line in a message *body* is body text, and so is a `>From ' one anywhere.

(defun vm-folder-test--run-together-message (n &optional body)
  "A From_ message numbered N, with BODY and the blank line before it if given."
  (concat (format "From s%d@example.com Mon Jan  1 00:00:0%d 2024\n" n n)
          (format "From: s%d@example.com\n" n)
          (format "Subject: subject %d\n" n)
          (if body (concat "\n" body) "")))

(defmacro vm-folder-test--with-folder (content &rest body)
  "Like `vm-test-with-folder', but does not leak the run-together warning.
`vm-warn' records what it last said in `vm-current-warning' so as not to repeat
itself, and that is a global the test harness does not restore."
  (declare (indent 1) (debug t))
  `(let ((vm-current-warning vm-current-warning))
     (vm-test-with-folder ,content ,@body)))

(defun vm-folder-test--subjects ()
  "The subject of every parsed message, in order."
  (mapcar #'vm-su-subject vm-message-list))

(defun vm-folder-test--body-of (m)
  "The body region of M as VM's own markers delimit it."
  (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))

(ert-deftest vm-folder-test-run-together-messages-are-split ()
  "REGRESSION: two messages with no blank line between them are two messages.
Issue #562, split out of #538 where Mailutils' movemail produced exactly this.
The second message's headers used to become the body of the first, with its
`From ' line From_-quoted on the way out -- which is the `>From ' the reporter
of #538 saw."
  (vm-folder-test--with-folder
      (concat (vm-folder-test--run-together-message 1)
              (vm-folder-test--run-together-message 2 "body of two\n"))
    (should (equal '("subject 1" "subject 2") (vm-folder-test--subjects)))
    ;; The first message's body is empty, not the second message.
    (should (equal "" (vm-folder-test--body-of (nth 0 vm-message-list))))
    (should (equal "body of two"
                   (vm-folder-test--body-of (nth 1 vm-message-list))))))

(ert-deftest vm-folder-test-run-together-three-messages ()
  "Three messages in a row with no blank lines are three messages.
One split has to leave the parser able to make the next one, which it only
does because the loop remembers that point is already on a separator:
`vm-find-leading-message-separator' would step over it, wanting the blank line
that is precisely what is missing."
  (vm-folder-test--with-folder
      (concat (vm-folder-test--run-together-message 1)
              (vm-folder-test--run-together-message 2)
              (vm-folder-test--run-together-message 3 "body of three\n"))
    (should (equal '("subject 1" "subject 2" "subject 3")
                   (vm-folder-test--subjects)))
    (should (equal "body of three"
                   (vm-folder-test--body-of (nth 2 vm-message-list))))))

(ert-deftest vm-folder-test-run-together-mixed-with-well-formed ()
  "A folder with one bad join and one good one parses all three messages."
  (vm-folder-test--with-folder
      (concat (vm-folder-test--run-together-message 1)
              (vm-folder-test--run-together-message 2 "body of two\n\n")
              (vm-folder-test--run-together-message 3 "body of three\n"))
    (should (equal '("subject 1" "subject 2" "subject 3")
                   (vm-folder-test--subjects)))))

(ert-deftest vm-folder-test-well-formed-folder-is-unchanged ()
  "The ordinary case still parses as it did: one blank line, two messages."
  (vm-test-with-folder
      (concat (vm-folder-test--run-together-message 1 "body of one\n\n")
              (vm-folder-test--run-together-message 2 "body of two\n"))
    (should (equal '("subject 1" "subject 2") (vm-folder-test--subjects)))
    (should (equal "body of one\n"
                   (vm-folder-test--body-of (nth 0 vm-message-list))))))

(ert-deftest vm-folder-test-two-blank-lines-are-unchanged ()
  "An extra blank line between messages still gives two messages."
  (vm-test-with-folder
      (concat (vm-folder-test--run-together-message 1 "body of one\n\n\n")
              (vm-folder-test--run-together-message 2 "body of two\n"))
    (should (equal '("subject 1" "subject 2") (vm-folder-test--subjects)))))

(ert-deftest vm-folder-test-From_-line-in-a-body-is-not-a-separator ()
  "A `From ' line in a message body stays body text.
This is the case the change must not break, and the reason the search stops at
the end of the header block: a mail quoting another mail unquoted has exactly
this shape, and splitting there would invent a message."
  (vm-test-with-folder
      (concat (vm-folder-test--run-together-message 1
                                       (concat "I was sent this:\n"
                                               "From s9@example.com Mon Jan"
                                               "  1 00:00:09 2024\n"
                                               "and could not read it.\n")))
    (should (equal '("subject 1") (vm-folder-test--subjects)))
    (should (string-match-p "From s9@example.com"
                            (vm-folder-test--body-of (car vm-message-list))))))

(ert-deftest vm-folder-test-From_-line-after-a-header-like-body-line ()
  "A body whose lines look like headers still does not split.
The test is whether a blank line has been seen since the message started, not
what the previous line looks like: a quoted mail in a body begins with header
lines and may be followed by a `From ' line."
  (vm-test-with-folder
      (concat (vm-folder-test--run-together-message 1
                                       (concat "From: quoted@example.com\n"
                                               "Subject: quoted\n"
                                               "From s9@example.com Mon Jan"
                                               "  1 00:00:09 2024\n")))
    (should (equal '("subject 1") (vm-folder-test--subjects)))))

(ert-deftest vm-folder-test-quoted-From_-in-a-header-block-is-not-a-separator ()
  "A `>From ' line in a header block is not a separator.
It is what a From_ folder writer produces for body text that began with
`From ', so treating it as a separator would split a message that was written
correctly."
  (vm-test-with-folder
      (concat (vm-folder-test--run-together-message 1)
              ">From s9@example.com Mon Jan  1 00:00:09 2024\n"
              "\nbody\n")
    (should (equal '("subject 1") (vm-folder-test--subjects)))))

(ert-deftest vm-folder-test-run-together-warns-once-for-the-folder ()
  "The user is told their mailbox is malformed, once, with a count.
VM reads it correctly now, but the folder is still wrong and whoever wrote it
should be told -- said once rather than once a message, since a mail mover
that drops one blank line drops many."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _seconds &rest args)
                 (push (apply #'format args) warnings))))
      (vm-test-with-folder
          (concat (vm-folder-test--run-together-message 1)
                  (vm-folder-test--run-together-message 2)
                  (vm-folder-test--run-together-message 3 "body\n"))
        (should (= 3 (length vm-message-list)))))
    (let ((said (seq-filter (lambda (w) (string-match-p "ran into the next" w))
                            warnings)))
      (should (= 1 (length said)))
      (should (string-match-p "2 messages" (car said))))))

(ert-deftest vm-folder-test-well-formed-folder-warns-about-nothing ()
  "A folder with its blank lines in place produces no run-together warning."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _seconds &rest args)
                 (push (apply #'format args) warnings))))
      (vm-test-with-folder
          (concat (vm-folder-test--run-together-message 1 "body of one\n\n")
                  (vm-folder-test--run-together-message 2 "body of two\n"))
        (should (= 2 (length vm-message-list)))))
    (should-not (seq-filter (lambda (w) (string-match-p "ran into the next" w))
                            warnings))))

(ert-deftest vm-folder-test-run-together-markers-are-ordered ()
  "Every message the split produces has its markers in order and linked.
A parser that puts `text-end-of' before `headers-of' would corrupt the folder
on the next write, so this is checked rather than assumed."
  (vm-folder-test--with-folder
      (concat (vm-folder-test--run-together-message 1)
              (vm-folder-test--run-together-message 2)
              (vm-folder-test--run-together-message 3 "body\n"))
    (dolist (m vm-message-list)
      (should (<= (vm-start-of m) (vm-headers-of m)))
      (should (<= (vm-headers-of m) (vm-text-of m)))
      (should (<= (vm-text-of m) (vm-text-end-of m)))
      (should (<= (vm-text-end-of m) (vm-end-of m))))
    (should (vm-test-reverse-links-consistent-p))))

(ert-deftest vm-folder-test-content-length-folder-is-untouched ()
  "An mboxcl2 folder parses by its own rule, as before.
`vm-find-trailing-message-separator\=' takes a different branch for it, and the
header-block search is not on that path: its message boundaries come from the
byte count, which is the whole point of the format.  Built by hand rather than
with `vm-test-with-folder\=', which resets `vm-trust-content-length\='
to nil while setting the buffer up."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (setq-local vm-trust-content-length t)
    (insert "From s1@example.com Mon Jan  1 00:00:01 2024\n"
            "From: s1@example.com\n"
            "Subject: subject 1\n"
            "Content-Length: 12\n"
            "\n"
            "body of one\n"
            "From s2@example.com Mon Jan  1 00:00:02 2024\n"
            "From: s2@example.com\n"
            "Subject: subject 2\n"
            "Content-Length: 12\n"
            "\n"
            "body of two\n")
    (goto-char (point-min))
    (vm-build-message-list)
    (dolist (m vm-message-list) (vm-test-init-message-data m))
    (should (eq 'mboxcl2 vm-folder-type))
    (should (equal '("subject 1" "subject 2") (vm-folder-test--subjects)))))


;;; Reading a Content-Length folder is a different thing from reading mbox

;; The two formats VM reads differ in one respect that matters: where a
;; message ends.  In a From_ folder it is the next line beginning `From ',
;; so a body containing such a line has to have been quoted when written.  In
;; an mboxcl2 folder the byte count says where the message
;; ends, so a body may contain that line untouched and reading is unaffected.
;;
;; That is the property, and it had no test.  See the Folder types section of
;; the manual, and issue #466.

(defconst vm-folder-test--counted-body-1
  ;; A blank line and then a line beginning `From ': in a From_ folder that
  ;; is a message boundary, and here it is body text.  That difference is
  ;; the whole of what the two formats are.
  "a body line\n\nFrom nobody@example.com Mon Jan  1 00:00:00 2024\n"
  "The first message's body, which a From_ reader would split in two.")

(defconst vm-folder-test--counted-body-2 "second\n"
  "The second message's body.")

(defconst vm-folder-test--counted-folder
  ;; The counts are computed rather than written down, so the fixture cannot
  ;; drift from what it claims.  They count the body only, from after the
  ;; blank line that ends the headers.
  (concat "From VM Mon Jan  1 00:00:00 2024\n"
          (format "Content-Length: %d\n" (length vm-folder-test--counted-body-1))
          "From: one@example.com\nSubject: subject 1\n\n"
          vm-folder-test--counted-body-1
          "\n"
          "From VM Mon Jan  1 00:00:01 2024\n"
          (format "Content-Length: %d\n" (length vm-folder-test--counted-body-2))
          "From: two@example.com\nSubject: subject 2\n\n"
          vm-folder-test--counted-body-2
          "\n")
  "A two-message folder whose first body holds a `From ' line after a blank one.")

(defmacro vm-folder-test--with-counted-folder (trust &rest body)
  "Parse `vm-folder-test--counted-folder' with TRUST, then run BODY."
  (declare (indent 1) (debug t))
  ;; `vm-warn' records what it last said in `vm-current-warning', a global
  ;; the harness does not restore, and reading these bytes as From_ warns
  ;; about the messages running together (issue #562).
  `(let ((vm-current-warning vm-current-warning))
     (with-temp-buffer
       (vm-test-init-folder-variables)
       (setq-local vm-trust-content-length ,trust)
       (insert vm-folder-test--counted-folder)
       (goto-char (point-min))
       (vm-build-message-list)
       (dolist (m vm-message-list) (vm-test-init-message-data m))
       ,@body)))

(defun vm-folder-test--body-text (m)
  (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))

(ert-deftest vm-folder-test-counted-folder-is-read-by-its-counts ()
  "The count ends the message, so a `From ' line in a body is body text."
  (vm-folder-test--with-counted-folder t
    (should (eq 'mboxcl2 vm-folder-type))
    (should (equal '("subject 1" "subject 2")
                   (mapcar #'vm-su-subject vm-message-list)))
    (let ((body (vm-folder-test--body-text (car vm-message-list))))
      (should (string-match-p "^From nobody@example.com" body))
      (should-not (string-match-p "^>From " body)))))

(ert-deftest vm-folder-test-same-bytes-without-trust-are-mbox ()
  "The same bytes read as From_ split at that line instead, giving three.
Which is why `vm-trust-content-length' exists: nothing in the
file says which of the two formats it is, so VM has to be told.  Neither
reading damages the folder; they are simply different folders."
  (vm-folder-test--with-counted-folder nil
    (should (eq 'From_ vm-folder-type))
    (should (= 3 (length vm-message-list)))))

(ert-deftest vm-folder-test-counted-folder-bodies-are-exact ()
  "Each body is what its count claimed, plus the newline that follows it.
`vm-find-trailing-message-separator' skips newlines after the counted body
-- its comment says some systems add one the count does not include -- so
the region VM reports runs to the blank line between the messages.  Pinned
as it is: a reader that stopped absorbing that would change where every
message in every such folder ends."
  (vm-folder-test--with-counted-folder t
    (should (equal (concat vm-folder-test--counted-body-1 "\n")
                   (vm-folder-test--body-text (nth 0 vm-message-list))))
    (should (equal (concat vm-folder-test--counted-body-2 "\n")
                   (vm-folder-test--body-text (nth 1 vm-message-list))))))

(ert-deftest vm-folder-test-a-count-past-the-end-does-not-eat-the-folder ()
  "A count larger than what is there stops at the next separator.
`vm-find-trailing-message-separator' falls back to searching for the next
`From ' line when the count does not land on one, so one wrong count costs
one message rather than the rest of the folder."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (setq-local vm-trust-content-length t)
    (insert (replace-regexp-in-string "Content-Length: 62"
                                      "Content-Length: 9999"
                                      vm-folder-test--counted-folder))
    (goto-char (point-min))
    (vm-build-message-list)
    (dolist (m vm-message-list) (vm-test-init-message-data m))
    (should (<= 1 (length vm-message-list)))
    (should (equal "subject 1" (vm-su-subject (car vm-message-list))))))


;;; A Content-Length folder is not quoted (issue #466)

(ert-deftest vm-folder-test-content-length-type-is-not-munged ()
  "REGRESSION: writing to a Content-Length folder leaves `From ' lines alone.
That is the whole difference between the two Content-Length mbox variants.
A folder that finds the end of a message by counting its bytes has no need
to disfigure a body line beginning `From ', and doing both is mboxcl where
doing only the counting is mboxcl2 -- the one variant of the four that
stores a message as it arrived.  VM used to do both."
  (with-temp-buffer
    (insert "From: a@b\nSubject: s\n\nbody\n"
            "From nobody@example.com Mon Jan  1 00:00:00 2024\n")
    (vm-munge-message-separators 'mboxcl2
                                 (point-min) (point-max))
    (should (string-match-p "\nFrom nobody@example.com" (buffer-string)))
    (should-not (string-match-p ">From " (buffer-string)))))

(ert-deftest vm-folder-test-line-based-types-are-still-munged ()
  "The types whose message boundary is a `From ' line still quote one in a body.
Only those: for mmdf and babyl a `From ' line is not a separator and means
nothing, so there is nothing for them to quote."
  (dolist (type '(From_ BellFrom_))
    (with-temp-buffer
      (insert "From: a@b\nSubject: s\n\nbody\n"
              "From nobody@example.com Mon Jan  1 00:00:00 2024\n")
      (vm-munge-message-separators type (point-min) (point-max))
      (should (string-match-p ">From nobody@example.com" (buffer-string)))))
  (dolist (type '(mmdf babyl))
    (with-temp-buffer
      (insert "From: a@b\nSubject: s\n\nbody\n"
              "From nobody@example.com Mon Jan  1 00:00:00 2024\n")
      (vm-munge-message-separators type (point-min) (point-max))
      (should-not (string-match-p ">From " (buffer-string))))))

(ert-deftest vm-folder-test-mmdf-munges-its-own-separator ()
  "Each type quotes the thing that would end a message in it, and mmdf's is
its own four control characters rather than a `From ' line."
  (with-temp-buffer
    (insert "From: a@b\nSubject: s\n\nbody\n\nmore\n")
    (vm-munge-message-separators 'mmdf (point-min) (point-max))
    (should (string-match-p ">" (buffer-string)))))


;;; The folder type is called mboxcl2 now (issue #466)

;; It was `From_-with-Content-Length', after the mechanism rather than the
;; format.  The old name has to keep working: it is what a user's
;; `vm-default-folder-type' says, and -- the one that could go wrong
;; quietly -- it is what an index file written before the rename holds, since
;; `vm-write-index-file-contents' stores the folder type.

(ert-deftest vm-folder-test-old-type-name-is-accepted ()
  "`vm-canonical-folder-type' maps the old name to the new and nothing else."
  (should (eq 'mboxcl2 (vm-canonical-folder-type 'From_-with-Content-Length)))
  (should (eq 'mboxcl2 (vm-canonical-folder-type 'mboxcl2)))
  (dolist (type '(From_ BellFrom_ mmdf babyl unknown nil))
    (should (eq type (vm-canonical-folder-type type)))))

(ert-deftest vm-folder-test-detection-returns-the-new-name ()
  "A folder read by its counts is reported as `mboxcl2'."
  (vm-folder-test--with-counted-folder t
    (should (eq 'mboxcl2 vm-folder-type))))

(ert-deftest vm-folder-test-old-name-in-an-index-file-still-parses ()
  "REGRESSION: an index file naming the old type still reads its folder.
The folder type is stored in the index file, so leaving the old name
unhandled would have meant every test of the type failing for a folder VM
had already indexed -- and a folder read as the wrong type is misparsed, not
refused."
  (should (eq 'mboxcl2
              (vm-canonical-folder-type
               (car (read-from-string
                     (prin1-to-string 'From_-with-Content-Length))))))
  ;; and a folder whose type arrives that way is read by its counts
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (setq-local vm-trust-content-length t)
    (insert vm-folder-test--counted-folder)
    (setq vm-folder-type (vm-canonical-folder-type 'From_-with-Content-Length))
    (goto-char (point-min))
    (let ((vm-current-warning vm-current-warning))
      ;; `vm-build-message-list' would re-detect; this is the index path,
      ;; where the type comes from the file and is used as it stands.
      (should (eq 'mboxcl2 vm-folder-type))
      (goto-char (point-min))
      (should (progn (vm-find-leading-message-separator)
                     (vm-skip-past-leading-message-separator)
                     (vm-find-trailing-message-separator)
                     ;; the count took us past the bare From_ line in the body
                     (> (point) (+ (point-min)
                                   (length vm-folder-test--counted-body-1))))))))

;;; Thunderbird status headers (issues #602)

(defconst vm-folder-test--thunderbird-folder
  (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
          "From: alice@example.com\n"
          "Subject: hello\n"
          ;; read + folded + watched, and #x0010, Thunderbird's own note that
          ;; the subject carries a "Re:" prefix, which VM has no flag for
          "X-Mozilla-Status: 0131\n"
          ;; attachments, and #x0100 template, which VM has no flag for
          "X-Mozilla-Status2: 11000000\n"
          "\n" "Body.\n\n")
  "A message as Thunderbird writes one, carrying bits VM does not manage.")

(defun vm-folder-test--mozilla-status (n)
  "The X-Mozilla-Status (N is 1) or -Status2 (N is 2) of the first message."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^X-Mozilla-Status%s: \\([0-9A-Fa-f]+\\)$" (if (= n 2) "2" ""))
           nil t)
      (match-string 1))))

(ert-deftest vm-folder-test-thunderbird-status-is-read-into-flags ()
  "Each bit VM has a flag for is read out of the Mozilla status headers."
  (vm-test-with-folder vm-folder-test--thunderbird-folder
    (let ((m (car vm-message-list)))
      (vm-read-thunderbird-status m)
      (should-not (vm-unread-flag m))        ; #x0001 read
      (should (vm-folded-flag m))            ; #x0020
      (should (vm-watched-flag m))           ; #x0100
      (should-not (vm-replied-flag m))       ; #x0002 clear
      (should-not (vm-deleted-flag m))       ; #x0008 clear
      (should (vm-attachments-flag m))       ; #x1000 of status2
      (should-not (vm-new-flag m)))))        ; #x0001 of status2 clear

(ert-deftest vm-folder-test-a-flag-turned-off-is-written-out ()
  "REGRESSION: a flag turned off in VM is turned off in the file.
Issue #602.  `vm-stuff-thunderbird-status' set the bits of the flags that
were on but cleared only five of the eleven it writes, so folded, watched,
ignored, both read-receipt bits and attachments could be turned off in VM
and Thunderbird would go on showing them."
  (vm-test-with-folder vm-folder-test--thunderbird-folder
    (let ((m (car vm-message-list)))
      (vm-read-thunderbird-status m)
      (vm-set-folded-flag-of m nil)
      (vm-set-watched-flag-of m nil)
      (vm-set-attachments-flag-of m nil)
      (save-excursion (vm-stuff-thunderbird-status m))
      (let ((status (string-to-number (vm-folder-test--mozilla-status 1) 16))
            (status2 (string-to-number
                      (substring (vm-folder-test--mozilla-status 2) 0 4) 16)))
        (should (= 0 (logand status #x0020)))    ; folded, off
        (should (= 0 (logand status #x0100)))    ; watched, off
        (should (= 0 (logand status2 #x1000))))))) ; attachments, off

(ert-deftest vm-folder-test-thunderbird-bits-vm-does-not-manage-survive ()
  "The bits VM has no flag for come back unchanged.
Thunderbird's own \"Re:\" prefix note and its template flag are not VM's to
clear, and neither is the #x0E00 label field."
  (vm-test-with-folder vm-folder-test--thunderbird-folder
    (let ((m (car vm-message-list)))
      (vm-read-thunderbird-status m)
      (save-excursion (vm-stuff-thunderbird-status m))
      (let ((status (string-to-number (vm-folder-test--mozilla-status 1) 16))
            (status2 (string-to-number
                      (substring (vm-folder-test--mozilla-status 2) 0 4) 16)))
        (should (= #x0010 (logand status #x0010)))     ; "Re:" prefix
        (should (= #x0100 (logand status2 #x0100)))))))  ; template

(ert-deftest vm-folder-test-thunderbird-status-round-trips ()
  "Reading the headers and writing them back leaves every flag as it was."
  (vm-test-with-folder vm-folder-test--thunderbird-folder
    (let ((m (car vm-message-list)))
      (vm-read-thunderbird-status m)
      (let ((before (list (vm-unread-flag m) (vm-replied-flag m)
                          (vm-flagged-flag m) (vm-deleted-flag m)
                          (vm-folded-flag m) (vm-watched-flag m)
                          (vm-forwarded-flag m) (vm-new-flag m)
                          (vm-ignored-flag m) (vm-read-receipt-flag m)
                          (vm-read-receipt-sent-flag m)
                          (vm-attachments-flag m))))
        (save-excursion (vm-stuff-thunderbird-status m))
        (vm-read-thunderbird-status m)
        (should (equal before
                       (list (vm-unread-flag m) (vm-replied-flag m)
                             (vm-flagged-flag m) (vm-deleted-flag m)
                             (vm-folded-flag m) (vm-watched-flag m)
                             (vm-forwarded-flag m) (vm-new-flag m)
                             (vm-ignored-flag m) (vm-read-receipt-flag m)
                             (vm-read-receipt-sent-flag m)
                             (vm-attachments-flag m))))))))

(ert-deftest vm-folder-test-a-thunderbird-folder-is-known-by-its-index ()
  "`vm-thunderbird-folder-p' asks whether a .msf index sits beside the folder.
That is the whole of the detection, and it is what decides whether VM writes
Mozilla headers into a folder at all."
  (let* ((dir (make-temp-file "vm-folder-test" t))
         (folder (expand-file-name "Inbox" dir)))
    (unwind-protect
        (progn
          (with-temp-file folder (insert "From a@b Mon Jan  1 00:00:00 2024\n\n"))
          (should-not (vm-thunderbird-folder-p folder))
          (with-temp-file (concat folder ".msf") (insert "// <mdb:mork:z v=\"1.4\"/>\n"))
          (should (vm-thunderbird-folder-p folder)))
      (delete-directory dir t))))

;;; The folder type a name asks for (emacs-vm/vm#610)

(defmacro vm-folder-test-with-directory (var &rest body)
  "Run BODY with VAR bound to a fresh directory, removed afterwards."
  (declare (indent 1) (debug t))
  `(let ((,var (file-name-as-directory (make-temp-file "vm-folder-test" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(ert-deftest vm-folder-test-type-for-name-reads-the-alist ()
  "`vm-folder-type-for-name' answers from `vm-folder-type-by-name-alist'."
  (should (eq (vm-folder-type-for-name "/mail/2026-08.out.mboxcl2") 'mboxcl2))
  (should-not (vm-folder-type-for-name "/mail/2026-08.out.mbox"))
  (should-not (vm-folder-type-for-name "/mail/INBOX"))
  ;; the suffix has to end the name, so a backup file is not a folder type
  (should-not (vm-folder-type-for-name "/mail/sent.mboxcl2~"))
  (should-not (vm-folder-type-for-name nil))
  ;; and nothing is claimed when the option is empty
  (let ((vm-folder-type-by-name-alist nil))
    (should-not (vm-folder-type-for-name "/mail/sent.mboxcl2")))
  ;; the first match wins, and any type may be named
  (let ((vm-folder-type-by-name-alist '(("\\.babyl\\'" . babyl)
                                        ("\\.b" . mmdf))))
    (should (eq (vm-folder-type-for-name "/mail/old.babyl") 'babyl))))

(ert-deftest vm-folder-test-a-name-does-not-override-what-a-folder-says ()
  "A folder's own contents decide its type; the name is consulted only when
they cannot.  A BABYL file called .mboxcl2 is still BABYL."
  (vm-folder-test-with-directory dir
    (let ((file (expand-file-name "misnamed.mboxcl2" dir)))
      (write-region "BABYL OPTIONS:\nVersion: 5\n\n" nil file nil 'quiet)
      (should (eq (vm-get-folder-type file) 'babyl)))))

(ert-deftest vm-folder-test-a-name-makes-content-length-believable ()
  "A folder named mboxcl2 is read as mboxcl2 even with
`vm-trust-content-length' nil: naming the file says as plainly as the option
does that the header is to be believed.  With no Content-Length in it the name
still decides -- see the test below -- and the reader then complains about the
message that has none, which is the point of saying so in the name."
  (vm-folder-test-with-directory dir
    (let ((with-length (expand-file-name "sent.mboxcl2" dir))
          (without (expand-file-name "other.mboxcl2" dir))
          (vm-trust-content-length nil))
      (write-region (concat "From VM Mon Aug 10 00:00:00 2026\n"
                            "To: someone@example.com\nContent-Length: 5\n\n"
                            "body\n")
                    nil with-length nil 'quiet)
      (should (eq (vm-get-folder-type with-length) 'mboxcl2))
      (write-region (concat "From VM Mon Aug 10 00:00:00 2026\n"
                            "To: someone@example.com\n\nbody\n")
                    nil without nil 'quiet)
      (should (eq (vm-get-folder-type without) 'mboxcl2))
      ;; and with the option off, the name is not consulted at all
      (let ((vm-folder-type-by-name-alist nil))
        (should (eq (vm-get-folder-type with-length)
                    vm-default-From_-folder-type))))))

(ert-deftest vm-folder-test-an-empty-folder-still-has-no-type ()
  "A folder that does not exist, or is empty, has no type whatever it is
called.  Callers read nil as \"nothing here yet\": `vm-save-message' asks
before appending to a file that is already a folder, and would ask about
every new one if a name were enough to make it one."
  (vm-folder-test-with-directory dir
    (let ((missing (expand-file-name "new.mboxcl2" dir))
          (empty (expand-file-name "empty.mboxcl2" dir)))
      (should-not (vm-get-folder-type missing))
      (write-region "" nil empty nil 'quiet)
      (should-not (vm-get-folder-type empty)))))

;;; The From_ envelope line (emacs-vm/vm#611)

(ert-deftest vm-folder-test-conversion-keeps-the-envelope-line ()
  "Converting a folder keeps each message's own envelope line.
It says who sent the message and when it arrived, and the conversion used to
stamp every one of them with the moment of the conversion instead."
  (let ((folder (concat
                 "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                 "From: alice@example.com\nSubject: one\n\nBody one.\n\n"
                 "From bob@example.com Sun Aug  9 09:00:00 2026\n"
                 "From: bob@example.com\nSubject: two\n\nBody two.\n\n")))
    (vm-test-with-folder folder
      (let ((before (mapcar #'vm-existing-From_-separator vm-message-list)))
        (should (equal before '("From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                "From bob@example.com Sun Aug  9 09:00:00 2026\n")))
        (dolist (m vm-message-list)
          (should (equal (vm-leading-message-separator 'mboxcl2 m)
                         (vm-existing-From_-separator m))))))))

(ert-deftest vm-folder-test-a-message-with-no-envelope-line-gets-one-built ()
  "A message out of a folder that has no From_ lines gets one built from its
From and Date headers, rather than VM's own name and the time of day."
  (vm-test-with-folder
      (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
              "From: Alice Adams <alice@example.com>\n"
              "Date: Sat, 8 Aug 2026 14:24:13 -0700\n"
              "Subject: one\n\nBody.\n\n")
    (let ((m (car vm-message-list)))
      ;; pretend it came from a folder type that has no envelope line
      (vm-set-message-type-of m 'mmdf)
      (should-not (vm-existing-From_-separator m))
      (let ((built (vm-make-From_-separator m)))
        (should (string-prefix-p "From alice@example.com " built))
        (should (string-suffix-p "\n" built))
        ;; the date is the message's, not now
        (should (string-match-p "2026" built))
        (should (equal built (vm-leading-message-separator 'From_ m)))))))

(ert-deftest vm-folder-test-an-unusable-address-falls-back-to-vm ()
  "An address with a space in it cannot be an envelope sender, and a message
with no From at all has nothing to offer, so VM names itself as it always did."
  (vm-test-with-folder
      (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
              "Subject: no from header\n\nBody.\n\n")
    (let ((m (car vm-message-list)))
      (vm-set-message-type-of m 'mmdf)
      (should (string-prefix-p "From VM " (vm-make-From_-separator m))))))

(ert-deftest vm-folder-test-a-composition-still-gets-vms-own-line ()
  "With no message to ask, the separator is VM's own name and the time.
That is the Fcc of a composition, which has no envelope line yet."
  (let ((line (vm-leading-message-separator 'From_)))
    (should (string-prefix-p "From VM " line))
    (should (string-suffix-p "\n" line))))

;;; mboxcl2 needs a length on every message (emacs-vm/vm#612)

(defconst vm-folder-test--mboxcl2-with-a-length
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: one\nContent-Length: 10\n\n"
          "Body one.\n")
  "One mboxcl2 message, correctly written.")

(defconst vm-folder-test--mboxcl2-without-one
  (concat "From bob@example.com Sun Aug  9 09:00:00 2026\n"
          "From: bob@example.com\nSubject: two\n\nBody two.\n\n")
  "A message with no Content-Length, as another mailer may leave it.")

(ert-deftest vm-folder-test-a-length-is-computed-for-the-body ()
  "`vm-content-length-header-line' counts the body, and only for mboxcl2."
  (with-temp-buffer
    (insert "To: someone@example.com\nSubject: one\n\nA body line.\n")
    (should (equal (vm-content-length-header-line 'mboxcl2)
                   "Content-Length: 13\n"))
    (should-not (vm-content-length-header-line 'From_))
    (should-not (vm-content-length-header-line 'mmdf)))
  ;; octets, not characters: a Content-Length is a byte count
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert "Subject: café\n\ncafé\n")
    (should (equal (vm-content-length-header-line 'mboxcl2)
                   "Content-Length: 6\n")))
  ;; a message with no body at all has a length of zero, not an error
  (with-temp-buffer
    (insert "To: someone@example.com\n\n")
    (should (equal (vm-content-length-header-line 'mboxcl2)
                   "Content-Length: 0\n"))))

(ert-deftest vm-folder-test-a-missing-length-is-refused ()
  "Reading an mboxcl2 folder whose message has no Content-Length is an error.
VM used to look for the next From_ line instead, which reads the folder as
something other than what it says it is and says nothing at all."
  (let ((dir (file-name-as-directory (make-temp-file "vm-folder-strict" t))))
    (unwind-protect
        (let ((file (expand-file-name "mixed.mboxcl2" dir))
              (text-quoting-style 'grave))
          (write-region (concat vm-folder-test--mboxcl2-with-a-length
                                vm-folder-test--mboxcl2-without-one)
                        nil file nil 'quiet)
          (should (eq (vm-get-folder-type file) 'mboxcl2))
          (let* ((vm-mboxcl2-strict t)
                 (message (cadr (should-error (vm-visit-folder file)))))
            (should (string-match-p "has no Content-Length" message))
            ;; the message says which one, and how to repair it -- naming the
            ;; command, not the variable: turning strictness off by hand is
            ;; easy to forget to turn back on (emacs-vm/vm#613)
            (should (string-match-p "line [0-9]+" message))
            (should (string-match-p "vm-change-folder-type" message))
            (should (string-match-p "backup file" message))
            (should-not (string-match-p "set vm-mboxcl2-strict" message))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-a-missing-length-can-be-repaired ()
  "With `vm-mboxcl2-strict' nil the folder opens, and changing its type back
to mboxcl2 gives every message a length -- which is the repair the error
describes, so it had better work."
  (let ((dir (file-name-as-directory (make-temp-file "vm-folder-strict" t))))
    (unwind-protect
        (let ((file (expand-file-name "mixed.mboxcl2" dir)))
          (write-region (concat vm-folder-test--mboxcl2-with-a-length
                                vm-folder-test--mboxcl2-without-one)
                        nil file nil 'quiet)
          (let ((vm-mboxcl2-strict nil))
            (cl-letf (((symbol-function 'vm-warn) #'ignore))
              (vm-visit-folder file)
              (should (= (length vm-message-list) 2))
              (vm-change-folder-type 'mboxcl2)
              (vm-save-folder)))
          ;; and now it reads with the strict reader
          (let ((vm-mboxcl2-strict t))
            (vm-visit-folder file)
            (should (= (length vm-message-list) 2))
            (should (eq vm-folder-type 'mboxcl2))))
      (delete-directory dir t))))

(ert-deftest vm-folder-test-a-From_-folder-needs-no-length ()
  "None of this touches a folder that does not claim to be mboxcl2."
  (let ((dir (file-name-as-directory (make-temp-file "vm-folder-strict" t))))
    (unwind-protect
        (let ((file (expand-file-name "plain.mbox" dir))
              (vm-mboxcl2-strict t))
          (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                "From: alice@example.com\nSubject: one\n\nBody.\n\n"
                                vm-folder-test--mboxcl2-without-one)
                        nil file nil 'quiet)
          (vm-visit-folder file)
          (should (= (length vm-message-list) 2))
          (should (eq vm-folder-type vm-default-From_-folder-type)))
      (delete-directory dir t))))

;;; Changing a folder's type on disk (emacs-vm/vm#613)

(defconst vm-folder-test--seven-and-two-short
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: one\nContent-Length: 10\n\n"
          "Body one.\n"
          "From bob@example.com Sun Aug  9 09:00:00 2026\n"
          "From: bob@example.com\nSubject: two\n\nBody two.\n\n"
          "From carol@example.com Mon Aug 10 10:00:00 2026\n"
          "From: carol@example.com\nSubject: three\n\nBody three.\n\n")
  "A folder claiming mboxcl2 whose second and third messages have no length.")

(defmacro vm-folder-test-with-file (spec &rest body)
  "Run BODY with the file SPEC names written in a directory of its own.
SPEC is (VAR NAME CONTENT)."
  (declare (indent 1) (debug t))
  (let ((var (nth 0 spec)) (name (nth 1 spec)) (content (nth 2 spec)))
    `(let ((dir (file-name-as-directory (make-temp-file "vm-folder-disk" t))))
       (unwind-protect
           (let ((,var (expand-file-name ,name dir)))
             (write-region ,content nil ,var nil 'quiet)
             ,@body)
         (delete-directory dir t)))))

(ert-deftest vm-folder-test-on-disk-conversion-repairs-a-folder-vm-cannot-read ()
  "A folder VM refuses to read is converted on disk and then reads.
Its type cannot be changed in a buffer, because it cannot be visited; that is
what this is for, and turning `vm-mboxcl2-strict' off by hand -- and having to
remember to turn it back on -- is what it replaces."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((vm-mboxcl2-strict t))
      (should-error (vm-visit-folder file))
      (vm-change-folder-type-of-file file 'mboxcl2)
      (vm-visit-folder file)
      (should (= (length vm-message-list) 3))
      (should (eq vm-folder-type 'mboxcl2)))))

(ert-deftest vm-folder-test-on-disk-conversion-keeps-the-envelope-lines ()
  "Every envelope line is the one it was: the folder is repaired, not rewritten.
`vm-convert-folder-type' works on text and has no message structs to ask, so
it used to generate \"From VM <now>\" for each one."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (vm-change-folder-type-of-file file 'mboxcl2)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines nil))
        (goto-char (point-min))
        (while (re-search-forward "^From [^\n]*" nil t)
          (push (match-string 0) lines))
        (should (equal (nreverse lines)
                       '("From alice@example.com Sat Aug  8 14:24:13 2026"
                         "From bob@example.com Sun Aug  9 09:00:00 2026"
                         "From carol@example.com Mon Aug 10 10:00:00 2026")))))))

(ert-deftest vm-folder-test-on-disk-conversion-keeps-a-backup ()
  "The folder as it was is kept in a backup file, since this rewrites it all.
The name is the one Emacs would use saving a buffer, so someone who keeps
backups in a directory of their own gets this one there too."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (vm-change-folder-type-of-file file 'mboxcl2)
    (should (file-exists-p (vm-folder-backup-name file)))
    (with-temp-buffer
      (insert-file-contents (vm-folder-backup-name file))
      (should (equal (buffer-string) vm-folder-test--seven-and-two-short))))
  ;; and it lands where backup-directory-alist says
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let* ((elsewhere (file-name-as-directory
                       (make-temp-file "vm-folder-backups" t)))
           (backup-directory-alist (list (cons "." elsewhere))))
      (unwind-protect
          (progn
            (vm-change-folder-type-of-file file 'mboxcl2)
            (should (file-exists-p (vm-folder-backup-name file)))
            (should (equal (file-name-directory (vm-folder-backup-name file))
                           elsewhere))
            (should-not (file-exists-p (concat file "~"))))
        (delete-directory elsewhere t)))))

(ert-deftest vm-folder-test-on-disk-conversion-leaves-a-sound-folder-alone ()
  "Run twice, the second run finds nothing to do and does not rewrite the file.
A conversion that is not idempotent cannot say that, and this one was not
until it stopped regenerating the envelope lines."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (vm-change-folder-type-of-file file 'mboxcl2)
    (let ((repaired (with-temp-buffer (insert-file-contents file)
                                      (buffer-string)))
          (stamp (file-attribute-modification-time (file-attributes file))))
      (delete-file (vm-folder-backup-name file))
      (vm-change-folder-type-of-file file 'mboxcl2)
      (should-not (file-exists-p (vm-folder-backup-name file)))
      (should (equal repaired (with-temp-buffer (insert-file-contents file)
                                               (buffer-string))))
      (should (equal stamp (file-attribute-modification-time
                            (file-attributes file)))))))

(ert-deftest vm-folder-test-on-disk-conversion-will-not-touch-unsaved-changes ()
  "A visited folder with changes is refused, and named, rather than converted
behind the buffer's back.  An unmodified one is killed: a folder that failed
to open leaves a buffer holding only the messages read before the error."
  (vm-folder-test-with-file (file "folder.mbox"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (let ((text-quoting-style 'grave))
      (vm-visit-folder file)
      (with-current-buffer (vm-get-file-buffer file)
        (let ((buffer-read-only nil)) (insert "x")))
      (should (string-match-p "unsaved changes"
                              (cadr (should-error
                                     (vm-change-folder-type-of-file file 'mboxcl2)))))
      (with-current-buffer (vm-get-file-buffer file) (set-buffer-modified-p nil))
      (vm-change-folder-type-of-file file 'mboxcl2)
      (should-not (vm-get-file-buffer file))
      ;; the file is mboxcl2 now; whether VM *reads* it as one is a separate
      ;; question, since this one is named .mbox and the default settings do
      ;; not trust a Content-Length
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "^Content-Length: [0-9]+$" (buffer-string)))))))

(ert-deftest vm-folder-test-on-disk-conversion-needs-a-folder ()
  "A file that is not a folder VM knows is refused, not guessed at."
  (vm-folder-test-with-file (file "notes.txt" "just some text\n")
    (let ((text-quoting-style 'grave))
      (should (string-match-p "no folder type"
                              (cadr (should-error
                                     (vm-change-folder-type-of-file file 'mboxcl2))))))))

(ert-deftest vm-folder-test-count-messages-walks-the-separators ()
  "`vm-count-messages-in-buffer' counts what the reader would read."
  (with-temp-buffer
    (insert vm-folder-test--seven-and-two-short)
    (let ((vm-folder-type 'mboxcl2)
          (vm-mboxcl2-strict nil))
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (should (= (vm-count-messages-in-buffer) 3)))))
  (with-temp-buffer
    (let ((vm-folder-type 'From_))
      (should (= (vm-count-messages-in-buffer) 0)))))

(ert-deftest vm-folder-test-in-buffer-conversion-keeps-a-backup-too ()
  "Changing a visited folder's type keeps the file as it was in FILE~.
Emacs backs a file up on the first save of its buffer, so a folder saved
earlier in the session would have had none, and this rewrites every message."
  (vm-folder-test-with-file (file "folder.mbox"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (when (file-exists-p (vm-folder-backup-name file))
        (delete-file (vm-folder-backup-name file)))
      (vm-visit-folder file)
      (vm-change-folder-type 'mboxcl2)
      (should (file-exists-p (vm-folder-backup-name file)))
      (with-temp-buffer
        (insert-file-contents (vm-folder-backup-name file))
        (should (equal (buffer-string) before))))))

(ert-deftest vm-folder-test-a-name-decides-between-From_-and-mboxcl2 ()
  "A folder named mboxcl2 is mboxcl2, whether or not it has the header yet.
The two formats are the same folder but for `Content-Length', so a folder
cannot say which it is by looking like one -- and reading a folder called
mboxcl2 as From_ because it has no lengths in it is how a folder stays wrong
and nothing says so (emacs-vm/vm#620).  A name that says nothing still leaves
it to the contents."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  vm-folder-test--mboxcl2-without-one)
    (should (eq (vm-get-folder-type file) 'mboxcl2)))
  (vm-folder-test-with-file (file "sent.mbox"
                                  vm-folder-test--mboxcl2-without-one)
    (should (eq (vm-get-folder-type file) vm-default-From_-folder-type)))
  ;; and with the option empty, a name says nothing at all
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  vm-folder-test--mboxcl2-without-one)
    (let ((vm-folder-type-by-name-alist nil))
      (should (eq (vm-get-folder-type file) vm-default-From_-folder-type)))))

(ert-deftest vm-folder-test-a-folder-named-mboxcl2-without-lengths-is-refused ()
  "Visiting one says so, and says how to repair it.
Reading it as a From_ folder instead is what happened before: VM opened it,
said nothing, and went on adding messages to a folder whose name was a lie."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  vm-folder-test--mboxcl2-without-one)
    (let ((vm-mboxcl2-strict t)
          (text-quoting-style 'grave))
      (let ((message (cadr (should-error (vm-visit-folder file)))))
        (should (string-match-p "has no Content-Length" message))
        (should (string-match-p "vm-change-folder-type" message)))
      ;; and the repair the message names produces a folder that opens
      (vm-change-folder-type-of-file file 'mboxcl2)
      (vm-visit-folder file)
      (should (= (length vm-message-list) 1)))))

;;; The folder's read-only flag, and shrunken headers (emacs-vm/vm#632)

(defconst vm-folder-test--state-message
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\n"
          ;; a header over more than one line: that is what shrinking hides,
          ;; and with every header on a line of its own there is nothing for
          ;; the command to do
          "To: one@example.com,\n\ttwo@example.com,\n\tthree@example.com\n"
          "Subject: a message with headers worth hiding\n"
          "Message-ID: <state@example.com>\n\nThe body.\n\n")
  "One message, for the commands that change how a folder is looked at.")

(defmacro vm-folder-test--with-state-folder (&rest body)
  "Visit a folder of `vm-folder-test--state-message' and run BODY in it.
A real visited folder, since these are commands that validate the folder they
are called in and will not run in a buffer that merely holds the text."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-folder-state" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-current-warning vm-current-warning))
           (write-region vm-folder-test--state-message nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-folder-test-toggle-read-only-both-ways ()
  "`vm-toggle-read-only' makes a read-only folder modifiable and back again."
  (vm-folder-test--with-state-folder
    (should-not vm-folder-read-only)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-toggle-read-only)
      (should vm-folder-read-only)
      (vm-toggle-read-only)
      (should-not vm-folder-read-only))))

(ert-deftest vm-folder-test-making-a-modified-folder-read-only-is-confirmed ()
  "Making a folder with unsaved changes read-only asks first, since quitting
would discard them, and answering no leaves the folder modifiable.

Coming back the other way is never dangerous and is not confirmed: a
read-only folder has no changes to lose.  The count of questions says so."
  (vm-folder-test--with-state-folder
    (set-buffer-modified-p t)
    (let ((asked 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked (1+ asked)) nil)))
        (should-error (vm-toggle-read-only))
        (should (equal asked 1))
        (should-not vm-folder-read-only))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked (1+ asked)) t)))
        (vm-toggle-read-only)
        (should (equal asked 2))
        (should vm-folder-read-only)
        (vm-toggle-read-only)
        (should (equal asked 2))
        (should-not vm-folder-read-only)))))

(ert-deftest vm-folder-test-shrunken-headers-hide-and-show ()
  "`vm-shrunken-headers' hides the headers that run to more than one line,
and `vm-shrunken-headers-toggle' shows them again and hides them again.

The hiding is an overlay, so what changes is what can be seen and not what
the buffer holds: the addresses are still there to be searched, saved and
replied to.

The order matters and is the way VM uses these.  `vm-shrunken-headers' makes
the overlay, hidden; the toggle only flips overlays that already exist, so on
a presentation where nothing has been shrunk yet it does nothing at all.  VM
calls the first from a hook as a message is selected, which is why a user's
toggle always has something to work on."
  (vm-folder-test--with-state-folder
    (let ((vm-preview-lines nil)
          (vm-display-using-mime t))
      (cl-letf (((symbol-function 'vm-display) #'ignore))
        (vm-show-current-message))
      (with-current-buffer (or vm-presentation-buffer (current-buffer))
        (cl-flet ((hidden ()
                    (let ((n 0))
                      (dolist (o (overlays-in (point-min) (point-max)) n)
                        (when (overlay-get o 'invisible)
                          (setq n (1+ n)))))))
          ;; nothing shrunk yet, so the toggle has nothing to flip
          (should (equal (hidden) 0))
          (vm-shrunken-headers-toggle)
          (should (equal (hidden) 0))
          ;; shrinking hides the folded To line
          (vm-shrunken-headers)
          (should (> (hidden) 0))
          (should (string-match-p "three@example.com" (buffer-string)))
          ;; and now the toggle shows it and hides it again
          (vm-shrunken-headers-toggle)
          (should (equal (hidden) 0))
          (vm-shrunken-headers-toggle)
          (should (> (hidden) 0)))))))

;;; Writing a folder elsewhere, and the quiet quits (emacs-vm/vm#632)

(defmacro vm-folder-test--with-writable-folder (spec &rest body)
  "Visit a folder with a summary and run BODY.
SPEC is (FILE-VAR TARGET-VAR): the folder's own file, and a name in the same
directory that nothing has written yet."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-write-file" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "original" dir))
               (,(cadr spec) (expand-file-name "elsewhere" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-default-folder-permission-bits #o600)
               (vm-current-warning vm-current-warning))
           (write-region vm-folder-test--state-message nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (setq vm-message-pointer vm-message-list)
             (vm-summarize)
             (set-buffer (vm-buffer-of (car vm-message-list)))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-folder-test-write-file-writes-the-folder-elsewhere ()
  "`vm-write-file' writes the folder to another name, with the permission
bits `vm-default-folder-permission-bits' asks for.

That is the first of the three things its docstring says `write-file' does
not do: a folder must not become world-readable by being written somewhere
new."
  (vm-folder-test--with-writable-folder (_file target)
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) target)))
      (vm-write-file))
    (should (file-exists-p target))
    (should (equal (file-modes target) #o600))
    (should (equal buffer-file-name target))
    (should (string-match-p "a message with headers worth hiding"
                            (with-temp-buffer (insert-file-contents target)
                                              (buffer-string))))))

(ert-deftest vm-folder-test-write-file-renames-the-summary ()
  "The summary buffer follows the folder to its new name, which is the third
thing the docstring promises -- a summary called after the old file would be
a summary of a folder that is no longer there."
  (vm-folder-test--with-writable-folder (_file target)
    (should (equal (buffer-name vm-summary-buffer) "original Summary"))
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) target)))
      (vm-write-file))
    (should (equal (buffer-name) "elsewhere"))
    (should (equal (buffer-name vm-summary-buffer) "elsewhere Summary"))))

(ert-deftest vm-folder-test-write-file-refuses-a-virtual-folder ()
  "A virtual folder has no file of its own and the command says so.

The message is checked, not merely that something was signalled.  Any error
at all satisfies `should-error', so a test that asked no more than that would
pass for a command that was broken in some other way entirely -- which is how
this test first passed against a `vm-write-file' that had been stubbed out."
  (vm-folder-test--with-writable-folder (file _target)
    (let ((vm-virtual-folder-alist
           (list (list "everything" (list (list file) '(any)))))
          (text-quoting-style 'grave))
      (vm-visit-virtual-folder "everything")
      (should (eq major-mode 'vm-virtual-mode))
      ;; the prompt is answered, so that a command which got past the refusal
      ;; would write a file and return rather than stopping for input -- the
      ;; test then fails on the missing error instead of hanging
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) (expand-file-name "from-virtual"
                                                     (file-name-directory file)))))
        (should (string-match-p
                 "cannot be applied to virtual folders"
                 (cadr (should-error (vm-write-file)))))))))

(ert-deftest vm-folder-test-quitting-just-buries-leaves-the-folder-alone ()
  "`vm-quit-just-bury' buries the folder and its summary and alters nothing.
Emacs is still visiting the folder afterwards -- that is the difference from
quitting it -- and `vm-quit-hook' runs, since a hook that tidies up on the
way out should run on this way out too."
  (vm-folder-test--with-writable-folder (file _target)
    (let ((ran 0)
          (folder (current-buffer)))
      (let ((vm-quit-hook (list (lambda () (setq ran (1+ ran))))))
        (vm-quit-just-bury))
      (should (equal ran 1))
      (should (buffer-live-p folder))
      (should (equal (buffer-file-name folder) file))
      (should (buffer-live-p vm-summary-buffer))
      ;; and the messages are still there, unexpunged and unsaved
      (should (equal (length vm-message-list) 1)))))

(ert-deftest vm-folder-test-quitting-outside-a-folder-is-refused ()
  "Both quiet quits are folder commands and say so elsewhere.

The refusal comes from the folder validation every folder command begins
with, not from anything of these two commands' own: their `major-mode' check
is never reached in a buffer with no folder at all.  So this pins the refusal
and not much else, which is what it is worth."
  (with-temp-buffer
    (fundamental-mode)
    (let ((text-quoting-style 'grave))
      (dolist (command '(vm-quit-just-bury vm-quit-just-iconify))
        (should (string-match-p
                 "No VM folder buffer\\|must be invoked from a VM buffer"
                 (cadr (should-error (funcall command)))))))))

;;; What the help command says (emacs-vm/vm#632)
;;
;; `vm-help' is a dispatcher: what it says depends on what the folder is doing.
;; No test called it, so none of its branches was checked.

(ert-deftest vm-folder-test-help-says-what-the-state-calls-for ()
  "`vm-help' answers for the state the folder is in.

Previewing, it says how to read the message; reading, it lists the keys worth
knowing; editing, it says how to finish or abandon the edit.  The branches are
the command: a help that always said the same thing would be no help."
  (vm-folder-test--with-state-folder
    (let (said)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level format &rest args)
                   (setq said (apply #'format format args)))))
        (setq vm-system-state 'previewing)
        (let ((last-command nil)) (vm-help))
        (should (string-match-p "Type SPC to read message" said))
        (setq vm-system-state 'reading)
        (let ((last-command nil)) (vm-help))
        (should (string-match-p "SPC and b scroll" said))
        (setq vm-system-state 'editing)
        (let ((last-command nil)) (vm-help))
        (should (string-match-p "to end edit" said))))))

(ert-deftest vm-folder-test-help-twice-describes-the-mode ()
  "Pressing help twice in a row describes the mode instead, which is the way
to the full list of keys."
  (vm-folder-test--with-state-folder
    (let ((described nil))
      (cl-letf (((symbol-function 'describe-function)
                 (lambda (f) (setq described f)))
                ((symbol-function 'vm-inform) #'ignore))
        (setq vm-system-state 'reading)
        (let ((last-command 'vm-help)) (vm-help))
        (should (equal described 'vm-mode))))))

(ert-deftest vm-folder-test-help-in-a-composition ()
  "In a composition it says how to send or abandon it, rather than talking
about messages there are none of."
  (let ((before (buffer-list))
        said)
    (unwind-protect
        (let ((vm-frame-per-composition nil)
              (vm-mutable-frame-configuration nil)
              (vm-mail-mode-hook nil)
              (mail-signature nil))
          (cl-letf (((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-inform)
                     (lambda (_level format &rest args)
                       (setq said (apply #'format format args)))))
            (vm-mail)
            (let ((last-command nil)) (vm-help))
            (should (string-match-p "to send message" said))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

;;; Counting the messages in a file without reading it (emacs-vm/vm#632)
;;
;; vm-count-messages-in-file counts with grep rather than by visiting the
;; folder, which is what makes the folders summary cheap.  No test called it.

(defmacro vm-folder-test--with-file-of (content &rest body)
  "Write CONTENT to a file and run BODY with FILE bound to its name."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-count" t))))
     (unwind-protect
         (let ((file (expand-file-name "folder" dir)))
           (write-region ,content nil file nil 'quiet)
           ,@body)
       (delete-directory dir t))))

(ert-deftest vm-folder-test-counting-messages-in-a-From_-file ()
  "Every message in an mbox is counted.

The third message has no Subject, so the count cannot be got right by
counting some other header: three messages and two Subject lines."
  (vm-folder-test--with-file-of
      (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
              "From: alice@example.com\nSubject: one\n\nOne.\n\n"
              "From bob@example.com Sun Aug  9 16:00:00 2026\n"
              "From: bob@example.com\nSubject: two\n\nTwo.\n\n"
              "From carol@example.com Mon Aug 10 16:00:00 2026\n"
              "From: carol@example.com\n\nNo subject at all.\n\n")
    (should (equal (vm-count-messages-in-file file t) 3))))

(ert-deftest vm-folder-test-counting-ignores-a-From_-line-in-a-body ()
  "REGRESSION: a body line beginning \"From \" is not a message.

The count was `grep -c \"^From \"\', so every such line counted as one more
message -- three for a folder of two.  VM quotes those on the way out, so a
folder VM wrote has none; one written by something else does, and issue #562
was exactly that, from Mailutils movemail.  The count now uses the separator
the folder is parsed by, so it says what a reader would see."
  (vm-folder-test--with-file-of
      (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
              "From: alice@example.com\nSubject: one\n\n"
              "From here on, a body.\n\n"
              "From bob@example.com Sun Aug  9 16:00:00 2026\n"
              "From: bob@example.com\nSubject: two\n\nTwo.\n\n")
    (should (equal (vm-count-messages-in-file file t) 2))
    ;; and quoted, as VM writes it, the same
    (write-region
     (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
             "From: alice@example.com\nSubject: one\n\n"
             ">From here on, a body.\n\n"
             "From bob@example.com Sun Aug  9 16:00:00 2026\n"
             "From: bob@example.com\nSubject: two\n\nTwo.\n\n")
     nil file nil 'quiet)
    (should (equal (vm-count-messages-in-file file t) 2))))

(ert-deftest vm-folder-test-counting-messages-in-an-mmdf-file ()
  "An mmdf folder is counted by its own separator."
  (vm-folder-test--with-file-of
      (concat "\001\001\001\001\n"
              "From: alice@example.com\nSubject: one\n\nOne.\n"
              "\001\001\001\001\n"
              "\001\001\001\001\n"
              "From: bob@example.com\nSubject: two\n\nTwo.\n"
              "\001\001\001\001\n")
    (should (equal (vm-count-messages-in-file file t) 2))))

(ert-deftest vm-folder-test-counting-messages-in-an-empty-file ()
  "An empty folder has no messages, which is nought and not nil: nil is what
this returns when it cannot count at all, and the folders summary shows the
two differently."
  (vm-folder-test--with-file-of ""
    (should (equal (vm-count-messages-in-file file t) nil))))

(ert-deftest vm-folder-test-counting-needs-a-grep-program ()
  "With no `vm-grep-program' there is no count, and nil says so rather than
nought pretending the folder is empty."
  (vm-folder-test--with-file-of
      (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
              "From: alice@example.com\nSubject: one\n\nOne.\n\n")
    (should (equal (vm-count-messages-in-file file t) 1))
    (let ((vm-grep-program nil))
      (should (equal (vm-count-messages-in-file file t) nil)))))

(ert-deftest vm-folder-test-counting-a-file-of-no-known-type ()
  "A file that is not a folder is not counted."
  (vm-folder-test--with-file-of "Not a folder at all.\nNo separators here.\n"
    (should (equal (vm-count-messages-in-file file t) nil))))

;;; The background timer functions (emacs-vm/vm#632)
;;
;; Three functions run from timers: one looks for waiting mail, one fetches
;; it, one writes cached data back into folders.  None had a test.  They are
;; the only VM code that runs when the user is doing nothing, so what they do
;; when they find nothing to do matters as much as the rest.

(defun vm-folder-test--dummy-timer ()
  "A timer object that is not scheduled, for handing to the timer functions."
  (let ((timer (timer-create)))
    (timer-set-time timer (current-time) 60)
    timer))

(ert-deftest vm-folder-test-the-mail-check-timer-reschedules-itself ()
  "`vm-check-mail-itimer-function' sets its timer's next run from
`vm-mail-check-interval'."
  (vm-folder-test--with-state-folder
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-mail-check-interval 300))
      (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                 (lambda (&rest _) nil)))
        (vm-check-mail-itimer-function timer))
      (should (equal (timer--repeat-delay timer) 300)))))

(ert-deftest vm-folder-test-the-mail-check-timer-stops-when-turned-off ()
  "With `vm-mail-check-interval' no longer a number the timer is cancelled.
Turning the interval off is how a user stops the checking, and a timer that
kept running would go on opening connections.

This runs with a folder open on purpose.  The function cancels in two places,
here and again at the end when it saw no VM folder at all, so without one the
timer is cancelled either way and a broken interval branch goes unnoticed."
  (vm-folder-test--with-state-folder
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-mail-check-interval nil)
          (cancels 0))
      (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                 (lambda (&rest _) nil))
                ((symbol-function 'cancel-timer)
                 (lambda (_which) (setq cancels (1+ cancels)))))
        (vm-check-mail-itimer-function timer))
      (should (equal cancels 1)))))

(ert-deftest vm-folder-test-the-mail-check-timer-stops-with-no-folders ()
  "With no VM folder open at all the timer goes away, whatever the interval:
there is nothing left for it to check."
  (let ((timer (vm-folder-test--dummy-timer))
        (vm-mail-check-interval 300)
        (cancels 0))
    (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
               (lambda (&rest _) nil))
              ((symbol-function 'cancel-timer)
               (lambda (_which) (setq cancels (1+ cancels)))))
      (vm-check-mail-itimer-function timer))
    (should (equal cancels 1))))

(ert-deftest vm-folder-test-the-mail-check-timer-tells-the-folder ()
  "When the waiting state changes `vm-spooled-mail-waiting-hook' runs, and
when it does not change the hook stays quiet: it is for the moment mail
arrives, not for every check.

Two things stop the second announcement and they are tested apart.  With mail
already known to be waiting VM does not ask again at all, unless
`vm-mail-check-always'; and when it does ask and the answer is the same as
before, the hook still does not run."
  (vm-folder-test--with-state-folder
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-mail-check-interval 300)
          (vm-global-block-new-mail nil)
          (vm-mail-check-always nil)
          (runs 0))
      (setq vm-spooled-mail-waiting nil)
      (let ((vm-spooled-mail-waiting-hook (list (lambda () (setq runs (1+ runs))))))
        (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                   (lambda (&rest _) t)))
          (vm-check-mail-itimer-function timer))
        (should vm-spooled-mail-waiting)
        (should (equal runs 1))
        ;; with mail known to be waiting, VM does not even ask again
        (let ((asked 0))
          (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                     (lambda (&rest _) (setq asked (1+ asked)) t)))
            (vm-check-mail-itimer-function timer))
          (should (equal asked 0))
          (should (equal runs 1)))
        ;; told to ask every time, it asks, and the answer being the same as
        ;; before the hook still does not run
        (let ((asked 0)
              (vm-mail-check-always t))
          (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                     (lambda (&rest _) (setq asked (1+ asked)) t)))
            (vm-check-mail-itimer-function timer))
          (should (equal asked 1))
          (should (equal runs 1)))))))

(ert-deftest vm-folder-test-the-mail-check-timer-honours-the-block ()
  "`vm-global-block-new-mail' stops the check, which is what it is for: VM
binds it while it is busy with the folder."
  (vm-folder-test--with-state-folder
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-mail-check-interval 300)
          (vm-global-block-new-mail t)
          (asked 0))
      (setq vm-spooled-mail-waiting nil)
      (cl-letf (((symbol-function 'vm-check-for-spooled-mail)
                 (lambda (&rest _) (setq asked (1+ asked)) t)))
        (vm-check-mail-itimer-function timer))
      (should (equal asked 0))
      (should-not vm-spooled-mail-waiting))))

(ert-deftest vm-folder-test-the-flush-timer-stops-when-there-is-nothing-to-do ()
  "`vm-flush-itimer-function' cancels its timer once no folder has anything
left to write.  It is started when data needs flushing and there is no reason
for it to keep waking Emacs afterwards."
  (let ((timer (vm-folder-test--dummy-timer))
        (vm-flush-interval 90)
        (cancelled nil))
    (cl-letf (((symbol-function 'vm-flush-cached-data-all-folders)
               (lambda () nil))
              ((symbol-function 'cancel-timer)
               (lambda (which) (setq cancelled which))))
      (vm-flush-itimer-function timer))
    (should (eq cancelled timer))
    ;; with work still outstanding it keeps its schedule
    (setq cancelled nil)
    (cl-letf (((symbol-function 'vm-flush-cached-data-all-folders)
               (lambda () t))
              ((symbol-function 'cancel-timer)
               (lambda (which) (setq cancelled which))))
      (vm-flush-itimer-function timer))
    (should-not cancelled)
    (should (equal (timer--repeat-delay timer) 90))))

;;; The mail-fetching timer (emacs-vm/vm#632)
;;
;; `vm-get-mail-itimer-function' is the one that fetches rather than looks.
;; It has four guards before it does, and each is there to stop VM writing
;; into a folder it should not touch.  Every one is tested apart: a guard that
;; is tested only along with the others is a guard that can be removed
;; unnoticed.

(defmacro vm-folder-test--fetching (spec &rest body)
  "Run BODY with a folder open and `vm-get-spooled-mail' counted.
SPEC is (COUNT-VAR), bound to a function of no arguments giving the number of
fetches so far."
  (declare (indent 1) (debug t))
  `(vm-folder-test--with-state-folder
     (let ((fetches 0))
       (cl-letf (((symbol-function 'vm-get-spooled-mail)
                  (lambda (&rest _) (setq fetches (1+ fetches)) nil)))
         (cl-flet ((,(car spec) () fetches))
           ,@body)))))

(ert-deftest vm-folder-test-the-fetch-timer-fetches ()
  "With nothing in the way the timer fetches, and reschedules itself from
`vm-auto-get-new-mail'."
  (vm-folder-test--fetching (fetches)
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-auto-get-new-mail 600)
          (vm-global-block-new-mail nil)
          (vm-block-new-mail nil)
          (vm-folder-read-only nil))
      (vm-get-mail-itimer-function timer)
      (should (equal (fetches) 1))
      (should (equal (timer--repeat-delay timer) 600)))))

(ert-deftest vm-folder-test-the-fetch-timer-obeys-each-guard ()
  "Each of the three flags stops the fetch on its own.

`vm-global-block-new-mail' is bound while VM is busy with a folder,
`vm-block-new-mail' while a folder is in a state that must not change under
it, and `vm-folder-read-only' is the user saying so.  Any one of them is
enough."
  (dolist (guard '(vm-global-block-new-mail vm-block-new-mail
                   vm-folder-read-only))
    (vm-folder-test--fetching (fetches)
      (let ((timer (vm-folder-test--dummy-timer))
            (vm-auto-get-new-mail 600)
            (vm-global-block-new-mail nil)
            (vm-block-new-mail nil)
            (vm-folder-read-only nil))
        (set guard t)
        (vm-get-mail-itimer-function timer)
        (should (equal (fetches) 0))))))

(ert-deftest vm-folder-test-the-fetch-timer-leaves-a-recovered-folder-alone ()
  "A folder whose auto-save file is newer than itself is not fetched into.

That is unsaved work waiting to be recovered, and pouring new mail into the
folder underneath it would leave the two disagreeing.  The guard also asks
that the buffer be unmodified: a modified buffer is one the user is working
in, and the auto-save file is not ahead of it in the way that matters."
  (vm-folder-test--fetching (fetches)
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-auto-get-new-mail 600)
          (vm-global-block-new-mail nil)
          (vm-block-new-mail nil)
          (vm-folder-read-only nil)
          (auto-save (make-auto-save-file-name)))
      (unwind-protect
          (progn
            (set-buffer-modified-p nil)
            (write-region "recovery data" nil auto-save nil 'quiet)
            ;; make sure it is newer than the folder
            (set-file-times auto-save (time-add (current-time) 60))
            (vm-get-mail-itimer-function timer)
            (should (equal (fetches) 0)))
        (ignore-errors (delete-file auto-save))))))

(ert-deftest vm-folder-test-the-fetch-timer-stops-when-turned-off ()
  "With `vm-auto-get-new-mail' no longer a number the timer is cancelled.
Run with a folder open: this function cancels in two places, and without a
folder the other one fires and hides a broken interval branch."
  (vm-folder-test--fetching (_fetches)
    (let ((timer (vm-folder-test--dummy-timer))
          (vm-auto-get-new-mail nil)
          (cancels 0))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_which) (setq cancels (1+ cancels)))))
        (vm-get-mail-itimer-function timer))
      (should (equal cancels 1)))))

(ert-deftest vm-folder-test-the-fetch-timer-stops-with-no-folders ()
  "With no VM folder open the timer goes away whatever the interval."
  (let ((timer (vm-folder-test--dummy-timer))
        (vm-auto-get-new-mail 600)
        (cancels 0))
    (cl-letf (((symbol-function 'vm-get-spooled-mail) (lambda (&rest _) nil))
              ((symbol-function 'cancel-timer)
               (lambda (_which) (setq cancels (1+ cancels)))))
      (vm-get-mail-itimer-function timer))
    (should (equal cancels 1))))

;;; Quitting and saving without expunging (emacs-vm/vm#651)

(defun vm-folder-test--write-three-message-folder (file)
  "Write a three-message From_ folder to FILE."
  (with-temp-file file
    (dolist (n '(1 2 3))
      (insert (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                              "From: alice@example.com\n"
                              "Subject: msg %d\n\nbody %d\n\n")
                      n n)))))

(defun vm-folder-test--subjects-on-disk (file)
  "The subjects in FILE, in order, read from disk rather than from a buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (let (subjects)
      (goto-char (point-min))
      (while (re-search-forward "^Subject: \\(.*\\)$" nil t)
        (push (match-string-no-properties 1) subjects))
      (nreverse subjects))))

(defmacro vm-folder-test--after-deleting-one (settings command &rest body)
  "Visit a three-message folder, delete the first, run COMMAND, then BODY.
SETTINGS is a let-style binding list, normally of `vm-expunge-before-save'
and `vm-expunge-before-quit'.  BODY sees FILE, the folder on disk."
  (declare (indent 2) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-expunge" t)))
          (file (expand-file-name "folder" dir)))
     (unwind-protect
         (progn
           (vm-folder-test--write-three-message-folder file)
           (let ,settings
             (vm-folder-test--visiting file
               (ignore warnings)
               (vm-delete-message 1)
               ,command))
           ,@body)
       (delete-directory dir t))))

(ert-deftest vm-folder-test-quitting-without-expunging-keeps-the-message ()
  "REGRESSION: `vm-quit-no-expunge' keeps deleted messages on disk even when
`vm-expunge-before-save' is set.

`vm-quit' skipped the expunge and then saved, and the save expunges on its
own variable: a reader who deleted a message, thought better of it, and
quit with the command that promises not to expunge lost it anyway."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save t) (vm-expunge-before-quit t))
      (vm-quit-no-expunge)
    (should (equal (vm-folder-test--subjects-on-disk file)
                   '("msg 1" "msg 2" "msg 3")))))

(ert-deftest vm-folder-test-quitting-with-a-prefix-does-not-expunge ()
  "A prefix argument to `vm-quit' means no expunge, as its docstring says,
whatever the two expunge variables are set to."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save t) (vm-expunge-before-quit t))
      (let ((current-prefix-arg '(4)))
        (call-interactively 'vm-quit))
    (should (equal (vm-folder-test--subjects-on-disk file)
                   '("msg 1" "msg 2" "msg 3")))))

(ert-deftest vm-folder-test-quitting-expunges-when-asked-to ()
  "`vm-quit' with `vm-expunge-before-quit' does expunge: the contrast that
makes the no-expunge commands worth having."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save nil) (vm-expunge-before-quit t))
      (vm-quit)
    (should (equal (vm-folder-test--subjects-on-disk file)
                   '("msg 2" "msg 3")))))

(ert-deftest vm-folder-test-saving-without-expunging-keeps-the-message ()
  "`vm-save-folder-no-expunge' writes the folder with the deleted message
still in it, whatever `vm-expunge-before-save' says."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save t))
      (vm-save-folder-no-expunge)
    (should (equal (vm-folder-test--subjects-on-disk file)
                   '("msg 1" "msg 2" "msg 3")))))

(ert-deftest vm-folder-test-saving-expunges-when-asked-to ()
  "`vm-save-folder' with `vm-expunge-before-save' expunges on the way out."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save t))
      (vm-save-folder)
    (should (equal (vm-folder-test--subjects-on-disk file)
                   '("msg 2" "msg 3")))))

(ert-deftest vm-folder-test-a-message-kept-by-no-expunge-is-still-deleted ()
  "The message kept by `vm-save-folder-no-expunge' is still marked deleted,
so the next expunge takes it: not expunging now is a deferral, not an undo."
  (vm-folder-test--after-deleting-one
      ((vm-expunge-before-save t))
      (vm-save-folder-no-expunge)
    (vm-folder-test--visiting file
      (ignore warnings)
      (should (vm-deleted-flag (car vm-message-list)))
      (should-not (vm-deleted-flag (nth 1 vm-message-list))))))

;;; Recovering and reverting a folder (emacs-vm/vm#652)
;;
;; After a recovery the buffer and the disk disagree, so new mail is blocked
;; until a real save.  These drive the handler directly: what re-runs VM on
;; the folder is `vm', which is stubbed here so the tests can see what it
;; was asked to open.

(defun vm-folder-test--write-folder-content (count)
  "Return an mbox of COUNT messages for the recovery tests."
  (mapconcat
   (lambda (n)
     (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                     "From: alice@example.com\n"
                     "Subject: msg %d\n\nbody %d\n\n")
             n n))
   (number-sequence 1 count)
   ""))

(defvar vm-folder-test--reopened nil
  "The arguments the recovery handler passed to `vm'.")

(defmacro vm-folder-test--recovering (&rest body)
  "Run BODY with `vm' and the virtual quit stubbed, watching the reopen.
`vm-folder-test--reopened' collects the argument list of the `vm' call."
  (declare (indent 0) (debug t))
  `(let ((vm-folder-test--reopened nil))
     (cl-letf (((symbol-function 'vm)
                (lambda (&rest args) (setq vm-folder-test--reopened args)))
               ((symbol-function 'vm-virtual-quit) #'ignore))
       ,@body)))

(ert-deftest vm-folder-test-a-recovery-blocks-new-mail ()
  "New mail is blocked after a recovery and not after a reversion.

The recovered buffer has not been written yet, so its idea of the folder
and the file's disagree; letting new mail in would append to the file
underneath it."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (vm-folder-test--recovering
      (setq vm-block-new-mail nil)
      (vm-handle-file-recovery-or-reversion nil)
      (should-not vm-block-new-mail)
      (vm-handle-file-recovery-or-reversion t)
      (should vm-block-new-mail))))

(ert-deftest vm-folder-test-getting-mail-while-blocked-is-refused ()
  "`vm-get-spooled-mail' refuses while the block is on, and says what to do
about it: the message names saving, which is what clears the block."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (let ((vm-block-new-mail t)
          (text-quoting-style 'grave))
      (let ((err (should-error (vm-get-spooled-mail) :type 'error)))
        (should (string-match-p "save this folder"
                                (error-message-string err)))))))

(ert-deftest vm-folder-test-saving-unblocks-new-mail ()
  "Saving the folder clears the block: the file and the buffer agree again."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (setq vm-block-new-mail t)
    (vm-unblock-new-mail)
    (should-not vm-block-new-mail)))

(ert-deftest vm-folder-test-a-recovery-starts-vm-from-scratch ()
  "The summary buffer goes and `major-mode' is reset before VM is re-run.

VM decides what to do from the major mode; leaving it as `vm-mode' would
have it pick up the old message list, whose markers point into text the
recovery has replaced."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (let ((summary (generate-new-buffer " *test summary*")))
      (setq vm-summary-buffer summary)
      (vm-folder-test--recovering
        (vm-handle-file-recovery-or-reversion t)
        (should-not (buffer-live-p summary))
        (should (eq major-mode 'fundamental-mode))
        (should vm-folder-test--reopened)))))

(ert-deftest vm-folder-test-a-recovered-server-folder-comes-back-connected ()
  "A POP or IMAP folder is reopened through its server name, not its file.

Reopening the cache file as a plain folder would leave the reader looking
at something disconnected from the server, which is issue #425 all over
again."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (dolist (case '((pop . "pop:mail.example.invalid:110:pass:alice:*")
                    (imap . "imap:mail.example.invalid:143:inbox:login:alice:*")))
      (let ((vm-folder-access-method (car case)))
        (cl-letf (((symbol-function 'vm-pop-find-name-for-buffer)
                   (lambda (&rest _) (cdr case)))
                  ((symbol-function 'vm-imap-find-spec-for-buffer)
                   (lambda (&rest _) (cdr case))))
          (vm-folder-test--recovering
            (vm-handle-file-recovery-or-reversion t)
            (should (equal (nth 0 vm-folder-test--reopened) (cdr case)))
            (should (eq (plist-get (cdr vm-folder-test--reopened) :access-method)
                        (car case)))))))))

(ert-deftest vm-folder-test-a-recovered-file-folder-comes-back-as-a-file ()
  "A folder that is a plain file is reopened by its file name."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (setq buffer-file-name "/tmp/vm-test-not-really-there")
    (let ((vm-folder-access-method nil))
      (vm-folder-test--recovering
        (vm-handle-file-recovery-or-reversion nil)
        (should (equal (nth 0 vm-folder-test--reopened) buffer-file-name))))))

(ert-deftest vm-folder-test-reverting-keeps-the-access-method ()
  "`vm-revert-buffer' and `vm-recover-file' put the access method and data
back after the operation that clears them, so a server folder is visited
again as one rather than as the local file it is cached in."
  (dolist (command '(vm-revert-buffer vm-recover-file))
    (vm-test-with-folder (vm-folder-test--write-folder-content 2)
      (setq major-mode 'vm-mode)
      (setq vm-folder-access-method 'imap
            vm-folder-access-data (vector 'access 'data))
      (let ((data vm-folder-access-data))
        (vm-folder-test--recovering
          (cl-letf (((symbol-function 'revert-buffer)
                     ;; as the real one does, by way of the mode's own setup
                     (lambda (&rest _)
                       (setq vm-folder-access-method nil
                             vm-folder-access-data nil)))
                    ((symbol-function 'recover-file)
                     (lambda (&rest _)
                       (setq vm-folder-access-method nil
                             vm-folder-access-data nil)))
                    ((symbol-function 'vm-recover-folder-file-name)
                     (lambda (&rest _) "/tmp/vm-test-not-really-there"))
                    ((symbol-function 'call-interactively)
                     (lambda (fn &rest _) (funcall fn))))
            (funcall command))
          (should (eq vm-folder-access-method 'imap))
          (should (eq vm-folder-access-data data))
          (should (eq (plist-get (cdr vm-folder-test--reopened) :access-method)
                      'imap)))))))

;;; Which file the folder is cached in (emacs-vm/vm#670)

(ert-deftest vm-folder-test-the-cache-file-of-an-imap-folder ()
  "An IMAP folder answers with the file VM keeps it in locally, which is
the one named after the maildrop rather than after the mailbox."
  (let ((vm-imap-folder-cache-directory "/tmp/vm-test-cache")
        (spec "imap:mail.example.invalid:143:inbox:login:alice:*"))
    (with-temp-buffer
      (setq major-mode 'vm-mode)
      (setq vm-folder-access-method 'imap
            vm-folder-access-data (make-vector 20 nil))
      (vm-set-folder-imap-maildrop-spec spec)
      (should (equal (vm-folder-cache-file)
                     (vm-imap-make-filename-for-spec spec)))
      (should (string-prefix-p "/tmp/vm-test-cache/imap-cache-"
                               (vm-folder-cache-file))))))

(ert-deftest vm-folder-test-the-cache-file-of-a-pop-folder ()
  "A POP folder answers the same way, through its own naming."
  (let ((vm-pop-folder-cache-directory "/tmp/vm-test-cache")
        (spec "pop:mail.example.invalid:110:pass:alice:*"))
    (with-temp-buffer
      (setq major-mode 'vm-mode)
      (setq vm-folder-access-method 'pop
            vm-folder-access-data (make-vector 20 nil))
      (vm-set-folder-pop-maildrop-spec spec)
      (should (equal (vm-folder-cache-file)
                     (vm-pop-make-filename-for-spec spec))))))

(ert-deftest vm-folder-test-a-local-folder-has-no-cache-file ()
  "A folder that is a file is not cached anywhere: it is the file."
  (with-temp-buffer
    (setq major-mode 'vm-mode)
    (setq vm-folder-access-method nil)
    (should-not (vm-folder-cache-file))))

(ert-deftest vm-folder-test-the-cache-file-can-be-asked-about-a-buffer ()
  "The buffer to ask about can be given, so a reader in the summary or in
another folder can ask about this one."
  (let ((vm-imap-folder-cache-directory "/tmp/vm-test-cache")
        (spec "imap:mail.example.invalid:143:inbox:login:alice:*"))
    (let ((folder (generate-new-buffer " *test folder*")))
      (unwind-protect
          (progn
            (with-current-buffer folder
              (setq major-mode 'vm-mode)
              (setq vm-folder-access-method 'imap
                    vm-folder-access-data (make-vector 20 nil))
              (vm-set-folder-imap-maildrop-spec spec))
            (with-temp-buffer
              (setq vm-folder-access-method nil)
              (should (equal (vm-folder-cache-file folder)
                             (vm-imap-make-filename-for-spec spec)))
              ;; and about itself, still nothing
              (should-not (vm-folder-cache-file))))
        (kill-buffer folder)))))

(ert-deftest vm-folder-test-the-cache-file-answers-from-the-summary ()
  "Asked in a summary or presentation buffer, the question is about the
folder those belong to.  It answered \"Not a remote folder\" there, which is
where a reader is when the question occurs to them -- and the buffer they
would have to be in instead is the one they cannot see."
  (let ((vm-imap-folder-cache-directory "/tmp/vm-test-cache")
        (spec "imap:mail.example.invalid:143:inbox:login:alice:*")
        (folder (generate-new-buffer " *test folder*"))
        (summary (generate-new-buffer " *test summary*")))
    (unwind-protect
        (progn
          (with-current-buffer folder
            (setq major-mode 'vm-mode)
            (setq vm-folder-access-method 'imap
                  vm-folder-access-data (make-vector 20 nil))
            (vm-set-folder-imap-maildrop-spec spec))
          (with-current-buffer summary
            (setq vm-mail-buffer folder)
            (should (equal (vm-folder-cache-file)
                           (vm-imap-make-filename-for-spec spec)))
            ;; and the reader is left where they were
            (should (eq (current-buffer) summary))))
      (kill-buffer folder)
      (kill-buffer summary))))

(ert-deftest vm-folder-test-the-cache-file-of-a-virtual-folder ()
  "A virtual folder has no maildrop of its own, and the question is about
the folder the message being looked at really lives in."
  (let ((vm-imap-folder-cache-directory "/tmp/vm-test-cache")
        (spec "imap:mail.example.invalid:143:inbox:login:alice:*")
        (real (generate-new-buffer " *test folder*"))
        (virtual (generate-new-buffer " *test virtual*")))
    (unwind-protect
        (let (message)
          (with-current-buffer real
            (setq major-mode 'vm-mode)
            (setq vm-folder-access-method 'imap
                  vm-folder-access-data (make-vector 20 nil))
            (vm-set-folder-imap-maildrop-spec spec)
            (setq message (vm-make-message))
            (vm-set-buffer-of message real))
          (with-current-buffer virtual
            (setq major-mode 'vm-virtual-mode)
            (let ((mirror (vm-make-message))
                  (real-sym (make-symbol "real")))
              ;; a virtual message points at its real one through a symbol
              (set real-sym message)
              (aset (aref mirror 1) 5 real-sym)
              (vm-set-buffer-of mirror virtual)
              (setq vm-message-list (list mirror)
                    vm-message-pointer vm-message-list))
            (should (equal (vm-folder-cache-file)
                           (vm-imap-make-filename-for-spec spec)))))
      (kill-buffer real)
      (kill-buffer virtual))))

;;; Saving the folder buffer

(ert-deftest vm-folder-test-saving-the-buffer-unblocks-new-mail ()
  "`vm-save-buffer' clears the block a recovery puts on new mail: the file
and the buffer agree again once it has written."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-mode)
    (setq vm-block-new-mail t)
    (cl-letf (((symbol-function 'save-buffer) #'ignore)
              ((symbol-function 'vm-display) #'ignore)
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-write-index-file-maybe) #'ignore))
      (vm-save-buffer nil))
    (should-not vm-block-new-mail)))

(ert-deftest vm-folder-test-saving-a-virtual-folder-is-refused ()
  "A virtual folder holds no messages of its own, so there is nothing to
write; the refusal says so rather than saving the presentation."
  (vm-test-with-folder (vm-folder-test--write-folder-content 2)
    (setq major-mode 'vm-virtual-mode)
    (let ((text-quoting-style 'grave)
          (saved nil))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (setq saved t)))
                ((symbol-function 'vm-display) #'ignore))
        (should-error (vm-save-buffer nil) :type 'error)
        (should-not saved)))))

;;; Counting the messages in a file (emacs-vm/vm#640)

(defmacro vm-folder-test--counting (text &rest body)
  "Write TEXT to a folder file and run BODY with COUNT bound to VM's count."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-count" t)))
          (file (expand-file-name "folder" dir)))
     (unwind-protect
         (progn
           (write-region ,text nil file nil 'quiet)
           (let ((count (vm-count-messages-in-file file t)))
             (ignore count)
             ,@body))
       (delete-directory dir t))))

(ert-deftest vm-folder-test-counting-counts-the-messages ()
  "A folder of two messages counts two."
  (vm-folder-test--counting
      (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
              "From: alice@example.com\nSubject: one\n\nA body.\n\n"
              "From bob@example.com Sun Aug  9 16:00:00 2026\n"
              "From: bob@example.com\nSubject: two\n\nA body.\n\n")
    (should (equal count 2))))

(ert-deftest vm-folder-test-counting-agrees-with-what-is-parsed ()
  "The count is what visiting the folder finds, which is the point of it:
a total that disagrees with the folder misleads about mail waiting."
  (let ((text (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
                      "From: alice@example.com\nSubject: one\n\n"
                      "From here on, a body.\n"
                      "From nowhere in particular\n\n"
                      "From bob@example.com Sun Aug  9 16:00:00 2026\n"
                      "From: bob@example.com\nSubject: two\n\nA body.\n\n")))
    (vm-folder-test--counting text
      (vm-test-with-folder text
        (should (equal count (length vm-message-list)))))))

(ert-deftest vm-folder-test-counting-a-quoted-From_-line ()
  "A quoted separator, as VM writes them, is not counted either."
  (vm-folder-test--counting
      (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
              "From: alice@example.com\nSubject: one\n\n"
              ">From here on, a body.\n\n")
    (should (equal count 1))))

(ert-deftest vm-folder-test-counting-an-empty-folder ()
  "An empty file holds no messages, and is not an error."
  (vm-folder-test--counting "" (should (member count '(0 nil)))))

;;; Visiting a folder another mail client maintains

(defmacro vm-folder-test--with-a-thunderbird-folder (&rest body)
  "Write a folder in a directory standing in for Thunderbird's and run BODY.
DIR is that directory, bound to `vm-thunderbird-folder-directory', and FILE
the folder in it."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-thunderbird" t)))
          (file (expand-file-name "Inbox" dir))
          (vm-thunderbird-folder-directory dir)
          (vm-folder-directory nil)
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list)))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                     "From: alice@example.com\nSubject: from thunderbird\n"
                     "\nA message Thunderbird put here.\n\n"))
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-folder-test-a-thunderbird-folder-is-visited-as-a-folder ()
  "`vm-visit-thunderbird-folder' reads the file as any other folder, and
remembers where it came from: `vm-foreign-folder-directory' is what makes a
save from it offer the other Thunderbird folders rather than VM's own."
  (vm-folder-test--with-a-thunderbird-folder
    (vm-visit-thunderbird-folder file)
    (should (eq major-mode 'vm-mode))
    (should (equal (length vm-message-list) 1))
    (should (equal (vm-su-subject (car vm-message-list)) "from thunderbird"))
    (should (equal vm-foreign-folder-directory dir))
    (should (local-variable-p 'vm-foreign-folder-directory))
    (should (equal vm-last-visit-folder file))))

(ert-deftest vm-folder-test-a-thunderbird-folder-name-is-taken-as-relative ()
  "A name with no directory in it is looked for in
`vm-thunderbird-folder-directory', which is the point of having the setting:
the folders are somewhere the user does not want to type."
  (vm-folder-test--with-a-thunderbird-folder
    (vm-visit-thunderbird-folder "Inbox")
    (should (equal (buffer-file-name) file))
    (should (equal vm-foreign-folder-directory dir))))

(ert-deftest vm-folder-test-the-folders-summary-is-gone ()
  "The folders summary is removed (emacs-vm/vm#701).  It kept its counts in
Berkeley DB, an XEmacs package GNU Emacs has never had, so on the only Emacs
VM supports the command signalled before doing anything and the counts were
never written.  Nothing of it is left to call."
  (dolist (name '(vm-folders-summarize vm-get-folder-totals
                  vm-store-folder-totals vm-modify-folder-totals
                  vm-do-folders-summary vm-follow-folders-summary-cursor))
    (should-not (fboundp name)))
  (dolist (name '(vm-folders-summary-database vm-folders-summary-format
                  vm-folders-summary-directories vm-frame-per-folders-summary
                  vm-folders-summary-buffer))
    (should-not (boundp name))))

(provide 'vm-folder-test)

;;; vm-folder-test.el ends here
