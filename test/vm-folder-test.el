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

(provide 'vm-folder-test)

;;; vm-folder-test.el ends here
