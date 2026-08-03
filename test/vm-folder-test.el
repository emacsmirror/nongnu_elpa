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

(provide 'vm-folder-test)

;;; vm-folder-test.el ends here
