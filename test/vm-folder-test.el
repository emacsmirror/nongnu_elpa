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
  "Finding an MMDF trailing separator leaves point on it.
Where the message is over is what the function is for; it used to be checked
by its return value, which meant nothing here.  `vm-build-message-list' then
read that value as saying point was on the *next* message\\='s leading
separator, and no mmdf folder could be read at all (emacs-vm/vm#786).  Now
every arm but the From_ header-block one answers nil, so the position is the
only thing left to assert, and the only thing that was ever true."
  (with-temp-buffer
    (insert "Body text\n")
    (insert "\001\001\001\001\n")
    (let ((vm-folder-type 'mmdf))
      (goto-char (point-min))
      (should-not (vm-find-trailing-message-separator))
      (should (looking-at "\001\001\001\001")))))

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
the folder\\='s name gets a new inode and the other name keeps the old mail.
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
this structure, and splitting there would invent a message."
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
`vm-find-trailing-message-separator' takes a different branch for it, and the
header-block search is not on that path: its message boundaries come from the
byte count, which is the whole point of the format.  Built by hand rather than
with `vm-test-with-folder', which resets `vm-trust-content-length'
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
  "`vm-folder-type-for-name' answers from `vm-folder-type-by-extension-alist'."
  (should (eq (vm-folder-type-for-name "/mail/2026-08.out.mboxcl2") 'mboxcl2))
  ;; .mbox is what the rest of the world means by an mbox file, which is From_
  (should (eq (vm-folder-type-for-name "/mail/2026-08.out.mbox") 'From_))
  (should-not (vm-folder-type-for-name "/mail/INBOX"))
  ;; `file-name-extension' looks past a backup suffix, so the backup of an
  ;; mboxcl2 folder is one too -- which it is, and which the old pattern,
  ;; anchored at the end of the name, said it was not
  (should (eq (vm-folder-type-for-name "/mail/sent.mboxcl2~") 'mboxcl2))
  ;; a compressed folder is named for the compression, and VM is not asked
  (should-not (vm-folder-type-for-name "/mail/sent.mboxcl2.gz"))
  (should-not (vm-folder-type-for-name nil))
  ;; and nothing is claimed when the option is empty
  (let ((vm-folder-type-by-extension-alist nil))
    (should-not (vm-folder-type-for-name "/mail/sent.mboxcl2")))
  ;; the first match wins, and any type may be named
  (let ((vm-folder-type-by-extension-alist '(("babyl" . babyl)
                                             ("b" . mmdf))))
    (should (eq (vm-folder-type-for-name "/mail/old.babyl") 'babyl))
    (should (eq (vm-folder-type-for-name "/mail/old.b") 'mmdf))))

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
      (let ((vm-folder-type-by-extension-alist nil))
        (should (eq (vm-get-folder-type with-length)
                    vm-default-From_-folder-type))))))

(ert-deftest vm-folder-test-the-default-type-does-not-decide-an-existing-folder ()
  "`vm-default-folder-type' decides what VM creates and nothing else.
Issue #767.  It briefly decided an existing folder whose name said nothing,
which meant a reader who set mboxcl2 had every From_ folder refused for want
of a `Content-Length'.  An existing folder is read as what it is."
  (vm-folder-test-with-directory dir
    (let ((silent (expand-file-name "INBOX" dir))
          (message (concat "From VM Mon Aug 10 00:00:00 2026\n"
                           "To: someone@example.com\n\nbody\n")))
      (write-region message nil silent nil 'quiet)
      (dolist (default '(From_ mboxcl2 From_-with-Content-Length))
        (let ((vm-default-folder-type default)
              (vm-trust-content-length nil))
          (should (eq (vm-get-folder-type silent)
                      vm-default-From_-folder-type)))))))

(ert-deftest vm-folder-test-a-new-folder-is-named-for-the-default-type ()
  "REGRESSION: mboxcl2 as the default names the folder VM creates.
Issue #767.  A folder\\='s type is read back from its name, so creating mboxcl2
under a name that says nothing leaves a folder read as From_ next time and
split wherever a body line begins \"From \", which is what
`vm-error-if-name-contradicts-type' refuses to do on a conversion."
  (vm-folder-test-with-directory dir
    (let ((new (expand-file-name "2026-08.out" dir))
          (named (expand-file-name "sent.mboxcl2" dir))
          (as-mbox (expand-file-name "legacy.mbox" dir)))
      (let ((vm-default-folder-type 'mboxcl2))
        (should (equal (vm-new-folder-file-name new)
                       (concat new ".mboxcl2")))
        ;; a name that already says a type is left alone, either way
        (should (equal (vm-new-folder-file-name named) named))
        (should (equal (vm-new-folder-file-name as-mbox) as-mbox))
        ;; and so is a file that exists, which has a type of its own
        (write-region "From VM Mon Aug 10 00:00:00 2026\n\nbody\n"
                      nil new nil 'quiet)
        (should (equal (vm-new-folder-file-name new) new))
        (delete-file new))
      ;; the shipped default names nothing: From_ is what a silent name means
      (let ((vm-default-folder-type 'From_))
        (should (equal (vm-new-folder-file-name new) new)))
      ;; nor do the types that are recognised from their contents
      (dolist (default '(babyl mmdf))
        (let ((vm-default-folder-type default))
          (should (equal (vm-new-folder-file-name new) new))))
      ;; the extension comes from the option that reads it back
      (let ((vm-default-folder-type 'mboxcl2)
            (vm-folder-type-by-extension-alist '(("cl2" . mboxcl2))))
        (should (equal (vm-new-folder-file-name new) (concat new ".cl2")))))))

(ert-deftest vm-folder-test-an-fcc-creates-the-folder-with-the-type-in-its-name ()
  "An `FCC:' to a folder that does not exist creates NAME.mboxcl2 under that
default, and the copy is readable as mboxcl2 afterwards."
  (vm-folder-test-with-directory dir
    (let ((asked (expand-file-name "2026-08.out" dir))
          (vm-default-folder-type 'mboxcl2))
      (with-temp-buffer
        (insert "From: me@example.com\nTo: you@example.com\nSubject: s\n\nbody\n")
        (vm-fcc-write asked))
      (should-not (file-exists-p asked))
      (should (file-exists-p (concat asked ".mboxcl2")))
      (should (eq (vm-get-folder-type (concat asked ".mboxcl2")) 'mboxcl2))
      ;; and the length is there, which is what makes it readable
      (with-temp-buffer
        (insert-file-contents (concat asked ".mboxcl2"))
        (should (string-match-p "^Content-Length:" (buffer-string)))))))

(ert-deftest vm-folder-test-an-unnamed-cache-is-read-as-from_ ()
  "A cache with no type in its name is From_, whatever its lengths look like.
Issue #767.  A cache was written in `vm-default-folder-type', which was
mboxcl2 on Solaris, AIX and System V until 2026, so such a cache can be
mboxcl2, and VM cannot tell it from a From_ cache that collected a few
lengths.  Believing a length that is wrong puts a boundary inside a body;
ignoring one that is right costs a spurious message the reader can see."
  (vm-folder-test-with-directory dir
    (let* ((body "A short body.\n")
           (length (number-to-string (length body)))
           (counted (concat "From VM Thu May  7 06:22:17 2026\n"
                            "From: a@example.com\nSubject: one\n"
                            "Content-Length: " length "\n\n" body))
           (cache (expand-file-name "imap-cache-0123456789abcdef" dir))
           (named (expand-file-name "imap-cache-0123456789abcdef.mboxcl2" dir))
           (vm-trust-content-length t))
      ;; every message counted, which is what an mboxcl2 cache looks like
      (write-region (concat counted counted counted) nil cache nil 'quiet)
      (let ((vm-current-warning nil))
        (should (eq (vm-get-folder-type cache) vm-default-From_-folder-type)))
      ;; and the name is how to say otherwise
      (write-region (concat counted counted counted) nil named nil 'quiet)
      (should (eq (vm-get-folder-type named) 'mboxcl2)))))

(defconst vm-folder-test--uncounted-cache
  (concat "From VM Thu May  7 06:22:17 2026\n"
          "From: a@example.com\nSubject: one\n\nBody one.\n\n"
          "From VM Thu May  7 06:22:18 2026\n"
          "From: b@example.com\nSubject: two\n\nBody two.\n\n")
  "Two messages with no Content-Length: a cache as an older VM wrote one.")

(defmacro vm-folder-test--with-cache-directory (var &rest body)
  "Run BODY with VAR bound to a directory that is the only place caches live."
  (declare (indent 1) (debug t))
  `(vm-folder-test-with-directory ,var
     (let ((vm-imap-folder-cache-directory ,var)
           (vm-pop-folder-cache-directory nil)
           (vm-folder-directory nil)
           (process-environment (cons (concat "HOME=" ,var)
                                      process-environment)))
       ,@body)))

(defun vm-folder-test--cache (dir name &optional text)
  "Write TEXT as cache NAME in DIR, and answer the file."
  (let ((file (expand-file-name name dir)))
    (write-region (or text vm-folder-test--uncounted-cache) nil file nil 'quiet)
    file))

(ert-deftest vm-folder-test-the-older-caches-are-the-ones-without-the-suffix ()
  "`vm-cache-folders-in-the-older-format' finds a cache whose name says no type.
Not one that names its type, and nothing that is not a cache: the name is all
there is to go on, since the maildrop a cache belongs to is deliberately not
recorded in it."
  (vm-folder-test--with-cache-directory dir
    (let ((old (vm-folder-test--cache dir "imap-cache-0123456789abcdef"))
          (old-pop (vm-folder-test--cache dir "pop-cache-fedcba9876543210")))
      (vm-folder-test--cache dir "imap-cache-abcdef0123456789.mboxcl2")
      (vm-folder-test--cache dir "INBOX")
      (vm-folder-test--cache dir "imap-cache-nothexadecimal")
      ;; truenames, since that is how one directory reached two ways is
      ;; recognised as one (#771)
      (should (equal (car (vm-cache-folders-in-the-older-format))
                     (sort (mapcar #'file-truename (list old old-pop))
                           #'string-lessp)))
      ;; and nothing was unreadable, so there is no fault to report
      (should-not (cdr (vm-cache-folders-in-the-older-format))))))

(ert-deftest vm-folder-test-converting-the-caches-names-and-counts-them ()
  "REGRESSION: `vm-convert-caches-to-mboxcl2' converts each older cache.
Issue #768.  Every cache VM creates is mboxcl2 and named for it; one from
before that was read as From_, so a message whose body holds a line beginning
\"From \" could still split it in two."
  (vm-folder-test--with-cache-directory dir
    (let ((old (vm-folder-test--cache dir "imap-cache-0123456789abcdef"))
          (said nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said))))
        (vm-convert-caches-to-mboxcl2))
      (should-not (file-exists-p old))
      (should (file-exists-p (concat old ".mboxcl2")))
      ;; the original bytes are kept in a backup, which is the safety net
      (should (file-exists-p (vm-folder-backup-name old)))
      (should (eq (vm-get-folder-type (concat old ".mboxcl2")) 'mboxcl2))
      ;; both messages are there, and each carries its length now
      (with-temp-buffer
        (insert-file-contents (concat old ".mboxcl2"))
        (should (equal 2 (how-many "^Content-Length:" (point-min) (point-max)))))
      (should (cl-find-if (lambda (s) (string-match-p "1 cache of 1 converted" s))
                          said)))))

(ert-deftest vm-folder-test-a-visited-cache-is-refused-not-killed ()
  "REGRESSION: converting a cache being visited refuses and keeps the buffer.
Issue #770.  `vm-change-folder-type-of-file' guarded only on
`buffer-modified-p', so a healthy folder buffer was killed without asking and
its summary and presentation were left pointing at a dead buffer, where every
command answers \"Folder buffer has been killed\"."
  (vm-folder-test--with-cache-directory dir
    (let ((old (vm-folder-test--cache dir "imap-cache-0123456789abcdef")))
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (vm-visit-folder old))
      (let ((folder (vm-get-file-buffer old))
            (faults nil))
        (should (buffer-live-p folder))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'vm-inform) #'ignore)
                  ((symbol-function 'vm-report-cache-conversion)
                   (lambda (_found _converted fs) (setq faults fs))))
          (vm-convert-caches-to-mboxcl2))
        ;; the folder is still there, and so is the file
        (should (buffer-live-p folder))
        (should (file-exists-p old))
        (should-not (file-exists-p (concat old ".mboxcl2")))
        ;; and the reader is told which one, and what to do
        (should (equal (length faults) 1))
        (should (string-match-p "being visited" (cdr (car faults))))
        (should (string-match-p "vm-quit" (cdr (car faults))))))))

(ert-deftest vm-folder-test-a-failed-visit-takes-its-attendants-with-it ()
  "REGRESSION: the buffer a failed visit left is killed with its attendants.
Issue #770.  That buffer is still the one case the on-disk conversion may kill
-- it holds part of a folder and no `vm-message-pointer' -- but killing it
alone orphaned the summary and presentation."
  (vm-folder-test--with-cache-directory dir
    (let ((old (vm-folder-test--cache dir "imap-cache-0123456789abcdef")))
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (vm-visit-folder old))
      (let* ((folder (vm-get-file-buffer old))
             (summary (with-current-buffer folder vm-summary-buffer))
             (presentation (with-current-buffer folder
                             vm-presentation-buffer-handle)))
        (should (buffer-live-p summary))
        ;; make it look like a visit that failed partway: messages, no pointer
        (with-current-buffer folder (setq vm-message-pointer nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'vm-inform) #'ignore))
          (vm-convert-caches-to-mboxcl2))
        (should-not (buffer-live-p folder))
        (should-not (buffer-live-p summary))
        (should-not (and presentation (buffer-live-p presentation)))
        (should (file-exists-p (concat old ".mboxcl2")))))))

(ert-deftest vm-folder-test-the-faults-are-listed-in-a-buffer ()
  "REGRESSION: a cache that could not be converted is named in a buffer.
Issue #770.  One `vm-warn' per fault replaced each with the next, so with a
dozen caches only the last was readable, while the docstring said every fault
was named in the report."
  (vm-folder-test--with-cache-directory dir
    (let ((good (vm-folder-test--cache dir "imap-cache-0123456789abcdef"))
          (bad (vm-folder-test--cache dir "pop-cache-fedcba9876543210"
                                      "this is not a folder at all\n"))
          (report nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'vm-inform) #'ignore))
        (vm-convert-caches-to-mboxcl2))
      (should (file-exists-p (concat good ".mboxcl2")))
      (when (get-buffer "*VM cache conversion*")
        (with-current-buffer "*VM cache conversion*"
          (setq report (buffer-string)))
        (kill-buffer "*VM cache conversion*"))
      (should report)
      (should (string-match-p "1 cache of 2 converted, 1 could not be" report))
      (should (string-match-p "pop-cache-fedcba9876543210" report))
      ;; and the one that worked is not listed as a fault
      (should-not (string-match-p "imap-cache-0123456789abcdef" report)))))

(ert-deftest vm-folder-test-a-clean-conversion-opens-no-buffer ()
  "Nothing went wrong, so there is nothing to read: the tally is one line.
A buffer for a run with no faults would be the noise that stops the report
being worth opening when there are some."
  (vm-folder-test--with-cache-directory dir
    (vm-folder-test--cache dir "imap-cache-0123456789abcdef")
    (let ((said nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said))))
        (vm-convert-caches-to-mboxcl2))
      (should-not (get-buffer "*VM cache conversion*"))
      (should (cl-find-if (lambda (s) (string-match-p "1 cache of 1 converted" s))
                          said)))))

(ert-deftest vm-folder-test-one-cache-reached-two-ways-is-one-cache ()
  "REGRESSION: a directory configured twice under two names yields one cache.
Issue #771.  `expand-file-name' does not resolve symbolic links, so a
directory reached two ways contributed its caches twice: the first conversion
renamed the file and the second was reported as a failure that had not
happened.  /tmp is a symbolic link on macOS and a home directory is one on many
managed systems, so this is ordinary rather than exotic."
  (vm-folder-test-with-directory root
    (let ((real (file-name-as-directory (expand-file-name "real" root)))
          (link (expand-file-name "link" root)))
      (make-directory real)
      (make-symbolic-link "real" link)
      (write-region vm-folder-test--uncounted-cache nil
                    (expand-file-name "imap-cache-0123456789abcdef" real)
                    nil 'quiet)
      (let ((vm-imap-folder-cache-directory real)
            (vm-pop-folder-cache-directory link)
            (vm-folder-directory nil)
            (process-environment (cons (concat "HOME=" root) process-environment)))
        (should (equal (length (car (vm-cache-folders-in-the-older-format))) 1))
        (let ((faults 'unset) (found nil) (converted nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'vm-inform) #'ignore)
                    ((symbol-function 'vm-report-cache-conversion)
                     (lambda (f c fs) (setq found f converted c faults fs))))
            (vm-convert-caches-to-mboxcl2))
          (should (equal found 1))
          (should (equal converted 1))
          (should-not faults))))))

(ert-deftest vm-folder-test-a-directory-that-cannot-be-read-is-reported ()
  "REGRESSION: one unreadable directory does not stop the others being searched.
Issue #771.  `directory-files' raised out of the whole command before anything
was converted, which is the half-done job with no account of it that the
conversion collects its faults to avoid."
  (vm-folder-test-with-directory root
    (let ((open (file-name-as-directory (expand-file-name "open" root)))
          (shut (file-name-as-directory (expand-file-name "shut" root))))
      (make-directory open)
      (make-directory shut)
      (write-region vm-folder-test--uncounted-cache nil
                    (expand-file-name "imap-cache-0123456789abcdef" open)
                    nil 'quiet)
      (set-file-modes shut #o000)
      (unwind-protect
          (let ((vm-imap-folder-cache-directory open)
                (vm-pop-folder-cache-directory shut)
                (vm-folder-directory nil)
                (process-environment (cons (concat "HOME=" open) process-environment))
                (report nil))
            ;; the search says what it could not read rather than signalling
            (should (equal (length (cdr (vm-cache-folders-in-the-older-format))) 1))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'vm-inform) #'ignore))
              (vm-convert-caches-to-mboxcl2))
            ;; the readable directory's cache was converted anyway
            (should (file-exists-p
                     (expand-file-name "imap-cache-0123456789abcdef.mboxcl2" open)))
            ;; and the one that could not be read is named
            (when (get-buffer "*VM cache conversion*")
              (with-current-buffer "*VM cache conversion*"
                (setq report (buffer-string)))
              (kill-buffer "*VM cache conversion*"))
            (should report)
            (should (string-match-p "shut" report)))
        (set-file-modes shut #o700)))))

(ert-deftest vm-folder-test-an-unreadable-folder-says-so ()
  "REGRESSION: a file that cannot be read or is gone says which.
Issue #771.  Both came back \"has no folder type VM recognizes\", which sends
the reader to look at the contents of a file they cannot open or that is not
there at all."
  (vm-folder-test-with-directory dir
    (let ((gone (expand-file-name "not-here" dir))
          (shut (expand-file-name "shut" dir))
          (text-quoting-style 'grave))
      (should (string-match-p "does not exist"
                              (cadr (should-error
                                     (vm-change-folder-type-of-file gone 'mboxcl2)))))
      (write-region vm-folder-test--uncounted-cache nil shut nil 'quiet)
      (set-file-modes shut #o000)
      (unwind-protect
          (should (string-match-p "cannot be read"
                                  (cadr (should-error
                                         (vm-change-folder-type-of-file shut 'mboxcl2)))))
        (set-file-modes shut #o600)))))

(defconst vm-folder-test--sound-mboxcl2-message
  (concat "From VM Thu May  7 06:22:17 2026\n"
          "Content-Length: 11\n"
          "From: a@example.com\nSubject: one\n"
          "\nBody one.\n\n")
  "A message as VM writes one into an mboxcl2 folder.
`Content-Length' first in the header block, which is where the conversion puts
it, and 11 counts the body and the blank line that ends the message.  Written
any other way the folder is not sound and a conversion rewrites it, which is
what the test below is distinguishing.")

(ert-deftest vm-folder-test-a-sound-folder-is-renamed-rather-than-rewritten ()
  "A folder already sound is renamed where its name does not state its type.
Nothing has to be written for that, and the name is what the type is read from
next time, so leaving it would mean the conversion did not outlive the session
(#743).  A cache does not reach this: one is read as From_ whatever its lengths
look like (#767), and converting From_ to mboxcl2 rewrites the headers."
  (vm-folder-test-with-directory dir
    (let* ((text (concat vm-folder-test--sound-mboxcl2-message
                         vm-folder-test--sound-mboxcl2-message))
           (file (expand-file-name "sent" dir))
           (vm-trust-content-length t))
      (write-region text nil file nil 'quiet)
      ;; read as mboxcl2 by its contents, under a name that says nothing
      (should (eq (vm-get-folder-type file) 'mboxcl2))
      (vm-change-folder-type-of-file file 'mboxcl2)
      (should-not (file-exists-p file))
      (should (file-exists-p (concat file ".mboxcl2")))
      ;; renamed, so the bytes are the same ones and there is no backup
      (should (equal text (with-temp-buffer
                            (insert-file-contents (concat file ".mboxcl2"))
                            (buffer-string))))
      (should-not (file-exists-p (vm-folder-backup-name file))))))

(ert-deftest vm-folder-test-a-cache-that-cannot-be-converted-is-reported ()
  "One cache that cannot be converted does not stop the others.
The fault is named and the run goes on: stopping partway through a dozen
caches would leave a half-done job and no account of it."
  (vm-folder-test--with-cache-directory dir
    (let ((good (vm-folder-test--cache dir "imap-cache-0123456789abcdef"))
          (bad (vm-folder-test--cache dir "pop-cache-fedcba9876543210"
                                      "this is not a folder at all\n"))
          (report nil)
          (said nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said))))
        (vm-convert-caches-to-mboxcl2))
      (should (file-exists-p (concat good ".mboxcl2")))
      (should (file-exists-p bad))
      (when (get-buffer "*VM cache conversion*")
        (with-current-buffer "*VM cache conversion*"
          (setq report (buffer-string)))
        (kill-buffer "*VM cache conversion*"))
      (should (string-match-p "pop-cache" report))
      (should (cl-find-if (lambda (s)
                            (string-match-p "1 cache of 2 converted, 1 could not be" s))
                          said)))))

(ert-deftest vm-folder-test-converting-the-caches-with-nothing-to-do ()
  "Nothing to convert says so, and asks nothing."
  (vm-folder-test--with-cache-directory dir
    (vm-folder-test--cache dir "imap-cache-abcdef0123456789.mboxcl2")
    (let ((said nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "Nothing should be asked")))
                ((symbol-function 'vm-inform)
                 (lambda (_level &rest args) (push (apply #'format args) said))))
        (vm-convert-caches-to-mboxcl2))
      (should (cl-find-if (lambda (s) (string-match-p "No cache is in the older" s))
                          said)))))

(ert-deftest vm-folder-test-declining-the-question-converts-nothing ()
  "Answering no leaves every cache as it was."
  (vm-folder-test--with-cache-directory dir
    (let ((old (vm-folder-test--cache dir "imap-cache-0123456789abcdef")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (vm-convert-caches-to-mboxcl2))
      (should (file-exists-p old))
      (should-not (file-exists-p (concat old ".mboxcl2"))))))

(ert-deftest vm-folder-test-a-prefix-argument-asks-about-each-cache ()
  "With a prefix argument each cache is asked about, and only those accepted go."
  (vm-folder-test--with-cache-directory dir
    (let ((first (vm-folder-test--cache dir "imap-cache-0123456789abcdef"))
          (second (vm-folder-test--cache dir "pop-cache-fedcba9876543210"))
          (asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt asked)
                   (string-match-p "imap-cache" prompt))))
        (vm-convert-caches-to-mboxcl2 t))
      (should (equal (length asked) 2))
      (should (file-exists-p (concat first ".mboxcl2")))
      (should (file-exists-p second))
      (should-not (file-exists-p (concat second ".mboxcl2"))))))

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
  "A visited folder is refused whether or not it has changes, and named.
Changes make it a different message, since saving is the way out of that one.
An unmodified folder in use is refused too (#770): killing it took its summary
and presentation with it into a state where every command answered \"Folder
buffer has been killed\"."
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
      (should (string-match-p "being visited"
                              (cadr (should-error
                                     (vm-change-folder-type-of-file file 'mboxcl2)))))
      ;; and the folder is still there to go back to
      (should (vm-get-file-buffer file))
      ;; quitting it is the way through, and then the conversion writes the
      ;; folder under the name mboxcl2 asks for (emacs-vm/vm#743)
      (with-current-buffer (vm-get-file-buffer file) (vm-quit-no-change))
      (vm-change-folder-type-of-file file 'mboxcl2)
      (with-temp-buffer
        (insert-file-contents (vm-folder-name-for-type file 'mboxcl2))
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
    (let ((vm-folder-type-by-extension-alist nil))
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

;;; Which body lines get quoted, which the manual states exactly

(ert-deftest vm-folder-test-only-a-from-line-ending-in-a-digit-is-quoted ()
  "VM quotes `^From .*[0-9]$' in a body and nothing else.
The manual says so in as many words, so it wants a test: a reader deciding
whether their folder is safe to hand to another program is relying on the
rule being what the text claims.  Converting to From_ is the path that can be
driven without a server; the same `vm-munge-message-separators' serves mail
arriving over POP and IMAP, editing, composing and bursting a digest."
  (vm-folder-test-with-directory dir
    (let* ((body (concat "Quoting an old note:\n"
                         "From bob@example.com Mon Jan  1 12:00:00 2026\n"
                         "From now on we ship on Fridays\n"
                         "From bob and no digit\n"
                         ">From already quoted when it arrived 2026\n"))
           (file (expand-file-name "folder.mboxcl2" dir))
           (message (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            "Content-Length: "
                            (number-to-string (1+ (length body)))
                            "\nFrom: alice@example.com\nSubject: one\n\n"
                            body "\n"))
           written)
      (write-region message nil file nil 'quiet)
      (vm-change-folder-type-of-file file 'From_)
      (setq written (with-temp-buffer
                      (insert-file-contents-literally
                       (expand-file-name "folder" dir))
                      (buffer-string)))
      ;; begins From_ and ends in a digit: quoted
      (should (string-match-p
               "^>From bob@example\\.com Mon Jan  1 12:00:00 2026$" written))
      ;; begins From_ and does not end in a digit: left alone
      (should (string-match-p "^From now on we ship on Fridays$" written))
      (should (string-match-p "^From bob and no digit$" written))
      ;; already quoted when it arrived: left alone, and now indistinguishable
      ;; from one VM quoted, which is the round trip the manual warns about
      (should (string-match-p
               "^>From already quoted when it arrived 2026$" written))
      (should-not (string-match-p ">>From" written)))))

(ert-deftest vm-folder-test-mboxcl2-quotes-nothing ()
  "Converting to mboxcl2 alters no body line.
The whole difference between mboxcl2 and the older mboxcl, which counted the
bytes and quoted as well, and the reason mboxcl2 stores a message as it
arrived (emacs-vm/vm#466)."
  (vm-folder-test-with-directory dir
    (let* ((body (concat "Quoting an old note:\n"
                         "From bob@example.com Mon Jan  1 12:00:00 2026\n"))
           (file (expand-file-name "folder" dir))
           (message (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            "From: alice@example.com\nSubject: one\n\n"
                            body "\n")))
      ;; the From-line in the body is not preceded by a blank line, so the
      ;; folder reads as the one message it is
      (write-region message nil file nil 'quiet)
      (vm-change-folder-type-of-file file 'mboxcl2)
      (let ((written (with-temp-buffer
                       (insert-file-contents-literally
                        (expand-file-name "folder.mboxcl2" dir))
                       (buffer-string))))
        (should (string-match-p
                 "^From bob@example\\.com Mon Jan  1 12:00:00 2026$" written))
        (should-not (string-match-p ">From" written))))))

(defun vm-folder-test--plain-rule-separators (file)
  "How many separators a reader using the plain mbox rule finds in FILE.
Any line beginning \"From \" that stands at the start of the file or after a
blank line, which is the rule most mailers use and the one Zawinski
describes.  VM's own rule is narrower, and the difference is what the two
tests below measure."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let ((found 0))
      (while (re-search-forward "^From " nil t)
        (goto-char (match-beginning 0))
        (when (or (bobp) (equal (char-after (- (point) 2)) ?\n))
          (setq found (1+ found)))
        (forward-line 1))
      found)))

(ert-deftest vm-folder-test-a-from_-folder-can-split-elsewhere ()
  "A From_ folder VM wrote is read as two messages by the plain rule.
The manual says so, so it wants a test.  VM escapes only a `From ' line that
ends in a digit, so \"From the desk of Bob\" after a blank line is left as it
was written: right by VM's rule and wrong by everyone else's."
  (vm-folder-test-with-directory dir
    (let* ((body "Here is the note.\n\nFrom the desk of Bob\n\nregards\n")
           (source (expand-file-name "inbox.mboxcl2" dir))
           (written (expand-file-name "inbox" dir)))
      (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            "Content-Length: " (number-to-string (1+ (length body)))
                            "\nFrom: alice@example.com\nSubject: one\n\n"
                            body "\n")
                    nil source nil 'quiet)
      (vm-change-folder-type-of-file source 'From_)
      ;; VM left it alone, having no reason of its own to escape it
      (should-not (string-match-p ">From the desk"
                                  (with-temp-buffer
                                    (insert-file-contents-literally written)
                                    (buffer-string))))
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (vm-visit-folder written))
      (should (equal (length vm-message-list) 1))
      (should (equal (vm-folder-test--plain-rule-separators written) 2)))))

(ert-deftest vm-folder-test-an-mboxcl2-folder-splits-without-the-header ()
  "An mboxcl2 folder is read wrongly by anything that ignores the length.
Nothing in one is escaped, which is what makes it store a message as it
arrived and what makes it unreadable to a program that does not honour
`Content-Length'."
  (vm-folder-test-with-directory dir
    (let* ((body (concat "Here is the note.\n\n"
                         "From bob@example.com Mon Jan  1 12:00:00 2026\n\n"
                         "regards\n"))
           (file (expand-file-name "kept.mboxcl2" dir)))
      (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            "Content-Length: " (number-to-string (1+ (length body)))
                            "\nFrom: alice@example.com\nSubject: one\n\n"
                            body "\n")
                    nil file nil 'quiet)
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (vm-visit-folder file))
      (should (equal (length vm-message-list) 1))
      (should (equal (vm-folder-test--plain-rule-separators file) 2)))))

;;; mboxcl2 lengths are octets, and a wrong one does not stop the reader

(defconst vm-folder-test--accented-body
  "Caf\N{U+00E9} na\N{U+00EF}ve \N{U+00FC}ber stra\N{U+00DF}e\n"
  "A body whose UTF-8 is longer than its character count.
Four characters of it take two octets each, so a length counted in characters
and one counted in octets differ by four, which is what the tests below tell
apart.")

(defun vm-folder-test--accented-message ()
  "One From_ message whose body is `vm-folder-test--accented-body'."
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: one\n"
          "Content-Type: text/plain; charset=utf-8\n\n"
          vm-folder-test--accented-body "\n"))

(defun vm-folder-test--declared-and-actual (file)
  "Answer (DECLARED . ACTUAL) for the first message of mboxcl2 FILE.
DECLARED is what its `Content-Length' says, ACTUAL the octets from the end of
the header block to the next separator or the end.  Read literally into a
unibyte buffer, or the measurement would make the mistake it is looking for."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let ((declared (and (re-search-forward "^Content-Length: \\([0-9]+\\)$" nil t)
                         (string-to-number (match-string 1)))))
      (goto-char (point-min))
      (re-search-forward "\n\n" nil t)
      (let* ((start (point))
             (end (or (and (re-search-forward "^From " nil t) (match-beginning 0))
                      (point-max))))
        (cons declared (- end start))))))

(ert-deftest vm-folder-test-a-converted-length-counts-octets ()
  "The on-disk conversion writes octet counts for a body that is not ASCII.
Not a repair: it is right, and this is what says so.  A refactor that let a
multibyte buffer into the conversion would write character counts instead, and
every folder it wrote would be wrong in a way VM itself would not notice --
the reader falls back on searching for the next separator, so only another
program reading the folder would see it."
  (vm-folder-test-with-directory dir
    (let ((file (expand-file-name "folder" dir))
          (coding-system-for-write 'utf-8))
      (write-region (concat (vm-folder-test--accented-message)
                            (vm-folder-test--accented-message))
                    nil file nil 'quiet)
      (vm-change-folder-type-of-file file 'mboxcl2)
      (let ((lengths (vm-folder-test--declared-and-actual
                      (concat file ".mboxcl2"))))
        (should (car lengths))
        ;; ACTUAL is measured in a unibyte buffer, so agreeing with it is
        ;; what says the declared length is octets: a character count would
        ;; be four short, one for each two-octet character in the body.
        (should (equal (car lengths) (cdr lengths)))))))

(ert-deftest vm-folder-test-the-accented-body-has-multi-octet-characters ()
  "The premise of the two tests above, which would otherwise pass vacuously.
A body of pure ASCII has the same length counted either way, so it could not
tell an octet count from a character count."
  (should (> (length (encode-coding-string vm-folder-test--accented-body 'utf-8))
             (length vm-folder-test--accented-body))))

(ert-deftest vm-folder-test-a-saved-length-counts-octets ()
  "Saving a message into an mboxcl2 folder writes an octet count too.
The other path that writes a length, and the one a reader uses every day."
  (vm-folder-test-with-directory dir
    (let ((src (expand-file-name "inbox" dir))
          (dest (expand-file-name "kept.mboxcl2" dir))
          (coding-system-for-write 'utf-8))
      (write-region (vm-folder-test--accented-message) nil src nil 'quiet)
      (cl-letf (((symbol-function 'vm-warn) #'ignore))
        (vm-visit-folder src))
      (goto-char (point-min))
      (vm-save-message dest 1)
      (let ((lengths (vm-folder-test--declared-and-actual dest)))
        (should (car lengths))
        (should (equal (car lengths) (cdr lengths)))))))

(ert-deftest vm-folder-test-a-malformed-length-does-not-stop-the-reader ()
  "A Content-Length that is nonsense still opens, one message, no error.
The reader falls back on searching for the next separator when the count does
not land on one, which is what makes an mboxcl2 folder no worse than a From_
one when its lengths are wrong.  Worth pinning now that mboxcl2 is what
`vm-default-folder-type' creates: arbitrary mail reaches this code."
  (let ((header (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                        "From: alice@example.com\n")))
    (dolist (length '("999999" "0" "   12" "abc" "99999999999999999999" "-5"))
      (vm-folder-test-with-directory dir
        (let ((file (expand-file-name "odd.mboxcl2" dir)))
          (write-region (concat header "Content-Length: " length
                                "\n\nbody here!\n\n")
                        nil file nil 'quiet)
          (should (eq (vm-get-folder-type file) 'mboxcl2))
          (let ((vm-mboxcl2-strict nil))
            (cl-letf (((symbol-function 'vm-warn) #'ignore))
              (vm-visit-folder file)))
          (should (equal (length vm-message-list) 1)))))))

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

(ert-deftest vm-folder-test-one-length-does-not-make-a-folder-mboxcl2 ()
  "A From_ folder holding a message with a `Content-Length' stays From_.

The two types are the same folder but for that header, so one message's is no
evidence: mail arrives carrying a `Content-Length' of its own, and VM adds one
to each message it rewrites, so a From_ folder ends up with a few.  Read as
mboxcl2, such a folder stops at the first message without one -- 6433 of 6498
messages in a maintainer's IMAP cache had none, and the folder would not open.

Two lengths in a row is what says the folder is written that way.

`vm-default-folder-type' is mboxcl2 here and makes no difference: it decides
a folder VM creates, not one that already exists (#767)."
  (let* ((body "A short body.\n")
         (length (number-to-string (length body)))
         (with-length (concat "From VM Thu May  7 06:22:17 2026\n"
                              "From: a@example.com\nSubject: one\n"
                              "Content-Length: " length "\n\n" body))
         (without (concat "From VM Thu May  7 06:22:18 2026\n"
                          "From: b@example.com\nSubject: two\n\n" body))
         (vm-trust-content-length t)
         (vm-default-folder-type 'mboxcl2)
         (vm-default-From_-folder-type 'From_))
    ;; the maintainer's folder: the first message has one, the rest do not
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert with-length without without)
      (should (eq (vm-get-folder-type) 'From_)))
    ;; a folder written as mboxcl2: every message has one
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert with-length with-length with-length)
      (should (eq (vm-get-folder-type) 'mboxcl2)))
    ;; and a plain From_ folder is unchanged
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert without without)
      (should (eq (vm-get-folder-type) 'From_)))))

(ert-deftest vm-folder-test-the-type-is-read-past-a-long-first-message ()
  "The second message decides even when the first is longer than the first read.

`vm-get-folder-type' reads 4096 bytes of a file, and the maintainer's first
message was 237 kilobytes: without reading further, a folder written as mboxcl2
would be taken for From_ and one that is not for mboxcl2.  It reads as far as
the first message's length says the second one begins,
`vm-folder-type-examine-limit' permitting.

`vm-default-folder-type' is mboxcl2 here and does not enter into it: an
existing folder is read as what it is, and the default decides only what VM
creates (#767)."
  (let* ((body (concat (make-string 20000 ?x) "\n"))
         (length (number-to-string (length body)))
         (with-length (concat "From VM Thu May  7 06:22:17 2026\n"
                              "From: a@example.com\nSubject: big\n"
                              "Content-Length: " length "\n\n" body))
         (without (concat "From VM Thu May  7 06:22:18 2026\n"
                          "From: b@example.com\nSubject: two\n\n" body))
         (dir (file-name-as-directory (make-temp-file "vm-folder-type" t)))
         (vm-trust-content-length t)
         (vm-default-folder-type 'mboxcl2)
         (vm-default-From_-folder-type 'From_))
    (unwind-protect
        (let ((all (expand-file-name "all-lengths" dir))
              (one (expand-file-name "one-length" dir))
              (coding-system-for-write 'binary))
          (write-region (concat with-length with-length with-length) nil all
                        nil 'quiet)
          (write-region (concat with-length without without) nil one nil 'quiet)
          (should (eq (vm-get-folder-type all) 'mboxcl2))
          (should (eq (vm-get-folder-type one) 'From_)))
      (delete-directory dir t))))

;;; The repair a folder VM will not read is told to use

(ert-deftest vm-folder-test-a-lax-read-warns-once-and-counts-no-lines ()
  "Reading such a folder with `vm-mboxcl2-strict' nil warns once, for the folder.
It used to warn per message, naming its line, and both halves of that are
quadratic: `line-number-at-pos' counts from the start of the buffer, and
`vm-warn' pauses for two seconds on every warning whose text is new, which a
line number makes each of them.  The repair itself reads a folder this way, so
on a 1.1 gigabyte IMAP cache short of a length on 6433 of its 6498 messages the
repair the strict error recommends did not finish; it takes seven seconds now.

What is checked is the text and the counting, not a duration: a folder big
enough to be slow would be too slow to have in a test."
  (let ((folder (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                        "From: alice@example.com\nSubject: one\n"
                        "Content-Length: 10\n\nBody one.\n"))
        (texts nil)
        (numberings 0))
    (dotimes (i 40)
      (setq folder (concat folder
                           (format "From bob@example.com Sun Aug  9 09:%02d:00 2026\n"
                                   i)
                           "From: bob@example.com\nSubject: short\n\nBody.\n\n")))
    (with-temp-buffer
      (insert folder)
      (let ((vm-folder-type 'mboxcl2)
            (vm-mboxcl2-strict nil)
            (number (symbol-function 'line-number-at-pos)))
        (cl-letf (((symbol-function 'vm-warn)
                   (lambda (_l _secs &rest args)
                     (push (apply #'format args) texts)))
                  ((symbol-function 'line-number-at-pos)
                   (lambda (&rest args)
                     (setq numberings (1+ numberings))
                     (apply number args))))
          (should (= (vm-count-messages-in-buffer) 41)))))
    ;; every message short of a length was warned about
    (should (> (length texts) 1))
    ;; with one text, so `vm-warn' shows it once and pauses once
    (should (= (length (delete-dups (copy-sequence texts))) 1))
    ;; and no line was numbered: naming the message is what cost the scan
    (should (= numberings 0))))

(ert-deftest vm-folder-test-a-half-read-folder-is-not-converted-in-place ()
  "The buffer a failed visit leaves behind is refused, not converted.
That buffer holds only the messages read before the error and has no
`vm-message-pointer'.  Converting it rewrote those and left the rest in the
format they were, then signalled `wrong-type-argument arrayp nil' on the
missing pointer -- with the folder already backed up and the buffer already
modified.  This is what a reader who misses the prefix argument does, since
the error that sends them here leaves them in exactly that buffer."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((vm-mboxcl2-strict t)
          (text-quoting-style 'grave))
      (should-error (vm-visit-folder file))
      (with-current-buffer (vm-get-file-buffer file)
        (should vm-message-list)
        (should-not vm-message-pointer)
        (let ((err (should-error (vm-change-folder-type 'mboxcl2))))
          (should (eq (car err) 'error))
          (should (string-match-p "not read all the way through" (cadr err)))
          (should (string-match-p "vm-change-folder-type" (cadr err))))
        (should-not (buffer-modified-p)))
      ;; and it did not get as far as backing the folder up or writing it
      (should-not (file-exists-p (vm-folder-backup-name file)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) vm-folder-test--seven-and-two-short))))))

(ert-deftest vm-folder-test-a-prefix-argument-repairs-the-folder-it-asks-for ()
  "The whole of the documented repair, from the interactive spec onwards.
`C-u M-x vm-change-folder-type mboxcl2' is what the error tells the reader to
type, so what that reads from the minibuffer has to reach the on-disk
conversion and the folder has to open afterwards."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((vm-mboxcl2-strict t))
      (should-error (vm-visit-folder file))
      ;; the name is not `file': the interactive spec binds one of its own,
      ;; and a special variable of that name would be shadowed by it
      (let* ((answer file)
             (args (let ((current-prefix-arg '(4)))
                     (cl-letf (((symbol-function 'vm-read-string)
                                (lambda (&rest _) "mboxcl2"))
                               ((symbol-function 'vm-read-file-name)
                                (lambda (&rest _) answer)))
                       (eval (cadr (interactive-form 'vm-change-folder-type))
                             t)))))
        ;; the third is the output file, which one prefix argument does not
        ;; ask for
        (should (equal args (list 'mboxcl2 file nil)))
        (apply #'vm-change-folder-type args))
      (should (file-exists-p (vm-folder-backup-name file)))
      (vm-visit-folder file)
      (should (= (length vm-message-list) 3))
      (should (eq vm-folder-type 'mboxcl2)))))

(ert-deftest vm-folder-test-both-conversion-paths-check-the-type ()
  "`From_-with-Content-Length' reaches either path as `mboxcl2', and a type
neither can write is refused before a folder is rewritten in it.  The checks
used to be inside the in-buffer branch, so the on-disk one took whatever it
was given and the alias the docstring promises was rejected."
  (vm-folder-test-with-file (file "plain.mbox"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (let ((text-quoting-style 'grave))
      (vm-change-folder-type 'From_-with-Content-Length file)
      (with-temp-buffer
        (insert-file-contents (vm-folder-name-for-type file 'mboxcl2))
        (should (string-match-p "^Content-Length: [0-9]+$" (buffer-string))))
      (should (string-match-p "Unknown folder type: mbox"
                              (cadr (should-error
                                     (vm-change-folder-type 'mbox file))))))))

;;; a cache file says its type in its name (issue #736)

(defmacro vm-folder-test--in-a-cache-directory (&rest body)
  "Run BODY with DIR an empty directory both cache directories point at.
BODY can look at WARNINGS, the list of warning strings."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "vm-cache-" t))
          (vm-imap-folder-cache-directory dir)
          (vm-pop-folder-cache-directory dir)
          (warnings nil))
     (unwind-protect
         (cl-letf (((symbol-function 'vm-warn)
                    (lambda (_level _secs format &rest args)
                      (push (apply #'format format args) warnings))))
           ,@body)
       (delete-directory dir t))))

(ert-deftest vm-folder-test-a-new-cache-is-named-after-its-type ()
  "A cache that does not exist yet gets the type suffix."
  (vm-folder-test--in-a-cache-directory
    (let ((base (expand-file-name "imap-cache-0123456789abcdef" dir)))
      (should (equal (vm-cache-file-in-use base)
                     (concat base vm-cache-folder-type-suffix)))
      (should-not warnings))))

(ert-deftest vm-folder-test-an-existing-cache-keeps-its-name ()
  "A cache made before VM named them is used as it stands.
Renaming it would say a type of it that need not be true, and refusing it
would mean refetching the mailbox."
  (vm-folder-test--in-a-cache-directory
    (let ((base (expand-file-name "imap-cache-0123456789abcdef" dir)))
      (write-region "From alice@example.com Sat Aug  8 14:24:13 2026\n" nil base)
      (should (equal (vm-cache-file-in-use base) base))
      (should-not warnings))))

(ert-deftest vm-folder-test-a-converted-cache-names-the-file-left-behind ()
  "Both names present means a conversion that left the old file.
The suffixed one is the cache, and the other holds the older mail, so it is
named rather than passed over."
  (vm-folder-test--in-a-cache-directory
    (let ((base (expand-file-name "imap-cache-0123456789abcdef" dir)))
      (write-region "old\n" nil base)
      (write-region "new\n" nil (concat base vm-cache-folder-type-suffix))
      (should (equal (vm-cache-file-in-use base)
                     (concat base vm-cache-folder-type-suffix)))
      (should (= 1 (length warnings)))
      (should (string-match-p "ignoring imap-cache-0123456789abcdef\\'"
                              (car warnings))))))

(ert-deftest vm-folder-test-a-suffixed-cache-is-still-a-cache ()
  "The type suffix does not stop a cache being recognised as one.
`vm-cache-folder-name-p' is what warns a user who opened a cache as a folder
of its own, so a name it does not match is a cache with the warning lost."
  (should (vm-cache-folder-name-p
           (concat "imap-cache-b979c2934ac0b4ba3f08dabfdd1b2299"
                   vm-cache-folder-type-suffix)))
  (should (vm-cache-folder-name-p
           (concat "/home/someone/Mail/pop-cache-0123456789abcdef"
                   vm-cache-folder-type-suffix)))
  (should-not (vm-cache-folder-name-p "imap-cache-0123456789abcdef.txt")))

(ert-deftest vm-folder-test-an-imap-cache-file-name-carries-the-type ()
  "The name VM builds for a maildrop is the suffixed one, or the file there."
  (vm-folder-test--in-a-cache-directory
    (let* ((spec "imap-ssl:imap.example.com:993:INBOX:login:someone:*")
           (named (vm-imap-make-filename-for-spec spec))
           (base (string-remove-suffix vm-cache-folder-type-suffix named)))
      (should (string-suffix-p vm-cache-folder-type-suffix named))
      (should-not (equal named base))
      (write-region "From alice@example.com Sat Aug  8 14:24:13 2026\n" nil base)
      (should (equal (vm-imap-make-filename-for-spec spec) base)))))

;;; the guess is deprecated and says so (issue #736)

(defmacro vm-folder-test--warnings (&rest body)
  "Run BODY with `vm-warn' captured, and answer with the warnings it gave."
  (declare (indent 0) (debug t))
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'vm-warn)
                (lambda (_level _secs format &rest args)
                  (push (apply #'format format args) warnings))))
       ,@body)
     (nreverse warnings)))

(ert-deftest vm-folder-test-a-guessed-type-warns-once-per-folder ()
  "Reading a folder as mboxcl2 because of how it looks says so, once.
`vm-trust-content-length' is the last release to decide a type by looking, and
a folder read as something it does not say it is is why."
  (vm-folder-test-with-file
      (file "guessed"
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "Content-Length: 5\nFrom: alice@example.com\n"
                    "Subject: one\n\nbody\n"
                    "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                    "Content-Length: 5\nFrom: bob@example.com\n"
                    "Subject: two\n\nbody\n"))
    (let ((vm-trust-content-length t)
          (vm-folder-type-by-extension-alist nil)
          (vm-guessed-folder-types nil))
      (let ((said (vm-folder-test--warnings
                    (should (eq 'mboxcl2 (vm-get-folder-type file))))))
        (should (= 1 (length said)))
        (should (string-match-p "name such a folder .mboxcl2" (car said)))
        (should (string-match-p "guessed" (car said))))
      ;; the same folder again says nothing
      (should-not (vm-folder-test--warnings (vm-get-folder-type file))))))

(ert-deftest vm-folder-test-a-named-type-is-not-guessed-and-says-nothing ()
  "A folder whose name says its type is not looked at, so nothing is warned.
That is the way out of the warning above, so it has to be silent."
  (vm-folder-test-with-file
      (file "named.mboxcl2"
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "Content-Length: 5\nFrom: alice@example.com\n"
                    "Subject: one\n\nbody\n"))
    (let ((vm-trust-content-length t)
          (vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2)))
          (vm-guessed-folder-types nil))
      (should-not (vm-folder-test--warnings
                    (should (eq 'mboxcl2 (vm-get-folder-type file))))))))

(ert-deftest vm-folder-test-the-trust-setting-is-called-deprecated-at-startup ()
  "An init file that still turns the guess on is told, and is not stopped.
The setting works this release; an error would stop a working configuration
from starting."
  (let ((vm-trust-content-length t)
        (vm-default-folder-type 'mboxcl2))
    (let ((said (vm-folder-test--warnings
                  (vm-warn-about-deprecated-trust-setting))))
      (should (= 1 (length said)))
      (should (string-match-p "deprecated" (car said)))
      (should (string-match-p "vm-default-folder-type no longer needs"
                              (car said)))))
  ;; and nothing to say where it is off
  (let ((vm-trust-content-length nil))
    (should-not (vm-folder-test--warnings
                  (vm-warn-about-deprecated-trust-setting)))))

(ert-deftest vm-folder-test-the-default-folder-type-is-From_-everywhere ()
  "No platform decides `vm-default-folder-type' now.
It was mboxcl2 on Solaris, AIX and System V: a guess about the local delivery
agent, and the only thing that made `vm-trust-content-length' default on."
  (should (eq 'From_ (eval (car (get 'vm-default-folder-type 'standard-value)))))
  (let ((vm-default-folder-type 'From_))
    (should-not (eval (car (get 'vm-trust-content-length 'standard-value))))))

;;; the extension says what a folder is (issue #741)

(ert-deftest vm-folder-test-the-extension-says-the-type ()
  "The extension decides, matched literally, wherever the folder sits."
  (let ((vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2))))
    (should (eq 'mboxcl2 (vm-folder-type-for-name "/home/me/mail/sent.mboxcl2")))
    (should (eq 'mboxcl2 (vm-folder-type-for-name "sent.mboxcl2")))
    ;; no extension, so the name says nothing: an inbox has to be renamed or
    ;; left to vm-default-folder-type
    (should-not (vm-folder-type-for-name "/home/me/mail/current/INBOX"))
    (should-not (vm-folder-type-for-name "/home/me/mail/imap-cache-0123abcd"))
    ;; a directory cannot be claimed, which is the point
    (should-not (vm-folder-type-for-name "/home/me/mail/current/archive.mbox"))
    (should-not (vm-folder-type-for-name nil))
    ;; and the extension is matched whole, not as a pattern
    (should-not (vm-folder-type-for-name "/home/me/mail/sent.mboxcl2x"))
    (should-not (vm-folder-type-for-name "/home/me/mail/sent.xmboxcl2"))))

;;; the type a new folder is written in, and keeping a length true (issue #736)

(ert-deftest vm-folder-test-the-type-to-write-comes-from-the-name ()
  "An empty folder is written in the type its name asks for.
It has no type of its own to read, so the name is the only place its type can
have been stated, and it has to beat `vm-default-folder-type'."
  (let ((vm-default-folder-type 'From_)
        (vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2))))
    (with-temp-buffer
      (setq vm-folder-type nil)
      (should (eq 'mboxcl2 (vm-folder-type-to-write "/tmp/imap-cache-ab.mboxcl2")))
      (should (eq 'From_ (vm-folder-type-to-write "/tmp/imap-cache-ab")))
      ;; what the folder already is wins over both
      (setq vm-folder-type 'BellFrom_)
      (should (eq 'BellFrom_ (vm-folder-type-to-write "/tmp/imap-cache-ab.mboxcl2"))))))

(defmacro vm-folder-test--two-mboxcl2-messages (&rest body)
  "Visit a two-message mboxcl2 folder and run BODY in it.
The first message is written as a headers-only fetch writes one: a length of
zero and no body, so its text region is empty and ends where the second
message begins."
  (declare (indent 0) (debug t))
  `(vm-folder-test-with-file
       (file "two.mboxcl2"
             (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                     "Content-Length: 0\nFrom: alice@example.com\n"
                     "Subject: one\n\n"
                     "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                     "Content-Length: 5\nFrom: bob@example.com\n"
                     "Subject: two\n\nbody\n"))
     (let ((vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2))))
       (vm-folder-test--visiting file
         (should (eq vm-folder-type 'mboxcl2))
         ,@body))))

(ert-deftest vm-folder-test-a-body-lands-before-the-next-message-not-inside-it ()
  "REGRESSION: a retrieved body does not end up in the next message's headers.
Issue #737.  An mboxcl2 folder's trailing separator is empty, so a message
whose body has not been retrieved ends exactly where the next one begins --
and a marker at the position text is inserted at stays in front of that text,
so the next message's start was left reading the body."
  (vm-folder-test--two-mboxcl2-messages
    (let ((m1 (car vm-message-list))
          (m2 (nth 1 vm-message-list))
          (inhibit-read-only t))
      ;; the precondition: the two positions are the same one
      (should (= (vm-text-end-of m1) (vm-start-of m2)))
      (save-restriction
        (widen)
        (narrow-to-region (vm-headers-of m1) (vm-text-end-of m1))
        (goto-char (vm-text-of m1))
        (insert "INSERTED BODY\n")
        (vm-settle-message-boundaries m1))
      (save-restriction
        (widen)
        (should (= (vm-start-of m2) (vm-text-end-of m1)))
        (should (string-prefix-p
                 "From bob@example.com"
                 (buffer-substring (vm-start-of m2)
                                   (min (point-max)
                                        (+ 20 (marker-position
                                               (vm-start-of m2)))))))
        ;; and the body is where it belongs, in message one
        (should (string-match-p
                 "INSERTED BODY"
                 (buffer-substring (vm-text-of m1) (vm-text-end-of m1))))))))

(ert-deftest vm-folder-test-a-body-that-arrives-brings-its-length-up-to-date ()
  "A `Content-Length' says what the body is now, not what it was.
An external message's headers are written with a length of zero and the body
arrives later.  In an mboxcl2 folder that header is where the next message
starts, so a stale one puts every message after this one in the wrong place."
  (vm-folder-test--two-mboxcl2-messages
    (let ((m (car vm-message-list))
          (inhibit-read-only t))
      (save-restriction
        (widen)
        (goto-char (vm-text-of m))
        (insert "six.\n")
        (set-marker (vm-text-end-of m) (point))
        (vm-set-content-length-of m)
        (goto-char (vm-headers-of m))
        (should (re-search-forward "^Content-Length: 5$" (vm-text-of m) t))))))


;;; a folder is shown under its own name (issue #738)

(ert-deftest vm-folder-test-a-folder-keeps-its-name-over-an-open-buffer ()
  "REGRESSION: a folder is shown under its name even where its file was open.
Issue #738.  `vm-read-folder' answered with the buffer already visiting the
file and left it named after the file, which for an IMAP or POP folder is the
local cache -- imap-cache-<md5>, saying nothing about which mailbox it holds.
Something else makes that buffer: desktop.el restoring the session,
`recover-file', or a plain `find-file'."
  (vm-folder-test-with-file
      (file "imap-cache-0123456789abcdef"
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "From: alice@example.com\nSubject: one\n\nBody.\n"))
    (let ((opened (find-file-noselect file))
          (before (buffer-list)))
      (unwind-protect
          (progn
            ;; as something other than VM left it
            (should (equal (buffer-name opened)
                           (file-name-nondirectory file)))
            (let ((buffer (vm-read-folder file nil "ucsc")))
              (should (eq buffer opened))
              (should (equal (buffer-name buffer) "ucsc"))))
        (dolist (buffer (buffer-list))
          (when (or (eq buffer opened) (not (memq buffer before)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest vm-folder-test-a-folder-with-no-name-of-its-own-keeps-the-buffer ()
  "A plain file folder has no name but its file's, and nothing is renamed."
  (vm-folder-test-with-file
      (file "plain.mbox"
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "From: alice@example.com\nSubject: one\n\nBody.\n"))
    (let ((opened (find-file-noselect file)))
      (unwind-protect
          (progn
            (should (eq opened (vm-read-folder file)))
            (should (equal (buffer-name opened)
                           (file-name-nondirectory file))))
        (when (buffer-live-p opened)
          (with-current-buffer opened (set-buffer-modified-p nil))
          (kill-buffer opened))))))


;;; a cache with no type in its name is the older format (issue #739)

(defconst vm-folder-test--two-with-lengths
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "Content-Length: 22\nFrom: alice@example.com\nSubject: one\n\n"
          "From $249.50 a month\n\n"
          "From bob@example.com Sat Aug  8 14:25:13 2026\n"
          "Content-Length: 5\nFrom: bob@example.com\nSubject: two\n\nbody\n")
  "A folder carrying a length on each message, whose first body holds a
From_ line.  mboxcl2 does not escape one -- the lengths delimit instead -- so
read as From_ this folder has a message of nonsense in it.")

(ert-deftest vm-folder-test-a-cache-not-named-mboxcl2-is-the-older-format ()
  "A cache whose name does not say mboxcl2 is read as the older format.
Issue #739.  Its lengths are not to be relied on: VM gave one to each message
it rewrote, so a From_ cache collects a few, and a length believed wrongly
puts a message boundary inside a body.  Nothing here is guessed at."
  (vm-folder-test-with-file
      (file "imap-cache-0123456789abcdef" vm-folder-test--two-with-lengths)
    (let ((vm-trust-content-length nil)
          (vm-folder-type-by-extension-alist nil)
          (vm-unnamed-mboxcl2-caches nil)
          (vm-default-From_-folder-type 'From_))
      (should (eq 'From_ (vm-get-folder-type file))))))

(ert-deftest vm-folder-test-a-cache-that-looks-like-mboxcl2-says-so ()
  "Looking like the other format is worth one warning, naming the way out.
Renaming the file is all it takes where the folder really is mboxcl2, and
that is the reader\\='s call, not VM\\='s."
  (vm-folder-test-with-file
      (file "imap-cache-0123456789abcdef" vm-folder-test--two-with-lengths)
    (let ((vm-trust-content-length nil)
          (vm-folder-type-by-extension-alist nil)
          (vm-unnamed-mboxcl2-caches nil)
          (vm-default-From_-folder-type 'From_))
      (let ((said (vm-folder-test--warnings (vm-get-folder-type file))))
        (should (= 1 (length said)))
        (should (string-match-p "rename it" (car said)))
        (should (string-match-p "\\.mboxcl2" (car said))))
      ;; and once only
      (should-not (vm-folder-test--warnings (vm-get-folder-type file))))))

(ert-deftest vm-folder-test-a-named-cache-is-mboxcl2-and-says-nothing ()
  "The way out works: the same bytes under the suffixed name are mboxcl2."
  (vm-folder-test-with-file
      (file "imap-cache-0123456789abcdef.mboxcl2" vm-folder-test--two-with-lengths)
    (let ((vm-trust-content-length nil)
          (vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2)))
          (vm-unnamed-mboxcl2-caches nil))
      (should-not (vm-folder-test--warnings
                    (should (eq 'mboxcl2 (vm-get-folder-type file))))))))

(ert-deftest vm-folder-test-a-From_-cache-says-nothing ()
  "A cache with no lengths in it is the older format and unremarkable."
  (vm-folder-test-with-file
      (file "imap-cache-0123456789abcdef"
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "From: alice@example.com\nSubject: one\n\nBody.\n\n"
                    "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                    "From: bob@example.com\nSubject: two\n\nBody.\n"))
    (let ((vm-trust-content-length nil)
          (vm-folder-type-by-extension-alist nil)
          (vm-unnamed-mboxcl2-caches nil)
          (vm-default-From_-folder-type 'From_))
      (should-not (vm-folder-test--warnings
                    (should (eq 'From_ (vm-get-folder-type file))))))))

;;; an extension that names no folder type says so (issue #741)

(ert-deftest vm-folder-test-a-bad-type-in-an-extension-rule-is-caught ()
  "An extension cannot be mistyped -- it is matched literally -- but the type
it names can be, and a symbol that is not a folder type is as quiet as a
regexp that matched nothing used to be."
  (let ((vm-folder-type-by-extension-alist '(("mbox" . mbox))))
    (let ((said (vm-folder-test--warnings (vm-check-folder-type-extensions))))
      (should (= 1 (length said)))
      (should (string-match-p "not (EXTENSION . TYPE)" (car said)))))
  ;; the old name for mboxcl2 is still a folder type, and the default is fine
  (let ((vm-folder-type-by-extension-alist
         '(("mboxcl2" . From_-with-Content-Length))))
    (should-not (vm-folder-test--warnings (vm-check-folder-type-extensions))))
  (let ((vm-folder-type-by-extension-alist
         (eval (car (get 'vm-folder-type-by-extension-alist 'standard-value)))))
    (should-not (vm-folder-test--warnings (vm-check-folder-type-extensions)))))


;; The periodic mail check, which runs from a timer.  A timer cannot answer a
;; password prompt, and an error in one is reported every time it fires
;; (emacs-vm/vm#712).

(defmacro vm-folder-test--with-a-check-that-cannot-start (method &rest body)
  "Run BODY in a folder buffer of METHOD whose driver check cannot start.
The driver answers nil for a maildrop VM holds no password for, which is the
case a mail-check timer meets and cannot do anything about."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq vm-folder-access-method ,method)
     (let ((vm-global-block-new-mail nil))
       (cl-letf (((symbol-function 'vm-imap-net-folder-check-mail) #'ignore)
                 ((symbol-function 'vm-pop-net-folder-check-mail) #'ignore))
         ,@body))))

(ert-deftest vm-folder-test-a-check-that-cannot-start-does-not-signal ()
  "A check that cannot reach the server answers no rather than signalling.

`vm-check-mail-itimer-function' calls this every `vm-mail-check-interval'
seconds and cannot be asked for a password, so a maildrop VM has no password
for filled *Messages* with \"Error running timer\" for as long as Emacs ran.
There is one check now and it is the driver's, which answers nil rather than
signalling; the blocking check this used to fall through to, and the
once-per-failure bookkeeping that went with it, are gone."
  (dolist (method '(imap pop))
    (vm-folder-test--with-a-check-that-cannot-start method
      (should-not (vm-check-for-spooled-mail nil t))
      ;; and again, since a timer calls it over and over
      (dotimes (_ 5)
        (should-not (vm-check-for-spooled-mail nil t))))))

(ert-deftest vm-folder-test-a-check-that-cannot-start-says-nothing ()
  "It says nothing a reader sees, however often it runs.

Reporting it every interval was the complaint: a folder whose password VM does
not hold is one nothing changes about until the reader does something.  The
driver says so at level 6, which `vm-verbosity' 5 does not show, so the
silence is the default rather than something counted."
  (vm-folder-test--with-a-check-that-cannot-start 'imap
    (let ((said (vm-folder-test--warnings
                  (dotimes (_ 5)
                    (vm-check-for-spooled-mail nil t)))))
      (should (equal said nil)))))

;; Changing a folder's type changes its name to match, since the name is what
;; states the type (emacs-vm/vm#743).

(defconst vm-folder-test--743-message
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: one\n\nBody.\n\n")
  "A one-message From_ folder, for the conversion-renames-it tests.")

(defmacro vm-folder-test--converting (spec &rest body)
  "Visit the folder SPEC names and run BODY with the purge answered.
SPEC is (VAR NAME CONTENT ANSWER): ANSWER is what `yes-or-no-p' says to the
offer to delete the file left behind, and the conversion is made to look
interactive so that the offer is put at all."
  (declare (indent 1) (debug t))
  (let ((var (nth 0 spec)) (name (nth 1 spec))
        (content (nth 2 spec)) (answer (nth 3 spec)))
    `(vm-folder-test-with-file (,var ,name ,content)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) ,answer))
                 ((symbol-function 'vm-warn) #'ignore))
         (vm-visit-folder ,var)
         ,@body))))

(defun vm-folder-test--convert (type)
  "Change this folder to TYPE as a reader typing the command would.
The command decides whether to offer the deletion with `vm-interactive-p',
a macro over `called-interactively-p', which in batch answers no however
the call is made: `funcall-interactively' does not change it.  So the function
is stubbed, and only around this call.  Stubbed for a whole test body, with
the folder visited inside it, six of these took 25 seconds each."
  (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
    (vm-change-folder-type type)))

(ert-deftest vm-folder-test-conversion-to-mboxcl2-names-the-folder-for-it ()
  "A folder converted to mboxcl2 is written as NAME.mboxcl2, and reads back
as mboxcl2.  Left under a name saying nothing it was read as
`vm-default-folder-type' next time and written back in it, so the conversion
did not outlive the session."
  (vm-folder-test--converting (file "inbox" vm-folder-test--743-message t)
    (vm-folder-test--convert 'mboxcl2)
    (should (equal (buffer-file-name) (concat file ".mboxcl2")))
    (should (eq vm-folder-type 'mboxcl2))
    (vm-quit-no-change)
    ;; the old name is gone, the answer having been yes, and the new one reads
    (should-not (file-exists-p file))
    (vm-visit-folder (concat file ".mboxcl2"))
    (should (eq vm-folder-type 'mboxcl2))
    (should (= (length vm-message-list) 1))))

(ert-deftest vm-folder-test-conversion-away-from-mboxcl2-drops-the-extension ()
  "sent.mboxcl2 converted to From_ is written as sent.  Left named mboxcl2
with its lengths stripped, the folder could not be opened at all: the strict
reader refused it."
  (vm-folder-test--converting (file "sent.mboxcl2"
                                    (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                            "From: alice@example.com\nSubject: one\n"
                                            "Content-Length: 6\n\nBody.\n\n")
                                    t)
    (should (eq vm-folder-type 'mboxcl2))
    (vm-folder-test--convert 'From_)
    (should (equal (buffer-file-name) (file-name-sans-extension file)))
    (vm-quit-no-change)
    (vm-visit-folder (file-name-sans-extension file))
    (should (eq vm-folder-type 'From_))
    (should (= (length vm-message-list) 1))))

(ert-deftest vm-folder-test-conversion-keeps-the-old-file-when-told-to ()
  "Answering no to the offer keeps the old file, and VM is on the new one.
Two folders holding the same mail is the reader's business -- the old name may
be where mail is delivered -- but the folder VM is looking at is the new one."
  (vm-folder-test--converting (file "inbox" vm-folder-test--743-message nil)
    (vm-folder-test--convert 'mboxcl2)
    (should (equal (buffer-file-name) (concat file ".mboxcl2")))
    (should (file-exists-p file))
    (should (file-exists-p (concat file ".mboxcl2")))))

(ert-deftest vm-folder-test-conversion-keeps-a-backup-even-when-purging ()
  "The old file is backed up before it is offered for deletion.
It looks like backup enough while it is there, which is exactly why deleting
it would otherwise leave no previous copy at all."
  (vm-folder-test--converting (file "inbox" vm-folder-test--743-message t)
    (vm-folder-test--convert 'mboxcl2)
    (should-not (file-exists-p file))
    (should (file-exists-p (vm-folder-backup-name file)))))

(ert-deftest vm-folder-test-conversion-will-not-overwrite-a-folder ()
  "The name the new type asks for is refused when a folder already has it.
Overwriting one folder with another is not something to ask about."
  (vm-folder-test--converting (file "inbox" vm-folder-test--743-message t)
    (write-region "not a folder VM wrote\n" nil (concat file ".mboxcl2") nil 'quiet)
    (let ((text-quoting-style 'grave))
      (should (string-match-p "is a folder already"
                              (cadr (should-error
                                     (vm-folder-test--convert 'mboxcl2))))))
    ;; and nothing was done: the folder is what it was
    (should (eq vm-folder-type 'From_))))

(ert-deftest vm-folder-test-a-conversion-with-nobody-to-ask-keeps-both ()
  "With no reader to answer, the old file stays.  A file is not deleted on a
guess, and a batch caller has nobody to put the question to."
  (vm-folder-test-with-file (file "inbox" vm-folder-test--743-message)
    (cl-letf (((symbol-function 'vm-warn) #'ignore))
      (vm-visit-folder file)
      (vm-change-folder-type 'mboxcl2)
      (should (equal (buffer-file-name) (concat file ".mboxcl2")))
      (should (file-exists-p file)))))

(ert-deftest vm-folder-test-a-name-that-already-fits-is-left-alone ()
  "Converting mboxcl2 to mboxcl2 is the repair for wrong lengths, and the name
already states the type, so nothing is renamed and nothing is offered."
  (let ((asked 0))
    (vm-folder-test-with-file (file "sent.mboxcl2"
                                    (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                            "From: alice@example.com\nSubject: one\n"
                                            "Content-Length: 6\n\nBody.\n\n"))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq asked (1+ asked)) t)))
        (vm-visit-folder file)
        (vm-folder-test--convert 'mboxcl2)
        (should (equal (buffer-file-name) file))
        (should (equal asked 0))))))

(ert-deftest vm-folder-test-the-name-a-type-asks-for ()
  "`vm-folder-name-for-type' replaces an extension that states a type, keeps
one that does not, and answers the name itself for a type no extension names."
  (let ((vm-folder-type-by-extension-alist '(("mboxcl2" . mboxcl2))))
    (should (equal (vm-folder-name-for-type "/m/inbox" 'mboxcl2)
                   "/m/inbox.mboxcl2"))
    (should (equal (vm-folder-name-for-type "/m/sent.mboxcl2" 'From_)
                   "/m/sent"))
    (should (equal (vm-folder-name-for-type "/m/sent.mboxcl2" 'mboxcl2)
                   "/m/sent.mboxcl2"))
    ;; an extension VM does not know is part of the name
    (should (equal (vm-folder-name-for-type "/m/notes.txt" 'mboxcl2)
                   "/m/notes.txt.mboxcl2"))
    ;; the old name for mboxcl2 reaches the same answer
    (should (equal (vm-folder-name-for-type "/m/inbox" 'From_-with-Content-Length)
                   "/m/inbox.mboxcl2"))
    ;; no extension names babyl, so the name stands
    (should (equal (vm-folder-name-for-type "/m/inbox" 'babyl) "/m/inbox")))
  ;; and with the option emptied, nothing is renamed at all
  (let ((vm-folder-type-by-extension-alist nil))
    (should (equal (vm-folder-name-for-type "/m/inbox" 'mboxcl2) "/m/inbox"))))


;;; vm-check-folder: what the folder is, and whether it is sound

(defconst vm-folder-test--check-body "Body line.\n"
  "The body of the messages the vm-check-folder tests use.")

(defun vm-folder-test--mboxcl2-message (&optional length)
  "One mboxcl2 message, its Content-Length LENGTH or the right one."
  (format (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                  "From: alice@example.com\nSubject: one\n"
                  "Content-Length: %d\n\n%s\n")
          (or length (length vm-folder-test--check-body))
          vm-folder-test--check-body))

(defmacro vm-folder-test--checking (spec &rest body)
  "Visit the folder SPEC names, then run BODY with the check's output bound.
SPEC is (VAR NAME CONTENT).  BODY sees `said', what the echo area was told,
and `report', the text of the report buffer or nil where there was none."
  (declare (indent 1) (debug t))
  (let ((var (nth 0 spec)) (name (nth 1 spec)) (content (nth 2 spec)))
    `(vm-folder-test-with-file (,var ,name ,content)
       (let ((vm-mboxcl2-strict nil))
         (cl-letf (((symbol-function 'vm-warn) #'ignore))
           (vm-visit-folder ,var)))
       (let (said report)
         (cl-letf (((symbol-function 'vm-inform)
                    (lambda (_level format &rest args)
                      (setq said (apply #'format format args)))))
           (vm-check-folder))
         (when (get-buffer "*VM folder check*")
           (with-current-buffer "*VM folder check*"
             (setq report (buffer-string)))
           (kill-buffer "*VM folder check*"))
         ,@body))))

(ert-deftest vm-folder-test-check-folder-passes-a-sound-folder ()
  "A folder whose lengths are right is one line in the echo area, no buffer.
It has to be quiet when there is nothing to say, or nobody will run it."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should-not report)
    (should (string-match-p "mboxcl2" said))
    (should (string-match-p "2 messages" said))
    (should (string-match-p "sound" said))))

(ert-deftest vm-folder-test-check-folder-finds-a-wrong-length ()
  "A length that does not describe the message is named, with both numbers.
This is the fault the command exists for: the folder opens, because the reader
falls back on searching for the next separator, so nothing else says so."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message 999)
                                          (vm-folder-test--mboxcl2-message)))
    (should report)
    (should (string-match-p "message 1 says Content-Length 999" report))
    (should (string-match-p "vm-change-folder-type mboxcl2" report))
    ;; and the sound one is not complained about
    (should-not (string-match-p "message 2" report))))

(ert-deftest vm-folder-test-check-folder-finds-a-missing-length ()
  "A message with no Content-Length in an mboxcl2 folder is named too.
Visiting such a folder is refused unless `vm-mboxcl2-strict' is nil, so this
is what a reader sees after turning that off to get in."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (should report)
    (should (string-match-p "message 1 has no Content-Length" report))))

(ert-deftest vm-folder-test-check-folder-allows-an-uncounted-newline ()
  "A length short by the newlines at the end of the body is accepted.
`vm-find-trailing-message-separator' skips any number of them past the count,
because some mailers do not count the last one, so the check must not call
what the reader accepts a fault."
  (let ((short (1- (length vm-folder-test--check-body))))
    (vm-folder-test--checking (file "sent.mboxcl2"
                                    (vm-folder-test--mboxcl2-message short))
      (should-not report)
      (should (string-match-p "sound" said)))))

(ert-deftest vm-folder-test-check-folder-has-nothing-to-check-in-From_ ()
  "A From_ folder carries no lengths, so there is nothing to be wrong.
The type is still reported, which is half of what the command is for."
  (vm-folder-test--checking (file "plain"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (should-not report)
    (should (string-match-p "From_" said))))

(ert-deftest vm-folder-test-check-folder-reports-the-type-and-the-name ()
  "The report says what the folder is, what its name says, and the default,
since a folder read as something its name does not claim is the thing a
reader is trying to find out about."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (vm-folder-test--mboxcl2-message 999))
    (should (string-match-p "Type: *mboxcl2" report))
    (should (string-match-p "The name says: *mboxcl2" report))
    (should (string-match-p "The default is From_" report))))

(ert-deftest vm-folder-test-check-folder-writes-nothing ()
  "The command reports and does not repair: the folder on disk is untouched
and the buffer is not modified, so running it is never a decision."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  (vm-folder-test--mboxcl2-message 999))
    (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string)))
          (stamp (file-attribute-modification-time (file-attributes file))))
      (let ((vm-mboxcl2-strict nil))
        (cl-letf (((symbol-function 'vm-warn) #'ignore))
          (vm-visit-folder file)))
      (cl-letf (((symbol-function 'vm-inform) #'ignore))
        (vm-check-folder))
      (when (get-buffer "*VM folder check*") (kill-buffer "*VM folder check*"))
      (should-not (buffer-modified-p))
      (should (equal before (with-temp-buffer (insert-file-contents file)
                                             (buffer-string))))
      (should (equal stamp (file-attribute-modification-time
                            (file-attributes file)))))))

(defun vm-folder-test--From_-message-with-a-length ()
  "One message carrying a Content-Length that fits, under no particular name."
  (vm-folder-test--mboxcl2-message))

(ert-deftest vm-folder-test-check-folder-reads-the-contents-not-the-name ()
  "A folder carrying a fitting length on every message is reported as mboxcl2
whatever it is called.  The reader takes the type from the name, so this is
the disagreement that leaves a folder read as a type it is not -- and the
check is the only thing that says so."
  (vm-folder-test--checking (file "imap-cache-d0c3b3a9"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should report)
    (should (string-match-p "Type: *From_" report))
    (should (string-match-p "The name says: *nothing" report))
    (should (string-match-p "The contents say: *mboxcl2" report))
    (should (string-match-p "every one of 2 messages" report))))

(ert-deftest vm-folder-test-check-folder-names-the-rename-and-the-conversion ()
  "The report says what to do about it: rename the file, or convert it.
Which is what the warning at visit time leaves to the reader without saying
how to decide."
  (vm-folder-test--checking (file "imap-cache-d0c3b3a9"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should (string-match-p "imap-cache-d0c3b3a9\\.mboxcl2" report))
    (should (string-match-p "vm-change-folder-type" report))))

(ert-deftest vm-folder-test-check-folder-does-not-cry-mboxcl2-over-a-few ()
  "A From_ folder where only some messages carry a length is left alone.
Mail arrives carrying the header and VM gives one to every message it
rewrites, so a few in a folder are no evidence at all -- and a check that
said mboxcl2 on those would be wrong about most From_ folders."
  (vm-folder-test--checking (file "plain"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          "From bob@example.com Sat Aug  8 14:25:13 2026\n"
                                          "From: bob@example.com\nSubject: two\n\nBody.\n\n"))
    (should-not report)
    (should (string-match-p "sound" said))))

(ert-deftest vm-folder-test-check-folder-counts-the-lengths-it-found ()
  "The counts are reported where they are not conclusive, since a folder with
most of a length on most of its messages is the case a reader has to judge."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message 999)))
    (should report)
    (should (string-match-p "2 of 2 messages carry a length, 1 of those fit"
                            report))))

(ert-deftest vm-folder-test-check-folder-says-nothing-carries-a-length ()
  "A From_ folder with no lengths at all says so, rather than saying nothing."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                          "From: alice@example.com\nSubject: one\n\nBody.\n\n"))
    (should report)
    (should (string-match-p "no message carries a length" report))))

(ert-deftest vm-folder-test-check-folder-leaves-a-named-mboxcl2-alone ()
  "A folder named mboxcl2 and read as mboxcl2 is not told its contents agree
with its name: there is no disagreement to report, so it stays one line."
  (vm-folder-test--checking (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should-not report)
    (should (string-match-p "sound" said))))

(defconst vm-folder-test--plain-From_-message
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: alice@example.com\nSubject: one\n\nBody.\n\n")
  "One From_ message carrying no Content-Length, as an older cache holds them.")

(ert-deftest vm-folder-test-check-folder-names-the-cache-conversion ()
  "A cache whose name states no type is reported, and the command named.
Nothing in such a cache is wrong -- read as From_ it is From_ -- so no other
part of VM says so: the visit-time warning fires only for a cache that carries
lengths.  This is the one place a reader is told, which is what #768 asked."
  (vm-folder-test--checking (file "imap-cache-0123456789abcdef"
                                  (concat vm-folder-test--plain-From_-message
                                          vm-folder-test--plain-From_-message))
    (should report)
    (should (string-match-p "vm-convert-caches-to-mboxcl2" report))
    (should (string-match-p "before VM named its caches" report))
    ;; and it is not called a fault: the lengths are not complained about,
    ;; because there are none to be wrong
    (should-not (string-match-p "Content-Length" report))))

(ert-deftest vm-folder-test-check-folder-suggests-nothing-for-an-ordinary-folder ()
  "A sound folder that is not a cache stays one line in the echo area.
The suggestion is for a cache and nothing else: a From_ folder of the reader's
own is From_ because they named it that, and telling them to convert it would
be wrong."
  (vm-folder-test--checking (file "plain"
                                  (concat vm-folder-test--plain-From_-message
                                          vm-folder-test--plain-From_-message))
    (should-not report)
    (should (string-match-p "sound" said))))

(ert-deftest vm-folder-test-check-folder-leaves-a-named-cache-alone ()
  "A cache that already names its type is not suggested for conversion.
That is every cache VM creates now, so suggesting it would mean the report
never stops asking for something already done."
  (vm-folder-test--checking (file "imap-cache-0123456789abcdef.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should-not report)
    (should (string-match-p "sound" said))))

(ert-deftest vm-folder-test-check-folder-says-both-of-a-cache-that-is-mboxcl2 ()
  "A cache carrying lengths under a name that says nothing gets both findings.
What the contents turned out to be, then what converts it: the rename advice
and the cache advice are about the same file and neither one says the whole of
it."
  (vm-folder-test--checking (file "imap-cache-0123456789abcdef"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (should report)
    (should (string-match-p "The contents say mboxcl2 and the name does not"
                            report))
    (should (string-match-p "vm-convert-caches-to-mboxcl2" report))
    ;; the finding before what to do about it
    (should (< (string-match "The contents say mboxcl2 and the name does not"
                             report)
               (string-match "vm-convert-caches-to-mboxcl2" report)))))

;;; Saying what a conversion is doing

(defmacro vm-folder-test--saying (&rest body)
  "Run BODY with what `vm-inform' was told bound to `said', newest last.
The progress interval is 1, so every message counts: the tests have three
messages to work with, not the thousands a folder that needs reporting on has."
  (declare (indent 0) (debug t))
  `(let ((said nil)
         (vm-folder-progress-interval 1))
     (cl-letf (((symbol-function 'vm-inform)
                (lambda (_level format &rest args)
                  (setq said (append said (list (apply #'format format args)))))))
       ,@body)
     said))

(defun vm-folder-test--said-p (said pattern)
  "Whether any line of SAID matches PATTERN."
  (and (seq-find (lambda (line) (string-match-p pattern line)) said) t))

(ert-deftest vm-folder-test-progress-says-at-the-interval-and-not-between ()
  "`vm-folder-say-progress' speaks every `vm-folder-progress-interval'.
A message per message would be the echo area doing more work than the job it
is reporting on."
  (let ((said nil)
        (vm-folder-progress-interval 3))
    (cl-letf (((symbol-function 'vm-inform)
               (lambda (_level format &rest args)
                 (push (apply #'format format args) said))))
      (dolist (n '(1 2 3 4 5 6))
        (vm-folder-say-progress "Converting" n 6)))
    (should (equal (nreverse said)
                   '("Converting... 3 of 6" "Converting... 6 of 6")))))

(ert-deftest vm-folder-test-progress-without-a-total-says-the-count ()
  "A phase whose total is not yet known says how far it has got and no more.
A folder has to be walked before it can be counted, and the walk is itself the
slow part."
  (let ((said nil)
        (vm-folder-progress-interval 1))
    (cl-letf (((symbol-function 'vm-inform)
               (lambda (_level format &rest args)
                 (push (apply #'format format args) said))))
      (vm-folder-say-progress "Finding the messages" 7))
    (should (equal said '("Finding the messages... 7")))))

(ert-deftest vm-folder-test-converting-a-buffer-counts-against-the-total ()
  "`vm-convert-folder-type' says which message of how many it is on.
It said nothing at all, and it is where the on-disk repair of a gigabyte
folder spends its time."
  (let ((said (vm-folder-test--saying
                (with-temp-buffer
                  (insert (vm-folder-test--mboxcl2-message)
                          (vm-folder-test--mboxcl2-message)
                          (vm-folder-test--mboxcl2-message))
                  (let ((vm-mboxcl2-strict nil))
                    (vm-convert-folder-type 'mboxcl2 'From_))))))
    (should (vm-folder-test--said-p said "Finding the messages\\.\\.\\. 3"))
    (should (vm-folder-test--said-p said "Converting\\.\\.\\. 3 of 3"))
    (should (vm-folder-test--said-p said "3 messages, done"))))

(ert-deftest vm-folder-test-the-on-disk-conversion-names-every-phase ()
  "Each phase that walks or copies the whole folder says it is starting.
On a gigabyte cache each is a minute of silence, and six of them in a row is
not distinguishable from a hung Emacs."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (let ((said (vm-folder-test--saying
                  (vm-change-folder-type-of-file file 'From_ nil))))
      (should (vm-folder-test--said-p said "^Reading sent\\.mboxcl2\\.\\.\\."))
      (should (vm-folder-test--said-p said "bytes, checksumming"))
      (should (vm-folder-test--said-p said "^Counting the messages in sent"))
      (should (vm-folder-test--said-p said "^Converting sent.* from mboxcl2 to From_, 2 messages"))
      (should (vm-folder-test--said-p said "^Checking that the result reads back as From_"))
      (should (vm-folder-test--said-p said "^Backing sent\\.mboxcl2 up as"))
      (should (vm-folder-test--said-p said "^Writing sent")))))

(defconst vm-folder-test--message-with-no-length
  (concat "From bob@example.com Sat Aug  8 15:24:13 2026\n"
          "From: bob@example.com\nSubject: two\n\nNo length here.\n\n")
  "A message with no Content-Length, which an mboxcl2 folder must not have.")

(defun vm-folder-test--check-file (file)
  "Check FILE on disk and answer the text of the report, or nil if there was none."
  (cl-letf (((symbol-function 'vm-inform) #'ignore)
            ((symbol-function 'vm-warn) #'ignore))
    (vm-check-folder file))
  (when (get-buffer "*VM folder check*")
    (let ((report (with-current-buffer "*VM folder check*" (buffer-string))))
      (kill-buffer "*VM folder check*")
      report)))

(ert-deftest vm-folder-test-check-folder-checks-a-file-vm-will-not-visit ()
  "A folder on disk is checked without being visited, which is the whole point.
An mboxcl2 folder with a message that has no length cannot be visited at all,
so there is no buffer in which to check the folder that most wants checking."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          vm-folder-test--message-with-no-length
                                          (vm-folder-test--mboxcl2-message)))
    (let ((report (vm-folder-test--check-file file)))
      (should report)
      (should (string-match-p "Type: *mboxcl2" report))
      (should (string-match-p "message 2 has no Content-Length" report))
      ;; and it is not visited: no folder buffer is left holding the file
      (should-not (get-file-buffer file)))))

(ert-deftest vm-folder-test-check-folder-of-a-file-reads-the-contents ()
  "The contents are counted for a file too, so a misnamed cache can be
settled without visiting it -- the case the visit-time warning leaves open."
  (vm-folder-test-with-file (file "imap-cache-d0c3b3a9"
                                  (concat (vm-folder-test--mboxcl2-message)
                                          (vm-folder-test--mboxcl2-message)))
    (let ((report (vm-folder-test--check-file file)))
      (should (string-match-p "Type: *From_" report))
      (should (string-match-p "The name says: *nothing" report))
      (should (string-match-p "The contents say: *mboxcl2" report)))))

(ert-deftest vm-folder-test-check-folder-of-a-file-writes-nothing ()
  "It reports and does not repair: the file on disk is untouched, and no
buffer is left behind to be saved over it."
  (vm-folder-test-with-file (file "sent.mboxcl2"
                                  (concat (vm-folder-test--mboxcl2-message 999)
                                          (vm-folder-test--mboxcl2-message)))
    (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string)))
          (stamp (file-attribute-modification-time (file-attributes file)))
          (buffers (buffer-list)))
      (vm-folder-test--check-file file)
      (should (equal before (with-temp-buffer (insert-file-contents file)
                                             (buffer-string))))
      (should (equal stamp (file-attribute-modification-time
                            (file-attributes file))))
      (should (equal buffers (buffer-list))))))

(ert-deftest vm-folder-test-check-folder-of-a-file-refuses-an-unknown-type ()
  "A file that is no folder VM knows is refused, and says so of the file
rather than of a buffer nobody asked about."
  (vm-folder-test-with-file (file "notes.txt" "This is not a folder at all.\n")
    (let* ((text-quoting-style 'grave)
           (err (should-error (vm-check-folder file) :type 'error)))
      (should (string-match-p "notes\\.txt has no folder type" (cadr err))))))

(ert-deftest vm-folder-test-a-name-can-say-plain-mbox ()
  "A folder named .mbox is From_, whatever `vm-default-folder-type' says.
That is what everything outside VM means by an mbox file, and a folder that
does not exist yet has nowhere else to have said it."
  (let ((vm-default-folder-type 'mboxcl2))
    (should (eq (vm-folder-type-for-name "/mail/sent.mbox") 'From_))
    (should (eq (vm-folder-type-to-write "/mail/sent.mbox") 'From_))
    ;; and the default still decides a name that says nothing
    (should (eq (vm-folder-type-to-write "/mail/INBOX") 'mboxcl2))))

(ert-deftest vm-folder-test-From_-is-never-imposed-on-a-name ()
  "Converting to From_ does not put .mbox on a folder that has not got it.
From_ is the type a folder has when its name says nothing about it, so it is
not a type a name has to state -- and renaming INBOX to INBOX.mbox would take
a `vm-primary-inbox' setting with it."
  (should-not (vm-folder-extension-for-type 'From_))
  (should (equal (vm-folder-name-for-type "/mail/INBOX" 'From_) "/mail/INBOX"))
  (should (equal (vm-folder-name-for-type "/mail/notes.txt" 'From_)
                 "/mail/notes.txt"))
  ;; the extension that does state a type is still replaced
  (should (equal (vm-folder-name-for-type "/mail/sent.mboxcl2" 'From_)
                 "/mail/sent"))
  (should (equal (vm-folder-extension-for-type 'mboxcl2) "mboxcl2")))

(ert-deftest vm-folder-test-a-name-that-says-the-type-is-left-alone ()
  "A folder the reader called sent.mbox is still sent.mbox after a conversion
to From_, rather than losing the extension it was given."
  (should (equal (vm-folder-name-for-type "/mail/sent.mbox" 'From_)
                 "/mail/sent.mbox"))
  (should (equal (vm-folder-name-for-type "/mail/sent.mboxcl2" 'mboxcl2)
                 "/mail/sent.mboxcl2"))
  ;; and the old name for the type counts as the type
  (should (equal (vm-folder-name-for-type "/mail/sent.mboxcl2"
                                          'From_-with-Content-Length)
                 "/mail/sent.mboxcl2"))
  ;; converting the other way still renames
  (should (equal (vm-folder-name-for-type "/mail/sent.mbox" 'mboxcl2)
                 "/mail/sent.mboxcl2")))

;;; Fetching a message the folder was once given and no longer holds (#751)
;;
;; `vm-imap-get-synchronization-data' fetches such a message only when it is
;; asked for `full', and until now nothing asked: every caller passed t or
;; nil, so the branch was dead and a folder whose cache had lost messages
;; could not be refilled.

(defun vm-folder-test--retrieves-asked-for (full)
  "What `vm-get-spooled-mail' asks an IMAP folder for, given FULL.
The driver is the only path, so it is what is measured (emacs-vm/vm#822)."
  (let (asked)
    (cl-letf (((symbol-function 'vm-imap-net-get-spooled-mail)
               (lambda (&optional _interactive full) (setq asked full) t)))
      (vm-get-spooled-mail nil full))
    asked))

(ert-deftest vm-folder-test-a-full-fetch-asks-for-the-messages-already-recorded ()
  "`vm-get-spooled-mail' hands FULL on when it is asked to, and nil otherwise.
FULL is what reaches the branch in `vm-imap-get-synchronization-data' that
fetches a message the folder was given once and no longer holds, rather than
passing it over.

`vm-imap-sync-on-get' no longer decides anything here: it chose between two
shapes of blocking synchronisation, and there is one path now."
  (vm-test-with-folder (vm-folder-test--write-folder-content 1)
    (setq vm-folder-access-method 'imap)
    (let ((vm-block-new-mail nil))
      (should-not (vm-folder-test--retrieves-asked-for nil))
      (should (eq (vm-folder-test--retrieves-asked-for t) t)))))

(defmacro vm-folder-test--getting-new-mail (spec &rest body)
  "Run BODY in a folder with `vm-get-spooled-mail' recording how it was asked.
SPEC is (ASKED-VAR), bound to a function of no arguments answering with the
list of FULL arguments the calls were given, newest last."
  (declare (indent 1) (debug t))
  `(vm-folder-test--with-state-folder
     (let ((calls nil))
       (cl-letf (((symbol-function 'vm-get-spooled-mail)
                  (lambda (&optional _interactive full)
                    (setq calls (append calls (list full)))
                    nil)))
         (cl-flet ((,(car spec) () calls))
           ,@body)))))

(ert-deftest vm-folder-test-two-prefix-arguments-fetch-what-was-retrieved-before ()
  "`C-u C-u M-x vm-get-new-mail' asks for a full fetch, and a plain call does
not.  One prefix argument still means gathering from a folder the reader
names, so the second is what says this."
  (vm-folder-test--getting-new-mail (asked)
    (vm-get-new-mail nil)
    (should (equal (asked) '(nil)))
    (vm-get-new-mail '(16))
    (should (equal (asked) '(nil t)))))

(ert-deftest vm-folder-test-one-prefix-argument-still-gathers-from-a-folder ()
  "The single prefix argument is untouched: it reads a folder name and
gathers from it rather than fetching anything from a spool file."
  (vm-folder-test--getting-new-mail (asked)
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) (error "asked for a folder to gather from"))))
      (let ((err (should-error (vm-get-new-mail '(4)) :type 'error)))
        (should (string-match-p "gather from" (error-message-string err)))))
    (should (equal (asked) nil))))


;;; Converting into a file of the reader's naming (emacs-vm/vm#763)

(ert-deftest vm-folder-test-on-disk-conversion-can-write-elsewhere ()
  "With an output file the conversion is written there and the folder
converted is left exactly as it was: no backup, since nothing is overwritten,
and nothing to delete afterwards."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((output (expand-file-name "repaired.mboxcl2"
                                    (file-name-directory file))))
      (vm-change-folder-type-of-file file 'mboxcl2 nil output)
      (should (file-exists-p output))
      ;; the input is byte for byte what it was, and has no backup
      (should (equal (with-temp-buffer (insert-file-contents file)
                                       (buffer-string))
                     vm-folder-test--seven-and-two-short))
      (should-not (file-exists-p (vm-folder-backup-name file)))
      ;; and the output is a folder VM reads as what was asked for
      (vm-visit-folder output)
      (should (= (length vm-message-list) 3))
      (should (eq vm-folder-type 'mboxcl2)))))

(ert-deftest vm-folder-test-on-disk-conversion-writes-a-sound-folder-elsewhere ()
  "A folder already sound is still written to the output.  In place there is
nothing to do and the file is left untouched; asked for a copy under another
name, answering that there was nothing to do would leave the reader without
one."
  (vm-folder-test-with-file (file "sound.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (vm-change-folder-type-of-file file 'mboxcl2)
    (let ((output (expand-file-name "copy.mboxcl2" (file-name-directory file))))
      (vm-change-folder-type-of-file file 'mboxcl2 nil output)
      (should (file-exists-p output))
      (should (equal (with-temp-buffer (insert-file-contents output)
                                       (buffer-string))
                     (with-temp-buffer (insert-file-contents file)
                                       (buffer-string)))))))

(ert-deftest vm-folder-test-an-output-naming-the-folder-itself-is-in-place ()
  "An output naming the folder being converted is the in-place conversion said
another way, backup and all, rather than a refusal about a file that exists."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (vm-change-folder-type-of-file file 'mboxcl2 nil file)
    (should (file-exists-p (vm-folder-backup-name file)))
    (vm-visit-folder file)
    (should (= (length vm-message-list) 3))))

(ert-deftest vm-folder-test-an-output-that-exists-is-refused ()
  "An output file that exists is not overwritten: it may be a folder."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((output (expand-file-name "taken.mboxcl2"
                                    (file-name-directory file)))
          (text-quoting-style 'grave))
      (write-region "something already here\n" nil output nil 'quiet)
      (should (string-match-p
               "exists already"
               (cadr (should-error
                      (vm-change-folder-type-of-file file 'mboxcl2 nil output)))))
      ;; and it is still what it was
      (should (equal (with-temp-buffer (insert-file-contents output)
                                       (buffer-string))
                     "something already here\n")))))

(ert-deftest vm-folder-test-an-output-name-that-cannot-hold-the-type-is-refused ()
  "REGRESSION: mboxcl2 is not written under a name that says something else,
nor under one that says nothing.

Issue #763.  The name is where a folder's type is stated, so an mboxcl2 folder
called out.mbox is one VM reads as From_ and splits wherever a body line
begins `From ', and one called plain-name is the same.  The conversion says
what to call it instead, and writes nothing."
  (vm-folder-test-with-file (file "broken.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((dir (file-name-directory file))
          (text-quoting-style 'grave))
      ;; a name that states another type
      (let* ((wrong (expand-file-name "out.mbox" dir))
             (message (cadr (should-error
                             (vm-change-folder-type-of-file
                              file 'mboxcl2 nil wrong)))))
        (should (string-match-p "says it is From_" message))
        (should (string-match-p "out.mboxcl2" message))
        (should-not (file-exists-p wrong)))
      ;; a name that states nothing
      (let* ((bare (expand-file-name "out" dir))
             (message (cadr (should-error
                             (vm-change-folder-type-of-file
                              file 'mboxcl2 nil bare)))))
        (should (string-match-p "has to say so in its name" message))
        (should (string-match-p "out.mboxcl2" message))
        (should-not (file-exists-p bare))))))

(ert-deftest vm-folder-test-an-output-name-for-a-nameless-type-is-taken-as-it-is ()
  "From_ asks nothing of a name, being the type a folder has when its name
says nothing, so an output called anything at all holds it -- and a name that
says mboxcl2 still does not."
  (vm-folder-test-with-file (file "folder.mboxcl2"
                                  vm-folder-test--seven-and-two-short)
    (let ((dir (file-name-directory file))
          (text-quoting-style 'grave))
      (vm-change-folder-type-of-file file 'mboxcl2) ; sound first
      (let ((plain (expand-file-name "plain" dir)))
        (vm-change-folder-type-of-file file 'From_ nil plain)
        (should (file-exists-p plain))
        (vm-visit-folder plain)
        (should (eq vm-folder-type 'From_))
        (should (= (length vm-message-list) 3)))
      (should (string-match-p
               "says it is mboxcl2"
               (cadr (should-error
                      (vm-change-folder-type-of-file
                       file 'From_ nil (expand-file-name "no.mboxcl2" dir)))))))))

(ert-deftest vm-folder-test-writing-elsewhere-needs-a-folder-on-disk ()
  "Asked to write the conversion elsewhere with no file to convert, the
command says to save the folder and convert that, rather than converting the
buffer and writing it who knows where."
  (let ((text-quoting-style 'grave))
    (should (string-match-p
             "needs a folder on disk"
             (cadr (should-error
                    (vm-change-folder-type 'mboxcl2 nil "/tmp/somewhere")))))))

(ert-deftest vm-folder-test-two-prefix-arguments-ask-where-to-write ()
  "C-u C-u M-x vm-change-folder-type asks for the folder and then for where to
write the conversion; one prefix argument asks only for the folder."
  (let* ((asked nil)
         (args
          (cl-letf (((symbol-function 'vm-read-file-name)
                     (lambda (prompt &rest _)
                       (push prompt asked)
                       (if (string-match-p "Write" prompt)
                           "/tmp/out.mboxcl2"
                         "/tmp/in.mboxcl2")))
                    ((symbol-function 'vm-read-string)
                     (lambda (&rest _) "mboxcl2")))
            (let ((current-prefix-arg '(16)))
              (eval (cadr (interactive-form 'vm-change-folder-type)) t)))))
    (should (equal (nth 1 args) "/tmp/in.mboxcl2"))
    (should (equal (nth 2 args) "/tmp/out.mboxcl2"))
    (should (= (length asked) 2)))
  (let* ((asked nil)
         (args
          (cl-letf (((symbol-function 'vm-read-file-name)
                     (lambda (prompt &rest _)
                       (push prompt asked)
                       "/tmp/in.mboxcl2"))
                    ((symbol-function 'vm-read-string)
                     (lambda (&rest _) "mboxcl2")))
            (let ((current-prefix-arg '(4)))
              (eval (cadr (interactive-form 'vm-change-folder-type)) t)))))
    (should (equal (nth 1 args) "/tmp/in.mboxcl2"))
    (should-not (nth 2 args))
    (should (= (length asked) 1))))

;;; Attributes across a save and a re-read

;; A label has `vm-label-test-labels-survive-saving-and-reading'.  The
;; attributes had nothing: 171 calls to the setters across the suite and not
;; one that saved the folder and read it again.  They live in the same
;; X-VM-v5-Data header, and losing one means a message you deleted coming
;; back, or one you have read coming back unread.

(defconst vm-folder-test--attribute-setters
  '((deleted       . vm-set-deleted-flag)
    (filed         . vm-set-filed-flag)
    (replied       . vm-set-replied-flag)
    (written       . vm-set-written-flag)
    (forwarded     . vm-set-forwarded-flag)
    (redistributed . vm-set-redistributed-flag)
    (flagged       . vm-set-flagged-flag))
  "Each attribute VM keeps for a message, and the function that sets it.
`new' and `unread' are left out: they are turned off by reading a message,
which the act of visiting the folder does.")

(defun vm-folder-test--attribute-of (name m)
  "Whether M carries the attribute NAME."
  (and (funcall (intern (format "vm-%s-flag" name)) m) t))

(ert-deftest vm-folder-test-attributes-survive-saving-and-reading ()
  "Every attribute set on a message is still set next time the folder is read.
One attribute per message, so that a header holding the wrong one is caught
rather than hidden by all of them agreeing."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-attrs" t)))
         (file (expand-file-name "folder" dir))
         (names (mapcar #'car vm-folder-test--attribute-setters))
         (before (buffer-list)))
    (unwind-protect
        (let ((vm-frame-per-folder nil)
              (vm-mutable-frame-configuration nil)
              (vm-summary-show-threads nil))
          (vm-test-write-simple-folder file (length names))
          (cl-letf (((symbol-function 'vm-display) #'ignore))
            (vm-visit-folder file)
            ;; one attribute per message, in order
            (let ((mp vm-message-list)
                  (setters vm-folder-test--attribute-setters))
              (while (and mp setters)
                (funcall (cdr (car setters)) (car mp) t)
                (setq mp (cdr mp) setters (cdr setters))))
            (vm-save-folder))
          ;; read it again in a buffer of its own
          (let ((again (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer again
                  (cl-letf (((symbol-function 'vm-display) #'ignore))
                    (vm-mode))
                  (should (= (length names) (length vm-message-list)))
                  (let ((mp vm-message-list)
                        (wanted names)
                        wrong)
                    (while (and mp wanted)
                      ;; the one that was set is set
                      (unless (vm-folder-test--attribute-of (car wanted) (car mp))
                        (push (format "message %s lost %s"
                                      (vm-number-of (car mp)) (car wanted))
                              wrong))
                      ;; and none of the others is
                      (dolist (other names)
                        (unless (eq other (car wanted))
                          (when (vm-folder-test--attribute-of other (car mp))
                            (push (format "message %s gained %s"
                                          (vm-number-of (car mp)) other)
                                  wrong))))
                      (setq mp (cdr mp) wanted (cdr wanted)))
                    (should (equal nil (nreverse wrong)))))
              (with-current-buffer again (set-buffer-modified-p nil))
              (kill-buffer again))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; What the manual promises about a message making the trip more than once

;; The mbox section of the manual states this, having been asked to say what
;; the quoting costs rather than change it (emacs-vm/vm#789).
;;
;; vm-folder-test-only-a-from-line-ending-in-a-digit-is-quoted above already
;; holds the narrow rule and the already-quoted line, through a real
;; conversion.  What nothing held is the promise that follows from them: that
;; the cost is one `>' per line for good, and not one per trip.

(ert-deftest vm-folder-test-munging-is-idempotent ()
  "Quoting a line that is already quoted changes nothing."
  (let ((line "From bob@example.com Mon Jan  1 00:00:00 2024\n"))
    (dolist (times '(1 2 3))
      (with-temp-buffer
        (insert line)
        (dotimes (_ times)
          (vm-munge-message-separators 'From_ (point-min) (point-max)))
        (should (equal (buffer-string) (concat ">" line)))))))

(ert-deftest vm-folder-test-a-second-trip-through-From_-adds-no-second-quote ()
  "A message converted to From_ and back twice carries one `>', not two.
The manual promises the loss is one character per quoted line however many
times the message passes through a From_ folder.  Round trips it twice and
compares: the first trip adds the `>', the second changes nothing.

The `>' is never removed, which is what emacs-vm/vm#789 is about and what
`vm-folder-roundtrip-test.el' pins by name.  This is the other half of it:
that it does not accumulate."
  (vm-folder-test-with-directory dir
    (let* ((body (concat "Quoting an old note:\n"
                         "From bob@example.com Mon Jan  1 12:00:00 2026\n"))
           (file (expand-file-name "folder.mboxcl2" dir))
           (message (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                            "Content-Length: "
                            (number-to-string (1+ (length body)))
                            "\nFrom: alice@example.com\nSubject: one\n\n"
                            body "\n"))
           (quotes-in
            (lambda (path)
              (with-temp-buffer
                (insert-file-contents path)
                (goto-char (point-min))
                (let ((n 0))
                  (while (re-search-forward "^>+From bob@example.com" nil t)
                    (setq n (+ n (length (match-string 0))
                               (- (length "From bob@example.com")))))
                  n))))
           (trip
            (lambda (from-file n)
              ;; mboxcl2 to From_ and back, into files of their own so that
              ;; each step is a folder VM reads by its name.
              (let ((out (expand-file-name (format "trip%d" n) dir))
                    (back (expand-file-name (format "back%d.mboxcl2" n) dir)))
                (vm-change-folder-type-of-file from-file 'From_ nil out)
                (vm-change-folder-type-of-file out 'mboxcl2 nil back)
                back))))
      (write-region message nil file nil 'quiet)
      (should (equal 0 (funcall quotes-in file)))
      (let ((after-one (funcall trip file 1)))
        (should (equal 1 (funcall quotes-in after-one)))
        (let ((after-two (funcall trip after-one 2)))
          (should (equal 1 (funcall quotes-in after-two))))))))


;;; The types VM offers to create, and the wider set it reads

;; #787: a folder VM wrote as BellFrom_ is read back as From_ with its
;; messages run together, that format being From_ without the blank line
;; between messages and so having no signature of its own.  The decision was
;; to stop offering it: VM reads one it is handed and creates none.

(ert-deftest vm-folder-test-BellFrom_-is-not-offered-for-creation ()
  "Neither `vm-default-folder-type' nor the conversion prompt offers BellFrom_.
The two places a reader is asked what type to make a folder."
  (should-not (member "BellFrom_" vm-supported-folder-types))
  (should-not (memq 'BellFrom_
                    ;; the symbols the option's :type offers
                    (let ((type (get 'vm-default-folder-type 'custom-type)))
                      (mapcar (lambda (choice)
                                (and (consp choice) (eq (car choice) 'const)
                                     (nth 1 choice)))
                              (cdr type))))))

(ert-deftest vm-folder-test-BellFrom_-is-still-read ()
  "VM still reads a BellFrom_ folder it is handed.
Dropping it from what VM creates must not drop it from what VM understands:
`vm-folder-types' is the wider list, `vm-change-folder-type' still accepts the
symbol, and `vm-default-From_-folder-type' still offers it, that option being
how a reader says which of the two From-style formats their system writes."
  (should (memq 'BellFrom_ vm-folder-types))
  (should (memq 'BellFrom_
                (mapcar (lambda (choice)
                          (and (consp choice) (eq (car choice) 'const)
                               (nth 1 choice)))
                        (cdr (get 'vm-default-From_-folder-type
                                  'custom-type))))))

(ert-deftest vm-folder-test-a-BellFrom_-default-is-warned-about ()
  "VM says so at startup if `vm-default-folder-type' still asks for BellFrom_.
A configuration written before it was withdrawn gets what it asks for, so it
is told what that means rather than finding out from a folder whose messages
have run together."
  (let (warnings)
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _time format &rest args)
                 (push (apply #'format format args) warnings))))
      (let ((vm-default-folder-type 'BellFrom_))
        (vm-check-default-folder-type))
      (should (equal 1 (length warnings)))
      (should (string-match-p "BellFrom_" (car warnings)))
      ;; solution-directed: it says what to set instead
      (should (string-match-p "From_\\|mboxcl2" (car warnings)))
      (setq warnings nil)
      (dolist (type '(From_ mboxcl2 mmdf babyl))
        (let ((vm-default-folder-type type))
          (vm-check-default-folder-type)))
      (should-not warnings))))


;;; BSD Mail(1) Status: headers (`vm-berkeley-mail-compatibility')

;; The option had no test of any kind, and it decides what VM writes into a
;; folder: with it on, a From_ folder gets a `Status:' header per message and
;; any Status already there is removed first.  What is written has to be what
;; is read back, or an attribute is lost every time a folder is saved.
;;
;; These call `vm-stuff-message-data' per message rather than
;; `vm-stuff-folder-data', which stuffs only the messages whose
;; `vm-stuff-flag-of' is set and so writes nothing for a message whose
;; attributes were set with `norecord'.

(defun vm-berkeley-test--folder (n)
  "A From_ folder of N messages, none of them carrying VM's own data."
  (mapconcat
   (lambda (i)
     (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                     "From: alice@example.com\nSubject: subject %d\n\n"
                     "body %d\n\n")
             i i))
   (number-sequence 1 n) ""))

(defun vm-berkeley-test--status-lines (text)
  "The Status: header lines in TEXT."
  (let ((lines nil)
        (start 0))
    (while (string-match "^Status: .*$" text start)
      (push (match-string 0 text) lines)
      (setq start (match-end 0)))
    (nreverse lines)))

(ert-deftest vm-berkeley-test-a-status-header-is-written-only-when-asked ()
  "`vm-berkeley-mail-compatibility' decides whether a Status: header is written.
Off, which is the default away from BSD, nothing of the sort reaches the
folder."
  (dolist (wanted '(t nil))
    (vm-test-with-folder (vm-berkeley-test--folder 2)
      (let ((vm-berkeley-mail-compatibility wanted))
        (dolist (m vm-message-list)
          (vm-set-new-flag m nil 'norecord)
          (vm-set-unread-flag m nil 'norecord))
        (dolist (m vm-message-list) (vm-stuff-message-data m))
        (let ((written (vm-berkeley-test--status-lines
                        (buffer-substring-no-properties (point-min) (point-max)))))
          (if wanted
              (should (equal 2 (length written)))
            (should-not written)))))))

(ert-deftest vm-berkeley-test-the-status-says-whether-the-message-was-read ()
  "A read message is written `Status: RO', an unread one `Status: O'.
That is the whole of what the header carries, and `vm-read-attributes' reads
the R back out of it."
  (vm-test-with-folder (vm-berkeley-test--folder 2)
    (let ((vm-berkeley-mail-compatibility t)
          (read (car vm-message-list))
          (unread (nth 1 vm-message-list)))
      (dolist (m vm-message-list) (vm-set-new-flag m nil 'norecord))
      (vm-set-unread-flag read nil 'norecord)
      (vm-set-unread-flag unread t 'norecord)
      (dolist (m vm-message-list) (vm-stuff-message-data m))
      (let ((written (vm-berkeley-test--status-lines
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (should (equal '("Status: RO" "Status: O") written))))))

(ert-deftest vm-berkeley-test-a-new-message-gets-no-status-header ()
  "Nothing is written for a message still new.
The code writes the header only where the new flag is off, so a folder of
unseen mail is left alone."
  (vm-test-with-folder (vm-berkeley-test--folder 2)
    (let ((vm-berkeley-mail-compatibility t))
      (dolist (m vm-message-list) (vm-set-new-flag m t 'norecord))
      (dolist (m vm-message-list) (vm-stuff-message-data m))
      (should-not (vm-berkeley-test--status-lines
                   (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest vm-berkeley-test-stuffing-twice-leaves-one-status-header ()
  "Saving a folder again does not add a second Status: header.
The writer removes what is there before writing, so the count stays at one
per message however many times the folder is saved.  Without that a folder
would grow a header on every save."
  (vm-test-with-folder (vm-berkeley-test--folder 2)
    (let ((vm-berkeley-mail-compatibility t))
      (dolist (m vm-message-list)
        (vm-set-new-flag m nil 'norecord)
        (vm-set-unread-flag m nil 'norecord))
      (dotimes (_ 3)
        (dolist (m vm-message-list) (vm-stuff-message-data m)))
      (should (equal 2 (length (vm-berkeley-test--status-lines
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))))))

(ert-deftest vm-berkeley-test-only-a-From_-folder-gets-the-header ()
  "The header is written into a From_ folder and no other type.
`Status:' is a From_ mbox convention; an mboxcl2 folder counts its bytes and
an mmdf or babyl folder has its own attribute machinery, so writing one there
would be a header nothing reads."
  (dolist (type '(mboxcl2 mmdf babyl))
    (vm-test-with-folder (vm-berkeley-test--folder 2)
      (let ((vm-berkeley-mail-compatibility t)
            (vm-folder-type type))
        (dolist (m vm-message-list)
          (vm-set-new-flag m nil 'norecord)
          (vm-set-unread-flag m nil 'norecord))
        (dolist (m vm-message-list) (vm-stuff-message-data m))
        (should-not (vm-berkeley-test--status-lines
                     (buffer-substring-no-properties (point-min) (point-max))))))))


;;; Keyword arguments that really are keyword arguments (emacs-vm/vm#795)

(ert-deftest vm-folder-test-retrieve-operable-messages-takes-a-real-keyword ()
  "REGRESSION: `vm-retrieve-operable-messages' has a genuine `:fail' keyword.
It was a plain `defun' with `&key fail' in the arglist, which Emacs Lisp's
lambda list does not understand: `&key' became an ordinary variable of that
name and `fail' one more positional argument.  Every caller writes
`:fail t', so the variable named `&key' swallowed the `:fail' and `t' landed
in `fail' -- right by coincidence.

What that cost: `(f 1 mlist :fail)' with no value answered nil rather than
complaining, a second keyword would have been a wrong-number-of-arguments
error, and edebug could not read vm-folder.el at all, so
test/forms-coverage-report.el was blind to the largest file in the tree.

Asserts the signature rather than the behaviour, there being nothing to
observe from outside: with `cl-defun' there is no variable called `&key', and
the keyword is parsed by cl-lib."
  (let ((arglist (help-function-arglist 'vm-retrieve-operable-messages)))
    ;; no pseudo-variable called `&key'
    (should-not (memq '&key arglist))
    ;; cl-lib collects the keywords into a &rest, which is its signature
    (should (memq '&rest arglist))
    ;; and `fail' is no longer a positional of its own
    (should-not (memq 'fail arglist))))


;;; The message order header (X-VM-Message-Order)

;; The header that records the order a folder's messages are in, written into
;; the first message.  Nothing tested it, and the coverage report put
;; `vm-stuff-message-order' among the definitions with the most forms never
;; evaluated: half of them.  An order header VM cannot read back is a folder
;; that comes back in the wrong order.

(defun vm-order-test--folder (n)
  "A From_ folder of N messages, subjects and bodies numbered."
  (mapconcat
   (lambda (i)
     (format (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                     "From: alice@example.com\nSubject: subject %d\n\n"
                     "body %d\n\n")
             i i))
   (number-sequence 1 n) ""))

(defun vm-order-test--header ()
  "The order header in the current buffer, continuation lines and all.
Nil when there is none."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward vm-message-order-header-regexp nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-line 1)
        (while (looking-at "[ \t]") (forward-line 1))
        (buffer-substring-no-properties start (point))))))

(defun vm-order-test--order-in-header ()
  "The list of numbers the order header holds, read as VM reads it."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward vm-message-order-header-regexp nil t)
      (read (current-buffer)))))

(ert-deftest vm-order-test-a-one-message-folder-gets-no-order-header ()
  "Nothing is written for a folder with one message in it.
`vm-stuff-message-order' begins `(if (cdr vm-message-list)', there being no
order to record when there is nothing to order."
  (vm-test-with-folder (vm-order-test--folder 1)
    (vm-number-messages)
    (vm-stuff-message-order)
    (should-not (vm-order-test--header))))

(ert-deftest vm-order-test-the-header-holds-every-message-number ()
  "Two messages are written as (1 2), in the order they sit in the folder."
  (vm-test-with-folder (vm-order-test--folder 2)
    (vm-number-messages)
    (vm-stuff-message-order)
    (should (equal '(1 2) (vm-order-test--order-in-header)))))

(ert-deftest vm-order-test-a-long-order-is-folded-every-fifteen ()
  "The header wraps after every fifteenth number, and stays readable.

`vm-stuff-message-order' writes \"\\n\\t \" where `(zerop (% n 15))', so a
folder of sixteen messages is the first with a continuation line.  Nothing
reached that branch before: the coverage report had it among the never
evaluated, every test until now using two or three messages.

Both halves matter.  The continuation lines have to begin with whitespace or
they are not folding and the header ends early, and the whole has to `read'
back as the same list of numbers or the order is lost."
  (dolist (n '(16 33))
    (vm-test-with-folder (vm-order-test--folder n)
      (vm-number-messages)
      (vm-stuff-message-order)
      (let ((header (vm-order-test--header)))
        (should header)
        (let ((lines (split-string (string-trim-right header "\n") "\n")))
          ;; it wrapped: 16 numbers over two lines, 33 over three
          (should (equal (+ 2 (/ (1- n) 15)) (length lines)))
          ;; every line after the first is a continuation
          (dolist (line (cdr lines))
            (should (string-match-p "\\`[ \t]" line))))
        ;; and it reads back as the numbers that went in
        (should (equal (number-sequence 1 n) (vm-order-test--order-in-header)))))))

(ert-deftest vm-order-test-stuffing-twice-leaves-one-header ()
  "Writing the order again replaces it rather than adding a second.
The writer deletes any order header it finds before inserting, and without
that a folder would grow one on every save."
  (vm-test-with-folder (vm-order-test--folder 4)
    (vm-number-messages)
    (dotimes (_ 3) (vm-stuff-message-order))
    (let ((n 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward vm-message-order-header-regexp nil t)
          (setq n (1+ n))))
      (should (equal 1 n)))
    (should (equal '(1 2 3 4) (vm-order-test--order-in-header)))))

(ert-deftest vm-order-test-a-reordered-folder-round-trips ()
  "An order VM wrote is the order VM reads back.

Reverses the message list, writes the order, then reads the folder afresh and
gobbles the header: the messages come back in the reversed order.  This is
what the header is for, and what a folder loses if the writing and the reading
disagree.

The header lists the numbers in the folder\'s *physical* order, not in
presentation order: `vm-stuff-message-order\' sorts by `vm-start-of\' and
writes each message\'s number.  So a reversed list gives (5 4 3 2 1), the
message stored first being the one now presented fifth."
  (vm-test-with-folder (vm-order-test--folder 5)
    (vm-number-messages)
    (setq vm-message-list (nreverse vm-message-list))
    (vm-number-messages)
    (vm-stuff-message-order)
    (should (equal '(5 4 3 2 1) (vm-order-test--order-in-header)))
    ;; the subjects in the order the list now has
    (let ((subjects (mapcar (lambda (m) (vm-su-subject m)) vm-message-list))
          (text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal "subject 5" (car subjects)))
      ;; read it again from the same text and apply the header
      (vm-test-with-folder text
        (vm-number-messages)
        (vm-gobble-message-order)
        (should (equal subjects
                       (mapcar (lambda (m) (vm-su-subject m)) vm-message-list)))))))

(ert-deftest vm-order-test-a-bad-order-header-is-a-warning-not-a-failure ()
  "A header that will not `read' is complained about and ignored.
The folder is still readable afterwards, in the order it is stored in, which
is the point of the `condition-case': a corrupt order header must not stop a
folder being opened."
  (vm-test-with-folder (vm-order-test--folder 3)
    (vm-number-messages)
    ;; Into the first message's header block, which is the only place
    ;; `vm-gobble-message-order' looks: it stops at the blank line.
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)                  ; past the From_ separator
      (insert "X-VM-Message-Order:\n\t(1 2 3\n"))
    (let (warned)
      (cl-letf (((symbol-function 'vm-warn)
                 (lambda (_l _t format &rest args)
                   (push (apply #'format format args) warned))))
        (vm-gobble-message-order))
      (should warned)
      (should (string-match-p "[Bb]ad order header" (car warned))))
    ;; still three messages, still in their stored order
    (should (equal 3 (length vm-message-list)))
    (should (equal '("subject 1" "subject 2" "subject 3")
                   (mapcar (lambda (m) (vm-su-subject m)) vm-message-list)))))

(ert-deftest vm-order-test-removing-the-header-takes-it-out ()
  "`vm-remove-message-order' leaves no order header behind."
  (vm-test-with-folder (vm-order-test--folder 4)
    (vm-number-messages)
    (vm-stuff-message-order)
    (should (vm-order-test--header))
    (vm-remove-message-order)
    (should-not (vm-order-test--header))))

(ert-deftest vm-order-test-having-an-order-header-is-detected ()
  "`vm-has-message-order' answers for the folder in the buffer.
It is how VM decides whether a folder has an order to apply at all, and it
looks only in the first message's header block, which is where the writer
puts one."
  (vm-test-with-folder (vm-order-test--folder 3)
    (vm-number-messages)
    (should-not (vm-has-message-order))
    (vm-stuff-message-order)
    (should (vm-has-message-order))
    (vm-remove-message-order)
    (should-not (vm-has-message-order))))

(ert-deftest vm-order-test-an-order-header-in-a-body-is-not-found ()
  "A message body holding what looks like an order header is not one.
`vm-has-message-order' and `vm-gobble-message-order' both stop at the blank
line that ends the first message's headers, so text further down cannot
reorder a folder.  A message quoting one of these headers would otherwise do
it."
  (vm-test-with-folder (vm-order-test--folder 3)
    (vm-number-messages)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^body 1$")
      (beginning-of-line)
      (insert "X-VM-Message-Order:\n\t(3 2 1)\n"))
    (should-not (vm-has-message-order))
    (vm-gobble-message-order)
    ;; the order is unchanged, the body text notwithstanding
    (should (equal '("subject 1" "subject 2" "subject 3")
                   (mapcar (lambda (m) (vm-su-subject m)) vm-message-list)))))


;;; What VM says when mail arrives (emacs-vm/vm#796)

(ert-deftest vm-folder-test-the-arrival-line-names-the-folder-once ()
  "REGRESSION: the arrival announcement does not say the folder twice.

Issue #796, reported against develop.  The asynchronous IMAP and POP paths
each prefixed the folder name and then appended `vm-totals-blurb', which
labels itself, so a reader getting new mail saw

    folder: 1 new message.  folder: 3 messages, 3 new, 0 unread, 0 deleted

with the name twice and the new count twice.  The reporter took it for a
corrupted status bar; it is one message, and the echo area was showing all of
it."
  (vm-test-with-real-folder (3)
    (let* ((name (buffer-name))
           (line (vm-arrival-blurb 1))
           (times (let ((n 0) (start 0))
                    (while (string-match (regexp-quote name) line start)
                      (setq n (1+ n) start (match-end 0)))
                    n)))
      (should (equal 1 times))
      ;; and it still says both things: what arrived, and what the folder holds
      (should (string-match-p "1 new message\\." line))
      (should (string-match-p "3 messages, 3 new, 0 unread, 0 deleted" line)))))

(ert-deftest vm-folder-test-the-totals-blurb-can-leave-its-label-off ()
  "`vm-totals-blurb' labels itself unless asked not to.
The labelled form is what `vm-emit-totals-blurb' shows on its own; the
unlabelled one is for a caller that has named the folder already, which is
what #796 was about."
  (vm-test-with-real-folder (3)
    (let ((name (buffer-name)))
      (should (string-prefix-p (concat name ": ") (vm-totals-blurb)))
      (should-not (string-match-p (regexp-quote name) (vm-totals-blurb t)))
      ;; the counts are the same either way
      (should (equal (vm-totals-blurb)
                     (concat name ": " (vm-totals-blurb t)))))))

(ert-deftest vm-folder-test-an-empty-folder-says-so-either-way ()
  "The no-messages form is labelled or not, as asked.
The other arm of `vm-totals-blurb', which a folder with nothing in it takes."
  (vm-test-with-real-folder (1)
    ;; one message, then emptied of it
    (setq vm-message-list nil
          vm-totals nil
          vm-modification-counter (1+ vm-modification-counter))
    (let ((name (buffer-name)))
      (should (equal (concat name ": No messages.") (vm-totals-blurb)))
      (should (equal "No messages." (vm-totals-blurb t))))))

;;; Fetching from a spool maildrop: one way in (emacs-vm/vm#822)

(defmacro vm-folder-test--in-a-folder-with-a-spool (maildrop &rest body)
  "Run BODY in a folder buffer whose only spool entry is MAILDROP.
`vm-get-spooled-mail-normal' works on the buffer visiting the folder file
named in the triple, so there has to be a file and it has to be visited."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "vm-spool-test" t))
	  (folder (expand-file-name "inbox" directory))
	  (buffer nil))
     (unwind-protect
	 (progn
	   (write-region "" nil folder nil 'quiet)
	   (setq buffer (find-file-noselect folder))
	   (cl-letf (((symbol-function 'vm-compute-spool-files)
		      (lambda (&rest _)
			(list (list folder ,maildrop
				    (expand-file-name "crash" directory)))))
		     ((symbol-function 'vm-assimilate-new-messages)
		      (lambda (&rest _) nil))
		     ((symbol-function 'vm-update-summary-and-mode-line)
		      (lambda () nil)))
	     (with-current-buffer buffer
	       (let ((vm-folder-directory directory)
		     (vm-buffers-needing-display-update (make-vector 29 0))
		     (vm-global-block-new-mail nil))
		 ,@body))))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer (set-buffer-modified-p nil))
	 (kill-buffer buffer))
       (delete-directory directory t))))

(ert-deftest vm-folder-test-a-network-maildrop-is-not-fetched-by-waiting ()
  "A network spool maildrop the driver declines is not fetched blockingly.
The driver declines only where VM has no password and the reader, who was
asked, gave none, so calling the blocking implementation would put the same
question a second time."
  (let ((called nil)
	(said nil))
    (cl-letf (((symbol-function 'vm-start-spooled-mail) (lambda (&rest _) nil))
	      ((symbol-function 'vm-imap-move-mail)
	       (lambda (&rest _) (setq called t) t))
	      ((symbol-function 'vm-gobble-crash-box) (lambda (&rest _) nil))
	      ((symbol-function 'vm-inform)
	       (lambda (_level format &rest args)
		 (setq said (concat (or said "")
				    (apply #'format format args))))))
      (vm-folder-test--in-a-folder-with-a-spool
	  "imap:mail.example:143:INBOX:login:me:*"
	(vm-get-spooled-mail-normal nil))
      (should-not called)
      (should (string-match-p "no password" (or said ""))))))

(ert-deftest vm-folder-test-a-spool-file-is-still-fetched-by-waiting ()
  "A local spool file is fetched by waiting, which is the only way there is.
`movemail' and the rest are not on the driver, so the branch that waits for
them has to stay."
  (let ((fetched nil))
    (cl-letf (((symbol-function 'vm-start-spooled-mail) (lambda (&rest _) nil))
	      ((symbol-function 'vm-spool-move-mail)
	       (lambda (&rest _) (setq fetched t) t))
	      ((symbol-function 'vm-gobble-crash-box) (lambda (&rest _) nil))
	      ((symbol-function 'vm-inform) (lambda (&rest _) nil)))
      (vm-folder-test--in-a-folder-with-a-spool "po:me"
	(vm-get-spooled-mail-normal nil))
      (should fetched))))

(ert-deftest vm-folder-test-move-spooled-mail-answers-t-once-mail-has-arrived ()
  "An error or a quit answers t when mail has already reached the folder.
Anything else would leave the reader looking for mail that is neither in the
crash box nor visibly in the folder."
  (cl-letf (((symbol-function 'vm-warn) (lambda (&rest _) nil)))
    (should (eq t (vm-move-spooled-mail
		   (lambda (&rest _) (error "server said no")) "drop" "crash" t)))
    (should (eq t (vm-move-spooled-mail
		   (lambda (&rest _) (signal 'quit nil)) "drop" "crash" t)))
    (should (equal "fetched" (vm-move-spooled-mail
			      (lambda (&rest _) "fetched") "drop" "crash" t)))
    (should-error (vm-move-spooled-mail
		   (lambda (&rest _) (error "server said no")) "drop" "crash" nil))))

(provide 'vm-folder-test)

;;; vm-folder-test.el ends here
