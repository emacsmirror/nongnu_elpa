;;; vm-digest-test.el --- Tests for vm-digest.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM digest/encapsulation functions in vm-digest.el

;;; Code:

(require 'vm-test-init)
(require 'vm-digest)

;;; RFC 934 char stuffing tests

(ert-deftest vm-digest-test-rfc934-stuff-single-dash ()
  "Test RFC 934 stuffing of line starting with single dash."
  (with-temp-buffer
    (insert "-Line starts with dash\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "- -Line starts with dash"))))

(ert-deftest vm-digest-test-rfc934-stuff-double-dash ()
  "Test RFC 934 stuffing of line starting with double dash."
  (with-temp-buffer
    (insert "--Double dash line\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "- --Double dash line"))))

(ert-deftest vm-digest-test-rfc934-stuff-multiple-lines ()
  "Test RFC 934 stuffing of multiple lines with dashes."
  (with-temp-buffer
    (insert "Normal line\n")
    (insert "-Dash line\n")
    (insert "Another normal\n")
    (insert "---Triple dash\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (search-forward "Normal line" nil t))
    (should (search-forward "- -Dash line" nil t))
    (should (search-forward "Another normal" nil t))
    (should (search-forward "- ---Triple dash" nil t))))

(ert-deftest vm-digest-test-rfc934-stuff-no-dash ()
  "Test RFC 934 stuffing leaves non-dash lines unchanged."
  (with-temp-buffer
    (insert "No dashes here\n")
    (insert "Or here either\n")
    (vm-rfc934-char-stuff-region (point-min) (point-max))
    (should (string= (buffer-string) "No dashes here\nOr here either\n"))))

(ert-deftest vm-digest-test-rfc934-unstuff-single ()
  "Test RFC 934 unstuffing of single stuffed line."
  (with-temp-buffer
    (insert "- -Line was stuffed\n")
    (vm-rfc934-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "-Line was stuffed"))))

(ert-deftest vm-digest-test-rfc934-unstuff-multiple ()
  "Test RFC 934 unstuffing of multiple stuffed lines."
  (with-temp-buffer
    (insert "Normal line\n")
    (insert "- -Stuffed line\n")
    (insert "- --Double stuffed\n")
    (vm-rfc934-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (search-forward "Normal line" nil t))
    (should (search-forward "-Stuffed line" nil t))
    (should (search-forward "--Double stuffed" nil t))))

(ert-deftest vm-digest-test-rfc934-roundtrip ()
  "Test RFC 934 stuff then unstuff returns original."
  (with-temp-buffer
    (let ((original "-Original dash line\n--Double\n---Triple\n"))
      (insert original)
      (vm-rfc934-char-stuff-region (point-min) (point-max))
      (vm-rfc934-char-unstuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

;;; RFC 1153 char stuffing tests

(ert-deftest vm-digest-test-rfc1153-stuff-separator ()
  "Test RFC 1153 stuffing of 30-hyphen separator."
  (with-temp-buffer
    (insert "------------------------------\n")
    (vm-rfc1153-char-stuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at " -----------------------------"))))

(ert-deftest vm-digest-test-rfc1153-stuff-non-separator ()
  "Test RFC 1153 stuffing leaves non-separator lines unchanged."
  (with-temp-buffer
    (insert "Normal text\n")
    (insert "-----------------------------\n")  ; Only 29 dashes
    (insert "-------------------------------\n")  ; 31 dashes
    (let ((original (buffer-string)))
      (vm-rfc1153-char-stuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

(ert-deftest vm-digest-test-rfc1153-unstuff-separator ()
  "Test RFC 1153 unstuffing of stuffed separator."
  (with-temp-buffer
    (insert " -----------------------------\n")
    (vm-rfc1153-char-unstuff-region (point-min) (point-max))
    (goto-char (point-min))
    (should (looking-at "------------------------------"))))

(ert-deftest vm-digest-test-rfc1153-roundtrip ()
  "Test RFC 1153 stuff then unstuff returns original."
  (with-temp-buffer
    (let ((original "Text before\n------------------------------\nText after\n"))
      (insert original)
      (vm-rfc1153-char-stuff-region (point-min) (point-max))
      (vm-rfc1153-char-unstuff-region (point-min) (point-max))
      (should (string= (buffer-string) original)))))

;;; vm-digest-get-header-contents tests

(ert-deftest vm-digest-test-get-header-from ()
  "Test extracting From header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (let ((from (vm-digest-get-header-contents "From")))
      (should (stringp from))
      (should (string-match "test@example.com" from)))))

(ert-deftest vm-digest-test-get-header-subject ()
  "Test extracting Subject header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test Subject Line\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (let ((subject (vm-digest-get-header-contents "Subject")))
      (should (stringp subject))
      (should (string-match "Test Subject Line" subject)))))

(ert-deftest vm-digest-test-get-header-missing ()
  "Test extracting non-existent header returns nil."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (should (null (vm-digest-get-header-contents "X-NonExistent")))))

(ert-deftest vm-digest-test-get-header-case-insensitive ()
  "Test header matching is case-insensitive."
  (with-temp-buffer
    (insert "FROM: test@example.com\n")
    (insert "\n")
    (goto-char (point-min))
    (let ((from (vm-digest-get-header-contents "from")))
      (should (stringp from))
      (should (string-match "test@example.com" from)))))

(ert-deftest vm-digest-test-get-header-multiline ()
  "Test extracting multiline header."
  (with-temp-buffer
    (insert "Subject: This is a very long subject\n")
    (insert "   that continues on the next line\n")
    (insert "From: test@example.com\n")
    (insert "\n")
    (goto-char (point-min))
    (let ((subject (vm-digest-get-header-contents "Subject")))
      (should (stringp subject))
      (should (string-match "very long subject" subject))
      (should (string-match "continues" subject)))))

;;; vm-guess-digest-type tests

;; Note: These tests would need actual message structures to work fully

;;; Interactive command tests

(ert-deftest vm-digest-test-burst-commands-interactive ()
  "Test that burst commands are interactive."
  (should (commandp 'vm-burst-digest))
  (should (commandp 'vm-burst-rfc934-digest))
  (should (commandp 'vm-burst-rfc1153-digest))
  (should (commandp 'vm-burst-mime-digest))
  (should (commandp 'vm-burst-digest-to-temp-folder)))

;;; Variable existence tests

(ert-deftest vm-digest-test-variables ()
  "Test that digest-related variables exist with expected types."
  (should (boundp 'vm-digest-burst-type))
  (should (stringp vm-digest-burst-type))
  (should (boundp 'vm-digest-identifier-header-format))
  (should (boundp 'vm-delete-after-bursting)))

(provide 'vm-digest-test)

;;; vm-digest-test.el ends here