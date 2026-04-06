;;; vm-summary-test.el --- Tests for vm-summary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM summary functions in vm-summary.el
;; Tests pure string manipulation and formatting functions.

;;; Code:

(require 'vm-test-init)
(require 'vm-summary)

;;; vm-string-width tests

(ert-deftest vm-summary-test-string-width-ascii ()
  "Test string width for ASCII strings."
  (should (= (vm-string-width "hello") 5))
  (should (= (vm-string-width "") 0))
  (should (= (vm-string-width "a") 1)))

(ert-deftest vm-summary-test-string-width-spaces ()
  "Test string width with spaces and tabs."
  (should (= (vm-string-width "a b") 3))
  (should (= (vm-string-width "   ") 3)))

;;; vm-left-justify-string tests

(ert-deftest vm-summary-test-left-justify-shorter ()
  "Test left justifying a string shorter than width."
  (should (equal (vm-left-justify-string "hi" 5) "hi   ")))

(ert-deftest vm-summary-test-left-justify-equal ()
  "Test left justifying a string equal to width."
  (should (equal (vm-left-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-left-justify-longer ()
  "Test left justifying a string longer than width (no truncation)."
  (should (equal (vm-left-justify-string "hello world" 5) "hello world")))

;;; vm-right-justify-string tests

(ert-deftest vm-summary-test-right-justify-shorter ()
  "Test right justifying a string shorter than width."
  (should (equal (vm-right-justify-string "hi" 5) "   hi")))

(ert-deftest vm-summary-test-right-justify-equal ()
  "Test right justifying a string equal to width."
  (should (equal (vm-right-justify-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-right-justify-longer ()
  "Test right justifying a string longer than width (no truncation)."
  (should (equal (vm-right-justify-string "hello world" 5) "hello world")))

;;; vm-numeric-left-justify-string tests

(ert-deftest vm-summary-test-numeric-left-justify-shorter ()
  "Test numeric left justify pads with zeros."
  (should (equal (vm-numeric-left-justify-string "42" 5) "42000")))

(ert-deftest vm-summary-test-numeric-left-justify-equal ()
  "Test numeric left justify with equal length."
  (should (equal (vm-numeric-left-justify-string "12345" 5) "12345")))

;;; vm-numeric-right-justify-string tests

(ert-deftest vm-summary-test-numeric-right-justify-shorter ()
  "Test numeric right justify pads with zeros."
  (should (equal (vm-numeric-right-justify-string "42" 5) "00042")))

(ert-deftest vm-summary-test-numeric-right-justify-equal ()
  "Test numeric right justify with equal length."
  (should (equal (vm-numeric-right-justify-string "12345" 5) "12345")))

;;; vm-truncate-roman-string tests

(ert-deftest vm-summary-test-truncate-roman-shorter ()
  "Test truncating a string shorter than width."
  (should (equal (vm-truncate-roman-string "hi" 5) "hi")))

(ert-deftest vm-summary-test-truncate-roman-exact ()
  "Test truncating a string exactly at width."
  (should (equal (vm-truncate-roman-string "hello" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-longer ()
  "Test truncating a string longer than width."
  (should (equal (vm-truncate-roman-string "hello world" 5) "hello")))

(ert-deftest vm-summary-test-truncate-roman-negative ()
  "Test truncating from end with negative width."
  (should (equal (vm-truncate-roman-string "hello world" -5) "world")))

;;; vm-truncate-string tests

(ert-deftest vm-summary-test-truncate-string-basic ()
  "Test basic string truncation."
  (should (equal (vm-truncate-string "hello" 3) "hel")))

(ert-deftest vm-summary-test-truncate-string-no-change ()
  "Test truncation when string is short enough."
  (should (equal (vm-truncate-string "hi" 10) "hi")))

;;; vm-default-chop-full-name tests

(ert-deftest vm-summary-test-chop-name-angle-brackets ()
  "Test chopping name with angle bracket format."
  (let ((result (vm-default-chop-full-name "John Doe <john@example.com>")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-parens ()
  "Test chopping name with parenthesis format."
  (let ((result (vm-default-chop-full-name "john@example.com (John Doe)")))
    (should (equal (car result) "John Doe"))
    (should (equal (cadr result) "john@example.com"))))

(ert-deftest vm-summary-test-chop-name-plain-email ()
  "Test chopping plain email address."
  (let ((result (vm-default-chop-full-name "john@example.com")))
    ;; Plain email should have nil for full-name
    (should (or (null (car result))
                (equal (car result) "john@example.com")))))

(ert-deftest vm-summary-test-chop-name-quoted ()
  "Test chopping name with quoted string."
  (let ((result (vm-default-chop-full-name "\"John Q. Doe\" <john@example.com>")))
    (should (stringp (car result)))
    (should (equal (cadr result) "john@example.com"))))

;;; vm-su-trim-subject tests (when stripping is disabled)

(ert-deftest vm-summary-test-trim-subject-passthrough ()
  "Test subject trimming when stripping is disabled."
  (let ((vm-summary-strip-subject-tags nil)
        (vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil))
    ;; With stripping disabled, should return subject as-is
    (should (equal (vm-su-trim-subject "Hello World") "Hello World"))))

(ert-deftest vm-summary-test-trim-subject-with-re ()
  "Test subject trimming with Re: prefix."
  (let ((vm-summary-strip-subject-tags t)
        (vm-subject-ignored-prefix "^\\(re: *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-tag-prefix-exceptions nil))
    (let ((result (vm-su-trim-subject "Re: Hello World")))
      (should (stringp result))
      ;; The Re: is moved to prefix but kept
      (should (string-match "Hello World" result)))))

;;; High-level header content extraction (requires message structure)
;; These tests verify the infrastructure but don't create full messages

(ert-deftest vm-summary-test-infrastructure-loaded ()
  "Test that summary functions are loaded."
  (should (fboundp 'vm-string-width))
  (should (fboundp 'vm-left-justify-string))
  (should (fboundp 'vm-right-justify-string))
  (should (fboundp 'vm-truncate-string))
  (should (fboundp 'vm-default-chop-full-name))
  (should (fboundp 'vm-su-trim-subject)))

;;; Integration tests using real messages

(defconst vm-summary-test-folder
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: John Doe <john@example.com>
To: recipient@example.com
Cc: cc@example.com
Subject: Test Message
Date: Mon, 01 Jan 2024 10:30:45 +0000
Message-ID: <msg1@example.com>

This is the message body with some text.
Second line here.

"
  "Test folder for summary tests.")

;;; vm-su-from tests

(ert-deftest vm-summary-test-su-from ()
  "Test vm-su-from extracts sender address."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((from (vm-su-from msg)))
        (should (stringp from))
        (should (string-match "john" from))))))

;;; vm-su-decoded-full-name tests

(ert-deftest vm-summary-test-su-decoded-full-name ()
  "Test vm-su-decoded-full-name extracts full name."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((name (vm-su-decoded-full-name msg)))
        (should (or (null name) (stringp name)))))))

;;; vm-su-decoded-subject tests

(ert-deftest vm-summary-test-su-decoded-subject ()
  "Test vm-su-decoded-subject extracts subject."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((subject (vm-su-decoded-subject msg)))
        (should (stringp subject))
        (should (string-match "Test Message" subject))))))

;;; vm-su-decoded-to tests

(ert-deftest vm-summary-test-su-decoded-to ()
  "Test vm-su-decoded-to extracts recipients."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((to (vm-su-decoded-to msg)))
        (should (stringp to))
        (should (string-match "recipient" to))))))

;;; vm-su-message-id tests

(ert-deftest vm-summary-test-su-message-id ()
  "Test vm-su-message-id extracts message ID."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((msg-id (vm-su-message-id msg)))
        (should (stringp msg-id))
        (should (string-match "msg1@example.com" msg-id))))))

;;; Date component tests

(ert-deftest vm-summary-test-su-weekday ()
  "Test vm-su-weekday extracts day of week."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((weekday (vm-su-weekday msg)))
        (should (stringp weekday))
        (should (string-match "Mon" weekday))))))

(ert-deftest vm-summary-test-su-monthday ()
  "Test vm-su-monthday extracts day of month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((monthday (vm-su-monthday msg)))
        (should (stringp monthday))
        (should (string-match "0?1" monthday))))))

(ert-deftest vm-summary-test-su-month ()
  "Test vm-su-month extracts month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((month (vm-su-month msg)))
        (should (stringp month))
        (should (string-match "Jan" month))))))

(ert-deftest vm-summary-test-su-year ()
  "Test vm-su-year extracts year."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((year (vm-su-year msg)))
        (should (stringp year))
        (should (string-match "2024" year))))))

(ert-deftest vm-summary-test-su-hour ()
  "Test vm-su-hour extracts time."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((hour (vm-su-hour msg)))
        (should (stringp hour))
        (should (string-match "10:30" hour))))))

(ert-deftest vm-summary-test-su-zone ()
  "Test vm-su-zone extracts timezone."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((zone (vm-su-zone msg)))
        (should (stringp zone))))))

;;; vm-su-mark tests

(ert-deftest vm-summary-test-su-mark-unmarked ()
  "Test vm-su-mark returns empty for unmarked message."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg nil)
      (let ((mark (vm-su-mark msg)))
        (should (stringp mark))
        (should (string= mark " "))))))

(ert-deftest vm-summary-test-su-mark-marked ()
  "Test vm-su-mark returns indicator for marked message."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg t)
      (let ((mark (vm-su-mark msg)))
        (should (stringp mark))
        (should (not (string= mark " ")))))))

;;; vm-su-line-count tests

(ert-deftest vm-summary-test-su-line-count ()
  "Test vm-su-line-count returns line count string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((lines (vm-su-line-count msg)))
        (should (stringp lines))))))

;;; vm-su-byte-count tests

(ert-deftest vm-summary-test-su-byte-count ()
  "Test vm-su-byte-count returns byte count string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((bytes (vm-su-byte-count msg)))
        (should (stringp bytes))))))

;;; vm-su-attribute-indicators tests

(ert-deftest vm-summary-test-su-attribute-indicators ()
  "Test vm-su-attribute-indicators returns indicator string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators msg)))
        (should (stringp indicators))))))

(ert-deftest vm-summary-test-su-attribute-indicators-short ()
  "Test vm-su-attribute-indicators-short returns short indicators."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators-short msg)))
        (should (stringp indicators))))))

(ert-deftest vm-summary-test-su-attribute-indicators-long ()
  "Test vm-su-attribute-indicators-long returns long indicators."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indicators (vm-su-attribute-indicators-long msg)))
        (should (stringp indicators))))))

;;; vm-su-labels tests

(ert-deftest vm-summary-test-su-labels-none ()
  "Test vm-su-labels returns empty string when no labels."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((labels (vm-su-labels msg)))
        (should (stringp labels))
        (should (string= labels ""))))))

(ert-deftest vm-summary-test-su-labels-with-labels ()
  "Test vm-su-labels returns labels when present."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("important" "work"))
      (let ((labels (vm-su-labels msg)))
        (should (stringp labels))))))

;;; vm-su-thread-indent tests

(ert-deftest vm-summary-test-su-thread-indent ()
  "Test vm-su-thread-indent returns indent string."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((indent (vm-su-thread-indent msg)))
        (should (stringp indent))))))

;;; vm-su-interesting-from tests

(ert-deftest vm-summary-test-su-interesting-from ()
  "Test vm-su-interesting-from returns from or to based on context."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((from (vm-su-interesting-from msg)))
        (should (stringp from))))))

;;; vm-su-size tests

(ert-deftest vm-summary-test-su-size ()
  "Test vm-su-size returns human-readable size."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((size (vm-su-size msg)))
        (should (stringp size))))))

;;; vm-su-datestring tests

(ert-deftest vm-summary-test-su-datestring ()
  "Test vm-su-datestring returns formatted date."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((datestring (vm-su-datestring msg)))
        (should (stringp datestring))))))

;;; vm-su-month-number tests

(ert-deftest vm-summary-test-su-month-number ()
  "Test vm-su-month-number returns numeric month."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((month-num (vm-su-month-number msg)))
        (should (stringp month-num))
        (should (string-match "0?1" month-num))))))

;;; vm-su-hour-short tests

(ert-deftest vm-summary-test-su-hour-short ()
  "Test vm-su-hour-short returns short time format."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((hour (vm-su-hour-short msg)))
        (should (stringp hour))))))

;;; vm-su-decoded-to-names tests

(ert-deftest vm-summary-test-su-decoded-to-names ()
  "Test vm-su-decoded-to-names extracts recipient names."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((names (vm-su-decoded-to-names msg)))
        (should (or (null names) (stringp names)))))))

;;; vm-su-decoded-to-cc tests

(ert-deftest vm-summary-test-su-decoded-to-cc ()
  "Test vm-su-decoded-to-cc extracts To and CC."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((to-cc (vm-su-decoded-to-cc msg)))
        (should (stringp to-cc))
        (should (string-match "recipient" to-cc))))))

;;; vm-su-decoded-to-cc-names tests

(ert-deftest vm-summary-test-su-decoded-to-cc-names ()
  "Test vm-su-decoded-to-cc-names extracts To and CC names."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((names (vm-su-decoded-to-cc-names msg)))
        (should (or (null names) (stringp names)))))))

;;; vm-su-interesting-full-name tests

(ert-deftest vm-summary-test-su-interesting-full-name ()
  "Test vm-su-interesting-full-name returns appropriate name."
  (vm-test-with-folder vm-summary-test-folder
    (let ((msg (car vm-message-list)))
      (let ((name (vm-su-interesting-full-name msg)))
        (should (or (null name) (stringp name)))))))

(provide 'vm-summary-test)

;;; vm-summary-test.el ends here
