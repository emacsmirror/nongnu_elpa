;;; vm-sort-test.el --- Tests for vm-sort.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM sort functions in vm-sort.el

;;; Code:

(require 'vm-test-init)
(require 'vm-sort)

;;; vm-so-trim-subject tests

(ert-deftest vm-sort-test-trim-subject-plain ()
  "Test trim-subject with plain subject."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello World") "Hello World"))))

(ert-deftest vm-sort-test-trim-subject-re-prefix ()
  "Test trim-subject strips Re: prefix."
  (let ((vm-subject-ignored-prefix "^\\(re: *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Re: Hello") "Hello"))
    (should (equal (vm-so-trim-subject "Re: Re: Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-fwd-prefix ()
  "Test trim-subject strips Fwd: prefix."
  (let ((vm-subject-ignored-prefix "^\\(\\(re\\|fwd?\\): *\\)+")
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Fwd: Hello") "Hello"))
    (should (equal (vm-so-trim-subject "Fw: Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-suffix ()
  "Test trim-subject strips suffix."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix " *(fwd)$")
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello (fwd)") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-tag-prefix ()
  "Test trim-subject strips tag prefix like [list]."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix "^\\[[^]]+\\] *")
        (vm-subject-tag-prefix-exceptions nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "[list] Hello") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-significant-chars ()
  "Test trim-subject respects significant chars limit."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars 5))
    (should (equal (vm-so-trim-subject "Hello World") "Hello"))))

(ert-deftest vm-sort-test-trim-subject-whitespace ()
  "Test trim-subject collapses whitespace."
  (let ((vm-subject-ignored-prefix nil)
        (vm-subject-ignored-suffix nil)
        (vm-subject-tag-prefix nil)
        (vm-subject-significant-chars nil))
    (should (equal (vm-so-trim-subject "Hello   World") "Hello World"))))

;;; Sort comparison function tests with real messages

(defconst vm-sort-test-folder
  "From alice@example.com Mon Jan  1 00:00:00 2024
From: Alice <alice@example.com>
To: recipient@example.com
Subject: Zebra
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <sort1@example.com>

First message.

From bob@example.com Tue Jan  2 00:00:00 2024
From: Bob <bob@example.com>
To: recipient@example.com
Subject: Apple
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <sort2@example.com>

Second message with more lines.
This has three lines.
Actually.

From carol@example.com Wed Jan  3 00:00:00 2024
From: Carol <carol@example.com>
To: other@example.com
Subject: Middle
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <sort3@example.com>

Third message.

"
  "Test folder with messages for sorting tests.")

(ert-deftest vm-sort-test-so-sortable-subject ()
  "Test vm-so-sortable-subject returns trimmed subject."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg (car vm-message-list)))
      ;; Should return the sortable subject
      (should (stringp (vm-so-sortable-subject msg))))))

(ert-deftest vm-sort-test-so-sortable-datestring ()
  "Test vm-so-sortable-datestring returns sortable date."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg (car vm-message-list)))
      ;; Should return a date string suitable for sorting
      (should (stringp (vm-so-sortable-datestring msg))))))

(ert-deftest vm-sort-test-compare-date ()
  "Test vm-sort-compare-date compares dates correctly."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg2 (cadr vm-message-list)))
      ;; msg1 is Jan 1, msg2 is Jan 2
      ;; vm-sort-compare-date returns: t (less), '= (equal), nil (greater)
      ;; msg1 < msg2 chronologically, so should return t
      (let ((result (vm-sort-compare-date msg1 msg2)))
        (should (memq result '(t =)))))))

(ert-deftest vm-sort-test-compare-date-r ()
  "Test vm-sort-compare-date-r reverses date comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg2 (cadr vm-message-list)))
      ;; vm-sort-compare-date-r returns: nil (less), '= (equal), t (greater)
      ;; msg1 < msg2 chronologically, reversed returns nil
      (let ((result (vm-sort-compare-date-r msg1 msg2)))
        (should (memq result '(nil =)))))))

(ert-deftest vm-sort-test-compare-author ()
  "Test vm-sort-compare-author compares authors lexicographically."
  (vm-test-with-folder vm-sort-test-folder
    (let ((alice (car vm-message-list))
          (bob (cadr vm-message-list)))
      ;; Alice < Bob alphabetically
      (should (eq (vm-sort-compare-author alice bob) t)))))

(ert-deftest vm-sort-test-compare-subject ()
  "Test vm-sort-compare-subject compares subjects lexicographically."
  (vm-test-with-folder vm-sort-test-folder
    (let ((zebra (car vm-message-list))    ; Subject: Zebra
          (apple (cadr vm-message-list)))   ; Subject: Apple
      ;; Apple < Zebra alphabetically
      (should (eq (vm-sort-compare-subject zebra apple) nil)))))

(ert-deftest vm-sort-test-compare-line-count ()
  "Test vm-sort-compare-line-count compares by number of lines."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))   ; 1 line
          (msg2 (cadr vm-message-list))) ; 3 lines
      ;; msg1 has fewer lines than msg2
      (should (eq (vm-sort-compare-line-count msg1 msg2) t)))))

;;; Sort comparison function existence tests (backwards compat)

(ert-deftest vm-sort-test-compare-functions-exist ()
  "Test that sort comparison functions exist."
  (should (fboundp 'vm-sort-compare-author))
  (should (fboundp 'vm-sort-compare-author-r))
  (should (fboundp 'vm-sort-compare-date))
  (should (fboundp 'vm-sort-compare-date-r))
  (should (fboundp 'vm-sort-compare-subject))
  (should (fboundp 'vm-sort-compare-subject-r))
  (should (fboundp 'vm-sort-compare-recipients))
  (should (fboundp 'vm-sort-compare-recipients-r))
  (should (fboundp 'vm-sort-compare-line-count))
  (should (fboundp 'vm-sort-compare-line-count-r))
  (should (fboundp 'vm-sort-compare-byte-count))
  (should (fboundp 'vm-sort-compare-byte-count-r))
  (should (fboundp 'vm-sort-compare-physical-order))
  (should (fboundp 'vm-sort-compare-physical-order-r)))

;;; vm-supported-sort-keys tests

(ert-deftest vm-sort-test-keys-recognized ()
  "Test that sort keys are in vm-supported-sort-keys."
  (should (member "date" vm-supported-sort-keys))
  (should (member "reversed-date" vm-supported-sort-keys))
  (should (member "author" vm-supported-sort-keys))
  (should (member "subject" vm-supported-sort-keys))
  (should (member "recipients" vm-supported-sort-keys))
  (should (member "line-count" vm-supported-sort-keys))
  (should (member "byte-count" vm-supported-sort-keys))
  (should (member "physical-order" vm-supported-sort-keys)))

(provide 'vm-sort-test)

;;; vm-sort-test.el ends here
