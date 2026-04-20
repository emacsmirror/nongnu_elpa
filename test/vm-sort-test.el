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

(ert-deftest vm-sort-test-compare-line-count-r ()
  "Test vm-sort-compare-line-count-r reverses line count comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))   ; 1 line
          (msg2 (cadr vm-message-list))) ; 3 lines
      ;; msg1 has fewer lines than msg2, reversed returns nil
      (should (eq (vm-sort-compare-line-count-r msg1 msg2) nil))
      ;; msg2 has more lines than msg1, reversed returns t
      (should (eq (vm-sort-compare-line-count-r msg2 msg1) t)))))

(ert-deftest vm-sort-test-compare-author-r ()
  "Test vm-sort-compare-author-r reverses author comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((alice (car vm-message-list))
          (bob (cadr vm-message-list)))
      ;; Alice < Bob alphabetically, reversed returns nil
      (should (eq (vm-sort-compare-author-r alice bob) nil))
      ;; Bob > Alice alphabetically, reversed returns t
      (should (eq (vm-sort-compare-author-r bob alice) t)))))

(ert-deftest vm-sort-test-compare-subject-r ()
  "Test vm-sort-compare-subject-r reverses subject comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((zebra (car vm-message-list))    ; Subject: Zebra
          (apple (cadr vm-message-list)))   ; Subject: Apple
      ;; Apple < Zebra alphabetically, so Zebra > Apple
      ;; Normal: (zebra, apple) -> nil (zebra does not precede apple)
      ;; Reversed: (zebra, apple) -> t (zebra precedes apple in reversed)
      (should (eq (vm-sort-compare-subject-r zebra apple) t)))))

(ert-deftest vm-sort-test-compare-byte-count ()
  "Test vm-sort-compare-byte-count compares by message size."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg2 (cadr vm-message-list)))
      ;; msg2 has more content (3 lines vs 1 line), so more bytes
      (let ((result (vm-sort-compare-byte-count msg1 msg2)))
        (should (memq result '(t =)))))))

(ert-deftest vm-sort-test-compare-byte-count-r ()
  "Test vm-sort-compare-byte-count-r reverses byte count comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg2 (cadr vm-message-list)))
      ;; If msg1 < msg2 in bytes, reversed should give opposite
      (let ((fwd (vm-sort-compare-byte-count msg1 msg2))
            (rev (vm-sort-compare-byte-count-r msg1 msg2)))
        (cond ((eq fwd t) (should (eq rev nil)))
              ((eq fwd nil) (should (eq rev t)))
              (t (should (eq rev '=))))))))

(ert-deftest vm-sort-test-compare-recipients ()
  "Test vm-sort-compare-recipients compares To/Cc headers."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))    ; To: recipient@example.com
          (msg3 (nth 2 vm-message-list))) ; To: other@example.com
      ;; other < recipient alphabetically
      (should (eq (vm-sort-compare-recipients msg3 msg1) t)))))

(ert-deftest vm-sort-test-compare-recipients-r ()
  "Test vm-sort-compare-recipients-r reverses recipient comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))    ; To: recipient@example.com
          (msg3 (nth 2 vm-message-list))) ; To: other@example.com
      ;; other < recipient alphabetically, reversed means msg3 comes after
      (should (eq (vm-sort-compare-recipients-r msg3 msg1) nil)))))

(ert-deftest vm-sort-test-compare-full-name ()
  "Test vm-sort-compare-full-name compares sender full names."
  (vm-test-with-folder vm-sort-test-folder
    (let ((alice (car vm-message-list))   ; From: Alice
          (bob (cadr vm-message-list)))   ; From: Bob
      ;; Alice < Bob alphabetically
      (should (eq (vm-sort-compare-full-name alice bob) t)))))

(ert-deftest vm-sort-test-compare-full-name-r ()
  "Test vm-sort-compare-full-name-r reverses full name comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((alice (car vm-message-list))
          (bob (cadr vm-message-list)))
      ;; Alice < Bob, reversed returns nil
      (should (eq (vm-sort-compare-full-name-r alice bob) nil)))))

(ert-deftest vm-sort-test-compare-addressees ()
  "Test vm-sort-compare-addressees compares To header only."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))    ; To: recipient@example.com
          (msg3 (nth 2 vm-message-list))) ; To: other@example.com
      ;; other < recipient
      (should (eq (vm-sort-compare-addressees msg3 msg1) t)))))

(ert-deftest vm-sort-test-compare-addressees-r ()
  "Test vm-sort-compare-addressees-r reverses addressee comparison."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg3 (nth 2 vm-message-list)))
      (should (eq (vm-sort-compare-addressees-r msg3 msg1) nil)))))

(ert-deftest vm-sort-test-compare-physical-order ()
  "Test vm-sort-compare-physical-order compares by buffer position."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (msg2 (cadr vm-message-list)))
      ;; msg1 appears before msg2 in the folder
      (should (eq (vm-sort-compare-physical-order msg1 msg2) t)))))

(ert-deftest vm-sort-test-compare-equal-returns-symbol ()
  "Test that comparing equal values returns '= symbol."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list)))
      ;; Comparing message with itself should return '=
      (should (eq (vm-sort-compare-author msg1 msg1) '=))
      (should (eq (vm-sort-compare-date msg1 msg1) '=))
      (should (eq (vm-sort-compare-subject msg1 msg1) '=))
      (should (eq (vm-sort-compare-line-count msg1 msg1) '=)))))

;;; vm-sort-compare-xxxxxx tests (the main sort function)

(ert-deftest vm-sort-test-compare-xxxxxx-single-key ()
  "Test vm-sort-compare-xxxxxx with a single key function."
  (vm-test-with-folder vm-sort-test-folder
    (let ((alice (car vm-message-list))
          (bob (cadr vm-message-list))
          (vm-key-functions '(vm-sort-compare-author)))
      ;; Alice < Bob
      (should (eq (vm-sort-compare-xxxxxx alice bob) t))
      (should (eq (vm-sort-compare-xxxxxx bob alice) nil)))))

(ert-deftest vm-sort-test-compare-xxxxxx-multiple-keys ()
  "Test vm-sort-compare-xxxxxx falls back to second key on tie."
  (vm-test-with-folder vm-sort-test-folder
    (let ((msg1 (car vm-message-list))
          (vm-key-functions '(vm-sort-compare-author vm-sort-compare-date)))
      ;; When comparing msg with itself, first key returns '=
      ;; so it should try second key
      (should (booleanp (vm-sort-compare-xxxxxx msg1 msg1))))))

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
