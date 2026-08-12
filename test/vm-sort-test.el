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

;;; Sorting and reverse links (issue #453)

;; Sorting is the one operation that rebuilds every reverse link rather than
;; patching one: `vm-sort-messages' installs a new list and calls
;; `vm-reverse-link-messages' over it, but only when the order actually changed.
;; It then relocates `vm-message-pointer' through the links it just rebuilt.  A
;; folder sorted into a list whose links still describe the old order would
;; expunge the wrong message afterwards.

(defmacro vm-sort-test--with-real-folder (n &rest body)
  "Visit a generated folder of N messages with descending subjects, run BODY.
Subjects run down so that sorting by subject really reorders the list."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-sort" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          ;; Visiting a folder records it in these; bound so the test does not
          ;; leave the folder it invented in the session's history.
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list)))
     (require 'vm)
     (unwind-protect
         (progn
           (with-temp-file file
             (dotimes (i ,n)
               (insert (format "From s%d@example.com Mon Jan  1 00:00:00 2024\n" i)
                       (format "From: S%02d <s%d@example.com>\n" (- ,n i) i)
                       (format "Subject: subject %02d\n" (- ,n i))
                       "Date: Mon, 01 Jan 2024 00:00:00 +0000\n"
                       (format "Message-ID: <sort-%d@example.com>\n" i)
                       "\n" (format "Body %d.\n\n" i))))
           (vm-visit-folder file)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-sort-test-reverse-links-follow-the-sort ()
  "Sorting rebuilds the reverse links to match the new order.
Checked after a sort that reorders, and again after sorting back, since only a
sort that changes the order rebuilds the links at all."
  (vm-sort-test--with-real-folder 6
    (should (= 6 (length vm-message-list)))
    (should (vm-test-reverse-links-consistent-p))
    (vm-sort-messages "subject")
    (should (vm-test-reverse-links-consistent-p))
    ;; Really reordered: subjects descend in the file, so ascending now.
    (should (equal (sort (mapcar #'vm-su-subject vm-message-list) #'string<)
                   (mapcar #'vm-su-subject vm-message-list)))
    (should (null (vm-reverse-link-of (vm-test-first-message))))
    (vm-sort-messages "physical-order")
    (should (vm-test-reverse-links-consistent-p))
    (should (null (vm-reverse-link-of (vm-test-first-message))))))

(ert-deftest vm-sort-test-reverse-links-survive-a-sort-that-changes-nothing ()
  "Sorting a folder already in that order leaves the links alone and correct.
`vm-sort-messages' skips the rebuild when the order did not change, so this is
the path where the existing links have to be right already."
  (vm-sort-test--with-real-folder 5
    (vm-sort-messages "subject")
    (should (vm-test-reverse-links-consistent-p))
    (vm-sort-messages "subject")
    (should (vm-test-reverse-links-consistent-p))))

(ert-deftest vm-sort-test-expunging-after-a-sort-removes-the-right-message ()
  "A sorted folder expunges the message asked for, not its neighbour.
This is what a stale reverse link costs in practice: `vm-expunge-message' finds
the cons to splice through the link, so after a sort that rebuilt them wrongly
the folder would lose a different message than the one deleted."
  (vm-sort-test--with-real-folder 6
    (vm-sort-messages "subject")
    (let* ((victim (vm-test-nth-message 3))
           (subject (vm-su-subject victim))
           (others (delete subject (mapcar #'vm-su-subject vm-message-list))))
      (vm-set-deleted-flag victim t)
      (vm-expunge-folder)
      (should (= 5 (length vm-message-list)))
      (should (vm-test-reverse-links-consistent-p))
      (should (equal others (mapcar #'vm-su-subject vm-message-list))))))

;;; Moving one message about (issue #453)

;; `vm-move-message-forward' is the third place that rewrites reverse links, and
;; the only one that both removes and reinserts a cons.  It had no test at all:
;; dropping either of its four `vm-set-reverse-link-of' calls left the whole
;; suite passing.  A wrong link here is not visible in the summary, and shows up
;; later as an expunge removing a different message.
;;
;; `vm-move-message-backward' is `vm-move-message-forward' with the count
;; negated, so it needs no separate coverage of the splice.

(defun vm-sort-test--subjects ()
  "Return the subjects of `vm-message-list', in order."
  (mapcar #'vm-su-subject vm-message-list))

(defun vm-sort-test--conses ()
  "Return every cons of `vm-message-list', so membership can be asserted."
  (let ((mp vm-message-list) (all nil))
    (while mp
      (push mp all)
      (setq mp (cdr mp)))
    all))

(ert-deftest vm-sort-test-moving-a-message-forward-relinks-the-list ()
  "Moving a message one place forward leaves the links and numbers right.
Moving the head is the case where the message being moved has no link and the
one it displaces must be given none."
  (vm-sort-test--with-real-folder 4
    (let ((folder (current-buffer)))
      (setq vm-message-pointer vm-message-list)
      (vm-move-message-forward 1)
      (with-current-buffer folder
        (should (equal '("subject 03" "subject 04" "subject 02" "subject 01")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))
        ;; Renumbering starts from the reverse link of the redo start point.
        (should (equal '("1" "2" "3" "4")
                       (mapcar #'vm-number-of vm-message-list)))
        ;; The message moved is still the selected one.
        (should (equal "subject 04" (vm-su-subject (car vm-message-pointer))))))))

(ert-deftest vm-sort-test-moving-a-message-several-places-relinks-the-list ()
  "A move of more than one place lands the message in the middle, both ways.
Two moves, so the second starts from a list this function itself built."
  (vm-sort-test--with-real-folder 5
    (let ((folder (current-buffer)))
      (setq vm-message-pointer vm-message-list)
      (vm-move-message-forward 3)
      (with-current-buffer folder
        (should (equal '("subject 04" "subject 03" "subject 02"
                         "subject 05" "subject 01")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))
        (vm-move-message-backward 2))
      (with-current-buffer folder
        (should (equal '("subject 04" "subject 05" "subject 03"
                         "subject 02" "subject 01")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))
        (should (equal '("1" "2" "3" "4" "5")
                       (mapcar #'vm-number-of vm-message-list)))))))

(ert-deftest vm-sort-test-moving-a-message-to-the-end-relinks-the-list ()
  "Moving the last message backward, and the one before it forward.
The tail is the case where the moved message has nothing after it, so the
reinsertion has to give the new last message a nil cdr and the displaced one a
link to it."
  (vm-sort-test--with-real-folder 4
    (let ((folder (current-buffer)))
      (setq vm-message-pointer (last vm-message-list))
      (vm-move-message-backward 1)
      (with-current-buffer folder
        (should (equal '("subject 04" "subject 03" "subject 01" "subject 02")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))
        (setq vm-message-pointer (nthcdr 2 vm-message-list))
        (vm-move-message-forward 1))
      (with-current-buffer folder
        (should (equal '("subject 04" "subject 03" "subject 02" "subject 01")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))))))

(ert-deftest vm-sort-test-moving-past-the-end-of-the-folder-signals ()
  "There is nowhere past the ends to move to, and the list is left alone.
`vm-move-message-pointer' signals, and it does so while looking for the
destination, before anything is spliced."
  (vm-sort-test--with-real-folder 3
    (let ((folder (current-buffer))
          (vm-circular-folders nil)
          (before nil))
      (setq before (vm-sort-test--subjects))
      (setq vm-message-pointer vm-message-list)
      (should-error (vm-move-message-backward 1) :type 'beginning-of-folder)
      (with-current-buffer folder
        (setq vm-message-pointer (last vm-message-list))
        (should-error (vm-move-message-forward 1) :type 'end-of-folder))
      (with-current-buffer folder
        (should (equal before (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))))))

(ert-deftest vm-sort-test-expunging-after-a-move-removes-that-message ()
  "REGRESSION: the links a move leaves behind are the ones expunge follows.
`vm-expunge-message' picks the cons to splice from the reverse link, so a link
left wrong by the move removes a message nobody deleted.  This is the failure a
links check on its own only implies."
  (vm-sort-test--with-real-folder 4
    (let ((folder (current-buffer)))
      (setq vm-message-pointer vm-message-list)
      (vm-move-message-forward 2)
      (with-current-buffer folder
        (should (equal '("subject 03" "subject 02" "subject 04" "subject 01")
                       (vm-sort-test--subjects)))
        (vm-set-deleted-flag (nth 2 vm-message-list) t)   ; subject 04 again
        (vm-expunge-folder))
      (with-current-buffer folder
        (should (equal '("subject 03" "subject 02" "subject 01")
                       (vm-sort-test--subjects)))
        (should (vm-test-reverse-links-consistent-p))))))

(ert-deftest vm-sort-test-sorting-keeps-the-selected-message-selected ()
  "Sorting moves the pointers to where their messages ended up.
`vm-sort-messages' rebuilds the list, so the old conses are gone; it finds each
pointer's new cons through the message's rebuilt reverse link.  Missed, the
folder jumps to its first message on every sort, and `vm-last-message-pointer'
comes to mean a different message than the one the user was last at."
  (vm-sort-test--with-real-folder 5
    (let ((selected (vm-test-nth-message 3))
          (previous (vm-test-nth-message 1)))
      (setq vm-message-pointer (nthcdr 3 vm-message-list))
      (setq vm-last-message-pointer (nthcdr 1 vm-message-list))
      ;; Subjects descend in the file, so this really reorders.
      (vm-sort-messages "subject")
      (should (equal (sort (vm-sort-test--subjects) #'string<)
                     (vm-sort-test--subjects)))
      (should (eq selected (car vm-message-pointer)))
      (should (eq previous (car vm-last-message-pointer)))
      ;; And the conses really are the ones in the list now.
      (should (memq vm-message-pointer (vm-sort-test--conses)))
      (should (memq vm-last-message-pointer (vm-sort-test--conses))))))

;;; Sorting a folder (emacs-vm/vm#632)
;;
;; The sort comparators were called by no test: `vm-sort-messages' is what a
;; user runs, and the order it leaves the folder in is what they see.  Each
;; key is checked against a folder built so that the keys disagree -- sorting
;; by author, by date and by subject must each give a different order, or the
;; test would pass with every comparator returning the same thing.

(defconst vm-sort-test--folder
  (concat
   ;; alice: second by date, third by subject, second by size, first in file
   "From alice@example.com Sun Aug  2 10:00:00 2026\n"
   "From: alice@example.com\nTo: me@example.com\n"
   "Date: Sun, 2 Aug 2026 10:00:00 +0000\n"
   "Subject: cherries\nMessage-ID: <a@example.com>\n\n"
   "A body of middling length.\nWith a second line to it.\n\n"
   ;; carol: first by date, second by subject, longest, second in file
   "From carol@example.com Sat Aug  1 10:00:00 2026\n"
   "From: carol@example.com\nTo: me@example.com\n"
   "Date: Sat, 1 Aug 2026 10:00:00 +0000\n"
   "Subject: bananas\nMessage-ID: <c@example.com>\n\n"
   "The longest body of the three, which is what makes the size order\n"
   "differ from every other order in this folder.\nA third line.\n"
   "A fourth line.\nA fifth line.\n\n"
   ;; bob: last by date, first by subject, shortest, last in file
   "From bob@example.com Mon Aug  3 10:00:00 2026\n"
   "From: bob@example.com\nTo: me@example.com\n"
   "Date: Mon, 3 Aug 2026 10:00:00 +0000\n"
   "Subject: apples\nMessage-ID: <b@example.com>\n\nShort.\n\n")
  "Three messages whose every order differs from every other.
Author, date, subject, size and the order in the file are five different
arrangements of the same three messages, so a test of one key cannot pass by
agreeing with another -- which is how a date test first passed here with the
date comparator swapped for the subject one.")

(defmacro vm-sort-test--with-folder (&rest body)
  "Visit the sorting fixture and run BODY in the folder buffer."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-sort" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-show-threads nil)
               (vm-move-messages-physically nil))
           (write-region vm-sort-test--folder nil folder nil 'quiet)
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

(defun vm-sort-test--subjects ()
  "The subjects of the folder, in the order the folder holds them."
  (mapcar #'vm-su-subject vm-message-list))

(defun vm-sort-test--authors ()
  "The authors of the folder, in the order the folder holds them."
  (mapcar #'vm-su-from vm-message-list))

(ert-deftest vm-sort-test-by-author ()
  "Sorting by author orders by the From address, and `reversed-author'
undoes it.  The subjects say which order came out, and the author order is
not the subject order."
  (vm-sort-test--with-folder
    (vm-sort-messages "author")
    (should (equal (vm-sort-test--subjects) '("cherries" "apples" "bananas")))
    (vm-sort-messages "reversed-author")
    (should (equal (vm-sort-test--subjects) '("bananas" "apples" "cherries")))))

(ert-deftest vm-sort-test-by-date ()
  "Sorting by date orders by when the message was sent, oldest first.
Neither that nor its reverse is the subject order, so a comparator reading
the wrong field cannot give this answer."
  (vm-sort-test--with-folder
    (vm-sort-messages "date")
    (should (equal (vm-sort-test--subjects) '("bananas" "cherries" "apples")))
    (vm-sort-messages "reversed-date")
    (should (equal (vm-sort-test--subjects) '("apples" "cherries" "bananas")))))

(ert-deftest vm-sort-test-by-subject ()
  "Sorting by subject is alphabetical on the subject as sorting sees it."
  (vm-sort-test--with-folder
    (vm-sort-messages "subject")
    (should (equal (vm-sort-test--subjects) '("apples" "bananas" "cherries")))
    (vm-sort-messages "reversed-subject")
    (should (equal (vm-sort-test--subjects) '("cherries" "bananas" "apples")))))

(ert-deftest vm-sort-test-by-size ()
  "Sorting by byte-count puts the shortest first and the longest last, an
order that is neither the date's nor the subject's."
  (vm-sort-test--with-folder
    (vm-sort-messages "byte-count")
    (should (equal (vm-sort-test--subjects) '("apples" "cherries" "bananas")))
    (vm-sort-messages "reversed-byte-count")
    (should (equal (vm-sort-test--subjects) '("bananas" "cherries" "apples")))
    (vm-sort-messages "line-count")
    (should (equal (car (vm-sort-test--subjects)) "apples"))
    (should (equal (car (last (vm-sort-test--subjects))) "bananas"))))

(ert-deftest vm-sort-test-physical-order-is-the-order-in-the-file ()
  "`physical-order' is the order the messages sit in the folder file, which
is what an unsorted folder shows and what sorting by it restores."
  (vm-sort-test--with-folder
    (let ((original (vm-sort-test--subjects)))
      (should (equal original '("cherries" "bananas" "apples")))
      (vm-sort-messages "author")
      (should-not (equal (vm-sort-test--subjects) original))
      (vm-sort-messages "physical-order")
      (should (equal (vm-sort-test--subjects) original))
      (vm-sort-messages "reversed-physical-order")
      (should (equal (vm-sort-test--subjects) (reverse original))))))

(ert-deftest vm-sort-test-several-keys-in-order ()
  "Several keys are tried in turn: the first decides, and a later one only
breaks a tie.  Sorting by a key every message shares leaves the second key
to do the work."
  (vm-sort-test--with-folder
    ;; every message has a different subject, so subject alone decides
    (vm-sort-messages "subject author")
    (should (equal (vm-sort-test--subjects) '("apples" "bananas" "cherries")))
    ;; every message is to the same address, so recipients decides nothing
    ;; and author breaks every tie
    (vm-sort-messages "recipients author")
    (should (equal (vm-sort-test--authors)
                   '("alice@example.com" "bob@example.com"
                     "carol@example.com")))))

(ert-deftest vm-sort-test-sorting-does-not-move-messages-in-the-file ()
  "Sorting changes the order VM shows, not the order on disk.
`vm-move-messages-physically' is what would change the file, and it is off
here; the folder must be left holding what it held."
  (vm-sort-test--with-folder
    (let ((file (buffer-file-name)))
      (vm-sort-messages "reversed-author")
      (should (equal (vm-sort-test--authors)
                     '("carol@example.com" "bob@example.com"
                       "alice@example.com")))
      (let ((on-disk (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
        ;; alice is still the first message in the file
        (should (string-match-p "\\`From alice@example\\.com" on-disk))))))

(provide 'vm-sort-test)

;;; vm-sort-test.el ends here
