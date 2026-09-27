;;; vm-virtual-test.el --- Tests for vm-virtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM virtual folder functions in vm-virtual.el

;;; Code:

(require 'vm-test-init)
(require 'vm-virtual)

;;; vm-vs-any tests

(ert-deftest vm-virtual-test-vs-any ()
  "Test vm-vs-any always returns t."
  (should (eq (vm-vs-any nil) t))
  (should (eq (vm-vs-any 'anything) t)))

;;; vm-vs-not tests (with mock)

(ert-deftest vm-virtual-test-vs-not-negates ()
  "Test vm-vs-not negates selector result."
  ;; not any = nil (any always returns t)
  (should (null (vm-vs-not nil '(any)))))

;;; Selector function existence tests

(ert-deftest vm-virtual-test-selectors-exist ()
  "Test that virtual selector functions exist."
  (should (fboundp 'vm-vs-or))
  (should (fboundp 'vm-vs-and))
  (should (fboundp 'vm-vs-not))
  (should (fboundp 'vm-vs-any))
  (should (fboundp 'vm-vs-author))
  (should (fboundp 'vm-vs-recipient))
  (should (fboundp 'vm-vs-subject))
  (should (fboundp 'vm-vs-sent-before))
  (should (fboundp 'vm-vs-sent-after))
  (should (fboundp 'vm-vs-older-than))
  (should (fboundp 'vm-vs-newer-than))
  (should (fboundp 'vm-vs-outgoing))
  (should (fboundp 'vm-vs-attachment))
  (should (fboundp 'vm-vs-header))
  (should (fboundp 'vm-vs-text))
  (should (fboundp 'vm-vs-header-or-text)))

;;; vm-virtual-selector-function-alist tests

(ert-deftest vm-virtual-test-selector-alist-populated ()
  "Test that vm-virtual-selector-function-alist has entries."
  (should (assq 'and vm-virtual-selector-function-alist))
  (should (assq 'or vm-virtual-selector-function-alist))
  (should (assq 'not vm-virtual-selector-function-alist))
  (should (assq 'any vm-virtual-selector-function-alist))
  (should (assq 'author vm-virtual-selector-function-alist))
  (should (assq 'subject vm-virtual-selector-function-alist))
  (should (assq 'recipient vm-virtual-selector-function-alist)))

;;; Virtual folder creation function existence

(ert-deftest vm-virtual-test-creation-functions-exist ()
  "Test that virtual folder creation functions exist."
  (should (fboundp 'vm-create-virtual-folder))
  (should (fboundp 'vm-create-virtual-folder-same-subject))
  (should (fboundp 'vm-create-virtual-folder-same-author))
  (should (fboundp 'vm-apply-virtual-folder)))

;;; vm-vs-sexp tests
;; Note: vm-vs-sexp passes expression to vm-vs-and without apply,
;; so test using apply directly for correct selector format.

(ert-deftest vm-virtual-test-vs-and-with-apply ()
  "Test vm-vs-and with correct selector format using apply."
  ;; (any) selector should return t
  (should (apply 'vm-vs-and nil '((any)))))

;;; vm-vs-or tests

(ert-deftest vm-virtual-test-vs-or-any-true ()
  "Test vm-vs-or returns true when any selector matches."
  ;; or with any should return t
  (should (vm-vs-or nil '(any))))

(ert-deftest vm-virtual-test-vs-or-multiple ()
  "Test vm-vs-or with multiple selectors."
  ;; First selector (not any) fails, second (any) succeeds
  (should (vm-vs-or nil '(not (any)) '(any))))

;;; vm-vs-and tests

(ert-deftest vm-virtual-test-vs-and-all-true ()
  "Test vm-vs-and returns true when all selectors match."
  ;; and with any should return t
  (should (vm-vs-and nil '(any))))

(ert-deftest vm-virtual-test-vs-and-one-false ()
  "Test vm-vs-and returns nil when any selector fails."
  ;; and with any and (not any) should return nil
  (should-not (vm-vs-and nil '(any) '(not (any)))))

(ert-deftest vm-virtual-test-vs-and-multiple-true ()
  "Test vm-vs-and with multiple passing selectors."
  ;; Multiple any selectors should all pass
  (should (vm-vs-and nil '(any) '(any) '(any))))

;;; vm-vs-not with vm-vs-and/vm-vs-or

(ert-deftest vm-virtual-test-vs-not-with-or ()
  "Test vm-vs-not negates vm-vs-or result."
  ;; not (or any) = not t = nil
  (should-not (vm-vs-not nil '(or (any)))))

;;; Integration tests using real messages

(defconst vm-virtual-test-folder
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: John Doe <john@example.com>
To: recipient@example.com
Subject: Test Virtual Message
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <virtual-test@example.com>
X-Label: important

This is the test message body.
It has multiple lines.

"
  "Test folder for virtual selector tests.")

;;; Flag selector tests with real messages

(ert-deftest vm-virtual-test-vs-new ()
  "Test vm-vs-new selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-new-flag-of msg t)
      (should (vm-vs-new msg))
      (vm-set-new-flag-of msg nil)
      (should-not (vm-vs-new msg)))))

(ert-deftest vm-virtual-test-vs-unread ()
  "Test vm-vs-unread selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-unread-flag-of msg t)
      (should (vm-vs-unread msg))
      (vm-set-unread-flag-of msg nil)
      (should-not (vm-vs-unread msg)))))

(ert-deftest vm-virtual-test-vs-read ()
  "Test vm-vs-read selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-new-flag-of msg nil)
      (vm-set-unread-flag-of msg nil)
      (should (vm-vs-read msg))
      (vm-set-new-flag-of msg t)
      (should-not (vm-vs-read msg)))))

(ert-deftest vm-virtual-test-vs-deleted ()
  "Test vm-vs-deleted selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-deleted-flag-of msg t)
      (should (vm-vs-deleted msg))
      (vm-set-deleted-flag-of msg nil)
      (should-not (vm-vs-deleted msg)))))

(ert-deftest vm-virtual-test-vs-undeleted ()
  "Test vm-vs-undeleted selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-deleted-flag-of msg nil)
      (should (vm-vs-undeleted msg))
      (vm-set-deleted-flag-of msg t)
      (should-not (vm-vs-undeleted msg)))))

(ert-deftest vm-virtual-test-vs-replied ()
  "Test vm-vs-replied selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-replied-flag-of msg t)
      (should (vm-vs-replied msg))
      (vm-set-replied-flag-of msg nil)
      (should-not (vm-vs-replied msg)))))

(ert-deftest vm-virtual-test-vs-unreplied ()
  "Test vm-vs-unreplied selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-replied-flag-of msg nil)
      (should (vm-vs-unreplied msg)))))

(ert-deftest vm-virtual-test-vs-forwarded ()
  "Test vm-vs-forwarded selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-forwarded-flag-of msg t)
      (should (vm-vs-forwarded msg))
      (vm-set-forwarded-flag-of msg nil)
      (should-not (vm-vs-forwarded msg)))))

(ert-deftest vm-virtual-test-vs-filed ()
  "Test vm-vs-filed selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-filed-flag-of msg t)
      (should (vm-vs-filed msg)))))

(ert-deftest vm-virtual-test-vs-unfiled ()
  "Test vm-vs-unfiled selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-filed-flag-of msg nil)
      (should (vm-vs-unfiled msg)))))

(ert-deftest vm-virtual-test-vs-written ()
  "Test vm-vs-written selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-written-flag-of msg t)
      (should (vm-vs-written msg)))))

(ert-deftest vm-virtual-test-vs-unwritten ()
  "Test vm-vs-unwritten selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-written-flag-of msg nil)
      (should (vm-vs-unwritten msg)))))

(ert-deftest vm-virtual-test-vs-flagged ()
  "Test vm-vs-flagged selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-flagged-flag-of msg t)
      (should (vm-vs-flagged msg)))))

(ert-deftest vm-virtual-test-vs-unflagged ()
  "Test vm-vs-unflagged selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-flagged-flag-of msg nil)
      (should (vm-vs-unflagged msg)))))

(ert-deftest vm-virtual-test-vs-marked ()
  "Test vm-vs-marked selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg t)
      (should (vm-vs-marked msg)))))

(ert-deftest vm-virtual-test-vs-unmarked ()
  "Test vm-vs-unmarked selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg nil)
      (should (vm-vs-unmarked msg)))))

(ert-deftest vm-virtual-test-vs-edited ()
  "Test vm-vs-edited selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      ;; edited flag is at index 7
      (aset (vm-attributes-of msg) 7 t)
      (should (vm-vs-edited msg)))))

(ert-deftest vm-virtual-test-vs-unedited ()
  "Test vm-vs-unedited selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (aset (vm-attributes-of msg) 7 nil)
      (should (vm-vs-unedited msg)))))

(ert-deftest vm-virtual-test-vs-redistributed ()
  "Test vm-vs-redistributed selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-redistributed-flag-of msg t)
      (should (vm-vs-redistributed msg)))))

(ert-deftest vm-virtual-test-vs-unredistributed ()
  "Test vm-vs-unredistributed selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-redistributed-flag-of msg nil)
      (should (vm-vs-unredistributed msg)))))

(ert-deftest vm-virtual-test-vs-unforwarded ()
  "Test vm-vs-unforwarded selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-forwarded-flag-of msg nil)
      (should (vm-vs-unforwarded msg)))))

;;; Header matching selector tests

(ert-deftest vm-virtual-test-vs-author ()
  "Test vm-vs-author selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-author msg "john"))
      (should (vm-vs-author msg "John"))
      (should-not (vm-vs-author msg "notfound")))))

(ert-deftest vm-virtual-test-vs-recipient ()
  "Test vm-vs-recipient selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-recipient msg "recipient"))
      (should-not (vm-vs-recipient msg "notfound")))))

(ert-deftest vm-virtual-test-vs-subject ()
  "Test vm-vs-subject selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-subject msg "Virtual"))
      (should (vm-vs-subject msg "Test"))
      (should-not (vm-vs-subject msg "notfound")))))

(ert-deftest vm-virtual-test-vs-message-id ()
  "Test vm-vs-message-id selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-message-id msg "virtual-test"))
      (should-not (vm-vs-message-id msg "notfound")))))

;;; Text selector tests

(ert-deftest vm-virtual-test-vs-text ()
  "Test vm-vs-text selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      ;; Make sure text boundaries are set
      (vm-find-and-set-text-of msg)
      (should (vm-vs-text msg "multiple lines"))
      (should-not (vm-vs-text msg "notfoundtext")))))

(ert-deftest vm-virtual-test-vs-header ()
  "Test vm-vs-header selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-header msg "X-Label"))
      (should (vm-vs-header msg "important"))
      (should-not (vm-vs-header msg "notinheader")))))

(ert-deftest vm-virtual-test-vs-header-or-text ()
  "Test vm-vs-header-or-text selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (vm-find-and-set-text-of msg)
      ;; Should find in header
      (should (vm-vs-header-or-text msg "X-Label"))
      ;; Should find in text
      (should (vm-vs-header-or-text msg "multiple lines"))
      (should-not (vm-vs-header-or-text msg "notfoundanywhere")))))

;;; Size selector tests

(ert-deftest vm-virtual-test-vs-more-chars-than ()
  "Test vm-vs-more-chars-than selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-more-chars-than msg 10))
      (should-not (vm-vs-more-chars-than msg 10000)))))

(ert-deftest vm-virtual-test-vs-less-chars-than ()
  "Test vm-vs-less-chars-than selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-less-chars-than msg 10000))
      (should-not (vm-vs-less-chars-than msg 10)))))

(ert-deftest vm-virtual-test-vs-more-lines-than ()
  "Test vm-vs-more-lines-than selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-more-lines-than msg 1))
      (should-not (vm-vs-more-lines-than msg 100)))))

(ert-deftest vm-virtual-test-vs-less-lines-than ()
  "Test vm-vs-less-lines-than selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      (should (vm-vs-less-lines-than msg 100))
      (should-not (vm-vs-less-lines-than msg 1)))))

;;; Label selector tests

(ert-deftest vm-virtual-test-vs-label ()
  "Test vm-vs-label selector."
  (vm-test-with-folder vm-virtual-test-folder
    (let ((msg (car vm-message-list)))
      ;; Set some labels
      (vm-set-labels-of msg '("work" "urgent"))
      (should (vm-vs-label msg "work"))
      (should (vm-vs-label msg "urgent"))
      (should-not (vm-vs-label msg "personal")))))


;;; a virtual folder's own summary format (issue #107)

(defun vm-virtual-test--write-folder (file n)
  "Write a folder of N messages, each referencing the one before it, to FILE."
  (with-temp-file file
    (dotimes (i n)
      (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
              "From: alice@example.com\n"
              (format "Subject: subject %d\n" i)
              (format "Message-ID: <virt-%d@example.com>\n" i)
              (if (> i 0)
                  (format "References: <virt-%d@example.com>\n" (1- i))
                "")
              "\n"
              (format "Body %d.\n\n" i)))))

(defmacro vm-virtual-test--with-folders (spec &rest body)
  "Visit a generated real folder and two virtual folders over it, run BODY.
SPEC is (REAL-BUF-VAR VIRT-A-VAR VIRT-B-VAR &optional N).  Everything the visits
created is killed afterwards."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-virtual" t)))
          (file (expand-file-name "real-folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-virtual-folder-alist nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list))
          ,(car spec) ,(nth 1 spec) ,(nth 2 spec))
     (require 'vm)
     (unwind-protect
         (progn
           (vm-virtual-test--write-folder file ,(or (nth 3 spec) 3))
           (setq vm-virtual-folder-alist
                 (list (list "virt-a" (list (list file) '(any)))
                       (list "virt-b" (list (list file) '(any)))))
           (vm-visit-folder file)
           (setq ,(car spec) (current-buffer))
           (vm-visit-virtual-folder "virt-a")
           (setq ,(nth 1 spec) (current-buffer))
           (vm-visit-virtual-folder "virt-b")
           (setq ,(nth 2 spec) (current-buffer))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-virtual-test--summary (buffer)
  "Return the text of BUFFER's summary."
  (with-current-buffer buffer
    (with-current-buffer vm-summary-buffer
      (buffer-string))))

(ert-deftest vm-virtual-test-summary-format-is-per-folder ()
  "REGRESSION: a virtual folder summarizes with its own `vm-summary-format'.
Issue #107.  `vm-summary-format' is buffer-local, but the cached summary line
lives in the message's cached data, which a mirrored virtual message shares with
its real message -- so whichever folder summarized first decided the line for all
of them.  `vm-su-decoded-tokenized-summary' would only look at a virtual
message's own summary slot when the message had no virtual mirrors, which for a
mirrored virtual message is never: the mirror data it shares lists the virtual
copies, itself among them.

Two virtual folders over one real folder, each with its own format, is the case
that cannot be faked by any amount of cache invalidation."
  ;; Each format these folders use is compiled into the shared memo, keyed by
  ;; the format string, so the entries must not outlive the test.
  (let ((vm-summary-tokenized-compiled-format-alist
         vm-summary-tokenized-compiled-format-alist))
   (vm-virtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer real
      (setq-local vm-summary-format "REAL %s\n")
      (vm-fix-my-summary))
    (with-current-buffer virt-a
      (setq-local vm-summary-format "AAA %s\n")
      (vm-fix-my-summary))
    (with-current-buffer virt-b
      (setq-local vm-summary-format "BBB %s\n")
      (vm-fix-my-summary))
    (let ((sr (vm-virtual-test--summary real))
          (sa (vm-virtual-test--summary virt-a))
          (sb (vm-virtual-test--summary virt-b)))
      (should (string-match-p "REAL subject 0" sr))
      (should (string-match-p "AAA subject 0" sa))
      (should (string-match-p "BBB subject 0" sb))
      ;; And no folder is showing another's lines.
      (should-not (string-match-p "AAA\\|BBB" sr))
      (should-not (string-match-p "REAL\\|BBB" sa))
      (should-not (string-match-p "REAL\\|AAA" sb))))))

(ert-deftest vm-virtual-test-summary-stored-on-the-virtual-message ()
  "A virtual message's summary line is kept on the virtual message.
The representation behind the test above: the line belongs in the virtual
message's own soft data, not in the cached data it shares with the real one."
  (let ((vm-summary-tokenized-compiled-format-alist
         vm-summary-tokenized-compiled-format-alist))
   (vm-virtual-test--with-folders (real virt-a virt-b)
    (ignore virt-b)
    (with-current-buffer virt-a
      (setq-local vm-summary-format "AAA %s\n")
      (vm-fix-my-summary)
      (let* ((vm (car vm-message-list))
             (rm (vm-real-message-of vm)))
        (should (vm-virtual-message-p vm))
        ;; The shared-cache condition that used to send this down the wrong
        ;; branch still holds -- the fix is not to change the sharing.
        (should (vm-virtual-messages-of vm))
        (should (eq (vm-cached-data-of vm) (vm-cached-data-of rm)))
        (should-not (eq (vm-softdata-of vm) (vm-softdata-of rm)))
        ;; The summary is on the virtual message.
        (should (vm-virtual-summary-of vm)))))))

(ert-deftest vm-virtual-test-summary-survives-folder-operations ()
  "Virtual folder summaries survive the operations that broke this in 2012.
This fix was made once before, in bzr rev 1430, and reverted in 1435 for
\"causing errors for virtual folder summaries\".  The errors were not described,
so these are the paths worth being sure of: an attribute change in the real
folder, a deletion in a virtual folder, threading in a virtual folder,
re-summarizing the real folder afterwards, and quitting the real folder with
virtual folders open."
  (vm-virtual-test--with-folders (real virt-a virt-b 4)
    (with-current-buffer real
      (vm-set-new-flag (car vm-message-list) nil)
      (vm-update-summary-and-mode-line))
    (with-current-buffer virt-a (vm-update-summary-and-mode-line))
    (with-current-buffer virt-b (vm-update-summary-and-mode-line))
    (with-current-buffer virt-a
      (vm-delete-message 1)
      (vm-update-summary-and-mode-line)
      (let ((vm-summary-show-threads t))
        (vm-build-threads vm-message-list)
        (vm-do-summary))
      (should (vm-virtual-test--summary virt-a)))
    (with-current-buffer real
      (vm-fix-my-summary)
      (should (vm-virtual-test--summary real))
      (let ((vm-confirm-quit nil))
        (vm-quit-no-change)))))

;;; Reverse links across folders (issue #453)

;; Every folder has its own message list, so a message appearing in a real
;; folder and in two virtual folders is three message objects with three
;; different links.  `vm-expunge-folder' expunges all three, each in its own
;; buffer, and `vm-expunge-message' reads the link of whichever message it was
;; given to decide which cons to splice.  Getting the wrong list's link would
;; splice the wrong folder.

(ert-deftest vm-virtual-test-reverse-links-are-per-folder ()
  "The real folder and each virtual folder over it are linked independently.
The virtual messages are copies of the real ones, so a link shared between a
copy and its original would make one folder's list describe another's."
  (vm-virtual-test--with-folders (real virt-a virt-b 4)
    (dolist (buffer (list real virt-a virt-b))
      (with-current-buffer buffer
        (should (= 4 (length vm-message-list)))
        (should (vm-test-reverse-links-consistent-p))))
    ;; Same message, three folders, three distinct links.
    (let ((links (mapcar (lambda (buffer)
                           (with-current-buffer buffer
                             (vm-reverse-link-of (vm-test-nth-message 1))))
                         (list real virt-a virt-b))))
      (should (cl-every #'consp links))
      (should (= 3 (length (delete-dups (copy-sequence links))))))))

(ert-deftest vm-virtual-test-expunging-relinks-every-folder ()
  "Expunging through a real folder leaves every virtual folder linked too.
`vm-expunge-folder' walks into each virtual folder's buffer to expunge the
mirror, so all three lists are spliced in one pass.  Issue #453."
  (vm-virtual-test--with-folders (real virt-a virt-b 4)
    (with-current-buffer real
      (vm-set-deleted-flag (vm-test-nth-message 1) t)
      (vm-expunge-folder))
    (dolist (buffer (list real virt-a virt-b))
      (with-current-buffer buffer
        (should (= 3 (length vm-message-list)))
        (should (vm-test-reverse-links-consistent-p))
        (should (null (vm-reverse-link-of (vm-test-first-message))))))
    ;; The surviving messages are the right ones, in every folder.
    (dolist (buffer (list real virt-a virt-b))
      (with-current-buffer buffer
        (should (equal '("subject 0" "subject 2" "subject 3")
                       (mapcar #'vm-su-subject vm-message-list)))))))

(ert-deftest vm-virtual-test-expunging-the-virtual-head-relinks-the-real-folder ()
  "Expunging the first message of a virtual folder relinks both lists.
The head is the case that must end with no link at all, and here two lists have
to arrive there at once."
  (vm-virtual-test--with-folders (real virt-a virt-b 4)
    (with-current-buffer virt-a
      (vm-set-deleted-flag (vm-test-first-message) t)
      (vm-expunge-folder))
    (dolist (buffer (list real virt-a virt-b))
      (with-current-buffer buffer
        (should (vm-test-reverse-links-consistent-p))
        (should (null (vm-reverse-link-of (vm-test-first-message))))))))

;;; Killing a real folder takes its virtual folders with it (issue #573)

;; A virtual message keeps its text in the real folder's buffer, so a virtual
;; folder whose real folder has been killed cannot do much of anything with what
;; it lists.  It cannot expunge, and an expunge allowed to finish would drop the
;; message from the virtual folder while its text stayed in a file nobody has
;; open.  So the virtual folders are quit when the real folder buffer is killed,
;; after asking if any of them has changes to lose.
;;
;; The prompt is skipped when `noninteractive', there being nobody to answer it,
;; so a test that wants to reach it has to bind that to nil.

(defvar vm-virtual-test--prompt nil
  "Prompt of the last question `vm-virtual-test--answering' saw.
Bound by that macro, so it is read inside its body and leaks nothing.")

(defmacro vm-virtual-test--answering (answer &rest body)
  "Run BODY with `y-or-n-p' answering ANSWER, recording the prompt.
The prompt of the last question asked is in `vm-virtual-test--prompt' within
BODY, nil if none was asked.  `noninteractive' is bound to nil because the
query skips itself in batch, there being nobody to answer."
  (declare (indent 1) (debug t))
  `(let ((noninteractive nil)
         (vm-virtual-test--prompt nil))
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt) (setq vm-virtual-test--prompt prompt) ,answer)))
       ,@body)))

(ert-deftest vm-virtual-test-killing-the-real-folder-kills-the-virtual-ones ()
  "Killing a real folder buffer quits the virtual folders over it.
Issue #573.  With nothing to lose there is no question about it."
  (vm-virtual-test--with-folders (real virt-a virt-b 3)
    (vm-virtual-test--answering t
      (dolist (buffer (list real virt-a virt-b))
        (with-current-buffer buffer (set-buffer-modified-p nil)))
      (kill-buffer real)
      (should-not vm-virtual-test--prompt))
    (should-not (buffer-live-p real))
    (should-not (buffer-live-p virt-a))
    (should-not (buffer-live-p virt-b))))

(ert-deftest vm-virtual-test-killing-the-real-folder-asks-about-changes ()
  "A virtual folder with changes is named in a question before it is killed.
Issue #573.  Deleting a message in the virtual folder marks it modified, and
those changes go when it does."
  (vm-virtual-test--with-folders (real virt-a virt-b 3)
    (with-current-buffer virt-a
      (vm-set-deleted-flag (vm-test-first-message) t))
    (should (buffer-modified-p virt-a))
    ;; the name has to be taken before the kill, which is what takes it away
    (let ((name (buffer-name virt-a)))
      (vm-virtual-test--answering t
        (kill-buffer real)
        (should vm-virtual-test--prompt)
        (should (string-match-p (regexp-quote name) vm-virtual-test--prompt))
        (should (string-match-p "unsaved changes" vm-virtual-test--prompt))))
    (should-not (buffer-live-p real))
    (should-not (buffer-live-p virt-a))))

(ert-deftest vm-virtual-test-refusing-keeps-the-real-and-virtual-folders ()
  "Answering no to that question leaves every folder alone.
Issue #573.  The question is on `kill-buffer-query-functions' rather than
`kill-buffer-hook' precisely so that the answer can still stop the kill."
  (vm-virtual-test--with-folders (real virt-a virt-b 3)
    (with-current-buffer virt-a
      (vm-set-deleted-flag (vm-test-first-message) t))
    (vm-virtual-test--answering nil
      (kill-buffer real)
      (should vm-virtual-test--prompt))
    (should (buffer-live-p real))
    (should (buffer-live-p virt-a))
    (should (buffer-live-p virt-b))))

(ert-deftest vm-virtual-test-killing-the-real-folder-deregisters-mirrors ()
  "The virtual folders are quit, not merely killed, so their mirrors go too.
Issue #573.  A killed virtual folder leaves its messages registered on the real
messages, which is what #571 had to guard against; quitting deregisters them.
Asserted on a second real folder, this one's own messages being gone with it."
  (vm-virtual-test--with-folders (real virt-a virt-b 3)
    (let ((message (with-current-buffer real (vm-test-first-message))))
      (should (= 2 (length (vm-virtual-messages-of message))))
      (vm-virtual-test--answering t (kill-buffer real))
      (should (null (vm-virtual-messages-of message))))))

(ert-deftest vm-virtual-test-killing-a-virtual-folder-asks-nothing ()
  "Killing a virtual folder itself is unaffected: no question, nothing else dies.
The query and the kill are both on the real folder's hooks, and a virtual folder
carries no other folder's messages."
  (vm-virtual-test--with-folders (real virt-a virt-b 3)
    (with-current-buffer virt-a
      (vm-set-deleted-flag (vm-test-first-message) t))
    (vm-virtual-test--answering t
      (with-current-buffer virt-a (set-buffer-modified-p nil))
      (kill-buffer virt-a)
      (should-not vm-virtual-test--prompt))
    (should-not (buffer-live-p virt-a))
    (should (buffer-live-p real))
    (should (buffer-live-p virt-b))))

(ert-deftest vm-virtual-test-folder-alist-type-accepts-a-real-definition ()
  "REGRESSION: the `:type' accepts the structure the documentation shows.
Issue #582.  A definition is a name followed by one or more clauses, and a
clause is a folder list followed by one or more selectors.  The type described
one clause of one selector, so Customize rejected the two-clause example in this
variable's own docstring and in the manual.  The default matched, which is why
the check for #575 did not see it."
  (require 'wid-edit)
  (let ((type (widget-convert (get 'vm-virtual-folder-alist 'custom-type))))
    (should (widget-apply type :match nil))
    ;; the example from the docstring and the manual
    (should (widget-apply
             type :match
             '(("virtual-folder-name"
                (("/path/to/folder" "/path/to/folder2")
                 (header "foo") (header "bar"))
                (("/path/to/folder3" "/path/to/folder4")
                 (and (header "baz") (header "woof")))))))
    ;; one clause, one selector: what these tests build
    (should (widget-apply type :match '(("test-virt" (("/tmp/f") (any))))))
    ;; two virtual folders
    (should (widget-apply type :match '(("a" (("/f") (any))) ("b" (("/g") (any))))))))

(ert-deftest vm-virtual-test-combinators-print-diagnostics ()
  "REGRESSION: `vm-virtual-check-diagnostics\' reaches the combinators.
Issue #584.  `vm-vs-and\', `vm-vs-or\' and `vm-vs-not\' were defined twice, plainly
in vm-virtual.el and with the diagnostics in vm-avirtual.el, and the plain pair
won: vm-summary.el pulls vm-avirtual in through vm-summary-faces.el and vm.el
requires vm-virtual afterwards.  So `vm-virtual-check-selector-interactive\' with
a prefix argument printed a line for each leaf selector and nothing for the
combinators, which is where its indentation and its order of evaluation would
have been worth reading."
  (require 'vm-avirtual)
  (let* ((vm-virtual-check-diagnostics t)
         (vm-virtual-check-level 0)
         (vm-virtual-selector-function-alist
          (append (list (cons 'yes (lambda (_m) t))
                        (cons 'no (lambda (_m) nil)))
                  vm-virtual-selector-function-alist)))
    (should (equal "  and: t (yes)\n"
                   (with-output-to-string (vm-vs-and nil '(yes)))))
    (should (equal "  or: nil (no)\n"
                   (with-output-to-string (vm-vs-or nil '(no)))))
    (should (equal "  not: t (no)\n"
                   (with-output-to-string (vm-vs-not nil '(no)))))
    ;; nesting shows as nesting
    (should (equal "    or: t (yes)\n  and: t (or ((yes)))\n"
                   (with-output-to-string (vm-vs-and nil '(or (yes))))))))

(ert-deftest vm-virtual-test-combinators-honour-case-folding ()
  "The combinators bind `case-fold-search\' from the option that names them.
Issue #584.  `vm-virtual-check-case-fold-search\' was read only by the copies in
vm-avirtual.el, which lost, so the option did nothing for a folder's selectors."
  (require 'vm-avirtual)
  (let* ((seen 'unset)
         (vm-virtual-selector-function-alist
          (list (cons 'peek (lambda (_m) (setq seen case-fold-search) t)))))
    (let ((vm-virtual-check-case-fold-search t) (case-fold-search nil))
      (vm-vs-and nil '(peek))
      (should (eq t seen)))
    (let ((vm-virtual-check-case-fold-search nil) (case-fold-search t))
      (vm-vs-and nil '(peek))
      (should (eq nil seen)))))

;;; The status letters in a virtual folder's summary (emacs-vm/vm#623)

(defun vm-virtual-test--summary-flags (summary-buffer)
  "The attribute characters of the first summary line in SUMMARY-BUFFER.
The default format puts them after the message number, so this reads the line
the way a person does."
  (with-current-buffer summary-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (point) (line-end-position))))
        (should (string-match "\\`..[ 0-9]+ \\(.\\{1,4\\}?\\) [^ ]" line))
        (match-string 1 line)))))

(ert-deftest vm-virtual-test-an-operation-in-a-virtual-folder-shows-its-letter ()
  "Deleting a message in a virtual folder puts the D in its summary at once.
It appeared only after leaving the folder and entering it again: a virtual
message's summary is cached in `vm-virtual-summary-of', and the invalidation
cleared `vm-decoded-tokenized-summary-of' instead -- a different slot -- so the
line was regenerated from the copy it already had (emacs-vm/vm#623).

The FIXME beside it asked whether it tossed the cache of the virtual mirrors,
and had gone unanswered since 2012."
  (vm-virtual-test--with-folders (real virt-a _virt-b)
    (with-current-buffer virt-a
      (setq vm-message-pointer vm-message-list)
      (should-not (string-match-p "D" (vm-virtual-test--summary-flags
                                       vm-summary-buffer)))
      (vm-delete-message 1)
      (should (string-match-p "D" (vm-virtual-test--summary-flags
                                   vm-summary-buffer)))
      ;; and undeleting takes it away again
      (vm-undelete-message 1)
      (should-not (string-match-p "D" (vm-virtual-test--summary-flags
                                       vm-summary-buffer))))
    ;; the real folder's own summary followed along, as it always did
    (with-current-buffer real
      (should-not (string-match-p "D" (vm-virtual-test--summary-flags
                                       vm-summary-buffer))))))

(ert-deftest vm-virtual-test-an-operation-in-the-real-folder-reaches-the-virtual ()
  "Deleting in the real folder puts the D in the virtual folder's summary too.
The same cache, reached from the other side: the real message's invalidation
said it tossed the cache of every virtual message mirroring it, and did not."
  (vm-virtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer real
      (setq vm-message-pointer vm-message-list)
      (vm-delete-message 1))
    (with-current-buffer virt-a
      (should (string-match-p "D" (vm-virtual-test--summary-flags
                                   vm-summary-buffer))))
    ;; and every virtual folder over it, not just the first
    (with-current-buffer virt-b
      (should (string-match-p "D" (vm-virtual-test--summary-flags
                                   vm-summary-buffer))))))

(ert-deftest vm-virtual-test-a-forwarded-flag-shows-in-a-virtual-folder ()
  "Not only deletion: any attribute that shows in the summary shows at once.
Göran reported the Z of a forward as well as the D of a delete."
  (vm-virtual-test--with-folders (_real virt-a _virt-b)
    (with-current-buffer virt-a
      (setq vm-message-pointer vm-message-list)
      (vm-set-forwarded-flag (car vm-message-list) t)
      (vm-update-summary-and-mode-line)
      (should (string-match-p "Z" (vm-virtual-test--summary-flags
                                   vm-summary-buffer))))))

(ert-deftest vm-virtual-test-every-attribute-letter-shows-at-once ()
  "Each attribute that has a letter in the summary gets it without a revisit.
Göran named the D of a delete and the Z of a forward; the summary has eight
such letters and they all come from the same cached line, so they all failed
the same way.  Checked here in the three columns they live in, since a letter
in one column masks the ones below it in the same `cond'."
  (vm-virtual-test--with-folders (_real virt-a _virt-b)
    (with-current-buffer virt-a
      (setq vm-message-pointer vm-message-list)
      (let ((m (car vm-message-list)))
        ;; column one: deleted beats new beats unread beats flagged
        (vm-set-new-flag m nil)
        (vm-set-unread-flag m nil)
        (vm-set-flagged-flag m t)
        (vm-update-summary-and-mode-line)
        (should (string-match-p "!" (vm-virtual-test--summary-flags
                                     vm-summary-buffer)))
        ;; column two
        (vm-set-filed-flag m t)
        (vm-update-summary-and-mode-line)
        (should (string-match-p "F" (vm-virtual-test--summary-flags
                                     vm-summary-buffer)))
        ;; column three
        (vm-set-replied-flag m t)
        (vm-update-summary-and-mode-line)
        (should (string-match-p "R" (vm-virtual-test--summary-flags
                                     vm-summary-buffer)))
        ;; column four.  Editing sets this through the accessor, there being
        ;; no vm-set-edited-flag of the kind the others have.
        (vm-set-edited-flag-of m t)
        (vm-mark-for-summary-update m)
        (vm-update-summary-and-mode-line)
        (should (string-match-p "E" (vm-virtual-test--summary-flags
                                     vm-summary-buffer)))))))

(ert-deftest vm-virtual-test-a-label-change-shows-in-a-virtual-folder ()
  "A label added in a virtual folder appears in its summary at once.
Not only the attribute letters: a summary format with %L in it -- and a virtual
folder selected by label is the reason to have one -- was equally stale, the
whole line coming from the one cache."
  (vm-virtual-test--with-folders (_real virt-a _virt-b)
    (with-current-buffer virt-a
      (let ((vm-summary-format "%n %L %s\n"))
        ;; regenerate under this format, so the label is on the line
        (vm-set-summary-redo-start-point t)
        (vm-update-summary-and-mode-line)
        (setq vm-message-pointer vm-message-list)
        (vm-add-message-labels "todo" 1)
        (with-current-buffer vm-summary-buffer
          (should (string-match-p "todo" (buffer-string))))))))

(ert-deftest vm-virtual-test-renumbering-does-not-throw-the-summary-away ()
  "`dont-kill-cache' still means what it says.
Renumbering and thread indentation pass it, because they change what is around
a summary line rather than the line itself, and rebuilding every line for them
would be waste.  The cache-clearing added for emacs-vm/vm#623 is inside that
guard, and this pins it: a marked-with-DONT-KILL-CACHE update leaves the
cached line in place."
  (vm-virtual-test--with-folders (_real virt-a _virt-b)
    (with-current-buffer virt-a
      (let* ((m (car vm-message-list))
             (cached (vm-su-summary m)))
        (should cached)
        (vm-mark-for-summary-update m t)
        (should (eq (vm-virtual-summary-of m) cached))
        ;; and without the flag it is thrown away
        (vm-mark-for-summary-update m)
        (should-not (vm-virtual-summary-of m))))))

(ert-deftest vm-virtual-test-the-cache-helper-clears-the-right-slot ()
  "`vm-discard-summary-cache-of' clears the slot its message actually uses.
The bug was one slot being cleared for a message that keeps its summary in the
other, so this is the invariant stated on its own."
  (vm-virtual-test--with-folders (real virt-a _virt-b)
    (let ((v (with-current-buffer virt-a (car vm-message-list)))
          (r (with-current-buffer real (car vm-message-list))))
      (should (vm-virtual-message-p v))
      (should-not (vm-virtual-message-p r))
      ;; fill both caches
      (vm-su-summary v)
      (vm-su-summary r)
      (should (vm-virtual-summary-of v))
      (should (vm-decoded-tokenized-summary-of r))
      (vm-discard-summary-cache-of v)
      (should-not (vm-virtual-summary-of v))
      (should (vm-decoded-tokenized-summary-of r)) ; untouched
      (vm-discard-summary-cache-of r)
      (should-not (vm-decoded-tokenized-summary-of r)))))

;;; Creating a search folder (emacs-vm/vm#627)
;;
;; The nine vm-create-*-virtual-folder commands the manual documents had no
;; test between them.  Each is a thin wrapper on `vm-create-virtual-folder'
;; with a selector, which is exactly the kind of code that goes wrong by
;; naming the wrong selector or dropping an argument, and never by failing
;; loudly.

(defconst vm-virtual-test--assorted
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: Alice Adams <alice@example.com>\nTo: list@example.org\n"
          "Subject: badgers in the garden\n\nA body about badgers.\n\n"
          "From bob@example.com Sun Aug  9 09:00:00 2026\n"
          "From: Bob Brown <bob@example.com>\nTo: alice@example.com\n"
          "Subject: the roof\n\nA body about slates.\n\n"
          "From carol@example.com Mon Aug 10 10:00:00 2026\n"
          "From: Carol Clark <carol@example.com>\nTo: bob@example.com\n"
          "Subject: badgers again\n\nMore badgers.\n\n")
  "Three messages that differ in author, recipient, subject and body.")

(defmacro vm-virtual-test--with-real-folder (spec &rest body)
  "Visit a folder of `vm-virtual-test--assorted' and run BODY.
SPEC is (FOLDER-VAR).  Every buffer the visits create is killed afterwards,
including the virtual folders BODY makes."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-virtual-create" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "real-folder" dir))
               (vm-virtual-folder-alist nil)
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-visit-when-saving nil))
           (write-region vm-virtual-test--assorted nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-virtual-test--subjects ()
  "The subjects of the folder in the current buffer, in order."
  (mapcar #'vm-su-subject vm-message-list))

(ert-deftest vm-virtual-test-an-author-search-folder-selects-by-author ()
  "`vm-create-author-virtual-folder' collects the messages from one author."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-create-author-virtual-folder "alice")
    (should (eq major-mode 'vm-virtual-mode))
    (should (equal (vm-virtual-test--subjects) '("badgers in the garden")))))

(ert-deftest vm-virtual-test-an-author-or-recipient-search-folder-takes-both ()
  "`vm-create-author-or-recipient-virtual-folder' matches either end.
Alice wrote one and received another, so the two searches differ -- which is
the whole reason this command exists beside the author one."
  (vm-virtual-test--with-real-folder (folder)
    (vm-create-author-or-recipient-virtual-folder "alice")
    (should (equal (vm-virtual-test--subjects)
                   '("badgers in the garden" "the roof")))
    (vm-visit-folder folder)
    (vm-create-author-virtual-folder "alice")
    (should (equal (vm-virtual-test--subjects) '("badgers in the garden")))))

(ert-deftest vm-virtual-test-a-subject-search-folder-selects-by-subject ()
  "`vm-create-subject-virtual-folder' matches the Subject header only.
The body of the first message mentions badgers too, which is what tells this
apart from the text search below."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-create-subject-virtual-folder "badgers")
    (should (equal (vm-virtual-test--subjects)
                   '("badgers in the garden" "badgers again")))))

(ert-deftest vm-virtual-test-a-text-search-folder-looks-in-the-body ()
  "`vm-create-text-virtual-folder' matches the text of the message.
A word in a body and in no subject finds its message, which a subject search
does not."
  (vm-virtual-test--with-real-folder (folder)
    (vm-create-text-virtual-folder "slates")
    (should (equal (vm-virtual-test--subjects) '("the roof")))
    (vm-visit-folder folder)
    (vm-create-subject-virtual-folder "slates")
    (should (equal (vm-virtual-test--subjects) nil))))

(ert-deftest vm-virtual-test-a-label-search-folder-selects-by-label ()
  "`vm-create-label-virtual-folder' collects the messages carrying a label."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-set-labels (nth 2 vm-message-list) '("todo"))
    (vm-create-label-virtual-folder "todo")
    (should (equal (vm-virtual-test--subjects) '("badgers again")))))

(ert-deftest vm-virtual-test-a-flagged-search-folder-selects-the-flagged ()
  "`vm-create-flagged-virtual-folder' takes the flagged messages and no more."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-set-flagged-flag (nth 1 vm-message-list) t)
    (vm-create-flagged-virtual-folder)
    (should (equal (vm-virtual-test--subjects) '("the roof")))))

(ert-deftest vm-virtual-test-a-new-search-folder-takes-the-new-mail ()
  "`vm-create-new-virtual-folder' collects what has not been looked at.
A message is new when the folder is visited and stops being new when it is
read, so clearing the flag on one takes it out of the search."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-set-new-flag (car vm-message-list) nil)
    (vm-create-new-virtual-folder)
    (should (equal (vm-virtual-test--subjects)
                   '("the roof" "badgers again")))))

(ert-deftest vm-virtual-test-an-unseen-search-folder-is-empty-in-fresh-mail ()
  "Unseen is unread, which is not the same as new.
A folder just visited is all new and none of it is unread, so this search finds
nothing there -- worth stating, because the two commands sit next to each other
in the manual and read as synonyms."
  (vm-virtual-test--with-real-folder (_folder)
    (should (cl-every #'vm-new-flag vm-message-list))
    (should-not (cl-some #'vm-unread-flag vm-message-list))
    (vm-create-unseen-virtual-folder)
    (should-not (vm-virtual-test--subjects))))

(ert-deftest vm-virtual-test-an-unseen-search-folder-takes-the-unread ()
  "`vm-create-unseen-virtual-folder' collects the messages marked unread."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-set-unread-flag (nth 1 vm-message-list) t)
    (vm-create-unseen-virtual-folder)
    (should (equal (vm-virtual-test--subjects) '("the roof")))))

(defmacro vm-virtual-test--at-a-fixed-day (&rest body)
  "Run BODY with the clock held at Monday 10 August 2026.
The stub is on `current-time-string', which is what `vm-vs-newer-than' reads.
Stubbing `current-time' does nothing for it -- `current-time-string' is its
own primitive and goes to the system clock -- so a test that stubbed only
that one measured the fixture's age against the real today.  This one passed
on 11 August 2026 and failed on the 12th, with an expectation calibrated to
the accident."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (date-to-time "Mon, 10 Aug 2026 12:00:00 -0700")))
             ((symbol-function 'current-time-string)
              (lambda (&rest _) "Mon Aug 10 12:00:00 2026")))
     ,@body))

(ert-deftest vm-virtual-test-a-date-search-folder-selects-by-age ()
  "`vm-create-date-virtual-folder' takes the messages of the last N days.
One day back from the stubbed Monday reaches Sunday's message and Monday's,
and leaves Saturday's out."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-virtual-test--at-a-fixed-day
      (vm-create-date-virtual-folder 1)
      (should (equal (vm-virtual-test--subjects)
                     '("the roof" "badgers again"))))))

(ert-deftest vm-virtual-test-a-date-search-folder-counts-from-the-stubbed-day ()
  "The window is measured from the stubbed day, and is inclusive.
Nought days back is Monday's message alone; two days back reaches Saturday's
as well, so all three.  These answers hold whatever the real date is.

Each search runs from the real folder: `vm-create-date-virtual-folder' works
on the current folder, so called again without going back it would search the
virtual folder it had just made."
  (vm-virtual-test--with-real-folder (_folder)
    (let ((real (current-buffer)))
      (vm-virtual-test--at-a-fixed-day
        (vm-create-date-virtual-folder 0)
        (should (equal (vm-virtual-test--subjects) '("badgers again")))
        (set-buffer real)
        (vm-create-date-virtual-folder 2)
        (should (equal (vm-virtual-test--subjects)
                       '("badgers in the garden" "the roof"
                         "badgers again")))))))

(ert-deftest vm-virtual-test-a-search-folder-can-be-read-only ()
  "The prefix argument every one of these takes makes the folder read only.
Passed through `vm-create-virtual-folder', so checking it once checks the
wrapper's argument order -- which is what a thin wrapper gets wrong."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-create-author-virtual-folder "alice" t)
    (should vm-folder-read-only)))

(ert-deftest vm-virtual-test-a-search-folder-is-named-after-its-search ()
  "The folder's name says what was searched for, since several may be open."
  (vm-virtual-test--with-real-folder (_folder)
    (vm-create-subject-virtual-folder "badgers")
    (should (string-match-p "badgers" (buffer-name)))))

;;; The virtual selectors (emacs-vm/vm#633)
;;
;; Thirty of the vm-vs-* selectors were called by no test.  They are what a
;; virtual folder definition and an interactive search are built from, and a
;; selector that answers wrongly quietly mis-files mail.

(defconst vm-virtual-test--selector-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: Alice Adams <alice@example.com>\n"
          "To: bob@example.com\nCc: carol@example.com\n"
          "Reply-To: desk@example.com\n"
          "Subject: Re: badgers\nMessage-ID: <one@example.com>\n"
          "X-Spam-Flag: YES\n\nThe first body.\n\n"
          "From dave@example.com Sat Aug  8 16:05:00 2026\n"
          "From: dave@example.com\nTo: me@example.com\n"
          "Subject: ordinary mail\nMessage-ID: <two@example.com>\n\n"
          "The second body.\n\n")
  "Two messages, the second lacking the headers the first has.")

(defmacro vm-virtual-test--with-selectors (spec &rest body)
  "Visit a folder of two messages and run BODY with them bound.
SPEC is (FIRST-VAR SECOND-VAR)."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-selectors" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil))
           (write-region vm-virtual-test--selector-folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             (let ((,(car spec) (car vm-message-list))
                   (,(cadr spec) (cadr vm-message-list)))
               ,@body)))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-virtual-test-header-field-selector-without-the-header ()
  "REGRESSION: `header-field' does not match a message that lacks the header.
Issue #633.  `vm-get-header-contents' answers nil for a header that is not
there, and that went straight to `string-match': the selector signalled
rather than declining to match, and nothing catches it -- `vm-vs-or' and
`vm-vs-and' apply the selector with no `condition-case', so the whole
virtual folder stopped being built.  Which is the case the selector exists
for: the messages you are picking out are the ones carrying the header."
  (vm-virtual-test--with-selectors (first second)
    (should (vm-vs-header-field first "X-Spam-Flag" "YES"))
    (should-not (vm-vs-header-field second "X-Spam-Flag" "YES"))
    ;; and a header that is there but does not match is still no match
    (should-not (vm-vs-header-field first "X-Spam-Flag" "NO"))))

(ert-deftest vm-virtual-test-a-virtual-folder-on-a-missing-header ()
  "REGRESSION: the folder builds, and holds the messages that have the header.
Issue #633, end to end: this is what the selector is for, and it used to
signal `wrong-type-argument stringp nil' on the first message without the
header."
  (vm-virtual-test--with-selectors (_first _second)
    (let* ((folder (buffer-file-name))
           (vm-virtual-folder-alist
            (list (list "spam" (list (list folder)
                                     '(header-field "X-Spam-Flag" "YES"))))))
      (vm-visit-virtual-folder "spam")
      (should (equal (length vm-message-list) 1))
      (should (equal (vm-su-subject (car vm-message-list)) "Re: badgers")))))

(ert-deftest vm-virtual-test-addressee-recipient-and-principal-selectors ()
  "`addressee' is the To line, `recipient' the To and the Cc, `principal'
the Reply-To.  Three different questions, and the copied-in address is what
tells the first two apart."
  (vm-virtual-test--with-selectors (first second)
    (should (vm-vs-addressee first "bob@example\\.com"))
    (should-not (vm-vs-addressee first "carol@example\\.com"))
    (should (vm-vs-recipient first "bob@example\\.com"))
    (should (vm-vs-recipient first "carol@example\\.com"))
    (should-not (vm-vs-addressee first "desk@example\\.com"))
    (should (vm-vs-principal first "desk@example\\.com"))
    (should-not (vm-vs-principal first "bob@example\\.com"))
    ;; the second message has no Reply-To, and that is not an error
    (should-not (vm-vs-principal second "desk@example\\.com"))))

(ert-deftest vm-virtual-test-sortable-subject-ignores-the-reply-prefix ()
  "`sortable-subject' matches the subject as sorting sees it.
The Re: is not part of it, which is the whole difference from `subject'."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-subject first "Re: badgers"))
    (should (vm-vs-sortable-subject first "\\`badgers\\'"))
    (should-not (vm-vs-sortable-subject first "\\`Re: badgers\\'"))))

(ert-deftest vm-virtual-test-date-selectors ()
  "`sent-before' and `sent-after' put the message on the right side of a date.
The two must disagree about any given date, or one of them is wrong."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-sent-after first "1 Jan 1990"))
    (should-not (vm-vs-sent-before first "1 Jan 1990"))
    (should (vm-vs-sent-before first "1 Jan 2050"))
    (should-not (vm-vs-sent-after first "1 Jan 2050"))))

(ert-deftest vm-virtual-test-header-and-text-selectors ()
  "`header' searches the headers, `text' the body, `header-or-text' both.
A word in the body is not in the headers, and the test would pass by
accident if the folder were searched whole."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-header first "X-Spam-Flag"))
    (should-not (vm-vs-header first "The first body"))
    (should (vm-vs-text first "The first body"))
    (should-not (vm-vs-text first "X-Spam-Flag"))
    (should (vm-vs-header-or-text first "The first body"))
    (should (vm-vs-header-or-text first "X-Spam-Flag"))))

(ert-deftest vm-virtual-test-selector-combinators ()
  "`and', `or' and `not' combine selectors, and an invalid one matches
nothing rather than being negated into a match."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-and first '(subject "badgers") '(author "alice")))
    (should-not (vm-vs-and first '(subject "badgers") '(author "nobody")))
    (should (vm-vs-or first '(subject "nothing") '(author "alice")))
    (should-not (vm-vs-or first '(subject "nothing") '(author "nobody")))
    (should (vm-vs-not first '(author "nobody")))
    (should-not (vm-vs-not first '(author "alice")))))

(defconst vm-virtual-test--thread-folder
  (concat "From alice@example.com Sat Aug  8 16:00:00 2026\n"
          "From: alice@example.com\nSubject: badgers\n"
          "Message-ID: <root@example.com>\n\nThe root.\n\n"
          "From bob@example.com Sat Aug  8 16:05:00 2026\n"
          "From: bob@example.com\nSubject: Re: badgers\n"
          "Message-ID: <reply@example.com>\n"
          "In-Reply-To: <root@example.com>\n\nThe reply.\n\n")
  "A thread of two: a root from alice and a reply from bob.")

(defmacro vm-virtual-test--with-thread-folder (spec &rest body)
  "Visit a folder holding one thread of two messages and run BODY.
SPEC is (ROOT-VAR REPLY-VAR).  Threading is on, since the thread selectors
have nothing to walk without it."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-thread-sel" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-show-threads t))
           (write-region vm-virtual-test--thread-folder nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             (vm-build-threads nil)
             (let ((,(car spec) (car vm-message-list))
                   (,(cadr spec) (cadr vm-message-list)))
               ,@body)))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-virtual-test-thread-selector-holds-for-any-of-the-thread ()
  "`thread' asks whether the selector holds for any message in the thread.
The reply is from bob, so a thread selector for alice matches it too -- that
is what makes a thread selector different from the plain one, and it is how
you pull a whole conversation into a folder."
  (vm-virtual-test--with-thread-folder (root reply)
    (should (vm-vs-thread root '(author "alice")))
    (should (vm-vs-thread reply '(author "alice")))
    (should (vm-vs-thread root '(author "bob")))
    (should-not (vm-vs-thread reply '(author "nobody")))
    ;; the plain selector still speaks only for the message it is given
    (should-not (vm-vs-author reply "alice"))))

(ert-deftest vm-virtual-test-thread-all-selector-holds-for-every-message ()
  "`thread-all' asks whether the selector holds for every message in the
thread, which is the other question and gives the other answer here: only
one of these two is from alice."
  (vm-virtual-test--with-thread-folder (root reply)
    (should-not (vm-vs-thread-all root '(author "alice")))
    (should (vm-vs-thread-all root '(subject "badgers")))
    (should (vm-vs-thread-all reply '(subject "badgers")))))

(ert-deftest vm-virtual-test-uid-and-uidl-selectors ()
  "`uid' and `uidl' compare the server's identifier for the message.
A folder read from a file has neither, and asking must answer no rather than
matching everything or signalling.

The two read the same slot -- a message comes from POP or from IMAP, not
both -- so they are one question asked in two vocabularies, and setting
either is setting the other."
  (vm-virtual-test--with-selectors (first _second)
    (should-not (vm-vs-uid first "1"))
    (should-not (vm-vs-uidl first "1"))
    (vm-set-imap-uid-of first "42")
    (should (vm-vs-uid first "42"))
    (should-not (vm-vs-uid first "43"))
    (should (vm-vs-uidl first "42"))
    (vm-set-pop-uidl-of first "abc")
    (should (vm-vs-uidl first "abc"))
    (should (vm-vs-uid first "abc"))))

(ert-deftest vm-virtual-test-folder-name-selector ()
  "`folder-name' matches the name of the folder the message is really in.
For a virtual message that is the real message's folder, not the virtual
one, which is what makes the selector usable inside a virtual folder."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-folder-name first "\\`incoming\\'"))
    (should-not (vm-vs-folder-name first "\\`nothing\\'"))))

(ert-deftest vm-virtual-test-eval-and-sexp-selectors ()
  "`eval' runs Lisp against `vm-virtual-message'; `sexp' combines selectors.
They are the escape hatches, and what they are given is the message being
checked."
  (vm-virtual-test--with-selectors (first _second)
    (should (vm-vs-eval first '(vm-vs-author vm-virtual-message "alice")))
    (should-not (vm-vs-eval first '(vm-vs-author vm-virtual-message "nobody")))
    (should (vm-vs-sexp first '(and (subject "badgers") (author "alice"))))
    (should-not (vm-vs-sexp first '(and (subject "badgers")
                                        (author "nobody"))))))

(ert-deftest vm-virtual-test-outgoing-selector ()
  "`outgoing' is a message from you, and who that is comes from
`vm-summary-uninteresting-senders'.  With nothing set there, nothing is
outgoing -- rather than everything."
  (vm-virtual-test--with-selectors (first _second)
    (let ((vm-summary-uninteresting-senders nil))
      (should-not (vm-vs-outgoing first)))
    (let ((vm-summary-uninteresting-senders "alice@example\\.com"))
      (should (vm-vs-outgoing first)))
    (let ((vm-summary-uninteresting-senders "nobody@example\\.com"))
      (should-not (vm-vs-outgoing first)))))

(ert-deftest vm-virtual-test-virtual-folder-member-selector ()
  "`virtual-folder-member' is true of a message some virtual folder is
showing.  Before any virtual folder exists, no message is a member."
  (vm-virtual-test--with-selectors (first _second)
    (should-not (vm-vs-virtual-folder-member first))
    (let* ((folder (buffer-file-name))
           (vm-virtual-folder-alist
            (list (list "everything" (list (list folder) '(any))))))
      (vm-visit-virtual-folder "everything")
      (should (vm-vs-virtual-folder-member first)))))

;;; The summary display toggles (emacs-vm/vm#632)

(ert-deftest vm-virtual-test-toggle-virtual-mirror-in-a-virtual-folder ()
  "`vm-toggle-virtual-mirror' stops a virtual folder mirroring the real one,
and starts it again.

Mirrored, deleting a message here deletes the real one.  Unmirrored, the
virtual folder keeps its own attributes and the real message is left alone --
which is the whole use of the command, marking a search up without touching
the folders it came from."
  (vm-virtual-test--with-selectors (first _second)
    (let* ((folder (buffer-file-name))
           (real (current-buffer))
           (vm-virtual-folder-alist
            (list (list "everything" (list (list folder) '(any))))))
      (vm-visit-virtual-folder "everything")
      (should (eq major-mode 'vm-virtual-mode))
      (should vm-virtual-mirror)
      ;; mirrored: deleting here deletes the real message
      (vm-set-deleted-flag (car vm-message-list) t)
      (should (vm-deleted-flag first))
      (vm-set-deleted-flag (car vm-message-list) nil)
      ;; unmirrored: it does not
      (vm-toggle-virtual-mirror)
      (should-not vm-virtual-mirror)
      (vm-set-deleted-flag (car vm-message-list) t)
      (should-not (vm-deleted-flag first))
      ;; and back
      (vm-toggle-virtual-mirror)
      (should vm-virtual-mirror)
      (should (buffer-live-p real)))))

(ert-deftest vm-virtual-test-toggle-virtual-mirror-elsewhere-is-an-error ()
  "In a folder that is not virtual the command says so.  There is nothing for
it to mirror, and quietly doing nothing would leave the user thinking the
attributes were now their own."
  (vm-virtual-test--with-selectors (_first _second)
    (let ((text-quoting-style 'grave))
      (should (equal (cadr (should-error (vm-toggle-virtual-mirror)))
                     "This is not a virtual folder.")))))

;;; Building a search folder from the current message (emacs-vm/vm#650)
;;
;; The three same-* commands compose a selector out of the current message
;; and a name out of that.  What they hand to `vm-create-virtual-folder' is
;; the whole of their behaviour, so that is what these check; creating and
;; visiting the folder is `vm-create-virtual-folder''s own business.

(defvar vm-virtual-test--created nil
  "The arguments the command under test passed to `vm-create-virtual-folder'.")

(defmacro vm-virtual-test--creating-from (headers &rest body)
  "Run BODY in a folder of one message with HEADERS, watching folder creation.
`vm-virtual-test--created' is set to the argument list of the
`vm-create-virtual-folder' call, which is not made."
  (declare (indent 1) (debug t))
  `(let ((vm-virtual-test--created nil))
     (vm-test-with-folder
         (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                 ,headers "\n" "The body.\n")
       (setq major-mode 'vm-mode)
       (cl-letf (((symbol-function 'vm-follow-summary-cursor) #'ignore)
                 ((symbol-function 'vm-create-virtual-folder)
                  (lambda (&rest args) (setq vm-virtual-test--created args))))
         ,@body))))

(defun vm-virtual-test--selector ()
  "The selector symbol of the watched `vm-create-virtual-folder' call."
  (nth 0 vm-virtual-test--created))

(defun vm-virtual-test--argument ()
  "The selector argument of the watched call."
  (nth 1 vm-virtual-test--created))

(defun vm-virtual-test--folder-name ()
  "The folder name of the watched call."
  (nth 3 vm-virtual-test--created))

(ert-deftest vm-virtual-test-same-subject-selects-the-sortable-subject ()
  "The selector is the current message's subject with the reply prefix
stripped, so the folder holds the thread rather than the one message."
  (vm-virtual-test--creating-from "From: someone@example.com\nSubject: Re: the topic\n"
    (vm-create-virtual-folder-same-subject)
    (should (eq (vm-virtual-test--selector) 'sortable-subject))
    (should (equal (vm-virtual-test--argument) (regexp-quote "the topic")))
    (should (equal (vm-virtual-test--folder-name)
                   (vm-virtual-folder-name (buffer-name) 'subject "the topic")))))

(ert-deftest vm-virtual-test-same-subject-quotes-what-it-searches-for ()
  "A subject holding regexp characters is quoted, so the folder collects
messages with that subject rather than everything the subject would match
if it were read as a regexp."
  (vm-virtual-test--creating-from
      "From: someone@example.com\nSubject: [PATCH] fix a.b (again)\n"
    (vm-create-virtual-folder-same-subject)
    (should (equal (vm-virtual-test--argument)
                   (regexp-quote "[PATCH] fix a.b (again)")))
    ;; and the quoted form really does match only the subject itself
    (should (string-match-p (vm-virtual-test--argument)
                            "[PATCH] fix a.b (again)"))
    (should-not (string-match-p (vm-virtual-test--argument)
                                "PATCH fix axb again"))))

(ert-deftest vm-virtual-test-an-empty-subject-matches-only-empty-ones ()
  "A message with no subject selects the other messages with none, rather
than every message in the folder, and says so in the folder name."
  (vm-virtual-test--creating-from "From: someone@example.com\nSubject: \n"
    (vm-create-virtual-folder-same-subject)
    (should (equal (vm-virtual-test--argument) "^$"))
    (should (equal (vm-virtual-test--folder-name)
                   (vm-virtual-folder-name (buffer-name) 'subject "\"\"")))))

(ert-deftest vm-virtual-test-same-author-selects-the-author ()
  "The author is taken from the current message and quoted."
  (vm-virtual-test--creating-from
      "From: A. Writer <writer+tag@example.com>\nSubject: a subject\n"
    (vm-create-virtual-folder-same-author)
    (should (eq (vm-virtual-test--selector) 'author))
    (should (equal (vm-virtual-test--argument)
                   (regexp-quote (vm-su-from (car vm-message-pointer)))))
    (should (string-match-p (vm-virtual-test--argument)
                            (vm-su-from (car vm-message-pointer))))))

(ert-deftest vm-virtual-test-same-recipient-takes-the-first-addressee ()
  "With several To addressees the first is used, as the docstring says, and
the selector is author-or-recipient: the folder is the correspondence with
that person, not only the mail sent to them."
  (vm-virtual-test--creating-from
      (concat "From: someone@example.com\n"
              "To: first@example.com, second@example.com\n"
              "Subject: a subject\n")
    (vm-create-virtual-folder-same-recipient)
    (should (eq (vm-virtual-test--selector) 'author-or-recipient))
    (should (equal (vm-virtual-test--argument)
                   (regexp-quote "first@example.com")))
    (should-not (string-match-p (vm-virtual-test--argument)
                                "second@example.com"))))

(ert-deftest vm-virtual-test-an-empty-recipient-selects-none ()
  "A message whose To header is empty selects the messages with none, and is
named <none> rather than with an empty string nobody could read."
  (vm-virtual-test--creating-from "From: someone@example.com\nTo: \nSubject: a subject\n"
    (vm-create-virtual-folder-same-recipient)
    (should (equal (vm-virtual-test--argument) "^$"))
    (should (equal (vm-virtual-test--folder-name)
                   (vm-virtual-folder-name (buffer-name)
                                           'author-or-recipient "<none>")))))

(ert-deftest vm-virtual-test-an-empty-author-selects-none ()
  "The same for an empty From: the folder is of the messages with no author,
not of every message."
  (vm-virtual-test--creating-from "From: \nSubject: a subject\n"
    (vm-create-virtual-folder-same-author)
    (should (equal (vm-virtual-test--argument) "^$"))
    (should (equal (vm-virtual-test--folder-name)
                   (vm-virtual-folder-name (buffer-name) 'author "<none>")))))

(ert-deftest vm-virtual-test-a-missing-recipient-falls-back-to-the-login-name ()
  "A message with no To header at all is treated as addressed to you.

`vm-su-do-addressees' reads To, then Apparently-To, then Newsgroups, and
then -- its own comment says \"desperation\" -- `user-login-name'.  So this
command builds a folder of correspondence with your login name rather than
one of messages with no addressee, and the empty case above is reached only
by a To header that is present and empty."
  (vm-virtual-test--creating-from "From: someone@example.com\nSubject: a subject\n"
    (vm-create-virtual-folder-same-recipient)
    (should (equal (vm-virtual-test--argument)
                   (regexp-quote (user-login-name))))))

(ert-deftest vm-virtual-test-the-bookmark-is-the-message-it-was-called-on ()
  "The new folder opens on the message the reader was looking at, which is
what the bookmark argument is for."
  (vm-virtual-test--creating-from "From: someone@example.com\nSubject: a subject\n"
    (dolist (command '(vm-create-virtual-folder-same-subject
                       vm-create-virtual-folder-same-author
                       vm-create-virtual-folder-same-recipient))
      (funcall command)
      (should (eq (nth 4 vm-virtual-test--created) (car vm-message-pointer))))))

;;; Building a virtual folder on the fly (emacs-vm/vm#660)
;;
;; Both commands define a folder, visit it, and leave
;; `vm-virtual-folder-alist' as they found it.  These watch the definition
;; they hand to `vm-visit-virtual-folder' rather than visiting anything: what
;; is worth pinning is the clause built and the global left alone.

(defvar vm-virtual-test--visited nil
  "The name and definition passed to `vm-visit-virtual-folder'.")

(defmacro vm-virtual-test--building (&rest body)
  "Run BODY watching folder definition and visiting.
`vm-virtual-test--visited' becomes (NAME . DEFINITION), the definition
being `vm-virtual-folder-alist' as the command had bound it."
  (declare (indent 0) (debug t))
  `(let ((vm-virtual-test--visited nil)
         (vm-use-menus nil))
     (cl-letf (((symbol-function 'vm-visit-virtual-folder)
                (lambda (name &rest _)
                  (setq vm-virtual-test--visited
                        (cons name (copy-tree vm-virtual-folder-alist)))))
               ((symbol-function 'vm-build-threads-if-unbuilt) #'ignore))
       ,@body)))

(defun vm-virtual-test--definition ()
  "The clauses of the folder the command defined."
  (cdr (assoc (car vm-virtual-test--visited)
              (cdr vm-virtual-test--visited))))

(defmacro vm-virtual-test--in-a-small-folder (&rest body)
  "Run BODY in a folder of two messages, ready for the building commands."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder
       (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
               "From: sender@example.com\nSubject: one\n\nbody one\n\n"
               "From sender@example.com Mon Jan  1 00:00:00 2024\n"
               "From: sender@example.com\nSubject: two\n\nbody two\n\n")
     (setq major-mode 'vm-mode)
     ,@body))

(ert-deftest vm-virtual-test-a-thread-folder-selects-by-thread ()
  "The clause wraps the selector in `thread', which is what makes the folder
hold whole threads rather than the messages that matched."
  (vm-virtual-test--in-a-small-folder
    (vm-virtual-test--building
      (vm-create-virtual-folder-of-threads 'author "sender")
      (should (equal (vm-virtual-test--definition)
                     `(((( get-buffer ,(buffer-name)))
                        (thread (author "sender")))))))))

(ert-deftest vm-virtual-test-a-thread-folder-with-no-argument-passes-none ()
  "A selector that takes no argument gets a clause with none, rather than
one carrying nil for it to test against."
  (vm-virtual-test--in-a-small-folder
    (vm-virtual-test--building
      (vm-create-virtual-folder-of-threads 'unread)
      (should (equal (vm-virtual-test--definition)
                     `(((( get-buffer ,(buffer-name)))
                        (thread (unread)))))))))

(ert-deftest vm-virtual-test-a-thread-folder-can-follow-the-marks ()
  "After `vm-next-command-uses-marks' the folder holds the marked messages'
threads: the clause is the selector and the marks together."
  (vm-virtual-test--in-a-small-folder
    (vm-virtual-test--building
      (let ((last-command 'vm-next-command-uses-marks))
        (vm-create-virtual-folder-of-threads 'author "sender"))
      (should (equal (vm-virtual-test--definition)
                     `(((( get-buffer ,(buffer-name)))
                        (and (marked) (thread (author "sender"))))))))))

(ert-deftest vm-virtual-test-building-a-folder-leaves-the-alist-alone ()
  "The definition is bound for the visit only.  A folder built to answer one
question is not one the reader asked to keep, and adding it to
`vm-virtual-folder-alist' would put it in the menu of known folders and in
whatever they save."
  (let ((vm-virtual-folder-alist '(("kept" ((("inbox")) (author "someone"))))))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (vm-create-virtual-folder-of-threads 'author "sender"))
      (should (equal vm-virtual-folder-alist
                     '(("kept" ((("inbox")) (author "someone")))))))))

(ert-deftest vm-virtual-test-applying-a-folder-retargets-it-here ()
  "`vm-apply-virtual-folder' runs a named folder's selectors over the current
folder, so the clause names this buffer rather than the folders the
definition names."
  (let ((vm-virtual-folder-alist
         '(("interesting" ((("inbox" "archive")) (author "someone"))))))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (vm-apply-virtual-folder "interesting")
        (should (equal (vm-virtual-test--definition)
                       `(((( get-buffer ,(buffer-name))) (author "someone")))))))))

(ert-deftest vm-virtual-test-applying-a-folder-does-not-rewrite-it ()
  "The definition is copied before its clauses are retargeted.

Without the copy the reader's saved folder would be rewritten in place, so
the next use of it would select from whichever folder it was last applied
in rather than from the folders it names."
  (let* ((definition '(("interesting" ((("inbox" "archive")) (author "someone")))))
         (vm-virtual-folder-alist definition)
         (before (copy-tree definition)))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (vm-apply-virtual-folder "interesting"))
      (should (equal vm-virtual-folder-alist before)))))

(ert-deftest vm-virtual-test-applying-a-folder-can-follow-the-marks ()
  "With marks, the applied selectors are taken together and combined with
the marks, so the result is the marked messages that the folder would have
selected."
  (let ((vm-virtual-folder-alist
         '(("interesting" ((("inbox")) (author "someone") (subject "badgers"))))))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (let ((last-command 'vm-next-command-uses-marks))
          (vm-apply-virtual-folder "interesting"))
        (should (equal (vm-virtual-test--definition)
                       `(((( get-buffer ,(buffer-name)))
                          (and (marked)
                               (or (author "someone") (subject "badgers")))))))))))

(ert-deftest vm-virtual-test-applying-a-folder-that-is-not-defined-is-refused ()
  "A name that is not in `vm-virtual-folder-alist' is reported, and named."
  (let ((vm-virtual-folder-alist nil)
        (text-quoting-style 'grave))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (let ((err (should-error (vm-apply-virtual-folder "absent") :type 'error)))
          (should (string-match-p "absent" (error-message-string err))))))))

(ert-deftest vm-virtual-test-an-applied-folder-is-named-after-both ()
  "The new folder's name says which folder was applied to which."
  (let ((vm-virtual-folder-alist
         '(("interesting" ((("inbox")) (author "someone"))))))
    (vm-virtual-test--in-a-small-folder
      (vm-virtual-test--building
        (vm-apply-virtual-folder "interesting")
        (should (equal (car vm-virtual-test--visited)
                       (vm-virtual-application-folder-name (buffer-name)
                                                           "interesting")))))))

;;; A label added in one folder is a label in the others

(defun vm-virtual-test--known-labels (buffer)
  "The labels BUFFER's folder knows about, sorted."
  (with-current-buffer buffer
    (let (names)
      (mapatoms (lambda (s) (push (symbol-name s) names)) vm-label-obarray)
      (sort names #'string<))))

(ert-deftest vm-virtual-test-a-label-added-here-is-known-there ()
  "A label put on a message in a virtual folder is a label of the real
folder and of every other virtual folder showing that message, so
completion offers it in all of them and the summary can show it."
  (vm-virtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer virt-a
      (vm-add-or-delete-message-labels "urgent" (list (car vm-message-list))
                                       'all))
    (should (member "urgent" (vm-virtual-test--known-labels real)))
    (should (member "urgent" (vm-virtual-test--known-labels virt-b)))
    (should (member "urgent" (vm-virtual-test--known-labels virt-a)))))

(ert-deftest vm-virtual-test-deleting-a-label-teaches-nobody-about-it ()
  "Deleting a label does not make the virtual folders start offering it:
the label list of a folder is what its messages use, and a label just
taken off is not one of them."
  (vm-virtual-test--with-folders (real virt-a virt-b)
    (with-current-buffer real
      (vm-add-or-delete-message-labels "gone" (list (car vm-message-list)) nil))
    (should-not (member "gone" (vm-virtual-test--known-labels virt-a)))
    (should-not (member "gone" (vm-virtual-test--known-labels virt-b)))
    (should-not (member "gone" (vm-virtual-test--known-labels real)))))

(ert-deftest vm-virtual-test-a-label-survives-a-killed-virtual-folder ()
  "Quitting one virtual folder does not stop labelling working in the
others: the killed buffer is passed over rather than selected."
  (vm-virtual-test--with-folders (real virt-a virt-b)
    (kill-buffer virt-b)
    (with-current-buffer real
      (vm-add-or-delete-message-labels "urgent" (list (car vm-message-list))
                                       'all))
    (should (member "urgent" (vm-virtual-test--known-labels virt-a)))))


;;; Leaving a virtual folder keeps the summary on display (emacs-vm/vm#821)

(defmacro vm-virtual-test--with-a-displayed-folder (spec &rest body)
  "Visit a folder of `vm-virtual-test--assorted', summarize it, run BODY.
SPEC is (FOLDER-VAR).  A real visit with real windows, so `vm-display' is
not stubbed the way `vm-virtual-test--with-real-folder' stubs it: a test of
what the windows show is vacuous without it, because nothing then displays
anything."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-virtual-quit" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "real-folder" dir))
               (vm-virtual-folder-alist nil)
               (vm-init-file nil)
               (vm-preferences-file nil)
               (vm-confirm-quit nil)
               (vm-frame-per-folder nil)
               (vm-frame-per-summary nil)
               (vm-mutable-frame-configuration nil)
               (vm-visit-when-saving nil))
           (write-region vm-virtual-test--assorted nil ,(car spec) nil 'quiet)
           (vm-visit-folder ,(car spec))
           (vm-summarize)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
       (delete-directory dir t))))

(defun vm-virtual-test--displayed-buffers ()
  "The buffers the windows of the selected frame show."
  (mapcar #'window-buffer (window-list)))

(ert-deftest vm-virtual-test-leaving-a-virtual-folder-keeps-the-summary ()
  "REGRESSION: `q' in a virtual folder leaves the real folder summarized.

Reported by @goeran: after leaving a virtual folder the summary was displayed
nowhere and a keystroke was needed to bring it back (emacs-vm/vm#821).

`vm-quit' undisplays its own summary and presentation buffers before killing
them, and `vm-undisplay-buffer' hands the window to whatever `other-buffer'
answers.  Leaving a virtual folder that is the real folder's presentation
buffer, `vm-virtual-quit' having just presented into it to carry the message
pointer across, so the summary lost its window to the presentation of the
same folder.

Asserts on which buffers are displayed, not how many windows there are: the
window count was right throughout and the buffer in it was wrong."
  (vm-virtual-test--with-a-displayed-folder (folder)
    (let ((real (current-buffer))
          (summary vm-summary-buffer))
      (should (memq summary (vm-virtual-test--displayed-buffers)))
      (setq vm-virtual-folder-alist
            (list (list "everything" (list (list folder) '(header "Subject")))))
      (vm-visit-virtual-folder "everything")
      (should (eq major-mode 'vm-virtual-mode))
      (should-not (memq summary (vm-virtual-test--displayed-buffers)))
      (vm-quit)
      (should (buffer-live-p real))
      (should (buffer-live-p summary))
      (should (memq summary (vm-virtual-test--displayed-buffers))))))

(ert-deftest vm-virtual-test-leaving-the-only-folder-leaves-no-folder ()
  "Quitting the last folder has none to return to, and must still work.
Putting the folder left on display is the case above; here there is none,
which the fix has to pass over rather than fail on.  Asserted on what is
left rather than on the function that answers it, so it holds whichever way
that is written."
  (vm-virtual-test--with-a-displayed-folder (_folder)
    (let ((folder (current-buffer)))
      (vm-quit)
      (should-not (buffer-live-p folder))
      (should-not (seq-find (lambda (buffer)
                              (with-current-buffer buffer
                                (memq major-mode '(vm-mode vm-virtual-mode))))
                            (buffer-list))))))

(provide 'vm-virtual-test)

;;; vm-virtual-test.el ends here
