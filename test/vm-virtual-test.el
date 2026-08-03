;;; vm-virtual-test.el --- Tests for vm-virtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

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
      (should-not (string-match-p "REAL\\|AAA" sb)))))

(ert-deftest vm-virtual-test-summary-stored-on-the-virtual-message ()
  "A virtual message's summary line is kept on the virtual message.
The representation behind the test above: the line belongs in the virtual
message's own soft data, not in the cached data it shares with the real one."
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
        (should (vm-virtual-summary-of vm))))))

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

(provide 'vm-virtual-test)

;;; vm-virtual-test.el ends here
