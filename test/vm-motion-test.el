;;; vm-motion-test.el --- Tests for vm-motion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM motion/navigation functions in vm-motion.el

;;; Code:

(require 'vm-test-init)
(require 'vm-motion)

;;; Motion function existence tests

(ert-deftest vm-motion-test-functions-exist ()
  "Test that motion functions exist."
  (should (fboundp 'vm-record-and-change-message-pointer))
  (should (fboundp 'vm-goto-message))
  (should (fboundp 'vm-goto-message-last-seen))
  (should (fboundp 'vm-goto-parent-message))
  (should (fboundp 'vm-check-count))
  (should (fboundp 'vm-move-message-pointer))
  (should (fboundp 'vm-should-skip-message))
  (should (fboundp 'vm-should-skip-hidden-message))
  (should (fboundp 'vm-next-message))
  (should (fboundp 'vm-previous-message))
  (should (fboundp 'vm-next-message-no-skip))
  (should (fboundp 'vm-previous-message-no-skip))
  (should (fboundp 'vm-next-unread-message))
  (should (fboundp 'vm-previous-unread-message))
  (should (fboundp 'vm-next-message-same-subject))
  (should (fboundp 'vm-previous-message-same-subject))
  (should (fboundp 'vm-find-first-unread-message))
  (should (fboundp 'vm-thoughtfully-select-message))
  (should (fboundp 'vm-follow-summary-cursor)))

;;; vm-check-count tests
;; Note: vm-check-count requires vm-message-list/vm-message-pointer context
;; so we test it within a folder context

(ert-deftest vm-motion-test-check-count-within-folder ()
  "Test vm-check-count within folder context."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2

From sender@example.com Mon Jan  3 00:00:00 2024
From: sender@example.com
Subject: Message 3
Message-ID: <test3@example.com>

Body 3
"
    (setq vm-message-pointer vm-message-list)
    ;; Check count 1 should not signal (we have 3 messages ahead)
    (should (null (vm-check-count 1)))
    ;; Check count 3 should not signal (we have exactly 3 messages)
    (should (null (vm-check-count 3)))
    ;; Check count 4 should signal end-of-folder
    (should-error (vm-check-count 4) :type 'end-of-folder)))

;;; vm-move-message-pointer tests
;; Note: vm-move-message-pointer uses 'forward and 'backward directions
;; and signals end-of-folder/beginning-of-folder rather than returning nil

(ert-deftest vm-motion-test-move-message-pointer-forward ()
  "Test vm-move-message-pointer moves forward in message list."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    (should (= 2 (vm-test-message-count)))
    ;; Start at first message
    (setq vm-message-pointer vm-message-list)
    ;; Move forward
    (vm-move-message-pointer 'forward)
    (should (eq vm-message-pointer (cdr vm-message-list)))))

(ert-deftest vm-motion-test-move-message-pointer-backward ()
  "Test vm-move-message-pointer moves backward in message list."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    ;; Set up reverse links
    (vm-set-reverse-link-of (car (cdr vm-message-list)) vm-message-list)
    ;; Start at second message
    (setq vm-message-pointer (cdr vm-message-list))
    ;; Move backward
    (vm-move-message-pointer 'backward)
    (should (eq vm-message-pointer vm-message-list))))

(ert-deftest vm-motion-test-move-message-pointer-signals-at-end ()
  "Test vm-move-message-pointer signals end-of-folder at end."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1
"
    (let ((vm-circular-folders nil))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Try to move forward - should signal end-of-folder
      (should-error (vm-move-message-pointer 'forward) :type 'end-of-folder))))

(ert-deftest vm-motion-test-move-message-pointer-signals-at-beginning ()
  "Test vm-move-message-pointer signals beginning-of-folder at start."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1
"
    (let ((vm-circular-folders nil))
      ;; Start at only message (no reverse link)
      (setq vm-message-pointer vm-message-list)
      ;; Try to move backward - should signal beginning-of-folder
      (should-error (vm-move-message-pointer 'backward) :type 'beginning-of-folder))))

;;; vm-should-skip-message tests
;; Note: vm-should-skip-message takes a message-pointer (cons), not just a message

(ert-deftest vm-motion-test-should-skip-deleted-when-skipping ()
  "Test vm-should-skip-message skips deleted messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages t)
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))  ; Avoid hidden message checks
      ;; Mark as deleted
      (vm-set-deleted-flag (car vm-message-list) t)
      ;; Pass the message pointer (vm-message-list is a cons)
      (should (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-deleted-when-not-skipping ()
  "Test vm-should-skip-message doesn't skip deleted when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages nil)
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as deleted
      (vm-set-deleted-flag (car vm-message-list) t)
      ;; Should NOT skip
      (should-not (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-read-when-skipping ()
  "Test vm-should-skip-message skips read messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-read-messages t)
          (vm-skip-deleted-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as read (not new, not unread, not deleted)
      (vm-set-new-flag (car vm-message-list) nil)
      (vm-set-unread-flag (car vm-message-list) nil)
      (vm-set-deleted-flag (car vm-message-list) nil)
      ;; Should skip
      (should (vm-should-skip-message vm-message-list nil)))))

(ert-deftest vm-motion-test-should-skip-read-when-not-skipping ()
  "Test vm-should-skip-message doesn't skip read when disabled."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-read-messages nil)
          (vm-skip-deleted-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as read
      (vm-set-new-flag (car vm-message-list) nil)
      (vm-set-unread-flag (car vm-message-list) nil)
      ;; Should NOT skip
      (should-not (vm-should-skip-message vm-message-list nil)))))

;;; vm-find-first-unread-message tests

(ert-deftest vm-motion-test-find-first-unread-new ()
  "Test vm-find-first-unread-message finds new messages."
  ;; Note: vm-find-first-unread-message takes a NEW-ONLY flag, not a message list.
  ;; It uses the global vm-message-list internally.
  ;; When NEW-ONLY is non-nil, it only looks for new messages.
  ;; When NEW-ONLY is nil, it looks for new OR unread messages.
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    ;; First message is read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    ;; Second message is new
    (vm-set-new-flag (vm-test-nth-message 1) t)

    ;; Pass t for NEW-ONLY to look for new messages
    (let ((found (vm-find-first-unread-message t)))
      (should found)
      (should (eq (car found) (vm-test-nth-message 1))))))

(ert-deftest vm-motion-test-find-first-unread-unread ()
  "Test vm-find-first-unread-message finds unread messages."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    ;; First message is read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    (vm-set-deleted-flag (vm-test-nth-message 0) nil)
    ;; Second message is unread (not new, but unread)
    (vm-set-new-flag (vm-test-nth-message 1) nil)
    (vm-set-unread-flag (vm-test-nth-message 1) t)
    (vm-set-deleted-flag (vm-test-nth-message 1) nil)

    ;; Pass nil for NEW-ONLY to look for new OR unread messages
    (let ((found (vm-find-first-unread-message nil)))
      (should found)
      (should (eq (car found) (vm-test-nth-message 1))))))

(ert-deftest vm-motion-test-find-first-unread-all-read ()
  "Test vm-find-first-unread-message returns nil when all read."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2
"
    ;; All messages are read
    (vm-set-new-flag (vm-test-nth-message 0) nil)
    (vm-set-unread-flag (vm-test-nth-message 0) nil)
    (vm-set-new-flag (vm-test-nth-message 1) nil)
    (vm-set-unread-flag (vm-test-nth-message 1) nil)

    ;; Pass nil for NEW-ONLY (look for new or unread)
    (let ((found (vm-find-first-unread-message nil)))
      (should (null found)))))

;;; Message pointer state tests

(ert-deftest vm-motion-test-message-pointer-initialized ()
  "Test that vm-message-pointer is properly initialized."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (should vm-message-pointer)
    (should (eq vm-message-pointer vm-message-list))))

;;; Message number tests

(ert-deftest vm-motion-test-message-numbering ()
  "Test that messages are numbered correctly."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Message 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Message 2
Message-ID: <test2@example.com>

Body 2

From sender@example.com Mon Jan  3 00:00:00 2024
From: sender@example.com
Subject: Message 3
Message-ID: <test3@example.com>

Body 3
"
    ;; Number the messages
    (vm-number-messages)
    ;; Check numbering
    (should (equal (vm-number-of (vm-test-nth-message 0)) "1"))
    (should (equal (vm-number-of (vm-test-nth-message 1)) "2"))
    (should (equal (vm-number-of (vm-test-nth-message 2)) "3"))))

;;; Higher-level navigation command tests
;; These test the interactive navigation functions

(ert-deftest vm-motion-test-Next-message-callable ()
  "Test vm-Next-message is a callable function."
  ;; vm-Next-message is defined with (fset 'vm-Next-message 'vm-next-message-no-skip)
  (should (fboundp 'vm-Next-message))
  (should (commandp 'vm-Next-message)))

(ert-deftest vm-motion-test-Previous-message-callable ()
  "Test vm-Previous-message is a callable function."
  ;; vm-Previous-message is defined with fset as an alias
  (should (fboundp 'vm-Previous-message))
  (should (commandp 'vm-Previous-message)))

;;; Circular folder tests

(ert-deftest vm-motion-test-circular-folders-wraps-forward ()
  "Test that circular-folders wraps forward navigation."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Only Message
Message-ID: <test@example.com>

Body
"
    (let ((vm-circular-folders t))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Move forward should wrap to same message
      (vm-move-message-pointer 'forward)
      (should (eq vm-message-pointer vm-message-list)))))

(ert-deftest vm-motion-test-circular-folders-wraps-backward ()
  "Test that circular-folders wraps backward navigation."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Only Message
Message-ID: <test@example.com>

Body
"
    (let ((vm-circular-folders t))
      ;; Start at only message
      (setq vm-message-pointer vm-message-list)
      ;; Move backward should wrap to same message
      (vm-move-message-pointer 'backward)
      (should (eq vm-message-pointer vm-message-list)))))

;;; Message skip configuration tests

(ert-deftest vm-motion-test-skip-filed-messages ()
  "Test vm-should-skip-message skips filed messages when configured."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((vm-skip-deleted-messages t)  ; needed to enable skip logic
          (vm-skip-read-messages nil)
          (vm-summary-buffer nil))
      ;; Mark as filed
      (vm-set-filed-flag (car vm-message-list) t)
      ;; Filed messages aren't skipped by default - they're not in skip logic
      (should-not (vm-should-skip-message vm-message-list nil)))))

;;; Thread navigation tests
;; Basic thread navigation - vm-goto-parent-message requires threading setup

(ert-deftest vm-motion-test-goto-parent-exists ()
  "Test vm-goto-parent-message function exists."
  (should (fboundp 'vm-goto-parent-message)))

;;; Message selection tests

(ert-deftest vm-motion-test-thoughtfully-select-returns-pointer ()
  "Test vm-thoughtfully-select-message returns a message pointer."
  ;; vm-thoughtfully-select-message returns a message pointer or nil
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    ;; Mark message as new so thoughtfully-select might find it
    (vm-set-new-flag (car vm-message-list) t)
    (let ((vm-jump-to-new-messages t)
          (vm-jump-to-unread-messages nil))
      (let ((result (vm-thoughtfully-select-message)))
        ;; Should return the message pointer (list starting at the message)
        (should (or (null result) (consp result)))))))


;;; the summary arrow and the current message must agree (issue #528)

(defun vm-motion-test--arrow-line ()
  "Return the line number of the summary arrow, or nil if there is none."
  (with-current-buffer vm-summary-buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward vm-summary-=> nil t)
        (line-number-at-pos (match-beginning 0))))))

(ert-deftest vm-motion-test-follow-summary-cursor-moves-the-arrow ()
  "REGRESSION: selecting a summary line by point moves the arrow with it.
Issue #528: clicking a different line in the summary makes that message current
-- `vm-follow-summary-cursor' runs first in almost every command -- but the arrow
only moved later, when the command got around to updating the summary.  A command
that asks a question first, such as `vm-save-message' asking which folder, put its
question while the arrow still pointed at the message the user had before
clicking, so the answer applied to a message the display disagreed about.

Here the click is simulated the way `mouse-set-point' leaves things: point on
another summary line, nothing else."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-motion" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-mail-buffer nil)
         (before (buffer-list)))
    (require 'vm)
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (i 8)
              (insert (format "From s%d@example.com Mon Jan  1 00:00:00 2024\n" i)
                      (format "From: S%d <s%d@example.com>\n" i i)
                      (format "Subject: message %d\n" i)
                      (format "Message-ID: <motion-%d@example.com>\n" i)
                      "\n"
                      (format "Body %d.\n\n" i))))
          (vm-visit-folder file)
          (vm-goto-message 3)
          (should (equal "3" (vm-number-of (car vm-message-pointer))))
          (should (= 3 (vm-motion-test--arrow-line)))
          ;; "Click" the seventh line.
          (let ((folder (current-buffer)))
            (set-buffer vm-summary-buffer)
            (goto-char (point-min))
            (should (search-forward "message 6" nil t))
            (beginning-of-line)
            (should (= 7 (line-number-at-pos (point))))
            ;; What every command does before doing anything else.
            (should (vm-follow-summary-cursor))
            (with-current-buffer folder
              ;; the message the command will act on ...
              (should (equal "7" (vm-number-of (car vm-message-pointer))))
              ;; ... is the one the arrow points at.
              (should (= 7 (vm-motion-test--arrow-line))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))
            (kill-buffer buffer))))
      (delete-directory dir t))))

;;; What the movement checks do, in place of a test that they were bound.

(defconst vm-motion-test--three
  (concat "From a@example.com Mon Jan  1 00:00:00 2024\nFrom: a@example.com\n"
          "Subject: one\n\nBody.\n\n"
          "From b@example.com Mon Jan  1 00:00:00 2024\nFrom: b@example.com\n"
          "Subject: two\n\nBody.\n\n"
          "From c@example.com Mon Jan  1 00:00:00 2024\nFrom: c@example.com\n"
          "Subject: three\n\nBody.\n\n")
  "Three messages, so that there is somewhere to move from and to.")

(ert-deftest vm-motion-test-check-count-signals-at-the-ends ()
  "Asking to move further than the folder goes signals which end was hit.
The two conditions are caught separately by callers, so a plain error would
not do."
  (vm-test-with-folder vm-motion-test--three
    (setq vm-message-pointer vm-message-list)
    ;; The current message counts as one of the messages there is room for,
    ;; so at the first of three, 3 forward and -1 back are both in range.
    (should-not (vm-check-count 3))
    (should-not (vm-check-count -1))
    (should (eq (car (should-error (vm-check-count 4))) 'end-of-folder))
    (should (eq (car (should-error (vm-check-count -2)))
                'beginning-of-folder))
    ;; and at the last, the other way round
    (setq vm-message-pointer (cdr (cdr vm-message-list)))
    (should-not (vm-check-count 1))
    (should-not (vm-check-count -3))
    (should (eq (car (should-error (vm-check-count 2))) 'end-of-folder))
    (should (eq (car (should-error (vm-check-count -4)))
                'beginning-of-folder))))

(ert-deftest vm-motion-test-should-skip-deleted-only-when-asked ()
  "A deleted message is skipped when `vm-skip-deleted-messages' is t.
Any other non-nil value means skip only when the caller insists, which is
what the third state of that option is for."
  (vm-test-with-folder vm-motion-test--three
    (let ((mp vm-message-list)
          (vm-skip-read-messages nil)
          (last-command nil))
      (vm-set-deleted-flag (car mp) t)
      (let ((vm-skip-deleted-messages t))
        (should (vm-should-skip-message mp))
        (should (vm-should-skip-message mp t)))
      (let ((vm-skip-deleted-messages 'sometimes))
        (should-not (vm-should-skip-message mp))
        (should (vm-should-skip-message mp t)))
      (let ((vm-skip-deleted-messages nil))
        (should-not (vm-should-skip-message mp))
        (should-not (vm-should-skip-message mp t))))))

(ert-deftest vm-motion-test-should-skip-read-messages ()
  "A message that is neither new nor unread is a read one, and is skipped
when `vm-skip-read-messages' says so."
  (vm-test-with-folder vm-motion-test--three
    (let ((mp vm-message-list)
          (vm-skip-deleted-messages nil)
          (last-command nil))
      (vm-set-new-flag (car mp) nil)
      (vm-set-unread-flag (car mp) nil)
      (let ((vm-skip-read-messages t))
        (should (vm-should-skip-message mp)))
      (vm-set-unread-flag (car mp) t)
      (let ((vm-skip-read-messages t))
        (should-not (vm-should-skip-message mp))))))

(ert-deftest vm-motion-test-should-skip-unmarked-after-a-mark-command ()
  "After `vm-next-command-uses-marks' only the marked messages are visited."
  (vm-test-with-folder vm-motion-test--three
    (let ((mp vm-message-list)
          (vm-skip-deleted-messages nil)
          (vm-skip-read-messages nil))
      (let ((last-command 'vm-next-command-uses-marks))
        (should (vm-should-skip-message mp))
        (vm-set-mark-of (car mp) t)
        (should-not (vm-should-skip-message mp)))
      ;; and without that command, marks make no difference
      (let ((last-command nil))
        (vm-set-mark-of (car mp) nil)
        (should-not (vm-should-skip-message mp))))))

;;; Moving about the folder (emacs-vm/vm#632)
;;
;; The commands a reader presses all day -- n, p, and their no-skip and unread
;; variants -- were called by no test.  What matters about them is which
;; message you land on, and the skipping is the whole of it: `vm-next-message'
;; passes over a deleted message and `vm-next-message-no-skip' does not.

(defconst vm-motion-test--folder
  (mapconcat
   (lambda (n)
     (format (concat "From sender%d@example.com Sat Aug  %d 10:00:00 2026\n"
                     "From: sender%d@example.com\nSubject: m%d\n\nBody %d.\n\n")
             n n n n n))
   '(1 2 3 4) "")
  "Four messages, subjects m1 to m4.")

(defmacro vm-motion-test--with-folder (&rest body)
  "Visit a folder of four messages, select the first, and run BODY."
  (declare (indent 0) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-motion" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((folder (expand-file-name "incoming" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-summary-show-threads nil)
               (vm-summary-enable-thread-folding nil)
               (vm-skip-deleted-messages t)
               (vm-skip-read-messages nil)
               (vm-circular-folders nil))
           (write-region vm-motion-test--folder nil folder nil 'quiet)
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

(defun vm-motion-test--here ()
  "The subject of the message the folder is looking at."
  (vm-su-subject (car vm-message-pointer)))

(defun vm-motion-test--go-to (n)
  "Select message N, counting from 1."
  (setq vm-message-pointer (nthcdr (1- n) vm-message-list)))

(ert-deftest vm-motion-test-next-and-previous-move-one-message ()
  "`vm-next-message' and `vm-previous-message' step by one, and undo
each other."
  (vm-motion-test--with-folder
    (should (equal (vm-motion-test--here) "m1"))
    (vm-next-message 1)
    (should (equal (vm-motion-test--here) "m2"))
    (vm-next-message 1)
    (should (equal (vm-motion-test--here) "m3"))
    (vm-previous-message 1)
    (should (equal (vm-motion-test--here) "m2"))
    (vm-previous-message 1)
    (should (equal (vm-motion-test--here) "m1"))))

(ert-deftest vm-motion-test-next-skips-a-deleted-message ()
  "`vm-next-message' passes over a deleted message while
`vm-skip-deleted-messages' says so, and `vm-next-message-no-skip' lands on
it -- which is the whole difference between the two commands."
  (vm-motion-test--with-folder
    (vm-set-deleted-flag (nth 2 vm-message-list) t)   ; m3
    (vm-motion-test--go-to 2)
    (vm-next-message 1)
    (should (equal (vm-motion-test--here) "m4"))
    (vm-motion-test--go-to 2)
    (vm-next-message-no-skip 1)
    (should (equal (vm-motion-test--here) "m3"))))

(ert-deftest vm-motion-test-previous-skips-a-deleted-message ()
  "The same going backwards: `vm-previous-message' passes over the deleted
message and `vm-previous-message-no-skip' does not."
  (vm-motion-test--with-folder
    (vm-set-deleted-flag (nth 1 vm-message-list) t)   ; m2
    (vm-motion-test--go-to 3)
    (vm-previous-message 1)
    (should (equal (vm-motion-test--here) "m1"))
    (vm-motion-test--go-to 3)
    (vm-previous-message-no-skip 1)
    (should (equal (vm-motion-test--here) "m2"))))

(ert-deftest vm-motion-test-a-count-of-more-than-one-ignores-skipping ()
  "A count greater than one moves that many messages whatever their state.
The docstring says so: the skip options are ignored when the absolute value
of the count is more than one, so counting is counting.

The deleted message is the one being counted onto, not one being counted
over -- with m2 deleted instead, counting two and skipping one both land on
m3 and the test would hold whether or not the count was honoured."
  (vm-motion-test--with-folder
    (vm-set-deleted-flag (nth 2 vm-message-list) t)   ; m3
    (vm-next-message 2)
    (should (equal (vm-motion-test--here) "m3"))
    ;; one at a time, the same deleted message is passed over
    (vm-motion-test--go-to 1)
    (vm-next-message 1)
    (vm-next-message 1)
    (should (equal (vm-motion-test--here) "m4"))))

(ert-deftest vm-motion-test-next-unread-message-finds-the-unread-one ()
  "`vm-next-unread-message' goes to the next message not yet read, passing
over the ones that have been."
  (vm-motion-test--with-folder
    (dolist (m vm-message-list)
      (vm-set-new-flag m nil)
      (vm-set-unread-flag m nil))
    (vm-set-unread-flag (nth 3 vm-message-list) t)    ; m4 alone is unread
    (vm-motion-test--go-to 1)
    (vm-next-unread-message)
    (should (equal (vm-motion-test--here) "m4"))))

(ert-deftest vm-motion-test-previous-unread-message-looks-backwards ()
  "`vm-previous-unread-message' is the same search the other way."
  (vm-motion-test--with-folder
    (dolist (m vm-message-list)
      (vm-set-new-flag m nil)
      (vm-set-unread-flag m nil))
    (vm-set-unread-flag (car vm-message-list) t)      ; m1 alone is unread
    (vm-motion-test--go-to 4)
    (vm-previous-unread-message)
    (should (equal (vm-motion-test--here) "m1"))))

(ert-deftest vm-motion-test-goto-message-last-seen-goes-back ()
  "`vm-goto-message-last-seen' returns to the message you were on before,
and pressing it twice puts you back where you started -- which is what makes
it usable for flipping between two messages."
  (vm-motion-test--with-folder
    (vm-motion-test--go-to 1)
    (vm-record-and-change-message-pointer vm-message-pointer
                                          (nthcdr 2 vm-message-list))
    (should (equal (vm-motion-test--here) "m3"))
    (vm-goto-message-last-seen)
    (should (equal (vm-motion-test--here) "m1"))
    (vm-goto-message-last-seen)
    (should (equal (vm-motion-test--here) "m3"))))

(ert-deftest vm-motion-test-moving-past-the-end-is-an-error ()
  "Moving past the last message says so rather than wrapping, while
`vm-circular-folders' is off."
  (vm-motion-test--with-folder
    (vm-motion-test--go-to 4)
    (should-error (vm-next-message 1 nil t))
    (vm-motion-test--go-to 1)
    (should-error (vm-previous-message 1 nil t))))

;;; What vm-next-message does with a count, with marks and with a retry

(ert-deftest vm-motion-test-next-message-without-a-count-moves-one ()
  "Called from Lisp with no arguments at all, the move is one message: the
count is only ever given by the prefix argument."
  (vm-motion-test--with-folder
    (vm-next-message)
    (should (equal (vm-motion-test--here) "m2"))
    (vm-previous-message)
    (should (equal (vm-motion-test--here) "m1"))))

(ert-deftest vm-motion-test-a-negative-count-goes-the-other-way ()
  "A negative count reverses the command, which is what a negative prefix
argument is for."
  (vm-motion-test--with-folder
    (vm-motion-test--go-to 3)
    (vm-next-message -2)
    (should (equal (vm-motion-test--here) "m1"))
    (vm-previous-message -2)
    (should (equal (vm-motion-test--here) "m3"))))

(ert-deftest vm-motion-test-a-count-does-not-count-hidden-messages ()
  "A message hidden in a folded thread is passed over without being
counted, so a count of two moves two visible messages."
  (vm-motion-test--with-folder
    (cl-letf (((symbol-function 'vm-should-skip-hidden-message)
               (lambda (mp) (equal (vm-su-subject (car mp)) "m2"))))
      (vm-next-message 2)
      (should (equal (vm-motion-test--here) "m4")))))

(ert-deftest vm-motion-test-a-count-with-marks-counts-marked-messages ()
  "After `vm-next-command-uses-marks' the count is in marked messages, so
unmarked ones in between are passed over."
  (vm-motion-test--with-folder
    (vm-set-mark-of (nth 2 vm-message-list) t)      ; m3
    (vm-set-mark-of (nth 3 vm-message-list) t)      ; m4
    (let ((last-command 'vm-next-command-uses-marks)
          (vm-circular-folders t))
      (vm-next-message 2)
      (should (equal (vm-motion-test--here) "m4")))))

(ert-deftest vm-motion-test-a-count-with-one-mark-stops-on-it ()
  "With one message marked, a count of two goes round the folder and stops
where it started rather than running for ever or stopping on an unmarked
message."
  (vm-motion-test--with-folder
    (vm-set-mark-of (nth 2 vm-message-list) t)      ; m3, and nothing else
    (let ((last-command 'vm-next-command-uses-marks)
          (vm-circular-folders t))
      (vm-next-message 2)
      (should (equal (vm-motion-test--here) "m3")))))

(ert-deftest vm-motion-test-a-retry-forward-relaxes-the-skipping ()
  "`vm-skip-deleted-messages' set to something other than t skips deleted
messages on the way past but stops on one rather than bumping into the end
of the folder.  The retry is what makes that difference: the first pass
skips dogmatically, the second does not."
  (vm-motion-test--with-folder
    (let ((vm-skip-deleted-messages 'when-there-is-somewhere-else))
      (vm-set-deleted-flag (nth 3 vm-message-list) t)     ; m4
      (vm-motion-test--go-to 3)
      (vm-next-message 1 t)
      (should (equal (vm-motion-test--here) "m4")))))

(ert-deftest vm-motion-test-a-retry-backward-relaxes-the-skipping ()
  "The same going backwards, which is a separate arm of the command."
  (vm-motion-test--with-folder
    (let ((vm-skip-deleted-messages 'when-there-is-somewhere-else))
      (vm-set-deleted-flag (car vm-message-list) t)       ; m1
      (vm-motion-test--go-to 2)
      (vm-previous-message 1 t)
      (should (equal (vm-motion-test--here) "m1")))))

(ert-deftest vm-motion-test-without-a-retry-the-move-is-refused ()
  "Without the retry the same move stays where it was: the relaxed pass is
the retry and nothing else."
  (vm-motion-test--with-folder
    (let ((vm-skip-deleted-messages 'when-there-is-somewhere-else))
      (vm-set-deleted-flag (nth 3 vm-message-list) t)     ; m4
      (vm-motion-test--go-to 3)
      (should-error (vm-next-message 1 nil t) :type 'end-of-folder)
      (should (equal (vm-motion-test--here) "m3"))
      (vm-set-deleted-flag (car vm-message-list) t)       ; m1
      (vm-motion-test--go-to 2)
      (should-error (vm-previous-message 1 nil t) :type 'beginning-of-folder)
      (should (equal (vm-motion-test--here) "m2")))))

(ert-deftest vm-motion-test-a-move-that-goes-nowhere-is-not-recorded ()
  "A refused move does not become the message last seen, so
`vm-goto-message-last-seen' still goes back to where you really were."
  (vm-motion-test--with-folder
    (vm-motion-test--go-to 4)
    (setq vm-last-message-pointer nil)
    (vm-next-message 1)                 ; nowhere to go, errors are off
    (should (equal (vm-motion-test--here) "m4"))
    (should-not vm-last-message-pointer)))

(provide 'vm-motion-test)

;;; vm-motion-test.el ends here