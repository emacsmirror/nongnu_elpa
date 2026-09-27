;;; vm-mark-test.el --- Tests for vm-mark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM mark functions in vm-mark.el

;;; Code:

(require 'vm-test-init)
(require 'vm-mark)

;;; Mark function existence tests

(ert-deftest vm-mark-test-functions-exist ()
  "Test that mark functions exist."
  (should (fboundp 'vm-clear-all-marks))
  (should (fboundp 'vm-toggle-all-marks))
  (should (fboundp 'vm-mark-all-messages))
  (should (fboundp 'vm-mark-message))
  (should (fboundp 'vm-unmark-message))
  (should (fboundp 'vm-mark-summary-region))
  (should (fboundp 'vm-unmark-summary-region))
  (should (fboundp 'vm-mark-messages-by-selector))
  (should (fboundp 'vm-unmark-messages-by-selector))
  (should (fboundp 'vm-mark-thread-subtree))
  (should (fboundp 'vm-unmark-thread-subtree))
  (should (fboundp 'vm-mark-messages-same-subject))
  (should (fboundp 'vm-unmark-messages-same-subject))
  (should (fboundp 'vm-mark-messages-same-author))
  (should (fboundp 'vm-unmark-messages-same-author)))

;;; vm-mark-or-unmark helper tests

(ert-deftest vm-mark-test-or-unmark-functions-exist ()
  "Test that mark/unmark helper functions exist."
  (should (fboundp 'vm-mark-or-unmark-summary-region))
  (should (fboundp 'vm-mark-or-unmark-messages-by-selector))
  (should (fboundp 'vm-mark-or-unmark-thread-subtree))
  (should (fboundp 'vm-mark-or-unmark-messages-same-subject))
  (should (fboundp 'vm-mark-or-unmark-messages-same-author)))

;;; Tests using real messages

(defconst vm-mark-test-folder
  "From alice@example.com Mon Jan  1 00:00:00 2024
From: Alice <alice@example.com>
To: recipient@example.com
Subject: First Subject
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <mark1@example.com>

First message body.

From alice@example.com Tue Jan  2 00:00:00 2024
From: Alice <alice@example.com>
To: recipient@example.com
Subject: First Subject
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <mark2@example.com>

Second message body, same author and subject.

From bob@example.com Wed Jan  3 00:00:00 2024
From: Bob <bob@example.com>
To: recipient@example.com
Subject: Different Subject
Date: Wed, 03 Jan 2024 12:00:00 +0000
Message-ID: <mark3@example.com>

Third message from different author.

"
  "Test folder for mark and label tests.")

;;; vm-marked-messages tests

(ert-deftest vm-mark-test-marked-messages-empty ()
  "Test vm-marked-messages with no marks returns nil."
  (vm-test-with-folder vm-mark-test-folder
    (dolist (msg vm-message-list)
      (vm-set-mark-of msg nil))
    (should (null (vm-marked-messages)))))

(ert-deftest vm-mark-test-marked-messages-some ()
  "Test vm-marked-messages returns only marked messages."
  (vm-test-with-folder vm-mark-test-folder
    ;; Mark first and third messages
    (vm-set-mark-of (nth 0 vm-message-list) t)
    (vm-set-mark-of (nth 1 vm-message-list) nil)
    (vm-set-mark-of (nth 2 vm-message-list) t)
    (let ((marked (vm-marked-messages)))
      (should (= (length marked) 2))
      (should (memq (nth 0 vm-message-list) marked))
      (should-not (memq (nth 1 vm-message-list) marked))
      (should (memq (nth 2 vm-message-list) marked)))))

(ert-deftest vm-mark-test-marked-messages-all ()
  "Test vm-marked-messages when all are marked."
  (vm-test-with-folder vm-mark-test-folder
    (dolist (msg vm-message-list)
      (vm-set-mark-of msg t))
    (should (= (length (vm-marked-messages)) 3))))

;;; Label accessor tests

(ert-deftest vm-mark-test-labels-accessor ()
  "Test vm-labels-of accessor."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Initially no labels
      (vm-set-labels-of msg nil)
      (should (null (vm-labels-of msg)))
      ;; Set some labels
      (vm-set-labels-of msg '("work" "important"))
      (should (equal (vm-labels-of msg) '("work" "important"))))))

(ert-deftest vm-mark-test-label-string-of ()
  "Test vm-label-string-of returns labels as string."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Set labels via vm-set-labels-of
      (vm-set-labels-of msg '("work" "urgent"))
      ;; vm-label-string-of returns cached string from mirror-data
      ;; It must be explicitly set or retrieved via vm-su-labels
      ;; which computes it. Set it explicitly for this test.
      (vm-set-label-string-of msg "work urgent")
      (let ((label-str (vm-label-string-of msg)))
        (should (stringp label-str))
        (should (string-match "work" label-str))
        (should (string-match "urgent" label-str))))))

(ert-deftest vm-mark-test-label-string-empty ()
  "Test vm-label-string-of with no labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg nil)
      (let ((label-str (vm-label-string-of msg)))
        (should (or (null label-str) (equal label-str "")))))))

;;; Mark flag accessor tests

(ert-deftest vm-mark-test-mark-of-accessor ()
  "Test vm-mark-of and vm-set-mark-of."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mark-of msg nil)
      (should (null (vm-mark-of msg)))
      (vm-set-mark-of msg t)
      (should (vm-mark-of msg)))))

;;; Mark same subject tests

(ert-deftest vm-mark-test-same-subject-found ()
  "Test finding messages with same subject."
  (vm-test-with-folder vm-mark-test-folder
    ;; First two messages have same subject
    (let ((msg1 (nth 0 vm-message-list))
          (msg2 (nth 1 vm-message-list)))
      (let ((subj1 (vm-su-subject msg1))
            (subj2 (vm-su-subject msg2)))
        (should (equal subj1 subj2))))))

;;; Mark same author tests

(ert-deftest vm-mark-test-same-author-found ()
  "Test finding messages with same author."
  (vm-test-with-folder vm-mark-test-folder
    ;; First two messages have same author (Alice)
    (let ((msg1 (nth 0 vm-message-list))
          (msg2 (nth 1 vm-message-list))
          (msg3 (nth 2 vm-message-list)))
      (let ((from1 (vm-su-from msg1))
            (from2 (vm-su-from msg2))
            (from3 (vm-su-from msg3)))
        (should (string-match "Alice" from1))
        (should (string-match "Alice" from2))
        (should (string-match "Bob" from3))))))

;;; Label manipulation tests

(ert-deftest vm-mark-test-add-label ()
  "Test adding a label to a message."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg nil)
      ;; Manually add a label
      (vm-set-labels-of msg (cons "new-label" (vm-labels-of msg)))
      (should (member "new-label" (vm-labels-of msg))))))

(ert-deftest vm-mark-test-multiple-labels ()
  "Test message can have multiple labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("label1" "label2" "label3"))
      (let ((labels (vm-labels-of msg)))
        (should (= (length labels) 3))
        (should (member "label1" labels))
        (should (member "label2" labels))
        (should (member "label3" labels))))))

(ert-deftest vm-mark-test-remove-label ()
  "Test removing a label from a message."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("keep" "remove" "also-keep"))
      (vm-set-labels-of msg (delete "remove" (vm-labels-of msg)))
      (should-not (member "remove" (vm-labels-of msg)))
      (should (member "keep" (vm-labels-of msg))))))

;;; vm-set-xxxx-flag accessor coverage
;; Note: flag accessors are named vm-xxx-flag not vm-xxx-flag-of

(ert-deftest vm-mark-test-flag-accessors ()
  "Test message flag accessors."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      ;; Test new flag
      (vm-set-new-flag-of msg t)
      (should (vm-new-flag msg))
      (vm-set-new-flag-of msg nil)
      (should-not (vm-new-flag msg))

      ;; Test deleted flag
      (vm-set-deleted-flag-of msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag-of msg nil)

      ;; Test replied flag
      (vm-set-replied-flag-of msg t)
      (should (vm-replied-flag msg))
      (vm-set-replied-flag-of msg nil)

      ;; Test forwarded flag
      (vm-set-forwarded-flag-of msg t)
      (should (vm-forwarded-flag msg))
      (vm-set-forwarded-flag-of msg nil)

      ;; Test flagged flag
      (vm-set-flagged-flag-of msg t)
      (should (vm-flagged-flag msg))
      (vm-set-flagged-flag-of msg nil))))

;;; Integration test with virtual selectors

(ert-deftest vm-mark-test-vs-label-integration ()
  "Test vm-vs-label selector with labels."
  (vm-test-with-folder vm-mark-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-labels-of msg '("important" "work"))
      (should (vm-vs-label msg "important"))
      (should (vm-vs-label msg "work"))
      (should-not (vm-vs-label msg "personal")))))

;;; Marking messages (emacs-vm/vm#628)
;;
;; The nineteen mark commands the manual documents had no test between them.
;; Marks are how a command is applied to a set of messages -- `vm-mark-message'
;; then `vm-next-command-uses-marks' -- so a mark that is set on the wrong
;; message is a delete or a save on the wrong message.

(defconst vm-mark-test--folder
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: Alice Adams <alice@example.com>\n"
          "Message-ID: <one@example.com>\nSubject: badgers\n\nOne.\n\n"
          "From bob@example.com Sun Aug  9 09:00:00 2026\n"
          "From: Bob Brown <bob@example.com>\n"
          "Message-ID: <two@example.com>\nReferences: <one@example.com>\n"
          "Subject: Re: badgers\n\nTwo.\n\n"
          "From alice@example.com Mon Aug 10 10:00:00 2026\n"
          "From: Alice Adams <alice@example.com>\n"
          "Message-ID: <three@example.com>\nSubject: the roof\n\nThree.\n\n")
  "Three messages: two from Alice, two in one thread, two subjects.")

(defmacro vm-mark-test--with-folder (spec &rest body)
  "Visit a folder of `vm-mark-test--folder' and run BODY.
SPEC is (FOLDER-VAR)."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-mark" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "marked" dir))
               (vm-frame-per-folder nil)
               (vm-mutable-frame-configuration nil)
               (vm-virtual-folder-alist nil))
           (write-region vm-mark-test--folder nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-mark-test--marked ()
  "The subjects of the marked messages, in folder order."
  (let (marked)
    (dolist (m vm-message-list)
      (when (vm-mark-of m) (push (vm-su-subject m) marked)))
    (nreverse marked)))

(ert-deftest vm-mark-test-marking-one-message-and-a-count ()
  "`vm-mark-message' marks the current message, and N of them with a count.
A negative count marks backwards from the current message, which is what its
docstring promises and the only way to mark the ones above."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-message 1)
    (should (equal (vm-mark-test--marked) '("badgers")))
    (vm-clear-all-marks)
    (vm-mark-message 2)
    (should (equal (vm-mark-test--marked) '("badgers" "Re: badgers")))
    (vm-clear-all-marks)
    ;; from the last message, backwards
    (setq vm-message-pointer (cdr (cdr vm-message-list)))
    (vm-mark-message -2)
    (should (equal (vm-mark-test--marked) '("Re: badgers" "the roof")))))

(ert-deftest vm-mark-test-unmarking-takes-a-count-too ()
  "`vm-unmark-message' is the same in reverse, count and all."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-all-messages)
    (should (= (length (vm-mark-test--marked)) 3))
    (vm-unmark-message 2)
    (should (equal (vm-mark-test--marked) '("the roof")))))

(ert-deftest vm-mark-test-all-none-and-the-toggle ()
  "`vm-mark-all-messages', `vm-clear-all-marks' and `vm-toggle-all-marks'."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-all-messages)
    (should (= (length (vm-mark-test--marked)) 3))
    (vm-clear-all-marks)
    (should-not (vm-mark-test--marked))
    ;; the toggle turns the unmarked on
    (vm-toggle-all-marks)
    (should (= (length (vm-mark-test--marked)) 3))
    ;; and back off
    (vm-toggle-all-marks)
    (should-not (vm-mark-test--marked))
    ;; a mixture is inverted message by message, not all set or all cleared
    (vm-mark-message 1)
    (vm-toggle-all-marks)
    (should (equal (vm-mark-test--marked) '("Re: badgers" "the roof")))))

(ert-deftest vm-mark-test-marking-by-author-and-subject ()
  "The same-author and same-subject commands take the current message's own.
Alice wrote two of the three and one subject is shared by two, so a command
that marked everything, or only the current message, would show here."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-messages-same-author)
    (should (equal (vm-mark-test--marked) '("badgers" "the roof")))
    (vm-clear-all-marks)
    (vm-mark-messages-same-subject)
    ;; "Re: badgers" and "badgers" are the same subject once the reply prefix
    ;; is taken off, which is what makes this different from a string match
    (should (equal (vm-mark-test--marked) '("badgers" "Re: badgers")))))

(ert-deftest vm-mark-test-unmarking-by-author-and-subject ()
  "The unmark halves take the same messages out again."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-all-messages)
    (vm-unmark-messages-same-author)
    (should (equal (vm-mark-test--marked) '("Re: badgers")))
    (vm-mark-all-messages)
    (vm-unmark-messages-same-subject)
    (should (equal (vm-mark-test--marked) '("the roof")))))

(ert-deftest vm-mark-test-marking-by-a-selector ()
  "`vm-mark-messages-by-selector' takes any virtual folder selector.
That is the general one the others are shorthand for."
  (vm-mark-test--with-folder (_folder)
    (vm-mark-messages-by-selector 'author "bob")
    (should (equal (vm-mark-test--marked) '("Re: badgers")))
    (vm-clear-all-marks)
    (vm-mark-messages-by-selector 'subject "roof")
    (should (equal (vm-mark-test--marked) '("the roof")))
    ;; and the unmark half
    (vm-mark-all-messages)
    (vm-unmark-messages-by-selector 'author "alice")
    (should (equal (vm-mark-test--marked) '("Re: badgers")))))

(ert-deftest vm-mark-test-marking-a-thread-subtree ()
  "`vm-mark-thread-subtree' marks the message and its replies.
The third message is not in the thread, so a command that marked the folder
would show."
  (vm-mark-test--with-folder (_folder)
    (let ((vm-summary-show-threads t))
      (vm-build-threads-if-unbuilt)
      (vm-mark-thread-subtree)
      (should (equal (vm-mark-test--marked) '("badgers" "Re: badgers")))
      (vm-unmark-thread-subtree)
      (should-not (vm-mark-test--marked)))))

(ert-deftest vm-mark-test-the-next-command-uses-marks ()
  "`vm-next-command-uses-marks' makes the next command act on the marked set.
It works by leaving its own name in `last-command', which the operable-message
selection reads -- so this checks the mechanism the whole marking system rests
on rather than the command's own return value."
  (vm-mark-test--with-folder (_folder)
    ;; mark a message other than the current one, so the two answers differ
    (setq vm-message-pointer (cdr (cdr vm-message-list)))
    (vm-mark-message 1)
    (setq vm-message-pointer vm-message-list)
    (vm-next-command-uses-marks)
    (should (eq this-command 'vm-next-command-uses-marks))
    (let ((last-command 'vm-next-command-uses-marks))
      (should (equal (mapcar #'vm-su-subject
                             (vm-select-operable-messages 1 nil "Test"))
                     '("the roof"))))
    ;; and without it, the current message rather than the marked one
    (let ((last-command nil))
      (should (equal (mapcar #'vm-su-subject
                             (vm-select-operable-messages 1 nil "Test"))
                     '("badgers"))))))

(ert-deftest vm-mark-test-marking-a-region-of-the-summary ()
  "`vm-mark-summary-region' marks the messages whose lines are in the region.
Point and the mark are in the summary buffer, so this is the one pair of mark
commands that reads the summary's text rather than the message list."
  (vm-mark-test--with-folder (_folder)
    (with-current-buffer vm-summary-buffer
      ;; a region over the first two summary lines
      (goto-char (point-min))
      (set-mark (point))
      (forward-line 2)
      (vm-mark-summary-region))
    (should (equal (vm-mark-test--marked) '("badgers" "Re: badgers")))
    ;; and the unmark half takes them out again
    (with-current-buffer vm-summary-buffer
      (goto-char (point-min))
      (set-mark (point))
      (forward-line 1)
      (vm-unmark-summary-region))
    (should (equal (vm-mark-test--marked) '("Re: badgers")))))

(ert-deftest vm-mark-test-a-region-command-needs-a-region ()
  "Both refuse when there is no mark, rather than marking the whole folder."
  (vm-mark-test--with-folder (_folder)
    (with-current-buffer vm-summary-buffer
      (set-mark nil)
      (let ((text-quoting-style 'grave))
        (should (string-match-p "region"
                                (cadr (should-error (vm-mark-summary-region)))))))
    (should-not (vm-mark-test--marked))))

(ert-deftest vm-mark-test-marking-by-virtual-folder ()
  "`vm-mark-messages-by-virtual-folder' marks the messages a named virtual
folder's selectors pick out, and `vm-unmark-messages-by-virtual-folder'
unmarks them.

The folder is not visited: its selectors are applied to the messages here,
with the folder list replaced by this buffer.  That is what makes the command
useful -- a virtual folder definition doubles as a saved search to mark by."
  (vm-mark-test--with-folder (folder)
    (let ((vm-virtual-folder-alist
           (list (list "from-alice" (list (list folder) '(author "alice")))
                 (list "about-roofs" (list (list folder) '(subject "roof"))))))
      (vm-mark-messages-by-virtual-folder "from-alice")
      (should (equal (vm-mark-test--marked) '("badgers" "the roof")))
      ;; a second definition marks another message without clearing the first
      (vm-mark-messages-by-virtual-folder "about-roofs")
      (should (equal (vm-mark-test--marked) '("badgers" "the roof")))
      (vm-unmark-messages-by-virtual-folder "from-alice")
      (should (equal (vm-mark-test--marked) nil)))))

(ert-deftest vm-mark-test-marking-by-a-virtual-folder-that-does-not-exist ()
  "A name no virtual folder has says so, rather than marking nothing in
silence."
  (vm-mark-test--with-folder (_folder)
    (let ((vm-virtual-folder-alist nil)
          (text-quoting-style 'grave))
      (should (equal (cadr (should-error
                            (vm-mark-messages-by-virtual-folder "nowhere")))
                     "No such virtual folder, nowhere")))))

(ert-deftest vm-mark-test-unmarking-by-virtual-folder-leaves-the-rest ()
  "Unmarking by a virtual folder takes the mark off the messages it selects
and leaves any other marks alone."
  (vm-mark-test--with-folder (folder)
    (let ((vm-virtual-folder-alist
           (list (list "about-roofs" (list (list folder) '(subject "roof"))))))
      (dolist (m vm-message-list)
        (vm-set-mark-of m t))
      (should (equal (length (vm-mark-test--marked)) 3))
      (vm-unmark-messages-by-virtual-folder "about-roofs")
      (should (equal (vm-mark-test--marked) '("badgers" "Re: badgers"))))))

(provide 'vm-mark-test)

;;; vm-mark-test.el ends here
