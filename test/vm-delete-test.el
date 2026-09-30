;;; vm-delete-test.el --- Tests for vm-delete.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM delete functions in vm-delete.el

;;; Code:

(require 'vm-test-init)
(require 'vm-delete)

;;; Delete function existence tests

(ert-deftest vm-delete-test-functions-exist ()
  "Test that delete functions exist."
  (should (fboundp 'vm-delete-message))
  (should (fboundp 'vm-delete-message-backward))
  (should (fboundp 'vm-undelete-message))
  (should (fboundp 'vm-toggle-flag-message))
  (should (fboundp 'vm-kill-subject))
  (should (fboundp 'vm-kill-thread-subtree))
  (should (fboundp 'vm-delete-duplicate-messages))
  (should (fboundp 'vm-delete-duplicate-messages-by-body))
  (should (fboundp 'vm-expunge-folder))
  (should (fboundp 'vm-expunge-message)))

;;; vm-expunge-folder keyword args

(ert-deftest vm-delete-test-expunge-folder-accepts-keywords ()
  "Test that vm-expunge-folder accepts keyword arguments."
  ;; Just verify the function signature accepts :quiet
  (should (equal (car (func-arity 'vm-expunge-folder)) 0)))

;;; Behavioral tests for vm-set-deleted-flag

(ert-deftest vm-delete-test-set-deleted-flag ()
  "Test that vm-set-deleted-flag sets the deleted attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test message
Message-ID: <test1@example.com>

Body text
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not deleted
      (should-not (vm-deleted-flag msg))
      ;; Set deleted flag
      (vm-set-deleted-flag msg t)
      (should (vm-deleted-flag msg))
      ;; Unset deleted flag
      (vm-set-deleted-flag msg nil)
      (should-not (vm-deleted-flag msg)))))

;;; Behavioral tests for vm-expunge-message

(ert-deftest vm-delete-test-expunge-message-removes-from-list ()
  "Test that vm-expunge-message removes message from vm-message-list."
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
    (let ((msg1 (vm-test-first-message)))
      (vm-expunge-message msg1)
      (should (= 1 (vm-test-message-count)))
      ;; The remaining message should be message 2
      (should (string-match "Message 2"
                            (vm-test-message-header (vm-test-first-message) "Subject"))))))

(ert-deftest vm-delete-test-expunge-message-sets-expunged-marker ()
  "Test that expunged messages have deleted-flag set to 'expunged."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test 1
Message-ID: <test1@example.com>

Body 1

From sender@example.com Mon Jan  2 00:00:00 2024
From: sender@example.com
Subject: Test 2
Message-ID: <test2@example.com>

Body 2
"
    ;; Need at least 2 messages so expunge doesn't leave empty list
    (should (= 2 (vm-test-message-count)))
    (let ((msg (vm-test-first-message)))
      ;; Set reverse links (needed by vm-expunge-message)
      (vm-set-reverse-link-of (vm-test-nth-message 1) vm-message-list)
      (vm-expunge-message msg)
      ;; After expunge, deleted-flag should be 'expunged
      ;; (vm-deleted-flag accesses slot 2 of the attributes vector)
      (should (eq 'expunged (vm-deleted-flag msg))))))

;;; Expunging and reverse links (issue #453)

;; `vm-expunge-message' derives which cons of `vm-message-list' to splice from
;; the message's reverse link, and `vm-expunge-folder' separately deletes the
;; message's text from the folder buffer.  If the link is wrong the two
;; disagree: the wrong message leaves the list while the right one's text is
;; deleted.  These walk the whole list after each expunge rather than checking
;; only the count, because a count survives that mix-up unchanged.

(defconst vm-delete-test--five-messages
  (mapconcat
   (lambda (i)
     (format "From sender@example.com Mon Jan  %d 00:00:00 2024
From: sender@example.com
Subject: Message %d
Message-ID: <test%d@example.com>

Body %d
" (1+ i) i i i))
   (number-sequence 0 4) "\n")
  "A five-message From_ folder, subjects \"Message 0\" through \"Message 4\".")

(defun vm-delete-test--subjects ()
  "Return the subject number of each message in `vm-message-list', in order."
  (mapcar (lambda (m)
            (string-to-number
             (replace-regexp-in-string
              "[^0-9]" "" (vm-test-message-header m "Subject"))))
          vm-message-list))

(ert-deftest vm-delete-test-expunge-the-last-message ()
  "Expunging the tail leaves the rest linked, and the new tail has a link.
The tail is the case where the cons spliced out has no successor to relink."
  (vm-test-with-folder vm-delete-test--five-messages
    (should (equal '(0 1 2 3 4) (vm-delete-test--subjects)))
    (vm-expunge-message (vm-test-nth-message 4))
    (should (equal '(0 1 2 3) (vm-delete-test--subjects)))
    (should (vm-test-reverse-links-consistent-p))
    (should (eq (car (vm-reverse-link-of (vm-test-nth-message 3)))
                (vm-test-nth-message 2)))))

(ert-deftest vm-delete-test-expunge-every-message-front-to-back ()
  "Expunging the head repeatedly empties the folder, in order.
Each expunge makes the next message the head, which must lose its link."
  (vm-test-with-folder vm-delete-test--five-messages
    (dotimes (i 5)
      (vm-expunge-message (vm-test-first-message))
      (should (equal (number-sequence (1+ i) 4) (vm-delete-test--subjects)))
      (should (vm-test-reverse-links-consistent-p))
      (when vm-message-list
        (should (null (vm-reverse-link-of (vm-test-first-message))))))
    (should (null vm-message-list))))

(ert-deftest vm-delete-test-expunge-every-message-back-to-front ()
  "Expunging the tail repeatedly empties the folder, in order.
The other direction, because the two take different branches on whether the
spliced cons has a successor."
  (vm-test-with-folder vm-delete-test--five-messages
    (dotimes (i 5)
      (vm-expunge-message (car (last vm-message-list)))
      (should (equal (number-sequence 0 (- 3 i)) (vm-delete-test--subjects)))
      (should (vm-test-reverse-links-consistent-p)))
    (should (null vm-message-list))))

(ert-deftest vm-delete-test-expunge-from-the-middle-outwards ()
  "A scattered set of expunges leaves the survivors correctly linked.
The interesting case is expunging two messages that were adjacent, so the
survivor must end up linked past both."
  (vm-test-with-folder vm-delete-test--five-messages
    (let ((m1 (vm-test-nth-message 1))
          (m2 (vm-test-nth-message 2)))
      (vm-expunge-message m1)
      (should (vm-test-reverse-links-consistent-p))
      (vm-expunge-message m2)
      (should (equal '(0 3 4) (vm-delete-test--subjects)))
      (should (vm-test-reverse-links-consistent-p))
      ;; Message 3 now follows message 0 directly.
      (should (eq (car (vm-reverse-link-of (vm-test-nth-message 1)))
                  (vm-test-first-message))))))

(ert-deftest vm-delete-test-expunge-moves-the-message-pointer-back ()
  "Expunging the selected message selects its predecessor, not another message.
`vm-expunge-message' finds it through the reverse link, so a wrong link moves
the user somewhere else in the folder."
  (vm-test-with-folder vm-delete-test--five-messages
    (setq vm-message-pointer (nthcdr 3 vm-message-list))
    (vm-expunge-message (vm-test-nth-message 3))
    (should (equal '(0 1 2 4) (vm-delete-test--subjects)))
    (should (vm-test-reverse-links-consistent-p))
    ;; Predecessor of the expunged message, which is "Message 2".
    (should (= 2 (string-to-number
                  (replace-regexp-in-string
                   "[^0-9]" ""
                   (vm-test-message-header (car vm-message-pointer)
                                           "Subject")))))))

;;; Tests for flagged status

(ert-deftest vm-delete-test-flagged-flag ()
  "Test that vm-set-flagged-flag toggles the flagged attribute."
  (vm-test-with-folder
    "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test
Message-ID: <test@example.com>

Body
"
    (let ((msg (vm-test-first-message)))
      ;; Initially not flagged
      (should-not (vm-flagged-flag msg))
      ;; Set flagged
      (vm-set-flagged-flag msg t)
      (should (vm-flagged-flag msg))
      ;; Unset flagged
      (vm-set-flagged-flag msg nil)
      (should-not (vm-flagged-flag msg)))))

;;; k must not delete the whole folder (issue #496)

(defun vm-delete-test--find-and-set-text-of-moving-point (m)
  "The pre-#492 `vm-find-and-set-text-of': computes the marker, moves point.
Kept here as the hazard `vm-get-header-contents' must not care about."
  (with-current-buffer (vm-buffer-of m)
    (save-restriction
      (widen)
      (goto-char (vm-headers-of m))
      (search-forward "\n\n" (vm-text-end-of m) 0)
      (vm-set-text-of m (point-marker)))))

(defun vm-delete-test--cool-message (m)
  "Forget what is cached about M, so the next access recomputes it."
  (fillarray (vm-cached-data-of m) nil)
  (aset (vm-location-data-of m) 3 nil))   ; vm-text-of

(defconst vm-delete-test--subjects-folder
  (concat "From s0@example.com Mon Jan  1 00:00:00 2024\n"
          "From: S0 <s0@example.com>\nSubject: shared subject\n"
          "Message-ID: <k-0@example.com>\n\nBody 0.\n\n"
          "From s1@example.com Mon Jan  1 00:00:00 2024\n"
          "From: S1 <s1@example.com>\nSubject: shared subject\n"
          "Message-ID: <k-1@example.com>\n\nBody 1.\n\n"
          "From s2@example.com Mon Jan  1 00:00:00 2024\n"
          "From: S2 <s2@example.com>\nSubject: unique two\n"
          "Message-ID: <k-2@example.com>\n\nBody 2.\n\n"
          "From s3@example.com Mon Jan  1 00:00:00 2024\n"
          "From: S3 <s3@example.com>\nSubject: unique three\n"
          "Message-ID: <k-3@example.com>\n\nBody 3.\n\n")
  "Four messages, the first two sharing a subject.")

(ert-deftest vm-delete-test-kill-subject-reads-cold-subjects ()
  "REGRESSION: reading a header does not depend on point being left alone.
Issue #496: `k' sometimes deleted every message in the folder, on the first
press after visiting.  `vm-get-header-contents' went to the start of the headers
and then evaluated `(vm-text-of message)' as the bound of its search --
and `vm-text-of' computes that marker on first use.  While that computation moved
point (issue #492, since fixed), the search ran from the body with a bound behind
it and matched nothing, so every header came back empty.  Every subject was then
\"\", every message compared equal to the current one, and `vm-kill-subject'
deleted the lot.

The bound is now computed before point moves, so the old hazard is installed here
deliberately: with it in place the subjects must still read correctly."
  (vm-test-with-folder vm-delete-test--subjects-folder
    (should (= 4 (vm-test-message-count)))
    ;; Cold, as before anything has asked for a subject.
    (mapc #'vm-delete-test--cool-message vm-message-list)
    (cl-letf (((symbol-function 'vm-find-and-set-text-of)
               (symbol-function
                'vm-delete-test--find-and-set-text-of-moving-point)))
      (should (equal '("shared subject" "shared subject"
                       "unique two" "unique three")
                     (mapcar #'vm-so-sortable-subject vm-message-list))))))

(ert-deftest vm-delete-test-kill-subject-kills-only-the-subject ()
  "REGRESSION: `k' deletes the messages sharing a subject and no others.
The behaviour issue #496 is about, with the subject cache cold and the pre-#492
hazard in place -- the combination that deleted the whole folder."
  (vm-test-with-folder vm-delete-test--subjects-folder
    (mapc #'vm-delete-test--cool-message vm-message-list)
    (setq vm-message-pointer vm-message-list)
    ;; `vm-kill-subject' validates that it is in a folder buffer.
    (setq major-mode 'vm-mode)
    (setq vm-mail-buffer nil)
    (cl-letf (((symbol-function 'vm-find-and-set-text-of)
               (symbol-function
                'vm-delete-test--find-and-set-text-of-moving-point))
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-display) (lambda (&rest _) nil)))
      ;; 0 means do not move afterwards, so the test does not need a summary.
      (vm-kill-subject 0))
    (should (equal '(t t nil nil)
                   (mapcar (lambda (m) (and (vm-deleted-flag m) t))
                           vm-message-list)))))

;;; The reverse link guard (issue #570)

;; `vm-expunge-message' takes the cons to splice from the message's reverse
;; link, so a wrong link removes a different message and flags that one
;; expunged, silently.  #569 is a reachable path to it.  These pin that the
;; guard fires and that it fires before anything is modified.

(defconst vm-delete-test--three-messages
  (mapconcat
   (lambda (i)
     (format (concat "From sender@example.com Mon Jan  %d 00:00:00 2024\n"
                     "From: sender@example.com\n"
                     "Subject: message %d\n"
                     "Message-ID: <guard-%d@example.com>\n"
                     "\n"
                     "Body %d.\n\n")
             (1+ i) i i i))
   '(0 1 2) "")
  "A folder of three messages, enough to have a head, a middle and a tail.")

(ert-deftest vm-delete-test-expunge-message-refuses-a-stale-reverse-link ()
  "A message whose reverse link points elsewhere is not expunged.
The link decides which cons is spliced, so following it here would remove
message 2 and mark it expunged while message 3 stayed in the folder.  The structure
is the one #569 produces: a message dropped from the list whose link still
points at where it used to be."
  (vm-test-with-folder vm-delete-test--three-messages
    (let* ((m2 (vm-test-nth-message 1))
           (m3 (vm-test-nth-message 2))
           (list-before vm-message-list))
      (should (eq m2 (car (vm-reverse-link-of m3))))
      (vm-set-reverse-link-of m3 vm-message-list)
      (should-error (vm-expunge-message m3))
      ;; Nothing was spliced and nothing was flagged.
      (should (eq list-before vm-message-list))
      (should (= 3 (vm-test-message-count)))
      (should-not (vm-deleted-flag m2))
      (should-not (vm-deleted-flag m3)))))

(ert-deftest vm-delete-test-expunge-message-refuses-a-missing-reverse-link ()
  "A message with no reverse link is expunged only if it is the folder head.
No link reads as \"first in the list\", so before the guard this expunged
message 1 in place of message 2."
  (vm-test-with-folder vm-delete-test--three-messages
    (let* ((m1 (vm-test-first-message))
           (m2 (vm-test-nth-message 1)))
      (vm-set-reverse-link-of m2 nil)
      (should-error (vm-expunge-message m2))
      (should (= 3 (vm-test-message-count)))
      (should (eq m1 (vm-test-first-message)))
      (should-not (vm-deleted-flag m1))
      ;; The head itself has no link and still expunges.
      (vm-set-reverse-link-of m2 vm-message-list)
      (vm-expunge-message m1)
      (should (= 2 (vm-test-message-count)))
      (should (eq m2 (vm-test-first-message))))))

(ert-deftest vm-delete-test-expunge-message-guard-precedes-any-change ()
  "The guard fires before the message is unregistered as fetched.
`vm-unregister-fetched-message' ran first, so a refused expunge used to leave
the folder's fetched-message bookkeeping already changed."
  (vm-test-with-folder vm-delete-test--three-messages
    (let ((m3 (vm-test-nth-message 2))
          (unregistered nil))
      (vm-set-reverse-link-of m3 vm-message-list)
      (cl-letf (((symbol-function 'vm-unregister-fetched-message)
                 (lambda (&rest _) (setq unregistered t))))
        (should-error (vm-expunge-message m3)))
      (should-not unregistered))))

;;; What else expunging a message has to let go of

;; Two things `vm-expunge-message' does besides splicing the list, both of them
;; leaving a reference to a message that is no longer in the folder if they are
;; missed, and neither of them noticed by any test until now.

(ert-deftest vm-delete-test-expunge-drops-a-pointer-to-the-expunged-message ()
  "`vm-last-message-pointer' is cleared when it holds the expunged message.
It is the cons, not the message, so what it would otherwise hold is a cons
spliced out of the list: `p' after an expunge would present a message the
folder no longer has."
  (vm-test-with-folder vm-delete-test--three-messages
    (let ((m2 (vm-test-nth-message 1)))
      (setq vm-last-message-pointer (cdr vm-message-list))
      (should (eq m2 (car vm-last-message-pointer)))
      (vm-expunge-message m2)
      (should (null vm-last-message-pointer)))))

(ert-deftest vm-delete-test-expunge-keeps-a-pointer-to-another-message ()
  "Expunging one message leaves `vm-last-message-pointer' at another alone.
The other half of the same branch: it is cleared because it points at the
message going away, not on every expunge."
  (vm-test-with-folder vm-delete-test--three-messages
    (let ((head vm-message-list))
      (setq vm-last-message-pointer head)
      (vm-expunge-message (vm-test-nth-message 1))
      (should (eq head vm-last-message-pointer))
      (should (eq (vm-test-first-message) (car vm-last-message-pointer))))))

(ert-deftest vm-delete-test-expunge-cancels-a-scheduled-summary-update ()
  "The summary line position recorded on a message is cleared as it goes.
`vm-su-start-of' is where the message's line sits in the summary buffer.  An
expunged message keeps its `expunged' flag so the undo machinery can recognise
it, but a stale position would have the next summary update write over another
message's line."
  (vm-test-with-folder vm-delete-test--three-messages
    (let ((m2 (vm-test-nth-message 1)))
      (with-temp-buffer
        (insert "a summary line\n")
        (vm-set-su-start-of m2 (point-min-marker)))
      (should (vm-su-start-of m2))
      (vm-expunge-message m2)
      (should (null (vm-su-start-of m2)))
      ;; The flag the undo machinery reads is still there.
      (should (eq 'expunged (vm-deleted-flag m2))))))

;;; Expunging with a mirror in a killed buffer (issue #571)

;; Killing a virtual folder buffer instead of quitting it does not deregister
;; its messages, so the real message keeps a mirror whose buffer is gone.  Step
;; 2 of `vm-expunge-folder' walks those mirrors; without a liveness check it
;; signals `Selecting deleted buffer', leaves the expunge half done, and
;; signals again in the same place next time, so the folder can never be
;; expunged.  Every other walker of `vm-virtual-messages-of' guards for this.

(defmacro vm-delete-test--with-real-and-virtual (spec &rest body)
  "Visit a generated real folder and a virtual folder over it, run BODY.
SPEC is (REAL-VAR VIRT-VAR &optional N), each buffer var.  Everything the
visits created is killed afterwards, and killing one inside BODY is fine."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-expunge" t)))
          (file (expand-file-name "real-folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-summary-show-threads nil)
          (vm-virtual-folder-alist nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list))
          ,(car spec) ,(nth 1 spec))
     (require 'vm)
     (unwind-protect
         (progn
           (with-temp-file file
             (dotimes (i ,(or (nth 2 spec) 4))
               (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                       "From: alice@example.com\n"
                       (format "Subject: subject %d\n" i)
                       (format "Message-ID: <exp-%d@example.com>\n" i)
                       "\n" (format "Body %d.\n\n" i))))
           (setq vm-virtual-folder-alist
                 (list (list "expunge-virt" (list (list file) '(any)))))
           (vm-visit-folder file)
           (setq ,(car spec) (current-buffer))
           (vm-visit-virtual-folder "expunge-virt")
           (setq ,(nth 1 spec) (current-buffer))
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-delete-test--subjects-of (buffer)
  "Return the subjects of BUFFER's message list, in order."
  (with-current-buffer buffer
    (mapcar #'vm-su-subject vm-message-list)))

(ert-deftest vm-delete-test-expunge-past-a-mirror-in-a-killed-buffer ()
  "REGRESSION: a killed virtual folder does not stop the real folder expunging.
Issue #571.  The mirror is still registered on the real message and its buffer
is gone."
  (vm-delete-test--with-real-and-virtual (real virt)
    (let (mirrored)
      (with-current-buffer real
        (setq mirrored (nth 1 vm-message-list))
        (should (= 1 (length (vm-virtual-messages-of mirrored)))))
      (with-current-buffer virt (set-buffer-modified-p nil))
      (kill-buffer virt)
      (should-not (buffer-live-p virt))
      ;; Still registered: that is the state the guard has to survive.
      (should (= 1 (length (vm-virtual-messages-of mirrored))))
      (with-current-buffer real
        (vm-set-deleted-flag mirrored t)
        (vm-expunge-folder))
      (should (equal '("subject 0" "subject 2" "subject 3")
                     (vm-delete-test--subjects-of real)))
      (with-current-buffer real
        (should (vm-test-reverse-links-consistent-p)))
      ;; And the dead mirror is off the list, so a second expunge is clean too.
      (should (null (vm-virtual-messages-of mirrored)))
      (with-current-buffer real
        (vm-set-deleted-flag (nth 1 vm-message-list) t)
        (vm-expunge-folder))
      (should (equal '("subject 0" "subject 3")
                     (vm-delete-test--subjects-of real))))))

(ert-deftest vm-delete-test-expunge-still-reaches-a-live-mirror ()
  "The liveness check does not skip mirrors that are still in a folder.
The other side of the branch: with the virtual folder open, expunging in the
real folder removes the message from both."
  (vm-delete-test--with-real-and-virtual (real virt)
    (with-current-buffer real
      (vm-set-deleted-flag (nth 1 vm-message-list) t)
      (vm-expunge-folder))
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-delete-test--subjects-of real)))
    (should (equal '("subject 0" "subject 2" "subject 3")
                   (vm-delete-test--subjects-of virt)))
    (with-current-buffer virt
      (should (vm-test-reverse-links-consistent-p)))))
;;; What an expunge that expunged nothing reports (issue #572)

;; `vm-expunge-folder' has a message for the case where no message is flagged,
;; guarded by (null buffers-altered).  That is an obarray, hence a vector, hence
;; never nil, so the message could not appear and VM said the deleted messages
;; had been expunged instead.  The same dead branch re-sorted a folder that
;; nothing had been expunged from.

(defun vm-delete-test--said (thunk)
  "Return the messages emitted while THUNK runs."
  (let ((said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (push (if (car args) (apply #'format args) "") said)
                 (car said))))
      (funcall thunk))
    (nreverse said)))

(defun vm-delete-test--said-p (said text)
  "Return non-nil if any of SAID ends in TEXT.
The messages are prefixed with the folder buffer's name, which for these is a
temporary buffer."
  (let ((found nil))
    (dolist (s said)
      (when (string-suffix-p text s) (setq found t)))
    found))

(defmacro vm-delete-test--expungeable (&rest body)
  "Run BODY in a three-message folder that `vm-expunge-folder' will accept.
Enough of a folder buffer to pass validation, with the display work stubbed
out: what is under test is what the command reports, not what it draws."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder vm-delete-test--three-messages
     (setq major-mode 'vm-mode)
     (setq vm-mail-buffer nil)
     (setq vm-message-pointer vm-message-list)
     (cl-letf (((symbol-function 'vm-display) (lambda (&rest _) nil))
               ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
               ((symbol-function 'vm-present-current-message) #'ignore)
               ((symbol-function 'vm-garbage-collect-message) #'ignore))
       ,@body)))

(ert-deftest vm-delete-test-expunge-with-nothing-deleted-says-so ()
  "REGRESSION: an expunge with nothing flagged does not claim to have expunged.
Issue #572."
  (vm-delete-test--expungeable
    (let ((said (vm-delete-test--said (lambda () (vm-expunge-folder)))))
      (should (vm-delete-test--said-p said "No messages are flagged for deletion."))
      (should-not (vm-delete-test--said-p said "Deleted messages expunged."))
      (should (= 3 (vm-test-message-count))))))

(ert-deftest vm-delete-test-expunge-with-something-deleted-says-that ()
  "An expunge that did expunge still reports that, and removes the message.
The other side of the branch, so the fix is not simply reporting the new
message every time."
  (vm-delete-test--expungeable
    (vm-set-deleted-flag (vm-test-nth-message 1) t)
    (let ((said (vm-delete-test--said (lambda () (vm-expunge-folder)))))
      (should (vm-delete-test--said-p said "Deleted messages expunged."))
      (should-not (vm-delete-test--said-p said "No messages are flagged for deletion."))
      (should (= 2 (vm-test-message-count))))))

(ert-deftest vm-delete-test-quiet-expunge-with-nothing-deleted-is-quiet ()
  "`:quiet t' silences the new message too.
It has to: vm-avirtual.el's spam auto-delete expunges quietly, and mostly finds
nothing to expunge.

Not under instrumentation: edebug says things of its own while the tests run,
and this one asks that nothing at all was said (emacs-vm/vm#870)."
  (skip-unless (not vm-test-instrumented))
  (vm-delete-test--expungeable
    (should (null (vm-delete-test--said
                   (lambda () (vm-expunge-folder :quiet t)))))))

(ert-deftest vm-delete-test-expunge-with-nothing-deleted-does-not-sort ()
  "Nothing expunged means nothing to renumber, so no sort either.
The dead branch put the sort on the path that runs when no message was flagged,
so every expunge of a sorted folder re-sorted it for nothing."
  (vm-delete-test--expungeable
    (let ((sorted nil)
          (vm-ml-sort-keys "date"))
      (cl-letf (((symbol-function 'vm-sort-messages)
                 (lambda (&rest _) (setq sorted t))))
        (vm-expunge-folder :quiet t)
        (should-not sorted)
        ;; And it does sort when something was expunged.
        (vm-set-deleted-flag (vm-test-nth-message 1) t)
        (vm-expunge-folder :quiet t)
        (should sorted)))))
;;; Killing a thread subtree

;; `vm-kill-thread-subtree' had no behavioural test, only a check that the
;; symbol was bound.  It is the sibling of `vm-kill-subject', which is what
;; #496 was: a kill command that deleted the whole folder.  What it must delete
;; is the message at point and its descendants, and nothing else.

(defconst vm-delete-test--thread-folder
  (mapconcat
   (lambda (spec)
     (let ((i (car spec)) (parent (cdr spec)))
       (concat (format "From alice@example.com Mon Jan  1 00:00:00 2024\n")
               "From: alice@example.com\n"
               (format "Subject: subject %d\n" i)
               (format "Message-ID: <kt-%d@example.com>\n" i)
               (if parent (format "References: <kt-%d@example.com>\n" parent) "")
               "\n" (format "Body %d.\n\n" i))))
   '((0 . nil) (1 . 0) (2 . 1) (3 . 0) (4 . nil))
   "")
  "Five messages structured 0 < 1 < 2, 0 < 3, and 4 on its own.")

(defun vm-delete-test--deleted-indices ()
  "Return the positions in `vm-message-list' of the messages flagged deleted."
  (let ((i -1) (out nil))
    (dolist (m vm-message-list)
      (setq i (1+ i))
      (when (vm-deleted-flag m) (push i out)))
    (nreverse out)))

(defmacro vm-delete-test--killable (n &rest body)
  "Run BODY in the thread folder with message N current and threads built.
Only the display work is stubbed: what the command selects for deletion is the
point of these, so the thread database is real."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder vm-delete-test--thread-folder
     (setq major-mode 'vm-mode)
     (setq vm-mail-buffer nil)
     (setq vm-message-pointer (nthcdr ,n vm-message-list))
     (let ((vm-move-after-killing nil)
           (vm-summary-show-threads t)
           ;; `vm-inform' records where it spoke when it thinks a command is
           ;; running, and the buffer these run in is gone afterwards.
           (vm-user-interaction-buffer vm-user-interaction-buffer))
       (cl-letf (((symbol-function 'vm-display) (lambda (&rest _) nil))
                 ((symbol-function 'vm-follow-summary-cursor) #'ignore)
                 ((symbol-function 'vm-update-summary-and-mode-line) #'ignore))
         ,@body))))

(ert-deftest vm-delete-test-kill-thread-subtree-takes-the-descendants ()
  "Killing at a message in the middle takes it and what descends from it.
Message 1 has message 2 below it; message 3 is its sibling and message 0 its
parent, and neither goes."
  (vm-delete-test--killable 1
    (vm-kill-thread-subtree 0)
    (should (equal '(1 2) (vm-delete-test--deleted-indices)))))

(ert-deftest vm-delete-test-kill-thread-subtree-at-the-root-takes-the-thread ()
  "Killing at the root of a thread takes the whole thread and nothing outside.
Message 4 is in the folder and in no thread with the others."
  (vm-delete-test--killable 0
    (vm-kill-thread-subtree 0)
    (should (equal '(0 1 2 3) (vm-delete-test--deleted-indices)))))

(ert-deftest vm-delete-test-kill-thread-subtree-at-a-leaf-takes-one ()
  "A message with nothing below it is the whole subtree."
  (vm-delete-test--killable 2
    (vm-kill-thread-subtree 0)
    (should (equal '(2) (vm-delete-test--deleted-indices)))))

(ert-deftest vm-delete-test-kill-thread-subtree-outside-a-thread-takes-one ()
  "A message that is in no thread takes only itself, not the folder.
The #496 case: a kill command with nothing to match on deleting everything."
  (vm-delete-test--killable 4
    (vm-kill-thread-subtree 0)
    (should (equal '(4) (vm-delete-test--deleted-indices)))))

(ert-deftest vm-delete-test-kill-thread-subtree-counts-what-it-deleted ()
  "The count is of messages it deleted, not of the size of the subtree.
Killing the same subtree twice deletes nothing the second time, and says so."
  (vm-delete-test--killable 0
    (let ((said nil))
      (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (&rest args)
                   (push (if (car args) (apply #'format args) "") said)
                   (car said))))
        (vm-kill-thread-subtree 0)
        (should (member "4 messages deleted" said))
        (should (equal '(0 1 2 3) (vm-delete-test--deleted-indices)))
        ;; Nothing left to delete in that subtree.
        (setq said nil)
        (vm-kill-thread-subtree 0)
        (should (member "No messages deleted." said))
        (should (equal '(0 1 2 3) (vm-delete-test--deleted-indices)))))))

;;; Deleting duplicates

;; `vm-delete-duplicate-messages' and its by-body variant flag messages for
;; deletion, so what they do not delete matters as much as what they do.  Their
;; coverage was three tests that re-implement the hash logic in the test file
;; and never call VM, so nothing exercised either command.

(defconst vm-delete-test--duplicates
  (concat
   "From alice@example.com Mon Jan  1 00:00:00 2024\n"
   "From: alice@example.com\nSubject: first copy\n"
   "Message-ID: <dup-a@example.com>\n\nShared body.\n\n"
   "From alice@example.com Mon Jan  1 00:00:01 2024\n"
   "From: alice@example.com\nSubject: second copy\n"
   "Message-ID: <dup-a@example.com>\n\nShared body.\n\n"
   "From alice@example.com Mon Jan  1 00:00:02 2024\n"
   "From: alice@example.com\nSubject: on its own\n"
   "Message-ID: <dup-b@example.com>\n\nA different body.\n\n")
  "Two messages with one message id between them, and a third with its own.")

(defmacro vm-delete-test--dedupable (content &rest body)
  "Run BODY in a folder of CONTENT that the duplicate commands will accept."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder ,content
     (setq major-mode 'vm-mode)
     (setq vm-mail-buffer nil)
     (cl-letf (((symbol-function 'vm-display) (lambda (&rest _) nil))
               ((symbol-function 'vm-update-summary-and-mode-line) #'ignore))
       ,@body)))

(defun vm-delete-test--deleted-flags ()
  "Return the deleted flag of each message as t or nil, in order."
  (mapcar (lambda (m) (and (vm-deleted-flag m) t)) vm-message-list))

(ert-deftest vm-delete-test-duplicates-by-id-keep-the-first-copy ()
  "The second message with a message id is flagged, the first is not.
Which copy survives is the point: the command walks the folder in order and
keeps the one it meets first."
  (vm-delete-test--dedupable vm-delete-test--duplicates
    (should (= 1 (vm-delete-duplicate-messages)))
    (should (equal '(nil t nil) (vm-delete-test--deleted-flags)))))

(ert-deftest vm-delete-test-duplicates-by-id-spare-the-last-copy ()
  "A copy already flagged does not claim the id, so the other copy survives.
This is the promise in the docstring: VM never deletes the last copy of a
message.  Deleted messages are skipped, so the id belongs to the first copy
that is still there."
  (vm-delete-test--dedupable vm-delete-test--duplicates
    (vm-set-deleted-flag-of (vm-test-first-message) t)
    (should (= 0 (vm-delete-duplicate-messages)))
    (should (equal '(t nil nil) (vm-delete-test--deleted-flags)))))

(ert-deftest vm-delete-test-duplicates-by-id-ignore-messages-with-no-id ()
  "Messages with no message id are never duplicates of each other.
There is nothing to compare, and flagging them would be flagging on the
strength of nothing."
  (vm-delete-test--dedupable
      (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
              "From: alice@example.com\nSubject: no id here\n\nBody.\n\n"
              "From alice@example.com Mon Jan  1 00:00:01 2024\n"
              "From: alice@example.com\nSubject: none here either\n\nBody.\n\n")
    (should (= 0 (vm-delete-duplicate-messages)))
    (should (equal '(nil nil) (vm-delete-test--deleted-flags)))))

(ert-deftest vm-delete-test-duplicates-by-body-compare-the-body ()
  "The by-body command flags the second message with the same body.
The two copies here have the same body and the third does not, and the ids are
not consulted at all."
  (vm-delete-test--dedupable vm-delete-test--duplicates
    (should (= 1 (vm-delete-duplicate-messages-by-body)))
    (should (equal '(nil t nil) (vm-delete-test--deleted-flags)))))

(ert-deftest vm-delete-test-duplicates-by-body-ignore-differing-bodies ()
  "Messages sharing a message id but not a body are left alone by the by-body
command, which is the whole reason for having both."
  (vm-delete-test--dedupable
      (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
              "From: alice@example.com\nSubject: one\n"
              "Message-ID: <same@example.com>\n\nOne body.\n\n"
              "From alice@example.com Mon Jan  1 00:00:01 2024\n"
              "From: alice@example.com\nSubject: two\n"
              "Message-ID: <same@example.com>\n\nAnother body entirely.\n\n")
    (should (= 0 (vm-delete-duplicate-messages-by-body)))
    (should (equal '(nil nil) (vm-delete-test--deleted-flags)))
    ;; ...and the by-id command does flag one of them.
    (should (= 1 (vm-delete-duplicate-messages)))
    (should (equal '(nil t) (vm-delete-test--deleted-flags)))))

(ert-deftest vm-delete-test-duplicates-by-body-spare-the-last-copy ()
  "A copy already flagged is not hashed, so the other copy survives."
  (vm-delete-test--dedupable vm-delete-test--duplicates
    (vm-set-deleted-flag-of (vm-test-first-message) t)
    (should (= 0 (vm-delete-duplicate-messages-by-body)))
    (should (equal '(t nil nil) (vm-delete-test--deleted-flags)))))

;;; The delete, undelete and flag commands

;; `vm-delete-message' had coverage only through `vm-kill-subject' and the
;; duplicate commands calling it; `vm-delete-message-backward',
;; `vm-undelete-message' and `vm-toggle-flag-message' had none.  All four take a
;; count and apply to a range, and `vm-select-operable-messages' is what turns
;; the count into that range, so a folder VM has visited is the honest place to
;; run them.

(defun vm-delete-test--flags (accessor)
  "Return ACCESSOR of each message in `vm-message-list' as t or nil."
  (mapcar (lambda (m) (and (funcall accessor m) t)) vm-message-list))

(defmacro vm-delete-test--with-five (&rest body)
  "Run BODY in a visited folder of five messages, message 1 current."
  (declare (indent 0) (debug t))
  `(vm-test-with-real-folder (5)
     (let ((vm-move-after-deleting nil)
           (vm-move-after-undeleting nil))
       (setq vm-message-pointer vm-message-list)
       ,@body)))

(ert-deftest vm-delete-test-delete-message-takes-a-count-forward ()
  "A count deletes that many messages from the current one on."
  (vm-delete-test--with-five
    (vm-delete-message 3)
    (should (equal '(t t t nil nil) (vm-delete-test--flags #'vm-deleted-flag)))))

(ert-deftest vm-delete-test-delete-message-backward-takes-the-count-back ()
  "`vm-delete-message-backward' is the same command with the count negated.
From the fourth message, three back is the second, third and fourth."
  (vm-delete-test--with-five
    (setq vm-message-pointer (nthcdr 3 vm-message-list))
    (vm-delete-message-backward 3)
    (should (equal '(nil t t t nil) (vm-delete-test--flags #'vm-deleted-flag)))))

(ert-deftest vm-delete-test-delete-message-stops-at-the-end-of-the-folder ()
  "A count past the end of the folder deletes what there is.
`vm-select-operable-messages' bounds the range; the command does not signal."
  (vm-delete-test--with-five
    (setq vm-message-pointer (nthcdr 3 vm-message-list))
    (vm-delete-message 99)
    (should (equal '(nil nil nil t t)
                   (vm-delete-test--flags #'vm-deleted-flag)))))

(ert-deftest vm-delete-test-undelete-message-takes-a-count ()
  "Undeleting undoes the flag on a range, leaving the rest deleted."
  (vm-delete-test--with-five
    (vm-delete-message 4)
    (should (equal '(t t t t nil) (vm-delete-test--flags #'vm-deleted-flag)))
    (setq vm-message-pointer vm-message-list)
    (vm-undelete-message 2)
    (should (equal '(nil nil t t nil)
                   (vm-delete-test--flags #'vm-deleted-flag)))))

(ert-deftest vm-delete-test-undelete-message-leaves-undeleted-ones-alone ()
  "Undeleting messages that are not deleted changes nothing and does not fail."
  (vm-delete-test--with-five
    (vm-undelete-message 3)
    (should (equal '(nil nil nil nil nil)
                   (vm-delete-test--flags #'vm-deleted-flag)))))

(ert-deftest vm-delete-test-delete-then-move-after-deleting ()
  "With `vm-move-after-deleting' set, the folder moves past what it deleted.
The default is nil, so this is the other configuration rather than the usual
one."
  (vm-test-with-real-folder (5)
    (let ((vm-move-after-deleting t)
          (vm-circular-folders nil))
      (setq vm-message-pointer vm-message-list)
      (vm-delete-message 2)
      (should (equal '(t t nil nil nil)
                     (vm-delete-test--flags #'vm-deleted-flag)))
      ;; Past the two it deleted, on the third message.
      (should (equal "subject 2" (vm-su-subject (car vm-message-pointer)))))))

(ert-deftest vm-delete-test-toggle-flag-message-flags-and-unflags ()
  "The flag command sets the flag when it is unset, and unsets it when set."
  (vm-delete-test--with-five
    (vm-toggle-flag-message 1)
    (should (equal '(t nil nil nil nil) (vm-delete-test--flags #'vm-flagged-flag)))
    (vm-toggle-flag-message 1)
    (should (equal '(nil nil nil nil nil)
                   (vm-delete-test--flags #'vm-flagged-flag)))))

(ert-deftest vm-delete-test-toggle-flag-message-follows-the-first-message ()
  "Over a range, every message is set to the opposite of the first one's flag.
Not each message toggled in turn: the command decides once, from the message it
starts at, so a range that is already mixed comes out uniform."
  (vm-delete-test--with-five
    (vm-set-flagged-flag (nth 1 vm-message-list) t)
    (should (equal '(nil t nil nil nil)
                   (vm-delete-test--flags #'vm-flagged-flag)))
    ;; The first of the three is unflagged, so all three end up flagged.
    (vm-toggle-flag-message 3)
    (should (equal '(t t t nil nil) (vm-delete-test--flags #'vm-flagged-flag)))
    ;; And now the first of the three is flagged, so all three come off.
    (vm-toggle-flag-message 3)
    (should (equal '(nil nil nil nil nil)
                   (vm-delete-test--flags #'vm-flagged-flag)))))

(ert-deftest vm-delete-test-expunge-moves-the-message-list-generation ()
  "Expunging a message moves `vm-message-list-generation'.
Anything following the list by its conses is told that way that a message has
gone and its cons may be the one that left.  `vm-imap-net-uids-held' follows
it so, a fetch asking it once per arriving message."
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
    (let ((generation vm-message-list-generation))
      (vm-expunge-message (vm-test-first-message))
      (should (= 1 (vm-test-message-count)))
      (should (> vm-message-list-generation generation)))))

;;; What an expunge tells the server (emacs-vm/vm#757)

(defconst vm-delete-test--two-messages
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
  "A two-message folder, for the expunge tests below.")

(defmacro vm-delete-test--in-an-imap-cache (&rest body)
  "Run BODY in a folder that is an IMAP cache of validity \"100\".
Its messages carry UIDs 1 upwards, as messages read out of a cache do."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder vm-delete-test--two-messages
     ;; `vm-select-folder-buffer-and-validate' is a defsubst, so the compiled
     ;; command has it inlined and it cannot be stubbed; it asks for the mode
     (setq major-mode 'vm-mode)
     (setq vm-folder-access-method 'imap
           vm-folder-access-data (make-vector vm-folder-imap-access-data-length
                                              nil))
     ;; a password in the spec, as a configured maildrop has: what is
     ;; recorded has to name the maildrop without it
     (aset vm-folder-access-data 0
           "imap:localhost:143:INBOX:login:reader:secret")
     (aset vm-folder-access-data 2 "100")
     (let ((uid 0))
       (dolist (message vm-message-list)
         (vm-set-imap-uid-of message (number-to-string (setq uid (1+ uid))))
         (vm-set-imap-uid-validity-of message "100")))
     ,@body))

(defun vm-delete-test--expunge-the-first (&rest arguments)
  "Expunge the folder's first message, passing ARGUMENTS to the expunge."
  (let ((message (vm-test-first-message)))
    (vm-set-deleted-flag message t)
    (apply #'vm-expunge-folder :quiet t :just-these-messages (list message)
           arguments)))

(ert-deftest vm-delete-test-an-expunge-queues-the-uid-for-the-server ()
  "The ordinary case: what the reader expunged is queued for its mailbox."
  (vm-delete-test--in-an-imap-cache
    (vm-delete-test--expunge-the-first)
    (should (equal vm-imap-messages-to-expunge '(("1" . "100"))))))

(ert-deftest vm-delete-test-a-message-with-no-uid-is-not-queued ()
  "A message that never came from the server is not queued for deletion there.
The queue used to collect (nil . nil) for one, which is a UID no mailbox has."
  (vm-delete-test--in-an-imap-cache
    (let ((message (vm-test-first-message)))
      (vm-set-imap-uid-of message nil)
      (vm-set-imap-uid-validity-of message nil))
    (vm-delete-test--expunge-the-first)
    (should-not vm-imap-messages-to-expunge)))

(ert-deftest vm-delete-test-a-stale-uid-is-not-queued ()
  "A UID under another UIDVALIDITY names nothing in the mailbox now.
`vm-imap-expunge-remote-messages' refuses those and says so, which tells the
reader about something they can do nothing about."
  (vm-delete-test--in-an-imap-cache
    (vm-set-imap-uid-validity-of (vm-test-first-message) "99")
    (vm-delete-test--expunge-the-first)
    (should-not vm-imap-messages-to-expunge)))

(ert-deftest vm-delete-test-what-the-server-has-lost-is-not-queued ()
  "A message expunged because the server no longer has it is not queued for it.
That is what a synchronise's local expunge is, and STORE and EXPUNGE on a UID
that is not there is a no-op at best; some servers answer NO."
  (vm-delete-test--in-an-imap-cache
    (vm-delete-test--expunge-the-first :not-on-the-server t)
    (should-not vm-imap-messages-to-expunge)
    ;; and it is still expunged here
    (should (= 1 (vm-test-message-count)))))

(ert-deftest vm-delete-test-an-expunge-records-the-uid-once ()
  "The UID of an expunged message is recorded as retrieved once, not again.

It was recorded when the message arrived, and that entry names the maildrop
without its password, so comparing whole entries saw no match and the list
grew one duplicate per expunge -- in a list written into the folder header on
every save."
  (vm-delete-test--in-an-imap-cache
    ;; as the arrival recorded it: the maildrop without its password
    (setq vm-imap-retrieved-messages
          (list (list "1" "100" "imap:localhost:143:INBOX:login:reader" 'uid)))
    (vm-delete-test--expunge-the-first)
    (should (equal (length vm-imap-retrieved-messages) 1))))

(ert-deftest vm-delete-test-an-expunge-records-a-uid-that-was-not-recorded ()
  "A UID no entry names is recorded, so a later synchronise leaves it alone."
  (vm-delete-test--in-an-imap-cache
    (setq vm-imap-retrieved-messages nil)
    (vm-delete-test--expunge-the-first)
    (should (equal (length vm-imap-retrieved-messages) 1))
    (should (equal (car (car vm-imap-retrieved-messages)) "1"))
    (should (equal (cadr (car vm-imap-retrieved-messages)) "100"))))

;;; What an expunge tells a POP maildrop (emacs-vm/vm#758)

(defmacro vm-delete-test--in-a-pop-folder (&rest body)
  "Run BODY in a folder whose access method is `pop', its messages given UIDLs.
The maildrop spec carries a password, as a configured one does: what is
recorded has to name the maildrop without it."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder vm-delete-test--two-messages
     ;; `vm-select-folder-buffer-and-validate' is a defsubst, so the compiled
     ;; command has it inlined and it cannot be stubbed; it asks for the mode
     (setq major-mode 'vm-mode)
     (setq vm-folder-access-method 'pop
           vm-folder-access-data (make-vector vm-folder-pop-access-data-length
                                              nil))
     (aset vm-folder-access-data 0 "pop:localhost:110:pass:reader:secret")
     (let ((n 0))
       (dolist (message vm-message-list)
         (vm-set-pop-uidl-of message (format "uidl-%d" (setq n (1+ n))))))
     ,@body))

(ert-deftest vm-delete-test-a-pop-expunge-queues-the-uidl ()
  "The ordinary case: what the reader expunged is queued for its maildrop."
  (vm-delete-test--in-a-pop-folder
    (vm-delete-test--expunge-the-first)
    (should (equal vm-pop-messages-to-expunge '("uidl-1")))))

(ert-deftest vm-delete-test-a-message-with-no-uidl-is-not-queued ()
  "A message that never came from a maildrop is not queued for deletion there.
The queue used to collect nil for one, which names no message at all."
  (vm-delete-test--in-a-pop-folder
    (vm-set-pop-uidl-of (vm-test-first-message) nil)
    (vm-delete-test--expunge-the-first)
    (should-not vm-pop-messages-to-expunge)))

(ert-deftest vm-delete-test-what-the-maildrop-has-lost-is-not-queued ()
  "A message expunged because the maildrop no longer has it is not queued.
That is what `vm-pop-synchronize-folder's local expunge is, and DELE on a
message that is not there is a no-op at best."
  (vm-delete-test--in-a-pop-folder
    (vm-delete-test--expunge-the-first :not-on-the-server t)
    (should-not vm-pop-messages-to-expunge)
    ;; and it is still expunged here
    (should (= 1 (vm-test-message-count)))))

(ert-deftest vm-delete-test-a-pop-expunge-records-the-uidl-once ()
  "The UIDL of an expunged message is recorded once, not again.
The entry made when the message arrived names the maildrop without its
password, and this one used to name it with, so nothing matched and the list
grew a duplicate per expunge."
  (vm-delete-test--in-a-pop-folder
    (setq vm-pop-retrieved-messages
          (list (list "uidl-1" "pop:localhost:110:pass:reader:*" 'uidl)))
    (vm-delete-test--expunge-the-first)
    (should (equal (length vm-pop-retrieved-messages) 1))))

(ert-deftest vm-delete-test-a-pop-expunge-records-the-maildrop-without-a-password ()
  "What is recorded names the maildrop the way every check compares it.
`vm-pop-net-unretrieved' asks whether an entry's maildrop is the one it is
looking at, `vm-popdrop-sans-password' of it, so an entry naming the maildrop
with its password is one no check can match."
  (vm-delete-test--in-a-pop-folder
    (setq vm-pop-retrieved-messages nil)
    (vm-delete-test--expunge-the-first)
    (should (equal vm-pop-retrieved-messages
                   (list (list "uidl-1" "pop:localhost:110:pass:reader:*"
                               'uidl))))))

(ert-deftest vm-delete-test-an-imap-expunge-records-the-maildrop-without-a-password ()
  "The same on the IMAP side, which `vm-imap-check-mail' compares that way.
`vm-imap-get-synchronization-data' keys on the UID and its UIDVALIDITY, so a
folder never noticed; the maildrop-as-spool check does."
  (vm-delete-test--in-an-imap-cache
    (setq vm-imap-retrieved-messages nil)
    (vm-delete-test--expunge-the-first)
    (should (equal (nth 2 (car vm-imap-retrieved-messages))
                   "imap:localhost:143:INBOX:login:reader:*"))
    ;; and not the spec the folder was visited with, password and all
    (should-not (equal (nth 2 (car vm-imap-retrieved-messages))
                       (vm-folder-imap-maildrop-spec)))))

(provide 'vm-delete-test)

;;; vm-delete-test.el ends here
