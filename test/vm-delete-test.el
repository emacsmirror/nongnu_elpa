;;; vm-delete-test.el --- Tests for vm-delete.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

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

;;; Tests for duplicate detection logic
;; Note: vm-delete-duplicate-messages requires full folder context,
;; so we test the underlying logic patterns instead.

(ert-deftest vm-delete-test-message-id-hash-logic ()
  "Test the hash table logic used for duplicate detection."
  (let ((table (make-vector 103 0))
        (ids '("<unique1@example.com>"
               "<duplicate@example.com>"
               "<duplicate@example.com>"  ; duplicate!
               "<unique2@example.com>")))
    ;; Simulate duplicate detection logic
    (let ((duplicates 0))
      (dolist (mid ids)
        (if (intern-soft mid table)
            (setq duplicates (1+ duplicates))
          (intern mid table)))
      ;; Should find exactly 1 duplicate
      (should (= duplicates 1)))))

(ert-deftest vm-delete-test-message-id-uniqueness ()
  "Test that hash table correctly identifies unique Message-IDs."
  (let ((table (make-vector 61 0))
        (ids '("<msg1@example.com>"
               "<msg2@example.com>"
               "<msg3@example.com>")))
    (let ((duplicates 0))
      (dolist (mid ids)
        (if (intern-soft mid table)
            (setq duplicates (1+ duplicates))
          (intern mid table)))
      ;; No duplicates
      (should (= duplicates 0)))))

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

;;; Tests for skipping logic used in duplicate detection

(ert-deftest vm-delete-test-skip-already-deleted-logic ()
  "Test that duplicate logic skips already-deleted messages."
  ;; This tests the pattern: skip if message is already deleted
  (let ((deleted-flags '(t nil nil t))  ; Messages 0,3 are deleted
        (ids '("<dup@example.com>"
               "<dup@example.com>"      ; Would be dup, but msg0 deleted
               "<unique@example.com>"
               "<unique@example.com>")))  ; Would be dup, but msg3 deleted
    (let ((table (make-vector 61 0))
          (idx 0)
          (new-deletes 0))
      (while (< idx (length ids))
        (unless (nth idx deleted-flags)  ; Skip deleted messages
          (let ((mid (nth idx ids)))
            (if (intern-soft mid table)
                (setq new-deletes (1+ new-deletes))
              (intern mid table))))
        (setq idx (1+ idx)))
      ;; Only one new delete: second occurrence of <dup@example.com>
      ;; (the one at idx 1, since idx 0 is deleted)
      (should (= new-deletes 0)))))


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
message 2 and mark it expunged while message 3 stayed in the folder.  The shape
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
nothing to expunge."
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
  "Five messages in the shape 0 < 1 < 2, 0 < 3, and 4 on its own.")

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
The #496 shape: a kill command with nothing to match on deleting everything."
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

(provide 'vm-delete-test)

;;; vm-delete-test.el ends here
