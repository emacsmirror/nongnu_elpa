;;; vm-undo-test.el --- Tests for vm-undo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM undo functions in vm-undo.el

;;; Code:

(require 'vm-test-init)
(require 'vm-undo)

;;; Undo function existence tests

(ert-deftest vm-undo-test-functions-exist ()
  "Test that undo functions exist."
  (should (fboundp 'vm-undo))
  (should (fboundp 'vm-undo-boundary))
  (should (fboundp 'vm-add-undo-boundaries))
  (should (fboundp 'vm-undo-record))
  (should (fboundp 'vm-undo-describe))
  (should (fboundp 'vm-undo-set-message-pointer))
  (should (fboundp 'vm-clear-expunge-invalidated-undos))
  (should (fboundp 'vm-clear-virtual-quit-invalidated-undos))
  (should (fboundp 'vm-clear-modification-flag-undos))
  (should (fboundp 'vm-squeeze-consecutive-undo-boundaries)))

;;; Label functions tests

(ert-deftest vm-undo-test-label-functions-exist ()
  "Test that label manipulation functions exist."
  (should (fboundp 'vm-set-message-attributes))
  (should (fboundp 'vm-add-message-labels))
  (should (fboundp 'vm-add-existing-message-labels))
  (should (fboundp 'vm-delete-message-labels))
  (should (fboundp 'vm-add-or-delete-message-labels))
  (should (fboundp 'vm-set-labels))
  (should (fboundp 'vm-expunge-label)))

;;; vm-expunge-label tests

(require 'vm-misc)

(ert-deftest vm-undo-test-expunge-label-removes-from-messages ()
  "Test that vm-expunge-label removes label from all messages."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test 1

Body 1

From sender@example.com Mon Jan  1 00:00:01 2024
From: sender@example.com
Subject: Test 2

Body 2

From sender@example.com Mon Jan  1 00:00:02 2024
From: sender@example.com
Subject: Test 3

Body 3
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray
    (setq vm-label-obarray (make-vector 29 0))
    ;; Add labels to messages
    (let ((m1 (nth 0 vm-message-list))
          (m2 (nth 1 vm-message-list))
          (m3 (nth 2 vm-message-list)))
      ;; Set up labels directly (bypass vm-set-labels which needs full folder setup)
      (vm-set-decoded-labels-of m1 '("important" "work"))
      (vm-set-decoded-labels-of m2 '("important"))
      (vm-set-decoded-labels-of m3 '("personal"))
      ;; Add labels to obarray
      (intern "important" vm-label-obarray)
      (intern "work" vm-label-obarray)
      (intern "personal" vm-label-obarray)
      ;; Mock yes-or-no-p to return t and vm-set-labels to just update labels
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'vm-set-labels)
                 (lambda (m labels) (vm-set-decoded-labels-of m labels)))
                ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
                ((symbol-function 'vm-inform) #'ignore))
        (vm-expunge-label "important"))
      ;; Check that "important" was removed from all messages
      (should-not (member "important" (vm-labels-of m1)))
      (should-not (member "important" (vm-labels-of m2)))
      ;; Check that other labels remain
      (should (member "work" (vm-labels-of m1)))
      (should (member "personal" (vm-labels-of m3))))))

(ert-deftest vm-undo-test-expunge-label-removes-from-obarray ()
  "Test that vm-expunge-label removes label from folder obarray."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray
    (setq vm-label-obarray (make-vector 29 0))
    (intern "remove-me" vm-label-obarray)
    (intern "keep-me" vm-label-obarray)
    ;; Set up label on message
    (vm-set-decoded-labels-of (car vm-message-list) '("remove-me"))
    ;; Mock required functions
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'vm-set-labels)
               (lambda (m labels) (vm-set-decoded-labels-of m labels)))
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-inform) #'ignore))
      (vm-expunge-label "remove-me"))
    ;; Check that "remove-me" is gone from obarray
    (should-not (intern-soft "remove-me" vm-label-obarray))
    ;; Check that "keep-me" remains
    (should (intern-soft "keep-me" vm-label-obarray))))

(ert-deftest vm-undo-test-expunge-label-aborts-on-no ()
  "Test that vm-expunge-label aborts when user says no."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray
    (setq vm-label-obarray (make-vector 29 0))
    (intern "keep-this" vm-label-obarray)
    ;; Set up label on message
    (vm-set-decoded-labels-of (car vm-message-list) '("keep-this"))
    ;; Mock yes-or-no-p to return nil
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (vm-expunge-label "keep-this")))
    ;; Label should still be on message
    (should (member "keep-this" (vm-labels-of (car vm-message-list))))
    ;; Label should still be in obarray
    (should (intern-soft "keep-this" vm-label-obarray))))

(ert-deftest vm-undo-test-expunge-label-case-insensitive ()
  "Test that vm-expunge-label is case-insensitive."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray
    (setq vm-label-obarray (make-vector 29 0))
    (intern "mixedcase" vm-label-obarray)
    ;; Set up label on message (stored lowercase)
    (vm-set-decoded-labels-of (car vm-message-list) '("mixedcase"))
    ;; Mock required functions
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'vm-set-labels)
               (lambda (m labels) (vm-set-decoded-labels-of m labels)))
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-inform) #'ignore))
      ;; Call with uppercase - should still work
      (vm-expunge-label "MIXEDCASE"))
    ;; Label should be removed (downcased internally)
    (should-not (member "mixedcase" (vm-labels-of (car vm-message-list))))))

(ert-deftest vm-undo-test-expunge-label-no-matching-messages ()
  "Test vm-expunge-label when no messages have the label."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray
    (setq vm-label-obarray (make-vector 29 0))
    (intern "orphan-label" vm-label-obarray)
    ;; No messages have this label
    (vm-set-decoded-labels-of (car vm-message-list) '("other-label"))
    ;; Mock required functions
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-inform) #'ignore))
      ;; Should still work - removes from obarray even with 0 messages
      (vm-expunge-label "orphan-label"))
    ;; Label should be removed from obarray
    (should-not (intern-soft "orphan-label" vm-label-obarray))))

(ert-deftest vm-undo-test-expunge-label-records-undo ()
  "Test that vm-expunge-label records an undo entry for the obarray."
  (vm-test-with-folder
      "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body
"
    ;; Set up as a valid VM folder buffer
    (setq major-mode 'vm-mode)
    ;; Initialize the label obarray and undo list
    (setq vm-label-obarray (make-vector 29 0))
    (setq vm-undo-record-list nil)
    (intern "undo-test" vm-label-obarray)
    ;; No messages have this label (simpler test)
    ;; Mock required functions
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
              ((symbol-function 'vm-inform) #'ignore))
      (vm-expunge-label "undo-test"))
    ;; Label should be gone from obarray
    (should-not (intern-soft "undo-test" vm-label-obarray))
    ;; Check that an undo record was created for the obarray
    (should (member '(intern "undo-test" vm-label-obarray) vm-undo-record-list))
    ;; Evaluate the intern undo record
    (eval '(intern "undo-test" vm-label-obarray))
    ;; Label should be back in obarray
    (should (intern-soft "undo-test" vm-label-obarray))))

;;; Test for clear-expunge handling of non-message records (bug fix)

(ert-deftest vm-undo-test-clear-expunge-handles-intern-records ()
  "Test that vm-clear-expunge-invalidated-undos handles intern records.
This tests the fix for a bug where intern records from vm-expunge-label
caused wrong-type-argument errors because the function assumed all
non-boundary records had message structs."
  (let ((vm-undo-record-list
         (list nil
               '(intern "some-label" vm-label-obarray)  ; non-message record
               nil)))
    ;; Should not error
    (vm-clear-expunge-invalidated-undos)
    ;; The intern record should still be there (not removed)
    (should (member '(intern "some-label" vm-label-obarray)
                    vm-undo-record-list))))

(ert-deftest vm-undo-test-clear-virtual-quit-handles-intern-records ()
  "Test that vm-clear-virtual-quit-invalidated-undos handles intern records."
  (let ((vm-undo-record-list
         (list nil
               '(intern "some-label" vm-label-obarray)  ; non-message record
               nil)))
    ;; Should not error
    (vm-clear-virtual-quit-invalidated-undos)
    ;; The intern record should still be there (not removed)
    (should (member '(intern "some-label" vm-label-obarray)
                    vm-undo-record-list))))

(ert-deftest vm-undo-test-set-message-pointer-handles-intern-records ()
  "Test that vm-undo-set-message-pointer handles intern records."
  (let ((vm-message-pointer nil))
    ;; Should not error when called with an intern record
    (vm-undo-set-message-pointer '(intern "some-label" vm-label-obarray))))

;;; Flag setting functions tests

(ert-deftest vm-undo-test-flag-functions-exist ()
  "Test that flag setting functions exist."
  (should (fboundp 'vm-set-xxxx-flag))
  (should (fboundp 'vm-set-xxxx-cached-data-flag)))

;;; vm-undo-describe operation recognition
;; Note: vm-undo-describe requires real message structures for full testing.
;; Here we test that the function recognizes different record types.

;;; vm-squeeze-consecutive-undo-boundaries tests

(ert-deftest vm-undo-test-squeeze-removes-consecutive-nils ()
  "Test that vm-squeeze-consecutive-undo-boundaries removes consecutive nils."
  (let ((vm-undo-record-list '(nil nil record1 nil nil nil record2 nil)))
    (vm-squeeze-consecutive-undo-boundaries)
    ;; Should collapse consecutive nils to single nils
    (should-not (and (null (car vm-undo-record-list))
                     (null (cadr vm-undo-record-list))))))

(ert-deftest vm-undo-test-squeeze-empty-list ()
  "Test vm-squeeze-consecutive-undo-boundaries with nil list."
  (let ((vm-undo-record-list nil))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (null vm-undo-record-list))))

(ert-deftest vm-undo-test-squeeze-only-nils-becomes-nil ()
  "Test that list of only nils becomes nil."
  (let ((vm-undo-record-list '(nil)))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (null vm-undo-record-list))))

(ert-deftest vm-undo-test-squeeze-preserves-records ()
  "Test that vm-squeeze preserves actual records."
  (let ((vm-undo-record-list '(record1 nil record2 nil record3)))
    (vm-squeeze-consecutive-undo-boundaries)
    (should (memq 'record1 vm-undo-record-list))
    (should (memq 'record2 vm-undo-record-list))
    (should (memq 'record3 vm-undo-record-list))))

;;; vm-undo-record tests

(ert-deftest vm-undo-test-record-adds-to-list ()
  "Test that vm-undo-record adds sexp to list."
  (let ((vm-undo-record-list nil))
    (vm-undo-record '(vm-set-new-flag msg t))
    (should (equal (car vm-undo-record-list) '(vm-set-new-flag msg t)))))

(ert-deftest vm-undo-test-record-prepends ()
  "Test that vm-undo-record prepends to existing list."
  (let ((vm-undo-record-list '(existing-record)))
    (vm-undo-record 'new-record)
    (should (eq (car vm-undo-record-list) 'new-record))
    (should (eq (cadr vm-undo-record-list) 'existing-record))))

;;; vm-undo-boundary tests

(ert-deftest vm-undo-test-boundary-adds-nil ()
  "Test that vm-undo-boundary adds nil separator."
  (let ((vm-undo-record-list '(record1)))
    (vm-undo-boundary)
    (should (null (car vm-undo-record-list)))
    (should (eq (cadr vm-undo-record-list) 'record1))))

(ert-deftest vm-undo-test-boundary-no-double-nil ()
  "Test that vm-undo-boundary doesn't add nil if list starts with nil."
  (let ((vm-undo-record-list '(nil record1)))
    (vm-undo-boundary)
    ;; Should not add another nil
    (should (equal vm-undo-record-list '(nil record1)))))

;;; unused label tests

(defmacro vm-undo-test-with-labels (obarray-labels message-labels &rest body)
  "Run BODY in a three-message folder with labels set up.
OBARRAY-LABELS is the folder's label list; MESSAGE-LABELS is a list of
label lists, one per message.

`vm-current-warning' and `vm-user-interaction-buffer' are bound: declining an
expunge warns, and asking the question records the buffer it was asked in."
  (declare (indent 2))
  `(let ((vm-current-warning vm-current-warning)
         (vm-user-interaction-buffer vm-user-interaction-buffer))
     (vm-test-with-folder
         "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test 1

Body 1

From sender@example.com Mon Jan  1 00:00:01 2024
From: sender@example.com
Subject: Test 2

Body 2

From sender@example.com Mon Jan  1 00:00:02 2024
From: sender@example.com
Subject: Test 3

Body 3
"
     (setq major-mode 'vm-mode)
     (setq vm-label-obarray (make-vector 29 0))
     (dolist (label ,obarray-labels)
       (intern label vm-label-obarray))
     (let ((i 0))
       (dolist (labels ,message-labels)
         (vm-set-decoded-labels-of (nth i vm-message-list) labels)
         (setq i (1+ i))))
     (cl-letf (((symbol-function 'vm-follow-summary-cursor) #'ignore)
               ((symbol-function 'vm-select-folder-buffer-and-validate)
                (lambda (&rest _) nil))
               ((symbol-function 'vm-error-if-folder-read-only) #'ignore)
               ((symbol-function 'vm-update-summary-and-mode-line) #'ignore)
               ((symbol-function 'vm-mark-folder-modified-p) #'ignore)
               ((symbol-function 'vm-inform) #'ignore))
         ,@body))))

(ert-deftest vm-undo-test-unused-labels-finds-them ()
  "Test that `vm-unused-labels' reports labels no message carries."
  (vm-undo-test-with-labels
      '("important" "work" "stale" "gone")
      '(("important" "work") ("important") nil)
    (should (equal (vm-unused-labels) '("gone" "stale")))))

(ert-deftest vm-undo-test-unused-labels-none ()
  "Test that `vm-unused-labels' returns nil when every label is in use."
  (vm-undo-test-with-labels
      '("important" "work")
      '(("important") ("work") nil)
    (should (null (vm-unused-labels)))))

(ert-deftest vm-undo-test-expunge-unused-labels ()
  "Test that `vm-expunge-unused-labels' removes exactly the unused ones.
Regression test for issue #269: deleting a label from the last message
holding it left it in the folder, so it kept appearing in completions."
  (vm-undo-test-with-labels
      '("important" "work" "stale" "gone")
      '(("important" "work") ("important") nil)
    (vm-expunge-unused-labels)
    (should (null (vm-unused-labels)))
    (should (equal (sort (vm-obarray-to-string-list vm-label-obarray)
                         #'string-lessp)
                   '("important" "work")))))

(ert-deftest vm-undo-test-expunge-unused-labels-leaves-messages-alone ()
  "Test that `vm-expunge-unused-labels' changes no message."
  (vm-undo-test-with-labels
      '("important" "stale")
      '(("important") ("important") nil)
    (vm-expunge-unused-labels)
    (should (equal (vm-labels-of (nth 0 vm-message-list)) '("important")))
    (should (equal (vm-labels-of (nth 1 vm-message-list)) '("important")))
    (should (null (vm-labels-of (nth 2 vm-message-list))))))

(ert-deftest vm-undo-test-expunge-unused-labels-declined ()
  "Test that declining the confirmation removes nothing."
  (vm-undo-test-with-labels
      '("important" "stale")
      '(("important") nil nil)
    ;; vm-interactive-p is a macro over called-interactively-p, so that
    ;; is what has to be stubbed to make the command think it is
    ;; interactive and reach the confirmation
    (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (vm-expunge-unused-labels) :type 'error))
    (should (equal (vm-unused-labels) '("stale")))))

(ert-deftest vm-undo-test-list-unused-labels ()
  "Test that `vm-list-unused-labels' reports them without changing anything."
  (vm-undo-test-with-labels
      '("important" "stale" "gone")
      '(("important") nil nil)
    (vm-list-unused-labels)
    (let ((buf (get-buffer "*VM unused labels*")))
      (should buf)
      (with-current-buffer buf
        (let ((text (buffer-string)))
          (should (string-match "2 unused labels" text))
          (should (string-match "^  gone$" text))
          (should (string-match "^  stale$" text))
          (should-not (string-match "important" text))))
      (kill-buffer buf))
    ;; nothing removed
    (should (equal (vm-unused-labels) '("gone" "stale")))))

(ert-deftest vm-undo-test-missing-labels-finds-them ()
  "Test that `vm-missing-labels' reports labels the folder does not list.
A message saved in from another folder brings its labels with it, but
nothing interns them, so they never reach completion."
  (vm-undo-test-with-labels
      '("important")
      '(("important") ("arrived-with-message") ("another" "important"))
    (should (equal (vm-missing-labels) '("another" "arrived-with-message")))))

(ert-deftest vm-undo-test-missing-labels-none ()
  "Test that `vm-missing-labels' returns nil when the folder lists them all."
  (vm-undo-test-with-labels
      '("important" "work" "spare")
      '(("important") ("work") nil)
    (should (null (vm-missing-labels)))))

(ert-deftest vm-undo-test-labels-compare-case-insensitively ()
  "Test that label case does not make one label look like two.
Labels are lowercase by convention -- `vm-expunge-label' and
`vm-add-or-delete-message-labels' both downcase -- but a message can
arrive carrying \"Work\" while the folder lists \"work\".  Comparing
verbatim reported the one label as unused and missing at once."
  (vm-undo-test-with-labels
      '("work")
      '(("Work") nil nil)
    (should (null (vm-unused-labels)))
    (should (null (vm-missing-labels)))))

(ert-deftest vm-undo-test-sync-labels-leaves-case-alone ()
  "Test that syncing does not rewrite the folder's canonical spelling."
  (vm-undo-test-with-labels
      '("work")
      '(("Work") nil nil)
    (vm-sync-labels)
    (should (equal (vm-obarray-to-string-list vm-label-obarray) '("work")))))

(ert-deftest vm-undo-test-sync-labels ()
  "Test that `vm-sync-labels' fixes the list in both directions."
  (vm-undo-test-with-labels
      '("important" "stale")
      '(("important") ("newcomer") nil)
    (vm-sync-labels)
    (should (null (vm-unused-labels)))
    (should (null (vm-missing-labels)))
    (should (equal (sort (vm-obarray-to-string-list vm-label-obarray)
                         #'string-lessp)
                   '("important" "newcomer")))))

(ert-deftest vm-undo-test-sync-labels-leaves-messages-alone ()
  "Test that `vm-sync-labels' changes no message."
  (vm-undo-test-with-labels
      '("stale")
      '(("newcomer") ("newcomer") nil)
    (vm-sync-labels)
    (should (equal (vm-labels-of (nth 0 vm-message-list)) '("newcomer")))
    (should (equal (vm-labels-of (nth 1 vm-message-list)) '("newcomer")))
    (should (null (vm-labels-of (nth 2 vm-message-list))))))

(ert-deftest vm-undo-test-sync-labels-records-undo ()
  "Test that both directions of the sync are undoable."
  (vm-undo-test-with-labels
      '("stale")
      '(("newcomer") nil nil)
    (let ((vm-undo-record-list nil)
          (vm-undo-record-pointer nil))
      (vm-sync-labels)
      ;; an added label is undone by uninterning it, a removed one by
      ;; re-interning it
      (should (member '(unintern "newcomer" vm-label-obarray)
                      vm-undo-record-list))
      (should (member '(intern "stale" vm-label-obarray)
                      vm-undo-record-list))
      ;; and the recorded forms actually reverse the change when evalled
      (dolist (record vm-undo-record-list) (eval record t))
      (should (equal (sort (vm-obarray-to-string-list vm-label-obarray)
                           #'string-lessp)
                     '("stale"))))))

(ert-deftest vm-undo-test-sync-labels-nothing-to-do ()
  "Test that a folder already in agreement is left alone."
  (vm-undo-test-with-labels
      '("important")
      '(("important") nil nil)
    (vm-sync-labels)
    (should (equal (vm-obarray-to-string-list vm-label-obarray)
                   '("important")))))

(ert-deftest vm-undo-test-expunge-unused-labels-records-undo ()
  "Test that the removal can be undone."
  (vm-undo-test-with-labels
      '("important" "stale")
      '(("important") nil nil)
    (let ((vm-undo-record-list nil)
          (vm-undo-record-pointer nil))
      (vm-expunge-unused-labels)
      (should (member '(intern "stale" vm-label-obarray)
                      vm-undo-record-list)))))

;;; Undo across an expunge

;; An undo record holds the message it would change, so expunging a message
;; leaves records that would set flags on something no longer in the folder.
;; `vm-clear-expunge-invalidated-undos' drops them, recognising an expunged
;; message by its deleted flag being `expunged' rather than t.  It had no test
;; beyond one that it survives a record with no message in it.

(defconst vm-undo-test--two-messages
  "From alice@example.com Mon Jan  1 00:00:00 2024
From: alice@example.com
Subject: subject 0
Message-ID: <undo-0@example.com>

Body 0.

From alice@example.com Mon Jan  1 00:00:01 2024
From: alice@example.com
Subject: subject 1
Message-ID: <undo-1@example.com>

Body 1.
"
  "Two messages, enough to have one expunged and one not.")

(defun vm-undo-test--record-messages ()
  "Return the message of each undo record, nil for a boundary."
  (mapcar (lambda (r) (and r (nth 1 r))) vm-undo-record-list))

(ert-deftest vm-undo-test-clear-expunge-drops-the-expunged-records ()
  "Records naming an expunged message go; the others and the boundaries stay.
The expunged record is the first here, which is the branch that has to move the
head of the list rather than splice."
  (vm-test-with-folder vm-undo-test--two-messages
    (let ((live (vm-test-first-message))
          (gone (vm-test-nth-message 1)))
      (vm-set-deleted-flag-of gone 'expunged)
      (setq vm-undo-record-list
            (list (list 'vm-set-deleted-flag gone nil)
                  nil
                  (list 'vm-set-replied-flag live nil)))
      (vm-clear-expunge-invalidated-undos)
      (should (equal (list nil live) (vm-undo-test--record-messages))))))

(ert-deftest vm-undo-test-clear-expunge-drops-a-record-from-the-middle ()
  "The same when the record to drop is not the first: the list is spliced.
Two records for the expunged message, one either side of a live one, so both
branches run in one list."
  (vm-test-with-folder vm-undo-test--two-messages
    (let ((live (vm-test-first-message))
          (gone (vm-test-nth-message 1)))
      (vm-set-deleted-flag-of gone 'expunged)
      (setq vm-undo-record-list
            (list (list 'vm-set-replied-flag live nil)
                  (list 'vm-set-deleted-flag gone nil)
                  nil
                  (list 'vm-set-new-flag gone nil)
                  (list 'vm-set-flagged-flag live nil)))
      (vm-clear-expunge-invalidated-undos)
      (should (equal (list live nil live) (vm-undo-test--record-messages))))))

(ert-deftest vm-undo-test-clear-expunge-keeps-records-for-deleted-messages ()
  "A message merely flagged deleted keeps its undo records.
`expunged' is a distinct value of the same flag, and undeleting is exactly what
undo is for, so a deleted message's records must survive."
  (vm-test-with-folder vm-undo-test--two-messages
    (let ((m (vm-test-first-message)))
      (vm-set-deleted-flag-of m t)
      (setq vm-undo-record-list (list (list 'vm-set-deleted-flag m nil)))
      (vm-clear-expunge-invalidated-undos)
      (should (equal (list m) (vm-undo-test--record-messages))))))

(ert-deftest vm-undo-test-undo-after-an-expunge-changes-the-right-message ()
  "An expunge drops the undo record it invalidated and leaves the rest usable.
The whole sequence in a visited folder: flag one message replied, delete
another, expunge, undo.  The undo has to reach the replied flag on the message
that is still there, and the expunged message's own record has to be gone so
that nothing tries to undelete it."
  (vm-test-with-real-folder (4)
    (let ((replied (nth 2 vm-message-list))
          (doomed (nth 1 vm-message-list)))
      (vm-undo-boundary)
      (vm-set-replied-flag replied t)
      (vm-undo-boundary)
      (vm-set-deleted-flag doomed t)
      (should (= 4 (length vm-message-list)))
      (vm-expunge-folder)
      ;; The message is gone and so is the record that would have undeleted it.
      (should (= 3 (length vm-message-list)))
      (should-not (memq doomed vm-message-list))
      (should-not (memq doomed (vm-undo-test--record-messages)))
      (should (vm-replied-flag replied))
      ;; And the undo lands on the surviving message.
      (vm-undo)
      (should-not (vm-replied-flag replied))
      (should (= 3 (length vm-message-list)))
      (should (equal '("subject 0" "subject 2" "subject 3")
                     (mapcar #'vm-su-subject vm-message-list))))))

(ert-deftest vm-undo-test-nothing-left-to-undo-after-an-expunge ()
  "With only the expunged message's record recorded, there is nothing to undo.
`vm-undo' signals rather than reporting, which is worth pinning because it is
the visible consequence of the record having been dropped: an undo that reached
the record would undelete a message the folder no longer has."
  (vm-test-with-real-folder (3)
    (let ((doomed (nth 1 vm-message-list))
          (before nil))
      (vm-undo-boundary)
      (vm-set-deleted-flag doomed t)
      (vm-expunge-folder)
      (setq before (mapcar #'vm-su-subject vm-message-list))
      (should-error (vm-undo) :type 'error)
      (should (equal before (mapcar #'vm-su-subject vm-message-list)))
      (should (equal '(nil nil) (mapcar #'vm-deleted-flag vm-message-list))))))

;;; Setting attributes by name

;; The name-to-flag mapping was inlined in `vm-set-message-attributes' until
;; `vm-virtual-filter-alist' needed it per-message too, so it is now
;; `vm-set-message-attribute'.  These pin the mapping across that move: the
;; command still takes a space separated list over a run of messages, and the
;; extracted function is what does the work.

(ert-deftest vm-undo-test-set-message-attributes-takes-a-list ()
  "The command sets every named attribute on every message it covers."
  (vm-test-with-real-folder (3)
    (setq vm-message-pointer vm-message-list)
    (vm-set-message-attributes "read flagged replied" 2)
    (dolist (m (list (nth 0 vm-message-list) (nth 1 vm-message-list)))
      (should (null (vm-new-flag m)))
      (should (null (vm-unread-flag m)))
      (should (vm-flagged-flag m))
      (should (vm-replied-flag m)))
    ;; the third is past the count
    (should (vm-new-flag (nth 2 vm-message-list)))
    (should (null (vm-flagged-flag (nth 2 vm-message-list))))))

(ert-deftest vm-undo-test-set-message-attribute-negations ()
  "The un- names clear the flag their positive counterpart sets."
  (vm-test-with-real-folder (1)
    (let ((m (car vm-message-list)))
      (dolist (name '("deleted" "replied" "forwarded" "redistributed"
                      "filed" "written" "flagged"))
        (vm-set-message-attribute m name))
      (should (vm-deleted-flag m))
      (should (vm-filed-flag m))
      (dolist (name '("undeleted" "unreplied" "unforwarded" "unredistributed"
                      "unfiled" "unwritten" "unflagged"))
        (vm-set-message-attribute m name))
      ;; the flag setters queue the message globally; the callers of
      ;; `vm-set-message-attribute' flush that queue, so do the same here
      (vm-update-summary-and-mode-line)
      (should (null (vm-deleted-flag m)))
      (should (null (vm-replied-flag m)))
      (should (null (vm-forwarded-flag m)))
      (should (null (vm-redistributed-flag m)))
      (should (null (vm-filed-flag m)))
      (should (null (vm-written-flag m)))
      (should (null (vm-flagged-flag m))))))

(ert-deftest vm-undo-test-set-message-attribute-unknown-name-warns ()
  "An unrecognised name warns and leaves the message alone.
It does not signal: `vm-set-message-attributes' reads a space separated list
from the user, and one typo should not abandon the rest of it."
  (vm-test-with-real-folder (1)
    (let ((m (car vm-message-list))
          (warned nil))
      (cl-letf (((symbol-function 'vm-warn)
                 (lambda (&rest args) (setq warned args))))
        (vm-set-message-attribute m "no-such-attribute"))
      (vm-update-summary-and-mode-line)
      (should warned)
      (should (vm-new-flag m))
      (should (null (vm-deleted-flag m))))))

;;; What a boundary is and when there is one, in place of a test that the
;;; functions were bound.

(ert-deftest vm-undo-test-boundary-is-not-added-to-nothing ()
  "A boundary marks the end of a group of records, so an empty list gets none
and two in a row are not made."
  (with-temp-buffer
    (let ((vm-undo-record-list nil))
      (vm-undo-boundary)
      (should-not vm-undo-record-list)
      (vm-undo-record '(vm-set-deleted-flag a-message nil))
      (vm-undo-boundary)
      (should (= (length vm-undo-record-list) 2))
      (should-not (car vm-undo-record-list))
      (vm-undo-boundary)
      (should (= (length vm-undo-record-list) 2)))))

(ert-deftest vm-undo-test-squeeze-removes-the-boundaries-with-nothing-between ()
  "Records removed by an expunge can leave two boundaries together, which
would make one undo command do nothing.  Squeezing them is what stops that."
  (with-temp-buffer
    (let ((vm-undo-record-list '(nil nil (a) nil nil nil (b) nil)))
      (vm-squeeze-consecutive-undo-boundaries)
      (should (equal vm-undo-record-list '(nil (a) nil (b) nil))))
    ;; a list of nothing but a boundary is an empty list
    (let ((vm-undo-record-list '(nil)))
      (vm-squeeze-consecutive-undo-boundaries)
      (should-not vm-undo-record-list))
    ;; and one with records is left as it is
    (let ((vm-undo-record-list '((a) nil (b))))
      (vm-squeeze-consecutive-undo-boundaries)
      (should (equal vm-undo-record-list '((a) nil (b)))))))

;;; What an undo says it is undoing

(ert-deftest vm-undo-test-describe-names-the-flag-and-its-two-states ()
  "Undoing a flag change says which flag, and which way the undo goes.
A record is (FUNCTION MESSAGE VALUE) where VALUE is what undoing will set --
`vm-set-xxxx-flag' records `(not flag)\\=' -- so a record carrying t reads
undeleted -> deleted: the state it is in now, and the state it goes to."
  (vm-test-with-folder
      (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
              "From: alice@example.com\nSubject: one\n\nBody.\n\n")
    (let ((m (car vm-message-list))
          said)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level fmt &rest args)
                   (setq said (apply #'format fmt args)))))
        (vm-undo-describe (list 'vm-set-deleted-flag m t))
        (should (string-match-p "undeleted -> deleted" said))
        (vm-undo-describe (list 'vm-set-deleted-flag m nil))
        (should (string-match-p "deleted -> undeleted" said))
        (vm-undo-describe (list 'vm-set-replied-flag m t))
        (should (string-match-p "unanswered -> answered" said))
        ;; and it names the folder the message is in
        (should (string-match-p (regexp-quote (buffer-name)) said))))))

(ert-deftest vm-undo-test-describe-names-the-labels ()
  "Undoing a label change says what the labels go back to.
It never said anything: the clause tested `(car cell)', which is what the
alist of flag names gave -- and that alist has no `vm-set-labels' in it, so
`cell' was nil and the test could not be true."
  (vm-test-with-folder
      (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
              "From: alice@example.com\nSubject: one\n\nBody.\n\n")
    (let ((m (car vm-message-list))
          said)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (_level fmt &rest args)
                   (setq said (apply #'format fmt args)))))
        (vm-undo-describe (list 'vm-set-labels m '("work" "urgent")))
        (should (string-match-p "labels set to work, urgent" said))
        (setq said nil)
        (vm-undo-describe (list 'vm-set-labels m nil))
        (should (string-match-p "lost all its labels" said))))))

(ert-deftest vm-undo-test-describe-says-nothing-about-what-it-does-not-know ()
  "A record of a kind the message does not cover is not announced.
`vm-set-buffer-modified-p' records are in the list too, and there is nothing
to tell the user about them."
  (vm-test-with-folder
      (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
              "From: alice@example.com\nSubject: one\n\nBody.\n\n")
    (let (said)
      (cl-letf (((symbol-function 'vm-inform)
                 (lambda (&rest args) (setq said args))))
        (vm-undo-describe (list 'vm-set-buffer-modified-p nil))
        (should-not said)))))

;;; Adding and deleting labels

(defmacro vm-undo-test--labelling (&rest body)
  "Run BODY in a folder of two messages with an empty label obarray.
`one' and `two' are the messages, and `known' is what the obarray holds."
  (declare (indent 0) (debug t))
  `(vm-test-with-folder
       (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
               "From: alice@example.com\nSubject: one\n\nBody.\n\n"
               "From alice@example.com Sat Aug  8 14:25:13 2026\n"
               "From: alice@example.com\nSubject: two\n\nBody.\n\n")
     (setq major-mode 'vm-mode)
     (setq vm-label-obarray (make-vector 29 0))
     (let ((one (car vm-message-list))
           (two (nth 1 vm-message-list)))
       (cl-flet ((known ()
                   (let (names)
                     (mapatoms (lambda (s) (push (symbol-name s) names))
                               vm-label-obarray)
                     (sort names #'string<))))
         ,@body))))

(ert-deftest vm-undo-test-adding-a-label-keeps-the-labels-there-already ()
  "A label is added to the labels the message has, not put in place of
them, and it is added to the folder's list of labels so that completion
offers it."
  (vm-undo-test--labelling
    (vm-set-labels one '("work"))
    (should-not (vm-add-or-delete-message-labels "Urgent" (list one) 'all))
    (should (equal (sort (copy-sequence (vm-decoded-labels-of one)) #'string<)
                   '("urgent" "work")))
    ;; a label is lower case whatever you typed
    (should (equal (known) '("urgent")))
    (should (vm-attribute-modflag-of one))))

(ert-deftest vm-undo-test-adding-a-label-twice-adds-it-once ()
  "Adding a label the message has already leaves one of it: the labels are
a set, and a repeat would show twice in the summary."
  (vm-undo-test--labelling
    (vm-set-labels one '("work"))
    (vm-add-or-delete-message-labels "work" (list one) 'all)
    (should (equal (vm-decoded-labels-of one) '("work")))))

(ert-deftest vm-undo-test-an-existing-only-label-must-be-known-already ()
  "`vm-add-existing-message-labels' adds only labels the folder already
uses, and returns the others rather than inventing them -- which is what
makes a typo visible instead of making a new label."
  (vm-undo-test--labelling
    (vm-add-or-delete-message-labels "work" (list one) 'all)
    (should (equal (vm-add-or-delete-message-labels "work bogus" (list two)
                                                   'existing-only)
                   '("bogus")))
    (should (equal (vm-decoded-labels-of two) '("work")))
    (should (equal (known) '("work")))))

(ert-deftest vm-undo-test-deleting-a-label-leaves-the-others-alone ()
  "Deleting a label takes that label off the message and touches nothing
else -- not the other labels, and not the folder's list of labels, which
`vm-expunge-label' is for."
  (vm-undo-test--labelling
    (vm-set-labels one '("work" "work" "urgent"))
    (vm-add-or-delete-message-labels "urgent" (list one) nil)
    (should (equal (vm-decoded-labels-of one) '("work" "work")))
    (should-not (known))))

(ert-deftest vm-undo-test-a-label-of-nothing-changes-no-message ()
  "A string with no label in it is not a label to add: the messages are
left as they are rather than being given an empty one."
  (vm-undo-test--labelling
    (vm-set-labels one '("work"))
    (vm-set-attribute-modflag-of one nil)
    (vm-add-or-delete-message-labels "   " (list one two) 'all)
    (should (equal (vm-decoded-labels-of one) '("work")))
    (should-not (vm-decoded-labels-of two))
    (should-not (vm-attribute-modflag-of one))))

(provide 'vm-undo-test)

;;; vm-undo-test.el ends here
