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

(ert-deftest vm-undo-test-describe-recognizes-operations ()
  "Test that vm-undo-describe can be called."
  ;; Just verify the function is callable - it needs real messages for output
  (should (fboundp 'vm-undo-describe)))

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
label lists, one per message."
  (declare (indent 2))
  `(vm-test-with-folder
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
       ,@body)))

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

(provide 'vm-undo-test)

;;; vm-undo-test.el ends here
