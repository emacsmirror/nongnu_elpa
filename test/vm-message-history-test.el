;;; vm-message-history-test.el --- Tests for vm-message-history.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Tests for the per-folder message history: what `vm-message-history-add'
;; records, and how `vm-message-history-backward' and its forward sibling
;; walk it.

;;; Code:

(require 'vm-test-init)
(require 'vm-message-history)

(defun vm-message-history-test--folder-text (count)
  "Return an mbox folder of COUNT messages, numbered from 1."
  (mapconcat
   (lambda (n)
     (format (concat "From sender@example.com Mon Jan  1 00:00:00 2024\n"
                     "From: sender@example.com\n"
                     "Subject: message %d\n\nbody %d\n\n")
             n n))
   (number-sequence 1 count)
   ""))

(defmacro vm-message-history-test--with-messages (count &rest body)
  "Run BODY in a folder of COUNT messages, with an empty history.

The buffer is put in `vm-mode' because the history commands validate
against it; `vm-message-history' is buffer-local, so each test starts empty
without any tearing down."
  (declare (indent 1) (debug t))
  `(vm-test-with-folder (vm-message-history-test--folder-text ,count)
     (setq major-mode 'vm-mode)
     (setq vm-message-history nil
           vm-message-history-pointer nil)
     ,@body))

(defun vm-message-history-test--select (n)
  "Make message N of the current folder current, and record it in the history.
Recording is what `vm-select-message-hook' does for a real selection."
  (setq vm-message-pointer (nthcdr (1- n) vm-message-list))
  (vm-message-history-add))

(defun vm-message-history-test--subjects ()
  "The subjects of the messages in `vm-message-history', newest first."
  (mapcar (lambda (m) (vm-su-subject m)) vm-message-history))

;;; What the history records

(ert-deftest vm-message-history-test-a-selection-goes-to-the-head ()
  "Each selected message is recorded, newest first."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (should (equal (vm-message-history-test--subjects)
                   '("message 2" "message 1")))))

(ert-deftest vm-message-history-test-stepping-through-records-nothing ()
  "Selecting a message by stepping through the history does not record it.
Otherwise walking backwards would keep appending to the very list being
walked, and there would be no way back to where the walk started."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (let ((before (copy-sequence vm-message-history)))
      (dolist (command '(vm-message-history-backward
                         vm-message-history-forward
                         vm-message-history-browse-select
                         vm-goto-message-last-seen))
        (let ((this-command command))
          (setq vm-message-pointer (nthcdr 2 vm-message-list))
          (vm-message-history-add)))
      (should (equal vm-message-history before)))))

(ert-deftest vm-message-history-test-a-message-appears-once ()
  "A message selected again moves to the head instead of repeating.
A history with the same message twice would step to the same place twice."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (vm-message-history-test--select 1)
    (should (equal (vm-message-history-test--subjects)
                   '("message 1" "message 2")))))

(ert-deftest vm-message-history-test-the-history-is-capped ()
  "The history holds `vm-message-history-max' messages, and drops the oldest."
  (let ((vm-message-history-max 3))
    (vm-message-history-test--with-messages 5
      (dolist (n '(1 2 3 4 5))
        (vm-message-history-test--select n))
      (should (equal (vm-message-history-test--subjects)
                     '("message 5" "message 4" "message 3"))))))

(ert-deftest vm-message-history-test-selecting-discards-what-was-ahead ()
  "Selecting a message after stepping back discards the newer entries.
This is what makes the history a path rather than a set: after going back
two and choosing something else, forward from there is the new choice."
  (vm-message-history-test--with-messages 4
    (dolist (n '(1 2 3))
      (vm-message-history-test--select n))
    ;; step back to message 1, as `vm-message-history-backward' would
    (setq vm-message-history-pointer (nthcdr 2 vm-message-history))
    (setq vm-message-pointer (nthcdr 3 vm-message-list))
    (vm-message-history-add)
    (should (equal (vm-message-history-test--subjects)
                   '("message 4" "message 1")))))

;;; Walking it

(defmacro vm-message-history-test--walking (&rest body)
  "Run BODY with the display side of the history commands stubbed out.
What is under test is which message the walk selects; presenting it and
popping up the browse buffer are not."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'vm-present-current-message) #'ignore)
             ((symbol-function 'vm-message-history-browse) #'ignore)
             ((symbol-function 'vm-record-and-change-message-pointer)
              (lambda (_old new &rest _) (setq vm-message-pointer new))))
     ,@body))

(ert-deftest vm-message-history-test-backward-walks-to-the-older-message ()
  "`vm-message-history-backward' selects the message before the current one."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (vm-message-history-test--walking
      (vm-message-history-backward))
    (should (equal (vm-su-subject (car vm-message-pointer)) "message 1"))))

(ert-deftest vm-message-history-test-backward-wraps-to-the-newest ()
  "Stepping back past the oldest message wraps to the newest, rather than
signalling or stopping."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (vm-message-history-test--walking
      (vm-message-history-backward)
      (vm-message-history-backward))
    (should (equal (vm-su-subject (car vm-message-pointer)) "message 2"))))

(ert-deftest vm-message-history-test-forward-takes-no-argument ()
  "REGRESSION: `vm-message-history-forward' works when its optional argument
is omitted.  It passed (- arg) to `vm-message-history-backward', so a call
from Lisp signalled `wrong-type-argument' on nil before anything else
happened; `vm-message-history-backward' has always defaulted its own."
  (vm-message-history-test--with-messages 3
    (vm-message-history-test--select 1)
    (vm-message-history-test--select 2)
    (vm-message-history-test--walking
      (vm-message-history-backward)
      (vm-message-history-forward))
    (should (equal (vm-su-subject (car vm-message-pointer)) "message 2"))))

(ert-deftest vm-message-history-test-walking-an-empty-history-is-refused ()
  "With nothing in the history there is nowhere to go, and the commands say
so rather than selecting something arbitrary."
  (vm-message-history-test--with-messages 3
    (let ((text-quoting-style 'grave))
      (dolist (command '(vm-message-history-backward
                         vm-message-history-browse))
        (let ((err (should-error (funcall command) :type 'error)))
          (should (string-match-p "No message history"
                                  (error-message-string err))))))))


;;; The mode, and loading not switching it on (emacs-vm/vm#788)

(ert-deftest vm-message-history-test-loading-does-not-switch-it-on ()
  "Loading vm-message-history does not bind a key or add a hook.
Loading a file should not change how Emacs behaves, and Customize loads this
one whenever it is asked about a VM option: `C-h v' on any VM variable did it,
so a reader who had never asked for a message history got three keys taken out
of `vm-mode-map' and a function on `vm-select-message-hook'.  The mode does it
instead."
  (require 'vm-message-history)
  (let ((vm-select-message-hook nil)
        (vm-message-history-mode nil))
    ;; Loading has already happened; nothing is installed.
    (should-not (memq 'vm-message-history-add vm-select-message-hook))
    (dolist (binding vm-message-history-key-bindings)
      (should-not (eq (lookup-key vm-mode-map (car binding)) (cdr binding))))))

(ert-deftest vm-message-history-test-mode-toggles-everything-it-installs ()
  "The mode binds the keys, adds the menu entries and the hook, and undoes all three."
  (require 'vm-message-history)
  (let ((vm-select-message-hook nil)
        (vm-menu-motion-menu nil)
        (vm-message-history-mode nil))
    (vm-message-history-mode 1)
    (should (memq 'vm-message-history-add vm-select-message-hook))
    (dolist (binding vm-message-history-key-bindings)
      (should (eq (lookup-key vm-mode-map (car binding)) (cdr binding))))
    (should (equal vm-menu-motion-menu vm-message-history-menu-items))
    (vm-message-history-mode -1)
    (should-not (memq 'vm-message-history-add vm-select-message-hook))
    (dolist (binding vm-message-history-key-bindings)
      (should-not (eq (lookup-key vm-mode-map (car binding)) (cdr binding))))
    (should-not vm-menu-motion-menu)))

(ert-deftest vm-message-history-test-mode-is-idempotent ()
  "Switching it on twice leaves one copy of the hook and one of each menu entry.
`add-hook' guarantees the hook; the menu is an `append' and would have listed
the three entries twice, which is why the mode removes them before adding."
  (require 'vm-message-history)
  (let ((vm-select-message-hook nil)
        (vm-menu-motion-menu nil)
        (vm-message-history-mode nil))
    (vm-message-history-mode 1)
    (vm-message-history-mode 1)
    (should (= 1 (seq-count (lambda (f) (eq f 'vm-message-history-add))
                            vm-select-message-hook)))
    (should (equal (length vm-menu-motion-menu)
                   (length vm-message-history-menu-items)))))

(provide 'vm-message-history-test)

;;; vm-message-history-test.el ends here
