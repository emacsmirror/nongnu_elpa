;;; vm-ps-print-test.el --- Tests for vm-ps-print.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM's PostScript printing in vm-ps-print.el.

;;; Code:

(require 'vm-test-init)
(require 'vm-ps-print)
(require 'vm-summary)

;;; vm-ps-print-tokenized-summary

(ert-deftest vm-ps-print-test-tokenized-summary-passes-a-string-through ()
  "A summary that is already a string is its own answer."
  (should (equal (vm-ps-print-tokenized-summary nil "done") "done")))

(ert-deftest vm-ps-print-test-tokenized-summary-indents-a-thread ()
  "The thread-indent token becomes that many spaces.
It was built with `concat' over a character and a count, which are not
sequences, so the token signalled `wrong-type-argument sequencep 32'
instead of indenting anything."
  (cl-letf (((symbol-function 'vm-thread-indentation) (lambda (_m) 3)))
    (let ((vm-summary-show-threads t)
          (vm-summary-thread-indent-level 2)
          (vm-summary-maximum-thread-indentation 20)
          (vm-display-using-mime nil))
      (should (equal (vm-ps-print-tokenized-summary nil '("a" thread-indent "b"))
                     "a      b")))))

(ert-deftest vm-ps-print-test-tokenized-summary-caps-the-indentation ()
  "`vm-summary-maximum-thread-indentation' bounds the indent, as in the summary."
  (cl-letf (((symbol-function 'vm-thread-indentation) (lambda (_m) 40)))
    (let ((vm-summary-show-threads t)
          (vm-summary-thread-indent-level 1)
          (vm-summary-maximum-thread-indentation 5)
          (vm-display-using-mime nil))
      (should (equal (vm-ps-print-tokenized-summary nil '(thread-indent)) "     ")))))

(ert-deftest vm-ps-print-test-tokenized-summary-does-not-indent-unthreaded ()
  "With threads off the token contributes nothing."
  (cl-letf (((symbol-function 'vm-thread-indentation) (lambda (_m) 3)))
    (let ((vm-summary-show-threads nil)
          (vm-summary-thread-indent-level 2)
          (vm-display-using-mime nil))
      (should (equal (vm-ps-print-tokenized-summary nil '("a" thread-indent "b"))
                     "ab")))))

(ert-deftest vm-ps-print-test-tokenized-summary-agrees-with-the-summary-buffer ()
  "The printed summary indents a thread as the summary buffer does.
It is the same renderer now: `vm-ps-print-tokenized-summary' runs
`vm-tokenized-summary-insert' in a buffer of its own and answers with what it
wrote (emacs-vm/vm#862).  This holds the two together, which is what it was
written for when they were separate."
  (cl-letf (((symbol-function 'vm-thread-indentation) (lambda (_m) 4)))
    (let ((vm-summary-show-threads t)
          (vm-summary-thread-indent-level 2)
          (vm-summary-maximum-thread-indentation 20)
          (vm-display-using-mime nil)
          (tokens '("[" thread-indent "]")))
      (should (equal (vm-ps-print-tokenized-summary nil tokens)
                     (with-temp-buffer
                       (vm-tokenized-summary-insert nil tokens)
                       (buffer-string)))))))

(ert-deftest vm-ps-print-test-a-printed-group-is-padded-and-cut ()
  "REGRESSION: a group carries its width and its maximum onto paper.

`vm-ps-print-tokenized-summary' was a copy of the buffer renderer with no
`group-begin\' or `group-end\' branch, and unknown tokens fall through its
`cond\', so a printed summary ignored every group: `%20.4(%s%)\' printed the
whole subject where the format asked for four columns of it, and a
`vm-summary-format\' written to line up did not line up on paper
(emacs-vm/vm#862).  The width and the maximum after a `group-begin\' are
numbers rather than strings, so they fell through as well."
  (let ((vm-display-using-mime nil))
    (dolist (case '(((group-begin 20 4 "Test Message" group-end)
                     . "                Test")
                    ((group-begin -20 4 "Test Message" group-end)
                     . "Test                ")
                    ((group-begin nil 4 "Test Message" group-end) . "Test")
                    ((group-begin 6 nil "ab" group-end) . "    ab")))
      (should (equal (list (car case)
                           (vm-ps-print-tokenized-summary nil (car case)))
                     (list (car case) (cdr case)))))))

(ert-deftest vm-ps-print-test-a-printed-group-matches-the-summary-buffer ()
  "And it is padded and cut the same way the summary buffer does it."
  (let ((vm-display-using-mime nil))
    (dolist (tokens '((group-begin 20 4 "Test Message" group-end)
                      (group-begin -20 4 "Test Message" group-end)
                      ("[" group-begin 6 nil "ab" group-end "]")))
      (should (equal (list tokens (vm-ps-print-tokenized-summary nil tokens))
                     (list tokens (with-temp-buffer
                                    (vm-tokenized-summary-insert nil tokens)
                                    (buffer-string))))))))

;;; vm-ps-print-message-internal

(defun vm-ps-print-test--header-lines (each)
  "Answer the `ps-header-lines' a print job is given, EACH as in a job per message."
  (let ((seen nil))
    (let ((vm-ps-print-message-header-lines 7)
          (vm-ps-print-each-message-header-lines 9)
          (vm-ps-print-message-left-header ''("l"))
          (vm-ps-print-message-right-header ''("r"))
          (vm-ps-print-each-message-left-header ''("l"))
          (vm-ps-print-each-message-right-header ''("r"))
          (vm-ps-print-message-function
           (lambda (&rest _) (setq seen ps-header-lines))))
      (vm-ps-print-message-internal nil each "folder" 1 nil))
    seen))

(ert-deftest vm-ps-print-test-header-lines-for-one-job ()
  "A single job reads `vm-ps-print-message-header-lines'.
Both arms of the choice named the per-message option, so this one was a
user option that did nothing."
  (should (= (vm-ps-print-test--header-lines nil) 7)))

(ert-deftest vm-ps-print-test-header-lines-for-a-job-per-message ()
  "A job per message reads `vm-ps-print-each-message-header-lines'."
  (should (= (vm-ps-print-test--header-lines t) 9)))

;;; vm-ps-print-message-fix-menu

(defvar vm-ps-print-test--menu nil
  "A menu for `vm-ps-print-message-fix-menu' to rewrite.")

(defun vm-ps-print-test--fixed-menu (each)
  "Answer the command `vm-ps-print-message-fix-menu' puts in a Print entry."
  (setq vm-ps-print-test--menu
        '("Dispose" ["Print" vm-print-message vm-message-list]))
  (vm-ps-print-message-fix-menu 'vm-ps-print-test--menu each)
  (aref (nth 1 vm-ps-print-test--menu) 1))

(ert-deftest vm-ps-print-test-fix-menu-names-a-command-that-exists ()
  "The rewritten menu entry names a command VM defines.
With EACH it wrote `vm-print-each-message', which VM has never had, so the
menu entry `vm-ps-print-message-infect-vm' installed could only fail."
  (should (eq (vm-ps-print-test--fixed-menu t) 'vm-ps-print-each-message))
  (should (fboundp (vm-ps-print-test--fixed-menu t)))
  (should (eq (vm-ps-print-test--fixed-menu nil) 'vm-ps-print-message))
  (should (fboundp (vm-ps-print-test--fixed-menu nil))))

;;; vm-ps-print-message-folder-name

(ert-deftest vm-ps-print-test-folder-name-strips-the-folder-directory ()
  "The header names the folder, not the path to it."
  (let ((vm-folder-directory "/tmp/vm-ps-print-test-folders"))
    (with-current-buffer (get-buffer-create "/tmp/vm-ps-print-test-folders/inbox")
      (unwind-protect
          (should (equal (vm-ps-print-message-folder-name) "inbox"))
        (kill-buffer (current-buffer))))))

(ert-deftest vm-ps-print-test-folder-name-keeps-a-folder-elsewhere-whole ()
  "A folder outside `vm-folder-directory' keeps its name as it stands."
  (let ((vm-folder-directory "/tmp/vm-ps-print-test-folders"))
    (with-current-buffer (get-buffer-create "/tmp/somewhere/else/inbox")
      (unwind-protect
          (should (equal (vm-ps-print-message-folder-name)
                         "/tmp/somewhere/else/inbox"))
        (kill-buffer (current-buffer))))))

(provide 'vm-ps-print-test)

;;; vm-ps-print-test.el ends here
