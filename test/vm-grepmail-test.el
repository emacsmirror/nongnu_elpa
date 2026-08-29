;;; vm-grepmail-test.el --- Tests for vm-grepmail.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; vm-grepmail drives an external program and reads its output as a folder,
;; so whether that program succeeded is the whole question: a grepmail that
;; failed or was killed leaves a truncated buffer that must not be read as
;; mail.  The condition that decided it was inverted (emacs-vm/vm#773).

;;; Code:

(require 'vm-test-init)
(require 'vm-grepmail)

;;; Whether the process did its job

(ert-deftest vm-grepmail-test-a-clean-exit-is-the-only-success ()
  "REGRESSION: only exiting with status zero counts as finished.
Issue #773.  The condition raised only where the status was zero and the
process had not exited, so every real failure -- a non-zero status, a signal
-- was taken for success and the truncated output read as a folder."
  (should (vm-grepmail-finished-cleanly-p 'exit 0))
  (should (vm-grepmail-finished-cleanly-p 'finished 0))
  ;; grepmail failed
  (should-not (vm-grepmail-finished-cleanly-p 'exit 3))
  ;; grepmail was killed
  (should-not (vm-grepmail-finished-cleanly-p 'signal 9))
  (should-not (vm-grepmail-finished-cleanly-p 'signal 0))
  ;; not finished at all
  (should-not (vm-grepmail-finished-cleanly-p 'run 0)))

(defun vm-grepmail-test--status-of (&rest args)
  "Run a program to completion and answer (STATE . EXIT-STATUS).
A real process rather than a table of numbers, so that what the sentinel is
handed is what the test judges."
  (let ((process (make-process :name "vm-grepmail-test"
                               :command args
                               :buffer (generate-new-buffer " *grepmail-test*")
                               :noquery t)))
    (unwind-protect
        (progn
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (cons (process-status process) (process-exit-status process)))
      (kill-buffer (process-buffer process)))))

(ert-deftest vm-grepmail-test-a-real-failure-is-not-clean ()
  "REGRESSION: a program that exits non-zero is judged a failure.
Issue #773, checked against a process rather than a truth table: `exit 3' is
what a grepmail that could not read a folder does, and it used to be taken
for success."
  (let ((ok (vm-grepmail-test--status-of "sh" "-c" "exit 0"))
        (bad (vm-grepmail-test--status-of "sh" "-c" "exit 3")))
    (should (vm-grepmail-finished-cleanly-p (car ok) (cdr ok)))
    (should (equal (cdr bad) 3))
    (should-not (vm-grepmail-finished-cleanly-p (car bad) (cdr bad)))))

(ert-deftest vm-grepmail-test-a-killed-program-is-not-clean ()
  "REGRESSION: a program killed by a signal is judged a failure.
Issue #773.  Its output is whatever it had written when it died."
  (let ((killed (vm-grepmail-test--status-of "sh" "-c" "kill -9 $$")))
    (should-not (vm-grepmail-finished-cleanly-p (car killed) (cdr killed)))))

;;; What the file defines

(ert-deftest vm-grepmail-test-functions-exist ()
  "The entry point and the pieces the sentinel uses."
  (should (fboundp 'vm-grepmail))
  (should (commandp 'vm-grepmail))
  (should (fboundp 'vm-grepmail-process-filter))
  (should (fboundp 'vm-grepmail-process-done))
  (should (fboundp 'vm-grepmail-grab-message))
  (should (fboundp 'vm-grepmail-finished-cleanly-p)))

(provide 'vm-grepmail-test)

;;; vm-grepmail-test.el ends here
