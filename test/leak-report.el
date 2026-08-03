;;; leak-report.el --- Report tests that leave global state behind -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Runs the suite with isolation turned off and reports, per test, which VM
;; global variables it changed and how many buffers it left behind.  Run with
;; `make test-leaks'.
;;
;; `vm-test-init.el' puts that state back after every test, so leaking it is
;; harmless day to day -- this is how to see what is being leaked anyway, which
;; matters for two reasons.  A test that leaves a folder buffer or a live
;; connection behind is usually a test whose fixture does not clean up, and that
;; is worth knowing on its own.  And the isolation only covers variables and
;; buffers: state of other kinds -- advice, hooks in non-VM variables, files --
;; is not restored, so a leak here can still be a leak that matters.
;;
;; The interesting output is the variable names, not the test names: one
;; variable appearing under many tests says where the state lives.

;;; Code:

(setq load-prefer-newer t)

(defvar leak-report-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "vm-test-init.el" leak-report-dir))
(vm-test-load-all-test-files)

;; The point is to see the leaks, so nothing may be put back.
(setq vm-test-isolate-global-state nil)

(defun leak-report--snapshot (variables)
  (let ((state (make-hash-table :test 'eq)))
    (dolist (symbol variables)
      (puthash symbol
               (condition-case nil (default-value symbol) (error :void))
               state))
    state))

(defun leak-report--changed (variables before after)
  "Return the variables whose value differs between BEFORE and AFTER."
  (let ((changed nil))
    (dolist (symbol variables)
      (let ((old (gethash symbol before))
            (new (gethash symbol after)))
        (unless (or (eq old new)
                    ;; A fresh cons of the same content is not a leak worth
                    ;; reporting; a different buffer object is, even when two
                    ;; buffers print alike.
                    (and (not (bufferp old)) (not (bufferp new))
                         (equal old new)))
          (push (list symbol old new) changed))))
    (nreverse changed)))

(defun leak-report--abbreviate (value)
  (if (bufferp value)
      (format "#<buffer %s%s>" (buffer-name value)
              (if (buffer-live-p value) "" " killed"))
    (let ((printed (format "%S" value)))
      (if (> (length printed) 60)
          (concat (substring printed 0 60) "...")
        printed))))

(let ((tests (ert-select-tests t t))
      (counts (make-hash-table :test 'eq))
      (leaked 0))
  (princ (format "Running %d tests with isolation off.\n\n" (length tests)))
  (dolist (test tests)
    (let* ((variables (vm-test-isolated-variables))
           (before (leak-report--snapshot variables))
           (buffers (length (buffer-list))))
      (let ((inhibit-message t))
        (condition-case nil (ert-run-test test) (error nil)))
      (let ((changed (leak-report--changed
                      variables before (leak-report--snapshot variables)))
            (new-buffers (- (length (buffer-list)) buffers)))
        (when (or changed (/= 0 new-buffers))
          (setq leaked (1+ leaked))
          (princ (format "%s\n" (ert-test-name test)))
          (when (/= 0 new-buffers)
            (puthash 'buffers (1+ (gethash 'buffers counts 0)) counts)
            (princ (format "    %+d buffer(s) left behind\n" new-buffers)))
          (dolist (entry changed)
            (puthash (car entry) (1+ (gethash (car entry) counts 0)) counts)
            (princ (format "    %s: %s -> %s\n"
                           (nth 0 entry)
                           (leak-report--abbreviate (nth 1 entry))
                           (leak-report--abbreviate (nth 2 entry)))))))))
  (let ((ranked nil))
    (maphash (lambda (k v) (push (cons k v) ranked)) counts)
    (princ (format "\n%d of %d tests leaked.  By what was leaked:\n\n"
                   leaked (length tests)))
    (dolist (entry (sort ranked (lambda (a b) (> (cdr a) (cdr b)))))
      (princ (format "  %5d  %s\n" (cdr entry) (car entry))))))

;;; leak-report.el ends here
