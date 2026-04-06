;;; coverage-report.el --- Runtime coverage for VM tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tracks which vm-* functions are actually called during test execution.
;; Run with: emacs -batch -Q -L lisp -l test/coverage-report.el

;;; Code:

(defvar vm-coverage-called (make-hash-table :test 'eq)
  "Hash of functions that were called during tests.")

(defvar vm-coverage-all-functions nil
  "List of all vm-* functions to track.")

(defvar vm-coverage-base-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Base directory of VM project.")

(defun vm-coverage-setup ()
  "Set up coverage tracking by advising all vm-* functions."
  ;; First load VM modules
  (let ((test-dir (file-name-directory load-file-name)))
    (load (expand-file-name "vm-test-init.el" test-dir)))
  (require 'vm-misc)
  (require 'vm-mime)
  (require 'vm-folder)
  (require 'vm-imap)

  ;; Collect all vm-* functions and add tracking advice
  (mapatoms
   (lambda (sym)
     (when (and (string-prefix-p "vm-" (symbol-name sym))
                (fboundp sym)
                (not (string-prefix-p "vm-coverage" (symbol-name sym)))
                (not (string-prefix-p "vm-test" (symbol-name sym))))
       (push sym vm-coverage-all-functions)
       (let ((fn sym))
         (advice-add sym :before
                     (lambda (&rest _)
                       (puthash fn t vm-coverage-called))
                     '((name . vm-coverage-tracker)))))))

  (message "Tracking %d vm-* functions" (length vm-coverage-all-functions)))

(defun vm-coverage-report ()
  "Generate coverage report."
  (let ((called nil)
        (uncalled nil))

    ;; Categorize
    (dolist (fn vm-coverage-all-functions)
      (if (gethash fn vm-coverage-called)
          (push fn called)
        (push fn uncalled)))

    ;; Sort
    (setq called (sort called (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (setq uncalled (sort uncalled (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

    ;; Report
    (message "\n")
    (message "============================================================")
    (message "VM TEST COVERAGE REPORT (Runtime)")
    (message "============================================================")
    (message "")
    (message "Total functions: %d" (length vm-coverage-all-functions))
    (message "Called during tests: %d" (length called))
    (message "NOT called: %d" (length uncalled))
    (message "Coverage: %.1f%%" (* 100.0 (/ (float (length called))
                                            (length vm-coverage-all-functions))))
    (message "")
    (message "--- FUNCTIONS NOT CALLED (candidates for new tests) ---")
    (message "")
    (dolist (fn uncalled)
      (message "  %s" fn))

    ;; Write to file
    (with-temp-file (expand-file-name "coverage-results.txt"
                                       (file-name-directory load-file-name))
      (insert "VM TEST COVERAGE REPORT\n")
      (insert (format-time-string "%Y-%m-%d %H:%M:%S\n\n"))
      (insert (format "Total: %d, Called: %d, Uncalled: %d\n"
                      (length vm-coverage-all-functions)
                      (length called)
                      (length uncalled)))
      (insert (format "Coverage: %.1f%%\n\n"
                      (* 100.0 (/ (float (length called))
                                  (length vm-coverage-all-functions)))))
      (insert "=== UNCALLED FUNCTIONS ===\n\n")
      (dolist (fn uncalled)
        (insert (format "%s\n" fn)))
      (insert "\n=== CALLED FUNCTIONS ===\n\n")
      (dolist (fn called)
        (insert (format "%s\n" fn))))
    (message "\nReport written to test/coverage-results.txt")))

;; Main
(progn
  (message "Setting up coverage tracking...")
  (vm-coverage-setup)

  ;; Load and run tests
  (let ((test-dir (file-name-directory load-file-name)))
    (dolist (f '("vm-misc-test.el" "vm-mime-test.el"
                 "vm-folder-test.el" "vm-imap-test.el"
                 "vm-pop-test.el" "vm-sort-test.el"
                 "vm-thread-test.el" "vm-virtual-test.el"
                 "vm-delete-test.el" "vm-mark-test.el"
                 "vm-save-test.el" "vm-undo-test.el"
                 "vm-reply-test.el" "vm-summary-test.el"
                 "vm-accessors-test.el" "vm-integration-test.el"))
      (load (expand-file-name f test-dir))))

  (ert-run-tests-batch t)
  (vm-coverage-report))

;;; coverage-report.el ends here
