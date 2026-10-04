;;; forms-coverage-report.el --- Per-form coverage for VM tests -*- lexical-binding: t; -*-

;;; Commentary:

;; What `coverage-report.el' cannot tell you.  That one advises every vm-*
;; function and records which were called, so a function of forty lines
;; entered once and never again counts as covered.  This one instruments
;; every form with `testcover', so it reports what happened inside.
;;
;; Three states per form, which is edebug's own vocabulary:
;;
;;   never      the form was never evaluated.  For a `cond' arm or an `if'
;;              branch that is branch coverage: the arm was never taken.
;;   one-value  the form was evaluated and every time returned the same
;;              value.  Not a fault in itself -- plenty of forms have one
;;              answer -- but a predicate that never returned nil is a
;;              predicate no test varied, which is what mutation testing
;;              looks for.
;;   varied     evaluated, and seen to return more than one distinct value.
;;
;; This is form coverage, not path coverage: nothing here enumerates the
;; paths through a function, and no Emacs tool does.
;;
;; Run with: emacs -batch -Q -L lisp -l test/forms-coverage-report.el

;;; Code:

(require 'testcover)

(defvar vm-forms-coverage-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Base directory of VM project.")

(defconst vm-forms-coverage-exempt
  '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el")
  "Generated files, which have no coverage worth reporting.")

(defun vm-forms-coverage-files ()
  "The VM lisp files to instrument."
  (let ((dir (expand-file-name "lisp" vm-forms-coverage-dir)))
    (seq-remove (lambda (f) (member (file-name-nondirectory f)
                                    vm-forms-coverage-exempt))
                (directory-files dir t "\\.el\\'"))))

(defun vm-forms-coverage-instrument ()
  "Instrument every VM file, and answer the names of any that would not.
A file that fails is named rather than passed over: `vm-forms-coverage-test'
in test/vm-integration-test.el asserts the list is empty, since a file edebug
cannot read is a file this report is blind to."
  (let ((failed nil))
    (dolist (file (vm-forms-coverage-files))
      (condition-case err
          (testcover-start file)
        (error (push (cons (file-name-nondirectory file)
                           (error-message-string err))
                     failed))))
    (nreverse failed)))

(defun vm-forms-coverage-tally ()
  "Answer (SYMBOL NEVER ONE-VALUE VARIED) for each instrumented definition."
  (let ((rows nil))
    (mapatoms
     (lambda (sym)
       (let ((data (get sym 'edebug-coverage)))
         (when (and data (string-prefix-p "vm-" (symbol-name sym)))
           (let ((never 0) (one 0) (varied 0))
             (dotimes (i (length data))
               (cond ((eq (aref data i) 'edebug-unknown)
                      (setq never (1+ never)))
                     ((eq (aref data i) 'edebug-ok-coverage)
                      (setq varied (1+ varied)))
                     (t (setq one (1+ one)))))
             (push (list sym never one varied) rows))))))
    (sort rows (lambda (a b)
                 (if (= (nth 1 a) (nth 1 b))
                     (string< (car a) (car b))
                   (> (nth 1 a) (nth 1 b)))))))

(defun vm-forms-coverage-lines ()
  "Which source lines hold a form that was never evaluated, or only one value.

Answers an alist of (FILE (NEVER . lines) (ONE . lines) (ANY . lines)), the
line numbers sorted.  ANY is every line carrying an instrumented form at all,
which is the denominator: a line of comment or a blank one is not uncovered,
it simply has nothing to run.

Where the positions come from: `(get SYMBOL \\='edebug)\\=' holds a marker at
the definition and, third, a vector of offsets from it, one per instrumented
form.  Form I therefore sits at (+ MARKER (aref OFFSETS I)).  That is how
`testcover-mark\\=' places the splotches it shows interactively, so the data
is exact rather than guessed at."
  (let ((files nil))
    (mapatoms
     (lambda (sym)
       (let* ((data (get sym 'edebug))
              (coverage (get sym 'edebug-coverage))
              (mark (car-safe data))
              (offsets (nth 2 data)))
         (when (and coverage offsets (markerp mark)
                    (buffer-live-p (marker-buffer mark))
                    (string-prefix-p "vm-" (symbol-name sym)))
           (with-current-buffer (marker-buffer mark)
             (let* ((file (or (buffer-file-name) (buffer-name)))
                    (entry (or (assoc file files)
                               (car (push (list file nil nil nil) files)))))
               (dotimes (i (min (length coverage) (length offsets)))
                 (let* ((position (+ mark (aref offsets i)))
                        (line (and (<= (point-min) position)
                                   (<= position (point-max))
                                   (line-number-at-pos position))))
                   (when line
                     (push line (nth 3 entry))
                     (cond ((eq (aref coverage i) 'edebug-unknown)
                            (push line (nth 1 entry)))
                           ((eq (aref coverage i) 'edebug-ok-coverage) nil)
                           (t (push line (nth 2 entry)))))))))))))
    (dolist (entry files)
      (dotimes (i 3)
        (setf (nth (1+ i) entry)
              (sort (delete-dups (nth (1+ i) entry)) #'<))))
    (sort files (lambda (a b) (string< (car a) (car b))))))

(defun vm-forms-coverage-write-lines (rows)
  "Write the per-line report and an lcov file from ROWS.
Two files: one to read, and one for `genhtml' and the coverage viewers, which
speak lcov.  lcov counts executions and this knows only whether a form ran, so
a line is written as run once or not at all."
  (let ((plain (expand-file-name "test/line-coverage-results.txt"
                                 vm-forms-coverage-dir))
        (lcov (expand-file-name "test/line-coverage.info"
                                vm-forms-coverage-dir)))
    (with-temp-file plain
      (insert "VM LINE COVERAGE REPORT\n")
      (insert (format-time-string "%Y-%m-%d %H:%M:%S\n\n"))
      (insert "A line is counted only where it carries a form that was\n")
      (insert "instrumented.  NEVER lists the lines holding a form that never\n")
      (insert "ran; 1VALUE those whose forms all ran but always answered the\n")
      (insert "same thing.  Ranges are collapsed.\n\n")
      (let ((any 0) (never 0))
        (dolist (row rows)
          (setq any (+ any (length (nth 3 row)))
                never (+ never (length (nth 1 row)))))
        (insert (format "Lines with forms: %d, never reached: %d (%.1f%%)\n\n"
                        any never
                        (if (zerop any) 0.0 (* 100.0 (/ (float never) any))))))
      (dolist (row rows)
        (insert (format "%s\n  %d lines with forms, %d never reached\n"
                        (file-name-nondirectory (car row))
                        (length (nth 3 row)) (length (nth 1 row))))
        (when (nth 1 row)
          (insert (format "  NEVER:  %s\n"
                          (vm-forms-coverage-ranges (nth 1 row)))))
        (when (nth 2 row)
          (insert (format "  1VALUE: %s\n"
                          (vm-forms-coverage-ranges (nth 2 row)))))
        (insert "\n")))
    (with-temp-file lcov
      (dolist (row rows)
        (insert (format "SF:%s\n" (car row)))
        (dolist (line (nth 3 row))
          (insert (format "DA:%d,%d\n" line
                          (if (memq line (nth 1 row)) 0 1))))
        (insert (format "LF:%d\nLH:%d\nend_of_record\n"
                        (length (nth 3 row))
                        (- (length (nth 3 row)) (length (nth 1 row)))))))
    (message "Line coverage written to test/line-coverage-results.txt")
    (message "lcov written to test/line-coverage.info")))

(defun vm-forms-coverage-ranges (lines)
  "LINES as a compact string, runs of consecutive numbers collapsed."
  (let ((pieces nil)
        (start nil)
        (previous nil))
    (dolist (line (append lines (list nil)))
      (cond ((null start) (setq start line previous line))
            ((and line (= line (1+ previous))) (setq previous line))
            (t (push (if (= start previous)
                         (number-to-string start)
                       (format "%d-%d" start previous))
                     pieces)
               (setq start line previous line))))
    (mapconcat #'identity (nreverse pieces) " ")))

(defun vm-forms-coverage-report (failed)
  "Write the report, naming any FAILED files first."
  (let* ((rows (vm-forms-coverage-tally))
         (forms (apply #'+ (mapcar (lambda (r) (+ (nth 1 r) (nth 2 r) (nth 3 r)))
                                   rows)))
         (never (apply #'+ (mapcar (lambda (r) (nth 1 r)) rows)))
         (one (apply #'+ (mapcar (lambda (r) (nth 2 r)) rows)))
         (varied (apply #'+ (mapcar (lambda (r) (nth 3 r)) rows)))
         (out (expand-file-name "test/forms-coverage-results.txt"
                                vm-forms-coverage-dir)))
    (with-temp-file out
      (insert "VM FORM COVERAGE REPORT\n")
      (insert (format-time-string "%Y-%m-%d %H:%M:%S\n\n"))
      (when failed
        (insert "=== FILES THAT WOULD NOT INSTRUMENT ===\n\n")
        (insert "This report is blind to these.\n\n")
        (dolist (f failed)
          (insert (format "%s\n    %s\n" (car f) (cdr f))))
        (insert "\n"))
      (insert (format "Definitions: %d\n" (length rows)))
      (insert (format "Forms: %d\n" forms))
      (insert (format "  never evaluated: %d (%.1f%%)\n"
                      never (if (zerop forms) 0.0 (* 100.0 (/ (float never) forms)))))
      (insert (format "  always one value: %d (%.1f%%)\n"
                      one (if (zerop forms) 0.0 (* 100.0 (/ (float one) forms)))))
      (insert (format "  varied: %d (%.1f%%)\n"
                      varied (if (zerop forms) 0.0 (* 100.0 (/ (float varied) forms)))))
      (insert "\n=== DEFINITIONS WITH FORMS NEVER EVALUATED ===\n")
      (insert "Most first.  A never-evaluated `cond' arm is a branch no test took.\n\n")
      (insert (format "%-52s %6s %6s %6s\n" "definition" "never" "1value" "varied"))
      (dolist (row rows)
        (when (> (nth 1 row) 0)
          (insert (format "%-52s %6d %6d %6d\n"
                          (car row) (nth 1 row) (nth 2 row) (nth 3 row)))))
      (insert "\n=== EVERY FORM EVALUATED, SOME ALWAYS ONE VALUE ===\n")
      (insert "A form that always answered the same thing is one no test varied.\n\n")
      (dolist (row rows)
        (when (and (zerop (nth 1 row)) (> (nth 2 row) 0))
          (insert (format "%-52s %6d %6d %6d\n"
                          (car row) (nth 1 row) (nth 2 row) (nth 3 row))))))
    (message "\nForm coverage: %d forms, %d never evaluated, %d always one value"
             forms never one)
    (message "Report written to test/forms-coverage-results.txt")
    (vm-forms-coverage-write-lines (vm-forms-coverage-lines))))

;; Main
;;
;; `vm-test-init.el' first, and only then instrument.  It is what puts
;; test/opt/elpa on the load path, and VM loads its optional companions as it
;; loads: instrumenting first meant `vm-serial.el' called for `bbdb-sc' with
;; no BBDB to find, failed into the `condition-case' in `vm-load-features',
;; and provided `vm-serial' all the same -- so the later `(require
;; \='vm-serial)' in `vm-optional-test-bbdb-names-resolve' was a no-op and
;; `bbdb-sc-get-attrib' was unbound in that pass alone (emacs-vm/vm#870).
(load (expand-file-name "test/vm-test-init.el" vm-forms-coverage-dir))
(let ((failed (vm-forms-coverage-instrument)))
  (dolist (f failed)
    (message "forms-coverage: %s would not instrument: %s" (car f) (cdr f)))
  (setq vm-test-instrumented t)
  (vm-test-load-all-test-files)
  (ert-run-tests-batch t)
  (vm-forms-coverage-report failed))

;;; forms-coverage-report.el ends here
