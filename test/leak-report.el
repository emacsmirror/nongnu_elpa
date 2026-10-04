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

;; What the isolation covers and what it cannot
;;
;; Restoring a variable puts back the reference it held, not what that
;; reference points at.  A test that mutates a shared object leaks it whether
;; the isolation is on or off, and nothing above notices, since the variable
;; still holds the same object.
;;
;; That is what the fingerprints below are for.  They cover the classes where
;; it has actually happened or would matter: keymaps, since define-key changes
;; a map in place; obarrays, which VM uses as sets and intern adds to;
;; processes and VM's own timers, which outlive the test that made them; and
;; advice, which the isolation does not touch at all.
;;
;; A fingerprint is a summary rather than a copy.  Copying every keymap around
;; every test would cost more than the tests do, and the question here is only
;; whether something changed.

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

(defun leak-report--keymap-variables (variables)
  "Those of VARIABLES whose global value is a keymap, one per keymap.
Several variables can hold the same map: vm-mode-map and vm-summary-mode-map
are one object, so reporting each would be reporting one mutation twice.  The name kept is the first, and the
others are named with it."
  (let (found seen)
    (dolist (symbol variables)
      (let ((value (condition-case nil (default-value symbol) (error nil))))
        (when (keymapp value)
          (let ((already (assq (sxhash-eq value) seen)))
            (if already
                (setcdr already (cons symbol (cdr already)))
              (push (list (sxhash-eq value) symbol) seen))))))
    (dolist (entry (nreverse seen) (nreverse found))
      (push (cons (cadr entry) (reverse (cdr entry))) found))))

(defun leak-report--keymap-label (entry)
  "How to name the keymap ENTRY in the report."
  (if (cdr (cdr entry))
      (format "%s (and %d more holding the same map)"
              (car entry) (1- (length (cdr entry))))
    (format "%s" (car entry))))

(defun leak-report--keymap-fingerprint (map)
  "What MAP binds, as a sorted list of key and command names.

Printing the keymap will not do: a menu keeps generated symbols and closures
in it, so the printed form differs from one moment to the next and every test
looks like a mutation.  What matters here is which keys are bound to which
commands, so anything that is not a symbol is recorded as its type."
  (let (bindings)
    (map-keymap
     (lambda (key binding)
       (push (format "%s=%s"
                     (if (integerp key) (single-key-description key) key)
                     (cond ((symbolp binding) binding)
                           ((keymapp binding) 'keymap)
                           (t (type-of binding))))
             bindings))
     map)
    (sort bindings #'string<)))

(defun leak-report--obarray-variables (variables)
  "Those of VARIABLES whose global value is an obarray.
VM uses obarrays as sets: the labels a folder has seen, the buffers needing a
display update.  `intern' changes one in place, which no restore of the
variable undoes."
  (let (found)
    (dolist (symbol variables (nreverse found))
      (let ((value (condition-case nil (default-value symbol) (error nil))))
        (when (or (obarrayp value)
                  ;; before Emacs 30 an obarray is a vector of nil and symbols
                  (and (vectorp value) (> (length value) 0)
                       (catch 'obarray
                         (dotimes (i (length value))
                           (let ((slot (aref value i)))
                             (unless (or (null slot) (symbolp slot))
                               (throw 'obarray nil))))
                         t)))
          (push symbol found))))))

(defun leak-report--obarray-contents (value)
  "The symbol names in obarray VALUE, sorted."
  (let (names)
    (condition-case nil
        (mapatoms (lambda (symbol) (push (symbol-name symbol) names)) value)
      (error nil))
    (sort names #'string<)))

(defun leak-report--advised-vm-functions ()
  "The VM functions that currently carry advice."
  (let (found)
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (string-prefix-p "vm-" (symbol-name symbol))
                  (advice--p (advice--symbol-function symbol)))
         (push symbol found))))
    (sort found (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun leak-report--object-fingerprints (keymaps obarrays)
  "A fingerprint per shared object worth watching.
Each entry is (LABEL . FINGERPRINT), compared with `equal' between tests.  A
fingerprint is a summary rather than a copy: the point is to notice a change,
and printing a keymap is cheaper than copying every one of them."
  (let ((fingerprints nil))
    (dolist (entry keymaps)
      (push (cons (leak-report--keymap-label entry)
                  (condition-case nil
                      (leak-report--keymap-fingerprint (default-value (car entry)))
                    (error :unreadable)))
            fingerprints))
    (dolist (symbol obarrays)
      (push (cons symbol
                  (condition-case nil
                      (leak-report--obarray-contents (default-value symbol))
                    (error :unreadable)))
            fingerprints))
    (push (cons 'processes
                (sort (mapcar #'process-name (process-list)) #'string<))
          fingerprints)
    ;; the functions rather than a count: a report saying a timer appeared is
    ;; no use without saying which one.  Only VM's own: Emacs runs timers of
    ;; its own accord -- undo-auto--boundary-timer comes and goes with editing
    ;; -- and reporting those buries the one a test left behind
    (push (cons 'timers
                (sort (delq nil
                            (mapcar (lambda (timer)
                                      (let ((name (format "%s" (timer--function
                                                               timer))))
                                        (and (string-prefix-p "vm-" name) name)))
                                    (append timer-list timer-idle-list)))
                      #'string<))
          fingerprints)
    (push (cons 'advice (leak-report--advised-vm-functions)) fingerprints)
    (nreverse fingerprints)))

(defun leak-report--object-changes (before after)
  "The labels whose fingerprint differs between BEFORE and AFTER."
  (let (changed)
    (dolist (entry before (nreverse changed))
      (let ((old (cdr entry))
            ;; assoc, not assq: a keymap label is a string, and two equal
        ;; strings are not eq -- with assq the lookup missed every time and
        ;; every test looked like it had emptied the map
        (new (cdr (assoc (car entry) after))))
        (unless (equal old new)
          (push (list (car entry) old new) changed))))))

(defun leak-report--describe-object-change (label old new)
  "One line saying how the object behind LABEL changed."
  (cond ((and (listp old) (listp new))
         (let ((added (cl-set-difference new old :test #'equal))
               (removed (cl-set-difference old new :test #'equal)))
           (format "%s: %s%s"
                   label
                   (if added (format "+%s " (leak-report--abbreviate added)) "")
                   (if removed
                       (format "-%s" (leak-report--abbreviate removed))
                     ""))))
        (t (format "%s: %s -> %s" label
                   (leak-report--abbreviate old)
                   (leak-report--abbreviate new)))))

(let ((tests (ert-select-tests t t))
      ;; equal, not eq: a keymap label is a string built for the report, so
      ;; with eq every occurrence made its own entry and the ranking counted
      ;; each one once
      (counts (make-hash-table :test 'equal))
      (leaked 0))
  (princ (format "Running %d tests with isolation off.\n\n" (length tests)))
  (dolist (test tests)
    (let* ((variables (vm-test-isolated-variables))
           (keymaps (leak-report--keymap-variables variables))
           (obarrays (leak-report--obarray-variables variables))
           (before (leak-report--snapshot variables))
           (objects-before (leak-report--object-fingerprints keymaps obarrays))
           (buffers (length (buffer-list))))
      (let ((inhibit-message t))
        (condition-case nil (ert-run-test test) (error nil)))
      ;; The composition timer is cancelled the way the isolation does it, or a
      ;; stray timer fires during later tests and their leaks become nonsense.
      (vm-test-cancel-composition-timer)
      (let ((changed (leak-report--changed
                      variables before (leak-report--snapshot variables)))
            (objects (leak-report--object-changes
                      objects-before
                      (leak-report--object-fingerprints keymaps obarrays)))
            (new-buffers (- (length (buffer-list)) buffers)))
        (when (or changed objects (/= 0 new-buffers))
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
                           (leak-report--abbreviate (nth 2 entry)))))
          (dolist (entry objects)
            (puthash (car entry) (1+ (gethash (car entry) counts 0)) counts)
            (princ (format "    mutated %s\n"
                           (leak-report--describe-object-change
                            (nth 0 entry) (nth 1 entry) (nth 2 entry)))))))))
  (let ((ranked nil))
    (maphash (lambda (k v) (push (cons k v) ranked)) counts)
    (princ (format "\n%d of %d tests leaked.  By what was leaked:\n\n"
                   leaked (length tests)))
    (dolist (entry (sort ranked (lambda (a b) (> (cdr a) (cdr b)))))
      (princ (format "  %5d  %s\n" (cdr entry) (car entry))))))

;;; leak-report.el ends here
