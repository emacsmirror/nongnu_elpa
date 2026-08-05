;;; vm-custom-test.el --- invariants of VM's defcustom declarations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; A `:type' is documentation that Customize acts on, and it can be wrong in a
;; way nothing notices: byte compilation does not check it, and until now
;; neither did anything else in the tree.  A type that rejects the variable's own
;; default is the sharp case.  `vm-page-continuation-glyph' was declared
;; `boolean' with a string default, so customizing it stored t and VM put that
;; in the buffer as the page continuation marker; three regexp variables
;; defaulted to nil, which is how each of them is turned off, and `regexp' does
;; not accept nil, so Customize would not let a user turn them back off
;; (issue #575).
;;
;; The work is done by test/vm-custom-check.el in an Emacs of its own, because
;; seeing every declaration means loading every VM module, and those loads add
;; hooks and rewrite menus.  Doing that in the suite would leave the state
;; behind for the tests that follow.

;;; Code:

(require 'vm-test-init)

(defun vm-custom-test--run-checker ()
  "Run test/vm-custom-check.el in a fresh Emacs.  Return (STATUS . OUTPUT)."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (program (expand-file-name "vm-custom-check.el" vm-test-dir)))
    (with-temp-buffer
      (let ((status (call-process emacs nil t nil
                                  "-Q" "--batch"
                                  "-L" vm-test-lisp-dir
                                  "-l" program)))
        (cons status (buffer-string))))))

(ert-deftest vm-custom-test-every-type-accepts-its-own-default ()
  "REGRESSION: no `defcustom' has a `:type' that rejects its default value.
Issue #575.  Such a variable cannot be edited in Customize, and its declared
type says something false about what the code accepts."
  (let* ((result (vm-custom-test--run-checker))
         (output (cdr result))
         (mismatches nil))
    (dolist (line (split-string output "\n" t))
      (when (string-prefix-p "MISMATCH " line)
        (push (substring line (length "MISMATCH ")) mismatches)))
    ;; The premise: a lot of declarations really were examined, so a module that
    ;; failed to load cannot quietly turn this into a test of nothing.
    (should (string-match "checked \\([0-9]+\\) VM defcustoms" output))
    (should (> (string-to-number (match-string 1 output)) 300))
    (should (equal nil (nreverse mismatches)))
    (should (= 0 (car result)))))

(provide 'vm-custom-test)

;;; vm-custom-test.el ends here
