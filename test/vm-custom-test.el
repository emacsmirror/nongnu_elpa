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

;;; Options renamed out of a misspelling keep their old name working

;; Three user options were spelled wrong in their own names.  Renaming one
;; silently would break every configuration that sets it, so each old name is
;; an obsolete alias -- setting it still sets the option, and the byte
;; compiler says which name to use instead.  Issue #589.

(defconst vm-custom-test--renamed-options
  '((vm-mime-deleteable-types           . vm-mime-deletable-types)
    (vm-mime-deleteable-type-exceptions . vm-mime-deletable-type-exceptions)
    (vm-ps-print-message-separater      . vm-ps-print-message-separator)
    ;; Older still: this one was aliased to the misspelling, and has to
    ;; follow the rename rather than being left pointing at nothing.
    (vm-mime-delete-all-attachments-types . vm-mime-deletable-types))
  "Old option name to the name it now stands for.")

(ert-deftest vm-custom-test-renamed-options-are-aliases ()
  "Each old name resolves to the option it was renamed to."
  (require 'vm-vars)
  (require 'vm-ps-print)
  (require 'vm-rfaddons)
  (dolist (pair vm-custom-test--renamed-options)
    (should (boundp (car pair)))
    (should (eq (cdr pair) (indirect-variable (car pair))))))

(ert-deftest vm-custom-test-renamed-options-are-marked-obsolete ()
  "Setting an old name warns, so a configuration using one is told to change.
An alias that is not marked obsolete keeps working and says nothing, which
leaves the misspelling in people's init files for good."
  (require 'vm-vars)
  (require 'vm-ps-print)
  (require 'vm-rfaddons)
  (dolist (pair vm-custom-test--renamed-options)
    (should (get (car pair) 'byte-obsolete-variable))))

(ert-deftest vm-custom-test-setting-an-old-name-sets-the-option ()
  "The point of the alias: an init file setting the old name still works."
  (require 'vm-vars)
  (let ((vm-mime-deletable-types nil))
    (with-no-warnings
      (setq vm-mime-deleteable-types '("application/x-test")))
    (should (equal '("application/x-test") vm-mime-deletable-types))))

(provide 'vm-custom-test)

;;; vm-custom-test.el ends here
