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

(ert-deftest vm-custom-test-every-option-declares-a-type ()
  "REGRESSION: every VM user option declares a `:type'.
Customize offers a raw sexp editor for an option without one, which asks the
reader to know the structure the code wants and helps them with none of it.

Thirty-two looked untyped (emacs-vm/vm#837) and every one of them was an
obsolete name.  A `defvaralias' carries no type of its own, the option it
points at carries it, and `customize-option' resolves the alias before it
builds anything, saying which option it went to.  The checker resolves them
too, so a genuinely untyped option is what would show here."
  (let* ((result (vm-custom-test--run-checker))
         (output (cdr result))
         (untyped nil))
    (dolist (line (split-string output "\n" t))
      (when (string-prefix-p "UNTYPED " line)
        (push (substring line (length "UNTYPED ")) untyped)))
    ;; the same premise as above: a load that went wrong checks nothing
    (should (string-match "checked \\([0-9]+\\) VM defcustoms" output))
    (should (> (string-to-number (match-string 1 output)) 300))
    (should (equal nil (sort untyped #'string<)))))

;;; The misspelled option names are gone, not aliased

;; Three user options were spelled wrong in their own names.  They were
;; renamed, and the misspellings were *not* kept as aliases: a name nobody
;; meant to type is not worth carrying.  Issue #589.
;;
;; The cost of that decision is silent: an init file still setting a
;; misspelling gets no error, the option simply keeps its default.  These tests
;; pin what was decided, so a later well-meaning re-addition has to argue with
;; them rather than slip in.

(defconst vm-custom-test--corrected-names
  '((vm-mime-deleteable-types           . vm-mime-deletable-types)
    (vm-mime-deleteable-type-exceptions . vm-mime-deletable-type-exceptions)
    (vm-ps-print-message-separater      . vm-ps-print-message-separator))
  "Misspelling that was dropped, and the option it used to name.")

(ert-deftest vm-custom-test-corrected-names-exist ()
  "Each corrected spelling is a real user option."
  (require 'vm-vars)
  (require 'vm-ps-print)
  (dolist (pair vm-custom-test--corrected-names)
    (should (boundp (cdr pair)))
    (should (get (cdr pair) 'standard-value))))

(ert-deftest vm-custom-test-misspellings-are-gone ()
  "No misspelling is left bound, as an alias or otherwise."
  (require 'vm-vars)
  (require 'vm-ps-print)
  (dolist (pair vm-custom-test--corrected-names)
    (should-not (boundp (car pair)))))

(ert-deftest vm-custom-test-confusing-names-keep-an-alias ()
  "A name renamed for being confusing keeps its old name for good.
Unlike the misspellings above, which were dropped: a name someone chose and
typed on purpose stays working, and there is no plan to remove it.  Issue
#466 renamed `From_-with-Content-Length' to `mboxcl2' and this option with
it."
  (require 'vm-vars)
  (should (boundp 'vm-trust-From_-with-Content-Length))
  (should (eq 'vm-trust-content-length
              (indirect-variable 'vm-trust-From_-with-Content-Length)))
  (should (get 'vm-trust-From_-with-Content-Length 'byte-obsolete-variable))
  (let ((vm-trust-content-length nil))
    (with-no-warnings
      (setq vm-trust-From_-with-Content-Length t))
    (should (eq t vm-trust-content-length))))

(ert-deftest vm-custom-test-the-oldest-renames-are-gone ()
  "The compatibility aliases from 8.1.1 and before are no longer defined.

They had been telling people to stop for four release cycles.  This test
replaces one that asserted `vm-mime-delete-all-attachments-types\' stays:
it does not, and a test saying so would keep the name alive by accident."
  (require 'vm-vars)
  (dolist (name '(vm-mime-delete-all-attachments-types
                  vm-mime-delete-all-attachments-types-exceptions
                  vm-mime-save-all-attachments-types
                  vm-mime-save-all-attachments-types-exceptions))
    (should-not (boundp name)))
  ;; and what they were renamed to is still here
  (dolist (name '(vm-mime-deletable-types
                  vm-mime-deletable-type-exceptions
                  vm-mime-saveable-types
                  vm-mime-saveable-type-exceptions))
    (should (boundp name))))

;;; The generated manual files must not depend on the machine that built them

;; They are generated *and committed*, so a default worked out from the
;; environment -- a home directory, a temporary directory, the user's own name
;; -- puts one developer's machine into the tree, makes the file differ for
;; everyone else who builds it, and fails `check-reference' for all but the
;; last.  It also published @diekhans' home directory and full name.
;; `vm-reference-insert-default' says the value is worked out at load time
;; instead.  Issue #600.

(defconst vm-custom-test--generated-texinfo
  (mapcar (lambda (name)
            (expand-file-name name (expand-file-name "../info" vm-test-dir)))
          '("vm-reference.texinfo" "vm-docstrings.texinfo"))
  "The texinfo files generated from the docstrings and committed.")

(ert-deftest vm-custom-test-generated-files-name-no-machine ()
  "Neither committed file holds anything belonging to the machine it was built on.
Reads the files rather than regenerating them, so this is fast and so it
checks what is actually committed."
  (dolist (file vm-custom-test--generated-texinfo)
    (should (file-readable-p file))
    (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
      ;; a path or an address is looked for as it stands
      (dolist (private (list (expand-file-name "~")
                             (directory-file-name temporary-file-directory)
                             (and (stringp user-mail-address) user-mail-address)))
        (when (and (stringp private) (> (length private) 3))
          (should-not (string-search private text))))
      ;; a name only as a whole word: a login name is a few letters and turns
      ;; up inside ordinary words, "markd" inside "text/markdown" being the
      ;; one that failed here
      (dolist (private (list (user-login-name)
                             (and (stringp user-full-name) user-full-name)))
        (when (and (stringp private) (> (length private) 3))
          (should-not (string-match-p (concat "\\_<" (regexp-quote private)
                                              "\\_>")
                                      text)))))))

(provide 'vm-custom-test)

;;; vm-custom-test.el ends here
