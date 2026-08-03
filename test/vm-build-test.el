;;; vm-build-test.el --- Tests for the build system -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Invariants of the `Makefile.in' templates that configure turns into the
;; build system.  These are checked by reading the templates rather than by
;; running make, so they hold for every make implementation and cost nothing.
;;
;; They exist because the build system has no other test coverage, and the
;; failures they catch are quiet ones: GNU make keeps going after a failed
;; command inside a `for' loop, so a stale file name in an install list makes
;; the file silently not get installed while make still reports success.

;;; Code:

(require 'vm-test-init)

(defvar vm-build-test--root
  (file-name-as-directory (expand-file-name ".." vm-test-dir))
  "Top of the VM source tree.")

(defun vm-build-test--makefile-templates ()
  "Return the absolute names of the `Makefile.in' templates configure uses.
These are the directories named in `AC_CONFIG_FILES'.  src/ is deliberately
absent: its Makefile was dropped from the build, and a stale src/Makefile.in
left in a working tree is not part of the project."
  (let (found)
    (dolist (dir '("" "lisp" "info" "test" "pixmaps"))
      (let ((file (expand-file-name (concat (if (equal dir "") "" (concat dir "/"))
                                            "Makefile.in")
                                    vm-build-test--root)))
        (when (file-exists-p file)
          (push file found))))
    (nreverse found)))

(defun vm-build-test--make-variable (file var)
  "Return the words VAR is assigned with `=' or `+=' in makefile FILE."
  (let (words)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote var) "[ \t]*\\+?=[ \t]*\\(.*\\)$")
              nil t)
        (setq words (append words (split-string (match-string 1) nil t)))))
    words))

(ert-deftest vm-build-test-doc-sources-exist ()
  "REGRESSION: every file the top Makefile installs as documentation exists.
`SOURCES' listed README and TODO, neither of which is in the tree -- the
former was renamed README.md and the latter deleted.  GNU make ran the
install loop, `install' failed on each, and because nothing checked the
status make still exited 0, so the docs quietly went uninstalled.  BSD make,
which runs recipes under `sh -e', aborted the whole install instead."
  (let ((missing nil))
    (dolist (file (vm-build-test--make-variable
                   (expand-file-name "Makefile.in" vm-build-test--root)
                   "SOURCES"))
      (unless (file-exists-p (expand-file-name file vm-build-test--root))
        (push file missing)))
    (should (equal missing nil))))

(ert-deftest vm-build-test-no-elc-for-never-compiled-lisp ()
  "REGRESSION: no .elc is listed for lisp that is never byte-compiled.
`vm-custom-make-dependencies' writes vm-cus-load.el with a
`no-byte-compile: t' local variable, so vm-cus-load.elc is never produced.
Listing it in `emacs_OBJECTS' put it on the install list, where it failed
the same silent way as the missing docs above."
  (let ((objects (vm-build-test--make-variable
                  (expand-file-name "lisp/Makefile.in" vm-build-test--root)
                  "emacs_OBJECTS")))
    (should objects)
    (should-not (member "vm-cus-load.elc" objects)))
  ;; The premise: that file really is marked never-to-be-compiled.  It is
  ;; generated, so only check when the tree has been built.
  (let ((generated (expand-file-name "lisp/vm-cus-load.el" vm-build-test--root)))
    (when (file-exists-p generated)
      (with-temp-buffer
        (insert-file-contents generated)
        (goto-char (point-min))
        (should (re-search-forward "no-byte-compile:[ \t]*t" nil t))))))

(ert-deftest vm-build-test-no-gnu-only-make-constructs ()
  "REGRESSION: the makefile templates use no GNU-make-only syntax.
VM is expected to build with BSD make too (issue #443, FreeBSD packaging).
`$(shell pwd)' was the live offender: BSD make has no `shell' function, so
it expanded to nothing and rooted the XEmacs package tree at \"/\".

Conditionals are matched only at column 0, since `else'/`endif' also appear
tab-indented inside shell `if' statements in recipes, which is fine.
Comment lines are skipped, so the comments explaining this very fix do not
trip it."
  (let ((offenders nil)
        (pattern (concat "\\$(\\(?:shell\\|wildcard\\|patsubst\\|foreach\\|"
                         "addprefix\\|addsuffix\\|notdir\\)[ \t]"
                         "\\|^\\(?:ifeq\\|ifneq\\|ifdef\\|ifndef\\|else\\|endif\\)\\b"
                         "\\|^export[ \t]")))
    (dolist (file (vm-build-test--makefile-templates))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((line 0))
          (while (not (eobp))
            (setq line (1+ line))
            (let ((text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`[ \t]*#" text)
                (when (string-match pattern text)
                  (push (format "%s:%d: %s"
                                (file-relative-name file vm-build-test--root)
                                line (match-string 0 text))
                        offenders))))
            (forward-line 1)))))
    (should (equal (nreverse offenders) nil))))


;;; the build refuses an Emacs too old to run VM (issue #526)

(defun vm-build-test--package-requires-emacs ()
  "Return the Emacs version vm.el's Package-Requires header asks for."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "vm.el" vm-test-lisp-dir))
    (goto-char (point-min))
    (when (re-search-forward ";; Package-Requires:.*(emacs \"\\([0-9.]+\\)\")"
                             nil t)
      (match-string 1))))

(ert-deftest vm-build-test-minimum-emacs-version-is-read-from-vm-el ()
  "The minimum version is read out of vm.el rather than copied into the build.
Issue #526.  Two places already have to agree -- the Package-Requires header and
`vm-min-emacs-version' -- and a third copy in the build would be one more to
fall out of step.  This also checks those two agree with each other, which
nothing else does."
  (load (expand-file-name "vm-build.el" vm-test-lisp-dir) nil t)
  (let ((minimum (vm-build-minimum-emacs-version vm-test-lisp-dir))
        (declared (vm-build-test--package-requires-emacs)))
    (should (stringp minimum))
    (should (string-match-p "\\`[0-9]+\\.[0-9]" minimum))
    (should (equal minimum declared))))

(ert-deftest vm-build-test-refuses-an-old-emacs ()
  "REGRESSION: building with too old an Emacs is an error, not a warning.
Issue #526.  An Emacs too old to run VM will still byte-compile it, mostly
without complaint, and the failure turns up at run time instead -- which is how
#524 happened, with an old Emacs first on root's PATH.  vm.el checks the version
when VM starts; this checks it where the wrong Emacs actually gets chosen."
  (load (expand-file-name "vm-build.el" vm-test-lisp-dir) nil t)
  (let ((minimum (vm-build-minimum-emacs-version vm-test-lisp-dir)))
    ;; This Emacs is new enough, so the check passes and returns the minimum.
    (should (equal minimum (vm-build-check-emacs-version vm-test-lisp-dir)))
    ;; An older one is refused, and the message names both versions.
    (let* ((emacs-version "26.3")
           (message (condition-case err
                        (progn (vm-build-check-emacs-version vm-test-lisp-dir)
                               nil)
                      (error (error-message-string err)))))
      (should message)
      (should (string-match-p (regexp-quote minimum) message))
      (should (string-match-p "26\\.3" message)))))

(provide 'vm-build-test)

;;; vm-build-test.el ends here
