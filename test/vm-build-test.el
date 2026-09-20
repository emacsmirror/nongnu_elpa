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
which runs recipes under `sh -e', aborted the whole install instead.

A word that is a glob has to match something rather than name something,
`SOURCES' being where the NEWS files are matched by pattern."
  (let ((missing nil))
    (dolist (file (vm-build-test--make-variable
                   (expand-file-name "Makefile.in" vm-build-test--root)
                   "SOURCES"))
      (let ((path (expand-file-name file vm-build-test--root)))
        ;; A word may be a shell glob, which is how the NEWS files are named
        ;; so that starting NEWS-4.md needs no edit to the Makefile.  It has
        ;; to match something: a glob matching nothing installs nothing, and
        ;; is the same silent failure as a name that does not exist.
        (unless (if (string-match-p "[*?[]" file)
                    (file-expand-wildcards path)
                  (file-exists-p path))
          (push file missing))))
    (should (equal missing nil))))

(ert-deftest vm-build-test-no-elc-for-never-compiled-lisp ()
  "REGRESSION: no .elc is listed for lisp that is never byte-compiled.
`vm-custom-make-dependencies' writes vm-cus-load.el with a
`no-byte-compile: t' local variable, so vm-cus-load.elc is never produced.
Listing it in `OBJECTS' put it on the install list, where it failed
the same silent way as the missing docs above.

The variable was `emacs_OBJECTS' until XEmacs support was dropped, there
having been an `xemacs_OBJECTS' beside it that the flavor chose between."
  (let ((objects (vm-build-test--make-variable
                  (expand-file-name "lisp/Makefile.in" vm-build-test--root)
                  "OBJECTS")))
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


;;; substituting an elisp value into a shell command (issue #495)

(ert-deftest vm-build-test-otherdirs-substitution-is-quoted ()
  "REGRESSION: the OTHERDIRS assignment in a recipe is shell-quoted.
Issue #495: `--with-other-dirs=/path' makes configure substitute an elisp list,
`(\"/path\")', into

    EMACS_COMP = OTHERDIRS=@OTHERDIRS@ ...

and the shell then meets an unquoted parenthesis:

    /bin/sh: 2: Syntax error: \"(\" unexpected

so every recipe using EMACS_COMP dies and the build cannot get past the
autoloads.  It went unnoticed because with the option absent the value is the
bare word `nil', which the shell accepts, so the default build works."
  (let ((template (expand-file-name "lisp/Makefile.in" vm-build-test--root)))
    (should (file-exists-p template))
    (with-temp-buffer
      (insert-file-contents template)
      (goto-char (point-min))
      (should (re-search-forward "^EMACS_COMP[ \t]*=[ \t]*OTHERDIRS=\\(.\\)" nil t))
      ;; the character after the = has to open a quote, not the value itself
      (should (member (match-string 1) '("'" "\""))))))

(ert-deftest vm-build-test-help-strings-are-not-glued-to-a-macro-name ()
  "REGRESSION: a conditional in a help string is a token of its own.
Issue #495: `VM_ARG_SUBST' built its help text as `--with-$2ifelse($3, , , =$3)'.
With $2 expanding to `other-dirs' that leaves `--with-other-dirsifelse(...)',
in which m4 reads `dirsifelse' as one word -- so the conditional was never a
macro call and went into ./configure --help verbatim:

    --with-other-dirsifelse(DIRS, , , =DIRS)

The fix is to close the quoted string first, `[--with-$2]m4_ifval(...)', which
makes the macro name its own token.  Every option `VM_ARG_SUBST' defines was
affected, `--with-package-dir' as well."
  (let ((configure-ac (expand-file-name "configure.ac" vm-build-test--root)))
    (should (file-exists-p configure-ac))
    (with-temp-buffer
      (insert-file-contents configure-ac)
      ;; Comments are skipped: the explanation of this bug in configure.ac
      ;; quotes the broken form, and so would match.
      (goto-char (point-min))
      (let ((glued nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-match-p "\\`[ \t]*\\(#\\|dnl\\)" line)
              ;; no macro name may follow a $N expansion directly
              (when (string-match-p "\\$[0-9]\\(ifelse\\|m4_if\\)" line)
                (push line glued))))
          (forward-line 1))
        (should (equal nil glued))))))

;;; The generated autoloads file, which is loaded before anything else of VM's

(defun vm-build-test--load-in-a-clean-emacs (form)
  "Evaluate FORM in a batch Emacs that has only lisp/ on its load-path.
Returns (EXIT-STATUS . OUTPUT).  A subprocess is the only way to see what a
user's startup sees: this Emacs has VM loaded already, so nothing here would
notice a loaddefs file that cannot be loaded on its own."
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil
                   "-batch" "-Q"
                   "-L" (expand-file-name "lisp" vm-build-test--root)
                   "--eval" (prin1-to-string form))))
      (cons status (buffer-string)))))

(ert-deftest vm-build-test-autoloads-load-on-their-own ()
  "`(require \\='vm-autoloads)' works in an Emacs with nothing else loaded.
That is what INSTALL.md tells anyone running from a checkout to do, and it
broke: an autoloaded defcustom whose default value read another VM variable
put the value form in the loaddefs file, where the variable it read did not
exist yet, and startup died with \"Symbol's value as variable is void:
vm-included-text-prefix\" (emacs-vm/vm#608)."
  (let ((result (vm-build-test--load-in-a-clean-emacs '(require 'vm-autoloads))))
    (should (equal (car result) 0))
    (should-not (string-match-p "void-variable\\|Symbol's value as variable"
                                (cdr result)))))

(ert-deftest vm-build-test-vm-vars-autoloads-nothing ()
  "No option in vm-vars.el is autoloaded, which is what makes the above safe.
Every VM file requires vm-vars, so autoloading an option from it gains
nothing, and an autoloaded default that reads another variable depends on the
order the two happen to appear in the file.  Four cookies arrived with the
vm-rfaddons merge, where they had been needed because that file was an add-on
loaded on demand."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/vm-vars.el"
                                            vm-build-test--root))
    (goto-char (point-min))
    (should-not (re-search-forward "^;;;###autoload" nil t))))

(ert-deftest vm-build-test-the-makefiles-run-no-tests-themselves ()
  "The tests are run by test/test-runner and by nothing else.

Two places deciding what a test run is meant a pass that existed only as a
Makefile target -- the live servers, the mock servers, the optional packages
-- and no one place that knew about all of them.  A Makefile that loads a
runner or vm-test-init.el is that split coming back."
  (let ((offenders nil))
    (dolist (file (vm-build-test--makefile-templates))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((line 0))
          (while (not (eobp))
            (setq line (1+ line))
            (let ((text (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`[ \t]*\\(#\\|@#\\)" text)
                (when (string-match-p
                       "-l +[^ ]*\\(run-[a-z-]*tests\\|vm-test-init\\|leak-report\\|coverage-report\\)"
                       text)
                  (push (format "%s:%d: %s"
                                (file-relative-name file vm-build-test--root)
                                line text)
                        offenders))))
            (forward-line 1)))))
    (should (equal (nreverse offenders) nil))))

(ert-deftest vm-build-test-a-test-run-fetches-the-optional-packages-first ()
  "The test target depends on the optional-package stamp, so a run covers as
much as the machine can rather than skipping whatever nobody remembered to
install.  A stamp because the packages land in directories whose names carry a
version, which no rule can name in advance."
  (let ((makefile (expand-file-name "test/Makefile.in" vm-build-test--root)))
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (should (re-search-forward "^OPTIONAL_STAMP *= *\\(.*\\)$" nil t))
      (goto-char (point-min))
      (should (re-search-forward "^test: .*\\$(OPTIONAL_STAMP)" nil t))
      ;; and the way past it, for a machine that cannot fetch them
      (goto-char (point-min))
      (should (re-search-forward "^test-no-opt:[ \t]*$" nil t)))))

(defun vm-build-test--bare-install-lines (file)
  "Recipe lines in FILE that run `$(INSTALL)' rather than `$(INSTALL_DATA)'.
Each answered as \"path:line: text\".  A comment line is not a recipe, and
`INSTALL_PROGRAM' and `INSTALL_SCRIPT' expand to `${INSTALL}' in their own
assignments, which are not recipes either."
  (let ((found nil)
        (line 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (1+ line))
        (let ((text (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (and (string-match-p "\\`\t" text)
                     (not (string-match-p "\\`\t[ \t]*\\(@#\\|#\\)" text))
                     (string-match-p "\\$[({]INSTALL[)}]" text))
            (push (format "%s:%d: %s"
                          (file-relative-name file vm-build-test--root)
                          line text)
                  found)))
        (forward-line 1)))
    (nreverse found)))

(ert-deftest vm-build-test-data-is-installed-with-install-data ()
  "REGRESSION: no makefile installs a data file with a bare `$(INSTALL)'.
`INSTALL' is `install -c', which means mode 0755, and `INSTALL_DATA' is the
same with `-m 644'.  info/Makefile.in used the bare one, so the whole manual
was installed executable.  VM installs no programs, so a bare `$(INSTALL)' in
any of the templates is this bug again.

Read from the templates rather than by installing, so it holds for every make
and needs no writable prefix."
  (let ((offenders nil))
    (dolist (file (vm-build-test--makefile-templates))
      (setq offenders
            (append offenders (vm-build-test--bare-install-lines file))))
    (should (equal offenders nil))))

(defun vm-build-test--matches (file regexp group)
  "Return GROUP of every match of REGEXP in FILE."
  (let (found)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string group) found)))
    (nreverse found)))

(defun vm-build-test--substituted-variables ()
  "Return the names configure substitutes into the `Makefile.in' templates."
  (let (found)
    (dolist (file (vm-build-test--makefile-templates))
      (setq found (append found (vm-build-test--matches
                                 file "@\\([A-Z][A-Z0-9_]*\\)@" 1))))
    (delete-dups found)))

(defun vm-build-test--absolute-path-variables ()
  "Return the names configure.ac looks up with `AC_PATH_PROG'.
That macro records the absolute path found on the build machine."
  (vm-build-test--matches
   (expand-file-name "configure.ac" vm-build-test--root)
   "^AC_PATH_PROG(\\[?\\([A-Z][A-Z0-9_]*\\)\\]?," 1))

(ert-deftest vm-build-test-no-tool-path-is-baked-into-a-makefile ()
  "REGRESSION: no `AC_PATH_PROG' name is substituted into a Makefile.
`AC_PATH_PROG' answers the absolute path on the machine that ran configure,
so lisp/Makefile came out with RM = /opt/local/libexec/gnubin/rm and the same
for ls, mkdir and rmdir.  A packager who builds in one place and runs in
another then calls tools that are not there.  `AC_CHECK_PROG' records the
bare name, which make looks up in PATH at build time.

A tool configure uses only for itself may still be an absolute path, GREP
being the one, so what is forbidden is the pair: looked up by path, and
reaching a template."
  (let ((baked (seq-intersection (vm-build-test--absolute-path-variables)
                                 (vm-build-test--substituted-variables))))
    (should (equal baked nil))))

(defun vm-build-test--recipe-lines (file regexp)
  "Recipe lines of FILE matching REGEXP, each as \"path:line: text\"."
  (let ((line 0) (found nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (1+ line))
        (let ((text (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (and (string-match-p "\\`\t" text)
                     (not (string-match-p "\\`\t[ \t]*@?#" text))
                     (string-match-p regexp text))
            (push (format "%s:%d: %s"
                          (file-relative-name file vm-build-test--root)
                          line text)
                  found)))
        (forward-line 1)))
    (nreverse found)))

(ert-deftest vm-build-test-no-makefile-takes-autoconfs-install ()
  "REGRESSION: the templates name VM_INSTALL, never autoconf's INSTALL.
config.status rewrites a relative INSTALL for each subdirectory, so a bare
name substituted through `@INSTALL@' reached lisp/Makefile as `../install'
and the install died with \"No such file or directory\".  VM_INSTALL is
configure's own answer, either the bare name or a full path, and nothing
rewrites it.

INSTALL_PROGRAM and INSTALL_SCRIPT are gone with it.  VM installs no
programs, and both were substituted into two templates and used by no
recipe."
  (let ((offenders nil))
    (dolist (file (vm-build-test--makefile-templates))
      (dolist (var '("INSTALL" "INSTALL_DATA" "INSTALL_PROGRAM" "INSTALL_SCRIPT"))
        (when (with-temp-buffer
                (insert-file-contents file)
                (search-forward (concat "@" var "@") nil t))
          (push (format "%s: @%s@" (file-relative-name file vm-build-test--root) var)
                offenders))))
    (should (equal offenders nil))))

(ert-deftest vm-build-test-data-is-installed-one-file-at-a-time ()
  "REGRESSION: every `$(INSTALL_DATA)' call installs one file, named `$$i'.
install-sh takes a single source and ignores the rest without a word, so
`$(INSTALL_DATA) ${INFO_ALL_FILES} $(infodestdir)' put vm.info in place and
silently dropped vm.info-1 and vm.info-2: a machine with no install of its
own got half a manual and an exit status of zero.  Every other directory
already looped, and info/ does now.

A source of `$$i' or `$$f' is the loop variable, which is one file whatever
the list holds."
  (let ((offenders nil))
    (dolist (file (vm-build-test--makefile-templates))
      (dolist (line (vm-build-test--recipe-lines file "\\$(INSTALL_DATA)"))
        (unless (string-match-p "\\$(INSTALL_DATA)[ \t]+\"?\\$\\$[a-z]+\\b" line)
          (push line offenders))))
    (should (equal offenders nil))))

(ert-deftest vm-build-test-no-template-bakes-the-configure-directory ()
  "REGRESSION: no template substitutes a directory configure resolved.
@abs_builddir@ is where configure ran, written into the Makefile, so a tree
configured through one path and built through another writes its autoloads
somewhere else.  On the machine this was found on the two paths differed and
lisp/vm-autoloads.el came out with 676 lines naming
../../../../../../../System/Volumes/... as the file to load.  `pwd' in the
recipe is where make is, which is the directory meant."
  (let ((offenders nil))
    (dolist (file (vm-build-test--makefile-templates))
      (dolist (var '("abs_builddir" "abs_srcdir" "abs_top_builddir" "abs_top_srcdir"))
        (when (with-temp-buffer
                (insert-file-contents file)
                (search-forward (concat "@" var "@") nil t))
          (push (format "%s: @%s@" (file-relative-name file vm-build-test--root) var)
                offenders))))
    (should (equal offenders nil))))

(ert-deftest vm-build-test-the-autoloads-name-no-directory ()
  "REGRESSION: lisp/vm-autoloads.el loads its files by bare name.
What goes wrong shows up here rather than in the recipe: an autoload naming
a directory names one on the machine that built it, and the file is
installed as it stands.  Skipped where the tree has not been built."
  (let ((file (expand-file-name "lisp/vm-autoloads.el" vm-build-test--root)))
    (skip-unless (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should-not (re-search-forward "^(autoload '[^ ]+ \"[^\"]*/" nil t)))))

(provide 'vm-build-test)

;;; vm-build-test.el ends here
