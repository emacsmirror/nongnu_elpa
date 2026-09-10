;;; vm-reference-test.el --- Tests for info/gen-reference.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The manual's reference appendix is generated from the docstrings in lisp/
;; (issue #586).  These tests cover the docstring-to-texinfo conversion, which
;; is where a manual build breaks, and check that the collection finds what it
;; should.  They do not run the generator over the whole of VM: that is what
;; `make -C info' does, and makeinfo is the judge of it.

;;; Code:

(require 'vm-test-init)
(require 'vm)

(load (expand-file-name "../info/gen-reference.el" vm-test-dir) nil t)

;;; Texinfo escaping

(ert-deftest vm-reference-test-escapes-texinfo-specials ()
  "@, { and } are quoted, since a docstring may hold any of them."
  (should (equal (vm-reference-escape "mail@@example.com {a}")
                 "mail@@@@example.com @{a@}"))
  (should (equal (vm-reference-docstring "see @code") "see @@code")))

(ert-deftest vm-reference-test-marks-up-quotes ()
  "A quoted symbol becomes @code.
`substitute-command-keys' has already turned `foo\\=' into curved quotes by
the time the markup runs."
  (should (equal (vm-reference-docstring "Set `vm-imap-max-message-size'.")
                 "Set @code{vm-imap-max-message-size}."))
  ;; Escaping happens first, so a brace inside a quoted name stays quoted.
  (should (equal (vm-reference-docstring "`a{b}'") "@code{a@{b@}}")))

(ert-deftest vm-reference-test-strips-the-old-option-marker ()
  "The leading asterisk of an old-style user option docstring is not text.
392 of VM's docstrings still begin with one."
  (should (equal (vm-reference-docstring "*Non-nil means do it.")
                 "Non-nil means do it."))
  (should (equal (vm-reference-docstring "Non-nil * means do it.")
                 "Non-nil * means do it.")))

;;; Examples

(ert-deftest vm-reference-test-wraps-example-blocks ()
  "Two or more indented lines become @example, so texinfo leaves them alone."
  (should (equal (vm-reference-docstring "Do this:\n  (setq a 1)\n  (setq b 2)\nand that.")
                 "Do this:\n@example\n  (setq a 1)\n  (setq b 2)\n@end example\nand that.")))

(ert-deftest vm-reference-test-leaves-an-indented-paragraph-alone ()
  "A single indented line starts a paragraph in many VM docstrings.
Wrapping one in @example gives an unwrapped line in the middle of prose,
which is what the first version of this generator did to
`vm-mime-alternative-show-method' -- seven times in the one entry."
  (let ((converted (vm-reference-docstring
                    "First paragraph.\n\n  A second one, indented,\ncontinuing here.")))
    (should-not (string-match-p "@example" converted))))

;;; What Emacs adds to a docstring

(ert-deftest vm-reference-test-drops-the-arglist-trailer ()
  "A compiled function's trailing \"(fn ARGS)\" line is not documentation.
The line can hold parentheses of its own when the arglist has keywords, as
`vm-expunge-folder's does: \"(fn &key (QUIET nil) ...)\"."
  (should (string-match-p "(fn " (documentation 'vm-expunge-folder t)))
  (dolist (command '(vm-scroll-forward vm-expunge-folder))
    (should-not (string-match-p "(fn " (vm-reference-command-documentation command))))
  ;; and the documentation itself survives
  (should (string-match-p "[a-z]" (vm-reference-command-documentation 'vm-scroll-forward))))

(ert-deftest vm-reference-test-drops-advice-notes ()
  "Advice on a command is not documentation of it either.
vm-pgg and vm-epg both advise `vm-scroll-forward' as they load, and
`documentation' reports that."
  (require 'vm-epg nil t)
  (dolist (command '(vm-scroll-forward vm-scroll-backward))
    (let ((doc (vm-reference-command-documentation command)))
      (when doc
        (should-not (string-match-p "advice:" doc))))))

;;; Default values

(ert-deftest vm-reference-test-default-values-are-printable ()
  "A default holding control characters is escaped, not written raw.
makeinfo loses the rest of the line when it meets a NUL, and
`vm-mime-encode-words-regexp' has one -- it broke the build the first time."
  (let ((text (with-temp-buffer
                (vm-reference-insert-default 'vm-mime-encode-words-regexp)
                (buffer-string))))
    (should (string-match-p "Default value" text))
    (should-not (string-match-p "[\0-\010\013\014\016-\037\177]" text))))

(ert-deftest vm-reference-test-a-nil-default-says-nothing ()
  "An option defaulting to nil gets no \"Default value\" line, nil being
what the docstring says already."
  (let ((text (with-temp-buffer
                (vm-reference-insert-default 'vm-imap-max-message-size)
                (buffer-string))))
    (should (equal text ""))))

(ert-deftest vm-reference-test-a-default-found-on-the-path-is-not-printed ()
  "A default that searches `exec-path' names the machine, not VM.
`vm-imagemagick-program' came out as /opt/local/lib/ImageMagick7/bin/magick
on one developer's machine and as a miniforge path on another's.  It
survived the first version of this check, which evaluated the default under
two invented environments and compared them: the search comes to nil under
either, and two nils agree.

The entry is the same whether or not the program is installed.  Otherwise
the two machines still differ, one printing a path and the other nothing at
all -- which is what `vm-icontopbm-program' and `vm-uncompface-program' did,
one each way round."
  (dolist (symbol '(vm-imagemagick-program vm-icontopbm-program
                    vm-uncompface-program))
    (dolist (path (list exec-path (list "/nonexistent/bin")))
      (let* ((exec-path path)
             (standard (car (get symbol 'standard-value))))
        (should (vm-reference-environment-dependent-p standard (eval standard t)))
        (should (string-match-p
                 "from this system"
                 (with-temp-buffer (vm-reference-insert-default symbol)
                                   (buffer-string))))))))

(ert-deftest vm-reference-test-a-macro-body-doubles-its-backslashes ()
  "A backslash inside a `@macro' names a parameter, so it has to be doubled.
`vm-mime-encode-words-regexp' defaults to \"[^\\x0-\\x7f]+\", and makeinfo
stopped with \\ followed by `0-' the first time a chapter invoked that macro.
The error comes when the macro is used, not when it is defined, so every
macro nobody has invoked yet is carrying the same fault until this holds."
  (let ((text (with-temp-buffer
                (vm-reference-insert-macro 'vm-mime-encode-words-regexp
                                           #'vm-reference-insert-option)
                (buffer-string))))
    (should (string-match-p "\\\\" text))
    ;; every backslash in the body is part of a doubled pair
    (should-not (string-match-p "\\(\\`\\|[^\\]\\)\\\\\\([^\\]\\|\\'\\)" text))))

;;; Which modules are loaded

(ert-deftest vm-reference-test-a-stray-module-is-not-loaded ()
  "The modules are the ones the build compiles, not what is in the directory.
An old vm-pine.el, deleted from the repository when it became
vm-postpone.el, was still in one working tree; loading it defined the
`vm-pine' group a second time and moved fourteen options into a Pine section
that no other build produced."
  (let ((dir (make-temp-file "vm-reference-test" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Makefile.in" dir)
            (insert "SOURCES = vm.el\n"
                    "SOURCES += vm-postpone.el\n"
                    "SOURCES += u-vm-color.el\n"
                    "OBJECTS = $(SOURCES:.el=.elc)\n"))
          (with-temp-file (expand-file-name "vm-pine.el" dir) (insert ";; stray\n"))
          (should (equal (vm-reference-module-files dir)
                         '("vm.el" "vm-postpone.el"))))
      (delete-directory dir t))))

(ert-deftest vm-reference-test-the-real-module-list-is-found ()
  "VM's own lisp directory yields its modules and none of the generated files."
  (let ((files (vm-reference-module-files
                (expand-file-name "../lisp" vm-test-dir))))
    (should (member "vm-postpone.el" files))
    (should (member "vm-imap.el" files))
    (should-not (member "vm-pine.el" files))
    (should-not (member "vm-autoloads.el" files))))

;;; Collection

(ert-deftest vm-reference-test-collects-commands-with-their-options ()
  "A command joins the options of the same area, in one section.
VM declares every `defcustom' in vm-vars.el, so filing options by the file
that defines them puts all 500 in a single section; they are filed by
customization group instead, which is what Customize shows and what the
manual's own chapters follow."
  (let* ((sections (vm-reference-collect))
         (imap (assoc "IMAP" sections)))
    (should imap)
    (should (memq 'vm-imap-max-message-size (nth 2 imap)))   ; group vm-imap
    (should (memq 'vm-imap-list-folders (nth 1 imap)))       ; file vm-imap.el
    (should-not (memq 'vm-imap-max-message-size (nth 1 imap)))
    ;; no section holds anything like all of the options
    (dolist (section sections)
      (should (< (length (nth 2 section)) 100)))))

(ert-deftest vm-reference-test-skips-obsolete-names ()
  "An obsolete alias is not a thing to document."
  (let ((collected (apply #'append
                          (mapcar (lambda (s) (append (nth 1 s) (nth 2 s)))
                                  (vm-reference-collect)))))
    (dolist (symbol collected)
      (should-not (vm-reference-obsolete-p symbol)))))

;;; The invariant makeinfo cares about

(ert-deftest vm-reference-test-every-converted-docstring-is-balanced ()
  "Converting every docstring VM has leaves balanced braces and @example.
This is the check that would have caught both build failures found while
writing the generator, and it runs over the real corpus rather than over
examples chosen to pass."
  (let ((unbalanced nil))
    (dolist (section (vm-reference-collect))
      (dolist (symbol (append (nth 1 section) (nth 2 section)))
        (let* ((doc (if (eq (vm-reference-kind symbol) 'command)
                        (vm-reference-command-documentation symbol)
                      (documentation-property symbol 'variable-documentation t)))
               (text (and doc (vm-reference-docstring doc))))
          (when text
            (let ((opens (vm-reference-test-count "@example" text))
                  (closes (vm-reference-test-count "@end example" text))
                  (left (vm-reference-test-count "[^@]{" text))
                  (right (vm-reference-test-count "[^@]}" text)))
              (unless (and (= opens closes) (= left right))
                (push (format "%s: @example %d/%d, braces %d/%d"
                              symbol opens closes left right)
                      unbalanced)))))))
    (should (equal nil unbalanced))))

(defun vm-reference-test-count (regexp string)
  "How many times REGEXP matches in STRING."
  (let ((count 0) (start 0))
    (while (and (string-match regexp string start)
                (setq start (1+ (match-beginning 0))))
      (setq count (1+ count)))
    count))

;;; Commands the manual documents, against the autoloads a user's Emacs has

(defconst vm-reference-test--not-commands
  '(;; a value for `vm-url-browser', called once VM is loaded, not typed
    vm-mouse-send-url-to-netscape
    ;; named to show the form of a name the reader is to invent
    vm-mouse-send-url-to-xxx vm-mouse-send-url-to-xxx-new-window)
  "Functions the manual names that no one invokes by name.
Everything else the manual indexes and that is a command has to be reachable
before VM is loaded; see the test below.  The Personality Crisis conditions
and actions are excluded by being functions rather than commands: they are
written into `vm-pcrisis-conditions' and `vm-pcrisis-actions' and run from there.")

(defun vm-reference-test--unautoloaded-commands ()
  "Documented commands that are not reachable from the loaddefs.
Asks a batch Emacs with only lisp/ on its load-path, because this Emacs has
all of VM loaded and so cannot tell an autoloaded command from a loaded one.
That Emacs loads the loaddefs, notes which of the manual's symbols are
defined, then loads every module and reports which of the ones that were
missing turn out to be commands."
  (let* ((lisp (expand-file-name "../lisp" vm-test-dir))
         (manual (expand-file-name "../info/vm.texinfo" vm-test-dir))
         (form `(let ((indexed nil) (missing nil) (commands nil))
                  (with-temp-buffer
                    (insert-file-contents ,manual)
                    (goto-char (point-min))
                    (while (re-search-forward "^@findex +\\([^ \t\n]+\\)" nil t)
                      (push (intern (match-string 1)) indexed))
                    ;; and anything the manual tells the reader to type.  An
                    ;; @findex is how a command is indexed, not how it is
                    ;; documented: `vm-imap-synchronize' was named as the
                    ;; thing to run after working offline, in prose, with no
                    ;; index entry and no autoload cookie.
                    (goto-char (point-min))
                    (while (re-search-forward
                            "M-x[ \t\n]+@?[a-z]*{?\\(vm-[a-z0-9-]+\\)" nil t)
                      (push (intern (match-string 1)) indexed)))
                  (setq indexed (delete-dups indexed))
                  (require 'vm-autoloads)
                  (dolist (symbol indexed)
                    (unless (fboundp symbol) (push symbol missing)))
                  (require 'vm)
                  (dolist (file (directory-files ,lisp nil "\\`vm.*\\.el\\'"))
                    (ignore-errors
                      (require (intern (file-name-sans-extension file)) nil t)))
                  (dolist (symbol missing)
                    (when (commandp symbol) (push symbol commands)))
                  (prin1 (sort commands #'string<)))))
    (with-temp-buffer
      (let ((status (call-process
                     (expand-file-name invocation-name invocation-directory)
                     nil t nil "-batch" "-Q" "-L" lisp
                     "--eval" (prin1-to-string form))))
        (should (equal status 0))
        (goto-char (point-max))
        (backward-sexp)
        (read (current-buffer))))))

(ert-deftest vm-reference-test-documented-commands-are-autoloaded ()
  "Every command the manual documents can be run before VM is loaded.
`M-x' has to find what the manual tells the reader to type, and with only
`(require \\='vm-autoloads)' in an init file -- what INSTALL.md describes --
it finds only what carries an autoload cookie.  Twenty-two documented
commands did not, among them `vm-compact-folder', `vm-recover-folder' and
`vm-toggle-thread', while 455 others did.

The list is of commands, so a function the manual names for a user to put in
an option is not one; `vm-reference-test--not-commands' holds the few that are
neither."
  (should (equal nil
                 (seq-difference (vm-reference-test--unautoloaded-commands)
                                 vm-reference-test--not-commands))))

(defconst vm-reference-test--no-cookie-files
  '("vm-vars.el"                        ; every VM file requires it, and an
                                        ; autoloaded default that reads
                                        ; another variable broke startup
                                        ; (emacs-vm/vm#608)
    "vm-pgg.el")                        ; deprecated in favour of vm-epg
                                        ; (emacs-vm/vm#375): reaching its
                                        ; commands before VM is loaded is not
                                        ; something to make easier
  "Files whose commands are deliberately not autoloaded, and why.")

(ert-deftest vm-reference-test-every-command-a-reader-types-is-autoloaded ()
  "Every command the appendix lists is reachable before VM is loaded.
The other direction from the test above, which asks it of the commands the
manual names by hand.  84 interactive functions were neither autoloaded nor
documented (emacs-vm/vm#715); the ones VM invokes for itself now say so with
`vm-called-by-vm' and are listed apart, and what is left is what a reader
types, which `M-x' has to find.

The exemptions are by file, in `vm-reference-test--no-cookie-files', so a
new command in any other file has to carry a cookie."
  (vm-reference-load-everything)
  (let* ((lisp (expand-file-name "../lisp" vm-test-dir))
         (modules (vm-reference-module-files lisp))
         (missing nil))
    (dolist (section (vm-reference-collect))
      (dolist (command (nth 1 section))
        ;; only what a module defines: a menu `easy-menu-define' builds when
        ;; VM installs its menus is a command whose definition is wherever
        ;; the installing happened, and there is nowhere to put a cookie
        (let ((file (vm-reference-defining-file command)))
          (when (and (member file modules)
                     (not (member file vm-reference-test--no-cookie-files))
                     (not (vm-reference-autoloaded-p command)))
            (push command missing)))))
    (should (equal nil (sort missing #'string<)))))

;;; The manual against the code

(defconst vm-reference-test--manual
  (expand-file-name "../info/vm.texinfo" vm-test-dir)
  "The hand-written manual, which the generated files are included by.")

(defconst vm-reference-test--foreign-symbols
  '(;; other packages, which VM does not require and a batch run has not loaded
    smtpmail-smtp-service smtpmail-stream-type
    w3m-force-redisplay w3m-goto-article-function w3m-pop-up-frames
    ;; named to show the form of a name the reader is to invent
    vm-mouse-send-url-to-xxx vm-mouse-send-url-to-xxx-new-window)
  "Symbols the manual indexes that are deliberately not VM's own.")

(defun vm-reference-test--manual-symbols ()
  "Every symbol the manual indexes with @vindex or @findex."
  (let ((symbols nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      (while (re-search-forward "^@\\(?:vindex\\|findex\\) +\\([^ \t\n]+\\)" nil t)
        (push (intern (match-string 1)) symbols)))
    (delete-dups symbols)))

(ert-deftest vm-reference-test-the-manual-names-things-that-exist ()
  "Every command and variable the manual indexes exists.
The manual sent anyone wanting to revert a folder to `revert-file', which is
not a command in any Emacs, and it went unnoticed because nothing checked.
Deleting or renaming a function is the other way to break this, and half of
vm-rfaddons.el was deleted in one release."
  (vm-reference-load-everything)
  (let ((missing nil))
    (dolist (symbol (vm-reference-test--manual-symbols))
      (unless (or (memq symbol vm-reference-test--foreign-symbols)
                  (boundp symbol) (fboundp symbol) (facep symbol)
                  (get symbol 'variable-documentation))
        (push symbol missing)))
    (should (equal nil (sort missing #'string<)))))

;;; What VM invokes for itself

(defconst vm-reference-test--callback-prefixes
  '("vm-toolbar-" "vm-menu-" "vm-mouse-" "vm-minibuffer-")
  "Name prefixes every one of whose commands VM invokes for itself.
A toolbar handler, a menu entry, a mouse binding, a minibuffer key: none of
them is typed by name, and each is marked `vm-called-by-vm' beside its
definition so that the appendix lists it apart from the commands.")

(defun vm-reference-test--defined-commands (file)
  "Every command FILE defines at top level whose name says VM invokes it."
  (let ((names nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(defun \\(vm-[^ ()\n]+\\)" nil t)
        (let ((symbol (intern (match-string 1))))
          (when (and (commandp symbol)
                     (seq-some (lambda (prefix)
                                 (string-prefix-p prefix (symbol-name symbol)))
                               vm-reference-test--callback-prefixes))
            (push symbol names)))))
    (nreverse names)))

(defun vm-reference-test--unmarked-in (file)
  "The commands FILE defines and does not mark `vm-called-by-vm'."
  (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (seq-remove (lambda (symbol)
                  (string-match-p (regexp-quote
                                   (format "(put '%s 'vm-called-by-vm t)" symbol))
                                  text))
                (vm-reference-test--defined-commands file))))

(ert-deftest vm-reference-test-what-is-marked-called-by-vm-is-a-command ()
  "Nothing carries the mark but a command.
The mark moves an entry out of the appendix\\='s command list, so putting one
on a function that is not `interactive' at all hides nothing and means the
mark is wrong."
  (vm-reference-load-everything)
  (let ((wrong nil))
    (mapatoms (lambda (symbol)
                (when (and (get symbol 'vm-called-by-vm)
                           (not (commandp symbol)))
                  (push symbol wrong))))
    (should (equal nil (sort wrong #'string<)))))

(ert-deftest vm-reference-test-every-callback-by-name-is-marked ()
  "A command named for the toolbar, a menu, the mouse or the minibuffer
carries the mark, beside its own definition.

The appendix listed 431 commands as though a reader might type any of them,
a dozen toolbar handlers among them (emacs-vm/vm#715).  This is what stops
the next handler joining them: a new `vm-toolbar-' command with no
`(put ... \\='vm-called-by-vm t)\\=' after it fails here.  Read from the files
rather than from the running Emacs, since a menu `easy-menu-define' builds
when VM installs its menus is a command that no file defines."
  (vm-reference-load-everything)
  (let ((unmarked nil))
    (let ((lisp (expand-file-name "../lisp" vm-test-dir)))
      (dolist (name (vm-reference-module-files lisp))
        (setq unmarked (append unmarked (vm-reference-test--unmarked-in
                                         (expand-file-name name lisp))))))
    (should (equal nil unmarked))))

(ert-deftest vm-reference-test-a-marked-command-is-classified-as-one ()
  "The generator files a marked command under the callbacks, not the commands.
`vm-reference-kind' is what the appendix asks, and the mark is read from the
code rather than from a list in the generator."
  (vm-reference-load-everything)
  (should (eq (vm-reference-kind 'vm-toolbar-next-command) 'callback))
  (should (eq (vm-reference-kind 'vm-menu-popup-context-menu) 'callback))
  (should (eq (vm-reference-kind 'vm-next-message) 'command)))

(defconst vm-reference-test--documented-by-hand
  '(vm-customize vm-view-manual vm-view-news vm-edit-init-file
    vm-list-mime-part-structure vm-attach-files-in-directory
    vm-delete-postponed-message vm-isearch-presentation)
  "Commands given a manual entry under emacs-vm/vm#715.
The list grows as the rest of that issue is cleared.  It is here so that a
command cannot quietly lose its entry again: 222 of them were undocumented
when it was counted, and nothing had noticed them going.")

(ert-deftest vm-reference-test-commands-documented-by-hand-stay-documented ()
  "Every command in `vm-reference-test--documented-by-hand' is still indexed.
The other direction from `vm-reference-test-documented-commands-are-autoloaded':
that one keeps the manual\\='s commands reachable, this one keeps the reachable
commands in the manual."
  (let ((indexed (vm-reference-test--manual-symbols))
        (missing nil))
    (dolist (command vm-reference-test--documented-by-hand)
      (unless (memq command indexed) (push command missing)))
    (should (equal nil (sort missing #'string<)))))

(ert-deftest vm-reference-test-the-manual-does-not-name-the-unreleased-version ()
  "The manual does not date a change to the version this tree will become.
The number is not settled until the release is made, and dating changes to it
means editing the manual again when it is.  \"In earlier releases\" needs no
upkeep.  Naming a version that has been out for years, as the manual does for
8.2.0, is another matter and is left alone.

The tree has also said two things at once: fifty-one obsolescence markers
naming 8.3.3 while two named 8.4.0, neither of which is the release they
landed in, which is what made this worth pinning."
  (let* ((version (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "../lisp/vm.el" vm-test-dir))
                    (goto-char (point-min))
                    (should (re-search-forward "^;; Version: *\\([0-9.]+\\)" nil t))
                    (match-string 1)))
         (found nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      ;; the version history at the end names every release on purpose
      (let ((end (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "^@unnumberedsubsec Selected Releases" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (while (re-search-forward (regexp-quote version) end t)
          (push (buffer-substring (line-beginning-position) (line-end-position))
                found))))
    (should (equal nil found))))

(ert-deftest vm-reference-test-the-manual-names-the-option-that-is-current ()
  "Where the manual indexes a renamed option, it names the current one too.
Mentioning the old name is right -- someone looking it up needs to find the
explanation -- but the manual must not tell a reader to set a variable that is
only an alias.  It told them to name the ImageMagick programs in
`vm-imagemagick-identify-program' and `vm-imagemagick-convert-program' for a
release after both became aliases of `vm-imagemagick-program'.

The neighbourhood is twelve lines, which is the paragraph that explains the
rename in each of the three places the manual does it."
  (vm-reference-load-everything)
  (let ((orphaned nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      (while (re-search-forward "^@vindex +\\([^ \t\n]+\\)" nil t)
        (let* ((symbol (intern (match-string 1)))
               (obsolete (get symbol 'byte-obsolete-variable))
               (current (car-safe obsolete)))
          (when (and current (symbolp current))
            (let ((from (save-excursion (forward-line -12) (point)))
                  (to (save-excursion (forward-line 12) (point))))
              (unless (save-excursion
                        (goto-char from)
                        (search-forward (symbol-name current) to t))
                (push (list symbol 'should-name current) orphaned)))))))
    (should (equal nil (nreverse orphaned)))))

(ert-deftest vm-reference-test-no-command-is-attributed-to-a-generated-file ()
  "Every command belongs to the file that defines it, not to the loaddefs.
The appendix is built by asking `symbol-file' which file each symbol came
from, and skipping the generated files, whose symbols are defined elsewhere.
An autoloaded `defalias' breaks that: the cookie copies the whole `defalias'
into vm-autoloads.el, so that is where the alias is defined and the command
drops out of the manual.  Four did -- `vm-compact-folder',
`vm-recover-folder', `vm-unread-message' and `vm-headers-summary' -- when
they were autoloaded for emacs-vm/vm#609.  An alias needs the explicit form:

  ;;;###autoload (autoload \\='vm-compact-folder \"vm-delete\" nil t)"
  (vm-reference-load-everything)
  (let ((orphans nil))
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "vm" (symbol-name symbol))
                  (commandp symbol)
                  (not (vm-reference-obsolete-p symbol))
                  (member (vm-reference-defining-file symbol)
                          vm-reference-excluded-files))
         (push symbol orphans))))
    (should (equal nil (sort orphans #'string<)))))

(defun vm-reference-test--keymaps ()
  "Every keymap VM defines, found rather than listed.
A list would go stale: vm-epg-compose-mode-map was the one missing from the
first version of the check below, and the bindings it holds looked wrong."
  (let ((maps nil))
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "vm" (symbol-name symbol))
                  (string-suffix-p "-map" (symbol-name symbol))
                  (boundp symbol)
                  (keymapp (symbol-value symbol)))
         (push (symbol-value symbol) maps))))
    maps))

(defun vm-reference-test--documented-bindings ()
  "The key/command pairs the manual states, as (KEY-STRING . COMMAND).
The manual writes them as `@kbd{s} (@code{vm-save-message})'."
  (let ((pairs nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      (while (re-search-forward
              "@kbd{\\([^}]+\\)}[ \n]*(@code{\\(vm-[^}]+\\)})" nil t)
        ;; @@ is texinfo for a literal @
        (push (cons (replace-regexp-in-string "@@" "@" (match-string 1))
                    (intern (match-string 2)))
              pairs)))
    (delete-dups (nreverse pairs))))

(ert-deftest vm-reference-test-the-manual-states-the-bindings-that-exist ()
  "Every key the manual attributes to a command is bound to it.
The manual tells the reader to type a key and names the command it runs; if
the binding moves or goes, the manual is telling them to type something else.
Half of vm-rfaddons.el was deleted in one release, and with it three
rebindings, which is the way this breaks.

Only the pairs the manual states as a pair are checked: a key mentioned on its
own may belong to Emacs, or to a mode VM knows nothing about."
  (vm-reference-load-everything)
  (let ((maps (vm-reference-test--keymaps))
        (wrong nil))
    (dolist (pair (vm-reference-test--documented-bindings))
      (let ((keys (ignore-errors (kbd (car pair))))
            (found nil))
        (dolist (map maps)
          (when (and keys (eq (lookup-key map keys) (cdr pair)))
            (setq found t)))
        (unless found (push pair wrong))))
    (should (equal nil (nreverse wrong)))))

(ert-deftest vm-reference-test-the-manual-states-the-defaults-that-hold ()
  "Where the manual says what an option defaults to, that is its default.
It said `vm-highlighted-header-face' defaults to \\='bold; the default is the
face `vm-highlighted-header', which inherits bold.  Close enough to go
unnoticed, and wrong enough to send someone looking for a name that is not
there.

The value may be marked up or not -- @samp{nil} and a bare \\='bold both
count, and the first version of this test matched only the marked-up form,
which is not how the wording that was wrong had been written.

The generated appendix prints every default from the code, so prose saying it
again is the only place the two can disagree."
  (vm-reference-load-everything)
  (let ((wrong nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "@code{\\(vm-[a-z0-9-]+\\)}[^.]\\{0,120\\}?"
                      "defaults to[ \n]+\\(?:the face[ \n]+\\)?"
                      "\\(?:@\\(?:samp\\|code\\){\\([^}]+\\)}"
                      "\\|\\('?[a-z][a-z0-9-]*\\)\\)")
              nil t)
        (let* ((symbol (intern (match-string 1)))
               (said (or (match-string 2) (match-string 3)))
               (actual (and (boundp symbol)
                            (format "%S" (default-value symbol)))))
          (unless (or (null actual)
                      (equal said actual)
                      ;; the manual quotes a string without its quotes, and a
                      ;; symbol with or without its tick
                      (equal (format "%S" said) actual)
                      (equal (concat "'" said) actual)
                      (equal said (concat "'" actual))
                      ;; "defaults to the value of ..." is not a value
                      (member said '("the" "whatever" "a" "an" "what")))
            (push (list symbol said actual) wrong)))))
    (should (equal nil (nreverse wrong)))))

;;; The generated appendix does not print Lisp internals

(ert-deftest vm-reference-test-no-internal-argument-names-in-the-manual ()
  "The generated files name no `--cl-…--' argument.

`cl-defun' expands its `&rest' argument to `--cl-rest--', and printing that
in the appendix tells a reader nothing: `vm-compact-folder &rest --cl-rest--'
was in the manual until `vm-reference-argument-name' started stripping it
(emacs-vm/vm#699)."
  (dolist (file '("../info/vm-reference.texinfo" "../info/vm-docstrings.texinfo"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file vm-test-dir))
      (goto-char (point-min))
      (should-not (re-search-forward "--cl-[a-z-]+--" nil t)))))

(ert-deftest vm-reference-test-the-appendix-lists-every-command ()
  "Every command a VM module defines is in the appendix.

222 interactive commands were in no part of the manual (emacs-vm/vm#715), and
what answers that is the generated appendix rather than a chapter entry
written by hand for each: it says what the code says.  So the appendix has to
be complete, and the sweep that fills it asked for a name beginning with vm --
which left `bbdb/vm-set-virtual-folder-alist' and its by-mail-alias twin out
of the manual entirely.

This asks the question the other way round, from the files the build lists
rather than from the names, so a command called anything at all has to be
there.  A command VM invokes for itself counts: those are listed under their
own heading, not among the ones to type."
  (vm-reference-load-everything)
  (let ((modules (vm-reference-modules))
        (listed (make-hash-table :test 'eq))
        (missing nil))
    (dolist (section (vm-reference-collect))
      (dolist (kind '(1 2 3))
        (dolist (symbol (nth kind section))
          (puthash symbol t listed))))
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (not (gethash symbol listed))
                  (not (vm-reference-obsolete-p symbol)))
         (let ((file (vm-reference-defining-file symbol)))
           (when (and file (member file modules))
             (push symbol missing))))))
    (should (equal nil (sort missing #'string<)))))


;;; Every attribute a reader can set is described in the manual

(defconst vm-reference-test--attributes-not-in-the-table
  '(;; not an attribute: the manual says it is what negates both new and unread
    "read"
    ;; alias names, for BABYL and for IMAP, documented as aliases where the
    ;; selectors are: recent is new, unseen is unread, answered is replied
    "recent" "unseen" "answered"
    ;; offered by completion and rejected by `vm-set-message-attribute',
    ;; which warns "Invalid attribute" for both (emacs-vm/vm#827)
    "expanded" "collapsed")
  "Names in `vm-supported-attribute-names' that the table need not carry.")

(defun vm-reference-test--attributes-in-the-manual ()
  "The attribute names the Message Attributes table of the manual describes."
  (with-temp-buffer
    (insert-file-contents vm-reference-test--manual)
    (goto-char (point-min))
    (should (re-search-forward "^@node Message Attributes," nil t))
    (should (re-search-forward "^@table @code" nil t))
    (let ((end (save-excursion (re-search-forward "^@end table") (point)))
          (names nil))
      (while (re-search-forward "^@item \\([a-z]+\\)$" end t)
        (push (match-string 1) names))
      names)))

(ert-deftest vm-reference-test-every-settable-attribute-is-described ()
  "REGRESSION: an attribute a reader can set is described in the manual.

`flagged' was not.  It is stored in the folder like the rest, `!' toggles it,
the summary shows it as `!', a virtual folder can select on it and an IMAP
server keeps it as \\Flagged, and the one place the manual explains what an
attribute means did not mention it.  So the `!' in a summary line could not
be looked up, which is what the maintainer noticed (emacs-vm/vm#776)."
  (let ((described (vm-reference-test--attributes-in-the-manual))
        (missing nil))
    (dolist (name vm-supported-attribute-names)
      (unless (or (string-prefix-p "un" name)
                  (member name vm-reference-test--attributes-not-in-the-table)
                  (member name described))
        (push name missing)))
    (should-not missing)))

;;; The generated files do not depend on the machine that generated them

(ert-deftest vm-reference-test-the-generated-texinfo-ignores-the-locale ()
  "REGRESSION: the quoting style of the build machine changes nothing.

`text-quoting-style' defaults to asking whether curved quotes can be
displayed, so the answer depends on the locale of whoever runs the build.
`substitute-command-keys' then returns grave quotes, which
`vm-reference-mark-up-quotes' does not match, and 2480 lines of each
generated file come out with `symbol' where @code{symbol} belongs.  Both
files are committed, so a reader in a C locale had a dirty working tree
after every build and nothing they could do about it (emacs-vm/vm#830).

Includes nil, which is the value that asks the terminal and so the one that
was doing the damage."
  (dolist (style '(curve grave nil))
    (let ((text-quoting-style style))
      (should (equal "See @code{vm-quit} now."
                     (vm-reference-prose "See `vm-quit' now.")))
      (should (equal "@code{vm-quit} and @code{vm-save-folder}."
                     (vm-reference-prose "`vm-quit' and `vm-save-folder'."))))))

(provide 'vm-reference-test)

;;; vm-reference-test.el ends here

(defun vm-reference-test--manual-nodes ()
  "Every node the manual defines, as a string."
  (let ((nodes nil))
    (with-temp-buffer
      (insert-file-contents vm-reference-test--manual)
      (goto-char (point-min))
      (while (re-search-forward "^@node +\\([^,\n]+\\)" nil t)
        (push (string-trim (match-string 1)) nodes)))
    nodes))

(defun vm-reference-test--nodes-the-lisp-names ()
  "Every manual section a string in `lisp/' sends the reader to.
The forms recognised are \"NAME in the VM manual\" and \"the VM manual
section \\\"NAME\\\"\", which is how VM's messages and docstrings write one."
  (let ((named nil)
        ;; nil, or `[A-Z]' matches the "s" of "see" and the prefix is taken
        ;; for part of the name.
        (case-fold-search nil))
    ;; "vm.*" and not "vm-.*": vm.el itself was skipped, and it is where the
    ;; configuration checker's messages are.
    (dolist (file (directory-files (expand-file-name "../lisp" vm-test-dir)
                                   t "\\`vm.*\\.el\\'"))
      (unless (string-match-p "vm-\\(autoloads\\|cus-load\\)\\.el\\'" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\(?:the node \\|[Ss]ee \\)?"
                          "\\([A-Z][A-Za-z/ ]+?\\) in the VM manual")
                  nil t)
            (push (cons (string-trim (match-string 1)) file) named))
          (goto-char (point-min))
          (while (re-search-forward
                  "VM manual section .\"\\([^\"\\\\]+\\)" nil t)
            (push (cons (match-string 1) file) named)))))
    named))

(ert-deftest vm-reference-test-a-message-that-names-the-manual-names-a-node ()
  "REGRESSION: a section a message sends the reader to is one the manual has.

The Bcc question and the error behind it both said \"Mail Sending Options\",
which was the section's printed heading and never its node name, so `g' in
Info found nothing and the reader was left where they started
(emacs-vm/vm#815).  A message that points nowhere is worse than one that
does not point, because it costs the reader the search as well."
  (let ((nodes (vm-reference-test--manual-nodes))
        (missing nil))
    (dolist (named (vm-reference-test--nodes-the-lisp-names))
      (unless (member (car named) nodes)
        (push (format "%s names %S, which is no node in the manual"
                      (file-name-nondirectory (cdr named)) (car named))
              missing)))
    (should-not missing)))
