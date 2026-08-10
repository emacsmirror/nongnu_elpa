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
`vm-expunge-folder\='s does: \"(fn &key (QUIET nil) ...)\"."
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
VM declares every `defcustom\=' in vm-vars.el, so filing options by the file
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
    ;; named to show the shape of a name the reader is to invent
    vm-mouse-send-url-to-xxx vm-mouse-send-url-to-xxx-new-window)
  "Functions the manual names that no one invokes by name.
Everything else the manual indexes and that is a command has to be reachable
before VM is loaded; see the test below.  The Personality Crisis conditions
and actions are excluded by being functions rather than commands: they are
written into `vmpc-conditions' and `vmpc-actions' and run from there.")

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

;;; The manual against the code

(defconst vm-reference-test--manual
  (expand-file-name "../info/vm.texinfo" vm-test-dir)
  "The hand-written manual, which the generated files are included by.")

(defconst vm-reference-test--foreign-symbols
  '(;; other packages, which VM does not require and a batch run has not loaded
    smtpmail-smtp-service smtpmail-stream-type
    w3m-force-redisplay w3m-goto-article-function w3m-pop-up-frames
    ;; named to show the shape of a name the reader is to invent
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

(ert-deftest vm-reference-test-the-manual-does-not-name-the-unreleased-version ()
  "The manual does not date a change to the version this tree will become.
The number is not settled until the release is made, and dating changes to it
means editing the manual again when it is.  \"In earlier releases\" needs no
upkeep.  Naming a version that has been out for years, as the manual does for
8.2.0, is another matter and is left alone.

The tree has also said two things at once: 8.3.3 in one obsolescence marker
and 8.4.0 in three others, which is what made this worth pinning."
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

(provide 'vm-reference-test)

;;; vm-reference-test.el ends here
