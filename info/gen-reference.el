;;; gen-reference.el --- generate VM's reference appendix from the code  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Writes vm-reference.texinfo: one entry per VM command and user option,
;; taken from the docstring in the code, so that the manual does not carry a
;; second copy of what the code already says.  Run from info/Makefile:
;;
;;   emacs -batch -q -no-site-file -L ../lisp -l gen-reference.el \
;;         -f vm-reference-batch vm-reference.texinfo
;;
;; The result is @include'd by vm.texinfo and is committed, so that a build
;; from a release tarball or from ELPA needs no Emacs of its own to run.

;;; Code:

(require 'cl-lib)
(require 'help-fns)
(require 'pp)

(defvar vm-reference-area-titles
  '(("vm"              . "VM itself")
    ("vm-imap"         . "IMAP")
    ("vm-pop"          . "POP")
    ("vm-mime"         . "MIME and attachments")
    ("vm-summary"      . "The summary")
    ("vm-summary-faces" . "Summary faces")
    ("vm-virtual"      . "Virtual folders")
    ("vm-avirtual"     . "Virtual folder actions")
    ("vm-biff"         . "New mail notification")
    ("vm-serial"       . "Serial mail")
    ("vm-postpone"     . "Postponed compositions")
    ("vm-pcrisis"      . "Personality Crisis")
    ("vmpc"            . "Personality Crisis")
    ("vm-rfaddons"     . "Additions")
    ("vm-grepmail"     . "Searching with grepmail")
    ("vm-ps-print"     . "Printing with ps-print")
    ("vm-epg"          . "Encryption with EasyPG")
    ("vm-smime"        . "S/MIME")
    ("vm-menu"         . "Menus")
    ("vm-toolbar"      . "The toolbar")
    ("vm-thread"       . "Threading")
    ("vm-sort"         . "Sorting")
    ("vm-save"         . "Saving and forwarding")
    ("vm-reply"        . "Replying and composing")
    ("vm-compose"      . "Replying and composing")
    ("vm-edit"         . "Editing messages")
    ("vm-delete"       . "Deleting and expunging")
    ("vm-folder"       . "Folders")
    ("vm-folders"      . "Folders")
    ("vm-mark"         . "Marks")
    ("vm-motion"       . "Moving about")
    ("vm-page"         . "Reading a message")
    ("vm-presentation" . "Reading a message")
    ("vm-search"       . "Searching")
    ("vm-undo"         . "Undo")
    ("vm-window"       . "Windows and frames")
    ("vm-frames"       . "Windows and frames")
    ("vm-crypto"       . "Cryptography")
    ("vm-digest"       . "Digests")
    ("vm-print"        . "Printing")
    ("vm-startup"      . "Starting VM")
    ("vm-vars"         . "Other settings")
    ("vm-misc"         . "Other settings"))
  "Titles for the reference sections, by area.
An area is a source file without its extension, or a customization group,
so that the commands defined in vm-imap.el and the options in the
@code{vm-imap} group land in one section.  An area with no entry here is
titled after itself.")

(defvar vm-reference-excluded-files
  '("vm-autoloads.el" "vm-cus-load.el" "vm-version-conf.el")
  "Generated files, whose symbols are defined elsewhere.")

;;; Loading VM

(defun vm-reference-module-files (dir)
  "The VM modules DIR/Makefile.in lists, as file names.
The list comes from the build rather than from what DIR happens to hold, so
that a file left lying around beside the sources is not loaded: an old
vm-pine.el, deleted from the repository when it became vm-postpone.el but
still in one working tree, defined the `vm-pine' customization group a
second time and moved fourteen options into a Pine section that no other
build produced."
  (let ((file (expand-file-name "Makefile.in" dir))
        (files nil))
    (unless (file-readable-p file)
      (error "No %s: it is where the list of VM modules is read from" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^SOURCES *\\+?= *\\(vm.*\\.el\\)[ \t]*$" nil t)
        (push (match-string 1) files)))
    (unless files
      (error "No SOURCES lines in %s: has the build changed?" file))
    (nreverse files)))

(defun vm-reference-load-everything ()
  "Load every VM module from source, so that every symbol is defined.

From source because a compiled function does not carry the argument names
its definition was written with: `vm-reorder-message-headers' comes back
as \"arg1 &rest rest\" from the .elc and \"message &rest --cl-rest--\"
from the .el.  Which of those the manual says must not depend on whether
anyone ran `make' first (issue #699).

`require' is shadowed rather than `load-suffixes' narrowed: narrowing it
sends Emacs looking for a cl-macs.el that an installed Emacs does not ship.
Loading each file twice does not work either -- a second load replaces the
file\\='s `load-history' entry, and a definition guarded by `fboundp' is not
made again, so `symbol-file' forgets it.  That is what dropped the
vm-toolbar-*-command aliases out of the manual while this was being
written."
  (let* ((dir (or (locate-library "vm-vars")
                  (error "VM is not on `load-path'; pass -L path/to/lisp")))
         (standard-require (symbol-function 'require)))
    (setq dir (file-name-directory dir))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (let ((source (expand-file-name
                                (concat (symbol-name feature) ".el") dir)))
                   (if (and (not (featurep feature))
                            (file-readable-p source))
                       ;; nosuffix: the name is the file, not a stem
                       (load source noerror t t)
                     (funcall standard-require feature filename noerror))))))
      (require 'vm-vars)
      (require 'vm)
      (dolist (file (vm-reference-module-files dir))
        (unless (member file vm-reference-excluded-files)
          (let ((feature (intern (file-name-sans-extension file))))
            (condition-case err
                (require feature nil t)
              (error (message "gen-reference: %s: %s" file
                              (error-message-string err))))))))))

;;; Docstrings to texinfo

(defun vm-reference-resolve-keys (string)
  "Expand `\\\\[command]' and `\\\\{keymap}' in STRING against VM's own keymap.
`substitute-command-keys' reads the keymaps of the current buffer, and in a
batch Emacs that is not a VM buffer, so `\\\\[vm-scroll-forward]' would come
out as \"M-x vm-scroll-forward\" instead of the key it is bound to.

`text-quoting-style' is bound because its default asks the terminal whether
it can show curved quotes, so the answer depends on the locale of whoever
runs the build.  In a C locale `substitute-command-keys' returns grave
quotes, `vm-reference-mark-up-quotes' matches none of them, and 2480 lines
of each generated file come out differently: every `symbol' left as it was
rather than turned into @code.  Both files are committed, so that is a dirty
working tree on every build for that reader (emacs-vm/vm#830)."
  (with-temp-buffer
    (when (boundp 'vm-mode-map)
      (use-local-map vm-mode-map))
    (let ((text-quoting-style 'curve))
      (substitute-command-keys string))))

(defun vm-reference-command-documentation (symbol)
  "The docstring SYMBOL was written with, without what Emacs adds to it.
Advice on a command puts \"This function has :around advice: ...\" in what
`documentation' returns, as vm-epg's on `vm-scroll-forward' does, and a
compiled function carries a trailing \"(fn ARGS)\" line.  Neither belongs in
a manual."
  (let* ((function (and (fboundp symbol) (indirect-function symbol)))
         (unadvised (if (and function (fboundp 'advice--cd*r))
                        (advice--cd*r function)
                      function))
         (doc (and unadvised (documentation unadvised t))))
    (when doc
      ;; The fallback, for an Emacs where `advice--cd*r' has gone.
      (setq doc (replace-regexp-in-string
                 "\n+This function \\(?:has\\|is\\) [^\n]*advice[^\n]*\n?" "\n" doc))
      (string-trim-right
       ;; The line can hold parentheses of its own, as a `cl-defun' with
       ;; keyword arguments does: "(fn &key (QUIET nil) ...)".
       (replace-regexp-in-string "\n+(fn\\(?: [^\n]*\\)?)\\'" "" doc)))))

(defun vm-reference-escape (string)
  "Quote the three characters texinfo reserves."
  (replace-regexp-in-string "[@{}]" "@\\&" string))

(defun vm-reference-mark-up-quotes (string)
  "Turn the curved quotes `substitute-command-keys' produced into @code."
  (replace-regexp-in-string "‘\\([^’\n]*\\)’"
                            (lambda (m)
                              (let ((inner (substring m 1 -1)))
                                (if (string-empty-p inner)
                                    "@samp{}"
                                  (concat "@code{" inner "}"))))
                            string t t))

(defun vm-reference-indented-p (line)
  (and line (string-match-p "\\`\\(?:[ \t][ \t]+\\|\t\\)[^ \t]" line)))

(defun vm-reference-example-run-p (lines)
  "Whether LINES starts a run of at least two indented lines.
One indented line on its own is how many VM docstrings begin a paragraph,
and a paragraph put in @example comes out unwrapped and out of place."
  (and (vm-reference-indented-p (car lines))
       (vm-reference-indented-p (cadr lines))))

(defun vm-reference-example-blocks (string)
  "Wrap runs of indented lines in STRING in @example, marking up the rest.
Two reasons the two jobs are one function.  Texinfo reflows ordinary text,
which would run the lines of an example together.  And @code{x} inside an
@example prints x with no quotes at all, where outside one it prints ‘x’ --
so marking up an example's quotes takes two characters off every line that
has one, and a docstring whose columns line up around ‘...' stops lining
up.  Inside an example the line is left exactly as the docstring wrote it."
  (let ((lines (split-string string "\n"))
        (out nil) (in-example nil))
    (while lines
      (let ((line (car lines)))
        (cond ((and (not in-example) (vm-reference-example-run-p lines))
               (setq in-example t)
               (push "@example" out))
              ((and in-example (not (vm-reference-indented-p line))
                    (not (string-match-p "\\`[ \t]*\\'" line)))
               (setq in-example nil)
               (push "@end example" out)))
        (push (if in-example line (vm-reference-mark-up-quotes line)) out)
        (setq lines (cdr lines))))
    (when in-example (push "@end example" out))
    (mapconcat #'identity (nreverse out) "\n")))

(defun vm-reference-prose (string)
  "Convert a run of docstring prose, holding no keymap, to texinfo."
  (vm-reference-example-blocks
   (vm-reference-escape (vm-reference-resolve-keys string))))

(defun vm-reference-untabify (string)
  "Replace the tabs in STRING with spaces to the next eight-column stop.
`substitute-command-keys' separates a key from its binding with tabs, so
the table lines up only in a reader whose tab stops are eight columns
apart.  Spaces line it up in any of them."
  (with-temp-buffer
    (insert string)
    (untabify (point-min) (point-max))
    (buffer-string)))

(defun vm-reference-keymap-block (form)
  "Convert FORM, a `\\{MAP}' from a docstring, to an @example of its bindings.
`substitute-command-keys' lays a keymap out in columns and starts those
lines at the left margin, so the run-of-indented-lines rule does not see
them and texinfo reflowed the whole table into one paragraph."
  (concat "@example\n"
          (vm-reference-escape
           (vm-reference-untabify
            (string-trim (vm-reference-resolve-keys form))))
          "\n@end example\n"))

(defun vm-reference-docstring (string)
  "Convert docstring STRING to texinfo."
  ;; The old convention marked a user option with a leading asterisk.
  (let ((text (replace-regexp-in-string "\\`\\*" "" (or string "")))
        (out nil)
        (start 0))
    (while (string-match "\\\\{[^}\n]+}" text start)
      ;; Read the match out before converting anything: the conversions
      ;; search strings of their own and leave the match data theirs, so
      ;; a later `match-end' would not be this match's and `start' would
      ;; not advance.
      (let ((from (match-beginning 0))
            (to (match-end 0)))
        (push (vm-reference-prose (substring text start from)) out)
        (push (vm-reference-keymap-block (substring text from to)) out)
        (setq start to)))
    (push (vm-reference-prose (substring text start)) out)
    (string-trim (mapconcat #'identity (nreverse out) ""))))

;;; Collecting

(defun vm-reference-defining-file (symbol)
  "The base name of the file SYMBOL was defined in, or nil."
  (let ((file (or (symbol-file symbol 'defvar)
                  (symbol-file symbol))))
    (and file (file-name-nondirectory
               (replace-regexp-in-string "\\.elc\\'" ".el" file)))))

(defun vm-reference-obsolete-p (symbol)
  (or (get symbol 'byte-obsolete-info)
      (get symbol 'byte-obsolete-variable)
      (get symbol 'obsolete-name)))

(defun vm-reference-area-title (area)
  "The section title for AREA, a file base name or a customization group."
  (or (cdr (assoc area vm-reference-area-titles))
      (let ((name (replace-regexp-in-string "\\`vm-?" "" area)))
        (if (string-empty-p name)
            "VM itself"
          (capitalize (replace-regexp-in-string "-" " " name))))))

(defun vm-reference-option-areas ()
  "Map each user option to the customization group that lists it.
An option in more than one group is filed under the first by name, so that
the appendix does not move about between builds."
  (let ((map (make-hash-table :test 'eq))
        (groups nil))
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "vm" (symbol-name symbol))
                  (get symbol 'custom-group))
         (push symbol groups))))
    (dolist (group (sort groups #'string<))
      (dolist (member (get group 'custom-group))
        (unless (gethash (car member) map)
          (puthash (car member) group map))))
    map))

(defun vm-reference-area (symbol kind option-areas)
  "The area SYMBOL belongs to, or nil if it is not VM's to document.
A command belongs to the file that defines it, a user option to its
customization group -- VM declares every `defcustom' in vm-vars.el, so the
defining file would put all of them in one section."
  (let ((group (and (eq kind 'option) (gethash symbol option-areas)))
        (file (vm-reference-defining-file symbol)))
    (cond (group (symbol-name group))
          ((and file
                (string-prefix-p "vm" file)
                (not (member file vm-reference-excluded-files)))
           (file-name-sans-extension file)))))

(defun vm-reference-callback-p (symbol)
  "Whether VM invokes SYMBOL for itself rather than a reader typing it.
A toolbar button, a menu entry, a mouse binding, a keymap entry, a mode of
one of VM\\='s own buffers, or a function written into a hook or an option.
Such a function is `interactive' because something has to be able to invoke
it, and the appendix lists it under its own heading rather than among the
commands, so that a reader looking for something to type is not offered a
dozen toolbar handlers (emacs-vm/vm#715).  Nothing stops a reader calling
one.

The mark is `vm-called-by-vm', put beside the definition in lisp/ rather
than kept in a list here: a list away from the code is a list that goes out
of date, and the definition is where whoever adds a toolbar handler is
looking."
  (get symbol 'vm-called-by-vm))

(defun vm-reference-kind (symbol)
  (cond ((vm-reference-callback-p symbol) 'callback)
        ((commandp symbol) 'command)
        ((custom-variable-p symbol) 'option)))

(defun vm-reference-ours-p (symbol)
  "Whether SYMBOL is VM\\='s to document.
Almost every one of VM\\='s names starts with vm, and asking only that left the
two BBDB commands in vm-avirtual.el out of the appendix altogether:
`bbdb/vm-set-virtual-folder-alist' and its by-mail-alias twin
(emacs-vm/vm#715).  So a name that merely mentions vm is asked the real
question, which is where the definition is.

The name is asked first because `symbol-file' is not free and the answer is
no for every command in Emacs, and because it is asked at all: vm-vars.el
carries `(defvar pop-up-frames nil)\\=' so the compiler knows the name, which
puts that file last in the variable\\='s `load-history' entry and would file an
Emacs option in VM\\='s appendix."
  (let ((name (symbol-name symbol)))
    (or (string-prefix-p "vm" name)
        (and (string-match-p "vm" name)
             (let ((file (vm-reference-defining-file symbol)))
               (and file (member file (vm-reference-modules))))))))

(defun vm-reference-classify (symbol option-areas by-title)
  "File SYMBOL under its section title in BY-TITLE, if it belongs there.
What kind of thing SYMBOL is comes first because it is the cheap question:
`vm-reference-ours-p' reads `symbol-file' for a name that is not vm-
prefixed, and asking that of every atom in Emacs made the sweep 0.07s into
3.1s.  Of the tens of thousands of atoms only a few thousand are commands or
user options at all."
  (unless (vm-reference-obsolete-p symbol)
    (let ((kind (vm-reference-kind symbol)))
      (when (and kind (vm-reference-ours-p symbol))
        (let ((area (vm-reference-area symbol kind option-areas)))
          (when area
            (let ((title (vm-reference-area-title area)))
              (push (cons kind symbol) (gethash title by-title)))))))))

(defun vm-reference-of-kind (entries kind)
  (sort (mapcar #'cdr (cl-remove-if-not (lambda (e) (eq (car e) kind)) entries))
        #'string<))

(defvar vm-reference-modules-cache nil
  "The module file names, read once: `vm-reference-ours-p' asks per symbol.")

(defun vm-reference-modules ()
  "The VM modules the build lists, as file names."
  (or vm-reference-modules-cache
      (setq vm-reference-modules-cache
            (vm-reference-module-files
             (file-name-directory (or (locate-library "vm-vars")
                                      (error "VM is not on `load-path'")))))))

(defun vm-reference-collect ()
  "Return an alist of (TITLE COMMANDS OPTIONS CALLBACKS), sorted by title."
  (let ((by-title (make-hash-table :test 'equal))
        (option-areas (vm-reference-option-areas))
        (sections nil))
    (mapatoms (lambda (symbol)
                (vm-reference-classify symbol option-areas by-title)))
    (maphash (lambda (title entries)
               (push (list title
                           (vm-reference-of-kind entries 'command)
                           (vm-reference-of-kind entries 'option)
                           (vm-reference-of-kind entries 'callback))
                     sections))
             by-title)
    (sort sections (lambda (a b) (string< (car a) (car b))))))

;;; Emitting

(defun vm-reference-node-name (title)
  (concat "Reference for " title))

(defun vm-reference-argument-name (argument)
  "ARGUMENT as the manual should print it.
`cl-defun' expands its `&rest\\=' argument to the internal name
`--cl-rest--', which says nothing to a reader of the manual."
  (let ((name (format "%s" argument)))
    (if (string-match "\\`--cl-\\(.+\\)--\\'" name)
        (match-string 1 name)
      name)))

(defvar vm-reference-autoloaded nil
  "Every command `lisp/vm-autoloads.el\\=' autoloads, or nil before it is read.")

(defun vm-reference-autoloaded-p (symbol)
  "Whether SYMBOL is autoloaded, according to the generated loaddefs.
Read from the file rather than asked of the symbol: by the time the appendix
is generated the whole of VM is loaded, so every command looks available and
nothing would be marked.  Derived rather than listed, so that autoloading a
command is all it takes for the appendix to stop saying otherwise."
  (unless vm-reference-autoloaded
    (setq vm-reference-autoloaded (make-hash-table :test 'eq))
    (let ((file (locate-library "vm-autoloads.el")))
      (when file
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "(autoload '\\([^ )]+\\)" nil t)
            (puthash (intern (match-string 1)) t vm-reference-autoloaded))))))
  (gethash symbol vm-reference-autoloaded))

(defun vm-reference-insert-command (symbol)
  (let ((args (help-function-arglist symbol t))
        (doc (vm-reference-command-documentation symbol)))
    (insert (format "@deffn Command %s%s\n" symbol
                    (if args
                        (concat " " (mapconcat #'vm-reference-argument-name
                                               args " "))
                      "")))
    (insert (format "@findex %s\n" symbol))
    (insert (if doc
                (concat (vm-reference-docstring doc) "\n")
              "Not documented.\n"))
    (unless (vm-reference-autoloaded-p symbol)
      (insert "\n@emph{Not autoloaded}: VM has to be loaded before"
              " @kbd{M-x} offers this one.\n"))
    (insert "@end deffn\n\n")))

(defun vm-reference-escape-controls (string)
  "Escape the control characters of STRING that makeinfo cannot take.
A NUL and a DEL live in `vm-mime-encode-words-regexp', and makeinfo loses
the rest of the line when it meets one.  Newline and tab are left alone:
inside an @example they are what breaks a long default over lines, and
`print-escape-control-characters' escapes the newlines along with the rest,
which is what left one default 1642 characters wide."
  (replace-regexp-in-string "[^\n\t[:print:]]"
                            (lambda (c) (format "\\\\%o" (aref c 0)))
                            string t t))

(defun vm-reference-print-value (value one-line)
  "Print VALUE for the manual, on one line if ONE-LINE, else broken up.
`prin1-to-string' puts a whole alist on one line, and some of VM's defaults
are long enough to leave the reader scrolling sideways -- `vm-serial-cookies'
runs to nearly three thousand characters.  `pp' breaks those at their
structure, and a string keeps the newlines it was written with."
  (if one-line
      (let ((print-escape-control-characters t)
            (print-escape-newlines t))
        (prin1-to-string value))
    ;; Print first and let `pp-buffer' lay the text out, rather than
    ;; `pp-to-string', which binds `print-escape-newlines' itself and so
    ;; puts a multi-line string back on one line.
    (vm-reference-escape-controls
     (string-trim-right
      (with-temp-buffer
        (let ((print-escape-newlines nil)
              (print-escape-control-characters nil))
          (prin1 value (current-buffer)))
        (pp-buffer)
        (buffer-string))))))

(defun vm-reference-eval-elsewhere (form tag)
  "Evaluate FORM as if on a machine identified by TAG.
Everything a default is likely to read about its surroundings is given a
value derived from TAG, so that two different tags agree only for a default
that reads none of them.

The two searches for a program answer with a path made from TAG rather than
with nothing.  A search of an invented `exec-path' finds nothing, and a
default whose program is not installed here finds nothing either, so the two
agree and the default looks fixed -- while on a machine where the program is
installed the same default prints a path.  That is `vm-icontopbm-program',
found here and not on the machine that reported this, and
`vm-uncompface-program', the other way round."
  (condition-case nil
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &rest _) (format "/nonexistent-%s/bin/%s" tag name)))
                ((symbol-function 'vm-locate-executable-file)
                 (lambda (name) (format "/nonexistent-%s/bin/%s" tag name))))
        (let ((process-environment
             (append (list (format "HOME=/nonexistent-%s" tag)
                           (format "TMPDIR=/nonexistent-%s/tmp" tag)
                           (format "USER=nobody-%s" tag)
                           (format "LOGNAME=nobody-%s" tag)
                           (format "PATH=/nonexistent-%s/bin" tag))
                     process-environment))
              (exec-path (list (format "/nonexistent-%s/bin" tag)))
              (user-mail-address (format "nobody-%s@example.invalid" tag))
              (user-full-name (format "Nobody %s" tag))
              (system-configuration (format "none-none-%s" tag))
              (temporary-file-directory (format "/nonexistent-%s/tmp/" tag)))
          (eval form t)))
    (error (list :vm-reference-error tag))))

(defun vm-reference-environment-dependent-p (form value)
  "Whether FORM's VALUE follows the machine the manual is built on.
Decided by evaluating FORM a second time with the environment it might read
changed: a default that comes out different is one that would put this
machine's home or temporary directory into the manual, so that the generated
file differs for every developer who builds it and `check-reference' fails
for all but the last one.

Asked rather than guessed from a list of names, so a default added later
that reads the environment is caught without anyone remembering to add it.

Three values are compared: the two invented ones and VALUE, which is what
FORM came to here.  Both comparisons are needed.  Two invented environments
catch a default whose value happens to equal what one invented environment
produces, which is not hypothetical: it hid one here.  VALUE catches a
default that searches `exec-path' for a program, since that search comes to
nil under either invented environment, and two nils agree."
  (let ((one (vm-reference-eval-elsewhere form "one"))
        (two (vm-reference-eval-elsewhere form "two")))
    (not (and (equal one two) (equal one value)))))

(defun vm-reference-insert-default (symbol)
  "Insert the default value of SYMBOL, unless it has none worth printing."
  (let* ((standard (car (get symbol 'standard-value)))
         (value (and standard (ignore-errors (eval standard t))))
         (short (and value (vm-reference-print-value value t))))
    (cond
     ;; Asked before the nil case: a search of `exec-path' finds the program
     ;; on one machine and nothing on another, and saying nothing there would
     ;; leave the two builds writing different files again.
     ((and standard (vm-reference-environment-dependent-p standard value))
      (insert "\nDefault value: worked out when VM is loaded, "
              "from this system.\n"))
     ((null short))
     ((<= (length short) 60)
      (insert "\nDefault value: @code{" (vm-reference-escape short) "}\n"))
     (t
      (insert "\nDefault value:\n@example\n"
              (vm-reference-escape (vm-reference-print-value value nil))
              "\n@end example\n")))))

(defun vm-reference-insert-option (symbol)
  (let ((doc (documentation-property symbol 'variable-documentation t)))
    (insert (format "@defopt %s\n" symbol))
    (insert (format "@vindex %s\n" symbol))
    (insert (if doc
                (concat (vm-reference-docstring doc) "\n")
              "Not documented.\n"))
    (vm-reference-insert-default symbol)
    (insert "@end defopt\n\n")))

(defun vm-reference-insert-menu (sections)
  (insert "@menu\n")
  (dolist (section sections)
    (insert (format "* %s::\n" (vm-reference-node-name (car section)))))
  (insert "@end menu\n\n"))

(defun vm-reference-insert-section (section previous next)
  (let* ((title (car section))
         (commands (nth 1 section))
         (options (nth 2 section))
         (callbacks (nth 3 section)))
    (insert (format "@node %s, %s, %s, Reference\n"
                    (vm-reference-node-name title)
                    (if next (vm-reference-node-name next) "")
                    (if previous (vm-reference-node-name previous) "Reference")))
    (insert (format "@appendixsec %s\n" title))
    (insert (format "@cindex %s\n\n" title))
    (when commands
      (insert "@appendixsubsec Commands\n\n")
      (mapc #'vm-reference-insert-command commands))
    (when options
      (insert "@appendixsubsec User options\n\n")
      (mapc #'vm-reference-insert-option options))
    (when callbacks
      (insert "@appendixsubsec Called by VM\n\n")
      (insert "These are invoked by a toolbar button, a menu entry, a mouse\n"
              "binding, a keymap entry or a hook rather than typed by name.\n"
              "Nothing stops you calling one.\n\n")
      (mapc #'vm-reference-insert-command callbacks))))

(defun vm-reference-insert-sections (sections)
  "Insert every section in SECTIONS, chaining the nodes together."
  (let* ((files (mapcar #'car sections))
         (count (length files)))
    (dotimes (i count)
      (vm-reference-insert-section
       (nth i sections)
       (and (> i 0) (nth (1- i) files))
       (and (< (1+ i) count) (nth (1+ i) files))))))

(defun vm-reference-generate (file)
  "Write the reference appendix to FILE."
  (vm-reference-load-everything)
  (let ((sections (vm-reference-collect))
        (commands 0) (options 0) (callbacks 0))
    (with-temp-buffer
      (insert "@c This file is generated by info/gen-reference.el -- do not edit.\n")
      (insert "@c Every entry here comes from a docstring in lisp/.\n\n")
      (insert "@node Reference, Concept Index, Internals, Top\n")
      (insert "@appendix Command and Variable Reference\n")
      (insert "@cindex reference\n\n")
      (insert "Every VM command and user option, as the code describes it.\n"
              "This appendix is generated from the docstrings when the manual is\n"
              "built, so it says what the code says; the chapters above explain\n"
              "what to do with it.\n\n"
              "Each section lists what you can type, then the options that\n"
              "govern it, then what VM invokes for itself: a toolbar button, a\n"
              "menu entry, a mouse binding, a keymap entry or a hook function.\n"
              "The last are here because they can be called, not because a\n"
              "reader normally would, and which ones they are is marked in the\n"
              "code beside each definition.\n\n"
              "An entry marked @emph{not autoloaded} needs VM loaded before\n"
              "@kbd{M-x} will offer it, which for a command that only makes\n"
              "sense inside a folder is no hardship.  The mark is worked out\n"
              "when the manual is built, so it says what is so rather than\n"
              "what was so.\n\n")
      (vm-reference-insert-menu sections)
      (vm-reference-insert-sections sections)
      (dolist (section sections)
        (setq commands (+ commands (length (nth 1 section)))
              options (+ options (length (nth 2 section)))
              callbacks (+ callbacks (length (nth 3 section)))))
      (write-region (point-min) (point-max) file nil 'quiet))
    (message (concat "gen-reference: %d commands, %d user options,"
                     " %d called by VM, %d sections -> %s")
             commands options callbacks (length sections) file)))

(defun vm-reference-batch ()
  "Write the reference appendix to the file named on the command line."
  (let ((file (or (car command-line-args-left)
                  (error "Usage: -f vm-reference-batch OUTPUT.texinfo"))))
    (setq command-line-args-left (cdr command-line-args-left))
    (vm-reference-generate file)))

;;; The same entries, as macros the chapters can invoke

(defun vm-reference-macro-name (symbol)
  "The texinfo macro name that stands for SYMBOL's entry.
Texinfo promises only letters and digits in a macro name, so the hyphens
go: `vm-primary-inbox' becomes `vmdocvmprimaryinbox'.  Checked against every
command and user option VM has -- no two collide once stripped."
  (concat "vmdoc" (replace-regexp-in-string "[^a-zA-Z0-9]" ""
                                            (symbol-name symbol))))

(defun vm-reference-insert-macro (symbol kind)
  "Define the macro for SYMBOL, whose entry is written by KIND.
Backslashes in the body are doubled.  Inside a `@macro' a backslash names a
parameter, so the default of `vm-mime-encode-words-regexp' -- \"[^\\x0-\\x7f]+\"
-- makes makeinfo stop with \\ followed by `0-' instead of a parameter name.
The error comes when the macro is *used*, not when it is defined, so a
macro nobody invokes yet carries the fault silently."
  (insert (format "@macro %s\n" (vm-reference-macro-name symbol)))
  (let ((start (point)))
    (funcall kind symbol)
    (save-excursion
      (goto-char start)
      (while (search-forward "\\" nil t)
        (replace-match "\\\\" t t))))
  (insert "@end macro\n\n"))

(defun vm-reference-generate-macros (file)
  "Write to FILE a macro per command and user option, holding its entry.
The chapters invoke these where they used to carry a description of their
own, so what the manual says about an option is what the docstring says,
and the prose after the macro is left to say the things a docstring is not
the place for."
  (vm-reference-load-everything)
  (let ((sections (vm-reference-collect))
        (count 0))
    (with-temp-buffer
      (insert "@c This file is generated by info/gen-reference.el -- do not edit.\n")
      (insert "@c One macro per command and user option, named @vmdoc<symbol>\n")
      (insert "@c with the hyphens taken out.  Invoke one in a chapter to say\n")
      (insert "@c what the code says about that symbol.\n\n")
      (dolist (section sections)
        (dolist (command (nth 1 section))
          (vm-reference-insert-macro command #'vm-reference-insert-command)
          (setq count (1+ count)))
        (dolist (callback (nth 3 section))
          (vm-reference-insert-macro callback #'vm-reference-insert-command)
          (setq count (1+ count)))
        (dolist (option (nth 2 section))
          (vm-reference-insert-macro option #'vm-reference-insert-option)
          (setq count (1+ count))))
      (write-region (point-min) (point-max) file nil 'quiet))
    (message "gen-reference: %d docstring macros -> %s" count file)))

(defun vm-reference-macros-batch ()
  "Write the docstring macros to the file named on the command line."
  (let ((file (or (car command-line-args-left)
                  (error "Usage: -f vm-reference-macros-batch OUTPUT.texinfo"))))
    (setq command-line-args-left (cdr command-line-args-left))
    (vm-reference-generate-macros file)))

(provide 'gen-reference)

;;; gen-reference.el ends here
