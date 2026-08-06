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

(provide 'vm-reference-test)

;;; vm-reference-test.el ends here
