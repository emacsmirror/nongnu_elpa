;;; vm-minibuf-test.el --- Tests for vm-minibuf.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM minibuffer functions in vm-minibuf.el

;;; Code:

(require 'vm-test-init)
(require 'vm-minibuf)

;;; Minibuffer function existence tests

(ert-deftest vm-minibuf-test-functions-exist ()
  "Test that minibuffer functions exist."
  (should (fboundp 'vm-minibuffer-complete-word))
  (should (fboundp 'vm-minibuffer-complete-word-and-exit))
  (should (fboundp 'vm-minibuffer-completion-message))
  (should (fboundp 'vm-minibuffer-replace-word))
  (should (fboundp 'vm-minibuffer-show-completions))
  (should (fboundp 'vm-show-list))
  (should (fboundp 'vm-minibuffer-completion-help))
  (should (fboundp 'vm-keyboard-read-string))
  (should (fboundp 'vm-read-string))
  (should (fboundp 'vm-read-number))
  (should (fboundp 'vm-keyboard-read-file-name))
  (should (fboundp 'vm-read-file-name))
  (should (fboundp 'vm-read-folder-name)))

;;; vm-show-list tests

(ert-deftest vm-minibuf-test-show-list-simple ()
  "Test vm-show-list with simple list."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list '("apple" "banana" "cherry"))
                  (buffer-string))))
    ;; Should contain all items
    (should (string-match "apple" result))
    (should (string-match "banana" result))
    (should (string-match "cherry" result))))

(ert-deftest vm-minibuf-test-show-list-empty ()
  "Test vm-show-list with empty list."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list nil)
                  (buffer-string))))
    ;; Should handle empty list gracefully
    (should (stringp result))))

(ert-deftest vm-minibuf-test-show-list-single-item ()
  "Test vm-show-list with single item."
  (let ((result (with-temp-buffer
                  (display-buffer (current-buffer))
                  (vm-show-list '("item"))
                  (buffer-string))))
    ;; Should contain the item
    (should (string-match "item" result))))

;;; vm-minibuffer-replace-word tests
;; Note: vm-minibuffer-replace-word always replaces the last word in buffer

(ert-deftest vm-minibuf-test-replace-word-basic ()
  "Test vm-minibuffer-replace-word replaces last word in buffer."
  (with-temp-buffer
    (insert "hello world")
    (vm-minibuffer-replace-word "universe")
    (should (string-match "hello universe" (buffer-string)))))

(ert-deftest vm-minibuf-test-replace-word-at-end ()
  "Test vm-minibuffer-replace-word replaces last word."
  (with-temp-buffer
    (insert "hello")
    (vm-minibuffer-replace-word "goodbye")
    (should (string-match "goodbye" (buffer-string)))))

;;; vm-read-number tests
;; Note: vm-read-number calls read-from-minibuffer which we can't easily
;; test interactively, so we test the parsing logic

(ert-deftest vm-minibuf-test-read-number-function-arity ()
  "Test vm-read-number function signature."
  (let ((arity (func-arity 'vm-read-number)))
    ;; Should accept at least 1 argument (prompt)
    ;; cdr arity can be 'many when advice is installed (e.g. coverage)
    (should (or (eq (cdr arity) 'many)
                (>= (cdr arity) 1)))))

;;; vm-keyboard-read-string tests

;;; vm-read-file-name tests

(ert-deftest vm-minibuf-test-read-file-name-function-arity ()
  "Test vm-read-file-name accepts proper arguments."
  (let ((arity (func-arity 'vm-read-file-name)))
    ;; Should accept arguments like read-file-name
    ;; cdr arity can be 'many when advice is installed (e.g. coverage)
    (should (or (eq (cdr arity) 'many)
                (>= (cdr arity) 1)))))

;;; vm-read-folder-name tests

(ert-deftest vm-minibuf-test-read-folder-name-function-arity ()
  "Test vm-read-folder-name function signature."
  (let ((arity (func-arity 'vm-read-folder-name)))
    ;; Should be callable
    ;; cdr arity can be 'many when advice is installed (e.g. coverage)
    (should (or (eq (cdr arity) 'many)
                (>= (cdr arity) 0)))))

;;; Completion variables

(ert-deftest vm-minibuf-test-completion-variables-exist ()
  "Test that completion-related variables exist."
  (should (boundp 'vm-completion-auto-correct))
  (should (boundp 'vm-completion-auto-space)))

;;; File name history (#587)

;; `read-file-name' has no HISTORY argument in GNU Emacs; its sixth
;; argument is a completion predicate.  VM used to pass the history
;; symbol there, so the history was neither offered nor extended.

(defmacro vm-minibuf-test-with-answer (answer &rest body)
  "Run BODY with `read-file-name' answering ANSWER.
Binds `vm-minibuf-test-saw-history' to the `file-name-history' the call
was given."
  (declare (indent 1))
  `(let ((vm-minibuf-test-saw-history 'unset))
     (cl-letf (((symbol-function 'read-file-name)
		(lambda (&rest _args)
		  (setq vm-minibuf-test-saw-history file-name-history)
		  ,answer)))
       ,@body)))

(ert-deftest vm-minibuf-test-file-name-history-is-offered ()
  "The history named by HISTORY reaches the minibuffer."
  (let ((vm-folder-history '("~/Mail/inbox" "~/Mail/old")))
    (vm-minibuf-test-with-answer "~/Mail/new"
      (vm-keyboard-read-file-name "Folder: " "~/Mail/" nil nil nil
				  'vm-folder-history)
      (should (equal vm-minibuf-test-saw-history
		     '("~/Mail/inbox" "~/Mail/old"))))))

(ert-deftest vm-minibuf-test-file-name-history-is-extended ()
  "The answer is pushed onto HISTORY, most recent first, without duplicates."
  (let ((vm-folder-history '("~/Mail/inbox" "~/Mail/old")))
    (vm-minibuf-test-with-answer "~/Mail/new"
      (should (equal (vm-keyboard-read-file-name "Folder: " "~/Mail/" nil nil nil
						 'vm-folder-history)
		     "~/Mail/new")))
    (should (equal vm-folder-history
		   '("~/Mail/new" "~/Mail/inbox" "~/Mail/old")))
    (vm-minibuf-test-with-answer "~/Mail/inbox"
      (vm-keyboard-read-file-name "Folder: " "~/Mail/" nil nil nil
				  'vm-folder-history))
    (should (equal vm-folder-history
		   '("~/Mail/inbox" "~/Mail/new" "~/Mail/old")))))

(ert-deftest vm-minibuf-test-file-name-history-leaves-the-global-alone ()
  "`file-name-history' itself is not touched."
  (let ((file-name-history '("/etc/passwd"))
	(vm-folder-history '("~/Mail/inbox")))
    (vm-minibuf-test-with-answer "~/Mail/new"
      (vm-keyboard-read-file-name "Folder: " "~/Mail/" nil nil nil
				  'vm-folder-history))
    (should (equal file-name-history '("/etc/passwd")))))

(ert-deftest vm-minibuf-test-file-name-history-without-history ()
  "With no HISTORY the call is a plain `read-file-name'."
  (vm-minibuf-test-with-answer "~/Mail/new"
    (should (equal (vm-keyboard-read-file-name "Folder: " "~/Mail/") "~/Mail/new"))))

(ert-deftest vm-minibuf-test-file-name-history-must-be-a-symbol ()
  "A history list where a history name belongs is an error, not a prompt.
Four callers used to pass the variable's value; while the variables were
always empty that went unnoticed, and a user who filled one in got
`(wrong-type-argument symbolp ...)' from `symbol-value' instead."
  (let ((text-quoting-style 'grave))
    (vm-minibuf-test-with-answer "~/Mail/new"
      (should (equal (should-error
		      (vm-keyboard-read-file-name "Folder: " "~/Mail/" nil nil nil
						  '("~/attachments")))
		     '(error "HISTORY should name a variable holding a list of file names, not (\"~/attachments\")"))))))

(ert-deftest vm-minibuf-test-history-variables-are-not-functions ()
  "No history variable doubles as an always-true completion predicate.
`vm-folder-history' and `vm-grepmail-folders-history' were both defined
as functions returning t, so that being passed as `read-file-name''s
PREDICATE would not signal `void-function'."
  (require 'vm-grepmail)
  (dolist (history '(vm-folder-history
		     vm-grepmail-folders-history
		     vm-mime-save-all-attachments-history
		     vm-attach-files-in-directory-regexps-history))
    (should (boundp history))
    (should-not (fboundp history))))

(ert-deftest vm-minibuf-test-history-arguments-are-quoted-symbols ()
  "Every caller passes HISTORY as a symbol, not as the variable's value."
  (let ((sites 0))
    (dolist (file '("vm-mime.el" "vm-avirtual.el"
		    "vm-postpone.el" "vm-grepmail.el" "vm-save.el" "vm.el"))
      (with-temp-buffer
	(insert-file-contents (expand-file-name file vm-test-lisp-dir))
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*'?\\(vm-[a-z-]*history\\))*$" nil t)
	  (unless (save-excursion
		    (goto-char (match-beginning 0))
		    (looking-at "[ \t]*'"))
	    (error "%s:%d passes %s by value, not by name"
		   file (line-number-at-pos) (match-string 1)))
	  (setq sites (1+ sites)))))
    (should (> sites 5))))

;;; What these do, in place of tests that they were bound.

(ert-deftest vm-minibuf-test-read-number-keeps-asking-until-it-gets-one ()
  "A number is what comes back, and nothing else is accepted.
The prompt is repeated rather than an error signalled, since this is what
answers `C-u' style counts."
  (let ((answers '("not a number" "  -12 messages")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (pop answers))))
      (should (= (vm-read-number "How many? ") -12))
      (should-not answers))))

(ert-deftest vm-minibuf-test-read-number-takes-the-leading-number ()
  "Leading whitespace and trailing text are ignored, and a sign is not."
  (dolist (case '(("3" . 3) ("  7  " . 7) ("-2" . -2) ("12 foo" . 12)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (car case))))
      (should (= (vm-read-number "n? ") (cdr case))))))

(ert-deftest vm-minibuf-test-replace-word-replaces-the-last-word-only ()
  "Completion replaces the last whitespace-delimited word, not the whole
line: the last of several addresses stands, and the ones before it stay.
A path counts as one word, so the completion has to carry the whole of it."
  (with-temp-buffer
    (insert "/home/me/mail/inb")
    (vm-minibuffer-replace-word "/home/me/mail/inbox")
    (should (equal (buffer-string) "/home/me/mail/inbox")))
  (with-temp-buffer
    (insert "alice@example.com bo")
    (vm-minibuffer-replace-word "bob@example.com")
    (should (equal (buffer-string) "alice@example.com bob@example.com")))
  ;; with nothing typed yet, the word is simply inserted
  (with-temp-buffer
    (vm-minibuffer-replace-word "inbox")
    (should (equal (buffer-string) "inbox"))))

(provide 'vm-minibuf-test)

;;; vm-minibuf-test.el ends here
