;;; vm-serial-test.el --- Tests for vm-serial.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM serial mail functions in vm-serial.el

;;; Code:

(require 'vm-test-init)
(require 'vm-serial)
;; `warning-suppress-log-types' below is let-bound, and a `let' of a
;; variable warnings.el has not yet declared special binds it lexically;
;; the `defvar' that arrives with the file then signals.  The suite hid
;; this by loading warnings.el in an earlier file, so it showed only in
;; `test-runner --one vm-serial-test.el'.
(require 'warnings)

;;; vm-serial-cookie tests

(ert-deftest vm-serial-test-default-cookie ()
  "Test that the default cookie is $."
  (should (stringp vm-serial-cookie))
  (should (string= vm-serial-cookie "$")))

;;; vm-serial-random-string tests

(ert-deftest vm-serial-test-random-string-returns-string ()
  "Test that vm-serial-random-string returns a string from the list."
  (let ((result (vm-serial-random-string '("a" "b" "c"))))
    (should (stringp result))
    (should (member result '("a" "b" "c")))))

(ert-deftest vm-serial-test-random-string-single-element ()
  "Test vm-serial-random-string with single element list."
  (let ((result (vm-serial-random-string '("only"))))
    (should (string= result "only"))))

(ert-deftest vm-serial-test-random-string-distribution ()
  "Test vm-serial-random-string returns varied results over many calls."
  (let ((results (make-hash-table :test 'equal))
        (choices '("a" "b" "c")))
    ;; Call many times and collect results
    (dotimes (_ 100)
      (let ((r (vm-serial-random-string choices)))
        (puthash r (1+ (gethash r results 0)) results)))
    ;; All choices should appear at least once (probabilistically)
    (should (>= (hash-table-count results) 2))))

;;; vm-serial-get-name tests

(ert-deftest vm-serial-test-get-name-full ()
  "Test vm-serial-get-name returns full name."
  (let ((vm-serial-to '("John Doe" "john@example.com")))
    (should (string= (vm-serial-get-name) "John Doe"))))

(ert-deftest vm-serial-test-get-name-first ()
  "Test vm-serial-get-name extracts first name."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'first "John Doe") "John"))))

(ert-deftest vm-serial-test-get-name-last ()
  "Test vm-serial-get-name extracts last name."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'last "John Doe") "Doe"))))

(ert-deftest vm-serial-test-get-name-first-multi-word ()
  "Test vm-serial-get-name with multi-word names."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'first "Mary Jane Watson") "Mary"))))

(ert-deftest vm-serial-test-get-name-last-multi-word ()
  "Test vm-serial-get-name last with multi-word names.
Returns everything after the first name, not just the last word."
  (let ((vm-serial-to nil))
    (should (string= (vm-serial-get-name 'last "Mary Jane Watson") "Jane Watson"))))

;;; vm-serial-eval-token-value tests

(ert-deftest vm-serial-test-eval-token-value-string ()
  "Test vm-serial-eval-token-value with string input."
  (should (string= (vm-serial-eval-token-value "hello") "hello")))

(ert-deftest vm-serial-test-eval-token-value-function ()
  "Test vm-serial-eval-token-value with function."
  (should (string= (vm-serial-eval-token-value (lambda () "result")) "result")))

(ert-deftest vm-serial-test-eval-token-value-expression ()
  "Test vm-serial-eval-token-value with expression."
  (should (string= (vm-serial-eval-token-value '(concat "a" "b")) "ab")))

(ert-deftest vm-serial-test-eval-token-value-number ()
  "Test vm-serial-eval-token-value with numeric expression."
  (should (string= (vm-serial-eval-token-value '(number-to-string (+ 1 2))) "3")))

;;; vm-serial-token-alist tests

(ert-deftest vm-serial-test-default-tokens-exist ()
  "Test that default tokens are defined."
  (should (assoc "to" vm-serial-token-alist))
  (should (assoc "sir" vm-serial-token-alist))
  (should (assoc "you" vm-serial-token-alist))
  (should (assoc "mr" vm-serial-token-alist))
  (should (assoc "me" vm-serial-token-alist))
  (should (assoc "point" vm-serial-token-alist))
  (should (assoc "sig" vm-serial-token-alist)))

(ert-deftest vm-serial-test-token-structure ()
  "Test that tokens have proper structure (name . value)."
  (let ((token (assoc "me" vm-serial-token-alist)))
    (should token)
    (should (stringp (car token)))
    (should (cdr token))))  ; Value should be non-nil

;;; vm-serial-get-token tests

(ert-deftest vm-serial-test-get-token-existing ()
  "Test vm-serial-get-token for existing token."
  (let ((value (vm-serial-get-token "me")))
    ;; Should return a value (actual value depends on user settings)
    (should value)))

(ert-deftest vm-serial-test-get-token-nonexistent ()
  "Test vm-serial-get-token returns nil for nonexistent token.
It warns as well, which `display-warning' logs in `*Warnings*', a buffer that
then outlives the test (issue #559).  Logging is suppressed here rather than the
buffer killed afterwards: the warning is not what this test is about, and
nothing else in the run wants it."
  (let ((warning-suppress-log-types '((emacs))))
    (should (null (vm-serial-get-token "nonexistent-token-xyz-12345")))))

;;; vm-serial-set-token tests

(ert-deftest vm-serial-test-set-token ()
  "Test vm-serial-set-token adds or updates token."
  (let ((vm-serial-token-alist (copy-alist vm-serial-token-alist)))
    (vm-serial-set-token "test-token" "test-value")
    (should (assoc "test-token" vm-serial-token-alist))
    (should (string= (vm-serial-get-token "test-token") "test-value"))))

(ert-deftest vm-serial-test-set-token-update ()
  "Test vm-serial-set-token updates existing token."
  (let ((vm-serial-token-alist (copy-alist vm-serial-token-alist)))
    (vm-serial-set-token "test-token" "value1")
    (vm-serial-set-token "test-token" "value2")
    (should (string= (vm-serial-get-token "test-token") "value2"))))

;;; vm-serial-mails-alist tests

(ert-deftest vm-serial-test-mails-alist-is-list ()
  "Test that vm-serial-mails-alist is a list."
  (should (listp vm-serial-mails-alist)))

;;; vm-serial-get-mail tests

(ert-deftest vm-serial-test-get-mail-nonexistent ()
  "Test vm-serial-get-mail returns nil for nonexistent template."
  (should (null (vm-serial-get-mail "nonexistent-mail-xyz-12345"))))

;;; Interactive command tests

(ert-deftest vm-serial-test-commands-interactive ()
  "Test that serial commands are interactive."
  (should (commandp 'vm-serial-expand-tokens))
  (should (commandp 'vm-serial-yank-mail))
  (should (commandp 'vm-serial-send-mail))
  (should (commandp 'vm-serial-set-token))
  (should (commandp 'vm-serial-get-token))
  (should (commandp 'vm-serial-insert-token)))

;;; Variable existence tests

(ert-deftest vm-serial-test-variables-exist ()
  "Test that serial-related variables exist."
  (should (boundp 'vm-serial-token-alist))
  (should (boundp 'vm-serial-mails-alist))
  (should (boundp 'vm-serial-cookie))
  (should (boundp 'vm-serial-fcc))
  (should (boundp 'vm-serial-mail-signature))
  (should (boundp 'vm-serial-unknown-to)))

;;; Composing in a mail buffer

(defmacro vm-serial-test--in-a-composition (headers body &rest forms)
  "Run FORMS in a `mail-mode' buffer holding HEADERS and BODY."
  (declare (indent 2))
  `(with-temp-buffer
     (mail-mode)
     (insert ,headers mail-header-separator "\n" ,body)
     ,@forms))

(defun vm-serial-test--fake-bbdb-extract (address &optional all)
  "Answer as BBDB 3's `bbdb-extract-address-components' does.
BBDB is not in every test environment, and what these tests are about is
the arguments VM passes and the answer it expects back, not BBDB itself."
  (if all
      (mail-extract-address-components address t)
    (mail-extract-address-components address)))

;;; vm-serial-get-emails tests

(ert-deftest vm-serial-test-get-emails-answers-a-pair ()
  "`vm-serial-get-emails' answers (NAME ADDRESS), with BBDB and without."
  (vm-serial-test--in-a-composition "To: Alice Smith <alice@example.com>\n" ""
    (cl-letf (((symbol-function 'bbdb-extract-address-components) nil))
      (should (equal (vm-serial-get-emails "To:")
                     '("Alice Smith" "alice@example.com"))))
    (cl-letf (((symbol-function 'bbdb-extract-address-components)
               #'vm-serial-test--fake-bbdb-extract))
      (should (equal (vm-serial-get-emails "To:")
                     '("Alice Smith" "alice@example.com"))))))

(ert-deftest vm-serial-test-name-tokens-work-with-bbdb ()
  "The name tokens read the recipient when BBDB is loaded.
`vm-serial-get-emails' took a `car' of BBDB's answer, so every name token
was reading `car' of a string and expanding to nothing."
  (vm-serial-test--in-a-composition "To: Alice Smith <alice@example.com>\n" ""
    (cl-letf (((symbol-function 'bbdb-extract-address-components)
               #'vm-serial-test--fake-bbdb-extract))
      (let ((vm-serial-to nil))
        (should (equal (vm-serial-get-name) "Alice Smith"))
        (should (equal (vm-serial-get-name 'first) "Alice"))
        (should (equal (vm-serial-get-name 'last) "Smith"))))))

(ert-deftest vm-serial-test-get-emails-with-no-such-header ()
  "A missing or empty header answers nil, and the name falls back.
`mail-extract-address-components' signals on nil, which reached the caller
as a warning and left `vm-serial-unknown-to' unreachable."
  (vm-serial-test--in-a-composition "Subject: hi\n" ""
    (let ((vm-serial-to nil)
          (vm-serial-unknown-to "unknown"))
      (should (null (vm-serial-get-emails "To:")))
      (should (equal (vm-serial-get-name) "unknown"))))
  (vm-serial-test--in-a-composition "To:   \n" ""
    (let ((vm-serial-to nil))
      (should (null (vm-serial-get-emails "To:"))))))

;;; vm-serial-eval-token-value warning tests

(defun vm-serial-test--signals-plainly ()
  (error "kaboom"))

(defun vm-serial-test--signals-with-a-percent ()
  (error "50%% off"))

(ert-deftest vm-serial-test-eval-token-value-names-the-value-that-failed ()
  "The warning names the token value, which clearing it first hid."
  (let (captured)
    (cl-letf (((symbol-function 'warn)
               (lambda (&rest args) (setq captured args))))
      (should (null (vm-serial-eval-token-value
                     '(vm-serial-test--signals-plainly))))
      (should captured)
      (should (string-match-p "vm-serial-test--signals-plainly"
                              (apply #'format captured))))))

(ert-deftest vm-serial-test-eval-token-value-warns-through-a-percent ()
  "A percent sign in the error text does not break the warning.
`warn' formats its own message, so a string already run through `format'
is formatted a second time and an error text of \"50% off\" made the
warning signal in place of the token it was reporting."
  (let (captured)
    (cl-letf (((symbol-function 'warn)
               (lambda (&rest args) (setq captured args))))
      (should (null (vm-serial-eval-token-value
                     '(vm-serial-test--signals-with-a-percent))))
      (should (string-match-p "50% off" (apply #'format captured))))))

;;; vm-serial-expand-tokens region tests

(ert-deftest vm-serial-test-expand-tokens-honours-its-region ()
  "RSTART and REND bound what is expanded.
They were overwritten with the whole body, so `vm-serial-insert-token'
re-expanded every token already in the message."
  (vm-serial-test--in-a-composition "To: Alice Smith <alice@example.com>\n"
      "leave $you alone\n"
    (let ((vm-serial-to nil)
          (start (point-max)))
      (goto-char (point-max))
      (insert "$mr")
      (vm-serial-expand-tokens start (point))
      (should (string-match-p "leave \\$you alone" (buffer-string)))
      (should (string-match-p "Alice Smith" (buffer-string))))))

(ert-deftest vm-serial-test-expand-tokens-can-expand-in-a-header ()
  "`vm-serial-insert-token' expands where point is, header included.
Expansion was confined to the body whatever the caller asked for, so a
token inserted in a header was left standing as its own text."
  (vm-serial-test--in-a-composition "To: Alice Smith <alice@example.com>\n"
      "body\n"
    (let ((vm-serial-to nil))
      (goto-char (point-min))
      (end-of-line)
      (vm-serial-insert-token "mr")
      (should (string-match-p "^To: Alice Smith <alice@example.com>Alice Smith$"
                              (buffer-string))))))

(ert-deftest vm-serial-test-expand-tokens-leaves-the-buffer-as-wide-as-it-found-it ()
  "An invalid token expression does not leave the composition narrowed.
`narrow-to-region' was undone by a `widen' the error jumped over, so the
composition buffer showed the body alone from then on."
  (vm-serial-test--in-a-composition "To: Alice Smith <alice@example.com>\n"
      "bad ${you and more\n"
    (let ((vm-serial-to nil)
          (size (buffer-size)))
      (should-error (vm-serial-expand-tokens) :type 'error)
      (should (= (point-min) 1))
      (should (= (point-max) (1+ size))))))

;;; vm-serial-send-mail tests

(defvar vm-serial-test--sent nil
  "The To header of each message `vm-serial-send-mail' sent.")

(defun vm-serial-test--work-buffer (&rest args)
  "Stand in for `vm-mail-internal', which wants a running VM."
  (let ((name (plist-get args :buffer-name)))
    (with-current-buffer (get-buffer-create name)
      (erase-buffer)
      (mail-mode))
    (get-buffer name)))

(defun vm-serial-test--send-to-two (&optional bbdb)
  "Send to two recipients, answering with the To header of each message.
With BBDB non-nil, BBDB's address extractor is present."
  (let ((vm-serial-test--sent nil))
    (vm-serial-test--in-a-composition
        "To: Alice Smith <alice@example.com>, Bob Jones <bob@example.com>\n"
        "hello\n"
      (cl-letf (((symbol-function 'bbdb-extract-address-components)
                 (and bbdb #'vm-serial-test--fake-bbdb-extract))
                ((symbol-function 'vm-mail-internal)
                 #'vm-serial-test--work-buffer)
                ((symbol-function 'vm-mail-send)
                 (lambda (&rest _)
                   (push (vm-mail-mode-get-header-contents "To:")
                         vm-serial-test--sent)))
                ((symbol-function 'switch-to-buffer) #'ignore)
                ((symbol-function 'kill-this-buffer) #'ignore))
        (unwind-protect
            (vm-serial-send-mail t)
          (let ((work (get-buffer vm-serial-send-mail-buffer)))
            (when work (kill-buffer work))))))
    (nreverse vm-serial-test--sent)))

(ert-deftest vm-serial-test-send-mail-without-bbdb ()
  "One message per recipient with no BBDB installed.
The branch taken without BBDB called `bbdb-split', a BBDB function, so the
command died with a void-function error for everyone who has no BBDB."
  (should (equal (vm-serial-test--send-to-two nil)
                 '("Alice Smith <alice@example.com>"
                   "Bob Jones <bob@example.com>"))))

(ert-deftest vm-serial-test-send-mail-with-bbdb ()
  "One message per recipient with BBDB installed.
BBDB's extractor was asked without its ALL argument, so it answered one
pair for the whole header and a single message went out with no To at all."
  (should (equal (vm-serial-test--send-to-two t)
                 '("Alice Smith <alice@example.com>"
                   "Bob Jones <bob@example.com>"))))


;;; The mode, and loading not switching it on (emacs-vm/vm#788)

(ert-deftest vm-serial-test-loading-does-not-advise-anything ()
  "Loading vm-serial does not advise `vm-mail-send-and-exit'.
Customize loads this file whenever it is asked about a VM option, and it
required vm-postpone too, so one `C-h v' installed the advice here and the
hooks and keys there."
  (require 'vm-serial)
  (let ((vm-serial-mode nil))
    (should-not (advice-member-p #'vm-serial--send-mail
                                'vm-mail-send-and-exit))))

(ert-deftest vm-serial-test-mode-toggles-the-advice ()
  "The mode adds the advice and removes it again."
  (require 'vm-serial)
  (let ((vm-serial-mode nil))
    (vm-serial-mode 1)
    (should (advice-member-p #'vm-serial--send-mail 'vm-mail-send-and-exit))
    (vm-serial-mode -1)
    (should-not (advice-member-p #'vm-serial--send-mail
                                 'vm-mail-send-and-exit))))

(provide 'vm-serial-test)

;;; vm-serial-test.el ends here