;;; vm-reply-test.el --- Tests for vm-reply.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM reply functions in vm-reply.el

;;; Code:

(require 'vm-test-init)
(require 'vm-reply)

;;; Reply function existence tests

(ert-deftest vm-reply-test-functions-exist ()
  "Test that reply functions exist."
  (should (fboundp 'vm-reply))
  (should (fboundp 'vm-reply-include-text))
  (should (fboundp 'vm-followup))
  (should (fboundp 'vm-followup-include-text))
  (should (fboundp 'vm-forward-message))
  (should (fboundp 'vm-resend-message))
  (should (fboundp 'vm-send-digest))
  (should (fboundp 'vm-continue-composing-message)))

;;; Reply helper function tests

(ert-deftest vm-reply-test-helper-functions-exist ()
  "Test that reply helper functions exist."
  (should (fboundp 'vm-mail-internal))
  (should (fboundp 'vm-reply-other-frame))
  (should (fboundp 'vm-reply-include-text-other-frame))
  (should (fboundp 'vm-followup-other-frame))
  (should (fboundp 'vm-followup-include-text-other-frame))
  (should (fboundp 'vm-forward-message-other-frame)))

;;; vm-mail-mode-get-header-contents tests

(ert-deftest vm-reply-test-get-header-contents ()
  "Test vm-mail-mode-get-header-contents."
  (with-temp-buffer
    (insert "To: recipient@example.com\n")
    (insert "Subject: Test\n")
    (insert "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    ;; Test extracting To header
    (should (equal (vm-mail-mode-get-header-contents "To:")
                   "recipient@example.com"))))

(ert-deftest vm-reply-test-get-header-contents-missing ()
  "Test vm-mail-mode-get-header-contents with missing header."
  (with-temp-buffer
    (insert "To: recipient@example.com\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (should (null (vm-mail-mode-get-header-contents "Cc:")))))

(ert-deftest vm-reply-test-get-header-contents-multiline ()
  "Test vm-mail-mode-get-header-contents with folded header."
  (with-temp-buffer
    (insert "To: recipient1@example.com,\n")
    (insert " recipient2@example.com\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (let ((result (vm-mail-mode-get-header-contents "To:")))
      (should (string-match "recipient1" result))
      (should (string-match "recipient2" result)))))

;;; Yank/citation tests

(ert-deftest vm-reply-test-citation-variables ()
  "Test that citation variables exist."
  (should (boundp 'vm-included-text-prefix))
  (should (boundp 'vm-included-text-attribution-format)))

;;; vm-sanitize-buffer-name tests

(ert-deftest vm-reply-test-sanitize-buffer-name-nil ()
  "Test vm-sanitize-buffer-name with nil input."
  (let ((vm-drop-buffer-name-chars nil)
        (vm-buffer-name-limit 80))
    (should (null (vm-sanitize-buffer-name nil)))))

(ert-deftest vm-reply-test-sanitize-buffer-name-no-changes ()
  "Test vm-sanitize-buffer-name with clean name."
  (let ((vm-drop-buffer-name-chars "[^a-zA-Z0-9]")
        (vm-buffer-name-limit 80))
    (should (equal (vm-sanitize-buffer-name "CleanName123")
                   "CleanName123"))))

(ert-deftest vm-reply-test-sanitize-buffer-name-replaces ()
  "Test vm-sanitize-buffer-name replaces invalid chars."
  (let ((vm-drop-buffer-name-chars "[<>]")
        (vm-buffer-name-limit 80))
    (should (equal (vm-sanitize-buffer-name "Re: <test>")
                   "Re: _test_"))))

(ert-deftest vm-reply-test-sanitize-buffer-name-truncates ()
  "Test vm-sanitize-buffer-name truncates long names."
  (let ((vm-drop-buffer-name-chars nil)
        (vm-buffer-name-limit 20))
    (let ((result (vm-sanitize-buffer-name "This is a very long buffer name that exceeds limit")))
      (should (<= (length result) 20))
      (should (string-suffix-p "..." result)))))

(ert-deftest vm-reply-test-sanitize-buffer-name-keeps-accents ()
  "Test that the default keeps accented letters in a composition name.
Regression test for issue #29: `vm-drop-buffer-name-chars' was a
US-ASCII whitelist, so a reply to \"Ren\\='e\" was named \"reply to Ren_\"."
  (let ((vm-drop-buffer-name-chars
         (default-value 'vm-drop-buffer-name-chars))
        (vm-buffer-name-limit 80))
    (should (equal (vm-sanitize-buffer-name "reply to René")
                   "reply to René"))
    (should (equal (vm-sanitize-buffer-name "mail to 山田")
                   "mail to 山田"))))

(ert-deftest vm-reply-test-sanitize-buffer-name-drops-separator ()
  "Test that the default replaces what a file name cannot hold.
The buffer name is what the auto-save file is named after."
  (let ((vm-drop-buffer-name-chars "[[:cntrl:]/]")
        (vm-buffer-name-limit 80))
    (dolist (bad '("/" "\t" "\n"))
      (should (equal (vm-sanitize-buffer-name (concat "re: a" bad "b"))
                     "re: a_b")))))

(ert-deftest vm-reply-test-sanitize-buffer-name-windows-set ()
  "Test the wider set used on MS-Windows, where those characters are illegal."
  (let ((vm-drop-buffer-name-chars "[[:cntrl:]/\\:*?\"<>|]")
        (vm-buffer-name-limit 80))
    (dolist (bad '("/" "\\" ":" "*" "?" "\"" "<" ">" "|" "\t"))
      (should (equal (vm-sanitize-buffer-name (concat "a" bad "b")) "a_b")))
    (should (equal (vm-sanitize-buffer-name "René") "René"))))

(ert-deftest vm-reply-test-sanitize-buffer-name-keeps-subject-colon ()
  "Test that a subject colon survives off MS-Windows.
The MS-Windows set has to include `:', which would otherwise turn every
\"Re:\" into \"Re_\"; that is why it is not the default everywhere."
  (let ((vm-drop-buffer-name-chars "[[:cntrl:]/]")
        (vm-buffer-name-limit 80))
    (should (equal (vm-sanitize-buffer-name "mail to x on \"Re: hello\"")
                   "mail to x on \"Re: hello\"")))
  ;; and the default here does the same, unless this is MS-Windows
  (unless (memq system-type '(windows-nt ms-dos cygwin))
    (let ((vm-drop-buffer-name-chars
           (default-value 'vm-drop-buffer-name-chars))
          (vm-buffer-name-limit 80))
      (should (equal (vm-sanitize-buffer-name "re: hello") "re: hello")))))

;;; vm-strip-ignored-addresses tests

(ert-deftest vm-reply-test-strip-ignored-addresses-empty ()
  "Test vm-strip-ignored-addresses with no ignored addresses."
  (let ((vm-reply-ignored-addresses nil))
    (should (equal (vm-strip-ignored-addresses '("user@example.com"))
                   '("user@example.com")))))

(ert-deftest vm-reply-test-strip-ignored-addresses-removes ()
  "Test vm-strip-ignored-addresses removes matching addresses."
  (let ((vm-reply-ignored-addresses '("^noreply@")))
    (should (equal (vm-strip-ignored-addresses
                    '("user@example.com" "noreply@example.com"))
                   '("user@example.com")))))

(ert-deftest vm-reply-test-strip-ignored-addresses-multiple ()
  "Test vm-strip-ignored-addresses with multiple patterns."
  (let ((vm-reply-ignored-addresses '("^noreply@" "^bounce@" "@donotreply\\.com$")))
    (should (equal (vm-strip-ignored-addresses
                    '("user@example.com" "noreply@foo.com" "bounce@bar.com"
                      "other@donotreply.com" "valid@other.com"))
                   '("user@example.com" "valid@other.com")))))

;;; vm-add-reply-subject-prefix tests
;; Note: This function adds text attribution prefix to quoted text in a
;; composition buffer, NOT "Re:" to the subject line.

(ert-deftest vm-reply-test-add-reply-subject-prefix-function-exists ()
  "Test vm-add-reply-subject-prefix function exists."
  (should (fboundp 'vm-add-reply-subject-prefix)))

(ert-deftest vm-reply-test-add-reply-subject-prefix-prefixes-lines ()
  "Test vm-add-reply-subject-prefix adds prefix to lines."
  (let ((vm-included-text-prefix "> ")
        (vm-included-text-attribution-format nil))
    (with-temp-buffer
      (insert "To: test@example.com\n")
      (insert mail-header-separator)
      (insert "\n")
      (insert "Line one\n")
      (insert "Line two\n")
      (vm-add-reply-subject-prefix nil)
      (should (string-match "^> Line one" (buffer-string)))
      (should (string-match "^> Line two" (buffer-string))))))

;;; vm-mail-mode-remove-header tests

(ert-deftest vm-reply-test-mail-mode-remove-header ()
  "Test vm-mail-mode-remove-header removes header."
  (with-temp-buffer
    (insert "To: recipient@example.com\n")
    (insert "Cc: other@example.com\n")
    (insert "Subject: Test\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (vm-mail-mode-remove-header "Cc:")
    (should-not (string-match "Cc:" (buffer-string)))
    (should (string-match "To:" (buffer-string)))
    (should (string-match "Subject:" (buffer-string)))))

(ert-deftest vm-reply-test-mail-mode-remove-header-not-present ()
  "Test vm-mail-mode-remove-header with missing header."
  (with-temp-buffer
    (insert "To: recipient@example.com\n")
    (insert "\n")
    (insert "Body\n")
    (goto-char (point-min))
    ;; Should not error when header doesn't exist
    (vm-mail-mode-remove-header "Cc:")
    (should (string-match "To:" (buffer-string)))))

;;; vm-ignored-reply-to tests

(ert-deftest vm-reply-test-ignored-reply-to-nil ()
  "Test vm-ignored-reply-to with no ignored addresses."
  (let ((vm-reply-ignored-reply-tos nil))
    (should-not (vm-ignored-reply-to "user@example.com"))))

(ert-deftest vm-reply-test-ignored-reply-to-matches ()
  "Test vm-ignored-reply-to matches pattern."
  (let ((vm-reply-ignored-reply-tos '("^noreply@")))
    (should (vm-ignored-reply-to "noreply@example.com"))))

(ert-deftest vm-reply-test-ignored-reply-to-no-match ()
  "Test vm-ignored-reply-to doesn't match valid address."
  (let ((vm-reply-ignored-reply-tos '("^noreply@")))
    (should-not (vm-ignored-reply-to "user@example.com"))))

;;; vm-fill-long-lines-in-reply tests

(ert-deftest vm-reply-test-fill-long-lines-exists ()
  "Test vm-fill-long-lines-in-reply function exists."
  (should (fboundp 'vm-fill-long-lines-in-reply)))

;;; Composition buffer functions

(ert-deftest vm-reply-test-composition-buffer-functions-exist ()
  "Test composition buffer management functions exist."
  (should (fboundp 'vm-update-composition-buffer-name))
  (should (fboundp 'vm-forget-composition-buffer))
  (should (fboundp 'vm-new-composition-buffer)))

;;; vm-mail-to-mailto-url tests

(ert-deftest vm-reply-test-mail-to-mailto-url-exists ()
  "Test vm-mail-to-mailto-url function exists."
  (should (fboundp 'vm-mail-to-mailto-url)))

;;; Digest functions

(ert-deftest vm-reply-test-digest-functions-exist ()
  "Test digest-related functions exist."
  (should (fboundp 'vm-send-digest))
  (should (fboundp 'vm-send-rfc934-digest))
  (should (fboundp 'vm-send-rfc1153-digest))
  (should (fboundp 'vm-send-mime-digest)))

;;; Bounce/resend functions

(ert-deftest vm-reply-test-resend-functions-exist ()
  "Test resend/bounce functions exist."
  (should (fboundp 'vm-resend-message))
  (should (fboundp 'vm-resend-bounced-message))
  (should (fboundp 'vm-retry-bounced-message)))

;;; Yank functions

(ert-deftest vm-reply-test-yank-functions-exist ()
  "Test yank functions exist."
  (should (fboundp 'vm-yank-message))
  (should (fboundp 'vm-yank-message-other-folder))
  (should (fboundp 'vm-yank-message-presentation))
  (should (fboundp 'vm-yank-message-mime))
  (should (fboundp 'vm-yank-message-text))
  (should (fboundp 'vm-mail-yank-default)))

;;; Preview composition

(ert-deftest vm-reply-test-preview-composition-exists ()
  "Test vm-preview-composition exists."
  (should (fboundp 'vm-preview-composition)))

;;; Mail mode header functions

(ert-deftest vm-reply-test-mail-mode-header-functions-exist ()
  "Test mail mode header functions exist."
  (should (fboundp 'vm-mail-mode-insert-message-id-maybe))
  (should (fboundp 'vm-mail-mode-insert-date-maybe))
  (should (fboundp 'vm-mail-mode-remove-message-id-maybe))
  (should (fboundp 'vm-mail-mode-remove-date-maybe))
  (should (fboundp 'vm-mail-get-header-contents)))

;;; Mail send functions

(ert-deftest vm-reply-test-mail-send-functions-exist ()
  "Test mail send functions exist."
  (should (fboundp 'vm-mail-send))
  (should (fboundp 'vm-mail-send-and-exit))
  (should (fboundp 'vm-keep-mail-buffer)))

;;; Forward functions

(ert-deftest vm-reply-test-forward-functions-exist ()
  "Test forward message functions exist."
  (should (fboundp 'vm-forward-message))
  (should (fboundp 'vm-forward-message-all-headers))
  (should (fboundp 'vm-forward-message-plain)))


;;; X-Mailer names the editor (issue #520)

(ert-deftest vm-reply-test-emacs-name-and-version ()
  "REGRESSION: the editor is named, not just its version number.
Issue #520.  `emacs-version' the variable has held only the number for years, so
an X-Mailer built from it read \"VM 8.3.x under 31.0.50\" and did not say which
editor sent the mail.  The function `emacs-version' does say, but with a build
number, platform and date after it, which is more than a header wants."
  (require 'vm)
  (should (string-match-p "\\`GNU Emacs [0-9]" (vm-emacs-name-and-version)))
  ;; Only the name and the version -- no build, platform or date.
  (should-not (string-match-p "build\\|of [0-9]\\|(" (vm-emacs-name-and-version))))

(ert-deftest vm-reply-test-emacs-name-and-version-other-emacsen ()
  "The name and version are taken off the front of any of these strings.
Includes the XEmacs form quoted on the issue, since VM still claims to support
XEmacs and its `emacs-version' is a different shape."
  (require 'vm)
  (dolist (case
           '(("GNU Emacs 30.2 (build 2, aarch64-apple-darwin24.6.0, NS appkit-2575.70)\n of 2025-09-25"
              . "GNU Emacs 30.2")
             ("GNU Emacs 31.0.50 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.43)\n of 2025-11-01"
              . "GNU Emacs 31.0.50")
             ("GNU Emacs 28.1 (build 1, x86_64-pc-linux-gnu)" . "GNU Emacs 28.1")
             ("XEmacs 21.4 (patch 22) \"Instant Classic\" [Lucid] (i686-pc-linux, Mule) of Tue Jan 15 2002"
              . "XEmacs 21.4")))
    (cl-letf (((symbol-function 'emacs-version)
               (lambda (&rest _) (car case))))
      (should (equal (cdr case) (vm-emacs-name-and-version))))))

(ert-deftest vm-reply-test-emacs-name-and-version-falls-back ()
  "An unrecognisable version string still names the editor.
The header is worth less without the name than with a guessed one, and this is
the branch that runs if the function ever stops leading with \"GNU Emacs\"."
  (require 'vm)
  (cl-letf (((symbol-function 'emacs-version)
             (lambda (&rest _) "something entirely unexpected")))
    (should (string-match-p "Emacs" (vm-emacs-name-and-version)))
    (should (string-match-p (regexp-quote emacs-version)
                            (vm-emacs-name-and-version)))))

(provide 'vm-reply-test)

;;; vm-reply-test.el ends here
