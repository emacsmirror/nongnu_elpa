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


;;; composing from an empty folder (issue #514)

(defmacro vm-reply-test--in-folder (spec &rest body)
  "Visit a generated folder and run BODY in it, then clean up.
SPEC is (CONTENT), the folder text -- \"\" for an empty folder.  Every buffer the
visit created is killed afterwards, so one test cannot leave a folder buffer for
the next to trip over."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-reply-test" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-frame-per-composition nil)
          (vm-mutable-frame-configuration nil)
          (before (buffer-list)))
     (require 'vm)
     (unwind-protect
         (progn
           (with-temp-file file (insert ,(car spec)))
           (vm-visit-folder file)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               ;; vm-postpone asks whether to save a composition as a draft
               ;; when its buffer is killed, and a question in batch reads
               ;; stdin and fails.  Nothing here is about drafts.
               (setq kill-buffer-hook nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defconst vm-reply-test--one-message
  "From alice@example.com Mon Jan  1 00:00:00 2024\nFrom: Alice <alice@example.com>\nSubject: hello\n\nBody.\n\n"
  "A one-message folder.")

(ert-deftest vm-reply-test-validate-allows-an-empty-folder ()
  "REGRESSION: composing does not require the folder to hold a message.
Issue #514: `vm-mail-from-folder' -- `m' -- validated with a minimum of 1, so in
an empty folder it answered \"Folder is empty\" and composed nothing.  An IMAP
inbox with no mail in it is the ordinary way to meet that.  The current message
is wanted only as a parent, and an empty folder simply has none.

This pins the contract `m' asks for.  The command itself is driven end to end
by the test below, and again by `vm-pcrisis-test-mail-from-an-empty-folder',
which covers the copy of this validation in the pcrisis advice."
  (require 'vm)
  (let ((vm-mail-buffer nil))
    (with-temp-buffer
      (setq major-mode 'vm-mode)
      (setq vm-message-list nil)
      ;; What `m' asks for now: no minimum, so an empty folder is fine.
      (should-not (condition-case err
                      (progn (vm-select-folder-buffer-and-validate 0) nil)
                    (error err)))
      ;; What it asked for before, kept so the difference is visible.  A minimum
      ;; of 1 still refuses, which is right for commands that act on a message.
      (should (eq 'folder-empty
                  (car (condition-case err
                           (progn (vm-select-folder-buffer-and-validate 1) nil)
                         (error err))))))))

(ert-deftest vm-reply-test-sender-guess-without-a-message ()
  "The sender guess gives nil when there is no message to take a sender from.
The other half of #514: \"if possible\" has to include there being a message,
or `vm-select-recipient-from-sender-if-possible' reads a header out of nil and
signals instead of returning no recipient."
  (require 'vm)
  (with-temp-buffer
    (setq major-mode 'vm-mode)
    (setq vm-message-pointer nil)
    (let ((vm-mail-use-sender-address t))
      (should-not (vm-select-recipient-from-sender-if-possible))
      ;; and with the variable off, as by default
      (let ((vm-mail-use-sender-address nil))
        (should-not (vm-select-recipient-from-sender-if-possible))))))

(ert-deftest vm-reply-test-mail-from-an-empty-folder ()
  "REGRESSION: `m' in an empty folder composes a message.
Issue #514 driven through the command rather than through the validation it
calls.  The advice pcrisis installs on this command has its own copy of that
validation and its own test; this is the plain command, so the advice is taken
off for the duration if the module happens to be loaded."
  (require 'vm)
  (let ((advised (advice-member-p 'vmpc--mail 'vm-mail-from-folder)))
    (when advised (advice-remove 'vm-mail-from-folder 'vmpc--mail))
    (unwind-protect
        (vm-reply-test--in-folder ("")
          (should (null vm-message-list))
          (vm-mail-from-folder)
          (should (eq major-mode 'mail-mode))
          (should (string-match-p "^To:" (buffer-string))))
      (when advised (advice-add 'vm-mail-from-folder :around #'vmpc--mail)))))

(ert-deftest vm-reply-test-mail-from-folder-still-uses-the-sender ()
  "The control: with a message present the sender is still offered.
Without this, the fix above could pass by never looking at the sender at all."
  (vm-reply-test--in-folder (vm-reply-test--one-message)
    (should (= 1 (length vm-message-list)))
    (let ((vm-mail-use-sender-address t))
      (should (string-match-p "alice@example.com"
                              (or (vm-select-recipient-from-sender-if-possible)
                                  ""))))))


;;; parenting the composition keymap (issue #560)

(ert-deftest vm-reply-test-parenting-the-mail-keymap-is-repeatable ()
  "REGRESSION: giving `vm-mail-mode-map' its parent twice is harmless.
Issue #560: on GNU Emacs this was done with `(nconc vm-mail-mode-map
mail-mode-map)\', which splices Mail mode\'s keymap onto the end of VM\'s.
`keymap-parent\' then answers `mail-mode-map\', so it looks like parenting, but a
second call walks to the end of the spliced list -- which is now inside
`mail-mode-map\' -- and points that cell back at `mail-mode-map\'.  The keymap is
then circular, `lookup-key\' on it does not return, and Emacs dies of a stack
overflow.  A global flag was all that kept it to one call.

Checked here on keymaps of our own, because the failure this guards against is
Emacs crashing, and a test may not do that: it has to be able to report."
  (require 'vm)
  (let* ((mail-mode-map (make-sparse-keymap))
         (vm-mail-mode-map (make-sparse-keymap)))
    (define-key mail-mode-map "\C-c\C-q" 'from-the-parent)
    (define-key vm-mail-mode-map "\C-c\C-v" 'from-vm)
    (vm-mail-mode-parent-keymap)
    (vm-mail-mode-parent-keymap)
    ;; Neither keymap is a circular list.  `proper-list-p' answers nil for one,
    ;; and unlike `lookup-key' it returns either way.
    (should (proper-list-p vm-mail-mode-map))
    (should (proper-list-p mail-mode-map))
    ;; and the parenting did its job
    (should (eq mail-mode-map (keymap-parent vm-mail-mode-map)))
    (should (eq 'from-vm (lookup-key vm-mail-mode-map "\C-c\C-v")))
    (should (eq 'from-the-parent (lookup-key vm-mail-mode-map "\C-c\C-q")))
    ;; VM's own binding is not written into Mail mode's keymap
    (should-not (lookup-key mail-mode-map "\C-c\C-v"))))

(ert-deftest vm-reply-test-composing-parents-the-mail-keymap ()
  "Composing a message leaves Mail mode's bindings reachable.
The control for the test above, through the real keymaps: whatever the
mechanism, `C-c C-q' has to keep coming from Mail mode once VM has installed
its own map."
  (vm-reply-test--in-folder (vm-reply-test--one-message)
    (vm-mail-from-folder)
    (should (eq major-mode 'mail-mode))
    (should (eq mail-mode-map (keymap-parent vm-mail-mode-map)))
    (should (proper-list-p mail-mode-map))
    (should (commandp (lookup-key vm-mail-mode-map "\C-c\C-q")))))

(provide 'vm-reply-test)

;;; vm-reply-test.el ends here
