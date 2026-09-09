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

;;; vm-mail-to-mailto-url tests

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
Includes the XEmacs form quoted on the issue: VM no longer supports XEmacs,
but the parsing is of whatever `emacs-version' returns and should not depend
on which editor wrote it."
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
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          ;; VM counts compositions for the mode line; with none left behind the
          ;; count should not follow the test out, and `vm-ml-composition-buffer-count'
          ;; does not go back to "" on its own -- `vm-update-ml-composition-buffer-count'
          ;; writes "0 compositions", which the mode line only shows while
          ;; `vm-compositions-exist'.
          (vm-composition-buffer-count vm-composition-buffer-count)
          (vm-ml-composition-buffer-count vm-ml-composition-buffer-count)
          (vm-compositions-exist vm-compositions-exist)
          ;; `vm-warn' remembers its last warning so as not to repeat it, and a
          ;; composition here warns about the signature file it cannot read.
          (vm-current-warning vm-current-warning)
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
               ;; stdin and fails.  Nothing here is about drafts.  Only that hook goes:
               ;; VM's own `vm-forget-composition-buffer' is on the same hook,
               ;; and without it the composition counters never come back down.
               (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))
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
  (let ((advised (advice-member-p 'vm-pcrisis--mail 'vm-mail-from-folder)))
    (when advised (advice-remove 'vm-mail-from-folder 'vm-pcrisis--mail))
    (unwind-protect
        (vm-reply-test--in-folder ("")
          (should (null vm-message-list))
          (vm-mail-from-folder)
          (should (eq major-mode 'mail-mode))
          (should (string-match-p "^To:" (buffer-string))))
      (when advised (advice-add 'vm-mail-from-folder :around #'vm-pcrisis--mail)))))

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

(ert-deftest vm-reply-test-x-mailer-names-the-editor ()
  "The X-Mailer of a real composition says which editor built it.
Issue #520 was reported against the header, not against the helper that makes
part of it, so this looks at the header: \"VM 8.3.x under 31.0.50\" was what
the reporter saw, and the editor\='s name was the missing part.  It also has to
stay short -- `emacs-version\=' the function follows the version with a build
number, a platform and a date, none of which belongs in a header."
  (vm-reply-test--in-folder (vm-reply-test--one-message)
    (vm-mail-from-folder)
    (goto-char (point-min))
    (should (re-search-forward "^X-Mailer: .*$" nil t))
    (let ((header (match-string 0)))
      (should (string-match-p "\\`X-Mailer: VM " header))
      (should (string-match-p " under GNU Emacs [0-9]" header))
      ;; the platform, in parentheses, is the last of it
      (should (string-match-p (concat " (" (regexp-quote system-configuration) ")\\'")
                              header))
      ;; and none of the rest of what `emacs-version' returns
      (should-not (string-match-p "build\\|of [0-9][0-9][0-9][0-9]\\|appkit\\|GTK"
                                  header)))))


;;; Drag and drop into a composition (#531)

(ert-deftest vm-reply-test-composition-takes-drops-the-portable-way ()
  "A composition installs VM\='s handlers in `dnd-protocol-alist\='.
This is how a dropped file becomes an attachment, and it is what replaced
the `[ns-drag-file]\=' binding VM carried for Mac and NextStep -- whose own
comment said to remove it once this existed.  Removed in #531, after the
maintainer confirmed on a Mac that dropping a file on a composition attaches
it."
  (vm-reply-test--in-folder (vm-reply-test--one-message)
    (vm-mail-from-folder)
    (should (local-variable-p 'dnd-protocol-alist))
    (dolist (entry vm-dnd-protocol-alist)
      (should (member entry dnd-protocol-alist)))
    (should (assoc "^file:" vm-dnd-protocol-alist))))

(ert-deftest vm-reply-test-no-nextstep-drag-binding ()
  "REGRESSION: the Mac/NextStep drag binding is gone, and so is its command.
Modern Emacs dispatches a drop through `dnd-protocol-alist\=' on every window
system, NS included, so `[ns-drag-file]\=' was a second path that no longer
ran -- and `vm-ns-attach-file\=' read `ns-input-file\=', which nothing sets any
more."
  (should-not (lookup-key vm-mail-mode-map [ns-drag-file]))
  (should-not (fboundp 'vm-ns-attach-file)))

;;; VM files its own Fcc copies (issue #597)

;; `mail-do-fcc' wrote one format whatever the folder was: `\nFrom ' quoted
;; to `>From ' always, and never a `Content-Length'.  So an Fcc into a
;; mboxcl2 folder appended a message the byte counts did
;; not describe, and the folder stopped reading back the way it was written.

(defmacro vm-reply-test--with-composition (fcc &rest body)
  "Run BODY in a composition buffer with an Fcc header naming FCC.
The body holds a line beginning `From ', which is the line every one of
these is about."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-fcc" t))))
     (unwind-protect
         (with-temp-buffer
           (insert "To: someone@example.com\n"
                   "Subject: filed\n"
                   "Fcc: " ,fcc "\n"
                   mail-header-separator "\n"
                   "a body line\n"
                   "From nobody@example.com Mon Jan  1 00:00:00 2024\n"
                   "the last line\n")
           ,@body)
       (delete-directory dir t))))

(defun vm-reply-test--folder-text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest vm-reply-test-fcc-quotes-for-a-From_-folder ()
  "Into a From_ folder the copy is quoted, because the boundary is a line."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir)))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\nSubject: filed\n"
                  "Fcc: " folder "\n" mail-header-separator "\n"
                  "a body line\n"
                  "From nobody@example.com Mon Jan  1 00:00:00 2024\n")
          (let ((vm-default-folder-type 'From_))
            (vm-do-fcc-in-composition))
          ;; the separator was taken out to make the copy, and put back
          (should (string-match-p (regexp-quote mail-header-separator)
                                  (buffer-string))))
      (let ((text (vm-reply-test--folder-text (expand-file-name "archive" dir))))
        ;; the envelope line names the sender, not VM (emacs-vm/vm#611)
        (should (string-match-p "^From [^ ]+@[^ ]+ " text))
        (should (string-match-p "^>From nobody@example.com" text))
        (should-not (string-match-p "^Content-Length:" text)))
      (delete-directory dir t))))

(defun vm-reply-test--fcc (folder body)
  "File a composition with BODY in FOLDER, through the Fcc header."
  (with-temp-buffer
    (insert "To: someone@example.com\nSubject: filed\n"
            "Fcc: " folder "\n" mail-header-separator "\n" body)
    (vm-do-fcc-in-composition)))

(defun vm-reply-test--parse-folder (folder &optional lengths)
  "The messages FOLDER holds, as a list of body strings.
LENGTHS non-nil reads it as a folder whose messages carry a Content-Length."
  (with-temp-buffer
    (vm-test-init-folder-variables)
    (when lengths (setq-local vm-trust-content-length t))
    (insert-file-contents folder)
    (setq-local vm-folder-type (vm-get-folder-type folder))
    (goto-char (point-min))
    (vm-build-message-list)
    (mapcar (lambda (m)
              (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))
            vm-message-list)))

(ert-deftest vm-reply-test-fcc-ends-a-body-that-has-no-newline ()
  "A composition whose last line has no newline still files as one message.
The copy was written as it stood, so the folder lost the blank line before
the next envelope line and the two messages read back as one
(emacs-vm/vm#783).  A composition need not end with a newline; a message in
a folder does."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir)))
    (unwind-protect
        (let ((vm-default-folder-type 'From_))
          (vm-reply-test--fcc folder "first message, no newline at the end")
          (vm-reply-test--fcc folder "second message\n")
          (should (equal (vm-reply-test--parse-folder folder)
                         '("first message, no newline at the end\n"
                           "second message\n"))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-ends-a-body-that-has-no-newline-mboxcl2 ()
  "The same in an mboxcl2 folder, where the damage is worse.
That type ends a message by a byte count and has no trailing separator at
all, so the next envelope line was glued to the end of the previous body:
\"...no newline at the endFrom someone@...\".  The count written has to
cover the newline, which is why it is added before the count is taken."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive.mboxcl2" dir)))
    (unwind-protect
        (progn
          (vm-reply-test--fcc folder "first message, no newline at the end")
          (vm-reply-test--fcc folder "second message\n")
          (let ((text (vm-reply-test--folder-text folder)))
            ;; the second envelope line begins a line of its own
            (should (string-match-p "^From .*\nContent-Length: 15$" text))
            (should-not (string-match-p "the endFrom " text)))
          (should (equal (vm-reply-test--parse-folder folder t)
                         '("first message, no newline at the end\n"
                           "second message\n"))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-counts-for-a-Content-Length-folder ()
  "REGRESSION: a copy filed in a Content-Length folder carries a count.
`mail-do-fcc' never wrote one, whatever the folder was, so the byte counts
stopped describing the folder from that message on and it no longer read
back the way it was written.  The test is that it does read back: the folder
still parses as two messages, with the second one's body intact.

Quoting is not the point here; that the count is written is.  The folder type
no longer quotes at all (#466), and this same code followed it there without
further change."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir)))
    (unwind-protect
        (progn
          ;; An existing folder of that type, so the type is read, not guessed.
          (with-temp-buffer
            (insert "From VM Mon Jan  1 00:00:00 2024\n"
                    "Content-Length: 6\n"
                    "From: someone@example.com\n\n"
                    "first\n\n")
            (write-region (point-min) (point-max) folder))
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " folder "\n" mail-header-separator "\n"
                    "a body line\n"
                    "From nobody@example.com Mon Jan  1 00:00:00 2024\n")
            (let ((vm-trust-content-length t))
              (should (eq 'mboxcl2 (vm-get-folder-type folder)))
              (vm-do-fcc-in-composition)))
          ;; A count was written at all -- this is what was missing.
          (should (= 2 (cl-count-if
                        (lambda (l) (string-prefix-p "Content-Length:" l))
                        (split-string (vm-reply-test--folder-text folder)
                                      "\n"))))
          ;; And it is the right count: the folder reads back as two.
          (with-temp-buffer
            (vm-test-init-folder-variables)
            (setq-local vm-trust-content-length t)
            (insert-file-contents folder)
            (goto-char (point-min))
            (vm-build-message-list)
            (dolist (m vm-message-list) (vm-test-init-message-data m))
            (should (eq 'mboxcl2 vm-folder-type))
            (should (= 2 (length vm-message-list)))
            (should (string-match-p
                     "a body line"
                     (buffer-substring (vm-text-of (nth 1 vm-message-list))
                                       (vm-text-end-of (nth 1 vm-message-list)))))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-files-one-copy-per-header ()
  "Every Fcc header gets a copy, and each folder gets exactly one."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (one (expand-file-name "one" dir))
         (two (expand-file-name "two" dir)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " one "\n"
                    "Fcc: " two "\n"
                    mail-header-separator "\nbody\n")
            (let ((vm-default-folder-type 'From_))
              (vm-do-fcc-in-composition)))
          (dolist (file (list one two))
            (should (file-exists-p file))
            (should (= 1 (cl-count-if
                          (lambda (l) (string-prefix-p "From " l))
                          (split-string (vm-reply-test--folder-text file)
                                        "\n"))))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-imap-maildrop-is-not-a-file-name ()
  "REGRESSION: an Fcc naming an IMAP maildrop goes to the server, not to disk.
Issue #605.  The manual has always said an Fcc value may be \"the maildrop
specification of a folder on an IMAP server\", and `vm-fcc-write' took every
value as a file name -- so the sent copy went into a file called
imap:mail.example.com:143:inbox:login:user:* in whatever the default
directory was, silently."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (spec "imap:mail.example.com:143:inbox:login:user:*")
         (appended nil))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-fcc-write-imap)
                   (lambda (folder) (push folder appended))))
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " spec "\n"
                    mail-header-separator "\nbody\n")
            (let ((default-directory dir)
                  (vm-default-folder-type 'From_))
              (vm-do-fcc-in-composition)))
          (should (equal appended (list spec)))
          (should (equal nil (directory-files dir nil "[^.]"))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-tells-a-maildrop-from-a-file ()
  "A file whose name merely mentions imap is still a file.
`vm-imap-folder-spec-p' is what VM uses everywhere else to make this
distinction, and it is the one used here, so the two agree."
  (should (vm-imap-folder-spec-p "imap:host:143:inbox:login:user:*"))
  (should (vm-imap-folder-spec-p "imap-ssl:host:993:inbox:login:user:*"))
  (should-not (vm-imap-folder-spec-p "~/Mail/imap-notes"))
  (should-not (vm-imap-folder-spec-p "/var/mail/imap")))

(ert-deftest vm-reply-test-fcc-imap-needs-a-mailbox ()
  "A maildrop specification with no mailbox in it is refused, not guessed at."
  (with-temp-buffer
    (insert "To: someone@example.com\nSubject: filed\n\nbody\n")
    (should-error (vm-fcc-write-imap "imap:host:143::login:user:*"))))

(ert-deftest vm-reply-test-fcc-header-stays-in-the-composition ()
  "The composition keeps its Fcc headers; only the copies lose them.
The buffer VM leaves you with should still say where the copy went, and
editing it and sending again should file it again.  What must not carry an
Fcc header is the message that goes out -- it names a folder on this
machine -- and the copy that is filed, which does not need to say where it
was filed."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir)))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\n"
                  "Fcc: " folder "\n"
                  "Subject: filed\n"
                  mail-header-separator "\nbody\n")
          (let ((vm-default-folder-type 'From_))
            (vm-do-fcc-in-composition))
          ;; still there, and still a header
          (should (string-match-p (concat "^Fcc: " (regexp-quote folder) "$")
                                  (buffer-string)))
          (goto-char (point-min))
          (should (< (save-excursion (re-search-forward "^Fcc:"))
                     (save-excursion
                       (re-search-forward
                        (concat "^" (regexp-quote mail-header-separator) "$")))))
          ;; but the filed copy does not carry it
          (should-not (string-match-p
                       "^Fcc:" (vm-reply-test--folder-text folder)))
          ;; and sending again files again
          (let ((vm-default-folder-type 'From_))
            (vm-do-fcc-in-composition))
          (should (= 2 (cl-count-if
                        (lambda (l) (string-prefix-p "From " l))
                        (split-string (vm-reply-test--folder-text folder)
                                      "\n")))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-strip-headers-takes-them-out ()
  "`vm-fcc-strip-headers' removes every Fcc header and reports the folders.
This is what stands in for `mail-do-fcc' while the message is sent: the one
part of its job still worth doing is keeping the Fcc header out of what goes
to the recipient, and nothing else in sendmail.el or smtpmail.el does that."
  (with-temp-buffer
    (insert "To: someone@example.com\n"
            "Fcc: /one\n"
            "Subject: filed\n"
            "Fcc: /two\n"
            "\nbody\n")
    (goto-char (point-min))
    (let ((header-end (save-excursion (re-search-forward "^$") (point-marker))))
      (should (equal '("/one" "/two") (vm-fcc-strip-headers header-end))))
    (should-not (string-match-p "^Fcc:" (buffer-string)))
    (should (string-match-p "^Subject: filed$" (buffer-string)))))

(ert-deftest vm-reply-test-fcc-into-a-visited-folder ()
  "A folder VM is visiting is appended to in its buffer, not behind its back.
Writing the file under a live folder buffer would leave the two disagreeing
until someone reverted.  The branch has its own bookkeeping -- the message
count and the undo records -- so it is worth exercising rather than assuming."
  (vm-test-with-real-folder (2)
    (let ((folder buffer-file-name)
          (before (length vm-message-list))
          (folder-buffer (current-buffer)))
      (with-temp-buffer
        (insert "To: someone@example.com\nSubject: filed\n"
                "Fcc: " folder "\n" mail-header-separator "\nbody\n")
        (vm-do-fcc-in-composition))
      (with-current-buffer folder-buffer
        ;; it went into the buffer, and VM counted it
        (should (= (1+ before) (length vm-message-list)))
        (should (string-match-p "^Subject: filed$"
                                (save-restriction (widen) (buffer-string))))
        ;; and the file on disk was not written behind the buffer's back,
        ;; which would leave the two disagreeing until someone reverted
        (should (= 2 (with-temp-buffer
                       (insert-file-contents folder)
                       (cl-count-if (lambda (l) (string-prefix-p "From " l))
                                    (split-string (buffer-string) "\n")))))))))

(ert-deftest vm-reply-test-fcc-refuses-a-folder-it-cannot-read ()
  "A folder whose type VM does not recognize is not written to.
Appending to it in some other format is how a folder gets two formats in it."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "junk" dir)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "this is not a mail folder at all\n")
            (write-region (point-min) (point-max) folder))
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " folder "\n" mail-header-separator "\nbody\n")
            (let ((text-quoting-style 'grave))
              (should-error (vm-do-fcc-in-composition) :type 'error)))
          ;; and it was left as it was
          (should (equal "this is not a mail folder at all\n"
                         (vm-reply-test--folder-text folder))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-is-filed-once-through-the-send ()
  "Sending files exactly one copy, and the sent message has no Fcc header.
This is the wiring rather than the parts: `vm-mail-send' files the copy
itself and then binds `mail-do-fcc' to something that only strips the
header, because the real one would file a second copy.  `mail-send' is
stubbed here with something that does what the send functions do -- copy the
message and call `mail-do-fcc' on the copy."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir))
         (sent nil))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\nSubject: filed\n"
                  "Fcc: " folder "\n" mail-header-separator "\nbody\n")
          (let ((vm-default-folder-type 'From_)
                (composition (current-buffer)))
            (cl-letf (((symbol-function 'mail-send)
                       (lambda ()
                         ;; what sendmail-send-it and smtpmail-send-it do
                         (with-temp-buffer
                           (insert-buffer-substring composition)
                           (goto-char (point-min))
                           (re-search-forward
                            (concat "^" (regexp-quote mail-header-separator)
                                    "$"))
                           (replace-match "")
                           (mail-do-fcc (point-marker))
                           (setq sent (buffer-string))))))
              (vm-do-fcc-in-composition)
              (cl-letf (((symbol-function 'mail-do-fcc)
                         #'vm-fcc-strip-headers))
                (mail-send))))
          ;; one copy filed, not two
          (should (= 1 (cl-count-if
                        (lambda (l) (string-prefix-p "From " l))
                        (split-string (vm-reply-test--folder-text folder)
                                      "\n"))))
          ;; the message that went out does not name the folder
          (should sent)
          (should-not (string-match-p "^Fcc:" sent))
          (should (string-match-p "^Subject: filed$" sent))
          ;; the composition still does
          (should (string-match-p "^Fcc:" (buffer-string))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-counts-octets-not-characters ()
  "REGRESSION: the count is of octets, so a non-ASCII body still reads back.
A `Content-Length' counts octets, and so does the reader -- `vm-visit-folder'
makes a folder buffer unibyte, so its `forward-char' moves over bytes.  The
copy is built in a multibyte buffer, though, so counting characters there
was short by however much of the body was not ASCII, and every message after
it in the folder was misplaced."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir))
         (body "grüße von René: 日本語\n"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "From VM Mon Jan  1 00:00:00 2024\n"
                    "Content-Length: 6\n"
                    "From: someone@example.com\n\nfirst\n\n")
            (write-region (point-min) (point-max) folder))
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " folder "\n" mail-header-separator "\n" body)
            (let ((vm-trust-content-length t)
                  (coding-system-for-write 'utf-8-unix))
              (vm-do-fcc-in-composition)))
          ;; the count is the octet length, which is more than the characters
          (let ((text (vm-reply-test--folder-text folder)))
            (should (string-match "\nContent-Length: \\([0-9]+\\)\nTo: "
                                  text))
            (should (= (string-bytes body)
                       (string-to-number (match-string 1 text))))
            (should (< (length body) (string-bytes body))))
          ;; and the folder reads back as two messages with the body intact
          (with-temp-buffer
            (vm-test-init-folder-variables)
            (setq-local vm-trust-content-length t)
            (let ((coding-system-for-read 'utf-8-unix))
              (insert-file-contents folder))
            (goto-char (point-min))
            (vm-build-message-list)
            (dolist (m vm-message-list) (vm-test-init-message-data m))
            (should (= 2 (length vm-message-list)))
            (should (string-match-p
                     "grüße von René"
                     (buffer-substring (vm-text-of (nth 1 vm-message-list))
                                       (vm-text-end-of (nth 1 vm-message-list)))))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-second-send-files-again ()
  "REGRESSION: sending a kept composition again files another copy.
VM keeps the composition buffer after a send, so this is the ordinary way to
correct a message and send it once more.  `vm-fcc-filed' is what stops the
copy being filed twice for one send; left set afterwards it would mean the
next send filed nowhere and said nothing.  Goes through `vm-mail-send'
rather than around it, because the clearing lives there: calling
`vm-do-fcc-in-composition' directly would pass either way."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir)))
    (unwind-protect
        (with-temp-buffer
          (insert "To: someone@example.com\nSubject: filed\n"
                  "Fcc: " folder "\n" mail-header-separator "\nbody\n")
          (let ((vm-default-folder-type 'From_)
                (vm-confirm-mail-send nil)
                (vm-mail-check-recipient-format nil)
                (vm-send-using-mime nil)
                (vm-mail-reorder-message-headers nil)
                (vm-mail-send-hook nil)
                (vm-system-state nil))
            (cl-letf (((symbol-function 'mail-send) #'ignore)
                      ((symbol-function 'vm-rename-current-mail-buffer)
                       #'ignore)
                      ((symbol-function 'vm-keep-mail-buffer) #'ignore)
                      ((symbol-function 'vm-display) #'ignore))
              (vm-mail-send)
              ;; Cleared at the end of the send, not at the start of the
              ;; next: a copy filed before the send began counts, which is
              ;; how an encrypting command's copy stops the send filing a
              ;; second one (emacs-vm/vm#784).
              (should-not vm-fcc-filed)
              ;; edit it and send it again
              (goto-char (point-max))
              (insert "a correction\n")
              (vm-mail-send)))
          (should (= 2 (cl-count-if
                        (lambda (l) (string-prefix-p "From " l))
                        (split-string (vm-reply-test--folder-text folder)
                                      "\n"))))
          ;; and the second copy is the edited one
          (should (string-match-p "a correction"
                                  (vm-reply-test--folder-text folder))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-into-mboxcl2-keeps-the-body-exact ()
  "REGRESSION: a copy filed in a Content-Length folder is stored unaltered.
The count says where the message ends, so a body line beginning `From ' is
left as the author wrote it -- which is mboxcl2, and the reason to keep a
folder in that format at all.  VM used to quote it as well as counting,
which is mboxcl and alters the message for no gain.  Issue #466.

Written and then read back, because the two have to agree: quoting changes
the body's length, so a writer that stopped quoting while the count still
included the quote would put every later message in the wrong place."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
         (folder (expand-file-name "archive" dir))
         (line "From nobody@example.com Mon Jan  1 00:00:00 2024\n"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "From VM Mon Jan  1 00:00:00 2024\n"
                    "Content-Length: 6\n"
                    "From: someone@example.com\n\nfirst\n\n")
            (write-region (point-min) (point-max) folder))
          (with-temp-buffer
            (insert "To: someone@example.com\nSubject: filed\n"
                    "Fcc: " folder "\n" mail-header-separator "\n"
                    "a body line\n" line)
            (let ((vm-trust-content-length t))
              (vm-do-fcc-in-composition)))
          (let ((text (vm-reply-test--folder-text folder)))
            (should (string-match-p (concat "\n" (regexp-quote line)) text))
            (should-not (string-match-p ">From nobody@example.com" text)))
          ;; and the count still finds the end of it
          (with-temp-buffer
            (vm-test-init-folder-variables)
            (setq-local vm-trust-content-length t)
            (insert-file-contents folder)
            (goto-char (point-min))
            (vm-build-message-list)
            (dolist (m vm-message-list) (vm-test-init-message-data m))
            (should (= 2 (length vm-message-list)))
            (should (string-match-p
                     (regexp-quote line)
                     (buffer-substring (vm-text-of (nth 1 vm-message-list))
                                       (vm-text-end-of (nth 1 vm-message-list)))))))
      (delete-directory dir t))))

;;; Composition options moved out of vm-rfaddons (issue #606)

(ert-deftest vm-reply-test-composition-options-replace-the-addon-flags ()
  "The vm-enable-addons flags are ordinary options now.
`check-for-empty-subject' and `encode-headers' were on by default through
that list; the first is an option that defaults to t and the second is not
optional at all."
  (should (get 'vm-check-recipients 'standard-value))
  (should (get 'vm-check-for-empty-subject 'standard-value))
  (should (get 'vm-clean-subject-prefixes 'standard-value))
  (should (get 'vm-open-line-in-quoted-text 'standard-value))
  (should (eq t (default-value 'vm-check-for-empty-subject)))
  ;; vm-enable-addons and the file it switched features on in are gone
  (should-not (boundp 'vm-enable-addons))
  (should-not (featurep 'vm-rfaddons)))

(ert-deftest vm-reply-test-apply-options-does-nothing-when-they-are-off ()
  "With the options off, setting a composition up changes nothing."
  (with-temp-buffer
    (insert "To: someone@example.com\nSubject: Re: Re: hello\n"
            mail-header-separator "\n")
    (let ((vm-clean-subject-prefixes nil)
          (vm-open-line-in-quoted-text nil)
          (before (buffer-string)))
      (vm-mail-mode-apply-options)
      (should (equal before (buffer-string)))
      (should-not (memq 'vm-mail-mode-open-line before-change-functions)))))

(ert-deftest vm-reply-test-open-line-is-installed-when-asked ()
  "`vm-open-line-in-quoted-text' installs the change hooks, buffer-locally."
  (with-temp-buffer
    (let ((vm-open-line-in-quoted-text t))
      (vm-mail-mode-apply-options)
      (should (memq 'vm-mail-mode-open-line before-change-functions))
      (should (memq 'vm-mail-mode-open-line after-change-functions))
      ;; buffer-local, not global
      (should (local-variable-p 'before-change-functions)))))

(ert-deftest vm-reply-test-a-date-of-your-own-is-kept ()
  "`vm-mail-mode-fake-date-p' keeps a Date header you wrote yourself.
This was advice on `vm-mail-mode-insert-date-maybe'; it is a test inside it."
  (with-temp-buffer
    (insert "To: someone@example.com\nDate: Wed, 01 Jan 2020 00:00:00 +0000\n"
            "Subject: hello\n" mail-header-separator "\n")
    (let ((vm-mail-header-insert-date t)
          (vm-mail-mode-fake-date-p t))
      (vm-mail-mode-insert-date-maybe)
      (should (= 1 (cl-count-if (lambda (l) (string-prefix-p "Date:" l))
                                (split-string (buffer-string) "\n"))))
      (should (string-match-p "01 Jan 2020" (buffer-string))))))

(ert-deftest vm-reply-test-a-date-is-replaced-when-not-faking ()
  "With the flag off, VM writes the Date itself."
  (with-temp-buffer
    (insert "To: someone@example.com\nDate: Wed, 01 Jan 2020 00:00:00 +0000\n"
            "Subject: hello\n" mail-header-separator "\n")
    (let ((vm-mail-header-insert-date t)
          (vm-mail-mode-fake-date-p nil))
      (vm-mail-mode-insert-date-maybe)
      (should-not (string-match-p "01 Jan 2020" (buffer-string))))))

;;; Checking recipients, from vm-rfaddons-test.el (issue #606)
;; The feature moved to vm-reply.el; these are its tests, unchanged apart
;; from their names.

(defmacro vm-reply-test-with-headers (headers &rest body)
  "Run BODY in a mail-mode buffer whose header section is HEADERS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,headers mail-header-separator "\n" "body\n")
     (let ((text-quoting-style 'grave))
       ,@body)))

(ert-deftest vm-reply-test-check-recipients-plain ()
  "Test that ordinary recipients pass."
  (vm-reply-test-with-headers
      "To: someone@example.com\nCC: a@example.com, b@example.org\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-reply-test-check-recipients-missing-comma ()
  "Test that a genuinely missing separator is still caught."
  (vm-reply-test-with-headers
      "To: first@example.com second@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-reply-test-check-recipients-missing-comma-in-cc ()
  "Test that the other recipient headers are checked too."
  (vm-reply-test-with-headers
      "To: fine@example.com\nCC: first@example.com second@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-reply-test-check-recipients-encoded-word ()
  "Test that an encoded word containing an address is not a missing comma.
Regression test for issue #417: the check looked for two \"@\" anywhere in
the header, so a display name that is a MIME encoded word holding an
address -- which Exchange and Outlook both produce -- blocked sending
with \"Missing separator\"."
  (vm-reply-test-with-headers
      (concat "To: Uday S Reddy "
              "=?utf-8?Q?=E2=80=8E[u.s.reddy@cs.bham.ac.uk]=E2=80=8E?="
              " <u.s.reddy@cs.bham.ac.uk>\n")
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-reply-test-check-recipients-quoted-at ()
  "Test that a quoted display name containing \"@\" is allowed."
  (vm-reply-test-with-headers
      "To: \"someone@elsewhere\" <someone@example.com>\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-reply-test-check-recipients-encoded-word-and-real-error ()
  "Test that an encoded word does not mask a real missing separator."
  (vm-reply-test-with-headers
      (concat "To: =?utf-8?Q?name?= <first@example.com>"
              " second@example.org\n")
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-reply-test-check-recipients-comment ()
  "Test that an RFC 5322 comment containing \"@\" does not block sending.
A parenthesised comment may hold anything, an address included, and the
check counted its \"@\" as a second address."
  (vm-reply-test-with-headers
      "To: a@example.com (the a@b guy)\n"
    (should (null (vm-mail-check-recipients))))
  (vm-reply-test-with-headers
      "To: Jane <jane@example.com> (jane@old)\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-reply-test-check-recipients-nested-comment ()
  "Test that nested comments are stripped too; RFC 5322 allows them."
  (vm-reply-test-with-headers
      "To: a@example.com (outer (inner b@c) still)\n"
    (should (null (vm-mail-check-recipients)))))

(ert-deftest vm-reply-test-check-recipients-comment-hides-nothing ()
  "Test that a comment does not mask a real missing separator."
  (vm-reply-test-with-headers
      "To: a@example.com (note) b@example.org\n"
    (should-error (vm-mail-check-recipients) :type 'error)))

(ert-deftest vm-reply-test-check-recipients-percent-in-address ()
  "Test that a \"%\" in an address does not break the error message.
The message has the address interpolated into it and was passed to
`error' as the format string, so \"%\" -- legal in a local part, and
used by percent-hack routing -- gave \"Not enough arguments for format
string\" instead of the missing-separator complaint."
  (vm-reply-test-with-headers
      "To: a%s@example.com b%d@example.org\n"
    (let ((err (should-error (vm-mail-check-recipients) :type 'error)))
      (should (string-match "Missing separator" (cadr err))))))

(ert-deftest vm-reply-test-check-recipients-strip ()
  "Test the helper that removes the parts allowed to contain \"@\"."
  (should (equal (vm-mail-check-recipients-strip
                  "=?utf-8?Q?a@b?= <c@d.example>")
                 " <c@d.example>"))
  (should (equal (vm-mail-check-recipients-strip
                  "\"a@b\" <c@d.example>")
                 " <c@d.example>"))
  (should (equal (vm-mail-check-recipients-strip "c@d.example")
                 "c@d.example")))

;;; What these do, in place of tests that they were bound.

(ert-deftest vm-reply-test-add-reply-subject-prefix-prefixes-the-body ()
  "Every line of the included text is prefixed, after the attribution line.
The name says subject, but the function is what quotes a reply."
  (with-temp-buffer
    (insert "To: someone@example.com\n" mail-header-separator "\n"
            "First line.\nSecond line.\n")
    (let ((vm-included-text-prefix "> ")
          (vm-included-text-attribution-format nil))
      (vm-add-reply-subject-prefix nil)
      (should (string-match-p "^> First line\\.$" (buffer-string)))
      (should (string-match-p "^> Second line\\.$" (buffer-string)))
      ;; and the headers are left alone
      (should (string-match-p "^To: someone@example.com$" (buffer-string))))))

(ert-deftest vm-reply-test-add-reply-subject-prefix-writes-the-attribution ()
  "With a message and an attribution format, the attribution goes in first
and is not itself quoted: it is the reply's own line, not included text."
  (with-temp-buffer
    (insert "To: someone@example.com\n" mail-header-separator "\nBody.\n")
    (cl-letf (((symbol-function 'vm-summary-sprintf)
               (lambda (_fmt _m) "Alice wrote:\n")))
      (let ((vm-included-text-prefix "> ")
            (vm-included-text-attribution-format "%F wrote:\n"))
        (vm-add-reply-subject-prefix 'a-message)
        (should (string-match-p "^Alice wrote:$" (buffer-string)))
        (should (string-match-p "^> Body\\.$" (buffer-string)))))))

(ert-deftest vm-reply-test-mailto-url-becomes-the-composition-it-names ()
  "A mailto URL is parsed into the fields it names, decoding %-escapes.
This is the entry point emacsclient hands a clicked link to."
  (let (args)
    (cl-letf (((symbol-function 'vm-session-initialization) #'ignore)
              ((symbol-function 'vm-check-for-killed-folder) #'ignore)
              ((symbol-function 'vm-select-folder-buffer-if-possible) #'ignore)
              ((symbol-function 'vm-check-for-killed-summary) #'ignore)
              ((symbol-function 'vm-mail-mode-apply-options) #'ignore)
              ((symbol-function 'vm-mail-internal)
               (lambda (&rest a)
                 (setq args a)
                 (set-buffer (generate-new-buffer " *vm-reply-test-mailto*"))
                 (insert mail-header-separator "\n"))))
      (let ((vm-mail-hook nil) (vm-mail-mode-hook nil)
            (buffer nil))
        (unwind-protect
            (save-current-buffer
              (vm-mail-to-mailto-url
               "mailto:someone@example.com?subject=A%20subject&cc=other@example.com&body=Hello%20there")
              (setq buffer (current-buffer))
              (should (equal (plist-get args :to) "someone@example.com"))
              (should (equal (plist-get args :subject) "A subject"))
              (should (equal (plist-get args :cc) "other@example.com"))
              (should (string-match-p "Hello there" (buffer-string))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest vm-reply-test-mailto-url-inserts-a-header-it-does-not-know ()
  "A field that is not one of the known ones is inserted as a header."
  (let (buffer)
    (cl-letf (((symbol-function 'vm-session-initialization) #'ignore)
              ((symbol-function 'vm-check-for-killed-folder) #'ignore)
              ((symbol-function 'vm-select-folder-buffer-if-possible) #'ignore)
              ((symbol-function 'vm-check-for-killed-summary) #'ignore)
              ((symbol-function 'vm-mail-mode-apply-options) #'ignore)
              ((symbol-function 'vm-mail-internal)
               (lambda (&rest _)
                 (set-buffer (generate-new-buffer " *vm-reply-test-mailto*"))
                 (insert mail-header-separator "\n"))))
      (let ((vm-mail-hook nil) (vm-mail-mode-hook nil))
        (unwind-protect
            (save-current-buffer
              (vm-mail-to-mailto-url
               "mailto:someone@example.com?reply-to=third@example.com")
              (setq buffer (current-buffer))
              (should (string-match-p "^Reply-to: third@example.com$"
                                      (buffer-string))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest vm-reply-test-composition-buffers-are-counted ()
  "The mode line count goes up with a new composition and down when it goes.
`vm-compositions-exist' is what the folder's mode line reads."
  (let ((vm-composition-buffer-count 0)
        (vm-compositions-exist nil))
    (cl-letf (((symbol-function 'vm-update-ml-composition-buffer-count)
               #'ignore))
      (with-temp-buffer
        (vm-new-composition-buffer)
        (should (= vm-composition-buffer-count 1))
        (should vm-compositions-exist)
        ;; the buffer takes itself off the count when it is killed or sent
        (should (memq 'vm-forget-composition-buffer kill-buffer-hook))
        (should (memq 'vm-forget-composition-buffer vm-mail-send-hook))
        (vm-new-composition-buffer)
        (should (= vm-composition-buffer-count 2))
        (vm-forget-composition-buffer)
        (should (= vm-composition-buffer-count 1))
        (should vm-compositions-exist)
        (vm-forget-composition-buffer)
        (should (= vm-composition-buffer-count 0))
        (should-not vm-compositions-exist)))))

(ert-deftest vm-reply-test-composition-buffer-is-renamed-for-its-recipient ()
  "A composition is named after who it is to and what it is about.
Several recipients get an ellipsis, and a reply is named differently from
fresh mail."
  (cl-letf (((symbol-function 'vm-sanitize-buffer-name) #'identity))
    (with-temp-buffer
      (rename-buffer "mail to nobody" t)
      (setq major-mode 'mail-mode)
      (insert "To: Alice Adams <alice@example.com>\n"
              "Subject: the subject\n" mail-header-separator "\n")
      (let ((vm-reply-list nil))
        (vm-update-composition-buffer-name)
        (should (equal (buffer-name) "mail to Alice Adams on \"the subject\"")))
      ;; a second recipient is shown as an ellipsis rather than a list
      (goto-char (point-min))
      (insert "Cc: Bob <bob@example.com>\n")
      (let ((vm-reply-list nil))
        (vm-update-composition-buffer-name)
        (should (equal (buffer-name)
                       "mail to Alice Adams, ... on \"the subject\"")))
      ;; a reply is named without the subject
      (let ((vm-reply-list '(a-message)))
        (vm-update-composition-buffer-name)
        (should (equal (buffer-name) "reply to Alice Adams, ..."))))))

(ert-deftest vm-reply-test-composition-buffer-name-left-alone-elsewhere ()
  "Only VM's own composition buffers are renamed: the name is the marker.
A user's own buffer called something else keeps its name, and so does any
buffer that is not in mail mode."
  (with-temp-buffer
    (rename-buffer "notes on mail" t)
    (setq major-mode 'mail-mode)
    (insert "To: alice@example.com\n" mail-header-separator "\n")
    (let ((name (buffer-name)))
      (vm-update-composition-buffer-name)
      (should (equal (buffer-name) name))))
  (with-temp-buffer
    (rename-buffer "mail to nobody" t)
    (setq major-mode 'text-mode)
    (let ((name (buffer-name)))
      (vm-update-composition-buffer-name)
      (should (equal (buffer-name) name)))))

(ert-deftest vm-reply-test-fcc-makes-a-new-folder-in-the-type-its-name-asks-for ()
  "An Fcc to a file that does not exist yet is written in the type its name
asks for, so a folder called .mboxcl2 gets a Content-Length from the first
message on (emacs-vm/vm#610).  It used to be written as a From_ folder
whatever it was called, and then read back as one, so the name was a lie that
never came true."
  (let ((dir (file-name-as-directory (make-temp-file "vm-reply-fcc" t))))
    (unwind-protect
        (let ((named (expand-file-name "sent.mboxcl2" dir))
              (plain (expand-file-name "sent.mbox" dir))
              (vm-default-folder-type 'From_)
              (vm-trust-content-length nil))
          (dolist (folder (list named plain))
            (with-temp-buffer
              (insert "To: someone@example.com\nSubject: one\n\nA body line.\n")
              (vm-fcc-write folder)))
          (with-temp-buffer
            (insert-file-contents named)
            (should (string-match-p "^Content-Length: 13$" (buffer-string))))
          (should (eq (vm-get-folder-type named) 'mboxcl2))
          ;; a name that asks for nothing still follows vm-default-folder-type
          (with-temp-buffer
            (insert-file-contents plain)
            (should-not (string-match-p "Content-Length:" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-fcc-envelope-line-names-the-sender-and-the-date ()
  "A filed copy's envelope line names the address the message is from and
carries the message's own Date.  Every other writer of an mbox puts an
addr-spec there; VM used to write its own name and the moment of filing, so
the line recorded neither fact (emacs-vm/vm#611)."
  (let ((user-mail-address "me@example.com"))
    (with-temp-buffer
      (insert "To: someone@example.com\n"
              "From: Alice Adams <alice@example.com>\n"
              "Date: Sat, 8 Aug 2026 14:24:13 -0700\n"
              "Subject: dated\n\nBody.\n")
      (should (equal (vm-fcc-leading-separator 'From_)
                     "From alice@example.com Sat Aug  8 14:24:13 2026\n"))
      (should (equal (vm-fcc-leading-separator 'mboxcl2)
                     "From alice@example.com Sat Aug  8 14:24:13 2026\n")))
    ;; a composition often has no From header -- the MTA adds one -- and the
    ;; copy is of your own outgoing mail, so you are its sender
    (with-temp-buffer
      (insert "To: someone@example.com\n"
              "Date: Sat, 8 Aug 2026 14:24:13 -0700\n\nBody.\n")
      (should (equal (vm-fcc-leading-separator 'From_)
                     "From me@example.com Sat Aug  8 14:24:13 2026\n")))
    ;; a From with a name and no address cannot be an envelope sender
    (with-temp-buffer
      (insert "To: someone@example.com\nFrom: Alice Adams\n\nBody.\n")
      (should (string-prefix-p "From me@example.com "
                               (vm-fcc-leading-separator 'From_))))
    ;; and with nothing to go on, VM names itself as it always did
    (let ((user-mail-address nil))
      (with-temp-buffer
        (insert "To: someone@example.com\n\nBody.\n")
        (should (string-prefix-p "From VM "
                                 (vm-fcc-leading-separator 'From_)))))
    ;; no Date header, or one that cannot be read: the time of filing
    (with-temp-buffer
      (insert "To: someone@example.com\nFrom: alice@example.com\n\nBody.\n")
      (should (string-prefix-p "From alice@example.com "
                               (vm-fcc-leading-separator 'From_))))
    (with-temp-buffer
      (insert "To: someone@example.com\nDate: whenever\n\nBody.\n")
      (should (string-prefix-p "From me@example.com "
                               (vm-fcc-leading-separator 'From_))))
    ;; headers are headers: a Date or From in the body is not the message's
    (with-temp-buffer
      (insert "To: someone@example.com\n\nFrom: bob@example.com\n"
              "Date: Sat, 8 Aug 2026 14:24:13 -0700\n")
      (let ((line (vm-fcc-leading-separator 'From_)))
        (should (string-prefix-p "From me@example.com " line))
        (should-not (string-match-p "Aug  8" line))))
    ;; a format with no From_ line is untouched
    (with-temp-buffer
      (insert "To: someone@example.com\nFrom: alice@example.com\n\nB\n")
      (should (equal (vm-fcc-leading-separator 'mmdf) "\001\001\001\001\n")))))

(ert-deftest vm-reply-test-fcc-writes-that-envelope-line ()
  "The filed copy on disk carries that line, not the time of filing.
The test above checks what the function returns; this one checks that the
write path is the caller, which is the part a wiring mistake breaks."
  (let ((dir (file-name-as-directory (make-temp-file "vm-reply-fcc-date" t))))
    (unwind-protect
        (let ((folder (expand-file-name "sent.mbox" dir))
              (user-mail-address "me@example.com")
              (vm-default-folder-type 'From_))
          (with-temp-buffer
            (insert "To: someone@example.com\n"
                    "From: Alice Adams <alice@example.com>\n"
                    "Date: Sat, 8 Aug 2026 14:24:13 -0700\n"
                    "Subject: dated\n\nBody.\n")
            (vm-fcc-write folder))
          (with-temp-buffer
            (insert-file-contents folder)
            (goto-char (point-min))
            (should (looking-at
                     "From alice@example.com Sat Aug  8 14:24:13 2026$"))))
      (delete-directory dir t))))

;;; Replying, following up and forwarding (emacs-vm/vm#629)
;;
;; `vm-reply', `vm-followup', their include-text halves and `vm-forward-message'
;; had no test between them: the composition they produce -- who it is to, what
;; it quotes, what threads it to the original -- was checked by hand or not at
;; all.

(defconst vm-reply-test--incoming
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: Alice Adams <alice@example.com>\n"
          "To: bob@example.com, me@example.com\n"
          "Cc: carol@example.com\n"
          "Message-ID: <one@example.com>\n"
          "Subject: badgers\n\n"
          "The body of the message.\n\n")
  "A message with several recipients, so a reply and a followup differ.")

(defconst vm-reply-test--mime-bounce
  (concat "From MAILER-DAEMON Sat Aug  8 16:00:00 2026\n"
          "From: Mail Delivery Subsystem <MAILER-DAEMON@example.com>\n"
          "To: me@example.com\nSubject: Returned mail: User unknown\n"
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"bnd\"\n\n"
          "--bnd\nContent-Type: text/plain\n\n"
          "Your message could not be delivered.\n\n"
          "--bnd\nContent-Type: message/rfc822\n\n"
          "From: me@example.com\nTo: nosuch@example.com\n"
          "Subject: the original\nMessage-ID: <orig@example.com>\n\n"
          "The original body.\n\n"
          "--bnd--\n\n")
  "A bounce that returns the message as a MIME attachment.")

(defconst vm-reply-test--plain-bounce
  (concat "From MAILER-DAEMON Sat Aug  8 16:00:00 2026\n"
          "From: Mail Delivery Subsystem <MAILER-DAEMON@example.com>\n"
          "To: me@example.com\nSubject: Returned mail: User unknown\n\n"
          "   ----- Transcript of session follows -----\n"
          "550 nosuch@example.com... User unknown\n\n"
          "   ----- Original message follows -----\n\n"
          "Received: from example.com by example.net\n"
          "From: me@example.com\nTo: nosuch@example.com\n"
          "Subject: the original\n\n"
          "The original body.\n\n")
  "A bounce that quotes the message as text, no MIME about it.")

(defmacro vm-reply-test--composing (spec &rest body)
  "Visit a folder of one message, select it, and run BODY.
SPEC is (FOLDER-VAR TEXT): the folder holds TEXT and FOLDER-VAR is bound to
its name, for a test that wants a second folder beside it.  BODY runs with
the folder current, and a composition it starts becomes the current buffer,
as it does interactively.  Everything the visits and compositions created is
killed afterwards."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "vm-composing" t)))
         (before (buffer-list)))
     (unwind-protect
         (let ((,(car spec) (expand-file-name "incoming" dir))
               ;; visiting a folder pushes onto these, and the isolation
               ;; restores values rather than list contents
               (vm-folder-history vm-folder-history)
               (vm-last-visit-folder vm-last-visit-folder)
               (vm-frame-per-composition nil)
               (vm-mutable-frame-configuration nil)
               (vm-mail-mode-hook nil)
               (vm-mail-hook nil)
               (vm-resend-bounced-message-hook nil)
               (mail-signature nil)
               (mail-setup-hook nil)
               (user-mail-address "me@example.com")
               (vm-included-text-prefix "> ")
               (vm-included-text-attribution-format nil))
           (write-region ,(cadr spec) nil ,(car spec) nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder ,(car spec))
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun vm-reply-test--header (name)
  "The contents of header NAME in the composition in the current buffer.
Continuation lines are included: VM folds a long recipient list one address
to a line, so reading only the first line of Cc reads only the first address."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (end (save-excursion
                 (re-search-forward
                  (concat "^" (regexp-quote mail-header-separator) "$") nil t))))
      (when (re-search-forward (concat "^" (regexp-quote name) ": *") end t)
        (let ((start (point)))
          (forward-line 1)
          (while (and (< (point) (or end (point-max)))
                      (looking-at "[ \t]"))
            (forward-line 1))
          (string-trim (buffer-substring-no-properties start (point))))))))

(ert-deftest vm-reply-test-a-reply-goes-to-the-author-alone ()
  "`vm-reply' addresses the author and nobody else, and threads the reply.
The other recipients are the difference between this and a followup, and the
In-Reply-To and References are what make a reply part of the thread."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-reply 1)
    (should (equal (vm-reply-test--header "To")
                   "Alice Adams <alice@example.com>"))
    (should-not (vm-reply-test--header "Cc"))
    (should (equal (vm-reply-test--header "Subject") "badgers"))
    (should (equal (vm-reply-test--header "In-Reply-To") "<one@example.com>"))
    (should (equal (vm-reply-test--header "References") "<one@example.com>"))
    ;; a reply without include-text quotes nothing
    (should-not (string-match-p "The body of the message" (buffer-string)))))

(ert-deftest vm-reply-test-a-followup-goes-to-everyone ()
  "`vm-followup' adds the other recipients of the message to the reply.
That is the whole difference between it and `vm-reply', which writes to the
author alone.  Your own address among them is not removed: VM leaves that to
`vm-reply-ignored-addresses', tested below."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-followup 1)
    (let ((all (concat (or (vm-reply-test--header "To") "") " "
                       (or (vm-reply-test--header "Cc") ""))))
      (should (string-match-p "alice@example.com" all))
      (should (string-match-p "bob@example.com" all))
      (should (string-match-p "carol@example.com" all))
      (should (string-match-p "me@example.com" all)))))

(ert-deftest vm-reply-test-ignored-addresses-are-dropped-from-a-followup ()
  "`vm-reply-ignored-addresses' keeps an address out of the reply.
Its use is to keep your own addresses out of a followup, so answering a
message you were a recipient of does not mail you a copy of your answer."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (let ((vm-reply-ignored-addresses '("me@example\\.com")))
      (vm-followup 1)
      (let ((all (concat (or (vm-reply-test--header "To") "") " "
                         (or (vm-reply-test--header "Cc") ""))))
        (should-not (string-match-p "me@example.com" all))
        (should (string-match-p "bob@example.com" all))
        (should (string-match-p "carol@example.com" all))))))

(ert-deftest vm-reply-test-include-text-quotes-the-message ()
  "The include-text commands quote the body, prefixed as the option says.
That is the whole difference between them and the plain ones."
  (vm-reply-test--composing (folder vm-reply-test--incoming)
    (vm-reply-include-text 1)
    (should (string-match-p "^> The body of the message\\.$" (buffer-string)))
    (should (equal (vm-reply-test--header "To")
                   "Alice Adams <alice@example.com>"))
    (set-buffer-modified-p nil)
    ;; and the followup half quotes it too, while addressing everyone
    (vm-visit-folder folder)
    (setq vm-message-pointer vm-message-list)
    (vm-followup-include-text 1)
    (should (string-match-p "^> The body of the message\\.$" (buffer-string)))
    (should (string-match-p "bob@example.com"
                            (concat (vm-reply-test--header "To") " "
                                    (vm-reply-test--header "Cc"))))))

(ert-deftest vm-reply-test-a-reply-subject-keeps-its-prefix-once ()
  "Replying to a reply does not stack another prefix on the subject.
`vm-reply-subject-prefix' is added only when it is not there already, so a
thread does not accumulate Re: Re: Re:."
  (vm-reply-test--composing (folder vm-reply-test--incoming)
    (let ((answered (expand-file-name "answered" (file-name-directory folder)))
          (vm-reply-subject-prefix "Re: "))
      ;; a subject without the prefix gets one
      (vm-reply 1)
      (should (equal (vm-reply-test--header "Subject") "Re: badgers"))
      (set-buffer-modified-p nil)
      ;; a subject that has one already is left alone
      (write-region (replace-regexp-in-string
                     "^Subject: badgers$" "Subject: Re: badgers"
                     vm-reply-test--incoming)
                    nil answered nil 'quiet)
      (vm-visit-folder answered)
      (setq vm-message-pointer vm-message-list)
      (vm-reply 1)
      (should (equal (vm-reply-test--header "Subject") "Re: badgers"))
      (set-buffer-modified-p nil))))

(ert-deftest vm-reply-test-forwarding-attaches-the-message ()
  "`vm-forward-message' forwards as MIME by default, so the message is an
attachment rather than text in the composition.  `vm-forwarding-digest-type'
is what chooses that, and its default is mime.
The subject names the sender, which is what tells a forward from a reply at
a glance, and the To is left empty for you to fill in: a forward is for
somebody else, not for the author."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (should (equal vm-forwarding-digest-type "mime"))
    (vm-forward-message)
    (should (string-match-p "message/rfc822" (buffer-string)))
    (should (string-match-p "Alice Adams"
                            (or (vm-reply-test--header "Subject") "")))
    (should (equal (vm-reply-test--header "To") ""))))

(ert-deftest vm-reply-test-forwarding-plain-sends-it-as-text ()
  "`vm-forward-message-plain' forwards the text rather than as an attachment,
so the words of the message are in the composition itself.  That is its
reason for existing beside `vm-forward-message'."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-forward-message-plain)
    (should (string-match-p "The body of the message" (buffer-string)))
    (should-not (string-match-p "message/rfc822" (buffer-string)))))

(ert-deftest vm-reply-test-yanking-a-message-into-a-composition ()
  "`vm-yank-message' pulls a folder's message into the composition at point.
It is bound to C-c C-y in a reply for exactly this, and the prefix it quotes
with is the same option the include-text commands use."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (let ((message (car vm-message-list)))
      (vm-mail)
      (goto-char (point-max))
      (vm-yank-message message)
      (should (string-match-p "^> The body of the message\\.$" (buffer-string)))
      (set-buffer-modified-p nil))))

(ert-deftest vm-reply-test-the-composition-is-a-mail-buffer-set-up-for-vm ()
  "A reply is left in a buffer VM's own commands work in.
`vm-mail-buffer' points back at the folder -- that is how C-c C-y knows which
folder to yank from -- and `vm-reply-list' records what is being answered, so
the message is marked replied when it is sent."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-reply 1)
    (should (eq major-mode 'mail-mode))
    (should (buffer-live-p vm-mail-buffer))
    (should (equal (mapcar #'vm-su-subject vm-reply-list) '("badgers")))
    (set-buffer-modified-p nil)))

(ert-deftest vm-reply-test-yanking-from-another-folder ()
  "`vm-yank-message-other-folder' quotes a message from a folder other than
the one the composition came from.  It reads the message number from the
minibuffer, so a test has to answer that prompt."
  (vm-reply-test--composing (folder vm-reply-test--incoming)
    (let ((other (expand-file-name "other" (file-name-directory folder))))
      (write-region (concat "From dave@example.com Sat Aug  8 15:00:00 2026\n"
                            "From: dave@example.com\nSubject: otters\n\n"
                            "Text from the other folder.\n\n")
                    nil other nil 'quiet)
      (vm-mail)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1"))
                ((symbol-function 'vm-summarize) #'ignore))
        (vm-yank-message-other-folder other))
      (should (string-match-p "^> Text from the other folder\\.$"
                              (buffer-string)))
      (set-buffer-modified-p nil))))

(ert-deftest vm-reply-test-resending-asks-for-a-new-recipient ()
  "`vm-resend-message' copies the message and offers a Resent-To to fill in.
The original headers come along -- that is what makes it a resend rather than
a fresh message -- and the empty Resent-To is what its docstring says you must
fill in for the result to mean anything."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-resend-message)
    (should (equal (vm-reply-test--header "Resent-To") ""))
    (should (equal (vm-reply-test--header "From")
                   "Alice Adams <alice@example.com>"))
    (should (equal (vm-reply-test--header "Subject") "badgers"))
    (set-buffer-modified-p nil)))

(ert-deftest vm-reply-test-a-digest-holds-the-folder ()
  "`vm-send-digest' packs the messages into one composition.
Sending the whole folder is confirmed first, since it is rarely what a stray
keystroke meant, and the subject counts what went in."
  (vm-reply-test--composing (folder vm-reply-test--incoming)
    (let ((two (expand-file-name "two" (file-name-directory folder))))
      (write-region (concat vm-reply-test--incoming
                            "From dave@example.com Sat Aug  8 15:00:00 2026\n"
                            "From: dave@example.com\nSubject: otters\n\n"
                            "A second message.\n\n")
                    nil two nil 'quiet)
      (vm-visit-folder two)
      (setq vm-message-pointer vm-message-list)
      (should (= (length vm-message-list) 2))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (vm-send-digest))
      (should (string-match-p "Alice Adams"
                              (or (vm-reply-test--header "Subject") "")))
      (should (string-match-p "and 1 more message"
                              (or (vm-reply-test--header "Subject") "")))
      (should (= (length vm-forward-list) 2))
      (set-buffer-modified-p nil))))

(ert-deftest vm-reply-test-a-digest-refused-sends-nothing ()
  "Answering no to the whole-folder question aborts, rather than digesting
the folder anyway."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (vm-send-digest)))))

(ert-deftest vm-reply-test-previewing-a-composition-shows-it-as-a-folder ()
  "`vm-preview-composition' encodes a copy of the composition and reads it
back as a one-message folder.  The copy is what is encoded: the buffer being
composed must come back untouched, or previewing would cost you your message.

The command used to signal `end-of-buffer', which this test tolerated and
put down to there being no window in batch.  That was wrong: the preview is
an mmdf folder, and no mmdf folder could be read (emacs-vm/vm#786).  It now
runs to the end and narrows to the message, as VM narrows any preview, so
the body is read with the restriction lifted."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (vm-reply 1)
    (goto-char (point-max))
    (insert "Some text to preview.\n")
    (let ((composition (current-buffer))
          (before (buffer-string)))
      (vm-preview-composition)
      (should (get-buffer "composition preview"))
      (with-current-buffer "composition preview"
        (should (eq major-mode 'vm-mode))
        (should (= (length vm-message-list) 1))
        ;; narrowed to the message it is showing, as a preview is
        (should (< (point-min) (point-max)))
        (save-restriction
          (widen)
          ;; what the reader would see: encoded, and with the headers a
          ;; composition does not carry yet
          (should (string-match-p "Some text to preview" (buffer-string)))
          (should (string-match-p "MIME-Version: 1.0" (buffer-string)))
          (should-not (string-match-p (regexp-quote mail-header-separator)
                                      (buffer-string)))))
      (with-current-buffer composition
        (should (equal (buffer-string) before))
        (set-buffer-modified-p nil)))))

(ert-deftest vm-reply-test-previewing-outside-a-composition-is-refused ()
  "`vm-preview-composition' is a Mail mode command and says so, rather than
building a folder out of whatever buffer it was called in."
  (with-temp-buffer
    (fundamental-mode)
    (let ((text-quoting-style 'grave))
      (should (equal (cadr (should-error (vm-preview-composition)))
                     "Command must be used in a VM Mail mode buffer.")))))

(ert-deftest vm-reply-test-filling-long-lines-in-a-reply ()
  "`vm-fill-long-lines-in-reply' fills the body, and only the body.
The headers are not paragraphs to be reflowed: filling a long To across lines
by this route would break it."
  (let ((vm-fill-paragraphs-containing-long-lines-in-reply 40)
        (vm-fill-long-lines-in-reply-column 40)
        (vm-word-wrap-paragraphs-in-reply nil))
    (with-temp-buffer
      (mail-mode)
      (insert "To: " (make-string 60 ?a) "@example.com\n"
              mail-header-separator "\n"
              (string-join (make-list 40 "word") " ") "\n")
      (vm-fill-long-lines-in-reply)
      (goto-char (point-min))
      (should (looking-at (concat "To: " (make-string 60 ?a) "@example.com$")))
      (let ((body (buffer-substring (progn (mail-text) (point)) (point-max))))
        (should (string-match-p "word\nword" body))
        (should (< (apply #'max (mapcar #'length (split-string body "\n")))
                   60))))))

(ert-deftest vm-reply-test-citation-clean-up-cuts-doubly-cited-text ()
  "`vm-mail-mode-citation-clean-up' replaces a block of doubly-cited text
with an ellipsis, so a reply to a reply does not carry the whole thread.
`vm-mail-mode-citation-kill-regexp-alist' is what it works from."
  (with-temp-buffer
    (mail-mode)
    ;; the quoting here is `vm-included-text-prefix', whose default is " > ";
    ;; the alist is built from it when vm-vars is loaded, so a test that binds
    ;; the variable afterwards changes nothing
    (insert "To: someone@example.com\n" mail-header-separator "\n"
            "My answer.\n"
            " > > The message before that.\n"
            " > > More of it.\n"
            " > What they wrote.\n")
    (vm-mail-mode-citation-clean-up)
    (should-not (string-match-p "The message before that" (buffer-string)))
    (should (string-match-p "\\[\\.\\.\\.\\]" (buffer-string)))
    ;; the single-cited text, which is what is being answered, stays
    (should (string-match-p "^ > What they wrote\\.$" (buffer-string)))))

(ert-deftest vm-reply-test-retrying-a-mime-bounce ()
  "`vm-resend-bounced-message' digs the returned message out of the bounce.
What comes back is the message you sent -- its headers and its body -- and not
the postmaster's report of why it failed, with an empty Resent-To to put the
corrected address in."
  (vm-reply-test--composing (_folder vm-reply-test--mime-bounce)
    (vm-resend-bounced-message)
    (should (equal (vm-reply-test--header "Subject") "the original"))
    (should (equal (vm-reply-test--header "To") "nosuch@example.com"))
    (should (equal (vm-reply-test--header "Resent-To") ""))
    (should (string-match-p "The original body" (buffer-string)))
    (should-not (string-match-p "could not be delivered" (buffer-string)))))

(ert-deftest vm-reply-test-retrying-a-bounce-that-is-only-text ()
  "A bounce with no MIME part is handled by looking for the returned
message's own Received line, which is where VM takes the start of it to be."
  (vm-reply-test--composing (_folder vm-reply-test--plain-bounce)
    (vm-resend-bounced-message)
    (should (equal (vm-reply-test--header "Subject") "the original"))
    (should (string-match-p "The original body" (buffer-string)))
    (should-not (string-match-p "Transcript of session" (buffer-string)))))

(ert-deftest vm-reply-test-retrying-what-is-not-a-bounce-is-refused ()
  "A message with no returned message in it says so, rather than composing
something out of whatever was there."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (let ((text-quoting-style 'grave))
      (should (equal (cadr (should-error (vm-resend-bounced-message)))
                     "This doesn't look like a bounced message.")))))

;;; Cleaning up the Subject of a composition (emacs-vm/vm#655)
;;
;; `vm-mail-subject-cleanup' is documented for `vm-mail-mode-hook', so an
;; error in it aborts composition setup rather than merely failing to tidy a
;; subject.

(defmacro vm-reply-test--with-subject (spec &rest body)
  "Run BODY in a composition built from SPEC, a plist.
:subject is the Subject header, :references the References header, :prefix
`vm-reply-subject-prefix' and :number `vm-mail-subject-number-reply'.  The
composition counts as a reply, since that is what the numbering needs."
  (declare (indent 1) (debug t))
  `(let ((mail-header-separator "--text follows this line--")
         (vm-reply-subject-prefix (plist-get ,spec :prefix))
         (vm-mail-subject-number-reply (plist-get ,spec :number))
         (vm-reply-list '(a-message))
         (text-quoting-style 'grave))
     (with-temp-buffer
       (mail-mode)
       (insert "To: someone@example.com\n"
               "Subject: " (plist-get ,spec :subject) "\n"
               (let ((refs (plist-get ,spec :references)))
                 (if refs (concat "References: " refs "\n") ""))
               mail-header-separator "\n"
               "The body.\n")
       ,@body)))

(defun vm-reply-test--subject ()
  "The Subject header of the current composition."
  (vm-mail-mode-get-header-contents "Subject:"))

(ert-deftest vm-reply-test-subject-cleanup-replaces-a-foreign-prefix ()
  "A reply prefix in another language is replaced by the configured one, so
a thread does not accumulate one prefix per correspondent's mail program."
  (vm-reply-test--with-subject '(:subject "AW: hello" :prefix "Re: ")
    (vm-mail-subject-cleanup)
    (should (equal (vm-reply-test--subject) "Re: hello"))))

(ert-deftest vm-reply-test-subject-cleanup-collapses-repeated-prefixes ()
  "A pile of prefixes becomes one: the default replacements match a run of
them, however they are spelled and numbered."
  (vm-reply-test--with-subject '(:subject "Re: AW: Re[3]: hello" :prefix "Re: ")
    (vm-mail-subject-cleanup)
    (should (equal (vm-reply-test--subject) "Re: hello"))))

(ert-deftest vm-reply-test-subject-cleanup-replaces-a-forward-prefix ()
  "The forward prefixes are replaced as well as the reply ones: the second
entry of `vm-mail-subject-prefix-replacements' is what handles WG and FO."
  (vm-reply-test--with-subject '(:subject "WG: hello" :prefix "Re: ")
    (vm-mail-subject-cleanup)
    (should (equal (vm-reply-test--subject) "Fo: hello"))))

(ert-deftest vm-reply-test-subject-cleanup-numbers-by-the-references ()
  "With numbering on, the reply prefix carries the number of references, so
the subject says how deep in the thread it is."
  (vm-reply-test--with-subject '(:subject "Re: hello" :prefix "Re: " :number t
                                 :references "<a@x> <b@x>")
    (vm-mail-subject-cleanup)
    (should (equal (vm-reply-test--subject) "Re[2]: hello"))))

(ert-deftest vm-reply-test-subject-cleanup-leaves-a-first-reply-unnumbered ()
  "One reference is the message being replied to, so there is nothing to
count yet and the subject is left as it is."
  (vm-reply-test--with-subject '(:subject "Re: hello" :prefix "Re: " :number t
                                 :references "<a@x>")
    (vm-mail-subject-cleanup)
    (should (equal (vm-reply-test--subject) "Re: hello"))))

(ert-deftest vm-reply-test-subject-cleanup-needs-a-prefix-to-number ()
  "REGRESSION: numbering without a reply prefix is reported, not a type error.

The number goes inside the prefix, and `vm-reply-subject-prefix' is nil by
default: passing that to `regexp-quote' signalled wrong-type-argument from
inside `vm-mail-mode-hook', which aborts composition setup.  The message
names both variables, since setting one without the other is the mistake."
  (vm-reply-test--with-subject '(:subject "Re: hello" :number t
                                 :references "<a@x> <b@x>")
    (let ((err (should-error (vm-mail-subject-cleanup) :type 'error)))
      (should (string-match-p "vm-reply-subject-prefix"
                              (error-message-string err)))
      (should (string-match-p "vm-mail-subject-number-reply"
                              (error-message-string err))))))

(ert-deftest vm-reply-test-subject-cleanup-reports-a-subject-it-cannot-number ()
  "A subject that does not begin with the prefix is reported by name.

The message used to name `vm-mail-check-subject-cleanup', which does not
exist, so a reader could not find what had complained."
  (vm-reply-test--with-subject '(:subject "hello" :prefix "Re: " :number t
                                 :references "<a@x> <b@x>")
    (let ((err (should-error (vm-mail-subject-cleanup) :type 'error)))
      (should (string-match-p "vm-mail-subject-cleanup"
                              (error-message-string err)))
      (should-not (string-match-p "vm-mail-check-subject-cleanup"
                                  (error-message-string err))))))

(ert-deftest vm-reply-test-subject-cleanup-leaves-a-new-message-alone ()
  "A composition that is not a reply is not numbered, whatever References it
carries: `vm-reply-list' is what says it is a reply."
  (let ((vm-reply-list nil))
    (vm-reply-test--with-subject '(:subject "Re: hello" :prefix "Re: " :number t
                                   :references "<a@x> <b@x>")
      (setq vm-reply-list nil)
      (vm-mail-subject-cleanup)
      (should (equal (vm-reply-test--subject) "Re: hello")))))

;;; Return receipts (emacs-vm/vm#656)

(defconst vm-reply-test--receipt-requested
  (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
          "From: Alice Adams <alice@example.com>\n"
          "To: me@example.com\n"
          "Return-Receipt-To: receipts@example.com\n"
          "Message-ID: <one@example.com>\n"
          "Subject: badgers\n\n"
          "The body of the message.\n\n")
  "A message asking for a return receipt, addressed elsewhere than its author.")

(defun vm-reply-test--composition-among (buffers)
  "The composition buffer among the buffers not in BUFFERS, or nil.

`vm-handle-return-receipt' works inside `save-excursion', so the
composition it leaves is not the current buffer when it returns; it has to
be found."
  (car (seq-filter (lambda (buffer)
                     (and (buffer-live-p buffer)
                          (with-current-buffer buffer
                            ;; VM's compositions are in mail-mode, with
                            ;; vm-mail-mode-map bound over it
                            (derived-mode-p 'mail-mode))))
                   (seq-remove (lambda (b) (memq b buffers)) (buffer-list)))))

(defmacro vm-reply-test--receipting (settings &rest body)
  "Select a message asking for a receipt, run BODY with SETTINGS bound.
BODY sees SENT, non-nil when the receipt was sent, and can call
`vm-reply-test--composition-among' on BEFORE to find a composition left
behind."
  (declare (indent 1) (debug t))
  `(vm-reply-test--composing (_folder vm-reply-test--receipt-requested)
     (let ((before (buffer-list))
           (sent nil))
       (ignore sent)
       (cl-letf (((symbol-function 'vm-mail-send-and-exit)
                  (lambda (&rest _) (setq sent t))))
         (let ,settings
           ,@body)))))

(ert-deftest vm-reply-test-a-receipt-goes-to-the-address-that-asked ()
  "The receipt is addressed to Return-Receipt-To rather than to the author,
and asks for no receipt of its own -- two of them answering each other would
never stop.

The command removes that header, which nothing can make it need to do: a
reply composed by `vm-reply' does not carry the replied-to message's
Return-Receipt-To.  Removing the call changes no test, so what is pinned
here is the invariant rather than the call."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'edit))
    (vm-handle-return-receipt)
    (let ((composition (vm-reply-test--composition-among before)))
      (should composition)
      (with-current-buffer composition
        (should (equal (vm-reply-test--header "To") "receipts@example.com"))
        (should-not (vm-reply-test--header "Return-Receipt-To"))))))

(ert-deftest vm-reply-test-a-receipt-says-when-and-quotes-the-message ()
  "The body reports the message as received and quotes the beginning of it,
so the sender can tell which message the receipt is about."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'edit))
    (vm-handle-return-receipt)
    (with-current-buffer (vm-reply-test--composition-among before)
      (let ((text (buffer-string)))
        (should (string-match-p "has been received on" text))
        (should (string-match-p "Subject: badgers" text))
        (should (string-match-p "The body of the message" text))
        (should (string-match-p "\\[\\.\\.\\.\\]" text))))))

(ert-deftest vm-reply-test-a-receipt-quotes-no-more-than-the-peek ()
  "`vm-handle-return-receipt-peek' bounds how much of the message comes back.
It is the reader's copy of somebody else's mail, so a small number means a
small quotation rather than the whole message."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'edit)
                              (vm-handle-return-receipt-peek 4))
    (vm-handle-return-receipt)
    (with-current-buffer (vm-reply-test--composition-among before)
      (let ((text (buffer-string)))
        (should (string-match-p "Subject: badgers" text))
        (should-not (string-match-p "body of the message" text))))))

(ert-deftest vm-reply-test-a-receipt-is-not-sent-in-edit-mode ()
  "In `edit' mode the composition is left for the reader to look at and
send; nothing goes out behind their back."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'edit))
    (vm-handle-return-receipt)
    (should-not sent)
    (should (vm-reply-test--composition-among before))))

(ert-deftest vm-reply-test-a-receipt-is-sent-in-auto-mode ()
  "In any mode but `edit' the receipt is sent as soon as it is composed."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'auto))
    (vm-handle-return-receipt)
    (should sent)))

(ert-deftest vm-reply-test-asking-about-a-receipt-takes-no-for-an-answer ()
  "With `ask', a receipt is composed only if the reader says so.  Telling
somebody their mail was read is the reader's business."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'ask))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (vm-handle-return-receipt))
    (should-not sent)
    (should-not (vm-reply-test--composition-among before))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (vm-handle-return-receipt))
    (should (vm-reply-test--composition-among before))))

(ert-deftest vm-reply-test-a-mode-that-is-an-expression-is-evaluated ()
  "The mode may be an expression, which decides per message."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode
                              '(string-match "badgers"
                                             (vm-su-subject
                                              (car vm-message-pointer)))))
    (vm-handle-return-receipt)
    (should sent))
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode
                              '(string-match "otters"
                                             (vm-su-subject
                                              (car vm-message-pointer)))))
    (vm-handle-return-receipt)
    (should-not sent)))

(ert-deftest vm-reply-test-a-message-already-replied-to-gets-no-receipt ()
  "A message that has been replied to is left alone: the sender has heard
back, and a second receipt for the same message is noise."
  (vm-reply-test--receipting ((vm-handle-return-receipt-mode 'auto))
    (vm-set-replied-flag (car vm-message-pointer) t)
    (vm-handle-return-receipt)
    (should-not sent)))

(ert-deftest vm-reply-test-a-message-not-asking-gets-no-receipt ()
  "A message with no Return-Receipt-To header gets nothing, whatever the
mode says."
  (vm-reply-test--composing (_folder vm-reply-test--incoming)
    (let ((before (buffer-list))
          (sent nil)
          (vm-handle-return-receipt-mode 'auto))
      (cl-letf (((symbol-function 'vm-mail-send-and-exit)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "asked about a receipt nobody wanted"))))
        (vm-handle-return-receipt))
      (should-not sent)
      (should-not (vm-reply-test--composition-among before)))))

;;; Where a composition lives (emacs-vm/vm#666)

(defmacro vm-reply-test--composing-elsewhere (&rest body)
  "Run BODY with the folder in one directory and `vm-folder-directory' in
another, so a composition that takes the folder's directory is visible.

DRAFTS is `vm-mail-auto-save-directory', unset unless a test binds it;
FOLDERS is `vm-folder-directory', which is what VM falls back to."
  (declare (indent 0) (debug t))
  `(let* ((folders (file-name-as-directory (make-temp-file "vm-folders" t)))
          (vm-folder-directory folders)
          (vm-mail-auto-save-directory nil)
          (auto-save-default t))
     (unwind-protect
         (vm-reply-test--composing (_folder vm-reply-test--incoming)
           ,@body)
       (delete-directory folders t))))

(defun vm-reply-test--composition-directory ()
  "The directory of the composition the last command made."
  (directory-file-name (expand-file-name default-directory)))

(ert-deftest vm-reply-test-a-reply-lives-where-vm-put-it ()
  "REGRESSION: a reply's composition keeps the directory VM chose for it.

`vm-mail-internal' sets it to `vm-mail-auto-save-directory' or
`vm-folder-directory' so that auto-save files are written somewhere
writable; `vm-do-reply' then put the folder's own directory back.  For an
IMAP folder that is its local cache, so anything that recomputed the
auto-save name -- `rename-buffer' does, and VM renames a composition after
sending it -- wrote half-written mail into the cache directory."
  (vm-reply-test--composing-elsewhere
    (vm-reply 1)
    (should (equal (vm-reply-test--composition-directory)
                   (directory-file-name folders)))
    (should (string-prefix-p (file-name-as-directory folders)
                             buffer-auto-save-file-name))))

(ert-deftest vm-reply-test-the-auto-save-directory-is-preferred ()
  "`vm-mail-auto-save-directory' is where compositions go when it is set:
that is what it is for, and nothing may put the folder's directory back
over it."
  (let ((drafts (file-name-as-directory (make-temp-file "vm-drafts" t))))
    (unwind-protect
        (vm-reply-test--composing-elsewhere
          (let ((vm-mail-auto-save-directory drafts))
            (vm-reply 1)
            (should (equal (vm-reply-test--composition-directory)
                           (directory-file-name drafts)))
            (should (string-prefix-p drafts buffer-auto-save-file-name))))
      (delete-directory drafts t))))

(ert-deftest vm-reply-test-every-composition-lives-there ()
  "Forwarding, resending and sending a digest choose the directory the same
way a reply does: each of them used to put the folder's back."
  ;; the two that can be started from an ordinary message; a bounced
  ;; message and a digest each need a message of their own kind
  (dolist (start (list (lambda () (vm-forward-message))
                       (lambda () (vm-resend-message))))
    (vm-reply-test--composing-elsewhere
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "someone@example.com"))
                ((symbol-function 'completing-read) (lambda (&rest _) "rfc934"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (funcall start))
      (should (equal (vm-reply-test--composition-directory)
                     (directory-file-name folders))))))

(ert-deftest vm-reply-test-the-auto-save-file-follows-a-rename ()
  "Renaming the composition keeps its auto-save file where VM put it.

Emacs recomputes the name from `default-directory' when a buffer with no
file is renamed, and VM renames a composition to \"sent ...\" after sending
it.  That is how the folder's directory used to end up holding drafts even
though the name was right when the composition began."
  (vm-reply-test--composing-elsewhere
    (vm-reply 1)
    (rename-buffer "sent reply to Alice Adams" t)
    (should (string-prefix-p (file-name-as-directory folders)
                             buffer-auto-save-file-name))))

;;; Filing a copy on the server without a hook (emacs-vm/vm#68)

(defmacro vm-reply-test--sending-with-imap-fcc (headers &rest body)
  "Compose a message with HEADERS, send it, and run BODY.
APPENDED collects (MAILBOX . TEXT) for each copy VM files on the server,
and SESSIONS the maildrops it opened a session for.  Nothing leaves the
machine: the IMAP session and the append are stubbed."
  (declare (indent 1) (debug t))
  `(let ((appended nil)
         (sessions nil)
         (mail-header-separator "--text follows this line--")
         (vm-fcc-filed nil))
     ;; `vm-imap-net-append-text' is the one way a composition is filed on a
     ;; server now: the blocking session it used to fall back to is gone
     ;; (emacs-vm/vm#822).
     (cl-letf (((symbol-function 'vm-imap-net-append-text)
                (lambda (maildrop mailbox string &rest _)
                  (push maildrop sessions)
                  (push (cons mailbox string) appended)
                  t))
               ((symbol-function 'mail-send) #'ignore)
               ((symbol-function 'vm-mail-mode-remove-tm-hooks) #'ignore))
       (with-temp-buffer
         (mail-mode)
         (insert "To: someone@example.com\n"
                 "Subject: with a copy on the server\n"
                 ,headers
                 mail-header-separator "\n"
                 "The body.\n")
         ,@body))))

(ert-deftest vm-reply-test-an-imap-fcc-is-filed-without-a-hook ()
  "An IMAP-FCC header files a copy on the server as VM sends, with nothing
added to `mail-send-hook'.

That request is issue #68, from 2010: the manual told the reader to wire
`vm-imap-save-composition' up by hand, and a reader who did not notice got
no copy at all."
  (vm-reply-test--sending-with-imap-fcc "IMAP-FCC: Sent\n"
    (let ((vm-imap-default-account "work")
          (vm-imap-account-alist
           '(("imap-ssl:mail.example.invalid:993:*:login:alice:*" "work"))))
      (vm-mail-send))
    (should (equal (length appended) 1))
    (should (equal (car (car appended)) "Sent"))
    (should (string-match-p "The body" (cdr (car appended))))
    ;; and the header is not in the message that went out
    (should-not (vm-mail-mode-get-header-contents "IMAP-FCC:"))))

(ert-deftest vm-reply-test-an-imap-fcc-uses-the-account-it-came-from ()
  "The mailbox is on the account of the folder being replied from, which is
what makes IMAP-FCC easier to write than a full maildrop: no host, no
password, just the mailbox."
  (vm-reply-test--sending-with-imap-fcc "IMAP-FCC: Sent\n"
    (let ((folder (generate-new-buffer " *test folder*")))
      (unwind-protect
          (progn
            (with-current-buffer folder
              (setq major-mode 'vm-mode)
              (setq vm-folder-access-method 'imap
                    vm-folder-access-data (make-vector 20 nil))
              (vm-set-folder-imap-maildrop-spec
               "imap-ssl:mail.example.invalid:993:inbox:login:alice:*")
              (setq vm-message-pointer nil))
            (setq vm-mail-buffer folder)
            (vm-mail-send))
        (kill-buffer folder)))
    (should (equal (car (car appended)) "Sent"))
    (should (equal sessions
                   '("imap-ssl:mail.example.invalid:993:inbox:login:alice:*")))))

(ert-deftest vm-reply-test-an-imap-fcc-needs-an-account-to-file-to ()
  "With no parent folder and no default account there is nowhere to file,
and the refusal names the option to set."
  (vm-reply-test--sending-with-imap-fcc "IMAP-FCC: Sent\n"
    (let ((vm-imap-default-account nil)
          (text-quoting-style 'grave))
      (let ((err (should-error (vm-mail-send) :type 'error)))
        (should (string-match-p "vm-imap-default-account"
                                (error-message-string err)))))))

(ert-deftest vm-reply-test-a-composition-with-no-imap-fcc-files-nothing ()
  "A composition without the header opens no session: nobody who does not
use IMAP-FCC pays for it."
  (vm-reply-test--sending-with-imap-fcc ""
    (vm-mail-send)
    (should-not appended)
    (should-not sessions)))


(defmacro vm-reply-test--with-a-displayed-folder (&rest body)
  "Visit a folder holding a receipt request, with a summary window, run BODY.
FOLDER is the folder buffer.  A real visit with a summary on display, because
that is what the bug needs: `vm-reply-test--receipting' works over temp
buffers with no folder displayed, so `vm-reply' changes no window there and a
test written on it passes whether the fix is in or not."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-receipt" t)))
          (file (expand-file-name "inbox" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-frame-per-summary nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list))
          folder)
     (unwind-protect
         (progn
           (with-temp-file file
             (insert vm-reply-test--receipt-requested))
           (vm-visit-folder file)
           (setq folder (current-buffer))
           (vm-summarize)
           (ignore folder)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-reply-test-sending-a-receipt-leaves-the-windows-alone ()
  "REGRESSION: sending a return receipt does not take the summary away.

Issue #800, reported by @goeran.  The receipt is composed by `vm-reply' and
sent at once without the reader asking to see either, but `vm-reply' displays
the composition on the way past, so the window showing the summary was left
showing the presentation buffer and had to be brought back by hand.

Compares the buffers the windows show, not how many there are: replacing one
buffer with another keeps the number the same, which is exactly what happened.

The `edit' side of the fix, where the composition is shown on purpose and the
display is the reader's to keep, is checked by
vm-reply-test-editing-a-receipt-still-shows-it below."
  (vm-reply-test--with-a-displayed-folder
    (let ((vm-handle-return-receipt-mode 'ask)
          (shown (mapcar #'window-buffer (window-list))))
      (should (> (length shown) 1))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'vm-mail-send) (lambda (&rest _) t))
                ((symbol-function 'mail-send) (lambda (&rest _) t)))
        (vm-handle-return-receipt))
      (should (equal shown (mapcar #'window-buffer (window-list)))))))

(ert-deftest vm-reply-test-editing-a-receipt-still-shows-it ()
  "In `edit' mode the composition is shown, the windows being theirs to keep.

The other side of #800: the fix puts the display back only where the reader
was never shown anything, so the mode that exists to let them edit the receipt
must still put it in front of them."
  (vm-reply-test--with-a-displayed-folder
    (let ((vm-handle-return-receipt-mode 'edit)
          (shown (mapcar #'window-buffer (window-list))))
      (vm-handle-return-receipt)
      (let ((now (mapcar #'window-buffer (window-list))))
        (should-not (equal shown now))
        (should (seq-some (lambda (buffer)
                            (with-current-buffer buffer
                              (derived-mode-p 'mail-mode)))
                          now))))))


;;; Filing a copy exactly once (emacs-vm/vm#784)

(defun vm-reply-test--messages-in (file)
  "How many messages FILE holds, counted by their envelope lines."
  (if (not (file-exists-p file))
      0
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^From " nil t)
          (setq count (1+ count)))
        count))))

(defun vm-reply-test--send-with-fcc (early edit-and-send-again)
  "Compose with an Fcc and send, answering how many copies were filed.
EARLY files before the send, which is what `vm-epg-encrypt' does when it
encodes.  EDIT-AND-SEND-AGAIN sends a second time, which must file again.
`mail-send' is stubbed: what is under test is the filing, not the sending."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-fcc-once" t)))
         (fcc (expand-file-name "archive" dir))
         (before (buffer-list))
         (vm-do-fcc-before-mime-encode t)
         (vm-send-using-mime t)
         (vm-confirm-mail-send nil)
         (vm-check-recipients nil)
         (vm-check-for-empty-subject nil)
         (vm-mail-send-hook nil)
         (mail-send-hook nil)
         (vm-dont-ask-coding-system-question t)
         (select-safe-coding-system-function nil))
    (unwind-protect
        (cl-letf (((symbol-function 'vm-display) #'ignore)
                  ((symbol-function 'mail-send) #'ignore)
                  ((symbol-function 'vm-mail-mark-sent) #'ignore))
          (with-temp-buffer
            (mail-mode)
            (insert "To: someone@example.com\nSubject: s\nFcc: " fcc "\n"
                    mail-header-separator "\nthe body\n")
            (when early
              (vm-do-fcc-in-composition))
            (vm-mail-send)
            (when edit-and-send-again
              (goto-char (point-max))
              (insert "an afterthought\n")
              (vm-mail-send))
            (vm-reply-test--messages-in fcc)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-a-copy-filed-before-the-send-is-not-filed-again ()
  "REGRESSION: an Fcc copy filed before the send is not filed a second time.

emacs-vm/vm#784, reported against encryption: with
`vm-do-fcc-before-mime-encode' set, `vm-epg-encrypt' files the unencoded copy
when it encodes, and the send then filed the encrypted message as well, so
the folder held both.

Two causes, and both are here.  The call to `vm-do-fcc-before-mime-encode'
inside `vm-mail-send' had no guard, so it filed again even where the flag
said the copy had gone.  And `vm-fcc-filed' was cleared at the start of the
send, which threw away what an encrypting command had done moments before it.

Checked without gpg: what matters is a copy filed before the send, and any
command that encodes early does that."
  (should (equal 1 (vm-reply-test--send-with-fcc nil nil)))
  (should (equal 1 (vm-reply-test--send-with-fcc t nil))))

(ert-deftest vm-reply-test-a-second-send-files-its-own-copy ()
  "A message edited and sent again is filed again.

This is why `vm-fcc-filed' has to be cleared somewhere: VM keeps the
composition buffer after a send, and a flag left set would mean the second
send filed nothing.  Cleared at the end of the send rather than the start,
which is what leaves a copy filed by an earlier command counted."
  (should (equal 2 (vm-reply-test--send-with-fcc nil t)))
  (should (equal 2 (vm-reply-test--send-with-fcc t t))))


;;; Refusing to hand a Bcc to a transport that may leak it (emacs-vm/vm#815)

(defvar vm-reply-test--asked nil
  "The question the Bcc check put, if it put one.")

(defun vm-reply-test--send-with (bcc sender check &optional answer)
  "Try to send a composition, answering `sent' or the message it stopped with.
BCC puts a Bcc header on it, SENDER is `send-mail-function', CHECK is
`vm-check-bcc-removal' and ANSWER is what to say to any question.
`mail-send' is stubbed: what is under test is whether VM gets that far."
  (with-temp-buffer
    (mail-mode)
    (insert "To: someone@example.com\nSubject: s\n"
            (if bcc "BCC: me@example.com,\n" "")
            mail-header-separator "\nbody\n")
    (let ((send-mail-function sender)
          (vm-check-bcc-removal check)
          (vm-confirm-mail-send nil)
          (vm-check-recipients nil)
          (vm-check-for-empty-subject nil)
          (vm-mail-send-hook nil)
          (mail-send-hook nil)
          (vm-dont-ask-coding-system-question t)
          (select-safe-coding-system-function nil))
      (setq vm-reply-test--asked nil)
      (cl-letf (((symbol-function 'vm-display) #'ignore)
                ((symbol-function 'mail-send) #'ignore)
                ((symbol-function 'vm-mail-mark-sent) #'ignore)
                ((symbol-function 'vm-rename-current-mail-buffer) #'ignore)
                ((symbol-function 'vm-keep-mail-buffer) #'ignore)
                ((symbol-function 'y-or-n-p)
                 (lambda (question)
                   (setq vm-reply-test--asked question)
                   answer)))
        (condition-case caught (progn (vm-mail-send) 'sent)
          (error (error-message-string caught)))))))

(ert-deftest vm-reply-test-a-bcc-going-out-through-a-delegating-sender-asks ()
  "REGRESSION: VM asks before handing a Bcc to a transport that may leak it.

emacs-vm/vm#815, reported after a Bcc reached everyone on a message.  VM and
Emacs keep the header in what they hand `sendmail-program', because with -t
those addresses are how the transport learns whom to deliver to, and trust
that program to remove it.  Where it does not, the promise a Bcc makes is
broken and nothing anywhere reports a failure.

Asking rather than refusing: a working sendmail, postfix or exim does remove
it, and Emacs cannot tell one of those from a transport that does not."
  (should (equal 'sent (vm-reply-test--send-with t 'sendmail-send-it t t)))
  (should (stringp vm-reply-test--asked))
  (should (string-match-p "Bcc" vm-reply-test--asked))
  (should (string-match-p "sendmail-send-it" vm-reply-test--asked))
  ;; The question itself names where the answer is written down, not only the
  ;; error that follows a no: a reader who says yes at the prompt and wants to
  ;; fix it afterwards has nothing else to go on.
  (should (string-match-p "Sending Options" vm-reply-test--asked))
  (should (string-match-p "VM manual" vm-reply-test--asked)))

(ert-deftest vm-reply-test-declining-the-bcc-question-says-what-to-change ()
  "Answering no stops the send and points at the manual.
A message that says only what is wrong leaves the reader to guess; this one
names the setting, the alternative, and where it is written down."
  (let ((stopped (vm-reply-test--send-with t 'sendmail-send-it t nil)))
    (should (stringp stopped))
    (should (string-match-p "smtpmail-send-it" stopped))
    (should (string-match-p "vm-check-bcc-removal" stopped))
    (should (string-match-p "Bcc header out" stopped))
    (should (string-match-p "VM manual" stopped))
    ;; Named exactly as the manual has them, so `g' in Info reaches them:
    ;; both strings said "Mail Sending Options", which was the section
    ;; heading and never a node.
    (should (string-match-p "Sending Options" stopped))
    (should (string-match-p "Setting Up" stopped))))

(ert-deftest vm-reply-test-a-sender-that-removes-the-bcc-itself-does-not-ask ()
  "`smtpmail-send-it' works the recipients out first and then deletes the
header, so there is nothing to ask about."
  (should (equal 'sent (vm-reply-test--send-with t 'smtpmail-send-it t nil)))
  (should (equal nil vm-reply-test--asked)))

(ert-deftest vm-reply-test-a-message-with-no-bcc-is-never-asked-about ()
  "The check costs a composition without a Bcc nothing at all."
  (should (equal 'sent (vm-reply-test--send-with nil 'sendmail-send-it t nil)))
  (should (equal nil vm-reply-test--asked)))

(ert-deftest vm-reply-test-the-bcc-check-can-be-turned-off ()
  "`vm-check-bcc-removal' nil sends without asking, for a reader who knows
their transport removes it."
  (should (equal 'sent (vm-reply-test--send-with t 'sendmail-send-it nil nil)))
  (should (equal nil vm-reply-test--asked)))

(ert-deftest vm-reply-test-an-unknown-sender-is-not-assumed-to-be-safe ()
  "Only what is known to remove the header is treated as removing it.
A sender VM has never heard of is asked about, which is the safe direction."
  (should (equal 'sent (vm-reply-test--send-with t 'some-unknown-send-it t t)))
  (should (stringp vm-reply-test--asked)))

;;; Mail aliases from ~/.mailrc (emacs-vm/vm#820)

(ert-deftest vm-reply-test-an-edited-mailrc-is-read-again ()
  "REGRESSION: an alias added during the session is seen by the next message.

`build-mail-aliases' fills `mail-aliases' once and leaves it filled, so
without `sendmail-sync-aliases' the file was whatever it said when the
first composition of the session was made, and an alias added afterwards
did not work until Emacs was restarted (emacs-vm/vm#820).  Emacs's own
`mail-setup' has always called it."
  (require 'sendmail)
  (require 'mailalias)
  (let* ((file (make-temp-file "vm-mailrc-"))
         (mail-personal-alias-file file)
         (mail-aliases t)
         (mail-alias-modtime nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "alias fred fred@example.com\n"))
          (sendmail-sync-aliases)
          (when (eq mail-aliases t)
            (setq mail-aliases nil)
            (build-mail-aliases))
          (should (equal "fred@example.com" (cdr (assoc "fred" mail-aliases))))
          ;; the reader adds one and composes again.  The modification time
          ;; has to move for the check to mean anything, and a file written
          ;; twice in the same second has not moved.
          (with-temp-file file
            (insert "alias fred fred@example.com\n"
                    "alias barney barney@example.com\n"))
          (set-file-times file (time-add (current-time) 2))
          (sendmail-sync-aliases)
          (when (eq mail-aliases t)
            (setq mail-aliases nil)
            (build-mail-aliases))
          (should (equal "barney@example.com"
                         (cdr (assoc "barney" mail-aliases)))))
      (delete-file file))))

(ert-deftest vm-reply-test-a-composition-syncs-the-aliases ()
  "REGRESSION: `vm-mail-internal' asks for the sync, not just this test.
The check above would pass on its own arithmetic whatever VM did, so this
one holds the call itself: it is what makes the manual's paragraph on
aliases true."
  (let ((asked nil))
    (cl-letf (((symbol-function 'sendmail-sync-aliases)
               (lambda (&rest _) (setq asked t))))
      ;; the source, rather than a composition: making one needs a folder
      ;; and a window configuration, and what is being pinned is that the
      ;; call is in the function at all.
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "../lisp/vm-reply.el" vm-test-dir))
        (goto-char (point-min))
        (should (re-search-forward "^(cl-defun vm-mail-internal" nil t))
        (let ((start (match-beginning 0)))
          (goto-char start)
          (forward-sexp)
          (should (string-match-p "(sendmail-sync-aliases)"
                                  (buffer-substring start (point))))))
      (ignore asked))))

;;; Killing a composition that has writing in it (emacs-vm/vm#824)

(defmacro vm-reply-test--killing-a-composition (setup &rest body)
  "Make a composition, run SETUP in it, kill it, and run BODY.
BODY sees `asked' -- what a question was put about, newest first -- and
`composition', the buffer, which is alive if the kill was refused."
  (declare (indent 1) (debug t))
  `(let ((asked nil)
         (composition nil)
         (before (buffer-list)))
     (unwind-protect
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (prompt) (push prompt asked) nil))
                   ((symbol-function 'y-or-n-p)
                    (lambda (prompt) (push prompt asked) nil)))
           (vm-mail)
           (setq composition (current-buffer))
           ,setup
           (kill-buffer composition)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))

(ert-deftest vm-reply-test-killing-a-written-composition-keeps-it ()
  "REGRESSION: a composition with writing in it is not lost when it is killed.

Reported by a user who had lost countless drafts: a composition buffer belongs
to no file, so `kill-buffer' does not put the question it puts about an unsaved
file, and everything bound to it took the draft with no warning.  His words:
\"It will only kill the buffer when there are no pending changes.  Only in a VM
reply buffer will this happen\" (emacs-vm/vm#824).

The writing is kept as a draft, and VM says where.  No question: the reader
asked to kill the buffer, and keeping what was in it costs them nothing."
  (let ((dir (file-name-as-directory (make-temp-file "vm-reply-keep" t)))
        (said nil))
    (unwind-protect
        (let ((vm-save-killed-messages-folder
               (expand-file-name "postponed" dir))
              (vm-folder-directory dir))
          (cl-letf (((symbol-function 'vm-inform)
                     (lambda (_level format &rest args)
                       (push (apply #'format format args) said))))
            (vm-reply-test--killing-a-composition
                (progn (goto-char (point-max))
                       (insert "A draft I would rather not lose.\n"))
              (should (equal asked nil))
              (should-not (buffer-live-p composition))))
          ;; the draft is on disk, and the reader was told where
          (should (file-exists-p vm-save-killed-messages-folder))
          (should (> (nth 7 (file-attributes vm-save-killed-messages-folder)) 0))
          (with-temp-buffer
            (insert-file-contents vm-save-killed-messages-folder)
            (should (string-match-p "A draft I would rather not lose"
                                    (buffer-string))))
          (should (seq-find (lambda (line)
                              (string-match-p "kept as a draft" line))
                            said)))
      (delete-directory dir t))))

(ert-deftest vm-reply-test-killing-a-written-composition-asks-when-not-kept ()
  "With keeping turned off, VM asks instead of losing the writing.

`vm-save-killed-message' nil says not to keep it, and then the question is
all that stands between a keystroke and the writing."
  (let ((vm-save-killed-message nil))
    (vm-reply-test--killing-a-composition
        (progn (goto-char (point-max))
               (insert "A draft I would rather not lose.\n"))
      (should (= (length asked) 1))
      (should (string-match-p "has not been sent" (car asked)))
      ;; and the answer was no, so it is still here
      (should (buffer-live-p composition)))))

(ert-deftest vm-reply-test-killing-an-untouched-composition-does-not-ask ()
  "A composition nothing has been written in goes without a question.

VM writes the headers itself, so a composition is modified from the moment it
appears; asking about that would put a question in the way of every abandoned
`vm-mail'."
  (vm-reply-test--killing-a-composition nil
    (should (equal asked nil))
    (should-not (buffer-live-p composition))))

(ert-deftest vm-reply-test-killing-a-blank-composition-does-not-ask ()
  "Whitespace is not writing."
  (vm-reply-test--killing-a-composition
      (progn (goto-char (point-max)) (insert "  \n\t\n"))
    (should (equal asked nil))
    (should-not (buffer-live-p composition))))

(ert-deftest vm-reply-test-killing-a-sent-composition-does-not-ask ()
  "A composition that has been sent goes without a question.
Sending leaves the buffer unmodified, which is what says the writing in it is
no longer only here."
  (vm-reply-test--killing-a-composition
      (progn (goto-char (point-max))
             (insert "Sent already.\n")
             (set-buffer-modified-p nil))
    (should (equal asked nil))
    (should-not (buffer-live-p composition))))

(ert-deftest vm-reply-test-the-question-can-be-turned-off ()
  "`vm-confirm-killing-a-composition' nil restores the old behaviour."
  (vm-reply-test--killing-a-composition
      (progn (goto-char (point-max))
             (insert "Kill this without asking.\n")
             (setq-local vm-confirm-killing-a-composition nil))
    (should (equal asked nil))
    (should-not (buffer-live-p composition))))

(ert-deftest vm-reply-test-agreeing-to-the-question-kills-the-composition ()
  "Yes kills it, which is what the reader asked for."
  (let ((composition nil)
        (before (buffer-list)))
    (unwind-protect
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (vm-mail)
          (setq composition (current-buffer))
          (goto-char (point-max))
          (insert "Really do go away.\n")
          (kill-buffer composition)
          (should-not (buffer-live-p composition)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))

(ert-deftest vm-reply-test-no-question-where-the-kill-saves-the-draft ()
  "No question where killing the composition will offer to keep it.

`vm-postpone-mode' puts `vm-save-killed-message-hook' on the local
`kill-buffer-hook', and `vm-postpone-unfinished-compositions' kills a
composition for the express purpose of reaching it.  Asking first would put
two questions in a row and, answered no, would stop the save it exists to
make -- which is what happened when this guard was first written."
  (require 'vm-postpone)
  (vm-reply-test--killing-a-composition
      (progn (goto-char (point-max))
             (insert "A draft the kill hook will offer to keep.\n")
             (setq-local vm-save-killed-message 'ask))
    ;; the hook had its own say; what matters is that the guard did not
    (should-not (seq-find (lambda (prompt)
                            (string-match-p "has not been sent" prompt))
                          asked))
    (should-not (buffer-live-p composition))))

(provide 'vm-reply-test)

;;; vm-reply-test.el ends here
