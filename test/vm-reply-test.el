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
        (should (string-match-p "^From VM " text))
        (should (string-match-p "^>From nobody@example.com" text))
        (should-not (string-match-p "^Content-Length:" text)))
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
                          (lambda (l) (string-prefix-p "From VM " l))
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
                        (lambda (l) (string-prefix-p "From VM " l))
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
                        (lambda (l) (string-prefix-p "From VM " l))
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
copy being filed twice *within* one send; left set from the last one it
meant the next send filed nowhere and said nothing.  Goes through
`vm-mail-send' rather than around it, because the clearing lives there --
calling `vm-do-fcc-in-composition' directly would pass either way."
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
              (should vm-fcc-filed)
              ;; edit it and send it again
              (goto-char (point-max))
              (insert "a correction\n")
              (vm-mail-send)))
          (should (= 2 (cl-count-if
                        (lambda (l) (string-prefix-p "From VM " l))
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

(provide 'vm-reply-test)

;;; vm-reply-test.el ends here
