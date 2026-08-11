;;; vm-postpone-test.el --- Tests for vm-postpone.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM postpone/draft message handling (formerly vm-pine).
;; Tests cover utility functions, header insertion, FCC handling,
;; and hook management.

;;; Code:

(require 'vm-test-init)
(require 'vm-postpone)

;;; vm-buffer-in-vm-mode tests

(ert-deftest vm-postpone-test-buffer-in-vm-mode-yes ()
  "Test vm-buffer-in-vm-mode returns t for VM modes."
  (with-temp-buffer
    (let ((major-mode 'vm-mode))
      (should (vm-buffer-in-vm-mode)))
    (let ((major-mode 'vm-virtual-mode))
      (should (vm-buffer-in-vm-mode)))
    (let ((major-mode 'vm-presentation-mode))
      (should (vm-buffer-in-vm-mode)))
    (let ((major-mode 'vm-summary-mode))
      (should (vm-buffer-in-vm-mode)))
    (let ((major-mode 'vm-mail-mode))
      (should (vm-buffer-in-vm-mode)))))

(ert-deftest vm-postpone-test-buffer-in-vm-mode-no ()
  "Test vm-buffer-in-vm-mode returns nil for non-VM modes."
  (with-temp-buffer
    (let ((major-mode 'fundamental-mode))
      (should-not (vm-buffer-in-vm-mode)))
    (let ((major-mode 'text-mode))
      (should-not (vm-buffer-in-vm-mode)))
    (let ((major-mode 'emacs-lisp-mode))
      (should-not (vm-buffer-in-vm-mode)))))

;;; vm-mail-fcc-file-join tests

(ert-deftest vm-postpone-test-fcc-file-join-simple ()
  "Test vm-mail-fcc-file-join with simple path."
  (let ((result (vm-mail-fcc-file-join "/home/user/mail" "sent")))
    (should (stringp result))
    (should (string-match-p "sent" result))))

(ert-deftest vm-postpone-test-fcc-file-join-absolute ()
  "Test vm-mail-fcc-file-join with absolute file path."
  (let ((result (vm-mail-fcc-file-join "/home/user/mail" "/tmp/sent")))
    (should (stringp result))
    (should (string-match-p "/tmp/sent" result))))

(ert-deftest vm-postpone-test-fcc-file-join-nil-path ()
  "Test vm-mail-fcc-file-join returns dir when path expansion fails."
  ;; When file is nil, expand-file-name returns just the dir
  (let ((result (vm-mail-fcc-file-join "/home/user/mail" "")))
    (should (stringp result))))

;;; Defgroup and alias tests

(ert-deftest vm-postpone-test-group-exists ()
  "Test vm-postpone group is defined."
  (should (get 'vm-postpone 'custom-group)))

(ert-deftest vm-postpone-test-no-variable-alias-for-the-old-group-name ()
  "REGRESSION: the old group name is not aliased as though it were a variable.
This group was `vm-pine' before 8.4.0, and the file carried
\(defvaralias \='vm-pine \='vm-postpone\) with a comment calling it a group
alias.  Neither name is a variable, so what that did was make `vm-pine' a
variable alias pointing at nothing, and Customize has no group-alias mechanism
for it to have meant.  Both lines are gone.

This test replaces one that asserted the alias resolved to `vm-postpone', which
it did -- to an unbound variable of that name, not to the group."
  (should-not (boundp 'vm-pine))
  (should-not (eq (ignore-errors (indirect-variable 'vm-pine)) 'vm-postpone))
  ;; the group itself is still there, under its current name
  (should (get 'vm-postpone 'custom-group)))

;;; Defcustom tests

(ert-deftest vm-postpone-test-postponed-folder-default ()
  "Test vm-postponed-folder has default value."
  (should (stringp vm-postponed-folder))
  (should (equal vm-postponed-folder "postponed")))

(ert-deftest vm-postpone-test-postponed-header-default ()
  "Test vm-postponed-header has default value."
  (should (stringp vm-postponed-header))
  (should (string-match-p "X-VM-postponed-data:" vm-postponed-header)))

(ert-deftest vm-postpone-test-postponed-message-headers ()
  "Test vm-postponed-message-headers is a list of headers."
  (should (listp vm-postponed-message-headers))
  (should (member "From:" vm-postponed-message-headers))
  (should (member "To:" vm-postponed-message-headers))
  (should (member "Subject:" vm-postponed-message-headers)))

(ert-deftest vm-postpone-test-auto-expunge-default ()
  "Test vm-auto-expunge-postponed-folder defaults to nil."
  (should (null vm-auto-expunge-postponed-folder)))

;;; Mail composition buffer setup macro

(defmacro vm-postpone-test-with-mail-buffer (&rest body)
  "Execute BODY in a buffer set up like a mail composition buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "From: sender@example.com\n")
     (insert "To: recipient@example.com\n")
     (insert "Subject: Test\n")
     (insert mail-header-separator "\n")
     (insert "Body text\n")
     (goto-char (point-min))
     (mail-mode)
     ,@body))

;;; vm-mail-return-receipt-to tests

(ert-deftest vm-postpone-test-mail-return-receipt-to ()
  "Test vm-mail-return-receipt-to inserts receipt headers."
  (vm-postpone-test-with-mail-buffer
    (vm-mail-return-receipt-to)
    (goto-char (point-min))
    (should (search-forward "Return-Receipt-To:" nil t))
    (goto-char (point-min))
    (should (search-forward "Read-Receipt-To:" nil t))
    (goto-char (point-min))
    (should (search-forward "Delivery-Receipt-To:" nil t))))

(ert-deftest vm-postpone-test-mail-return-receipt-to-value ()
  "Test vm-mail-return-receipt-to uses configured value."
  (let ((vm-mail-return-receipt-to "test@example.com"))
    (vm-postpone-test-with-mail-buffer
      (vm-mail-return-receipt-to)
      (goto-char (point-min))
      (should (search-forward "test@example.com" nil t)))))

;;; vm-mail-priority tests

(ert-deftest vm-postpone-test-mail-priority ()
  "Test vm-mail-priority inserts priority headers."
  (vm-postpone-test-with-mail-buffer
    (vm-mail-priority)
    (goto-char (point-min))
    (should (search-forward "Priority:" nil t))))

(ert-deftest vm-postpone-test-mail-priority-custom ()
  "Test vm-mail-priority with custom value."
  (let ((vm-mail-priority "X-Priority: 5"))
    (vm-postpone-test-with-mail-buffer
      (vm-mail-priority)
      (goto-char (point-min))
      (should (search-forward "X-Priority: 5" nil t)))))

;;; vm-mail-to-fcc tests

(ert-deftest vm-postpone-test-mail-to-fcc-return-only ()
  "Test vm-mail-to-fcc with return-only extracts address."
  (vm-postpone-test-with-mail-buffer
    (let ((vm-mail-to-regexp "\\([^<\t\n ]+\\)@")
          (vm-mail-to-headers '("To:")))
      (let ((result (vm-mail-to-fcc nil t)))
        (should (equal result "recipient"))))))

(ert-deftest vm-postpone-test-mail-to-fcc-no-match ()
  "Test vm-mail-to-fcc returns mail-archive-file-name when no match."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "Subject: Test\n")
    (insert mail-header-separator "\n")
    (insert "Body\n")
    (goto-char (point-min))
    (mail-mode)
    (let ((vm-mail-to-regexp "\\([^<\t\n ]+\\)@")
          (vm-mail-to-headers '("To:"))
          (mail-archive-file-name "archive"))
      (let ((result (vm-mail-to-fcc nil t)))
        (should (equal result "archive"))))))

;;; vm-mail-select-folder tests

(ert-deftest vm-postpone-test-mail-select-folder-match ()
  "Test vm-mail-select-folder matches header."
  (vm-postpone-test-with-mail-buffer
    (let ((vm-auto-folder-case-fold-search t)
          (folder-alist '(("To:" ("recipient" . "recipient-folder")))))
      (should (equal (vm-mail-select-folder folder-alist)
                     "recipient-folder")))))

(ert-deftest vm-postpone-test-mail-select-folder-no-match ()
  "Test vm-mail-select-folder returns nil when no match."
  (vm-postpone-test-with-mail-buffer
    (let ((vm-auto-folder-case-fold-search t)
          (folder-alist '(("To:" ("nonexistent" . "some-folder")))))
      (should (null (vm-mail-select-folder folder-alist))))))

(ert-deftest vm-postpone-test-mail-select-folder-case-insensitive ()
  "Test vm-mail-select-folder respects case-fold setting."
  (vm-postpone-test-with-mail-buffer
    (let ((vm-auto-folder-case-fold-search t)
          (folder-alist '(("To:" ("RECIPIENT" . "matched-folder")))))
      (should (equal (vm-mail-select-folder folder-alist)
                     "matched-folder")))))

(ert-deftest vm-postpone-test-mail-select-folder-empty-alist ()
  "Test vm-mail-select-folder with empty alist."
  (vm-postpone-test-with-mail-buffer
    (should (null (vm-mail-select-folder nil)))))

;;; Hook management tests

(ert-deftest vm-postpone-test-add-save-killed-message-hook ()
  "Test vm-add-save-killed-message-hook adds the hook."
  (with-temp-buffer
    (vm-add-save-killed-message-hook)
    (should (memq 'vm-save-killed-message-hook kill-buffer-hook))))

(ert-deftest vm-postpone-test-remove-save-killed-message-hook ()
  "Test vm-remove-save-killed-message-hook removes the hook."
  (with-temp-buffer
    (vm-add-save-killed-message-hook)
    (vm-remove-save-killed-message-hook)
    (should-not (memq 'vm-save-killed-message-hook kill-buffer-hook))))

;;; vm-continue-what-message setting tests

(ert-deftest vm-postpone-test-continue-what-message-values ()
  "Test vm-continue-what-message accepts valid values."
  (let ((vm-continue-what-message nil))
    (should (null vm-continue-what-message)))
  (let ((vm-continue-what-message 'ask))
    (should (eq vm-continue-what-message 'ask)))
  (let ((vm-continue-what-message 'continue))
    (should (eq vm-continue-what-message 'continue))))

;;; vm-zero-drafts-start-compose tests

(ert-deftest vm-postpone-test-zero-drafts-default ()
  "Test vm-zero-drafts-start-compose defaults to nil."
  (should (null vm-zero-drafts-start-compose)))

;;; vm-save-killed-message tests

(ert-deftest vm-postpone-test-save-killed-message-values ()
  "Test vm-save-killed-message accepts valid values."
  (should (memq vm-save-killed-message '(ask always nil))))

;;; vm-postpone-message-modes-to-disable tests

(ert-deftest vm-postpone-test-modes-to-disable ()
  "Test vm-postpone-message-modes-to-disable is a list of modes."
  (should (listp vm-postpone-message-modes-to-disable))
  (should (memq 'font-lock-mode vm-postpone-message-modes-to-disable))
  (should (memq 'auto-fill-mode vm-postpone-message-modes-to-disable)))

;;; Obsolete alias tests

(ert-deftest vm-postpone-test-obsolete-decode-postponed ()
  "Test vm-decode-postponed-mime-message is aliased."
  ;; Check that the symbol is defined as an alias
  (should (fboundp 'vm-decode-postponed-mime-message))
  (should (symbolp (symbol-function 'vm-decode-postponed-mime-message)))
  (should (eq (symbol-function 'vm-decode-postponed-mime-message)
              'vm-mime-convert-to-attachment-buttons)))

(ert-deftest vm-postpone-test-obsolete-fake-attachment-is-gone ()
  "REGRESSION: `vm-pine-fake-attachment-overlays' is not an alias to nothing.
It was aliased to `vm-mime-re-fake-attachment-overlays', which was deleted as
unused in 2011, so calling it signalled `void-function' and `make-obsolete'
named a replacement that did not exist either.  The alias is gone.

This test replaces one that asserted the alias was there and pointed at that
name, which is how it survived: nothing checked that the target was defined."
  (should-not (fboundp 'vm-pine-fake-attachment-overlays))
  (should-not (fboundp 'vm-mime-re-fake-attachment-overlays)))

(ert-deftest vm-postpone-test-obsolete-decode-button ()
  "Test vm-decode-postponed-mime-button is aliased."
  (should (fboundp 'vm-decode-postponed-mime-button))
  (should (symbolp (symbol-function 'vm-decode-postponed-mime-button)))
  (should (eq (symbol-function 'vm-decode-postponed-mime-button)
              'vm-mime-replace-by-attachment-button)))

;;; Keybinding tests

(ert-deftest vm-postpone-test-keybinding-postpone ()
  "Test C-c C-d is bound to vm-postpone-message."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-d")
              'vm-postpone-message)))

(ert-deftest vm-postpone-test-keybinding-return-receipt ()
  "Test C-c C-f C-a is bound to vm-mail-return-receipt-to."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-a")
              'vm-mail-return-receipt-to)))

(ert-deftest vm-postpone-test-keybinding-priority ()
  "Test C-c C-f C-p is bound to vm-mail-priority."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-p")
              'vm-mail-priority)))

(ert-deftest vm-postpone-test-keybinding-fcc ()
  "Test C-c C-f C-f is bound to vm-mail-fcc."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-f")
              'vm-mail-fcc)))

(ert-deftest vm-postpone-test-keybinding-notice ()
  "Test C-c C-f C-n is bound to vm-mail-notice-requested-upon-delivery-to."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-n")
              'vm-mail-notice-requested-upon-delivery-to)))

;;; Feature provide tests

(ert-deftest vm-postpone-test-provides-vm-postpone ()
  "Test vm-postpone feature is provided."
  (should (featurep 'vm-postpone)))

(ert-deftest vm-postpone-test-provides-vm-pine ()
  "Test vm-pine feature is provided for backward compatibility."
  (should (featurep 'vm-pine)))

;;; vm-mail-to-headers tests

(ert-deftest vm-postpone-test-mail-to-headers-default ()
  "Test vm-mail-to-headers has sensible defaults."
  (should (listp vm-mail-to-headers))
  (should (member "To:" vm-mail-to-headers))
  (should (member "CC:" vm-mail-to-headers))
  (should (member "BCC:" vm-mail-to-headers)))

;;; vm-mail-to-regexp tests

(ert-deftest vm-postpone-test-mail-to-regexp-matches ()
  "Test vm-mail-to-regexp matches email addresses."
  (should (string-match vm-mail-to-regexp "user@example.com"))
  (should (equal (match-string 1 "user@example.com") "user")))

(ert-deftest vm-postpone-test-mail-to-regexp-angle-brackets ()
  "Test vm-mail-to-regexp handles angle bracket addresses."
  (let ((addr "John Doe <john@example.com>"))
    (should (string-match vm-mail-to-regexp addr))
    (should (equal (match-string 1 addr) "john"))))

;;; vm-get-persistent-message-ids-for tests (with nil input)

(ert-deftest vm-postpone-test-get-persistent-ids-nil ()
  "Test vm-get-persistent-message-ids-for with nil returns nil."
  (should (null (vm-get-persistent-message-ids-for nil))))

;;; vm-get-message-pointers-for tests (with nil input)

(ert-deftest vm-postpone-test-get-message-pointers-nil ()
  "Test vm-get-message-pointers-for with nil returns nil."
  (should (null (vm-get-message-pointers-for nil))))

;;; Hook variable tests

(ert-deftest vm-postpone-test-hooks-are-lists ()
  "Test hook variables are properly initialized."
  (should (listp vm-continue-postponed-message-hook))
  (should (listp vm-postpone-message-hook)))

;;; vm-mail-notice-requested-upon-delivery-to tests

(ert-deftest vm-postpone-test-notice-requested ()
  "Test vm-mail-notice-requested-upon-delivery-to inserts header."
  (vm-postpone-test-with-mail-buffer
    (vm-mail-notice-requested-upon-delivery-to)
    (goto-char (point-min))
    (should (search-forward "Notice-Requested-Upon-Delivery-To:" nil t))))

;;; user-home-directory

(ert-deftest vm-postpone-test-user-home-directory ()
  "Test user-home-directory returns HOME."
  (should (equal (user-home-directory) (getenv "HOME"))))

;;; vm-continue-postponed-message MIME handling

(defvar vm-postpone-test-draft
  (concat "From VM Sat Aug  1 12:00:00 2026\n"
          "MIME-Version: 1.0\n"
          "Content-Type: multipart/mixed; boundary=\"SEP\"\n"
          "Content-Transfer-Encoding: 8bit\n"
          "To: someone@example.com\n"
          "Subject: draft with an attachment\n"
          "\n"
          "--SEP\n"
          "Content-Type: text/plain; charset=us-ascii\n"
          "Content-Transfer-Encoding: 7bit\n"
          "\n"
          "Here is the body.\n"
          "\n"
          "--SEP\n"
          "Content-Type: text/plain; name=\"att.txt\"\n"
          "Content-Disposition: attachment; filename=\"att.txt\"\n"
          "Content-Transfer-Encoding: 7bit\n"
          "\n"
          "attachment payload\n"
          "\n"
          "--SEP--\n\n")
  "A postponed draft carrying an attachment, as `vm-postpone-message' writes it.")

(defun vm-postpone-test-continue (decoded)
  "Continue the test draft and return the resulting composition as a string.
DECODED is the value to give `vm-mime-decoded' in the folder buffer.
There is no presentation buffer, so the body copied is the raw one."
  (let (result (before (buffer-list)))
    (vm-test-with-folder vm-postpone-test-draft
      (setq vm-message-pointer vm-message-list)
      (setq vm-mime-decoded decoded)
      ;; Satisfy vm-select-folder-buffer-and-validate rather than
      ;; stubbing it: it is a defsubst, so once vm-postpone.el is
      ;; byte-compiled its body is inlined into the caller and
      ;; replacing its function cell does nothing.  All it wants is a
      ;; buffer that looks like a folder.
      (setq major-mode 'vm-mode)
      (cl-letf (((symbol-function 'vm-session-initialization) #'ignore)
                ((symbol-function 'vm-follow-summary-cursor) #'ignore)
                ((symbol-function 'vm-show-current-message) #'ignore))
        (vm-continue-postponed-message t)
        (setq result (buffer-string))))
    ;; Continuing the draft makes a composition buffer, which outlives the
    ;; temp folder buffer `vm-test-with-folder' takes away (issue #559).  Its
    ;; kill hook would ask whether to save it as a draft, and a question in
    ;; batch reads stdin.
    (dolist (buffer (buffer-list))
      (unless (memq buffer before)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t)
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    result))

(ert-deftest vm-postpone-test-continue-keeps-content-transfer-encoding ()
  "Test that Content-Transfer-Encoding survives with the other MIME headers.
Keeping Content-Type but dropping Content-Transfer-Encoding leaves the
body declared as the default 7bit when it is not."
  (let ((composition (vm-postpone-test-continue nil)))
    (should (string-match "^MIME-Version: 1\\.0$" composition))
    (should (string-match "boundary=\"SEP\"" composition))
    (should (string-match "^Content-Transfer-Encoding: 8bit$" composition))))

(ert-deftest vm-postpone-test-continue-raw-body-keeps-mime-headers ()
  "Test that a raw body is never copied without its MIME headers.
Regression test for the failure described in issue #141: when the
headers say nothing about MIME but the body still carries boundary
lines, sending re-encodes the whole thing and the attachments are lost.
`vm-mime-decoded' is set here with no presentation buffer, so the body
inserted is the raw one and the headers must be kept to match."
  (let ((composition (vm-postpone-test-continue 'decoded)))
    ;; the raw boundary lines did get copied ...
    (should (string-match "^--SEP$" composition))
    ;; ... so the headers describing them must be there too
    (should (string-match "^MIME-Version: 1\\.0$" composition))
    (should (string-match "boundary=\"SEP\"" composition))))


;;; Leaving Emacs with a composition unfinished (#160)

(defmacro vm-postpone-test-with-composition (&rest body)
  "Start a composition from a folder and run BODY with it as `composition'.
Everything is torn down afterwards, and the postponed folder is a file in a
temporary directory, bound as `drafts'."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-postpone-exit" t)))
          (file (expand-file-name "folder" dir))
          (drafts (expand-file-name "drafts" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-frame-per-composition nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-directory dir)
          (vm-postponed-folder "drafts")
          ;; Killing a composition asks whether to keep it as a draft, which
          ;; the teardown below would trip over.  Each test binds this to
          ;; whatever it is about.
          (vm-save-killed-message nil)
          (vm-save-killed-messages-folder "drafts")
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-composition-buffer-count vm-composition-buffer-count)
          (vm-ml-composition-buffer-count vm-ml-composition-buffer-count)
          (vm-compositions-exist vm-compositions-exist)
          (vm-current-warning vm-current-warning)
          (vm-summary-tokenized-compiled-format-alist
           vm-summary-tokenized-compiled-format-alist)
          (before (buffer-list))
          composition)
     (require 'vm)
     (require 'vm-postpone)
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "From a@example.com  Thu Jan  1 00:00:00 2026\n"
                     "From: a@example.com\nSubject: s\n\nbody\n"))
           (vm-visit-folder file)
           (vm-mail-from-folder)
           (setq composition (current-buffer))
           (goto-char (point-max))
           (insert "a few words\n")
           (set-buffer-modified-p t)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-postpone-test-a-composition-is-recognised ()
  "A composition VM started is one; another package\='s is not.
Mail mode alone is not the test -- postponing somebody else\='s composition
into a VM folder is not VM\='s business."
  (vm-postpone-test-with-composition
    (should (vm-composition-buffer-p composition))
    (should (vm-composition-worth-keeping-p composition))
    (should (memq composition (vm-unfinished-compositions))))
  (with-temp-buffer
    (mail-mode)
    (insert "To: you@example.com\n" mail-header-separator "\ntext\n")
    (set-buffer-modified-p t)
    (should-not (vm-composition-buffer-p))))

(ert-deftest vm-postpone-test-an-empty-composition-is-not-worth-keeping ()
  "A composition begun and abandoned untouched raises no question."
  (vm-postpone-test-with-composition
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (delete-region (point) (point-max))
    (should-not (vm-composition-worth-keeping-p composition))
    (should-not (memq composition (vm-unfinished-compositions)))))

(ert-deftest vm-postpone-test-exit-postpones-without-asking ()
  "With `vm-save-killed-message\=' `always\=', leaving Emacs writes the draft."
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message 'always))
      (should (vm-postpone-unfinished-compositions))
      (should-not (buffer-live-p composition))
      (should (file-exists-p drafts))
      (with-temp-buffer
        (insert-file-contents drafts)
        (should (string-match-p "a few words" (buffer-string)))))))

(ert-deftest vm-postpone-test-exit-asks-and-takes-no-for-an-answer ()
  "With `ask\=', declining leaves the composition alone."
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message 'ask)
          (asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) nil)))
        (should (vm-postpone-unfinished-compositions)))
      (should (string-match-p "as draft" asked))
      (should-not (file-exists-p drafts)))))

(ert-deftest vm-postpone-test-exit-asks-and-takes-yes ()
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message 'ask))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
        (should (vm-postpone-unfinished-compositions)))
      (should-not (buffer-live-p composition))
      (should (file-exists-p drafts)))))

(ert-deftest vm-postpone-test-exit-can-be-left-to-emacs ()
  "With `vm-save-killed-message\=' nil nothing happens, as before."
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (error "asked when it should not have"))))
        (should (vm-postpone-unfinished-compositions)))
      (should (buffer-live-p composition))
      (should-not (file-exists-p drafts)))))

(ert-deftest vm-postpone-test-a-failure-does-not-block-the-exit ()
  "Emacs still leaves when a draft cannot be written.
Losing a draft is a reason to say so, not to stand in the doorway."
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message 'always)
          (vm-current-warning nil))
      (cl-letf (((symbol-function 'kill-buffer)
                 (lambda (&rest _) (error "disk on fire"))))
        (should (vm-postpone-unfinished-compositions)))
      (should (buffer-live-p composition)))))

(ert-deftest vm-postpone-test-the-exit-hook-is-registered ()
  "Starting VM puts the offer on `kill-emacs-query-functions\='.
Not done as vm-postpone.el loads: loading a file should not change how Emacs
behaves."
  (should (memq 'vm-postpone-unfinished-compositions
                kill-emacs-query-functions)))

(ert-deftest vm-postpone-test-deletes-the-auto-save-file ()
  "REGRESSION: postponing a composition takes its auto-save file with it.
A composition buffer visits no file, and VM points its auto-saves at
`vm-mail-auto-save-directory' or, failing that, `vm-folder-directory'.
Emacs deletes a fileless buffer's auto-save file when `mail-send' succeeds
and at no other time -- not when the buffer is killed -- so every postponed
composition left one `#mail%20to%20...#' file behind, in among the folders."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-postpone" t)))
         (drafts (expand-file-name "drafts" dir))
         (auto-save nil))
    (unwind-protect
        (let ((buffer (generate-new-buffer "mail to _ on \"a draft\"")))
          (with-current-buffer buffer
            (setq default-directory dir)
            (auto-save-mode 1)
            (mail-mode)
            (insert "To: someone@example.com\nSubject: a draft\n"
                    mail-header-separator "\nbody\n")
            (do-auto-save)
            (setq auto-save buffer-auto-save-file-name)
            (should (file-exists-p auto-save))
            (let ((vm-folder-directory dir)
                  (vm-postponed-folder "drafts")
                  (vm-default-folder-type 'From_)
                  (vm-postpone-message-hook nil)
                  (vm-postponed-message-folder-buffer nil)
                  (vm-confirm-quit nil))
              (cl-letf (((symbol-function 'vm-display) #'ignore)
                        ((symbol-function 'vm-delete-postponed-message)
                         #'ignore))
                (vm-postpone-message))))
          ;; the draft is in the folder ...
          (should (file-exists-p drafts))
          (should (string-match-p
                   "^Subject: a draft$"
                   (with-temp-buffer (insert-file-contents drafts)
                                     (buffer-string))))
          ;; ... and nothing was left in the folder directory
          (should-not (file-exists-p auto-save))
          (should (null (directory-files dir nil "\\`#"))))
      (delete-directory dir t))))

;;; Postponing into an mboxcl2 folder (emacs-vm/vm#612)

(ert-deftest vm-postpone-test-a-draft-gets-a-content-length ()
  "A draft postponed into an mboxcl2 folder carries a `Content-Length'.
Neither branch of the writer had one, so a draft written into such a folder
could not be read back -- and the folder said nothing was wrong until VM was
made to check."
  (let ((dir (file-name-as-directory (make-temp-file "vm-postpone-cl2" t))))
    (unwind-protect
        (let ((folder (expand-file-name "drafts.mboxcl2" dir))
              (vm-postpone-message-hook nil)
              (user-mail-address "me@example.com"))
          (cl-letf (((symbol-function 'vm-delete-postponed-message) #'ignore)
                    ((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-mail-mode-show-headers) #'ignore))
            (with-temp-buffer
              (insert "To: someone@example.com\nSubject: a draft\n"
                      mail-header-separator "\nUnfinished.\n")
              (vm-postpone-message folder t)))
          (with-temp-buffer
            (insert-file-contents folder)
            (should (string-match-p "^Content-Length: [0-9]+$" (buffer-string))))
          ;; and it reads back, with the strict reader that emacs-vm/vm#612 added
          (let ((vm-mboxcl2-strict t))
            (should (eq (vm-get-folder-type folder) 'mboxcl2))
            (vm-visit-folder folder)
            (should (= (length vm-message-list) 1))
            (should (equal (vm-su-subject (car vm-message-list)) "a draft"))))
      (delete-directory dir t))))

(ert-deftest vm-postpone-test-a-draft-into-an-open-folder-gets-one-too ()
  "The same when the drafts folder is already open in VM.
That branch appends to the folder buffer rather than to the file, and reads
the type from the folder while the message is in another buffer -- which the
first version of the fix got the wrong way round, so it wrote nothing."
  (let ((dir (file-name-as-directory (make-temp-file "vm-postpone-cl2" t))))
    (unwind-protect
        (let ((folder (expand-file-name "drafts.mboxcl2" dir))
              (vm-postpone-message-hook nil)
              (user-mail-address "me@example.com"))
          (write-region (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                                "From: alice@example.com\nSubject: first\n"
                                "Content-Length: 10\n\nBody one.\n")
                        nil folder nil 'quiet)
          (vm-visit-folder folder)
          (should (eq vm-folder-type 'mboxcl2))
          (cl-letf (((symbol-function 'vm-delete-postponed-message) #'ignore)
                    ((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-mail-mode-show-headers) #'ignore))
            (with-temp-buffer
              (insert "To: someone@example.com\nSubject: a draft\n"
                      mail-header-separator "\nUnfinished.\n")
              (vm-postpone-message folder t)))
          ;; the folder buffer now holds two messages, each with a length
          (with-current-buffer (vm-get-file-buffer folder)
            ;; a folder buffer is narrowed to the message on show
            (should (= 2 (save-restriction
                           (widen)
                           (cl-count-if
                            (lambda (l) (string-prefix-p "Content-Length:" l))
                            (split-string (buffer-string) "\n")))))
            (let ((buffer-read-only nil)) (vm-save-folder)))
          (let ((vm-mboxcl2-strict t))
            (vm-visit-folder folder)
            (should (= (length vm-message-list) 2))))
      (delete-directory dir t))))

(ert-deftest vm-postpone-test-a-draft-is-continued-out-of-an-mboxcl2-folder ()
  "A draft in an mboxcl2 folder is continued into a composition, cleanly.
The other half of postponing: the folder holds the draft with a
`Content-Length', and that header must not follow it into the composition and
out onto the wire.  It does not, because `vm-continue-postponed-message'
rebuilds the headers from `vm-postponed-message-headers', a keep-list -- which
is worth a test, since a keep-list is exactly the kind of thing someone
extends without thinking about mboxcl2."
  (let ((dir (file-name-as-directory (make-temp-file "vm-postpone-cont" t))))
    (unwind-protect
        (let ((folder (expand-file-name "drafts.mboxcl2" dir))
              (vm-postpone-message-hook nil)
              (vm-continue-postponed-message-hook nil)
              (user-mail-address "me@example.com"))
          (cl-letf (((symbol-function 'vm-delete-postponed-message) #'ignore)
                    ((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-mail-mode-show-headers) #'ignore))
            (with-temp-buffer
              (insert "To: someone@example.com\nSubject: a draft\n"
                      mail-header-separator "\nUnfinished business.\n")
              (vm-postpone-message folder t))
            (vm-visit-folder folder)
            (should (eq vm-folder-type 'mboxcl2))
            (setq vm-message-pointer vm-message-list)
            (vm-continue-postponed-message)
            ;; the composition has the draft, and none of the folder's
            ;; bookkeeping
            (should (string-match-p "^To: someone@example.com$" (buffer-string)))
            (should (string-match-p "^Subject: a draft$" (buffer-string)))
            (should (string-match-p "Unfinished business" (buffer-string)))
            (should-not (string-match-p "Content-Length" (buffer-string)))
            (should-not (string-match-p "X-VM-postponed-data" (buffer-string)))
            (set-buffer-modified-p nil)))
      (delete-directory dir t))))

(ert-deftest vm-postpone-test-a-draft-being-previewed-keeps-its-body ()
  "Continuing a draft that has not been shown yet copies its body.
A message being previewed has its presentation buffer narrowed to the headers
and however many lines `vm-preview-lines' says -- none, by default -- and the
body was copied from whatever was visible there.  So continuing a draft
straight from the summary, without pressing SPC first, produced a composition
with no text in it (emacs-vm/vm#621).

The state matters: this test does not show the message, which is what makes it
the failing case."
  (let ((dir (file-name-as-directory (make-temp-file "vm-postpone-prev" t))))
    (unwind-protect
        (let ((folder (expand-file-name "drafts.mbox" dir))
              (vm-postpone-message-hook nil)
              (vm-continue-postponed-message-hook nil)
              (user-mail-address "me@example.com"))
          (cl-letf (((symbol-function 'vm-delete-postponed-message) #'ignore)
                    ((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-mail-mode-show-headers) #'ignore))
            (with-temp-buffer
              (insert "To: someone@example.com\nSubject: a draft\n"
                      mail-header-separator "\nUnfinished business.\n")
              (vm-postpone-message folder t))
            (vm-visit-folder folder)
            (should (eq vm-system-state 'previewing))
            (setq vm-message-pointer vm-message-list)
            (vm-continue-postponed-message)
            (should (string-match-p "Unfinished business" (buffer-string)))
            (set-buffer-modified-p nil)))
      (delete-directory dir t))))

(provide 'vm-postpone-test)

;;; vm-postpone-test.el ends here
