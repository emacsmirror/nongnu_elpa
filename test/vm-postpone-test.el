;;; vm-postpone-test.el --- Tests for vm-postpone.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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
\(defvaralias \\='vm-pine \\='vm-postpone\) with a comment calling it a group
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

(ert-deftest vm-postpone-test-the-decode-aliases-are-gone ()
  "The vm-decode-postponed-mime-* names are no longer defined.

They were aliases from vm-pine, marked obsolete in 8.2.0 and dropped in
9.0.0; what they pointed at is still here under its own name."
  (should-not (fboundp 'vm-decode-postponed-mime-message))
  (should-not (fboundp 'vm-decode-postponed-mime-button))
  (should (fboundp 'vm-mime-convert-to-attachment-buttons))
  (should (fboundp 'vm-mime-replace-by-attachment-button)))

(ert-deftest vm-postpone-test-obsolete-fake-attachment-is-gone ()
  "REGRESSION: `vm-pine-fake-attachment-overlays' is not an alias to nothing.
It was aliased to `vm-mime-re-fake-attachment-overlays', which was deleted as
unused in 2011, so calling it signalled `void-function' and `make-obsolete'
named a replacement that did not exist either.  The alias is gone.

This test replaces one that asserted the alias was there and pointed at that
name, which is how it survived: nothing checked that the target was defined."
  (should-not (fboundp 'vm-pine-fake-attachment-overlays))
  (should-not (fboundp 'vm-mime-re-fake-attachment-overlays)))

;;; Keybinding tests

(ert-deftest vm-postpone-test-keybinding-postpone ()
  "Test C-c C-d is bound to vm-postpone-message."
  (should (eq (lookup-key vm-mail-mode-map "\C-c\C-d")
              'vm-postpone-message)))

;; The four header keys are bound by `vm-postpone-mode' since 2026, where
;; loading the file bound them before (emacs-vm/vm#788), so each turns the mode
;; on.  vm-postpone-test-loading-does-not-switch-it-on below is the other side
;; of that.

(defmacro vm-postpone-test-with-the-mode (&rest body)
  "Run BODY with `vm-postpone-mode' on, and leave it as it was."
  (declare (indent 0) (debug t))
  `(let ((vm-mail-mode-hook vm-mail-mode-hook)
         (mail-send-hook (and (boundp 'mail-send-hook) mail-send-hook))
         (vm-postpone-message-hook vm-postpone-message-hook)
         (was vm-postpone-mode))
     (unwind-protect
         (progn (vm-postpone-mode 1) ,@body)
       (unless was (vm-postpone-mode -1)))))

(ert-deftest vm-postpone-test-keybinding-return-receipt ()
  "Test C-c C-f C-a is bound to vm-mail-return-receipt-to."
  (vm-postpone-test-with-the-mode
    (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-a")
                'vm-mail-return-receipt-to))))

(ert-deftest vm-postpone-test-keybinding-priority ()
  "Test C-c C-f C-p is bound to vm-mail-priority."
  (vm-postpone-test-with-the-mode
    (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-p")
                'vm-mail-priority))))

(ert-deftest vm-postpone-test-keybinding-fcc ()
  "Test C-c C-f C-f is bound to vm-mail-fcc."
  (vm-postpone-test-with-the-mode
    (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-f")
                'vm-mail-fcc))))

(ert-deftest vm-postpone-test-keybinding-notice ()
  "Test C-c C-f C-n is bound to vm-mail-notice-requested-upon-delivery-to."
  (vm-postpone-test-with-the-mode
    (should (eq (lookup-key vm-mail-mode-map "\C-c\C-f\C-n")
                'vm-mail-notice-requested-upon-delivery-to))))

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
temporary directory, bound as `drafts'.

`vm-postpone-mode' is on inside, since 2026 that being what puts
`vm-add-save-killed-message-hook' on `vm-mail-mode-hook'; loading the file
used to (emacs-vm/vm#788)."
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
          (vm-mail-mode-hook vm-mail-mode-hook)
          (mail-send-hook (and (boundp 'mail-send-hook) mail-send-hook))
          (vm-postpone-message-hook vm-postpone-message-hook)
          ;; The mode is global and binds keys in `vm-mail-mode-map', which no
          ;; `let' restores, so it is turned back off in the teardown.
          (mode-was (and (boundp 'vm-postpone-mode) vm-postpone-mode))
          composition)
     (require 'vm)
     (require 'vm-postpone)
     (unwind-protect
         (progn
           ;; The hooks are the mode's since 2026, not the file's.
           (vm-postpone-mode 1)
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
       (unless mode-was (vm-postpone-mode -1))
       (delete-directory dir t))))

(ert-deftest vm-postpone-test-a-composition-is-recognised ()
  "A composition VM started is one; another package\\='s is not.
Mail mode alone is not the test -- postponing somebody else\\='s composition
into a VM folder is not VM\\='s business."
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

(ert-deftest vm-postpone-test-a-header-the-writer-typed-is-writing ()
  "REGRESSION: a subject typed with the body still empty is worth keeping.

`vm-composition-worth-keeping-p' looked only at what followed
`mail-header-separator', so a composition with recipients and a subject and
no body yet was neither kept as a draft nor asked about; it was killed
silently (emacs-vm/vm#856).

VM writes the headers itself, which is why they could not simply be counted.
What counts is a header differing from the one VM wrote, recorded in
`vm-composition-headers-vm-wrote' once the composition is the writer's."
  (vm-postpone-test-with-composition
    ;; take the body away, leaving the headers as VM wrote them
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (delete-region (point) (point-max))
    (set-buffer-modified-p t)
    (should-not (vm-composition-worth-keeping-p composition))
    ;; now type a subject, and nothing else
    (goto-char (point-min))
    (re-search-forward "^Subject:")
    (insert " a subject and no body")
    (set-buffer-modified-p t)
    (should (vm-composition-worth-keeping-p composition))
    (should (memq composition (vm-unfinished-compositions)))))

(ert-deftest vm-postpone-test-such-a-composition-is-kept-when-killed ()
  "Killing it files the draft, rather than dropping what was typed.
The end of the same path: `vm-save-killed-message-hook' asks
`vm-composition-worth-keeping-p' before it keeps anything (emacs-vm/vm#856)."
  (vm-postpone-test-with-composition
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (re-search-forward "^Subject:")
    (insert " kept by its subject alone")
    (set-buffer-modified-p t)
    (let ((vm-save-killed-message 'always)
          (vm-save-killed-messages-folder drafts))
      (kill-buffer composition))
    (should-not (buffer-live-p composition))
    (should (file-exists-p drafts))
    (with-temp-buffer
      (insert-file-contents drafts)
      (should (string-match-p "kept by its subject alone" (buffer-string))))))

(ert-deftest vm-postpone-test-exit-postpones-without-asking ()
  "With `vm-save-killed-message' `always', leaving Emacs writes the draft."
  (vm-postpone-test-with-composition
    (let ((vm-save-killed-message 'always))
      (should (vm-postpone-unfinished-compositions))
      (should-not (buffer-live-p composition))
      (should (file-exists-p drafts))
      (with-temp-buffer
        (insert-file-contents drafts)
        (should (string-match-p "a few words" (buffer-string)))))))

(ert-deftest vm-postpone-test-exit-asks-and-takes-no-for-an-answer ()
  "With `ask', declining leaves the composition alone."
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
  "With `vm-save-killed-message' nil nothing happens, as before."
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
  "Starting VM puts the offer on `kill-emacs-query-functions'.
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

;;; Filing a copy automatically (emacs-vm/vm#667)

(defmacro vm-postpone-test--auto-fccing (settings &rest body)
  "Run BODY in a composition with SETTINGS bound, ready for `vm-mail-auto-fcc'.
DIR is a temporary directory bound to `vm-folder-directory'."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-fcc" t)))
          (vm-folder-directory dir)
          (vm-mail-folder-alist nil)
          (mail-archive-file-name nil))
     (unwind-protect
         (let ,settings
           (vm-postpone-test-with-mail-buffer
             ,@body))
       (delete-directory dir t))))

(defun vm-postpone-test--fcc ()
  "The FCC header of the composition, or nil."
  (vm-mail-mode-get-header-contents "FCC:"))

(ert-deftest vm-postpone-test-auto-fcc-files-where-the-alist-says ()
  "The folder chosen for the message is written as an FCC header, under
`vm-folder-directory' -- so a reply is filed where the reader's rules say
rather than wherever Emacs happened to be."
  (vm-postpone-test--auto-fccing
      ((vm-mail-folder-alist '(("To:" ("recipient" . "recipient-folder")))))
    (vm-mail-auto-fcc)
    (should (equal (vm-postpone-test--fcc)
                   (vm-abbreviate-file-name
                    (expand-file-name "recipient-folder" dir))))))

(ert-deftest vm-postpone-test-auto-fcc-falls-back-to-the-recipient ()
  "With no rule matching, the folder is named after the recipient.

That is the second element of `vm-mail-fcc-default', `vm-mail-to-fcc',
which takes the address out of the To header.  Its own fallback is
`mail-archive-file-name', so the third element of the default is never
reached."
  (vm-postpone-test--auto-fccing
      ((vm-mail-folder-alist '(("To:" ("nobody" . "unused"))))
       (mail-archive-file-name "archive"))
    (vm-mail-auto-fcc)
    (should (equal (vm-postpone-test--fcc)
                   (vm-abbreviate-file-name (expand-file-name "recipient" dir))))))

(ert-deftest vm-postpone-test-auto-fcc-uses-the-archive-with-no-recipient ()
  "With nothing to take an address from, `mail-archive-file-name' is what
is left."
  (vm-postpone-test--auto-fccing
      ((mail-archive-file-name "archive"))
    (vm-mail-mode-remove-header "To:")
    (vm-mail-auto-fcc)
    (should (equal (vm-postpone-test--fcc)
                   (vm-abbreviate-file-name (expand-file-name "archive" dir))))))

(ert-deftest vm-postpone-test-auto-fcc-writes-one-header ()
  "Running it twice leaves one FCC header, not two.

It is meant for `vm-reply-hook', and a composition continued or replied
from again would otherwise collect a header per run -- and be filed once
per header."
  (vm-postpone-test--auto-fccing
      ((vm-mail-folder-alist '(("To:" ("recipient" . "recipient-folder")))))
    (vm-mail-auto-fcc)
    (vm-mail-auto-fcc)
    (goto-char (point-min))
    (should (= 1 (count-matches "^FCC:" (point-min)
                                (save-excursion
                                  (re-search-forward
                                   (concat "^" (regexp-quote mail-header-separator) "$"))
                                  (point)))))))

(ert-deftest vm-postpone-test-auto-fcc-replaces-a-header-it-finds ()
  "An FCC already in the composition is replaced rather than kept beside
the new one."
  (vm-postpone-test--auto-fccing
      ((vm-mail-folder-alist '(("To:" ("recipient" . "recipient-folder")))))
    (goto-char (point-min))
    (insert "FCC: somewhere-else\n")
    (vm-mail-auto-fcc)
    (should-not (string-match-p "somewhere-else" (buffer-string)))
    (should (string-match-p "recipient-folder" (vm-postpone-test--fcc)))))

(ert-deftest vm-postpone-test-auto-fcc-adds-nothing-when-it-has-no-name ()
  "A `vm-mail-fcc-default' that yields nothing writes no header, rather
than filing the copy somewhere arbitrary."
  (vm-postpone-test--auto-fccing
      ((vm-mail-fcc-default nil))
    (vm-mail-auto-fcc)
    (should-not (vm-postpone-test--fcc))))

(ert-deftest vm-postpone-test-auto-fcc-refuses-a-directory ()
  "REGRESSION: a name that resolves to a directory is refused.

The check was made on the name as the rules gave it, and the header
written from that name joined to `vm-folder-directory' -- so it tested one
file and wrote another.  A folder name that was a directory under
`vm-folder-directory' passed it, and VM went on to file a copy into a
directory."
  (vm-postpone-test--auto-fccing
      ((vm-mail-folder-alist '(("To:" ("recipient" . "a-directory"))))
       (text-quoting-style 'grave))
    (make-directory (expand-file-name "a-directory" dir))
    (let ((err (should-error (vm-mail-auto-fcc) :type 'error)))
      (should (string-match-p "a-directory" (error-message-string err)))
      (should (string-match-p "is a directory" (error-message-string err)))
      (should (string-match-p "vm-mail-folder-alist"
                              (error-message-string err))))
    ;; and nothing was written
    (should-not (vm-postpone-test--fcc))))

;;; The summary of a folder of mail you sent (emacs-vm/vm#668)

(defun vm-postpone-test--summary-f (headers &optional uninteresting)
  "Return `vm-summary-function-f' for a message with HEADERS.
UNINTERESTING is `vm-summary-uninteresting-senders', me@example.com by
default -- the reader themselves, whose name the summary of a sent-mail
folder should not be full of."
  (vm-test-with-folder
      (concat "From me@example.com Mon Jan  1 00:00:00 2024\n"
              headers "\n" "The body.\n")
    (let ((vm-summary-uninteresting-senders
           (or uninteresting "me@example\\.com")))
      (vm-summary-function-f (car vm-message-list)))))

(ert-deftest vm-postpone-test-summary-f-shows-an-interesting-sender ()
  "Mail from somebody else shows their address, with no label: the From
header is what a summary shows anyway."
  (should (equal (vm-postpone-test--summary-f
                  "From: Alice <alice@example.com>\nTo: me@example.com\n")
                 "alice@example.com")))

(ert-deftest vm-postpone-test-summary-f-shows-who-you-wrote-to ()
  "Mail from you shows the recipient instead, labelled with the header it
came from.  That is the point of this summary function: a folder of sent
mail otherwise says your own name on every line."
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nTo: Alice <alice@example.com>\n")
                 "To: alice@example.com"))
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nCC: Bob <bob@example.com>\n")
                 "CC: bob@example.com")))

(ert-deftest vm-postpone-test-summary-f-keeps-a-newsgroup-whole ()
  "REGRESSION: a newsgroup is shown as it is written.

The Newsgroups header went through `mail-extract-address-components',
which reads comp.emacs as somebody called \"comp emacs\" and shows the dot
as a space."
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nNewsgroups: comp.emacs\n")
                 "News:comp.emacs"))
  ;; the first of several, as for several recipients
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nNewsgroups: comp.emacs,comp.mail.misc\n")
                 "News:comp.emacs")))

(ert-deftest vm-postpone-test-summary-f-labels-the-fallback-correctly ()
  "REGRESSION: a message with nobody interesting in it shows the first
address under its own label.

`arrow' held whichever header was examined last, so mail you sent to
yourself came out as \"Resent:me@example.com\": the address from From and
the label from Resent-From."
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nTo: me@example.com\n")
                 "me@example.com")))

(ert-deftest vm-postpone-test-summary-f-has-nothing-to-say-about-nothing ()
  "A message with none of those headers gives an empty string rather than
a label with nothing after it."
  (should (equal (vm-postpone-test--summary-f "Subject: nothing\n") "")))

(ert-deftest vm-postpone-test-summary-f-with-nobody-uninteresting ()
  "With `vm-summary-uninteresting-senders' matching nobody, the From
address is always the answer, which is what the ordinary summary shows."
  (should (equal (vm-postpone-test--summary-f
                  "From: me@example.com\nTo: alice@example.com\n"
                  "\\`\\'")
                 "me@example.com")))

;;; What `vm-continue-what-message' decides to do

(defconst vm-postpone-test--draft
  (concat "From me@example.com Mon Jan  1 00:00:00 2024\n"
          "From: me@example.com\n"
          "To: alice@example.com\n"
          "Subject: half written\n"
          vm-postponed-header "(nil nil nil)\n"
          "\n"
          "As I was saying\n")
  "A postponed message, as VM writes one into the drafts folder.")

(defmacro vm-postpone-test--deciding (bindings &rest body)
  "Run BODY with the world `vm-continue-what-message-composing' reads.
`vm-folder-directory' is a temp directory, available to BODY as `dir',
and `vm-postponed-folder' names \"postponed\" in it.  Nothing is being
composed, no prefix argument was given, and `vm-continue-what-message'
is `ask'.  BINDINGS are let bindings on top of that.

A question is an error unless the test stubs `y-or-n-p' itself: batch ert
has a terminal to read the answer from, so a question nothing answers
hangs the run rather than failing it."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-postpone-test-" t)))
          (vm-folder-directory dir)
          (vm-postponed-folder "postponed")
          (vm-continue-what-message 'ask)
          (current-prefix-arg nil)
          ,@bindings)
     (unwind-protect
         (cl-letf (((symbol-function 'vm-session-initialization) #'ignore)
                   ((symbol-function 'vm-find-composition-buffer) #'ignore)
                   ((symbol-function 'y-or-n-p)
                    (lambda (prompt) (error "Asked unexpectedly: %s" prompt))))
           ,@body)
       (delete-directory dir t))))

(defun vm-postpone-test--write-drafts (dir &optional contents)
  "Write CONTENTS, one draft by default, as the drafts folder in DIR."
  (let ((file (expand-file-name "postponed" dir)))
    (with-temp-file file
      (insert (or contents vm-postpone-test--draft)))
    file))

(defun vm-postpone-test--open-drafts (dir)
  "Visit the drafts folder in DIR as VM would leave it: a folder buffer
whose message pointer is on an undeleted draft."
  (let ((buffer (find-file-noselect (vm-postpone-test--write-drafts dir))))
    (with-current-buffer buffer
      (vm-test-init-folder-variables)
      (vm-build-message-list)
      (dolist (m vm-message-list) (vm-test-init-message-data m))
      (setq vm-message-pointer vm-message-list))
    buffer))

(ert-deftest vm-postpone-test-nothing-half-written-starts-a-new-message ()
  "With no composition, no draft under the cursor and no drafts folder on
disk, there is nothing to continue."
  (vm-postpone-test--deciding ()
    (should (eq (vm-continue-what-message-composing) 'new))))

(ert-deftest vm-postpone-test-a-composition-in-progress-is-continued ()
  "A composition buffer is what you meant, whatever is in the drafts
folder."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir)
    (cl-letf (((symbol-function 'vm-find-composition-buffer)
               (lambda (&optional _) (current-buffer))))
      (should (eq (vm-continue-what-message-composing) 'continue)))))

(ert-deftest vm-postpone-test-a-prefix-argument-forces-a-continue ()
  "C-u says continue even with nothing to continue, which is how the
command offers the drafts folder anyway."
  (vm-postpone-test--deciding ((current-prefix-arg '(4)))
    (should (eq (vm-continue-what-message-composing) 'force-continue))))

(ert-deftest vm-postpone-test-a-draft-under-the-cursor-is-continued ()
  "A message in the current folder carrying `vm-postponed-header' is a
draft, so it is continued rather than the drafts folder visited."
  (vm-postpone-test--deciding ()
    (vm-test-with-folder vm-postpone-test--draft
      (let ((major-mode 'vm-mode))
        (should (eq (vm-continue-what-message-composing) 'continue))))))

(ert-deftest vm-postpone-test-an-ordinary-message-under-the-cursor-is-not ()
  "A message without the postponed header is mail, not a draft."
  (vm-postpone-test--deciding ()
    (vm-test-with-folder
        (concat "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                "From: alice@example.com\nSubject: mail\n\nBody\n")
      (let ((major-mode 'vm-mode))
        (should (eq (vm-continue-what-message-composing) 'new))))))

(ert-deftest vm-postpone-test-a-deleted-draft-under-the-cursor-is-not ()
  "A draft you have marked for deletion is not one to continue."
  (vm-postpone-test--deciding ()
    (vm-test-with-folder vm-postpone-test--draft
      ;; set the flag itself: vm-set-deleted-flag records undo and asks
      ;; for a display update, neither of which this is about
      (aset (vm-attributes-of (car vm-message-list)) 2 t)
      (let ((major-mode 'vm-mode))
        (should (eq (vm-continue-what-message-composing) 'new))))))

(ert-deftest vm-postpone-test-a-drafts-folder-on-disk-is-visited ()
  "Drafts saved in an earlier session are found by their folder, which VM
offers to visit."
  (vm-postpone-test--deciding ((vm-continue-what-message 'continue))
    (vm-postpone-test--write-drafts dir)
    (should (eq (vm-continue-what-message-composing) 'visit))))

(ert-deftest vm-postpone-test-an-empty-drafts-folder-holds-no-drafts ()
  "An empty drafts folder is what expunging every draft leaves behind, and
visiting it would show nothing."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir "")
    (should (eq (vm-continue-what-message-composing) 'new))))

(ert-deftest vm-postpone-test-a-drafts-folder-already-open-is-visited ()
  "The drafts folder open but off screen is visited, which selects the
buffer that is already there."
  (vm-postpone-test--deciding ((vm-continue-what-message 'continue))
    (let ((buffer (vm-postpone-test--open-drafts dir)))
      (unwind-protect
          (should (eq (vm-continue-what-message-composing) 'visit))
        (kill-buffer buffer)))))

(ert-deftest vm-postpone-test-a-drafts-folder-on-screen-asks-you-to-pick ()
  "The drafts folder already in a window is not visited again: VM selects
that window and leaves the choice of draft to you."
  (vm-postpone-test--deciding ()
    (let ((buffer (vm-postpone-test--open-drafts dir)))
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (should (eq (vm-continue-what-message-composing) 'none))
            (should (eq (window-buffer (selected-window)) buffer)))
        (kill-buffer buffer)))))

(ert-deftest vm-postpone-test-an-open-drafts-folder-wins-over-the-file ()
  "The open buffer is what is visited, not what is on disk: drafts written
in this session are in the buffer before they are in the file."
  (vm-postpone-test--deciding ((vm-continue-what-message 'continue))
    (let ((buffer (find-file-noselect (vm-postpone-test--write-drafts dir ""))))
      (unwind-protect
          (should (eq (vm-continue-what-message-composing) 'visit))
        (kill-buffer buffer)))))

(ert-deftest vm-postpone-test-a-deleted-draft-on-screen-is-not-yours-to-pick ()
  "The drafts folder on screen showing a draft marked for deletion is not
a draft to pick, so VM goes on and visits the folder."
  (vm-postpone-test--deciding ((vm-continue-what-message 'continue))
    (let ((buffer (vm-postpone-test--open-drafts dir)))
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (with-current-buffer buffer
              (aset (vm-attributes-of (car vm-message-pointer)) 2 t))
            (should (eq (vm-continue-what-message-composing) 'visit)))
        (kill-buffer buffer)))))

(ert-deftest vm-postpone-test-never-continuing-never-continues ()
  "`vm-continue-what-message' nil is never continue, drafts or no drafts.
Which of the two it is, it says: `declined' where there are drafts it is not
continuing, `new' where there are none.  Neither continues anything, and the
difference is whether there is anything to say about drafts."
  (vm-postpone-test--deciding ((vm-continue-what-message nil))
    (should (eq (vm-continue-what-message-composing) 'new))
    (vm-postpone-test--write-drafts dir)
    (should (eq (vm-continue-what-message-composing) 'declined))))

(ert-deftest vm-postpone-test-asking-takes-no-for-a-decline ()
  "`ask' asks before visiting the drafts folder, and no is a decline.
Not `new': the drafts are there, which is what the question was about, and
answering it no is not the same as there being none to answer about."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (should (eq (vm-continue-what-message-composing) 'declined)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (should (eq (vm-continue-what-message-composing) 'visit)))))

(ert-deftest vm-postpone-test-declining-says-nothing-about-drafts ()
  "Answering no says nothing, rather than that there are no known drafts.
That is what a reader saw: the question named the drafts, and declining it
answered that there were none -- with the drafts still sitting in the folder
VM had just offered to visit."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir)
    (let (said)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-mail) #'ignore)
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (setq said (apply #'format format args)))))
        (vm-continue-what-message))
      (should-not said))))

(ert-deftest vm-postpone-test-no-drafts-at-all-still-says-so ()
  "With no drafts anywhere the message stands: there is nothing to continue
and a keystroke that does nothing silently is one nobody can read."
  (vm-postpone-test--deciding ()
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (setq said (apply #'format format args)))))
        (vm-continue-what-message))
      (should (equal said "There are no known drafts.")))))

(ert-deftest vm-postpone-test-declining-composes-where-that-is-asked-for ()
  "`vm-zero-drafts-start-compose' still composes on a decline.
The option decides what a keystroke with nothing to continue does, and that
is unchanged: only the sentence about there being no drafts has gone."
  (vm-postpone-test--deciding ((vm-zero-drafts-start-compose t))
    (vm-postpone-test--write-drafts dir)
    (let (composed)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-mail)
                 (lambda (&rest _) (setq composed t))))
        (vm-continue-what-message))
      (should composed))))


(ert-deftest vm-postpone-test-asking-is-only-about-the-drafts-folder ()
  "`ask' asks about visiting the drafts folder and nothing else: a
composition in progress is continued without a question."
  (vm-postpone-test--deciding ()
    (cl-letf (((symbol-function 'vm-find-composition-buffer)
               (lambda (&optional _) (current-buffer))))
      (should (eq (vm-continue-what-message-composing) 'continue)))))

(ert-deftest vm-postpone-test-continuing-visits-the-drafts-folder-unasked ()
  "`continue' is the answer to the question `ask' would have put, so it is
not put."
  (vm-postpone-test--deciding ((vm-continue-what-message 'continue))
    (vm-postpone-test--write-drafts dir)
    (should (eq (vm-continue-what-message-composing) 'visit))))

(ert-deftest vm-postpone-test-declining-starts-a-new-message ()
  "Answering no composes: the drafts were offered and refused, and the key
that offered them is the compose key.  It used to do nothing at all, which
made \\[vm-continue-what-message] a dead keystroke for anyone with a draft on
disk -- the manual binds the other-window command to \\`C-x m\\', where doing
nothing loses `compose-mail' as well."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir)
    (let (composed)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-mail)
                 (lambda (&rest _) (setq composed t))))
        (vm-continue-what-message))
      (should composed))))

(ert-deftest vm-postpone-test-declining-in-another-window-uses-that-window ()
  "`vm-continue-what-message-other-window' composes in the other window.
WHERE picks the command by name, so a decline has to reach
`vm-mail-other-window' and not `vm-mail'."
  (vm-postpone-test--deciding ()
    (vm-postpone-test--write-drafts dir)
    (let (composed)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-mail)
                 (lambda (&rest _) (setq composed 'here)))
                ((symbol-function 'vm-mail-other-window)
                 (lambda (&rest _) (setq composed 'other-window))))
        (vm-continue-what-message-other-window))
      (should (eq composed 'other-window)))))

(ert-deftest vm-postpone-test-never-continuing-composes-instead ()
  "`vm-continue-what-message' nil declines every time, so it composes every
time: the drafts are never continued and are left where they are."
  (vm-postpone-test--deciding ((vm-continue-what-message nil))
    (vm-postpone-test--write-drafts dir)
    (let (composed)
      (cl-letf (((symbol-function 'vm-mail)
                 (lambda (&rest _) (setq composed t))))
        (vm-continue-what-message))
      (should composed))))

(ert-deftest vm-postpone-test-declining-leaves-the-drafts-alone ()
  "Composing on a decline writes nothing to the drafts folder: the draft
that was refused is still there to be continued later."
  (vm-postpone-test--deciding ()
    (let ((file (vm-postpone-test--write-drafts dir)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-mail) #'ignore))
        (vm-continue-what-message))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     vm-postpone-test--draft)))))

(ert-deftest vm-postpone-test-the-drafts-on-screen-are-not-said-to-be-missing ()
  "The drafts folder on screen asks for a draft and says nothing else.
It said \"Please select a draft!\" and then \"There are no known drafts.\" over
the top of it, of the very drafts the reader was being asked to pick from."
  (vm-postpone-test--deciding ()
    (let ((buffer (vm-postpone-test--open-drafts dir))
          (said nil))
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) said))))
              (vm-continue-what-message))
            (should (equal (nreverse said) (list "Please select a draft!"))))
        (kill-buffer buffer)))))


;;; The mode, and loading not switching it on (emacs-vm/vm#788)

(ert-deftest vm-postpone-test-loading-does-not-switch-it-on ()
  "Loading vm-postpone does not bind a header key or add a hook.
Customize loads this file whenever it is asked about a VM option, so loading
had to stop meaning enabling."
  (require 'vm-postpone)
  (let ((vm-mail-mode-hook nil)
        (vm-postpone-mode nil))
    (should-not (memq 'vm-add-save-killed-message-hook vm-mail-mode-hook))
    (dolist (binding vm-postpone-key-bindings)
      (should-not (eq (lookup-key vm-mail-mode-map (car binding))
                      (cdr binding))))))

(ert-deftest vm-postpone-test-mode-toggles-the-keys-and-the-hooks ()
  "The mode binds four header keys and three hooks, and undoes both."
  (require 'vm-postpone)
  (let ((vm-mail-mode-hook nil)
        (mail-send-hook nil)
        (vm-postpone-message-hook nil)
        (vm-postpone-mode nil))
    (vm-postpone-mode 1)
    (dolist (binding vm-postpone-key-bindings)
      (should (eq (lookup-key vm-mail-mode-map (car binding)) (cdr binding))))
    (dolist (pair vm-postpone-hooks)
      (should (memq (cdr pair) (symbol-value (car pair)))))
    (vm-postpone-mode -1)
    (dolist (binding vm-postpone-key-bindings)
      (should-not (eq (lookup-key vm-mail-mode-map (car binding))
                      (cdr binding))))
    (dolist (pair vm-postpone-hooks)
      (should-not (memq (cdr pair) (symbol-value (car pair)))))))

(ert-deftest vm-postpone-test-C-c-C-d-is-VMs-own-and-survives-the-mode ()
  "Turning the mode off leaves C-c C-d bound to `vm-postpone-message'.
This file bound that key as it loaded, but `vm-mail-mode-map' in vm-vars.el
already binds it to the same command, so the binding here was a no-op.  It is
deliberately not among `vm-postpone-key-bindings': a mode that unbound it
would take away a binding VM's core owns, and the command it runs is
autoloaded, so it works with the mode off."
  (require 'vm-postpone)
  (should-not (assoc "\C-c\C-d" vm-postpone-key-bindings))
  (let ((vm-postpone-mode nil))
    (vm-postpone-mode 1)
    (vm-postpone-mode -1)
    (should (eq (lookup-key vm-mail-mode-map "\C-c\C-d")
                'vm-postpone-message))))

(ert-deftest vm-postpone-test-unbinding-lets-mail-mode-show-through ()
  "Turning the mode off removes the entry rather than binding it to nil.
`vm-mail-mode-map' has `mail-mode-map' for its parent and Mail mode binds
C-c C-f C-a to `mail-mail-reply-to'.  A nil binding in the child shadows the
parent instead of falling through, which would leave the key dead rather than
Mail mode's.  Needs `keymap-unset', which arrived in Emacs 29; on 28 the nil
is the best available, so the test asks for the fall-through only where the
function is there to do it."
  (require 'vm-postpone)
  (require 'sendmail)
  (skip-unless (fboundp 'keymap-unset))
  (let ((vm-postpone-mode nil)
        (composed nil))
    (vm-postpone-mode 1)
    (vm-postpone-mode -1)
    (setq composed (make-composed-keymap vm-mail-mode-map mail-mode-map))
    (should (eq (lookup-key composed "\C-c\C-f\C-a") 'mail-mail-reply-to))))

(ert-deftest vm-postpone-test-the-mode-leaves-the-keymap-as-it-found-it ()
  "On and then off leaves `vm-mail-mode-map' equal to what it was.
`define-key' on a multi-key sequence makes the intermediate keymap it needs,
and unbinding the leaf does not take it away: this left an empty keymap for
C-c C-f where there had been no entry for that prefix, which is a keymap
changed behind the mode and what `test-runner --leaks' reported.

Compares the whole keymap rather than the four keys, that being the only way
to see a residue nobody thought to look for."
  (require 'vm-postpone)
  (skip-unless (fboundp 'keymap-unset))
  (let ((before (copy-tree vm-mail-mode-map))
        (vm-mail-mode-hook vm-mail-mode-hook)
        (mail-send-hook (and (boundp 'mail-send-hook) mail-send-hook))
        (vm-postpone-message-hook vm-postpone-message-hook)
        (was vm-postpone-mode))
    (unwind-protect
        (progn
          (vm-postpone-mode 1)
          (should-not (equal before vm-mail-mode-map))
          (vm-postpone-mode -1)
          (should (equal before vm-mail-mode-map)))
      (when was (vm-postpone-mode 1)))))

(ert-deftest vm-postpone-test-postponing-does-not-ask ()
  "REGRESSION: postponing a composition with writing in it asks nothing.

The guard on `kill-buffer-query-functions' asks whether killing will keep the
writing, and `vm-postpone-message-hook' takes `vm-save-killed-message-hook'
off before the kill -- rightly, the draft being in the folder by then.  So the
guard saw a composition with writing that nothing would save, and postponing
asked \"has writing in it and has not been sent; kill it?\" over a composition
it had just filed."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-postpone-ask" t)))
         (drafts (expand-file-name "drafts" dir))
         (asked nil)
         (before (buffer-list)))
    (unwind-protect
        (let ((vm-folder-directory dir)
              (vm-postponed-folder "drafts")
              (vm-default-folder-type 'From_)
              (vm-confirm-killing-a-composition t)
              (vm-postponed-message-folder-buffer nil))
          (cl-letf (((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-delete-postponed-message) #'ignore)
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (push prompt asked) t))
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt) (push prompt asked) t)))
            (vm-mail)
            (goto-char (point-max))
            (insert "Writing worth keeping.\n")
            (vm-postpone-message))
          (should (equal nil asked))
          (should (file-exists-p drafts)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
      (delete-directory dir t))))

(ert-deftest vm-postpone-test-the-after-hook-runs-with-the-draft-filed ()
  "`vm-postponed-message-hook' runs after the draft is in the folder.
In the composition buffer, so a function there can still read what was
postponed, and after the file exists, which is what \"postponed\" means."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-postpone-hook" t)))
         (drafts (expand-file-name "drafts" dir))
         (saw nil)
         (before (buffer-list)))
    (unwind-protect
        (let ((vm-folder-directory dir)
              (vm-postponed-folder "drafts")
              (vm-default-folder-type 'From_)
              (vm-postponed-message-folder-buffer nil)
              (vm-postponed-message-hook
               (list (lambda ()
                       (setq saw (list :filed (file-exists-p drafts)
                                       :mode major-mode
                                       :live (buffer-live-p
                                              (current-buffer))))))))
          (cl-letf (((symbol-function 'vm-display) #'ignore)
                    ((symbol-function 'vm-delete-postponed-message) #'ignore))
            (vm-mail)
            (goto-char (point-max))
            (insert "Body.\n")
            (vm-postpone-message))
          (should (equal saw (list :filed t :mode 'mail-mode :live t))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
      (delete-directory dir t))))

(provide 'vm-postpone-test)

;;; vm-postpone-test.el ends here
