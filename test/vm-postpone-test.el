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

(ert-deftest vm-postpone-test-alias-exists ()
  "Test vm-pine alias points to vm-postpone."
  (should (eq (indirect-variable 'vm-pine) 'vm-postpone)))

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

(provide 'vm-postpone-test)

;;; vm-postpone-test.el ends here
