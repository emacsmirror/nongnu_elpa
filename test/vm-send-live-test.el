;;; vm-send-live-test.el --- VM sending real mail -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; These send mail and then read the mailbox it arrived in.  Nothing is stubbed:
;; `vm-mail-send' runs, the message leaves by whatever route the machine is set
;; up for, and the test waits for it.
;;
;; Run with: make test-send.  See vm-send-live-init.el for the config, and
;; issue #607 for why this exists: every other test stubs `mail-send', so a
;; fault in the send path could only be found by hand.
;;
;; Each test sends to the one address the config names, marks its messages with
;; an `X-VM-Send-Test' header, and deletes them again.

;;; Code:

(require 'vm-test-init)
(require 'vm-send-live-init)
(require 'vm)

(ert-deftest vm-send-live-test-inert-without-a-config ()
  "Without a `vm-send-test-config' nothing here sends anything.
The check that an unconfigured checkout stays off the network -- and out of
anyone's mailbox."
  (let ((vm-send-test-config nil))
    (should-not (vm-send-live-configured-p))))

(ert-deftest vm-send-live-test-a-message-arrives ()
  "A message composed in VM and sent arrives, headers and body intact.
The simplest thing VM does, and the thing no other test does at all."
  (vm-send-live-skip-unless-configured)
  (vm-send-live-with-delivery (conn mailbox)
    (let (subject)
      (vm-send-live-with-composition (s "plain")
        (setq subject s)
        (insert "One plain line of body.\n"))
      (let ((n (vm-send-live-await conn mailbox subject)))
        (let ((text (vm-imap-live-cmd-ok conn "FETCH %s (BODY.PEEK[])" n)))
          (should (string-match-p (regexp-quote subject) text))
          (should (string-match-p "One plain line of body" text))
          (should (string-match-p (regexp-quote (vm-send-live-config :from))
                                  text)))))))

(ert-deftest vm-send-live-test-an-accented-subject-is-encoded ()
  "A Subject outside US-ASCII arrives encoded, not as raw bytes.
It went out raw whenever `vm-send-using-mime' was nil until the encoding was
moved into the send path (emacs-vm/vm#606), and no test could see it: the
encoding happens on the way out.

Which charset is VM's business -- an e-acute arrived as
=?iso-8859-1?Q?caf=E9?=, and insisting on UTF-8 here failed a message that was
perfectly well encoded."
  (vm-send-live-skip-unless-configured)
  (vm-send-live-with-delivery (conn mailbox)
    (let (subject)
      (vm-send-live-with-composition (s "accented café")
        (setq subject s)
        (insert "Body.\n"))
      ;; The subject on the wire is RFC 2047 encoded, so searching for the
      ;; plain text finds nothing.  Search for the run and message number at
      ;; the end of it: ASCII, and unlike the words at the front it does not
      ;; also match what an earlier run left behind.
      (let ((n (vm-send-live-await
                conn mailbox (car (last (split-string subject " "))))))
        (let ((text (vm-imap-live-cmd-ok conn "FETCH %s (BODY.PEEK[HEADER])" n)))
          (should (string-match-p "Subject:[^\n]*=\\?[^?]+\\?[QqBb]\\?" text))
          (should-not (string-match-p "café" text)))))))

(ert-deftest vm-send-live-test-a-long-line-arrives-whole ()
  "A line too long to send unencoded arrives as the one line it was.
emacs-vm/vm#593.  The point of quoted-printable here is that the recipient
puts it back together, which is only checkable by receiving it."
  (vm-send-live-skip-unless-configured)
  (vm-send-live-with-delivery (conn mailbox)
    (let ((line (make-string 1200 ?x))
          subject)
      (vm-send-live-with-composition (s "long line")
        (setq subject s)
        (insert line "\n"))
      (let ((n (vm-send-live-await conn mailbox subject)))
        (let ((text (vm-imap-live-cmd-ok conn "FETCH %s (BODY.PEEK[])" n)))
          ;; on the wire it is folded, with soft breaks
          (should (string-match-p "quoted-printable" (downcase text)))
          (should (string-match-p "=\r?\n" text))
          ;; and no line of it breaks what RFC 5322 allows
          (dolist (l (split-string text "\r?\n"))
            (should (<= (length l) 998))))))))

(ert-deftest vm-send-live-test-an-fcc-copy-is-filed ()
  "A send with an Fcc files the copy, and the composition keeps the header.
emacs-vm/vm#597 gave VM its own FCC.  Every other test of it stubs
`mail-send', so this is the only one that watches a real send do it."
  (vm-send-live-skip-unless-configured)
  (vm-send-live-with-delivery (conn mailbox)
    (let* ((dir (file-name-as-directory (make-temp-file "vm-send-fcc" t)))
           (folder (expand-file-name "sent" dir))
           subject)
      (unwind-protect
          (progn
            (vm-send-live-with-composition (s "fcc")
              (setq subject s)
              (save-excursion
                (goto-char (point-min))
                (insert "Fcc: " folder "\n"))
              (insert "Filed as it was sent.\n"))
            ;; the copy is on disk, as a folder VM can read
            (should (file-exists-p folder))
            (with-temp-buffer
              (insert-file-contents folder)
              (should (string-match-p (regexp-quote subject) (buffer-string)))
              (should (string-match-p "^From " (buffer-string)))
              ;; the filed copy does not carry the Fcc header
              (should-not (string-match-p "^Fcc:" (buffer-string))))
            ;; and the message still arrived
            (vm-send-live-await conn mailbox subject))
        (delete-directory dir t)))))

(provide 'vm-send-live-test)

;;; vm-send-live-test.el ends here
