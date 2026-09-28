;;; vm-send-live-init.el --- Harness for the mail-sending tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; VM's suite sends no mail.  Every test stubs `mail-send', so nothing has ever
;; watched a message leave VM, travel, and arrive -- which is why sending is
;; the longest section of the manual testing a release needs (issue #607).
;; These tests close that gap for the developer who wants them to.
;;
;; Run with: make test-send
;;
;; Deliberately NOT part of `make test'.  The live IMAP and POP tests are, once
;; a config exists, because talking to a server on localhost costs nothing.
;; Sending mail leaves the machine, so it happens only when asked for by name.
;;
;; The opt-in is a `vm-send-test-config' in the gitignored
;; test/vm-live-config.el, which test/vm-live-config.el.template describes.
;; Without it every test here skips.
;;
;; How the sending is done is the developer's own arrangement.  The config file
;; is elisp and may set `send-mail-function', `sendmail-program',
;; `smtpmail-smtp-server' or whatever else that machine uses -- so no password
;; belongs in it: msmtp, auth-source and the like keep their own.
;;
;; Delivery is verified by reading the mailbox with the live-IMAP harness'
;; own IMAP client, not with vm-imap.el, for the same reason that harness
;; gives: a fixture built with the code under test can hide a bug in it.

;;; Code:

(require 'cl-lib)
(require 'vm-imap-live-init)

(defvar vm-send-live-enabled t
  "Whether the mail-sending tests may run.
They run only when `vm-send-test-config' is also set, so the config is the
real opt-in.  Bind this to nil to keep a configured checkout from sending
mail without editing the config.")

(defvar vm-send-test-config nil
  "How to send a test message and where to look for it.
Set in the gitignored test/vm-live-config.el; see the template.  A plist:

  :from            the From address to compose with
  :to              where the test message is sent.  Your own address: these
                   tests send real mail, and it must not reach anyone else.
  :verify-server   the name of a server in `vm-imap-test-servers' whose
                   account receives that mail.  It and :to are two ends of
                   one mailbox: a test account on a local dovecot, which is
                   what those servers usually are, receives nothing sent to
                   your address, and every test then sends its mail and waits
                   for it in a mailbox it will never reach
  :verify-mailbox  the mailbox to look in there, \"INBOX\" unless said
  :wait            seconds to wait for delivery, 600 unless said

Sending itself is not described here: the config file sets
`send-mail-function' and friends however this machine sends mail.")

(defconst vm-send-live-tag "X-VM-Send-Test"
  "Header naming a message as one of these tests', so cleanup can be exact.")

(defun vm-send-live-config (key &optional default)
  "The value of KEY in `vm-send-test-config', or DEFAULT."
  (or (plist-get vm-send-test-config key) default))

(defun vm-send-live-configured-p ()
  "Whether a usable sending configuration is present."
  (and vm-send-live-enabled
       vm-send-test-config
       (vm-send-live-config :from)
       (vm-send-live-config :to)
       (vm-send-live-config :verify-server)
       ;; and a way to send.  Any named function will do: how this machine
       ;; sends mail is the config's business, and a list of the four the
       ;; harness happened to know left a valid one reading as no
       ;; configuration at all.  `functionp' is no test either -- it answers
       ;; nil for a function in a library not loaded yet, which is what
       ;; `message-send-mail-with-sendmail' is until message.el arrives, so
       ;; the send tests skipped in silence for a config that worked.
       (or (functionp send-mail-function)
           (and send-mail-function (symbolp send-mail-function)))
       t))

(defun vm-send-live-skip-unless-configured ()
  "Skip the test unless mail can be sent and its arrival checked.
`vm-test-skip-unless' throughout: `skip-unless' is bound by `ert-deftest'
itself, with `cl-macrolet', so it does not exist in a function called from
one."
  (vm-test-skip-unless (vm-send-live-configured-p)
		       "No vm-send-test-config in test/vm-live-config.el")
  ;; The mailbox has to be reachable too, and a named-but-unreachable server
  ;; is a failure rather than a skip, as it is for the live IMAP tests.
  (vm-imap-live-skip-unless-server (vm-send-live-config :verify-server)))

(defvar vm-send-live--counter 0
  "Distinguishes two messages sent by the same run.")

(defun vm-send-live-unique-subject (what)
  "A subject naming WHAT and this run, so that it can be found and only it."
  (format "vm-send-test %s %d-%d" what (emacs-pid)
          (cl-incf vm-send-live--counter)))

(defmacro vm-send-live-with-composition (spec &rest body)
  "Compose a message, run BODY in its buffer, and send it.
SPEC is (SUBJECT-VAR WHAT): SUBJECT-VAR is bound to a subject naming WHAT.
The composition is made with `vm-mail', addressed to the configured :to, and
carries the marker header.  BODY adds whatever the test is about; the send
happens after it.

The buffer is killed afterwards whether the send worked or not, so a failed
test does not leave Emacs asking about it."
  (declare (indent 1) (debug t))
  (let ((subject (nth 0 spec)) (what (nth 1 spec)))
    `(let* ((,subject (vm-send-live-unique-subject ,what))
            (user-mail-address (vm-send-live-config :from))
            ;; A composition needs a From header of its own.  VM leaves it to
            ;; the MTA by default, but a configuration that takes the envelope
            ;; sender from the header -- `mail-envelope-from' set to `header',
            ;; which is how msmtp is usually driven -- then has nothing to send
            ;; from, and `sendmail-send-it' dies with "Invalid address: nil".
            ;; This is the option a person in that position sets.
            (vm-mail-header-from (vm-send-live-config :from))
            (mail-from-style nil)
            (mail-setup-hook nil)
            (vm-mail-mode-hook nil)
            ;; Never asked: a prompt in batch hangs the run.
            (vm-confirm-mailto-links nil)
            ;; And never in a frame of its own: `vm-multiple-frames-possible-p'
            ;; says no in batch now, but the tests should not turn on what the
            ;; person running them has set either way.
            (vm-frame-per-composition nil)
            (vm-mutable-frame-configuration nil)
            (buffer nil))
       (unwind-protect
           (progn
             (vm-mail (vm-send-live-config :to))
             (setq buffer (current-buffer))
             (goto-char (point-min))
             (vm-mail-mode-remove-header "Subject:")
             (mail-position-on-field "Subject")
             (insert ,subject)
             (goto-char (point-min))
             (insert (format "%s: %d\n" vm-send-live-tag (emacs-pid)))
             (goto-char (point-max))
             ,@body
             (vm-mail-send))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun vm-send-live-await (conn mailbox subject &optional seconds)
  "Wait for a message with SUBJECT to appear in MAILBOX on CONN.
Returns its sequence number.  Mail takes its own time, so this polls; it
signals when the message does not arrive, since a test that goes on to check
nothing is worse than one that says what went wrong.

The default deadline is ten minutes, and generous on purpose.  Measured over
68 sends to one real address: 60 to 110 seconds usually, a tail reaching 150,
and three that had not arrived at 180.  A deadline near the middle of that
makes this fail for the mail system rather than for VM, and a summary that
fails for reasons of its own is one nobody reads.  Waiting costs nothing when
the mail is on time, since this stops as soon as it arrives.

The likely cause is not slowness.  `:verify-server' has to be a server whose
account receives what is sent to `:to' -- the two are ends of the same
mailbox -- and a server configured for the live IMAP tests is usually a test
account that receives nothing at all.  That is what the message says, with
what the mailbox does hold, since an empty one makes the point on its own."
  (let ((deadline (+ (float-time) (or seconds (vm-send-live-config :wait 600))))
        (found nil))
    (while (and (not found) (< (float-time) deadline))
      (vm-imap-live-cmd-ok conn "NOOP")
      (let ((text (vm-imap-live-cmd-ok
                   conn "SEARCH HEADER SUBJECT \"%s\"" subject)))
        (when (string-match "\\* SEARCH \\([0-9 ]+\\)" text)
          (setq found (car (last (split-string (match-string 1 text)))))))
      (unless found (sleep-for 2)))
    (or found (vm-send-live-not-delivered conn mailbox subject))))

(defun vm-send-live-not-delivered (conn mailbox subject)
  "Signal that SUBJECT never turned up in MAILBOX on CONN, and say why not."
  (let ((held (vm-send-live-message-count conn mailbox)))
    (error (concat "%s never arrived in %s on the %s server, which holds %d"
                   " message%s.  Sending worked; the search did not."
                   "  :verify-server has to name a server whose account"
                   " receives what is sent to :to -- one configured for the"
                   " live IMAP tests is usually a test account that receives"
                   " nothing.  Or raise :wait, if the mail is merely slow.")
           subject mailbox (vm-send-live-config :verify-server) held
           (if (= held 1) "" "s"))))

(defun vm-send-live-message-count (conn mailbox)
  "How many messages MAILBOX holds on CONN, from the SELECT response."
  (let ((text (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)))
    (if (string-match "^\\* \\([0-9]+\\) EXISTS" text)
        (string-to-number (match-string 1 text))
      0)))

(defun vm-send-live-delete (conn mailbox n)
  "Delete and expunge message N in MAILBOX on CONN.
These tests put real mail in a real mailbox; each takes its own out again."
  (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)
  (vm-imap-live-cmd-ok conn "STORE %s +FLAGS (\\Deleted)" n)
  (vm-imap-live-cmd-ok conn "EXPUNGE"))

(defmacro vm-send-live-with-delivery (spec &rest body)
  "Open the verifying mailbox and run BODY with a connection to it.
SPEC is (CONN-VAR MAILBOX-VAR).  Any message this run sent is deleted
afterwards, found by its marker header rather than by guessing."
  (declare (indent 1) (debug t))
  (let ((conn (nth 0 spec)) (mailbox (nth 1 spec)))
    `(let* ((server (vm-imap-live-server (vm-send-live-config :verify-server)))
            (,mailbox (vm-send-live-config :verify-mailbox "INBOX"))
            (,conn (vm-imap-live--open server)))
       (unwind-protect
           (progn
             (vm-imap-live-login ,conn server
                                 (car (plist-get server :accounts)))
             (vm-imap-live-cmd-ok ,conn "SELECT \"%s\"" ,mailbox)
             ,@body)
         ;; take out whatever this run put in, whether or not the test passed
         (ignore-errors
           (vm-imap-live-cmd-ok ,conn "SELECT \"%s\"" ,mailbox)
           (let ((text (vm-imap-live-cmd-ok
                        ,conn "SEARCH HEADER %s \"%d\""
                        vm-send-live-tag (emacs-pid))))
             (when (string-match "\\* SEARCH \\([0-9 ]+\\)" text)
               (dolist (n (nreverse (split-string (match-string 1 text))))
                 (vm-imap-live-cmd-ok ,conn "STORE %s +FLAGS (\\Deleted)" n))
               (vm-imap-live-cmd-ok ,conn "EXPUNGE"))))
         (vm-imap-live-close ,conn)))))

(provide 'vm-send-live-init)

;;; vm-send-live-init.el ends here
