;;; vm-imap-live-test.el --- Live IMAP smoke tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Tier 2 smoke tests: prove the harness can reach a real server, build a
;; fixture and read it back, over both plain and TLS.  These do not test
;; vm-imap.el yet -- they establish the ground the ticket tests will stand on.
;;
;; Skipped unless test/vm-imap-config.el exists and `make test-imap' set the
;; enable flag, so `make test' never touches the network.  See
;; dev/docs/design/imap-live-tests.org.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap-live-init)
(require 'vm-imap)

(defconst vm-imap-live-test--message
  "From: alice@example.com\r
To: vmtest@example.com\r
Subject: live imap smoke test\r
Date: Mon, 01 Jan 2024 00:00:00 +0000\r
Message-ID: <smoke-1@example.com>\r
\r
Body of the smoke test message.\r
"
  "A minimal RFC 5322 message to APPEND.  CRLF, as the wire wants.")

;;; ------------------------------------------------------------------
;;; Configuration sanity -- these run without a server
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-inert-without-a-config ()
  "With no config file, nothing here touches the network.
The config is the opt-in: an unconfigured checkout must run the whole suite
without a server, so every live test has to skip rather than fail."
  (skip-unless (not vm-imap-test-servers))
  (should-not (vm-imap-live-available-p)))

(ert-deftest vm-imap-live-test-runs-when-configured ()
  "With a config file, and not suppressed, the live tests are live.
The other half of the contract above: a configured checkout must actually
exercise them, or the config silently buys nothing."
  (skip-unless (and vm-imap-test-servers vm-imap-live-enabled))
  (should (vm-imap-live-available-p))
  (should (vm-imap-live-server "plain")))

(ert-deftest vm-imap-live-test-enabled-flag-suppresses ()
  "Binding `vm-imap-live-enabled' to nil suppresses the live tests.
The escape hatch for a configured checkout that must not use the network."
  (let ((vm-imap-live-enabled nil))
    (should-not (vm-imap-live-available-p))))

(ert-deftest vm-imap-live-test-spec-round-trips-through-vm ()
  "A spec built by the harness parses back to the same parts in VM.
Guards the harness against VM's colon-delimited maildrop format drifting,
and documents that a password containing a colon cannot be expressed."
  (let* ((server '(:name "x" :host "127.0.0.1" :port 143 :tls nil
                         :auth "login" :accounts (("u" . "p"))))
         (spec (vm-imap-live-spec server '("u" . "p") "INBOX"))
         (parsed (vm-imap-parse-spec-to-list spec)))
    (should (equal spec "imap:127.0.0.1:143:INBOX:login:u:p"))
    (should (equal (nth 1 parsed) "127.0.0.1"))
    (should (equal (nth 2 parsed) "143"))
    (should (equal (nth 3 parsed) "INBOX"))
    (should (equal (nth 4 parsed) "login"))
    (should (equal (nth 5 parsed) "u"))
    (should (equal (nth 6 parsed) "p"))))

(ert-deftest vm-imap-live-test-tls-spec-uses-imap-ssl ()
  "A TLS server yields an imap-ssl spec.
VM has no STARTTLS; imap-ssl is implicit TLS, so the scheme is the only
thing that selects it."
  (let ((server '(:name "x" :host "localhost" :port 993 :tls t
                        :auth "login" :accounts (("u" . "p")))))
    (should (string-prefix-p
             "imap-ssl:" (vm-imap-live-spec server '("u" . "p") "INBOX")))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- plain
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-plain-greeting-and-capability ()
  "The server greets us and advertises IMAP4rev1."
  (vm-imap-live-skip-unless-server "plain")
  (let ((conn (vm-imap-live--open (vm-imap-live-server "plain"))))
    (unwind-protect
        (should (member "IMAP4REV1" (vm-imap-live-capabilities conn)))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-plain-login-and-namespace ()
  "We can log in, and the personal namespace is discoverable.
The prefix and separator are read from NAMESPACE rather than assumed, since
mdbox gives \"\" and \"/\" where Maildir++ gives \"INBOX.\" and \".\"."
  (vm-imap-live-skip-unless-server "plain")
  (let* ((server (vm-imap-live-server "plain"))
         (conn (vm-imap-live--open server)))
    (unwind-protect
        (progn
          (vm-imap-live-login conn server (car (plist-get server :accounts)))
          (let ((ns (vm-imap-live-namespace conn)))
            (should (stringp (car ns)))
            (should (stringp (cdr ns)))
            (should (> (length (cdr ns)) 0))))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-plain-bad-password-is-refused ()
  "A wrong password is refused rather than accepted or hanging.
Note dovecot's auth_failure_delay, 2s by default, is charged to this test."
  (vm-imap-live-skip-unless-server "plain")
  (let* ((server (vm-imap-live-server "plain"))
         (user (car (car (plist-get server :accounts))))
         (conn (vm-imap-live--open server)))
    (unwind-protect
        (should (memq (car (vm-imap-live-cmd
                            conn "LOGIN \"%s\" \"%s\"" user "definitely-wrong"))
                      '(no bad)))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-plain-fixture-round-trip ()
  "A throwaway mailbox can be created, APPENDed to, and read back."
  (vm-imap-live-skip-unless-server "plain")
  (vm-imap-live-with-mailbox (conn mailbox "plain"
                              (list vm-imap-live-test--message))
    (should (string-match-p "vmtest" mailbox))
    (should-not (string-match-p "\\`INBOX\\'" mailbox))
    (let ((text (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)))
      (should (string-match-p "1 EXISTS" text)))
    (let ((text (vm-imap-live-cmd-ok
                 conn "FETCH 1 (BODY.PEEK[HEADER.FIELDS (SUBJECT)])")))
      (should (string-match-p "live imap smoke test" text)))))

(ert-deftest vm-imap-live-test-plain-flags-are-readable ()
  "Flags set at APPEND time come back from FETCH.
Groundwork for #38, which is about flags being lost when saving to an IMAP
folder -- the assertion there needs this to be trustworthy first."
  (vm-imap-live-skip-unless-server "plain")
  (vm-imap-live-with-mailbox (conn mailbox "plain")
    (vm-imap-live-append conn mailbox vm-imap-live-test--message "\\Seen")
    (should (member "\\Seen" (vm-imap-live-flags-of conn mailbox 1)))))

(ert-deftest vm-imap-live-test-mailbox-is-cleaned-up ()
  "The throwaway mailbox is gone after the macro returns.
A failed test must not leave state for the next one to trip over."
  (vm-imap-live-skip-unless-server "plain")
  (let (leaked)
    (vm-imap-live-with-mailbox (conn mailbox "plain")
      (setq leaked mailbox))
    (let* ((server (vm-imap-live-server "plain"))
           (conn (vm-imap-live--open server)))
      (unwind-protect
          (progn
            (vm-imap-live-login conn server
                                (car (plist-get server :accounts)))
            (should (memq (car (vm-imap-live-cmd
                                conn "SELECT \"%s\"" leaked))
                          '(no bad))))
        (vm-imap-live-close conn)))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- TLS
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-tls-greeting-and-login ()
  "Implicit TLS on 993 connects, greets and authenticates.
Requires dovecot to have ssl = yes and a cert whose CN matches the
configured :host; with ssl = no the port listens but cannot handshake."
  (vm-imap-live-skip-unless-server "tls")
  (let* ((server (vm-imap-live-server "tls"))
         (conn (vm-imap-live--open server)))
    (unwind-protect
        (progn
          (should (member "IMAP4REV1" (vm-imap-live-capabilities conn)))
          (vm-imap-live-login conn server (car (plist-get server :accounts))))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-tls-fixture-round-trip ()
  "The fixture path works over TLS as well as plain."
  (vm-imap-live-skip-unless-server "tls")
  (vm-imap-live-with-mailbox (conn mailbox "tls"
                              (list vm-imap-live-test--message))
    (let ((text (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)))
      (should (string-match-p "1 EXISTS" text)))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- VM's own session
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-vm-can-open-a-session ()
  "VM's own `vm-imap-make-session' reaches the server and authenticates.
The first test of vm-imap.el itself against a real server, and the point of
the whole exercise: everything above only proves the harness works."
  (vm-imap-live-skip-unless-server "plain")
  (vm-imap-live-with-vm-account ("plain" "vmtest")
    (let* ((spec (car (car vm-imap-account-alist)))
           (process nil))
      (unwind-protect
          (progn
            (setq process (vm-imap-make-session spec nil :purpose "test"))
            (should (processp process))
            (should (memq (process-status process) '(open run))))
        (when (processp process)
          (ignore-errors (vm-imap-end-session process)))))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- saving between IMAP folders keeps attributes (issue #38)
;;; ------------------------------------------------------------------

(defmacro vm-imap-live-with-two-mailboxes (spec &rest body)
  "Create two throwaway mailboxes on the same account, run BODY, remove them.
SPEC is (CONN-VAR SRC-VAR DST-VAR SERVER-NAME).  Same account both sides, so
`vm-save-message-to-imap-folder' takes its server-to-server branch, which is
the one #38 is about."
  (declare (indent 1) (debug t))
  (let ((conn (nth 0 spec)) (src (nth 1 spec))
        (dst (nth 2 spec)) (server-name (nth 3 spec)))
    `(let* ((server (vm-imap-live-server ,server-name))
            (account (car (plist-get server :accounts)))
            (,conn (vm-imap-live--open server))
            (,src nil) (,dst nil))
       (unwind-protect
           (progn
             (vm-imap-live-login ,conn server account)
             (vm-imap-live-namespace ,conn)
             (setq ,src (vm-imap-live-mailbox-name ,conn)
                   ,dst (vm-imap-live-mailbox-name ,conn))
             (vm-imap-live-cmd-ok ,conn "CREATE \"%s\"" ,src)
             (vm-imap-live-cmd-ok ,conn "CREATE \"%s\"" ,dst)
             ,@body)
         (dolist (mailbox (list ,src ,dst))
           (when mailbox
             (ignore-errors (vm-imap-live-cmd ,conn "DELETE \"%s\"" mailbox))))
         (vm-imap-live-close ,conn)))))

(ert-deftest vm-imap-live-test-save-to-imap-keeps-attributes ()
  "Attribute changes made in VM reach a message saved to another IMAP folder.
Issue #38: saving between folders on one server uses the server's own COPY to
avoid shifting the message over the network, and the worry was that local
attribute changes would not be in the copy, since the server copies what the
server has.

They are.  `vm-imap-copy-message' flushes pending flags with
`vm-imap-save-message-flags' before issuing UID COPY.  This is a
characterisation test, not a fix: it passes on unmodified alpha and exists so
the flush cannot be dropped unnoticed.

Covers a system flag and a label, which take different paths -- \\Seen is an
IMAP system flag, a VM label is an IMAP keyword."
  (vm-imap-live-skip-unless-server "plain")
  ;; Required here rather than at the top of the file: loading all of VM has
  ;; side effects at load time, and only this test needs the folder-visiting
  ;; commands.
  (require 'vm)
  (vm-imap-live-with-two-mailboxes (conn src dst "plain")
    (vm-imap-live-append conn src vm-imap-live-test--message)
    (let ((vm-imap-server-timeout vm-imap-live-timeout))
      (unwind-protect
          (progn
            (vm-visit-imap-folder (vm-imap-live-spec server account src))
            (let ((m (car vm-message-list)))
              (should m)
              ;; Change attributes the way a user would, and check VM agrees
              ;; they are pending before the save.
              (vm-set-new-flag m nil)
              (vm-set-unread-flag m nil)
              (vm-add-message-labels "vmtestlabel" 1)
              (should-not (vm-unread-flag m))
              (should (member "vmtestlabel" (vm-labels-of m)))
              (should (vm-attribute-modflag-of m))
              (vm-save-message-to-imap-folder
               (vm-imap-live-spec server account dst) 1)))
        (when (eq major-mode 'vm-mode)
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))))
    ;; Read the destination back with the independent client.
    (let ((flags (vm-imap-live-flags-of conn dst 1)))
      (should (member "\\Seen" flags))
      (should (member "vmtestlabel" flags)))))

(provide 'vm-imap-live-test)

;;; vm-imap-live-test.el ends here
