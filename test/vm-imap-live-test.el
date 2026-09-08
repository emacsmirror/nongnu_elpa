;;; vm-imap-live-test.el --- Live IMAP smoke tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Tier 2 smoke tests: prove the harness can reach a real server, build a
;; fixture and read it back, over both plain and TLS.  These do not test
;; vm-imap.el yet -- they establish the ground the ticket tests will stand on.
;;
;; Skipped unless test/vm-live-config.el exists and `make test-imap' set the
;; enable flag, so `make test' never touches the network.  See
;; dev/docs/design/imap-live-tests.org.

;;; Code:

(require 'cl-lib)
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

(ert-deftest vm-imap-live-test-an-unconfigured-checkout-stays-off-the-network ()
  "With no config, nothing here touches the network.
The config is the opt-in: an unconfigured checkout must run the whole suite
without a server, so every live test has to skip rather than fail.

Checked by taking the config away rather than by skipping where there is
one.  It used to skip on a configured machine, which left every run of a
configured checkout reporting a skip that nothing was wrong with -- and a
skip nobody can explain is one nobody reads."
  (let ((vm-imap-test-servers nil))
    (should-not (vm-imap-live-available-p))))

(ert-deftest vm-imap-live-test-a-configured-checkout-runs-them ()
  "With a config file, and not suppressed, the live tests are live.
The other half of the contract above: a configured checkout must actually
exercise them, or the config silently buys nothing."
  (vm-test-skip-unless
   (and vm-imap-test-servers vm-imap-live-enabled)
   (concat "No live IMAP config, or the live tests are suppressed.  To "
           "exercise them, copy test/vm-live-config.el.template to "
           "test/vm-live-config.el and fill in vm-imap-test-servers; see "
           "dev/docs/design/imap-live-tests.org."))
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
  "VM's own driver reaches the server and authenticates.
The first test of VM's IMAP code itself against a real server, and the point
of the whole exercise: everything above only proves the harness works."
  (vm-imap-live-skip-unless-server "plain")
  (vm-imap-live-with-vm-account ("plain" "vmtest")
    (let* ((spec (car (car vm-imap-account-alist)))
           (opened (vm-imap-net-open spec "live test" 'may-ask))
           (session (car opened)))
      (unwind-protect
          (progn
            (should (processp (vm-net-session-process session)))
            (vm-net-start session
                          (vm-imap-net-one-command-session
                           (nth 2 opened) (nth 3 opened) "NOOP" "NOOP"))
            (let ((deadline (+ (float-time) 30)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            ;; it logged in and the server answered a command
            (should (eq (vm-net-session-state session) 'done)))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))
        (let ((buffer (vm-net-session-buffer session)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- saving between IMAP folders keeps attributes (issue #38)
;;; ------------------------------------------------------------------

(defun vm-imap-live-test--numbered (n)
  "Return a small message carrying number N."
  (format (concat "From: alice@example.com\r\n"
                  "To: vmtest@example.com\r\n"
                  "Subject: message %d\r\n"
                  "Message-ID: <numbered-%d@example.com>\r\n"
                  "\r\n"
                  "Body %d.\r\n")
          n n n))

(defun vm-imap-live-test--exists (conn mailbox)
  "Return how many messages MAILBOX holds, asked over CONN."
  (let ((text (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)))
    (if (string-match "\\([0-9]+\\) EXISTS" text)
        (string-to-number (match-string 1 text))
      (error "No EXISTS in SELECT response"))))

(defun vm-imap-live-test--wait-for-exists (conn mailbox count &optional seconds)
  "Wait until MAILBOX holds COUNT messages, up to SECONDS, and answer with it.
Filing a composition goes through the driver and lands after the send returns,
so a test that reads the mailbox has to let it arrive."
  (let ((deadline (+ (float-time) (or seconds 20)))
        (held (vm-imap-live-test--exists conn mailbox)))
    (while (and (< held count) (< (float-time) deadline))
      (accept-process-output nil 0.2)
      (setq held (vm-imap-live-test--exists conn mailbox)))
    held))

(defun vm-imap-live-test--quit-folder ()
  "Quit the current folder buffer and kill it, as ending a session does."
  (let ((buffer (current-buffer))
        (vm-confirm-quit nil))
    (ignore-errors (vm-quit))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

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
            (,src nil) (,dst nil)
            ;; A visit records where it went; bound so the throwaway mailbox
            ;; this test invents does not turn up in a later test's history.
            (vm-folder-history vm-folder-history)
            (vm-last-visit-folder vm-last-visit-folder)
            (vm-last-visit-imap-folder vm-last-visit-imap-folder)
            (vm-imap-passwords vm-imap-passwords)
            (vm-kept-imap-buffers vm-kept-imap-buffers)
            ;; A session negotiates a size limit and pushes buffer types; an
            ;; error path can leave the stack unbalanced, and neither belongs to
            ;; the next test.
            (vm-imap-max-message-size vm-imap-max-message-size)
            (vm-buffer-types vm-buffer-types)
            ;; `vm-warn' remembers its last warning so as not to repeat it, and a
            ;; refused flag warns on purpose.
            (vm-current-warning vm-current-warning)
            ;; No session trace buffer: VM keeps one per session for debugging, and
            ;; the harness kills every new buffer the moment the test ends, so the
            ;; trace is unreachable anyway.  A session that errors sets this back
            ;; buffer-locally, so a failure still has its trace while it matters.
            (vm-imap-keep-trace-buffer nil))
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
characterisation test, not a fix: it passes on unmodified VM and exists so
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
            (vm-imap-net-wait nil 30)
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
               (vm-imap-live-spec server account dst) 1)
              (vm-imap-net-wait nil 30)))
        (when (eq major-mode 'vm-mode)
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))))
    ;; Read the destination back with the independent client.
    (let ((flags (vm-imap-live-flags-of conn dst 1)))
      (should (member "\\Seen" flags))
      (should (member "vmtestlabel" flags)))))

;;; ------------------------------------------------------------------
;;; Tier 3 -- fault injection (issues #335, #286)
;;; ------------------------------------------------------------------

(require 'vm-imap-relay)

(defmacro vm-imap-live-with-relayed-folder (spec &rest body)
  "Set up a folder reachable through a relay and visit it with VM.
SPEC is (RELAY-VAR MAILBOX-VAR SERVER-NAME).  BODY runs in the VM folder
buffer, with the folder already visited through the relay and its
UIDVALIDITY cached, so a rule set on the relay afterwards affects the *next*
session -- which is the situation #335 describes."
  (declare (indent 1) (debug t))
  (let ((relay (nth 0 spec)) (mailbox (nth 1 spec)) (server-name (nth 2 spec)))
    `(let* ((server (vm-imap-live-server ,server-name))
            (account (car (plist-get server :accounts)))
            (conn (vm-imap-live--open server))
            (,mailbox nil)
            ;; A visit records where it went; bound so the throwaway mailbox
            ;; this test invents does not turn up in a later test's history.
            (vm-folder-history vm-folder-history)
            (vm-last-visit-folder vm-last-visit-folder)
            (vm-last-visit-imap-folder vm-last-visit-imap-folder)
            (vm-imap-passwords vm-imap-passwords)
            (vm-kept-imap-buffers vm-kept-imap-buffers)
            ;; A session negotiates a size limit and pushes buffer types; an
            ;; error path can leave the stack unbalanced, and neither belongs to
            ;; the next test.
            (vm-imap-max-message-size vm-imap-max-message-size)
            (vm-buffer-types vm-buffer-types)
            ;; `vm-warn' remembers its last warning so as not to repeat it, and a
            ;; refused flag warns on purpose.
            (vm-current-warning vm-current-warning)
            ;; No session trace buffer: VM keeps one per session for debugging, and
            ;; the harness kills every new buffer the moment the test ends, so the
            ;; trace is unreachable anyway.  A session that errors sets this back
            ;; buffer-locally, so a failure still has its trace while it matters.
            (vm-imap-keep-trace-buffer nil))
       (unwind-protect
           (progn
             (vm-imap-live-login conn server account)
             (vm-imap-live-namespace conn)
             (setq ,mailbox (vm-imap-live-mailbox-name conn))
             (vm-imap-live-cmd-ok conn "CREATE \"%s\"" ,mailbox)
             (vm-imap-live-append conn ,mailbox vm-imap-live-test--message)
             (vm-imap-relay-with (,relay :host (plist-get server :host)
                                         :port (plist-get server :port))
               (let* ((via (list :name "via" :host "127.0.0.1"
                                 :port (vm-imap-relay-port ,relay)
                                 :tls nil :auth "login"
                                 :accounts (list account)))
                      (vm-imap-server-timeout vm-imap-live-timeout))
                 (unwind-protect
                     (progn
                       (vm-visit-imap-folder
                        (vm-imap-live-spec via account ,mailbox))
                       (vm-imap-net-wait nil 30)
                       ,@body)
                   (when (eq major-mode 'vm-mode)
                     (let ((vm-confirm-quit nil))
                       (ignore-errors (vm-quit-no-change))))))))
         (when ,mailbox
           (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" ,mailbox)))
         (vm-imap-live-close conn)))))

(ert-deftest vm-imap-live-test-fcc-to-a-maildrop-reaches-the-server ()
  "An Fcc naming an IMAP maildrop puts the copy in that mailbox.
Issue #605: it used to write a file named after the maildrop
specification.  This drives `vm-do-fcc-in-composition\' against a real
server and reads the mailbox back with the harness\' own client."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain")
    (let* ((server (vm-imap-live-server "plain"))
           (account (car (plist-get server :accounts)))
           (spec (vm-imap-live-spec server account mailbox))
           (subject (format "vmtest fcc %d" (emacs-pid)))
           (vm-imap-server-timeout vm-imap-live-timeout)
           ;; A session asks for a password when it has none cached.
           (vm-imap-passwords (list (list spec (cdr account)))))
      (with-temp-buffer
        (insert "From: " (car account) "@example.com\n"
                "To: someone@example.com\n"
                "Subject: " subject "\n"
                "Fcc: " spec "\n"
                mail-header-separator "\nfiled by Fcc\n")
        (vm-do-fcc-in-composition))
      ;; the copy is in the mailbox, headers and body intact.  Waited for: the
      ;; filing goes through the driver, so sending returns before it lands.
      (should (equal 1 (vm-imap-live-test--wait-for-exists conn mailbox 1)))
      (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)
      (let ((text (vm-imap-live-cmd-ok conn "FETCH 1 (BODY.PEEK[])")))
        (should (string-match-p (regexp-quote subject) text))
        (should (string-match-p "filed by Fcc" text))))))

(ert-deftest vm-imap-live-test-fcc-is-not-filed-twice ()
  "REGRESSION: an Fcc maildrop is filed once, not once per mechanism.
Issue #605.  `vm-imap-save-composition\' used to handle Fcc entries naming
an IMAP maildrop as well as the IMAP-FCC header.  Now that VM files those
itself, a user who followed the manual and put that function on
`mail-send-hook\' would have had two copies appended: one by VM as it
sends, one by the hook."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain")
    (let* ((server (vm-imap-live-server "plain"))
           (account (car (plist-get server :accounts)))
           (spec (vm-imap-live-spec server account mailbox))
           (vm-imap-server-timeout vm-imap-live-timeout)
           (vm-imap-passwords (list (list spec (cdr account)))))
      (with-temp-buffer
        (insert "From: " (car account) "@example.com\n"
                "To: someone@example.com\n"
                "Subject: filed once\n"
                "Fcc: " spec "\n"
                mail-header-separator "\nbody\n")
        (vm-do-fcc-in-composition)
        ;; what the hook would have done, on top of what VM just did
        (vm-imap-save-composition))
      ;; one copy has to arrive before the count means anything, and a second
      ;; would have arrived by then too: both filings were started together
      (should (equal 1 (vm-imap-live-test--wait-for-exists conn mailbox 1)))
      (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)
      (let ((text (vm-imap-live-cmd-ok conn "STATUS \"%s\" (MESSAGES)" mailbox)))
        (should (string-match "MESSAGES \\([0-9]+\\)" text))
        (should (equal "1" (match-string 1 text)))))))

(ert-deftest vm-imap-live-test-relay-passes-traffic-through ()
  "The relay is transparent when given no rules.
If this fails, nothing else in tier 3 means anything."
  (vm-imap-live-skip-unless-server "plain")
  (let* ((server (vm-imap-live-server "plain"))
         (account (car (plist-get server :accounts))))
    (vm-imap-relay-with (relay :host (plist-get server :host)
                               :port (plist-get server :port))
      (let* ((via (list :name "via" :host "127.0.0.1"
                        :port (vm-imap-relay-port relay)
                        :tls nil :auth "login" :accounts (list account)))
             (conn (vm-imap-live--open via)))
        (unwind-protect
            (progn
              (vm-imap-live-login conn via account)
              (should (member "IMAP4REV1" (vm-imap-live-capabilities conn)))
              (should-not (vm-imap-relay-dropped relay)))
          (vm-imap-live-close conn))))))

(ert-deftest vm-imap-live-test-dropped-select-is-not-a-uidvalidity-change ()
  "REGRESSION: a connection lost at SELECT is not read as a new UIDVALIDITY.
Issue #335: a trace showed the peer dropping after SELECT \"INBOX\" and VM
treating that as the folder having been recreated, which triggers a
destructive resync of the cache.

It does not, as VM stands.  VM raises a protocol error and leaves the cached
UIDVALIDITY alone, and critically never reaches the \"Refresh cache?\"
prompt -- `y-or-n-p' is stubbed here so that reaching it would be visible
rather than hanging batch ert."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((prompts nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      (let ((cached (vm-folder-imap-uid-validity)))
        (should cached)
        (should (= (length vm-message-list) 1))
        ;; Kill the next SELECT mid-flight.
        (setf (vm-imap-relay-drop-on relay) "SELECT")
        (let ((warned nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (push prompt prompts) nil))
                    ((symbol-function 'vm-warn)
                     (lambda (_l _secs &rest args)
                       (push (apply #'format args) warned))))
            ;; the fetch happens after the command returns, so the lost
            ;; connection is a warning when it happens rather than an error
            ;; where the command was typed
            (vm-get-new-mail)
            (vm-imap-net-wait nil 30))
          (should warned))
        (should (vm-imap-relay-dropped relay))
        ;; The destructive branch was never offered...
        (should-not (seq-find (lambda (p) (string-match-p "UID VALIDITY" p))
                              prompts))
        ;; ...and the cache is intact.
        (should (equal (vm-folder-imap-uid-validity) cached))))))

(ert-deftest vm-imap-live-test-dropped-connection-says-so ()
  "REGRESSION: a dropped connection is reported as one, not as a timeout.
`accept-process-output' returns nil both when it waited in vain and when the
process is gone, and `vm-imap-accept-process-output' used to call both a
timeout.  That named the wrong cause, and since `vm-imap-server-timeout'
defaults to nil it blamed a timeout that was not configured at all -- which
is the \"cannot tell a broken connection from anything else\" complaint
behind #335 and #286."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-relayed-folder (relay mailbox "plain")
    (setf (vm-imap-relay-drop-on relay) "SELECT")
    (let ((warned nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                ((symbol-function 'vm-warn)
                 (lambda (_l _secs &rest args) (push (apply #'format args) warned))))
        ;; the fetch is not where the command was typed, so what says the
        ;; connection went is a warning from the session that lost it
        (vm-get-new-mail)
        (vm-imap-net-wait nil 30))
      (let ((message (car warned)))
        (should message)
        (should (string-match-p "connection" message))
        (should-not (string-match-p "timed out" message))))))

;;; ------------------------------------------------------------------
;;; Tier 3 -- a refused STORE must not cost the user their change (#270)
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-refused-store-keeps-the-local-label ()
  "REGRESSION: a STORE the server refuses does not destroy the local label.
Issue #270 asked whether VM could check that flags really were stored.  It
was worse than not checking.  Uploading attributes and downloading them are
two separate passes of a sync: `vm-imap-save-attributes' counts a refused
STORE as an error and carries on, and the download pass then applied the
server's flags to every message, including the one whose upload had just
failed.  So the server's stale view overwrote the user's label, the change
was gone, and the modflag left set for a retry had nothing left to retry.

The download pass now leaves alone any message whose changes have not
reached the server."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-relayed-folder (relay mailbox "plain")
    (let ((m (car vm-message-list)))
      (vm-add-message-labels "vmtest-refused" 1)
      (should (member "vmtest-refused" (vm-labels-of m)))
      (should (vm-attribute-modflag-of m))
      ;; The server now refuses every STORE.
      (setf (vm-imap-relay-reject relay) "STORE")
      (vm-get-new-mail)
      (vm-imap-net-wait nil 30)
      ;; The label survives, and is still pending, so a later sync can retry.
      (should (member "vmtest-refused" (vm-labels-of m)))
      (should (vm-attribute-modflag-of m)))))

(ert-deftest vm-imap-live-test-a-refused-flush-does-not-copy-stale-flags ()
  "Saving to another IMAP folder does not file a copy with the old flags.
Issue #38.  The happy path keeps them -- `vm-imap-copy-message' flushes
pending flags before UID COPY, and
`vm-imap-live-test-save-to-imap-keeps-attributes' pins that.  This is the
path where the flush fails: the call sits in a `condition-case' that
swallows `vm-imap-protocol-error', so a server that refuses the STORE left
the COPY to go ahead and file the server's stale flags, with nothing said to
anyone.  Which is the report, fifteen years on."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((label "vmtestflushed") (checked nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      (let ((dst (vm-imap-live-mailbox-name conn))
            (m (car vm-message-list)))
        (unwind-protect
            (progn
              (vm-imap-live-cmd-ok conn "CREATE \"%s\"" dst)
              (vm-add-message-labels label 1)
              (should (vm-attribute-modflag-of m))
              ;; From here the server refuses to store any flag.
              (setf (vm-imap-relay-reject relay) "STORE")
              (let ((vm-current-warning nil))
                (vm-save-message-to-imap-folder
                 (vm-imap-live-spec via account dst) 1)
                (vm-imap-net-wait nil 30)
                ;; The copy really does carry the server's flags ...
                (should-not (member label (vm-imap-live-flags-of conn dst 1)))
                ;; ... the change is still ours and still pending ...
                (should (member label (vm-labels-of m)))
                (should (vm-attribute-modflag-of m))
                ;; ... and the user was told, rather than the save looking
                ;; like it had done what was asked.
                (should (string-match-p "flags the server holds"
                                        (or vm-current-warning ""))))
              (setq checked t))
          (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" dst)))))
    (should checked)))

(ert-deftest vm-imap-live-test-one-refused-flag-does-not-block-the-others ()
  "REGRESSION: a keyword the server will not take does not hold back the rest.
Issue #391.  VM sent every pending flag in one STORE, so a server that refuses
one of them refuses the command, and nothing was stored: the reporter marked mail
deleted and labelled it, Exchange would not take the label, and the deletion
never reached the server either.  The refusal is now met by offering the flags
one at a time, so what the server will take is stored and what it will not is
remembered and not offered again.

The relay refuses any command mentioning the label, which is how an Exchange
server behaves towards a keyword it does not know."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((label "vmtestrefusedkeyword") (checked nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      (let ((m (car vm-message-list)))
        (vm-add-message-labels label 1)
        (vm-set-deleted-flag m t)
        (should (vm-attribute-modflag-of m))
        ;; From here the server takes any flag but that one.
        (setf (vm-imap-relay-reject relay) label)
        (should (vm-imap-net-save-attributes))
        (vm-imap-net-wait nil 30)
        ;; The label is still ours ...
        (should (member label (vm-labels-of m)))
        ;; ... and the deletion reached the server, read back on the direct
        ;; connection, which the relay is not refusing anything on.
        (let ((flags (vm-imap-live-flags-of conn mailbox 1)))
          (should (seq-find (lambda (f) (equal (downcase f) "\\deleted")) flags))
          (should-not (seq-find (lambda (f) (equal (downcase f) (downcase label)))
                                flags)))
        (setq checked t)))
    (should checked)))

(ert-deftest vm-imap-live-test-a-refused-flag-is-offered-only-once ()
  "REGRESSION: a keyword the server refuses is not offered for the next message.
Issue #389.  John Stoffel's trace shows ten identical refusals, one message after
another --

    VM STORE 460 +FLAGS.SILENT (filed)
    VM BAD Command Argument Error. 11
    VM STORE 477 +FLAGS.SILENT (filed)
    VM BAD Command Argument Error. 11
    ...
    Process IMAP connection broken by remote peer

-- and the server hung up on him for it, which is how a refused keyword turned
into \"cannot get new mail\".  VM now remembers a refused flag for the session, so
the server is asked once however many messages carry it.

Three messages, all labelled, and the relay refusing anything that mentions the
label: the transcript must hold one STORE of it, not three."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((label "vmtestonceonly") (checked nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      ;; two more messages, so a per-message repeat would show
      (vm-imap-live-append conn mailbox vm-imap-live-test--message)
      (vm-imap-live-append conn mailbox vm-imap-live-test--message)
      (vm-get-new-mail)
      (vm-imap-net-wait nil 30)
      (should (= 3 (length vm-message-list)))
      (dolist (n '(1 2 3))
        (vm-goto-message n)
        (vm-add-message-labels label 1))
      (setf (vm-imap-relay-reject relay) label)
      (should (vm-imap-net-save-attributes))
      (vm-imap-net-wait nil 30)
      (let* ((sent (vm-imap-relay-transcript relay 'client))
             (count 0)
             (start 0))
        (while (string-match (concat "STORE[^\n]*" (regexp-quote label)) sent start)
          (setq count (1+ count) start (match-end 0)))
        (should (= 1 count))
        ;; And nothing at all about \Recent, which a client may never set or
        ;; clear: VM used to send "-FLAGS.SILENT (\recent)" for every message it
        ;; synced, which is one more BAD from a server like the one in this
        ;; report and ignored by the rest.
        (should-not (string-match-p "STORE[^\n]*[Rr]ecent" sent)))
      (setq checked t))
    (should checked)))

(defconst vm-imap-live-test--message-with-attachment
  (concat "From: alice@example.com\r\nTo: vmtest@example.com\r\n"
          "Subject: external with attachment\r\n"
          "Date: Mon, 01 Jan 2024 00:00:00 +0000\r\n"
          "Message-ID: <ext-attachment@example.com>\r\n"
          "MIME-Version: 1.0\r\n"
          "Content-Type: multipart/mixed; boundary=\"SEP\"\r\n\r\n"
          "--SEP\r\nContent-Type: text/plain\r\n\r\nSee attachment.\r\n\r\n"
          "--SEP\r\nContent-Type: application/pdf; name=\"t.pdf\"\r\n"
          "Content-Disposition: attachment; filename=\"t.pdf\"\r\n"
          "Content-Transfer-Encoding: base64\r\n\r\n"
          (let ((s "")) (dotimes (_ 40)
                          (setq s (concat s "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU2Nzg5\r\n")))
               s)
          "\r\n--SEP--\r\n")
  "A message with an attachment, over any sensible `vm-imap-max-message-size'.")

(ert-deftest vm-imap-live-test-external-attachment-is-not-silently-empty ()
  "REGRESSION: a part of a message whose body is not here refuses, not empties.
Issue #386.  With `vm-external-fetch-message-for-presentation' nil, a message
over `vm-imap-max-message-size' is presented from its headers alone, and the
layout parsed from them has parts with no text.  Acting on one of those parts
wrote a file of zero length and said nothing: the reporter got an empty PDF and
Adobe Reader complaining about it.

Measured before the fix, on this server: the presentation buffer holds 249
characters of headers, both parts report body 250..250, and the written file is 0
bytes.  Now the button refuses and says what to do; after loading the message it
writes the attachment."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let* ((server (vm-imap-live-server "plain"))
         (account (car (plist-get server :accounts)))
         (conn (vm-imap-live--open server))
         (mailbox nil)
         (out (expand-file-name "vm-386-test.pdf" temporary-file-directory))
         (vm-imap-server-timeout vm-imap-live-timeout)
         (vm-enable-external-messages '(imap))
         (vm-imap-max-message-size 500)
         (vm-external-fetch-message-for-presentation nil)
         (vm-imap-passwords vm-imap-passwords)
         (vm-kept-imap-buffers vm-kept-imap-buffers)
         (vm-imap-keep-trace-buffer nil)
         (vm-current-warning vm-current-warning)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-last-visit-imap-folder vm-last-visit-imap-folder)
         (vm-buffer-types vm-buffer-types)
         ;; Presenting a message with an attachment compiles the summary and
         ;; button formats into their memos, which are global.
         (vm-summary-untokenized-compiled-format-alist
          vm-summary-untokenized-compiled-format-alist)
         (vm-mime-compiled-format-alist vm-mime-compiled-format-alist))
    (unwind-protect
        (progn
          (vm-imap-live-login conn server account)
          (vm-imap-live-namespace conn)
          (setq mailbox (vm-imap-live-mailbox-name conn))
          (vm-imap-live-cmd-ok conn "CREATE \"%s\"" mailbox)
          (vm-imap-live-append conn mailbox
                               vm-imap-live-test--message-with-attachment)
          (vm-visit-imap-folder (vm-imap-live-spec server account mailbox))
          (vm-imap-net-wait nil 30)
          (should (= 1 (length vm-message-list)))
          (let ((folder (current-buffer)))
            (should (vm-body-to-be-retrieved-of (car vm-message-list)))
            (vm-show-current-message)
            (vm-imap-net-wait nil 30)
            (set-buffer folder)
            ;; The button refuses rather than writing nothing.
            (with-current-buffer vm-presentation-buffer
              (let ((err (should-error (vm-mime-run-display-function-at-point
                                        'vm-mime-send-body-to-file)
                                       :type 'error)))
                (should (string-match-p "not loaded" (error-message-string err)))))
            ;; Load it, as the message says, and the attachment comes out whole.
            (set-buffer folder)
            (vm-load-message)
            (vm-imap-net-wait nil 30)
            (set-buffer folder)
            (let* ((m (car vm-message-list))
                   (parts (vm-mm-layout-parts (vm-mm-layout m)))
                   (pdf (car (last parts))))
              (should (= 2 (length parts)))
              (should (equal "application/pdf" (car (vm-mm-layout-type pdf))))
              (when (file-exists-p out) (delete-file out))
              (vm-mime-send-body-to-file pdf nil out t)
              (should (file-exists-p out))
              (should (> (nth 7 (file-attributes out)) 0)))))
      (when (file-exists-p out) (delete-file out))
      (ignore-errors
        (when (memq major-mode '(vm-mode vm-virtual-mode))
          (let ((vm-confirm-quit nil)) (vm-quit-no-change))))
      (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" mailbox))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-loading-a-body-does-not-ask-the-size-again ()
  "Loading message bodies costs one command each, not two.
Issue #185.  `vm-fetch-imap-message' asked the server for RFC822.SIZE before
every body it fetched, for a progress meter, when the FETCH that brought the
message in had already recorded the size: loading three messages took eight
commands where four will do.  It now asks only when the size is not cached.

The whole of #185 is not done here -- the bodies are still fetched one message
per command, where a UID set would do -- but the doubling is."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((vm-enable-external-messages '(imap))
        (vm-imap-max-message-size 100)
        (vm-external-fetch-message-for-presentation nil)
        (checked nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      (dotimes (_ 2) (vm-imap-live-append conn mailbox vm-imap-live-test--message))
      (vm-get-new-mail)
      (vm-imap-net-wait nil 30)
      (should (= 3 (length vm-message-list)))
      (dolist (m vm-message-list)
        (should (vm-body-to-be-retrieved-of m))
        ;; the premise: the size is there to be used
        (should (vm-fetch-imap-message-size m)))
      (setf (vm-imap-relay-log relay) nil)
      (vm-goto-message 2)
      (vm-load-message 2)
      (vm-imap-net-wait nil 30)
      (let ((sent (vm-imap-relay-transcript relay 'client)))
        (should (string-match-p "FETCH[^\n]*BODY" sent))
        (should-not (string-match-p "FETCH[^\n]*RFC822.SIZE" sent)))
      (setq checked t))
    (should checked)))

(ert-deftest vm-imap-live-test-presentation-honours-the-fetch-option ()
  "REGRESSION: presenting does not fetch a body the option said to leave alone.
Issue #585.  `vm-make-presentation-copy' fetched an external body whatever
`vm-external-fetch-message-for-presentation' said, since the option is consulted
in `vm-preview-current-message' and not there.  It fetched into the presentation
buffer rather than the folder, so the flag stayed set and the next presentation
fetched the same body again, and the layout it parsed outlived the filling of the
buffer it described -- parts whose markers had all collapsed to the end, which is
the empty attachment of #386.

Off: no fetch, and a layout with no parts, which is the truth about a message
whose body is elsewhere.  On: the body is fetched into the folder, the parts have
their text, and the attachment writes whole."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((out (expand-file-name "vm-585-test.pdf" temporary-file-directory)))
    (dolist (fetch-for-presentation '(nil t))
      (let ((vm-enable-external-messages '(imap))
            (vm-imap-max-message-size 500)
            (vm-external-fetch-message-for-presentation fetch-for-presentation)
            (fetches 0))
        (vm-imap-live-with-relayed-folder (relay mailbox "plain")
          (vm-imap-live-append conn mailbox
                               vm-imap-live-test--message-with-attachment)
          (vm-get-new-mail)
          (vm-imap-net-wait nil 30)
          (let ((m (car (last vm-message-list)))
                (folder (current-buffer)))
            (should (vm-body-to-be-retrieved-of m))
            (setf (vm-imap-relay-log relay) nil)
            ;; `vm-number-of' is a string, as the summary needs it
            (vm-goto-message (string-to-number (vm-number-of m)))
            (set-buffer folder)
            ;; presenting fetches the body through the driver, which returns
            ;; before the server has answered
            (should (vm-imap-net-wait nil 30))
            (let ((sent (vm-imap-relay-transcript relay 'client))
                  (start 0))
              (while (string-match "FETCH[^\n]*BODY.PEEK\\[\\]" sent start)
                (setq fetches (1+ fetches) start (match-end 0))))
            (if (null fetch-for-presentation)
                (progn
                  ;; nothing fetched, and nothing pretended
                  (should (= 0 fetches))
                  (should (vm-body-to-be-retrieved-of m))
                  (should (null (vm-mm-layout-parts (vm-mm-layout m)))))
              ;; fetched once, into the folder, and usable
              (should (= 1 fetches))
              (should-not (vm-body-to-be-retrieved-of m))
              (let* ((parts (vm-mm-layout-parts (vm-mm-layout m)))
                     (pdf (car (last parts))))
                (should (= 2 (length parts)))
                (when (file-exists-p out) (delete-file out))
                (vm-mime-send-body-to-file pdf nil out t)
                (should (> (nth 7 (file-attributes out)) 0))))))))
    (when (file-exists-p out) (delete-file out))))

(ert-deftest vm-imap-live-test-accepted-store-reaches-the-server ()
  "The other half: an accepted STORE does sync and stops being pending.
Guards the fix above from being a blanket refusal to ever apply server
flags, which would break normal synchronisation instead."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((label "vmtest-accepted") (checked nil))
    (vm-imap-live-with-relayed-folder (relay mailbox "plain")
      (let ((m (car vm-message-list)))
        (vm-add-message-labels label 1)
        (should (vm-attribute-modflag-of m))
        (vm-get-new-mail)
        (vm-imap-net-wait nil 30)
        (should (member label (vm-labels-of m)))
        ;; Uploaded, so no longer pending.
        (should-not (vm-attribute-modflag-of m))
        (setq checked t)))
    (should checked)))

;;; ------------------------------------------------------------------
;;; Tier 2 -- duplicate deletion and stale copies (issue #286)
;;; ------------------------------------------------------------------
;;
;; These drive `vm-delete-duplicate-messages' in a real IMAP folder rather
;; than a constructed one.  It needs the full folder context that only
;; visiting gives -- vm-delete-test.el says as much, and tests the hash
;; logic in isolation instead, which cannot catch a bug in the command.

(defconst vm-imap-live-test--duplicate
  "From: a@example.com\r
To: vmtest@example.com\r
Subject: duplicate\r
Date: Mon, 01 Jan 2024 00:00:00 +0000\r
Message-ID: <vmtest-dup@example.com>\r
\r
A copy.\r
"
  "A message appended twice, so the folder holds two copies of one id.")

(ert-deftest vm-imap-live-test-duplicates-spare-the-good-copy ()
  "REGRESSION: a stale copy does not get the good copy flagged for deletion.
Issue #286.  An interrupted `vm-get-new-mail' leaves copies whose UID
validity does not match the folder's, and a later fetch brings down good
copies of the same messages.  `vm-delete-duplicate-messages', run from
`vm-arrived-messages-hook', keeps whichever copy it meets first and flags
the rest.  The stale copies come first, so the good ones were flagged --
and then answering yes to \"Found N messages with invalid UIDs.  Expunge
them?\" took the stale ones as well, losing every copy.

VM already skipped messages carrying the `stale' label, but that label is
only applied when the user *declines* that prompt, which is after this has
run.  Staleness is now judged by UID validity, so the good copy survives and
the stale one is left for the invalid-UID path to deal with."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-relayed-folder (relay mailbox "plain")
    (ignore relay)
    ;; The macro already put one unrelated message in the mailbox; add two
    ;; copies sharing a message id, then resync so VM sees all three.
    ;; `conn' comes from the macro and is already logged in.
    (vm-imap-live-append conn mailbox vm-imap-live-test--duplicate)
    (vm-imap-live-append conn mailbox vm-imap-live-test--duplicate)
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (let* ((messages vm-message-list)
           (stale (nth 1 messages))
           (good (nth 2 messages)))
      (should (= (length messages) 3))
      ;; Make the first copy look like the wreckage of an interrupted fetch.
      (vm-set-imap-uid-validity-of stale "definitely-not-current")
      (should-not (equal (vm-imap-uid-validity-of stale)
                         (vm-folder-imap-uid-validity)))
      (vm-delete-duplicate-messages)
      ;; The good copy must survive; the stale one is not this command's
      ;; business.
      (should-not (vm-deleted-flag good))
      (should-not (vm-deleted-flag stale)))))

(ert-deftest vm-imap-live-test-duplicates-still-deleted-when-current ()
  "Real duplicates are still flagged when both copies are current.
Guards the fix above from becoming a blanket refusal to dedupe IMAP folders,
which would leave duplicates behind instead."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-relayed-folder (relay mailbox "plain")
    (ignore relay)
    (vm-imap-live-append conn mailbox vm-imap-live-test--duplicate)
    (vm-imap-live-append conn mailbox vm-imap-live-test--duplicate)
    (vm-get-new-mail)
    (vm-imap-net-wait nil 30)
    (let* ((messages vm-message-list)
           (first (nth 1 messages))
           (second (nth 2 messages)))
      (should (= (length messages) 3))
      ;; Both current, as after an ordinary fetch.
      (should (equal (vm-imap-uid-validity-of first)
                     (vm-folder-imap-uid-validity)))
      (vm-delete-duplicate-messages)
      (should-not (vm-deleted-flag first))
      (should (vm-deleted-flag second)))))

;;; ------------------------------------------------------------------
;;; Tier 2 -- headers-only fetch must not corrupt the message (issue #500)
;;; ------------------------------------------------------------------

(defun vm-imap-live-test--bulky (n)
  "Return message N whose body is well over `vm-imap-live-test--size-limit'.
Every body line carries VMTESTBODY, so body text appearing among the headers
is unmistakable.  The last header is a sentinel for the same reason."
  (concat "From: alice@example.com\r\n"
          "To: vmtest@example.com\r\n"
          (format "Subject: headers only fetch %d\r\n" n)
          "Date: Mon, 01 Jan 2024 00:00:00 +0000\r\n"
          (format "Message-ID: <vmtest-bulky-%d@example.com>\r\n" n)
          "X-VMTest-Last-Header: sentinel\r\n"
          "\r\n"
          (mapconcat (lambda (i) (format "VMTESTBODY line %03d\r\n" i))
                     (number-sequence 1 60) "")))

(defconst vm-imap-live-test--size-limit 200
  "`vm-imap-max-message-size' for the headers-only tests.
Smaller than `vm-imap-live-test--bulky', so VM fetches its headers only.")

(defun vm-imap-live-test--headers-of (m)
  "Return M's header section, from its start up to where its body begins."
  (with-current-buffer (vm-buffer-of m)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (vm-start-of m) (vm-text-of m)))))

(defun vm-imap-live-test--body-of (m)
  "Return M's body, the region VM considers to be after its headers."
  (with-current-buffer (vm-buffer-of m)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (vm-text-of m) (vm-text-end-of m)))))

(ert-deftest vm-imap-live-test-headers-only-fetch-keeps-body-out-of-headers ()
  "REGRESSION: loading an external body puts it after the headers.
Issue #500, via the deleted README.headers-only: with headers-only IMAP
downloading, the body could be inserted in the *midst* of the headers rather
than after them.  Reported as infrequent and never diagnosed; the warning was
retired in 2f33c4b without a code change, so nothing had been shown either
way since 2010.

The path is `vm-retrieve-real-message-body' (vm-folder.el), not
`vm-fetch-message'.  It empties the body region, has `vm-fetch-imap-message'
insert the whole message there -- headers again and all -- and then removes
the duplicate headers with

    (delete-region (vm-text-of mm)
                   (or (re-search-forward \"\\\\n\\\\n\" (point-max) t)
                       (point-max)))

That search runs from point, so it is only correct while point is still at
`vm-text-of'.  Drift past the blank line makes the delete take the body
instead: to the body's own first blank line if it has one, and otherwise to
`point-max', which discards the body entirely.

Drift alone does not reproduce the reported symptom -- it loses body, it does
not move body up among the headers.  For that the *insertion* would have to
land inside the header region, which needs a caller whose point is elsewhere
in a buffer holding the message; `vm-fetch-imap-message' inserts into whatever
buffer is current when it is called.  This test pins the folder-buffer path,
where point is controlled.

VM knows the invariant and guards it with four `vm-assert's around the
insertion.  All four are inert in normal use: `vm-assertion-checking-off'
defaults to t, so the macro expands to a disjunction that short-circuits.
This test binds it to nil, so a fetch that lands point in the wrong place
fails here rather than silently damaging the folder.

Both levers were checked by advising `vm-fetch-imap-message' to leave point
past the text it inserted: with assertions on the third assert fires, and with
them off the body is discarded rather than kept, which the body assertions
below catch.  So neither half of this test passes vacuously.

Two messages are appended because visiting the folder previews the first one,
and previewing an external message loads its body; only the second is still
external by the time the test can look at it."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-two-mailboxes (conn src _dst "plain")
    (ignore _dst)
    (vm-imap-live-append conn src (vm-imap-live-test--bulky 1))
    (vm-imap-live-append conn src (vm-imap-live-test--bulky 2))
    (let ((vm-imap-server-timeout vm-imap-live-timeout)
          (vm-enable-external-messages '(imap))
          (vm-imap-max-message-size vm-imap-live-test--size-limit)
          ;; Never prompt about the large message; treat it as external.
          (vm-imap-ok-to-ask nil))
      (unwind-protect
          (progn
            (vm-visit-imap-folder (vm-imap-live-spec server account src))
            (vm-imap-net-wait nil 30)
            (should (= (length vm-message-list) 2))
            (let ((m (nth 1 vm-message-list)))
              ;; It arrived headers-only, or the rest proves nothing about
              ;; the fetch path.
              (should (vm-body-to-be-retrieved-of m))
              (should (equal "" (vm-imap-live-test--body-of m)))
              (let ((headers (vm-imap-live-test--headers-of m)))
                (should (string-match-p "X-VMTest-Last-Header: sentinel"
                                        headers))
                (should-not (string-match-p "VMTESTBODY" headers)))
              ;; Load the body the way displaying the message does, with VM's
              ;; own point assertions live.  Selecting the message is enough
              ;; to load it -- previewing an external message fetches it --
              ;; and `vm-load-message' then confirms that path too.
              ;;
              ;; `inhibit-debugger' because `vm-assert' raises its error with
              ;; `debug-on-error' bound to t, which in batch prints a
              ;; backtrace and kills Emacs, taking the rest of the suite with
              ;; it.  Inhibited, the error reaches ert as an ordinary failure
              ;; of this one test.
              (let ((vm-assertion-checking-off nil)
                    (inhibit-debugger t))
                (vm-goto-message 2)
                (vm-load-message 1)
                ;; the load goes through the driver and returns before the
                ;; body does
                (should (vm-imap-net-wait nil 30)))
              (should-not (vm-body-to-be-retrieved-of m))
              (let ((headers (vm-imap-live-test--headers-of m))
                    (body (vm-imap-live-test--body-of m)))
                ;; The body arrived whole, and starts at the start...
                (should (string-prefix-p "VMTESTBODY line 001" body))
                (should (string-match-p "VMTESTBODY line 060" body))
                ;; ...the headers survived...
                (should (string-match-p "X-VMTest-Last-Header: sentinel"
                                        headers))
                (should (string-match-p "Subject: headers only fetch 2"
                                        headers))
                ;; ...only one copy of them is left...
                (should (= 1 (cl-count-if
                              (lambda (l) (string-prefix-p "From: alice" l))
                              (split-string headers "\n"))))
                ;; ...and no body line landed among them, which is #500.
                (should-not (string-match-p "VMTESTBODY" headers)))))
        (when (eq major-mode 'vm-mode)
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))))))


;;; ------------------------------------------------------------------
;;; Deletions owed to the server outlive the session (issue #556)
;;; ------------------------------------------------------------------

(ert-deftest vm-imap-live-test-offline-expunge-reaches-the-server-later ()
  "REGRESSION: an expunge made offline is sent the next time we are online.
Issue #556.  `vm-imap-messages-to-expunge' is buffer-local, and used to be
written nowhere, so a session that could not reach the server dropped the
deletions: the messages stayed on the server for good and the user was told
nothing.  A later session did not fetch them again -- X-VM-IMAP-Retrieved
remembers those UIDs -- so nothing looked wrong locally while the mail the user
deleted was still in their mailbox.

Three messages; delete two, go offline, expunge, save, quit.  The server should
still have three, and the folder should have recorded what it owes.  Then visit
again, online, and save: the two deletions should go out."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain"
                              (list (vm-imap-live-test--numbered 1)
                                    (vm-imap-live-test--numbered 2)
                                    (vm-imap-live-test--numbered 3)))
    (let* ((account (car (plist-get server :accounts)))
           (spec (vm-imap-live-spec server account mailbox))
           (cache (vm-imap-make-filename-for-spec spec))
           (vm-imap-server-timeout vm-imap-live-timeout)
           (vm-imap-ok-to-ask nil)
           (vm-confirm-quit nil))
      (unwind-protect
          (progn
            ;; ---- session one, going offline before the expunge
            (vm-visit-imap-folder spec)
            (vm-imap-net-wait nil 30)
            (should (= 3 (length vm-message-list)))
            (vm-delete-message 1)
            (vm-next-message 1)
            (vm-delete-message 1)
            (setq vm-imap-connection-mode 'offline)
            (ignore-errors (vm-imap-end-session (vm-folder-imap-process)))
            (vm-expunge-folder)
            (should (= 2 (length vm-imap-messages-to-expunge)))
            (vm-save-folder)
            (vm-imap-live-test--quit-folder)
            (setq vm-imap-connection-mode 'online)
            ;; The folder knows what it owes ...
            (with-temp-buffer
              (insert-file-contents cache)
              (should (string-match-p "X-VM-IMAP-To-Expunge" (buffer-string))))
            ;; ... and the server still has everything.
            (should (= 3 (vm-imap-live-test--exists conn mailbox)))
            ;; ---- session two, online: the visit's own session sends what
            ;; the folder owes, so by the time it has the mail the deletions
            ;; have gone and nothing is pending
            (vm-visit-imap-folder spec)
            (vm-imap-net-wait nil 30)
            (should (= 0 (length vm-imap-messages-to-expunge)))
            (should (= 1 (vm-imap-live-test--exists conn mailbox)))
            (vm-save-folder)
            (vm-imap-live-test--quit-folder))
        (when (file-exists-p cache) (delete-file cache))))))


;;; Fetching several bodies in one command (#185)

(defun vm-imap-live-test--numbered-message (n)
  "A message whose body says which one it is, and is big enough to be external."
  (format (concat "From: a@example.com\nTo: b@example.com\n"
                  "Subject: number %d\n\nbody-of-%d %s\n")
          n n (make-string 400 ?x)))

(ert-deftest vm-imap-live-test-several-bodies-are-fetched-together ()
  "Loading four message bodies is one IMAP command, not four.
`vm-load-message\=' asked for each body with its own `UID FETCH\=', so reading a
folder of external messages cost a round trip apiece.  Issue #185."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let ((commands nil))
    (vm-imap-live-with-vm-account ("plain" "bunched")
      (vm-imap-live-with-mailbox (conn mailbox "plain")
        (dolist (n '(1 2 3 4))
          (vm-imap-live-append conn mailbox
                               (vm-imap-live-test--numbered-message n)))
        (let ((vm-imap-server-timeout vm-imap-live-timeout)
              (vm-enable-external-messages '(imap))
              (vm-imap-max-message-size 100)
              (account (car (plist-get server :accounts))))
          (unwind-protect
              (progn
                (vm-visit-imap-folder (vm-imap-live-spec server account mailbox))
                (vm-imap-net-wait nil 30)
                (should (= 4 (length vm-message-list)))
                ;; the first was fetched to be shown; the rest are pending
                (should (= 3 (length (seq-filter #'vm-body-to-be-retrieved-of
                                                 vm-message-list))))
                ;; `cl-letf', not `advice-add': `advice-remove' compares
                ;; functions with `equal', so removing a *different* lambda
                ;; from the one added removes nothing and the advice outlives
                ;; the test.  Nothing restores advice between tests either --
                ;; `vm-test-isolate-global-state' restores variables and kills
                ;; buffers, and that is all.
                (let ((real (symbol-function 'vm-imap-net-send)))
                  (cl-letf (((symbol-function 'vm-imap-net-send)
                             (lambda (command &rest args)
                               (push command commands)
                               (apply real command args))))
                    (vm-load-message 4)
                    (vm-imap-net-wait nil 30)
))
                ;; one FETCH for the three of them
                (let ((fetches (seq-filter (lambda (c)
                                             (string-match-p "FETCH" c))
                                           commands)))
                  (should (= 1 (length fetches)))
                  (should (string-match-p "UID FETCH 2,3,4" (car fetches))))
                (should (null (seq-filter #'vm-body-to-be-retrieved-of
                                          vm-message-list)))
                ;; and each body went to its own message.  Widened: the
                ;; folder is narrowed to whatever message is being shown, and
                ;; the others lie outside it
                (save-restriction
                 (widen)
                 (dolist (m vm-message-list)
                  (let ((body (buffer-substring (vm-text-of m) (vm-text-end-of m)))
                        (number (progn (string-match "number \\([0-9]+\\)"
                                                     (vm-su-subject m))
                                       (match-string 1 (vm-su-subject m)))))
                    (should (string-match-p (format "body-of-%s " number) body))))))
            ;; Leave no folder, summary or presentation buffer behind.
            (let ((vm-confirm-quit nil))
              (ignore-errors (vm-quit-no-change)))))))))

(ert-deftest vm-imap-live-test-bunched-fetch-keeps-bodies-out-of-headers ()
  "REGRESSION: bodies fetched in one command each land after their own headers.
Issue #500 again, on the path issue #185 added.  The single-message test above
covers `vm-retrieve-real-message-body\=', where `vm-fetch-imap-message\=' does the
insertion inside a `save-excursion\=' and point is controlled throughout.

`vm-fetch-imap-messages\=' has neither of those protections.  It inserts into the
folder buffer from the process buffer as each response arrives, re-narrowing to
a different message every time, and leaves point after the text it inserted --
which is why `vm-settle-message-body\=' has to put point back before its
`\\n\\n\=' search.  Several messages in one buffer, markers on all of them, and
the narrowing moving between them is what README.headers-only described:
body appearing in the midst of headers.

So this checks the whole folder, not the region a message's own markers claim.
Every message must keep its sentinel header, no header block may contain body
text, and there must be exactly one copy of each message's headers -- a
duplicate would mean the second copy of the headers that the fetch inserts was
not removed.

Three messages because visiting previews the first, which loads its body; two
are left external, which is what makes the fetch a bunched one."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-vm-account ("plain" "bunched-500")
    (vm-imap-live-with-mailbox (conn mailbox "plain")
      (dolist (n '(1 2 3))
        (vm-imap-live-append conn mailbox (vm-imap-live-test--bulky n)))
      (let ((vm-imap-server-timeout vm-imap-live-timeout)
            (vm-enable-external-messages '(imap))
            (vm-imap-max-message-size vm-imap-live-test--size-limit)
            (vm-imap-ok-to-ask nil)
            (account (car (plist-get server :accounts)))
            (bunched 0))
        (unwind-protect
            (progn
              (vm-visit-imap-folder (vm-imap-live-spec server account mailbox))
              (vm-imap-net-wait nil 30)
              (should (= 3 (length vm-message-list)))
              ;; Two still external, so the load below is a bunched one and
              ;; this test is not passing on the single-message path.
              (should (= 2 (length (seq-filter #'vm-body-to-be-retrieved-of
                                               vm-message-list))))
              (let ((vm-assertion-checking-off nil)
                    (inhibit-debugger t)
                    (real (symbol-function 'vm-imap-net-load-message-bodies)))
                (cl-letf (((symbol-function 'vm-imap-net-load-message-bodies)
                           (lambda (mlist)
                             (setq bunched (length mlist))
                             (funcall real mlist))))
                  ;; From message 1, which is already loaded.  Selecting a
                  ;; message previews it, and previewing an external message
                  ;; loads its body one at a time -- so moving to message 2
                  ;; first would leave only one to fetch and no bunch.
                  (vm-load-message 3)
                  (vm-imap-net-wait nil 30)))
              (should (= 2 bunched))
              (should (null (seq-filter #'vm-body-to-be-retrieved-of
                                        vm-message-list)))
              (dolist (m vm-message-list)
                (let ((headers (vm-imap-live-test--headers-of m))
                      (body (vm-imap-live-test--body-of m)))
                  (should (string-match-p "X-VMTest-Last-Header: sentinel"
                                          headers))
                  ;; the body is whole and starts at the start ...
                  (should (string-prefix-p "VMTESTBODY line 001" body))
                  (should (string-match-p "VMTESTBODY line 060" body))
                  ;; ... and none of it is up among the headers, which is #500
                  (should-not (string-match-p "VMTESTBODY" headers))))
              ;; Each message's headers appear once in the folder as a whole:
              ;; three messages, three sentinels, three subjects.
              (save-restriction
                (widen)
                (let ((folder (buffer-substring-no-properties
                               (point-min) (point-max))))
                  (should (= 3 (cl-count-if
                                (lambda (l)
                                  (string-prefix-p "X-VMTest-Last-Header:" l))
                                (split-string folder "\n"))))
                  (dolist (n '(1 2 3))
                    (should (= 1 (cl-count-if
                                  (lambda (l)
                                    (string= l (format
                                                "Subject: headers only fetch %d"
                                                n)))
                                  (split-string folder "\n"))))))))
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))))))

(ert-deftest vm-imap-live-test-the-skip-helper-works-outside-a-test-body ()
  "`vm-imap-live-skip-unless-server' skips when called from a function.
It used to expand to `skip-unless', which `ert-deftest' binds with
`cl-macrolet' -- so it exists inside a test body and nowhere else.  Called
from a test it worked; called from a helper function, as the mail-sending
tests call it, every one of them died with \"(void-function skip-unless)\"
instead of skipping.

Checked by catching the skip rather than being skipped by it."
  ;; before the let: binding a variable this file has not seen declared makes
  ;; it lexical, and the defvar in the required file then refuses it
  (require 'vm-send-live-init)
  ;; the skips here are provoked on purpose, so their reasons are not printed:
  ;; `vm-test-skip-unless' says why it is skipping, and a run on a configured
  ;; machine would otherwise carry two lines announcing there is no
  ;; configuration
  (cl-letf (((symbol-function 'message) #'ignore))
    (let ((vm-imap-test-servers nil))
      (should-error (funcall (lambda ()
                               (vm-imap-live-skip-unless-server "plain")))
                    :type 'ert-test-skipped))
  ;; and the same for the one the mail-sending tests call, with a config that
  ;; gets past its own check and into the IMAP one
  (let ((vm-imap-test-servers nil)
        (vm-send-test-config '(:from "me@example.com" :to "me@example.com"
                               :verify-server "nowhere"))
        (send-mail-function 'sendmail-send-it))
    (should-error (funcall (lambda () (vm-send-live-skip-unless-configured)))
                  :type 'ert-test-skipped))))

;;; ------------------------------------------------------------------
;;; Saving a message into an IMAP folder
;;; ------------------------------------------------------------------

(defun vm-imap-live-test--select-count (conn mailbox)
  "How many messages MAILBOX holds on CONN."
  (let ((text (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)))
    (when (string-match "^\\* \\([0-9]+\\) EXISTS" text)
      (string-to-number (match-string 1 text)))))

(defun vm-imap-live-test--body-on-server (conn mailbox n)
  "Message N of MAILBOX on CONN, headers and all."
  (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)
  (vm-imap-live-cmd-ok conn "FETCH %d (BODY.PEEK[])" n))

(defmacro vm-imap-live-test--with-a-file-folder (spec &rest body)
  "Visit a file folder of one message and run BODY in it.
SPEC is (SUBJECT).  The folder is a real file: saving from one to IMAP is
the path that sends the message, the other being a copy on the server."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-imap-save" t)))
          (folder (expand-file-name "outgoing" dir))
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-delete-after-saving nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (before (buffer-list)))
     (unwind-protect
         (progn
           (write-region
            (concat "From alice@example.com Sat Aug  8 14:24:13 2026\n"
                    "From: alice@example.com\n"
                    "To: vmtest@example.com\n"
                    (format "Subject: %s\n" ,(car spec))
                    "\n"
                    "The body of a message being saved.\n\n")
            nil folder nil 'quiet)
           (cl-letf (((symbol-function 'vm-display) #'ignore))
             (vm-visit-folder folder)
             (setq vm-message-pointer vm-message-list)
             ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest vm-imap-live-test-saving-a-message-reaches-the-server ()
  "A message saved from a file folder arrives in the IMAP mailbox, with the
flags it had here.  This is the other half of #38: that one saves between two
IMAP folders and the server copies; this sends the message, so what the server
ends up with is what VM put on the wire.

Read back with the harness' own client rather than with vm-imap.el."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain")
    (let ((vm-imap-server-timeout vm-imap-live-timeout)
          (account (car (plist-get server :accounts)))
          (subject "saved from a file folder"))
      (vm-imap-live-test--with-a-file-folder (subject)
        (let ((m (car vm-message-list)))
          (vm-set-new-flag m nil)
          (vm-set-unread-flag m nil)
          (vm-set-replied-flag m t)
          (vm-save-message-to-imap-folder
           (vm-imap-live-spec server account mailbox) 1)
          (vm-imap-net-wait nil 30)
          (should (vm-filed-flag m))))
      (should (equal (vm-imap-live-test--select-count conn mailbox) 1))
      (should (string-match-p (regexp-quote subject)
                              (vm-imap-live-test--body-on-server conn mailbox 1)))
      (let ((flags (vm-imap-live-flags-of conn mailbox 1)))
        (should (member "\\Seen" flags))
        (should (member "\\Answered" flags))))))

(ert-deftest vm-imap-live-test-saving-deletes-the-message-when-asked ()
  "`vm-delete-after-saving' marks the message deleted here once the server
has it, and not before: the deletion is the last thing the save does."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain")
    (let ((vm-imap-server-timeout vm-imap-live-timeout)
          (account (car (plist-get server :accounts))))
      (vm-imap-live-test--with-a-file-folder ("saved and deleted")
        (let ((vm-delete-after-saving t)
              (m (car vm-message-list)))
          (vm-save-message-to-imap-folder
           (vm-imap-live-spec server account mailbox) 1)
          (vm-imap-net-wait nil 30)
          (should (vm-deleted-flag m))))
      (should (equal (vm-imap-live-test--select-count conn mailbox) 1)))))

(ert-deftest vm-imap-live-test-saving-to-another-account-sends-the-message ()
  "Two mailboxes on one server but under different logins are not a copy the
server can make: VM has to send the message.  The account is part of what
decides that, not just the host, and the second account is the only way to
tell the two apart."
  (vm-imap-live-skip-unless-server "plain")
  (let* ((server (vm-imap-live-server "plain"))
         (accounts (plist-get server :accounts)))
    (vm-test-skip-unless
     (cdr accounts)
     (concat "Only one account on the server called plain.  The cross-account "
             "tests want two; see test/vm-live-config.el.template."))
    (require 'vm)
    (let* ((other (cadr accounts))
           (conn (vm-imap-live--open server))
           (mailbox nil)
           (vm-imap-server-timeout vm-imap-live-timeout)
           (vm-imap-passwords vm-imap-passwords)
           (vm-kept-imap-buffers vm-kept-imap-buffers)
           (vm-imap-keep-trace-buffer nil))
      (unwind-protect
          (progn
            (vm-imap-live-login conn server other)
            (vm-imap-live-namespace conn)
            (setq mailbox (vm-imap-live-mailbox-name conn))
            (vm-imap-live-cmd-ok conn "CREATE \"%s\"" mailbox)
            (vm-imap-live-test--with-a-file-folder ("saved across accounts")
              (vm-save-message-to-imap-folder
               (vm-imap-live-spec server other mailbox) 1)
              (vm-imap-net-wait nil 30))
            (should (equal (vm-imap-live-test--select-count conn mailbox) 1)))
        (when mailbox
          (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" mailbox)))
        (vm-imap-live-close conn)))))

(ert-deftest vm-imap-live-test-a-saved-to-mailbox-can-be-inside-a-directory ()
  "REGRESSION: saving to a mailbox in a directory that VM has to create works.
Issue #691.  Against a real server because it is the server that refuses
\"vmtest/\" as a mailbox name, and dovecot does: VM created the parents itself,
one CREATE per component, rather than leaving it to the server as RFC 3501
requires."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (let* ((server (vm-imap-live-server "plain"))
         (account (car (plist-get server :accounts)))
         (conn (vm-imap-live--open server))
         (mailbox nil)
         (vm-imap-server-timeout vm-imap-live-timeout)
         (vm-imap-passwords vm-imap-passwords)
         (vm-kept-imap-buffers vm-kept-imap-buffers)
         (vm-imap-keep-trace-buffer nil)
         (vm-delete-after-saving nil))
    (unwind-protect
        (progn
          (vm-imap-live-login conn server account)
          (vm-imap-live-namespace conn)
          ;; a name with the separator in it, which is what went wrong
          (setq mailbox (concat (vm-imap-live-mailbox-name conn) "/inside"))
          (vm-imap-live-test--with-a-file-folder ("saved into a directory")
            (vm-save-message-to-imap-folder
             (vm-imap-live-spec server account mailbox) 1)
            (vm-imap-net-wait nil 30))
          (should (equal (vm-imap-live-test--select-count conn mailbox) 1)))
      (when mailbox
        (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" mailbox)))
      (vm-imap-live-close conn))))

(ert-deftest vm-imap-live-test-copying-makes-the-mailbox-if-it-is-missing ()
  "REGRESSION: saving between two folders on one server makes the target if
it is not there.  Issue #690.  Against a real server because the answer to
CREATE on an existing mailbox, and to COPY into a missing one, is the server\='s
to give: dovecot says NO [TRYCREATE], which is what the copy path used to fail
on."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-mailbox (conn mailbox "plain"
                                   (list vm-imap-live-test--message))
    (let* ((account (car (plist-get server :accounts)))
           (target (concat mailbox "-target"))
           (vm-imap-server-timeout vm-imap-live-timeout)
           (vm-delete-after-saving nil))
      (unwind-protect
          (progn
            (vm-visit-imap-folder (vm-imap-live-spec server account mailbox))
            (vm-imap-net-wait nil 30)
            (vm-save-message-to-imap-folder
             (vm-imap-live-spec server account target) 1)
            (vm-imap-net-wait nil 30)
            (should (equal (vm-imap-live-test--select-count conn target) 1)))
        (when (eq major-mode 'vm-mode)
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))
        (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" target))))))

(provide 'vm-imap-live-test)

;;; vm-imap-live-test.el ends here
