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
            (,mailbox nil))
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
                       ,@body)
                   (when (eq major-mode 'vm-mode)
                     (let ((vm-confirm-quit nil))
                       (ignore-errors (vm-quit-no-change))))))))
         (when ,mailbox
           (ignore-errors (vm-imap-live-cmd conn "DELETE \"%s\"" ,mailbox)))
         (vm-imap-live-close conn)))))

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

It does not, on alpha.  VM raises a protocol error and leaves the cached
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
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (push prompt prompts) nil)))
          (should-error (vm-get-new-mail) :type 'error))
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
    (let ((message
           (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
             (condition-case err (progn (vm-get-new-mail) nil)
               (error (error-message-string err))))))
      (should message)
      (should (string-match-p "closed the connection" message))
      (should-not (string-match-p "Timed out" message)))))

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
      ;; The label survives, and is still pending, so a later sync can retry.
      (should (member "vmtest-refused" (vm-labels-of m)))
      (should (vm-attribute-modflag-of m)))))

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

(defconst vm-imap-live-test--bulky
  (concat "From: alice@example.com\r\n"
          "To: vmtest@example.com\r\n"
          "Subject: headers only fetch\r\n"
          "Date: Mon, 01 Jan 2024 00:00:00 +0000\r\n"
          "Message-ID: <vmtest-bulky@example.com>\r\n"
          "X-VMTest-Last-Header: sentinel\r\n"
          "\r\n"
          (mapconcat (lambda (n) (format "VMTESTBODY line %03d\r\n" n))
                     (number-sequence 1 60) ""))
  "A message whose body is comfortably larger than the size limit below.
Every body line carries VMTESTBODY, so body text appearing among the headers
is unmistakable.  The last header is a sentinel for the same reason.")

(defconst vm-imap-live-test--size-limit 200
  "`vm-imap-max-message-size' for the headers-only tests.
Smaller than `vm-imap-live-test--bulky', so VM fetches its headers only.")

(defun vm-imap-live-test--header-section ()
  "Return the header section of the message in the current buffer.
Everything up to the first empty line, which is where a body must never
appear."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (point-min)
     (if (re-search-forward "^\r?$" nil t) (point) (point-max)))))

(ert-deftest vm-imap-live-test-headers-only-fetch-keeps-body-out-of-headers ()
  "REGRESSION: fetching an external body puts it after the headers.
Issue #500, via the deleted README.headers-only: with headers-only IMAP
downloading, the body could be inserted in the *midst* of the headers rather
than after them.  Reported as infrequent and never diagnosed; the warning was
retired in 2f33c4b without a code change, so nothing has been shown either
way since 2010.

The suspect is `vm-fetch-message' (vm-mime.el).  After the handler inserts
the fetched message it deletes \"the new headers\" with

    (delete-region (vm-text-of mm)
                   (or (re-search-forward \"\\\\n\\\\n\" (point-max) t)
                       (point-max)))

and that search starts from wherever the handler left point, not from the
start of what was just inserted -- the IMAP handler ends at
`insert-buffer-substring', which leaves point after the inserted text.

This test states the invariant rather than the theory: however the fetch is
implemented, the body must end up after the headers and the headers must
survive intact."
  (vm-imap-live-skip-unless-server "plain")
  (require 'vm)
  (vm-imap-live-with-two-mailboxes (conn src _dst "plain")
    (ignore _dst)
    (vm-imap-live-append conn src vm-imap-live-test--bulky)
    (let ((vm-imap-server-timeout vm-imap-live-timeout)
          (vm-enable-external-messages '(imap))
          (vm-imap-max-message-size vm-imap-live-test--size-limit)
          ;; Never prompt about the large message; treat it as external.
          (vm-imap-ok-to-ask nil))
      (unwind-protect
          (progn
            (vm-visit-imap-folder (vm-imap-live-spec server account src))
            (should (= (length vm-message-list) 1))
            (let ((m (car vm-message-list)))
              ;; Precondition: it has to have arrived headers-only, or the
              ;; rest proves nothing about the fetch path.
              ;;
              ;; Today it never does, and this skips.  Headers-only
              ;; downloading does not engage: with
              ;; vm-enable-external-messages '(imap) and a message well over
              ;; vm-imap-max-message-size, vm-body-to-be-retrieved-of comes
              ;; back nil.  Reproduced repeatedly.
              ;;
              ;; Why is *not* established.  In vm-imap-retrieve-messages the
              ;; annotation looks correct on inspection -- retrieve-list
              ;; entries are (uid msn flag), the consumer reads (nth 2
              ;; r-entry), the size table is already populated by then, and
              ;; the size does exceed the limit -- and the same loop does set
              ;; vm-byte-count-of, which arrives correctly as "1201", so the
              ;; loop runs with a valid uid.  Something between computing the
              ;; flag and observing the message clears or bypasses it, and I
              ;; have not found what.  See the note on issue #500.
              ;;
              ;; A skip rather than a failure: this test is about what the
              ;; fetch does to the message, and it becomes able to say
              ;; something the moment headers-only downloading works.
              (skip-unless (vm-body-to-be-retrieved-of m))
              ;; The headers are intact and the body is absent so far.
              (let ((headers (vm-imap-live-test--header-section)))
                (should (string-match-p "X-VMTest-Last-Header: sentinel"
                                        headers))
                (should-not (string-match-p "VMTESTBODY" headers)))
              ;; Now fetch the body, the way displaying the message does.
              (vm-make-presentation-copy m)
              (with-current-buffer (or vm-presentation-buffer (current-buffer))
                (let ((headers (vm-imap-live-test--header-section))
                      (whole (buffer-substring-no-properties
                              (point-min) (point-max))))
                  ;; The body arrived...
                  (should (string-match-p "VMTESTBODY line 001" whole))
                  ;; ...the headers survived...
                  (should (string-match-p "X-VMTest-Last-Header: sentinel"
                                          headers))
                  (should (string-match-p "Subject: headers only fetch"
                                          headers))
                  ;; ...and no body line landed among them, which is #500.
                  (should-not (string-match-p "VMTESTBODY" headers))))))
        (when (eq major-mode 'vm-mode)
          (let ((vm-confirm-quit nil))
            (ignore-errors (vm-quit-no-change))))))))

(provide 'vm-imap-live-test)

;;; vm-imap-live-test.el ends here
