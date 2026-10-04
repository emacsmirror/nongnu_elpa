;;; vm-imap-live-init.el --- Harness for live IMAP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Support for testing vm-imap.el against a real IMAP server.  See
;; dev/docs/design/imap-live-tests.org.
;;
;; The opt-in is a gitignored test/vm-live-config.el naming the servers to use;
;; copy test/vm-live-config.el.template to create one.  With it in place these
;; run as part of `make test' like anything else; without it they all skip.
;; `make test-imap' runs only this file, which is the convenient thing while
;; working on IMAP.
;;
;; So on a configured machine `make test' does reach the network and does take
;; longer.  Bind `vm-imap-live-enabled' to nil to suppress that without
;; removing the config.
;;
;; Everything here talks to the server with its own minimal IMAP client
;; rather than with vm-imap.el.  That is deliberate: if fixtures were built
;; with the code under test, a bug in it could corrupt the fixture and hide
;; itself -- which is the very complaint in issue #286, that VM acts on data
;; it may have got wrong.
;;
;; If the configured password is wrong, expect a slow cascade of timeouts
;; rather than a tidy row of authentication failures: dovecot's anvil service
;; applies an escalating per-IP penalty to repeated auth failures, so each
;; attempt waits longer than the last until it exceeds
;; `vm-imap-live-timeout'.  That is the server working as intended, not a
;; fault in the harness -- check the credentials first.

;;; Code:

(require 'vm-test-init)
(require 'cl-lib)

;; Load these before anything binds their variables.  Under lexical binding a
;; `let' on a name that is not yet special creates a *lexical* binding, and
;; the library's own `defvar' then fails with "Defining as dynamic an already
;; lexical var" the first time a TLS connection loads it.
(require 'nsm)
(require 'gnutls nil t)
(defvar network-security-level)
(defvar gnutls-trustfiles)

;;; ------------------------------------------------------------------
;;; Opt-in
;;; ------------------------------------------------------------------

(defvar vm-imap-live-enabled (vm-test-live-wanted-p)
  "Whether the live IMAP tests may run at all.
They run only when `vm-live-config-file' also exists, so the config is the real
opt-in.  Bind this to nil to keep a configured checkout from using the network
-- in CI, say -- without deleting the config.

The default answers VM_TEST_LIVE, so `make test-mock' and
`VM_TEST_LIVE=0 make test' run the mock servers alone on a machine that has a
live one.  See `vm-test-live-wanted-p'.")

(defvar vm-live-config-file
  (expand-file-name "vm-live-config.el" vm-test-dir)
  "Gitignored file describing the servers to test against, IMAP and POP alike.
Copy test/vm-live-config.el.template to create it; that template documents the
structure, and dev/docs/dev-guide.org has the server-side setup.")

(defvar vm-live-config-obsolete-file
  (expand-file-name "vm-imap-config.el" vm-test-dir)
  "What `vm-live-config-file' used to be called.
Still loaded, with a warning, so that an existing setup does not silently stop
testing anything -- which is what renaming a file whose absence means \"skip\"
would otherwise do.")

(defvar vm-imap-test-servers nil
  "List of IMAP server plists, set by `vm-live-config-file'.")

(defvar vm-imap-live-timeout 10
  "Seconds any single server interaction may take.
`vm-imap-server-timeout' defaults to nil, meaning never time out, so without
this a wedged test would hang forever.")

(defun vm-imap-live-load-config ()
  "Load `vm-live-config-file' if it exists.  Return non-nil if loaded.
Falls back to `vm-live-config-obsolete-file', warning about the name."
  (cond ((file-readable-p vm-live-config-file)
	 (load vm-live-config-file nil t)
	 t)
	((file-readable-p vm-live-config-obsolete-file)
	 (message "%s is the old name for %s; rename it."
		  (file-name-nondirectory vm-live-config-obsolete-file)
		  (file-name-nondirectory vm-live-config-file))
	 (load vm-live-config-obsolete-file nil t)
	 t)))

(defun vm-imap-live-available-p ()
  "Return non-nil if the harness is enabled and a server is configured."
  (and vm-imap-live-enabled vm-imap-test-servers t))

(defun vm-imap-live-server (name)
  "Return the configured server plist called NAME, or nil."
  (seq-find (lambda (s) (equal (plist-get s :name) name))
            vm-imap-test-servers))

(defun vm-imap-live-skip-unless-server (name)
  "Skip the running test unless a server called NAME is configured.
Skipping when nothing is configured is intended.  Failing when a configured
server cannot be reached is also intended, and happens later, at connect
time -- a suite that silently skips a server it was told about is a suite
with no coverage.

Written with `vm-test-skip-unless' rather than `skip-unless'.  `skip-unless' is bound
by `ert-deftest' with `cl-macrolet', so it exists only inside a test body:
a helper that used it worked when called from a test and failed with
\"(void-function skip-unless)\" when called from a function, which is what
happened to the mail-sending tests."
  (vm-test-skip-unless
   (vm-imap-live-available-p)
   (concat "No live IMAP configuration.  To run the live tests, copy "
	   "test/vm-live-config.el.template to test/vm-live-config.el and "
	   "fill in vm-imap-test-servers; see "
	   "dev/docs/design/imap-live-tests.org."))
  (vm-test-skip-unless
   (vm-imap-live-server name)
   (format (concat "No server called %s in vm-imap-test-servers.  Add one to "
		   "test/vm-live-config.el, or this server's tests stay "
		   "unrun.")
	   name)))

;;; ------------------------------------------------------------------
;;; Independent minimal IMAP client
;;; ------------------------------------------------------------------
;;
;; Just enough to build and inspect fixtures: no vm-imap.el, no parsing
;; beyond what the assertions need.

(cl-defstruct (vm-imap-live-conn (:constructor vm-imap-live--make-conn))
  process buffer (tag 0) capabilities prefix separator)

(defun vm-imap-live--open (server)
  "Open a connection to SERVER and read its greeting.  Return a conn."
  (let* ((host (plist-get server :host))
         (port (plist-get server :port))
         (tls (plist-get server :tls))
         (trustfile (plist-get server :trustfile))
         (buffer (generate-new-buffer
                  (format " *vm-imap-live %s*" (plist-get server :name))))
         ;; A self-signed certificate otherwise fails verification, or worse
         ;; prompts -- and a prompt under batch ert hangs.
         (gnutls-trustfiles (if trustfile
                                (cons trustfile
                                      (bound-and-true-p gnutls-trustfiles))
                              (bound-and-true-p gnutls-trustfiles)))
         (network-security-level (if tls 'low
                                   (bound-and-true-p network-security-level)))
         process)
    (condition-case err
        (with-timeout (vm-imap-live-timeout
                       (error "Timed out connecting to %s:%s" host port))
          (setq process (open-network-stream
                         "vm-imap-live" buffer host port
                         :type (if tls 'tls 'plain))))
      (error
       (kill-buffer buffer)
       ;; Configured but unreachable is a failure, never a skip.
       (signal (car err) (cdr err))))
    (set-process-query-on-exit-flag process nil)
    (let ((conn (vm-imap-live--make-conn :process process :buffer buffer)))
      (vm-imap-live--read-until conn "^\\* \\(OK\\|PREAUTH\\)")
      conn)))

(defun vm-imap-live--read-until (conn regexp)
  "Read from CONN until REGEXP matches a line.  Return all text read."
  (with-current-buffer (vm-imap-live-conn-buffer conn)
    (let ((start (point-max)))
      (with-timeout (vm-imap-live-timeout
                     (error "Timed out awaiting %s; got: %s"
                            regexp (buffer-substring start (point-max))))
        (goto-char start)
        (while (not (save-excursion
                      (goto-char start)
                      (re-search-forward regexp nil t)))
          (accept-process-output (vm-imap-live-conn-process conn) 0 200)))
      (buffer-substring-no-properties start (point-max)))))

(defun vm-imap-live--send (conn string)
  "Send STRING to CONN followed by CRLF."
  (process-send-string (vm-imap-live-conn-process conn)
                       (concat string "\r\n")))

(defun vm-imap-live-cmd (conn format &rest args)
  "Run a tagged IMAP command on CONN and return (STATUS . TEXT).
The command is FORMAT with ARGS applied to it by `format'.  STATUS is the
symbol ok, no or bad.  Signal if the command does not complete within
`vm-imap-live-timeout'."
  (let* ((tag (format "v%d" (cl-incf (vm-imap-live-conn-tag conn))))
         (command (apply #'format format args))
         text)
    (vm-imap-live--send conn (concat tag " " command))
    (setq text (vm-imap-live--read-until
                conn (concat "^" (regexp-quote tag) " \\(OK\\|NO\\|BAD\\)")))
    (cons (cond ((string-match (concat "^" (regexp-quote tag) " OK") text) 'ok)
                ((string-match (concat "^" (regexp-quote tag) " NO") text) 'no)
                (t 'bad))
          text)))

(defun vm-imap-live-cmd-ok (conn format &rest args)
  "Run FORMAT with ARGS on CONN as `vm-imap-live-cmd' does, requiring OK.
Signal an error unless the server tagged the response OK."
  (let ((result (apply #'vm-imap-live-cmd conn format args)))
    (unless (eq (car result) 'ok)
      (error "IMAP command failed: %s => %s"
             (apply #'format format args) (cdr result)))
    (cdr result)))

(defun vm-imap-live-login (conn server account)
  "Log in to CONN on SERVER as ACCOUNT, a (USER . PASSWORD) cons."
  ;; Quoted strings, so that passwords need no escaping here.  Note VM's own
  ;; maildrop spec is colon-delimited and cannot carry a password with a
  ;; colon in it; that is a constraint on the config, not on this client.
  (ignore server)
  (vm-imap-live-cmd-ok conn "LOGIN \"%s\" \"%s\"" (car account) (cdr account))
  conn)

(defun vm-imap-live-namespace (conn)
  "Discover and record the personal namespace prefix and separator of CONN.
Hardcoding these would tie the suite to one mailbox layout: Mark's dovecot is
mdbox, so LAYOUT=fs, giving an empty prefix and \"/\", where a Maildir++ box
would give \"INBOX.\" and \".\"."
  (let ((text (vm-imap-live-cmd-ok conn "NAMESPACE")))
    (if (string-match "\\* NAMESPACE ((\"\\([^\"]*\\)\" \"\\([^\"]*\\)\")" text)
        (setf (vm-imap-live-conn-prefix conn) (match-string 1 text)
              (vm-imap-live-conn-separator conn) (match-string 2 text))
      ;; NIL personal namespace, or no NAMESPACE support.
      (setf (vm-imap-live-conn-prefix conn) ""
            (vm-imap-live-conn-separator conn) "/"))
    (cons (vm-imap-live-conn-prefix conn)
          (vm-imap-live-conn-separator conn))))

(defun vm-imap-live-capabilities (conn)
  "Return CONN's capabilities as a list of upper-case strings."
  (or (vm-imap-live-conn-capabilities conn)
      (setf (vm-imap-live-conn-capabilities conn)
            (let ((text (vm-imap-live-cmd-ok conn "CAPABILITY")))
              (when (string-match "\\* CAPABILITY \\(.*\\)" text)
                (split-string (upcase (match-string 1 text)) "[ \r\n]+" t))))))

(defun vm-imap-live-close (conn)
  "Log out and tear down CONN, ignoring errors."
  (ignore-errors (vm-imap-live-cmd conn "LOGOUT"))
  (ignore-errors (delete-process (vm-imap-live-conn-process conn)))
  (when (buffer-live-p (vm-imap-live-conn-buffer conn))
    (kill-buffer (vm-imap-live-conn-buffer conn))))

;;; ------------------------------------------------------------------
;;; Throwaway mailboxes
;;; ------------------------------------------------------------------

(defvar vm-imap-live--mailbox-counter 0)

(defun vm-imap-live-mailbox-name (conn)
  "Return a fresh throwaway mailbox name on CONN, under vmtest.
Never INBOX.  The pid keeps concurrent runs from colliding."
  (let ((prefix (or (vm-imap-live-conn-prefix conn) ""))
        (sep (or (vm-imap-live-conn-separator conn) "/")))
    (format "%svmtest%s%d-%d" prefix sep (emacs-pid)
            (cl-incf vm-imap-live--mailbox-counter))))

(defun vm-imap-live-append (conn mailbox message &optional flags)
  "APPEND MESSAGE to MAILBOX on CONN, with optional FLAGS string.
Uses a LITERAL+ non-synchronising literal when the server offers one, so no
continuation handshake is needed."
  (let* ((crlf (replace-regexp-in-string "\n" "\r\n"
                                         (replace-regexp-in-string
                                          "\r\n" "\n" message)))
         (literal-plus (member "LITERAL+" (vm-imap-live-capabilities conn)))
         (header (format "APPEND \"%s\"%s {%d%s}"
                         mailbox (if flags (format " (%s)" flags) "")
                         (string-bytes crlf) (if literal-plus "+" ""))))
    (if literal-plus
        (progn
          (let ((tag (format "v%d" (cl-incf (vm-imap-live-conn-tag conn)))))
            (vm-imap-live--send conn (concat tag " " header))
            (vm-imap-live--send conn crlf)
            (let ((text (vm-imap-live--read-until
                         conn (concat "^" (regexp-quote tag)
                                      " \\(OK\\|NO\\|BAD\\)"))))
              (unless (string-match (concat "^" (regexp-quote tag) " OK") text)
                (error "APPEND failed: %s" text)))))
      ;; Synchronising literal: wait for the "+" continuation first.
      (let ((tag (format "v%d" (cl-incf (vm-imap-live-conn-tag conn)))))
        (vm-imap-live--send conn (concat tag " " header))
        (vm-imap-live--read-until conn "^\\+")
        (vm-imap-live--send conn crlf)
        (let ((text (vm-imap-live--read-until
                     conn (concat "^" (regexp-quote tag)
                                  " \\(OK\\|NO\\|BAD\\)"))))
          (unless (string-match (concat "^" (regexp-quote tag) " OK") text)
            (error "APPEND failed: %s" text)))))))

(defun vm-imap-live-flags-of (conn mailbox n)
  "Return the FLAGS of message N in MAILBOX on CONN, as a list of strings."
  (vm-imap-live-cmd-ok conn "SELECT \"%s\"" mailbox)
  (let ((text (vm-imap-live-cmd-ok conn "FETCH %d (FLAGS)" n)))
    (when (string-match "FLAGS (\\([^)]*\\))" text)
      (split-string (match-string 1 text) "[ \r\n]+" t))))

(defmacro vm-imap-live-with-mailbox (spec &rest body)
  "Create a throwaway mailbox, run BODY, then delete it.
SPEC is (CONN-VAR MAILBOX-VAR SERVER-NAME &optional MESSAGES), where MESSAGES
is a list of strings to APPEND in order.  The mailbox is removed even if BODY
signals, so a failed test does not leave state behind for the next one."
  (declare (indent 1) (debug t))
  (let ((conn-var (nth 0 spec))
        (mailbox-var (nth 1 spec))
        (server-name (nth 2 spec))
        (messages (nth 3 spec)))
    `(let* ((server (vm-imap-live-server ,server-name))
            (,conn-var (vm-imap-live--open server))
            (,mailbox-var nil)
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
             (vm-imap-live-login ,conn-var server
                                 (car (plist-get server :accounts)))
             (vm-imap-live-namespace ,conn-var)
             (setq ,mailbox-var (vm-imap-live-mailbox-name ,conn-var))
             (vm-imap-live-cmd-ok ,conn-var "CREATE \"%s\"" ,mailbox-var)
             (dolist (m ,messages)
               (vm-imap-live-append ,conn-var ,mailbox-var m))
             ,@body)
         (when ,mailbox-var
           (ignore-errors
             (vm-imap-live-cmd ,conn-var "DELETE \"%s\"" ,mailbox-var)))
         (vm-imap-live-close ,conn-var)))))

;;; ------------------------------------------------------------------
;;; Driving VM itself
;;; ------------------------------------------------------------------

(defun vm-imap-live-spec (server account mailbox)
  "Return a VM maildrop spec for MAILBOX on SERVER as ACCOUNT."
  (format "%s:%s:%s:%s:%s:%s:%s"
          (if (plist-get server :tls) "imap-ssl" "imap")
          (plist-get server :host)
          (plist-get server :port)
          mailbox
          (or (plist-get server :auth) "login")
          (car account)
          (cdr account)))

(defmacro vm-imap-live-with-vm-account (spec &rest body)
  "Run BODY with VM configured for SERVER-NAME, bound to nickname NICK.
SPEC is (SERVER-NAME NICK &optional MAILBOX).  Sets up
`vm-imap-account-alist' so the tests take the same nickname path a user
does, and binds the timeout, since `vm-imap-server-timeout' is nil by
default and would let a wedged session hang forever."
  (declare (indent 1) (debug t))
  (let ((server-name (nth 0 spec))
        (nick (nth 1 spec))
        (mailbox (or (nth 2 spec) "INBOX")))
    `(let* ((server (vm-imap-live-server ,server-name))
            (account (car (plist-get server :accounts)))
            (vm-imap-server-timeout vm-imap-live-timeout)
            ;; A visit records where it went; bound so the mailbox this test
            ;; invents does not turn up in a later test's history.
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
            (vm-imap-keep-trace-buffer nil)
            (vm-imap-account-alist
             (list (list (vm-imap-live-spec server account ,mailbox) ,nick)))
            (gnutls-trustfiles
             (if (plist-get server :trustfile)
                 (cons (plist-get server :trustfile)
                       (bound-and-true-p gnutls-trustfiles))
               (bound-and-true-p gnutls-trustfiles)))
            (network-security-level
             (if (plist-get server :tls) 'low
               (bound-and-true-p network-security-level))))
       ,@body)))

(vm-imap-live-load-config)

(provide 'vm-imap-live-init)

;;; vm-imap-live-init.el ends here
