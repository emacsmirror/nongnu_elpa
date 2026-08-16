;;; vm-imap-net-test.el --- IMAP over the non-blocking driver -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The IMAP reader written as generators, run against test/vm-imap-mock.el --
;; a real IMAP server on a local port, so what is tested is a session on a
;; socket rather than a string handed to a parser.
;;
;; The tokens are the ones vm-imap.el's own parser produces, positions in the
;; process buffer and all, so `vm-imap-response-matches' is what the tests
;; assert with, as the client code does.
;;
;; The tests wait for a session to finish.  That is the test waiting: the
;; session runs in the process filter, which is what would be happening
;; between a user's keystrokes.

;;; Code:

(require 'vm-test-init)
(require 'vm-imap-mock)
(require 'vm-imap-net)
(require 'vm-imap)
(require 'vm-net)

(defconst vm-imap-net-test--alice
  "From: alice@example.com\nTo: me@example.com\nSubject: badgers\n\nThe first body.\n"
  "A message for a mock mailbox.")

(defconst vm-imap-net-test--bob
  "From: bob@example.com\nTo: me@example.com\nSubject: otters\n\nThe second body.\n"
  "Another, so a test can tell one from the next.")

(defvar vm-imap-net-test--buffer nil
  "The process buffer of the session a test is running, for reading tokens.")

(defun vm-imap-net-test--run (mock iterator &optional timeout wait)
  "Run ITERATOR as an IMAP session against MOCK and answer with the session.
TIMEOUT is the session's own, WAIT how long this waits for it to finish."
  (let* ((buffer (generate-new-buffer " *vm-imap-net-test*"))
         (process (make-network-process
                   :name "vm-imap-net-test" :host 'local
                   :service (vm-imap-mock-port mock)
                   :buffer buffer :noquery t :coding 'binary))
         (session (vm-net-session :process process :name "imap"
                                  :timeout (or timeout 5))))
    (with-current-buffer buffer (vm-imap-net-init))
    (setq vm-imap-net-test--buffer buffer)
    (vm-net-start session iterator)
    (let ((deadline (+ (float-time) (or wait timeout 5))))
      (while (and (vm-net-session-live-p session) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (when (process-live-p process) (delete-process process))
    session))

(defmacro vm-imap-net-test--with-session (spec &rest body)
  "Run BODY with a mock server bound, and kill the session buffer after.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-imap-mock-start'."
  (declare (indent 1) (debug t))
  `(let ((vm-imap-net-test--buffer nil))
     (unwind-protect
         (vm-imap-mock-with ,spec ,@body)
       (when (buffer-live-p vm-imap-net-test--buffer)
         (kill-buffer vm-imap-net-test--buffer)))))

(defun vm-imap-net-test--matches (response &rest pattern)
  "Whether RESPONSE matches PATTERN, read in the session's own buffer.
The tokens are positions in that buffer, so they mean nothing anywhere else."
  (with-current-buffer vm-imap-net-test--buffer
    (apply #'vm-imap-response-matches response pattern)))

(defun vm-imap-net-test--text (token)
  "The text TOKEN covers."
  (with-current-buffer vm-imap-net-test--buffer
    (buffer-substring-no-properties (nth 1 token) (nth 2 token))))

;;; Reading a session

(iter-defun vm-imap-net-test--login (user password)
  (iter-yield-from (vm-imap-net-open-session user password)))

(ert-deftest vm-imap-net-test-a-session-greets-and-logs-in ()
  "The greeting, CAPABILITY and LOGIN, none of them waiting: the whole
exchange happens in the process filter."
  (vm-imap-net-test--with-session (mock)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret"))))
      (should (eq (vm-net-session-state session) 'done))
      (let ((capabilities (car (vm-net-session-value session))))
        (should (memq 'IMAP4REV1 capabilities))
        (should (memq 'UIDPLUS capabilities)))
      (should (vm-imap-mock-received-p mock "LOGIN")))))

(ert-deftest vm-imap-net-test-a-refused-login-signals ()
  "A tagged NO ends the session with the server's own words, rather than
leaving the caller to work out that nothing happened."
  (vm-imap-net-test--with-session (mock :refuse "LOGIN")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "server says"
                              (error-message-string
                               (vm-net-session-error session)))))))

(ert-deftest vm-imap-net-test-the-password-is-not-in-the-transcript ()
  "The session buffer is a transcript, and LOGIN's arguments are left out of
it, as the blocking implementation leaves them out."
  (vm-imap-net-test--with-session (mock)
    (vm-imap-net-test--run mock (vm-imap-net-test--login "vmtest" "secret"))
    (with-current-buffer vm-imap-net-test--buffer
      (should (string-match-p "LOGIN <parameters omitted>" (buffer-string)))
      (should-not (string-match-p "secret" (buffer-string))))))

;;; The tokens

(iter-defun vm-imap-net-test--select (user password mailbox)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command
                    (format "SELECT \"%s\"" mailbox) "SELECT")))

(ert-deftest vm-imap-net-test-select-comes-back-as-tokens ()
  "SELECT's untagged lines arrive as the tokens vm-imap.el's parser makes,
so `vm-imap-response-matches' reads them without knowing which reader
produced them."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice
                                                        vm-imap-net-test--bob))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--select "vmtest" "secret" "INBOX")))
           (lines (vm-net-session-value session)))
      (should (eq (vm-net-session-state session) 'done))
      ;; * 2 EXISTS
      (should (cl-some (lambda (line)
                         (vm-imap-net-test--matches line '* 2 'EXISTS))
                       lines))
      ;; * OK [UIDVALIDITY n]
      (should (cl-some (lambda (line)
                         (vm-imap-net-test--matches
                          line '* 'OK '(vector UIDVALIDITY atom)))
                       lines))
      ;; and the last line is the tagged one
      (should (vm-imap-net-test--matches (car (last lines)) 'VM 'OK)))))

(ert-deftest vm-imap-net-test-a-literal-arrives-whole ()
  "A {n} literal is read by its octet count.  A message body is one, and an
off-by-one here is a message with a line missing or one glued to the next."
  (let ((long (concat "From: alice@example.com\nSubject: long\n\n"
                      (mapconcat (lambda (n) (format "line %d" n))
                                 (number-sequence 1 500) "\n")
                      "\n")))
    (vm-imap-net-test--with-session (mock :messages (list long))
      (let* ((session (vm-imap-net-test--run
                       mock
                       (vm-imap-net-test--fetch "vmtest" "secret" "INBOX"
                                                "1 (RFC822)")))
             (lines (vm-net-session-value session))
             (fetch (car lines)))
        (should (eq (vm-net-session-state session) 'done))
        (should (vm-imap-net-test--matches fetch '* 1 'FETCH 'list))
        (let* ((contents (cdr (nth 3 fetch)))
               (text (vm-imap-net-test--text (nth 1 contents))))
          (should (string-match-p "^line 1$" text))
          (should (string-match-p "^line 500$" text))
          ;; every octet the server said it would send, and no more: the
          ;; count in the {n} is what says where the literal ends
          (should (equal (length text) (length long))))))))

(iter-defun vm-imap-net-test--fetch (user password mailbox spec)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command (format "SELECT \"%s\"" mailbox)))
  (iter-yield-from (vm-imap-net-command (format "FETCH %s" spec) "FETCH")))

(ert-deftest vm-imap-net-test-a-quoted-string-is-a-string-token ()
  "A quoted string comes back as a string token covering what was between
the quotes, which is how a flag list and a mailbox name arrive."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice))
    (let* ((session (vm-imap-net-test--run
                     mock (vm-imap-net-test--list "vmtest" "secret")))
           (lines (vm-net-session-value session))
           (listing (car lines)))
      (should (eq (vm-net-session-state session) 'done))
      (should (vm-imap-net-test--matches listing '* 'LIST 'list 'string 'string))
      (should (equal (vm-imap-net-test--text (nth 4 listing)) "INBOX")))))

(iter-defun vm-imap-net-test--list (user password)
  (iter-yield-from (vm-imap-net-open-session user password))
  (iter-yield-from (vm-imap-net-command "LIST \"\" \"*\"" "LIST")))

;;; What the driver does when the server does not play along

(ert-deftest vm-imap-net-test-a-dropped-connection-ends-the-session ()
  "The server hanging up mid-command ends the session rather than leaving a
generator waiting for input that cannot arrive."
  (vm-imap-net-test--with-session (mock :drop-on "SELECT")
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--select "vmtest" "secret" "INBOX"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (vm-net-session-error session)))))

(ert-deftest vm-imap-net-test-a-truncated-literal-ends-the-session ()
  "A FETCH cut off mid-literal is a session that fails, not one that hangs:
the octet count says how much is coming and the connection says it is not."
  (vm-imap-net-test--with-session (mock :messages (list vm-imap-net-test--alice)
                                        :truncate-fetch t)
    (let ((session (vm-imap-net-test--run
                    mock
                    (vm-imap-net-test--fetch "vmtest" "secret" "INBOX"
                                             "1 (RFC822)")
                    2 4)))
      (should (eq (vm-net-session-state session) 'failed)))))

(ert-deftest vm-imap-net-test-a-silent-server-times-the-session-out ()
  "A server that accepts the connection and says nothing ends the session
when the timeout runs out, rather than waiting for ever."
  (vm-imap-net-test--with-session (mock :slow-greeting 2)
    (let ((session (vm-imap-net-test--run
                    mock (vm-imap-net-test--login "vmtest" "secret") 0.5 4)))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session)))))))

;;; Tags

(ert-deftest vm-imap-net-test-each-command-has-a-tag-of-its-own ()
  "Every command is numbered, and `VM' in a pattern means the tag of the
command being waited for -- so a late response to an earlier command is not
read as the answer to this one."
  (vm-imap-net-test--with-session (mock)
    (vm-imap-net-test--run mock (vm-imap-net-test--select "vmtest" "secret"
                                                          "INBOX"))
    (let ((commands (vm-imap-mock-commands mock)))
      (should (equal (length commands) 4))
      (should (equal (mapcar (lambda (c) (car (split-string c " "))) commands)
                     '("vm1" "vm2" "vm3" "vm4"))))))

(provide 'vm-imap-net-test)

;;; vm-imap-net-test.el ends here
