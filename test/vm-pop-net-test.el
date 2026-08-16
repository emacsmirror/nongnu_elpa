;;; vm-pop-net-test.el --- POP over the non-blocking driver -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The POP protocol written as generators, run against test/vm-pop-mock.el --
;; a real POP3 server on a local port, so what is tested is a session on a
;; socket rather than a string handed to a parser.
;;
;; The tests wait for a session to finish.  That is the test waiting: the
;; session itself runs in the process filter, which is what would be happening
;; between a user's keystrokes.

;;; Code:

(require 'vm-test-init)
(require 'vm-pop-mock)
(require 'vm-pop-net)
(require 'vm-net)

(defconst vm-pop-net-test--alice
  "From: alice@example.com\r\nTo: me@example.com\r\nSubject: badgers\r\n\r\nThe first body.\r\n"
  "A message for the mock maildrop.")

(defconst vm-pop-net-test--bob
  "From: bob@example.com\r\nTo: me@example.com\r\nSubject: otters\r\n\r\nThe second body.\r\n"
  "Another, so a test can tell one from the next.")

(defun vm-pop-net-test--run (mock iterator &optional timeout)
  "Run ITERATOR as a POP session against MOCK and answer with the session.
Waits for it to finish, or for TIMEOUT seconds, whichever comes first."
  (let* ((buffer (generate-new-buffer " *vm-pop-net-test*"))
         (process (make-network-process
                   :name "vm-pop-net-test" :host 'local
                   :service (vm-pop-mock-port mock)
                   :buffer buffer :noquery t :coding 'binary))
         (session (vm-net-session :process process :name "pop"
                                  :timeout (or timeout 5))))
    (with-current-buffer buffer (vm-pop-net-init))
    (vm-net-start session iterator)
    (let ((deadline (+ (float-time) (or timeout 5))))
      (while (and (vm-net-session-live-p session) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (when (process-live-p process) (delete-process process))
    (when (buffer-live-p buffer) (kill-buffer buffer))
    session))

(defmacro vm-pop-net-test--with-mock (spec &rest body)
  "Run BODY with a mock POP server bound as in `vm-pop-mock-with'."
  (declare (indent 1) (debug t))
  `(vm-pop-mock-with ,spec ,@body))

;;; Reading

(ert-deftest vm-pop-net-test-a-session-logs-in-and-asks-what-is-there ()
  "Greeting, USER, PASS and STAT, none of which waited: every one of them
resumed in the filter when its line arrived."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-session (vm-pop-mock-user mock)
                                             (vm-pop-mock-password mock)))))
      (should (eq (vm-net-session-state session) 'done))
      (should (equal (car (vm-net-session-value session)) 2))
      (should (vm-pop-mock-received-p mock "\\`USER "))
      (should (vm-pop-mock-received-p mock "\\`STAT")))))

(iter-defun vm-pop-net-test--uidl (user password)
  (iter-yield-from (vm-pop-net-greeting))
  (iter-yield-from (vm-pop-net-authenticate user password))
  (iter-yield-from (vm-pop-net-uidl)))

(ert-deftest vm-pop-net-test-uidl-answers-the-uids-in-order ()
  "A multi-line response is read to its dot and comes back as lines."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--uidl (vm-pop-mock-user mock)
                                                (vm-pop-mock-password mock)))))
      (should (eq (vm-net-session-state session) 'done))
      (let ((uids (vm-net-session-value session)))
        (should (equal (mapcar #'car uids) '(1 2)))
        (should (cl-every #'stringp (mapcar #'cdr uids)))))))

(ert-deftest vm-pop-net-test-uidl-answers-nothing-without-support ()
  "A server with no UIDL answers -ERR, which is not a failed session: it is
a maildrop VM cannot identify messages in, and the caller decides what that
means."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :no-uidl t)
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--uidl (vm-pop-mock-user mock)
                                                (vm-pop-mock-password mock)))))
      (should (eq (vm-net-session-state session) 'done))
      (should-not (vm-net-session-value session)))))

(iter-defun vm-pop-net-test--retrieve (user password n)
  (iter-yield-from (vm-pop-net-greeting))
  (iter-yield-from (vm-pop-net-authenticate user password))
  (iter-yield-from (vm-pop-net-retrieve n)))

(ert-deftest vm-pop-net-test-a-message-arrives-whole ()
  "RETR reads to the dot, however many chunks the message arrives in, and a
line of the body that begins with one comes back with the one it started
with: the server doubles it on the way out (RFC 1939 3), and the client
takes the extra one off again."
  (vm-pop-net-test--with-mock
      (mock :messages (list (concat "From: alice@example.com\r\n"
                                    "Subject: dots\r\n\r\n"
                                    "a line\r\n"
                                    ".a line that began with a dot\r\n")))
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--retrieve (vm-pop-mock-user mock)
                                                    (vm-pop-mock-password mock)
                                                    1))))
      (should (eq (vm-net-session-state session) 'done))
      (let ((message (vm-net-session-value session)))
        (should (string-match-p "Subject: dots" message))
        (should (string-match-p "^\\.a line that began with a dot" message))))))

;;; What a server that misbehaves does to a session

(ert-deftest vm-pop-net-test-a-refused-login-fails-the-session ()
  "A -ERR to PASS ends the session with what the server said, rather than
nil for the caller to interpret."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-session (vm-pop-mock-user mock)
                                             "the wrong password"))))
      (should (eq (vm-net-session-state session) 'failed))
      (should (eq (car (vm-net-session-error session)) 'vm-pop-net-error))
      (should (string-match-p "-ERR"
                              (format "%s" (vm-net-session-error session)))))))

(ert-deftest vm-pop-net-test-a-silent-server-ends-the-session ()
  "A server that accepts the connection and then says nothing does not hold
Emacs: the session's timeout ends it.  That is the hang this whole exercise
is about, and here it is a test that finishes in a fifth of a second."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :silent-on "STAT")
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-session (vm-pop-mock-user mock)
                                             (vm-pop-mock-password mock))
                    0.4)))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session)))))))

(ert-deftest vm-pop-net-test-a-dropped-connection-ends-the-session ()
  "A server that hangs up mid-session ends it rather than waiting for input
that cannot arrive."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :drop-on "STAT")
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-session (vm-pop-mock-user mock)
                                             (vm-pop-mock-password mock))
                    1)))
      (should-not (eq (vm-net-session-state session) 'done)))))

;;; QUIT, which is what makes the server act

(ert-deftest vm-pop-net-test-a-session-says-quit-even-when-abandoned ()
  "The QUIT is in an `unwind-protect', and `vm-net-abandon' closes the
generator, so it is said.  A POP server that is not told QUIT rolls back the
session's deletions -- so a session that is dropped rather than closed loses
the user's deletes, silently."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let* ((buffer (generate-new-buffer " *vm-pop-net-test*"))
           (process (make-network-process
                     :name "vm-pop-net-test" :host 'local
                     :service (vm-pop-mock-port mock)
                     :buffer buffer :noquery t :coding 'binary))
           (session (vm-net-session :process process :name "pop")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (vm-pop-net-init))
            (vm-net-start session (vm-pop-net-session
                                   (vm-pop-mock-user mock)
                                   (vm-pop-mock-password mock)))
            ;; let it get as far as waiting for the server
            (accept-process-output nil 0.2)
            (vm-net-abandon session)
            (accept-process-output nil 0.2)
            (should (vm-pop-mock-received-p mock "\\`QUIT")))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;;; Checking for mail, over a maildrop specification

(defun vm-pop-net-test--check (mock retrieved &optional seconds)
  "Ask MOCK whether it has mail, with RETRIEVED as what VM has seen.
Answers what the callback was given."
  (let ((answer 'not-called)
        (vm-pop-retrieved-messages retrieved)
        (vm-pop-server-timeout 3))
    (vm-pop-net-check-mail (vm-pop-mock-spec mock)
                           (lambda (result) (setq answer result)))
    (let ((deadline (+ (float-time) (or seconds 5))))
      (while (and (eq answer 'not-called) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    answer))

(ert-deftest vm-pop-net-test-a-maildrop-with-new-mail-says-so ()
  "`vm-pop-net-check-mail' takes a maildrop specification, opens it, asks
UIDL and answers t through its callback -- and returns before any of that,
which is the whole point: the mail check runs on a timer, and today it stops
Emacs every time it fires."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (should (eq (vm-pop-net-test--check mock nil) t))))

(ert-deftest vm-pop-net-test-a-maildrop-already-read-says-nothing-new ()
  "A maildrop whose UIDs are all in `vm-pop-retrieved-messages' has no new
mail: that list is how VM remembers what it has taken."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let* ((spec (vm-pop-mock-spec mock))
           (popdrop (vm-popdrop-sans-password spec))
           ;; the mock's UID for message 1, as the server gives it
           (uid (cdr (car (vm-net-session-value
                           (vm-pop-net-test--run
                            mock (vm-pop-net-test--uidl
                                  (vm-pop-mock-user mock)
                                  (vm-pop-mock-password mock))))))))
      (should uid)
      (should-not (vm-pop-net-test--check mock (list (list uid popdrop 'uidl)))))))

(ert-deftest vm-pop-net-test-an-empty-maildrop-says-nothing-new ()
  "Nothing there is nothing new, which is not the same as a server that
cannot say."
  (vm-pop-net-test--with-mock (mock :messages nil)
    (should-not (vm-pop-net-test--check mock nil))))

(ert-deftest vm-pop-net-test-a-server-without-uidl-cannot-say ()
  "Without UIDL VM cannot tell what it has already taken, so the answer is
nil rather than a guess at t."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :no-uidl t)
    (should-not (vm-pop-net-test--check mock nil))))

(ert-deftest vm-pop-net-test-a-failed-check-hands-back-the-error ()
  "A check that cannot log in tells the callback what went wrong rather than
answering \"no mail\", which would be indistinguishable from an empty
maildrop."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((answer 'not-called)
          (vm-pop-retrieved-messages nil)
          (vm-pop-server-timeout 3)
          (spec (replace-regexp-in-string ":[^:]*\\'" ":wrong"
                                          (vm-pop-mock-spec mock))))
      (vm-pop-net-check-mail spec (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 5)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (consp answer))
      (should (eq (car answer) 'vm-pop-net-error)))))

(ert-deftest vm-pop-net-test-a-check-cleans-up-after-itself ()
  "The connection and its buffer go when the check ends, whichever way it
ends.  A check runs on a timer, so anything it leaves behind it leaves once
a minute."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((buffers (length (buffer-list)))
          (processes (length (process-list))))
      (should (eq (vm-pop-net-test--check mock nil) t))
      (should (equal (length (buffer-list)) buffers))
      (should (equal (length (process-list)) processes)))))

(ert-deftest vm-pop-net-test-a-maildrop-it-cannot-open-says-so ()
  "A maildrop whose connection would itself be a wait -- pop-ssl, pop-ssh,
or one whose password VM does not hold -- signals rather than pretending.
Those connect paths are converted with the connect, not here, and until
then a caller that meets this uses the blocking implementation."
  (should-error (vm-pop-net-open "pop-ssl:example.com:995:pass:user:secret" "x")
                :type 'vm-pop-net-unsupported)
  (should-error (vm-pop-net-open "pop:example.com:110:pass:user:*" "x")
                :type 'vm-pop-net-unsupported))

(provide 'vm-pop-net-test)

;;; vm-pop-net-test.el ends here
