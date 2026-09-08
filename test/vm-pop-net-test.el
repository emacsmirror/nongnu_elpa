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
  "From: alice@example.com\nTo: me@example.com\nSubject: badgers\n\nThe first body.\n"
  "A message for the mock maildrop.

Terminated with LF, not CRLF: `vm-pop-mock--send-multiline' splits on LF and
appends the CRLF itself, so a fixture with CRLF in it goes on the wire as
CR CR LF and every line comes back with a stray CR (emacs-vm/vm#822).")

(defconst vm-pop-net-test--bob
  "From: bob@example.com\nTo: me@example.com\nSubject: otters\n\nThe second body.\n"
  "Another, so a test can tell one from the next.")

(defun vm-pop-net-test--run (mock iterator &optional timeout wait)
  "Run ITERATOR as a POP session against MOCK and answer with the session.
TIMEOUT is the session's own, WAIT how long this waits for it to finish --
longer than TIMEOUT where the point is to see the session time out, or the
two race and the test sometimes sees the process being torn down instead."
  (let* ((buffer (generate-new-buffer " *vm-pop-net-test*"))
         (process (make-network-process
                   :name "vm-pop-net-test" :host 'local
                   :service (vm-pop-mock-port mock)
                   :buffer buffer :noquery t :coding 'binary))
         (session (vm-net-session :process process :name "pop"
                                  :timeout (or timeout 5))))
    (with-current-buffer buffer (vm-pop-net-init))
    (vm-net-start session iterator)
    (let ((deadline (+ (float-time) (or wait timeout 5))))
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

(ert-deftest vm-pop-net-test-the-password-is-not-in-the-transcript ()
  "A session's buffer holds no password.

It is kept as the trace `vm-pop-submit-bug-report' sends, so a credential in
it would be mailed to the maintainer.  Nothing sent is echoed there at all --
where the IMAP driver echoes each command and leaves LOGIN's arguments out,
this writes only what the server said."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    ;; `vm-pop-net-test--run' kills the session buffer, so the session is set
    ;; up here to keep it and read it afterwards
    (let* ((buffer (generate-new-buffer " *vm-pop-net-transcript*"))
           (process (make-network-process
                     :name "vm-pop-net-transcript" :host 'local
                     :service (vm-pop-mock-port mock)
                     :buffer buffer :noquery t :coding 'binary))
           (session (vm-net-session :process process :name "pop" :timeout 5)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (vm-pop-net-init))
            (vm-net-start session (vm-pop-net-test--uidl
                                   (vm-pop-mock-user mock)
                                   (vm-pop-mock-password mock)))
            (let ((deadline (+ (float-time) 5)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (with-current-buffer buffer
              (let ((transcript (buffer-string)))
                (should-not (string-match-p
                             (regexp-quote (vm-pop-mock-password mock))
                             transcript))
                (should-not (string-match-p "PASS" transcript))
                ;; and it is a transcript of something: the server answered
                (should (string-match-p "\\+OK" transcript)))))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

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
                    0.4 3)))
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

(defun vm-pop-net-test--say-and-fail (text)
  "Log TEXT in full and fail with it.
ert elides a long message where the backtrace prints it, and what a stall says
about itself is the whole point of saying it."
  (message "%s" text)
  (ert-fail text))

(defun vm-pop-net-test--timer-report ()
  "Every timer there is, and whether it will fire again.
A repeating timer whose function signalled is left triggered and never
rescheduled, and stays in `timer-list' firing nothing.  One of those sitting
where the watchdog should be is how a session waits out its whole read with
the answer already in its buffer."
  (mapconcat (lambda (timer)
               (format "%s(%s)" (timer--function timer)
                       (if (timer--triggered timer) "dead" "waiting")))
             timer-list ", "))

(defun vm-pop-net-test--wait-until (done seconds what)
  "Pump until DONE answers non-nil, and fail with WHAT if SECONDS pass first.

A wait that runs out says what the session was doing, what its buffer holds
and what the timers are, because a wait that only says \"nothing came\" is a
wait that has to be debugged from scratch: this is what found the watchdog
that had stopped firing."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall done)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (funcall done)
      (let* ((session (and (boundp 'vm-pop-net-session) vm-pop-net-session))
             (process (and session (vm-net-session-process session)))
             (buffer (and session (vm-net-session-buffer session))))
        (vm-pop-net-test--say-and-fail
         (format (concat "%s: nothing after %s seconds.  Session %s, process %s,"
                         " request %s, resuming %s, buffer %s.  Timers: %s")
                 what seconds
                 (if session
                     (format "%s (live %s, error %S)"
                             (vm-net-session-name session)
                             (and (vm-net-session-live-p session) t)
                             (vm-net-session-error session))
                   "none")
                 (if process
                     (format "%s, %s" (process-status process)
                             (if (eq (process-get process 'vm-net-session)
                                     session)
                                 "carrying this session"
                               "not carrying it: nothing will poll"))
                   "none")
                 (if (and session (vm-net-session-request session))
                     (format "outstanding, says %s"
                             (if (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (funcall (vm-net-session-request session)))
                               "no buffer"))
                   "none")
                 (and session (vm-net-session-resuming session) t)
                 (if (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (format "%d bytes ending %S" (buffer-size)
                               (buffer-substring
                                (max (point-min) (- (point-max) 40))
                                (point-max))))
                   "gone")
                 (vm-pop-net-test--timer-report)))))))

(defun vm-pop-net-test--check (mock retrieved &optional seconds)
  "Ask MOCK whether it has mail, with RETRIEVED as what VM has seen.
Answers what the callback was given."
  (let ((answer 'not-called)
        (vm-pop-retrieved-messages retrieved)
        (vm-pop-server-timeout 3))
    (vm-pop-net-check-mail (vm-pop-mock-spec mock)
                           (lambda (result) (setq answer result)))
    (vm-pop-net-test--wait-until (lambda () (not (eq answer 'not-called)))
                                 (or seconds 20) "the mail check")
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
      (let ((deadline (+ (float-time) 20)))
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
  "A maildrop whose password VM does not hold signals rather than pretending:
there is nobody to ask from inside a filter, and a caller that meets it uses
the blocking implementation, which can ask.  So does a protocol this does not
speak."
  ;; no password and nobody to ask: the work does not start, quietly
  (should-error (vm-pop-net-open "pop:example.com:110:pass:user:*" "x")
                :type 'vm-pop-net-no-password)
  ;; but a maildrop that is not POP at all is an error the reader sees:
  ;; there is no other path to hand it to
  (let* ((text-quoting-style 'grave)
         (message (cadr (should-error
                         (vm-pop-net-open
                          "imap:example.com:143:INBOX:login:user:x" "x")))))
    (should (string-match-p "not a POP maildrop type" message))))

;;; The mail check that runs on a timer

(defmacro vm-pop-net-test--in-a-folder-with-spool (spec &rest body)
  "Visit a folder whose spool file is MOCK's maildrop, and run BODY in it.
SPEC is (MOCK-VAR &rest ARGS) as for `vm-pop-mock-start'."
  (declare (indent 1) (debug t))
  `(vm-pop-mock-with (,(car spec) ,@(cdr spec))
     (let* ((dir (file-name-as-directory (make-temp-file "vm-check" t)))
            (folder (expand-file-name "inbox" dir))
            (crash (expand-file-name "crash" dir))
            (vm-init-file nil)
            (vm-preferences-file nil)
            (vm-confirm-quit nil)
            (vm-frame-per-folder nil)
            (vm-mutable-frame-configuration nil)
            (vm-folder-history vm-folder-history)
            (vm-last-visit-folder vm-last-visit-folder)
            (vm-global-block-new-mail nil)
            ;; visiting must not fetch the mail first: what is being tested
            ;; is the check that says whether there is any
            (vm-auto-get-new-mail nil)
            ;; generous: this fixture runs late in a suite of a couple of
            ;; thousand tests, and a check that times out is read as "no mail"
            (vm-pop-server-timeout 15)
            (vm-pop-retrieved-messages nil)
            (vm-crash-box crash)
            (vm-spool-files (list (list folder (vm-pop-mock-spec ,(car spec))
                                        crash)))
            (before (buffer-list)))
       (unwind-protect
           (progn
             (write-region "" nil folder nil 'quiet)
             (cl-letf (((symbol-function 'vm-display) #'ignore))
               (vm-visit-folder folder)
               ,@body))
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory dir t)))))

(ert-deftest vm-pop-net-test-a-pop-save-sends-the-deletions-without-waiting ()
  "Saving a POP folder deletes on the server without waiting for it.

The blocking save also worked out what the server no longer has, which means
downloading the maildrop's UIDs; that is the next fetch's business.  Deletions
that do not get through stay in `vm-pop-messages-to-expunge', which is in the
folder file, so the next save offers them again."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-pop-mock-spec mock))
          (vm-pop-server-timeout 10))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-folder-access-method 'pop)
            (setq vm-folder-access-data (make-vector 10 nil))
            (vm-set-folder-pop-maildrop-spec spec)
            (setq vm-pop-messages-to-expunge (list "uid1"))
            (should (eq (vm-pop-net-send-changes) t))
            (vm-pop-net-test--wait-until (lambda () (not (vm-pop-net-busy-p)))
                                         10 "the deletions")
            ;; gone on the server, and off the folder's list
            (should (equal (vm-pop-mock-deleted mock) '(1)))
            (should-not vm-pop-messages-to-expunge))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(ert-deftest vm-pop-net-test-a-quit-that-fails-keeps-the-deletions ()
  "A server that will not commit the deletions leaves them to be asked again.

QUIT is what makes a POP server act on a session's DELEs, and RFC 1939 3.5
lets it answer -ERR when it could not remove them.  VM wrote QUIT blind and
never read that answer, so it struck the messages off
`vm-pop-messages-to-expunge\=' while the maildrop still had them."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :refuse "\\`QUIT")
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-pop-mock-spec mock))
          (vm-pop-server-timeout 10))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-folder-access-method 'pop)
            (setq vm-folder-access-data (make-vector 10 nil))
            (vm-set-folder-pop-maildrop-spec spec)
            (setq vm-pop-messages-to-expunge (list "uid1"))
            (should (eq (vm-pop-net-send-changes) t))
            (should (vm-pop-net-wait nil 10))
            ;; the DELE went out, the server would not commit it, and the
            ;; request is still there for the next save
            (should (vm-pop-mock-received-p mock "\\`DELE"))
            (should (equal vm-pop-messages-to-expunge (list "uid1"))))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(ert-deftest vm-pop-net-test-a-deletion-of-what-is-gone-settles ()
  "A request to delete a message the maildrop no longer lists is done with.

Someone else deleted it, or an earlier session did and its answer was lost.
The request stayed on `vm-pop-messages-to-expunge\=' either way, so every save
opened a session to ask for a message that was not there -- for ever.  What
the maildrop does not list is settled; what it lists and would not delete
stays, so the next save offers that again."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-pop-mock-spec mock))
          (vm-pop-server-timeout 10))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-folder-access-method 'pop)
            (setq vm-folder-access-data (make-vector 10 nil))
            (vm-set-folder-pop-maildrop-spec spec)
            (setq vm-pop-messages-to-expunge (list "uid1" "went-away"))
            (should (eq (vm-pop-net-send-changes) t))
            (should (vm-pop-net-wait nil 10))
            (should (equal (vm-pop-mock-deleted mock) '(1)))
            (should-not vm-pop-messages-to-expunge)
            ;; and a second save has nothing to ask for
            (should-not (vm-pop-net-send-changes)))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(ert-deftest vm-pop-net-test-a-deletion-can-be-asked-for-again ()
  "A deletion the server did not do is still on the list to be asked again.

Which is what makes a crash safe: the list is in the folder file, so a
deletion that did not get through -- Emacs gone before the QUIT, the server
refusing -- is offered by the next save."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :refuse "DELE")
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-pop-mock-spec mock))
          (vm-pop-server-timeout 10))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-folder-access-method 'pop)
            (setq vm-folder-access-data (make-vector 10 nil))
            (vm-set-folder-pop-maildrop-spec spec)
            (setq vm-pop-messages-to-expunge (list "uid1"))
            (should (eq (vm-pop-net-send-changes) t))
            (should (vm-pop-net-wait nil 10))
            ;; nothing deleted, and the request kept
            (should-not (vm-pop-mock-deleted mock))
            (should (equal vm-pop-messages-to-expunge (list "uid1")))
            ;; the next save asks again, and this time the server takes it
            (setf (vm-pop-mock-refuse mock) nil)
            (should (eq (vm-pop-net-send-changes) t))
            (should (vm-pop-net-wait nil 10))
            (should (equal (vm-pop-mock-deleted mock) '(1)))
            (should-not vm-pop-messages-to-expunge))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(ert-deftest vm-pop-net-test-a-pop-save-during-a-session-waits ()
  "Deletions asked for while a session is running go up next time.
Two POP sessions to one maildrop is what the server refuses anyway, and two
writing one folder is what corrupts it."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-pop-mock-spec mock)))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-folder-access-method 'pop)
            (setq vm-folder-access-data (make-vector 10 nil))
            (vm-set-folder-pop-maildrop-spec spec)
            (setq vm-pop-messages-to-expunge (list "uid1"))
            (let ((session (vm-net-session :name "stuck")))
              (setf (vm-net-session-buffer session)
                    (generate-new-buffer " *vm-pop-net-test-stuck*"))
              (vm-net-start session (vm-pop-net-test--never-finishes))
              (setq vm-pop-net-session session)
              (should (vm-pop-net-busy-p))
              (should (eq (vm-pop-net-send-changes) 'later))
              ;; nothing sent, nothing lost
              (should-not (vm-pop-mock-deleted mock))
              (should (equal vm-pop-messages-to-expunge (list "uid1")))
              (vm-net-abandon session)
              (let ((buffer (vm-net-session-buffer session)))
                (when (buffer-live-p buffer) (kill-buffer buffer)))))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(iter-defun vm-pop-net-test--never-finishes ()
  "A session that waits for something that never arrives."
  (iter-yield (lambda () nil))
  'never)

(ert-deftest vm-pop-net-test-expunging-says-so-with-no-password ()
  "`vm-expunge-pop-messages' says so when the driver cannot start.

It used to fall back to a blocking expunge, a session per maildrop with Emacs
held for all of them, which is the second implementation this does not have
any more.  A command that answers a keystroke with silence looks as though it
worked, so it says what happened instead."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-pop-net-expunge-retrieved) (lambda () nil))
              ((symbol-function 'vm-follow-summary-cursor) #'ignore)
              ((symbol-function 'vm-pop-expunge-entries)
               (lambda (&rest _) (error "the blocking expunge was called")))
              ((symbol-function 'vm-inform)
               (lambda (_level format &rest args)
                 (push (apply #'format format args) said))))
      (with-temp-buffer
        ;; `vm-select-folder-buffer-and-validate' and
        ;; `vm-error-if-virtual-folder' are defsubsts, inlined into the
        ;; compiled command, so stubbing the symbols does nothing: the buffer
        ;; has to be a folder buffer for real
        (setq major-mode 'vm-mode)
        (setq-local vm-pop-retrieved-messages (list (list "uid1" "pop:h:110:p:pass:u:*" 'uidl)))
        (vm-expunge-pop-messages)
        ;; the record is untouched: nothing was expunged, so nothing is forgotten
        (should (equal (length vm-pop-retrieved-messages) 1)))
      (should (seq-find (lambda (line) (string-match-p "no password" line)) said)))))

(ert-deftest vm-imap-net-test-expunging-says-so-with-no-password ()
  "`vm-expunge-imap-messages' says so when the driver cannot start.
It discarded the answer, so with no password the command did nothing and said
nothing."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-imap-net-expunge-retrieved) (lambda () nil))
              ((symbol-function 'vm-follow-summary-cursor) #'ignore)
              ((symbol-function 'vm-inform)
               (lambda (_level format &rest args)
                 (push (apply #'format format args) said))))
      (with-temp-buffer
        ;; a folder buffer for real; see the test above
        (setq major-mode 'vm-mode)
        (vm-expunge-imap-messages))
      (should (seq-find (lambda (line) (string-match-p "no password" line)) said)))))

(ert-deftest vm-pop-net-test-expunging-a-maildrop-does-not-wait ()
  "`vm-expunge-pop-messages' deletes on the server what the folder retrieved,
one maildrop at a time and without waiting for any of it.

What the server took is forgotten and what it did not is kept, so an expunge
that fails half way leaves the rest to be offered again."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-popdrop-sans-password (vm-pop-mock-spec mock)))
          (vm-pop-server-timeout 10)
          ;; what the folder holds is the maildrop without its password, as
          ;; `vm-pop-retrieved-messages' does; the password is the one VM
          ;; learned when it fetched
          (vm-pop-passwords (list (list (vm-popdrop-sans-password
                                         (vm-pop-mock-spec mock))
                                        (vm-pop-mock-password mock)))))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-pop-retrieved-messages
                  (list (list "uid1" spec 'uidl) (list "uid2" spec 'uidl)))
            (should (eq (vm-pop-net-expunge-retrieved) t))
            ;; asked, not answered: nothing is deleted yet
            (should-not (vm-pop-mock-deleted mock))
            (let ((deadline (+ (float-time) 10)))
              (while (and vm-pop-retrieved-messages (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (equal (sort (copy-sequence (vm-pop-mock-deleted mock)) #'<)
                           '(1 2)))
            (should-not vm-pop-retrieved-messages))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(ert-deftest vm-pop-net-test-a-maildrop-that-refuses-keeps-its-messages ()
  "A maildrop that will not delete keeps its entries, so the next expunge
offers them again rather than forgetting messages that are still there."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice)
                                    :refuse "DELE")
    (let ((folder (generate-new-buffer " *vm-pop-net-test-folder*"))
          (spec (vm-popdrop-sans-password (vm-pop-mock-spec mock)))
          (vm-pop-server-timeout 10)
          (vm-pop-passwords (list (list (vm-popdrop-sans-password
                                         (vm-pop-mock-spec mock))
                                        (vm-pop-mock-password mock))))
          (warned nil))
      (unwind-protect
          (with-current-buffer folder
            (setq vm-pop-retrieved-messages (list (list "uid1" spec 'uidl)))
            (cl-letf (((symbol-function 'vm-warn)
                       (lambda (_level _seconds &rest args)
                         (setq warned (apply #'format args)))))
              (should (eq (vm-pop-net-expunge-retrieved) t))
              (let ((deadline (+ (float-time) 10)))
                (while (and (not warned) (< (float-time) deadline))
                  (accept-process-output nil 0.05))))
            (should warned)
            (should-not (vm-pop-mock-deleted mock))
            (should (equal (length vm-pop-retrieved-messages) 1)))
        (when (buffer-live-p folder)
          (with-current-buffer folder (set-buffer-modified-p nil))
          (kill-buffer folder))))))

(defun vm-pop-net-test--settle (&optional seconds)
  "Let the outstanding mail checks answer."
  (let ((deadline (+ (float-time) (or seconds 20))))
    (while (and vm-mail-checks-outstanding (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(ert-deftest vm-pop-net-test-the-mail-check-does-not-wait ()
  "`vm-check-for-spooled-mail' starts the check and returns.  It cannot know
the answer yet -- the connection has only just been made -- and that is the
point: the check runs on a timer, and every round of it stopped Emacs for as
long as the server took."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let ((started (float-time)))
      (should-not (vm-check-for-spooled-mail nil t))
      (should (< (- (float-time) started) 0.5))
      (should vm-mail-checks-outstanding))
    ;; and the answer arrives afterwards
    (vm-pop-net-test--settle)
    (should vm-spooled-mail-waiting)
    (should-not vm-mail-checks-outstanding)))

(ert-deftest vm-pop-net-test-the-next-round-counts-the-last-answer ()
  "The round after the answer reports the mail: a check that cannot answer
in the round that started it would otherwise never report anything."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (vm-check-for-spooled-mail nil t)
    (vm-pop-net-test--settle)
    (should (vm-check-for-spooled-mail nil t))))

(ert-deftest vm-pop-net-test-an-empty-maildrop-reports-nothing ()
  "A maildrop with nothing in it answers nil, and the folder says so."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages nil)
    (vm-check-for-spooled-mail nil t)
    (vm-pop-net-test--settle)
    (should-not vm-spooled-mail-waiting)
    (should-not (vm-check-for-spooled-mail nil t))))

(ert-deftest vm-pop-net-test-one-check-at-a-time-for-a-maildrop ()
  "A second round while the first check is still out does not start another.
The timer fires every `vm-mail-check-interval' seconds and a server slower
than that would otherwise collect a connection per round."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice)
                                                 :silent-on "UIDL")
    (vm-check-for-spooled-mail nil t)
    (should (equal (length vm-mail-checks-outstanding) 1))
    (vm-check-for-spooled-mail nil t)
    (vm-check-for-spooled-mail nil t)
    (should (equal (length vm-mail-checks-outstanding) 1))))

(ert-deftest vm-pop-net-test-a-failed-check-leaves-the-last-answer ()
  "A check that fails does not report \"no mail\": a folder that had mail
waiting goes on saying so while the server is down, which is the truth as
far as anyone knows."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (vm-check-for-spooled-mail nil t)
    (vm-pop-net-test--settle)
    (should vm-spooled-mail-waiting)
    (vm-note-mail-waiting (current-buffer)
                          (nth 1 (car vm-spool-files))
                          '(vm-pop-net-error "server said no"))
    (should vm-spooled-mail-waiting)))

(ert-deftest vm-pop-net-test-a-maildrop-that-needs-waiting-is-left-alone ()
  "A pop-ssh maildrop is checked the old way: its connect starts a tunnel
program, which is a wait of its own.  So is a maildrop whose password VM
does not hold, which would ask for one from a timer.  POP over TLS is fine:
Emacs negotiates it as the connection comes up."
  (should (vm-pop-net-checkable-p "pop:127.0.0.1:110:pass:user:secret"))
  (should (vm-pop-net-checkable-p "pop-ssl:host:995:pass:user:secret"))
  (should-not (vm-pop-net-checkable-p "pop-ssh:host:110:pass:user:secret"))
  (should-not (vm-pop-net-checkable-p "pop:127.0.0.1:110:pass:user:*")))

;;; Connecting, which does not wait either

(ert-deftest vm-pop-net-test-a-refused-connection-answers-the-callback ()
  "A port with nothing listening ends the session through its sentinel, so
the caller is told rather than left waiting.  `make-network-process' with
:nowait returns before it knows, and this is where the answer arrives."
  (let ((answer 'not-called)
        (vm-pop-retrieved-messages nil)
        (vm-pop-server-timeout 3)
        ;; a port nothing is on: opened and closed again, so it is free
        (port (let* ((probe (make-network-process
                             :name "vm-pop-net-probe" :server t :host 'local
                             :service t :family 'ipv4 :noquery t))
                     (number (process-contact probe :service)))
                (delete-process probe)
                number)))
    (vm-pop-net-check-mail (format "pop:127.0.0.1:%d:pass:user:secret" port)
                           (lambda (result) (setq answer result)))
    (let ((deadline (+ (float-time) 20)))
      (while (and (eq answer 'not-called) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (consp answer))
    (should (memq (car answer) '(vm-net-connection-lost file-error)))))

(ert-deftest vm-pop-net-test-a-connection-is-open-before-it-is-open ()
  "The session starts while the connection is still coming up: the generator
asks for input and the filter feeds it when there is any.  Nothing here
waits for the connect, which is what `:nowait' is for."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let* ((opened (let ((vm-pop-server-timeout 3))
                     (vm-pop-net-open (vm-pop-mock-spec mock) "POP test")))
           (session (car opened))
           (process (vm-net-session-process session)))
      (unwind-protect
          (progn
            (should (memq (process-status process) '(connect open run)))
            (vm-net-start session (vm-pop-net-session (nth 1 opened)
                                                      (nth 2 opened)))
            (let ((deadline (+ (float-time) 20)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (should (equal (car (vm-net-session-value session)) 1)))
        (when (process-live-p process) (delete-process process))
        (let ((buffer (vm-net-session-buffer session)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;;; Fetching

(defun vm-pop-net-test--fetch (mock retrieved &optional seconds)
  "Fetch from MOCK what RETRIEVED does not have, and answer with the result."
  (let ((answer 'not-called)
        (vm-pop-server-timeout 3)
        (vm-pop-max-message-size nil)
        (vm-pop-messages-per-session nil))
    (vm-pop-net-fetch (vm-pop-mock-spec mock) retrieved
                      (lambda (result) (setq answer result)))
    (vm-pop-net-test--wait-until (lambda () (not (eq answer 'not-called)))
                                 (or seconds 20) "the fetch")
    answer))

(ert-deftest vm-pop-net-test-fetching-brings-back-every-new-message ()
  "The messages come back oldest first, each with the UID it is known by --
which is what the folder needs to remember so it does not fetch it twice."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((fetched (vm-pop-net-test--fetch mock nil)))
      (should (equal (length fetched) 2))
      (should (string-match-p "badgers" (cdr (nth 0 fetched))))
      (should (string-match-p "otters" (cdr (nth 1 fetched))))
      (should (cl-every #'stringp (mapcar #'car fetched))))))

(ert-deftest vm-pop-net-test-the-fetch-says-it-began-and-not-each-message ()
  "The fetch says it began, and keeps the per-message count out of the way.
As for IMAP: the reader is using Emacs while it runs, so the count goes to the
mode line and the log, and the echo area gets the start and the end."
  (let ((vm-verbosity 5)                ; the default
        (said nil))
    (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                      vm-pop-net-test--bob))
      (let ((inform (symbol-function 'vm-inform)))
        (cl-letf (((symbol-function 'vm-inform)
                   (lambda (level &rest args)
                     (push (cons level (apply #'format-message args)) said)
                     (apply inform level args))))
          (should (equal (length (vm-pop-net-test--fetch mock nil)) 2))))
      (let ((shown (mapcar #'cdr
                           (seq-filter (lambda (line) (<= (car line) vm-verbosity))
                                       said))))
        (should (seq-find (lambda (text)
                            (string-match-p "retrieving 2 messages" text))
                          shown))
        (should-not (seq-find (lambda (text)
                                (string-match-p "of 2 messages retrieved" text))
                              shown))
        ;; recorded, for a fetch that has to be explained afterwards
        (should (seq-find (lambda (line)
                            (and (= (car line) 6)
                                 (string-match-p "2 of 2 messages retrieved"
                                                 (cdr line))))
                          said))))))

(ert-deftest vm-pop-net-test-fetching-passes-over-what-it-has ()
  "A message whose UID is in `vm-pop-retrieved-messages' is not fetched
again: that list is how VM remembers, and fetching twice is how a folder
gets duplicates."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let* ((spec (vm-pop-mock-spec mock))
           (popdrop (vm-popdrop-sans-password spec))
           (all (vm-pop-net-test--fetch mock nil))
           (first-uid (car (nth 0 all))))
      (should (equal (length all) 2))
      (let ((rest (vm-pop-net-test--fetch
                   mock (list (list first-uid popdrop 'uidl)))))
        (should (equal (length rest) 1))
        (should (string-match-p "otters" (cdr (car rest))))))))

(ert-deftest vm-pop-net-test-the-fetch-deletes-nothing ()
  "The fetch marks nothing deleted, whatever the maildrop is set to.

A DELE takes effect at the QUIT that ends the session, and that QUIT is sent
whether the session finished or failed.  Deleting as the messages come down
would therefore commit the deletion of messages whose text went with the
session that failed: fetched, deleted on the server, written nowhere.  What
deletes them is `vm-pop-net-delete-fetched\=', once the crash box is on disk."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((vm-pop-expunge-after-retrieving t)
          (vm-pop-auto-expunge-alist nil))
      (should (equal (length (vm-pop-net-test--fetch mock nil)) 1))
      (should-not (vm-pop-mock-received-p mock "\\`DELE"))
      (should (vm-pop-mock-received-p mock "\\`QUIT")))))

(ert-deftest vm-pop-net-test-fetching-stops-at-the-session-limit ()
  "`vm-pop-messages-per-session' bounds one session's work: a maildrop with
a great many messages in it should not be one command that runs for ever."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice
                                                    vm-pop-net-test--bob))
    (let ((answer 'not-called)
          (vm-pop-server-timeout 3)
          (vm-pop-max-message-size nil)
          (vm-pop-messages-per-session 1))
      (vm-pop-net-fetch (vm-pop-mock-spec mock) nil
                        (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 20)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal (length answer) 1)))))

(ert-deftest vm-pop-net-test-fetching-passes-over-a-message-too-big ()
  "`vm-pop-max-message-size' is asked before RETR, not after: the size comes
from LIST, so an enormous message is never pulled down to be measured."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((answer 'not-called)
          (vm-pop-server-timeout 3)
          (vm-pop-max-message-size 10)
          (vm-pop-messages-per-session nil))
      (vm-pop-net-fetch (vm-pop-mock-spec mock) nil
                        (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 20)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not answer)
      (should-not (vm-pop-mock-received-p mock "\\`RETR")))))

(ert-deftest vm-pop-net-test-a-fetch-that-fails-says-so ()
  "A fetch that cannot log in hands the error to the callback, rather than
an empty list that reads as an empty maildrop."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let ((answer 'not-called)
          (vm-pop-server-timeout 3)
          (spec (replace-regexp-in-string ":[^:]*\\'" ":wrong"
                                          (vm-pop-mock-spec mock))))
      (vm-pop-net-fetch spec nil (lambda (result) (setq answer result)))
      (let ((deadline (+ (float-time) 20)))
        (while (and (eq answer 'not-called) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (consp answer))
      (should (eq (car answer) 'vm-pop-net-error)))))

;;; Into a folder

(defun vm-pop-net-test--get-mail (mock crash &optional seconds)
  "Fetch from MOCK into CRASH from the current folder, and answer the result."
  (let ((answer 'not-called)
        (vm-pop-server-timeout 3)
        (vm-pop-max-message-size nil)
        (vm-pop-messages-per-session nil))
    (vm-pop-net-get-mail (vm-pop-mock-spec mock) crash
                         (lambda (result) (setq answer result)))
    (vm-pop-net-test--wait-until (lambda () (not (eq answer 'not-called)))
                                 (or seconds 25) "the fetch into the folder")
    answer))

(ert-deftest vm-pop-net-test-mail-arrives-in-a-crash-box-vm-can-read ()
  "The fetched messages are written where VM recovers from: a crash box, in
the folder's own type, which `vm-gobble-crash-box' reads here and after a
crash alike.  Gobbling it leaves the folder holding them."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice
                                                       vm-pop-net-test--bob))
    (let* ((crash (nth 2 (car vm-spool-files)))
           (written (vm-pop-net-test--get-mail mock crash)))
      (should (equal written 2))
      (should (file-exists-p crash))
      ;; gobbling puts the text in the folder; making messages of it is the
      ;; next thing the folder does, and is what a caller would do here
      (vm-gobble-crash-box crash)
      (vm-assimilate-new-messages)
      (should (equal (length vm-message-list) 2))
      (should (equal (mapcar #'vm-su-subject vm-message-list)
                     '("badgers" "otters"))))))

(ert-deftest vm-pop-net-test-what-arrived-is-not-fetched-again ()
  "The UIDs go into `vm-pop-retrieved-messages', so a second fetch brings
nothing: that list is what stops a folder filling with duplicates."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let ((crash (nth 2 (car vm-spool-files))))
      (should (equal (vm-pop-net-test--get-mail mock crash) 1))
      (should (equal (length vm-pop-retrieved-messages) 1))
      (should (equal (vm-pop-net-test--get-mail mock crash) 0)))))

(defun vm-pop-net-test--wait-for (predicate &optional seconds)
  "Pump until PREDICATE answers non-nil, or SECONDS pass.  Answers what it saw."
  (let ((deadline (+ (float-time) (or seconds 20)))
        (answer nil))
    (while (and (not (setq answer (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    answer))

(ert-deftest vm-pop-net-test-the-mode-line-counts-what-has-arrived ()
  "The POP mode line says how far the fetch has got, message by message."
  (let ((seen nil))
    (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                   (list vm-pop-net-test--alice
                                                         vm-pop-net-test--bob))
      (let ((crash (nth 2 (car vm-spool-files)))
            (folder (current-buffer))
            (real (symbol-function 'vm-pop-net-note-progress)))
        (cl-letf (((symbol-function 'vm-pop-net-note-progress)
                   (lambda (&rest args)
                     (apply real args)
                     (with-current-buffer folder
                       (push (substring-no-properties (or vm-ml-session "")) seen)))))
          (should (equal (vm-pop-net-test--get-mail mock crash) 2)))
        (setq seen (nreverse seen))
        (should (equal (car seen) " fetching 0/2 "))
        (should (equal (car (last seen)) " fetching 2/2 "))
        (should-not vm-ml-session)))))

(ert-deftest vm-pop-net-test-two-maildrops-into-one-folder-take-turns ()
  "Two POP maildrops among a folder's spool files are fetched in turn.

`vm-get-new-mail\=' starts each without waiting, so both ran at once against
one folder: two crash boxes written and gobbled into one buffer, and the
folder's session slot pointing at the second while the first went on
unowned -- invisible to `vm-pop-net-busy-p\=', to the mode line and to
`vm-pop-net-stop\='.  There was no refusal on this side at all."
  (vm-pop-mock-with (one :messages (list "From: a@example.com\nSubject: from-one\n\nA.\n"))
    (vm-pop-mock-with (two :messages (list "From: b@example.com\nSubject: from-two\n\nB.\n"))
      (let* ((dir (file-name-as-directory (make-temp-file "vm-pop-two" t)))
             (local (expand-file-name "inbox" dir))
             (vm-pop-server-timeout 10)
             (vm-frame-per-folder nil)
             (vm-mutable-frame-configuration nil)
             (vm-auto-get-new-mail nil)
             (vm-spool-files (list (list local (vm-pop-mock-spec one)
                                         (concat local ".crash1"))
                                   (list local (vm-pop-mock-spec two)
                                         (concat local ".crash2"))))
             (before (buffer-list))
             (overlapped nil)
             (real (symbol-function 'vm-pop-net-take-session)))
        (unwind-protect
            (progn
              (write-region "" nil local nil 'quiet)
              (cl-letf (((symbol-function 'vm-display) #'ignore)
                        ((symbol-function 'vm-pop-net-take-session)
                         (lambda (session &rest more)
                           (when (and vm-pop-net-session
                                      (not (eq vm-pop-net-session session))
                                      (vm-net-session-live-p vm-pop-net-session))
                             (setq overlapped t))
                           (apply real session more))))
                (vm-visit-folder local)
                (vm-get-new-mail)
                (let ((deadline (+ (float-time) 20)))
                  (while (and (or (vm-pop-net-busy-p) vm-pop-net-waiting)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.05))))
              (should-not overlapped)
              (should (equal (sort (mapcar #'vm-su-subject vm-message-list)
                                   #'string-lessp)
                             '("from-one" "from-two"))))
          (dolist (buffer (buffer-list))
            (unless (memq buffer before)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer))))
          (delete-directory dir t))))))

(ert-deftest vm-pop-net-test-a-fetch-that-times-out-says-so ()
  "A fetch whose server goes quiet reports the timeout, and does not hang.

The timeout error was not a defined condition, so the callback took it for a
list of messages and died writing the crash box; the caller was left waiting
for an answer that had come and gone.  The session was over and nothing had
been said -- which is the one thing an asynchronous fetch must not do."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice)
                                                 :silent-on "UIDL")
    (let* ((crash (nth 2 (car vm-spool-files)))
           (vm-pop-server-timeout 2)
           (result (vm-pop-net-test--get-mail mock crash 20)))
      (should (vm-net-error-p result))
      (should (eq (car result) 'vm-net-timeout))
      (should-not (file-exists-p crash)))))

(ert-deftest vm-pop-net-test-a-fetch-that-fails-deletes-nothing ()
  "A fetch that fails part way leaves every message on the server.

The maildrop is set to delete what is fetched, and the second RETR is
refused.  Deleting as the fetch went would have sent DELE for the first
message; the QUIT that ends a failed session is sent all the same, so the
server would have acted on it -- and the text of that message went with the
session, having never reached the crash box."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice
                                                       vm-pop-net-test--bob)
                                                 :refuse "\\`RETR 2")
    (let ((crash (nth 2 (car vm-spool-files)))
          (vm-pop-expunge-after-retrieving t)
          (vm-pop-auto-expunge-alist nil))
      (let ((result (vm-pop-net-test--get-mail mock crash)))
        (should (consp result))
        (should (eq (car result) 'vm-pop-net-error)))
      (should-not (file-exists-p crash))
      (should (vm-pop-mock-received-p mock "\\`QUIT"))
      (should-not (vm-pop-mock-deleted mock)))))

(ert-deftest vm-pop-net-test-what-was-written-is-deleted-afterwards ()
  "A maildrop set to delete has its messages deleted once they are written.

In a session of its own and by UID, after the crash box is on disk: what the
server still holds is what VM has not saved yet."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let ((crash (nth 2 (car vm-spool-files)))
          (vm-pop-expunge-after-retrieving t)
          (vm-pop-auto-expunge-alist nil))
      (should (equal (vm-pop-net-test--get-mail mock crash) 1))
      (should (file-exists-p crash))
      (should (vm-pop-net-test--wait-for
               (lambda () (vm-pop-mock-deleted mock)) 10))
      (should (equal (vm-pop-mock-deleted mock) '(1)))
      ;; the deletion is a second session: it logs in again
      (should (>= (length (seq-filter (lambda (line)
                                        (string-prefix-p "USER" line))
                                      (vm-pop-mock-commands mock)))
                  2)))))

(ert-deftest vm-pop-net-test-a-failed-fetch-writes-no-crash-box ()
  "A fetch that fails hands the error on and leaves no crash box behind: a
crash box is a promise that there is mail in it."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice)
                                                 :drop-on "UIDL")
    (let* ((crash (nth 2 (car vm-spool-files)))
           (result (vm-pop-net-test--get-mail mock crash)))
      (should (consp result))
      (should-not (file-exists-p crash)))))

(ert-deftest vm-pop-net-test-the-callback-runs-in-the-folder ()
  "The callback is given the folder buffer it was started from, not whatever
buffer the reader happened to be in when the answer arrived."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let* ((folder (current-buffer))
           (crash (nth 2 (car vm-spool-files)))
           (seen nil)
           (answer 'not-called)
           (vm-pop-server-timeout 3))
      (vm-pop-net-get-mail (vm-pop-mock-spec mock) crash
                           (lambda (result)
                             (setq seen (current-buffer) answer result)))
      (with-temp-buffer
        (let ((deadline (+ (float-time) 20)))
          (while (and (eq answer 'not-called) (< (float-time) deadline))
            (accept-process-output nil 0.05))))
      (should (eq seen folder)))))

(ert-deftest vm-pop-net-test-expunging-follows-the-setting ()
  "Whether the server is told to delete what was fetched is
`vm-pop-auto-expunge-alist' and `vm-pop-expunge-after-retrieving', the same
two the blocking path asks.  A maildrop is named there without its password
but with the colon and star that stands in for it, which is what
`vm-popdrop-sans-password' makes of it."
  (let ((vm-pop-expunge-after-retrieving t)
        (vm-pop-auto-expunge-alist nil))
    (should (vm-pop-net-auto-expunge-p "pop:h:110:pass:user:secret")))
  (let ((vm-pop-expunge-after-retrieving nil)
        (vm-pop-auto-expunge-alist '(("pop:h:110:pass:user:*" . t))))
    (should (vm-pop-net-auto-expunge-p "pop:h:110:pass:user:secret")))
  (let ((vm-pop-expunge-after-retrieving t)
        (vm-pop-auto-expunge-alist '(("pop:h:110:pass:user:*" . nil))))
    (should-not (vm-pop-net-auto-expunge-p "pop:h:110:pass:user:secret"))))


;;; What the server actually sends

(defconst vm-pop-net-test--blank-lines
  (concat "From: alice@example.com\n"
          "Subject: blank lines\n"
          "\n"
          "First paragraph.\n"
          "\n"
          "Second paragraph, after a blank line.\n")
  "A message with the blank lines that matter: the one before the body, and
the one between two paragraphs.  The server sends every line CRLF-terminated,
which is where they were being lost.")

(ert-deftest vm-pop-net-test-a-blank-line-is-part-of-the-message ()
  "The blank line between the headers and the body survives the read.

Dropping empty lines takes that one with them, and a message whose headers
run straight into its text has no body at all: every line of it is read as
another header.  The blank lines inside the body go the same way, which is
every paragraph break in the message."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--blank-lines))
    (let ((fetched (vm-pop-net-test--fetch mock nil)))
      (should (equal (length fetched) 1))
      (let ((text (cdr (car fetched))))
        ;; headers, then a blank line, then the body
        (should (string-match-p "Subject: blank lines\n\nFirst paragraph" text))
        ;; and the paragraph break inside the body
        (should (string-match-p "First paragraph\.\n\nSecond paragraph" text))))))

(ert-deftest vm-pop-net-test-a-message-that-ends-in-a-blank-line-keeps-it ()
  "Only the terminating CRLF of the last line is taken off, not every one:
a body that ends in blank lines ends in them here too.  Trimming every
trailing CRLF would run two messages together in the folder, the separator
between them being the blank line."
  (vm-pop-net-test--with-mock (mock :messages
                                    (list (concat "Subject: trailing\n\n"
                                                  "Text.\n\n\n")))
    (let* ((fetched (vm-pop-net-test--fetch mock nil))
           (text (cdr (car fetched))))
      (should (string-match-p "Text\.\n\n\n" text)))))


;;; The connections that go through something else

(defvar vm-pop-net-test--stunnel-script
  (concat "printf '+OK ready\\r\\n'\n"
	  "while IFS= read -r line; do\n"
	  "  case \"$line\" in\n"
	  "    USER*) printf '+OK send the password\\r\\n' ;;\n"
	  "    PASS*) printf '+OK logged in\\r\\n' ;;\n"
	  "    STAT*) printf '+OK 2 640\\r\\n' ;;\n"
	  "    QUIT*) printf '+OK bye\\r\\n'; exit 0 ;;\n"
	  "  esac\n"
	  "done\n")
  "A shell script that talks enough POP to be logged in to.
Stands in for stunnel, which VM talks to over its standard input and output
rather than over a socket.")

(ert-deftest vm-pop-net-test-an-stunnel-maildrop-talks-over-the-programs-pipes ()
  "A pop-ssl maildrop with `vm-stunnel-program' set runs the program and talks
to it over its pipes.

As for IMAP: VM used to hand stunnel `-d 127.0.0.1:PORT' and wait for that
port, which stunnel has no such option for, so the session failed after the
whole server timeout with \"did not start listening on port\"."
  (let* ((vm-stunnel-program "sh")
         (vm-stunnel-program-switches nil)
         (vm-pop-server-timeout 10)
         (spec "pop-ssl:far.example.com:995:pass:vmtest:secret")
         (opened nil)
         (session nil))
    (cl-letf (((symbol-function 'vm-setup-stunnel-random-data-if-needed)
               (lambda () nil))
              ((symbol-function 'vm-stunnel-configuration-args)
               (lambda (&rest _) (list "-c" vm-pop-net-test--stunnel-script))))
      (setq opened (vm-pop-net-open spec "stunnel")
            session (car opened)))
    (unwind-protect
        (let ((process (vm-net-session-process session)))
          (should (processp process))
          (should (eq (process-type process) 'real))
          (should-not (member "-d" (process-command process)))
          (vm-net-start session (vm-pop-net-session (nth 1 opened)
                                                    (nth 2 opened)))
          (let ((deadline (+ (float-time) 10)))
            (while (and (vm-net-session-live-p session)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (vm-net-session-state session) 'done))
          (should (equal (vm-net-session-value session) '(2 . 640))))
      (let ((process (vm-net-session-process session)))
        (when (process-live-p process) (delete-process process)))
      (let ((buffer (vm-net-session-buffer session)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vm-pop-net-test-an-ssh-maildrop-waits-for-its-tunnel ()
  "A pop-ssh maildrop starts the tunnel and has no connection until it is
listening.  The blocking path waits inside `vm-setup-ssh-tunnel', with a loop
of connect attempts to find a free port and an `accept-process-output' for
the tunnel to come up."
  (let* ((vm-ssh-program "sleep")
         (vm-ssh-program-switches nil)
         (vm-ssh-remote-command "")
         (vm-pop-server-timeout 0.5)
         (spec "pop-ssh:far.example.com:110:pass:vmtest:secret")
         (opened (vm-pop-net-open spec "ssh"))
         (session (car opened))
         (finished nil))
    (setf (vm-net-session-finished session) (lambda (s) (setq finished s)))
    (unwind-protect
        (progn
          (should-not (vm-net-session-process session))
          (vm-net-start session (vm-pop-net-greeting))
          (should (eq (vm-net-session-state session) 'running))
          (should-not (vm-net-session-request session))
          (let ((deadline (+ (float-time) 10)))
            (while (and (vm-net-session-live-p session)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (eq (vm-net-session-state session) 'failed))
          (should (eq finished session))
          (should (string-match-p "did not start listening"
                                  (error-message-string
                                   (vm-net-session-error session)))))
      (let ((buffer (vm-net-session-buffer session)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vm-pop-net-test-a-tunnelled-session-reads-what-comes-back ()
  "With something listening where the tunnel would be, the session runs as
any other does."
  (vm-pop-net-test--with-mock (mock :messages (list vm-pop-net-test--alice))
    (let* ((port (vm-pop-mock-port mock))
           (session (vm-net-session :name "tunnelled" :timeout 10))
           (buffer (generate-new-buffer " *vm-pop-net-test*")))
      (with-current-buffer buffer (vm-pop-net-init))
      (setf (vm-net-session-buffer session) buffer)
      (unwind-protect
          (progn
            (vm-net-start session (vm-pop-net-session "vmtest" "secret"))
            (should-not (vm-net-session-request session))
            (vm-net-tunnel session "sleep" (list "30") port 5
                           (lambda (tunnel)
                             (when tunnel
                               (vm-net-attach
                                session
                                (vm-pop-net-connect "tunnelled" "127.0.0.1"
                                                    port buffer)))))
            (let ((deadline (+ (float-time) 10)))
              (while (and (vm-net-session-live-p session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should (eq (vm-net-session-state session) 'done))
            (should (equal (car (vm-net-session-value session)) 1)))
        (let ((process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))


;;; More of what only happens because nothing waits

(ert-deftest vm-pop-net-test-a-check-during-a-fetch-does-not-start-a-second ()
  "The mail check runs on a timer, so it fires while a fetch is running.
It answers from what it already knows rather than opening a session of its
own into the same folder."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let ((crash (nth 2 (car vm-spool-files))))
      (vm-pop-net-get-mail (vm-pop-mock-spec mock) crash #'ignore)
      (should (vm-pop-net-busy-p))
      ;; a check while that runs
      (vm-check-for-spooled-mail nil t)
      (should (vm-pop-net-wait nil 25))
      (vm-pop-net-test--settle)
      ;; one login, so the check did not open a session of its own on top
      (should (equal (cl-count-if (lambda (c) (string-match-p "\\`USER" c))
                                  (vm-pop-mock-commands mock))
                     1)))))

(ert-deftest vm-pop-net-test-the-session-is-the-folders-own ()
  "`vm-pop-net-busy-p' is asked of a folder, and a summary buffer counts as
its folder: the session belongs to the folder, and a caller asking from the
summary was told there was nothing running."
  (vm-pop-net-test--in-a-folder-with-spool (mock :messages
                                                 (list vm-pop-net-test--alice))
    (let ((folder (current-buffer))
          (summary (generate-new-buffer " *pop test summary*")))
      (unwind-protect
          (progn
            (setq vm-pop-net-session
                  (vm-pop-net-fetch (vm-pop-mock-spec mock) nil #'ignore))
            (should (vm-pop-net-busy-p))
            (with-current-buffer summary
              (setq vm-mail-buffer folder)
              (should (vm-pop-net-busy-p))
              (should (vm-pop-net-wait nil 20))
              ;; and the caller is left where it was
              (should (eq (current-buffer) summary))))
        (kill-buffer summary)))))


;;; APOP, and not downgrading to a cleartext password (emacs-vm/vm#823)

(iter-defun vm-pop-net-test--login (user password auth)
  "Greet and authenticate as AUTH asks, through the dispatch production uses.
`vm-pop-net-auth' is buffer-local to the session buffer and set by
`vm-pop-net-open', which this harness does not go through."
  (setq vm-pop-net-auth auth)
  (let ((greeting (iter-yield-from (vm-pop-net-greeting))))
    (iter-yield-from (vm-pop-net-authenticate user password greeting))))

(ert-deftest vm-pop-net-test-an-apop-maildrop-is-not-downgraded ()
  "REGRESSION: an apop maildrop does not send its password in clear.

`vm-pop-net-open' read every field of the maildrop but the authentication
method, and `vm-pop-net-authenticate' always sent USER and PASS.  So a
maildrop written `apop' was served with the password in clear, which is the
one thing asking for APOP is asking not to happen (emacs-vm/vm#823)."
  (vm-pop-net-test--with-mock (mock)
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--login (vm-pop-mock-user mock)
                                                 (vm-pop-mock-password mock)
                                                 "apop"))))
      (should (eq (vm-net-session-state session) 'done))
      (should (vm-pop-mock-received-p mock "\\`APOP "))
      ;; and the password was never sent as itself
      (should-not (vm-pop-mock-received-p mock "\\`PASS ")))))

(ert-deftest vm-pop-net-test-a-pass-maildrop-still-uses-pass ()
  "The other side of it: `pass' is unchanged."
  (vm-pop-net-test--with-mock (mock)
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--login (vm-pop-mock-user mock)
                                                 (vm-pop-mock-password mock)
                                                 "pass"))))
      (should (eq (vm-net-session-state session) 'done))
      (should (vm-pop-mock-received-p mock "\\`USER "))
      (should (vm-pop-mock-received-p mock "\\`PASS "))
      (should-not (vm-pop-mock-received-p mock "\\`APOP ")))))

(ert-deftest vm-pop-net-test-a-wrong-apop-digest-is-refused ()
  "The mock checks the digest, so this covers the arithmetic and not only
that an APOP command was sent."
  (vm-pop-net-test--with-mock (mock :password "secret")
    (let ((session (vm-pop-net-test--run
                    mock (vm-pop-net-test--login (vm-pop-mock-user mock)
                                                 "wrong" "apop"))))
      (should-not (eq (vm-net-session-state session) 'done))
      (should (vm-pop-mock-received-p mock "\\`APOP ")))))

(ert-deftest vm-pop-net-test-apop-without-a-timestamp-is-an-error ()
  "REGRESSION: a server offering no timestamp is an error, not a fall back.
Falling back to PASS would send the password in clear, which is what the
maildrop asked not to happen; the blocking implementation refuses too."
  (should-not (vm-pop-net-timestamp "+OK POP3 server ready"))
  (should (equal "<1896.697170952@dbc.mtview.ca.us>"
                 (vm-pop-net-timestamp
                  "+OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>"))))

(ert-deftest vm-pop-net-test-rpop-says-it-is-gone ()
  "REGRESSION: an rpop maildrop is told what happened to rpop.

It was RFC 1081\='s trusted-host scheme: a privileged source port stood for
the authentication and the password went under another verb.  Nothing
offers it, and VM no longer serves it either -- so the error names what to
write instead, rather than reporting an authentication VM does not
recognise (emacs-vm/vm#822)."
  (vm-pop-mock-with (mock)
    (let ((text-quoting-style 'grave))
      (let ((message (cadr (should-error
                            (vm-pop-net-open (vm-pop-mock-spec mock "rpop")
                                             "rpop test" nil)))))
        (should (string-match-p "no longer supported" message))
        (should (string-match-p "pass or apop" message))
        (should (string-match-p "VM manual" message))))))

(ert-deftest vm-pop-net-test-an-auth-the-driver-cannot-do-is-declined ()
  "REGRESSION: an authentication VM does not know is refused, not attempted.
Ignoring the field is what made the apop downgrade possible."
  (vm-pop-mock-with (mock)
    (let* ((text-quoting-style 'grave)
           (message (cadr (should-error
                           (vm-pop-net-open
                            (vm-pop-mock-spec mock "kerberos_v4")
                            "unknown auth" nil)))))
      (should (string-match-p "kerberos_v4" message))
      (should (string-match-p "pass, or apop" message)))
    ;; and the two it does serve are opened
    (dolist (auth '("pass" "apop"))
      (let ((opened (vm-pop-net-open (vm-pop-mock-spec mock auth)
                                     "auth test" nil)))
        (should opened)
        (let* ((session (nth 0 opened))
               (process (vm-net-session-process session)))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p (vm-net-session-buffer session))
            (kill-buffer (vm-net-session-buffer session))))))))

;;; What the blocking POP session's own tests used to cover (emacs-vm/vm#822)

(defun vm-pop-net-test--fetched-text (mock crash)
  "Fetch from MOCK into CRASH and answer what was written there."
  (should (equal (vm-pop-net-test--get-mail mock crash) 1))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally crash)
    (buffer-string)))

(ert-deftest vm-pop-net-test-a-doubled-leading-dot-comes-back-single ()
  "A body line that begins with a dot arrives as it was sent.

The server doubles it, and the reader has to undo that; without it the message
would end early at its own text, which is the classic POP3 mistake in both
directions."
  (vm-pop-net-test--in-a-folder-with-spool
      (mock :messages (list (concat "From: alice@example.com\n"
                                    "Subject: dotted\n\n"
                                    "before\n.hidden line\nafter\n")))
    (let ((text (vm-pop-net-test--fetched-text mock (nth 2 (car vm-spool-files)))))
      (should (string-match-p "^\\.hidden line$" text))
      (should-not (string-match-p "^\\.\\.hidden line$" text))
      ;; and nothing after it was lost to an early end
      (should (string-match-p "^after$" text)))))

(ert-deftest vm-pop-net-test-a-wrong-octet-count-does-not-truncate ()
  "A server that reports the wrong size still gets its message stored whole.

A POP body ends at a dot on a line of its own; the octet count in LIST is for
the progress report and the size threshold, so getting it wrong must not
truncate anything."
  (vm-pop-net-test--in-a-folder-with-spool
      (mock :lie-about-size t :messages (list vm-pop-net-test--alice))
    (let ((text (vm-pop-net-test--fetched-text mock (nth 2 (car vm-spool-files)))))
      (should (string-match-p "badgers" text))
      (should (string-match-p "The first body" text)))))

(ert-deftest vm-pop-net-test-a-server-with-no-uidl-says-why-it-fetches-nothing ()
  "A server with no UIDL fetches nothing, and says why.

UIDL is what tells one message from another between sessions, so without it
nothing here can say which messages have been fetched before.  The blocking
implementation kept count by deleting each message as it took it, which is
not something to start from a process filter and is not what a reader who
leaves mail on the server asked for.  So this refuses and names the reason: a
maildrop that quietly never arrives is worse than one that says why.

A capability the blocking implementation had and this does not; see
emacs-vm/vm#822."
  (vm-pop-net-test--in-a-folder-with-spool
      (mock :no-uidl t :messages (list vm-pop-net-test--alice
                                       vm-pop-net-test--bob))
    (let* ((crash (nth 2 (car vm-spool-files)))
           (result (vm-pop-net-test--get-mail mock crash)))
      (should (vm-net-error-p result))
      (should (string-match-p "no UIDL" (error-message-string result)))
      ;; nothing was written, rather than mail arriving twice later
      (should-not (file-exists-p crash)))))

(ert-deftest vm-pop-net-test-the-timeout-is-only-a-backstop ()
  "A server that answers normally is not cut off by the timeout.
The point of a timeout is a bound on waiting, not a bound on the session."
  (vm-pop-net-test--in-a-folder-with-spool
      (mock :messages (list vm-pop-net-test--alice vm-pop-net-test--bob))
    (let ((crash (nth 2 (car vm-spool-files)))
          (vm-pop-server-timeout 2))
      ;; two messages fetched inside a two-second timeout, and the session
      ;; finishes rather than being ended by it
      (should (equal (vm-pop-net-test--get-mail mock crash) 2))
      (should-not (vm-pop-net-busy-p)))))

(provide 'vm-pop-net-test)

;;; vm-pop-net-test.el ends here
