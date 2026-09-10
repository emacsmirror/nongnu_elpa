;;; vm-net-test.el --- Tests for vm-net.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The driver that runs a network session without waiting for it: a generator
;; that yields when it wants input, a process filter that feeds it.
;;
;; The sessions here run against a real process -- a small server on a local
;; port -- so the filter is the one that runs and the timing is real.  What the
;; tests wait for is the session to finish, which is the test blocking, not VM.

;;; Code:

(require 'vm-test-init)
(require 'vm-net)
(require 'generator)

(defvar vm-net-test--server nil
  "The listening process of the server a test is talking to.")

(defun vm-net-test--start-server (script)
  "Serve SCRIPT on a local port and return (PROCESS . PORT).
SCRIPT is called with the connection and the line that arrived, and answers
by writing to it.  A server rather than a pipe: VM talks to a socket, and a
socket is what shows a filter being called with whatever chunks arrive."
  (let* ((server (make-network-process
                  :name "vm-net-test-server" :server t :host 'local
                  :service t :family 'ipv4 :noquery t
                  :filter (lambda (connection string)
                            (funcall script connection string))))
         (port (process-contact server :service)))
    (cons server port)))

(defmacro vm-net-test--with-server (spec &rest body)
  "Run BODY with a server answering according to SCRIPT.
SPEC is (PORT-VAR SCRIPT).  Everything the test opened is killed after."
  (declare (indent 1) (debug t))
  `(let* ((pair (vm-net-test--start-server ,(cadr spec)))
          (vm-net-test--server (car pair))
          (,(car spec) (cdr pair))
          (before (buffer-list)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p vm-net-test--server)
         (delete-process vm-net-test--server))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun vm-net-test--connect (port)
  "A connection to PORT, with a buffer of its own and no filter yet."
  (make-network-process :name "vm-net-test" :host 'local :service port
                        :buffer (generate-new-buffer " *vm-net-test*")
                        :noquery t :filter nil))

(defun vm-net-test--wait (session &optional seconds)
  "Wait for SESSION to finish, up to SECONDS.
The test waits; VM does not.  Everything this drives happens in the filter,
which is what a real Emacs would be running between keystrokes."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (vm-net-session-live-p session) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (vm-net-session-state session)))

(defun vm-net-test--echo-once (connection string)
  "Answer STRING with a line of its own."
  (process-send-string connection (format "you said %s" string)))

;;; A session that reads one line

(iter-defun vm-net-test--read-line ()
  "Wait until there is a line in the buffer, and answer with it.
Searching from `point-min' each time round: the filter leaves point at the
end of what it inserted, and a search from there finds nothing.  VM\\='s own
parsers keep a read point of their own for the same reason."
  (while (not (save-excursion (goto-char (point-min))
                              (re-search-forward "\n" nil t)))
    (iter-yield (vm-net-request-growth)))
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (line-end-position))))

(ert-deftest vm-net-test-a-session-answers-when-the-line-arrives ()
  "The generator asks for input, the filter gives it, and the session ends
with what the generator returned.  Nothing waited: the whole of it happened
in the filter."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (finished nil)
           (session (vm-net-session
                     :process process :name "test"
                     :finished (lambda (s) (setq finished s)))))
      (vm-net-start session (vm-net-test--read-line))
      (process-send-string process "hello\n")
      (should (eq (vm-net-test--wait session) 'done))
      (should (equal (vm-net-session-value session) "you said hello"))
      (should (eq finished session)))))

(ert-deftest vm-net-test-a-session-waits-rather-than-spinning ()
  "Before the answer arrives the session is running and holds the request it
is waiting on, and the request says no."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test")))
      (vm-net-start session (vm-net-test--read-line))
      (should (eq (vm-net-session-state session) 'running))
      (should (functionp (vm-net-session-request session)))
      (should-not (with-current-buffer (vm-net-session-buffer session)
                    (funcall (vm-net-session-request session))))
      (vm-net-abandon session))))

;;; What the generator is handed

(iter-defun vm-net-test--collect-two ()
  "Ask twice, and answer with what the driver handed back both times."
  (list (iter-yield nil) (iter-yield nil)))

(ert-deftest vm-net-test-a-generator-with-no-request-resumes-on-anything ()
  "A generator that yields nil is asking to be resumed as soon as anything
happens, which is what a command that only needs the connection to be alive
wants."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test")))
      (vm-net-start session (vm-net-test--collect-two))
      ;; two arrivals, not one chunk carrying both: the point is that each
      ;; one resumes the generator
      (process-send-string process "one\n")
      (accept-process-output nil 0.2)
      (process-send-string process "two\n")
      (should (eq (vm-net-test--wait session) 'done))
      (should (equal (vm-net-session-value session) '(nil nil))))))

;;; Errors and timeouts

(iter-defun vm-net-test--fails ()
  (iter-yield (vm-net-request-growth))
  (error "the protocol said no"))

(ert-deftest vm-net-test-an-error-ends-the-session-and-is-kept ()
  "An error out of the generator ends the session and is kept on it rather
than signalled: the stack that started the session has gone, so there is
nobody left to signal to."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test")))
      (vm-net-start session (vm-net-test--fails))
      (process-send-string process "anything\n")
      (should (eq (vm-net-test--wait session) 'failed))
      (should (string-match-p "the protocol said no"
                              (error-message-string
                               (vm-net-session-error session)))))))

(ert-deftest vm-net-test-a-silent-server-times-the-session-out ()
  "A server that accepts the connection and then says nothing ends the
session when the timeout runs out.  Waiting for ever is what VM does now
when a POP or IMAP server goes quiet."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :timeout 0.2)))
      (vm-net-start session (vm-net-test--read-line))
      (should (eq (vm-net-test--wait session 3) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session)))))))

;;; Cleanup, which is the reason the generator is closed rather than dropped

(defvar vm-net-test--cleanup nil)

(iter-defun vm-net-test--with-cleanup ()
  (unwind-protect
      (progn (iter-yield (vm-net-request-growth)) 'finished)
    (push 'cleanup vm-net-test--cleanup)))

(ert-deftest vm-net-test-abandoning-a-session-runs-its-cleanup ()
  "`vm-net-abandon' closes the generator, so its `unwind-protect' runs.

Dropping the generator instead runs nothing -- the spike in
dev/docs/design/async-imap.org measures that -- and what those forms do is
put the folder and the server back in step: a QUIT, an EXPUNGE, flags
written back."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((vm-net-test--cleanup nil)
           (process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test")))
      (vm-net-start session (vm-net-test--with-cleanup))
      (should (eq (vm-net-session-state session) 'running))
      (vm-net-abandon session)
      (should (eq (vm-net-session-state session) 'failed))
      (should (equal vm-net-test--cleanup '(cleanup))))))

(ert-deftest vm-net-test-a-timeout-runs-the-cleanup-too ()
  "The same when the timeout ends it: whatever the session undertook to do
at the end still happens."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((vm-net-test--cleanup nil)
           (process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :timeout 0.2)))
      (vm-net-start session (vm-net-test--with-cleanup))
      (should (eq (vm-net-test--wait session 3) 'failed))
      (should (equal vm-net-test--cleanup '(cleanup))))))

;;; The requests the two protocols make

(ert-deftest vm-net-test-a-growth-request-is-about-what-arrived-since ()
  "`vm-net-request-growth' is what the IMAP parser asks with: it has read
everything there is and wants more than that."
  (with-temp-buffer
    (insert "* OK")
    (let ((request (vm-net-request-growth)))
      (should-not (funcall request))
      (insert " more")
      (should (funcall request)))))

(defvar vm-net-test--scanned 0
  "Characters offered to `re-search-forward' while a request was polled.")

(defun vm-net-test--count-scan (original &rest args)
  (setq vm-net-test--scanned (+ vm-net-test--scanned (- (point-max) (point))))
  (apply original args))

(ert-deftest vm-net-test-a-match-request-searches-only-what-arrived ()
  "The filter polls the pending request once per chunk, so a request that
searches the whole response each time is quadratic in its size: reading a
2 MB message took seconds and a 20 MB one minutes, with Emacs blocked
throughout.

Counted rather than timed: the characters `re-search-forward' is given must
stay within a small multiple of the response, not a multiple of the number
of chunks it arrived in."
  (let* ((line "line of the body, padded out to make the line reasonably long\n")
         (body (mapconcat #'identity (make-list 4000 line) ""))
         (response (concat (replace-regexp-in-string "\n" "\r\n" body) ".\r\n"))
         (chunk 1400)
         (vm-net-test--scanned 0)
         (sent 0))
    (advice-add 're-search-forward :around #'vm-net-test--count-scan)
    (unwind-protect
        (with-temp-buffer
          (let ((request (vm-net-request-match "^\\.\r\n" (point-max))))
            (while (< sent (length response))
              (let ((end (min (length response) (+ sent chunk))))
                (goto-char (point-max))
                (insert (substring response sent end))
                (setq sent end))
              (funcall request))
            (should (funcall request))))
      (advice-remove 're-search-forward #'vm-net-test--count-scan))
    (should (> (/ (length response) chunk) 100)) ; enough chunks to tell
    (should (< vm-net-test--scanned (* 4 (length response))))))

(ert-deftest vm-net-test-a-match-request-sees-a-terminator-split-in-two ()
  "The terminator arriving in two pieces is still found, which is what the
request may not skip over when it searches only the new text."
  (with-temp-buffer
    (let ((request (vm-net-request-match "^\\.\r\n" (point-max))))
      (insert "+OK\r\nbody\r\n.\r")
      (should-not (funcall request))
      (insert "\n")
      (should (funcall request)))))

(ert-deftest vm-net-test-a-match-request-is-about-a-terminator ()
  "`vm-net-request-match' is what the POP reads ask with: each waits for the
end of its response, and searches from where the reader is."
  (with-temp-buffer
    (insert "+OK\r\n1 UID001\r\n")
    (goto-char (point-min))
    (let ((request (vm-net-request-match "^\\.\r\n")))
      (should-not (funcall request))
      (goto-char (point-max))
      (insert ".\r\n")
      (should (funcall request)))))


;;; A connection that has to be tunnelled

(ert-deftest vm-net-test-a-free-port-is-one-nothing-answers-on ()
  "`vm-net-free-port' asks the operating system for a port rather than
trying one after another until a connection fails -- which is what the
blocking path does, and every one of those attempts waits."
  (let ((port (vm-net-free-port)))
    (should (integerp port))
    (should (> port 0))
    (should-not (vm-net-listening-p port))))

(ert-deftest vm-net-test-a-session-waits-for-its-tunnel ()
  "A session with no process yet does not run until one is attached: the
program it goes through has to be listening before there is anything to
connect to."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((finished nil)
           (session (vm-net-session :name "test"
                                    :finished (lambda (s) (setq finished s)))))
      (setf (vm-net-session-buffer session)
            (generate-new-buffer " *vm-net-test*"))
      (vm-net-start session (vm-net-test--read-line))
      ;; started, but nothing has run: there is nothing to read from
      (should (eq (vm-net-session-state session) 'running))
      (should-not (vm-net-session-request session))
      (should-not finished)
      ;; now it has a connection
      (let ((process (make-network-process
                      :name "vm-net-test" :host 'local :service port
                      :buffer (vm-net-session-buffer session) :noquery t)))
        (vm-net-attach session process)
        (should (vm-net-session-request session))
        (process-send-string process "hello\n")
        (should (eq (vm-net-test--wait session) 'done))
        (should (equal (vm-net-session-value session) "you said hello"))))))

(ert-deftest vm-net-test-a-tunnel-that-never-comes-up-fails-the-session ()
  "The session is failed and its caller told, rather than left waiting for a
port that will never answer."
  (let* ((finished nil)
         (session (vm-net-session :name "test"
                                  :finished (lambda (s) (setq finished s))))
         (port (vm-net-free-port))
         (ready 'not-called))
    (setf (vm-net-session-buffer session) (generate-new-buffer " *vm-net-test*"))
    (vm-net-start session (vm-net-test--read-line))
    ;; a program that does not listen on anything
    (vm-net-tunnel session "sleep" (list "30") port 0.3
                   (lambda (tunnel) (setq ready tunnel)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (eq ready 'not-called) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (null ready))
    (should (eq (vm-net-session-state session) 'failed))
    (should (eq finished session))
    (should (string-match-p "did not start listening"
                            (error-message-string (vm-net-session-error session))))
    (kill-buffer (vm-net-session-buffer session))))

(ert-deftest vm-net-test-a-tunnel-is-killed-with-the-session ()
  "The program is the session's, and goes when it goes: a tunnel left behind
holds a port open and, for ssh, a connection to the far end."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((session (vm-net-session :name "test"))
           (tunnel nil))
      (setf (vm-net-session-buffer session)
            (generate-new-buffer " *vm-net-test*"))
      (vm-net-start session (vm-net-test--read-line))
      ;; the port is already listening, so the tunnel is "ready" at once
      (setq tunnel (vm-net-tunnel session "sleep" (list "30") port 5
                                  (lambda (_) nil)))
      (should (process-live-p tunnel))
      (vm-net-abandon session)
      (should-not (process-live-p tunnel))
      (kill-buffer (vm-net-session-buffer session)))))


(ert-deftest vm-net-test-the-tunnel-probe-does-not-wait ()
  "Asking whether a port is listening starts a connection and reads its
answer on the next ask.  A blocking connect is fast to a port on this machine
but it is still a wait, and this runs from a timer while somebody is typing.

So the first ask is always nil, whatever is there, and nothing is left
connected once it has answered."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (clrhash vm-net--probes)
    ;; something is listening, and the first ask still does not know
    (should-not (vm-net-listening-p port))
    (let ((deadline (+ (float-time) 5))
          (answer nil))
      (while (and (not answer) (< (float-time) deadline))
        (accept-process-output nil 0.02)
        (setq answer (vm-net-listening-p port)))
      (should answer))
    ;; the probe that answered was closed, and this port has none outstanding
    (should-not (gethash port vm-net--probes))))

(ert-deftest vm-net-test-a-port-with-nothing-there-answers-no ()
  "A port nothing is listening on answers nil however often it is asked, and
leaves no probe behind."
  (let ((port (vm-net-free-port)))
    (clrhash vm-net--probes)
    (dotimes (_ 5)
      (should-not (vm-net-listening-p port))
      (accept-process-output nil 0.05))
    ;; one probe for this port at a time, whatever the answer: they do not
    ;; pile up, and reading the last one closes it
    (should (processp (gethash port vm-net--probes)))
    (should-not (vm-net-listening-p port))
    (should-not (gethash port vm-net--probes))))


(ert-deftest vm-net-test-a-tunnel-leaves-no-probe-behind ()
  "The probe waiting on the port goes when the wait is over.

A connection left open to a port keeps whatever is on the other end of it
busy, and a port on this machine is handed out again: a probe left over from
one wait was found holding a later server's only connection, and the fetch on
it timed out."
  (let* ((session (vm-net-session :name "test"))
         (port (vm-net-free-port))
         (ready 'not-called))
    (setf (vm-net-session-buffer session) (generate-new-buffer " *vm-net-test*"))
    (vm-net-start session (vm-net-test--read-line))
    (vm-net-tunnel session "sleep" (list "30") port 0.3
                   (lambda (tunnel) (setq ready tunnel)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (eq ready 'not-called) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (null ready))
    (should-not (gethash port vm-net--probes))
    (kill-buffer (vm-net-session-buffer session))))

;;; A connection that is a program's pipes

(ert-deftest vm-net-test-a-pipe-feeds-a-session ()
  "A program's standard output reaches the session's generator.
How stunnel is talked to: it is given no port to listen on, so it relays its
own standard input and output and the program is the connection."
  (let* ((session (vm-net-session :name "test" :timeout 5))
         (buffer (generate-new-buffer " *vm-net-test-pipe*"))
         (process (vm-net-pipe session "vm-net-test-pipe" buffer
                               "sh" (list "-c" "printf '* OK ready\\n'"))))
    (setf (vm-net-session-buffer session) buffer)
    (setf (vm-net-session-process session) process)
    (vm-net-start session (vm-net-test--read-line))
    (should (eq (vm-net-test--wait session) 'done))
    (should (equal (vm-net-session-value session) "* OK ready"))
    (kill-buffer buffer)))

(ert-deftest vm-net-test-a-pipe-keeps-diagnostics-out-of-the-read-buffer ()
  "What the program says on standard error is not read as protocol.
stunnel writes its log there, and in the process buffer VM's parser would try
to make IMAP of it.  The buffer it goes to is killed with the session."
  (let* ((session (vm-net-session :name "test" :timeout 5))
         (buffer (generate-new-buffer " *vm-net-test-pipe*"))
         (process (vm-net-pipe
                   session "vm-net-test-pipe" buffer
                   "sh" (list "-c" "printf 'complaining\\n' >&2; printf '* OK ready\\n'")))
         (errors (get-buffer " *vm-net-test-pipe errors*")))
    (setf (vm-net-session-buffer session) buffer)
    (setf (vm-net-session-process session) process)
    (vm-net-start session (vm-net-test--read-line))
    (should (eq (vm-net-test--wait session) 'done))
    (should (equal (vm-net-session-value session) "* OK ready"))
    (with-current-buffer buffer
      (should-not (string-match-p "complaining" (buffer-string))))
    (should-not (buffer-live-p errors))
    (kill-buffer buffer)))

(ert-deftest vm-net-test-cleanups-run-even-when-the-caller-sets-finished ()
  "A cleanup added by the connection survives the caller assigning `finished'.

Both used to be the same slot, so a caller that set `finished' after opening
the session -- which is what every one of them does -- threw away the tunnel's
kill and the pipe's buffer, and stunnel or ssh stayed running."
  (let* ((session (vm-net-session :name "test"))
         (cleaned nil)
         (finished nil))
    (setf (vm-net-session-buffer session) (generate-new-buffer " *vm-net-test*"))
    (vm-net-at-end session (lambda () (push 'first cleaned)))
    (vm-net-at-end session (lambda () (push 'second cleaned)))
    (setf (vm-net-session-finished session) (lambda (_) (setq finished t)))
    (vm-net-start session (vm-net-test--read-line))
    (vm-net-abandon session)
    ;; newest first, and the caller still hears about the end
    (should (equal cleaned '(first second)))
    (should finished)
    (kill-buffer (vm-net-session-buffer session))))

(ert-deftest vm-net-test-a-cleanup-that-fails-does-not-cost-the-others ()
  "One cleanup signalling still leaves the rest run and `finished' called."
  (let* ((session (vm-net-session :name "test"))
         (cleaned nil)
         (finished nil))
    (setf (vm-net-session-buffer session) (generate-new-buffer " *vm-net-test*"))
    (vm-net-at-end session (lambda () (push 'ran cleaned)))
    (vm-net-at-end session (lambda () (error "Cleanup went wrong")))
    (setf (vm-net-session-finished session) (lambda (_) (setq finished t)))
    (vm-net-start session (vm-net-test--read-line))
    (let ((vm-verbosity 0))
      (vm-net-abandon session))
    (should (equal cleaned '(ran)))
    (should finished)
    (kill-buffer (vm-net-session-buffer session))))

(iter-defun vm-net-test--answer-arrives-while-running ()
  "Read a line, then read what turned up while this was running.

The second line is put in the buffer by the generator itself, which is what a
chunk arriving mid-resume amounts to: the filter has already polled, and it
polled with the request this one is about to replace."
  (while (not (save-excursion (goto-char (point-min))
                              (re-search-forward "\n" nil t)))
    (iter-yield (vm-net-request-growth)))
  (save-excursion (goto-char (point-max)) (insert "second\n"))
  (iter-yield (vm-net-request-match "second" (point-min)))
  'both)

(ert-deftest vm-net-test-what-arrived-while-running-is-not-waited-for ()
  "A request that is already satisfied when the generator yields it resumes
at once, rather than waiting for a chunk that has been and gone.

Found as a POP fetch that stopped with its whole answer in the buffer -- the
UIDL and LIST responses complete, the session waiting -- and finished the
moment anything polled it."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test" :timeout 5)))
      (vm-net-start session (vm-net-test--answer-arrives-while-running))
      (process-send-string process "hello\n")
      (should (eq (vm-net-test--wait session 2) 'done))
      (should (eq (vm-net-session-value session) 'both)))))

(ert-deftest vm-net-test-one-watchdog-watches-every-deadline ()
  "A session waiting on a read is failed by the watchdog, not by a timer of
its own.

emacs-vm/vm#717: a session was seen 8.8 seconds into a three-second timeout,
still running, with its own timer sitting unrun in `timer-list'.  A lost timer
took away the only thing that would ever have reported the stall.  One timer
serves every waiting session and is armed only while one is waiting, so what a
lost tick costs is a quarter of a second."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :timeout 0.3)))
      (vm-net-start session (vm-net-test--read-line))
      ;; waiting, watched, and one timer for it
      (should (memq session (vm-net--sessions)))
      (should (vm-net--watchdog-timers))
      (should (eq (vm-net-test--wait session 5) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session))))
      ;; and nothing left running once nothing is waiting
      (should-not (memq session (vm-net--sessions)))
      (vm-net--watch)
      (should-not (vm-net--watchdog-timers)))))

(ert-deftest vm-net-test-a-poll-nobody-made-is-made-by-the-watchdog ()
  "A session whose answer arrived without the filter asking about it is not
left waiting for ever.

The filter is the only other thing that polls, and an answer it does not ask
about afterwards is an answer nobody looks at.  Seen in the suite: a POP
session with its login answered sat until the test gave up on it, and one poll
by hand finished it.  The watchdog polls as well as timing out, so lateness
costs a quarter of a second rather than the session."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test" :timeout 30)))
      (vm-net-start session (vm-net-test--read-line))
      ;; the answer arrives with nobody watching: the filter is taken away, so
      ;; nothing polls when it lands
      (set-process-filter process
                          (lambda (proc string)
                            (with-current-buffer (process-buffer proc)
                              (goto-char (point-max))
                              (insert string))))
      (process-send-string process "hello\n")
      (should (eq (vm-net-test--wait session 5) 'done))
      (should (equal (vm-net-session-value session) "you said hello")))))

(ert-deftest vm-net-test-a-session-times-out-with-its-timer-taken-away ()
  "Even with no timer to fire, a stalled read is reported.
The failure in emacs-vm/vm#717 was exactly this: whatever was to fire did not."
  (vm-net-test--with-server (port (lambda (&rest _) nil))
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :timeout 0.2)))
      (vm-net-start session (vm-net-test--read-line))
      ;; the watchdog is taken away as a lost timer would take it, and the
      ;; deadline is still there to be noticed the next time anything looks
      (mapc #'cancel-timer (vm-net--watchdog-timers))
      (let ((deadline (+ (float-time) 2)))
        (while (and (vm-net-session-live-p session) (< (float-time) deadline))
          (accept-process-output nil 0.05)
          (vm-net--watch)))
      (should (eq (vm-net-session-state session) 'failed))
      (should (string-match-p "timed out"
                              (error-message-string
                               (vm-net-session-error session)))))))

(ert-deftest vm-net-test-a-timeout-is-an-error-the-caller-can-tell ()
  "The condition a timed-out session reports is a defined error.

A caller tells an answer from an error by asking whether the first element of
what it was given has been through `define-error'.  `vm-net-timeout' had not,
so a timed-out session's error read as an answer: a POP fetch that timed out
was handed to the crash-box writer as a list of messages, which died in the
attempt -- and the callback that would have reported the timeout died with it,
leaving the caller waiting for an answer it had already been given.

\"Session POP fetch (live nil), process closed\" is what that looks like from
outside: over, and never reported."
  (should (get 'vm-net-timeout 'error-conditions))
  (should (memq 'error (get 'vm-net-timeout 'error-conditions)))
  (should (vm-net-error-p (list 'vm-net-timeout "POP server timed out")))
  (should (vm-net-error-p (list 'vm-net-connection-lost "gone")))
  (should (vm-net-error-p (list 'vm-net-tunnel-failed "no")))
  ;; and an answer is not mistaken for one
  (should-not (vm-net-error-p (list (cons "uid1" "From: a\n\nbody\n"))))
  (should-not (vm-net-error-p nil))
  (should-not (vm-net-error-p 3))
  (should-not (vm-net-error-p (list 'not-an-error-symbol "text"))))

(ert-deftest vm-net-test-the-driver-says-things-without-stopping ()
  "Nothing the driver says holds Emacs still.

`vm-warn' keeps its message on screen with `sit-for', and `vm-inform' does
the same for `vm-verbal-time'.  The driver speaks from process filters,
sentinels and timers, where a pause is Emacs stopped -- two seconds per
warning, and a folder whose server refused a flag per message stopped for two
seconds each time: four seconds to save two messages' flags, where the work
itself takes a tenth of one.  A `sit-for' there also runs timers and other
filters, which is the re-entry the driver's own guards refuse."
  (let ((paused nil))
    (cl-letf (((symbol-function 'vm-pause)
               (lambda (seconds) (push seconds paused))))
      (let ((vm-verbosity 5) (vm-verbal-time 2) (vm-current-warning nil))
        (vm-net-warn 0 "something went wrong")
        (vm-net-inform 5 "getting on with it")
        (should-not (delq 0 (delq nil (copy-sequence paused))))
        ;; and the blocking path still pauses, which is what it is for
        (setq paused nil)
        (setq vm-current-warning nil)
        (vm-warn 0 2 "something else went wrong")
        (should (member 2 paused))))))

(ert-deftest vm-net-test-there-is-never-more-than-one-watchdog ()
  "Arming the watchdog again while one is running adds nothing.

The timer used to be remembered in a variable, and whatever reset that
variable -- the test harness restores VM's variables between tests -- left the
timer running with nothing pointing at it, so the next session armed another.
Thirty were found in `timer-list' at once, and the cancel could reach none of
them.  It is looked up by its function now, so there is one or none."
  (mapc #'cancel-timer (vm-net--watchdog-timers))
  (unwind-protect
      (let ((session (vm-net-session :name "test" :timeout 5)))
        (setf (vm-net-session-state session) 'running)
        (vm-net--arm-timeout session)
        (should (equal (length (vm-net--watchdog-timers)) 1))
        ;; again, as the next read of the next session would
        (vm-net--arm-timeout session)
        (vm-net--arm-timeout session)
        (should (equal (length (vm-net--watchdog-timers)) 1))
        ;; and with no session left to watch, the watch stops
        (setf (vm-net-session-state session) 'done)
        (vm-net--watch)
        (should-not (vm-net--watchdog-timers)))
    (mapc #'cancel-timer (vm-net--watchdog-timers))))

(ert-deftest vm-net-test-the-watchdog-leaves-a-running-generator-alone ()
  "A session whose generator is running is not closed under it.

`iter-close' on a running generator signals, and by then the session has been
marked failed and its caller told; the resume it interrupted then finishes the
session a second time.  So a tick that lands mid-generator says so and waits
for the next one."
  (let ((session (vm-net-session :name "test" :timeout 1))
        (said nil))
    (setf (vm-net-session-state session) 'running)
    (setf (vm-net-session-resuming session) t)
    (setf (vm-net-session-deadline session) (- (float-time) 10))
    (cl-letf (((symbol-function 'vm-net--sessions) (lambda () (list session)))
              ((symbol-function 'vm-net-warn)
               (lambda (_level &rest args) (push (apply #'format args) said)))
              ((symbol-function 'vm-net-poll)
               (lambda (&rest _) (error "polled a running generator"))))
      (vm-net--watch)
      (should (vm-net-session-live-p session))
      (should (equal (length said) 1))
      (should (string-match-p "still working" (car said)))
      ;; and it is said once, not every quarter second
      (vm-net--watch)
      (should (equal (length said) 1))
      ;; once the generator has stopped, the tick times it out as before
      (setf (vm-net-session-resuming session) nil)
      (cl-letf (((symbol-function 'vm-net-poll) #'ignore))
        (vm-net--watch))
      (should (eq (vm-net-session-state session) 'failed))
      (should (eq (car (vm-net-session-error session)) 'vm-net-timeout)))))

(ert-deftest vm-net-test-a-timer-that-signalled-is-not-a-watchdog ()
  "A watchdog timer that has signalled is replaced, not counted.

Emacs marks a timer as triggered before running it and clears that only if the
function returns; one that signals is left triggered, never rescheduled, and
left in `timer-list' all the same.  It fires nothing from then on.  Counting it
as the watchdog left every session afterwards with nothing to poll it -- an
answer in the buffer that nobody would look at, which is a folder hung for
good, and what emacs-vm/vm#717 saw as a timer in `timer-list' unrun."
  (mapc #'cancel-timer (vm-net--watchdog-timers))
  (unwind-protect
      (let ((session (vm-net-session :name "test" :timeout 5)))
        (setf (vm-net-session-state session) 'running)
        (vm-net--arm-timeout session)
        (let ((timer (car (vm-net--watchdog-timers))))
          (should timer)
          ;; as Emacs leaves one whose function signalled
          (setf (timer--triggered timer) t)
          (should-not (vm-net--watchdog-timers))
          ;; it is gone from the list rather than sitting there for ever
          (should-not (memq timer timer-list))
          ;; and the next arm makes a live one
          (vm-net--arm-timeout session)
          (should (equal (length (vm-net--watchdog-timers)) 1))))
    (mapc #'cancel-timer (vm-net--watchdog-timers))))

(ert-deftest vm-net-test-one-bad-session-does-not-stop-the-watch ()
  "An error watching one session does not take the watchdog or the rest down.

An error out of a timer is what leaves it triggered and unrescheduled, so a
fault in one session would have cost every session afterwards its safety net."
  (let* ((bad (vm-net-session :name "bad" :timeout 5))
         (good (vm-net-session :name "good" :timeout 5))
         (watched nil)
         (said nil))
    (setf (vm-net-session-state bad) 'running)
    (setf (vm-net-session-state good) 'running)
    (cl-letf (((symbol-function 'vm-net--sessions) (lambda () (list bad good)))
              ((symbol-function 'vm-net-warn)
               (lambda (_level &rest args) (push (apply #'format args) said)))
              ((symbol-function 'vm-net-poll)
               (lambda (session)
                 (if (eq session bad)
                     (error "this session is broken")
                   (push session watched)))))
      (vm-net--watch)
      ;; the good one was still looked at, and the fault was reported
      (should (equal watched (list good)))
      (should (equal (length said) 1))
      (should (string-match-p "watching it failed" (car said))))))

;;; the resume loop hands Emacs back (issue #473)

(iter-defun vm-net-test--many-steps (count)
  "Ask COUNT times for something that is always there, doing a little work.
What a fetch of thousands of messages looks like to the driver: the answer to
the next read is already in the buffer, so nothing waits and the loop runs on
until the work is done."
  (let ((i 0))
    (while (< i count)
      (iter-yield (lambda () t))
      ;; enough work that the loop cannot finish inside one slice
      (let ((n 0)) (while (< n 400) (format "%d" n) (setq n (1+ n))))
      (setq i (1+ i)))
    i))

(ert-deftest vm-net-test-a-long-run-of-work-is-cut-into-slices ()
  "REGRESSION: one filter call does not hold Emacs for as long as it likes.
Issue #473.  The answer to the next read is often already in the buffer -- a
server sending six thousand responses to one FETCH fills it faster than they
are parsed -- and the resume loop ran to the end of them without returning.
Measured on a mailbox of 6438: 0.67 seconds of Emacs stopped in one filter
call.  It now hands back after `vm-net--slice' and asks to be called again."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :finished #'ignore))
           (start (float-time))
           (first-slice nil))
      (vm-net-start session (vm-net-test--many-steps 4000))
      (setq first-slice (- (float-time) start))
      ;; the first run gave Emacs its turn back rather than finishing
      (should (vm-net-session-live-p session))
      (should (< first-slice (* 4 vm-net--slice)))
      ;; and the work still finishes, from the timer it left behind
      (should (eq (vm-net-test--wait session 30) 'done))
      (should (equal (vm-net-session-value session) 4000)))))

(ert-deftest vm-net-test-work-that-fits-in-a-slice-is-not-cut-up ()
  "A short run finishes in the one call, with no timer and no waiting."
  (vm-net-test--with-server (port #'vm-net-test--echo-once)
    (let* ((process (vm-net-test--connect port))
           (session (vm-net-session :process process :name "test"
                                    :finished #'ignore)))
      (vm-net-start session (vm-net-test--many-steps 5))
      (should-not (vm-net-session-live-p session))
      (should (equal (vm-net-session-value session) 5)))))

(ert-deftest vm-net-test-a-request-of-nothing-is-satisfied-at-once ()
  "`vm-net-request-now' means \"take me back as soon as you can\".
What a generator yields when it has more to do and none of it is waiting, so
that a long parse is interruptible: the driver takes it straight back, and
the slice is then free to hand Emacs a turn first."
  (should (funcall (vm-net-request-now)))
  (with-temp-buffer
    (should (funcall (vm-net-request-now)))))

(provide 'vm-net-test)

;;; vm-net-test.el ends here
