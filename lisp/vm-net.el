;;; vm-net.el --- Driving a network session without blocking Emacs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 The VM Developers
;;
;; This file is part of VM.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; VM's IMAP and POP code reads by waiting: a parser wants more bytes than the
;; process buffer holds, so it calls `accept-process-output' and Emacs stops
;; until the server answers.  This file is the other way round.  A session is a
;; generator that yields when it wants input, a process filter puts what
;; arrived into the buffer and asks the generator to carry on, and Emacs is
;; never inside a wait.
;;
;; The pieces:
;;
;;   `vm-net-session'      what a session is: its process, the generator
;;                         running it, what that generator last asked for, and
;;                         where its answer goes.
;;   `vm-net-start'        install the filter and set the generator going.
;;   `vm-net-poll'         ask the generator to carry on if what it wanted has
;;                         arrived.  The filter calls this; so does the
;;                         timeout.
;;   `vm-net-abandon'      stop a session that will not finish, running the
;;                         generator's cleanup forms.
;;
;; A generator yields a *request*, which says what has to be true before it is
;; worth resuming:
;;
;;   nil                   resume as soon as anything arrives
;;   FUNCTION              resume when calling FUNCTION in the process buffer
;;                         answers non-nil
;;
;; The request is what lets one driver serve both protocols: IMAP's parser
;; wants "more than there was", POP's reads want "the terminator is in the
;; buffer", and neither has to know how the other waits.  See
;; dev/docs/design/async-imap.org.
;;
;; The generator is closed when a session ends, however it ends.  That is not
;; tidiness: a generator that is dropped rather than closed never runs its
;; `unwind-protect' forms, so a session abandoned half way would leave the
;; server's view and VM's disagreeing, silently.

;;; Code:

(require 'cl-lib)
(require 'generator)

(eval-when-compile (require 'vm-misc))
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-inform "vm-misc" (level &rest args))
(declare-function vm-warn "vm-misc" (level seconds &rest args))

(cl-defstruct (vm-net-session (:constructor vm-net-session--make)
			      (:copier nil))
  "A network session VM is running without waiting for it."
  process				; the network process
  buffer				; its process buffer
  name					; for messages: "imap", "pop"
  timeout				; seconds to wait for input, or nil
  deadline				; when the read it is waiting on runs out
  resuming				; whether its generator is running now
  iterator				; the generator doing the work
  request				; what it last asked to wait for
  (state 'new)				; new, running, done or failed
  value					; what the generator returned
  error					; the error that stopped it, if any
  finished				; called with the session when it ends
  cleanups				; what to undo when it ends, newest first
  said-stuck)				; whether the watchdog has complained

(defface vm-net-session-face
  '((t :inherit mode-line-emphasis))
  "Face for what a folder is doing with its server, in the mode line.

Inherits `mode-line-emphasis', which every theme renders differently from
the rest of the mode line.  For something louder, give it a background:

    (set-face-attribute \\='vm-net-session-face nil :background \"yellow2\")"
  :group 'vm-faces)

(defconst vm-net-session-words
  '(("fetch" . "fetching")
    ("save" . "saving")
    ("flags" . "sending changes")
    ("checkmail" . "checking")
    ("check" . "checking")
    ("uids" . "checking")
    ("synchronize" . "syncing")
    ("expunge" . "deleting")
    ("maildrop expunge" . "deleting")
    ("folders" . "listing folders")
    ("names" . "listing folders")
    ("FCC" . "filing")
    ("CREATE" . "making a folder")
    ("DELETE" . "deleting a folder")
    ("RENAME" . "renaming a folder"))
  "What to call each kind of session in the mode line.
Keyed by the tail of the session name, so that a reader sees what is
happening rather than which protocol is doing it: \"fetching\", not \"IMAP
fetch\".")

(defun vm-net-session-doing (name)
  "What to call the session called NAME, for a reader watching the mode line."
  (let ((tail (and name (replace-regexp-in-string "\\`\\(IMAP\\|POP\\) +" ""
						 name))))
    (or (cdr (assoc tail vm-net-session-words)) "busy")))

(defun vm-net-session-live-p (session)
  "Whether SESSION is still to finish."
  (memq (vm-net-session-state session) '(new running)))

(defun vm-net-at-end (session function)
  "Call FUNCTION with no arguments when SESSION ends, however it ends.

For what the connection was made out of and the caller knows nothing about: a
tunnel program to kill, a buffer of diagnostics to bury.  Kept apart from
`finished', which is the caller's and is assigned rather than added to --
chaining onto it here was undone by the next caller that set it, and the
tunnel stayed running."
  (push function (vm-net-session-cleanups session)))

(defun vm-net--clean-up (session)
  "Run SESSION's cleanups, newest first.
One that signals is reported and the rest still run: they are independent, and
a cleanup that fails must not cost the caller its `finished' call."
  (let ((cleanups (vm-net-session-cleanups session)))
    (setf (vm-net-session-cleanups session) nil)
    (dolist (cleanup cleanups)
      (condition-case err
	  (funcall cleanup)
	(error (vm-net-warn 0 "cleaning up after %s: %s"
			(or (vm-net-session-name session) "session")
			(error-message-string err)))))))

(defun vm-net--sentinel (process event)
  "Fail PROCESS's session when the connection goes, saying what EVENT was.

A connection made with :nowait is not open when `make-network-process'
returns, so this is where a refused or unreachable server is heard about --
and where a server that hangs up mid-session is, which would otherwise leave
a generator waiting for input that cannot arrive."
  (let ((session (process-get process 'vm-net-session)))
    (when (and session (vm-net-session-live-p session)
	       (not (memq (process-status process) '(open run connect))))
      (setf (vm-net-session-error session)
	    (list 'vm-net-connection-lost
		  (format "%s connection %s"
			  (or (vm-net-session-name session) "network")
			  (string-trim event))))
      (vm-net-abandon session))))

(defvar vm-verbal-time)

(defun vm-net-warn (level &rest args)
  "Warn as `vm-warn' does, without stopping to be read.

The driver speaks from process filters, sentinels and timers.  `vm-warn' holds
its message on screen with `sit-for', which there is Emacs stopped for those
seconds: a folder whose server refused a flag per message stopped for two
seconds each time, four seconds to save two messages' flags where the work
itself takes a tenth of one.  `sit-for' in a filter also runs timers and other
filters, which is the re-entry the driver's own guards are there to refuse."
  (apply #'vm-warn level 0 args))

(defun vm-net-inform (level &rest args)
  "Say as `vm-inform' does, without stopping to be read.
`vm-verbal-time' is a pause per message, which a reader may want and a process
filter must not have: a fetch says how far it has got once per bunch."
  (let ((vm-verbal-time 0))
    (apply #'vm-inform level args)))

(define-error 'vm-net-connection-lost "Network connection lost")
(define-error 'vm-net-timeout "Network server timed out")

(defun vm-net-error-p (value)
  "Whether VALUE is an error object rather than an answer a session gave.

A session hands its caller either what its generator returned or the error
that stopped it, and this is how the caller tells them apart: an error object
is (SYMBOL . DATA) whose symbol has been through `define-error'.

Every condition the driver puts in `vm-net-session-error' has to be defined
for that to work.  `vm-net-timeout' was not, so a timed-out fetch was taken
for a list of messages, and the callback that would have reported it died in
the attempt: a POP fetch whose server went quiet left its caller waiting for
an answer that had already come and been thrown away."
  (and (consp value) (symbolp (car value))
       (get (car value) 'error-conditions)
       t))

(defun vm-net-start (session iterator)
  "Set ITERATOR going as SESSION's work, and let its process feed it.

Returns SESSION.  Nothing waits: the answer arrives at the `finished'
function, which is called with the session whether it returned or signalled.

A session with no process yet is one whose connection is still being made --
a tunnel program has to be running before there is anything to connect to.
It waits here until `vm-net-attach' gives it one, and only then does its
generator run."
  (setf (vm-net-session-iterator session) iterator)
  (setf (vm-net-session-state session) 'running)
  (let ((process (vm-net-session-process session)))
    (if (null process)
	session
      (vm-net--install session process)
      (vm-net--resume session nil)
      session)))

(defun vm-net--install (session process)
  "Make PROCESS SESSION's, and let it feed the session's generator."
  (setf (vm-net-session-process session) process)
  (unless (vm-net-session-buffer session)
    (setf (vm-net-session-buffer session) (process-buffer process)))
  (set-process-filter process #'vm-net--filter)
  (set-process-sentinel process #'vm-net--sentinel)
  (process-put process 'vm-net-session session))

(defun vm-net-attach (session process)
  "Give SESSION the PROCESS it was waiting for, and let it start.
For a connection that could not be made when the session was: the tunnel it
goes through had to be running first."
  (when (vm-net-session-live-p session)
    (vm-net--install session process)
    (vm-net--resume session nil))
  session)

(defun vm-net-fail (session error-data)
  "End SESSION with ERROR-DATA, which is what its caller hears about.
For a connection that was never made: there is no generator to signal in, so
the error is put where one that came out of the generator would go."
  (when (vm-net-session-live-p session)
    (setf (vm-net-session-error session) error-data)
    (vm-net--finish session 'failed))
  session)

(defun vm-net-when-ready (test seconds callback)
  "Call CALLBACK with t once TEST answers non-nil, or with nil after SECONDS.
Polled from a timer, so nothing waits.  What a tunnel program is waited for
with: it has to be listening before there is a port to connect to, and the
only way to know is to look."
  (let ((deadline (+ (float-time) seconds))
	(timer nil))
    (setq timer
	  (run-at-time
	   0.05 0.05
	   (lambda ()
	     (cond ((funcall test)
		    (cancel-timer timer)
		    (funcall callback t))
		   ((> (float-time) deadline)
		    (cancel-timer timer)
		    (funcall callback nil))))))
    timer))

(defun vm-net--filter (process string)
  "Put STRING in PROCESS's buffer and let its session carry on.
What VM's parsers read is the buffer, not the string, so this inserts the
way the default filter does -- at the process mark, keeping point where the
reader left it -- and only then asks the session to look."
  (let ((session (process-get process 'vm-net-session))
	(buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((moving (= (point) (process-mark process)))
	      (inhibit-read-only t))
	  (save-excursion
	    (goto-char (process-mark process))
	    (insert string)
	    (set-marker (process-mark process) (point)))
	  (when moving (goto-char (process-mark process))))))
    (when (and session (vm-net-session-live-p session))
      (vm-net-poll session))))

(defun vm-net-poll (session)
  "Resume SESSION if what it is waiting for has arrived.

Called by the filter for every chunk, and by the watchdog.  Refuses while the
generator is running: the watchdog fires from a timer, a timer can fire inside
whatever the generator is doing, and `iter-next' on a generator that is
already running is an error.  A dead buffer is refused for the same kind of
reason -- there is nothing there to read the answer out of."
  (when (and (vm-net-session-live-p session)
	     (not (vm-net-session-resuming session))
	     (buffer-live-p (vm-net-session-buffer session)))
    (let ((request (vm-net-session-request session)))
      (when (or (null request)
		(with-current-buffer (vm-net-session-buffer session)
		  (funcall request)))
	(vm-net--resume session nil)))))

(defconst vm-net--slice 0.05
  "How long `vm-net--resume' may work before it hands Emacs back.
Twenty turns a second, which is enough that typing and redisplay do not
stutter, and long enough that the timer between slices costs nothing next to
the parsing done in one.")

(defun vm-net--continue-soon (session)
  "Ask for SESSION to be resumed once Emacs has had its turn.
A timer of no delay, so it runs after the current command and after
redisplay.  Losing it costs a quarter of a second and not the session: the
watchdog polls every live session, which is what finds a session whose answer
arrived while its generator was running."
  (run-at-time 0 nil
	       (lambda ()
		 (when (vm-net-session-live-p session)
		   (vm-net-poll session)))))

(defun vm-net--resume (session input)
  "Give INPUT to SESSION's generator and record what it asks for next.
The generator returning ends the session; so does an error out of it, which
is kept rather than signalled -- there is no caller left to signal to, the
stack that started the session having gone.

Carries on for as long as what the generator asks for is already there.  The
filter polls with the request the generator had when the chunk arrived, so
anything that turned up while the generator was running is unasked about, and
a session whose whole answer arrived in that window waited for a chunk that
was never coming: three responses complete in the buffer and a POP fetch
stopped dead, until something else happened to poll it."
  (when (vm-net-session-resuming session)
    ;; A generator resumed while it is running is `iter-next' on a running
    ;; generator, which signals; and if it did not signal, it would be two
    ;; halves of one session writing one folder.  The poll refuses this case
    ;; rather than reaching it, so getting here is a fault in the driver.
    (error "%s session resumed while it was running"
	   (or (vm-net-session-name session) "network")))
  (let ((buffer (vm-net-session-buffer session))
	(deadline (+ (float-time) vm-net--slice))
	(again t))
    (setf (vm-net-session-resuming session) t)
    (unwind-protect
    (while again
      (setq again nil)
      (vm-net--cancel-timeout session)
      (let ((iterator (vm-net-session-iterator session)))
	(condition-case err
	    (let ((request (if (buffer-live-p buffer)
			       (with-current-buffer buffer
				 (iter-next iterator input))
			     (iter-next iterator input))))
	      (setf (vm-net-session-request session) request)
	      (vm-net--arm-timeout session)
	      (setq input nil)
	      (setq again (and request
			       (vm-net-session-live-p session)
			       (buffer-live-p buffer)
			       (with-current-buffer buffer
				 (and (funcall request) t))))
	      ;; Emacs gets a turn.  The answer to the next read is often
	      ;; already in the buffer -- a server that sends six thousand
	      ;; responses to one FETCH fills it faster than they are parsed --
	      ;; and this loop would then run to the end of them without
	      ;; returning, which is the whole of Emacs stopped for as long as
	      ;; that takes: 0.67 seconds in one filter call, measured on a
	      ;; mailbox of 6438.  Past the slice it hands back and asks to be
	      ;; called again, so redisplay and the keyboard get in between.
	      (when (and again (> (float-time) deadline))
		(setq again nil)
		(vm-net--continue-soon session)))
	  (iter-end-of-sequence
	   (setf (vm-net-session-value session) (cdr err))
	   (vm-net--finish session 'done))
	  (error
	   (setf (vm-net-session-error session) err)
	   (vm-net--finish session 'failed)))))
      (setf (vm-net-session-resuming session) nil))))


;; The watchdog: one timer, found in `timer-list' rather than remembered.
;;
;; One for all the sessions, rather than a timer armed and cancelled per
;; session per read.  A session was seen waiting 8.8 seconds on a three-second
;; timeout with its own timer sitting in `timer-list' unrun (emacs-vm/vm#717):
;; a lost timer took away the only thing that would ever have reported the
;; stall, and a generator waiting for input that cannot arrive waits for ever.
;; This one is armed while any session is waiting and cancelled when none is,
;; so a lost tick costs a quarter of a second rather than a session.
;;
;; Not kept in a variable, for the reason `vm-net--sessions' is not either: a
;; variable is state, and something that resets it -- the test harness restores
;; VM's variables between tests -- leaves the timer running with nothing
;; pointing at it, so the next session arms another.  Thirty of them were found
;; in `timer-list' at once that way, and the cancel could reach none of them.

(defconst vm-net--watchdog-interval 0.25
  "How often the watchdog looks at the deadlines.")

(defun vm-net--arm-timeout (session)
  "Note when SESSION's outstanding read runs out of time, and watch for it."
  (let ((seconds (vm-net-session-timeout session)))
    (when (and seconds (> seconds 0) (vm-net-session-live-p session))
      (setf (vm-net-session-deadline session) (+ (float-time) seconds))
      (unless (vm-net--watchdog-timers)
	(run-at-time vm-net--watchdog-interval vm-net--watchdog-interval
		     #'vm-net--watch)))))

(defun vm-net--watchdog-timers ()
  "Every watchdog timer that will still fire.

A timer whose function signalled is left marked as having been triggered and
is never rescheduled, and Emacs leaves it in `timer-list' all the same: it
sits there for ever, firing nothing.  One of those would satisfy a check for
\"is there a watchdog?\" while no watchdog was running, and every session after
it would wait on an answer nobody was going to look for -- which is a folder
hung for good.  A session was seen waiting 8.8 seconds on a three-second
timeout with its timer in `timer-list' unrun (emacs-vm/vm#717), and a POP
session in a long test run waited out its whole read with the answer sitting in
its buffer.

The dead ones are taken out as they are found, so the next arm makes a live
one."
  (let ((live nil))
    (dolist (timer timer-list live)
      (when (eq (timer--function timer) #'vm-net--watch)
	(if (timer--triggered timer)
	    (cancel-timer timer)
	  (push timer live))))))

(defun vm-net--cancel-timeout (session)
  "Stop watching SESSION's deadline."
  (setf (vm-net-session-deadline session) nil))

(defun vm-net--sessions ()
  "Every session a live process is running.

Found through the processes rather than kept in a list of its own: a list is
state, and the test harness restores VM's state between tests -- which took
sessions off the watch list and left them unwatched, hanging where they should
have timed out."
  (let (sessions)
    (dolist (process (process-list) sessions)
      (let ((session (process-get process 'vm-net-session)))
	(when (and session (vm-net-session-live-p session)
		   (not (memq session sessions)))
	  (push session sessions))))))

(defun vm-net--watch ()
  "Poll every live session, and fail the ones whose read has run out of time.

Polled here as well as from the filter because a session that is not polled
waits for ever: the filter is the only other thing that asks, and an answer it
does not ask about after is an answer nobody looks at.  A quarter of a second
of lateness is the cost of that safety net; a hung folder was the cost of not
having it."
  (let ((now (float-time))
	(watching nil))
    (dolist (session (vm-net--sessions))
      (setq watching t)
      ;; One session's fault must not take the watchdog down with it: an error
      ;; out of a timer leaves Emacs marking the timer as triggered and never
      ;; rescheduling it, so the sessions after this one -- and every session
      ;; afterwards, for the life of the Emacs -- would have nothing watching
      ;; them.  Said out loud and carried on with instead.
      (condition-case err
	  (vm-net--watch-session session now)
	(error (vm-net-warn 0 "%s: watching it failed: %s"
			    (or (vm-net-session-name session) "session")
			    (error-message-string err)))))
    (unless watching
      (mapc #'cancel-timer (vm-net--watchdog-timers)))))

(defun vm-net--watch-session (session now)
  "Poll SESSION, and fail it if its read ran out of time before NOW."
  (if (vm-net-session-resuming session)
      ;; Mid-generator: polling it would be `iter-next' on a running
      ;; generator, and timing it out would be `iter-close' on one -- which
      ;; errors, after the session has been marked failed and its caller told,
      ;; leaving the resume to finish it a second time.  A session still
      ;; running when its read has run out of time is a fault in the driver
      ;; rather than a slow server, so it is said once and left for the next
      ;; tick, when the generator will have stopped.
      (when (and (vm-net-session-deadline session)
		 (> now (vm-net-session-deadline session))
		 (not (vm-net-session-said-stuck session)))
	(setf (vm-net-session-said-stuck session) t)
	(vm-net-warn 0 "%s: still working after its read timed out"
		     (or (vm-net-session-name session) "session")))
    (vm-net-poll session)
    (when (and (vm-net-session-live-p session)
	       (vm-net-session-deadline session)
	       (> now (vm-net-session-deadline session)))
      (vm-net--timed-out session))))

(defun vm-net--timed-out (session)
  "End SESSION: the server said nothing for long enough."
  (when (vm-net-session-live-p session)
    (setf (vm-net-session-error session)
	  (list 'vm-net-timeout
		(format "%s server timed out after %s seconds"
			(or (vm-net-session-name session) "network")
			(vm-net-session-timeout session))))
    (vm-net-abandon session 'failed)))

(defun vm-net-abandon (session &optional state)
  "Stop SESSION, running its generator's cleanup forms.

STATE is what to record, `failed' unless said otherwise.  The generator is
closed rather than dropped: a dropped one runs no `unwind-protect', so
whatever the session had undertaken to do at the end -- a QUIT, an EXPUNGE,
putting a folder's flags back -- would silently not happen."
  (when (vm-net-session-live-p session)
    (vm-net--finish session (or state 'failed))))

(defun vm-net--finish (session state)
  "Record that SESSION has reached STATE, close its generator and say so."
  (vm-net--cancel-timeout session)
  (setf (vm-net-session-state session) state)
  (setf (vm-net-session-request session) nil)
  (let ((iterator (vm-net-session-iterator session)))
    (when iterator
      ;; Closing runs the generator's `unwind-protect' forms -- the LOGOUT, the
      ;; QUIT, the folder's bookkeeping.  One that signals used to be dropped
      ;; on the floor by `ignore-errors', which is where a folder update that
      ;; failed would have gone.
      (condition-case err
	  (iter-close iterator)
	(error (vm-net-warn 0 "%s: while stopping: %s"
			(or (vm-net-session-name session) "session")
			(error-message-string err))))
      (setf (vm-net-session-iterator session) nil)))
  (let ((process (vm-net-session-process session)))
    (when (processp process)
      (process-put process 'vm-net-session nil)))
  (vm-net--clean-up session)
  (let ((finished (vm-net-session-finished session)))
    (when finished
      ;; The caller's own code, run from wherever the session ended -- which is
      ;; usually a process filter, where an error is printed as "error in
      ;; process filter" and the reader is left to guess whose.  Said plainly
      ;; instead; the session is over either way.
      (condition-case err
	  (funcall finished session)
	(error (vm-net-warn 0 "%s: %s" (or (vm-net-session-name session) "session")
			(error-message-string err)))))))

(cl-defun vm-net-session (&key process name timeout finished)
  "A session over PROCESS, to be started with `vm-net-start'.
NAME goes in messages, TIMEOUT is how long a single read may take, and
FINISHED is called with the session when it ends, whichever way it ends."
  (vm-net-session--make
   :process process
   :buffer (and (processp process) (process-buffer process))
   :name name
   :timeout timeout
   :finished finished))

;;; What a generator asks to wait for

(defun vm-net-request-growth ()
  "A request that is satisfied when the process buffer has more in it.
What the IMAP parser wants: it re-reads from where it was and asks again."
  (let ((size (buffer-size)))
    (lambda () (> (buffer-size) size))))

(defun vm-net-request-now ()
  "A request that is satisfied straight away.
What a generator yields to say \"I have more to do and none of it is
waiting\": the driver takes it back at once, and the slice in
`vm-net--resume' is then free to hand Emacs a turn first.  A generator that
parses a great deal of what has already arrived -- thousands of responses to
one command -- yields this between pieces of it, so that the work is
interruptible rather than one step that runs to the end."
  (lambda () t))

(defun vm-net-request-position (position)
  "A request that is satisfied when the process buffer reaches POSITION.
What a read of a known length asks with -- an IMAP literal, whose octet
count arrives before its octets do.  One resume for the whole of it, however
many chunks it comes in."
  (lambda () (>= (point-max) position)))

(defun vm-net-request-match (regexp &optional start)
  "A request that is satisfied when REGEXP is in the process buffer.
Searched from START, or from where the reader is now.  What the POP reads
want: each of them waits for a terminator.

The filter asks once per chunk that arrives, so each ask searches only what
has turned up since the last one, back to the start of the line the last one
ended in -- far enough for a terminator split across two chunks, and REGEXP
is a terminator, which does not span a line.  Searching the whole response
every time made reading it quadratic: a 2 MB message arriving in TCP-sized
chunks spent seconds in `re-search-forward' and a 20 MB one minutes, which
is what a hung Emacs looks like from the outside.

Answering yes is remembered, so a request that has been satisfied stays
satisfied however often it is asked: what it reports is that the response is
complete, and searching only the new text would otherwise make that answer
depend on when it was asked."
  (let ((from (or start (point)))
	(searched nil)
	(found nil))
    (lambda ()
      (or found
	  (let ((begin (if searched
			   (max from (save-excursion
				       (goto-char (min searched (point-max)))
				       (forward-line 0)
				       (point)))
			 from)))
	    (setq searched (point-max))
	    (setq found (save-excursion
			  (goto-char begin)
			  (and (re-search-forward regexp nil t) t))))))))


;;; A connection that has to be tunnelled

(defun vm-net-free-port ()
  "A local port nothing is listening on.
Asked of the operating system by taking one and giving it back, rather than
by trying to connect to one port after another until a connection fails --
which is what the blocking path does, and every one of those attempts waits."
  (let* ((server (make-network-process
		  :name " *vm-net-port*" :server t :service t
		  :host 'local :family 'ipv4 :noquery t))
	 (port (process-contact server :service)))
    (delete-process server)
    port))

(defvar vm-net--probes (make-hash-table :test 'eql)
  "The connection each port is being probed with, by port.
A probe outlives the poll that started it: a connection made with :nowait is
not open when `make-network-process' returns, and the next poll is where the
answer is.")

(defun vm-net-listening-p (port)
  "Whether something is listening on PORT here.

Asked without waiting.  A connection is started on the first ask and its
status read on the next: a blocking connect is fast to a port on this machine
but it is still a wait, and this runs from a timer while somebody is typing.

The probe is closed as soon as it has answered, so nothing is left connected
to the tunnel."
  (let ((probe (gethash port vm-net--probes)))
    (cond
     ((null probe)
      (setf (gethash port vm-net--probes)
	    (ignore-errors
	      (make-network-process :name " *vm-net-probe*"
				    :host "127.0.0.1" :service port
				    :noquery t :nowait t)))
      nil)
     ((not (processp probe))
      ;; the connection could not even be started: nothing is listening, and
      ;; asking again is the next poll's business
      (remhash port vm-net--probes)
      nil)
     (t
      (let ((status (process-status probe)))
	(cond ((memq status '(open run))
	       (delete-process probe)
	       (remhash port vm-net--probes)
	       t)
	      ((eq status 'connect)		; still trying
	       nil)
	      (t				; refused, or gone
	       (delete-process probe)
	       (remhash port vm-net--probes)
	       nil)))))))

(defun vm-net-forget-probe (port)
  "Close the probe waiting on PORT, if there is one.
A probe outlives the poll that started it, so the wait has to take the last
one away with it: a connection left open to a port keeps whatever is on the
other end of it busy, and a port on this machine is handed out again."
  (let ((probe (gethash port vm-net--probes)))
    (when (processp probe) (delete-process probe))
    (remhash port vm-net--probes)))

(defun vm-net-tunnel (session program arguments port seconds ready)
  "Run PROGRAM with ARGUMENTS and call READY when PORT is listening.

READY is called with the tunnel process, or with nil if PORT is still not
listening after SECONDS -- in which case SESSION is failed, since there is
nothing for it to talk to.  The tunnel is killed when the session ends.

Nothing waits: the program is started, and a timer looks every fiftieth of a
second to see whether it is ready yet."
  (let ((tunnel (apply #'start-process (format "vm-net tunnel %s" port)
		       (generate-new-buffer (format " *vm-net tunnel %s*" port))
		       program arguments)))
    (set-process-query-on-exit-flag tunnel nil)
    (vm-net-at-end
     session
     (lambda ()
       (when (process-live-p tunnel) (delete-process tunnel))
       (let ((buffer (process-buffer tunnel)))
	 (when (buffer-live-p buffer) (kill-buffer buffer)))
       (vm-net-forget-probe port)))
    (vm-net--watch-for-tunnel session port seconds ready tunnel)
    tunnel))

(defun vm-net--watch-for-tunnel (session port seconds ready tunnel)
  "Wait for PORT to answer, and call READY with TUNNEL or nil.
The timer goes when the session does: a session abandoned while its tunnel was
still coming up left this polling, and each poll opens a probe connection."
  (let ((timer nil))
    (setq timer
	  (vm-net-when-ready
	   (lambda () (vm-net-listening-p port))
	   seconds
	   (lambda (up)
	     ;; whichever way it went, the wait is over and its probe goes with it
	     (vm-net-forget-probe port)
	     (if up
		 (funcall ready tunnel)
	       (vm-net-fail session
			    (list 'vm-net-tunnel-failed
				  (format "%s did not start listening on port %s"
					  (car (process-command tunnel)) port)))
	       (funcall ready nil)))))
    (vm-net-at-end session
		   (lambda ()
		     (when (memq timer timer-list) (cancel-timer timer))))
    timer))

(define-error 'vm-net-tunnel-failed "Tunnel program did not come up")


;;; A connection that is a program's standard input and output

(defun vm-net-pipe (session name buffer program arguments)
  "Run PROGRAM with ARGUMENTS as SESSION's connection, and answer with it.

What stunnel is used through, and what the blocking path has always done with
it: given no port to listen on, stunnel relays its standard input and output,
so the program is the connection.  There is nothing to wait for and nothing to
connect to -- unlike ssh, which is asked for a local port and forwards it.

The program's diagnostics go to a buffer of their own, killed when the session
ends.  In BUFFER they would be read as protocol.

Pipes rather than a pty: a pty echoes what is written to it and rewrites the
line endings, and IMAP counts octets."
  (let* ((errors (generate-new-buffer (format " *%s errors*" name)))
	 (process (condition-case err
		      (make-process :name name :buffer buffer
				    :command (cons program arguments)
				    :coding 'binary :connection-type 'pipe
				    :noquery t :stderr errors)
		    ;; a program that is not installed is the usual one, and
		    ;; the buffer for its diagnostics would outlive the attempt
		    (error (kill-buffer errors)
			   (signal (car err) (cdr err))))))
    (let ((reader (get-buffer-process errors)))
      (when (processp reader) (set-process-query-on-exit-flag reader nil)))
    (vm-net-at-end session
		   (lambda ()
		     (when (buffer-live-p errors) (kill-buffer errors))))
    process))

(provide 'vm-net)
;;; vm-net.el ends here
