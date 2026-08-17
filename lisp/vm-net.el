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

(declare-function vm-inform "vm-misc" (level &rest args))

(cl-defstruct (vm-net-session (:constructor vm-net-session--make)
			      (:copier nil))
  "A network session VM is running without waiting for it."
  process				; the network process
  buffer				; its process buffer
  name					; for messages: "imap", "pop"
  timeout				; seconds to wait for input, or nil
  timeout-timer				; the timer enforcing that
  iterator				; the generator doing the work
  request				; what it last asked to wait for
  (state 'new)				; new, running, done or failed
  value					; what the generator returned
  error					; the error that stopped it, if any
  finished				; called with the session when it ends
  buffer-types)				; the per-session buffer-type stack

(defun vm-net-session-live-p (session)
  "Whether SESSION is still to finish."
  (memq (vm-net-session-state session) '(new running)))

(defun vm-net--sentinel (process event)
  "Fail PROCESS's session when the connection goes, saying what EVENT was.

A connection made with :nowait is not open when `make-network-process\='
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

(define-error 'vm-net-connection-lost "Network connection lost")

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
Called by the filter for every chunk, and by the timeout."
  (when (vm-net-session-live-p session)
    (let ((request (vm-net-session-request session)))
      (when (or (null request)
		(with-current-buffer (vm-net-session-buffer session)
		  (funcall request)))
	(vm-net--resume session nil)))))

(defun vm-net--resume (session input)
  "Give INPUT to SESSION's generator and record what it asks for next.
The generator returning ends the session; so does an error out of it, which
is kept rather than signalled -- there is no caller left to signal to, the
stack that started the session having gone."
  (vm-net--cancel-timeout session)
  (let ((iterator (vm-net-session-iterator session))
	(buffer (vm-net-session-buffer session)))
    (condition-case err
	(let ((request (if (buffer-live-p buffer)
			   (with-current-buffer buffer
			     (iter-next iterator input))
			 (iter-next iterator input))))
	  (setf (vm-net-session-request session) request)
	  (vm-net--arm-timeout session))
      (iter-end-of-sequence
       (setf (vm-net-session-value session) (cdr err))
       (vm-net--finish session 'done))
      (error
       (setf (vm-net-session-error session) err)
       (vm-net--finish session 'failed)))))

(defun vm-net--arm-timeout (session)
  "Start SESSION's timeout, if it has one, for the read it is now waiting on."
  (let ((seconds (vm-net-session-timeout session)))
    (when (and seconds (> seconds 0) (vm-net-session-live-p session))
      (setf (vm-net-session-timeout-timer session)
	    (run-at-time seconds nil #'vm-net--timed-out session)))))

(defun vm-net--cancel-timeout (session)
  (let ((timer (vm-net-session-timeout-timer session)))
    (when timer
      (cancel-timer timer)
      (setf (vm-net-session-timeout-timer session) nil))))

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
      (ignore-errors (iter-close iterator))
      (setf (vm-net-session-iterator session) nil)))
  (let ((process (vm-net-session-process session)))
    (when (processp process)
      (process-put process 'vm-net-session nil)))
  (let ((finished (vm-net-session-finished session)))
    (when finished
      (funcall finished session))))

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

(defun vm-net-listening-p (port)
  "Whether something is listening on PORT here.
The connection is made with :nowait and thrown away: what is being asked is
whether the tunnel is up, and the answer is that a connection to it can be
started at all."
  (let ((process (ignore-errors
		   (make-network-process :name " *vm-net-probe*"
					 :host "127.0.0.1" :service port
					 :noquery t :nowait nil))))
    (when process
      (delete-process process)
      t)))

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
    (let ((finished (vm-net-session-finished session)))
      (setf (vm-net-session-finished session)
	    (lambda (ended)
	      (when (process-live-p tunnel) (delete-process tunnel))
	      (let ((buffer (process-buffer tunnel)))
		(when (buffer-live-p buffer) (kill-buffer buffer)))
	      (when finished (funcall finished ended)))))
    (vm-net-when-ready
     (lambda () (vm-net-listening-p port))
     seconds
     (lambda (up)
       (if up
	   (funcall ready tunnel)
	 (vm-net-fail session
		      (list 'vm-net-tunnel-failed
			    (format "%s did not start listening on port %s"
				    program port)))
	 (funcall ready nil))))
    tunnel))

(define-error 'vm-net-tunnel-failed "Tunnel program did not come up")

(provide 'vm-net)
;;; vm-net.el ends here
