;;; jabber-keepalive.el --- Try to detect lost connection  -*- lexical-binding: t; -*-

;; Copyright (C) 2004, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2007 - Detlev Zundel - dzu@gnu.org
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:
;;
;; Keepalive - send something to the server and see if it answers.
;; These keepalive functions send a urn:xmpp:ping request to the
;; server every X minutes, and considers the connection broken if
;; they get no answer within Y seconds.

;;; Code:

(require 'fsm)
(require 'jabber-core)
(require 'jabber-lifecycle)
(require 'jabber-ping)

(declare-function sleep-event-state "ext:system-sleep" (event) t)
(declare-function system-sleep-enable "ext:system-sleep" ())
(defvar system-sleep-event-functions)

;;;###autoload
(defgroup jabber-keepalive nil
  "Keepalive functions try to detect lost connection."
  :group 'jabber)

(defcustom jabber-keepalive-interval 600
  "Interval in seconds between connection checks."
  :type 'integer)

(defcustom jabber-keepalive-timeout 20
  "Seconds to wait for response from server."
  :type 'integer)

(defvar jabber-keepalive-timer nil
  "Timer object for keepalive function.")

(defvar jabber-keepalive-timeout-timer nil
  "Timer object for keepalive timeout function.")

(defvar jabber-keepalive-pending nil
  "Outstanding keepalive entries as (CONNECTION . TRANSPORT) pairs.")

(defvar jabber-keepalive-round 0
  "Identity of the current keepalive round.")

(defvar jabber-keepalive-debug nil
  "Log keepalive traffic when non-nil.")

;; Global reference declarations

(defun jabber-keepalive--retire-round ()
  "Retire the current keepalive round without stopping future rounds."
  (when (timerp jabber-keepalive-timeout-timer)
    (cancel-timer jabber-keepalive-timeout-timer))
  (setq jabber-keepalive-timeout-timer nil
        jabber-keepalive-pending nil
        jabber-keepalive-round (1+ jabber-keepalive-round)))

(defun jabber-keepalive--active-transports ()
  "Return active connection and exact transport pairs."
  (let (result)
    (dolist (jc jabber-connections)
      (when (jabber-connection-active-p jc)
        (push (cons jc (plist-get (fsm-get-state-data jc) :connection))
              result)))
    (nreverse result)))

;;;###autoload
(defun jabber-keepalive-start (&optional _jc)
  "Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect it if it doesn't answer.  See variable `jabber-keepalive-interval'
and variable `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument."
  (interactive)

  (jabber-keepalive-stop)

  (setq jabber-keepalive-timer
	(run-with-timer jabber-keepalive-interval
			jabber-keepalive-interval
			#'jabber-keepalive-do))
  (add-hook 'jabber-post-disconnect-hook #'jabber-keepalive-stop))

(defun jabber-keepalive-stop ()
  "Deactivate keepalive."
  (interactive)

  (when jabber-keepalive-timer
    (cancel-timer jabber-keepalive-timer))
  (setq jabber-keepalive-timer nil)
  (jabber-keepalive--retire-round))

(defun jabber-keepalive-do ()
  "Send a ping to every connection and arm the timeout timer."
  (when jabber-keepalive-debug
    (message "%s: sending keepalive packet(s)" (current-time-string)))
  (jabber-keepalive--retire-round)
  (setq jabber-keepalive-pending (jabber-keepalive--active-transports))
  (when jabber-keepalive-pending
    (let ((round jabber-keepalive-round))
      (setq jabber-keepalive-timeout-timer
	    (run-with-timer jabber-keepalive-timeout nil
			    #'jabber-keepalive-timeout round))
      (dolist (entry (copy-sequence jabber-keepalive-pending))
        (let ((c (car entry))
              (transport (cdr entry)))
          (condition-case err
              ;; Any IQ response proves that the transport is alive.
              (jabber-ping-send
               c nil #'jabber-keepalive-got-response
               (cons round transport) (cons round transport))
            (error
             (setq jabber-keepalive-pending
                   (delq entry jabber-keepalive-pending))
             (message "Jabber keepalive send failed: %s"
                      (error-message-string err))
             (fsm-send c
                       (list :connection-dead transport
                             "Keepalive send failed")))))))
    (when (and (null jabber-keepalive-pending)
               (timerp jabber-keepalive-timeout-timer))
      (cancel-timer jabber-keepalive-timeout-timer)
      (setq jabber-keepalive-timeout-timer nil))))

(defun jabber-keepalive-got-response (jc _xml-data round-transport)
  "Mark JC answered when ROUND-TRANSPORT matches the current ping.
_XML-DATA is the ignored IQ result or error stanza."
  (when jabber-keepalive-debug
    (message "%s: got keepalive response from %s"
	     (current-time-string)
	     (plist-get (fsm-get-state-data jc) :server)))
  (let ((entry (assq jc jabber-keepalive-pending)))
    (when (and entry
               (= (car round-transport) jabber-keepalive-round)
               (eq (cdr round-transport) (cdr entry)))
      (setq jabber-keepalive-pending
            (delq entry jabber-keepalive-pending))
      (when (and (null jabber-keepalive-pending)
                 (timerp jabber-keepalive-timeout-timer))
        (cancel-timer jabber-keepalive-timeout-timer)
        (setq jabber-keepalive-timeout-timer nil)))))

(defun jabber-keepalive-timeout (round)
  "Report unanswered transports from current ROUND as lost."
  (when (= round jabber-keepalive-round)
    (let ((pending jabber-keepalive-pending))
      (setq jabber-keepalive-timeout-timer nil
            jabber-keepalive-pending nil
            jabber-keepalive-round (1+ jabber-keepalive-round))
      (dolist (entry pending)
        (let ((c (car entry))
              (transport (cdr entry)))
          (condition-case err
              (progn
                (message "%s: keepalive timeout, connection to %s considered lost"
		         (current-time-string)
		         (plist-get (fsm-get-state-data c) :server))
                (fsm-send-sync
                 c (list :connection-dead transport "Keepalive timeout")))
            (error
             (message "Jabber keepalive timeout handling failed: %s"
                      (error-message-string err))))))
      (unless jabber-connections
        (jabber-keepalive-stop)))))

(defun jabber-keepalive--system-sleep-event (event)
  "Reconnect active transports after a post-wake sleep EVENT."
  (condition-case err
      (when (eq (sleep-event-state event) 'post-wake)
        (jabber-keepalive--retire-round)
        (dolist (entry (jabber-keepalive--active-transports))
          (condition-case peer-error
              (fsm-send-sync
               (car entry)
               (list :connection-dead (cdr entry) "System wake"))
            (error
             (message "Jabber post-wake reconnect failed: %s"
                      (error-message-string peer-error))))))
    (error
     (message "Jabber system sleep handler failed: %s"
              (error-message-string err)))))

(defun jabber-keepalive--enable-system-sleep (_jc)
  "Enable optional sleep handling for a newly established session JC."
  (condition-case err
      (when (require 'system-sleep nil t)
        (add-hook 'system-sleep-event-functions
                  #'jabber-keepalive--system-sleep-event)
        (system-sleep-enable))
    (error
     (message "Jabber system sleep support failed: %s"
              (error-message-string err)))))

(add-hook 'jabber-lifecycle-session-bootstrap-functions
          #'jabber-keepalive--enable-system-sleep)

;;;; Whitespace pings - less traffic, no error checking on our side
;;;
;;; Whitespace pings keep NAT mappings active between response-checked pings.

(defcustom jabber-whitespace-ping-interval 30
  "Send a space character to the server with this interval, in seconds.

This is a traditional remedy for a number of problems: to keep NAT
boxes from considering the connection dead, to have the OS discover
earlier that the connection is lost, and to placate servers which rely
on the client doing this, e.g.  Openfire.

If you want to verify that the server is able to answer, see
`jabber-keepalive-start' for another mechanism."
  :type '(integer :tag "Interval in seconds")
  :group 'jabber-core)

(defvar jabber-whitespace-ping-timer nil
  "Timer object for whitespace pings.")

;;;###autoload
(defun jabber-whitespace-ping-start (&optional _jc)
  "Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts."
  (interactive)

  (when jabber-whitespace-ping-timer
    (jabber-whitespace-ping-stop))

  ;; Send one ping immediately to prevent servers with aggressive
  ;; idle timeouts from dropping the connection before the first
  ;; timer fires.
  (jabber-whitespace-ping-do)
  (setq jabber-whitespace-ping-timer
	(run-with-timer jabber-whitespace-ping-interval
			jabber-whitespace-ping-interval
			#'jabber-whitespace-ping-do))
  (add-hook 'jabber-post-disconnect-hook #'jabber-whitespace-ping-stop))

(defun jabber-whitespace-ping-stop ()
  "Deactivate whitespace pings."
  (interactive)

  (when jabber-whitespace-ping-timer
    (cancel-timer jabber-whitespace-ping-timer)
    (setq jabber-whitespace-ping-timer nil)))

(defun jabber-whitespace-ping-do ()
  "Send a single space to every live connection as a whitespace ping."
  (dolist (c jabber-connections)
    (let* ((state-data (fsm-get-state-data c))
	   (connection (plist-get state-data :connection)))
      (if (and connection (process-live-p connection))
	  (condition-case err
	      (jabber-send-string c " ")
	    (error
	     (message "jabber-keepalive: whitespace ping failed: %s" err)
	     (fsm-send c (list :connection-dead connection
			       "Whitespace ping failed"))))
	;; Connection process is dead but FSM didn't transition.
	;; Only act when stuck in :session-established; other states
	;; are transient and will resolve on their own.
	(when (eq (get c :state) :session-established)
	  (fsm-send c (list :connection-dead connection
			    "Connection process lost")))))))

(provide 'jabber-keepalive)

;;; jabber-keepalive.el ends here
