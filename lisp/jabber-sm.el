;;; jabber-sm.el --- XEP-0198 Stream Management  -*- lexical-binding: t; -*-

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

;; XEP-0198 Stream Management provides two features:
;;
;; 1. Stanza acknowledgement: counters tracking what each side received,
;;    so undelivered stanzas can be detected and retransmitted.
;;
;; 2. Stream resumption: fast reconnect that skips SASL auth and
;;    preserves the server-side session.
;;
;; SM state is stored on the FSM state-data plist.  All functions in this
;; file are pure (take state-data, return values) except for the timer
;; management and the send helpers that call `jabber-send-string'.

;;; Code:

(require 'cl-lib)

(declare-function fsm-get-state-data "fsm" (fsm))
(declare-function jabber-send-string "jabber-core.el" (jc string))
(declare-function jabber-xml-node-name "jabber-xml.el" (node))
(declare-function jabber-xml-get-attribute "jabber-xml.el" (node attribute))
(declare-function jabber-xml-get-xmlns "jabber-xml.el" (node))
(declare-function jabber-xml-get-children "jabber-xml.el" (node child-name))
(declare-function jabber-xml-child-with-xmlns "jabber-xml.el" (node xmlns))
(declare-function jabber-sexp2xml "jabber-xml.el" (sexp))
(defvar jabber-debug-log-xml)
(defvar jabber-connections)

(defconst jabber-sm-xmlns "urn:xmpp:sm:3"
  "XEP-0198 Stream Management namespace (version 3).")

(defgroup jabber-sm nil
  "XEP-0198 Stream Management."
  :group 'jabber)

(defcustom jabber-sm-enable t
  "If non-nil, negotiate Stream Management when the server supports it."
  :type 'boolean
  :group 'jabber-sm)

(defcustom jabber-sm-request-interval 30
  "Seconds between periodic ack requests."
  :type 'integer
  :group 'jabber-sm)

(defcustom jabber-sm-ack-interval 50
  "Send a proactive ack every this many inbound stanzas.
When nil, only send acks in response to server <r/> requests."
  :type '(choice (integer :tag "Stanzas between acks")
                 (const :tag "Only on request" nil))
  :group 'jabber-sm)

(defcustom jabber-sm-max-in-flight 10
  "Maximum number of unacknowledged outbound stanzas before queuing.
When the in-flight count reaches this limit, further stanzas are
queued and drained as the server acknowledges previous ones.
Set to nil to disable back-pressure (send everything immediately)."
  :type '(choice (integer :tag "Max unacked stanzas")
                 (const :tag "No limit" nil))
  :group 'jabber-sm)

;;; Counter arithmetic (handles 2^32 wraparound per XEP-0198 section 5)

(defconst jabber-sm--counter-max (expt 2 32)
  "Stanza counters wrap at 2^32.")

(defun jabber-sm--inc-counter (n)
  "Increment counter N, wrapping at 2^32."
  (mod (1+ n) jabber-sm--counter-max))

(defun jabber-sm--counter-delta (a b)
  "Return the forward distance from counter B to counter A.
Both values are mod 2^32.  Result is in [0, 2^32)."
  (mod (- a b) jabber-sm--counter-max))

(defun jabber-sm--counter-<= (a b)
  "Return non-nil if counter A is at or behind counter B.
Uses forward-distance heuristic: if delta(B,A) < 2^31, A <= B."
  (< (jabber-sm--counter-delta b a) (/ jabber-sm--counter-max 2)))

;;; Predicates for SM XML elements

(defun jabber-sm--r-p (stanza)
  "Return non-nil if STANZA is an SM <r/> request."
  (and (eq (jabber-xml-node-name stanza) 'r)
       (equal (jabber-xml-get-xmlns stanza) jabber-sm-xmlns)))

(defun jabber-sm--a-p (stanza)
  "Return non-nil if STANZA is an SM <a/> acknowledgement."
  (and (eq (jabber-xml-node-name stanza) 'a)
       (equal (jabber-xml-get-xmlns stanza) jabber-sm-xmlns)))

(defun jabber-sm--enabled-p (stanza)
  "Return non-nil if STANZA is an SM <enabled/> response."
  (and (eq (jabber-xml-node-name stanza) 'enabled)
       (equal (jabber-xml-get-xmlns stanza) jabber-sm-xmlns)))

(defun jabber-sm--resumed-p (stanza)
  "Return non-nil if STANZA is an SM <resumed/> response."
  (and (eq (jabber-xml-node-name stanza) 'resumed)
       (equal (jabber-xml-get-xmlns stanza) jabber-sm-xmlns)))

(defun jabber-sm--failed-p (stanza)
  "Return non-nil if STANZA is an SM <failed/> response."
  (and (eq (jabber-xml-node-name stanza) 'failed)
       (equal (jabber-xml-get-xmlns stanza) jabber-sm-xmlns)))

;;; State-data management

(defconst jabber-sm--initial-keys
  '(:sm-enabled nil
    :sm-id nil
    :sm-resume-max nil
    :sm-outbound-count 0
    :sm-inbound-count 0
    :sm-outbound-queue nil
    :sm-pending-queue nil
    :sm-last-acked 0
    :sm-resuming nil
    :sm-resumed nil
    :sm-r-timer nil)
  "Initial SM keys for the FSM state-data plist.")

(defun jabber-sm--reset (state-data)
  "Return STATE-DATA with all SM keys reset to initial values."
  (let ((keys jabber-sm--initial-keys))
    (while keys
      (setq state-data (plist-put state-data (car keys) (cadr keys)))
      (setq keys (cddr keys))))
  state-data)

;;; Stream features check

(defun jabber-sm--features-have-sm-p (state-data)
  "Return non-nil if stream features in STATE-DATA include SM."
  (let ((features (plist-get state-data :stream-features)))
    (when features
      (jabber-xml-child-with-xmlns features jabber-sm-xmlns))))

;;; Stanza counting

(defun jabber-sm--stanza-p (sexp)
  "Return non-nil if SEXP is a countable stanza (message, presence, or iq)."
  (memq (jabber-xml-node-name sexp) '(message presence iq)))

(defun jabber-sm--count-outbound (state-data sexp)
  "Increment outbound counter and queue SEXP if SM is enabled.
Return updated STATE-DATA."
  (when (and (plist-get state-data :sm-enabled)
             (jabber-sm--stanza-p sexp))
    (let ((count (jabber-sm--inc-counter
                  (plist-get state-data :sm-outbound-count))))
      (setq state-data (plist-put state-data :sm-outbound-count count))
      (setq state-data
            (plist-put state-data :sm-outbound-queue
                       (nconc (plist-get state-data :sm-outbound-queue)
                              (list (cons count sexp)))))))
  state-data)

(defun jabber-sm--count-inbound (jc state-data stanza)
  "Increment inbound counter if SM is enabled and STANZA is countable.
When `jabber-sm-ack-interval' is set, send a proactive <a/> every
that many stanzas.  JC is the Jabber connection.
Return updated STATE-DATA."
  (when (and (plist-get state-data :sm-enabled)
             (jabber-sm--stanza-p stanza))
    (let ((count (jabber-sm--inc-counter
                  (plist-get state-data :sm-inbound-count))))
      (setq state-data
            (plist-put state-data :sm-inbound-count count))
      (when (and jabber-sm-ack-interval
                 (zerop (mod count jabber-sm-ack-interval)))
        (jabber-sm--send-ack jc state-data))))
  state-data)

;;; Ack send/receive

(defun jabber-sm--make-ack-xml (h)
  "Return the XML string for <a h='H' xmlns='urn:xmpp:sm:3'/>."
  (format "<a xmlns='%s' h='%d'/>" jabber-sm-xmlns h))

(defun jabber-sm--make-request-xml ()
  "Return the XML string for <r xmlns='urn:xmpp:sm:3'/>."
  (format "<r xmlns='%s'/>" jabber-sm-xmlns))

(defun jabber-sm--send-ack (jc state-data)
  "Send an <a/> ack to JC with inbound count from STATE-DATA."
  (jabber-send-string jc (jabber-sm--make-ack-xml
                          (plist-get state-data :sm-inbound-count))))

(defun jabber-sm--request-ack (jc)
  "Send an <r/> request to JC."
  (jabber-send-string jc (jabber-sm--make-request-xml)))

(defun jabber-sm--prune-queue (queue h)
  "Return QUEUE with entries whose count is <= H removed."
  (cl-remove-if (lambda (entry)
                  (jabber-sm--counter-<= (car entry) h))
                queue))

(defun jabber-sm--process-ack (state-data stanza)
  "Process an incoming <a/> ack STANZA, pruning the outbound queue.
Return updated STATE-DATA."
  (let* ((h (string-to-number (or (jabber-xml-get-attribute stanza 'h) "0")))
         (sent (plist-get state-data :sm-outbound-count))
         (queue (plist-get state-data :sm-outbound-queue))
         (pruned (jabber-sm--prune-queue queue h)))
    (when (not (jabber-sm--counter-<= h sent))
      (message "SM warning: server acked more stanzas than sent (h=%d, sent=%d)"
               h sent))
    (setq state-data (plist-put state-data :sm-last-acked h))
    (setq state-data (plist-put state-data :sm-outbound-queue pruned))
    state-data))

;;; Enable/resume XML generation

(defun jabber-sm--make-enable-xml ()
  "Return the XML string for <enable resume='true' xmlns='urn:xmpp:sm:3'/>."
  (format "<enable xmlns='%s' resume='true'/>" jabber-sm-xmlns))

(defun jabber-sm--make-resume-xml (h previd)
  "Return the XML string for <resume h='H' previd='PREVID'/>."
  (format "<resume xmlns='%s' h='%d' previd='%s'/>" jabber-sm-xmlns h previd))

(defun jabber-sm--parse-enabled (stanza)
  "Parse an <enabled/> STANZA.
Return a plist (:id ID :resume RESUME :max MAX)."
  (list :id (jabber-xml-get-attribute stanza 'id)
        :resume (member (jabber-xml-get-attribute stanza 'resume) '("true" "1"))
        :max (let ((max-str (jabber-xml-get-attribute stanza 'max)))
               (when max-str (string-to-number max-str)))))

(defun jabber-sm--apply-enabled (state-data enabled-info)
  "Apply parsed ENABLED-INFO to STATE-DATA, enabling SM.
Return updated STATE-DATA."
  (setq state-data (plist-put state-data :sm-enabled t))
  ;; Only store session ID when the server actually granted resumption.
  ;; Without this, an unexpected disconnect would attempt resume against
  ;; a server that only supports acking, skipping MUC cleanup.
  (when (plist-get enabled-info :resume)
    (setq state-data (plist-put state-data :sm-id (plist-get enabled-info :id))))
  (when (plist-get enabled-info :max)
    (setq state-data (plist-put state-data :sm-resume-max
                                (plist-get enabled-info :max))))
  state-data)

;;; Resume handling

(defun jabber-sm--handle-resumed (state-data stanza)
  "Process <resumed/> STANZA against STATE-DATA after stream resumption.
Prune the outbound queue per the server's h value.
Return (UPDATED-STATE-DATA . STANZAS-TO-RESEND)."
  (let* ((h (string-to-number (or (jabber-xml-get-attribute stanza 'h) "0")))
         (queue (plist-get state-data :sm-outbound-queue))
         (pruned (jabber-sm--prune-queue queue h))
         (to-resend (mapcar #'cdr pruned)))
    (setq state-data (plist-put state-data :sm-last-acked h))
    (setq state-data (plist-put state-data :sm-outbound-count h))
    (setq state-data (plist-put state-data :sm-outbound-queue nil))
    (setq state-data (plist-put state-data :sm-resumed t))
    (setq state-data (plist-put state-data :sm-resuming nil))
    (cons state-data to-resend)))

;;; Periodic ack request timer

(defun jabber-sm--r-timer-function (jc)
  "Timer callback: send <r/> if JC is still connected."
  (when (memq jc jabber-connections)
    (condition-case nil
        (jabber-sm--request-ack jc)
      (error nil))))

(defun jabber-sm--start-r-timer (jc state-data)
  "Start a periodic <r/> timer for connection JC.
Return updated STATE-DATA with the timer stored."
  (jabber-sm--stop-r-timer state-data)
  (let ((timer (run-with-timer jabber-sm-request-interval
                               jabber-sm-request-interval
                               #'jabber-sm--r-timer-function jc)))
    (plist-put state-data :sm-r-timer timer)))

(defun jabber-sm--stop-r-timer (state-data)
  "Cancel the periodic <r/> timer if running.
Return updated STATE-DATA."
  (let ((timer (plist-get state-data :sm-r-timer)))
    (when (timerp timer)
      (cancel-timer timer)))
  (plist-put state-data :sm-r-timer nil))

;;; FSM routing helper

(defun jabber-sm--maybe-enable-or-establish (state-data)
  "Return FSM transition for STATE-DATA to :sm-enable or :session-established.
Checks `jabber-sm-enable' and whether stream features include SM."
  (if (and jabber-sm-enable
           (jabber-sm--features-have-sm-p state-data))
      (list :sm-enable state-data)
    (list :session-established state-data)))

;;; Post-connect hook entry point

(defun jabber-sm-maybe-start (jc)
  "Start SM ack timer if SM was successfully enabled on JC.
Intended for `jabber-post-connect-hooks'."
  (let ((state-data (fsm-get-state-data jc)))
    (when (plist-get state-data :sm-enabled)
      (jabber-sm--start-r-timer jc state-data))))

(provide 'jabber-sm)

;;; jabber-sm.el ends here
