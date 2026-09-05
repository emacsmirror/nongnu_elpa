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
;; SM state is stored on the FSM state-data plist.  This module contains the
;; state and XML transformations; `jabber-sm-runtime' owns network and timer
;; effects.

;;; Code:

(require 'cl-lib)
(require 'jabber-jid)
(require 'jabber-state)
(require 'jabber-xml)
(require 'rx)
(require 'subr-x)
(require 'fsm)

(defconst jabber-sm-xmlns "urn:xmpp:sm:3"
  "XEP-0198 Stream Management namespace (version 3).")

(declare-function jabber-omemo--current-echo-key "jabber-omemo" (jc stanza))
(defvar jabber-omemo--sending-echo-key nil)
(defvar jabber-omemo--sending-stanza nil)

(defgroup jabber-sm nil
  "XEP-0198 Stream Management."
  :group 'jabber)

(defcustom jabber-sm-enable t
  "If non-nil, negotiate Stream Management when the server supports it."
  :type 'boolean)

(defcustom jabber-sm-request-interval 30
  "Seconds between periodic ack requests."
  :type 'integer)

(defcustom jabber-sm-ack-interval 50
  "Send a proactive ack every this many inbound stanzas.
When nil, only send acks in response to server <r/> requests."
  :type '(choice (integer :tag "Stanzas between acks")
                 (const :tag "Only on request" nil)))

(defcustom jabber-sm-max-in-flight 40
  "Maximum number of unacknowledged outbound stanzas before queuing.
When the in-flight count reaches this limit, further stanzas are
queued and drained as the server acknowledges previous ones.
Set to nil to disable back-pressure (send everything immediately)."
  :type '(choice (integer :tag "Max unacked stanzas")
                 (const :tag "No limit" nil)))

(defcustom jabber-sm-stall-timeout 90
  "Seconds before reconnecting after an SM acknowledgement stall.
When the server has not acknowledged outbound stanzas for this
many seconds while the pending queue is non-empty, reconnect so
Stream Management can resume without discarding unacknowledged data."
  :type 'integer
  :group 'jabber-sm)

(define-error 'jabber-sm-protocol-error
  "Invalid Stream Management acknowledgement")
(define-error 'jabber-sm-handled-count-too-high
  "Server acknowledged more stanzas than were sent"
  'jabber-sm-protocol-error)
(define-error 'jabber-sm-invalid-acknowledgement
  "Invalid Stream Management acknowledgement"
  'jabber-sm-protocol-error)

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

(defun jabber-sm--parse-handled-count (stanza &optional optional)
  "Return STANZA's unsignedInt h value.
When OPTIONAL is non-nil, return nil if h is absent."
  (let* ((raw (jabber-xml-get-attribute stanza 'h))
         (text (and raw (string-trim raw))))
    (cond
     ((and optional (null raw)) nil)
     ((not (and text
                (string-match-p
                 (rx string-start (? "+") (+ (in "0-9")) string-end)
                 text)))
      (signal 'jabber-sm-invalid-acknowledgement (list raw)))
     (t
      (let ((h (string-to-number text)))
        (if (< h jabber-sm--counter-max)
            h
          (signal 'jabber-sm-invalid-acknowledgement (list raw))))))))

(defun jabber-sm--handled-count-status (state-data h)
  "Classify H against STATE-DATA as :current, :forward, or :stale.
Signal `jabber-sm-handled-count-too-high' for a forward over-ack."
  (let* ((last (plist-get state-data :sm-last-acked))
         (sent (plist-get state-data :sm-outbound-count))
         (handled-delta (jabber-sm--counter-delta h last))
         (in-flight (jabber-sm--counter-delta sent last)))
    (cond
     ((zerop handled-delta) :current)
     ((<= handled-delta in-flight) :forward)
     ((jabber-sm--counter-<= h last) :stale)
     (t (signal 'jabber-sm-handled-count-too-high (list h sent))))))

(defun jabber-sm--apply-handled-count (state-data h &optional reject-stale)
  "Apply validated handled count H to STATE-DATA.
When REJECT-STALE is non-nil, stale evidence is a protocol error."
  (pcase (jabber-sm--handled-count-status state-data h)
    (:current state-data)
    (:stale
     (if reject-stale
         (signal 'jabber-sm-invalid-acknowledgement (list h))
       state-data))
    (:forward
     (let ((queue (jabber-sm--prune-queue
                   (plist-get state-data :sm-outbound-queue) h)))
       (setq state-data (plist-put state-data :sm-last-acked h))
       (setq state-data (plist-put state-data :sm-outbound-queue queue))
       (plist-put state-data :sm-stall-since nil)))))

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
                :sm-fresh-recovery nil
		:sm-id nil
		:sm-resume-max nil
		:sm-outbound-count 0
		:sm-inbound-count 0
		:sm-outbound-queue nil
		:sm-recovered-queue nil
		:sm-pending-queue nil
		:sm-last-acked 0
		:sm-resuming nil
		:sm-resumed nil
		:sm-r-timer nil
		:sm-stall-since nil)
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

(defun jabber-sm--count-outbound (state-data sexp &optional entry)
  "Increment outbound counter and queue SEXP if SM is enabled.
Return updated STATE-DATA.  ENTRY supplies exact replay echo ownership;
transport callbacks are never retained after handoff."
  (when (and (plist-get state-data :sm-enabled)
             (jabber-sm--stanza-p sexp))
    (let ((count (jabber-sm--inc-counter
                  (plist-get state-data :sm-outbound-count))))
      (setq state-data (plist-put state-data :sm-outbound-count count))
      (setq state-data
            (plist-put state-data :sm-outbound-queue
                       (nconc (plist-get state-data :sm-outbound-queue)
                              (list (cons count
                                          (if (equal (jabber-xml-get-attribute sexp 'type)
                                                     "groupchat")
                                              (jabber-sm--pending-entry
                                               sexp nil nil
                                               (list :echo-key
                                                     (if entry
                                                         (plist-get entry :echo-key)
                                                       (and (eq sexp jabber-omemo--sending-stanza)
                                                            jabber-omemo--sending-echo-key))))
                                            sexp))))))))
  state-data)

;;; Back-pressure helpers

(defun jabber-sm--in-flight-count (state-data)
  "Return the number of unacknowledged outbound stanzas in STATE-DATA."
  (jabber-sm--counter-delta (plist-get state-data :sm-outbound-count)
                            (plist-get state-data :sm-last-acked)))

(defun jabber-sm--entry-room (entry)
  "Return the retained room JID owned by ENTRY, or nil."
  (and (keywordp (car-safe entry))
       (plist-get entry :retained-room)))

(defun jabber-sm--room-attempt (state-data room)
  "Return the current room-attempt record for ROOM in STATE-DATA."
  (cdr (assoc room (plist-get state-data :muc-room-attempts))))

(defun jabber-sm--room-ready-p (state-data room)
  "Return non-nil when ROOM in STATE-DATA currently permits handoff."
  (let ((attempt (jabber-sm--room-attempt state-data room)))
    (and attempt
         (eq (plist-get attempt :status) 'ready)
         (eq (plist-get attempt :transport)
             (plist-get state-data :connection))
         (equal (plist-get attempt :session)
                (plist-get state-data :session-id)))))

(defun jabber-sm--entry-blocked-p (state-data entry)
  "Return non-nil when ENTRY's room is not ready in STATE-DATA."
  (let ((room (jabber-sm--entry-room entry)))
    (and room (not (jabber-sm--room-ready-p state-data room)))))

(defun jabber-sm--active-recovery-room-p (state-data room)
  "Return non-nil if ROOM has recovery work or an attempt in STATE-DATA."
  (or (cl-some (lambda (entry)
                 (equal (jabber-sm--entry-room entry) room))
               (plist-get state-data :sm-pending-queue))
      (let ((attempt (jabber-sm--room-attempt state-data room)))
        (and attempt (memq (plist-get attempt :status) '(pending ready))))))

(defun jabber-sm--recovery-join-p (state-data sexp)
  "Return non-nil if SEXP is join/nick presence needed to unblock STATE-DATA."
  (and (eq (jabber-xml-node-name sexp) 'presence)
       (or (jabber-xml-child-with-xmlns sexp "http://jabber.org/protocol/muc")
           (let* ((to (jabber-xml-get-attribute sexp 'to))
                  (attempt (jabber-sm--room-attempt state-data (jabber-jid-user to))))
             (and (null (jabber-xml-get-attribute sexp 'type))
                  (eq (plist-get attempt :status) 'pending)
                  (plist-get attempt :previous)
                  (equal (jabber-jid-resource to) (plist-get attempt :nick)))))
       (jabber-sm--active-recovery-room-p
        state-data (jabber-jid-user (jabber-xml-get-attribute sexp 'to)))))

(defun jabber-sm--blocked-room-p (state-data sexp)
  "Return non-nil when SEXP targets an active recovery room in STATE-DATA."
  (and (equal (jabber-xml-get-attribute sexp 'type) "groupchat")
       (jabber-sm--active-recovery-room-p
        state-data (jabber-xml-get-attribute sexp 'to))))

(defun jabber-sm--should-queue-p (state-data sexp)
  "Return non-nil if SEXP must wait for owned work in STATE-DATA.
Fresh recovery allows bootstrap IQ and presence past blocked rooms.
Required join presence bypasses a full SM window.
Successful resumption preserves all recovered wire order."
  (and (jabber-sm--stanza-p sexp)
       (not (jabber-sm--recovery-join-p state-data sexp))
       (or (and (plist-get state-data :sm-fresh-recovery)
                (eq (jabber-xml-node-name sexp) 'message)
                (plist-get state-data :sm-pending-queue))
           (and (jabber-sm--blocked-room-p state-data sexp)
                (not (jabber-sm--room-ready-p
                      state-data (jabber-xml-get-attribute sexp 'to))))
           (and (plist-get state-data :sm-enabled)
                (or (and (not (plist-get state-data :sm-resuming))
                         (plist-get state-data :sm-recovered-queue))
                    (and jabber-sm-max-in-flight
                         (not (eq (jabber-xml-node-name sexp) 'iq))
                         (>= (jabber-sm--in-flight-count state-data)
                             jabber-sm-max-in-flight)))))))

(defun jabber-sm--stanza-priority (sexp)
  "Return priority for SEXP: 0 for message, 1 for iq, 2 for presence."
  (pcase (jabber-xml-node-name sexp)
    ('message 0)
    ('iq 1)
    (_ 2)))

(defun jabber-sm--pending-entry
    (stanza &optional success-callback failure-callback extra)
  "Return a pending queue entry for STANZA.
SUCCESS-CALLBACK and FAILURE-CALLBACK are optional.  EXTRA plists
are merged after the common fields."
  (append (list :priority (jabber-sm--stanza-priority stanza)
                :stanza stanza
                :success success-callback
                :failure failure-callback)
          extra))

(defun jabber-sm--annotate-retained-entry (entry state-data &optional retain)
  "Return ENTRY with room and echo ownership from STATE-DATA.
When RETAIN is non-nil, associate any groupchat with its room."
  (let ((stanza (jabber-sm--pending-stanza entry)))
    (if (not (equal (jabber-xml-get-attribute stanza 'type) "groupchat"))
        entry
      (let ((annotated
             (if (keywordp (car-safe entry))
                 (copy-sequence entry)
               (jabber-sm--pending-entry stanza))))
        (when (and (not (plist-get annotated :retained-room))
                   (or retain (jabber-sm--blocked-room-p state-data stanza)))
          (setq annotated
                (plist-put annotated :retained-room
                           (jabber-xml-get-attribute stanza 'to))))
        (unless (plist-member annotated :echo-key)
          (setq annotated
                (plist-put annotated :echo-key
                           (or (and (eq stanza jabber-omemo--sending-stanza)
                                    jabber-omemo--sending-echo-key)
                               (when (fboundp 'jabber-omemo--current-echo-key)
                                 (jabber-omemo--current-echo-key
                                  (cl-find-if
                                   (lambda (jc)
                                     (eq (fsm-get-state-data jc) state-data))
                                   jabber-connections) stanza))))))
        annotated))))

(defun jabber-sm--enqueue-pending
    (state-data sexp &optional success-callback failure-callback)
  "Append SEXP to the pending queue in STATE-DATA.
Callbacks run after transport handoff or queue disposal.
Return updated STATE-DATA."
  (plist-put state-data :sm-pending-queue
             (nconc (plist-get state-data :sm-pending-queue)
                    (list
                     (jabber-sm--annotate-retained-entry
                      (if (or success-callback failure-callback
                              (jabber-sm--blocked-room-p state-data sexp))
                          (jabber-sm--pending-entry
                           sexp success-callback failure-callback)
                        (cons (jabber-sm--stanza-priority sexp) sexp))
                      state-data)))))

(defun jabber-sm--pending-priority (entry)
  "Return the priority stored in pending ENTRY."
  (if (keywordp (car-safe entry))
      (plist-get entry :priority)
    (car entry)))

(defun jabber-sm--pending-stanza (entry)
  "Return the stanza stored in pending ENTRY."
  (if (keywordp (car-safe entry))
      (plist-get entry :stanza)
    (cdr entry)))

;;; Ack send/receive

(defun jabber-sm--make-ack-xml (h)
  "Return the XML string for <a h='H' xmlns='urn:xmpp:sm:3'/>."
  (format "<a xmlns='%s' h='%d'/>" jabber-sm-xmlns h))

(defun jabber-sm--make-request-xml ()
  "Return the XML string for <r xmlns='urn:xmpp:sm:3'/>."
  (format "<r xmlns='%s'/>" jabber-sm-xmlns))

(defun jabber-sm--prune-queue (queue h)
  "Return QUEUE with entries whose count is <= H removed."
  (cl-remove-if (lambda (entry)
                  (jabber-sm--counter-<= (car entry) h))
                queue))

(defun jabber-sm--process-ack (state-data stanza)
  "Process an incoming <a/> ack STANZA, pruning the outbound queue.
Only advance `:sm-last-acked' forward -- ignore stale acks whose h
is at or behind the current value.  Signal `jabber-sm-protocol-error'
for malformed or impossible acknowledgements.  Return updated STATE-DATA."
  (jabber-sm--apply-handled-count
   state-data (jabber-sm--parse-handled-count stanza)))

;;; Enable/resume XML generation

(defun jabber-sm--make-enable-xml ()
  "Return the XML string for <enable resume='true' xmlns='urn:xmpp:sm:3'/>."
  (format "<enable xmlns='%s' resume='true'/>" jabber-sm-xmlns))

(defun jabber-sm--make-resume-xml (h previd)
  "Return the XML string for <resume h='H' previd='PREVID'/>."
  (format "<resume xmlns='%s' h='%d' previd='%s'/>"
          jabber-sm-xmlns h (jabber-escape-xml previd)))

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
Move unacknowledged stanzas into the recovered partition in wire order.
Return updated state data."
  (unless (and (jabber-xml-get-attribute stanza 'previd)
               (equal (jabber-xml-get-attribute stanza 'previd)
                      (plist-get state-data :sm-id)))
    (signal 'jabber-sm-invalid-acknowledgement
            (list (jabber-xml-get-attribute stanza 'previd))))
  (let* ((state-data (copy-sequence state-data))
         (h (jabber-sm--parse-handled-count stanza))
         (state-data (jabber-sm--apply-handled-count state-data h t))
         (recovered
          (append (mapcar #'cdr
                          (plist-get state-data :sm-outbound-queue))
                  (plist-get state-data :sm-recovered-queue))))
    (setq state-data (plist-put state-data :sm-last-acked h))
    (setq state-data (plist-put state-data :sm-outbound-count h))
    (setq state-data (plist-put state-data :sm-outbound-queue nil))
    (setq state-data
          (plist-put state-data :sm-recovered-queue recovered))
    (setq state-data (plist-put state-data :sm-resumed t))
    (setq state-data (plist-put state-data :sm-resuming nil))
    state-data))

(defun jabber-sm--handle-failed-resume (state-data stanza)
  "Prepare STATE-DATA for a new session after failed resume STANZA.
Preserve stanzas the server did not acknowledge and existing
pending entries."
  (let ((h (jabber-sm--parse-handled-count stanza t)))
    (when h
      (setq state-data (jabber-sm--apply-handled-count state-data h))))
  (let* ((stanzas (append (mapcar #'cdr
                                 (plist-get state-data :sm-outbound-queue))
                          (plist-get state-data :sm-recovered-queue)))
         (entries (append
                   (mapcar (lambda (stanza)
                             (if (keywordp (car-safe stanza)) stanza
                               (cons (jabber-sm--stanza-priority stanza) stanza)))
                           stanzas)
                   (plist-get state-data :sm-pending-queue)))
         (retained
          (mapcar (lambda (entry)
                    (jabber-sm--annotate-retained-entry entry state-data t))
                  entries))
         (state-data (jabber-sm--reset (copy-sequence state-data))))
    (setq state-data (plist-put state-data :sm-fresh-recovery t))
    (plist-put state-data :sm-pending-queue retained)))

;;; FSM routing helper

(defun jabber-sm--maybe-enable-or-establish (state-data)
  "Return FSM transition for STATE-DATA to :sm-enable or :session-established.
Checks `jabber-sm-enable' and whether stream features include SM."
  (if (and jabber-sm-enable
           (jabber-sm--features-have-sm-p state-data))
      (list :sm-enable state-data)
    (list :session-established state-data)))

(provide 'jabber-sm)

;;; jabber-sm.el ends here
