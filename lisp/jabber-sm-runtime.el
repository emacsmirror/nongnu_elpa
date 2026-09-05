;;; jabber-sm-runtime.el --- Stream Management effects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module contains the network and timer effects for XEP-0198.  State
;; transitions and XML transformations remain in `jabber-sm'.

;;; Code:

(require 'cl-lib)
(require 'fsm)
(require 'jabber-sm)
(require 'jabber-stanza)

(defun jabber-sm--take-pending (state-data)
  "Return STATE-DATA and its pending entries with queue ownership detached."
  (let ((entries (plist-get state-data :sm-pending-queue)))
    (list (plist-put state-data :sm-pending-queue nil) entries)))

(defun jabber-sm--fail-pending (entries reason)
  "Fail ENTRIES independently with REASON."
  (dolist (entry entries)
    (when (keywordp (car-safe entry))
      (when (fboundp 'jabber-omemo--move-echo)
        (jabber-omemo--move-echo entry nil))
      (when (functionp (plist-get entry :failure))
        (jabber-sm--run-pending-callback
         (plist-get entry :failure) reason)))))

(defun jabber-sm--discard-pending (state-data reason)
  "Detach and fail pending entries in STATE-DATA with REASON."
  (pcase-let ((`(,detached ,entries)
               (jabber-sm--take-pending state-data)))
    (jabber-sm--fail-pending entries reason)
    detached))

(defun jabber-sm--count-inbound (jc state-data stanza)
  "Record inbound STANZA and send a periodic acknowledgement when due.
JC is the Jabber connection.  Return updated STATE-DATA."
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

(defun jabber-sm--drain-owner-p (jc state-data)
  "Return non-nil when JC still owns active STATE-DATA."
  (and (eq (get jc :state) :session-established)
       (eq (fsm-get-state-data jc) state-data)
       (memq jc jabber-connections)
       (plist-get state-data :connection)
       (not (plist-get state-data :terminalized))
       (not (plist-get state-data :disconnection-expected))))

(defun jabber-sm--prepare-drain-state (state-data)
  "Return a copy of STATE-DATA with ordinary pending work stably sorted."
  (let ((state-data (copy-sequence state-data)))
    (plist-put
     state-data :sm-pending-queue
     (if (plist-get state-data :sm-fresh-recovery)
         (plist-get state-data :sm-pending-queue)
       (cl-stable-sort
        (copy-sequence (plist-get state-data :sm-pending-queue))
        (lambda (a b)
          (< (jabber-sm--pending-priority a)
             (jabber-sm--pending-priority b))))))))

(defun jabber-sm--next-drain-entry (state-data)
  "Return the next drain entry descriptor from STATE-DATA."
  (let ((recovered (plist-get state-data :sm-recovered-queue))
        (pending (cl-remove-if
                  (lambda (entry)
                    (jabber-sm--entry-blocked-p state-data entry))
                  (plist-get state-data :sm-pending-queue))))
    (cond
     (recovered
      (let ((entry (car recovered)))
        (list :sm-recovered-queue entry
              (if (keywordp (car-safe entry))
                  (jabber-sm--pending-stanza entry) entry) nil)))
     ((and pending
           (or (not (plist-get state-data :sm-enabled))
               (null jabber-sm-max-in-flight)
               (< (jabber-sm--in-flight-count state-data)
                  jabber-sm-max-in-flight)))
      (let ((entry (car pending)))
        (list :sm-pending-queue entry
              (jabber-sm--pending-stanza entry)
              (and (keywordp (car-safe entry))
                   (plist-get entry :success))))))))

(defun jabber-sm--commit-drain-entry (jc state-data queue-key entry sexp)
  "Commit one sent ENTRY from QUEUE-KEY when JC still owns STATE-DATA."
  (when (and (jabber-sm--drain-owner-p jc state-data)
             (memq entry (plist-get state-data queue-key)))
    (let ((next (copy-sequence state-data)))
      (setq next
            (plist-put next queue-key
                       (cl-delete entry (copy-sequence
                                         (plist-get state-data queue-key))
                                  :test #'eq :count 1)))
      (setq next
            (plist-put next :sm-outbound-queue
                       (copy-sequence
                        (plist-get state-data :sm-outbound-queue))))
      (setq next (jabber-sm--count-outbound
                  next sexp (and (keywordp (car-safe entry)) entry)))
      (put jc :state-data next)
      next)))

(defun jabber-sm--drain-pending (jc state-data)
  "Drain work owned by JC under the exact STATE-DATA lease."
  (when (jabber-sm--drain-owner-p jc state-data)
    (setq state-data (jabber-sm--prepare-drain-state state-data))
    (put jc :state-data state-data)
    (let ((continue t))
      (while (and continue (jabber-sm--drain-owner-p jc state-data))
        (if-let* ((descriptor (jabber-sm--next-drain-entry state-data)))
            (pcase-let ((`(,queue-key ,entry ,sexp ,success) descriptor)
                        (connection (plist-get state-data :connection)))
              (condition-case nil
                  (progn
                    (jabber-send-sexp--raw jc sexp)
                    (if-let* ((next (jabber-sm--commit-drain-entry
                                     jc state-data queue-key entry sexp)))
                        (progn
                          (setq state-data next)
                          (jabber-sm--run-pending-callback success)
                          (unless (jabber-sm--drain-owner-p jc state-data)
                            (setq continue nil)))
                      (setq continue nil)))
                ((error quit)
                 (message "SM: queue drain write failed")
                 (setq continue nil)
                 (when (and (jabber-sm--drain-owner-p jc state-data)
                            (eq connection (plist-get state-data :connection)))
                   (fsm-send jc (list :connection-dead connection
                                      "Stream Management replay write failed"))))))
          (setq continue nil))))))

(defun jabber-sm--schedule-drain (jc state-data)
  "Schedule a top-level queue drain for JC and exact STATE-DATA."
  (run-at-time 0 nil #'jabber-sm--drain-pending jc state-data))

(defun jabber-sm--check-stall (jc)
  "Check JC for an acknowledgement stall and recover when timed out."
  (let ((state-data (fsm-get-state-data jc)))
    (if (and jabber-sm-max-in-flight
             (plist-get state-data :sm-pending-queue)
             (>= (jabber-sm--in-flight-count state-data)
                 jabber-sm-max-in-flight))
        (let ((stall-since (plist-get state-data :sm-stall-since)))
          (if stall-since
              (when (>= (- (float-time) stall-since)
                        jabber-sm-stall-timeout)
                (jabber-sm--recover-stall jc state-data))
            (plist-put state-data :sm-stall-since (float-time))))
      (plist-put state-data :sm-stall-since nil))))

(defun jabber-sm--recover-stall (jc state-data)
  "Reconnect JC after an acknowledgement stall in STATE-DATA."
  (let ((pending-count (length (plist-get state-data :sm-pending-queue))))
    (message "SM: ack stall detected, reconnecting (%d stanzas pending)"
             pending-count)
    (plist-put state-data :disconnection-reason
               "Stream Management acknowledgement timeout")
    (if-let* ((connection (plist-get state-data :connection))
              ((processp connection)))
        (delete-process connection)
      (fsm-send jc (list :connection-dead connection
                         "Stream Management acknowledgement timeout")))))

(defun jabber-sm--send-count-too-high-error (jc h sent)
  "Reject on JC an acknowledgement H beyond SENT."
  (jabber-send-string
   jc
   (format
    (concat "<stream:error>"
            "<undefined-condition xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>"
            "<handled-count-too-high xmlns='%s' h='%d' send-count='%d'/>"
            "</stream:error>")
    jabber-sm-xmlns h sent)))

(defun jabber-sm--send-ack (jc state-data)
  "Send an acknowledgement to JC using STATE-DATA."
  (jabber-send-string jc (jabber-sm--make-ack-xml
                          (plist-get state-data :sm-inbound-count))))

(defun jabber-sm--request-ack (jc)
  "Request an acknowledgement from JC."
  (jabber-send-string jc (jabber-sm--make-request-xml)))

(defun jabber-sm--r-timer-function (jc)
  "Request an acknowledgement from JC and check for a stall."
  (when (memq jc jabber-connections)
    (condition-case err
        (progn
          (jabber-sm--request-ack jc)
          (jabber-sm--check-stall jc))
      (error
       (message "SM: ack timer failed: %s" (error-message-string err))))))

(defun jabber-sm--start-r-timer (jc state-data)
  "Start the acknowledgement request timer for JC in STATE-DATA."
  (jabber-sm--stop-r-timer state-data)
  (let ((timer (run-with-timer jabber-sm-request-interval
                               jabber-sm-request-interval
                               #'jabber-sm--r-timer-function jc)))
    (plist-put state-data :sm-r-timer timer)))

(defun jabber-sm--stop-r-timer (state-data)
  "Cancel the acknowledgement request timer in STATE-DATA."
  (let ((timer (plist-get state-data :sm-r-timer)))
    (when (timerp timer)
      (cancel-timer timer)))
  (plist-put state-data :sm-r-timer nil))

(defun jabber-sm-maybe-start (jc)
  "Start the Stream Management acknowledgement timer for JC when enabled."
  (let ((state-data (fsm-get-state-data jc)))
    (when (plist-get state-data :sm-enabled)
      (jabber-sm--start-r-timer jc state-data))))

(provide 'jabber-sm-runtime)

;;; jabber-sm-runtime.el ends here
