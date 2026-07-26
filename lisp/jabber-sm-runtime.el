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

(require 'fsm)
(require 'jabber-sm)
(require 'jabber-stanza)

(defun jabber-sm--discard-pending (state-data reason)
  "Fail pending entries in STATE-DATA with REASON, then clear them."
  (dolist (entry (plist-get state-data :sm-pending-queue))
    (when (keywordp (car-safe entry))
      (jabber-sm--run-pending-callback
       (plist-get entry :failure) reason)))
  (plist-put state-data :sm-pending-queue nil))

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

(defun jabber-sm--drain-pending (jc state-data)
  "Send queued stanzas on JC up to the in-flight limit.
STATE-DATA is the FSM plist.  Return updated state data."
  (let ((queue (sort (plist-get state-data :sm-pending-queue)
                     (lambda (a b)
                       (< (jabber-sm--pending-priority a)
                          (jabber-sm--pending-priority b)))))
        failed)
    (while (and queue
                (not failed)
                (or (null jabber-sm-max-in-flight)
                    (< (jabber-sm--in-flight-count state-data)
                       jabber-sm-max-in-flight)))
      (let* ((entry (pop queue))
             (sexp (jabber-sm--pending-stanza entry)))
        (condition-case err
            (progn
              (jabber-send-sexp--raw jc sexp)
              (setq state-data
                    (jabber-sm--count-outbound state-data sexp))
              (when (keywordp (car-safe entry))
                (jabber-sm--run-pending-callback
                 (plist-get entry :success))))
          (error
           (setq failed t)
           (when (keywordp (car-safe entry))
             (jabber-sm--run-pending-callback
              (plist-get entry :failure)
              (error-message-string err)))))))
    (plist-put state-data :sm-pending-queue queue)))

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
  "Recover an acknowledgement stall on JC with STATE-DATA."
  (let ((pending-count (length (plist-get state-data :sm-pending-queue))))
    (message "SM: ack stall detected, recovering (%d stanzas pending)"
             pending-count)
    (plist-put state-data :sm-last-acked
               (plist-get state-data :sm-outbound-count))
    (plist-put state-data :sm-outbound-queue nil)
    (plist-put state-data :sm-stall-since nil)
    (jabber-sm--drain-pending jc state-data)))

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
