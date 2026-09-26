;;; jabber-state.el --- Shared Jabber state  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; State shared by the connection FSM and protocol handlers lives here.  This
;; module has no network, database, or user-interface effects.

;;; Code:

(require 'cl-lib)

(defvar jabber-connections nil
  "List of Jabber connection FSMs.")

(define-obsolete-variable-alias '*jabber-roster*
  'jabber-roster-list "0.11.0")
(defvar jabber-roster-list nil
  "The roster list.")

(defvar jabber-jid-obarray (make-vector 127 0)
  "Legacy accountless JID namespace; connected contacts are private to each FSM.")

(define-obsolete-variable-alias '*jabber-disconnecting*
  'jabber-disconnecting "0.11.0")
(defvar jabber-disconnecting nil
  "Non-nil while voluntarily disconnecting.")

(defvar jabber-message-chain nil
  "Ordered handlers for incoming message stanzas.")

(defvar jabber-iq-chain nil
  "Ordered handlers for incoming IQ stanzas.")

(defvar jabber-presence-chain nil
  "Ordered handlers for incoming presence stanzas.")

;;;###autoload
(defun jabber-chain-add (chain-var handler &optional depth)
  "Add HANDLER to CHAIN-VAR at numeric priority DEPTH.
Lower depths run first.  Bare function entries from older versions are
accepted when checking for an existing handler."
  (let ((entry (cons (or depth 0) handler))
        (entry-depth (lambda (item) (if (consp item) (car item) 0)))
        (entry-function (lambda (item) (if (consp item) (cdr item) item))))
    (unless (cl-find handler (symbol-value chain-var) :key entry-function)
      (set chain-var
           (sort (cons entry (symbol-value chain-var))
                 (lambda (a b)
                   (< (funcall entry-depth a)
                      (funcall entry-depth b))))))))

(defun jabber-clear-roster (&optional jc)
  "Clear contact metadata belonging to JC.
Without JC, clear every currently registered connection and legacy state.
Retain scoped symbol identities and their owner for existing hook arguments."
  (if jc
      (when-let* ((table (get jc 'jabber--jid-obarray)))
        (mapatoms (lambda (jid)
                    (setplist jid (list 'jabber--jid-owner jc)))
                  table))
    (mapc #'jabber-clear-roster jabber-connections)
    (mapatoms (lambda (jid) (unintern jid jabber-jid-obarray))
              jabber-jid-obarray)
    (setq jabber-roster-list nil)))

(provide 'jabber-state)

;;; jabber-state.el ends here
