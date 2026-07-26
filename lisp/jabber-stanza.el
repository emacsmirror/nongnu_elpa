;;; jabber-stanza.el --- Jabber stanza transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Serialize, transmit, and dispatch XMPP stanzas.  Stream Management state
;; transformations remain in `jabber-sm'; this module applies them at the
;; network boundary.

;;; Code:

(require 'fsm)
(require 'jabber-sm)
(require 'jabber-state)
(require 'jabber-xml)

(defconst jabber-bind-xmlns "urn:ietf:params:xml:ns:xmpp-bind"
  "RFC 6120 resource binding namespace.")

(defconst jabber-session-xmlns "urn:ietf:params:xml:ns:xmpp-session"
  "RFC 6120 session establishment namespace.")

(defconst jabber-streams-xmlns "http://etherx.jabber.org/streams"
  "RFC 6120 XMPP streams namespace.")

(defcustom jabber-debug-log-xml nil
  "Set to non-nil to log XML input and output in a console buffer.
Set to a string to also append XML input and output to that file."
  :type '(choice (const :tag "Do not dump XML input or output" nil)
                 (const :tag "Dump XML in console" t)
                 (string :tag "Dump XML in console and this file"))
  :group 'jabber-debug)

(defvar jabber-stanza-log-function nil
  "Function called with connection, direction, and stanza log data.")

(defun jabber-log-xml (jc direction data)
  "Log DATA for JC in DIRECTION when XML debugging is enabled."
  (when (and jabber-debug-log-xml
             (functionp jabber-stanza-log-function))
    (funcall jabber-stanza-log-function jc direction data)))

(defun jabber-stanza--connection-jid (state-data)
  "Return the full JID represented by STATE-DATA."
  (concat (plist-get state-data :username) "@"
          (plist-get state-data :server) "/"
          (plist-get state-data :resource)))

(defun jabber-send-string (jc string)
  "Send STRING through connection JC."
  (let* ((state-data (fsm-get-state-data jc))
         (connection (plist-get state-data :connection))
         (send-function (plist-get state-data :send-function)))
    (unless connection
      (error "%s has no connection"
             (jabber-stanza--connection-jid state-data)))
    (funcall send-function connection string)))

(defun jabber-send-sexp--raw (jc sexp)
  "Send SEXP to JC without updating Stream Management state."
  (condition-case err
      (jabber-log-xml jc "sending" sexp)
    (error
     (ding)
     (message "Couldn't write XML log: %s" (error-message-string err))
     (sit-for 2)))
  (let* ((xml (jabber-sexp2xml sexp))
         (state-data (fsm-get-state-data jc))
         (sm-countable (and (plist-get state-data :sm-enabled)
                            (jabber-sm--stanza-p sexp))))
    (jabber-send-string
     jc
     (if sm-countable
         (concat xml (jabber-sm--make-request-xml))
       xml))))

(defun jabber-sm--run-pending-callback (callback &optional argument)
  "Run CALLBACK with optional ARGUMENT without disrupting send state."
  (when callback
    (condition-case err
        (if argument
            (funcall callback argument)
          (funcall callback))
      (error
       (message "SM callback failed: %s" (error-message-string err))))))

(defun jabber-send-sexp--immediate
    (jc sexp &optional success-callback failure-callback)
  "Send SEXP on JC, update SM state, and run transport callbacks."
  (condition-case err
      (prog1
          (progn
            (jabber-send-sexp--raw jc sexp)
            (jabber-sm--count-outbound (fsm-get-state-data jc) sexp))
        (jabber-sm--run-pending-callback success-callback))
    (error
     (if failure-callback
         (jabber-sm--run-pending-callback
          failure-callback (error-message-string err))
       (signal (car err) (cdr err))))))

(defun jabber-send-sexp
    (jc sexp &optional success-callback failure-callback)
  "Send SEXP on JC with SM back-pressure and transport callbacks."
  (let ((state-data (fsm-get-state-data jc)))
    (if (jabber-sm--should-queue-p state-data sexp)
        (progn
          (jabber-sm--enqueue-pending
           state-data sexp success-callback failure-callback)
          (when (eq (jabber-xml-node-name sexp) 'message)
            (message "SM: message queued (waiting for server ack, %d pending)"
                     (length (plist-get state-data :sm-pending-queue)))))
      (jabber-send-sexp--immediate
       jc sexp success-callback failure-callback))))

(defun jabber-send-sexp-if-connected (jc sexp)
  "Send SEXP through JC only after its session is established."
  (fsm-send-sync jc (cons :send-if-connected sexp)))

(defvar jabber-use-sasl)                ; jabber-core.el

(defsubst jabber-have-sasl-p ()
  "Return non-nil when the SASL library is available."
  (featurep 'sasl))

(defun jabber-send-stream-header (jc)
  "Send the opening XML stream header to JC."
  (let* ((state-data (fsm-get-state-data jc))
         (header
          (concat "<?xml version='1.0'?><stream:stream to='"
                  (plist-get state-data :server)
                  "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
                  (if (and (jabber-have-sasl-p) jabber-use-sasl)
                      " version='1.0'"
                    "")
                  ">\n")))
    (jabber-log-xml jc "sending" header)
    (jabber-send-string jc header)))

(defvar jabber-xml-data)                ; jabber.el

(defun jabber-process-input (jc xml-data)
  "Dispatch XML-DATA received on JC through its stanza handler chain."
  (let* ((jabber-xml-data xml-data)
         (tag (jabber-xml-node-name xml-data))
         (handlers (pcase tag
                     ('iq jabber-iq-chain)
                     ('presence jabber-presence-chain)
                     ('message jabber-message-chain))))
    (dolist (entry handlers)
      (let ((handler (if (consp entry) (cdr entry) entry)))
        (condition-case err
            (funcall handler jc xml-data)
          ((debug error)
           (fsm-debug-output "Error %S while processing %S with function %s"
                             err xml-data handler)))))))

(provide 'jabber-stanza)

;;; jabber-stanza.el ends here
