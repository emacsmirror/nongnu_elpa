;;; jabber-subscription.el --- Presence subscription UI  -*- lexical-binding: t; -*-

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

;; Displays subscription requests in chat buffers and handles replies.

;;; Code:

(require 'ewoc)
(require 'seq)
(require 'jabber-alert)
(require 'jabber-chat)
(require 'jabber-chatbuffer)
(require 'jabber-presence-events)
(require 'jabber-util)

(defun jabber-process-subscription-request (jc from presence-status)
  "Display a subscription request from FROM on JC with PRESENCE-STATUS."
  (with-current-buffer (jabber-chat-create-buffer jc from)
    (jabber-chat-ewoc-enter
     (list :subscription-request presence-status :time (current-time)))
    (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
      (run-hook-with-args
       hook (jabber-jid-symbol from jc) nil "subscribe" presence-status
       (funcall jabber-alert-presence-message-function
                (jabber-jid-symbol from jc) nil "subscribe" presence-status)))))

(defun jabber-subscription-accept-mutual (&rest _ignored)
  "Accept the pending request and request a reciprocal subscription."
  (message "Subscription accepted; reciprocal subscription request sent")
  (jabber-subscription-reply "subscribed" "subscribe"))

(defun jabber-subscription-accept-one-way (&rest _ignored)
  "Accept the pending subscription request without reciprocating."
  (message "Subscription accepted")
  (jabber-subscription-reply "subscribed"))

(defun jabber-subscription-decline (&rest _ignored)
  "Decline the pending subscription request."
  (message "Subscription declined")
  (jabber-subscription-reply "unsubscribed"))

(defun jabber-subscription--remove-prompt ()
  "Remove the subscription request EWOC node at point."
  (when (bound-and-true-p jabber-chat-ewoc)
    (let ((node (ewoc-locate jabber-chat-ewoc)))
      (when (and node (eq :subscription-request (car (ewoc-data node))))
        (jabber-chat-ewoc-delete node)))))

(defun jabber-subscription--remove-stale (jc from)
  "Remove subscription request nodes from FROM's chat on live connection JC."
  (when-let* ((live (memq jc jabber-connections))
              (buffer
               (seq-find
                (lambda (candidate)
                  (and (eq (buffer-local-value 'jabber-buffer-connection
                                                candidate) jc)
                       (jabber-chat--buffer-peer-p candidate from nil)))
                (buffer-list))))
    (with-current-buffer buffer
      (when (bound-and-true-p jabber-chat-ewoc)
        (let ((node (ewoc-nth jabber-chat-ewoc 0))
              to-delete)
          (while node
            (when (eq :subscription-request (car (ewoc-data node)))
              (push node to-delete))
            (setq node (ewoc-next jabber-chat-ewoc node)))
          (mapc #'jabber-chat-ewoc-delete to-delete))))))

(defun jabber-subscription-reply (&rest types)
  "Send one presence stanza per TYPES to the current peer."
  (let ((to (jabber-jid-user jabber-chatting-with)))
    (dolist (type types)
      (jabber-send-sexp
       jabber-buffer-connection
       `(presence ((to . ,to) (type . ,type))))))
  (jabber-subscription--remove-prompt))

(add-hook 'jabber-presence-contact-functions
          #'jabber-subscription--remove-stale)
(add-hook 'jabber-presence-subscription-request-functions
          #'jabber-process-subscription-request)

(provide 'jabber-subscription)
;;; jabber-subscription.el ends here
