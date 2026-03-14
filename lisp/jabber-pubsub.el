;;; jabber-pubsub.el --- XEP-0060: Publish-Subscribe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Implementation of XEP-0060 (Publish-Subscribe) core operations:
;; publish, retract, request items, delete node, configure node, and
;; event notification dispatch.  Used by OMEMO (key distribution) and
;; bookmarks sync (XEP-0402).

;;; Code:

(require 'jabber-iq)
(require 'jabber-xml)
(require 'jabber-util)
(require 'jabber-disco)

;; Global reference declarations

(defvar jabber-message-chain)           ; jabber-core.el

;;; xmlns constants

(defconst jabber-pubsub-xmlns "http://jabber.org/protocol/pubsub"
  "XEP-0060: Publish-Subscribe.")

(defconst jabber-pubsub-owner-xmlns "http://jabber.org/protocol/pubsub#owner"
  "XEP-0060: Publish-Subscribe (owner operations).")

(defconst jabber-pubsub-event-xmlns "http://jabber.org/protocol/pubsub#event"
  "XEP-0060: Publish-Subscribe (event notifications).")

;;; Publish-options helper

(defun jabber-pubsub--publish-options (options)
  "Build a <publish-options> XML sexp from OPTIONS alist.
Each element is (VAR . VALUE)."
  `(publish-options ()
    (x ((xmlns . ,jabber-xdata-xmlns) (type . "submit"))
       (field ((var . "FORM_TYPE") (type . "hidden"))
              (value () "http://jabber.org/protocol/pubsub#publish-options"))
       ,@(mapcar (lambda (opt)
                   `(field ((var . ,(car opt))) (value () ,(cdr opt))))
                 options))))

;;; Core operations

(defun jabber-pubsub-publish (jc jid node item-id payload
                                 &optional options callback error-callback)
  "Publish PAYLOAD to NODE on JID via JC.
ITEM-ID is the item identifier.  OPTIONS, if non-nil, is an alist
of publish-options (VAR . VALUE).  CALLBACK and ERROR-CALLBACK are
called as (funcall cb JC XML-DATA CLOSURE-DATA)."
  (let ((query `(pubsub ((xmlns . ,jabber-pubsub-xmlns))
              (publish ((node . ,node))
                       (item ((id . ,item-id))
                             ,payload))
              ,@(when options
                  (list (jabber-pubsub--publish-options options))))))
    (jabber-send-iq jc jid "set" query
                    callback "pubsub publish"
                    error-callback "pubsub publish")))

(defun jabber-pubsub-retract (jc jid node item-id
                                 &optional notify callback error-callback)
  "Retract ITEM-ID from NODE on JID via JC.
When NOTIFY is non-nil, add notify=\"true\" to the retract element.
CALLBACK and ERROR-CALLBACK follow `jabber-send-iq' conventions."
  (jabber-send-iq jc jid "set"
                  `(pubsub ((xmlns . ,jabber-pubsub-xmlns))
                           (retract ((node . ,node)
                                     ,@(when notify '((notify . "true"))))
                                    (item ((id . ,item-id)))))
                  callback "pubsub retract"
                  error-callback "pubsub retract"))

(defun jabber-pubsub-request (jc jid node callback
                                 &optional error-callback)
  "Request items from NODE on JID via JC.
CALLBACK receives the full IQ result; caller extracts items."
  (jabber-send-iq jc jid "get"
                  `(pubsub ((xmlns . ,jabber-pubsub-xmlns))
                           (items ((node . ,node))))
                  callback "pubsub request"
                  error-callback "pubsub request"))

(defun jabber-pubsub-delete-node (jc jid node
                                     &optional callback error-callback)
  "Delete NODE on JID via JC (owner operation).
CALLBACK and ERROR-CALLBACK follow `jabber-send-iq' conventions."
  (jabber-send-iq jc jid "set"
                  `(pubsub ((xmlns . ,jabber-pubsub-owner-xmlns))
                           (delete ((node . ,node))))
                  callback "pubsub delete-node"
                  error-callback "pubsub delete-node"))

(defun jabber-pubsub-configure-node (jc jid node options
                                        &optional callback error-callback)
  "Configure NODE on JID via JC (owner operation).
OPTIONS is an alist of (VAR . VALUE) for the node configuration form."
  (jabber-send-iq jc jid "set"
                  `(pubsub ((xmlns . ,jabber-pubsub-owner-xmlns))
                           (configure ((node . ,node))
                             (x ((xmlns . ,jabber-xdata-xmlns) (type . "submit"))
                                (field ((var . "FORM_TYPE") (type . "hidden"))
                                       (value () "http://jabber.org/protocol/pubsub#node_config"))
                                ,@(mapcar (lambda (opt)
                                            `(field ((var . ,(car opt)))
                                                    (value () ,(cdr opt))))
                                          options))))
                  callback "pubsub configure-node"
                  error-callback "pubsub configure-node"))

;;; Event notification handler

(defvar jabber-pubsub-node-handlers nil
  "Alist of (NODE-NAME . HANDLER) for PubSub event dispatch.
HANDLER is called as (funcall HANDLER JC FROM NODE ITEMS)
where ITEMS is the list of child elements (item or retract).")

(defun jabber-pubsub--process-event (jc xml-data)
  "Process incoming PubSub event notifications.
JC is the Jabber connection.  XML-DATA is the message stanza."
  (let ((event (jabber-xml-child-with-xmlns xml-data jabber-pubsub-event-xmlns)))
    (when event
      (let* ((items-or-purge (or (car (jabber-xml-get-children event 'items))
                                 (car (jabber-xml-get-children event 'purge))))
             (node (and items-or-purge
                        (jabber-xml-get-attribute items-or-purge 'node)))
             (handler (and node (cdr (assoc node jabber-pubsub-node-handlers)))))
        (when handler
          (funcall handler jc
                   (jabber-xml-get-attribute xml-data 'from)
                   node
                   (jabber-xml-node-children items-or-purge)))))))

(with-eval-after-load "jabber-core"
  (add-to-list 'jabber-message-chain #'jabber-pubsub--process-event))

;;; Disco advertisement

(jabber-disco-advertise-feature jabber-pubsub-xmlns)

(provide 'jabber-pubsub)
;;; jabber-pubsub.el ends here
