;;; jabber-receipts.el --- Delivery receipts and chat markers  -*- lexical-binding: t; -*-

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

;; XEP-0184 Message Delivery Receipts and XEP-0333 Chat Markers for
;; 1:1 chats.  Outgoing messages get <request/> and <markable/>
;; elements.  Incoming receipts update the DB and header-line.
;; Optionally sends <received/> and <displayed/> back.

;;; Code:

(require 'jabber-xml)
(require 'jabber-core)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-disco)

(declare-function jabber-chat-ewoc-find-by-id "jabber-chatbuffer" (stanza-id))
(declare-function jabber-jid-user "jabber-util" (jid))
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el

(defgroup jabber-receipts nil
  "Message delivery receipts (XEP-0184) and chat markers (XEP-0333)."
  :group 'jabber-chat)

(defconst jabber-receipts-xmlns "urn:xmpp:receipts"
  "XML namespace for XEP-0184 Message Delivery Receipts.")

(defconst jabber-chat-markers-xmlns "urn:xmpp:chat-markers:0"
  "XML namespace for XEP-0333 Chat Markers.")

(defcustom jabber-chat-send-receipts t
  "Send delivery receipts and read markers to peers.
When non-nil, send <received/> on message delivery and
<displayed/> when a chat buffer becomes visible.
Incoming receipts are always processed regardless of this setting."
  :type 'boolean
  :group 'jabber-receipts)

(defface jabber-chat-delivered
  '((t :inherit shadow :slant italic))
  "Face for delivery receipt status in header-line."
  :group 'jabber-receipts)

(defface jabber-chat-seen
  '((t :inherit success :slant italic))
  "Face for seen/displayed status in header-line."
  :group 'jabber-receipts)

(defvar-local jabber-chat-receipt-message ""
  "Header-line string showing receipt status for current chat.")

(defvar-local jabber-receipts--pending-displayed-id nil
  "Stanza ID of latest unread markable message in this buffer.")

;;; Send hook

(defun jabber-receipts--send-hook (_body _id)
  "Add receipt request and markable elements to outgoing messages.
Added to `jabber-chat-send-hooks'."
  `((request ((xmlns . ,jabber-receipts-xmlns)))
    (markable ((xmlns . ,jabber-chat-markers-xmlns)))))

(add-hook 'jabber-chat-send-hooks #'jabber-receipts--send-hook)

;;; Receive handler

(defun jabber-receipts--handle-message (jc xml-data)
  "Process incoming delivery receipts and chat markers in XML-DATA.
JC is the connection.  Added to `jabber-message-chain'."
  (let ((from (jabber-xml-get-attribute xml-data 'from)))
    ;; XEP-0184: <received xmlns='urn:xmpp:receipts' id='...'/>
    (when-let* ((received (jabber-xml-child-with-xmlns
                           xml-data jabber-receipts-xmlns))
                ((eq (jabber-xml-node-name received) 'received))
                (ref-id (jabber-xml-get-attribute received 'id)))
      (jabber-receipts--update-status jc from ref-id "delivered_at"))
    ;; XEP-0333: <displayed xmlns='urn:xmpp:chat-markers:0' id='...'/>
    (when-let* ((marker (jabber-xml-child-with-xmlns
                         xml-data jabber-chat-markers-xmlns))
                ((eq (jabber-xml-node-name marker) 'displayed))
                (ref-id (jabber-xml-get-attribute marker 'id)))
      (jabber-receipts--update-status jc from ref-id "displayed_at"))
    ;; Send <received/> back if the message requests it
    (let ((id (jabber-xml-get-attribute xml-data 'id)))
      (when (and jabber-chat-send-receipts
                 id
                 (jabber-xml-get-children xml-data 'body)
                 (or (jabber-xml-child-with-xmlns
                      xml-data jabber-receipts-xmlns)
                     (jabber-xml-child-with-xmlns
                      xml-data jabber-chat-markers-xmlns)))
        (jabber-send-sexp-if-connected
         jc `(message ((to . ,from) (type . "chat"))
                      (received ((xmlns . ,jabber-receipts-xmlns)
                                 (id . ,id)))))))
    ;; Track pending markable message for <displayed/> on visibility
    (when-let* ((id (jabber-xml-get-attribute xml-data 'id))
                ((jabber-xml-get-children xml-data 'body))
                ((jabber-xml-child-with-xmlns
                  xml-data jabber-chat-markers-xmlns)))
      (when-let* ((buffer (get-buffer
                           (jabber-chat-get-buffer from jc))))
        (with-current-buffer buffer
          (setq jabber-receipts--pending-displayed-id id))))))

(defun jabber-receipts--update-status (jc from ref-id column)
  "Update receipt status for message REF-ID from FROM on JC.
COLUMN is \"delivered_at\" or \"displayed_at\"."
  (let ((timestamp (floor (float-time)))
        (status (if (string= column "displayed_at") :displayed :delivered)))
    (jabber-db-update-receipt ref-id column timestamp)
    (when-let* ((buffer (get-buffer (jabber-chat-get-buffer from jc))))
      (with-current-buffer buffer
        (jabber-receipts--update-header-line column timestamp)
        (when-let* ((node (jabber-chat-ewoc-find-by-id ref-id)))
          (let ((msg (cadr (ewoc-data node)))
                (inhibit-read-only t))
            (plist-put msg :status status)
            (ewoc-invalidate jabber-chat-ewoc node)
            (when (string= column "displayed_at")
              (jabber-receipts--cascade-displayed node)
              (when-let* ((msg-ts (plist-get msg :timestamp)))
                (jabber-db-cascade-displayed
                 (jabber-connection-bare-jid jc)
                 (jabber-jid-user from)
                 timestamp
                 (floor (float-time msg-ts)))))))))))

(defun jabber-receipts--cascade-displayed (node)
  "Walk backward from NODE, promoting :delivered nodes to :displayed.
Per XEP-0333, a <displayed/> marker implies all prior messages were
also seen.  Only promotes :local nodes whose :status is :delivered."
  (let ((prev (ewoc-prev jabber-chat-ewoc node))
        (inhibit-read-only t))
    (while prev
      (let* ((data (ewoc-data prev))
             (type (car data))
             (msg (cadr data)))
        (cond
         ((and (eq type :local)
               (eq (plist-get msg :status) :delivered))
          (plist-put msg :status :displayed)
          (ewoc-invalidate jabber-chat-ewoc prev))
         ((and (eq type :local)
               (eq (plist-get msg :status) :displayed))
          (setq prev nil))))              ; stop, already cascaded
      (when prev
        (setq prev (ewoc-prev jabber-chat-ewoc prev))))))

(defun jabber-receipts--update-header-line (column timestamp)
  "Update `jabber-chat-receipt-message' for COLUMN at TIMESTAMP."
  (let* ((time-str (format-time-string "%H:%M" timestamp))
         (is-seen (string= column "displayed_at"))
         (label (if is-seen "seen" "delivered"))
         (face (if is-seen 'jabber-chat-seen 'jabber-chat-delivered)))
    (setq jabber-chat-receipt-message
          (propertize (format " %s %s" label time-str) 'face face))
    (force-mode-line-update)))

(add-to-list 'jabber-message-chain #'jabber-receipts--handle-message t)

;;; Display marker on buffer visibility

(defun jabber-receipts--on-window-change ()
  "Send displayed marker when chat buffer becomes visible."
  (when (and jabber-chat-send-receipts
             (derived-mode-p 'jabber-chat-mode)
             jabber-receipts--pending-displayed-id
             jabber-chatting-with
             (get-buffer-window (current-buffer) 'visible))
    (jabber-send-sexp-if-connected
     jabber-buffer-connection
     `(message ((to . ,jabber-chatting-with) (type . "chat"))
               (displayed ((xmlns . ,jabber-chat-markers-xmlns)
                           (id . ,jabber-receipts--pending-displayed-id)))))
    (setq jabber-receipts--pending-displayed-id nil)))

(add-hook 'window-configuration-change-hook #'jabber-receipts--on-window-change)

;;; Disco feature advertisement

(jabber-disco-advertise-feature jabber-receipts-xmlns)
(jabber-disco-advertise-feature jabber-chat-markers-xmlns)

(provide 'jabber-receipts)

;;; jabber-receipts.el ends here
