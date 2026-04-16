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
(declare-function jabber-chat-ewoc-invalidate "jabber-chatbuffer" (node))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-jid-resource "jabber-util" (jid))
(declare-function jabber-muc-joined-p "jabber-muc" (group &optional jc))
(declare-function jabber-muc-private-get-buffer "jabber-muc"
                  (group nickname &optional jc))
(declare-function jabber-chat--extract-carbon "jabber-chat" (xml-data))
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
  :type 'boolean)

(defface jabber-chat-delivered
  '((t :inherit shadow :slant italic))
  "Face for delivery receipt status in header-line.")

(defface jabber-chat-seen
  '((t :inherit success :slant italic))
  "Face for seen/displayed status in header-line.")

(defvar-local jabber-chat-receipt-message ""
  "Header-line string showing receipt status for current chat.")

(defvar-local jabber-receipts--pending-displayed-id nil
  "Stanza ID of latest unread markable message in this buffer.")

;;; Send hook

(defun jabber-receipts--send-hook (_body _id)
  "Add receipt request and markable elements to outgoing messages.
Added to `jabber-chat-send-hooks'.
Per XEP-0184, receipt requests are NOT RECOMMENDED in MUC
groupchat because every occupant would respond.  Chat markers
\(XEP-0333) are fine in MUC."
  (if (bound-and-true-p jabber-group)
      ;; MUC groupchat: markable only, no receipt request.
      `((markable ((xmlns . ,jabber-chat-markers-xmlns))))
    `((request ((xmlns . ,jabber-receipts-xmlns)))
      (markable ((xmlns . ,jabber-chat-markers-xmlns))))))

(add-hook 'jabber-chat-send-hooks #'jabber-receipts--send-hook)

;;; Receive handler

(defun jabber-receipts--find-buffer (from jc)
  "Find the chat buffer for FROM on connection JC.
For MUC participant JIDs, look up the MUC private buffer.
For regular JIDs, look up the 1:1 chat buffer."
  (if (and (jabber-jid-resource from)
           (jabber-muc-joined-p (jabber-jid-user from)))
      (get-buffer (jabber-muc-private-get-buffer
                   (jabber-jid-user from) (jabber-jid-resource from) jc))
    (get-buffer (jabber-chat-get-buffer from jc))))

(defun jabber-receipts--effective-stanza (jc xml-data)
  "Return (EFFECTIVE-XML . CARBON-TYPE) for XML-DATA on JC.
CARBON-TYPE is nil (not a carbon), `received', or `sent'.  For carbons,
EFFECTIVE-XML is the inner forwarded message; for non-carbons it is
XML-DATA unchanged.  Returns (nil . invalid) for forged carbons whose
outer from does not match our bare JID (CVE-2017-5589)."
  (let ((carbon (jabber-chat--extract-carbon xml-data)))
    (if (not carbon)
        (cons xml-data nil)
      (let ((outer-from-bare
             (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
            (our-bare (jabber-connection-bare-jid jc)))
        (if (string= outer-from-bare our-bare)
            (cons (cdr carbon) (car carbon))
          (warn "Jabber: dropping forged carbon receipts from %s"
                outer-from-bare)
          (cons nil 'invalid))))))

(defun jabber-receipts--process-incoming-markers (jc effective from)
  "Dispatch incoming markers in EFFECTIVE stanza from FROM.
Handles XEP-0184 <received/>, XEP-0333 <received/> and <displayed/>.
JC is the connection."
  ;; XEP-0184: <received xmlns='urn:xmpp:receipts' id='...'/>
  (when-let* ((received (jabber-xml-child-with-xmlns
                         effective jabber-receipts-xmlns))
              ((eq (jabber-xml-node-name received) 'received))
              (ref-id (jabber-xml-get-attribute received 'id)))
    (jabber-receipts--update-status jc from ref-id "delivered_at"))
  ;; XEP-0333: <received xmlns='urn:xmpp:chat-markers:0' id='...'/>
  ;; Some clients send XEP-0333 received instead of XEP-0184.
  (when-let* ((marker (jabber-xml-child-with-xmlns
                       effective jabber-chat-markers-xmlns))
              ((eq (jabber-xml-node-name marker) 'received))
              (ref-id (jabber-xml-get-attribute marker 'id)))
    (jabber-receipts--update-status jc from ref-id "delivered_at"))
  ;; XEP-0333: <displayed xmlns='urn:xmpp:chat-markers:0' id='...'/>
  (when-let* ((marker (jabber-xml-child-with-xmlns
                       effective jabber-chat-markers-xmlns))
              ((eq (jabber-xml-node-name marker) 'displayed))
              (ref-id (jabber-xml-get-attribute marker 'id)))
    (jabber-receipts--update-status jc from ref-id "displayed_at")))

(defun jabber-receipts--maybe-send-receipt (jc effective from carbon-type)
  "Send XEP-0184 <received/> reply if EFFECTIVE requests it.
Suppressed for carbons (primary resource owns the receipt) and
MAM-replayed messages.  JC is the connection, FROM is the sender,
CARBON-TYPE is nil, `received', or `sent'."
  (let ((id (jabber-xml-get-attribute effective 'id)))
    (when (and jabber-chat-send-receipts
               id
               (null carbon-type)
               (not (jabber-xml-get-attribute effective 'jabber-mam--origin))
               (jabber-xml-get-children effective 'body)
               (let ((req (jabber-xml-child-with-xmlns
                           effective jabber-receipts-xmlns)))
                 (and req (eq (jabber-xml-node-name req) 'request))))
      (jabber-send-sexp-if-connected
       jc `(message ((to . ,from) (type . "chat"))
                    (received ((xmlns . ,jabber-receipts-xmlns)
                               (id . ,id))))))))

(defun jabber-receipts--track-displayed (jc effective from carbon-type)
  "Queue or send <displayed/> for the markable message in EFFECTIVE.
If the chat buffer is visible, send immediately; otherwise queue the
id for `jabber-receipts--on-window-change' to flush later.
Skipped for `sent' carbons (we are the sender) and MAM replays.
JC is the connection, FROM is the sender, CARBON-TYPE is nil,
`received', or `sent'."
  (when-let* ((id (jabber-xml-get-attribute effective 'id))
              ((not (eq carbon-type 'sent)))
              ((not (jabber-xml-get-attribute effective 'jabber-mam--origin)))
              ((jabber-xml-get-children effective 'body))
              ((jabber-xml-child-with-xmlns
                effective jabber-chat-markers-xmlns)))
    (when-let* ((buffer (jabber-receipts--find-buffer from jc)))
      (with-current-buffer buffer
        (when jabber-chat-send-receipts
          (if (get-buffer-window buffer 'visible)
              (progn
                (jabber-send-sexp-if-connected
                 jc `(message ((to . ,from) (type . "chat"))
                              (displayed ((xmlns . ,jabber-chat-markers-xmlns)
                                          (id . ,id)))))
                (setq jabber-receipts--pending-displayed-id nil))
            (setq jabber-receipts--pending-displayed-id id)))))))

(defun jabber-receipts--handle-message (jc xml-data)
  "Process incoming delivery receipts and chat markers in XML-DATA.
JC is the connection.  Added to `jabber-message-chain'.
Unwraps XEP-0280 Message Carbons before dispatching to sub-handlers."
  (let* ((eff (jabber-receipts--effective-stanza jc xml-data))
         (effective (car eff))
         (carbon-type (cdr eff)))
    (when (and effective (not (eq carbon-type 'invalid)))
      (let* ((from (jabber-xml-get-attribute effective 'from))
             (type (jabber-xml-get-attribute effective 'type))
             (groupchat-p (equal type "groupchat")))
        (unless (or (null from) groupchat-p)
          ;; Incoming markers: skip for `sent' carbons (those carry our
          ;; own-device outgoing markers, not peer-to-us markers).
          ;; TODO: cross-device read-sync could use sent-carbon markers
          ;; to locally mark peer messages as read.
          (unless (eq carbon-type 'sent)
            (jabber-receipts--process-incoming-markers jc effective from))
          (jabber-receipts--maybe-send-receipt jc effective from carbon-type)
          (jabber-receipts--track-displayed jc effective from carbon-type))))))

(defvar-local jabber-receipts--latest-displayed-ts 0
  "Timestamp of the most recently displayed outgoing message.
Used to enforce XEP-0333 forward-only rule: displayed markers
referencing older messages are redundant and MUST be ignored.")

(defun jabber-receipts--update-status (jc from ref-id column)
  "Update receipt status for message REF-ID from FROM on JC.
COLUMN is \"delivered_at\" or \"displayed_at\"."
  (let ((timestamp (floor (float-time)))
        (account (jabber-connection-bare-jid jc))
        (peer (jabber-jid-user from))
        (status (if (string= column "displayed_at") :displayed :delivered)))
    (jabber-db-update-receipt account peer ref-id column timestamp)
    (when-let* ((buffer (jabber-receipts--find-buffer from jc)))
      (with-current-buffer buffer
        (when-let* ((node (jabber-chat-ewoc-find-by-id ref-id)))
          (let* ((msg (cadr (ewoc-data node)))
                 (msg-ts (plist-get msg :timestamp))
                 (msg-epoch (and msg-ts (floor (float-time msg-ts))))
                 (inhibit-read-only t))
            ;; XEP-0333: displayed markers for older messages MUST be
            ;; ignored (forward-only rule).
            (when (or (not (string= column "displayed_at"))
                      (not msg-epoch)
                      (> msg-epoch jabber-receipts--latest-displayed-ts))
              (plist-put msg :status status)
              (jabber-chat-ewoc-invalidate node)
              (jabber-receipts--update-header-line column timestamp)
              (when (string= column "displayed_at")
                (when msg-epoch
                  (setq jabber-receipts--latest-displayed-ts msg-epoch))
                (jabber-receipts--cascade-displayed node)
                (when msg-epoch
                  (jabber-db-cascade-displayed
                   account peer timestamp msg-epoch))))))))))

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
          (jabber-chat-ewoc-invalidate prev))
         ((and (eq type :local)
               (eq (plist-get msg :status) :displayed))
          (setq prev nil))))              ; stop, already cascaded
      (when prev
        (setq prev (ewoc-prev jabber-chat-ewoc prev))))))

(defun jabber-receipts--update-header-line (column timestamp)
  "Update `jabber-chat-receipt-message' for COLUMN at TIMESTAMP.
Does not downgrade from \"seen\" to \"delivered\"."
  (let* ((time-str (format-time-string "%H:%M" timestamp))
         (is-seen (string= column "displayed_at"))
         (label (if is-seen "seen" "delivered"))
         (face (if is-seen 'jabber-chat-seen 'jabber-chat-delivered)))
    (unless (and (not is-seen)
                 (string-match-p "seen" jabber-chat-receipt-message))
      (setq jabber-chat-receipt-message
            (propertize (format " %s %s" label time-str) 'face face))
      (force-mode-line-update))))

(jabber-chain-add 'jabber-message-chain #'jabber-receipts--handle-message 50)

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
