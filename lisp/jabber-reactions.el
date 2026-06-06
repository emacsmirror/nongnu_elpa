;;; jabber-reactions.el --- XEP-0444 Message Reactions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; XEP-0444 Message Reactions data helpers and outgoing stanza support.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'jabber-db)
(require 'jabber-disco)
(require 'jabber-util)

(defconst jabber-reactions-xmlns "urn:xmpp:reactions:0"
  "XEP-0444 Message Reactions namespace.")

(defconst jabber-reactions-hints-xmlns "urn:xmpp:hints"
  "XEP-0334 Message Processing Hints namespace.")

(defconst jabber-reactions-fallback-xmlns "urn:xmpp:fallback:0"
  "XEP-0428 Fallback Indication namespace.")

(defcustom jabber-reactions-default-choices
  '("👍" "❤️" "😂" "🎉" "😮" "😢" "🙏")
  "Reaction strings offered by the outgoing reaction picker.
This list is only a default set of picker choices.  Incoming reactions
are not filtered against it."
  :type '(repeat string)
  :group 'jabber)

(defface jabber-reaction
  '((t :inherit shadow))
  "Face for message reaction summaries."
  :group 'jabber)

(defface jabber-reaction-chosen
  '((t :inherit success))
  "Face for message reactions selected by the local user."
  :group 'jabber)

(defvar jabber-buffer-connection)
(defvar jabber-chat-ewoc)
(defvar jabber-chatting-with)
(defvar jabber-group)
(defvar jabber-point-insert)

(declare-function jabber-db-reaction-stale-p
                  "jabber-db" (account peer type target-id sender updated-at))
(declare-function jabber-db-replace-reactions
                  "jabber-db" (account peer type target-id sender reactions
                                        &optional updated-at))
(declare-function jabber-chat--unwrap-carbon "jabber-chat" (jc xml-data))
(declare-function jabber-chat-ewoc-find-by-id "jabber-chatbuffer" (stanza-id))
(declare-function jabber-chat-ewoc-invalidate "jabber-chatbuffer" (node))
(declare-function jabber-chat-find-buffer "jabber-chat" (chat-with))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-muc-nickname "jabber-muc" (group &optional jc))

;;; Pure helpers

(defun jabber-reactions--deduplicate (reactions)
  "Return REACTIONS without duplicates, nils, or empty strings.
The first occurrence of each non-empty string is kept."
  (let ((seen nil)
        (result nil))
    (dolist (reaction reactions (nreverse result))
      (when (and (stringp reaction)
                 (not (string-empty-p reaction))
                 (not (member reaction seen)))
        (push reaction seen)
        (push reaction result)))))

(defun jabber-reactions--target-id (msg muc-p)
  "Return the XEP-0444 target message ID from MSG.
Use :server-id for MUC messages when MUC-P is non-nil, and :id for
1:1 chat messages.  Return nil when the needed ID is unavailable."
  (plist-get msg (if muc-p :server-id :id)))

(defun jabber-reactions--message-attributes (to type outgoing-id)
  "Return message attributes for reaction stanza TO TYPE OUTGOING-ID."
  (append `((to . ,to) (type . ,type))
          (and outgoing-id `((id . ,outgoing-id)))))

(defun jabber-reactions--build-stanza (to type target-id reactions outgoing-id)
  "Build an outgoing XEP-0444 reaction stanza.
TO is the destination JID.  TYPE is the message type string.  TARGET-ID
is the stanza ID of the message being reacted to.  REACTIONS is a list
of selected reaction strings.  OUTGOING-ID is the ID for the outgoing
message stanza, or nil to omit it."
  `(message ,(jabber-reactions--message-attributes to type outgoing-id)
            (reactions ((xmlns . ,jabber-reactions-xmlns)
                        (id . ,target-id))
                       ,@(mapcar (lambda (reaction)
                                   `(reaction () ,reaction))
                                 (jabber-reactions--deduplicate reactions)))
            (store ((xmlns . ,jabber-reactions-hints-xmlns)))))

(defun jabber-reactions--display-entry (reaction senders chosen-sender)
  "Build a display entry for REACTION by SENDERS.
Non-nil CHOSEN-SENDER marks entries selected by that sender."
  (list :reaction reaction
        :count (length senders)
        :chosen (and chosen-sender (member chosen-sender senders) t)
        :senders senders))

(defun jabber-reactions--display-entries (sender-state &optional chosen-sender)
  "Aggregate SENDER-STATE into reaction display entries.
SENDER-STATE is an alist of (SENDER . REACTIONS), where REACTIONS is a
list of reaction strings currently selected by SENDER.  CHOSEN-SENDER is
used to mark locally selected reactions.  Return a list of plists with
:reaction, :count, :chosen, and :senders keys, preserving the first-seen
reaction order."
  (let ((order nil)
        (senders-by-reaction nil))
    (dolist (entry sender-state)
      (let ((sender (car entry)))
        (dolist (reaction (jabber-reactions--deduplicate (cdr entry)))
          (unless (assoc reaction senders-by-reaction)
            (push reaction order))
          (cl-pushnew sender (alist-get reaction senders-by-reaction nil nil #'equal)
                      :test #'equal))))
    (mapcar (lambda (reaction)
              (jabber-reactions--display-entry
               reaction
               (nreverse (alist-get reaction senders-by-reaction nil nil #'equal))
               chosen-sender))
            (nreverse order))))

(defun jabber-reactions--sender-reactions (sender msg)
  "Return SENDER's current reactions from MSG."
  (copy-sequence (alist-get sender (plist-get msg :reactions) nil nil #'equal)))

(defun jabber-reactions--toggle-reaction (reaction reactions)
  "Return REACTIONS with REACTION toggled.
Existing REACTION is removed.  Missing REACTION is appended after the
current deduplicated reaction list."
  (let ((deduplicated (jabber-reactions--deduplicate reactions)))
    (if (member reaction deduplicated)
        (remove reaction deduplicated)
      (append deduplicated (list reaction)))))

(defun jabber-reactions--replace-sender-reactions (sender sender-reactions reactions)
  "Return REACTIONS with SENDER replaced by SENDER-REACTIONS.
When SENDER-REACTIONS is nil, remove SENDER from REACTIONS."
  (let ((without-sender (cl-remove sender reactions :key #'car :test #'equal))
        (deduplicated (jabber-reactions--deduplicate sender-reactions)))
    (if deduplicated
        (cons (cons sender deduplicated) without-sender)
      without-sender)))

(defun jabber-reactions--parse-element (reactions)
  "Return (TARGET-ID REACTIONS) parsed from REACTIONS element.
REACTIONS is a XEP-0444 `<reactions>' XML node.  Empty strings and
repeated reaction strings are ignored.  The reaction list may be nil for
an empty update."
  (when-let* ((target-id (jabber-xml-get-attribute reactions 'id)))
    (list target-id
          (jabber-reactions--deduplicate
           (mapcar (lambda (reaction)
                     (car (jabber-xml-node-children reaction)))
                   (jabber-xml-get-children reactions 'reaction))))))

(defun jabber-reactions--single-element (message)
  "Return MESSAGE's sole XEP-0444 reactions element, or nil."
  (let ((reactions (cl-remove-if-not
                    (lambda (child)
                      (string= (jabber-xml-get-attribute child 'xmlns)
                               jabber-reactions-xmlns))
                    (jabber-xml-get-children message 'reactions))))
    (and (null (cdr reactions))
         (car reactions))))

(defun jabber-reactions--fallback-for-reactions-p (fallback)
  "Return non-nil when FALLBACK is XEP-0444 reaction fallback text.
Mark fallback text as reaction-only when the fallback element covers the body."
  (and (string= (or (jabber-xml-get-attribute fallback 'xmlns) "")
                jabber-reactions-fallback-xmlns)
       (string= (or (jabber-xml-get-attribute fallback 'for) "")
                jabber-reactions-xmlns)))

(defun jabber-reactions--body-text (xml-data)
  "Return the plain `<body>' text from XML-DATA, or nil."
  (car (jabber-xml-node-children
        (car (jabber-xml-get-children xml-data 'body)))))

(defun jabber-reactions--integer-attribute (xml-data attribute)
  "Return XML-DATA's integer ATTRIBUTE, or nil when malformed."
  (when-let* ((value (jabber-xml-get-attribute xml-data attribute))
              ((string-match-p "\\`[0-9]+\\'" value)))
    (string-to-number value)))

(defun jabber-reactions--element-children (xml-data)
  "Return XML-DATA's child elements."
  (cl-remove-if-not #'listp (jabber-xml-node-children xml-data)))

(defun jabber-reactions--fallback-body-range (fallback)
  "Return FALLBACK body coverage as `whole', (START END), or nil."
  (let ((children (jabber-reactions--element-children fallback)))
    (if (null children)
        'whole
      (when-let* ((body (car (jabber-xml-get-children fallback 'body))))
        (let ((start-attr (jabber-xml-get-attribute body 'start))
              (end-attr (jabber-xml-get-attribute body 'end)))
          (cond
           ((and (null start-attr) (null end-attr)) 'whole)
           ((and start-attr end-attr)
            (when-let* ((start (jabber-reactions--integer-attribute body 'start))
                        (end (jabber-reactions--integer-attribute body 'end)))
              (list start end)))))))))

(defun jabber-reactions--range-covers-body-p (range body)
  "Return non-nil when RANGE covers all of BODY."
  (or (eq range 'whole)
      (pcase range
        (`(,start ,end)
         (and (zerop start)
              (>= end (length body)))))))

(defun jabber-reactions--fallback-body-p (xml-data)
  "Return non-nil when XML-DATA's body is only XEP-0444 fallback text."
  (when-let* ((body (jabber-reactions--body-text xml-data)))
    (cl-some (lambda (fallback)
               (and (jabber-reactions--fallback-for-reactions-p fallback)
                    (jabber-reactions--range-covers-body-p
                     (jabber-reactions--fallback-body-range fallback)
                     body)))
             (jabber-xml-get-children xml-data 'fallback))))

(defun jabber-reactions--reaction-only-p (xml-data)
  "Return non-nil when XML-DATA is only a reaction update stanza.
A reaction-only stanza has a XEP-0444 `<reactions>' payload and no
real `<body>', `<subject>', or `<error>' child.  A `<body>' fully marked
as XEP-0428 fallback for reactions does not count as a real body."
  (and (jabber-xml-child-with-xmlns xml-data jabber-reactions-xmlns)
       (or (not (jabber-xml-get-children xml-data 'body))
           (jabber-reactions--fallback-body-p xml-data))
       (not (jabber-xml-get-children xml-data 'subject))
       (not (jabber-xml-get-children xml-data 'error))))

(defun jabber-reactions--history-inhibit-p (_jc xml-data)
  "Return non-nil when XML-DATA should not be stored as a message body."
  (jabber-reactions--reaction-only-p xml-data))

(defun jabber-reactions--incoming-sender (from type)
  "Return the reaction sender key for incoming FROM and message TYPE."
  (when from
    (if (string= type "groupchat")
        from
      (jabber-jid-user from))))

(defun jabber-reactions--buffer-for-stanza (from type)
  "Return the displayed chat buffer for incoming FROM and message TYPE."
  (when from
    (if (string= type "groupchat")
        (jabber-muc-find-buffer (jabber-jid-user from))
      (jabber-chat-find-buffer (jabber-jid-user from)))))

(defun jabber-reactions--storage-peer (jc message type)
  "Return the DB peer for reaction-bearing MESSAGE on JC with TYPE."
  (let ((from (jabber-xml-get-attribute message 'from))
        (to (jabber-xml-get-attribute message 'to)))
    (if (string= type "groupchat")
        (and from (jabber-jid-user from))
      (let ((account (jabber-connection-bare-jid jc)))
        (cond
         ((and from (string= (jabber-jid-user from) account))
          (and to (jabber-jid-user to)))
         (from (jabber-jid-user from))
         (to (jabber-jid-user to)))))))

(defun jabber-reactions--message-updated-at (message)
  "Return the source timestamp for reaction MESSAGE, or nil.
Nil means MESSAGE has no delayed/source timestamp and should be applied
in arrival order."
  (when-let* ((timestamp (jabber-message-timestamp message)))
    (floor (float-time timestamp))))

(defun jabber-reactions--persist-update (jc message target-id sender reactions)
  "Persist SENDER's REACTIONS for TARGET-ID from MESSAGE on JC.
Return :stale when persistent storage confirms MESSAGE is stale."
  (let ((type (or (jabber-xml-get-attribute message 'type) "chat"))
        (updated-at (jabber-reactions--message-updated-at message)))
    (when-let* ((account (jabber-connection-bare-jid jc))
                (peer (jabber-reactions--storage-peer jc message type)))
      (unless (jabber-db-replace-reactions
               account peer type target-id sender reactions updated-at)
        (when (jabber-db-reaction-stale-p
               account peer type target-id sender updated-at)
          :stale)))))

(defun jabber-reactions--unwrap-stanza (jc xml-data)
  "Return (MESSAGE . BUFFER) for reaction-bearing XML-DATA on JC."
  (if (or (string= (or (jabber-xml-get-attribute xml-data 'type) "") "groupchat")
          (not (fboundp 'jabber-chat--unwrap-carbon)))
      (cons xml-data nil)
    (jabber-chat--unwrap-carbon jc xml-data)))

(defun jabber-reactions--update-message (msg sender sender-reactions)
  "Return MSG with SENDER's reactions replaced by SENDER-REACTIONS."
  (plist-put (copy-sequence msg)
             :reactions
             (jabber-reactions--replace-sender-reactions
              sender sender-reactions (plist-get msg :reactions))))

(defun jabber-reactions--local-sender ()
  "Return the local sender key for reaction state in the current buffer."
  (when jabber-buffer-connection
    (if (bound-and-true-p jabber-group)
        (when-let* ((nick (jabber-muc-nickname jabber-group jabber-buffer-connection)))
          (concat jabber-group "/" nick))
      (jabber-connection-bare-jid jabber-buffer-connection))))

;;; Command support

(defun jabber-reactions--composition-point-p ()
  "Return non-nil when point is in the chat composition area."
  (and (boundp 'jabber-point-insert)
       (markerp jabber-point-insert)
       (>= (point) jabber-point-insert)))

(defun jabber-reactions--message-id ()
  "Return a generated message ID for outgoing reaction stanzas."
  (format "emacs-reaction-%.6f" (float-time)))

(defun jabber-reactions--reactable-node-at-point ()
  "Return reaction context for the EWOC node at point, or nil.
The returned list has the form (NODE MSG TARGET-ID)."
  (unless (jabber-reactions--composition-point-p)
    (when-let* ((node (and (bound-and-true-p jabber-chat-ewoc)
                           (ewoc-locate jabber-chat-ewoc (point))))
                (data (ewoc-data node))
                ((listp data))
                (msg (cadr data))
                ((listp msg))
                (target-id (jabber-reactions--target-id
                            msg (bound-and-true-p jabber-group))))
      (list node msg target-id))))

(defun jabber-reactions--insert-literal-bang ()
  "Insert a literal exclamation mark in the composition area."
  (when (and (boundp 'jabber-point-insert)
             (markerp jabber-point-insert)
             (< (point) jabber-point-insert))
    (goto-char jabber-point-insert))
  (insert "!"))

(defun jabber-reactions--chat-target ()
  "Return reaction stanza target information for the current buffer.
The returned list has the form (TO TYPE), or nil outside chat and MUC
buffers with a known destination."
  (cond
   ((bound-and-true-p jabber-group)
    (list jabber-group "groupchat"))
   ((bound-and-true-p jabber-chatting-with)
    (list jabber-chatting-with "chat"))))

(defun jabber-reactions--optimistic-update (node msg sender sender-reactions)
  "Update NODE's MSG reaction state for SENDER to SENDER-REACTIONS."
  (let* ((reactions (plist-get msg :reactions))
         (updated-msg (plist-put
                       (copy-sequence msg)
                       :reactions
                       (jabber-reactions--replace-sender-reactions
                        sender sender-reactions reactions))))
    (setcar (cdr (ewoc-data node)) updated-msg)
    (jabber-chat-ewoc-invalidate node)))

;;;###autoload
(defun jabber-reactions-react-at-point-or-insert ()
  "React to the message at point, or insert a literal exclamation mark."
  (interactive)
  (pcase-let ((`(,node ,msg ,target-id)
               (jabber-reactions--reactable-node-at-point))
              (`(,to ,type) (jabber-reactions--chat-target))
              (sender (jabber-reactions--local-sender)))
    (if (and node msg target-id to type sender)
        (let ((reaction (completing-read
                         "Reaction: "
                         jabber-reactions-default-choices
                         nil nil)))
          (when (string-empty-p reaction)
            (user-error "Reaction cannot be empty"))
          (let ((sender-reactions (jabber-reactions--toggle-reaction
                                   reaction
                                   (jabber-reactions--sender-reactions sender msg))))
            (jabber-send-sexp
             jabber-buffer-connection
             (jabber-reactions--build-stanza to type target-id
                                             sender-reactions
                                             (jabber-reactions--message-id)))
            (jabber-db-replace-reactions
             (jabber-connection-bare-jid jabber-buffer-connection)
             (jabber-jid-user to)
             type target-id sender sender-reactions)
            (unless (bound-and-true-p jabber-group)
              (jabber-reactions--optimistic-update node msg sender sender-reactions))))
      (jabber-reactions--insert-literal-bang))))

;;; Incoming updates

(defun jabber-reactions--apply-incoming-update (node sender sender-reactions)
  "Apply SENDER's SENDER-REACTIONS to the message stored in NODE."
  (when-let* ((data (ewoc-data node))
              ((listp data))
              (msg (cadr data))
              ((listp msg)))
    (setcar (cdr data)
            (jabber-reactions--update-message msg sender sender-reactions))
    (jabber-chat-ewoc-invalidate node)))

(defun jabber-reactions--handle-message (jc xml-data)
  "Handle incoming XEP-0444 reaction stanzas in XML-DATA on JC.
Update stored and visible reaction state for the sending entity."
  (pcase-let* ((`(,message . ,carbon-buffer)
                (jabber-reactions--unwrap-stanza jc xml-data)))
    (when-let* ((reactions (jabber-reactions--single-element message))
                (parsed (jabber-reactions--parse-element reactions))
                (from (jabber-xml-get-attribute message 'from))
                (type (or (jabber-xml-get-attribute message 'type) "chat"))
                (sender (jabber-reactions--incoming-sender from type)))
      (unless (eq (jabber-reactions--persist-update
                   jc message (car parsed) sender (cadr parsed))
                  :stale)
        (when-let* ((buffer (or carbon-buffer
                                (jabber-reactions--buffer-for-stanza from type))))
          (with-current-buffer buffer
            (when-let* ((node (jabber-chat-ewoc-find-by-id (car parsed))))
              (jabber-reactions--apply-incoming-update node sender (cadr parsed)))))))))

(jabber-chain-add 'jabber-message-chain #'jabber-reactions--handle-message -5)
(add-to-list 'jabber-history-inhibit-received-message-functions
             #'jabber-reactions--history-inhibit-p)

;;; Disco

(jabber-disco-advertise-feature jabber-reactions-xmlns)

(provide 'jabber-reactions)
;;; jabber-reactions.el ends here
