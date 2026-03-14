;;; jabber-mam.el --- XEP-0313 Message Archive Management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is part of emacs-jabber.

;; emacs-jabber is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; emacs-jabber is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-jabber.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; XEP-0313 (Message Archive Management) support.
;; Queries the server's message archive on connect to sync missed
;; messages across devices.  Results are stored in the local database
;; and displayed in open chat buffers.
;;
;; Pagination uses XEP-0059 (Result Set Management).
;; Deduplication uses XEP-0359 stanza-id / server-id.

;;; Code:

(require 'jabber-xml)
(require 'jabber-util)

(eval-when-compile (require 'cl-lib))

(declare-function jabber-connection-bare-jid "jabber-util" (jc))
(declare-function jabber-jid-user "jabber-util" (jid))
(declare-function jabber-jid-resource "jabber-util" (jid))
(declare-function jabber-send-iq "jabber-iq"
                  (jc to type query success-callback success-closure-data
                      error-callback error-closure-data &optional result-id))
(declare-function jabber-iq-query "jabber-util" (xml-data))
(declare-function jabber-disco-get-info "jabber-disco"
                  (jc jid node callback closure-data &optional force))
(declare-function jabber-disco-advertise-feature "jabber-disco" (feature))
(declare-function jabber-db-store-message "jabber-db"
                  (account peer direction type body timestamp
                           &optional resource stanza-id
                           server-id raw-xml oob-url oob-desc
                           encrypted))
(declare-function jabber-db-last-server-id "jabber-db" (account &optional peer))
(declare-function jabber-chat--decrypt-if-needed "jabber-chat" (jc xml-data))
(declare-function jabber-chat-find-buffer "jabber-chat" (chat-with))
(declare-function jabber-chat-insert-backlog-entry "jabber-chat" (msg-plist))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-muc-message-p "jabber-muc" (message))
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat"
                  (xml-data &optional delayed))
(declare-function jabber-maybe-print-rare-time "jabber-chat" (node))
(declare-function ewoc-enter-last "ewoc" (ewoc data))
(declare-function jabber-parse-time "jabber-util" (raw-time))
(declare-function jabber-sexp2xml "jabber-xml" (sexp))

(defvar jabber-message-chain)           ; jabber-core.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-group)                   ; jabber-muc.el

;;; Constants

(defconst jabber-mam-xmlns "urn:xmpp:mam:2"
  "Namespace for XEP-0313 MAM.")

(defconst jabber-mam-rsm-xmlns "http://jabber.org/protocol/rsm"
  "Namespace for XEP-0059 Result Set Management.")

(defconst jabber-mam-forward-xmlns "urn:xmpp:forward:0"
  "Namespace for XEP-0297 Stanza Forwarding.")

(defconst jabber-mam-delay-xmlns "urn:xmpp:delay"
  "Namespace for XEP-0203 Delayed Delivery.")

(defconst jabber-mam-sid-xmlns "urn:xmpp:sid:0"
  "Namespace for XEP-0359 Stanza IDs.")

;;; Customization

(defcustom jabber-mam-enable t
  "Whether to sync messages via MAM on connect."
  :type 'boolean
  :group 'jabber)

(defcustom jabber-mam-page-size 50
  "Number of messages to request per MAM page."
  :type 'integer
  :group 'jabber)

(defcustom jabber-mam-catch-up-days 7
  "Limit initial MAM catch-up to this many days back.
Only used when no previous sync point exists (first sync).
Set to nil to fetch the entire archive."
  :type '(choice integer (const :tag "Fetch all" nil))
  :group 'jabber)

;;; Internal state

(defvar jabber-mam--syncing nil
  "Non-nil while a MAM sync is in progress.
Alist of (JC . QUERYID) for active queries.")

;;; Query building

(defun jabber-mam--make-queryid ()
  "Generate a unique query ID for MAM."
  (format "mam-%d" (floor (* 1000000 (float-time)))))

(defun jabber-mam--build-query (queryid &optional with start after-id max)
  "Build a MAM <query> sexp.
QUERYID is echoed in results for correlation.
WITH filters by JID, START is an XEP-0082 datetime string.
AFTER-ID is an RSM cursor for pagination.
MAX is the page size."
  (let ((form-fields nil)
        (rsm-children nil))
    ;; Data form fields
    (when (or with start)
      (push `(field ((var . "FORM_TYPE") (type . "hidden"))
                    (value () ,jabber-mam-xmlns))
            form-fields)
      (when with
        (push `(field ((var . "with"))
                      (value () ,with))
              form-fields))
      (when start
        (push `(field ((var . "start"))
                      (value () ,start))
              form-fields))
      (setq form-fields (nreverse form-fields)))
    ;; RSM
    (when max
      (push `(max () ,(number-to-string max)) rsm-children))
    (when after-id
      (push `(after () ,after-id) rsm-children))
    (setq rsm-children (nreverse rsm-children))
    ;; Build query
    `(query ((xmlns . ,jabber-mam-xmlns)
             (queryid . ,queryid))
            ,@(when form-fields
                (list `(x ((xmlns . "jabber:x:data")
                           (type . "submit"))
                         ,@form-fields)))
            ,@(when rsm-children
                (list `(set ((xmlns . ,jabber-mam-rsm-xmlns))
                            ,@rsm-children))))))

;;; Result parsing

(defun jabber-mam--parse-result (xml-data)
  "Extract MAM result from a <message> stanza.
Returns (ARCHIVE-ID DELAY-STAMP INNER-MESSAGE) or nil."
  (when-let* ((result-el (jabber-xml-child-with-xmlns
                           xml-data jabber-mam-xmlns)))
    (let* ((archive-id (jabber-xml-get-attribute result-el 'id))
           (fwd-el (car (jabber-xml-get-children result-el 'forwarded)))
           (delay-el (and fwd-el
                          (car (jabber-xml-get-children fwd-el 'delay))))
           (stamp (and delay-el
                       (jabber-xml-get-attribute delay-el 'stamp)))
           (inner-msg (and fwd-el
                           (car (jabber-xml-get-children fwd-el 'message)))))
      (when inner-msg
        (list archive-id stamp inner-msg)))))

(defun jabber-mam--parse-fin (xml-data)
  "Parse a MAM <fin> IQ result.
Returns plist (:complete BOOL :first ID :last ID)."
  (let* ((fin-el (jabber-xml-child-with-xmlns xml-data jabber-mam-xmlns))
         (complete (string= (or (jabber-xml-get-attribute fin-el 'complete) "")
                            "true"))
         (set-el (and fin-el
                      (car (jabber-xml-get-children fin-el 'set))))
         (first-el (and set-el
                        (car (jabber-xml-get-children set-el 'first))))
         (last-el (and set-el
                       (car (jabber-xml-get-children set-el 'last))))
         (first-id (and first-el
                        (car (jabber-xml-node-children first-el))))
         (last-id (and last-el
                       (car (jabber-xml-node-children last-el)))))
    (list :complete complete :first first-id :last last-id)))

;;; Message chain handler

(defun jabber-mam--process-message (jc xml-data)
  "Handle a MAM result <message> from the message chain.
JC is the Jabber connection.  XML-DATA is the stanza."
  (when-let* ((parsed (jabber-mam--parse-result xml-data)))
    (let* ((archive-id (nth 0 parsed))
           (stamp (nth 1 parsed))
           (inner-msg (nth 2 parsed))
           ;; Detect encryption before decryption modifies the stanza
           (encrypted (or (jabber-xml-child-with-xmlns
                           inner-msg "eu.siacs.conversations.axolotl")
                          (jabber-xml-child-with-xmlns
                           inner-msg "jabber:x:encrypted")
                          (jabber-xml-child-with-xmlns
                           inner-msg "urn:xmpp:openpgp:0")))
           (inner-msg (jabber-chat--decrypt-if-needed jc inner-msg))
           (from (jabber-xml-get-attribute inner-msg 'from))
           (to (jabber-xml-get-attribute inner-msg 'to))
           (type (or (jabber-xml-get-attribute inner-msg 'type) "chat"))
           (body-el (car (jabber-xml-get-children inner-msg 'body)))
           (body (and body-el (car (jabber-xml-node-children body-el))))
           (stanza-id (jabber-xml-get-attribute inner-msg 'id))
           (our-jid (jabber-connection-bare-jid jc))
           (groupchat-p (string= type "groupchat"))
           (direction (if groupchat-p
                         (let* ((nick (jabber-jid-resource from))
                                (room (jabber-jid-user from))
                                (our-nick
                                 (jabber-mam--our-muc-nick room)))
                           (if (and nick our-nick (string= nick our-nick))
                               "out" "in"))
                       (if (string= (jabber-jid-user from) our-jid)
                           "out" "in")))
           (peer (if groupchat-p
                     (jabber-jid-user from)
                   (jabber-jid-user
                    (if (string= direction "out") to from))))
           (timestamp (and stamp (jabber-parse-time stamp)))
           ;; OOB
           (oob-x (cl-find-if
                    (lambda (x)
                      (and (listp x)
                           (string= (or (jabber-xml-get-attribute x 'xmlns) "")
                                    "jabber:x:oob")))
                    (jabber-xml-node-children inner-msg)))
           (oob-url (when oob-x
                      (car (jabber-xml-node-children
                            (car (jabber-xml-get-children oob-x 'url))))))
           (oob-desc (when oob-x
                       (car (jabber-xml-node-children
                             (car (jabber-xml-get-children oob-x 'desc)))))))
      (when (and peer body)
        (jabber-db-store-message
         our-jid peer direction type body
         (floor (float-time (or timestamp (current-time))))
         (jabber-jid-resource from)
         stanza-id archive-id nil
         oob-url oob-desc encrypted)
        ;; Display in open buffer if it exists
        (jabber-mam--maybe-display jc inner-msg peer direction
                                   type timestamp))
      ;; Strip children so downstream handlers (jabber-process-chat,
      ;; jabber-muc-process-message, jabber-db--message-handler) see
      ;; an empty stanza and skip it.
      (setcdr (cdr xml-data) nil))))

(defvar jabber-muc-participants)        ; jabber-muc.el

(defun jabber-mam--our-muc-nick (room)
  "Return our nickname in ROOM, or nil."
  (when-let* ((participants (cdr (assoc room jabber-muc-participants))))
    ;; The room stores (nick . plist) entries; we stored our nick
    ;; at join time via jabber-muc-join-set.
    (require 'jabber-muc)
    (ignore-errors (jabber-muc-nickname room))))

(declare-function jabber-muc-nickname "jabber-muc" (group))

(defun jabber-mam--maybe-display (_jc inner-msg peer direction type timestamp)
  "Display a MAM result in an open chat buffer, if one exists.
JC is the connection, INNER-MSG the forwarded message,
PEER the bare JID, DIRECTION \"in\" or \"out\",
TYPE the message type, TIMESTAMP the parsed time."
  (let* ((groupchat-p (string= type "groupchat"))
         (buffer (if groupchat-p
                     (jabber-muc-find-buffer peer)
                   (jabber-chat-find-buffer peer))))
    (when buffer
      (let ((msg-plist (jabber-chat--msg-plist-from-stanza inner-msg t)))
        (when timestamp
          (plist-put msg-plist :timestamp timestamp))
        (plist-put msg-plist :direction direction)
        (with-current-buffer buffer
          (let ((node-type
                 (cond
                  ((and groupchat-p (string= direction "out")) :muc-local)
                  (groupchat-p :muc-foreign)
                  ((string= direction "out") :local)
                  (t :foreign))))
            (jabber-maybe-print-rare-time
             (ewoc-enter-last jabber-chat-ewoc
                              (list node-type msg-plist)))))))))

;;; Query and pagination

(defun jabber-mam--query (jc &optional after-id queryid with start to)
  "Send a MAM query via JC, paginating from AFTER-ID.
QUERYID correlates results; generated if nil.
WITH and START are optional filters.
TO is the query target; nil for user archive, a room JID for MUC MAM."
  (let ((queryid (or queryid (jabber-mam--make-queryid))))
    (push (cons jc queryid) jabber-mam--syncing)
    (jabber-send-iq
     jc to "set"
     (jabber-mam--build-query queryid with start after-id
                              jabber-mam-page-size)
     #'jabber-mam--handle-fin
     (list queryid with start to)
     #'jabber-mam--handle-error
     (list queryid to))))

(defun jabber-mam--handle-fin (jc xml-data closure)
  "Handle the <fin> IQ result for a MAM query.
JC is the connection.  XML-DATA is the IQ response.
CLOSURE is (QUERYID WITH START TO)."
  (let* ((queryid (nth 0 closure))
         (with (nth 1 closure))
         (start (nth 2 closure))
         (to (nth 3 closure))
         (fin (jabber-mam--parse-fin xml-data))
         (complete (plist-get fin :complete))
         (last-id (plist-get fin :last)))
    ;; Remove from syncing list
    (setq jabber-mam--syncing
          (cl-remove queryid jabber-mam--syncing
                     :key #'cdr :test #'string=))
    (if (or complete (null last-id))
        (message "MAM: sync complete%s"
                 (if to (format " for %s" to)
                   (if with (format " for %s" with) "")))
      ;; More pages, continue
      (jabber-mam--query jc last-id queryid with start to))))

(defun jabber-mam--handle-error (jc xml-data closure)
  "Handle a MAM query error.
JC is the connection.  XML-DATA is the IQ error.
CLOSURE is (QUERYID TO).
On item-not-found (stale sync point), falls back to time-based query."
  (let ((queryid (car closure))
        (to (cadr closure)))
    (setq jabber-mam--syncing
          (cl-remove queryid jabber-mam--syncing
                     :key #'cdr :test #'string=))
    (let ((error-el (car (jabber-xml-get-children xml-data 'error))))
      (if (and error-el
               (car (jabber-xml-get-children error-el 'item-not-found)))
          (progn
            (message "MAM: sync point expired%s, falling back to time-based query"
                     (if to (format " for %s" to) ""))
            (let ((start (when jabber-mam-catch-up-days
                           (format-time-string
                            "%Y-%m-%dT%H:%M:%SZ"
                            (time-subtract (current-time)
                                           (* jabber-mam-catch-up-days 86400))
                            t))))
              (jabber-mam--query jc nil nil nil start to)))
        (message "MAM: query failed: %s"
                 (jabber-sexp2xml xml-data))))))

;;; Post-connect catch-up

(defun jabber-mam--catch-up (jc)
  "Sync missed messages for JC via MAM."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account)))
    (if last-id
        ;; Resume from last known server-id
        (jabber-mam--query jc last-id)
      ;; First sync: limit to N days back
      (let ((start (when jabber-mam-catch-up-days
                     (format-time-string
                      "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time)
                                     (* jabber-mam-catch-up-days 86400))
                      t))))
        (jabber-mam--query jc nil nil nil start)))))

(defun jabber-mam-maybe-catchup (jc)
  "Post-connect hook: sync messages via MAM if enabled.
Added to `jabber-post-connect-hooks'."
  (when jabber-mam-enable
    (jabber-disco-get-info
     jc (jabber-connection-bare-jid jc) nil
     (lambda (jc _closure-data result)
       (when (and (listp result)
                  (not (eq (car result) 'error))
                  (member jabber-mam-xmlns (cadr result)))
         (jabber-mam--catch-up jc)))
     nil)))

;;; MUC MAM catch-up

(defun jabber-mam--muc-catch-up (jc group)
  "Sync missed messages for GROUP via MUC MAM.
JC is the Jabber connection.  GROUP is the room bare JID."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account group)))
    (if last-id
        (jabber-mam--query jc last-id nil nil nil group)
      ;; First sync for this room: limit to N days back
      (let ((start (when jabber-mam-catch-up-days
                     (format-time-string
                      "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time)
                                     (* jabber-mam-catch-up-days 86400))
                      t))))
        (jabber-mam--query jc nil nil nil start group)))))

(defun jabber-mam-muc-joined (jc group)
  "Trigger MUC MAM catch-up after joining GROUP.
JC is the Jabber connection.  Called from MUC self-presence handler."
  (when jabber-mam-enable
    (jabber-disco-get-info
     jc group nil
     (lambda (jc closure-data result)
       (when (and (listp result)
                  (not (eq (car result) 'error))
                  (member jabber-mam-xmlns (cadr result)))
         (jabber-mam--muc-catch-up jc (car closure-data))))
     (list group))))

;;; Registration

(jabber-disco-advertise-feature jabber-mam-xmlns)

;; Add at front of message chain so MAM results are intercepted
;; before jabber-process-chat and jabber-muc-process-message.
(add-to-list 'jabber-message-chain #'jabber-mam--process-message)

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-connect-hooks #'jabber-mam-maybe-catchup))

(provide 'jabber-mam)
;;; jabber-mam.el ends here
