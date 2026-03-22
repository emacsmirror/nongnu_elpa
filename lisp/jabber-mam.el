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
(declare-function jabber-db-ensure-open "jabber-db" ())
(declare-function jabber-chat--decrypt-if-needed "jabber-chat" (jc xml-data))
(declare-function jabber-chat-find-buffer "jabber-chat" (chat-with))
(declare-function jabber-chat-insert-backlog-entry "jabber-chat" (msg-plist))
(declare-function jabber-chat--insert-backlog-chunked "jabber-chat"
                  (buffer entries callback))
(declare-function jabber-chat-display-buffer-images "jabber-chat" ())
(declare-function jabber-db-backlog "jabber-db"
                  (account peer &optional count start-time))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-parse-time "jabber-util" (raw-time))
(declare-function jabber-sexp2xml "jabber-xml" (sexp))

(defvar jabber-message-chain)           ; jabber-core.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat--msg-nodes)        ; jabber-chatbuffer.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-point-insert)            ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-chat-earliest-backlog)   ; jabber-chat.el
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

(defcustom jabber-mam-catch-up-days 3
  "Limit initial MAM catch-up to this many days back.
Only used when no previous sync point exists (first sync).
Set to nil to fetch the entire archive."
  :type '(choice integer (const :tag "Fetch all" nil))
  :group 'jabber)

;;; Internal state

(defvar jabber-mam--syncing nil
  "Non-nil while a MAM sync is in progress.
Alist of (JC . QUERYID) for active queries.")

(defvar jabber-mam--dirty-buffers nil
  "Buffers that received MAM messages during sync.
Accumulated during sync, drained by the post-sync redisplay FSM.")

(defvar jabber-mam--tx-depth 0
  "Reference count for the shared MAM transaction.
BEGIN when 0->1, COMMIT when 1->0.  Allows concurrent MAM queries
to share one SQLite transaction.")

(defvar jabber-mam--completion-callbacks nil
  "Alist of (QUERYID . CALLBACK) for per-query completion hooks.
CALLBACK is called with no arguments when the query finishes.")

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

(defun jabber-mam--unwrap-into (outer inner)
  "Replace OUTER stanza's attributes and children with INNER's.
Marks the stanza as MAM-origin so downstream handlers can suppress
outgoing receipts.  Mutates OUTER in place."
  (setcar (cdr outer) (append (jabber-xml-node-attributes inner)
                              '((jabber-mam--origin . "t"))))
  (setcdr (cdr outer) (cddr inner)))

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
      (if (and peer body)
          (progn
            (jabber-db-store-message
             our-jid peer direction type body
             (floor (float-time (or timestamp (current-time))))
             (jabber-jid-resource from)
             stanza-id archive-id nil
             oob-url oob-desc encrypted)
            ;; Don't display during sync.  Track the buffer for
            ;; post-sync redisplay instead.
            (jabber-mam--mark-dirty peer type)
            ;; Strip children so downstream handlers see an empty
            ;; stanza and skip it.
            (setcdr (cdr xml-data) nil))
        ;; Bodyless MAM result (receipt, marker, chat state): unwrap
        ;; the inner message so downstream handlers see the original
        ;; sender JID and protocol elements.
        (jabber-mam--unwrap-into xml-data inner-msg)))))

(defvar jabber-muc-participants)        ; jabber-muc.el

(defun jabber-mam--our-muc-nick (room)
  "Return our nickname in ROOM, or nil."
  (when-let* ((participants (cdr (assoc room jabber-muc-participants))))
    ;; The room stores (nick . plist) entries; we stored our nick
    ;; at join time via jabber-muc-join-set.
    (require 'jabber-muc)
    (ignore-errors (jabber-muc-nickname room))))

(declare-function jabber-muc-nickname "jabber-muc" (group))

(defun jabber-mam--mark-dirty (peer type)
  "Record that PEER's buffer needs redisplay after sync.
TYPE is the message type (\"groupchat\" for MUC)."
  (let ((buffer (if (string= type "groupchat")
                    (jabber-muc-find-buffer peer)
                  (jabber-chat-find-buffer peer))))
    (when (and buffer (not (memq buffer jabber-mam--dirty-buffers)))
      (push buffer jabber-mam--dirty-buffers))))

(defun jabber-mam--scroll-to-bottom (buffer)
  "Scroll BUFFER's window to the chat prompt if visible."
  (when-let* ((win (and (buffer-live-p buffer)
                        (get-buffer-window buffer))))
    (with-selected-window win
      (goto-char jabber-point-insert)
      (recenter -1))))

(defun jabber-mam--redisplay-next ()
  "Reload the next dirty buffer from DB, then schedule the next one.
Clears the ewoc and re-inserts backlog so MAM messages appear.
Processes one buffer per timer callback so the event loop stays
responsive between redraws."
  (while (and jabber-mam--dirty-buffers
              (not (buffer-live-p (car jabber-mam--dirty-buffers))))
    (pop jabber-mam--dirty-buffers))
  (when-let* ((buf (pop jabber-mam--dirty-buffers)))
    (jabber-mam--reload-buffer buf)
    (when jabber-mam--dirty-buffers
      (run-with-timer 0.05 nil #'jabber-mam--redisplay-next))))

(defun jabber-mam--reload-buffer (buffer)
  "Clear BUFFER's ewoc and reload backlog from the database.
This is needed after MAM sync because messages were stored to DB
but never inserted into the ewoc."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Clear all ewoc nodes and stanza ID index
      (ewoc-filter jabber-chat-ewoc #'ignore)
      (when jabber-chat--msg-nodes
        (clrhash jabber-chat--msg-nodes))
      ;; Reset backlog marker so full backlog loads
      (setq jabber-chat-earliest-backlog nil)
      ;; Reload from DB
      (let* ((peer (or (bound-and-true-p jabber-chatting-with)
                       (bound-and-true-p jabber-group)))
             (backlog-entries
              (when peer
                (jabber-db-backlog
                 (jabber-connection-bare-jid jabber-buffer-connection)
                 (jabber-jid-user peer)))))
        (if backlog-entries
            (progn
              (setq jabber-chat-earliest-backlog
                    (float-time (plist-get (car (last backlog-entries))
                                           :timestamp)))
              (jabber-chat--insert-backlog-chunked
               buffer backlog-entries
               (lambda ()
                 (jabber-chat-display-buffer-images)
                 (jabber-mam--scroll-to-bottom buffer))))
          (jabber-mam--scroll-to-bottom buffer))))))

;;; Query and pagination

(defun jabber-mam--query (jc &optional after-id queryid with start to)
  "Send a MAM query via JC, paginating from AFTER-ID.
QUERYID correlates results; generated if nil.
WITH and START are optional filters.
TO is the query target; nil for user archive, a room JID for MUC MAM."
  (let ((queryid (or queryid (jabber-mam--make-queryid))))
    (push (cons jc queryid) jabber-mam--syncing)
    ;; Open a shared transaction for concurrent MAM queries.
    ;; COMMIT happens when the last active query finishes.
    (when (zerop jabber-mam--tx-depth)
      (when-let* ((db (jabber-db-ensure-open)))
        (sqlite-execute db "BEGIN")))
    (cl-incf jabber-mam--tx-depth)
    (condition-case err
        (jabber-send-iq
         jc to "set"
         (jabber-mam--build-query queryid with start after-id
                                  jabber-mam-page-size)
         #'jabber-mam--handle-fin
         (list queryid with start to)
         #'jabber-mam--handle-error
         (list queryid to))
      (error
       (when (> jabber-mam--tx-depth 0)
         (cl-decf jabber-mam--tx-depth))
       (when (zerop jabber-mam--tx-depth)
         (when-let* ((db (jabber-db-ensure-open)))
           (sqlite-execute db "COMMIT")))
       (setq jabber-mam--syncing
             (cl-remove queryid jabber-mam--syncing
                        :key #'cdr :test #'string=))
       (message "MAM: query failed to send: %s"
                (error-message-string err))))))

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
    ;; Decrement transaction ref count; COMMIT when last query finishes.
    (when (> jabber-mam--tx-depth 0)
      (cl-decf jabber-mam--tx-depth))
    (when (zerop jabber-mam--tx-depth)
      (when-let* ((db (jabber-db-ensure-open)))
        (sqlite-execute db "COMMIT")))
    ;; Remove from syncing list
    (setq jabber-mam--syncing
          (cl-remove queryid jabber-mam--syncing
                     :key #'cdr :test #'string=))
    (if (or complete (null last-id))
        (progn
          (let ((inhibit-message t))
            (message "MAM: sync complete%s"
                     (if to (format " for %s" to)
                       (if with (format " for %s" with) ""))))
          ;; Fire per-query completion callback if registered.
          (when-let* ((cb (assoc queryid jabber-mam--completion-callbacks
                                 #'string=)))
            (setq jabber-mam--completion-callbacks
                  (delq cb jabber-mam--completion-callbacks))
            (funcall (cdr cb)))
          ;; Redisplay affected buffers one at a time.
          (when jabber-mam--dirty-buffers
            (run-with-timer 0.05 nil #'jabber-mam--redisplay-next)))
      ;; More pages: yield to the event loop for redisplay and input,
      ;; then continue pagination.
      (run-with-timer 0.1 nil #'jabber-mam--query
                      jc last-id queryid with start to))))

(defun jabber-mam--handle-error (jc xml-data closure)
  "Handle a MAM query error.
JC is the connection.  XML-DATA is the IQ error.
CLOSURE is (QUERYID TO).
On item-not-found (stale sync point), falls back to time-based query."
  (let ((queryid (car closure))
        (to (cadr closure)))
    ;; Decrement transaction ref count; COMMIT when last query finishes.
    (when (> jabber-mam--tx-depth 0)
      (cl-decf jabber-mam--tx-depth))
    (when (zerop jabber-mam--tx-depth)
      (when-let* ((db (jabber-db-ensure-open)))
        (sqlite-execute db "COMMIT")))
    (setq jabber-mam--syncing
          (cl-remove queryid jabber-mam--syncing
                     :key #'cdr :test #'string=))
    (let ((error-el (car (jabber-xml-get-children xml-data 'error))))
      (if (and error-el
               (car (jabber-xml-get-children error-el 'item-not-found)))
          (progn
            (let ((inhibit-message t))
              (message "MAM: sync point expired%s, falling back to time-based query"
                       (if to (format " for %s" to) "")))
            (let ((start (when jabber-mam-catch-up-days
                           (format-time-string
                            "%Y-%m-%dT%H:%M:%SZ"
                            (time-subtract (current-time)
                                           (* jabber-mam-catch-up-days 86400))
                            t))))
              (jabber-mam--query jc nil nil nil start to)))
        ;; Permanent error: fire completion callback so callers aren't stuck.
        (when-let* ((cb (assoc queryid jabber-mam--completion-callbacks
                               #'string=)))
          (setq jabber-mam--completion-callbacks
                (delq cb jabber-mam--completion-callbacks))
          (funcall (cdr cb)))
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

(defun jabber-mam-sync-buffer ()
  "Fetch new messages from the server archive for this chat buffer.
Queries MAM for the current peer, stores results in the database,
then redraws the buffer."
  (interactive)
  (unless (memq jabber-buffer-connection jabber-connections)
    (user-error "Not connected"))
  (let* ((jc jabber-buffer-connection)
         (group (bound-and-true-p jabber-group))
         (peer (or group (bound-and-true-p jabber-chatting-with)))
         (account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account peer)))
    (message "MAM: syncing %s..." peer)
    (if group
        (jabber-mam--query jc last-id nil nil nil group)
      (jabber-mam--query jc last-id nil peer nil nil))
    ;; Force redraw even when no new messages arrive, so the user
    ;; sees any previously stored but undisplayed messages.
    (unless (memq (current-buffer) jabber-mam--dirty-buffers)
      (push (current-buffer) jabber-mam--dirty-buffers))))

;;; On-demand history fetch

(defun jabber-mam-fetch-peer-history (jc peer &optional muc-p callback)
  "Fetch full MAM history for PEER via JC.
When MUC-P is non-nil, query the room archive (to=PEER).
When CALLBACK is non-nil, call it with no arguments after the
query completes (including all pagination)."
  (let ((queryid (jabber-mam--make-queryid)))
    (when callback
      (push (cons queryid callback) jabber-mam--completion-callbacks))
    (if muc-p
        (jabber-mam--query jc nil queryid nil nil peer)
      (jabber-mam--query jc nil queryid peer nil nil))))

;;; Disconnect cleanup

(defun jabber-mam--cleanup-connection (jc)
  "Clean up MAM state for connection JC.
Called from `jabber-lost-connection-hooks' on involuntary disconnect."
  (let ((jc-queries (cl-remove-if-not
                     (lambda (entry) (eq (car entry) jc))
                     jabber-mam--syncing)))
    (when jc-queries
      (setq jabber-mam--syncing
            (cl-set-difference jabber-mam--syncing jc-queries))
      (cl-decf jabber-mam--tx-depth (length jc-queries))
      (when (< jabber-mam--tx-depth 0)
        (setq jabber-mam--tx-depth 0))
      (when (zerop jabber-mam--tx-depth)
        (condition-case nil
            (when-let* ((db (jabber-db-ensure-open)))
              (sqlite-execute db "COMMIT"))
          (error nil)))
      ;; Remove leaked completion callbacks for this connection's queries.
      (dolist (entry jc-queries)
        (let ((cb (assoc (cdr entry) jabber-mam--completion-callbacks
                         #'string=)))
          (when cb
            (setq jabber-mam--completion-callbacks
                  (delq cb jabber-mam--completion-callbacks)))))
      ;; Trigger redisplay for any dirty buffers accumulated so far.
      (when jabber-mam--dirty-buffers
        (run-with-timer 0.05 nil #'jabber-mam--redisplay-next)))))

(defun jabber-mam--cleanup-all ()
  "Clean up all MAM state on voluntary disconnect.
Called from `jabber-pre-disconnect-hook'."
  (when (> jabber-mam--tx-depth 0)
    (condition-case nil
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "COMMIT"))
      (error nil)))
  (setq jabber-mam--syncing nil
        jabber-mam--tx-depth 0
        jabber-mam--completion-callbacks nil)
  (when jabber-mam--dirty-buffers
    (run-with-timer 0.05 nil #'jabber-mam--redisplay-next)))

;;; Registration

(jabber-disco-advertise-feature jabber-mam-xmlns)

;; Add at front of message chain so MAM results are intercepted
;; before jabber-process-chat and jabber-muc-process-message.
(add-to-list 'jabber-message-chain #'jabber-mam--process-message)

(with-eval-after-load "jabber-core"
  (add-hook 'jabber-post-connect-hooks #'jabber-mam-maybe-catchup)
  (add-hook 'jabber-pre-disconnect-hook #'jabber-mam--cleanup-all)
  (add-hook 'jabber-lost-connection-hooks #'jabber-mam--cleanup-connection))

(provide 'jabber-mam)
;;; jabber-mam.el ends here
