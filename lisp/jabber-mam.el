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
                           server-id occupant-id oob-url oob-desc
                           encrypted))
(declare-function jabber-db--extract-occupant-id "jabber-db" (xml-data))
(declare-function jabber-db-last-server-id "jabber-db" (account &optional peer))
(declare-function jabber-db-ensure-open "jabber-db" ())
(declare-function jabber-chat--decrypt-if-needed "jabber-chat" (jc xml-data))
(declare-function jabber-chat-find-buffer "jabber-chat" (chat-with))
(declare-function jabber-chat-buffer-refresh "jabber-chatbuffer" (&optional count))
(declare-function jabber-muc-find-buffer "jabber-muc" (group))
(declare-function jabber-parse-time "jabber-util" (raw-time))
(declare-function jabber-sexp2xml "jabber-xml" (sexp))

(defvar jabber-message-chain)           ; jabber-core.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-chatting-with)           ; jabber-chat.el
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-backlog-number)          ; jabber-db.el
(defvar jabber-chat-mam-syncing)       ; jabber-chatbuffer.el

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

(defvar jabber-mam--dirty-peers nil
  "Peers that received MAM messages during sync.
Alist of (PEER . TYPE) where TYPE is the message type string.
Accumulated during sync, drained after COMMIT.")

(defvar jabber-mam--tx-depth 0
  "Reference count for the shared MAM transaction.
BEGIN when 0->1, COMMIT when 1->0.  Allows concurrent MAM queries
to share one SQLite transaction.")

(defvar jabber-mam--completion-callbacks nil
  "Alist of (QUERYID . CALLBACK) for per-query completion hooks.
CALLBACK is called with no arguments when the query finishes.")

(defvar jabber-mam--sync-received nil
  "Alist of (QUERYID . PLIST) for sync-buffer reconciliation.
PLIST keys: :ids (hash-table), :min-ts, :max-ts, :account, :peer.
Populated during sync; consumed by `jabber-mam--reconcile-sync'.")

(defvar jabber-mam--query-targets nil
  "Alist of (QUERYID . TARGET) for active MAM queries.
TARGET is a room JID for MUC MAM, or nil for 1:1 MAM.")

;;; Public predicates

(defun jabber-mam-syncing-p ()
  "Return non-nil if any MAM sync is in progress."
  (not (null jabber-mam--syncing)))

;;; Query building

(defvar jabber-mam--queryid-counter 0
  "Monotonic counter for unique MAM query IDs.")

(defun jabber-mam--make-queryid ()
  "Generate a unique query ID for MAM."
  (format "mam-%d-%d"
          (cl-incf jabber-mam--queryid-counter)
          (floor (float-time))))

(defun jabber-mam--build-query (queryid &optional with start after-id max
                                         before-id)
  "Build a MAM <query> sexp.
QUERYID is echoed in results for correlation.
WITH filters by JID, START is an XEP-0082 datetime string.
AFTER-ID is an RSM cursor for forward pagination.
MAX is the page size.
BEFORE-ID is an RSM cursor for backward pagination; when t, emit
an empty <before/> element (meaning \"last page\")."
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
    (when before-id
      (if (eq before-id t)
          (push '(before ()) rsm-children)
        (push `(before () ,before-id) rsm-children)))
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

(defun jabber-mam--active-query-p (queryid)
  "Return non-nil if QUERYID is an active MAM query."
  (cl-find queryid jabber-mam--syncing :key #'cdr :test #'string=))

(defun jabber-mam--valid-sender-p (jc from queryid)
  "Return non-nil if FROM is a valid MAM result sender for JC.
Valid senders are our own bare JID (1:1 archive) or a joined MUC
room (room archive).  A nil FROM is accepted only for own-archive
queries (no MUC target) because some servers omit the attribute
when the message originates from the user's own archive.
QUERYID identifies the active query for target lookup."
  (if (null from)
      ;; Accept nil from only for own-archive queries.
      ;; MUC queries always have a room JID in query-targets.
      (let ((target (cdr (assoc queryid jabber-mam--query-targets
                                #'string=))))
        (or (null target) (eq target 'one-shot)))
    (let ((bare (jabber-jid-user from))
          (our-jid (jabber-connection-bare-jid jc)))
      (or (string= bare our-jid)
          (jabber-muc-nickname bare)))))

(defun jabber-mam--process-message (jc xml-data)
  "Handle a MAM result <message> from the message chain.
JC is the Jabber connection.  XML-DATA is the stanza."
  ;; Validate query ID and sender JID against active queries.
  (when-let* ((result-el (jabber-xml-child-with-xmlns
                           xml-data jabber-mam-xmlns))
              (qid (jabber-xml-get-attribute result-el 'queryid))
              ((jabber-mam--active-query-p qid))
              (parsed (jabber-mam--parse-result xml-data))
              ;; Sender must be our bare JID (1:1 archive) or a
              ;; joined MUC room (room archive).
              ((jabber-mam--valid-sender-p
                jc (jabber-xml-get-attribute xml-data 'from) qid)))
    (let* ((archive-id (nth 0 parsed))
           (stamp (nth 1 parsed))
           (inner-msg (nth 2 parsed))
           ;; Detect encryption before decryption modifies the stanza
           (encrypted (and (or (jabber-xml-child-with-xmlns
                                inner-msg "eu.siacs.conversations.axolotl")
                               (jabber-xml-child-with-xmlns
                                inner-msg "jabber:x:encrypted")
                               (jabber-xml-child-with-xmlns
                                inner-msg "urn:xmpp:openpgp:0"))
                           t))
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
                         (let ((nick (jabber-jid-resource from))
                               (room (jabber-jid-user from)))
                           (if (and nick
                                    (jabber-mam--our-muc-nick-p
                                     room nick jc))
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
          (let ((ts (floor (float-time (or timestamp (current-time))))))
            (jabber-db-store-message
             our-jid peer direction type body ts
             (jabber-jid-resource from)
             stanza-id archive-id
             (jabber-db--extract-occupant-id inner-msg)
             oob-url oob-desc encrypted)
            ;; Track IDs for sync-buffer reconciliation.
            (when-let* ((sync-data
                         (cdr (assoc qid jabber-mam--sync-received
                                     #'string=)))
                        (ids (plist-get sync-data :ids)))
              (when archive-id (puthash archive-id t ids))
              (when stanza-id (puthash stanza-id t ids))
              (when (or (null (plist-get sync-data :min-ts))
                        (< ts (plist-get sync-data :min-ts)))
                (plist-put sync-data :min-ts ts))
              (when (or (null (plist-get sync-data :max-ts))
                        (> ts (plist-get sync-data :max-ts)))
                (plist-put sync-data :max-ts ts)))
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

(defun jabber-mam--our-muc-nick-p (room nick jc)
  "Return non-nil if NICK in ROOM is us on connection JC.
Checks the current room nickname first, then falls back to
comparing with the account username to handle nick changes."
  (require 'jabber-muc)
  (or (and-let* ((my-nick (jabber-muc-nickname room jc)))
        (string= nick my-nick))
      (string= nick (plist-get (fsm-get-state-data jc) :username))))

(declare-function jabber-muc-nickname "jabber-muc" (group &optional jc))

(defun jabber-mam--mark-dirty (peer type)
  "Record that PEER's buffer needs redisplay after sync.
TYPE is the message type (\"groupchat\" for MUC)."
  (unless (cl-find peer jabber-mam--dirty-peers :key #'car :test #'string=)
    (push (cons peer type) jabber-mam--dirty-peers)))

(defun jabber-mam--clear-syncing-flag (peer type)
  "Clear the syncing indicator in PEER's chat buffer.
TYPE is \"groupchat\" for MUC or \"chat\" for 1:1."
  (when-let* ((buffer (if (string= type "groupchat")
                          (jabber-muc-find-buffer peer)
                        (jabber-chat-find-buffer peer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq jabber-chat-mam-syncing nil)
      (force-mode-line-update))))

(defun jabber-mam--redraw-dirty ()
  "Refresh all chat buffers that received MAM messages during sync.
Reloads each buffer's ewoc from the database in place.
Does NOT manage `jabber-chat-mam-syncing'; that flag is controlled
by per-query completion callbacks registered in the catch-up functions."
  (let ((peers (prog1 jabber-mam--dirty-peers
                 (setq jabber-mam--dirty-peers nil))))
    (dolist (entry peers)
      (let* ((peer (car entry))
             (type (cdr entry))
             (buffer (if (string= type "groupchat")
                         (jabber-muc-find-buffer peer)
                       (jabber-chat-find-buffer peer))))
        (when (and buffer (buffer-live-p buffer))
          (with-current-buffer buffer
            (jabber-chat-buffer-refresh)))))))

;;; Query and pagination

(defun jabber-mam--query (jc &optional after-id queryid with start to
                                  before-id max)
  "Send a MAM query via JC, paginating from AFTER-ID.
QUERYID correlates results; generated if nil.
WITH and START are optional filters.
TO is the query target; nil for user archive, a room JID for MUC MAM.
BEFORE-ID and MAX support backward pagination (last-page queries).
When BEFORE-ID is non-nil, the query is one-shot (no forward pagination)."
  (let ((queryid (or queryid (jabber-mam--make-queryid)))
        (page-size (or max jabber-mam-page-size)))
    (push (cons jc queryid) jabber-mam--syncing)
    (when to
      (push (cons queryid to) jabber-mam--query-targets))
    ;; Mark one-shot queries so handle-fin skips forward pagination.
    (when before-id
      (push (cons queryid 'one-shot) jabber-mam--query-targets))
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
                                  page-size before-id)
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
    (let ((one-shot-p (assoc queryid jabber-mam--query-targets
                                   #'string=)))
      ;; One-shot queries (before-id based) never paginate forward.
      (setq one-shot-p (and one-shot-p
                            (eq (cdr one-shot-p) 'one-shot)))
      (if (or complete (null last-id) one-shot-p)
          (progn
            ;; Clean up query target tracking.
            (setq jabber-mam--query-targets
                  (cl-remove queryid jabber-mam--query-targets
                             :key #'car :test #'string=))
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
            ;; Redraw affected buffers from DB.
            (jabber-mam--redraw-dirty))
        ;; More pages: yield to the event loop for redisplay and input,
        ;; then continue pagination.
        (run-with-timer 0.1 nil #'jabber-mam--query
                        jc last-id queryid with start to)))))

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
    (setq jabber-mam--query-targets
          (cl-remove queryid jabber-mam--query-targets
                     :key #'car :test #'string=))
    (let ((error-el (car (jabber-xml-get-children xml-data 'error))))
      (if (and error-el
               (car (jabber-xml-get-children error-el 'item-not-found)))
          (progn
            (let ((inhibit-message t))
              (message "MAM: sync point expired%s, falling back to time-based query"
                       (if to (format " for %s" to) "")))
            ;; Transfer completion callback to the fallback query.
            (let* ((old-cb (assoc queryid jabber-mam--completion-callbacks
                                  #'string=))
                   (new-queryid (jabber-mam--make-queryid))
                   (start (when jabber-mam-catch-up-days
                            (format-time-string
                             "%Y-%m-%dT%H:%M:%SZ"
                             (time-subtract (current-time)
                                            (* jabber-mam-catch-up-days 86400))
                             t))))
              (when old-cb
                (setq jabber-mam--completion-callbacks
                      (delq old-cb jabber-mam--completion-callbacks))
                (push (cons new-queryid (cdr old-cb))
                      jabber-mam--completion-callbacks))
              (jabber-mam--query jc nil new-queryid nil start to)))
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

;;; 1:1 chat MAM catch-up

(defun jabber-mam--chat-catch-up (jc peer)
  "Sync missed messages for PEER via MAM.
JC is the Jabber connection.  PEER is the bare JID.
Registers a completion callback to clear the syncing indicator."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account peer))
         (queryid (jabber-mam--make-queryid)))
    (push (cons queryid
                (lambda ()
                  (jabber-mam--clear-syncing-flag peer "chat")))
          jabber-mam--completion-callbacks)
    (if last-id
        (jabber-mam--query jc last-id queryid peer nil nil)
      (let ((start (when jabber-mam-catch-up-days
                     (format-time-string
                      "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time)
                                     (* jabber-mam-catch-up-days 86400))
                      t))))
        (jabber-mam--query jc nil queryid peer start nil)))))

(defun jabber-mam-chat-opened (jc peer)
  "Trigger 1:1 MAM catch-up when opening a chat with PEER.
JC is the Jabber connection.  Called from `jabber-chat-create-buffer'.
Sets the syncing indicator immediately; clears it when the catch-up
query completes (or when disco reveals MAM is not supported)."
  (when jabber-mam-enable
    (when-let* ((buffer (jabber-chat-find-buffer peer))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (setq jabber-chat-mam-syncing t)
        (force-mode-line-update)))
    (jabber-disco-get-info
     jc (jabber-connection-bare-jid jc) nil
     (lambda (jc closure-data result)
       (let ((peer (car closure-data)))
         (if (and (listp result)
                  (not (eq (car result) 'error))
                  (member jabber-mam-xmlns (cadr result)))
             (jabber-mam--chat-catch-up jc peer)
           (jabber-mam--clear-syncing-flag peer "chat"))))
     (list peer))))

;;; MUC MAM catch-up

(defun jabber-mam--muc-catch-up (jc group)
  "Sync missed messages for GROUP via MUC MAM.
JC is the Jabber connection.  GROUP is the room bare JID.
Registers a completion callback to clear the syncing indicator."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account group))
         (queryid (jabber-mam--make-queryid)))
    (push (cons queryid
                (lambda ()
                  (jabber-mam--clear-syncing-flag group "groupchat")))
          jabber-mam--completion-callbacks)
    (if last-id
        (jabber-mam--query jc last-id queryid nil nil group)
      (let ((start (when jabber-mam-catch-up-days
                     (format-time-string
                      "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time)
                                     (* jabber-mam-catch-up-days 86400))
                      t))))
        (jabber-mam--query jc nil queryid nil start group)))))

(defun jabber-mam-muc-joined (jc group)
  "Trigger MUC MAM catch-up after joining GROUP.
JC is the Jabber connection.  Called from MUC self-presence handler.
Sets the syncing indicator immediately; clears it when the catch-up
query completes (or when disco reveals MAM is not supported)."
  (when jabber-mam-enable
    (when-let* ((buffer (jabber-muc-find-buffer group))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (setq jabber-chat-mam-syncing t)
        (force-mode-line-update)))
    (jabber-disco-get-info
     jc group nil
     (lambda (jc closure-data result)
       (let ((group (car closure-data)))
         (if (and (listp result)
                  (not (eq (car result) 'error))
                  (member jabber-mam-xmlns (cadr result)))
             (jabber-mam--muc-catch-up jc group)
           (jabber-mam--clear-syncing-flag group "groupchat"))))
     (list group))))

(defun jabber-mam--reconcile-sync (queryid)
  "Delete local messages not found in the remote archive for QUERYID.
Uses the IDs and timestamp range accumulated during sync to find
local messages that the server no longer has."
  (when-let* ((entry (assoc queryid jabber-mam--sync-received #'string=)))
    (let* ((data (cdr entry))
           (ids (plist-get data :ids))
           (min-ts (plist-get data :min-ts))
           (max-ts (plist-get data :max-ts))
           (account (plist-get data :account))
           (peer (plist-get data :peer)))
      (when (and min-ts max-ts (> (hash-table-count ids) 0))
        (when-let* ((db (jabber-db-ensure-open)))
          (let ((local-rows
                 (sqlite-select db "\
SELECT id, stanza_id, server_id FROM message \
WHERE account = ? AND peer = ? AND timestamp BETWEEN ? AND ? \
AND retracted_by IS NULL"
                                (list account peer min-ts max-ts)))
                (deleted 0))
            (dolist (row local-rows)
              (let ((row-id (nth 0 row))
                    (sid (nth 1 row))
                    (svid (nth 2 row)))
                ;; Only consider messages that have a server-side ID.
                ;; Messages without IDs can't be compared.
                (when (and (or sid svid)
                           (not (and svid (gethash svid ids)))
                           (not (and sid (gethash sid ids))))
                  (sqlite-execute db "DELETE FROM message WHERE id = ?"
                                  (list row-id))
                  (cl-incf deleted))))
            (when (> deleted 0)
              (message "MAM: removed %d messages not found on server"
                       deleted))))))
    (setq jabber-mam--sync-received
          (cl-remove queryid jabber-mam--sync-received
                     :key #'car :test #'string=))))

(defun jabber-mam-sync-buffer (count)
  "Sync the last COUNT messages from the server archive for this buffer.
Fetches recent messages using RSM backward pagination.  New messages
are decrypted and stored; existing messages are preserved via dedup.
Failed-decrypt placeholders are replaced if decryption now succeeds.
Local messages in the synced time range whose IDs are not found on
the server are deleted.  The buffer is refreshed in place after sync."
  (interactive
   (list (let ((input (read-string
                       (format "Messages to sync (default %d): "
                               jabber-backlog-number))))
           (if (string-empty-p input) jabber-backlog-number
             (string-to-number input)))))
  (unless (memq jabber-buffer-connection jabber-connections)
    (user-error "Not connected"))
  (let* ((jc jabber-buffer-connection)
         (group (bound-and-true-p jabber-group))
         (peer (or group
                   (jabber-jid-user (bound-and-true-p jabber-chatting-with))))
         (account (jabber-connection-bare-jid jc))
         (muc-p (not (null group)))
         (queryid (jabber-mam--make-queryid)))
    ;; Register ID tracking for post-sync reconciliation.
    (push (cons queryid (list :ids (make-hash-table :test #'equal)
                              :min-ts nil :max-ts nil
                              :account account :peer peer))
          jabber-mam--sync-received)
    (let ((type (if group "groupchat" "chat")))
      (push (cons queryid
                  (lambda ()
                    (jabber-mam--reconcile-sync queryid)
                    (jabber-mam--clear-syncing-flag peer type)))
            jabber-mam--completion-callbacks))
    (setq jabber-chat-mam-syncing t)
    (force-mode-line-update)
    (jabber-mam--mark-dirty peer (if group "groupchat" "chat"))
    (message "MAM: syncing last %d messages for %s..." count peer)
    (if muc-p
        (jabber-mam--query jc nil queryid nil nil peer t count)
      (jabber-mam--query jc nil queryid peer nil nil t count))))

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
      ;; Fire and remove leaked completion callbacks and query targets.
      (dolist (entry jc-queries)
        (let ((qid (cdr entry)))
          (when-let* ((cb (assoc qid jabber-mam--completion-callbacks
                                 #'string=)))
            (setq jabber-mam--completion-callbacks
                  (delq cb jabber-mam--completion-callbacks))
            (condition-case err (funcall (cdr cb))
              (error (message "MAM: cleanup callback error: %S" err))))
          (setq jabber-mam--query-targets
                (cl-remove qid jabber-mam--query-targets
                           :key #'car :test #'string=))))
      ;; Redraw affected buffers.
      (jabber-mam--redraw-dirty))))

(defun jabber-mam--cleanup-all ()
  "Clean up all MAM state on voluntary disconnect.
Called from `jabber-pre-disconnect-hook'."
  (when (> jabber-mam--tx-depth 0)
    (condition-case nil
        (when-let* ((db (jabber-db-ensure-open)))
          (sqlite-execute db "COMMIT"))
      (error nil)))
  ;; Fire remaining completion callbacks to clear syncing flags.
  (dolist (cb jabber-mam--completion-callbacks)
    (condition-case err (funcall (cdr cb))
      (error (message "MAM: cleanup callback error: %S" err))))
  (setq jabber-mam--syncing nil
        jabber-mam--tx-depth 0
        jabber-mam--completion-callbacks nil
        jabber-mam--query-targets nil
        jabber-mam--sync-received nil)
  (jabber-mam--redraw-dirty))

;;; MUC query cancellation

(defun jabber-mam--cancel-muc-query (room)
  "Cancel any active MUC MAM query for ROOM.
Removes the query from syncing state and decrements the transaction
depth.  Called when leaving a room to stop wasting bandwidth."
  (when-let* ((target-entry (cl-find room jabber-mam--query-targets
                                     :key #'cdr :test #'string=)))
    (let ((qid (car target-entry)))
      (setq jabber-mam--syncing
            (cl-remove qid jabber-mam--syncing
                       :key #'cdr :test #'string=))
      (setq jabber-mam--query-targets
            (delq target-entry jabber-mam--query-targets))
      (when-let* ((cb (assoc qid jabber-mam--completion-callbacks
                             #'string=)))
        (setq jabber-mam--completion-callbacks
              (delq cb jabber-mam--completion-callbacks))
        (condition-case err (funcall (cdr cb))
          (error (message "MAM: cleanup callback error: %S" err))))
      (when (> jabber-mam--tx-depth 0)
        (cl-decf jabber-mam--tx-depth))
      (when (zerop jabber-mam--tx-depth)
        (condition-case nil
            (when-let* ((db (jabber-db-ensure-open)))
              (sqlite-execute db "COMMIT"))
          (error nil))
        (jabber-mam--redraw-dirty)))))

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
