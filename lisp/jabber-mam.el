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
(require 'jabber-db)
(require 'jabber-chat)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-lifecycle)
(require 'jabber-message-correct)
(require 'jabber-muc-state)

(eval-when-compile (require 'cl-lib))

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
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

;;; Hooks

(defvar jabber-mam-peer-syncing-functions nil
  "Hook run when a peer's MAM sync state changes.
Each function receives three arguments: PEER (bare JID), TYPE
\(\"groupchat\" or \"chat\"), and SYNCING-P (non-nil when sync
starts, nil when it ends).")

(defvar jabber-mam-sync-complete-functions nil
  "Hook run after MAM stores messages for one or more peers.
Each function receives one argument: a list of (ACCOUNT PEER TYPE) entries.")

;;; Internal state

(defvar jabber-mam--syncing nil
  "Active query plists, including queries waiting for their next page.
Each owns its connection/session predicate, archive target, page token,
transaction contribution, pagination timer and completion callbacks.")

(defvar jabber-mam--dirty-peers nil
  "Peers awaiting refresh after COMMIT, as (ACCOUNT PEER TYPE) entries.")

(defvar jabber-mam--tx-depth 0
  "Reference count for the shared MAM transaction.
Each active page owns exactly one contribution; waiting timers own none.")

(defvar jabber-mam--peer-syncing nil
  "Alist of active automatic peer catch-ups.
Each entry has the shape ((JC PEER) . TOKEN).")

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

(defun jabber-mam--begin-peer-sync (jc peer)
  "Record an automatic MAM catch-up for JC and PEER.
Return its unique token, or nil when one was already active."
  (let ((key (list jc peer)))
    (unless (assoc key jabber-mam--peer-syncing #'equal)
      (let ((token (make-symbol "jabber-mam-peer-sync-")))
        (push (cons key token) jabber-mam--peer-syncing)
        token))))

(defun jabber-mam--peer-sync-active-p (jc peer token)
  "Return non-nil when TOKEN owns JC and PEER's catch-up."
  (eq token
      (cdr (assoc (list jc peer) jabber-mam--peer-syncing #'equal))))

(defun jabber-mam--end-peer-sync (jc peer token)
  "Forget TOKEN's automatic MAM catch-up for JC and PEER."
  (when-let* ((entry (assoc (list jc peer)
                            jabber-mam--peer-syncing #'equal))
              ((eq token (cdr entry))))
    (setq jabber-mam--peer-syncing
          (delq entry jabber-mam--peer-syncing))
    t))

(defun jabber-mam--finish-peer-sync (jc peer token)
  "Finish TOKEN's peer catch-up for JC and PEER."
  (when (jabber-mam--end-peer-sync jc peer token)
    (jabber-lifecycle--dispatch-contained
     'jabber-mam-peer-syncing-functions peer "chat" nil)))

(defun jabber-mam--release-page (query)
  "Release QUERY's page and transaction contribution at most once."
  (plist-put query :page nil)
  (when-let* ((id (plist-get query :iq-id)))
    (setq jabber-open-info-queries
          (delete (assoc id jabber-open-info-queries) jabber-open-info-queries))
    (plist-put query :iq-id nil))
  (when (plist-get query :transaction)
    (plist-put query :transaction nil)
    (jabber-mam--tx-end)))

(defun jabber-mam--complete-query (query outcome)
  "Retire QUERY with OUTCOME before releasing resources and calling consumers.
OUTCOME is `success', `failed' or `cancelled'; see `jabber-mam--query'.
Contain callback errors and quits so other queries can finish cleanup."
  (when (memq query jabber-mam--syncing)
    (setq jabber-mam--syncing (delq query jabber-mam--syncing))
    (when-let* ((timer (plist-get query :timer)))
      (cancel-timer timer)
      (plist-put query :timer nil))
    (unwind-protect
        (jabber-mam--release-page query)
      (when-let* ((callback (plist-get query :callback)))
        (jabber-lifecycle--call-contained callback))
      (when-let* ((callback (plist-get query :result-callback)))
        (jabber-lifecycle--call-contained callback outcome)))
    (when (zerop jabber-mam--tx-depth)
      (jabber-mam--redraw-dirty))))

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
  "Extract MAM result from the <message> stanza XML-DATA.
Return (ARCHIVE-ID DELAY-STAMP INNER-MESSAGE) or nil."
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
  "Parse a MAM <fin> IQ result XML-DATA.
Return plist (:complete BOOL :first ID :last ID)."
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

(defun jabber-mam--unwrap-into (outer inner &optional archive-id)
  "Replace OUTER stanza's attributes and children with INNER's.
Marks the stanza as MAM-origin so downstream handlers can suppress
outgoing receipts.  ARCHIVE-ID is preserved for bodyless archive
stanzas whose downstream handlers need the MAM result id.  Mutates
OUTER in place."
  (setcar (cdr outer) (append (jabber-xml-node-attributes inner)
                              `((jabber-mam--origin . "t")
                                ,@(and archive-id
                                       `((jabber-mam--archive-id
                                          . ,archive-id))))))
  (setcdr (cdr outer) (cddr inner)))

(defun jabber-mam--session-predicate (jc)
  "Return a predicate for JC's captured transport, stream and account.
Ordinary FSM state copies do not retire this session."
  (let* ((state (fsm-get-state-data jc))
         (connection (plist-get state :connection))
         (stream (plist-get state :session-id))
         (username (plist-get state :username))
         (server (plist-get state :server)))
    (lambda ()
      (let ((current (fsm-get-state-data jc)))
        (and (eq connection (plist-get current :connection))
             (equal stream (plist-get current :session-id))
             (equal username (plist-get current :username))
             (equal server (plist-get current :server)))))))

(defun jabber-mam--current-query-p (jc query)
  "Return non-nil when JC still owns the registered QUERY."
  (and (eq jc (plist-get query :jc))
       (memq query jabber-mam--syncing)
       (funcall (plist-get query :current-p))))

(defun jabber-mam--valid-sender-p (jc from query)
  "Return non-nil when FROM is QUERY's archive on its owning JC.
Require the exact archive JID, never a room occupant or account resource.
For compatibility, absent FROM is accepted for an owned personal archive."
  (and (jabber-mam--current-query-p jc query)
       (let ((target (plist-get query :to)))
         (if from
             (equal from (or target (jabber-connection-bare-jid jc)))
           (null target)))))

(defun jabber-mam--classify-direction (jc from to type)
  "Classify message direction and peer from MAM result fields.
JC is the connection, FROM/TO are stanza JIDs, TYPE is message type.
Return (DIRECTION . PEER) where DIRECTION is \"in\" or \"out\"."
  (let* ((our-jid (jabber-connection-bare-jid jc))
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
                  (if (string= direction "out") to from)))))
    (cons direction peer)))

(defun jabber-mam--extract-fields (jc inner-msg stamp)
  "Extract message fields from INNER-MSG for storage.
JC is the connection.  STAMP is the MAM delay timestamp string.
Returns a plist with :from :to :type :body :stanza-id :our-jid
:direction :peer :timestamp :oob-entries, or nil if direction
cannot be determined."
  (let* ((from (jabber-xml-get-attribute inner-msg 'from))
         (to (jabber-xml-get-attribute inner-msg 'to))
         (type (or (jabber-xml-get-attribute inner-msg 'type) "chat"))
         (body-el (car (jabber-xml-get-children inner-msg 'body)))
         (body (and body-el (car (jabber-xml-node-children body-el))))
         (stanza-id (jabber-xml-get-attribute inner-msg 'id))
         (our-jid (jabber-connection-bare-jid jc))
         (dir-peer (jabber-mam--classify-direction jc from to type))
         (direction (car dir-peer))
         (peer (cdr dir-peer))
         (timestamp (and stamp (jabber-parse-time stamp)))
         (oob-entries (jabber-db--extract-oob-entries inner-msg)))
    (list :from from :to to :type type :body body
          :stanza-id stanza-id :our-jid our-jid
          :direction direction :peer peer
          :timestamp timestamp :oob-entries oob-entries)))

(defun jabber-mam--store-new-message-p (jc inner-msg)
  "Return non-nil when INNER-MSG should be stored as a new message for JC."
  (not (run-hook-with-args-until-success
        'jabber-history-inhibit-received-message-functions
        jc inner-msg)))

(defun jabber-mam--message-action (jc inner-msg fields)
  "Return how MAM should handle INNER-MSG with FIELDS for JC."
  (cond
   ((or (not (plist-get fields :peer))
        (not (plist-get fields :body)))
    :unwrap)
   ((jabber-message-correct--replace-id inner-msg) :correct)
   ((jabber-mam--store-new-message-p jc inner-msg) :store)
   (t :unwrap)))

(defun jabber-mam--process-message (jc xml-data)
  "Handle a MAM result <message> from the message chain.
JC is the Jabber connection.  XML-DATA is the stanza."
  (when-let* ((result-el (jabber-xml-child-with-xmlns
                          xml-data jabber-mam-xmlns))
              (qid (jabber-xml-get-attribute result-el 'queryid))
              (query (cl-find qid jabber-mam--syncing
                              :key (lambda (entry) (plist-get entry :id))
                              :test #'equal))
              (page (plist-get query :page))
              ((jabber-mam--valid-sender-p
                jc (jabber-xml-get-attribute xml-data 'from) query))
              (parsed (jabber-mam--parse-result xml-data)))
    (let* ((archive-id (nth 0 parsed))
           (stamp (nth 1 parsed))
           (inner-msg (nth 2 parsed))
           (encrypted (jabber-xml-encrypted-p inner-msg))
           (inner-msg
            (let ((inhibit-message t)
                  (message-log-max nil))
              (jabber-chat--decrypt-if-needed jc inner-msg)))
           (fields (jabber-mam--extract-fields jc inner-msg stamp))
           (peer (plist-get fields :peer))
           (body (plist-get fields :body))
           (timestamp (and-let* ((time (plist-get fields :timestamp)))
                        (floor (float-time time))))
           (action (jabber-mam--message-action jc inner-msg fields)))
      (when (and (eq page (plist-get query :page))
                 (jabber-mam--current-query-p jc query))
        (pcase action
          ((or :correct :store)
           (pcase action
             (:correct
              (unless (jabber--decrypt-failure-body-p body)
                (jabber-message-correct--apply
                 (jabber-message-correct--replace-id inner-msg)
                 body (plist-get fields :from)
                 (string= (plist-get fields :type) "groupchat") nil
                 (jabber-db--extract-occupant-id inner-msg)
                 (plist-get fields :our-jid) peer nil)))
             (:store
              (let ((jabber-db-message-thread-stored-functions nil))
                (jabber-db-store-message
                 (plist-get fields :our-jid) peer
                 (plist-get fields :direction) (plist-get fields :type)
                 body timestamp (jabber-jid-resource (plist-get fields :from))
                 (plist-get fields :stanza-id) archive-id
                 (jabber-db--extract-occupant-id inner-msg)
                 (plist-get fields :oob-entries) encrypted
                 (jabber-db--extract-reply-fields inner-msg)
                 (jabber-db--extract-thread-fields inner-msg)))))
           (jabber-mam--mark-dirty jc peer (plist-get fields :type))
           (setcdr (cdr xml-data) nil))
          (:unwrap
           (jabber-mam--unwrap-into xml-data inner-msg archive-id)))))))

(defun jabber-mam--our-muc-nick-p (room nick jc)
  "Return non-nil if NICK in ROOM is us on connection JC.
Checks the current room nickname first, then falls back to
comparing with the account username to handle nick changes."
  (require 'jabber-muc)
  (or (and-let* ((my-nick (jabber-muc-nickname room jc)))
        (string= nick my-nick))
      (string= nick (plist-get (fsm-get-state-data jc) :username))))


(defun jabber-mam--mark-dirty (jc peer type)
  "Record that PEER's buffer needs redisplay after sync.
JC identifies the local account.  TYPE is the message type."
  (let ((entry (list (jabber-connection-bare-jid jc) peer type)))
    (cl-pushnew entry jabber-mam--dirty-peers :test #'equal)))


(defun jabber-mam--redraw-dirty ()
  "Signal that accumulated dirty peers need display refresh.
Drains `jabber-mam--dirty-peers' and runs
`jabber-mam-sync-complete-functions'."
  (let ((peers (prog1 jabber-mam--dirty-peers
                 (setq jabber-mam--dirty-peers nil))))
    (when peers
      (jabber-lifecycle--dispatch-contained
       'jabber-mam-sync-complete-functions peers))))

;;; Shared transaction management

(defun jabber-mam--tx-begin ()
  "Increment the MAM transaction ref count.
BEGIN a SQLite transaction when transitioning from 0 to 1."
  (when (zerop jabber-mam--tx-depth)
    (when-let* ((db (jabber-db-ensure-open)))
      (sqlite-execute db "BEGIN")))
  (cl-incf jabber-mam--tx-depth))

(defun jabber-mam--tx-end ()
  "Decrement the MAM transaction ref count.
COMMIT the SQLite transaction when transitioning from 1 to 0."
  (when (> jabber-mam--tx-depth 0)
    (cl-decf jabber-mam--tx-depth)
    (when (zerop jabber-mam--tx-depth)
      (when-let* ((db (jabber-db-ensure-open)))
        (sqlite-execute db "COMMIT")))))

;;; Query and pagination

(defun jabber-mam--query (jc &optional after-id queryid with start to
                             before-id max callback result-callback)
  "Start a MAM query on JC, paginating from AFTER-ID.
QUERYID correlates results; generate it if nil.  WITH and START are filters.
TO is nil for the personal archive, or a room bare JID.
BEFORE-ID requests one backward page of MAX messages.
Call CALLBACK without arguments once on settlement, including failure.
Then call RESULT-CALLBACK once with `success', `failed' or `cancelled'.
Success means accepted query completion, including a bounded backward page,
not complete archive coverage or physical COMMIT while other pages remain.
Failure means a permanent IQ error, local begin/send error, or incomplete
forward pagination without cursor progress.  Cancellation means retirement
of the room, connection or session, or caller quit (which is re-signaled).
Intermediate pages and the first stale-cursor retry do not settle the query.
Retire ownership before attempting each callback independently; contain their
errors and quits without changing the outcome.  Callbacks can run before this
function returns, including on synchronous begin/send failure.
Return the owned query record."
  (let ((query (list :id (or queryid (jabber-mam--make-queryid)) :jc jc
                     :current-p (jabber-mam--session-predicate jc)
                     :with with :start start :to to :before before-id
                     :max (or max jabber-mam-page-size) :after after-id
                     :callback callback :result-callback result-callback
                     :page nil :timer nil :iq-id nil
                     :transaction nil :retried nil)))
    (push query jabber-mam--syncing)
    (jabber-mam--send-page query)
    query))

(defun jabber-mam--send-page (query)
  "Send the next page of QUERY only while its owner remains current."
  (when (and (memq query jabber-mam--syncing)
             (not (plist-get query :page)))
    (plist-put query :timer nil)
    (if (not (jabber-mam--current-query-p (plist-get query :jc) query))
        (jabber-mam--complete-query query 'cancelled)
      (let ((page (list nil)))
        (plist-put query :page page)
        (plist-put query :iq-id (jabber-mam--make-queryid))
        (condition-case err
            (progn
              (jabber-mam--tx-begin)
              (plist-put query :transaction t)
              (jabber-send-iq
               (plist-get query :jc) (plist-get query :to) "set"
               (jabber-mam--build-query
                (plist-get query :id) (plist-get query :with)
                (plist-get query :start) (plist-get query :after)
                (plist-get query :max) (plist-get query :before))
               #'jabber-mam--handle-fin (cons query page)
               #'jabber-mam--handle-error (cons query page)
               (plist-get query :iq-id)
               (lambda (jc xml)
                 (and (eq page (plist-get query :page))
                      (jabber-mam--valid-sender-p
                       jc (jabber-xml-get-attribute xml 'from) query)))))
          ((error quit)
           (jabber-mam--complete-query
            query (if (eq (car err) 'quit) 'cancelled 'failed))
           (if (eq (car err) 'quit)
               (signal (car err) (cdr err))
             (message "MAM: query failed to send: %s"
                      (error-message-string err)))))))))

(defun jabber-mam--reply-query (jc xml-data closure)
  "Return the active query owned by JC for XML-DATA and CLOSURE."
  (let ((query (car closure)))
    (when (and (eq (cdr closure) (plist-get query :page))
               (jabber-mam--valid-sender-p
                jc (jabber-xml-get-attribute xml-data 'from) query))
      query)))

(defun jabber-mam--handle-fin (jc xml-data closure)
  "Settle or paginate JC's owned page from XML-DATA and CLOSURE."
  (when-let* ((query (jabber-mam--reply-query jc xml-data closure)))
    (let* ((fin (jabber-mam--parse-fin xml-data))
           (last-id (plist-get fin :last)))
      (cond
       ((or (plist-get fin :complete) (plist-get query :before))
        (jabber-mam--complete-query query 'success))
       ((or (null last-id) (equal last-id (plist-get query :after)))
        (jabber-mam--complete-query query 'failed))
       (t
        (jabber-mam--release-page query)
        (plist-put query :after last-id)
        ;; Keep the query registered while waiting so teardown can retire it.
        (plist-put query :timer
                   (run-with-timer 0.1 nil #'jabber-mam--send-page query)))))))

(defun jabber-mam--handle-error (jc xml-data closure)
  "Settle JC's failed page from XML-DATA and CLOSURE.
Retry a stale forward cursor once without that cursor, preserving filters."
  (when-let* ((query (jabber-mam--reply-query jc xml-data closure)))
    (let ((error-el (car (jabber-xml-get-children xml-data 'error))))
      (if (and (plist-get query :after)
               (not (plist-get query :before))
               (not (plist-get query :retried))
               (car (jabber-xml-get-children error-el 'item-not-found)))
          (progn
            (jabber-mam--release-page query)
            (plist-put query :after nil)
            (plist-put query :retried t)
            (jabber-mam--send-page query))
        (jabber-mam--complete-query query 'failed)
        (message "MAM: query failed: %s" (jabber-sexp2xml xml-data))))))

;;; Post-connect catch-up

(defun jabber-mam--initial-start ()
  "Return the configured initial catch-up timestamp, or nil."
  (when jabber-mam-catch-up-days
    (format-time-string
     "%Y-%m-%dT%H:%M:%SZ"
     (time-subtract (current-time) (* jabber-mam-catch-up-days 86400)) t)))

(defun jabber-mam--catch-up (jc)
  "Sync missed messages for JC via MAM."
  (let ((last-id (jabber-db-last-server-id (jabber-connection-bare-jid jc))))
    (jabber-mam--query jc last-id nil nil
                       (unless last-id (jabber-mam--initial-start)))))

(defun jabber-mam-maybe-catchup (jc)
  "Post-connect hook on JC: sync messages via MAM if enabled.
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

(defun jabber-mam--chat-catch-up (jc peer token)
  "Sync PEER's archive on JC for automatic catch-up TOKEN."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account peer))
         (start (unless last-id (jabber-mam--initial-start))))
    (jabber-mam--query
     jc last-id nil peer start nil nil nil
     (lambda () (jabber-mam--finish-peer-sync jc peer token)))))

(defun jabber-mam--handle-chat-disco (jc closure-data result)
  "Start a peer catch-up after disco completes.
JC is the connection.  CLOSURE-DATA is (PEER TOKEN).  RESULT is
the disco response."
  (pcase-let ((`(,peer ,token) closure-data))
    (when (jabber-mam--peer-sync-active-p jc peer token)
      (if (and (listp result)
               (not (eq (car result) 'error))
               (member jabber-mam-xmlns (cadr result)))
          (condition-case err
              (jabber-mam--chat-catch-up jc peer token)
            (error
             (jabber-mam--finish-peer-sync jc peer token)
             (message "MAM: peer catch-up failed to start: %s"
                      (error-message-string err))))
        (jabber-mam--finish-peer-sync jc peer token)))))

(defun jabber-mam-chat-opened (jc peer)
  "Trigger 1:1 MAM catch-up when opening a chat with PEER.
JC is the Jabber connection.  Called from `jabber-chat-with'.
Sets the syncing indicator immediately; clears it when the catch-up
query completes (or when disco reveals MAM is not supported)."
  (when-let* ((token (and jabber-mam-enable
                          (jabber-mam--begin-peer-sync jc peer))))
    (run-hook-with-args 'jabber-mam-peer-syncing-functions peer "chat" t)
    (condition-case err
        (jabber-disco-get-info
         jc (jabber-connection-bare-jid jc) nil
         #'jabber-mam--handle-chat-disco (list peer token))
      (error
       (jabber-mam--finish-peer-sync jc peer token)
       (signal (car err) (cdr err))))))

;;; MUC MAM catch-up

(defun jabber-mam--muc-catch-up (jc group)
  "Sync GROUP's archive on JC, then clear its syncing indicator."
  (let* ((account (jabber-connection-bare-jid jc))
         (last-id (jabber-db-last-server-id account group))
         (start (unless last-id (jabber-mam--initial-start))))
    (jabber-mam--query
     jc last-id nil nil start group nil nil
     (lambda ()
       (jabber-lifecycle--dispatch-contained
        'jabber-mam-peer-syncing-functions group "groupchat" nil)))))

(defun jabber-mam-muc-joined (jc group)
  "Trigger MUC MAM catch-up after joining GROUP.
JC is the Jabber connection.  Called from MUC self-presence handler.
Sets the syncing indicator immediately; clears it when the catch-up
query completes (or when disco reveals MAM is not supported)."
  (when jabber-mam-enable
    (run-hook-with-args 'jabber-mam-peer-syncing-functions
                        group "groupchat" t)
    (jabber-disco-get-info
     jc group nil
     (lambda (jc closure-data result)
       (let ((group (car closure-data)))
         (if (and (listp result)
                  (not (eq (car result) 'error))
                  (member jabber-mam-xmlns (cadr result)))
             (jabber-mam--muc-catch-up jc group)
           (run-hook-with-args 'jabber-mam-peer-syncing-functions
                               group "groupchat" nil))))
     (list group))))

(defun jabber-mam-sync-buffer ()
  "Sync recent messages from this buffer's server archive.
Fetch at most `jabber-chat-buffer-msg-count' messages.  Store new messages
and update existing messages through deduplication, including successful
re-decryption of placeholders.  Never infer deletion from archive absence:
a bounded page, even a successful one, is not evidence of remote deletion.
Refresh the buffer after settlement."
  (interactive)
  (unless (memq jabber-buffer-connection jabber-connections)
    (user-error "Not connected"))
  (let* ((jc jabber-buffer-connection)
         (count (jabber-chat-buffer-msg-count))
         (group (bound-and-true-p jabber-group))
         (peer (or group
                   (jabber-jid-user (bound-and-true-p jabber-chatting-with))))
         (type (if group "groupchat" "chat")))
    (run-hook-with-args 'jabber-mam-peer-syncing-functions peer type t)
    (jabber-mam--mark-dirty jc peer type)
    (jabber-mam--query
     jc nil nil (unless group peer) nil group t count
     (lambda ()
       (jabber-lifecycle--dispatch-contained
        'jabber-mam-peer-syncing-functions peer type nil)))))

;;; Disconnect cleanup

(defun jabber-mam--cleanup-connection (jc)
  "Retire JC's queries and automatic catch-ups on disconnect."
  (dolist (query (copy-sequence jabber-mam--syncing))
    (when (eq jc (plist-get query :jc))
      (jabber-mam--complete-query query 'cancelled)))
  (dolist (entry (copy-sequence jabber-mam--peer-syncing))
    (when (eq (caar entry) jc)
      (jabber-mam--finish-peer-sync jc (cadar entry) (cdr entry)))))

(defun jabber-mam--cleanup-all ()
  "Retire all MAM queries and automatic catch-ups on disconnect."
  (dolist (query (copy-sequence jabber-mam--syncing))
    (jabber-mam--complete-query query 'cancelled))
  (dolist (entry (copy-sequence jabber-mam--peer-syncing))
    (jabber-mam--finish-peer-sync (caar entry) (cadar entry) (cdr entry))))

;;; MUC query cancellation

(defun jabber-mam--cancel-muc-query (room &optional jc)
  "Retire MAM queries and pending pagination for ROOM on JC.
When JC is nil, intentionally retire this room's queries on all accounts."
  (dolist (query (copy-sequence jabber-mam--syncing))
    (when (and (equal room (plist-get query :to))
               (or (null jc) (eq jc (plist-get query :jc))))
      (jabber-mam--complete-query query 'cancelled))))

;;; Registration

(jabber-disco-advertise-feature jabber-mam-xmlns)

(jabber-chain-add 'jabber-message-chain #'jabber-mam--process-message -10)

(add-hook 'jabber-post-connect-hooks #'jabber-mam-maybe-catchup)
(add-hook 'jabber-pre-disconnect-hook #'jabber-mam--cleanup-all)
(add-hook 'jabber-lifecycle-session-reset-functions #'jabber-mam--cleanup-connection)
(add-hook 'jabber-lost-connection-hooks #'jabber-mam--cleanup-connection)

(provide 'jabber-mam)
;;; jabber-mam.el ends here
