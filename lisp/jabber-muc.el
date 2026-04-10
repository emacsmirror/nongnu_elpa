;;; jabber-muc.el --- advanced MUC functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
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
;;

;;; Code:

(require 'cl-lib)

(require 'jabber-widget)
(require 'jabber-disco)

;; we need jabber-bookmarks for jabber-muc-autojoin (via
;; jabber-get-bookmarks and jabber-parse-conference-bookmark):
(require 'jabber-bookmarks)

(require 'ewoc)

(defconst jabber-muc-xmlns "http://jabber.org/protocol/muc"
  "XEP-0045 MUC namespace.")

(defconst jabber-muc-xmlns-user "http://jabber.org/protocol/muc#user"
  "XEP-0045 MUC user namespace.")

(defconst jabber-muc-xmlns-owner "http://jabber.org/protocol/muc#owner"
  "XEP-0045 MUC owner namespace.")

(defconst jabber-muc-xmlns-admin "http://jabber.org/protocol/muc#admin"
  "XEP-0045 MUC admin namespace.")

(defconst jabber-muc-xmlns-direct-invite "jabber:x:conference"
  "XEP-0249 Direct MUC Invitations namespace.")

(defvar jabber-muc--rooms (make-hash-table :test #'equal)
  "Internal hash table of active MUC rooms.
Keys are group JID strings; values are lists of (JC . NICKNAME)
cons cells, one per connection that has joined the room.  This
allows multiple accounts to be in the same room simultaneously.")

(defvar jabber-muc--generation 0
  "Generation counter for `jabber-muc--rooms'.
Incremented on every join/leave, enabling cheap change detection
without copying the room list.")

(defun jabber-muc-nickname (group &optional jc)
  "Return our nickname in GROUP, or nil.
If JC is given, return the nickname for that specific connection.
Otherwise return the nickname from the first entry."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if jc
        (alist-get jc entries)
      (cdar entries))))

(defun jabber-muc-connection (group)
  "Return a connection object for GROUP, or nil.
When multiple accounts are in the same room, returns the first."
  (caar (gethash group jabber-muc--rooms)))

(defun jabber-muc-joined-p (group &optional jc)
  "Return non-nil if we are in GROUP.
If JC is given, check whether that specific connection is in GROUP."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if jc
        (and (assq jc entries) t)
      (and entries t))))

(defun jabber-muc-our-nick-p (group nick)
  "Return non-nil if NICK is our nickname in GROUP on any connection."
  (let ((entries (gethash group jabber-muc--rooms)))
    (cl-some (lambda (entry) (string= nick (cdr entry))) entries)))

(defun jabber-muc-room-entries (group)
  "Return list of (JC . NICKNAME) entries for GROUP."
  (gethash group jabber-muc--rooms))

(defun jabber-muc-active-rooms ()
  "Return list of joined room JIDs."
  (hash-table-keys jabber-muc--rooms))

(defun jabber-muc-join-set (group jc nickname)
  "Record that we joined GROUP via JC with NICKNAME."
  (let ((entries (gethash group jabber-muc--rooms)))
    (if-let* ((existing (assq jc entries)))
        (setcdr existing nickname)
      (push (cons jc nickname) entries))
    (puthash group entries jabber-muc--rooms))
  (cl-incf jabber-muc--generation))

(defun jabber-muc-leave-remove (group &optional jc)
  "Remove GROUP from active rooms.
If JC is given, only remove that connection's entry; the room
stays tracked if other connections remain in it."
  (if jc
      (let ((entries (gethash group jabber-muc--rooms)))
        (setq entries (assq-delete-all jc entries))
        (if entries
            (puthash group entries jabber-muc--rooms)
          (remhash group jabber-muc--rooms)))
    (remhash group jabber-muc--rooms))
  (cl-incf jabber-muc--generation))

(defun jabber-muc-generation ()
  "Return current generation counter for change detection."
  jabber-muc--generation)


(defvar jabber-pending-groupchats (make-hash-table)
  "Hash table of groupchats and nicknames.
Keys are JID symbols; values are strings.
This table records the last nickname used to join the particular
chat room.  Items are thus never removed.")

(defvar jabber-muc-participants nil
  "Alist of groupchats and participants.
Keys are strings, the bare JID of the room.
Values are lists of nickname strings.")

(defvar jabber-group nil
  "The groupchat you are participating in.")

(defvar jabber-muc-topic ""
  "The topic of the current MUC room.")

(defvar-local jabber-muc--auto-configure nil
  "When non-nil, automatically open the config form on room creation.
Set by `jabber-muc-create' and consumed by `jabber-muc--enter-extra-notices'.")

(defvar jabber-role-history ()
  "Keeps track of previously used roles.")

(defvar jabber-affiliation-history ()
  "Keeps track of previously used affiliations.")

(defvar jabber-muc-nickname-history ()
  "Keeps track of previously referred-to nicknames.")

(defvar jabber-muc--rooms-before-disconnect nil
  "Alist of (ROOM . NICK) saved before disconnect.
Used to rejoin non-bookmarked rooms on reconnect.")

(defvar jabber-muc--autojoin-queue nil
  "Alist of (JC . ((COUNT GROUP . NICK) ...)) for prioritized MUC autojoin.
Each entry is sorted by COUNT (occupant count from disco#items).
Rooms with fewer occupants join first.  COUNT is
`most-positive-fixnum' for rooms whose disco query failed.")

(defvar jabber-muc--autojoin-timer nil
  "Timer for autojoin timeout fallback.
If a self-presence doesn't arrive within the timeout, advance
to the next queued room.")

(defvar jabber-muc--autojoin-pending nil
  "Alist of (JC . ((GROUP . NICK) ...)) awaiting disco#items query.
Rooms are moved from here into `jabber-muc--autojoin-queue' as
disco results arrive.  Only `jabber-muc-autojoin-max-disco'
queries are in-flight at once to avoid saturating the SM window.")

(defvar jabber-muc--autojoin-disco-count nil
  "Alist of (JC . COUNT) tracking in-flight disco#items queries.")

(defcustom jabber-muc-autojoin-max-disco 5
  "Maximum concurrent disco#items queries during autojoin.
Limits how many disco queries are in-flight simultaneously to
avoid saturating the SM back-pressure window."
  :type 'natnum
  :group 'jabber-muc)

(defcustom jabber-muc-autojoin-timeout 10
  "Seconds to wait for a MUC self-presence before joining the next room.
During staggered autojoin, if the server doesn't respond within
this many seconds, the room is skipped and the next one is tried."
  :type 'integer
  :group 'jabber-muc)

;;; MUC status codes (XEP-0045)
(defconst jabber-muc-status-self-presence    "110")
(defconst jabber-muc-status-room-created     "201")
(defconst jabber-muc-status-nick-modified    "210")
(defconst jabber-muc-status-banned           "301")
(defconst jabber-muc-status-nick-changed     "303")
(defconst jabber-muc-status-kicked           "307")
(defconst jabber-muc-status-nick-not-allowed "406")
(defconst jabber-muc-status-nick-conflict    "409")

(defcustom jabber-muc-default-nicknames nil
  "Default nickname for specific MUC rooms."
  :group 'jabber-chat
  :type '(repeat
	  (cons :format "%v"
		(string :tag "JID of room")
		(string :tag "Nickname"))))

(defcustom jabber-muc-autojoin nil
  "List of MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'."
  :group 'jabber-chat
  :type '(repeat (string :tag "JID of room")))

(defcustom jabber-muc-disable-disco-check nil
  "If non-nil, disable checking disco#info of rooms before joining them.
Disco information can tell whether the room exists and whether it is
password protected, but some servers do not support it.  If you want
to join chat rooms on such servers, set this variable to t."
  :group 'jabber-chat
  :type 'boolean)

(defcustom jabber-muc-self-ping-interval 180
  "Seconds between periodic MUC self-pings.
Set to 0 to disable.  When non-zero, all joined rooms are pinged
at this interval to detect silent server-side drops.  Rooms that
fail are automatically rejoined.  See XEP-0410."
  :type 'natnum
  :group 'jabber-chat)

(defvar jabber-muc--self-ping-timer nil
  "Timer for periodic MUC self-pings.")

(defcustom jabber-groupchat-buffer-format "*#%n-%a*"
  "The format specification for the name of groupchat buffers.

These fields are available (all are about the group you are chatting
in):

%n   Roster name of group, or JID if no nickname set
%b   Name of group from bookmarks or roster name or JID if none set
%j   Bare JID (without resource)

These fields are about your account:

%a   Your bare JID (account)
%u   Your username
%s   Your server"
  :type 'string
  :group 'jabber-chat)


(defcustom jabber-muc-header-line-format
  '(" " (:eval (propertize (jabber-jid-displayname jabber-group) 'face 'shadow))
    " " (:eval jabber-chat-encryption-message)	;see jabber-chatbuffer.el
    (:eval (when jabber-chat-mam-syncing
	     (propertize " [syncing]" 'face 'shadow))))
  "The specification for the header line of MUC buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

(defcustom jabber-muc-private-buffer-format "*%g/%n-%a*"
  "The format specification for the buffer name for private MUC messages.

These fields are available:

%g   Roster name of group, or JID if no nickname set
%n   Nickname of the group member you're chatting with

These fields are about your account:

%a   Your bare JID (account)
%u   Your username
%s   Your server"
  :type 'string
  :group 'jabber-chat)


(defcustom jabber-muc-print-names-format "	%n	%a	%j\n"
  "The format specification for MUC list lines.

Fields available:

%n  Nickname in room
%a  Affiliation status
%j  Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-private-header-line-format
  '(" " (:eval (jabber-jid-resource jabber-chatting-with))
    " in " (:eval (jabber-jid-displayname (jabber-jid-user jabber-chatting-with)))
    "\t" (:eval jabber-chatstates-message)
    " " (:eval jabber-chat-encryption-message))	;see jabber-chatbuffer.el
  "The specification for the header line of private MUC chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

;; Global reference declarations

(declare-function jabber-chain-add "jabber-core"
                  (chain-var handler &optional depth))
(declare-function jabber-presence-children "jabber-presence.el" (jc))
(declare-function jabber-vcard-get "jabber-vcard.el" (jc jid))
(declare-function jabber-get-version "jabber-version.el" (jc to))
(declare-function jabber-get-disco-info "jabber-disco.el" (jc to &optional node))
(declare-function jabber-disco-get-items "jabber-disco.el"
                  (jc jid node callback closure-data &optional force))
(declare-function jabber-ping-send "jabber-ping.el"
                  (jc to process-func on-success on-error))
(declare-function jabber-process-ping "jabber-ping.el"
                  (_jc xml-data))
(declare-function jabber-parse-conference-bookmark "jabber-bookmarks.el"
                  (node))
(declare-function jabber-get-bookmarks-from-cache "jabber-bookmarks"
                  (jc))
(declare-function jabber-send-sexp "jabber-core.el"  (jc sexp))
(declare-function jabber-chat--run-send-hooks "jabber-chat.el"
                  (stanza body id))
(declare-function jabber-chat-send "jabber-chat.el"
                  (jc body &optional extra-elements))
(declare-function jabber-send-message "jabber-chat.el"
                  (jc to subject body type))
(declare-function jabber-maybe-print-rare-time "jabber-chat.el" (node))
(declare-function jabber-chat-pp "jabber-chat.el" (data))
(declare-function jabber-chat-mode "jabber-chatbuffer.el" ())
(declare-function jabber-chat-mode-setup "jabber-chatbuffer.el" (jc ewoc-pp))
(declare-function jabber-chat-ewoc-enter "jabber-chatbuffer.el" (data))
(declare-function jabber-chatbuffer--registry-put "jabber-chatbuffer" (type key))
(declare-function jabber-chatbuffer--registry-get "jabber-chatbuffer" (type key))
(declare-function jabber-chat-insert-backlog-entry "jabber-chat.el" (msg-plist))
(declare-function jabber-chat--insert-backlog-chunked "jabber-chat.el"
                  (buffer entries callback &optional generation))
(declare-function jabber-chat-display-buffer-images "jabber-chat.el" ())
(declare-function jabber-chat--msg-plist-from-stanza "jabber-chat.el"
                  (xml-data &optional delayed))
(declare-function jabber-chat--insert-prompt "jabber-chat.el"
                  (timestamp nick face &optional plaintext-face encrypted))
(declare-function jabber-chat--format-time "jabber-chat.el"
                  (timestamp delayed))
(declare-function jabber-omemo--send-muc "jabber-omemo.el" (jc body &optional extra-elements))
(declare-function jabber-omemo--prefetch-sessions "jabber-omemo" (jc jid))
(declare-function jabber-omemo--prefetch-muc-sessions "jabber-omemo" (jc group))
(declare-function jabber-openpgp--send-muc "jabber-openpgp.el" (jc body &optional extra-elements))
(declare-function jabber-openpgp-legacy--send-muc "jabber-openpgp-legacy.el" (jc body &optional extra-elements))
(declare-function jabber-chat--decrypt-if-needed "jabber-chat.el" (jc xml-data))
(declare-function jabber-db-last-timestamp "jabber-db.el"
                  (account peer))
(declare-function jabber-db-get-chat-encryption "jabber-db.el"
                  (account peer))
(declare-function jabber-chat-encryption--update-header "jabber-chatbuffer.el"
                  ())
(declare-function jabber-mam-muc-joined "jabber-mam.el" (jc group))
(declare-function jabber-mam--cancel-muc-query "jabber-mam.el" (room))
(declare-function jabber-bookmarks-auto-add-maybe "jabber-bookmarks.el"
                  (jc jid nick))
(declare-function jabber-db-backlog "jabber-db.el"
                  (account peer &optional count start-time resource msg-type))
(declare-function jabber-message-correct--replace-id "jabber-message-correct"
                  (xml-data))
(declare-function jabber-message-correct--apply "jabber-message-correct"
                  (replace-id new-body new-from muc-p buffer))
(defvar jabber-silent-mode)             ; jabber.el
(defvar jabber-message-chain)           ; jabber-core.el
(defvar jabber-alert-muc-function)      ; jabber-alert.el
(defvar jabber-body-printers)           ; jabber-chat.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-chat-delayed-time-format) ; jabber-chat.el
(defvar jabber-chat-delayed-time-format) ; jabber-chat.el
(defvar jabber-chat-encryption)         ; jabber-chatbuffer.el
(defvar jabber-chat-send-hooks)         ; jabber-chat.el
(defvar jabber-chat-ewoc)               ; jabber-chatbuffer.el
(defvar jabber-chat--backlog-generation) ; jabber-chatbuffer.el
(defvar jabber-chat-printers)           ; jabber-chat.el
(defvar jabber-chat-time-format)        ; jabber-chat.el
(defvar jabber-connections)             ; jabber-core.el
(defvar jabber-post-disconnect-hook)   ; jabber-core.el
(defvar jabber-send-function)           ; jabber-console.el
(defvar jabber-xdata-xmlns)            ; jabber-xml.el
(defvar jabber-delay-xmlns)            ; jabber-xml.el
(defvar jabber-delay-legacy-xmlns)     ; jabber-xml.el

;;

;;;###autoload
(defvar jabber-muc-printers '()
  "List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

;;;###autoload
(defun jabber-muc-get-buffer (group &optional jc)
  "Return the chat buffer name for chatroom GROUP.
When JC is provided, account-specific format specs (%a, %u, %s) are
expanded.  Either a string or a buffer is returned, so use `get-buffer'
or `get-buffer-create'."
  (format-spec jabber-groupchat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname group))
                (cons ?b (jabber-jid-bookmarkname group))
		(cons ?j (jabber-jid-user group))
		(cons ?a (if jc (jabber-connection-bare-jid jc) ""))
		(cons ?u (if jc (plist-get (fsm-get-state-data jc) :username) ""))
		(cons ?s (if jc (plist-get (fsm-get-state-data jc) :server) "")))))

(defun jabber-muc-find-buffer (group)
  "Find an existing MUC buffer for GROUP, or nil."
  (jabber-chatbuffer--registry-get 'muc group))

(defun jabber-muc-create-buffer (jc group)
  "Prepare a buffer for chatroom GROUP.
This function is idempotent.

JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-muc-get-buffer group jc))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode)

      (setq-local jabber-group group)
      (setq-local jabber-muc-topic nil)

      (jabber-chat-mode-setup jc #'jabber-chat-pp)

      (setq jabber-send-function #'jabber-muc-send)
      (setq header-line-format jabber-muc-header-line-format)

      (setq-local jabber-chat-earliest-backlog nil)
      (when (null jabber-chat-earliest-backlog)
        (let ((backlog-entries (jabber-db-backlog
                                (jabber-connection-bare-jid jc)
                                (jabber-jid-user group)
                                nil nil nil "groupchat")))
          (if (null backlog-entries)
              (setq jabber-chat-earliest-backlog (float-time))
            (setq jabber-chat-earliest-backlog
                  (float-time (plist-get (car (last backlog-entries)) :timestamp)))
            (cl-incf jabber-chat--backlog-generation)
            (jabber-chat--insert-backlog-chunked
             (current-buffer) backlog-entries
             #'jabber-chat-display-buffer-images
             jabber-chat--backlog-generation))))

      (when-let* ((win (get-buffer-window (current-buffer))))
        (with-selected-window win
          (goto-char jabber-point-insert)
          (recenter -1))))

    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)
    (jabber-chatbuffer--registry-put 'muc group)

    (current-buffer)))

;;;###autoload
(defun jabber-muc-private-get-buffer (group nickname &optional jc)
  "Return the chat buffer name for private chat with NICKNAME in GROUP.
When JC is provided, account-specific format specs (%a, %u, %s) are
expanded.  Either a string or a buffer is returned, so use `get-buffer'
or `get-buffer-create'."
  (format-spec jabber-muc-private-buffer-format
	       (list
		(cons ?g (jabber-jid-displayname group))
		(cons ?n nickname)
		(cons ?a (if jc (jabber-connection-bare-jid jc) ""))
		(cons ?u (if jc (plist-get (fsm-get-state-data jc) :username) ""))
		(cons ?s (if jc (plist-get (fsm-get-state-data jc) :server) "")))))

(defun jabber-muc-private-find-buffer (group nickname)
  "Find an existing MUC private buffer for GROUP/NICKNAME, or nil."
  (jabber-chatbuffer--registry-get 'muc-private (format "%s/%s" group nickname)))

(defun jabber-muc-private-create-buffer (jc group nickname)
  "Prepare a buffer for chatting with NICKNAME in GROUP.
This function is idempotent.

JC is the Jabber connection."
  (with-current-buffer (get-buffer-create (jabber-muc-private-get-buffer group nickname jc))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode)
      ;; Set jabber-chatting-with before mode-setup so the DB peer
      ;; lookup uses the correct JID.
      (setq-local jabber-chatting-with (concat group "/" nickname))
      (jabber-chat-mode-setup jc #'jabber-chat-pp)
      ;; MUC private messages are addressed to an occupant JID, not a
      ;; real bare JID, so OMEMO/OpenPGP session setup cannot work.
      ;; Default to plaintext like MUC buffers.
      (unless (jabber-db-get-chat-encryption
               (jabber-connection-bare-jid jc)
               (jabber-jid-user jabber-chatting-with))
        (setq jabber-chat-encryption 'plaintext)
        (jabber-chat-encryption--update-header)))

    (setq-local jabber-chatting-with (concat group "/" nickname))
    (jabber-chatbuffer--registry-put 'muc-private (format "%s/%s" group nickname))
    (setq jabber-send-function #'jabber-chat-send)
    (setq header-line-format jabber-muc-private-header-line-format)

    (setq-local jabber-chat-earliest-backlog nil)
    (when (null jabber-chat-earliest-backlog)
      (let ((backlog-entries (jabber-db-backlog
                              (jabber-connection-bare-jid jc)
                              group nil nil nickname)))
        (if (null backlog-entries)
            (setq jabber-chat-earliest-backlog (float-time))
          (setq jabber-chat-earliest-backlog
                (float-time (plist-get (car (last backlog-entries)) :timestamp)))
          (cl-incf jabber-chat--backlog-generation)
          (jabber-chat--insert-backlog-chunked
           (current-buffer) backlog-entries
           #'jabber-chat-display-buffer-images
           jabber-chat--backlog-generation))))

    (current-buffer)))

(defun jabber-muc-send (jc body &optional extra-elements)
  "Send BODY to MUC room in current buffer.

JC is the Jabber connection.
EXTRA-ELEMENTS, when non-nil, is a list of XML sexp elements to
splice into the stanza after the body (e.g. XEP-0308 replace)."
  ;; There is no need to display the sent message in the buffer, as
  ;; we will get it back from the MUC server.
  (pcase jabber-chat-encryption
    ('omemo
     (require 'jabber-omemo)
     (jabber-omemo--send-muc jc body extra-elements))
    ('openpgp
     (require 'jabber-openpgp)
     (jabber-openpgp--send-muc jc body extra-elements))
    ('openpgp-legacy
     (require 'jabber-openpgp-legacy)
     (jabber-openpgp-legacy--send-muc jc body extra-elements))
    (_
     (let* ((id (format "emacs-msg-%.6f" (float-time)))
            (stanza `(message
                      ((to . ,jabber-group)
                       (type . "groupchat")
                       (id . ,id))
                      (body () ,body)
                      ,@extra-elements)))
       (jabber-chat--run-send-hooks stanza body id)
       (jabber-send-sexp jc stanza)))))

(defun jabber-muc-add-groupchat (group nickname &optional jc)
  "Remember participating in GROUP under NICKNAME via JC."
  (jabber-muc-join-set group jc nickname))

(defun jabber-muc-remove-groupchat (group &optional jc)
  "Remove GROUP from internal bookkeeping.
If JC is given, only remove that connection's entry."
  (jabber-muc-leave-remove group jc)
  (jabber-mam--cancel-muc-query group)
  ;; Only clear participants when no account remains in the room.
  (unless (jabber-muc-joined-p group)
    (let ((whichparticipants (assoc group jabber-muc-participants)))
      (setq jabber-muc-participants
	    (delq whichparticipants jabber-muc-participants)))))

(defun jabber-muc-connection-closed (bare-jid)
  "Remove MUC data for BARE-JID, saving room list for reconnect.
Forget all information about rooms that had been entered with
this JID.  The room list is saved to `jabber-muc--rooms-before-disconnect'
so non-bookmarked rooms can be rejoined on reconnect.  When
multiple accounts share a room, only the disconnecting account's
entry is removed."
  (let (snapshot)
    (dolist (room (jabber-muc-active-rooms))
      (let* ((entries (jabber-muc-room-entries room))
             (match (cl-find bare-jid entries
                             :key (lambda (e)
                                    (and (car e)
                                         (jabber-connection-bare-jid (car e))))
                             :test #'string=)))
        (when match
          (push (cons room (cdr match)) snapshot)
          ;; Clear autojoin queue for this connection.
          (jabber-muc--autojoin-clear (car match))
          (jabber-muc-leave-remove room (car match))
          ;; Only clear participants when no account remains in the room.
          (unless (jabber-muc-joined-p room)
            (let ((whichparticipants (assoc room jabber-muc-participants)))
              (setq jabber-muc-participants
                    (delq whichparticipants jabber-muc-participants)))))))
    (setq jabber-muc--rooms-before-disconnect snapshot)))

(defun jabber-muc--self-ping-failed (jc xml-data closure-data)
  "Handle failed MUC self-ping per XEP-0410 error classification.
JC is the connection.  XML-DATA is the IQ error stanza.
CLOSURE-DATA is (ROOM . NICK).

Error conditions per XEP-0410:
- service-unavailable, feature-not-implemented, item-not-found:
  still joined (ping target doesn't support XEP-0199)
- remote-server-not-found, remote-server-timeout:
  undecided, treat as transient failure
- any other error (e.g. not-acceptable): not joined, rejoin"
  (let* ((room (car closure-data))
         (nick (cdr closure-data))
         (error-node (jabber-iq-error xml-data))
         (condition (when error-node
                      (jabber-error-condition error-node))))
    (pcase condition
      ((or 'service-unavailable 'feature-not-implemented 'item-not-found)
       (message "MUC self-ping for %s: still joined (ping unsupported)" room))
      ((or 'remote-server-not-found 'remote-server-timeout)
       (message "MUC self-ping for %s: server unreachable, will retry" room))
      (_
       (message "MUC self-ping failed for %s (%s), rejoining"
                room (or condition "unknown"))
       (jabber-muc-remove-groupchat room jc)
       (let ((password (jabber-get-conference-data jc room nil :password)))
         (jabber-muc--send-join-presence jc room nick password nil))))))

(defun jabber-muc--self-ping-one (jc group)
  "Self-ping GROUP via JC to verify membership.
On success, does nothing.  On failure, classifies the error per
XEP-0410 and auto-rejoins if needed."
  (let ((nick (jabber-muc-nickname group jc)))
    (if (not nick)
        (message "MUC self-ping: no nick for %s, skipping" group)
      (let ((self-jid (format "%s/%s" group nick))
            (closure (cons group nick)))
        (jabber-send-iq
         jc self-jid "get"
         '(ping ((xmlns . "urn:xmpp:ping")))
         #'ignore
         nil
         #'jabber-muc--self-ping-failed
         closure)))))

(defun jabber-muc-self-ping-rooms (jc)
  "Ping all joined MUC rooms via JC to verify membership.
After SM resume, the MUC server may have kicked us while offline.
Rooms that fail the self-ping are rejoined automatically.
XEP-0410: MUC Self-Ping (Schroedingers Chat)."
  (let ((bare-jid (jabber-connection-bare-jid jc)))
    (dolist (room (jabber-muc-active-rooms))
      (let ((room-jc (jabber-muc-connection room)))
        (when (and room-jc (string= bare-jid (jabber-connection-bare-jid room-jc)))
          (jabber-muc--self-ping-one jc room))))))

(defun jabber-muc-self-ping-start (&optional _jc)
  "Start periodic MUC self-ping timer.
Pings all joined rooms on all connections every
`jabber-muc-self-ping-interval' seconds.  Suitable for
`jabber-post-connect-hooks'."
  (jabber-muc-self-ping-stop)
  (when (> jabber-muc-self-ping-interval 0)
    (setq jabber-muc--self-ping-timer
          (run-with-timer jabber-muc-self-ping-interval
                          jabber-muc-self-ping-interval
                          #'jabber-muc--self-ping-all-connections))
    (add-hook 'jabber-post-disconnect-hook #'jabber-muc-self-ping-stop)))

(defun jabber-muc-self-ping-stop ()
  "Cancel periodic MUC self-ping timer."
  (when jabber-muc--self-ping-timer
    (cancel-timer jabber-muc--self-ping-timer)
    (setq jabber-muc--self-ping-timer nil)))

(defun jabber-muc--self-ping-all-connections ()
  "Self-ping rooms on all active connections."
  (dolist (jc jabber-connections)
    (when (and jc (plist-get (fsm-get-state-data jc) :ever-session-established))
      (jabber-muc-self-ping-rooms jc))))

(defun jabber-muc-participant-plist (group nickname)
  "Return plist associated with NICKNAME in GROUP.
Return nil if nothing known about that combination."
  (let ((whichparticipants (assoc group jabber-muc-participants)))
    (when whichparticipants
      (cdr (assoc nickname whichparticipants)))))

(defun jabber-muc--merge-plist (old new)
  "Merge NEW plist into OLD, returning the result.
Keys in NEW overwrite OLD.  Keys in OLD not present in NEW are preserved."
  (let ((result (copy-sequence (or old '()))))
    (cl-loop for (key val) on new by #'cddr
             do (setq result (plist-put result key val)))
    result))

(defun jabber-muc-modify-participant (group nickname new-plist)
  "Assign properties in NEW-PLIST to NICKNAME in GROUP.
Existing properties not present in NEW-PLIST are preserved."
  (let ((participants (assoc group jabber-muc-participants)))
    ;; either we have a list of participants already...
    (if participants
	(let ((participant (assoc nickname participants)))
	  ;; and maybe this participant is already in the list
	  (if participant
	      ;; if so, merge to preserve keys like jid across updates
	      (setf (cdr participant)
		    (jabber-muc--merge-plist (cdr participant) new-plist))
	    (push (cons nickname new-plist) (cdr participants))))
      ;; or we don't
      (push (cons group (list (cons nickname new-plist))) jabber-muc-participants))))

(defun jabber-muc--format-affiliation-change (nickname from to actor-reason)
  "Generate message for affiliation transition of NICKNAME.
FROM and TO are the old and new affiliation strings.
ACTOR-REASON is the pre-formatted \" by actor: reason\" suffix.
Return a string describing the change, or nil if unrecognized."
  ;; There are many ways to express these transitions in English.
  ;; This one favors eloquence over regularity and consistency.
  (cond
   ;; Higher affiliation
   ((or (and (member from '("outcast" "none" "member"))
             (member to '("admin" "owner")))
        (and (string= from "admin") (string= to "owner")))
    (concat nickname " has been promoted to " to actor-reason))
   ;; Lower affiliation
   ((or (and (member from '("owner" "admin"))
             (string= to "member"))
        (and (string= from "owner") (string= to "admin")))
    (concat nickname " has been demoted to " to actor-reason))
   ;; Become member
   ((string= to "member")
    (concat nickname " has been granted membership" actor-reason))
   ;; Lose membership
   ((string= to "none")
    (concat nickname " has been deprived of membership" actor-reason))))

(defun jabber-muc--format-role-change (nickname from to actor-reason)
  "Generate message for role transition of NICKNAME.
FROM and TO are the old and new role strings.
ACTOR-REASON is the pre-formatted \" by actor: reason\" suffix.
Return a string describing the change, or nil."
  ;; Possible roles are "none" (not in room, hence not of interest
  ;; in this function), "visitor" (no voice), "participant" (has
  ;; voice), and "moderator".
  (cond
   ((string= to "moderator")
    (concat nickname " has been granted moderator privileges" actor-reason))
   ((and (string= from "moderator")
         (string= to "participant"))
    (concat nickname " had moderator privileges revoked" actor-reason))
   ((string= to "participant")
    (concat nickname " has been granted voice" actor-reason))
   ((string= to "visitor")
    (concat nickname " has been denied voice" actor-reason))))

(defun jabber-muc-report-delta (nickname old-plist new-plist reason actor)
  "Compare OLD-PLIST and NEW-PLIST, and return a string explaining the change.
Return nil if nothing noteworthy has happened.
NICKNAME is the user experiencing the change.  REASON and ACTOR, if non-nil,
are the corresponding presence fields.

This function is only concerned with presence stanzas resulting
in the user entering/staying in the room."
  ;; The keys in the plist are affiliation, role and jid.
  (let ((display-nick (if (plist-get new-plist 'jid)
                         (concat nickname " <"
                                 (jabber-jid-user (plist-get new-plist 'jid))
                                 ">")
                       nickname)))
    (cond
     ((null old-plist)
      (concat display-nick " enters the room ("
              (plist-get new-plist 'role)
              (unless (string= (plist-get new-plist 'affiliation) "none")
                (concat ", " (plist-get new-plist 'affiliation)))
              ")"))

     ;; If affiliation changes, the role change is usually the logical
     ;; one, so don't report it separately.
     ((not (string= (plist-get old-plist 'affiliation)
                    (plist-get new-plist 'affiliation)))
      (let ((actor-reason (concat (when actor
                                    (concat " by " actor))
                                  (when reason
                                    (concat ": " reason)))))
        (jabber-muc--format-affiliation-change
         display-nick
         (plist-get old-plist 'affiliation)
         (plist-get new-plist 'affiliation)
         actor-reason)))

     ;; Role changes
     ((not (string= (plist-get old-plist 'role)
                    (plist-get new-plist 'role)))
      (let ((actor-reason (concat (when actor
                                    (concat " by " actor))
                                  (when reason
                                    (concat ": " reason)))))
        (jabber-muc--format-role-change
         display-nick
         (plist-get old-plist 'role)
         (plist-get new-plist 'role)
         actor-reason))))))

(defun jabber-muc-remove-participant (group nickname)
  "Forget everything about NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    (when participants
      (let ((participant (assoc nickname (cdr participants))))
	(setf (cdr participants) (delq participant (cdr participants)))))))

(defmacro jabber-muc-argument-list (&optional args)
  "Prepend connection and group name to ARGS.
If the current buffer is not an MUC buffer, signal an error.
This macro is meant for use as an argument to `interactive'."
  `(if (null jabber-group)
       (error "Not in MUC buffer")
     (nconc (list jabber-buffer-connection jabber-group) ,args)))

(defun jabber-muc-read-completing (prompt &optional allow-not-joined)
  "Read the name of a joined chatroom, or use chatroom of current buffer if any.
If ALLOW-NOT-JOINED is provided and non-nil, permit choosing any
JID; only provide completion as a guide."
  (or jabber-group
      (let ((rooms (jabber-muc-active-rooms)))
        (jabber-read-jid-completing prompt
                                    (if (null rooms)
                                        (error "You haven't joined any group")
                                      (mapcar #'jabber-jid-symbol rooms))
                                    (not allow-not-joined)
                                    jabber-group))))

(defun jabber-muc-read-nickname (group prompt)
  "Read the nickname of a participant in GROUP."
  (let ((nicknames (cdr (assoc group jabber-muc-participants))))
    (unless nicknames
      (error "Unknown group: %s" group))
    (completing-read prompt nicknames nil nil nil 'jabber-muc-nickname-history)))


;;;###autoload
(defun jabber-muc-vcard-get (jc group nickname)
  "Request vcard from chat with NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
    (let ((muc-name (format "%s/%s" group nickname)))
	(jabber-vcard-get jc muc-name)))

;;;###autoload
(defun jabber-muc-get-version (jc group nickname)
  "Request software version from NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (jabber-get-version jc (format "%s/%s" group nickname)))

;;;###autoload
(defun jabber-muc-get-disco-info (jc group nickname)
  "Request disco info from NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (jabber-get-disco-info jc (format "%s/%s" group nickname)))

;;;###autoload
(defun jabber-muc-ping (jc group nickname)
  "Ping NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (jabber-ping-send jc (format "%s/%s" group nickname)
                    #'jabber-silent-process-data
                    #'jabber-process-ping "Ping is unsupported"))

(defun jabber-muc-instant-config (jc group)
  "Accept default configuration for GROUP.
This can be used for a newly created room, as an alternative to
filling out the configuration form with `jabber-muc-get-config'.
Both of these methods unlock the room, so that other users can
enter it.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "set"
		  `(query ((xmlns . ,jabber-muc-xmlns-owner))
			  (x ((xmlns . ,jabber-xdata-xmlns) (type . "submit"))))
		  #'jabber-report-success "MUC instant configuration"
		  #'jabber-report-success "MUC instant configuration"))


(defun jabber-muc-get-config (jc group)
  "Ask for MUC configuration form.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "get"
		  `(query ((xmlns . ,jabber-muc-xmlns-owner)))
		  #'jabber-process-data #'jabber-muc-render-config
		  #'jabber-process-data "MUC configuration request failed"))

(defun jabber-muc-render-config (jc xml-data)
  "Render MUC configuration form.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((query (jabber-iq-query xml-data))
	xdata)
    (dolist (x (jabber-xml-get-children query 'x))
      (if (string= (jabber-xml-get-attribute x 'xmlns) jabber-xdata-xmlns)
	  (setq xdata x)))
    (if (not xdata)
	(message "No configuration possible.")
      (save-window-excursion
	(jabber-widget-init-buffer (jabber-xml-get-attribute xml-data 'from))
	(setq jabber-buffer-connection jc)
	(jabber-widget-render-xdata-form xdata)
	(widget-create 'push-button :notify #'jabber-muc-submit-config "Submit")
	(widget-insert "\t")
	(widget-create 'push-button :notify #'jabber-muc-cancel-config "Cancel")
	(widget-insert "\n")
	(widget-setup)
	(widget-minor-mode 1)
	(recursive-edit)))))

(defun jabber-muc-submit-config (&rest _ignore)
  "Submit MUC configuration form."
  (jabber-send-iq jabber-buffer-connection jabber-widget-submit-to
		  "set"
		  `(query ((xmlns . ,jabber-muc-xmlns-owner))
			  ,(jabber-widget-parse-xdata-form))
		  #'jabber-report-success "MUC configuration"
		  #'jabber-report-success "MUC configuration")
  (exit-recursive-edit))

(defun jabber-muc-cancel-config (&rest _ignore)
  "Cancel MUC configuration form."
  (jabber-send-iq jabber-buffer-connection jabber-widget-submit-to
		  "set"
		  `(query ((xmlns . ,jabber-muc-xmlns-owner))
			  (x ((xmlns . ,jabber-xdata-xmlns) (type . "cancel"))))
		  nil nil nil nil)
  (exit-recursive-edit))


(defun jabber-muc--validate-disco-result (result)
  "Classify a disco#info RESULT for MUC join.
Return a plist describing the outcome:
  (:status ok :features FEATURES)  - valid MUC service
  (:status not-found)              - item-not-found
  (:status no-disco)               - feature-not-implemented
  (:status not-conference)         - not a conference
  (:status error :error-msg STR)   - other error"
  (let* ((identities (car result))
         (features (cadr result))
         (condition (when (eq identities 'error)
                      (jabber-error-condition result))))
    (cond
     ((eq condition 'item-not-found)
      '(:status not-found))
     ((eq condition 'feature-not-implemented)
      (list :status 'no-disco :features features))
     (condition
      (list :status 'error :error-msg (jabber-parse-error result)))
     ((and (eq identities 'error) (not condition))
      (list :status 'error :error-msg "Bad error stanza received"))
     ((cl-find "conference" (if (sequencep identities) identities nil)
               :key (lambda (i) (aref i 1))
               :test #'string=)
      (list :status 'ok :features features))
     (t
      '(:status not-conference)))))

(defun jabber-muc--room-completions (jc)
  "Return completion candidates for rooms available to JC.
Includes joined rooms and bookmarked rooms for this connection."
  (let ((rooms (make-hash-table :test #'equal)))
    ;; Joined rooms for this connection
    (maphash (lambda (group entries)
               (when (assq jc entries)
                 (puthash group t rooms)))
             jabber-muc--rooms)
    ;; Bookmarked rooms
    (let ((bookmarks (jabber-get-bookmarks-from-cache jc)))
      (when (listp bookmarks)
        (dolist (bm bookmarks)
          (when-let* ((jid (plist-get bm :jid)))
            (puthash jid t rooms)))))
    (hash-table-keys rooms)))

(defun jabber-muc-join (jc group nickname &optional popup)
  "Join a groupchat, or change nick.
In interactive calls, or if POPUP is non-nil, switch to the
groupchat buffer.

JC is the Jabber connection."
  (interactive
   (let* ((account (jabber-read-account))
	  (group (completing-read "Groupchat: "
				  (jabber-muc--room-completions account)
				  nil nil nil nil))
          (joined (jabber-muc-joined-p group account)))
     (list account
           group
           (if joined
               (jabber-muc-nickname group account)
             (or (jabber-muc-nickname group account)
                 (jabber-muc-read-my-nickname account group)))
           t)))
  ;; Remove from autojoin queue to prevent double-join.
  (jabber-muc--autojoin-dequeue jc group)
  (cond
   ;; Already joined: open buffer, sync and verify membership.
   ((jabber-muc-joined-p group jc)
    (when popup
      (switch-to-buffer (jabber-muc-create-buffer jc group)))
    (jabber-mam-muc-joined jc group)
    (jabber-muc--self-ping-one jc group))
   ;; Skip disco check if configured.
   (jabber-muc-disable-disco-check
    (jabber-muc--send-join-presence jc group nickname nil popup))
   (t
    (jabber-disco-get-info jc group nil #'jabber-muc--disco-callback
			   (list group nickname popup)))))

;;;###autoload
(defun jabber-muc-create (jc group nickname)
  "Create a new MUC room and open its configuration form.
Send join presence to GROUP with NICKNAME.  When the server
confirms creation (status 201), the room configuration form
opens automatically.

JC is the Jabber connection."
  (interactive
   (let* ((account (jabber-read-account))
          (servers (let (s)
                     (maphash (lambda (room _)
                                (let ((host (jabber-jid-server room)))
                                  (unless (member host s) (push host s))))
                              jabber-muc--rooms)
                     (nreverse s)))
          (server (completing-read "MUC server: " servers nil nil))
          (name   (read-string "Room name: "))
          (group  (concat name "@" server)))
     (list account group (jabber-muc-read-my-nickname account ""))))
  (jabber-muc--send-join-presence jc group nickname nil t t)
  (jabber-bookmarks--publish-one jc group nickname))

;;;###autoload
(defun jabber-muc-switch-to (group)
  "Switch to an active groupchat buffer.
Prompt with completion for joined rooms only."
  (interactive
   (list (completing-read "Groupchat: "
			  (jabber-muc-active-rooms)
			  nil t)))
  (let* ((jc (jabber-muc-connection group))
         (buffer (if jc
                     (get-buffer (jabber-muc-get-buffer group jc))
                   (jabber-muc-find-buffer group))))
    (if buffer
	(switch-to-buffer buffer)
      ;; Buffer was killed; recreate it.
      (when (setq jc (or jc (car jabber-connections)))
        (switch-to-buffer (jabber-muc-create-buffer jc group))))))

(defun jabber-muc--disco-callback (jc closure result)
  "Disco callback for MUC join.
JC is the Jabber connection.  CLOSURE is (GROUP NICKNAME POPUP).
RESULT is the disco#info result."
  (pcase-let ((`(,group ,nickname ,popup) closure))
    (let* ((v (jabber-muc--validate-disco-result result))
           (status (plist-get v :status)))
      (pcase status
        ('not-found
         (unless (or jabber-silent-mode
                     (y-or-n-p (format "%s doesn't exist.  Create it? "
                                       (jabber-jid-displayname group))))
           (error "Non-existent groupchat")))
        ('error
         (message "Couldn't query groupchat: %s" (plist-get v :error-msg)))
        ('not-conference
         (message "%s is not a conference service" (jabber-jid-displayname group))))
      (unless (eq status 'not-conference)
        (let ((password
               (when (member "muc_passwordprotected" (plist-get v :features))
                 (or
                  (jabber-get-conference-data jc group nil :password)
                  (read-passwd (format "Password for %s: "
                                       (jabber-jid-displayname group)))))))
          (jabber-muc--send-join-presence jc group nickname password popup))))))

(defalias 'jabber-muc-join-2 #'jabber-muc--disco-callback)

(defun jabber-muc--send-join-presence (jc group nickname password popup
                                          &optional auto-configure)
  "Send MUC join presence for GROUP with NICKNAME.
PASSWORD is the room password, or nil.  When POPUP is non-nil,
switch to the MUC buffer.  When AUTO-CONFIGURE is non-nil, set
`jabber-muc--auto-configure' in the buffer so the config form
opens on room creation.

JC is the Jabber connection."
  ;; Remember that this is a groupchat _before_ sending the stanza.
  ;; The response might come quicker than you think.
  (puthash (jabber-jid-symbol group) nickname jabber-pending-groupchats)

  (jabber-send-sexp jc
		    `(presence ((to . ,(format "%s/%s" group nickname)))
			       (x ((xmlns . ,jabber-muc-xmlns))
				  (history ((maxchars . "0")))
				  ,@(when password
				      `((password () ,password))))
			       ,@(jabber-presence-children jc)))

  ;; There, stanza sent.  Now we just wait for the MUC service to
  ;; mirror the stanza.  This is handled in
  ;; `jabber-muc-process-presence', where a buffer will be created for
  ;; the room.

  ;; But if the user interactively asked to join, he/she probably
  ;; wants the buffer to pop up right now.
  (when popup
    (let ((buffer (jabber-muc-create-buffer jc group)))
      (when auto-configure
        (with-current-buffer buffer
          (setq jabber-muc--auto-configure t)))
      (switch-to-buffer buffer))))

(defalias 'jabber-muc-join-3 #'jabber-muc--send-join-presence)

(defun jabber-muc-read-my-nickname (jc group &optional default)
  "Read nickname for joining GROUP.
If DEFAULT is non-nil, return default nick without prompting.

JC is the Jabber connection."
  (let ((default-nickname (or
			   (jabber-get-conference-data jc group nil :nick)
			   (cdr (assoc group jabber-muc-default-nicknames))
			   (plist-get (fsm-get-state-data jc) :username))))
    (if default
        default-nickname
        (jabber-read-with-input-method (format "Nickname: (default %s) "
					   default-nickname)
				   nil nil default-nickname))))

;;;###autoload
(defun jabber-muc-nick (jc group nickname)
  "Change nickname in GROUP to NICKNAME.
JC is the Jabber connection."
  (interactive
   (let* ((group (or (and (eq major-mode 'jabber-chat-mode)
                          (bound-and-true-p jabber-group))
                     (completing-read "Groupchat: "
                                      (jabber-muc-active-rooms) nil t)))
          (jc (or (jabber-muc-connection group)
                  (jabber-read-account)))
          (current (jabber-muc-nickname group jc))
          (new-nick (read-string
                     (format "New nickname (current: %s): " current)
                     nil nil current)))
     (list jc group new-nick)))
  (jabber-send-sexp jc
                    `(presence ((to . ,(format "%s/%s" group nickname))))))

(defun jabber-muc-leave (jc group)
  "Leave a groupchat.

JC is the Jabber connection."
  (interactive (jabber-muc-argument-list))
  (let ((nick (jabber-muc-nickname group jc)))
    ;; send unavailable presence to our own nick in room
    (jabber-send-sexp jc
		      `(presence ((to . ,(format "%s/%s" group nick))
				  (type . "unavailable")))))
  (jabber-bookmarks--retract-one jc group))


(defvar-local jabber-muc-names--group nil
  "Room JID for the current participants buffer.")

(define-derived-mode jabber-muc-names-mode tabulated-list-mode
  "MUC-Names"
  "Major mode for displaying MUC participant lists."
  (setq tabulated-list-format [("Nick" 20 t)
                                ("Role" 12 t)
                                ("Affiliation" 12 t)
                                ("JID" 30 t)])
  (tabulated-list-init-header))

(defun jabber-muc-names ()
  "Display participants of the current room in a tabulated-list buffer."
  (interactive)
  (let* ((group jabber-group)
         (participants (cdr (assoc group jabber-muc-participants)))
         (buf (get-buffer-create (format "*MUC Participants: %s*"
                                        (jabber-jid-displayname group)))))
    (with-current-buffer buf
      (jabber-muc-names-mode)
      (setq jabber-muc-names--group group)
      (setq tabulated-list-entries
            (mapcar (lambda (p)
                      (let ((nick (car p))
                            (props (cdr p)))
                        (list nick (vector
                                    nick
                                    (or (plist-get props 'role) "")
                                    (or (plist-get props 'affiliation) "")
                                    (or (plist-get props 'jid) "")))))
                    participants))
      (tabulated-list-print))
    (display-buffer buf)))


(defun jabber-muc-set-topic (jc group topic)
  "Set topic of GROUP to TOPIC.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-read-with-input-method "New topic: " jabber-muc-topic))))
  (jabber-send-message jc group topic nil "groupchat"))

(defun jabber-muc-snarf-topic (xml-data)
  "Record subject (topic) of the given <message/>, if any.

XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((body (car
	       (jabber-xml-node-children
		(car
		 (jabber-xml-get-children xml-data 'body)))))
	(new-topic (jabber-xml-path xml-data '(subject ""))))
    (when (and new-topic (not body))
      (setq jabber-muc-topic new-topic))))


(defun jabber-muc-set-role (jc group nickname role reason)
  "Set role of NICKNAME in GROUP to ROLE, specifying REASON.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (list nickname
	    (completing-read "New role: " '(("none") ("visitor") ("participant") ("moderator")) nil t nil 'jabber-role-history)
	    (read-string "Reason: ")))))
  (unless (or (zerop (length nickname)) (zerop (length role)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . ,jabber-muc-xmlns-admin))
			    (item ((nick . ,nickname)
				   (role . ,role))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Role change"
		    'jabber-report-success "Role change")))


(defun jabber-muc-set-affiliation (jc group nickname-or-jid nickname-p affiliation reason)
  "Set affiliation of NICKNAME-OR-JID in GROUP to AFFILIATION.
If NICKNAME-P is non-nil, NICKNAME-OR-JID is a nickname in the
group, else it is a JID.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname-p (y-or-n-p "Specify user by room nickname? ")))
      (list
       (if nickname-p
	   (jabber-muc-read-nickname jabber-group "Nickname: ")
	 (jabber-read-jid-completing "User: "))
       nickname-p
       (completing-read "New affiliation: "
			'(("none") ("outcast") ("member") ("admin") ("owner")) nil t nil 'jabber-affiliation-history)
       (read-string "Reason: ")))))
  (let ((jid
	 (if nickname-p
	     (let ((participants (cdr (assoc group jabber-muc-participants))))
	       (unless participants
		 (error "Couldn't find group %s" group))
	       (let ((participant (cdr (assoc nickname-or-jid participants))))
		 (unless participant
		   (error "Couldn't find %s in group %s" nickname-or-jid group))
		 (or (plist-get participant 'jid)
		     (error "JID of %s in group %s is unknown" nickname-or-jid group))))
	   nickname-or-jid)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . ,jabber-muc-xmlns-admin))
			    (item ((jid . ,jid)
				   (affiliation . ,affiliation))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Affiliation change"
		    'jabber-report-success "Affiliation change")))


(defun jabber-muc-invite (jc jid group reason)
  "Invite JID to GROUP, stating REASON.
Uses XEP-0249 direct invitations.

JC is the Jabber connection."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing
          "Invite whom: "
          ;; The current room is _not_ a good default for whom to invite.
          (remq (jabber-jid-symbol jabber-group) (jabber-concat-rosters)))
	 (jabber-muc-read-completing "To group: ")
	 (jabber-read-with-input-method "Reason: ")))
  (jabber-send-sexp
   jc
   `(message ((to . ,jid))
	     (x ((xmlns . ,jabber-muc-xmlns-direct-invite)
		 (jid . ,group)
		 ,@(unless (zerop (length reason))
		     `((reason . ,reason))))))))

;; FIXME: If this file is loaded before `jabber-chat', it will prevent
;; `jabber-body-printers' to have its default set of functions, because
;; the var will have been set here already.
(add-hook 'jabber-body-printers #'jabber-muc-print-invite)

(defun jabber-muc--parse-mediated-invite (xml-data)
  "Parse XEP-0045 mediated invite from XML-DATA.
Return (GROUP INVITER REASON) or nil."
  (cl-dolist (x (jabber-xml-get-children xml-data 'x))
    (when (string= (jabber-xml-get-attribute x 'xmlns) jabber-muc-xmlns-user)
      (when-let* ((invitation (car (jabber-xml-get-children x 'invite)))
                  (group (jabber-xml-get-attribute xml-data 'from)))
        (let ((inviter (jabber-xml-get-attribute invitation 'from))
              (reason (car (jabber-xml-node-children
                            (car (jabber-xml-get-children
                                  invitation 'reason))))))
          (cl-return (list group inviter reason)))))))

(defun jabber-muc--parse-direct-invite (xml-data)
  "Parse XEP-0249 direct invite from XML-DATA.
Return (GROUP INVITER REASON) or nil."
  (cl-dolist (x (jabber-xml-get-children xml-data 'x))
    (when (string= (jabber-xml-get-attribute x 'xmlns)
                   jabber-muc-xmlns-direct-invite)
      (let ((group (jabber-xml-get-attribute x 'jid))
            (inviter (jabber-xml-get-attribute xml-data 'from))
            (reason (jabber-xml-get-attribute x 'reason)))
        (when (and group (not (jabber-muc-joined-p group)))
          (cl-return (list group inviter reason)))))))

(defun jabber-muc--insert-invite (group inviter reason &optional mediated-p)
  "Insert MUC invitation UI for GROUP from INVITER with REASON.
When MEDIATED-P is non-nil, include a Decline button."
  ;; XXX: password
  (insert "You have been invited to MUC room "
          (jabber-jid-displayname group))
  (when inviter
    (insert " by " (jabber-jid-displayname inviter)))
  (insert ".")
  (when (and reason (not (zerop (length reason))))
    (insert "  Reason: " reason))
  (insert "\n\n")
  (let ((action (lambda (&rest _ignore) (interactive)
                  (jabber-muc-join jabber-buffer-connection group
                                  (jabber-muc-read-my-nickname
                                   jabber-buffer-connection group)))))
    (insert-button "Accept" 'action action))
  (when mediated-p
    (insert "\t")
    (let ((action (lambda (&rest _ignore) (interactive)
                    (let ((reason (jabber-read-with-input-method "Reason: ")))
                      (jabber-send-sexp
                       jabber-buffer-connection
                       `(message
                         ((to . ,group))
                         (x ((xmlns . ,jabber-muc-xmlns-user))
                            (decline
                             ((to . ,inviter))
                             ,(unless (zerop (length reason))
                                `(reason nil ,reason))))))))))
      (insert-button "Decline" 'action action))))

(defun jabber-muc-print-invite (msg _who mode)
  "Print MUC invitation from message plist MSG.
Requires :xml-data key in MSG for raw stanza access."
  (when-let* ((xml-data (plist-get msg :xml-data)))
    (or (when-let* ((parsed (jabber-muc--parse-mediated-invite xml-data)))
          (when (eql mode :insert)
            (jabber-muc--insert-invite (nth 0 parsed) (nth 1 parsed)
                                       (nth 2 parsed) t))
          t)
        (when-let* ((parsed (jabber-muc--parse-direct-invite xml-data)))
          (when (eql mode :insert)
            (jabber-muc--insert-invite (nth 0 parsed) (nth 1 parsed)
                                       (nth 2 parsed)))
          t))))

(defun jabber-muc--autojoin-insert (jc count group nick)
  "Insert (COUNT GROUP . NICK) into the sorted autojoin queue for JC.
The queue is kept sorted ascending by COUNT so rooms with fewer
occupants are joined first."
  (let* ((new-entry (cons count (cons group nick)))
         (cell (assq jc jabber-muc--autojoin-queue)))
    (if (not cell)
        (push (cons jc (list new-entry)) jabber-muc--autojoin-queue)
      (let ((rooms (cdr cell))
            (inserted nil)
            (prev nil))
        (while (and rooms (not inserted))
          (if (< count (caar rooms))
              (progn
                (if prev
                    (setcdr prev (cons new-entry rooms))
                  (setcdr cell (cons new-entry rooms)))
                (setq inserted t))
            (setq prev rooms
                  rooms (cdr rooms))))
        (unless inserted
          (if prev
              (setcdr prev (list new-entry))
            (setcdr cell (list new-entry))))))))

(defun jabber-muc--autojoin-cancel-timer ()
  "Cancel the autojoin timeout timer if running."
  (when (timerp jabber-muc--autojoin-timer)
    (cancel-timer jabber-muc--autojoin-timer)
    (setq jabber-muc--autojoin-timer nil)))

(defun jabber-muc--autojoin-disco-callback (jc closure-data result)
  "Disco#items callback for autojoin prioritization.
JC is the connection.  CLOSURE-DATA is (GROUP . NICK).
RESULT is a list of item vectors on success or an error node."
  (let* ((group (car closure-data))
         (nick (cdr closure-data))
         (count (if (and (listp result) (eq (car result) 'error))
                    most-positive-fixnum
                  (length result))))
    ;; Decrement in-flight disco counter.
    (when-let* ((cell (assq jc jabber-muc--autojoin-disco-count)))
      (cl-decf (cdr cell)))
    (jabber-muc--autojoin-insert jc count group nick)
    ;; Fire more disco queries if slots are available.
    (jabber-muc--autojoin-fire-pending jc)
    ;; Start draining if no join is currently in-flight.
    ;; Defer via timer so Emacs can redisplay between joins.
    (unless jabber-muc--autojoin-timer
      (run-with-timer 0 nil #'jabber-muc--autojoin-next jc))))

(defun jabber-muc--autojoin-dequeue (jc group)
  "Remove GROUP from the autojoin queue for JC if present."
  (when-let* ((cell (assq jc jabber-muc--autojoin-queue)))
    (let ((rooms (cdr cell)))
      (setcdr cell (cl-remove-if (lambda (entry) (string= group (cadr entry)))
                                 rooms))
      (unless (cdr cell)
        (setq jabber-muc--autojoin-queue
              (assq-delete-all jc jabber-muc--autojoin-queue))))))

(defun jabber-muc--autojoin-timeout (jc)
  "Timer callback: advance to the next room when self-presence times out."
  (setq jabber-muc--autojoin-timer nil)
  (when (memq jc jabber-connections)
    (jabber-muc--autojoin-next jc)))

(defun jabber-muc--autojoin-next (jc)
  "Join the next room in the autojoin queue for JC.
Pops one entry (COUNT GROUP . NICK) and sends the join presence.
Starts a timeout timer so the queue advances even if the server
never responds.  Does nothing if the queue is empty."
  (jabber-muc--autojoin-cancel-timer)
  (when-let* ((entry (assq jc jabber-muc--autojoin-queue))
              (rooms (cdr entry)))
    (let* ((head (pop rooms))
           (group (cadr head))
           (nick (cddr head))
           (password (jabber-get-conference-data jc group nil :password)))
      (setcdr entry rooms)
      (unless rooms
        (setq jabber-muc--autojoin-queue
              (assq-delete-all jc jabber-muc--autojoin-queue)))
      (jabber-muc--send-join-presence jc group nick password nil)
      ;; Start timeout: if no self-presence arrives, try next room.
      (when (assq jc jabber-muc--autojoin-queue)
        (setq jabber-muc--autojoin-timer
              (run-with-timer jabber-muc-autojoin-timeout nil
                             #'jabber-muc--autojoin-timeout jc))))))

(defun jabber-muc--autojoin-queued-p (jc group)
  "Return non-nil if GROUP is already in the autojoin queue for JC."
  (when-let* ((entry (assq jc jabber-muc--autojoin-queue)))
    (cl-find group (cdr entry) :key #'cadr :test #'string=)))

(defun jabber-muc--autojoin-clear (jc)
  "Remove all autojoin queue entries for JC."
  (jabber-muc--autojoin-cancel-timer)
  (setq jabber-muc--autojoin-queue
        (assq-delete-all jc jabber-muc--autojoin-queue))
  (setq jabber-muc--autojoin-pending
        (assq-delete-all jc jabber-muc--autojoin-pending))
  (setq jabber-muc--autojoin-disco-count
        (assq-delete-all jc jabber-muc--autojoin-disco-count)))

(defun jabber-muc--rejoin-snapshot (jc)
  "Rejoin rooms from the pre-disconnect snapshot not already joined.
Called after bookmark autojoin to recover non-bookmarked rooms.
Rooms are added to the pending disco list for batched querying."
  (dolist (room-nick jabber-muc--rooms-before-disconnect)
    (let ((room (car room-nick))
          (nick (cdr room-nick)))
      (unless (or (jabber-muc-joined-p room jc)
                  (jabber-muc--autojoin-queued-p jc room))
        (jabber-muc--autojoin-enqueue-pending jc room nick))))
  (setq jabber-muc--rooms-before-disconnect nil))

(defun jabber-muc--autojoin-enqueue-pending (jc group nick)
  "Add (GROUP . NICK) to the pending disco list for JC."
  (if-let* ((cell (assq jc jabber-muc--autojoin-pending)))
      (setcdr cell (nconc (cdr cell) (list (cons group nick))))
    (push (cons jc (list (cons group nick))) jabber-muc--autojoin-pending)))

(defun jabber-muc--autojoin-fire-pending (jc)
  "Fire disco#items queries for JC up to the concurrency limit.
Moves rooms from `jabber-muc--autojoin-pending' into in-flight
disco queries, respecting `jabber-muc-autojoin-max-disco'."
  (let* ((count-cell (or (assq jc jabber-muc--autojoin-disco-count)
                         (car (push (cons jc 0) jabber-muc--autojoin-disco-count))))
         (pending-cell (assq jc jabber-muc--autojoin-pending)))
    (while (and pending-cell
                (cdr pending-cell)
                (< (cdr count-cell) jabber-muc-autojoin-max-disco))
      (let* ((room-nick (cadr pending-cell))
             (group (car room-nick))
             (nick (cdr room-nick)))
        (setcdr pending-cell (cddr pending-cell))
        (cl-incf (cdr count-cell))
        (jabber-disco-get-items jc group nil
                                #'jabber-muc--autojoin-disco-callback
                                (cons group nick))))
    ;; Clean up empty pending entry.
    (unless (cdr pending-cell)
      (setq jabber-muc--autojoin-pending
            (assq-delete-all jc jabber-muc--autojoin-pending)))))

(defun jabber-muc-autojoin (jc)
  "Join rooms specified in account bookmarks and global `jabber-muc-autojoin'.
Fires disco#items queries in batches (up to
`jabber-muc-autojoin-max-disco' at a time).  As results arrive,
rooms are inserted into a priority queue ordered by occupant
count (fewest first) and drained sequentially.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-muc--autojoin-clear jc)
  (when (bound-and-true-p jabber-muc-autojoin)
    (dolist (group jabber-muc-autojoin)
      (jabber-muc--autojoin-enqueue-pending
       jc group
       (or (cdr (assoc group jabber-muc-default-nicknames))
           (plist-get (fsm-get-state-data jc) :username)))))
  (jabber-muc--autojoin-fire-pending jc)
  (jabber-get-bookmarks
   jc
   (lambda (jc bookmarks)
     (dolist (bookmark bookmarks)
       (when (plist-get bookmark :autojoin)
         (let ((group (plist-get bookmark :jid)))
           (unless (or (jabber-muc-joined-p group jc)
                       (jabber-muc--autojoin-queued-p jc group))
             (jabber-muc--autojoin-enqueue-pending
              jc group
              (or (plist-get bookmark :nick)
                  (plist-get (fsm-get-state-data jc) :username)))))))
     (jabber-muc--rejoin-snapshot jc)
     (jabber-muc--autojoin-fire-pending jc))))

;;;###autoload
(defun jabber-muc-message-p (message)
  "Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites."
  ;; Public groupchat messages have type "groupchat" and are from
  ;; room@server/nick.  Public groupchat errors have type "error" and
  ;; are from room@server.
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (or
     (string= type "groupchat")
     (and (string= type "error")
	  (gethash (jabber-jid-symbol from) jabber-pending-groupchats))
     (jabber-xml-path message `((,jabber-muc-xmlns-user . "x") invite))
     ;; XEP-0249 direct invite
     (jabber-xml-path message
		      `((,jabber-muc-xmlns-direct-invite . "x"))))))

;;;###autoload
(defun jabber-muc-sender-p (jid)
  "Return non-nil if JID is a full JID of an MUC participant."
  (and (jabber-muc-joined-p (jabber-jid-user jid))
       (jabber-jid-resource jid)))

;;;###autoload
(defun jabber-muc-private-message-p (message)
  "Return non-nil if MESSAGE is a private message in a groupchat."
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (and
     (not (string= type "groupchat"))
     (jabber-muc-sender-p from))))


(defun jabber-muc-private (_jc group nickname)
  "Open private chat with NICKNAME in GROUP.

JC is the Jabber connection."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (switch-to-buffer (jabber-muc-private-create-buffer jabber-buffer-connection group nickname)))

(defun jabber-muc-presence-p (presence)
  "Return non-nil if PRESENCE is presence from groupchat."
  (let ((from (jabber-xml-get-attribute presence 'from))
	(type (jabber-xml-get-attribute presence 'type))
	(muc-marker (cl-find-if
		     (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
				   jabber-muc-xmlns-user))
		     (jabber-xml-get-children presence 'x))))
    ;; This is MUC presence if it has an MUC-namespaced tag...
    (or muc-marker
	;; ...or if it is error presence from a room we tried to join.
	(and (string= type "error")
	     (gethash (jabber-jid-symbol from) jabber-pending-groupchats)))))

(defun jabber-muc-parse-affiliation (x-muc)
  "Parse X-MUC in the muc#user namespace and return a plist.
Return nil if X-MUC is nil."
  ;; XXX: parse <actor/> and <reason/> tags?  or maybe elsewhere?
  (apply #'nconc (mapcar (lambda (prop) (list (car prop) (cdr prop)))
			 (jabber-xml-node-attributes
			  (car (jabber-xml-get-children x-muc 'item))))))

(defun jabber-muc-print-prompt (msg &optional local dont-print-nick-p)
  "Print MUC prompt for message plist MSG."
  (let ((nick (jabber-jid-resource (plist-get msg :from)))
	(timestamp (plist-get msg :timestamp))
	(delayed (plist-get msg :delayed)))
    (if (stringp nick)
	(jabber-chat--insert-prompt
	 (jabber-chat--format-time timestamp delayed)
	 (if dont-print-nick-p "" nick)
	 (if local
	     'jabber-chat-nick-plaintext
	   'jabber-chat-nick-foreign-plaintext))
      (jabber-muc-system-prompt))))

(defun jabber-muc-private-print-prompt (msg)
  "Print prompt for private MUC message plist MSG."
  (let* ((from (plist-get msg :from))
	 (timestamp (plist-get msg :timestamp))
	 (delayed (plist-get msg :delayed))
	 (nick (jabber-jid-resource from))
	 (group (jabber-jid-user from))
	 (group-name (or (jabber-jid-rostername group)
			 (jabber-jid-username group))))
    (jabber-chat--insert-prompt
     (jabber-chat--format-time timestamp delayed)
     (concat group-name "/" nick)
     'jabber-chat-nick-foreign-plaintext)))

(defun jabber-muc-system-prompt (&rest _ignore)
  "Print system prompt for MUC."
  (jabber-chat--insert-prompt
   (jabber-chat--format-time nil nil)
   ""
   'jabber-chat-nick-system))

(defun jabber-muc--classify-message (jc group nick xml-data)
  "Return message type for a MUC stanza.
JC is the connection that received the stanza.  GROUP is the room
JID, NICK is the sender's room nickname, and XML-DATA is the
parsed stanza.  Returns `:muc-error' if the stanza contains an
error child, `:muc-local' if NICK matches our own nickname in
GROUP on JC, or `:muc-foreign' otherwise."
  (cond
   ((jabber-xml-get-children xml-data 'error) :muc-error)
   ((and nick (string= nick (jabber-muc-nickname group jc))) :muc-local)
   (t :muc-foreign)))

(defun jabber-muc--history-message-p (xml-data)
  "Return non-nil if XML-DATA is a MUC history message per XEP-0045.
Per XEP-0045 section 7.2.15, a MUC history message has a <delay/>
element whose `from' attribute is the room JID.  Delay elements with
other `from' values (bridges, gateways, time corrections) indicate
live messages with extra metadata, not history."
  (when-let* ((delay (or (jabber-xml-child-with-xmlns xml-data jabber-delay-xmlns)
                         (jabber-xml-child-with-xmlns xml-data
                                                      jabber-delay-legacy-xmlns)))
              (delay-from (jabber-xml-get-attribute delay 'from))
              (msg-from (jabber-xml-get-attribute xml-data 'from)))
    (string= delay-from (jabber-jid-user msg-from))))

(defun jabber-muc--display-message (_jc xml-data group nick type msg-plist)
  "Display a MUC message and conditionally run alert hooks.
Insert an EWOC entry into the MUC buffer for GROUP.  _JC is the Jabber
connection, XML-DATA the parsed stanza, NICK the sender nickname, TYPE
one of `:muc-local', `:muc-foreign', or `:muc-error', and MSG-PLIST
the message property list.  Alert hooks are skipped for history
messages."
  (let ((error-p (eq type :muc-error))
        (printers (append jabber-muc-printers jabber-chat-printers))
        (body-text (plist-get msg-plist :body))
        (buffer (jabber-muc-find-buffer group)))
    ;; Only insert into EWOC when the buffer already exists.
    ;; Messages are persisted in the DB regardless; backlog loads
    ;; when the user opens the room.
    (when buffer
      (with-current-buffer buffer
        (jabber-muc-snarf-topic xml-data)
        ;; Skip ewoc insert for delayed (history) messages when
        ;; backlog was already loaded from DB, to avoid duplicates.
        ;; The DB handler stores them; backlog refresh will show them.
        (when (and (or error-p
                      (cl-some (lambda (f) (funcall f msg-plist type :printp))
                               printers))
                   (not (and (jabber-muc--history-message-p xml-data)
                             jabber-chat-earliest-backlog)))
          (jabber-maybe-print-rare-time
           (jabber-chat-ewoc-enter (list type msg-plist))))))
    ;; Alert hooks run regardless of buffer existence, but not for
    ;; history messages.
    (unless (jabber-muc--history-message-p xml-data)
      (let ((inhibit-message (and buffer (buffer-live-p buffer)
                                    (buffer-local-value
                                     'jabber-chat-mam-syncing buffer))))
        (dolist (hook '(jabber-muc-hooks jabber-alert-muc-hooks))
          (run-hook-with-args hook
                              nick group buffer body-text
                              (funcall jabber-alert-muc-function
                                       nick group buffer
                                       body-text)))))))

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-message-chain #'jabber-muc-process-message))

(defun jabber-muc-process-message (jc xml-data)
  "If XML-DATA is a groupchat message, handle it as such.

JC is the Jabber connection."
  (when (jabber-muc-message-p xml-data)
    (let* ((xml-data (jabber-chat--decrypt-if-needed jc xml-data))
           (from (jabber-xml-get-attribute xml-data 'from))
           (group (jabber-jid-user from))
           (nick (jabber-jid-resource from))
           (type (jabber-muc--classify-message jc group nick xml-data))
           (msg-plist (jabber-chat--msg-plist-from-stanza xml-data))
           (replace-id (jabber-message-correct--replace-id xml-data)))
      (if (and replace-id (not (jabber-muc--history-message-p xml-data)))
          (jabber-message-correct--apply
           replace-id
           (plist-get msg-plist :body)
           from
           t
           (jabber-muc-find-buffer group))
        (jabber-muc--display-message jc xml-data group nick type msg-plist)))))

(defun jabber-muc--format-actor-reason (actor reason)
  "Format optional \" by ACTOR\" / \" - \\='REASON\\='\" suffix."
  (concat (when actor (concat " by " actor))
          (when reason (concat " - '" reason "'"))))

(defun jabber-muc--process-self-leave (jc group type status-codes
                                         error-node actor reason)
  "Handle our own departure from GROUP.
TYPE is the presence type (\"unavailable\" or \"error\").
STATUS-CODES, ERROR-NODE, ACTOR and REASON come from the stanza."
  (let* ((leavingp t)
         (message (cond
                   ((string= type "error")
                    (cond
                     ;; Nick-change errors don't mean we left the room.
                     ((or (member jabber-muc-status-nick-not-allowed status-codes)
                          (member jabber-muc-status-nick-conflict status-codes))
                      (setq leavingp nil)
                      (concat "Nickname change not allowed"
                              (when error-node
                                (concat ": " (jabber-parse-error error-node)))))
                     (t
                      (concat "Error entering room"
                              (when error-node
                                (concat ": " (jabber-parse-error error-node)))))))
                   ((member jabber-muc-status-banned status-codes)
                    (concat "You have been banned"
                            (jabber-muc--format-actor-reason actor reason)))
                   ((member jabber-muc-status-kicked status-codes)
                    (concat "You have been kicked"
                            (jabber-muc--format-actor-reason actor reason)))
                   (t
                    "You have left the chatroom"))))
    (when leavingp
      (jabber-muc-remove-groupchat group jc))
    ;; If there is no buffer for this groupchat, don't bother
    ;; creating one just to tell that user left the room.
    (let ((buffer (get-buffer (jabber-muc-get-buffer group jc))))
      (if buffer
          (with-current-buffer buffer
            (jabber-maybe-print-rare-time
             (jabber-chat-ewoc-enter
              (list (if (string= type "error")
                        :muc-error
                      :muc-notice)
                    message
                    :time (current-time)))))
        (message "%s: %s" (jabber-jid-displayname group) message)))
    ;; Stagger: skip failed room and try the next one.
    ;; Defer via timer so Emacs can redisplay between joins.
    (when (string= type "error")
      (run-with-timer 0 nil #'jabber-muc--autojoin-next jc))))

(defun jabber-muc--process-other-leave (_jc group nickname status-codes
                                           item actor reason)
  "Handle another participant leaving GROUP.
NICKNAME is the departing user.  STATUS-CODES, ITEM, ACTOR and REASON
come from the stanza."
  (let* ((plist (jabber-muc-participant-plist group nickname))
         (jid (plist-get plist 'jid))
         (name (concat nickname
                       (when jid
                         (concat " <"
                                 (jabber-jid-user jid)
                                 ">")))))
    (jabber-muc-remove-participant group nickname)
    (when-let* ((buffer (jabber-muc-find-buffer group)))
      (with-current-buffer buffer
        (jabber-maybe-print-rare-time
         (jabber-chat-ewoc-enter
          (list :muc-notice
                (cond
                 ((member jabber-muc-status-banned status-codes)
                  (concat name " has been banned"
                          (jabber-muc--format-actor-reason actor reason)))
                 ((member jabber-muc-status-kicked status-codes)
                  (concat name " has been kicked"
                          (jabber-muc--format-actor-reason actor reason)))
                 ((member jabber-muc-status-nick-changed status-codes)
                  (concat name " changes nickname to "
                          (jabber-xml-get-attribute item 'nick)))
                 (t
                  (concat name " has left the chatroom")))
                :time (current-time))))))))

(defun jabber-muc--room-created-message ()
  "Return a string with buttons for configuring a newly created room."
  (with-temp-buffer
    (insert "This room was just created, and is locked to other participants.\n"
            "To unlock it, ")
    (insert-text-button
     "configure the room"
     'action (apply-partially 'call-interactively 'jabber-muc-get-config))
    (insert " or ")
    (insert-text-button
     "accept the default configuration"
     'action (apply-partially 'call-interactively 'jabber-muc-instant-config))
    (insert ".")
    (buffer-string)))

(defun jabber-muc--enter-extra-notices (nickname status-codes)
  "Insert extra ewoc notices for STATUS-CODES into the current MUC buffer.
NICKNAME is the entering user.  Assumes `jabber-chat-ewoc' is current."
  (when (member jabber-muc-status-nick-modified status-codes)
    (jabber-chat-ewoc-enter
     (list :muc-notice
           (concat "Your nick was changed to " nickname " by the server")
           :time (current-time))))
  (when (member jabber-muc-status-room-created status-codes)
    (if jabber-muc--auto-configure
        (progn
          (setq jabber-muc--auto-configure nil)
          (jabber-muc-get-config jabber-buffer-connection jabber-group))
      (jabber-chat-ewoc-enter
       (list :muc-notice
             (jabber-muc--room-created-message)
             :time (current-time))))))

(defun jabber-muc--process-enter (jc group nickname symbol status-codes
                                     x-muc actor reason our-nickname)
  "Handle a participant entering or updating presence in GROUP.
NICKNAME is the user.  SYMBOL is their JID symbol.  STATUS-CODES,
X-MUC, ACTOR, REASON and OUR-NICKNAME come from the stanza."
  ;; Self-presence: check nickname too since some servers (e.g.
  ;; ejabberd mod_irc) omit the 110 status code.
  (when (or (member jabber-muc-status-self-presence status-codes)
            (string= nickname our-nickname))
    (let ((was-joined (jabber-muc-joined-p group jc)))
      (jabber-muc-add-groupchat group nickname jc)
      (puthash symbol nickname jabber-pending-groupchats)
      ;; Trigger MUC MAM catch-up on initial join (not nick change)
      (unless was-joined
        (jabber-mam-muc-joined jc group)
        (jabber-bookmarks-auto-add-maybe jc group nickname)
        ;; Stagger: join the next queued room now that this one succeeded.
        ;; Defer via timer so Emacs can redisplay between joins.
        (run-with-timer 0 nil #'jabber-muc--autojoin-next jc))))
  (let* ((self-p (or (member jabber-muc-status-self-presence status-codes)
                     (string= nickname our-nickname)))
         (old-plist (jabber-muc-participant-plist group nickname))
         (new-plist (jabber-muc-parse-affiliation x-muc)))
    (jabber-muc-modify-participant group nickname new-plist)
    ;; Prefetch OMEMO sessions for newly-joining non-self participants.
    (when (and (not self-p) (null old-plist))
      (when-let* ((jid (plist-get new-plist 'jid))
                  (bare (jabber-jid-user jid))
                  (buf (jabber-muc-find-buffer group)))
        (with-current-buffer buf
          (when (and (eq jabber-chat-encryption 'omemo)
                     (fboundp 'jabber-omemo--prefetch-sessions))
            (jabber-omemo--prefetch-sessions jc bare)))))
    (when-let* ((buffer (jabber-muc-find-buffer group)))
      (let ((report (jabber-muc-report-delta nickname old-plist new-plist
                                             reason actor)))
        (when report
          (with-current-buffer buffer
            (jabber-maybe-print-rare-time
             (jabber-chat-ewoc-enter
              (list :muc-notice report
                    :time (current-time)))))))
      ;; Extra notices (status 201/210) fire for self-presence regardless
      ;; of whether there was an affiliation delta report.
      (when self-p
        (with-current-buffer buffer
          (jabber-muc--enter-extra-notices nickname status-codes)
          (when (and (eq jabber-chat-encryption 'omemo)
                     (fboundp 'jabber-omemo--prefetch-muc-sessions))
            (jabber-omemo--prefetch-muc-sessions jc group)))))))

(defun jabber-muc--parse-presence (presence)
  "Extract fields from a MUC PRESENCE stanza.
Return a plist with keys :from, :type, :group, :nickname, :symbol,
:our-nickname, :x-muc, :item, :actor, :reason, :error-node, :status-codes.
Accesses `jabber-pending-groupchats' to determine our nickname."
  (let* ((from (jabber-xml-get-attribute presence 'from))
	 (type (jabber-xml-get-attribute presence 'type))
	 (x-muc (cl-find-if
		 (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
			       jabber-muc-xmlns-user))
		 (jabber-xml-get-children presence 'x)))
	 (group (jabber-jid-user from))
	 (nickname (jabber-jid-resource from))
	 (symbol (jabber-jid-symbol from))
	 (our-nickname (gethash symbol jabber-pending-groupchats))
	 (item (car (jabber-xml-get-children x-muc 'item)))
	 (actor (jabber-xml-get-attribute
		 (car (jabber-xml-get-children item 'actor)) 'jid))
	 (reason (car (jabber-xml-node-children
		       (car (jabber-xml-get-children item 'reason)))))
	 (error-node (car (jabber-xml-get-children presence 'error)))
	 (status-codes (if error-node
			   (list (jabber-xml-get-attribute error-node 'code))
			 (mapcar
			  (lambda (status-element)
			    (jabber-xml-get-attribute status-element 'code))
			  (jabber-xml-get-children x-muc 'status)))))
    (list :from from :type type :group group :nickname nickname
	  :symbol symbol :our-nickname our-nickname :x-muc x-muc
	  :item item :actor actor :reason reason
	  :error-node error-node :status-codes status-codes)))

(defun jabber-muc-process-presence (jc presence)
  "Dispatch a MUC presence stanza to the appropriate handler."
  (let* ((p (jabber-muc--parse-presence presence))
	 (type (plist-get p :type))
	 (group (plist-get p :group))
	 (nickname (plist-get p :nickname))
	 (symbol (plist-get p :symbol))
	 (our-nickname (plist-get p :our-nickname))
	 (x-muc (plist-get p :x-muc))
	 (item (plist-get p :item))
	 (actor (plist-get p :actor))
	 (reason (plist-get p :reason))
	 (error-node (plist-get p :error-node))
	 (status-codes (plist-get p :status-codes)))
    (cond
     ((or (string= type "unavailable") (string= type "error"))
      (if (or (null nickname)
              (member jabber-muc-status-self-presence status-codes)
              (string= nickname our-nickname))
          (jabber-muc--process-self-leave jc group type status-codes
                                         error-node actor reason)
        (jabber-muc--process-other-leave jc group nickname status-codes
                                         item actor reason)))
     (t
      (jabber-muc--process-enter jc group nickname symbol status-codes
                                 x-muc actor reason our-nickname)))))
(jabber-disco-advertise-feature jabber-muc-xmlns-direct-invite)

(provide 'jabber-muc)
;;; jabber-muc.el ends here.
