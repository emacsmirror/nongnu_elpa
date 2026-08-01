;;; jabber-message-thread.el --- XEP-0201 Message Threads  -*- lexical-binding: t; -*-

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

;; XEP-0201 message thread parsing, routing, and thread chat buffers.

;;; Code:

(require 'subr-x)
(require 'ewoc)
(require 'seq)
(require 'jabber-buffer-registry)
(require 'jabber-db)
(require 'jabber-input)
(require 'jabber-message-thread-protocol)
(require 'jabber-util)
(require 'jabber-xml)

(defvar jabber-chat-earliest-backlog)
(defvar jabber-chat-ewoc)
(defvar jabber-chat-encryption)
(defvar jabber-chat-header-line-format-override)
(defvar jabber-chat-send-hooks)
(defvar jabber-chat--send-hook-stanza)
(defvar jabber-chatting-with)
(defvar jabber-group)
(defvar jabber-muc-private-p)
(defvar jabber-send-function)

(defvar jabber-message-thread-buffer-created-functions nil
  "Functions run after initializing a new thread buffer.
Each function receives the thread's parent buffer.")

(defcustom jabber-message-thread-use-buffers t
  "Non-nil means display XEP-0201 threads in dedicated buffers.
When nil, hide thread controls and display threaded messages through the
original parent chat buffer paths.  Thread metadata remains stored."
  :type 'boolean
  :group 'jabber-chat)

(declare-function jabber-chat-mode "jabber-chatbuffer" ())
(declare-function jabber-chat-mode-setup "jabber-chatbuffer" (jc ewoc-pp))
(declare-function jabber-chat-pp "jabber-chat" (data))
(declare-function jabber-chat-send "jabber-chat"
                  (jc body &optional extra-elements success-callback
                      failure-callback))
(declare-function jabber-muc-send "jabber-muc"
                  (jc body &optional extra-elements success-callback
                      failure-callback))
(declare-function jabber-chat--insert-backlog-chunked "jabber-chat"
                  (buffer entries callback &optional generation))
(declare-function jabber-chat-display-buffer-images "jabber-chat" ())
(declare-function jabber-chat-buffer-refresh "jabber-chatbuffer" ())

(defvar-local jabber-message-thread-id nil
  "Opaque XEP-0201 thread identifier for the current buffer.")

(defvar-local jabber-message-thread-parent-id nil
  "Parent thread identifier for the current buffer, or nil.")

(defvar-local jabber-message-thread-type nil
  "Message type for the current thread buffer.")

(defvar-local jabber-message-thread-peer nil
  "Bare contact or room JID for the current thread buffer.")

(defvar-local jabber-message-thread-title nil
  "Local display title for the current thread buffer, or nil.")

(defvar-local jabber-message-thread--root-reply-id nil
  "Root message ID to link from the first locally sent reply.")

(defvar-local jabber-message-thread--root-reply-jid nil
  "Root author JID for the first locally sent reply.")

(defun jabber-message-thread--fields (xml-data)
  "Return XEP-0201 thread fields parsed from XML-DATA.
Return nil when XML-DATA has no valid, unambiguous thread element."
  (jabber-message-thread-protocol-fields xml-data))

(defun jabber-message-thread--elements (thread-id parent-id)
  "Return a thread element for THREAD-ID and optional PARENT-ID.
Return nil when THREAD-ID is empty or equals PARENT-ID."
  (jabber-message-thread-protocol-elements thread-id parent-id))

(defun jabber-message-thread--generate-id ()
  "Return a new opaque message thread identifier."
  (secure-hash
   'sha256
   (format "%S:%S:%S:%S"
           (current-time)
           (random most-positive-fixnum)
           (random most-positive-fixnum)
           (make-temp-name ""))))

(defun jabber-message-thread-available-p ()
  "Return non-nil when dedicated threads are available here."
  (and jabber-message-thread-use-buffers
       (not (bound-and-true-p jabber-muc-private-p))))

(defun jabber-message-thread--ensure-buffers ()
  "Signal a user error when dedicated threads are unavailable."
  (cond
   ((not jabber-message-thread-use-buffers)
    (user-error "Message thread buffers are disabled"))
   ((bound-and-true-p jabber-muc-private-p)
    (user-error "Dedicated threads are not supported in MUC private chats"))))

(defun jabber-message-thread--renew-id ()
  "Replace the current thread ID while preserving its lineage."
  (when-let* ((old-id jabber-message-thread-id)
              (account
               (jabber-connection-bare-jid jabber-buffer-connection))
              (new-id (jabber-message-thread--generate-id)))
    (jabber-db-register-message-thread
     account jabber-message-thread-peer jabber-message-thread-type
     new-id old-id nil nil (floor (float-time)))
    (when jabber-message-thread-title
      (jabber-db-set-message-thread-title
       account jabber-message-thread-peer jabber-message-thread-type
       new-id jabber-message-thread-title))
    (jabber-buffer-registry--remove-current)
    (setq jabber-message-thread-id new-id
          jabber-message-thread-parent-id old-id
          jabber-message-thread--root-reply-id nil
          jabber-message-thread--root-reply-jid nil)
    (jabber-buffer-registry-register
     'thread
     (jabber-message-thread--key
      account jabber-message-thread-peer jabber-message-thread-type new-id))
    new-id))

(defun jabber-message-thread--send-hook (_body _id)
  "Return thread metadata and the pending root reply link."
  (jabber-message-thread--ensure-buffers)
  (let ((elements
         (unless (and (bound-and-true-p jabber-chat--send-hook-stanza)
                      (jabber-message-thread-protocol-has-core-p
                       jabber-chat--send-hook-stanza))
           (jabber-message-thread--elements
            jabber-message-thread-id jabber-message-thread-parent-id))))
    (unless (bound-and-true-p jabber-chat--sending-correction)
      (when jabber-message-thread--root-reply-id
        (setq elements
              (append
               elements
               (list
                `(reply ((xmlns . "urn:xmpp:reply:0")
                         ,@(and jabber-message-thread--root-reply-jid
                                (list
                                 (cons 'to
                                       jabber-message-thread--root-reply-jid)))
                         (id . ,jabber-message-thread--root-reply-id))))))
        (setq jabber-message-thread--root-reply-id nil
              jabber-message-thread--root-reply-jid nil)))
    elements))

(defun jabber-message-thread--key (account peer type thread-id)
  "Return the registry key for ACCOUNT, PEER, TYPE, and THREAD-ID."
  (list account peer type thread-id))

(defun jabber-message-thread-find-buffer (account peer type thread-id)
  "Return THREAD-ID's live buffer for ACCOUNT, PEER, and TYPE, or nil."
  (jabber-buffer-registry-find
   'thread (jabber-message-thread--key account peer type thread-id)))

(defun jabber-message-thread--buffer-name (parent-buffer thread-id)
  "Return a thread buffer name derived from PARENT-BUFFER and THREAD-ID."
  (format "%s [thread %s]*"
          (string-remove-suffix "*" (buffer-name parent-buffer))
          (substring thread-id 0 (min 8 (length thread-id)))))

(defun jabber-message-thread--header ()
  "Return the header text for the current thread buffer."
  (format " %sThread in %s"
          (if jabber-message-thread-title
              (concat
               (string-replace "%" "%%" jabber-message-thread-title)
               " · ")
            "")
          (jabber-jid-displayname jabber-message-thread-peer)))

(defun jabber-message-thread--setup-kind (peer type)
  "Set conversation variables and send function for PEER and TYPE."
  (if (equal type "groupchat")
      (progn
        (setq-local jabber-group peer)
        (setq-local jabber-send-function #'jabber-muc-send))
    (setq-local jabber-chatting-with peer)
    (setq-local jabber-send-function #'jabber-chat-send)))

(defun jabber-message-thread--load-backlog (account peer type thread-id)
  "Load THREAD-ID backlog for ACCOUNT, PEER, and TYPE."
  (let ((entries
         (jabber-db-thread-backlog account peer type thread-id)))
    (if entries
        (progn
          (setq jabber-chat-earliest-backlog
                (float-time
                 (plist-get (car (last entries)) :timestamp)))
          (jabber-chat--insert-backlog-chunked
           (current-buffer) entries
           #'jabber-chat-display-buffer-images))
      (setq jabber-chat-earliest-backlog (float-time)))))

(defun jabber-message-thread-create-buffer
    (jc peer type thread-id parent-id parent-buffer &optional root-msg)
  "Create or return THREAD-ID's buffer on JC for PEER and TYPE.
PARENT-ID records lineage, PARENT-BUFFER names the sibling view, and
ROOT-MSG supplies the initial XEP-0461 link."
  (jabber-message-thread--ensure-buffers)
  (let* ((account (jabber-connection-bare-jid jc))
         (existing
          (jabber-message-thread-find-buffer account peer type thread-id)))
    (or existing
        (with-current-buffer
            (get-buffer-create
             (generate-new-buffer-name
              (jabber-message-thread--buffer-name parent-buffer thread-id)))
          (jabber-chat-mode)
          (setq-local jabber-message-thread-id thread-id)
          (setq-local jabber-message-thread-parent-id parent-id)
          (setq-local jabber-message-thread-type type)
          (setq-local jabber-message-thread-peer peer)
          (setq-local jabber-message-thread-title
                      (plist-get
                       (jabber-db-message-thread-summary
                        account peer type thread-id)
                       :title))
          (jabber-message-thread--setup-kind peer type)
          (jabber-chat-mode-setup jc #'jabber-chat-pp)
          (setq-local jabber-send-function
                      (if (equal type "groupchat")
                          #'jabber-muc-send
                        #'jabber-chat-send))
          (setq-local jabber-chat-header-line-format-override
                      '((:eval (jabber-message-thread--header))
                        (:eval jabber-chat-receipt-message)))
          (setq-local header-line-format
                      jabber-chat-header-line-format-override)
          (add-hook 'jabber-chat-send-hooks
                    #'jabber-message-thread--send-hook nil t)
          (setq-local jabber-message-thread--root-reply-id
                      (and root-msg
                           (if (equal type "groupchat")
                               (plist-get root-msg :server-id)
                             (or (plist-get root-msg :origin-id)
                                 (plist-get root-msg :id))))
                      jabber-message-thread--root-reply-jid
                      (and root-msg (plist-get root-msg :from)))
          (jabber-buffer-registry-register
           'thread
           (jabber-message-thread--key account peer type thread-id))
          (jabber-message-thread--load-backlog
           account peer type thread-id)
          (run-hook-with-args
           'jabber-message-thread-buffer-created-functions parent-buffer)
          (current-buffer)))))

(defun jabber-message-thread-display-target
    (jc peer type msg)
  "Return MSG's target on JC for PEER and TYPE.
The result is `parent', a thread buffer, or nil."
  (let ((thread-id (plist-get msg :thread-id)))
    (when thread-id
      (if (not jabber-message-thread-use-buffers)
          'parent
        (let ((account (jabber-connection-bare-jid jc)))
          (if (or (not (jabber-db-message-thread-known-p
                        account peer type thread-id))
                  (jabber-db-message-thread-root-p
                   account peer type thread-id
                   (plist-get msg :id) (plist-get msg :server-id)
                   (plist-get msg :db-id)))
              'parent
            (jabber-message-thread-find-buffer
             account peer type thread-id)))))))

(defun jabber-message-thread--update-targets
    (account peer type location)
  "Return live buffers for LOCATION in ACCOUNT, PEER, and TYPE.
Return `closed' when the threaded message has no live view."
  (when location
    (if jabber-message-thread-use-buffers
        (let* ((thread-id (plist-get location :thread-id))
               (thread-buffer
                (jabber-message-thread-find-buffer
                 account peer type thread-id)))
          (if (plist-get location :root)
              (or (delete-dups
                   (delq nil
                         (list
                          (jabber-message-thread--parent-buffer
                           account peer type)
                          thread-buffer)))
                  'closed)
            (or (and thread-buffer (list thread-buffer))
                'closed))))))

(defun jabber-message-thread-update-targets
    (jc peer type message-id &optional server-id-p)
  "Return live buffers containing MESSAGE-ID on JC.
PEER and TYPE scope the lookup.  SERVER-ID-P selects server IDs.
Return `closed' when the threaded message has no live view, and nil
when MESSAGE-ID has no thread association."
  (let ((account (jabber-connection-bare-jid jc)))
    (jabber-message-thread--update-targets
     account peer type
     (jabber-db-message-thread-location
      account peer type message-id server-id-p))))

(defun jabber-message-thread-update-targets-for-row
    (jc peer type row-id)
  "Return live buffers containing database ROW-ID on JC.
PEER and TYPE scope the exact stored message lookup."
  (let ((account (jabber-connection-bare-jid jc)))
    (jabber-message-thread--update-targets
     account peer type
     (jabber-db-message-thread-location-by-row
      account peer type row-id))))

(defun jabber-message-thread--root-node-p (msg summary)
  "Return non-nil when MSG is the root described by SUMMARY."
  (let ((db-id (plist-get msg :db-id))
        (root-id (plist-get summary :root-message-id))
        (root-stanza-id (plist-get summary :root-stanza-id))
        (root-server-id (plist-get summary :root-server-id))
        (groupchat-p
         (equal (plist-get summary :thread-type) "groupchat")))
    (cond
     (db-id
      (and root-id (equal db-id root-id)))
     (groupchat-p
      (and root-server-id
           (equal (plist-get msg :server-id) root-server-id)))
     (root-stanza-id
      (equal (plist-get msg :id) root-stanza-id)))))

(defun jabber-message-thread--node-for-root (ewoc summary)
  "Return the EWOC node matching the root IDs in SUMMARY."
  (let ((node (ewoc-nth ewoc 0))
        found)
    (while (and node (not found))
      (let* ((data (ewoc-data node))
             (msg (and (listp (cadr data)) (cadr data))))
        (when (and msg (jabber-message-thread--root-node-p msg summary))
          (setq found node)))
      (setq node (and (not found) (ewoc-next ewoc node))))
    found))

(defun jabber-message-thread--new-row-only-root-p (summary)
  "Return non-nil when SUMMARY's new root needs row reconciliation."
  (and (plist-get summary :root-message-id)
       (zerop (or (plist-get summary :reply-count) 0))
       (if (equal (plist-get summary :thread-type) "groupchat")
           (null (plist-get summary :root-server-id))
         (null (plist-get summary :root-stanza-id)))))

(defun jabber-message-thread--parent-buffer (account peer type)
  "Return the live parent buffer matching ACCOUNT, PEER, and TYPE."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (eq major-mode 'jabber-chat-mode)
            (not (bound-and-true-p jabber-message-thread-id))
            (bound-and-true-p jabber-buffer-connection)
            (equal account
                   (jabber-connection-bare-jid jabber-buffer-connection))
            (if (equal type "groupchat")
                (equal peer (bound-and-true-p jabber-group))
              (and (not (bound-and-true-p jabber-group))
                   (equal peer
                          (and (bound-and-true-p jabber-chatting-with)
                               (jabber-jid-user jabber-chatting-with))))))))
   (buffer-list)))

(defun jabber-message-thread--refresh-root
    (account peer type thread-id)
  "Refresh THREAD-ID's root marker for ACCOUNT, PEER, and TYPE."
  (when-let* ((buffer
               (jabber-message-thread--parent-buffer account peer type))
              (summary
               (jabber-db-message-thread-summary
                account peer type thread-id)))
    (with-current-buffer buffer
      (if-let* ((node
                 (jabber-message-thread--node-for-root
                  jabber-chat-ewoc summary)))
          (progn
            (plist-put (cadr (ewoc-data node)) :thread-summary summary)
            (ewoc-invalidate jabber-chat-ewoc node))
        (when (jabber-message-thread--new-row-only-root-p summary)
          (jabber-chat-buffer-refresh))))))

(defun jabber-message-thread--stored
    (account peer type thread-id _timestamp)
  "Update THREAD-ID views after storage for ACCOUNT, PEER, and TYPE."
  (when (and jabber-message-thread-use-buffers thread-id)
    (when-let* ((buffer
                 (jabber-message-thread-find-buffer
                  account peer type thread-id))
                ((get-buffer-window buffer t)))
      (jabber-db-mark-message-thread-read
       account peer type thread-id))
    (jabber-message-thread--refresh-thread-root
     account peer type thread-id)
    (jabber-message-thread--refresh-root
     account peer type thread-id)))

(add-hook 'jabber-db-message-thread-stored-functions
          #'jabber-message-thread--stored)

(defun jabber-message-thread--message-at-point ()
  "Return the message plist at point, or nil."
  (when-let* (((number-or-marker-p jabber-point-insert))
              ((< (point) jabber-point-insert))
              (node (and (bound-and-true-p jabber-chat-ewoc)
                         (ewoc-locate jabber-chat-ewoc (point))))
              (data (ewoc-data node))
              ((listp (cadr data))))
    (cadr data)))

(defun jabber-message-thread--parent-context ()
  "Return the current parent conversation as (PEER TYPE)."
  (cond
   ((bound-and-true-p jabber-group)
    (list (jabber-jid-user jabber-group) "groupchat"))
   ((bound-and-true-p jabber-chatting-with)
    (list (jabber-jid-user jabber-chatting-with) "chat"))
   (t
    (user-error "Not in a chat buffer"))))

(defun jabber-message-thread--browse-context ()
  "Return (ACCOUNT PEER TYPE PARENT-BUFFER) for the current chat."
  (pcase-let* ((`(,peer ,type) (jabber-message-thread--parent-context))
               (account
                (jabber-connection-bare-jid jabber-buffer-connection))
               (parent
                (if jabber-message-thread-id
                    (or (jabber-message-thread--parent-buffer
                         account peer type)
                        (user-error "Parent chat buffer is not open"))
                  (current-buffer))))
    (list account peer type parent)))

(defun jabber-message-thread--completion-label (thread)
  "Return THREAD's compact completion label."
  (let* ((title (plist-get thread :title))
         (root (plist-get thread :root-message))
         (from (plist-get root :from))
         (body (string-trim
                (replace-regexp-in-string
                 "[[:space:]]+" " " (or (plist-get root :body) ""))))
         (preview
          (if (plist-get root :retracted)
              "[Message retracted]"
            (truncate-string-to-width
             (if (string-empty-p body) "(no text)" body)
             72 nil nil "…")))
         (nick (and (equal (plist-get thread :thread-type) "groupchat")
                    (stringp from)
                    (jabber-jid-resource from))))
    (cond
     ((and (stringp title) (not (string-empty-p title)))
      (truncate-string-to-width title 72 nil nil "…"))
     (nick (format "%s: %s" nick preview))
     (t preview))))

(defun jabber-message-thread--unique-label (label used)
  "Return a unique completion LABEL and record it in USED."
  (let ((candidate label)
        (number 1))
    (while (gethash candidate used)
      (setq number (1+ number)
            candidate (format "%s (%d)" label number)))
    (puthash candidate t used)
    candidate))

(defun jabber-message-thread--completion-items (threads)
  "Return unique completion items for ordered THREADS."
  (let ((used (make-hash-table :test #'equal)))
    (mapcar
     (lambda (thread)
       (cons
        (jabber-message-thread--unique-label
         (jabber-message-thread--completion-label thread) used)
        thread))
     threads)))

(defun jabber-message-thread--completion-annotation (thread)
  "Return a completion annotation for THREAD."
  (let* ((count (or (plist-get thread :reply-count) 0))
         (latest (plist-get thread :latest-at))
         (parts
          (list (format "%d %s" count (if (= count 1) "reply" "replies"))
                (and latest
                     (format "active %s"
                             (format-time-string "%Y-%m-%d %H:%M" latest)))
                (and (plist-get thread :unread) "unread"))))
    (concat "  " (string-join (delq nil parts) " · "))))

(defun jabber-message-thread--read-thread (threads)
  "Read and return one entry from ordered THREADS."
  (unless threads
    (user-error "No threads in this chat"))
  (let* ((items (jabber-message-thread--completion-items threads))
         (table (completion-table-with-metadata
                 items '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
         (completion-extra-properties
          (list :annotation-function
                (lambda (candidate)
                  (jabber-message-thread--completion-annotation
                   (cdr (assoc-string candidate items))))))
         (choice (completing-read "Open thread: " table nil t)))
    (cdr (assoc-string choice items))))

(defun jabber-message-thread--open-stored
    (connection account peer type parent summary)
  "Open SUMMARY for CONNECTION, ACCOUNT, PEER, TYPE, and PARENT."
  (let ((buffer
         (jabber-message-thread-create-buffer
          connection peer type (plist-get summary :thread-id)
          (plist-get summary :thread-parent-id) parent
          (and (zerop (plist-get summary :local-reply-count))
               (plist-get summary :root-message)))))
    (jabber-db-mark-message-thread-read
     account peer type (plist-get summary :thread-id))
    (jabber-message-thread--refresh-root
     account peer type (plist-get summary :thread-id))
    (pop-to-buffer buffer)))

;;;###autoload
(defun jabber-message-thread-browse ()
  "Choose and open a thread from the current chat."
  (interactive)
  (jabber-message-thread--ensure-buffers)
  (unless (jabber-db-ensure-open)
    (user-error "Message threads require persistent message storage"))
  (pcase-let* ((`(,account ,peer ,type ,parent)
                (jabber-message-thread--browse-context))
               (summary
                (jabber-message-thread--read-thread
                 (jabber-db-message-threads account peer type))))
    (jabber-message-thread--open-stored
     jabber-buffer-connection account peer type parent summary)))

;;;###autoload
(defun jabber-message-thread-set-title (title)
  "Set the current thread's local TITLE, or clear it when empty."
  (interactive
   (list (read-string "Thread title (empty clears): "
                      jabber-message-thread-title)))
  (unless jabber-message-thread-id
    (user-error "Not in a thread buffer"))
  (unless (jabber-db-ensure-open)
    (user-error "Message threads require persistent message storage"))
  (let ((account
         (jabber-connection-bare-jid jabber-buffer-connection)))
    (setq jabber-message-thread-title
          (jabber-db-set-message-thread-title
           account jabber-message-thread-peer jabber-message-thread-type
           jabber-message-thread-id title))
    (force-mode-line-update t)))

(defun jabber-message-thread--refresh-thread-root
    (account peer type thread-id)
  "Reload THREAD-ID's buffer when its stored root is missing.
ACCOUNT, PEER, and TYPE scope the thread."
  (when-let* ((buffer
               (jabber-message-thread-find-buffer
                account peer type thread-id))
              (summary
               (jabber-db-message-thread-summary
                account peer type thread-id)))
    (with-current-buffer buffer
      (unless (jabber-message-thread--node-for-root
               jabber-chat-ewoc summary)
        (jabber-chat-buffer-refresh)))))

;;;###autoload
(defun jabber-message-thread-start ()
  "Send the current draft as a new thread and open its buffer."
  (interactive)
  (jabber-message-thread--ensure-buffers)
  (when jabber-message-thread-id
    (user-error "Already in a thread buffer"))
  (unless (and (number-or-marker-p jabber-point-insert)
               (< jabber-point-insert (point-max)))
    (user-error "No draft to send"))
  (unless (jabber-db-ensure-open)
    (user-error "Message threads require persistent message storage"))
  (pcase-let* ((`(,peer ,type)
                (jabber-message-thread--parent-context))
               (thread-id (jabber-message-thread--generate-id))
               (account
                (jabber-connection-bare-jid jabber-buffer-connection))
               (created-at (floor (float-time)))
               (parent-buffer (current-buffer)))
    (jabber-chat-buffer-send
     (jabber-message-thread--elements thread-id nil))
    (jabber-db-register-message-thread
     account peer type thread-id nil nil nil created-at)
    (pop-to-buffer
     (jabber-message-thread-create-buffer
      jabber-buffer-connection peer type thread-id nil parent-buffer nil))))

;;;###autoload
(defun jabber-message-thread-open (&optional msg)
  "Open the thread rooted at MSG or the message at point."
  (interactive)
  (jabber-message-thread--ensure-buffers)
  (when jabber-message-thread-id
    (user-error "Already in a thread buffer"))
  (unless (jabber-db-ensure-open)
    (user-error "Message threads require persistent message storage"))
  (let* ((msg (or msg (jabber-message-thread--message-at-point)))
         (type (if (bound-and-true-p jabber-group) "groupchat" "chat"))
         (peer (jabber-jid-user
                (or (bound-and-true-p jabber-group)
                    (bound-and-true-p jabber-chatting-with))))
         (account (jabber-connection-bare-jid jabber-buffer-connection))
         (received-thread-id (and msg (plist-get msg :thread-id)))
         (summary
          (or (and msg (plist-get msg :thread-summary))
              (and received-thread-id
                   (jabber-db-message-thread-summary
                    account peer type received-thread-id))))
         (root-stanza-id (and msg (plist-get msg :id)))
         (root-server-id (and msg (plist-get msg :server-id)))
         (thread-id (or (plist-get summary :thread-id)
                        received-thread-id
                        (jabber-message-thread--generate-id))))
    (unless msg
      (user-error "No message at point"))
    (unless (or summary
                (plist-get msg :db-id)
                (if (equal type "groupchat")
                    root-server-id
                  root-stanza-id))
      (user-error "Message is not stored and has no stable ID"))
    (unless summary
      (jabber-db-register-message-thread
       account peer type thread-id (plist-get msg :thread-parent-id)
       root-stanza-id root-server-id
       (floor (float-time (plist-get msg :timestamp)))
       (plist-get msg :db-id))
      (setq summary
            (jabber-db-message-thread-summary
             account peer type thread-id))
      (jabber-message-thread--refresh-root
       account peer type thread-id))
    (let ((buffer
           (jabber-message-thread-create-buffer
            jabber-buffer-connection peer type thread-id
            (plist-get summary :thread-parent-id)
            (current-buffer) (and (zerop (plist-get summary
                                                    :local-reply-count))
                                  msg))))
      (jabber-db-mark-message-thread-read
       account peer type thread-id)
      (jabber-message-thread--refresh-root account peer type thread-id)
      (pop-to-buffer buffer))))

(provide 'jabber-message-thread)
;;; jabber-message-thread.el ends here
