;;; vm-delete.el --- Delete and expunge commands for VM  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1997 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2026 The VM Developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Code:

(require 'vm-macro)

(require 'vm-misc)
(require 'vm-window)
(require 'vm-undo)
(require 'vm-sort)
(eval-when-compile (require 'cl-lib))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

;;;###autoload
(defun vm-delete-message (count &optional mlist)
  "Add the `deleted' attribute to the current message.

The message will be physically deleted from the current folder the next
time the current folder is expunged.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are deleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are deleted, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are deleted."
  (interactive "p")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(del-count 0))
    (unless mlist
      (setq mlist (vm-select-operable-messages 
		   count (vm-interactive-p) "Delete")))
    (while mlist
      (unless (vm-deleted-flag (car mlist))
	(when (vm-set-deleted-flag (car mlist) t)
	  (vm-increment del-count)))
      ;; The following is a temporary fix.  To be absorted into
      ;; vm-update-summary-and-mode-line eventually.
      (when (and vm-summary-enable-thread-folding
		 vm-summary-show-threads
		 (> (vm-thread-count (car mlist)) 1))
	(with-current-buffer vm-summary-buffer
	  (vm-expand-thread (vm-thread-root (car mlist)))))
      (setq mlist (cdr mlist)))
    (vm-display nil nil '(vm-delete-message vm-delete-message-backward)
		(list this-command))
    (when (vm-interactive-p)
      (if (zerop del-count)
	  (vm-inform 5 "No messages deleted")
	(vm-inform 5 "%d message%s deleted"
		   del-count
		   (if (= 1 del-count) "" "s"))))
    (vm-update-summary-and-mode-line)
    (when (and vm-move-after-deleting (not used-marks))
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-deleting t))))
	(vm-next-message count t executing-kbd-macro)))))

;;;###autoload
(defun vm-delete-message-backward (count)
  "Like vm-delete-message, except the deletion direction is reversed."
  (interactive "p")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-delete-message (- count)))

;;;###autoload
(defun vm-undelete-message (count)
  "Remove the `deleted' attribute from the current message.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are undeleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are undeleted, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are undeleted."
  (interactive "p")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-operable-messages 
		count (vm-interactive-p) "Undelete"))
	(undel-count 0))
    (while mlist
      (when (vm-deleted-flag (car mlist))
	(when (vm-set-deleted-flag (car mlist) nil)
	  (vm-increment undel-count)))
      (setq mlist (cdr mlist)))
    (when (and used-marks (vm-interactive-p))
      (if (zerop undel-count)
	  (vm-inform 5 "No messages undeleted")
	(vm-inform 5 "%d message%s undeleted"
		   undel-count
		   (if (= 1 undel-count)
		       "" "s"))))
    (vm-display nil nil '(vm-undelete-message) '(vm-undelete-message))
    (vm-update-summary-and-mode-line)
    (when (and vm-move-after-undeleting (not used-marks))
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-undeleting t))))
	(vm-next-message count t executing-kbd-macro)))))

;;;###autoload
(defun vm-toggle-flag-message (count &optional mlist)
  "Toggle the `flagged' attribute to the current message, i.e., if it 
has not been flagged then it will be flagged and, if it is already
flagged, then it will be unflagged.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are flagged/unflagged.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
flagged/unflagged.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are flagged/unflagged, other messages are
ignored.  If applied to collapsed threads in summary and thread
operations are enabled via `vm-enable-thread-operations' then all
messages in the thread are flagged/unflagged."
  (interactive "p")
  (if (vm-interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(flagged-count 0)
	(new-flagged nil))
    (unless mlist
      (setq mlist (vm-select-operable-messages 
		   count (vm-interactive-p) "Flag/unflag")))
    (when mlist
      (setq new-flagged (not (vm-flagged-flag (car mlist)))))
    (while mlist
      (when (vm-set-flagged-flag (car mlist) new-flagged)
	(vm-increment flagged-count)
	;; The following is a temporary fix.  To be absorted into
	;; vm-update-summary-and-mode-line eventually.
	(when (and vm-summary-enable-thread-folding
		 vm-summary-show-threads
		 (> (vm-thread-count (car mlist)) 1))
	(with-current-buffer vm-summary-buffer
	  (vm-expand-thread (vm-thread-root (car mlist))))))
      (setq mlist (cdr mlist)))
    (vm-display nil nil '(vm-toggle-flag-message)
		(list this-command))
    (if (and used-marks (vm-interactive-p))
	(if (zerop flagged-count)
	    (vm-inform 5 "No messages flagged/unflagged")
	  (vm-inform 5 "%d message%s %sflagged"
		      flagged-count
		      (if (= 1 flagged-count) "" "s")
		      (if new-flagged "" "un"))))
    (vm-update-summary-and-mode-line)))


;;;###autoload
(defun vm-kill-subject (&optional arg)
"Delete all messages with the same subject as the current message.
Message subjects are compared after ignoring parts matched by
the variables `vm-subject-ignored-prefix' and `vm-subject-ignored-suffix'.

The optional prefix argument ARG specifies the direction to move
if `vm-move-after-killing' is non-nil.  The default direction is
forward.  A positive prefix argument means move forward, a
negative argument means move backward, a zero argument means
don't move at all."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((subject (vm-so-sortable-subject (car vm-message-pointer)))
	(mp vm-message-list)
	(n 0)
	(case-fold-search t))
    (while mp
      (if (and (not (vm-deleted-flag (car mp)))
	       (string-equal subject (vm-so-sortable-subject (car mp))))
	  (if (vm-set-deleted-flag (car mp) t)
	      (vm-increment n)))
      (setq mp (cdr mp)))
    (and (vm-interactive-p)
	 (if (zerop n)
	     (vm-inform 5 "No messages deleted.")
	   (vm-inform 5 "%d message%s deleted" n (if (= n 1) "" "s")))))
  (vm-display nil nil '(vm-kill-subject) '(vm-kill-subject))
  (vm-update-summary-and-mode-line)
  (cond ((or (not (numberp arg)) (> arg 0))
	 (setq arg 1))
	((< arg 0)
	 (setq arg -1))
	(t (setq arg 0)))
  (if vm-move-after-killing
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-killing t))))
	(vm-next-message arg t executing-kbd-macro))))

;;;###autoload
(defun vm-kill-thread-subtree (&optional arg)
  "Delete all messages in the thread tree rooted at the current message.

The optional prefix argument ARG specifies the direction to move
if vm-move-after-killing is non-nil.  The default direction is
forward.  A positive prefix argument means move forward, a
negative argument means move backward, a zero argument means
don't move at all."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (vm-build-threads-if-unbuilt)
  (let ((list (vm-thread-subtree
	       (vm-thread-symbol (car vm-message-pointer))))
	(n 0))
    (while list
      (unless (vm-deleted-flag (car list))
	(if (vm-set-deleted-flag (car list) t)
	    (vm-increment n)))
      (setq list (cdr list)))
    (when (vm-interactive-p)
      (if (zerop n)
	  (vm-inform 5 "No messages deleted.")
	(vm-inform 5 "%d message%s deleted" n (if (= n 1) "" "s")))))
  (vm-display nil nil '(vm-kill-thread-subtree) '(vm-kill-thread-subtree))
  (vm-update-summary-and-mode-line)
  (cond ((or (not (numberp arg)) (> arg 0))
	 (setq arg 1))
	((< arg 0)
	 (setq arg -1))
	(t (setq arg 0)))
  (if vm-move-after-killing
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-killing t))))
	(vm-next-message arg t executing-kbd-macro))))

;;;###autoload
(defun vm-delete-duplicate-messages ()
  "Delete duplicate messages in the current folder.
This command works by comparing the message ID's.  Messages that
are already deleted are not considered, so VM will never delete the last
copy of a message in a folder.  `Deleting' means flagging for
deletion; you will have to expunge the messages with
`vm-expunge-folder' to really get rid of them, as usual.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only duplicate messages among the marked messages are deleted;
unmarked messages are not considered for deletion."
  (interactive)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(table (make-vector 103 0))
	(mp vm-message-list)
        (n 0)
        (case-fold-search t)
        mid)
    (if used-marks
	(let ((vm-enable-thread-operations nil))
	  (setq mp (vm-select-operable-messages 0))))
    ;; Flag duplicate copies of messages for deletion
    (while mp
      (cond ((vm-deleted-flag (car mp))
	     ;; ignore messages already flagged for deletion
	     )
	    ((and (eq vm-folder-access-method 'imap)
		  (or (member "stale" (vm-labels-of (car mp)))
		      ;; ...and those that are stale but not yet labelled.
		      ;; The label is only applied once the user declines to
		      ;; expunge them, which happens *after* this runs from
		      ;; `vm-arrived-messages-hook'.  A message left with a
		      ;; mismatched UID validity by an interrupted retrieval
		      ;; would otherwise claim the message id and get the
		      ;; freshly fetched good copy flagged for deletion
		      ;; instead of itself (issue #286).
		      (not (equal (vm-imap-uid-validity-of (car mp))
				  (vm-folder-imap-uid-validity)))))
	     ;; ignore stale messages
	     )
            (t
             (setq mid (vm-su-message-id (car mp)))
	     (when mid
	       (when (intern-soft mid table)
		 (if (vm-set-deleted-flag (car mp) t)
		     (setq n (1+ n))))
	       (intern mid table))))
      (setq mp (cdr mp)))
    (when (vm-interactive-p)
      (if (zerop n)
	  (vm-inform 5 "No messages deleted")
	(vm-inform 5 "%d message%s deleted" n (if (= 1 n) "" "s"))))
    (vm-update-summary-and-mode-line)
    n))

;;;###autoload
(defun vm-delete-duplicate-messages-by-body ()
"Delete duplicate messages in the current folder.
This command works by computing an MD5 hash for the body of each
non-deleted message in the folder and deleting messages that have
a hash that has already been seen.  Messages that are already deleted
are never hashed, so VM will never delete the last copy of a
message in a folder.  `Deleting' means flagging for deletion; you
will have to expunge the messages with `vm-expunge-folder' to
really get rid of them, as usual.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only duplicate messages among the marked messages are deleted,
unmarked messages are not hashed or considered for deletion."
  (interactive)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist vm-message-list)
	(table (make-vector 61 0))
	hash m
	(del-count 0))
    (when used-marks
      (let ((vm-enable-thread-operations nil))
	(setq mlist (vm-select-operable-messages 0))))
    (save-excursion
      (save-restriction
	(widen)
	(while mlist
	  (if (vm-deleted-flag (car mlist))
	      nil
	    (setq m (vm-real-message-of (car mlist)))
	    (set-buffer (vm-buffer-of m))
	    (setq hash (vm-md5-region (vm-text-of m) (vm-text-end-of m)))
	    (if (intern-soft hash table)
		(if (vm-set-deleted-flag (car mlist) t)
		    (vm-increment del-count))
	      (intern hash table)))
	  (setq mlist (cdr mlist)))))
    (vm-display nil nil '(vm-delete-duplicate-messages)
		(list this-command))
    (when (vm-interactive-p)
      (if (zerop del-count)
	  (vm-inform 5 "No messages deleted")
	(vm-inform 5 "%d message%s deleted" 
		 del-count (if (= 1 del-count) "" "s"))))
    (vm-update-summary-and-mode-line)
    del-count))

(defun vm-expunge-queue-pop-deletion (message)
  "Queue MESSAGE\\='s deletion for its maildrop, if the maildrop can act on it.
The current buffer is MESSAGE\\='s folder, and its access method is `pop\\='.

Only a message with a UIDL.  Without one the maildrop was never asked for it
by name and there is nothing there to delete, and the queue collected nil
for it (emacs-vm/vm#758)."
  (when (vm-pop-uidl-of message)
    (setq vm-pop-messages-to-expunge
	  (cons (vm-pop-uidl-of message) vm-pop-messages-to-expunge))))

(defun vm-expunge-record-pop-uidl (message)
  "Remember MESSAGE\\='s UIDL as one this folder has had, once.
The current buffer is MESSAGE\\='s folder, and its access method is `pop\\='.
So that a later check or fetch does not bring it again.

The maildrop is named without its password, which is how the entry made when
the message arrived names it (`vm-pop-net-note-retrieved\\=') and what every
check compares against (`vm-pop-net-unretrieved\\='): recorded with the
password in it, as this did, the entry matched nothing and was a duplicate of
one that does (emacs-vm/vm#758)."
  (let ((uidl (vm-pop-uidl-of message))
	(maildrop (vm-popdrop-sans-password (vm-folder-pop-maildrop-spec))))
    (when (and uidl
	       (null (vm-find vm-pop-retrieved-messages
			      (lambda (entry)
				(and (equal (car entry) uidl)
				     (equal (nth 1 entry) maildrop))))))
      (setq vm-pop-retrieved-messages
	    (cons (list uidl maildrop 'uidl) vm-pop-retrieved-messages)))))

(defun vm-expunge-queue-imap-deletion (message)
  "Queue MESSAGE\\='s deletion for its mailbox, if the mailbox can act on it.
The current buffer is MESSAGE\\='s folder, and its access method is `imap\\='.

Only a UID under the mailbox\\='s own UIDVALIDITY is queued.  A message with no
UID never came from the server; one whose UIDVALIDITY is not the mailbox\\='s
names nothing there now, and `vm-imap-expunge-remote-messages\\=' would refuse
it and say so, which tells the reader about something they can do nothing
about.  A validity that goes stale after the queue is written is a different
matter and is still refused at that point: the mailbox can be recreated while
a deletion waits for a session (emacs-vm/vm#757)."
  (when (and (vm-imap-uid-of message)
	     (equal (vm-imap-uid-validity-of message)
		    (vm-folder-imap-uid-validity)))
    (setq vm-imap-messages-to-expunge
	  (cons (cons (vm-imap-uid-of message)
		      (vm-imap-uid-validity-of message))
		vm-imap-messages-to-expunge))))

(defun vm-expunge-record-imap-uid (message)
  "Remember MESSAGE\\='s UID as one this folder has had, once.
The current buffer is MESSAGE\\='s folder, and its access method is `imap\\='.
So that a later synchronise does not fetch it again.

The UID went on the list when the message arrived, so this is usually a
message the list already names: it is the key that says so, the UID and its
UIDVALIDITY, and not the whole entry -- the entry made on arrival holds the
maildrop without its password and this one holds it as the folder has it.
Comparing entries grew one duplicate per expunge, in a list written into the
folder\\='s `X-VM-IMAP-Retrieved\\=' header on every save (emacs-vm/vm#757)."
  (let ((uid (vm-imap-uid-of message))
	(validity (vm-imap-uid-validity-of message)))
    (when (and uid validity
	       (null (vm-find vm-imap-retrieved-messages
			      (lambda (entry)
				(and (equal (car entry) uid)
				     (equal (cadr entry) validity))))))
      (setq vm-imap-retrieved-messages
	    (cons (list uid validity
			(vm-imapdrop-sans-password
			 (vm-folder-imap-maildrop-spec))
			'uid)
		  vm-imap-retrieved-messages)))))

;;;###autoload
(cl-defun vm-expunge-folder (&key (quiet nil)
				((:just-these-messages message-list)
				 nil	; default value
				 just-these-messages)
				(not-on-the-server nil))
  "Expunge messages with the `deleted' attribute.
For normal folders this means that the deleted messages are
removed from the message list and the message contents are
removed from the folder buffer.

For virtual folders, messages are removed from the virtual
message list.  If virtual mirroring is in effect for the virtual
folder, the corresponding real messages are also removed from real
message lists and the message contents are removed from real folders.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only messages both marked and deleted are expunged, other messages are
ignored.

NOT-ON-THE-SERVER says these messages are being expunged because the server no
longer has them, so their deletion is not queued for it.  A synchronise passes
it for the messages it found gone; without it the queue collected UIDs that no
longer exist there, which is a no-op at best and a NO from some servers
(emacs-vm/vm#757)."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  ;; do this so we have a clean slate.  code below depends on the
  ;; fact that the numbering redo start point begins as nil in
  ;; all folder buffers.
  (vm-update-summary-and-mode-line)
  (unless quiet
    (vm-inform 5 "%s: Expunging..." (buffer-name)))
  (let ((use-marks (and (eq last-command 'vm-next-command-uses-marks)
			(null just-these-messages)))
	(mp vm-message-list)
	(virtual (eq major-mode 'vm-virtual-mode))
	(buffers-altered (make-vector 29 0))
	virtual-messages)
    (while mp
      (when (if just-these-messages
		(memq (car mp) message-list)
	      (and (vm-deleted-flag (car mp))
		   (or (not use-marks)
		       (vm-mark-of (car mp)))))
	;; 1. remove the message from the thread tree.
	(if (vectorp vm-thread-obarray)
	    (vm-unthread-message-and-mirrors 
	     (vm-real-message-of (car mp)) :message-changing nil))
	;; 2. remove the virtual mirrors from message lists.
	(when (setq virtual-messages (vm-virtual-messages-of (car mp)))
	  (let ((vms (if virtual
			 (cons (vm-real-message-of (car mp))
			       (vm-virtual-messages-of (car mp)))
		       (vm-virtual-messages-of (car mp)))))
	    (while vms
	      ;; Don't trust blindly.  The user could have killed some of
	      ;; these buffers, and killing a folder buffer does not
	      ;; deregister its messages, so a mirror can outlive the list it
	      ;; was in.  Expunging it would signal, leaving this expunge half
	      ;; done and the next one signalling in the same place (#571).
	      ;; The trimming below takes the dead mirror off the list.
	      (when (buffer-name (vm-buffer-of (car vms)))
		(with-current-buffer (vm-buffer-of (car vms))
		  (vm-expunge-message (car vms))
		  (intern (buffer-name) buffers-altered)))
	      (vm-set-virtual-messages-of (car mp) (cdr vms))
	      (setq vms (cdr vms)))))
	;; 3. remove this message from message lists.
	(when (or (null virtual-messages) (not virtual))
	  (when (and (null virtual-messages) virtual)
	    (vm-set-virtual-messages-of
	     (vm-real-message-of (car mp))
	     (delq (car mp) (vm-virtual-messages-of
			     (vm-real-message-of (car mp))))))
	  (vm-expunge-message (car mp))
	  (intern (buffer-name) buffers-altered))
	;; 4. expunge the real message from its folder
	(if (eq (vm-attributes-of (car mp))
		(vm-attributes-of (vm-real-message-of (car mp))))
	    (let ((real-m (vm-real-message-of (car mp))))
	      (with-current-buffer (vm-buffer-of real-m)
		(cond ((eq vm-folder-access-method 'pop)
		       (unless not-on-the-server
			 (vm-expunge-queue-pop-deletion real-m))
		       (vm-expunge-record-pop-uidl real-m))
		      ((eq vm-folder-access-method 'imap)
		       (unless not-on-the-server
			 (vm-expunge-queue-imap-deletion real-m))
		       (vm-expunge-record-imap-uid real-m)))
		(vm-increment vm-modification-counter)
		(save-restriction
		 (widen)
		 (let ((buffer-read-only nil))
		   (delete-region (vm-start-of real-m)
				  (vm-end-of real-m))))))))
      (setq mp (cdr mp)))
    (vm-display nil nil '(vm-expunge-folder) '(vm-expunge-folder))

    ;; 5. Update display

    ;; BUFFERS-ALTERED is an obarray, so it is a vector and never nil: this
    ;; used to test it with `null' and so always reported the messages as
    ;; expunged, and re-sorted a folder nothing had been expunged from (#572).
    (if (vm-obarray-empty-p buffers-altered)
	(unless quiet
	  (vm-inform 5 "%s: No messages are flagged for deletion."
		     (buffer-name)))
      (mapatoms
       (lambda (buffer)
	 (with-current-buffer (symbol-name buffer)
	   ;; FIXME The update summary here is a heavy duty
	   ;; operation.  Can we be more clever about it, for
	   ;; instance avoid doing it before quitting a folder?
	   (if (null vm-system-state)
	       (progn
		 (vm-garbage-collect-message)
		 (if (null vm-message-pointer)
		     ;; folder is now empty
		     (progn (setq vm-folder-type nil)
			    (vm-update-summary-and-mode-line))
		   (vm-present-current-message)))
	     (vm-update-summary-and-mode-line))
	   (unless (eq major-mode 'vm-virtual-mode)
	     (setq vm-message-order-changed
		   (or vm-message-order-changed 
		       vm-message-order-header-present)))
	   (vm-clear-expunge-invalidated-undos)))
       buffers-altered)
      (if vm-ml-sort-keys
          (vm-sort-messages vm-ml-sort-keys))
      (unless quiet
	(vm-inform 5 "%s: Deleted messages expunged." (buffer-name))))
    )
  (when vm-debug
    (vm-check-thread-integrity)))
;;;###autoload (autoload 'vm-compact-folder "vm-delete" nil t)
(defalias 'vm-compact-folder 'vm-expunge-folder)

(defun vm-expunge-message (m)
  "Expunge the message M from the current folder buffer."
  (let (prev curr)
    (setq prev (vm-reverse-link-of m)
	  curr (or (cdr prev) vm-message-list))
    ;; CURR is spliced out below on the strength of the reverse link alone, so a
    ;; stale link removes the message after the one it used to precede and a
    ;; missing link removes the head of the folder, leaving M in place and
    ;; flagging the wrong message expunged.  In a folder with virtual mirrors
    ;; the wrong flag then propagates through the shared attributes and the
    ;; expunge loop cascades.  Nothing recovers from that, so check before
    ;; anything is touched (#570).  Not `vm-assert': assertion checking is off
    ;; by default, which is where this has to hold.
    (unless (eq m (car curr))
      (error (concat "Message %s of %s is not where its reverse link says,"
		     " so expunging it would remove another message."
		     "  This is a VM bug: kill this folder without saving it,"
		     " visit it again, and report this at"
		     " https://gitlab.com/emacs-vm/vm/-/issues")
	     (vm-number-of m) (buffer-name)))
    (vm-unregister-fetched-message m)
    (vm-set-numbering-redo-start-point (or prev t))
    (vm-set-summary-redo-start-point (or prev t))
    (when (eq vm-message-pointer curr)
      (setq vm-system-state nil)
      (setq vm-message-pointer (or prev (cdr curr))))
    (when (eq vm-last-message-pointer curr)
      (setq vm-last-message-pointer nil))
    ;; lock out interrupts to preserve message-list integrity
    (let ((inhibit-quit t))
      ;; vm-clear-expunge-invalidated-undos uses
      ;; this to recognize expunged messages.
      ;; If this stuff is mirrored we'll be
      ;; setting this value multiple times if there
      ;; are multiple virtual messages referencing
      ;; the underlying real message.  Harmless.
      (vm-set-deleted-flag-of (car curr) 'expunged)
      ;; disable any summary update that may have
      ;; already been scheduled.
      (vm-set-su-start-of (car curr) nil)
      (if (null prev)
	  (progn
	    (setq vm-message-list (cdr vm-message-list))
	    (and (cdr curr)
		 (vm-set-reverse-link-of (car (cdr curr)) nil)))
	(setcdr prev (cdr curr))
	(and (cdr curr)
	     (vm-set-reverse-link-of (car (cdr curr)) prev)))
      (vm-mark-folder-modified-p (current-buffer))
      (vm-increment vm-modification-counter)
      ;; a message left the list, so whoever was following its conses has to
      ;; look again: see `vm-message-list-generation'
      (vm-increment vm-message-list-generation))))

(provide 'vm-delete)
;;; vm-delete.el ends here
