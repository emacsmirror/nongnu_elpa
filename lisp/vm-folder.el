;;; vm-folder.el --- VM folder related functions  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-2001 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2008-2010 Uday S. Reddy
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
(require 'vm-toolbar)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)
(eval-when-compile (require 'cl-lib))

;; FIXME: Cyclic dependency.
(provide 'vm-folder)

(require 'vm-delete)
(require 'vm-pop)
(require 'vm-page)


(declare-function vm-update-draft-count "vm.el" ())
(declare-function vm "vm.el"
		  (&optional folder
			     &key read-only access-method reload revisit))
(declare-function vm-mode "vm.el" (&optional read-only))
(declare-function vm-version "vm.el" ())

;; vm-imap.el functions - cyclic dependency
(declare-function vm-imap-make-filename-for-spec "vm-imap" (spec))
(declare-function vm-imap-cache-file-for-folder-name "vm-imap" (name))
(declare-function vm-imap-set-default-attributes "vm-imap" (m))
(declare-function vm-imap-end-session "vm-imap"
		  (process &optional imap-buffer keep-buffer))
(declare-function vm-imap-synchronize-folder "vm-imap" t)
(declare-function vm-net-error-p "vm-net" (value))
(declare-function vm-imap-net-send-changes "vm-imap-net" ())
(declare-function vm-imap-net-stop "vm-imap-net" ())
(declare-function vm-pop-net-stop "vm-pop-net" ())
(declare-function vm-pop-net-send-changes "vm-pop-net" ())
(declare-function vm-imap-find-spec-for-buffer "vm-imap" (buffer))
(declare-function vm-imap-folder-check-mail "vm-imap" (&optional interactive))
(declare-function vm-imap-account-name-for-spec "vm-imap" (spec))

;; vm-virtual.el functions - cyclic dependency
(declare-function vm-virtual-quit "vm-virtual" (&optional no-expunge no-change))
(declare-function vm-virtual-save-folder "vm-virtual" (prefix))
(declare-function vm-virtual-get-new-mail "vm-virtual" ())
(declare-function vm-build-virtual-message-list "vm-virtual"
		  (new-messages &optional dont-finalize))

;; vm-mark.el function
(declare-function vm-marked-messages "vm-mark" ())
		  

;; Operations for vm-folder-access-data

(defsubst vm-folder-pop-maildrop-spec ()
  (aref vm-folder-access-data 0))
(defsubst vm-folder-pop-process ()
  (aref vm-folder-access-data 1))

(defsubst vm-set-folder-pop-maildrop-spec (val)
  (aset vm-folder-access-data 0 val))
(defsubst vm-set-folder-pop-process (val)
  (aset vm-folder-access-data 1 val))

;; the maildrop spec of the imap folder
(defsubst vm-folder-imap-maildrop-spec ()
  (aref vm-folder-access-data 0))
;; current imap process of the folder - each folder has a separate one
(defsubst vm-folder-imap-process ()
  (aref vm-folder-access-data 1))
;; the UIDVALIDITY value of the imap folder on the server
(defsubst vm-folder-imap-uid-validity ()
  (aref vm-folder-access-data 2))
;; the list of uid's and flags of the messages in the imap folder on
;; the server (msg-num . uid . size . flags list)
(defsubst vm-folder-imap-uid-list ()
  (aref vm-folder-access-data 3))	
;; the number of messages in the imap folder on the server
(defsubst vm-folder-imap-mailbox-count ()
  (aref vm-folder-access-data 4))
(defsubst vm-folder-imap-permanent-flags ()
  (aref vm-folder-access-data 8))
;; obarray of uid's with message numbers as their values (on the server)
(defsubst vm-folder-imap-uid-obarray ()
  (aref vm-folder-access-data 9))	; obarray(uid, msg-num)
;; obarray of uid's with flags lists as their values (on the server)
(defsubst vm-folder-imap-flags-obarray ()
  (aref vm-folder-access-data 10))	; obarray(uid, (size . flags list))
					; cons-pair shared with imap-uid-list
;; the number of recent messages in the imap folder on the server
(defsubst vm-folder-imap-recent-count ()
  (aref vm-folder-access-data 11))
;; the number of messages in the imap folder on the server, when last retrieved
(defsubst vm-folder-imap-retrieved-count ()
  (aref vm-folder-access-data 12))

(defsubst vm-set-folder-imap-maildrop-spec (val)
  (aset vm-folder-access-data 0 val))
(defsubst vm-set-folder-imap-process (val)
  (aset vm-folder-access-data 1 val))
(defsubst vm-set-folder-imap-uid-validity (val)
  (aset vm-folder-access-data 2 val))
(defsubst vm-set-folder-imap-uid-list (val)
  (aset vm-folder-access-data 3 val))
(defsubst vm-set-folder-imap-mailbox-count (val)
  (aset vm-folder-access-data 4 val))
(defsubst vm-set-folder-imap-read-write (val)
  (aset vm-folder-access-data 5 val))
(defsubst vm-set-folder-imap-can-delete (val)
  (aset vm-folder-access-data 6 val))
(defsubst vm-set-folder-imap-body-peek (val)
  (aset vm-folder-access-data 7 val))
(defsubst vm-set-folder-imap-permanent-flags (val)
  (aset vm-folder-access-data 8 val))
(defsubst vm-set-folder-imap-uid-obarray (val)
  (aset vm-folder-access-data 9 val))
(defsubst vm-set-folder-imap-flags-obarray (val)
  (aset vm-folder-access-data 10 val))
(defsubst vm-set-folder-imap-recent-count (val)
  (aset vm-folder-access-data 11 val))
(defsubst vm-set-folder-imap-retrieved-count (val)
  (aset vm-folder-access-data 12 val))

;;;###autoload
(defun vm-folder-cache-file (&optional buffer)
  "Say which file holds the local cache of this POP or IMAP folder.

Answers nil for a folder that is a file in the first place.  BUFFER is the
folder to ask about, the current one by default -- and a summary or
presentation buffer counts as its folder, since that is where the reader
is when the question occurs to them.  In a virtual folder the answer is
about the folder the message being looked at really lives in.

A cache file is named after the MD5 of the maildrop, so it can be neither
read nor typed by hand."
  (interactive)
  (let* ((where nil)
         (file (save-current-buffer
                 (when buffer (set-buffer buffer))
                 (vm-select-folder-buffer-if-possible)
                 ;; a virtual folder has no maildrop of its own; the message
                 ;; being looked at came from a folder that has one
                 (when (and (eq major-mode 'vm-virtual-mode) vm-message-pointer)
                   (set-buffer (vm-buffer-of
                                (vm-real-message-of (car vm-message-pointer)))))
                 (setq where (buffer-name))
                 (cond ((eq vm-folder-access-method 'imap)
                        (vm-imap-make-filename-for-spec
                         (vm-folder-imap-maildrop-spec)))
                       ((eq vm-folder-access-method 'pop)
                        (vm-pop-make-filename-for-spec
                         (vm-folder-pop-maildrop-spec)))
                       (t nil)))))
    (when (called-interactively-p 'interactive)
      (if file
          (message "%s" file)
        (message "%s is not a POP or IMAP folder, so nothing caches it"
                 where)))
    file))

(defun vm-set-buffer-modified-p (flag &optional buffer)
  "Sets the `buffer-modified-p' of the current folder to FLAG.  Optional
argument BUFFER can ask for it to be done for some other folder. 

This function is deprecated.  Use `vm-mark-folder-modified-p' or
  `vm-unmark-folder-modified-p' instead."
  (if flag
      (vm-mark-folder-modified-p buffer)
    (vm-unmark-folder-modified-p buffer)))

(defun vm-mark-folder-modified-p (&optional buffer)
  "Sets the `buffer-modified-p' flag of the current folder to t.  Optional
argument BUFFER can ask for it to be done for some other folder. 

This function also zeroes `vm-messages-not-on-disk' and schedules the
folder for redisplay."
  (with-current-buffer (or buffer (current-buffer))
    (set-buffer-modified-p t)
    (vm-increment vm-modification-counter)
    (intern (buffer-name) vm-buffers-needing-display-update)
    (setq vm-messages-not-on-disk 0)))

(defun vm-unmark-folder-modified-p (buffer)
  "Sets the `buffer-modified-p' flag of the current folder to nil."
  (with-current-buffer (or buffer (current-buffer))
    (set-buffer-modified-p nil)
    (vm-increment vm-modification-counter)
    (intern (buffer-name) vm-buffers-needing-display-update)))

(defun vm-reset-buffer-modified-p (value buffer)
  "Sets the `buffer-modified-p' flag of BUFFER to VALUE.  This
is not meant for changing the flag for folders.  Use
`vm-mark-folder-modified-p' or `vm-unmark-folder-modified-p' instead."
  (with-current-buffer buffer
    (set-buffer-modified-p value)))

(defun vm-restore-buffer-modified-p (value buffer)
  "Restores the `buffer-modified-p' flag of BUFFER to a saved VALUE. 
This is the same as `vm-reset-buffer-modified-p' but represents a
specific intent."  
  (with-current-buffer buffer
    (set-buffer-modified-p value)))

(defun vm-message-position (m)
  "Return a message-pointer pointing to the message M in the
`vm-message-list'." 
  (memq m vm-message-list))

(defun vm-number-messages (&optional start-point end-point)
  "Set the number-of slot of the messages in vm-message-list.

If non-nil, START-POINT should point to a cons cell in
vm-message-list and the numbering will begin there, else the
numbering will begin at the head of vm-message-list.  If
START-POINT is non-nil the reverse-link-of slot of the message in
the cons must be valid and the message pointed to (if any) must
have a non-nil number-of slot, because it is used to determine
what the starting message number should be.

If non-nil, END-POINT should point to a cons cell in
vm-message-list and the numbering will end with the message just
before this cell.  A nil value means numbering will be done until
the end of vm-message-list is reached."
  (let ((n 1) 
	(message-list vm-message-list))
    (when (and start-point (vm-reverse-link-of (car start-point)))
      (if (null (vm-number-of (car (vm-reverse-link-of (car start-point)))))
	  (vm-warn 0 2 "%s: Bad numbering start-point; please report bug."
		   (buffer-name))
	(setq n (1+ (string-to-number
		     (vm-number-of
		      (car (vm-reverse-link-of (car start-point))))))
	      message-list start-point)))
    (while (not (eq message-list end-point))
      (vm-set-number-of (car message-list) (int-to-string n))
      (setq n (1+ n) 
	    message-list (cdr message-list)))
    (or end-point (setq vm-ml-highest-message-number (int-to-string (1- n))))
    (if vm-summary-buffer
	(vm-copy-local-variables vm-summary-buffer
				 'vm-ml-highest-message-number))))

(defun vm-set-numbering-redo-start-point (start-point)
  "Set vm-numbering-redo-start-point to START-POINT if appropriate.
Also mark the current buffer as needing a display update.

START-POINT should be a cons in vm-message-list or just t.
 (t means start from the beginning of vm-message-list.)
If START-POINT is closer to the head of vm-message-list than
vm-numbering-redo-start-point or is equal to t, then
vm-numbering-redo-start-point is set to match it.
If START-POINT is nil, nothing is updated."
  (when start-point
    (intern (buffer-name) vm-buffers-needing-display-update)
    (cond ((eq vm-numbering-redo-start-point t)
	   nil)
	  ((and (consp start-point) (consp vm-numbering-redo-start-point))
	   (let ((mp vm-message-list))
	     (while (and mp
			 (not
			  (or (eq (car mp) (car start-point))
			      (eq (car mp) 
				  (car vm-numbering-redo-start-point)))))
	       (setq mp (cdr mp)))
	     (when (null mp)
	       (error 
		"Something is wrong in vm-set-numbering-redo-start-point"))
	     (when (eq (car mp) (car start-point))
	       (setq vm-numbering-redo-start-point start-point))))
	   (t
	    (setq vm-numbering-redo-start-point start-point)))))

(defun vm-set-numbering-redo-end-point (end-point)
  "Set vm-numbering-redo-end-point to END-POINT if appropriate.
Also mark the current buffer as needing a display update.

END-POINT should be a cons in vm-message-list or just t.
 (t means number all the way to the end of vm-message-list.)
If END-POINT is closer to the end of vm-message-list or is equal
to t, then vm-numbering-redo-start-point is set to match it.
The number-of slot is used to determine proximity to the end of
vm-message-list, so this slot must be valid in END-POINT's message
and the message in the cons pointed to by vm-numbering-redo-end-point.
If END-PIONT is nil, nothing is updated."
  (when end-point
    (intern (buffer-name) vm-buffers-needing-display-update)
    (cond ((eq end-point t)
	   (setq vm-numbering-redo-end-point t))
	  ((and (consp end-point)
		(> (string-to-number
		    (vm-number-of
		     (car end-point)))
		   (string-to-number
		    (vm-number-of
		     (car vm-numbering-redo-end-point)))))
	   (setq vm-numbering-redo-end-point end-point))
	  ((null end-point)
	   (setq vm-numbering-redo-end-point end-point)))))

(defun vm-do-needed-renumbering ()
  "Number messages in vm-message-list as specified by
vm-numbering-redo-start-point and vm-numbering-redo-end-point.

vm-numbering-redo-start-point = t means start at the head
of vm-message-list.
vm-numbering-redo-end-point = t means number all the way to the
end of vm-message-list.

Otherwise the variables' values should be conses in vm-message-list
or nil."
  (when vm-numbering-redo-start-point
    ;; vm-number-messages expects nil for defaults, not t!
    (vm-number-messages (if (consp vm-numbering-redo-start-point)
			    vm-numbering-redo-start-point)
			(if (consp vm-numbering-redo-end-point)
			    vm-numbering-redo-end-point))
    (setq vm-numbering-redo-start-point nil
	  vm-numbering-redo-end-point nil)))

(defun vm-set-summary-redo-start-point (start-point)
  "Set vm-summary-redo-start-point to START-POINT if appropriate.
Also mark the current buffer as needing a display update.

START-POINT should be a cons in vm-message-list or just t.
 (t means start from the beginning of vm-message-list.)
If START-POINT is closer to the head of vm-message-list than
vm-summary-redo-start-point or is equal to t, then
vm-summary-redo-start-point is set to match it.
If START-POINT is nil, nothing is updated."
  (when start-point
    (intern (buffer-name) vm-buffers-needing-display-update)
    (cond ((eq vm-summary-redo-start-point t)
	   nil)
	  ((and (consp start-point) (consp vm-summary-redo-start-point))
	   (let ((mp vm-message-list))
	     (while (and mp (not (or (eq mp start-point)
				     (eq mp vm-summary-redo-start-point))))
	       (setq mp (cdr mp)))
	     (when (null mp)
	       (error "Something is wrong in vm-set-summary-redo-start-point"))
	     (when (eq mp start-point)
	       (setq vm-summary-redo-start-point start-point))))
	  (t
	   (setq vm-summary-redo-start-point start-point)))))

(defun vm-discard-summary-cache-of (m)
  "Forget the summary line cached for message M.
Which slot that is depends on what M is: a virtual message's summary lives in
`vm-virtual-summary-of', a real one's in `vm-decoded-tokenized-summary-of'.
Clearing the wrong one leaves the line as it was, which is how a virtual
folder came to show no status letter until it was left and entered again."
  (if (vm-virtual-message-p m)
      (vm-set-virtual-summary-of m nil)
    (vm-set-decoded-tokenized-summary-of m nil)))

(defun vm-mark-for-summary-update (m &optional dont-kill-cache)
  "Mark message M and all its mirrored messages for a summary update.
Also mark M's buffer as needing a display update. Any virtual
messages of M and their buffers are similarly marked for update.
If M is a virtual message and virtual mirroring is in effect for
M (i.e. attribute-of eq attributes-of M's real message), M's real
message and its buffer are scheduled for an update.

Optional arg DONT-KILL-CACHE non-nil means don't invalidate the
summary-of slot for any messages marked for update.  This is
meant to be used by functions that update message information
that is not cached in the summary-of slot, e.g. message numbers
and thread indentation."
  (cond ((eq m (vm-real-message-of m))
	 ;; this is a real message.
	 ;; its summary and modeline need to be updated.
	 (unless dont-kill-cache
	   ;; Toss the cache.  The summary entry cache must be cleared when an
	   ;; attribute of a message that could appear in the summary has
	   ;; changed.  This used to say that it tossed the cache of any
	   ;; virtual message mirroring this one; it did not, their summaries
	   ;; being kept in a slot of their own, so each is cleared below.
	   (vm-discard-summary-cache-of m))
	 (when (vm-su-start-of m)
	   (vm-add-to-list m vm-messages-needing-summary-update))
	 (intern (buffer-name (vm-buffer-of m))
		 vm-buffers-needing-display-update)
	 ;; find the virtual messages of this real message that
	 ;; need a summary update.
	 (dolist (v-m (vm-virtual-messages-of m))
	   (when (eq (vm-attributes-of m) (vm-attributes-of v-m))
	     (unless dont-kill-cache
	       (vm-discard-summary-cache-of v-m))
	     (when (vm-su-start-of v-m)
	       (vm-add-to-list v-m 
			       vm-messages-needing-summary-update))
	     ;; don't trust blindly.  The user could have killed some
	     ;; of these buffers
	     (when (buffer-name (vm-buffer-of v-m))
	       (intern (buffer-name (vm-buffer-of v-m))
		       vm-buffers-needing-display-update)))))
	(t
	 ;; this is a virtual message.
	 ;;
	 ;; if this message has virtual messages then we need to
	 ;; schedule updates for all the virtual messages that
	 ;; share a cache with this message and we need to
	 ;; schedule an update for the underlying real message
	 ;; since we are mirroring it.
	 ;;
	 ;; if there are no virtual messages, then this virtual
	 ;; message is not mirroring its real message so we need
	 ;; only take care of this one message.
	 (if (vm-virtual-messages-of m)
	     (progn
	       ;; schedule updates for all the virtual message which share
	       ;; the same cache as this message.
	       (dolist (v-m (vm-virtual-messages-of m))
		 (when (eq (vm-attributes-of m) (vm-attributes-of v-m))
		   (unless dont-kill-cache
		     (vm-discard-summary-cache-of v-m))
		   (when (vm-su-start-of v-m)
		     (vm-add-to-list v-m 
				     vm-messages-needing-summary-update))
		   (when (buffer-name (vm-buffer-of v-m))
		     (intern (buffer-name (vm-buffer-of v-m))
			     vm-buffers-needing-display-update))))
	       ;; now take care of the real message.  M is a virtual message
	       ;; here, so tossing its cache is not tossing the real one's --
	       ;; which is what the FIXME of 2012-10-14 asked, and the answer
	       ;; was no: this cleared vm-decoded-tokenized-summary-of on a
	       ;; message whose summary is kept in vm-virtual-summary-of, so
	       ;; the virtual folder kept showing the line it already had.
	       (unless dont-kill-cache
		 (vm-discard-summary-cache-of m)
		 (vm-discard-summary-cache-of (vm-real-message-of m)))
	       (when (vm-su-start-of (vm-real-message-of m))
		 (vm-add-to-list (vm-real-message-of m)
				 vm-messages-needing-summary-update))
	       (intern (buffer-name (vm-buffer-of (vm-real-message-of m)))
		       vm-buffers-needing-display-update))
	   (unless dont-kill-cache
	     (vm-discard-summary-cache-of m))
	   (when (vm-su-start-of m)
	     (vm-add-to-list m vm-messages-needing-summary-update))
	   (intern (buffer-name (vm-buffer-of m))
		   vm-buffers-needing-display-update)))))

(defun vm-do-needed-mode-line-update ()
  "Do a modeline update for the current folder buffer.
This means setting up all the various vm-ml attribute variables
in the folder buffer and copying necessary variables to the
folder buffer's summary and presentation buffers, and then
forcing Emacs to update all modelines.

If a virtual folder being updated has no messages, then
erase-buffer is called on its buffer.

If any type of folder is empty, erase-buffer is called
on its presentation buffer, if any."
  ;; XXX This last bit should probably should be moved to
  ;; XXX vm-expunge-folder.

  (if (null vm-message-pointer)
      (progn
	;; erase the leftover message if the folder is really empty.
	(if (eq major-mode 'vm-virtual-mode)
	    (let ((buffer-read-only nil)
		  (omodified (buffer-modified-p)))
	      (unwind-protect
		  (erase-buffer)
		(vm-restore-buffer-modified-p omodified (current-buffer)))))
	(if (and vm-presentation-buffer (buffer-name vm-presentation-buffer))
	    (let ((omodified (buffer-modified-p)))
	      (unwind-protect
		  (with-current-buffer vm-presentation-buffer
		    (let ((buffer-read-only nil))
		      (erase-buffer)))
		(vm-restore-buffer-modified-p omodified (current-buffer))))))
    ;; try to avoid calling vm-su-labels if possible so as to
    ;; avoid loading vm-summary.el.
    (if (vm-decoded-labels-of (car vm-message-pointer))
	(setq vm-ml-labels (vm-su-labels (car vm-message-pointer)))
      (setq vm-ml-labels nil))
    (setq vm-ml-message-number (vm-number-of (car vm-message-pointer)))
    (setq vm-ml-message-new (vm-new-flag (car vm-message-pointer)))
    (setq vm-ml-message-unread (vm-unread-flag (car vm-message-pointer)))
    (setq vm-ml-message-read
	  (and (not (vm-new-flag (car vm-message-pointer)))
	       (not (vm-unread-flag (car vm-message-pointer)))))
    (setq vm-ml-message-edited (vm-edited-flag (car vm-message-pointer)))
    (setq vm-ml-message-filed (vm-filed-flag (car vm-message-pointer)))
    (setq vm-ml-message-written (vm-written-flag (car vm-message-pointer)))
    (setq vm-ml-message-replied (vm-replied-flag (car vm-message-pointer)))
    (setq vm-ml-message-forwarded (vm-forwarded-flag (car vm-message-pointer)))
    (setq vm-ml-message-redistributed (vm-redistributed-flag (car vm-message-pointer)))
    (setq vm-ml-message-deleted (vm-deleted-flag (car vm-message-pointer)))
    (setq vm-ml-message-marked (vm-mark-of (car vm-message-pointer))))
  (if (and vm-summary-buffer (buffer-name vm-summary-buffer))
      (let ((modified (buffer-modified-p)))
	  (vm-copy-local-variables vm-summary-buffer
				   'default-directory
				   'vm-ml-message-new
				   'vm-ml-message-unread
				   'vm-ml-message-read
				   'vm-ml-message-edited
				   'vm-ml-message-replied
				   'vm-ml-message-forwarded
				   'vm-ml-message-filed
				   'vm-ml-message-written
				   'vm-ml-message-deleted
				   'vm-ml-message-marked
                                   'vm-ml-message-redistributed
				   'vm-ml-message-number
				   'vm-ml-highest-message-number
				   'vm-folder-read-only
				   'vm-folder-type
				   'vm-virtual-folder-definition
				   'vm-virtual-mirror
				   'vm-ml-sort-keys
				   'vm-ml-labels
				   'vm-spooled-mail-waiting
				   'vm-ml-session
				   'vm-message-list)
	  (vm-reset-buffer-modified-p modified vm-summary-buffer)))
  (if (and vm-presentation-buffer (buffer-name vm-presentation-buffer))
      (let ((modified (buffer-modified-p)))
	(vm-copy-local-variables vm-presentation-buffer
				 'default-directory
				 'vm-ml-message-new
				 'vm-ml-message-unread
				 'vm-ml-message-read
				 'vm-ml-message-edited
				 'vm-ml-message-replied
				 'vm-ml-message-forwarded
				 'vm-ml-message-filed
				 'vm-ml-message-written
				 'vm-ml-message-deleted
				 'vm-ml-message-marked
				 'vm-ml-message-number
				 'vm-ml-message-redistributed
				 'vm-ml-highest-message-number
				 'vm-folder-read-only
				 'vm-folder-type
				 'vm-virtual-folder-definition
				 'vm-virtual-mirror
				 'vm-ml-labels
				 'vm-spooled-mail-waiting
				 'vm-ml-session
				 'vm-message-list)
	(vm-reset-buffer-modified-p modified vm-presentation-buffer)))
  (vm-force-mode-line-update))

(defun vm-update-summary-and-mode-line ()
  "Update summary and mode line for all VM folder and summary buffers.
Really this updates all the visible status indicators.

Message lists are renumbered.
Summary entries are wiped and regenerated.
Mode lines are updated.
Toolbars are updated."
  (save-excursion
    (vm-update-draft-count)
    (mapatoms (function
	       (lambda (b)
		 (setq b (get-buffer (symbol-name b)))
		 (when b
		   (set-buffer b)
		   (intern (buffer-name)
			   vm-buffers-needing-undo-boundaries)
		   (vm-check-for-killed-summary)
		   (when (and vm-use-toolbar (vm-toolbar-support-possible-p))
		     (vm-toolbar-update-toolbar))
		   (when vm-summary-show-threads
		     (vm-build-threads-if-unbuilt))
		   (vm-do-needed-renumbering)
		   (when vm-summary-buffer
		       (vm-do-needed-summary-rebuild))
		   (vm-do-needed-mode-line-update))))
	      vm-buffers-needing-display-update)
    (fillarray vm-buffers-needing-display-update 0))
  (when vm-messages-needing-summary-update
    (let ((n 1)
	  (ms vm-messages-needing-summary-update)
	  m)
      (while ms
	(setq m (car ms))
	(unless (or (eq (vm-deleted-flag m) 'expunged)
		    (equal (vm-message-id-number-of m) "Q"))
	  (vm-update-message-summary (car ms)))
	(if (eq (mod n 10) 0)
	    (vm-inform 7 "%s: Recreating summary... %s" 
		       (buffer-name vm-mail-buffer) n))
	(setq n (1+ n))
	(setq ms (cdr ms)))
      (vm-inform 7 "%s: Recreating summary... done" 
		 (buffer-name vm-mail-buffer))
      (setq vm-messages-needing-summary-update nil)))
  (vm-force-mode-line-update))

(defun vm-reverse-link-messages ()
  "Set reverse links for all messages in vm-message-list."
  (let ((mp vm-message-list)
	(prev nil))
    (while mp
      (vm-set-reverse-link-of (car mp) prev)
      (setq prev mp mp (cdr mp)))))

(defun vm-match-ordered-header (alist)
  "Try to match a header in ALIST and return the matching cell.
This is used by header ordering code.

ALIST looks like this ((\"From\") (\"To\")).  This function returns
the alist element whose car matches the header starting at point.
The header ordering code uses the cdr of the element
returned to hold headers to be output later."
  (let ((case-fold-search t))
    (catch 'match
      (while alist
	(if (looking-at (car (car alist)))
	    (throw 'match (car alist)))
	(setq alist (cdr alist)))
      nil)))

(defun vm-match-header (&optional header-name)
  "Match a header and save some state information about the matched header.
Optional first arg HEADER-NAME means match the header only
if it matches HEADER-NAME.  HEADER-NAME should be a string
containing a header name.  The string should end with a colon if just
that name should be matched.  A string that does not end in a colon
will match all headers that begin with that string.

State information is stored in vm-matched-header-vector bound to a vector
of this form.

 [ header-start header-end
   header-name-start header-name-end
   header-contents-start header-contents-end ]

Elements are integers.
There are functions to access and use this info."
  (let ((case-fold-search t)
	(header-name-regexp "\\([^ \t\n:]+\\):"))
    (if (if header-name
	    (and (looking-at header-name) (looking-at header-name-regexp))
	  (looking-at header-name-regexp))
	(save-excursion
	  (aset vm-matched-header-vector 0 (point))
	  (aset vm-matched-header-vector 2 (point))
	  (aset vm-matched-header-vector 3 (match-end 1))
	  (goto-char (match-end 0))
	  ;; skip leading whitespace
	  (skip-chars-forward " \t")
	  (aset vm-matched-header-vector 4 (point))
	  (forward-line 1)
	  (while (looking-at "[ \t]")
	    (forward-line 1))
	  (aset vm-matched-header-vector 1 (point))
	  ;; drop the trailing newline
	  (aset vm-matched-header-vector 5 (1- (point)))))))

(defun vm-matched-header ()
  "Returns the header last matched by vm-match-header.
Trailing newline is included."
  (vm-buffer-substring-no-properties (aref vm-matched-header-vector 0)
				     (aref vm-matched-header-vector 1)))

(defun vm-matched-header-name ()
  "Returns the name of the header last matched by vm-match-header."
  (vm-buffer-substring-no-properties (aref vm-matched-header-vector 2)
				     (aref vm-matched-header-vector 3)))

(defun vm-matched-header-contents ()
  "Returns the contents of the header last matched by vm-match-header.
Trailing newline is not included."
  (vm-buffer-substring-no-properties (aref vm-matched-header-vector 4)
				     (aref vm-matched-header-vector 5)))

(defun vm-matched-header-start ()
  "Returns the start position of the header last matched by vm-match-header."
  (aref vm-matched-header-vector 0))

(defun vm-matched-header-end ()
  "Returns the end position of the header last matched by vm-match-header."
  (aref vm-matched-header-vector 1))

(defun vm-matched-header-name-start ()
  "Returns the start position of the name of the header last matched
by vm-match-header."
  (aref vm-matched-header-vector 2))

(defun vm-matched-header-name-end ()
  "Returns the end position of the name of the header last matched
by vm-match-header."
  (aref vm-matched-header-vector 3))

(defun vm-matched-header-contents-start ()
  "Returns the start position of the contents of the header last matched
by vm-match-header."
  (aref vm-matched-header-vector 4))

(defun vm-matched-header-contents-end ()
  "Returns the end position of the contents of the header last matched
by vm-match-header."
  (aref vm-matched-header-vector 5))

(defconst vm-folder-type-aliases
  '((From_-with-Content-Length . mboxcl2))
  "Older names for folder types, and what they are called now.
`mboxcl2' was `From_-with-Content-Length' until 2026.  The old name is still
accepted, and has to be: it is what a user's `vm-default-folder-type' says,
and it is what an index file written before the rename holds -- the folder
type is stored there, so a folder whose index VM has already written would
be misparsed if the name were simply dropped.")

(defun vm-canonical-folder-type (type)
  "Return the current name of folder type TYPE.
An unknown or already-current name is returned unchanged, so this is safe to
apply to anything that might be a folder type."
  (or (cdr (assq type vm-folder-type-aliases)) type))

(defconst vm-folder-types '(From_ BellFrom_ mboxcl2 mmdf babyl)
  "The folder types VM can read and write.
`vm-folder-type-aliases' has the older name for one of them.")

(defun vm-check-folder-type-extensions ()
  "Complain about an entry of `vm-folder-type-by-extension-alist' that cannot work.
Run as VM starts, after the init file has been read.  An extension is matched
literally, so there is nothing to mistype there; a folder type is a symbol,
and a symbol that is not one names a type VM will never give anything."
  (dolist (entry vm-folder-type-by-extension-alist)
    (let ((type (cdr-safe entry)))
      (unless (and (consp entry)
		   (stringp (car entry))
		   (memq (vm-canonical-folder-type type) vm-folder-types))
	(vm-warn 1 2 (concat "vm-folder-type-by-extension-alist: %S is not"
			     " (EXTENSION . TYPE) naming one of %s")
		 entry vm-folder-types)))))

(defun vm-check-default-folder-type ()
  "Complain if `vm-default-folder-type' names a type VM will not create.
Run as VM starts, after the init file has been read.  BellFrom_ was offered
until 2026 and is not now: it is From_ without the blank line between
messages, so it has no signature of its own, and a folder VM writes as one is
read back as From_ with its messages run together.  A configuration that
still asks for it gets what it asks for, and is told once what that means
rather than finding out from a folder.  Issue #787."
  (when (eq (vm-canonical-folder-type vm-default-folder-type) 'BellFrom_)
    (vm-warn 1 2 (concat "vm-default-folder-type is BellFrom_, and a folder"
                         " written as one reads back as From_ with its"
                         " messages run together; set it to From_, or to"
                         " mboxcl2 for a folder kept as a record"))))

(defun vm-folder-type-for-name (file)
  "The folder type FILE's name asks for, or nil if the name says nothing.
The extension decides, matched literally against
`vm-folder-type-by-extension-alist': a folder called sent.mboxcl2 is mboxcl2.

An extension and not a pattern.  A pattern over the whole name can be written
so that it matches nothing, silently, and it can be written so that it claims
a whole directory -- and a directory of nine From_ folders claimed as mboxcl2
is nine folders VM then refuses to read.  Neither can be said in an
extension.  A folder that cannot be renamed therefore cannot be typed by its
name, which is what `vm-default-folder-type' is for."
  (let ((extension (and file (file-name-extension file))))
    (when extension
      (cdr (assoc extension vm-folder-type-by-extension-alist)))))

(defconst vm-folder-type-with-no-name-of-its-own 'From_
  "The type a folder has when its name says nothing about it.
So it is not a type a name has to state.  `.mbox' is read as this and a folder
may be named that way, but a conversion to it does not put the extension on a
folder that has not got one: that would rename INBOX to INBOX.mbox and take a
`vm-primary-inbox' setting with it.")

(defun vm-folder-extension-for-type (type)
  "The file name extension a folder is named with to state TYPE, or nil.
Mostly the reverse of `vm-folder-type-by-extension-alist'.  Nil for a type no
extension names, for every type when a user has emptied that option, and for
`vm-folder-type-with-no-name-of-its-own', which an extension may name without
being the name that type is written under."
  (let ((type (vm-canonical-folder-type type)))
    (unless (eq type vm-folder-type-with-no-name-of-its-own)
      (car (rassq type vm-folder-type-by-extension-alist)))))

(defun vm-folder-name-for-type (file type)
  "The name FILE needs in order to say that it is TYPE.

The extension that states a type replaces one that states any type, so
sent.mboxcl2 converted to From_ is sent, and back again is sent.mboxcl2.  An
extension VM does not know is part of the name and is kept: notes.txt
converted to mboxcl2 is notes.txt.mboxcl2.

Answers FILE itself when its name already states TYPE, so a folder the reader
called sent.mbox is still sent.mbox after being converted to From_ rather than
losing the extension it was given.  And when no extension names TYPE, which is
the case for the types that have no entry, for
`vm-folder-type-with-no-name-of-its-own', and for a user who has emptied
`vm-folder-type-by-extension-alist'."
  (if (eq (vm-folder-type-for-name file) (vm-canonical-folder-type type))
      file
    (let* ((base (if (vm-folder-type-for-name file)
		     (file-name-sans-extension file)
		   file))
	   (extension (vm-folder-extension-for-type type)))
      (if extension (concat base "." extension) base))))

(defun vm-new-folder-file-name (file)
  "The name to create FILE under, given `vm-default-folder-type'.
A folder's type is read back from its name, so a folder created as mboxcl2
under a name that says nothing would be read as From_ next time and split
wherever a body line begins \"From \".  `vm-default-folder-type' decides a
folder VM creates, so where it says mboxcl2 it decides the name too, through
`vm-folder-name-for-type', which is the rule a conversion follows.

FILE itself for a name that already states a type, for a file that exists,
which has a type of its own, and for every other default: a name that says
nothing means From_, and BABYL and MMDF are recognised by what stands at the
front of the file."
  (if (or (file-exists-p file)
	  (vm-folder-type-for-name file)
	  (not (eq (vm-canonical-folder-type vm-default-folder-type) 'mboxcl2)))
      file
    (let ((named (vm-folder-name-for-type file 'mboxcl2)))
      (unless (equal named file)
	(vm-inform 5 "Creating %s: mboxcl2 has to be said in the name"
		   (file-name-nondirectory named)))
      named)))

(defun vm-error-if-name-contradicts-type (file type)
  "Signal unless FILE is a name a TYPE folder may be written under.
The name is where a folder's type is stated, so writing TYPE under a name that
says another type leaves a folder VM refuses to read, and writing mboxcl2
under a name that says nothing leaves one read as From_ and split wherever a
body line begins `From ' (emacs-vm/vm#763).  Neither is worth doing on the
reader's behalf: the conversion stops and says what to call it instead.

Nothing is asked of a name that no extension could state the type in: the
types with no extension of their own, `vm-folder-type-with-no-name-of-its-own',
and every type at all where a reader has emptied
`vm-folder-type-by-extension-alist', which is how to say that names mean
nothing here."
  (let* ((type (vm-canonical-folder-type type))
	 (stated (vm-folder-type-for-name file))
	 (extension (vm-folder-extension-for-type type)))
    (cond ((and stated (not (eq stated type)))
	   (error "%s says it is %s, so it cannot hold %s; write it as %s"
		  (file-name-nondirectory file) stated type
		  (file-name-nondirectory (vm-folder-name-for-type file type))))
	  ((and (null stated) extension)
	   (error "A %s folder has to say so in its name; write it as %s"
		  type
		  (file-name-nondirectory
		   (vm-folder-name-for-type file type)))))))

(defun vm-folder-type-to-write (&optional file)
  "The folder type to write the current folder in.
What the folder already is, else what FILE's name asks for, else
`vm-default-folder-type'.  FILE defaults to the file the buffer is visiting.

The name has to come before the default.  A folder that does not exist yet, or
is empty, has no type of its own to read, and its name is then the only place
its type can have been stated -- which is how an IMAP or POP cache VM creates
comes out as the type `vm-cache-folder-type-suffix' names."
  (or vm-folder-type
      (vm-folder-type-for-name (or file (buffer-file-name)))
      vm-default-folder-type))

(defconst vm-folder-type-examine-limit (* 8 1024 1024)
  "How far into a folder `vm-get-folder-type' will read to decide its type.
It reads as far as the second message when the first says how long it is; a
first message longer than this leaves the folder read as From_, which is what
VM did with every folder before the length was looked at.")

(defun vm-folder-second-message-position ()
  "Where the second message begins, going by the first one's length, or nil.
Point is at the start of a folder that looks like From_.  Answers a position
even when it is past what the buffer holds, which is what says how much of a
file has to be read to see it."
  (save-excursion
    (let ((case-fold-search t))
      (and (re-search-forward vm-content-length-search-regexp nil t)
	   (null (match-beginning 1))
	   (progn (goto-char (match-beginning 0))
		  (vm-match-header vm-content-length-header))
	   (let ((length (string-to-number (vm-matched-header-contents))))
	     (goto-char (match-beginning 0))
	     (and (search-forward "\n\n" nil t)
		  (+ (point) length)))))))

(defun vm-folder-looks-like-mboxcl2-p ()
  "Whether the folder at point is written with a length on every message.

Point is at the start of a folder that looks like From_.  From_ and mboxcl2 are
the same folder but for the `Content-Length' header, so the headers are the
only evidence -- and one message's is not evidence.  Mail arrives carrying a
`Content-Length' of its own, and VM gives one to each message it rewrites, so
a From_ folder ends up with a few.  Read as mboxcl2, such a folder stops at the
first message that has none: 6433 of the 6498 messages in a maintainer's IMAP
cache had none, and the folder would not open at all.

So the first message's length has to say where the message ends, and then:

  - it ends the folder, and there is nothing else to ask.  A folder holding one
    message is all the evidence there is, which is how an FCC file starts.
  - the next message begins there, and it must carry a length too.  Two in a
    row is what tells a folder written this way from a message that came with
    the header.
  - it lands in the middle of something, and the folder is not this type.

Answering from match data is what this replaces: the search for a length could
fail and leave the match of the `From ' at the top of the folder standing, and
that was read as a length having been found."
  (save-excursion
    (let ((case-fold-search t)
	  (length nil))
      (and (re-search-forward vm-content-length-search-regexp nil t)
	   (null (match-beginning 1))
	   (progn (goto-char (match-beginning 0))
		  (and (vm-match-header vm-content-length-header)
		       (setq length (string-to-number
				     (vm-matched-header-contents)))))
	   (progn (goto-char (match-beginning 0))
		  (search-forward "\n\n" nil t))
	   (<= (+ (point) length) (point-max))
	   (progn (forward-char length)
		  ;; a trailing newline the count does not include, which the
		  ;; reader allows for as well
		  (skip-chars-forward "\n")
		  (cond
		   ((eobp) t)
		   ((looking-at "From ")
		    (and (re-search-forward vm-content-length-search-regexp
					    nil t)
			 (null (match-beginning 1))
			 (progn (goto-char (match-beginning 0))
				(vm-match-header vm-content-length-header))))
		   (t nil)))))))

(defun vm-warn-about-deprecated-trust-setting ()
  "Say once at startup that `vm-trust-content-length' is on and deprecated.
Setting `vm-default-folder-type' to mboxcl2 used to require it, so an init
file that asks for mboxcl2 folders almost certainly sets both -- and that is
the pairing being undone: what new folders are written as should not decide
how every folder is read.

A warning and not an error.  The setting still works this release, and an
error here would stop a working configuration from starting."
  (when vm-trust-content-length
    (vm-warn 1 2 (concat "vm-trust-content-length is deprecated: VM decides"
			 " a folder is mboxcl2 by looking at it.  Name the"
			 " folders .mboxcl2 instead"
			 (if (eq vm-default-folder-type 'mboxcl2)
			     ", which vm-default-folder-type no longer needs"
			   "")))))

(defvar vm-unnamed-mboxcl2-caches nil
  "Caches already complained about for looking like mboxcl2, by name.")

(defun vm-warn-about-unnamed-mboxcl2-cache (file)
  "Say that cache FILE looks like mboxcl2 while its name does not say so.
Read as the older format, which is what a cache with no type in its name is
taken for, an mboxcl2 folder splits wherever a body line begins \"From \":
mboxcl2 leaves those alone, the lengths delimiting instead of the separators.
That is a message or two of nonsense in the summary rather than anything lost,
and it is the reader's to settle -- by renaming the file, which is all it
takes when the folder really is mboxcl2, or by converting it when it is not.

`vm-check-folder' is what settles it, and is named here because \"once you are
sure\" said no way of becoming sure.  It counts the lengths over the whole
folder, where this looks at the first two messages, which is as much as a
folder being visited can afford to read."
  (let ((name (or file "this folder")))
    (unless (member name vm-unnamed-mboxcl2-caches)
      (push name vm-unnamed-mboxcl2-caches)
      (vm-warn 1 2 (concat "%s carries lengths but is not named mboxcl2, so it"
			   " is read as %s; M-x vm-check-folder says what the"
			   " contents are, then rename it %s%s or convert it with"
			   " vm-change-folder-type")
	       (file-name-nondirectory name)
	       vm-default-From_-folder-type
	       (file-name-nondirectory name)
	       vm-cache-folder-type-suffix))))

(defvar vm-guessed-folder-types nil
  "Folders `vm-trust-content-length' has already been warned about.
By name, so a folder visited again in the same session is not complained
about again.")

(defun vm-warn-about-guessed-folder-type (file)
  "Say that FILE was read as mboxcl2 because of how it looks, once per folder.
`vm-trust-content-length' is the last release to decide a type by looking, and
a folder read as something it does not say it is is the reason: what the
looking got wrong on one 1.1 GB cache took the folder out of use entirely.
The name is where to say it instead."
  (let ((name (or file (buffer-name))))
    (unless (member name vm-guessed-folder-types)
      (push name vm-guessed-folder-types)
      (vm-warn 1 1 (concat "%s is read as mboxcl2 because its first messages"
			   " have lengths; vm-trust-content-length is"
			   " deprecated, so name such a folder .mboxcl2")
	       (file-name-nondirectory name)))))

(defun vm-get-folder-type (&optional file start end ignore-visited)
  "Return a symbol indicating the folder type of the current buffer.
This function works by examining the beginning of a folder.
If optional arg FILE is present the type of FILE is returned instead.
If FILE is being visited, the type of the buffer is returned.
If optional second and third arg START and END are provided,
vm-get-folder-type will examine the text between those buffer
positions.  START and END default to 1 and (buffer-size) + 1.
If IGNORED-VISITED is non-nil, even if FILE is being visited, its
buffer is ignored and the disk copy of FILE is examined.

Returns
  nil       if folder has no type (empty)
  unknown   if the type is not known to VM
  mmdf      for MMDF folders
  babyl     for BABYL folders
  From_     for BSD UNIX From_ folders
  BellFrom_ for old SysV From_ folders
  mboxcl2
            for new SysV folders that use the Content-Length header

If vm-trust-content-length is non-nil,
mboxcl2 is returned if the first message in the
folder has a Content-Length header and the folder otherwise looks
like a From_ folder.

Since BellFrom_ and From_ folders cannot be reliably distinguished
from each other, you must tell VM which one your system uses by
setting the variable vm-default-From_-folder-type to either From_ or
BellFrom_.  For folders that could be From_ or BellFrom_ folders,
the value of vm-default-From_folder-type will be returned."
  (let ((temp-buffer nil)
	(b nil)
	(case-fold-search nil))
    (unwind-protect
	(save-excursion
	  (if file
	      (progn
		(if (not ignore-visited)
		    (setq b (vm-get-file-buffer file)))
		(if b
		    (set-buffer b)
		  (setq temp-buffer (vm-make-work-buffer))
		  (set-buffer temp-buffer)
		  (if (file-readable-p file)
		      (let ((coding-system-for-read
				(vm-binary-coding-system)))
			(insert-file-contents file nil 0 4096)
			;; Enough to see the second message, when the first says
			;; how long it is.  4096 bytes need not reach even the
			;; end of the first message's headers: in a maintainer's
			;; cache the first message is VM's own bookkeeping and
			;; its header block is 237 kilobytes, so the search for a
			;; length found nothing and the answer came from match
			;; data the previous search had left behind.  A second
			;; read of a bounded region, not of the file: a folder
			;; can be a gigabyte.
			(let ((wanted (vm-folder-second-message-position)))
			  (when (and wanted (> wanted (buffer-size))
				     (<= wanted vm-folder-type-examine-limit))
			    (erase-buffer)
			    (insert-file-contents file nil 0
						  (+ wanted 4096)))))))))
	  (save-excursion
	    (save-restriction
	      (or start (setq start 1))
	      (or end (setq end (1+ (buffer-size))))
	      (widen)
	      (narrow-to-region start end)
	      (goto-char (point-min))
	      (cond ((zerop (buffer-size)) nil)
		    ((looking-at "\n*From ")
		     (let ((named (vm-folder-type-for-name
				   (or file (buffer-file-name)))))
		       (cond
			;; From_ and mboxcl2 are the same folder but for the
			;; Content-Length header, so a folder cannot say which
			;; it is by looking like one -- a name that says
			;; mboxcl2 decides it, and a message with no header is
			;; then the reader's complaint rather than a folder
			;; quietly read as something it does not claim to be.
			((memq named '(From_ BellFrom_ mboxcl2)) named)
			;; A cache whose name does not say mboxcl2 is one VM
			;; wrote before it named its caches, and it is read as
			;; From_ whatever it looks like.  It can be mboxcl2: a
			;; cache is written in `vm-default-folder-type', which
			;; was mboxcl2 on Solaris, AIX and System V until 2026.
			;; But VM cannot tell that from a From_ cache that
			;; collected a few lengths, and 6433 of the 6498 messages
			;; in a maintainer's had none.  A length believed
			;; wrongly puts a message boundary inside a body, where
			;; one ignored is a spurious message the reader can see.
			;; So From_, and the name to rename it to where it looks
			;; like the other thing (#767).
			((vm-cache-folder-name-p (or file (buffer-file-name)))
			 (when (vm-folder-looks-like-mboxcl2-p)
			   (vm-warn-about-unnamed-mboxcl2-cache
			    (or file (buffer-file-name))))
			 vm-default-From_-folder-type)
			((not vm-trust-content-length)
			 vm-default-From_-folder-type)
			(t
			 (if (vm-folder-looks-like-mboxcl2-p)
			     (progn
			       (vm-warn-about-guessed-folder-type
				(or file (buffer-file-name)))
			       'mboxcl2)
			   vm-default-From_-folder-type)))))
		    ((looking-at "\001\001\001\001\n") 'mmdf)
		    ((looking-at "BABYL OPTIONS:") 'babyl)
		    (t 'unknown)))))
      (and temp-buffer (kill-buffer temp-buffer)))))

(defun vm-message-body-octets (start end)
  "The number of octets the text between START and END occupies on disk.
A `Content-Length' counts octets, and so does the reader: `vm-visit-folder'
makes a folder buffer unibyte, so the `forward-char' in
`vm-find-trailing-message-separator' moves over bytes.  A composition buffer
is multibyte, and so is a temporary one a copy is built in, so a character
count would be short by however much of the body is not ASCII."
  (length (encode-coding-string (buffer-substring-no-properties start end)
				(or buffer-file-coding-system
				    (vm-binary-coding-system)))))

(defun vm-content-length-header-line (type)
  "The `Content-Length' line a folder of TYPE wants for this buffer's message,
or nil for a type that carries no such header.  The buffer holds one message:
headers, a blank line, the body.

Every writer of an mboxcl2 folder goes through this, because a message written
into one without a Content-Length cannot be read back -- see
`vm-find-trailing-message-separator', which says so now rather than guessing."
  (when (eq type 'mboxcl2)
    (let ((body (save-excursion
		  (goto-char (point-min))
		  (if (re-search-forward "\n\n" nil t) (point) (point-max)))))
      (format "%s %d\n" vm-content-length-header
	      (vm-message-body-octets body (point-max))))))

(defun vm-set-content-length-of (mm)
  "Make MM's `Content-Length' header say how long its body is now.
Nothing to do in a folder of any other type than mboxcl2, which has no such
header.

In an mboxcl2 folder that header is how the reader finds the end of the
message, so a body inserted or discarded without it being brought up to date
leaves every message after this one misplaced.  An external body arrives
after its headers have been written, and the headers were written with a
length of zero, so both the fetch and the discard have to come through here.

The header is rewritten in place, or inserted at the top of the block if the
message has none.  Neither inserts before `vm-headers-of', so the message's
markers stay where they are."
  (when (eq vm-folder-type 'mboxcl2)
    (let ((line (format "%s %d\n" vm-content-length-header
			(vm-message-body-octets (vm-text-of mm)
						(vm-text-end-of mm))))
	  (case-fold-search t))
      (save-excursion
	(goto-char (vm-headers-of mm))
	(if (re-search-forward (concat "^" (regexp-quote vm-content-length-header)
				       ".*\n")
			       (vm-text-of mm) t)
	    (replace-match line t t)
	  (goto-char (vm-headers-of mm))
	  (insert line))))))

(defun vm-count-messages-in-buffer ()
  "How many messages the current buffer holds, read as `vm-folder-type'.
Used to check that a conversion kept them all; it walks the separators the
same way the reader does, so a folder it cannot parse signals here."
  (save-excursion
    (goto-char (point-min))
    (vm-skip-past-folder-header)
    (let ((n 0))
      (while (vm-find-leading-message-separator)
	(setq n (1+ n))
	(vm-skip-past-leading-message-separator)
	(vm-find-trailing-message-separator)
	(vm-skip-past-trailing-message-separator))
      n)))

(defvar vm-folder-progress-interval 100
  "How many messages between one progress message and the next.
A folder that takes long enough to want reporting on holds thousands, so a
message per message would be the echo area doing more work than the job.
Bound down in the tests, which do not have thousands of messages to spare.")

(defun vm-folder-say-progress (what n &optional total)
  "Say that N of TOTAL messages of WHAT are done, every so often.
TOTAL is omitted where it is not yet known -- a folder has to be walked before
it can be counted, which is itself the slow part on a folder of any size."
  (when (zerop (% n vm-folder-progress-interval))
    (if total
	(vm-inform 5 "%s... %d of %d" what n total)
      (vm-inform 5 "%s... %d" what n))))

(defun vm-convert-folder-type (old-type new-type)
  "Convert buffer from OLD-TYPE to NEW-TYPE.
OLD-TYPE and NEW-TYPE should be symbols returned from vm-get-folder-type.
This should be called on non-live buffers like crash boxes.
This will confuse VM if called on a folder buffer in vm-mode.

Says how far it has got as it goes: this is what the on-disk repair spends its
time in, and it used to say nothing at all while doing it (emacs-vm/vm#748)."
  (let ((vm-folder-type old-type)
	(pos-list nil)
	(found 0)
	(done 0)
	total beg end)
    (goto-char (point-min))
    (vm-skip-past-folder-header)
    (while (vm-find-leading-message-separator)
      (setq pos-list (cons (point-marker) pos-list))
      (vm-skip-past-leading-message-separator)
      (setq pos-list (cons (point-marker) pos-list))
      (vm-find-trailing-message-separator)
      (setq pos-list (cons (point-marker) pos-list))
      (vm-skip-past-trailing-message-separator)
      (setq pos-list (cons (point-marker) pos-list))
      (setq found (1+ found))
      (vm-folder-say-progress "Finding the messages" found))
    (setq pos-list (nreverse pos-list))
    (setq total found)
    (goto-char (point-min))
    (vm-convert-folder-header old-type new-type)
    (while pos-list
      (setq beg (car pos-list))
      (goto-char (car pos-list))
      ;; Keep the envelope line when both types have one: it says who sent the
      ;; message and when it arrived, and generating a new one puts VM's name
      ;; and the time of the conversion there instead.  This function has no
      ;; message structs to ask -- it works on text -- so the line is taken
      ;; from the folder.
      (insert-before-markers
       (or (and (memq old-type '(From_ mboxcl2 BellFrom_))
		(memq new-type '(From_ mboxcl2 BellFrom_))
		(buffer-substring (car pos-list) (car (cdr pos-list))))
	   (vm-leading-message-separator new-type)))
      (delete-region (car pos-list) (car (cdr pos-list)))
      (vm-convert-folder-type-headers old-type new-type)
      (setq pos-list (cdr (cdr pos-list)))
      (setq end (marker-position (car pos-list)))
      (goto-char (car pos-list))
      (insert-before-markers (vm-trailing-message-separator new-type))
      (delete-region (car pos-list) (car (cdr pos-list)))
      (goto-char beg)
      (vm-munge-message-separators new-type beg end)
      (setq pos-list (cdr (cdr pos-list)))
      (setq done (1+ done))
      (vm-folder-say-progress "Converting" done total))
    (vm-inform 5 "Converting... %d messages, done" total)))

(defun vm-convert-folder-header (old-type new-type)
  "Convert the folder header form OLD-TYPE to NEW-TYPE.
The folder header is the text at the beginning of a folder that
is a legal part of the folder but is not part of the first
message.  This is for dealing with BABYL files."
  (if (eq old-type 'babyl)
      (save-excursion
	(let ((beg (point))
	      (case-fold-search t))
	  (cond ((and (looking-at "BABYL OPTIONS:")
		      (search-forward "\037" nil t))
		 (delete-region beg (point)))))))
  (if (eq new-type 'babyl)
      ;; insert before markers so that message location markers
      ;; for the first message get moved forward.
      (insert-before-markers "BABYL OPTIONS:\nVersion: 5\n\037")))

(defun vm-skip-past-folder-header ()
  "Move point past the folder header.
The folder header is the text at the beginning of a folder that
is a legal part of the folder but is not part of the first
message.  This is for dealing with BABYL files."
  (cond ((eq vm-folder-type 'babyl)
	 (search-forward "\037" nil 0))))

(defun vm-convert-folder-type-headers (old-type new-type)
  "Convert headers in the message around point from OLD-TYPE to NEW-TYPE.
This means to add/delete Content-Length and any other
headers related to folder-type as needed for folder type
conversions.  This function expects point to be at the beginning
of the header section of a message, and it only deals with that
message."
  (let (length)
    ;; get the length now before the content-length headers are
    ;; removed.
    (if (eq new-type 'mboxcl2)
	(let (start)
	  (save-excursion
	    (save-excursion
	      (search-forward "\n\n" nil 0)
	      (setq start (point)))
	    (let ((vm-folder-type old-type)
		  ;; This is measuring a message in order to give it a length,
		  ;; so its having none is the ordinary case here and not the
		  ;; folder-is-broken case `vm-mboxcl2-strict' is about.  A
		  ;; digest burst builds its messages and converts them, and
		  ;; with strictness on that raised on the first one.
		  (vm-mboxcl2-strict nil))
	      (vm-find-trailing-message-separator))
	    (setq length (- (point) start)))))
    ;; chop out content-length header if new format doesn't need
    ;; it or if the new format computed his own copy.
    (if (or (eq old-type 'mboxcl2)
	    (eq new-type 'mboxcl2))
	(save-excursion
	  (while (and (let ((case-fold-search t))
			(re-search-forward vm-content-length-search-regexp
					   nil t))
		      (null (match-beginning 1))
		      (progn (goto-char (match-beginning 0))
			     (vm-match-header vm-content-length-header)))
	    (delete-region (vm-matched-header-start)
			   (vm-matched-header-end)))))
    ;; insert the content-length header if needed
    (if (eq new-type 'mboxcl2)
	(save-excursion
	  (insert vm-content-length-header " " (int-to-string length) "\n")))))

(defun vm-munge-message-separators (folder-type start end)
  "Munge message separators of FOLDER-TYPE found between START and END.
This function is used to eliminate message separators for a particular
folder type that happen to occur in a message.  \">\" is prepended to such
separators.

`mboxcl2' is not one of them, and that is the whole
difference between the two Content-Length mbox variants.  A folder that
finds the end of a message by counting its bytes has no need to disfigure a
body line that begins \"From \", and doing both is mboxcl where doing only
the counting is mboxcl2 -- the one variant of the four that stores a message
as it arrived.  Issue #466."
  (save-excursion
    ;; when munging From-type separators it is best to use the
    ;; least forgiving of the folder types, so that we don't
    ;; create folders that other mailers or older versions of VM
    ;; will misparse.
    (if (eq folder-type 'From_)
	(setq folder-type 'BellFrom_))
    (let ((vm-folder-type folder-type))
      (cond ((memq folder-type '(From_ mmdf BellFrom_ babyl))
	     (setq end (vm-marker end))
	     (goto-char start)
	     (while (and (vm-find-leading-message-separator)
			 (< (point) end))
	       (insert ">"))
	     (set-marker end nil))))))

(defun vm-compatible-folder-p (file)
  "Return non-nil if FILE is a compatible folder with the current buffer.
The current folder must have vm-folder-type initialized.
FILE is compatible if
  - it is empty
  - the current folder is empty
  - the two folder types are equal"
  (let ((type (vm-get-folder-type file)))
    (or (not (and vm-folder-type type))
	(eq vm-folder-type type))))

(defun vm-existing-From_-separator (message)
  "MESSAGE's own From_ envelope line, or nil if it has none.
A message in an mbox folder already has one, and it says who sent the
message and when it was delivered.  Converting the folder is no reason to
throw that away."
  (when (memq (vm-message-type-of message) '(From_ mboxcl2 BellFrom_))
    (with-current-buffer (vm-buffer-of message)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (when (looking-at "From [^\n]*\n")
	    (match-string 0)))))))

(defun vm-From_-date (date)
  "DATE, the contents of a Date header, as an envelope line's ctime date.
Nil when it cannot be read, which is what a Date header written by hand
often cannot be."
  (and date (ignore-errors (current-time-string (date-to-time date)))))

(defun vm-From_-address (from)
  "The bare address in FROM, a From header, if it can be an envelope sender.
Nil when there is none, or when it has a space in it: an envelope line is
delimited by spaces, so an address containing one cannot go in it."
  (let ((address (and from (nth 1 (mail-extract-address-components from)))))
    (and address (string-match "\\`[^ \t\n]+\\'" address) address)))

(defun vm-From_-separator (address date)
  "An mbox envelope line naming ADDRESS, dated from DATE, a Date header.
Every other writer of an mbox puts an addr-spec there.  VM names itself when
there is no address to give, which is what it used to do always."
  (concat "From " (or address "VM") " "
	  (or (vm-From_-date date) (current-time-string))
	  "\n"))

(defun vm-make-From_-separator (message)
  "An envelope line built from MESSAGE's From and Date headers.
For a message that has no envelope line of its own -- one coming out of an
MMDF or BABYL folder, say."
  (vm-From_-separator
   (vm-From_-address (vm-get-header-contents message "From:"))
   (vm-get-header-contents message "Date:")))

(defun vm-leading-message-separator (&optional folder-type message
				     for-other-folder)
  "Returns a leading message separator for the current folder.
Defaults to returning a separator for the current folder type.

Optional first arg FOLDER-TYPE means return a separator for that
folder type instead.

Optional second arg MESSAGE should be a message struct.  This is used
generating BABYL separators, because they contain message attributes
and labels that must must be copied from the message.

Optional third arg FOR-OTHER-FOLDER non-nil means that this separator will
be used a `foreign' folder.  This means that the `deleted'
attributes should not be copied for BABYL folders."
  (let ((type (or folder-type vm-folder-type)))
    (cond ((memq type '(From_ mboxcl2 BellFrom_))
	   ;; A composition being filed has no envelope line and no message
	   ;; struct; anything else does, or can have one built.
	   (if message
	       (or (vm-existing-From_-separator message)
		   (vm-make-From_-separator message))
	     (concat "From VM " (current-time-string) "\n")))
	  ((eq type 'mmdf)
	   "\001\001\001\001\n")
	  ((eq type 'babyl)
	   (cond (message
		  (concat "\014\n0,"
			  (vm-babyl-attributes-string message for-other-folder)
			  ",\n*** EOOH ***\n"))
		 (t "\014\n0, recent, unseen,,\n*** EOOH ***\n"))))))

(defun vm-trailing-message-separator (&optional folder-type)
  "Returns a trailing message separator for the current folder.
Defaults to returning a separator for the current folder type.

Optional first arg FOLDER-TYPE means return a separator for that
folder type instead."
  (let ((type (or folder-type vm-folder-type)))
    (cond ((eq type 'From_) "\n")
	  ((eq type 'mboxcl2) "")
	  ((eq type 'BellFrom_) "")
	  ((eq type 'mmdf) "\001\001\001\001\n")
	  ((eq type 'babyl) "\037"))))

(defun vm-folder-header (&optional folder-type label-obarray)
  "Returns a folder header for the current folder.
Defaults to returning a folder header for the current folder type.

Optional first arg FOLDER-TYPE means return a folder header for that
folder type instead.

Optional second arg LABEL-OBARRAY should be an obarray of labels
that have been used in this folder.  This is used for BABYL folders."
  (let ((type (or folder-type vm-folder-type)))
    (cond ((eq type 'babyl)
	   (let ((list nil))
	     (if label-obarray
		 (mapatoms (function
			    (lambda (sym)
			      (setq list (cons sym list))))
			   label-obarray))
	     (if list
		 (format "BABYL OPTIONS:\nVersion: 5\nLabels: %s\n\037"
			 (mapconcat (function symbol-name) list ", "))
	       "BABYL OPTIONS:\nVersion: 5\n\037")))
	  (t ""))))

;; This separator regexp is a bit too permissive.
;; Jose Manuel Garcia-Patos suggests the following
;; "^From .+[@]?.+ .+ [+-]?[0-9][0-9][0-9][0-9]$"
(defvar vm-leading-message-separator-regexp-From_
  "^From .*[0-9]$"
  "Regular expression that matches the leading message separator in
From_ type mail folders.")
(defvar vm-leading-message-separator-regexp-BellFrom_
  "^From .*[0-9]$"
  "Regular expression that matches the leading message separator in
BellFrom_ type mail folders.")
(defvar vm-leading-message-separator-regexp-mboxcl2
  "\\(^\\|\n+\\)From "
  "Regular expression that matches the leading message separator in
mboxcl2 type mail folders.")
(defvar vm-leading-message-separator-regexp-mmdf
  "^\001\001\001\001"
  "Regular expression that matches the leading message separator in
mmdf_ type mail folders.")


(defun vm-find-leading-message-separator ()
  "Find the next leading message separator in a folder.
Returns non-nil if the separator is found, nil otherwise."
  (cond
   ((eq vm-folder-type 'From_)
    (let ((case-fold-search nil))
      (catch 'done
	(while (re-search-forward  
		vm-leading-message-separator-regexp-From_ nil 'no-error)
	  (goto-char (match-beginning 0))
	  (if (or (< (point) 3)
		  (equal (char-after (- (point) 2)) ?\n))
	      (throw 'done t)
	    (forward-char 1)))
	nil )))
   ((eq vm-folder-type 'BellFrom_)
    (let ((case-fold-search nil))
      (if (re-search-forward 
	   vm-leading-message-separator-regexp-BellFrom_ nil 'no-error)
	  (progn
	    (goto-char (match-beginning 0))
	    t )
	nil )))
   ((eq vm-folder-type 'mboxcl2)
    (let ((case-fold-search nil))
      (if (re-search-forward 
	   vm-leading-message-separator-regexp-mboxcl2
	   nil 'no-error)
	  (progn (goto-char (match-end 1)) t)
	nil )))
   ((eq vm-folder-type 'mmdf)
    (let ((case-fold-search nil))
      (if (re-search-forward 
	   vm-leading-message-separator-regexp-mmdf nil 'no-error)
	  (progn
	    (goto-char (match-beginning 0))
	    t )
	nil )))
   ((eq vm-folder-type 'baremessage)
    (goto-char (point-max)))
   ((eq vm-folder-type 'babyl)
    (let ((reg1 "\014\n[01],")
	  (case-fold-search nil))
      (catch 'done
	(while (re-search-forward reg1 nil 'no-error)
	  (goto-char (match-beginning 0))
	  (if (and (not (bobp)) (= (preceding-char) ?\037))
	      (throw 'done t)
	    (forward-char 1)))
	nil )))))

(defun vm-find-From_-in-header-block (headers-start)
  "Position point before a `From ' line in the header block at HEADERS-START.
Returns t if there is one, leaving point on the newline that precedes it,
which is where a From_ trailing message separator goes.  Returns nil, point
unmoved, if there is none.

A `From '-looking line cannot be a header: RFC 5322 wants a field name and a
colon before the first space, and `From alice@example.com  Mon Jan  1 ...'
has neither.  So one inside a header block is a message separator whose
blank line the writer left out, and reading it as such loses nothing that
could have been valid.  Without this the second message is read as the body
of the first and the two are silently merged.  Issue #562.

The search stops at the end of the header block, so a `From ' line in a
message *body* is not a separator and nothing about a well-formed folder
changes.  It starts one character in, so the block's own first line cannot
match and point can never end up before HEADERS-START."
  (let ((case-fold-search nil)
	(block-end (save-excursion
		     (goto-char headers-start)
		     (if (search-forward "\n\n" nil t) (point) (point-max))))
	(found nil))
    (save-excursion
      (goto-char (1+ headers-start))
      (when (re-search-forward "^From " block-end t)
	(setq found (1- (match-beginning 0)))))
    (when found
      (goto-char found)
      t)))

(defun vm-mboxcl2-length-missing (position)
  "Complain about the mboxcl2 message at POSITION having no `Content-Length'.
An error unless `vm-mboxcl2-strict' is nil, in which case one warning for the
folder, naming no message: the caller then falls back on looking for the next
line beginning \"From \", which is how to get such a folder open in order to
repair it.

The strict error stops the read, so numbering its line costs one count.  The
warning stops nothing and is reached once per message, where both halves of
naming one are quadratic: `line-number-at-pos' counts from the start of the
buffer, and `vm-warn' pauses for two seconds on each warning whose text it did
not just show -- a line number in the text making every one of them new.  A
1.1 gigabyte IMAP cache short of a length on 6433 of its 6498 messages paid
both per message, and the repair this error recommends, which reads a folder
this way, did not finish.  It takes seven seconds with the number left out."
  (let ((header (string-remove-suffix ":" vm-content-length-header)))
    (if vm-mboxcl2-strict
	(error (concat "Message at line %d has no %s, which this mboxcl2 folder"
		       " needs.  To repair it: C-u M-x vm-change-folder-type"
		       " mboxcl2, which converts the folder on disk without"
		       " visiting it, gives every message a length, and keeps"
		       " the folder as it was in a backup file")
	       (line-number-at-pos position) header)
      (vm-warn 0 2 (concat "This mboxcl2 folder has messages with no %s;"
			   " looking for the next From_ line instead")
	       header))))

(defun vm-find-trailing-message-separator (&optional headers-start)
  "Find the next trailing message separator in a folder.
HEADERS-START, if given, is where the current message's headers begin, and
allows a From_ folder to notice a separator that has no blank line before it
-- see `vm-find-From_-in-header-block'.  Callers that do not pass it get the
behaviour they always had.

Answers non-nil only when point has been left on the *leading* separator of
the next message rather than on this one's trailing separator, which is what
`vm-build-message-list' reads it as.  Only the From_ header-block case does
that.  Every other arm answers nil, said here rather than left to whatever
the last form happens to return: the mmdf arm returned the `t' of
`vm-find-leading-message-separator', and an mmdf folder could not be read at
all (#786)."
  (cond
   ((eq vm-folder-type 'From_)
    (if (and headers-start (vm-find-From_-in-header-block headers-start))
	t
      (vm-find-leading-message-separator)
      (forward-char -1)
      nil))
   ((eq vm-folder-type 'BellFrom_)
    (vm-find-leading-message-separator)
    nil)
   ((eq vm-folder-type 'mboxcl2)
    (let ((reg1 "^From ")
	  content-length
	  (start-point (point))
	  (case-fold-search nil))
      (if (and (let ((case-fold-search t))
		 (re-search-forward vm-content-length-search-regexp nil t))
	       (null (match-beginning 1))
	       (progn (goto-char (match-beginning 0))
		      (vm-match-header vm-content-length-header)))
	  (progn
	    (setq content-length
		  (string-to-number (vm-matched-header-contents)))
	    ;; if search fails, we'll be at point-max
	    ;; if specified content-length is too long, go to point-max
	    (if (search-forward "\n\n" nil 0)
		(if (>= (- (point-max) (point)) content-length)
		    (forward-char content-length)
		  (goto-char (point-max))))
	    ;; Some systems seem to add a trailing newline that's
	    ;; not counted in the Content-Length header.  Allow
	    ;; any number of them to avoid trouble.
	    (skip-chars-forward "\n"))
	;; The folder says mboxcl2 and this message does not carry the header
	;; that makes it one.  Falling back on the next From_ line, which is
	;; what VM did, reads the folder as something other than what it says
	;; it is and says nothing about a message written wrongly.
	(vm-mboxcl2-length-missing start-point))
      (if (or (eobp) (looking-at reg1))
	  nil
	(goto-char start-point)
	(if (re-search-forward reg1 nil 0)
	    (forward-char -5)))))
   ((eq vm-folder-type 'mmdf)
    (vm-find-leading-message-separator)
    nil)
   ((eq vm-folder-type 'baremessage)
    (goto-char (point-max))
    nil)
   ((eq vm-folder-type 'babyl)
    (vm-find-leading-message-separator)
    (forward-char -1)
    nil)))

(defun vm-skip-past-leading-message-separator ()
  "Move point past a leading message separator at point."
  (cond
   ((memq vm-folder-type '(From_ BellFrom_ mboxcl2))
    (let ((reg1 "^>From ")
	  (case-fold-search nil))
      (forward-line 1)
      (while (looking-at reg1)
	(forward-line 1))))
   ((eq vm-folder-type 'mmdf)
    (forward-char 5)
    ;; skip >From.  Either SCO's MMDF implementation leaves this
    ;; stuff in the message, or many sysadmins have screwed up
    ;; their mail configuration.  Either way I'm tired of getting
    ;; bug reports about it.
    (let ((reg1 "^>From ")
	  (case-fold-search nil))
      (while (looking-at reg1)
	(forward-line 1))))
   ((eq vm-folder-type 'babyl)
    (search-forward "\n*** EOOH ***\n" nil 0))))

(defun vm-skip-past-trailing-message-separator ()
  "Move point past a trailing message separator at point."
  (cond
   ((eq vm-folder-type 'From_)
    (if (not (eobp))
	(forward-char 1)))
   ((eq vm-folder-type 'mboxcl2))
   ((eq vm-folder-type 'BellFrom_))
   ((eq vm-folder-type 'mmdf)
    (forward-char 5))
   ((eq vm-folder-type 'babyl)
    (forward-char 1))))

(defun vm-build-message-list ()
  "Build a chain of message structures, stored them in vm-message-list.
Finds the start and end of each message and fills in the relevant
fields in the message structures.

Also finds the beginning of the header section and the end of the
text section and fills in these fields in the message structures.

vm-text-of and vm-vheaders-of fields don't get filled until they
are needed.

If vm-message-list already contained messages, the end of the last
known message is found and then the parsing of new messages begins
there and the message are appended to vm-message-list.

vm-folder-type is initialized here."
  (setq vm-folder-type (vm-get-folder-type))
  (save-excursion
    (let ((tail-cons nil)
	  (n 0)
	  ;; How many messages ran into the next one with no blank line
	  ;; between them, and whether point is on such a separator now.
	  ;; Issue #562.
	  (run-together 0)
	  (at-separator nil)
	  ;; Just for yucks, make the update interval vary.
	  (modulus (+ (% (vm-abs (random)) 11) 25))
	  message last-end)
      (if vm-message-list
	  ;; there are already messages, therefore we're supposed
	  ;; to add to this list.
	  (let ((mp vm-message-list)
		(end (point-min)))
	    ;; first we have to find physical end of the folder
	    ;; prior to the new messages that just came in.
	    (while mp
	      (if (< end (vm-end-of (car mp)))
		  (setq end (vm-end-of (car mp))))
	      (if (not (consp (cdr mp)))
		  (setq tail-cons mp))
	      (setq mp (cdr mp)))
	    (goto-char end))
	;; there are no messages so we're building the whole list.
	;; start from the beginning of the folder.
	(goto-char (point-min))
	;; whine about newlines at the beginning of the folder.
	;; technically I think this is corruption, but there are
	;; too many busted mail-do-fcc's installed out there to
	;; do more than whine.
	(if (and (memq vm-folder-type '(From_ BellFrom_
					mboxcl2))
		 (= (following-char) ?\n))
	    (vm-warn 0 2 "Warning: newline found at beginning of folder, %s"
		     (or buffer-file-name (buffer-name))))
	(vm-skip-past-folder-header))
      (setq last-end (point))
      ;; parse the messages, set the markers that specify where
      ;; things are.
      ;; `at-separator' says the last message ran into this one, so point is
      ;; already on its separator and searching for one would step over it:
      ;; `vm-find-leading-message-separator' wants a blank line before a From_
      ;; line, which is the very thing missing here.  Issue #562.
      (while (or at-separator (vm-find-leading-message-separator))
	(setq at-separator nil)
	(setq message (vm-make-message))
	(vm-set-message-type-of message vm-folder-type)
	(vm-set-message-access-method-of message vm-folder-access-method)
	(vm-set-start-of message (vm-marker (point)))
	(vm-skip-past-leading-message-separator)
	(vm-set-headers-of message (vm-marker (point)))
	(when (vm-find-trailing-message-separator (point))
	  (vm-increment run-together)
	  (setq at-separator t))
	(vm-assert (>= (point) (marker-position (vm-headers-of message))))
	(vm-set-text-end-of message (vm-marker (point)))
	(vm-skip-past-trailing-message-separator)
	(setq last-end (point))
	(vm-set-end-of message (vm-marker (point)))
	(vm-set-reverse-link-of message tail-cons)
	(if (null tail-cons)
	    (setq vm-message-list (list message)
		  tail-cons vm-message-list)
	  (setcdr tail-cons (list message))
	  (setq tail-cons (cdr tail-cons)))
	(vm-increment n)
	(if (zerop (% n modulus))
	    (vm-inform 7 "%s: Parsing messages... %d" 
		       (buffer-name) n)))
      (if (>= n modulus)
	  (vm-inform 7 "%s: Parsing messages... done"
		       (buffer-name)))
      (if (and (not (= last-end (point-max)))
	       (not (eq vm-folder-type 'unknown)))
	  (vm-warn 1 2
		   "Warning: garbage found at end of folder, %s, starting at %d"
		   (or buffer-file-name (buffer-name))
		   last-end))
      ;; Said once for the folder rather than once per message: whoever
      ;; wrote it left out a blank line, and the user should know their
      ;; mailbox is malformed even though VM has read it correctly.
      (if (> run-together 0)
	  (vm-warn 1 2
		   (concat "Warning: %d message%s in %s ran into the next "
			   "with no blank line between them")
		   run-together (if (= run-together 1) "" "s")
		   (or buffer-file-name (buffer-name)))))))

(defun vm-build-header-order-alist (vheaders)
  (let ((order-alist (cons nil nil))
	list)
    (setq list order-alist)
    (while vheaders
      (setcdr list (cons (cons (car vheaders) nil) nil))
      (setq list (cdr list) vheaders (cdr vheaders)))
    (cdr order-alist)))

;; Reorder the headers in a message.
;;
;; If a message struct is passed into this function, then we're
;; operating on a message in a folder buffer.  Headers are
;; grouped so that the headers that the user wants to see are at
;; the end of the headers section so we can narrow to them.  This
;; is done according to the preferences specified in
;; vm-visible-header and vm-invisible-header-regexp.  The
;; vheaders field of the message struct is also set.  This
;; function is called on demand whenever a vheaders field is
;; discovered to be nil for a particular message.
;;
;; If the message argument is nil, then we are operating on a
;; freestanding message that is not part of a folder buffer.  The
;; keep-list and discard-regexp parameters are used in this case.
;; Headers not matched by the keep list or matched by the discard
;; list are stripped from the message.  The remaining headers
;; are ordered according to the order of the keep list.

;;;###autoload
(cl-defun vm-reorder-message-headers (message ; &optional
				   &key (keep-list nil)
				   (discard-regexp nil))
  (interactive
   (progn 
     (goto-char (point-min))
     (list nil vm-mail-header-order "NO_MATCH_ON_HEADERS:")))
  (save-excursion
    (when message
      (with-current-buffer (vm-buffer-of message)
	(setq keep-list vm-visible-headers
	      discard-regexp vm-invisible-header-regexp)))
    (save-excursion
      (save-restriction
	(widen)
	;; if there is a cached regexp that points to the already
	;; ordered headers then use it and avoid a lot of work.
	(if (and message (vm-vheaders-regexp-of message))
	    (save-excursion
	      (goto-char (vm-headers-of message))
	      (let ((case-fold-search t))
		(re-search-forward (vm-vheaders-regexp-of message)
				   (vm-text-of message) t))
	      (vm-set-vheaders-of message (vm-marker (match-beginning 0))))
	  ;; oh well, we gotta do it the hard way.
	  ;;
	  ;; header-alist will contain an assoc list version of
	  ;; keep-list.  For messages associated with a folder
	  ;; buffer: when a matching header is found, the
	  ;; header's start and end positions are added to its
	  ;; corresponding assoc cell.  The positions of unwanted
	  ;; headers are remember also so that they can be copied
	  ;; to the top of the message, to be out of sight after
	  ;; narrowing.  Once the positions have all been
	  ;; recorded a new copy of the headers is inserted in
	  ;; the proper order and the old headers are deleted.
	  ;;
	  ;; For free standing messages, unwanted headers are
	  ;; stripped from the message, unremembered.
	  (save-restriction
	   (let ((header-alist (vm-build-header-order-alist keep-list))
		 (buffer-read-only nil)
		 (work-buffer nil)
		 (extras nil)
		 list end-of-header vheader-offset
		 (folder-buffer (current-buffer))
		 ;; This prevents file locking from occuring.  Disabling
		 ;; locking can speed things noticeably if the lock directory
		 ;; is on a slow device.  We don't need locking here because
		 ;; in a mail context reordering headers is harmless.
		 (buffer-file-name nil)
		 (case-fold-search t)
		 (unwanted-list nil)
		 unwanted-tail
		 new-header-start
		 old-header-start
		 (old-buffer-modified-p (buffer-modified-p)))
	     (unwind-protect
		 (progn
		   (if message
		       (progn
			 ;; for babyl folders, keep an untouched
			 ;; copy of the headers between the
			 ;; attributes line and the *** EOOH ***
			 ;; line.
			 (if (and (eq vm-folder-type 'babyl)
				  (null (vm-babyl-frob-flag-of message)))
			     (progn
			       (goto-char (vm-start-of message))
			       (forward-line 2)
			       (vm-set-babyl-frob-flag-of message t)
			       (insert-buffer-substring
				(current-buffer)
				(vm-headers-of message)
				(1- (vm-text-of message)))
			       ;; Yep, messages can come in
			       ;; without the two newlines after
			       ;; the header section.
			       (if (not (eq (char-after (1- (point))) ?\n))
				   (insert ?\n))))
			 (setq work-buffer (vm-make-work-buffer))
			 (set-buffer work-buffer)
			 (insert-buffer-substring
			  folder-buffer
			  (vm-headers-of message)
			  (vm-text-of message))
			 (goto-char (point-min))))
		   (setq old-header-start (point))
		   ;; as we loop through the headers, skip >From
		   ;; lines.  these can occur anywhere in the
		   ;; header section if the message has been
		   ;; manhandled by some dumb delivery agents
		   ;; (SCO and Solaris are the usual suspects.)
		   ;; it's a tough ol' world.
		   (while (progn (while (looking-at ">From ")
				   (forward-line))
				 (and (not (= (following-char) ?\n))
				      (vm-match-header)))
		     (setq end-of-header (vm-matched-header-end)
			   list (vm-match-ordered-header header-alist))
		     ;; don't display/keep this header if
		     ;;  keep-list not matched
		     ;;  and discard-regexp is nil
		     ;;       or
		     ;;  discard-regexp is matched
		     (if (or (and (null list) (null discard-regexp))
			     (and discard-regexp
                                  (not (eq 'none discard-regexp))
                                  discard-regexp (looking-at discard-regexp)))
			 ;; delete the unwanted header if not doing
			 ;; work for a folder buffer, otherwise
			 ;; remember the start and end of the
			 ;; unwanted header so we can copy it
			 ;; later.
			 (if (not message)
			     (delete-region (point) end-of-header)
			   (if (null unwanted-list)
			       (setq unwanted-list
				     (cons (point) (cons end-of-header nil))
				     unwanted-tail unwanted-list)
			     (if (= (point) (car (cdr unwanted-tail)))
				 (setcar (cdr unwanted-tail)
					 end-of-header)
			       (setcdr (cdr unwanted-tail)
				       (cons (point)
					     (cons end-of-header nil)))
			       (setq unwanted-tail (cdr (cdr unwanted-tail)))))
			   (goto-char end-of-header))
		       ;; got a match
		       ;; stuff the start and end of the header
		       ;; into the cdr of the returned alist
		       ;; element.
		       (if list
			   ;; reverse point and end-of-header.
			   ;; list will be nreversed later.
			   (setcdr list (cons end-of-header
					      (cons (point)
						    (cdr list))))
			 ;; reverse point and end-of-header.
			 ;; list will be nreversed later.
			 (setq extras
			       (cons end-of-header
				     (cons (point) extras))))
		       (goto-char end-of-header)))
		   (setq new-header-start (point))
		   (while unwanted-list
		     (insert-buffer-substring (current-buffer)
					      (car unwanted-list)
					      (car (cdr unwanted-list)))
		     (setq unwanted-list (cdr (cdr unwanted-list))))
		   ;; remember the offset of where the visible
		   ;; header start so we can initialize the
		   ;; vm-vheaders-of field later.
		   (if message
		       (setq vheader-offset (- (point) new-header-start)))
		   (while header-alist
		     (setq list (nreverse (cdr (car header-alist))))
		     (while list
		       (insert-buffer-substring (current-buffer)
						(car list)
						(car (cdr list)))
		       (setq list (cdr (cdr list))))
		     (setq header-alist (cdr header-alist)))
		   ;; now the headers that were not explicitly
		   ;; undesirable, if any.
		   (setq extras (nreverse extras))
		   (while extras
		     (insert-buffer-substring (current-buffer)
					      (car extras)
					      (car (cdr extras)))
		     (setq extras (cdr (cdr extras))))
		   (delete-region old-header-start new-header-start)
		   ;; update the folder buffer if we're supposed to.
		   ;; lock out interrupts.
		   (if message
		       (let ((inhibit-quit t))
			 (set-buffer (vm-buffer-of message))
			 (goto-char (vm-headers-of message))
			 (insert-buffer-substring work-buffer)
			 (delete-region (point) (vm-text-of message))
			 (vm-restore-buffer-modified-p ; folder-buffer
			  old-buffer-modified-p (current-buffer)))))
	       (when work-buffer (kill-buffer work-buffer)))
	     (if message
		 (progn
		   (vm-set-vheaders-of message
				       (vm-marker (+ (vm-headers-of message)
						     vheader-offset)))
		   ;; cache a regular expression that can be used to
		   ;; find the start of the reordered header the next
		   ;; time this folder is visited.
		   (goto-char (vm-vheaders-of message))
		   (if (vm-match-header)
		       (vm-set-vheaders-regexp-of
			message
			(concat "^" (vm-matched-header-name) ":"))))))))))))

;; Thunderbird source code files describing the status flags
;; http://mxr.mozilla.org/seamonkey/source/mailnews/base/public/nsMsgMessageFlags.h#45
;; http://mxr.mozilla.org/seamonkey/source/mailnews/base/public/nsMsgMessageFlags.h#108
;; Commentary here:
;; http://www.eyrich-net.org/mozilla/X-Mozilla-Status.html?en

(defun vm-thunderbird-folder-p (folder-path-name)
  (file-exists-p (concat folder-path-name ".msf")))

(defun vm-read-thunderbird-status (message)
  (let (status)
    (setq status (vm-get-header-contents message "X-Mozilla-Status:"))
    (when status
      (setq status (string-to-number status 16))
      ;; read flag
      (vm-set-unread-flag-of message (= 0 (logand status #x0001)))
      ;; answered flag
      (vm-set-replied-flag-of message (not (= 0 (logand status #x0002))))
      ;; flagged flag
      (vm-set-flagged-flag-of message (not (= 0 (logand status #x0004))))
      ;; deleted flag
      (vm-set-deleted-flag-of message (not (= 0 (logand status #x0008))))
      ;; folded flag
      (vm-set-folded-flag-of message (not (= 0 (logand status #x0020))))
      ;; watched flag
      (vm-set-watched-flag-of message (not (= 0 (logand status #x0100))))
      ;; Read and not acted on: #x0010 subject carries a "Re:" prefix,
      ;; #x0080 offline article, #x0200 authenticated sender, #x0400 remote
      ;; POP article, #x0800 queued.  VM has no flag of its own for any of
      ;; them.
      ;; forwarded
      (vm-set-forwarded-flag-of message (not (= 0 (logand status #x1000)))))

    (setq status (vm-get-header-contents message "X-Mozilla-Status2:"))
    (when status
      (if (> (length status) 4)
	  (progn
	    (setq status (substring status 0 -4)) ; ignore the last 4 hextets,
					; which are assumed to be 0000
	    (setq status (string-to-number status 16)))
	;; handle badly formatted status strings written by older versions
	(setq status (string-to-number status 16))
	(setq status (/ status #x1000)))
      ;; new on the server
      (vm-set-new-flag-of message (not (= 0 (logand status #x0001))))
      ;; ignored thread
      (vm-set-ignored-flag-of message (not (= 0 (logand status #x0004))))
      ;; read-receipt requested
      (vm-set-read-receipt-flag-of message (not (= 0 (logand status #x0040))))
      ;; read-receipt sent
      (vm-set-read-receipt-sent-flag-of message (not (= 0 (logand status #x0080))))
      ;; has attachments
      (vm-set-attachments-flag-of message (not (= 0 (logand status #x1000))))
      ;; Read and not acted on: #x0020 deleted on the server, #x0100
      ;; template, #x0E00 the label field -- Thunderbird's five labels have
      ;; no counterpart among VM's own labels.
      )

    (vm-mark-for-summary-update message)
    (vm-set-stuff-flag-of message t)))

(defun vm-read-VM-data (message-list)
  "Reads the message attributes and cached header information.

Reads the message attributes and cached header information from the
header portion of the each message, if our X-VM- attributes header is
present.  If the header is not present, assume the message is new,
unless we are being compatible with Berkeley Mail in which case we
also check for a Status header.

If a message already has attributes don't bother checking the
headers.

This function also discovers and stores the position where the
message text begins.

Totals are gathered for use by vm-emit-totals-blurb.

Supports version 4 format of attribute storage, for backward compatibility."
  (save-excursion
    (let ((mp (or message-list vm-message-list))
          (vm-new-count 0)
          (vm-unread-count 0)
          (vm-deleted-count 0)
	  (vm-total-count 0)
	  (vm-bad-cache-count 0)
	  (vm-upgrade-count 0)
	  (modulus (+ (% (vm-abs (random)) 11) 25))
	  (case-fold-search t)
	  oldpoint data cache)
      (while mp
	(vm-increment vm-total-count)
	(if (vm-attributes-of (car mp))
	    ()
	  (goto-char (vm-headers-of (car mp)))
	  ;; find start of text section and save it
	  (search-forward "\n\n" (vm-text-end-of (car mp)) 0)
	  (vm-set-text-of (car mp) (point-marker))
	  ;; now look for our header
	  (goto-char (vm-headers-of (car mp)))
	  (cond
	   ((re-search-forward vm-attributes-header-regexp
			       (vm-text-of (car mp)) t)
	    (goto-char (match-beginning 2))
	    (condition-case ()
		(progn
		  (setq oldpoint (point)
			data (read (current-buffer))
                        cache (cadr data))
		  (when (and (or (not (listp data)) (not (> (length data) 1)))
			     (not (vectorp data)))
		    (error "Bad x-vm-v5-data at %d in buffer %s: %S"
			   oldpoint (buffer-name) data)
		    (sit-for 1))
		  data)
	      (error
	       (vm-warn 1 1
			"Bad x-vm-v5-data header at %d in buffer %s, ignoring"
			oldpoint (buffer-name))
	       (setq data
		     (list
		      (make-vector vm-attributes-vector-length nil)
		      (make-vector vm-cached-data-vector-length nil)
		      nil))
	       ;; In lieu of a valid attributes header
	       ;; assume the message is new.  avoid
	       ;; vm-set-new-flag because it asks for a
	       ;; summary update.
	       (vm-set-new-flag-in-vector (car data) t)))
	    ;; support version 4 format
	    (cond ((vectorp data)
		   (setq data (vm-convert-v4-attributes data))
		   ;; tink the message stuff flag so that if the
		   ;; user saves we get rid of the old v4
		   ;; attributes header.  otherwise we could be
		   ;; dealing with these things for all eternity.
		   (vm-set-stuff-flag-of (car mp) t))
		  (t
		   ;; extend vectors if necessary to accomodate
		   ;; more caching and attributes without alienating
		   ;; other version 5 folders.
		   (cond ((< (length (car data))
			     vm-attributes-vector-length)
			  ;; tink the message stuff flag so that if
			  ;; the user saves we get rid of the old
			  ;; short vector.  otherwise we could be
			  ;; dealing with these things for all
			  ;; eternity.
			  (vm-set-stuff-flag-of (car mp) t)
			  (setcar data (vm-extend-vector
					(car data)
					vm-attributes-vector-length))))
		   (cond ((< (length cache)
			     vm-cached-data-vector-length)
			  ;; tink the message stuff flag so that if
			  ;; the user saves we get rid of the old
			  ;; short vector.  otherwise we could be
			  ;; dealing with these things for all
			  ;; eternity.
			  (vm-set-stuff-flag-of (car mp) t)
			  (setcar (cdr data)
				  (vm-extend-vector
				   cache
				   vm-cached-data-vector-length))
			  (setq cache (cadr data))))))
	    ;; data list might not be long enough for (nth 2 ...)  but
	    ;; that's OK because nth returns nil if you overshoot the
	    ;; end of the list.
            (unless (and (vectorp cache)
			 (>= (length cache) vm-cached-data-vector-length)
			 (or (null (aref cache 7)) (stringp (aref cache 7)))
			 (or (null (aref cache 11)) (stringp (aref cache 11))))
	      (when (zerop vm-bad-cache-count)
		(vm-warn 0 2 "%s: Bad VM cache data: %S" (buffer-name) cache))
	      (vm-set-stuff-flag-of (car mp) t)
	      (vm-increment vm-bad-cache-count)
              (setcar (cdr data)
                      (setq cache 
			    (make-vector vm-cached-data-vector-length nil))))

	    (when (vm-stuff-flag-of (car mp))
	      (vm-increment vm-upgrade-count))
	    (vm-set-decoded-labels-of 
	     (car mp) 
	     (mapcar 'vm-decode-mime-encoded-words-in-string (nth 2 data)))
	    (vm-set-cached-data-of (car mp) cache)
	    (vm-set-attributes-of (car mp) (car data)))
	   ((and vm-berkeley-mail-compatibility
		 (re-search-forward vm-berkeley-mail-status-header-regexp
				    (vm-text-of (car mp)) t))
	    (vm-set-cached-data-of 
	     (car mp) (make-vector vm-cached-data-vector-length nil))
	    (goto-char (match-beginning 1))
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    (vm-set-unread-flag (car mp) (not (looking-at ".*R.*")) 'norecord)
	    (vm-increment vm-upgrade-count))
	   (t
	    (vm-set-cached-data-of 
	     (car mp) (make-vector vm-cached-data-vector-length nil))
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    ;; In lieu of a valid attributes header
	    ;; assume the message is new.  avoid
	    ;; vm-set-new-flag because it asks for a
	    ;; summary update.
	    (vm-set-new-flag-of (car mp) t)))
	  ;; let babyl attributes override the normal VM
	  ;; attributes header.
	  (cond ((eq vm-folder-type 'babyl)
		 (vm-read-babyl-attributes (car mp))))
          ;; read the status flags of Thunderbird
          (if vm-folder-read-thunderbird-status
              (vm-read-thunderbird-status (car mp))))
	(cond ((vm-deleted-flag (car mp))
	       (vm-increment vm-deleted-count))
	      ((vm-new-flag (car mp))
	       (vm-increment vm-new-count))
	      ((vm-unread-flag (car mp))
	       (vm-increment vm-unread-count)))
	(if (zerop (% vm-total-count modulus))
	    (vm-inform 7 "%s: Reading attributes... %d" (buffer-name)
		       vm-total-count))
	(setq mp (cdr mp)))
      (cond ((> vm-bad-cache-count 0)
	     (vm-warn 0 5 
		      (concat "%s: Bad VM cache data found for %s messages; "
			      "Reset to empty data.")
		      (buffer-name) vm-bad-cache-count)))
      (cond ((> vm-upgrade-count vm-bad-cache-count)
	     (vm-warn 0 1 "%s: Attributes data upgraded for %s messages"
		      (buffer-name) (- vm-upgrade-count vm-bad-cache-count)))
	    ((>= vm-total-count modulus)
	     (vm-inform 7 "%s: Reading attributes... done" (buffer-name))))
      (if (null message-list)
	  (setq vm-totals (list vm-modification-counter
				vm-total-count
				vm-new-count
				vm-unread-count
				vm-deleted-count))))))

(defun vm-read-babyl-attributes (message)
  (let ((case-fold-search t)
	(labels nil)
	(vect (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of message vect)
    (save-excursion
      (goto-char (vm-start-of message))
      ;; skip past ^L\n
      (forward-char 2)
      (vm-set-babyl-frob-flag-of message (if (= (following-char) ?1) t nil))
      ;; skip past 0,
      (forward-char 2)
      ;; loop, noting attributes as we go.
      (while (and (not (eobp)) (not (looking-at ",")))
	(cond ((looking-at " unseen,")
	       (vm-set-unread-flag-of message t))
	      ((looking-at " recent,")
	       (vm-set-new-flag-of message t))
	      ((looking-at " deleted,")
	       (vm-set-deleted-flag-of message t))
	      ((looking-at " answered,")
	       (vm-set-replied-flag-of message t))
	      ((looking-at " forwarded,")
	       (vm-set-forwarded-flag-of message t))
	      ((looking-at " filed,")
	       (vm-set-filed-flag-of message t))
	      ((looking-at " redistributed,")
	       (vm-set-redistributed-flag-of message t))
	      ;; only VM knows about these, as far as I know.
	      ((looking-at " edited,")
	       (vm-set-forwarded-flag-of message t))
	      ((looking-at " written,")
	       (vm-set-forwarded-flag-of message t)))
	(skip-chars-forward "^,")
	(and (not (eobp)) (forward-char 1)))
      (and (not (eobp)) (forward-char 1))
      (while (looking-at " \\([^\000-\040,\177-\377]+\\),")
	(setq labels (cons (vm-buffer-substring-no-properties
			    (match-beginning 1)
			    (match-end 1))
			   labels))
	(goto-char (match-end 0)))
      (vm-set-decoded-labels-of message labels))))

(defun vm-set-default-attributes (message-list)
  (let ((mp (or message-list vm-message-list)) attr access-method cache)
    (while mp
      (setq attr (make-vector vm-attributes-vector-length nil)
	    cache (make-vector vm-cached-data-vector-length nil))
      (vm-set-cached-data-of (car mp) cache)
      (vm-set-attributes-of (car mp) attr)
      ;; make message be new by default, but avoid vm-set-new-flag
      ;; because it asks for a summary update for the message.
      (vm-set-new-flag-of (car mp) t)
      (vm-set-unread-flag-of (car mp) t)
      (setq access-method (vm-message-access-method-of (car mp)))
      (cond ((eq access-method 'imap)
	     (vm-imap-set-default-attributes (car mp)))
	    ((eq access-method 'pop)
	     (vm-pop-set-default-attributes (car mp))))
      ;; since this function is usually called in lieu of reading
      ;; attributes from the buffer, the buffer attributes may be
      ;; untrustworthy.  tink the message stuff flag to force the
      ;; new attributes out if the user saves.
      (vm-set-stuff-flag-of (car mp) t)
      (setq mp (cdr mp)))))

(defun vm-compute-totals ()
  (save-excursion
    (vm-select-folder-buffer)
    (let ((mp vm-message-list)
	  (vm-new-count 0)
	  (vm-unread-count 0)
	  (vm-deleted-count 0)
	  (vm-total-count 0))
      (while mp
	(vm-increment vm-total-count)
	(cond ((vm-deleted-flag (car mp))
	       (vm-increment vm-deleted-count))
	      ((vm-new-flag (car mp))
	       (vm-increment vm-new-count))
	      ((vm-unread-flag (car mp))
	       (vm-increment vm-unread-count)))
	(setq mp (cdr mp)))
      (setq vm-totals (list vm-modification-counter
			    vm-total-count
			    vm-new-count
			    vm-unread-count
			    vm-deleted-count)))))

(defun vm-totals-blurb (&optional unlabelled)
  "How many messages the folder holds, and how many are in each state.
New, unread and deleted are counted separately, and a folder with nothing in
it says so.  This is the line the mode line summarises.  The totals are
recomputed only when the folder has changed since they were last worked out.

Answers the line without showing it, for a caller that puts it in a message
of its own: showing it here as well printed the same counts twice, once on
its own and once inside the line that followed it.

UNLABELLED non-nil leaves the folder's name off the front, for a caller
that has named it already.  A caller that prefixed its own name to the
labelled form printed the name twice (emacs-vm/vm#796)."
  (save-excursion
    (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
    (let ((label (if unlabelled "" (concat (buffer-name) ": "))))
      (if (not (equal (nth 0 vm-totals) vm-modification-counter))
	  (vm-compute-totals))
      (if (equal (nth 1 vm-totals) 0)
	  (format "%sNo messages." label)
	(format "%s%d message%s, %d new, %d unread, %d deleted"
		label
		(nth 1 vm-totals) (if (= (nth 1 vm-totals) 1) "" "s")
		(nth 2 vm-totals)
		(nth 3 vm-totals)
		(nth 4 vm-totals))))))

(defun vm-arrival-blurb (count)
  "What to say when COUNT messages have just arrived in the current folder.

The folder is named once.  `vm-totals-blurb' labels itself, so the
asynchronous IMAP and POP paths, which prefixed the folder name and then
appended the labelled blurb, said it twice and gave the new count twice with
it: \"folder: 1 new message.  folder: 3 messages, 1 new, 0 unread, 0
deleted\" (emacs-vm/vm#796).

Both of them build the line here rather than each writing its own, the two
having drifted into the same fault separately."
  (format "%s: %d new message%s.  %s"
	  (buffer-name)
	  count (if (= count 1) "" "s")
	  (vm-totals-blurb t)))

;;;###autoload
(defun vm-emit-totals-blurb ()
  "Show the line `vm-totals-blurb' answers, and answer with it."
  (interactive)
  (let ((blurb (vm-totals-blurb)))
    (vm-inform 5 "%s" blurb)
    blurb))

(defun vm-convert-v4-attributes (data)
  (list (apply 'vector
	       (nconc (vm-vector-to-list data)
		      (make-list (- vm-attributes-vector-length
				    (length data))
				 nil)))
	(make-vector vm-cached-data-vector-length nil)))

(defun vm-gobble-last-modified ()
  (let ((case-fold-search t)
	(time nil)
	lim oldpoint)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-last-modified-header-regexp lim t)
	   (condition-case ()
	       (progn
		 (setq oldpoint (point)
		       time (read (current-buffer)))
		 (unless (consp time)
		   (error "Bad last-modified header at %d in buffer %s"
			  oldpoint (buffer-name))
		   (sit-for 1))
		 time )
	     (error
	      (vm-warn 1 1 
		       "Bad last-modified header at %d in buffer %s, ignoring"
		       oldpoint (buffer-name))
	      (setq time '(0 0 0)))))))
    time ))

(defun vm-gobble-labels ()
  (let ((case-fold-search t)
	lim)
    (save-excursion
      (save-restriction
       (widen)
       (if (eq vm-folder-type 'babyl)
	   (progn
	     (goto-char (point-min))
	     (vm-skip-past-folder-header)
	     (setq lim (point))
	     (goto-char (point-min))
	     (if (re-search-forward "^Labels:" lim t)
		 (let (string list)
		   (setq string (buffer-substring
				 (point)
				 (progn (end-of-line) (point)))
			 list (vm-parse string
"[\000-\040,\177-\377]*\\([^\000-\040,\177-\377]+\\)[\000-\040,\177-\377]*"))
		   (mapc (function
			  (lambda (s)
			    (intern (downcase s) vm-label-obarray)))
			 list))))
	 (goto-char (point-min))
	 (vm-skip-past-folder-header)
	 (vm-skip-past-leading-message-separator)
	 (search-forward "\n\n" nil t)
	 (setq lim (point))
	 (goto-char (point-min))
	 (vm-skip-past-folder-header)
	 (vm-skip-past-leading-message-separator)
	 (if (re-search-forward vm-labels-header-regexp lim t)
	     (let ((oldpoint (point))
		   list)
	       (condition-case ()
		   (progn
		     (setq list (read (current-buffer)))
		     (unless (listp list)
		       (error "Bad global label list at %d in buffer %s"
			      oldpoint (buffer-name))
		       (sit-for 1))
		     list )
		 (error
		  (vm-warn 1 1 
			   "Bad global label list at %d in buffer %s, ignoring"
			   oldpoint (buffer-name))
		  (setq list nil) ))
	       (vm-startup-apply-labels list))))))
    t ))

(defun vm-startup-apply-labels (labels)
  (mapcar (function (lambda (s) (intern s vm-label-obarray))) labels))

(defun vm-register-message-labels (messages)
  "Add the labels carried by MESSAGES to the current folder's label list.
Nothing else does.  A folder learns its labels from its own stored list,
read by `vm-gobble-labels' when it is visited, and from labels the user
adds by hand; a message that arrives already labelled -- new mail with a
label in its `X-VM-v5-Data', or a message saved in from another folder --
would otherwise carry a label the folder never hears about, and that
label would be missing from label completion.

Called for newly assimilated messages.  See `vm-sync-labels' for
repairing a folder whose list has already drifted."
  (dolist (m messages)
    (dolist (label (vm-labels-of m))
      ;; downcased, as `vm-add-or-delete-message-labels' and
      ;; `vm-expunge-label' both do -- a "Work" interned as it stands
      ;; would not be found by an expunge, which downcases its argument
      (intern (downcase label) vm-label-obarray))))

;; Go to the message specified in a bookmark and eat the bookmark.
;; Returns non-nil if successful, nil otherwise.
(defun vm-gobble-bookmark ()
  (let ((case-fold-search t)
	(n nil)
	lim oldpoint)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-bookmark-header-regexp lim t)
	   (condition-case ()
	       (progn
		 (setq oldpoint (point)
		       n (read (current-buffer)))
		 (unless (natnump n)
		   (error "Bad bookmark at %d in buffer %s"
			  oldpoint (buffer-name))
		   (sit-for 1))
		 n )
	     (error
	      (vm-warn 1 1 "Bad bookmark at %d in buffer %s, ignoring"
		       oldpoint (buffer-name))
	      (setq n 1))))))
    (vm-startup-apply-bookmark n)
    t ))

(defun vm-startup-apply-bookmark (n)
  (if n
      (vm-record-and-change-message-pointer
       vm-message-pointer (nthcdr (1- n) vm-message-list)
       :present nil)))

(defun vm-gobble-pop-retrieved ()
  (let ((case-fold-search t)
	ob lim oldpoint)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-pop-retrieved-header-regexp lim t)
	   (condition-case ()
	       (progn
		 (setq oldpoint (point)
		       ob (read (current-buffer)))
		 (unless (listp ob)
		   (error "Bad pop-retrieved header at %d in buffer %s"
			  oldpoint (buffer-name))
		   (sit-for 1))
		 (setq vm-pop-retrieved-messages ob))
	     (error
	      (vm-warn 1 1 
		       "Bad pop-retrieved header at %d in buffer %s, ignoring"
		       oldpoint (buffer-name)))))))
    t ))

(defun vm-gobble-imap-retrieved ()
  (let ((case-fold-search t)
	ob lim oldpoint)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-imap-retrieved-header-regexp lim t)
	   (condition-case ()
	       (progn
		 (setq oldpoint (point)
		       ob (read (current-buffer)))
		 (unless (listp ob)
		   (error "Bad imap-retrieved header at %d in buffer %s"
			  oldpoint (buffer-name))
		   (sit-for 1))
		 (setq vm-imap-retrieved-messages ob))
	     (error
	      (vm-warn 1 1 
		       "Bad imap-retrieved header at %d in buffer %s, ignoring"
		       oldpoint (buffer-name)))))))
    t ))


(defun vm-gobble-imap-to-expunge ()
  "Read back the server deletions a previous session could not send.
The companion of `vm-stuff-imap-to-expunge'; see issue #556.  Entries for a
different UID validity are left for `vm-imap-net-expunge-remote-messages'
to notice and refuse, as it does for any other stale UID."
  (let ((case-fold-search t)
	ob oldpoint lim)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-find-leading-message-separator)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-imap-to-expunge-header-regexp lim t)
	   (condition-case ()
	       (progn
		 (setq oldpoint (point)
		       ob (read (current-buffer)))
		 (unless (listp ob)
		   (error "Bad imap-to-expunge header at %d in buffer %s"
			  oldpoint (buffer-name))
		   (sit-for 1))
		 (setq vm-imap-messages-to-expunge ob))
	     (error
	      (vm-warn 1 1
		       "Bad imap-to-expunge header at %d in buffer %s, ignoring"
		       oldpoint (buffer-name)))))))
    t ))

(defun vm-gobble-visible-header-variables ()
  (save-excursion
    (save-restriction
     (let ((case-fold-search t)
	   lim)
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-vheader-header-regexp lim t)
	   (let (vis invis (got nil))
	     (condition-case ()
		 (setq vis (read (current-buffer))
		       invis (read (current-buffer))
		       got t)
	       (error nil))
	     (if got
		 (vm-startup-apply-header-variables vis invis))))))))

(defun vm-startup-apply-header-variables (vis invis)
  ;; if the variables don't match the values stored when this
  ;; folder was saved, then we have to discard any cached
  ;; vheader info so the user will see the right headers.
  (and (or (not (equal vis vm-visible-headers))
	   (not (equal invis vm-invisible-header-regexp)))
       (let ((mp vm-message-list))
	 (vm-inform 7 "%s: Discarding visible header info..." (buffer-name))
	 (while mp
	   (vm-set-vheaders-regexp-of (car mp) nil)
	   (vm-set-vheaders-of (car mp) nil)
	   (setq mp (cdr mp)))
	 (vm-inform 7 "%s: Discarding visible header info... done" 
		    (buffer-name))
	 )))

;; Read and delete the header that gives the folder's desired
;; message order.
(defun vm-gobble-message-order ()
  (let ((case-fold-search t)
	lim order)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(vm-skip-past-folder-header)
	(vm-skip-past-leading-message-separator)
	(search-forward "\n\n" nil t)
	(setq lim (point))
	(goto-char (point-min))
	(vm-skip-past-folder-header)
	(vm-skip-past-leading-message-separator)
	(when (re-search-forward vm-message-order-header-regexp lim t)
	  (let ((oldpoint (point)))
	    (condition-case nil
		(progn
		  (setq order (read (current-buffer)))
		  (unless (listp order)
		    (error "Bad order header at %d in buffer %s"
			   oldpoint (buffer-name))
		    (sit-for 1))
		  order )
	      (error
	       (vm-warn 1 1 
			"Bad order header at %d in buffer %s, ignoring"
			oldpoint (buffer-name))
	       (setq order nil)))
	    (when order
	      (vm-inform 7 "%s: Reordering messages..." (buffer-name))
	      (vm-startup-apply-message-order order)
	      (vm-inform 7 "%s: Reordering messages... done" (buffer-name)))))
	))))

(defun vm-has-message-order ()
  (let ((case-fold-search t)
	lim) ;; order
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(vm-skip-past-folder-header)
	(vm-skip-past-leading-message-separator)
	(search-forward "\n\n" nil t)
	(setq lim (point))
	(goto-char (point-min))
	(vm-skip-past-folder-header)
	(vm-skip-past-leading-message-separator)
	(re-search-forward vm-message-order-header-regexp lim t)))))

(defun vm-startup-apply-message-order (order)
  (let (list-length v (mp vm-message-list))
    (setq list-length (length vm-message-list)
	  v (make-vector (max list-length (length order)) nil))
    (while (and order mp)
      (condition-case nil
	  (aset v (1- (car order)) (car mp))
	(args-out-of-range nil))
      (setq order (cdr order) mp (cdr mp)))
    ;; lock out interrupts while the message list is in
    ;; an inconsistent state.
    (let ((inhibit-quit t))
      (vm-increment vm-message-list-generation)
      (setq vm-message-list (delq nil (append v mp))
	    vm-message-order-changed nil
	    vm-message-order-header-present t
	    vm-message-pointer (memq (car vm-message-pointer)
				     vm-message-list))
      (vm-set-numbering-redo-start-point t)
      (vm-reverse-link-messages))))

;; Read the header that gives the folder's cached summary format
;; If the current summary format is different, then the cached
;; summary lines are discarded.
(defun vm-gobble-summary ()
  (let ((case-fold-search t)
	summary lim)
    (save-excursion
      (save-restriction
       (widen)
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (vm-skip-past-folder-header)
       (vm-skip-past-leading-message-separator)
       (if (re-search-forward vm-summary-header-regexp lim t)
	   (let ((oldpoint (point)))
	     (condition-case ()
		 (setq summary (read (current-buffer)))
	       (error
		(vm-warn 1 1 
			 "Bad summary header at %d in buffer %s, ignoring"
			 oldpoint (buffer-name))
		(setq summary "")))
	     (vm-startup-apply-summary summary)))))))

(defun vm-startup-apply-summary (summary)
  (if (not (equal summary vm-summary-format))
      (if vm-restore-saved-summary-formats
	  (progn
           (make-local-variable 'vm-summary-format)
           (setq vm-summary-format summary))
	(let ((mp vm-message-list))
	  (while mp
	    (vm-set-decoded-tokenized-summary-of (car mp) nil)
	    ;; force restuffing of cache to clear old
	    ;; summary entry cache.
	    (vm-set-stuff-flag-of (car mp) t)
	    (setq mp (cdr mp)))))))

(defun vm-stuff-message-data (m &optional for-other-folder)
  "Stuff the attributes, labels, soft and cached data of the
message M into the folder buffer.  The optional argument
FOR-OTHER-FOLDER indicates <someting unknown>.  USR 2010-03-06"
  (save-excursion
    (save-restriction
     (widen)
     (let ((old-buffer-modified-p (buffer-modified-p))
	   (case-fold-search t)
	   (buffer-read-only nil)
 	   ;; don't truncate the printing of large Lisp objects
 	   (print-length nil)
	   ;; This prevents file locking from occuring.  Disabling
	   ;; locking can speed things noticeably if the lock
	   ;; directory is on a slow device.  We don't need locking
	   ;; here because the user shouldn't care about VM stuffing
	   ;; its own status headers.
	   (buffer-file-name nil))
       (unwind-protect
	   (vm-stuff-message-data-internal m for-other-folder)
	 (vm-restore-buffer-modified-p	; folder-buffer
	  old-buffer-modified-p (current-buffer)))))))

(defun vm-stuff-message-data-internal (m &optional for-other-folder)
  "Stuff the attributes, labels, soft and cached data of the
message M into the folder buffer.  The optional argument
FOR-OTHER-FOLDER indicates <something unknown>.  USR 2010-03-06"
  (let (attributes cache opoint
	(delflag (vm-deleted-flag m)))
    (progn
      ;; don't put this folder's summary entry into another folder.
      (if for-other-folder
	  (vm-set-decoded-tokenized-summary-of m nil)
	(if (vm-su-start-of m)
	    ;; fill the summary cache if it's not done already.
	    (vm-su-decoded-tokenized-summary m)))
      (setq attributes (vm-attributes-of m)
	    cache (vm-cached-data-of m))
      (when (and delflag for-other-folder)
	(vm-set-deleted-flag-in-vector
	 (setq attributes (copy-sequence attributes)) nil))
      (when (eq vm-folder-type 'babyl)
	(vm-stuff-babyl-attributes m for-other-folder))
      (when (eq vm-sync-thunderbird-status t)
	(vm-stuff-thunderbird-status m))
      (goto-char (vm-headers-of m))
      (while (re-search-forward vm-attributes-header-regexp
				(vm-text-of m) t)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (vm-headers-of m))
      (setq opoint (point))
      (insert				; insert-before-markers?
       vm-attributes-header " ("
       (let ((print-escape-newlines t))
	 (prin1-to-string attributes))
       "\n\t"
       (let ((print-escape-newlines t))
	 (prin1-to-string (vm-mime-encode-words-in-cache-vector cache)))
       "\n\t"
       (let ((print-escape-newlines t))
	 (prin1-to-string (vm-decoded-labels-of m)))
       ")\n")
      (set-marker (vm-headers-of m) opoint)
      (cond ((and (eq vm-folder-type 'From_)
		  vm-berkeley-mail-compatibility)
	     (goto-char (vm-headers-of m))
	     (while (re-search-forward
		     vm-berkeley-mail-status-header-regexp
		     (vm-text-of m) t)
	       (delete-region (match-beginning 0) (match-end 0)))
	     (goto-char (vm-headers-of m))
	     (cond ((not (vm-new-flag m))
		    (insert-before-markers
		     vm-berkeley-mail-status-header
		     (if (vm-unread-flag m) "" "R")
		     "O\n")
		    (set-marker (vm-headers-of m) opoint)))))
      (if for-other-folder
	  (vm-set-stuff-flag-of m nil)	  ; same effect as VM 7.19
	(vm-set-stuff-flag-of m nil))	  ; new
      )))


  
(cl-defun vm-stuff-folder-data (&key interactive abort-if-input-pending)
  "Stuff the soft and cached data of all the messages that have the
stuff-flag set in the current folder.
Keyword parameter INTERACTIVE says whether the stuffing is being done
as part of an interactive command.
ABORT-IF-INPUT-PENDING says stuffing should be aborted if there is
pending input.   So, presumably this is non-interactive.  USR 2012-12-22"
  (let ((newlist nil) mp len (n 0) (p 0) (p-last 0)
	(inform-level (if interactive 8 9)))
    ;; stuff the attributes of messages that need it.
    ;; build a list of messages that need their attributes stuffed
    (setq mp vm-message-list)
    (while mp
      (if (vm-stuff-flag-of (car mp))
	  (setq newlist (cons (car mp) newlist)))
      (setq mp (cdr mp)))
    (when newlist
      (setq len (length newlist))
      (vm-inform inform-level "%s: %d message%s to stuff" (buffer-name)
		 len (if (= 1 len) "" "s")))
    ;; now sort the list by physical order so that we
    ;; reduce the amount of gap motion induced by modifying
    ;; the buffer.  what we want to avoid is updating
    ;; message 3, then 234, then 10, then 500, thus causing
    ;; large chunks of memory to be copied repeatedly as
    ;; the gap moves to accomodate the insertions.
    (let ((vm-key-functions '(vm-sort-compare-physical-order-r)))
      (setq mp (sort newlist 'vm-sort-compare-xxxxxx)))
    (save-excursion
      (save-restriction
       (widen)
       (let ((old-buffer-modified-p (buffer-modified-p))
	     (case-fold-search t)
	     (buffer-read-only nil)
	     ;; don't truncate the printing of large Lisp objects
	     (print-length nil)
	     ;; This prevents file locking from occuring.  Disabling
	     ;; locking can speed things noticeably if the lock
	     ;; directory is on a slow device.  We don't need locking
	     ;; here because the user shouldn't care about VM stuffing
	     ;; its own status headers.
	     (buffer-file-name nil))
	 (unwind-protect
	     (while (and mp 
			 (not (and abort-if-input-pending
			           (input-pending-p))))
	       (vm-stuff-message-data-internal (car mp))
	       (setq n (1+ n))
	       (setq p-last p
		     p (truncate (* 100 n) len))
	       (when (> p p-last)
		 (vm-inform inform-level
			    "%s: Stuffing %d%% complete..." (buffer-name) p))
	       (setq mp (cdr mp)))
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))
	 (if mp nil t))))))

;; we can be a bit lazy in this function since it's only called
;; from within vm-stuff-message-data.  we don't worry about
;; restoring the modified flag, setting buffer-read-only, or
;; about not moving point.
(defun vm-stuff-babyl-attributes (m for-other-folder)
  (goto-char (vm-start-of m))
  (forward-char 2)
  (if (vm-babyl-frob-flag-of m)
      (insert "1")
    (insert "0"))
  (delete-char 1)
  (forward-char 1)
  (if (looking-at "\\( [^\000-\040,\177-\377]+,\\)+")
      (delete-region (match-beginning 0) (match-end 0)))
  (if (vm-new-flag m)
      (insert " recent, unseen,")
    (if (vm-unread-flag m)
	(insert " unseen,")))
  (if (and (not for-other-folder) (vm-deleted-flag m))
      (insert " deleted,"))
  (if (vm-replied-flag m)
      (insert " answered,"))
  (if (vm-forwarded-flag m)
      (insert " forwarded,"))
  (if (vm-redistributed-flag m)
      (insert " redistributed,"))
  (if (vm-filed-flag m)
      (insert " filed,"))
  (if (vm-edited-flag m)
      (insert " edited,"))
  (if (vm-written-flag m)
      (insert " written,"))
  (forward-char 1)
  (if (looking-at "\\( [^\000-\040,\177-\377]+,\\)+")
      (delete-region (match-beginning 0) (match-end 0)))
  (mapcar (function (lambda (label) (insert " " label ",")))
	  (vm-decoded-labels-of m)))

(defun vm-babyl-attributes-string (m for-other-folder)
  (concat
   (if (vm-new-flag m)
       " recent, unseen,"
     (if (vm-unread-flag m)
	 " unseen,"))
   (if (and (not for-other-folder) (vm-deleted-flag m))
       " deleted,")
   (if (vm-replied-flag m)
       " answered,")
   (if (vm-forwarded-flag m)
       " forwarded,")
   (if (vm-redistributed-flag m)
       " redistributed,")
   (if (vm-filed-flag m)
       " filed,")
   (if (vm-edited-flag m)
       " edited,")
   (if (vm-written-flag m)
       " written,")))

(defun vm-stuff-virtual-message-data (message)
  (let ((virtual (vm-virtual-message-p message))
	(real-m (vm-real-message-of message)))
    (if (or (not virtual) (and virtual (vm-virtual-messages-of message)))
	(with-current-buffer
	    (vm-buffer-of real-m)
	  (vm-stuff-message-data real-m)))))

(defun vm-stuff-thunderbird-status (message)
  (let (status status2 status2-hi status2-lo)
    (goto-char (vm-headers-of message))
    (if (re-search-forward "^X-Mozilla-Status: \\([ 0-9A-Fa-f]+\\)\n"
			   (vm-text-of message) t)
	(progn
	  (setq status (buffer-substring (match-beginning 1) (match-end 1)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (setq status (string-to-number status 16))
	  ;; Clear every bit VM writes below and keep the rest, so that a
	  ;; flag turned off here is turned off in the file too, and the
	  ;; bits VM has no flag for -- #x0010 "Re:" prefix, #x0080 offline,
	  ;; #x0200 authenticated sender, #x0400 remote POP, #x0800 queued --
	  ;; survive the round trip.
	  ;; #xeed0 is (lognot (logior #x1 #x2 #x4 #x8 #x0020 #x0100 #x1000))
	  (setq status (logand status #xeed0))
	  )
      (setq status 0))

    (goto-char (vm-headers-of message))
    (if (re-search-forward "^X-Mozilla-Status2: \\([ 0-9A-Fa-f]+\\)\n"
			   (vm-text-of message) t)
	(progn
	  (setq status2 (buffer-substring (match-beginning 1) (match-end 1)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (if (> (length status2) 4)
	      (setq status2-hi (string-to-number (substring status2 0 -4) 16)
		    status2-lo (string-to-number (substring status2 -4 nil) 16))
	    ;; handle badly fomatted status strings written by old
	    ;; versions
	    (setq status2 (string-to-number status2 16)
		  status2-hi (/ status2 #x1000)
		  status2-lo (mod status2 #x1000)))
	  ;; As above.  #x0020 deleted on the server, #x0100 template and
	  ;; the #x0E00 label field are Thunderbird's alone and are kept.
	  ;; #xef3a is (lognot (logior #x1 #x4 #x0040 #x0080 #x1000))
	  (setq status2-hi (logand status2-hi #xef3a)))
      (setq status2 0
	    status2-hi 0
	    status2-lo 0))

    (unless (vm-unread-flag message)
      (setq status (logior status #x1)))
    (when (vm-replied-flag message)
      (setq status (logior status #x2)))
    (when (vm-flagged-flag message)
      (setq status (logior status #x4)))
    (when (vm-deleted-flag message)
      (setq status (logior status #x8)))
    (when (vm-folded-flag message)
      (setq status (logior status #x0020)))
    (when (vm-watched-flag message)
      (setq status (logior status #x0100)))
    (when (vm-forwarded-flag message)
      (setq status (logior status #x1000)))
    (when (vm-new-flag message)
      (setq status2-hi (logior status2-hi #x0001)))
    (when (vm-ignored-flag message)
      (setq status2-hi (logior status2-hi #x0004)))
    (when (vm-read-receipt-flag message)
      (setq status2-hi (logior status2-hi #x0040)))
    (when (vm-read-receipt-sent-flag message)
      (setq status2-hi (logior status2-hi #x0080)))
    (when (vm-attachments-flag message)
      (setq status2-hi (logior status2-hi #x1000)))
    (goto-char (vm-headers-of message))
    (insert (format "X-Mozilla-Status: %04x\n" status))
    (insert (format "X-Mozilla-Status2: %04x%04x\n" status2-hi status2-lo))))
  
(defun vm-stuff-labels ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; don't truncate the printing of large Lisp objects
	       (print-length nil)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       lim)
	   (if (eq vm-folder-type 'babyl)
	       (progn
		 (goto-char (point-min))
		 (vm-skip-past-folder-header)
		 (delete-region (point) (point-min))
		 (insert-before-markers (vm-folder-header vm-folder-type
							  vm-label-obarray))))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (while (re-search-forward vm-labels-header-regexp lim t)
	     (progn (goto-char (match-beginning 0))
		    (if (vm-match-header vm-labels-header)
			(delete-region (vm-matched-header-start)
				       (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the summary header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-labels-header " "
		   (let ((print-escape-newlines t)
			 (list nil))
		     (mapatoms (function
				(lambda (sym)
				  (setq list (cons (symbol-name sym) list))))
			       vm-label-obarray)
		     (prin1-to-string list))
		   "\n")
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

;; Insert a bookmark into the first message in the folder.
(defun vm-stuff-bookmark ()
  (if vm-message-pointer
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-bookmark-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-bookmark-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the bookmark header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-bookmark-header " "
		   (vm-number-of (car vm-message-pointer))
		   "\n")
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

(defun vm-stuff-last-modified ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-last-modified-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-last-modified-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the last-modified header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-last-modified-header " "
		   (prin1-to-string (current-time))
		   "\n")
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

(defun vm-stuff-pop-retrieved ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       (print-length nil)
	       (p vm-pop-retrieved-messages)
	       (curbuf (current-buffer))
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-pop-retrieved-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-pop-retrieved-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the pop-retrieved header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-pop-retrieved-header)
	   (if (null p)
	       (insert " nil\n")
	     (insert "\n   (\n")
	     (while p
	       (insert "\t")
	       (prin1 (car p) curbuf)
	       (insert "\n")
	       (setq p (cdr p)))
	     (insert "   )\n"))
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

(defun vm-stuff-imap-retrieved ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       (print-length nil)
	       (p vm-imap-retrieved-messages)
	       (curbuf (current-buffer))
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-imap-retrieved-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-imap-retrieved-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the imap-retrieved header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-imap-retrieved-header)
	   (if (null p)
	       (insert " nil\n")
	     (insert "\n   (\n")
	     (while p
	       (insert "\t")
	       (prin1 (car p) curbuf)
	       (insert "\n")
	       (setq p (cdr p)))
	     (insert "   )\n"))
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

;; Insert the summary format variable header into the first message.

(defun vm-stuff-imap-to-expunge ()
  "Write into the folder the server deletions that have not been sent yet.
The companion of `vm-gobble-imap-to-expunge'.  `vm-imap-messages-to-expunge'
is buffer-local, so without this a session that ended before it could reach
the server dropped the deletions and the messages stayed on the server for
good, with nothing said -- issue #556."
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; As in vm-stuff-imap-retrieved: no file locking for VM's own
	       ;; status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       (print-length nil)
	       (p vm-imap-messages-to-expunge)
	       (curbuf (current-buffer))
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-imap-to-expunge-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-imap-to-expunge-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   (insert vm-imap-to-expunge-header)
	   (if (null p)
	       (insert " nil\n")
	     (insert "\n   (\n")
	     (while p
	       (insert "\t")
	       (prin1 (car p) curbuf)
	       (insert "\n")
	       (setq p (cdr p)))
	     (insert "   )\n"))
	   (vm-restore-buffer-modified-p	; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

(defun vm-stuff-summary ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; don't truncate the printing of large Lisp objects
	       (print-length nil)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       lim)
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (while (re-search-forward vm-summary-header-regexp lim t)
	     (progn (goto-char (match-beginning 0))
		    (if (vm-match-header vm-summary-header)
			(delete-region (vm-matched-header-start)
				       (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the summary header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-summary-header " "
		   (let ((print-escape-newlines t))
		     (prin1-to-string vm-summary-format))
		   "\n")
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

;; stuff the current values of the header variables for future messages.
(defun vm-stuff-header-variables ()
  (if vm-message-list
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       (print-escape-newlines t)
	       lim
	       ;; don't truncate the printing of large Lisp objects
	       (print-length nil)
	       (buffer-read-only nil)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (while (re-search-forward vm-vheader-header-regexp lim t)
	     (progn (goto-char (match-beginning 0))
		    (if (vm-match-header vm-vheader-header)
			(delete-region (vm-matched-header-start)
				       (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This header will be visible if there
	   ;; are no non-visible headers, oh well, no way around this.
	   (insert vm-vheader-header " "
		   (prin1-to-string vm-visible-headers) " "
		   (prin1-to-string vm-invisible-header-regexp)
		   "\n")
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

;; Insert a header into the first message of the folder that lists
;; the folder's message order.
(defun vm-stuff-message-order ()
  (if (cdr vm-message-list)
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       lim n
	       (buffer-read-only nil)
	       (mp (copy-sequence vm-message-list)))
	   (setq mp
		 (sort mp
		       (function
			(lambda (p q)
			  (< (vm-start-of p) (vm-start-of q))))))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-find-leading-message-separator)
	   (vm-skip-past-leading-message-separator)
	   (while (re-search-forward vm-message-order-header-regexp lim t)
	     (progn (goto-char (match-beginning 0))
		    (if (vm-match-header vm-message-order-header)
			(delete-region (vm-matched-header-start)
				       (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This header will be visible if there
	   ;; are no non-visible headers, oh well, no way around this.
	   (insert vm-message-order-header "\n\t(")
	   (setq n 0)
	   (while mp
	     (insert (vm-number-of (car mp)))
	     (setq n (1+ n) mp (cdr mp))
	     (and mp (insert
		      (if (zerop (% n 15))
			  "\n\t "
			" "))))
	   (insert ")\n")
	   (setq vm-message-order-changed nil
		 vm-message-order-header-present t)
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

;; Remove the message order header.
(defun vm-remove-message-order ()
  (if (cdr vm-message-list)
      (save-excursion
	(save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       lim
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-skip-past-leading-message-separator)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-folder-header)
	   (vm-skip-past-leading-message-separator)
	   (while (re-search-forward vm-message-order-header-regexp lim t)
	     (progn (goto-char (match-beginning 0))
		    (if (vm-match-header vm-message-order-header)
			(delete-region (vm-matched-header-start)
				       (vm-matched-header-end)))))
	   (setq vm-message-order-header-present nil)
	   (vm-restore-buffer-modified-p ; folder-buffer
	    old-buffer-modified-p (current-buffer)))))))

(defun vm-make-index-file-name ()
  (concat (file-name-directory buffer-file-name)
          "."
          (file-name-nondirectory buffer-file-name)
          vm-index-file-suffix))

(defun vm-read-index-file-maybe ()
  (catch 'done
    (if (or (not (stringp buffer-file-name))
	    (not (stringp vm-index-file-suffix)))
	(throw 'done nil))
    (let* ((index-file (vm-make-index-file-name))
           (mtime-buffer (nth 5 (file-attributes buffer-file-name)))
           (mtime-index (nth 5 (file-attributes index-file))))
      (if (and (file-readable-p index-file)
               (>= (car mtime-index) (car mtime-buffer))
               (>= (car (cdr mtime-index)) (car (cdr mtime-buffer))))
          (vm-read-index-file index-file)
	nil))))

(defun vm-read-index-file (index-file)
  (catch 'done
    (condition-case error-data
	(let ((work-buffer nil))
	  (unwind-protect
	      (let (obj attr-list cache-list location-list label-list
		    validity-check vis invis folder-type index-version
		    bookmark summary labels pop-retrieved imap-retrieved
		    imap-to-expunge order
		    v m (m-list nil) tail)
		(vm-inform 5 "%s: Reading index file..." (buffer-name))
		(setq work-buffer (vm-make-work-buffer))
		(with-current-buffer work-buffer
		  (insert-file-contents-literally index-file))
		(goto-char (point-min))

		;; check version
		;; Version 2 adds the not-yet-sent server deletions at the
		;; end (issue #556).  Version 1 is still read, with that list
		;; empty, and the next save writes version 2; an older VM
		;; meeting a version 2 file signals here, and the handler
		;; below ignores the index and parses the folder, which is
		;; correct if slower.  The file is only ever a cache.
		(setq obj (read work-buffer))
		(if (not (memq obj '(1 2)))
		    (error "Unsupported index file version: %s" obj))
		(setq index-version obj)

		;; folder type.  Through `vm-canonical-folder-type' because an
		;; index file written before mboxcl2 was renamed holds the old
		;; name, and every test of the type is against the new one.
		(setq folder-type (vm-canonical-folder-type (read work-buffer)))

		;; validity check
		(setq validity-check (read work-buffer))
		(if (null (vm-check-index-file-validity validity-check))
		    (throw 'done nil))

		;; bookmark
		(setq bookmark (read work-buffer))

		;; message order
		(setq order (read work-buffer))

		;; what summary format was used to produce the
		;; folder's summary cache line.
		(setq summary (read work-buffer))

		;; folder-wide list of labels
		(setq labels (read work-buffer))

		;; what vm-visible-headers / vm-invisible-header-regexp
		;; settings were used to order the headers and to
		;; produce the vm-headers-regexp-of slot value.
		(setq vis (read work-buffer))
		(setq invis (read work-buffer))

		;; location offsets
		;; attributes list
		;; cache list
		;; label list
		(setq location-list (read work-buffer))
		(setq attr-list (read work-buffer))
		(setq cache-list (read work-buffer))
		(setq label-list (read work-buffer))
		(while location-list
		  (setq v (car location-list)
			m (vm-make-message))
		  (if (null m-list)
		      (setq m-list (list m)
			    tail m-list)
		    (setcdr tail (list m))
		    (setq tail (cdr tail)))
		  (vm-set-start-of m (vm-marker (aref v 0)))
		  (vm-set-headers-of m (vm-marker (aref v 1)))
		  (vm-set-text-end-of m (vm-marker (aref v 2)))
		  (vm-set-end-of m (vm-marker (aref v 3)))
		  (if (null attr-list)
		      (error "Attribute list is shorter than location list")
		    (setq v (car attr-list))
		    (if (< (length v) vm-attributes-vector-length)
			(setq v (vm-extend-vector
				 v vm-attributes-vector-length)))
		    (vm-set-attributes-of m v))
		  (if (null cache-list)
		      (error "Cache list is shorter than location list")
		    (setq v (car cache-list))
		    (if (< (length v) vm-cached-data-vector-length)
			(setq v (vm-extend-vector v vm-cached-data-vector-length)))
		    (vm-set-cached-data-of m v))
		  (if (null label-list)
		      (error "Label list is shorter than location list")
		    (vm-set-decoded-labels-of m (car label-list)))
		  (setq location-list (cdr location-list)
			attr-list (cdr attr-list)
			cache-list (cdr cache-list)
			label-list (cdr label-list)))

		;; pop retrieved messages
		(setq pop-retrieved (read work-buffer))

		;; imap retrieved messages
		(setq imap-retrieved (read work-buffer))

		;; server deletions not sent yet -- version 2 and later
		(setq imap-to-expunge (and (>= index-version 2)
					   (read work-buffer)))

		(vm-increment vm-message-list-generation)
		(setq vm-message-list m-list
		      vm-folder-type folder-type
		      vm-pop-retrieved-messages pop-retrieved
		      vm-imap-retrieved-messages imap-retrieved
		      vm-imap-messages-to-expunge imap-to-expunge)

		(vm-startup-apply-bookmark bookmark)
		(and order (vm-startup-apply-message-order order))
		(if vm-summary-show-threads
		    (progn
		      ;; get numbering of new messages done now
		      ;; so that the sort code only has to worry about the
		      ;; changes it needs to make.
		      (vm-update-summary-and-mode-line)
		      (vm-sort-messages (or vm-ml-sort-keys "activity"))))
		(vm-startup-apply-summary summary)
		(vm-startup-apply-labels labels)
		(vm-startup-apply-header-variables vis invis)

		(vm-inform 5 "%s: Reading index file... done" (buffer-name))
		t )
	    (and work-buffer (kill-buffer work-buffer))))
      (error (vm-warn 1 2 "%s: Index file read of %s signaled: %s"
		      (buffer-name) index-file error-data)
	     (vm-warn 1 2 "%s: Ignoring index file..." (buffer-name))))))

(defun vm-check-index-file-validity (blob)
  (save-excursion
    (widen)
    (catch 'done
      (cond ((not (consp blob))
	     (error "Validity check object not a cons: %s" blob))
	    ((eq (car blob) 'file)
	     (let (ch time time2)
	       (setq blob (cdr blob))
	       (setq time (car blob)
		     time2 (vm-gobble-last-modified))
	       (if (and time2 (> 0 (vm-time-difference time time2)))
		   (throw 'done nil))
	       (setq blob (cdr blob))
	       (while blob
		 (setq ch (char-after (car blob)))
		 (if (or (null ch) (not (eq (vm-char-to-int ch) (nth 1 blob))))
		     (throw 'done nil))
		 (setq blob (cdr (cdr blob)))))
	     t )
	    (t (error "Unknown validity check type: %s" (car blob)))))))

(defun vm-generate-index-file-validity-check ()
  (save-restriction
    (widen)
    (let ((step (max 1 (/ (point-max) 11)))
	  (pos (1- (point-max)))
	  (lim (point-min))
	  (blob nil))
      (while (>= pos lim)
	(setq blob (cons pos (cons (vm-char-to-int (char-after pos)) blob))
	      pos (- pos step)))
      (cons 'file (cons (current-time) blob)))))

(defun vm-write-index-file-maybe ()
  (catch 'done
    (if (not (stringp buffer-file-name))
	(throw 'done nil))
    (if (not (stringp vm-index-file-suffix))
	(throw 'done nil))
    (let ((index-file (vm-make-index-file-name)))
      (vm-write-index-file index-file))))

(defun vm-write-index-file (index-file)
  (let ((work-buffer nil))
    (unwind-protect
	(let ((print-escape-newlines t)
	      (print-length nil)
	      m-list mp m)
	  (vm-inform 7 "%s: Sorting for index file..." (buffer-name))
	  (setq m-list (sort (copy-sequence vm-message-list)
			     (function vm-sort-compare-physical-order)))
	  (vm-inform 6 "%s: Stuffing index file..." (buffer-name))
	  (setq work-buffer (vm-make-work-buffer))

	  (princ ";; index file version\n" work-buffer)
	  (prin1 2 work-buffer)
	  (terpri work-buffer)

	  (princ ";; folder type\n" work-buffer)
	  (prin1 vm-folder-type work-buffer)
	  (terpri work-buffer)

	  (princ
	   ";; timestamp + sample of folder bytes for consistency check\n"
	   work-buffer)
	  (prin1 (vm-generate-index-file-validity-check) work-buffer)
	  (terpri work-buffer)

	  (princ ";; bookmark\n" work-buffer)
	  (princ (if vm-message-pointer
		     (vm-number-of (car vm-message-pointer))
		   "1")
		 work-buffer)
	  (terpri work-buffer)

	  (princ ";; message order\n" work-buffer)
	  (let ((n 0) (mp vm-message-list))
	   (princ "(" work-buffer)
	   (setq n 0)
	   (while mp
	     (if (zerop (% n 15))
		 (princ "\n\t" work-buffer)
	       (princ " " work-buffer))
	     (princ (vm-number-of (car mp)) work-buffer)
	     (setq n (1+ n) mp (cdr mp)))
	   (princ "\n)\n" work-buffer))

	  (princ ";; summary\n" work-buffer)
	  (prin1 vm-summary-format work-buffer)
	  (terpri work-buffer)

	  (princ ";; labels used in this folder\n" work-buffer)
	  (let ((list nil))
	    (mapatoms (function
		       (lambda (sym)
			 (setq list (cons (symbol-name sym) list))))
		      vm-label-obarray)
	    (prin1 list work-buffer))
	  (terpri work-buffer)

	  (princ ";; visible headers\n" work-buffer)
	  (prin1 vm-visible-headers work-buffer)
	  (terpri work-buffer)

	  (princ ";; hidden headers\n" work-buffer)
	  (prin1 vm-invisible-header-regexp work-buffer)
	  (terpri work-buffer)

	  (princ ";; location list\n" work-buffer)
	  (princ "(\n" work-buffer)
	  (setq mp m-list)
	  (while mp
	    (setq m (car mp))
	    (princ "  [" work-buffer)
	    (prin1 (marker-position (vm-start-of m)) work-buffer)
	    (princ " " work-buffer)
	    (prin1 (marker-position (vm-headers-of m)) work-buffer)
	    (princ " " work-buffer)
	    (prin1 (marker-position (vm-text-end-of m)) work-buffer)
	    (princ " " work-buffer)
	    (prin1 (marker-position (vm-end-of m)) work-buffer)
	    (princ "]\n" work-buffer)
	    (setq mp (cdr mp)))
	  (princ ")\n" work-buffer)
	  (princ ";; attribute list\n" work-buffer)
	  (princ "(\n" work-buffer)
	  (setq mp m-list)
	  (while mp
	    (setq m (car mp))
	    (princ "  " work-buffer)
	    (prin1 (vm-attributes-of m) work-buffer)
	    (princ "\n" work-buffer)
	    (setq mp (cdr mp)))
	  (princ ")\n" work-buffer)
	  (princ ";; cache list\n" work-buffer)
	  (princ "(\n" work-buffer)
	  (setq mp m-list)
	  (while mp
	    (setq m (car mp))
	    (princ "  " work-buffer)
	    (prin1 (vm-cached-data-of m) work-buffer)
	    (princ "\n" work-buffer)
	    (setq mp (cdr mp)))
	  (princ ")\n" work-buffer)
	  (princ ";; labels list\n" work-buffer)
	  (princ "(\n" work-buffer)
	  (setq mp m-list)
	  (while mp
	    (setq m (car mp))
	    (princ "  " work-buffer)
	    (prin1 (vm-decoded-labels-of m) work-buffer)
	    (princ "\n" work-buffer)
	    (setq mp (cdr mp)))
	  (princ ")\n" work-buffer)
	  (princ ";; retrieved POP messages\n" work-buffer)
	  (let ((p vm-pop-retrieved-messages))
	    (if (null p)
		(princ "nil\n" work-buffer)
	      (princ "(\n" work-buffer)
	      (while p
		(princ "\t" work-buffer)
		(prin1 (car p) work-buffer)
		(princ "\n" work-buffer)
		(setq p (cdr p)))
	      (princ ")\n" work-buffer)))
	  (princ ";; retrieved IMAP messages\n" work-buffer)
	  (let ((p vm-imap-retrieved-messages))
	    (if (null p)
		(princ "nil\n" work-buffer)
	      (princ "(\n" work-buffer)
	      (while p
		(princ "\t" work-buffer)
		(prin1 (car p) work-buffer)
		(princ "\n" work-buffer)
		(setq p (cdr p)))
	      (princ ")\n" work-buffer)))

	  ;; Version 2 and later.  Anything added here goes after the fields
	  ;; an older reader knows about, so that reader stops at the version
	  ;; check rather than misreading the file.
	  (princ ";; IMAP messages to expunge on the server\n" work-buffer)
	  (let ((p vm-imap-messages-to-expunge))
	    (if (null p)
		(princ "nil\n" work-buffer)
	      (princ "(\n" work-buffer)
	      (while p
		(princ "\t" work-buffer)
		(prin1 (car p) work-buffer)
		(princ "\n" work-buffer)
		(setq p (cdr p)))
	      (princ ")\n" work-buffer)))

	  (princ ";; end of index file\n" work-buffer)

	  (vm-inform 6 "%s: Writing index file..." (buffer-name))
	  (catch 'done
	    (with-current-buffer work-buffer
	      (condition-case data
		  (let ((coding-system-for-write (vm-binary-coding-system))
			(selective-display nil))
		    (write-region (point-min) (point-max) index-file))
		(error
		 (vm-warn 1 2 "%s: Write of %s signaled: %s" 
			  (buffer-name) index-file data)
		 (throw 'done nil))))
	    (vm-error-free-call 'set-file-modes index-file (vm-octal 600))
	    (vm-inform 6 "%s: Writing index file... done" (buffer-name))
	    t ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-delete-index-file ()
  (if (stringp vm-index-file-suffix)
      (let ((index-file (vm-make-index-file-name)))
	(vm-error-free-call 'delete-file index-file))))

(defun vm-change-all-new-to-unread ()
  (let ((mp vm-message-list))
    (while mp
      (if (vm-new-flag (car mp))
	  (progn
	    (vm-set-new-flag (car mp) nil)
	    (vm-set-unread-flag (car mp) t)))
      (setq mp (cdr mp)))))

;;;###autoload
(defun vm-mark-message-unread (&optional count)
  "Mark the current message as unread.  If the message is already
new or unread, then it is left unchanged.

Numeric prefix argument N means to mark the current message plus
the next N-1 messages as unread.  A negative N means mark the
current message and the previous N-1 messages as unread.

When invoked on marked messages (via `vm-next-command-uses-marks'),
all marked messages are affected, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are affected."
  (interactive "p")
  (or count (setq count 1))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (let ((mlist (vm-select-operable-messages
		count (vm-interactive-p) "Unread")))
    (while mlist
      (if (and (not (vm-unread-flag (car mlist)))
	       (not (vm-new-flag (car mlist))))
	  (vm-set-unread-flag (car mlist) t))
      (setq mlist (cdr mlist))))
  (vm-display nil nil '(vm-mark-message-unread) '(vm-mark-message-unread))
  (vm-update-summary-and-mode-line))
;;;###autoload (autoload 'vm-unread-message "vm-folder" nil t)
(defalias 'vm-unread-message 'vm-mark-message-unread)

;;;###autoload
(defun vm-mark-message-read (&optional count)
  "Mark the current message as read, i.e., set the `unread' and `new'
attributes to nil.  If the message is already marked as read, then
it is left unchanged.

Numeric prefix argument N means to unread the current message plus the
next N-1 messages.  A negative N means mark the current message and
the previous N-1 messages as read.

When invoked on marked messages (via `vm-next-command-uses-marks'),
all marked messages are affected, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are affected."
  (interactive "p")
  (or count (setq count 1))
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
        ) ;; (del-count 0)
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (let ((mlist (vm-select-operable-messages
		  count (vm-interactive-p) "Mark as read")))
      (while mlist
	(when (or (vm-unread-flag (car mlist))
		  (vm-new-flag (car mlist)))
	  (vm-set-unread-flag (car mlist) nil)
	  (vm-set-new-flag (car mlist) nil))
	(setq mlist (cdr mlist))))
    (vm-display nil nil '(vm-mark-message-read) '(vm-mark-message-read))
    (vm-update-summary-and-mode-line)
    (when (and vm-move-after-reading (not used-marks))
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-reading t))))
	(vm-next-message count t executing-kbd-macro)))))


;;;###autoload
(defun vm-quit-just-bury ()
  "Bury the current VM folder and its auxiliary buffers.
The folder is not altered and Emacs is still visiting it.  You
can switch back to it with switch-to-buffer or by using the
Buffer Menu."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (not (memq major-mode '(vm-mode vm-virtual-mode)))
      (error "%s must be invoked from a VM buffer." this-command))

  (vm--dlet ((virtual (eq major-mode 'vm-virtual-mode))
	     (no-expunge t)
	     (no-change nil))
    (save-excursion (run-hooks 'vm-quit-hook)))

  (vm-garbage-collect-message)

  (vm-display nil nil '(vm-quit-just-bury)
	      '(vm-quit-just-bury quitting))
  (if vm-summary-buffer
      (vm-display vm-summary-buffer nil nil nil))
  (if vm-summary-buffer
      (vm-bury-buffer vm-summary-buffer))
  (if vm-presentation-buffer-handle
      (vm-display vm-presentation-buffer-handle nil nil nil))
  (if vm-presentation-buffer-handle
      (vm-bury-buffer vm-presentation-buffer-handle))
  (vm-display (current-buffer) nil nil nil)
  (vm-bury-buffer (current-buffer)))

;;;###autoload
(defun vm-quit-just-iconify ()
  "Iconify the frame and bury the current VM folder and summary buffers.
The folder is not altered and Emacs is still visiting it."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (not (memq major-mode '(vm-mode vm-virtual-mode)))
      (error "%s must be invoked from a VM buffer." this-command))

  (vm--dlet ((virtual (eq major-mode 'vm-virtual-mode))
	     (no-expunge t)
	     (no-change nil))
    (save-excursion (run-hooks 'vm-quit-hook)))

  (vm-garbage-collect-message)

  (vm-display nil nil '(vm-quit-just-iconify)
	      '(vm-quit-just-iconify quitting))
  (let ((summary-buffer vm-summary-buffer)
	(pres-buffer vm-presentation-buffer-handle))
    (vm-bury-buffer (current-buffer))
    (if summary-buffer
	(vm-bury-buffer summary-buffer))
    (if pres-buffer
	(vm-bury-buffer pres-buffer))
    (vm-iconify-frame)))

;;;###autoload
(defun vm-quit-no-change ()
  "Quit visiting the current folder and discard any changes made to the folder."
  (interactive)
  (vm-quit t t))

;;;###autoload
(defun vm-quit-no-expunge ()
  "Quit visiting the current folder without expunging deleted
messages.  

The setting of `vm-expunge-before-quit' is ignored."
  (interactive)
  (vm-quit t nil))

(defvar dired-listing-switches)		; defined only in FSF Emacs?

(defun vm-folder-left-after-quitting ()
  "The folder buffer a quit leaves behind, or nil if it quit the last one.
`buffer-list' is in most-recently-used order, so the first VM folder in it
is the one the reader came from."
  (seq-find (lambda (buffer)
	      (with-current-buffer buffer
		(memq major-mode '(vm-mode vm-virtual-mode))))
	    (buffer-list)))

(defun vm-display-folder-left-after-quitting ()
  "Put the summary of the folder a quit returns to back on display.
The quitting folder\\='s windows go with it, and `vm-undisplay-buffer' hands
them to whatever `other-buffer' answers.  On leaving a virtual folder that
is the real folder\\='s presentation buffer, `vm-virtual-quit' having just
presented into it, so the summary was left displayed nowhere and the reader
had to press a key to bring it back (emacs-vm/vm#821)."
  (let ((folder (vm-folder-left-after-quitting)))
    (when folder
      (with-current-buffer folder
	(when (buffer-live-p vm-summary-buffer)
	  (vm-display vm-summary-buffer t nil nil))))))

;;;###autoload
(defun vm-quit (&optional no-expunge no-change)
  "Quit visiting the current folder, saving changes.  If the folder is
being visited read-only then changes are not saved.  This behavior
can be customized using `vm-preserve-read-only-folders-on-disk'.

If the customization variable `vm-expunge-before-quit' is set to
  non-nil value then deleted messages are expunged.

Giving a prefix argument overrides the variable and no expunge is
done.

When called internally, the optional argument NO-EXPUNGE says
that the deleted messages should not be expunged (irrespective of
the value of `vm-expunge-before-quit'.  NO-CHANGE says that
changes should be discarded."
  (interactive "P")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (not (memq major-mode '(vm-mode vm-virtual-mode)))
      (error "%s must be invoked from a VM buffer." this-command))
  (vm-display nil nil '(vm-quit vm-quit-no-change vm-quit-no-expunge)
	      (list this-command 'quitting))
  (if (and vm-folder-read-only vm-preserve-read-only-folders-on-disk)
      (setq no-change t))
  (let ((virtual (eq major-mode 'vm-virtual-mode)))

    ;; 1. Save folder if necessary
    ;; Why are we saving before expunging?  USR, 2012-11-12
    (unless (or virtual
		(and vm-folder-read-only vm-preserve-read-only-folders-on-disk))
      (cond
       ((and no-change (buffer-modified-p)
	     (or buffer-file-name buffer-offer-save)
	     (not (zerop vm-messages-not-on-disk))
	     ;; Folder may have been saved with C-x C-s and attributes may have
	     ;; been changed after that; in that case vm-messages-not-on-disk
	     ;; would not have been zeroed.  However, all modification flag
	     ;; undos are cleared if VM actually modifies the folder buffer
	     ;; (as opposed to the folder's attributes), so this can be used
	     ;; to verify that there are indeed unsaved messages.
	     (null (assq 'vm-set-buffer-modified-p vm-undo-record-list))
	     (not
	      (y-or-n-p
	       (format
		"%s: %d message%s have not been saved to disk, quit anyway? "
		(buffer-name)
		vm-messages-not-on-disk
		(if (= 1 vm-messages-not-on-disk) "" "s")))))
	(error "Aborted"))
       ((and no-change
	     (or buffer-file-name buffer-offer-save)
	     (buffer-modified-p)
	     vm-confirm-quit
	     (not (y-or-n-p 
		   (format "%s: There are unsaved changes, quit anyway?  "
			   (buffer-name)))))
	(error "Aborted"))
       ((and (eq vm-confirm-quit t)
	     (not (y-or-n-p 
		   (format "%s: Do you really want to quit? "
			   (buffer-name)))))
	(error "Aborted"))))

    ;; 2. Run vm-quit-hook
    (save-excursion (run-hooks 'vm-quit-hook))

    ;; 3. Expunge folder if necessary
    (when vm-expunge-before-quit
      (unless (or virtual
		  no-expunge
		  no-change
		  (not (buffer-modified-p)))
	(vm-expunge-folder)))

    (vm-garbage-collect-message)
    (vm-garbage-collect-folder)

    ;; 4. Save folder if necessary
    (unless (or no-change virtual)
      ;; this could take a while, so give the user some feedback
      (vm-inform 5 "%s: Quitting..." (buffer-name))
      (unless (or vm-folder-read-only (eq major-mode 'vm-virtual-mode))
	(vm-change-all-new-to-unread)))
    (when (and (buffer-modified-p)
	       (or buffer-file-name buffer-offer-save)
	       (not no-change)
	       (not virtual))
      ;; NO-EXPUNGE covers this save too: `vm-save-folder' expunges on
      ;; `vm-expunge-before-save', which would undo the decision made above.
      (let ((vm-expunge-before-save (and (not no-expunge)
					 vm-expunge-before-save)))
	(vm-save-folder)))

    ;; 5. Handle virtual folders
    ;;    If this is a virtual folder with component folders, quit the
    ;;    component folders.
    ;;    If there are virtual folders dependent on this one, clear away
    ;;    their virtual copies.
    (vm-virtual-quit no-expunge no-change)

    ;; 6. Kill the folder along with its buffers and processes.
    ;;    What it is doing without waiting stops first: the buffer is about to
    ;;    go, and a session that went on writing into it would be writing into
    ;;    nothing.  Nothing is lost that is not still on the server.
    (vm-imap-net-stop)
    (vm-pop-net-stop)
    (message "")			; why this?  USR, 2010-05-03

    (let ((summary-buffer vm-summary-buffer)
	  (pres-buffer vm-presentation-buffer-handle)
	  (mail-buffer (current-buffer)))
      (if summary-buffer
	  (progn
	    (vm-display summary-buffer nil nil nil)
	    (kill-buffer summary-buffer)))
      (if pres-buffer
	  (progn
	    (vm-display pres-buffer nil nil nil)
	    (kill-buffer pres-buffer)))
      (set-buffer mail-buffer)
      (vm-display mail-buffer nil nil nil)
      ;; vm-display is not supposed to change the current buffer.
      ;; still it's better to be safe here.
      (set-buffer mail-buffer)
      (vm-delete-auto-save-file-if-necessary)
      ;; this is a hack to suppress another confirmation dialogue
      ;; coming from kill-buffer
      (set-buffer-modified-p nil)	; folder buffer
      (kill-buffer (current-buffer)))

    ;; 7. Put the folder the reader is left in back on display.
    (vm-display-folder-left-after-quitting)
    (vm-update-summary-and-mode-line)))

(defun vm-start-itimers-if-needed ()
  "Start the timers for whichever of the three intervals is a number.
Named for XEmacs's itimer package, which is what VM used before Emacs had
timers of its own and is where the -itimer-function names come from."
  (require 'timer)
  (let (timer)
    (when (and (natnump vm-flush-interval)
	       (not (vm-timer-using 'vm-flush-itimer-function))
	       (setq timer
		     ;; time restart-time function args
		     (run-at-time vm-flush-interval vm-flush-interval
				  'vm-flush-itimer-function nil)))
      (timer-set-function timer 'vm-flush-itimer-function
			  (list timer)))
    (when (and (natnump vm-mail-check-interval)
	       (not (vm-timer-using 'vm-check-mail-itimer-function))
	       (setq timer
		     (run-at-time vm-mail-check-interval
				  vm-mail-check-interval
				  'vm-check-mail-itimer-function nil)))
      (timer-set-function timer 'vm-check-mail-itimer-function
			  (list timer)))
    (when (and (natnump vm-auto-get-new-mail)
	       (not (vm-timer-using 'vm-get-mail-itimer-function))
	       (setq timer
		     (run-at-time vm-auto-get-new-mail
				  vm-auto-get-new-mail
				  'vm-get-mail-itimer-function nil)))
      (timer-set-function timer 'vm-get-mail-itimer-function
			  (list timer)))))

(defvar timer-list)
(defun vm-timer-using (fun)
  (let ((p timer-list)
	(done nil))
    (while (and p (not done))
      (if (eq (aref (car p) 5) fun)
	  (setq done t)
	(setq p (cdr p))))
    p ))

(defun vm-mail-waiting-can-stop-waiting-p ()
  "Whether mail this folder waits for can stop waiting without VM taking it.
The current buffer is the folder.

Mail in a local spool stays there until something takes it, so asking again
while `vm-spooled-mail-waiting' is set costs a check and cannot change the
answer.  That is the optimisation `vm-mail-check-always' turns off, for the
reader whose spool another client also reads.

On a server it is not an optimisation.  The mail stops being new without VM
doing anything -- read on a phone, moved by a server-side filter, taken by
another Emacs -- and the folder is then waiting for mail that is not there.
Only a retrieval cleared the flag, so the mode line said Mail for ever
(emacs-vm/vm#839)."
  (memq vm-folder-access-method '(imap pop)))

;; support for vm-mail-check-interval
(defun vm-check-mail-itimer-function (timer)
  ;; FSF Emacs sets this non-nil, which means the user can't
  ;; interrupt the check.  Bogus.
  (setq inhibit-quit nil)
  (if (integerp vm-mail-check-interval)
      (timer-set-time
       timer
       (timer-relative-time (current-time) vm-mail-check-interval)
       vm-mail-check-interval)
    ;; user has changed the variable value to something that
    ;; isn't a number, make the timer go away.
    (cancel-timer timer))
  (let ((b-list (buffer-list))
	(found-one nil)
	oldval)
    (save-excursion
      (while (and (not (input-pending-p)) b-list)
	(when (buffer-live-p (car b-list))
	  (set-buffer (car b-list))
	  (when (and (eq major-mode 'vm-mode)
		     (setq found-one t)
		     (or (not vm-spooled-mail-waiting)
			 vm-mail-check-always
			 (vm-mail-waiting-can-stop-waiting-p))
		     ;; to avoid reentrance into the pop and imap code
		     (not vm-global-block-new-mail))
	    (setq oldval vm-spooled-mail-waiting)
	    (setq vm-spooled-mail-waiting (vm-check-for-spooled-mail nil t))
	    (unless (eq oldval vm-spooled-mail-waiting)
	      (intern (buffer-name) vm-buffers-needing-display-update)
	      (run-hooks 'vm-spooled-mail-waiting-hook))))
	(setq b-list (cdr b-list))))
    (vm-update-summary-and-mode-line)
    ;; make the timer go away if we didn't encounter a vm-mode buffer.
    (when (and (not found-one) (null b-list))
      (cancel-timer timer))))

;; support for numeric vm-auto-get-new-mail
(defun vm-get-mail-itimer-function (timer)
  ;; FSF Emacs sets this non-nil, which means the user can't
  ;; interrupt mail retrieval.  Bogus.
  (setq inhibit-quit nil)
  (if (integerp vm-auto-get-new-mail)
      (timer-set-time
       timer
       (timer-relative-time (current-time) vm-auto-get-new-mail)
       vm-auto-get-new-mail)
    ;; user has changed the variable value to something that
    ;; isn't a number, make the timer go away.
    (cancel-timer timer))
  (let ((b-list (buffer-list))
	(found-one nil))
    (while (and (not (input-pending-p)) b-list)
      (save-excursion
	(when (buffer-live-p (car b-list))
	  (set-buffer (car b-list))
	  (when (and (eq major-mode 'vm-mode)
		     (setq found-one t)
		     (not vm-global-block-new-mail)
		     (not vm-block-new-mail)
		     (not vm-folder-read-only)
		     (not (and (not (buffer-modified-p))
			       buffer-file-name
			       (file-newer-than-file-p
				(make-auto-save-file-name)
				buffer-file-name)))
		     (vm-get-spooled-mail nil))
	    ;; don't move the message pointer unless the folder
	    ;; was empty.
	    (if (and (null vm-message-pointer)
		     (vm-thoughtfully-select-message))
		(vm-present-current-message)
	      (vm-update-summary-and-mode-line)))))
      (setq b-list (cdr b-list)))
    ;; make the timer go away if we didn't encounter a vm-mode buffer.
    (when (and (not found-one) (null b-list))
      (cancel-timer timer))))

;; support for numeric vm-flush-interval
;; if timer argument is present, this means we're using the Emacs
;; 'timer package rather than the 'itimer package.
(defun vm-flush-itimer-function (timer)
  (when (integerp vm-flush-interval)
    (timer-set-time
     timer
     (timer-relative-time (current-time) vm-flush-interval)
     vm-flush-interval))
  ;; if no vm-mode buffers are found, we might as well shut down the
  ;; flush timer.
  (unless (vm-flush-cached-data-all-folders)
    (cancel-timer timer)))

;; flush cached data in all vm-mode buffers.
;; returns non-nil if any vm-mode buffers were found.
(defun vm-flush-cached-data-all-folders ()
  "Put the cached data for all folders into the X-VM-v5-data headers.
This function is only used in background tasks.  USR 2012-12-22."
  (save-excursion
    (let ((buf-list (buffer-list))
	  (found-one nil))
      (while (and buf-list (not (input-pending-p)))
	(if (not (buffer-live-p (car buf-list)))
	    nil
	  (set-buffer (car buf-list))
	  (cond ((and (eq major-mode 'vm-mode) vm-message-list)
		 (setq found-one t)
		 (if (not (eq vm-modification-counter
			      vm-flushed-modification-counter))
		     (progn
		       (vm-stuff-last-modified)
		       (vm-stuff-pop-retrieved)
		       (vm-stuff-imap-retrieved)
		       (vm-stuff-imap-to-expunge)
		       (vm-stuff-summary)
		       (vm-stuff-labels)
		       (and vm-message-order-changed
			    (vm-stuff-message-order))
		       (and (vm-stuff-folder-data
			     :interactive nil
			     :abort-if-input-pending t)
			    (setq vm-flushed-modification-counter
				  vm-modification-counter)))))))
	(setq buf-list (cdr buf-list)))
      ;; if we haven't checked them all return non-nil so
      ;; the flusher won't give up trying.
      (or buf-list found-one) )))

;; This allows C-x C-s to do the right thing for VM mail buffers.
;; Note that deleted messages are not expunged.
(defun vm-write-file-hook ()
  (if (and (eq major-mode 'vm-mode) (not vm-inhibit-write-file-hook))
    ;; The save-restriction isn't really necessary here, since
    ;; the stuff routines clean up after themselves, but should remain
    ;; as a safeguard against the time when other stuff is added here.
    (save-restriction
     (let ((buffer-read-only))
       (vm-discard-fetched-messages)
       (vm-inform 7 "%s: Stuffing cached data..." (buffer-name))
       (vm-with-timing 8 "stuffing the messages that changed"
	 (vm-stuff-folder-data :interactive t :abort-if-input-pending nil))
       (vm-inform 7 "%s: Stuffing cached data... done" (buffer-name))
       (when vm-message-list
	 ;; get summary cache up-to-date
	 (vm-inform 8 "%s: Stuffing folder data..." (buffer-name))
	 (vm-with-timing 8 "updating the summary"
	   (vm-update-summary-and-mode-line))
	 (vm-stuff-bookmark)
	 (vm-stuff-pop-retrieved)
	 (vm-with-timing 8 "stuffing the IMAP retrieved list"
	   (vm-stuff-imap-retrieved))
	 (vm-stuff-imap-to-expunge)
	 (vm-stuff-last-modified)
	 (vm-stuff-header-variables)
	 (vm-stuff-labels)
	 (vm-stuff-summary)
	 (when vm-message-order-changed
	   (vm-with-timing 8 "stuffing the message order"
	     (vm-stuff-message-order)))
	 (vm-inform 8 "%s: Stuffing folder data... done" (buffer-name)))
       nil ))))

;;;###autoload
(defun vm-save-buffer (prefix)
  ;; This function hasn't been documented.  Not clear why it is
  ;; different from vm-save-folder.  USR, 2011-04-27
  (interactive "P")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-virtual-folder)
  ;; FIXME Why not basic-save-buffer?
  (save-buffer prefix)
  (intern (buffer-name) vm-buffers-needing-display-update)
  (setq vm-block-new-mail nil)
  (vm-display nil nil '(vm-save-buffer) '(vm-save-buffer))
  (vm-update-summary-and-mode-line)
  (vm-write-index-file-maybe))

;;;###autoload
(defun vm-write-file ()
  "Write this folder to a file of another name, as `write-file\' does.

Three things `write-file\' does not do.  The file is created with
`vm-default-folder-permission-bits\', so a folder does not become
world-readable through being written somewhere new.  The message totals
are stored against the new name for the folders summary, so it does not
have to open the folder to know them.  And the summary and presentation
buffers are renamed to follow the folder.

Refuses on a virtual folder, which has no file of its own."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-virtual-folder)
  (vm-write-file-to nil))

(defun vm-write-file-to (file)
  "Write this folder to FILE, or to a name asked for when FILE is nil.
What `vm-write-file\' does once it has a name, so that a caller which has
worked one out -- `vm-change-folder-type\', naming a folder for the type it
now holds -- gets the same treatment as a reader who typed one."
  (let ((old-buffer-name (buffer-name))
	(oldmodebits (and (fboundp 'default-file-modes)
			  (default-file-modes))))
    (unwind-protect
	(save-excursion
	  (and oldmodebits (set-default-file-modes
			    vm-default-folder-permission-bits))
	  (if file
	      (write-file file)
	    (call-interactively 'write-file)))
      (and oldmodebits (set-default-file-modes oldmodebits)))
    (if (not (equal (buffer-name) old-buffer-name))
	(progn
	  (vm-check-for-killed-summary)
	  (if vm-summary-buffer
	      (save-excursion
		(let ((name (buffer-name)))
		  (set-buffer vm-summary-buffer)
		  (rename-buffer (format "%s Summary" name) t))))
	  (vm-check-for-killed-presentation)
	  (if vm-presentation-buffer-handle
	      (save-excursion
		(let ((name (buffer-name)))
		  (set-buffer vm-presentation-buffer-handle)
		  (rename-buffer (format "%s Presentation" name) t)))))))
  (intern (buffer-name) vm-buffers-needing-display-update)
  (setq vm-block-new-mail nil)
  (vm-display nil nil '(vm-write-file) '(vm-write-file))
  (vm-update-summary-and-mode-line)
  (vm-write-index-file-maybe))

(defun vm-unblock-new-mail ()
  (setq vm-block-new-mail nil))

;;;###autoload
(defun vm-save-folder-no-expunge (&optional prefix)
  "Save current folder to disk.
Prefix arg is handled the same as for the command `save-buffer'.  

Deleted messages are _not_ expunged irrespective of the variable
`vm-expunge-before-save'.

When applied to a virtual folder, this command runs itself on
each of the underlying real folders associated with the virtual
folder."
  (interactive (list current-prefix-arg))
  (let ((vm-expunge-before-save nil))
    (vm-save-folder prefix)))


;;;###autoload
(defun vm-save-folder (&optional prefix)
  "Save current folder to disk.
Prefix arg is handled the same as for the command `save-buffer'.

If the customization variable `vm-expunge-before-save' is set to
non-nil value then deleted messages are expunged.

When applied to a virtual folder, this command runs itself on
each of the underlying real folders associated with the virtual
folder."
  (interactive (list current-prefix-arg))
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-display nil nil '(vm-save-folder) '(vm-save-folder))
  (if (eq major-mode 'vm-virtual-mode)
      (vm-virtual-save-folder prefix)
    ;; Pending server deletions count as work to do even when the buffer
    ;; itself is unchanged: they may have been read back from the folder,
    ;; where a previous session left them because it could not reach the
    ;; server (issue #556).  Without this a visit that changes nothing saves
    ;; nothing, and the deletions wait for a session that happens to modify
    ;; something.
    (if (or (buffer-modified-p)
	    (and (eq vm-folder-access-method 'imap)
		 vm-imap-messages-to-expunge)
	    (and (eq vm-folder-access-method 'pop)
		 vm-pop-messages-to-expunge))
	(let ((buffer-undo-list t)) ;; (mp nil) (newlist nil)
	  (when vm-expunge-before-save
	    (vm-expunge-folder))
	  ;; What the save owes the server goes without waiting: the flags
	  ;; that changed and the deletions asked for.  What the server has
	  ;; expunged is deliberately not worked out here -- that means the
	  ;; flags of every message in the mailbox, nineteen seconds on a
	  ;; folder of six thousand, and the next fetch and
	  ;; `vm-imap-synchronize' work it out anyway.
	  (cond ((eq vm-folder-access-method 'pop)
		 (vm-pop-net-send-changes))
		((eq vm-folder-access-method 'imap)
		 (vm-imap-net-send-changes)))
	  (vm-discard-fetched-messages)
          ;; remove the message summary file of Thunderbird and force
	  ;; it to rebuild it.  Expect error if Thunderbird is active.
          (let ((msf (concat buffer-file-name ".msf")))
            (if (and (eq vm-sync-thunderbird-status t)
		     (file-exists-p msf))
                (delete-file msf)))
	  ;; stuff the attributes of messages that need it.
	  (vm-inform 7 "%s: Stuffing cached data..." (buffer-name))
	  (vm-stuff-folder-data :interactive t 
				:abort-if-input-pending nil)
	  (vm-inform 7 "%s: Stuffing cached data... done" (buffer-name))
	  ;; stuff bookmark and header variable values
	  (when vm-message-list
	    ;; get summary cache up-to-date
	    (vm-inform 7 "%s: Stuffing folder data..." (buffer-name))
	    (vm-update-summary-and-mode-line)
	    (vm-stuff-bookmark)
	    (vm-stuff-pop-retrieved)
	    (vm-stuff-imap-retrieved)
	    (vm-stuff-imap-to-expunge)
	    (vm-stuff-last-modified)
	    (vm-stuff-header-variables)
	    (vm-stuff-labels)
	    (vm-stuff-summary)
	    (and vm-message-order-changed
		 (vm-stuff-message-order))
	    (vm-inform 7 "%s: Stuffing folder data... done" (buffer-name)))
	  (vm-inform 5 "%s: Saving folder..." (buffer-name))
	  (let ((vm-inhibit-write-file-hook t)
		(oldmodebits (and (fboundp 'default-file-modes)
				  (default-file-modes))))
	    (unwind-protect
		(progn
		  (and oldmodebits (set-default-file-modes
				    vm-default-folder-permission-bits))
		  ;; FIXME Why not basic-save-buffer?
		  (save-buffer prefix))
	      (and oldmodebits (set-default-file-modes oldmodebits))))
	  (vm-unmark-folder-modified-p (current-buffer)) ; folder buffer
	  ;; clear the modified flag in virtual folders if all the
	  ;; real buffers associated with them are unmodified.
	  (let ((b-list vm-virtual-buffers) rb-list one-modified)
	    (save-excursion
	      (while b-list
		(if (null (cdr (with-current-buffer (car b-list)
				 vm-real-buffers)))
		    (vm-unmark-folder-modified-p (car b-list))
		  (set-buffer (car b-list))
		  (setq rb-list vm-real-buffers one-modified nil)
		  (while rb-list
		    (if (buffer-modified-p (car rb-list))
			(setq one-modified t rb-list nil)
		      (setq rb-list (cdr rb-list))))
		  (if (not one-modified)
		      (vm-unmark-folder-modified-p (car b-list))))
		(setq b-list (cdr b-list)))))
	  (vm-clear-modification-flag-undos)
	  (setq vm-messages-not-on-disk 0)
	  (setq vm-block-new-mail nil)
	  (vm-write-index-file-maybe)
	  (vm-update-summary-and-mode-line)
	  (and (zerop (buffer-size))
	       vm-delete-empty-folders
	       buffer-file-name
	       (or (eq vm-delete-empty-folders t)
		   (y-or-n-p (format "%s is empty, remove it? "
				     (or buffer-file-name (buffer-name)))))
	       (condition-case ()
		   (progn
		     (delete-file buffer-file-name)
		     (vm-delete-index-file)
		     (clear-visited-file-modtime)
		     (vm-inform 5 "%s removed" buffer-file-name))
		 ;; no can do, oh well.
		 (error nil)))
	  )
      (vm-inform 5 "%s: No changes need to be saved" (buffer-name)))))

;;;###autoload
(defun vm-save-and-expunge-folder (&optional prefix)
  "Expunge folder, then save it to disk.
Prefix arg is handled the same as for the command `save-buffer'.
Expunge won't be done if folder is read-only.

When applied to a virtual folder, this command works as if you had
run `vm-expunge-folder' followed by `vm-save-folder'."
  (interactive (list current-prefix-arg))
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-display nil nil '(vm-save-and-expunge-folder)
	      '(vm-save-and-expunge-folder))
  (if (not vm-folder-read-only)
      (progn
	(vm-inform 6 "%s: Expunging..." (buffer-name))
	(vm-expunge-folder :quiet t)))
  (vm-save-folder prefix))

(defvar inhibit-local-variables) ;; FIXME: Unknown var.  XEmacs, maybe?

;;;###autoload
(defun vm-rename-folder-buffer (buffer folder-name)
  "Give BUFFER the name FOLDER-NAME, and answer with it.  Nil stays nil.
A folder VM is asked for by name is shown under that name even where the
buffer for its file was made by something else -- desktop.el restoring the
session, `recover-file', or a plain `find-file' -- since that buffer is named
after the file.  For an IMAP or POP folder the file is the local cache,
imap-cache-<md5>, which says nothing about which mailbox it holds.

`rename-buffer' uniquifies, so a name already taken gets a suffix rather than
an error."
  (when buffer
    (when (and folder-name (not (equal (buffer-name buffer) folder-name)))
      (with-current-buffer buffer
	(rename-buffer folder-name t)))
    buffer))

(defun vm-read-folder (folder &optional remote-spec folder-name)
  "Reads the FOLDER from the file system and creates a buffer.
Returns the buffer created.
Optional argument REMOTE-SPEC gives the maildrop specification for
the server folder that the FOLDER might be caching.
Optional argument FOLDER-NAME gives the name of the folder that should
be used as the name of the buffer."
  ;; Security issue:
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (let ((file (or folder (expand-file-name vm-primary-inbox
					   vm-folder-directory))))
    (if (file-directory-p file)
	;; MH code perhaps... ?
	(error "%s is a directory" file)
      (or (vm-rename-folder-buffer (vm-get-file-buffer file) folder-name)
	  (let ((default-directory
		  (or (and vm-folder-directory
			   (expand-file-name vm-folder-directory))
		      default-directory))
		(inhibit-local-variables t)
		(enable-local-variables nil)
		(enable-local-eval nil)
		;; for Emacs/MULE
		;; disabled because Emacs 23 doesn't like it, and it
		;; is not clear if it does anything at all.  USR, 2010-07-10.
		;; The only place this function is called from is vm,
		;; which takes care of multibyte issues.  TX, 2010-07-03

		;; for XEmacs/Mule
		(coding-system-for-read
		 (vm-line-ending-coding-system)))
	    (vm-inform 5 "%s: Reading folder..." (or folder-name file))
	    (let ((buffer (find-file-noselect file t))
		  (hist-item (or remote-spec folder vm-primary-inbox)))
	      (when folder-name
		(with-current-buffer buffer
		  (rename-buffer folder-name t)))
	      ;; update folder history
	      (if (not (equal hist-item (car vm-folder-history)))
		    (setq vm-folder-history
			  (cons hist-item vm-folder-history)))
	      (vm-inform 5 "%s: Reading folder... done" (or folder-name file))
	      (vm-inform 8 "%s: read %d characters" (or folder-name file)
			 (- (point-max) (point-min)))
	      buffer))))))

;;;###autoload
(defun vm-revert-buffer ()
"Revert the current folder to its version on the disk.
The summary and presentation buffers are killed, the file is read again,
and the folder is visited afresh with the access method it had, so an
IMAP or POP folder comes back connected rather than as a plain file.

Also available as `vm-revert-folder'."
  (interactive)
  (vm-select-folder-buffer-if-possible)
  (let ((access-method vm-folder-access-method) ; preserve these across
	(access-data vm-folder-access-data)	; the revert-buffer opn
	(summary-buffer vm-summary-buffer)
	(pres-buffer vm-presentation-buffer-handle))
    (if summary-buffer
	(progn
	  (vm-display summary-buffer nil nil nil)
	  (kill-buffer summary-buffer)))
    (if pres-buffer
	(progn
	  (vm-display pres-buffer nil nil nil)
	  (kill-buffer pres-buffer)))
    (call-interactively 'revert-buffer)
    (setq vm-folder-access-data access-data) ; restore preserved data
    (setq vm-folder-access-method access-method)
    (vm (current-buffer) :access-method access-method :reload 'reload)))

;;;###autoload (autoload 'vm-revert-folder "vm-folder" nil t)
(defalias 'vm-revert-folder 'vm-revert-buffer)

(defun vm-recover-folder-file-name ()
  "Read the name of the folder whose auto-save file is to be recovered.
Defaults to the current folder, which is what one almost always wants and for
a server folder is the only practical answer: its file is a cache named after
the MD5 of the maildrop specification, so nobody can be expected to type
imap-cache-d0c3b3a91bbebdf09dd2f78ab0f4c4cc from memory (issue #547).

An IMAP folder may also be named as ACCOUNT:MAILBOX -- the form the mode line
shows and `vm-visit-imap-folder' takes -- and its cache file is then worked out
from `vm-imap-account-alist'."
  (let* ((default (and buffer-file-name
		       (memq major-mode '(vm-mode vm-virtual-mode))
		       buffer-file-name))
	 (answer (read-file-name
		  (if default
		      (format "Recover folder (default %s): "
			      (file-name-nondirectory default))
		    "Recover folder: ")
		  nil default)))
    (or (and (not (file-exists-p answer))
	     ;; Not a file, so perhaps ACCOUNT:MAILBOX.  read-file-name has
	     ;; expanded it against the current directory by now.
	     (vm-imap-cache-file-for-folder-name
	      (file-name-nondirectory answer)))
	answer)))

;;;###autoload
(defun vm-recover-file ()
"Recover the autosave file for the current folder.
Same as \\[vm-recover-folder]."
  (interactive)
  (vm-select-folder-buffer-if-possible)
  (let ((access-method vm-folder-access-method) ; preserve these across
	(access-data vm-folder-access-data)	; the recover-file opn.
	(summary-buffer vm-summary-buffer)
	(pres-buffer vm-presentation-buffer-handle))
    (if summary-buffer
	(progn
	  (vm-display summary-buffer nil nil nil)
	  (kill-buffer summary-buffer)))
    (if pres-buffer
	(progn
	  (vm-display pres-buffer nil nil nil)
	  (kill-buffer pres-buffer)))
    (recover-file (vm-recover-folder-file-name))
    (setq vm-folder-access-method access-method)
    (setq vm-folder-access-data access-data) ; restore data
    (vm (current-buffer) :access-method access-method :reload 'reload)))

;;;###autoload (autoload 'vm-recover-folder "vm-folder" nil t)
(defalias 'vm-recover-folder 'vm-recover-file)

;; It doesn't seem that any of these recover/reversion handlers are
;; working any more.  Not on GNU Emacs.  USR, 2010-01-23

(defun vm-handle-file-recovery-or-reversion (recovery)
  (if (buffer-live-p vm-summary-buffer)
      (kill-buffer vm-summary-buffer))
  (vm-virtual-quit)
  ;; reset major mode, this will cause vm to start from scratch.
  (setq major-mode 'fundamental-mode)
  ;; If this is a recovery, we can't allow the user to get new
  ;; mail until a real save is performed.  Until then the buffer
  ;; and the disk don't match.
  (if recovery
      (setq vm-block-new-mail t))
  (let ((name (cond ((eq vm-folder-access-method 'pop)
		     (vm-pop-find-name-for-buffer (current-buffer)))
		    ((eq vm-folder-access-method 'imap)
		     (vm-imap-find-spec-for-buffer (current-buffer))))))
    (vm (or name buffer-file-name) :access-method vm-folder-access-method)))

;; detect if a recover-file is being performed
;; and handle things properly.
(defun vm-handle-file-recovery ()
  (if (and (buffer-modified-p)
	   (eq major-mode 'vm-mode)
	   (or (null vm-message-list)
	       (= (vm-end-of (car vm-message-list)) 1)))
      (vm-handle-file-recovery-or-reversion t)))

;; detect if a revert-buffer is being performed
;; and handle things properly.
(defun vm-handle-file-reversion ()
  (if (and (not (buffer-modified-p))
	   (eq major-mode 'vm-mode)
	   (or (null vm-message-list)
	       (= (vm-end-of (car vm-message-list)) 1)))
      (vm-handle-file-recovery-or-reversion nil)))

;; FSF v19.23 revert-buffer doesn't mash all the markers together
;; like v18 and prior v19 versions, so the check in
;; vm-handle-file-reversion doesn't work.  However v19.23 has a
;; hook we can use, after-revert-hook.
(defun vm-after-revert-buffer-hook ()
  (if (eq major-mode 'vm-mode)
      (vm-handle-file-recovery-or-reversion nil)))

;;;###autoload
(defun vm-help ()
  "Display help for various VM activities."
  (interactive)
  (if (eq major-mode 'vm-summary-mode)
      (vm-select-folder-buffer-and-validate 0 (vm-interactive-p)))
  (let ((pop-up-windows (and pop-up-windows 
			     (eq vm-mutable-window-configuration t)))
	(pop-up-frames (and vm-mutable-frame-configuration vm-frame-per-help)))
    (cond
     ((eq last-command 'vm-help)
      (describe-function major-mode))
     ((eq vm-system-state 'previewing)
      (vm-inform 0 "Type SPC to read message, n previews next message   (? gives more help)"))
     ((memq vm-system-state '(showing reading))
      (vm-inform 0 "SPC and b scroll, (d)elete, (s)ave, (n)ext, (r)eply   (? gives more help)"))
     ((eq vm-system-state 'editing)
      (vm-inform 0
       (substitute-command-keys
	"Type \\[vm-edit-message-end] to end edit, \\[vm-edit-message-abort] to abort with no change.")))
     ((eq major-mode 'mail-mode)
      (vm-inform 0
       (substitute-command-keys
	"Type \\[vm-mail-send-and-exit] to send message, \\[kill-buffer] to discard this composition")))
     (t (describe-mode)))))

(defun vm-movemail-program-name ()
  "Return the movemail program VM should run.
`vm-movemail-program' when the user has set it, and otherwise the movemail
Emacs came with, in `exec-directory'.

Resolved here rather than as the default of `vm-movemail-program' because
`exec-directory' names an Emacs version and a build architecture; baked into
a defcustom it would be compiled into vm-vars.elc and go stale at the next
Emacs upgrade.

The reason not to leave it to `exec-path' -- which is what plain \"movemail\"
would do, and which is how VM used to spell this -- is that `exec-path' finds
`/usr/bin/movemail' first, and on Debian and Ubuntu that is GNU Mailutils'
rather than Emacs's.  Emacs's copies the spool byte for byte; Mailutils'
rewrites it, and on a message with an empty body merges it with the next one.
See `vm-movemail-program' and issue #538.

If this Emacs has no movemail, signal an error rather than looking along
`exec-path' for one.  Falling back would mean reaching for a program on the
strength of its name alone, to do the one job where getting a different
implementation than the expected one damages mail -- so this asks instead."
  (or vm-movemail-program
      (let ((own (expand-file-name "movemail" exec-directory)))
	(if (file-executable-p own)
	    own
	  (error
	   (concat
	    "This Emacs has no movemail of its own, so VM will not fetch"
	    " local mail until you say which movemail to use."
	    "  Either install Emacs's -- it is left out when Emacs is built"
	    " --with-mailutils, which is its default if GNU Mailutils is"
	    " present at build time -- or set vm-movemail-program to one,"
	    " for instance (setq vm-movemail-program \"/usr/bin/movemail\")."
	    "  Be aware that GNU Mailutils' movemail rewrites mailboxes"
	    " rather than copying them, and merges a message whose body is"
	    " empty with the message after it: see VM issue #538."
	    "  Looked for Emacs's in %s")
	   exec-directory)))))

;;;###autoload
(defun vm-spool-move-mail (source destination)
  (let ((handler (and (fboundp 'find-file-name-handler)
		      (find-file-name-handler source 'vm-spool-move-mail)))
	(movemail (vm-movemail-program-name))
	status error-buffer)
    (if handler
	(funcall handler 'vm-spool-move-mail source destination)
      (setq error-buffer
	    (get-buffer-create
	     (format "*output of %s %s %s*"
		     movemail source destination)))
      (with-current-buffer error-buffer
	(erase-buffer))
      (setq status
	    (apply 'call-process
		   (nconc
		    (list movemail nil error-buffer t)
		    (copy-sequence vm-movemail-program-switches)
		    (list source destination))))
      (save-current-buffer
	(set-buffer error-buffer)
	(if (and (numberp status) (not (= 0 status)))
	    (insert (format "\n%s exited with code %s\n"
			    movemail status)))
	(if (> (buffer-size) 0)
	    (progn
	      (vm-display-buffer error-buffer)
	      (if (and (numberp status) (not (= 0 status)))
		  (error "Failed getting new mail from %s" source)
		(vm-warn 1 2 "Warning: unexpected output from %s"
			 movemail)))
	  ;; nag, nag, nag.
	  (kill-buffer error-buffer))
	t ))))

(defun vm-gobble-crash-box (crash-box)
  (save-excursion
    (save-restriction
     (widen)
     (let ((opoint-max (point-max)) crash-buf
	   (buffer-read-only nil)
	   (inbox-buffer-file buffer-file-name)
	   (inbox-folder-type vm-folder-type)
	   (inbox-empty (zerop (buffer-size)))
	   got-mail crash-folder-type
	   (old-buffer-modified-p (buffer-modified-p)))
       (setq crash-buf
	     ;; crash box could contain a letter bomb...
	     ;; force user notification of file variables for v18 Emacses
	     ;; enable-local-variables == nil disables them for newer Emacses
	     (let ((inhibit-local-variables t)
		   (enable-local-variables nil)
		   (enable-local-eval nil)
		   (coding-system-for-read (vm-line-ending-coding-system)))
	       (find-file-noselect crash-box)))
       (if (eq (current-buffer) crash-buf)
	   (error "folder is the same file as crash box, cannot continue"))
       (with-current-buffer crash-buf
	 (setq crash-folder-type (vm-get-folder-type))
	 (if (and crash-folder-type vm-check-folder-types)
	     (cond ((eq crash-folder-type 'unknown)
		    (error "crash box %s's type is unrecognized" crash-box))
		   ((eq inbox-folder-type 'unknown)
		    (error "inbox %s's type is unrecognized"
			   inbox-buffer-file))
		   ((null inbox-folder-type)
		    (if vm-default-folder-type
			(if (not (eq vm-default-folder-type
				     crash-folder-type))
			    (if vm-convert-folder-types
				(progn
				  (vm-convert-folder-type
				   crash-folder-type
				   vm-default-folder-type)
				  ;; so that kill-buffer won't ask a
				  ;; question later...
				  (set-buffer-modified-p nil)) ; crash-buf
			      (error "crash box %s mismatches vm-default-folder-type: %s, %s"
				     crash-box crash-folder-type
				     vm-default-folder-type)))))
		   ((not (eq inbox-folder-type crash-folder-type))
		    (if vm-convert-folder-types
			(progn
			  (vm-convert-folder-type crash-folder-type
						  inbox-folder-type)
			  ;; so that kill-buffer won't ask a
			  ;; question later...
			  (set-buffer-modified-p nil)) ; crash-buf
		      (error "crash box %s mismatches %s's folder type: %s, %s"
			     crash-box inbox-buffer-file
			     crash-folder-type inbox-folder-type)))))
	 ;; toss the folder header if the inbox is not empty
	 (goto-char (point-min))
	 (if (not inbox-empty)
	     (vm-convert-folder-header (or inbox-folder-type
					   vm-default-folder-type)
				       nil)
	   (set-buffer-modified-p nil))) ; crash-buf
       (goto-char (point-max))
       (insert-buffer-substring crash-buf
				1 (1+ (with-current-buffer crash-buf
					(widen)
					(buffer-size))))
       (setq got-mail (/= opoint-max (point-max)))
       (if (not got-mail)
	   nil
	 (let ((coding-system-for-write (vm-binary-coding-system))
	       (selective-display nil))
	   (write-region opoint-max (point-max) buffer-file-name t t))
	 (vm-increment vm-modification-counter)
	 (vm-restore-buffer-modified-p	; folder-buffer
	  old-buffer-modified-p (current-buffer)))
       (kill-buffer crash-buf)
       (if (not (stringp vm-keep-crash-boxes))
	   (vm-error-free-call 'delete-file crash-box)
	 (let ((time (decode-time (current-time)))
	       name)
	   (setq name
		 (expand-file-name (format "Z-%02d-%02d-%02d%02d%02d-%05d"
					   (nth 4 time)
					   (nth 3 time)
					   (nth 2 time)
					   (nth 1 time)
					   (nth 0 time)
					   (% (vm-abs (random)) 100000))
				   vm-keep-crash-boxes))
	   (while (file-exists-p name)
	     (setq name
		   (expand-file-name (format "Z-%02d-%02d-%02d%02d%02d-%05d"
					     (nth 4 time)
					     (nth 3 time)
					     (nth 2 time)
					     (nth 1 time)
					     (nth 0 time)
					     (% (vm-abs (random)) 100000))
				     vm-keep-crash-boxes)))
	   (rename-file crash-box name)))
       got-mail ))))

(defun vm-compute-spool-files (&optional all)
  (let ((fallback-triples nil)
	(crash-box (or vm-crash-box
		       (concat vm-primary-inbox vm-crash-box-suffix)))
	file file-list
	triples)
    (cond ((null (vm-spool-files))
	   (setq triples (list
			  (list vm-primary-inbox
				(concat vm-spool-directory (user-login-name))
				crash-box))))
	  ((stringp (car (vm-spool-files)))
	   (setq triples
		 (mapcar (function
			  (lambda (s) (list vm-primary-inbox s crash-box)))
			 (vm-spool-files))))
	  ((consp (car (vm-spool-files)))
	   (setq triples (vm-spool-files))))
    (setq file-list (if all (mapcar 'car triples) (list buffer-file-name)))
    (while file-list
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (cond ((and file
		  (consp vm-spool-file-suffixes)
		  (stringp vm-crash-box-suffix))
	     (setq fallback-triples
		   (mapcar (function
			    (lambda (suffix)
			      (list file
				    (concat file suffix)
				    (concat file
					    vm-crash-box-suffix))))
			   vm-spool-file-suffixes))))
      (cond ((and file
		  vm-make-spool-file-name vm-make-crash-box-name)
	     (setq fallback-triples
		   (nconc fallback-triples
			  (list (list file
				      (save-excursion
					(funcall vm-make-spool-file-name
						 file))
				      (save-excursion
					(funcall vm-make-crash-box-name
						 file)))))))))
    (setq triples (append triples fallback-triples))
    triples ))

(defun vm-spool-check-mail (source)
  (let ((handler (find-file-name-handler source 'vm-spool-check-mail)))
    (if handler
	(funcall handler 'vm-spool-check-mail source)
      (let ((size (nth 7 (file-attributes source)))
	    (hash vm-spool-file-message-count-hash)
	    val)
	(setq val (symbol-value (intern-soft source hash)))
	(if (and val (equal size (car val)))
	    (> (nth 1 val) 0)
	  (let ((count (vm-count-messages-in-file source)))
	    (if (null count)
		nil
	      (set (intern source hash) (list size count))
	      (> count 0))))))))

(defun vm-count-messages-in-file (file &optional quietly)
  (let ((type (vm-get-folder-type file nil nil t))
	(work-buffer nil)
	count)
    (if (or (memq type '(unknown nil)) (null vm-grep-program))
	nil
      (unwind-protect
	  (let (regexp)
	    (save-excursion
	      (setq work-buffer (vm-make-work-buffer))
	      (set-buffer work-buffer)
	      ;; The same separator the folder would be parsed by, so that
	      ;; the count agrees with the messages a reader would see.
	      ;; "^From " alone counts every body line beginning "From ",
	      ;; and a folder written by something that does not quote
	      ;; those has them (emacs-vm/vm#640).
	      (cond ((memq type '(From_ BellFrom_ mboxcl2))
		     (setq regexp vm-leading-message-separator-regexp-From_))
		    ((eq type 'mmdf)
		     (setq regexp "^\001\001\001\001"))
		    ((eq type 'babyl)
		     (setq regexp "^\037")))
	      (condition-case data
		  (progn
		    (unless quietly 
		      (vm-inform 7 "Counting messages in %s..." file))
		    (call-process vm-grep-program nil t nil "-c" regexp
				  (expand-file-name file))
		    (unless quietly 
		      (vm-inform 7 "Counting messages in %s... done" file)))
		(error (vm-warn 1 2 "Attempt to run %s on %s signaled: %s"
				vm-grep-program file data)
		       (setq vm-grep-program nil)))
	      (setq count (string-to-number (buffer-string)))
	      (cond ((memq type '(From_ BellFrom_ mboxcl2))
		     t )
		    ((eq type 'mmdf)
		     (setq count (/ count 2)))
		    ((eq type 'babyl)
		     (setq count (1- count))))
	      count ))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-movemail-specific-spool-file-p (file)
  (string-match "^po:[^:]+$" file))

;; The non-blocking POP layer, which the mail check uses.  Required here
;; rather than declared: the check runs from a timer, and a timer is a poor
;; place to discover that a file has not been loaded.
(require 'vm-pop-net)
(require 'vm-imap-net)

(defvar vm-mail-check-answers nil
  "What the last check of each of this folder's maildrops said.
An alist of maildrop to t or nil.  A check that does not wait cannot answer
in the round that started it, so its answer is kept here and counted by the
rounds after it (emacs-vm/vm#473).")
(make-variable-buffer-local 'vm-mail-check-answers)

(defvar vm-mail-checks-outstanding nil
  "The maildrops of this folder with a check still to answer.
One check at a time for each: the timer fires every
`vm-mail-check-interval' seconds, and a server slower than that would
otherwise be asked again before it had answered the first time.")
(make-variable-buffer-local 'vm-mail-checks-outstanding)

(defun vm-mail-waiting-p (maildrop)
  "What the last check of MAILDROP said, for this folder."
  (cdr (assoc maildrop vm-mail-check-answers)))

(defun vm-note-mail-waiting (buffer maildrop answer)
  "Record in BUFFER what a check of MAILDROP found, and show it.

ANSWER is t, nil, or the error that stopped the check -- an error leaves
the last answer standing rather than reporting no mail, which is what a
folder would show while a server was down.

Called from a process filter, so it takes the buffer it was given: the one
that is current belongs to whoever was typing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq vm-mail-checks-outstanding
	    (delete maildrop vm-mail-checks-outstanding))
      (unless (and answer (not (eq answer t)))
	(setf (alist-get maildrop vm-mail-check-answers nil nil #'equal)
	      answer)
	(let ((waiting (and (rassq t vm-mail-check-answers) t)))
	  (unless (eq waiting vm-spooled-mail-waiting)
	    (setq vm-spooled-mail-waiting waiting)
	    (intern (buffer-name) vm-buffers-needing-display-update)
	    (run-hooks 'vm-spooled-mail-waiting-hook)
	    (vm-update-summary-and-mode-line)))))))

(defun vm-start-mail-check (maildrop)
  "Ask MAILDROP whether it has mail, and carry on without the answer.

Not while this folder is fetching: the check would open a second connection
to a maildrop VM is in the middle of reading, and a POP server holds one
session at a time -- the second is refused, or worse, taken and the first
one's view of the maildrop is stale.  The fetch will say what arrived
anyway, which is what the check was going to ask."
  (unless (or (member maildrop vm-mail-checks-outstanding)
	      (vm-pop-net-busy-p)
	      (vm-imap-net-busy-p))
    (let ((buffer (current-buffer)))
      (setq vm-mail-checks-outstanding
	    (cons maildrop vm-mail-checks-outstanding))
      (condition-case err
	  (funcall (if (vm-imap-folder-spec-p maildrop)
		       #'vm-imap-net-check-mail
		     #'vm-pop-net-check-mail)
		   maildrop
		   (lambda (answer)
		     (vm-note-mail-waiting buffer maildrop answer)))
	(error
	 (setq vm-mail-checks-outstanding
	       (delete maildrop vm-mail-checks-outstanding))
	 (signal (car err) (cdr err)))))))

(defun vm-check-for-spooled-mail (&optional interactive this-buffer-only)
  (if vm-global-block-new-mail
      nil
    (if (and vm-folder-access-method this-buffer-only)
	;; On the driver, which answers whether it asked rather than whether
	;; there is mail: the answer arrives in `vm-spooled-mail-waiting',
	;; which is what the mode line reads.  A check that fails says so at
	;; level 6 and is not repeated at the reader, so the once-per-failure
	;; bookkeeping the blocking check needed has nothing to do here.
	(cond ((eq vm-folder-access-method 'pop)
	       (vm-pop-net-folder-check-mail))
	      ((eq vm-folder-access-method 'imap)
	       (vm-imap-net-folder-check-mail)))
      (let ((triples (vm-compute-spool-files (not this-buffer-only)))
	    ;; since we could accept-process-output here (POP code),
	    ;; a timer process might try to start retrieving mail
	    ;; before we finish.  block these attempts.
	    (vm-global-block-new-mail t)
	    (vm-pop-ok-to-ask interactive)
	    (vm-imap-ok-to-ask interactive)
	    ;; for string-match calls below
	    (case-fold-search nil)
	    this-buffer crash in maildrop meth
	    (mail-waiting nil))
	(while triples
	  (setq in (expand-file-name (nth 0 (car triples)) vm-folder-directory)
		maildrop (nth 1 (car triples))
		crash (nth 2 (car triples)))
	  (if (vm-movemail-specific-spool-file-p maildrop)
	      ;; spool file is accessible only with movemail
	      ;; so skip it.
	      nil
	    (setq this-buffer (eq (current-buffer) (vm-get-file-buffer in)))
	    (when (or this-buffer (not this-buffer-only))
		  (if (file-exists-p crash)
		      (setq mail-waiting t)
		    (setq meth
			  (cond ((vm-imap-folder-spec-p maildrop) 'imap)
				((vm-pop-folder-spec-p maildrop) 'pop)
				(t 'spool)))
		    (cond
		     ;; A spool file is looked at here and now: it is a file,
		     ;; and there is no session to be had with it.
		     ((eq meth 'spool)
		      (setq mail-waiting
			    (or mail-waiting (vm-spool-check-mail maildrop))))
		     ;; A network maildrop is asked without waiting: the check
		     ;; is started here and its answer arrives at
		     ;; `vm-note-mail-waiting'.  What this round contributes is
		     ;; the answer the last one got (emacs-vm/vm#473).
		     ((if (eq meth 'imap)
			  (vm-imap-net-checkable-p maildrop)
			(vm-pop-net-checkable-p maildrop))
		      (vm-start-mail-check maildrop)
		      (setq mail-waiting
			    (or mail-waiting (vm-mail-waiting-p maildrop))))
		     ;; Not checkable: VM has no password for it and nobody can
		     ;; be asked from here.  Nothing is contributed rather than
		     ;; the same question being put a second time by a blocking
		     ;; check; the last answer, if there was one, still stands.
		     (t
		      (setq mail-waiting
			    (or mail-waiting (vm-mail-waiting-p maildrop))))))))
	  (setq triples (cdr triples)))
	mail-waiting ))))

(defun vm-get-spooled-mail (&optional interactive full)
  "Get new mail for the current folder from its spool file.
The optional argument INTERACTIVE says whether the function can make
interactive queries to the user.  The possible values are t,
`password-only', and nil.

FULL asks an IMAP mailbox for every message the folder has not got, rather
than only for those `vm-imap-retrieved-messages' has no record of.  It is
what refills a folder whose cache lost messages the record still names; see
`vm-get-new-mail'.  Other access methods have nothing to be full about and
ignore it."
  (if vm-block-new-mail
      (error "Can't get new mail until you save this folder."))
  (cond ((eq vm-folder-access-method 'pop)
	 (vm-pop-net-get-folder-mail))
	((eq vm-folder-access-method 'imap)
	 (vm-imap-net-get-spooled-mail interactive full))
	(t (vm-get-spooled-mail-normal interactive))))

(defun vm-spooled-mail-arrived (crash safe-maildrop)
  "Take what a session wrote into CRASH into this folder.
The tail of the spool loop, run when the mail lands rather than when the
command was typed: gobble the crash box, take the messages into the message
list, and say where they came from."
  (when (vm-gobble-crash-box crash)
    (setq vm-spooled-mail-waiting nil)
    (intern (buffer-name) vm-buffers-needing-display-update)
    (condition-case errmsg
	(run-hooks 'vm-retrieved-spooled-mail-hook)
      (t (vm-warn 0 2 (concat "Ignoring error while running "
			      "vm-retrieved-spooled-mail-hook. %S")
		  errmsg)))
    (vm-assimilate-new-messages :read-attributes nil)
    ;; and one of them is made current, as `vm-get-new-mail' does after the
    ;; blocking fetch: a folder that was empty has no current message until
    ;; this runs, and every command that works on the current message takes
    ;; `(car vm-message-pointer)' and gets nil
    (if (vm-thoughtfully-select-message)
	(vm-present-current-message)
      (vm-update-summary-and-mode-line))
    (vm-inform 5 "Got mail from %s." safe-maildrop)
    t))

(defun vm-start-spooled-mail (retrieval-function maildrop crash safe-maildrop)
  "Start fetching MAILDROP into CRASH without waiting, if that can be done.
Answers with whether it started.  Nil means nothing was started: either the
maildrop is not one VM fetches over the network, or VM has no password for it
and the reader, who has been asked, gave none.

SAFE-MAILDROP is the name to show; RETRIEVAL-FUNCTION says which protocol it
is, and is `imap', `pop' or `vm-spool-move-mail' -- the first two name no
function, being the two protocols the driver fetches."
  (let ((folder (current-buffer))
	(starter (cond ((eq retrieval-function 'imap) #'vm-imap-net-move-mail)
		       ((eq retrieval-function 'pop) #'vm-pop-net-get-mail))))
    (and starter
	 (condition-case nil
	     (progn
	       (funcall starter maildrop crash
			(lambda (result)
			  (cond
			   ((vm-net-error-p result)
			    (vm-warn 0 2 "%s: %s" safe-maildrop
				     (error-message-string result)))
			   ((and (numberp result) (> result 0))
			    (with-current-buffer folder
			      (vm-spooled-mail-arrived crash safe-maildrop)))
			   (t
			    (vm-inform 5 "No mail from %s." safe-maildrop)))))
	       t)
	   (vm-imap-net-no-password nil)
	   (vm-pop-net-no-password nil)))))

(defun vm-move-spooled-mail (retrieval-function maildrop crash got-mail)
  "Move MAILDROP into CRASH with RETRIEVAL-FUNCTION, and answer whether to read
CRASH.  For a spool file, `movemail' and the rest, which VM fetches by waiting.

GOT-MAIL says whether mail has already been appended to this folder.  Once it
has, an error must not be signalled: the reader would be left looking for mail
that is neither in the crash box, nor in the spool file, nor visibly in the
folder.  So it answers t on an error and on a quit, both of which leave it
unknown whether anything reached the crash box."
  (if got-mail
      (condition-case error-data
	  (funcall retrieval-function maildrop crash)
	(error (vm-warn 0 2 "%s signaled: %s" retrieval-function error-data)
	       t)
	(quit (vm-warn 0 2 "quitting from %s..." retrieval-function)
	      t))
    (funcall retrieval-function maildrop crash)))

(defun vm-get-spooled-mail-normal (&optional interactive)
  (if vm-global-block-new-mail
      nil
    (let ((triples (vm-compute-spool-files))
	  ;; since we could accept-process-output here (POP code),
	  ;; a timer process might try to start retrieving mail
	  ;; before we finish.  block these attempts.
	  (vm-global-block-new-mail t)
	  (vm-pop-ok-to-ask interactive)
	  (vm-imap-ok-to-ask interactive)
	  ;; for string-match calls below
	  (case-fold-search nil)
	  non-file-maildrop crash in safe-maildrop maildrop ;; popdrop
	  retrieval-function
	  (got-mail nil))
      (if (and (not (verify-visited-file-modtime (current-buffer)))
	       (or (null interactive)
		   (not (yes-or-no-p
			 (format
			  "Folder %s changed on disk, discard those changes? "
			  (buffer-name))))))
	  (progn
	    (vm-warn 0 2 
		     "Folder %s changed on disk, consider M-x revert-buffer"
		     (buffer-name))
	    nil )
	(while triples
	  (setq in (expand-file-name (nth 0 (car triples)) vm-folder-directory))
	  (setq maildrop (nth 1 (car triples)))
	  (setq crash (nth 2 (car triples)))
	  (setq safe-maildrop maildrop)
	  (setq non-file-maildrop nil)
	  (cond ((vm-movemail-specific-spool-file-p maildrop)
		 (setq non-file-maildrop t)
		 (setq retrieval-function 'vm-spool-move-mail))
		((vm-imap-folder-spec-p maildrop)
		 (setq non-file-maildrop t)
		 (setq safe-maildrop 
		       (or (vm-imap-account-name-for-spec maildrop)
			   (vm-safe-imapdrop-string maildrop)))
		 (setq retrieval-function 'imap))
		((vm-pop-folder-spec-p maildrop)
		 (setq non-file-maildrop t)
		 (setq safe-maildrop 
		       (or (vm-pop-find-name-for-spec maildrop)
			   (vm-safe-popdrop-string maildrop)))
		 (setq retrieval-function 'pop))
		(t (setq retrieval-function 'vm-spool-move-mail)))
	  (setq crash (expand-file-name crash vm-folder-directory))
	  (when (eq (current-buffer) (vm-get-file-buffer in))
	    (when (file-exists-p crash)
	      (vm-inform 1 "Recovering messages from %s..." crash)
	      (setq got-mail (or (vm-gobble-crash-box crash) got-mail))
	      (vm-inform 1 "Recovering messages from %s... done" crash))
	    (when (or non-file-maildrop
		      (and (not (equal 0 (nth 7 (file-attributes maildrop))))
			   (file-readable-p maildrop)))
	      (unless non-file-maildrop
		(setq maildrop 
		      (expand-file-name maildrop 
					vm-folder-directory)))
	      (when (cond
		     ((vm-start-spooled-mail retrieval-function maildrop
					     crash safe-maildrop)
		      ;; on its way; the crash box is gobbled when it lands
		      nil)
		     ((memq retrieval-function '(imap pop))
		      ;; The driver did not start, which for a network maildrop
		      ;; means VM has no password for it and the reader has
		      ;; already been asked.  There is nothing else to try: one
		      ;; way in, and asking again through a second
		      ;; implementation would put the same question twice.
		      (vm-inform 5 "No mail from %s, VM has no password for it."
				 safe-maildrop)
		      nil)
		     (t (vm-move-spooled-mail retrieval-function maildrop
					      crash got-mail)))
		(when (vm-gobble-crash-box crash)
		  (setq got-mail t)
		  (vm-inform 5 "Got mail from %s."
			   safe-maildrop)))))
	  (setq triples (cdr triples)))
	;; not really correct, but it is what the user expects to see.
	(setq vm-spooled-mail-waiting nil)
	(intern (buffer-name) vm-buffers-needing-display-update)
	(vm-update-summary-and-mode-line)
	(when got-mail
          (condition-case errmsg
              (run-hooks 'vm-retrieved-spooled-mail-hook)
            (t 
	     (vm-warn 0 2
	      "Ignoring error while running vm-retrieved-spooled-mail-hook. %S"
	      errmsg)))
          (vm-assimilate-new-messages :read-attributes nil))))))

;;;###autoload
(defun vm-folder-name ()
  "Return the current folder's name (local file name, or POP/IMAP
maildrop string)."
  (interactive)
  (if vm-folder-access-method
      (aref vm-folder-access-data 0)
    buffer-file-name))

;; This function is now obsolete.  USR, 2011-12-26
(defun vm-safe-popdrop-string (maildrop)
  "Return a human-readable version of a pop MAILDROP string."
  (or (and (string-match "^\\(pop:\\|pop-ssl:\\|pop-ssh:\\)?\\([^:]*\\):[^:]*:[^:]*:\\([^:]*\\):[^:]*" maildrop)
	   (concat (substring maildrop (match-beginning 3) (match-end 3))
		   "@"
		   (substring maildrop (match-beginning 2) (match-end 2))))
      "???"))

(defun vm-popdrop-sans-password (source)
  "Return popdrop SOURCE, but replace the password by a \"*\"."
  (mapconcat 'identity 
             (append (reverse (cdr (reverse (vm-parse source "\\([^:]*\\):?"))))
                     '("*"))
             ":"))

(defun vm-popdrop-sans-personal-info (source)
  "Return popdrop SOURCE, but replace the login and password by a \"*\"."
  (mapconcat 'identity 
             (append (reverse (cdr (cdr (reverse (vm-parse source "\\([^:]*\\):?")))))
                     '("*" "*"))
             ":"))

;; This function is now obsolete.  USR, 2011-12-26
(defun vm-safe-imapdrop-string (maildrop)
  "Return a human-readable version of an imap MAILDROP string."
  (or (and (string-match "^\\(imap\\|imap-ssl\\|imap-ssh\\):\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*" maildrop)
	   (concat (substring maildrop (match-beginning 4) (match-end 4))
		   "@"
		   (substring maildrop (match-beginning 2) (match-end 2))
		   " ["
		   (substring maildrop (match-beginning 3) (match-end 3))
		   "]"))
      "???"))

(defun vm-imapdrop-sans-password (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]*\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":"
	    (nth 3 source-list) ":"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":" "*")))

(defun vm-imapdrop-sans-password-and-mailbox (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]*\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":" "*:"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":" "*")))

(defun vm-imapdrop-sans-personal-info (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]*\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":" "*:"
	    (nth 4 source-list) ":" "*:" "*")))

(defun vm-maildrop-sans-password (drop)
  (or (and (string-match "^\\(pop:\\|pop-ssl:\\|pop-ssh:\\)?\\([^:]*\\):[^:]*:[^:]*:\\([^:]*\\):[^:]*" drop)
	   (vm-popdrop-sans-password drop))
      (and (string-match "^\\(imap\\|imap-ssl\\|imap-ssh\\):\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*" drop)
	   (vm-imapdrop-sans-password drop))
      drop))

(defun vm-maildrop-sans-personal-info (drop)
  (or (and (string-match "^\\(pop:\\|pop-ssl:\\|pop-ssh:\\)?\\([^:]*\\):[^:]*:[^:]*:\\([^:]*\\):[^:]*" drop)
	   (vm-popdrop-sans-personal-info drop))
      (and (string-match "^\\(imap\\|imap-ssl\\|imap-ssh\\):\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*" drop)
	   (vm-imapdrop-sans-personal-info drop))
      drop))

(defun vm-maildrop-alist-sans-personal-info (alist)
  (vm-mapcar 
   (lambda (pair-xxx)
     (cons (vm-maildrop-sans-personal-info (car pair-xxx)) (cdr pair-xxx)))
   alist))

;;;###autoload
(defun vm-get-new-mail (&optional arg)
  "Move any new mail that has arrived in any of the spool files for the
current folder into the folder.  New mail is appended to the disk
and buffer copies of the folder.

Prefix arg means to gather mail from a user specified folder, instead of
the usual spool files.  The file name will be read from the minibuffer.
Unlike when getting mail from a spool file, the source file is left
undisturbed after its messages have been copied.

Two prefix args (\\[universal-argument] \\[universal-argument]) mean to fetch \
everything an IMAP mailbox
has and this folder has not, including the messages `vm-imap-retrieved-messages'
records as fetched once already.  That record is what stops a message deleted
here on purpose from coming back, so this is not the way to read mail day to
day; it is how to refill a folder whose cache lost messages the record still
names.  It gathers from no other folder, and other access methods ignore it.

When applied to a virtual folder, this command runs itself on
each of the underlying real folders associated with this virtual
folder.  A prefix argument has no effect when this command is
applied to virtual folder; mail is always gathered from the spool
files."
  (interactive "P")
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let* ((folder (buffer-name))
	 (full (equal arg '(16)))
	 (description (if (consp (car (vm-spool-files)))
					; folder-specific spool files
			  (format "new mail for %s" (buffer-name))
			(format "new mail")))
	 totals-blurb)
    (cond ((eq major-mode 'vm-virtual-mode)
	   (vm-virtual-get-new-mail))
	  ((not (eq major-mode 'vm-mode))
	   (error "Can't get mail for a non-VM folder buffer"))
	  ((or (null arg) full)
	   ;; This is redundant now.  USR, 2011-12-26
	   (vm-inform 5 "%s: Checking for %s..." folder description)
	   (let ((got (vm-get-spooled-mail t full)))
	     (cond
	      ;; Under way, and nothing has arrived yet: what the folder holds
	      ;; is what it held before, and the arrival says what came
	      ;; (emacs-vm/vm#825).
	      ((eq got 'started)
	       (vm-inform 5 "%s: getting %s..." folder description))
	      (got
	       ;; say this NOW, before the non-previewers read
	       ;; a message, alter the new message count and
	       ;; confuse themselves.
	       (setq totals-blurb (vm-emit-totals-blurb))
	       (vm-display nil nil '(vm-get-new-mail) '(vm-get-new-mail))
	       (if (vm-thoughtfully-select-message)
		   (vm-present-current-message)
		 (vm-update-summary-and-mode-line))
	       (vm-inform 5 "%s" totals-blurb))
	      (t
	       (vm-inform 5 "%s: No %s" folder description)
	       (and (vm-interactive-p) (vm-sit-for 4) (vm-inform 5 ""))))))
	  (t
	   (let ((buffer-read-only nil)
		 folder mcount)
	     (setq folder (read-file-name "Gather mail from folder: "
					  vm-folder-directory nil t))
	     (if (and vm-check-folder-types
		      (not (vm-compatible-folder-p folder)))
		 (error "Folder %s is not the same format as this folder."
			folder))
	     (save-excursion
	       (save-restriction
		(widen)
		(goto-char (point-max))
		(let ((coding-system-for-read (vm-binary-coding-system)))
		  (insert-file-contents folder))))
	     (setq mcount (length vm-message-list))
	     (if (vm-assimilate-new-messages)
		 (progn
		   ;; say this NOW, before the non-previewers read
		   ;; a message, alter the new message count and
		   ;; confuse themselves.
		   (setq totals-blurb (vm-emit-totals-blurb))
		   (vm-display nil nil '(vm-get-new-mail) '(vm-get-new-mail))
		   (if (vm-thoughtfully-select-message)
		       (vm-present-current-message)
		     (vm-update-summary-and-mode-line))
		   (vm-inform 5 "%s" totals-blurb)
		   ;; The gathered messages are actually still on disk
		   ;; unless the user deletes the folder himself.
		   ;; However, users may not understand what happened if
		   ;; the messages go away after a "quit, no save".
		   (setq vm-messages-not-on-disk
			 (+ vm-messages-not-on-disk
			    (- (length vm-message-list)
			       mcount))))
	       (vm-inform 5 "%s: No messages gathered." folder)))))))

;; returns list of new messages if there were any new messages, nil otherwise
(cl-defun vm-assimilate-new-messages (&key
				    (read-attributes t) (run-hooks t)
				    gobble-order labels)
  ;; We are only guessing what this function does.  USR, 2010-05-20
  ;; This is called in a Folder buffer, which already has messages
  ;; loaded into it, but some of the messages (the "new" messages)
  ;; have not been parsed and separated yet.  
  ;; The function first builds a vm-message-list.
  ;; If READ-ATTRIBUTES is non-nil, it reads the message
  ;; attributes in the X-VM-v5-Data headers and stores them.
  ;; If GOBBLE-ORDER is non-nil, it reads the X-VM-Message-Order
  ;; header and uses it to reorder the messages.
  ;; If vm-summary-show-threads is non-nil, it builds threads.
  ;; If vm-ml-sort-keys is non-nil, sorts the messages accordingly.
  ;; If LABELS is non-nil, they are added to the message labels of all 
  ;; the new messages.
  ;; If RUN-HOOKS is t, arrived-message-hook functions are
  ;; called.  Normally, this argument is nil for the first
  ;; time vm-assimilate-new-messages is called in a folder.  It is
  ;; t for subsequent calls when new mail is being incorporated.
  (let ((tail-cons (vm-last vm-message-list))
	b-list new-messages)
    (save-excursion
      (save-restriction
       (widen)
       (vm-build-message-list)
       (when (or (null tail-cons) (cdr tail-cons))
	 (unless vm-assimilate-new-messages-sorted
	   (setq vm-ml-sort-keys nil))
	 (if read-attributes
	     (vm-read-VM-data (cdr tail-cons))
	   (vm-set-default-attributes (cdr tail-cons)))
	 ;; Yuck.  This has to be done here instead of in the
	 ;; vm function because this needs to be done before
	 ;; any initial thread sort (so that if the thread
	 ;; sort matches the saved order the folder won't be
	 ;; modified) but after the message list is created.
	 ;; Since thread sorting is done here this has to be
	 ;; done here too.
	 (when gobble-order
	   (vm-gobble-message-order))
	 (when (or (vectorp vm-thread-obarray)
		   vm-summary-show-threads)
	   ;; may need threads for sorting
	   (vm-build-threads (cdr tail-cons)))))
      (setq new-messages (if tail-cons (cdr tail-cons) vm-message-list))
      (when new-messages
	(vm-set-numbering-redo-start-point new-messages)
	(vm-set-summary-redo-start-point new-messages)))
    ;; Only update the folders summary count here if new messages
    ;; have arrived, not when we're reading the folder for the
    ;; first time, and not if we cannot assume that all the arrived
    ;; messages should be considered new.  Use gobble-order as a
    ;; first time indicator along with the new messages being equal
    ;; to the whole message list.
    (when new-messages
      ;; copy the new-messages list because sorting might scramble
      ;; it.  Also something the user does when
      ;; vm-arrived-message-hook is run might affect it.
      ;; vm-assimilate-new-messages returns this value so it must
      ;; not be mangled.
      (setq new-messages (copy-sequence new-messages))
      ;; add the labels
      (when (and labels vm-burst-digest-messages-inherit-labels)
	(mapc (lambda (m)
		(vm-set-decoded-labels-of m (copy-sequence labels)))
	      new-messages))
      (vm-register-message-labels new-messages)
      (when vm-summary-show-threads
	;; get numbering of new messages done now
	;; so that the sort code only has to worry about the
	;; changes it needs to make.
	(vm-update-summary-and-mode-line)
	(vm-sort-messages (or vm-ml-sort-keys 
			      (if vm-summary-show-threads
				  "activity"
				"date"))))
      (when (and run-hooks
		 (or vm-arrived-message-hook vm-arrived-messages-hook))
	;; seems wise to do this so that if the user runs VM
	;; commands here they start with as much of a clean
	;; slate as we can provide, given we're currently deep
	;; in the guts of VM.
	(vm-update-summary-and-mode-line)
	(when (and vm-arrived-message-hook
		   (not (eq vm-folder-access-method 'imap)))
	  (mapc (lambda (m)
		  (vm-run-hook-on-message 'vm-arrived-message-hook m))
		new-messages))
	(run-hooks 'vm-arrived-messages-hook))
      (when vm-virtual-buffers
	(save-excursion
	  (setq b-list vm-virtual-buffers)
	  (while b-list
	    ;; buffer might be dead
	    (when (buffer-name (car b-list))
	      (let (tail-cons)
		(set-buffer (car b-list))
		(setq tail-cons (vm-last vm-message-list))
		(vm-build-virtual-message-list new-messages)
		(when (or (null tail-cons) (cdr tail-cons))
		  (if (not vm-assimilate-new-messages-sorted)
		      (setq vm-ml-sort-keys nil))
		  (if (vectorp vm-thread-obarray)
		      (vm-build-threads (cdr tail-cons)))
		  (vm-set-summary-redo-start-point
		   (or (cdr tail-cons) vm-message-list))
		  (vm-set-numbering-redo-start-point
		   (or (cdr tail-cons) vm-message-list))
		  (unless vm-message-pointer
		    (setq vm-message-pointer vm-message-list
			  vm-need-summary-pointer-update t)
		    (if vm-message-pointer
			(vm-present-current-message)))
		  (when vm-summary-show-threads
		    (vm-update-summary-and-mode-line)
		    (vm-sort-messages (or vm-ml-sort-keys "activity")))
		  )))
	    (setq b-list (cdr b-list)))))
      (when vm-ml-sort-keys
	(vm-sort-messages vm-ml-sort-keys)))
    new-messages ))

(defun vm-select-operable-messages (count 
				    &optional interactive op-description)
  "Return a list of all marked messages, messages indicated by
the COUNT or messages in a collapsed thread, in that
order.  

Marked messages are returned only if the previous command was
`vm-next-command-uses-marks'.  

COUNT is used if it is non-nil and different from 1 or
INTERACTIVE is nil.  In that case, a number of messages around
`vm-message-pointer' equal to (abs count) are returned, either
backward (if COUNT is negative) or forward (if positive).  If
COUNT is zero, then all messages in the folder are returned.

If INTERACTIVE is t and the current operation is a thread operation
invoked in a Summary buffer, then all the messages in the thread are
returned. 

Otherwise, if COUNT is 1, then the current message is returned.  If
COUNT is nil then no messages are returned.

OP-DESCRIPTION is a string describing the opeartion being peformed,
which is used in interactive confirmations."
  (cond ((eq last-command 'vm-next-command-uses-marks)
	 (vm-marked-messages))
	((and count (not (= count 1)))
	 (let ((direction (if (< count 0) 'backward 'forward))
	       (count (vm-abs count))
	       (vm-message-pointer vm-message-pointer) ; why this?
	       mlist)
	   (if (= count 0)
	       (setq mlist (copy-sequence vm-message-list))
	     (unless (eq vm-circular-folders t)
	       ;; Operate on as many messages as there are, rather than
	       ;; refusing to act.  This used to be a `vm-check-count', which
	       ;; signals end-of-folder, so `C-u 10 d' with fewer than ten
	       ;; messages left deleted nothing at all -- issue #550.  Doing as
	       ;; much as was asked for is what Emacs's own commands do at a
	       ;; boundary, and the commands here report how many they acted on,
	       ;; so a short count is visible rather than silent.
	       (setq count
		     (min count
			  (if (eq direction 'forward)
			      (length vm-message-pointer)
			    (1+ (- (length vm-message-list)
				   (length vm-message-pointer)))))))
	     (while (not (zerop count))
	       (setq mlist (cons (car vm-message-pointer) mlist))
	       (vm-decrement count)
	       (unless (zerop count)
		 (vm-move-message-pointer direction))))
	   (nreverse mlist)))
	((and interactive
	      (vm-summary-operation-p)
	      vm-summary-enable-thread-folding
	      vm-summary-show-threads
	      vm-enable-thread-operations
	      (vm-thread-root-p (vm-current-message))
	      (vm-collapsed-root-p (vm-current-message))
	      (or (eq vm-enable-thread-operations t)
		  (y-or-n-p 
		   (format "%s: %s all messages in thread? " 
			   (buffer-name) op-description))))
	 (vm-thread-subtree (vm-current-message)))
	((null count)
	 nil)
	(t
	 (list (vm-current-message)))
	))

(defun vm-display-startup-message ()
  (if (sit-for 5)
      (let ((lines vm-startup-message-lines))
	(vm-inform 8 "VM %s. Type ? for help." (vm-version))
	(setq vm-startup-message-displayed t)
	(while (and (sit-for 4) lines)
	  (vm-inform 8 "%s" (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (vm-inform 8 ""))

;;;###autoload
(defun vm-toggle-read-only ()
  "If the current VM folder is read-only, make it modifiable.

This command can also be used to make a modifiable folder read-only.
However it is unsafe to do so because any previous modifications will
be discarded when the folder is quit.  You should first save the
current changes of the folder before making it read-only."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if vm-folder-read-only
      (setq vm-folder-read-only nil)
    (if (or (not (buffer-modified-p))
	    (y-or-n-p 
	     (concat "It is unsafe to make the folder read-only. "
		     "Proceed? ")))
	(setq vm-folder-read-only t)
      (error "Aborted")))
  (intern (buffer-name) vm-buffers-needing-display-update)
  (vm-inform 5 "Folder is now %s"
	   (if vm-folder-read-only "read-only" "modifiable"))
  (vm-display nil nil '(vm-toggle-read-only) '(vm-toggle-read-only))
  (vm-update-summary-and-mode-line))

(defvar scroll-in-place)

;; this does the real major mode scutwork.
(defun vm-folder-hard-link-count (&optional file)
  "How many names the folder's file has, or nil if that cannot be told.
FILE defaults to the visited file."
  (let* ((name (or file buffer-file-name))
	 (attributes (and (stringp name) (file-attributes name))))
    (and attributes (file-attribute-link-number attributes))))

(defun vm-warn-about-hard-links ()
  "Say so when the folder's file has another name, and saving will break it.
`file-precious-flag' writes a temporary file and renames it into place, which
gives the folder's name a new inode; every other name for the old one keeps
the mail as it was and quietly stops following this folder.  Symbolic links
are handled -- see `file-preserve-symlinks-on-save' above -- but a hard link
cannot be, so the choice is `vm-folder-file-precious-flag' nil or knowing.
Issue #532."
  (let ((links (vm-folder-hard-link-count)))
    (when (and links (> links 1) file-precious-flag)
      (vm-warn 0 3 (concat "%s has %d names; saving will leave the other%s "
			   "with the mail as it is now.  Set "
			   "vm-folder-file-precious-flag to nil for this "
			   "folder to keep them together")
	       (file-name-nondirectory buffer-file-name)
	       links (if (> links 2) "s" "")))))

(defun vm-mode-internal (&optional access-method reload)
  "Turn on vm-mode in the current buffer.
ACCESS-METHOD is either `pop' or `imap' for server folders.
If RELOAD is non-Nil, then the folder is being recovered.  So,
folder-access-data should be preserved."
  (widen)
  (make-local-variable 'require-final-newline)
  ;; don't kill local variables, as there is some state we'd like to
  ;; keep.  rather than non-portably marking the variables we
  ;; want to keep, just avoid calling kill-local-variables and
  ;; reset everything that needs to be reset.
  (setq
   major-mode 'vm-mode
   mode-line-format vm-mode-line-format
   mode-name "VM"
   ;; must come after the setting of major-mode
   mode-popup-menu (and vm-use-menus
			(vm-menu-support-possible-p)
			(vm-menu-mode-menu))
   buffer-read-only t
   ;; If the user quits a vm-mode buffer, the default action is
   ;; to kill the buffer.  Make a note that we should offer to
   ;; save this buffer even if it has no file associated with it.
   ;; We have no idea of the value of the data in the buffer
   ;; before it was put into vm-mode.
   buffer-offer-save t
   require-final-newline nil
   ;; don't let CR's in folders be mashed into LF's because of a
   ;; stupid user setting.
   selective-display nil
   vm-thread-obarray 'bonk
   vm-thread-subject-obarray 'bonk
   vm-label-obarray (make-vector 29 0)
   vm-last-message-pointer nil
   vm-modification-counter 0
   vm-message-list nil
   vm-message-pointer nil
   vm-message-order-changed nil
   vm-message-order-header-present nil
   vm-imap-retrieved-messages nil
   vm-pop-retrieved-messages nil
   vm-summary-buffer nil
   vm-system-state nil
   vm-undo-record-list nil
   vm-undo-record-pointer nil
   vm-virtual-buffers (vm-link-to-virtual-buffers)
   vm-folder-type (vm-get-folder-type))
  ;; the list was emptied above, and this counter is not reset with it: a
  ;; reader of the folder this buffer held before has to see the number move,
  ;; and one set back to zero would look to it like nothing had happened
  (vm-increment vm-message-list-generation)
  (when (not reload)
    (cond ((eq access-method 'pop)
	   (setq vm-folder-access-method 'pop)
	   (setq vm-folder-access-data
		 (make-vector vm-folder-pop-access-data-length nil)))
	  ((eq access-method 'imap)
	   (setq vm-folder-access-method 'imap)
	   (setq vm-folder-access-data
		 (make-vector vm-folder-imap-access-data-length nil)))
	  ((vm-cache-folder-name-p buffer-file-name)
	   ;; A server folder's local cache, opened as though it were a folder
	   ;; of its own -- by find-file, or by desktop.el restoring it, or by
	   ;; recover-file outside vm-recover-file.  It reads correctly, which
	   ;; is the trouble: it looks like the mailbox and is not connected to
	   ;; it, so nothing here reaches the server and the next real session
	   ;; will not see any of it.  Issue #425.
	   (vm-warn 0 3 (concat "%s is the local cache of a server folder; "
				"visit it with vm-visit-imap-folder or "
				"vm-visit-pop-folder, or changes here will "
				"be lost")
		    (file-name-nondirectory buffer-file-name)))))
  (use-local-map vm-mode-map)
  ;; if the user saves after M-x recover-file, let them get new
  ;; mail again.
  (add-hook 'after-save-hook 'vm-unblock-new-mail nil t)
  (when (vm-menu-support-possible-p)
    (vm-menu-install-menus))
  (add-hook 'kill-buffer-hook 'vm-garbage-collect-folder)
  (add-hook 'kill-buffer-hook 'vm-garbage-collect-message)
  ;; Killing a real folder takes its virtual folders with it, since they cannot
  ;; work without its buffer (issue #573).  Buffer-local, unlike the two above:
  ;; these have no business running as every other buffer in Emacs is killed.
  (add-hook 'kill-buffer-query-functions 'vm-virtual-kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook 'vm-virtual-kill-buffers nil t)
  ;; avoid the XEmacs file dialog box.
  (defvar use-dialog-box)
  (make-local-variable 'use-dialog-box)
  (setq use-dialog-box nil)
  ;; mail folders are precious.  protect them by default.
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag vm-folder-file-precious-flag)
  ;; That protection writes a temporary file and renames it over the folder,
  ;; which replaces the folder's own name -- so a folder visited through a
  ;; symbolic link had the link replaced by a plain file (issue #532).  Emacs
  ;; has a companion setting for exactly this case: with it, the link is
  ;; resolved first and the rename lands on the file it points at.  Its
  ;; docstring says it matters only when `file-precious-flag' is set, which
  ;; here it is by default, and that symlinks are preserved anyway when it is
  ;; not -- so this is right either way.
  (when (boundp 'file-preserve-symlinks-on-save) ; Emacs 28.1
    (make-local-variable 'file-preserve-symlinks-on-save)
    (setq file-preserve-symlinks-on-save t))
  ;; A *hard* link cannot be saved that way, and cannot be saved any other way
  ;; either while the write is atomic: a rename gives this name a new inode and
  ;; leaves every other name on the old one.  So say so on the way in, rather
  ;; than let the other name quietly stop following the folder (issue #532).
  (vm-warn-about-hard-links)
  ;; scroll in place messes with scroll-up and this loses
  (make-local-variable 'scroll-in-place)
  (setq scroll-in-place nil)
  (run-hooks 'vm-mode-hook)
  ;; compatibility
  (run-hooks 'vm-mode-hooks))

(defun vm-link-to-virtual-buffers ()
  "If there are visited virtual folders that depend on the current
real folder, then link them to the current folder and update their
contents." 
  (let ((b-list (buffer-list))
	(vbuffers nil)
	(folder-buffer (current-buffer))
	folders folder clauses)
    (save-excursion
      (while b-list
	(set-buffer (car b-list))
	(cond ((eq major-mode 'vm-virtual-mode)
	       (setq clauses (cdr vm-virtual-folder-definition))
	       (while clauses
		 (setq folders (car (car clauses)))
		 (while folders
		   (setq folder (car folders))
		   (if (eq folder-buffer 
			   (or (and (stringp folder)
				    (vm-get-file-buffer
				     (expand-file-name folder 
						       vm-folder-directory)))
			       (and (listp folder)
				    (eval folder))))
		       (setq vbuffers (cons (car b-list) vbuffers)
			     vm-real-buffers (cons folder-buffer
						   vm-real-buffers)
			     folders nil
			     clauses nil))
		   (setq folders (cdr folders)))
		 (setq clauses (cdr clauses)))))
	(setq b-list (cdr b-list)))
      vbuffers )))

(defun vm-folder-backup-name (file)
  "The name Emacs would back FILE up as, were it saving a buffer.
`make-backup-file-name' honours `backup-directory-alist', and
`find-backup-file-name' the numbered-backup settings, so a user who keeps
backups somewhere else, or keeps several, gets this one where the others are.

The copy is made whatever `make-backup-files' says: this is not an ordinary
save but a rewrite of every message in a folder, and VM promises the copy in
the message it prints and in the error that recommends the conversion."
  (if version-control
      (car (find-backup-file-name file))
    (make-backup-file-name file)))

(defun vm-backup-folder-file ()
  "Copy this folder's file to its backup name, if it has one and it exists.
For a change about to rewrite every message: Emacs backs a file up on the
first save of its buffer, so a folder already saved this session would have
none."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let ((backup (vm-folder-backup-name buffer-file-name)))
      (copy-file buffer-file-name backup t)
      (vm-inform 5 "Kept the folder as it was in %s"
		 (abbreviate-file-name backup)))))

;;;###autoload
(defun vm-backup-folder ()
  "Keep a copy of this folder as it is on disk.
Emacs backs a file up on the first save of its buffer and not again, so a
folder saved earlier in this session has no copy of what is on disk now.
This makes one whatever `make-backup-files' says.

The copy is named as Emacs would name a backup: `backup-directory-alist'
decides where it goes and the numbered-backup settings how many are kept, so
it lands where your other backups are.

It copies the file and not the buffer, so changes you have not saved are not
in it.  Save the folder first to keep those.

Run it from anywhere in a folder, the summary included.  `backup-buffer'
does nothing in a summary or presentation buffer, those visiting no file,
which is what makes a command of VM's own worth having."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-virtual-folder)
  (unless buffer-file-name
    (error (concat "This folder has no file to copy; write it to one with"
		   " vm-write-file first")))
  (unless (file-exists-p buffer-file-name)
    (error "%s does not exist yet; save the folder with vm-save-folder first"
	   (abbreviate-file-name buffer-file-name)))
  (vm-backup-folder-file))

(defun vm-folder-attendant-files (file)
  "The files VM keeps beside the folder FILE.
Its index file and the message summary Thunderbird writes.  Not the backup,
which is a reader's to keep, and not the auto-save file, whose name a
reader can have moved with `auto-save-file-name-transforms'."
  (let ((directory (file-name-directory file))
	(name (file-name-nondirectory file)))
    (append (when (stringp vm-index-file-suffix)
	      (list (concat directory "." name vm-index-file-suffix)))
	    (list (concat file ".msf")))))

(defun vm-purge-renamed-folder-file (old interactive)
  "Offer to delete OLD, left holding a folder that has been written elsewhere.
INTERACTIVE says whether there is anybody to ask; without one nothing is
deleted, since a file is not removed on a guess.

Asked rather than done.  The old name may be where mail is delivered, or what
`vm-spool-files' or an account's inbox names, and a reader who keeps it is
entitled to.  Said either way, because two folders holding the same mail is
a thing to know about: VM is looking at the new one, so the old goes stale."
  (when (and old (file-exists-p old))
    (if (and interactive
	     (yes-or-no-p (format "Delete %s, which holds the folder in its old type? "
				  (abbreviate-file-name old))))
	(progn
	  (vm-error-free-call 'delete-file old)
	  (dolist (file (vm-folder-attendant-files old))
	    (when (file-exists-p file)
	      (vm-error-free-call 'delete-file file)))
	  (vm-inform 5 "%s removed" (abbreviate-file-name old)))
      (vm-warn 1 0 (concat "%s still holds this folder in its old type, and"
			   " VM is not looking at it: it will go stale")
	       (abbreviate-file-name old)))))

(defun vm-folder-buffer-in-use-p (buffer)
  "Whether BUFFER is a folder somebody is reading, rather than a leftover.
`vm-message-pointer' is what tells them apart.  A visit that fails partway
leaves a `vm-mode' buffer without one -- holding the messages read before the
error, or none at all, depending on how far it got -- and that buffer is not
one to keep, let alone to convert from.  It is also the state
`vm-error-if-folder-not-read-through' refuses, and the folder whose repair is
the on-disk conversion in the first place.

A buffer with a pointer is a folder in use, and converting the file under it
would leave it holding the folder in a type it no longer is."
  (with-current-buffer buffer
    (and (eq major-mode 'vm-mode)
	 vm-message-pointer
	 t)))

(defun vm-kill-folder-buffer-with-its-attendants (buffer)
  "Kill folder BUFFER, and the summary and presentation buffers that serve it.
Killing the folder buffer alone leaves those two pointing at a dead buffer,
where every command answers \"Folder buffer has been killed\" and the reader
has nothing to do but kill them by hand."
  (let (summary presentation)
    (with-current-buffer buffer
      (setq summary vm-summary-buffer
	    presentation vm-presentation-buffer-handle))
    (dolist (attendant (list summary presentation))
      (when (buffer-live-p attendant)
	(kill-buffer attendant)))
    (kill-buffer buffer)))

(defun vm-change-folder-type-of-file (file type &optional interactive output)
  "Convert the folder FILE on disk to TYPE, without visiting it.
INTERACTIVE says whether there is anybody to ask about deleting the file left
behind when the name changes.

OUTPUT, if given, is where the converted folder is written, and FILE is then
left exactly as it was: no backup is made, since nothing is overwritten, and
nothing is offered for deletion.  OUTPUT must not exist, and its name must be
one a TYPE folder may be written under -- a folder called out.mbox cannot hold
mboxcl2 (emacs-vm/vm#763).  OUTPUT naming FILE itself is the in-place
conversion below.
A folder somebody is reading is refused, since converting the file under a live
buffer would leave that buffer holding the folder in a type it no longer is.
The buffer a failed visit left behind is killed instead, with its summary and
presentation, which is the case this is for.

This is how to repair a folder VM will not read: a folder saying it is mboxcl2
with a message that has no `Content-Length' cannot be visited, so its type
cannot be changed in a buffer.  `vm-mboxcl2-strict' is bound to nil while the
folder is read here, since repairing it is the whole point -- nothing global
is left switched off afterwards, which is the trouble with doing it by hand.

TYPE may be the type the folder already claims: converting mboxcl2 to mboxcl2
recomputes every length, and that is the repair.

The file is written only if the result reads back as TYPE, strictly, and holds
the same number of messages.  A folder already sound is not rewritten at all,
and is renamed where its name does not state TYPE: nothing has to be written
for that, and the name is what the type is read from next time.

Without OUTPUT it is written under the name TYPE asks for, since the name is
what states the type: sent.mboxcl2 converted to From_ is written as sent, and
FILE is then offered for deletion.  Where the name does not change, the
previous contents are kept in a backup file, named as Emacs would name one
when saving a buffer."
  ;; Before anything else, and said precisely: without these the type is read
  ;; as nothing and the fault came back "has no folder type VM recognizes",
  ;; which sends the reader to look at the contents of a file that is not
  ;; there or that they cannot open (emacs-vm/vm#771).
  (unless (file-exists-p file)
    (error "%s does not exist" (abbreviate-file-name file)))
  (unless (file-readable-p file)
    (error "%s cannot be read; check its permissions"
	   (abbreviate-file-name file)))
  (let ((buffer (vm-get-file-buffer file)))
    (when buffer
      (when (buffer-modified-p buffer)
	(error (concat "%s is visited and has unsaved changes; save it, or"
		       " change its type in its buffer with"
		       " M-x vm-change-folder-type")
	       (file-name-nondirectory file)))
      (when (vm-folder-buffer-in-use-p buffer)
	(error (concat "%s is being visited; quit that folder with"
		       " M-x vm-quit and convert it again")
	       (file-name-nondirectory file)))
      ;; Only the buffer a failed visit left behind gets here, and nothing is
      ;; lost: it has no changes, it holds however many messages were read
      ;; before the error -- five of seven, in the case this was written for
      ;; -- and it is not a buffer to keep, let alone to convert from.
      (vm-inform 5 "Killing the buffer visiting %s, which was not read through"
		 (file-name-nondirectory file))
      (vm-kill-folder-buffer-with-its-attendants buffer)))
  (let ((old (vm-get-folder-type file))
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	;; nil where OUTPUT is FILE: that is the in-place conversion, said
	;; another way
	(destination (and output
			  (not (equal (expand-file-name output)
				      (expand-file-name file)))
			  (expand-file-name output)))
	before after original)
    (when (memq old '(nil unknown))
      (error "%s has no folder type VM recognizes, so there is nothing to convert"
	     (file-name-nondirectory file)))
    ;; Before the folder is read, which on a gigabyte cache is a minute: a
    ;; name that cannot hold TYPE is refused whatever the contents turn out
    ;; to be.
    (when destination
      (vm-error-if-name-contradicts-type destination type)
      (when (file-exists-p destination)
	(error "%s exists already; move it aside, or name another file"
	       (abbreviate-file-name destination))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Each of these walks or copies the whole folder, which on a gigabyte
      ;; cache is a minute at a time, so each says it is starting: a silence
      ;; that long is indistinguishable from a hung Emacs (emacs-vm/vm#748).
      (vm-inform 5 "Reading %s..." (file-name-nondirectory file))
      (insert-file-contents-literally file)
      ;; A hash of the folder rather than a copy of it: an IMAP cache folder
      ;; runs to a gigabyte, and `buffer-string' here and again at the end
      ;; would ask for two more of them.
      (vm-inform 5 "Reading %s... %d bytes, checksumming"
		 (file-name-nondirectory file) (buffer-size))
      (setq original (buffer-hash))
      (let ((vm-folder-type old)
	    (vm-mboxcl2-strict nil))
	(vm-inform 5 "Counting the messages in %s..."
		   (file-name-nondirectory file))
	(setq before (vm-count-messages-in-buffer))
	(vm-inform 5 "Converting %s from %s to %s, %d messages..."
		   (file-name-nondirectory file) old type before)
	(vm-convert-folder-type old type))
      ;; strict this time: what would be written has to read back as what it
      ;; now says it is, or the file is left as it was
      (vm-inform 5 "Checking that the result reads back as %s..." type)
      (let ((vm-folder-type type))
	(setq after (vm-count-messages-in-buffer)))
      (cond ((/= before after)
	     (error (concat "Not writing %s: it holds %d messages and the"
			    " conversion produced %d")
		    (file-name-nondirectory file) before after))
	    (destination
	     ;; A folder already sound is written all the same: the reader
	     ;; asked for a copy of it under this name, and answering that
	     ;; there was nothing to do would leave them without one.
	     (vm-inform 5 "Writing %s..." (file-name-nondirectory destination))
	     (write-region (point-min) (point-max) destination nil 'quiet)
	     (vm-inform 5 "%s converted from %s to %s as %s, %d messages; %s is unchanged"
			(file-name-nondirectory file) old type
			(file-name-nondirectory destination) after
			(file-name-nondirectory file)))
	    ((equal original (buffer-hash))
	     ;; Sound already.  Nothing to write, but the name still has to
	     ;; state the type, or the folder is read as something else next
	     ;; time and the conversion did not outlive the session (#743).
	     ;; That is a cache VM wrote as mboxcl2 before it named them.
	     (let ((new-file (vm-folder-name-for-type file type)))
	       (if (equal new-file file)
		   (vm-inform 5 "%s is already a sound %s folder, %d messages"
			      (file-name-nondirectory file) type after)
		 (vm-error-if-name-contradicts-type new-file type)
		 (when (file-exists-p new-file)
		   (error (concat "%s is a folder already; move it aside, or"
				  " rename this one by hand")
			  (abbreviate-file-name new-file)))
		 (rename-file file new-file)
		 (vm-inform 5 (concat "%s is already a sound %s folder,"
				      " %d messages; renamed to %s")
			    (file-name-nondirectory file) type after
			    (file-name-nondirectory new-file)))))
	    ((equal (vm-folder-name-for-type file type) file)
	     (vm-error-if-name-contradicts-type file type)
	     (let ((backup (vm-folder-backup-name file)))
	       (vm-inform 5 "Backing %s up as %s..."
			  (file-name-nondirectory file)
			  (file-name-nondirectory backup))
	       (copy-file file backup t)
	       (vm-inform 5 "Writing %s..." (file-name-nondirectory file))
	       (write-region (point-min) (point-max) file nil 'quiet)
	       (vm-inform 5 "%s converted from %s to %s, %d messages; was %s"
			  (file-name-nondirectory file) old type after
			  (abbreviate-file-name backup))))
	    (t
	     ;; The name states the type, so the converted folder is written
	     ;; under the name TYPE asks for, and FILE is then offered for
	     ;; deletion (emacs-vm/vm#743).  Backed up all the same: FILE looks
	     ;; like backup enough until the offer is accepted.
	     (let ((new-file (vm-folder-name-for-type file type))
		   (backup (vm-folder-backup-name file)))
	       (vm-error-if-name-contradicts-type new-file type)
	       (when (file-exists-p new-file)
		 (error (concat "%s is a folder already; move it aside, or"
				" rename this one by hand")
			(abbreviate-file-name new-file)))
	       (vm-inform 5 "Backing %s up as %s..."
			  (file-name-nondirectory file)
			  (file-name-nondirectory backup))
	       (copy-file file backup t)
	       (vm-inform 5 "Writing %s..." (file-name-nondirectory new-file))
	       (write-region (point-min) (point-max) new-file nil 'quiet)
	       (vm-inform 5 "%s converted from %s to %s as %s, %d messages"
			  (file-name-nondirectory file) old type
			  (file-name-nondirectory new-file) after)
	       (vm-purge-renamed-folder-file file interactive)))))))

(defun vm-cache-folders-in-the-older-format ()
  "The POP and IMAP caches on disk whose names do not state their type.
Every cache VM creates carries `vm-cache-folder-type-suffix' and is written
in that type.  One without it was written by a VM that did not name its
caches, and is read as From_, so it keeps the weakness mboxcl2 exists to
remove: a message whose body holds a line beginning \"From \" can split it in
two.

The directories are the ones a cache name is built in:
`vm-imap-folder-cache-directory', `vm-pop-folder-cache-directory',
`vm-folder-directory' and the home directory.

Answers (FILES . FAULTS), FAULTS pairing each directory that could not be
listed with what went wrong.  Collected rather than raised, for the reason the
conversion collects its own: one directory VM cannot read is no reason to
search none of the others, and the reader is told which it was."
  (let ((files nil)
	(faults nil))
    (dolist (dir (vm-cache-folder-directories))
      (condition-case fault
	  (setq files (nconc files (vm-cache-folders-in-directory dir)))
	(file-error (push (cons dir (error-message-string fault)) faults))))
    (cons files (nreverse faults))))

(defun vm-cache-folder-directories ()
  "The directories a cache file can have been created in, each of them once.
Once by `file-truename', not by the name as configured: two of these being one
directory reached two ways is ordinary -- /tmp is a symbolic link on macOS, and
a home directory is one on many managed systems -- and it made the cache in it
appear twice.  The second conversion then found the file already renamed and
reported a failure that had not happened."
  (delete-dups
   (delq nil
	 (mapcar (lambda (dir)
		   (and dir (file-directory-p dir)
			(file-name-as-directory (file-truename dir))))
		 (list vm-imap-folder-cache-directory
		       vm-pop-folder-cache-directory
		       vm-folder-directory
		       (getenv "HOME"))))))

(defun vm-cache-folders-in-directory (dir)
  "The caches in DIR whose names do not state a type."
  (let ((found nil))
    (dolist (file (directory-files dir t))
      (when (and (vm-cache-folder-name-p file)
		 (null (vm-folder-type-for-name file))
		 (file-regular-p file))
	(push file found)))
    (sort found #'string-lessp)))

(defun vm-convert-one-cache (file ask)
  "Convert cache FILE to mboxcl2.  ASK asks about this one first.
Answers t where it was converted, nil where it was declined, and the fault as
a string where it could not be."
  (if (and ask
	   (not (y-or-n-p (format "Convert %s? " (file-name-nondirectory file)))))
      nil
    (condition-case fault
	(progn (vm-change-folder-type-of-file file 'mboxcl2 t) t)
      (error (error-message-string fault)))))

(defun vm-convert-caches (files ask)
  "Convert each cache in FILES to mboxcl2, and answer (CONVERTED . FAULTS).
FAULTS pairs each cache that could not be converted with what went wrong.  A
fault is collected rather than raised: stopping partway through a dozen caches
would leave the reader a half-done job and no account of it, and every fault is
named in the report."
  (let ((faults nil)
	(converted 0))
    (dolist (file files)
      (let ((answer (vm-convert-one-cache file ask)))
	(cond ((stringp answer) (push (cons file answer) faults))
	      (answer (setq converted (1+ converted))))))
    (cons converted (nreverse faults))))

(defun vm-cache-conversion-tally (found converted faults)
  "One line saying how the conversion of FOUND caches went.
CONVERTED is how many were, FAULTS what stopped the rest."
  (format "%d cache%s of %d converted%s"
	  converted (if (= converted 1) "" "s") found
	  (if faults (format ", %d could not be" (length faults)) "")))

(defun vm-report-cache-conversion (found converted faults)
  "Say how the conversion of FOUND caches went, and name every fault.
The tally alone where nothing went wrong.  A buffer as soon as anything did,
because the faults are what the reader has to act on and a run of messages in
the echo area replaces each with the next: with a dozen caches only the last
of them would still be readable, which is why `vm-check-folder-report' puts
its list in a buffer too."
  (let ((tally (vm-cache-conversion-tally found converted faults)))
    (if (null faults)
	(vm-inform 1 "%s" tally)
      (with-output-to-temp-buffer "*VM cache conversion*"
	(princ (format "%s\n\n" tally))
	(princ "These caches were left exactly as they were:\n\n")
	(dolist (fault faults)
	  (princ (format "  %s\n      %s\n"
			 (abbreviate-file-name (car fault)) (cdr fault))))
	(princ (concat "\nNothing was refetched.  Deal with each of these and"
		       " run M-x vm-convert-caches-to-mboxcl2 again: the"
		       " caches already converted are named for their type"
		       " now and will not be offered a second time.\n")))
      (vm-inform 1 "%s; see *VM cache conversion*" tally))))

;;;###autoload
(defun vm-convert-caches-to-mboxcl2 (&optional each)
  "Convert every POP and IMAP cache whose name does not state its type.
A cache VM creates now is named for its type and written as mboxcl2, where the
end of a message is a byte count rather than a line that has to be recognised.
A cache from before that has no such name and is read as From_, so a message
whose body holds a line beginning \"From \" can still split it in two.  This
converts each of those and renames it, which is `vm-change-folder-type-of-file'
once per cache: the previous contents are kept in a backup file, and a cache
that is already mboxcl2 in all but its name is only renamed.

It asks once before starting, and then once per cache about deleting the copy
left under the old name -- that one is a file on disk and a decision of its
own, so it is asked rather than assumed either way.  With a prefix argument it
asks about each cache before converting it as well.

A cache being visited cannot be converted and is reported; quit that folder
with `vm-quit' and run this again.  Nothing is refetched, and a cache that
cannot be read is left exactly as it was.  Where anything could not be
converted the faults are listed in a buffer, since a run of them in the echo
area cannot be read.

The caches are looked for where their names are built, which is
`vm-imap-folder-cache-directory', `vm-pop-folder-cache-directory',
`vm-folder-directory' and the home directory."
  (interactive "P")
  (let* ((search (vm-cache-folders-in-the-older-format))
	 (files (car search))
	 ;; A directory that could not be listed is a fault like any other and
	 ;; is reported with them, rather than stopping the search of the rest.
	 (unsearched (cdr search)))
    (cond ((and (null files) (null unsearched))
	   (vm-inform 1 "No cache is in the older format; each one names its type"))
	  ((null files)
	   (vm-report-cache-conversion 0 0 unsearched))
	  ((not (or each
		    (y-or-n-p (format "Convert %d cache%s to mboxcl2? "
				      (length files)
				      (if (cdr files) "s" "")))))
	   (vm-inform 1 "No caches converted"))
	  (t
	   (let ((result (vm-convert-caches files each)))
	     (vm-report-cache-conversion (length files) (car result)
					 (append unsearched (cdr result))))))))

(defun vm-error-if-folder-not-read-through ()
  "Signal unless this folder buffer holds the whole of its folder.
A visit that fails partway leaves a `vm-mode' buffer behind holding the
messages read before the error and no `vm-message-pointer' -- five of seven,
for the mboxcl2 folder this was written for.  Rewriting that buffer converts
those messages and leaves the rest in the format they were, so the folder ends
up half of each; the missing pointer then signalled `wrong-type-argument
arrayp nil', with the buffer already modified and the folder already backed up.

The repair for such a folder is the on-disk conversion, which reads the file
rather than the buffer -- see `vm-change-folder-type-of-file'."
  (when (and vm-message-list (null vm-message-pointer))
    (error (concat "%s was not read all the way through, so its type cannot"
		   " be changed here.  Convert it on disk instead:"
		   " C-u M-x vm-change-folder-type, which asks for the file")
	   (or (and buffer-file-name
		    (file-name-nondirectory buffer-file-name))
	       (buffer-name)))))

(defun vm-folder-claimed-content-length (m)
  "The octet count M's own `Content-Length' header claims, or nil if it has
none.  What the header says, not what the body measures: the two disagreeing
is the fault worth finding."
  (save-excursion
    (goto-char (vm-headers-of m))
    (let ((case-fold-search t))
      (when (re-search-forward
	     (concat "^" (regexp-quote vm-content-length-header) "[ \t]*\\([0-9]+\\)")
	     (vm-text-of m) t)
	(string-to-number (match-string 1))))))

(defun vm-folder-body-octets-without-trailing-newlines (m)
  "The octets of M's body, not counting newlines at the end of it.
`vm-find-trailing-message-separator' skips any number of newlines past the
count, on the grounds that some mailers do not count the last one, so a length
short by them is one the reader accepts and this must not complain about."
  (let ((end (vm-text-end-of m)))
    (save-excursion
      (goto-char end)
      (skip-chars-backward "\n" (vm-text-of m))
      (vm-message-body-octets (vm-text-of m) (point)))))

(defun vm-folder-length-fits-p (m)
  "Whether M's `Content-Length' describes its body.  Nil when it has none.
A length is right when it counts the body, and accepted when it counts the
body without the newlines at the end of it, since that is what the reader
accepts.  Anything else is a length that does not describe the message."
  (let ((claimed (vm-folder-claimed-content-length m))
	(actual (vm-message-body-octets (vm-text-of m) (vm-text-end-of m)))
	(least (vm-folder-body-octets-without-trailing-newlines m)))
    (and claimed
	 (or (= claimed actual) (and (>= claimed least) (<= claimed actual))))))

(defun vm-folder-length-fault (m number)
  "What is wrong with M's `Content-Length', as a line, or nil if nothing is.
NUMBER is the message's position in the folder, for the report."
  (let ((claimed (vm-folder-claimed-content-length m))
	(actual (vm-message-body-octets (vm-text-of m) (vm-text-end-of m))))
    (cond ((null claimed)
	   (format "message %d has no Content-Length; its body is %d octets"
		   number actual))
	  ((vm-folder-length-fits-p m) nil)
	  (t
	   (format "message %d says Content-Length %d and its body is %d octets"
		   number claimed actual)))))

(defun vm-folder-length-faults ()
  "Every message in this folder whose `Content-Length' is wrong or missing.
A list of lines, empty for a sound folder.  Nil for a folder of any type that
carries no such header, which has nothing to be wrong."
  (when (eq vm-folder-type 'mboxcl2)
    (let ((number 0)
	  (faults nil))
      (dolist (m vm-message-list (nreverse faults))
	(setq number (1+ number))
	(let ((fault (vm-folder-length-fault m number)))
	  (when fault (push fault faults)))))))

(defun vm-folder-length-survey ()
  "How many messages carry a `Content-Length' and how many of those it fits.
A cons of the two counts.  The type is not consulted: the header is what says
a folder is mboxcl2, so counting it over the whole folder is how a name that
says otherwise gets checked.  `vm-folder-looks-like-mboxcl2-p' asks the same
question of the first two messages, which is as much as a folder being visited
can afford to read."
  (let ((carrying 0)
	(fitting 0))
    (dolist (m vm-message-list (cons carrying fitting))
      (when (vm-folder-claimed-content-length m)
	(setq carrying (1+ carrying))
	(when (vm-folder-length-fits-p m)
	  (setq fitting (1+ fitting)))))))

(defun vm-folder-mboxcl2-by-contents-p (survey held)
  "Whether the contents say mboxcl2: a length on every message, and each fits.
SURVEY is `vm-folder-length-survey' and HELD how many messages the folder
holds.  Read as From_, such a folder splits wherever a body line begins
\"From \", so a name that does not say mboxcl2 is worth reporting."
  (and (> held 0)
       (= (car survey) held)
       (= (cdr survey) held)))

(defun vm-check-folder-misnamed-p (survey held)
  "Whether the contents say mboxcl2 while the folder is read as something else.
SURVEY is `vm-folder-length-survey' and HELD how many messages the folder
holds.  The reader takes the type from the name, so this is the disagreement
that leaves a folder read as a type it is not."
  (and (not (eq vm-folder-type 'mboxcl2))
       (vm-folder-mboxcl2-by-contents-p survey held)))

(defun vm-check-folder-contents-line (survey held)
  "What the contents say about the type, as a line for the report.
SURVEY is `vm-folder-length-survey' and HELD how many messages the folder
holds.  A few lengths in a folder are no evidence: mail arrives carrying the
header, and VM gives one to every message it rewrites."
  (cond ((vm-folder-mboxcl2-by-contents-p survey held)
	 (format "mboxcl2 -- every one of %d messages carries a length that fits"
		 held))
	((zerop (car survey)) "nothing -- no message carries a length")
	(t (format "nothing -- %d of %d messages carry a length, %d of those fit"
		   (car survey) held (cdr survey)))))

(defun vm-check-folder-name-advice (held)
  "What to do about contents that say mboxcl2 under a name that does not.
HELD is how many messages the folder holds, for the count in the sentence."
  (format (concat "The contents say mboxcl2 and the name does not, so the"
		  " folder is read as %s: the lengths that delimit its %d"
		  " messages are ignored, and a body line beginning \"From \""
		  " splits the message it is in.\n\nTo settle it, rename the"
		  " file %s%s, which is all it takes when the folder really is"
		  " this type, or convert it with M-x vm-change-folder-type"
		  " when it is not.\n\n")
	  vm-folder-type
	  held
	  (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
	  vm-cache-folder-type-suffix))

(defun vm-check-folder-older-cache-p ()
  "Whether this folder is a POP or IMAP cache whose name states no type.
Every cache VM creates carries `vm-cache-folder-type-suffix' and is written
in that type; one without it was written before VM named its caches, so it is
read as From_ whatever it holds.  Judged by the name, since that is what a
cache is recognised by and what the type is read from."
  (and (buffer-file-name)
       (vm-cache-folder-name-p (buffer-file-name))
       (null (vm-folder-type-for-name (buffer-file-name)))))

(defun vm-check-folder-cache-advice ()
  "What to do about a cache whose name states no type.
Nothing is wrong with the folder as it stands: read as From_ it is From_, and
the messages in it now are delimited correctly.  What it lacks is the guarantee
mboxcl2 exists to give, so the report says so and names the command rather than
calling it a fault."
  (concat "This is a POP or IMAP cache from before VM named its caches for"
	  " their type, so it is read as From_.  Nothing in it is wrong now,"
	  " but a message whose body holds a line beginning \"From \" can"
	  " split in two, which is the one thing mboxcl2 rules out: it ends a"
	  " message by a byte count rather than by a line that has to be"
	  " recognised.\n\nM-x vm-convert-caches-to-mboxcl2 converts this"
	  " cache and every other one like it, keeping the previous contents"
	  " in a backup file.  Nothing is refetched.\n\n"))

(defun vm-check-folder-report (faults reader held survey)
  "Say what `vm-check-folder' found.
FAULTS is what the lengths said, READER how many messages walking the
separators finds, HELD how many the folder is holding and SURVEY what
`vm-folder-length-survey' counted.  A sound folder is one line in the echo
area; anything else gets a buffer, since a list of messages is not something
to read there.

Contents saying mboxcl2 under a name that does not is not a fault in the
folder, and gets the buffer all the same: it is the one thing here that no
other part of VM will tell the reader.  A cache whose name states no type is
the same case: the folder is sound, and a folder VM would not create now is
what this command is asked to notice."
  (if (and (null faults)
	   (equal reader held)
	   (not (vm-check-folder-misnamed-p survey held))
	   (not (vm-check-folder-older-cache-p)))
      (vm-inform 5 "%s: %s, %d messages, sound"
		 (buffer-name) vm-folder-type held)
    (let ((name (buffer-name)))
      (with-output-to-temp-buffer "*VM folder check*"
	(princ (format "%s\n\n" name))
	(princ (format "Type:              %s\n" vm-folder-type))
	(princ (format "The name says:     %s\n"
		       (or (vm-folder-type-for-name (buffer-file-name))
			   "nothing")))
	(princ (format "The contents say:  %s\n"
		       (vm-check-folder-contents-line survey held)))
	(princ (format "The default is %s\n\n" vm-default-folder-type))
	(when (vm-check-folder-misnamed-p survey held)
	  (princ (vm-check-folder-name-advice held)))
	;; After the name advice, which says what the contents turned out to
	;; be: for a cache that is mboxcl2 already both are printed, and the
	;; order reads as the finding and then what to do about it.
	(when (vm-check-folder-older-cache-p)
	  (princ (vm-check-folder-cache-advice)))
	(unless (equal reader held)
	  (princ (format (concat "The folder holds %d messages and walking the"
				 " separators finds %d.\n\n")
			 held reader)))
	(when faults
	  (princ (format "%d message%s with a length the body does not match:\n"
			 (length faults) (if (cdr faults) "s" "")))
	  (dolist (fault faults) (princ (format "  %s\n" fault)))
	  (princ (concat "\nTo repair: M-x vm-change-folder-type mboxcl2,"
			 " which recomputes every length.\n")))))))

;;;###autoload
(defun vm-check-folder (&optional file)
  "Report this folder's type and check that it is sound, writing nothing.

With a prefix argument, or with FILE given, check a folder on disk that VM is
not visiting -- see `vm-check-folder-of-file'.  That is how to check a folder
VM will not read, which is the folder most likely to want it.

Says what type the folder is, what its name says it is, what its contents say
and what the default is, how many messages it holds against how many the
reader finds by walking the separators, and for an mboxcl2 folder whether every
message's `Content-Length' matches its body.

What the contents say is counted over every message, and the name is not
consulted for it: the reader takes the type from the name, so a folder
carrying a length on every message under a name that does not say mboxcl2 is
read as From_ and split wherever a body line begins \"From \".  Nothing else
tells the reader that, and it is the question the warning at visit time
leaves open.

A wrong length is the fault this is for, because it is the one that gives no
other sign.  A missing one is refused when the folder is visited, but a
length that is merely wrong opens without complaint: the reader falls back on
searching for the next separator when the count does not land on one, so VM
reads the folder correctly while anything that believes the header -- which is
what the format is for -- takes the wrong bytes.

A POP or IMAP cache whose name states no type is reported too, and
`vm-convert-caches-to-mboxcl2' named as what converts it.  Nothing in such a
cache is wrong, so this is the one place a reader is told: it is read as From_,
where a message whose body holds a line beginning \"From \" can split in two.

Nothing is written.  `vm-change-folder-type' is the repair: converting a
folder to the type it already is recomputes every length."
  (interactive
   (list (when current-prefix-arg
	   (vm-read-file-name "Check folder file: "
			      (or vm-folder-directory default-directory)
			      nil t nil 'vm-folder-history))))
  (if file
      (vm-check-folder-of-file file)
    (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
    (vm-error-if-virtual-folder)
    (when (or (null vm-folder-type) (eq vm-folder-type 'unknown))
      (error (concat "%s has no folder type VM recognizes, so there is nothing"
		     " to check")
	     (buffer-name)))
    (save-excursion
      (save-restriction
	(widen)
	;; Not strictly, while counting: `vm-count-messages-in-buffer' reads
	;; with the reader, and the reader refuses a message with no
	;; Content-Length -- which is one of the things being reported on.
	;; Refusing to count the folder that most needs counting is no use to
	;; anybody, and nothing global is left switched off afterwards.
	(let ((vm-mboxcl2-strict nil))
	  (vm-check-folder-report (vm-folder-length-faults)
				  (vm-count-messages-in-buffer)
				  (length vm-message-list)
				  (vm-folder-length-survey)))))))

(defun vm-check-folder-of-file (file)
  "Report on the folder FILE on disk, without visiting it.  Writes nothing.
The folder that most wants checking is one VM will not visit: a folder whose
name says mboxcl2 and which has a message with no `Content-Length' is refused,
so there is no buffer in which to check it.  `vm-mboxcl2-strict' is bound nil
here, as the on-disk conversion binds it, and nothing global is left switched
off afterwards.

The type is the one the reader would take, from the name, and everything
`vm-check-folder' says of a visited folder is said of this one."
  (let ((type (vm-get-folder-type file))
	(coding-system-for-read (vm-binary-coding-system)))
    (when (memq type '(nil unknown))
      (error (concat "%s has no folder type VM recognizes, so there is nothing"
		     " to check")
	     (file-name-nondirectory file)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (vm-inform 5 "Reading %s..." (file-name-nondirectory file))
      (insert-file-contents-literally file)
      ;; The name is what states the type, and the report says what the name
      ;; says, so the buffer has to carry it.  Renamed as well, since the
      ;; report is headed with the buffer name and " *temp*" names nothing.
      (setq buffer-file-name file)
      (rename-buffer (file-name-nondirectory file) t)
      (let ((vm-mboxcl2-strict nil))
	(vm-build-message-list)
	(vm-check-folder-report (vm-folder-length-faults)
				(vm-count-messages-in-buffer)
				(length vm-message-list)
				(vm-folder-length-survey)))
      ;; or killing the buffer offers to save the folder back
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

;;;###autoload
(defun vm-change-folder-type (type &optional file output)
  "Change folder type to TYPE.
The old name `From_-with-Content-Length' is accepted for `mboxcl2'.
TYPE may be one of the following symbol values:

    From_
    mboxcl2
    BellFrom_
    mmdf
    babyl

Interactively TYPE will be read from the minibuffer.

With a prefix argument, or with FILE given, convert a folder on disk that VM
is not visiting.  That is how to repair a folder VM will not read -- see
`vm-change-folder-type-of-file'.

With two prefix arguments, or with OUTPUT given, the converted folder is
written to a file of your naming and the one converted is left as it was.
OUTPUT wants FILE: to convert the folder you are in into a new file, save it
and convert that.  A name that cannot hold TYPE is refused rather than
written, here and on disk both -- a folder called out.mbox cannot hold
mboxcl2 (emacs-vm/vm#763).

The folder's current type is offered as well as the others: converting a
folder to what it already is rewrites every message in it, which for mboxcl2
recomputes every `Content-Length'.  That is the repair for a folder whose
lengths are wrong -- and a wrong length is not a missing one, so such a folder
opens without complaint and the reader quietly falls back on searching for the
next separator.

Without FILE the folder in the current buffer is converted, and a buffer whose
visit failed partway is refused: it holds only the messages read before the
error, and the on-disk conversion is what such a folder wants."
  (interactive
   (let ((this-command this-command)
	 (last-command last-command)
	 (types vm-supported-folder-types)
	 (file nil)
	 (output nil))
     (when current-prefix-arg
       (setq file (vm-read-file-name "Change folder type of file: "
				     (or vm-folder-directory default-directory)
				     nil t nil 'vm-folder-history))
       ;; C-u C-u: convert into a file of the reader's naming and leave the
       ;; folder converted as it was
       (when (>= (prefix-numeric-value current-prefix-arg) 16)
	 (setq output (vm-read-file-name
		       (format "Write the converted folder to (leaving %s): "
			       (file-name-nondirectory file))
		       (file-name-directory file)
		       nil nil nil 'vm-folder-history))))
     (save-current-buffer
       (unless file
	 (vm-select-folder-buffer)
	 (vm-error-if-virtual-folder))
       (list (vm-canonical-folder-type
	      (intern (vm-read-string "Change folder to type: " types)))
	     file output))))
  ;; Both paths, and before either does anything: the old name has to reach
  ;; the conversion as the current one, and a type neither path can write is
  ;; worth saying so about before a folder is rewritten in it.
  (setq type (vm-canonical-folder-type type))
  (if (not (memq type '(From_ BellFrom_ mboxcl2 mmdf babyl)))
      (error "Unknown folder type: %s" type))
  (when (and output (null file))
    (error (concat "Writing the conversion elsewhere needs a folder on disk:"
		   " save this one, then C-u C-u M-x vm-change-folder-type")))
  (when file
    (vm-change-folder-type-of-file (expand-file-name file) type
				   (vm-interactive-p)
				   (and output (expand-file-name output))))
  (unless file
  (let ((asked (vm-interactive-p))
	(old-file nil)
	(new-file nil))
  (vm-select-folder-buffer-and-validate 1 asked)
  (vm-error-if-virtual-folder)
  (if (or (null vm-folder-type)
	  (eq vm-folder-type 'unknown))
      (error "Current folder's type is unknown, can't change it."))
  (vm-error-if-folder-not-read-through)
  ;; The name states the type, so a conversion that left the name alone would
  ;; not outlive the session: the folder would be read as
  ;; `vm-default-folder-type' next time and written back in it, or, where the
  ;; name states the type it no longer holds, refused (emacs-vm/vm#743).
  (setq old-file buffer-file-name
	new-file (and old-file (vm-folder-name-for-type old-file type)))
  (when new-file
    (vm-error-if-name-contradicts-type new-file type))
  (when (and new-file (not (equal new-file old-file)) (file-exists-p new-file))
    (error (concat "%s is a folder already; move it aside, or rename this"
		   " folder by hand and change its type in place")
	   (abbreviate-file-name new-file)))
  ;; Changing the type rewrites every message in the folder, so keep what is
  ;; on disk now.  Emacs' own backup happens on the first save of a buffer,
  ;; which for a folder saved earlier in the session has been and gone.
  ;;
  ;; Where the name changes too, the file left behind is the folder as it was
  ;; and looks like backup enough -- until the reader accepts the offer to
  ;; delete it, and is left with no previous copy at all.  So: always.
  (vm-backup-folder-file)
  (let ((mp vm-message-list)
	(buffer-read-only nil)
	(old-type vm-folder-type)
	;; no interruptions
	(inhibit-quit t)
	(n 0)
	(total (length vm-message-list))
	text-end) ;; opoint
    (save-excursion
      (save-restriction
       (widen)
       (setq vm-folder-type type)
       (goto-char (point-min))
       (vm-convert-folder-header old-type type)
       (while mp
	 (goto-char (vm-start-of (car mp)))
	 (insert (vm-leading-message-separator type (car mp)))
	 (if (> (vm-headers-of (car mp)) (vm-start-of (car mp)))
	     (delete-region (point) (vm-headers-of (car mp)))
	   (set-marker (vm-headers-of (car mp)) (point))
	   ;; if headers-of == start-of then so could vheaders-of
	   ;; and text-of.  clear them to force a recompute.
	   (vm-set-vheaders-of (car mp) nil)
	   (vm-set-text-of (car mp) nil))
	 (vm-convert-folder-type-headers old-type type)
	 (goto-char (vm-text-end-of (car mp)))
	 (setq text-end (point))
	 (insert-before-markers (vm-trailing-message-separator type))
	 (delete-region (vm-text-end-of (car mp)) (vm-end-of (car mp)))
	 (set-marker (vm-text-end-of (car mp)) text-end)
	 (goto-char (vm-headers-of (car mp)))
	 (vm-munge-message-separators type (vm-headers-of (car mp))
				      (vm-text-end-of (car mp)))
	 (vm-set-byte-count-of (car mp) nil)
	 (vm-set-babyl-frob-flag-of (car mp) nil)
	 (vm-set-message-type-of (car mp) type)
	 ;; Technically we should mark each message for a
	 ;; summary update since the message byte counts might
	 ;; have changed.  But I don't think anyone cares that
	 ;; much and the summary regeneration would make this
	 ;; process slower.
	 (setq mp (cdr mp) n (1+ n))
	 (vm-folder-say-progress "Converting" n total)))))
  (vm-clear-modification-flag-undos)
  (intern (buffer-name) vm-buffers-needing-display-update)
  (vm-update-summary-and-mode-line)
  (vm-inform 5 "Conversion complete.")
  ;; message separator strings may have leaked into view
  (if (> (point-max) (vm-text-end-of (car vm-message-pointer)))
      (narrow-to-region (point-min) (vm-text-end-of (car vm-message-pointer))))
  ;; The folder is written under the name its new type asks for before
  ;; anything is removed, so the mail is in two places or one and never in
  ;; none.  What is left behind is then the folder as it was, and deleting it
  ;; is asked about rather than done.
  (when (and new-file (not (equal new-file old-file)))
    (vm-write-file-to new-file)
    (vm-inform 5 "Converted to %s" (abbreviate-file-name new-file))
    (vm-purge-renamed-folder-file old-file asked))
  (vm-display nil nil '(vm-change-folder-type) '(vm-change-folder-type)))))

(defun vm-register-global-garbage-files (files)
  "Add global garbage collection actions to delete all of FILES."
  (while files
    (setq vm-global-garbage-alist
	  (cons (cons (car files) 'delete-file)
		vm-global-garbage-alist)
	  files (cdr files))))

(defun vm-save-folder-caches ()
  "Save any modified POP or IMAP folder cache, without asking.
Run from `kill-emacs-hook'.

A folder cache is VM's own file: the reader never chose it, never edited it,
and cannot tell one from another by name, the name being a hash of the
maildrop.  Leaving it modified hands them Emacs's own question about a path
that means nothing to them, and answering no throws away the read and deleted
flags of everything since the last save.  So it is written for them
\(emacs-vm/vm#798).

Only caches.  A folder the reader named themselves is theirs, and Emacs
asking about that one is right; this does not touch it.

`vm-expunge-before-save' is bound off: writing the flags out as Emacs is left
is one thing, deleting messages unasked as the frame goes away is another."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(when (and (memq major-mode '(vm-mode vm-virtual-mode))
		   vm-folder-access-method
		   buffer-file-name
		   (vm-cache-folder-name-p buffer-file-name)
		   (buffer-modified-p))
	  ;; Nothing here may stop Emacs from exiting.
	  (condition-case error-data
	      (let ((vm-expunge-before-save nil))
		(vm-save-folder))
	    (error
	     (vm-warn 0 2 "%s: could not be saved on exit: %s"
		      (buffer-name)
		      (error-message-string error-data)))))))))

(defun vm-garbage-collect-global ()
  "Carry out all the registered global garbage collection actions."
  (save-excursion
    (while vm-global-garbage-alist
      (condition-case nil
	  (funcall (cdr (car vm-global-garbage-alist))
		   (car (car vm-global-garbage-alist)))
	(error nil))
      (setq vm-global-garbage-alist (cdr vm-global-garbage-alist)))))

(defun vm-register-folder-garbage-files (files)
  "Add folder garbage collection actions to delete all of FILES."
  (vm-register-global-garbage-files files)
  (save-excursion
    (vm-select-folder-buffer)
    (while files
      (setq vm-folder-garbage-alist
	    (cons (cons (car files) 'delete-file)
		  vm-folder-garbage-alist)
	    files (cdr files)))))

(defun vm-register-folder-garbage (action garbage)
  "Add a folder garbage-collection action to carry out ACTION on
argument GARBAGE."
  (save-excursion
    (vm-select-folder-buffer)
    (setq vm-folder-garbage-alist
	  (cons (cons garbage action)
		vm-folder-garbage-alist))))

(defun vm-garbage-collect-folder ()
  "Carry out all the folder garbage-collection actions."
  (save-excursion
    (while vm-folder-garbage-alist
      (condition-case nil
	  (funcall (cdr (car vm-folder-garbage-alist))
		   (car (car vm-folder-garbage-alist)))
	(error nil))
      (setq vm-folder-garbage-alist (cdr vm-folder-garbage-alist)))))

(defun vm-register-fetched-message (m)
  "Register real message M as having been fetched into its folder
temporarily.  Such fetched messages are discarded before the
folder is saved."
  (save-current-buffer
    (set-buffer (vm-buffer-of m))
    ;; m should have retrieve=nil, i.e., already retrieved
    (vm-assert (vm-body-retrieved-of m))
    (let ((vm-folder-read-only nil)
	  (modified (buffer-modified-p)))
      (if (memq m vm-fetched-messages)
	  (progn
	    ;; at the moment, this case doesn't arise.  USR, 2010-06-11
	    ;; move m to the rear
	    (setq vm-fetched-messages
		  (delq m vm-fetched-messages))
	    (setq vm-fetched-messages	; add-to-list is no good on XEmacs
		  (nconc vm-fetched-messages (list m))))

	(if vm-external-fetched-message-limit
	    (while (>= vm-fetched-message-count
		       vm-external-fetched-message-limit)
	      (let ((mm (car vm-fetched-messages)))
		;; These tests should always come out true, but we are
		;; not confident.  A lot could have happened since the
		;; message was first loaded.
		(when (and (vm-body-retrieved-of mm)
			   (vm-body-to-be-discarded-of mm))
		    (vm-discard-real-message-body mm))
		(vm-unregister-fetched-message mm))))
	(setq vm-fetched-messages
	      (nconc vm-fetched-messages (list m)))
	(vm-increment vm-fetched-message-count)
	(vm-set-body-to-be-discarded-of m t)
	(vm-restore-buffer-modified-p
	 modified (vm-buffer-of m))))))

(defun vm-unregister-fetched-message (m)
  "Unregister a real message M as a fetched message.  If M was never
registered as a fetched message, then there is no effect."
  (save-current-buffer
    (set-buffer (vm-buffer-of m))
    (let ((vm-folder-read-only nil))
      ;; Only count down for a message that was actually on the list;
      ;; otherwise the count drifts below the length of the list and
      ;; vm-external-fetched-message-limit stops evicting when it should.
      (when (memq m vm-fetched-messages)
	(setq vm-fetched-messages (delq m vm-fetched-messages))
	(vm-decrement vm-fetched-message-count))
      (vm-set-body-to-be-discarded-of m nil))))

(defun vm-discard-fetched-messages ()
  "Discard the message bodies of all the fetched messages in the
current folder."
  (while vm-fetched-messages
    (let ((m (car vm-fetched-messages))
	  (vm-folder-read-only nil))
      (vm-discard-real-message-body m)
      (vm-set-body-to-be-discarded-of m nil))
    (setq vm-fetched-messages (cdr vm-fetched-messages)))
  (setq vm-fetched-message-count 0))

(defun vm-register-message-garbage-files (files)
  "Add message garbage collection actions to delete all of FILES."
  (vm-register-folder-garbage-files files)
  (save-excursion
    (vm-select-folder-buffer)
    (while files
      (setq vm-message-garbage-alist
	    (cons (cons (car files) 'delete-file)
		  vm-message-garbage-alist)
	    files (cdr files)))))

(defun vm-register-message-garbage (action garbage)
  "Add a message garbage-collection action to carry out ACTION on
argument GARBAGE."
  (vm-register-folder-garbage action garbage)
  (save-excursion
    (vm-select-folder-buffer)
    (setq vm-message-garbage-alist
	  (cons (cons garbage action)
		vm-message-garbage-alist))))

(defun vm-garbage-collect-message ()
  "Carry out all the folder garbage-collection actions."
  (save-excursion
    (while vm-message-garbage-alist
      (condition-case nil
	  (funcall (cdr (car vm-message-garbage-alist))
		   (car (car vm-message-garbage-alist)))
	(error nil))
      (setq vm-message-garbage-alist (cdr vm-message-garbage-alist)))))

(add-hook 'before-save-hook #'vm-write-file-hook)     ;FIXME: Buffer-local!
(add-hook 'find-file-hook #'vm-handle-file-recovery)  ;FIXME: Buffer-local!
(add-hook 'find-file-hook #'vm-handle-file-reversion) ;FIXME: Buffer-local!

(add-hook 'after-revert-hook #'vm-after-revert-buffer-hook) ;FIXME: Buffer-local!

(defun vm-message-can-be-external (m)
  "Check if the message M can be used in external (headers-only) mode."
  (and (eq (vm-message-access-method-of m) 'imap)
       (or (eq vm-enable-external-messages t)
	   (memq 'imap vm-enable-external-messages))
       ))

;;;###autoload
(defun vm-load-message (&optional count)
  "Load the message by retrieving its body from its
permanent location.  Currently this facility is only available for IMAP
folders.

With a prefix argument COUNT, the current message and the next 
COUNT - 1 messages are loaded.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
loaded.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are loaded, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are loaded."
  (interactive "p")
  (if (vm-interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (when (null count) (setq count 1))
  (let ((mlist (vm-select-operable-messages
		count (vm-interactive-p) "Load"))
	(n 0)
	;; bodies the driver was asked for, which are not loaded yet
	(asked 0)
	;; fetch-method
	m mm
	(need-refresh (not (vm-body-retrieved-of (vm-current-message)))))
    (setq count 0)
    (unwind-protect
	(save-excursion
	  (vm-inform 8 "Retrieving message body...")
	  ;; IMAP bodies go through the driver: one command for all of them,
	  ;; and nothing waits for the answer.  This is the reading path, so a
	  ;; body that has not arrived is no reason to hold Emacs; the fetch's
	  ;; own callback shows the message again when it lands.
	  ;;
	  ;; So they are not counted as loaded: they are on their way, and
	  ;; `asked' is what the reader is told about instead of `count'.
	  (let ((wanted (vm-imap-messages-to-fetch mlist)))
	    (when wanted
	      (if (vm-imap-net-load-message-bodies wanted)
		  (progn
		    (setq asked (length wanted))
		    (setq mlist (seq-remove
				 (lambda (m) (memq (vm-real-message-of m) wanted))
				 mlist)))
		;; nothing was started, so say so rather than reporting a load
		;; that did not happen; the messages keep their flag and can be
		;; asked for again
		(vm-warn 0 2 "No message body loaded: VM has no password for %s"
			 (buffer-name (vm-buffer-of (car wanted))))
		(setq mlist nil))))
	  (while mlist
	    (setq m (car mlist))
	    (setq mm (vm-real-message-of m))
	    (set-buffer (vm-buffer-of mm))
	    (if (vm-body-retrieved-of mm)
		(when (vm-body-to-be-discarded-of mm)
		  (vm-unregister-fetched-message mm)
		  (setq count (1+ count)))
	      ;; else retrieve the body
	      (setq n (1+ n))
	      (vm-inform 8 "Retrieving message body... %s" n)
	      (vm-retrieve-real-message-body mm)
	      (setq count (1+ count))
	      (when (> n 0)
		(vm-inform 8 "Retrieving message body... done")))
	    (setq mlist (cdr mlist)))
      (intern (buffer-name) vm-buffers-needing-display-update)
      ;; FIXME - is this needed?  Is it correct?
      (vm-display nil nil '(vm-load-message vm-refresh-message)
		  (list this-command))	
      (when (> count 0) (vm-mark-folder-modified-p))
      (vm-update-summary-and-mode-line))
      (when need-refresh
	(vm-preview-current-message))
      (cond
       ;; asked for and on their way: the fetch shows each message again as
       ;; its body lands, so saying they are loaded here would be wrong
       ((> asked 0)
	(vm-inform 5 "Retrieving %d message bod%s..."
		   asked (if (= asked 1) "y" "ies")))
       ((= count 1) (vm-inform 5 "Message body loaded"))
       (t (vm-inform 5 "%s message bodies loaded"
		     (if (= count 0) "No" count)))))
    ))

;;;###autoload
(cl-defun vm-retrieve-operable-messages (&optional count mlist
						   &key fail)
  "Retrieve the current \"operable\" messages from their
permanent locations for temporary use.  Currently this facility is
only available for IMAP folders.  If FAIL is non-nil then any errors
during retrieval cause failure.

If COUNT and MLIST or both nil, then the \"operable\" message is just
the current message, and it is retrieved.

If the optional argument MLIST is non-nil, then the messages in
MLIST are retrieved.  Otherwise, the following applies.

With a positive integer argument COUNT, the current message and
the next COUNT - 1 messages are retrieved.  A negative argument
means the current message and the previous |COUNT| - 1 messages
are retrieved.  If COUNT is 0, then all the messages in the current
folder are retrieved.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are retrieved, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are retrieved."
  (save-current-buffer
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (when (null count) (setq count 1))
    (let (;; (used-marks (eq last-command 'vm-next-command-uses-marks))
	  (vm-external-fetched-message-limit nil)
	  (n 0)
	  ;; fetch-method
	  m mm)
      (unless mlist
	(setq mlist (vm-select-operable-messages
		     count (vm-interactive-p) "Retrieve")))
      (save-excursion
	;; More than one to fetch from the same IMAP folder is one command,
	;; not one each (issue #185).  This caller must have the bodies, so it
	;; waits; see `vm-load-bodies-through-the-driver'.
	(let ((bunch (vm-messages-to-fetch-together mlist)))
	  (when bunch
	    (setq n (length bunch))
	    (vm-inform 8 "Retrieving %s message bodies..." n)
	    (set-buffer (vm-buffer-of (vm-real-message-of (car bunch))))
	    ;; `vm-messages-to-fetch-together' answers with the real messages
	    (dolist (mm (vm-load-bodies-through-the-driver bunch))
	      (vm-register-fetched-message mm))
	    (setq mlist (seq-remove
			 (lambda (m) (memq (vm-real-message-of m) bunch))
			 mlist))))
	(while mlist
	  (setq m (car mlist))
	  (setq mm (vm-real-message-of m))
	  (set-buffer (vm-buffer-of mm))
	  (when (vm-body-to-be-retrieved-of mm)
	    (setq n (1+ n))
	    (vm-inform 8 "Retrieving message body... %s" n)
	    (vm-retrieve-real-message-body mm :register t :fail fail))
	  (setq mlist (cdr mlist)))
	(when (> n 0)
	  (vm-inform 8 "Retrieving message body... done")
	  (intern (buffer-name) vm-buffers-needing-display-update)
	  (when (vm-interactive-p)
	    (vm-update-summary-and-mode-line))))
      )))

(declare-function vm-imap-net-load-message-bodies "vm-imap-net" (messages))
(declare-function vm-imap-net-wait "vm-imap-net" (&optional folder seconds))
(declare-function vm-body-retrieved-of "vm-message" (m))

(defun vm-load-bodies-through-the-driver (messages)
  "Fetch the bodies of MESSAGES on the driver and wait for them.
They must be in one folder.  Answers MESSAGES.  One command for all of them
rather than one each (issue #185), which is what the driver does with a list.

For a caller that must have the bodies in hand: it is saving or copying them,
and a message whose body has not arrived would be written as an empty one.  So
this waits, on the folder's own session rather than on a second connection
into it, and `accept-process-output' leaves C-g working.

Signals when the fetch cannot be started or does not finish."
  (when messages
    (let ((folder (vm-buffer-of (car messages))))
      (unless (vm-imap-net-load-message-bodies messages)
	(error "VM has no password for this maildrop yet"))
      ;; `vm-imap-server-timeout' nil means never time out, which is what it
      ;; says and what the blocking fetch did; `vm-imap-net-wait' would read
      ;; nil as its own default of 30 seconds.  C-g is the way out of a wait
      ;; with no deadline, and works because the wait is
      ;; `accept-process-output'.
      (unless (vm-imap-net-wait folder
				(or vm-imap-server-timeout most-positive-fixnum))
	(if vm-imap-server-timeout
	    (error (concat "The server did not answer in %s seconds; raise"
			   " vm-imap-server-timeout or set it to nil to wait")
		   vm-imap-server-timeout)
	  (error "The folder went away while its server was being waited for")))
      (let ((missing (seq-remove #'vm-body-retrieved-of messages)))
	(when missing
	  (error "The server did not send %d message bod%s"
		 (length missing) (if (cdr missing) "ies" "y"))))))
  messages)

(defun vm-load-body-through-the-driver (mm may-arrive-later)
  "Fetch MM's body on the driver, waiting unless MAY-ARRIVE-LATER.
Answers `settled' when the body is in the folder and the driver has already
made room for it, inserted it and settled it, and nil when it is not to be
waited for.

MAY-ARRIVE-LATER nil means the caller is saving or copying the message; see
`vm-load-bodies-through-the-driver'."
  (cond
   (may-arrive-later
    (unless (vm-imap-net-load-message-bodies (list mm))
      (error "VM has no password for this maildrop yet"))
    nil)
   (t
    (vm-load-bodies-through-the-driver (list mm))
    ;; `vm-imap-net-store-body' has made room, inserted and settled the body
    ;; already, so the caller must not do any of it again: settling a settled
    ;; message moves the markers and leaves the text empty.
    'settled)))

(cl-defun vm-retrieve-real-message-body (mm &key
					  (fetch nil) (register nil)
					  (fail nil) (may-arrive-later nil))
  "Retrieve the body of a real message MM from its external
source and insert it into the Folder buffer.  

If FETCH is non-nil, then the retrieval is for a temporary
message fetch.  If REGISTER is non-nil, then register it as a
fetched message If FAIL is non-nil, then fail for any errors
during retrieval.

Gives an error if unable to retrieve message."
  (if (not (eq (vm-message-access-method-of mm) 'imap))
      (message "External messages currently available only for imap folders.")
    (with-current-buffer (vm-buffer-of mm)
      (save-restriction
       (widen)
       (narrow-to-region (marker-position (vm-headers-of mm)) 
			 (marker-position (vm-text-end-of mm)))
       (let ((fetch-method (vm-message-access-method-of mm))
	     (vm-folder-read-only (and vm-folder-read-only (not fetch)))
	     (inhibit-read-only t)
	     (buffer-undo-list t)	; why this?  USR, 2010-06-11
	     (modified (buffer-modified-p))
	     (fetch-result nil))
	 (vm-make-room-for-message-body mm)
	 ;; MAY-ARRIVE-LATER does not wait: the message is shown without its
	 ;; body for now and the fetch's own callback shows it again when it
	 ;; lands.  A body wanted while a fetch is running is fetched when that
	 ;; one ends rather than by a second session writing this same folder.
	 ;;
	 ;; Without it the body has to be here when this returns -- the caller
	 ;; is saving the message, or copying it -- and a message whose body has
	 ;; not arrived would be written without one.  So this waits, on the
	 ;; same driver and the same session, the way folder-name completion
	 ;; does; `accept-process-output' leaves C-g working.
	 (condition-case err
	     (setq fetch-result
		   (if (eq fetch-method 'imap)
		       (vm-load-body-through-the-driver mm may-arrive-later)
		     (apply (intern (format "vm-fetch-%s-message" fetch-method))
			    mm nil)))
	   (error 
	    (if fail
		(error "Unable to load message; %s"
		       (error-message-string err))
	      (vm-warn 0 0 "Unable to load message; %s" 
		       (error-message-string err)))))
	 (when fetch-result
	   (unless (eq fetch-result 'settled)
	     (vm-settle-message-body mm modified))
	   (when register
	     (vm-register-fetched-message mm))))))))


(defun vm-messages-to-fetch-together (mlist)
  "The messages of MLIST whose bodies can be fetched in one IMAP command.
That is: those still to be retrieved, from the same IMAP folder, when there
is more than one of them.  Anything else is nil, and the caller falls back
to fetching one at a time -- which is what a single message, a POP folder or
a mixed list gets.  Issue #185."
  (let* ((wanted (seq-filter
		  (lambda (m)
		    (let ((mm (vm-real-message-of m)))
		      (and (vm-body-to-be-retrieved-of mm)
			   (eq (vm-message-access-method-of mm) 'imap))))
		  mlist))
	 (reals (delete-dups (mapcar #'vm-real-message-of wanted)))
	 (buffers (delete-dups (mapcar #'vm-buffer-of reals))))
    (and (cdr reals)			; more than one
	 (null (cdr buffers))		; all in the same folder
	 reals)))

(defun vm-imap-messages-to-fetch (mlist)
  "The messages of MLIST whose bodies are to be fetched from one IMAP folder.
Like `vm-messages-to-fetch-together', but a single message counts: the
driver sends one command either way, and there is no round trip to save by
treating one differently from four."
  (let* ((wanted (seq-filter
		  (lambda (m)
		    (let ((mm (vm-real-message-of m)))
		      (and (vm-body-to-be-retrieved-of mm)
			   (eq (vm-message-access-method-of mm) 'imap))))
		  mlist))
	 (reals (delete-dups (mapcar #'vm-real-message-of wanted)))
	 (buffers (delete-dups (mapcar #'vm-buffer-of reals))))
    (and reals
	 (null (cdr buffers))		; all in the same folder
	 reals)))

(defun vm-make-room-for-message-body (mm)
  "Make room in the folder for the body of MM, about to be retrieved.
The folder buffer must already be narrowed to MM.  Point is left where the
body goes, which is where the retrieval inserts it.

Split out of `vm-retrieve-real-message-body' so that a retrieval of several
bodies in one command can do this for each of them as its response arrives
-- only it knows where each message goes.  Issue #185."
  (goto-char (vm-text-of mm))
  ;; Check to see that we are at the right place
  (vm-assert (save-excursion (forward-line -1) (looking-at "\n")))
  (delete-region (point) (point-max)))

(defun vm-settle-message-boundaries (mm)
  "Put the boundary markers MM's body was inserted in front of back in order.
MM's text now ends at point-max, the folder being narrowed to MM.

A marker at the position text is inserted at stays in front of that text, so
every marker that sat at the end of MM's empty text region is now before the
body rather than after it: MM's own end, and the start of the message that
follows it.  The bytes are in the right order -- only the markers were left
behind, which is why this repairs them rather than the insertion being done
differently.

An mboxcl2 folder is where they coincide.  Its trailing message separator is
the empty string (`vm-trailing-message-separator'), so a message whose body
has not been retrieved ends exactly where the next one begins, and that is
also where the body goes.  A From_ folder has a newline between the two and
nothing here has anything to do.  Issue #737."
  (let ((end (point-max))
	(next (cadr (memq mm vm-message-list))))
    (set-marker (vm-text-end-of mm) end)
    (when (< (vm-end-of mm) end)
      (set-marker (vm-end-of mm) end))
    (when (and next (< (vm-start-of next) end))
      (set-marker (vm-start-of next) end))))

(defun vm-settle-message-body (mm modified)
  "Put the folder and MM in order after its body has been inserted.
MODIFIED is what `buffer-modified-p' said before the retrieval.  The other
half of `vm-make-room-for-message-body'.

Point is put back at the start of the body first.  The one-message path got
that for nothing, working inside a `save-excursion', but a fetch of several
bodies inserts into the folder from the process buffer and leaves point
after the text it inserted.  The `\n\n' search
below starts from point, so without this it found nothing and the delete
took the whole message out again."
  (goto-char (vm-text-of mm))
  ;; delete the new headers
  (delete-region
   (vm-text-of mm)
   (or (re-search-forward "\n\n" (point-max) t) (point-max)))
  (vm-assert (eq (point) (marker-position (vm-text-of mm))))
  ;; fix markers now
  (vm-settle-message-boundaries mm)
  (vm-assert (save-excursion (forward-line -1) (looking-at "\n")))
  ;; the headers were written with a length of zero, and in an mboxcl2 folder
  ;; that is where the next message starts
  (vm-set-content-length-of mm)
  ;; now care for the layout of the message
  (vm-set-mime-layout-of mm (vm-mime-parse-entity-safe mm))
  ;; update the message data
  (vm-set-body-to-be-retrieved-flag mm nil)
  (vm-set-body-to-be-discarded-flag mm nil)
  (vm-set-line-count-of mm nil)
  (vm-set-byte-count-of mm nil)
  ;; update the virtual messages
  (vm-update-virtual-messages mm :message-changing nil)
  (vm-restore-buffer-modified-p modified (vm-buffer-of mm)))

;;;###autoload
(defun vm-refresh-message ()
  "Reload the message body from its permanent location.  Currently
this facility is only available for IMAP folders."
  (interactive)
  (vm-unload-message 1 t)
  (vm-load-message)
  (vm-set-edited-flag-of (vm-current-message) nil)
  (intern (buffer-name) vm-buffers-needing-display-update)
  (let ((vm-preview-lines nil))
    (vm-present-current-message)))

;;;###autoload
(defun vm-unload-message (&optional count physical)
  "Unload the message body, i.e., delete it from the folder
buffer.  It can be retrieved again in future from its permanent
external location.  Currently this facility is only available for
IMAP folders.

With a prefix argument COUNT, the current message and the next 
COUNT - 1 messages are unloaded.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
unloaded.

When invoked on marked messages (via `vm-next-command-uses-marks'), only 
marked messages are unloaded, other messages are ignored.  If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in
the thread are unloaded.

If the optional argument PHYSICAL is non-nil, then the message is
physically discarded.  Otherwise, the discarding may be delayed until
the folder is saved."
  (interactive "p")
  (if (vm-interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (when (null count) 
    (setq count 1))
  (let ((mlist (vm-select-operable-messages
		count (vm-interactive-p) "Unload"))
	(buffer-undo-list t)
	m mm)
    (save-excursion
      (setq count 0)
      (while mlist
	(setq m (car mlist))
	(setq mm (vm-real-message-of m))
	(set-buffer (vm-buffer-of mm))
	(cond ((null (vm-message-can-be-external mm)))
	      ((vm-body-to-be-retrieved-of mm))
	      ((vm-body-to-be-discarded-of mm)
	       (when physical
		 (vm-discard-real-message-body mm)
		 (setq count (1+ count))))
	      (t
	       (if physical
		   (vm-discard-real-message-body mm)
		 ;; Register the message as fetched instead of actually
		 ;; discarding the message
		 (vm-register-fetched-message mm))
	       (setq count (1+ count))))
	(setq mlist (cdr mlist))))
    (if (= count 1) 
	(vm-inform 5 "Message body discarded")
      (vm-inform 5 "%s message bodies discarded" 
		 (if (= count 0) "No" count)))
    (vm-mark-folder-modified-p)
    (vm-update-summary-and-mode-line)
    ))

(defun vm-discard-real-message-body (mm)
  "Discard the real message body of MM from its Folder buffer."
  (if (not (vm-message-can-be-external mm))
      (vm-set-body-to-be-discarded-flag mm nil)
    (save-current-buffer
      (set-buffer (vm-buffer-of mm))
      (save-restriction
       (widen)
       (let ((inhibit-read-only t)
	     (modified (buffer-modified-p)))
	 (goto-char (vm-text-of mm))
	 ;; Check to see that we are at the right place
	 (if (or (bobp)
		 (save-excursion (forward-line -1) (looking-at "\n")))
	     (progn
	       (delete-region (point) (vm-text-end-of mm))
	       (vm-set-content-length-of mm)
	       (vm-set-mime-layout-of mm nil)
	       (vm-set-body-to-be-retrieved-flag mm t)
	       (vm-set-body-to-be-discarded-flag mm nil)
	       (vm-set-line-count-of mm nil)
	       (vm-update-virtual-messages mm :message-changing nil)
	       (vm-restore-buffer-modified-p modified (vm-buffer-of mm)))
	   (if (y-or-n-p
		(concat "VM internal error: "
			"headers of a message have been corrupted. "
			"Continue? "))
	       (progn
		 (vm-warn 1 5 (concat "The damaged message, with UID %s, "
				      "is left in the folder")
			  (vm-imap-uid-of mm))
		 (vm-set-body-to-be-discarded-flag mm nil))
	     (error "Aborted operation")))
	 )))))


;;; vm-folder.el ends here
