;;; jabber-roster.el --- displaying the roster    -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2009 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
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

(require 'cl-lib)
(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-menu)
(require 'jabber-private)
(require 'jabber-presence)
(require 'jabber-carbons)
(require 'format-spec)
(require 'ewoc)
(require 'transient)

(defgroup jabber-roster nil "roster display options"
  :group 'jabber)



(defcustom jabber-resource-line-format "     %r - %s (%S)"
  "The format specification of resource lines in the roster display.
These are displayed when `jabber-show-resources' permits it.

These fields are available:

%c   \"*\" if the contact is connected, or \" \" if not
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%p   Priority of this resource
%r   Name of this resource
%s   Availability of resource as string (\"Online\", \"Away\" etc)
%S   Status string specified by resource."
  :type 'string)

(defcustom jabber-roster-sort-functions
  '(jabber-roster-sort-by-status jabber-roster-sort-by-displayname)
  "Sort roster according to these criteria.

These functions should take two roster items A and B, and return:
<0 if A < B
0  if A = B
>0 if A > B."
  :type 'hook
  :options '(jabber-roster-sort-by-status
	     jabber-roster-sort-by-displayname
	     jabber-roster-sort-by-group))

(defcustom jabber-sort-order '("chat" "" "away" "dnd" "xa")
  "Sort by status in this order.  Anything not in list goes last.
Offline is represented as nil."
  :type '(repeat (restricted-sexp :match-alternatives (stringp nil))))

(defcustom jabber-show-resources 'sometimes
  "Show contacts' resources in roster?
This can be one of the following symbols:

nil       Never show resources
sometimes Show resources when there are more than one
always    Always show resources."
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always)))

(defcustom jabber-show-offline-contacts t
  "Show offline contacts in roster when non-nil."
  :type 'boolean)

(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean)



(defcustom jabber-roster-mode-hook nil
  "Hook run when entering Roster mode."
  :type 'hook)

(defcustom jabber-roster-default-group-name "Ungrouped"
  "Default group name for buddies without groups."
  :type 'string
  :get (lambda (var)
	 (let ((val (symbol-value var)))
	   (when (stringp val)
	     (set-text-properties 0 (length val) nil val))
	   val))
  :set (lambda (var val)
         (when (stringp val)
	   (set-text-properties 0 (length val) nil val))
         (custom-set-default var val)))

(defcustom jabber-roster-show-empty-group nil
  "Show empty groups in roster?"
  :type 'boolean)

(defface jabber-roster-user-online
  '((t :inherit success :weight bold))
  "Face for displaying online users.")

(defface jabber-roster-user-xa
  '((t :inherit shadow :slant italic))
  "Face for displaying extended away users.")

(defface jabber-roster-user-dnd
  '((t :inherit error :weight bold))
  "Face for displaying do not disturb users.")

(defface jabber-roster-user-away
  '((t :inherit warning :slant italic))
  "Face for displaying away users.")

(defface jabber-roster-user-chatty
  '((t :inherit success :weight bold :slant italic))
  "Face for displaying chatty users.")

(defface jabber-roster-user-error
  '((t :inherit error :slant italic))
  "Face for displaying users sending presence errors.")

(defface jabber-roster-user-offline
  '((t :inherit shadow :slant italic))
  "Face for displaying offline users.")

(defface jabber-roster-groupchat
  '((t :inherit font-lock-type-face))
  "Face for groupchat room names in the roster buffer.")

(defface jabber-roster-groupchat-nick
  '((t :inherit shadow))
  "Face for the user's nickname in groupchat roster entries.")

(defface jabber-roster-unread
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for roster entries with unread messages.")

(defun jabber-roster-separator ()
  "Return a propertized separator string for the roster buffer."
  (propertize (jabber-separator)
	       'cursor-intangible t))

(defvar jabber-roster-debug nil
  "Debug roster draw.")

(defvar-keymap jabber-roster-mode-map
  :parent (make-composed-keymap jabber-common-keymap special-mode-map)
  "TAB"       #'jabber-go-to-next-roster-item
  "S-TAB"     #'jabber-go-to-previous-roster-item
  "M-TAB"     #'jabber-go-to-previous-roster-item
  "<backtab>" #'jabber-go-to-previous-roster-item
  "RET"       #'jabber-roster-ret-action-at-point
  "C-k"       #'jabber-roster-delete-at-point
  "d"         #'jabber-roster-delete-at-point
  "D"         #'jabber-roster-delete-at-point
  "e"         #'jabber-roster-edit-action-at-point
  "s"         #'jabber-send-subscription-request
  "q"         #'bury-buffer
  "i"         #'jabber-get-disco-items
  "j"         #'jabber-muc-join
  "I"         #'jabber-get-disco-info
  "b"         #'jabber-get-browse
  "v"         #'jabber-get-version
  "a"         #'jabber-send-presence
  "g"         #'jabber-roster
  "h"         #'jabber-roster-menu
  "o"         #'jabber-roster-toggle-offline-display
  "H"         #'jabber-roster-menu
  "?"         #'jabber-roster-menu
  "f"         #'jabber-omemo-show-fingerprints)

;; Global reference declarations

(declare-function jabber-omemo-show-fingerprints "jabber-omemo-trust.el"
                  (jc))
(declare-function jabber-muc-read-my-nickname "jabber-muc.el"
                  (jc group &optional default))
(declare-function jabber-muc-join "jabber-muc.el"
                  (jc group nickname &optional popup))
(declare-function jabber-chat-with "jabber-chat.el"
                  (jc jid &optional other-window))
(declare-function jabber-disco-get-info "jabber-disco.el"
                  (jc jid node callback closure-data &optional force))
(declare-function jabber-get-version "jabber-version.el"  (jc to))
(declare-function jabber-get-browse "jabber-browse.el"  (jc to))
(declare-function jabber-get-disco-items "jabber-disco.el" (jc to &optional node))
(declare-function jabber-get-disco-info "jabber-disco.el"
                  (jc to &optional node))
(declare-function jabber-send-presence "jabber-presence.el"
                  (show status priority &optional jc))
(declare-function jabber-muc-switch-to "jabber-muc.el" (group))
(declare-function jabber-muc-get-buffer "jabber-muc.el" (group &optional jc))
(declare-function jabber-send-subscription-request "jabber-presence.el" (jc to &optional request))
(declare-function jabber-roster-delete-jid-at-point "jabber-presence.el" ())
(declare-function jabber-roster-delete-group-from-jids "jabber-presence.el" (jc jids group))
(declare-function jabber-roster-edit-group-from-jids "jabber-presence.el" (jc jids group))
(declare-function jabber-roster-change "jabber-presence.el" (jc jid name groups))
(declare-function jabber-edit-bookmarks "jabber-bookmarks.el" (jc))
(declare-function jabber-muc-joined-p "jabber-muc.el" (group &optional jc))
(declare-function jabber-muc-active-rooms "jabber-muc.el" ())
(declare-function jabber-muc-nickname "jabber-muc.el" (group &optional jc))
(declare-function jabber-muc-connection "jabber-muc.el" (group))
(declare-function jabber-muc-generation "jabber-muc.el" ())
(defvar jabber-connections)             ; jabber-core.el
(defvar jabber-roster-buffer)           ; jabber-core.el
(defvar *jabber-current-show*)          ; jabber.el
(defvar jabber-presence-strings)        ; jabber.el
(defvar *jabber-current-status*)        ; jabber.el
(defvar jabber-presence-faces)          ; jabber.el
(defvar jabber-activity-jids)           ; jabber-activity.el

(transient-define-prefix jabber-roster-menu ()
  "Jabber roster commands."
  [["Chat"
    ("RET" "Open chat buffer" jabber-roster-ret-action-at-point)
    ("e"   "Edit item" jabber-roster-edit-action-at-point)
    ("s"   "Subscribe" jabber-send-subscription-request)]
   ["Roster"
    ("d"   "Delete item" jabber-roster-delete-at-point)
    ("g"   "Refresh" jabber-roster)
    ("m"   "Jump to item" imenu)
    ("o"   "Toggle offline" jabber-roster-toggle-offline-display)]
   ["MUC & Presence"
    ("B"   "Bookmarks" jabber-edit-bookmarks)
    ("a"   "Send presence" jabber-send-presence)]
   ["Discovery"
    ("i"   "Disco items" jabber-get-disco-items)
    ("I"   "Disco info" jabber-get-disco-info)
    ("b"   "Browse" jabber-get-browse)
    ("v"   "Client version" jabber-get-version)]
   ["OMEMO"
    ("f"   "Fingerprints" jabber-omemo-show-fingerprints)]])

;;

(defun jabber-roster--accounts-for-jid (jid)
  "Return list of connections that have JID in their roster."
  (let ((sym (jabber-jid-symbol jid)))
    (cl-remove-if-not
     (lambda (jc)
       (memq sym (plist-get (fsm-get-state-data jc) :roster)))
     jabber-connections)))

(defun jabber-roster--choose-account (jid account-at-point)
  "Choose which account to use for JID.
If JID appears in more than one account's roster, prompt.
Otherwise return ACCOUNT-AT-POINT."
  (let ((accounts (jabber-roster--accounts-for-jid jid)))
    (if (cdr accounts)
        ;; Multiple accounts have this contact; prompt.
        (let* ((completions (mapcar (lambda (jc)
                                      (cons (jabber-connection-bare-jid jc) jc))
                                    accounts))
               (default (when account-at-point
                          (jabber-connection-bare-jid account-at-point)))
               (input (completing-read
                       (format "Account for %s (default %s): "
                               (jabber-jid-user jid) default)
                       completions nil t nil 'jabber-account-history
                       default)))
          (cdr (assoc input completions)))
      account-at-point)))

(defun jabber-roster-ret-action-at-point ()
  "Action for RET.
Before try to roll up/down group.  Eval `chat-with-jid-at-point' is no group at
point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account))
        (jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (cond
     ((and group-at-point account-at-point)
      (jabber-roster-roll-group account-at-point group-at-point))
     ;; Already-joined groupchat: switch directly to buffer.
     ((jabber-muc-joined-p jid-at-point)
      (jabber-muc-switch-to jid-at-point))
     ;; Contact or other JID: disco-check to decide chat vs MUC join.
     ((and jid-at-point account-at-point)
      (let ((jc (jabber-roster--choose-account jid-at-point account-at-point)))
        (jabber-disco-get-info
         jc (jabber-jid-user jid-at-point) nil
         #'jabber-roster-ret-action-at-point-1
         jid-at-point))))))

(defun jabber-roster-ret-action-at-point-1 (jc jid result)
  ;; If we get an error, assume it's a normal contact.
  (if (eq (car result) 'error)
      (jabber-chat-with jc jid)
    ;; Otherwise, let's check whether it has a groupchat identity.
    (let ((identities (car result)))
      (if (cl-find "conference" (if (sequencep identities) identities nil)
		   :key (lambda (i) (aref i 1))
		   :test #'string=)
	  ;; Yes!  Let's join it.
	  (jabber-muc-join jc jid
			   (jabber-muc-read-my-nickname jc jid t)
			   t)
	;; No.  Let's open a normal chat buffer.
	(jabber-chat-with jc jid)))))

(defun jabber-roster-mouse-2-action-at-point (e)
  "Action for mouse 2.
Before try to roll up/down group.  Eval `chat-with-jid-at-point' is no group
at point."
  (interactive "e")
  (mouse-set-point e)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(jabber-roster-roll-group account-at-point group-at-point)
      (call-interactively #'jabber-roster-menu))))

(defun jabber-roster-delete-at-point ()
  "Delete at point from roster.
Try to delete the group from all contacts.
Delete a jid if there is no group at point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-delete-group-from-jids account-at-point
						jids-with-group
						group-at-point))
      (jabber-roster-delete-jid-at-point))))

(defun jabber-roster-edit-action-at-point ()
  "Action for e.  Before try to edit group name.
Eval `jabber-roster-change' is no group at point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-edit-group-from-jids account-at-point
					      jids-with-group
					      group-at-point))
      (call-interactively 'jabber-roster-change))))

(defun jabber-roster-roll-group (jc group-name &optional set)
  "Roll up/down group in roster.
If optional SET is t, roll up group.
If SET is non-nil and not t, roll down group.
If SET is nil, toggle."
  (let* ((state-data (fsm-get-state-data jc))
	 (roll-groups (plist-get state-data :roster-roll-groups))
	 (rolled-p (cl-find group-name roll-groups :test #'string=))
	 (new-roll-groups
	  (cond
	   ;; Currently rolled up: unroll unless SET forces roll-up
	   ((and rolled-p (not (eq set t)))
	    (cl-remove group-name roll-groups :test #'string=))
	   ;; Currently unrolled: roll up on toggle or explicit roll-up
	   ((and (not rolled-p) (or (not set) (eq set t)))
	    (cons group-name roll-groups))
	   ;; No change needed
	   (t roll-groups))))
    (unless (equal roll-groups new-roll-groups)
      (plist-put
       state-data :roster-roll-groups
       new-roll-groups)
      (jabber-roster--refresh))))

(defun jabber-roster-imenu-create-index ()
  "Create an imenu index for the roster buffer."
  (let (contacts-index groupchats-index)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((group (get-text-property (point) 'jabber-group))
	      (jid (get-text-property (point) 'jabber-jid)))
	  (cond
	   (group
	    (push (cons group (point))
		  (if (string= group "Groupchats")
		      groupchats-index
		    contacts-index)))
	   (jid
	    (let ((entry (cons jid (point))))
	      (if (jabber-muc-joined-p jid)
		  (push entry groupchats-index)
		(push entry contacts-index))))))
	(forward-line 1)))
    (let (index)
      (when groupchats-index
	(push (cons "Groupchats" (nreverse groupchats-index)) index))
      (when contacts-index
	(push (cons "Contacts" (nreverse contacts-index)) index))
      index)))

(define-derived-mode jabber-roster-mode special-mode "jabber-roster"
  "Major mode for Jabber roster display.
Use the keybindings (mnemonic as Chat, Roster, Info, MUC, Service) to
bring up menus of actions.
\\{jabber-roster-mode-map}"
  :keymap jabber-roster-mode-map
  (setq display-line-numbers nil)
  (setq left-margin-width 1)
  (setq line-spacing 0.15)
  (cursor-intangible-mode 1)
  (setq left-fringe-width 0
	right-fringe-width 0)
  (setq imenu-create-index-function #'jabber-roster-imenu-create-index)
  ;; Re-apply buffer to window so margin/fringe changes take effect.
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (set-window-buffer win (current-buffer)))))

(defun jabber-sort-roster (jc)
  "Sort roster according to online status.
JC is the Jabber connection."
  (let ((state-data (fsm-get-state-data jc)))
    (dolist (group (plist-get state-data :roster-groups))
      (let ((group-name (car group)))
	(puthash group-name
		 (sort
		  (gethash group-name
			   (plist-get state-data :roster-hash))
		  #'jabber-roster-sort-items)
		 (plist-get state-data :roster-hash))))))

(defun jabber-roster-prepare-roster (jc)
  "Make a hash based roster.
JC is the Jabber connection."
  (let* ((state-data (fsm-get-state-data jc))
	 (hash (make-hash-table :test 'equal))
	 (buddies (plist-get state-data :roster))
	 (all-groups '()))
    (dolist (buddy buddies)
      (let ((groups (or (get buddy 'groups)
			(list jabber-roster-default-group-name))))
	(dolist (group groups)
	  (push group all-groups)
	  (puthash group
		   (cons buddy (gethash group hash))
		   hash))))
    ;; Reverse hash values to restore buddy order within each group
    (maphash (lambda (key val) (puthash key (nreverse val) hash)) hash)

    ;; remove duplicates name of group
    (setq all-groups (sort
		      (cl-remove-duplicates all-groups
					    :test #'string=)
		      #'string<))

    ;; put to state-data all-groups as list of list
    (plist-put state-data :roster-groups
	       (mapcar #'list all-groups))

    ;; put to state-data hash-roster
    (plist-put state-data :roster-hash
	       hash)))

(defun jabber-roster-sort-items (a b)
  "Sort roster items A and B according to `jabber-roster-sort-functions'.
Return t if A is less than B."
  (cl-dolist (fn jabber-roster-sort-functions)
    (let ((comparison (funcall fn a b)))
      (cond
       ((< comparison 0)
	(cl-return t))
       ((> comparison 0)
	(cl-return nil))))))

(defun jabber-roster-sort-by-status (a b)
  "Sort roster items by online status.
See `jabber-sort-order' for order used."
  (cl-flet ((order (item) (length (member (get item 'show) jabber-sort-order))))
    (let ((a-order (order a))
	  (b-order (order b)))
      ;; Note reversed test.  Items with longer X-order go first.
      (cond
       ((< a-order b-order)
	1)
       ((> a-order b-order)
	-1)
       (t
	0)))))

(defun jabber-roster-sort-by-displayname (a b)
  "Sort roster items by displayed name."
  (let ((a-name (jabber-jid-displayname a))
	(b-name (jabber-jid-displayname b)))
    (cond
     ((string-lessp a-name b-name) -1)
     ((string= a-name b-name) 0)
     (t 1))))

(defun jabber-roster-sort-by-group (a b)
  "Sort roster items by group membership."
  (cl-flet ((first-group (item) (or (car (get item 'groups)) "")))
    (let ((a-group (first-group a))
	  (b-group (first-group b)))
      (cond
       ((string-lessp a-group b-group) -1)
       ((string= a-group b-group) 0)
       (t 1)))))

(defun jabber-fix-status (status)
  "Make status strings more readable."
  (when status
    (when (string-match "\n+$" status)
      (setq status (replace-match "" t t status)))
    (when jabber-remove-newlines
      (while (string-match "\n" status)
	(setq status (replace-match " " t t status))))
    status))

(defvar jabber-roster-ewoc nil
  "Ewoc displaying the roster.
There is only one; we don't rely on buffer-local variables or
such.")

(defun jabber-roster-filter-display (buddies)
  "Filter BUDDIES for items to be displayed in the roster."
  (cl-remove-if-not (lambda (buddy) (or jabber-show-offline-contacts
				   (get buddy 'connected)))
		    buddies))

(defun jabber-roster-toggle-offline-display ()
  "Toggle display of offline contacts.
To change this permanently, customize the `jabber-show-offline-contacts'."
  (interactive)
  (setq jabber-show-offline-contacts
	(not jabber-show-offline-contacts))
  (jabber-roster--refresh))

(defun jabber-roster--insert-status ()
  "Insert the connection status header into the current buffer."
  (if (null jabber-connections)
      (insert "\nNot connected\n")
    (let ((show (cdr (assoc *jabber-current-show* jabber-presence-strings)))
	  (accounts (mapconcat
		     (lambda (jc)
		       (concat (plist-get (fsm-get-state-data jc) :username)
			       "@"
			       (plist-get (fsm-get-state-data jc) :server)))
		     jabber-connections ", ")))
      (insert (propertize (concat "Connected"
				  (unless (string= show "Online")
				    (format " [%s]" show))
				  ": " accounts)
			  'face 'shadow)
	      "\n" (jabber-roster-separator) "\n"))))

(defun jabber-roster--merged-groups ()
  "Return a sorted list of unique group names across all connections."
  (sort (cl-remove-duplicates
	 (cl-mapcan
	  (lambda (jc)
	    (mapcar #'car
		    (plist-get (fsm-get-state-data jc) :roster-groups)))
	  jabber-connections)
	 :test #'string=)
	#'string<))

(defun jabber-roster--group-buddies (group-name)
  "Collect buddies for GROUP-NAME across all connections.
Return (BUDDIES . BUDDY-JC-MAP) where BUDDIES is a filtered,
sorted list and BUDDY-JC-MAP maps buddy names to connections."
  (let ((buddies '())
	(buddy-jc-map (make-hash-table :test 'equal)))
    (dolist (jc jabber-connections)
      (let ((hash (plist-get (fsm-get-state-data jc) :roster-hash)))
	(when hash
	  (dolist (buddy (gethash group-name hash))
	    (unless (gethash (symbol-name buddy) buddy-jc-map)
	      (puthash (symbol-name buddy) jc buddy-jc-map)
	      (push buddy buddies))))))
    (cons (jabber-roster-filter-display
	   (sort (nreverse buddies) #'jabber-roster-sort-items))
	  buddy-jc-map)))

(defun jabber-roster--group-rolled-p (group-name)
  "Return non-nil if GROUP-NAME is rolled up in any connection."
  (cl-some (lambda (jc)
	     (cl-find group-name
		      (plist-get (fsm-get-state-data jc) :roster-roll-groups)
		      :test #'string=))
	   jabber-connections))

(defun jabber-roster--insert-contacts ()
  "Insert the contact roster using an ewoc."
  (dolist (jc jabber-connections)
    (unless (plist-get (fsm-get-state-data jc) :roster-hash)
      (jabber-roster-prepare-roster jc))
    (jabber-sort-roster jc))
  (let* ((first-jc (car jabber-connections))
	 (ewoc (ewoc-create
		(lambda (data)
		  (let* ((group (car data))
			 (group-name (car group))
			 (buddy (cadr data))
			 (jc (nth 2 data)))
		    (jabber-roster--display-entry (or jc first-jc) group-name buddy)))
		""
		(jabber-roster-separator))))
    (dolist (jc jabber-connections)
      (plist-put (fsm-get-state-data jc) :roster-ewoc ewoc))
    (insert (propertize "Contacts"
                        'face 'jabber-title)
            "\n")
    (dolist (group-name (jabber-roster--merged-groups))
      (let* ((result (jabber-roster--group-buddies group-name))
	     (buddies (car result))
	     (buddy-jc-map (cdr result)))
	(when (or jabber-roster-show-empty-group (> (length buddies) 0))
	  (let ((group (list group-name))
		(rolled (jabber-roster--group-rolled-p group-name)))
	    (let ((group-node (ewoc-enter-last ewoc (list group nil first-jc))))
	      (unless rolled
		(dolist (buddy (reverse buddies))
		  (ewoc-enter-after ewoc group-node
				    (list group buddy
					  (gethash (symbol-name buddy)
						   buddy-jc-map))))))))))
    (goto-char (point-max))
    (insert "\n")))

(defun jabber-roster--insert-groupchats ()
  "Insert the active groupchats section."
  (let ((rooms (sort (jabber-muc-active-rooms) #'string<)))
    (when rooms
      (insert (propertize "Groupchats"
			  'face 'jabber-title
			  'jabber-group "Groupchats")
	      "\n")
      (dolist (room-jid rooms)
	(let* ((nick (jabber-muc-nickname room-jid))
	       (room-name (or (jabber-jid-user room-jid) room-jid))
	       (gc-jc (or (jabber-muc-connection room-jid)
			  (car jabber-connections)))
	       (unread (member room-jid
			       (bound-and-true-p jabber-activity-jids)))
	       (room-part (propertize (format "  %s" room-name)
				      'face (if unread
						'jabber-roster-unread
					      'jabber-roster-groupchat)))
	       (nick-part (propertize (format " (%s)" nick)
				      'face 'jabber-roster-groupchat-nick))
	       (line (concat room-part nick-part)))
	  (add-text-properties 0 (length line)
			       (list 'jabber-jid room-jid
				     'jabber-account gc-jc)
			       line)
	  (insert line "\n"))))))

(defun jabber-roster--restore-point (line column window window-line)
  "Restore cursor to LINE and COLUMN, and WINDOW scroll to WINDOW-LINE."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column)
  (when window
    (set-window-point window (point))
    (when window-line
      (set-window-start window
			(save-excursion
			  (forward-line (- window-line))
			  (point))
			t))))

(defun jabber-roster--refresh ()
  "Refresh the roster buffer contents without switching to it."
  (let ((buffer (get-buffer-create jabber-roster-buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'jabber-roster-mode)
	(jabber-roster-mode))
      (let* ((inhibit-read-only t)
	     (current-line (line-number-at-pos))
	     (current-column (current-column))
	     (window (get-buffer-window buffer))
	     (window-line (when window
			    (count-lines (min (window-start window) (point))
					 (max (window-start window) (point))))))
	(erase-buffer)
	(setq jabber-roster-ewoc nil)
	(setq header-line-format
	      (propertize " Jabber roster" 'face '(:weight bold)))
	(jabber-roster--insert-status)
	(when jabber-connections
	  (jabber-roster--insert-contacts))
	(jabber-roster--insert-groupchats)
	(jabber-roster--restore-point current-line current-column
				      window window-line)))))

;;;###autoload
(defun jabber-roster ()
  "Switch to the roster buffer and refresh it."
  (interactive)
  (jabber-roster--refresh)
  (pop-to-buffer-same-window jabber-roster-buffer)
  (when (called-interactively-p 'interactive)
    (message "Press %s for commands" (propertize "h" 'face 'help-key-binding))))

(defun jabber-roster--format-resource (buddy resource jc)
  "Return a propertized string for RESOURCE of BUDDY.
JC is the Jabber connection."
  (let* ((res-name (car resource))
	 (res-data (cdr resource))
	 (resource-str
	  (format-spec jabber-resource-line-format
		       (list
			(cons ?c "*")
			(cons ?n (if (> (length (get buddy 'name)) 0)
				     (get buddy 'name)
				   (symbol-name buddy)))
			(cons ?j (symbol-name buddy))
			(cons ?r (if (> (length res-name) 0) res-name "empty"))
			(cons ?s (or (cdr (assoc (plist-get res-data 'show)
						 jabber-presence-strings))
				     (plist-get res-data 'show)))
			(cons ?S (if (plist-get res-data 'status)
				     (jabber-fix-status (plist-get res-data 'status))
				   ""))
			(cons ?p (number-to-string
				  (plist-get res-data 'priority)))))))
    (add-text-properties 0 (length resource-str)
			 (list 'face (or (cdr (assoc (plist-get res-data 'show)
						     jabber-presence-faces))
					 'jabber-roster-user-online)
			       'jabber-jid (format "%s/%s" (symbol-name buddy) res-name)
			       'jabber-account jc)
			 resource-str)
    resource-str))

(defun jabber-roster--display-entry (jc group-name buddy)
  "Format and insert a roster entry for BUDDY at point.
BUDDY is a JID symbol. JC is the Jabber connection."
  (if buddy
      (let* ((bare-jid (symbol-name buddy))
             (nick (get buddy 'name))
             (show (or (cdr (assoc (get buddy 'show) jabber-presence-strings))
                       (get buddy 'show)))
             (unread (member bare-jid
                             (bound-and-true-p jabber-activity-jids)))
             (face (if unread
                       'jabber-roster-unread
                     (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                         'jabber-roster-user-online)))
             (props (list 'jabber-jid bare-jid
                          'jabber-account jc)))
        (insert (apply #'propertize (concat "    " bare-jid) 'face face props))
        (when (and nick (> (length nick) 0))
          (insert (propertize (format " (%s)" nick) 'face 'shadow)))
        (when show
          (insert (propertize (concat " " show) 'face 'shadow)))

	(when (or (eq jabber-show-resources 'always)
		  (and (eq jabber-show-resources 'sometimes)
		       (> (jabber-count-connected-resources buddy) 1)))
	  (dolist (resource (get buddy 'resources))
	    (when (plist-get (cdr resource) 'connected)
	      (insert "\n" (jabber-roster--format-resource buddy resource jc))))))
    (let* ((group-name (or group-name
			   jabber-roster-default-group-name))
	    (line (concat "  " group-name)))
      (add-text-properties 0
			   (length line)
			   (list
			    'face 'jabber-title
			    'jabber-group group-name
			    'jabber-account jc)
			   line)
      (insert line))))

;;;###autoload
(defun jabber-roster-update (jc new-items changed-items deleted-items)
  "Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.
JC is the Jabber connection."
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
	 (hash (plist-get (fsm-get-state-data jc) :roster-hash))
	 (all-groups (plist-get (fsm-get-state-data jc) :roster-groups)))

    ;; fix a old-roster
    (dolist (delete-this deleted-items)
      (setq roster (delq delete-this roster)))
    (setq roster (append new-items roster))
    (plist-put (fsm-get-state-data jc) :roster roster)

    ;; update a hash-roster
    (if (not hash)
	(jabber-roster-prepare-roster jc)

      (when jabber-roster-debug
	(message "update hash-based roster"))

      ;; delete items
      (dolist (delete-this (append deleted-items changed-items))
	(when jabber-roster-debug
	  (message "delete jid: %s" (symbol-name delete-this)))
	(dolist (group (mapcar #'car all-groups))
	  (when jabber-roster-debug
	    (message "try to delete jid: %s from group %s"
		     (symbol-name delete-this) group))
	  (puthash group
		   (delq delete-this (gethash group hash))
		   hash)))

      ;; insert changed-items
      (dolist (insert-this (append changed-items new-items))
	(when jabber-roster-debug
	  (message "insert jid: %s" (symbol-name insert-this)))
	(dolist (group (or (get insert-this 'groups)
			   (list jabber-roster-default-group-name)))
	  (when jabber-roster-debug
	    (message "insert jid: %s to group %s" (symbol-name insert-this) group))
	  (puthash group
		   (cons insert-this (gethash group hash))
		   hash)
	  (push (list group) all-groups)))

      (when jabber-roster-debug
	(message "remove duplicates from new group"))
      (setq all-groups (sort
			(cl-remove-duplicates all-groups
					      :key #'car :test #'string=)
			(lambda (a b) (string< (car a) (car b)))))

      (plist-put (fsm-get-state-data jc) :roster-groups all-groups))

    (when jabber-roster-debug
      (message "re display roster"))

    ;; recreate roster buffer
    (jabber-roster--refresh)))

(defun jabber-next-property (&optional prev)
  "Return position of next property appearence or nil if there is none.
If optional PREV is non-nil, return position of previous property appearence."
  (let ((pos (point))
        (found nil)
        (nextprev (if prev 'previous-single-property-change
                    'next-single-property-change)))
    (while (not found)
      (setq pos
            (let ((jid (funcall nextprev pos 'jabber-jid))
                  (group (funcall nextprev pos 'jabber-group)))
              (cond
               ((not jid) group)
               ((not group) jid)
               (t (funcall (if prev 'max 'min) jid group)))))
      (if (not pos)
          (setq found t)
        (setq found (or (get-text-property pos 'jabber-jid)
                        (get-text-property pos 'jabber-group)))))
    pos))

(defun jabber-go-to-next-roster-item ()
  "Move the cursor to the next jid/group in the buffer."
  (interactive)
  (let* ((next (jabber-next-property))
         (next (if (not next)
                   (progn (goto-char (point-min))
                          (jabber-next-property)) next)))
    (if next (goto-char next)
      (goto-char (point-min)))))

(defun jabber-go-to-previous-roster-item ()
  "Move the cursor to the previous jid/group in the buffer."
  (interactive)
  (let* ((previous (jabber-next-property 'prev))
         (previous (if (not previous)
                       (progn (goto-char (point-max))
                              (jabber-next-property 'prev)) previous)))
    (if previous (goto-char previous)
      (goto-char (point-max)))))

(defun jabber-roster-restore-groups (jc)
  "Restore roster's groups rolling state from private storage.
JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-private-get jc 'roster "emacs-jabber"
                      'jabber-roster-restore-groups-1 'ignore))

(defun jabber-roster-restore-groups-1 (jc xml-data)
  "Parse roster groups and restore rolling state.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (when (string= (jabber-xml-get-xmlns xml-data) "emacs-jabber")
    (let* ((data (car (last xml-data)))
           (groups (if (stringp data) (split-string data "\n") nil)))
      (dolist (group groups)
        (jabber-roster-roll-group jc group t)))))

(defun jabber-roster-save-groups ()
  "Save roster's groups rolling state in private storage."
  (interactive)
  (dolist (jc jabber-connections)
    (let* ((groups (plist-get (fsm-get-state-data jc) :roster-roll-groups))
           (roll-groups
            (if groups
                (mapconcat (lambda (a) (substring-no-properties a)) groups "\n")
              "")))
      (jabber-private-set jc
                          `(roster ((xmlns . "emacs-jabber"))
                                   ,roll-groups)
                          'jabber-report-success "Roster groups saved"
                          'jabber-report-success "Failed to save roster groups"))))

(defvar jabber-roster--last-muc-generation 0
  "Last seen `jabber-muc--generation' value.")

(defvar jabber-roster--needs-refresh nil
  "Non-nil when the roster buffer needs a redraw.")

(defun jabber-roster--refresh-if-visible ()
  "Refresh roster only if its buffer is visible, otherwise defer."
  (let ((buf (get-buffer jabber-roster-buffer)))
    (when buf
      (if (get-buffer-window buf 'visible)
	  (progn
	    (jabber-roster--refresh)
	    (setq jabber-roster--needs-refresh nil))
	(setq jabber-roster--needs-refresh t)))))

(defun jabber-roster--on-window-state-change (&rest _)
  "Refresh roster when it becomes visible and needs it."
  (when (and jabber-roster--needs-refresh
	     (let ((buf (get-buffer jabber-roster-buffer)))
	       (and buf (get-buffer-window buf 'visible))))
    (setq jabber-roster--needs-refresh nil)
    (jabber-roster--refresh)))

(add-hook 'window-state-change-hook #'jabber-roster--on-window-state-change)

(defun jabber-roster--maybe-refresh-on-muc (_jc _xml-data)
  "Refresh roster when groupchat list changes."
  (unless (= (jabber-muc-generation) jabber-roster--last-muc-generation)
    (setq jabber-roster--last-muc-generation (jabber-muc-generation))
    (jabber-roster--refresh-if-visible)))

(with-eval-after-load 'jabber-activity
  (add-hook 'jabber-activity-update-hook
	    #'jabber-roster--refresh-if-visible))

;; MUC join/leave is signaled via presence stanzas, so we hook into
;; the presence chain.  The handler short-circuits via `equal' check
;; and only triggers a refresh when the groupchat list actually changes.
(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-presence-chain
                    #'jabber-roster--maybe-refresh-on-muc 10))

(provide 'jabber-roster)

;;; jabber-roster.el ends here
