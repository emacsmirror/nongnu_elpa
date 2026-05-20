;;; jabber-roster.el --- roster management    -*- coding: utf-8; lexical-binding: t; -*-

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

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-private)
(require 'jabber-presence)
(require 'jabber-carbons)
(require 'keymap-popup)

(defgroup jabber-roster nil "Roster options."
  :group 'jabber)

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

(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean)

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

;;; Faces

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

;;; Declarations

(declare-function jabber-get-info "jabber-info.el" (jc to))
(declare-function jabber-blocking-block-jid "jabber-blocking.el" (jc jid))
(declare-function jabber-activity-switch-to "jabber-activity.el" (&optional jid-param))

(defvar *jabber-current-show*)          ; jabber.el
(defvar jabber-presence-strings)        ; jabber.el
(defvar jabber-activity-jids)           ; jabber-activity.el
(defvar jabber-muc--rooms)              ; jabber-muc.el

(defvar jabber-roster-debug nil
  "Debug roster operations.")

(defvar jabber-roster--scoped-connection nil
  "When non-nil, a connection object to scope roster views to.
Only contacts and rooms belonging to this connection are shown.")

;;; Popup interface

(defun jabber-roster--contacts ()
  "Return roster contacts, filtered by scope if active."
  (if jabber-roster--scoped-connection
      (plist-get (fsm-get-state-data jabber-roster--scoped-connection)
                 :roster)
    (jabber-concat-rosters)))

(defun jabber-roster--online-count ()
  "Return count of contacts with at least one connected resource."
  (cl-count-if (lambda (buddy) (get buddy 'connected))
               (jabber-roster--contacts)))

(defun jabber-roster--total-count ()
  "Return total number of contacts across all accounts."
  (length (jabber-roster--contacts)))

(defun jabber-roster--unread-count ()
  "Return number of JIDs with unread activity."
  (length (bound-and-true-p jabber-activity-jids)))

(defun jabber-roster--muc-count ()
  "Return number of currently joined MUC rooms."
  (if jabber-roster--scoped-connection
      (let ((count 0))
        (maphash (lambda (_room entries)
                   (when (assq jabber-roster--scoped-connection entries)
                     (cl-incf count)))
                 jabber-muc--rooms)
        count)
    (hash-table-count jabber-muc--rooms)))

(keymap-popup-define jabber-roster-presence-map
  "Set presence."
  :description (lambda ()
                 (format "Presence (current: %s)"
                         (propertize
                          (or (cdr (assoc *jabber-current-show*
                                          jabber-presence-strings))
                              "Offline")
                          'face 'keymap-popup-value)))
  "o" ("Online" jabber-send-default-presence)
  "a" ("Away" jabber-send-away-presence)
  "x" ("Extended away" jabber-send-xa-presence)
  "p" ("Custom" jabber-send-presence))

(keymap-popup-define jabber-roster-discovery-map
  "Discovery commands."
  "i" ("Disco items" jabber-get-disco-items)
  "I" ("Disco info" jabber-get-disco-info)
  "b" ("Browse" jabber-get-browse)
  "v" ("Client version" jabber-get-version))

(defvar jabber-roster--selected-jid nil
  "JID selected by `completing-read', used by action submenu.")

(keymap-popup-define jabber-roster-contact-action-map
  "Action for selected contact."
  :description (lambda ()
                 (format "Contact: %s"
                         (propertize (or jabber-roster--selected-jid "?")
                                     'face 'font-lock-constant-face)))
  "c" ("Chat" jabber-roster--action-chat)
  "i" ("Info" jabber-roster--action-info)
  "e" ("Edit" jabber-roster--action-edit)
  "d" ("Delete" jabber-roster--action-delete)
  "b" ("Block" jabber-roster--action-block))

(keymap-popup-define jabber-roster-popup-map
  "Jabber roster."
  :description (lambda ()
                 (if jabber-connections
                     (format "Jabber: %s"
                             (propertize
                              (if jabber-roster--scoped-connection
                                  (jabber-connection-bare-jid
                                   jabber-roster--scoped-connection)
                                (string-join
                                 (mapcar #'jabber-connection-bare-jid
                                         jabber-connections)
                                 ", "))
                              'face 'font-lock-constant-face))
                   "Jabber (not connected)"))
  :group "Contacts"
  "o" ((lambda ()
         (format "Online %s"
                 (propertize (number-to-string (jabber-roster--online-count))
                             'face 'success)))
       jabber-roster-chat-online
       :if (lambda () jabber-connections))
  "c" ((lambda ()
         (format "All contacts %s"
                 (propertize (number-to-string (jabber-roster--total-count))
                             'face 'keymap-popup-value)))
       jabber-roster-chat-any
       :if (lambda () jabber-connections))
  "u" ((lambda ()
         (format "Unread %s"
                 (propertize (number-to-string (jabber-roster--unread-count))
                             'face 'warning)))
       jabber-roster-chat-unread
       :if (lambda () (bound-and-true-p jabber-activity-jids)))
  :group "MUC"
  "m" ((lambda ()
         (format "Joined rooms %s"
                 (propertize (number-to-string (jabber-roster--muc-count))
                             'face 'keymap-popup-value)))
       jabber-roster-switch-muc
       :if (lambda () (> (jabber-roster--muc-count) 0)))
  "j" ("Join room" jabber-muc-join
       :if (lambda () jabber-connections))
  "B" ("Bookmarks" jabber-edit-bookmarks
       :if (lambda () jabber-connections))
  :group "Roster"
  "a" ("Add contact" jabber-roster-change
       :if (lambda () jabber-connections))
  "s" ("Subscribe" jabber-send-subscription-request
       :if (lambda () jabber-connections))
  :row
  :group "Presence"
  "p" ("Presence" :keymap jabber-roster-presence-map
       :if (lambda () jabber-connections))
  :group "Discovery"
  "d" ("Discovery" :keymap jabber-roster-discovery-map
       :if (lambda () jabber-connections))
  :group "Connection"
  "C" ("Connect" jabber-connect-all
       :if (lambda () (null jabber-connections)))
  "D" ("Disconnect all" jabber-disconnect
       :if (lambda () jabber-connections))
  "A" ((lambda ()
         (format "Accounts %s"
                 (propertize
                  (format "[%s]"
                          (if jabber-roster--scoped-connection
                              (jabber-connection-bare-jid
                               jabber-roster--scoped-connection)
                            "all"))
                  'face 'font-lock-constant-face)))
       jabber-roster-accounts
       :if (lambda () (cdr jabber-connections)))
  :group "OMEMO"
  "f" ("Fingerprints" jabber-omemo-show-fingerprints
       :if (lambda () jabber-connections)))

;;;###autoload
(defun jabber-roster-popup ()
  "Show the Jabber roster popup menu."
  (interactive)
  (keymap-popup jabber-roster-popup-map))

;;;###autoload
(defalias 'jabber-roster #'jabber-roster-popup)

(defun jabber-roster-chat-online ()
  "Select an online contact and open chat."
  (interactive)
  (let* ((online (cl-remove-if-not
                  (lambda (buddy) (get buddy 'connected))
                  (jabber-roster--contacts)))
         (jid (jabber-read-jid-completing "Chat with (online): "
                                          online t)))
    (when jid
      (let ((jc (jabber-roster--jc-for-jid jid)))
        (jabber-chat-with jc jid)))))

(defun jabber-roster-chat-any ()
  "Select any contact and show action submenu."
  (interactive)
  (let ((jid (jabber-read-jid-completing "Contact: " nil t)))
    (when jid
      (setq jabber-roster--selected-jid jid)
      (keymap-popup jabber-roster-contact-action-map))))

(defun jabber-roster-chat-unread ()
  "Select a JID with unread activity and switch to its buffer."
  (interactive)
  (if (null jabber-activity-jids)
      (message "No unread messages")
    (let ((jid (completing-read "Unread: " jabber-activity-jids nil t)))
      (when (and jid (not (string-empty-p jid)))
        (jabber-activity-switch-to jid)))))

(defun jabber-roster-switch-muc ()
  "Select a joined MUC room and switch to it."
  (interactive)
  (let* ((rooms (jabber-muc-active-rooms))
         (room (completing-read "Room: " rooms nil t)))
    (when (and room (not (string-empty-p room)))
      (jabber-muc-switch-to room))))

;;; Account management

(defvar jabber-roster--selected-account nil
  "Connection selected in the accounts menu.")

(keymap-popup-define jabber-roster-account-action-map
  "Account actions."
  :description (lambda ()
                 (if jabber-roster--selected-account
                     (jabber-connection-bare-jid jabber-roster--selected-account)
                   "Account"))
  "i" ((lambda ()
         (if (eq jabber-roster--scoped-connection
                 jabber-roster--selected-account)
             "Show all accounts"
           "Isolate"))
       jabber-roster--account-toggle-scope)
  "d" ("Disconnect" jabber-roster--account-disconnect))

(defconst jabber-roster--all-accounts-label "All accounts"
  "Synthetic entry in the account picker that clears scope.")

(defun jabber-roster-accounts ()
  "Select a connected account and show actions.
When the roster is scoped to a single account, an \"All accounts\"
entry is offered to clear the scope."
  (interactive)
  (let* ((accounts (mapcar (lambda (jc)
                             (cons (jabber-connection-bare-jid jc) jc))
                           jabber-connections))
         (candidates (if jabber-roster--scoped-connection
                         (cons jabber-roster--all-accounts-label
                               (mapcar #'car accounts))
                       (mapcar #'car accounts)))
         (choice (completing-read "Account: " candidates nil t)))
    (cond
     ((or (null choice) (string-empty-p choice)))
     ((string= choice jabber-roster--all-accounts-label)
      (setq jabber-roster--scoped-connection nil)
      (keymap-popup jabber-roster-popup-map))
     (t
      (setq jabber-roster--selected-account (cdr (assoc choice accounts)))
      (keymap-popup jabber-roster-account-action-map)))))

(defun jabber-roster--account-disconnect ()
  "Disconnect the selected account."
  (interactive)
  (when jabber-roster--selected-account
    (when (eq jabber-roster--scoped-connection jabber-roster--selected-account)
      (setq jabber-roster--scoped-connection nil))
    (jabber-disconnect-one jabber-roster--selected-account)
    (setq jabber-roster--selected-account nil)))

(defun jabber-roster--account-toggle-scope ()
  "Toggle roster scope to/from the selected account."
  (interactive)
  (setq jabber-roster--scoped-connection
        (if (eq jabber-roster--scoped-connection
                jabber-roster--selected-account)
            nil
          jabber-roster--selected-account))
  (setq jabber-roster--selected-account nil)
  (keymap-popup jabber-roster-popup-map))

(defun jabber-roster--clear-scope ()
  "Clear scope if the scoped connection is no longer active."
  (when (and jabber-roster--scoped-connection
             (not (memq jabber-roster--scoped-connection jabber-connections)))
    (setq jabber-roster--scoped-connection nil)))

(add-hook 'jabber-post-disconnect-hook #'jabber-roster--clear-scope)

(defun jabber-roster--jc-for-jid (jid)
  "Return the connection that has JID in its roster."
  (let ((sym (jabber-jid-symbol jid)))
    (or (cl-find-if
         (lambda (jc)
           (memq sym (plist-get (fsm-get-state-data jc) :roster)))
         jabber-connections)
        (car jabber-connections))))

(defun jabber-roster--action-chat ()
  "Open chat with the selected contact."
  (interactive)
  (when jabber-roster--selected-jid
    (let ((jc (jabber-roster--jc-for-jid jabber-roster--selected-jid)))
      (jabber-chat-with jc jabber-roster--selected-jid))))

(defun jabber-roster--action-info ()
  "Get info for the selected contact."
  (interactive)
  (when jabber-roster--selected-jid
    (let ((jc (jabber-roster--jc-for-jid jabber-roster--selected-jid)))
      (jabber-get-info jc jabber-roster--selected-jid))))

(defun jabber-roster--action-edit ()
  "Edit name and groups of the selected contact."
  (interactive)
  (when jabber-roster--selected-jid
    (let* ((jc (jabber-roster--jc-for-jid jabber-roster--selected-jid))
           (sym (jabber-jid-symbol jabber-roster--selected-jid))
           (name (get sym 'name))
           (groups (get sym 'groups))
           (all-groups
            (apply #'append
                   (mapcar (lambda (j) (get j 'groups))
                           (plist-get (fsm-get-state-data jc) :roster))))
           (new-name (jabber-read-with-input-method
                      (format "Name: (default `%s') " name) nil nil name))
           (new-groups (delete ""
                               (completing-read-multiple
                                (format "Groups, comma-separated: (default %s) "
                                        (if groups (string-join groups ",") "none"))
                                all-groups
                                nil nil nil
                                'jabber-roster-group-history
                                (string-join groups ",")
                                t))))
      (jabber-roster-change jc sym new-name new-groups))))

(defun jabber-roster--action-delete ()
  "Delete the selected contact from roster."
  (interactive)
  (when jabber-roster--selected-jid
    (when (yes-or-no-p (format "Delete %s from roster? "
                               jabber-roster--selected-jid))
      (let ((jc (jabber-roster--jc-for-jid jabber-roster--selected-jid)))
        (jabber-roster-delete jc jabber-roster--selected-jid)))))

(defun jabber-roster--action-block ()
  "Block the selected contact."
  (interactive)
  (when jabber-roster--selected-jid
    (let ((jc (jabber-roster--jc-for-jid jabber-roster--selected-jid)))
      (jabber-blocking-block-jid jc jabber-roster--selected-jid))))

;;; Roster data management

(defun jabber-roster--accounts-for-jid (jid)
  "Return list of connections that have JID in their roster."
  (let ((sym (jabber-jid-symbol jid)))
    (cl-remove-if-not
     (lambda (jc)
       (memq sym (plist-get (fsm-get-state-data jc) :roster)))
     jabber-connections)))

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
    (maphash (lambda (key val) (puthash key (nreverse val) hash)) hash)
    (setq all-groups (sort
		      (cl-remove-duplicates all-groups
					    :test #'string=)
		      #'string<))
    (plist-put state-data :roster-groups
	       (mapcar #'list all-groups))
    (plist-put state-data :roster-hash
	       hash)))

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

;;;###autoload
(defun jabber-roster-update (jc new-items changed-items deleted-items)
  "Update roster in memory.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.
JC is the Jabber connection."
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
	 (hash (plist-get (fsm-get-state-data jc) :roster-hash))
	 (all-groups (plist-get (fsm-get-state-data jc) :roster-groups)))

    (dolist (delete-this deleted-items)
      (setq roster (delq delete-this roster)))
    (setq roster (append new-items roster))
    (plist-put (fsm-get-state-data jc) :roster roster)

    (if (not hash)
	(jabber-roster-prepare-roster jc)

      (when jabber-roster-debug
	(message "update hash-based roster"))

      (dolist (delete-this (append deleted-items changed-items))
	(when jabber-roster-debug
	  (message "delete jid: %s" (symbol-name delete-this)))
	(dolist (group (mapcar #'car all-groups))
	  (puthash group
		   (delq delete-this (gethash group hash))
		   hash)))

      (dolist (insert-this (append changed-items new-items))
	(when jabber-roster-debug
	  (message "insert jid: %s" (symbol-name insert-this)))
	(dolist (group (or (get insert-this 'groups)
			   (list jabber-roster-default-group-name)))
	  (puthash group
		   (cons insert-this (gethash group hash))
		   hash)
	  (push (list group) all-groups)))

      (setq all-groups (sort
			(cl-remove-duplicates all-groups
					      :key #'car :test #'string=)
			(lambda (a b) (string< (car a) (car b)))))

      (plist-put (fsm-get-state-data jc) :roster-groups all-groups))))

;;; Private storage (group rolling state)

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
        (let* ((state-data (fsm-get-state-data jc))
               (roll-groups (plist-get state-data :roster-roll-groups)))
          (unless (cl-find group roll-groups :test #'string=)
            (plist-put state-data :roster-roll-groups
                       (cons group roll-groups))))))))

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

(provide 'jabber-roster)

;;; jabber-roster.el ends here
