;;; jabber-roster-menu.el --- Roster commands and popup menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Interactive roster commands and their cross-feature popup menus.

;;; Code:

(require 'cl-lib)
(require 'keymap-popup)
(require 'jabber-activity)
(require 'jabber-blocking)
(require 'jabber-bookmarks)
(require 'jabber-browse)
(require 'jabber-chat)
(require 'jabber-core)
(require 'jabber-disco)
(require 'jabber-info)
(require 'jabber-muc)
(require 'jabber-presence)
(require 'jabber-presence-display)
(require 'jabber-roster)
(require 'jabber-version)

(autoload 'jabber-omemo-show-fingerprints "jabber-omemo-trust" nil t)

(defvar jabber-current-show)             ; jabber.el
(defvar jabber-activity-jids)            ; jabber-activity.el
(defvar jabber-muc--rooms)               ; jabber-muc.el
(defvar jabber-roster-account-action-map)
(defvar jabber-roster-contact-action-map)
(defvar jabber-roster-discovery-map)
(defvar jabber-roster-popup-map)
(defvar jabber-roster-presence-map)

(defvar jabber-roster--scoped-connection nil
  "When non-nil, a connection object to scope roster views to.
Only contacts and rooms belonging to this connection are shown.")

;;; Roster commands

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
                          (or (cdr (assoc jabber-current-show
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

(defun jabber-roster--presence-menu ()
  "Show roster presence menu."
  (interactive)
  (keymap-popup jabber-roster-presence-map))

(defun jabber-roster--discovery-menu ()
  "Show roster discovery menu."
  (interactive)
  (keymap-popup jabber-roster-discovery-map))

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
       :if (lambda () (> (jabber-roster--muc-count) 0))
       :c-u "match by room name")
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
  "p" ("Presence" jabber-roster--presence-menu
       :if (lambda () jabber-connections))
  :group "Discovery"
  "d" ("Discovery" jabber-roster--discovery-menu
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

(defun jabber-roster--muc-room-name (room)
  "Return cached bookmark name for ROOM, or nil if absent."
  (when-let* ((jc (jabber-muc-connection room))
              (name (jabber-get-conference-data jc room nil :name)))
    (unless (or (string-empty-p name)
                (string= name room))
      name)))

(defun jabber-roster--muc-completion-entries ()
  "Return active MUC completion entries as (ROOM . NAME)."
  (mapcar (lambda (room)
            (cons room (jabber-roster--muc-room-name room)))
          (jabber-muc-active-rooms)))

(defun jabber-roster--muc-name-counts (entries)
  "Return hash table of cached-name counts from MUC ENTRIES."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries counts)
      (when-let* ((name (cdr entry)))
        (puthash name (1+ (gethash name counts 0)) counts)))))

(defun jabber-roster--muc-room-jids (entries)
  "Return hash table of active room JIDs from MUC ENTRIES."
  (let ((rooms (make-hash-table :test 'equal)))
    (dolist (entry entries rooms)
      (puthash (car entry) t rooms))))

(defun jabber-roster--muc-safe-name-candidate-p (name name-counts room-jids)
  "Return non-nil when NAME is safe as a completion candidate.
NAME-COUNTS records cached-name frequency.  ROOM-JIDS records
active room JIDs in the same completion set."
  (and name
       (= (gethash name name-counts 0) 1)
       (not (gethash name room-jids))))

(defun jabber-roster--muc-completion-candidate
    (entry name-counts room-jids use-names)
  "Return completion candidate for ENTRY using NAME-COUNTS and ROOM-JIDS.
When USE-NAMES is non-nil, use a cached name only if it is safe."
  (let ((room (car entry))
        (name (cdr entry)))
    (if (and use-names
             (jabber-roster--muc-safe-name-candidate-p
              name name-counts room-jids))
        name
      room)))

(defun jabber-roster--muc-completion-item
    (entry name-counts room-jids use-names)
  "Return a completion item plist for MUC ENTRY.
NAME-COUNTS records cached-name frequency.  ROOM-JIDS records
active room JIDs.  When USE-NAMES is non-nil, safe cached names
become candidates."
  (let* ((room (car entry))
         (name (cdr entry))
         (candidate (jabber-roster--muc-completion-candidate
                     entry name-counts room-jids use-names))
         (annotation (if (string= candidate room) name room)))
    (list :room room
          :candidate candidate
          :annotation annotation)))

(defun jabber-roster--muc-completion-items (entries use-names)
  "Return completion item plists for MUC ENTRIES.
When USE-NAMES is non-nil, safe cached names become candidates
and room JIDs become annotations."
  (let ((name-counts (jabber-roster--muc-name-counts entries))
        (room-jids (jabber-roster--muc-room-jids entries)))
    (mapcar (lambda (entry)
              (jabber-roster--muc-completion-item
               entry name-counts room-jids use-names))
            entries)))

(defun jabber-roster--muc-completion-table (items)
  "Return completion table for MUC ITEMS."
  (let ((candidates (mapcar (lambda (item)
                              (cons (plist-get item :candidate)
                                    (plist-get item :room)))
                            items))
        (annotations (make-hash-table :test 'equal)))
    (dolist (item items)
      (when-let* ((annotation (plist-get item :annotation)))
        (puthash (plist-get item :candidate) annotation annotations)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function
             . ,(lambda (candidate)
                  (when-let* ((annotation (gethash candidate annotations)))
                    (concat "  " annotation)))))
        (complete-with-action action candidates string pred)))))

(defun jabber-roster--muc-completion-normalize (candidate items)
  "Return CANDIDATE normalized to a room JID from MUC ITEMS."
  (or (cdr (assoc-string candidate
                         (mapcar (lambda (item)
                                   (cons (plist-get item :candidate)
                                         (plist-get item :room)))
                                 items)))
      (and (cl-find candidate items
                    :key (lambda (item) (plist-get item :room))
                    :test #'string=)
           candidate)))

(defun jabber-roster-switch-muc (use-names)
  "Select a joined MUC room and switch to it.
With prefix argument USE-NAMES, complete on unique cached room
names and annotate them with room JIDs."
  (interactive "P")
  (let* ((entries (jabber-roster--muc-completion-entries))
         (items (jabber-roster--muc-completion-items entries use-names))
         (candidate (completing-read "Room: "
                                     (jabber-roster--muc-completion-table items)
                                     nil t)))
    (when-let* ((room (and (not (string-empty-p candidate))
                           (jabber-roster--muc-completion-normalize
                            candidate items))))
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
  (or (cl-find-if (lambda (jc) (jabber-roster-contact-p jc jid))
                  jabber-connections)
      (car jabber-connections)))

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

(provide 'jabber-roster-menu)

;;; jabber-roster-menu.el ends here
