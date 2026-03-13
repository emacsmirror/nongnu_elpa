;;; jabber-menu.el --- menu definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

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

(eval-when-compile (require 'cl-lib))
(require 'jabber-util)
(require 'transient)
(require 'wid-edit)

;;;###autoload
(defvar jabber-menu
  (let ((map (make-sparse-keymap "jabber-menu")))
    (define-key-after map
      [jabber-menu-connect]
      '("Connect" . jabber-connect-all))

    (define-key-after map
      [jabber-menu-disconnect]
      '(menu-item "Disconnect" jabber-disconnect
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-status]
      `(menu-item "Set Status" ,(make-sparse-keymap "set-status")
		  :enable (bound-and-true-p jabber-connections)))

    (define-key map
      [jabber-menu-status jabber-menu-status-chat]
      '(menu-item
	"Chatty"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "chat"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "chat")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-dnd]
      '(menu-item
	"Do not Disturb"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "dnd"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "dnd")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-xa]
      '(menu-item "Extended Away" jabber-send-xa-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "xa")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-away]
      '(menu-item "Away" jabber-send-away-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "away")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-online]
      '(menu-item "Online" jabber-send-default-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "")))))

    (define-key-after map
      [separator]
      '(menu-item "--"))

    (define-key-after map
      [jabber-menu-chat-with]
      '(menu-item "Chat with..." jabber-chat-with
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-nextmsg]
      '(menu-item "Next unread message" jabber-activity-switch-to
		  :enable (bound-and-true-p jabber-activity-jids)))

    (define-key-after map
      [jabber-menu-send-subscription-request]
      '(menu-item "Send subscription request" jabber-send-subscription-request
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-roster]
      '("Switch to roster" . jabber-roster))

    (define-key-after map
      [separator2]
      '(menu-item "--"))

    (define-key-after map
      [jabber-menu-customize]
      '("Customize" . jabber-customize))

    (define-key-after map
      [jabber-menu-info]
      '("Help" . jabber-info))

    map))

;;;###autoload
(defcustom jabber-display-menu 'maybe
  "Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if jabber.el is installed under `package-user-dir', or
if any of `jabber-account-list' or `jabber-connections' is non-nil."
  :group 'jabber
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When installed by user, or when any accounts have been configured or connected" maybe)))

(defun jabber-menu (&optional remove)
  "Put \"Jabber\" menu on menubar.
With prefix argument, remove it."
  (interactive "P")
  (setq jabber-display-menu (if remove nil t))
  (force-mode-line-update))
(make-obsolete 'jabber-menu "set the variable `jabber-display-menu' instead."
               "2008")

;;;###autoload
(define-key-after (lookup-key global-map [menu-bar])
  [jabber-menu]
  (list 'menu-item "Jabber" jabber-menu
	:visible
        '(or (eq jabber-display-menu t)
             (and (eq jabber-display-menu 'maybe)
                  (or (bound-and-true-p jabber-account-list)
                      (bound-and-true-p jabber-connections))))))

(defvar jabber-jid-chat-menu nil
  "Menu items for chat menu.")

(defvar jabber-jid-info-menu nil
  "Menu item for info menu.")

(defvar jabber-jid-roster-menu nil
  "Menu items for roster menu.")

(defvar jabber-jid-muc-menu nil
  "Menu items for MUC menu.")

(defvar jabber-jid-service-menu nil
  "Menu items for service menu.")

;; Global reference declarations for transient suffix commands

(declare-function jabber-chat-with "jabber-chat.el" (jc jid &optional other-window))
(declare-function jabber-compose "jabber-compose.el" ())
(declare-function jabber-chat-display-more-backlog "jabber-chat.el" (jc &optional before))
(declare-function jabber-roster-change "jabber-presence.el" (jc jid name groups))
(declare-function jabber-send-subscription-request "jabber-presence.el" (jc to &optional request))
(declare-function jabber-roster-delete "jabber-presence.el" (jc jid))
(declare-function jabber-get-disco-items "jabber-disco.el" (jc to &optional node))
(declare-function jabber-get-disco-info "jabber-disco.el" (jc jid node callback closure-data &optional force))
(declare-function jabber-get-browse "jabber-browse.el" (jc to))
(declare-function jabber-get-version "jabber-version.el" (jc to))
(declare-function jabber-ping "jabber-ping.el" (jc to))
(declare-function jabber-get-time "jabber-time.el" (jc to))
(declare-function jabber-vcard-get "jabber-vcard.el" (jc jid))
(declare-function jabber-muc-join "jabber-muc.el" (jc group nickname &optional popup))
(declare-function jabber-muc-leave "jabber-muc.el" (jc group))
(declare-function jabber-muc-nick "jabber-muc.el" (jc group nickname &optional popup))
(declare-function jabber-muc-set-topic "jabber-muc.el" (jc group topic))
(declare-function jabber-muc-invite "jabber-muc.el" (jc jid group reason))
(declare-function jabber-muc-names "jabber-muc.el" (jc group))
(declare-function jabber-muc-get-config "jabber-muc.el" (jc group))
(declare-function jabber-muc-set-role "jabber-muc.el" (jc group nickname role reason))
(declare-function jabber-muc-set-affiliation "jabber-muc.el" (jc group nickname-or-jid nickname-p affiliation reason))
(declare-function jabber-muc-private "jabber-muc.el" (jc group nickname))
(declare-function jabber-muc-vcard-get "jabber-muc.el" (jc group nickname))
(declare-function jabber-get-register "jabber-register.el" (jc to))
(declare-function jabber-get-search "jabber-search.el" (jc to))
(declare-function jabber-ahc-execute-command "jabber-ahc.el" (jc to node))
(declare-function jabber-ahc-get-list "jabber-ahc.el" (jc to))
(declare-function jabber-enable-carbons "jabber-carbons.el" (jc))

;;;###autoload
(transient-define-prefix jabber-chat-menu ()
  "Jabber chat commands."
  [["Chat"
    ("c" "Start chat" jabber-chat-with)
    ("m" "Compose message" jabber-compose)
    ("b" "Display more context" jabber-chat-display-more-backlog)]])

;;;###autoload
(transient-define-prefix jabber-roster-context-menu ()
  "Jabber roster commands."
  [["Roster"
    ("a" "Add/modify contact" jabber-roster-change)
    ("s" "Subscribe" jabber-send-subscription-request)
    ("d" "Delete roster entry" jabber-roster-delete)]])

;;;###autoload
(transient-define-prefix jabber-info-menu ()
  "Jabber info/discovery commands."
  [["Discovery"
    ("i" "Disco items" jabber-get-disco-items)
    ("I" "Disco info" jabber-get-disco-info)
    ("b" "Browse" jabber-get-browse)
    ("v" "Client version" jabber-get-version)
    ("p" "Ping" jabber-ping)
    ("t" "Request time" jabber-get-time)
    ("V" "View vCard" jabber-vcard-get)]])

;;;###autoload
(transient-define-prefix jabber-muc-menu ()
  "Jabber MUC commands."
  [["MUC"
    ("j" "Join" jabber-muc-join)
    ("l" "Leave" jabber-muc-leave)
    ("n" "Change nick" jabber-muc-nick)
    ("t" "Set topic" jabber-muc-set-topic)
    ("i" "Invite" jabber-muc-invite)
    ("w" "List participants" jabber-muc-names)
    ("c" "Configure" jabber-muc-get-config)
    ("r" "Set role" jabber-muc-set-role)
    ("a" "Set affiliation" jabber-muc-set-affiliation)
    ("p" "Private chat" jabber-muc-private)
    ("v" "Request vcard" jabber-muc-vcard-get)]])

;;;###autoload
(transient-define-prefix jabber-service-menu ()
  "Jabber service commands."
  [["Services"
    ("r" "Register" jabber-get-register)
    ("s" "Search directory" jabber-get-search)
    ("c" "Execute command" jabber-ahc-execute-command)
    ("l" "Command list" jabber-ahc-get-list)
    ("C" "Enable carbons" jabber-enable-carbons)]])

(define-obsolete-function-alias 'jabber-popup-chat-menu #'jabber-chat-menu "29.1")
(define-obsolete-function-alias 'jabber-popup-roster-menu #'jabber-roster-context-menu "29.1")
(define-obsolete-function-alias 'jabber-popup-info-menu #'jabber-info-menu "29.1")
(define-obsolete-function-alias 'jabber-popup-muc-menu #'jabber-muc-menu "29.1")
(define-obsolete-function-alias 'jabber-popup-service-menu #'jabber-service-menu "29.1")
(define-obsolete-function-alias 'jabber-popup-combined-menu #'jabber-chat-menu "29.1")

(provide 'jabber-menu)
;;; jabber-menu.el ends here
