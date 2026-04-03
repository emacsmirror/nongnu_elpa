;;; jabber-notifications.el --- emacs-jabber interface to notifications.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 - Adam Sjøgren - asjo@koldfront.dk
;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2007 - Rodrigo Lazo - rlazo.paz@gmail.com
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

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

;; Built on jabber-libnotify.el.

(eval-when-compile (require 'jabber-alert))

(require 'notifications)

(defcustom jabber-notifications-icon "emacs"
  "Icon to be used on the notification pop-up.
The default \"emacs\" is resolved by the system icon theme."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-notifications-timeout nil
  "Specifies the timeout of the pop up window in millisecond"
  :type 'integer
  :group 'jabber-alerts)

(defcustom jabber-notifications-message-header "Jabber message"
  "Defines the header of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-notifications-app "Emacs Jabber"
  "Defines the app of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-notifications-urgency "low"
  "Urgency of message"
  :type '(choice (const :tag "Low" "low")
                 (const :tag "Normal" "normal")
                 (const :tag "Critical" "critical"))
  :group 'jabber-alerts)

(defcustom jabber-notifications-muc 'mentions
  "When to show desktop notifications for MUC messages.
`all' shows a notification for every MUC message, `mentions'
only when the message looks like it is addressed to you, and
nil disables MUC notifications entirely."
  :type '(choice (const :tag "All messages" all)
                 (const :tag "Mentions only" mentions)
                 (const :tag "Disabled" nil))
  :group 'jabber-alerts)

;; Global reference declarations

(declare-function jabber-muc-looks-like-personal-p "jabber-muc-nick-completion.el"
                  (message &optional group))
(declare-function jabber-avatar-find-cached "jabber-avatar.el" (sha1-sum))
(declare-function jabber-jid-symbol "jabber-util.el" (jid))
(declare-function jabber-escape-xml "jabber-xml.el" (string))

;;

(defun jabber-message-notifications (from _buffer text title)
  "Show a message through the notifications.el interface."
  (let ((body (or (jabber-escape-xml text) " "))
        (avatar-hash (get (jabber-jid-symbol from) 'avatar-hash)))
    (condition-case err
        (notifications-notify
         :title title
         :body body
         :app-icon (or (and avatar-hash (jabber-avatar-find-cached avatar-hash))
                       jabber-notifications-icon)
         :app-name jabber-notifications-app
         :category "jabber.message"
         :timeout jabber-notifications-timeout)
      (dbus-error
       (message "jabber-notifications: D-Bus error: %s" (error-message-string err))))))

(defun jabber-muc-notifications (nick group buffer text title)
  "Show MUC message through the notifications.el interface.
Controlled by `jabber-notifications-muc': notify for all messages,
mentions only, or not at all."
  (when (pcase jabber-notifications-muc
          ('all t)
          ('mentions (jabber-muc-looks-like-personal-p text group))
          (_ nil))
    (jabber-message-notifications
     group buffer (if nick (format "%s: %s" nick text) text) title)))

;; jabber-*-notifications* requires "from" argument, so we cant use
;; define-jabber-alert/define-personal-jabber-alert here and do the
;; work by hand:

(cl-pushnew 'jabber-message-notifications (get 'jabber-alert-message-hooks 'custom-options))
(cl-pushnew 'jabber-muc-notifications (get 'jabber-alert-muc-hooks 'custom-options))

(define-obsolete-function-alias 'jabber-muc-notifications-personal
  #'jabber-muc-notifications "0.10.0")

(add-hook 'jabber-alert-message-hooks #'jabber-message-notifications)
(add-hook 'jabber-alert-muc-hooks #'jabber-muc-notifications)

(provide 'jabber-notifications)

;;; jabber-notifications.el ends here
