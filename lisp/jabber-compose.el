;;; jabber-compose.el --- compose a Jabber message in a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2006, 2007  Magnus Henoch
;; Copyright (C) 2026  Thanos Apollo

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:

(require 'jabber-core)
(require 'jabber-util)
(require 'jabber-chat)

;; Global reference declarations

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

(defvar-local jabber-compose-recipients nil
  "Recipients for the message in the current compose buffer.")

(defvar-local jabber-compose-subject ""
  "Subject for the message in the current compose buffer.")

(defun jabber-compose--read-recipients (&optional initial)
  "Read message recipients, using INITIAL as default values."
  (completing-read-multiple
   "Recipients: "
   (mapcar #'symbol-name (jabber-concat-rosters))
   nil nil nil nil (jabber-completion-multiple-default initial)))

(defun jabber-compose-edit-recipients ()
  "Edit recipients for the current composed message."
  (interactive)
  (setq-local jabber-compose-recipients
              (jabber-compose--read-recipients jabber-compose-recipients))
  (force-mode-line-update))

(defun jabber-compose-edit-subject ()
  "Edit subject for the current composed message."
  (interactive)
  (setq-local jabber-compose-subject
              (read-string "Subject: " jabber-compose-subject))
  (force-mode-line-update))

(defvar-keymap jabber-compose-mode-map
  :doc "Keymap for composing Jabber messages."
  :parent text-mode-map
  "C-c C-c" #'jabber-compose-send
  "C-c C-r" #'jabber-compose-edit-recipients
  "C-c C-s" #'jabber-compose-edit-subject)

(define-derived-mode jabber-compose-mode text-mode "Jabber-Compose"
  "Major mode for composing a multiline Jabber message."
  (setq-local header-line-format
              '(:eval (format "To: %s  Subject: %s"
                              (string-join jabber-compose-recipients ", ")
                              jabber-compose-subject))))

;;

;;;###autoload
(defun jabber-compose (jc &optional recipient)
  "Create a buffer for composing a Jabber message to RECIPIENT.

JC is the Jabber connection.  RECIPIENT, when non-nil, prefills
the recipient list."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "To whom? ")))
  (let* ((interactive-p (called-interactively-p 'interactive))
         (recipients (if interactive-p
                         (jabber-compose--read-recipients
                          (and recipient (list recipient)))
                       (and recipient (list recipient))))
         (subject (if interactive-p (read-string "Subject: ") ""))
         (buffer (generate-new-buffer
                  (concat "*Jabber Compose"
                          (when recipient
                            (format ": %s" (jabber-jid-displayname recipient)))
                          "*"))))
    (with-current-buffer buffer
      (jabber-compose-mode)
      (setq-local jabber-buffer-connection jc
                  jabber-compose-recipients recipients
                  jabber-compose-subject subject))
    (pop-to-buffer buffer)
    (message "Send with C-c C-c; edit recipients with C-c C-r")))

(defun jabber-compose-send (&rest _ignore)
  "Send the message composed in the current `jabber-compose' buffer."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (null jabber-compose-recipients)
      (user-error "No recipients specified"))

    (dolist (to jabber-compose-recipients)
      (jabber-send-message jabber-buffer-connection to
                           jabber-compose-subject text nil))

    (bury-buffer)
    (message "Message sent")))

(provide 'jabber-compose)

;;; jabber-compose.el ends here
