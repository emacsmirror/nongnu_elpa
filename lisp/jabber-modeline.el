;;; jabber-modeline.el --- display jabber status in modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu
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

;;; Code:

(require 'jabber-presence)
(require 'jabber-alert)
(eval-when-compile (require 'cl-lib))

(defgroup jabber-mode-line nil
  "Display Jabber status in mode line"
  :group 'jabber)

(defcustom jabber-mode-line-compact t
  "Count contacts in fewer categories for compact view."
  :type 'boolean)

(defvar jabber-mode-line-string nil)

(defvar jabber-mode-line-presence nil)

(defvar jabber-mode-line-contacts nil)

;; Global reference declarations

(defvar *jabber-current-show*)          ; jabber.el
(defvar jabber-presence-strings)        ; jabber.el

;;

(defun jabber-mode-line-presence-update (&rest _)
  "Update `jabber-mode-line-presence' from current connection state."
  (let ((text (if (and jabber-connections (not *jabber-disconnecting*))
		  (cdr (assoc *jabber-current-show* jabber-presence-strings))
		"Offline")))
    (setq jabber-mode-line-presence
	  (propertize text 'jabber-modeline t))))

(defun jabber-mode-line-count-contacts (&rest _ignore)
  "Update `jabber-mode-line-contacts' with roster counts."
  (let ((count (list (cons "chat" 0)
		     (cons "" 0)
		     (cons "away" 0)
		     (cons "xa" 0)
		     (cons "dnd" 0)
		     (cons nil 0))))
    (dolist (jc jabber-connections)
      (dolist (buddy (plist-get (fsm-get-state-data jc) :roster))
	(when (assoc (get buddy 'show) count)
	  (cl-incf (cdr (assoc (get buddy 'show) count))))))
    (setq jabber-mode-line-contacts
	  (propertize
	   (if jabber-mode-line-compact
	       (format "(%d/%d/%d)"
		       (+ (cdr (assoc "chat" count))
			  (cdr (assoc "" count)))
		       (+ (cdr (assoc "away" count))
			  (cdr (assoc "xa" count))
			  (cdr (assoc "dnd" count)))
		       (cdr (assoc nil count)))
	     (apply #'format "(%d/%d/%d/%d/%d/%d)"
		    (mapcar #'cdr count)))
	   'jabber-modeline t))))

(defun jabber-mode-line--add ()
  "Install status indicator in `global-mode-string'."
  (cl-pushnew 'jabber-mode-line-string global-mode-string :test #'eq))

(defun jabber-mode-line--remove ()
  "Remove status indicator from `global-mode-string'."
  (setq global-mode-string
	(delq 'jabber-mode-line-string global-mode-string)))

(define-minor-mode jabber-mode-line-mode
  "Toggle display of Jabber status in mode lines.
Display consists of your own status, and six numbers
meaning the number of chatty, online, away, xa, dnd
and offline contacts, respectively."
  :global t
  (if jabber-mode-line-mode
      (progn
	(setq jabber-mode-line-string
	      (list (propertize " " 'jabber-modeline t)
		    'jabber-mode-line-presence
		    (propertize " " 'jabber-modeline t)
		    'jabber-mode-line-contacts))
	(put 'jabber-mode-line-string 'risky-local-variable t)
	(put 'jabber-mode-line-presence 'risky-local-variable t)
	(jabber-mode-line-presence-update)
	(jabber-mode-line-count-contacts)
	(jabber-mode-line--add)
	(add-hook 'jabber-send-presence
		  #'jabber-mode-line-presence-update)
	(add-hook 'jabber-post-disconnect-hook
		  #'jabber-mode-line-presence-update)
	(add-hook 'jabber-presence-hooks
		  #'jabber-mode-line-count-contacts))
    (jabber-mode-line--remove)
    (setq jabber-mode-line-string "")
    (remove-hook 'jabber-post-disconnect-hook
		 #'jabber-mode-line-presence-update)
    (remove-hook 'jabber-send-presence
		 #'jabber-mode-line-presence-update)
    (remove-hook 'jabber-presence-hooks
		 #'jabber-mode-line-count-contacts)))

(provide 'jabber-modeline)

;;; jabber-modeline.el ends here