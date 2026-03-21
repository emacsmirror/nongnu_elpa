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

(defcustom jabber-modeline-sections '(activity)
  "Which sections to show in the mode line.
Available sections: `presence', `contacts', `activity'."
  :type '(set (const presence) (const contacts) (const activity))
  :group 'jabber-mode-line)

(defvar jabber-mode-line-presence "")
(defvar jabber-mode-line-contacts "")

;; Global reference declarations

(defvar *jabber-current-show*)          ; jabber.el
(defvar jabber-presence-strings)        ; jabber.el

;; Activity variables (defined in jabber-activity.el)

(declare-function jabber-activity--init "jabber-activity")
(declare-function jabber-activity--teardown "jabber-activity")
(declare-function jabber-activity--on-disconnect "jabber-activity")
(defvar jabber-activity-mode-string)
(defvar jabber-activity-count-in-title)
(defvar jabber-activity-count-in-title-format)

;;

(defconst jabber-modeline--eval-form
  '(:eval (jabber-modeline--render))
  "The `:eval' form added to `global-mode-string'.")

(defun jabber-modeline--render ()
  "Return the string to display in the mode line."
  (let ((parts nil))
    (when (and (memq 'activity jabber-modeline-sections)
               (not (string-empty-p jabber-activity-mode-string)))
      (push jabber-activity-mode-string parts))
    (when (and (memq 'contacts jabber-modeline-sections)
               (not (string-empty-p jabber-mode-line-contacts)))
      (push jabber-mode-line-contacts parts))
    (when (and (memq 'presence jabber-modeline-sections)
               (not (string-empty-p jabber-mode-line-presence)))
      (push jabber-mode-line-presence parts))
    (if parts
        (concat " " (string-join parts " "))
      "")))

(defun jabber-mode-line-presence-update (&rest _)
  "Update `jabber-mode-line-presence' from current connection state."
  (setq jabber-mode-line-presence
        (if (and jabber-connections (not *jabber-disconnecting*))
            (cdr (assoc *jabber-current-show* jabber-presence-strings))
          "Offline")))

(defvar jabber-mode-line--recount-timer nil
  "Pending timer for a debounced `jabber-mode-line--do-count-contacts' call.")

(defun jabber-mode-line--do-count-contacts ()
  "Perform the actual O(roster) presence recount."
  (setq jabber-mode-line--recount-timer nil)
  (let ((count (list (cons "chat" 0)
		     (cons "" 0)
		     (cons "away" 0)
		     (cons "xa" 0)
		     (cons "dnd" 0)
		     (cons nil 0))))
    (dolist (jc jabber-connections)
      (dolist (buddy (plist-get (fsm-get-state-data jc) :roster))
	(when-let* ((cell (assoc (get buddy 'show) count)))
	  (cl-incf (cdr cell)))))
    (setq jabber-mode-line-contacts
	  (if jabber-mode-line-compact
	      (format "(%d/%d/%d)"
		      (+ (cdr (assoc "chat" count))
			 (cdr (assoc "" count)))
		      (+ (cdr (assoc "away" count))
			 (cdr (assoc "xa" count))
			 (cdr (assoc "dnd" count)))
		      (cdr (assoc nil count)))
	    (apply #'format "(%d/%d/%d/%d/%d/%d)"
		   (mapcar #'cdr count))))
    (force-mode-line-update t)))

(defun jabber-mode-line-count-contacts (&rest _ignore)
  "Schedule a debounced roster recount (coalesces rapid presence bursts)."
  (when (timerp jabber-mode-line--recount-timer)
    (cancel-timer jabber-mode-line--recount-timer))
  (setq jabber-mode-line--recount-timer
        (run-with-timer 0.1 nil #'jabber-mode-line--do-count-contacts)))

(defun jabber-modeline--add-to-frame-title ()
  "Add activity count to `frame-title-format' and `icon-title-format'."
  (dolist (var '(frame-title-format icon-title-format))
    (let ((fmt (symbol-value var)))
      (unless (member jabber-activity-count-in-title-format fmt)
	(if (equal (car-safe fmt) "")
	    (set var (cons "" (cons jabber-activity-count-in-title-format
				   (cdr fmt))))
	  (set var (list "" jabber-activity-count-in-title-format fmt)))))))

(defun jabber-modeline--remove-from-frame-title ()
  "Remove activity count from `frame-title-format' and `icon-title-format'."
  (dolist (var '(frame-title-format icon-title-format))
    (when (listp (symbol-value var))
      (set var (delete jabber-activity-count-in-title-format
		       (symbol-value var))))))

(defun jabber-modeline--on-disconnect ()
  "Clear all modeline state on disconnect."
  (when (timerp jabber-mode-line--recount-timer)
    (cancel-timer jabber-mode-line--recount-timer)
    (setq jabber-mode-line--recount-timer nil))
  (jabber-mode-line--do-count-contacts)
  (jabber-activity--on-disconnect)
  (jabber-mode-line-presence-update))

;;;###autoload
(define-minor-mode jabber-modeline-mode
  "Toggle display of Jabber status in mode lines.
Which sections are shown is controlled by `jabber-modeline-sections'."
  :global t
  (if jabber-modeline-mode
      (progn
        (unless global-mode-string
          (setq global-mode-string '("")))
        (jabber-mode-line-presence-update)
        (jabber-mode-line-count-contacts)
        (add-hook 'jabber-send-presence
                  #'jabber-mode-line-presence-update)
        (add-hook 'jabber-post-disconnect-hook
                  #'jabber-mode-line-presence-update)
        (add-hook 'jabber-presence-hooks
                  #'jabber-mode-line-count-contacts)
        (add-hook 'jabber-post-disconnect-hook
                  #'jabber-modeline--on-disconnect)
        (jabber-activity--init)
        (when jabber-activity-count-in-title
          (jabber-modeline--add-to-frame-title))
        (add-to-list 'global-mode-string jabber-modeline--eval-form t))
    (when (timerp jabber-mode-line--recount-timer)
      (cancel-timer jabber-mode-line--recount-timer)
      (setq jabber-mode-line--recount-timer nil))
    (setq jabber-mode-line-presence ""
          jabber-mode-line-contacts "")
    (remove-hook 'jabber-send-presence
                 #'jabber-mode-line-presence-update)
    (remove-hook 'jabber-post-disconnect-hook
                 #'jabber-mode-line-presence-update)
    (remove-hook 'jabber-presence-hooks
                 #'jabber-mode-line-count-contacts)
    (remove-hook 'jabber-post-disconnect-hook
                 #'jabber-modeline--on-disconnect)
    (jabber-activity--teardown)
    (jabber-modeline--remove-from-frame-title)
    (setq global-mode-string
          (delete jabber-modeline--eval-form global-mode-string))
    (force-mode-line-update t)))

;; Backward compatibility
(defalias 'jabber-mode-line-mode #'jabber-modeline-mode)

(defun jabber-activity-mode (&optional arg)
  "Toggle the `activity' section in `jabber-modeline-sections'.
With a positive ARG, ensure activity is shown.
With a zero or negative ARG, remove activity."
  (interactive "P")
  (if (if arg (> (prefix-numeric-value arg) 0)
        (not (memq 'activity jabber-modeline-sections)))
      (cl-pushnew 'activity jabber-modeline-sections)
    (setq jabber-modeline-sections
          (delq 'activity jabber-modeline-sections)))
  (force-mode-line-update t))

(provide 'jabber-modeline)

;;; jabber-modeline.el ends here