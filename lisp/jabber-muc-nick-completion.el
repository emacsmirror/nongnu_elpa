;;; jabber-muc-nick-completion.el --- Add nick completion abilyty to emacs-jabber  -*- lexical-binding: t; -*-

;; Copyright (C) 2008 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2007, 2008, 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2007 - Serguei Jidkov - jsv@e-mail.ru
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

;;; History:
;;

;;; Code:

(require 'cl-lib)

(require 'jabber-chatbuffer)

;;; User customizations here:
(defcustom jabber-muc-completion-delimiter ": "
  "String to add to end of completion line."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-looks-personaling-symbols '("," ":" ">")
  "Symbols for personaling messages."
  :type '(repeat string)
  :group 'jabber-chat)

(defcustom jabber-muc-personal-message-bonus (* 60 20)
  "Bonus for personal message, in seconds."
  :type 'integer
  :group 'jabber-chat)

(defcustom jabber-muc-all-string "all"
  "String meaning all conference members (to insert in completion).
Note that \":\" or alike not needed (it appended in other string)"
  :type 'string
  :group 'jabber-chat)


(defvar *jabber-muc-participant-last-speaking* nil
  "Global alist in form (group . ((member . time-of-last-speaking) ...) ...).")

;; Global reference declarations

(declare-function jabber-muc-nickname "jabber-muc.el" (group &optional jc))
(defvar jabber-group)                   ; jabber-muc.el
(defvar jabber-muc-default-nicknames)   ; jabber-muc.el
(defvar jabber-muc-participants)        ; jabber-muc.el
(defvar jabber-chatting-with)           ; jabber-chat.el

;;

(defun jabber-my-nick (&optional group)
  "Return my jabber nick in GROUP."
  (let ((room (or group jabber-group)))
    (or (jabber-muc-nickname room)
        (cdr (assoc room jabber-muc-default-nicknames)))))

;;;###autoload
(defun jabber-muc-looks-like-personal-p (message &optional group)
  "Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look."
  (if message (string-match (concat
		 "^"
		 (jabber-my-nick group)
		 (regexp-opt jabber-muc-looks-personaling-symbols))
		message)
    nil))

(defun jabber-muc-nicknames ()
  "List of conference participants, excluding self, or nil if we not in conference."
  (cl-delete-if (lambda (nick)
		  (string= nick (jabber-my-nick)))
		(append (mapcar #'car (cdr (assoc jabber-group jabber-muc-participants)))
		        (list jabber-muc-all-string))))

(defun jabber-muc-participant-update-activity (group nick time)
  "Update NICK's time of last speaking in GROUP to TIME."
  (let* ((room (assoc group *jabber-muc-participant-last-speaking*))
         (room-activity (cdr room))
         (entry (assoc nick room-activity))
         (old-time (or (cdr entry) 0)))
    (when (> time old-time)
      ;; don't use put-alist for speed
      (progn
        (if entry (setcdr entry time)
          (setq room-activity
                (cons (cons nick time) room-activity)))
        (if room (setcdr room room-activity)
          (setq *jabber-muc-participant-last-speaking*
                (cons (cons group room-activity)
                      *jabber-muc-participant-last-speaking*)))))))

(defun jabber-muc-track-message-time (nick group _buffer text &optional _title)
  "Tracks time of NICK's last speaking in GROUP."
  (when nick
    (let ((time (float-time)))
      (jabber-muc-participant-update-activity
       group
       nick
       (if (jabber-muc-looks-like-personal-p text group)
	   (+ time jabber-muc-personal-message-bonus)
	 time)))))

(defun jabber-sort-nicks (nicks group)
  "Return list of NICKS in GROUP, sorted."
  ;; when completing word at beginning of line each nick, each element of NICKS
  ;; has a trailing completion-delimiter (usually ": ").
  (let ((times (cdr (assoc group *jabber-muc-participant-last-speaking*))))
    (cl-flet ((fetch-time (nick)
                (let ((time-entry (assoc
                                   (if (string-suffix-p
                                        jabber-muc-completion-delimiter
                                        nick)
                                       (substring
                                        nick 0 (- (length nick) 2))
                                     nick)
                                   times)))
                  (cons nick
                        (if time-entry (cdr time-entry) 0))))
              (cmp (nt1 nt2)
                (let ((t1 (cdr nt1))
                      (t2 (cdr nt2)))
                  (if (and (zerop t1) (zerop t2))
                      (string<
                       (car nt1)
                       (car nt2))
                    (> t1 t2)))))
      (mapcar #'car (sort (mapcar #'fetch-time nicks)
                          #'cmp)))))

(defun jabber-muc-beginning-of-line ()
  "Return position of line begining."
  (save-excursion
    (if (looking-back jabber-muc-completion-delimiter (line-beginning-position))
        (backward-char (+ (length jabber-muc-completion-delimiter) 1)))
    (skip-syntax-backward "^-")
    (point)))

(defun jabber-muc-active-participants (group)
  "Return nicks for speaking participants."
  (let ((times (cdr (assoc group *jabber-muc-participant-last-speaking*))))
    (cl-remove-if-not
     (lambda (nick) (assoc nick times))
     (jabber-muc-nicknames))))

(defun jabber-muc-nick-completion-at-point ()
  "Nick completion function for `completion-at-point'."
  (when (bound-and-true-p jabber-group)
    (let* ((completion-begin (max (line-beginning-position)
                                  (or jabber-point-insert (point-min))))
           (group jabber-group)
           (beg (save-excursion
                  (skip-syntax-backward "^ " completion-begin)
                  (point)))
           (start-of-line-p (= beg completion-begin))
           (nicks (jabber-muc-nicknames))
           (table (mapcar
                   (lambda (str)
                     (if start-of-line-p
                         (concat str jabber-muc-completion-delimiter)
                       str))
                   nicks))
           (prefix (buffer-substring-no-properties beg (point))))
      (when (cl-some (lambda (c) (string-prefix-p prefix c t)) table)
        (list beg (point)
              (lambda (str pred action)
                (if (eq action 'metadata)
                    `(metadata
                      (display-sort-function
                       . ,(lambda (nicks)
                            (jabber-sort-nicks nicks group)))
                      (cycle-sort-function
                       . ,(lambda (nicks)
                            (jabber-sort-nicks nicks group))))
                  (complete-with-action action table str pred))))))))

(add-hook 'jabber-muc-hooks #'jabber-muc-track-message-time)

(provide 'jabber-muc-nick-completion)

;;; jabber-muc-nick-completion.el ends here
