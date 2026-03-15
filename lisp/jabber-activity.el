;;; jabber-activity.el --- show jabber activity in the mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2004 Carl Henrik Lunde - <chlunde+jabber+@ping.uio.no>
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Allows tracking messages from buddies using the global mode line
;; See (info "(jabber)Tracking activity")

;;; TODO:

;; - Make it possible to enable this mode using M-x customize
;; - When Emacs is on another desktop, (get-buffer-window buf 'visible)
;;   returns nil.  We need to know when the user selects the frame again
;;   so we can remove the string from the mode line.  (Or just run
;;   jabber-activity-clean often).
;; - jabber-activity-switch-to needs a keybinding.  In which map?
;; - Is there any need for having defcustom jabber-activity-make-string?
;; - When there's activity in a buffer it would be nice with a hook which
;;   does the opposite of bury-buffer, so switch-to-buffer will show that
;;   buffer first.

;;; Code:

(require 'cl-lib)
(require 'jabber-core)
(require 'jabber-util)

(defgroup jabber-activity nil
  "Activity tracking options."
  :group 'jabber)

;; All the (featurep 'jabber-activity) is so we don't call a function
;; with an autoloaded cookie while the file is loading, since that
;; would lead to endless load recursion.

(defcustom jabber-activity-make-string 'jabber-activity-make-string-default
  "Function to call to show a string in the modeline.
The default function returns the nick of the user."
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (fboundp 'jabber-activity-make-name-alist))
	     (jabber-activity-make-name-alist)
	     (jabber-activity-mode-line-update)))
  :type 'function)

(defcustom jabber-activity-shorten-minimum 1
  "Length of the strings returned by `jabber-activity-make-strings-shorten'.
All strings returned by `jabber-activity-make-strings-shorten' will be
at least this long, when possible."
  :type 'number)

(defcustom jabber-activity-make-strings #'jabber-activity-make-strings-default
  "Function which should return an alist of JID -> string when given a list of
JIDs."
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (fboundp 'jabber-activity-make-name-alist))
	     (jabber-activity-make-name-alist)
	     (jabber-activity-mode-line-update)))
  :type '(choice (function-item :tag "Keep strings"
				:value jabber-activity-make-strings-default)
		 (function-item :tag "Shorten strings"
				:value jabber-activity-make-strings-shorten)
		 (function :tag "Other function")))

(defcustom jabber-activity-count-in-title nil
  "If non-nil, display number of active JIDs in frame title."
  :type 'boolean
  :set #'(lambda (var val)
	   (custom-set-default var val)
	   (when (and (featurep 'jabber-activity)
		      (bound-and-true-p jabber-activity-mode))
	     (jabber-activity-mode -1)
	     (jabber-activity-mode 1))))

(defcustom jabber-activity-count-in-title-format
  '(jabber-activity-jids ("[" jabber-activity-count-string "] "))
  "Format string used for displaying activity in frame titles.
Same syntax as `mode-line-format'."
  :type 'sexp
  :set #'(lambda (var val)
	   (if (not (and (featurep 'jabber-activity) (bound-and-true-p jabber-activity-mode)))
	       (custom-set-default var val)
	     (jabber-activity-mode -1)
	     (custom-set-default var val)
	     (jabber-activity-mode 1))))

(defcustom jabber-activity-show-p 'jabber-activity-show-p-default
  "Function that checks if the given JID should be shown on the mode line.
Predicate function to call to check if the given JID should be
shown in the mode line or not."
  :type 'function)

(defcustom jabber-activity-query-unread t
  "Query the user as to whether killing Emacs should be cancelled when
there are unread messages which otherwise would be lost."
  :type 'boolean)

(defcustom jabber-activity-banned nil
  "List of regexps of banned JID"
  :type '(repeat string))

(defface jabber-activity-face
  '((t (:foreground "red" :weight bold)))
  "The face for displaying jabber-activity-string in the mode line")

(defface jabber-activity-personal-face
  '((t (:foreground "blue" :weight bold)))
  "The face for displaying personal jabber-activity-string in the mode line")

(defvar jabber-activity-jids nil
  "A list of JIDs which have caused activity.")

(defvar jabber-activity-personal-jids nil
  "Subset of `jabber-activity-jids' for JIDs with \"personal\" activity.")

(defvar jabber-activity-name-alist nil
  "Alist of mode line names for bare JIDs.")

(defvar jabber-activity-mode-string ""
  "The mode string for jabber activity.")

(defvar jabber-activity-count-string "0"
  "Number of active JIDs as a string.")

(defvar jabber-activity-update-hook nil
  "Hook called when `jabber-activity-jids' changes.
It is called after `jabber-activity-mode-string' and
`jabber-activity-count-string' are updated.")

(defvar jabber-activity--updating nil
  "Non-nil while `jabber-activity-mode-line-update' is running.
Prevents recursive calls from hooks triggered during an update.")

;; Protect this variable from being set in Local variables etc.
(put 'jabber-activity-mode-string 'risky-local-variable t)
(put 'jabber-activity-count-string 'risky-local-variable t)

;; Global reference declarations

(declare-function jabber-chat-find-buffer "jabber-chat.el" (chat-with))
(declare-function jabber-muc-find-buffer "jabber-muc.el" (group))
(declare-function jabber-muc-private-find-buffer "jabber-muc.el"
                  (group nickname))
(declare-function jabber-muc-sender-p "jabber-muc.el" (jid))
(declare-function jabber-muc-looks-like-personal-p
                  "jabber-muc-nick-completion.el" (message &optional group))
(defvar jabber-silent-mode)             ; jabber.el

;;

(defun jabber-activity-make-string-default (jid)
  "Return the nick of the JID.
If no nick is available, return the user name part of the JID. In
private MUC conversations, return the user's nickname."
  (if (jabber-muc-sender-p jid)
      (jabber-jid-resource jid)
    (let ((nick (jabber-jid-displayname jid))
	  (user (jabber-jid-user jid))
	  (username (jabber-jid-username jid)))
      (if (and username (string= nick user))
	  username
	nick))))

(defun jabber-activity-make-strings-default (jids)
  "Apply `jabber-activity-make-string' on JIDS."
  (mapcar #'(lambda (jid) (cons jid (funcall jabber-activity-make-string jid)))
	  jids))

(defun jabber-activity-common-prefix (s1 s2)
  "Return length of common prefix string shared by S1 and S2."
  (let ((len (min (length s1) (length s2))))
    (or (cl-dotimes (i len)
	  (when (not (eq (aref s1 i) (aref s2 i)))
	    (cl-return i)))
	;; Substrings, equal, nil, or empty ("")
	len)))

(defun jabber-activity-make-strings-shorten (jids)
  "Return an alist of (JID . short-names).
This is acquired by running `jabber-activity-make-string' on
JIDS, and then shortening the names as much as possible such that
all strings still are unique and at least
`jabber-activity-shorten-minimum' long."
  (let ((alist
	 (sort (mapcar
		#'(lambda (x) (cons x (funcall jabber-activity-make-string x)))
		jids)
	       #'(lambda (x y) (string-lessp (cdr x) (cdr y))))))
    (cl-loop
     for ((_prev-jid . prev) (cur-jid . cur) (_next-jid . next))
     on (cons nil alist)
     until (null cur)
     collect
     (cons
      cur-jid
      (substring
       cur
       0 (min (length cur)
	      (max jabber-activity-shorten-minimum
		   (1+ (jabber-activity-common-prefix cur prev))
		   (1+ (jabber-activity-common-prefix cur next)))))))))

(defun jabber-activity-find-buffer-name (jid)
  "Find the buffer that messages from JID would use, or nil."
  (or (and (jabber-jid-resource jid)
	   (jabber-muc-private-find-buffer
	    (jabber-jid-user jid)
	    (jabber-jid-resource jid)))
      (jabber-chat-find-buffer jid)
      (jabber-muc-find-buffer jid)))

(defun jabber-activity-show-p-default (jid)
  "Return non-nil if JID should be hidden.
A JID should be hidden when there is an invisible buffer for JID,
and JID is not in `jabber-activity-banned'."
  (let ((buffer (jabber-activity-find-buffer-name jid)))
    (and (buffer-live-p buffer)
	 (not (get-buffer-window buffer 'visible))
         (not (cl-dolist (entry jabber-activity-banned)
                (when (string-match entry jid)
                  (cl-return t)))))))

(defun jabber-activity-make-name-alist (&optional _jc)
  "Rebuild `jabber-activity-name-alist' based on currently known JIDs."
  (let ((jids (or (mapcar #'car jabber-activity-name-alist)
		  (mapcar #'symbol-name *jabber-roster*))))
    (setq jabber-activity-name-alist
	  (funcall jabber-activity-make-strings jids))))

(defun jabber-activity-lookup-name (jid)
  "Lookup JID in `jabber-activity-name-alist'.
Return a (jid . string) pair suitable for the mode line, creating
an entry if needed."
  (let ((elm (assoc jid jabber-activity-name-alist)))
    (or elm
	(progn
	  ;; Remake alist with the new JID
	  (setq jabber-activity-name-alist
		(funcall jabber-activity-make-strings
		         (cons jid (mapcar #'car jabber-activity-name-alist))))
	  (jabber-activity-lookup-name jid)))))

(defun jabber-activity-mode-line-update ()
  "Update the string shown in the mode line.
Recomputes `jabber-activity-mode-string' and
`jabber-activity-count-string' from `jabber-activity-jids'."
  (unless jabber-activity--updating
    (let ((jabber-activity--updating t))
      (let ((new-mode-string
	     (if jabber-activity-jids
		 (mapconcat
		  (lambda (x)
		    (let ((jump-to-jid (car x)))
		      (propertize
		       (cdr x)
		       'face (if (member jump-to-jid jabber-activity-personal-jids)
				 'jabber-activity-personal-face
			       'jabber-activity-face)
		       'local-map (make-mode-line-mouse-map
				   'mouse-1 (lambda ()
					      (interactive "@")
					      (jabber-activity-switch-to
					       jump-to-jid)))
		       'help-echo (concat "Jump to "
					  (jabber-jid-displayname (car x))
					  "'s buffer"))))
		  (mapcar #'jabber-activity-lookup-name
			  jabber-activity-jids)
		  ",")
	       ""))
	    (new-count-string
	     (number-to-string (length jabber-activity-jids)))
	    (changed nil))
	(unless (equal-including-properties jabber-activity-mode-string
					    new-mode-string)
	  (setq jabber-activity-mode-string new-mode-string
		changed t))
	(unless (string= jabber-activity-count-string new-count-string)
	  (setq jabber-activity-count-string new-count-string
		changed t))
	(when changed
	  (force-mode-line-update 'all)
	  (run-hooks 'jabber-activity-update-hook))))))

;;; Hooks

(defun jabber-activity-clean ()
  "Remove JIDs where `jabber-activity-show-p' no longer is true."
  (setq jabber-activity-jids (cl-delete-if-not jabber-activity-show-p
					       jabber-activity-jids))
  (setq jabber-activity-personal-jids
	(cl-delete-if-not jabber-activity-show-p
			  jabber-activity-personal-jids))
  (jabber-activity-mode-line-update))

(defun jabber-activity-add (from _buffer _text _proposed-alert)
  "Add a JID to mode line when `jabber-activity-show-p'."
  (when (funcall jabber-activity-show-p from)
    (add-to-list 'jabber-activity-jids from)
    (add-to-list 'jabber-activity-personal-jids from)
    (jabber-activity-mode-line-update)))

(defun jabber-activity-add-muc (_nick group _buffer text _proposed-alert)
  "Add a JID to mode line when `jabber-activity-show-p'."
  (when (funcall jabber-activity-show-p group)
    (add-to-list 'jabber-activity-jids group)
    (when (jabber-muc-looks-like-personal-p text group)
      (add-to-list 'jabber-activity-personal-jids group))
    (jabber-activity-mode-line-update)))

(defun jabber-activity-presence (who _oldstatus newstatus _statustext _proposed-alert)
  "Add a JID to mode line on subscription requests."
  (when (string= newstatus "subscribe")
    (add-to-list 'jabber-activity-jids (symbol-name who))
    (add-to-list 'jabber-activity-personal-jids (symbol-name who))
    (jabber-activity-mode-line-update)))

(defun jabber-activity-kill-hook ()
  "Query the user if is sure to kill Emacs when there are unread messages.
Query the user as to whether killing Emacs should be cancelled
when there are unread messages which otherwise would be lost, if
`jabber-activity-query-unread' is t"
  (if (and jabber-activity-jids
	   jabber-activity-query-unread)
      (or jabber-silent-mode (yes-or-no-p
       "You have unread Jabber messages, are you sure you want to quit?"))
    t))

;;; Interactive functions

(defvar jabber-activity-last-buffer nil
  "Last non-Jabber buffer used.")

(defun jabber-activity-switch-to (&optional jid-param)
  "If JID-PARAM is provided, switch to that buffer.
If JID-PARAM is nil and there has been activity in another
buffer, switch to that buffer. If no such buffer exists, switch
back to the last non Jabber chat buffer used."
    (interactive)
    (if (or jid-param jabber-activity-jids)
        (let ((jid (or jid-param (car jabber-activity-jids))))
          (unless (eq major-mode 'jabber-chat-mode)
            (setq jabber-activity-last-buffer (current-buffer)))
          (switch-to-buffer (jabber-activity-find-buffer-name jid))
          (jabber-activity-clean))
      (if (eq major-mode 'jabber-chat-mode)
	  ;; Switch back to the buffer used last
	  (when (buffer-live-p jabber-activity-last-buffer)
	    (switch-to-buffer jabber-activity-last-buffer))
	(message "No new activity"))))

;;;###autoload
(define-minor-mode jabber-activity-mode
  "Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive."
  :global t
  :init-value t
  (if jabber-activity-mode
      (progn
	(add-hook 'window-configuration-change-hook
		  #'jabber-activity-clean)
	(add-hook 'jabber-message-hooks
		  #'jabber-activity-add)
	(add-hook 'jabber-muc-hooks
		  #'jabber-activity-add-muc)
	(add-hook 'jabber-presence-hooks
		  #'jabber-activity-presence)
	(add-hook 'buffer-list-update-hook
		  #'jabber-activity-clean)
	(add-hook 'jabber-post-connect-hooks
		  #'jabber-activity-make-name-alist)
	(add-hook 'kill-emacs-query-functions
		  #'jabber-activity-kill-hook)
	(add-to-list 'global-mode-string
		     '(t jabber-activity-mode-string))
	(when jabber-activity-count-in-title
	  ;; Be careful not to override specific meanings of the
	  ;; existing title format.  In particular, if the car is
	  ;; a symbol, we can't just add our stuff at the beginning.
	  ;; If the car is "", we should be safe.
	  ;;
	  ;; In my experience, sometimes the activity count gets
	  ;; included twice in the title.  I'm not sure exactly why,
	  ;; but it would be nice to replace the code below with
	  ;; something cleaner.
	  (if (equal (car-safe frame-title-format) "")
	      (add-to-list 'frame-title-format
			   jabber-activity-count-in-title-format)
	    (setq frame-title-format (list ""
					   jabber-activity-count-in-title-format
					   frame-title-format)))
	  (if (equal (car-safe icon-title-format) "")
	      (add-to-list 'icon-title-format
			 jabber-activity-count-in-title-format)
	    (setq icon-title-format (list ""
					    jabber-activity-count-in-title-format
					    icon-title-format)))))
    (progn
      (remove-hook 'window-configuration-change-hook
		   #'jabber-activity-clean)
      (remove-hook 'jabber-message-hooks
		   #'jabber-activity-add)
      (remove-hook 'jabber-muc-hooks
		   #'jabber-activity-add-muc)
      (remove-hook 'jabber-presence-hooks
		   #'jabber-activity-presence)
      (remove-hook 'buffer-list-update-hook
		   #'jabber-activity-clean)
      (remove-hook 'jabber-post-connect-hooks
		   #'jabber-activity-make-name-alist)
      (setq global-mode-string (delete '(t jabber-activity-mode-string)
				       global-mode-string))
      (when (listp frame-title-format)
	(setq frame-title-format
	      (delete jabber-activity-count-in-title-format
		      frame-title-format)))
      (when (listp icon-title-format)
	(setq icon-title-format
	      (delete jabber-activity-count-in-title-format
		      icon-title-format))))))

;; XXX: define-minor-mode should probably do this for us, but it doesn't.
(if jabber-activity-mode (jabber-activity-mode 1))

(provide 'jabber-activity)

;;; jabber-activity.el ends here
