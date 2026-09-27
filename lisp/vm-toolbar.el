;;; vm-toolbar.el --- Toolbar related functions and commands  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1995-1997, 2000, 2001 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2026 The VM Developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Code:

(require 'vm-misc)
(require 'vm-window)
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-follow-summary-cursor "vm-motion" ())
(declare-function vm-mime-plain-message-p "vm-mime" (message))
(declare-function vm-save-message "vm-save" (folder 
					     &optional count mlist quiet))
(declare-function vm-auto-select-folder "vm-save" 
		  (mp &optional auto-folder-alist))

;; Each button is [ICON COMMAND ENABLED-FORM HELP].  ICON named a list of
;; XEmacs glyphs and is unread now, `vm-toolbar-fsfemacs-install-toolbar'
;; finding the image files by the button's own name; the two delete icons
;; below are variables still, being what tells the delete button from the
;; undelete one.
(defconst vm-toolbar-next-button
  [vm-toolbar-next-icon
   vm-toolbar-next-command
   (vm-toolbar-any-messages-p)
   "Go to the next message.\n
The command `vm-toolbar-next-command' is run, which is normally
fbound to `vm-next-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-next-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-next-command)
    (defalias 'vm-toolbar-next-command 'vm-next-message))
(put 'vm-toolbar-next-command 'vm-called-by-vm t)

(defconst vm-toolbar-previous-button
  [vm-toolbar-previous-icon
   vm-toolbar-previous-command
   (vm-toolbar-any-messages-p)
   "Go to the previous message.\n
The command `vm-toolbar-previous-command' is run, which is normally
fbound to `vm-previous-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-previous-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-previous-command)
    (defalias 'vm-toolbar-previous-command 'vm-previous-message))
(put 'vm-toolbar-previous-command 'vm-called-by-vm t)

(defconst vm-toolbar-autofile-button
  [vm-toolbar-autofile-icon
   vm-toolbar-autofile-message
   (vm-toolbar-can-autofile-p)
  "Save the current message to a folder selected using vm-auto-folder-alist."])

(defconst vm-toolbar-file-button
  [vm-toolbar-file-icon vm-toolbar-file-command (vm-toolbar-any-messages-p)
   "Save the current message to a folder.\n
The command `vm-toolbar-file-command' is run, which is normally
fbound to `vm-save-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-file-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-file-command)
    (defalias 'vm-toolbar-file-command 'vm-save-message))
(put 'vm-toolbar-file-command 'vm-called-by-vm t)

(defconst vm-toolbar-getmail-button
  [vm-toolbar-getmail-icon vm-toolbar-getmail-command
   (vm-toolbar-mail-waiting-p)
   "Retrieve spooled mail for the current folder.\n
The command `vm-toolbar-getmail-command' is run, which is normally
fbound to `vm-get-new-mail'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-getmail-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-getmail-command)
    (defalias 'vm-toolbar-getmail-command 'vm-get-new-mail))
(put 'vm-toolbar-getmail-command 'vm-called-by-vm t)

(defconst vm-toolbar-print-button
  [vm-toolbar-print-icon
   vm-toolbar-print-command
   (vm-toolbar-any-messages-p)
   "Print the current message.\n
The command `vm-toolbar-print-command' is run, which is normally
fbound to `vm-print-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-print-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-print-command)
    (defalias 'vm-toolbar-print-command 'vm-print-message))
(put 'vm-toolbar-print-command 'vm-called-by-vm t)

(defconst vm-toolbar-visit-button
  [vm-toolbar-visit-icon vm-toolbar-visit-command t
   "Visit a different folder.\n
The command `vm-toolbar-visit-command' is run, which is normally
fbound to `vm-visit-folder'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-visit-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-visit-command)
    (defalias 'vm-toolbar-visit-command 'vm-visit-folder))
(put 'vm-toolbar-visit-command 'vm-called-by-vm t)

(defconst vm-toolbar-reply-button
  [vm-toolbar-reply-icon
   vm-toolbar-reply-command
   (vm-toolbar-any-messages-p)
   "Reply to the current message.\n
The command `vm-toolbar-reply-command' is run, which is normally
fbound to `vm-followup-include-text'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-reply-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-reply-command)
    (defalias 'vm-toolbar-reply-command 'vm-followup-include-text))
(put 'vm-toolbar-reply-command 'vm-called-by-vm t)

(defconst vm-toolbar-forward-button
  [vm-toolbar-forward-icon
   vm-toolbar-forward-command
   (vm-toolbar-any-messages-p)
   "Forward the current message.\n
The command `vm-toolbar-forward-command' is run, which is normally
fbound to `vm-forward-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-forward-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-forward-command)
    (defalias 'vm-toolbar-forward-command 'vm-forward-message))
(put 'vm-toolbar-forward-command 'vm-called-by-vm t)

(defconst vm-toolbar-followup-button
  [vm-toolbar-followup-icon
   vm-toolbar-followup-command
   (vm-toolbar-any-messages-p)
   "Follow up the current message.\n
The command `vm-toolbar-followup-command' is run, which is normally
fbound to `vm-followup-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-followup-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-followup-command)
    (defalias 'vm-toolbar-followup-command 'vm-followup))
(put 'vm-toolbar-followup-command 'vm-called-by-vm t)

(defconst vm-toolbar-compose-button
  [vm-toolbar-compose-icon vm-toolbar-compose-command t
   "Compose a new message.\n
The command `vm-toolbar-compose-command' is run, which is normally
fbound to `vm-mail'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-compose-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-compose-command)
    (defalias 'vm-toolbar-compose-command 'vm-mail))
(put 'vm-toolbar-compose-command 'vm-called-by-vm t)

(defconst vm-toolbar-decode-mime-button
  [vm-toolbar-decode-mime-icon vm-toolbar-decode-mime-command
   (vm-toolbar-can-decode-mime-p)
   "Decode the MIME objects in the current message.\n
The objects might be displayed immediately, or buttons might be
displayed that you need to click on to view the object.  See the
documentation for the variables vm-mime-internal-content-types
and vm-mime-external-content-types-alist to see how to control
whether you see buttons or objects.\n
The command `vm-toolbar-decode-mime-command' is run, which is normally
fbound to `vm-decode-mime-messages'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-decode-mime-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-decode-mime-command)
    (defalias 'vm-toolbar-decode-mime-command 'vm-decode-mime-message))
(put 'vm-toolbar-decode-mime-command 'vm-called-by-vm t)

;; The values of these two are used by the FSF Emacs toolbar
;; code.  The values don't matter as long as they are different
;; (as compared with eq).  Under XEmacs these values are ignored
;; and overwritten.
(defvar vm-toolbar-delete-icon t)
(defvar vm-toolbar-undelete-icon nil)

(defconst vm-toolbar-delete/undelete-button
  [vm-toolbar-delete/undelete-icon
   vm-toolbar-delete/undelete-message
   (vm-toolbar-any-messages-p)
   "Delete the current message, or undelete it if it is already deleted."])
(defvar vm-toolbar-delete/undelete-icon nil)
(make-variable-buffer-local 'vm-toolbar-delete/undelete-icon)

(defconst vm-toolbar-help-button
  [vm-toolbar-helper-icon vm-toolbar-helper-command
   (vm-toolbar-can-help-p)
   "Don't Panic.\n
VM uses this button to offer help if you're in trouble.
Under normal circumstances, this button runs `vm-help'.
If the current folder looks out-of-date relative to its auto-save
file then this button will run `vm-recover-folder'.
If there is mail waiting in one of the spool files associated
with the current folder, and the `getmail' button is not on the
toolbar, this button will run `vm-get-new-mail'.
If the current message needs to be MIME decoded then this button
will run 'vm-decode-mime-message'."])

(defvar vm-toolbar-helper-command nil)
(make-variable-buffer-local 'vm-toolbar-helper-command)

;;;###autoload
(defun vm-toolbar-helper-command ()
  "Run whatever command `vm-toolbar-helper-command\' currently holds.
The toolbar\'s helper button changes with the context: what it does is
decided by reassigning this variable before the button is drawn, rather
than by having a button per command."
  (interactive)
  (setq this-command vm-toolbar-helper-command)
  (call-interactively vm-toolbar-helper-command))
(put 'vm-toolbar-helper-command 'vm-called-by-vm t)

(defconst vm-toolbar-quit-button
  [vm-toolbar-quit-icon vm-toolbar-quit-command
   (vm-toolbar-can-quit-p)
   "Quit visiting this folder.\n
The command `vm-toolbar-quit-command' is run, which is normally
fbound to `vm-quit'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-quit-command 'some-other-command)"])
(or (fboundp 'vm-toolbar-quit-command)
    (defalias 'vm-toolbar-quit-command 'vm-quit))
(put 'vm-toolbar-quit-command 'vm-called-by-vm t)

(defun vm-toolbar-any-messages-p ()
  (condition-case nil
      (save-excursion
	(vm-check-for-killed-folder)
	(vm-select-folder-buffer-if-possible)
	vm-message-list)
    (error nil)))

;;;###autoload
(defun vm-toolbar-delete/undelete-message (&optional prefixarg)
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((current-prefix-arg prefixarg))
    (if (vm-deleted-flag (car vm-message-pointer))
	(call-interactively 'vm-undelete-message)
      (call-interactively 'vm-delete-message))))
(put 'vm-toolbar-delete/undelete-message 'vm-called-by-vm t)

;;;###autoload
(defun vm-toolbar-can-autofile-p ()
  "Return the folder the current message would be auto-filed to, or nil.
What decides whether the toolbar\'s autofile button is enabled.  Never
signals: no folder, no message, and a folder buffer that has been killed
all give nil."
  (interactive)
  (condition-case nil
      (save-excursion
	(vm-check-for-killed-folder)
	(vm-select-folder-buffer-if-possible)
	(and vm-message-pointer
	     (vm-auto-select-folder vm-message-pointer)))
    (error nil)))
(put 'vm-toolbar-can-autofile-p 'vm-called-by-vm t)

;;;###autoload
(defun vm-toolbar-autofile-message ()
  "Save this message to the folder `vm-auto-folder-alist\' chooses for it.
The toolbar\'s autofile button.  Signals if no entry in that list matches
the message, rather than prompting for a folder."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (let ((file (vm-auto-select-folder vm-message-pointer)))
    (if file
	(progn
	  (vm-save-message file 1)
	  (vm-inform 5 "Message saved to %s" file))
      (error "No match for message in vm-auto-folder-alist."))))

(put 'vm-toolbar-autofile-message 'vm-called-by-vm t)
(defun vm-toolbar-can-recover-p ()
  (condition-case nil
      (save-excursion
	(vm-select-folder-buffer)
	(and vm-folder-read-only
	     buffer-file-name
	     buffer-auto-save-file-name
	     (null (buffer-modified-p))
	     (file-newer-than-file-p
	      buffer-auto-save-file-name
	      buffer-file-name)))
    (error nil)))

(defun vm-toolbar-can-decode-mime-p ()
  (condition-case nil
      (save-excursion
	(vm-select-folder-buffer)
	(and
	 vm-display-using-mime
	 vm-message-pointer
	 vm-presentation-buffer
	 (not (vm-mime-plain-message-p (car vm-message-pointer)))))
    (error nil)))

(defun vm-toolbar-can-quit-p ()
  (condition-case nil
      (save-excursion
	(vm-select-folder-buffer)
	(memq major-mode '(vm-mode vm-virtual-mode)))
    (error nil)))

(defun vm-toolbar-mail-waiting-p ()
  (condition-case nil
      (save-excursion
	(vm-select-folder-buffer)
	(or (not (natnump vm-mail-check-interval))
	    vm-spooled-mail-waiting))
    (error nil)))

(defalias 'vm-toolbar-can-help-p 'vm-toolbar-can-quit-p)

(defun vm-toolbar-update-toolbar ()
  (if (and vm-message-pointer (vm-deleted-flag (car vm-message-pointer)))
      (setq vm-toolbar-delete/undelete-icon vm-toolbar-undelete-icon)
    (setq vm-toolbar-delete/undelete-icon vm-toolbar-delete-icon))
  (cond ((vm-toolbar-can-recover-p)
	 (setq vm-toolbar-helper-command 'vm-recover-folder))
	((and (vm-toolbar-mail-waiting-p)
	      (not (memq 'getmail vm-use-toolbar)))
	 (setq vm-toolbar-helper-command 'vm-get-new-mail))
	((and (vm-toolbar-can-decode-mime-p) (not vm-mime-decoded)
	      (not (memq 'mime vm-use-toolbar)))
	 (setq vm-toolbar-helper-command 'vm-decode-mime-message))
	(t
	 (setq vm-toolbar-helper-command 'vm-help)))
  (if (and vm-summary-buffer (buffer-name vm-summary-buffer))
      (vm-copy-local-variables vm-summary-buffer
			       'vm-toolbar-delete/undelete-icon
			       'vm-toolbar-helper-command))
  (if (and vm-presentation-buffer (buffer-name vm-presentation-buffer))
      (vm-copy-local-variables vm-presentation-buffer
			       'vm-toolbar-delete/undelete-icon
			       'vm-toolbar-helper-command)))

(defun vm-toolbar-install-or-uninstall-toolbar ()
  (when (and (vm-toolbar-support-possible-p) vm-use-toolbar)
    (vm-toolbar-install-toolbar))
  (unless vm-use-toolbar
    (vm-toolbar-fsfemacs-uninstall-toolbar)))

(defun vm-toolbar-install-toolbar ()
  ;; drag these in now instead of waiting for them to be
  ;; autoloaded.  the "loading..." messages could come at a bad
  ;; moment and wipe an important echo area message, like "Auto
  ;; save file is newer..."
  (require 'vm-save)
  (require 'vm-summary)
  (unless vm-fsfemacs-toolbar-installed-p
    (vm-toolbar-fsfemacs-install-toolbar)))

(defun vm-toolbar-fsfemacs-uninstall-toolbar ()
  (define-key vm-mode-map [toolbar] nil)
  (setq vm-fsfemacs-toolbar-installed-p nil))

(defun vm-toolbar-fsfemacs-install-toolbar ()
  (let ((button-list (reverse vm-use-toolbar))
	(dir (vm-toolbar-pixmap-directory))
	(extension "xpm")
	item t-spec sym name images)
    (defvar tool-bar-map)
    ;; hide the toolbar entries that are in the global keymap so
    ;; VM has full control of the toolbar in its buffers.
    (if (and (boundp 'tool-bar-map)
	     (consp tool-bar-map))
	(let ((map (cdr tool-bar-map))
	      (v (vector 'tool-bar 'x)))
	  (while map
	    (aset v 1 (car (car map)))
	    (define-key vm-mode-map v 'undefined)
	    (setq map (cdr map)))))
    (while button-list
      (setq sym (car button-list))
      (cond ((null sym)
	     ;; can't do flushright in FSF Emacs
	     t)
	    ((integerp sym)
	     ;; can't do separators in FSF Emacs
	     t)
	    ((memq sym '(autofile compose file getmail
			 mime next previous print quit
			 reply followup forward visit))
	     (setq t-spec (symbol-value
			   (intern (format "vm-toolbar-%s-button"
					   (if (eq sym 'mime)
					       'decode-mime
					     sym)))))
             (setq name (symbol-name sym))
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir
			   (if (eq sym 'mime) nil 'heuristic)))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar sym) item))
	    ((eq sym 'delete/undelete)
	     (setq t-spec vm-toolbar-delete/undelete-button)
	     (setq name "delete")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-delete/undelete-icon
					vm-toolbar-delete-icon)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'delete) item)
	     (setq name "undelete")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-delete/undelete-icon
					vm-toolbar-undelete-icon)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'undelete) item))
	    ((eq sym 'help)
	     (setq t-spec vm-toolbar-help-button)
	     (setq name "help")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-helper-command 'vm-help)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'help-help) item)
	     (setq name "recover")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-helper-command
					'recover-file)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'help-recover) item)
	     (setq name "getmail")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-helper-command
					'vm-get-new-mail)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'help-getmail) item)
             (setq name "mime")
	     (setq images (vm-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir nil))
	     (setq item
		   (list 'menu-item
			 name
			 (aref t-spec 1)
			 ':help (aref t-spec 3)
			 ':visible '(eq vm-toolbar-helper-command
					'vm-decode-mime-message)
			 ':enable (aref t-spec 2)
;			 ':button '(:toggle nil)
			 ':image images))
	     (define-key vm-mode-map (vector 'tool-bar 'help-mime) item)))
      (setq button-list (cdr button-list))))
  (setq vm-fsfemacs-toolbar-installed-p t))

(defun vm-toolbar-make-fsfemacs-toolbar-image-spec (name extension dir _mask)
  (if vm-gtk-emacs-p
      ;; the GTK-toolbar will not display icons when providing a vector since
      ;; some version of GTK resp. Emacs 22 ...
      (list 'image
	    ':type (intern extension)
	    ':file (expand-file-name
		    (format "%s-up.%s"
			    name extension)
		    dir))
    (vector
     (list 'image
	   ':type (intern extension)
	   ':file (expand-file-name
		   (format "%s-dn.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':file (expand-file-name
		   (format "%s-up.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':file (expand-file-name
		   (format "%s-dn.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':file (expand-file-name
		   (format "%s-dn.%s"
			   name extension)
		   dir)))))

(provide 'vm-toolbar)
;;; vm-toolbar.el ends here
