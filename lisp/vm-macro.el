;;; vm-macro.el ---  Random VM macros  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1997 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2025 The VM Developers
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

;; Definitions for things that aren't in all Emacsen and that we really
;; prefer not to live without.
(eval-and-compile
  (if (fboundp 'unless) nil
    (defmacro unless (bool &rest forms) `(if ,bool nil ,@forms))
    (defmacro when (bool &rest forms) `(if ,bool (progn ,@forms))))
  (unless (fboundp 'save-current-buffer)
    (defalias 'save-current-buffer 'save-excursion))
  (if (fboundp 'mapc)
      (defalias 'bbdb-mapc 'mapc)
    (defalias 'bbdb-mapc 'mapcar))

  (unless (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buf &rest body)
      `(save-current-buffer (set-buffer ,buf) ,@body)))

  (unless (fboundp 'defvaralias)
    (defmacro defvaralias (&rest _args)))

  (unless (fboundp 'declare-function)
    (defmacro declare-function (_fn _file &optional _arglist _fileonly))))

(defmacro vm-interactive-p ()
  (if (featurep 'xemacs)
      `(interactive-p)
    (if (fboundp 'called-interactively-p) ;; (> emacs-major-version 23)
	`(called-interactively-p 'interactive)
      `(interactive-p))))

(declare-function vm-check-for-killed-summary "vm-misc" ())
(declare-function vm-check-for-killed-presentation "vm-misc" ())
(declare-function vm-error-if-folder-empty "vm-misc" ())
(declare-function vm-build-threads "vm-thread" (message-list))

(defvar vm-assertion-checking-off)
(defvar vm-buffer-types)
(defvar vm-current-warning)
(defvar vm-folder-read-only)
(defvar vm-mail-buffer)
(defvar vm-recognize-imap-maildrops)
(defvar vm-recognize-pop-maildrops)
(defvar vm-summary-buffer)
(defvar vm-thread-obarray)
(defvar vm-user-interaction-buffer)

(defmacro vm--dlet (binders &rest body)
  ;; Copied from `calendar-dlet'.
  "Like `dlet' but without warnings about non-prefixed var names."
  (declare (indent 1) (debug let))
  (let ((vars (mapcar (lambda (binder)
                        (if (consp binder) (car binder) binder))
                      binders)))
    ;; (defvar FOO) only affects the current scope, but in order for this
    ;; not to affect code after the main `let' we need to create a new scope,
    ;; which is what the surrounding `let' is for.
    ;; FIXME: (let () ...) currently doesn't actually create a new scope,
    ;; which is why we use (let (_) ...).
    `(let (_)
       (with-suppressed-warnings ((lexical ,@vars))
         ,@(mapcar (lambda (var) `(defvar ,var)) vars)
         (dlet ,binders ,@body)))))

(defmacro vm-add-to-list (elem list)
  "Like add-to-list, but compares elements by `eq' rather than `equal'."
  `(if (not (memq ,elem ,list))
       (setq ,list (cons ,elem ,list))))

(defsubst vm-sit-for (seconds &optional nodisplay)
  "Like sit-for, but has no effect if `display-hourglass' is set
to `t' and the Emacs is running in a `window-system'.  Otherwise,
the hourglass would be displayed while `sit-for' happens."
  (unless (and (boundp 'display-hourglass) display-hourglass
	       window-system)
    (sit-for seconds nodisplay)))

(defsubst vm-marker (pos &optional buffer)
  (set-marker (make-marker) pos buffer))

(defsubst vm-pop-folder-spec-p (folder)
  (and vm-recognize-pop-maildrops
       (string-match vm-recognize-pop-maildrops folder)))

(defsubst vm-imap-folder-spec-p (folder)
  (and vm-recognize-imap-maildrops
       (string-match vm-recognize-imap-maildrops folder)))

(defsubst vm-select-folder-buffer ()
  "Select the folder buffer corresponding to the current buffer (which
could be Summary or Presentation).  Gives an error message if there
isn't a folder buffer.  USR, 2010-03-08"
  (cond (vm-mail-buffer
	 (or (buffer-name vm-mail-buffer)
	     (error "Folder buffer has been killed."))
	 (set-buffer vm-mail-buffer))
	((not (or (eq major-mode 'vm-mode)
		  (eq major-mode 'vm-virtual-mode)))
	 (error "No VM folder buffer associated with this buffer")))
  ;;--------------------------
  ;; This may be problematic - done in revno 570.
  ;; All kinds of operations call vm-select-folder-buffer, including
  ;; asynchronous things like the toolbar.
  ;; (vm-buffer-type:set 'folder)
  ;;--------------------------
  )

(defsubst vm-select-folder-buffer-if-possible ()
  "Select the folder buffer corresponding to the current buffer (which
could be Summary or Presentation).  Returns normally if there
isn't a folder buffer.  USR, 2010-03-08"
  (cond ((and (bufferp vm-mail-buffer)
	      (buffer-name vm-mail-buffer))
	 (set-buffer vm-mail-buffer)
	 ;;--------------------------
	 ;; This may be problematic - done in revno 570.
	 ;; (vm-buffer-type:set 'folder)
	 ;;--------------------------
	 )
	((or (eq major-mode 'vm-mode)
	     (eq major-mode 'vm-virtual-mode))
	 ;;--------------------------
	 ;; This may be problematic - done in revno 570.
	 ;; (vm-buffer-type:set 'folder)
	 ;;--------------------------
	 )))

(defsubst vm-select-folder-buffer-and-validate (&optional minimum interactive-p)
  "Select the folder buffer corresponding to the current buffer (which
could be Summary or Presentation) and make sure that it has valid
references to Summary and Presentation buffers.  

If optional argument MINIMUM is 1, the folder should be nonempty
as well.  If INTERACTIVE-p is t, then it also records the
current-buffer in `vm-user-interaction-buffer'."
  (when interactive-p
    (setq vm-user-interaction-buffer (current-buffer))
    ;; Do some initializations for a new interactive command
    (setq vm-current-warning nil))
  (cond (vm-mail-buffer
	 (or (buffer-name vm-mail-buffer)
	     (error "Folder buffer has been killed."))
	 (set-buffer vm-mail-buffer))
	((not (or (eq major-mode 'vm-mode)
		  (eq major-mode 'vm-virtual-mode)))
	 (error "No VM folder buffer associated with this buffer")))
  ;;--------------------------
  ;; This may be problematic - done in revno 570.
  ;; (vm-buffer-type:set 'folder)
  ;;--------------------------

  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (if (and minimum (= minimum 1))
      (vm-error-if-folder-empty))
  )

(defsubst vm-error-if-folder-read-only ()
  (while vm-folder-read-only
    (signal 'folder-read-only (list (current-buffer)))))

(defsubst vm-error-if-virtual-folder ()
  (and (eq major-mode 'vm-virtual-mode)
       (error "%s cannot be applied to virtual folders." this-command)))

(defsubst vm-buffer-p ()
  (or (eq major-mode 'vm-mode)
      (eq major-mode 'vm-presentation-mode)
      (eq major-mode 'vm-virtual-mode)
      (eq major-mode 'vm-summary-mode)))

(defsubst vm-summary-operation-p ()
  (and vm-summary-buffer
       (eq vm-summary-buffer vm-user-interaction-buffer)))

(defsubst vm-build-threads-if-unbuilt ()
  (if (not (vectorp vm-thread-obarray))
      (vm-build-threads nil)))

(defsubst vm-binary-coding-system ()
  (cond ((featurep 'xemacs) 'binary)
	((featurep 'xemacs) 'binary)
	(t 'no-conversion)))

(defsubst vm-line-ending-coding-system ()
  (cond ((featurep 'xemacs) 'no-conversion)
	((featurep 'xemacs) 'no-conversion)
	(t 'raw-text)))

;; can't use defsubst where quoting is needed in some places but
;; not others.

(defmacro vm-assert (expression)
  (list 'or 'vm-assertion-checking-off
	(list 'or expression
	      (list 'let
		    (list (list 'debug-on-error t))
		    (list 'error "assertion failed: %S"
			  (list 'quote expression))))))

(defmacro vm-increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro vm-decrement (variable)
  (list 'setq variable (list '1- variable)))

;; This should be turned into a defsubst eventually

(defun vm-make-trace-buffer-name (session-name host)
   (format "trace of %s session to %s at %s" 
	   session-name host
	   (substring (current-time-string) 11 19)))

;; For verification of the correct buffer protocol
;; Possible values are 'folder, 'presentation, 'summary, 'process

;; (defvar vm-buffer-types nil)    ; moved to vm-vars.el

(defvar vm-buffer-type-debug nil
  "*This flag can be set to t for debugging asynchronous buffer change
  errors.")

(defvar vm-buffer-type-debug nil)	; for debugging asynchronous
					; buffer change errors
(defvar vm-buffer-type-trail nil
  "List of VM buffer types entered and exited, used for debugging
purposes.") 

(defsubst vm-buffer-type:enter (type)
  "Note that vm is temporarily entering a buffer of TYPE."
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail 
	    (cons type (cons 'enter vm-buffer-type-trail))))
  (setq vm-buffer-types (cons type vm-buffer-types)))

(defsubst vm-buffer-type:exit ()
  "Note that vm is exiting the current temporary buffer."
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'exit vm-buffer-type-trail)))
  (setq vm-buffer-types (cdr vm-buffer-types)))

(defsubst vm-buffer-type:duplicate ()
  "Note that vm is reentering the current buffer for a temporary purpose."
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons (car vm-buffer-type-trail)
				       vm-buffer-type-trail)))
  (setq vm-buffer-types (cons (car vm-buffer-types) vm-buffer-types)))

(defun vm-buffer-type:set (type)
  "Note that vm is changing to a buffer of TYPE."
  (when (and (eq type 'folder) vm-buffer-types 
	     (eq (car vm-buffer-types) 'process))
 	;; This may or may not be a problem.
 	;; It just means that no save-excursion was done among the
 	;; functions currently tracked by vm-buffe-types.
    (if vm-buffer-type-debug
	(debug "folder buffer being entered from %s" (car vm-buffer-types))
      (message "folder buffer being entered from %s" (car vm-buffer-types)))
    (setq vm-buffer-type-trail (cons type vm-buffer-type-trail)))
  (if vm-buffer-types
      (rplaca vm-buffer-types type)
    (setq vm-buffer-types (cons type vm-buffer-types))))

(defsubst vm-buffer-type:assert (type)
  "Check that vm is currently in a buffer of TYPE."
  (vm-assert (eq (car vm-buffer-types) type)))

(defsubst vm-buffer-type:wait-for-imap-session ()
  "Wait until the IMAP session is free to use, based on the
vm-buffer-types stack."
  (while (and vm-buffer-types 
	      (eq (car vm-buffer-types) 'process))
    (sleep-for 1)))


(provide 'vm-macro)
;;; vm-macro.el ends here
