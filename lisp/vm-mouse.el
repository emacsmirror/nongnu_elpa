;;; vm-mouse.el --- Mouse related functions and commands  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1995-1997 Kyle E. Jones
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

(require 'vm-menu)
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-mail-to-mailto-url "vm-reply" (url))
(defun vm-mouse-set-mouse-track-highlight (start end &optional overlay)
  "Create and return an overlay for mouse selection from START to
END.  If the optional argument OVERLAY is provided then that that
overlay is moved to cover START to END.  No new overlay is created in
that case.                                            USR, 2010-08-01"
  (if (null overlay)
      (let ((o (make-overlay start end)))
	(overlay-put o 'mouse-face 'highlight)
	o)
    (move-overlay overlay start end)))

;;;###autoload
(defun vm-mouse-button-2 (event)
  "The immediate action event in VM buffers, depending on where the
mouse is clicked.  See Info node `(VM) Using the Mouse'."
  (interactive "e")
  ;; go to where the event occurred
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  ;; now dispatch depending on where we are
  (cond ((eq major-mode 'vm-summary-mode)
	 (mouse-set-point event)
	 (beginning-of-line)
	 (if (let ((vm-follow-summary-cursor t))
	       (vm-follow-summary-cursor))
	     nil
	   (setq this-command 'vm-scroll-forward)
	   (call-interactively 'vm-scroll-forward)))
	((memq major-mode '(vm-mode vm-virtual-mode vm-presentation-mode))
	 (vm-mouse-popup-or-select event))))
(put 'vm-mouse-button-2 'vm-called-by-vm t)

;;;###autoload
(defun vm-mouse-button-3 (event)
  "Brings up the context-sensitive menu in VM buffers, depending
on where the mouse is clicked.  See Info node `(VM) Using the
Mouse'."
  (interactive "e")
  (if vm-use-menus
      (progn
	;; go to where the event occurred
	(set-buffer (window-buffer (posn-window (event-start event))))
	(goto-char (posn-point (event-start event)))
	;; now dispatch depending on where we are
	(cond ((eq major-mode 'vm-summary-mode)
	       (vm-menu-popup-mode-menu event))
	      ((eq major-mode 'vm-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'vm-presentation-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'vm-virtual-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'mail-mode)
	       (vm-menu-popup-context-menu event))))))
(put 'vm-mouse-button-3 'vm-called-by-vm t)

(defun vm-mouse-3-help (_object)
  nil
  "Use mouse button 3 to see a menu of options.")

(defun vm-mouse-get-mouse-track-string (event)
  (save-current-buffer
    ;; go to where the event occurred
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    (let ((o-list (overlays-at (point)))
	  (string nil))
      (while o-list
	(if (overlay-get (car o-list) 'mouse-face)
	    (setq string (vm-buffer-substring-no-properties
			  (overlay-start (car o-list))
			  (overlay-end (car o-list)))
		  o-list nil)
	  (setq o-list (cdr o-list))))
      string)))

;;;###autoload
(defun vm-mouse-popup-or-select (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (let ((o-list (overlays-at (point)))
	(found nil))
    (while (and o-list (not found))
      (cond ((overlay-get (car o-list) 'vm-url)
	     (setq found t)
	     (vm-mouse-send-url-at-event event))
	    ((overlay-get (car o-list) 'vm-mime-function)
	     (setq found t)
	     (funcall (overlay-get (car o-list) 'vm-mime-function)
		      (car o-list))))
      (setq o-list (cdr o-list)))
    (and (not found) (vm-menu-popup-context-menu event))))
(put 'vm-mouse-popup-or-select 'vm-called-by-vm t)

;;;###autoload
(defun vm-mouse-send-url-at-event (event)
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (goto-char (posn-point (event-start event)))
  (vm-mouse-send-url-at-position (posn-point (event-start event))))
(put 'vm-mouse-send-url-at-event 'vm-called-by-vm t)

(defun vm-mouse-send-url-at-position (pos &optional browser)
  (save-restriction
    (widen)
    (let ((o-list (overlays-at pos))
	  url o)
      (while (and o-list (null (overlay-get (car o-list) 'vm-url)))
	(setq o-list (cdr o-list)))
      (when o-list
	(setq o (car o-list))
	(setq url (vm-buffer-substring-no-properties
		   (overlay-start o)
		   (overlay-end o)))
	(vm-mouse-send-url url browser)))))

(defun vm-mouse-send-url (url &optional browser switches)
  (if (string-match "^[A-Za-z0-9._-]+@[A-Za-z0-9._-]+$" url)
      (setq url (concat "mailto:" url)))
  (if (string-match "^mailto:" url)
      (vm-mail-to-mailto-url url)
    (let ((browser (or browser vm-url-browser))
	  (switches (or switches vm-url-browser-switches)))
      (cond ((null browser)
	     ;; nil means URL passing is turned off; see `vm-url-browser'.
	     nil)
	    ;; a symbol may name a function that is not loaded yet, so it is
	    ;; called without asking whether it is one; a lambda is what
	    ;; customize's function type gives, and used to be dropped here.
	    ((or (symbolp browser) (functionp browser))
	     (funcall browser url))
	    ((stringp browser)
	     (vm-inform 5 "Sending URL to %s..." browser)
	     (apply 'vm-run-background-command browser
		    (append switches (list url)))
	     (vm-inform 5 "Sending URL to %s... done" browser))))))

(defvar vm-warn-for-interprogram-cut-function t)

(defun vm-mouse-send-url-to-window-system (url)
  (unless interprogram-cut-function
    (when vm-warn-for-interprogram-cut-function 
      (vm-warn 1 2 
	       (concat "Copying to kill ring only; "
		       "Customize interprogram-cut-function to copy to Window system"))
      (setq vm-warn-for-interprogram-cut-function nil)))
  (kill-new url))

(defun vm-mouse-send-url-to-clipboard (url &optional type)
  (unless type (setq type 'CLIPBOARD))
  (vm-inform 5 "Sending URL to %s..." type)
  ;; `gui-set-selection' and not `x-set-selection', which is an obsolete
  ;; alias for it and which VM called: `fboundp' is true of an obsolete
  ;; alias, so the guard that was here picked the deprecated name and left
  ;; the right one unreached (emacs-vm/vm#818).
  (gui-set-selection type url)
  (vm-inform 5 "Sending URL to %s... done" type))

;;;###autoload
(defun vm-mouse-install-mouse ()
  (if (null (lookup-key vm-mode-map [mouse-2]))
      (define-key vm-mode-map [mouse-2] 'vm-mouse-button-2))
  (when vm-popup-menu-on-mouse-3
    (define-key vm-mode-map [mouse-3] 'ignore)
    (define-key vm-mode-map [down-mouse-3] 'vm-mouse-button-3)))

(defun vm-run-background-command (command &rest arg-list)
  (vm-inform 5 "vm-run-background-command: %S %S" command arg-list)
  (apply (function call-process) command
         nil
         0
         nil arg-list))

(defun vm-run-command (command &rest arg-list)
  (vm-inform 5 "vm-run-command: %S %S" command arg-list)
  (apply (function call-process) command
         nil
         (get-buffer-create (concat " *" command "*"))
         nil arg-list))

(defvar binary-process-input) ;; FIXME: Unknown var.  XEmacs?

;; return t on zero exit status
;; return (exit-status . stderr-string) on nonzero exit status
(defun vm-run-command-on-region (start end output-buffer command
				       &rest arg-list)
  (let ((tempfile nil)
	;; use binary coding system in FSF Emacs/MULE
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
        (buffer-file-format nil)
	;; for DOS/Windows command to tell it that its input is
	;; binary.
	(binary-process-input t)
	;; call-process-region calls write-region.
	;; don't let it do CR -> LF translation.
	(selective-display nil)
	status errstring)
    (unwind-protect
	(progn
	  (setq tempfile (vm-make-tempfile-name))
	  (setq status
		(apply 'call-process-region
		       start end command nil
		       (list output-buffer tempfile)
		       nil arg-list))
	  (cond ((equal status 0) t)
		;; even if exit status non-zero, if there was no
		;; diagnostic output the command probably
		;; succeeded.  I have tried to just use exit status
		;; as the failure criterion and users complained.
		((equal (nth 7 (file-attributes tempfile)) 0)
		 (vm-warn 0 0 "%s exited non-zero (code %s)" command status)
		 (if vm-report-subprocess-errors
		     (cons status "")
		   t))
		(t (save-excursion
		     (vm-warn 0 0 "%s exited non-zero (code %s)" command status)
		     (set-buffer (find-file-noselect tempfile))
		     (setq errstring (buffer-string))
		     (kill-buffer nil)
		     (cons status errstring)))))
      (vm-error-free-call 'delete-file tempfile))))

;; stupid yammering compiler
(defvar vm-mouse-read-file-name-prompt)
(defvar vm-mouse-read-file-name-dir)
(defvar vm-mouse-read-file-name-default)
(defvar vm-mouse-read-file-name-must-match)
(defvar vm-mouse-read-file-name-initial)
(defvar vm-mouse-read-file-name-history)
(defvar vm-mouse-read-file-name-return-value)
(defvar vm-mouse-read-file-name-should-delete-frame)

(defun vm-mouse-read-file-name (prompt &optional dir default
				       must-match initial history)
  "Like read-file-name, except uses a mouse driven interface.
HISTORY argument is ignored."
  (save-excursion
    (or dir (setq dir default-directory))
    (set-buffer (vm-make-work-buffer " *Files*"))
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t
	  default-directory dir)
    (make-local-variable 'vm-mouse-read-file-name-prompt)
    (make-local-variable 'vm-mouse-read-file-name-dir)
    (make-local-variable 'vm-mouse-read-file-name-default)
    (make-local-variable 'vm-mouse-read-file-name-must-match)
    (make-local-variable 'vm-mouse-read-file-name-initial)
    (make-local-variable 'vm-mouse-read-file-name-history)
    (make-local-variable 'vm-mouse-read-file-name-return-value)
    (make-local-variable 'vm-mouse-read-file-name-should-delete-frame)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-dir dir)
    (setq vm-mouse-read-file-name-default default)
    (setq vm-mouse-read-file-name-must-match must-match)
    (setq vm-mouse-read-file-name-initial initial)
    (setq vm-mouse-read-file-name-history history)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-return-value nil)
    (setq vm-mouse-read-file-name-should-delete-frame nil)
    (if (and vm-mutable-frame-configuration vm-frame-per-completion
	     (vm-multiple-frames-possible-p))
	(save-excursion
	  (setq vm-mouse-read-file-name-should-delete-frame t)
	  (vm-goto-new-frame 'completion)))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-file-name-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-file-name-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-file-name-return-value)
	 (prog1
	     vm-mouse-read-file-name-return-value
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-file-name-event-handler (&optional string)
  (let ((key-doc "Click here for keyboard interface.")
	start list)
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (setq vm-mouse-read-file-name-return-value
			   (save-excursion
			     (vm-keyboard-read-file-name
			      vm-mouse-read-file-name-prompt
			      vm-mouse-read-file-name-dir
			      vm-mouse-read-file-name-default
			      vm-mouse-read-file-name-must-match
			      vm-mouse-read-file-name-initial
			      vm-mouse-read-file-name-history)))
		     (vm-mouse-read-file-name-quit-handler t))
		 (quit (vm-mouse-read-file-name-quit-handler))))
	      ((file-directory-p string)
	       (setq default-directory (expand-file-name string)))
	      (t (setq vm-mouse-read-file-name-return-value
		       (expand-file-name string))
		 (vm-mouse-read-file-name-quit-handler t))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-file-name-prompt)
    (vm-set-region-face start (point) 'bold)
    (cond ((and (not string) vm-mouse-read-file-name-default)
	   (setq start (point))
	   (insert vm-mouse-read-file-name-default)
	   (vm-mouse-set-mouse-track-highlight start (point))
	   )
	  ((not string) nil)
	  (t (insert default-directory)))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n ?\n)
    (setq list (vm-delete-backup-file-names
		(vm-delete-auto-save-file-names
		 (vm-delete-index-file-names
		  (directory-files default-directory)))))

    ;; delete dot files
    (setq list (vm-delete (lambda (file)
                            (string-match "^\\.\\([^.].*\\)?$" file))
                          list))
    ;; append a "/" to directories
    (setq list (mapcar (lambda (file)
                         (if (file-directory-p file)
                             (concat file "/")
                           file))
                       list))
    
    (vm-show-list list 'vm-mouse-read-file-name-event-handler)
    (setq buffer-read-only t)))

;;;###autoload
(defun vm-mouse-read-file-name-quit-handler (&optional normal-exit)
  (interactive)
  (if vm-mouse-read-file-name-should-delete-frame
      (vm-maybe-delete-windows-or-frames-on (current-buffer)))
  (if normal-exit
      (throw 'exit nil)
    (throw 'exit t)))
(put 'vm-mouse-read-file-name-quit-handler 'vm-called-by-vm t)

(defvar vm-mouse-read-string-prompt)
(defvar vm-mouse-read-string-completion-list)
(defvar vm-mouse-read-string-multi-word)
(defvar vm-mouse-read-string-return-value)
(defvar vm-mouse-read-string-should-delete-frame)

(defun vm-mouse-read-string (prompt completion-list &optional multi-word)
  (with-current-buffer (vm-make-work-buffer " *Choices*")
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t)
    (make-local-variable 'vm-mouse-read-string-prompt)
    (make-local-variable 'vm-mouse-read-string-completion-list)
    (make-local-variable 'vm-mouse-read-string-multi-word)
    (make-local-variable 'vm-mouse-read-string-return-value)
    (make-local-variable 'vm-mouse-read-string-should-delete-frame)
    (setq vm-mouse-read-string-prompt prompt)
    (setq vm-mouse-read-string-completion-list completion-list)
    (setq vm-mouse-read-string-multi-word multi-word)
    (setq vm-mouse-read-string-return-value nil)
    (setq vm-mouse-read-string-should-delete-frame nil)
    (if (and vm-mutable-frame-configuration vm-frame-per-completion
	     (vm-multiple-frames-possible-p))
	(save-excursion
	  (setq vm-mouse-read-string-should-delete-frame t)
	  (vm-goto-new-frame 'completion)))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-string-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-string-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-string-return-value)
	 (prog1
	     (if (listp vm-mouse-read-string-return-value)
		 (mapconcat 'identity vm-mouse-read-string-return-value " ")
	       vm-mouse-read-string-return-value)
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-string-event-handler (&optional string)
  (let ((key-doc  "Click here for keyboard interface.")
	(bs-doc   "      .... to go back one word.")
	(done-doc "      .... when you're done.")
	start) ;; list
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (setq vm-mouse-read-string-return-value
			   (vm-keyboard-read-string
			    vm-mouse-read-string-prompt
			    vm-mouse-read-string-completion-list
			    vm-mouse-read-string-multi-word))
		     (vm-mouse-read-string-quit-handler t))
		 (quit (vm-mouse-read-string-quit-handler))))
	      ((equal string bs-doc)
	       (setq vm-mouse-read-string-return-value
		     (nreverse
		      (cdr
		       (nreverse vm-mouse-read-string-return-value)))))
	      ((equal string done-doc)
	       (vm-mouse-read-string-quit-handler t))
	      (t (setq vm-mouse-read-string-return-value
		       (nconc vm-mouse-read-string-return-value
			      (list string)))
		 (if (null vm-mouse-read-string-multi-word)
		     (vm-mouse-read-string-quit-handler t)))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-string-prompt)
    (vm-set-region-face start (point) 'bold)
    (insert (mapconcat 'identity vm-mouse-read-string-return-value " "))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n)
    (if vm-mouse-read-string-multi-word
	(progn
	  (setq start (point))
	  (insert bs-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)
	  (setq start (point))
	  (insert done-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)))
    (insert ?\n)
    (vm-show-list vm-mouse-read-string-completion-list
		  'vm-mouse-read-string-event-handler)
    (setq buffer-read-only t)))

;;;###autoload
(defun vm-mouse-read-string-quit-handler (&optional normal-exit)
  (interactive)
  (if vm-mouse-read-string-should-delete-frame
      (vm-maybe-delete-windows-or-frames-on (current-buffer)))
  (if normal-exit
      (throw 'exit nil)
    (throw 'exit t)))
(put 'vm-mouse-read-string-quit-handler 'vm-called-by-vm t)

(provide 'vm-mouse)
;;; vm-mouse.el ends here
