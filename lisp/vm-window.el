;;; vm-window.el --- Window management code for VM  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1997 Kyle E. Jones
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

(require 'vm-macro)
(require 'vm-misc)
(require 'tapestry) ;; FIXME: Use Emacs-24's `frameset'?
(eval-when-compile (require 'cl-lib))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

;;;###autoload
(defun vm-display (buffer display commands configs
		   &optional do-not-raise)
;; the clearinghouse VM display function.
;;
;; First arg BUFFER non-nil is a buffer to display or undisplay.
;; nil means there is no request to display or undisplay a
;; buffer.
;;
;; Second arg DISPLAY non-nil means to display the buffer, nil means
;; to undisplay it.  This function guarantees to display the
;; buffer if requested.  Undisplay is not guaranteed.
;;
;; Third arg COMMANDS is a list of symbols.  this-command must
;; match one of these symbols for a window configuration to be
;; applied.
;;
;; Fourth arg CONFIGS is a list of window configurations to try.
;; vm-set-window-configuration will step through the list looking
;; for an existing configuration, and apply the one it finds.
;;
;; Display is done this way:
;;  1. if the buffer is visible in an invisible frame, make that frame visible
;;  2. if the buffer is already displayed, quit
;;  3. if vm-display-buffer-hook in non-nil
;;        run the hooks
;;        use the selected window/frame to display the buffer
;;        quit
;;  4. apply a window configuration
;;        if the buffer is displayed now, quit
;;  5. call vm-display-buffer which will display the buffer.
;;
;; Undisplay is done this way:
;;  1. if the buffer is not displayed, quit
;;  2. if vm-undisplay-buffer-hook is non-nil
;;        run the hooks
;;        quit
;;  3. apply a window configuration
;;  4, if a window configuration was applied
;;        quit
;;  5. call vm-undisplay-buffer which will make the buffer
;;     disappear from at least one window/frame.
;;
;; If display/undisplay is not requested, only window
;; configuration is done, and only then if the value of
;; this-command is found in the COMMANDS list.
  (and (stringp buffer) (setq buffer (get-buffer buffer)))
  (save-current-buffer
   (let* ((w (and buffer (vm-get-buffer-window buffer)))
	  (wf (and w (vm-window-frame w))))
     (if (and w display (not do-not-raise))
	 (vm-raise-frame wf))
     (if (and w display (not (eq (vm-selected-frame) wf)))
	 (vm-select-frame wf))
     (cond ((and buffer display)
	    (if (and vm-display-buffer-hook
		     (null (vm-get-visible-buffer-window buffer)))
		(progn (with-current-buffer buffer
			 (run-hooks 'vm-display-buffer-hook))
		       (switch-to-buffer buffer))
	      (if (not (and (memq this-command commands)
			    (apply 'vm-set-window-configuration configs)
			    (vm-get-visible-buffer-window buffer)))
		  (vm-display-buffer buffer))))
	   ((and buffer (not display))
	    (if (and vm-undisplay-buffer-hook
		     (vm-get-visible-buffer-window buffer))
		(progn (with-current-buffer buffer
			 (run-hooks 'vm-undisplay-buffer-hook)))
	      (if (not (and (memq this-command commands)
			    (apply 'vm-set-window-configuration configs)))
		  (vm-undisplay-buffer buffer))))
	   ((memq this-command commands)
	    (apply 'vm-set-window-configuration configs))))))

(defun vm-display-buffer (buffer)
  (let ((pop-up-windows (eq vm-mutable-window-configuration t))
	(pop-up-frames (and pop-up-frames vm-mutable-frame-configuration)))
    (if (or pop-up-frames
	    (and (eq vm-mutable-window-configuration t)
		 (symbolp
		  (vm-buffer-to-label
		   (window-buffer
		    (selected-window))))))
	(select-window (display-buffer buffer))
      (switch-to-buffer buffer))))

(defun vm-undisplay-buffer (buffer)
  (save-current-buffer
   (let ((vm-mutable-frame-configuration 
	  (and vm-mutable-frame-configuration pop-up-frames)))
     (vm-maybe-delete-windows-or-frames-on buffer))
   (let (w)
     (while (setq w (vm-get-buffer-window buffer))
       (set-window-buffer w (other-buffer buffer))))))

(defun vm-load-window-configurations (file)
  (save-excursion
    (let ((work-buffer nil))
      (unwind-protect
	  (progn
	    (set-buffer (setq work-buffer (get-buffer-create "*vm-wconfig*")))
	    (set-buffer-multibyte nil)	; for empty buffer
	    (erase-buffer)
	    (setq vm-window-configurations
		  (condition-case ()
		      (progn
			(let ((coding-system-for-read
			          (vm-line-ending-coding-system)))
			  (insert-file-contents file))
			(read (current-buffer)))
		    (error nil))))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-store-window-configurations (file)
  (save-excursion
    (let ((work-buffer nil))
      (unwind-protect
	  (progn
	    (set-buffer (setq work-buffer (get-buffer-create "*vm-wconfig*")))
	    (set-buffer-multibyte nil)	; for empty buffer
	    (set-buffer-file-coding-system (vm-line-ending-coding-system))
	    (erase-buffer)
	    (print vm-window-configurations (current-buffer))
	    (let ((coding-system-for-write (vm-line-ending-coding-system))
		  (selective-display nil))
	      (write-region (point-min) (point-max) file nil 0)))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-set-window-configuration (&rest tags)
  (catch 'done
    (if (not vm-mutable-window-configuration)
	(throw 'done nil))
    (let ((nonexistent " *vm-nonexistent*")
	  (nonexistent-summary " *vm-nonexistent-summary*")
	  summary message composition edit config)
      (while (and tags (null config))
	(setq config (assq (car tags) vm-window-configurations)
	      tags (cdr tags)))
      (or config (setq config (assq 'default vm-window-configurations)))
      (or config (throw 'done nil))
      (setq config (vm-copy config))
      (setq composition (vm-find-composition-buffer t))
      (cond ((eq major-mode 'vm-summary-mode)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq summary (current-buffer))
	       (setq message vm-mail-buffer)))
	    ((eq major-mode 'vm-mode)
	     (setq message (current-buffer)))
	    ((eq major-mode 'vm-presentation-mode)
	     (setq message vm-mail-buffer))
	    ((eq major-mode 'vm-virtual-mode)
	     (setq message (current-buffer)))
	    ((eq major-mode 'mail-mode)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq message vm-mail-buffer
		     ;; assume that the proximity implies affinity
		     composition (current-buffer))))
	    ((eq vm-system-state 'editing)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq edit (current-buffer))
	       (setq message vm-mail-buffer)))
	    ;; not in a VM related buffer, bail...
	    (t (throw 'done nil)))
      (set-buffer message)
      (vm-check-for-killed-presentation)
      (if vm-presentation-buffer
	  (setq message vm-presentation-buffer))
      (vm-check-for-killed-summary)
      (or summary (setq summary (or vm-summary-buffer nonexistent-summary)))
      (or composition (setq composition nonexistent))
      (or edit (setq edit nonexistent))
      (let ((label-to-buffer ;; The reverse of `vm-buffer-to-label'.
             (lambda (label)
               (cl-ecase label
	         (summary summary)
	         (composition composition)
	         (message message)
	         (edit edit)))))
	(tapestry-replace-tapestry-element (nth 1 config) 'buffer-name
					   (function
					    (lambda (x)
					      (if (symbolp x)
						  (funcall label-to-buffer x)
						(if (and (stringp x)
						         (get-buffer x)
						         (zerop
							  (with-current-buffer x
							    (buffer-size))))
						    nonexistent
						  x ))))))
      (set-tapestry (nth 1 config) 1)
      (and (get-buffer nonexistent)
	   (vm-maybe-delete-windows-or-frames-on nonexistent))
      (if (and (vm-get-buffer-window nonexistent-summary)
	       (not (vm-get-buffer-window message)))
	  ;; user asked for summary to be displayed but doesn't
	  ;; have one, nor is the folder buffer displayed.  Help
	  ;; the user not to lose here.
	  (vm-replace-buffer-in-windows nonexistent-summary message)
	(and (get-buffer nonexistent-summary)
	     (vm-maybe-delete-windows-or-frames-on nonexistent-summary)))
      config )))

;;;###autoload
(defun vm-save-window-configuration (tag)
  "Name and save the current window configuration.
With this command you associate the current window setup with an
action.  Each time you perform this action VM will duplicate this
window setup.

Nearly every VM command can have a window configuration
associated with it.  VM also allows some category configurations,
`startup', `reading-message', `composing-message', `editing-message',
`marking-message' and `searching-message' for the commands that
do these things.  There is also a `default' configuration that VM
will use if no other configuration is applicable.  Command
specific configurations are searched for first, then the category
configurations and then the default configuration.  The first
configuration found is the one that is applied.

The value of vm-mutable-window-configuration must be non-nil for VM to use
window configurations."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
     (list
      (intern
       (completing-read 
	;; prompt
	"Name this window configuration: "
	;; collection
	vm-supported-window-configurations
	;; predicate, require-match
	'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
  (let (map p)
    (setq map (tapestry (list (vm-selected-frame))))
    ;; set frame map to nil since we don't use it.  this prevents
    ;; cursor objects and any other objects that have an
    ;; "unreadable" read syntax appearing in the window
    ;; configuration file by way of frame-parameters.
    (setcar map nil)
    (tapestry-replace-tapestry-element map 'buffer-name #'vm-buffer-to-label)
    (tapestry-nullify-tapestry-elements map t nil t t t nil)
    (setq p (assq tag vm-window-configurations))
    (if p
	(setcar (cdr p) map)
      (setq vm-window-configurations
	    (cons (list tag map) vm-window-configurations)))
    (vm-store-window-configurations vm-window-configuration-file)
    (vm-inform 5 "%s configuration recorded" tag)))

(defun vm-buffer-to-label (buf)
  (with-current-buffer buf
    (cond ((eq major-mode 'vm-summary-mode)
	   'summary)
	  ((eq major-mode 'mail-mode)
	   'composition)
	  ((eq major-mode 'vm-mode)
	   'message)
	  ((eq major-mode 'vm-presentation-mode)
	   'message)
	  ((eq major-mode 'vm-virtual-mode)
	   'message)
	  ((eq vm-system-state 'editing)
	   'edit)
	  (t buf))))

;;;###autoload
(defun vm-delete-window-configuration (tag)
  "Delete the configuration saved for a particular action.
This action will no longer have an associated window configuration.
The action will be read from the minibuffer."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
     (list
      (intern
       (completing-read 
	;; prompt
	"Delete window configuration: "
	;; collection
	(mapcar (function
		 (lambda (x)
		   (list (symbol-name (car x)))))
		vm-window-configurations)
	;; predicate, require-match
	'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
  (let (p)
    (setq p (assq tag vm-window-configurations))
    (if p
	(if (eq p (car vm-window-configurations))
	    (setq vm-window-configurations (cdr vm-window-configurations))
	  (setq vm-window-configurations (delq p vm-window-configurations)))
      (error "No window configuration set for %s" tag)))
  (vm-store-window-configurations vm-window-configuration-file)
  (vm-inform 5 "%s configuration deleted" tag))

;;;###autoload
(defun vm-apply-window-configuration (tag)
  "Change the current window configuration to be one
associated with a particular action.  The action will be read
from the minibuffer."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (intern
       (completing-read 
	;; prompt
	"Apply window configuration: "
	;; collection
	(mapcar (function
		 (lambda (x)
		   (list (symbol-name (car x)))))
		vm-window-configurations)
	;; predicate, require-match
	'identity t)))))
  (vm-set-window-configuration tag))

;;;###autoload
(defun vm-window-help ()
  "Show the window configuration commands and their keys in the echo area."
  (interactive)
  (vm-inform 0 "WS = save configuration, WD = delete configuration, WW = apply configuration"))

;;;###autoload
(defun vm-iconify-frame ()
  "Iconify the current frame.
Run the hooks in vm-iconify-frame-hook before doing so."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (vm-multiple-frames-possible-p)
      (progn
	(run-hooks 'vm-iconify-frame-hook)
	(vm-iconify-frame-xxx))))

(defun vm-window-loop (action obj-1 &optional obj-2)
  (let ((delete-me nil)
	(done nil)
	(all-frames (if vm-search-other-frames t nil))
	start w)
    (setq start (next-window (selected-window) 'nomini all-frames)
	  w start)
    (and obj-1 (setq obj-1 (get-buffer obj-1)))
    (while (not done)
      (if (and delete-me (not (eq delete-me (next-window delete-me 'nomini))))
	  (progn
	    (delete-window delete-me)
	    (if (eq delete-me start)
		(setq start nil))
	    (setq delete-me nil)))
      (cond ((and (eq action 'delete) (eq obj-1 (window-buffer w)))
	     ;; a deleted window has no next window, so we
	     ;; defer the deletion until after we've moved
	     ;; to the next window.
	     (setq delete-me w))
	    ((and (eq action 'replace) (eq obj-1 (window-buffer w)))
	     (set-window-buffer w obj-2)))
      (setq done (eq start
		     (setq w (next-window w 'nomini all-frames))))
      (if (null start)
	  (setq start w)))
    (if (and delete-me (not (eq delete-me (next-window delete-me 'nomini))))
	(delete-window delete-me))))

(defun vm-frame-loop (action obj-1)
  (if (fboundp 'vm-next-frame)
      (let ((start (vm-next-frame (vm-selected-frame)))
	    (delete-me nil)
	    (done nil)
	    f)
	(setq f start)
	(and obj-1 (setq obj-1 (get-buffer obj-1)))
	(while (not done)
	  (if delete-me
	      (progn
		(condition-case nil
		    (progn
		      (if (vm-created-this-frame-p delete-me)
			  (progn
			    (vm-delete-frame delete-me)
			    (if (eq delete-me start)
				(setq start nil)))))
		  (error nil))
		(setq delete-me nil)))
	  (cond ((and (eq action 'delete)
		      ;; one-window-p doesn't take a frame argument
		      (eq (next-window (vm-frame-selected-window f) 'nomini)
			  (previous-window (vm-frame-selected-window f)
					   'nomini))
		      ;; the next-window call is to avoid looking
		      ;; at the minibuffer window
		      (eq obj-1 (window-buffer
				 (next-window
				  (vm-frame-selected-window f)
				  'nomini))))
		 ;; a deleted frame has no next frame, so we
		 ;; defer the deletion until after we've moved
		 ;; to the next frame.
		 (setq delete-me f))
		((eq action 'bury)
		 (bury-buffer obj-1)))
	  (setq done (eq start (setq f (vm-next-frame f))))
	  (if (null start)
	      (setq start f)))
	(if (and delete-me (vm-created-this-frame-p delete-me))
	    (progn
	      (vm-error-free-call 'vm-delete-frame delete-me)
	      (setq delete-me nil))))))

(defun vm-maybe-delete-windows-or-frames-on (buffer)
  (and (eq vm-mutable-window-configuration t) (vm-window-loop 'delete buffer))
  (and vm-mutable-frame-configuration (vm-frame-loop 'delete buffer)))

(defun vm-replace-buffer-in-windows (old new)
  (vm-window-loop 'replace old new))

(defun vm-bury-buffer (&optional buffer)
  (bury-buffer (or buffer (current-buffer))))

(defun vm-unbury-buffer (buffer)
  (save-excursion
    (save-window-excursion
      ;; catch errors--- the selected window might be a dedicated
      ;; window or a minibuffer window.  We don't care and we
      ;; don't want to crash because of it.
      (condition-case _data
	  (switch-to-buffer buffer)
	(error nil)))))

(defun vm-set-hooks-for-frame-deletion ()
  (make-local-variable 'vm-undisplay-buffer-hook)
  (add-hook 'vm-undisplay-buffer-hook 'vm-delete-buffer-frame)
  (add-hook 'kill-buffer-hook 'vm-delete-buffer-frame))

(defun vm-created-this-frame-p (&optional frame)
  (memq (or frame (vm-selected-frame)) vm-frame-list))

(defun vm-delete-buffer-frame ()
  ;; kludge.  we only want to this to run on VM related buffers
  ;; but this function is generally on a global hook.  Check for
  ;; vm-undisplay-buffer-hook set; this is a good sign that this
  ;; is a VM buffer.
  (if vm-undisplay-buffer-hook
      (save-excursion
	;; run once only per buffer.
	(remove-hook 'vm-undisplay-buffer-hook 'vm-delete-buffer-frame)
	(let* ((w (vm-get-visible-buffer-window (current-buffer)))
	       (b (current-buffer))
	       (wf (and w (vm-window-frame w))))
	  (and w (eq (vm-selected-frame) wf) (vm-created-this-frame-p wf)
	       (vm-error-free-call 'vm-delete-frame wf))
	  (and w (let ((vm-mutable-frame-configuration t))
		   (vm-maybe-delete-windows-or-frames-on b)))))))

(defun vm-register-frame (frame)
  (setq vm-frame-list (cons frame vm-frame-list)))

(defun vm-goto-new-frame (&rest types)
  (let ((params nil))
    (while (and types (null params))
      (setq params (car (cdr (assq (car types) vm-frame-parameter-alist)))
	    types (cdr types)))
    ;; `make-frame' is defined in an Emacs with no window system too, and
    ;; fails there; the callers check `vm-multiple-frames-possible-p' first.
    (vm-select-frame (make-frame params))
    (vm-register-frame (vm-selected-frame))
    (and vm-warp-mouse-to-new-frame
	 (vm-warp-mouse-to-frame-maybe (vm-selected-frame)))))

(defun vm-goto-new-summary-frame-maybe ()
  (if (and vm-mutable-frame-configuration vm-frame-per-summary
	   (vm-multiple-frames-possible-p))
      (let ((w (vm-get-buffer-window vm-summary-buffer)))
	(if (null w)
	    (progn
	      (vm-goto-new-frame 'summary)
	      (vm-set-hooks-for-frame-deletion))
	  (save-excursion
	    (select-window w)
	    (and vm-warp-mouse-to-new-frame
		 (vm-warp-mouse-to-frame-maybe (vm-window-frame w))))))))

(defun vm-goto-new-folder-frame-maybe (&rest types)
  (if (and vm-mutable-frame-configuration vm-frame-per-folder
	   (vm-multiple-frames-possible-p))
      (let ((w (or (vm-get-buffer-window (current-buffer))
		   ;; summary == folder for the purpose
		   ;; of frame reuse.
		   (and vm-summary-buffer
			(vm-get-buffer-window vm-summary-buffer))
		   ;; presentation == folder for the purpose
		   ;; of frame reuse.
		   (and vm-presentation-buffer
			(vm-get-buffer-window vm-presentation-buffer)))))
	(if (null w)
	    (progn
	      (apply 'vm-goto-new-frame types)
	      (vm-set-hooks-for-frame-deletion))
	  (save-excursion
	    (select-window w)
	    (and vm-warp-mouse-to-new-frame
		 (vm-warp-mouse-to-frame-maybe (vm-window-frame w))))))))

(defun vm-warp-mouse-to-frame-maybe (&optional frame)
  (or frame (setq frame (vm-selected-frame)))
  (when (vm-mouse-support-possible-here-p)
    (let ((mp (mouse-position)))
      (unless (and (eq (car mp) frame)
		   ;; nil coordinates mean that the mouse pointer is not
		   ;; really within the frame
		   (car (cdr mp)))
	(set-mouse-position frame
			    (/ (frame-width frame) 2)
			    (/ (frame-height frame) 2))))))

(defun vm-selected-frame ()
  "The selected frame.  A wrapper, and not a command as `selected-frame' is."
  (selected-frame))

(defun vm-delete-frame (&optional frame force)
  "Delete FRAME, which defaults to the selected frame.
FORCE deletes it even when it is the last frame on its terminal."
  (delete-frame frame force))

;; xxx because vm-iconify-frame is a command
(defun vm-iconify-frame-xxx (&optional frame)
  "Iconify FRAME, which defaults to the selected frame."
  (iconify-frame frame))

(defun vm-deiconify-frame (frame)
  "Deiconify FRAME."
  (when (eq (frame-visible-p frame) 'icon)
    (select-frame frame)
    (iconify-or-deiconify-frame)))

(defun vm-raise-frame (&optional frame)
  "Raise FRAME, which defaults to the selected frame."
  (raise-frame frame))

(defun vm-frame-visible-p (frame)
  "Whether FRAME is visible, or `icon' if it is iconified."
  (frame-visible-p frame))

(defun vm-frame-iconified-p (&optional frame)
  "Whether FRAME is iconified, which `frame-visible-p' says by answering
`icon'."
  (eq (vm-frame-visible-p frame) 'icon))

(defun vm-frame-totally-visible-p (&optional frame)
  (or frame (setq frame (selected-frame)))
  (not (memq (frame-visible-p frame) '(nil hidden))))

(defun vm-window-frame (window)
  "The frame WINDOW is on."
  (window-frame window))

(defun vm-select-frame (frame &optional norecord)
  "Select FRAME, as `select-frame' does.
NORECORD leaves the frame's position in the recent-selection order alone."
  (select-frame frame norecord))

(defun vm-next-frame (&optional frame miniframe)
  "The frame after FRAME in the cyclic order, as `next-frame' has it.
MINIFRAME says how minibuffer-only frames are treated; VM never passes it."
  (next-frame frame miniframe))

(defun vm-frame-selected-window (&optional frame)
  "The window selected in FRAME, which defaults to the selected frame."
  (frame-selected-window frame))

(provide 'vm-window)
;;; vm-window.el ends here
