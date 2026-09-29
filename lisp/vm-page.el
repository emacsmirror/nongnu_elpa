;;; vm-page.el ---  Commands to move around within a VM message  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;
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

(require 'wid-edit)		; the shrunken-header widget
(require 'vm-macro)
(require 'vm-window)
(require 'vm-motion)
(require 'vm-menu)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-make-virtual-copy "vm-virtual" (message))
(declare-function vm-make-presentation-copy "vm-mime" (message))
(declare-function vm-decode-mime-message "vm-mime" (&optional state))
(declare-function vm-mime-plain-message-p "vm-mime" (message))


;;;###autoload
(defun vm-scroll-forward (&optional arg)
  "Scrolls forward a screenful of text.
If the current message is being previewed, the message body is revealed.
If at the end of the current message, moves to the next message iff the
value of vm-auto-next-message is non-nil.
Prefix argument N means scroll forward N lines."
  (interactive "P")
  (let (mp-changed
	needs-decoding 
	(was-invisible nil))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (setq mp-changed
	  (or (null vm-presentation-buffer)
	      (not (equal (vm-number-of (car vm-message-pointer))
		       (with-current-buffer vm-presentation-buffer
			 (vm-number-of (car vm-message-pointer)))))))
    ;; the following vodoo was added by USR for fixing the jumping
    ;; cursor problem in the summary window, reported on May 4, 2008
    ;; in gnu.emacs.vm.info, title "Re: synchronization of vm buffers"
    ;; The original vodoo was:
    (when mp-changed 
      (vm-present-current-message)
      (sit-for 0))

    (setq needs-decoding (and vm-display-using-mime
			      (not vm-mime-decoded)
			      (not (vm-mime-plain-message-p
				    (car vm-message-pointer)))
			      vm-auto-decode-mime-messages
			      (eq vm-system-state 'previewing)))
    (when vm-presentation-buffer
      (set-buffer vm-presentation-buffer))
    ;; We are either in the Presentation buffer or the Folder buffer
    (let (;; (point (point))
	  (w (vm-get-visible-buffer-window (current-buffer))))
      (unless (and w (vm-frame-totally-visible-p (vm-window-frame w)))
	(vm-display (current-buffer) t
		    '(vm-scroll-forward vm-scroll-backward)
		    (list this-command 'reading-message))
	;; window start sticks to end of clip region when clip
	;; region moves back past it in the buffer.  fix it.
	(setq w (vm-get-visible-buffer-window (current-buffer)))
	(if (= (window-start w) (point-max))
	    (set-window-start w (point-min)))
	(setq was-invisible t)))
    (if (or mp-changed was-invisible needs-decoding
	    (and (eq vm-system-state 'previewing)
		 (pos-visible-in-window-p
		  (point-max)
		  (vm-get-visible-buffer-window (current-buffer)))))
	(progn
	  (unless was-invisible
	    (let ((w (vm-get-visible-buffer-window (current-buffer)))
		  old-w-start)
	      (setq old-w-start (window-start w))
	      ;; save-excursion to avoid possible buffer change
	      (save-excursion (vm-select-frame (window-frame w)))
	      (vm-raise-frame (window-frame w))
	      (vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
			  (list this-command 'reading-message))
	      (setq w (vm-get-visible-buffer-window (current-buffer)))
	      (and w (set-window-start w old-w-start))))
	  (cond ((eq vm-system-state 'previewing)
		 (vm-show-current-message)
		 ;; The window start marker sometimes drifts forward
		 ;; because of something that vm-show-current-message
		 ;; does.  In Emacs 20, replacing ASCII chars with
		 ;; multibyte chars seems to cause it, but I _think_
		 ;; the drift can happen in Emacs 19 and even
		 ;; XEmacs for different reasons.  So we reset the
		 ;; start marker here, since it is an easy fix.
		 (let ((w (vm-get-visible-buffer-window (current-buffer))))
		   (set-window-start w (point-min)))))
	  (vm-howl-if-eom))
      (let ((vmp vm-message-pointer)
	    (msg-buf (current-buffer))
	    (h-diff 0)
	    w old-w old-w-height old-w-start result)
	(when (eq vm-system-state 'previewing)
	  (vm-show-current-message))
	(setq vm-system-state 'reading)
	(setq old-w (vm-get-visible-buffer-window msg-buf)
	      old-w-height (window-height old-w)
	      old-w-start (window-start old-w))
	(setq w (vm-get-visible-buffer-window msg-buf))
	(vm-select-frame (window-frame w))
	(vm-raise-frame (window-frame w))
	(vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
		    (list this-command 'reading-message))
	(setq w (vm-get-visible-buffer-window msg-buf))
	(if (null w)
	    (error "current window configuration hides the message buffer.")
	  (setq h-diff (- (window-height w) old-w-height)))
	;; must restore this since it gets clobbered by window
	;; teardown and rebuild done by the window config stuff.
	(set-window-start w old-w-start)
	(setq old-w (selected-window))
	(unwind-protect
	    (progn
	      (select-window w)
	      (let ((next-screen-context-lines
		     (+ next-screen-context-lines h-diff)))
		(while (eq (setq result (vm-scroll-forward-internal arg))
			   'tryagain))
		(cond ((and (not (eq result 'next-message))
			    vm-honor-page-delimiters)
		       (vm-narrow-to-page)
		       (goto-char (max (window-start w)
				       (vm-text-of (car vmp))))
		       ;; This is needed because in some cases
		       ;; the scroll-up call in vm-howl-if-emo
		       ;; does not signal end-of-buffer when
		       ;; it should unless we do this.  This
		       ;; sit-for most likely removes the need
		       ;; for the (scroll-up 0) below, but
		       ;; since the voodoo has worked this
		       ;; long, it's probably best to let it
		       ;; be.
		       (sit-for 0)
		       ;; This voodoo is required!  For some
		       ;; reason the 18.52 emacs display
		       ;; doesn't immediately reflect the
		       ;; clip region change that occurs
		       ;; above without this mantra. 
		       (scroll-up 0)))))
	  (select-window old-w))
	(set-buffer msg-buf)
	(cond ((eq result 'next-message)
	       (vm-next-message))
	      ((eq result 'end-of-message)
	       (let ((vm-message-pointer vmp))
		 (vm-emit-eom-blurb)))
	      (t
	       (and (> (prefix-numeric-value arg) 0)
		    (vm-howl-if-eom)))))))
  (unless vm-startup-message-displayed
    (vm-display-startup-message)))

(defun vm-scroll-forward-internal (arg)
  (let ((direction (prefix-numeric-value arg))
	(w (selected-window)))
    (condition-case error-data
	(progn (scroll-up arg) nil)
;; this looks like it should work, but doesn't because the
;; redisplay code is schizophrenic when it comes to updates.  A
;; window position may no longer be visible but
;; pos-visible-in-window-p will still say it is because it was
;; visible before some window size change happened.
      (error
       (if (or (and (< direction 0)
		    (> (point) (vm-text-of (car vm-message-pointer))))
	       (and (>= direction 0)
		    (/= (point)
			(vm-text-end-of (car vm-message-pointer)))))
	   (progn
	     (vm-widen-page)
	     (if (>= direction 0)
		 (progn
		   (forward-page 1)
		   (set-window-start w (point))
		   nil )
	       (if (or (bolp)
		       (not (save-excursion
			      (beginning-of-line)
			      (looking-at page-delimiter))))
		   (forward-page -1))
	       (beginning-of-line)
	       (set-window-start w (point))
	       'tryagain))
	 (if (eq (car error-data) 'end-of-buffer)
	     (if vm-auto-next-message
		 'next-message
	       (set-window-point w (point))
	       'end-of-message)))))))

(defvar scroll-in-place-replace-original) ;; FIXME: Unknown var.  XEmacs?

;; exploratory scrolling, what a concept.
;;
;; we do this because pos-visible-in-window-p checks the current
;; window configuration, while this exploratory scrolling forces
;; Emacs to recompute the display, giving us an up to the moment
;; answer about where the end of the message is going to be
;; visible when redisplay finally does occur.
(defun vm-howl-if-eom ()
  (let ((w (get-buffer-window (current-buffer))))
    (and w
	 (save-excursion
	   (save-window-excursion
	     (condition-case ()
		 (let ((next-screen-context-lines 0))
		   (select-window w)
		   (save-excursion
		     (save-window-excursion
		       ;; scroll-fix.el replaces scroll-up and
		       ;; doesn't behave properly when it hits
		       ;; end of buffer.  It does this!
		       (let ((scroll-in-place-replace-original nil))
			 (scroll-up nil))))
		   nil)
	       (error t))))
	 (= (vm-text-end-of (car vm-message-pointer)) (point-max))
	 (vm-emit-eom-blurb))))

(defun vm-emit-eom-blurb ()
  "Prints a minibuffer message when the end of message is reached, but
it is suppressed if the variable `vm-auto-next-message' is nil."
  (interactive)
  (if vm-auto-next-message
      (let ((vm-summary-recipient-marker "")
	    (case-fold-search nil))
	(vm-inform 6 (if (and (stringp vm-summary-uninteresting-senders)
			  (string-match vm-summary-uninteresting-senders
					(vm-su-from (car vm-message-pointer))))
		     "End of message %s to %.50s..."
		   "End of message %s from %.50s...")
		 (vm-number-of (car vm-message-pointer))
		 (vm-summary-sprintf "%F" (car vm-message-pointer))))))
(put 'vm-emit-eom-blurb 'vm-called-by-vm t)

(defun vm-emit-mime-decoding-message (format &rest args)
  (interactive)
  (when vm-emit-messages-for-mime-decoding
    (apply 'message (concat "%s: " format) (buffer-name vm-mail-buffer) args)))
(put 'vm-emit-mime-decoding-message 'vm-called-by-vm t)

;;;###autoload
(defun vm-scroll-backward (&optional arg)
  "Scroll backward a screenful of text.
Prefix N scrolls backward N lines."
  (interactive "P")
  (vm-scroll-forward (cond ((null arg) '-)
			   ((consp arg) (list (- (car arg))))
			   ((numberp arg) (- arg))
			   ((symbolp arg) nil)
			   (t arg))))

;;;###autoload
(defun vm-scroll-forward-one-line (&optional count)
  "Scroll forward one line.
Prefix arg N means scroll forward N lines.
Negative arg means scroll backward."
  (interactive "p")
  (vm-scroll-forward count))

;;;###autoload
(defun vm-scroll-backward-one-line (&optional count)
  "Scroll backward one line.
Prefix arg N means scroll backward N lines.
Negative arg means scroll forward."
  (interactive "p")
  (vm-scroll-forward (- count)))

(defun vm-highlight-headers ()
  (let (o-lists p)
    (setq o-lists (overlay-lists)
	  p (car o-lists))
    (while p
      (when (overlay-get (car p) 'vm-highlight)
	(vm-delete-extent (car p)))
      (setq p (cdr p)))
    (setq p (cdr o-lists))
    (while p
      (when (overlay-get (car p) 'vm-highlight)
	(vm-delete-extent (car p)))
      (setq p (cdr p)))
    (goto-char (point-min))
    (while (vm-match-header)
      (when (vm-match-header vm-highlighted-header-regexp)
	(setq p (make-overlay (vm-matched-header-contents-start)
			      (vm-matched-header-contents-end)))
	(overlay-put p 'face vm-highlighted-header-face)
	(overlay-put p 'vm-highlight t))
      (goto-char (vm-matched-header-end)))))


;;;###autoload
(defun vm-energize-urls (&optional clean-only)
  (interactive "P")
  ;; Don't search too long in large regions.  If the region is
  ;; large, search just the head and the tail of the region since
  ;; they tend to contain the interesting text.
  (let ((search-limit vm-url-search-limit)
	search-pairs n)
    (if (and search-limit (> (- (point-max) (point-min)) search-limit))
	(setq search-pairs (list (cons (point-min)
				       (+ (point-min) (/ search-limit 2)))
				 (cons (- (point-max) (/ search-limit 2))
				       (point-max))))
      (setq search-pairs (list (cons (point-min) (point-max)))))
    (let (e)
      (vm-map-extents (function
		       (lambda (e _ignore)
			 (when (vm-extent-property e 'vm-url)
			   (vm-delete-extent e))
			 nil))
		      )
      (if clean-only (vm-inform 1 "Energy from urls removed!")
	(while search-pairs
	  (goto-char (car (car search-pairs)))
	  (while (re-search-forward vm-url-regexp (cdr (car search-pairs)) t)
	    (setq n 1)
	    (while (null (match-beginning n))
	      (vm-increment n))
	    (setq e (vm-make-extent (match-beginning n) (match-end n)))
	    (vm-set-extent-property e 'vm-url t)
	    (if vm-highlight-url-face
		(vm-set-extent-property e 'face vm-highlight-url-face))
	    (if vm-url-browser
		(let ((keymap (make-sparse-keymap))
		      (popup-function
		       (if (save-excursion
			     (goto-char (match-beginning n))
			     (looking-at "mailto:"))
			   'vm-menu-popup-mailto-url-browser-menu
			 'vm-menu-popup-url-browser-menu)))
		  (setq keymap (nconc keymap (current-local-map)))
		  (when vm-popup-menu-on-mouse-3
		    (define-key keymap [mouse-3] popup-function))
		  (define-key keymap "\r"
			      (function (lambda () (interactive)
				          (vm-mouse-send-url-at-position (point)))))
		  (vm-set-extent-property e 'vm-button t)
		  ;; for xemacs
		  (vm-set-extent-property e 'keymap keymap)
		  ;; for fsfemacs
		  (vm-set-extent-property e 'local-map keymap)
		  (vm-set-extent-property e 'balloon-help 'vm-url-help)
		  ;; for xemacs
		  (vm-set-extent-property e 'highlight t)
		  ;; for fsfemacs
		  (vm-set-extent-property e 'mouse-face 'highlight)
		  ;; for vm-continue-postponed-message
		  (vm-set-extent-property e 'duplicable t)
		  )))
	  (setq search-pairs (cdr search-pairs)))))))

(defun vm-energize-headers ()
  (let ((search-tuples '(("^From:" vm-menu-fsfemacs-author-menu)
			 ("^Subject:" vm-menu-fsfemacs-subject-menu)))
	regexp menu
	o-lists o p)
    (setq o-lists (overlay-lists)
	  p (car o-lists))
    (while p
      (when (overlay-get (car p) 'vm-header)
	(vm-delete-extent (car p)))
      (setq p (cdr p)))
    (setq p (cdr o-lists))
    (while p
      (when (overlay-get (car p) 'vm-header)
	(vm-delete-extent (car p)))
      (setq p (cdr p)))
    (while search-tuples
      (goto-char (point-min))
      (setq regexp (nth 0 (car search-tuples))
	    menu (symbol-value (nth 1 (car search-tuples))))
      (while (re-search-forward regexp nil t)
	(goto-char (match-end 0))
	(save-excursion (goto-char (match-beginning 0)) (vm-match-header))
	(setq o (make-overlay (vm-matched-header-contents-start)
			      (vm-matched-header-contents-end)))
	(overlay-put o 'vm-header menu)
	(overlay-put o 'mouse-face 'highlight))
      (setq search-tuples (cdr search-tuples)))))

(defun vm-display-xface ()
  (when (stringp vm-uncompface-program)
    (vm-display-xface-fsfemacs)))

(defun vm-display-xface-fsfemacs ()
  (catch 'done
    (let ((case-fold-search t) i g h ooo)
      (setq ooo (overlays-in (point-min) (point-max)))
      (while ooo
	(when (overlay-get (car ooo) 'vm-xface)
	  (vm-delete-extent (car ooo)))
	(setq ooo (cdr ooo)))
      (goto-char (point-min))
      (if (re-search-forward "^X-Face:" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (vm-match-header)
	    (setq h (vm-matched-header-contents))
	    (setq g (intern h vm-xface-cache))
	    (if (boundp g)
		(setq g (symbol-value g))
	      (setq i (vm-convert-xface-to-fsfemacs-image-instantiator h))
	      (cond (i
		     (set g i)
		     (setq g (symbol-value g)))
		    (t (throw 'done nil))))
	    (let ((pos (vm-vheaders-of (car vm-message-pointer)))
		  o )
	      ;; An image must replace the normal display of at
	      ;; least one character.  Since we want to put the
	      ;; image at the beginning of the visible headers
	      ;; section, it will obscure the first character of
	      ;; that section.  To display that character we add
	      ;; an after-string that contains the character.
	      ;; Kludge city, but it works.
	      (setq o (make-overlay (+ 0 pos) (+ 1 pos)))
	      (overlay-put o 'vm-xface t)
	      (overlay-put o 'evaporate t)
	      (overlay-put o 'after-string
			   (char-to-string (char-after pos)))
	      (overlay-put o 'display g)))))))

(defun vm-convert-xface-to-fsfemacs-image-instantiator (data)
  (let ((work-buffer nil)
	retval)
    (catch 'done
      (unwind-protect
	  (save-excursion
	    (if (not (stringp vm-uncompface-program))
		(throw 'done nil))
	    (setq work-buffer (vm-make-work-buffer))
	    (set-buffer work-buffer)
	    (insert data)
	    (setq retval
		  (apply 'call-process-region
			 (point-min) (point-max)
			 vm-uncompface-program t t nil
			 (if vm-uncompface-accepts-dash-x '("-X") nil)))
	    (if (not (eq retval 0))
		(throw 'done nil))
	    (if vm-uncompface-accepts-dash-x
		(throw 'done
		       (list 'image ':type 'xbm
			     ':ascent 80
			     ':foreground "black"
			     ':background "white"
			     ':data (buffer-string))))
	    (if (not (stringp vm-icontopbm-program))
		(throw 'done nil))
	    (goto-char (point-min))
	    (insert "/* Width=48, Height=48 */\n");
	    (setq retval
		  (call-process-region
		   (point-min) (point-max)
		   vm-icontopbm-program t t nil))
	    (if (not (eq retval 0))
		nil
	      (list 'image ':type 'pbm
		    ':ascent 80
		    ':foreground "black"
		    ':background "white"
		    ':data (buffer-string))))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-url-help (_object)
  (format
   "Use mouse button 2 to send the URL to %s.
Use mouse button 3 to choose a Web browser for the URL."
   (cond ((stringp vm-url-browser) vm-url-browser)
	 ((symbolp vm-url-browser) (symbol-name vm-url-browser))
	 ;; customize's function type also allows a lambda, which has no name
	 (t "a Lisp function"))))

;;;###autoload
(defun vm-energize-urls-in-message-region (&optional start end)
  (interactive "r")
  (save-excursion
    (or start (setq start (vm-headers-of (car vm-message-pointer))))
    (or end (setq end (vm-text-end-of (car vm-message-pointer))))
    ;; energize the URLs
    (if (or (facep vm-highlight-url-face) vm-url-browser)
        (save-restriction
          (widen)
          (narrow-to-region start end)
          (vm-energize-urls)))))
    
(defconst vm-citation-prefix-regexp "[ \t]*[-A-Za-z0-9]*>[ \t]*"
  "One level of quoting at the start of a line.
A `>' on its own, or one behind the initials some readers put there.")

(defun vm-citation-depth ()
  "How many levels of quoting the line at point begins with.
Point is left after the prefixes counted."
  (let ((depth 0))
    (while (looking-at vm-citation-prefix-regexp)
      (goto-char (match-end 0))
      (setq depth (1+ depth)))
    depth))

(defun vm-fontify-citations (start end)
  "Colour quoted text between START and END, a face per level of quoting.
The faces are `vm-citation-faces', and text quoted deeper than there are
faces wears the last of them."
  (when vm-citation-faces
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (point))
               (depth (vm-citation-depth)))
          (when (> depth 0)
            (let ((face (nth (min (1- depth) (1- (length vm-citation-faces)))
                             vm-citation-faces)))
              (vm-fontify-region line-start (line-end-position) face)))
          (forward-line 1))))))

(defun vm-fontify-signature (start end)
  "Colour the signature between START and END with `vm-signature-face'.
The signature is what follows the last line of exactly \"-- \", which is the
separator RFC 3676 describes.  A leading `- ' is allowed on it, that being
what a signature quoted into a digest looks like."
  (when vm-signature-face
    (save-excursion
      (goto-char end)
      (let ((separator (re-search-backward "^\\(- \\)?-- ?$" start t)))
        (when separator
          (vm-fontify-region separator end vm-signature-face))))))

(defun vm-fontify-region (start end face)
  "Put FACE on the text between START and END, marked as VM's own.
An overlay rather than a text property, and marked, so that the next message
shown in this buffer can take it off again the way `vm-highlight-headers'
does with the headers it puts on."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'vm-highlight t)))

(defun vm-fontify-body-maybe ()
  "Colour quoted text and the signature of the message being shown.
Does nothing unless `vm-enable-body-faces' says to.  Called where
`vm-highlight-headers-maybe' is, and it removes what this leaves behind:
both mark their overlays `vm-highlight'."
  (when (and vm-enable-body-faces vm-message-pointer)
    (save-restriction
      (widen)
      (let ((start (vm-text-of (car vm-message-pointer)))
            (end (vm-text-end-of (car vm-message-pointer))))
        (vm-fontify-citations start end)
        (vm-fontify-signature start end)))))

(defun vm-highlight-headers-maybe ()
  ;; highlight the headers
  (if vm-highlighted-header-regexp
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer)))
	(vm-highlight-headers))))

(defun vm-energize-headers-and-xfaces ()
  ;; energize certain headers
  (if (and vm-use-menus (vm-menu-support-possible-p))
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-of (car vm-message-pointer)))
	(vm-energize-headers)))
  ;; display xfaces, if we can
  (if (and vm-display-xfaces (stringp vm-uncompface-program))
      (save-restriction
	(widen)
	(narrow-to-region (vm-headers-of (car vm-message-pointer))
			  (vm-text-of (car vm-message-pointer)))
	(vm-display-xface))))

(defun vm-narrow-for-preview (&optional _just-passing-through)
  "Hide as much of the message body as vm-preview-lines specifies.
JUST-PASSING-THROUGH said that no real preview was necessary, and is
ignored: it suppressed a workaround for XEmacs displaying the begin-glyph of
an extent at the end of a narrowed region, which put the image of a message
that held only one on the screen at preview time however small
vm-preview-lines was."
  (widen)
  (narrow-to-region
   (vm-vheaders-of (car vm-message-pointer))
   (cond ((not (eq vm-preview-lines t))
	  (min
	   (vm-text-end-of (car vm-message-pointer))
	   (save-excursion
	     (goto-char (vm-text-of (car vm-message-pointer)))
	     (forward-line (if (natnump vm-preview-lines) vm-preview-lines 0))
	     (point))))
	 (t (vm-text-end-of (car vm-message-pointer))))))

;; This function was originally famous as `vm-preview-current-buffer',
;; but it was a misnomer because it does both previewing and showing.

;;;###autoload
(defun vm-present-current-message ()
  "Display the current message in the Presentation Buffer.  A
copy of the message is made in the Presentation Buffer and MIME
decoding is done if necessary.  The displayed content might be a
preview or the full message, governed by the the variables
`vm-preview-lines' and `vm-preview-read-messages'.  USR,2010-01-14"

  ;; Set need-preview if the user needs to see the
  ;; message in the previewed state.  Save some time later by not
  ;; doing preview action that the user will never see anyway.
  (let ((need-preview
	 (and vm-preview-lines
		   (or (vm-new-flag (car vm-message-pointer))
		       (vm-unread-flag (car vm-message-pointer))
		       vm-preview-read-messages))))
    (save-current-buffer
     (setq vm-system-state 'previewing)
     (setq vm-mime-decoded nil)

     ;; 1a. make sure that the message body is loaded (if needed)
     (when vm-external-fetch-message-for-presentation
       (when (vm-body-to-be-retrieved-of (car vm-message-pointer))
	 (let ((mm (vm-real-message-of (car vm-message-pointer))))
	   ;; the body may arrive after this returns: presentation is where
	   ;; that is what the reader wants -- the message now, its body when
	   ;; the server answers -- and the fetch shows it again then
	   (vm-retrieve-real-message-body mm :fetch t :register t
					  :may-arrive-later t))))
     ;; 1b. create a virtual copy if in a virtual folder
     (when vm-real-buffers
       (vm-make-virtual-copy (car vm-message-pointer)))

     ;; 2. run the message select hooks.
     (save-excursion
       (vm-select-folder-buffer)
       (when (and vm-auto-save-all-attachments
		  (vm-new-flag (car vm-message-pointer)))
	 (vm-mime-auto-save-all-attachments))
       (when (and vm-select-new-message-hook 
		  (vm-new-flag (car vm-message-pointer)))
	    (vm-run-hook-on-message 'vm-select-new-message-hook
				    (car vm-message-pointer)))
       (when (and vm-select-unread-message-hook
		  (vm-unread-flag (car vm-message-pointer)))
	    (vm-run-hook-on-message 'vm-select-unread-message-hook
				    (car vm-message-pointer))))

     ;; 3. prepare the Presentation buffer
     (vm-narrow-for-preview (not need-preview))
     (if (or vm-always-use-presentation
             vm-mime-display-function
             vm-fill-paragraphs-containing-long-lines
             (and vm-display-using-mime
		  (not (vm-mime-plain-message-p (car vm-message-pointer)))))
	 (let ((layout (vm-mm-layout (car vm-message-pointer))))
	   ;; This check is for Bug Report 740755.  USR, 2011-12-24
	   (let ((new-layout (vm-mime-parse-entity-safe 
			      (car vm-message-pointer))))
	     ;; repair the cached layout if necessary
	     (unless (vm-mime-verify-cached-layout layout new-layout)
	       (unless vm-debug
		 (vm-set-mime-layout-of 
		  (car vm-message-pointer) new-layout))))
	   (vm-make-presentation-copy (car vm-message-pointer))
	   (save-current-buffer
	    (vm-replace-buffer-in-windows (current-buffer)
					  vm-presentation-buffer))
	   (set-buffer vm-presentation-buffer)
	   (setq vm-system-state 'previewing)
	   (vm-narrow-for-preview))
       ;; never used because vm-always-use-presentation is t.
       ;; USR 2010-05-07
       (setq vm-presentation-buffer nil)
       (and vm-presentation-buffer-handle
	    (vm-replace-buffer-in-windows vm-presentation-buffer-handle
					  (current-buffer))))

     ;; at this point the current buffer is the presentation buffer
     ;; if we're using one for this message.
     (vm-unbury-buffer (current-buffer))

     
     ;; 4. decode MIME
     (if (and vm-display-using-mime
	      vm-auto-decode-mime-messages
	      vm-mime-decode-for-preview
	      need-preview
	      (if vm-mail-buffer
		  (not (with-current-buffer vm-mail-buffer
			 vm-mime-decoded))
		(not vm-mime-decoded))
	      (not (vm-mime-plain-message-p (car vm-message-pointer))))
	 (if (eq vm-preview-lines 0)
	     (progn
	       (vm-decode-mime-message-headers (car vm-message-pointer))
	       (vm-energize-urls)
	       (vm-highlight-headers-maybe)
	       (vm-fontify-body-maybe)
	       (vm-energize-headers-and-xfaces))
	   ;; restrict the things that are auto-displayed, since
	   ;; decode-for-preview is meant to allow a numeric
	   ;; vm-preview-lines to be useful in the face of multipart
	   ;; messages.
	   ;; But why restrict the external viewers?  USR, 2011-02-08
	   (let ((vm-mime-auto-displayed-content-type-exceptions
		  (if (integerp vm-preview-lines)
		      (cons "message/external-body"
			    vm-mime-auto-displayed-content-type-exceptions)
		    vm-mime-auto-displayed-content-type-exceptions))
		 )
	     (condition-case data
		 (progn
		   (vm-decode-mime-message)
		   ;; reset vm-mime-decoded so that when the user
		   ;; opens the message completely, the full MIME
		   ;; display will happen.
		   ;; As an experiment, we turn off the double
		   ;; decoding and see what happens. USR, 2010-02-01
		   (if (and vm-mime-decode-for-show
			    vm-mail-buffer 
			    (vm-body-retrieved-of (car vm-message-pointer)))
			(with-current-buffer vm-mail-buffer
			  (setq vm-mime-decoded nil)))
		   )
	       (vm-mime-error (vm-set-mm-layout-display-error
			       (vm-mime-layout-of (car vm-message-pointer))
			       (car (cdr data)))
			      (vm-warn 0 2 "%s: %s" 
				       (buffer-name vm-mail-buffer)
				       (car (cdr data)))))
	     (vm-narrow-for-preview)))
       ;; if no MIME decoding is needed
       (vm-energize-urls-in-message-region)
       (vm-highlight-headers-maybe)
       (vm-fontify-body-maybe)
       (vm-energize-headers-and-xfaces))

     ;; 6. Go to the text of message
     (if (and vm-honor-page-delimiters need-preview)
	 (vm-narrow-to-page))
     (goto-char (vm-text-of (car vm-message-pointer)))

     ;; 7. If we have a window, set window start appropriately.
     (let ((w (vm-get-visible-buffer-window (current-buffer))))
       (when w
	 (set-window-start w (point-min))
	 (set-window-point w (vm-text-of (car vm-message-pointer)))))

     ;; 8. Show the full message if necessary
     (if need-preview
	 (vm-update-summary-and-mode-line)
       (vm-show-current-message))

     ;; 9. Fold the headers that run long, if asked
     (when vm-enable-shrunken-headers
       (vm-shrunken-headers))))

  (when vm-handle-return-receipts
    (vm-handle-return-receipt))
  (vm-run-hook-on-message 'vm-select-message-hook (car vm-message-pointer)))

(defalias 'vm-preview-current-message 'vm-present-current-message)

;;; Shrunken headers

;;;###autoload
(defun vm-shrunken-headers-toggle ()
  "Toggle display of shrunken headers."
  (interactive)
  (vm-shrunken-headers 'toggle))

;;;###autoload
(defun vm-shrunken-headers-toggle-this-mouse (&optional event)
  "Toggle display of shrunken headers."
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (vm-shrunken-headers-toggle-this))
(put 'vm-shrunken-headers-toggle-this-mouse 'vm-called-by-vm t)

;;;###autoload
(defun vm-shrunken-headers-toggle-this-widget (widget &rest _event)
  (goto-char (widget-get widget :to))
  (end-of-line)
  (vm-shrunken-headers-toggle-this))

;;;###autoload
(defun vm-shrunken-headers-toggle-this ()
  "Toggle display of shrunken headers."
  (interactive)
  
  (save-excursion
    (if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
        (set-buffer (symbol-value 'vm-mail-buffer)))
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    (let ((o (or (car (vm-shrunken-headers-get-overlays (point)))
                 (car (vm-shrunken-headers-get-overlays
                       (save-excursion (end-of-line)
                                       (forward-char 1)
                                       (point)))))))
      (save-restriction
        (narrow-to-region (- (overlay-start o) 7) (overlay-end o))
        (vm-shrunken-headers 'toggle)
        (widen)))))

(defun vm-shrunken-headers-get-overlays (start &optional end)
  (let ((o-list (if end
                    (overlays-in start end)
                  (overlays-at start))))
    (setq o-list (mapcar (lambda (o)
                           (if (overlay-get o 'vm-shrunken-headers)
                               o
                             nil))
                         o-list)
          o-list (delete nil o-list))))

;;;###autoload
(defun vm-shrunken-headers (&optional toggle)
  "Hide or show headers which occupy more than one line.
Well, one might do it more precisely with only some headers,
but it is sufficient for me!

If the optional argument TOGGLE, then hiding is toggled.

The face used for the visible hidden regions is `vm-shrunken-headers-face' and
the keymap used within that region is `vm-shrunken-headers-keymap'."
  (interactive "P")
  
  (save-excursion 
    (let (headers-start headers-end start end o shrunken modified)
      (if (equal major-mode 'vm-summary-mode)
          (if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
              (set-buffer (symbol-value 'vm-mail-buffer))))
      (if (equal major-mode 'vm-mode)
          (if vm-presentation-buffer
              (set-buffer vm-presentation-buffer)))

      ;; We cannot use the default functions (vm-headers-of, ...) since
      ;; we might also work within a presentation buffer.
      (setq modified (buffer-modified-p))
      (goto-char (point-min))
      (setq headers-start (point-min)
            headers-end (or (re-search-forward "\n\n" (point-max) t)
                            (point-max)))

      (cond (toggle
             (setq shrunken (vm-shrunken-headers-get-overlays
                             headers-start headers-end))
             (while shrunken
               (setq o (car shrunken))
               (let ((w (overlay-get o 'vm-shrunken-headers-widget)))
                 (widget-toggle-action w))
	       (overlay-put o 'invisible (not (overlay-get o 'invisible)))
	       (setq shrunken (cdr shrunken))))
            (t
             (goto-char headers-start)
             (while (re-search-forward "^\\(\\s-+.*\n\\)+" headers-end t)
               (setq start (match-beginning 0) end (match-end 0))
               (setq o (vm-shrunken-headers-get-overlays start end))
               (if o
                   (setq o (car o))
                 (setq o (make-overlay (1- start) end))
                 (overlay-put o 'face 'vm-shrunken-headers-face)
                 (overlay-put o 'mouse-face 'highlight)
                 (overlay-put o 'local-map vm-shrunken-headers-keymap)
                 (overlay-put o 'priority 10000)
                 ;; make a new overlay for the invisibility, the other one we
                 ;; made before is just for highlighting and key-bindings ...
                 (setq o (make-overlay start end))
                 (overlay-put o 'vm-shrunken-headers t)
		 (goto-char (1- start))
		 (overlay-put o 'start-closed nil)
		 (overlay-put o 'vm-shrunken-headers-widget
			      (widget-create 'visibility
					     :action
                                      'vm-shrunken-headers-toggle-this-widget))
		 (overlay-put o 'invisible t)))))
      (set-buffer-modified-p modified)
      (goto-char (point-min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vm-show-current-message ()
  "Show the current message in the Presentation Buffer.  MIME decoding
is done if necessary.  (USR, 2010-01-14)"
  ;; It looks like this function can be invoked in both the folder
  ;; buffer as well the presentation buffer, but we need to arrange
  ;; things so that it is always called in a presentation buffer.
  ;; (USR, 2010-05-04)
  (if (and vm-display-using-mime
	   vm-auto-decode-mime-messages
	   (not (vm-folder-buffer-value 'vm-mime-decoded))
	   (not (vm-mime-plain-message-p (car vm-message-pointer))))

      (condition-case data
	  (vm-decode-mime-message)
	(vm-mime-error (vm-set-mm-layout-display-error
			(vm-mime-layout-of (car vm-message-pointer))
			(car (cdr data)))
		       (vm-warn 0 2 "%s: %s" 
				(buffer-name vm-mail-buffer)
				(car (cdr data))))))
  ;; FIXME this probably cause folder corruption by filling the folder instead
  ;; of the presentation copy  ..., RWF, 2008-07
  ;; Well, so, we will check if we are in a presentation buffer! 
  ;; USR, 2010-01-07
  (when (and  (or vm-word-wrap-paragraphs
		  vm-fill-paragraphs-containing-long-lines)
	      (vm-mime-plain-message-p (car vm-message-pointer)))
    (if (null vm-mail-buffer)		; this can't be presentation then
	(if vm-always-use-presentation
	    (progn
	      (vm-make-presentation-copy (car vm-message-pointer))
	      (set-buffer vm-presentation-buffer))
	  ;; FIXME at this point, the folder buffer is being used for
	  ;; display.  Filling will corrupt the folder.
	  (debug "VM internal error #2010.  Please report it")))
    (save-restriction
     (widen)
     (vm-fill-paragraphs-containing-long-lines
      vm-fill-paragraphs-containing-long-lines
      (vm-text-of (car vm-message-pointer))
      (vm-text-end-of (car vm-message-pointer)))))
  (save-current-buffer
   (save-excursion
     (save-excursion
       (goto-char (point-min))
       (widen)
       (narrow-to-region (point) (vm-text-end-of (car vm-message-pointer))))
     (if vm-honor-page-delimiters
	 (progn
	   (if (looking-at page-delimiter)
	       (forward-page 1))
	   (vm-narrow-to-page))))
   ;; don't mark the message as read if the user can't see it!
   (if (vm-get-visible-buffer-window (current-buffer))
       (progn
	 (save-excursion
	   (setq vm-system-state 'showing)
	   (if vm-mail-buffer
	       (with-current-buffer vm-mail-buffer 
		 (setq vm-system-state 'showing)))
	   ;; We could be in the presentation buffer here.  Since
	   ;; the presentation buffer's message pointer and sole
	   ;; message are a mockup, they will cause trouble if
	   ;; passed into the undo/update system.  So we switch
	   ;; into the real message buffer to do attribute
	   ;; updates.
	   (vm-select-folder-buffer)
           (vm-run-hook-on-message 'vm-showing-message-hook
				   (car vm-message-pointer))
           (vm-set-new-flag (car vm-message-pointer) nil)
           (vm-set-unread-flag (car vm-message-pointer) nil))
         (vm-update-summary-and-mode-line)
	 (vm-howl-if-eom))
     (vm-update-summary-and-mode-line)))
  )

(defvar vm-headers-exposed nil
  "Whether `vm-expose-hidden-headers' has exposed the headers here.
Buffer-local to the buffer the message is shown in, and reset with it, so
the next message starts with its headers hidden as usual.

The narrowing used to carry this: exposed meant the visible region started
at the message rather than at its visible headers.  With
`vm-honor-page-delimiters' the visible region is a page, whose start says
nothing about the headers, so the state is kept here instead.  Issue #513.")
(make-variable-buffer-local 'vm-headers-exposed)

;;;###autoload
(defun vm-expose-hidden-headers ()
  "Toggle exposing and hiding message headers that are normally not visible."
  (interactive)
  (vm-follow-summary-cursor)
  (save-excursion
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (vm-display nil nil '(vm-expose-hidden-headers)
		'(vm-expose-hidden-headers))
    (save-current-buffer
     (vm-replace-buffer-in-windows (current-buffer) 
				   vm-presentation-buffer))
    (and vm-presentation-buffer
	 (set-buffer vm-presentation-buffer))
    (let* ((exposed (if vm-honor-page-delimiters
			;; The narrowing cannot say: it is a page, and its
			;; start has nothing to do with the headers.  #513
			vm-headers-exposed
		      (= (point-min) (vm-start-of (car vm-message-pointer)))))
	   ;; Where the reader was.  Toggling the headers changes what is
	   ;; narrowed, not the text, so this position stays good.
	   (reading (point)))
      (setq vm-headers-exposed (not exposed))
      (vm-widen-page)
      (goto-char (point-max))
      (widen)
      (if exposed
	  (narrow-to-region (point) (vm-vheaders-of (car vm-message-pointer)))
	(narrow-to-region (point) (vm-start-of (car vm-message-pointer))))
      (goto-char (point-min))
      (let (w)
	(setq w (vm-get-visible-buffer-window (current-buffer)))
	(and w (set-window-point w (point-min)))
	(and w
	     (= (window-start w) (vm-vheaders-of (car vm-message-pointer)))
	     (not exposed)
	     (set-window-start w (vm-start-of (car vm-message-pointer)))))
      (when vm-honor-page-delimiters
	;; Back to the page that was being read, rather than the first one.
	;; `vm-narrow-to-page' narrows to the page point is in, and point was
	;; sent to the top of the message just above -- so pressing `t' on
	;; page three left you looking at page one, which the reporter of
	;; issue #513 took for the command having failed.  The headers are
	;; exposed either way; they may be off screen, which is the price of
	;; not being moved.
	(vm-restore-reading-position reading)
	(vm-narrow-to-page))))
  (when vm-enable-shrunken-headers
    (vm-shrunken-headers)))

(defun vm-restore-reading-position (position)
  "Put point back at POSITION, and the window with it.
Does nothing if POSITION is outside what is visible now."
  (when (and position (<= (point-min) position) (<= position (point-max)))
    (goto-char position)
    (let ((w (vm-get-visible-buffer-window (current-buffer))))
      (when w (set-window-point w position)))))

(defun vm-widen-page ()
  (if (or (> (point-min) (vm-text-of (car vm-message-pointer)))
	  (/= (point-max) (vm-text-end-of (car vm-message-pointer))))
      (narrow-to-region (vm-vheaders-of (car vm-message-pointer))
			(if (or (vm-new-flag (car vm-message-pointer))
				(vm-unread-flag (car vm-message-pointer)))
			    (vm-text-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer))))))

(defun vm-narrow-to-page ()
  (unless (and vm-page-end-overlay
	       (overlay-buffer vm-page-end-overlay))
    (let ((g vm-page-continuation-glyph))
      (setq vm-page-end-overlay (make-overlay (point) (point)))
      (vm-set-extent-property vm-page-end-overlay 'vm-glyph g)
      (vm-set-extent-property vm-page-end-overlay 'before-string g)
      (overlay-put vm-page-end-overlay 'evaporate nil)))
  (save-excursion
    (let (min max (e vm-page-end-overlay))
      (if (or (bolp) (not (save-excursion
			    (beginning-of-line)
			    (looking-at page-delimiter))))
	  (forward-page -1))
      (setq min (point))
      (forward-page 1)
      (if (not (eobp))
	  (beginning-of-line))
      (cond ((/= (point) (vm-text-end-of (car vm-message-pointer)))
	     (vm-set-extent-property e vm-begin-glyph-property
				     (vm-extent-property e 'vm-glyph))
	     (vm-set-extent-endpoints e (point) (point)))
	    (t
	     (vm-set-extent-property e vm-begin-glyph-property nil)))
      (setq max (point))
      (narrow-to-region min max))))

;;;###autoload
(defun vm-beginning-of-message ()
  "Moves to the beginning of the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (vm-widen-page)
  (push-mark)
  (vm-display (current-buffer) t '(vm-beginning-of-message)
	      '(vm-beginning-of-message reading-message))
  (save-current-buffer
    (let ((osw (selected-window)))
      (unwind-protect
	  (progn
	    (select-window (vm-get-visible-buffer-window (current-buffer)))
	    (goto-char (point-min)))
	(if (not (eq osw (selected-window)))
	    (select-window osw)))))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page)))

;;;###autoload
(defun vm-end-of-message ()
  "Moves to the end of the current message, exposing and flagging it read
as necessary."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (vm-widen-page)
  (push-mark)
  (vm-display (current-buffer) t '(vm-end-of-message)
	      '(vm-end-of-message reading-message))
  (save-current-buffer
    (let ((osw (selected-window)))
      (unwind-protect
	  (progn
	    (select-window (vm-get-visible-buffer-window (current-buffer)))
	    (goto-char (point-max)))
	(if (not (eq osw (selected-window)))
	    (select-window osw)))))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page)))

;;;###autoload
(defun vm-next-button (count)
  "Moves to the next button in the current message.
Prefix argument N means move to the Nth next button.
Negative N means move to the Nth previous button.
If there is no next button, an error is signaled and point is not moved.

A button is a highlighted region of text where pressing RETURN
will produce an action.  If the message is being previewed, it is
exposed and marked as read."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (vm-widen-page)
  (vm-display (current-buffer) t '(vm-move-to-next-button)
	      '(vm-move-to-next-button reading-message))
  (select-window (vm-get-visible-buffer-window (current-buffer)))
  (unwind-protect
      (vm-move-to-xxxx-button (vm-abs count) (>= count 0))
    (if vm-honor-page-delimiters
	(vm-narrow-to-page))))
;;;###autoload (autoload 'vm-move-to-next-button "vm-page" nil t)
(defalias 'vm-move-to-next-button 'vm-next-button)

;;;###autoload
(defun vm-previous-button (count)
  "Moves to the previous button in the current message.
Prefix argument N means move to the Nth previous button.
Negative N means move to the Nth next button.
If there is no previous button, an error is signaled and point is not moved.

A button is a highlighted region of text where pressing RETURN
will produce an action.  If the message is being previewed, it is
exposed and marked as read."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (vm-widen-page)
  (vm-display (current-buffer) t '(vm-move-to-previous-button)
	      '(vm-move-to-previous-button reading-message))
  (select-window (vm-get-visible-buffer-window (current-buffer)))
  (unwind-protect
      (vm-move-to-xxxx-button (vm-abs count) (< count 0))
    (if vm-honor-page-delimiters
	(vm-narrow-to-page))))
;;;###autoload (autoload 'vm-move-to-previous-button "vm-page" nil t)
(defalias 'vm-move-to-previous-button 'vm-previous-button)

(defun vm-move-to-xxxx-button (count next)
  (let ((old-point (point))
	(endp (if next 'eobp 'bobp))
	(extent-end-position (if next
				 'vm-extent-end-position
			       'vm-extent-start-position))
	(next-extent-change (if next
				'vm-next-extent-change
			      'vm-previous-extent-change))
	e)
    (while (and (> count 0) (not (funcall endp)))
      (goto-char (funcall next-extent-change (+ (point) (if next 0 -1))))
      (setq e (vm-extent-at (point)))
      (if e
	  (progn
	    (if (vm-extent-property e 'vm-button)
		(vm-decrement count))
	    (goto-char (funcall extent-end-position e)))))
    (if e
	(goto-char (vm-extent-start-position e))
      (goto-char old-point)
      (error "No more buttons"))))

;;;###autoload
(defun vm-isearch-presentation ()
  "Switches to the Presentation buffer and starts isearch."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (let ((target (or vm-presentation-buffer (current-buffer))))
    (if (get-buffer-window-list target)
        (select-window (car (get-buffer-window-list target)))
      (switch-to-buffer target)))
  (isearch-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contributed by Alley Stoughton
;; gnu.emacs.vm.info, 2011-02-26

(provide 'vm-page)
;;; vm-page.el ends here
