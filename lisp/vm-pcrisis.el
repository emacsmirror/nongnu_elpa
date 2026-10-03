;;; vm-pcrisis.el --- Automatic setup for personalities in VM  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1999 Rob Hodges,
;; Copyright (C) 2006 Robert Widhopf, Robert P. Goldman
;; Copyright (C) 2011-2012 Uday S. Reddy
;; Copyright (C) 2024-2026 The VM Developers
;;
;; Original Author: Rob Hodges (Personality Crisis)
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.


;; DOCUMENTATION:
;; -------------
;;
;; Documentation is now in Texinfo format, included
;; in the standard VM distribution.

;;; Code:

(require 'timezone)
(require 'vm-misc)
(require 'vm-minibuf)
(require 'vm-folder)
(require 'vm-summary)
(require 'vm-motion)
(require 'vm-reply)
(eval-when-compile (vm-load-features-silent-when-compiling '(regexp-opt bbdb bbdb-com)))
(eval-when-compile (require 'vm-macro))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function timezone-absolute-from-gregorian "ext:timezone"
		  (month day year))
(declare-function bbdb-buffer "ext:bbdb" ())
(declare-function bbdb-record-set-xfield "ext:bbdb" (record label value))
(declare-function bbdb-record-xfield "ext:bbdb" (record label))
(declare-function bbdb-change-record "ext:bbdb" (record &rest ignored))
(declare-function bbdb-save "ext:bbdb" (&optional prompt noisy))
(declare-function bbdb-record-mail "ext:bbdb" (record))
(declare-function bbdb-records "ext:bbdb" ())
(declare-function bbdb-message-search "ext:bbdb-com" (name mail))
(declare-function bbdb-create-internal "ext:bbdb" (&rest spec))
(declare-function vm-imap-account-name-for-spec "vm-imap" (maildrop-spec))
(declare-function vm-pop-find-name-for-spec "vm-pop" (maildrop-spec))


;; Dummy declarations for variables that are defined in bbdb

(defvar bbdb-records)
(defvar bbdb-file)
(defvar bbdb-records)

;; -------------------------------------------------------------------
;; Variables:
;; -------------------------------------------------------------------

(defgroup vm-pcrisis nil
  "Automatic setup of rule-based mail personalities in VM.
Personality rules allow automatic configuration of From addresses,
signatures, headers, and other mail settings based on the context of
the message being composed (reply, forward, new mail, etc.)."
  :group  'vm)

(defcustom vm-pcrisis-conditions ()
  "*List of conditions which will be checked by pcrisis."
  :type '(repeat (list (choice :tag "Condition name"
			       (symbol) (string))
		       (sexp :tag "Condition")))
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-actions ()
  "*List of actions.
Actions are associated with conditions from `vm-pcrisis-conditions' by one of
`vm-pcrisis-default-rules', `vm-pcrisis-reply-rules',
`vm-pcrisis-forward-rules', `vm-pcrisis-resend-rules',
`vm-pcrisis-mail-rules', `vm-pcrisis-newmail-rules' or
`vm-pcrisis-automorph-rules'. 

These are also the actions from which you can choose when using the newmail
features of Personality Crisis, or the `vm-pcrisis-prompt-for-profile' action.

You may also define an action without associated commands, e.g. \"none\"."
  :type '(repeat (cons (choice :tag "Action name"
			       (symbol) (string))
                       (repeat :tag "Commands" sexp)))
  :group 'vm-pcrisis)

(defun vm-pcrisis-rules-set (symbol value)
  "Used as :set for vm-pcrisis-*-rules variables.
Checks if the condition and all the actions exist."
  (while value
    (let ((condition (caar value))
          (actions   (cdar value)))
      (if (and condition (not (assoc condition vm-pcrisis-conditions)))
          (error "Condition '%s' does not exist!" condition))
      (while actions 
        (if (not (assoc (car actions) vm-pcrisis-actions))
            (error "Action '%s' does not exist!" (car actions)))
        (setq actions (cdr actions))))
    (setq value (cdr value)))
  (set symbol value))


(defun vm-pcrisis-defcustom-rules-type ()
  "Generate :type for vm-pcrisis-*-rules variables."
  `(repeat
    (cons
     (choice :tag "Condition"
	     ,@(mapcar (lambda (c) `(const ,(car c))) vm-pcrisis-conditions)
	     (string))
     (repeat :tag "Actions to run"
	     (choice :tag "Action"
		     ,@(mapcar (lambda (a) `(const ,(car a))) vm-pcrisis-actions)
		     (string))))))

(defcustom vm-pcrisis-default-rules ()
  "A default list of condition-action rules used for replying, forwarding,
resending, composing and automorphing, unless overridden by more
specific variables such as `vm-pcrisis-reply-rules'."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-reply-rules ()
  "A list of condition-action rules used during reply."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-forward-rules ()
  "A list of condition-action rules used when forwarding."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-automorph-rules ()
  "Alist associating conditions with actions from `vm-pcrisis-actions'
when automorphing."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-mail-rules ()
  "An alist associating conditions with actions from `vm-pcrisis-actions'
when composing a message starting from a folder."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-newmail-rules ()
  "An alist associating conditions with actions from `vm-pcrisis-actions'
when composing." 
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-resend-rules ()
  "An alist associating conditions with actions from `vm-pcrisis-actions'
when resending."
  :type (vm-pcrisis-defcustom-rules-type)
;  :set 'vm-pcrisis-rules-set
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-default-profile "default"
  "*The default profile to select if no profile was found."
  :type '(choice (const :tag "None" nil)
                 (string))
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-auto-profiles-file "~/.vmpc-auto-profiles"
  "File in which to save information used by `vm-pcrisis-prompt-for-profile'.
When set to the symbol `BBDB', profiles will be stored there."
  :type '(choice (file)
                 (const BBDB))
  :group 'vm-pcrisis)

(defcustom vm-pcrisis-auto-profiles-expunge-days 100
  "*Number of days after which to expunge old address-profile associations.
Performance may suffer noticeably if this file becomes enormous, but in other
respects it is preferable for this value to be fairly high.  The value that is
right for you will depend on how often you send email to new addresses using
`vm-pcrisis-prompt-for-profile'."
  :type 'integer
  :group 'vm-pcrisis)

(defvar vm-pcrisis-current-state nil
  "The current state of pcrisis.
It is one of `reply', `forward', `resend', `automorph', `mail', or `newmail'.
It controls which actions/functions can/will be run.") 

(defvar vm-pcrisis-current-buffer nil
  "The current buffer, i.e. `none' or `composition'.
It is `none' before running an adviced VM function and `composition' afterward,
i.e. when within the composition buffer.")

(defvar vm-pcrisis-saved-headers-alist nil
  "Alist of headers from the original message saved for later use.")

(defvar vm-pcrisis-actions-to-run nil
  "The actions to run.")

(defvar vm-pcrisis-true-conditions nil
  "The true conditions.")

(defvar vm-pcrisis-auto-profiles nil
  "The auto profiles as stored in `vm-pcrisis-auto-profiles-file'.")

;; An "exerlay" is an overlay in FSF Emacs and an extent in XEmacs.
;; It's not a real type; it's just the way I'm dealing with the damn
;; things to produce containers for the signature and pre-signature
;; which can be highlighted etc. and work on both platforms.

(defvar vm-pcrisis-pre-sig-exerlay ()
  "Don't mess with this.")

(make-variable-buffer-local 'vm-pcrisis-pre-sig-exerlay)

(defvar vm-pcrisis-sig-exerlay ()
  "Don't mess with this.")

(make-variable-buffer-local 'vm-pcrisis-sig-exerlay)

;; These calls to make-face should be eliminated, and defface used
;; instead. USR 2016-10-26
(defvar vm-pcrisis-pre-sig-face (progn (make-face 'vm-pcrisis-pre-sig-face)
				 (set-face-foreground
				  'vm-pcrisis-pre-sig-face "forestgreen")
				 'vm-pcrisis-pre-sig-face)
  "Face used for highlighting the pre-signature.")

(defvar vm-pcrisis-sig-face (progn (make-face 'vm-pcrisis-sig-face)
			     (set-face-foreground 'vm-pcrisis-sig-face
						  "steelblue")
			     'vm-pcrisis-sig-face)
  "Face used for highlighting the signature.")

(defvar vm-pcrisis-intangible-pre-sig 'nil
  "Whether to forbid the cursor from entering the pre-signature.")

(defvar vm-pcrisis-intangible-sig 'nil
  "Whether to forbid the cursor from entering the signature.")

(defcustom vm-pcrisis-expect-default-signature t
  "Whether a signature is inserted by something other than Personality Crisis.
Emacs inserts one when `mail-signature' is set, taking it from that variable or
from the file `mail-signature-file' names, and VM does that as it builds a
composition.  Personality Crisis can only act on a signature whose extent it
knows, so with this unset `vm-pcrisis-signature' neither replaces nor deletes
that one: it looked to two readers of #540 like an action that had run and done
nothing.  Set, which is the default, the signature already in a composition is
found and comes under the same control as one Personality Crisis inserted
itself.

What is looked for is a line of exactly \"-- \", the separator Emacs writes,
and the signature is everything from there to the end of the composition.  A
signature in quoted text is prefixed by `vm-included-text-prefix' and so is not
that line.  Unset this to keep a signature action off a signature Personality
Crisis did not insert."
  :group 'vm-pcrisis
  :type 'boolean)


;; -------------------------------------------------------------------
;; Some easter-egg functionality:
;; -------------------------------------------------------------------

(defun vm-pcrisis-my-identities (&rest identities)
  "Set up Personality Crisis with the given IDENTITIES, replacing what is there.

Each of IDENTITIES becomes an action that puts that address in the `From'
header, and a composition asks which one to use, remembering the answer for
that correspondent.  It asks every time, including for a correspondent it
already has a profile for, since it installs `vm-pcrisis-prompt-for-profile'
with its PROMPT argument set.

This is a quick start for someone with no rules, not something to add to
rules of your own: `vm-pcrisis-conditions', `vm-pcrisis-actions' and
`vm-pcrisis-default-rules' are assigned outright, so anything set in them
before this call is discarded.  For a set of identities chosen by a rule,
write the rules instead, and use `vm-pcrisis-none-true-yet' for the
fallback."
  (setq vm-pcrisis-conditions    '(("always true" t))
        vm-pcrisis-default-rules '(("always true" "prompt for a profile"))
        vm-pcrisis-actions       '(("prompt for a profile" 
			      (vm-pcrisis-prompt-for-profile t t))))
  (setq vm-pcrisis-actions
        (append (mapcar
                 (lambda (identity)
		   `(,identity
		     (vm-pcrisis-substitute-header "From" ,identity)))
                 identities)
                vm-pcrisis-actions)))

(defun vm-pcrisis-header-field-for-point ()
  "*Return a string indicating the mail header field point is in.
If point is not in a header field, returns nil."
  (save-excursion
    (unless (save-excursion
	      (re-search-backward 
               (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
	       (point-min) t))
      (re-search-backward "^\\([^ \t\n:]+\\):")
      (match-string 1))))

(defun vm-pcrisis-tab-header-or-tab-stop (&optional backward)
  "*If in a mail header field, moves to next useful header or body.
When moving to the message body, calls the `vm-pcrisis-automorph' function.
If within the message body, runs `tab-to-tab-stop'.
If BACKWARD is specified and non-nil, moves to previous useful header
field, whether point is in the body or the headers.
\"Useful header fields\" are currently, in order, \"To\" and
\"Subject\"."
  (interactive)
  (let ((curfield) (nextfield) (useful-headers '("To" "Subject")))
    (if (or (setq curfield (vm-pcrisis-header-field-for-point))
	    backward)
	(progn
	  (setq nextfield
		(- (length useful-headers)
		   (length (member curfield useful-headers))))
	  (if backward
	      (setq nextfield (nth (1- nextfield) useful-headers))
	    (setq nextfield (nth (1+ nextfield) useful-headers)))
	  (if nextfield
	      (mail-position-on-field nextfield)
	    (mail-text)
	    (vm-pcrisis-automorph))
	  )
      (tab-to-tab-stop)
      )))
(put 'vm-pcrisis-tab-header-or-tab-stop 'vm-called-by-vm t)

(defun vm-pcrisis-backward-tab-header-or-tab-stop ()
  "*Wrapper for `vm-pcrisis-tab-header-or-tab-stop' with BACKWARD set."
  (interactive)
  (vm-pcrisis-tab-header-or-tab-stop t))
(put 'vm-pcrisis-backward-tab-header-or-tab-stop 'vm-called-by-vm t)


;; -------------------------------------------------------------------
;; Stuff for dealing with exerlays:
;; -------------------------------------------------------------------

(defun vm-pcrisis-set-overlay-insertion-types (overlay start end)
  "Set insertion types for OVERLAY from START to END.
In fact a new copy of OVERLAY with different insertion types at START and END
is created and returned.

START and END should be nil or t -- the marker insertion types at the start
and end.  This seems to be the only way you of changing the insertion types
for an overlay -- save the overlay properties that we care about, create a new
overlay with the new insertion types, set its properties to the saved ones.
Overlays suck.  Extents rule.  XEmacs got this right."
  (let* ((useful-props (list 'face 'intangible 'evaporate)) (saved-props)
	 (i 0) (len (length useful-props)) (startpos) (endpos) (new-ovl))
    (while (< i len)
      (setq saved-props (append saved-props (cons
		       (overlay-get overlay (nth i useful-props)) ())))
      (setq i (1+ i)))
    (setq startpos (overlay-start overlay))
    (setq endpos (overlay-end overlay))
    (delete-overlay overlay)
    (if (and startpos endpos)
	(setq new-ovl (make-overlay startpos endpos (current-buffer)
				    start end))
      (setq new-ovl (make-overlay 1 1 (current-buffer) start end))
      (vm-pcrisis-forcefully-detach-exerlay new-ovl))
    (setq i 0)
    (while (< i len)
      (overlay-put new-ovl (nth i useful-props) (nth i saved-props))
      (setq i (1+ i)))
    new-ovl))


(defun vm-pcrisis-set-exerlay-insertion-types (exerlay start end)
  "Set the insertion types for EXERLAY from START to END.
In other words, EXERLAY is the name of the overlay or extent with a quote in
front.  START and END are the equivalent of the marker insertion types for the
start and end of the overlay/extent."
  (set exerlay (vm-pcrisis-set-overlay-insertion-types (symbol-value exerlay)
						       start end)))


(defun vm-pcrisis-exerlay-start (exerlay)
  "Return buffer position of the start of EXERLAY."
  (overlay-start exerlay))


(defun vm-pcrisis-exerlay-end (exerlay)
  "Return buffer position of the end of EXERLAY."
  (overlay-end exerlay))


(defun vm-pcrisis-move-exerlay (exerlay new-start new-end)
  "Change EXERLAY to cover region from NEW-START to NEW-END."
  (move-overlay exerlay new-start new-end (current-buffer)))


(defun vm-pcrisis-set-exerlay-detachable-property (exerlay newval)
  "Set the `detachable' or `evaporate' property for EXERLAY to NEWVAL."
  (overlay-put exerlay 'evaporate newval))


(defun vm-pcrisis-set-exerlay-intangible-property (exerlay newval)
  "Set the `intangible' or `atomic' property for EXERLAY to NEWVAL."
  (overlay-put exerlay 'intangible newval))


(defun vm-pcrisis-set-exerlay-face (exerlay newface)
  "Set the face used by EXERLAY to NEWFACE."
  (overlay-put exerlay 'face newface))


(defun vm-pcrisis-forcefully-detach-exerlay (exerlay)
  "Leave EXERLAY in memory but detaches it from the buffer."
  (delete-overlay exerlay))


(defun vm-pcrisis-make-exerlay (startpos endpos)
  "Create a new exerlay spanning from STARTPOS to ENDPOS."
  (vm-make-extent startpos endpos))


(defconst vm-pcrisis-forwarding-terminator-regexp
  "^\\(------- end\\|End of this Digest\\)"
  "A line VM writes to close forwarded text.
The three encapsulations it writes end with `------- end -------' for RFC
934, `------- end of forwarded message -------' for the plain one, and
`End of this Digest' for RFC 1153.  VM writes these itself, so this is
exact for VM's own forwards, which is what it is used on.")

(defun vm-pcrisis-forwarded-text-ends-after (position)
  "Whether a line closing forwarded text appears after POSITION."
  (save-excursion
    (goto-char position)
    (and (re-search-forward vm-pcrisis-forwarding-terminator-regexp nil t) t)))

(defun vm-pcrisis-create-sig-and-pre-sig-exerlays ()
  "Create the extents in which the pre-sig and sig can reside.
Or overlays, in the case of GNU Emacs.  Thus, exerlays."
  (setq vm-pcrisis-pre-sig-exerlay (vm-pcrisis-make-exerlay 1 2))
  (setq vm-pcrisis-sig-exerlay (vm-pcrisis-make-exerlay 3 4))

  (vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-pre-sig-exerlay t)
  (vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-sig-exerlay t)
  (vm-pcrisis-forcefully-detach-exerlay vm-pcrisis-pre-sig-exerlay)
  (vm-pcrisis-forcefully-detach-exerlay vm-pcrisis-sig-exerlay)

  (vm-pcrisis-set-exerlay-face vm-pcrisis-pre-sig-exerlay 'vm-pcrisis-pre-sig-face)
  (vm-pcrisis-set-exerlay-face vm-pcrisis-sig-exerlay 'vm-pcrisis-sig-face)

  (vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-pre-sig-exerlay
					vm-pcrisis-intangible-pre-sig)
  (vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-sig-exerlay
					vm-pcrisis-intangible-sig)
  
  (vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-pre-sig-exerlay t nil)
  (vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-sig-exerlay t nil)

  ;; deal with signatures inserted by other things than vm-pcrisis:

  (if vm-pcrisis-expect-default-signature
      (save-excursion
	(let ((p-max (point-max))
	      (body-start (save-excursion (mail-text) (point)))
	      (sig-start nil))
	  (goto-char p-max)
	  (setq sig-start (re-search-backward "\n-- \n" body-start t))
	  ;; In a new composition the body is the signature and nothing else,
	  ;; so the newline before "-- " is the one ending the header separator
	  ;; line and lies outside the body: the search above cannot reach it,
	  ;; and the signature went unfound (#540).  Attach from the start of
	  ;; the body instead, which also leaves that newline in place when the
	  ;; signature is deleted.
	  (unless sig-start
	    (goto-char body-start)
	    (if (looking-at "-- \n")
		(setq sig-start body-start)))
	  ;; A forwarded message's own signature is not prefixed the way quoted
	  ;; text in a reply is, so the search reaches it, and a composition
	  ;; with no signature of its own took it for one: the forwarded
	  ;; signature and the line closing the encapsulation were both
	  ;; replaced, and the writer's signature ended up inside the forwarded
	  ;; message (#808).  A candidate with such a line after it is not this
	  ;; composition's; one below that line still is.
	  (when (and sig-start (vm-pcrisis-forwarded-text-ends-after sig-start))
	    (setq sig-start nil))
	  (if sig-start
	      (vm-pcrisis-move-exerlay vm-pcrisis-sig-exerlay sig-start p-max))))))
  

;; -------------------------------------------------------------------
;; Functions for vm-pcrisis-actions:
;; -------------------------------------------------------------------

(defmacro vm-pcrisis-composition-buffer (&rest form)
  "Evaluate FORM if in the composition buffer.
That is to say, evaluates the form if you are really in a composition
buffer.  This function should not be called directly, only from within
the `vm-pcrisis-actions' list."
  (list 'if '(eq vm-pcrisis-current-buffer 'composition)
        (list 'eval (cons 'progn form))))

(put 'vm-pcrisis-composition-buffer 'lisp-indent-hook 'defun)

(defmacro vm-pcrisis-pre-function (&rest form)
  "Evaluate FORM if in pre-function state.
That is to say, evaluates the FORM before VM does its thing, whether
that be creating a new mail or a reply.  This function should not be
called directly, only from within the `vm-pcrisis-actions' list."
  (list 'if '(and (eq vm-pcrisis-current-buffer 'none)
                  (not (eq vm-pcrisis-current-state 'automorph)))
        (list 'eval (cons 'progn form))))

(put 'vm-pcrisis-pre-function 'lisp-indent-hook 'defun)

(defvar vm-pcrisis-running-actions nil
  "Non-nil while `vm-pcrisis-run-actions' is evaluating an action list.
Personality Crisis evaluates the list twice, once before the composition
buffer exists and again inside it, so an action that needs a composition has
nothing to do on the first pass.  See `vm-pcrisis-composition-buffer-p'.")

(defun vm-pcrisis-composition-buffer-p (action)
  "Return non-nil if ACTION can act on the current buffer, and signal if never.
Actions that work on the composition are meant to be named in
`vm-pcrisis-actions'
and run by Personality Crisis.  On its first pass the composition does not
exist yet, and an action then has simply nothing to do.  Called by hand
somewhere else it can never have anything to do, and saying nothing is
indistinguishable from having worked: #540 was a report of exactly that."
  (cond ((eq vm-pcrisis-current-buffer 'composition) t)
	(vm-pcrisis-running-actions nil)
	(t (error (concat "%s works on a message composition, and there is"
			  " none here.  Name it in `vm-pcrisis-actions' and in a"
			  " rule so that Personality Crisis runs it as a"
			  " composition begins, with the mode switched on by"
			  " (vm-pcrisis-mode 1)")
		  action))))

(defconst vm-pcrisis-header-start-regexp "^[^ \t\n:]+:"
  "A line that begins a header field, as opposed to continuing one.
A continuation line begins with whitespace, so this cannot match one.")

(defun vm-pcrisis-header-contents-start (entire)
  "Move to the start of the field point is in, and answer where to delete from.
Point is anywhere in the field, which for a folded one is several lines below
where it begins.  With ENTIRE the answer is the start of the field, otherwise
the start of its contents."
  (unless (re-search-backward vm-pcrisis-header-start-regexp nil t)
    (error (concat "No header field around point in this composition, so"
		   " there is nothing to delete: the buffer does not hold"
		   " the headers Personality Crisis expects")))
  (if entire
      (match-beginning 0)
    (goto-char (match-end 0))
    (skip-chars-forward " \t")
    (point)))

(defun vm-pcrisis-delete-header (hdrfield &optional entire)
  "Delete the contents of a HDRFIELD in the current mail message.
If ENTIRE is specified and non-nil, deletes the header field as well.

The field is found by looking back for the line that begins it, not by
looking back for a colon and a space anywhere behind point.  That search was
unbounded and went wrong three ways (emacs-vm/vm#807).  It stopped inside a
value that held one, so deleting a subject of Re: your note left the Re: part
behind and a substitution wrote it in front of the new subject.  A field
written with no space after its colon sent the search back into the field
before it, whose contents were deleted along with this one.  And where that
field was the first in the block, it signalled Search failed."
  (if (vm-pcrisis-composition-buffer-p 'vm-pcrisis-delete-header)
      (save-excursion
	(mail-position-on-field hdrfield)
	(let* ((end (if entire (1+ (point)) (point)))
	       (start (vm-pcrisis-header-contents-start entire)))
	  (delete-region start end)))))


(defun vm-pcrisis-insert-header (hdrfield content)
  "Insert to HDRFIELD the new CONTENT.
Both arguments are strings.  The field can either be present or not,
but if present, HDRCONT will be appended to the current header
contents."
  (if (vm-pcrisis-composition-buffer-p 'vm-pcrisis-insert-header)
      (save-excursion
	(mail-position-on-field hdrfield)
	(insert content))))

(defun vm-pcrisis-substitute-header (hdrfield content)
  "Substitute HDRFIELD with new CONTENT.
Both arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, the
contents will be replaced, otherwise a new header is created."
  (if (vm-pcrisis-composition-buffer-p 'vm-pcrisis-substitute-header)
      (save-excursion
	(vm-pcrisis-delete-header hdrfield)
	(vm-pcrisis-insert-header hdrfield content))))

(defun vm-pcrisis-add-header (hdrfield content)
  "Add HDRFIELD with CONTENT if it is not present already.
Both arguments are strings.  
If a header field with the same CONTENT is present already nothing will be
done, otherwise  a new field with the same name and the new CONTENT will be
added to the message.

This is suitable for FCC, which can be specified multiple times."
  (when (vm-pcrisis-composition-buffer-p 'vm-pcrisis-add-header)
    ;; The headers to compare against are the composition's own, and read
    ;; from the buffer rather than through `vm-pcrisis-get-header-contents', which
    ;; answers for the message being replied to, or
    ;; `vm-pcrisis-get-current-header-contents', which answers only while
    ;; automorphing.  Both return nil in a composition, and `vm-pcrisis-split' then
    ;; choked on it (#576).
    (let ((prev-contents (save-excursion
			   (save-restriction
			     (widen)
			     (mail-fetch-field hdrfield nil nil t)))))
      ;; don't add this new header if it's already there
      (unless (member content prev-contents)
	(save-excursion
	  (or (mail-position-on-field hdrfield t) ; after an existing one
	      (mail-position-on-field "to"))
	  (unless (eq (aref hdrfield (1- (length hdrfield))) ?:)
	    (setq hdrfield (concat hdrfield ":")))
	  (insert "\n" hdrfield " ")
	  (insert content))))))

(defun vm-pcrisis-get-current-header-contents (hdrfield &optional clump-sep)
  "Return the contents of HDRFIELD in the current mail message.
Returns an empty string if the header doesn't exist.  HDRFIELD should
be a string.  If the string CLUMP-SEP is specified, it means to return
the contents of all headers matching the regexp HDRFIELD, separated by
CLUMP-SEP."
  ;; This code is based heavily on vm-get-header-contents and vm-match-header.
  ;; Thanks Kyle :)
  ;; Reading the current buffer's headers is a property of the buffer, not of
  ;; what Personality Crisis is in the middle of.  Gated on `automorph' alone
  ;; this returned nil in a composition, against its own documentation, and
  ;; `vm-pcrisis-replace-or-add-in-header' then silently did nothing (#578).
  (if (or (eq vm-pcrisis-current-state 'automorph)
	  (eq vm-pcrisis-current-buffer 'composition))
      (save-excursion
	(let ((contents nil) (header-name-regexp "\\([^ \t\n:]+\\):")
	      (case-fold-search t) (temp-contents) (end-of-headers) (regexp))
          (if (not (listp hdrfield))
              (setq hdrfield (list hdrfield)))
	  ;; find the end of the headers:
	  (goto-char (point-min))
	  (or (re-search-forward
               (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
               nil t)
              (error "Cannot find mail-header-separator %S in buffer %S"
                     mail-header-separator (current-buffer)))
	  (setq end-of-headers (match-beginning 0))
	  ;; now rip through finding all the ones we want:
          (while hdrfield
            (setq regexp (concat "^\\(" (car hdrfield) "\\)"))
            (goto-char (point-min))
            (while (and (or (null contents) clump-sep)
                        (re-search-forward regexp end-of-headers t)
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (let (header-cont-start header-cont-end)
                            (if (if (not clump-sep)
                                    (and (looking-at (car hdrfield))
                                         (looking-at header-name-regexp))
                                  (looking-at header-name-regexp))
                                (save-excursion
                                  (goto-char (match-end 0))
                                  ;; skip leading whitespace
                                  (skip-chars-forward " \t")
                                  (setq header-cont-start (point))
                                  (forward-line 1)
                                  (while (looking-at "[ \t]")
                                    (forward-line 1))
                                  ;; drop the trailing newline
                                  (setq header-cont-end (1- (point)))))
                            (setq temp-contents
                                  (buffer-substring header-cont-start
                                                    header-cont-end)))))
              (if contents
                  (setq contents
                        (concat contents clump-sep temp-contents))
                (setq contents temp-contents)))
            (setq hdrfield (cdr hdrfield)))

	  (if (null contents)
	      (setq contents ""))
	  contents ))))

(defun vm-pcrisis-get-current-body-text ()
  "Return the body text of the mail message in the current buffer."
  (if (eq vm-pcrisis-current-state 'automorph)
      (save-excursion
	(goto-char (point-min))
	(let ((start (re-search-forward
		      (concat "^" (regexp-quote mail-header-separator) "$")))
	      (end (point-max)))
	  (buffer-substring start end)))))


(defun vm-pcrisis-get-replied-header-contents (hdrfield &optional clump-sep)
  "Return the contents of HDRFIELD in the message being replied to.
If that header does not exist, returns an empty string.  If the string
CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP."
  (if (and (eq vm-pcrisis-current-buffer 'none)
	   (memq vm-pcrisis-current-state '(reply forward resend mail)))
      (let ((mp (car (vm-select-operable-messages
		      1 (vm-interactive-p) "Operate on")))
            content c)
        (if (not (listp hdrfield))
           (setq hdrfield (list hdrfield)))
        (while hdrfield
          (setq c (vm-get-header-contents mp (car hdrfield) clump-sep))
          (if c (setq content (cons c content)))
          (setq hdrfield (cdr hdrfield)))
        (or (mapconcat 'identity content "\n") ""))))

(defun vm-pcrisis-get-header-contents (hdrfield &optional clump-sep)
 "Return the contents of HDRFIELD."
 (cond ((and (eq vm-pcrisis-current-buffer 'none)
             (memq vm-pcrisis-current-state '(reply forward resend mail)))
        (vm-pcrisis-get-replied-header-contents hdrfield clump-sep))
       ((eq vm-pcrisis-current-state 'automorph)
        (vm-pcrisis-get-current-header-contents hdrfield clump-sep))))

(defun vm-pcrisis-get-replied-body-text ()
  "Return the body text of the message being replied to."
  (if (and (eq vm-pcrisis-current-buffer 'none)
	   (memq vm-pcrisis-current-state '(reply forward resend mail)))
      (save-excursion
	(let* ((mp (car (vm-select-operable-messages
			 1 (vm-interactive-p) "Operate on")))
	       (message (vm-real-message-of mp))
	       start end)
	  (set-buffer (vm-buffer-of message))
	  (save-restriction
	    (widen)
	    (setq start (vm-text-of message))
	    (setq end (vm-end-of message))
	    (buffer-substring start end))))))

(defun vm-pcrisis-save-replied-header (hdrfield)
  "Save the contents of HDRFIELD in `vm-pcrisis-saved-headers-alist'.
Does nothing if that header doesn't exist."
  (let ((hdrcont (vm-pcrisis-get-replied-header-contents hdrfield)))
  (if (and (eq vm-pcrisis-current-buffer 'none)
	   (memq vm-pcrisis-current-state '(reply forward resend mail))
	   (not (equal hdrcont "")))
      (add-to-list 'vm-pcrisis-saved-headers-alist (cons hdrfield hdrcont)))))

(defun vm-pcrisis-get-saved-header (hdrfield)
  "Return the contents of HDRFIELD from `vm-pcrisis-saved-headers-alist'.
The alist in question is created by `vm-pcrisis-save-replied-header'."
  (if (and (eq vm-pcrisis-current-buffer 'composition)
	   (memq vm-pcrisis-current-state '(reply forward resend mail)))
      (cdr (assoc hdrfield vm-pcrisis-saved-headers-alist))))

(defun vm-pcrisis-substitute-replied-header (dest src)
  "Substitute header DEST with content from SRC.
For example, if the address you want to send your reply to is the same
as the contents of the \"From\" header in the message you are replying
to, use (vm-pcrisis-substitute-replied-header \"To\" \"From\"."
  (if (memq vm-pcrisis-current-state '(reply forward resend mail))
      (progn
	(if (eq vm-pcrisis-current-buffer 'none)
	    (vm-pcrisis-save-replied-header src))
	(if (eq vm-pcrisis-current-buffer 'composition)
	    (vm-pcrisis-substitute-header dest (vm-pcrisis-get-saved-header src))))))

(defun vm-pcrisis-get-header-extents (hdrfield)
  "Return buffer positions (START . END) for the contents of HDRFIELD.
If HDRFIELD does not exist, return nil."
  (if (eq vm-pcrisis-current-buffer 'composition)
      (save-excursion
        (let ((header-name-regexp "^\\([^ \t\n:]+\\):") (start) (end))
          (setq end
                (if (mail-position-on-field hdrfield t)
                    (point)
                  nil))
          (setq start
                (if (re-search-backward header-name-regexp (point-min) t)
                    (match-end 0)
                  nil))
          (and start end (<= start end) (cons start end))))))

(defun vm-pcrisis-substitute-within-header
  (hdrfield regexp to-string &optional append-if-no-match sep)
  "Replace in HDRFIELD strings matched by  REGEXP with TO-STRING.
HDRFIELD need not exist.  TO-STRING may contain references to groups
within REGEXP, in the same manner as `replace-regexp'.  If REGEXP is
not found in the header contents, and APPEND-IF-NO-MATCH is t,
TO-STRING will be appended to the header contents (with HDRFIELD being
created if it does not exist).  In this case, if the string SEP is
specified, it will be used to separate the previous header contents
from TO-STRING, unless HDRFIELD has just been created or was
previously empty."
  (if (eq vm-pcrisis-current-buffer 'composition)
      (save-excursion
        (let ((se (vm-pcrisis-get-header-extents hdrfield)) (found))
          (if se
              ;; HDRFIELD exists
              (save-restriction
                (narrow-to-region (car se) (cdr se))
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (setq found t)
                  (replace-match to-string))
                (if (and (not found) append-if-no-match)
                    (progn
                      (goto-char (cdr se))
                      (if (and sep (not (equal (car se) (cdr se))))
                          (insert sep))
                      (insert to-string))))
            ;; HDRFIELD does not exist
            (if append-if-no-match
                (progn
                  (mail-position-on-field hdrfield)
                  (insert to-string))))))))


(defun vm-pcrisis-replace-or-add-in-header (hdrfield regexp hdrcont &optional sep)
  "Replace in HDRFIELD the match of REGEXP with HDRCONT.
All arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, HDRCONT
will be appended and if SEP is none nil it will be used as separator.

I use this function to modify recipients in the TO-header.
e.g.
 (vm-pcrisis-replace-or-add-in-header \"To\" \"[Rr]obert Fenk[^,]*\"
                                     \"Robert Fenk\" \", \"))"
  (when (vm-pcrisis-composition-buffer-p 'vm-pcrisis-replace-or-add-in-header)
    (let ((hdr (vm-pcrisis-get-current-header-contents hdrfield))
	  (old-point (point)))
      (vm-pcrisis-delete-header hdrfield)
      (setq hdr
	    (cond ((or (null hdr) (equal hdr ""))
		   ;; Nothing there to replace or to separate from.
		   hdrcont)
		  ((string-match regexp hdr)
		   (vm-replace-in-string hdr regexp hdrcont))
		  (sep (concat hdr sep hdrcont))
		  (t (concat hdr hdrcont))))
      (vm-pcrisis-insert-header hdrfield hdr)
      (goto-char old-point))))

(defun vm-pcrisis-insert-signature (sig &optional pos)
  "Insert SIG at the end of `vm-pcrisis-sig-exerlay'.
SIG is a string.  If it is the name of a file, its contents is inserted --
otherwise the string itself is inserted.  Optional parameter POS means insert
the signature at POS if `vm-pcrisis-sig-exerlay' is detached."
  (if (eq vm-pcrisis-current-buffer 'composition)
      (progn
	(let ((end (or (vm-pcrisis-exerlay-end vm-pcrisis-sig-exerlay) pos)))
	  (save-excursion
	    (vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-sig-exerlay nil t)
	    (vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-sig-exerlay nil)
	    (vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-sig-exerlay nil)
	    (unless end
	      (setq end (point-max))
	      (vm-pcrisis-move-exerlay vm-pcrisis-sig-exerlay end end))
	    (if (and pos (not (vm-pcrisis-exerlay-end vm-pcrisis-sig-exerlay)))
		(vm-pcrisis-move-exerlay vm-pcrisis-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n-- \n")
	    (if (and (file-exists-p sig)
		     (file-readable-p sig)
		     (not (equal sig "")))
		(insert-file-contents sig)
	      (insert sig)))
	  (vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-sig-exerlay
						vm-pcrisis-intangible-sig)
	  (vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-sig-exerlay t)
	  (vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-sig-exerlay t nil)))))
    

(defun vm-pcrisis-delete-signature ()
  "Deletes the contents of `vm-pcrisis-sig-exerlay'."
  (when (and (eq vm-pcrisis-current-buffer 'composition)
             ;; make sure it's not detached first:
             (vm-pcrisis-exerlay-start vm-pcrisis-sig-exerlay))
    (delete-region (vm-pcrisis-exerlay-start vm-pcrisis-sig-exerlay)
                   (vm-pcrisis-exerlay-end vm-pcrisis-sig-exerlay))
    (vm-pcrisis-forcefully-detach-exerlay vm-pcrisis-sig-exerlay)))


(defun vm-pcrisis-report-an-unknown-signature ()
  "Say what to set when the composition holds a signature not ours to act on.
Personality Crisis acts on a signature whose extent it knows, and knows one it
did not insert only when `vm-pcrisis-expect-default-signature' says to look
for it.  Without that, a rule saying (vm-pcrisis-signature \"\") deleted
nothing and said nothing, which is indistinguishable from having worked: #540
was reported twice over, the second time by a reader who had the fix and not
the setting."
  (when (and (not vm-pcrisis-expect-default-signature)
	     (save-excursion
	       (mail-text)
	       (re-search-forward "^-- $" nil t)))
    (vm-warn 0 2 (concat "This composition has a signature Personality Crisis"
			 " did not insert: set"
			 " vm-pcrisis-expect-default-signature to act on it"))))

(defun vm-pcrisis-signature (sig)
  "Remove a current signature if present, and replace it with SIG.
If the string SIG is the name of a readable file, its contents are
inserted as the signature; otherwise SIG is inserted literally.  If
SIG is the empty string (\"\"), the current signature is deleted if
present, and that's all.  A signature Personality Crisis did not insert itself
is only known to it when `vm-pcrisis-expect-default-signature' is set.  Where
that is unset and the composition has one anyway, this says so rather than
doing nothing quietly."
  (if (vm-pcrisis-composition-buffer-p 'vm-pcrisis-signature)
      (let ((pos (vm-pcrisis-exerlay-start vm-pcrisis-sig-exerlay)))
	(unless pos
	  (vm-pcrisis-report-an-unknown-signature))
	(save-excursion
	  (vm-pcrisis-delete-signature)
	  (if (not (equal sig ""))
	      (vm-pcrisis-insert-signature sig pos))))))
  

(defun vm-pcrisis-insert-pre-signature (pre-sig &optional pos)
  "Insert PRE-SIG at the end of `vm-pcrisis-pre-sig-exerlay'.
PRE-SIG is a string.  If it's the name of a file, the file's contents
are inserted; otherwise the string itself is inserted.  Optional
parameter POS means insert the pre-signature at position POS if
`vm-pcrisis-pre-sig-exerlay' is detached."
  (if (eq vm-pcrisis-current-buffer 'composition)
      (progn
	(let ((end (or (vm-pcrisis-exerlay-end vm-pcrisis-pre-sig-exerlay) pos))
	      (sigstart (vm-pcrisis-exerlay-start vm-pcrisis-sig-exerlay)))
	  (save-excursion
	    (vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-pre-sig-exerlay nil t)
	    (vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-pre-sig-exerlay nil)
	    (vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-pre-sig-exerlay nil)
	    (unless end
	      (if sigstart
		  (setq end sigstart)
		(setq end (point-max)))
	      (vm-pcrisis-move-exerlay vm-pcrisis-pre-sig-exerlay end end))
	    (if (and pos (not (vm-pcrisis-exerlay-end vm-pcrisis-pre-sig-exerlay)))
		(vm-pcrisis-move-exerlay vm-pcrisis-pre-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n")
	    (if (and (file-exists-p pre-sig)
		     (file-readable-p pre-sig)
		     (not (equal pre-sig "")))
		(insert-file-contents pre-sig)
	      (insert pre-sig))))
	(vm-pcrisis-set-exerlay-intangible-property vm-pcrisis-pre-sig-exerlay
					      vm-pcrisis-intangible-pre-sig)
	(vm-pcrisis-set-exerlay-detachable-property vm-pcrisis-pre-sig-exerlay t)
	(vm-pcrisis-set-exerlay-insertion-types 'vm-pcrisis-pre-sig-exerlay t nil))))


(defun vm-pcrisis-delete-pre-signature ()
  "Deletes the contents of `vm-pcrisis-pre-sig-exerlay'."
  ;; make sure it's not detached first:
  (if (eq vm-pcrisis-current-buffer 'composition)
      (if (vm-pcrisis-exerlay-start vm-pcrisis-pre-sig-exerlay)
	  (progn
	    (delete-region (vm-pcrisis-exerlay-start vm-pcrisis-pre-sig-exerlay)
			   (vm-pcrisis-exerlay-end vm-pcrisis-pre-sig-exerlay))
	    (vm-pcrisis-forcefully-detach-exerlay vm-pcrisis-pre-sig-exerlay)))))


(defun vm-pcrisis-pre-signature (pre-sig)
  "Insert PRE-SIG at the end of `vm-pcrisis-pre-sig-exerlay' removing last pre-sig."
  (if (vm-pcrisis-composition-buffer-p 'vm-pcrisis-pre-signature)
      (let ((pos (vm-pcrisis-exerlay-start vm-pcrisis-pre-sig-exerlay)))
	(save-excursion
	  (vm-pcrisis-delete-pre-signature)
	  (if (not (equal pre-sig ""))
	      (vm-pcrisis-insert-pre-signature pre-sig pos))))))


(defun vm-pcrisis-gregorian-days ()
  "Return the number of days elapsed since December 31, 1 B.C."
  ;; this code stolen from gnus-util.el :)
  (let ((tim (decode-time (current-time))))
    (timezone-absolute-from-gregorian
     (nth 4 tim) (nth 3 tim) (nth 5 tim))))


;;;###autoload
(defun vm-pcrisis-load-auto-profiles ()
  "Initialise `vm-pcrisis-auto-profiles' from `vm-pcrisis-auto-profiles-file'."
  (interactive)
  (setq vm-pcrisis-auto-profiles nil)
  (if (eq vm-pcrisis-auto-profiles-file 'BBDB)
      (let ((records (bbdb-records))
            profile rec nets)
        (while records
          (setq rec (car records)
                profile (bbdb-record-xfield rec 'vmpc-profile))
          (when (and profile (> (length profile) 0))
            (setq nets (bbdb-record-mail rec))
            (while nets
              (setq vm-pcrisis-auto-profiles (cons (cons (car nets) (read profile))
                                             vm-pcrisis-auto-profiles)
                    nets (cdr nets))))
          (setq records (cdr records)))
        (setq vm-pcrisis-auto-profiles (reverse vm-pcrisis-auto-profiles)))
    (when (and (file-exists-p vm-pcrisis-auto-profiles-file) ;
               (file-readable-p vm-pcrisis-auto-profiles-file))
      (with-current-buffer (get-buffer-create "*pcrisis-temp*")
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(insert-file-contents vm-pcrisis-auto-profiles-file)
	(goto-char (point-min))
	(setq vm-pcrisis-auto-profiles (read (current-buffer)))
	(kill-buffer (current-buffer))))))


(defun vm-pcrisis-save-auto-profiles ()
  "Save `vm-pcrisis-auto-profiles' to `vm-pcrisis-auto-profiles-file'."
  (when (not (eq vm-pcrisis-auto-profiles-file 'BBDB))
    (if (not (file-writable-p vm-pcrisis-auto-profiles-file))
        ;; if file is not writable, signal an error:
        (error "Error: P-Crisis could not write to file %s"
               vm-pcrisis-auto-profiles-file))
    (with-current-buffer (get-buffer-create "*pcrisis-temp*")
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (goto-char (point-min))
      (pp vm-pcrisis-auto-profiles (current-buffer))
      (write-region (point-min) (point-max)
                    vm-pcrisis-auto-profiles-file nil 'quietly)
      (kill-buffer (current-buffer)))))
    
;;;###autoload
(defun vm-pcrisis-fix-auto-profiles-file ()
  "Change `vm-pcrisis-auto-profiles-file' to the format used by v0.82+."
  (interactive)
  (vm-pcrisis-load-auto-profiles)
  (let ((len (length vm-pcrisis-auto-profiles)) (i 0) (day))
    (while (< i len)
      (setq day (cddr (nth i vm-pcrisis-auto-profiles)))
      (if (consp day)
	  (setcdr (cdr (nth i vm-pcrisis-auto-profiles)) (car day)))
      (setq i (1+ i))))
  (vm-pcrisis-save-auto-profiles)
  (setq vm-pcrisis-auto-profiles ()))


;;;###autoload
(defun vm-pcrisis-migrate-profiles-to-BBDB ()
  "Migrate the profiles stored in `vm-pcrisis-auto-profiles-file' to the BBDB.

This will automatically create records if they do not exist and add the new
field `vmpc-profile' to the records which is a sexp not meant to be edited."
  (interactive)
  ;; `bbdb-message-search' lives in bbdb-com.el and BBDB does not autoload it,
  ;; where the `bbdb-search' this replaced was in bbdb.el.  VM never requires
  ;; BBDB itself, so ask for the file that has it (#549).
  (require 'bbdb-com)
  (if (eq vm-pcrisis-auto-profiles-file 'BBDB)
      (error "`vm-pcrisis-auto-profiles-file' has been migrated already."))
  (unless vm-pcrisis-auto-profiles
    (vm-pcrisis-load-auto-profiles))
  ;; create a BBDB backup
  (bbdb-save)
  (copy-file (expand-file-name bbdb-file)
             (concat (expand-file-name bbdb-file) "-vmpc-profile-migration-backup"))
  ;; now migrate the profiles 
  (let ((profiles vm-pcrisis-auto-profiles)
        p addr rec)
    (while profiles
      (setq p (car profiles)
            addr (car p)
            ;; This passed BBDB 2.x's positional arguments to `bbdb-search',
            ;; whose modern form takes keywords and is a macro besides, so it
            ;; could not have worked either way.  Issue #549.
            rec (car (bbdb-message-search nil addr)))
      (when (not rec)
        ;; Keywords, and not the positional arguments this used: BBDB 3
        ;; reordered them, so `addr' was landing in the record's `aka' field
        ;; rather than its mail, and the profile was attached to a record
        ;; that could never be found again.
        (setq rec (bbdb-create-internal :name "?" :mail addr)))
      (bbdb-record-set-xfield rec 'vmpc-profile (format "%S" (cdr p)))
      ;; Registers the change with BBDB, which 2.x's `bbdb-record-putprop'
      ;; did itself.  Without it the field is set in a record BBDB does not
      ;; know has changed, and the next save writes the old value.
      (bbdb-change-record rec)
      (setq profiles (cdr profiles))))
  ;; move old profiles file out of the way
  (rename-file vm-pcrisis-auto-profiles-file
               (concat vm-pcrisis-auto-profiles-file "-migrated-to-BBDB"))
  ;; switch to BBDB mode
  (customize-save-variable 'vm-pcrisis-auto-profiles-file 'BBDB)
  (message "`vm-pcrisis-auto-profiles-file' has been set to 'BBDB"))

(defun vm-pcrisis-get-profile-for-address (addr)
  "Return profile for ADDR."
  (unless vm-pcrisis-auto-profiles
    (vm-pcrisis-load-auto-profiles))
  ;; TODO: BBDB "normalizes" email addresses, i.e. before we had a one-to-one
  ;; mapping of address=>actions, now multiple actions may point to the same
  ;; list of actions.  So either we should update vm-pcrisis-auto-profiles upon
  ;; storing a new profile or directly search BBDB for it, which might be
  ;; slower!
  (let ((prof (cadr (assoc addr vm-pcrisis-auto-profiles))))
    (when prof
      ;; we found a profile for this address and we are still
      ;; using it -- so "touch" the record to ensure it stays
      ;; newer than vm-pcrisis-auto-profiles-expunge-days
      (setcdr (cdr (assoc addr vm-pcrisis-auto-profiles)) (vm-pcrisis-gregorian-days))
      (vm-pcrisis-save-auto-profiles))
    prof))


(defun vm-pcrisis-where-profiles-are-kept ()
  "A name for wherever profiles are kept, for saying so in a message."
  (if (eq vm-pcrisis-auto-profiles-file 'BBDB)
      "the BBDB"
    (abbreviate-file-name (expand-file-name vm-pcrisis-auto-profiles-file))))

(defun vm-pcrisis-save-profile-for-address (addr actions)
  "Save the association ADDR => ACTIONS."
  (let ((today (vm-pcrisis-gregorian-days))
        (old-association (assoc addr vm-pcrisis-auto-profiles))
        profile)

    ;; we store the actions list and the durrent date
    (setq profile (append (list addr actions) today))

    ;; remove old profile
    (when old-association
      ;; now possibly delete it from the BBDB
      (setq vm-pcrisis-auto-profiles (delete old-association vm-pcrisis-auto-profiles))
      (when (and (eq vm-pcrisis-auto-profiles-file 'BBDB) (not actions))
        (require 'bbdb-com)             ; see vm-pcrisis-migrate-profiles-to-BBDB
        (let ((rec (bbdb-message-search nil addr)))
          (when rec
            (bbdb-record-set-xfield (car rec) 'vmpc-profile nil)
            (bbdb-change-record (car rec))))))

    ;; add new profile
    (when actions 
      (setq vm-pcrisis-auto-profiles (cons profile vm-pcrisis-auto-profiles))
      ;; now possibly add it to the BBDB
      (when (eq vm-pcrisis-auto-profiles-file 'BBDB)
        (require 'bbdb-com)             ; see vm-pcrisis-migrate-profiles-to-BBDB
        (let ((rec (car (bbdb-message-search nil addr))))
          (when (not rec)
            (setq rec (bbdb-create-internal :name "?" :mail addr)))
          (bbdb-record-set-xfield rec 'vmpc-profile (format "%S" (cdr profile)))
          (bbdb-change-record rec))))

    ;; expunge old stuff from the list:
    (when vm-pcrisis-auto-profiles-expunge-days
      (setq vm-pcrisis-auto-profiles
            (mapcar (lambda (p)
                      (if (> (- today (cddr p)) 
			     vm-pcrisis-auto-profiles-expunge-days)
                          nil
                        p))
                    vm-pcrisis-auto-profiles))
      (setq vm-pcrisis-auto-profiles (delete nil vm-pcrisis-auto-profiles)))

    ;; save the file 
    (vm-pcrisis-save-auto-profiles)
    ;; Said because this is state the reader did not ask for by name, and with
    ;; REMEMBER t nothing asked them at all: #810 was a reader who found a
    ;; profile in a file they did not know existed (emacs-vm/vm#809).
    (if actions
        (vm-inform 5 "Remembered %s for \"%s\" in %s"
                   actions addr (vm-pcrisis-where-profiles-are-kept))
      (vm-inform 5 "Removed the profile for \"%s\" from %s"
                 addr (vm-pcrisis-where-profiles-are-kept)))))


(defun vm-pcrisis-string-extract-address (str)
  "Find the first email address in the string STR and return it.
If no email address in found in STR, returns nil."
  (if (string-match "[^ \t,<]+@[^ \t,>]+" str)
      (match-string 0 str)))

(defun vm-pcrisis-split (string separators)
  "Return a list by splitting STRING at SEPARATORS and trimming all
whitespace." 
  (let (result
        (not-separators (concat "^" separators)))
    (with-current-buffer (get-buffer-create " *split*")
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (progn
               (skip-chars-forward separators)
               (skip-chars-forward " \t\n\r")
               (not (eobp)))
        (let ((begin (point))
              p)
          (skip-chars-forward not-separators)
          (setq p (point))
          (skip-chars-backward " \t\n\r")
          (setq result (cons (buffer-substring begin (point)) result))
          (goto-char p)))
      (erase-buffer))
    (nreverse result)))

(defconst vm-pcrisis-prompting-functions
  '(vm-pcrisis-prompt-for-profile)
  "The functions that ask which actions to run.")

(defun vm-pcrisis-form-mentions-p (form symbols)
  "Whether FORM mentions any of SYMBOLS anywhere within it."
  (cond ((memq form symbols) t)
        ((consp form) (or (vm-pcrisis-form-mentions-p (car form) symbols)
                          (vm-pcrisis-form-mentions-p (cdr form) symbols)))))

(defun vm-pcrisis-prompting-action-p (name)
  "Whether the action NAME asks which actions to run."
  (vm-pcrisis-form-mentions-p (cdr (assoc name vm-pcrisis-actions))
                              vm-pcrisis-prompting-functions))

(defun vm-pcrisis-only-asks-p (actions)
  "Whether ACTIONS is a non-empty list of nothing but actions that ask.
Such a list is a profile that can never ask and never act (#809)."
  (and actions
       (not (seq-find (lambda (action)
                        (not (vm-pcrisis-prompting-action-p action)))
                      actions))))

(defun vm-pcrisis-worth-remembering-p (actions profile)
  "Whether ACTIONS are worth remembering as the profile for PROFILE.

They are not when every one of them asks which actions to run.  Remembered as
the answer to its own question, such an action makes a profile that asks
nothing and does nothing: the lookup finds it, so nothing is asked, and what
it queues is the asking action, which finds it again (emacs-vm/vm#809).  No
header is ever set and the composition looks configured.

Says so rather than refusing quietly, and answers non-nil for anything else,
including no actions at all, which is how a profile is deleted."
  (if (vm-pcrisis-only-asks-p actions)
      (let ((vm-current-warning nil))
        (vm-warn 0 2 (concat "Not remembering %s for \"%s\": that action is the "
                             "one that asks, so remembering it would mean this "
                             "profile never asks again and never sets anything."
                             "  Answer with the actions to run, such as one that "
                             "sets the From header")
                 actions profile)
        nil)
    t))

(defun vm-pcrisis-read-actions (prompt &optional default)
  "Read a list of actions to run and store it in `vm-pcrisis-actions-to-run'.
The special action \"none\" will result in an empty action list."
  (interactive (list "VMPC actions%s: "))
  (let ((actions ())) ;; (read-count 0) (a nil)
    (setq actions (vm-read-string 
                   (format prompt (if default (format " %s" default) ""))
                   ;; Without the actions that ask, which are offered as an
                   ;; answer to their own question otherwise (#809).
                   (append '(("none"))
                           (seq-remove (lambda (action)
                                         (vm-pcrisis-prompting-action-p (car action)))
                                       vm-pcrisis-actions))
                   t))
    (if (string= actions "none")
        (setq actions nil)
      (if (string= actions "")
          (setq actions default)
        (setq actions (vm-pcrisis-split actions " "))
        (setq actions (reverse actions))))
    (when (vm-interactive-p)
      (setq vm-pcrisis-actions-to-run actions)
      (message "VMPC actions to run: %S" actions))
    actions))
(put 'vm-pcrisis-read-actions 'vm-called-by-vm t)

(defcustom vm-pcrisis-prompt-for-profile-headers
  '((composition ("To" "CC" "BCC"))
    (default     ("From" "Sender" "Reply-To" "From" "Resent-From")))
  "*List of headers to check for email addresses.

`vm-pcrisis-prompt-for-profile' will scan the given headers in the given order."
  :type '(repeat (list (choice (const default)
                               (const composition)
                               (const reply)
                               (const forward)
                               (const resent)
                               (const mail)
                               (const newmail))
                       (repeat (string :tag "Header"))))
  :group 'vm-pcrisis)

(defvar vm-pcrisis-profiles-history nil
  "History of profiles prompted for.")

(defun vm-pcrisis-read-profile (&optional require-match initial-contents default)
  "Read a profile and return it."
  (unless default
    (setq default (car vm-pcrisis-profiles-history)))
  (completing-read 
   ;; prompt
   (format "VMPC profile%s: "
	   (if vm-pcrisis-profiles-history (concat " (" default ")") ""))
   ;; collection
   vm-pcrisis-auto-profiles
   ;; predicate, require-match, initial-input, hist
   nil require-match initial-contents 'vm-pcrisis-profiles-history
   ;; default
   default))

;;;###autoload
(defun vm-pcrisis-prompt-for-profile (&optional remember prompt)
  "Find a profile or prompt for it and add its actions to the list of actions.

A profile is an association between a recipient address and a set of the
actions named in `vm-pcrisis-actions'.  When entering the list of actions,
one has
to press ENTER after each action and finish adding action by pressing ENTER
without an action.

The association is stored in `vm-pcrisis-auto-profiles-file' and in the
future the
stored actions will automatically run for messages to that address.

REMEMBER can be set to t or `prompt'.  When set to `prompt' you will be asked
if you want to store the association.  When set to t a new profile will be
stored without asking.

Storing one writes `vm-pcrisis-auto-profiles-file', which is
`~/.vmpc-auto-profiles' unless you have said otherwise, and that file is then
consulted by every later composition.  The question names it, and so does the
message after any profile is written or removed, since with REMEMBER t there
is no question to name it.  To be rid of a profile, answer with no actions,
or edit the file.

Set PROMPT to t and you will be prompted each time, i.e. not only for unknown
profiles.  If you want to change the profile only explicitly, then omit the
PROMPT argument and call this function interactively in the composition buffer."
  (interactive (progn (setq vm-pcrisis-current-state 'automorph)
                      (list 'prompt t)))
    
  (if (or (eq vm-pcrisis-current-state 'automorph)
	  (eq vm-pcrisis-current-buffer 'none))
      (let ((headers 
	     (or (assoc vm-pcrisis-current-buffer vm-pcrisis-prompt-for-profile-headers)
		 (assoc vm-pcrisis-current-state vm-pcrisis-prompt-for-profile-headers)
		 (assoc 'default vm-pcrisis-prompt-for-profile-headers)))
            field addrs a old-actions actions dest)
        (setq headers (cadr headers))
        ;; search also other headers for known addresses 
        (while (and headers (not actions))
          (when (setq field (vm-pcrisis-get-header-contents (car headers)))
	    (setq addrs (vm-pcrisis-split field  ","))
	    (while addrs
	      (setq a (vm-pcrisis-string-extract-address (car addrs)))
	      (if (vm-ignored-reply-to a)
		  (setq a nil))
	      (setq actions (append (vm-pcrisis-get-profile-for-address a) actions))
	      (if (not dest) (setq dest a))
	      (setq addrs (cdr addrs))))
	  (setq headers (cdr headers)))

        (setq dest 
	      (or dest vm-pcrisis-default-profile (if prompt (vm-pcrisis-read-profile))))
        
        (unless actions 
          (setq actions (vm-pcrisis-get-profile-for-address dest)))

        ;; A profile stored before this was refused says "ask" and so is never
        ;; asked: dropped here, which asks again and remembers the answer, so
        ;; a file written by an older VM heals itself (#809).
        (when (vm-pcrisis-only-asks-p actions)
          (let ((vm-current-warning nil))
            (vm-warn 0 2 (concat "The profile stored for \"%s\" is %s, the action "
                                 "that asks, so it can never ask or set anything."
                                 "  Asking again; the answer replaces it.  Remove "
                                 "the entry from %s to be rid of it by hand")
                     dest actions vm-pcrisis-auto-profiles-file))
          (setq actions nil))

        ;; save action to detect a change
        (setq old-actions actions)
        
        (when dest
          ;; figure out which actions to run
          (when (or prompt (not actions))
            (setq actions (vm-pcrisis-read-actions
                           (format "Actions for \"%s\"%%s: " dest)
                           actions)))

          ;; fixed old style format where there was only a single action
          (unless (listp actions)
            (setq remember t)
            (setq actions (list actions)))

          ;; save the association of this profile with these actions
	  ;; if applicable.  `vm-pcrisis-worth-remembering-p' keeps out the
	  ;; profile that never asks and never acts (#809); the actions
	  ;; themselves are left alone, so this composition still runs them.
          (if (and (not (equal old-actions actions))
                   (vm-pcrisis-worth-remembering-p actions dest)
                   (or (eq remember t)
                       (and (eq remember 'prompt)
                            (if actions 
                                (y-or-n-p 
				 (format "Always run %s for \"%s\"?  (kept in %s) "
					 actions dest
					 (vm-pcrisis-where-profiles-are-kept)))
                              (if (vm-pcrisis-get-profile-for-address dest)
                                  (yes-or-no-p 
				   (format "Delete the profile for \"%s\" from %s? "
					   dest
					   (vm-pcrisis-where-profiles-are-kept))))))))
              (vm-pcrisis-save-profile-for-address dest actions))
          
          ;; TODO: understand when vm-pcrisis-prompt-for-profile has to run actions 
          ;; if we are in automorph (actually being called from within
          ;; an action) 
          (if (eq vm-pcrisis-current-state 'automorph)
              (let ((vm-pcrisis-actions-to-run actions))
                (vm-pcrisis-run-actions))
            ;; otherwise add the actions to the end of the list as a
	    ;; side effect  
            (setq vm-pcrisis-actions-to-run (append vm-pcrisis-actions-to-run actions)))
	
          ;; return the actions, which makes the condition true if a
	  ;; profile exists  
          actions))))

;; -------------------------------------------------------------------
;; Functions for vm-pcrisis-conditions:
;; -------------------------------------------------------------------

(defun vm-pcrisis-none-true-yet (&rest exceptions)
  "True if none of the previous evaluated conditions was true.
This is a condition that can appear in `vm-pcrisis-conditions'.  If
EXCEPTIONS are
specified, it means none were true except those.  For example, if you wanted
to check whether no conditions had yet matched with the exception of the two
conditions named \"default\" and \"blah\", you would make the call like this:
  (vm-pcrisis-none-true-yet \"default\" \"blah\")
Then it will return true regardless of whether \"default\" and \"blah\" had
matched."
  (let ((lenex (length exceptions)) (lentc (length vm-pcrisis-true-conditions)))
    (cond
     ((> lentc lenex)
      'nil)
     ((<= lentc lenex)
      (let ((i 0) (j 0) (k 0))
	(while (< i lenex)
	  (setq k 0)
	  (while (< k lentc)
	    (if (equal (nth i exceptions) (nth k vm-pcrisis-true-conditions))
		(setq j (1+ j)))
	    (setq k (1+ k)))
	  (setq i (1+ i)))
	(if (equal j lentc)
	    't
	  'nil))))))

(defun vm-pcrisis-other-cond (condition)
  "Return true if the specified CONDITION in `vm-pcrisis-conditions' matched.
CONDITION can only be the name of a condition specified earlier in
`vm-pcrisis-conditions' -- that is to say, any conditions which follow the one
containing `vm-pcrisis-other-cond' will show up as not having matched,
because they
haven't yet been checked when this one is checked."
  (member condition vm-pcrisis-true-conditions))

(defun vm-pcrisis-folder-match (regexp)
  "Return true if the the folder name of the current message matches
REGEXP.  If the message is in a virtual folder, the folder of the
underlying real message is used."
  (when (vm-buffer-p)
    (let* ((m (vm-current-message))
	   (real-m (vm-real-message-of m)))
      (with-current-buffer (vm-buffer-of real-m)
	(string-match regexp (buffer-name))))))

(defun vm-pcrisis-folder-account-match (account-regexp)
  "Return true if the POP/IMAP account name of the currennt
message matches REGEXP.  If the message is in a virtual folder,
the folder of the underlying real message is used."
  (when (vm-buffer-p)
    (let* ((m (vm-current-message))
	   (real-m (vm-real-message-of m)))
      (with-current-buffer (vm-buffer-of real-m)
	(let ((account
	       (cond 
		((eq vm-folder-access-method 'imap)
		 (vm-imap-account-name-for-spec (vm-folder-imap-maildrop-spec)))
		((eq vm-folder-access-method 'pop)
		 (vm-pop-find-name-for-spec (vm-folder-pop-maildrop-spec)))
		(t "")
		)))
	  (and account
	      (string-match account-regexp account)))))))

(defun vm-pcrisis-header-match (hdrfield regexp &optional clump-sep num)
  "Return true if the contents of specified header HDRFIELD match REGEXP.
For automorph, this means the header in your message, when replying it means
the header in the message being replied to.

CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP.

If NUM is specified return the match string NUM."
  (cond ((memq vm-pcrisis-current-state '(reply forward resend mail))
         (let ((hdr (vm-pcrisis-get-replied-header-contents hdrfield clump-sep)))
           (and hdr (string-match regexp hdr)
                (if num (match-string num hdr) t))))
        ((eq vm-pcrisis-current-state 'automorph)
         (let ((hdr (vm-pcrisis-get-current-header-contents hdrfield clump-sep)))
           (and (string-match regexp hdr)
                (if num (match-string num hdr) t))))))

(defun vm-pcrisis-only-from-match (hdrfield regexp &optional clump-sep)
  "Return non-nil if all emails from the given HDRFIELD are matched by
REGEXP." 
  (let* ((content (vm-pcrisis-get-header-contents hdrfield clump-sep))
         (case-fold-search t)
         (pos 0)
         (len (length content))
         (only-from (not (null content))))
    (while (and only-from (< pos len)
                (setq pos (string-match "[a-z0-9._-]+@[a-z0-9._-]+" 
					content pos)))
      (if (not (string-match regexp (match-string 0 content)))
          (setq only-from nil))
      (setq pos (1+ pos)))
    only-from))

(defun vm-pcrisis-body-match (regexp)
  "Return non-nil if the contents of the message body match REGEXP.
For automorph, this means the body of your message; when replying it
means the body of the message being replied to."
  (cond ((and (memq vm-pcrisis-current-state '(reply forward resend mail))
	      (eq vm-pcrisis-current-buffer 'none))
	 (string-match regexp (vm-pcrisis-get-replied-body-text)))
	((eq vm-pcrisis-current-state 'automorph)
	 (string-match regexp (vm-pcrisis-get-current-body-text)))))


(defun vm-pcrisis-xor (&rest args)
  "Return true if one and only one argument in ARGS is true."
  (= 1 (length (delete nil args))))

;; -------------------------------------------------------------------
;; Support functions for the advices:
;; -------------------------------------------------------------------

;;;###autoload
(defun vm-pcrisis-true-conditions ()
  "Return a list of all true conditions.
Run this function in order to test/check your conditions."
  (interactive)
  (let (vm-pcrisis-true-conditions
        vm-pcrisis-current-state
        vm-pcrisis-current-buffer)
    (if (eq major-mode 'vm-mail-mode)
        (setq vm-pcrisis-current-state 'automorph
              vm-pcrisis-current-buffer 'composition)
      (setq vm-pcrisis-current-state 
	    (intern (completing-read
		     ;; prompt
		     "VMPC state (default is 'reply): "
		     ;; collection
		     '(("reply") ("forward") ("resend") 
		       ("mail") ("newmail") ("automorph"))
		     ;; predicate, require-match, initial-input, hist
		     nil t nil nil 
		     ;; default
		     "reply"))
            vm-pcrisis-current-buffer 'none))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (vm-pcrisis-build-true-conditions-list)
    (message "VMPC true conditions: %S" vm-pcrisis-true-conditions)
    vm-pcrisis-true-conditions))

;;;###autoload
(defun vm-pcrisis-build-true-conditions-list ()
  "Build list of true conditions and store it in the variable 
`vm-pcrisis-true-conditions'."
  (interactive)
  (setq vm-pcrisis-true-conditions nil)
  (mapc
   (lambda (c)
     (if (save-excursion (eval (cons 'progn (cdr c))))
	 (setq vm-pcrisis-true-conditions (cons (car c) vm-pcrisis-true-conditions))))
   vm-pcrisis-conditions)
  (setq vm-pcrisis-true-conditions (reverse vm-pcrisis-true-conditions)))

;;;###autoload
(defun vm-pcrisis-build-actions-to-run-list ()
  "Build a list of the actions to run.
These are the true conditions mapped to actions.  Duplicates will be
eliminated.  You may run it in a composition buffer in order to see what
actions will be run."
  (interactive)
  (if (and (vm-interactive-p) 
	   (not (member major-mode '(vm-mail-mode mail-mode))))
      (error "Run `vm-pcrisis-build-actions-to-run-list' in a composition buffer!"))
  (let ((alist (or (symbol-value (intern (format "vm-pcrisis-%s-rules"
                                                 vm-pcrisis-current-state)))
                   vm-pcrisis-default-rules))
        (old-vm-pcrisis-actions-to-run vm-pcrisis-actions-to-run)
        actions)
    (setq vm-pcrisis-actions-to-run nil)
    (mapc
     (lambda (c)
       (setq actions (cdr (assoc c alist)))
       ;; TODO: warn about unbound conditions?
       (while actions
	 (if (not (member (car actions) vm-pcrisis-actions-to-run))
	     (setq vm-pcrisis-actions-to-run 
		   (cons (car actions) vm-pcrisis-actions-to-run)))
	 (setq actions (cdr actions))))
     vm-pcrisis-true-conditions)
    (setq vm-pcrisis-actions-to-run (reverse vm-pcrisis-actions-to-run))
    (setq vm-pcrisis-actions-to-run 
	  (append vm-pcrisis-actions-to-run old-vm-pcrisis-actions-to-run)))
  (if (vm-interactive-p)
      (message "VMPC actions to run: %S" vm-pcrisis-actions-to-run))
  vm-pcrisis-actions-to-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-pcrisis-run-action (&optional action-regexp)
  "Run all actions with names matching the ACTION-REGEXP.
If called interactively it prompts for the regexp.  You may also use
completion."
  (interactive)
  (let ((action-names (mapcar (lambda (a)
				(list (regexp-quote (car a)) 1))
                              vm-pcrisis-actions)))
    (if (not action-regexp)
        (setq action-regexp (completing-read
			     ;; prompt
			     "VMPC action-regexp: "
			     ;; collection
			     action-names)))
    (mapcar (lambda (action)
	      (if (string-match action-regexp (car action))
		  (mapcar (lambda (action-command)
			    (eval action-command))
			  (cdr action))))
            vm-pcrisis-actions)))


;;;###autoload
(defun vm-pcrisis-run-actions (&optional actions verbose)
  "Run the argument actions, or the actions stored in `vm-pcrisis-actions-to-run'.
If verbose is supplied, it should be a STRING, indicating the name of a
buffer to which to write diagnostic output."
  (interactive)
  
  (if (and (not vm-pcrisis-actions-to-run) (not actions) (vm-interactive-p))
      (setq vm-pcrisis-actions-to-run (vm-pcrisis-read-actions "Actions: ")))

  (let ((actions (or actions vm-pcrisis-actions-to-run))
	(vm-pcrisis-running-actions t)
	form)
    (while actions
      (setq form (or (assoc (car actions) vm-pcrisis-actions)
                     (error "Action %S does not exist!" (car actions)))
            actions (cdr actions))
      (let ((form (cons 'progn (cdr form)))
	    (results (eval (cons 'progn (cdr form)))))
	(when verbose
	  (with-current-buffer verbose
	    (insert (format "Action form is:\n%S\nResults are:\n%S\n"
			    form results))))))))

;; ------------------------------------------------------------------------
;; The main functions and advices -- these are the entry points to pcrisis:
;; ------------------------------------------------------------------------
(defun vm-pcrisis-init-vars (&optional state buffer)
  "Initialize pcrisis variables and optionally set STATE and BUFFER."
  (setq vm-pcrisis-saved-headers-alist nil
        vm-pcrisis-actions-to-run nil
        vm-pcrisis-true-conditions nil
        vm-pcrisis-current-state state
        vm-pcrisis-current-buffer (or buffer 'none)))

(defun vm-pcrisis-make-vars-local ()
  "Make the pcrisis vars buffer local.

When the vars are first set they cannot be made buffer local as we are not in
the composition buffer then.

Unfortunately making them buffer local while they are bound by a `let' does
not work, see the info for `make-local-variable'.  So we are using the global
ones and make them buffer local when in the composition buffer.  At least for
`saved-headers-alist' this should fix the bug that another composition
overwrites the stored headers for subsequent morphs.

The current solution is not reentrant save, but there also should be no
recursion nor concurrent calls."
  ;; make the variables buffer local
  (let ((tc vm-pcrisis-true-conditions)
        (sha vm-pcrisis-saved-headers-alist)
        (atr vm-pcrisis-actions-to-run)
        (cs vm-pcrisis-current-state))
    (make-local-variable 'vm-pcrisis-true-conditions)
    (make-local-variable 'vm-pcrisis-saved-headers-alist)
    (make-local-variable 'vm-pcrisis-actions-to-run)
    (make-local-variable 'vm-pcrisis-current-state)
    (make-local-variable 'vm-pcrisis-current-buffer)
    ;; now set them again to make sure the contain the right value
    (setq vm-pcrisis-true-conditions tc)
    (setq vm-pcrisis-saved-headers-alist sha)
    (setq vm-pcrisis-actions-to-run atr)
    (setq vm-pcrisis-current-state cs))
    ;; mark, that we are in the composition buffer now
    (setq vm-pcrisis-current-buffer      'composition)
  ;; BUGME why is the global value resurrected after making the variable
  ;; buffer local?  Is this related to defadvice?  I have no idea what is
  ;; going on here!  Thus we clear it afterwards now!
  (with-current-buffer (get-buffer-create " *vm-pcrisis-cleanup*")
    (vm-pcrisis-init-vars)
    (setq vm-pcrisis-current-buffer nil)))

(defun vm-pcrisis--reply (orig-fun &rest args)
  "Reply to a message with pcrisis voodoo."
  (vm-pcrisis-init-vars 'reply)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--mail (orig-fun &rest args)
  "Start a new message with pcrisis voodoo."
  (vm-follow-summary-cursor)
  ;; No message needed, for the reason given at `vm-mail-from-folder': this
  ;; composes a new message and wants the current one only as a parent, if
  ;; there is one.  This copy of the validation is why the fix for issue #514
  ;; did not reach anyone using Personality Crisis -- the advice ran first and
  ;; still demanded a message, so `m' in an empty folder went on answering
  ;; "Folder is empty".
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-pcrisis-init-vars 'mail)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--newmail (orig-fun &rest args)
  "Start a new message with pcrisis voodoo."
  (vm-pcrisis-init-vars 'newmail)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--compose-newmail (orig-fun &rest args)
  "Start a new message with pcrisis voodoo."
  (vm-pcrisis-init-vars 'newmail)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--forward (orig-fun &rest args)
  "Forward a message with pcrisis voodoo."
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  ;;  the rest is almost exactly the same as replying:
  (vm-pcrisis-init-vars 'forward)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--forward-plain (orig-fun &rest args)
  "Forward a message in plain text with pcrisis voodoo."
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  ;;  the rest is almost exactly the same as replying:
  (vm-pcrisis-init-vars 'forward)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defun vm-pcrisis--resend (orig-fun &rest args)
  "Resent a message with pcrisis voodoo."
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  ;; the rest is almost exactly the same as replying:
  (vm-pcrisis-init-vars 'resend)
  (vm-pcrisis-build-true-conditions-list)
  (vm-pcrisis-build-actions-to-run-list)
  (vm-pcrisis-run-actions)
  (apply orig-fun args)
  (vm-pcrisis-create-sig-and-pre-sig-exerlays)
  (vm-pcrisis-make-vars-local)
  (vm-pcrisis-run-actions))

(defvar vm-pcrisis-no-automorph nil
  "When true automorphing will be disabled.")

(make-variable-buffer-local 'vm-pcrisis-no-automorph)

;;;###autoload
(defun vm-pcrisis-toggle-no-automorph ()
  "Disable automorph for the current buffer.
When automorph is not doing the right thing and you want to disable it for the
current composition, then call this function."
  (interactive)
  (setq vm-pcrisis-no-automorph (not vm-pcrisis-no-automorph))
  (message (if vm-pcrisis-no-automorph
               "Automorphing has been enabled"
             "Automorphing has been disabled")))

;;;###autoload
(defun vm-pcrisis-automorph ()
  "*Change contents of the current mail message based on its own headers.
Unless `vm-pcrisis-current-state' is `no-automorph', headers and
signatures can be
changed; pre-signatures added; functions called.

Call `vm-pcrisis-no-automorph' to disable it for the current buffer."
  (interactive)
  (unless vm-pcrisis-no-automorph
    (vm-pcrisis-make-vars-local)
    (vm-pcrisis-init-vars 'automorph 'composition)
    (vm-pcrisis-build-true-conditions-list)
    (vm-pcrisis-build-actions-to-run-list)
    (vm-pcrisis-run-actions)))

;;; Switching it on
;;
;; These advices used to be installed as this file loaded, so merely having
;; vm-pcrisis.el on the load path changed how every composition command in VM
;; behaved -- with no way to turn it off, and whether or not any pcrisis rule
;; had been set up.  That is the same complaint #512 made of vm-biff, and this
;; is the same answer: loading the file is inert, and the mode does the work.
;; Issue #561.

(defconst vm-pcrisis-advised-commands
  '((vm-do-reply             . vm-pcrisis--reply)
    (vm-mail-from-folder     . vm-pcrisis--mail)
    (vm-mail                 . vm-pcrisis--newmail)
    (vm-compose-mail         . vm-pcrisis--compose-newmail)
    (vm-forward-message      . vm-pcrisis--forward)
    (vm-forward-message-plain . vm-pcrisis--forward-plain)
    (vm-resend-message       . vm-pcrisis--resend))
  "The VM commands Personality Crisis advises, and the advice for each.")

;;;###autoload
(define-minor-mode vm-pcrisis-mode
  "Personality Crisis: vary the headers and body of a message you send.
Which headers, and how, is decided by `vm-pcrisis-conditions' and
`vm-pcrisis-actions';
see the commentary at the top of vm-pcrisis.el.

Turning this on advises VM's composition commands -- replying, forwarding,
resending and starting a new message -- so that the rules are consulted as each
composition begins.  Turning it off removes the advice, leaving those commands
as VM defines them."
  :global t
  :group 'vm-pcrisis
  (dolist (pair vm-pcrisis-advised-commands)
    (if vm-pcrisis-mode
        (advice-add (car pair) :around (cdr pair))
      (advice-remove (car pair) (cdr pair)))))

(defconst vm-pcrisis-rule-variables
  '(vm-pcrisis-default-rules vm-pcrisis-reply-rules vm-pcrisis-forward-rules
    vm-pcrisis-resend-rules vm-pcrisis-mail-rules vm-pcrisis-newmail-rules
    vm-pcrisis-automorph-rules)
  "Every variable that holds condition-action rules.")

(defun vm-pcrisis-names-in (entries)
  "The names ENTRIES defines, an entry being (NAME FORM...)."
  (delq nil (mapcar (lambda (entry) (and (consp entry) (stringp (car entry))
                                         (car entry)))
                    entries)))

(defun vm-pcrisis-repeated (names)
  "The members of NAMES that appear more than once."
  (let (seen repeated)
    (dolist (name names)
      (if (member name seen)
          (unless (member name repeated) (push name repeated))
        (push name seen)))
    (nreverse repeated)))

(defun vm-pcrisis-offer (names)
  "NAMES as a sentence fragment listing what is defined."
  (if names
      (concat "one of " (mapconcat (lambda (n) (format "%S" n)) names ", "))
    "nothing: the variable is empty"))

(defun vm-pcrisis-check-entries (variable what)
  "Report entries of VARIABLE that are not (NAME FORM...), WHAT naming them."
  (let ((n 0) problems)
    (dolist (entry (symbol-value variable))
      (setq n (1+ n))
      (unless (and (consp entry) (stringp (car entry)))
        (push (format (concat "Entry %d of %s is not a %s: %S.  Each entry is a "
                              "list whose first element is a string naming it.")
                      n variable what entry)
              problems)))
    (nreverse problems)))

(defun vm-pcrisis-check-repeats (variable what)
  "Report names VARIABLE defines twice, WHAT naming them."
  (mapcar (lambda (name)
            (format (concat "%s defines the %s %S more than once.  Only the "
                            "first is ever used; remove the later one or "
                            "rename it.")
                    variable what name))
          (vm-pcrisis-repeated (vm-pcrisis-names-in (symbol-value variable)))))

(defun vm-pcrisis-check-rule-condition (rule variable conditions)
  "Report a RULE in VARIABLE whose condition is not among CONDITIONS."
  (let ((name (car rule)))
    (unless (member name conditions)
      (list (format (concat "Rule %S in %s is keyed on the condition %S, which "
                            "vm-pcrisis-conditions does not define, so the rule "
                            "never runs.  Define that condition, or key the rule "
                            "on %s.  For a rule that runs when nothing else "
                            "matched, define a condition calling "
                            "`vm-pcrisis-none-true-yet' and key it on that.")
                    rule variable name (vm-pcrisis-offer conditions))))))

(defun vm-pcrisis-check-rule-actions (rule variable actions)
  "Report the actions of RULE in VARIABLE that are not among ACTIONS."
  (delq nil
        (mapcar (lambda (name)
                  (unless (and (stringp name) (member name actions))
                    (format (concat "Rule %S in %s names the action %S, which "
                                    "vm-pcrisis-actions does not define, so "
                                    "nothing happens when its condition is "
                                    "true.  Define that action, or name %s.")
                            rule variable name (vm-pcrisis-offer actions))))
                (cdr rule))))

(defun vm-pcrisis-check-rules (variable conditions actions)
  "Report the rules of VARIABLE naming a condition or action that is not defined."
  (let (problems)
    (dolist (rule (symbol-value variable))
      (when (and (consp rule) (stringp (car rule)))
        (setq problems
              (append problems
                      (vm-pcrisis-check-rule-condition rule variable conditions)
                      (vm-pcrisis-check-rule-actions rule variable actions)))))
    problems))

(defun vm-pcrisis-fallback-p (entry)
  "Whether ENTRY is a bare `vm-pcrisis-none-true-yet' condition.
One given exceptions is not bare: it is meant to ignore those and can stand
anywhere."
  (equal (cdr entry) '((vm-pcrisis-none-true-yet))))

(defun vm-pcrisis-check-fallback-is-last ()
  "Report a bare `vm-pcrisis-none-true-yet' condition that is not the last one."
  (let ((entries (seq-filter (lambda (e) (and (consp e) (stringp (car e))))
                             vm-pcrisis-conditions)))
    (delq nil
          (mapcar (lambda (entry)
                    (when (and (vm-pcrisis-fallback-p entry)
                               (not (eq entry (car (last entries)))))
                      (format (concat "The condition %S calls "
                                      "`vm-pcrisis-none-true-yet' but is not "
                                      "the last in vm-pcrisis-conditions, so a "
                                      "condition after it can be true as well "
                                      "and this is not a fallback.  Move it to "
                                      "the end, or name the conditions it "
                                      "should ignore as arguments.")
                              (car entry))))
                  entries))))

(defun vm-pcrisis-configuration-problems ()
  "Everything wrong with the Personality Crisis configuration, as sentences.
Answers nil when there is nothing to report."
  (let ((conditions (vm-pcrisis-names-in vm-pcrisis-conditions))
        (actions (vm-pcrisis-names-in vm-pcrisis-actions))
        (problems (append (vm-pcrisis-check-entries 'vm-pcrisis-conditions "condition")
                          (vm-pcrisis-check-entries 'vm-pcrisis-actions "action")
                          (vm-pcrisis-check-repeats 'vm-pcrisis-conditions "condition")
                          (vm-pcrisis-check-repeats 'vm-pcrisis-actions "action")
                          (vm-pcrisis-check-fallback-is-last))))
    (dolist (variable vm-pcrisis-rule-variables)
      (setq problems (append problems
                             (vm-pcrisis-check-entries variable "rule")
                             (vm-pcrisis-check-rules variable conditions actions))))
    problems))

;;;###autoload
(defun vm-pcrisis-check-configuration ()
  "Say what is wrong with your Personality Crisis rules, and what to do about it.

Checks that every rule is keyed on a condition that exists and names actions
that exist, that no name is defined twice, that each entry has the form the
variables want, and that a `vm-pcrisis-none-true-yet' fallback is last.

The mistake worth the command is a rule keyed on a name no condition has.
Nothing reports it and nothing runs: a fallback rule written as
\\=(\"default\" \"from-work\") with no condition called \"default\" leaves the
composition with `user-mail-address', which is the address most people would
have got anyway, so it can go years unnoticed."
  (interactive)
  (let ((problems (vm-pcrisis-configuration-problems)))
    (if (null problems)
        (message "Personality Crisis: the rules are consistent")
      (with-output-to-temp-buffer "*Personality Crisis*"
        (princ (format "%d problem%s with the Personality Crisis rules:\n\n"
                       (length problems) (if (cdr problems) "s" "")))
        (dolist (problem problems)
          (princ (concat "* " problem "\n\n")))))
    (length problems)))

;;;###autoload
(defun vm-pcrisis-warn-if-misconfigured ()
  "Warn in a composition about rules that cannot do what they say.

On `vm-mail-mode-hook', beside `vm-pcrisis-warn-if-off', and repeating for the
same reason: a rule that never runs looks exactly like one that ran and chose
the address VM would have chosen anyway."
  (let ((problems (vm-pcrisis-configuration-problems)))
    (when problems
      (let ((vm-current-warning nil))
        (vm-warn 0 2 "Personality Crisis: %s%s"
                 (car problems)
                 (if (cdr problems)
                     (format "  (%d more; M-x vm-pcrisis-check-configuration says what)"
                             (length (cdr problems)))
                   ""))))))

(add-hook 'vm-mail-mode-hook #'vm-pcrisis-warn-if-misconfigured)

(defun vm-pcrisis-rules-are-set-p ()
  "Whether Personality Crisis has been configured with anything to do."
  (and (or vm-pcrisis-conditions vm-pcrisis-actions)
       (or vm-pcrisis-default-rules vm-pcrisis-reply-rules vm-pcrisis-forward-rules
           vm-pcrisis-resend-rules vm-pcrisis-newmail-rules vm-pcrisis-automorph-rules)))

;;;###autoload
(defun vm-pcrisis-warn-if-off ()
  "Say so in a composition when the rules are set but `vm-pcrisis-mode' is off.

Setting `vm-pcrisis-conditions' and `vm-pcrisis-actions' with the mode
off can only be a
mistake: the rules are never consulted, and the composition gets whatever
`user-mail-address' says.  It is a quiet mistake, since a default rule naming
the same address VM would have used anyway looks exactly like a working
setup -- which is how an init file that had worked for years went unnoticed
after the mode arrived in 8.3.0.

Said at every composition rather than once, because a warning seen once at
startup is a warning forgotten.  `vm-current-warning' is bound around the
call for the same reason: `vm-warn' will not repeat itself, and here
repeating itself is the point."
  (when (and (not vm-pcrisis-mode) (vm-pcrisis-rules-are-set-p))
    (let ((vm-current-warning nil))
      (vm-warn 0 2 (concat "Personality Crisis rules are set but vm-pcrisis-mode "
			   "is off: add (vm-pcrisis-mode 1) to your init file")))))

(add-hook 'vm-mail-mode-hook #'vm-pcrisis-warn-if-off)

;;; The old vmpc- names
;;
;; Personality Crisis was the one part of VM not named after VM, which kept
;; it out of `M-x vm-' completion, `C-h a vm-' and the vm customize tree.
;; Everything is vm-pcrisis- now; what a reader's init file can hold keeps
;; working under its old name.
;;
;; The options matter most: an alias carries a value saved by customize
;; across.  The condition and action functions matter too -- they are
;; written into `vm-pcrisis-conditions' and `vm-pcrisis-actions' as data,
;; so a configuration names them without calling them.

(provide 'vm-pcrisis)
;;; vm-pcrisis.el ends here
