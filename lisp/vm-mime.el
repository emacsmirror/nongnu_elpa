;;; vm-mime.el ---  MIME support functions  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1997-2003 Kyle E. Jones
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
(require 'vm-reply)                     ;vm-mail-mode-show-headers
(require 'vm-summary)
(require 'sendmail)
(require 'smime)
(eval-when-compile (require 'cl-lib))
;; For the shr handler.  At compile time so that the let-bindings of shr's
;; variables are compiled as dynamic: binding a variable whose defvar the
;; compiler has not seen makes a lexical binding in this file, and shr would
;; never see it.  The handler requires it again at run time.
(eval-when-compile (require 'shr))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-get-sender ())
(declare-function vm-smime-get-recipient-certfiles ())
(declare-function vm-mode "vm" (&optional read-only))
(declare-function vm-imagemagick-available-p "vm-misc" ())
(declare-function vm-imagemagick-call-identify "vm-misc" (infile buffer args))
(declare-function vm-imagemagick-call-convert "vm-misc" (infile buffer args))
(declare-function vm-imagemagick-convert-shell-command "vm-misc" ())

;; vm-digest.el functions - cyclic dependency
(declare-function vm-mime-encapsulate-messages "vm-digest" t)
(declare-function vm-mime-burst-layout "vm-digest" (layout ident-header))

;; vm-edit.el function
(declare-function vm-discard-cached-data "vm-edit" (&optional count))

;; Image cache function
(declare-function clear-image-cache "image.c" (&optional filter))

(defvar enable-multibyte-characters)

;; The following variables are defined in the code, depending on the
;; Emacs version being used.  They should not be initialized here.

(defvar vm-image-list)
(defvar vm-image-type)
(defvar vm-image-type-name)
(defvar vm-overlay-list)


(defun vm-mime-error (&rest args)
  (signal 'vm-mime-error (list (apply 'format args)))
  (error "can't return from vm-mime-error"))

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-image-too-small "Image too small")
      (define-error 'vm-mime-error "MIME error"))
  (put 'vm-image-too-small 'error-conditions '(vm-image-too-small error))
  (put 'vm-image-too-small 'error-message "Image too small")
  (put 'vm-mime-error 'error-conditions '(vm-mime-error error))
  (put 'vm-mime-error 'error-message "MIME error"))

(defsubst vm-mime-handler (op type)
  (intern (concat "vm-mime-" op "-" type)))

(defvar coding-system-list)
;; defined by `easy-menu-define' in vm-menu.el, and referred to before that
;; file is loaded
(defvar vm-menu-fsfemacs-image-menu)

(defun vm-get-coding-system-priorities ()
  "Return the value of `vm-coding-system-priorities', or a reasonable
default for it if it's nil.  "
  (or vm-coding-system-priorities
      ;; FIXME: `utf-8' should be first nowadays!
      (let ((res '(iso-8859-1 iso-8859-2 iso-8859-15 iso-8859-16 utf-8)))
	(dolist (list-item res)
	  ;; Assumes iso-8859-1 is always available, which is reasonable.
	  (unless (vm-coding-system-p list-item)
	    (setq res (remq list-item res))))
	res)))

(defun vm-mime-charset-to-coding (charset)
  "Return the Emacs coding system corresonding to the given mime CHARSET."
  ;; We can depend on the fact that, in FSF Emacsen, coding systems
  ;; have aliases that correspond to MIME charset names.
  (let ((tmp nil))
    (cond ((vm-coding-system-p (setq tmp (intern (downcase charset))))
	   tmp)
	  ((equal charset "us-ascii")
	   'raw-text)
	  ((equal charset "unknown")
	   'iso-8859-1)
	  (t 'undecided))))


(defun vm-get-mime-ucs-list ()
  "The value of `vm-mime-ucs-list', or a reasonable default where it is nil.
A universal character set is one that can encode anything, so a message in
one needs no charset negotiation."
  (or vm-mime-ucs-list
      '(utf-8 iso-2022-jp ctext escape-quoted)))

;;----------------------------------------------------------------------------
;;; MIME layout structs (vm-mm)
;;----------------------------------------------------------------------------

(defconst vm-mime-layout-fields
  '[:type :qtype :encoding :id :description :disposition :qdisposition
	  :header-start :header-end :body-start :body-end
	  :parts :cache :message-symbol :display-error 
	  :layout-is-converted :unconverted-layout])

(defun vm-formatted-mime-layout (layout)
  (let ((copy (copy-sequence layout)))
    (vm-set-mm-layout-parts 
     copy
     (mapcar 'vm-formatted-mime-layout (vm-mm-layout-parts copy)))
    (vm-zip-vectors vm-mime-layout-fields copy)))

(defun vm-make-layout (&rest plist)
  (vector
   (plist-get plist 'type)
   (plist-get plist 'qtype)
   (plist-get plist 'encoding)
   (plist-get plist 'id)
   (plist-get plist 'description)
   (plist-get plist 'disposition)
   (plist-get plist 'qdisposition)
   (plist-get plist 'header-start)
   (plist-get plist 'header-end)
   (plist-get plist 'body-start)
   (plist-get plist 'body-end)
   (plist-get plist 'parts)
   (plist-get plist 'cache)
   (plist-get plist 'message-symbol)
   (plist-get plist 'display-error)
   (plist-get plist 'layout-is-converted)
   (plist-get plist 'unconverted-layout)))

(defun vm-mime-copy-layout (from to)
  "Copy a MIME layout FROM to the layout TO.  The previous contents of
TO are overwritten.                                    USR, 2011-03-27"
  (let ((i (1- (length from))))
    (while (>= i 0)
      (aset to i (aref from i))
      (setq i (1- i)))))

(defun vm-mime-layouts-equal (layout1 layout2)
  (catch 'return
    (if (equal layout1 layout2)
	(throw 'return t))
    (vm-mapc 
     (lambda (i)
       (unless (equal (aref layout1 i) (aref layout2 i))
	 (throw 'return nil)))
     '(0 1 2 3 4 5 6))			; type through q-disposition
    (vm-mapc
     (lambda (i)
       (unless (equal (marker-position (aref layout1 i))
		      (marker-position (aref layout2 i)))
	 (throw 'return nil)))
     '(7 9 10))				; header-start, body-start, body-end
    (vm-mapc 
     (lambda (part1 part2)
       (unless (vm-mime-layouts-equal part1 part2)
	 (throw 'return nil)))
     (vm-mm-layout-parts layout1)
     (vm-mm-layout-parts layout2))
    t))

(defun vm-mime-verify-cached-layout (cached current &optional external-body)
  "Returns a boolean value indicating whether a CACHED MIME layout is
valid with respect to a CURRENT layout.  

The optional argument EXTERNAL-BODY says whether this layout is the
body of a message/external-body part, in which case mismatches in the
body markers is tolerated."
  (let ((type (if (vectorp cached)
		  (car (vm-mm-layout-type cached))))
	mismatch			; position of mismatch in the
					; layout vector, or nil
	)
    (setq mismatch
	 (catch 'mismatch
	   ;; If the two layouts are identical, ok.
	   (when (equal cached current)
	     (throw 'mismatch nil))
	   ;; Otherwise, check if both are vectors first
	   (unless (and (vectorp cached) (vectorp current))
	     (throw 'mismatch nil))
	   ;; Check if the basic fields are equal
	   (vm-mapc 
	    (lambda (i)
	      (unless (equal (aref cached i) (aref current i))
		;; skip if the cached type has been inferred
		(unless (and (<= i 1)	; type or qtype
			     (equal (aref current i)
				    '("application/octet-stream"))
			     vm-infer-mime-types)
		  (throw 'mismatch i))))
	    '(0 1 2 3 4))		; type through description
	   ;; ignore disposition and qdisposition because of the hack
	   ;; in vm-mime-frob-image-xxxx
	   ;; Check if the markers are equal.  Buffer as well as position:
	   ;; a layout parsed in one buffer and cached against a message in
	   ;; another is invalid however well the offsets happen to line up,
	   ;; and they do line up for the first message of a folder, which
	   ;; starts at 1 just as a Presentation buffer does (issue #109).
	   (unless external-body
	     (vm-mapc
	      (lambda (i)
		(unless (and (equal (marker-position (aref cached i))
				    (marker-position (aref current i)))
			     (eq (marker-buffer (aref cached i))
				 (marker-buffer (aref current i))))
		  (throw 'mismatch i)))
	      '(7 9 10)))	  ; header-start, body-start, body-end
	   ;; Check if the subparts are equal
	   (vm-mapc 
	    (lambda (part1 part2)
	      (unless (vm-mime-verify-cached-layout 
		       part1 part2 
		       (vm-mime-types-match "message/external-body" type))
		(throw 'mismatch 11)))
	    (vm-mm-layout-parts cached)
	    (vm-mm-layout-parts current))
	   nil))
    (if (not mismatch)
	t
      ;; If we found a mismatch...
      (if (and (equal (aref vm-mime-layout-fields mismatch) ':type)
	       (equal (car (aref current mismatch))
		      "application/octet-stream"))
	  ;; Mismatch for application/octet-stream.  Ignore it.
	  t
	(when vm-debug
	  (debug 'vm-mime-verify-cached-layout
		 (aref vm-mime-layout-fields mismatch)
		 (aref cached mismatch) (aref current mismatch)))
	nil))
    ))

(defun vm-mm-layout-type (e) (aref e 0))
(defun vm-mm-layout-qtype (e) (aref e 1))
(defun vm-mm-layout-encoding (e) (aref e 2))
(defun vm-mm-layout-id (e) (aref e 3))
(defun vm-mm-layout-description (e) (aref e 4))
(defun vm-mm-layout-disposition (e) (aref e 5))
(defun vm-mm-layout-qdisposition (e) (aref e 6))
(defun vm-mm-layout-header-start (e) (aref e 7))
(defun vm-mm-layout-header-end (e) (aref e 8))
(defun vm-mm-layout-body-start (e) (aref e 9))
(defun vm-mm-layout-body-end (e) (aref e 10))
(defun vm-mm-layout-parts (e) (aref e 11))
(defun vm-mm-layout-cache (e) (aref e 12))
(defun vm-mm-layout-message-symbol (e) (aref e 13))
(defun vm-mm-layout-message (e)
  (symbol-value (vm-mm-layout-message-symbol e)))
;; if display of MIME part fails, error string will be here.
(defun vm-mm-layout-display-error (e) (aref e 14))
(defun vm-mm-layout-is-converted (e) (aref e 15))
(defun vm-mm-layout-unconverted-layout (e) (aref e 16))

(defun vm-set-mm-layout-type (e type) (aset e 0 type))
(defun vm-set-mm-layout-qtype (e type) (aset e 1 type))
(defun vm-set-mm-layout-encoding (e encoding) (aset e 2 encoding))
(defun vm-set-mm-layout-id (e id) (aset e 3 id))
(defun vm-set-mm-layout-description (e des) (aset e 4 des))
(defun vm-set-mm-layout-disposition (e d) (aset e 5 d))
(defun vm-set-mm-layout-qdisposition (e d) (aset e 6 d))
(defun vm-set-mm-layout-header-start (e start) (aset e 7 start))
(defun vm-set-mm-layout-header-end (e start) (aset e 8 start))
(defun vm-set-mm-layout-body-start (e start) (aset e 9 start))
(defun vm-set-mm-layout-body-end (e end) (aset e 10 end))
(defun vm-set-mm-layout-parts (e parts) (aset e 11 parts))
(defun vm-set-mm-layout-cache (e c) (aset e 12 c))
(defun vm-set-mm-layout-message-symbol (e s) (aset e 13 s))
(defun vm-set-mm-layout-display-error (e c) (aset e 14 c))
(defun vm-set-mm-layout-is-converted (e c) (aset e 15 c))
(defun vm-set-mm-layout-unconverted-layout (e l) (aset e 16 l))

;; Properties of layout-cache
(defun vm-mm-layout-image-file (e)
  (get (vm-mm-layout-cache e) 'vm-mime-display-internal-image-xxxx))
(defun vm-mm-layout-image-modified (e)
  (get (vm-mm-layout-cache e) 'vm-image-modified))
(defun vm-set-mm-layout-image-file (e file)
  (put (vm-mm-layout-cache e) 'vm-mime-display-internal-image-xxxx file))
(defun vm-set-mm-layout-image-modified (e flag)
  (put (vm-mm-layout-cache e) 'vm-image-modified flag))

(defun vm-mime-type-with-params (type params)
  "Returns a string concatenating MIME TYPE (a string) and PARAMS (a
list of strings)."
  (if params
      (if vm-mime-avoid-folding-content-type
	  (concat type ";\n\t " (mapconcat 'identity params ";\n\t"))
	(concat type "; " (mapconcat 'identity params "; ")))
    type))

(defun vm-mime-make-message-symbol (m)
  (let ((s (make-symbol "<<m>>")))
    (set s m)
    s ))

(defun vm-mime-make-cache-symbol ()
  (let ((s (make-symbol "<<c>>")))
    (set s s)
    s ))

(defun vm-mm-layout (m)
  "Returns the mime layout of message M, either from the cache or by
freshly parsing the message contents."
  (or (vm-mime-layout-of m)
      (progn (vm-set-mime-layout-of m (vm-mime-parse-entity-safe m))
	     (vm-mime-layout-of m))))

(defun vm-mm-encoded-header (m)
  "Return non-nil if M's headers need decoding before they can be displayed.
The symbol `none' means they do not.  Encoded words need it, and so does raw
8-bit text, which is legal under RFC 6532 and sent regardless; see
`vm-decode-8bit-text'."
  (or (vm-mime-encoded-header-flag-of m)
      (progn (setq m (vm-real-message-of m))
	     (vm-set-mime-encoded-header-flag-of
	      m
	      (with-current-buffer (vm-buffer-of m)
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (vm-headers-of m))
		    (let ((case-fold-search t))
		      (or (re-search-forward vm-mime-encoded-word-regexp
					     (vm-text-of m) t)
			  (and vm-mime-8bit-header-charsets
			       (progn
				 (goto-char (vm-headers-of m))
				 (re-search-forward "[\200-\377]"
						    (vm-text-of m) t)))
			  'none))))))
	     (vm-mime-encoded-header-flag-of m))))

;;----------------------------------------------------------------------------
;;; MIME encoding/decoding
;;----------------------------------------------------------------------------

(defun vm-mime-Q-decode-region (start end)
  (interactive "r")
  (let ((buffer-read-only nil))
    (subst-char-in-region start end ?_ (string-to-char " ") t)
    (quoted-printable-decode-region start end)))
(put 'vm-mime-Q-decode-region 'vm-called-by-vm t)

(fset 'vm-mime-B-decode-region 'vm-mime-base64-decode-region)

(defun vm-mime-Q-encode-region (start end)
  (let ((buffer-read-only nil)
	(val))
    (setq val (vm-mime-qp-encode-region start end t)) ; may modify buffer
    (subst-char-in-region start (min end (point-max))
                          (string-to-char " ") ?_ t)
    val ))

(defun vm-mime-B-encode-region (start end)
  (vm-mime-base64-encode-region start end nil t))

(defun vm-mime-base64-decode-string (string)
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-decode-region (point-min) (point-max))))))

(defun vm-mime-base64-encode-string (string)
  "Return the base64 encoding of multibyte STRING."
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-encode-region (point-min) (point-max)
					     nil t)))))

(defun vm-mime-crlf-to-lf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\r\n" nil t)
	  (delete-char -2)
	  (insert "\n"))))))
      
(defun vm-mime-lf-to-crlf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\n" nil t)
	  (delete-char -1)
	  (insert "\r\n"))))))
      
(defun vm-encode-coding-region (b-start b-end coding-system &rest foo)
  "This is a wrapper function to `encode-coding-region' having the
same effect."
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq oldsize (buffer-size))
	  (setq retval (apply 'encode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (setq start (point-min) end (point-max))
	  (setq retval (buffer-size))
	  (with-current-buffer b
	    (goto-char b-start)
	    (insert-buffer-substring work-buffer start end)
	    (delete-region (point) (+ (point) oldsize))
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after encode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-decode-coding-region (b-start b-end coding-system &rest foo)
  "This is a wrapper function for `decode-coding-region', having the
same effect."
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq retval (apply 'decode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (set-buffer-multibyte t)	; is this safe?
	  (setq start (point-min) end (point-max))
	  (with-current-buffer b
	    (goto-char b-start)
	    (delete-region (point) (+ (point) oldsize))
	    (insert-buffer-substring work-buffer start end)
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after decode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-charset-decode-region (charset start end)
  (or (markerp end) (setq end (vm-marker end)))
  (if t
      (let ((buffer-read-only nil)
	    (coding (vm-mime-charset-to-coding charset))
	    (opoint (point)))
	;; decode 8-bit indeterminate char to correct
	;; char in correct charset.
	(vm-decode-coding-region start end coding)
	(put-text-property start end 'vm-string t)
	(put-text-property start end 'vm-charset charset)
	(put-text-property start end 'vm-coding coding)
	;; In XEmacs 20.0 beta93 decode-coding-region moves point.
	(goto-char opoint))))

(defun vm-mime-transfer-decode-region (layout start end)
  "Decode the body of a mime part given by LAYOUT at positions START
to END, and replace it by the decoded content.  The decoding carried
out includes base-64, quoted-printable, uuencode and CRLF conversion."
  (let ((case-fold-search t) (crlf nil))
    (if (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
	    (vm-mime-types-match "message" (car (vm-mm-layout-type layout))))
	(setq crlf t))
    (cond ((string-match "^base64$" (vm-mm-layout-encoding layout))
	   (vm-mime-base64-decode-region start end crlf))
	  ((string-match "^quoted-printable$"
			 (vm-mm-layout-encoding layout))
	   (quoted-printable-decode-region start end))
	  ((string-match "^x-uue$\\|^x-uuencode$"
			 (vm-mm-layout-encoding layout))
	   (vm-mime-uuencode-decode-region start end crlf)))))

(defvar binary-process-output) ;; FIXME: Unknown var.  XEmacs?

(defun vm-mime-base64-decode-region (start end &optional crlf)
  (or (markerp end) (setq end (vm-marker end)))
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding base64..."))
  (save-excursion
    (condition-case data
	(base64-decode-region start end)
      (error (vm-mime-error "%S" data)))
    (and crlf (vm-mime-crlf-to-lf-region start end)))
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding base64... done")))

(defun vm-mime-base64-encode-region (start end &optional crlf B-encoding)
  ;; A marker that advances, as `vm-mime-qp-encode-region' has.  Turning the
  ;; last LF into CRLF below inserts at END, and a marker that does not
  ;; advance is left in front of the CR: the body's last line break then fell
  ;; outside the region and was not encoded, so a base64 part arrived without
  ;; the newline it was sent with, where quoted-printable and 8bit kept it.
  (setq end (copy-marker end t))
  (and (> (- end start) 200)
       (vm-inform 7 "Encoding base64..."))
  (let ((buffer-undo-list t)) ;; FIXME: Really?
    (save-excursion
      (and crlf (vm-mime-lf-to-crlf-region start end))
      (condition-case data
	  (base64-encode-region start end B-encoding)
	(error (vm-mime-error "%S" data)))
      (and (> (- end start) 200)
	   (vm-inform 7 "Encoding base64... done"))
      (- end start))))

(define-obsolete-function-alias 'vm-mime-qp-decode-region
  #'quoted-printable-decode-region "2024")

;; FIXME: Use `quoted-printable-encode-region'!
(defun vm-mime-qp-encode-region (start end &optional Q-encoding quote-from)
  (and (> (- end start) 200)
       (vm-inform 7 "Encoding quoted-printable..."))
  (setq end (copy-marker end t))
  (save-excursion
    (require 'qp) ;; `quoted-printable-encode-region' is not autoloaded :-(
    (declare-function quoted-printable-encode-region "qp")
    (defvar mm-use-ultra-safe-encoding)
    (let ((mm-use-ultra-safe-encoding (if quote-from t nil)))
      ;; Fold, except when Q-encoding a header word, which has no lines to
      ;; fold and strips the soft breaks again below.  Without the third
      ;; argument `quoted-printable-encode-region' encodes the characters
      ;; and leaves the lines however long they were, so a quoted-printable
      ;; part could carry a line past the 76 of RFC 2045 -- and no soft line
      ;; break was ever emitted, whatever the text looked like.
      (quoted-printable-encode-region start end (not Q-encoding)))
    (when Q-encoding
      (goto-char start)
      (while (search-forward "=\n" end t)
	(delete-char -2))
      ;; strip out the soft line breaks
      (goto-char start)
      (while (search-forward "\n" end t)
	(delete-char -1)))
    (and (> (- end start) 200)
	 (vm-inform 7 "Encoding quoted-printable... done"))
    (- end start)))

(defun vm-mime-uuencode-decode-region (start end &optional crlf)
  (vm-emit-mime-decoding-message "Decoding uuencoded stuff...")
  (let ((work-buffer nil)
	(region-buffer (current-buffer))
	(case-fold-search nil)
	(tempfile (vm-make-tempfile-name)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring region-buffer start end)
	  (goto-char (point-min))
	  (or (re-search-forward "^begin [0-7][0-7][0-7] " nil t)
	      (vm-mime-error "no begin line"))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (insert tempfile "\n")
	  (goto-char (point-max))
	  (beginning-of-line)
	  ;; Eudora reportedly doesn't terminate uuencoded multipart
	  ;; bodies with a line break. 21 June 1998.
	  ;; Actually it looks like Eudora doesn't understand the
	  ;; multipart newline boundary rule at all and can leave
	  ;; all types of attachments missing a line break.
	  (if (looking-at "^end\\'")
	      (progn
		(goto-char (point-max))
		(insert "\n")))
	  (uudecode-decode-region (point-min) (point-max))
	  (and crlf
	       (vm-mime-crlf-to-lf-region (point-min) (point-max)))
	  (set-buffer region-buffer)
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))
      (vm-error-free-call 'delete-file tempfile)))
  (vm-emit-mime-decoding-message "Decoding uuencoded stuff... done"))

;;; Raw 8-bit header text -- RFC 6532, and mail that predates it
;;
;; A folder buffer holds bytes: VM reads it as raw text so that positions and
;; MIME decoding work on the message as it arrived.  Header text is therefore
;; ASCII plus whatever bytes the sender put there, and RFC 2047 encoded words
;; are decoded from that.  Text that is simply 8-bit -- legal under RFC 6532,
;; and sent regardless of any RFC for decades -- carries no character set of
;; its own, so it has to be guessed.  See `vm-mime-8bit-header-charsets'.

(defun vm-decode-8bit-text (string)
  "Return STRING with raw 8-bit bytes decoded into characters.
The coding systems in `vm-mime-8bit-header-charsets' are tried in order and
the first one that leaves no undecodable byte behind wins.  STRING is
returned unchanged if it holds no raw bytes, if none of the coding systems
accounts for all of them, or if it cannot be handled as bytes at all -- text
that is already decoded is never decoded twice."
  (let ((bytes (cond ((not (multibyte-string-p string)) string)
		     ;; A run taken from a multibyte buffer: only raw bytes
		     ;; and ASCII can be turned back into bytes.  Anything
		     ;; else has been decoded already.
		     ((memq 'eight-bit (find-charset-string string))
		      (condition-case nil
			  (string-to-unibyte string)
			(error nil))))))
    (if (or (null bytes) (not (string-match-p "[\200-\377]" bytes)))
	string
      (let ((charsets vm-mime-8bit-header-charsets)
	    (result nil)
	    coding try)
	(while (and charsets (null result))
	  (setq coding (car charsets)
		charsets (cdr charsets))
	  (when (vm-coding-system-p coding)
	    (setq try (decode-coding-string bytes coding))
	    (unless (memq 'eight-bit (find-charset-string try))
	      (setq result try))))
	(or result string)))))

(defun vm-decode-8bit-text-region (start end)
  "Decode raw 8-bit bytes between START and END into characters.
Each run of bytes is decoded on its own, so text that was already decoded --
by `vm-decode-mime-encoded-words', which runs first and may have put real
characters in the same header -- is left alone."
  (when vm-mime-8bit-header-charsets
    (save-excursion
      (let ((end-marker (copy-marker end t))
	    (buffer-read-only nil)
	    (inhibit-read-only t)
	    raw decoded)
	(goto-char start)
	;; In a multibyte buffer this finds the eight-bit characters that
	;; undecodable bytes become; in a unibyte one, the bytes themselves.
	(while (re-search-forward "[\200-\377]+" end-marker t)
	  (setq raw (match-string-no-properties 0)
		decoded (vm-decode-8bit-text raw))
	  (unless (equal raw decoded)
	    (delete-region (match-beginning 0) (match-end 0))
	    (insert decoded)))
	(set-marker end-marker nil)))))

(defun vm-decode-mime-message-headers (&optional m)
  ;; The end is held as a marker because decoding shortens the text it
  ;; decodes, so a position taken before the first pass is too far along for
  ;; the second one.
  (let ((start (if m (vm-headers-of m) (point)))
	(end (copy-marker (if m (vm-text-of m) (point-max)) t)))
    (unwind-protect
	;; Encoded words first: they state their own character set, so they
	;; are not a guess, and decoding them can leave real characters in
	;; the same header as bytes that still need one.
	(progn
	  (vm-decode-mime-encoded-words start end)
	  (vm-decode-8bit-text-region start end))
      (set-marker end nil))))

;; optional argument rstart and rend delimit the region in
;; which to decode
(defun vm-decode-mime-encoded-words (&optional rstart rend)
  (let ((case-fold-search t)
	(buffer-read-only nil)
	charset need-conversion encoding match-start match-end start end
	previous-end)
    (save-excursion
      (goto-char (or rstart (point-min)))
      (while (re-search-forward vm-mime-encoded-word-regexp rend t)
	(setq match-start (match-beginning 0)
	      match-end (match-end 0)
	      charset (buffer-substring (match-beginning 1) (match-end 1))
              need-conversion nil
	      encoding (buffer-substring (match-beginning 4) (match-end 4))
	      start (match-beginning 5)
	      end (copy-marker (match-end 5) t))
	;; don't change anything if we can't display the
	;; character set properly.
	(if (and (not (vm-mime-charset-internally-displayable-p charset))
		 (not (setq need-conversion
			    (vm-mime-can-convert-charset charset))))
	    nil
	  ;; suppress whitespace between encoded words.
	  (and previous-end
	       (string-match "\\`[ \t\n]*\\'"
			     (buffer-substring previous-end match-start))
	       (setq match-start previous-end))
	  (delete-region end match-end)
	  (condition-case data
	      (cond ((string-match "B" encoding)
		     (vm-mime-base64-decode-region start end))
		    ((string-match "Q" encoding)
		     (vm-mime-Q-decode-region start end))
		    (t (vm-mime-error "unknown encoded word encoding, %s"
				      encoding)))
	    (vm-mime-error (apply 'message (cdr data))
			   (goto-char start)
			   (insert "**invalid encoded word**")
			   (delete-region (point) end)))
	  (and need-conversion
	       (setq charset (vm-mime-charset-convert-region
			      charset start end)))
	  (vm-mime-charset-decode-region charset start end)
	  (goto-char end)
	  (setq previous-end end)
	  (delete-region match-start start))))))

(defun vm-decode-header-text-in-buffer ()
  "Decode the header text in the current buffer for display.
Both passes, in the order that matters: RFC 2047 encoded words, which state
their own character set, and then whatever raw 8-bit text is left, which does
not and has to be guessed at."
  (vm-decode-mime-encoded-words)
  (vm-decode-8bit-text-region (point-min) (point-max)))

(defun vm-decode-mime-encoded-words-in-string (string)
  "Return STRING with its header text decoded for display.
RFC 2047 encoded words are decoded, and so is text that is simply raw 8-bit;
see `vm-decode-8bit-text'.  Both are needed here rather than only the first,
because this is what the summary lines and the composition headers are built
from, and a folder buffer holds the bytes of the message as it arrived.

The work happens in a buffer rather than on the string, so that a header
holding both an encoded word and raw 8-bit text gets each run treated on its
own -- once the encoded word is decoded the string holds real characters and
raw bytes together, and there is no one character set for the whole of it."
  (if (or (and vm-display-using-mime
	       (let ((case-fold-search t))
		 (string-match vm-mime-encoded-word-regexp string)))
	  (and vm-mime-8bit-header-charsets
	       ;; Matches raw bytes only; a character that has already been
	       ;; decoded does not match, so decoded text takes this exit.
	       (string-match-p "[\200-\377]" string)))
      (vm-with-string-as-temp-buffer string 'vm-decode-header-text-in-buffer)
    string ))

(defun vm-reencode-mime-absorb-separating-whitespace ()
  "Make whitespace between two to-be-encoded runs part of the first.
RFC 2047 says whitespace *between* two encoded words is a separator and
not part of the text, so a decoder drops it -- `vm-decode-mime-encoded-words'
does exactly that.  Whitespace carries no `vm-charset' property of its
own, so encoding the runs and leaving the space between them literal
turns \"f\\=\\o\\=\\o b\\=\\ar\" into two encoded words with a space between,
which decodes back as \"f\\=\\o\\=\\ob\\=\\ar\".

Give such whitespace the charset of the run before it, so it is encoded
along with it and stays significant."
  (let ((start (point-min))
	charset pos)
    (while (< start (point-max))
      (setq charset (get-text-property start 'vm-charset))
      (setq pos (or (next-single-property-change start 'vm-charset)
		    (point-max)))
      (when (and (null charset)
		 (> start (point-min))
		 (< pos (point-max))
		 (get-text-property (1- start) 'vm-charset)
		 (get-text-property pos 'vm-charset)
		 (string-match "\\`[ \t\n]+\\'"
			       (buffer-substring-no-properties start pos)))
	(let ((prev-charset (get-text-property (1- start) 'vm-charset))
	      (prev-coding (get-text-property (1- start) 'vm-coding)))
	  (put-text-property start pos 'vm-charset prev-charset)
	  (when prev-coding
	    (put-text-property start pos 'vm-coding prev-coding))))
      (setq start pos))))

(defun vm-reencode-mime-encoded-words ()
  "Reencode in mime the words in the current buffer that need
encoding.  The words that need encoding are expected to have
text-properties set with the appropriate characte set.  This would
have been done if the contents of the buffer are the result of a
previous mime decoding."
  (vm-reencode-mime-absorb-separating-whitespace)
  (let ((charset nil)
	start coding pos q-encoding
	old-size
	(case-fold-search t)
	(done nil))
    (save-excursion
      (setq start (point-min))
      (while (not done)
	(setq charset (get-text-property start 'vm-charset))
	(setq pos (next-single-property-change start 'vm-charset))
	(or pos (setq pos (point-max) done t))
	(if charset
	    (progn
	      (if (setq coding (get-text-property start 'vm-coding))
		  (progn
		    (setq old-size (buffer-size))
		    (encode-coding-region start pos coding)
		    (setq pos (+ pos (- (buffer-size) old-size)))))
	      (setq pos
		    (+ start
		       (if (setq q-encoding
				 (string-match "^iso-8859-\\|^us-ascii"
					       charset))
			   (vm-mime-Q-encode-region start pos)
			 (vm-mime-B-encode-region start pos))))
	      (goto-char pos)
	      (insert "?=")
	      (setq pos (point))
	      (goto-char start)
	      (insert "=?" charset "?" (if q-encoding "Q" "B") "?")
	      (setq pos (+ pos (- (point) start)))))
	(setq start pos)))))

(defun vm-reencode-mime-encoded-words-in-string (string)
  "Reencode in mime the words in STRING that need
encoding.  The words that need encoding are expected to have
text-properties set with the appropriate character set.  This would
have been done if the contents of the buffer are the result of a
previous mime decoding."
  (if (and vm-display-using-mime
	   (text-property-any 0 (length string) 'vm-string t string))
      (vm-with-string-as-temp-buffer string 'vm-reencode-mime-encoded-words)
    string ))

;;----------------------------------------------------------------------------
;;; MIME parsing
;;----------------------------------------------------------------------------

(fset 'vm-mime-parse-content-header 'vm-parse-structured-header)

(defun vm-mime-get-header-contents (header-name-regexp)
  (let (;; (contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^$\\)"))
    (save-excursion
      (let ((case-fold-search t))
	(if (and (re-search-forward regexp nil t)
		 (match-beginning 1)
		 (progn (goto-char (match-beginning 0))
			(vm-match-header)))
	    (vm-matched-header-contents)
	  nil )))))

(cl-defun vm-mime-parse-entity (&optional m &key
					(default-type nil)
					(default-encoding nil)
					(passing-message-only nil))
  "Parse a MIME message M and return its mime-layout.
Optional arguments:
DEFAULT-TYPE is the type to use if no Content-Type is specified.
DEFAULT-ENCODING is the default character encoding if none is
  specified in the message.
PASSING-MESSAGE-ONLY is a boolean argument that says that VM is only
  passing through this message.  So, a full analysis is not required.
                                                     (USR, 2010-01-12)"
  (catch 'return-value
    (save-excursion
      (if (and m (not passing-message-only))
	  (progn
	    (setq m (vm-real-message-of m))
	    (set-buffer (vm-buffer-of m))))
      (let ((case-fold-search t) version type qtype encoding id description
	    disposition qdisposition boundary boundary-regexp start end
	    multipart-list pos-list c-t c-t-e done p) ;; returnval
	(save-excursion
	  (save-restriction
	    (if (and m (not passing-message-only))
		(progn
		  (setq version (vm-get-header-contents m "MIME-Version:")
			version (car (vm-parse-structured-header version))
			type (vm-get-header-contents m "Content-Type:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if type "1.0" nil))
			qtype (vm-parse-structured-header type ?\; t)
			type (vm-parse-structured-header type ?\;)
			encoding (vm-get-header-contents
				  m "Content-Transfer-Encoding:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if encoding "1.0" nil))
			encoding (or encoding "7bit")
			encoding (or (car
				      (vm-parse-structured-header encoding))
				     "7bit")
			id (vm-get-header-contents m "Content-ID:")
			id (car (vm-parse-structured-header id))
			description (vm-get-header-contents
				     m "Content-Description:")
			description (and description
					 (if (string-match "^[ \t\n]*$"
							   description)
					     nil
					   description))
			disposition (vm-get-header-contents
				     m "Content-Disposition:")
			qdisposition (and disposition
					  (vm-parse-structured-header
					   disposition ?\; t))
			disposition (and disposition
					 (vm-parse-structured-header
					  disposition ?\;)))
		  (widen)
		  (narrow-to-region (vm-headers-of m) (vm-text-end-of m)))
	      (goto-char (point-min))
	      (setq type (vm-mime-get-header-contents "Content-Type:")
		    qtype (or (vm-parse-structured-header type ?\; t)
			      default-type)
		    type (or (vm-parse-structured-header type ?\;)
			     default-type)
		    encoding (or (vm-mime-get-header-contents
				  "Content-Transfer-Encoding:")
				 default-encoding)
		    encoding (or (car (vm-parse-structured-header encoding))
				 default-encoding)
		    id (vm-mime-get-header-contents "Content-ID:")
		    id (car (vm-parse-structured-header id))
		    description (vm-mime-get-header-contents
				 "Content-Description:")
		    description (and description (if (string-match "^[ \t\n]*$"
								   description)
						     nil
						   description))
		    disposition (vm-mime-get-header-contents
				 "Content-Disposition:")
		    qdisposition (and disposition
				      (vm-parse-structured-header
				       disposition ?\; t))
		    disposition (and disposition
				     (vm-parse-structured-header
				      disposition ?\;))))
	    (cond ((null m) t)
		  (passing-message-only t)
		  ((null version)
		   (throw 'return-value 'none))
		  ((or vm-mime-ignore-mime-version (string= version "1.0")) t)
		  (t (vm-mime-error "Unsupported MIME version: %s" version)))
	    ;; deal with known losers
	    ;; Content-Type: text
	    (cond ((and type (string-match "^text$" (car type)))
		   (setq type '("text/plain" "charset=us-ascii")
			 qtype '("text/plain" "charset=us-ascii"))))
	    (cond ((and m (not passing-message-only) (null type))
		   (throw 'return-value
			  (vm-make-layout
			   'type '("text/plain" "charset=us-ascii")
			   'qtype '("text/plain" "charset=us-ascii")
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-headers-of m)
			   'header-end (vm-marker (1- (vm-text-of m)))
			   'body-start (vm-text-of m)
			   'body-end (vm-text-end-of m)
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  ((null type)
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (vm-make-layout
		    'type default-type
		    'qtype default-type
		    'encoding encoding
		    'id id
		    'description description
		    'disposition disposition
		    'qdisposition qdisposition
		    'header-start (vm-marker (point-min))
		    'header-body (vm-marker (1- (point)))
		    'body-start (vm-marker (point))
		    'body-end (vm-marker (point-max))
		    'cache (vm-mime-make-cache-symbol)
		    'message-symbol (vm-mime-make-message-symbol m)
		    ))
		  ((null (string-match "[^/ ]+/[^/ ]+" (car type)))
		   (vm-mime-error "Malformed MIME content type: %s"
				  (car type)))
		  ((and (string-match "^multipart/\\|^message/" (car type))
			(null (string-match "^\\(7bit\\|8bit\\|binary\\)$"
					    encoding))
			(if vm-mime-ignore-composite-type-opaque-transfer-encoding
			    (progn
			      ;; Some mailers declare an opaque
			      ;; encoding on a composite type even
			      ;; though it's only a subobject that
			      ;; uses that encoding.  Deal with it
			      ;; by assuming a proper transfer encoding.
			      (setq encoding "binary")
			      ;; return nil so and-clause will fail
			      nil )
			  t ))
		   (vm-mime-error "Opaque transfer encoding used with multipart or message type: %s, %s" (car type) encoding))
		  ((and (string-match "^message/partial$" (car type))
			(null (string-match "^7bit$" encoding)))
		   (vm-mime-error "Non-7BIT transfer encoding used with message/partial message: %s" encoding))
		  ((string-match "^multipart/digest" (car type))
		   (setq c-t '("message/rfc822")
			 c-t-e "7bit"))
		  ((string-match "^multipart/" (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")) ; below
		  ((string-match "^message/\\(rfc822\\|news\\|external-body\\)"
				 (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'parts (list
				   (save-restriction
				     (narrow-to-region (point) (point-max))
				     (vm-mime-parse-entity-safe 
				      m :default-type c-t 
				      :default-encoding c-t-e 
				      :passing-message-only t)))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  (t
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   ))))
	    (setq p (cdr type)
		  boundary nil)
	    (while p
	      (if (string-match "^boundary=" (car p))
		  (setq boundary (car (vm-parse (car p) "=\\(.+\\)"))
			p nil)
		(setq p (cdr p))))
	    (or boundary
		(vm-mime-error
		 "Boundary parameter missing in %s type specification"
		 (car type)))
	    ;; the \' in the regexp is to "be liberal" in the
	    ;; face of broken software that does not add a line
	    ;; break after the final boundary of a nested
	    ;; multipart entity.
	    (setq boundary-regexp
		  (concat "^--" (regexp-quote boundary)
			  "\\(--\\)?[ \t]*\\(\n\\|\\'\\)"))
	    (goto-char (point-min))
	    (setq start nil
		  multipart-list nil
		  done nil)
	    (while (and (not done) (re-search-forward boundary-regexp nil 0))
	      (if (null start)
		  (setq start (match-end 0))
		(and (match-beginning 1)
		     (setq done t))
		(setq pos-list (cons start
				     (cons (1- (match-beginning 0)) pos-list))
		      start (match-end 0))))
	    (if (and (not done)
		     (not vm-mime-ignore-missing-multipart-boundary))
		(vm-mime-error "final %s boundary missing" boundary)
	      (if (and start (not done))
		  (setq pos-list (cons start (cons (point) pos-list)))))
	    (setq pos-list (nreverse pos-list))
	    (while pos-list
	      (setq start (car pos-list)
		    end (car (cdr pos-list))
		    pos-list (cdr (cdr pos-list)))
	      (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (setq multipart-list
			(cons (vm-mime-parse-entity-safe 
			       m :default-type c-t 
			       :default-encoding c-t-e 
			       :passing-message-only t)
			      multipart-list)))))
	    (goto-char (point-min))
	    (or (re-search-forward "^\n\\|\n\\'" nil t)
		(vm-mime-error "MIME part missing header/body separator line"))
	    (vm-make-layout
	     'type type
	     'qtype qtype
	     'encoding encoding
	     'id id
	     'description description
	     'disposition disposition
	     'qdisposition qdisposition
	     'header-start (vm-marker (point-min))
	     'header-end (vm-marker (1- (point)))
	     'body-start (vm-marker (point))
	     'body-end (vm-marker (point-max))
	     'parts (nreverse multipart-list)
	     'cache (vm-mime-make-cache-symbol)
	     'message-symbol (vm-mime-make-message-symbol m)
	     )))))))

(cl-defun vm-mime-parse-entity-safe (&optional m &key
					    (default-type nil)
					    (default-encoding nil)
					    (passing-message-only nil))
  "Like vm-mime-parse-entity, but recovers from any errors.
DEFAULT-TYPE, unless specified, is assumed to be text/plain.
DEFAULT-TRANSFER-ENCODING, unless specified, is assumed to be 7bit.
						(USR, 2010-01-12)"

  (or default-type (setq default-type '("text/plain" "charset=us-ascii")))
  (or default-encoding (setq default-encoding "7bit"))
  ;; don't let subpart parse errors make the whole parse fail.  use default
  ;; type if the parse fails.
  (condition-case error-data
      (vm-mime-parse-entity m :default-type default-type 
			    :default-encoding default-encoding 
			    :passing-message-only passing-message-only)
    (error
     (vm-inform 0 "%s" (car (cdr error-data)))
     ;; don't sleep, no one cares about MIME syntax errors
     (let ((header (if (and m (not passing-message-only))
		       (vm-headers-of m)
		     (vm-marker (point-min))))
	   (text (if (and m (not passing-message-only))
		     (vm-text-of m)
		   (save-excursion
		     (re-search-forward "^\n\\|\n\\'"
					nil 0)
		     (vm-marker (point)))))
	   (text-end (if (and m (not passing-message-only))
			 (vm-text-end-of m)
		       (vm-marker (point-max)))))
     (vm-make-layout
      'type '("error/error")
      'qtype '("error/error")
      'encoding (vm-determine-proper-content-transfer-encoding text text-end)
      ;; cram the error message into the description slot
      'description (car (cdr error-data))
      ;; mark as an attachment to improve the chance that the user
      ;; will see the description.
      'disposition '("attachment")
      'qdisposition '("attachment")
      'header-start header
      'header-end (vm-marker (1- text))
      'body-start text
      'body-end text-end
      'cache (vm-mime-make-cache-symbol)
      'message-symbol (vm-mime-make-message-symbol m)
      )))))

;;----------------------------------------------------------------------------
;;; MIME layout operations
;;----------------------------------------------------------------------------

;;; RFC 2231 -- internationalized MIME parameter values
;;
;; A parameter value may be tagged with a character set and a language, and
;; may be split into numbered segments:
;;
;;     Content-Disposition: attachment; filename*=UTF-8''r%C3%A4ksm%C3%B6rg%C3%A5s
;;     Content-Type: application/pdf;
;;       name*0*=UTF-8''%E5%A0%B1; name*1*=%E5%91%8A.pdf
;;
;; The segments carry raw bytes, and one character may straddle two of them,
;; so the bytes are joined before the character set is applied.  The language
;; tag is discarded: VM has nowhere to put it.

(defconst vm-mime-rfc2231-value-regexp
  "\\`\\([^']*\\)'\\([^']*\\)'\\(\\(?:.\\|\n\\)*\\)\\'"
  "Match an RFC 2231 extended parameter value.
Group 1 is the character set, group 2 the language, group 3 the
percent-encoded text.")

(defconst vm-mime-rfc2231-safe-chars "A-Za-z0-9!#$&+.^_~-"
  "Characters that need no percent-encoding in an RFC 2231 parameter value.
The attribute-char of RFC 2231 section 7, less anything whose literal meaning
elsewhere in a header makes it not worth the risk.")

(defun vm-mime-string-to-bytes (string)
  "Return STRING as a unibyte string, without reinterpreting its characters."
  (if (multibyte-string-p string)
      (encode-coding-string string 'utf-8)
    string))

(defun vm-mime-percent-decode-to-bytes (string)
  "Undo the percent-encoding of STRING, returning a unibyte string.
A percent sign not followed by two hex digits stands for itself, since that
is what senders that do not encode at all produce."
  (let ((i 0) (n (length string)) (bytes nil) c)
    (while (< i n)
      (setq c (aref string i))
      (cond ((and (eq c ?%) (<= (+ i 3) n)
		  (string-match-p "\\`[0-9a-fA-F][0-9a-fA-F]\\'"
				  (substring string (1+ i) (+ i 3))))
	     (push (string-to-number (substring string (1+ i) (+ i 3)) 16)
		   bytes)
	     (setq i (+ i 3)))
	    ((< c 256)
	     (push c bytes)
	     (setq i (1+ i)))
	    (t
	     ;; A character that was never encoded at all.  Keep its bytes.
	     (dolist (b (append (encode-coding-string (char-to-string c) 'utf-8)
				nil))
	       (push b bytes))
	     (setq i (1+ i)))))
    (apply #'unibyte-string (nreverse bytes))))

(defun vm-mime-decode-rfc2231-bytes (bytes charset)
  "Decode BYTES, a unibyte string, according to the MIME CHARSET.
CHARSET may be nil or unknown to Emacs, in which case the encoding is
guessed rather than the bytes being shown raw."
  (let ((coding (and charset (not (equal charset ""))
		     (vm-mime-charset-to-coding charset))))
    (decode-coding-string bytes
			  (if (and coding (vm-coding-system-p coding)
				   (not (eq coding 'undecided)))
			      coding
			    'undecided))))

(defun vm-mime-decode-rfc2231-value (value)
  "Decode VALUE, the value of a NAME* parameter, per RFC 2231.
VALUE is CHARSET'LANGUAGE'TEXT with TEXT percent-encoded.  A value missing
the character set section is still percent-decoded, since senders do send
that."
  (if (string-match vm-mime-rfc2231-value-regexp value)
      (vm-mime-decode-rfc2231-bytes
       (vm-mime-percent-decode-to-bytes (match-string 3 value))
       (match-string 1 value))
    (vm-mime-decode-rfc2231-bytes
     (vm-mime-percent-decode-to-bytes value) nil)))

(defun vm-mime-get-rfc2231-parameter (name param-list)
  "Return parameter NAME from PARAM-LIST, decoded from RFC 2231 notation.
Returns nil if NAME does not appear in that notation.

Both forms are handled: a single extended value, NAME*=, and continuations
NAME*0, NAME*1 and so on, in which each segment may independently be extended
\(NAME*0*=).  The character set is taken from the first segment, which is
where RFC 2231 section 4.1 puts it, and is applied only after the segments
have been joined, because a character may be split across two of them."
  (let ((single (vm-mime-get-xxx-parameter-internal
		 (concat name "*") param-list)))
    (if single
	(vm-mime-decode-rfc2231-value single)
      (let ((n 0) (bytes "") (found nil) charset segment extended)
	(while (progn
		 (setq extended (vm-mime-get-xxx-parameter-internal
				 (format "%s*%d*" name n) param-list)
		       segment (or extended
				   (vm-mime-get-xxx-parameter-internal
				    (format "%s*%d" name n) param-list)))
		 segment)
	  (setq found t)
	  (if extended
	      (let ((text segment))
		(when (and (= n 0)
			   (string-match vm-mime-rfc2231-value-regexp segment))
		  (setq charset (match-string 1 segment)
			text (match-string 3 segment)))
		(setq bytes (concat bytes
				    (vm-mime-percent-decode-to-bytes text))))
	    ;; A plain segment is literal text; its percent signs are not
	    ;; encoding.
	    (setq bytes (concat bytes (vm-mime-string-to-bytes segment))))
	  (setq n (1+ n)))
	(and found (vm-mime-decode-rfc2231-bytes bytes charset))))))

(defun vm-mime-encode-rfc2231-value (string)
  "Return STRING percent-encoded for use in an RFC 2231 parameter value."
  (mapconcat (lambda (byte)
	       (if (string-match-p (concat "[" vm-mime-rfc2231-safe-chars "]")
				   (char-to-string byte))
		   (char-to-string byte)
		 (format "%%%02X" byte)))
	     (append (encode-coding-string string 'utf-8) nil)
	     ""))

(defun vm-mime-quote-parameter-value (value)
  "Return VALUE quoted for use in a MIME parameter."
  (concat "\"" (vm-replace-in-string value "[\"\\\\]" "\\\\\\&") "\""))

(defun vm-mime-encode-parameter (name value)
  "Return a MIME parameter string assigning VALUE to NAME.
An ASCII VALUE is quoted, as it always was.  Anything else is written in the
RFC 2231 extended notation, which is what current mail clients send and
expect for international file names; the alternative, an RFC 2047 encoded
word, is not permitted in a parameter value, though VM still accepts it on
the way in."
  (if (string-match-p "\\`[[:ascii:]]*\\'" value)
      (concat name "=" (vm-mime-quote-parameter-value value))
    (concat name "*=UTF-8''" (vm-mime-encode-rfc2231-value value))))

(defun vm-mime-parameter-name-regexp (name)
  "Return a regexp matching an assignment to parameter NAME.
Matches the plain form and every RFC 2231 spelling of it, so that a parameter
can be replaced without leaving an alternative spelling of it behind."
  (concat "\\`" (regexp-quote name) "\\(\\*[0-9]*\\)?\\*?="))

(defun vm-mime-get-xxx-parameter-internal (name param-list)
  "Return the parameter NAME from PARAM-LIST."
  (let ((match-end (1+ (length name)))
	(name-regexp (concat (regexp-quote name) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match name-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (substring (car param-list) match-end))))

(defun vm-mime-get-xxx-parameter (name param-list)
  "Return the parameter NAME from PARAM-LIST.

RFC 2231 notation is decoded: a character-set-tagged value, NAME*=, and
continuations, NAME*0 and so on, whether or not the segments are themselves
tagged.  See `vm-mime-get-rfc2231-parameter'.

The tagged form wins over a plain NAME= when a sender supplies both.  RFC
2231 does not allow both, but senders do send them, and then the plain one is
the deliberately lossy fallback -- an ASCII approximation of the real name."
  (or (vm-mime-get-rfc2231-parameter name param-list)
      (vm-mime-get-xxx-parameter-internal name param-list)))

(defun vm-mime-get-parameter (layout param)
  (let ((string (vm-mime-get-xxx-parameter 
		 param (cdr (vm-mm-layout-type layout)))))
    (if string (vm-decode-mime-encoded-words-in-string string))))

(defun vm-mime-get-disposition-parameter (layout param)
  (let ((string (vm-mime-get-xxx-parameter 
		 param (cdr (vm-mm-layout-disposition layout)))))
    (if string (vm-decode-mime-encoded-words-in-string string))))

(defun vm-mime-set-xxx-parameter (param value param-list)
  (let ((match-end (1+ (length param)))
	(param-regexp (concat (regexp-quote param) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match param-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (setcar param-list (concat param "=" value)))))

(defun vm-mime-set-parameter (layout param value)
  (vm-mime-set-xxx-parameter param value (cdr (vm-mm-layout-type layout))))

(defun vm-mime-set-qparameter (layout param value)
  (setq value (concat "\"" value "\""))
  (vm-mime-set-xxx-parameter param value (cdr (vm-mm-layout-qtype layout))))

;;----------------------------------------------------------------------------
;;; Working with MIME layouts
;;----------------------------------------------------------------------------

(defun vm-mime-insert-mime-body (layout)
  "Insert in the current buffer the body of a mime part given by LAYOUT."
  (vm-insert-region-from-buffer 
   (marker-buffer (vm-mm-layout-body-start layout))
   (vm-mm-layout-body-start layout)
   (vm-mm-layout-body-end layout)))

(defun vm-mime-insert-mime-headers (layout)
  "Insert in the current buffer the headers of a mime part given by LAYOUT."
  (vm-insert-region-from-buffer
   (marker-buffer (vm-mm-layout-header-start layout))
   (vm-mm-layout-header-start layout)
   (vm-mm-layout-header-end layout)))

(defvar buffer-display-table)
(defvar standard-display-table)

(defun vm-generate-new-presentation-buffer (folder-buffer name)
  "Generate a new Presentation buffer for FOLDER-BUFFER.  NAME is
a string denoting the folder name."
  (let ((pres-buf (vm-generate-new-multibyte-buffer 
		   (concat name " Presentation"))))
    (with-current-buffer pres-buf
      (buffer-disable-undo (current-buffer))
      (setq mode-name "VM Presentation"
	    major-mode 'vm-presentation-mode
	    vm-message-pointer (list nil)
	    vm-mail-buffer folder-buffer
	    mode-popup-menu (and vm-use-menus
				 (vm-menu-support-possible-p)
				 (vm-menu-mode-menu))
	    ;; Tell XEmacs/MULE not to mess with the text on writes.
	    buffer-read-only t
	    mode-line-format vm-mode-line-format)
      ;; scroll in place messes with scroll-up and this loses
      (defvar scroll-in-place)
      (make-local-variable 'scroll-in-place)
      (setq scroll-in-place nil)
      (when (fboundp 'set-buffer-file-coding-system)
	(set-buffer-file-coding-system (vm-binary-coding-system) t))
      (vm-fsfemacs-nonmule-display-8bit-chars)
      (if (and vm-mutable-frame-configuration vm-frame-per-folder
	       (vm-multiple-frames-possible-p))
	  (vm-set-hooks-for-frame-deletion))
      (use-local-map vm-mode-map)
      (vm-toolbar-install-or-uninstall-toolbar)
      (when (vm-menu-support-possible-p)
	(vm-menu-install-menus))
      (run-hooks 'vm-presentation-mode-hook))
    pres-buf))

(defun vm-make-presentation-copy (m)
  "Create a copy of the message M in the Presentation Buffer.  If
the message is external then the copy is made from the external
source of the message."
  (let (;; (mail-buffer (current-buffer))
	pres-buf mm
	(real-m (vm-real-message-of m))
	(modified (buffer-modified-p)))
    (when (or (null vm-presentation-buffer-handle)
	      (null (buffer-name vm-presentation-buffer-handle)))
      ;; Create a new Presentation buffer
      (setq pres-buf (vm-generate-new-presentation-buffer 
		      (current-buffer) (buffer-name)))
      (setq vm-presentation-buffer-handle pres-buf))
    (setq pres-buf vm-presentation-buffer-handle)
    (setq vm-presentation-buffer vm-presentation-buffer-handle)
    (setq vm-mime-decoded nil)
    ;; W3 or some other external mode might have set local colours in this
    ;; buffer, and XEmacs's `remove-specifier' took them off again before a
    ;; different message was shown here.  Emacs has no equivalent: a face
    ;; is not specified per buffer, so there is nothing to remove.
    (with-current-buffer (vm-buffer-of real-m)
      (save-restriction
	(widen)
	;; must reference this now so that headers will be in
	;; their final position before the message is copied.
	;; otherwise the vheader offset computed below will be
	;; wrong.
	(vm-vheaders-of real-m)
	(set-buffer pres-buf)
	;; do not keep undo information in presentation buffers 
	(setq buffer-undo-list t)
	(widen)
	(let ((buffer-read-only nil)
	      (inhibit-read-only t))
	  ;; We don't care about the buffer-modified-p flag of the
	  ;; Presentation buffer.  Only that of the folder matters.
	  (unwind-protect
	      (progn
		(erase-buffer)
		(insert-buffer-substring (vm-buffer-of real-m)
					 (vm-start-of real-m)
					 (vm-end-of real-m)))
	    (vm-reset-buffer-modified-p modified pres-buf)))
	;; make a modifiable copy of the message struct
	(setq mm (copy-sequence m))
	;; also a modifiable copy of the location data
	(vm-set-location-data-of mm (vm-copy (vm-location-data-of m)))
	;; and of the soft data, because the cached MIME layout lives there
	;; and its markers point into whichever buffer was parsed.  Sharing
	;; the vector let a layout parsed here -- see vm-fetch-message, which
	;; parses the current buffer -- overwrite the folder's cache with
	;; markers into this buffer, which is then erased and refilled for
	;; the next message.  That is issue #109: the part markers all end up
	;; meaningless and no part has any text.  Copied shallowly, so every
	;; field still refers to the same object it did before; nothing but
	;; the layout is ever written through a presentation copy.
	(vm-set-softdata-of mm (copy-sequence (vm-softdata-of m)))
	(set-marker (vm-start-of mm) (point-min))
	(set-marker (vm-headers-of mm) (+ (vm-start-of mm)
					  (- (vm-headers-of real-m)
					     (vm-start-of real-m))))
	(set-marker (vm-vheaders-of mm) (+ (vm-start-of mm)
					   (- (vm-vheaders-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-text-of mm) (+ (vm-start-of mm)
				       (- (vm-text-of real-m)
					  (vm-start-of real-m))))
	(set-marker (vm-text-end-of mm) (+ (vm-start-of mm)
					   (- (vm-text-end-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-end-of mm) (+ (vm-start-of mm)
				      (- (vm-end-of real-m)
					 (vm-start-of real-m))))

	;; An external body is fetched into the folder buffer before this copy is
	;; made -- `vm-preview-current-message' does it, under
	;; `vm-external-fetch-message-for-presentation'.  Fetching it again here,
	;; into the presentation buffer, is what the questions in this comment
	;; were about:
	;;
	;;   why is this being done here, rather than in
	;;   vm-present-current-message or vm-show-current-message?
	;;   it was inserted by Rob F in rev. 506.1.1     USR, 2012-04-09
	;;   Let us turn it off and see waht happens.     USR, 2012-11-21
	;;
	;; Turned off now, with what happens measured (issue #585).  It fetched
	;; whatever that option said, since the option is only consulted in
	;; `vm-preview-current-message'; it fetched into this buffer rather than
	;; the folder, so `vm-body-to-be-retrieved-of' stayed set and the next
	;; presentation fetched the same body over again; and the layout it
	;; parsed here outlived the filling of the buffer it described, leaving
	;; parts whose markers had all collapsed to the end -- which is the empty
	;; attachment of #386.
	;;
	;; The `X-VM-Storage:' case is a different mechanism, on the copy rather
	;; than the folder, and stays.
	(goto-char (point-min))
	(when (re-search-forward vm-external-storage-header-regexp
				 (vm-text-of mm) t)
	  (vm-fetch-message (read (current-buffer)) mm))

	;; Attempt to show a message about the missing body.
	;; But it is not working right.  Needs more work.  USR, 2012-04-09

	;; This might be redundant.  Wasn't in revision 717.
	;; fixup the reference to the message
	(setcar vm-message-pointer mm)))))

(defun vm-fetch-message (storage mm)
  "Fetch the real message based on the \"^X-VM-Storage:\" header.

This allows for storing only the headers required for the summary
and maybe a small preview of the message, or keywords for search,
etc.  Only when displaying it the actual message is fetched based
on the storage handler.

The information about the actual message is stored in the
\"^X-VM-Storage:\" header and should be a Lisp list of the
following format.

    (HANDLER ARGS...)

HANDLER should correspond to a `vm-fetch-HANDLER-message'
function, e.g., the handler `file' corresponds to the function
`vm-fetch-file-message' which gets two arguments, the message
descriptor and the filename containing the message, and inserts the
message body from the file into the current buffer.  For example,

    X-VM-Storage: (file \"message-11\")

will fetch the actual message from the file \"message-11\"."
  (goto-char (match-end 0))
  (with-current-buffer (marker-buffer (vm-text-of mm))
    (let ((buffer-read-only nil)
	  (inhibit-read-only t)
	  (buffer-undo-list t)
	  (fetch-result nil))
      (goto-char (vm-text-of mm))
      (delete-region (point) (point-max))
      ;; Remember that this might do process I/O and accept-process-output,
      ;; allowing other threads to run!!!  USR, 2010-07-11 
      (vm-inform 6 "%s: Fetching message from external source..." (buffer-name))
      (setq fetch-result
	    (apply (intern (format "vm-fetch-%s-message" (car storage)))
		   mm (cdr storage)))
      (when fetch-result
	(vm-inform 6 "%s: Fetching message from external source... done"
		   (buffer-name))
	;; delete the new headers
	(delete-region (vm-text-of mm)
		       (or (re-search-forward "\n\n" (point-max) t)
			   (point-max)))
	;; fix markers now
	(set-marker (vm-text-end-of mm) (point-max))	
	(set-marker (vm-end-of mm) (point-max))
	;; now care for the layout of the message, old layouts are
	;; invalid as the presentation buffer may have been used for
	;; other messages in the meantime and the marker got invalid
	;; by this.
	(vm-set-mime-layout-of mm (vm-mime-parse-entity-safe))
	))))
  
(defun vm-fetch-message-size (storage mm)
  "Return the size of the message MM using the STORAGE specification.
The STORAGE specification is given in the same format as for
`vm-fetch-message', which see."
  (apply (intern (format "vm-fetch-%s-message-size" (car storage)))
	 mm (cdr storage)))

(defun vm-fetch-file-message (_m filename)
  "Insert the message with message descriptor MM stored in the given FILENAME."
  (insert-file-contents filename nil nil nil t)
  t)

(defalias 'vm-fetch-mode 'vm-mode)
(put 'vm-fetch-mode 'vm-called-by-vm t)
(put 'vm-fetch-mode 'mode-class 'special)
(defalias 'vm-presentation-mode 'vm-mode)
(put 'vm-presentation-mode 'vm-called-by-vm t)
(put 'vm-presentation-mode 'mode-class 'special)

(defvar buffer-file-coding-system)

(defun vm-determine-proper-charset (beg end)
  "Work out what MIME character set to use for sending a message.

Uses `us-ascii' if the message is entirely ASCII compatible.

`vm-coding-system-priorities' is searched, in order, for a coding system that
will encode all the characters in the message.  If none is found, uses
`iso-2022-jp', which will preserve information for all the character sets of
which Emacs is aware - at the expense of being incompatible with the
recipient's software, if that recipient is outside of East Asia."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((preapproved (vm-get-coding-system-priorities))
	     (ucs-list (vm-get-mime-ucs-list))
	     (cant-encode (check-coding-systems-region
			   (point-min) (point-max)
			   (cons 'us-ascii preapproved))))
	(if (not (assq 'us-ascii cant-encode))
	    ;; If there are only ASCII chars, we're done.
	    "us-ascii"
	  (while (and preapproved
		      (assq (car preapproved) cant-encode)
		      (not (memq (car preapproved) ucs-list)))
	    (setq preapproved (cdr preapproved)))
	  (if preapproved
	      (cadr (assq (car preapproved)
			  vm-mime-mule-coding-to-charset-alist))
	    ;; None of the entries in vm-coding-system-priorities
	    ;; can be used. This can only happen if no universal
	    ;; coding system is included. Fall back to utf-8.
	    "utf-8"))))))

(defun vm-mime-longest-line-length ()
  "The length of the longest line in the accessible region.
The line terminator is not counted, RFC 5322 measuring a line without it."
  (save-excursion
    (goto-char (point-min))
    (let ((longest 0))
      (while (not (eobp))
	(setq longest (max longest (- (line-end-position) (point))))
	(forward-line))
      longest)))

(defun vm-mime-line-length-limit ()
  "The longest line that may be sent in a text part without encoding it.
`vm-mime-max-text-line-length' says, but never above the 998 of RFC 5322:
a longer line cannot be sent as it stands whatever the setting."
  (min (or vm-mime-max-text-line-length 998) 998))

(defconst vm-mime-long-lines-encoding "long-lines"
  "What `vm-determine-proper-content-transfer-encoding' says for a long line.
Not a transfer encoding: `vm-mime-transfer-encode-region' turns it into
quoted-printable.  It cannot say \"quoted-printable\" itself, since that
means to that function that the region is encoded already.")

(defun vm-determine-proper-content-transfer-encoding (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (catch 'done
	(goto-char (point-min))
	(and (re-search-forward "[\000\015]" nil t)
	     (throw 'done "binary"))

	(and (> (vm-mime-longest-line-length) (vm-mime-line-length-limit))
	     (throw 'done vm-mime-long-lines-encoding))

	(goto-char (point-min))
	(and (re-search-forward "[^\000-\177]" nil t)
	     (throw 'done "8bit"))

	"7bit"))))

;;----------------------------------------------------------------------------
;;; Predicates on MIME types and layouts
;;----------------------------------------------------------------------------

(defun vm-mime-types-match (type type/subtype)
  (let ((case-fold-search t))
    (cond ((null type/subtype)
           nil)
          ((string-match "/" type)
	   (if (and (string-match (regexp-quote type) type/subtype)
		    (equal 0 (match-beginning 0))
		    (equal (length type/subtype) (match-end 0)))
	       t
	     nil ))
	  ((and (string-match (regexp-quote type) type/subtype)
		(equal 0 (match-beginning 0))
		(equal (save-match-data
			 (string-match "/" type/subtype (match-end 0)))
		       (match-end 0)))))))

(defvar native-sound-only-on-console)

(defun vm-mime-text/html-handler ()
  (if (eq vm-mime-text/html-handler 'auto-select)
      (setq vm-mime-text/html-handler
            (cond ((and (locate-library "w3m") (executable-find "w3m"))
                   ;; emacs-w3m drives the w3m program; the library alone
                   ;; cannot render anything, and choosing it then means
                   ;; every HTML part fails or waits for a process that is
                   ;; not there.
                   'emacs-w3m)
                  ((executable-find "w3m")
                   'w3m)
                  ((executable-find "lynx")
                   'lynx)
                  ;; shr needs nothing installed, so it is last and it is
                  ;; always there: a reader with none of the above used to
                  ;; get no HTML display at all.  It wants a libxml2-enabled
                  ;; Emacs, which is the usual build but not guaranteed.
                  ((and (fboundp 'libxml-available-p) (libxml-available-p))
                   'shr)))
    vm-mime-text/html-handler))

(defun vm-mime-can-display-internal (layout &optional deep)
  (let ((type (car (vm-mm-layout-type layout))))
    (cond ((vm-mime-types-match "image/jpeg" type)
	   (and (vm-image-type-available-p 'jpeg) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/gif" type)
	   (and (vm-image-type-available-p 'gif) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/png" type)
	   (and (vm-image-type-available-p 'png) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/tiff" type)
	   (and (vm-image-type-available-p 'tiff) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xpm" type)
	   (and (vm-image-type-available-p 'xpm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/pbm" type)
	   (and (vm-image-type-available-p 'pbm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xbm" type)
	   (and (vm-image-type-available-p 'xbm) (vm-images-possible-here-p)))
	  ;; audio/basic was played by XEmacs's own sound support and there
	  ;; is no Emacs equivalent to put here.
	  ((vm-mime-types-match "audio/basic" type) nil)
	  ((vm-mime-types-match "multipart" type) t)
	  ((vm-mime-types-match "message/external-body" type)
	   (or (not deep)
	       (vm-mime-can-display-internal
		(car (vm-mm-layout-parts layout)) t)))
	  ((vm-mime-types-match "message" type) t)
	  ((vm-mime-types-match "text/html" type)
	   ;; Allow vm-mime-text/html-handler to decide if text/html parts are displayable:
           (and (vm-mime-text/html-handler)
		(let ((charset (or (vm-mime-get-parameter layout "charset")
				   "us-ascii")))
		  (vm-mime-charset-internally-displayable-p charset))))
	  ((vm-mime-types-match "text" type)
	   (let ((charset (or (vm-mime-get-parameter layout "charset")
			      "us-ascii")))
	     (or (vm-mime-charset-internally-displayable-p charset)
		 (vm-mime-can-convert-charset charset))))
	  (t nil))))

(defun vm-mime-can-convert (type)
  "If given mime TYPE is convertible to some other type, return a
triple (source-type target-type command).  Otherwise, return nil."
  (or (vm-mime-can-convert-0 type vm-mime-type-converter-alist)
      (vm-mime-can-convert-0 type vm-mime-image-type-converter-alist)))

(defun vm-mime-can-convert-0 (type alist)
  (let (
	;; fake layout. make it the wrong length so an error will
	;; be signaled if vm-mime-can-display-internal ever asks
	;; for one of the other fields
	(fake-layout (make-vector 1 (list nil)))
	best second-best)
    (while (and alist (not best))
      (cond ((and (vm-mime-types-match (car (car alist)) type)
		  (not (vm-mime-types-match (nth 1 (car alist)) type)))
	     (cond ((and (not best)
			 (progn
			   (setcar (aref fake-layout 0) (nth 1 (car alist)))
			   (vm-mime-can-display-internal fake-layout)))
		    (setq best (car alist)))
		   ((and (not second-best)
			 (vm-mime-find-external-viewer (nth 1 (car alist))))
		    (setq second-best (car alist))))))
      (setq alist (cdr alist)))
    (or best second-best)))

(defun vm-mime-convert-undisplayable-layout (layout)
  (catch 'done
    (let ((ooo (vm-mime-can-convert (car (vm-mm-layout-type layout))))
	  ex work-buffer)
      (vm-inform 6 "Converting %s to %s..."
	       (car (vm-mm-layout-type layout))
	       (nth 1 ooo))
      (setq work-buffer (vm-make-work-buffer " *mime object*"))
      (vm-register-message-garbage 'kill-buffer work-buffer)
      (with-current-buffer work-buffer
	;; call-process-region calls write-region.
	;; don't let it do CR -> LF translation.
	(setq selective-display nil)
	(vm-mime-insert-mime-body layout)
	(vm-mime-transfer-decode-region layout (point-min) (point-max))
        ;; It is annoying to use cat for conversion of a mime type which
        ;; is just plain text.  Therefore we do not call it ...
        (setq ex 0)
        (if (= (length ooo) 2)
            (if (search-forward-regexp "\n\n" (point-max) t)
                (delete-region (point-min) (match-beginning 0)))
	  ;; it is arguable that if the type to be converted is text,
	  ;; we should convert from the object's native encoding to
	  ;; the default encoding. However, converting from text is
	  ;; likely to be rare, so we'll have that argument another
	  ;; time.  JCB, 2011-02-04
	  (let ((coding-system-for-write (vm-binary-coding-system))
		(coding-system-for-read (vm-binary-coding-system)))
	    (condition-case data
		(setq ex (call-process-region 
			  (point-min) (point-max) shell-file-name
			  t t nil shell-command-switch (nth 2 ooo)))
	      (error
	       ;; FIXME give a decent error message here
	       (vm-warn 0 2
			"Converstion from %s to %s failed: %S" 
			(car (vm-mm-layout-type layout)) (nth 1 ooo)
			data)
	       (throw 'done nil)))))
	(unless (eq ex 0)
	  (switch-to-buffer work-buffer)
	  (vm-warn 0 2
		   "Conversion from %s to %s failed (exit code %s)"
		   (car (vm-mm-layout-type layout)) (nth 1 ooo) ex)
	  (throw 'done nil))
	(goto-char (point-min))
	;; if the to-type is text, then we will assume that the conversion
	;; process outputs text in the default encoding.
	;; Really we ought to look at process-coding-system-alist etc,
	;; but I suspect that this is rarely used, and will become even
	;; less used as utf-8 becomes universal.  JCB, 2011-02-04
	;; But we will let detect-coding-region do as much work as it
	;; can.  USR, 2011-02-11
	(let* ((charset (vm-mime-find-charset-for-binary-buffer)))
	  (insert "Content-Type: " 
		  (vm-mime-type-with-params
		   (nth 1 ooo) 
		   (and (vm-mime-types-match "text" (nth 1 ooo))
			(list (concat "charset=" charset))))
		  "\n")
	  (insert "Content-Transfer-Encoding: binary\n\n")
	  (set-buffer-modified-p nil)
	  (vm-inform 6 "Converting %s to %s... done"
		   (car (vm-mm-layout-type layout))
		   (nth 1 ooo))
	  ;; irritatingly, we need to set the coding system here as well
	  (vm-make-layout
	   'type
	   (append (list (nth 1 ooo))
		   (append (cdr (vm-mm-layout-type layout))
			   (if (vm-mime-types-match "text" (nth 1 ooo))
			       (list (concat 
				      "charset=" charset)))))
	   'qtype
	   (append (list (nth 1 ooo)) (cdr (vm-mm-layout-type layout)))
	   'encoding "binary"
	   'id (vm-mm-layout-id layout)
	   'description (vm-mm-layout-description layout)
	   'disposition (vm-mm-layout-disposition layout)
	   'qdisposition (vm-mm-layout-qdisposition layout)
	   'header-start (vm-marker (point-min))
	   'header-end (vm-marker (1- (point)))
	   'body-start (vm-marker (point))
	   'body-end (vm-marker (point-max))
	   'parts nil
	   'cache (vm-mime-make-cache-symbol)
	   'message-symbol
	   (vm-mime-make-message-symbol (vm-mm-layout-message layout))
	   'display-error nil 
	   'layout-is-converted t ))))))

(defun vm-mime-find-charset-for-binary-buffer ()
  "Finds an appropriate MIME character set for the current buffer,
assuming that it is text."
  (let ((coding-systems
	 (condition-case _err
	     (detect-coding-region (point-min) (point-max))
	   (error nil)))
	(coding-system nil) (n nil))
    ;; XEmacs returns a single coding-system sometimes
    (unless (listp coding-systems)
      (setq coding-systems (list coding-systems)))
    ;; Skip over the uninformative coding-systems
    (setq n
	  (vm-find coding-systems
		   (function 
		    (lambda (coding)
		      (and coding
			   (not (memq (vm-coding-system-name-no-eol coding)
				      '(raw-text no-conversion))))))))
    (when n
      (setq coding-system (nth n coding-systems)))
    ;; If no informative coding-system detected then use the default
    ;; buffer-file-coding-system 
    (when (or (null coding-system)
	      (eq (vm-coding-system-name-no-eol coding-system) 'undecided))
      (setq coding-system buffer-file-coding-system))
    (or (cadr (assq (vm-coding-system-name-no-eol coding-system)
		    vm-mime-mule-coding-to-charset-alist))
	"us-ascii")))
    

(defun vm-mime-can-convert-charset (charset)
  (vm-mime-can-convert-charset-0 charset vm-mime-charset-converter-alist))

(defun vm-mime-can-convert-charset-0 (charset alist)
  (let ((done nil))
    (while (and alist (not done))
      (cond ((and (vm-string-equal-ignore-case (car (car alist)) charset)
		  (vm-mime-charset-internally-displayable-p
		   (nth 1 (car alist))))
	     (setq done t))
	    (t (setq alist (cdr alist)))))
    (and alist (car alist))))

(defun vm-mime-charset-convert-region (charset b-start b-end)
  (let ((b (current-buffer))
	start end oldsize work-buffer ooo ex)
    (setq ooo (vm-mime-can-convert-charset charset))
    (setq work-buffer (vm-make-work-buffer " *mime object*"))
    (unwind-protect
	(with-current-buffer work-buffer
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  ;; call-process-region calls write-region.
	  ;; don't let it do CR -> LF translation.
	  (setq selective-display nil)
	  (let ((coding-system-for-write (vm-binary-coding-system))
		(coding-system-for-read (vm-binary-coding-system)))
	    (setq ex (call-process-region 
		      (point-min) (point-max) shell-file-name
		      t t nil shell-command-switch (nth 2 ooo))))
	  (unless (eq ex 0)
	    (vm-warn 0 1 "Conversion from %s to %s signalled exit code %s"
		     (nth 0 ooo) (nth 1 ooo) ex))
	  ;; This cannot possibly safe.  USR, 2011-02-11
	  (setq start (point-min) end (point-max))
	  (with-current-buffer b
	    (save-excursion
	      (goto-char b-start)
	      (insert-buffer-substring work-buffer start end)
	      (delete-region (point) (+ (point) oldsize))))
	  (nth 1 ooo))
      ;; unwind-protection
      (when work-buffer (kill-buffer work-buffer)))))

(cl-defun vm-mime-should-display-button (layout &key ignore-content-disposition)
  "Checks whether MIME object with LAYOUT should be displayed as
a button.  Optional keyword argument IGNORE-CONTENT-DISPOSITION
says whether the Content-Disposition header of the MIME object
should be ignored."
  (not (vm-mime-should-display-object layout ignore-content-disposition)))

(defun vm-mime-should-display-object (layout ignore-content-disposition)
  "Checks whether MIME object with LAYOUT should be automatically
displayed.  Optional keyword argument IGNORE-CONTENT-DISPOSITION
says whether the Content-Disposition header of the MIME object
should be ignored."
  ;; Karnaugh map analysis shows that
  ;; - attachment disposition objects should be buttons
  ;; - all auto-displayed objects should not be buttons
  ;; - inline objects should be displayed if
  ;;   vm-mime-honor-content-disposition is either nil or
  ;;   it is 'internal-only and the object is internal-displayable
  ;; - all other cases should be buttons
  (let ((type (car (vm-mm-layout-type layout)))
	(disposition (car (vm-mm-layout-disposition layout)))
	(honor-content-disposition (and (not ignore-content-disposition)
					vm-mime-honor-content-disposition)))
    (setq disposition (and disposition (downcase disposition)))
    (cond 
     ;; multiparts are always displayed
     ((vm-mime-types-match "multipart" type)
      t)				
     ;; attachment objects are not displayed if the disposition is honored
     ((and (equal disposition "attachment")
	   honor-content-disposition)
      nil)				
     ;; inline objects are auto-displayed
     ;; if honor = t or 
     ;; honor = 'internal-only and they are internally auto-displayable
     ((equal disposition "inline")
      (cond ((eq honor-content-disposition 'internal-only)
	     (and (vm-mime-auto-displayable layout)
		  (vm-mime-internally-displayable layout)))
	    ((eq honor-content-disposition t)
	     t)
	    (t
	     (vm-mime-auto-displayable layout))))
     (t
      (vm-mime-auto-displayable layout)))))

(defun vm-mime-auto-displayable (layout)
  "Returns a boolean value indicating whether MIME object with LAYOUT
should be auto-displayed according to the settings of
`vm-mime-auto-displayed-content-types' and
`vm-mime-auto-displayed-content-type-exceptions'."
  (let ((type (car (vm-mm-layout-type layout))))
    (and (or (eq vm-mime-auto-displayed-content-types t)
	     (vm-find (cons "multipart" vm-mime-auto-displayed-content-types)
		      (lambda (i) (vm-mime-types-match i type))))
	 (not (vm-find vm-mime-auto-displayed-content-type-exceptions
		       (lambda (i) (vm-mime-types-match i type)))))))

(defun vm-mime-internally-displayable (layout)
  (let ((type (car (vm-mm-layout-type layout))))
    (if (or (eq vm-mime-internal-content-types t)
	    (vm-find (cons "multipart" vm-mime-internal-content-types)
		     (lambda (i)
		       (vm-mime-types-match i type))))
	(not (vm-find vm-mime-internal-content-type-exceptions
		      (lambda (i)
			(vm-mime-types-match i type))))
      nil)))

(defun vm-mime-find-external-viewer (type)
  (catch 'done
    (let ((list vm-mime-external-content-type-exceptions)
	  (matched nil))
      (while list
	(if (vm-mime-types-match (car list) type)
	    (throw 'done nil)
	  (setq list (cdr list))))
      (setq list vm-mime-external-content-types-alist)
      (while (and list (not matched))
	(if (and (vm-mime-types-match (car (car list)) type)
		 (cdr (car list)))
	    (setq matched (cdr (car list)))
	  (setq list (cdr list))))
      matched )))
(fset 'vm-mime-can-display-external 'vm-mime-find-external-viewer)

(defun vm-mime-delete-button-maybe (extent)
  (let ((buffer-read-only))
    ;; if displayed MIME object should replace the button
    ;; remove the button now.
    (cond ((vm-extent-property extent 'vm-mime-disposable)
	   (delete-region (vm-extent-start-position extent)
			  (vm-extent-end-position extent))
	   (vm-detach-extent extent)))))

;;------------------------------------------------------------------------------
;;; MIME decoding
;;
;; interactive command:
;;
;; vm-decode-mime-message :: (&optional 
;;			      state :: ENUM('decoded, 'button, 'undecoded))
;;			     -> void
;;------------------------------------------------------------------------------


;;;###autoload
(defun vm-decode-mime-message (&optional state)
  "Decode the MIME objects in the current message.

The first time this command is run on a message, decoding is done.
The second time, buttons for all the objects are displayed instead.
The third time, the raw, undecoded data is displayed.

The optional argument STATE can specify which decode state to display:
`decoded', `button', or `undecoded'.

If decoding, the decoded objects might be displayed immediately, or
buttons might be displayed that you need to activate to view the
object.  See the documentation for the variables

    vm-mime-auto-displayed-content-types
    vm-mime-auto-displayed-content-type-exceptions
    vm-mime-internal-content-types
    vm-mime-internal-content-type-exceptions
    vm-mime-external-content-types-alist

to see how to control whether you see buttons or objects.

If the variable vm-mime-display-function is set, then its value
is called as a function with no arguments, and none of the
actions mentioned in the preceding paragraphs are taken.  At the
time of the call, the current buffer will be the presentation
buffer for the folder and a copy of the current message will be
in the buffer.  The function is expected to make the message
`MIME presentable' to the user in whatever manner it sees fit."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (unless (or vm-display-using-mime vm-mime-display-function)
      (error "MIME display disabled, set vm-display-using-mime non-nil to enable."))
  (if vm-mime-display-function
      (progn
	(vm-make-presentation-copy (car vm-message-pointer))
	(set-buffer vm-presentation-buffer)
	(funcall vm-mime-display-function)
	;; We are done here
	)
    (when (null state)
      (cond ((null vm-mime-decoded)
	     (setq state 'decoded))
	    ((eq vm-mime-decoded 'decoded)
	     (setq state 'buttons))
	    ((eq vm-mime-decoded 'buttons)
	     (setq state 'undecoded))))
    (if vm-mime-decoded
	(cond ((eq state 'buttons)
	       (let ((vm-preview-lines nil)
		     (vm-auto-decode-mime-messages t)
		     (vm-mime-honor-content-disposition nil)
		     (vm-mime-auto-displayed-content-types '("multipart"))
		     (vm-mime-auto-displayed-content-type-exceptions nil))
		 (setq vm-mime-decoded nil)
		 (intern (buffer-name) vm-buffers-needing-display-update)
		 (save-excursion
		   (vm-present-current-message))
		 (setq vm-mime-decoded 'buttons)))
	      ((eq state 'undecoded)
	       (let ((vm-preview-lines nil)
		     (vm-auto-decode-mime-messages nil))
		 (intern (buffer-name) vm-buffers-needing-display-update)
		 (vm-present-current-message))))
      (let ((layout (vm-mm-layout (car vm-message-pointer)))
	    (m (car vm-message-pointer)))
	(vm-emit-mime-decoding-message "Decoding MIME message...")
	(when (stringp layout)
	  (error "Invalid MIME message: %s" layout))
	(when (vm-mime-plain-message-p m)
	  (error "Message needs no decoding."))
	(if (not vm-presentation-buffer)
	    ;; maybe user killed it - make a new one
	    (progn
	      (vm-make-presentation-copy (car vm-message-pointer))
	      (vm-expose-hidden-headers))
	  (set-buffer vm-presentation-buffer))
	;; Are we now in the Presentation buffer?  Why?  USR, 2010-05-08
	(when (and (vm-interactive-p) (eq vm-system-state 'previewing))
	  (let ((vm-display-using-mime nil))
	    (vm-show-current-message)))
	(setq m (car vm-message-pointer))
	(save-restriction
	 (widen)
	 (goto-char (vm-text-of m))
	 (let ((buffer-read-only nil)
	       (modified (buffer-modified-p)))
	   (unwind-protect
	       (save-excursion
		 (unless (eq (vm-mm-encoded-header m) 'none)
		   (vm-decode-mime-message-headers m))
		 (when (vectorp layout)
		   (vm-decode-mime-layout layout)
		   ;; Delete the original presentation copy
		   (delete-region (point) (point-max)))
		 (vm-energize-urls)
		 (vm-highlight-headers-maybe)
		 (vm-fontify-body-maybe)
		 (vm-energize-headers-and-xfaces))
	     (set-buffer-modified-p modified))))
	(with-current-buffer vm-mail-buffer
	  (setq vm-mime-decoded 'decoded))
	(intern (buffer-name vm-mail-buffer) vm-buffers-needing-display-update)
	(vm-update-summary-and-mode-line)
	(with-current-buffer vm-mail-buffer
	  (vm-emit-mime-decoding-message 
	   "Decoding MIME message... done"
	   ))
	)))
  (vm-display nil nil '(vm-decode-mime-message)
	      '(vm-decode-mime-message reading-message)))

(defun vm-mime-get-disposition-filename (layout)
  (let ((filename nil)
        (case-fold-search t))
    (setq filename (or (vm-mime-get-disposition-parameter layout "filename") 
                       (vm-mime-get-disposition-parameter layout "name")))
    (when (not filename)
      (setq filename (or (vm-mime-get-disposition-parameter layout "filename*") 
                         (vm-mime-get-disposition-parameter layout "name*")))
      ;; decode encoded filenames
      (when (and filename  
                 (string-match "^\\([^']+\\)'\\([^']*\\)'\\(.*%[0-9A-F][0-9A-F].*\\)$"
                               filename))
        ;; transform it to something we are already able to decode
        (let ((charset (match-string 1 filename))
              (f (match-string 3 filename)))
          (setq f (vm-replace-in-string f "%\\([0-9A-F][0-9A-F]\\)" "=\\1"))
          (setq filename (concat "=?" charset "?Q?" f "?="))
          (setq filename (vm-decode-mime-encoded-words-in-string filename)))))
    filename))

(defun vm-mime-rewrite-with-inferred-type (layout type2)
  (vm-set-mm-layout-type layout (list type2))
  (vm-set-mm-layout-qtype layout (list (concat "\"" type2 "\""))))

(defun vm-decode-mime-layout (layout &optional dont-honor-c-d)
  "Decode the MIME part in the current buffer using LAYOUT.  
If DONT-HONOR-C-D non-Nil, then don't honor the Content-Disposition
declarations in the attachments and make a decision independently.

LAYOUT can be a mime layout vector.  It can also be a button
extent in the current buffer, in which case the `vm-mime-layout'
property of the overlay will be extracted.  The button may be
deleted. 

Returns t if the display was successful.  Not clear what happens if it
is not successful.                                   USR, 2011-03-25"
  (let ((modified (buffer-modified-p))
	handler new-layout file 
	type primary-type     ; type = primary-type/secondary-type
	;; used only if vm-infer-mime-types is t
	infered-type infered-primary-type 
	(extent nil))
    (unless (vectorp layout)
      ;; handle a button extent
      (setq extent layout
	    layout (vm-extent-property extent 'vm-mime-layout))
      (goto-char (vm-extent-start-position extent))
      ;; if the button is for external-body, use the external-body
      (let ((type (downcase (car (vm-mm-layout-type layout)))))
	(when (vm-mime-types-match "message/external-body" type)
	  (setq layout (car (vm-mm-layout-parts layout))))))
    (unwind-protect
	(progn
	  (setq type (downcase (car (vm-mm-layout-type layout)))
		primary-type (car (vm-parse type "\\([^/]+\\)"))
		file (vm-mime-get-disposition-filename layout))
	  (when (and vm-infer-mime-types file)
	    (setq infered-type (vm-mime-default-type-from-filename file))
	    (when infered-type
	      (setq infered-type (downcase infered-type))
	      (setq infered-primary-type  
		    (car (vm-parse infered-type "\\([^/]+\\)")))))
	  (cond ((and infered-type
		      (or (and vm-infer-mime-types-for-text
			       (vm-mime-types-match "text/plain" type))
			  (vm-mime-types-match "application/octet-stream" type))
		      (not (vm-mime-types-match type infered-type)))
		 (vm-mime-rewrite-with-inferred-type layout infered-type)
		 (setq type (downcase (car (vm-mm-layout-type layout)))
		       primary-type (car (vm-parse type "\\([^/]+\\)")))))
	  (cond 
	   ;; If its a signed message, the signature comes later but we
	   ;; need to verify it now since other code may alter the text
	   ;; (and there's a pretty good chance that the message was
	   ;; encrypted, so it's non-trivial to get the original layout)
	   ((and (vm-mime-types-match "multipart/signed" type)
		 (when (and vm-mime-verify-signatures
			    (string-match 
			     "pkcs7-signature"
			     (vm-mime-get-parameter layout "protocol")))
		   (let ((verified nil))
		     (with-temp-buffer
		       (vm-mime-insert-mime-headers layout)
		       (vm-mime-insert-mime-body layout)
		       (when (smime-verify-buffer)
			 (setq verified t)))
		     (if verified
			 (insert
			  "/*****S/MIME SIGNATURE VERIFICATION SUCCESSFULL*****/\n")
		       (insert
			"/*****S/MIME SIGNATURE VERIFICATION FAILED*****/\n"))))
		 nil))

	   ((and (vm-mime-should-display-button 
		  layout :ignore-content-disposition dont-honor-c-d)
		 (or (vm-mime-display-button layout type primary-type)
		     (funcall 'vm-mime-display-button-application layout)))
	    ;; if the handler returns t, we are done
	    )

	   ((and infered-type
		 (vm-mime-should-display-button 
		  layout :ignore-content-disposition dont-honor-c-d)
		 (vm-mime-display-button 
		  layout infered-type infered-primary-type))
	    ;; if the handler returns t, overwrite the layout type
	    (vm-mime-rewrite-with-inferred-type layout infered-type))

	   ((and (vm-mime-internally-displayable layout)
		 (vm-mime-display-internal layout type primary-type))
	    ;; if the handler returns t, we are done
	    )

	   ((and vm-infer-mime-types infered-type
		 (vm-mime-internally-displayable layout)
		 (vm-mime-display-internal 
		  layout infered-type infered-primary-type))
	    ;; if the handler returns t, overwrite the layout type
	    (vm-mime-rewrite-with-inferred-type layout infered-type))

	   ((vm-mime-types-match "multipart" type)
	    (if (fboundp 
		 (setq handler (vm-mime-handler "display-internal" type)))
		(funcall handler layout)
	      (vm-mime-display-internal-multipart/mixed layout))
	    )

	   ((and (vm-mime-find-external-viewer type)
		 (vm-mime-display-external-generic layout))
	    ;; external viewer worked.  the button should go away.
	    (when extent (vm-set-extent-property
			  extent 'vm-mime-disposable nil)))

	   ((and (not (vm-mm-layout-is-converted layout))
		 (vm-mime-can-convert type)
		 (setq new-layout
		       (vm-mime-convert-undisplayable-layout layout)))
	    ;; conversion worked.  the button should go away.
	    (when extent
	      (vm-set-extent-property extent 'vm-mime-disposable t))
	    (vm-decode-mime-layout new-layout))

	   (t 
	    (when extent (vm-mime-rewrite-failed-button
			  extent
			  (or (vm-mm-layout-display-error layout)
			      "no external viewer defined for type")))
	    (cond ((vm-mime-types-match "message/external-body" type)
		   (if (null extent)
		       (vm-mime-display-button-xxxx layout t)
		     (setq extent nil)))
		  ((vm-mime-types-match "application/octet-stream" type)
		   (vm-mime-display-internal-application/octet-stream
		    (or extent layout)))
		  ;; if everything else fails, just display a button
		  (t
		   (vm-set-mm-layout-display-error 
		    layout "Unknown MIME type")
		   (vm-mime-display-button-application layout))
		  )
	    ))
	  (when extent (vm-mime-delete-button-maybe extent)))
      ;; unwind-protection
      (set-buffer-modified-p modified)))
  t )

(defun vm-mime-display-button (layout type primary-type)
  "Display MIME button, if possible, for a MIME LAYOUT of TYPE and
PRIMARY-TYPE.  Returns a boolean flag indicating success." 
  ;; original conditional-cases changed to fboundp
  ;; checks.  USR, 2011-03-25
  (let (handler)
    (when (or (fboundp 
	       (setq handler (vm-mime-handler "display-button" type)))
	      (fboundp 
	       (setq handler (vm-mime-handler "display-button" primary-type))))
      (funcall handler layout))))

(defun vm-mime-display-internal (layout type primary-type)
  "Display internally MIME LAYOUT of TYPE and PRIMARY-TYPE, if
possible.  Returns a boolean flag indicating success."
  ;; original conditional-cases changed to fboundp
  ;; checks.  USR, 2011-03-25
  (let (handler)
    (when 
	(or (fboundp 
	     (setq handler (vm-mime-handler "display-internal" type)))
	    (fboundp 
	     (setq handler (vm-mime-handler "display-internal" primary-type))))
      (funcall handler layout))))

(defun vm-mime-display-button-text (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-text (layout)
  (vm-mime-display-internal-text/plain layout))

(defun vm-mime-cid-retrieve (url message)
  "Insert the part of MESSAGE that URL names, URL having the cid: scheme.
Returns the part, or nil when the message carries no part with that
Content-ID.  `vm-mime-cid-retrieved' is set only when a part was inserted,
since it is what `vm-mime-display-internal-multipart/related' reads to decide
that the viewer has shown the related parts itself."
  (unless (string-match "\\`cid:" url)
    (error "%S is not a cid url" url))
  (let* ((id (concat "<" (substring url (match-end 0)) ">"))
         (top (vm-mm-layout (vm-real-message-of message)))
         (part (and (vectorp top) (vm-mime-find-leaf-content-id top id))))
    (if (null part)
        (vm-inform 5 "No data for cid %S" id)
      (vm-mime-insert-mime-body part)
      (setq vm-mime-cid-retrieved t))
    part))

(defun vm-mime-html-columns ()
  "The width an HTML converter should render to.
See `vm-html-fill-column', which the reply code binds so that quoted text
does not come out as wide as the window the message was read in."
  (cond ((eq vm-html-fill-column 'window-width)
	 (max 20 (1- (window-width (get-buffer-window (current-buffer))))))
	;; nil used to ask for a page 100000 columns wide, no converter taking
	;; an instruction to leave the text unbroken.  A page laid out that
	;; wide indents a centred table by hundreds of columns, and most HTML
	;; mail is a centred table (#540), so nil is no longer offered.
	((null vm-html-fill-column) vm-html-default-column)
	(t vm-html-fill-column)))

(defun vm-mime-display-internal-w3m-text/html (start end layout)
  (let* ((charset (or (vm-mime-get-parameter layout "charset") "us-ascii"))
	 (coding-system (coding-system-from-name charset)))
    ;; temporarily override default coding-system because we use our
    ;; own. (thanks to Ralf Fassel, viewmail-info, 2015-01-30)
    (let ((default-process-coding-system 
	    (if coding-system (cons coding-system coding-system)
	      default-process-coding-system)))
      (shell-command-on-region
       start (1- end)
       (format "%s -dump -cols %d -T text/html -I %s -O %s"
	       vm-w3m-program (vm-mime-html-columns) charset charset)
       nil t))))
  
(defun vm-mime-display-internal-lynx-text/html (start end _layout)
  (shell-command-on-region 
   start (1- end)
   (format "%s -force_html -dump -pseudo_inlines -stdin -width=%d"
	   vm-lynx-program (vm-mime-html-columns))
   nil t))

(defun vm-mime-display-internal-shr-text/html (start end _layout)
  "Render the HTML between START and END with shr, which Emacs ships.
Unlike the other handlers this needs nothing installed, so it is what makes
HTML display work on a stock Emacs.

No image is fetched.  A remote image in mail reports back to whoever sent
the message that it was opened, so `vm-mime-shr-inhibit-images' is bound
here rather than left to the reader's shr settings, which are for the web."
  (require 'shr)
  (let ((document (libxml-parse-html-region start (1- end)))
	(shr-width (vm-mime-html-columns))
	(shr-inhibit-images vm-mime-shr-inhibit-images)
	(shr-blocked-images (if vm-mime-shr-inhibit-images "." nil))
	;; the presentation buffer is VM's to lay out, not shr's
	(shr-use-fonts nil))
    (delete-region start (1- end))
    (goto-char start)
    (shr-insert-document document)))

(defun vm-mime-display-internal-text/html (layout)
  "Dispatch handling of html to the actual html handler."
  ;; If the user has set the vm-mime-text/html-handler _variable_ to
  ;; 'auto-select, and it is left set that way in this function, we will get a
  ;; failure because there is no function called
  ;; "vm-mime-display-internal-auto-select-text/html". But, the
  ;; vm-mime-text/html-handler _function_ sets the corresponding _variable_
  ;; based upon a heuristic about available packages, so call it for its
  ;; side-effect now.  -- Brent Goodrick, 2008-12-08
  (vm-mime-text/html-handler)
  (if vm-mime-text/html-handler
      (condition-case error-data
	  (let ((buffer-read-only nil)
		(start (point))
		(charset (or (vm-mime-get-parameter layout "charset")
			     "us-ascii"))
		end) ;; buffer-size
	    (vm-emit-mime-decoding-message
	     "Inlining text/html by %s..." vm-mime-text/html-handler)
	    (vm-mime-insert-mime-body layout)
	    (unless (bolp) (insert "\n"))
	    (setq end (point-marker))
	    (vm-mime-transfer-decode-region layout start end)
	    (vm-mime-charset-decode-region charset start end)
	    ;; Nothing blocks a remote image here.  A loop stood here that
	    ;; searched for `vm-mime-text/html-blocker' and then tested
	    ;; (or t ...), so it always took the branch holding a TODO and
	    ;; the `blocked:' it meant to insert was unreachable
	    ;; (emacs-vm/vm#845).  What does the blocking is
	    ;; `vm-w3m-safe-url-regexp', which vm-w3m.el binds emacs-w3m's
	    ;; own `w3m-safe-url-regexp' to while it renders; the w3m and
	    ;; lynx handlers convert outside Emacs and fetch nothing.
	    ;; A renderer that replaces the region deletes all of the
	    ;; text first, which makes end == start.  The fix is to move
	    ;; the end marker forward with a placeholder character so
	    ;; that end stays ahead of the insertion point and is moved
	    ;; forward when the new text is inserted.  We'll delete the
	    ;; placeholder afterward.
	    (goto-char end)
	    (insert-before-markers "z")
	    ;; the view port (scrollbar) is sometimes messed up, try to avoid it
	    (unwind-protect
		(save-window-excursion
		  ;; dispatch to actual handler
		  (funcall (intern 
			    (format "vm-mime-display-internal-%s-text/html"
				    vm-mime-text/html-handler))
			   start end layout))
	      ;; do clean up
	      (goto-char end)
	      (delete-char -1))
	    (vm-emit-mime-decoding-message
	     "Inlining text/html by %s... done." vm-mime-text/html-handler)
	    t)
	(error (vm-set-mm-layout-display-error
		layout
		(format "Inline text/html by %s display failed: %s"
			vm-mime-text/html-handler
			(error-message-string error-data)))
	       (vm-warn 0 2 "%s: %s" 
			(buffer-name vm-mail-buffer)
			(vm-mm-layout-display-error layout))
	       nil))
    ;; no handler
    (vm-warn 0 2 "%s: No handler available for internal display of text/html"
	     (buffer-name vm-mail-buffer))
    nil))
  

;;; RFC 3676 -- format=flowed
;;
;; A sender who does not know how wide the reader's window is can say so: it
;; wraps the text at some width of its own and marks every break it invented
;; by leaving a space at the end of the line.  The reader is then free to join
;; those lines back up and re-wrap them.  A break with no space before it is
;; the author's own and stays put.
;;
;;     Content-Type: text/plain; format=flowed
;;
;; Two wrinkles.  A line whose first character is a space, or that would
;; otherwise look like a quote or a From_ line, is sent with an extra space in
;; front of it -- "space-stuffing" -- which has to come off before anything
;; else is looked at.  And delsp=yes says the space at a soft break is part of
;; the marking rather than part of the text, so it comes off when the lines are
;; joined; that is how a language that does not put spaces between words uses
;; the format.

(defun vm-mime-flowed-layout-p (layout)
  "Return non-nil if LAYOUT is plain text sent as RFC 3676 format=flowed."
  (and vm-mime-unflow-flowed-text
       (vm-mime-types-match "text/plain" (car (vm-mm-layout-type layout)))
       (let ((format (vm-mime-get-parameter layout "format")))
	 (and format
	      (equal "flowed"
		     (downcase (vm-mime-unquote-parameter-value format)))))))

(defun vm-mime-delsp-layout-p (layout)
  "Return non-nil if LAYOUT carries the RFC 3676 delsp=yes parameter."
  (let ((delsp (vm-mime-get-parameter layout "delsp")))
    (and delsp
	 (equal "yes" (downcase (vm-mime-unquote-parameter-value delsp))))))

(defun vm-mime-flowed-quote-depth ()
  "Return the number of quote characters at point, leaving point after them."
  (skip-chars-forward ">"))

(defun vm-mime-unflow-region (start end &optional delsp)
  "Join the soft line breaks of RFC 3676 format=flowed between START and END.
A line that ends in a space is joined to the one after it, provided that line
is quoted to the same depth: quoting is part of the paragraph's identity, so
text quoted twice is never joined to text quoted once.  Space-stuffing is
undone first, as RFC 3676 section 4.4 requires.  With DELSP the space at the
break is dropped rather than kept.

The signature separator is not joined to, or joined from.  Strictly it is a
flowed line, ending as it does in a space, but RFC 3676 section 4.3 asks that
it be left as a line of its own; running it together with the text above or
the signature below would stop anything recognising either.

On a quoted line the space that follows the quote characters is put back after
the stuffing is removed, so that quoted text still looks quoted.  A sender
space-stuffs a quoted line precisely because the quote prefix is followed by
one, and every reader displays it that way."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let (line-start depth stuffed joining)
	(while (not (eobp))
	  (setq line-start (point)
		depth (vm-mime-flowed-quote-depth)
		stuffed nil)
	  ;; Un-stuff before deciding anything else about the line.
	  (when (eq (char-after) ?\s)
	    (delete-char 1)
	    (setq stuffed t))
	  (when (and stuffed (> depth 0))
	    (insert " "))
	  (setq joining t)
	  (while joining
	    (end-of-line)
	    (if (and (eq (char-before) ?\s)
		     (not (eobp))
		     (not (vm-mime-flowed-signature-line-p line-start (point)))
		     ;; The next line has to be quoted to the same depth, and
		     ;; must not be the signature separator.
		     (save-excursion
		       (forward-char 1)
		       (and (= depth (vm-mime-flowed-quote-depth))
			    (not (looking-at " ?-- $")))))
		(progn
		  (when delsp (delete-char -1))
		  (delete-char 1)		; the line break itself
		  (delete-char depth)		; the next line's quoting
		  ;; and its stuffing.  No quote prefix goes back in: the
		  ;; joined text now follows the prefix of the line it was
		  ;; appended to.
		  (when (eq (char-after) ?\s)
		    (delete-char 1)))
	      (setq joining nil)))
	  (unless (eobp) (forward-line 1)))))))

(defun vm-mime-flowed-signature-line-p (start end)
  "Return non-nil if the text between START and END is the signature separator.
That is the line \"-- \" of RFC 3676 section 4.3, quoted or not."
  (string-match-p "\\`>* ?-- \\'"
		  (buffer-substring-no-properties start end)))

(defun vm-mime-flowed-soft-break-width ()
  "Return the width at which a line break is taken to be VM's rather than yours.
A line filled out to about the fill column was broken there because that is
where the text ran out of room; a much shorter line was broken there because
you meant it to be -- an address, a list item, a line of code.  Only the first
kind is offered to the reader to undo.  This is a guess, but the alternative is
to flow everything and reflow the reader's view of text that was deliberately
laid out."
  (max 20 (- (or fill-column 70) 10)))

(defun vm-mime-flow-region (start end)
  "Mark the line breaks between START and END as soft, per RFC 3676.
A line that runs to about the fill column and is followed by more of the same
paragraph is given a trailing space, which tells the reader the break after it
was made to fit a width and may be undone.  A paragraph ends at a blank line,
at a change of quote depth, or at the signature separator, and its last line
keeps its break.  So does a line short enough to have been broken on purpose;
see `vm-mime-flowed-soft-break-width'.

Space-stuffing is applied as section 4.4 requires: a line whose text begins
with a space, or -- when the line is not quoted -- with a quote character or
with \"From \", is sent with one extra space in front of that text, so that the
reader can tell it from the format's own marks.  The quote characters of a
quoted line are its quote prefix and are left alone; stuffing goes after them.

Returns non-nil if any break was marked soft, which is the caller's cue to
declare format=flowed.  Text of one-line paragraphs comes back unchanged apart
from stuffing and does not need the parameter."
  (let ((flowed nil)
	(width (vm-mime-flowed-soft-break-width)))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	;; Stuffing first: it shifts the text, and the width measured below
	;; should be the width the reader will see.
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((depth (vm-mime-flowed-quote-depth)))
	    ;; Point is now after the quote prefix, at the text itself.
	    (when (if (> depth 0)
		      ;; The space a mailer conventionally puts after the quote
		      ;; characters is itself the stuffing -- the reader takes
		      ;; one space off and puts one back to display the line.
		      ;; Adding another would send "> " as ">  ".  A quoted
		      ;; line with no space there gets one, so that it reads
		      ;; the usual way at the other end.
		      (not (eq (char-after) ?\s))
		    (looking-at "[ >]\\|From "))
	      (insert " ")))
	  (forward-line 1))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((bol (point))
		(depth (save-excursion (vm-mime-flowed-quote-depth)))
		eol)
	    (end-of-line)
	    (setq eol (point))
	    (unless (or (= bol eol)		; a blank line ends a paragraph
			(vm-mime-flowed-signature-line-p bol eol)
			(eobp)			; the last line keeps its break
			(< (- eol bol) width)	; a break you meant
			;; A paragraph also ends where the next line is blank,
			;; quoted differently, or the signature separator.
			(save-excursion
			  (forward-char 1)
			  (or (eobp)
			      (looking-at "$")
			      (/= depth (save-excursion
					  (vm-mime-flowed-quote-depth)))
			      (looking-at " ?-- $"))))
	      ;; A soft break: leave exactly one space before it.
	      (unless (eq (char-before) ?\s)
		(insert " "))
	      (setq flowed t)))
	  (forward-line 1))))
    flowed))

(defun vm-mime-display-internal-text/plain (layout &optional no-highlighting)
  "Display a text/plain mime part given by LAYOUT, carrying out
any necessary MIME-decoding, CRLF-conversion, charset-conversion
and word-wrapping/filling.  The original text is replaced by the
converted content.  Unless NO-HIGHLIGHTING is non-nil, the URL's
in the text are highlighted and energized."
  (let ((start (point)) end need-conversion
	(buffer-read-only nil)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (if (and (not (vm-mime-charset-internally-displayable-p charset))
	     (not (setq need-conversion (vm-mime-can-convert-charset charset))))
	(progn
	  (vm-set-mm-layout-display-error
	   layout (concat "Undisplayable charset: " charset))
	  (vm-warn 0 2 "%s: %s" (buffer-name vm-mail-buffer) 
		   (vm-mm-layout-display-error layout))
	  nil)
      (vm-mime-insert-mime-body layout)
      (unless (bolp) (insert "\n"))
      (setq end (point-marker))
      (vm-mime-transfer-decode-region layout start end)
      (when need-conversion
	(setq charset (vm-mime-charset-convert-region charset start end)))
      (vm-mime-charset-decode-region charset start end)
      ;; Before anything looks at the line structure: the sender's line breaks
      ;; are not all real.  What is left is one long line per paragraph, which
      ;; the filling below then wraps to this window -- which is the point of
      ;; the format.
      (when (vm-mime-flowed-layout-p layout)
	(vm-mime-unflow-region start end (vm-mime-delsp-layout-p layout)))
      (unless no-highlighting (vm-energize-urls-in-message-region start end))
      (when (and (or vm-word-wrap-paragraphs
		     vm-fill-paragraphs-containing-long-lines)
		 (not no-highlighting))
	(vm-fill-paragraphs-containing-long-lines
	 vm-fill-paragraphs-containing-long-lines start end))
      (goto-char end)
      t )))

(defun vm-mime-display-internal-text/enriched (layout)
  (require 'enriched)
  (defvar enriched-verbose)
  (let ((start (point)) end
	(buffer-read-only nil)
	(enriched-verbose t)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (vm-emit-mime-decoding-message "Decoding text/enriched...")
    (vm-mime-insert-mime-body layout)
    (unless (bolp) (insert "\n"))
    (setq end (point-marker))
    (vm-mime-transfer-decode-region layout start end)
    (vm-mime-charset-decode-region charset start end)
    ;; enriched-decode expects a couple of headers at the top of
    ;; the region and will remove anything that looks like a
    ;; header.  Put a header section here for it to eat so it
    ;; won't eat message text instead.
    (goto-char start)
    (insert "Comment: You should not see this header\n\n")
    (condition-case errdata
	(enriched-decode start end)
      (error (vm-set-mm-layout-display-error
	      layout (format "enriched-decode signaled %s" errdata))
	     (vm-warn 0 2 "%s: %s" (buffer-name vm-mail-buffer)
		      (vm-mm-layout-display-error layout))
	     nil ))
    (vm-energize-urls-in-message-region start end)
    (goto-char end)
    (vm-emit-mime-decoding-message "Decoding text/enriched... done")
    t ))

(defun vm-mime-cid-file-name (id)
  "Return a file name component naming the cid: reference ID.
A Content-ID may hold anything an addr-spec may, `@' and `%' included, so it is
not a file name as it stands."
  (let ((name (copy-sequence id)))
    (while (string-match "[^A-Za-z0-9._-]" name)
      (setq name (replace-match "_" t t name)))
    name))

(defun vm-mime-write-cid-part (part id html-file)
  "Write PART, the target of cid: reference ID, beside HTML-FILE.
Returns the file written, or nil.  It goes in the same directory so that the
rewritten reference can be a bare file name, which is what a browser resolves
relative to the document it is reading.

Written with the same care `vm-make-tempfile' takes over the HTML part itself:
mode 600, because this is somebody's mail going into a directory other people
may be able to read, and any existing file removed first, so that a name
already occupying the path -- a symbolic link, say -- is not written through."
  (let* ((suffix (or (vm-mime-extract-filename-suffix part)
		     (vm-mime-find-filename-suffix-for-type part)
		     ""))
	 (file (expand-file-name
		(concat (file-name-base html-file) "-"
			(vm-mime-cid-file-name id) suffix)
		(file-name-directory html-file)))
	 (modes (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes (vm-octal 600))
	  (vm-error-free-call 'delete-file file)
	  (and (vm-mime-send-body-to-file part nil file t)
	       file))
      (set-default-file-modes modes))))

(defun vm-mime-html-fragment-p ()
  "Whether the HTML in the current buffer is a fragment rather than a document.
A document says so with a doctype or an `<html>' tag, or at least says what
character set it is in; a fragment says neither, and leaves a browser to
guess both.

Note that `>' is a symbol constituent in the standard syntax table, so
\"<html\\\\_>\" does not match `<html>'."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (not (or (re-search-forward "<html[ \t\r\n>]\\|<!doctype[ \t]" nil t)
	     (progn (goto-char (point-min))
		    (re-search-forward "<meta\\s-[^>]*charset" nil t))))))

(defun vm-mime-complete-html-file (layout html-file)
  "Make HTML-FILE a whole document if the text/html in it is a fragment.
HTML-FILE holds the text of LAYOUT, written out for an external viewer.  A
fragment carries no charset of its own -- that was in the part's header, and
the file has no header -- so the viewer guesses.  Issue #387.

The text is wrapped, not re-encoded: the bytes VM wrote are the bytes the
part had.  Returns t when the file was changed.

Does nothing when `vm-mime-complete-html-for-external-viewer' is nil."
  (when vm-mime-complete-html-for-external-viewer
    (let ((charset (or (vm-mime-get-parameter layout "charset") "us-ascii"))
	  (coding-system-for-read (vm-binary-coding-system))
	  (coding-system-for-write (vm-binary-coding-system)))
      (with-temp-buffer
	(insert-file-contents html-file)
	(when (vm-mime-html-fragment-p)
	  (goto-char (point-min))
	  (insert (format (concat "<html>\n<head>\n"
				  "<meta http-equiv=\"Content-Type\""
				  " content=\"text/html; charset=%s\">\n"
				  "</head>\n<body>\n")
			  charset))
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert "</body>\n</html>\n")
	  (write-region (point-min) (point-max) html-file nil 'quiet)
	  t)))))

(defun vm-mime-externalize-cid-references (layout html-file)
  "Point HTML-FILE's cid: references at local copies of the parts they name.
HTML-FILE holds the text of LAYOUT, a text/html part written out for an
external viewer.  A `cid:' URL names another part of the same message
(RFC 2392), which a browser handed a lone HTML file has no way to reach --
so it draws a broken image where the sender put a picture.  Issue #506.

Each referenced part is written beside HTML-FILE and the reference is
replaced by its file name.  Returns the list of files written, for the
caller to register as garbage.

Does nothing when `vm-mime-externalize-cid-references' is nil."
  (let ((message (and vm-mime-externalize-cid-references
		      (vm-mm-layout-message layout)))
	(written nil))
    (when message
      (let ((top (vm-mm-layout (vm-real-message-of message))))
	(when (vectorp top)
	  (with-temp-buffer
	    (let ((coding-system-for-read (vm-binary-coding-system)))
	      (insert-file-contents html-file))
	    (let ((found (make-hash-table :test 'equal))
		  (changed nil))
	      (goto-char (point-min))
	      ;; A cid: URL ends where the attribute or the CSS url() does.
	      (while (re-search-forward "cid:\\([^\"'>) \t\r\n]+\\)" nil t)
		(let* ((id (match-string 1))
		       (file (gethash id found)))
		  (unless file
		    (let ((part (vm-mime-find-leaf-content-id
				 top (concat "<" id ">"))))
		      (when part
			(setq file (vm-mime-write-cid-part part id html-file))
			(when file
			  (puthash id file found)
			  (push file written)))))
		  (when file
		    (replace-match (file-name-nondirectory file) t t)
		    (setq changed t))))
	      (when changed
		(let ((coding-system-for-write (vm-binary-coding-system)))
		  (write-region (point-min) (point-max) html-file nil 'quiet))))))))
    (nreverse written)))

(defun vm-mime-display-external-generic (layout)
  "Display mime object with LAYOUT in an external viewer, as
determined by `vm-mime-external-content-types-alist'."
  ;;  Optional argument FILE indicates that the content should be
  ;;  taken from it.
  (let ((program-list (copy-sequence
		       (vm-mime-find-external-viewer
			(car (vm-mm-layout-type layout)))))
	(buffer-read-only nil)
	;; start
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(append-file t)
	process	tempfile cache suffix basename) ;; end
    (setq cache (get (vm-mm-layout-cache layout)
		     'vm-mime-display-external-generic)
	  process (nth 0 cache)
	  tempfile (nth 1 cache))
    (if (and (processp process) (eq (process-status process) 'run))
	t
      (cond ((or (null tempfile) (null (file-exists-p tempfile)))
	     (setq suffix (vm-mime-extract-filename-suffix layout)
		   suffix (or suffix
			      (vm-mime-find-filename-suffix-for-type layout)))
	     (setq basename (vm-mime-get-disposition-filename layout))
	     (setq tempfile (vm-make-tempfile suffix basename))
             (vm-register-message-garbage-files (list tempfile))
             (vm-mime-send-body-to-file layout nil tempfile t)
	     ;; An external viewer given only this file cannot follow a cid:
	     ;; reference to another part of the message, so give it copies to
	     ;; look at instead of broken images (issue #506).  Nor does the
	     ;; file say what character set it is in, if the part was a
	     ;; fragment rather than a document (issue #387).
	     (when (vm-mime-types-match "text/html"
					(car (vm-mm-layout-type layout)))
	       (vm-mime-complete-html-file layout tempfile)
	       (vm-register-message-garbage-files
		(vm-mime-externalize-cid-references layout tempfile)))))

      (if (symbolp (car program-list))
	  ;; use internal function if provided
	  (apply (car program-list)
		 (append (cdr program-list) (list tempfile)))

	;; quote file name for shell command only
	(or (cdr program-list)
	    (setq tempfile (shell-quote-argument tempfile)))
      
	;; expand % specs
	(let ((p program-list)
	      (vm-mf-attachment-file tempfile))
	  (while p
	    (if (string-match "\\([^%]\\|^\\)%f" (car p))
		(setq append-file nil))
	    (setcar p (vm-mime-sprintf (car p) layout))
	    (setq p (cdr p))))

	(vm-inform 6 "Launching %s..." (mapconcat 'identity program-list " "))
	(setq process
	      (if (cdr program-list)
		  (apply 'start-process
			 (format "view %25s"
				 (vm-mime-sprintf
				  (vm-mime-find-format-for-layout layout)
				  layout))
			 nil (if append-file
				 (append program-list (list tempfile))
			       program-list))
		(apply 'start-process
		       (format "view %25s"
			       (vm-mime-sprintf
				(vm-mime-find-format-for-layout layout)
				layout))
		       nil
		       (or shell-file-name "sh")
		       shell-command-switch
		       (if append-file
			   (list (concat (car program-list) " " tempfile))
			 program-list))))
	(vm-process-kill-without-query process t)
	(vm-inform 6 "Launching %s... done" (mapconcat 'identity
						   program-list
						   " "))
	(if vm-mime-delete-viewer-processes
	    (vm-register-message-garbage 'delete-process process))
	(put (vm-mm-layout-cache layout)
	     'vm-mime-display-external-generic
	     (list process tempfile)))))
  t )

(defun vm-mime-display-internal-application/octet-stream (layout)
  "Display a button for the MIME LAYOUT.  If a button extent is
given as the argument instead, then nothing is done.   USR, 2011-03-25"
  (if (vectorp layout)
      (let ((buffer-read-only nil)
	    (vm-mf-default-action "save"))
	(vm-mime-insert-button
	 :caption
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 :action
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-save-application/octet-stream layout))))
	 :layout layout)))
  t)

(defun vm-mime-save-application/octet-stream (layout)
  "Save an application/octet-stream object with LAYOUT to the
stated filename.  A button extent with a layout can also be given as
the argument.                                        USR, 2011-03-25"
  (unless (vectorp layout)
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  ;; support old "name" paramater for application/octet-stream
  ;; but don't override the "filename" parameter extracted from
  ;; Content-Disposition, if any.
  (let ((default-filename (vm-mime-get-disposition-filename layout))
	(file nil))
    (setq file (vm-mime-send-body-to-file layout default-filename))
    (when (and file vm-mime-delete-after-saving)
      (let ((vm-mime-confirm-delete nil))
	;; we don't care if the delete fails
	(condition-case nil
	    (vm-delete-mime-object (expand-file-name file))
	  (error nil)))))
  t )
(fset 'vm-mime-display-button-application/octet-stream
      'vm-mime-display-internal-application/octet-stream)

(defun vm-mime-display-button-application (layout)
  "Display button for an application type object described by LAYOUT."
  (vm-mime-display-button-xxxx layout nil))


(defun vm-mime-display-button-audio (layout)
  (vm-mime-display-button-xxxx layout nil))

(defun vm-mime-display-button-video (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-message (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-multipart (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-multipart/mixed (layout)
  (let ((part-list (vm-mm-layout-parts layout)))
    (while part-list
      (let ((part (car part-list)))
        (vm-decode-mime-layout part)
        (setq part-list (cdr part-list))
	;; we always put separator because it is cleaner, and buttons
	;; may get expanded to documents in any case. USR, 2011-02-09
	(when part-list
	  (insert vm-mime-parts-display-separator))))
    t))


(defun vm-mime-display-internal-multipart/alternative (layout)
  (if (eq vm-mime-alternative-show-method 'all)
      (vm-mime-display-internal-multipart/mixed layout)
    (vm-mime-display-internal-show-multipart/alternative layout)))

(defun vm-mime-display-internal-show-multipart/alternative (layout)
  (let (best-layout)
    (cond ((eq vm-mime-alternative-show-method 'best)
	   (let ((done nil)
		 (best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (if (or (vm-mime-can-display-internal (car part-list) t)
		       (vm-mime-find-external-viewer type))
		   (setq best (car part-list)
			 done t)
		 (setq part-list (cdr part-list))))
	     (setq best-layout (or best (car (vm-mm-layout-parts layout))))))
	  ((eq vm-mime-alternative-show-method 'best-internal)
	   (let ((done nil)
		 (best nil)
		 (second-best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (cond ((and (vm-mime-can-display-internal (car part-list) t)
			   (vm-mime-internally-displayable (car part-list)))
		      (setq best (car part-list)
			    done t))
		     ((and (null second-best)
			   (vm-mime-find-external-viewer type))
		      (setq second-best (car part-list))))
	       (setq part-list (cdr part-list)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-show-method)
		(eq (car vm-mime-alternative-show-method)
		    'favorite-internal))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-show-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((or (vm-mime-can-display-internal (car part-list) t)
			    (vm-mime-find-external-viewer type))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-show-method)
		(eq (car vm-mime-alternative-show-method) 'favorite))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-show-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((and (vm-mime-can-display-internal (car part-list) t)
			     (vm-mime-internally-displayable (car part-list)))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout)))))))
    (when best-layout 
      (vm-decode-mime-layout best-layout))))

(defun vm-mime-display-internal-multipart/related (layout)
  "Decode multipart/related body parts.
This function decodes the ``start'' part only (see RFC2387).  The
other parts will be decoded by the other VM functions through
emacs-w3m."
  (if (eq vm-mime-multipart/related-show-method 'mixed)
      (vm-mime-display-internal-multipart/mixed layout)
    (let* ((part-list (vm-mm-layout-parts layout))
	   (start-part (car part-list))
	   (start-id (vm-mime-get-parameter layout "start"))
	   part
	   (vm-mime-cid-retrieved nil) ; override
	   )
      ;; Look for the start part.
      (if start-id
	  (while part-list
	    (setq part (car part-list))
	    (if (equal start-id (vm-mm-layout-id part))
		(setq start-part part
		      part-list nil)
	      (setq part-list (cdr part-list)))))
      (if start-part (vm-decode-mime-layout start-part))
      ;; if no related parts were fetched, display them now
      (unless (and start-part vm-mime-cid-retrieved)
	(let ((part-list (vm-mm-layout-parts layout)))
	  (while part-list
	    (let ((part (car part-list)))
	      (unless (eq part start-part)
		(vm-decode-mime-layout part))
	      (setq part-list (cdr part-list))
	      ;; we always put separator because it is cleaner, and buttons
	      ;; may get expanded to documents in any case. USR, 2011-02-09
	      (when part-list
		(insert vm-mime-parts-display-separator))))))
      t)))

(defun vm-mime-display-button-multipart/parallel (layout)
  (vm-mime-insert-button
   :caption
   (concat
    ;; display the file name or disposition
    (let ((file (vm-mime-get-disposition-filename layout)))
      (if file (format " %s " file) ""))
    (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout) )
   :action
   (function
    (lambda (layout)
      (save-excursion
	(let ((vm-mime-auto-displayed-content-types t)
	      (vm-mime-auto-displayed-content-type-exceptions nil))
	  (vm-decode-mime-layout layout t)))))
   :layout layout 
   :disposable t))

(fset 'vm-mime-display-internal-multipart/parallel
      'vm-mime-display-internal-multipart/mixed)

(defun vm-mime-display-internal-multipart/digest (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 :caption
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 :action
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-multipart/digest layout))))
	 :layout layout))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (generate-new-buffer (format "digest from %s/%s"
					     (buffer-name vm-mail-buffer)
					     (vm-number-of
					      (car vm-message-pointer)))))
    (setq vm-folder-type vm-default-folder-type)
    (let ((ident-header nil))
      (if vm-digest-identifier-header-format
	  (setq ident-header (vm-summary-sprintf
			      vm-digest-identifier-header-format
			      (vm-mm-layout-message layout))))
      (vm-mime-burst-layout layout ident-header))
    (save-current-buffer
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )

(fset 'vm-mime-display-button-multipart/digest
      'vm-mime-display-internal-multipart/digest)

(defun vm-mime-display-button-message/rfc822 (layout)
  (let ((buffer-read-only nil))
    (vm-mime-insert-button
     :caption
     (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
     :action
     (function
      (lambda (layout)
	(save-excursion
	  (vm-mime-display-internal-message/rfc822 layout))))
     :layout layout)))

(fset 'vm-mime-display-button-message/news
      'vm-mime-display-button-message/rfc822)

(defun vm-mime-display-internal-message/rfc822 (layout)
  (if (vectorp layout)
      (let ((start (point))
	    (buffer-read-only nil))
	(vm-mime-insert-mime-headers (car (vm-mm-layout-parts layout)))
	(insert ?\n)
	(save-excursion
	  (goto-char start)
	  (vm-reorder-message-headers
	   nil :keep-list vm-visible-headers
	   :discard-regexp vm-invisible-header-regexp))
	(save-restriction
	  (narrow-to-region start (point))
	  (vm-decode-mime-encoded-words))
	(vm-mime-display-internal-multipart/mixed layout))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (vm-generate-new-unibyte-buffer
		 (format "message from %s/%s"
			 (buffer-name vm-mail-buffer)
			 (vm-number-of
			  (car vm-message-pointer)))))
    (setq vm-folder-type vm-default-folder-type)
    (vm-mime-burst-layout layout nil)
    (set-buffer-modified-p nil)
    (save-current-buffer
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )
(fset 'vm-mime-display-internal-message/news
      'vm-mime-display-internal-message/rfc822)

(defun vm-mime-display-internal-message/delivery-status (layout)
  (vm-mime-display-internal-text/plain layout t))

(defun vm-mime-retrieve-external-body (layout)
  "Retrieve a message/external-body object described by LAYOUT into the
current buffer."
  (let ((access-method (downcase (vm-mime-get-parameter layout "access-type")))
	(work-buffer (current-buffer)))
    (cond ((string= access-method "local-file")
	   (let ((name (vm-mime-get-parameter layout "name")))
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (not (file-exists-p name))
		 (vm-mime-error "file %s does not exist" name))
	     (condition-case data
		 (insert-file-contents-literally name)
	       (error (signal 'vm-mime-error (cdr data))))))
	  ((and (string= access-method "url")
		vm-url-retrieval-methods)
	   (let ((url (vm-mime-get-parameter layout "url")))
	     (if (null url)
		 (vm-mime-error
		  "%s access type missing `url' parameter"
		  access-method))
	     (setq url (vm-with-string-as-temp-buffer
			url
			(function
			 (lambda ()
			   (goto-char (point-min))
			   (while (re-search-forward "[ \t\n]" nil t)
			     (delete-char -1))))))
	     (vm-mime-fetch-url url work-buffer)))
	  ((and (or (string= access-method "ftp")
		    (string= access-method "anon-ftp"))
		(fboundp 'ange-ftp-hook-function))
	   (let ((name (vm-mime-get-parameter layout "name"))
		 (directory (vm-mime-get-parameter layout "directory"))
		 (site (vm-mime-get-parameter layout "site"))
		 user)
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (null site)
		 (vm-mime-error
		  "%s access type missing `site' parameter"
		  access-method))
	     (cond ((string= access-method "ftp")
		    (setq user (read-string
				(format "User name to access %s: "
					site)
				(user-login-name))))
		   (t (setq user "anonymous")))
	     (if (and (string= access-method "ftp")
		      vm-url-retrieval-methods
		      (vm-mime-fetch-url
		       (if directory
			   (concat "ftp:////" site "/"
				   directory "/" name)
			 (concat "ftp:////" site "/" name))
		       work-buffer))
		 t
	       (cond (directory
		      (setq directory
			    (concat "/" user "@" site ":" directory))
		      (setq name (expand-file-name name directory)))
		     (t
		      (setq name (concat "/" user "@" site ":"
					 name))))
	       (condition-case data
		     (insert-file-contents-literally name)
		 (error (signal 'vm-mime-error
				(format "%s" (cdr data)))))))))))

(defun vm-mime-fetch-message/external-body (layout)
  "Fetch the external-body content described by LAYOUT and store
it in an internal buffer.  Update the LAYOUT so that it refers to the
fetched content."
  (let ((child-layout (car (vm-mm-layout-parts layout)))
	(access-method (downcase (vm-mime-get-parameter layout "access-type")))
	ob
	(work-buffer nil))
    (unwind-protect
	(cond
	 ((and (string= access-method "mail-server")
	       (vm-mm-layout-id child-layout)
	       (setq ob (vm-mime-find-leaf-content-id-in-layout-folder
			 layout (vm-mm-layout-id child-layout))))
	  (setq child-layout ob))
	 ((eq (marker-buffer (vm-mm-layout-header-start child-layout))
	      (marker-buffer (vm-mm-layout-body-start child-layout)))
	  ;; if the "body" is in the same buffer, that means that the
	  ;; external-body has not been retrieved yet
	  (setq work-buffer
		(vm-make-multibyte-work-buffer
		 (format "*%s mime object*"
			 (car (vm-mm-layout-type child-layout)))))
	  (condition-case data
	      (with-current-buffer work-buffer
		(if (fboundp 'set-buffer-file-coding-system)
		    (set-buffer-file-coding-system
		     (vm-binary-coding-system) t))
		(cond
		 ((or (string= access-method "ftp")
		      (string= access-method "anon-ftp")
		      (string= access-method "local-file")
		      (string= access-method "url"))
		  (vm-mime-retrieve-external-body layout))
		 ((string= access-method "mail-server")
		  (let ((server (vm-mime-get-parameter layout "server"))
			(subject (vm-mime-get-parameter layout "subject")))
		    (if (null server)
			(vm-mime-error
			 "%s access type missing `server' parameter"
			 access-method))
		    (if (not
			 (y-or-n-p
			  (format
			   "Send message to %s to retrieve external body? "
			   server)))
			(error "Aborted"))
		    (vm-mail-internal
		     :buffer-name (format "mail to MIME mail server %s" server)
		     :to server :subject subject)
		    (mail-text)
		    (vm-mime-insert-mime-body child-layout)
		    (let ((vm-confirm-mail-send nil))
		      (vm-mail-send))
		    (vm-warn 0 2
			     (concat "Retrieval message sent.  "
				     "Retry viewing this object after "
				     "the response arrives."))))
		 (t
		  (vm-mime-error "unsupported access method: %s"
				 access-method))
		 )
		(when child-layout
		  (vm-set-mm-layout-body-end 
		   child-layout (vm-marker (point-max)))
		  (vm-set-mm-layout-body-start 
		   child-layout (vm-marker (point-min)))))
	    (vm-mime-error		; handler
	     (vm-set-mm-layout-display-error layout (cdr data))
	     (setq child-layout nil)))))
      ;; unwind-protections
      (when work-buffer
	(if child-layout		; refers to work-buffer
	    (vm-register-folder-garbage 'kill-buffer work-buffer)
	  (kill-buffer work-buffer))))))

(defun vm-mime-display-external-message/external-body (layout)
  "Display the external-body content described by LAYOUT."
  (vm-mime-fetch-message/external-body layout)
  (let ((child-layout (car (vm-mm-layout-parts layout))))
    (when child-layout 
      (vm-mime-display-external-generic child-layout))))

(defun vm-mime-display-internal-message/external-body (layout
						       &optional extent)
  "Display the external-body content described by LAYOUT.  The
optional argument EXTENT, if present, gives the extent of the MIME
button that this LAYOUT comes from."
  (vm-mime-fetch-message/external-body layout)
  (let ((child-layout (car (vm-mm-layout-parts layout))))
    (when child-layout 
      (vm-decode-mime-layout (or extent child-layout)))))

(defun vm-mime-display-button-message/external-body (layout)
  "Return a button usable for viewing message/external-body MIME parts."
  (let ((buffer-read-only nil)
	(tmplayout (copy-tree (car (vm-mm-layout-parts layout)) t))
	(filename "external: ")
	format)
    (when (vm-mime-get-parameter layout "name")
      (setq filename 
	    (concat filename
		    (file-name-nondirectory 
		     (vm-mime-get-parameter layout "name")))))
    (vm-mime-set-parameter tmplayout "name" filename)
    (vm-mime-set-xxx-parameter "filename" filename 
			       (vm-mm-layout-disposition tmplayout))
    (setq format (vm-mime-find-format-for-layout tmplayout))
    (vm-mime-insert-button
     :caption
     (vm-replace-in-string
      (vm-replace-in-string
       (vm-mime-sprintf format tmplayout) "save\\]" "fetch]")
      "display\\]" "fetch]")
     :action
     (function
      (lambda (extent)
	;; reuse the internal display code, but make sure that no new
	;; buttons will be created for the external-body content.
	(let ((layout (vm-extent-property extent 'vm-mime-layout))
	      (vm-mime-auto-displayed-content-types t)
	      (vm-mime-auto-displayed-content-type-exceptions nil))
	  (vm-mime-display-internal-message/external-body 
	   layout extent))))
     ;; the button should be disposable so that it can be replaced by
     ;; the external body (see Launchpad bug #941561)
     :disposable t
     :layout layout)))


(defun vm-mime-url-response-body-start ()
  "Where the body starts in a response `url-retrieve-synchronously' returned.
`url-http' records it in `url-http-end-of-headers'.  Other schemes do not
bind that and still synthesise a header block: a file: URL comes back with
Content-type and Content-length in front of the file.  So the fallback is
the first blank line, and a response with no header block is all body."
  (cond ((and (boundp 'url-http-end-of-headers)
	      (symbol-value 'url-http-end-of-headers))
	 (symbol-value 'url-http-end-of-headers))
	(t
	 (save-excursion
	   (goto-char (point-min))
	   (if (re-search-forward "^\r?\n" nil t)
	       (point)
	     (point-min))))))

(defun vm-mime-fetch-url (url buffer)
  "Retrieve URL into BUFFER.  Return non-nil when anything was retrieved.

Emacs does the retrieving.  `url-retrieve-synchronously' is in core and
speaks http, https, ftp and file, so this needs no external program and no
option saying which one to run.  The body is copied buffer to buffer rather
than through a string, so that a binary object keeps its bytes."
  (let ((response
	 (condition-case err
	     (url-retrieve-synchronously url t t vm-url-retrieval-timeout)
	   (error
	    (vm-warn 0 2 "Could not retrieve %s: %s"
		     url (error-message-string err))
	    nil))))
    (when response
      (unwind-protect
	  (let ((start (with-current-buffer response
			 (vm-mime-url-response-body-start))))
	    (with-current-buffer buffer
	      (erase-buffer)
	      (insert-buffer-substring response start)
	      (not (zerop (buffer-size)))))
	(kill-buffer response)))))

(defun vm-mime-internalize-local-external-bodies (layout)
  "Given a LAYOUT representing a message/external-body object, convert
it to an internal object by retrieving the body.       USR, 2011-03-28"
  (cond ((vm-mime-types-match "message/external-body"
			      (car (vm-mm-layout-type layout)))
	 (when (string= (downcase
			 (vm-mime-get-parameter layout "access-type"))
			"local-file")
	   (let* ((child-layout 
		   (car (vm-mm-layout-parts layout)))
		  (work-buffer 
		   (vm-make-multibyte-work-buffer
		    (format "*%s mime object*"
			    (car (vm-mm-layout-type child-layout))))))
	     (let () ;;
	       (with-current-buffer work-buffer
		 (vm-mime-retrieve-external-body layout))
	       (goto-char (vm-mm-layout-body-start child-layout))
	       (condition-case data
		   (insert-buffer-substring work-buffer)
		 (error (signal 'vm-mime-error (cdr data))))
	       ;; This is redundant because insertion moves point
	       (if (< (point) (vm-mm-layout-body-end child-layout))
		   (delete-region (point)
				  (vm-mm-layout-body-end child-layout))
		 (vm-set-mm-layout-body-end child-layout (point-marker)))
	       (delete-region (vm-mm-layout-header-start layout)
			      (vm-mm-layout-body-start layout))
	       (vm-mime-copy-layout child-layout layout))
	     (when work-buffer (kill-buffer work-buffer)))))
	((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	 (let ((p (vm-mm-layout-parts layout)))
	   (while p
	     (vm-mime-internalize-local-external-bodies (car p))
	     (setq p (cdr p)))))
	(t nil)))

(defun vm-mime-display-internal-message/partial (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 :caption
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 :action
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-message/partial layout))))
	 :layout layout))
    (vm-inform 6 "Assembling message...")
    (let ((parts nil)
	  (missing nil)
	  extent id o total m i prev part-header-pos ;; number
	  p-number p-total p-list)                   ;; p-id
      (setq extent layout
	    layout (vm-extent-property extent 'vm-mime-layout)
	    id (vm-mime-get-parameter layout "id"))
      (if (null id)
	  (vm-mime-error
	   "message/partial message missing id parameter"))
      (with-current-buffer (marker-buffer (vm-mm-layout-body-start layout))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (and (search-forward id nil t)
			(setq m (vm-message-at-point)))
	      (setq o (vm-mm-layout m))
	      (if (not (vectorp o))
		  nil
		(setq p-list (vm-mime-find-message/partials o id))
		(while p-list
		  (setq p-total (vm-mime-get-parameter (car p-list) "total"))
		  (if (null p-total)
		      nil
		    (setq p-total (string-to-number p-total))
		    (when (< p-total 1)
		      (vm-mime-error 
		       "message/partial specified part total < 1, %d"
		       p-total))
		    (if total
			(unless (= total p-total)
			  (vm-mime-error 
			   (concat "message/partial specified total differs "
				   "between parts, (%d != %d)")
			   p-total total))
		      (setq total p-total)))
		  (setq p-number (vm-mime-get-parameter (car p-list) "number"))
		  (when (null p-number)
		    (vm-mime-error
		     "message/partial message missing number parameter"))
		  (setq p-number (string-to-number p-number))
		  (when (< p-number 1)
		    (vm-mime-error 
		     "message/partial part number < 1, %d" p-number))
		  (when (and total (> p-number total))
		    (vm-mime-error 
		     (concat "message/partial part number greater than "
			     " expected number of parts, (%d > %d)")
		     p-number total))
		  (setq parts (cons (list p-number (car p-list)) parts))
		  (setq p-list (cdr p-list))))
	      (goto-char (vm-mm-layout-body-end o))))))
      (when (null total)
	(vm-mime-error 
	 "total number of parts not specified in any message/partial part"))
      (setq parts (sort parts
			(function
			 (lambda (p q) (< (car p) (car q))))))
      (setq i 0)
      (setq p-list parts)
      (while p-list
	(cond ((< i (car (car p-list)))
	       (vm-increment i)
	       (cond ((not (= i (car (car p-list))))
		      (setq missing (cons i missing)))
		     (t (setq prev p-list
			      p-list (cdr p-list)))))
	      (t
	       ;; remove duplicate part
	       (setcdr prev (cdr p-list))
	       (setq p-list (cdr p-list)))))
      (while (< i total)
	(vm-increment i)
	(setq missing (cons i missing)))
      (if missing
	  (vm-mime-error 
	   "part%s %s%s missing"
	   (if (cdr missing) "s" "")
	   (mapconcat
	    (function identity)
	    (nreverse (mapcar 'int-to-string (or (cdr missing) missing)))
	    ", ")
	   (if (cdr missing) (concat " and " (car missing)) "")))
      (set-buffer (vm-generate-new-unibyte-buffer "assembled message"))
      (setq vm-folder-type vm-default-folder-type)
      (vm-mime-insert-mime-headers (car (cdr (car parts))))
      (goto-char (point-min))
      (vm-reorder-message-headers
       nil :keep-list nil
       :discard-regexp
"\\(Encrypted\\|Content-\\|MIME-Version\\|Message-ID\\|Subject\\|X-VM-\\|Status\\)")
      (goto-char (point-max))
      (setq part-header-pos (point))
      (while parts
	(vm-mime-insert-mime-body (car (cdr (car parts))))
	(setq parts (cdr parts)))
      (goto-char part-header-pos)
      (vm-reorder-message-headers
       nil 
       :keep-list '("Subject" "MIME-Version" "Content-" "Message-ID" "Encrypted")
       :discard-regexp nil)
      (vm-munge-message-separators vm-folder-type (point-min) (point-max))
      (goto-char (point-min))
      (insert (vm-leading-message-separator))
      (goto-char (point-max))
      ;; The reassembled message ends with a newline, so that the trailing
      ;; separator makes the blank line the next leading one has to follow.
      ;; A last fragment need not end with one (#783).
      (unless (bolp) (insert "\n"))
      (insert (vm-trailing-message-separator))
      (set-buffer-modified-p nil)
      (vm-inform 6 "Assembling message... done")
      (save-current-buffer
       (vm-goto-new-folder-frame-maybe 'folder)
       (vm-mode)
       (if (vm-should-generate-summary)
	   (progn
	     (vm-goto-new-summary-frame-maybe)
	     (vm-summarize))))
      ;; temp buffer, don't offer to save it.
      (setq buffer-offer-save nil)
      (vm-display (or vm-presentation-buffer (current-buffer)) t
		  (list this-command) '(vm-mode startup)))
    t ))
(fset 'vm-mime-display-button-message/partial
      'vm-mime-display-internal-message/partial)

(defun vm-mime-display-internal-image-xxxx (layout image-type name)
  "Display the image object described by LAYOUT internally.
IMAGE-TYPE is its image type (png, jpeg etc.).  NAME is a string
describing the image type.                             USR, 2011-03-25"
  (vm-mime-display-internal-image-fsfemacs-xxxx layout image-type name))

(defun vm-mime-display-internal-image-fsfemacs-xxxx (layout image-type name)
  "Display the image object described by LAYOUT internally.
IMAGE-TYPE is its image type (png, jpeg etc.).  NAME is a string
describing the image type.                            USR, 2011-03-25"
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (let (start end tempfile image work-buffer
	    (selective-display nil)
	    (incremental vm-mime-display-image-strips-incrementally)
	    do-strips
	    (buffer-read-only nil))
	(if (and (setq tempfile (vm-mm-layout-image-file layout))
		 (file-readable-p tempfile))
	    nil
	  (unwind-protect
	      (progn
		(save-excursion
		  (setq work-buffer (vm-make-work-buffer))
		  (set-buffer work-buffer)
		  (setq start (point))
		  (vm-mime-insert-mime-body layout)
		  (setq end (point-marker))
		  (vm-mime-transfer-decode-region layout start end)
		  (setq tempfile (vm-make-tempfile))
		  (let ((coding-system-for-write (vm-binary-coding-system)))
		    (write-region start end tempfile nil 0))
		  (vm-mm-layout-image-file layout))
		(vm-register-folder-garbage-files (list tempfile)))
	    (and work-buffer (kill-buffer work-buffer))))
	(if (not (bolp))
	    (insert-char ?\n 1))
	(setq do-strips (and (vm-imagemagick-available-p)
			     vm-mime-use-image-strips))
	(cond (do-strips
	       (condition-case error-data
		   (let ((strips (vm-make-image-strips
				  tempfile
				  (* 2 (frame-char-height))
				  image-type t incremental))
			 (first t)
			 start o process image-list overlay-list)
		     (setq process (car strips)
			   strips (cdr strips)
			   image-list strips)
		     (if (null (process-buffer process))
			 (error "ImageMagick conversion failed"))
		     (vm-register-message-garbage-files strips)
		     (setq start (point))
		     (while strips
		       (if (or first (null (cdr strips)))
			   (progn
			     (setq first nil)
			     (insert "+-----+"))
			 (insert "|image|"))
		       (setq o (make-overlay (- (point) 7) (point)))
		       (overlay-put o 'evaporate t)
		       (setq overlay-list (cons o overlay-list))
		       (insert "\n")
		       (setq strips (cdr strips)))
		     (setq o (make-overlay start (point) nil t nil))
		     (overlay-put o 'vm-mime-layout layout)
		     (overlay-put o 'vm-mime-disposable t)
		     (if vm-use-menus
			 (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu))
		     (with-current-buffer (process-buffer process)
		       (set (make-local-variable 'vm-image-list) image-list)
		       (set (make-local-variable 'vm-image-type) image-type)
		       (set (make-local-variable 'vm-image-type-name)
			    name)
		       (set (make-local-variable 'vm-overlay-list)
			    (nreverse overlay-list)))
		     (if incremental
			 (set-process-filter
			  process
			  'vm-process-filter-display-some-image-strips))
		     (set-process-sentinel
		      process
		      'vm-process-sentinel-display-image-strips))
		 (vm-image-too-small
		  (setq do-strips nil))
		 (error
		  (vm-warn 0 0 "%s: Failed making image strips: %s" 
			   (buffer-name vm-mail-buffer) error-data)
		  ;; fallback to the non-strips way
		  (setq do-strips nil)))))
	(cond ((not do-strips)
	       (setq image (list 'image ':type image-type ':file tempfile))
	       ;; insert one char so we can attach the image to it.
	       (insert "z")
	       (put-text-property (1- (point)) (point) 'display image)
	       (clear-image-cache t)
	       (let (o)
		 (setq o (make-overlay (- (point) 1) (point) nil t nil))
		 (overlay-put o 'evaporate t)
		 (overlay-put o 'vm-mime-layout layout)
		 (overlay-put o 'vm-mime-disposable t)
		 (if vm-use-menus
		     (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu)))))
	t )
    ;; otherwise, image-type not available here
    nil ))

(defun vm-get-image-dimensions (file)
  (let (work-buffer width height exit-status)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (setq exit-status
		(vm-imagemagick-call-identify nil t (list file)))
	  (goto-char (point-min))
	  (or (search-forward " " nil t)
	      (error "no spaces in 'identify' output (exit %s, file %s): %s"
		     exit-status file (buffer-string)))
	  (if (not (re-search-forward "\\b\\([0-9]+\\)x\\([0-9]+\\)\\b" nil t))
	      (error "file dimensions missing from 'identify' output: %s"
		     (buffer-string)))
	  (setq width (string-to-number (match-string 1))
		height (string-to-number (match-string 2))))
      (and work-buffer (kill-buffer work-buffer)))
    (list width height)))

(defun vm-imagemagick-type-indicator-for (image-type)
  (cond ((eq image-type 'jpeg) "jpeg:")
	((eq image-type 'gif) "gif:")
	((eq image-type 'png) "png:")
	((eq image-type 'tiff) "tiff:")
	((eq image-type 'xpm) "xpm:")
	((eq image-type 'pbm) "pbm:")
	((eq image-type 'xbm) "xbm:")
	(t "")))

(defun vm-make-image-strips (file min-height image-type async incremental
				  &optional hroll vroll)
  (or hroll (setq hroll 0))
  (or vroll (setq vroll 0))
  (let ((process-connection-type nil)
	(i 0)
	(output-type (vm-imagemagick-type-indicator-for image-type))
	image-list dimensions width height starty newfile work-buffer
	quotient remainder adjustment process)
    (setq dimensions (vm-get-image-dimensions file)
	  width (car dimensions)
	  height (car (cdr dimensions)))
    (if (< height min-height)
	(signal 'vm-image-too-small nil))
    (setq quotient (/ height min-height)
	  remainder (% height min-height)
	  adjustment (/ remainder quotient)
	  remainder (% remainder quotient)
	  starty 0)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (goto-char (point-min))
	  (while (< starty height)
	    (setq newfile (vm-make-tempfile))
	    (if async
		(progn
		  ;; Problem - we have no way of knowing whether these
		  ;; calls succeed or not.  USR, 2011-02-23
		  (insert (vm-imagemagick-convert-shell-command)
			  " -crop"
			  (format " %dx%d+0+%d"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1))
				  starty)
			  " -page"
			  (format " %dx%d+0+0"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1)))
			  (format " -roll +%d+%d" hroll vroll)
			  " \"" file "\" \"" output-type newfile "\"\n")
		  (when incremental
			(insert "echo XZXX" (int-to-string i) "XZXX\n"))
		  (setq i (1+ i)))
	      (vm-imagemagick-call-convert
	       nil nil
	       (list "-crop"
		     (format "%dx%d+0+%d"
			     width
			     (+ min-height adjustment
				(if (zerop remainder) 0 1))
			     starty)
		     "-page"
		     (format "%dx%d+0+0"
			     width
			     (+ min-height adjustment
				(if (zerop remainder) 0 1)))
		     "-roll"
		     (format "+%d+%d" hroll vroll)
		     file (concat output-type newfile))))
	    (setq image-list (cons newfile image-list)
		  starty (+ starty min-height adjustment
			    (if (zerop remainder) 0 1))
		  remainder (if (= 0 remainder) 0 (1- remainder))))
	  (when async
	    (goto-char (point-max))
	    (insert "exit\n")
	    (setq process
		  (start-process (format "image strip maker for %s" file)
				 (current-buffer)
				 shell-file-name))
	    (process-send-string process (buffer-string))
	    (setq work-buffer nil))
	  (if async
	      (cons process (nreverse image-list))
	    (nreverse image-list)))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-process-sentinel-display-image-strips (process _what-happened)
  (with-current-buffer (process-buffer process)
    (when (and (boundp 'vm-overlay-list)
	       (overlay-buffer (car vm-overlay-list))
	       (boundp 'vm-image-list))
      (let ((strips vm-image-list)
	    (overlays vm-overlay-list)
	    (image-type vm-image-type))
	(vm-display-image-strips-on-overlay-regions strips overlays
						    image-type)))
    (kill-buffer (current-buffer))))

(defun vm-display-image-strips-on-overlay-regions (strips overlays image-type)
  (let (prop value omodified)
    (with-current-buffer (overlay-buffer (car vm-overlay-list))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (setq prop 'display)
	      (while (and strips
			  (file-exists-p (car strips))
			  (overlay-end (car overlays)))
		(setq value (list 'image ':type image-type
				  ':file (car strips)
				  ':ascent 50))
		(put-text-property (overlay-start (car overlays))
				   (overlay-end (car overlays))
				   prop value)
		(setq strips (cdr strips)
		      overlays (cdr overlays))))
	  (set-buffer-modified-p omodified))))))

(defun vm-process-filter-display-some-image-strips (process output)
  (let (which-strips (i 0))
    (while (string-match "XZXX\\([0-9]+\\)XZXX" output i)
      (setq which-strips (cons (string-to-number (match-string 1 output))
			       which-strips)
	    i (match-end 0)))
    (with-current-buffer (process-buffer process)
      (when (and (boundp 'vm-overlay-list)
		 (overlay-buffer (car vm-overlay-list))
		 (boundp 'vm-image-list))
	(let ((strips vm-image-list)
	      (overlays vm-overlay-list)
	      (image-type vm-image-type))
	  (vm-display-some-image-strips-on-overlay-regions
	   strips overlays image-type which-strips))))))

(defun vm-display-some-image-strips-on-overlay-regions
  (strips overlays image-type which-strips)
  (let (sss ooo prop value omodified)
    (with-current-buffer (overlay-buffer (car vm-overlay-list))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (setq prop 'display)
	      (while which-strips
		(setq sss (nthcdr (car which-strips) strips)
		      ooo (nthcdr (car which-strips) overlays))
		(cond ((and sss
			    (file-exists-p (car sss))
			    (overlay-end (car ooo)))
		       (setq value (list 'image ':type image-type
					 ':file (car sss)
					 ':ascent 50))
		       (put-text-property (overlay-start (car ooo))
					  (overlay-end (car ooo))
					  prop value)))
		(setq which-strips (cdr which-strips))))
	  (set-buffer-modified-p omodified))))))

(defun vm-mime-display-internal-image/gif (layout)
  (vm-mime-display-internal-image-xxxx layout 'gif "GIF"))

(defun vm-mime-display-internal-image/jpeg (layout)
  (vm-mime-display-internal-image-xxxx layout 'jpeg "JPEG"))

(defun vm-mime-display-internal-image/png (layout)
  (vm-mime-display-internal-image-xxxx layout 'png "PNG"))

(defun vm-mime-display-internal-image/tiff (layout)
  (vm-mime-display-internal-image-xxxx layout 'tiff "TIFF"))

(defun vm-mime-display-internal-image/xpm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xpm "XPM"))

(defun vm-mime-display-internal-image/pbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'pbm "PBM"))

(defun vm-mime-display-internal-image/xbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xbm "XBM"))

(defun vm-mime-frob-image-xxxx (extent &rest convert-args)
  "Create and display a thumbnail (a PNG image) for the MIME
object described by EXTENT.  The thumbnail is stored in a file
whose identity is saved in the MIME layout cache of the object.

The remaining arguments CONVERT-ARGS are passed to the ImageMagick
convert program during the creation of the thumbnail image.  

The return value does not seem to be meaningful.     USR, 2011-03-25"
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (tempfile (vm-mm-layout-image-file layout))
         (saved-type (vm-mm-layout-type layout))
	 (saved-disposition (vm-mm-layout-disposition layout))
         success
	 (work-buffer nil))
    (if (and tempfile (vm-mm-layout-image-modified layout))
	;; image already frobbed
	(setq success t)
      ;; create a frob
      (setq work-buffer (vm-make-work-buffer))
      (unwind-protect
	  (with-current-buffer work-buffer
	    (set-buffer-file-coding-system (vm-binary-coding-system))
	    ;; convert just the first page "[0]" and enforce PNG
	    ;; output by "png:"
	    (let ((coding-system-for-read (vm-binary-coding-system)))
	      (setq success
		    (eq 0 (vm-imagemagick-call-convert
			   tempfile t
			   (append convert-args
				   (list "-[0]" "png:-"))))))
	    (when success
	      (write-region (point-min) (point-max) tempfile nil 0)
	      (vm-set-mm-layout-image-modified layout t)))
	;; unwind-protection
	(when work-buffer (kill-buffer work-buffer))))

    (unwind-protect
	(when success
	  ;; the output is always PNG now, so fix it for displaying, but restore
	  ;; it for the layout afterwards
	  (vm-set-mm-layout-type layout '("image/png"))
	  (vm-set-mm-layout-disposition layout '("inline"))
	  ;; Keep the image files around in case the user comes back
	  ;; to them.   USR, 2012-11-17
	  (vm-mime-display-internal-generic extent))
      (vm-set-mm-layout-type layout saved-type)
      (vm-set-mm-layout-disposition layout saved-disposition))))

(defun vm-mime-rotate-image-left (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "-90"))

(defun vm-mime-rotate-image-right (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "90"))

(defun vm-mime-mirror-image (extent)
  (vm-mime-frob-image-xxxx extent "-flop"))

(defun vm-mime-brighten-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "115"))

(defun vm-mime-dim-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "85"))

(defun vm-mime-monochrome-image (extent)
  (vm-mime-frob-image-xxxx extent "-monochrome"))

(defun vm-mime-revert-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (tempfile (vm-mm-layout-image-file layout)))
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; It is not supported any more.  USR, 2012-11-18
    ;; It is not clear why the tempfile is being deleted.  When will
    ;; it be re-created?  USR, 2012-11-18
    (and (stringp tempfile)
    	 (vm-error-free-call 'delete-file tempfile))
    (vm-set-mm-layout-image-modified layout nil)
    (vm-mime-display-generic extent)))

(defun vm-mime-larger-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (tempfile (vm-mm-layout-image-file layout))
	 dims)
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (* 2 (car dims)))
				     "x"
				     (int-to-string (* 2 (nth 1 dims)))))))

(defun vm-mime-smaller-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (tempfile (vm-mm-layout-image-file layout))
	 dims)
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (/ (car dims) 2))
				     "x"
				     (int-to-string (/ (nth 1 dims) 2))))))

(defcustom vm-mime-thumbnail-max-geometry "80x80"
  "If thumbnails should be displayed as part of MIME buttons, then set
this variable to a string describing the geometry, e.g., \"80x80\".
Otherwise, set it to nil.                              USR, 2011-03-25"
  :group 'vm-mime
  :type '(choice string
		 (const :tag "Disable thumbnails." nil)))

(defun vm-mime-display-button-image (layout)
  "Displays a button for the MIME LAYOUT and includes a thumbnail
image when possible."
  (if (and (vm-imagemagick-available-p)
	   vm-mime-thumbnail-max-geometry
	   (vm-images-possible-here-p))
      ;; create a thumbnail and display it
      (let (tempfile start thumb-extent glyph) ;; end
	;; fake an extent to display the image as thumb
	(setq start (point))
	(insert " ")
	(setq thumb-extent (vm-make-extent start (point)))
	(vm-set-extent-property thumb-extent 'vm-mime-layout layout)
	(vm-set-extent-property thumb-extent 'vm-mime-disposable nil)
	(vm-set-extent-property thumb-extent 'start-open t)
	;; write out the image data 
	(setq tempfile (vm-mm-layout-image-file layout))
	(unless tempfile
	  (with-current-buffer (vm-make-work-buffer)
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
	    (setq tempfile (vm-make-tempfile))
	    (let ((coding-system-for-write (vm-binary-coding-system)))
	      (write-region (point-min) (point-max) tempfile nil 0))
	    (kill-buffer (current-buffer)))
	  ;; store the temp filename
	  (vm-set-mm-layout-image-file layout tempfile)
	  (vm-register-folder-garbage-files (list tempfile)))
	;; display a thumbnail over the fake extent
	(let ((vm-mime-internal-content-types '("image"))
	      (vm-mime-internal-content-type-exceptions nil)
	      (vm-mime-auto-displayed-content-types '("image"))
	      (vm-mime-auto-displayed-content-type-exceptions nil)
	      (vm-mime-use-image-strips nil))
	  (vm-mime-frob-image-xxxx thumb-extent
				   "-thumbnail" 
				   vm-mime-thumbnail-max-geometry))
	;; extract image data, don't need the image itself!
	;; if the display was not successful, glyph will be nil
	(setq glyph (get-text-property start 'display))
	(delete-region start (point))
	;; insert the button and replace the image 
	(setq start (point))
	(vm-mime-display-button-xxxx layout t)
	(when glyph
	  (put-text-property start (1+ start) 'display glyph))
	;; remove the cached thumb so that full sized image will be shown
	;; next time
	t)
    ;; if image not possible, just display the normal button
    (vm-mime-display-button-xxxx layout t)))

(defun vm-mime-display-button-application/pdf (layout)
  (vm-mime-display-button-image layout))

(defun vm-mime-display-generic (layout)
  "Display the mime object described by LAYOUT, irrespective of
whether it is meant to be to be displayed automatically."
  (save-excursion
    (let ((vm-mime-auto-displayed-content-types t)
	  (vm-mime-auto-displayed-content-type-exceptions nil))
      (vm-decode-mime-layout layout t))))

(defun vm-mime-display-internal-generic (layout)
  "Display the mime object described by LAYOUT internally,
irrespective of whether it is meant to be to be displayed
automatically.  No external viewers are tried.     USR, 2011-03-25"
  (save-excursion
    (let ((vm-mime-auto-displayed-content-types t)
	  (vm-mime-auto-displayed-content-type-exceptions nil)
	  (vm-mime-external-content-types-alist nil))
      (vm-decode-mime-layout layout t))))

(defun vm-mime-display-button-xxxx (layout disposable)
  "Display a button for the mime object described by LAYOUT.  If
DISPOSABLE is true, then the button will be removed when it is
expanded to display the mime object."
  (vm-mime-insert-button
   :caption (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
   :action (function vm-mime-display-generic)
   :layout layout :disposable disposable))

;;----------------------------------------------------------------------------
;;; MIME buttons
;;
;; vm-find-layout-extent-at-point: () -> extent
;; vm-mime-run-display-funciton-at-point: (layout -> 'a) -> 'a
;; vm-mime-reader-map-save-file: () -> file
;; vm-mime-reader-map-save-message: () -> file
;; vm-mime-reader-map-pipe-to-command: () -> void
;; vm-mime-reader-map-pipe-to-command-discard-output: () -> void
;; vm-mime-reader-map-pipe-to-printer: () -> void
;; vm-mime-reader-map-display-using-external-viewer: () -> void
;; vm-mime-reader-map-display-using-default: () -> void
;; vm-mime-reader-map-display-object-as-type: () -> void
;; vm-mime-reader-map-attach-to-composition: () -> void
;;----------------------------------------------------------------------------

(defun vm-find-layout-extent-at-point ()
  "Return the MIME layout of the MIME button at point."
  (vm-extent-at (point) 'vm-mime-layout))

;;;###autoload
(defun vm-mime-run-display-function-at-point (&optional function)
  "Run the `vm-mime-function' for the MIME button at point.
If optional argument FUNCTION is given, run it instead.
					          USR, 2011-03-07"
  (interactive)
  ;; The presentation buffer counts too.  A message whose body is still on the
  ;; server has a layout parsed from its headers alone, so its parts have no
  ;; text: acting on one wrote an empty file and said nothing about why (issue
  ;; #386).  The folder buffer refused already; this refuses wherever the button
  ;; is, and says what to do about it.
  (when (and (memq major-mode '(vm-mode vm-virtual-mode vm-presentation-mode))
	     (vm-body-to-be-retrieved-of
	      (vm-real-message-of (car vm-message-pointer))))
    (error (concat "This message's body is not loaded, so its attachments have"
		   " no contents here.  Type o (vm-load-message) on the message"
		   " first, or set vm-external-fetch-message-for-presentation")))

  ;; save excursion to keep point from moving.  its motion would
  ;; drag window point along, to a place arbitrarily far from
  ;; where it was when the user triggered the button.
  (save-excursion
    (let ((extent (vm-find-layout-extent-at-point))
	  ) ;; retval
      (and extent
	   (funcall 
	    (or function (vm-extent-property extent 'vm-mime-function))
	    extent)))))

;;;###autoload
(defun vm-mime-reader-map-save-file ()
  "Write the MIME object at point to a file."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (file)
    (save-excursion
      (setq file (vm-mime-run-display-function-at-point
		  'vm-mime-send-body-to-file)))
    (when (and file vm-mime-delete-after-saving)
      (let ((extent (vm-find-layout-extent-at-point)))
	(vm-mime-delete-body-after-saving extent file)))
    file ))

;;;###autoload
(defun vm-mime-reader-map-save-message ()
  "Save the MIME object at point to a folder."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (folder)
    (save-excursion
      (setq folder (vm-mime-run-display-function-at-point
		    'vm-mime-send-body-to-folder)))
    (when (and folder vm-mime-delete-after-saving)
      (let ((extent (vm-find-layout-extent-at-point)))
	(vm-mime-delete-body-after-saving extent folder)))
    folder ))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-command ()
  "Pipe the MIME object at point to a shell command."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-pipe-body-to-queried-command))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-command-discard-output ()
  "Pipe the MIME object at point to a shell command."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-pipe-body-to-queried-command-discard-output))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-printer ()
  "Print the MIME object at point."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-send-body-to-printer))

;;;###autoload
(defun vm-mime-reader-map-display-using-external-viewer ()
  "Display the MIME object at point with an external viewer."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-display-body-using-external-viewer))

;;;###autoload
(defun vm-mime-reader-map-display-using-default ()
  "Display the MIME object at point using the `default' face."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-display-body-as-text))

;;;###autoload
(defun vm-mime-reader-map-display-object-as-type ()
  "Display the MIME object at point as some other type."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-display-object-as-type))

;;;###autoload
(defun vm-mime-reader-map-convert-then-display ()
  "Convert the MIME object at point to text and display it."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-convert-body-then-display))

;;;###autoload
(defun vm-mime-reader-map-attach-to-composition ()
  "Attach the MIME object at point to a message being composed.  The
buffer for message composition is queried from the minibuffer."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-attach-body-to-composition))

;;----------------------------------------------------------------------------
;;; MIME-related commands
;;
;; vm-mime-action-on-all-attachments ::
;;	(count :: int, 
;;	 action :: ((message, layout, type, filename) -> void),
;;	 &optional 
;;	 types :: type list, exceptions :: type list, 
;;	 mlist :: message list, quiet :: bool) 
;;	-> void
;; This function is replaced by the following, but interface retained
;; for backward-compatibility.
;;
;; vm-mime-operate-on-attachments ::
;;	(count::int, &key
;;	 :action :: ((message, layout, type, filename) -> void),
;;	 :included :: type list, 
;;	 :excluded :: type list, 
;;	 :messages :: message list, 
;;	 :name :: string) 
;;	-> void
;; vm-delete-all-attachments :: (&optional count :: int) -> void
;; vm-save-all-attachments :: (&optional
;;			       count :: int, directory :: path) -> void
;; vm-save-attachments :: (&optional count :: int) -> void
;;----------------------------------------------------------------------------

;;;###autoload
(cl-defun vm-mime-operate-on-attachments (count &key 
					      ((:name action-name))
					      ((:action action))
					      ((:included types)) 
					      ((:excluded exceptions))
					      ((:messages mlist)))
  "On the next COUNT messages or marked messages, call the
function ACTION on all \"attachments\".  

For the purpose of this function, an \"attachment\" is a mime
part part which has \"attachment\" as its disposition, or simply
has an associated filename, or has a type that matches a regexp
in TYPES but doesn't match one in EXCEPTIONS.

ACTION-NAME should be a human-readable string describing the
action in minibuffer messages.  Or it can be nil to suppress
messages. 

ACTION will get called with four arguments: MSG LAYOUT TYPE FILENAME." 
  (unless mlist
    (unless count (setq count 1))
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer-and-validate 1 nil))

  (let ((mlist (or mlist (vm-select-operable-messages
			  count (vm-interactive-p) "Action on"))))
    (vm-retrieve-operable-messages count mlist :fail t)
    (save-excursion
      (while mlist
        (let (parts layout filename type disposition o) ;; m
          (setq o (vm-mm-layout (car mlist)))
          (when (stringp o)
            (setq o 'none)
            (backtrace)
            (vm-inform 0 "There is a bug, please report it with *backtrace*"))
          (unless (eq o 'none)
            (setq type (car (vm-mm-layout-type o)))
            
            (cond ((or (vm-mime-types-match "multipart/alternative" type)
                       (vm-mime-types-match "multipart/mixed" type)
                       (vm-mime-types-match "multipart/report" type)
                       (vm-mime-types-match "message/rfc822" type)
                       )
                   (setq parts (copy-sequence (vm-mm-layout-parts o))))
                  (t (setq parts (list o))))
            
            (while parts
	      ;; Replace a composite part by its sub-parts, repeatedly.
	      ;; A composite with no sub-parts -- e.g. a multipart whose
	      ;; boundary never appears, as seen in delivery-failure
	      ;; reports -- just disappears, and if it was the last part
	      ;; that empties the list, so test PARTS as well.
              (while (and parts
			  (vm-mime-composite-type-p
			   (car (vm-mm-layout-type (car parts)))))
		(setq parts
		      (nconc (copy-sequence (vm-mm-layout-parts (car parts)))
			     (cdr parts))))

	      (when parts
		(setq layout (car parts)
		      type (car (vm-mm-layout-type layout))
		      disposition (car (vm-mm-layout-disposition layout))
		      filename (vm-mime-get-disposition-filename layout) )

		(cond ((or filename
			   (and disposition (string= disposition "attachment"))
			   (and (not (vm-mime-types-match
				      "message/external-body" type))
				types
				(vm-mime-is-type-valid type types exceptions)))
		       (when action-name
			 (vm-inform 10
			  "%s part type=%s filename=%s disposition=%s"
			  action-name type filename disposition))
		       (funcall action (car mlist) layout type filename))
		      (action-name
		       (vm-inform 10
			"No %s on part type=%s filename=%s disposition=%s"
			action-name type filename disposition)))
		(setq parts (cdr parts))))))
        (setq mlist (cdr mlist))))))

;;;###autoload
(defun vm-mime-action-on-all-attachments 
  (count action &optional types exceptions mlist quiet)
  "On the next COUNT messages or marked messages, call the
function ACTION on all \"attachments\".  For the purpose of this
function, an \"attachment\" is a mime part part which has
\"attachment\" as its disposition, or simply has an associated
filename, or has a type that matches a regexp in TYPES but
doesn't match one in EXCEPTIONS.

If QUIET is true no messages are generated.

ACTION will get called with four arguments: MSG LAYOUT TYPE FILENAME." 
  (vm-mime-operate-on-attachments
   count :action action :included types :excluded exceptions :messages mlist
   :name (if quiet nil "action on")))

(defun vm-mime-is-type-valid (type types-alist type-exceptions)
  (catch 'done
    (let ((list type-exceptions)
          (matched nil))
      (while list
        (if (vm-mime-types-match (car list) type)
            (throw 'done nil)
          (setq list (cdr list))))
      (setq list types-alist)
      (while (and list (not matched))
        (if (vm-mime-types-match (car list) type)
            (setq matched t)
          (setq list (cdr list))))
      matched )))

;;;###autoload
(defun vm-delete-all-attachments (&optional count)
  "Delete all attachments from the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-deletable-types' but not `vm-mime-deletable-type-exceptions'
are also included."
  (interactive "p")
  (vm-check-for-killed-summary)
  (if (vm-interactive-p) (vm-follow-summary-cursor))
  
  (let ((successes 0))
    (vm-mime-operate-on-attachments
     count
     :name "deleting"
     :action
     (lambda (_msg layout type file)
       (vm-inform 7 "Deleting `%s%s" type (if file (format " (%s)" file) ""))
       (vm-mime-discard-layout-contents layout)
       (setq successes (+ 1 successes)))
     :included vm-mime-deletable-types
     :excluded vm-mime-deletable-type-exceptions)
    (when (vm-interactive-p)
      (vm-discard-cached-data count)
      (let ((vm-preview-lines nil))
	(vm-present-current-message)))
    (if (> successes 0)
	(vm-inform 5 "%d attachment%s deleted" successes (if (= successes 1) "" "s"))
      (vm-inform 5 "No attachments deleted")))
  (vm-update-summary-and-mode-line))


;;;###autoload
(defun vm-save-all-attachments (&optional count directory)
  "Save all attachments in the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-saveable-types' but not `vm-mime-saveable-type-exceptions'
are also included.

The attachments are saved to the specified DIRECTORY.  The
variables `vm-mime-all-attachments-directory' or
`vm-mime-attachment-save-directory' can be used to set the
default location.  When directory does not exist it will be
created."
  (interactive
   (list current-prefix-arg
         (vm-read-file-name
          "Attachment directory: "
          (or vm-mime-all-attachments-directory
              vm-mime-attachment-save-directory
              default-directory)
          (or vm-mime-all-attachments-directory
              vm-mime-attachment-save-directory
              default-directory)
          nil nil
          'vm-mime-save-all-attachments-history)))

  (vm-check-for-killed-summary)
  (if (vm-interactive-p) (vm-follow-summary-cursor))
 
  (let ((successes 0)
	(failures 0)
	(result nil)
	(refused-directories nil))
    (vm-mime-operate-on-attachments
     count
     :name "saving"
     :included vm-mime-saveable-types
     :excluded vm-mime-saveable-type-exceptions
     :action
     (lambda (msg layout type file)
       (let ((directory (if (functionp directory)
                            (funcall directory msg)
                          directory)))
         (setq file
	       (if file
		   (expand-file-name (file-name-nondirectory file) directory)
		 (let* ((dir (or directory
				 vm-mime-all-attachments-directory
				 vm-mime-attachment-save-directory))
			;; Content-Disposition gave no filename, but the
			;; part may still name itself in its Content-Type.
			(name (vm-mime-get-parameter layout "name"))
			(answer
			 (vm-read-file-name
			  (format "Save %s (no filename given) to: " type)
			  dir
			  (if name
			      (expand-file-name (file-name-nondirectory name)
						dir)
			    dir)
			  nil nil
			  'vm-mime-save-all-attachments-history)))
		   ;; A directory is not a file name -- and it is what
		   ;; answering the prompt with RET used to give, since
		   ;; the directory was offered as the default.  Saving
		   ;; there would ask to "overwrite" the directory and
		   ;; then fail in delete-file.  Collect these and report
		   ;; them once at the end rather than pausing here for
		   ;; each one.
		   (if (and answer (file-directory-p answer))
		       (progn
			 (setq refused-directories
			       (cons answer refused-directories))
			 nil)
		     answer))))

         (if (and file (file-exists-p file))
             (if (y-or-n-p (format "Overwrite `%s'? " file))
                 (delete-file file)
               (setq file nil)))
         
         (if (null file)
	     (setq failures (+ 1 failures))
           (vm-inform 5 "Saving %s" (if file (format " (%s)" file) ""))
           (make-directory (file-name-directory file) t)
           (setq result (vm-mime-send-body-to-file layout file file))
           (when result 
	     (when vm-mime-delete-after-saving
               (let ((vm-mime-confirm-delete nil))
                 (vm-mime-discard-layout-contents 
		  layout (expand-file-name file))))
	     (setq successes (+ 1 successes))))))
     )

    (when (vm-interactive-p)
      (vm-discard-cached-data count)
      (let ((vm-preview-lines nil))
	(vm-present-current-message)))
    
    (when refused-directories
      ;; the same directory is the obvious answer for every part, so
      ;; the list is usually the same name over and over
      (setq refused-directories
	    (delete-dups (nreverse refused-directories)))
      (vm-warn 0 2 "Not saved: %s %s a directory, not a file name"
	       (mapconcat #'identity refused-directories ", ")
	       (if (cdr refused-directories) "name" "names")))

    (if (> failures 0)
	(if (> successes 0)
	    (vm-inform 5 "%d attachment%s saved; %s failed" 
		       successes (if (= successes 1) "" "s") failures)
	  (vm-inform 5 "No attachments saved; %s failed" failures))
	(if (> successes 0)
	    (vm-inform 5 "%d attachment%s saved" 
		       successes (if (= successes 1) "" "s"))
	  (vm-inform 5 "No attachments saved")))))


;;;###autoload
(defun vm-save-attachments (&optional count)
  "Save all attachments in the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-saveable-types' but not `vm-mime-saveable-type-exceptions'
are also included.

The attachments are saved in file names input from the
minibuffer.  (This is the main difference from
`vm-save-all-attachments'.) 

The variables `vm-mime-all-attachments-directory' or
`vm-mime-attachment-save-directory' can be used to set the
default location.  When directory does not exist it will be
confirmed before creating a new directory."
  (interactive "p")

  (vm-check-for-killed-summary)
  (if (vm-interactive-p) (vm-follow-summary-cursor))
 
  (let ((successes 0)
	(failures 0)
	(directory nil))
    (vm-mime-operate-on-attachments
     count
     :included vm-mime-saveable-types
     :excluded vm-mime-saveable-type-exceptions
     :name "saving"
     :action
     (lambda (_msg layout type file-name)
       (let ((file (vm-read-file-name
		    (if file-name			; prompt
			(format "Save (default %s): " file-name)
		      (format "Save %s: " type))
		    (file-name-as-directory		; directory
		     (or directory		      
			 vm-mime-attachment-save-directory
			 vm-mime-all-attachments-directory
			 ;; both are allowed to be nil -- the customize type
			 ;; of the first offers it -- and `file-name-as-directory'
			 ;; of nil is an error, so the command signalled instead
			 ;; of asking where to save
			 default-directory))
		    (and file-name			; default-filename
			 (concat
			  (file-name-as-directory 	      
			   (or directory		      
			       vm-mime-attachment-save-directory
			       vm-mime-all-attachments-directory
			       default-directory))
			  (or file-name "")))
		    nil nil			      ; mustmatch initial
		    'vm-mime-save-all-attachments-history
		    )))
	 (setq directory (file-name-directory file))
         (when (file-exists-p file)
	   (if (y-or-n-p (format "Overwrite `%s'? " file))
	       nil 		; (delete-file file)
	     (setq file nil)))
	 (unless (file-exists-p directory)
	   (if (y-or-n-p 
		(format "Directory %s does not exist; create it?" directory))
	       (make-directory directory t)
	     (setq file nil)))
         (if (null file)
	     (setq failures (+ 1 failures))
           (vm-inform 5 "Saving %s" (if file (format " (%s)" file) ""))
           (vm-mime-send-body-to-file layout file file)
           (if vm-mime-delete-after-saving
               (let ((vm-mime-confirm-delete nil))
                 (vm-mime-discard-layout-contents 
		  layout (expand-file-name file))))
           (setq successes (+ 1 successes)))))
     )

    (when (vm-interactive-p)
      (vm-discard-cached-data count)
      (vm-present-current-message))
    
    (if (> failures 0)
	(if (> successes 0)
	    (vm-inform 5 "%d attachment%s saved; %s failed" 
		       successes (if (= successes 1) "" "s") failures)
	  (vm-inform 5 "No attachments saved; %s failed" failures))
	(if (> successes 0)
	    (vm-inform 5 "%d attachment%s saved" 
		       successes (if (= successes 1) "" "s"))
	  (vm-inform 5 "No attachments saved")))))
;; for the karking compiler
(defvar vm-menu-mime-dispose-menu)

(defun vm-mime-set-image-stamp-for-type (e type)
  "Set an image stamp for MIME button extent E as appropriate for
TYPE.                                                 USR, 2011-03-25"
  (vm-mime-fsfemacs-set-image-stamp-for-type e type))

(defconst vm-mime-type-images
  '(("text" "text.xpm")
    ("image" "image.xpm")
    ("audio" "audio.xpm")
    ("video" "video.xpm")
    ("message" "message.xpm")
    ("application" "application.xpm")
    ("multipart" "multipart.xpm")))

(defun vm-mime-fsfemacs-set-image-stamp-for-type (e type)
  "Set an image stamp for MIME button extent E as appropriate for
TYPE.

This is done by extending the extent with one character position at
the front and placing the image there as the display text property.
                                                         USR, 2011-03-25"
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p 'xpm))
      (let ((dir (vm-image-directory))
        (tuples vm-mime-type-images)
             file)
	(setq file (catch 'done
		     (while tuples
		       (if (vm-mime-types-match (car (car tuples)) type)
			   (throw 'done (car tuples))
			 (setq tuples (cdr tuples))))
		     nil)
	      file (and file (nth 1 file))
	      file (and file (expand-file-name file dir)))
	(if file
	    (save-excursion
	      (let ((buffer-read-only nil))
		(set-buffer (overlay-buffer e))
		(goto-char (overlay-start e))
		(insert "x")
		(move-overlay e (1- (point)) (overlay-end e))
		(put-text-property (1- (point)) (point) 'display
				   (list 'image
					 ':ascent 80
					 ':color-symbols
					   (list
					    (cons "background"
						  (cdr (assq
							'background-color
							(frame-parameters)))))
					 ':type 'xpm
					 ':file file))))))))

(cl-defun vm-mime-insert-button (&key caption action layout (disposable nil))
  "Display a button for a mime object, using CAPTION as the label (a
string) and ACTION as the default action (a function).  The mime object
is described by LAYOUT.  If DISPOSABLE is true, then the button will
be removed when it is expanded to display the mime object."
  (let ((start (point))	e
	(keymap vm-mime-reader-map)
	(buffer-read-only nil))
    (setq keymap (append keymap (current-local-map)))
    (if (not (bolp))
	(insert "\n"))
    (insert caption "\n")
    ;; the five argument make-overlay: an overlay must advance when text is
    ;; inserted at its start position, or inline text and graphics seep into
    ;; the button overlay and are then removed when the button is
    (setq e (vm-make-extent start (point) nil t nil))
    (vm-mime-set-image-stamp-for-type e (car (vm-mm-layout-type layout)))
    (vm-set-extent-property e 'local-map keymap)
    (vm-set-extent-property e 'vm-button t)
    (vm-set-extent-property e 'vm-mime-disposable disposable)
    (vm-set-extent-property e 'face vm-mime-button-face)
    (vm-set-extent-property e 'mouse-face vm-mime-button-mouse-face)
    (vm-set-extent-property e 'vm-mime-layout layout)
    (vm-set-extent-property e 'vm-mime-function action)
    ;; for vm-continue-postponed-message
    (put-text-property (overlay-start e)
		       (overlay-end e)
		       'vm-mime-layout layout)
    ;; return t as decoding worked
    t))

(defun vm-mime-rewrite-failed-button (button error-string)
  (let* ((buffer-read-only nil)
	 (start (point)))
    (goto-char (vm-extent-start-position button))
    (insert (format "DISPLAY FAILED -- %s\n" error-string))
    (vm-set-extent-endpoints button start (vm-extent-end-position button))
    (delete-region (point) (vm-extent-end-position button))))


;;---------------------------------------------------------------------------
;;; MIME button operations
;;
;; vm-mime-send-body-to-file: (extent-or-layout 
;;		               &optional filename filepath bool) -> filename
;; vm-mime-send-body-to-folder: (extent-or-layout 
;;		                 &optional filename) -> filename
;; vm-mime-delete-body-after-saving: (extent) -> void
;; vm-mime-pipe-body-to-queried-command: (extent &optional bool) -> bool
;; vm-mime-pipe-body-to-queried-command-discard-output: (extent) -> bool
;; vm-mime-send-body-to-printer: (extent) -> bool
;; vm-mime-display-body-as-text: (extent) -> ?
;; vm-mime-display-object-as-type: (extent) -> ?
;; vm-mime-display-body-using-external-viewer: (extent) -> ?
;; vm-mime-convert-body-then-display: (extent) -> ?
;; vm-mime-attach-body-to-composition: (extent) -> ?
;;---------------------------------------------------------------------------
 
;; From: Eric E. Dors
;; Date: 1999/04/01
;; Newsgroups: gnu.emacs.vm.info
;; example filter-alist variable
(defvar vm-mime-write-file-filter-alist
  '(("application/mac-binhex40" . "hexbin -s "))
  "*A list of filter used when writing attachements to files."
  )
 
;; function to parse vm-mime-write-file-filter-alist
(defun vm-mime-find-write-filter (type)
  (let ((e-alist vm-mime-write-file-filter-alist)
	(matched nil))
    (while (and e-alist (not matched))
      (if (and (vm-mime-types-match (car (car e-alist)) type)
	       (cdr (car e-alist)))
	  (setq matched (cdr (car e-alist)))
	(setq e-alist (cdr e-alist))))
    matched))

(defun vm-mime-delete-body-after-saving (layout file)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (unless (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
    (let ((vm-mime-confirm-delete nil))
      ;; we don't care if the delete fails
      (condition-case nil
	  (vm-delete-mime-object (expand-file-name file))
	(error nil)))))

(defun vm-mime-send-body-to-file (layout &optional default-filename file
                                         overwrite)
  "Writes the body of MIME object given by LAYOUT to FILE.  Returns
boolean value indicating success or failure.
The optional argument DEFAULT-FILENAME gives the default filename to
be used if FILE is not specified.  OVERWRITE says whether any existing
file with the name should be overwritten."
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (unless default-filename
    (setq default-filename (vm-mime-get-disposition-filename layout)))
  (when default-filename
    (setq default-filename (file-name-nondirectory default-filename)))
  (let (;; evade the XEmacs dialog box, yeccch.
	(use-dialog-box nil)
	(dir vm-mime-attachment-save-directory)
	(done nil))
    (when (null file)
      (while (not done)
	(setq file
	      (read-file-name
	       (if default-filename
		   (format "Write MIME body to file (default %s): "
			   default-filename)
		 "Write MIME body to file: ")
	       dir default-filename)
	      file (expand-file-name file dir))
	(if (not (file-directory-p file))
	    (setq done t)
	  (unless default-filename
	    (error "%s is a directory" file))
	  (setq file (expand-file-name default-filename file)
		done t))))
    (let ((work-buffer (vm-make-work-buffer))
	  (coding-system-for-read (vm-binary-coding-system)))
      (unwind-protect
	  (condition-case err
	      (with-current-buffer work-buffer
		(setq selective-display nil)
		;; Tell XEmacs/MULE not to mess with the bits unless
		;; this is a text type.
		(if (fboundp 'set-buffer-file-coding-system)
		    (if (vm-mime-text-type-layout-p layout)
			(set-buffer-file-coding-system
			 (vm-line-ending-coding-system) nil)
		      (set-buffer-file-coding-system (vm-binary-coding-system) t)))
		(vm-mime-insert-mime-body layout)
		(vm-mime-transfer-decode-region layout (point-min) (point-max))
		(unless (or overwrite (not (file-exists-p file)))
		  (or (y-or-n-p "File exists, overwrite? ")
		      (error "Aborted")))
		;; Bind the jka-compr-compression-info-list to nil so
		;; that jka-compr won't compress already compressed
		;; data.  This is a crock, but as usual I'm getting
		;; the bug reports for somebody else's bad code.
		(let ((jka-compr-compression-info-list nil)
		      (command (vm-mime-find-write-filter
				(car (vm-mm-layout-type layout)))))
		  (if command 
		      (shell-command-on-region 
		       (point-min) (point-max) (concat command " > " file))
		    (write-region (point-min) (point-max) file nil nil)))
		file )
	    (error (vm-warn 1 2 "Error in writing %s: %s" file err)
		   nil))
	(when work-buffer (kill-buffer work-buffer))
	;; Saving can change what the part is displayed as, since
	;; `vm-mime-delete-after-saving' turns it into an external-body
	;; reference, so the message is presented again to show that.  This
	;; was advice on this function, from vm-rfaddons.
	(when vm-mime-delete-after-saving
	  (vm-present-current-message))))))

(defun vm-mime-send-body-to-folder (layout &optional default-filename)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (let ((type (car (vm-mm-layout-type layout)))
	file)
    (if (not (or (vm-mime-types-match type "message/rfc822")
		 (vm-mime-types-match type "message/news")))
	(vm-mime-send-body-to-file layout default-filename)
      (let ((work-buffer (vm-make-work-buffer))
	    (coding-system-for-read (vm-binary-coding-system))
	    (coding-system-for-write (vm-binary-coding-system)))
	(unwind-protect
	    (with-current-buffer work-buffer
	      (setq selective-display nil)
	      ;; Tell XEmacs/MULE not to mess with the bits unless
	      ;; this is a text type.
	      (if (fboundp 'set-buffer-file-coding-system)
		  (set-buffer-file-coding-system
		   (vm-line-ending-coding-system) nil))
	      (vm-mime-insert-mime-body layout)
	      (vm-mime-transfer-decode-region layout (point-min) (point-max))
	      (goto-char (point-min))
	      (insert (vm-leading-message-separator 'mmdf))
	      (goto-char (point-max))
	      ;; mmdf's separator begins a line, and a part's body need not
	      ;; end with a newline (#783).
	      (unless (bolp) (insert "\n"))
	      (insert (vm-trailing-message-separator 'mmdf))
	      (set-buffer-modified-p nil)
	      (vm-mode t)
	      (let ((vm-check-folder-types t)
		    (vm-convert-folder-types t))
		(setq file (call-interactively 'vm-save-message)))
	      (vm-quit-no-change)
	      file )
	  (when work-buffer (kill-buffer work-buffer)))))))

(defvar binary-process-input) ;; FIXME: Unknown var.  XEmacs?

(defun vm-mime-pipe-body-to-command (command layout &optional discard-output)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (let ((output-buffer (if discard-output
			   0
			 (get-buffer-create "*Shell Command Output*"))))
    (when (bufferp output-buffer)
      (with-current-buffer output-buffer
	(erase-buffer)))
    (let ((work-buffer (vm-make-work-buffer)))
      (unwind-protect
	  (with-current-buffer work-buffer
	    ;; call-process-region calls write-region.
	    ;; don't let it do CR -> LF translation.
	    (setq selective-display nil)
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
	    (let ((pop-up-windows (and pop-up-windows
				       (eq vm-mutable-window-configuration t)))
		  (process-coding-system-alist
		   (if (vm-mime-text-type-layout-p layout)
		       nil
		     (list (cons "." (vm-binary-coding-system)))))
		  ;; Tell DOS/Windows NT whether the input is binary
		  (binary-process-input
		   (not
		    (vm-mime-text-type-layout-p layout))))
	      (call-process-region (point-min) (point-max)
				   (or shell-file-name "sh")
				   nil output-buffer nil
				   shell-command-switch command)))
	(when work-buffer (kill-buffer work-buffer))))
    (when (bufferp output-buffer)
      (if (not (zerop (with-current-buffer output-buffer (buffer-size))))
	  (vm-display output-buffer t (list this-command)
		      '(vm-pipe-message-to-command))
	(vm-display nil nil (list this-command)
		    '(vm-pipe-message-to-command))))
    t ))

(defun vm-mime-pipe-body-to-queried-command (button &optional discard-output)
  (let ((command (read-string "Pipe object to command: ")))
    (vm-mime-pipe-body-to-command command button discard-output)))

(defun vm-mime-pipe-body-to-queried-command-discard-output (button)
  (vm-mime-pipe-body-to-queried-command button t))

(defun vm-mime-send-body-to-printer (button)
  (vm-mime-pipe-body-to-command (mapconcat (function identity)
					   (nconc (list vm-print-command)
						  vm-print-command-switches)
					   " ")
				button))

(defun vm-mime-display-body-as-text (button)
  (let ((vm-mime-auto-displayed-content-types '("text/plain"))
	(vm-mime-auto-displayed-content-type-exceptions nil)
	(layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (vm-set-mm-layout-type layout '("text/plain" "charset=us-ascii"))
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-object-as-type (button)
  (let ((vm-mime-auto-displayed-content-types t)
	(vm-mime-auto-displayed-content-type-exceptions nil)
	(old-layout (vm-extent-property button 'vm-mime-layout))
	layout
	(type (read-string "View as MIME type: ")))
    (setq layout (copy-sequence old-layout))
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (setcar (vm-mm-layout-type layout) type)
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-body-using-external-viewer (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout))
	(vm-mime-external-content-type-exceptions nil))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (if (vm-mm-layout-display-error layout)
	  (apply 'error (vm-mm-layout-display-error layout)))
      ;; Use the child layout for external viewer
      (setq layout (car (vm-mm-layout-parts layout))))
    (if (vm-mime-find-external-viewer (car (vm-mm-layout-type layout)))
	(vm-mime-display-external-generic layout)
      (error "No viewer defined for type %s"
	     (car (vm-mm-layout-type layout))))))

(defun vm-mime-convert-body-then-display (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout)))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (if (vm-mm-layout-display-error layout)
	  (apply 'error (vm-mm-layout-display-error layout)))
      (setq layout (car (vm-mm-layout-parts layout))))
    (setq layout (vm-mime-convert-undisplayable-layout layout))
    (if (vm-mm-layout-display-error layout)
	(apply 'error (vm-mm-layout-display-error layout)))
    (if (null layout)
	nil
      (vm-set-extent-property button 'vm-mime-disposable t)
      (vm-set-extent-property button 'vm-mime-layout layout)
      (goto-char (vm-extent-start-position button))
      (vm-decode-mime-layout button t))))


(defun vm-mime-attach-body-to-composition (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout))
	(vm-mime-external-content-type-exceptions nil))
    (goto-char (vm-extent-start-position button))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (setq layout (car (vm-mm-layout-parts layout))))
    (vm-attach-object-to-composition layout)))

(defun vm-mime-get-button-layout ()
  "Return the MIME layout of the MIME button at point.   USR, 2011-03-07"
  (vm-mime-run-display-function-at-point
   (function
    (lambda (extent)
      (vm-extent-property extent 'vm-mime-layout)))))

(defun vm-mime-scrub-description (string)
  (let ((work-buffer nil))
      (save-excursion
       (unwind-protect
	   (progn
	     (setq work-buffer (vm-make-work-buffer))
	     (set-buffer work-buffer)
	     (insert string)
	     (while (re-search-forward "[ \t\n]+" nil t)
	       (replace-match " "))
	     (buffer-string))
	 (and work-buffer (kill-buffer work-buffer))))))

;; unused

(defun vm-mime-layout-contains-type (layout type)
  (if (vm-mime-types-match type (car (vm-mm-layout-type layout)))
      layout
    (let ((p (vm-mm-layout-parts layout))
	  (result nil)
	  (done nil))
      (while (and p (not done))
	(if (setq result (vm-mime-layout-contains-type (car p) type))
	    (setq done t)
	  (setq p (cdr p))))
      result )))

;; breadth first traversal
(defun vm-mime-find-digests-in-layout (layout)
  (let ((layout-list (list layout))
	layout-type
	(result nil))
    (while layout-list
      (setq layout-type (car (vm-mm-layout-type (car layout-list))))
      (cond ((string-match "^multipart/digest\\|message/\\(rfc822\\|news\\)"
			   layout-type)
	     (setq result (nconc result (list (car layout-list)))))
	    ((vm-mime-composite-type-p layout-type)
	     (setq layout-list (nconc layout-list
				      (copy-sequence
				       (vm-mm-layout-parts
					(car layout-list)))))))
      (setq layout-list (cdr layout-list)))
    result ))
  
(defun vm-mime-plain-message-p (m)
  "A message M is considered plain if
   - it does not have encoded headers, and
   - - it does not have a MIME layout, or
   - - it has a text/plain component as its first element with ASCII
   - -   character set and unibyte encoding (7bit, 8bit or binary).
Returns non-NIL value M is a plain message."
  (save-match-data
    (let ((o (vm-mm-layout m))
	  (case-fold-search t))
      (and (eq (vm-mm-encoded-header m) 'none)
	   (or (not (vectorp o))
	       (and (vm-mime-types-match "text/plain"
					 (car (vm-mm-layout-type o)))
		    (string-match "^us-ascii$"
				  (or (vm-mime-get-parameter o "charset")
				      "us-ascii"))
		    (string-match "^\\(7bit\\|8bit\\|binary\\)$"
				  (vm-mm-layout-encoding o))))))))

(defun vm-mime-text-type-p (type)
  (let ((case-fold-search t))
    (or (string-match "^text/" type) (string-match "^message/" type))))

(defun vm-mime-text-type-layout-p (layout)
  (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
      (vm-mime-types-match "message" (car (vm-mm-layout-type layout)))))


(defun vm-mime-charset-internally-displayable-p (_name)
  "Whether VM can display the MIME charset NAME inside Emacs.  Always.
Emacs shows a replacement character for what it cannot render, which is
better than sending the part to an external viewer.  It answered per
charset when it had to serve XEmacs on a tty as well."
  t)

(defun vm-mime-find-message/partials (layout id)
  (let ((list nil)
	(type (vm-mm-layout-type layout)))
    (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	   (let ((parts (vm-mm-layout-parts layout)) o)
	     (while parts
	       (setq o (vm-mime-find-message/partials (car parts) id))
	       (if o
		   (setq list (nconc o list)))
	       (setq parts (cdr parts)))))
	  ((vm-mime-types-match "message/partial" (car type))
	   (if (equal (vm-mime-get-parameter layout "id") id)
	       (setq list (cons layout list)))))
    list ))

(defun vm-mime-find-leaf-content-id-in-layout-folder (layout id)
  ;; `save-restriction' in the folder being widened, not in whatever buffer
  ;; the caller was in (#780).
  (with-current-buffer (vm-buffer-of
			(vm-real-message-of
			 (vm-mm-layout-message layout)))
    (save-excursion
      (save-restriction
	(let (m (o nil))
	  (widen)
	  (goto-char (point-min))
	  (while (and (search-forward id nil t)
		      (setq m (vm-message-at-point)))
	    (setq o (vm-mm-layout m))
	    (if (not (vectorp o))
		nil
	      (setq o (vm-mime-find-leaf-content-id o id))
	      (if (null o)
		  nil
		;; if we found it, end the search loop
		(goto-char (point-max)))))
	  o )))))

(defun vm-mime-find-leaf-content-id (layout id)
  (let (;; (list nil)
	)
    (catch 'done
      (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	     (let ((parts (vm-mm-layout-parts layout)) o)
	       (while parts
		 (setq o (vm-mime-find-leaf-content-id (car parts) id))
		 (if o
		     (throw 'done o))
		 (setq parts (cdr parts)))))
	    (t
	     (if (equal (vm-mm-layout-id layout) id)
		 (throw 'done layout)))))))

(defun vm-message-at-point ()
  (let ((mp vm-message-list)
	(point (point))
	(done nil))
    (while (and mp (not done))
      (if (and (>= point (vm-start-of (car mp)))
	       (<= point (vm-end-of (car mp))))
	  (setq done t)
	(setq mp (cdr mp))))
    (car mp)))

(defun vm-mime-make-multipart-boundary ()
  (let ((boundary (make-string 10 ?a))
	(i 0))
    (random t)
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
			     (random
			      (length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary ))

(defun vm-mime-extract-filename-suffix (layout)
  (let ((filename (vm-mime-get-disposition-filename layout))
	(suffix nil)) ;; i
    (if (and filename (string-match "\\.[^.]+$" filename))
	(setq suffix (substring filename (match-beginning 0) (match-end 0))))
    suffix ))

(defun vm-mime-find-filename-suffix-for-type (layout)
  (let ((type (car (vm-mm-layout-type layout)))
	suffix
	(alist vm-mime-attachment-auto-suffix-alist))
    (while alist
      (if (vm-mime-types-match (car (car alist)) type)
	  (setq suffix (cdr (car alist))
		alist nil)
	(setq alist (cdr alist))))
    suffix ))

;;;###autoload


(defun vm-attach-file (file type &optional charset description
			    _no-suggested-filename)
  "Attach a file to a VM composition buffer to be sent along with the message.
The file is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, FILE, is the name of the file to attach.  Second
argument, TYPE, is the MIME Content-Type of the file.  Optional
third argument CHARSET is the character set of the attached
document.  This argument is only used for text types, and it is
ignored for other types.  Optional fourth argument DESCRIPTION
should be a one line description of the file.  Nil means include
no description.  Optional fifth argument NO-SUGGESTED-FILENAME non-nil
means that VM should not add a filename to the Content-Disposition
header created for the object.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use `vm-attach-mime-file' to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
         (completion-ignored-extensions nil)
	 (charset nil)
	 description file default-type type)
     (unless vm-send-using-mime
	 (error (concat "MIME attachments disabled, "
			"set vm-send-using-mime non-nil to enable.")))
     (setq file (vm-read-file-name "Attach file: "
                                   vm-mime-attachment-source-directory
                                   nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 ;; prompt
		 (format "Content type (default %s): " default-type)
		 ;; collection
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (when (vm-mime-types-match "text" type)
       (setq charset (completing-read
		      ;; prompt
		      "Character set (default US-ASCII): "
		      ;; collection
		      vm-mime-charset-completion-alist)
	     charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list file type charset description nil)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when (file-directory-p file)
    (error "%s is a directory, cannot attach" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "You don't have permission to read %s" file))
  (when charset 
    (setq charset (list (concat "charset=" charset))))
  (when description 
    (setq description (vm-mime-scrub-description description)))
  (vm-attach-object file :type type :params charset 
			 :description description :mimed nil))
;;;###autoload (autoload 'vm-mime-attach-file "vm-mime" nil t)
(defalias 'vm-mime-attach-file 'vm-attach-file)

;;;###autoload
(defun vm-attach-mime-file (file type)
  "Attach a MIME encoded file to a VM composition buffer to be sent
along with the message.

The file is not inserted into the buffer until you execute
`vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag indicating
the existence of the attachment is placed in the composition
buffer.  You can move the attachment around or remove it entirely
with normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

The first argument, FILE, is the name of the file to attach.
When called interactively the FILE argument is read from the
minibuffer.

The second argument, TYPE, is the MIME Content-Type of the object.

This command is for attaching files that have a MIME
header section at the top.  For files without MIME headers, you
should use `vm-attach-file' to attach the file."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 file type default-type)
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (setq file (vm-read-file-name "Attach file: "
                                   vm-mime-attachment-source-directory
                                   nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 ;; prompt
		 (format "Content type (default %s): " default-type)
		 ;; collection
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (list file type)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when (file-directory-p file)
    (error "%s is a directory, cannot attach" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "You don't have permission to read %s" file))
  (vm-attach-object file :type type :params nil 
			 :description nil :mimed t))
;;;###autoload (autoload 'vm-mime-attach-mime-file "vm-mime" nil t)
(defalias 'vm-mime-attach-mime-file 'vm-attach-mime-file)

;;;###autoload
(defun vm-attach-buffer (buffer type &optional charset description)
  "Attach a buffer to a VM composition buffer to be sent along with
the message.

The buffer contents are not inserted into the composition
buffer and MIME encoded until you execute `vm-mail-send' or
`vm-mail-send-and-exit'.  A visible tag indicating the existence
of the attachment is placed in the composition buffer.  You
can move the attachment around or remove it entirely with
normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

First argument, BUFFER, is the buffer or name of the buffer to
attach.  Second argument, TYPE, is the MIME Content-Type of the
file.  Optional third argument CHARSET is the character set of
the attached document.  This argument is only used for text
types, and it is ignored for other types.  Optional fourth
argument DESCRIPTION should be a one line description of the
file.  Nil means include no description.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use `vm-attach-mime-file' to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (charset nil)
	 description default-type type buffer-name) ;; file buffer
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (setq buffer-name (read-buffer "Attach buffer: " nil t)
	   default-type (or (vm-mime-default-type-from-filename buffer-name)
			    "application/octet-stream")
	   type (completing-read
		 ;; prompt
		 (format "Content type (default %s): " default-type)
		 ;; collection
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (when (vm-mime-types-match "text" type)
       (setq charset (completing-read
		      ;; prompt
		      "Character set (default US-ASCII): "
		      ;; collection
		      vm-mime-charset-completion-alist)
	     charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list buffer-name type charset description)))
  (unless (setq buffer (get-buffer buffer))
    (error "Buffer %s does not exist." buffer))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when charset 
    (setq charset (list (concat "charset=" charset))))
  (when description 
    (setq description (vm-mime-scrub-description description)))
  (vm-attach-object buffer :type type :params charset
			 :description description :mimed nil))
;;;###autoload (autoload 'vm-mime-attach-buffer "vm-mime" nil t)
(defalias 'vm-mime-attach-buffer 'vm-attach-buffer)


;;;###autoload
(defun vm-attach-message (message &optional description)
  "Attach a message from a VM folder to the current VM
composition.

The message is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, MESSAGE, is either a VM message struct or a list
of message structs.  When called interactively a message number is read
from the minibuffer.  The message will come from the parent
folder of this composition.  If the composition has no parent,
the name of a folder will be read from the minibuffer before the
message number is read.

If this command is invoked with a prefix argument, the name of a
folder is read and that folder is used instead of the parent
folder of the composition.

If this command is invoked on marked message (via
`vm-next-command-uses-marks') the marked messages in the selected
folder will be attached as a MIME message digest.    If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are attached.

Optional second argument DESCRIPTION is a one-line description of
the message being attached.  This is also read from the
minibuffer if the command is run interactively."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (result 0)
	 mlist mp default prompt description folder)
     (unless (eq major-mode 'mail-mode)
       (error "Command must be used in a VM Mail mode buffer."))
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (when current-prefix-arg
       (setq vm-mail-buffer (vm-read-folder-name)
	     vm-mail-buffer (if (string= vm-mail-buffer "") nil
			      (setq current-prefix-arg nil)
			      (get-buffer vm-mail-buffer))))
     (cond ((or current-prefix-arg (null vm-mail-buffer)
		(not (buffer-live-p vm-mail-buffer)))
	    (let ((dir (if vm-folder-directory
			   (expand-file-name vm-folder-directory)
			 default-directory))
		  file)
	      (let ((last-command last-command)
		    (this-command this-command))
		(setq file (read-file-name "Attach message from folder: "
					   dir nil t)))
	      (let ((coding-system-for-read (vm-binary-coding-system)))
		(setq folder (find-file-noselect file)))
	      (with-current-buffer folder
		(vm-mode))))
	   (t
	    (setq folder vm-mail-buffer)))
     ;; Select marked messages if there were any
     (with-current-buffer folder
       (setq mlist (vm-select-operable-messages nil t "Attach"))
       (vm-inform 1 "Attaching %s messages from %s..." 
		  (length mlist) (buffer-name folder))
       (vm-retrieve-operable-messages 1 mlist :fail t))
     ;; Otherwise, ask the user
     (when (null mlist)
       (with-current-buffer folder
	 (setq default (and vm-message-pointer
			    (vm-number-of (car vm-message-pointer)))
	       prompt (if default
			  (format "Attach message number from %s: (default %s) "
				  (buffer-name folder) default)
			(format "Attach message number from %s: "
				(buffer-name folder))))
	 (while (zerop result)
	   (setq result (read-string prompt))
	   (and (string= result "") default (setq result default))
	   (setq result (string-to-number result)))
	 (when (null (setq mp (nthcdr (1- result) vm-message-list)))
	   (error "No such message."))))

     (setq description (read-string "Description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list (or mlist (car mp)) description)))

  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (cond ((not (consp message))
	 (vm-attach-message-internal message description))
	((null (cdr message))
	 (vm-attach-message-internal (car message) description))
	(t
	 (vm-attach-message-digest-internal message description))))

;;;###autoload (autoload 'vm-mime-attach-message "vm-mime" nil t)
(defalias 'vm-mime-attach-message 'vm-attach-message)

(defun vm-attach-message-internal (message description)
  "Attach MESSAGE as a mime object to the current composition.  Use
DESCRIPTION." 
  (let* ((work-buffer (vm-generate-new-unibyte-buffer "*attached message*"))
	 (m (vm-real-message-of message))
	 (folder (vm-buffer-of m)))
    (with-current-buffer work-buffer
      (vm-insert-region-from-buffer folder (vm-headers-of m) (vm-text-end-of m))
      (goto-char (point-min))
      (vm-reorder-message-headers
       nil :keep-list nil
       :discard-regexp vm-internal-unforwarded-header-regexp))
    (when description 
      (setq description (vm-mime-scrub-description description)))
    (vm-attach-object work-buffer 
		      :type "message/rfc822" :params nil 
		      :disposition '("inline")
		      :description description)
    (make-local-variable 'vm-forward-list)
    (setq vm-system-state 'forwarding
	  vm-forward-list (list message))
    ;; move window point forward so that if this command
    ;; is used consecutively, the insertions will be in
    ;; the correct order in the composition buffer.
    (let ((w (vm-get-buffer-window (current-buffer))))
      (when w (set-window-point w (point))))
    (add-hook 'kill-buffer-hook
	      `(lambda ()
		 (if (eq (current-buffer) ,(current-buffer))
		     (kill-buffer ,work-buffer))))))

(defun vm-attach-message-digest-internal (mlist description)
  "Attach MLIST as a mail digest object to the current composition.  Use
DESCRIPTION." 
  (let ((work-buffer (vm-generate-new-unibyte-buffer "*attached messages*"))
	boundary)
    (with-current-buffer work-buffer
      (setq boundary (vm-mime-encapsulate-messages
		      mlist :keep-list vm-mime-digest-headers
		      :discard-regexp vm-mime-digest-discard-header-regexp
		      :always-use-digest t))
      (goto-char (point-min))
      (insert "MIME-Version: 1.0\n")
      (insert "Content-Type: "
	      (vm-mime-type-with-params 
	       "multipart/digest" (list (concat "boundary=\"" boundary "\"")))
	      "\n")
      (insert "Content-Transfer-Encoding: "
	      (vm-determine-proper-content-transfer-encoding
	       (point) (point-max))
	      "\n\n"))
    (when description 
      (setq description (vm-mime-scrub-description description)))
    (vm-attach-object work-buffer :type "multipart/digest"
		      :params (list (concat "boundary=\"" boundary "\"")) 
		      :disposition '("inline")
		      :description description :mimed t)
    (make-local-variable 'vm-forward-list)
    (setq vm-system-state 'forwarding
	  vm-forward-list (copy-sequence mlist))
    ;; move window point forward so that if this command
    ;; is used consecutively, the insertions will be in
    ;; the correct order in the composition buffer.
    (let ((w (vm-get-buffer-window (current-buffer))))
      (when w (set-window-point w (point))))
    (add-hook 'kill-buffer-hook
	      `(lambda ()
		 (if (eq (current-buffer) ,(current-buffer))
		     (kill-buffer ,work-buffer))))))
;;;###autoload
(defun vm-attach-message-to-composition (composition &optional description)
  "Attach the current message from the current VM folder to a VM
composition.

The message is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer.

If this command is invoked on marked message (via
`vm-next-command-uses-marks') the marked messages in the selected
folder will be attached as a MIME message digest.    If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are attached.

Optional second argument DESCRIPTION is a one-line description of
the message being attached.  This is also read from the
minibuffer if the command is run interactively."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 description)
     (save-current-buffer
       (vm-select-folder-buffer-and-validate 1 t)
       (unless (memq major-mode '(vm-mode vm-virtual-mode))
	 (error "Command must be used in a VM buffer."))
       (unless vm-send-using-mime
	 (error (concat "MIME attachments disabled, "
			"set vm-send-using-mime non-nil to enable.")))
       (list
	(read-buffer "Attach object to buffer: " (vm-find-composition-buffer) t)
	(progn (setq description (read-string "Description: "))
	       (when (string-match "^[ \t]*$" description)
		 (setq description nil))
	       description)))))

  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-follow-summary-cursor)

  (let ((mlist (vm-select-operable-messages 1 t "Attach")))
    (vm-retrieve-operable-messages 1 mlist :fail t)

    (with-current-buffer composition
    (if (null (cdr mlist))		; single message
	(vm-attach-message-internal (car mlist) description)
      (vm-attach-message-digest-internal mlist description)))))
;;;###autoload (autoload 'vm-mime-attach-message-to-composition "vm-mime" nil t)
(defalias 'vm-mime-attach-message-to-composition
  'vm-attach-message-to-composition)
		      
;;;###autoload
(defun vm-attach-object-to-composition (layout &optional composition)
  "Attach the mime object described by LAYOUT to a VM composition buffer.

The object is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the object is placed in the
composition buffer.  You can move the object around or remove
it entirely with normal text editing commands.  If you remove the
object tag, the object will not be sent.

The optional argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer."
  (unless composition
    (setq composition (read-buffer "Attach object to buffer: "
				   (vm-find-composition-buffer) t)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)

  (let ((work-buffer (vm-make-work-buffer)) 
	buf start w)
    (unwind-protect
	(with-current-buffer work-buffer
	  (vm-mime-insert-mime-headers layout)
	  (insert "\n")
	  (setq start (point))
	  (vm-mime-insert-mime-body layout)
	  (vm-mime-transfer-decode-region layout start (point-max))
	  (goto-char (point-min))
	  (vm-reorder-message-headers 
	   nil :keep-list nil :discard-regexp "Content-Transfer-Encoding:")
	  (insert "Content-Transfer-Encoding: binary\n")
	  (set-buffer composition)
	  ;; Append.  vm-attach-object inserts at point, which is right
	  ;; for vm-attach-file -- the user put it there -- but here the
	  ;; composition is a buffer we have just been named, whose point
	  ;; is wherever it was last left, quite possibly in the middle of
	  ;; what the user was typing.
	  (goto-char (point-max))
	  ;; FIXME need to copy the disposition from the original
	  (vm-attach-object work-buffer
			    :type (car (vm-mm-layout-type layout)) 
			    :params (cdr (vm-mm-layout-type layout))
			    :description (vm-mm-layout-description 
					  layout)
			    :mimed t)
	  ;; move window point forward so that if this command
	  ;; is used consecutively, the insertions will be in
	  ;; the correct order in the composition buffer.
	  (setq w (vm-get-buffer-window composition))
	  (and w (set-window-point w (point)))
	  (setq buf work-buffer
		work-buffer nil)	; schedule to be killed later
	  (add-hook 'kill-buffer-hook
		    `(lambda ()
		       (if (eq (current-buffer) ,(current-buffer))
			   (kill-buffer ,buf))))
	  )
      ;; unwind-protection
      (when work-buffer (kill-buffer work-buffer)))))
(defalias 'vm-mime-attach-object-to-composition
  'vm-attach-object-to-composition)


(cl-defun vm-attach-object (object &key type params description 
				      (mimed nil)
				      (disposition '("unspecified"))
				      (no-suggested-filename nil))
  "Attach a MIME OBJECT to the mail composition in the current
buffer.  The OBJECT could be:
  - the full path name of a file
  - a buffer, or
  - a list with the elements: buffer, start position, end position,
    disposition and optional file name.
TYPE, PARAMS and DESCRIPTION and DISPOSITION are the standard MIME
properties. 
MIMED says whether the OBJECT already has MIME headers.
Optional argument NO-SUGGESTED-FILENAME is a boolean indicating that
there is no file name for this object.             USR, 2011-03-07"
  (unless (eq major-mode 'mail-mode)
    (error "VM internal error: vm-attach-object not in Mail mode buffer."))
  (when (vm-mail-mode-get-header-contents "MIME-Version")
    (error "Can't attach MIME object to already encoded MIME buffer."))
  (let (start end tag-string file-name
	;; Forward references to external-body parts when
	;; vm-mime-forward-saved-attachments is nil; otherwise expand them
	(fb (list (not vm-mime-forward-saved-attachments))))
    (cond ((and (stringp object) (not mimed))
	   (if (or (vm-mime-types-match "application" type)
		   (vm-mime-types-match "model" type))
	       (setq disposition (list "attachment"))
	     (setq disposition (list "inline")))
	   (unless no-suggested-filename
	     (setq file-name (file-name-nondirectory object))
	     ;; why fuse things together?  USR, 2011-03-17
	     (setq params
		   (append params
			   (list (vm-mime-encode-parameter "name" file-name))))
	     (setq disposition
		   (nconc disposition
			  (list (vm-mime-encode-parameter
				 "filename" file-name))))))
	  ((listp object) 
	   (setq file-name (nth 4 object))
	   (setq disposition (nth 3 object)))
	  (t
	   (setq file-name 
		 (or (vm-mime-get-xxx-parameter "name" params)
		     (vm-mime-get-xxx-parameter "filename" params)))))
    (when (< (point) (save-excursion (mail-text) (point)))
      (mail-text))
    (setq start (point))
    (setq tag-string (format "[ATTACHMENT %s, %s]" 
			     (or file-name description "") 
			     (or type "MIME file")))
    (insert tag-string "\n")
    (setq end (1- (point)))


    (put-text-property start end 'front-sticky nil)
    (put-text-property start end 'rear-nonsticky t)
    ;; can't be intangible because menu clicking at a position
    ;; needs to set point inside the tag so that a command can
    ;; access the text properties there.
    (put-text-property start end 'face vm-attachment-button-face)
    (put-text-property start end 'font-lock-face vm-attachment-button-face)
    (put-text-property start end 'mouse-face vm-attachment-button-mouse-face)
    (put-text-property start end 'vm-mime-forward-local-refs fb)
    (put-text-property start end 'vm-mime-type type)
    (put-text-property start end 'vm-mime-object object)
    (put-text-property start end 'vm-mime-parameters params)
    (put-text-property start end 'vm-mime-description description)
    (put-text-property start end 'vm-mime-disposition disposition)
    (put-text-property start end 'vm-mime-encoding (list nil))
    (put-text-property start end 'vm-mime-encoded mimed)))

(defalias 'vm-mime-attach-object 'vm-attach-object)

(defun vm-mime-attachment-forward-local-refs-at-point ()
  (car (get-text-property (point) 'vm-mime-forward-local-refs)))

(defun vm-mime-set-attachment-forward-local-refs-at-point (val)
  (setcar (get-text-property (point) 'vm-mime-forward-local-refs) val))

;; vm-mime-delete-attachment-button and
;; vm-mime-delete-attachment-button-keep-infos were removed along with the
;; attachment-menu entries that called them: the GNU Emacs arm of each was
;; an empty placeholder, so on GNU Emacs they did nothing at all.  See #552
;; for reimplementing them; vm-mime-attachment-tag-bounds gives the region
;; the XEmacs versions got from the extent.  C-k on the tag deletes an
;; attachment in the meantime, as the manual says.

(defun vm-mime-set-parameter-in-list (params key value)
  "Return PARAMS with KEY set to VALUE, adding it if it is not there.
PARAMS is a list of \"key=value\" strings as carried by the
`vm-mime-parameters' and `vm-mime-disposition' properties of an
attachment tag."
  (let ((entry (vm-mime-encode-parameter key value))
	(regexp (vm-mime-parameter-name-regexp key))
	(found nil)
	(result nil))
    (dolist (param params)
      (if (and (stringp param) (string-match regexp param))
	  ;; Replace the first spelling of KEY and drop any other, so that a
	  ;; value set here cannot be overridden by a leftover NAME*= or by
	  ;; the remaining segments of a continuation.
	  (unless found
	    (setq found t)
	    (setq result (cons entry result)))
	(setq result (cons param result))))
    (setq result (nreverse result))
    (if found result (append result (list entry)))))

(defun vm-mime-attachment-tag-bounds ()
  "Return (START . END) for the attachment tag at point, or nil.
Point counts as being on the tag when it is just past its closing
bracket, which is where `end-of-line' leaves it."
  (let ((pos (cond ((get-text-property (point) 'vm-mime-type) (point))
		   ((and (> (point) (point-min))
			 (get-text-property (1- (point)) 'vm-mime-type))
		    (1- (point))))))
    (when pos
      (cons (or (previous-single-property-change
		 (min (1+ pos) (point-max)) 'vm-mime-type)
		(point-min))
	    (or (next-single-property-change pos 'vm-mime-type)
		(point-max))))))

(defun vm-mime-unquote-parameter-value (value)
  "Return VALUE with MIME parameter quoting removed."
  (if (and value (string-match "\\`\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\'" value))
      (vm-replace-in-string (match-string 1 value) "\\\\\\(.\\)" "\\1")
    value))

(defun vm-mime-attachment-name-at-point ()
  "Return the file name of the attachment at point, or nil.
Any MIME parameter quoting is removed."
  ;; through the bounds, so that point just past the tag counts as on it
  (let* ((pos (car (vm-mime-attachment-tag-bounds)))
	 (disposition (and pos (get-text-property pos 'vm-mime-disposition)))
	 (params (and pos (get-text-property pos 'vm-mime-parameters))))
    (vm-mime-unquote-parameter-value
     (or (vm-mime-get-xxx-parameter "filename" (cdr disposition))
	 (vm-mime-get-xxx-parameter "name" params)))))

(defun vm-mime-set-attachment-name-at-point (name)
  "Give the attachment at point the file NAME.
Sets it in both the Content-Type name parameter and the
Content-Disposition filename parameter, and updates the visible tag."
  (let ((bounds (vm-mime-attachment-tag-bounds)))
    (unless bounds (error "No attachment here"))
    (let* ((start (car bounds))
	     (end (cdr bounds))
	     (inhibit-read-only t)
	     ;; the whole tag carries one set of properties; work on a
	     ;; copy and put it back over the tag once the text is right
	     (props (copy-sequence (text-properties-at start)))
	     (disposition (plist-get props 'vm-mime-disposition)))
	(setq props (plist-put props 'vm-mime-parameters
			       (vm-mime-set-parameter-in-list
				(plist-get props 'vm-mime-parameters)
				"name" name)))
	(setq props (plist-put props 'vm-mime-disposition
			       (cons (car disposition)
				     (vm-mime-set-parameter-in-list
				      (cdr disposition) "filename" name))))
	(save-excursion
	  (save-restriction
	    ;; Narrow to this tag.  The name is matched greedily, because
	    ;; it may itself contain a comma, and `looking-at' would
	    ;; otherwise run past the end of the tag: two tags sharing a
	    ;; line -- which happens as soon as the user joins them --
	    ;; and the match would swallow the second one's text while
	    ;; leaving its properties orphaned in the buffer.
	    (narrow-to-region start end)
	    (goto-char start)
	    ;; The tag reads "[ATTACHMENT <name>, <type>]".
	    (when (looking-at "\\[ATTACHMENT \\(.*\\), [^,]*\\]\\'")
	      (let ((name-start (match-beginning 1))
		    (name-end (match-end 1)))
		(setq end (+ end (- (length name) (- name-end name-start))))
		(delete-region name-start name-end)
		(goto-char name-start)
		;; Not insert-and-inherit: the tag is rear-nonsticky, so
		;; inserted text inherits nothing and the tag's property
		;; run would be split in three -- which the encoder reads
		;; as two attachments where there is one.
		(insert name)))))
      (set-text-properties start end props))))

;;;###autoload
(defun vm-mime-rename-attachment ()
  "Give the attachment at point a different file name.
The name is the one the recipient sees, and the one their mailer will
suggest when they save it; the file the attachment was read from is not
touched."
  (interactive)
  (let ((current (vm-mime-attachment-name-at-point)))
    (unless (vm-mime-attachment-tag-bounds)
      (error "No attachment here"))
    (vm-mime-set-attachment-name-at-point
     (read-string "Attachment file name: " current))))

;;;###autoload
(defun vm-mime-change-content-disposition ()
  "Change the disposition of the attachment at point in this composition.
Reads `inline\', `attachment\' or `unspecified\'.  The disposition tells the
recipient\'s mail reader whether the part is meant to be shown as part of
the message or offered as a file to save; `unspecified\' sends no
Content-Disposition header and leaves the choice to them."
  (interactive)
  ;; before the prompt, as `vm-mime-rename-attachment' does: an answer read
  ;; and then thrown away is worse than the question not being asked
  (unless (vm-mime-attachment-tag-bounds)
    (error "No attachment here"))
  (vm-mime-set-attachment-disposition-at-point
   (intern
    (completing-read 
     ;; prompt
     "Disposition-type: "
     ;; collection
     '(("unspecified") ("inline") ("attachment"))
     ;; predicate, require-match
     nil t))))

(defun vm-mime-attachment-disposition-at-point ()
  (intern (car (get-text-property (point) 'vm-mime-disposition))))

(defun vm-mime-set-attachment-disposition-at-point (sym)
  (setcar (get-text-property (point) 'vm-mime-disposition)
	  (symbol-name sym)))


(defun vm-mime-attachment-encoding-at-point ()
  (car (get-text-property (point) 'vm-mime-encoding)))

(defun vm-mime-set-attachment-encoding-at-point (sym)
  (setcar (get-text-property (point) 'vm-mime-encoding) sym))

(defun vm-mime-attachment-button-extents (start end &optional prop)
  "Return the extents of all attachment buttons in the region.  Optional
argument PROP can specify an extent property, in which case only those
extents that have the property are returned.

Attachment buttons are denoted by text properties rather than by overlays,
so the overlays this returns are made for the purpose.  USR, 2011-03-27"
  (let ((e-list (vm-mime-fake-attachment-overlays start end prop)))
    (sort e-list (function
		  (lambda (e1 e2)
		    (< (vm-extent-end-position e1)
		       (vm-extent-end-position e2)))))))

(defun vm-mime-fake-attachment-overlays (start end &optional prop)
  "For all attachment buttons in the region, i.e., pieces of text
with the given text property PROP, create \"fake\" attachment
overlays with the `vm-mime-object' property.  The list of these
overlays is returned.

This function is only used with GNU Emacs, not XEmacs.  USR, 2011-02-19"
  ;; This round about method is being used because in GNU Emacs,
  ;; only text properties are preserved under killing and yanking.
  ;; So, text properties are normally used for attachment buttons and
  ;; converted to overlays just before MIME encoding.  USR, 2011-02-19
  (when (null prop) (setq prop 'vm-mime-object))
  (let ((o-list nil)
	(done nil)
	(pos start)
	object props o)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while (not done)
	  (setq object (get-text-property pos prop))
	  (setq pos (next-single-property-change pos prop))
	  (unless pos 
	    (setq pos (point-max) 
		  done t))
	  (when object
	    (setq o (make-overlay start pos nil t nil))
	    (setq props (text-properties-at start))
	    (unless (eq prop 'vm-mime-object)
	      (setq props (append (list 'vm-mime-object t) props)))
	    (while props
	      (overlay-put o (car props) (cadr props))
	      (setq props (cddr props)))
	    (setq o-list (cons o o-list)))
	  (setq start pos))
	o-list ))))

(defun vm-mime-default-type-from-filename (file)
  "The MIME type FILE's name suggests, or nil.

`vm-mime-attachment-auto-type-alist' first, so what the user has set there
decides.  Failing that, `mailcap-extension-to-mime', which knows the
system's /etc/mime.types and Emacs's own table: an .org file is text/x-org
there and a .patch text/x-patch, and either is better than the
application/octet-stream the callers fall back to -- octet-stream carries no
charset, so text sent as one arrives as a download rather than as text."
  (let ((alist vm-mime-attachment-auto-type-alist)
	(case-fold-search t)
	(done nil))
    (while (and alist (not done))
      (if (string-match (car (car alist)) file)
	  (setq done t)
	(setq alist (cdr alist))))
    (or (and alist (cdr (car alist)))
	(vm-mime-type-from-mailcap file))))

(declare-function mailcap-parse-mimetypes "mailcap" (&optional path force))
(declare-function mailcap-extension-to-mime "mailcap" (extn))

(defun vm-mime-type-from-mailcap (file)
  "The MIME type Emacs's mailcap tables give FILE's suffix, or nil."
  (let ((extension (file-name-extension file)))
    (when extension
      (require 'mailcap)
      (mailcap-parse-mimetypes)
      (mailcap-extension-to-mime extension))))

(defun vm-remove-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mail-header-separator "$") nil t)
	(progn
	  (delete-region (match-beginning 0) (match-end 0))
	   t )
      nil )))

(defun vm-add-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^$" nil t)
	(replace-match mail-header-separator t t))))

(defun vm-mime-transfer-encode-region (encoding beg end crlf)
  "Encode region between BEG and END using transfer ENCODING (base64,
quoted-printable or binary).  CRLF says whether carriage returns
should be included (?)                               USR, 2011-03-27"
  (let ((case-fold-search t)
	(armor-from (and vm-mime-composition-armor-from-lines
			 (let ((case-fold-search nil))
			   (save-excursion
			     (goto-char beg)
			     (re-search-forward "^From " nil t)))))
	(armor-dot (let ((case-fold-search nil))
		     (save-excursion
		       (goto-char beg)
		       (re-search-forward "^\\.\n" nil t)))))
    (cond ((string-match "^binary$" encoding)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((equal encoding vm-mime-long-lines-encoding)
	   ;; Quoted-printable rather than base64: it carries the long line
	   ;; just as exactly, and leaves the rest of the part readable to
	   ;; anyone looking at the message as it was sent.
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable"))
	  ((and (not armor-from) (not armor-dot)
	        (string-match "^7bit$" encoding))
	   t)
	  ((string-match "^base64$" encoding) t)
	  ((string-match "^quoted-printable$" encoding) t)
	  ((eq vm-mime-8bit-text-transfer-encoding 'quoted-printable)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable"))
	  ((eq vm-mime-8bit-text-transfer-encoding 'base64)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((or armor-from armor-dot)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable")))
    (downcase encoding) ))

(defun vm-mime-transfer-encode-layout (layout)
  "Encode a MIME object described by LAYOUT in transfer encoding (base64,
quoted-printable or binary).                            USR, 2011-03-27"
  (let ((list (vm-mm-layout-parts layout))
	(type (car (vm-mm-layout-type layout)))
	(encoding "7bit")
	(vm-mime-8bit-text-transfer-encoding
	 vm-mime-8bit-text-transfer-encoding))
  (cond ((vm-mime-composite-type-p type)
	 ;; MIME messages of type "message" and
	 ;; "multipart" are required to have a non-opaque
	 ;; content transfer encoding.  This means that
	 ;; if the user only wants to send out 7bit data,
	 ;; then any subpart that contains 8bit data must
	 ;; have an opaque (qp or base64) 8->7bit
	 ;; conversion performed on it so that the
	 ;; enclosing entity can use a non-opaque
	 ;; encoding.
	 ;;
	 ;; message/partial requires a "7bit" encoding so
	 ;; force 8->7 conversion in that case.
	 (cond ((memq vm-mime-8bit-text-transfer-encoding
		      '(quoted-printable base64))
		t)
	       ((vm-mime-types-match "message/partial" type)
		(setq vm-mime-8bit-text-transfer-encoding
		      'quoted-printable)))
	 (while list
	   (if (equal (vm-mime-transfer-encode-layout (car list)) "8bit")
	       (setq encoding "8bit"))
	   (setq list (cdr list))))
	(t
	 (when (and (vm-mime-types-match "message/partial" type)
		    (not (memq vm-mime-8bit-text-transfer-encoding
			       '(quoted-printable base64))))
	   (setq vm-mime-8bit-text-transfer-encoding 'quoted-printable))
	 ;; Encode charset to bytes before transfer encoding, but only if
	 ;; the content is not already transfer-encoded
	 (let ((current-encoding (downcase (vm-mm-layout-encoding layout))))
	   (when (and (member current-encoding '("7bit" "8bit" "binary"))
		      (vm-mime-text-type-layout-p layout))
	     (let ((charset (vm-mime-get-parameter layout "charset")))
	       (when charset
		 (let ((coding-system (vm-mime-charset-to-coding charset)))
		   (unless coding-system
		     (error "Can't find a coding system for charset %s"
			    charset))
		   (encode-coding-region
		    (vm-mm-layout-body-start layout)
		    (vm-mm-layout-body-end layout)
		    coding-system))))))
	 (setq encoding
	       (vm-mime-transfer-encode-region (vm-mm-layout-encoding layout)
					       (vm-mm-layout-body-start layout)
					       (vm-mm-layout-body-end layout)
					       (vm-mime-text-type-layout-p
						layout)))))
  ;; seems redundant because an encoding can never be equal to a type.
  ;; but it wasn't meant to be encoding becuase it woundn't be a list.
  ;; who knows that is supposed to be?  USR, 2011-03-27
  (unless (equal encoding (downcase (car (vm-mm-layout-type layout))))
      (save-excursion
	(save-restriction
	  (goto-char (vm-mm-layout-header-start layout))
	  (narrow-to-region (point) (vm-mm-layout-header-end layout))
	  (vm-reorder-message-headers 
	   nil :keep-list nil :discard-regexp "Content-Transfer-Encoding:")
	  (if (not (equal encoding "7bit"))
	      (insert "CONTENT-TRANSFER-ENCODING: " encoding "\n"))
	  encoding )))))

(defun vm-mime-text-description (start _end)
  (save-excursion
    (goto-char start)
    (if (looking-at "[ \t\n]*-- \n")
	".signature"
      (if (re-search-forward "^-- \n" nil t)
	  "message body and .signature"
	"message body text"))))
;; tried this but random text in the object tag does't look right.

;;;###autoload
(defun vm-delete-mime-object (&optional saved-file)
  "Delete the contents of the MIME object at point.
The MIME object is replaced by a text/plain object that briefly
describes what was deleted."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (vm-error-if-folder-read-only)
  (when (and (vm-virtual-message-p (car vm-message-pointer))
	     (null (vm-virtual-messages-of (car vm-message-pointer))))
    (error "Can't edit unmirrored virtual messages."))
  (when vm-presentation-buffer
    (set-buffer vm-presentation-buffer))
  (let (layout label)
    (let ((e (vm-extent-at (point) 'vm-mime-layout)))
      (if (null e)
	  (error "No MIME button found at point.")
	(setq layout (vm-extent-property e 'vm-mime-layout))
	(when (and (vm-mm-layout-message layout)
		   (eq layout (vm-mime-layout-of
			       (vm-mm-layout-message layout))))
	  (error (concat "Can't delete the only MIME object; "
			 "use vm-delete-message instead.")))
	(when vm-mime-confirm-delete
	  (unless (y-or-n-p (vm-mime-sprintf "Delete %t? " layout))
	    (error "Aborted")))
	(vm-mime-discard-layout-contents layout saved-file)
	(let ((inhibit-read-only t)
	      opos
	      (buffer-read-only nil))
	  (save-excursion
	    (save-restriction
	     (goto-char (vm-extent-start-position e))
	     (setq opos (point))
	     (setq label (vm-mime-sprintf 
			  vm-mime-deleted-object-label layout))
	     (insert label)
	     (delete-region (point) (vm-extent-end-position e))
	     (vm-set-extent-endpoints e opos (point)))))
	))
    (when (vm-interactive-p)
      ;; make the change visible and place the cursor behind the removed object
      (vm-discard-cached-data)
      (when vm-presentation-buffer
        (set-buffer vm-presentation-buffer)
        (re-search-forward (regexp-quote label) (point-max) t)))))

(defun vm-mime-discard-layout-contents (layout &optional file)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (m (vm-mm-layout-message layout))
	  newid new-layout)
      (when (null m)
	(error "Message body not loaded"))
      (set-buffer (vm-buffer-of m))
      (when (and (markerp (vm-mm-layout-body-start layout))
		 (not (eq (marker-buffer (vm-mm-layout-body-start layout))
			  (current-buffer))))
	(error "MIME body is not in the message"))
      (save-restriction
	(widen)
	(if (vm-mm-layout-is-converted layout)
	    (setq layout (vm-mm-layout-unconverted-layout layout)))
	(goto-char (vm-mm-layout-header-start layout))
	(cond ((null file)
	       (insert "Content-Type: text/plain; charset=us-ascii\n\n")
	       (vm-set-mm-layout-body-start layout (point-marker))
	       (insert (vm-mime-sprintf vm-mime-deleted-object-label layout)))
	      (t
	       (insert "Content-Type: message/external-body; access-type=local-file; name=\"" file "\"\n")
	       (insert "Content-Transfer-Encoding: 7bit\n\n")
	       (insert "Content-Type: " 
		       (vm-mime-type-with-params
			(car (vm-mm-layout-qtype layout))
			(cdr (vm-mm-layout-qtype layout)))
		       "\n")
	       (if (vm-mm-layout-qdisposition layout)
		   (let ((p (vm-mm-layout-qdisposition layout)))
		     (insert "Content-Disposition: "
			     (mapconcat 'identity p "; ")
			     "\n")))
	       (if (vm-mm-layout-id layout)
		   (insert "Content-ID: " (vm-mm-layout-id layout) "\n")
		 (setq newid (vm-make-message-id))
		 (insert "Content-ID: " newid "\n"))
	       (insert "Content-Transfer-Encoding: binary\n\n")
	       (insert "[Deleted " (vm-mime-sprintf "%d]\n" layout))
	       (insert "[Saved to " file " on " (system-name) "]\n")))
	(delete-region (point) (vm-mm-layout-body-end layout))
	(vm-set-edited-flag-of m t)
	(vm-set-byte-count-of m nil)
	(vm-set-line-count-of m nil)
	(vm-set-stuff-flag-of m t)
	;; For the dreaded mboxcl2 folders recompute
	;; the message length and make a new Content-Length header.
	(if (eq (vm-message-type-of m) 'mboxcl2)
	    (let (length)
	      (goto-char (vm-headers-of m))
	      ;; first delete all copies of Content-Length
	      (while (and (re-search-forward vm-content-length-search-regexp
					     (vm-text-of m) t)
			  (null (match-beginning 1))
			  (progn (goto-char (match-beginning 0))
				 (vm-match-header vm-content-length-header)))
		(delete-region (vm-matched-header-start)
			       (vm-matched-header-end)))
	      ;; now compute the message body length
	      (setq length (- (vm-end-of m) (vm-text-of m)))
	      ;; insert the header
	      (goto-char (vm-headers-of m))
	      (insert vm-content-length-header " "
		      (int-to-string length) "\n")))
	;; make sure we get the summary updated.  The 'edited'
	;; flag might already be set and therefore trying to set
	;; it again might not have triggered an update.  We need
	;; the update because the message size has changed.
	(vm-mark-for-summary-update (vm-mm-layout-message layout))
	(cond (file
	       (save-restriction
		 (narrow-to-region (vm-mm-layout-header-start layout)
				   (vm-mm-layout-body-end layout))
		 (setq new-layout (vm-mime-parse-entity-safe))
		 (vm-set-mm-layout-message-symbol
		  new-layout (vm-mm-layout-message-symbol layout))
		 (vm-mime-copy-layout new-layout layout)))
	      (t
	       (vm-set-mm-layout-type layout '("text/plain"))
	       (vm-set-mm-layout-qtype layout '("text/plain"))
	       (vm-set-mm-layout-encoding layout "7bit")
	       (vm-set-mm-layout-id layout nil)
	       (vm-set-mm-layout-description
		layout
		(vm-mime-sprintf "Deleted %d" layout))
	       (vm-set-mm-layout-disposition layout nil)
	       (vm-set-mm-layout-qdisposition layout nil)
	       (vm-set-mm-layout-parts layout nil)
	       (vm-set-mm-layout-display-error layout nil)))))))

(defconst vm-mime-encoded-word-limit 75
  "The longest an encoded word may be, from RFC 2047 section 2.
Counting the whole of it: the charset, the encoding letter, the question
marks and the payload.  A run of text too long to fit becomes several
encoded words in a row, which is what the standard says to do.")

(defconst vm-mime-header-line-limit 78
  "The longest a header line should be, from RFC 5322 section 2.1.1.
That section also sets a limit of 998 that a line MUST NOT exceed.  This is
the smaller, softer one, which VM aims for by folding; a header with a single
unbreakable run longer than this exceeds it and cannot be helped.")

(defun vm-mime-encoded-word-payload (text coding encoding)
  "TEXT encoded for the body of an RFC 2047 word, without the wrapper.
CODING is the coding system for the charset, ENCODING the symbol `Q' or `B'."
  (with-temp-buffer
    (insert text)
    (when (and coding (not (eq coding 'no-conversion)))
      ;; encode-coding-region and not vm-encode-coding-region, which encodes
      ;; this wrongly
      (encode-coding-region (point-min) (point-max) coding))
    ;; A marker that advances: Q-encoding expands the text it encodes, and
    ;; `vm-mime-Q-encode-region' turns the spaces into underscores afterwards
    ;; over the region it was given.  A plain position taken before the call
    ;; is short by then, and the tail of the word keeps a space, which ends
    ;; the encoded word where it stands.
    (let ((end (copy-marker (point-max) t)))
      (if (eq encoding 'Q)
	  (vm-mime-Q-encode-region (point-min) end)
	;; B-encoding, so that no line break is inserted: a break inside an
	;; encoded word would end it.
	(vm-mime-base64-encode-region (point-min) end nil t))
      (set-marker end nil))
    (buffer-string)))

(defun vm-mime-encoded-word (text charset coding encoding)
  "TEXT as one whole RFC 2047 encoded word."
  (concat "=?" charset "?" (format "%s" encoding) "?"
	  (vm-mime-encoded-word-payload text coding encoding)
	  "?="))

(defun vm-mime-encoded-word-budget (charset encoding)
  "How many characters of payload an encoded word for CHARSET has room for."
  (- vm-mime-encoded-word-limit
     (length (vm-mime-encoded-word "" charset nil encoding))))

(defun vm-mime-split-for-encoded-words (text charset coding encoding)
  "TEXT split into the pieces that each fit in one encoded word.
Split between characters, never inside one: a piece is encoded on its own, so
a multibyte character cut in half would encode as two invalid ones.

A piece that still does not fit is a single character whose encoding is
longer than the budget, which no split can help; it is passed through whole
rather than dropped."
  (let ((budget (vm-mime-encoded-word-budget charset encoding))
	(pieces nil)
	(piece "")
	(piece-length 0))
    (dolist (char (string-to-list text))
      (let* ((one (char-to-string char))
	     (cost (length (vm-mime-encoded-word-payload one coding encoding))))
	(when (and (> piece-length 0) (> (+ piece-length cost) budget))
	  (push piece pieces)
	  (setq piece "" piece-length 0))
	(setq piece (concat piece one)
	      piece-length (+ piece-length cost))))
    (when (> (length piece) 0)
      (push piece pieces))
    (nreverse pieces)))

(defun vm-mime-encoded-words (text charset coding encoding)
  "TEXT as one or more RFC 2047 encoded words, none over the limit.
Several of them are written next to each other, separated by a space.  That
is lossless: RFC 2047 section 6.2 has a decoder drop the whitespace between
two adjacent encoded words, so the text comes back as it went in.  It is also
where a folder may break the line."
  (mapconcat (lambda (piece) (vm-mime-encoded-word piece charset coding encoding))
	     (vm-mime-split-for-encoded-words text charset coding encoding)
	     " "))

(defun vm-mime-encode-words (&optional encoding)
  "MIME encode all words in the current buffer.
The optional argument ENCODING can be the symbol `Q' or `B' (for
quoted-printable and base64 respectively).
If none is specified, quoted-printable is used."
  (goto-char (point-min))

  ;; find right encoding 
  (setq encoding (or encoding vm-mime-encode-headers-type))
  (save-excursion
    (when (stringp encoding)
      (setq encoding 
            (if (re-search-forward encoding (point-max) t)
                'B
              'Q))))
  ;; now encode the words 
  (let ((case-fold-search nil)
        start end charset coding)
    (while (re-search-forward vm-mime-encode-headers-words-regexp (point-max) t)
      (setq start (match-beginning 1)
            end   (copy-marker (match-end 0) t)
            charset (vm-determine-proper-charset start end)
            coding (vm-mime-charset-to-coding charset))
      ;; One encoded word where the run fits in one, several in a row where it
      ;; does not: RFC 2047 puts a limit of 75 on each (emacs-vm/vm#794).
      (let ((words (vm-mime-encoded-words
		    (buffer-substring-no-properties start end)
		    charset coding encoding)))
	;; Insert first and delete after, not the other way about.  A marker
	;; sitting at the end of this run -- `body-start' in
	;; `vm-mime-encode-headers' is one -- collapses to START when the
	;; region under it is deleted, and an insertion there does not carry
	;; it along, its insertion type being nil.  Inserting first pushes it
	;; past the new text, and deleting the old text then leaves it exactly
	;; at the end of the new.
	(goto-char start)
	(insert words)
	;; END advances, so the insertion above has already carried it past the
	;; new text: what is left between point and it is the old text.
	(delete-region (point) end))
      (set-marker end nil))))

;;;###autoload
(defun vm-mime-encode-words-in-string (string &optional _encoding)
  (and string
       (vm-with-string-as-temp-buffer 
	(vm-substring-no-properties string 0)
	'vm-mime-encode-words)))

(defun vm-mime-encode-headers ()
  "Encodes the headers of a message.

Only the words containing a non-ASCII characters are encoded, but
not the whole header as this will cause trouble for the
recipient and author headers.

Whitespace between encoded words is trimmed during decoding and thus those
should be encoded together.

A run too long for one encoded word becomes several in a row, RFC 2047
allowing 75 characters each, and a header line longer than
`vm-mime-header-line-limit' is folded at whitespace.  Both are undone by the
reader: a folded line is joined back up, and the whitespace between two
adjacent encoded words is dropped."
  (interactive)
  (save-excursion 
    (let ((headers (concat "^\\(" vm-mime-encode-headers-regexp "\\):"))
          (case-fold-search nil)
          body-start
          start end)
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (setq body-start (vm-marker (match-beginning 0)))
      (goto-char (point-min))
      
      (while (let ((case-fold-search t))
	       (re-search-forward headers body-start t))
        (goto-char (match-end 0))
        (setq start (point))
        (when (not (looking-at "\\s-"))
          (insert " ")
          (backward-char 1))
        (save-excursion
          ;; A marker that advances: encoding the words expands the text, and
          ;; a plain position taken now points into the middle of the header
          ;; afterwards.  The folding below needs to know where it really ends.
          (setq end (copy-marker
                     (or (and (re-search-forward "^[^ \t:]+:" body-start t)
                              (match-beginning 0))
                         body-start)
                     t)))
        (save-restriction
         (narrow-to-region start end)
         (vm-mime-encode-words))
        ;; and fold what is now there, counting the header name, which is part
        ;; of the first line (emacs-vm/vm#794)
        (vm-mime-fold-header (save-excursion (goto-char start)
                                             (line-beginning-position))
                             end)
        (goto-char end)
        (set-marker end nil)))))
(defun vm-mime-fold-header--break (bol limit)
  "Where to break the line starting at BOL so it is no longer than LIMIT.
The last whitespace at or before the limit, and never the first character of
the line, a break there leaving an empty line.  Nil when there is nowhere to
break: one long run with no whitespace in it."
  (save-excursion
    (let ((eol (line-end-position))
          (break nil))
      (goto-char (1+ bol))
      (while (and (< (point) eol)
                  (re-search-forward "[ \t]" eol t)
                  (<= (- (point) bol) limit))
        (setq break (match-beginning 0)))
      break)))

(defun vm-mime-fold-header (start end)
  "Fold the header between START and END so its lines are not over-long.
RFC 5322 section 2.1.1 sets a limit of 998 characters that a line MUST NOT
exceed and 78 that it SHOULD NOT; `vm-mime-header-line-limit' is the second.
A folded line is broken at whitespace and the next one begins with a space,
which section 2.2.3 says a reader joins back up.

Breaks only where whitespace already is, so nothing is inserted into the
text: between two encoded words the whitespace is dropped when they are
decoded, and elsewhere it was in the header to begin with.  A run with no
whitespace in it stays over the limit, there being nowhere to break it."
  (save-excursion
    (let ((end (copy-marker end t)))
      (goto-char start)
      (while (< (point) end)
        (let* ((bol (line-beginning-position))
               (break (and (> (- (min end (line-end-position)) bol)
                              vm-mime-header-line-limit)
                           (vm-mime-fold-header--break
                            bol vm-mime-header-line-limit))))
          (if (not break)
              (forward-line 1)
            (goto-char break)
            (delete-region break (progn (skip-chars-forward " \t") (point)))
            (insert "\n ")
            ;; carry on from the continuation line, which may need folding too
            (beginning-of-line))))
      (set-marker end nil))))

(put 'vm-mime-encode-headers 'vm-called-by-vm t)

;;;###autoload
(defun vm-mime-encode-composition (&optional attachments-only)
 "MIME encode the current mail composition buffer.

This function chooses the MIME character set(s) to use, and transforms the
message content from the Emacs-internal encoding to the corresponding
octets in that MIME character set.

It then applies some transfer encoding to the message. For details of the
transfer encodings available, see the documentation for
`vm-mime-8bit-text-transfer-encoding.'

Finally, it creates the headers that are necessary to identify the message
as one that uses MIME.

Under MULE, it explicitly sets `buffer-file-coding-system' to a binary
 (no-transformation) coding system, to avoid further transformation of the
message content when it's passed to the MTA (that is, the mail transfer
agent; under Unix, normally sendmail.)

Attachment tags added to the buffer with `vm-attach-file' are expanded
and the appropriate content-type and boundary markup information is added."

  (interactive)

  (vm-mail-mode-show-headers)

  (vm-disable-modes vm-disable-modes-before-encoding)

  (vm-mime-encode-headers)

  (if vm-mail-reorder-message-headers
      (vm-reorder-message-headers 
       nil :keep-list vm-mail-header-order :discard-regexp 'none))
  
  (buffer-enable-undo)
  (let ((unwind-needed t)
	(mybuffer (current-buffer)))
    (unwind-protect
	(progn
	  (vm-mime-encode-composition-internal attachments-only)
	  (setq unwind-needed nil))
      (and unwind-needed (consp buffer-undo-list)
	   (eq mybuffer (current-buffer))
	   (setq buffer-undo-list (primitive-undo 1 buffer-undo-list))))))

(defvar enriched-mode)
(defvar enriched-initial-annotation)

;; This function was originally XEmacs-specific.  It has now been
;; generalized to both XEmacs and GNU Emacs.  USR, 2011-03-27

(defun vm-mime-encode-composition-internal (&optional attachments-only)
  "MIME encode the message composition in the current buffer."
  (save-restriction
    (widen)
    (unless (eq major-mode 'mail-mode)
      (error "Command must be used in a VM Mail mode buffer."))
    (when (vm-mail-mode-get-header-contents "MIME-Version:")
      (error "Message is already MIME encoded."))
    (let ((8bit nil)
	  (multipart t)		        ; start off asuming multipart
	  (boundary-positions nil)	; position markers for the parts
	  text-result			; results from text encodings
	  forward-local-refs already-mimed layout e e-list boundary
	  type encoding params description disposition object ;; charset
	  opoint-min encoded-attachment message-smimed)
      (goto-char (mail-text-start))
      (setq e-list (vm-mime-attachment-button-extents 
		    (point) (point-max) 'vm-mime-object))
      ;; We have a multipart message unless there's just one
      ;; attachment and no other readable text in the buffer.
      (when (and (= (length e-list) 1)
		 (looking-at "[ \t\n]*")
		 (= (match-end 0)
		    (vm-extent-start-position (car e-list)))
		 (save-excursion
		   (goto-char (vm-extent-end-position (car e-list)))
		   (looking-at "[ \t\n]*\\'")))
	(setq multipart nil))
      ;; 1. Insert the text parts and attachments
      (if (null e-list)
	  ;; no attachments
	  (vm-mime-encode-text-part (point) (point-max) t)
	;; attachments to be handled
	(while e-list
	  (setq e (car e-list))
	  ;; 1a. Insert the text part
	  (if (or (not multipart)
		  (save-excursion
		    (eq (vm-extent-start-position e)
			(re-search-forward 
			 "[ \t\n]*" (vm-extent-start-position e) t))))
	      ;; found an attachment
	      (delete-region (point) (vm-extent-start-position e))
	    ;; found text
	    (setq text-result 
		  (vm-mime-encode-text-part
		   (point) (vm-extent-start-position e) nil))
	    (setq boundary-positions 
		  (cons (car text-result) boundary-positions))
	    (setq 8bit (or 8bit (equal (cdr text-result) "8bit"))))

	  ;; 1b. Prepare for the object
	  (goto-char (vm-extent-start-position e))
	  (narrow-to-region (point) (point))
	  (setq object (vm-extent-property e 'vm-mime-object))

	  ;; 1c. Insert the object
	  (cond ((bufferp object)
		 (vm-mime-insert-buffer-substring 
		  object (vm-extent-property e 'vm-mime-type)))
		;; insert attachment from another folder
		((listp object)
		 (setq boundary-positions
		       (cons (point-marker) boundary-positions))
		 ;; `vm-insert-region-from-buffer' enters `save-restriction' in
		 ;; the folder it reads from, so that folder's narrowing comes
		 ;; back.  Widening it from here left it widened (#780).
		 (vm-insert-region-from-buffer
		  (nth 0 object) (nth 1 object) (nth 2 object))
		 (setq encoded-attachment t))
		;; insert file
		((stringp object)
		 (vm-mime-insert-file-contents 
		  object (vm-extent-property e 'vm-mime-type))))

	  ;; 1d. Gather information about the object from the extent.
	  (if (setq already-mimed (vm-extent-property e 'vm-mime-encoded))
	      (setq layout 
		    (vm-mime-parse-entity
		     nil :default-type (list "text/plain" "charset=us-ascii")
		     :default-encoding "7bit")
		    type (or (vm-extent-property e 'vm-mime-type)
			     (car (vm-mm-layout-type layout)))
		    params (or (vm-extent-property e 'vm-mime-parameters)
			       (cdr (vm-mm-layout-qtype layout)))
		    forward-local-refs
		        (car (vm-extent-property e 'vm-mime-forward-local-refs))
		    description (vm-extent-property e 'vm-mime-description)
		    disposition
		    (if (equal
			 (car (vm-extent-property e 'vm-mime-disposition))
			 "unspecified")
			(vm-mm-layout-qdisposition layout)
		      (vm-extent-property e 'vm-mime-disposition)))
	    (setq layout nil
		  type (vm-extent-property e 'vm-mime-type)
		  params (vm-extent-property e 'vm-mime-parameters)
		  forward-local-refs
		      (car (vm-extent-property e 'vm-mime-forward-local-refs))
		  description (vm-extent-property e 'vm-mime-description)
		  disposition
		  (if (equal
		       (car (vm-extent-property e 'vm-mime-disposition))
		       "unspecified")
		      (if attachments-only '("attachment") nil)
		    (if attachments-only
			(cons "attachment"
			      (cdr (vm-extent-property e 'vm-mime-disposition)))
		      (vm-extent-property e 'vm-mime-disposition)))))
	  ;; 1e. Encode the object if necessary
	  (cond ((vm-mime-types-match "text" type)
		 (setq encoding
		       (or (car (vm-extent-property e 'vm-mime-encoding))
			   (vm-determine-proper-content-transfer-encoding
			    (if already-mimed
				(vm-mm-layout-body-start layout)
			      (point-min))
			    (point-max)))
		       encoding (vm-mime-transfer-encode-region
				 encoding
				 (if already-mimed
				     (vm-mm-layout-body-start layout)
				   (point-min))
				 (point-max)
				 t))
		 (setq 8bit (or 8bit (equal encoding "8bit"))))

		((vm-mime-composite-type-p type)
		 (setq opoint-min (point-min))
		 (unless already-mimed
		   (goto-char (point-min))
		   (insert "Content-Type: " type "\n")
		   ;; vm-mime-transfer-encode-layout will replace
		   ;; this if the transfer encoding changes.
		   (insert "Content-Transfer-Encoding: 7bit\n\n")
		   (setq layout 
			 (vm-mime-parse-entity
			  nil 
			  :default-type (list "text/plain" "charset=us-ascii")
			  :default-encoding "7bit"))
		   (setq already-mimed t))
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data for the new layout
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (vm-mm-layout-qdisposition layout)))
		 (setq encoding (vm-mime-transfer-encode-layout layout))
		 (setq 8bit (or 8bit (equal encoding "8bit")))
		 (goto-char (point-max))
		 (widen)
		 (narrow-to-region opoint-min (point)))

		((not encoded-attachment)
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data that might now be stale
		   ; but retain the disposition if nothing new
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (or (vm-mm-layout-qdisposition layout)
					 disposition)))
		 (if already-mimed
		     (setq encoding (vm-mime-transfer-encode-layout layout))
		   (vm-mime-base64-encode-region (point-min) (point-max))
		   (setq encoding "base64"))))

	  ;; 1f. Add the required MIME headers
	  (unless (or (not multipart) encoded-attachment)
	    (goto-char (point-min))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (when already-mimed
	      ;; trim headers
	      (vm-reorder-message-headers 
	       nil :keep-list '("Content-ID:") :discard-regexp nil)
	      ;; remove header/text separator
	      (goto-char (1- (vm-mm-layout-body-start layout)))
	      (when (looking-at "\n")
		(delete-char 1)))
	    (insert "Content-Type: " 
		    (vm-mime-type-with-params type params)
		    "\n")
	    (when description
	      (insert "Content-Description: " description "\n"))
	    (when disposition
	      (insert "Content-Disposition: "
		      (vm-mime-type-with-params 
		       (car disposition) (cdr disposition))
		      "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n"))
	  (goto-char (point-max))
	  (widen)

	  ;; 1g. Delete the original attachment button
	  (save-excursion
	    (goto-char (vm-extent-start-position e))
	    (vm-assert (looking-at "\\[ATTACHMENT")))
	  (delete-region (vm-extent-start-position e)
			 (vm-extent-end-position e))
	  (vm-detach-extent e)
	  (when (looking-at "\n") (delete-char 1))
	  (setq e-list (cdr e-list)))

	;; 2. Handle the remaining chunk of text after the last
	;; extent, if any.
	(if (and multipart (not (looking-at "[ \t\n]*\\'")))
	    (progn
	      (setq text-result 
		    (vm-mime-encode-text-part (point) (point-max) nil))
	      (setq boundary-positions 
		    (cons (car text-result) boundary-positions))
	      (setq 8bit (or 8bit (equal (cdr text-result) "8bit")))
	      (goto-char (point-max)))
	  (delete-region (point) (point-max)))

	;; 3. Create and insert boundary lines
	(when multipart 
	  (setq boundary (vm-mime-make-multipart-boundary))
	  (mail-text)
	  (while (re-search-forward 
		  (concat "^--" (regexp-quote boundary) "\\(--\\)?$")
		  nil t)
	    (setq boundary (vm-mime-make-multipart-boundary))
	    (mail-text))
	  (goto-char (point-max))
	  (insert "\n--" boundary "--\n")
	  (while boundary-positions
	    (goto-char (car boundary-positions))
	    (insert "\n--" boundary "\n")
	    (setq boundary-positions (cdr boundary-positions))))

	;; 4. Add MIME headers to the message
	(when (and (not multipart) already-mimed)
	  (goto-char (vm-mm-layout-header-start layout))
	  ;; trim headers
	  (vm-reorder-message-headers
	   nil :keep-list '("Content-ID:") :discard-regexp nil)
	  ;; remove header/text separator
	  (goto-char (vm-mm-layout-header-end layout))
	  (when (looking-at "\n") (delete-char 1))
	  ;; copy remainder to enclosing entity's header section
	  (goto-char (point-max))
	  (when multipart
	    (insert-buffer-substring (current-buffer)
				     (vm-mm-layout-header-start layout)
				     (vm-mm-layout-body-start layout)))
	  (delete-region (vm-mm-layout-header-start layout)
			 (vm-mm-layout-body-start layout)))
	(goto-char (point-min))
	(vm-remove-mail-mode-header-separator)
	(vm-reorder-message-headers
	 nil :keep-list nil 
	 :discard-regexp
	 "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(vm-add-mail-mode-header-separator)
	(when (or vm-smime-sign-message vm-smime-encrypt-message)
	  (mail-text)
	  (open-line 1))
	(insert "MIME-Version: 1.0\n")
	(if multipart
	    (progn
	      (insert "Content-Type: "
		      (vm-mime-type-with-params 
		       "multipart/mixed" 
		       (list (format "boundary=\"%s\"" boundary)))
		      "\n")
	      (insert "Content-Transfer-Encoding: "
		      (if 8bit "8bit" "7bit") "\n"))
	  (insert "Content-Type: " (vm-mime-type-with-params type params) "\n")
	  (when disposition
	    (insert "Content-Disposition: " 
		    (vm-mime-type-with-params 
		     (car disposition) (cdr disposition))
		    "\n"))
	  (when description
	    (insert "Content-Description: " description "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n")))
      ;; If necessary do smime signing and encrypting. This is the last
      ;; task since it operates on the entirety of the message, including
      ;; mime
      (when vm-smime-sign-message
	(mail-text)
	(or (smime-sign-region 
	     (point) (point-max)
	     (or (smime-get-key-by-email
		  (vm-get-sender))
		 (error "S/MIME: cannot find key for sender, see smime-keys")))
	    (error "S/MIME: signing outgoing message failed"))
	;; Now that this composition is signed, if there is some reason
	;; it is not sent, we do not want to sign it again
	(setq message-smimed t)
	(setq vm-smime-sign-message nil))
      (when vm-smime-encrypt-message
	(mail-text)
	(or (smime-encrypt-region (point) (point-max)
				  (vm-smime-get-recipient-certfiles))
	    (error "S/MIME: encryption of outgoing message failed"))
	;; do not encrypt twice if message did not get sent
	(setq message-smimed t)
	(setq vm-smime-encrypt-message nil))
      ;; Now we need to move the S/MIME generated headers back into the
      ;; header area
      (when message-smimed
	(mail-text)
	(vm-remove-mail-mode-header-separator)
	(forward-line -1)
	(delete-char 1)
	(vm-add-mail-mode-header-separator)))))

(defun vm-mime-encode-text-part (beg end whole-message)
  "Encode the text from BEG to END in a composition buffer
as MIME part and add appropriate MIME headers.  If WHOLE-MESSAGE is
true, then encode it as the entire message.

Returns a pair consisting of a marker pointing to the start of the
encoded MIME part and the transfer-encoding used.  But if
WHOLE-MESSAGE is true then nil is returned."
  (let ((enriched (and (boundp 'enriched-mode) enriched-mode))
	encoding charset description marker flowed) ;; type params
    (narrow-to-region beg end)
    ;; Mark the soft line breaks before the text is encoded or measured, and
    ;; only for plain text: text/enriched carries its own line structure.
    (when (and vm-send-using-flowed-text (not enriched))
      (setq flowed (vm-mime-flow-region (point-min) (point-max))))
    ;; support enriched-mode for text/enriched composition
    (when enriched
      (let ((enriched-initial-annotation ""))
	(save-excursion
	  ;; insert/delete trick needed to avoid
	  ;; enriched-mode tags from seeping into the
	  ;; attachment overlays.  I really wish
	  ;; front-advance / rear-advance overlay
	  ;; endpoint properties actually worked.
	  (goto-char (point-max))
	  (insert-before-markers "\n")
	  (enriched-encode (point-min) (1- (point)))
	  (goto-char (point-max))
	  (delete-char -1))))
            
    (setq charset (vm-determine-proper-charset (point-min) (point-max)))
    (when t
      (let ((coding-system
	     (vm-mime-charset-to-coding charset)))
	(unless coding-system
	  (error "Can't find a coding system for charset %s" charset))
	(encode-coding-region (point-min) (point-max) 
	     ;; What about the case where vm-m-m-c-t-c-a doesn't have an
	     ;; entry for the given charset? That shouldn't happen, if
	     ;; vm-mime-mule-coding-to-charset-alist and
	     ;; vm-mime-mule-charset-to-coding-alist have complete and
	     ;; matching entries. Admittedly this last is not a
	     ;; given. Should we make it so on startup? (By setting the
	     ;; key for any missing entries in
	     ;; vm-mime-mule-coding-to-charset-alist to being (format "%s"
	     ;; coding-system), if necessary.)        RWF, 2005-03-25
			      coding-system)))

    (setq encoding (vm-determine-proper-content-transfer-encoding
		    (point-min) (point-max))
	  encoding (vm-mime-transfer-encode-region 
		    encoding (point-min) (point-max) t)
	  description (vm-mime-text-description 
		       (point-min) (point-max)))
    (if whole-message
	(progn
	  (widen)
	  (vm-remove-mail-mode-header-separator)
	  (goto-char (point-min))
	  (vm-reorder-message-headers
	   nil :keep-list nil 
	   :discard-regexp
	   "\\(Content-Type:\\|Content-Transfer-Encoding\\|MIME-Version:\\)")
	  (vm-add-mail-mode-header-separator)
	  (when (or vm-smime-sign-message vm-smime-encrypt-message)
		(mail-text)
		(open-line 1))
	  (insert "MIME-Version: 1.0\n")
	  (if enriched
	      (insert "Content-Type: text/enriched; charset=" charset "\n")
	    (insert "Content-Type: text/plain; charset=" charset
		    (if flowed "; format=flowed" "") "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n")
	  nil)

      (setq marker (point-marker))
      (if enriched
	  (insert "Content-Type: text/enriched; charset=" charset "\n")
	(insert "Content-Type: text/plain; charset=" charset
		(if flowed "; format=flowed" "") "\n"))
      (when description
	(insert "Content-Description: " description "\n"))
      (insert "Content-Transfer-Encoding: " encoding "\n\n")
      (widen)
      (cons marker encoding))))


;; This function is now defunct.   Use vm-mime-encode-composition.
;; USR, 2011-03-27



(defun vm-mime-fragment-composition (size)
  (save-restriction
    (widen)
    (vm-inform 5 "Fragmenting message...")
    (let ((buffers nil)
	  (total-markers nil)
	  (id (vm-mime-make-multipart-boundary))
	  (n 1)
	  b header-start header-end master-buffer start end)
      (vm-remove-mail-mode-header-separator)
      ;; message/partial must have "7bit" content transfer
      ;; encoding, so force everything to be encoded for
      ;; 7bit transmission.
      (let ((vm-mime-8bit-text-transfer-encoding
	     (if (eq vm-mime-8bit-text-transfer-encoding '8bit)
		 'quoted-printable
	       vm-mime-8bit-text-transfer-encoding)))
	(vm-mime-transfer-encode-layout
	 (vm-mime-parse-entity
	  nil 
	  :default-type (list "text/plain" "charset=us-ascii")
	  :default-encoding "7bit")))
      (goto-char (point-min))
      (setq header-start (point))
      (search-forward "\n\n")
      (setq header-end (1- (point)))
      (setq master-buffer (current-buffer))
      (goto-char (point-min))
      (setq start (point))
      (while (not (eobp))
	(condition-case nil
	    (progn
	      (forward-char (max (- size 150) 2000))
	      (beginning-of-line))
	  (end-of-buffer nil))
	(setq end (point))
	(setq b (generate-new-buffer (concat (buffer-name) " part "
					     (int-to-string n))))
	(setq buffers (cons b buffers))
	(set-buffer b)
	(make-local-variable 'vm-send-using-mime)
	(setq vm-send-using-mime nil)
	(insert-buffer-substring master-buffer header-start header-end)
	(goto-char (point-min))
	(vm-reorder-message-headers 
	 nil :keep-list nil
	 :discard-regexp
         "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(insert "MIME-Version: 1.0\n")
	(insert (format
		 (if vm-mime-avoid-folding-content-type
		     "Content-Type: message/partial; id=%s; number=%d"
		   "Content-Type: message/partial;\n\tid=%s;\n\tnumber=%d")
		 id n))
	;; No number here on purpose: how many parts there will be is not
	;; known until the last one has been cut, so the value is written
	;; below, at the position recorded next.  A `%d' here puts the
	;; fragment's own number in the way of it, and the two run together:
	;; three parts numbered 1, 2, 3 went out saying total=13, 23 and 33,
	;; which no reader can reassemble (emacs-vm/vm#797).
	(if vm-mime-avoid-folding-content-type
	    (insert "; total=")
	  (insert ";\n\ttotal="))
	(setq total-markers (cons (point) total-markers))
	(insert "\nContent-Transfer-Encoding: 7bit\n")
	(goto-char (point-max))
	(insert mail-header-separator "\n")
	(insert-buffer-substring master-buffer start end)
	(vm-increment n)
	(set-buffer master-buffer)
	(setq start (point)))
      (vm-decrement n)
      (vm-add-mail-mode-header-separator)
      (let ((bufs buffers))
	(while bufs
	  (set-buffer (car bufs))
	  (goto-char (car total-markers))
	  (prin1 n (current-buffer))
	  (setq bufs (cdr bufs)
		total-markers (cdr total-markers)))
	(set-buffer master-buffer))
      (vm-inform 5 "Fragmenting message... done")
      (nreverse buffers))))

;; moved to vm-reply.el, not MIME-specific.
;;;###autoload (autoload 'vm-mime-preview-composition "vm-mime" nil t)
(defalias 'vm-mime-preview-composition 'vm-preview-composition)

(defun vm-mime-composite-type-p (type)
  "Check if TYPE is a MIME type that might have subparts."
  (or (vm-mime-types-match "message/rfc822" type)
      (vm-mime-types-match "message/news" type)
      (vm-mime-types-match "multipart" type)))

;; Unused currrently.
;;

(defvar vm-mime-layout nil)		; used with dynamic binding

(defun vm-mime-sprintf (format layout)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let ((match (assoc format vm-mime-compiled-format-alist)))
    (if (null match)
	(progn
	  (vm-mime-compile-format format)
	  (setq match (assoc format vm-mime-compiled-format-alist))))
    ;; The local variable name `vm-mime-layout' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-mime-layout layout))
      (eval (cdr match)))))

(defconst vm-mime-number-specifiers '(?n ?N ?T)
  "The button specifiers whose substitution is a number.
A width beginning with 0 fills with zeros for these and with spaces for
everything else, as printf does.")

(defun vm-mime-compile-format (format)
  (let ((return-value (vm-mime-compile-format-1 format 0)))
    (setq vm-mime-compiled-format-alist
	  (cons (cons format (nth 1 return-value))
		vm-mime-compiled-format-alist))))

(defun vm-mime-compile-format-1 (format start-index)
  (or start-index (setq start-index 0))
  (let ((case-fold-search nil)
	(done nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end start-index)
	new-match-end conv-spec)
    (store-match-data nil)
    (while (not done)
      (while
	  (and (not done)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()acdefknNstTx%]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (memq conv-spec '(?\( ?a ?c ?d ?e ?f ?k ?n ?N ?s ?t ?T ?x))
	    (progn
	      (cond ((= conv-spec ?\()
		     (save-match-data
		       (let ((retval (vm-mime-compile-format-1 format
							       (match-end 5))))
			 (setq sexp (cons (nth 1 retval) sexp)
			       new-match-end (car retval)))))
		    ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-mf-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-mf-text-charset
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-mf-content-description
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?e)
		     (setq sexp (cons (list 'vm-mf-content-transfer-encoding
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-mf-attachment-file
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?k)
		     (setq sexp (cons (list 'vm-mf-event-for-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?n)
		     (setq sexp (cons (list 'vm-mf-parts-count
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?N)
		     (setq sexp (cons (list 'vm-mf-partial-number
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-mf-parts-count-pluralizer
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-mf-content-type-description
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-mf-partial-total
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?x)
		     (setq sexp (cons (list 'vm-mf-external-body-content-type
					    'vm-mime-layout) sexp))))
	      ;; The maximum first and the width after it, as printf does it
	      ;; and as `vm-summary-compile-format-1' does (emacs-vm/vm#848).
	      (cond ((match-beginning 3)
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-number
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (cond ((and (match-beginning 1) (match-beginning 2))
		     ;; Spaces whatever the width says: a `-' beats a `0'.
		     (setcar sexp
			     (list 'vm-left-justify-string
				   (car sexp)
				   (string-to-number
				    (substring format
					       (match-beginning 2)
					       (match-end 2))))))
		    ((match-beginning 2)
		     (setcar sexp
			     (list
			      (if (and (eq (aref format (match-beginning 2)) ?0)
				       (memq conv-spec vm-mime-number-specifiers))
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-number
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (setq sexp-fmt
		    (cons "%s"
			  (cons (vm-percent-quote
				 (substring format
					    last-match-end
					    (match-beginning 0)))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq done t))
			"%%")
		      (cons (vm-percent-quote
			     (substring format
					(or last-match-end 0)
					(match-beginning 0)))
			    sexp-fmt))))
	(setq last-match-end new-match-end))
      (unless done
	(setq sexp-fmt
	      (cons (vm-percent-quote
		     (substring format last-match-end (length format)))
		    sexp-fmt)
	      done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	;; Nothing to substitute, so nothing calls `format' and the doubled
	;; percents would reach the button as themselves.
	(setq sexp (vm-percent-unquote sexp-fmt))))
    (list last-match-end sexp)))

(defun vm-mime-find-format-for-layout (layout)
  (let ((p vm-mime-button-format-alist)
	(type (car (vm-mm-layout-type layout))))
    (catch 'done
      (cond ((vm-mime-types-match "error/error" type)
	     (throw 'done "%d"))
	    ((vm-mime-types-match "text/x-vm-deleted" type)
	     (throw 'done "%d")))
      (while p
	(if (vm-mime-types-match (car (car p)) type)
	    (throw 'done (cdr (car p)))
	  (setq p (cdr p))))
      "%-25.25t [%k to %a]" )))

(defun vm-mf-content-type (layout)
  (car (vm-mm-layout-type layout)))

(defun vm-mf-external-body-content-type (layout)
  (car (vm-mm-layout-type (car (vm-mm-layout-parts layout)))))

(defun vm-mf-content-transfer-encoding (layout)
  (vm-mm-layout-encoding layout))

(defun vm-mf-content-description (layout)
  (or (vm-mm-layout-description layout)
      (vm-mf-content-type-description layout)))

(defun vm-mf-content-type-description (layout)
  (let ((p vm-mime-type-description-alist)
	(type (car (vm-mm-layout-type layout))))
    (catch 'done
      (while p
	(if (vm-mime-types-match (car (car p)) type)
	    (throw 'done (cdr (car p)))
	  (setq p (cdr p))))
      (vm-mf-content-type layout) )))

(defun vm-mf-text-charset (layout)
  (or (vm-mime-get-parameter layout "charset")
      "us-ascii"))

(defun vm-mf-parts-count (layout)
  (int-to-string (length (vm-mm-layout-parts layout))))

(defun vm-mf-parts-count-pluralizer (layout)
  (if (= 1 (length (vm-mm-layout-parts layout))) "" "s"))

(defun vm-mf-partial-number (layout)
  (or (vm-mime-get-parameter layout "number")
      "?"))

(defun vm-mf-partial-total (layout)
  (or (vm-mime-get-parameter layout "total")
      "?"))

(defun vm-mf-attachment-file (layout)
  (or vm-mf-attachment-file ;; for %f expansion in external viewer arg lists
      (vm-mime-get-disposition-filename layout)
      (vm-mime-get-parameter layout "name")
      "<no suggested filename>"))

(defun vm-mf-event-for-default-action (_layout)
  (if (vm-mouse-support-possible-here-p)
      "Click mouse-2"
    "Press RETURN"))

;; This puts "alternative" on all attachments.  Silly.  USR, 2011-11-24

(defun vm-mf-default-action (layout)
  (or vm-mf-default-action
      (let () ;; cons
        (cond ((or (vm-mime-can-display-internal layout)
		   (vm-mime-find-external-viewer
		    (car (vm-mm-layout-type layout))))
	       (let ((p vm-mime-default-action-string-alist)
		     (type (car (vm-mm-layout-type layout))))
		 (catch 'done
		   (while p
		     (if (vm-mime-types-match (car (car p)) type)
			 (throw 'done (cdr (car p)))
		       (setq p (cdr p))))
		   nil )))
	      (;; (setq cons 
		(vm-mime-can-convert
			   (car (vm-mm-layout-type layout))) ;;)
	       "convert")
	      (t "save")))
      ;; should not be reached
      "burn in the raging fires of hell forever"))

(defun vm-mime-map-layout-parts (m function &optional layout path)
  "Apply FUNCTION to each part of the message M.
This function will call itself recursively with the currently processed LAYOUT
and the PATH to it.  PATH is a list of parent layouts where the root is at the
end of the path."
  (unless layout
    (setq layout (vm-mm-layout m)))
  (when (vectorp layout)
    (funcall function m layout path)
    (let ((parts (copy-sequence (vm-mm-layout-parts layout))))
      (while parts
        (vm-mime-map-layout-parts m function (car parts) (cons layout path))
        (setq parts (cdr parts))))))

;;;###autoload
(defun vm-list-mime-part-structure (&optional verbose)
  "List mime part structure of the current message."
  (interactive "P")
  (vm-check-for-killed-summary)
  (if (vm-interactive-p) (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (let ((m (car vm-message-pointer))
	(buffer (get-buffer-create "*VM mime part layout*")))
    (with-current-buffer buffer (setq truncate-lines t))
    (with-electric-help
     (lambda ()
       (princ (format "%s\n" (vm-decode-mime-encoded-words-in-string
			       (vm-su-subject m))))
       (vm-mime-map-layout-parts
	m
	(lambda (_m layout path)
	  (if verbose
	      (princ (format "%s%S\n" (make-string (length path) ? ) layout))
	    (princ (format "%s%S%s%s%s\n" (make-string (length path) ? )
			   (vm-mm-layout-type layout)
			   (let ((id (vm-mm-layout-id layout)))
			     (if id (format " id=%S" id) ""))
			   (let ((desc (vm-mm-layout-description layout)))
			     (if desc (format " desc=%S" desc) ""))
			   (let ((dispo (vm-mm-layout-disposition layout)))
			     (if dispo (format " %S" dispo) ""))))))))
     buffer)
    ))
;;;###autoload (autoload 'vm-mime-list-part-structure "vm-mime" nil t)
(defalias 'vm-mime-list-part-structure
  'vm-list-mime-part-structure)

;;;###autoload
(defun vm-nuke-alternative--enclosing-alternative (path)
  "The nearest multipart/alternative in PATH, or nil if there is none.
PATH runs from the immediate parent outwards, so the nearest one is the
alternative whose choices the part is among."
  (let ((tail path) (found nil))
    (while (and tail (not found))
      (when (vm-mime-types-match "multipart/alternative"
                                 (car (vm-mm-layout-type (car tail))))
        (setq found (car tail)))
      (setq tail (cdr tail)))
    found))

(defun vm-nuke-alternative--has-plain-text-p (layout)
  "Non-nil when the first part of LAYOUT is text/plain.
That part is the copy the reader is left with, so it is what makes
deleting the html safe; an alternative offering html alone is the only
copy there is."
  (let ((first (car (vm-mm-layout-parts layout))))
    (and (vectorp first)
         (vm-mime-types-match "text/plain" (car (vm-mm-layout-type first))))))

(defun vm-nuke-alternative-text/html-internal (m)
  "Delete all text/html parts of multipart/alternative parts of message M.
Returns the number of deleted parts.  text/html parts are only deleted iff
the first sub part of a multipart/alternative is a text/plain part."
  (let ((deleted-count 0)
        this-type alternative)
    (vm-mime-map-layout-parts
     m
     (lambda (m layout path)
       (setq this-type (car (vm-mm-layout-type layout))
             ;; the alternative this part is offered under, which is the
             ;; nearest one in the path: a text/html inside a
             ;; multipart/related inside an alternative is still one of the
             ;; alternatives on offer
             alternative (vm-nuke-alternative--enclosing-alternative path))
       (when (and alternative
                  (vm-nuke-alternative--has-plain-text-p alternative)
                  (vm-mime-types-match "text/html" this-type))
         (with-current-buffer (vm-buffer-of m)
           (let ((buffer-read-only nil))
             (save-restriction
              (widen)
              (if (vm-mm-layout-is-converted layout)
                  (setq layout (vm-mm-layout-unconverted-layout layout)))
              (goto-char (vm-mm-layout-header-start layout))
              (forward-line -1)
              (delete-region (point) (vm-mm-layout-body-end layout))
              (vm-set-edited-flag-of m t)
              (vm-set-byte-count-of m nil)
              (vm-set-line-count-of m nil)
              (vm-set-stuff-flag-of m t)
              (vm-mark-for-summary-update m)))
           (setq deleted-count (1+ deleted-count))))))
    deleted-count))

;;;###autoload
(defun vm-nuke-alternative-text/html (&optional count mlist)
  "Removes the text/html part of all multipart/alternative message parts.

This is a destructive operation and cannot be undone!"
  (interactive "p")
  (when (vm-interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (let ((mlist (or mlist 
		   (vm-select-operable-messages
		    count (vm-interactive-p) "Nuke html of"))))
    (vm-retrieve-operable-messages count mlist :fail t)
    (save-excursion
      (while mlist
        (let* ((m (vm-real-message-of (car mlist)))
	      (count (vm-nuke-alternative-text/html-internal m)))
          (when (vm-interactive-p)
            (if (= count 0)
                (vm-inform 5 "No text/html parts found.")
              (vm-inform 5 "%d text/html part%s deleted."
                       count (if (> count 1) "s" ""))))
          (setq mlist (cdr mlist))))))
  (when (vm-interactive-p)
    (vm-discard-cached-data count)
    (vm-present-current-message)))

;;-----------------------------------------------------------------------------
;; The following functions are taken from vm-postpone.el
;; Copyright (C) Robert Widhopf-Fenk
;; Copyright (C) Uday S. Reddy, 2010-2011
;; Copyright (C) 2024-2026 The VM Developers

;;;###autoload
(defun vm-mime-convert-to-attachment-buttons ()
  "Replace all mime buttons in the current buffer by attachment buttons."
  ;; called vm-mime-encode-mime-attachments in vm-postpone.el
  (interactive)
  (let ((e-list (vm-mime-attachment-button-extents
		 (point-min) (point-max) 'vm-mime-layout)))
    (while e-list
      (vm-mime-replace-by-attachment-button (car e-list))
      (setq e-list (cdr e-list)))
    (goto-char (point-max))))

;; The function vm-mime-re-fake-attachment-overlays from vm-postpone.el is
;; now unused.  USR, 2011-02-14 

(defun vm-mime-replace-by-attachment-button (x)
  "Replace the MIME button specified by extent X by an attachment button."
  ;; This was called vm-mime-encode-mime-button in vm-postpone.el
  (save-excursion
    (let* ((layout (vm-extent-property x 'vm-mime-layout))
	   (xstart (vm-extent-start-position x))
	   (hstart (vm-mm-layout-header-start layout))
	   (bstart (vm-mm-layout-body-start layout))
	   (end    (vm-mm-layout-body-end   layout))
	   (hbuf   (marker-buffer hstart))
	   (bbuf   (marker-buffer bstart))
	   (type   (vm-mm-layout-type layout))
	   (desc   (or (vm-mm-layout-description layout)
		       (vm-mime-get-parameter layout "name")
		       "attachment"))
	   (disp   (or (vm-mm-layout-disposition layout)
		       '("inline")))
	   (file   (vm-mime-get-disposition-parameter layout "filename"))
	   (ext-file nil))

      ;; special case of message/external-body
      ;; seems to be unused now.  USR, 2011-12-06
      (when (and type
		 (string= (car type) "message/external-body")
		 (string= (cadr type) "access-type=local-file"))
	(save-excursion
	  (setq ext-file (substring (caddr type) 5))
	  (vm-select-folder-buffer)
	  (let ((start (vm-mm-layout-body-start layout))
		(end   (vm-mm-layout-body-end layout)))
	    ;; `save-restriction' in the buffer the markers point into, which
	    ;; is normally the folder just selected but is not promised to be
	    ;; (#780).
	    (with-current-buffer (marker-buffer (vm-mm-layout-body-start layout))
	      (save-restriction
		(widen)
		(goto-char start)
		(if (not (re-search-forward
			  "Content-Type: \"?\\([^ ;\" \n\t]+\\)\"?;?"
			  end t))
		    (error "No `Content-Type' header found in: %s"
			   (buffer-substring start end))
		  (setq type (list (match-string 1)))))))))
        
      ;; insert an attached-object-button
      (goto-char xstart)
      (cond (ext-file
	     (vm-attach-file ext-file (car type)))
	    ((eq hbuf bbuf)
	     (vm-attach-object 
	      (if file
		  (list hbuf hstart end disp file)
		(list hbuf hstart end disp))
	      :type (car type) :params (cdr type) 
	      :disposition disp :description desc :mimed t))
	    (t
	     (vm-attach-object 
	      bbuf
	      :type (car type) :params (cdr type) 
	      :disposition disp :description desc :mimed nil)))
      ;; delete the mime-button
      (delete-region (vm-extent-start-position x) (vm-extent-end-position x))
      (vm-detach-extent x))))


;; This code was originally part of vm-mime-fsfemacs-encode-composition.

(defun vm-mime-insert-file-contents (file type)
  "Safely insert the contents of FILE of TYPE into the current buffer."
  ;; Even with the hooks attached to the attachment overlays, text can still
  ;; be inserted into them when font-lock is on.  Explaining why is beyond
  ;; the scope of this comment and I do not know the answer anyway.  This
  ;; insertion dance prevents it.
  (insert-before-markers " ")
  (forward-char -1)
  (let ((coding-system-for-read
	 (if (vm-mime-text-type-p type)
	     (vm-line-ending-coding-system)
	   (vm-binary-coding-system)))
	;; keep no undos
	(buffer-undo-list t)
	;; no transformations!
	(format-alist nil)
	;; no decompression!
	(jka-compr-compression-info-list nil)
	;; don't let buffer-file-coding-system be changed by
	;; insert-file-contents.  The value bound here is not important.
	(buffer-file-coding-system (vm-binary-coding-system)))
    (insert-file-contents file)
    (goto-char (point-max))
    (delete-char -1)))

(defun vm-mime-insert-buffer-substring (buffer _type)
  "Safe insert the contents of BUFFER of TYPE into the current buffer."
  (insert-buffer-substring buffer))


;;; Attachment commands, from vm-rfaddons.el (issue #606)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-attach-files-in-directory (directory &optional regexp)
  "Attach all files in DIRECTORY matching REGEXP.
The optional argument MATCH might specify a regexp matching all files
which should be attached, when empty all files will be attached.

When called with a prefix arg it will do a literal match instead of a regexp
match."
  (interactive
   ;; FIXME: Temporarily override substitute-in-file-name. but why?
   (cl-letf (((symbol-function 'substitute-in-file-name) #'identity))
     (let ((file (vm-read-file-name
                  "Attach files matching regexp: "
                  (or vm-mime-all-attachments-directory
                      vm-mime-attachment-save-directory
                      default-directory)
                  (or vm-mime-all-attachments-directory
                      vm-mime-attachment-save-directory
                      default-directory)
                  nil nil
                  'vm-attach-files-in-directory-regexps-history)))
       (list (file-name-directory file)
             (file-name-nondirectory file)))))

  (setq vm-mime-all-attachments-directory directory)

  (message "Attaching files matching `%s' from directory %s " regexp directory)
  
  (if current-prefix-arg
      (setq regexp (concat "^" (regexp-quote regexp) "$")))
  
  (let ((files (directory-files directory t regexp nil))
        file type charset)
    (if (null files)
        (error "No matching files!")
      (while files
        (setq file (car files))
        (if (file-directory-p file)
            nil ;; should we add recursion here?
          (setq type (or (vm-mime-default-type-from-filename file)
                         vm-attach-files-in-directory-default-type))
          (message "Attaching file %s with type %s ..." file type)
          (if (null type)
              (let ((default-type (or (vm-mime-default-type-from-filename file)
                                      "application/octet-stream")))
                (setq type (completing-read
			    ;; prompt
                            (format "Content type for %s (default %s): "
                                    (file-name-nondirectory file)
                                    default-type)
			    ;; collection
                            vm-mime-type-completion-alist)
                      type (if (> (length type) 0) type default-type))))
          (if (not (vm-mime-types-match "text" type)) nil
            (setq charset vm-attach-files-in-directory-default-charset)
            (cond ((eq 'guess charset)
                   (save-excursion
                     (let ((b (get-file-buffer file)))
                       (set-buffer (or b (find-file-noselect file t t)))
                       (setq charset (vm-determine-proper-charset (point-min)
                                                                  (point-max)))
                       (if (null b) (kill-buffer (current-buffer))))))
                  ((null charset)
                   (setq charset
                         (completing-read
			  ;; prompt
                          (format "Character set for %s (default US-ASCII): "
                                  file)
			  ;; collection
                          vm-mime-charset-completion-alist)
                         charset (if (> (length charset) 0) charset)))))
          (vm-attach-file file type charset))
        (setq files (cdr files))))))
(defun vm-mime-auto-save-all-attachments-subdir (msg)
  "Return a subdir for the attachments of MSG.
This will be done according to `vm-mime-auto-save-all-attachments-subdir'."
  (setq msg (vm-real-message-of msg))
  (cond ((functionp vm-mime-auto-save-all-attachments-subdir)
         (funcall vm-mime-auto-save-all-attachments-subdir msg))
        ((stringp vm-mime-auto-save-all-attachments-subdir)
         (vm-summary-sprintf vm-mime-auto-save-all-attachments-subdir msg))
        ((null vm-mime-auto-save-all-attachments-subdir)
         (let (;; for the folder
               (basedir (buffer-file-name (vm-buffer-of msg)))
               ;; for the message
               (subdir (concat 
                        "/"
                        (format "%04s.%02s.%02s-%s"
                                (vm-su-year msg)
                                (vm-su-month-number msg)
                                (vm-su-monthday msg)
                                (vm-su-hour msg))
                        "--"
			(or (vm-su-full-name msg)
			    "unknown")
                        "--"
                         (vm-su-subject msg))))
               
           (if (and basedir vm-folder-directory
                    (string-match
                     (concat "^" (expand-file-name vm-folder-directory))
                     basedir))
               (setq basedir (replace-match "" nil nil basedir)))
           
           (setq subdir (vm-replace-in-string subdir "\\s-\\s-+" " " t))
           (setq subdir (vm-replace-in-string subdir "[^A-Za-z0-9\241-_-]+" "_" t))
           (setq subdir (vm-replace-in-string subdir "?_-?_" "-" nil))
           (setq subdir (vm-replace-in-string subdir "^_+" "" t))
           (setq subdir (vm-replace-in-string subdir "_+$" "" t))
           (concat basedir "/" subdir)))
        (t
         (eval vm-mime-auto-save-all-attachments-subdir))))

(defun vm-mime-auto-save-all-attachments-path (msg)
  "Create a path for storing the attachments of MSG."
  (let ((subdir (vm-mime-auto-save-all-attachments-subdir
                 (vm-real-message-of msg))))
    (if (not vm-mime-attachment-save-directory)
        (error "Set `vm-mime-attachment-save-directory' for autosaving of attachments")
      (if subdir
          ;; the subdir may begin with a separator of its own, and two of
          ;; them in a path is untidy rather than wrong
          (concat (directory-file-name vm-mime-attachment-save-directory)
                  (if (string-prefix-p "/" subdir) "" "/")
                  subdir)
        vm-mime-attachment-save-directory))))

;;;###autoload
(defun vm-mime-auto-save-all-attachments (&optional count)
  "Save all attachments to a subdirectory.
Root directory for saving is `vm-mime-attachment-save-directory'.

You might add this to `vm-select-new-message-hook' in order to automatically
save attachments.

    (add-hook \\='vm-select-new-message-hook #\\='vm-mime-auto-save-all-attachments)"
  (interactive "P")

  (if vm-mime-auto-save-all-attachments-avoid-recursion
      nil
    (let ((vm-mime-auto-save-all-attachments-avoid-recursion t))
      (vm-check-for-killed-folder)
      (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
      
      (vm-save-all-attachments
       count
       'vm-mime-auto-save-all-attachments-path)

      (when (vm-interactive-p)
        (vm-discard-cached-data)
        (vm-present-current-message)))))

;;;###autoload
(defun vm-toggle-best-mime ()
  "Toggle between best-internal and best mime decoding modes. (Alley Soughton)"
  (interactive)
  (if (eq vm-mime-alternative-show-method 'best-internal)
      (progn
	(vm-decode-mime-message 'undecoded)
	(setq vm-mime-alternative-show-method 'best)
	(vm-decode-mime-message 'decoded)
	(message "using best MIME decoding"))
    (progn
      (vm-decode-mime-message 'undecoded)
      (setq vm-mime-alternative-show-method 'best-internal)
      (vm-decode-mime-message 'decoded)
      (message "using best internal MIME decoding"))))

(provide 'vm-mime)
;;; vm-mime.el ends here
