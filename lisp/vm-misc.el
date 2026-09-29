;;; -*- lexical-binding: t -*-
;;; vm-misc.el --- Miscellaneous functions for VM
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-2001 Kyle E. Jones
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
(require 'vm-message)
(require 'vm-vars)
(require 'auth-source)

(declare-function scroll-bar-mode "scroll-bar" (&optional arg))

;; VM's own names for the overlay functions, which were extents in XEmacs
(declare-function vm-buffer-substring-no-properties "vm-misc.el"
		  (start end))
(declare-function vm-extent-property "vm-misc.el" (overlay prop) t)
(declare-function vm-set-extent-property "vm-misc.el" (overlay prop value) t)
(declare-function vm-make-extent "vm-misc.el"
		  (beg end &optional buffer front-advance rear-advance) t)
(declare-function vm-extent-end-position "vm-misc.el" (overlay) t)
(declare-function vm-extent-start-position "vm-misc.el" (overlay) t)

(declare-function timezone-make-date-sortable "ext:timezone"
		  (date &optional local timezone))
(declare-function vm-decode-mime-encoded-words-in-string "vm-mime" (string))
(declare-function vm-su-subject "vm-summary" (message))

(require 'vm-vars)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

;; This file contains various low-level operations that address
;; incomaptibilities between Gnu and XEmacs.  Expect compiler warnings.

;; messages in the minibuffer

;; the chattiness levels are:
;; 0 - extremely quiet
;; 5 - medium
;; 7 - normal level
;; 10 - heavy debugging info

(defconst vm-log-buffer-name "*VM Log*"
  "Where `vm-log-level' records what VM had to say.")

(defvar vm-last-message-time nil
  "Real and CPU time when VM last logged a message, or nil.
What the intervals in the log are measured from.")

(defun vm-message-timing ()
  "The clock time, and how long it is since VM last said anything.
Answers a string.  Advances the interval both parts are measured from, so
call it once per message and only when the answer is going to be used."
  (let ((real (current-time))
	(cpu (get-internal-run-time))
	(previous vm-last-message-time))
    (setq vm-last-message-time (cons real cpu))
    (if (null previous)
	(format-time-string "%H:%M:%S.%3N" real)
      (format "%s +%.3fs +%.3fcpu"
	      (format-time-string "%H:%M:%S.%3N" real)
	      (float-time (time-subtract real (car previous)))
	      (float-time (time-subtract cpu (cdr previous)))))))

(defun vm-log-line (line)
  "Append LINE to the log buffer, trimming the front if it has grown.
`vm-log-max-lines' is how many are kept: the log runs for as long as Emacs
does, so something has to bound it, and what a reader wants is the end."
  (with-current-buffer (get-buffer-create vm-log-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert line "\n")
      (when (and vm-log-max-lines
		 (> (line-number-at-pos (point-max)) (* 2 vm-log-max-lines)))
	(goto-char (point-min))
	(forward-line (- (line-number-at-pos (point-max)) vm-log-max-lines 1))
	(delete-region (point-min) (point))))))

(defun vm-log-level-p (level)
  "Whether a message at LEVEL goes in the log.
Everything VM says goes in it -- the log is where a run is read afterwards,
and a message that was shown and not recorded is one nobody can go back to.
`vm-log-level' adds the levels that are recorded without being shown."
  (or (<= level vm-verbosity)
      (and vm-log-level (<= level vm-log-level))))

;;;###autoload
(defun vm-show-log ()
  "Show the log of what VM has been doing, and when.
It holds everything VM has said this session, timed, and whatever more
`vm-log-level' asks for."
  (interactive)
  (let ((buffer (get-buffer vm-log-buffer-name)))
    (if (and buffer (> (buffer-size buffer) 0))
	(display-buffer buffer)
      (vm-inform 0 "VM has not said anything yet"))))

(defun vm-emit-message (level text)
  "Show TEXT if LEVEL allows, and record it in the log.
Answers TEXT when it was shown, as `message' does, and nil otherwise.

The record is timed and the message shown is not: a time in the echo area
is in the way of what the message says, and each message is gone as the next
arrives anyway, so a run is read afterwards from the log."
  (let ((logging (vm-log-level-p level))
	(showing (<= level vm-verbosity)))
    (when logging
      (vm-log-line (format "%s [%d] %s" (vm-message-timing) level text)))
    (when showing
      (message "%s" text)
      text)))

(defmacro vm-with-timing (level name &rest body)
  "Run BODY and record at LEVEL how long it took.
NAME says what it was.  For the pieces of work that are neither a wait nor a
single message: a reader looking at a gap in the log needs to know whose it
is, and a phase that says only that it started leaves its own duration to be
guessed at."
  (declare (indent 2) (debug t))
  (let ((started (make-symbol "started"))
	(spent (make-symbol "spent"))
	(answer (make-symbol "answer")))
    `(let ((,started (float-time))
	   (,spent (get-internal-run-time)))
       (prog1 (progn ,@body)
	 (let ((,answer (float-time (time-subtract (get-internal-run-time)
						   ,spent))))
	   (vm-inform ,level "%s took %.2fs (%.2f cpu)" ,name
		      (- (float-time) ,started) ,answer))))))

(defun vm-inform (level &rest args)
  (let ((text (and (or (<= level vm-verbosity) (vm-log-level-p level))
		   (apply #'format-message args))))
    (when text
      (prog1 (vm-emit-message level text)
	(when (<= level vm-verbosity)
	  (vm-pause vm-verbal-time))))))

(defun vm-warn (l secs &rest args)
  "Give a warning at level L and display it for SECS seconds.  The
remaining arguments are passed to `message' to generate the warning
message."
  (when (or (<= l vm-verbosity) (vm-log-level-p l))
    (let ((warning (apply 'format args)))
      (unless (equal vm-current-warning warning)
	(setq vm-current-warning warning)
	(when (vm-emit-message l warning)
	  (vm-pause secs))))))

(defun vm-pause (seconds)
  "Leave the last message on screen for SECONDS, or until the user types.

`sit-for' rather than `sleep-for': both let a process filter run, and only
one of them lets the reader carry on.  A pause here is for reading a
message that the next one would overwrite -- it is never part of a
protocol, and a reader who has read it should not have to wait out the
rest (emacs-vm/vm#473)."
  (when (and seconds (> seconds 0))
    (sit-for seconds)))

;; garbage-collector result
(defconst gc-fields '(:conses :syms :miscs 
			      :chars :vector 
			      :floats :intervals :strings))

(defun vm-accept-process-output (process &optional timeout)
  "Accept output from PROCESS, optionally with TIMEOUT seconds.
Binds `inhibit-quit' to nil to allow user interrupts and avoid
the \"Blocking call to accept-process-output with quit inhibited\" warning.
Returns non-nil if output was received, nil on timeout."
  (let ((inhibit-quit nil))
    (accept-process-output process timeout)))

;; Make sure that interprogram-cut-function is defined
(unless (boundp 'interprogram-cut-function)
  (defvar interprogram-cut-function nil))

;; Taken from XEmacs as GNU Emacs is missing `replace-in-string' and defining
;; it may cause clashes with other packages defining it differently, in fact
;; we could also call the function `replace-regexp-in-string' as Roland
;; Winkler pointed out.
(defun vm-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
  (if (> (length str) 50)
      (let ((cfs case-fold-search))
	(with-temp-buffer
          (setq case-fold-search cfs)
	  (insert str)
	  (goto-char 1)
	  (while (re-search-forward regexp nil t)
	    (replace-match newtext t literal))
	  (buffer-string)))
    (let ((start 0) newstr)
      (while (string-match regexp str start)
        (setq newstr (replace-match newtext t literal str)
              start (+ (match-end 0) (- (length newstr) (length str)))
              str newstr))
      str)))

(defun vm-delete-non-matching-strings (regexp list &optional destructively)
  "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned."
  (or destructively (setq list (copy-sequence list)))
  (let ((curr list) (prev nil))
    (while curr
      (if (string-match regexp (car curr))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setcdr prev (cdr curr))
	  (setq curr (cdr curr)))))
    list ))

(defun vm-parse (string regexp &optional matchn matches)
  "Returns a list of strings by splitting STRING into items that match REGEXP.
  MATCHN can be used to select a match group from among that match REGEXP
  (default is 1).  MATCHES is the number of items to be returned
  (default is all).

This function is similar to a spring-split, but a bit more complex
  and flexible."
  (or matchn (setq matchn 1))
  (let (list tem)
    (store-match-data nil)
    (while (and (not (eq matches 0))
		(not (eq (match-end 0) (length string)))
		(string-match regexp string (match-end 0)))
      (and (integerp matches) (setq matches (1- matches)))
      (if (not (consp matchn))
	  (setq list (cons (substring string (match-beginning matchn)
				      (match-end matchn)) list))
	(setq tem matchn)
	(while tem
	  (if (match-beginning (car tem))
	      (setq list (cons (substring string
					  (match-beginning (car tem))
					  (match-end (car tem))) list)
		    tem nil)
	    (setq tem (cdr tem))))))
   (if (and (integerp matches) (match-end 0)
	    (not (eq (match-end 0) (length string))))
       (setq list (cons (substring string (match-end 0) (length string))
			 list)))
   (nreverse list)))

(defun vm-parse-addresses (string)
  "Given a STRING containing email addresses extracted from a header
field, parse it and return a list of individual email addresses."
  (if (null string)
      ()
    (let ((work-buffer (vm-make-multibyte-work-buffer)))
      (with-current-buffer work-buffer
        (unwind-protect
            (let (list start s char)
              (insert string)
              (goto-char (point-min))
	      ;; Remove useless white space  TX
              (while (re-search-forward "[\t\f\n\r]\\{1,\\}" nil t)
                (replace-match " "))
              (goto-char (point-min))
              (skip-chars-forward " \t\f\n\r")
              (setq start (point))
              (while (not (eobp))
                (skip-chars-forward "^\"\\\\,(")
                (setq char (following-char))
                (cond ((= char ?\\)
                       (forward-char 1)
                       (if (not (eobp))
                           (forward-char 1)))
                      ((= char ?,)
                       (setq s (buffer-substring start (point)))
                       (if (or (null (string-match "^[ \t\f\n\r]+$" s))
                               (not (string= s ""))) 
                           (setq list (cons s list)))
                       (skip-chars-forward ", \t\f\n\r")
                       (setq start (point)))
                      ((= char ?\")
                       (re-search-forward "[^\\\\]\"" nil 0))
                      ((= char ?\()
                       (let ((parens 1))
                         (forward-char 1)
                         (while (and (not (eobp)) (not (zerop parens)))
                           (re-search-forward "[()]" nil 0)
                           (cond ((or (eobp)
                                      (= (char-after (- (point) 2)) ?\\)))
                                 ((= (preceding-char) ?\()
                                  (setq parens (1+ parens)))
                                 (t
                                  (setq parens (1- parens)))))))))
              (setq s (buffer-substring start (point)))
              (if (and (null (string-match "^[ \t\f\n\r]+$" s))
                       (not (string= s "")))
                  (setq list (cons s list)))
              (mapcar 'vm-fix-quoted-address (reverse list)))
          (and work-buffer (kill-buffer work-buffer)))))))

(defun vm-fix-quoted-address (a)
  "Sometimes there are qp-encoded addresses not quoted by \" and thus we
need to add quotes or leave them undecoded.             RWF"
  (let ((da (vm-decode-mime-encoded-words-in-string a)))
    (if (string= da a)
        a
      (if (or (string-match "^\\s-*\\([^\"']*,[^\"']*\\)\\b\\s-*\\(<.*\\)" da)
              (string-match "^\\s-*\"'\\([^\"']+\\)'\"\\(.*\\)" da))
          (concat "\"" (match-string 1 da) "\" " (match-string 2 da))
        da))))

;; `vmrf-fix-quoted-address' was renamed to `vm-fix-quoted-address' above, in
;; 8.2.0, and not kept as an alias.  The `make-obsolete' that stood here named
;; `vm-quoted-address' as the replacement, which has never existed, and marked a
;; function that no longer exists as obsolete -- so it could never fire and only
;; misnamed the survivor.  Anyone who wants the old name back wants a
;; `defalias' to `vm-fix-quoted-address', not this.

(defun vm-parse-structured-header (string &optional sepchar keep-quotes)
  (if (null string)
      ()
    (let ((work-buffer (vm-make-work-buffer)))
      (buffer-disable-undo work-buffer)
      (with-current-buffer work-buffer
       (unwind-protect
	   (let ((list nil)
		 (nonspecials "^\"\\\\( \t\n\r\f")
		 start s char sp+sepchar)
	     (if sepchar
		 (setq nonspecials (concat nonspecials (list sepchar))
		       sp+sepchar (concat "\t\f\n\r " (list sepchar))))
	     (insert string)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward nonspecials)
	       (setq char (following-char))
	       (cond ((looking-at "[ \t\n\r\f]")
		      (delete-char 1))
		     ((= char ?\\)
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((and sepchar (= char sepchar))
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      (skip-chars-forward sp+sepchar)
		      (setq start (point)))
		     ((looking-at " \t\n\r\f")
		      (skip-chars-forward " \t\n\r\f"))
		     ((= char ?\")
		      (let ((done nil))
			(if keep-quotes
			    (forward-char 1)
			  (delete-char 1))
			(while (not done)
			  (if (null (re-search-forward "[\\\\\"]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (delete-char -1)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  (t (if (not keep-quotes)
					 (delete-char -1))
				     (setq done t)))))))
		     ((= char ?\()
		      (let ((done nil)
			    (pos (point))
			    (parens 1))
			(forward-char 1)
			(while (not done)
			  (if (null (re-search-forward "[\\\\()]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  ((char-equal char ?\()
				   (setq parens (1+ parens)))
				  (t
				   (setq parens (1- parens)
					 done (zerop parens))))))
			(delete-region pos (point))))))
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list))
	(and work-buffer (kill-buffer work-buffer)))))))

(defun vm-stale-compiled-files ()
  "VM's own files whose .elc is older than the .el beside it.
Answers a list of base names, or nil.

Only files VM has already loaded are asked about, so this says nothing about
a part of VM the session has not touched."
  (let (stale)
    (dolist (feature features (nreverse stale))
      (let ((name (symbol-name feature)))
        (when (or (string-prefix-p "vm-" name)
                  (member name '("vm" "tapestry")))
          (let* ((el (locate-library (concat name ".el")))
                 (elc (and el (concat el "c"))))
            (when (and el elc (file-exists-p elc)
                       (file-newer-than-file-p el elc))
              (push name stale))))))))

(defun vm-warn-about-stale-compiled-files ()
  "Say so, once, if VM is running compiled files from another build.
Two ways of telling: a .elc older than the .el beside it, and a .elc that
says it was compiled against a different VM (see `vm-assert-version').  The
second is the one that works on an installed tree, where every .elc is newer
than its source because `make install' copies it later.

Emacs loads a .elc in preference to a newer .el unless `load-prefer-newer\\='
says otherwise, and its own warning about that is one line among many at
startup.  What makes it worth repeating here is that VM\\='s files inline one
another: the accessors in vm-message.el are `defsubst\\='s, and the byte
compiler copies their bodies into every caller.  A stale .elc therefore runs
code that no longer matches the rest of VM, and fails somewhere with no
apparent connection to what is wrong.

That is not hypothetical.  #453 moved a message\\='s reverse link out of the
message vector; a vm-folder.elc compiled before it still ran the old
`vm-set-reverse-link-of\\=', which on a message built by the new
`vm-make-message\\=' is `(set nil ...)'.  Visiting any folder answered
\"(setting-constant nil)\" from inside `vm-build-message-list\\=', with nothing
in the backtrace to say why (#791)."
  (let* ((older (vm-stale-compiled-files))
         (mismatched (mapcar #'car vm-version-mismatched-files))
         (stale (delete-dups (append mismatched older))))
    (when stale
      (display-warning
       'vm
       (concat
        "VM is running compiled files left over from another build:\n  "
        (mapconcat #'identity stale " ")
        (when mismatched
          (concat "\n\nThose were compiled against "
                  (mapconcat (lambda (cell) (cdr cell))
                             vm-version-mismatched-files ", ")
                  ",\nand this is " (vm-version-stamp) "."))
        "\n\nEmacs loads the compiled file in preference to the newer source,"
        "\nand VM's files inline one another, so a stale one runs code that no"
        "\nlonger matches the rest of VM and fails in places that make no sense."
        "\n\nRecompile VM: `make' in the source tree, or M-x"
        " byte-recompile-directory\non "
        (or (file-name-directory (or (locate-library "vm-misc.el") "")) "VM's")
        " with a prefix argument.  Deleting the .elc files"
        "\nworks too; VM then runs interpreted, more slowly.")
       :warning))))

(defun vm-write-string (where string)
  (if (bufferp where)
      (save-current-buffer
	(set-buffer where)
	(goto-char (point-max))
	(let ((buffer-read-only nil))
	  (insert string)))
    (let ((temp-buffer (generate-new-buffer "*vm-work*")))
      (unwind-protect
	  (with-current-buffer temp-buffer
	    (setq selective-display nil)
	    (insert string)
	    (write-region (point-min) (point-max) where t 'quiet))
	(and temp-buffer (kill-buffer temp-buffer))))))

(defun vm-check-for-killed-summary ()
  "If the current folder's summary buffer has been killed, reset
the vm-summary-buffer variable and all the summary markers in the
folder so that it remains a valid folder."
  (and (bufferp vm-summary-buffer) (null (buffer-name vm-summary-buffer))
       (let ((mp vm-message-list))
	 (setq vm-summary-buffer nil)
	 (while mp
	   (vm-set-su-start-of (car mp) nil)
	   (vm-set-su-end-of (car mp) nil)
	   (setq mp (cdr mp))))))

(defun vm-check-for-killed-presentation ()
  "If the current folder's Presentation buffer has been killed, reset
the vm-presentation-buffer variable."
  (and (bufferp vm-presentation-buffer-handle)
       (null (buffer-name vm-presentation-buffer-handle))
       (progn
	 (setq vm-presentation-buffer-handle nil
	       vm-presentation-buffer nil))))

;;;###autoload
(defun vm-check-for-killed-folder ()
  "If the current buffer's Folder buffer has been killed, reset the
vm-mail-buffer variable."
  (and (bufferp vm-mail-buffer) (null (buffer-name vm-mail-buffer))
       (setq vm-mail-buffer nil)))

(put 'folder-read-only 'error-conditions '(folder-read-only error))
(put 'folder-read-only 'error-message "Folder is read-only")

(defun vm-abs (n) (if (< n 0) (- n) n))

(defun vm-last (list) 
  "Return the last cons-cell of LIST."
  (while (cdr-safe list) (setq list (cdr list)))
  list)

(defun vm-last-elem (list) 
  "Return the last element of LIST."
  (while (cdr-safe list) (setq list (cdr list)))
  (car list))

(defun vm-vector-to-list (vector)
  (let ((i (1- (length vector)))
	list)
    (while (>= i 0)
      (setq list (cons (aref vector i) list))
      (vm-decrement i))
    list ))

(defun vm-extend-vector (vector length &optional fill)
  (let ((vlength (length vector)))
    (if (< vlength length)
	(apply 'vector (nconc (vm-vector-to-list vector)
			      (make-list (- length vlength) fill)))
      vector )))

(defun vm-obarray-to-string-list (blobarray)
  (let ((list nil))
    (mapatoms (function (lambda (s) (setq list (cons (symbol-name s) list))))
	      blobarray)
    list ))

(defun vm-obarray-empty-p (blobarray)
  "Return t if nothing has been interned in BLOBARRAY.
An obarray used as a set is a vector, so it is never nil and cannot be
tested for emptiness with `null'."
  (let ((empty t))
    (mapatoms (function (lambda (_s) (setq empty nil))) blobarray)
    empty ))

(defun vm-zip-vectors (v1 v2)
  (if (= (length v1) (length v2))
      (let ((l1 (append v1 nil))
	    (l2 (append v2 nil)))
	(vconcat (vm-zip-lists l1 l2)))
    (error "Attempt to zip vectors of differing length: %s and %s" 
	   (length v1) (length v2))))

(defun vm-zip-lists (l1 l2)
  (cond ((or (null l1) (null l2))
	 (if (and (null l1) (null l2))
	     nil 
	   (error "Attempt to zip lists of differing length")))
	(t
	 (cons (car l1) (cons (car l2) (vm-zip-lists (cdr l1) (cdr l2)))))
	))

(defun vm-mapvector (proc vec)
  (let ((new-vec (make-vector (length vec) nil))
	(i 0)
	(n (length vec)))
    (while (< i n)
      (aset new-vec i (apply proc (aref vec i) nil))
      (setq i (1+ i)))
    new-vec))

(defun vm-mapcar (function &rest lists)
  "Apply function to all the curresponding elements of the remaining
argument lists.  The results are gathered into a list and returned.  

All the argument lists should be of the same length for this to be
well-behaved." 
  (let (arglist result)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (setq result (cons (apply function arglist) result))
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun vm-mapc (proc &rest lists)
  "Apply PROC to all the corresponding elements of the remaining
argument lists.  Discard any results.

All the argument lists should be of the same length for this to be
well-behaved." 
  (let (arglist)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (apply proc arglist)
      (setq lists (mapcar 'cdr lists)))))

(defun vm-delete (predicate list &optional retain)
  "Delete all elements satisfying PREDICATE from LIST and return
the resulting list.  If optional argument RETAIN is t, then
retain all elements that satisfy PREDICATE rather than deleting
them.  The original LIST is permanently modified."
  (let ((p list) 
	(retain (if retain 'not 'identity))
	prev)
    (while p
      (if (funcall retain (funcall predicate (car p)))
	  (if (null prev)
	      (setq list (cdr list) p list)
	    (setcdr prev (cdr p))
	    (setq p (cdr p)))
	(setq prev p p (cdr p))))
    list ))

(defun vm-delete-common-elements (list1 list2 pred)
  "Takes two sorted lists of unique values with dummy headers and
destructively deletes all their common elements.  PRED is used as
the function for < comparison."
  (rplacd list1 (sort (cdr list1) pred))
  (rplacd list2 (sort (cdr list2) pred))
  (while (and (cdr list1) (cdr list2))
    (cond ((equal (car (cdr list1)) (car (cdr list2)))
	   (rplacd list1 (cdr (cdr list1)))
	   (rplacd list2 (cdr (cdr list2))))
	  ((apply pred (car (cdr list1)) (car (cdr list2)) nil)
	   (setq list1 (cdr list1)))
	  (t
	   (setq list2 (cdr list2)))
	  )))

(defun vm-elems (n list)
  "Select the first N elements of LIST and return them as a list."
  (let (res)
    (while (and list (> n 0))
      (setq res (cons (car list) res))
      (setq list (cdr list))
      (setq n (1- n)))
    (nreverse res)))

(defun vm-find (list pred)
  "Find the first element of LIST satisfying PRED and return its position"
  (let ((n 0))
    (while (and list (not (apply pred (car list) nil)))
      (setq list (cdr list))
      (setq n (1+ n)))
    (if list n nil)))

(defun vm-find-all (list pred)
  "Find all the elements of LIST satisfying PRED and return thier list"
  (let ((n 0) (res nil))
    (while list 
      (when (apply pred (car list) nil)
	(setq res (cons (car list) res)))
      (setq list (cdr list))
      (setq n (1+ n)))
    (nreverse res)))

(defun vm-elems-of (list)
  "Return the set of elements of LIST as a list."
  (let ((res nil))
    (while list
      (unless (member (car list) res)
	(setq res (cons (car list) res)))
      (setq list (cdr list)))
    (nreverse res)))

(defun vm-for-all (list pred)
  (catch 'fail
    (progn
      (while list
	(if (apply pred (car list) nil)
	    (setq list (cdr list))
	  (throw 'fail nil)))
      t)))

;;;###autoload (autoload 'vm-view-file-other-frame "vm-misc" nil t)
(defalias 'vm-view-file-other-frame #'view-file-other-frame)


(defun vm-generate-new-unibyte-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    buffer))

(defun vm-generate-new-multibyte-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (set-buffer-multibyte t))
    buffer))

(defalias 'vm-abbreviate-file-name #'abbreviate-file-name)

(defalias 'vm-select-frame-set-input-focus #'select-frame-set-input-focus)

(defun vm-get-buffer-window (buffer &optional which-frames _which-devices)
  (or (get-buffer-window buffer which-frames)
      (and vm-search-other-frames
	   (get-buffer-window buffer t))))

(defun vm-get-visible-buffer-window (buffer &optional
					    which-frames _which-devices)
  (or (get-buffer-window buffer which-frames)
      (and vm-search-other-frames
	   (get-buffer-window buffer 'visible))))

(defun vm-force-mode-line-update ()
  "Force a mode line update in all frames."
  ;; FIXME: Do all callers really need to update *all* frames?
  (force-mode-line-update t))

(defun vm-delete-directory-file-names (list)
  (vm-delete 'file-directory-p list))

(defun vm-delete-backup-file-names (list)
  (vm-delete 'backup-file-name-p list))

(defun vm-delete-auto-save-file-names (list)
  (vm-delete 'auto-save-file-name-p list))

(defun vm-delete-index-file-names (list)
  (vm-delete 'vm-index-file-name-p list))

(defun vm-delete-directory-names (list)
  (vm-delete 'file-directory-p list))

(defun vm-index-file-name-p (file)
  (and (file-regular-p file)
       (stringp vm-index-file-suffix)
       (let ((str (concat (regexp-quote vm-index-file-suffix) "$")))
	 (string-match str file))
       t ))

(defun vm-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurrence of a string in the list,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
  (let ((hashtable vm-delete-duplicates-obarray)
	(new-list nil)
	sym-string sym)
    (fillarray hashtable 0)
    (while list
      (setq sym-string
	    (if hack-addresses
		(nth 1 (funcall vm-chop-full-name-function (car list)))
	      (car list))
	    sym-string (or sym-string "-unparseable-garbage-")
	    sym (intern (if hack-addresses (downcase sym-string) sym-string)
			hashtable))
      (if (boundp sym)
	  (and all (setcar (symbol-value sym) nil))
	(setq new-list (cons (car list) new-list))
	(set sym new-list))
      (setq list (cdr list)))
    (delq nil (nreverse new-list))))

(defun vm-delqual (ob list)
  (let ((prev nil)
	(curr list))
    (while curr
      (if (not (equal ob (car curr)))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setq curr (cdr curr))
	  (setcdr prev curr))))
    list ))

(defun vm-copy-local-variables (buffer &rest variables)
  (let ((values (mapcar 'symbol-value variables)))
    (with-current-buffer buffer
      (vm-mapc 'set variables values))))

(put 'folder-empty 'error-conditions '(folder-empty error))
(put 'folder-empty 'error-message "Folder is empty")
(put 'unrecognized-folder-type 'error-conditions
     '(unrecognized-folder-type error))
(put 'unrecognized-folder-type 'error-message "Unrecognized folder type")

(defun vm-error-if-folder-empty ()
  (while (null vm-message-list)
    (if vm-folder-type
	(signal 'unrecognized-folder-type nil)
      (signal 'folder-empty nil))))

(defconst vm-cache-folder-type-suffix ".mboxcl2"
  "The name suffix VM gives a cache file it creates, and the type it writes it in.
A cache is VM's own file and VM writes every message in it, so unlike any
other folder its type is known and can be stated where
`vm-folder-type-by-extension-alist' reads it back.  mboxcl2 because the lengths
make the message boundaries exact for arbitrary mail, which a cache holds.

Said in the name rather than in a header inside the folder: a claim written
into a folder outlives the belief that produced it, and a wrong one then
survives the fix.  See dev/docs/design/folder-type.org.")

(defconst vm-cache-folder-name-regexp
  (concat "\\`\\(imap\\|pop\\)-cache-[0-9a-f]+"
	  "\\(" (regexp-quote vm-cache-folder-type-suffix) "\\)?\\'")
  "Matches the name of a file VM uses as the local cache of a server folder.
`vm-imap-make-filename-for-spec' and `vm-pop-make-filename-for-spec' build
these names, from a prefix, the MD5 of the maildrop specification, and for a
cache VM created the type suffix.")

(defun vm-cache-folder-name-p (file)
  "Return non-nil if FILE is VM's local cache of a POP or IMAP folder.
Judged by the name, which is all there is to go on: the maildrop the cache
belongs to is deliberately not recorded in it, so a cache folder cannot be
reconnected to its server by reading it."
  (and file
       (string-match-p vm-cache-folder-name-regexp
		       (file-name-nondirectory file))))

(defun vm-cache-file-in-use (base)
  "The cache file to use, given BASE, its name without a type suffix.
BASE where that file exists, so a cache made before VM named them keeps its
name and goes on being read as whatever it is: renaming it would say a type of
it that may not be true, and refusing it would mean refetching the mailbox.

BASE with `vm-cache-folder-type-suffix' otherwise, which is the name a new
cache gets and the type it is then written in.

Both existing means a cache that was converted with the old file left beside
it.  The suffixed one is the cache, and the other is named in a warning rather
than passed over in silence, since it is the one holding the older mail."
  (let ((named (concat base vm-cache-folder-type-suffix)))
    (cond ((file-exists-p named)
	   (when (file-exists-p base)
	     (vm-warn 0 2 "Using cache %s, ignoring %s"
		      (file-name-nondirectory named)
		      (file-name-nondirectory base)))
	   named)
	  ((file-exists-p base) base)
	  (t named))))

(defun vm-copy (object)
  "Make a copy of OBJECT, which could be a list, vector, string or marker."
  (cond ((consp object)
	 (let (return-value cons)
	   (setq return-value (cons (vm-copy (car object)) nil)
		 cons return-value
		 object (cdr object))
	   (while (consp object)
	     (setcdr cons (cons (vm-copy (car object)) nil))
	     (setq cons (cdr cons)
		   object (cdr object)))
	   (setcdr cons object)
	   return-value ))
	((vectorp object) (apply 'vector (mapcar 'vm-copy object)))
	((stringp object) (copy-sequence object))
	((markerp object) (copy-marker object))
	(t object)))

(defun vm-run-hook-on-message (hook-variable message)
  (with-current-buffer (vm-buffer-of message)
    (save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(run-hooks hook-variable)))))


(defun vm-run-hook-on-message-with-args (hook-variable message &rest args)
  (with-current-buffer (vm-buffer-of message)
    (save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(apply 'run-hook-with-args hook-variable args)))))


(defun vm-error-free-call (function &rest args)
  (condition-case nil
      (apply function args)
    (error nil)))

(put 'beginning-of-folder 'error-conditions '(beginning-of-folder error))
(put 'beginning-of-folder 'error-message "Beginning of folder")
(put 'end-of-folder 'error-conditions '(end-of-folder error))
(put 'end-of-folder 'error-message "End of folder")

(defun vm-timezone-make-date-sortable (string)
  (or (cdr (assq string vm-sortable-date-alist))
      (let ((vect (vm-parse-date string))
	    (date (vm-parse (current-time-string) " *\\([^ ]+\\)")))
	;; if specified date is incomplete fill in the holes
	;; with useful information, defaulting to the current
	;; date and timezone for everything except hh:mm:ss which
	;; defaults to midnight.
	(if (equal (aref vect 1) "")
	    (aset vect 1 (nth 2 date)))
	(if (equal (aref vect 2) "")
	    (aset vect 2 (nth 1 date)))
	(if (equal (aref vect 3) "")
	    (aset vect 3 (nth 4 date)))
	(if (equal (aref vect 4) "")
	    (aset vect 4 "00:00:00"))
	(if (equal (aref vect 5) "")
	    (aset vect 5 (vm-current-time-zone)))
	;; save this work so we won't have to do it again
	(setq vm-sortable-date-alist
	      (cons (cons string
			  (condition-case nil
			      (timezone-make-date-sortable
			       (format "%s %s %s %s %s"
				       (aref vect 1)
				       (aref vect 2)
				       (aref vect 3)
				       (aref vect 4)
				       (aref vect 5)))
			    (error "1970010100:00:00")))
		    vm-sortable-date-alist))
	;; return result
	(cdr (car vm-sortable-date-alist)))))

(defun vm-current-time-zone ()
  (or (condition-case nil
	  (let* ((zone (car (current-time-zone)))
		 (absmin (/ (vm-abs zone) 60)))
	    (format "%c%02d%02d" (if (< zone 0) ?- ?+)
		    (/ absmin 60) (% absmin 60)))
	(error nil))
      (let ((temp-buffer (vm-make-work-buffer)))
	(condition-case nil
	    (unwind-protect
		(with-current-buffer temp-buffer
		  (call-process "date" nil temp-buffer nil)
		  (nth 4 (vm-parse (vm-buffer-string-no-properties)
				   " *\\([^ ]+\\)")))
	      (and temp-buffer (kill-buffer temp-buffer)))
	  (error nil)))
      ""))

(defun vm-parse-date (date)
  (let ((weekday "")
	(monthday "")
	(month "")
	(year "")
	(hour "")
	(timezone "")
	(start nil)
	string
	(case-fold-search t))
    (if (string-match "sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat" date)
	(setq weekday (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec" date)
	(setq month (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?" date)
	(setq hour (substring date (match-beginning 0) (match-end 0))))
    (cond ((string-match "[^a-z][+-][0-9][0-9][0-9][0-9]" date)
	   (setq timezone (substring date (1+ (match-beginning 0))
				     (match-end 0))))
	  ((or (string-match "e[ds]t\\|c[ds]t\\|p[ds]t\\|m[ds]t" date)
	       (string-match "ast\\|nst\\|met\\|eet\\|jst\\|bst\\|ut" date)
	       (string-match "gmt\\([+-][0-9]+\\)?" date))
	   (setq timezone (substring date (match-beginning 0) (match-end 0)))))
    (while (and (or (zerop (length monthday))
		    (zerop (length year)))
		(string-match "\\(^\\| \\)\\([0-9]+\\)\\($\\| \\)" date start))
      (setq string (substring date (match-beginning 2) (match-end 2))
	    start (match-end 0))
      (cond ((and (zerop (length monthday))
		  (<= (length string) 2))
	     (setq monthday string))
	    ((= (length string) 2)
	     (if (< (string-to-number string) 70)
		 (setq year (concat "20" string))
	       (setq year (concat "19" string))))
	    (t (setq year string))))
    
    (aset vm-parse-date-workspace 0 weekday)
    (aset vm-parse-date-workspace 1 monthday)
    (aset vm-parse-date-workspace 2 month)
    (aset vm-parse-date-workspace 3 year)
    (aset vm-parse-date-workspace 4 hour)
    (aset vm-parse-date-workspace 5 timezone)
    vm-parse-date-workspace))

(defun vm-should-generate-summary ()
  (cond ((eq vm-startup-with-summary t) t)
	((integerp vm-startup-with-summary)
	 (let ((n vm-startup-with-summary))
	   (cond ((< n 0) (null (nth (vm-abs n) vm-message-list)))
		 ((= n 0) nil)
		 (t (nth (1- n) vm-message-list)))))
	(vm-startup-with-summary t)
	(t nil)))

(defun vm-find-composition-buffer (&optional not-picky)
  (let ((b-list (buffer-list)) choice alternate)
    (save-excursion
     (while b-list
       (set-buffer (car b-list))
       (if (eq major-mode 'mail-mode)
	   (if (buffer-modified-p)
	       (setq choice (current-buffer)
		     b-list nil)
	     (and not-picky (null alternate)
		  (setq alternate (current-buffer)))
	     (setq b-list (cdr b-list)))
	 (setq b-list (cdr b-list))))
    (or choice alternate))))

(defun vm-get-file-buffer (file)
  "Like get-file-buffer, but also checks buffers against FILE's truename"
  (or (get-file-buffer file)
      (and (fboundp 'file-truename)
	   (get-file-buffer (file-truename file)))
      (and (fboundp 'find-buffer-visiting)
	   (find-buffer-visiting file))))

;; The following function is not working correctly on Gnu Emacs 23.
;; So we do it ourselves.
(defun vm-delete-auto-save-file-if-necessary ()
  (when (and buffer-auto-save-file-name delete-auto-save-files
	       (not (string= buffer-file-name buffer-auto-save-file-name))
	       (file-newer-than-file-p 
		buffer-auto-save-file-name buffer-file-name))
      (condition-case ()
	  (if (save-window-excursion
		(with-output-to-temp-buffer "*Directory*"
		  (buffer-disable-undo standard-output)
		  (save-excursion
		    (let ((switches dired-listing-switches)
			  (file buffer-file-name)
			  (save-file buffer-auto-save-file-name))
		      (if (file-symlink-p buffer-file-name)
			  (setq switches (concat switches "L")))
		      (set-buffer standard-output)
		      ;; Use insert-directory-safely, not insert-directory,
		      ;; because these files might not exist.  In particular,
		      ;; FILE might not exist if the auto-save file was for
		      ;; a buffer that didn't visit a file, such as "*mail*".
		      ;; The code in v20.x called `ls' directly, so we need
		      ;; to emulate what `ls' did in that case.
		      (insert-directory-safely save-file switches)
		      (insert-directory-safely file switches))))
		(yes-or-no-p 
		 (format "Delete auto save file %s? " 
			 buffer-auto-save-file-name)))
	      (delete-file buffer-auto-save-file-name))
	(file-error nil))
    (set-buffer-auto-saved)))

(defun vm-set-region-face (start end face)
  (let ((e (vm-make-extent start end)))
    (vm-set-extent-property e 'face face)))

(defun vm-default-buffer-substring-no-properties (beg end &optional buffer)
  (let ((s (if buffer
	       (with-current-buffer buffer
		 (buffer-substring beg end))
	     (buffer-substring beg end))))
    (set-text-properties 0 (length s) nil s)
    (copy-sequence s)))

(defalias 'vm-buffer-substring-no-properties #'buffer-substring-no-properties)

(defun vm-buffer-string-no-properties ()
  (vm-buffer-substring-no-properties (point-min) (point-max)))

(defalias 'vm-substring-no-properties #'substring-no-properties)

(defun vm-insert-region-from-buffer (buffer &optional start end)
  (let ((target-buffer (current-buffer)))
    (set-buffer buffer)
    (save-restriction
      (widen)
      (or start (setq start (point-min)))
      (or end (setq end (point-max)))
      (set-buffer target-buffer)
      (insert-buffer-substring buffer start end)
      (set-buffer buffer))
    (set-buffer target-buffer)))

(defalias 'vm-extent-property #'overlay-get)

(defalias 'vm-extent-object #'overlay-buffer)

(defalias 'vm-set-extent-property #'overlay-put)

(defalias 'vm-set-extent-endpoints #'move-overlay)

(defalias 'vm-make-extent #'make-overlay)

(defalias 'vm-extent-end-position #'overlay-end)

(defalias 'vm-extent-start-position #'overlay-start)

(defalias 'vm-next-extent-change #'next-overlay-change)

(defalias 'vm-previous-extent-change #'previous-overlay-change)

(defalias 'vm-detach-extent #'delete-overlay)

(defalias 'vm-delete-extent #'delete-overlay)

(defalias 'vm-disable-extents #'remove-overlays)

(defalias 'vm-extent-properties #'overlay-properties)

(defun vm-map-extents (function)
  "Map FUNCTION over the overlays in the current buffer.
FUNCTION is called with two arguments: an overlay and a dummy argument
which should be ignored."
  ;; This is based on old code in vm-page.el, rev. 1335
  (let ((o-lists (overlay-lists)))
    (dolist (o (car o-lists)) (funcall function o nil))
    (dolist (o (cdr o-lists)) (funcall function o nil))))

(defun vm-extent-at (pos &optional property)
  "Find an extent at POS in the current buffer having PROPERTY.
PROPERTY defaults nil, meaning any extent will do.

Not necessarily the smallest overlay there, XEmacs's `extent-at' having
answered that and this not."
  (let ((o-list (overlays-at pos))
	(o nil))
    (if (null property)
	(car o-list)
      (while o-list
	(if (overlay-get (car o-list) property)
	    (setq o (car o-list)
		  o-list nil)
	  (setq o-list (cdr o-list))))
      o)))

(defun vm-make-tempfile (&optional filename-suffix proposed-filename)
  (let ((modes (default-file-modes))
	(file (vm-make-tempfile-name filename-suffix proposed-filename)))
    (unwind-protect
	(progn
	  (set-default-file-modes (vm-octal 600))
	  (vm-error-free-call 'delete-file file)
	  (write-region (point) (point) file nil 0))
      (set-default-file-modes modes))
    file ))

(defun vm-make-tempfile-name (&optional filename-suffix proposed-filename)
  (if (stringp proposed-filename)
      (setq proposed-filename (file-name-nondirectory proposed-filename)))
  (let (filename)
    (cond ((and (stringp proposed-filename)
		(not (file-exists-p
		      (setq filename (convert-standard-filename
				      (expand-file-name
				       proposed-filename
				       vm-temp-file-directory))))))
	   t )
	  ((stringp proposed-filename)
	   (let ((done nil))
	     (while (not done)
	       (setq filename (convert-standard-filename
			       (expand-file-name
				(format "%d-%s"
					vm-tempfile-counter
					proposed-filename)
				vm-temp-file-directory))
		     vm-tempfile-counter (1+ vm-tempfile-counter)
                     done (not (file-exists-p filename))))))
	  (t
	   (let ((done nil))
	     (while (not done)
	       (setq filename (convert-standard-filename
			       (expand-file-name
				(format "vm%d%d%s"
					vm-tempfile-counter
					(random 100000000)
					(or filename-suffix ""))
				vm-temp-file-directory))
		     vm-tempfile-counter (1+ vm-tempfile-counter)
		     done (not (file-exists-p filename)))))))
    filename ))

(defun vm-make-work-buffer (&optional name)
  "Create a unibyte buffer with NAME for VM to do its work in
encoding/decoding, conversions, subprocess communication etc."
  (let ((work-buffer (vm-generate-new-unibyte-buffer 
		      (or name "*vm-workbuf*"))))
    (buffer-disable-undo work-buffer)
;; probably not worth doing since no one sets buffer-offer-save
;; non-nil globally, do they?
    work-buffer ))

(defun vm-make-multibyte-work-buffer (&optional name)
  (let ((work-buffer (vm-generate-new-multibyte-buffer 
		      (or name "*vm-workbuf*"))))
    (buffer-disable-undo work-buffer)
;; probably not worth doing since no one sets buffer-offer-save
;; non-nil globally, do they?
    work-buffer ))

(defun vm-insert-char (char &optional count _ignored buffer)
  "Insert COUNT copies of CHAR into BUFFER, or the current buffer.
IGNORED is there because XEmacs's `insert-char', which this stood in for,
took an argument here that Emacs's does not."
  (if (or (null buffer) (eq buffer (current-buffer)))
      (insert-char char count)
    (with-current-buffer buffer
      (insert-char char count))))

(defun vm-symbol-lists-intersect-p (list1 list2)
  (catch 'done
    (while list1
      (and (memq (car list1) list2)
	   (throw 'done t))
      (setq list1 (cdr list1)))
    nil ))

(defun vm-folder-buffer-value (var)
  (if vm-mail-buffer
      (with-current-buffer 
	  vm-mail-buffer
	(symbol-value var))
    (symbol-value var)))

(defsubst vm-with-string-as-temp-buffer (string function)
  (let ((work-buffer (vm-make-multibyte-work-buffer)))
    (unwind-protect
	(with-current-buffer work-buffer
	  (insert string)
	  (funcall function)
	  (buffer-string))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-string-assoc (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car (car list))))
	       (= (match-end 0) (length (car (car list)))))
	  (setq found t)
	(setq list (cdr list))))
    (car list)))

(defun vm-nonneg-string (n)
  (if (< n 0)
      "?"
    (int-to-string n)))

(defun vm-string-member (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car list)))
	       (= (match-end 0) (length (car list))))
	  (setq found t)
	(setq list (cdr list))))
    list))

(defun vm-string-equal-ignore-case (str1 str2)
  (let ((case-fold-search t)
	(reg (regexp-quote str1)))
    (and (equal 0 (string-match reg str2))
	 (= (match-end 0) (length str2)))))

(defun vm-match-data ()
  (let ((n (1- (/ (length (match-data)) 2)))
        (list nil))
    (while (>= n 0)
      (setq list (cons (match-beginning n) 
                       (cons (match-end n) list))
            n (1- n)))
    list))

(defun vm-time-difference (t1 t2)
  (let (usecs secs 65536-secs carry)
    (setq usecs (- (nth 2 t1) (nth 2 t2)))
    (if (< usecs 0)
	(setq carry 1
	      usecs (+ usecs 1000000))
      (setq carry 0))
    (setq secs (- (nth 1 t1) (nth 1 t2) carry))
    (if (< secs 0)
	 (setq carry 1
	       secs (+ secs 65536))
      (setq carry 0))
    (setq 65536-secs (- (nth 0 t1) (nth 0 t2) carry))
    (+ (* 65536-secs 65536)
       secs
       (/ usecs 1e6))))

(defalias 'vm-char-to-int #'identity)

(defalias 'vm-charsets-in-region #'find-charset-region)

(defalias 'vm-coding-system-p #'coding-system-p)

(defalias 'vm-coding-system-name #'identity)

(defun vm-coding-system-name-no-eol (coding-system)
  (coding-system-change-eol-conversion coding-system nil))

(defun vm-get-file-line-ending-coding-system (file)
  (let ((coding-system-for-read  (vm-binary-coding-system))
	(work-buffer (vm-make-work-buffer)))
    (unwind-protect
	(with-current-buffer work-buffer
	  (condition-case nil
	      (insert-file-contents file nil 0 4096)
	    (error nil))
	  (goto-char (point-min))
	  (cond ((re-search-forward "[^\r]\n" nil t)
		 'raw-text-unix)
		((re-search-forward "\r[^\n]" nil t)
		 'raw-text-mac)
		((search-forward "\r\n" nil t)
		 'raw-text-dos)
		(t (vm-line-ending-coding-system))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-new-folder-line-ending-coding-system ()
  (cond ((eq vm-default-new-folder-line-ending-type nil)
	 (vm-line-ending-coding-system))
	((eq vm-default-new-folder-line-ending-type 'lf)
	 'raw-text-unix)
	((eq vm-default-new-folder-line-ending-type 'crlf)
	 'raw-text-dos)
	((eq vm-default-new-folder-line-ending-type 'cr)
	 'raw-text-mac)
	(t
	 (vm-line-ending-coding-system))))

(defun vm-collapse-whitespace ()
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]+" nil 0)
    (replace-match
     (apply #'propertize " " (text-properties-at (match-beginning 0))) t t)))

(defvar vm-paragraph-prefix-regexp "^[ >]*"
  "A regexp used by `vm-forward-paragraph' to match paragraph prefixes.")

(defvar vm-empty-line-regexp "^[ \t>]*$"
  "A regexp used by `vm-forward-paragraph' to match paragraph prefixes.")

(defun vm-skip-empty-lines ()
  "Move forward as long as current line matches `vm-empty-line-regexp'."
  (while (and (not (eobp)) 
	      (looking-at vm-empty-line-regexp))
    (forward-line 1)))

(defun vm-forward-paragraph ()
  "Move forward to end of paragraph and do it also right for quoted text.
As a side-effect set `fill-prefix' to the paragraphs prefix.
Returns t if there was a line longer than `fill-column'."
  (let ((long-line)
	(line-no 1)
	len-fill-prefix)
    (forward-line 0)			; cover for bad fill-region fns
    (setq fill-prefix nil)
    (while (and 
	    ;; stop at end of buffer
	    (not (eobp)) 
	    ;; empty lines break paragraphs
	    (not (looking-at "^[ \t]*$"))
	    ;; do we see a prefix
	    (looking-at vm-paragraph-prefix-regexp)
	    (let ((m (match-string 0))
		  lenm)
	      (or (and (null fill-prefix)
		       ;; save prefix for next line
		       (setq fill-prefix m len-fill-prefix (length m)))
		  ;; is it still the same prefix?
		  (string= fill-prefix m)
		  ;; or is it just shorter by whitespace on the second line
		  (and 
		   (= line-no 2)
		   (< (setq lenm (length m)) len-fill-prefix)
		   (string-match "^[ \t]+$" (substring fill-prefix lenm))
		   ;; then save new shorter prefix
		   (setq fill-prefix m len-fill-prefix lenm)))))
      (end-of-line)
      (setq line-no (1+ line-no))
      (setq long-line (or long-line (> (current-column) fill-column)))
      (forward-line 1))
    long-line))

(defun vm-fill-prefix-leaves-room-p ()
  "Whether `fill-prefix' leaves any room for text inside `fill-column'.
A paragraph whose prefix is as wide as the column cannot be filled to
anything but one word a line, which is worse than the long lines it was
filled to be rid of.  `vm-forward-paragraph' reads a paragraph's
indentation as its prefix, and an HTML converter asked for a very wide page
indents a centred paragraph by hundreds of columns (#540)."
  (or (null fill-prefix)
      (< (string-width fill-prefix) fill-column)))

(defun vm-fill-paragraphs-containing-long-lines (width start end)
  "Fill paragraphs spanning more than WIDTH columns in region START to END.
If WIDTH is the symbol window-width, the current width of the Emacs window
is used; if it is nil, vm-paragraph-fill-column is.  The column filled to is
vm-paragraph-fill-column whatever WIDTH says.

vm-word-wrap-paragraphs non-nil wraps the long lines instead, leaving
every existing line break where it is.  That is the setting to use on
quoted text: filling joins the lines of a paragraph before breaking them
again, so a quoted block is drawn into the paragraph above it and its
markers end up mid-line.

In order to fill also quoted text you will need filladapt.el, the adaptive
filling of GNU Emacs not working correctly here."
  (when (eq width 'window-width)
    (setq width (- (window-width (get-buffer-window (current-buffer))) 1)))
  ;; No WIDTH at all means every line longer than the column it would be
  ;; wrapped to is long.  `vm-word-wrap-paragraphs' documents itself as
  ;; needing nothing else set, and its three callers pass
  ;; `vm-fill-paragraphs-containing-long-lines', which is nil for a reader who
  ;; asked only to wrap: the longlines call this replaced took no width at all,
  ;; so nothing noticed until it did (emacs-vm/vm#834).
  (unless width
    (setq width vm-paragraph-fill-column))
  (if vm-word-wrap-paragraphs
      (vm-word-wrap-long-lines width vm-paragraph-fill-column start end)
    (save-excursion
      (let ((buffer-read-only nil)
	    (fill-column vm-paragraph-fill-column)
	    (adaptive-fill-mode nil)
	    (abbrev-mode nil)
	    (fill-prefix nil)
	    (filled 0)
	    (_message (if (car vm-message-pointer)
			  (vm-su-subject (car vm-message-pointer))
			(buffer-name)))
	    (needmsg (> (- end start) 12000)))
      
	(if needmsg
	    (vm-inform 5 "Filling message to column %d" fill-column))
      
	;; we need a marker for the end since this position might change 
	(or (markerp end) (setq end (vm-marker end)))
	(goto-char start)
      
	(while (< (point) end)
	  (setq start (point))
	  (vm-skip-empty-lines)
	  (when (and (< (point) end)	; if no newline at the end
		     (let ((fill-column width)) (vm-forward-paragraph))
		     (vm-fill-prefix-leaves-room-p))
	    (fill-region start (point))
	    (setq filled (1+ filled))))
      
	;; Turning off these messages because they go by too fast and
	;; are not particularly enlightening.  USR, 2010-01-26
	))))

(defun vm-word-wrap-long-lines (width column start end)
  "Wrap lines longer than WIDTH columns to COLUMN, between START and END.

Each over-long line is filled on its own, so no existing line break is
removed.  That is the difference from filling, and the reason this exists:
`fill-region' joins the lines of a paragraph before breaking them again,
which pulls a quoted block into the paragraph above it.

A word longer than COLUMN is left whole rather than broken, so a long URL
survives.

This used the longlines package until 2026, which had been obsolete
since Emacs 24.4 and warned as it was loaded (emacs-vm/vm#817).  Its output
differed only in leaving a trailing space on each wrapped line, which was
how it marked its own soft breaks; VM never unwrapped them."
  (let ((end (copy-marker end))
	(buffer-read-only nil)
	(fill-column column)
	(adaptive-fill-mode nil)
	(fill-prefix nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(when (> (- (line-end-position) (point)) width)
	  (fill-region-as-paragraph (point) (line-end-position)))
	(forward-line 1)))))

(defun vm-make-message-id ()
  (let (hostname
	(time (current-time)))
    (setq hostname (cond ((string-match "\\." (system-name))
			  (system-name))
			 ((and (stringp mail-host-address)
			       (string-match "\\." mail-host-address))
			  mail-host-address)
			 (t "gargle.gargle.HOWL")))
    (format "<%d.%d.%d.%d@%s>"
	    (car time) (nth 1 time) (nth 2 time)
	    (random 1000000)
	    hostname)))

(defvar vm-session-trace-max-size)

(defun vm-insert-one-session-trace (buffer)
  "Insert BUFFER's text, the middle left out if it is too long to send.
`vm-session-trace-max-size' says how much; half of it comes from the start and
half from the end, which are the two ends that say anything.  Nil carries the
whole trace."
  (let* ((size (buffer-size buffer))
	 (limit vm-session-trace-max-size))
    (if (or (null limit) (<= size limit))
	(insert-buffer-substring buffer)
      ;; the positions are BUFFER's, read here rather than by making it
      ;; current: `insert-buffer-substring' inserts into the buffer that is
      ;; current, so making BUFFER current copies the trace into itself
      (let* ((first (with-current-buffer buffer (point-min)))
	     (last (with-current-buffer buffer (point-max)))
	     (half (/ limit 2)))
	(insert-buffer-substring buffer first (+ first half))
	(insert (format (concat "\n[%d characters left out of the middle of"
				" this trace; set vm-session-trace-max-size"
				" to nil for the whole of it]\n")
			(- size limit)))
	(insert-buffer-substring buffer (- last half) last)))))

(defun vm-insert-session-traces (protocol buffers)
  "Insert the text of BUFFERS into a bug report, newest first.
PROTOCOL names them in the heading, \"IMAP\" or \"POP\".  A dead buffer is
named and skipped rather than left out silently: a report that is missing a
session says so."
  (insert "\n\n" protocol " Trace buffers - most recent first\n\n")
  (dolist (buffer buffers)
    (insert "----" (format "%s" buffer) "----------\n")
    (if (buffer-live-p buffer)
	(vm-insert-one-session-trace buffer)
      (insert "(this buffer is gone)\n")))
  (insert "--------------------------------------------------\n"))

(defun vm-keep-some-buffers (buffer ring-variable number-to-keep 
				    &optional rename-prefix)
  "Keep the BUFFER in the variable RING-VARIABLE, with NUMBER-TO-KEEP
being the maximum number of buffers kept.  If necessary, the
RING-VARIABLE is pruned.  If the optional argument string
RENAME-PREFIX is given BUFFER is renamed by adding the prefix at the
front before adding it to the RING-VARIABLE."
  (unless rename-prefix
    (setq rename-prefix "saved "))
  (if (memq buffer (symbol-value ring-variable))
      (set ring-variable (delq buffer (symbol-value ring-variable)))
    (with-current-buffer buffer
      (rename-buffer (concat rename-prefix (buffer-name)) t)))
  (set ring-variable (cons buffer (symbol-value ring-variable)))
  (set ring-variable (vm-delete 'buffer-name
				(symbol-value ring-variable) t))
  (if (not (eq number-to-keep t))
      (let ((extras (nthcdr (or number-to-keep 0)
			    (symbol-value ring-variable))))
	(mapc (function
	       (lambda (b)
		 (when (and (buffer-name b)
			    (or (not (buffer-modified-p b))
				(not (with-current-buffer b
				       buffer-offer-save))))
		   (kill-buffer b))))
	      extras)
	(and (symbol-value ring-variable) extras
	     (setcdr (memq (car extras) (symbol-value ring-variable))
		     nil)))))

(defvar enable-multibyte-characters)
(defvar buffer-display-table)
(defun vm-fsfemacs-nonmule-display-8bit-chars ()
  (cond ((not enable-multibyte-characters)
	 (let* (tab (i 160))
	   ;; We need the function make-display-table, but it is
	   ;; in disp-table.el, which overwrites the value of
	   ;; standard-display-table when it is loaded, which
	   ;; sucks.  So here we cruftily copy just enough goop
	   ;; out of disp-table.el so that a display table can be
	   ;; created, and thereby avoid loading disp-table.
	   (put 'display-table 'char-table-extra-slots 6)
	   (setq tab (make-char-table 'display-table nil))
	   (while (< i 256)
	     (aset tab i (vector i))
	     (setq i (1+ i)))
	   (setq buffer-display-table tab)))))

(defun vm-url-decode-string (string)
  (vm-with-string-as-temp-buffer string 'vm-url-decode-buffer))

(defun vm-url-decode-buffer ()
  (let ((case-fold-search t)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)
			   (?a . 10)  (?b . 11)  (?c . 12)  (?d . 13)
			   (?e . 14)  (?f . 15)))
	)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "%[0-9A-F][0-9A-F]" nil t)
	(insert-char (+ (* (cdr (assq (char-after (- (point) 2))
				      hex-digit-alist))
			   16)
			(cdr (assq (char-after (- (point) 1))
				   hex-digit-alist)))
		     1)
	(delete-region (- (point) 1) (- (point) 4))))))

(defun vm-process-kill-without-query (process &optional flag)
  (set-process-query-on-exit-flag process flag))

(defun vm-process-sentinel-kill-buffer (process _what-happened)
  (kill-buffer (process-buffer process)))

(defvar vm-disable-modes-ignore nil
  "List of modes ignored by `vm-disable-modes'.
Any mode causing an error while trying to disable it will be added to this
list.  It still will try to diable it, but no error messages are generated
anymore for it.")

(defun vm-disable-modes (&optional modes)
  "Disable the given minor modes.
If MODES is nil the take the modes from the variable 
`vm-disable-modes-before-encoding'."
  (let (m)
    (while modes
      (setq m (car modes) modes (cdr modes))
      (condition-case errmsg
          (if (functionp m)
              (funcall m -1))
	(error 
	 (when (not (member m vm-disable-modes-ignore))
	   (vm-warn 0 2 "Could not disable mode `%S': %S" m errmsg)
	   (setq vm-disable-modes-ignore (cons m vm-disable-modes-ignore)))
	 nil)))))

(defun vm-menu-can-eval-item-name ()
  "Whether a menu item's name may be a form to evaluate.
Only XEmacs allowed it, so this is always nil.  The callers keep their
other branch, which spells the name out."
  nil)

(defun vm-multiple-frames-possible-p ()
  "Whether VM may put a buffer in a frame of its own.
Never in a batch Emacs: `make-frame' is defined there and fails, with
\"Unknown terminal type\", so a composition made by a script died at the
point where VM went to give it a frame."
  (and (not noninteractive) (fboundp 'make-frame)))
 
(defun vm-mouse-support-possible-p ()
  (fboundp 'track-mouse))
 
(defun vm-mouse-support-possible-here-p ()
  (memq window-system '(x mac w32 win32)))

(defun vm-menu-support-possible-p ()
  (fboundp 'menu-bar-mode))
 
(defun vm-menubar-buttons-possible-p ()
  "Menubar buttons are menus that have an immediate action.  Some
Windowing toolkits do not allow such buttons.  This says whether such
buttons are possible under the current windowing system."
  (not (or (and (eq window-system 'x) (featurep 'gtk))
	   (eq window-system 'ns))))

(defun vm-toolbar-support-possible-p ()
  (and (fboundp 'tool-bar-mode) (boundp 'tool-bar-map)))

(defun vm-multiple-fonts-possible-p ()
  (memq window-system '(x mac w32 win32)))

(defun vm-images-possible-here-p ()
  (and window-system
       (or (fboundp 'image-type-available-p)
	   (vm-imagemagick-available-p))))

(defalias 'vm-image-type-available-p #'image-type-available-p)

(defun vm-load-features (feature-list &optional silent)
  "Try to load those features listed in FEATURE_LIST.
If SILENT is t, do not display warnings for unloadable features.
Return the list of loaded features.

Silent in a batch Emacs whatever SILENT says.  The warning is for a reader who
asked for a feature and is not getting it, and a batch Emacs has nobody to
read it: eighteen lines of it came out of `make', where four WARNINGs in the
middle of a build read as a broken build (emacs-vm/vm#485, emacs-vm/vm#753).
Building the manual loads every module to read its docstrings, which is where
they were coming from -- SILENT is `byte-compile-current-file' at every call
site, and that is nil when a file is loaded rather than compiled."
  (setq feature-list
        (mapcar (lambda (f)
                  (condition-case nil
                      (progn (require f)
                             f)
                    (error
                     (if (load (format "%s" f) t)
                         f
                       (unless (or silent noninteractive)
                         (message "WARNING: Could not load feature %S." f)
                         (message "WARNING: Related functions may not work correctly!"))
                       nil))))
                feature-list))
  (delete nil feature-list))


(defun vm-load-features-silent-when-compiling (feature-list)
  "Try to load those features listed in FEATURE_LIST and
don't display warnings if compiling"
  (vm-load-features feature-list (bound-and-true-p byte-compile-current-file)))

(defun vm-call-process (program infile buffer args)
  "Call PROGRAM with ARGS, separating stdout from stderr.
PROGRAM is the program to run.
INFILE is the input file (or nil for no input).
BUFFER is where stdout goes (t for current buffer, or a buffer/name).
ARGS is a list of program arguments.

Stderr is captured separately and reported via `message' if non-empty,
prefixed with the program name.

Returns the exit status (a number) as `call-process' does."
  (let ((stderr-file (make-temp-file "vm-stderr"))
	(exit-status nil))
    (unwind-protect
	(progn
	  (setq exit-status
		(apply #'call-process program infile
		       (list buffer stderr-file) nil args))
	  ;; Report any stderr output as a message
	  (when (and (file-exists-p stderr-file)
		     (> (file-attribute-size (file-attributes stderr-file)) 0))
	    (message "%s: %s"
		     (file-name-nondirectory program)
		     (string-trim
		      (with-temp-buffer
			(insert-file-contents stderr-file)
			(buffer-string))))))
      (when (file-exists-p stderr-file)
	(delete-file stderr-file)))
    exit-status))

;; Declare ImageMagick variables defined in vm-vars.el
(defvar vm-imagemagick-program)
(defvar vm-imagemagick-convert-program)
(defvar vm-imagemagick-identify-program)
(declare-function vm-imagemagick-program-is-v7-p "vm-vars" ())

(defun vm-imagemagick-available-p ()
  "Return non-nil if ImageMagick is available for image operations."
  (with-suppressed-warnings ((obsolete vm-imagemagick-convert-program))
    (stringp (or vm-imagemagick-program
		 vm-imagemagick-convert-program))))

(defun vm-imagemagick-convert-command ()
  "Return the ImageMagick convert program path.
Returns the program to use for convert operations.  For ImageMagick 7,
this returns `vm-imagemagick-program' (magick); callers should prepend
\"convert\" to the args.  For older versions, returns the convert program."
  (with-suppressed-warnings ((obsolete vm-imagemagick-convert-program))
    (or vm-imagemagick-convert-program
	vm-imagemagick-program)))

(defun vm-imagemagick-identify-command ()
  "Return the ImageMagick identify program path.
Returns the program to use for identify operations.  For ImageMagick 7,
this returns `vm-imagemagick-program' (magick); callers should prepend
\"identify\" to the args.  For older versions, returns the identify program."
  (with-suppressed-warnings ((obsolete vm-imagemagick-identify-program))
    (or vm-imagemagick-identify-program
	vm-imagemagick-program)))

(defun vm-imagemagick-program-is-magick-p (program)
  "Return non-nil if PROGRAM is ImageMagick 7's magick command."
  (and program
       (string-match-p "magick\\'" program)))

(defun vm-imagemagick-convert-shell-command ()
  "Return the shell command string for converting an image with ImageMagick.

For ImageMagick 7 that is `magick' on its own.  Version 7 deprecated the
`convert' command, and it says so on every run:

    WARNING: The convert command is deprecated in IMv7, use \"magick\"
    instead of \"convert\" or \"magick convert\"

so `magick convert' printed that warning for every image VM displayed.  For
version 6 there is no `magick', and the program is `convert' itself."
  (vm-imagemagick-convert-command))

(defun vm-imagemagick-call-convert (infile buffer args)
  "Convert an image with ImageMagick and ARGS, and answer with the exit status.
INFILE and BUFFER are passed to `vm-call-process'.

ARGS go to `magick' as they are: version 7 deprecated the `convert'
command and warns about it on every run, so VM does not ask for it.  Version 6
has no `magick' and the program is `convert' itself, which takes the same
arguments.  `identify' is a different matter -- version 7 has it as a
subcommand of `magick' and does not deprecate it -- so
`vm-imagemagick-call-identify' still names it."
  (let ((program (vm-imagemagick-convert-command)))
    (when program
      (vm-call-process program infile buffer args))))

(defun vm-imagemagick-call-identify (infile buffer args)
  "Call ImageMagick identify with ARGS, handling v6 vs v7 differences.
INFILE and BUFFER are passed to `vm-call-process'.
ARGS is a list of arguments for the identify command.
Returns the exit status."
  (let ((program (vm-imagemagick-identify-command)))
    (when program
      (vm-call-process program infile buffer
		       (if (vm-imagemagick-program-is-magick-p program)
			   (cons "identify" args)
			 args)))))

;;; auth-source access

;; VM asks auth-source for a password under two names: the account name
;; from vm-imap-account-alist / vm-pop-folder-alist, and the real host
;; name.  Users write either one in ~/.authinfo, so both are tried.

(defun vm-auth-source-password (hosts port user)
  "Return the auth-source password for USER at PORT on any of HOSTS.
HOSTS is a list of machine names to try in order; nil entries are
ignored.  Returns nil if `auth-sources' has no matching entry, and
also when USER is nil: `auth-source-search' treats a nil :user as no
constraint rather than as a wildcard to match, so it would hand back
whichever entry for that host comes first -- someone else's password."
  (catch 'done
    (unless user
      (throw 'done nil))
    (dolist (host hosts)
      (when host
	(let ((found (car (auth-source-search :host host :port port
					      :user user :max 1))))
	  (when found
	    (let ((secret (plist-get found :secret)))
	      ;; auth-source returns the secret as a lambda when the
	      ;; backend can defer decryption (e.g. authinfo.gpg)
	      (throw 'done (if (functionp secret)
			       (funcall secret)
			     secret)))))))
    nil))

(defun vm-percent-quote (string)
  "STRING as a `format' control string standing for itself.
The summary and MIME button compilers copy the text between the specifiers
into the control string they hand to `format', so a percent in that text has
to be doubled.  Without it a format of \"%s %q\" reached `format' with a %q
in it, and every line failed with \"Not enough arguments for format
string\" (emacs-vm/vm#847)."
  (replace-regexp-in-string "%" "%%" string t t))

(defun vm-percent-unquote (string)
  "STRING with each doubled percent back to a single one.
For a format holding no specifier at all: nothing calls `format' on it, so
the doubling has to be undone by hand or \"100%% done\" comes out as
\"100%% done\" where the docstrings promise \"100% done\"."
  (replace-regexp-in-string "%%" "%" string t t))

(provide 'vm-misc)
;;; vm-misc.el ends here
