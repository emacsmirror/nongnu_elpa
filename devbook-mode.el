;;; devbook-mode.el --- edit the Gentoo Devmanual  -*-lexical-binding:t-*-

;; Copyright 2020-2026 Gentoo Authors

;; Author: Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: text, languages

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimum Emacs version:
;; GNU Emacs 26.3
;; XEmacs not supported

;;; Code:

(require 'nxml-mode)
(require 'rng-loc)
(require 'font-lock)
(require 'easymenu)
(require 'skeleton)

(defgroup devbook nil
  "Major mode for editing the Gentoo Devmanual."
  :group 'text)

(defcustom devbook-fontify-codesamples-natively t
  "When non-nil, fontify codesamples using their native major mode."
  :type 'boolean
  :safe #'booleanp
  :group 'devbook)

(defvar devbook-codesample-re
  "<codesample\\>[^>]*\\<lang=[\"']\\([^\"']+\\)[\"'][^>]*>\
\\([^<]+\\)</codesample>"
  "Regexp matching a codesample element.
Subexpressions 1 and 2 capture the value of the \"lang\" attribute
and the element's text, respectively.")

(defvar devbook-codesample-lang-modes
  '(("c"      . c-mode)
    ("ebuild" . ebuild-mode)
    ("make"   . makefile-mode)
    ("m4"     . m4-mode)
    ;; "sgml" is pointless because <> must be escaped
    )
  "Alist of codesample languages and their major modes.")

(defvar devbook-font-lock-keywords
  `((,devbook-codesample-re
     2 (devbook-fontify-codesample)))
  "Expressions to highlight in `devbook-mode'.")

(defun devbook-fontify-codesample ()
  "Font-lock highlight function for codesamples."
  (let* ((beg (match-beginning 2))
	 (end (match-end 2))
	 (lang (match-string-no-properties 1))
	 (mode (and devbook-fontify-codesamples-natively
		    (cdr (assoc lang devbook-codesample-lang-modes))))
	 (buf (current-buffer)))
    ;; mark the whole codesample element as multiline construct
    (put-text-property (match-beginning 0) (match-end 0)
		       'font-lock-multiline t)
    (when mode
      ;; based on org-src-font-lock-fontify-block: copy the text
      ;; to a dedicated buffer and let its native mode fontify it
      (remove-text-properties beg end '(face nil))
      (with-current-buffer
	  (get-buffer-create
	   (format " *devbook-fontify-codesample-%s*" lang))
	(erase-buffer)
	(insert-buffer-substring-no-properties buf beg end)
	(unless (eq major-mode mode)
	  (let ((inhibit-message t))
	    (funcall mode)))
	(font-lock-ensure)
	(let* ((pos (point-min))
	       (offset (- beg pos))
	       next face)
	  (while (progn
		   (setq next (next-single-property-change pos 'face))
		   (if (setq face (get-text-property pos 'face))
		       (put-text-property (+ pos offset)
					  (+ (or next (point-max)) offset)
					  'face face buf))
		   (setq pos next)))))
      nil)))

(defun devbook-codesample-bounds (pos)
  "Find the bounds of any codesample element containing POS.
Return a cons cell (BEG . END) if BEG < POS < END, or nil otherwise."
  (save-excursion
    (goto-char pos)
    (while (and (not (looking-at-p "<[^/]"))
		(search-backward "<" nil t)))
    (and (looking-at devbook-codesample-re)
	 (< (match-beginning 0) pos (match-end 0))
	 (cons (match-beginning 0) (match-end 0)))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun devbook-font-lock-extend-region ()
  "Extend the font-lock region if it partially overlaps any codesample.
Move `font-lock-beg' to the start of any codesample strictly
containing it; likewise for `font-lock-end'.  Return non-nil
if either variable was changed."
  (let ((beg (devbook-codesample-bounds font-lock-beg))
	(end (devbook-codesample-bounds font-lock-end)))
    (if beg (setq font-lock-beg (car beg)))
    (if end (setq font-lock-end (cdr end)))
    (or beg end)))

(defvar devbook-schema-file-name "devbook.rnc")

(defun devbook-set-schema (&optional noerror)
  "Set the schema for this buffer.
Call `rng-locate-schema-file' first, which tries to locate a schema
based on the buffer's contents and file-name.  If unsuccessful,
look for a devbook schema file in any parent directory.  If no
schema file could be found either way, use the vacuous schema which
allows any well-formed XML.

Optional argument NOERROR suppresses signalling of any errors.
Return the schema file name, or nil if no schema was found."
  (interactive "P")
  (condition-case err
      (rng-set-schema-file-1
       (or (cl-letf* ((origfn (symbol-function 'rng-document-element))
		      ;; make sure that rng-document-element returns
		      ;; a document element even if the buffer is empty
		      ((symbol-function 'rng-document-element)
		       (lambda ()
			 (or (funcall origfn)
			     '(nil nil "devbook")))))
	     (rng-locate-schema-file))
	   (let ((dir (and buffer-file-name
			   (locate-dominating-file buffer-file-name
						   devbook-schema-file-name))))
	     (and dir (expand-file-name devbook-schema-file-name dir)))))
    (error (unless noerror (signal (car err) (cdr err)))))
  (unless noninteractive (rng-what-schema))
  rng-current-schema-file-name)

(defvar devbook-child-indent-alist
  ;; The style guide says no indentation, except inside <tr>,
  ;; <ul>, <ol> and <dl>, where it must be 2 spaces.
  '((nil . 0)
    ("tr" . 2) ("th" . 2) ("ti" . 2)
    ("ul" . 2) ("ol" . 2) ("li" . 2)
    ("dl" . 2) ("dt" . 2) ("dd" . 2)
    ("authors" . 2) ("author" . 2) ("authorlist" . 2))
  "Alist of XML elements and their indentation.
The nil key is a fallback for elements not explicitly listed.")

(defun devbook-indent-line ()
  "Like `nxml-indent-line' but indent only in tables and lists."
  (let* ((bwup (save-excursion
		 (beginning-of-line)
		 (and (ignore-errors (nxml-backward-up-element) t)
		      (looking-at "<\\([[:alnum:]_.:-]+\\)")
		      (match-string-no-properties 1))))
	 (nxml-child-indent
	  (cdr (or (assoc bwup devbook-child-indent-alist)
		   (assq nil devbook-child-indent-alist)))))
    (nxml-indent-line)))

(defun devbook-fill-tag-nobreak-p ()
  "Return non-nil if point is inside a tag right after its name.
This is used in `fill-nobreak-predicate' to prevent breaking a line
between the element name and its first attribute."
  (save-excursion
    (skip-chars-backward " \t")
    (when (/= (skip-chars-backward "[:alnum:]_.:-") 0)
      (skip-chars-backward "/?!")
      (eq (char-before) ?<))))

;;;###autoload
(define-derived-mode devbook-mode nxml-mode "DevBook"
  "Major mode for editing the Gentoo Devmanual."
  ;; The style guide says 80 characters. Set to 79 to keep diff output
  ;; within the limit (and arguably, 80 includes the newline).
  (setq fill-column 79)
  (setq indent-tabs-mode nil)
  ;; Tabs are allowed in ebuild codesamples, so this isn't redundant.
  (setq tab-width 4)
  (set (make-local-variable 'nxml-child-indent) 0)
  (set (make-local-variable 'indent-line-function) #'devbook-indent-line)
  (add-hook 'fill-nobreak-predicate #'devbook-fill-tag-nobreak-p nil t)
  (unless rng-current-schema-file-name
    (devbook-set-schema t))
  (font-lock-add-keywords nil devbook-font-lock-keywords)
  (add-hook 'font-lock-extend-region-functions
	    #'devbook-font-lock-extend-region))

(define-skeleton devbook-insert-skeleton
  "Insert a skeleton for a DevBook XML document."
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<devbook "
  (let ((path (if (and buffer-file-name
		       (string-match "/devmanual[^/]*/\\(.*/\\)"
				     buffer-file-name))
		  (match-string 1 buffer-file-name)
		(skeleton-read "Path: "))))
    (if (string-match-p "\\`/*\\'" path)
	"root=\"true\""
      (concat "self=\"" (file-name-as-directory path) "\"")))
  ">\n"
  "<chapter>\n"
  "<title>" (skeleton-read "Title: ") "</title>\n"
  - "\n"
  "</chapter>\n"
  "</devbook>\n")

(define-key devbook-mode-map "\C-c\C-e\C-n" #'devbook-insert-skeleton)

(easy-menu-define devbook-mode-menu devbook-mode-map
  "Menu for `devbook-mode'."
  '("DevBook"
    ["Insert skeleton" devbook-insert-skeleton]))

;;;###autoload
(add-to-list 'auto-mode-alist '("/devmanual.*\\.xml\\'" . devbook-mode))

(provide 'devbook-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; devbook-mode.el ends here
