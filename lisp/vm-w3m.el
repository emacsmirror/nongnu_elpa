;;; vm-w3m.el --- additional functions to make VM use emacs-w3m for HTML mails  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 2003, 2005, 2006 Katsumi Yamaoka,
;; Copyright (C) 2007 Robert Widhopf-Fenk
;; Copyright (C) 2024-2025 The VM Developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; You need to have w3m and emacs-w3m installed for this module to
;; work.  Visit <URL:http://emacs-w3m.namazu.org/> for details.
;; You don't have to change VM at all.  Simply load this module and
;; you will see HTML mails inlined by emacs-w3m in the VM presentation
;; buffer.

;;; Code:

(require 'vm-mime)
(require 'vm-misc)

(eval-and-compile (vm-load-features '(w3m) byte-compile-current-file))

(declare-function w3m-region 
		  "ext:w3m" (start end &optional url charset))
(declare-function w3m-safe-toggle-inline-images 
		  "ext:w3m" (&optional force no-cache))


;; Dummy variable declarations to suppress warnings if w3m is not
;; loaded

(defvar w3m-current-buffer)
(defvar w3m-cid-retrieve-function-alist)
(defvar w3m-minor-mode-map)
(defvar url-working-buffer)
(defvar url-current-mime-type)
(defvar url-current-mime-headers)

(defvar vm-w3m-mode-map nil
  "Keymap for w3m within VM.")

(defgroup vm-w3m nil
  "w3m settings for VM."
  :group  'vm-presentation)

(defcustom vm-w3m-display-inline-images t
  "Non-nil means VM will allow retrieving images in the HTML contents
with the <img> tags.  See also the documentation for the variable
`vm-w3m-safe-url-regexp'."
  :group 'vm-w3m
  :type 'boolean)

(defcustom vm-w3m-safe-url-regexp "\\`cid:"
  "Regexp matching URLs which are considered to be safe.
Some HTML mails might contain a nasty trick used by spammers, using
the <img> tag which is far more evil than the [Click Here!] button.
It is most likely intended to check whether the ominous spam mail has
reached your eyes or not, in which case the spammer knows for sure
that your email address is valid.  It is done by embedding an
identifier string into a URL that you might automatically retrieve
when displaying the image.  The default value is \"\\\\`cid:\" which only
matches parts embedded to the Multipart/Related type MIME contents and
VM will never connect to the spammer's site arbitrarily.  You may set
this variable to nil if you consider all urls to be safe."
  :group 'vm-w3m
  :type '(choice (regexp :tag "Regexp")
		 (const :tag "All URLs are safe" nil)))

;; (defcustom vm-w3m-use-w3m-minor-mode-map nil
;;   "Say whether to use emacs-w3m command keys in VM presentation buffers.
;; Set this variable to nil if you don't want vm-w3m to override any VM
;; commend keys.  If it is non-nil, you will not be able to use some VM
;; command keys, which are bound to emacs-w3m commands defined in the
;; `w3m-minor-mode-command-alist' variable."
;;   :group 'vm-w3m
;;   :type 'boolean)

(defvaralias 'vm-w3m-use-w3m-minor-mode-map 
  'vm-use-presentation-minor-modes)
(make-obsolete-variable 'vm-w3m-use-w3m-minor-mode-map
			'vm-use-presentation-minor-modes "8.2.0")

(defvar vm-w3m-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "[" 'w3m-previous-anchor)
    (define-key map "]" 'w3m-next-anchor)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "}" 'w3m-next-image)
    (define-key map [return] 'w3m-view-this-url)
    (define-key map [C-return] 'w3m-view-url-with-external-browser)
    (define-key map "\C-xw" 'w3m-print-this-url)
    (define-key map "\C-xi" 'w3m-toggle-inline-image)
    (define-key map "\C-x\C-I" 'w3m-toggle-inline-images)
    (define-key map [down-mouse-1] 'w3m-mouse-view-this-url)
    (cond ((fboundp 'set-keymap-name)
	   (set-keymap-name map 'vm-w3m-map)))
    map )
  "Keymap for text/html parts inlined by emacs-w3m.")

(eval-and-compile
  (or (featurep 'xemacs) (>= emacs-major-version 21)
      (defvar vm-w3m-mode-map nil
	"Internal variable holding the keymap for text/html parts
inlined by emacs-w3m. 
This keymap will be bound only when Emacs 20 is running and overwritten
by the minor-mode-keymap for emacs-w3m text, as determined by
`vm-presentation-minor-modes'.")))

(defvar w3m-display-inline-images)
(defvar w3m-safe-url-regexp)

(defun vm-w3m-cid-retrieve (url &rest _args)
  "Insert a content of URL."
  (let ((message (with-current-buffer w3m-current-buffer                   (car vm-message-pointer)))
        part
        type)
    (setq part (vm-mime-cid-retrieve url message))
    (when part
      (setq type (car (vm-mm-layout-type part)))
      (vm-mime-transfer-decode-region part (point-min) (point-max)))
    type))

(or (assq 'vm-presentation-mode w3m-cid-retrieve-function-alist)
    (setq w3m-cid-retrieve-function-alist
	  (cons '(vm-presentation-mode . vm-w3m-cid-retrieve)
		w3m-cid-retrieve-function-alist)))

(defun vm-w3m-local-map-property ()
  (let* ((minor-mode (and vm-use-presentation-minor-modes
			 (cadr (assoc 'emacs-w3m vm-presentation-minor-modes))))
	 (keymap-name (and minor-mode 
			   (intern (format "%s-map"  minor-mode))))
	 (keymap (and keymap-name (boundp keymap-name)
		      (symbol-value keymap-name))))
    (when keymap
      (if (or (featurep 'xemacs) (>= emacs-major-version 21))
	  (list 'keymap keymap)
	(list 'local-map
	      (or vm-w3m-mode-map
		  (progn
		    (setq vm-w3m-mode-map
			  (copy-keymap keymap))
		    (set-keymap-parent vm-w3m-mode-map vm-mode-map)
		    vm-w3m-mode-map)))))))

;;;###autoload
(defun vm-mime-display-internal-emacs-w3m-text/html (start end _layout)
  "Use emacs-w3m to inline HTML mails in the VM presentation buffer."
  (let ((w3m-display-inline-images vm-w3m-display-inline-images)
        (w3m-safe-url-regexp vm-w3m-safe-url-regexp))
    (w3m-region start (1- end))
    (add-text-properties
     start end
     (nconc (if vm-use-presentation-minor-modes
		(if (equal major-mode 'vm-presentation-mode)
		    (vm-w3m-local-map-property)))
            ;; Put the mark meaning that this part was
            ;; inlined by emacs-w3m.
            '(text-rendered-by-emacs-w3m t)))))

(defun vm-w3m-safe-toggle-inline-images (&optional arg)
  "Toggle displaying of all images in the presentation buffer.
If the prefix arg is given, all images are considered to be safe."
  (interactive "P")
  (let ((buffer (cond ((eq major-mode 'vm-summary-mode)
		       (with-current-buffer vm-mail-buffer
			 vm-presentation-buffer))
		      ((eq major-mode 'vm-presentation-mode)
		       (current-buffer))
		      ((eq major-mode 'vm-mode)
		       vm-presentation-buffer))))
    (if (buffer-live-p buffer)
	(with-current-buffer buffer
	  (w3m-safe-toggle-inline-images arg)))))

(provide 'vm-w3m)
;;; vm-w3m.el ends here
