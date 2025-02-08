;;; vm-version.el --- Version information about VM and the Emacs running VM.  -*- lexical-binding: t; -*-
;;
;; Copyright (C) Kyle E. Jones, Robert Widhopf-Fenk
;; Copyright (C) 2003-2007 Robert Widhopf-Fenk
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

(require 'vm-macro)

;; Don't use vm-device-type here because it may not not be loaded yet.
(declare-function device-type "vm-xemacs" ())
(declare-function device-matching-specifier-tag-list "vm-xemacs" ())

(defun vm-read-version-file (file-name line-number)
  "Read the a line from of FILE-NAME, remove all whitespace, and return it as a string.
Returns \"undefined\" if the file cannot be read."
  (let ((file-path (expand-file-name
                    file-name
                    (and load-file-name (file-name-directory load-file-name)))))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents-literally file-path)
          (goto-char (point-min))
          (forward-line (1- line-number))
          (replace-regexp-in-string "\\s-" "" ; Remove all whitespace
                                    (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position))))
      (file-error "undefined"))))

(defconst vm-version (vm-read-version-file "version.txt" 1)
  "Version number of VM.")

(defun vm-version ()
  "Return the value of the variable `vm-version'."
  (interactive)
  (when (vm-interactive-p)
    (or (and (stringp vm-version)
	     (string-match "[0-9]" vm-version))
	(error "Cannot determine VM version!"))
    (message "VM version is: %s" vm-version))
  vm-version)

(defconst vm-version-commit (vm-read-version-file "version.txt" 2)
  "git commit number of VM.")

(defun vm-version-commit ()
  "Return the value of the variable `vm-version-commit'."
  (interactive)
  (when (vm-interactive-p)
    (or (and (stringp vm-version-commit)
	     (string-match "[a-f0-9]+" vm-version-commit))
	(error "Cannot determine VM commit!"))
    (message "VM commit is: %s" vm-version-commit))
  vm-version-commit)

(defun vm-menu-can-eval-item-name ()
  (and (featurep 'xemacs)
       (fboundp 'check-menu-syntax)
       (condition-case nil
	   (check-menu-syntax '("bar" ((identity "foo") 'ding t)))
	 (error nil))))

(defun vm-multiple-frames-possible-p ()
  (cond ((featurep 'xemacs)
	 (or (memq 'win (device-matching-specifier-tag-list))
	     (featurep 'tty-frames)))
        ((not (featurep 'xemacs))
         (fboundp 'make-frame))))
 
(defun vm-mouse-support-possible-p ()
  (cond ((featurep 'xemacs)
         (featurep 'window-system))
        ((not (featurep 'xemacs))
         (fboundp 'track-mouse))))
 
(defun vm-mouse-support-possible-here-p ()
  (cond ((featurep 'xemacs)
	 (memq 'win (device-matching-specifier-tag-list)))
	((not (featurep 'xemacs))
	 (memq window-system '(x mac w32 win32)))))

(defun vm-menu-support-possible-p ()
  (cond ((featurep 'xemacs)
	 (featurep 'menubar))
	((not (featurep 'xemacs))
	 (fboundp 'menu-bar-mode))))
 
(defun vm-menubar-buttons-possible-p ()
  "Menubar buttons are menus that have an immediate action.  Some
Windowing toolkits do not allow such buttons.  This says whether such
buttons are possible under the current windowing system."
  (not
   (cond ((featurep 'xemacs) (memq (device-type) '(gtk ns)))
	 ((not (featurep 'xemacs)) (or (and (eq window-system 'x) (featurep 'gtk))
			    (eq window-system 'ns))))))

(defun vm-toolbar-support-possible-p ()
  (or (and (featurep 'xemacs) (featurep 'toolbar))
      (and (not (featurep 'xemacs)) (fboundp 'tool-bar-mode) (boundp 'tool-bar-map))))

(defun vm-multiple-fonts-possible-p ()
  (cond ((featurep 'xemacs)
	 (memq (device-type) '(x gtk mswindows)))
	((not (featurep 'xemacs))
	 (memq window-system '(x mac w32 win32)))))

(defun vm-images-possible-here-p ()
  (or (and (featurep 'xemacs) (memq (device-type) '(x gtk mswindows)))
      (and (not (featurep 'xemacs)) window-system
	   (or (fboundp 'image-type-available-p)
	       (and (stringp vm-imagemagick-convert-program)
		    (stringp vm-imagemagick-identify-program))))))

(defun vm-image-type-available-p (type)
  (if (fboundp 'image-type-available-p)
      (image-type-available-p type)
    (or (featurep type) (eq type 'xbm))))

(defun vm-load-features (feature-list &optional silent)
  "Try to load those features listed in FEATURE_LIST.
If SILENT is t, do not display warnings for unloadable features.
Return the list of loaded features."
  (setq feature-list
        (mapcar (lambda (f)
                  (condition-case nil
                      (progn (require f)
                             f)
                    (error
                     (if (load (format "%s" f) t)
                         f
                       (when (not silent)
                         (message "WARNING: Could not load feature %S." f)
                         ;; (sit-for 1)
                         (message "WARNING: Related functions may not work correctly!")
                         ;; (sit-for 1)
			 )
                       nil))))
                feature-list))
  (delete nil feature-list))

(provide 'vm-version)
;;; vm-version.el ends here
