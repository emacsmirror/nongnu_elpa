;;; vm-rfaddons.el --- a collections of various useful VM helper functions  -*- lexical-binding: t; -*-
;;
;; This file is an add-on for VM
;; 
;; Copyright (C) 1999-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2025 The VM Developers
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Integrated into View Mail (aka VM), 8.0.x
;; Keywords:    VM helpers
;; X-URL:       http://bazaar.launchpad.net/viewmail

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;; Some of the functions should be unbundled into separate packages,
;; but well I'm a lazy guy.  And some of them are not tested well. 
;;
;; In order to use this package add the following lines to the _end_ of your
;; .vm file.  It should be the _end_ in order to ensure that variable you had
;; been setting are honored!
;;
;;      (require 'vm-rfaddons)
;;      (vm-rfaddons-infect-vm)
;;
;; If you want to use only a subset of the functions you should have a
;; look at the documentation of `vm-rfaddons-infect-vm' and modify
;; its call as desired.  
;; 
;; Additional packages you may need are:
;;
;; * Package: Personality Crisis for VM
;;   is a really cool package if you want to do automatic header rewriting,
;;   e.g.  if you have various mail accounts and always want to use the right
;;   from header, then check it out! 
;;
;; * Package: BBDB
;;   Homepage: http://bbdb.sourceforge.net
;;
;; All other packages should be included within standard (X)Emacs
;; distributions.
;;
;; As I am no active GNU Emacs user, I would be thankful for any patches to
;; make things work with GNU Emacs!
;;
;;; Code:

(require 'vm-macro)
(require 'vm-misc)
(require 'vm-folder)
(require 'vm-summary)
(require 'vm-window)
(require 'vm-minibuf)
(require 'vm-menu)
(require 'vm-toolbar)
(require 'vm-mouse)
(require 'vm-motion)
(require 'vm-undo)
(require 'vm-delete)
(require 'vm-crypto)
(require 'vm-message)
(require 'vm-mime)
(require 'vm-edit)
(require 'vm-virtual)
(require 'vm-pop)
(require 'vm-imap)
(require 'vm-sort)
(require 'vm-reply)
(require 'vm-postpone)
(require 'vm)
(eval-when-compile (require 'cl-lib))

(declare-function bbdb-record-xfields "ext:bbdb" (record))
(declare-function bbdb-record-mail "ext:bbdb" (record))
(declare-function bbdb-split "ext:bbdb" (separator string))
(declare-function bbdb-records "ext:bbdb" ())
(declare-function bbdb-save "ext:bbdb" (&optional prompt noisy))

(declare-function smtpmail-via-smtp-server "ext:smtpmail" ())
(declare-function esmtpmail-send-it "ext:esmtpmail" ())
(declare-function esmtpmail-via-smtp-server "ext:esmtpmail" ())
(declare-function vm-folder-buffers "ext:vm" (&optional non-virtual))

(eval-when-compile (vm-load-features-silent-when-compiling '(regexp-opt bbdb bbdb-vm)))

(require 'sendmail)
(vm-load-features '(bbdb))

(if (featurep 'xemacs) (require 'overlay))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vm-rfaddons nil
  "Customize vm-rfaddons.el"
  :group 'vm-ext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sometimes it's handy to fake a date.
;; I overwrite the standard function by a slightly different version.
(defmacro vm-rfaddons-check-option (option option-list &rest body)
  "Evaluate body if option is in OPTION-LIST or OPTION-LIST is
nil."
  (list 'if (list 'member option option-list)
        (cons 'progn
              (cons (list 'setq option-list (list 'delq option option-list))
                    (cons (list 'message "Adding vm-rfaddons-option `%s'."
                                option)
                          body)))))

(defun vm-rfaddons--do-preview-again (&rest _)
  (if vm-mime-delete-after-saving
      (vm-present-current-message)))

(defun vm-rfaddons-infect-vm (&optional _sit-for
                                        option-list exclude-option-list)
  "This function will setup the key bindings, advices and hooks
necessary to use all the function of vm-rfaddons.el.

SIT-FOR specifies the number of seconds to display the infection message.
The OPTION-LIST can be use to select individual option.
The EXCLUDE-OPTION-LIST can be use to exclude individual option.

The following options are possible.

`vm-mail-mode' options:
 - attach-save-files: bind [C-c C-a] to `vm-attach-files-in-directory' 

Other EXPERIMENTAL options:
 - auto-save-all-attachments: add `vm-mime-auto-save-all-attachments' to
   `vm-select-new-message-hook' for automatic saving of attachments.

If you want to use only a subset of the options then call
`vm-rfaddons-infect-vm' like this:
        (vm-rfaddons-infect-vm 2 \\='(vm-mail-mode shrunken-headers)
                                 \\='(fake-date))
This will enable all `vm-mail-mode' options plus the
`shrunken-headers' option, but it will exclude the `fake-date' option of the
`vm-mail-mode' options.

or do the binding and advising on your own."
  (interactive "")

  (if (eq option-list 'all)
      (setq option-list (list 'vm-mail-mode 'auto-save-all-attachments))
    (if (eq option-list t)
        (setq option-list (list 'vm-mail-mode))))
  
  
  (when (member 'vm-mail-mode option-list)
    (setq option-list (append '(attach-save-files)
                              option-list))
    (setq option-list (delq 'vm-mail-mode option-list)))
  
    
  (while exclude-option-list
    (if (member (car exclude-option-list) option-list)
        (setq option-list (delq (car exclude-option-list) option-list))
      (message "VM-RFADDONS: The option `%s' was not excluded, maybe it is unknown!"
               (car exclude-option-list))
      (ding)
      (sit-for 3))
    (setq exclude-option-list (cdr exclude-option-list)))
  



   (vm-rfaddons-check-option
    'return-receipt-to option-list
    (add-hook 'vm-select-message-hook 'vm-handle-return-receipt))

   (when option-list
    (message "VM-RFADDONS: The following options are unknown: %s" option-list)
    (ding)
    (sit-for 3)))


;;;###autoload


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This add-on is disabled becaust it has been integrated into the
;; core.  USR, 2010-05-01


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This has been moved to the VM core.  USR, 2010-03-11
;;;;;###autoload

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bbdb/vm-set-virtual-folder-alist ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record that has a `vm-virtual' attribute, add or modify the
corresponding BBDB-VM-VIRTUAL element of the `vm-virtual-folder-alist'.

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the `element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field  email-regexp folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-xfields record))
      (when (and (listp notes-field)
                 (setq folder (cdr (assq 'vm-virtual notes-field))))
        (setq email-regexp (mapconcat (lambda (addr)
					(regexp-quote addr))
                                      (bbdb-record-mail record) "\\|"))
        (unless (zerop (length email-regexp))
          (setq folder (or (assoc folder vm-virtual-folder-alist)
                           (car
                            (setq vm-virtual-folder-alist
                                  (nconc (list (list folder
                                                     (list (list vm-primary-inbox)
                                                           (list 'author-or-recipient))))
                                               vm-virtual-folder-alist))))
                folder (cadr folder)
                selector (assoc 'author-or-recipient folder))

          (if (cdr selector)
              (if (not (string-match (regexp-quote email-regexp)
                                     (cadr selector)))
                  (setcdr selector (list (concat (cadr selector) "\\|"
                                                 email-regexp))))
            (nconc selector (list email-regexp)))))
      )
    ))

(defun vm-virtual-find-selector (selector-spec type)
  "Return the first selector of TYPE in SELECTOR-SPEC."
  (let ((s (assoc type selector-spec)))
    (unless s
      (while (and (not s) selector-spec)
        (setq s (and (listp (car selector-spec))
                     (vm-virtual-find-selector (car selector-spec) type))
              selector-spec (cdr selector-spec))))
    s))

(defcustom bbdb/vm-virtual-folder-alist-by-mail-alias-alist nil
  "*A list of (ALIAS . FOLDER-NAME) pairs, which map an alias to a folder."
  :group 'vm-rfaddons
  :type '(repeat (cons :tag "Mapping Definition"
                       (regexp :tag "Alias")
                       (string :tag "Folder Name"))))

(defun bbdb/vm-set-virtual-folder-alist-by-mail-alias ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record check wheather its alias is in the variable 
`bbdb/vm-virtual-folder-alist-by-mail-alias-alist' and then
add/modify the corresponding VM-VIRTUAL element of the
`vm-virtual-folder-alist'. 

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the `element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field email-regexp mail-aliases folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-xfields record))
      (when (and (listp notes-field)
                 (setq mail-aliases (cdr (assq 'mail-alias notes-field)))
                 (setq mail-aliases (bbdb-split "," mail-aliases)))
        (setq folder nil)
        (while mail-aliases
          (setq folder
                (assoc (car mail-aliases)
                       bbdb/vm-virtual-folder-alist-by-mail-alias-alist))
          
          (when (and folder
                     (setq folder (cdr folder)
                           email-regexp (mapconcat (lambda (addr)
						     (regexp-quote addr))
                                                   (bbdb-record-mail record)
                                                   "\\|"))
                     (> (length email-regexp) 0))
            (setq folder (or (assoc folder vm-virtual-folder-alist)
                             (car
                              (setq vm-virtual-folder-alist
                                    (nconc
                                     (list
                                      (list folder
                                            (list (list vm-primary-inbox)
                                                  (list 'author-or-recipient))
                                            ))
                                     vm-virtual-folder-alist))))
                  folder (cadr folder)
                  selector (vm-virtual-find-selector folder
                                                     'author-or-recipient))
            (unless selector
              (nconc (cdr folder) (list (list 'author-or-recipient))))
            (if (cdr selector)
                (if (not (string-match (regexp-quote email-regexp)
                                       (cadr selector)))
                    (setcdr selector (list (concat (cadr selector) "\\|"
                                                   email-regexp))))
              (nconc selector (list email-regexp))))
          (setq mail-aliases (cdr mail-aliases)))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-handle-return-receipt-mode 'edit
  "Tells `vm-handle-return-receipt' how to handle return receipts.
One can choose between `ask', `auto', `edit', or an expression which should
return t if the return receipts should be sent."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Edit" edit)
                 (const :tag "Ask" ask)
                 (const :tag "Auto" auto)))

(defcustom vm-handle-return-receipt-peek 500
  "*Number of characters from the original message body to be returned."
  :group 'vm-rfaddons
  :type '(integer))

(defun vm-handle-return-receipt ()
  "Generate a reply to the current message if it requests a return receipt
and has not been replied so far.
See the variable `vm-handle-return-receipt-mode' for customization."
  (interactive)
  (save-excursion
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (let* ((msg (car vm-message-pointer))
           (sender (vm-get-header-contents msg  "Return-Receipt-To:"))
           (mail-signature nil)
           (mode (and sender
                      (cond ((equal 'ask vm-handle-return-receipt-mode)
                             (y-or-n-p "Send a return receipt? "))
                            ((symbolp vm-handle-return-receipt-mode)
                             vm-handle-return-receipt-mode)
                            (t
                             (eval vm-handle-return-receipt-mode)))))
           (vm-mutable-frame-configuration 
	    (if (eq mode 'edit) vm-mutable-frame-configuration nil))
           (vm-mail-mode-hook nil)
           (vm-mode-hook nil)
           message)
      (when (and mode (not (vm-replied-flag msg)))
        (vm-reply 1)
        (vm-mail-mode-remove-header "Return-Receipt-To:")
        (vm-mail-mode-remove-header "To:")
        (goto-char (point-min))
        (insert "To: " sender "\n")
        (mail-text)
        (delete-region (point) (point-max))
        (insert 
         (format 
          "Your mail has been received on %s."
          (current-time-string)))
        (save-restriction
          (with-current-buffer (vm-buffer-of msg)
            (widen)
            (setq message
                  (buffer-substring
                   (vm-vheaders-of msg)
                   (let ((tp (+ vm-handle-return-receipt-peek
                                (marker-position
                                 (vm-text-of msg))))
                         (ep (marker-position
                              (vm-end-of msg))))
                     (if (< tp ep) tp ep))
                   ))))
        (insert "\n-----------------------------------------------------------------------------\n"
                message)
        (if (re-search-backward "^\\s-+.*" (point-min) t)
            (replace-match ""))
        (insert "[...]\n")
        (if (not (eq mode 'edit))
            (vm-mail-send-and-exit nil))
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'vm-rfaddons)
;;; vm-rfaddons.el ends here
