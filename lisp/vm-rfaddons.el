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
(require 'wid-edit)
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
(defcustom vm-mail-mode-fake-date-p t
  "Non-nil means `vm-mail-mode-insert-date-maybe' keeps an existing date header.
Otherwise, overwrite existing date headers"
  :group 'vm-rfaddons
  :type '(boolean))

(defmacro vm-rfaddons-check-option (option option-list &rest body)
  "Evaluate body if option is in OPTION-LIST or OPTION-LIST is
nil."
  (list 'if (list 'member option option-list)
        (cons 'progn
              (cons (list 'setq option-list (list 'delq option option-list))
                    (cons (list 'message "Adding vm-rfaddons-option `%s'."
                                option)
                          body)))))

(defun vm-rfaddons--fake-date (orig-fun &rest args)
  "Do not change an existing date if `vm-mail-mode-fake-date-p' is t."
  (if (not (and vm-mail-mode-fake-date-p
                (vm-mail-mode-get-header-contents "Date:")))
      (apply orig-fun args)))

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
 - check-recipients: add `vm-mail-check-recipients' to `mail-send-hook' in
   order to check if the recipients headers are correct.
 - encode-headers: add `vm-mime-encode-headers' to `mail-send-hook' in
   order to encode the headers before sending.
 - fake-date: if enabled allows you to fake the date of an outgoing message.

`vm-mode' options:
 - shrunken-headers: enable shrunken-headers by advising several functions 

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
      (setq option-list (list 'vm-mail-mode 'vm-mode
                              'auto-save-all-attachments))
    (if (eq option-list t)
        (setq option-list (list 'vm-mail-mode 'vm-mode))))
  
  
  (when (member 'vm-mail-mode option-list)
    (setq option-list (append '(attach-save-files
                                check-recipients
                                check-for-empty-subject
                                encode-headers
                                clean-subject
                                fake-date
                                open-line)
                              option-list))
    (setq option-list (delq 'vm-mail-mode option-list)))
  
  (when (member 'vm-mode option-list)
    (setq option-list (append '(shrunken-headers)
                              option-list))
    (setq option-list (delq 'vm-mode option-list)))
    
  (while exclude-option-list
    (if (member (car exclude-option-list) option-list)
        (setq option-list (delq (car exclude-option-list) option-list))
      (message "VM-RFADDONS: The option `%s' was not excluded, maybe it is unknown!"
               (car exclude-option-list))
      (ding)
      (sit-for 3))
    (setq exclude-option-list (cdr exclude-option-list)))
  

  ;; vm-mail-mode -----------------------------------------------------------
  (vm-rfaddons-check-option
   'attach-save-files option-list
   ;; this binding overrides the VM binding of C-c C-a to `vm-attach-file'
   (define-key vm-mail-mode-map "\C-c\C-a" 'vm-attach-files-in-directory))
  
  ;; check recipients headers for errors before sending
  (vm-rfaddons-check-option
   'check-recipients option-list
   (add-hook 'mail-send-hook 'vm-mail-check-recipients))

  ;; check if the subjectline is empty
  (vm-rfaddons-check-option
   'check-for-empty-subject option-list
   (add-hook 'vm-mail-send-hook 'vm-mail-check-for-empty-subject))
  
  ;; encode headers before sending
  (vm-rfaddons-check-option
   'encode-headers option-list
   (add-hook 'mail-send-hook 'vm-mime-encode-headers))

  ;; This allows us to fake a date by advising vm-mail-mode-insert-date-maybe
  (vm-rfaddons-check-option
   'fake-date option-list
   (advice-add 'vm-mail-mode-insert-date-maybe
               :around #'vm-rfaddons--fake-date))
  
  (vm-rfaddons-check-option
   'open-line option-list
   (add-hook 'vm-mail-mode-hook 'vm-mail-mode-install-open-line))

  (vm-rfaddons-check-option
   'clean-subject option-list
   (add-hook 'vm-mail-mode-hook 'vm-mail-subject-cleanup))

  ;; vm-mode -----------------------------------------------------------

  ;; Shrunken header handlers
  (vm-rfaddons-check-option
   'shrunken-headers option-list
   (if (not (boundp 'vm-always-use-presentation))
       (message "Shrunken-headers do NOT work in standard VM!")
     ;; We would corrupt the folder buffer for messages which are
     ;; not displayed by a presentation buffer, thus we must ensure
     ;; that a presentation buffer is used.  The visibility-widget
     ;; would cause "*"s to be inserted into the folder buffer.
     (setq vm-always-use-presentation t)
     (advice-add 'vm-present-current-message :after #'vm-shrunken-headers)
     (advice-add 'vm-expose-hidden-headers :after #'vm-shrunken-headers)
     ;; this overrides the VM binding of "T" to `vm-toggle-thread'
     (define-key vm-mode-map "T" 'vm-shrunken-headers-toggle)))


;; This is not needed any more becaue it is in the core  

  ;; other experimental options ---------------------------------------------
  ;; Now take care of automatic saving of attachments
  (vm-rfaddons-check-option
   'auto-save-all-attachments option-list
   ;; In order to reflect MIME type changes when `vm-mime-delete-after-saving'
   ;; is t we preview the message again.
   (advice-add 'vm-mime-send-body-to-file
               :after #'vm-rfaddons--do-preview-again)
   (add-hook 'vm-select-new-message-hook 'vm-mime-auto-save-all-attachments))
   

   (vm-rfaddons-check-option
    'return-receipt-to option-list
    (add-hook 'vm-select-message-hook 'vm-handle-return-receipt))

   (when option-list
    (message "VM-RFADDONS: The following options are unknown: %s" option-list)
    (ding)
    (sit-for 3)))

(defcustom vm-reply-include-presentation nil
  "*If true a reply will include the presentation of a message.
This might give better results when using filling or MIME encoded messages,
e.g. HTML message.
 (This variable is part of vm-rfaddons.el.)"
  :group 'vm-rfaddons
  :type 'boolean)

;;;###autoload
(defun vm-followup-include-presentation (count)
  "Include presentation instead of text.
This does not work when replying to multiple messages."
  (interactive "p")
  (vm-reply-include-presentation count t))
(make-obsolete 'vm-followup-include-presentation
	       'vm-include-text-from-presentation "8.2.0")

;;;###autoload
(defun vm-reply-include-presentation (count &optional to-all)
  "Include presentation instead of text.
This does only work with my modified VM, i.e. a hacked
`vm-yank-message'."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
  (if (null vm-presentation-buffer)
      (if to-all
          (vm-followup-include-text count)
        (vm-reply-include-text count))
    (let ((vm-include-text-from-presentation t)
	  (vm-reply-include-presentation t)  ; is this variable necessary?
	  (vm-enable-thread-operations nil)) 
      (vm-do-reply to-all t count))))
(make-obsolete 'vm-reply-include-presentation
	       'vm-include-text-from-presentation "8.2.0")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This add-on is disabled becaust it has been integrated into the
;; core.  USR, 2010-05-01


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This has been moved to the VM core.  USR, 2010-03-11
;;;;;###autoload

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-mail-subject-prefix-replacements
  '(("\\(\\(re\\|aw\\|antw\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Re: ")
    ("\\(\\(fo\\|wg\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Fo: "))
  "*List of subject prefixes which should be replaced.
Matching will be done case insensitively."
  :group 'vm-rfaddons
  :type '(repeat (cons (regexp :tag "Regexp")
                       (string :tag "Replacement"))))

(defcustom vm-mail-subject-number-reply nil
  "*Non-nil means, add a number [N] after the reply prefix.
The number reflects the number of references."
  :group 'vm-rfaddons
  :type '(choice
          (const :tag "on" t)
          (const :tag "off" nil)))

(defun vm-mail-subject-cleanup ()
  "Do some subject line clean up.
- Replace subject prefixes according to `vm-mail-subject-prefix-replacements'.
- Add a number after replies is `vm-mail-subject-number-reply' is t.

You might add this function to `vm-mail-mode-hook' in order to clean up the
Subject header."
  (interactive)
  (save-excursion
    ;; cleanup
    (goto-char (point-min))
    (re-search-forward 
     (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
     (point-max))
    (let ((case-fold-search t)
          (rpl vm-mail-subject-prefix-replacements))
      (while rpl
        (if (re-search-backward (concat "^Subject:[ \t]*" (caar rpl))
                                (point-min) t)
            (replace-match (concat "Subject: " (cdar rpl))))
        (setq rpl (cdr rpl))))

    ;; add number to replys
    (let (refs (start 0) end (count 0))
      (when (and vm-mail-subject-number-reply vm-reply-list
                 (setq refs  (vm-mail-mode-get-header-contents "References:")))
        (while (string-match "<[^<>]+>" refs start)
          (setq count (1+ count)
                start (match-end 0)))
        (when (> count 1)
          (mail-position-on-field "Subject" t)
          (setq end (point))
          (if (re-search-backward "^Subject:" (point-min) t)
              (setq start (point))
            (error "vm-mail-check-subject-cleanup: Could not find end of Subject header start"))
          (goto-char start)
          (if (not (re-search-forward (regexp-quote vm-reply-subject-prefix)
                                      end t))
              (error "vm-mail-check-subject-cleanup: Cound not find vm-reply-subject-prefix `%s' in header"
                     vm-reply-subject-prefix)
            (goto-char (match-end 0))
            (skip-chars-backward ": \t")
            (insert (format "[%d]" count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-mime-set-8bit-composition-charset (charset &optional buffer-local)
  "*Set `vm-mime-8bit-composition-charset' to CHARSET.
With the optional BUFFER-LOCAL prefix arg, this only affects the current
buffer."
  (interactive (list (completing-read 
		      ;; prompt
		      "Composition charset: "
		      ;; collection
		      vm-mime-charset-completion-alist
		      ;; predicate, require-match
		      nil t)
		     current-prefix-arg))
  (if (or (featurep 'xemacs) (not (featurep 'xemacs)))
      (error "vm-mime-8bit-composition-charset has no effect in XEmacs/MULE"))
  (if buffer-local
      (set (make-local-variable 'vm-mime-8bit-composition-charset) charset)
    (setq vm-mime-8bit-composition-charset charset)))

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

(defalias 'vm-mime-find-type-of-message/external-body
  'vm-mf-external-body-content-type)
(make-obsolete 'vm-mime-find-type-of-message/external-body
	       'vm-mf-external-body-content-type "8.2.0")

;; This is a hack in order to get the right MIME button 
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvaralias 'vm-mime-attach-files-in-directory-regexps-history
  'vm-attach-files-in-directory-regexps-history)
(defvar vm-attach-files-in-directory-regexps-history nil
  "Regexp history for matching files.")

(defvaralias 'vm-mime-attach-files-in-directory-default-type
  'vm-attach-files-in-directory-default-type)
(defcustom vm-attach-files-in-directory-default-type nil
  "*The default MIME-type for attached files.
If set to nil you will be asked for the type if it cannot be guessed.
For guessing mime-types we use `vm-mime-attachment-auto-type-alist'."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Ask" nil)
                 (string "application/octet-stream")))

(defvaralias 'vm-mime-attach-files-in-directory-default-charset
  'vm-attach-files-in-directory-default-charset)
(defcustom vm-attach-files-in-directory-default-charset 'guess
  "*The default charset used for attached files of type `text'.
If set to nil you will be asked for the charset.
If set to `guess' it will be determined by `vm-determine-proper-charset', but
this may take some time, since the file needs to be visited."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Ask" nil)
                 (const :tag "Guess" guess)))

(defvaralias 'vm-mime-save-all-attachments-types
  'vm-mime-saveable-types)
(make-obsolete-variable 'vm-mime-save-all-attachments-types
			'vm-mime-saveable-types "8.1.1")

(defvaralias 'vm-mime-save-all-attachments-types-exceptions
  'vm-mime-saveable-type-exceptions)
(make-obsolete-variable 'vm-mime-save-all-attachments-types-exceptions
			'vm-mime-saveable-type-exceptions "8.1.1")

(defvaralias 'vm-mime-delete-all-attachments-types
  'vm-mime-deletable-types)
(make-obsolete-variable 'vm-mime-delete-all-attachments-types
			'vm-mime-deletable-types "8.1.1")

(defvaralias 'vm-mime-delete-all-attachments-types-exceptions
  'vm-mime-deletable-type-exceptions)
(make-obsolete-variable 'vm-mime-delete-all-attachments-types-exceptions
			'vm-mime-deletable-type-exceptions "8.1.1")

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
(defalias 'vm-mime-attach-files-in-directory 'vm-attach-files-in-directory)

(defcustom vm-mime-auto-save-all-attachments-subdir
  nil
  "*Subdirectory where to save the attachments of a message.
This variable might be set to a string, a function or anything which evaluates
to a string.  If set to nil we use a concatenation of the from, subject and
date header as subdir for the attachments."
  :group 'vm-rfaddons
  :type '(choice (directory :tag "Directory")
                 (string :tag "No Subdir" "")
                 (function :tag "Function")
                 (sexp :tag "sexp")))

(defun vm-mime-auto-save-all-attachments-subdir (msg)
  "Return a subdir for the attachments of MSG.
This will be done according to `vm-mime-auto-save-all-attachments-subdir'."
  (setq msg (vm-real-message-of msg))
  (when (not (string-match 
	      (regexp-quote (vm-reencode-mime-encoded-words-in-string
			     (vm-su-full-name msg)))
	      (vm-get-header-contents msg "From:")))
    (backtrace)
    (if (y-or-n-p (format "Is this wrong? %s <> %s "
                         (vm-su-full-name msg)
                         (vm-get-header-contents msg "From:")))
        (error "Yes it is wrong!")))
    
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
          (if (string-match "/$" vm-mime-attachment-save-directory)
              (concat vm-mime-attachment-save-directory subdir)
            (concat vm-mime-attachment-save-directory "/" subdir))
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
(defun vm-mail-check-recipients-strip (address)
  "Remove from ADDRESS the parts that may legitimately contain an \"@\".
That is MIME encoded words, quoted strings and RFC 5322 comments, all of
which occur alongside the address proper.  What is left should hold
exactly one address.

`vm-parse-addresses' decodes encoded words, marking what it decoded with
the `vm-string' text property, so those are removed by property; a word
still in its encoded form is removed by matching."
  (let ((start 0)
	(len (length address))
	(pieces nil)
	(stripped nil)
	(previous nil))
    ;; drop the decoded encoded words
    (while (< start len)
      (let ((end (or (next-single-property-change start 'vm-string address)
		     len)))
	(unless (get-text-property start 'vm-string address)
	  (push (substring-no-properties address start end) pieces))
	(setq start end)))
    (setq stripped
	  (vm-replace-in-string
	   (vm-replace-in-string (apply #'concat (nreverse pieces))
				 vm-mime-encoded-word-regexp "")
	   "\"[^\"]*\"" ""))
    ;; and the comments, innermost first so nested ones go too
    (while (not (equal previous stripped))
      (setq previous stripped)
      (setq stripped (vm-replace-in-string stripped "([^()]*)" "")))
    stripped))

(defun vm-mail-check-recipients ()
  "Check if the recipients are specified correctly.
Actually it checks only if there are any missing commas or the like in the
headers."
  (interactive)
  (let ((header-list '("To:" "CC:" "BCC:"
                       "Resent-To:" "Resent-CC:" "Resent-BCC:"))
        (contents nil)
        (errors nil))
    (while header-list
      (setq contents (vm-mail-mode-get-header-contents (car header-list)))
      ;; Split into addresses first, respecting quoting and comments, and
      ;; look for a second "@" within one of them.  Testing the whole
      ;; header at once cannot tell a missing comma from a display name
      ;; that contains an "@" -- an encoded word holding an address, say,
      ;; which is legal and which Exchange and Outlook both produce.
      (dolist (address (vm-parse-addresses contents))
        (let ((bare (vm-mail-check-recipients-strip address)))
          (when (string-match "@[^,]*@" bare)
            (setq errors
                  (vm-replace-in-string
                   (format
                    "vm-mail-check-recipients: Missing separator in %s \"%s\"!  "
                    (car header-list) address)
                   "[\n\t ]+" " ")))))
      (setq header-list (cdr header-list)))
    ;; "%s" matters: the message has an address interpolated into it, and
    ;; "%" is legal in a local part -- percent-hack routing uses it -- so
    ;; passing it as the format string fails with "Not enough arguments
    ;; for format string" instead of saying what is wrong.
    (if errors
        (error "%s" errors))))


(defcustom vm-mail-prompt-if-subject-empty t
  "*Prompt for a subject when empty."
  :group 'vm-rfaddons
  :type '(boolean))

;;;###autoload
(defun vm-mail-check-for-empty-subject ()
  "Check if the subject line is empty and issue an error if so."
  (interactive)
  (let (subject)
    (setq subject (vm-mail-mode-get-header-contents "Subject:"))
    (if (or (not subject) (string-match "^[ \t]*$" subject))
        (if (not vm-mail-prompt-if-subject-empty)
            (error "Empty subject header")
          (mail-position-on-field "Subject")
          (insert (read-string "Subject: "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface vm-shrunken-headers-face 
  '((((class color) (background light))
     (:background "grey"))
    (((class color) (background dark))
     (:background "DimGrey"))
    (t (:dim t)))
  "Used for marking shrunken headers."
  :group 'vm-rfaddons)

(defconst vm-shrunken-headers-keymap
  (let ((map (if (featurep 'xemacs) (make-keymap) (copy-keymap vm-mode-map))))
    (define-key map [(return)]   'vm-shrunken-headers-toggle-this)
    (if (featurep 'xemacs)
        (define-key map [(button2)]  'vm-shrunken-headers-toggle-this-mouse)
      (define-key map [(mouse-2)]  'vm-shrunken-headers-toggle-this-mouse))
    map)
  "Keymap used for shrunken-headers glyphs.")

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
(defcustom vm-mail-mode-citation-kill-regexp-alist
  (list
   ;; empty lines multi quoted 
   (cons (concat "^\\(" vm-included-text-prefix "[|{}>:;][^\n]*\n\\)+")
         "[...]\n")
   ;; empty quoted starting/ending lines
   (cons (concat "^\\([^|{}>:;]+.*\\)\n"
                 vm-included-text-prefix "[|{}>:;]*$")
         "\\1")
   (cons (concat "^" vm-included-text-prefix "[|{}>:;]*\n"
                 "\\([^|{}>:;]\\)")
         "\\1")
   ;; empty quoted multi lines 
   (cons (concat "^" vm-included-text-prefix "[|{}>:;]*\\s-*\n\\("
                 vm-included-text-prefix "[|{}>:;]*\\s-*\n\\)+")
         (concat vm-included-text-prefix "\n"))
   ;; empty lines
   (cons "\n\n\n+"
         "\n\n")
   ;; signature & -----Ursprüngliche Nachricht-----
   (cons (concat "^" vm-included-text-prefix "--[^\n]*\n"
                 "\\(" vm-included-text-prefix "[^\n]*\n\\)+")
         "\n")
   (cons (concat "^" vm-included-text-prefix "________[^\n]*\n"
                 "\\(" vm-included-text-prefix "[^\n]*\n\\)+")
         "\n")
   )
  "*Regexp replacement pairs for cleaning of replies."
  :group 'vm-rfaddons
  :type '(repeat (cons :tag "Kill Definition"
                       (regexp :tag "Regexp")
                       (string :tag "Replacement"))))
   
(defun vm-mail-mode-citation-clean-up ()
  "Remove doubly-cited text and extra lines in a mail message."
  (interactive)
  (save-excursion
    (mail-text)
    (let ((re-alist vm-mail-mode-citation-kill-regexp-alist)
          (pmin (point))
          re subst)

      (while re-alist
        (goto-char pmin)
        (setq re (caar re-alist)
              subst (cdar re-alist))
        (while (re-search-forward re (point-max) t)
          (replace-match subst))
        (setq re-alist (cdr re-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-mail-mode-install-open-line ()
  "Install the open-line hooks for VM composition buffers.
Add this to `vm-mail-mode-hook'."
  ;; these are not local even when using add-hook, so we make them local
  (add-hook 'before-change-functions 'vm-mail-mode-open-line nil t)
  (add-hook 'after-change-functions 'vm-mail-mode-open-line nil t))

(defvar vm-mail-mode-open-line nil
  "Flag used by `vm-mail-mode-open-line'.")

(defcustom vm-mail-mode-open-line-regexp "[ \t]*>"
  "Regexp matching prefix of quoted text at line start."
  :type 'regexp)

(defun vm-mail-mode-open-line (start end &optional length)
  "Opens a line when inserting into the region of a reply.

Insert newlines before and after an insert where necessary and does a cleanup
of empty lines which have been quoted." 
  (if (= start end)
      (save-excursion
        (beginning-of-line)
        (setq vm-mail-mode-open-line
              (if (and (eq this-command 'self-insert-command)
                       (looking-at (concat "^"
                                           vm-mail-mode-open-line-regexp)))
                  (if (< (point) start) (point) start))))
    (if (and length (= length 0) vm-mail-mode-open-line)
        (let (start-mark end-mark)
          (save-excursion 
            (if (< vm-mail-mode-open-line start)
                (progn
                  (insert "\n\n" vm-included-text-prefix)
                  (setq end-mark (point-marker))
                  (goto-char start)
                  (setq start-mark (point-marker))
                  (insert "\n\n"))
              (if (looking-at (concat "\\("
                                      vm-mail-mode-open-line-regexp
                                      "\\)+[ \t]*\n"))
                  (replace-match ""))
              (insert "\n\n")
              (setq end-mark (point-marker))
              (goto-char start)
              (setq start-mark (point-marker))
              (insert "\n"))

            ;; clean leading and trailing garbage 
            (let ((iq (concat "^" vm-mail-mode-open-line-regexp
                              "[> \t]*\n")))
              (save-excursion
                (goto-char start-mark)
                (beginning-of-line)
                (while (looking-at "^$") (forward-line -1))
                (while (looking-at iq)
                  (replace-match "")
                  (forward-line -1))
                (goto-char end-mark)
                (beginning-of-line)
                (while (looking-at "^$") (forward-line 1))
                (while (looking-at iq)
                  (replace-match "")))))
      
          (setq vm-mail-mode-open-line nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-mail-mode-elide-reply-region "[...]\n"
  "*String which is used as replacement for elided text."
  :group 'vm-rfaddons
  :type '(string))

;;;###autoload
(defun vm-mail-mode-elide-reply-region (b e)
  "Replace marked region or current line with `vm-mail-mode-elide-reply-region'.
B and E are the beginning and end of the marked region or the current line."
  (interactive (if (mark)
                   (if (< (mark) (point))
                       (list (mark) (point))
                     (list (point) (mark)))
                 (list (save-excursion (beginning-of-line) (point))
                       (save-excursion (end-of-line) (point)))))
  (if (eobp) (insert "\n"))
  (if (mark) (delete-region b e) (delete-region b (+ 1 e)))
  (insert vm-mail-mode-elide-reply-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defvaralias 'vm-mime-display-internal-multipart/mixed-separator
  'vm-mime-parts-display-separator)

(make-obsolete-variable 'vm-mime-display-internal-multipart/mixed-separator
			'vm-mime-parts-display-separator
			"8.2.0")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(provide 'vm-rfaddons)
;;; vm-rfaddons.el ends here
