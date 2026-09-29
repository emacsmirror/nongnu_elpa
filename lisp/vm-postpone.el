;;; vm-postpone.el --- draft/postponed message handling for VM  -*- lexical-binding: t; -*-
;;
;; This file is an add-on for VM
;; 
;; Copyright (C) 1998-2006 Robert Fenk
;; Copyright (C) 2024-2026 The VM Developers
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.19 & VM 7.19
;; Keywords:    vm draft handling
;; X-URL:       http://www.robf.de/Hacking/elisp

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
;;
;;; Commentary:
;; 
;; This package provides the following new features for VM:
;;
;;  A Pine-like postpone message function and folder.  There are two new
;;  functions. `vm-postpone-message' bound to [C-c C-d] in
;;  the composition buffer and the function `vm-continue-postponed-message'
;;  is bound to [C] in a folder buffer.
;;
;;  Typical usage: If you are writing a mail message, and you wish to
;;  postpone it for a while, hit C-c C-d.  The message will be saved in
;;  a folder called "postponed" by default.  Later, when you wish to
;;  resume editing that file, visit the "postponed" folder, find the
;;  message you wish to continue editing, and then hit C to resume
;;  editing.
;;
;;  Furthermore, this facility can be configured, using
;;  `vm-continue-what-message' to imitate Pine's message composing.
;;  You can set `vm-mode-map' in the following way to get Pine-like
;;  behaviour:
;;
;;  (define-key vm-mode-map "m" 'vm-continue-what-message)
;;
;;  Switch it on with
;;
;;  (vm-postpone-mode 1)
;;
;;  (require 'vm-postpone) on its own switched this on until 2026, and does
;;  nothing now beyond making the mode available: loading a file and asking
;;  for what it does are separate acts, and Customize loads this one without
;;  being asked (emacs-vm/vm#788).
;;  (setq vm-zero-drafts-start-compose t)
;;
;;  If you have postponed messages you will be asked if you want to continue
;;  composing them, if you say "yes" you will visit the `vm-postponed-folder'
;;  and you can select the message you would like to continue and press "m"
;;  again!  However be aware this works currently only if you expunge all
;;  messages marked for deletion and save the postponed folder.
;;
;;  You can also bind it to "C-x m" in order to check for postponed messages
;;  when composing a message without starting VM.
;;
;;  (autoload 'vm-continue-what-message-other-window "vm-postpone" "" t)
;;  (global-set-key "\C-xm" 'vm-continue-what-message-other-window)
;;
;;
;;  Three new mail header insertion functions make life easier. The
;;  bindings and names are:
;;     "\C-c\C-f\C-a"  vm-mail-return-receipt-to
;;     "\C-c\C-f\C-p"  vm-mail-priority
;;     "\C-c\C-f\C-f"  vm-mail-fcc
;;  The variables `vm-mail-return-receipt-to' and `vm-mail-priority'
;;  can be used to configure the inserted headers.
;;  `vm-mail-fcc' can be configured by setting the variable
;;  `vm-mail-folder-alist' which has the same syntax and default
;;  value as `vm-auto-folder-alist'.
;;  You may also add `vm-mail-auto-fcc' to `vm-reply-hook' in order to
;;  automatically setup the FCC header according to the variable
;;  `vm-mail-folder-alist'.
;;  There is another fcc-function `vm-mail-to-fcc' which set the FCC
;;  according to the recipients email-address.
;;
;;; Bug reports and feature requests:
;; Please report issues at https://gitlab.com/emacs-vm/vm/-/issues

;;; Code:

(require 'vm-macro)
(require 'vm-vars) 
(require 'vm-misc)
(require 'vm-folder)
(require 'vm-summary)
(require 'vm-window)
(require 'vm-minibuf)
(require 'vm-motion)
(require 'vm-undo)
(require 'vm-delete)
(require 'vm-mime)
(require 'vm-reply)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-session-initialization "vm" ())
(declare-function vm-visit-folder "vm" (folder &optional read-only))

(declare-function bbdb-extract-address-components 
		  "ext:bbdb" (adstring &optional ignore-errors))
(declare-function bbdb/vm-alternate-full-name "ext:bbdb-vm" (address))

; Group already defined in vm-vars.el      

(defgroup vm-postpone nil
  "Postponed message handling and draft support in VM."
  :group  'vm-ext)

;; This group was called `vm-pine' before 8.4.0.  There is nothing to write
;; here for that: Customize has no group-alias mechanism, and the two lines that
;; used to stand here -- a `defvaralias' from `vm-pine' to `vm-postpone' and an
;; obsolescence notice on it -- aliased one non-variable to another and made
;; `vm-pine' a variable alias pointing at nothing.

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-summary-function-f (m)
  "Return the recipient or newsgroup for uninteresting senders.
If the \"From:\" header contains the user login or full name then
this function returns the \"To:\" or \"Newsgroups:\" header field with a
\"To:\" as prefx.

For example the outgoing message box will now list to whom you sent the
messages.  Use `vm-fix-my-summary' to update the summary of a folder! With
loaded BBDB it uses `vm-summary-function-B' to obtain the full name of the
sender.  The only difference to VM's default behavior is the honoring of
messages sent to news groups.)

See also:    `vm-summary-uninteresting-senders'"
  (interactive)
  (let ((case-fold-search t)
        (headers '(("From:" "") 
                   ("Newsgroups:" "News:")
                   "To:" "CC:" "BCC:"
                   "Resent-To:" "Resent-CC:" "Resent-BCC:"
                   ("Sender:" "")  ("Resent-From:" "Resent:")))
        header-name arrow
        addresses
        address
        first first-arrow)

    (while (and (not address) headers)
      (if (listp (car headers))
          (setq header-name (caar headers) arrow (cadar headers))
        (setq header-name (car headers) arrow (concat header-name " ")))
      (setq addresses (vm-get-header-contents m header-name))
      (if addresses
          (setq addresses (vm-decode-mime-encoded-words-in-string addresses)
                addresses
                (if (equal header-name "Newsgroups:")
                    ;; a group is not an address: extraction reads
                    ;; comp.emacs as somebody called "comp emacs"
                    (mapcar (lambda (group) (list group nil))
                            (vm-parse addresses
                                      "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)"))
                  (or (if (functionp 'bbdb-extract-address-components)
                          (bbdb-extract-address-components addresses t))
                      (list (mail-extract-address-components addresses))
                      addresses))))
      ;; the label goes with the address, so it is kept with it: the fallback
      ;; below used whichever header was examined last
      (if (not first) (setq first (car addresses) first-arrow arrow))
      (while addresses
        (if (or (not vm-summary-uninteresting-senders)
                (and vm-summary-uninteresting-senders
                     (not (string-match vm-summary-uninteresting-senders
                                        (format "%s" (car addresses))))))
            (setq address (car addresses) addresses nil))
        (setq addresses (cdr addresses)))
      (setq headers (cdr headers)))

    (if (and (null address) (null first))
        ""
      (if (and (null address) first)
          (setq address first arrow first-arrow))
      (concat arrow
              (cond ((functionp 'bbdb/vm-alternate-full-name)
                     (or (bbdb/vm-alternate-full-name (cadr address))
                         (car address)
                         (cadr address)))
                    (t (or (car address) (cadr address))))))))

;;-----------------------------------------------------------------------------
;;;###autoload
(defcustom vm-postponed-header "X-VM-postponed-data: "
  "Additional header which is inserted to postponed messages.
It is used for internal things and should not be modified. 
It is a lisp list which currently contains the following items:
 <date of the postponing>
 <reply references list>
 <forward references list>
 <redistribute references list>
while the last three are set by `vm-get-persistent-message-ids-for'."
  :type 'string
  :group 'vm-postpone)

;;-----------------------------------------------------------------------------
;; A Pine-like postponed folder handling
;;;###autoload
(defcustom vm-postponed-folder "postponed"
  "The name of the folder where postponed messages are saved."
  :type 'string
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-auto-expunge-postponed-folder nil
  "If non-nil, the postponed-folder is auto-expunged whenever
postponed messages are continued and sent out."
  :type 'boolean
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-postponed-message-headers '("From:" "Organization:"
                                          "Reply-To:"
                                          "To:" "Newsgroups:"
                                          "CC:" "BCC:" "FCC:"
                                          "In-Reply-To:"
                                          "References:"
                                          "Subject:"
                                          "X-Priority:" "Priority:")
  "Similar to `vm-forwarded-headers'.
A list of headers that should be kept, when continuing a postponed message.

The following mime headers should not be kept, since this breaks things:
Mime-Version, Content-Type, Content-Transfer-Encoding."
  :type '(repeat (string))
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-postponed-message-discard-header-regexp nil
  "Similar to `vm-unforwarded-header-regexp'.
A regular expression matching all headers that should be discard when
when continuing a postponed message."
  :type '(choice (const :tag "Discard nothing" nil)
		 (regexp))
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-continue-postponed-message-hook nil
  "List of hook functions to be run after continuing a postponed message."
  :type 'hook
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-postpone-message-hook nil
  "List of hook functions to be run before postponing a message.
They run in the composition buffer, before it is written to the folder, so a
function here can still change what is filed.  See
`vm-after-postpone-message-hook' for after it is filed."
  :type 'hook
  :group 'vm-postpone)

(defcustom vm-after-postpone-message-hook nil
  "List of hook functions to be run after a message has been postponed.
They run in the composition buffer, after the draft is in the folder and the
source message has been dealt with, and before the composition is killed.  So
the buffer is still there to be read, and changing it changes nothing: what
was filed is already filed.

`vm-postpone-message-hook' is the one that runs before, where a change still
reaches the folder."
  :type 'hook
  :group 'vm-postpone)

(defvar vm-postponed-message-folder-buffer nil
  "Buffer of source folder.
This is only for internal use of vm-postpone.el.")

;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun vm-get-persistent-message-ids-for (mlist)
  "Return a list of message id and folder name of all messages in MLIST."
  (let (mp midlist folder mid f)
    (while mlist
      (setq mp (car mlist)
            folder (buffer-file-name (vm-buffer-of (vm-real-message-of mp)))
            mid (vm-message-id-of mp)
            f (assoc folder midlist))
      (if mid
          (if f
              (setcdr f (cons mid (cdr f)))
            (push (list folder mid) midlist)))
      (setq mlist (cdr mlist)))
    midlist))
  
(defun vm-get-message-pointers-for (msgidlist)
  "Return the message pointers belonging to the messages listed in MSGIDLIST.
MSGIDLIST is a list as returned by `vm-get-persistent-message-ids-for'."
  (let (folder vm-message-pointers)
    (while msgidlist
      (setq folder (caar msgidlist))
      (save-excursion
        (when (cond ((get-buffer folder)
                     (set-buffer (get-buffer folder)))
                    ((get-file-buffer folder)
                     (set-buffer (get-file-buffer folder)))
                    ((file-exists-p folder)
                     (vm-visit-folder folder))
                    (t
                     (message "The folder '%s' does not exist anymore.  Maybe it was virtual or closed before postponing." folder)
                     nil))
          (vm-select-folder-buffer)
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((msgid-regexp (concat "^Message-Id:\\s-*"
                                        (regexp-opt (cdar msgidlist))))
                  (point-max (point-max))
                    (case-fold-search t))
              
              (while (re-search-forward msgid-regexp point-max t)
                (let ((point (point))
                      (mp vm-message-list))
                  (while mp
                    (if (and (>= point (vm-start-of (car mp)))
                             (<= point (vm-end-of (car mp))))
                        (setq vm-message-pointers (cons (car mp)
                                                        vm-message-pointers)
                              mp nil)
                      (setq mp (cdr mp)))))))))
        (setq msgidlist (cdr msgidlist))))
    vm-message-pointers))

(defvar mail-mode-hook)
(defvar mail-setup-hook)

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-continue-postponed-message (&optional silent draft)
  "Continue composing of the currently selected message.
Before continuing the composition you may decode the presentation as
you like, by pressing [D] and viewing part of the message!
Then current message is copied to a new buffer and the vm-mail-mode is
entered.  When every thing is finished the hook functions in
`vm-mail-mode-hook' and `vm-continue-postponed-message-hook' are
executed.  When called with a prefix argument it will not switch to
the composition buffer, this may be used for automatic editing of
messages.

The variables `vm-postponed-message-headers' and
`vm-postponed-message-discard-header-regexp' control which
headers are copied to the composition buffer.

If optional argument SILENT is positive then act in background (no frame
creation). If DRAFT is non-nil, then do not delete the draft message."
  (interactive "P")

  (vm-session-initialization)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))

  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  
  (save-restriction
    (widen)
    (let* ((folder-buffer (current-buffer))
           (presentation-buffer vm-presentation-buffer)
           (vmp vm-message-pointer)
           (is-decoded vm-mime-decoded)
           (hstart (vm-headers-of (car vmp)))
           (tstart (vm-text-of    (car vmp)))
           (tend   (- (vm-end-of     (car vmp)) 1))
           (to (format "mail to %s" (vm-get-header-contents (car vmp)
                                                            "To:" ",")))
           (vm-pp-data (vm-get-header-contents (car vmp)
                                               vm-postponed-header)))
      
      ;; Prepare the composition buffer
      (if (and to (string-match "[^,\n<(]+" to))
          (setq to (match-string 0 to)))
      
      (if (not silent)
          (let ((vm-mail-hook nil)
                (vm-mail-mode-hook nil)
                (this-command 'vm-mail))
            (vm-mail-internal :to to))
        (set-buffer (generate-new-buffer to))
        (setq default-directory (expand-file-name
                                 (or vm-folder-directory "~/")))
        (auto-save-mode (if auto-save-default 1 -1))
        (let ((mail-mode-hook nil)
              (mail-setup-hook nil))
          (mail-mode))
        (setq vm-mail-buffer folder-buffer))
      
      (make-local-variable 'vm-postponed-message-folder-buffer)
      (setq vm-postponed-message-folder-buffer
            (vm-buffer-of (vm-real-message-of (car vmp))))
      (make-local-variable 'vm-message-pointer)
      (setq vm-message-pointer vmp)
      (unless draft
	(add-hook 'mail-send-hook 'vm-delete-postponed-message t t))
      (erase-buffer)

      ;; set the VM variables for setting source message attributes
      (when vm-pp-data
        (make-local-variable 'vm-reply-list)
        (make-local-variable 'vm-forward-list)
        (make-local-variable 'vm-redistribute-list)
        (setq vm-pp-data (read vm-pp-data)
              vm-reply-list 
              (and (nth 1 vm-pp-data) 
		   (vm-get-message-pointers-for (nth 1 vm-pp-data)))
              vm-forward-list
              (and (nth 2 vm-pp-data) 
		   (vm-get-message-pointers-for (nth 2 vm-pp-data)))
              vm-redistribute-list
              (and (nth 3 vm-pp-data) 
		   (vm-get-message-pointers-for (nth 3 vm-pp-data))))
        (if vm-reply-list (setq vm-system-state 'replying))
        (if vm-forward-list (setq vm-system-state 'forwarding))
        (if vm-redistribute-list (setq vm-system-state 'redistributing)))
      
      ;; Prepare headers
      (insert-buffer-substring folder-buffer hstart tstart)
      (goto-char (point-min))
      ;; The MIME headers must be kept if and only if the body we are
      ;; about to insert is the raw, still-encoded one.  Dropping them
      ;; while copying raw text leaves boundary lines in the body with
      ;; nothing declaring them, and the send-time re-encoding then
      ;; buries them in a fresh part -- the attachments are lost.
      ;; Keeping them while copying decoded text is just as wrong.
      (cond ((or (vm-mime-plain-message-p (car vmp))
		 (and is-decoded presentation-buffer))
             (vm-reorder-message-headers
	      nil :keep-list vm-postponed-message-headers
	      :discard-regexp vm-postponed-message-discard-header-regexp))
            (t ; copy undecoded messages with mime headers
             (vm-reorder-message-headers
	      nil
	      :keep-list (append '("MIME-Version:" "Content-type:"
				   "Content-Transfer-Encoding:")
				 vm-postponed-message-headers)
	      :discard-regexp vm-postponed-message-discard-header-regexp)))
      (vm-decode-mime-encoded-words)
      (search-forward-regexp "\n\n")
      (replace-match (concat "\n" mail-header-separator "\n") t t)

      ;; Add the message body.  Widened, in both buffers: a message being
      ;; previewed has its presentation buffer narrowed to the headers and
      ;; however many lines `vm-preview-lines' says, and the folder buffer is
      ;; narrowed to the message being shown -- so the body was copied from
      ;; whatever happened to be visible, and continuing a draft without
      ;; showing it first produced a composition with no text in it at all
      ;; (emacs-vm/vm#621).
      (goto-char (point-max))
      (insert
       (if presentation-buffer
           (with-current-buffer presentation-buffer
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (search-forward-regexp "\n\n")
                 (buffer-substring (match-end 0) (point-max)))))
         (with-current-buffer folder-buffer
           (save-restriction
             (widen)
             (buffer-substring tstart tend)))))
      ;; in order to show headers hidden by vm-shrunken-headers 
      (put-text-property (point-min) (point-max) 'invisible nil)
      
      ;; and add the buttons for attachments
      (vm-mime-convert-to-attachment-buttons)))

  (when (not silent)
    (run-hooks 'mail-setup-hook)
    (run-hooks 'vm-mail-hook)
    (run-hooks 'vm-mail-mode-hook))
  
  (run-hooks 'vm-continue-postponed-message-hook))

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-reply-by-continue-postponed-message ()
  "Like `vm-reply' but preserves attachments."
  (interactive)
  (let ((vm-continue-postponed-message-hook)
        (vm-reply-hook nil)
        (vm-mail-mode-hook nil)
        (mail-setup-hook nil)
        (mail-signature nil)
        reply-buffer
        start end)
    (vm-reply 1)
    (save-excursion
      (vm-continue-postponed-message t)
      (goto-char (point-min))
      (re-search-forward 
       (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
       (point-max))
      (forward-char 1)
      (setq reply-buffer (current-buffer)
            start (point)
            end (point-max)))
    (goto-char (point-max))
    (insert-buffer-substring reply-buffer start end)
    (vm-add-reply-subject-prefix (car vm-message-pointer)))
  (run-hooks 'mail-setup-hook)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook)
  (run-hooks 'vm-reply-hook))

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-delete-postponed-message ()
  "Delete the source message belonging to the continued composition."
  (interactive)
  (when vm-message-pointer
    ;; A warning, not an error.  By the time this runs the draft is already
    ;; in the folder, and this is on `mail-send-hook' too, so signalling
    ;; would abandon a send over a failure that has already been survived.
    ;; The handler used to be the bare string below, which `condition-case'
    ;; returns rather than signals, so every failure here was silent.
    (condition-case err
	(let* ((msg (car vm-message-pointer))
	       (buffer (vm-buffer-of msg)))
	  ;; only delete messages which have been postponed by us before
	  (when (vm-get-header-contents msg vm-postponed-header)
	    (vm-set-deleted-flag msg t)
	    (vm-update-summary-and-mode-line))
	  ;; in the postponded folder expunge them right now 
	  (when (string= (buffer-name buffer)
			 (file-name-nondirectory vm-postponed-folder))
	    (when vm-auto-expunge-postponed-folder
              (save-excursion
                (switch-to-buffer buffer)
                (vm-expunge-folder)
                (vm-save-folder)
                (when (not vm-message-list)
                  (let ((this-command 'vm-quit))
                    (vm-quit)))))))
      (error
       (vm-warn 0 2 "Source message not deleted, its folder is gone: %s"
		(error-message-string err))))))

;;-----------------------------------------------------------------------------

;; The following functions have been integrated into vm-mime.el
;; USR, 2011-01-25


;; `vm-pine-fake-attachment-overlays' was aliased here to
;; `vm-mime-re-fake-attachment-overlays', which was deleted as unused in 2011
;; (see the note in vm-mime.el).  The alias has been a void function ever
;; since, and `make-obsolete' was telling anyone who called it to use a name
;; that does not exist either, so both are gone.


;;-----------------------------------------------------------------------------
(defconst vm-postpone-key-bindings
  '(("\C-c\C-f\C-a" . vm-mail-return-receipt-to)
    ("\C-c\C-f\C-p" . vm-mail-priority)
    ("\C-c\C-f\C-f" . vm-mail-fcc)
    ("\C-c\C-f\C-n" . vm-mail-notice-requested-upon-delivery-to))
  "The `vm-mail-mode-map' keys `vm-postpone-mode' binds, each inserting a header.

`C-c C-d' is not among them.  This file bound it to `vm-postpone-message' as
it loaded, but `vm-mail-mode-map' in vm-vars.el already binds it to the same
command, so that was a no-op -- and unbinding it with the mode would take away
a binding VM's core owns.

Two of these four shadow `mail-mode-map', the parent map: it has
`mail-mail-reply-to' on C-c C-f C-a and `mail-fcc' on C-c C-f C-f.  Turning
the mode off removes the shadow and those show through again, rather than
leaving the keys undefined.")

(defvar vm-postpone-message-modes-to-disable
  '(font-lock-mode ispell-minor-mode filladapt-mode auto-fill-mode)
  "A list of modes to disable before postponing a message.")

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-postpone-message (&optional folder dont-kill no-postpone-header)
  "Save the current composition as a draft.
Before saving the composition the `vm-postpone-message-hook' functions
are executed and it is written into the FOLDER `vm-postponed-folder'.
When called with a prefix argument you will be asked for
the folder.

With DONT-KILL, keep the composition buffer rather than killing it, and
insert an FCC header naming FOLDER so that sending it later files it there
again.  The source message is deleted either way.

With NO-POSTPONE-HEADER, leave out the `vm-postponed-header' line that
records the reply, forward and redistribute lists.  The draft is then an
ordinary message, and `vm-continue-what-message' offers to continue only a
message carrying that header."
  (interactive "P")
  
  (let ((message-buffer (current-buffer))
        folder-buffer
        target-type)

    (let (m (modes vm-postpone-message-modes-to-disable))
      (while modes
        (setq m (car modes) modes (cdr modes))
        (if (and (boundp m) (symbol-value m))
            (funcall m 0))))

    (if (and folder (not (stringp folder)))
        (setq folder (vm-read-file-name
                      (format "Postpone to folder (%s): " vm-postponed-folder)
                      (or vm-folder-directory default-directory)
                      vm-postponed-folder nil nil
                      'vm-folder-history)))
    
    ;; there is no explicit folder given ...
    (if (not folder)
        (if vm-postponed-message-folder-buffer
            (setq folder (buffer-file-name vm-postponed-message-folder-buffer))
          (setq folder (expand-file-name vm-postponed-folder
                                         (or vm-folder-directory
                                             default-directory)))))

    (if (not folder)
        (error "I could not find a folder for postponing messages!"))

    ;; if it is no absolute folder path then prepend the folder directory
    (if (not (file-name-absolute-p folder))
        (setq folder (expand-file-name folder
                                       (or vm-folder-directory
                                           default-directory))))

    ;; Now add possibly missing headers
    (goto-char (point-min))
    (vm-mail-mode-show-headers)
    (if (not (vm-mail-mode-get-header-contents "From:"))
        (let* ((login user-mail-address)
               (fullname (user-full-name)))
          (cond ((and (eq mail-from-style 'angles) login fullname)
                 (insert (format "From: %s <%s>\n" fullname login)))
                ((and (eq mail-from-style 'parens) login fullname)
                 (insert (format "From: %s (%s)\n" login fullname)))
                (t
                 (insert (format "From: %s\n" login))))))
    
    ;; mime-encode the message if necessary and add "attachment" disposition
    (condition-case nil (vm-mime-encode-composition t) (error t))

    ;; add the current date 
    (if (not (vm-mail-mode-get-header-contents "Date:"))
        (insert "Date: "
                (format-time-string "%a, %d %b %Y %H:%M:%S %Z"
                                    (current-time))
                "\n"))
    ;; add the postponed header
    (vm-mail-mode-remove-header vm-postponed-header)

    (if no-postpone-header nil
      (insert vm-postponed-header " "
              (format
               "(\"%s\" %S %S %S)\n"
               (format-time-string "%a, %d %b %Y %T %Z" (current-time))
               (vm-get-persistent-message-ids-for vm-reply-list)
               (vm-get-persistent-message-ids-for vm-forward-list)
               (vm-get-persistent-message-ids-for vm-redistribute-list))))

    ;; ensure that the message ends with an empty line!
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (insert "\n\n\n")
    
    ;; run the hooks 
    (run-hooks 'vm-postpone-message-hook)

    ;; delete mail header separator
    (goto-char (point-min))
    (if (re-search-forward 
	 (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
	 nil t)
        (delete-region (match-beginning 0) (match-end 0)))


    (setq folder-buffer (vm-get-file-buffer folder))
    (if folder-buffer
        ;; o.k. the folder is already opened
        (with-current-buffer folder-buffer
          (vm-error-if-folder-read-only)
          (let ((buffer-read-only nil))
            (save-restriction
             (widen)
             (goto-char (point-max))
             (vm-write-string (current-buffer) (vm-leading-message-separator))
             ;; An mboxcl2 folder cannot be read back without this.  The
             ;; type is this buffer's, the message is the other one's.
             (let* ((type vm-folder-type)
                    (line (with-current-buffer message-buffer
			    (vm-content-length-header-line type))))
               (when line (vm-write-string (current-buffer) line)))
             (insert-buffer-substring message-buffer)
             (vm-write-string (current-buffer) (vm-trailing-message-separator))

             (cond ((eq major-mode 'vm-mode)
                    (vm-increment vm-messages-not-on-disk)
                    (vm-clear-modification-flag-undos)))
             
             (vm-check-for-killed-summary)
             (vm-assimilate-new-messages)
             (vm-update-summary-and-mode-line))))
      ;; well the folder is not visited, so we write to the file
      ;; A folder created as mboxcl2 is created under a name that says so, or
      ;; it would be read back as From_ (#767).
      (setq folder (vm-new-folder-file-name folder))
      (setq target-type (or (vm-get-folder-type folder)
                            (vm-folder-type-for-name folder)
                            vm-default-folder-type))
      
      (if (eq target-type 'unknown)
          (error "Folder `%s' type is unrecognized" folder))
      
      (vm-write-string folder (vm-leading-message-separator target-type))
      (let ((line (vm-content-length-header-line target-type)))
        (when line (vm-write-string folder line)))
      (write-region (point-min) (point-max) folder t 'quiet)
      (vm-write-string folder (vm-trailing-message-separator target-type)))
    
    ;; delete source message
    (vm-delete-postponed-message)

    ;; the draft is filed and the source is dealt with, so this is what
    ;; "postponed" means; the composition is still here to be read
    (run-hooks 'vm-after-postpone-message-hook)

    ;; mess around with the window configuration 
    (let ((b (current-buffer))
          (this-command 'vm-mail-send-and-exit))
      (cond ((null (buffer-name b));; dead buffer
             ;; This improves window configuration behavior in
             ;; XEmacs.  It avoids taking the folder buffer from
             ;; one frame and attaching it to the selected frame.
             (set-buffer (window-buffer (selected-window)))
             (vm-display nil nil '(vm-mail-send-and-exit)
                         '(vm-mail-send-and-exit
                           reading-message
                           startup)))
            (t
             (vm-display b nil '(vm-mail-send-and-exit)
                         '(vm-mail-send-and-exit reading-message startup)))))
    
    ;; and kill this buffer?
    (if dont-kill
        (insert (concat "FCC: " folder "\n" mail-header-separator))
      ;; The draft is in the folder now, so the auto-save file has done its
      ;; job.  Nothing else would delete it: Emacs deletes a fileless
      ;; buffer's auto-save file when `mail-send' succeeds and at no other
      ;; time -- not when the buffer is killed -- so every postponed
      ;; composition left one behind, in `vm-folder-directory', which is
      ;; where VM points them.
      (delete-auto-save-file-if-necessary t)
      ;; Nothing to confirm: the composition is in the folder.  The guard on
      ;; `kill-buffer-query-functions' cannot see that, because it asks whether
      ;; killing will keep the writing and `vm-postpone-message-hook' has just
      ;; taken `vm-save-killed-message-hook' off -- rightly, the draft being
      ;; filed already.  So postponing asked "has writing in it and has not
      ;; been sent; kill it?" over a composition it had just saved.
      ;;
      ;; `kill-current-buffer', not `kill-this-buffer': that one signals
      ;; unless a menu or a tool bar invoked it (emacs-vm/vm#855).
      (let ((vm-confirm-killing-a-composition nil))
        (kill-current-buffer)))

    (if (vm-interactive-p)
        (message "Message postponed to folder `%s'" folder))))

;;-----------------------------------------------------------------------------
(defun vm-buffer-in-vm-mode ()
  (member major-mode '(vm-mode vm-virtual-mode
                               vm-presentation-mode
                               vm-summary-mode
                               vm-mail-mode)))

(defcustom vm-continue-what-message 'ask
  "Whether to never continue, ask or always continue postponed messages."
  :type '(choice (const :tag "never" nil)
                 (const ask)
                 (const continue))
  :group 'vm-postpone)

(defcustom vm-zero-drafts-start-compose nil
  "When t and there are no drafts, `vm-continue-what-message' call `vm-mail'."
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "start new message" t))
  :group 'vm-postpone)

(defun vm-continue-what-message-composing ()
  "Decide whether to compose a new message or continue a draft.
This checks if the postponed folder contains drafts.
Drafts in other folders are not recognized!

One of `force-continue', `continue', `visit', `none', `declined' or `new'.
`declined' is drafts being there and not being continued -- the question
answered no, or `vm-continue-what-message' nil -- as against `new', which is
there being none.  The two were one value, and the caller told a reader who
had just declined the question that there were no drafts."
  (save-excursion
    (vm-session-initialization)
    
    (let* ((ppfolder (and vm-postponed-folder
                          (expand-file-name vm-postponed-folder
                                            (or vm-folder-directory
                                                default-directory))))
           action
           buffer)
      
      (when current-prefix-arg
        (setq action 'force-continue))
      
      (when (vm-find-composition-buffer)
        (setq action 'continue))
      
      ;; postponed message in current folder
      (when (vm-buffer-in-vm-mode)
        (vm-check-for-killed-folder)
        (vm-select-folder-buffer)
                  
        (if (and vm-message-pointer
                 (vm-get-header-contents (vm-real-message-of
                                          (car vm-message-pointer))
                                         (regexp-quote vm-postponed-header))
                 (not (vm-deleted-flag (car vm-message-pointer))))
            (setq action 'continue)))

      ;; postponed message in postponed folder
      (when (and (not action) (setq buffer (vm-get-file-buffer ppfolder)))
        (if (and (get-buffer-window-list buffer nil 0))
            (when (with-current-buffer buffer
                    (not (vm-deleted-flag (car vm-message-pointer))))
              (message "Please select a draft!")
              (select-window (car (get-buffer-window-list buffer nil 0)))
              (setq action 'none))
          (setq action 'visit)))

      ;; visit postponed folder 
      (when (and (not action) (file-exists-p ppfolder)
                 (> (nth 7 (file-attributes ppfolder)) 0))
        (setq action 'visit))

      (if (not action) (setq action 'new))

      ;; decide what to do
      (setq action 
            (cond ((eq vm-continue-what-message nil)
                   (if (eq action 'visit) 'declined 'new))
                  ((eq vm-continue-what-message 'ask)
                   (if (equal action 'visit)
                       (if (y-or-n-p
                            "Continue composition of postponed messages? ")
                           'visit
                         'declined)
                     action))
                  ((eq vm-continue-what-message 'continue)
                   action)
                  (t
                   action))))))
              
;;;###autoload
(defun vm-continue-what-message (&optional where)
  "Continue compositions or postponed messages if there are some.

With a prefix arg, call `vm-continue-postponed-message', i.e. continue the
currently selected message.

Declining the offer of the drafts folder starts a new message instead, as
does `vm-continue-what-message' nil with drafts on disk: the drafts stay
where they are.  With no drafts anywhere, a new message is started only
when `vm-zero-drafts-start-compose' is t.

See `vm-continue-what-message' and `vm-zero-drafts-start-compose' for
configuration."
  (interactive)
  (if where (setq where (concat "-" where)))
  (let ((action (vm-continue-what-message-composing))
        (visit  (intern (concat "vm-visit-folder" (or where ""))))
        (mail   (intern (concat "vm-mail" (or where "")))))
    (cond ((equal action 'force-continue)
           (vm-continue-postponed-message))
          ((equal action 'continue)
           (if (vm-find-composition-buffer)
               (vm-continue-composing-message)
             (vm-continue-postponed-message)))
          ((equal action 'visit)
           (funcall visit vm-postponed-folder)
           (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
           (add-hook 'vm-quit-hook 'vm-expunge-folder nil t)
	   (when vm-auto-expunge-postponed-folder
	     (vm-expunge-folder))
           (cond ((= (length vm-message-list) 0)
                  (let ((this-command 'vm-quit))
                    (vm-quit))
                  (let ((this-command mail))
                    (funcall mail)))
                 ((= (length vm-message-list) 1)
                  (vm-continue-postponed-message))))
          ((or (eq action 'declined)
               (and vm-zero-drafts-start-compose (eq action 'new)))
           ;; Declining the drafts is not declining to write: the key that
           ;; offered them is the key you press to compose, so it composes.
           ;; Doing nothing made it a dead key -- the drafts folder was
           ;; offered, refused, and that was the whole of the keystroke.
           (let ((this-command mail))
             (funcall mail)))
          ((eq action 'none)
           ;; The drafts folder is on screen and the reader has been asked
           ;; to pick one, with the cursor moved to that window.  Saying
           ;; there are none over the top of that is what this used to do.
           nil)
          (t
           (message "There are no known drafts.")))))

;;;###autoload
(defun vm-continue-what-message-other-window ()
    "Ask for continuing of postponed messages if there are some."
    (interactive)
    (vm-continue-what-message "other-window"))

;;;###autoload
(defun vm-continue-what-message-other-frame ()
  "Ask for continuing of postponed messages if there are some."
  (interactive)
  (vm-continue-what-message "other-frame"))

;;-----------------------------------------------------------------------------
;; And now do some cool stuff when killing a mail buffer
;; This was inspired by Uwe Brauer
(defcustom vm-save-killed-message
  'always
  "What killing a composition with writing in it does with the writing.

`always', the default, files it in `vm-save-killed-messages-folder' and says
so.  Nothing is lost by killing a composition, which is what a reader who has
lost drafts to a keystroke needs (emacs-vm/vm#824).

`ask' asks whether to keep it.  Nil neither keeps it nor asks, and then
`vm-confirm-killing-a-composition' is what stands between a keystroke and the
writing.

A composition nothing has been written in is not kept and is not asked about,
whichever this is."
  :type '(choice (const :tag "keep it" always)
                 (const :tag "ask" ask)
                 (const :tag "never keep it" nil))
  :group 'vm-postpone)

(defcustom vm-save-killed-messages-folder
  vm-postponed-folder
  "The name of the folder where killed messages are saved."
  :type 'string
  :group 'vm-postpone)

(defun vm-add-save-killed-message-hook ()
  (add-hook 'kill-buffer-hook 'vm-save-killed-message-hook nil t))

;;;###autoload
(defun vm-remove-save-killed-message-hook ()
  "Stop keeping this composition as a draft when it is killed.
On `mail-send-hook' and `vm-postpone-message-hook' in every composition
buffer: the writing is somewhere else by then."
  (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))

;;;###autoload
(defun vm-save-killed-message-hook ()
  "Keep this composition as a draft, as `vm-save-killed-message' says to.

On `kill-buffer-hook' in every composition buffer.  A composition nothing has
been written in is neither kept nor asked about nor complained over: VM writes
the headers itself, so every composition is modified from the moment it
appears, and a `vm-mail' typed by mistake is not a draft."
  (when (vm-composition-worth-keeping-p)
    (let ((name (buffer-name)))
      (if (or (eq vm-save-killed-message 'always)
              (and (eq vm-save-killed-message 'ask)
                   (y-or-n-p (format "Save `%s' as draft in folder `%s'? "
                                     name vm-save-killed-messages-folder))))
          (progn
            (vm-postpone-message vm-save-killed-messages-folder t)
            ;; said, since nobody asked for it: a draft nobody knows about is
            ;; one nobody goes back to
            (vm-inform 5 "%s kept as a draft in %s; %s takes it up again"
                       name vm-save-killed-messages-folder
                       (substitute-command-keys
                        "\\[vm-continue-postponed-message]")))
        (vm-inform 1 "%s is gone forever" name)))))

(defconst vm-postpone-hooks nil
  "The hooks `vm-postpone-mode' adds to, and what it adds.

Nothing now.  It used to arrange for a composition killed unsent to be kept as
a draft, which meant that a reader who had not switched the mode on lost the
writing to any key bound to `kill-buffer'.  Every composition arranges it, in
`vm-new-composition-buffer' (emacs-vm/vm#824).  What the mode still does is
bind four keys.")

(defun vm-postpone--drop-empty-prefix (prefix)
  "Take PREFIX out of `vm-mail-mode-map' if this mode left it holding nothing.
`define-key' on a multi-key sequence makes the intermediate keymap it needs,
and unbinding the leaf does not take it away again: turning the mode off left
`vm-mail-mode-map' holding an empty keymap for C-c C-f where it had no entry
for that prefix at all.  Harmless, in that lookup still falls through to
`mail-mode-map', but it is a keymap left changed behind the mode, which
`test-runner --leaks' reports and which would make a second look at this code
wonder what put it there.

Only an empty one, so a prefix something else has put a binding under is left
alone."
  (let ((under (lookup-key vm-mail-mode-map prefix)))
    (when (equal under '(keymap))
      (vm-postpone--unbind prefix))))

(defun vm-postpone--unbind (key)
  "Take KEY out of `vm-mail-mode-map', letting `mail-mode-map' show through.
Removing the entry and not binding it to nil: `vm-mail-mode-map' has
`mail-mode-map' for its parent, and a nil binding in the child shadows the
parent rather than falling through to it, so C-c C-f C-a would be dead where
Mail mode has `mail-mail-reply-to' on it.

`keymap-unset' with its REMOVE argument does that and arrived in Emacs 29;
VM supports 28.1, where the nil is the best available and leaves those two
keys undefined until the mode is turned back on.  It takes a key in the
`key-valid-p' syntax rather than the raw string `define-key' takes, so the
key is described for it."
  (if (fboundp 'keymap-unset)
      (keymap-unset vm-mail-mode-map (key-description key) t)
    (define-key vm-mail-mode-map key nil)))

;;;###autoload
(define-minor-mode vm-postpone-mode
  "Postpone a composition and continue it later, as Pine does.
\\<vm-mail-mode-map>\\[vm-postpone-message] in a composition files it in
`vm-save-killed-messages-folder'; visit that folder and type
\\<vm-mode-map>\\[vm-continue-postponed-message] to take it up again.  That
key is bound by VM itself and works whether this mode is on or off.

What turning this on adds is four keys that insert a header field.  Turning it
off takes them away again and leaves any postponed folder where it is.

It used to arrange for a composition killed unsent to be kept as a draft as
well, which meant a reader who had not switched it on lost the writing to any
key bound to `kill-buffer'.  Every composition arranges that now; see
`vm-save-killed-message' (emacs-vm/vm#824).

Loading this file switched it on until 2026 (emacs-vm/vm#788).  Customize
loads it whenever it is asked about a VM option, so loading no longer enables:
say so here."
  :global t
  :group 'vm-postpone
  (dolist (binding vm-postpone-key-bindings)
    (if vm-postpone-mode
	(define-key vm-mail-mode-map (car binding) (cdr binding))
      ;; Only what this mode bound, so a key another package has taken since
      ;; is left to it.
      (when (eq (lookup-key vm-mail-mode-map (car binding)) (cdr binding))
	(vm-postpone--unbind (car binding)))))
  (unless vm-postpone-mode
    (vm-postpone--drop-empty-prefix "\C-c\C-f"))
  (dolist (pair vm-postpone-hooks)
    (if vm-postpone-mode
	(add-hook (car pair) (cdr pair))
      (remove-hook (car pair) (cdr pair)))))

;;;###autoload
(defcustom vm-mail-return-receipt-to
  (concat (user-full-name) " <" user-mail-address ">")
  "The address where return receipts should be sent to."
  :type 'string
  :group 'vm-postpone)

;;;###autoload
(defun vm-mail-return-receipt-to ()
  "Insert the \"Return-Receipt-To\" header into a VM composition buffer.
See the variable `vm-mail-return-receipt-to'."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Return-Receipt-To" t)
        (progn (mail-position-on-field "Subject")
               (insert "\nReturn-Receipt-To: " vm-mail-return-receipt-to
                       "\nRead-Receipt-To: " vm-mail-return-receipt-to
                       "\nDelivery-Receipt-To: " vm-mail-return-receipt-to))))
  (message "Remove those headers you do not require!"))

;;;###autoload
(defun vm-mail-notice-requested-upon-delivery-to ()
  "Notice-Requested-Upon-Delivery-To:"
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Notice-Requested-Upon-Delivery-To" t)
        (progn (mail-position-on-field "Subject")
               (insert "\nNotice-Requested-Upon-Delivery-To: "
                       (let ((to (vm-mail-get-header-contents
                                  "\\(.*-\\)?To:")))
                         (if to to "")))))))

;;;###autoload
(defcustom vm-mail-priority
  "Priority: urgent\nImportance: High\nX-Priority: 1"
  "The priority headers."
  :type 'string
  :group 'vm-postpone)

;;;###autoload
(defun vm-mail-priority ()
  "Insert priority headers into a VM composition buffer.
See the variable `vm-mail-priority'."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Priority" t)
        (progn (mail-position-on-field "Subject")
               (insert "\n" vm-mail-priority)))))

(defun vm-mail-fcc-file-join (dir file)
  "Returns a nice path to a folder."
  (let* ((path (expand-file-name file dir)))
    (if path 
	(vm-abbreviate-file-name path)
      dir)))

;;;###autoload
(defcustom vm-mail-folder-alist (if (boundp 'vm-auto-folder-alist)
                                    vm-auto-folder-alist)
  "Like `vm-auto-folder-alist' but for outgoing messages.
It should be fed to `vm-mail-select-folder'."
  :type 'sexp
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-mail-fcc-default
  '(or (vm-mail-select-folder vm-mail-folder-alist)
       (vm-mail-to-fcc nil t)
       mail-archive-file-name)
  "A list which is evaluated to return a folder name.
By reordering the elements of this list or adding own functions you
can control the behavior of vm-mail-fcc and `vm-mail-auto-fcc'.
You may allow a sophisticated decision for the right folder for your
outgoing message."
  :type 'sexp
  :group 'vm-postpone)

;;;###autoload
(defun vm-mail-fcc (&optional arg)
  "Insert the FCC-header into a VM composition buffer.
Like `mail-fcc', but honors VM variables and offers a default folder
according to `vm-mail-folder-alist'.
Called with prefix ARG it just removes the FCC-header."
  (interactive "P")
  (expand-abbrev)

  (let ((dir (or vm-folder-directory default-directory))
        (fcc nil)
        (folder (vm-mail-mode-get-header-contents "FCC:"))
        (prompt nil))
    
    (if arg (progn (vm-mail-mode-remove-header "FCC:")
                   (message "FCC header removed!"))
      (save-excursion
        (setq fcc (eval vm-mail-fcc-default))

        ;; cleanup the name 
        (setq fcc (if fcc (vm-mail-fcc-file-join dir fcc)))

        (setq prompt (if fcc
                         (format "FCC to folder (%s): " fcc)
                       "FCC to folder: "))

        (setq folder (if (and folder (not (file-directory-p folder)))
                         (file-relative-name folder dir)))

        ;; we got the name so insert it 
        (vm-mail-mode-remove-header "FCC:")
        (setq fcc (vm-read-file-name prompt
                                        dir fcc
                                        nil folder
                                        'vm-folder-history))
        (setq fcc (vm-mail-fcc-file-join dir fcc))
        (if (file-directory-p fcc)
            (error "Folder `%s' in no file, but a directory!" fcc)
          (mail-position-on-field "FCC")
          (insert fcc))))))

;;;###autoload
(defun vm-mail-auto-fcc ()
  "Add a new FCC field, with file name guessed by `vm-mail-folder-alist'.
You likely want to add it to `vm-reply-hook' by
   (add-hook \\='vm-reply-hook #\\='vm-mail-auto-fcc)
or if sure about what you are doing you can add it to `mail-send-hook'."
  (interactive "")
  (expand-abbrev)
  (save-excursion
    (let ((dir (or vm-folder-directory default-directory))
          (fcc nil))
      
      (vm-mail-mode-remove-header "FCC:")
      (setq fcc (eval vm-mail-fcc-default))
      (when fcc
        ;; the name as it will be written: the check has to be on the file
        ;; the copy would go to, not on the same name read against whatever
        ;; directory the composition happens to be in
        (setq fcc (vm-mail-fcc-file-join dir fcc))
        (if (file-directory-p fcc)
            (error (concat "%s is a directory, so no copy can be filed there;"
                           " name a folder in `vm-mail-folder-alist' or"
                           " `mail-archive-file-name'")
                   fcc)
          (mail-position-on-field "FCC")
          (insert fcc))))))

;;;###autoload
(defun vm-mail-select-folder (folder-alist)
  "Return a folder according to FOLDER-ALIST for the current message.
This function is a slightly changed version of `vm-auto-select-folder'."
  (interactive)
  (condition-case error-data
      (catch 'match
        (let (header tuple-list)
          (while folder-alist
            (setq header (vm-mail-get-header-contents
                          (car (car folder-alist)) ", "))
            (if (null header)
                ()
              (setq tuple-list (cdr (car folder-alist)))
              (while tuple-list
                (if (let ((case-fold-search vm-auto-folder-case-fold-search))
                      (string-match (car (car tuple-list)) header))
                    ;; Don't waste time eval'ing an atom.
                    (if (stringp (cdr (car tuple-list)))
                        (throw 'match (cdr (car tuple-list)))
                      (let* ((match-data (vm-match-data))
                             ;; allow this buffer to live forever
                             (buf (get-buffer-create " *vm-auto-folder*"))
                             (result))
                        ;; Set up a buffer that matches our cached
                        ;; match data.
                        (with-current-buffer buf
                          (set-buffer-multibyte nil) ; for empty buffer
                          (widen)
                          (erase-buffer)
                          (insert header)
                          ;; It appears that get-buffer-create clobbers the
                          ;; match-data.
                          ;;
                          ;; The match data is off by one because we matched
                          ;; a string and Emacs indexes strings from 0 and
                          ;; buffers from 1.
                          ;;
                          ;; Also store-match-data only accepts MARKERS!!
                          ;; AUGHGHGH!!
                          (store-match-data
                           (mapcar
                            (function (lambda (n) (and n (vm-marker n))))
                            (mapcar
                             (function (lambda (n) (and n (1+ n))))
                             match-data)))
                          (setq result (eval (cdr (car tuple-list))))
                          (while (consp result)
                            (setq result (vm-mail-select-folder result)))
                          (if result
                              (throw 'match result))))))
                (setq tuple-list (cdr tuple-list))))
            (setq folder-alist (cdr folder-alist)))
          nil ))
    (error "Error processing folder-alist: %s"
           (prin1-to-string error-data))))

;;;###autoload
(defcustom vm-mail-to-regexp "\\([^<\t\n ]+\\)@"
  "A regexp matching the part of an email address to use as FCC-folder.
The string enclosed in \"\\\\(\\\\)\" is used as folder name."
  :type 'regexp
  :group 'vm-postpone)

;;;###autoload
(defcustom vm-mail-to-headers '("To:" "CC:" "BCC:")
  "A list of headers for finding the email address to use as FCC-folder."
  :type '(repeat (string))
  :group 'vm-postpone)

;;;###autoload
(defun vm-mail-to-fcc (&optional arg return-only)
  "Insert a FCC-header into a VM composition buffer.
Like `mail-fcc', but honors VM variables and inserts the first email
address (or the like matched by `vm-mail-to-regexp') found in the headers
listed in `vm-mail-to-headers'.
Called with prefix ARG it just removes the FCC-header.
If optional argument RETURN-ONLY is t just returns FCC."
  (interactive "P")
  (expand-abbrev)
  (let ((fcc nil)
        (headers vm-mail-to-headers))
    (if arg (progn (vm-mail-mode-remove-header "FCC:")
                   (message "FCC header removed!"))
      (progn
        (while (and (not fcc) headers)
          (setq fcc (vm-mail-get-header-contents (car headers)))
          (if (and fcc (string-match vm-mail-to-regexp fcc))
              (setq fcc (match-string 1 fcc))
            (setq fcc nil))
          (setq headers (cdr headers)))
        (setq fcc (or fcc mail-archive-file-name))
        (if return-only
            fcc
          (if fcc
              (if (file-directory-p fcc)
                  (error "Folder `%s' in no file, but a directory!" fcc)
                (vm-mail-mode-remove-header "FCC:")
                (mail-position-on-field "FCC")
                (insert (vm-mail-fcc-file-join (or vm-folder-directory
                                                   default-directory)
                                               fcc)))))))))


;;-----------------------------------------------------------------------------
;;; Leaving Emacs with a composition unfinished (issue #160)

(defun vm-composition-buffer-p (&optional buffer)
  "Whether BUFFER, or the current buffer, is a composition VM started.
Mail mode alone is not enough: another package's composition is in Mail
mode too, and postponing it into a VM folder is not VM's business."
  (with-current-buffer (or buffer (current-buffer))
    (and (eq major-mode 'mail-mode)
         (eq (current-local-map) vm-mail-mode-map))))

(defun vm-unfinished-compositions ()
  "The composition buffers with something in them, oldest first."
  (nreverse
   (seq-filter (lambda (buffer)
                 (and (vm-composition-buffer-p buffer)
                      (vm-composition-worth-keeping-p buffer)))
               (buffer-list))))

(defun vm-postpone-composition-quietly (buffer)
  "Kill BUFFER, so that `vm-save-killed-message-hook' offers to keep it.
The offer is the one killing a composition has always made, rather than a
second one of its own.  A failure is reported rather than raised: this runs
while Emacs is being left, where an error would put a debugger between the
user and the door."
  (condition-case err
      (progn (kill-buffer buffer) t)
    (error
     (vm-warn 0 2 "Could not save %s as a draft: %s"
              (buffer-name buffer) (error-message-string err))
     nil)))

;;;###autoload
(defun vm-postpone-unfinished-compositions ()
  "Offer each unfinished composition to the drafts folder as Emacs is left.
Killing a composition already offers this -- see `vm-save-killed-message',
which says whether to ask, to save without asking, or to do neither, and
`vm-save-killed-messages-folder', which says where.  But Emacs does not kill
buffers one at a time as it exits, so on the way out the offer was never
made and the drafts went with it.  Issue #160.

Runs from `kill-emacs-query-functions', where a nil return would stop Emacs
leaving.  This always returns t: losing a draft is a reason to ask a
question, not to stand in the doorway."
  (when vm-save-killed-message
    (dolist (buffer (vm-unfinished-compositions))
      (when (buffer-live-p buffer)
        (vm-postpone-composition-quietly buffer))))
  t)

;;-----------------------------------------------------------------------------

(provide 'vm-postpone)
;; Backward compatibility
(provide 'vm-pine)

;;; vm-postpone.el ends here
