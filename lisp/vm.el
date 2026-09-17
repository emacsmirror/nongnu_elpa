;;; vm.el --- VM mail reader -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1994-1998, 2003 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2024-2025 The VM Developers
;;
;; Version: 9.0.0snapshot
;; Maintainer: viewmail-info@nongnu.org
;; URL: https://gitlab.com/emacs-vm/vm
;; Package-Requires: ((emacs "28.1") (vcard "0.2.2"))
;; Keywords: mail
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

;;; Commentary:
;; VM mail reader for Emacs
;; It is designed ot be highly configurable and easy to use. It supports POP
;; and IMAP mail servers, understands MIME, and handles mail folders in the
;; standard UNIX mailbox format and the BABYL format used by the RMAIL
;; mailer. It has a powerful virtual folder facility to facilitate searching
;; as well as more advanced handling of multiple mail folders.

;;; Code:

(provide 'vm)


(require 'vm-macro)
(require 'vm-misc)
(require 'vm-folder)
(require 'vm-summary)
(require 'vm-window)
(require 'vm-minibuf)
(require 'vm-menu)
(require 'vm-toolbar)
(require 'vm-mouse)
(require 'vm-page)
(require 'vm-motion)
(require 'vm-undo)
(require 'vm-delete)
(require 'vm-crypto)
(require 'vm-mime)
(require 'vm-virtual)
(require 'vm-pop)
(require 'vm-imap)
(require 'vm-sort)
(require 'vm-reply)
(eval-when-compile (require 'cl-lib))
(require 'package)

;; keep vm-min-emacs-version in sync with above Package-Requires
(defconst vm-min-emacs-version "28.1"
  "Minimum Emacs version supported by VM.")

(defvar enable-multibyte-characters)

(defvar vm-version nil
  "Version number of VM.")

(defvar vm-version-commit nil
  "Git commit number of VM.")

(defvar vm-version-config nil
  "Version number of VM from generated file when building using make.")

(defvar vm-version-commit-config nil
  "Git commit number of VM from generated file when building using make.")



(declare-function vm-summary-faces-mode "vm-summary-faces.el" 
		  (&optional arg))

;; Don't get any farther if emacs is too old
(when (version< emacs-version vm-min-emacs-version)
  (error "VM requires Emacs %s or later" vm-min-emacs-version))


;; emacs 28 doesn't generate a provide in when generating autoloads
;; yet does bbdb-vm.el  (require 'vm-autoloads). This workds around
;; this issue, yet allows vm.el to work if evaluated without going through
;; vm-autoloads.
(cond ((<= emacs-major-version 28)
       (progn
	 (load-library "vm-autoloads")
	 (provide 'vm-autoloads)))
      (t (require 'vm-autoloads)))

;;;###autoload
(cl-defun vm (&optional folder &key read-only interactive
		      access-method reload just-visit)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It can
be the path name of a local folder or the maildrop specification
of a POP or IMAP folder.  It defaults to the value of
`vm-primary-inbox'.  The folder is visited in a VM buffer that is
put into VM mode, a major mode for reading mail.  (See
`vm-mode'.)

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting a folder normally causes any contents of its spool files
to be moved and appended to the folder buffer.  You can disable
this automatic fetching of mail by setting `vm-auto-get-new-mail'
to nil.

All the messages can be read by repeatedly pressing SPC.  Use `n' for the
next message and `p' for the previous one to move about in the folder.
Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages."

  ;; Additional documentation for internal calls to vm:

  ;; *** Note that this function causes the folder buffer to become
  ;; *** the current-buffer.

  ;; Internally, this function may also be called with a buffer as the
  ;; FOLDER argument.  In that case, the function sets up the buffer
  ;; as a folder buffer and puts it into VM mode.  This is normally used
  ;; with additional options described below.

  ;; ACCESS-METHOD, if non-nil, indicates that the FOLDER is the
  ;; maildrop spec of a remote server folder.  Possible values for the
  ;; parameter are 'pop and 'imap.  Or, if FOLDER is a buffer instead
  ;; of a name, it will be set up as a folder buffer using the
  ;; specified ACCESS-METHOD.

  ;; RELOAD, if non-nil, means that the folder should be reloaded into
  ;; an existing buffer.  All initialisations must be performed but
  ;; some variables need to be preserved, e.g., vm-folder-access-data.

  ;; JUST-VISIT, if non-nil, says that the folder should be visited
  ;; with as little initial processing as possible.  No summary
  ;; generation, no moving of the message-pointer, no retrieval of new
  ;; mail.

  ;; The functions find-name-for-spec and find-spec-for-name translate
  ;; between folder names and maildrop specs for the server folders.

  (interactive (list nil :read-only current-prefix-arg))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  ;; There used to be a recursive call to `vm' here, guarded by
  ;; (unless (boundp 'vm-session-beginning) ...), whose comment said it was
  ;; "to allow defadvice on its first call".  It could never run, for two
  ;; independent reasons: `vm-session-beginning' is defvar'd unconditionally in
  ;; vm-vars.el, which vm-autoloads.el requires, so it is bound before `vm' can
  ;; be reached at all; and `vm-session-initialization' just above requires
  ;; vm-vars itself, so the variable is bound by the time the test is made even
  ;; if it somehow was not before.  The defvar dates from 2007-01-12 and the
  ;; recursive call from 2008-02-15, so it was unreachable when it was written.
  ;; Removed for issue #240.
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (catch 'done
    (unless folder
      (setq folder vm-primary-inbox))

    ;; [1] Deduce the access method if none specified

    (unless access-method
      (cond ((bufferp folder)	    ; may be unnecessary. USR, 2010-01
	     (setq access-method vm-folder-access-method))
	    ((and (stringp folder) (vm-imap-folder-spec-p folder))
	     (setq access-method 'imap))
	    ((and (stringp folder) (vm-pop-folder-spec-p folder))
	     (setq access-method 'pop))
	    ((stringp folder)
	     (setq folder 
		   (expand-file-name folder vm-folder-directory)))))

    ;; [2] Set up control variables that decide what needs to be done
    ;;    (not yet fully understood.  USR, 2012-02)

    (let (;; if we need to read from disk, we need a full startup
	  (full-startup (and (not (bufferp folder)) (not reload)))
	  ;; if JUST-VISIT is t, we just revisit
	  ;; not sure if this flag can be set right away. USR, 2012-02-07
	  (revisiting (and (bufferp folder) just-visit))
	  ;; whether we should set vm-mode in the folder
	  ;; formerly controlled by a variable called `first-time'
	  set-vm-mode
	  ;; whether we should process the VM headers, depends on
	  ;; whether we get the information from an index file
	  gobble-headers
	  ;; whether thunderbird status flags should be processed
	  ;; this currently a global flag, but it shouldn't be
	  ;; whether the auto-save file should be preserved
	  preserve-auto-save-file
	  ;; some local variables
	  folder-buffer folder-name account-name remote-spec totals-blurb)

      ;; [3] Infer the folder (disk file) and the folder-name (buffer-name)

      (cond ((and full-startup (eq access-method 'pop))
	     (setq remote-spec folder)
	     (setq folder-name (or (vm-pop-find-name-for-spec folder) "POP"))
	     (setq folder (vm-pop-find-cache-file-for-spec remote-spec)))
	    ((and full-startup (eq access-method 'imap))
	     (setq remote-spec folder)
	     (setq folder-name (or (nth 3 (vm-imap-parse-spec-to-list
					   remote-spec))
				   folder))
	     (if (and vm-imap-refer-to-inbox-by-account-name
		      (equal (downcase folder-name) "inbox")
		      (setq account-name 
			    (vm-imap-account-name-for-spec remote-spec)))
		 (setq folder-name account-name))
	     (setq folder (vm-imap-make-filename-for-spec remote-spec))))

      ;; [4] Read the folder from disk and switch to it

      (if (bufferp folder)
	  (setq folder-buffer folder)
	(setq folder-buffer (vm-read-folder folder remote-spec folder-name)))
      (set-buffer folder-buffer)
      (setq set-vm-mode (not (eq major-mode 'vm-mode)))
      ;; Thunderbird folders
      (setq vm-folder-read-thunderbird-status 
	    (and (vm-thunderbird-folder-p (buffer-file-name))
		 vm-sync-thunderbird-status))

      ;; [5] Prepare the folder buffer for MULE

      (if enable-multibyte-characters
	  (set-buffer-multibyte nil))	; is this safe?
      (defvar buffer-file-coding-system)
      (vm-setup-fsfemacs-folder-coding-system)

      ;; [6] Safeguards

      (vm-check-for-killed-summary)
      (vm-check-for-killed-presentation)

      ;; [7] Initialize variables in folder buffer

      (unless (buffer-modified-p) 	; don't have messages that are
	(setq vm-messages-not-on-disk 0)) ; not on disk
      (setq preserve-auto-save-file 
	    (and buffer-file-name (not (buffer-modified-p))
		 (file-newer-than-file-p (make-auto-save-file-name)
					 buffer-file-name)))
      (setq vm-folder-read-only 
	    (or preserve-auto-save-file read-only
		(default-value 'vm-folder-read-only)
		(and set-vm-mode buffer-read-only)))

      ;; [8] Initializations for vm-mode

      (when set-vm-mode
	(buffer-disable-undo (current-buffer))
	(abbrev-mode 0)
	(auto-fill-mode 0)
	;; If an 8-bit message arrives undeclared the 8-bit
	;; characters in it should be displayed using the
	;; user's default face charset, rather than as octal
	;; escapes.
	(vm-fsfemacs-nonmule-display-8bit-chars)
	(vm-mode-internal access-method reload)

	(unless (buffer-modified-p) ; if the buffer is modified, the
				    ; index file may be invalid.
	  (let ((did-read-index-file (vm-read-index-file-maybe)))
	    (setq gobble-headers (not did-read-index-file)))))

      (when full-startup	    ; even if vm-mode is already on
	(cond ((eq access-method 'pop)
	       (vm-set-folder-pop-maildrop-spec remote-spec))
	      ((eq access-method 'imap)
	       (vm-set-folder-imap-maildrop-spec remote-spec)
	       ;; No garbage action for the session.  The blocking one
	       ;; registered `vm-kill-folder-imap-session' here, which ended a
	       ;; session that sat idle between commands.  A driver session is
	       ;; meant to outlive the folder buffer -- a quit writes the file
	       ;; and kills the buffer while the flags are still going up --
	       ;; and stopping it on kill-buffer would lose them.
	       )))


      ;; [9] Parse the messages and headers

      ;; Read attributes if they weren't  read from an index file.
      ;; but that is not what the code is doing! - USR, 2011-04-24
      (unless revisiting
	(vm-assimilate-new-messages :read-attributes t
				    :gobble-order gobble-headers 
				    :run-hooks nil)
	(vm-stuff-folder-data :interactive interactive 
			      :abort-if-input-pending t))

      (when (and set-vm-mode gobble-headers)
	(vm-gobble-headers))

      (when just-visit
	(setq full-startup nil))

      ;; Recall the UID VALIDITY value stored in the cache folder
      (when (and (eq access-method 'imap) vm-imap-retrieved-messages)
	(vm-set-folder-imap-uid-validity (vm-imap-recorded-uid-validity)))

      (when set-vm-mode
	(vm-start-itimers-if-needed))

      ;; [10] Create frame

      ;; make a new frame if the user wants one.  reuse an
      ;; existing frame that is showing this folder.
      (when (and full-startup
		 ;; this so that "emacs -f vm" doesn't create a frame.
		 this-command)
	(apply 'vm-goto-new-folder-frame-maybe
	       (if folder '(folder) '(primary-folder folder))))

      ;; raise frame if requested and apply startup window
      ;; configuration.
      (when full-startup
	(let ((buffer-to-display (or vm-summary-buffer
				     vm-presentation-buffer
				     (current-buffer))))
	  (vm-display buffer-to-display buffer-to-display
		      (list this-command)
		      (list (or this-command 'vm) 'startup))
	  (if vm-raise-frame-at-startup
	      (vm-raise-frame))))

      ;; [11] Control point
      ;; if the folder is being revisited, nothing more to be done
      (when (and revisiting (not set-vm-mode))
	(throw 'done t))

      ;; [12] Display the folder

      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (when full-startup
	;; save blurb so we can repeat it later as necessary.
	(setq totals-blurb (vm-emit-totals-blurb)))

      (vm-thoughtfully-select-message)
      (vm-update-summary-and-mode-line)
      ;; need to do this after any frame creation because the
      ;; toolbar sets frame-specific height and width specifiers.
      (vm-toolbar-install-or-uninstall-toolbar)

      (when (and vm-use-menus (vm-menu-support-possible-p))
	(vm-menu-install-visited-folders-menu))

      (when full-startup
	(when (and (vm-should-generate-summary)
		   ;; don't generate a summary if recover-file is
		   ;; likely to happen, since recover-file does
		   ;; not work in a summary buffer.
		   (not preserve-auto-save-file))
	  (vm-summarize t nil))
	;; raise the summary frame if the user wants frames
	;; raised and if there is a summary frame.
	(when (and vm-summary-buffer
		   vm-mutable-frame-configuration
		   vm-frame-per-summary
		   vm-raise-frame-at-startup)
	  (vm-raise-frame))
	;; if vm-mutable-window-configuration is nil, the startup
	;; configuration can't be applied, so do
	;; something to get a VM buffer on the screen
	(if vm-mutable-window-configuration
	    (vm-display nil nil (list this-command)
			(list (or this-command 'vm) 'startup))
	  (save-excursion
	    (switch-to-buffer (or vm-summary-buffer
				  vm-presentation-buffer
				  (current-buffer))))))

      (if vm-message-list
	  ;; don't decode MIME if recover-file is
	  ;; likely to happen, since recover-file does
	  ;; not work in a presentation buffer.
	  (let ((vm-auto-decode-mime-messages
		 (and vm-auto-decode-mime-messages
		      (not preserve-auto-save-file))))
	    (vm-present-current-message)))

      ;; [13] Run hooks

      (run-hooks 'vm-visit-folder-hook)

      ;; [14] Warn user about auto save file, if appropriate.
      (when preserve-auto-save-file
	  (vm-warn 0 2
	   (substitute-command-keys
	    (concat
	     "%s: Auto save file is newer; consider \\[vm-recover-folder].  "
	     "FOLDER IS READ ONLY."))
	   (buffer-name)))
      ;; if we're not doing a full startup or if doing more would
      ;; trash the auto save file that we need to preserve,
      ;; stop here.
      (when (or (not full-startup) preserve-auto-save-file)
	(throw 'done t))
      
      ;; [15] Display the totals-blurb again

      (when interactive
	(vm-inform 5 "%s" totals-blurb))

      ;; [16] Get new mail if requested

      (when (and vm-auto-get-new-mail
		 (not vm-block-new-mail)
		 (not vm-folder-read-only))
	(vm-inform 6 "%s: Checking for new mail..." (buffer-name))
	(let ((got (vm-get-spooled-mail interactive)))
	  (cond
	   ;; The fetch is under way and nothing has arrived yet.  Saying the
	   ;; totals here would say what the folder held before it, which on a
	   ;; folder with a cache is the old count and reads as nothing having
	   ;; happened; the arrival says what came (emacs-vm/vm#825).
	   ((eq got 'started)
	    ;; not the totals: `totals-blurb' names the folder itself, and
	    ;; what it counts is what the fetch has not changed yet
	    (vm-inform 5 "%s: getting new mail..." (buffer-name)))
	   (got
	    (setq totals-blurb (vm-emit-totals-blurb))
	    (if (vm-thoughtfully-select-message)
		(vm-present-current-message)
	      (vm-update-summary-and-mode-line))
	    (vm-inform 5 "%s" totals-blurb))
	   (t (vm-inform 5 "%s" totals-blurb)))))

      ;; [17] Display copyright and copying info.
      (when (and interactive (not vm-startup-message-displayed))
	(vm-display-startup-message)
	(if (not (input-pending-p))
	    (vm-inform 5 "%s" totals-blurb))))))

(defun vm-setup-fsfemacs-folder-coding-system ()
  ;; If the file coding system is not a no-conversion variant,
  ;; make it so by encoding all the text, then setting the
  ;; file coding system and decoding it.  This situation is
  ;; only possible if a file is visited and then vm-mode is
  ;; run on it afterwards.
  (if (null buffer-file-coding-system)
      (set-buffer-file-coding-system 'raw-text nil))
  (if (and (not (eq (coding-system-base buffer-file-coding-system)
		    (coding-system-base 'raw-text-unix)))
	   (not (eq (coding-system-base buffer-file-coding-system)
		    (coding-system-base 'raw-text-mac)))
	   (not (eq (coding-system-base buffer-file-coding-system)
		    (coding-system-base 'raw-text-dos)))
	   (not (eq (coding-system-base buffer-file-coding-system)
		    (coding-system-base 'no-conversion))))
      (let ((buffer-read-only nil)
	    (omodified (buffer-modified-p)))
	(unwind-protect
	    (progn
	      (encode-coding-region (point-min) (point-max)
				    buffer-file-coding-system)
	      (set-buffer-file-coding-system 'raw-text nil)
	      (decode-coding-region (point-min) (point-max)
				    buffer-file-coding-system))
	  (set-buffer-modified-p omodified)))))

(defun vm-gobble-headers ()
  "Process all the VM-specific headers in the current folder."
  (vm-gobble-visible-header-variables)
  (vm-gobble-bookmark)
  (vm-gobble-pop-retrieved)
  (vm-gobble-imap-retrieved)
  (vm-gobble-imap-to-expunge)
  (vm-gobble-summary)
  (vm-gobble-labels))

;;;###autoload
(cl-defun vm-other-frame (&optional folder read-only 
				  &key interactive)
  "Like vm, but run in a newly created frame."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  (if (vm-multiple-frames-possible-p)
      (if folder
	  (vm-goto-new-frame 'folder)
	(vm-goto-new-frame 'primary-folder 'folder)))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder :interactive interactive :read-only read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(cl-defun vm-other-window (&optional folder read-only
				   &key interactive)
  "Like vm, but run in a different window."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder :interactive interactive :read-only read-only)))

(put 'vm-mode 'mode-class 'special)

;;;###autoload
(defun vm-mode (&optional read-only)
  "Major mode for reading mail.

This is VM.

Use M-x vm-submit-bug-report to submit a bug report.

Commands:
\\{vm-mode-map}

Customize VM by setting variables and store them in the `vm-init-file'."
  (interactive "P")
  (vm (current-buffer) :read-only read-only)
  (vm-display nil nil '(vm-mode) '(vm-mode)))

;;;###autoload
(cl-defun vm-visit-folder (folder &optional read-only 
				&key interactive just-visit)
  "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

The optional third arg JUST-VISIT (not available interactively)
says that the folder should be visited with as little initial
processing as possible.  No summary generation, no moving of the
message-pointer, no retrieval of new mail."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (when (vm-interactive-p) (setq interactive t))
  (let ((access-method nil) foo)
    (cond ((and (vm-pop-folder-spec-p folder)
		(setq foo (vm-pop-find-name-for-spec folder)))
	   (setq folder foo
		 access-method 'pop
		 vm-last-visit-pop-folder folder))
	  ((and (vm-imap-folder-spec-p folder)
)
	   (setq ;; folder foo
	         access-method 'imap
		 vm-last-visit-imap-folder folder))
	  (t
	   (let ((default-directory 
		   (or vm-folder-directory default-directory)))
	     (setq folder (expand-file-name folder)
		   vm-last-visit-folder folder))))
    (vm folder 
	:interactive interactive
	:read-only read-only :access-method access-method 
	:just-visit just-visit)))

;;;###autoload
(cl-defun vm-visit-folder-other-frame (folder &optional read-only
					    &key interactive)
  "Like vm-visit-folder, but run in a newly created frame."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other frame:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only :interactive interactive))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(cl-defun vm-visit-folder-other-window (folder &optional read-only
					     &key interactive)
  "Like vm-visit-folder, but run in a different window."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other window:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only :interactive interactive)))

;;;###autoload
(cl-defun vm-visit-thunderbird-folder (folder &optional read-only
					    &key interactive)
  "Visit a mail file maintained by Thunderbird.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

This function differs from `vm-visit-folder' in that it remembers that
the folder is a foreign folder maintained by Thunderbird.  Saving
of messages is carried out preferentially to other Thunderbird folders."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory 
	     (if vm-thunderbird-folder-directory
		 (expand-file-name vm-thunderbird-folder-directory)
	       default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (when (vm-interactive-p) (setq interactive t))
  (let ((default-directory 
	  (or vm-thunderbird-folder-directory default-directory)))
    (setq folder (expand-file-name folder)
	  vm-last-visit-folder folder))
  (vm folder :interactive interactive :read-only read-only)
  (set (make-local-variable 'vm-foreign-folder-directory)
       vm-thunderbird-folder-directory)
  )

;;;###autoload
(cl-defun vm-visit-pop-folder (folder &optional read-only
				    &key interactive)
  "Visit a POP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the POP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the POP mailbox.

First arg FOLDER specifies the name of the POP mailbox to visit.
You can only visit mailboxes that are specified in `vm-pop-folder-alist'.
When this command is called interactively the mailbox name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (let (remote-spec)
    (vm-session-initialization)
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer-if-possible)
    (vm-check-for-killed-summary)
    (when (vm-interactive-p) (setq interactive t))
    (if (and (equal folder "") (stringp vm-last-visit-pop-folder))
	(setq folder vm-last-visit-pop-folder))
    (setq vm-last-visit-pop-folder folder)
    (setq remote-spec (vm-pop-find-spec-for-name folder))
    (if (null remote-spec)
	(error "No such POP folder: %s" folder))
    (vm remote-spec :access-method 'pop
	:interactive interactive :read-only read-only )))

;;;###autoload
(cl-defun vm-visit-pop-folder-other-frame (folder &optional read-only
						&key interactive)
  "Like vm-visit-pop-folder, but run in a newly created frame."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only :interactive interactive))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(cl-defun vm-visit-pop-folder-other-window (folder &optional read-only
						 &key interactive)
  "Like vm-visit-pop-folder, but run in a different window."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only :interactive interactive)))

;;;###autoload
(cl-defun vm-visit-imap-folder (folder &optional read-only
				     &key interactive)
  "Visit a IMAP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the IMAP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the IMAP mailbox when the folder is
saved. 

When this command is called interactively, the FOLDER name will
be read from the minibuffer in the format
\"account-name:folder-name\", where account-name is the short
name of an IMAP account listed in `vm-imap-account-alist' and
folder-name is a folder in this account.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (if (null vm-imap-account-alist)
	   (setq vm-imap-account-alist 
		 (mapcar 
		  'reverse
		  (with-no-warnings
		    (vm-imap-spec-list-to-host-alist vm-imap-server-list)))))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      t nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  ;; Before anything else does: a folder that is not a maildrop is read as
  ;; one further down, and `vm-imap-normalize-spec' fails on it with
  ;; "Wrong type argument: consp, nil" and no hint of the cause.  Nil is what
  ;; `vm-imap-spec-for-account' answers for an account VM has not got, which
  ;; is what a command naming an account hands over (emacs-vm/vm#826).
  (unless (and (stringp folder) (vm-imap-folder-spec-p folder))
    (error (concat "%s is not an IMAP maildrop."
		   "  A command that names an account gets nil from"
		   " vm-imap-spec-for-account until vm-imap-account-alist"
		   " has that account in it; M-x vm-check-configuration"
		   " lists the accounts VM knows")
	   (if folder (format "%S" folder) "Nothing")))
  (setq vm-last-visit-imap-folder folder)
  (vm folder :access-method 'imap
      :interactive interactive :read-only read-only))

;;;###autoload
(cl-defun vm-visit-imap-folder-other-frame (folder &optional read-only
						 &key interactive)
  "Like vm-visit-imap-folder, but run in a newly created frame."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      nil nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only :interactive interactive))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(cl-defun vm-visit-imap-folder-other-window (folder &optional read-only
						  &key interactive)
  "Like vm-visit-imap-folder, but run in a different window."
  (interactive
   (save-current-buffer
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      nil nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (when (vm-interactive-p) (setq interactive t))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only :interactive interactive)))


;;;###autoload
(defun vm-folder-buffers (&optional non-virtual)
  "Return the list of buffer names that are currently visiting VM
folders.  The optional argument NON-VIRTUAL says that only 
non-virtual folders should be returned."
  (save-excursion
    (let ((buffers (buffer-list))
          (modes (if non-virtual '(vm-mode) '(vm-mode vm-virtual-mode)))
          folders)
      (while buffers
        (set-buffer (car buffers))
        (if (member major-mode modes)
            (setq folders (cons (buffer-name) folders)))
        (setq buffers (cdr buffers)))
      folders)))
(defalias 'vm-folder-list 'vm-folder-buffers)

;; The following function is from vm-rfaddons.el.       USR, 2011-02-28
;;;###autoload
(defun vm-switch-to-folder (folder-name)
  "Switch to another opened VM folder and rearrange windows as with a scroll."
  (interactive (list
                (let* ((folder-buffers (vm-folder-buffers))
		       (current-folder 
			(save-excursion
			  (vm-select-folder-buffer)
			  (buffer-name)))
		       (history vm-switch-to-folder-history) 
		       pos default)
                  (if (member major-mode
                              '(vm-mode vm-presentation-mode vm-summary-mode))
                      (setq folder-buffers 
			    (delete current-folder folder-buffers)))
		  (if (setq pos (vm-find 
				 history 
				 (lambda (f) (member f folder-buffers))))
		      (setq default (nth pos history))
		    (setq default (car folder-buffers)))
                  (completing-read
		   ;; prompt
                   (format "Foldername%s: " 
			   (if default (format " (%s)" default) ""))
		   ;; collection
                   (mapcar (lambda (b) (list b)) (vm-folder-buffers))
		   ;; predicate, require-match, initial-input
                   nil t nil
		   ;; hist
                   'vm-switch-to-folder-history
		   ;; default
                   default))))

  (switch-to-buffer folder-name)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-summarize)
  (let ((this-command 'vm-scroll-backward))
    (vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
                (list this-command 'reading-message))
    (vm-update-summary-and-mode-line)))

;;;###autoload
(defun vm-get-folder-buffer (folder)
  "Returns the buffer visiting FOLDER if it exists, nil otherwise."
  (let ((buffers (vm-folder-buffers))
	pos)
    (setq pos 
	  (vm-find buffers
		   (lambda (b) 
		     (with-current-buffer b
		       (equal folder (vm-folder-name))))))
    (and pos (get-buffer (nth pos buffers)))))


(put 'vm-virtual-mode 'mode-class 'special)

(defun vm-virtual-mode (&rest _ignored)
  "Mode for reading multiple mail folders as one folder.

The commands available are the same commands that are found in
vm-mode, except that a few of them are not applicable to virtual
folders.

vm-virtual-mode is not a normal major mode.  If you run it, it
will not do anything.  The entry point to vm-virtual-mode is
`vm-visit-virtual-folder'.")

(defvar scroll-in-place)

;;;###autoload
(defun vm-visit-virtual-folder (folder-name 
				&optional read-only bookmark
				summary-format directory)
  "Visit the virtual folder FOLDER-NAME.  With a prefix argument,
visit it in read-only mode.

When called in Lisp code, additional optional arguments BOOKMARK and
SUMMARY-FORMAT specify the message where the pointer should be and the
summary format to use.  DIRECTORY is the default directory for the
virtual folder buffer."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (require 'vm-virtual)
  (unless (assoc folder-name vm-virtual-folder-alist)
    (error "No such virtual folder, %s" folder-name))
  (let ((buffer-name (concat "(" folder-name ")"))
	first-time blurb)
    (set-buffer (get-buffer-create buffer-name))
    (setq default-directory (or directory vm-virtual-default-directory 
				default-directory))
    (setq first-time (not (eq major-mode 'vm-virtual-mode)))
    (when first-time
      (buffer-disable-undo (current-buffer))
      (abbrev-mode 0)
      (auto-fill-mode 0)
      (vm-fsfemacs-nonmule-display-8bit-chars)
      (setq mode-name "VM Virtual"
	    mode-line-format vm-mode-line-format
	    buffer-read-only t
	    vm-folder-read-only read-only
	    vm-summary-format (or summary-format vm-summary-format)
	    vm-label-obarray (make-vector 29 0)
	    vm-virtual-folder-definition
	    (assoc folder-name vm-virtual-folder-alist))
      ;; scroll in place messes with scroll-up and this loses
      (make-local-variable 'scroll-in-place)
      (setq scroll-in-place nil)
      ;; Visit all the component folders and build message list
      (vm-build-virtual-message-list nil)
      (use-local-map vm-mode-map)
      (when (vm-menu-support-possible-p)
	(vm-menu-install-menus))
      (add-hook 'kill-buffer-hook 'vm-garbage-collect-folder)
      (add-hook 'kill-buffer-hook 'vm-garbage-collect-message)
      ;; save this for last in case the user interrupts.
      ;; an interrupt anywhere before this point will cause
      ;; everything to be redone next revisit.
      (setq major-mode 'vm-virtual-mode)
      (run-hooks 'vm-virtual-mode-hook)
      ;; must come after the setting of major-mode
      (setq mode-popup-menu (and vm-use-menus
				 (vm-menu-support-possible-p)
				 (vm-menu-mode-menu)))
      (setq blurb (vm-emit-totals-blurb))
      (when vm-summary-show-threads
	(vm-sort-messages "activity"))
      (if bookmark
	  (let ((mp vm-message-list))
	    (while mp
	      (if (eq bookmark (vm-real-message-of (car mp)))
		  (progn
		    (vm-record-and-change-message-pointer
		     vm-message-pointer mp :present t)
		    (setq mp nil))
		(setq mp (cdr mp))))))
      (unless vm-message-pointer
	(if (vm-thoughtfully-select-message)
	    (vm-present-current-message)
	  (vm-update-summary-and-mode-line)))
      (vm-inform 5 "%s" blurb))
    ;; make a new frame if the user wants one.  reuse an
    ;; existing frame that is showing this folder.
    (vm-goto-new-folder-frame-maybe 'folder)
    (if vm-raise-frame-at-startup
	(vm-raise-frame))
    (vm-display nil nil (list this-command) (list this-command 'startup))
    (vm-toolbar-install-or-uninstall-toolbar)
    (when first-time
      (when (vm-should-generate-summary)
	(vm-summarize t nil)
	(vm-inform 5 "%s" blurb))
      ;; raise the summary frame if the user wants frames
      ;; raised and if there is a summary frame.
      (when (and vm-summary-buffer
		 vm-mutable-frame-configuration
		 vm-frame-per-summary
		 vm-raise-frame-at-startup)
	(vm-raise-frame))
      ;; if vm-mutable-window-configuration is nil, the startup
      ;; configuration can't be applied, so do
      ;; something to get a VM buffer on the screen
      (if vm-mutable-window-configuration
	  (vm-display nil nil (list this-command)
		      (list (or this-command 'vm) 'startup))
	(save-excursion
	  (switch-to-buffer (or vm-summary-buffer
				vm-presentation-buffer
				(current-buffer))))))

    ;; check interactive-p so as not to bog the user down if they
    ;; run this function from within another function.
    (when (and (vm-interactive-p)
	       (not vm-startup-message-displayed))
      (vm-display-startup-message)
      (vm-inform 5 "%s" blurb))))

;;;###autoload
(defun vm-visit-virtual-folder-other-frame 
  	(folder-name &optional read-only bookmark summary-format directory)
  "Like `vm-visit-virtual-folder', but run in a newly created frame."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other frame: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only bookmark
			     summary-format directory))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-virtual-folder-other-window 
  		(folder-name &optional read-only bookmark
			     summary-format directory)
  "Like `vm-visit-virtual-folder', but run in a different window."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other window: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only bookmark
			     summary-format directory)))

;;;###autoload
(defun vm-mail (&optional to subject)
  "Send a mail message from within VM, or from without.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (vm-mail-internal :to to :subject subject)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook))

;;;###autoload
(defun vm-mail-other-frame (&optional to subject)
  "Like vm-mail, but run in a newly created frame.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (when (null to)
    (setq to (vm-select-recipient-from-sender-if-possible)))
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to subject))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-mail-other-window (&optional to subject)
  "Like vm-mail, but run in a different window.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (when (null to)
    (setq to (vm-select-recipient-from-sender-if-possible)))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to subject)))

;;;###autoload
(defun vm-mail-from-folder (&optional subject)
  "Compose a new mail message using the current folder as its
parent folder and current message as its parent message.  If the
variable `vm-mail-use-sender-address' is `t', then the sender of the
current message is selected as the recipient of the new composition."
  ;; FIXME We also need variants of this for other-frame and
  ;; other-window.                                USR, 2012-01-19
  
  (interactive)
  (vm-session-initialization)
  ;; No message needed: this composes a new message, and the current message is
  ;; only wanted as a parent if there is one.  Demanding one meant that `m' in
  ;; an empty folder answered "Folder is empty" and composed nothing, which is
  ;; issue #514 -- an IMAP inbox with no mail in it is the ordinary way to meet
  ;; that.
  (vm-select-folder-buffer-and-validate 0)
  (let* ((guess (vm-select-recipient-from-sender-if-possible)))
    (vm-mail-internal :to nil :guessed-to guess :subject subject)
    (run-hooks 'vm-mail-hook)
    (run-hooks 'vm-mail-mode-hook)))

(defvar mail-reply-action)
(defvar mail-send-actions)
(defvar mail-return-action)

;;;###autoload
(defun vm-compose-mail (&optional to subject other-headers continue
		        switch-function yank-action
			send-actions return-action &rest _ignored)
  (interactive)
  (vm-session-initialization)
  (require 'vm-reply)
  (if continue
      (vm-continue-composing-message)
    (let ((_buffer (vm-mail-internal
		    :buffer-name (if to
				     (format "message to %s"
					     (vm-truncate-roman-string to 20))
				   nil)
		    :to to :subject subject)))
      (goto-char (point-min))
      (re-search-forward (concat "^" mail-header-separator "$"))
      (beginning-of-line)
      (while other-headers
	(insert (car (car other-headers)))
	(while (eq (char-syntax (char-before (point))) ?\ )
	  (delete-char -1))
	(while (eq (char-before (point)) ?:)
	  (delete-char -1))
	(insert ": " (cdr (car other-headers)))
	(if (not (eq (char-before (point)) ?\n))
	    (insert "\n"))
	(setq other-headers (cdr other-headers)))
      (cond ((null to)
	     (mail-position-on-field "To"))
	    ((null subject)
	     (mail-position-on-field "Subject"))
	    (t
	     (mail-text)))
      (funcall (or switch-function (function switch-to-buffer))
	       (current-buffer))
      (if yank-action
	  (save-excursion
	    (mail-text)
	    (apply (car yank-action) (cdr yank-action))
	    (push-mark (point))
	    (mail-text)
	    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
		  ;; `mail-yank-hooks' is not consulted: it was obsoleted by
		  ;; `mail-citation-hook' above.
		  (t (vm-mail-yank-default)))))
      (make-local-variable 'mail-send-actions)
      (setq mail-send-actions send-actions)
      (make-local-variable 'mail-return-action)
      (setq mail-return-action return-action))))

;; Dynamically bound variables for sanitized data in the bug report

(defvar vm-bug-imap-auto-expunge-alist nil)
(defvar vm-bug-pop-auto-expunge-alist nil)
(defvar vm-bug-imap-account-alist nil)
(defvar vm-bug-pop-folder-alist nil)
(defvar vm-bug-primary-inbox nil)
(defvar vm-bug-spool-files nil)

;;;###autoload
(defun vm-submit-bug-report (&optional pre-hooks post-hooks)
  "Submit a bug report, with pertinent information to the VM bug list."
  (interactive)
  (require 'reporter)
  (vm-session-initialization)
  (require 'vm-reply)
  ;; Use VM to send the bug report.  Could be trouble if vm-mail
  ;; is what the user wants to complain about.  But most of the
  ;; time we'll be fine and users like to use MIME to attach
  ;; stuff to the reports.
  (let ((_reporter-mailer '(vm-mail))	; this is probably not needed
	(mail-user-agent 'vm-user-agent)
        (varlist nil)
	(errors 0))
    (setq varlist (apropos-internal "^\\(vm\\|vmpc\\)-" 'custom-variable-p))
    (setq varlist (vm-delete
		   (lambda (v)
		     ;; FIXME: `standard-value' stores an *expression*
                     ;; whose value is the default value!
		     (equal (symbol-value v) (car (get v 'standard-value))))
		   varlist))
    (setq varlist (sort varlist
                        (lambda (v1 v2)
                          (string-lessp (format "%s" v1) (format "%s" v2)))))
    (when (and (eq vm-mime-text/html-handler 'emacs-w3m)
	       (boundp 'emacs-w3m-version))
      (nconc varlist (list 'emacs-w3m-version 'w3m-version 
			   'w3m-goto-article-function)))
    (let ((fill-column (1- (window-width)))	; turn off auto-fill
	  (mail-user-agent (default-value 'mail-user-agent)) ; use the default
					; mail-user-agent for bug reports
	  (vars-to-delete 
	   '(vm-auto-folder-alist	; a bit private
	     vm-mail-folder-alist	; ditto
	     vm-virtual-folder-alist	; ditto
	     ;; vm-mail-fcc-default - is this private?
	     vm-pcrisis-actions vm-pcrisis-conditions 
	     vmpc-actions-alist vmpc-reply-alist vmpc-forward-alist
	     vmpc-resend-alist vmpc-newmail-alist vmpc-automorph-alist
	     ;; email addresses
	     vm-mail-header-from
	     vm-mail-return-receipt-to
	     vm-summary-uninteresting-senders
	     ;; obsolete-variables
	     vm-imap-server-list
	     ;; sanitized versions included later
	     vm-spool-files
             vm-primary-inbox
	     vm-pop-folder-alist
	     vm-imap-account-alist
	     vm-pop-auto-expunge-alist
	     vm-imap-auto-expunge-alist
	     ))
	  ;; delete any passwords stored in maildrop strings
	  (vm-bug-spool-files
	   (condition-case nil
	       (if (listp (car vm-spool-files))
		   (vm-mapcar 
		    (lambda (elem-xyz)
		      (vm-mapcar (function vm-maildrop-sans-personal-info)
				 elem-xyz))
		    vm-spool-files)
		 (vm-mapcar (function vm-maildrop-sans-personal-info)
			    vm-spool-files))
	     (error (vm-increment errors) vm-spool-files)))
	  (vm-bug-primary-inbox
	   (condition-case nil
	       (vm-maildrop-sans-personal-info
		vm-primary-inbox)
	     (error (vm-increment errors) vm-primary-inbox)))
	  (vm-bug-pop-folder-alist
	   (condition-case nil
	       (vm-maildrop-alist-sans-personal-info
		vm-pop-folder-alist)
	     (error (vm-increment errors) vm-pop-folder-alist)))
	  (vm-bug-imap-account-alist
	   (condition-case nil
	       (vm-maildrop-alist-sans-personal-info
		vm-imap-account-alist)
	     (error (vm-increment errors) vm-imap-account-alist)))
	  (vm-bug-pop-auto-expunge-alist
	   (condition-case nil
	       (vm-maildrop-alist-sans-personal-info
		vm-pop-auto-expunge-alist)
	     (error (vm-increment errors) vm-pop-auto-expunge-alist)))
	  (vm-bug-imap-auto-expunge-alist
	   (condition-case nil
	       (vm-maildrop-alist-sans-personal-info
		vm-imap-auto-expunge-alist)
	     (error (vm-increment errors) vm-imap-auto-expunge-alist))))
      (while vars-to-delete
        (setq varlist (delete (car vars-to-delete) varlist)
              vars-to-delete (cdr vars-to-delete)))
      ;; see what the user had loaded
      (setq varlist
	    (append (list 'features)
		    (list 'vm-bug-spool-files
                          'vm-bug-primary-inbox
			  'vm-bug-pop-folder-alist
			  'vm-bug-imap-account-alist
			  'vm-bug-pop-auto-expunge-alist
			  'vm-bug-imap-auto-expunge-alist)
		    varlist))
      (delete-other-windows)
      (reporter-submit-bug-report
       vm-maintainer-address		; address
       (concat "VM " (vm-version)	; pkgname
               " commit: " (vm-version-commit))
       varlist				; varlist
       pre-hooks			; pre-hooks
       post-hooks			; post-hooks
       (concat				; salutation
	"INSTRUCTIONS:

- The preferred way to report a bug is to open an issue at:

   https://gitlab.com/emacs-vm/vm/-/issues

  Paste the configuration below into the issue.  If you would rather not
  make your configuration public, send this mail instead: it goes only to
  the VM maintainers.

- You are using Emacs default messaging here.  *** NOT vm-mail-mode ***

- Please change the Subject header to a concise bug description.

- In this report, remember to cover the basics, that is, what you
  expected to happen and what in fact did happen and how to reproduce it.

- You may attach sample messages or attachments that can be used to
  reproduce the problem.

- Mail sent to viewmail-bugs@nongnu.org is only viewed by VM
  maintainers and it is not made public.  

- You may remove these instructions and other stuff which is unrelated
  to the bug from your message.
"
	(if (> errors 0)
	    "
- The below defintions should be scrubbed of sensitive information.
  However, please verify this is the case."
          )))
      (goto-char (point-min))
      (mail-position-on-field "Subject"))))

;;;###autoload
(defun vm-edit-init-file ()
  "Edit the `vm-init-file'."
  (interactive)
  (find-file-other-frame vm-init-file))

;;;###autoload
(defun vm-toggle-thread-operations ()
  "Toggle the variable `vm-enable-thread-operations'.

If enabled, VM operations on root messages of collapsed threads
will apply to all the messages in the threads.  If disabled, VM
operations only apply to individual messages.

\"Operations\" in this context include deleting, saving, setting
attributes, adding/deleting labels etc."
  (interactive)
  (setq vm-enable-thread-operations (not vm-enable-thread-operations))
  (if vm-enable-thread-operations
      (vm-inform 5 "Thread operations enabled")
    (vm-inform 5 "Thread operations disabled")))

(defvar vm-postponed-folder)

(defvar vm-drafts-exist nil)

(defvar vm-ml-draft-count ""
  "The current number of drafts in the `vm-postponed-folder'.")

(defvar vm-postponed-folder)

;;;###autoload
(defun vm-update-draft-count ()
  "Check number of postponed messages in folder `vm-postponed-folder'."
  (let ((f (expand-file-name vm-postponed-folder vm-folder-directory)))
    (if (or (not (file-exists-p f)) (= (nth 7 (file-attributes f)) 0))
        (setq vm-drafts-exist nil)
      (let ((mtime (nth 5 (file-attributes f))))
        (when (not (equal vm-drafts-exist mtime))
          (setq vm-drafts-exist mtime)
          (setq vm-ml-draft-count (format "%d postponed"
                                          (vm-count-messages-in-file f))))))))

;;;###autoload
(defvar vm-session-initializing nil
  "Whether `vm-session-initialization' is running.

Its own guard, `vm-session-beginning', is cleared at the end of the work
rather than the start, so anything reached from the middle of it -- the init
file it loads, and whatever that calls -- would start the work again.  A
lookup that initialises VM so as to answer at all
(`vm-imap-spec-for-account') is exactly such a caller.")

;;;###autoload
(defun vm-session-initialization ()
  "If this is the first time VM has been run in this Emacs session,
do some necessary preparations.  Otherwise, update the count of
draft messages.

Autoloaded, so that configuration code and the autoloaded functions that ask
for initialisation before they can answer -- `vm-imap-spec-for-account' --
reach it without vm.el having been loaded by something else first."
  (if (and (or (not (boundp 'vm-session-beginning))
	       vm-session-beginning)
	   (not vm-session-initializing))
      (let ((vm-session-initializing t))
        (require 'vm-macro)
        (require 'vm-vars)
        (require 'vm-misc)
        (require 'vm-message)
        (require 'vm-minibuf)
        (require 'vm-motion)
        (require 'vm-page)
        (require 'vm-mouse)
        (require 'vm-summary)
	(require 'vm-summary-faces)
        (require 'vm-undo)
        (require 'vm-mime)
        (require 'vm-folder)
        (require 'vm-toolbar)
        (require 'vm-window)
        (require 'vm-menu)
        ;; Before anything reads a folder: a stale .elc fails deep inside
        ;; the parser with an error that says nothing about the cause (#791).
        (vm-warn-about-stale-compiled-files)
        (add-hook 'kill-emacs-hook 'vm-garbage-collect-global)
        ;; A folder cache is VM's own file under a hashed name, so leaving it
        ;; modified asks the reader about a path that means nothing to them
        ;; (#798).  Their own folders are left for Emacs to ask about.
        (add-hook 'kill-emacs-hook 'vm-save-folder-caches)
        ;; Offers unfinished compositions to the postponed folder as Emacs
        ;; is left, rather than leaving them to be written to files one at a
        ;; time.  Registered here rather than as vm-postpone.el loads:
        ;; loading a file should not change how Emacs behaves (#160).
        (add-hook 'kill-emacs-query-functions
                  'vm-postpone-unfinished-compositions)
	(vm-load-init-file)
	;; After the init file, because that is where it is set: a
	;; configuration written before mboxcl2 was renamed still says
	;; `From_-with-Content-Length', and every test of a folder type is
	;; against the new name.
	(setq vm-default-folder-type
	      (vm-canonical-folder-type vm-default-folder-type))
	(vm-warn-about-deprecated-trust-setting)
	(vm-check-folder-type-extensions)
	(vm-check-default-folder-type)
	(if (not vm-window-configuration-file)
	    (setq vm-window-configurations vm-default-window-configuration)
	  (or (vm-load-window-configurations vm-window-configuration-file)
	      (setq vm-window-configurations vm-default-window-configuration)))
	(setq vm-buffers-needing-display-update (make-vector 29 0))
	(setq vm-buffers-needing-undo-boundaries (make-vector 29 0))
	(add-hook 'post-command-hook 'vm-add-undo-boundaries)
	(if (facep 'vm-monochrome-image)
	    nil
	  (make-face 'vm-monochrome-image)
	  (set-face-background 'vm-monochrome-image "white")
	  (set-face-foreground 'vm-monochrome-image "black"))
	(and (vm-mouse-support-possible-p)
	     (vm-mouse-install-mouse))
	(and (vm-menu-support-possible-p)
	     vm-use-menus
	     (vm-menu-initialize-vm-mode-menu-map))
	(setq vm-session-beginning nil)
	;; Last, so that a hook function sees VM assembled: the init file read,
	;; menus and the mouse installed, timers running.  Anything it changes
	;; therefore wins over VM's own setup, which is the point of being able
	;; to run code here at all.  `vm-session-beginning' is already nil, so a
	;; hook function may call VM commands without starting this again.
	;; Issue #565.
	(run-hooks 'vm-startup-hook)
	;; After the hook, so that a configuration finished there is seen as
	;; finished.
	(vm-suggest-checking-configuration-maybe)))
  ;; check for postponed messages
  (vm-update-draft-count))

;;;###autoload
(if (fboundp 'define-mail-user-agent)
    ;; The two functions are named, not referenced: this form is copied into
    ;; vm-autoloads.el, where nothing defines them yet, and a #' reference
    ;; there is a compiler warning about the generated file.  Emacs' own
    ;; define-mail-user-agent calls quote them plainly for the same reason.
    (define-mail-user-agent 'vm-user-agent
      'vm-compose-mail			; compose function
      'vm-mail-send-and-exit		; send function
      nil				; abort function (kill-buffer)
      nil)				; hook variable (mail-send-hook)
)

(autoload 'reporter-submit-bug-report "reporter")
(autoload 'timezone-make-date-sortable "timezone")
(autoload 'rfc822-addresses "rfc822")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-fetch-field "mail-utils")
(autoload 'mail-position-on-field "mail-utils")
(autoload 'mail-send "sendmail")
(autoload 'mail-mode "sendmail")
(autoload 'mail-extract-address-components "mail-extr")
(autoload 'set-tapestry "tapestry")
(autoload 'tapestry "tapestry")
(autoload 'tapestry-replace-tapestry-element "tapestry")
(autoload 'tapestry-nullify-tapestry-elements "tapestry")
(autoload 'tapestry-remove-frame-parameters "tapestry")

(defun vm-get-package-version ()
  "Return version of VM if it was installed as a package"
  ;; N.B. this must be in this file, as package-get-version wants
  ;; to be called from the file containg the `Version:' header.
  (package-get-version))

(defun vm--version-info-from-conf ()
  "Return version and commit from vm-version-conf.el if it exists."
  (when (ignore-errors (load "vm-version-conf"))
    (list vm-version-config vm-version-commit-config)))

(defun vm--commit-from-package (pkg)
  "Get commit hash from PKG, whether VC-installed or archive-installed."
  (let ((desc (package-get-descriptor pkg)))
    (or (alist-get :commit (package-desc-extras desc))
        "unknown-commit")))

(defun vm--version-info-from-package ()
  "Return version and commit if VM is loaded from a package."
  (let ((package-version (vm-get-package-version)))
    (if package-version
        (list package-version (vm--commit-from-package 'vm))
      (list nil nil))))

;; Define vm-version and vm-version-commit
(let ((version-info (or (vm--version-info-from-conf)
                        (vm--version-info-from-package)
                        (list nil nil))))
  (setq vm-version (nth 0 version-info)
        vm-version-commit (nth 1 version-info))
  (unless vm-version
    (warn "Can't obtain vm-version from package or vm-version-conf.el"))
  (unless vm-version-commit
    (warn "Can't obtain vm-version-commit from package or vm-version-conf.el")))

;;;###autoload
(defun vm-version ()
  "Display and return the value of the variable `vm-version'."
  (interactive)
  (when (vm-interactive-p)
    (if vm-version
        (message "VM version is: %s" vm-version)
      (message "VM version was not discovered when VM was loaded")))
  (or vm-version "unknown"))

;;;###autoload
(defun vm-emacs-name-and-version ()
  "Return the editor's name and version, as in \"GNU Emacs 30.2\".

The variable `emacs-version' has held only the number for years now, so an
X-Mailer built from it says which version sent the mail but not which editor
\(issue #520).  The function `emacs-version' does say, but follows it with the
build number, the platform and the build date, which is far more than an
X-Mailer wants -- so take the name and the version off the front of it.  For
XEmacs that yields \"XEmacs 21.4\" by the same rule.

Falls back to naming the editor from `featurep' if that string is not in the
form expected, and to \"Unknown Emacs\" if there is nothing to go on."
  (let ((full (if (fboundp 'emacs-version) (emacs-version) "")))
    (cond ((string-match "\\`\\([^0-9\n]*[A-Za-z]\\)[ \t]+\\([0-9][^ \t\n(]*\\)"
			 full)
	   (concat (match-string 1 full) " " (match-string 2 full)))
	  ((boundp 'emacs-version)
	   (concat "GNU Emacs " emacs-version))
	  (t "Unknown Emacs"))))

;;;###autoload
(defun vm-version-commit ()
  "Display and the value of the variable `vm-version-commit'."
  (interactive)
  (when (vm-interactive-p)
    (if vm-version-commit
        (message "VM commit is: %s" vm-version-commit)
      (message "VM commit was not discovered when VM was loaded")))
   (or vm-version-commit "unknown"))

;;; Checking a configuration (emacs-vm/vm#816)

(defun vm-configuration-problem-mail-agent ()
  "A problem with what Emacs uses to send mail, or nil."
  (unless (eq mail-user-agent 'vm-user-agent)
    (format (concat "`mail-user-agent' is %s, so C-x m and anything else that"
                    " composes mail through Emacs will not use VM."
                    "  Set it to `vm-user-agent'.  See Mail agent in the VM"
                    " manual")
            mail-user-agent)))

(defun vm-address-looks-machine-made-p (address)
  "Whether ADDRESS is one Emacs invented from the host name.
Emacs sets `user-mail-address' to the login name at `mail-host-address' or
the system name where nothing else says, and that is rarely an address
anyone can reply to."
  (and (stringp address)
       (or (string-suffix-p (concat "@" (system-name)) address)
           (and (stringp mail-host-address)
                (string-suffix-p (concat "@" mail-host-address) address))
           ;; no dot in the domain: not a name the DNS can resolve
           (not (string-match-p "@[^@]+\\.[^@.]+\\'" address)))))

(defun vm-configuration-problem-from-address ()
  "A problem with the address mail will be sent from, or nil."
  (cond ((not (stringp user-mail-address))
         (concat "`user-mail-address' is not set, so VM does not know what"
                 " address to send from.  Set it to your own address."
                 "  See Composing setup in the VM manual"))
        ((vm-address-looks-machine-made-p user-mail-address)
         (format (concat "`user-mail-address' is %S, which Emacs made up from"
                         " this machine's name rather than being told."
                         "  Mail will go out from an address nobody can reply"
                         " to.  Set it to your own address.  See Composing"
                         " setup in the VM manual")
                 user-mail-address))))

(defun vm-configuration-problem-from-header ()
  "A problem with the `From' header VM will write, or nil.
`vm-mail-header-from' goes into a composition verbatim, after \"From: \", so a
value that is not an address is a header nobody can reply to and nothing
says so.  A reader who meant to add a header to every composition and
reached for this variable gets \"From: IMAP-FCC: Sent\" in the message and in
every copy filed of it (emacs-vm/vm#832)."
  (when (stringp vm-mail-header-from)
    (cond ((string-match-p "\\`[A-Za-z][A-Za-z0-9-]*:" vm-mail-header-from)
           (format (concat "`vm-mail-header-from' is %S, which reads as a"
                           " header line rather than an address, so VM writes"
                           " `From: %s'.  To add a header to every composition"
                           " set `mail-default-headers' instead, and set this"
                           " to your own address or to nil")
                   vm-mail-header-from vm-mail-header-from))
          ((not (string-match-p "@" vm-mail-header-from))
           (format (concat "`vm-mail-header-from' is %S, which names no"
                           " domain, so a reply has nowhere to go.  Set it to"
                           " your own address or to nil")
                   vm-mail-header-from)))))

(defun vm-configuration-problem-sending ()
  "A problem with how mail will be sent, or nil."
  (cond ((eq send-mail-function 'sendmail-query-once)
         (concat "`send-mail-function' has not been set, so Emacs will ask how"
                 " to send the first message and remember the answer."
                 "  Set it yourself instead: `smtpmail-send-it' with"
                 " `smtpmail-smtp-server' is what most people want.  See"
                 " Sending setup in the VM manual"))
        ((null send-mail-function)
         (concat "`send-mail-function' is nil, so sending a message will fail."
                 "  Set it to `smtpmail-send-it'.  See Sending setup in the VM"
                 " manual"))))

(defun vm-configuration-problem-folder-directory ()
  "A problem with where folders are kept, or nil."
  (cond ((null vm-folder-directory)
         (concat "`vm-folder-directory' is not set, so saving a message offers"
                 " whatever directory happens to be current and folders end up"
                 " scattered.  Set it to the one directory your folders live"
                 " in.  See Local mail in the VM manual"))
        ((not (file-directory-p (expand-file-name vm-folder-directory)))
         (format (concat "`vm-folder-directory' is %S, which is not a"
                         " directory.  Create it, or set the variable to the"
                         " directory your folders are in.  See Local mail in"
                         " the VM manual")
                 vm-folder-directory))))

(defun vm-configuration-problem-mail-source ()
  "A problem with where new mail comes from, or nil."
  (unless (or (vm-spool-files) vm-imap-account-alist vm-pop-folder-alist)
    (concat "Nothing says where your mail comes from, so getting new mail"
            " will find none.  Set `vm-spool-files' for a local spool file or"
            " maildir, and see Local mail in the VM manual; for a mailbox on a"
            " server set `vm-imap-account-alist', and see Server mail in the"
            " VM manual")))

(defconst vm-maildrop-types
  '(("imap" . 7) ("imap-ssl" . 7) ("imap-ssh" . 7)
    ("pop" . 6) ("pop-ssl" . 6) ("pop-ssh" . 6))
  "Each maildrop type VM understands, and how many colon-separated fields it takes.

The parsers accept any number of fields and any leading word, so a misspelt
type or a missing field is not reported where it is written: the failure
comes later, from the session, saying something about the server instead.")

(defun vm-maildrop-problem (spec where)
  "A problem with the maildrop SPEC, which was found in WHERE, or nil."
  (let* ((fields (vm-parse spec "\\([^:]+\\):?" 1 8))
         (type (car fields))
         (known (assoc type vm-maildrop-types)))
    (cond ((null known)
           (format (concat "%s names the maildrop %S, whose type %S is not one"
                           " VM knows.  The types are %s.  See Server mail in"
                           " the VM manual")
                   where spec type
                   (mapconcat #'car vm-maildrop-types ", ")))
          ((/= (length fields) (cdr known))
           (format (concat "%s names the maildrop %S, which has %d"
                           " colon-separated field%s where %s takes %d.  See"
                           " Server mail in the VM manual")
                   where spec (length fields)
                   (if (= (length fields) 1) "" "s")
                   type (cdr known))))))

(defun vm-configuration-problems-maildrops ()
  "Every problem with a maildrop VM has been given."
  (let ((problems nil))
    (dolist (entry vm-imap-account-alist)
      (let ((problem (vm-maildrop-problem
                      (car entry) "`vm-imap-account-alist'")))
        (when problem (push problem problems))))
    (dolist (spec (vm-spool-files))
      (when (and (stringp spec) (string-match-p "\\`[a-z-]+:" spec))
        (let ((problem (vm-maildrop-problem spec "`vm-spool-files'")))
          (when problem (push problem problems)))))
    (nreverse problems)))

(defun vm-configuration-problems ()
  "Everything wrong with this VM configuration, as a list of strings."
  (append
   (delq nil (list (vm-configuration-problem-mail-agent)
                   (vm-configuration-problem-from-address)
                   (vm-configuration-problem-from-header)
                   (vm-configuration-problem-sending)
                   (vm-configuration-problem-folder-directory)
                   (vm-configuration-problem-mail-source)))
   (vm-configuration-problems-maildrops)))

;;;###autoload
(defun vm-check-configuration ()
  "Say what is missing from your VM configuration, and what to do about it.

Checks the settings a first-time reader has to get right before anything
works: which mail agent Emacs uses, the address mail goes out from, how it
is sent, where folders are kept, and where new mail comes from.  Each
maildrop is checked for a type VM knows and the right number of fields,
neither of which the parsers mind, and `vm-mail-header-from' for a value
that is not an address, which is a `From' header nobody can reply to.

Says nothing about taste.  Everything it reports is a setting whose default
either does nothing or does something the reader did not choose."
  (interactive)
  (let ((problems (vm-configuration-problems)))
    (if (null problems)
        (message "VM configuration: nothing missing")
      (with-output-to-temp-buffer "*VM Configuration*"
        (princ (format "%d thing%s to set up:\n\n"
                       (length problems) (if (cdr problems) "s" "")))
        (dolist (problem problems)
          (princ (concat "* " problem "\n\n")))
        (princ "The VM manual's Setting Up chapter works through all of\n")
        (princ "these in order: M-x info, then (vm) Setting Up.\n")))
    (length problems)))

(defvar vm-suggested-checking-configuration nil
  "Whether the suggestion to check the configuration has been made.
Once per Emacs session, not once per folder visited.")

(defun vm-suggest-checking-configuration-maybe ()
  "Say once that vm-check-configuration has something to report.

A suggestion and not the report: it names the command and how many things
it found, and leaves the reading of them to someone who asks.  Nothing is
said on a configuration whose checked settings are all in place, so this is
silent for everyone it has nothing to tell."
  (when (and vm-suggest-checking-configuration
             (not vm-suggested-checking-configuration))
    (setq vm-suggested-checking-configuration t)
    (let ((problems (length (vm-configuration-problems))))
      (when (> problems 0)
        (vm-warn 1 2 (concat "%d thing%s not set up: M-x vm-check-configuration"
                             " says what (vm-suggest-checking-configuration"
                             " to stop this)")
                 problems (if (= problems 1) " is" "s are"))))))

;;; vm.el ends here
