;;; vm-pop.el --- POP folder-side support for VM  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 1993, 1994, 1997, 1998 Kyle E. Jones
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
(require 'vm-summary)
(require 'vm-window)
(require 'vm-motion)
(require 'vm-undo)
(require 'vm-crypto)
(require 'vm-mime)
(eval-when-compile (require 'cl-lib))

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)

(declare-function vm-submit-bug-report
		  "vm.el" (&optional pre-hooks post-hooks))
(declare-function open-network-stream
		  "subr.el" (name buffer host service &rest parameters))

(defvar auth-sources)  ;; from auth-source.el, used for dynamic binding

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-cant-uidl "Can't use UIDL")
      (define-error 'vm-dele-failed "DELE command failed")
      (define-error 'vm-uidl-failed "UIDL command failed"))
  (put 'vm-cant-uidl 'error-conditions '(vm-cant-uidl error))
  (put 'vm-cant-uidl 'error-message "Can't use UIDL")
  (put 'vm-dele-failed 'error-conditions '(vm-dele-failed error))
  (put 'vm-dele-failed 'error-message "DELE command failed")
  (put 'vm-uidl-failed 'error-conditions '(vm-uidl-failed error))
  (put 'vm-uidl-failed 'error-message "UIDL command failed"))

(defun vm-pop-find-cache-file-for-spec (remote-spec)
  "Given REMOTE-SPEC, which is a maildrop specification of a folder on
a POP server, find its cache file on the file system"
  ;; Prior to VM 7.11, we computed the cache filename
  ;; based on the full POP spec including the password
  ;; if it was in the spec.  This meant that every
  ;; time the user changed his password, we'd start
  ;; visiting the wrong (and probably nonexistent)
  ;; cache file.
  ;;
  ;; To fix this we do two things.  First, migrate the
  ;; user's caches to the filenames based in the POP
  ;; sepc without the password.  Second, we visit the
  ;; old password based filename if it still exists
  ;; after trying to migrate it.
  ;;
  ;; For VM 7.16 we apply the same logic to the access
  ;; methods, pop, pop-ssh and pop-ssl and to
  ;; authentication method and service port, which can
  ;; also change and lead us to visit a nonexistent
  ;; cache file.  The assumption is that these
  ;; properties of the connection can change and we'll
  ;; still be accessing the same mailbox on the
  ;; server.

  (let ((f-pass (vm-pop-make-filename-for-spec remote-spec))
	(f-nopass (vm-pop-make-filename-for-spec remote-spec t))
	(f-nospec (vm-pop-make-filename-for-spec remote-spec t t)))
    (cond ((or (string= f-pass f-nospec)
	       (file-exists-p f-nospec))
	   nil )
	  ((file-exists-p f-pass)
	   ;; try to migrate
	   (condition-case nil
	       (rename-file f-pass f-nospec)
	     (error nil)))
	  ((file-exists-p f-nopass)
	   ;; try to migrate
	   (condition-case nil
	       (rename-file f-nopass f-nospec)
	     (error nil))))
    ;; choose the one that exists, password version,
    ;; nopass version and finally nopass+nospec
    ;; version.
    (cond ((file-exists-p f-pass)
	   f-pass)
	  ((file-exists-p f-nopass)
	   f-nopass)
	  (t
	   f-nospec))))


;; Our goal is to drag the mail from the POP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.

;;;###autoload
(defun vm-expunge-pop-messages ()
  "Deletes all messages from POP mailbox that have already been retrieved
into the current folder.  VM sends POP DELE commands to all the
relevant POP servers to remove the messages."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (vm-error-if-virtual-folder)
  (if (and (vm-interactive-p) (eq vm-folder-access-method 'pop))
      (error "This command is not meant for POP folders.  Use the normal folder expunge instead."))
  ;; On the driver, as the IMAP one is: a session per maildrop, and Emacs held
  ;; for all of them.
  (unless (vm-pop-net-expunge-retrieved)
    (vm-inform 5 (concat "Nothing expunged: VM has no password for the"
			 " maildrop yet"))))

(defun vm-pop-get-password (popdrop source user host port ask-password)
  "Return the password for POPDROP at server SOURCE.  It corresponds
to the USER login at HOST and PORT.  ASK-PASSWORD says whether
passwords can be queried interactively."
  (let ((pass (car (cdr (assoc source vm-pop-passwords)))))
    (when (null pass)
      (setq pass (vm-auth-source-password
		  (list (vm-pop-find-name-for-spec source) host)
		  port user)))
    (while (and (null pass) ask-password)
      (setq pass
	    (read-passwd
	     (format "POP password for %s: " popdrop)))
      (when (equal pass "")
	(vm-warn 0 2 "Password cannot be empty")
	(setq pass nil)))
    (when (null pass)
      (error "Need password for %s" popdrop))
    pass)  )


(defun vm-pop-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    ;; CRLF -> LF
    (goto-char start)
    (while (and (< (point) end) (search-forward "\r\n" end t))
      (replace-match "\n" t t))
    ;; chop leading dots
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^\\."  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

;;;###autoload
(defun vm-pop-find-spec-for-name (name)
  "Returns the full maildrop specification of a short name NAME."
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (equal name (nth 1 (car list)))
	  (setq done t)
	(setq list (cdr list))))
    (and list (car (car list)))))

;;;###autoload
(defun vm-pop-find-name-for-spec (spec)
  "Returns the short name of a POP maildrop specification SPEC."
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (equal spec (car (car list)))
	  (setq done t)
	(setq list (cdr list))))
    (and list (nth 1 (car list)))))

;;;###autoload
(defun vm-pop-find-name-for-buffer (buffer)
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (eq buffer (vm-get-file-buffer (vm-pop-make-filename-for-spec
					  (car (car list)))))
	  (setq done t)
	(setq list (cdr list))))
    (and list (nth 1 (car list)))))

;;;###autoload
(defun vm-pop-make-filename-for-spec (spec &optional scrub-password scrub-spec)
  "Returns the cache file in use for the POP maildrop specification SPEC.
The name is built from the MD5 of the specification; `vm-cache-file-in-use'
decides between an existing cache and the name a new one gets."
  (let (md5 list)
    (if (and (null scrub-password) (null scrub-spec))
	nil
      (setq list (vm-pop-parse-spec-to-list spec))
      (setcar (vm-last list) "*")	; scrub password
      (if scrub-spec
	  (progn
	    (cond ((= (length list) 6)
		   (setcar list "pop")	; standardise protocol name
		   (setcar (nthcdr 2 list) "*")	; scrub port number
		   (setcar (nthcdr 3 list) "*")) ; scrub auth method
		  (t
		   (setq list (cons "pop" list))
		   (setcar (nthcdr 2 list) "*")
		   (setcar (nthcdr 3 list) "*")))))
      (setq spec (mapconcat (function identity) list ":")))
    (setq md5 (vm-md5-string spec))
    (vm-cache-file-in-use
     (expand-file-name (concat "pop-cache-" md5)
		       (or vm-pop-folder-cache-directory
			   vm-folder-directory
			   (getenv "HOME"))))))

(defun vm-pop-parse-spec-to-list (spec)
  (if (string-match "\\(pop\\|pop-ssh\\|pop-ssl\\)" spec)
      (vm-parse spec "\\([^:]+\\):?" 1 5)
    (vm-parse spec "\\([^:]+\\):?" 1 4)))


;;;###autoload
(defun vm-pop-start-bug-report ()
  "Begin to compose a bug report for POP support functionality."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (setq vm-kept-pop-buffers nil)
  (setq vm-pop-keep-trace-buffer 20))

;;;###autoload
(defun vm-pop-submit-bug-report ()
  "Submit a bug report for VM's POP support functionality.  
It is necessary to run `vm-pop-start-bug-report' before the problem
occurrence and this command after the problem occurrence, in
order to capture the trace of POP sessions during the occurrence.

The session still running is included, so a report can be made about a fetch
while it is happening; nothing is closed to collect it."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (vm-interactive-p))
  (if (or vm-pop-keep-trace-buffer
	  (y-or-n-p "Did you run vm-pop-start-bug-report earlier? "))
      (vm-inform 5 "Thank you. Preparing the bug report... ")
    (vm-inform 1 "Consider running vm-pop-start-bug-report before the problem occurrence"))
  (let ((buffers (vm-pop-net-trace-buffers)))
    (vm-submit-bug-report
     nil (list (lambda () (vm-insert-session-traces "POP" buffers))))))

;;;###autoload
(defun vm-pop-set-default-attributes (m)
  (vm-set-headers-to-be-retrieved-of m nil)
  (vm-set-body-to-be-retrieved-of m nil)
  (vm-set-body-to-be-discarded-of m nil))


(provide 'vm-pop)
;;; vm-pop.el ends here
