;;; vm-smime.el ---  MIME support functions  -*- lexical-binding: t; -*-
;;
;; This file is part of VM
;;
;; Copyright (C) 2012 Arik Mitschang
;; Copyright (C) 2012 Uday S. Reddy
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

(require 'vm-misc)
(require 'vm-minibuf)
(require 'vm-toolbar)
(require 'vm-mouse)
(require 'vm-summary)
(require 'vm-folder)
(require 'vm-menu)
(require 'vm-crypto)
(require 'vm-window)
(require 'vm-motion)
(require 'vm-reply)
(require 'vm-digest)
(require 'vm-edit)
(require 'vm-mime)
(require 'smime)
(eval-when-compile (require 'cl-lib))

(defun vm-mime-smime-extract-pkcs7-signature (layout)
  (unless (vectorp layout)
	(goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (let ((certout (vm-read-file-name
		  "Append certificates to file: "
		  (concat smime-certificate-directory "/" 
			  (vm-get-sender)))))
	(with-temp-buffer
	  (insert "-----BEGIN PKCS7-----\n")
	  (vm-mime-insert-mime-body layout)
	  (insert "-----END PKCS7-----\n")
	  (if (smime-pkcs7-certificates-region (point-min) (point-max))
		  (progn
			(append-to-file nil nil certout)
			(message "Certificates extracted and appended to %s" certout))
		(message "Could not extract any certificates!")))))

;;;###autoload
(defun vm-mime-display-button-application/x-pkcs7-signature (layout)
  (vm-mime-display-button-application/pkcs7-signature layout))

;;;###autoload
(defun vm-mime-display-button-application/pkcs7-signature (layout)
  (if (vectorp layout)
      (let ((vm-mf-default-action "save"))
	(vm-mime-insert-button
	 :caption
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 :action
	 (function vm-mime-smime-extract-pkcs7-signature)
	 :layout layout)))
  t)

;;;###autoload
(defun vm-mime-display-application/pkcs7-mime (layout)
  (let ((vm-mime-auto-displayed-content-types
	 (append vm-mime-auto-displayed-content-types
		 '("application/pkcs7-mime" "application/x-pkcs7-mime"))))
    (vm-decode-mime-layout layout t)))

;;;###autoload
(defun vm-mime-display-button-application/x-pkcs7-mime (layout)
  (vm-mime-display-button-application/pkcs7-mime layout))

;;;###autoload
(defun vm-mime-display-button-application/pkcs7-mime (layout)
  (if (vectorp layout)
      (let ((vm-mf-default-action "decrypt message"))
	(vm-mime-insert-button
	 :caption
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 :action
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-application/pkcs7-mime layout))))
	 :layout layout
	 :disposable t)))
  t)

;;;###autoload
(defun vm-mime-display-internal-application/x-pkcs7-mime (layout
							  &optional key-email)
  (vm-mime-display-internal-application/pkcs7-mime layout key-email))

;;;###autoload
(defun vm-mime-display-internal-application/pkcs7-mime 
  (layout &optional key-email)
  "Decrypt a S/MIME encoded message using `smime-decode-region'
to do the work. The resulting structure will often be another
MIME-encoded message, so run the decoding again to present the
message as it is designed to be viewed. This funtion relies on
the user properly setting smime related variables, specifically
`smime-keys'

To have the decryption done automatically upon viewing, add 

application/pkcs7-mime, and
application/x-pkcs7-mime

to `vm-mime-auto-displayed-content-types', but at present the
smime code always asks for a password so this might mess up your
normal flow"
  (let ((start (point)) end
	(buffer-read-only nil)
	msg sub-layout ;; retval
	(real-content-type (vm-mm-layout-type layout)))
    ;; find the most appropriate key for this mail. First search in
    ;; smime keys for a recipient then fall back to user-mail-address,
    ;; this makes the assumption (probably a good one), that the mail
    ;; is encrypted with keys for any recipient
    (unless key-email
      (let ((skeys smime-keys)
	    (recips (save-excursion
		      (vm-select-folder-buffer)
		      (vm-get-recipients))))
	(while skeys
	  (when (member (caar skeys) recips)
	    (setq key-email (caar skeys)))
	  (setq skeys (cdr skeys)))))
    ;; need the content type for openssl smime to decrypt, insert the
    ;; real ones from this layout. Is there a better way? The full
    ;; content type including args (which may be necessary from smime)
    ;; is stored in a list, we must explode it
    (insert "Content-Type: ")
    (while real-content-type
      (insert (concat (car real-content-type) "; "))
      (setq real-content-type (cdr real-content-type)))
    (insert "\n\n")
    (vm-mime-insert-mime-body layout)
    (if (smime-decrypt-region
	 start (point)
	 (smime-get-key-by-email (or key-email user-mail-address)))
	(save-excursion
	  (setq end (point-marker))
	  (vm-mime-crlf-to-lf-region start end)
	  (setq msg (vm-make-message))
	  (goto-char start)
	  (vm-set-start-of msg (vm-marker (point)))
	  (vm-set-headers-of msg (vm-marker (point)))
	  (search-forward-regexp "\n\n" nil t)
	  (vm-set-text-of msg (vm-marker (point)))
	  (vm-set-text-end-of msg (vm-marker end))
	  (vm-set-end-of msg (vm-marker end))
	  (setq sub-layout (vm-mime-parse-entity-safe msg))
	  (goto-char start)
	  (insert "/*****S/MIME DECRYPT SUCCESSFUL - VIEWING SECURE*****/\n")
	  (when (vectorp layout)
	    (when (vm-decode-mime-layout sub-layout)
	      (put-text-property (point) end 'invisible t)))
	  t)
      (delete-region start (point))
      (if (y-or-n-p "Decryption failed, try a different key?")
	  (vm-mime-display-internal-application/pkcs7-mime 
	   layout (completing-read
		   ;; prompt
		   (concat "Key" (if smime-keys 
		     (concat " (default " (caar smime-keys) "): ") ":"))
		   ;; collection
		   smime-keys 
		   ;; predicate, require-match, initial-input, hist
		   nil nil nil nil 
		   ;; default
		   (car-safe (car-safe smime-keys))))
	(insert "/*****S/MIME DECRYPTION FAILED - BAD KEY?*****/\n")
	nil)
      )))

;;;###autoload
(defun vm-smime-sign-message ()
  "Toggle the current composition for S/MIME signing. This only
sets a flag and will not do the signing immediately. Actual
singing is done upon sending the message. If the message is
already set for signing this function will clear the flag so
that no signing is done"
  (interactive)
  (when (null smime-keys)
    (error "S/MIME: smime keys must be setup. See documentation for variable smime-keys"))
  (if (eq major-mode 'mail-mode)
      (if vm-smime-sign-message
	  (progn (set (make-local-variable
		       'vm-smime-sign-message) nil)
		 (setq mode-name (vm-replace-in-string
				  mode-name "SIGNED\\( \\|\\+\\)" "")))
	(set (make-local-variable 'vm-smime-sign-message) t)
	(setq mode-name (concat "SIGNED " mode-name)))
    (error "Command must be used in a VM Mail mode buffer.")))

;;;###autoload
(defun vm-smime-encrypt-message ()
  "Toggle the current composition for S/MIME encryption. This
only sets a flag and will not do the encryption immediately.
Actual encryption is done upon sending the message. If the
message is already set for encryption this function will clear
the flag so that no signing is done"
  (interactive)
  (if (eq major-mode 'mail-mode)
      (if vm-smime-encrypt-message
	  (progn (set (make-local-variable
		       'vm-smime-encrypt-message) nil)
		 (setq mode-name
		       (vm-replace-in-string
			(vm-replace-in-string
			 mode-name
			 "SIGNED\\+" "SINGED ")
			"ENCRYPTED " "")))
	(set (make-local-variable 'vm-smime-encrypt-message) t)
	(if (not vm-smime-sign-message)
	    (setq mode-name (concat "ENCRYPTED " mode-name))
	  (setq mode-name (vm-replace-in-string mode-name "SIGNED " ""))
	  (setq mode-name (concat "SIGNED+ENCRYPTED " mode-name))))
    (error "Command must be used in a VM Mail mode buffer.")))

;;;###autoload
(defun vm-smime-sign-encrypt-message ()
  "See documentation for `vm-smime-sign-message' and
`vm-smime-encrypt-message'. This function simply toggles the two
of those and can be used to instruct VM to S/MIME sign and
encrypt an outgoing message upon sending."
  (interactive)
  (if (eq major-mode 'mail-mode)
      (progn (vm-smime-sign-message)
	     (vm-smime-encrypt-message))
    (error "Command must be used in a VM Mail mode buffer.")))

;;;###autoload
(defun vm-smime-get-recipient-certfiles ()
  "Get the certificate files for encrypting a S/MIME encoded
message based on the recipient list. Uses the variable
`vm-smime-get-recipient-certificate-method' to determine how to
obtain the certificate files. Returns a list of paths to these
certificate files."
  (let ((certfiles '())
	(default-directory smime-certificate-directory))
    (cl-case vm-smime-get-recipient-certificate-method
      (ask
       ;; this method just always asks for all certificates
       (setq certfiles 
	     (append (list (read-file-name "Recipient certificate: "))))
       (while (y-or-n-p "Add more certificates?")
	 (setq certfiles 
	       (append certfiles 
		       (list (read-file-name
			      "Add recipient certificate: "))))))
      (links
       ;; go through all recipient headers getting emails. This method
       ;; assumes that the recipient cert is linked by email under the
       ;; variable smime-certificate-directory
       (setq certfiles
	     (append certfiles
		     (mapcar 'expand-file-name (vm-get-recipients))))
       (let ((files certfiles))
	 (while files
	   (when (not (file-exists-p (car files)))
	     (setq certfiles (delete (car files) certfiles))
	     (when (y-or-n-p (format "Could not find certificate %s, replace?"
				     (car files)))
	       (setq certfiles
		     (append certfiles
			     (list (read-file-name
				    "Replace with certificate: "))))))
	   (setq files (cdr files))))))
    certfiles))

;;;###autoload
(defun vm-get-sender ()
  "Determine the sender of the message, used for determining
which mapping of `smime-keys' to use in S/MIME signing a
composition. If there is no `From' header in the message,
`user-mail-address' will be used"
  (or (save-excursion
	(goto-char (point-min))
	(when (mail-fetch-field "From")
	  (cadr (mail-extract-address-components
		 (mail-fetch-field "From")))))
      user-mail-address))

(defun vm-get-recipients ()
  (let ((to-headers '("To" "Bcc" "Cc"))
	(case-fold-search t)
	(recips '()))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(narrow-to-region 
	 (point) 
	 (save-excursion (rfc822-goto-eoh) (point)))
	(while to-headers
	  (when (mail-fetch-field (car to-headers))
	    (setq 
	     recips 
	     (append 
	      recips
	      (mapcar 
	       'cadr 
	       (mail-extract-address-components
		(mail-fetch-field (car to-headers)) t)))))
	  (setq to-headers (cdr to-headers)))))
    recips))

(provide 'vm-smime)
;;; vm-smime.el ends here.
