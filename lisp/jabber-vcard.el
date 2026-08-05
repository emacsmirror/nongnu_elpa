;;; jabber-vcard.el --- vcards according to JEP-0054  -*- lexical-binding: t; -*-

;; Copyright (C) 2005, 2007  Magnus Henoch
;; Copyright (C) 2026  Thanos Apollo

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; There are great variations in Jabber vcard implementations.  This
;; one adds some spice to the mix, while trying to follow the JEP
;; closely.

;; Fields not implemented: GEO, LOGO, AGENT, ORG, CATEGORIES, SOUND,
;; CLASS, KEY.

;; The internal data structure used for vCards is an alist.  All
;; keys are uppercase symbols.
;;
;; FN, NICKNAME, BDAY, JABBERID, MAILER, TZ, TITLE, ROLE, NOTE,
;; PRODID, REV, SORT-STRING, UID, URL, DESC:
;; Value is a string.
;;
;; N:
;;   Value is an alist, with keys FAMILY, GIVEN, MIDDLE, PREFIX and SUFFIX.
;;
;; ADR:
;;   Value is a list, each element representing a separate address.
;;   The car of each address is a list of types; possible values are
;;   HOME, WORK, POSTAL, PARCEL, DOM, INTL, PREF.
;;   The cdr of each address is an alist, with keys POBOX, EXTADD,
;;   STREET, LOCALITY, REGION, PCODE, CTRY, and values being strings.
;;
;; TEL:
;;   Value is a list, each element representing a separate phone number.
;;   The car of each number is a list of types; possible values are
;;   HOME, WORK, VOICE, FAX, PAGER, MSG, CELL, VIDEO, BBS, MODEM, ISDN,
;;   PCS, PREF
;;   The cdr is the phone number as a string.
;;
;; EMAIL:
;;   Value is a list, each element representing a separate e-mail address.
;;   The car of each address is a list of types; possible values are
;;   HOME, WORK, INTERNET, PREF, X400.  At least one of INTERNET and
;;   X400 is always present.
;;   The cdr is the address as a string.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jabber-core)
(require 'jabber-iq)
(require 'jabber-avatar)
(require 'jabber-image)
(require 'keymap-popup)

(defconst jabber-vcard-xmlns "vcard-temp"
  "XEP-0054 vCard namespace.")

(defvar-local jabber-vcard-photo nil
  "The avatar structure for the photo in the vCard edit buffer.")

;; Global reference declarations

(declare-function jabber-vcard-avatars-update-current
                  "jabber-vcard-avatars" (jc new-hash))
(defvar jabber-vcard-fields)            ; jabber-vcard.el
(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el

;;

(defun jabber-vcard-parse (vcard)
  "Parse the vCard XML structure given in VCARD.
The top node should be the `vCard' node."
  ;; Hm... stpeter has a <query/> as top node...
  ;;(unless (eq (jabber-xml-node-name vcard) 'vCard)
  ;;  (error "Invalid vCard"))
  (let (result)
    (dolist (verbatim-node '(FN NICKNAME BDAY JABBERID MAILER TZ
				TITLE ROLE NOTE PRODID REV SORT-STRING
				UID URL DESC))
      ;; There should only be one of each of these.  They are
      ;; used verbatim.
      (let ((node (car (jabber-xml-get-children vcard
						verbatim-node))))
	;; Some clients include the node, but without data
	(when (car (jabber-xml-node-children node))
	  (push (cons (jabber-xml-node-name node)
		      (car (jabber-xml-node-children node)))
		result))))

    ;; Name components
    (let ((node (car (jabber-xml-get-children vcard 'N))))
      ;; Subnodes are FAMILY, GIVEN, MIDDLE, PREFIX, SUFFIX
      (push (cons 'N
		  (let (name)
		    (dolist (subnode (jabber-xml-node-children node))
		      (when (and (memq (jabber-xml-node-name subnode)
				       '(FAMILY GIVEN MIDDLE PREFIX SUFFIX))
				 (not (zerop (length
					      (car (jabber-xml-node-children
						    subnode))))))
			(push (cons (jabber-xml-node-name subnode)
				    (car (jabber-xml-node-children
					  subnode)))
			      name)))
		    name))
	    result))

    ;; There can be several addresses
    (let (addresses)
      (dolist (adr (jabber-xml-get-children vcard 'ADR))
	;; Find address type(s)
	(let (types)
	  (dolist (possible-type '(HOME WORK POSTAL PARCEL DOM INTL PREF))
	    (when (jabber-xml-get-children adr possible-type)
	      (push possible-type types)))

	  (let (components)
	    (dolist (component (jabber-xml-node-children adr))
	      (when (and (memq (jabber-xml-node-name component)
			       '(POBOX EXTADD STREET LOCALITY REGION
				       PCODE CTRY))
			 (not (zerop (length
				      (car (jabber-xml-node-children
					    component))))))
		(push (cons (jabber-xml-node-name component)
			    (car (jabber-xml-node-children component)))
		      components)))

	    (push (cons types components) addresses))))

      (when addresses
	(push (cons 'ADR addresses) result)))

    ;; Likewise for phone numbers
    (let (phone-numbers)
      (dolist (tel (jabber-xml-get-children vcard 'TEL))
	;; Find phone type(s)
	(let ((number (car (jabber-xml-node-children
			    (car (jabber-xml-get-children tel 'NUMBER)))))
	      types)
	  ;; Some clients put no NUMBER node.  Avoid that.
	  (when number
	    (dolist (possible-type '(HOME WORK VOICE FAX PAGER MSG CELL
					  VIDEO BBS MODEM ISDN PCS PREF))
	      (when (jabber-xml-get-children tel possible-type)
		(push possible-type types)))

	    (push (cons types number) phone-numbers))))

      (when phone-numbers
	(push (cons 'TEL phone-numbers) result)))

    ;; And for e-mail addresses
    (let (e-mails)
      (dolist (email (jabber-xml-get-children vcard 'EMAIL))
	(let ((userid (car (jabber-xml-node-children
			    (car (jabber-xml-get-children email 'USERID)))))
	      types)
	  ;; Some clients put no USERID node.  Avoid that.
	  (when userid
	    (dolist (possible-type '(HOME WORK INTERNET PREF X400))
	      (when (jabber-xml-get-children email possible-type)
		(push possible-type types)))
	    (unless (or (memq 'INTERNET types)
			(memq 'X400 types))
	      (push 'INTERNET types))

	    (push (cons types userid) e-mails))))

      (when e-mails
	(push (cons 'EMAIL e-mails) result)))

    ;; XEP-0153: vCard-based avatars
    (let ((photo-tag (car (jabber-xml-get-children vcard 'PHOTO))))
      (when photo-tag
	(let ((type (jabber-xml-path photo-tag '(TYPE "")))
	      (binval (jabber-xml-path photo-tag '(BINVAL ""))))
	  (when (and type binval)
	    (push (list 'PHOTO type binval) result)))))

    result))

(defun jabber-vcard-reassemble (parsed)
  "Create a vCard XML structure from PARSED."
  ;; Save photo in jabber-vcard-photo, to avoid excessive processing.
  (let ((photo (cdr (assq 'PHOTO parsed))))
    (cond
     ;; No photo
     ((null photo)
      (setq jabber-vcard-photo nil))
     ;; Existing photo
     ((listp photo)
      (setq jabber-vcard-photo
	    (jabber-avatar-from-base64-string
	     (nth 1 photo) (nth 0 photo))))
     ;; New photo from file
     (t
      (access-file photo "Avatar file not found")
      ;; Maximum allowed size is 8 kilobytes
      (when (> (nth 7 (file-attributes photo)) 8192)
	(error "Avatar bigger than 8 kilobytes"))
      (setq jabber-vcard-photo (jabber-avatar-from-file photo)))))

  `(vCard ((xmlns . ,jabber-vcard-xmlns))
	  ;; Put in simple fields
	  ,@(mapcar
	     (lambda (field)
	       (when (and (assq (car field) jabber-vcard-fields)
			  (not (zerop (length (cdr field)))))
		 (list (car field) nil (cdr field))))
	     parsed)
	  ;; Put in decomposited name
	  (N nil
	     ,@(mapcar
		(lambda (name-part)
		  (when (not (zerop (length (cdr name-part))))
		    (list (car name-part) nil (cdr name-part))))
		(cdr (assq 'N parsed))))
	  ;; Put in addresses
	  ,@(mapcar
	     (lambda (address)
	       (append '(ADR) '(())
		       (mapcar #'list (nth 0 address))
		       (mapcar (lambda (field)
				 (list (car field) nil (cdr field)))
			       (cdr address))))
	     (cdr (assq 'ADR parsed)))
	  ;; Put in phone numbers
	  ,@(mapcar
	     (lambda (phone)
	       (append '(TEL) '(())
		       (mapcar #'list (car phone))
		       (list (list 'NUMBER nil (cdr phone)))))
	     (cdr (assq 'TEL parsed)))
	  ;; Put in e-mail addresses
	  ,@(mapcar
	     (lambda (email)
	       (append '(EMAIL) '(())
		       (mapcar #'list (car email))
		       (list (list 'USERID nil (cdr email)))))
	     (cdr (assq 'EMAIL parsed)))
	  ;; Put in photo
	  ,@(when jabber-vcard-photo
	      `((PHOTO ()
		       (TYPE () ,(jabber-avatar-mime-type jabber-vcard-photo))
		       (BINVAL () ,(jabber-avatar-base64-data jabber-vcard-photo)))))))

(defun jabber-vcard-get (jc jid)
  "Request vcard from JID.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Request vcard from: " nil nil nil 'bare-or-muc)))
  (jabber-send-iq jc jid
		  "get"
		  `(vCard ((xmlns . ,jabber-vcard-xmlns)))
		  #'jabber-process-data #'jabber-vcard-display
		  #'jabber-process-data "Vcard request failed"))

(defun jabber-vcard-edit (jc)
  "Edit your own vcard.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc nil
		  "get"
		  `(vCard ((xmlns . ,jabber-vcard-xmlns)))
		  #'jabber-vcard-do-edit nil
		  #'jabber-report-success "Vcard request failed"))

(defconst jabber-vcard-fields '((FN . "Full name")
				(NICKNAME . "Nickname")
				(BDAY . "Birthday")
				(URL . "URL")
				(JABBERID . "JID")
				(MAILER . "User agent")
				(TZ . "Time zone")
				(TITLE . "Title")
				(ROLE . "Role")
				(REV . "Last changed")
				(DESC . "Description")
				(NOTE . "Note")))

(defconst jabber-vcard-name-fields '((PREFIX . "Prefix")
				     (GIVEN . "Given name")
				     (MIDDLE . "Middle name")
				     (FAMILY . "Family name")
				     (SUFFIX . "Suffix")))

(defconst jabber-vcard-phone-types '((HOME . "Home")
				     (WORK . "Work")
				     (VOICE . "Voice")
				     (FAX . "Fax")
				     (PAGER . "Pager")
				     (MSG . "Message")
				     (CELL . "Cell phone")
				     (VIDEO . "Video")
				     (BBS . "BBS")
				     (MODEM . "Modem")
				     (ISDN . "ISDN")
				     (PCS . "PCS")))

(defconst jabber-vcard-email-types '((HOME . "Home")
				     (WORK . "Work")
				     (INTERNET . "Internet")
				     (X400 . "X400")
				     (PREF . "Preferred")))

(defconst jabber-vcard-address-types '((HOME . "Home")
				       (WORK . "Work")
				       (POSTAL . "Postal")
				       (PARCEL . "Parcel")
				       (DOM . "Domestic")
				       (INTL . "International")
				       (PREF . "Preferred")))

(defconst jabber-vcard-address-fields '((POBOX . "Post box")
					(EXTADD . "Ext. address")
					(STREET . "Street")
					(LOCALITY . "Locality")
					(REGION . "Region")
					(PCODE . "Post code")
					(CTRY . "Country")))

(defun jabber-vcard-display (_jc xml-data)
  "Display received vcard.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((parsed (jabber-vcard-parse (jabber-iq-query xml-data))))
    (dolist (simple-field jabber-vcard-fields)
      (let ((field (assq (car simple-field) parsed)))
	(when field
	  (insert (cdr simple-field))
	  (indent-to 20)
	  (insert (cdr field) "\n"))))

    (let ((names (cdr (assq 'N parsed))))
      (when names
	(insert "\n")
	(dolist (name-field jabber-vcard-name-fields)
	  (let ((field (assq (car name-field) names)))
	    (when field
	      (insert (cdr name-field))
	      (indent-to 20)
	      (insert (cdr field) "\n"))))))

    (let ((email-addresses (cdr (assq 'EMAIL parsed))))
      (when email-addresses
	(insert "\n")
	(insert (propertize "E-mail addresses:\n"
			    'face 'jabber-title))
	(dolist (email email-addresses)
	  (insert (mapconcat (lambda (type)
			       (cdr (assq type jabber-vcard-email-types)))
			     (car email)
			     " "))
	  (insert ": " (cdr email) "\n"))))

    (let ((phone-numbers (cdr (assq 'TEL parsed))))
      (when phone-numbers
	(insert "\n")
	(insert (propertize "Phone numbers:\n"
			    'face 'jabber-title))
	(dolist (number phone-numbers)
	  (insert (mapconcat (lambda (type)
			       (cdr (assq type jabber-vcard-phone-types)))
			     (car number)
			     " "))
	  (insert ": " (cdr number) "\n"))))

    (let ((addresses (cdr (assq 'ADR parsed))))
      (when addresses
	(insert "\n")
	(insert (propertize "Addresses:\n"
			    'face 'jabber-title))
	(dolist (address addresses)
	  (insert (propertize
		   (mapconcat (lambda (type)
				(cdr (assq type jabber-vcard-address-types)))
			      (car address)
			      " ")
		   'face 'jabber-title))
	  (insert "\n")
	  (dolist (address-field jabber-vcard-address-fields)
	    (let ((field (assq (car address-field) address)))
	      (when field
		(insert (cdr address-field))
		(indent-to 20)
		(insert (cdr field) "\n")))))))

    ;; XEP-0153: vCard-based avatars
    (let ((photo-type (nth 1 (assq 'PHOTO parsed)))
	  (photo-binval (nth 2 (assq 'PHOTO parsed))))
      (when (and photo-type photo-binval)
	(condition-case nil
	    ;; ignore the type, let create-image figure it out.
	    (let ((image (jabber-image-create (base64-decode-string photo-binval))))
	      (insert-image image "[Photo]")
	      (insert "\n"))
	  (error (insert "Couldn't display photo\n")))))))

(defvar-local jabber-vcard--edit-data nil
  "Plain vCard alist edited in the current buffer.")

(defun jabber-vcard--set (key value)
  "Set KEY to VALUE in the current plain vCard data."
  (setq-local jabber-vcard--edit-data
              (cons (cons key value)
                    (assq-delete-all key jabber-vcard--edit-data))))

(defun jabber-vcard--render-editor ()
  "Render current plain vCard edit state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Edit vCard\n\n" 'face 'jabber-title))
    (dolist (entry (reverse jabber-vcard--edit-data))
      (insert (format "%s: %s\n" (car entry) (cdr entry))))
    (insert "\nPress m for edit commands; C-c C-c to publish.\n")
    (goto-char (point-min))))

(defun jabber-vcard-edit-simple ()
  "Edit one simple vCard field."
  (interactive)
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (cdr entry) (car entry)))
                             jabber-vcard-fields))
         (field-name (completing-read "Field: " candidates nil t))
         (field (cdr (assoc field-name candidates)))
         (value (read-string (format "%s: " field-name)
                             (cdr (assq field jabber-vcard--edit-data)))))
    (jabber-vcard--set field value)
    (jabber-vcard--render-editor)))

(defun jabber-vcard-edit-name ()
  "Edit one structured-name component."
  (interactive)
  (let* ((component (intern (completing-read
                             "Name component: "
                             '("PREFIX" "GIVEN" "MIDDLE" "FAMILY" "SUFFIX")
                             nil t)))
         (name (copy-tree (cdr (assq 'N jabber-vcard--edit-data))))
         (value (read-string (format "%s: " component)
                             (cdr (assq component name)))))
    (jabber-vcard--set 'N (cons (cons component value)
                                (assq-delete-all component name)))
    (jabber-vcard--render-editor)))

(defun jabber-vcard--read-types (prompt choices)
  "Read zero or more vCard types with PROMPT from CHOICES."
  (mapcar #'intern
          (completing-read-multiple prompt choices nil t)))

(defun jabber-vcard--add-value (key prompt types)
  "Add repeatable KEY value read with PROMPT and TYPES."
  (let ((value (read-string prompt))
        (selected (jabber-vcard--read-types "Types: " types)))
    (jabber-vcard--set key
                       (append (cdr (assq key jabber-vcard--edit-data))
                               (list (cons selected value))))
    (jabber-vcard--render-editor)))

(defun jabber-vcard-add-phone ()
  "Add a phone number."
  (interactive)
  (jabber-vcard--add-value
   'TEL "Number: "
   '("HOME" "WORK" "VOICE" "FAX" "PAGER" "MSG" "CELL" "VIDEO"
     "BBS" "MODEM" "ISDN" "PCS" "PREF")))

(defun jabber-vcard-add-email ()
  "Add an email address."
  (interactive)
  (jabber-vcard--add-value
   'EMAIL "Email: " '("HOME" "WORK" "INTERNET" "X400" "PREF")))

(defun jabber-vcard--delete-value (key)
  "Delete one repeatable value under KEY."
  (let* ((values (cdr (assq key jabber-vcard--edit-data)))
         (candidates (cl-loop for value in values for index from 1
                              collect (cons (format "%d: %s" index value)
                                            index)))
         (selected (cdr (assoc (completing-read "Delete: " candidates nil t)
                               candidates))))
    (jabber-vcard--set
     key (cl-loop for value in values for index from 1
                  unless (= index selected) collect value))
    (jabber-vcard--render-editor)))

(defun jabber-vcard-delete-phone ()
  "Delete a phone number."
  (interactive)
  (jabber-vcard--delete-value 'TEL))

(defun jabber-vcard-delete-email ()
  "Delete an email address."
  (interactive)
  (jabber-vcard--delete-value 'EMAIL))

(defun jabber-vcard-add-address ()
  "Add a postal address."
  (interactive)
  (let ((types (jabber-vcard--read-types
                "Types: " '("HOME" "WORK" "POSTAL" "PARCEL" "DOM" "INTL" "PREF")))
        fields)
    (dolist (field jabber-vcard-address-fields)
      (when-let* ((value (read-string (format "%s: " (cdr field))))
                  ((not (string-empty-p value))))
        (push (cons (car field) value) fields)))
    (jabber-vcard--set
     'ADR (append (cdr (assq 'ADR jabber-vcard--edit-data))
                  (list (cons types (nreverse fields)))))
    (jabber-vcard--render-editor)))

(defun jabber-vcard-delete-address ()
  "Delete a postal address."
  (interactive)
  (jabber-vcard--delete-value 'ADR))

(defun jabber-vcard-edit-avatar ()
  "Keep, remove, or replace the vCard avatar."
  (interactive)
  (pcase (completing-read "Avatar: " '("Keep existing" "Remove" "Choose file") nil t)
    ("Remove" (jabber-vcard--set 'PHOTO nil))
    ("Choose file" (jabber-vcard--set 'PHOTO (read-file-name "Avatar file: " nil nil t))))
  (jabber-vcard--render-editor))

(keymap-popup-define jabber-vcard-edit-mode-map
  "Edit vCard fields."
  :parent special-mode-map
  :group "Basic"
  "s" ("Simple field" jabber-vcard-edit-simple)
  "n" ("Structured name" jabber-vcard-edit-name)
  :group "Repeatable"
  "t" ("Add phone" jabber-vcard-add-phone)
  "T" ("Delete phone" jabber-vcard-delete-phone)
  "e" ("Add email" jabber-vcard-add-email)
  "E" ("Delete email" jabber-vcard-delete-email)
  "a" ("Add address" jabber-vcard-add-address)
  "A" ("Delete address" jabber-vcard-delete-address)
  :group "Avatar"
  "p" ("Edit avatar" jabber-vcard-edit-avatar)
  :group "Actions"
  "C-c C-c" ("Publish" jabber-vcard-submit)
  "m" ("Menu" jabber-vcard-edit-menu))

(define-derived-mode jabber-vcard-edit-mode special-mode "Jabber-vCard"
  "Major mode for editing a vCard as explicit plain data.")

(defun jabber-vcard-edit-menu ()
  "Show grouped vCard edit commands."
  (interactive)
  (keymap-popup jabber-vcard-edit-mode-map))

(defun jabber-vcard-do-edit (jc xml-data _closure-data)
  "Open a plain-data editor for our own vCard.
JC is the Jabber connection.  XML-DATA holds current vCard contents."
  (let ((buffer (generate-new-buffer "*Edit Jabber vCard*"))
        (parsed (jabber-vcard-parse (jabber-iq-query xml-data))))
    (with-current-buffer buffer
      (jabber-vcard-edit-mode)
      (setq-local jabber-buffer-connection jc
                  jabber-vcard--edit-data parsed)
      (jabber-vcard--render-editor))
    (pop-to-buffer buffer)
    (jabber-vcard-edit-menu)))

(defun jabber-vcard-submit (&rest _ignore)
  "Publish the vCard edited in the current plain-data buffer."
  (interactive)
  (let ((to-publish (jabber-vcard-reassemble jabber-vcard--edit-data)))
    (jabber-send-iq jabber-buffer-connection nil
		    "set"
		    to-publish
		    #'jabber-report-success "Changing vCard"
		    #'jabber-report-success "Changing vCard")
    (when (bound-and-true-p jabber-vcard-avatars-publish)
      (jabber-vcard-avatars-update-current
       jabber-buffer-connection
       (and jabber-vcard-photo (jabber-avatar-sha1-sum jabber-vcard-photo))))))

(provide 'jabber-vcard)

;;; jabber-vcard.el ends here
