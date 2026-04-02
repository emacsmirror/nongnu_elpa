;;; jabber-avatar.el --- generic functions for avatars  -*- lexical-binding: t; -*-

;; Copyright (C) 2006, 2007, 2008  Magnus Henoch
;; Copyright (C) 2026  Thanos Apollo

;; Author: Magnus Henoch <mange@freemail.hu>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; There are several methods for transporting avatars in Jabber. [1][2][3]
;;
;; They all have in common that they identify avatars by their SHA1
;; checksum, and (at least partially) use Base64-encoded image data.
;; Thus this library of support functions for interpreting and caching
;; avatars.

;; A contact with an avatar has the image in the avatar property of
;; the JID symbol.  Use `jabber-avatar-set' to set it.
;;
;; [1] XEP-0008: IQ-Based Avatars
;;     https://xmpp.org/extensions/xep-0008.html
;; [2] XEP-0084: User Avatar
;;     https://xmpp.org/extensions/xep-0084.html
;; [3] XEP-0153: vCard-Based Avatars
;;     https://xmpp.org/extensions/xep-0153.html

;;; Code:

(require 'mailcap)
(eval-when-compile (require 'cl-lib))
(require 'jabber-util)
(require 'jabber-image)

;;;; Variables

(defgroup jabber-avatar nil
  "Avatar related settings"
  :group 'jabber)

(defcustom jabber-avatar-cache-directory
  (expand-file-name "jabber/avatars" user-emacs-directory)
  "Directory to use for cached avatars."
  :type 'directory)

(defcustom jabber-avatar-verbose nil
  "Display messages about irregularities with other people's avatars."
  :type 'boolean)

(defcustom jabber-avatar-max-width 96
  "Maximum width of avatars."
  :type 'integer)

(defcustom jabber-avatar-max-height 96
  "Maximum height of avatars."
  :type 'integer)

;;;; Avatar data handling

(cl-defstruct avatar
  sha1-sum mime-type url base64-data height width bytes)

(defun jabber-avatar-from-url (url)
  "Construct an avatar structure from the given URL.
Retrieves the image to find info about it."
  (with-current-buffer (let ((coding-system-for-read 'binary))
			 (url-retrieve-synchronously url))
    (let* ((case-fold-search t)
	   (mime-type (ignore-errors
			(search-forward-regexp "^content-type:[ \t]*\\(.*\\)$")
			(match-string 1)))
	   (data (progn
		   (search-forward "\n\n")
		   (buffer-substring (point) (point-max)))))
      (prog1
	  (jabber-avatar-from-data data nil mime-type)
	(kill-buffer nil)))))

(defun jabber-avatar-from-file (filename)
  "Construct an avatar structure from FILENAME."
  (require 'mailcap)
  (let ((data (with-temp-buffer
		(insert-file-contents-literally filename)
		(buffer-string)))
	(mime-type (when (string-match "\\.[^.]+$" filename)
		     (mailcap-extension-to-mime (match-string 0 filename)))))
    (jabber-avatar-from-data data nil mime-type)))

(defun jabber-avatar-from-base64-string (base64-string &optional mime-type)
  "Construct an avatar stucture from BASE64-STRING.
If MIME-TYPE is not specified, try to find it from the image data."
  (jabber-avatar-from-data nil base64-string mime-type))

(defun jabber-avatar-from-data (raw-data base64-string &optional mime-type)
  "Construct an avatar structure from RAW-DATA and/or BASE64-STRING.
If either is not provided, it is computed.
If MIME-TYPE is not specified, try to find it from the image data."
  (let* ((data (or raw-data (base64-decode-string base64-string)))
	 (bytes (length data))
	 (sha1-sum (sha1 data))
	 (base64-data (or base64-string (base64-encode-string raw-data)))
	 (type (or mime-type
                   (when-let* ((detected (ignore-errors (image-type data nil t))))
                     (symbol-name detected)))))
    (make-avatar :mime-type type :sha1-sum sha1-sum :base64-data base64-data :bytes bytes)))

(defun jabber-avatar--line-height ()
  "Return the pixel height of a line, suitable for inline avatars."
  (frame-char-height))

(defun jabber-avatar-image (avatar)
  "Create an image from AVATAR sized to fit a single line.
Return nil if images of this type are not supported."
  (condition-case nil
      (let ((h (jabber-avatar--line-height)))
        (jabber-image-create
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert (avatar-base64-data avatar))
           (base64-decode-region (point-min) (point-max))
           (buffer-string))
         (avatar-mime-type avatar)
         h h))
    (error nil)))

;;;; Avatar cache

(defun jabber-avatar-find-cached (sha1-sum)
  "Return file name of cached image for avatar identified by SHA1-SUM.
If there is no cached image, return nil."
  (let ((filename (expand-file-name sha1-sum jabber-avatar-cache-directory)))
    (if (file-exists-p filename)
        filename
      nil)))

(defun jabber-avatar-cache (avatar)
  "Cache the AVATAR."
  (let* ((id (avatar-sha1-sum avatar))
	 (base64-data (avatar-base64-data avatar))
	 (filename (expand-file-name id jabber-avatar-cache-directory)))
    (unless (file-directory-p jabber-avatar-cache-directory)
      (make-directory jabber-avatar-cache-directory t))

    (if (file-exists-p filename)
	(when jabber-avatar-verbose
	  (message "Caching avatar, but %s already exists" filename))
      (with-temp-buffer
	(let ((require-final-newline nil)
	      (coding-system-for-write 'binary))
	  (set-buffer-multibyte nil)
	  (insert base64-data)
	  (base64-decode-region (point-min) (point-max))
	  (write-region (point-min) (point-max) filename nil 'silent))))))

;;;; Set avatar for contact
(defun jabber-avatar-set (jid avatar)
  "Set the avatar of JID to be AVATAR.
JID is a string containing a bare JID.
AVATAR may be one of:
 * An avatar structure.
 * The SHA1 sum of a cached avatar.
 * nil, meaning no avatar."
  ;; We want to optimize for the case of same avatar.
  ;; Loading an image is expensive, so do it lazily.
  (let ((jid-symbol (jabber-jid-symbol jid))
	image hash)
    (cond
     ((avatar-p avatar)
      (setq hash (avatar-sha1-sum avatar))
      (setq image (lambda () (jabber-avatar-image avatar))))
     ((stringp avatar)
      (setq hash avatar)
      (setq image (lambda ()
                    (when-let* ((file (jabber-avatar-find-cached avatar)))
                      (condition-case nil
                          (let ((h (jabber-avatar--line-height)))
                            (jabber-image-create-from-file file h h))
                        (error nil))))))
     (t
      (setq hash nil)
      (setq image #'ignore)))

    (unless (string= hash (get jid-symbol 'avatar-hash))
      (put jid-symbol 'avatar (funcall image))
      (put jid-symbol 'avatar-hash hash))))

(defun jabber-create-image (file-or-data &optional _type data-p)
  "Create a line-height-sized image from FILE-OR-DATA."
  (let ((h (jabber-avatar--line-height)))
    (if data-p
        (jabber-image-create file-or-data nil h h)
      (jabber-image-create-from-file file-or-data h h))))

(provide 'jabber-avatar)

;;; jabber-avatar.el ends here
