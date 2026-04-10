;;; jabber-private.el --- jabber:iq:private API by JEP-0049  -*- lexical-binding: t; -*-

;; Copyright (C) 2005  Magnus Henoch
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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-iq)

(defconst jabber-private-xmlns "jabber:iq:private"
  "XEP-0049 Private XML Storage namespace.")

;;;###autoload
(defun jabber-private-get (jc node-name namespace success-callback error-callback)
  "Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result."
  (jabber-send-iq jc nil "get"
		  `(query ((xmlns . ,jabber-private-xmlns))
			  (,node-name ((xmlns . ,namespace))))
		  #'jabber-private-get-1 success-callback
		  #'(lambda (jc xml-data error-callback)
		      (funcall error-callback jc xml-data))
		  error-callback))

(defun jabber-private-get-1 (jc xml-data success-callback)
  (funcall success-callback jc
	   (car (jabber-xml-node-children
		 (jabber-iq-query xml-data)))))

;;;###autoload
(defun jabber-private-set (jc fragment &optional
			      success-callback success-closure-data
			      error-callback error-closure-data)
  "Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

JC is the Jabber connection."
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . ,jabber-private-xmlns))
			  ,fragment)
		  success-callback success-closure-data
		  error-callback error-closure-data))

(provide 'jabber-private)
;;; jabber-private.el ends here
