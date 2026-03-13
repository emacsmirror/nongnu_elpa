;;; jabber-version.el --- version reporting by JEP-0092  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'jabber-iq)
(require 'jabber-util)
(require 'jabber-disco)
(require 'jabber-menu)
(require 'find-func)
(require 'lisp-mnt)

(defconst jabber-version-xmlns "jabber:iq:version"
  "XEP-0092 Software Version namespace.")

(defcustom jabber-version-show t
  "Show our client version to others.  Acts on loading."
  :type 'boolean
  :group 'jabber)

(defconst jabber-version (lm-version (find-library-name "jabber"))
  "Version string extracted from jabber.el.
This value provides the version field of the XEP-0092 Service Discovery
jabber:iq:version query response, when `jabber-version-show` is non
`nil`.")

(add-to-list 'jabber-jid-info-menu
	     (cons "Request software version" 'jabber-get-version))
(defun jabber-get-version (jc to)
  "Request software version.

JC is the Jabber connection."
  (interactive (list
		(jabber-read-account)
		(jabber-read-jid-completing "Request version of: " nil nil nil 'full t)))
  (jabber-send-iq jc to
		  "get"
		  `(query ((xmlns . ,jabber-version-xmlns)))
		  #'jabber-process-data #'jabber-process-version
		  #'jabber-process-data "Version request failed"))

;; called by jabber-process-data
(defun jabber-process-version (_jc xml-data)
  "Handle results from jabber:iq:version requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((query (jabber-iq-query xml-data)))
    (dolist (x '((name . "Name:\t\t") (version . "Version:\t") (os . "OS:\t\t")))
      (let ((data (car (jabber-xml-node-children (car (jabber-xml-get-children query (car x)))))))
	(when data
	  (insert (cdr x) data "\n"))))))

(if jabber-version-show
    (and
     (add-to-list 'jabber-iq-get-xmlns-alist (cons jabber-version-xmlns 'jabber-return-version))
     (jabber-disco-advertise-feature jabber-version-xmlns)))

(defun jabber-return-version (jc xml-data)
  "Return client version as defined in XEP-0092.
Sender and ID are determined from the incoming packet passed in XML-DATA.

JC is the Jabber connection."
  ;; Things we might check: does this iq message really have type='get' and
  ;; exactly one child, namely query with xmlns='jabber:iq:version'?
  ;; Then again, jabber-process-iq should take care of that.
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id))
	(os (format "Emacs %d.%d (%s)"
	     emacs-major-version emacs-minor-version
	     system-type)))
    (jabber-send-iq jc to "result"
		    `(query ((xmlns . ,jabber-version-xmlns))
			    (name () "jabber.el")
			    (version () ,jabber-version)
			    ;; Booting... /vmemacs.el
			    ;; Shamelessly stolen from someone's sig.
			    (os () ,os))
		    nil nil nil nil
		    id)))

(provide 'jabber-version)

;;; jabber-version.el ends here