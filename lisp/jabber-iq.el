;;; jabber-iq.el --- infoquery functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

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

(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-menu)

(defvar *jabber-open-info-queries* nil
  "Alist of open query id and their callback functions.")

(defvar jabber--iq-counter 0
  "Monotonic counter for generating unique IQ stanza IDs.")

(defvar jabber-iq-get-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ GET requests.")

(defvar jabber-iq-set-xmlns-alist nil
  "Mapping from XML namespace to handler for IQ SET requests.")

(defvar-keymap jabber-browse-mode-map
  :parent jabber-common-keymap)

(defcustom jabber-browse-mode-hook nil
  "Hook run when entering Browse mode."
  :group 'jabber
  :type 'hook)

(defgroup jabber-browse nil "browse display options"
  :group 'jabber)

(defcustom jabber-browse-buffer-format "*browse:%n*"
  "The format specification for the name of browse buffers.

These fields are available at this moment:

%n   JID to browse"
  :type 'string)

;; Global reference declarations

(declare-function jabber-send-sexp "jabber-core.el"  (jc sexp))
(defvar jabber-iq-chain)                ; jabber-core.el
(defvar jabber-stanzas-xmlns)          ; jabber-xml.el

;;

(define-derived-mode jabber-browse-mode special-mode "jabber-browse"
  "Special mode."  ;; FIXME: Improve!
  (setq buffer-read-only t)
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-minor-mode-cycle t)
  (outline-minor-mode 1))

(with-eval-after-load "jabber-core"
  (jabber-chain-add 'jabber-iq-chain #'jabber-process-iq))
(defun jabber-process-iq (jc xml-data)
  "Process an incoming iq stanza.

JC is the Jabber Connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((id (jabber-xml-get-attribute xml-data 'id))
         (type (jabber-xml-get-attribute xml-data 'type))
         (from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
         (callback (assoc id *jabber-open-info-queries*)))
    (cond
     ;; if type is "result" or "error", this is a response to a query we sent.
     ((or (string= type "result")
	  (string= type "error"))
      (let ((callback-cons (nth (cdr (assoc type '(("result" . 0)
						   ("error" . 1)))) (cdr callback))))
	(when (and (consp callback-cons) (car callback-cons))
	  (funcall (car callback-cons) jc xml-data (cdr callback-cons))))
      (setq *jabber-open-info-queries* (delq callback *jabber-open-info-queries*)))

     ;; if type is "get" or "set", correct action depends on namespace of request.
     ((and (listp query)
	   (or (string= type "get")
	       (string= type "set")))
      (let* ((which-alist (pcase type
			     ("get" jabber-iq-get-xmlns-alist)
			     ("set" jabber-iq-set-xmlns-alist)))
	     (handler (cdr (assoc (jabber-xml-get-attribute query 'xmlns) which-alist))))
	(if handler
	    (condition-case error-var
		(funcall handler jc xml-data)
	      (jabber-error
	       (apply #'jabber-send-iq-error jc from id query (cdr error-var)))
	      (error (jabber-send-iq-error jc from id query "wait" 'internal-server-error (error-message-string error-var))))
	  (jabber-send-iq-error jc from id query "cancel" 'feature-not-implemented)))))))

(defun jabber-send-iq (jc to type query success-callback success-closure-data
			  error-callback error-closure-data &optional result-id)
  "Send an iq stanza to the specified entity, and optionally set up a callback.
JC is the Jabber connection.
TO is the addressee.
TYPE is one of \"get\", \"set\", \"result\" or \"error\".
QUERY is a list containing the child of the iq node in the format
`jabber-sexp2xml' accepts.
SUCCESS-CALLBACK is the function to be called when a successful result arrives.
SUCCESS-CLOSURE-DATA is an extra argument to SUCCESS-CALLBACK.
ERROR-CALLBACK is the function to be called when an error arrives.
ERROR-CLOSURE-DATA is an extra argument to ERROR-CALLBACK.
RESULT-ID is the id to be used for a response to a received iq message.
`jabber-report-success' and `jabber-process-data' are common callbacks.

The callback functions are called like this:
\(funcall CALLBACK JC XML-DATA CLOSURE-DATA)
with XML-DATA being the IQ stanza received in response. "
  (let ((id (or result-id (format "emacs-iq-%d" (cl-incf jabber--iq-counter)))))
    (if (or success-callback error-callback)
	(setq *jabber-open-info-queries* (cons (list id
						     (cons success-callback success-closure-data)
						     (cons error-callback error-closure-data))

					       *jabber-open-info-queries*)))
    (jabber-send-sexp jc
		      (list 'iq (append
				 (if to (list (cons 'to to)))
				 (list (cons 'type type))
				 (list (cons 'id id)))
			    query))))

(defun jabber-send-iq-error (jc to id original-query error-type condition
				&optional text app-specific)
  "Send an error iq stanza in response to a previously sent iq stanza.
Send an error iq stanza to the specified entity in response to a
previously sent iq stanza.
TO is the addressee.
ID is the id of the iq stanza that caused the error.
ORIGINAL-QUERY is the original query, which should be included in the
error, or nil.
ERROR-TYPE is one of \"cancel\", \"continue\", \"modify\", \"auth\"
and \"wait\".
CONDITION is a symbol denoting a defined XMPP condition.
TEXT is a string to be sent in the error message, or nil for no text.
APP-SPECIFIC is a list of extra XML tags.
JC is the Jabber connection.

See section 9.3 of XMPP Core."
  (jabber-send-sexp
   jc
   `(iq (,@(when to `((to . ,to)))
	 (type . "error")
	 (id . ,(or id "")))
	,original-query
	(error ((type . ,error-type))
	       (,condition ((xmlns . ,jabber-stanzas-xmlns)))
	       ,(if text
		    `(text ((xmlns . ,jabber-stanzas-xmlns))
			   ,text))
	       ,@app-specific))))

(defun jabber-browse--buffer (jid)
  "Return the browse buffer for JID, creating it if needed.
When newly created, insert a JID header line."
  (let* ((name (format-spec jabber-browse-buffer-format
                            (list (cons ?n jid))))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'jabber-browse-mode)
        (jabber-browse-mode)
        (let ((inhibit-read-only t))
          (insert (propertize jid 'face 'jabber-title) "\n\n"))))
    buf))

(defun jabber-browse--insert (jc xml-data closure-data)
  "Render CLOSURE-DATA into the current buffer at point-max.
CLOSURE-DATA is a function (called with JC and XML-DATA), an error
string, or nil (dumps raw XML)."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (save-excursion
      (cond
       ((functionp closure-data)
        (let ((result (funcall closure-data jc xml-data)))
          (when (stringp result)
            (insert result "\n\n"))))
       ((stringp closure-data)
        (insert closure-data ": "
                (jabber-parse-error (jabber-iq-error xml-data)) "\n\n"))
       (t
        (insert (format "%S\n\n" xml-data)))))))

(defun jabber-process-data (jc xml-data closure-data)
  "Process random results from various requests.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let* ((from (or (jabber-xml-get-attribute xml-data 'from)
                   (plist-get (fsm-get-state-data jc) :server)))
         (buf (jabber-browse--buffer from)))
    (with-current-buffer buf
      (jabber-browse--insert jc xml-data closure-data)
      (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
        (run-hook-with-args hook 'browse buf
                            (funcall jabber-alert-info-message-function
                                     'browse buf))))))

(defun jabber-silent-process-data (jc xml-data closure-data)
  "Process random results from various requests to only alert hooks.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (let ((text (cond
               ((functionp closure-data)
                (funcall closure-data jc xml-data))
               ((stringp closure-data)
                (concat closure-data ": " (jabber-parse-error (jabber-iq-error xml-data))))
               (t
                (format "%S" xml-data)))))
    (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
      (run-hook-with-args hook 'browse (current-buffer)
                          text))))

(provide 'jabber-iq)
;;; jabber-iq.el ends here.
