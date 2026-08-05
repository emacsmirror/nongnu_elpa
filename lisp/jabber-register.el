;;; jabber-register.el --- registration according to JEP-0077  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007 - Magnus Henoch - mange@freemail.hu
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

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jabber-core)
(require 'jabber-iq)
(require 'jabber-lifecycle)
(require 'jabber-xdata)
(require 'jabber-xdata-form)

;; Global reference declarations

(defvar jabber-buffer-connection)       ; jabber-chatbuffer.el
(defvar jabber-silent-mode)             ; jabber.el
(defvar jabber-xdata-xmlns)            ; jabber-xml.el
(defvar jabber-search-xmlns)           ; jabber-search.el

(defvar-local jabber-register--submit-to nil
  "JID receiving the current registration or search form.")

(defvar-local jabber-register--legacy-p nil
  "Non-nil when the current form uses legacy XEP-0077 or XEP-0055 fields.")

(defvar-local jabber-register--registered-p nil
  "Non-nil when the current legacy XEP-0077 form edits an existing account.")

(defvar jabber-register-search-result-function nil
  "Callback used to render a submitted XEP-0055 search result.")

;; Namespace constants

(defconst jabber-register-xmlns "jabber:iq:register"
  "XEP-0077 In-Band Registration namespace.")

;;

(defun jabber-get-register (jc to)
  "Send IQ get request to TO in namespace \"jabber:iq:register\".

JC is the Jabber connection."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Register with: ")))
  (jabber-send-iq jc to
		  "get"
		  `(query ((xmlns . ,jabber-register-xmlns)))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Registration"))

(defun jabber-register--start-account-registration (jc)
  "Start in-band account registration on JC."
  (jabber-get-register jc nil))

(add-hook 'jabber-lifecycle-registration-functions
          #'jabber-register--start-account-registration)

(defconst jabber-register--legacy-fields
  '((username . "Username") (nick . "Nickname") (password . "Password")
    (name . "Full name") (first . "First name") (last . "Last name")
    (email . "E-mail") (address . "Address") (city . "City")
    (state . "State") (zip . "Zip") (phone . "Telephone")
    (url . "Web page") (date . "Birth date"))
  "Legacy XEP-0077 form fields and labels.")

(defun jabber-register--legacy-form (query default-username)
  "Return plain-data form for legacy QUERY using DEFAULT-USERNAME."
  (list
   :title "Legacy registration or search form"
   :instructions
   (when-let* ((node (car (jabber-xml-get-children query 'instructions)))
               (text (car (jabber-xml-node-children node))))
     (list text))
   :fields
   (cl-loop for node in (jabber-xml-node-children query)
            for name = (jabber-xml-node-name node)
            for label = (cdr (assq name jabber-register--legacy-fields))
            when label
            collect
            (list :var (symbol-name name)
                  :type (if (eq name 'password) "text-private" "text-single")
                  :label label
                  :values
                  (list (or (car (jabber-xml-node-children node))
                            (and (eq name 'username) default-username)
                            ""))))))

(defun jabber-register--submission (type)
  "Return current form encoded for its original protocol and request TYPE."
  (if jabber-register--legacy-p
      (let ((fields (plist-get (jabber-xdata-form-form) :fields)))
        (when (and (eq type 'register)
                   (not jabber-register--registered-p))
          (when-let* ((missing
                       (seq-find
                        (lambda (field)
                          (string-empty-p
                           (or (car (plist-get field :values)) "")))
                        fields)))
            (user-error "%s is required"
                        (or (plist-get missing :label)
                            (plist-get missing :var)))))
        (cl-loop for field in fields
                 for value = (or (car (plist-get field :values)) "")
                 unless (and (eq type 'search) (string-empty-p value))
                 collect (list (intern (plist-get field :var)) nil value)))
    (list (jabber-xdata-form-submit-form))))

(defun jabber-register--close-form ()
  "Close the current registration or search form without submitting."
  (interactive))

(defun jabber-register--actions (type)
  "Return form actions for registration or search TYPE."
  (append
   (list (list :key "RET" :label "Submit"
               :command (if (eq type 'register)
                            #'jabber-submit-register
                          #'jabber-submit-search)
               :submits-form t
               :stay-open t)
         (list :key "q" :label "Cancel" :command #'jabber-register--close-form))
   (when (eq type 'register)
     (list (list :key "d" :label "Cancel registration"
                 :command #'jabber-remove-register)))))

(defun jabber-process-register-or-search (jc xml-data)
  "Display results from jabber:iq:{register,search} query as a form.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."

  (let* ((query (jabber-iq-query xml-data))
	 (type (cond
	       ((string= (jabber-iq-xmlns xml-data) jabber-register-xmlns)
		'register)
	       ((string= (jabber-iq-xmlns xml-data) jabber-search-xmlns)
		'search)
	       (t
		(error "Namespace %s not handled by jabber-process-register-or-search" (jabber-iq-xmlns xml-data)))))
	(register-account
	 (plist-get (fsm-get-state-data jc) :registerp))
	(username
	 (plist-get (fsm-get-state-data jc) :username))
	 (server (plist-get (fsm-get-state-data jc) :server))
         (submit-to (or (jabber-xml-get-attribute xml-data 'from) server))
         (xdata (seq-find
                 (lambda (x)
                   (string= (jabber-xml-get-attribute x 'xmlns)
                            jabber-xdata-xmlns))
                 (jabber-xml-get-children query 'x)))
         (legacy-p (null xdata))
         (form (if xdata
                   (let ((parsed (jabber-xdata-parse xdata)))
                     (if (and register-account
                              (string= (jabber-xdata-form-type xdata)
                                       jabber-register-xmlns)
                              (jabber-xdata-field parsed "username"))
                         (jabber-xdata-set-values parsed "username"
                                                  (list username))
                       parsed))
                 (jabber-register--legacy-form
                  query (and register-account username))))
         (buffer (jabber-xdata-form-open form
                                          (jabber-register--actions type))))
    (with-current-buffer buffer
      (setq-local jabber-buffer-connection jc
                  jabber-register--submit-to submit-to
                  jabber-register--legacy-p legacy-p
                  jabber-register--registered-p
                  (and legacy-p
                       (jabber-xml-get-children query 'registered))))))

(defun jabber-register--submission-callback (jc xml-data closure-data)
  "Run the callback in CLOSURE-DATA for JC and XML-DATA.
Close its form buffer after a successful submission callback."
  (funcall (plist-get closure-data :callback)
           jc xml-data (plist-get closure-data :callback-data))
  (when-let* (((plist-get closure-data :close-form))
              (buffer (plist-get closure-data :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun jabber-register--submission-callback-data (callback callback-data
                                                           close-form)
  "Return form callback data for CALLBACK with CALLBACK-DATA.
CLOSE-FORM non-nil closes the current form after CALLBACK returns."
  (list :callback callback
        :callback-data callback-data
        :buffer (current-buffer)
        :close-form close-form))

(defun jabber-submit-register (&rest _ignore)
  "Submit registration input.  See `jabber-process-register-or-search'."
  (interactive)
  (let* ((registerp
          (plist-get (fsm-get-state-data jabber-buffer-connection) :registerp))
         (handler (if registerp
                      #'jabber-process-register-secondtime
                    #'jabber-report-success))
         (text (concat "Registration with " jabber-register--submit-to))
         (error-handler (if registerp #'jabber-report-success handler))
         (error-text (if registerp "Account registration" text)))
    (jabber-send-iq jabber-buffer-connection jabber-register--submit-to
                    "set"
                    `(query ((xmlns . ,jabber-register-xmlns))
                            ,@(jabber-register--submission 'register))
                    #'jabber-register--submission-callback
                    (jabber-register--submission-callback-data
                     handler (if registerp 'success text) t)
                    #'jabber-register--submission-callback
                    (jabber-register--submission-callback-data
                     error-handler error-text nil)))

  (message "Registration sent"))

(defun jabber-process-register-secondtime (jc xml-data closure-data)
  "Receive registration success or failure.
CLOSURE-DATA is either `success' or `error'.

JC is the Jabber connection.
XML-DATA is the parsed tree data from the stream (stanzas)
obtained from `xml-parse-region'."
  (cond
   ((eq closure-data 'success)
    (message "Registration successful.  You may now connect to the server."))
   (t
    (jabber-report-success jc xml-data "Account registration")))
  (sit-for 3)
  (jabber-disconnect-one jc))

(defun jabber-submit-search (&rest _ignore)
  "Submit the current XEP-0055 search form."
  (interactive)
  (unless jabber-register-search-result-function
    (user-error "Jabber search support is not loaded"))
  (let ((text (concat "Search at " jabber-register--submit-to)))
    (jabber-send-iq jabber-buffer-connection jabber-register--submit-to
                    "set"
                    `(query ((xmlns . ,jabber-search-xmlns))
                            ,@(jabber-register--submission 'search))
                    #'jabber-register--submission-callback
                    (jabber-register--submission-callback-data
                     #'jabber-process-data
                     jabber-register-search-result-function t)
                    #'jabber-register--submission-callback
                    (jabber-register--submission-callback-data
                     #'jabber-report-success text nil))
    (message "Search sent")))

(defun jabber-remove-register (&rest _ignore)
  "Cancel registration.  See `jabber-process-register-or-search'."
  (interactive)

  (if (or jabber-silent-mode (yes-or-no-p (concat "Are you sure that you want to cancel your registration to " jabber-register--submit-to "? ")))
      (jabber-send-iq jabber-buffer-connection jabber-register--submit-to
		      "set"
		      `(query ((xmlns . ,jabber-register-xmlns))
			      (remove))
		      #'jabber-report-success "Unregistration"
		      #'jabber-report-success "Unregistration")))

(provide 'jabber-register)

;;; jabber-register.el ends here
