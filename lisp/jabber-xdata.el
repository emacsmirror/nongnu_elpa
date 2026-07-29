;;; jabber-xdata.el --- XMPP data form helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Parse XMPP data forms without depending on widget rendering.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jabber-xml)

(defconst jabber-xdata--field-types
  '("boolean" "fixed" "hidden" "jid-multi" "jid-single"
    "list-multi" "list-single" "text-multi" "text-private" "text-single")
  "Field types defined by XEP-0004.")

(defun jabber-xdata--child-text (node child-name)
  "Return the text of NODE's first CHILD-NAME child."
  (when-let* ((child (car (jabber-xml-get-children node child-name))))
    (car (jabber-xml-node-children child))))

(defun jabber-xdata--values (field)
  "Return FIELD's value strings in document order."
  (mapcar (lambda (value)
            (or (car (jabber-xml-node-children value)) ""))
          (jabber-xml-get-children field 'value)))

(defun jabber-xdata--field-type (field)
  "Return FIELD's supported type, defaulting to text-single."
  (let ((type (jabber-xml-get-attribute field 'type)))
    (if (member type jabber-xdata--field-types)
        type
      "text-single")))

(defun jabber-xdata--option (option)
  "Return plain data for OPTION, or nil when it has no value."
  (when-let* ((value (jabber-xdata--child-text option 'value)))
    (list :label (or (jabber-xml-get-attribute option 'label) value)
          :value value)))

(defun jabber-xdata--field (field)
  "Return plain data parsed from XEP-0004 FIELD."
  (let* ((type (jabber-xdata--field-type field))
         (options (seq-keep #'jabber-xdata--option
                            (jabber-xml-get-children field 'option)))
         (result
          (list :var (jabber-xml-get-attribute field 'var)
                :type type
                :label (jabber-xml-get-attribute field 'label)
                :description (jabber-xdata--child-text field 'desc)
                :required
                (not (null (jabber-xml-get-children field 'required)))
                :options options)))
    (plist-put result :values
               (jabber-xdata--normalized-values
                result (jabber-xdata--values field)))))

(defun jabber-xdata-parse (x)
  "Return a plain-data representation of XEP-0004 form X."
  (list :title (jabber-xdata--child-text x 'title)
        :instructions
        (mapcar (lambda (instruction)
                  (or (car (jabber-xml-node-children instruction)) ""))
                (jabber-xml-get-children x 'instructions))
        :fields (mapcar #'jabber-xdata--field
                        (jabber-xml-get-children x 'field))))

(defun jabber-xdata-field (form var)
  "Return the field named VAR from plain-data FORM."
  (seq-find (lambda (field)
              (equal (plist-get field :var) var))
            (plist-get form :fields)))

(defun jabber-xdata--list-values (field values)
  "Return advertised FIELD option VALUES in server order."
  (let ((selected (delete-dups (copy-sequence values))))
    (cl-loop for option in (plist-get field :options)
             for value = (plist-get option :value)
             when (member value selected)
             collect value)))

(defun jabber-xdata--normalized-values (field values)
  "Return VALUES normalized for FIELD's XEP-0004 type."
  (pcase (plist-get field :type)
    ("boolean"
     (when values
       (list (if (member (car values) '("1" "true")) "1" "0"))))
    ("list-multi"
     (jabber-xdata--list-values field values))
    ("list-single"
     (seq-take (jabber-xdata--list-values field values) 1))
    ("jid-multi"
     (seq-uniq values #'jabber-xdata--jid-equal-p))
    ((or "text-multi" "hidden")
     (copy-sequence values))
    (_
     (seq-take values 1))))

(defun jabber-xdata--jid-equal-p (first second)
  "Return non-nil when JIDs FIRST and SECOND compare equal."
  (let ((split (lambda (jid)
                 (if (string-match
                      "\\`\\([^/]*\\)\\(?:/\\(.*\\)\\)?\\'" jid)
                     (cons (downcase (match-string 1 jid))
                           (match-string 2 jid))
                   (cons (downcase jid) nil)))))
    (equal (funcall split first) (funcall split second))))

(defun jabber-xdata-set-values (form var values)
  "Return FORM with field VAR replaced by normalized VALUES."
  (or (jabber-xdata-field form var)
      (error "Unknown XEP-0004 field %s" var))
  (let ((fields
         (mapcar
          (lambda (field)
            (if (equal (plist-get field :var) var)
                (plist-put (copy-sequence field) :values
                           (jabber-xdata--normalized-values field values))
              field))
          (plist-get form :fields))))
    (plist-put (copy-sequence form) :fields fields)))

(defun jabber-xdata--missing-required-fields (form)
  "Return required field plists missing values in FORM."
  (cl-loop for field in (plist-get form :fields)
           for values = (plist-get field :values)
           when (and (plist-get field :required)
                     (not (seq-some (lambda (value)
                                      (and (stringp value)
                                           (not (string-empty-p value))))
                                    values)))
           collect field))

(defun jabber-xdata-missing-required-fields (form)
  "Return labels of required fields missing values in FORM."
  (mapcar (lambda (field)
            (or (plist-get field :label)
                (plist-get field :var)
                "Unnamed field"))
          (jabber-xdata--missing-required-fields form)))

(defun jabber-xdata--submit-field (field)
  "Return FIELD encoded for an XEP-0004 submission."
  (when (and (plist-get field :var)
             (not (string= (plist-get field :type) "fixed")))
    `(field ((var . ,(plist-get field :var)))
            ,@(mapcar (lambda (value) `(value nil ,value))
                      (plist-get field :values)))))

(defun jabber-xdata-submit (form)
  "Return FORM encoded as an XEP-0004 submission."
  `(x ((xmlns . ,jabber-xdata-xmlns) (type . "submit"))
      ,@(seq-keep #'jabber-xdata--submit-field
                  (plist-get form :fields))))

(defun jabber-xdata-form-type (x)
  "Return the form type of the XData form X.
Return nil when X has no XEP-0068 FORM_TYPE field."
  (catch 'form-type
    (dolist (field (jabber-xml-get-children x 'field))
      (when (and (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
                 (string= (jabber-xml-get-attribute field 'type) "hidden"))
        (throw 'form-type
               (car (jabber-xml-node-children
                     (car (jabber-xml-get-children field 'value)))))))))

(define-obsolete-function-alias 'jabber-widget-xdata-formtype
  #'jabber-xdata-form-type "0.11.0")

(provide 'jabber-xdata)

;;; jabber-xdata.el ends here
