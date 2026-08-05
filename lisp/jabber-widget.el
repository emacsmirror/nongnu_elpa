;;; jabber-widget.el --- compatibility for legacy form entry points  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004, 2007  Magnus Henoch
;; Copyright (C) 2002, 2003, 2004  Tom Berger
;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Compatibility adapters for callers of the former widget-based form API.
;; Editors use the text-first XData model and do not load widget.el.

;;; Code:

(require 'cl-lib)
(require 'jabber-register)
(require 'jabber-util)
(require 'jabber-xdata)
(require 'jabber-xdata-form)

(defvar-local jabber-widget-alist nil
  "Legacy form state represented as plain XData fields.")

(defvar-local jabber-widget-form-type nil
  "Legacy form type, either `register' or `xdata'.")

(defvar-local jabber-widget-submit-to nil
  "JID receiving the current compatibility form.")

(define-widget 'jabber-widget-jid 'string
  "JID widget retained for legacy callers of this adapter."
  :value-to-internal
  (lambda (_widget value)
    (if-let* ((displayname (jabber-jid-rostername value)))
        (format "%s <%s>" displayname value)
      value))
  :value-to-external
  (lambda (_widget value)
    (if (string-match "<\\([^>]+\\)>[ \t]*$" value)
        (match-string 1 value)
      value))
  :complete #'jabber-widget-jid-complete)

(defun jabber-widget-jid-complete (widget)
  "Complete the JID preceding point in legacy WIDGET."
  (require 'wid-edit)
  (let* ((prefix (buffer-substring-no-properties
                  (funcall (symbol-function 'widget-field-start) widget)
                  (point)))
         (candidates
          (append (mapcar #'symbol-name jabber-roster-list)
                  (cl-loop for item in jabber-roster-list
                           for name = (jabber-jid-rostername item)
                           when name
                           collect (format "%s <%s>" name item))))
         (completion (try-completion prefix candidates)))
    (cond
     ((eq completion t) (message "Exact match"))
     ((null completion) (user-error "No completion for %s" prefix))
     ((not (equal prefix completion))
      (insert-and-inherit (substring completion (length prefix))))
     (t
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (all-completions prefix candidates)))))))

(defun jabber-widget-init-buffer (submit-to)
  "Initialize compatibility form state targeting SUBMIT-TO."
  (setq-local jabber-widget-alist nil
              jabber-widget-submit-to submit-to)
  (setq buffer-read-only nil)
  (rename-uniquely))

(defun jabber-widget--show-form (form type)
  "Render plain XData FORM for legacy compatibility TYPE."
  (let ((submit-to jabber-widget-submit-to))
    (jabber-xdata-form-mode)
    (setq-local jabber-widget-submit-to submit-to
                jabber-widget-form-type type
                jabber-widget-alist (plist-get form :fields)
                jabber-xdata-form--form form
                jabber-xdata-form--original-form form)
    (jabber-xdata-form--render)))

(defun jabber-widget-render-register-form (query &optional default-username)
  "Render legacy registration QUERY with optional DEFAULT-USERNAME."
  (jabber-widget--show-form
   (jabber-register--legacy-form query default-username) 'register))

(defun jabber-widget-parse-register-form ()
  "Return legacy registration elements from the current plain-data form."
  (cl-loop for field in (plist-get (jabber-xdata-form-form) :fields)
           collect (list (intern (plist-get field :var)) nil
                         (or (car (plist-get field :values)) ""))))

(defun jabber-widget-render-xdata-form (x &optional defaults)
  "Render XData form X, applying optional DEFAULTS alist."
  (let ((form (jabber-xdata-parse x)))
    (dolist (default defaults)
      (when (jabber-xdata-field form (car default))
        (setq form (jabber-xdata-set-values form (car default)
                                            (list (cdr default))))))
    (jabber-widget--show-form form 'xdata)))

(defun jabber-widget-parse-xdata-form ()
  "Return the current plain-data form encoded as XData submission."
  (jabber-xdata-form-submit-form))

(defun jabber-widget-xdata-value-convert (value type)
  "Convert legacy widget VALUE of XData TYPE to a list of strings."
  (cond
   ((string= type "boolean") (list (if value "1" "0")))
   ((string= type "text-multi") (split-string value "[\n\r]"))
   ((string-empty-p value) nil)
   (t (list value))))

(defun jabber-widget-render-xdata-search-results (xdata)
  "Render search-result XDATA with the text-first renderer."
  (jabber-xdata-render-result xdata))

(defun jabber-widget-render-xdata-search-results-multi (xdata)
  "Render multi-record search-result XDATA."
  (jabber-xdata-render-result xdata))

(defun jabber-widget-render-xdata-search-results-single (xdata)
  "Render single-record search-result XDATA."
  (jabber-xdata-render-result xdata))

(provide 'jabber-widget)

;;; jabber-widget.el ends here
