;;; transient-post.el --- Transient utilities for posting to an API -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Copyright (C) 2024 Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.1
;; Package Reqires: ((emacs "27.1"))
;; Keywords: convenience, api, requests
;; URL: https://codeberg.org/martianh/transient-post.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities, classes and methods for creating transients to POST or PATCH to
;; an API.

;;; Code:

(require 'transient)
(require 'json)

;;; OPTIONS

(defvar transient-post-convert-json-booleans-to-strings t
  "Whether to convert JSON booleans.
When fetching data, parsed JSON booleans, e.g. t and :json-false,
will be converted into the strings \"true\" and \"false\"."
  ;; NB: need to also work out how to then POST as parsed JSON data.
  )

;;; VARIABLES

(defvar transient-post-choice-booleans-json '("t" ":json-false")
  ;; add "" or nil to unset?
  "JSON Booleans formatted as parsed elisp and into a string.")

(defvar transient-post-choice-booleans '("true" "false")
  "Boolean strings for sending non-JSON requests.") ;; add "" or nil to unset?

(defvar transient-post-server-settings nil
  "Settings data (editable) as returned by the server.")

;;; UTILS

(defun transient-post-transient-to-alist (args)
  "Convert list of transient ARGS into an alist.
This currently assumes arguments are of the form \"key=value\".
Should return an alist that can be parsed as JSON data."
  (cl-loop for a in args
           for split = (split-string a "=")
           for key = (if (= (length split) 1) ; we didn't split = boolean
                         (car split)
                       (concat (car split) "="))
           for val = (transient-arg-value key args)
           for value = (cond ((member val transient-post-choice-booleans-json)
                              (intern val))
                             ((equal val "\"\"")
                              "") ;; POSTable empty string
                             (t val))
           collect (cons (car split) value)))

(defun transient-post-alist-to-transient (alist &optional prefix)
  "Convert ALIST to a list of transient args.
Returns transient arguments in the form \"key=value\".
PREFIX is a string and is used during recursion.
Should work with JSON arrays as both lists and vectors."
  (flatten-tree
   (cl-loop for a in alist
            ;; car recur if:
            if (and (proper-list-p (seq-first a)) ;; car isn't just a json cons
                    (> (length (seq-first a)) 1)) ;; car's cdr isn't nil
            do (transient-post-alist-to-transient (seq-first a) prefix)
            else
            for key = (symbol-name (seq-first a))
            for k = (if prefix
                        (concat prefix "." key)
                      key)
            for v = (seq-rest a)
            for val =
            (cond ((numberp v) (number-to-string v))
                  ((symbolp v) (symbol-name v))
                  ;; cdr recur if:
                  ((and (or (vectorp v)
                            (proper-list-p v))
                        (> (length v) 1))
                   (if (or (vectorp (seq-first v))
                           (proper-list-p (seq-first v)))
                       ;; recur on cdr as nested list or vector:
                       (cl-loop for x in v
                                collect (transient-post-alist-to-transient x k))
                     ;; recur on cdr normal list:
                     (transient-post-alist-to-transient v k)))
                  (t v))
            collect (if (stringp val)
                        (concat k "=" val)
                      val))))

(defun transient-post-remove-not-editable (alist var)
  "Remove non-editable fields from ALIST.
Check against the fields in VAR, which should be a list of strings."
  (cl-remove-if-not
   (lambda (x)
     (member (symbol-name (car x))
             var))
   alist))

(defun transient-post-return-data (fetch-fun &optional editable-var)
  "Return data to populate current settings.
Call FETCH-FUN with zero arguments to GET the data. Cull the data
with `transient-post-remove-not-editable', bind the result to
`transient-post-server-settings', then call
`transient-post-alist-to-transient' on it and return the result.
EDITABLE-VAR is a variable containing a list of strings
corresponding to the editable fields of the JSON data returned.
See `transient-post-remove-not-editable'."
  (let* ((data (funcall fetch-fun))
         (editable (if editable-var
                       (transient-post-remove-not-editable data editable-var)
                     data))
         (bools-parsed (if transient-post-convert-json-booleans-to-strings
                           (transient-post-bools-to-strs editable)
                         editable)))
    ;; used in `transient-post-arg-changed-p' and `transient-post-only-changed-args'
    (setq transient-post-server-settings bools-parsed)
    (setq transient-post-settings-as-transient
          (transient-post-alist-to-transient bools-parsed))))

(defun transient-post-get-server-val (arg)
  "Return the server value for ARG.
If ARG has dotted notation, drill down into the alist."
  (let ((split (split-string arg "\\.")))
    (if (< 1 (length split)) ;; 1 level of nesting:
        (alist-get (intern (cadr split))
                   (alist-get (intern (car split))
                              transient-post-server-settings))
      (alist-get (intern arg) ;; no dotted nesting:
                 transient-post-server-settings))))

(defun transient-post-arg-changed-p (arg-pair)
  "T if ARG-PAIR is different to the value in `transient-post-server-settings'.
The format of ARG is a transient pair as a string, ie \"key=val\".
Nil values will also match the empty string."
  (let* ((arg (split-string arg-pair "="))
         (server-val (transient-post-get-server-val (car arg)))
         (server-str (if (symbolp server-val)
                         (symbol-name server-val)
                       server-val)))
    (cond ((not (cadr arg)) (not (equal "" server-str)))
          (t (not (equal (cadr arg) server-str))))))

(defun transient-post-only-changed-args (alist)
  "Remove elts from ALIST if value is changed.
Values are considered changed if they do not match those in
`transient-post-server-settings'. Nil values are also removed if they
match the empty string."
  (cl-remove-if
   (lambda (x)
     (let* ((split (split-string (car x) "\\."))
            (server-val
             (if (< 1 (length split))
                 ;; FIXME: handle arbitrary nesting:
                 (alist-get (intern (cadr split))
                            (alist-get (intern (car split))
                                       transient-post-server-settings))
               (alist-get (intern (car x))
                          transient-post-server-settings))))
       (cond ((not (cdr x)) (equal "" server-val))
             (t (equal (cdr x) server-val)))))
   alist))

;; TODO: reverse this operation, so a suffix can parse data back to elisp
;; JSON:
(defun transient-post-bool-to-str (val)
  "Convert VAL, into a string boolean if it is either t or :json-false.
Otherwise just return VAL."
  (cond ((eq :json-false val) "false")
        ((eq t val) "true")
        (t val)))

(defun transient-post-bools-to-strs (alist)
  "Convert values in ALIST to string booleans if they are JSON booleans."
  (cl-loop for a in alist
           ;; car recur if:
           if (and (proper-list-p (seq-first a)) ;; car isn't just a json cons
                   (> (length (seq-first a)) 1)) ;; car's cdr isn't nil
           do (transient-post-bools-to-strs (seq-first a))
           else
           for v = (cdr a)
           for val =
           (cond
            ;; cdr recur if:
            ((and (or (vectorp v)
                      (proper-list-p v))
                  (> (length v) 1))
             (if (or (vectorp (seq-first v))
                     (proper-list-p (seq-first v)))
                 ;; recur on cdr as nested list or vector:
                 (cl-loop for x in v
                          collect (transient-post-bools-to-strs x))
               ;; recur on cdr normal list:
               (transient-post-bools-to-strs v)))
            (t ;; no recur:
             (transient-post-bool-to-str (cdr a))))
           collect (cons (car a) val)))

(defun transient-post-dots-to-arrays (alist)
  "Convert keys in ALIST transient-post dot annotation to array[key] annotation."
  ;; FIXME: handle multi dots?
  (cl-loop for a in alist
           collect (cons (transient-post-dot-to-array (car a))
                         (cdr a))))

(defun transient-post-dot-to-array (key)
  "Convert KEY from transient-post dot annotation to array[key] annotation."
  ;; FIXME: for multi dots, just return secondlast[last]?
  (let* ((split (split-string key "\\.")))
    (if (< 1 (length split))
        (concat (car (last split 2)) "[" (car (last split)) "]")
      key)))

;; CLASSES

(defclass transient-post-option (transient-option)
  ((always-read :initarg :always-read :initform t))
  "An infix option class for our options.
We always read.")

(defclass transient-post-option-str (transient-post-option)
  ((format :initform " %k %d %v"))
  "An infix option class for our option strings.
We always read, and our reader provides initial input from
default/current values.")

(defclass transient-post-choice-bool (transient-post-option)
  ((format :initform " %k %d %v")
   (choices :initarg :choices :initform
            '(lambda () transient-post-choice-booleans)))
  "An option class for our choice booleans.
We implement this as an option because we need to be able to
explicitly send true/false values to the server, whereas
transient ignores false/nil values.")

;;; METHODS
;; for `transient-post-choice-bool' we define our own infix option that displays
;; [t|:json-false] like exclusive switches. activating the infix just moves to
;; the next option.

(cl-defmethod transient-init-value ((obj transient-post-choice-bool))
  "Initiate the value of OBJ, fetching the value from the parent prefix."
  (let* ((arg (oref obj argument))
         (val (transient-arg-value arg (oref transient--prefix value))))
    ;; FIXME: don't understand why we don't want to set a key=val pair here:
    ;; while in fj-transient.el we set it as a key=val
    (oset obj value ;; (concat arg "=" val))))
          val)))

(cl-defmethod transient-format-value ((obj transient-post-option))
  "Format the value of OBJ, a `transient-post-option'.
Format should just be a string, highlighted green if it has been
changed from the server value."
  (let* ((pair (transient-infix-value obj))
         (value (when pair (cadr (split-string pair "=")))))
    (if (not pair)
        ""
      (propertize value
                  'face (if (transient-post-arg-changed-p pair)
                            'transient-value
                          'transient-inactive-value)))))

(defun transient-post-active-face-maybe (pair value)
  "Return a face spec based on PAIR and VALUE."
  (let ((active-p (equal pair value))
        (changed-p (transient-post-arg-changed-p pair)))
    ;; FIXME: differentiate init server value
    ;; from switching to other value then switching
    ;; back to server value?
    (cond ((and active-p changed-p)
           'transient-value)
          ((and active-p (not changed-p))
           '(:inherit transient-value :underline t))
          ((and (not active-p) (not changed-p))
           '(:inherit transient-inactive-value :underline t))
          (t
           'transient-inactive-value))))

(cl-defmethod transient-format-value ((obj transient-post-choice-bool))
  "Format the value of OBJ.
Format should be like \"[opt1|op2]\", with the active option highlighted.
The value currently on the server should be underlined."
  (let* ((value (transient-infix-value obj))
         (arg (oref obj argument))
         (choices-slot (oref obj choices))
         (choices (if (eq (car choices-slot) 'lambda)
                      (funcall choices-slot)
                    choices-slot)))
    (concat
     (propertize "["
                 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (let ((pair (concat arg choice)))
          (propertize
           choice
           'face (transient-post-active-face-maybe pair value))))
      choices
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj transient-post-choice-bool))
  "Cycle through the possible values of OBJ."
  (let* ((pair (transient-infix-value obj))
         ;; (arg (oref obj argument))
         (val (cadr (split-string pair "=")))
         (choices-slot (oref obj choices))
         (choices (if (eq (car choices-slot) 'lambda)
                      (funcall choices-slot)
                    choices-slot)))
    ;; FIXME: don't understand why we don't want to set a key=val pair here:
    ;; while in fj-transient.el we set it as a key=val:
    ;; (concat arg
    (if (equal val (car (last choices)))
        (car choices)
      (cadr (member val choices)))))

;; FIXME: see the `transient-infix-read' method's docstring:
;; we should preserve history, follow it. maybe just mod it.
(cl-defmethod transient-infix-read ((obj transient-post-option-str))
  "Reader function for OBJ, a `transient-post-option-str'.
We add the current value as initial input."
  (let* ((value (transient-infix-value obj))
         (list (split-string value "="))
         (prompt (concat (car list) "=")))
    (read-string prompt (cadr list))))

(provide 'transient-post)
;;; transient-post.el ends here
