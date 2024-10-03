;;; tp.el --- Transient utilities for posting to an API -*- lexical-binding: t; -*-

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

(defvar tp-convert-json-booleans-to-strings t
  "Whether to convert JSON booleans.
When fetching data, parsed JSON booleans, e.g. t and :json-false,
will be converted into the strings \"true\" and \"false\".")

;;; VARIABLES

(defvar tp-choice-booleans-json '("t" ":json-false")
  ;; add "" or nil to unset?
  "JSON Booleans formatted as parsed elisp and into a string.")

(defvar tp-choice-booleans '("true" "false")
  "Boolean strings for sending non-JSON requests.") ;; add "" or nil to unset?

(defvar tp-server-settings nil
  "Settings data (editable) as returned by the server.")

(defvar tp-settings-as-transient nil
  "Settings data converted to transient args.")

;;; UTILS

(defun tp-transient-to-alist (args)
  "Convert list of transient ARGS into an alist.
This currently assumes arguments are of the form \"key=value\".
Should return an alist that can be parsed as JSON data."
  (cl-loop for a in args
           for split = (split-string a "=")
           for key = (if (= (length split) 1) ; we didn't split = boolean
                         (car split)
                       (concat (car split) "="))
           for val = (transient-arg-value key args)
           for value = (cond ((member val tp-choice-booleans-json)
                              (intern val))
                             ((equal val "\"\"")
                              "") ;; POSTable empty string
                             (t val))
           collect (cons (car split) value)))

(defun tp-alist-to-transient (alist &optional prefix)
  "Convert ALIST to a list of transient args.
Returns transient arguments in the form \"key=value\".
PREFIX is a string and is used during recursion.
Should work with JSON arrays as both lists and vectors."
  (flatten-tree
   (cl-loop for a in alist
            ;; car recur if:
            if (and (proper-list-p (seq-first a)) ;; car isn't just a json cons
                    (> (length (seq-first a)) 1)) ;; car's cdr isn't nil
            do (tp-alist-to-transient (seq-first a) prefix)
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
                                collect (tp-alist-to-transient x k))
                     ;; recur on cdr normal list:
                     (tp-alist-to-transient v k)))
                  (t v))
            collect (if (stringp val)
                        (concat k "=" val)
                      val))))

(defun tp-remove-not-editable (alist var)
  "Remove non-editable fields from ALIST.
Check against the fields in VAR, which should be a list of strings."
  (cl-remove-if-not
   (lambda (x)
     (member (symbol-name (car x))
             var))
   alist))

(defun tp-return-data (fetch-fun &optional editable-var field)
  "Return data to populate current settings.
Call FETCH-FUN with zero arguments to GET the data. Cull the data
with `tp-remove-not-editable', bind the result to
`tp-server-settings', then call
`tp-alist-to-transient' on it and return the result.
EDITABLE-VAR is a variable containing a list of strings
corresponding to the editable fields of the JSON data returned.
See `tp-remove-not-editable'.
FIELD is a JSON field to set `tp-server-settings' to, fetched
with `alist-get'."
  (let* ((data (funcall fetch-fun))
         (editable (if editable-var
                       (tp-remove-not-editable data editable-var)
                     data))
         (bools-parsed (if tp-convert-json-booleans-to-strings
                           (tp-bools-to-strs editable)
                         editable)))
    ;; used in `tp-arg-changed-p' and `tp-only-changed-args'
    (setq tp-server-settings
          (if field
              (alist-get field bools-parsed)
            bools-parsed)
          tp-settings-as-transient
          (tp-alist-to-transient bools-parsed))))

(defun tp-get-server-val (arg)
  "Return the server value for ARG.
If ARG has dotted notation, drill down into the alist. Currently
only one level of nesting is supported, ie \"top.next=val\"."
  ;; TODO: perhaps a way to fix this us is for it to take an let-alist
  ;; .dotted.notation argument?
  (let ((split (split-string arg "\\.")))
    (cond ((= 1 (length split))
           (alist-get (intern arg) ;; no dotted nesting:
                      tp-server-settings))
          ((= 2 (length split)) ;; 1 level of nesting:
           (alist-get (intern (cadr split))
                      (alist-get (intern (car split))
                                 tp-server-settings)))
          (t nil)))) ;; (message "Unable to compare value with server.")))))

(defun tp-only-changed-args (alist)
  "Remove elts from ALIST if value is changed.
Values are considered changed if they do not match those in
`tp-server-settings'. Nil values are also removed if they
match the empty string."
  (prog1
      (cl-remove-if
       (lambda (x)
         (let* ((split (split-string (car x) "\\."))
                (server-val
                 (if (< 1 (length split))
                     ;; FIXME: handle arbitrary nesting:
                     (alist-get (intern (cadr split))
                                (alist-get (intern (car split))
                                           tp-server-settings))
                   (alist-get (intern (car x))
                              tp-server-settings))))
           (cond ((not (cdr x)) (equal "" server-val))
                 (t (equal (cdr x) server-val)))))
       alist)
    ;; unset our vars (comment if need to inspect):
    (setq tp-server-settings nil
          tp-settings-as-transient nil)))

(defun tp-bool-to-str (cons)
  "Convert CONS, into a string boolean if it is either t or :json-false.
Otherwise just return CONS."
  (if (not (consp cons))
      cons
    (cons (car cons)
          (cond ((eq :json-false (cdr cons)) "false")
                ((eq t (cdr cons)) "true")
                (t (cdr cons))))))

(defun tp-bool-str-to-json (cons)
  "Convert CONS, into a string boolean if it is either t or :json-false.
Otherwise just return CONS."
  ;; do nothing if not a cons: `-tree-map' doesn't handle parsed JSON well
  (if (not (consp cons))
      cons
    (cons (car cons)
          (cond ((equal "false" (cdr cons)) :json-false)
                ((equal "true" (cdr cons)) t)
                (t (cdr cons))))))

(defun tp-tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure.
This is just `-tree-map'."
  (declare (important-return-value t))
  (cond
   ((null tree) ())
   ;; the smallest element this operates on is not a list item, but a cons
   ;; pair, e.g. (a . b):
   ((nlistp (cdr-safe tree)) ;; ie `-cons-pair?'
    (funcall fn tree))
   ((consp tree)
    (mapcar (lambda (x) (tp-tree-map fn x)) tree))
   ((funcall fn tree))))

(defun tp-bools-to-strs (alist)
  "Convert values in ALIST to string booleans if they are JSON booleans."
  (tp-tree-map
   #'tp-bool-to-str alist))

(defun tp-bool-strs-to-json (alist)
  "Convert values in ALIST to string booleans if they are JSON booleans."
  (tp-tree-map
   #'tp-bool-str-to-json alist))

(defun tp-dots-to-arrays (alist)
  "Convert keys in ALIST tp dot annotation to array[key] annotation."
  ;; FIXME: handle multi dots?
  (cl-loop for a in alist
           collect (cons (tp-dot-to-array (car a))
                         (cdr a))))

(defun tp-dot-to-array (key)
  "Convert KEY from tp dot annotation to array[key] annotation."
  ;; FIXME: for multi dots, just return secondlast[last]?
  (let* ((split (split-string key "\\.")))
    (if (< 1 (length split))
        (concat (car (last split 2)) "[" (car (last split)) "]")
      key)))

(defun tp--get-choices (obj)
  "Return the value contained in OBJ's choices-slot.
It might be a symbol, in which case evaluate it, a function, in
which case call it. else just return it."
  (let ((slot (oref obj choices)))
    (cond ((functionp slot)
           (funcall slot))
          ((symbolp slot)
           (eval slot))
          (t slot))))

;; CLASSES

(defclass tp-option (transient-option)
  ((always-read :initarg :always-read :initform t))
  "An infix option class for our options.
We always read.")

(defclass tp-option-str (tp-option)
  ((format :initform " %k %d %v"))
  "An infix option class for our option strings.
We always read, and our reader provides initial input from
default/current values.")

(defclass tp-bool (tp-option)
  ((format :initform " %k %d %v")
   (choices :initarg :choices :initform
            ;; '(lambda ()
            'tp-choice-booleans))
  "An option class for our choice booleans.
We implement this as an option because we need to be able to
explicitly send true/false values to the server, whereas
transient ignores false/nil values.")

;;; TRANSIENT METHODS
;; for `tp-bool' we define our own infix option that displays
;; [t|:json-false] like exclusive switches. activating the infix just
;; moves to the next option.

(cl-defmethod transient-init-value ((obj tp-bool))
  "Initiate the value of OBJ, fetching the value from the parent prefix."
  (let* ((arg (oref obj argument))
         (val (transient-arg-value arg (oref transient--prefix value))))
    ;; FIXME: don't understand why we don't want to set a key=val pair here:
    ;; while in fj-transient.el we set it as a key=val
    (oset obj value ;; (concat arg "=" val))))
          val)))

(cl-defmethod transient-format-value ((obj tp-option))
  "Format the value of OBJ.
Format should just be a string, highlighted green if it has been
changed from the server value."
  (let* ((pair (transient-infix-value obj))
         (value (when pair (cadr (split-string pair "=")))))
    (if (not pair)
        ""
      (propertize value
                  'face (if (tp-arg-changed-p obj pair)
                            'transient-value
                          'transient-inactive-value)))))

(defun tp-active-face-maybe (obj pair)
  "Return a face spec based on OBJ's value and PAIR."
  (let* ((value (transient-infix-value obj))
         (active-p (equal pair value))
         (changed-p (tp-arg-changed-p obj pair)))
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

(cl-defmethod transient-format-value ((obj tp-bool))
  "Format the value of OBJ.
Format should be like \"[opt1|op2]\", with the active option highlighted.
The value currently on the server should be underlined."
  (let* ((arg (oref obj argument))
         (choices (tp--get-choices obj)))
    (concat
     (propertize "["
                 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (let ((pair (concat arg choice)))
          (propertize
           choice
           'face (tp-active-face-maybe obj pair))))
      choices
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj tp-bool))
  "Cycle through the possible values of OBJ."
  (let* ((pair (transient-infix-value obj))
         (val (cadr (split-string pair "=")))
         (choices (tp--get-choices obj)))
    ;; FIXME: don't understand why we don't want to set a key=val pair
    ;; here: while in fj-transient.el we set it as a key=val:
    ;; (concat arg
    (if (equal val (car (last choices)))
        (car choices)
      (cadr (member val choices)))))

;; FIXME: see the `transient-infix-read' method's docstring:
;; we should preserve history, follow it. maybe just mod it.
(cl-defmethod transient-infix-read ((obj tp-option-str))
  "Reader function for OBJ.
We add the current value as initial input."
  (let* ((value (transient-infix-value obj))
         (list (split-string value "="))
         (prompt (concat (car list) "=")))
    (read-string prompt (cadr list))))

(cl-defmethod transient-infix-read ((obj tp-option))
  "Cycle through the possible values of OBJ."
  (let* ((pair (transient-infix-value obj))
         (split (split-string pair "="))
         (choices (tp--get-choices obj)))
    ;; FIXME: don't understand why we don't want to set a key=val pair
    ;; here: while in fj-transient.el we set it as a key=val:
    ;; (concat arg
    (completing-read (concat (car split) "=")
                     choices nil :match)))

;;; OUR METHODS

(cl-defgeneric tp-arg-changed-p (obj pair)
  "T if value of PAIR is different to the value in `tp-server-settings'.
The format of the value is a transient pair as a string, ie \"key=val\".
Nil values will also match the empty string.
OBJ is the object whose args are being checked.")

(cl-defmethod tp-arg-changed-p ((_obj tp-option) pair)
  "T if value of PAIR is different to the value in `tp-server-settings'.
The format of the value is a transient pair as a string, ie \"key=val\".
Nil values will also match the empty string.
OBJ is the object whose args are being checked."
  (let* ((split (split-string pair "="))
         (server-val (tp-get-server-val (car split)))
         (server-str (if (and server-val
                              (symbolp server-val))
                         (symbol-name server-val)
                       server-val)))
    (cond ((not (cadr split))
           (not (equal "" server-str)))
          ;; NB: it is better to return false positive here rather than
          ;; false negative, so we do not check that we successfully
          ;; fetched server-str. for if we check for the string and it's
          ;; nil, we will always return nil, meaning that even after a
          ;; value is changed it will not be propertized. better to
          ;; propertize values whether or not they're changed rather than
          ;; to not propertize changed values.
          (t ;; (and server-str
           (not (equal (cadr split)
                       server-str))))))

(provide 'tp)
;;; tp.el ends here
