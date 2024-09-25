;;; fj-transient-repo.el --- Generate a transient from repo JSON -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Copyright (C) 2024 Marty Hiatt <martianhiatus AT riseup.net>
;;
;; Package-Requires: ((emacs "28.1") (fedi "0.2"))
;; Keywords: git, convenience
;; URL: https://codeberg.org/martianh/fj.el
;; Version: 0.2
;; Separator: -

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

;;; Code:
(autoload 'fj-get-repo-data "fj.el")
(autoload 'fj-alist-to-transient "fj.el")
(autoload 'fj-repo-editable "fj.el")
(autoload 'fj-opt-inits "fj.el")

(defvar fj-repo-settings-strings nil)
(defvar fj-repo-settings-simple-booleans nil)

(transient-define-prefix fj-repo-settings '()
  "A prefix for setting repo settings."
  :value (lambda ()
           (fj-repo-value-fun))
  ["Repo info"
   ;; :class transient-group
   :setup-children
   (lambda (_)
     (fj-setup-children-strings))]
  ["Repo options"
   :setup-children
   (lambda (_)
     (fj-setup-children-bools))]
  ["Update"
   ("C-c C-c" "send" fj-update-repo)])

(defun fj-repo-value-fun ()
  "Return current (editable) repo settings as transient values."
  (let ((data (fj-get-repo-data)))
    (fj-alist-to-transient
     (fj-repo-editable data :simple))))

(defun fj-setup-children-strings ()
  "Return a setup-children function for a transient.
We fetch repo data ourselves, convert it into transient options,
then parse it."
  (let* (;; not sure if this is rly needed?
         (objs (cl-loop for x in fj-repo-settings-strings
                        collect (eval `(fj-gen-transient-infix
                                        ,x nil
                                        'transient-option nil
                                        'fj-repo-settings-str-reader))))
         (list (fj-return-options-list objs)))
    (transient-parse-suffixes 'transient--prefix list)))

(defun fj-setup-children-bools ()
  "Return a setup-children function for a transient.
We fetch repo data ourselves, convert it into transient options,
then parse it."
  (let* ((objs (cl-loop for x in fj-repo-settings-simple-booleans
                        collect (eval `(fj-gen-transient-infix
                                        ,x nil
                                        'transient-option fj-choice-booleans))))
         (list (fj-return-options-list objs)))
    (transient-parse-suffixes 'transient--prefix list)))

(defun fj-return-options-list (list)
  "Return a list of options for each object in LIST."
  (cl-loop for x in list
           collect (fj-return-option-list x)))

(defun fj-return-option-list (opt)
  "Return a list of transient suffix options for object OPT."
  (list (oref opt command)
        ;; these really only need to be set if they are not set in the object OPT:
        :key (oref opt key)
        :description (oref opt description)
        :class transient-option
        :argument (oref opt argument)))

(defmacro fj-gen-transient-infix (name &optional binding class
                                       choices reader-fun no-always-read)
  "Generate a transient infix based on NAME, a string.
Optionally provide BINDING, CLASS, CHOICES, a READER-FUN, and
specify NO-ALWAYS-READ."
  (declare (debug t))
  `(transient-define-infix ,(intern name) ()
     "A prefix for setting repo settings."
     :class ,(or class transient option)
     :transient t
     :key ,(or binding (fj-opt-inits name))
     :argument ,(concat name "=")
     :description ,name
     ,@(when choices
         `(:choices (lambda () ,choices)))
     ,@(when reader-fun
         `(:reader (lambda (prompt initial-input history)
                     (funcall ,reader-fun prompt initial-input history))))
     ,@(unless no-always-read
         `(:always-read t))))

(provide 'fj-transient-repo)
;;; fj-transient-repo.el ends here
