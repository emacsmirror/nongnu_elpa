;;; fj-transient-repo.el --- Transients for fj.el -*- lexical-binding: t; -*-

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

;; Transient menus for updating repository and user settings.

;;; Code:

(require 'transient)
(require 'json)

;;; AUTOLOADS

(autoload 'fj-patch "fj")
(autoload 'fedi-http--triage "fedi-http")
(autoload 'fj--get-buffer-spec "fj")
(autoload 'fj-get-repo "fj")
(autoload 'fj-get "fj")

(defvar fj-current-repo nil)
(defvar fj-user nil)

(defvar fj-repo-settings-strings nil)
(defvar fj-repo-settings-simple-booleans nil)
(defvar fj-choice-booleans '("t" ":json-false")) ;; add "" or nil to unset?
(defvar fj-merge-types)

;;; VARIABLES

(defvar fj-repo-settings-editable
  '( ;; boolean:
    "allow_fast_forward_only_merge"
    "allow_manual_merge"
    "allow_merge_commits"
    "allow_rebase"
    "allow_rebase_explicit"
    "allow_rebase_update"
    "allow_squash_merge"
    "archived"
    "autodetect_manual_merge"
    "default_allow_maintainer_edit"
    "default_delete_branch_after_merge"
    "enable_prune"
    "globally_editable_wiki"
    "has_actions"
    "has_issues"
    "has_packages"
    "has_projects"
    "has_pull_requests"
    "has_releases"
    "has_wiki"
    "ignore_whitespace_conflicts"
    "private"
    "template"
    ;; complex ones (skip for now):
    ;; "external_tracker" ; complex
    ;; "external_wiki" ; complex
    ;; "internal_tracker" ; complex
    ;; strings:
    "name"
    "website"
    "description"
    "default_branch"
    "wiki_branch"
    "mirror_interval" ; sha
    "default_merge_style")) ;; member `fj-merge-types'

(defvar fj-repo-settings-simple
  '( ;; boolean:
    "archived"
    "has_issues"
    "has_projects"
    "has_pull_requests"
    "has_releases"
    "has_wiki"
    ;; "private"
    ;; "template"
    ;; strings:
    "name"
    "website"
    "description"
    "default_branch"
    ;; "wiki_branch"
    ;; "mirror_interval" ; sha
    "default_merge_style")) ;; member `fj-merge-types'

(defvar fj-user-settings-editable
  '(;; strings:
    "description"
    "full_name"
    "language"
    "location"
    "pronouns"
    ;; "theme" ;; web UI
    "website"
    ;; booleans:
    "enable_repo_unit_hints"
    "hide_activity"
    "hide_email"
    ;; enums:
    "diff_view_style" ;; enum, but what? not in API docs
    ;; "unified" is my current setting, can't find it in web UI to change.
    ))

(defvar fj-user-settings-editable-strings
  '("description"
    "full_name"
    "language"
    "location"
    "pronouns"
    ;; "theme" ;; web UI
    "website"))

;;; UTILS

(defun fj-repo-settings-patch (repo params)
  "Update settings for REPO, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  ;; NB: we only need params that we are updating
  (let* ((endpoint (format "repos/%s/%s" fj-user repo))
         (resp (fj-patch endpoint params :json)))
    (fedi-http--triage resp
                       (lambda ()
                         (message "Repo settings updated!:\n%s"
                                  params)))))

(defun fj-user-settings-patch (params)
  "Update user settings, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  ;; NB: we only need params that we are updating
  (let* ((endpoint "user/settings")
         (resp (fj-patch endpoint params :json)))
    (fedi-http--triage resp
                       (lambda ()
                         (message "User settings updated!:\n%s"
                                  params)))))

(defun fj-transient-to-alist (args)
  "Convert list of transient ARGS into an alist.
This currently assumes arguments are of the form \"key=value\"."
  (cl-loop for a in args
           for split = (split-string a "=")
           for key = (if (= (length split) 1) ; we didn't split = boolean
                         (car split)
                       (concat (car split) "="))
           for val = (transient-arg-value key args)
           for value = (cond ((member val fj-choice-booleans)
                              (intern val))
                             ((equal val "\"\"")
                              "") ;; POSTable empty string
                             (t
                              val))
           collect (cons (car split) value)))

(defun fj-alist-to-transient (alist)
  "Convert ALIST to a list of transient args.
This currently assumes arguments are of the form \"key=value\"."
  (cl-loop for a in alist
           for key = (symbol-name (car a))
           for v = (cdr a)
           for val = (cond ((numberp v) (number-to-string v))
                           ((symbolp v) (symbol-name v))
                           ((listp v) nil) ;; don't handle nesting yet
                           (t v))
           collect (concat key "=" val)))

(defun fj-remove-not-editable (alist editable-var &optional simple-var)
  "Remove non-editable fields from ALIST.
Check against EDITABLE-VAR, or, if present, SIMPLE-VAR."
  (cl-remove-if-not
   (lambda (x)
     (member (symbol-name (car x))
             (or simple-var
                 editable-var)))
   alist))

(defun fj-repo-editable (repo-alist &optional simple)
  "Remove any un-editable items from REPO-ALIST.
Checking is done against `fj-repo-settings-editable'.
If SIMPLE, then check against `fj-repo-settings-simple'."
  (fj-remove-not-editable repo-alist
                          fj-repo-settings-editable
                          (when simple fj-repo-settings-simple)))

(defun fj-user-editable (alist)
  "Return editable fields from ALIST.
Checked against `fj-user-settings-editable'."
  (fj-remove-not-editable alist
                          fj-user-settings-editable))

(defun fj-get-repo-data ()
  "Return repo data from previous buffer spec.
Designed to be used in a transient called from the repo."
  (with-current-buffer (car (buffer-list)) ; last buffer
    (let* ((repo (fj--get-buffer-spec :repo))
           (owner (fj--get-buffer-spec :owner)))
      (fj-get-repo repo owner))))

(defun fj-repo-defaults ()
  "Return the current repo setting values.
Used for default values in `fj-repo-update-settings'."
  ;; FIXME: looks like the only way we can access data is through
  ;; global varaibles? we need to access repo JSON in transients
  ;; (for defaults)
  (let* ((data (fj-get-repo-data))
         (editable (fj-repo-editable data :simple)))
    (fj-alist-to-transient editable)))

(defun fj-user-settings-current ()
  "Return current user settings that are editable."
  (let* ((data (fj-get-current-user-settings))
         (editable (fj-user-editable data)))
    (fj-alist-to-transient editable)))

(defun fj-repo-get-branches (repo owner)
  "Get branches data for REPO by OWNER."
  (let ((endpoint (format "/repos/%s/%s/branches" owner repo)))
    (fj-get endpoint)))

(defun fj-repo-branches-list (repo owner)
  "Return a list of branch names in REPO by OWNER."
  (let ((branches (fj-repo-get-branches repo owner)))
    (cl-loop for b in branches
             collect (alist-get 'name b))))

;;; TRANSIENT FUNCTIONS

(defun fj-str-reader (&optional prompt _initial-input _history)
  "Reader function for `fj-repo-update-settings' string options.
We populate the minibuffer with an initial input taken from the
transient's default value.
PROMPT, INITIAL-INPUT and HISTORY are default transient reader args."
  (let ((vals (oref transient--prefix value)))
    ;; (list (transient-args (or prefix transient-current-command))))
    (read-string prompt
                 (transient-arg-value prompt vals))))

;;; TRANSIENTS

(transient-define-suffix fj-update-repo (&optional args)
  "Update current repo settings."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'fj-repo-update-settings)))
  (let* (;;(args (transient-args (oref transient-current-prefix command)))
         (alist (fj-transient-to-alist args)))
    (message "%s %s %s" args alist (json-encode alist))
    (fj-repo-settings-patch
     ;; FIXME: need to use global vars in transients?:
     fj-current-repo alist)))

(transient-define-prefix fj-repo-update-settings ()
  "A transient for setting current repo settings."
  :value (lambda () (fj-repo-defaults))
  ["Repo settings"
   (:info (lambda () (format "Owner: %s" fj-user)))
   (:info (lambda () "Note: use the empty string (\"\") to remove a value from an option."))]
  ;; strings
  ["Repo info"
   ("-n" "name" "name=" :class fj-option)
   ("-d" "description" "description=" :class fj-option)
   ("-w" "website" "website=" :class fj-option)
   ("-b" "default_branch" "default_branch="
    :choices (lambda ()
               (fj-repo-branches-list fj-current-repo fj-user))
    :always-read t)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["Repo options"
   ("-a" "archived" "archived=" :class fj-choice-boolean)
   ("-i" "has_issues" "has_issues=" :class fj-choice-boolean)
   ("-k" "has_wiki" "has_wiki=" :class fj-choice-boolean)
   ("-pr" "has_pull_requests" "has_pull_requests=" :class fj-choice-boolean)
   ("-hp" "has_projects" "has_projects=" :class fj-choice-boolean)
   ("-hr" "has_releases" "has_releases=" :class fj-choice-boolean)
   ("-s" "default_merge_style" "default_merge_style="
    :always-read t
    :choices (lambda ()
               fj-merge-types))]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-repo)
   (:info (lambda ()
            "C-c C-k to revert all changes"))])

(transient-define-suffix fj-update-user-settings (&optional args)
  "Update current user settings."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'fj-user-update-settings)))
  (let* (;;(args (transient-args (oref transient-current-prefix command)))
         (alist (fj-transient-to-alist args)))
    (message "%s %s %s" args alist (json-encode alist))
    (fj-user-settings-patch alist)))

(transient-define-prefix fj-user-update-settings ()
  "A transient for setting current user settings."
  :value (lambda () (fj-user-settings-current))
  ["User settings"
   (:info (lambda () (format "for %s" fj-user)))
   (:info (lambda () "Note: use the empty string (\"\") to remove a value from an option."))]
  ;; strings
  ["User info"
   ("-n" "full name" "full_name=" :class fj-option)
   ("-d" "description" "description=" :class fj-option)
   ("-w" "website" "website=" :class fj-option)
   ("-p" "pronouns" "pronouns=" :class fj-option)
   ("-g" "language" "language=" :class fj-option)
   ("-l" "location" "location=" :class fj-option)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["User options"
   ("-ha" "hide_activity" "hide_activity=" :class fj-choice-boolean)
   ("-he" "hide_email" "hide_email=" :class fj-choice-boolean)
   ("-vs"  "diff_view_style" "diff_view_style=" :class fj-choice-boolean)
   ("-ruh" "enable_repo_unit_hints" "enable_repo_unit_hints=" :class fj-choice-boolean)]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-user-settings)
   (:info (lambda ()
            "C-c C-k to revert all changes"))])

;; CLASSES

(defclass fj-option (transient-option)
  ((reader :initarg :reader :initform
           (lambda (prompt initial-input history)
             (funcall 'fj-str-reader prompt initial-input history)))
   (always-read :initarg :always-read :initform t))
  "An infix option class for our options.
We always read, and our reader provides initial input from default values.")

(defclass fj-choice-boolean (transient-option)
  ((choices :initarg :choices :initform (lambda () fj-choice-booleans))
   (always-read :initarg :always-read :initform t))
  "An infix option class for our choice booleans.
We implement this class because we need to be able to explicitly
send nil values to the server, not just ignore nil values")

(provide 'fj-transient)
;;; fj-transient.el ends here
