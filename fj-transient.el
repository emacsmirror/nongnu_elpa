;;; fj-transient-repo.el --- Transients for fj.el -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <mousebot@disroot.org>
;; Copyright (C) 2024 Marty Hiatt <mousebot@disroot.org>
;;
;; Keywords: git, convenience
;; URL: https://codeberg.org/martianh/fj.el

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
(require 'tp)

;;; AUTOLOADS

(autoload 'fj-patch "fj")
(autoload 'fedi-http--triage "fedi-http")
(autoload 'fj--get-buffer-spec "fj")
(autoload 'fj-get-repo "fj")
(autoload 'fj-get "fj")
(autoload 'fj-put "fj")
(autoload 'fj-get-repo-topics "fj")
(autoload 'fj-get-current-user-settings "fj")

(defvar fj-current-repo)
(defvar fj-user)
(defvar fj-merge-types)

;;; VARIABLES

(defvar fj-choice-booleans '("t" ":json-false")) ;; add "" or nil to unset?

(defvar fj-server-settings nil
  "User or repo settings data (editable) as returned by the instance.")

(defvar fj-diff-style-types '("unified" "split"))

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
    "diff_view_style" ;; "unified" or "split" (undocumented?)
    ))

;;; UTILS

(defun fj-transient-patch (endpoint params)
  "Send a patch request to ENDPOINT with json PARAMS."
  (let* ((resp (fj-patch endpoint params :json))
         (item-str (if (string-prefix-p "user" endpoint) "User" "Repo")))
    (fedi-http--triage resp
                       (lambda (_)
                         (message "%s settings updated!:\n%s"
                                  item-str params)))))

(defun fj-repo-settings-patch (repo owner params)
  "Update settings for REPO by OWNER, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  (let* ((endpoint (format "repos/%s/%s" owner repo)))
    (fj-transient-patch endpoint params)))

(defun fj-user-settings-patch (params)
  "Update user settings, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  (fj-transient-patch "user/settings" params))

(defun fj-repo-editable (repo-alist &optional simple)
  "Remove any un-editable items from REPO-ALIST.
Checking is done against `fj-repo-settings-editable'.
If SIMPLE, then check against `fj-repo-settings-simple'."
  (tp-remove-not-editable
   repo-alist
   (if simple
       fj-repo-settings-simple
     fj-repo-settings-editable)))

(defun fj-user-editable (alist)
  "Return editable fields from ALIST.
Checked against `fj-user-settings-editable'."
  (tp-remove-not-editable alist fj-user-settings-editable))

(defun fj-get-repo-data ()
  "Return repo data from previous buffer spec.
Designed to be used in a transient called from the repo."
  (if (and fj-user fj-current-repo)
      (fj-get-repo fj-current-repo fj-user)
    (with-current-buffer transient--original-buffer
      (let* ((repo (fj--get-buffer-spec :repo))
             (owner (fj--get-buffer-spec :owner)))
        (fj-get-repo repo owner)))))

(defun fj-repo-get-branches (repo owner)
  "Get branches data for REPO by OWNER."
  (let ((endpoint (format "/repos/%s/%s/branches" owner repo)))
    (fj-get endpoint)))

(defun fj-repo-branches-list (repo owner)
  "Return a list of branch names in REPO by OWNER."
  (let ((branches (fj-repo-get-branches repo owner)))
    (cl-loop for b in branches
             collect (alist-get 'name b))))

;;; TRANSIENTS

(transient-define-suffix fj-update-repo (&optional args)
  "Update current repo settings."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'fj-repo-update-settings)))
  (let ((parsed (tp-parse-args-for-send args)))
    ;; FIXME: this is how to do it using transient, but perhaps we want
    ;; `fj-user' and `fj-current-repo' to be global after all
    (with-current-buffer (car (buffer-list)) ;transient--original-buffer
      (let* ((repo (fj--get-buffer-spec :repo))
             (owner (fj--get-buffer-spec :owner)))
        (fj-repo-settings-patch repo owner parsed)))))

(transient-define-suffix fj-update-topics ()
  "Update repo topics on the server.
Provide current topics for adding/removing."
  :transient 'transient--do-return
  (interactive)
  (let* ((endpoint (format "repos/%s/%s/topics" fj-user fj-current-repo))
         (current-topics (mapconcat #'identity (fj-get-repo-topics) " "))
         (topics (read-string
                  "Set repo topics [space separated, no spaces in topics]: "
                  current-topics))
         (list (split-string topics))
         (params `(("topics" . ,list)))
         (resp (fj-put endpoint params :json)))
    (fedi-http--triage resp
                       (lambda (_)
                         (message "Topics updated!\n%s" list)))))

(transient-define-prefix fj-repo-update-settings ()
  "A transient for setting current repo settings."
  :value (lambda ()
           (tp-return-data
            #'fj-get-repo-data fj-repo-settings-simple))
  [:description
   (lambda ()
     (format "Repo settings for %s/%s" fj-user fj-current-repo))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["Repo info"
   ("n" "name" "name" :alist-key name :class tp-option-str)
   ("d" "description" "description" :alist-key description :class tp-option-str)
   ("t" "topics" fj-update-topics)
   ("w" "website" "website" :alist-key website :class tp-option-str)
   ("b" "default branch" "default_branch"
    :class tp-option
    :alist-key default_branch
    :choices (lambda ()
               (fj-repo-branches-list fj-current-repo fj-user)))]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["Repo options"
   ("a" "archived" "archived" :alist-key archived :class tp-bool)
   ("i" "has issues" "has_issues" :alist-key has_issues :class tp-bool)
   ("k" "has wiki" "has_wiki" :alist-key has_wiki :class tp-bool)
   ("p" "has pull_requests" "has_pull_requests"
    :alist-key has_pull_requests :class tp-bool)
   ("o" "has projects" "has_projects" :alist-key has_projects :class tp-bool)
   ("r" "has releases" "has_releases" :alist-key has_releases :class tp-bool)
   ("s" "default merge style" "default_merge_style"
    :class tp-option
    :alist-key default_merge_style
    :choices fj-merge-types) ;; FIXME: broken?
   ("p" "is private" "private" :alist-key private :class tp-bool)]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-repo)
   ("C-x C-k" :info "to revert all changes")]
  (interactive)
  ;; FIXME: bail if we are not authorized to change this repo!
  (if fj-current-repo
      (transient-setup 'fj-repo-update-settings)
    (if (y-or-n-p "No repo. Try to use git config?")
        (if-let* ((repo-+-owner (fj-repo-+-owner-from-git))
                  (fj-current-repo (cadr repo-+-owner)))
            (transient-setup 'fj-repo-update-settings)))))

(transient-define-suffix fj-update-user-settings (&optional args)
  "Update current user settings on the server."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'fj-user-update-settings)))
  (let* ((parsed (tp-parse-args-for-send args)))
    (fj-user-settings-patch parsed)))

(transient-define-prefix fj-user-update-settings ()
  "A transient for setting current user settings."
  :value (lambda ()
           (tp-return-data #'fj-get-current-user-settings
                           fj-user-settings-editable))
  [:description
   (lambda () (format "User settings for %s" fj-user))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["User info"
   ("n" "full name" "full_name" :alist-key full_name :class tp-option-str)
   ("d" "description" "description" :alist-key description :class tp-option-str)
   ("w" "website" "website" :alist-key website :class tp-option-str)
   ("p" "pronouns" "pronouns" :alist-key pronouns :class tp-option-str)
   ("g" "language" "language" :alist-key language :class tp-option-str)
   ("l" "location" "location" :alist-key location :class tp-option-str)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["User options"
   ("a" "hide activity" "hide_activity" :alist-key hide_activity :class tp-bool)
   ("e" "hide email" "hide_email" :alist-key hide_email :class tp-bool)
   ("v"  "diff view style" "diff_view_style" :alist-key diff_view_style :class tp-cycle
    :choices (lambda () fj-diff-style-types)) ;; FIXME: lambdas don't work here?
   ("u" "enable repo unit hints" "enable_repo_unit_hints"
    :alist-key enable_repo_unit_hints
    :class tp-bool)]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-user-settings)
   ("C-x C-k" :info "to revert all changes")]
  (interactive)
  (if (not fj-user)
      (user-error "No user. Set `fj-user'")
    (transient-setup 'fj-user-update-settings)))

(provide 'fj-transient)
;;; fj-transient.el ends here
