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
(require 'transient-post)

;;; AUTOLOADS

(autoload 'fj-patch "fj")
(autoload 'fedi-http--triage "fedi-http")
(autoload 'fj--get-buffer-spec "fj")
(autoload 'fj-get-repo "fj")
(autoload 'fj-get "fj")

(defvar fj-current-repo)
(defvar fj-user)
(defvar fj-merge-types)

;;; VARIABLES

(defvar fj-choice-booleans '("t" ":json-false")) ;; add "" or nil to unset?

(defvar fj-server-settings nil
  "User or repo settings data (editable) as returned by the instance.")

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
                       (lambda ()
                         (message "%s settings updated!:\n%s"
                                  item-str params)))))

(defun fj-repo-settings-patch (repo params)
  "Update settings for REPO, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  (let* ((endpoint (format "repos/%s/%s" fj-user repo)))
    (fj-transient-patch endpoint params)))

(defun fj-user-settings-patch (params)
  "Update user settings, sending a PATCH request.
PARAMS is an alist of any settings to be changed."
  (fj-transient-patch "user/settings" params))

(defun fj-repo-editable (repo-alist &optional simple)
  "Remove any un-editable items from REPO-ALIST.
Checking is done against `fj-repo-settings-editable'.
If SIMPLE, then check against `fj-repo-settings-simple'."
  (transient-post-remove-not-editable repo-alist
                                      fj-repo-settings-editable
                                      (when simple fj-repo-settings-simple)))

(defun fj-user-editable (alist)
  "Return editable fields from ALIST.
Checked against `fj-user-settings-editable'."
  (transient-post-remove-not-editable alist fj-user-settings-editable))

(defun fj-get-repo-data ()
  "Return repo data from previous buffer spec.
Designed to be used in a transient called from the repo."
  (if (and fj-user fj-current-repo)
      (fj-get-repo fj-current-repo fj-user)
    (with-current-buffer (car (buffer-list)) ; last buffer
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
  (let* ((alist (transient-post-transient-to-alist args))
         (only-changed (transient-post-only-changed-args alist))
         (bools-converted (transient-post-bool-strs-to-json only-changed)))
    (fj-repo-settings-patch
     ;; FIXME: need to use global vars in transients?:
     fj-current-repo bools-converted)))

(transient-define-suffix fj-update-topics ()
  "Update repo topics on the server.
Provide current topics for adding/removing."
  :transient 'transient--do-exit
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
                       (lambda ()
                         (message "Topics updated!\n%s" list)))))

(transient-define-prefix fj-repo-update-settings ()
  "A transient for setting current repo settings."
  :value (lambda ()
           (transient-post-return-data
            #'fj-get-repo-data fj-repo-settings-simple))
  [:description
   (lambda ()
     (format "Repo settings for %s/%s" fj-user fj-current-repo))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["Repo info"
   ("n" "name" "name=" :class transient-post-option-str)
   ("d" "description" "description=" :class transient-post-option-str)
   ("w" "website" "website=" :class transient-post-option-str)
   ("b" "default_branch" "default_branch="
    :class fj-option
    :choices (lambda ()
               (fj-repo-branches-list fj-current-repo fj-user)))]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["Repo options"
   ("a" "archived" "archived=" :class transient-post-choice-bool)
   ("i" "has_issues" "has_issues=" :class transient-post-choice-bool)
   ("k" "has_wiki" "has_wiki=" :class transient-post-choice-bool)
   ("p" "has_pull_requests" "has_pull_requests=" :class transient-post-choice-bool)
   ("o" "has_projects" "has_projects=" :class transient-post-choice-bool)
   ("r" "has_releases" "has_releases=" :class transient-post-choice-bool)
   ("s" "default_merge_style" "default_merge_style="
    :class fj-option
    :choices (lambda () fj-merge-types))] ;; FIXME: broken?
  ["Topics"
   ("t" "update topics" fj-update-topics)]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-repo)
   ("C-c C-k" :info "to revert all changes")]
  (interactive)
  (if (not fj-current-repo)
      (user-error "No repo. Call from a repo view or set `fj-current-repo'")
    (transient-setup 'fj-repo-update-settings)))

(transient-define-suffix fj-update-user-settings (&optional args)
  "Update current user settings on the server."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'fj-user-update-settings)))
  (let* ((alist (transient-post-transient-to-alist args))
         (only-changed (transient-post-only-changed-args alist))
         (bools-converted (transient-post-bool-strs-to-json only-changed)))
    (fj-user-settings-patch ;;only-changed)))
     bools-converted)))

(transient-define-prefix fj-user-update-settings ()
  "A transient for setting current user settings."
  :value (lambda ()
           (transient-post-return-data #'fj-get-current-user-settings
                                       fj-user-settings-editable))
  [:description
   (lambda () (format "User settings for %s" fj-user))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["User info"
   ("n" "full name" "full_name=" :class transient-post-option-str)
   ("d" "description" "description=" :class transient-post-option-str)
   ("w" "website" "website=" :class transient-post-option-str)
   ("p" "pronouns" "pronouns=" :class transient-post-option-str)
   ("g" "language" "language=" :class transient-post-option-str)
   ("l" "location" "location=" :class transient-post-option-str)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["User options"
   ("a" "hide_activity" "hide_activity=" :class transient-post-choice-bool)
   ("e" "hide_email" "hide_email=" :class transient-post-choice-bool)
   ("v"  "diff_view_style" "diff_view_style=" :class transient-post-choice-bool
    :choices ("unified" "split")) ;; FIXME: lambdas don't work here?
   ("u" "enable_repo_unit_hints" "enable_repo_unit_hints="
    :class transient-post-choice-bool)]
  ["Update"
   ("C-c C-c" "Save settings" fj-update-user-settings)
   ("C-c C-k" :info "to revert all changes")]
  (interactive)
  (if (not fj-user)
      (user-error "No user. Set `fj-user'")
    (transient-setup 'fj-user-update-settings)))

(provide 'fj-transient)
;;; fj-transient.el ends here
