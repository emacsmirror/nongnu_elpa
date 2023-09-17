;;; fj.el --- Client for forgejo instances -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Copyright (C) 2023 Marty Hiatt <martianhiatus AT riseup.net>
;;
;; Package-Requires: ((emacs "28.1") (fedi "0.1"))
;; Keywords: git, convenience
;; URL: https://codeberg.org/martianh/fj.el
;; Version: 0.1
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

;; Some bare-bones commands for interacting with Forgejo instances.

;; To use these functions, first set `fj-token' and `fj-user', and `fj-host'.

;;; Code:

(require 'fedi)
(require 'fedi-post)
(require 'magit-git)

;;; VARIABLES

(defvar fj-token nil)

(defvar fj-user nil)

(defvar fj-host "https://codeberg.org")

(defun fj-api (endpoint)
  "Return a URL for ENDPOINT."
  (fedi-http--api endpoint fj-host "v1"))

;;; REQUESTS

(defmacro fj-authorized-request (method body &optional unauthenticated-p)
  "Make a METHOD type request using BODY, with token authorization.
Unless UNAUTHENTICATED-P is non-nil.
Requires `fj-token' to be set."
  (declare (debug 'body)
           (indent 1))
  `(let ((url-request-method ,method)
         (url-request-extra-headers
          (unless ,unauthenticated-p
            (list (cons "Authorization"
                        (concat "token " fj-token))))))
     ,body))

;; (defvar fj-user-data (fj-get "user"))

;; (setq fj-user (alist-get 'login fj-user-data))

(defun fj-get (endpoint)
  "Make a GET request to ENDPOINT."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "GET"
      (fedi-http--get-json url))))

(defun fj-post (endpoint params)
  "Make a POST request to ENDPOINT.
PARAMS."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "POST"
      (fedi-http--post url params nil :json))))

(defun fj-patch (endpoint &optional params json)
  "Make a PATCH request to ENDPOINT.
PARAMS.
JSON."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "PATCH"
      (fedi-http--patch-json url params json))))

(defun fj-delete (endpoint)
  "Make a DELETE request to ENDPOINT."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "DELETE"
      (fedi-http--delete url))))

;;; USER

(defun fj-get-user ()
  "Return the data for the current user."
  (fj-get "user"))

;;; REPOS

(defun fj-current-dir-repo ()
  "If we are in a repository, return its name."
  (if (magit-git-repo-p default-directory)
      (file-name-nondirectory
       (directory-file-name default-directory))))
    ;; (fj-read-user-repo)))

(defun fj-get-repos ()
  "Return the user's repos."
  (let ((endpoint "user/repos"))
    (fj-get endpoint)))

(defun fj-get-repo-candidates (repos)
  "Return REPOS as completion candidates."
  (cl-loop for r in repos
           collect `(,(alist-get 'name r)
                     ,(alist-get 'id r)
                     ,(alist-get 'description r))))

(defun fj-read-user-repo (&optional force)
  "Read a repo in the minibuffer.
If we are in a repo, return it instead, unless FORCE."
  (let ((current (fj-current-dir-repo)))
    (if (and (not force)
             (not current-prefix-arg)
             current)
        current
      (let* ((repos (fj-get-repos))
             (cands (fj-get-repo-candidates repos)))
        (completing-read "Repo: " cands)))))

(defun fj-create-repo ()
  "Create a new repo."
  (interactive)
  (let* ((name (read-string "Repo name: "))
         (desc (read-string "Repo description: "))
         (params `(("name" . ,name)
                   ("description" . ,desc)))
         (response (fj-post "user/repos" params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "Repo %s created!" name)))))

;;; ISSUES

(defun fj-get-issue-candidates (issues)
  "Return a list of ISSUES as completion candidates."
  (cl-loop for i in issues
           collect `(,(alist-get 'title i)
                     ,(alist-get 'number i)
                     ,(alist-get 'id i))))

(defun fj-read-repo-issue (repo)
  "Given REPO, read an issue in the minibuffer.
Return the issue number."
  (let* ((issues (fj-repo-get-issues repo))
         (cands (fj-get-issue-candidates issues))
         (choice (completing-read "Issue: " cands))
         (item
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr item)))

(defun fj-repo-get-issues (&optional repo)
  "Return issues for REPO."
  (let* ((repo (or repo (fj-read-user-repo)))
         (endpoint (format "repos/%s/%s/issues" fj-user repo)))
    (fj-get endpoint)))

(defun fj-get-issue (&optional repo issue)
  "GET ISSUE in REPO.
ISSUE is a number."
  ;; (fj-with-repo nil nil
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-issue-create (&optional repo)
  "Create an issue in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (url (format "repos/%s/%s/issues" fj-user repo))
         (title (read-string "Title: "))
         (body (read-string "Body: "))
         (params `(("body" . ,body)
                   ("title" . ,title)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue %s created!" title)))))

(defun fj-issue-patch (repo issue params)
  "PATCH/Edit ISSUE in REPO.
With PARAMS."
  (let* ((endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-patch endpoint params :json)))

(defun fj-issue-edit (&optional repo issue)
  "Edit ISSUE body in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-issue repo issue))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit issue body: " old-body))
         (response (fj-issue-patch repo issue `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue edited!")))))

(defun fj-issue-close (&optional repo issue)
  "Close ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (y-or-n-p "Close issue?")
      (let ((response (fj-issue-patch repo issue
                                      `(("state" . "closed")))))
        (fedi-http--triage response
                           (lambda ()
                             (message "issue closed!")))))))

(defun fj-issue-delete (&optional repo issue)
  "Delete ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (y-or-n-p "Delete issue?")
      (let* ((url (format "repos/%s/%s/issues/%s" fj-user repo issue))
             (response (fj-delete url)))
        (fedi-http--triage response
                           (lambda ()
                             (message "issue deleted!")))))))

;;; COMMENTS

(defun fj-get-comment-candidates (comments)
  "Return a list of COMMENTS as completion candidates."
  (cl-loop for c in comments
           collect `(,(alist-get 'body c)
                     ,(alist-get 'id c))))

(defun fj-read-issue-comment (repo issue)
  "Given ISSUE in REPO, read a comment in the minibuffer.
Return the comment number."
  (let* ((comments (fj-issue-get-comments repo issue))
         (cands (fj-get-comment-candidates comments))
         (choice (completing-read "Comment: " cands))
         (item
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr item)))

(defun fj-issue-get-comments (repo issue)
  "Return comments for ISSUE in REPO."
  (let* ((endpoint (format "/repos/%s/%s/issues/%s/comments"
                           fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-get-comment (&optional repo issue comment)
  "GET COMMENT in REPO.
ISSUE.
COMMENT is a number."
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (comment (or comment (fj-read-issue-comment repo issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s" fj-user repo comment)))
    (fj-get endpoint)))

(defun fj-issue-comment (&optional repo issue)
  "Add comment to ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (issue (or issue (fj-read-repo-issue repo)))
         (url (format "repos/%s/%s/issues/%s/comments" fj-user repo issue))
         (body (read-string "Comment: "))
         (params `(("body" . ,body)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment created!")))))

(defun fj-comment-patch (&optional repo issue params)
  "Edit ISSUE in REPO.
PARAMS."
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (comment (fj-read-issue-comment repo issue))
         (endpoint (format "repos/%s/%s/issues/comments/%s" fj-user repo comment)))
    (fj-patch endpoint params :json)))

(defun fj-comment-edit (&optional repo issue)
  "Edit comment on ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo
                         (when current-prefix-arg :force))))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-comment repo issue))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit comment: " old-body))
         (response (fj-issue-patch nil nil `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment created!")))))

;;; tl modes

(define-derived-mode fj-list-issue-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a list of issues."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format (vector (list "#" 3 t) (list "Issue" 2 t))))

(defun fj-list-issues-list (&optional repo)
  "List issues for current repo, or for REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo)))
         (issues (fj-repo-get-issues repo)))
    (with-current-buffer (get-buffer-create (format "*%s-issues*" repo))
      (setq tabulated-list-entries
            (cl-loop for issue in issues
                     for id = (alist-get 'number issue)
                     for name = (alist-get 'title issue)
                     collect `(nil [(,(number-to-string id)
                                     id id)
                                    (,name face link)])))
      (fj-list-issue-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (switch-to-buffer-other-window (current-buffer)))))

(define-derived-mode fj-issue-post-mode fedi-post-mode
  "fj-post")

(define-derived-mode fj-issue-view-mode special-mode "fj-issue"
  "Major mode for viewing an issue."
  :group "fj")

(defun fj-view-issue (&optional repo number)
  "View issue number NUMBER from REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (fj-get-issue repo number))
         (number (alist-get 'number issue))
         (comments (fj-issue-get-comments repo number)))
    (fedi-with-buffer (format "*fj-issue-%s" number) 'fj-issue-view-mode t
      (let-alist issue
        (insert .title "\n"
                .user.login "\n\n"
                .body "\n"
                fedi-horiz-bar "\n\n")
        (fj-render-comments comments)))))

(defun fj-render-comments (comments)
  "Render a list of COMMENTS."
  (insert
   (cl-loop for c in comments
            concat (let-alist c
                     (concat
                      .body "\n" .user.login "\n\n"
                      fedi-horiz-bar "\n\n")))))

(provide 'fj)
;;; fj.el ends here
