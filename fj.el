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

(defvar-local fj-current-repo nil)

(defvar-local fj-issue-spec nil
  "A plist holding some basic info about the issue currently displayed.
Repo, issue number, url.")

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

(defun fj-get (endpoint &optional params)
  "Make a GET request to ENDPOINT.
PARAMS is any parameters to send with the request."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "GET"
      (fedi-http--get-json url params))))

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
      (fedi-http--patch url params json))))

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
  (if (magit-inside-worktree-p)
      (file-name-nondirectory
       (directory-file-name
        (magit-toplevel)))))

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

(defun fj-read-user-repo-do ()
  "Prompt for a user repository."
  (let* ((repos (fj-get-repos))
         (cands (fj-get-repo-candidates repos)))
    (completing-read "Repo: " cands)))

(defun fj-read-user-repo (arg)
  "Return a user repo.
If ARG is a prefix, prompt with `completing-read'.
If it is a string, return it.
Otherwise, try `fj-current-repo' and `fj-current-dir-repo'.
If both return nil, also prompt."
  (if (consp arg)
      (fj-read-user-repo-do)
    (or arg
        fj-current-repo
        (ignore-errors (fj-current-dir-repo))
        (fj-read-user-repo-do))))

(defun fj-repo-create ()
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

(defun fj-get-item-candidates (items)
  "Return a list of ITEMS as completion candidates.
ITEMS may be issues or pull requests."
  (cl-loop for i in items
           collect `(,(alist-get 'title i)
                     ,(alist-get 'number i)
                     ,(alist-get 'id i))))

(defun fj-read-repo-issue (repo)
  "Given REPO, read an issue in the minibuffer.
Return the issue number."
  (let* ((issues (fj-repo-get-issues repo))
         (cands (fj-get-item-candidates issues))
         (choice (completing-read "Issue: " cands))
         (item
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr item)))

(defun fj-repo-get-issues (repo)
  "Return issues for REPO."
  (let* ((endpoint (format "repos/%s/%s/issues" fj-user repo)))
    (fj-get endpoint)))

(defun fj-get-issue (repo &optional issue)
  "GET ISSUE in REPO.
ISSUE is a number."
  (let* ((issue (or issue (fj-read-repo-issue repo)))
         (endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-issue-create (&optional repo)
  "Create an issue in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
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
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-issue repo issue))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit issue body: " old-body))
         (response (fj-issue-patch repo issue `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue edited!")))))

(defun fj-issue-edit-title (&optional repo issue)
  "Edit ISSUE title in REPO."
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-issue repo issue))
         (old-title (alist-get 'title data))
         (new-title (read-string "Edit issue title: " old-title))
         (response (fj-issue-patch repo issue `(("title" . ,new-title)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue title edited!")))))

(defun fj-issue-edit-title-current ()
  "Edit title of issue at point."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-edit-title fj-current-repo number)))

(defun fj-issue-close (&optional repo issue)
  "Close ISSUE in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (y-or-n-p (format "Close issue #%s?" issue))
      (let ((response (fj-issue-patch repo issue
                                      `(("state" . "closed")))))
        (fedi-http--triage response
                           (lambda ()
                             (message "issue closed!")))))))

(defun fj-issues-close-current-issue (&optional _)
  "Close current issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-close fj-current-repo number)
    (fj-list-issues-list)))

(defun fj-issue-delete (&optional repo issue)
  "Delete ISSUE in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (y-or-n-p "Delete issue?")
      (let* ((url (format "repos/%s/%s/issues/%s" fj-user repo issue))
             (response (fj-delete url)))
        (fedi-http--triage response
                           (lambda ()
                             (message "issue deleted!")))))))

;;; PULL REQUESTS

(defun fj-read-repo-pull-req (repo)
  "Given REPO, read an pull request in the minibuffer.
Return its number."
  (let* ((issues (fj-repo-get-pull-reqs repo))
         (cands (fj-get-item-candidates issues))
         (choice (completing-read "Pull request: " cands))
         (item
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr item)))

(defun fj-repo-get-pull-reqs (repo &optional state)
  "Return pull requests for REPO.
STATE should be \"open\", \"closed\", or \"all\"."
  (let* ((endpoint (format "repos/%s/%s/pulls" fj-user repo))
         (params `(("state" . ,(or state "open")))))
    (fj-get endpoint params)))

(defun fj-pull-req-comment (&optional repo pull)
  "Add comment to PULL in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (pull (or pull (fj-read-repo-pull-req repo))))
    (fj-issue-comment repo pull)))

(defun fj-pull-req-comment-edit (&optional repo pull)
  "Edit a comment on PULL in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (pull (or pull (fj-read-repo-pull-req repo))))
    (fj-issue-comment-edit repo pull)))

;;; COMMENTS

(defun fj-get-comment-candidates (comments)
  "Return a list of COMMENTS as completion candidates."
  (cl-loop for c in comments
           for body = (alist-get 'body c)
           for trim = (string-clean-whitespace
                       (substring body nil 70))
           collect `(,trim
                     ,(alist-get 'id c))))

(defun fj-read-item-comment (repo item)
  "Given ITEM in REPO, read a comment in the minibuffer.
Return the comment number."
  (let* ((comments (fj-issue-get-comments repo item))
         (cands (fj-get-comment-candidates comments))
         (choice (completing-read "Comment: " cands))
         (comm
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr comm)))

(defun fj-issue-get-comments (repo issue)
  "Return comments for ISSUE in REPO."
  (let* ((endpoint (format "repos/%s/%s/issues/%s/comments"
                           fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-get-comment (repo issue &optional comment)
  "GET data for COMMENT of ISSUE in REPO.
COMMENT is a number."
  (let* ((comment (or comment (fj-read-item-comment repo issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s"
                           fj-user repo comment)))
    (fj-get endpoint)))

(defun fj-issue-comment (&optional repo issue)
  "Add comment to ISSUE in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (url (format "repos/%s/%s/issues/%s/comments" fj-user repo issue))
         (body (read-string "Comment: "))
         (params `(("body" . ,body)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment created!")))))

(defun fj-issue-comment-current ()
  "Comment on the issue currently being displayed."
  (interactive)
  (let ((repo (get-text-property (point) 'fj-repo))
        (number (get-text-property (point) 'fj-issue)))
    (fj-issue-comment repo number)))

(defun fj-comment-patch (repo issue &optional comment params)
  "Edit ISSUE COMMENT in REPO.
PARAMS."
  (let* ((comment (or comment (fj-read-item-comment repo issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s" fj-user repo comment)))
    (fj-patch endpoint params :json)))

(defun fj-issue-comment-edit (&optional repo issue)
  "Edit comment on ISSUE in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-comment repo issue))
         (comment (alist-get 'id data))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit comment: " old-body))
         (response (fj-comment-patch repo issue comment
                                     `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment edited!")))))

;;; tl modes

(define-derived-mode fj-list-issue-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a list of issues."
  (setq tabulated-list-padding 0) ;2) ; point directly on issue
  (setq tabulated-list-format (vector (list "#" 3 t) (list "Issue" 2 t))))

(define-button-type 'fj-button
  'follow-link t
  'action 'fj-issues-view-current-issue
  'help-echo "RET: View this issue.")

(defun fj-list-issues-list (&optional repo issues)
  "Display ISSUES in a tabulated list view.
Either for `fj-current-repo', or for REPO, a string.
With a prefix arg, or if REPO and `fj-current-repo' are nil,
prompt for a repo to list."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issues (or issues (fj-repo-get-issues repo)))
         (prev-buf (buffer-name (current-buffer))))
    (with-current-buffer (get-buffer-create (format "*%s-issues*" repo))
      (setq tabulated-list-entries
            (cl-loop for issue in issues
                     for id = (alist-get 'number issue)
                     for name = (alist-get 'title issue)
                     collect `(nil [(,(number-to-string id)
                                     id ,id
                                     type fj-button)
                                    (,name face link
                                           id ,id
                                           type fj-button)])))
      (fj-list-issue-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq fj-current-repo repo)
      (unless (string-suffix-p "-issues*" prev-buf)
        (switch-to-buffer-other-window (current-buffer))))))

(defun fj-list-pull-reqs (&optional repo)
  "List pull requests for REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (prs (fj-repo-get-pull-reqs repo)))
    (fj-list-issues-list repo prs)))

(define-derived-mode fj-issue-post-mode fedi-post-mode
  "fj-post")

(define-derived-mode fj-issue-view-mode special-mode "fj-issue"
  "Major mode for viewing an issue."
  :group "fj")

;; arg fj-button in tl view:
(defun fj-issues-view-current-issue (&optional _)
  "View current issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-view fj-current-repo number)))

(defun fj-issue-view (&optional repo number)
  "View issue number NUMBER from REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (fj-get-issue repo number))
         (number (alist-get 'number issue))
         (comments (fj-issue-get-comments repo number)))
    (fedi-with-buffer (format "*fj-issue-%s" number) 'fj-issue-view-mode t
      (let-alist issue
        (insert
         (propertize
          (concat
           (propertize .title
                       'face '(:weight bold))
           "\n"
           (propertize .user.login
                       'face '(:underline t))
           "\n\n"
           .body "\n"
           fedi-horiz-bar "\n\n"
           (fj-render-comments comments))
          'fj-issue number
          'fj-repo repo))
        (setq fj-issue-spec
              `((:repo ,repo :issue ,number :url ,.url)))))))

(defun fj-render-comments (comments)
  "Render a list of COMMENTS."
  (cl-loop for c in comments
           concat (let-alist c
                    (concat
                     .body "\n"
                     (propertize .user.login
                                 'face '(:underline t))
                     "\n\n"
                     fedi-horiz-bar "\n\n"))))

(defun fj-issue-edit-current ()
  "Edit the issue currently being displayed."
  (interactive)
  (goto-char (point-min))
  (let ((repo (get-text-property (point) 'fj-repo))
        (number (get-text-property (point) 'fj-issue)))
    (fj-issue-edit repo number)))

(provide 'fj)
;;; fj.el ends here
