;;; fj.el --- client for forgejo instances      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus

;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Keywords: convenience

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

;; 

;;; Code:

(require 'fedi)

(defvar fj-token nil)
(defvar fj-user nil)

(defvar fj-host "https://codeberg.org")

(defun fj-api (endpoint)
  "Return a URL for ENDPOINT."
  (fedi-http--api endpoint fj-host "v1"))

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

(defun fj-read-user-repo ()
  "Read a repo in the minibuffer."
  (let* ((repos (fj-get-repos))
         (cands (fj-get-repo-candidates repos)))
    (completing-read "Repo:" cands)))

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
         (choice (completing-read "Issue:" cands))
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
ISSUE is a number"
  ;; (fj-with-repo nil nil
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-issue-create (&optional repo)
  "Create an issue in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo)))
         (url (format "repos/%s/%s/issues" fj-user repo))
         (title (read-string "Title: "))
         (body (read-string "Body: "))
         (params `(("body" . ,body)
                   ("title" . ,title)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue %s created!" title)))))

(defun fj-issue-patch (&optional repo issue params)
  "Edit ISSUE in REPO.
PARAMS."
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-patch endpoint params :json)))

(defun fj-issue-edit (&optional repo issue)
  "REPO ISSUE."
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (data (fj-get-issue repo issue))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit issue: " old-body)))
    (fj-issue-patch nil nil `(("body" . ,new-body)))))

(defun fj-issue-close ()
  "Close issue."
  (interactive)
  (fj-issue-patch nil nil `(("state" . "closed"))))


(defun fj-issue-delete (&optional repo issue)
  "Delete ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (url (format "repos/%s/%s/issues/%s" fj-user repo issue))
         (response (fj-delete url)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue deleted!")))))

;;; COMMENTS

(defun fj-issue-get-comments (repo issue)
  "Return comments for ISSUE in REPO."
  (let* ((index (alist-get 'number issue))
         (endpoint (format "/repos/%s/%s/issues/%s/comments"
                           fj-user repo index)))
    (fj-get endpoint)))

(defun fj-issue-reply (&optional repo issue)
  "REPLY to ISSUE in REPO."
  (interactive)
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (url (format "repos/%s/%s/issues/%s/comments" fj-user repo issue))
         (body (read-string "Comment: "))
         (params `(("body" . ,body)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment created!")))))

(provide 'fj)
;;; fj.el ends here
