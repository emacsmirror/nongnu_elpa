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
  (let* ((issues (fj-get-repo-issues repo))
         (cands (fj-get-issue-candidates issues))
         (choice (completing-read "Issue:" cands))
         (item
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr item)))

(defun fj-get-repo-issues (&optional repo)
  "Return issues for REPO."
  (let* ((repo (or repo (fj-read-user-repo)))
         (endpoint (format "repos/%s/%s/issues" fj-user repo)))
    (fj-get endpoint)))

(defun fj-get-issue (&optional issue repo)
  "GET ISSUE in REPO.
ISSUE is a number"
  (let* ((repo (or repo (fj-read-user-repo)))
         (issue (or issue (fj-read-repo-issue repo)))
         (endpoint (format "repos/%s/%s/issues/%s" fj-user repo issue)))
    (fj-get endpoint)))

(defun fj-create-issue (repo)
  "Create an issue in REPO."
  (interactive)
  (let* ((url (format "repos/%s/%s/issues" fj-user repo))
         (title (read-string "Title: "))
         (body (read-string "Body: "))
         (params `(("body" . ,body)
                   ("title" . ,title)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue %s created!" title)))))

(defun fj-get-issue-comments (repo issue)
  "Return comments for ISSUE in REPO."
  (let* ((index (alist-get 'number issue))
         (endpoint (format "/repos/%s/%s/issues/%s/comments"
                           fj-user repo index)))
    (fj-get endpoint)))

(provide 'fj)
;;; fj.el ends here
