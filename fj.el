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
(require 'magit-process)

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
  (let* ((url (fj-api endpoint))
         (resp (fj-authorized-request "GET"
                 (fedi-http--get-json url params))))
    (if (eq (caar resp) 'errors)
        (error "I am Error: %s Endpoint: %s"
               (alist-get 'message resp)
               endpoint)
      resp)))

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
  ;; FIXME: fails if remote url is diff to root dir!
  (ignore-errors
    (if (magit-inside-worktree-p)
        (file-name-nondirectory
         (directory-file-name
          (magit-toplevel))))))

(defun fj-get-repos ()
  "Return the user's repos."
  (let ((endpoint "user/repos"))
    (fj-get endpoint '(("limit" . "100")))))

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
        (fj-current-dir-repo) ;; requires loaded magit
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

(defun fj-repo-get-issues (repo &optional state)
  "Return issues for REPO.
STATE is for issue status, a string of open, closed or all."
  (let* ((endpoint (format "repos/%s/%s/issues" fj-user repo))
         (params `(("state" . ,state))))
    (fj-get endpoint params)))

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

;;; TABLIST VIEWS

(define-derived-mode fj-list-issue-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a tabulated list of issues."
  (setq tabulated-list-padding 0) ;2) ; point directly on issue
  (setq tabulated-list-format (vector (list "#" 3 t) (list "Issue" 2 t))))

(defvar fj-list-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") #'fj-issues-tl-comment-issue)
    (define-key map (kbd "e") #'fj-issues-tl-edit-issue)
    (define-key map (kbd "t") #'fj-issues-tl-edit-issue-title)
    (define-key map (kbd "v") #'fj-issues-tl-view-issue)
    (define-key map (kbd "k") #'fj-issues-tl-close-issue)
    (define-key map (kbd "n") #'fj-issues-tl-create)
    (define-key map (kbd "c") #'fj-issues-tl-create)
    map)
  "Map for `fj-list-issue-mode', a tabluated list of issues.")

(define-button-type 'fj-button
  'follow-link t
  'action 'fj-issues-tl-view-issue
  'help-echo "RET: View this issue.")

(defun fj-list-issues (&optional repo issues)
  "Display ISSUES in a tabulated list view.
Either for `fj-current-repo', or for REPO, a string.
With a prefix arg, or if REPO and `fj-current-repo' are nil,
prompt for a repo to list."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issues (or issues (fj-repo-get-issues repo)))
         (prev-buf (buffer-name (current-buffer)))
         (buf-name (format "*%s-issues*" repo)))
    (with-current-buffer (get-buffer-create buf-name)
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
      (cond ((string= buf-name prev-buf) ; same repo
             nil)
            ((string-suffix-p "-issues*" prev-buf) ; diff repo
             (switch-to-buffer (current-buffer)))
            (t ; new buf
             (switch-to-buffer-other-window (current-buffer)))))))

(defun fj-list-pull-reqs (&optional repo)
  "List pull requests for REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (prs (fj-repo-get-pull-reqs repo)))
    (fj-list-issues repo prs)))

(defun fj-issues-tl-create ()
  "Create an issue in current repo."
  (interactive)
  (fj-issue-create fj-current-repo))

;; arg fj-button in tl view:
(defun fj-issues-tl-view-issue (&optional _)
  "View current issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-view fj-current-repo number)))

(defun fj-issues-tl-edit-issue ()
  "Edit issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-edit fj-current-repo number)))

(defun fj-issues-tl-edit-issue-title ()
  "Edit title of issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-edit-title fj-current-repo number)))

(defun fj-issues-tl-comment-issue ()
  "Comment on issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-comment fj-current-repo number)))

(defun fj-issues-tl-close-issue (&optional _)
  "Close current issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item))))
    (fj-issue-close fj-current-repo number)
    (fj-list-issues)))

;;; ISSUE VIEW

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

(defun fj-issue-view (&optional repo number reload)
  "View issue number NUMBER from REPO.
RELOAD means we are reloading, so don't open in other window."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (fj-get-issue repo number))
         (number (alist-get 'number issue))
         (comments (fj-issue-get-comments repo number)))
    (fedi-with-buffer (format "*fj-issue-%s" number) 'fj-issue-view-mode
                      (not reload)
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
        (setq fj-current-repo repo)
        (setq fj-issue-spec
              `(:repo ,repo :issue ,number :url ,.url))))))

(defun fj-issue-view-comment ()
  "Comment on the issue currently being viewed."
  (interactive)
  (let ((repo (get-text-property (point) 'fj-repo))
        (number (get-text-property (point) 'fj-issue)))
    (fj-issue-comment repo number)))

(defun fj-issue-view-reload ()
  "Reload the current issue view."
  (interactive)
  ;; TODO: check for current issue view!
  (if (not fj-issue-spec)
      (user-error "Not in an issue view?")
    (let ((number (plist-get fj-issue-spec :issue)))
      (fj-issue-view fj-current-repo
                     number :reload))))

(defun fj-issue-view-edit ()
  "Edit the issue currently being viewed."
  (interactive)
  (goto-char (point-min))
  (let ((repo (get-text-property (point) 'fj-repo))
        (number (get-text-property (point) 'fj-issue)))
    (fj-issue-edit repo number)))

(define-derived-mode fj-issue-view-mode view-mode "fj-issue"
  "Major mode for viewing an issue."
  :group "fj")

;;; POST MODE

(define-derived-mode fj-issue-post-mode fedi-post-mode
  "fj-post")

(provide 'fj)
;;; fj.el ends here
