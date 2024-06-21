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
(require 'magit-diff)

(require 'markdown-mode)
(require 'shr)

;;; VARIABLES

(defvar fj-token nil)

(defvar fj-user nil)

(defvar fj-host "https://codeberg.org")

(defvar-local fj-current-repo nil)

(defvar-local fj-issue-spec nil
  "A plist holding some basic info about the issue currently displayed.
Repo, issue number, url.")

(defvar-local fj-issues-tl-spec nil
  "A plist holding some basic info about the issues currently displayed.
Repo, view parameters, etc.")

(defun fj-api (endpoint)
  "Return a URL for ENDPOINT."
  (fedi-http--api endpoint fj-host "v1"))

;;; FACES

(defface fj-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for secondary info.")

(defface fj-closed-issue-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the title of a closed issue.")

(defface fj-user-face
  '((t :inherit font-lock-function-name-face))
  "User face.")

(defface fj-figures-face
  '((t :inherit font-lock-doc-face))
  "Face for figures (stars count, comments count, etc.).")

(defface fj-item-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for item names.")

(defface fj-item-author-face
  `((t ;:background ,(face-attribute 'magit-diff-hunk-heading :background)
     :inherit magit-diff-hunk-heading
     :extend t))
  "Face for item authors.")

(defface fj-issue-label-face
  '((t :inherit font-lock-keyword-face :box t))
  "Face for issue labels.")

;;; UTILS

(defun fj-issues-tl-own-repo-p ()
  "T if repo is owned by `fj-user'."
  (equal (plist-get fj-issues-tl-spec :owner)
         fj-user))

(defun fj-issue-own-p ()
  "T if issue is authored by `fj-user'."
  (cond ((eq major-mode 'fj-issue-view-mode)
         (equal fj-user
                (plist-get fj-issue-spec :author)))
        ((eq major-mode 'fj-list-issue-mode)
         (let* ((entry (tabulated-list-get-entry))
                (author (car (seq-elt entry 2))))
           (equal fj-user author)))))

(defun fj-comment-own-p ()
  "T if comment is authored by `fj-user'."
  (and (eq major-mode 'fj-issue-view-mode)
       (equal fj-user (fj--property 'fj-comment-author))))

(defmacro fj-with-issue (&optional body)
  "Execute BODY if we are in an issue view."
  (declare (debug t))
  `(if (not fj-issue-spec)
       (user-error "Not in an issue view?")
     ,body))

(defmacro fj-with-own-repo (&optional body)
  "Execute BODY if a repo owned by `fj-user'."
  (declare (debug t))
  `(if (not (fj-issues-tl-own-repo-p))
       (user-error "Not in a repo you own")
     ,body))

(defmacro fj-with-own-issue (&optional body)
  "Execute BODY if issue is authored by `fj-user'."
  (declare (debug t))
  `(fj-with-issue
    (if (not (fj-issue-own-p))
        (user-error "Not an issue you own")
      ,body)))

(defmacro fj-with-own-issue-or-repo (&optional body)
  "Execute BODY if issue authored or repo owned by `fj-user'."
  (declare (debug t))
  `(if (not (or (fj-issue-own-p)
                (fj-issues-tl-own-repo-p)))
       (user-error "Not an issue or repo you own")
     ,body))

(defmacro fj-with-own-comment (&optional body)
  "Execute BODY if comment at point is authored by `fj-user'."
  (declare (debug t))
  `(fj-with-issue
    (if (not (fj-comment-own-p))
        (user-error "No comment of yours at point")
      ,body)))

(defun fj-kill-all-buffers ()
  "Kill all fj buffers."
  (interactive)
  (fedi-kill-all-buffers "*fj-"))

(defun fj-switch-to-buffer ()
  "Switch to a live fj buffer."
  (interactive)
  (fedi-switch-to-buffer "*fj-"))

(defun fj-issue-right-align-str (str)
  "Right align STR and return it."
  (concat
   (propertize
    " "
    'display
    `(space :align-to (- right ,(+ (length str) 4))))
   str))

;;; NAV

(defun fj-issue-next ()
  "Go to next issue or comment."
  (interactive)
  (fedi--goto-pos #'next-single-property-change 'fj-byline))


(defun fj-issue-prev ()
  "Goto previous issue or comment."
  (interactive)
  (fedi--goto-pos #'previous-single-property-change 'fj-byline))

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

(defun fj-get (endpoint &optional params)
  "Make a GET request to ENDPOINT.
PARAMS is any parameters to send with the request."
  (let* ((url (fj-api endpoint))
         (resp (fj-authorized-request "GET"
                 (fedi-http--get-json url params))))
    (cond ((or (eq (caar resp) 'errors)
               (eq (caar resp) 'message))
           (user-error "I am Error: %s Endpoint: %s"
                       (alist-get 'message resp)
                       endpoint))
          (t
           resp))))

(defun fj-post (endpoint params)
  "Make a POST request to ENDPOINT.
PARAMS."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "POST"
      (fedi-http--post url params nil :json))))


(defun fj-put (endpoint &optional params json)
  "Make a PUT request to ENDPOINT.
PARAMS.
JSON."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "PUT"
      (fedi-http--put url params nil json))))

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

;;; USER REPOS TL

(defvar fj-user-spec nil)

(define-button-type 'fj-user-repo-button
  'follow-link t
  'action 'fj-user-repo-tl-list-issues
  'help-echo "RET: View this repo's issues.")

(define-derived-mode fj-user-repo-tl-mode tabulated-list-mode
  "fj-user-repos"
  "Mode for displaying a tabulated list of user repos."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0) ;2) ; point directly on issue
  (setq tabulated-list-format
        '[("Name" 16 t)
          ("★" 3 t)
          ("" 2 t)
          ("Lang" 10 t)
          ("Description" 55 nil)]))

(defun fj-get-current-user ()
  "Return the data for the current user."
  (fj-get "user"))

(defun fj-get-user-repos (user)
  "GET request repos for USER."
  (let ((endpoint (format "users/%s/repos" user)))
    (fj-get endpoint)))

(defun fj-user-repos-tl (&optional user)
  "View a tabulated list of respos for USER."
  (interactive "sView user repos: ")
  (let* ((repos (fj-get-user-repos user))
         (entries (fj-search-tl-entries repos :no-owner))
         (buf (format "*fj-repos-%s*" user))
         (prev-buf))
    (fj-repos-tl-render buf entries #'fj-user-repo-tl-mode prev-buf)
    (setq fj-user-spec `(:owner ,user)))) ;; TODO: URL

(defun fj-list-own-repos ()
  "List repos for `fj-user'."
  (interactive)
  (fj-user-repos-tl fj-user))

(defun fj-user-repo-tl-list-issues (&optional _)
  "View issues of current repo from tabulated user repos listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (name (car (seq-first item)))
         (user (plist-get fj-user-spec :owner)))
    (fj-list-issues name nil nil user)))

;; FIXME: make work in search-repos TL too
(defun fj-user-repo-tl-star (&optional unstar)
  "Star or UNSTAR current repo from tabulated user repos listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (name (car (seq-first item)))
         (owner (plist-get fj-user-spec :owner)))
    (fj-star-repo name owner unstar)))

(defun fj-star-repo (repo owner &optional unstar)
  "Star or UNSTAR REPO owned by OWNER."
  (let* ((endpoint (format "user/starred/%s/%s" owner repo))
         (resp (if unstar
                   (fj-delete endpoint)
                 (fj-put endpoint))))
    (fedi-http--triage resp
                       (lambda ()
                         (message "Repo %s %s!" repo
                                  (if unstar "unstarred" "starred"))))))

(defun fj-user-repo-tl-unstar ()
  "Unstar current repo from tabulated user repos listing."
  (interactive)
  (fj-user-repo-tl-star :unstar))

;;; USER REPOS

(defun fj-current-dir-repo ()
  "If we are in a repository, return its name."
  ;; FIXME: fails if remote url is diff to root dir!
  (ignore-errors
    (when (magit-inside-worktree-p)
      (let* ((repos (fj-get-repos))
             (names (cl-loop for r in repos
                             collect (alist-get 'name r)))
             (dir (file-name-nondirectory
                   (directory-file-name
                    (magit-toplevel)))))
        (when (member dir names)
          dir))))) ; nil if dir no match any remotes

(defun fj-get-repos ()
  "Return the user's repos."
  (let ((endpoint "user/repos"))
    (fj-get endpoint '(("limit" . "100")))))

(defun fj-get-repo-candidates (repos)
  "Return REPOS as completion candidates."
  (cl-loop for r in repos
           collect `(,(alist-get 'name r)
                     ,(alist-get 'id r)
                     ,(alist-get 'description r)
                     ,(alist-get 'username
                                 (alist-get 'owner r)))))

(defun fj-read-user-repo-do (&optional default)
  "Prompt for a user repository.
DEFAULT is initial input for `completing-read'."
  (let* ((repos (fj-get-repos))
         (cands (fj-get-repo-candidates repos)))
    (completing-read "Repo: " cands
                     nil nil default)))

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

(defun fj-repo-get-issues (repo &optional state user)
  "Return issues for REPO.
STATE is for issue status, a string of open, closed or all.
USER is the repo owner."
  (let* ((endpoint (format "repos/%s/%s/issues" (or user fj-user) repo))
         (params `(("state" . ,state)
                   ("limit" . "100"))))
    (condition-case err
        (fj-get endpoint params)
      (t (format "%s" (error-message-string err))))))

(defun fj-get-issue (repo &optional owner issue)
  "GET ISSUE in REPO.
ISSUE is a number.
OWNER is the repo owner."
  (let* ((issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner fj-user)) ;; FIXME
         (endpoint (format "repos/%s/%s/issues/%s" owner repo issue)))
    (fj-get endpoint)))

(defun fj-issue-create (&optional repo user)
  "Create an issue in REPO owned by USER."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (user (or user fj-user))
         (title (read-string "Title: "))
         (body (read-string "Body: "))
         (response (fj-issue-post repo user title body)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue %s created!" title)
                         (fj-issues-tl-reload)))))

(defun fj-issue-post (repo user title body)
  "POST a new issue to REPO owned by USER.
TITLE and BODY are the parts of the issue to send."
  (let ((url (format "repos/%s/%s/issues" user repo))
        (params `(("body" . ,body)
                  ("title" . ,title))))
    (fj-post url params)))

(defun fj-issue-patch (repo owner issue title body)
  "PATCH/Edit ISSUE in REPO.
With PARAMS.
OWNER is the repo owner."
  (let* ((params `(("body" . ,body)
                   ("title" . ,title)))
         (endpoint (format "repos/%s/%s/issues/%s" owner repo issue)))
    (fj-patch endpoint params :json)))

(defun fj-issue-edit (&optional repo owner issue)
  "Edit ISSUE body in REPO.
OWNER is the repo owner."
  (interactive)
  ;; FIXME: only if author!
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner ;; FIXME: owner
                    (plist-get fj-issue-spec :owner)
                    fj-user))
         (data (fj-get-issue repo owner issue))
         (old-body (alist-get 'body data))
         (new-body (read-string "Edit issue body: " old-body))
         (response (fj-issue-patch repo owner issue nil new-body)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue edited!")))))

(defun fj-issue-edit-title (&optional repo owner issue)
  "Edit ISSUE title in REPO.
OWNER is the repo owner."
  (interactive)
  ;; FIXME: only if author!
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner ;; FIXME: owner
                    (plist-get fj-issue-spec :owner)
                    fj-user))
         (data (fj-get-issue repo owner issue))
         (old-title (alist-get 'title data))
         (new-title (read-string "Edit issue title: " old-title))
         (response (fj-issue-patch repo owner issue new-title)))
    (fedi-http--triage response
                       (lambda ()
                         (message "issue title edited!")))))

(defun fj-issue-close (&optional repo owner issue state)
  "Close ISSUE in REPO or set to STATE.
OWNER is the repo owner."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (action-str (if (string= state "open") "Open" "Close"))
         (state (or state "closed")))
    (when (y-or-n-p (format "%s issue #%s?" action-str issue))
      (let ((response (fj-issue-patch repo owner issue
                                      `(("state" . ,state)))))
        (fedi-http--triage response
                           (lambda ()
                             (message "Issue %s %s!" issue state)))))))

(defun fj-issue-delete (&optional repo issue no-confirm)
  "Delete ISSUE in REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (or no-confirm
              (y-or-n-p "Delete issue?"))
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

;; (defun fj-pull-req-comment-edit (&optional repo pull)
;;   "Edit a comment on PULL in REPO."
;;   (interactive "P")
;;   (let* ((repo (fj-read-user-repo repo))
;;          (pull (or pull (fj-read-repo-pull-req repo))))
;;     (fj-issue-comment-edit repo pull)))

(defun fj-list-pull-reqs (&optional repo)
  "List pull requests for REPO."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (prs (fj-repo-get-pull-reqs repo)))
    (fj-list-issues repo prs)))

;;; COMMENTS

(defun fj-get-comment-candidates (comments)
  "Return a list of COMMENTS as completion candidates."
  (cl-loop for c in comments
           for body = (alist-get 'body c)
           for trim = (string-clean-whitespace
                       (substring body nil 70))
           collect `(,trim
                     ,(alist-get 'id c))))

(defun fj-read-item-comment (repo owner item)
  "Given ITEM in REPO, read a comment in the minibuffer.
Return the comment number.
OWNER is the repo owner."
  (let* ((comments (fj-issue-get-comments repo owner item))
         (cands (fj-get-comment-candidates comments))
         (choice (completing-read "Comment: " cands))
         (comm
          (car
           (cl-member-if (lambda (c)
                           (string= (car c) choice))
                         cands))))
    (cadr comm)))

(defun fj-issue-get-comments (repo owner issue)
  "Return comments for ISSUE in REPO.
OWNER is the repo owner."
  (let* ((endpoint (format "repos/%s/%s/issues/%s/comments"
                           owner repo issue)))
    (fj-get endpoint)))

(defun fj-get-comment (repo owner issue &optional comment)
  "GET data for COMMENT of ISSUE in REPO.
COMMENT is a number.
OWNER is the repo owner."
  (let* ((comment (or comment (fj-read-item-comment repo owner issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s"
                           owner repo comment)))
    (fj-get endpoint)))

(defun fj-issue-comment (&optional repo owner issue comment)
  "Add COMMENT to ISSUE in REPO.
OWNER is the repo owner."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner fj-user)) ;; FIXME owner
         (url (format "repos/%s/%s/issues/%s/comments" owner repo issue))
         (body (or comment (read-string "Comment: ")))
         (params `(("body" . ,body)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment created!")))))

(defun fj-comment-patch (repo owner comment-id &optional params issue)
  "Edit ISSUE COMMENT in REPO owned by OWNER.
PARAMS."
  (let* ((id (or comment-id (fj-read-item-comment repo owner issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s" owner repo id)))
    (fj-patch endpoint params :json)))

(defun fj-issue-comment-edit (&optional repo owner id new-body)
  "Edit comment on ISSUE in REPO.
OWNER is the repo owner."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (id (or id (fj--property 'fj-comment-id)))
         (owner (or owner fj-user)) ;; FIXME
         (response (fj-comment-patch repo owner id
                                     `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment edited!")))))

;; (defun fj-issue-comment-edit (&optional repo owner issue)
;;   "Edit comment on ISSUE in REPO.
;; OWNER is the repo owner."
;;   (interactive "P")
;;   (let* ((repo (fj-read-user-repo repo))
;;          (issue (or issue (fj-read-repo-issue repo)))
;;          (owner (or owner fj-user)) ;; FIXME
;;          (data (fj-get-comment repo owner issue))
;;          (comment (alist-get 'id data))
;;          (old-body (alist-get 'body data))
;;          (new-body (read-string "Edit comment: " old-body))
;;          (response (fj-comment-patch repo owner issue comment
;;                                      `(("body" . ,new-body)))))
;;     (fedi-http--triage response
;;                        (lambda ()
;;                          (message "comment edited!")))))

;;; ISSUES TABLIST

;; webUI sort options:
;; (defvar fj-list-tl-sort-options
;;   '("latest"
;;     "oldest"
;;     "recentupdate"
;;     "leastupdate"
;;     "mostcomment"
;;     "leastcomment"
;;     "nearduedate"
;;     "farduedate"))

(defvar fj-list-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map) ; has nav
    (define-key map (kbd "C") #'fj-issues-tl-comment-issue)
    (define-key map (kbd "e") #'fj-issues-tl-edit-issue)
    (define-key map (kbd "t") #'fj-issues-tl-edit-issue-title)
    (define-key map (kbd "v") #'fj-issues-tl-view-issue)
    (define-key map (kbd "k") #'fj-issues-tl-close-issue)
    (define-key map (kbd "K") #'fj-issues-tl-delete-issue)
    (define-key map (kbd "c") #'fj-issues-tl-create)
    (define-key map (kbd "g") #'fj-issues-tl-reload)
    (define-key map (kbd "C-c C-c") #'fj-list-issues-cycle)
    (define-key map (kbd "o") #'fj-issues-tl-reopen-issue)
    map)
  "Map for `fj-list-issue-mode', a tabluated list of issues.")

(define-derived-mode fj-list-issue-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a tabulated list of issues."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0) ;2) ; point directly on issue
  (setq tabulated-list-format
        '[("#" 5 t)
          ("💬" 3 t)
          ("Author" 10 t)
          ("Issue" 2 t)]))

(define-button-type 'fj-issue-button
  'follow-link t
  'action 'fj-issues-tl-view-issue
  'help-echo "RET: View this issue.")

(defun fj-issues-tl-entries (issues &optional state)
  "Return tabluated list entries for ISSUES.
STATE is a string."
  (cl-loop for issue in issues
           for id = (alist-get 'number issue)
           for title = (alist-get 'title issue)
           for state = (or state (alist-get 'state issue))
           for comments = (number-to-string
                           (alist-get 'comments issue))
           for author = (alist-get 'username
                                   (alist-get 'user issue))
           collect `(nil [(,(number-to-string id)
                           id ,id
                           state ,state
                           type fj-issue-button)
                          ,(propertize comments
                                       'face 'fj-figures-face)
                          (,author face fj-user-face
                                   id ,id
                                   state ,state
                                   type  fj-issues-owner-button)
                          (,title face ,(if (equal state "closed")
                                            'fj-closed-issue-face
                                          'fj-item-face)
                                  id ,id
                                  state ,state
                                  type fj-issue-button)])))

(defun fj-list-issues (&optional repo issues state user)
  "Display ISSUES in a tabulated list view.
Either for `fj-current-repo', or for REPO, a string.
With a prefix arg, or if REPO and `fj-current-repo' are nil,
prompt for a repo to list."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issues (or issues (fj-repo-get-issues repo state user)))
         (owner (alist-get 'owner
                           (alist-get 'repository (car issues))))
         (prev-buf (buffer-name (current-buffer)))
         (state-str (or state "open"))
         ;; FIXME: opens a buf for each state:
         ;; can we put the state in the header?
         (buf-name (format "*fj-%s-%s-issues*" repo state-str)))
    (with-current-buffer (get-buffer-create buf-name)
      (setq tabulated-list-entries
            (fj-issues-tl-entries issues state-str))
      (fj-list-issue-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq fj-current-repo repo)
      (setq fj-issues-tl-spec
            `(:repo ,repo :state ,state-str :owner ,owner)) ;; TODO: URL?
      (cond ((string= buf-name prev-buf) ; same repo
             nil)
            ((string-suffix-p "-issues*" prev-buf) ; diff repo
             (switch-to-buffer (current-buffer)))
            (t                             ; new buf
             (switch-to-buffer-other-window (current-buffer)))))))

(defun fj-list-issues-closed (&optional repo issues user)
  "Display closed ISSUES for REPO in tabulated list view."
  (interactive "P")
  (fj-list-issues repo issues "closed" user))

(defun fj-list-issues-all (&optional repo issues user)
  "Display all ISSUES for REPO in tabulated list view."
  (interactive "P")
  (fj-list-issues repo issues "all" user))

(defun fj-list-issues-cycle ()
  "Cycle between listing of open, closed, and all issues."
  (interactive)
  (let ((state (plist-get fj-issues-tl-spec :state))
        (owner (plist-get fj-issues-tl-spec :owner)))
    (cond ((string= state "closed")
           (fj-list-issues-all nil nil owner))
          ((string= state "all")
           (fj-list-issues nil nil nil owner))
          (t ; open is default
           (fj-list-issues-closed nil nil owner)))))

(defun fj-issues-tl-reload ()
  "Reload current issues tabulated list view."
  (interactive)
  (when (string-suffix-p "-issues*"
                         (buffer-name (current-buffer)))
    (let ((state (plist-get fj-issues-tl-spec :state))
          (user (plist-get fj-issues-tl-spec :owner)))
      (fj-list-issues nil nil state user))))

;; ISSUES TL ACTIONS

(defun fj-issues-tl-create ()
  "Create an issue in current repo."
  (interactive)
  ;; TODO: handle response if not allowed to create
  ;; or check before creating
  (fj-issue-create fj-current-repo))

;; arg fj-issue-button in tl view:
(defun fj-issues-tl-view-issue (&optional _)
  "View current issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item)))
         (owner (plist-get fj-issues-tl-spec :owner)))
    (fj-issue-view fj-current-repo owner number)))

(defun fj-issues-tl-edit-issue ()
  "Edit issue from tabulated issues listing."
  (interactive)
  (fj-with-own-issue
   (let* ((item (tabulated-list-get-entry))
          (number (car (seq-first item))))
     (fj-issue-edit fj-current-repo number))))

(defun fj-issues-tl-edit-issue-title ()
  "Edit title of issue from tabulated issues listing."
  (interactive)
  (fj-with-own-issue-or-repo
   (let* ((item (tabulated-list-get-entry))
          (number (car (seq-first item))))
     (fj-issue-edit-title fj-current-repo number)
     (fj-issues-tl-reload))))

(defun fj-issues-tl-comment-issue ()
  "Comment on issue from tabulated issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (number (car (seq-first item)))
         (comment (read-string
                   (format "Comment on issue #%s: " number))))
    (fj-issue-comment fj-current-repo number comment)))

(defun fj--property (prop)
  "Get text property PROP at point."
  (get-text-property (point) prop))

(defun fj-issues-tl-close-issue (&optional _)
  "Close current issue from tabulated issues listing."
  (interactive)
  ;; TODO make check work for "all": need to prop each tl entry
  (fj-with-own-issue-or-repo
   (if (string= (fj--property 'state) "closed")
       (user-error "Issue already closed")
     (let* ((item (tabulated-list-get-entry))
            (number (car (seq-first item))))
       (fj-issue-close fj-current-repo number)
       (fj-issues-tl-reload)))))

(defun fj-issues-tl-delete-issue (&optional _)
  "Delete current issue from tabulated issues listing."
  (interactive)
  (fj-with-own-repo
   (let* ((item (tabulated-list-get-entry))
          (number (car (seq-first item))))
     (when (y-or-n-p (format "Delete issue %s?" number))
       (fj-issue-delete fj-current-repo number :no-confirm)
       (fj-issues-tl-reload)))))

(defun fj-issues-tl-reopen-issue (&optional _)
  "Reopen current issue from tabulated issues listing."
  (interactive)
  ;; FIXME: check should be per entry:
  (if (string= (plist-get fj-issues-tl-spec :state) "open")
      (user-error "Viewing open issues?")
    (let* ((item (tabulated-list-get-entry))
           (number (car (seq-first item))))
      (fj-issue-close fj-current-repo number "open")
      (fj-issues-tl-reload))))

;;; ISSUE VIEW
(defvar fj-url-regex fedi-post-url-regex)

(defun fj-mdize-plain-urls ()
  "Markdown-ize any plain string URLs found in current buffer."
  ;; FIXME: this doesn't rly work with ```verbatim``` in md
  ;; NB: this must not break any md, otherwise `markdown-standalone' may
  ;; hang!
  (while (re-search-forward fj-url-regex nil :no-error)
    (unless
        (save-excursion
          (goto-char (1- (point)))
          (or (markdown-inside-link-p)
              ;; bbcode (seen in spam, breaks markdown if url replaced):
              (let ((regex (concat "\\[url=" markdown-regex-uri "\\/\\]"
                                   ".*" ; description
                                   "\\[\\/url\\]")))
                (thing-at-point-looking-at regex))))
      (replace-match
       (concat "<" (match-string 0) ">")))))

(defvar fj-previous-window-config nil
  "A list of window configuration prior to composing a toot.
Takes its form from `window-configuration-to-register'.")

(defun fj-restore-previous-window-config (config)
  "Restore the window CONFIG after killing the toot compose buffer.
Buffer-local variable `fj-previous-window-config' holds the config."
  (set-window-configuration (car config))
  (goto-char (cadr config)))

;; I think magit/forge just uses markdown-mode rather than rendering
(defun fj-render-body (body)
  "Render item BODY as markdowned html.
JSON is the item's data to process the link with."
  ;; NB: make sure this doesn't leak into our issue buffers!
  (let ((buf "*fj-md*")
        str)
    ;; shr.el fucks windows up, so we save and restore:
    (setq fj-previous-window-config
          (list (current-window-configuration)
                (point-marker)))
    ;; 1: temp buffer, prepare for md
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (fj-mdize-plain-urls)
      (goto-char (point-min))
      ;; 2: md-ize or fallback
      (let ((old-buf (buffer-string)))
        (condition-case nil
            (markdown-standalone buf)
          (t ; if rendering fails, return unrendered body:
           (with-current-buffer buf
             (erase-buffer)
             (insert old-buf)))))
      ;; 3: shr-render the md
      (with-current-buffer (get-buffer-create buf)
        (let ((shr-width (window-width))
              (shr-discard-aria-hidden t)) ; for pandoc md image output
          ;; shr render:
          (shr-render-buffer (current-buffer))))
      ;; 4 collect result
      (with-current-buffer "*html*"
        (re-search-forward "\n\n" nil :no-error)
        (setq str (buffer-substring (point) (point-max)))
        (kill-buffer-and-window)        ; shr's *html*
        (kill-buffer buf)))             ; our md
    (fj-restore-previous-window-config fj-previous-window-config)
    str))

(defvar fj-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'fj-issue-next)
    (define-key map (kbd "p") #'fj-issue-prev)
    (define-key map (kbd "g") #'fj-issue-view-reload)
    (define-key map (kbd "e") #'fj-issue-view-edit)
    (define-key map (kbd "c") #'fj-issue-view-comment)
    (define-key map (kbd "k") #'fj-issue-view-close)
    map)
  "Keymap for `fj-issue-view-mode'.")

(define-derived-mode fj-issue-view-mode special-mode "fj-issue"
  "Major mode for viewing an issue."
  :group 'fj
  (read-only-mode 1))

(defun fj-render-comments (comments &optional author owner)
  "Render a list of COMMENTS.
AUTHOR is the author of the parent issue.
OWNER is the repo owner."
  (cl-loop for c in comments
           concat (let-alist c
                    (let ((stamp (fedi--relative-time-description
                                  (date-to-time .created_at))))
                      (propertize
                       (concat
                        ;; FIXME: no better way than this?
                        (propertize (concat .user.username " ")
                                    'face 'fj-item-author-face
                                    'fj-byline t)
                        (fj-author-or-owner-str .user.username author)
                        (propertize " "
                                    'face 'fj-item-author-face)
                        (fj-author-or-owner-str .user.username nil owner)
                        (propertize (fj-issue-right-align-str stamp)
                                    'face 'fj-item-author-face)
                        "\n\n"
                        (fj-render-body .body)
                        "\n"
                        fedi-horiz-bar fedi-horiz-bar "\n\n")
                       'fj-comment c
                       'fj-comment-author .user.username
                       'fj-comment-id .id)))))

(defun fj-author-or-owner-str (username author &optional owner)
  "If USERNAME is equal either AUTHOR or OWNER, return a boxed string."
  ;; FIXME: improve this junk
  (if owner
      (if (equal owner username)
          (propertize "owner"
                      'face '(:inherit fj-item-author-face :box t))
        "")
    (if (equal author username)
        (propertize "author"
                    'face '(:inherit fj-item-author-face :box t))
      "")))

(defun fj-render-labels (labels)
  "Render LABELS, a list of issue labels."
  (concat "\nLabels: "
          (cl-loop for l in labels
                   concat (concat (propertize (alist-get 'name l)
                                              'face 'fj-issue-label-face)
                                  " "))))

(defun fj-render-issue (repo owner issue number comments &optional reload)
  "Render an ISSUE number NUMBER, in REPO and its COMMENTS.
OWNER is the repo owner.
RELOAD mean we reloaded."
  (fedi-with-buffer (format "*fj-issue-%s" number) 'fj-issue-view-mode
                    (not reload)
    (let ((header-line-indent " "))
      (header-line-indent-mode 1) ; broken?
      (let-alist issue
        (let ((stamp (fedi--relative-time-description
                      (date-to-time .created_at))))
          .is_locked
          (setq header-line-format
                `("" header-line-indent
                  ,(concat "#" (number-to-string .number) " "
                           (propertize .title
                                       'face 'fj-item-face))))
          (insert
           ;; header stuff:
           "State: " .state
           (if .labels
               (fj-render-labels .labels)
             "")
           "\n\n"
           (propertize
            (concat
             ;; issue stuff:
             ;; FIXME: :extend t doesn't work here whatever i do
             (propertize (concat .user.username " ")
                         'face 'fj-item-author-face
                         'fj-byline t
                         'fj-issue issue)
             (fj-author-or-owner-str .user.username nil owner)
             (propertize (fj-issue-right-align-str stamp)
                         'face 'fj-item-author-face)
             "\n\n"
             (fj-render-body .body)
             "\n"
             fedi-horiz-bar "\n\n"
             ;; comments
             (fj-render-comments comments .user.username owner))
            'fj-issue number
            'fj-repo repo))
          (setq fj-current-repo repo)
          (setq fj-issue-spec
                `(:repo ,repo :owner ,owner :issue ,number
                        :author ,.user.username :title ,.title
                        :body ,.body :url ,.url)))))))

(defun fj-issue-view (&optional repo owner number reload)
  "View issue NUMBER from REPO of OWNER.
RELOAD means we are reloading, so don't open in other window."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (fj-get-issue repo owner number))
         (number (or number (alist-get 'number issue)))
         (comments (fj-issue-get-comments repo owner number)))
    (fj-render-issue repo owner issue number comments reload)))

;; (defun fj-issue-view-comment ()
;;   "Comment on the issue currently being viewed."
;;   (interactive)
;;   (fj-with-issue
;;    (let ((number (plist-get fj-issue-spec :issue))
;;          (owner (plist-get fj-issue-spec :owner))
;;          (repo (plist-get fj-issue-spec :repo)))
;;      (fj-issue-comment repo number))))

(defun fj-issue-view-reload ()
  "Reload the current issue view."
  (interactive)
  (fj-with-issue
   (let ((number (plist-get fj-issue-spec :issue))
         (owner (plist-get fj-issue-spec :owner)))
     (fj-issue-view fj-current-repo owner
                    number :reload))))

;; TODO: merge simple action functions
(defun fj-issue-view-close ()
  "Close issue being viewed."
  (interactive)
  (fj-with-issue
   (let ((number (plist-get fj-issue-spec :issue))
         (owner (plist-get fj-issue-spec :owner))
         (repo (plist-get fj-issue-spec :repo)))
     (fj-issue-close repo owner number))))

(defvar fj-compose-spec nil)

(defvar fj-compose-issue-number nil)

(defun fj-issue-view-edit ()
  "Edit the issue currently being viewed."
  (interactive)
  (fj-with-own-issue
   (let ((number (plist-get fj-issue-spec :issue))
         (repo (plist-get fj-issue-spec :repo))
         (owner (plist-get fj-issue-spec :owner))
         (title (plist-get fj-issue-spec :title))
         (body (plist-get fj-issue-spec :body)))
     (fj-issue-compose :edit nil 'issue body)
     (setq fj-compose-issue-title title
           fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-issue-number number)
     (fedi-post--update-status-fields))))

(defun fj-issue-view-comment ()
  "Comment on the issue currently being viewed."
  (interactive)
  (let ((number (plist-get fj-issue-spec :issue))
        (repo (plist-get fj-issue-spec :repo))
        (owner (plist-get fj-issue-spec :owner)))
    (fj-issue-compose nil 'fj-compose-comment-mode 'comment)
    (setq fj-compose-repo repo
          fj-compose-repo-owner owner
          fj-compose-issue-number number)
    (fedi-post--update-status-fields)))

(defun fj-issue-view-edit-comment ()
  "Edit the comment at point."
  (interactive)
  (fj-with-own-comment
   (let ((number (fj--property 'fj-comment-id))
         (repo (plist-get fj-issue-spec :repo))
         (owner (plist-get fj-issue-spec :owner))
         (body (alist-get 'body
                          (fj--property 'fj-comment))))
     (fj-issue-compose :edit 'fj-compose-comment-mode 'comment body)
     (setq fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-comment-number number)
     (fedi-post--update-status-fields))))

;; fj-compose-spec
;; `(:title ,title :repo ,repo :owner ,owner :issue ,issue))))))

;;; SEARCH

(defun fj-repo-search (query &optional topic)
  "Search repos for QUERY.
If TOPIC, QUERY is a search for topic keywords."
  (interactive "sSearch for repo: ")
  (let* ((params `(("q" . ,query)
                   ("limit" . "100")
                   ("sort" . "updated")
                   ,(when topic
                      '("topic" . "t"))))
         (resp (fj-get "/repos/search" params))
         (data (alist-get 'data resp))
         (cands (fj-get-repo-candidates data))
         (completion-extra-properties
          '(:annotation-function fj-repo-candidates-annot-fun))
         (choice (completing-read "Repo: " cands))
         (user (cl-fourth
                (assoc choice cands #'equal))))
    (fj-list-issues choice nil nil user)))

;; doesn't work
(defun fj-repo-candidates-annot-fun (cand)
  "CAND."
  (cl-fourth
   (assoc cand minibuffer-completion-table
          #'equal)))

;;; SEARCH REPOS TL

(defvar fj-repo-tl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'fj-repo-tl-list-issues)
    map)
  "Map for `fj-repo-search-mode a tabluated list of repos.")

(define-derived-mode fj-repo-tl-mode tabulated-list-mode
  "fj-repo-search"
  "Mode for displaying a tabulated list of repo search results."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0) ;2) ; point directly on issue
  (setq tabulated-list-format
        '[("Name" 16 t)
          ("Owner" 12 t)
          ("★" 3 t)
          ("" 2 t)
          ("Lang" 10 t)
          ("Description" 55 nil)]))

(define-button-type 'fj-search-repo-button
  'follow-link t
  'action 'fj-repo-tl-list-issues
  'help-echo "RET: View this repo's issues.")

(define-button-type 'fj-search-owner-button
  'follow-link t
  'action 'fj-repo-tl-list-user-repos
  'help-echo "RET: View this user.")

(define-button-type 'fj-issues-owner-button
  'follow-link t
  'action 'fj-issues-tl-list-user-repos
  'help-echo "RET: View this user.")

(defun fj-search-tl-entries (repos &optional no-owner)
  "Return tabluated list entries for REPOS.
NO-OWNER means don't display owner column (user repos view)."
  (cl-loop for r in repos
           for id = (alist-get 'id r)
           for name = (alist-get 'name r)
           for desc = (string-replace "\n" " "
                                      (alist-get 'description r))
           for owner = (unless no-owner
                         (alist-get 'username
                                    (alist-get 'owner r)))
           for lang = (alist-get 'language r)
           ;; for url = (alist-get 'html_url r)
           for stars = (number-to-string
                        (alist-get 'stars_count r))
           for fork = (let ((status (alist-get 'fork r)))
                        (if (eq status :json-false)
                            "ℹ"
                          "⑂"))
           collect
           (if no-owner
               ;; user repo button:
               `(nil [(,name face fj-item-face
                             id ,id
                             type fj-user-repo-button)
                      (,stars id ,id face fj-figures-face)
                      (,fork id ,id face fj-figures-face)
                      ,lang
                      ,(propertize desc
                                   'face 'fj-comment-face)])
             ;; search-repo and search owner button:
             `(nil [(,name face fj-item-face
                           id ,id
                           type fj-search-repo-button)
                    (,owner face fj-user-face
                            id ,id
                            type fj-search-owner-button)
                    (,stars id ,id face fj-figures-face)
                    (,fork id ,id face fj-figures-face)
                    ,lang
                    ,(propertize desc
                                 'face 'fj-comment-face)]))))

(defun fj-repo-search-tl (query &optional topic)
  "Search repos for QUERY, and display a tabulated list of results.
TOPIC, a boolean, means search in repo topics."
  (interactive "sSearch for repos: ")
  (let* ((params `(("q" . ,query)
                   ("limit" . "100")
                   ("sort" . "updated")
                   ,(when topic
                      '("topic" . "t"))))
         (resp (fj-get "/repos/search" params))
         (buf (format "*fj-search-%s*" query))
         (data (alist-get 'data resp))
         (entries (fj-search-tl-entries data))
         (prev-buf (current-buffer)))
    (fj-repos-tl-render buf entries #'fj-repo-tl-mode prev-buf)))

(defun fj-repos-tl-render (buf entries mode prev-buf)
  "RENDER a tabulated list in BUF fer, with ENTRIES, in MODE."
  (with-current-buffer (get-buffer-create buf)
    (setq tabulated-list-entries entries)
    (funcall mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (cond
     ;; ((string= buf-name prev-buf) ; same repo
     ;;  nil)
     ;; ((string-suffix-p "-issues*" prev-buf) ; diff repo
     ;;  (switch-to-buffer (current-buffer)))
     ((string-prefix-p "*fj-search" buf) ;; any search
      (switch-to-buffer (current-buffer)))
     (t                             ; new buf
      (switch-to-buffer-other-window (current-buffer))))))

(defun fj-repo-tl-list-issues (&optional _)
  "View issues of current repo from tabulated repos listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (name (car (seq-first item)))
         (user (car (seq-elt item 1))))
    (fj-list-issues name nil nil user)))

(defun fj-repo-tl-list-user-repos (&optional _)
  "View repos of current entry user from tabulated repos listing."
  (interactive)
  ;; FIXME: this really needs to be somehow independent of columns
  ;; as we want to have author in diff columns depending on different views.
  ;; text-props to the rescue? (but they're also per column)
  ;; tabulated lists should be prop lists!
  (let* ((item (tabulated-list-get-entry))
         (user (car (seq-elt item 1))))
    (fj-user-repos-tl user)))

(defun fj-issues-tl-list-user-repos (&optional _)
  "View a tabulated list of current user from issues listing."
  (interactive)
  (let* ((item (tabulated-list-get-entry))
         (user (car (seq-elt item 2))))
    (fj-user-repos-tl user)))

;;; COMPOSING

(defvar fj-post-last-buffer nil)

(defvar-local fj-compose-repo nil)

(defvar-local fj-compose-repo-owner nil)

(defvar-local fj-compose-issue-title nil)

(defvar-local fj-compose-item-type nil)

(defalias 'fj-compose-cancel #'fedi-post-cancel)

(defvar fj-compose-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'fj-compose-cancel)
    (define-key map (kbd "C-c C-c") #'fj-compose-send)
    map)
  "Keymap for `fj-compose-comment-mode'.")

(define-minor-mode fj-compose-comment-mode
  "Minor mode for composing issues."
  :keymap fj-compose-comment-mode-map
  :global nil)

(defvar fj-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'fj-compose-read-title)
    (define-key map (kbd "C-c C-r") #'fj-compose-read-repo)
    (define-key map (kbd "C-c C-k") #'fj-compose-cancel)
    (define-key map (kbd "C-c C-c") #'fj-compose-send)
    map)
  "Keymap for `fj-compose-mode'.")

(define-minor-mode fj-compose-mode
  "Minor mode for composing issues and comments."
  :keymap fj-compose-mode-map
  :global nil)

(defun fj-compose-read-repo ()
  "Read a repo for composing a issue or comment."
  (interactive)
  ;; FIXME: combine own repos and search:
  (setq fj-compose-repo
        (fj-read-user-repo-do fj-compose-repo))
  (fedi-post--update-status-fields))

(defun fj-compose-read-title ()
  "Read an issue title."
  (interactive)
  (setq fj-compose-issue-title
        (read-string "Title: "
                     fj-compose-issue-title))
  (fedi-post--update-status-fields))

(defun fj-issue-compose (&optional edit mode type init-text)
  "Compose a new post.
EDIT means we are editing.
MODE is the fj.el minor mode to enable in the compose buffer.
TYPE is a symbol of what we are composing, it may be issue or comment.
Inject INIT-TEXT into the buffer, for editing."
  (interactive)
  (setq fj-post-last-buffer (buffer-name (current-buffer)))
  (fedi-post--compose-buffer
   edit
   #'markdown-mode
   (or mode #'fj-compose-mode)
   (when mode "fj-compose")
   (or type 'issue)
   ;; (list #'lem-post--mentions-capf
   ;; #'lem-post--comms-capf)
   nil
   ;; TODO: why not have a compose-buffer-spec rather than 10 separate vars?
   (unless type ; post
     '(((name . "title")
        (prop . compose-title)
        (item-var . fj-compose-issue-title)
        (face . lem-post-title-face))
       ((name . "repo")
        (prop . compose-repo)
        (item-var . fj-compose-repo)
        (face . link))))
   init-text)
  (setq fj-compose-item-type
        (if edit
            (if (eq type 'comment)
                'edit-comment
              'edit-issue)
          (if (eq type 'comment)
              'new-comment
            'new-issue))))

(defun fj-compose-send ()
  "Submit the issue or comment to your Forgejo instance.
Call response and update functions."
  (interactive)
  (let ((buf (buffer-name))
        (type fj-compose-item-type))
    (if (and (or (eq type 'new-issue)
                 (eq type 'edit-issue))
             (not (and fj-compose-repo
                       fj-compose-issue-title)))
        (user-error "You need to set a repo and title")
      (let* ((body (fedi-post--remove-docs))
             (response
              (cond ((eq type 'new-comment)
                     (fj-issue-comment fj-compose-repo
                                       fj-compose-repo-owner
                                       fj-compose-issue-number
                                       body))
                    ((eq type 'edit-comment)
                     (fj-issue-comment-edit fj-compose-repo
                                            fj-compose-repo-owner
                                            fj-compose-comment-number
                                            body))
                    ((eq type 'edit-issue)
                     (fj-issue-patch fj-compose-repo
                                     fj-compose-repo-owner
                                     fj-compose-issue-number
                                     fj-compose-issue-title
                                     body))
                    (t ; new issue
                     (fj-issue-post fj-compose-repo
                                    fj-compose-repo-owner
                                    fj-compose-issue-title body)))))
        (when response
          (with-current-buffer buf
            (fedi-post-kill))
          (if (not (eq type 'new-issue))
              (fj-issue-view-reload)
            (fj-list-issues fj-compose-repo)))))))

;;; NOTIFICATIONS

(defvar fj-notifications-status-types
  '("unread" "read" "pinned")
  "List of possible status types for getting notifications.")

(defvar fj-notifications-subject-types
  '("issue" "pull" "commit" "repository")
  "List of possible subject types for getting notifications.")

(defun fj-get-notifications (&optional all) ; status-types subject-type)
                                        ; before since page limit
  "GET notifications for `fj-user'.
ALL is a boolean string, meaning also show read notifications.
STATUS-TYPES and SUBJECT-TYPE are array strings."
  (let ((params `(("all" . ,all)))
        (endpoint "notifications"))
    (fj-get endpoint params)))

(defun fj-get-new-notifications-count ()
  "Return the number of new notifications for `fj-user'."
  (alist-get 'new
             (fj-get "notifications/new")))

(provide 'fj)
;;; fj.el ends here
