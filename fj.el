;;; fj.el --- Client for forgejo instances -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Copyright (C) 2023 Marty Hiatt <martianhiatus AT riseup.net>
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

(defvar-local fj-buffer-spec nil
  "A plist holding some basic info about the current buffer.
Repo, owner, item number, url.")

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

(defface fj-issue-commit-face
  '((t :inherit (link ;font-lock-comment-face
                 highlight)))
  "Face for issue commit references.")

(defface fj-name-face
  '((t :weight bold))
  "Face for timeline item names (user, issue, PR).
Not used for items that are links.")

;;; UTILS

(defun fj--property (prop)
  "Get text property PROP at point, else return nil."
  (get-text-property (point) prop))

(defun fj--get-buffer-spec (key)
  "Get entry for KEY from `fj-buffer-spec', else return nil."
  (plist-get fj-buffer-spec key))

(defun fj-issues-tl-own-repo-p ()
  "T if repo is owned by `fj-user'."
  ;; NB: is also T in issues TL!
  (equal (fj--get-buffer-spec :owner)
         fj-user))

(defun fj-issue-own-p ()
  "T if issue is authored by `fj-user'.
Works in issue view mode or in issues tl."
  (cond ((eq major-mode 'fj-issue-view-mode)
         (equal fj-user
                (fj--get-buffer-spec :author)))
        ((eq major-mode 'fj-issue-tl-mode)
         (let* ((entry (tabulated-list-get-entry))
                (author (car (seq-elt entry 2))))
           (equal fj-user author)))))

(defun fj-comment-own-p ()
  "T if comment is authored by `fj-user'."
  (and (eq major-mode 'fj-issue-view-mode)
       (equal fj-user (fj--property 'fj-comment-author))))

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

(defun fj--repo-owner ()
  "Return repo owner, whatever view we are in."
  (if (eq major-mode #'fj-repo-tl-mode)
      (let ((entry (tabulated-list-get-entry)))
        (car (seq-elt entry 1)))
    ;;(eq major-mode #'fj-user-repo-tl-mode)
    (fj--get-buffer-spec :owner)))

(defun fj--repo-name ()
  "Return repo name, whatever view we are in."
  (or (fj--get-buffer-spec :repo)
      fj-current-repo
      (let ((entry (tabulated-list-get-entry)))
        (car (seq-elt entry 0)))))

;;; MACROS

(defmacro fj-with-issue (&optional body)
  "Execute BODY if we are in an issue view or if issue at point."
  (declare (debug t))
  `(if (not (or (fj--get-buffer-spec :issue)
                (eq 'issue (fj--property 'item))))
       (user-error "Not issue here?")
     ,body))

(defmacro fj-with-issue-view (&optional body)
  "Execute BODY if we are in an issue view."
  (declare (debug t))
  `(if (not (fj--get-buffer-spec :issue))
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

(defmacro fj-with-entry (&optional body)
  "Execute BODY if we have a tabulated list entry at point."
  (declare (debug t))
  `(if (not (tabulated-list-get-entry))
       (user-error "No entry at point")
     ,body))

(defmacro fj-with-own-entry (&optional body)
  "Execute BODY if the tabulated list entry at point is owned by `fj-user'."
  (declare (debug t))
  `(fj-with-entry
    (if (not (fj-issue-own-p))
        (user-error "No an entry you own")
      ,body)))

(defmacro fj-with-repo-entry (&optional body)
  "Execute BODY if we have a repo tabulated list entry at point."
  (declare (debug t))
  `(if (or (not (tabulated-list-get-entry))
           (eq major-mode #'fj-issue-tl-mode))
       (user-error "No repo entry at point")
     ,body))

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

(defun fj-get (endpoint &optional params no-json)
  "Make a GET request to ENDPOINT.
PARAMS is any parameters to send with the request.
NO-JSON means return the raw response."
  (let* ((url (fj-api endpoint))
         (resp (fj-authorized-request "GET"
                 (if no-json
                     (fedi-http--get url params)
                   (fedi-http--get-json url params)))))
    (if no-json
        resp
      (cond ((or (eq (caar resp) 'errors)
                 (eq (caar resp) 'message))
             (user-error "I am Error: %s Endpoint: %s"
                         (alist-get 'message resp)
                         endpoint))
            (t
             resp)))))

(defun fj-post (endpoint &optional params)
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

(defun fj-get-repo (repo owner)
  "GET REPO owner by OWNER."
  (let* ((endpoint (format "repos/%s/%s/" owner repo)))
    (fj-get endpoint)))

(define-button-type 'fj-user-repo-button
  'follow-link t
  'action 'fj-repo-tl-list-issues
  'help-echo "RET: View this repo's issues.")

(defvar fj-repo-tl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'fj-repo-tl-list-issues)
    (define-key map (kbd "*") #'fj-repo-tl-star-repo)
    (define-key map (kbd "c") #'fj-create-issue)
    (define-key map (kbd "O") #'fj-list-own-repos)
    (define-key map (kbd "s") #'fj-repo-search-tl)
    (define-key map (kbd "u") #'fj-list-user-repos)
    (define-key map (kbd "B") #'fj-tl-browse-entry)
    (define-key map (kbd "L") #'fj-repo-copy-clone-url)
    (define-key map (kbd "b") #'fj-browse-view)
    (define-key map (kbd "j") #'imenu)
    (define-key map (kbd "g") #'fj-repo-tl-reload)
    (define-key map (kbd "N") #'fj-view-notifications)
    map)
  "Map for `fj-repo-tl-mode' and `fj-user-repo-tl-mode' to inherit.")

(defvar fj-user-repo-tl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fj-repo-tl-map)
    ;; (define-key map (kbd "g") #'fj-repo-tl-reload)
    map)
  "Map for `fj-user-repo-tl-mode', a tabluated list of repos.")

(define-derived-mode fj-user-repo-tl-mode tabulated-list-mode
  "fj-user-repos"
  "Mode for displaying a tabulated list of user repos."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("Name" 16 t)
          ("★" 2 t)
          ("" 2 t)
          ("issues" 5 t)
          ("Lang" 10 t)
          ("Updated" 12 t)
          ("Description" 55 nil)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(defun fj-get-current-user ()
  "Return the data for the current user."
  (fj-get "user"))

(defun fj-get-user-repos (user)
  "GET request repos for USER."
  (let ((params '(("limit" . "100")))
        (endpoint (format "users/%s/repos" user)))
    (fj-get endpoint params)))

(defun fj-user-repos-tl (&optional user)
  "View a tabulated list of respos for USER."
  (interactive "sView user repos: ")
  (let* ((repos (fj-get-user-repos user))
         (entries (fj-repo-tl-entries repos :no-owner))
         (buf (format "*fj-repos-%s*" user)))
    (with-current-buffer
        (fj-repos-tl-render buf entries #'fj-user-repo-tl-mode)
      (setq fj-buffer-spec
            `(:owner ,user :url ,(concat fj-host "/" user))))))

(defun fj-list-own-repos ()
  "List repos for `fj-user'."
  (interactive)
  (fj-user-repos-tl fj-user))

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

(defun fj-fork-repo (repo owner &optional name) ; org
  "Fork REPO owned by OWNER, optionally call fork NAME."
  (let* ((endpoint (format "repos/%s/%s/forks" owner repo))
         (params `(("name" . ,name)))
         ;; ("organization" . ,org)))
         (resp (fj-post endpoint params)))
    (fedi-http--triage resp
                       (lambda ()
                         (message "Repo forked!" repo)))))

;;; USER REPOS

(defun fj-current-dir-repo ()
  "If we are in a `fj-host' repository, return its name."
  ;; NB: fails if remote url is diff to root dir!
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

(defun fj-repo-get-issues (repo &optional owner state type query)
  "Return issues for REPO by OWNER.
STATE is for issue status, a string of open, closed or all.
TYPE is item type: issue pull or all."
  ;; FIXME: how to get issues by number, or get all issues?
  (let* ((endpoint (format "repos/%s/%s/issues" (or owner fj-user) repo))
         (params `(("state" . ,state)
                   ("limit" . "100")
                   ("type" . ,type)
                   ("q" . ,query))))
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

;; (defun fj-issue-create (&optional repo user)
;;   "Create an issue in REPO owned by USER."
;;   (interactive "P")
;;   (let* ((repo (fj-read-user-repo repo))
;;          (user (or user fj-user))
;;          (title (read-string "Title: "))
;;          (body (read-string "Body: "))
;;          (response (fj-issue-post repo user title body)))
;;     (fedi-http--triage response
;;                        (lambda ()
;;                          (message "issue %s created!" title)
;;                          (fj-issues-tl-reload)))))

(defun fj-issue-post (repo user title body)
  "POST a new issue to REPO owned by USER.
TITLE and BODY are the parts of the issue to send."
  (let ((url (format "repos/%s/%s/issues" user repo))
        (params `(("body" . ,body)
                  ("title" . ,title))))
    (fj-post url params)))

(defun fj-issue-patch (repo owner issue &optional title body state)
  "PATCH/Edit ISSUE in REPO.
With PARAMS.
OWNER is the repo owner."
  (let* ((params `(("body" . ,body)
                   ("title" . ,title)
                   ("state" . ,state)))
         (endpoint (format "repos/%s/%s/issues/%s" owner repo issue)))
    (fj-patch endpoint params :json)))

(defun fj-issue-edit (&optional repo owner issue)
  "Edit ISSUE body in REPO.
OWNER is the repo owner."
  (interactive)
  (fj-with-own-issue ;; or owner?
   (let* ((repo (fj-read-user-repo repo))
          (issue (or issue (fj-read-repo-issue repo)))
          (owner (or owner ;; FIXME: owner
                     (fj--get-buffer-spec :owner)
                     fj-user))
          (data (fj-get-issue repo owner issue))
          (old-body (alist-get 'body data))
          (new-body (read-string "Edit issue body: " old-body))
          (response (fj-issue-patch repo owner issue nil new-body)))
     (fedi-http--triage response
                        (lambda ()
                          (message "issue edited!"))))))

(defun fj-issue-edit-title (&optional repo owner issue)
  "Edit ISSUE title in REPO.
OWNER is the repo owner."
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner ;; FIXME: owner
                    (fj--get-buffer-spec :owner)
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
      (let ((response (fj-issue-patch repo owner issue nil nil state)))
        (fedi-http--triage response
                           (lambda ()
                             (message "Issue %s %s!" issue state)))))))

(defun fj-issue-delete (&optional repo owner issue no-confirm)
  "Delete ISSUE in REPO of OWNER.
Optionally, NO-CONFIRM means don't ask before deleting."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo))))
    (when (or no-confirm
              (y-or-n-p "Delete issue?"))
      (let* ((url (format "repos/%s/%s/issues/%s" owner repo issue))
             (response (fj-delete url)))
        (fedi-http--triage response
                           (lambda ()
                             (message "issue deleted!")))))))

;;; PULL REQUESTS
;; TODO: owner args not `fj-user'
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

;; (defun fj-list-pull-reqs (&optional repo)
;;   "List pull requests for REPO."
;;   (interactive "P")
;;   (let* ((repo (fj-read-user-repo repo))
;;          (prs (fj-repo-get-pull-reqs repo)))
;;     (fj-list-issues repo nil prs))) ;; FIXME: owner

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

(defun fj-issue-get-comments-timeline (repo owner issue)
  "Return comments timeline for ISSUE in REPO.
OWNER is the repo owner.
Comments timeline contains comments of any type."
  (let* ((endpoint (format "repos/%s/%s/issues/%s/timeline"
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

(defun fj-comment-patch (repo owner id &optional params issue)
  "Edit comment with ID in REPO owned by OWNER.
PARAMS."
  (let* ((id (or id (fj-read-item-comment repo owner issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s" owner repo id)))
    (fj-patch endpoint params :json)))

(defun fj-issue-comment-edit (&optional repo owner id new-body)
  "Edit comment with ID in REPO.
OWNER is the repo owner.
NEW-BODY is the new comment text to send."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (id (or id (fj--property 'fj-comment-id)))
         (owner (or owner fj-user)) ;; FIXME
         (response (fj-comment-patch repo owner id
                                     `(("body" . ,new-body)))))
    (fedi-http--triage response
                       (lambda ()
                         (message "comment edited!")))))

;;; ISSUES TL

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

(defvar fj-issue-tl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map) ; has nav
    (define-key map (kbd "C") #'fj-issues-tl-comment)
    (define-key map (kbd "e") #'fj-issues-tl-edit)
    (define-key map (kbd "t") #'fj-issues-tl-edit-title)
    (define-key map (kbd "v") #'fj-issues-tl-view)
    (define-key map (kbd "k") #'fj-issues-tl-close)
    (define-key map (kbd "K") #'fj-issues-tl-delete)
    (define-key map (kbd "c") #'fj-create-issue)
    (define-key map (kbd "g") #'fj-issues-tl-reload)
    (define-key map (kbd "C-c C-c") #'fj-list-issues-cycle)
    (define-key map (kbd "o") #'fj-issues-tl-reopen)
    (define-key map (kbd "s") #'fj-list-issues-search)
    (define-key map (kbd "S") #'fj-repo-search-tl)
    (define-key map (kbd "u") #'fj-list-user-repos)
    (define-key map (kbd "O") #'fj-list-own-repos)
    (define-key map (kbd "B") #'fj-tl-browse-entry)
    (define-key map (kbd "b") #'fj-browse-view)
    (define-key map (kbd "N") #'fj-view-notifications)
    (define-key map (kbd "L") #'fj-repo-copy-clone-url)
    (define-key map (kbd "j") #'imenu)
    map)
  "Map for `fj-issue-tl-mode', a tabluated list of issues.")

(define-derived-mode fj-issue-tl-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a tabulated list of issues."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        ;; this is changed by `tabulated-list-sort' which sorts by col at point:
        tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("#" 5 t)
          ("💬" 3 t)
          ("Author" 10 t)
          ("Updated" 12 t) ;; instead of display, you could use a sort fun here
          ("Issue" 20 t)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(define-button-type 'fj-issue-button
  'follow-link t
  'action 'fj-issues-tl-view
  'help-echo "RET: View this issue.")

(defun fj-issue-tl-entries (issues)
  "Return tabluated list entries for ISSUES.
STATE is a string."
  (cl-loop
   for issue in issues
   collect
   (let-alist issue
     (let* ((updated (date-to-time .updated_at))
            (updated-str (format-time-string "%s" updated))
            (updated-display (fedi--relative-time-description updated nil :brief))
            (type (if .pull_request 'pull 'issue)))
       `(nil ;; TODO: id
         [(,(number-to-string .number)
           id ,.id
           state ,.state
           type fj-issue-button
           item ,type
           fj-url ,.html_url)
          ,(propertize (number-to-string .comments)
                       'face 'fj-figures-face
                       'item type)
          (,.user.username face fj-user-face
                           id ,.id
                           state ,.state
                           type  fj-issues-owner-button
                           item ,type)
          ,(propertize updated-str
                       'display updated-display
                       'item type)
          (,.title face ,(if (equal .state "closed")
                             'fj-closed-issue-face
                           'fj-item-face)
                   id ,.id
                   state ,.state
                   type fj-issue-button
                   item ,type)])))))

(defun fj-list-issues-+-pulls (repo &optional owner state)
  "List issues and pulls for REPO by OWNER, filtered by STATE."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo)))
    (fj-list-issues repo owner state "all")))

(defun fj-list-pulls (repo &optional owner state)
  "List pulls for REPO by OWNER, filtered by STATE."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo)))
    (fj-list-issues repo owner state "pulls")))

(defun fj-list-issues (repo &optional owner state type query)
  "Display ISSUES in a tabulated list view.
Either for `fj-current-repo' or REPO, a string, owned by OWNER.
With a prefix arg, or if REPO and `fj-current-repo' are nil,
prompt for a repo to list.
Optionally specify the STATE filter (open, closed, all), and the
TYPE filter (issues, pulls, all)."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (type (or type "issues"))
         ;; (alist-get 'owner
         ;; (alist-get 'repository (car issues)))))
         (issues (fj-repo-get-issues repo owner state type query))
         (repo-data (fj-get-repo repo owner))
         (url (concat (alist-get 'html_url repo-data)
                      "/issues"))
         (prev-buf (buffer-name (current-buffer)))
         (state-str (or state "open"))
         (buf-name (format "*fj-%s-%s-%s*" repo state-str type)))
    (with-current-buffer (get-buffer-create buf-name)
      (setq tabulated-list-entries
            (fj-issue-tl-entries issues))
      (fj-issue-tl-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq fj-current-repo repo)
      (setq fj-buffer-spec
            `(:repo ,repo :state ,state-str :owner ,owner :url ,url
                    :type ,type))
      (cond ((string= buf-name prev-buf) ; same repo
             nil)
            ((string-suffix-p "-issues*" prev-buf) ; diff repo
             (switch-to-buffer (current-buffer)))
            (t                             ; new buf
             (switch-to-buffer-other-window (current-buffer)))))))

(defun fj-list-issues-search (query &optional state type)
  "Search current repo issues for QUERY.
STATE and TYPE as for `fj-list-issues'."
  (interactive "sSearch issues in repo: ")
  (let ((owner (fj--get-buffer-spec :owner)))
    (fj-list-issues nil owner (or state "all") type query)))

(defun fj-list-issues-closed (&optional repo owner type)
  "Display closed ISSUES for REPO by OWNER in tabulated list view."
  (interactive "P")
  (fj-list-issues repo owner "closed" type))

(defun fj-list-issues-all (&optional repo owner type)
  "Display all ISSUES for REPO by OWNER in tabulated list view."
  (interactive "P")
  (fj-list-issues repo owner "all" type))

(defun fj-list-issues-cycle ()
  "Cycle between listing of open, closed, and all issues."
  (interactive)
  (let ((state (fj--get-buffer-spec :state))
        (owner (fj--get-buffer-spec :owner))
        (repo (fj--get-buffer-spec :repo))
        (type (fj--get-buffer-spec :type)))
    (cond ((string= state "closed")
           (fj-list-issues-all repo owner type))
          ((string= state "all")
           (fj-list-issues repo owner nil type))
          (t ; open is default
           (fj-list-issues-closed repo owner type)))))

(defun fj-issues-tl-reload ()
  "Reload current issues tabulated list view."
  (interactive)
  (when (string-suffix-p "-issues*"
                         (buffer-name (current-buffer)))
    (let ((state (fj--get-buffer-spec :state))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--get-buffer-spec :repo)))
      (fj-list-issues repo owner state))))

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
;; FIXME: use POST /markdown on the instance to render!
(defun fj-render-body (body &optional json)
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
    ;; propertize special items:
    (setq str
          (fedi-propertize-items str fedi-post-handle-regex 'handle json
                                 fj-link-keymap 1 2 nil nil
                                 '(fj-tab-stop t)))
    (setq str
          (fedi-propertize-items str fedi-post-tag-regex 'tag json
                                 fj-link-keymap 1 2 nil nil
                                 '(fj-tab-stop t)))
    (setq str
          (fedi-propertize-items str fedi-post-commit-regex 'commit json
                                 fj-link-keymap 1 1 nil nil
                                 '(fj-tab-stop t)
                                 'fj-issue-commit-face))
    (fj-restore-previous-window-config fj-previous-window-config)
    str))

(defvar fj-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'fj-issue-next)
    (define-key map (kbd "p") #'fj-issue-prev)
    (define-key map [?\t] #'fj-next-tab-item)
    (define-key map [backtab] #'fj-prev-tab-item)
    (define-key map (kbd "g") #'fj-issue-view-reload)
    (define-key map (kbd "e") #'fj-issue-view-edit-item-at-point)
    (define-key map (kbd "c") #'fj-issue-view-comment)
    (define-key map (kbd "k") #'fj-issue-view-close)
    (define-key map (kbd "o") #'fj-issue-view-reopen)
    (define-key map (kbd "K") #'fj-issue-view-comment-delete)
    (define-key map (kbd "s") #'fj-list-issues-search)
    (define-key map (kbd "S") #'fj-repo-search-tl)
    (define-key map (kbd "O") #'fj-list-own-repos)
    (define-key map (kbd "b") #'fj-browse-view)
    (define-key map (kbd "N") #'fj-view-notifications)
    map)
  "Keymap for `fj-issue-view-mode'.")

(define-derived-mode fj-issue-view-mode special-mode "fj-issue"
  "Major mode for viewing an issue."
  :group 'fj
  (read-only-mode 1))

(defun fj-format-comment (comment &optional author owner)
  "Render COMMENT.
AUTHOR is of comment, OWNER is of repo."
  (let-alist comment
    (let ((stamp (fedi--relative-time-description
                  (date-to-time .created_at))))
      (propertize
       (concat
        ;; TODO: improve this?
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
        (fj-render-body .body comment)
        "\n"
        fedi-horiz-bar fedi-horiz-bar) ; "\n")
       'fj-comment comment
       'fj-comment-author .user.username
       'fj-comment-id .id))))

(defun fj-render-comments (comments &optional author owner)
  "Render a list of COMMENTS.
AUTHOR is the author of the parent issue.
OWNER is the repo owner."
  (cl-loop for c in comments
           concat (fj-format-comment c author owner)))

(defun fj-author-or-owner-str (username author &optional owner)
  "If USERNAME is equal either AUTHOR or OWNER, return a boxed string."
  (let ((name (or owner author)))
    (if (equal name username)
        (propertize (if owner "owner" "author")
                    'face '(:inherit fj-item-author-face :box t))
      "")))

(defun fj-render-labels (labels)
  "Render LABELS, a list of issue labels."
  (concat "\nLabels: "
          (cl-loop for l in labels
                   concat (concat (propertize (alist-get 'name l)
                                              'face 'fj-issue-label-face)
                                  " "))))

(defun fj-render-issue (repo owner issue number timeline &optional reload)
  "Render an ISSUE number NUMBER, in REPO and its TIMELINE.
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
           ;; header stuff (forge has: state, status, milestone, labels, marks, assignees):
           (propertize
            (concat
             "State: " .state
             (if .labels
                 (fj-render-labels .labels)
               "")
             "\n\n"
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
             (fj-render-body .body issue)
             "\n"
             fedi-horiz-bar "\n\n")
            'fj-issue number
            'fj-repo repo
            'fj-item-data issue))
          ;; comments
          ;; (fj-render-comments comments .user.username owner))
          (fj-render-timeline timeline .user.username owner)
          (setq fj-current-repo repo)
          (setq fj-buffer-spec
                `(:repo ,repo :owner ,owner :issue ,number
                        :author ,.user.username :title ,.title
                        :body ,.body :url ,.html_url)))))))

(defun fj-issue-view (&optional repo owner number reload)
  "View issue NUMBER from REPO of OWNER.
RELOAD means we are reloading, so don't open in other window."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (fj-get-issue repo owner number))
         (number (or number (alist-get 'number issue)))
         (timeline (fj-issue-get-comments-timeline repo owner number)))
    (fj-render-issue repo owner issue number timeline reload)))

;; (defun fj-issue-view-comment ()
;;   "Comment on the issue currently being viewed."
;;   (interactive)
;;   (fj-with-issue
;;    (let ((number (fj--get-buffer-spec :issue))
;;          (owner (fj--get-buffer-spec :owner))
;;          (repo (fj--get-buffer-spec :repo)))
;;      (fj-issue-comment repo number))))

(defun fj-issue-view-reload ()
  "Reload the current issue view."
  (interactive)
  (fj-with-issue-view
   (let ((number (fj--get-buffer-spec :issue))
         (owner (fj--get-buffer-spec :owner)))
     (fj-issue-view fj-current-repo owner
                    number :reload))))

;; TODO: merge simple action functions
(defun fj-issue-view-close (&optional state)
  "Close issue being viewed, or set to STATE."
  (interactive)
  (fj-with-issue-view
   (let ((number (fj--get-buffer-spec :issue))
         (owner (fj--get-buffer-spec :owner))
         (repo (fj--get-buffer-spec :repo)))
     (fj-issue-close repo owner number state)
     (fj-issue-view-reload))))

(defun fj-issue-view-reopen ()
  "Reopen issue being viewed."
  (interactive)
  (fj-issue-view-close "open"))

(defun fj-issue-view-edit-item-at-point ()
  "Edit issue or comment at point in issue view mode."
  (interactive)
  (if (fj--property 'fj-comment)
      (fj-issue-view-edit-comment)
    (fj-issue-view-edit)))

(defun fj-issue-view-edit ()
  "Edit the issue currently being viewed."
  (interactive)
  (fj-with-own-issue
   (let ((number (fj--get-buffer-spec :issue))
         (repo (fj--get-buffer-spec :repo))
         (owner (fj--get-buffer-spec :owner))
         (title (fj--get-buffer-spec :title))
         (body (fj--get-buffer-spec :body)))
     (fj-issue-compose :edit nil 'issue body)
     (setq fj-compose-issue-title title
           fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-issue-number number)
     (fedi-post--update-status-fields))))

(defun fj-issue-view-comment ()
  "Comment on the issue currently being viewed."
  (interactive)
  (let ((number (fj--get-buffer-spec :issue))
        (repo (fj--get-buffer-spec :repo))
        (owner (fj--get-buffer-spec :owner))
        (title (fj--get-buffer-spec :title)))
    (fj-issue-compose nil 'fj-compose-comment-mode 'comment)
    (setq fj-compose-repo repo
          fj-compose-issue-title title
          fj-compose-repo-owner owner
          fj-compose-issue-number number)
    (fedi-post--update-status-fields)))

(defun fj-issue-view-edit-comment ()
  "Edit the comment at point."
  (interactive)
  (fj-with-own-comment
   (let ((id (fj--property 'fj-comment-id))
         (repo (fj--get-buffer-spec :repo))
         (owner (fj--get-buffer-spec :owner))
         (body (alist-get 'body
                          (fj--property 'fj-comment))))
     (fj-issue-compose :edit 'fj-compose-comment-mode 'comment body)
     (setq fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-comment-id id)
     (fedi-post--update-status-fields))))

(defun fj-issue-view-comment-delete ()
  "Delete comment at point."
  (interactive)
  (fj-with-own-comment
   (let* ((id (fj--property 'fj-comment-id))
          (repo (fj--get-buffer-spec :repo))
          (owner (fj--get-buffer-spec :owner))
          (endpoint (format "repos/%s/%s/issues/comments/%s" owner repo id)))
     (when (yes-or-no-p "Delete comment?")
       (fj-delete endpoint)
       (fj-issue-view-reload)))))

;;; TIMELINE ITEMS

(defvar fj-issue-timeline-action-str-alist
  ;; issues:
  '(("close" . "%s closed this issue %s")
    ("reopen" . "%s reopened this issue %s")
    ("comment_ref" . "%s referenced this issue %s")
    ("change_title" . "%s changed title from \"%s\" to \"%s\" %s")
    ("commit_ref" . "%s referenced this issue from a commit %s")
    ("issue_ref" . "%s referenced this issue from %s %s")
    ("label" . "%s added the %s label %s")
    ;; PRs:
    ("pull_push" . "%s %s %d commits %s")
    ("merge_pull" . "%s merged this pull request %s")
    ("pull_ref" . "%s referenced a PR that will close this %s")
    ("delete_branch" . "%s deleted branch %s %s")))

(defun fj-render-timeline (data &optional author owner)
  "Render timeline DATA.
DATA contains all types of issue comments (references, name
changes, commit references, etc.)."
  (cl-loop for i in data
           do (fj-render-timeline-item i author owner)))

(defun fj-render-timeline-item (item &optional author owner)
  "Render timeline ITEM."
  (let-alist item
    (let ((format-str
           (cdr (assoc .type fj-issue-timeline-action-str-alist)))
          (ts (fedi--relative-time-description
               (date-to-time .updated_at)))
          (user (propertize .user.username
                            'face 'fj-name-face)))
      (insert
       (propertize
        (cond ((equal .type "comment")
               (fj-format-comment item author owner))
              ((equal .type "close")
               (format format-str user ts))
              ((equal .type "reopen")
               (format format-str user ts))
              ((equal .type "change_title")
               (format format-str user
                       (propertize .old_title
                                   'face '(:strike-through t))
                       (propertize .new_title
                                   'face 'fj-name-face)
                       ts))
              ((equal .type "comment_ref")
               (let ((number (number-to-string
                              .ref_issue.number)))
                 (concat
                  (format format-str user ts)
                  "\n"
                  (fj-propertize-link (concat .ref_issue.title " #" number)
                                      'comment-ref number))))
              ((equal .type "commit_ref")
               (concat
                (format format-str user ts)
                "\n"
                (fj-propertize-link (fj-get-html-link-desc .body)
                                    'commit-ref .ref_commit_sha)
                ))
              ((equal .type "issue_ref")
               (format format-str user .repository.full_name ts))
              ((equal .type "label")
               (format format-str user .label.name ts))
              ;; PRs:
              ((equal .type "pull_push")
               (let* ((json-array-type 'list)
                      (json (json-read-from-string .body))
                      (commits (alist-get 'commit_ids json))
                      (force (equal (alist-get 'is_force_push json) "t")))
                 (concat
                  (format format-str user (if force "force pushed" "added")
                          (length commits) ts)
                  ;; FIXME: display commit msg here too:
                  (cl-loop for c in commits
                           concat
                           (concat "\n"
                                   (fj-propertize-link (substring c 0 7)
                                                       'commit-ref c))))))
              ((equal .type "merge_pull")
               ;; FIXME: get commit and branch for merge:
               ;; Commit is the *merge* commit, created by actually merging
               ;; the proposed commits
               ;; branch etc. details should be given at top, diff details to
               ;; plain issue
               (format format-str user ts))
              ((equal .type "pull_ref")
               (concat
                (format format-str user ts)
                "\n"
                (fj-propertize-link .ref_issue.title 'comment-ref .ref_issue.number)))
              ((equal .type "delete_branch")
               (format format-str user
                       (propertize .old_ref
                                   'face 'fj-name-face)
                       ts))
              (t ;; just so we never break the rest of the view:
               (format "%s did unknown action %s" user ts)))
        'fj-item-data item)
       "\n\n"))))

(defun fj-get-html-link-desc (str)
"Return a description string from HTML link STR."
(save-match-data
  (let ((match (string-match "<a[^\n]*>\\(?2:[^\n]*\\)</a>" str)))
    (match-string 2 str))))

(defun fj-propertize-link (str &optional type item)
  "Propertize a link with text STR.
Optionally set link TYPE and ITEM number."
  ;; TODO: poss to refactor with `fedi-link-props'?
  (propertize str
              'face 'shr-link
              'mouse-face 'highlight
              'shr-tabstop t
              'keymap fj-link-keymap
              'button t
              'type type
              'item item
              'fj-tab-stop t
              'category 'shr
              'follow-link t))

;;; SEARCH

(defun fj-repo-search-do (query &optional topic)
  "Search for QUERY, optionally flag it as a TOPIC."
  (let* ((params `(("q" . ,query)
                   ("limit" . "100")
                   ("sort" . "updated")
                   ,(when topic
                      '("topic" . "t")))))
    (fj-get "/repos/search" params)))

(defun fj-repo-search (query &optional topic)
  "Search repos for QUERY.
If TOPIC, QUERY is a search for topic keywords."
  (interactive "sSearch for repo: ")
  (let* ((resp (fj-repo-search-do query topic))
         (data (alist-get 'data resp))
         (cands (fj-get-repo-candidates data))
         (completion-extra-properties
          '(:annotation-function fj-repo-candidates-annot-fun))
         (choice (completing-read "Repo: " cands))
         (user (cl-fourth
                (assoc choice cands #'equal))))
    (fj-list-issues choice user)))

;; doesn't work
(defun fj-repo-candidates-annot-fun (cand)
  "CAND."
  (cl-fourth
   (assoc cand minibuffer-completion-table
          #'equal)))

;;; SEARCH REPOS TL

(defvar fj-repo-tl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fj-repo-tl-map)
    map)
  "Map for `fj-repo-tl-mode', a tabluated list of repos.")

(define-derived-mode fj-repo-tl-mode tabulated-list-mode
  "fj-repo-search"
  "Mode for displaying a tabulated list of repo search results."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("Name" 12 t)
          ("Owner" 12 t)
          ("★" 2 t)
          ("" 2 t)
          ("issues" 5 t)
          ("Lang" 10 t)
          ("Updated" 12 t)
          ("Description" 55 nil)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(define-button-type 'fj-search-repo-button
  'follow-link t
  'action 'fj-repo-tl-list-issues
  'help-echo "RET: View this repo's issues.")

(define-button-type 'fj-search-owner-button
  'follow-link t
  'action 'fj-list-user-repos
  'help-echo "RET: View this user.")

(define-button-type 'fj-issues-owner-button
  'follow-link t
  'action 'fj-list-user-repos
  'help-echo "RET: View this user.")

(defun fj-repo-tl-entries (repos &optional no-owner)
  "Return tabluated list entries for REPOS.
NO-OWNER means don't display owner column (user repos view)."
  (cl-loop
   for r in repos
   collect
   (let-alist r
     (let* ((fork (if (eq .fork :json-false) "ℹ" "⑂"))
            (updated (date-to-time .updated_at))
            (updated-str (format-time-string "%s" updated))
            (updated-display (fedi--relative-time-description updated nil :brief)))
       `(nil ;; TODO: id
         ,(cl-remove 'nil
                     `[(,.name face fj-item-face
                               id ,.id
                               type fj-user-repo-button
                               item repo
                               fj-url ,.html_url)
                       ,(unless no-owner
                          `(,.owner.username face fj-user-face
                                             id ,.id
                                             type fj-search-owner-button
                                             item repo))
                       (,(number-to-string .stars_count)
                        id ,.id face fj-figures-face
                        item repo)
                       (,fork id ,.id face fj-figures-face item repo)
                       (,(number-to-string .open_issues_count)
                        id ,.id face fj-figures-face
                        item repo)
                       ,.language
                       ,(propertize updated-str
                                    'display updated-display
                                    'item 'repo)
                       ,(propertize (string-replace "\n" " " .description)
                                    'face 'fj-comment-face
                                    'item 'repo)]))))))

(defun fj-repo-search-tl (query &optional topic)
  "Search repos for QUERY, and display a tabulated list of results.
TOPIC, a boolean, means search in repo topics."
  (interactive "sSearch for repos: ")
  (let* ((resp (fj-repo-search-do query topic))
         (buf (format "*fj-search-%s*" query))
         (url ;(fedi-http--concat-params-to-url
          (concat fj-host "/explore/repos"))
         (data (alist-get 'data resp))
         (entries (fj-repo-tl-entries data)))
    (with-current-buffer
        (fj-repos-tl-render buf entries #'fj-repo-tl-mode)
      (setq fj-buffer-spec
            `(:url ,url :query ,query)))))

(defun fj-repo-search-tl-topic (query)
  "Search repo topics for QUERY, and display a tabulated list."
  (interactive "sSearch for topic in repos: ")
  (fj-repo-search-tl query 'topic))

(defun fj-repos-tl-render (buf entries mode)
  "Render a tabulated list in BUF fer, with ENTRIES, in MODE.
Optionally specify repo OWNER and URL."
  (with-current-buffer (get-buffer-create buf)
    (setq tabulated-list-entries entries)
    (funcall mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (cond
     ;; FIXME: when called by reload, keep no switch:
     ;; ((string= buf-name prev-buf) ; same repo
     ;;  nil)
     ;; ((string-suffix-p "-issues*" prev-buf) ; diff repo
     ;;  (switch-to-buffer (current-buffer)))
     ((string-prefix-p "*fj-search" buf) ;; any search
      (switch-to-buffer (current-buffer)))
     (t                             ; new buf
      (switch-to-buffer-other-window (current-buffer))))))

;;; TL ACTIONS

;; in repo's issues TL, or for repo entry at point:
(defun fj-create-issue (&optional _)
  "Create issue in current repo or repo at point in tabulated listing."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (owner (fj--repo-owner))
         (repo (fj--repo-name)))
    (fj-issue-compose)
    (setq fj-compose-repo repo
          fj-compose-repo-owner owner)
    (fedi-post--update-status-fields)))

;; in search or user repo TL
(defun fj-repo-tl-list-issues (&optional _)
  "View issues of current repo from tabulated repos listing."
  (interactive)
  (fj-with-repo-entry
   (let* ((entry (tabulated-list-get-entry))
          (name (car (seq-first entry)))
          (owner (fj--repo-owner)))
     (fj-list-issues name owner))))

;; author/owner button, in search or issues TL, not user repo TL
(defun fj-list-user-repos (&optional _)
  "View repos of current entry user from tabulated repos listing."
  (interactive)
  (if (eq major-mode #'fj-user-repo-tl-mode)
      (user-error "Already viewing user repos")
    (fj-with-entry
     (let* ((entry (tabulated-list-get-entry))
            ;; in issues TL, we want ISSUE author, not REPO owner:
            (owner (if (eq major-mode #'fj-issue-tl-mode)
                       (car (seq-elt entry 2))
                     (fj--repo-owner))))
       (fj-user-repos-tl owner)))))

(defun fj-repo-tl-reload ()
  "Reload current user repos tl."
  (interactive)
  (let* ((query (fj--get-buffer-spec :query)) ; only in search tl
         (user (unless query (fj--get-buffer-spec :owner))))
    (if query
        (fj-repo-search-tl query)
      (fj-user-repos-tl user))))

;; search or user repo TL
(defun fj-repo-tl-star-repo (&optional unstar)
  "Star or UNSTAR current repo from tabulated user repos listing."
  (interactive)
  (fj-with-repo-entry
   (let* ((repo (fj--repo-name))
          (owner (fj--repo-owner)))
     (fj-star-repo repo owner unstar))))

(defun fj-repo-tl-unstar-repo ()
  "Unstar current repo from tabulated user repos listing."
  (interactive)
  (fj-repo-tl-star-repo :unstar))

(defun fj-repo-tl-fork ()
  "Fork repo entry at point."
  (interactive)
  (fj-with-entry
   (let* ((repo (fj--repo-name))
          (owner (fj--repo-owner))
          (name (read-string "Fork name: " repo)))
     (fj-fork-repo repo owner name))))

(defun fj-repo-copy-clone-url ()
  "Add the clone_url of repo at point to the kill ring.
Or if viewing a repo's issues, use its clone_url."
  (interactive)
  ;; FIXME: refactor - we don't want `fj-with-entry' in issues tl, as there it
  ;; is anywhere in buffer while in repos tl it is for repo at point.
  (if (equal major-mode #'fj-issue-tl-mode)
      (let* ((repo (fj--get-buffer-spec :repo))
             (owner (fj--get-buffer-spec :owner))
             (resp (fj-get-repo repo owner))
             (url (alist-get 'clone_url resp)))
        (kill-new url)
        (message (format "Copied: %s" url)))
    (fj-with-repo-entry
     (let* ((entry (tabulated-list-get-entry))
            (repo (car (seq-first entry)))
            (owner (fj--repo-owner))
            (resp (fj-get-repo repo owner))
            (url (alist-get 'clone_url resp)))
       (kill-new url)
       (message (format "Copied: %s" url))))))

;; TODO: star toggle

(defun fj-get-repo-files (repo owner &optional ref)
  "Get files for REPO of OWNER.
REF is a commiit, branch or tag."
  (let ((endpoint (format"repos/%s/%s/contents" owner repo)))
    (fj-get endpoint)))

(defun fj-get-repo-file (repo owner file)
  "Return FILE from REPO of OWNER.
FILE is a string, including type suffix, and is case-sensitive."
  (let ((endpoint (format "repos/%s/%s/raw/%s" owner repo file)))
    (fj-get endpoint nil :no-json)))

(defun fj-repo-readme (&optional repo owner ref)
  "Display readme file of REPO by OWNER.
Optionally specify REF, a commit, branch, or tag."
  (let* ((files (fj-get-repo-files repo owner ref))
         (names (cl-loop for f in files
                         collect (alist-get 'name f)))
         (readme-name
          (car (or (cl-member "readme" names :test #'string-prefix-p)
                   (cl-member "README" names :test #'string-prefix-p))))
         (suffix (file-name-extension readme-name))
         (file  (fj-get-repo-file repo owner readme-name))
         (file-str (with-current-buffer file
                     (goto-char (point-min))
                     (re-search-forward "^$" nil 'move)
                     (buffer-substring (point) (point-max))))
         (buf (format "*fj-%s-%s*" repo readme-name)))
    (with-current-buffer (get-buffer-create buf)
      (let ((inhibit-read-only t)) ;; in case already open
        (erase-buffer)
        (save-excursion
          (insert file-str)))
      (if (string= suffix "org")
          (org-mode)
        (markdown-mode))
      (read-only-mode 1)
      ;; TODO: setq a quick q kill-buffer with (local keymap)
      ;; (quit-window)
      ;; (kill-buffer buffer)
      (switch-to-buffer-other-window (current-buffer)))))

(defun fj-repo-tl-readme ()
  "Display readme file of current repo."
  (interactive)
  (let ((repo (fj--repo-name))
        (owner (fj--repo-owner)))
    (fj-repo-readme repo owner)))

;;; TL ACTIONS, ISSUES ONLY

(defun fj-issues-tl-view (&optional _)
  "View current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (let* ((entry (tabulated-list-get-entry))
          (number (car (seq-first entry)))
          (owner (fj--get-buffer-spec :owner)))
     (fj-issue-view fj-current-repo owner number))))

(defun fj-issues-tl-edit ()
  "Edit issue from tabulated issues listing."
  (interactive)
  (fj-with-own-entry
   (let* ((entry (tabulated-list-get-entry))
          (number (car (seq-first entry)))
          (owner (fj--get-buffer-spec :owner))
          (title (car (seq-elt entry 4)))
          (repo (fj--get-buffer-spec :repo))
          (data (fj-get-issue repo owner number))
          (old-body (alist-get 'body data)))
     ;; (fj-issue-edit fj-current-repo owner number))))
     (fj-issue-compose :edit nil 'issue old-body)
     (setq fj-compose-issue-title title
           fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-issue-number number)
     (fedi-post--update-status-fields))))

(defun fj-issues-tl-comment ()
  "Comment on issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (let* ((entry (tabulated-list-get-entry))
          (number (car (seq-first entry)))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--get-buffer-spec :repo))
          (title (car (seq-elt entry 4))))
     ;; (comment (read-string
     ;; (format "Comment on issue #%s: " number))))
     ;; (fj-issue-comment fj-current-repo owner number comment))))
     ;; TODO: display repo in status fields, but not editable?
     (fj-issue-compose nil #'fj-compose-comment-mode 'comment)
     (setq fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-issue-title title
           fj-compose-issue-number number)
     (fedi-post--update-status-fields))))

(defun fj-issues-tl-close (&optional _)
  "Close current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   ;; TODO make check work for "all": need to prop each tl entry
   (fj-with-own-issue-or-repo
    (if (string= (fj--property 'state) "closed")
        (user-error "Issue already closed")
      (let* ((entry (tabulated-list-get-entry))
             (number (car (seq-first entry)))
             (owner (fj--get-buffer-spec :owner)))
        (fj-issue-close fj-current-repo owner number)
        (fj-issues-tl-reload))))))

(defun fj-issues-tl-delete (&optional _)
  "Delete current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (fj-with-own-repo
    (let* ((entry (tabulated-list-get-entry))
           (number (car (seq-first entry)))
           (owner (fj--get-buffer-spec :owner)))
      (when (y-or-n-p (format "Delete issue %s?" number))
        (fj-issue-delete fj-current-repo owner number :no-confirm)
        (fj-issues-tl-reload))))))

(defun fj-issues-tl-reopen (&optional _)
  "Reopen current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (if (string= (fj--property 'state) "open")
       (user-error "Issue already open")
     ;; (if (string= (fj--get-buffer-spec :state) "open")
     ;; (user-error "Viewing open issues?")
     (let* ((entry (tabulated-list-get-entry))
            (number (car (seq-first entry)))
            (owner (fj--get-buffer-spec :owner)))
       (fj-issue-close fj-current-repo owner number "open")
       (fj-issues-tl-reload)))))

(defun fj-issues-tl-edit-title ()
  "Edit issue title from issues tabulatd list view."
  (interactive)
  (fj-with-own-issue
   (let* ((entry (tabulated-list-get-entry))
          (repo (fj--get-buffer-spec :repo))
          (owner (fj--get-buffer-spec :owner))
          (number (car (seq-first entry))))
     (fj-issue-edit-title repo owner number)
     (fj-issues-tl-reload))))

;;; COMPOSING

(defvar fj-compose-last-buffer nil)

(defvar-local fj-compose-repo nil)

(defvar-local fj-compose-repo-owner nil)

(defvar-local fj-compose-issue-title nil)

(defvar-local fj-compose-item-type nil)

(defvar-local fj-compose-spec nil)

(defvar-local fj-compose-issue-number nil)

(defalias 'fj-compose-cancel #'fedi-post-cancel)

(defvar fj-compose-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'fj-compose-cancel)
    (define-key map (kbd "C-c C-c") #'fj-compose-send)
    map)
  "Keymap for `fj-compose-comment-mode'.")

(define-minor-mode fj-compose-comment-mode
  "Minor mode for composing comments."
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
  ;; FIXME: combine own repos and completing search:
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
  (setq fj-compose-last-buffer (buffer-name (current-buffer)))
  (fedi-post--compose-buffer
   edit
   #'markdown-mode
   (or mode #'fj-compose-mode)
   (when mode "fj-compose")
   (or type 'issue)
   (list #'fj-compose-mentions-capf
         #'fj-compose-issues-capf)
   ;; TODO: why not have a compose-buffer-spec rather than 10 separate vars?
   `(((name . "repo")
      (prop . compose-repo)
      (item-var . fj-compose-repo)
      (face . link))
     ((name . ,(if (eq type 'comment) "issue ""title"))
      (prop . compose-title)
      (item-var . fj-compose-issue-title)
      (face . lem-post-title-face)))
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
             (repo fj-compose-repo)
             (response
              (cond ((eq type 'new-comment)
                     (fj-issue-comment repo
                                       fj-compose-repo-owner
                                       fj-compose-issue-number
                                       body))
                    ((eq type 'edit-comment)
                     (fj-issue-comment-edit repo
                                            fj-compose-repo-owner
                                            fj-compose-comment-id
                                            body))
                    ((eq type 'edit-issue)
                     (fj-issue-patch repo
                                     fj-compose-repo-owner
                                     fj-compose-issue-number
                                     fj-compose-issue-title
                                     body))
                    (t ; new issue
                     (fj-issue-post repo
                                    fj-compose-repo-owner
                                    fj-compose-issue-title body)))))
        (when response
          (with-current-buffer buf
            (fedi-post-kill))
          (if (not (eq type 'new-issue))
              ;; FIXME: we may have been in issues TL or issue view.
              ;; we we need prev-buffer arg?
              ;; else generic reload function
              (fj-issue-view-reload)
            (fj-list-issues repo)))))))

(defun fj-search-users (query &optional limit)
  "Search instance users for QUERY.
Optionally set LIMIT to results."
  ;; FIXME: server: limit is an integer; it doesn't respect our 25, returns 2500
  (let ((params `(("q" . ,query)
                  ("limit" . ,limit))))
    (fj-get "users/search" params)))

(defun fj-users-alist (data)
  "Return an alist of users' DATA, containing handle and id."
  ;; users have no html_url, just concat it to `fj-host'
  (cl-loop for u in data
           for id = (alist-get 'id u)
           for name = (alist-get 'login u)
           collect (concat "@" name))) ;; @ needed to match for display!

(defun fj-compose-handle-exit-fun (str _status)
  "Turn completion STR into a markdown link."
  (save-excursion
    (delete-backward-char (length str))
    (insert
     ;; FIXME: doesn't work with markdown-mode!
     (propertize str
                 'face 'fj-user-face)))
  (forward-word))

(defun fj-compose-mentions-fun (start end)
  "Given prefix str between START and END, return an alist of mentions for capf."
  (let* ((resp (fj-search-users
                (buffer-substring-no-properties (1+ start) ; cull '@'
                                                end)
                "25")) ; limit
         (data (alist-get 'data resp)))
    (fj-users-alist data)))

(defun fj-compose-mentions-capf ()
  "Build a mentions completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf fedi-post-handle-regex
                          #'fj-compose-mentions-fun
                          nil nil
                          #'fj-compose-handle-exit-fun))

;;; issues capf
;; TODO: we need to trigger completion on typing # alone (depends on regex)
;; TODO: we need to fetch all issues or do GET query by number
(defun fj-issues-alist (data)
  "Return an alist from issue DATA, a cons of number and title."
  (cl-loop for i in data
           collect (cons (concat "#"
                                 (number-to-string
                                  (alist-get 'number i)))
                         (alist-get 'title i))))

(defun fj-compose-issues-fun (start end)
  "Given prefix str between START and END, return an alist of issues for capf."
  (let ((resp (fj-repo-get-issues fj-compose-repo fj-compose-repo-owner
                                  "all")))
    (fj-issues-alist resp)))

(defun fj-compose-issues-capf ()
  "Build an issues completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf fedi-post-tag-regex
                          #'fj-compose-issues-fun
                          #'fj--issues-annot-fun nil
                          #'fj-compose-issue-exit-fun))

(defun fj--issues-annot-fun (candidate)
  "Given an issues completion CANDIDATE, return its annotation."
  (concat " " (cdr (assoc candidate fedi-post-completions #'equal))))

(defun fj-compose-issue-exit-fun (str _status)
  "Mark completion STR as verbatim."
  (save-excursion
    (backward-char (length str))
    (insert "`"))
  (insert "`"))

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

(defun fj-view-notifications ()
  "View notifications for `fj-user'."
  (interactive)
  (let ((buf "*fj-notifications*") ;"*fj-notifications-%s" read-flag)))
        (data (fj-get-notifications "t")))
    (fedi-with-buffer buf 'fj-issue-view-mode nil
      (fj-render-notifications data))))

(defun fj-render-notifications (data)
  "Render notifications DATA."
  (cl-loop for n in data
           do (fj-render-notification n)))

(defun fj-render-notification (notif)
  "Render NOTIF."
  (let-alist notif
    ;; notifs don't have item #, so we get from URL:
    (let ((number (car (last (split-string .subject.url "/")))))
      (insert
       (propertize (concat "#" number)
                   'face 'fj-comment-face)
       (concat " "
               (propertize
                (fj-propertize-link .subject.title 'notif number)
                'fj-repo .repository.name
                'fj-owner .repository.owner.login
                'fj-url .subject.html_url
                'fj-byline t) ; for nav
               "\n"
               .repository.owner.login "/" .repository.name
               "\n"
               fedi-horiz-bar fedi-horiz-bar
               "\n\n")))))

;;; BROWSE

(defun fj-tl-browse-entry ()
  "Browse URL of tabulated list entry at point."
  (interactive)
  (fj-with-entry
   (let ((url (fj--property 'fj-url)))
     (browse-url-generic url))))

(defun fj-browse-view ()
  "Brose URL of view at point."
  (interactive)
  (let ((url (fj--get-buffer-spec :url)))
    (browse-url-generic url)))

(defun fj-tl-imenu-index-fun ()
  "Function for `imenu-create-index-function'.
Allow quick jumping to an element in a tabulated list view."
  (let* (alist)
    (save-excursion
      (goto-char (point-min))
      (while (tabulated-list-get-entry)
        (let* ((entry (tabulated-list-get-entry))
               (name (if (eq major-mode #'fj-issue-tl-mode)
                         (car (seq-elt entry 4))
                       (car (seq-first entry)))))
          (push `(,name . ,(point)) alist))
        (next-line)))
    alist))

;;; ITEMS: RENDERING HANDLES, etc.

(defun fj-do-link-action (pos)
  "Do the action of the link at POS.
Used for hitting RET on a given link."
  (interactive "d")
  (let ((type (get-text-property pos 'type))
        (owner (fj--get-buffer-spec :owner))
        (repo (fj--get-buffer-spec :repo))
        (item (fj--property 'item)))
    (cond ((or (eq type 'tag)
               (eq type 'comment-ref))
           (fj-issue-view repo owner item))
          ((eq type 'handle)
           (fj-user-repos-tl item))
          ((or (eq type 'commit)
               (eq type 'commit-ref))
           (fj-view-commit repo owner item))
          ((eq type 'notif)
           (let ((repo (fj--property 'fj-repo))
                 (owner (fj--property 'fj-owner)))
             (fj-issue-view repo owner item)))
          (t
           (error "Unknown link type %s" type)))))

(defun fj-do-link-action-mouse (event)
  "Do the action of the link at point.
Used for a mouse-click EVENT on a link."
  (interactive "e")
  (fj-do-link-action (posn-point (event-end event))))

(defvar fj-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'fj-do-link-action)
    (define-key map [mouse-2] #'fj-do-link-action-mouse)
    (define-key map [follow-link] 'mouse-face)
    map)
  "The keymap for link-like things in buffer (except for shr.el links).
This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc.")

(defun fj-next-tab-item ()
  "Jump to next tab item."
  (interactive)
  (fedi-next-tab-item nil 'fj-tab-stop))

(defun fj-prev-tab-item ()
  "Jump to prev tab item."
  (interactive)
  (fedi-next-tab-item :prev 'fj-tab-stop))

;;; COMMITS

(defun fj-get-commit (repo owner sha)
  "Get a commit with SHA in REPO by OWNER."
  (let ((endpoint
         (format "repos/%s/%s/git/commits/%s" owner repo sha)))
    (fj-get endpoint)))

(defun fj-view-commit (repo owner sha)
  "View commit with SHA in REPO by OWNER.
Currently we just `browse-url' it."
  (interactive)
  (let* ((resp (fj-get-commit repo owner sha))
         (url (alist-get 'html_url resp)))
    ;; TODO: view commit
    (browse-url-generic url)))

(provide 'fj)
;;; fj.el ends here
