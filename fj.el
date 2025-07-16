;;; fj.el --- Client for forgejo instances -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <mousebot@disroot.org>
;; Copyright (C) 2023 Marty Hiatt <mousebot@disroot.org>
;;
;; Package-Requires: ((emacs "29.1") (fedi "0.2") (tp "0.5") (transient) (magit))
;; Keywords: git, convenience
;; URL: https://codeberg.org/martianh/fj.el
;; Version: 0.16
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

;; To use these functions, first set `fj-token' and `fj-user'.
;; Then either set your Forgejo access token in your auth-source file or
;; set `fj-host' to it.

;;; Code:

(require 'fedi)
(require 'fedi-post)
(require 'fedi-auth)

(require 'magit-git)
(require 'magit-process)
(require 'magit-diff)
(require 'magit-fetch)

(require 'markdown-mode)
(require 'shr)
(require 'mm-url)

(require 'fj-transient)

;;; VARIABLES
;; ours

(defvar fj-token nil)

(defvar fj-user nil)

(defvar fj-host "https://codeberg.org")

(defvar fj-extra-repos nil
  ;; list of "owner/repo"
  ;; TODO: (owner . repo)
  )

(defvar-local fj-current-repo nil)

(defvar-local fj-buffer-spec nil
  "A plist holding some basic info about the current buffer.
Repo, owner, item number, url.")

(defun fj-api (endpoint)
  "Return a URL for ENDPOINT."
  (fedi-http--api endpoint fj-host "v1"))

(defvar-keymap fj-link-keymap
  :doc "The keymap for link-like things in buffer (except for shr.el links).
This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc."
  "<return>" #'fj-do-link-action
  "<mouse-2>" #'fj-do-link-action-mouse
  ;; "<remap> <follow-link>" #'mouse-face ???
  )

;; composing vars

(defvar fj-compose-last-buffer nil)

(defvar-local fj-compose-repo nil)

(defvar-local fj-compose-repo-owner nil)

(defvar-local fj-compose-issue-title nil)

(defvar-local fj-compose-item-type nil)

(defvar-local fj-compose-spec nil)

(defvar-local fj-compose-issue-number nil)

(defvar-local fj-compose-issue-labels nil)

(defvar-local fj-compose-milestone nil)

;; instance vars

(defvar fj-commit-status-types
  '("pending" "success" "error" "failure"))

(defvar fj-merge-types
  '("merge" "rebase" "rebase-merge" "squash"
    "fast-forward-only" "manually-merged"))

(defvar fj-notifications-status-types
  '("unread" "read" "pinned")
  "List of possible status types for getting notifications.")

(defvar fj-notifications-subject-types
  '(nil "issue" "pull" "commit" "repository")
  "List of possible subject types for getting notifications.")

(defvar fj-own-repos-order
  '("name" "id" "newest" "oldest" "recentupdate" "leastupdate"
    "reversealphabetically" "alphabetically" "reversesize" "size"
    "reversegitsize" "gitsize" "reverselfssize" "lfssize" "moststars"
    "feweststars" "mostforks" "fewestforks"))

(defvar fj-issues-sort
  '("relevance" "latest" "oldest" "recentupdate" "leastupdate"
    "mostcomment" "leastcomment" "nearduedate" "farduedate")
  "A list of sort options for listing repo issues.")

;;; CUSTOMIZES

(defgroup fj nil
  "Fj.el, a Forgejo client."
  :group 'external
  :prefix "fj-")

(defcustom fj-token-use-auth-source t
  "Whether to use an auth-source file.
If non-nil, use an auth-source file such as ~/,authinfo.gpg for the user
authorization token of the foregejo instance.
If set to nil, you need to set `fj-token' to your user token."
  :type 'boolean)

(defcustom fj-use-emojify t
  "Whether to enable `emojify-mode' in item views.
This will not install emojify for you, you have to do that yourself."
  :type 'boolean)

(defcustom fj-own-repos-default-order "recentupdate"
  "The default order parameter for `fj-list-own-repos'.
The value must be a member of `fj-own-repos-order'."
  :type (append '(choice)
                (mapcar (lambda (x)
                          `(const ,x))
                        fj-own-repos-order)))

(defcustom fj-issues-sort-default "recentUpdate"
  "Default sort parameter for repo issues listing."
  :type (append '(choice)
                (mapcar (lambda (x)
                          `(const ,x))
                        fj-issues-sort)))

(defcustom fj-timeline-default-items 15
  "The default number of timeline items to load.
Used for issues and pulls.
If set to nil, `fj-default-limit',the general default amount, will be used.
Fj.el currently struggles with performances in timelines, and it seems
like the actual requests might be the culprit (after we culled all
contenders on our end), so if you find that frustrating, ensure this is
set to a pretty low number (10-15)."
  :type 'integer)

(defun fj-timeline-default-items ()
  "Return a value for default timeline items to load.
If `fj-timeline-default-items' is not set, call `fj-default-limit'."
  (if (not fj-timeline-default-items)
      (fj-default-limit)
    (number-to-string fj-timeline-default-items)))

;;; FACES

(defface fj-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for secondary info.")

(defface fj-closed-issue-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the title of a closed issue.")

(defface fj-closed-issue-notif-face
  '((t :inherit fj-closed-issue-face :underline t))
  "Face for the title of a closed issue in notifications view.")

(defface fj-closed-issue-notif-verbatim-face
  `((t :inherit (highlight font-lock-comment-face)))
  "Face for the title of a closed issue in notifications view.")

(defface fj-user-face
  '((t :inherit font-lock-function-name-face))
  "User face.")

(defface fj-figures-face
  '((t :inherit font-lock-doc-face))
  "Face for figures (stars count, comments count, etc.).")

(defface fj-item-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for item names.")

(defface fj-item-verbatim-face
  `((t :inherit (highlight font-lock-type-face)))
  "Face for item names.")

(defface fj-item-closed-verbatim-face
  `((t :inherit (highlight font-lock-comment-face)))
  "Face for item names.")

(defface fj-item-author-face
  `((t :inherit magit-diff-hunk-heading
       :underline t
       :foreground
       ,(face-attribute 'font-lock-function-name-face :foreground)
       :weight bold))
  "Face for item authors.")

(defface fj-item-byline-face
  `((t :inherit magit-diff-hunk-heading))
  "Face for item author bylines.")

(defface fj-issue-commit-face
  '((t :inherit (link ;font-lock-comment-face
                 highlight)))
  "Face for issue commit references.")

(defface fj-name-face
  '((t :weight bold))
  "Face for timeline item names (user, issue, PR).
Not used for items that are links.")

(defface fj-name-verbatim-face
  `((t :inherit highlight :weight bold))
  "Face for timeline item names (user, issue, PR).
Not used for items that are links.")

(defface fj-simple-link-face
  '((t :underline t))
  "Face for links in v simple displays.")

(defface fj-label-face
  '((t :inherit secondary-selection :slant italic))
  "Face for issue labels.")

(defface fj-post-title-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for post title in compose buffer.")

;;; AUTH / TOKENS

(defun fj-create-token ()
  "Create an access token for `fj-user' on `fj-host'.
Reads a token name and reads a user password for BasicAuth.
Copies the token to the kill ring and returns it."
  (interactive)
  (let* ((name (read-string "Token name: "))
         (params `(("name" . ,name)
                   ("scopes" . ("all"))))
         (endpoint (format "users/%s/tokens" fj-user))
         (password (read-passwd (format "%s password: " fj-host)))
         (url (fj-api endpoint))
         (resp (fedi-http--basic-auth-request
                #'fedi-http--post url fj-user password params nil :json)))
    (fedi-http--triage
     resp
     (lambda (resp)
       (let-alist (fj-resp-json resp)
         (kill-new .sha1)
         (message "Token %s copied to kill ring." .name)
         .sha1)))))

(defun fj-auth-source-get ()
  "Fetch an auth source token.
Optionally prompt for a token and save it if needed."
  (let ((host (url-host (url-generic-parse-url fj-host))))
    (nth 1
         (fedi-auth-source-get fj-user host :create))))

(defun fj-token ()
  "Fetch user access token from auth source, or try to add one.
If `fj-token-use-auth-source' is nil, use `fj-token' instead."
  (interactive)
  (if (not fj-token-use-auth-source)
      fj-token
    (or (fj-auth-source-get)
        (user-error
         "No token. Call `fj-create-token' and try again,\
 set token in auth-source file, or set `fj-token'"))))

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
                        (concat "token "
                                (encode-coding-string
                                 (fj-token) 'utf-8)))))))
     ,body))

(defun fj-get (endpoint &optional params no-json silent)
  "Make a GET request to ENDPOINT.
PARAMS is any parameters to send with the request.
NO-JSON means return the raw response."
  (let* ((url (fj-api endpoint))
         (resp (fj-authorized-request "GET"
                 (if no-json
                     (fedi-http--get url params silent)
                   (fedi-http--get-json url params silent)))))
    (if no-json
        ;; return response buffer, not resulting string. the idea is to then
        ;; call --triage on the result, in case we don't get a 200 response.
        ;; (fj-resp-str resp)
        resp
      (if (or (eq (caar resp) 'errors)
              (eq (caar resp) 'message))
          (user-error "I am Error: %s Endpoint: %s"
                      (alist-get 'message resp)
                      endpoint)
        resp))))

(defun fj-resp-str (resp)
  "Return the response string from RESP, an HTTP response buffer."
  (with-current-buffer resp
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (buffer-substring (point) (point-max))))

(defun fj-resp-json (resp)
  "Parse JSON from RESP, a buffer."
  (let ((json-array-type 'list))
    (with-current-buffer resp
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (decode-coding-region (point) (point-max) 'utf-8)
      (json-parse-buffer :object-type 'alist))))

(defun fj-post (endpoint &optional params json silent)
  "Make a POST request to ENDPOINT.
PARAMS.
If JSON, encode request data as JSON. else encode like query params."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "POST"
      (fedi-http--post url params nil json silent))))

(defun fj-put (endpoint &optional params json)
  "Make a PUT request to ENDPOINT.
PARAMS.
JSON."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "PUT"
      (fedi-http--put url params nil json))))

(defun fj-patch (endpoint &optional params json)
  "Make a PATCH request to ENDPOINT.
PARAMS are query params unless JSON."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "PATCH"
      (fedi-http--patch url params json))))

(defun fj-delete (endpoint &optional params json)
  "Make a DELETE request to ENDPOINT.
PARAMS and JSON are for `fedi-http--delete'."
  (let ((url (fj-api endpoint)))
    (fj-authorized-request "DELETE"
      (fedi-http--delete url params json))))

;;; INSTANCE SETTINGS
;; https://forgejo.org/docs/latest/user/api-usage/#pagination
;; the description is confusing, saying that max_response_items and
;; default_paging_num are for the max and default values of the page
;; parameter, but surely the max and default would be for the limit parameter?

(defun fj-get-api-settings ()
  "Return API settings from the current instance."
  (let ((endpoint "/settings/api"))
    (fj-get endpoint)))

(defvar fj-default-limit nil)

(defun fj-default-limit ()
  "Return the value of `fj-default-limit' or fetch from instance."
  (or fj-default-limit
      (let ((settings (fj-get-api-settings)))
        (setq fj-default-limit
              (number-to-string
               (alist-get 'default_paging_num settings))))))

(defvar fj-max-items nil)

(defun fj-max-items ()
  "Return the max response items setting from the current instance."
  (or fj-max-items
      (let ((settings (fj-get-api-settings)))
        (setq fj-max-items (number-to-string
                            (alist-get 'max_response_items settings))))))

(defun fj-get-swagger-json ()
  "Return the full swagger JSON from the current instance."
  (let ((url (format "%s/swagger.v1.json" fj-host)))
    (fedi-http--get-json url)))

;;; UTILS

(defun fj--property (prop)
  "Get text property PROP at point, else return nil."
  (get-text-property (point) prop))

(defun fj--get-buffer-spec (key)
  "Get entry for KEY from `fj-buffer-spec', else return nil."
  (plist-get fj-buffer-spec key))

(defun fj--own-repo-p ()
  "T if repo at point, or in current view, is owned by `fj-user'."
  (or (eq major-mode 'fj-user-repo-tl-mode) ;; own repos listing
      (eq major-mode 'fj-owned-issues-tl-mode) ;; own issues listing
      (and (eq major-mode 'fj-repo-tl-mode)
           (equal fj-user (fj--get-tl-col 1)))
      (and (or (eq major-mode 'fj-item-view-mode)
               (eq major-mode 'fj-issue-tl-mode))
           (equal fj-user (fj--get-buffer-spec :owner)))))

(defun fj--issue-own-p ()
  "T if issue is authored by `fj-user'.
Works in issue view mode or in issues tl."
  (pcase major-mode
    ('fj-item-view-mode
     (equal fj-user
            (fj--get-buffer-spec :author)))
    ('fj-issue-tl-mode
     (let ((author (fj--get-tl-col 2)))
       (equal fj-user author)))
    ('fj-owned-issues-tl-mode
     (let ((author (fj--get-tl-col 3)))
       (equal fj-user author)))))

(defun fj--comment-own-p ()
  "T if comment is authored by `fj-user'."
  (and (eq major-mode 'fj-item-view-mode)
       (equal fj-user (fj--property 'fj-comment-author))))

(defun fj-kill-all-buffers ()
  "Kill all fj buffers."
  (interactive)
  (fedi-kill-all-buffers "*fj-"))

(defun fj-switch-to-buffer ()
  "Switch to a live fj buffer."
  (interactive)
  (fedi-switch-to-buffer "*fj-"))

(defun fj--issue-right-align-str (str)
  "Right align STR and return it."
  (concat
   (propertize
    " "
    'display
    `(space :align-to (- right ,(+ (length str) 4))))
   str))

(defun fj--repo-owner ()
  "Return repo owner, whatever view we are in.
If we fail, return `fj-user'." ;; poss insane
  (if (eq major-mode #'fj-repo-tl-mode)
      (fj--get-tl-col 1)
    (or (fj--get-buffer-spec :owner)
        fj-user))) ;; FIXME: fallback hack

(defun fj--repo-name ()
  "Return repo name, whatever view we are in."
  (or (fj--get-buffer-spec :repo)
      fj-current-repo
      (if (equal major-mode 'fj-owned-issues-tl-mode)
          ;; own repos mode:
          (fj--get-tl-col 2)
        ;; repos mode (repo tl search):
        (fj--get-tl-col 0))
      (fj-current-dir-repo)))

(defun fj--map-alist-key (list key)
  "Return the values of KEY in LIST, a list of alists."
  (let ((test-fun (when (stringp key) #'equal)))
    (mapcar (lambda (x)
              (alist-get key x nil nil test-fun))
            list)))

(defun fj--map-alist-to-cons (list k1 k2)
  "Return an alist of values of K1 and K2 from LIST."
  (let ((test-fun (when (stringp k1) #'equal)))
    (mapcar (lambda (x)
              (cons (alist-get k1 x nil nil test-fun)
                    (alist-get k2 x nil nil test-fun)))
            list)))

;;; GIT CONFIG

(defun fj-git-config-remote-url ()
  "Return a remote's URL.
Either get push default or call `magit-read-remote'."
  ;; used in `fj-list-issues' and `fj-issue-compose'
  ;; FIXME: use everywhere?
  (let* ((remote ;; maybe works for own repo, as you gotta push:
          (or (magit-get-push-remote)
              ;; nice for not own repo:
              (magit-read-remote "Remote:" nil :use-only))))
    (magit-get (format "remote.%s.url" remote))))

;;; TL ENTRIES

(defun fj--get-tl-col (num)
  "Return column number NUM from current tl entry."
  (let ((entry (tabulated-list-get-entry)))
    (car (seq-elt entry num))))

(defun fj--repo-col-or-buf-spec (&optional current-repo)
  "Try to return a repo name.
If `fj-owned-issues-tl-mode', return column 3 of entry at point.
Else get repo from `fj-buffer-spec'.
If CURRENT-REPO, get from `fj-current-repo' instead."
  (if (eq major-mode 'fj-owned-issues-tl-mode)
      (fj--get-tl-col 2)
    (if current-repo
        fj-current-repo
      (fj--get-buffer-spec :repo))))

;;; MACROS

(defmacro fj-with-issue (&rest body)
  "Execute BODY if we are in an issue view or if issue at point."
  (declare (debug t))
  `(if (not (or (fj--get-buffer-spec :item)
                (eq 'issue (fj--property 'item))))
       (user-error "Not issue here?")
     ,@body))

(defmacro fj-with-item-view (&rest body)
  "Execute BODY if we are in an item view."
  (declare (debug t))
  `(if (not (fj--get-buffer-spec :item))
       (user-error "Not in an issue view?")
     ,@body))

(defmacro fj-with-own-repo (&rest body)
  "Execute BODY if a repo owned by `fj-user'."
  (declare (debug t))
  `(if (not (fj--own-repo-p))
       (user-error "Not in a repo you own")
     ,@body))

(defmacro fj-with-own-issue (&rest body)
  "Execute BODY if issue is authored by `fj-user'."
  (declare (debug t))
  `(fj-with-issue
    (if (not (fj--issue-own-p))
        (user-error "Not an issue you own")
      ,@body)))

(defmacro fj-with-own-issue-or-repo (&rest body)
  "Execute BODY if issue authored or repo owned by `fj-user'."
  (declare (debug t))
  `(if (not (or (fj--issue-own-p)
                (fj--own-repo-p)))
       (user-error "Not an issue or repo you own")
     ,@body))

(defmacro fj-with-own-comment (&rest body)
  "Execute BODY if comment at point is authored by `fj-user'."
  (declare (debug t))
  `(fj-with-issue
    (if (not (fj--comment-own-p))
        (user-error "No comment of yours at point")
      ,@body)))

(defmacro fj-with-entry (&rest body)
  "Execute BODY if we have a tabulated list entry at point."
  (declare (debug t))
  `(if (not (tabulated-list-get-entry))
       (user-error "No entry at point")
     ,@body))

(defmacro fj-with-own-entry (&rest body)
  "Execute BODY if the tabulated list entry at point is owned by `fj-user'."
  (declare (debug t))
  `(fj-with-entry
    (if (not (fj--issue-own-p))
        (user-error "No an entry you own")
      ,@body)))

(defmacro fj-with-repo-entry (&rest body)
  "Execute BODY if we have a repo tabulated list entry at point."
  (declare (debug t))
  `(if (or (not (tabulated-list-get-entry))
           (eq major-mode #'fj-issue-tl-mode))
       (user-error "No repo entry at point")
     ,@body))

(defmacro fj-with-pull (&rest body)
  "Execute BODY if we are in an PR view or if pull request at point."
  (declare (debug t))
  `(if (not (or (eq (fj--get-buffer-spec :type) :pull)
                (eq 'pull (fj--property 'item))))
       (user-error "No PR here?")
     ,@body))

(defmacro fj-destructure-buf-spec (parameters &rest body)
  "Destructure `fj-buffer-spec' with keyword PARAMETERS and call BODY."
  (declare (debug t)
           (indent 1))
  `(cl-destructuring-bind (&key ,@parameters &allow-other-keys)
       fj-buffer-spec
     ,@body))

;;; MAP

;; FIXME: we need 1 derived from tl, one from special?
(defvar-keymap fj-generic-map
  :doc "Generic keymap."
  :parent special-mode-map
  ;; should actually be universal:
  "<tab>" #'fj-next-tab-item
  "<backtab>" #'fj-prev-tab-item
  "C-M-q" #'fj-kill-all-buffers
  "/" #'fj-switch-to-buffer
  ;; really oughta be universal:
  "R" #'fj-repo-update-settings
  "I" #'fj-list-issues
  "P" #'fj-list-pulls
  "O" #'fj-list-own-repos
  "W" #'fj-list-own-issues
  "N" #'fj-view-notifications
  "S" #'fj-repo-search-tl
  "U" #'fj-user-update-settings
  "b" #'fj-browse-view
  "C" #'fj-copy-item-url
  "n" #'fj-item-next
  "p" #'fj-item-prev
  "g" #'fj-view-reload)

(defvar-keymap fj-generic-tl-map
  :doc "Generic timeline keymap."
  :parent tabulated-list-mode-map
  ;; should actually be universal:
  "<tab>" #'fj-next-tab-item
  "<backtab>" #'fj-prev-tab-item
  "." #'fj-next-page
  "," #'fj-prev-page
  "C-M-q" #'fj-kill-all-buffers
  "/" #'fj-switch-to-buffer
  ;; really oughta be universal:
  "R" #'fj-repo-update-settings
  "I" #'fj-list-issues
  "P" #'fj-list-pulls
  "O" #'fj-list-own-repos
  "W" #'fj-list-own-issues
  "N" #'fj-view-notifications
  "S" #'fj-repo-search-tl
  "U" #'fj-user-update-settings
  "C" #'fj-copy-item-url
  "b" #'fj-browse-view
  "g" #'fj-view-reload)

;;; NAV

(defun fj-item-next ()
  "Go to next item or notification.
Should work for anything with an fj-byline property."
  (interactive)
  (fedi--goto-pos #'next-single-property-change 'fj-byline
                  #'fj-item-view-more*))

(defun fj-item-prev ()
  "Goto previous item or notification.
Should work for anything with an fj-byline property."
  (interactive)
  (fedi--goto-pos #'previous-single-property-change 'fj-byline))

;;; PAGINATION
;; it seems like the LIMIT param only works when PAGE is set.
;; not sure if we should therefore always set a default PAGE value ("1")?

(defun fj--inc-str (str &optional dec)
  "Incrememt STR, and return a string.
When DEC, decrement string instead."
  (let ((num (string-to-number str)))
    (number-to-string
     (if dec (1- num) (1+ num)))))

(defun fj-inc-or-2 (&optional page)
  "Incrememt PAGE.
If nil, return \"2\"."
  (if page (fj--inc-str page) "2"))

(defun fj-dec-or-nil (&optional page)
  "Decrement PAGE.
If nil, return nil."
  (when page (fj--inc-str page :dec)))

(defun fj-dec-plist-page (plist)
  "Decrement the :page entry in PLIST and return it."
  (let* ((new-page (fj-dec-or-nil
                    (plist-get plist :page))))
    (plist-put plist :page new-page)))

(defun fj-inc-plist-page (plist)
  "Increment the :page entry in PLIST and return it."
  (let ((new-page (fj-inc-or-2
                   (plist-get plist :page))))
    (plist-put plist :page new-page)))

(defun fj-plist-values (plist)
  "Return the values of PLIST as a list."
  ;; prob a better way to implement this!:
  (cl-loop for x in plist by 'cddr
           collect (plist-get plist x)))

(defun fj-next-page ()
  "Load the next page of the current view."
  ;; NB: for this to work, :viewargs in `fj-buffer-spec' must be a plist
  ;; whose values match the signature of :viewfun. the value of :page in
  ;; :viewargs is incremented.
  (interactive)
  (message "Loading next page...")
  (fj-destructure-buf-spec (viewfun viewargs)
    ;; incremement page:
    (let ((args (fj-inc-plist-page viewargs)))
      (apply viewfun (fj-plist-values args))))
  (message "Loading next page... Done."))

(defmacro fj-prev-page-maybe (page &rest body)
  "Call BODY if PAGE is neither nil nor 1."
  (declare (debug t) (indent 1))
  `(if (or (not ,page) ;; never paginated
           (= 1 (string-to-number ,page))) ;; after paginating
       (user-error "No previous page")
     ,@body))

(defun fj-prev-page ()
  "Load the previous page."
  (interactive)
  (message "Loading previous page...")
  (fj-destructure-buf-spec (viewfun viewargs)
    (fj-prev-page-maybe (plist-get viewargs :page)
      ;; decrement page:
      (let ((args (fj-dec-plist-page viewargs)))
        (apply viewfun (fj-plist-values args)))))
  (message "Loading previous page... Done."))

;;; REPOS TL UTILS

(defun fj--tl-get-elt (number entry)
  "Return element from column NUMBER from tabulated list ENTRY."
  ;; this is run by `fj-tl-sort-pred' below, which is called on whole entries
  ;; (nil [(blah)]) and not just the vector, so we have to cadr the entry.
  ;; for other uses you prob want `fj--get-tl-col'
  (car
   (seq-elt
    (cadr entry)
    number)))

(defun fj-tl-sort-pred (x y col &optional col-repo col-user)
  "Predicate function for sorting numeric tl columns.
X Y are tl entries to sort. COL, COL-REPO and COL-USER are numbers,
corresponding to the (zero-indexed) tl column to search by. The
first is for `fj-repo-tl-mode', the second for
`fj-user-repo-tl-mode'."
  ;; FIXME can we handle the col- args better than this?
  (let* ((col (or col
                  (if (eq major-mode 'fj-repo-tl-mode) col-repo col-user)))
         (a (fj--tl-get-elt col x))
         (b (fj--tl-get-elt col y)))
    (> (string-to-number a)
       (string-to-number b))))

(defun fj-tl-sort-by-stars (x y)
  "Predicate function for sorting repos by stars.
X Y are tl entries to sort."
  (fj-tl-sort-pred x y nil 2 1))

(defun fj-tl-sort-by-issue-count (x y)
  "Predicate function for sorting repos by issues count.
X Y are tl entries to sort."
  (fj-tl-sort-pred x y nil 4 3))

(defun fj-tl-sort-by-issues (x y)
  "Predicate function to sort issues by issue number.
X and Y are sorting args."
  (fj-tl-sort-pred x y 0))

(defun fj-tl-sort-by-comment-count (x y)
  "Predicate function to sort issues by number of comments.
X and Y are sorting args."
  (fj-tl-sort-pred x y 1))

;;; USER REPOS TL

(defun fj-get-repo (repo owner)
  "GET REPO owner by OWNER."
  (let* ((endpoint (format "repos/%s/%s/" owner repo)))
    (fj-get endpoint)))

(define-button-type 'fj-user-repo-button
  'follow-link t
  'action 'fj-repo-tl-list-issues
  'help-echo "RET: View this repo's issues.")

(defvar-keymap fj-repo-tl-map
  :doc "Map for `fj-repo-tl-mode' and `fj-user-repo-tl-mode' to inherit."
  :parent fj-generic-tl-map
  "RET" #'fj-repo-tl-list-issues
  "M-RET" #'fj-repo-tl-list-pulls
  "*" #'fj-repo-tl-star-repo
  "c" #'fj-create-issue
  "s" #'fj-repo-search-tl
  "r" #'fj-repo-tl-readme
  "B" #'fj-tl-browse-entry
  "L" #'fj-repo-copy-clone-url
  "j" #'imenu)

(defvar-keymap fj-user-repo-tl-mode-map
  :doc "Map for `fj-user-repo-tl-mode', a tabluated list of repos."
  :parent fj-repo-tl-map
  "C-c C-x" #'fj-list-own-repos-read)

(define-derived-mode fj-user-repo-tl-mode tabulated-list-mode
  "fj-user-repos"
  "Mode for displaying a tabulated list of user repos."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("Name" 16 t)
          ("★" 3 fj-tl-sort-by-stars :right-align t)
          ("" 2 t)
          ("issues" 5 fj-tl-sort-by-issue-count :right-align t)
          ("Lang" 10 t)
          ("Updated" 12 t)
          ("Description" 55 nil)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(defun fj-get-current-user ()
  "Return the data for the current user."
  (fj-get "user"))

(defun fj-get-current-user-settings ()
  "Return settings for the current user."
  (fj-get "user/settings"))

(defun fj-get-user-repos (user &optional page limit order)
  "GET request repos owned by USER.
PAGE, LIMIT, ORDER."
  (let ((params (append
                 `(("limit" . ,(or limit (fj-default-limit))))
                 (fedi-opt-params page order)))
        (endpoint (format "users/%s/repos" user)))
    (fj-get endpoint params)))

(defun fj-user-repos-tl (&optional user page limit order)
  "View a tabulated list of respos for USER.
PAGE, LIMIT, ORDER."
  (interactive "sView user repos: ")
  (let* ((repos (fj-get-user-repos user page limit order))
         (entries (fj-repo-tl-entries repos :no-owner))
         (buf (format "*fj-repos-%s*" user)))
    (fj-repos-tl-render buf entries #'fj-user-repo-tl-mode)
    (with-current-buffer (get-buffer-create buf)
      (setq fj-buffer-spec
            `( :owner ,user :url ,(concat fj-host "/" user)
               :viewargs ( :user ,user :page ,page
                           :limit ,limit :order ,order)
               :viewfun fj-user-repos-tl)))))

(defun fj-list-own-repos-read ()
  "List repos for `fj-user', prompting for an order type."
  (interactive)
  (fj-list-own-repos '(4)))

(defun fj-list-own-repos (&optional limit order page)
  "List repos for `fj-user'.
With prefix arg ORDER, prompt for an argument to sort
results (server-side).
LIMIT and PAGE are for pagination."
  (interactive "P")
  (let* ((order (fj-repos-order-arg order))
         (buf (format "*fj-repos-%s*" fj-user))
         ;; FIXME: we should hit /users/$user/repos here not /user/repos?
         ;; but the former has "order" arg!
         (repos (and fj-user (fj-get-repos
                              limit nil nil page
                              (or order fj-own-repos-default-order))))
         (entries (fj-repo-tl-entries repos :no-owner)))
    (if (not repos)
        (user-error "No repos")
      (fj-repos-tl-render buf entries #'fj-user-repo-tl-mode :nil)
      (with-current-buffer (get-buffer-create buf)
        (setq fj-buffer-spec
              `( :owner ,fj-user :url ,(concat fj-host "/" fj-user)
                 :viewfun fj-list-own-repos
                 :viewargs (:order ,order :page ,page)))))))

(defun fj-repos-order-arg (&optional order)
  "Return an ORDER argument.
If ORDER is a string, return it.
If there is a prefix arg, completing read from `fj-own-repos-order'.
Else `fj-own-repos-default-order'."
  (cond ((stringp order) ;; we are paginating
         order)
        ((or current-prefix-arg ;; we are prefixing
             (equal order '(4))) ;; fj-list-own-repos-read
         (completing-read "Order repos by:" fj-own-repos-order))
        (t fj-own-repos-default-order)))

(defun fj-list-repos (&optional order page limit)
  "List repos for `fj-user' extended by `fj-extra-repos'.
Order by ORDER, paginate by PAGE and LIMIT."
  (interactive)
  (let* ((order (fj-repos-order-arg order))
         (buf (format "*fj-repos-%s*" fj-user))
         (own-repos (and fj-user (fj-get-repos limit nil nil page order)))
         (extra-repos (mapcar (lambda (repo)
                                (fj-get (format "repos/%s/" repo)))
                              fj-extra-repos))
         (repos (append own-repos extra-repos))
         (entries (fj-repo-tl-entries repos)))
    (if (not repos)
        (user-error "No (more) repos. \
Unless paginating, set `fj-user' or `fj-extra-repos'")
      (fj-repos-tl-render buf entries #'fj-repo-tl-mode)
      (with-current-buffer (get-buffer-create buf)
        (setq fj-buffer-spec
              `( :owner ,fj-user :url ,(concat fj-host "/" fj-user)
                 :viewfun fj-list-repos
                 :viewargs (:order ,order :page ,page)))))))

(defun fj-star-repo (repo owner &optional unstar)
  "Star or UNSTAR REPO owned by OWNER."
  (let* ((endpoint (format "user/starred/%s/%s" owner repo))
         (resp (if unstar
                   (fj-delete endpoint)
                 (fj-put endpoint))))
    (fedi-http--triage resp
                       (lambda (_)
                         (message "Repo %s %s!" repo
                                  (if unstar "unstarred" "starred"))))))

(defun fj-fork-repo (repo owner &optional name org)
  "Fork REPO owned by OWNER, optionally call fork NAME."
  (let* ((endpoint (format "repos/%s/%s/forks" owner repo))
         (params (fedi-opt-params name org))
         ;; ("organization" . ,org)))
         (resp (fj-post endpoint params :json)))
    (fedi-http--triage resp
                       (lambda (_)
                         (message "Repo %s forked!" repo)))))

(defun fj-delete-repo ()
  "Delete repo at point, if you are its owner."
  (interactive)
  (let* ((repo (or (fj--get-buffer-spec :repo) ;; issues tl
                   (fj--get-tl-col 0))) ;; own repos/search
         (endpoint (format "repos/%s/%s/" fj-user repo)))
    (if (not (fj--own-repo-p))
        (user-error "Not your own repo")
      (when (y-or-n-p
             (format "Delete repo %s [Permanent and cannot be undone]?"
                     repo))
        (let ((resp (fj-delete endpoint)))
          (fedi-http--triage resp
                             (lambda (_)
                               (message "Repo %s deleted!" repo))))))))

(defun fj-starred-repos ()
  "List your starred repos."
  (interactive)
  (fj--list-user-repos "starred" "starred" "?tab=stars"))

(defun fj-watched-repos ()
  "List your watched repos."
  (interactive)
  (fj--list-user-repos "subscriptions" "watched"))

(defun fj--list-user-repos (endpoint buf-str &optional url-str)
  "Fetch user data at /user/ENDPOINT and list them.
BUF-STR is to name the buffer, URL-STR is for the buffer-spec."
  (let* ((endpoint (format "/user/%s" endpoint))
         (repos (fj-get endpoint))
         (entries (fj-repo-tl-entries repos))
         (buf (format "*fj-%s-repos*" buf-str)))
    (fj-repos-tl-render buf entries #'fj-repo-tl-mode)
    (with-current-buffer buf
      (setq fj-buffer-spec
            `( :owner fj-user
               :url (when url-str
                      ,(concat fj-host "/" fj-user url-str))
               :viewfun fj--list-user-repos)))))

;;; USER REPOS

(defun fj-current-dir-repo ()
  "If we are in a `fj-host' repository, return its name.
Also set `fj-current-repo' to the name."
  ;; NB: fails if remote url is diff to root dir!
  (ignore-errors
    (when (magit-inside-worktree-p)
      ;; FIXME: this is slow, as we just fetch all our repos. why not repo
      ;; search, with dir name, and search repos with exclusive param set
      ;; to `fj-user's UID. unfortunately tho, we can't guess mode, so we
      ;; can't be sure of our result:
      ;; (let ((id (alist-get 'id
      ;;                      (fj-get-current-user)))
      ;;       (repo (fj-repo-search-do query nil id mode))))
      (let* ((repos (fj-get-repos nil nil :silent))
             (names (cl-loop for r in repos
                             collect (alist-get 'name r)))
             (dir (file-name-nondirectory
                   (directory-file-name
                    (magit-toplevel)))))
        (when (member dir names) ;; nil if dir no match any remotes
          (setq fj-current-repo dir))))))

(defun fj-get-repos (&optional limit no-json silent page order)
  "Return the user's repos.
Return LIMIT repos, LIMIT is a string.
NO-JSON is for `fj-get'.
ORDER should be a member of `fj-own-repos-order'."
  (let ((endpoint "user/repos")
        (params
         (append `(("limit" . ,(or limit (fj-default-limit))))
                 (fedi-opt-params page (order :alias "order_by")))))
    (fj-get endpoint params no-json silent)))

(defun fj-get-repo-candidates (repos)
  "Return REPOS as completion candidates."
  (cl-loop for r in repos
           collect (let-alist r
                     (list .name .id .description .owner.username))))

(defun fj-read-user-repo-do (&optional default silent)
  "Prompt for a user repository.
DEFAULT is initial input for `completing-read'.
SILENT means silent request."
  (let* ((repos (fj-get-repos (fj-max-items) nil silent))
         (cands (fj-get-repo-candidates repos)))
    (completing-read "Repo: " cands
                     nil nil default)))

(defun fj-read-user-repo (&optional arg)
  "Return a user repo.
If ARG is a prefix, prompt with `completing-read'.
If it is a string, return it.
Otherwise, try `fj-current-repo' and `fj-current-dir-repo'.
If both return nil, also prompt."
  (if (consp arg)
      (fj-read-user-repo-do nil :silent)
    (or arg
        fj-current-repo
        (fj-current-dir-repo) ;; requires loaded magit
        (fj-read-user-repo-do nil :silent))))

(defun fj-repo-create ()
  "Create a new repo.
Save its URL to the kill ring."
  (interactive)
  (let* ((name (read-string "Repo name: "))
         (desc (read-string "Repo description: "))
         (params `(("name" . ,name)
                   ("description" . ,desc)))
         (resp (fj-post "user/repos" params :json)))
    (fedi-http--triage resp
                       (lambda (resp)
                         (let* ((json (fj-resp-json resp))
                                (url (alist-get 'html_url json)))
                           (message "Repo %s created! %s" name url)
                           (kill-new url))))))

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

(defun fj-list-issues-sort ()
  "Reload current issues listing, prompting for a sort type.
The default sort value is \"latest\"."
  (interactive)
  ;; FIXME: broken in own issues view (needs to be unbound)
  (cl-destructuring-bind (&key repo owner state type
                               query labels milestones page limit)
      (fj--get-buffer-spec :viewargs)
    (let ((sort (completing-read "Sort by: " fj-issues-sort)))
      (fj-list-issues-do repo owner state type
                         query labels milestones page limit sort))))

;; GET /repos/{owner}/{repo}/issues
;; params: owner, repo, state, labels, q, type, milestones, since, before,
;; created_by, assigned_by, mentioned_by, page, limit
(defun fj-repo-get-issues (repo &optional owner state type query
                                labels milestones page limit sort)
  ;; TODO: since, before, created_by, assigned_by, mentioned_by
  ;; default sort = latest.
  "Return issues for REPO by OWNER.
STATE is for issue status, a string of open, closed or all.
TYPE is item type: issue pull or all.
QUERY is a search term to filter by.
Optionally limit results to LABELS or MILESTONES, which are
comma-separated lists.
PAGE is 1-based page of results to return.
LIMIT is the number of results."
  ;; FIXME: how to get issues by number, or get all issues?
  (let* ((endpoint (format "repos/%s/%s/issues" (or owner fj-user) repo))
         ;; NB: get issues has no sort param!
         (params
          (append `(("limit" . ,(or limit (fj-default-limit)))
                    ("sort" . ,(or sort fj-issues-sort-default)))
                  (fedi-opt-params state type
                                   (query :alias "q")
                                   labels milestones
                                   page limit))))
    (condition-case err
        (fj-get endpoint params)
      (t (format "%s" (error-message-string err))))))

(defun fj-issues-search
    (&optional state lables milestones query priority_repo_id type since
               before assigned created mentioned
               review_requested reviewed owner team page limit)
  "Make a GET request for issues matching QUERY.
Optionally limit search by OWNER, STATE, or TYPE.
Either QUERY or OWNER must be provided.
STATE is \"open\" or \"closed\".
TYPE is \"issues\" or \"pulls\".
Optionally filter results for those you have CREATED, been ASSIGNED to,
or MENTIONED in.
STATE defaults to open.
PAGE is a 1-based number for pagination.
MILESTONES and LABELS are comma-separated lists."
  ;; GET /repos/issues/search
  ;; NB: this endpoint can be painfully slow
  ;; NB: this endpoint has no sort!
  (let* ((endpoint "repos/issues/search")
         (params
          (append
           `(("limit" . ,(fj-default-limit)))
           (fedi-opt-params state lables milestones (query :alias "q")
                            priority_repo_id type since before
                            (assigned :boolean "true")
                            (created :boolean "true")
                            (mentioned :boolean "true")
                            review_requested reviewed owner
                            team page limit))))
    (condition-case err
        (fj-get endpoint params)
      (t (format "%s" (error-message-string err))))))

(defun fj-list-own-pulls (&optional query state
                                    created assigned mentioned)
  "List pulls in repos owned by `fj-user'.
QUERY, STATE, TYPE, CREATED, ASSIGNED, and MENTIONED are all for
`fj-issues-search'."
  (interactive)
  (fj-list-own-items
   query state "pulls" created assigned mentioned))

(defun fj-list-own-issues (&optional query state
                                     created assigned mentioned)
  "List issues in repos owned by `fj-user'.
QUERY, STATE, TYPE, CREATED, ASSIGNED, and MENTIONED are all for
`fj-issues-search'."
  (interactive)
  (fj-list-own-items
   query state "issues" created assigned mentioned))

(defun fj-list-own-items (&optional query state type
                                    created assigned mentioned page)
  "List items of TYPE in repos owned by `fj-user'.
QUERY, STATE, TYPE, CREATED, ASSIGNED, MENTIONED and PAGE are all for
`fj-issues-search'."
  (interactive)
  ;; NB: defaults are now required for buff spec:
  (let ((state (or state "open"))
        (type (or type "issues"))
        (items
         (fj-issues-search state nil nil query nil type nil nil
                           assigned created mentioned nil nil fj-user
                           nil page))
        (buf-name (format "*fj-user-repos-%s*" type))
        (prev-buf (buffer-name (current-buffer)))
        (prev-mode major-mode))
    ;; FIXME refactor with `fj-list-issues'? just tab list entries fun and
    ;; the buffer spec settings change
    (with-current-buffer (get-buffer-create buf-name)
      (setq tabulated-list-entries
            (fj-issue-tl-entries items :repo))
      (fj-owned-issues-tl-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq fj-buffer-spec
            `( :owner ,fj-user
               :viewfun fj-list-own-items
               :viewargs ( :query ,query :state ,state :type ,type
                           :created ,created :assigned ,assigned
                           :mentioned ,mentioned
                           :page ,page)))
      (fj-other-window-maybe
       prev-buf (format "-%s*" type) #'string-suffix-p prev-mode)
      (message (substitute-command-keys
                "\\`C-c C-c': cycle state | \\`C-c C-s': cycle type")))))

(defun fj-get-item (repo &optional owner number type)
  "GET ISSUE NUMBER, in REPO by OWNER.
If TYPE is :pull, get a pull request, not issue."
  (let* ((number (or number (if type
                                (fj-read-repo-pull-req repo)
                              (fj-read-repo-issue repo))))
         (owner (or owner fj-user)) ;; FIXME
         (endpoint (format "repos/%s/%s/%s/%s" owner repo
                           (if (eq type :pull) "pulls" "issues") number)))
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

(defun fj-issue-post (repo user title body &optional labels
                           assigneees closed due-date milestone ref)
  "POST a new issue to REPO owned by USER.
TITLE and BODY are the parts of the issue to send.
LABELS is a list of label names.
MILESTONE is a cons of title string and ID."
  ;; POST /repos/{owner}/{repo}/issues
  ;; assignee (deprecated) assignees closed due_date milestone ref
  (let* ((url (format "repos/%s/%s/issues" user repo))
         (milestone (cdr milestone))
         (params (append
                  `(("body" . ,body)
                    ("title" . ,title)
                    ("labels" . ,(cl-loop for x in labels
                                          collect (cdr x))))
                  (fedi-opt-params assigneees closed
                                   (due-date :alias "due_date")
                                   milestone ref))))
    (fj-post url params :json)))

(defun fj-issue-patch
    (repo owner issue &optional title body state assignee assignees
          due_date milestone ref unset_due_date updated_at)
  "PATCH/Edit ISSUE in REPO.
With PARAMS.
OWNER is the repo owner."
  ;; PATCH /repos/{owner}/{repo}/issues/{index}
  (let* ((params (fedi-opt-params
                  title body state assignee assignees due_date
                  milestone ref unset_due_date updated_at))
         (endpoint (format "repos/%s/%s/issues/%s" owner repo issue)))
    (fj-patch endpoint params)))

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
          (data (fj-get-item repo owner issue))
          (old-body (alist-get 'body data))
          (new-body (read-string "Edit issue body: " old-body))
          (response (fj-issue-patch repo owner issue nil new-body)))
     (fedi-http--triage response
                        (lambda (_)
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
         (data (fj-get-item repo owner issue))
         (old-title (alist-get 'title data))
         (body (alist-get 'body data))
         (new-title (read-string "Edit issue title: " old-title))
         (response (fj-issue-patch repo owner issue new-title body)))
    (fedi-http--triage response
                       (lambda (_)
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
                           (lambda (_)
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
                           (lambda (_)
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

(defun fj-pull-merge-post (repo owner number merge-type
                                &optional merge-commit)
  "POST a merge pull request REPO by OWNER.
NUMBER is that of the PR.
MERGE-TYPE is one of `fj-merge-types'.
MERGE-COMMIT is the merge commit ID, used for type manually-merged."
  (let ((url (format "repos/%s/%s/pulls/%s/merge" owner repo number))
        (params `(("owner" . ,owner)
                  ("repo" . ,repo)
                  ("index" . ,number)
                  ("Do" . ,merge-type)
                  ("MergeCommitId" . ,merge-commit))))
    (fj-post url params :json)))

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
;;     (fj-list-issues-do repo nil prs))) ;; FIXME: owner

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

(defun fj-issue-get-timeline (repo owner issue &optional page limit)
                                        ; since before
  "Return comments timeline for ISSUE in REPO.
OWNER is the repo owner.
Timeline contains comments and events of any type."
  (let* ((endpoint (format "repos/%s/%s/issues/%s/timeline"
                           owner repo issue))
         ;; NB: limit only works if page specified:
         (params (fedi-opt-params page limit)))
    (fj-get endpoint params)))

(defun fj-issue-timeline-more-link-mayb ()
  "Insert a Load more: link if there is more timeline data.
Increments the current page argument and checks for 2 more items.
Do nothing if there is no more data."
  (cl-destructuring-bind (&key repo owner number _type page limit)
      (fj--get-buffer-spec :viewargs)
    ;; NB: we can't limit=2 here, as then it assumes page 1 is also
    ;; limit=2, giving false results. sad but true:
    (fj-issue-get-timeline-async
     repo owner number (fj--inc-str page) limit
     #'fj-issue-timeline-more-link-mayb-cb
     (current-buffer))))

(defun fj-issue-timeline-more-link-mayb-cb (json buf)
  "Insert Load more: link at end of BUF, if JSON is non-nil."
  (with-current-buffer buf
    (when json
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; FIXME: remove on adding more!
          (insert
           (fj-propertize-link "[Load more]"
                               'more nil 'underline))
          (message "Load more"))))))

(defun fj-issue-get-timeline-async (repo owner issue
                                         &optional page limit cb
                                         &rest cbargs)
                                        ; since before
  "Return comments timeline for ISSUE in REPO asynchronously.
OWNER is the repo owner.
Timeline contains comments and events of any type.
PAGE and LIMIT are for pagination.
CB is a callback, called on JSON response as first arg, followed by CBARGS."
  (let* ((endpoint (format "repos/%s/%s/issues/%s/timeline"
                           owner repo issue))
         (url (fj-api endpoint))
         ;; NB: limit only works if page specified:
         (params (fedi-opt-params page limit)))
    (fj-authorized-request "GET"
      (apply #'fedi-http--get-json-async url params cb cbargs))))

(defun fj-get-comment (repo owner issue &optional comment)
  "GET data for COMMENT of ISSUE in REPO.
COMMENT is a number.
OWNER is the repo owner."
  (let* ((comment (or comment (fj-read-item-comment repo owner issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s"
                           owner repo comment)))
    (fj-get endpoint)))

(defun fj-issue-comment (&optional repo owner issue comment
                                   close)
  "Add COMMENT to ISSUE in REPO.
OWNER is the repo owner.
With arg CLOSE, also close ISSUE."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner fj-user)) ;; FIXME owner
         (url (format "repos/%s/%s/issues/%s/comments" owner repo issue))
         (body (or comment (read-string "Comment: ")))
         (params `(("body" . ,body)))
         (response (fj-post url params)))
    (fedi-http--triage response
                       (lambda (_)
                         (if (not close)
                             (message "comment created!")
                           (fj-issue-close repo owner issue)
                           (message "comment created, issue closed!"))))))

(defun fj-comment-patch (repo owner id &optional params issue json)
  "Edit comment with ID in REPO owned by OWNER.
PARAMS."
  (let* ((id (or id (fj-read-item-comment repo owner issue)))
         (endpoint (format "repos/%s/%s/issues/comments/%s" owner repo id)))
    (fj-patch endpoint params json)))

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
                       (lambda (_)
                         (message "comment edited!")))))

;;; ISSUE/COMMENT REACTIONS
;; render reactions

(defun fj-get-issue-reactions (id)
  "Return reactions data for comment with ID."
  ;; GET /repos/{owner}/{repo}/issues/{index}/reactions
  (fj-with-item-view
   (fj-destructure-buf-spec (owner repo)
     (let ((endpoint (format "repos/%s/%s/issues/%s/reactions"
                             owner repo id)))
       (fj-get endpoint)))))

(defun fj-get-comment-reactions (id)
  "Return reactions data for comment with ID."
  ;; GET /repos/{owner}/{repo}/issues/comments/{id}/reactions
  (fj-with-item-view
   (fj-destructure-buf-spec (owner repo)
     (let ((endpoint (format "repos/%s/%s/issues/comments/%s/reactions"
                             owner repo id)))
       (fj-get endpoint nil nil :silent)))))

(defun fj-render-issue-reactions (id)
  "Render reactions for issue with ID.
If none, return emptry string."
  (if-let* ((reactions (fj-get-issue-reactions id))
            (grouped (fj-group-reactions reactions)))
      (concat fedi-horiz-bar "\n"
              (mapconcat #'fj-render-grouped-reaction
                         grouped " "))
    ""))

(defun fj-render-comment-reactions (reactions)
  "Render REACTIONS for comment with ID.
If none, return emptry string."
  (if-let* ((grouped (fj-group-reactions reactions)))
      (concat fedi-horiz-bar "\n"
              (mapconcat #'fj-render-grouped-reaction
                         grouped " "))
    ""))

(defun fj-group-reactions (data)
  "Return an alist of reacting users keyed by emoji.
DATA is a list of single reactions."
  (let ((emoji (cl-remove-duplicates
                (cl-loop for x in data
                         collect (alist-get 'content x))
                :test #'string=)))
    (cl-loop
     for x in emoji
     collect
     (cons x
           (cl-loop
            for y in data
            when (equal x (alist-get 'content y))
            collect (map-nested-elt y '(user username)))))))

(defun fj-render-grouped-reaction (group)
  "Render a grouped reaction GROUP."
  (let ((count (number-to-string
                (length (cdr group)))))
    (propertize
     (format
      ":%s:%s"
      (car group)
      (propertize
       (concat " " count)
       'face 'fj-user-face
       'help-echo
       (mapconcat #'identity
                  (cdr group) " "))))))

;; create reactions

(defvar fj-base-reactions
  ;; TODO: render these during completion (:rocket: style emoji)
  '("laugh" "hooray" "+1" "-1" "confused" "heart" "rocket" "eyes")
  "Reactions as per the WebUI.
Not sure what the server actually accepts.")

(defun fj-add-reaction ()
  "Add reaction to issue, PR or comment at point."
  ;; POST /repos/{owner}/{repo}/issues/[comments/]{id}/reactions
  (interactive)
  (fj-with-item-view
   (fj-destructure-buf-spec (owner repo)
     (let ((data (fedi--property 'fj-item-data))
           (index (fedi--property 'fj-item-number)))
       (let-alist data
         (let* ((reac (completing-read "Reaction: " fj-base-reactions))
                (endpoint
                 (if (string= "comment" .type)
                     (format "repos/%s/%s/issues/comments/%s/reactions"
                             owner repo .id)
                   (format "repos/%s/%s/issues/%s/reactions"
                           owner repo index)))
                (params `(("content" . ,reac)))
                (resp (fj-post endpoint params :json)))
           (fedi-http--triage
            resp
            (lambda (_resp)
              (message "Reaction %s created!" reac)))))))))

(defun fj-item-own-reactions ()
  "Return a list of `fj-user's reactions of the item at point."
  (cl-loop for x in (fedi--property 'fj-reactions)
           when (string= fj-user
                         (map-nested-elt x '(user username)))
           collect (alist-get 'content x)))

(defun fj-remove-reaction ()
  "Remove a reaction from issue, PR or comment at point."
  ;; DELETE /repos/{owner}/{repo}/issues/[comments/]{id}/reactions
  ;; FIXME: deletes all `fj-user's reactions. poss API issue?
  (interactive)
  (fj-with-item-view
   (fj-destructure-buf-spec (owner repo)
     (let ((data (fedi--property 'fj-item-data))
           (own (fj-item-own-reactions)))
       (if (not own)
           (user-error "No own reactions here?")
         (let-alist data
           (let* ((reac (completing-read "Reaction: " own))
                  (endpoint
                   (format (if (string= "comment" .type)
                               "repos/%s/%s/issues/comments/%s/reactions"
                             "repos/%s/%s/issues/%s/reactions")
                           owner repo .id))
                  (params `(("content" . ,reac)))
                  (resp (fj-delete endpoint params :json)))
             (fedi-http--triage
              resp
              (lambda (_resp)
                (message "Reaction %s removed!" reac))))))))))

;;; ISSUE LABELS
;; TODO: - reload issue on add label
;;       - display label desc help-echo
;;       - add label from issues TL and from issue timeline
;;       - API doesn't implement initializing labels. ASKED

(defun fj-repo-get-labels (&optional repo owner)
  "Return labels JSON for REPO by OWNER."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (endpoint (format "repos/%s/%s/labels" owner repo)))
    (fj-get endpoint)))

(defun fj-issue-get-labels (&optional repo owner issue)
  "Get labels on ISSUE in REPO by OWNER."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue (fj-read-repo-issue repo)))
         (owner (or owner fj-user))
         (url (format "repos/%s/%s/issues/%s/labels" owner repo issue)))
    (fj-get url)))

(defun fj-issue-read-label (&optional repo owner issue id)
  "Read a label in the minibuffer and return it.
Label is for ISSUE in REPO by OWNER.
Return its name, or if ID, return a cons of its name and id."
  (let* ((labels (fj-repo-get-labels repo owner))
         ;; (pairs (fj--map-alist-to-cons labels 'name 'id))
         (pairs (fj-propertize-label-names labels))
         (choice
          (if issue
              (completing-read
               (format "Add label to #%s: " issue)
               pairs)
            (completing-read "Label: " pairs))))
    (if id
        (assoc choice pairs #'string=)
      choice)))

(defun fj-propertize-label-names (json)
  "Propertize all labels in label data JSON.
Return an alist, with each cons being (name . id)"
  (cl-loop ;; label JSON is a list of alists:
   for x in json
   collect
   (let-alist x
     (cons (fj-propertize-label .name (concat "#" .color) .description)
           .id))))

(defun fj-propertize-label (name color &optional desc)
  "Propertize NAME as a label, with COLOR prop and DESC, a description."
  (propertize name
              'face
              `( :inherit fj-label-face
                 :background ,color
                 :foreground ,(readable-foreground-color color))
              ;; FIXME: in label data for an issue, desc is empty
              'help-echo desc))

(defun fj-issue-label-add (&optional repo owner issue)
  "Add a label to ISSUE in REPO by OWNER."
  ;; POST "repos/%s/%s/issues/%s/labels"
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue
                    (fj--get-buffer-spec :item)
                    (fj-read-repo-issue repo)))
         (owner (or owner fj-user)) ;; FIXME owner
         (url (format "repos/%s/%s/issues/%s/labels" owner repo issue))
         (issue-labels (fj--map-alist-key
                        (fj-issue-get-labels repo owner issue)
                        'id)) ;; we post IDs to avoid duplicating:
         (choice (fj-issue-read-label repo owner issue :id))
         (id (cdr choice))
         (params `(("labels" . ,(cl-pushnew id issue-labels
                                            :test #'equal))))
         (resp (fj-post url params :json)))
    (fedi-http--triage
     resp
     (lambda (resp)
       (let ((json (fj-resp-json resp)))
         (message "%s" (prin1-to-string json))
         (fj-view-reload)
         (message "Label %s added to #%s!" (car choice) issue))))))

(defun fj-issue-label-remove (&optional repo owner issue)
  "Remove label from ISSUE in REPO by OWNER."
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (issue (or issue
                    (fj--get-buffer-spec :item)
                    (fj-read-repo-issue repo)))
         (owner (or owner fj-user)) ;; FIXME owner
         (issue-labels (fj-issue-get-labels repo owner issue)))
    (if (not issue-labels)
        (user-error "No labels to remove")
      (let* ((labels (fj-propertize-label-names issue-labels))
             (choice (completing-read
                      (format "Remove label from #%s: " issue)
                      labels))
             (id (cdr (assoc choice labels #'string=)))
             (color (fj-label-color-from-name choice labels))
             (url (format "repos/%s/%s/issues/%s/labels/%s"
                          owner repo issue id))
             (resp (fj-delete url)))
        (fedi-http--triage
         resp
         (lambda (_)
           (fj-view-reload)
           (let ((label (fj-propertize-label choice color)))
             (message "Label %s removed from #%s!" label issue))))))))

(defun fj-repo-create-label (&optional repo owner)
  "Create a new label for REPO by OWNER."
  ;; POST /repos/{owner}/{repo}/labels
  ;;  ((id . 456278) (name . "api") (exclusive . :json-false)
  ;;   (is_archived . :json-false) (color . "ee9a49") (description . "")
  ;;   (url . "https://codeberg.org/api/v1/repos/martianh/fj.el/labels/456278"))
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         ;; FIXME: check if name or color already used?:
         (name (read-string "Label: "))
         (color (read-color nil :rgb))
         ;; convert eg #D2D2B4B48C8C to #D2B48C:
         (color (concat (substring color 0 3)
                        (substring color 5 7)
                        (substring color 9 11)))
         (params `(("name" . ,name)
                   ("color" . ,color)))
         (endpoint (format "/repos/%s/%s/labels" owner repo))
         (resp (fj-post endpoint params :json)))
    (fedi-http--triage
     resp
     (lambda (_)
       (let ((label (fj-propertize-label name color)))
         (message "Label %s added to %s!" label repo))))))

(defun  fj-repo-delete-label (&optional repo owner)
  "Delete a label for REPO by OWNER."
  ;; DELETE /repos/{owner}/{repo}/labels/{id}
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (labels (fj-repo-get-labels repo owner))
         (list (fj-propertize-label-names labels))
         (choice (completing-read "Delete label: " list))
         (id (cdr (assoc choice list #'string=)))
         (color (fj-label-color-from-name choice list))
         (endpoint (format "/repos/%s/%s/labels/%s" owner repo id))
         (resp (fj-delete endpoint)))
    (fedi-http--triage
     resp
     (lambda (_)
       (fj-view-reload)
       (let ((label (fj-propertize-label choice color)))
         (message "Label %s deleted from %s" label repo))))))

(defun fj-label-color-from-name (name alist)
  "Fetch the backgroun property of NAME in ALIST.
ALIST is of labels as names and ids, and the names are propertized in
the label's color, as per `fj-propertize-label-names'."
  (plist-get
   (get-text-property 0 'face
                      (car (assoc name alist #'string=)))
   :background))

;;; MILESTONES

(defun fj-get-milestones (&optional repo owner state name page limit)
  "Get milestones for REPO by OWNER."
  ;; GET /repos/{owner}/{repo}/milestones
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (endpoint (format "/repos/%s/%s/milestones" owner repo))
         ;; state param worth implementing:
         (params (fedi-opt-params state name page limit)))
    (fj-get endpoint params)))

(defun fj-milestones-alist (milestones)
  "Return an alist of title and ID from MILESTONES."
  (cl-loop for m in milestones
           collect (cons (alist-get 'title m)
                         (alist-get 'id m))))

(defun fj-create-milestone (&optional repo owner)
  "Create a milestone for REPO by OWNER."
  ;; POST /repos/{owner}/{repo}/milestones due_on, state (open/closed)
  (interactive)
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (endpoint (format "/repos/%s/%s/milestones" owner repo))
         (title (read-string "Title: "))
         (desc (read-string "Description (optional): "))
         (params `(("title" . ,title)
                   ("description" . ,desc)))
         (resp (fj-post endpoint params :json)))
    (fedi-http--triage
     resp
     (lambda (_resp)
       (message "Milestone %s created!" title)))))

(defun fj-add-issue-to-milestone (&optional repo owner)
  "Add issue at point or in current view to milestone."
  (interactive)
  (fj-with-issue
   (let* ((repo (fj-read-user-repo repo))
          (owner (or owner fj-user))
          (issue (pcase major-mode
                   ('fj-item-view-mode
                    (fj--get-buffer-spec :item))
                   ('fj-issue-tl-mode
                    (fj--get-tl-col 0))))
          (milestones (fj-get-milestones repo owner))
          (alist (fj-milestones-alist milestones))
          (choice (completing-read "Milestone: " alist))
          (id (number-to-string
               (cdr (assoc choice alist #'equal))))
          (resp (fj-issue-patch repo owner issue nil nil
                                nil nil nil nil id)))
     (fedi-http--triage
      resp
      (lambda (_resp)
        (message "%s added to milestone %s" issue choice))))))

;;; ISSUES TL

;;; mode and listings rendering

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

(defvar-keymap fj-issue-tl-mode-map
  :doc "Map for `fj-issue-tl-mode', a tabluated list of issues."
  :parent fj-generic-tl-map ; has nav
  "C"        #'fj-issues-tl-comment
  "e"        #'fj-issues-tl-edit
  "t"        #'fj-issues-tl-edit-title
  "v"        #'fj-issues-tl-view
  "k"        #'fj-issues-tl-close
  "K"        #'fj-issues-tl-delete
  "c"        #'fj-create-issue
  "C-c C-x"  #'fj-list-issues-sort
  "C-c C-c"  #'fj-cycle-state
  "C-c C-s"  #'fj-cycle-type
  "o"        #'fj-issues-tl-reopen
  "s"        #'fj-list-issues-search
  "B"        #'fj-tl-browse-entry
  "u"        #'fj-repo-copy-clone-url
  "L"        #'fj-repo-commit-log
  "j"        #'imenu
  "l"        #'fj-issues-tl-label-add
  "U"        #'fj-copy-pr-url)

(define-derived-mode fj-issue-tl-mode tabulated-list-mode
  "fj-issues"
  "Major mode for browsing a tabulated list of issues."
  :group 'fj
  (hl-line-mode 1)
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        ;; this is changed by `tabulated-list-sort' which sorts by col at point:
        ;; Superceded by new API sort param:
        ;; tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("#" 5 fj-tl-sort-by-issues :right-align)
          ("💬" 3 fj-tl-sort-by-comment-count :right-align)
          ("Author" 10 t)
          ("Updated" 12 t) ;; instead of display, you could use a sort fun here
          ("Issue" 20 t)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(define-button-type 'fj-issue-button
  'follow-link t
  'action 'fj-issues-tl-view
  'help-echo "RET: View this issue.")

(defvar-keymap fj-owned-issues-tl-mode-map
  :doc "Map for `fj-owned-issues-tl-mode', a tabluated list of issues."
  :parent fj-issue-tl-mode-map ; has nav
  "C-c C-c" #'fj-cycle-state
  "C-c C-s" #'fj-cycle-type)

;; FIXME: refactor with `fj-issue-tl-mode' mode?
;; this just adds Repo col
(define-derived-mode fj-owned-issues-tl-mode fj-issue-tl-mode
  "fj-own-issues"
  "Major mode for browsing a tabulated list of issues."
  :group 'fj
  (hl-line-mode 1)
  (keymap-unset fj-owned-issues-tl-mode-map
                "C-c C-x") ;; unset sort from issues tl
  (setq tabulated-list-padding 0 ;2) ; point directly on issue
        ;; this is changed by `tabulated-list-sort' which sorts by col at point:
        tabulated-list-sort-key '("Updated" . t) ;; default
        tabulated-list-format
        '[("#" 5 fj-tl-sort-by-issues :right-align)
          ("💬" 3 fj-tl-sort-by-comment-count :right-align)
          ("Repo" 10 t)
          ("Author" 10 t)
          ("Updated" 12 t) ;; instead of display, you could use a sort fun here
          ("Issue" 20 t)])
  (setq imenu-create-index-function #'fj-tl-imenu-index-fun))

(define-button-type 'fj-owned-issues-repo-button
  'follow-link t
  'action 'fj-owned-issues-list-repo-issues
  'help-echo "RET: View this repo's issues.")

;; FIXME: refactor with `fj-issue-tl-mode'? just the name seq elt changes
(defun fj-owned-issues-list-repo-issues (&optional _)
  "View issues of current repo from tabulated repos listing."
  (interactive)
  (fj-with-repo-entry
   (let* ((entry (tabulated-list-get-entry))
          (name (car (seq-elt entry 2)))
          (owner (fj--repo-owner)))
     (fj-list-issues-do name owner))))

(defun fj-issue-tl-entries (issues &optional repo)
  "Return tabluated list entries for ISSUES.
If REPO is provided, also include a repo column."
  (cl-loop
   for issue in issues
   collect
   (let-alist issue
     ;; FIXME: GET full item data and set to fj-item-data?
     ;; e.g. pulls listed don't have full base/head data (e.g. branch)
     (let* ((updated (date-to-time .updated_at))
            (updated-str (format-time-string "%s" updated))
            (updated-display (fedi--relative-time-description updated nil :brief))
            (type (if .pull_request 'pull 'issue)))
       ;; NB: avoid using propertize here as it creates cells with
       ;; unreadable/hash #(blah) notation:
       `(nil ;; TODO: id
         [(,(number-to-string .number)
           id ,.id
           state ,.state
           type fj-issue-button
           item ,type
           fj-url ,.html_url
           fj-item-data ,issue
           fj-tab-stop t)
          (,(number-to-string .comments)
           face fj-figures-face
           item ,type)
          ,@(when repo
              `((,.repository.name face fj-user-face
                                   id ,.repository.name
                                   state ,.state
                                   type fj-owned-issues-repo-button
                                   item ,type
                                   fj-tab-stop t)))
          (,.user.username face fj-user-face
                           id ,.id
                           state ,.state
                           type  fj-issues-owner-button
                           item ,type
                           fj-tab-stop t)
          (,updated-str
           display ,updated-display
           face default
           item ,type)
          (,(concat
             (fj-format-tl-title .title .state)
             (fj-plain-space)
             (fj-propertize-labels .labels))
           id ,.id
           state ,.state
           type fj-issue-button
           item ,type)])))))

(defun fj-format-tl-title (str &optional state face verbatim-face)
  "Propertize STR, respecting its state (open/closed).
Propertize any verbatim markdown in STR."
  (let ((face (or face
                  (if (equal state "closed")
                      'fj-closed-issue-face
                    'fj-item-face)))
        (verbatim (or verbatim-face
                      (if (equal state "closed")
                          'fj-item-closed-verbatim-face
                        'fj-item-verbatim-face))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert
       (propertize str 'face face))
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward markdown-regex-code nil :noerror)
          (add-text-properties (match-beginning 1) (match-end 1)
                               `(face ,verbatim)
                               (current-buffer))
          (replace-match (buffer-substring (match-beginning 3)
                                           (match-end 3))
                         nil nil nil 1)))
      (buffer-string))))

(defun fj-plain-space ()
  "Return a space with default face."
  (propertize " "
              'face 'default))

(defun fj-propertize-labels (data)
  "Propertize and concat labels in DATA."
  (if (null data)
      ""
    (mapconcat
     (lambda (l)
       (let-alist l
         (let ((bg (concat "#" .color)))
           (fj-propertize-label .name bg .description))))
     data
     (fj-plain-space))))

;;; issues tl commands

(defvar fj-non-fj-hosts
  '("github.com" "gitlab.com" "bitbucket.com"))

(defun fj--forgejo-repo-maybe (url)
  "Nil if the host of URL is a member of `fj-non-fj-hosts'.
Otherwise t."
  (let* ((parsed (url-generic-parse-url url)))
    (not (cl-member-if (lambda (x)
                         (string-prefix-p x (url-host parsed)))
                       fj-non-fj-hosts))))

(defun fj-repo-+-owner-from-git ()
  "Return repo and owner from git config.
Nil if we fail to parse."
  ;; https://git-scm.com/book/en/v2/Git-on-the-Server-The-Protocols
  ;; docs are unclear on how to distinguish these!
  (let* ((remote (fj-git-config-remote-url)))
    (when (fj--forgejo-repo-maybe remote)
      (cond
       ((string-prefix-p "http" remote) ;; http(s)
        (last (split-string remote "/") 2))
       ;; git protocol: git:// or git@...?
       ;; can't just be prefix "git" because that matches ssh gitea@...
       ((string-prefix-p "git://" remote) ;; git maybe
        nil) ;;  TODO: git protocol
       (t ;; ssh (can omit ssh:// prefix)
        ;; “sshuser@domain.com:username/repo.git”
        ;; nb sshuser is not the foregejo user!
        (let* ((split (split-string remote "[@:/]"))
               (repo (string-trim-right (nth 3 split) ".git")))
          (list (nth 2 split) repo)))))))

(defun fj-list-issues-+-pulls (repo &optional owner state)
  "List issues and pulls for REPO by OWNER, filtered by STATE."
  (interactive "P")
  (fj-list-items repo owner state "all"))

(defun fj-list-pulls (repo &optional owner state)
  "List pulls for REPO by OWNER, filtered by STATE."
  (interactive "P")
  (fj-list-items repo owner state "pulls"))

(defun fj-list-issues (&optional repo)
  "List issues for current REPO with default sorting.
If we are in a repo, don't assume `fj-user' owns it. In that case we
fetch owner/repo from git config.
If we are not in a repo, call `fj-list-issues-do' without using git
config.
The default sort value is \"latest\"."
  (interactive "P")
  (fj-list-items repo nil nil "issues"))

(defun fj-list-items (&optional repo owner state type)
  "List issues or pulls for REPO by OWNER, filtered by STATE and TYPE.
TYPE is item type, a member of `fj-items-types'.
STATE is a member of `fj-items-states'.
If we are in a repo, don't assume `fj-user' owns it. In that case we
fetch owner/repo from git config and check if it might have a foregejo
remote.
If we are not in a repo, call `fj-list-issues-do' without using
git config."
  (interactive "P")
  (if (or current-prefix-arg ;; still allow completing-read a repo
          (not (magit-inside-worktree-p :noerror)))
      (fj-list-issues-do repo owner state type) ;; fall back to `fj-user' repos
    ;; if in fj buffer, respect its buf-spec:
    (if (string-prefix-p "*fj-" (buffer-name (current-buffer)))
        (let* ((repo (fj-read-user-repo repo))
               (owner (or owner fj-user)))
          (fj-list-issues-do repo owner state type))
      ;; if NOT fj.el buffer, try to get info from git:
      (if-let* ((repo-+-owner (fj-repo-+-owner-from-git))
                (owner (or owner (car repo-+-owner)))
                (repo (or repo (cadr repo-+-owner))))
          (fj-list-issues-do repo owner state type)
        (message "Failed to find Forgejo repo")
        (fj-list-issues-do repo owner state type)))))

(defun fj-list-issues-by-milestone (&optional repo owner state type query
                                              labels)
  "List issues in REPO by OWNER, filtering by milestone.
STATE, TYPE and QUERY are for `fj-list-issues-do'."
  (interactive)
  (let* ((milestones (fj-get-milestones repo owner))
         (alist (fj-milestones-alist milestones))
         (milestone (completing-read "Milestone: " alist)))
    (fj-list-issues-do repo owner state type query labels milestone)))

(defun fj-list-issues-by-label (&optional repo owner state type query)
  "List issues in REPO by OWNER, filtering by label.
STATE, TYPE and QUERY are for `fj-list-issues-do'."
  (interactive)
  ;; FIXME: labels list is CSV, so we should do that here and send on:
  (let* ((label (fj-issue-read-label)))
    (fj-list-issues-do repo owner state type query label)))

(defvar fj-repo-data nil) ;; for transients for now

(defun fj-list-issues-do (&optional repo owner state type query
                                    labels milestones page limit sort)
  "Display ISSUES in a tabulated list view.
Either for `fj-current-repo' or REPO, a string, owned by OWNER.
With a prefix arg, or if REPO and `fj-current-repo' are nil,
prompt for a repo to list.
Optionally specify the STATE filter (open, closed, all), and the
TYPE filter (issues, pulls, all).
QUERY is a search query to filter by."
  (interactive "P")
  (let* ((repo (fj-read-user-repo repo))
         (owner (or owner fj-user))
         (type (or type "issues"))
         (issues (fj-repo-get-issues repo owner state type query
                                     labels milestones page limit sort))
         (repo-data (fj-get-repo repo owner))
         (has-issues (fj-repo-has-items-p type repo-data))
         (url (concat (alist-get 'html_url repo-data)
                      (if (equal type "pulls")
                          "/pulls"
                        "/issues"))) ;; all is also issues
         (prev-buf (buffer-name (current-buffer)))
         (prev-mode major-mode)
         (state-str (or state "open"))
         (wd default-directory)
         (buf-name (format "*fj-%s-%s-%s*" repo state-str type)))
    (if (not has-issues)
        (user-error "Repo does not have %s" type)
      (with-current-buffer (get-buffer-create buf-name)
        (setq tabulated-list-entries
              (fj-issue-tl-entries issues))
        (fj-issue-tl-mode)
        (tabulated-list-init-header)
        (tabulated-list-print)
        (setq fj-current-repo repo
              fj-repo-data repo-data
              fj-buffer-spec
              ;; viewargs must match function signature, but also we
              ;; duplicate it outside of view args for easy destructuring
              ;; for other actions
              `( :repo ,repo :owner ,owner
                 :viewargs
                 ( :repo ,repo :owner ,owner :state ,state-str
                   :type ,type :query ,query :labels ,labels
                   :milestones ,milestones
                   :page ,page :limit ,limit)
                 :viewfun fj-list-issues-do
                 :url ,url))
        ;; ensure our .dir-locals.el settings take effect:
        ;; via https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
        (setq default-directory wd)
        (let ((enable-local-variables :all))
          (hack-dir-local-variables-non-file-buffer))
        (fj-other-window-maybe
         prev-buf "-issues*" #'string-suffix-p prev-mode)
        (message (substitute-command-keys
                  ;; it can't find our bindings:
                  "\\`C-c C-c': cycle state | \\`C-c C-x': sort\
 | \\`C-c C-s': cycle type"))))))

(defun fj-repo-has-items-p (type data)
  "Return t if repo DATA has items of TYPE enabled."
  (let ((key (if (equal type "pulls")
                 'has_pull_requests
               'has_issues)))
    (eq t ;; i.e. not :json-false
        (alist-get key data))))

(defun fj-other-window-maybe (prev-buf string suffix-or-prefix
                                       &optional prev-mode)
  "Conditionally call `switch-to-buffer' or `switch-to-buffer-other-window'.
Depending on where we are. PREV-BUF is the name of the
previous buffer. STRING is a buffer name string to be checked by
SUFFIX-OR-PREFIX, ie `string-suffix-p' or `string-prefix-p'.
PREV-MODE is the major mode active in the previous buffer."
  ;; TODO: it is reasonable to keep same window if same mode?
  ;; else it seems to be just a mess using buffer names, as they can often be
  ;; used to add detail precisely about the current view
  (cond ((string= (buffer-name) prev-buf) ; same repo
         nil)
        ;; FIXME: don't use buffer names (pulls/state):
        ((or (equal prev-mode major-mode)
             (funcall suffix-or-prefix string prev-buf)) ; diff repo
         (switch-to-buffer (current-buffer)))
        (t                             ; new buf
         (switch-to-buffer-other-window (current-buffer)))))

(defun fj-list-issues-search (query &optional state type)
  "Search current repo issues for QUERY.
STATE and TYPE as for `fj-list-issues-do'."
  (interactive "sSearch issues in repo: ")
  (let ((owner (fj--get-buffer-spec :owner)))
    (fj-list-issues-do nil owner (or state "all") type query)))

(defun fj-list-issues-closed (&optional repo owner type)
  "Display closed ISSUES for REPO by OWNER in tabulated list view.
TYPE is the item type."
  (interactive "P")
  (fj-list-issues-do repo owner "closed" type))

(defun fj-list-issues-all (&optional repo owner type)
  "Display all ISSUES for REPO by OWNER in tabulated list view.
TYPE is the item type."
  (interactive "P")
  (fj-list-issues-do repo owner "all" type))

;;; VIEW CYCLE

(defvar fj-items-states
  '("open" "closed" "all"))

(defvar fj-items-types
  '("issues" "pulls" "all"))

(defun fj-next-item-var (current var)
  "Return the next item in VAR based on CURRENT."
  (let ((mem (member current var)))
    (if (length= mem 1)
        (car var)
      (cadr mem))))

(defun fj-next-item-state-plist (plist)
  "Update the value of :state in PLIST and return it."
  (let* ((current (plist-get plist :state))
         (next (fj-next-item-var current fj-items-states)))
    (plist-put plist :state next)))

(defun fj-next-item-type-plist (plist)
  "Update the value of :type in PLIST and return it."
  (let* ((current (plist-get plist :type))
         (next (fj-next-item-var current fj-items-types)))
    (plist-put plist :type next)))

(defun fj-cycle-state ()
  "Cycle item state listing of open, closed, and all."
  (interactive)
  (fj-destructure-buf-spec (viewfun viewargs)
    (let ((args (fj-next-item-state-plist viewargs)))
      (apply viewfun (fj-plist-values args)))))

(defun fj-cycle-type ()
  "Cycle item type listing of issues, pulls, and all."
  (interactive)
  (fj-destructure-buf-spec (viewfun viewargs)
    (let ((args (fj-next-item-type-plist viewargs)))
      (apply viewfun (fj-plist-values args)))))

;;; RELOADING

(defun fj-view-reload ()
  "Try to reload the current view based on its major-mode."
  (interactive)
  (fj-destructure-buf-spec (viewfun viewargs)
    ;; works so long as we set viewargs right:
    (apply viewfun (fj-plist-values viewargs))))

;;; ISSUE VIEW
(defvar fj-url-regex fedi-post-url-regex)

(defun fj-mdize-plain-urls ()
  "Markdown-ize any plain string URLs found in current buffer."
  ;; FIXME: this doesn't rly work with ```verbatim``` in md
  ;; NB: this must not break any md, otherwise `markdown-standalone' may
  ;; hang!
  (save-match-data
    ;; FIXME: this will match ). after url and put point after both:
    ;; that means it will put < > surreounding the ).
    (while (re-search-forward fj-url-regex nil :no-error)
      (unless
          (save-excursion
            ;;
            (goto-char (- (point) 2))
            (or (markdown-inside-link-p)
                ;; bbcode (seen in spam, breaks markdown if url replaced):
                (let ((regex (concat "\\[url=" markdown-regex-uri "\\/\\]"
                                     ".*" ; description
                                     "\\[\\/url\\]")))
                  (thing-at-point-looking-at regex))))
        (replace-match
         (concat "<" (match-string 0) ">"))))))

(defvar fj-previous-window-config nil
  "A list of window configuration prior to composing a toot.
Takes its form from `window-configuration-to-register'.")

(defun fj-restore-previous-window-config (config)
  "Restore the window CONFIG after killing the toot compose buffer.
Buffer-local variable `fj-previous-window-config' holds the config."
  (set-window-configuration (car config))
  (goto-char (cadr config)))

(defun fj-render-markdown (text)
  "Return server-rendered markdown TEXT."
  ;; NB: sync request:
  (let* ((resp (fj-post "markdown" `(("text" . ,text)) nil :silent)))
    (fedi-http--triage
     resp (lambda (resp) (fj-resp-str resp)))))

(defun fj-body-prop-regexes (str json)
  "Propertize items by regexes in STR.
JSON is the data associated with STR."
  (insert str)
  (goto-char (point-min))
  (fedi-propertize-items fedi-post-handle-regex 'handle json
                         fj-link-keymap 1 2 nil nil
                         '(fj-tab-stop t))
  (fedi-propertize-items fedi-post-tag-regex 'tag json
                         fj-link-keymap 1 2 nil nil
                         '(fj-tab-stop t))
  ;; NB: this is required for shr tab stops
  ;; - why doesn't shr always add shr-tab-stop prop?
  ;; - does not add tab-stops for []() links (nor does shr!?)
  ;; - fixed prev breakage here by adding item as link in
  ;; - `fedi-propertize-items'.
  (fedi-propertize-items fedi-post-url-regex 'shr json
                         fj-link-keymap 1 1 nil nil
                         '(fj-tab-stop t))
  ;; FIXME: md []() links:
  ;; doesn't work
  ;; (setq str
  ;;       (fedi-propertize-items str markdown-regex-link-inline 'shr json
  ;;                              fj-link-keymap 1 1 nil nil
  ;;                              '(fj-tab-stop t)))
  (fedi-propertize-items fedi-post-commit-regex 'commit json
                         fj-link-keymap 1 1 nil nil
                         '(fj-tab-stop t)
                         'fj-issue-commit-face)
  (buffer-string))

;; I think magit/forge just uses markdown-mode rather than rendering
(defun fj-render-body (body)
  "Render BODY as markdown and decode."
  (decode-coding-string
   (fj-render-markdown body)
   'utf-8))

(defun fj-render-item-bodies ()
  "Render all item bodies in the buffer.
Uses property fj-item-body to find them.
Also propertize all handles, tags, commits, and URLs."
  (save-match-data
    (let ((inhibit-read-only t)
          match)
      (save-excursion
        (goto-char (point-min))
        (while (setq match (text-property-search-forward 'fj-item-body))
          (let ((shr-width (- (window-width) 2))
                (shr-discard-aria-hidden t) ; for pandoc md image output
                (props (text-properties-at (point) (current-buffer))))
            ;; (fj-mdize-plain-urls) ;; FIXME: still needed since we
            ;; changed to buffer parsing?
            (shr-render-region (prop-match-beginning match)
                               (prop-match-end match)
                               (current-buffer))
            ;; Re-add props (so we can edit when point on body, etc.):
            (add-text-properties (prop-match-beginning match)
                                 (point)
                                 props
                                 (current-buffer)))))
      ;; propertize handles, tags, URLs, and commits
      (fedi-propertize-items fedi-post-handle-regex 'handle
                             fj-link-keymap 1 2 nil nil
                             '(fj-tab-stop t))
      (fedi-propertize-items fedi-post-tag-regex 'tag
                             fj-link-keymap 1 2 nil nil
                             '(fj-tab-stop t))
      ;; NB: this is required for shr tab stops
      ;; - why doesn't shr always add shr-tab-stop prop?
      ;; - does not add tab-stops for []() links (nor does shr!?)
      ;; - fixed prev breakage here by adding item as link in
      ;; - `fedi-propertize-items'.
      (fedi-propertize-items fedi-post-url-regex 'shr
                             fj-link-keymap 1 1 nil nil
                             '(fj-tab-stop t))
      (fedi-propertize-items fedi-post-commit-regex 'commit
                             fj-link-keymap 1 1 nil nil
                             '(fj-tab-stop t)
                             'fj-issue-commit-face)
      ;; FIXME: make md []() links tab stops? (doesn't work):
      ;; (fedi-propertize-items markdown-regex-link-inline 'shr
      ;;                        fj-link-keymap 1 1 nil nil
      ;;                        '(fj-tab-stop t))
      )))

(defvar-keymap fj-item-view-mode-map
  :doc "Keymap for `fj-item-view-mode'."
  :parent  fj-generic-map
  "e" #'fj-item-view-edit-item-at-point
  "c" #'fj-item-view-comment
  "k" #'fj-item-view-close
  "o" #'fj-item-view-reopen
  "K" #'fj-item-view-comment-delete
  "t" #'fj-issue-view-edit-title
  "s" #'fj-list-issues-search
  "D" #'fj-view-pull-diff
  "L" #'fj-repo-commit-log
  "l" #'fj-issue-label-add
  "M" #'fj-merge-pull
  "r" #'fj-add-reaction
  ">" #'fj-item-view-more)

(define-derived-mode fj-item-view-mode special-mode "fj-issue"
  "Major mode for viewing items."
  :group 'fj
  (read-only-mode 1))

(defun fj-format-comment (comment &optional author owner)
  "Format COMMENT.
AUTHOR is of comment, OWNER is of repo."
  (let-alist comment
    (let ((stamp (fedi--relative-time-description
                  (date-to-time .created_at)))
          (reactions (fj-get-comment-reactions .id)))
      (propertize
       (concat
        (fj-format-comment-header
         .user.username author owner
         (fj-edited-str-maybe .created_at .updated_at)
         stamp)
        "\n\n"
        (propertize (fj-render-body .body)
                    'fj-item-body t)
        "\n"
        (fj-render-comment-reactions reactions) "\n"
        fedi-horiz-bar fedi-horiz-bar)
       'fj-comment comment
       'fj-comment-author .user.username
       'fj-comment-id .id
       'fj-reactions reactions))))

(defun fj-format-comment-header (username author owner edited ts)
  "Format a comment header line.
USERNAME is the commenter, AUTHOR is of the item, OWNER of the repo.
EDITED is a formatted string if the item is edited.
TS is a formatted timestamp."
  (concat
   (fj-render-item-user username)
   (propertize " "
               'face 'fj-item-byline-face)
   (fj-author-or-owner-str username author)
   (propertize " "
               'face 'fj-item-byline-face)
   (fj-author-or-owner-str username nil owner)
   edited ;; (fj-edited-str-maybe .created_at .updated_at)
   (propertize (fj--issue-right-align-str ts)
               'face 'fj-item-byline-face)))

(defun fj-render-comments (comments &optional author owner)
  "Render a list of COMMENTS.
AUTHOR is the author of the parent issue.
OWNER is the repo owner."
  (cl-loop for c in comments
           concat (fj-format-comment c author owner)))

(defun fj-prop-item-flag (str)
  "Propertize STR as author face in box."
  (propertize str
              'face '(:inherit fj-item-byline-face :box t)))

(defun fj-author-or-owner-str (username author &optional owner)
  "If USERNAME is equal either AUTHOR or OWNER, return a boxed string."
  (let ((name (or owner author)))
    (if (equal name username)
        (fj-prop-item-flag (if owner "owner" "author"))
      "")))

(defun fj-render-labels (labels)
  "Render LABELS, a list of issue labels."
  (concat "\nLabels: "
          (fj-propertize-labels labels)))

(defun fj-edited-str-maybe (created updated)
  "If UPDATED timestamp is after CREATED timestamp, return edited str."
  (let ((c-secs (time-to-seconds
                 (date-to-time created)))
        (u-secs (time-to-seconds
                 (date-to-time updated))))
    (when (> u-secs c-secs)
      (concat (propertize " "
                          'face 'fj-item-byline-face)
              (fj-prop-item-flag "edited")))))

(defun fj-render-item (repo owner item number
                            &optional type page limit)
  "Render ITEM number NUMBER, in REPO and its TIMELINE.
OWNER is the repo owner.
RELOAD mean we reloaded."
  (let ((header-line-indent " "))
    (header-line-indent-mode 1) ; broken?
    (let-alist item
      (let* ((stamp (fedi--relative-time-description
                     (date-to-time .created_at)))
             (pull-p .base) ;; rough PR check!
             (type (or type (if pull-p :pull :issue))))
        ;; set vars before timeline so they're avail:
        (setq fj-current-repo repo)
        (setq fj-buffer-spec
              `( :repo ,repo :owner ,owner
                 :item ,number ;; used by lots of stuff
                 :type ,type ;; used by with-pull
                 :author ,.user.username ;; used by own-issue
                 :title ,.title ;; for commenting
                 :body ,.body ;; for editing
                 :url ,.html_url ;; for browsing
                 :viewfun fj-item-view
                 ;; signature: repo owner number pull page limit:
                 :viewargs ( :repo ,repo :owner ,owner :number ,number
                             :type ,type
                             :page ,page :limit ,limit)))
        ;; .is_locked
        (setq header-line-format
              `("" header-line-indent
                ;; number:
                ,(concat "#" (number-to-string .number) " "
                         ;; title:
                         (fj-format-tl-title .title .state))))
        (insert
         ;; header stuff
         ;; (forge has: state, status, milestone, labels, marks, assignees):
         (propertize
          (concat
           "State: " .state
           (if (string= "closed" .state)
               (concat " " (fedi--relative-time-description
                            (date-to-time .closed_at)))
             "")
           (when .milestone
             (concat "\nMilestone: " .milestone.title "\n"))
           (if .labels
               (fj-render-labels .labels)
             "")
           ;; PR stuff:
           (if pull-p
               (format
                (if (eq :json-false .merged)
                    "\n%s wants to merge from %s into %s"
                  "\n%s merged from %s into %s")
                (propertize .user.username
                            'face 'fj-name-face)
                ;; FIXME: make links work! data doesn't have branch URLs
                ;; and gitnex doesn't linkify them
                ;; webUI uses $fork-repo/src/branch/$name:
                (propertize
                 (format "%s:%s".head.repo.full_name .head.label)
                 'face 'fj-name-face)
                (propertize .base.label 'face 'fj-name-face))
             "")
           "\n\n"
           ;; item byline:
           ;; FIXME: :extend t doesn't work here whatever i do
           (fj-render-item-user (concat .user.username))
           (propertize " "
                       'face 'fj-item-byline-face)
           (fj-author-or-owner-str .user.username nil owner)
           ;; FIXME: this diffing will mark any issue as edited if it has
           ;; merely been commented on.
           ;; (fj-edited-str-maybe .created_at .updated_at)
           (propertize (fj--issue-right-align-str stamp)
                       'face 'fj-item-byline-face)
           "\n\n"
           (propertize (fj-render-body .body)
                       'fj-item-body t)
           "\n"
           (fj-render-issue-reactions .number)
           "\n"
           fedi-horiz-bar fedi-horiz-bar "\n\n")
          'fj-item-number number
          'fj-repo repo
          'fj-item-data item))
        ;; FIXME: move this to after async timeline rendering?:
        (insert
         (pcase .mergeable
           (:json-false "This PR has changes conflicting with the target branch.\n\n")
           ('t
            "This PR can be merged automatically.\n\n")
           (_ "")))
        (when (and fj-use-emojify
                   (require 'emojify nil :noerror))
          (emojify-mode t))
        ;; Propertize top level item only:
        (fj-render-item-bodies)))))

(defun fj-item-view (&optional repo owner number type page limit)
  "View item NUMBER from REPO of OWNER.
RELOAD means we are reloading, so don't open in other window.
TYPE is :pull or :list (default).
PAGE and LIMIT are for `fj-issue-get-timeline'."
  (interactive "P")
  (let* ( ;; set defaults for pagination:
         (page (or page "1"))
         (limit (or limit (fj-timeline-default-items)))
         ;; mode check for other-window arg:
         (ow (not (eq major-mode 'fj-item-view-mode)))
         (repo (fj-read-user-repo repo))
         (item (fj-get-item repo owner number type))
         (number (or number (alist-get 'number item))))
    ;; (timeline (fj-issue-get-timeline repo owner number page limit)))
    (fedi-with-buffer (format "*fj-%s-item-%s*" repo number)
        'fj-item-view-mode ow
      (let ((enable-local-variables :all))
        (hack-dir-local-variables-non-file-buffer))
      (fj-render-item repo owner item number type page limit)
      ;; if we have paginated, re-append all pages sequentially:
      (if (fj-string-number> page "1")
          (fj-reload-paginated-pages)
        (fj-item-view-more* page)))))

(defun fj-reload-paginated-pages (&optional end-page)
  "Reload a page of timeline items.
Possibly reload more than one page. The idea is that if you reply to an
item after having paginated through a timeline, we reload the timeline
up to where you were, page by page.
`fj-item-view' calls this with no args.
In subsequent calls, END-PAGE is the original (i.e. highest) pagination page.
Conditionally called from the end of `fj-item-view-more-cb'."
  (cl-destructuring-bind (&key repo owner number _reload _type page limit)
      (fj--get-buffer-spec :viewargs)
    ;; if no pagination, do nothing:
    (when (fj-string-number> (or end-page ;; repeat calls
                                 page) ;; first call
                             "2"
                             #'>=)
      ;; we have pagination to restore, starting from 1:
      (fj-destructure-buf-spec (viewargs)
        ;; update spec:
        (let ((new-page (if end-page page ;; repeat calls
                          "1"))) ;; first call
          (setq fj-buffer-spec
                (plist-put fj-buffer-spec :viewargs
                           (plist-put viewargs :page new-page)))
          (fj-issue-get-timeline-async
           repo owner number new-page limit
           #'fj-item-view-more-cb (current-buffer) (point) nil page))))))

(defun fj-item-view-more ()
  "Load more items to the timeline, if it has more items.
A view is considered to have more items if we can find a load-more
button in it, which has the text property type of more."
  (interactive)
  ;; when we are not clicking on a button but calling the cmd:
  (let ((inhibit-read-only t)
        match)
    (save-excursion
      (if (setq match
                (or (text-property-search-forward 'type 'more t)
                    ;; if we didn't find by searching forward, maybe we
                    ;; are just after the button (eob):
                    (text-property-search-backward 'type 'more t)))
          (progn ;; remove load more button:
            (delete-region (prop-match-beginning match)
                           (prop-match-end match))
            ;; load more async (if we found a "load more" btn):
            (fj-item-view-more*))
        (user-error "No more items to load")))))

(defun fj-item-view-more* (&optional init-page)
  "Append more timeline items to the current view, asynchronously.
Interactively, INIT-PAGE is nil.
INIT-PAGE is used in `fj-render-item' on first load of a view and when
reloading a paginated view."
  (cl-destructuring-bind (&key repo owner number _reload _type page limit)
      (fj--get-buffer-spec :viewargs)
    (fj-issue-get-timeline-async
     repo owner number (or init-page (fj-inc-or-2 page)) limit
     #'fj-item-view-more-cb (current-buffer) (point-max) init-page)))

(defun fj-item-view-more-cb (json buf point &optional init-page end-page)
  "Callback function to append more tiemline items to current view.
JSON is the parsed HTTP response, BUF is the buffer to add to, POINT is
where it was prior to updating.
If INIT-PAGE, do not update :page in viewargs."
  (with-current-buffer buf
    (save-excursion
      (goto-char point)
      (cond ((and (not json)
                  (called-interactively-p 'any))
             (user-error "No more items"))
            ((equal 'errors (caar json))
             (user-error "I am Error: %s - %s"
                         (alist-get 'message json) json))
            (t
             (fj-destructure-buf-spec (viewargs)
               ;; unless init-page arg, increment page in viewargs
               (let* ((page (plist-get viewargs :page))
                      (args (if init-page
                                viewargs
                              (plist-put viewargs :page (fj-inc-or-2 page)))))
                 (setq fj-buffer-spec
                       (plist-put fj-buffer-spec :viewargs args))
                 (message "Loading comments...")
                 (let ((inhibit-read-only t))
                   ;; FIXME: we need .user.username owner args for new elements:
                   (fj-render-timeline json))
                 (message "Loading comments... Done")
                 (when end-page ;; if we are re-paginating, go again maybe:
                   (fj-reload-paginated-pages-maybe end-page page))
                 ;; shr-render-region and regex props:
                 (fj-render-item-bodies)
                 ;; maybe add a "more" link:
                 (fj-issue-timeline-more-link-mayb))))))))

(defun fj-reload-paginated-pages-maybe (end-page page)
  "Call `fj-reload-paginated-pages' maybe.
Do so if END-PAGE is non-nil and larger than PAGE."
  (when (and end-page (fj-string-number> end-page page))
    (fj-reload-paginated-pages end-page)))

(defun fj-string-number> (str1 str2 &optional op)
  "Call `>' on STR1 and STR2.
Alternatively, call OP on them instead."
  (funcall (or op #'>)
           (string-to-number str1)
           (string-to-number str2)))

;; (defun fj-item-view-comment ()
;;   "Comment on the issue currently being viewed."
;;   (interactive)
;;   (fj-with-issue
;;    (let ((number (fj--get-buffer-spec :item))
;;          (owner (fj--get-buffer-spec :owner))
;;          (repo (fj--get-buffer-spec :repo)))
;;      (fj-issue-comment repo number))))

;;; ITEM VIEW ACTIONS

;; TODO: merge simple action functions
(defun fj-item-view-close (&optional state)
  "Close item being viewed, or set to STATE."
  (interactive)
  (fj-with-item-view
   (fj-destructure-buf-spec (item owner repo)
     (fj-issue-close repo owner item state)
     (fj-view-reload))))

(defun fj-item-view-reopen ()
  "Reopen item being viewed."
  (interactive)
  (fj-item-view-close "open"))

(defun fj-item-view-edit-item-at-point ()
  "Edit issue or comment at point in item view mode."
  (interactive)
  (if (fj--property 'fj-comment)
      (fj-item-view-edit-comment)
    (fj-item-view-edit)))

(defun fj-item-view-edit ()
  "Edit the item currently being viewed."
  (interactive)
  (fj-with-own-issue
   (fj-destructure-buf-spec (item repo owner title body)
     (fj-issue-compose :edit nil 'issue body)
     (setq fj-compose-issue-title title
           fj-compose-repo repo
           fj-compose-repo-owner owner
           fj-compose-issue-number item)
     (fedi-post--update-status-fields))))

(defun fj-issue-view-edit-title ()
  "Edit the title of the item being viewed."
  (interactive)
  (fj-with-own-issue-or-repo
   (fj-destructure-buf-spec (repo owner item)
     (fj-issue-edit-title repo owner item)
     (fj-view-reload))))

(defun fj-item-view-comment ()
  "Comment on the item currently being viewed."
  (interactive)
  (fj-destructure-buf-spec (item repo owner title)
    (fj-issue-compose nil 'fj-compose-comment-mode 'comment)
    (setq fj-compose-repo repo
          fj-compose-issue-title title
          fj-compose-repo-owner owner
          fj-compose-issue-number item)
    (fedi-post--update-status-fields)))

(defun fj-item-view-edit-comment ()
  "Edit the comment at point."
  (interactive)
  (fj-with-own-comment
   (fj-destructure-buf-spec (repo owner)
     (let ((id (fj--property 'fj-comment-id))
           (body (alist-get 'body
                            (fj--property 'fj-comment))))
       (fj-issue-compose :edit 'fj-compose-comment-mode 'comment body)
       (setq fj-compose-repo repo
             fj-compose-repo-owner owner
             fj-compose-issue-number id)
       (fedi-post--update-status-fields)))))

(defun fj-item-view-comment-delete ()
  "Delete comment at point."
  (interactive)
  (fj-with-own-comment
   (fj-destructure-buf-spec (repo owner)
     (let* ((id (fj--property 'fj-comment-id))
            (endpoint (format "repos/%s/%s/issues/comments/%s" owner repo id)))
       (when (yes-or-no-p "Delete comment?")
         (fj-delete endpoint)
         (fj-view-reload))))))

;;; PR VIEWS

(defun fj-view-commit-diff (&optional sha)
  "View a diff of a commit at point.
Optionally, provide the commit's SHA."
  (interactive)
  (fj-destructure-buf-spec (repo owner item)
    (let* ((sha (or sha
                    (fj--property 'item) ;; commit at point
                    item)) ;; item view? FIXME: remove?
           (endpoint (format "repos/%s/%s/git/commits/%s.diff"
                             owner repo sha)))
      (fj-view-item-diff endpoint))))

(defun fj-view-pull-diff ()
  "View a diff of the entire current PR."
  (interactive)
  (fj-with-pull
   (fj-destructure-buf-spec (repo owner item)
     (let* ((endpoint (format "repos/%s/%s/pulls/%s.diff"
                              owner repo item)))
       (fj-view-item-diff endpoint)))))

(defun fj-view-item-diff (endpoint)
  "View a diff of an item, commit or pull diff.
ENDPOINT is the API endpoint to hit."
  (let* ((resp (fj-get endpoint nil :no-json))
         (buf "*fj-diff*"))
    (fedi-http--triage
     resp
     (lambda (resp)
       (when (get-buffer buf)
         (kill-buffer buf))
       (with-current-buffer (get-buffer-create buf)
         (erase-buffer)
         (insert (fj-resp-str resp))
         (setq buffer-read-only t)
         (goto-char (point-min))
         (switch-to-buffer-other-window (current-buffer))
         ;; FIXME: make this work like special-mode, easy bindings and
         ;; read-only:
         (diff-mode))))))

(defun fj-get-pull-commits ()
  "Return the data for the commits of the current pull."
  (interactive)
  (fj-with-pull
   (fj-destructure-buf-spec (repo owner item)
     (let* ((endpoint (format "/repos/%s/%s/pulls/%s/commits"
                              owner repo item)))
       (fj-get endpoint)))))

(defun fj-merge-pull ()
  "Merge pull request of current view or at point."
  (interactive)
  (fj-with-pull
   (fj-destructure-buf-spec (repo owner item)
     (let* ((data (save-excursion
                    (goto-char (point-min))
                    (fedi--property 'fj-item-data)))
            (number (if (eq major-mode 'fj-issue-tl-mode)
                        (let* ((entry (tabulated-list-get-entry)))
                          (car (seq-first entry)))
                      item))
            ;; FIXME: branch is not provided in the tl data :/
            (branch (unless (eq major-mode 'fj-issue-tl-mode)
                      (map-nested-elt data '(base label))))
            (branch-str (if branch (concat " " branch) ""))
            ;; FIXME: If not mergeable, then must do manual merge. but if
            ;; manual/local merge gets done and pushed, does foregejo
            ;; update the status of mergeable? if so we can user-error on
            ;; not merbeable, else we have to y-or-n-p only, but also only
            ;; allow merge-type to be manually-merged
            (mergeable (not (eq :json-false (alist-get 'mergeable data))))
            (merge-type (completing-read "Merge type: " fj-merge-types))
            (merge-commit (when (equal merge-type "manually-merged")
                            (read-string "Merge commit: "))))
       (if (not mergeable)
           (user-error "PR not mergeable")
         (when (y-or-n-p
                (format "Merge PR #%s into %s/%s%s?" number owner repo branch-str))
           (let ((resp (fj-pull-merge-post repo owner number
                                           merge-type merge-commit)))
             (fedi-http--triage resp
                                (lambda (_)
                                  (fj-view-reload)
                                  (message "Merged!"))))))))))

(defun fj-fetch-pull-as-branch ()
  "From a PR view, fetch it as a new git branch using magit."
  ;; WIP, needs testing on a fresh PR!
  (interactive)
  (fj-with-pull
   (let* ((pull (fedi--property 'fj-item-number))
          (data (fedi--property 'fj-item-data))
          ;; remote in this case is the PR base, what we will merge into
          ;; (not the PR creator's fork):
          (remote (map-nested-elt data '(base repo html_url)))
          (head (map-nested-elt data '(head repo full_name)))
          ;; fetch the PR head's branch as default branch name:
          (branch (read-string "Pull branch name: "
                               (map-nested-elt data '(head label))))
          (refspec (format "refs/pull/%s/head:%s" pull branch)))
     (when (y-or-n-p
            (format "Fetch %s from %s as new branch?" branch head))
       ;; mayb we want to check out PR, and magit-status or sth?:
       ;; FIXME: assumes we are in repo:
       (magit-fetch-refspec remote refspec nil)))))

;;; TIMELINE ITEMS

(defvar fj-issue-timeline-action-str-alist
  ;; issues:
  '(("close" . "%s closed this issue %s")
    ("reopen" . "%s reopened this issue %s")
    ("comment_ref" . "%s referenced this issue %s")
    ("change_title" . "%s changed title from \"%s\" to \"%s\" %s")
    ("commit_ref" . "%s referenced this item from a commit %s")
    ("issue_ref" . "%s referenced this issue from %s %s")
    ("label" . "%s %s the %s label %s")
    ;; PRs:
    ("pull_push" . "%s %s %d commits %s")
    ("merge_pull" . "%s merged this pull request %s")
    ("pull_ref" . "%s referenced a PR that will close this %s")
    ("delete_branch" . "%s deleted branch %s %s")
    ("review" . "%s %s %s")
    ;; FIXME: add a request for changes review? not just approval?
    ("review_request" . "%s requested review from %s %s")
    ("milestone" . "%s added milestone %s %s")
    ("assignees" . "%s %sassigned this%s %s")
    ("change_target_branch" . "%s changed target branch from %s to %s")))

(defun fj-render-timeline (data &optional author owner)
  "Render timeline DATA.
DATA contains all types of issue comments (references, name
changes, commit references, etc.).
AUTHOR is timeline item's author, OWNER is of item's repo."
  (cl-loop for i in data
           ;; prevent a `nil' item (seen in the wild) from breaking our
           ;; timeline:
           when i
           do (fj-render-timeline-item i author owner)))

(defun fj-render-timeline-item (item &optional author owner)
  "Render timeline ITEM.
AUTHOR is timeline item's author, OWNER is of item's repo."
  (let-alist item
    (let ((format-str
           (cdr (assoc .type fj-issue-timeline-action-str-alist)))
          (ts (fedi--relative-time-description
               (date-to-time .updated_at)))
          (user (propertize .user.username
                            'face 'fj-name-face))
          (assignee (if .assignee_team
                        (or .assignee_team.name "")
                      (or .assignee.username
                          .assignee.login
                          "")))
          (body (mm-url-decode-entities-string .body)))
      (insert
       (propertize
        (pcase .type
          ("comment" (fj-format-comment item author owner))
          ("close" (format format-str user ts))
          ("reopen" (format format-str user ts))
          ("change_title"
           (format format-str user
                   (propertize .old_title
                               'face '(:strike-through t))
                   (fj-format-tl-title .new_title nil 'fj-name-face
                                       'highlight)
                   ts))
          ("comment_ref"
           (let ((number (number-to-string
                          .ref_issue.number)))
             (concat
              (format format-str user ts)
              "\n"
              (fj-propertize-link
               (fj-format-tl-title
                (concat .ref_issue.title " #" number))
               'comment-ref number nil :noface))))
          ("commit_ref"
           (concat
            (format format-str user ts)
            "\n"
            (fj-propertize-link
             (fj-format-tl-title
              (url-unhex-string (fj-get-html-link-desc body)))
             'commit-ref .ref_commit_sha nil :noface)))
          ("issue_ref"
           (concat
            (format format-str user .ref_issue.repository.full_name ts)
            "\n"
            (fj-propertize-link
             (fj-format-tl-title
              (url-unhex-string .ref_issue.title))
             'issue-ref .ref_issue.number nil :noface)))
          ("label"
           (let ((action (if (string= body "1") "added" "removed")))
             (format format-str user action .label.name ts)))
          ;; PRs:
          ;; FIXME: reimplement "pull_push" force-push and non-force
          ;; format strings
          ("pull_push"
           (let* ((json-array-type 'list)
                  (json (json-read-from-string body))
                  (commits (alist-get 'commit_ids json))
                  (force
                   (not
                    (eq :json-false
                        (alist-get 'is_force_push json)))))
             (concat
              (format format-str user (if force "force pushed" "added")
                      (if force (1- (length commits))
                        (length commits))
                      ts)
              ;; FIXME: fix force format string:
              ;; (format "%s force-pushed %s from %s to %s %s"
              ;; user branch c1 c2 ts)
              (if force
                  (concat ": from "
                          (fj-propertize-link
                           (substring (car commits) 0 7)
                           'commit-ref (car commits))
                          " to "
                          (fj-propertize-link
                           (substring (cadr commits) 0 7)
                           'commit-ref (cadr commits)))
                (cl-loop
                 for c in commits
                 for short = (substring c 0 7)
                 concat
                 (concat " "
                         (fj-propertize-link short
                                             'commit-ref c)))))))
          ("merge_pull"
           ;; FIXME: get commit and branch for merge:
           ;; Commit is the *merge* commit, created by actually merging
           ;; the proposed commits
           ;; branch etc. details should be given at top, diff details to
           ;; plain issue
           (format format-str user ts))
          ("pull_ref"
           (concat
            (format format-str user ts)
            "\n"
            (fj-propertize-link .ref_issue.title 'comment-ref .ref_issue.number)))
          ("delete_branch"
           (format format-str user
                   (propertize .old_ref
                               'face 'fj-name-face)
                   ts))
          ;; reviews
          ("review"
           (fj-format-review .review_id ts format-str user))
          ("review_request"
           (fj-format-assignee format-str user assignee ts))
          ;; milestones:
          ("milestone"
           (format format-str user
                   (propertize .milestone.title
                               'face 'fj-name-face)
                   ts))
          ("assignees"
           (fj-format-assignee format-str .user.username assignee ts))
          ("change_target_branch"
           (fj-format-change-target format-str .user.username
                                    .old_ref .new_ref))
          (_ ;; just so we never break the rest of the view:
           (format "%s did unknown action: %s %s" user .type ts)))
        'fj-item-data item)
       "\n\n"))))

(defun fj-format-change-target (format-str user old new)
  "Format a change-target-branch timeline item.
FORMAT-STR is the base string. USER is the agent, OLD is old branch, NEW
is new branch."
  (format format-str
          (propertize user 'face 'fj-name-face)
          (propertize old 'face 'fj-name-face)
          (propertize new 'face 'fj-name-face)))

(defun fj-format-assignee (format-str user assignee ts)
  "Format an assignee timeline item.
FORMAT-STR is the base string. USER is the agent, ASSIGNEE is the user
assigned to. TS is a timeline timestamp."
  (let ((user (propertize user 'face 'fj-name-face))
        (assignee (propertize assignee 'face 'fj-name-face)))
    ;; "%s %sassigned this%s %s":
    (if (string= user assignee)
        (format format-str user "self-" "" ts)
      (format format-str user "" (concat "to " assignee) ts))))

(defun fj-get-html-link-desc (str)
  "Return a description string from HTML link STR."
  (save-match-data
    (string-match "<a[^\n]*>\\(?2:[^\n]*\\)</a>" str)
    (match-string 2 str)))

(defun fj-propertize-link (str &optional type item face no-face)
  "Propertize a link with text STR.
Optionally set link TYPE and ITEM number and FACE."
  ;; TODO: poss to refactor with `fedi-link-props'?
  ;; make plain links work:
  (when (eq type 'shr)
    (setq str (propertize str 'shr-url str)))
  (if no-face
      (propertize str
                  'mouse-face 'highlight
                  'fj-tab-stop t
                  'keymap fj-link-keymap
                  'button t
                  'type type
                  'item item
                  'fj-tab-stop t
                  'category 'shr
                  'follow-link t)
    (propertize str
                'face (or face 'shr-link)
                'mouse-face 'highlight
                'fj-tab-stop t
                'keymap fj-link-keymap
                'button t
                'type type
                'item item
                'fj-tab-stop t
                'category 'shr
                'follow-link t)))

;;; REVIEWS (PRS)

(defun fj-format-review (review-id ts format-str user)
  "Render code review with REVIEW-ID.
TS, FORMAT-STR and USER are from `fj-render-timeline-item', which see.
Renders a review heading and review comments."
  (fj-destructure-buf-spec (repo owner item)
    (let ((review (fj-get-review repo owner
                                 item review-id))
          (comments (fj-get-review-comments repo owner
                                            item review-id)))
      (let-alist review
        (let ((state (pcase .state
                       ("APPROVED"
                        (concat (downcase .state) " these changes"))
                       ("REQUEST_CHANGES"
                        "requested changes")
                       (_ "reviewed"))))
          (propertize
           (concat
            (format format-str user state ts)
            (fj-format-grouped-review-comments comments owner ts))
           'fj-review review))))))

(defun fj-format-grouped-review-comments (comments owner ts)
  "Build an alist where each cons is a diff hunk and its comments.
Then format the hunk followed by its comments. COMMENTS is the comments
data, OWNER is the repo owner, and TS is a timestamp."
  (let* ((hunks (cl-remove-duplicates
                 (cl-loop for c in comments
                          collect (alist-get 'diff_hunk c))
                 :test #'string=))
         (alist
          (cl-loop for x in hunks
                   collect
                   (cons x
                         (cl-loop for c in comments
                                  when (equal x (alist-get 'diff_hunk c))
                                  collect c)))))
    (cl-loop for x in alist
             concat (fj-format-diff-+-comments x nil owner ts))))

(defun fj-format-diff-+-comments (data author owner ts)
  "Format a diff hunk followed by its comments.
DATA is a cons from `fj-format-grouped-review-comments'.
AUTHOR, OWNER, and TS are for header formatting."
  ;; (diff-minor-mode 1)
  (concat
   "\n" fedi-horiz-bar "\n"
   (fj-format-review-diff (car data)) ;; diff hunk
   "\n"
   (cl-loop for c in (cdr data)
            concat (fj-format-review-comment c author owner ts))))

(defun fj-format-review-diff (diff)
  "Return a formatted diff hunk for DIFF."
  ;; FIXME: propertize this diff somehow
  ;; (diff-mode uses overlays we can can't copy them)
  ;; magit (wash/paint hunk) replies on its own classes, and wants a file
  ;; ansi color apply wants color codes
  (with-temp-buffer
    (insert diff)
    (goto-char (point-min))
    ;; (magit-diff-wash-hunk)
    (buffer-string)))

(defun fj-format-review-comment (comment author owner ts)
  "Format a review COMMENT.
AUTHOR of item, OWNER of repo, TS is a timestamp."
  (let-alist comment
    (propertize
     (concat
      "\n"
      (fj-format-comment-header
       .user.login author
       owner "" ;; FIXME: (fj-edited-str-maybe .created_at .updated_at)
       ts)
      "\n"
      (fj-render-body .body))
     'fj-review-comment comment
     'line-prefix "  "))) ;; indent

(defun fj-get-review (repo owner item-id review-id)
  "Get review data for REVIEW-ID.
In ITEM-ID in REPO by OWNER."
  ;; /repos/{owner}/{repo}/pulls/{index}/reviews/{id}
  (let* ((endpoint (format "repos/%s/%s/pulls/%s/reviews/%s"
                           owner repo item-id review-id)))
    (fj-get endpoint)))

(defun fj-get-review-comments (repo owner item-id review-id)
  "Get review comments.
Use REVIEW-ID for ITEM-ID in REPO by OWNER."
  ;; /repos/{owner}/{repo}/pulls/{index}/reviews/{id}/comments
  (let ((endpoint (format "repos/%s/%s/pulls/%s/reviews/%s/comments"
                          owner repo item-id review-id)))
    (fj-get endpoint)))

;;; SEARCH

(defvar fj-search-modes
  '("source" "fork" "mirror" "collaborative")
  "Types of repositories in foregejo search.")

(defvar fj-search-sorts
  '("alpha" "created" "updated" "size" "git_size" "lfs_size" "stars"
    "forks" "id"))

(defun fj-repo-search-do (query &optional topic id mode
                                include-desc sort order page limit)
  "Search repos for QUERY.
Optionally flag it as a TOPIC.
ID is a user ID, which if given must own the repo.
MODE must be a member of `fj-search-modes', else it is silently
ignored.
INCLUDE-DESC SORT ORDER PAGE LIMIT
Sort must be a member of `fj-search-sorts'."
  ;; GET /repos/search. args TODO:
  ;; priority_owner_id, team_id, starredby
  ;; private, is_private, template, archived
  (let* ((params
          (append
           `(("limit" . ,(fj-default-limit))
             ("sort" . ,(or sort "updated")))
           (when id `(("exclusive" . "true")))
           (fedi-opt-params (query :alias "q") (topic :boolean "true")
                            (id :alias "uid")
                            (mode :when (member mode fj-search-modes))
                            (include-desc :alias "includeDesc"
                                          :boolean "true")
                            order page limit))))
    (fj-get "/repos/search" params)))


(defun fj-repo-search (query &optional topic id mode)
  "Search repos for QUERY.
If TOPIC, QUERY is a search for topic keywords.
ID is a user ID, which if given must own the repo.
MODE must be a member of `fj-search-modes', else it is silently
ignored."
  (interactive "sSearch for repo: ")
  (let* ((resp (fj-repo-search-do query topic id mode))
         (data (alist-get 'data resp))
         (cands (fj-get-repo-candidates data))
         (completion-extra-properties
          '(:annotation-function fj-repo-candidates-annot-fun))
         (choice (completing-read "Repo: " cands))
         (user (cl-fourth
                (assoc choice cands #'equal))))
    (fj-list-issues-do choice user)))

(defun fj-repo-candidates-annot-fun (cand)
  "Annocation function for `fj-repo-search'.
Returns annotation for CAND, a candidate."
  (if-let* ((entry (assoc cand minibuffer-completion-table
                          #'equal)))
      (concat "\t\t" (cl-fourth entry))
    ""))

;;; SEARCH REPOS TL

(defvar-keymap fj-repo-tl-mode-map
  :doc   "Map for `fj-repo-tl-mode', a tabluated list of repos."
  :parent fj-repo-tl-map)

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
          ("★" 3 fj-tl-sort-by-stars :right-align t)
          ("" 2 t)
          ("issues" 5 fj-tl-sort-by-issue-count :right-align t)
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

(define-button-type 'fj-repo-stargazers-button
  'follow-link t
  'action 'fj-repo-tl-stargazers
  'help-echo "RET: View stargazers.")

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
         [(,.name face fj-item-face
                  id ,.id
                  type fj-user-repo-button
                  item repo
                  fj-url ,.html_url
                  fj-item-data ,r
                  fj-tab-stop t)
          ,@(unless no-owner
              `((,.owner.username face fj-user-face
                                  id ,.id
                                  type fj-search-owner-button
                                  item repo
                                  fj-tab-stop t)))
          (,(number-to-string .stars_count)
           id ,.id face fj-figures-face
           type fj-repo-stargazers-button
           item repo)
          (,fork id ,.id face fj-figures-face item repo)
          (,(number-to-string .open_issues_count)
           id ,.id face fj-figures-face
           item repo)
          ,.language
          (,updated-str
           display ,updated-display
           face default
           item repo)
          (,(string-replace "\n" " " .description)
           face 'fj-comment-face
           item repo)])))))

(defun fj-repo-search-tl (query &optional topic id mode
                                include-desc sort order page limit)
  "Search repos for QUERY, and display a tabulated list of results.
TOPIC, a boolean, means search in repo topics.
ID MODE INCLUDE-DESC SORT ORDER PAGE LIMIT."
  (interactive "sSearch for repos: ")
  (let* ((resp (fj-repo-search-do query topic id mode
                                  include-desc sort order page limit))
         (buf (format "*fj-search-%s*" query))
         (url (concat fj-host "/explore/repos"))
         (data (alist-get 'data resp))
         (entries (fj-repo-tl-entries data)))
    (fj-repos-tl-render buf entries #'fj-repo-tl-mode)
    (with-current-buffer (get-buffer-create buf)
      (setq fj-buffer-spec
            `( :url ,url
               :viewargs
               ( :query ,query :topic ,topic :id ,id :mode ,mode
                 :include-desc ,include-desc :sort ,sort
                 :order ,order :page ,page :limit ,limit)
               :viewfun fj-repo-search-tl)))))

(defun fj-repo-search-tl-topic (query)
  "Search repo topics for QUERY, and display a tabulated list."
  (interactive "sSearch for topic in repos: ")
  (fj-repo-search-tl query 'topic))

(defun fj-repos-tl-render (buf entries mode &optional sort-key)
  "Render a tabulated list in BUF fer, with ENTRIES, in MODE.
Optionally specify repo OWNER and URL.
Set `tabulated-list-sort-key' to SORT-KEY. It may optionally be :nil to
unset any default values."
  (let ((prev-buf (buffer-name (current-buffer))))
    (with-current-buffer (get-buffer-create buf)
      (setq tabulated-list-entries entries)
      (funcall mode)
      ;; some modes set a sort-key, but we also may want to selectively
      ;; unset it. but if we don't have sort-key, we also don't want to
      ;; nil the mode setting:
      (when sort-key
        (if (eq :nil sort-key)
            (setq tabulated-list-sort-key nil)
          (setq tabulated-list-sort-key sort-key)))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (fj-other-window-maybe prev-buf "*fj-search" #'string-prefix-p))))

;; (cond ((or (string= buf prev-buf) ;; reloading
;;            (string-prefix-p "*fj-search" buf)) ;; any search
;;        ;; (string-suffix-p "-issues*" prev-buf) ; diff repo
;;        (switch-to-buffer (current-buffer)))
;;       (t ;; new buf
;;        (switch-to-buffer-other-window (current-buffer)))))))

;;; TL ACTIONS

;; in repo's issues TL, or for repo entry at point:
(defun fj-create-issue (&optional _)
  "Create issue in current repo or repo at point in tabulated listing."
  ;; for this to work simply from eg a code file not an fj.el buffer,
  ;; we need `fj--repo-owner' to work.
  
  ;; we cd fall back to `fj-user' but that's assuming we are creating an
  ;; issue in a repo that's ours, not that we are contributing to.
  ;; otherwise, maybe we cd prompt for "owner/repo" format rather than
  ;; just repo name, allowing user to choose from their own repos but also
  ;; forks, upstreams?

  ;; maybe we allow the fallback to `fj-user' but also allow interactive
  ;; modifying of it like repo name? cd format like "owner/repo" in
  ;; compose buffer.
  (interactive)
  (let* ((owner (fj--repo-owner))
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
     (fj-list-issues-do name owner))))

(defun fj-repo-tl-list-pulls (&optional _)
  "View issues of current repo from tabulated repos listing."
  (interactive)
  (fj-with-repo-entry
   (let* ((entry (tabulated-list-get-entry))
          (name (car (seq-first entry)))
          (owner (fj--repo-owner)))
     (fj-list-pulls name owner))))

;; author/owner button, in search or issues TL, not user repo TL
(defun fj-list-user-repos (&optional _)
  "View repos of current entry user from tabulated repos listing."
  (interactive)
  (if (eq major-mode #'fj-user-repo-tl-mode)
      (user-error "Already viewing user repos")
    (fj-with-entry
     (let* ((owner (if (eq major-mode #'fj-issue-tl-mode)
                       (fj--get-tl-col 2) ;; ISSUE author not REPO owner
                     (fj--repo-owner))))
       (fj-user-repos-tl owner)))))

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
      (fj-destructure-buf-spec (repo owner)
        (let* ((resp (fj-get-repo repo owner))
               (url (alist-get 'clone_url resp)))
          (kill-new url)
          (message (format "Copied: %s" url))))
    (fj-with-repo-entry
     (let* ((entry (tabulated-list-get-entry))
            (repo (car (seq-first entry)))
            (owner (fj--repo-owner))
            (resp (fj-get-repo repo owner))
            (url (alist-get 'clone_url resp)))
       (kill-new url)
       (message (format "Copied: %s" url))))))

(defun fj-copy-item-url ()
  "Copy URL of current item, either issue or PR."
  (interactive)
  (let ((url
         ;; issues tl view:
         (or (fj--property 'fj-url)
             ;; issue view:
             (alist-get 'html_url
                        (fj--property 'fj-item-data)))))
    (kill-new url)
    (message (format "Copied: %s" url))))

(defun fj-copy-pr-url ()
  "Copy upstream Pull Request URL with branch name."
  (interactive)
  (let* ((owner (fj--get-buffer-spec :owner))
         (repo (fj--get-buffer-spec :repo))
         (number (fj--get-buffer-spec :item))
         (author (fj--get-buffer-spec :author))

         (endpoint (format "repos/%s/%s/pulls/%s" owner repo number))
         (pr (fj-get endpoint))
         (data (alist-get 'head pr))
         (branch (alist-get 'ref data))
         (author+repo (alist-get 'full_name
                                 (alist-get 'repo data)))

         (str (concat fj-host "/" author+repo
                      "  "
                      (format "%s:pr-%s-%s-%s"
                              branch
                              number
                              author
                              branch))))
    (kill-new str)
    (message (format "Copied: %s" str))))

(defun fj-fork-to-parent ()
  "From a repo TL listing, jump to the parent repo."
  ;; FIXME: this works ok, but perhaps we would rather jump not to the
  ;; issues listing, but to another TL listing of the parent? to see
  ;; owner, stats, etc.
  (interactive)
  (let* ((data (fj--property 'fj-item-data))
         (forkp (eq t (alist-get 'fork data)))
         (parent (if forkp (alist-get 'parent data)))
         (repo (alist-get 'name parent))
         (owner (map-nested-elt parent '(owner username))))
    (if (not forkp)
        (user-error "No fork at point?")
      (fj-list-items repo owner nil "issues")
      (message "Jumped to parent repo %s"
               (alist-get 'full_name parent)))))

;; TODO: star toggle

(defun fj-get-repo-files (repo owner)
  "Get files for REPO of OWNER.
REF is a commit, branch or tag."
  (let ((endpoint (format"repos/%s/%s/contents" owner repo)))
    (fj-get endpoint)))

(defun fj-get-repo-file (repo owner file)
  "Return FILE from REPO of OWNER.
FILE is a string, including type suffix, and is case-sensitive."
  (let* ((endpoint (format "repos/%s/%s/raw/%s" owner repo file))
         (resp (fj-get endpoint nil :no-json)))
    (fedi-http--triage resp
                       (lambda (resp)
                         (fj-resp-str resp)))))

(defun fj-repo-readme (&optional repo owner)
  "Display readme file of REPO by OWNER.
Optionally specify REF, a commit, branch, or tag."
  (let* ((files (fj-get-repo-files repo owner))
         (names (cl-loop for f in files
                         collect (alist-get 'name f)))
         (readme-name
          (car (or (cl-member "readme" names :test #'string-prefix-p)
                   (cl-member "README" names :test #'string-prefix-p))))
         (suffix (file-name-extension readme-name))
         (file-str  (fj-get-repo-file repo owner readme-name))
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
      (keymap-local-set "q" #'quit-window)
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
   (let* ((number (fj--get-tl-col 0))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--repo-col-or-buf-spec))
          (item (fj--property 'item)))
     (fj-item-view repo owner number
                   (when (eq item 'pull) :pull)))))

(defun fj-issues-tl-edit ()
  "Edit issue from tabulated issues listing."
  (interactive)
  (fj-with-own-entry
   (let* ((number (fj--get-tl-col 0))
          (owner (fj--get-buffer-spec :owner))
          (title (substring-no-properties
                  (fj--get-tl-col 4)))
          (repo (fj--repo-col-or-buf-spec))
          (data (fj-get-item repo owner number))
          (old-body (alist-get 'body data)))
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
   (let* ((number (fj--get-tl-col 0))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--repo-col-or-buf-spec))
          (title (fj--get-tl-col 4)))
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
             (owner (fj--get-buffer-spec :owner))
             (repo (fj--repo-col-or-buf-spec)))
        (fj-issue-close repo owner number)
        (fj-view-reload))))))

(defun fj-issues-tl-delete (&optional _)
  "Delete current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (fj-with-own-repo
    (let* ((entry (tabulated-list-get-entry))
           (number (car (seq-first entry)))
           (owner (fj--get-buffer-spec :owner))
           (repo (fj--repo-col-or-buf-spec)))
      (when (y-or-n-p (format "Delete issue %s?" number))
        (fj-issue-delete repo owner number :no-confirm)
        (fj-view-reload))))))

(defun fj-issues-tl-reopen (&optional _)
  "Reopen current issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (if (string= (fj--property 'state) "open")
       (user-error "Issue already open")
     (let* ((entry (tabulated-list-get-entry))
            (number (car (seq-first entry)))
            (owner (fj--get-buffer-spec :owner))
            (repo (fj--repo-col-or-buf-spec)))
       (fj-issue-close repo owner number "open")
       (fj-view-reload)))))

(defun fj-issues-tl-edit-title ()
  "Edit issue title from issues tabulated list view."
  (interactive)
  (fj-with-own-issue-or-repo
   (let* ((entry (tabulated-list-get-entry))
          (repo (fj--repo-col-or-buf-spec))
          (owner (fj--get-buffer-spec :owner))
          (number (car (seq-first entry))))
     (fj-issue-edit-title repo owner number)
     (fj-view-reload))))

(defun fj-issues-tl-label-add ()
  "Add label to issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (let* ((number (fj--get-tl-col 0))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--repo-col-or-buf-spec)))
     (fj-issue-label-add repo owner number))))

(defun fj-issues-tl-label-remove ()
  "Remove label from issue from tabulated issues listing."
  (interactive)
  (fj-with-entry
   (let* ((number (fj--get-tl-col 0))
          (owner (fj--get-buffer-spec :owner))
          (repo (fj--repo-col-or-buf-spec)))
     (fj-issue-label-remove repo owner number))))

;;; COMPOSING

(defalias 'fj-compose-cancel #'fedi-post-cancel)

(defun fj-match-next-issue (limit)
  "A font-lock match function for issue references.
LIMIT is for `re-search-forward''s bound argument."
  (re-search-forward "#[[:digit:]]+" limit :no-error))

(defun fj-add-font-lock-keywords ()
  "Add a font-lock keyword to highlight #123 as issue ref."
  (font-lock-add-keywords
   nil ;; = current buffer
   '((fj-match-next-issue 0 ; = limit (actually match number?!)
                          'fj-item-face)
     (fj-match-next-handle 0 'fj-user-face))))

(defun fj-match-next-handle (limit)
  "A font-lock match function for handles.
LIMIT is for `re-search-forward''s bound argument."
  (re-search-forward "@[[:alnum:]_-]+" limit :no-error))

(defvar-keymap fj-compose-comment-mode-map
  :doc "Keymap for `fj-compose-comment-mode'."
  "C-c C-k" #'fj-compose-cancel
  "C-c C-c" #'fj-compose-send)

(define-minor-mode fj-compose-comment-mode
  "Minor mode for composing comments."
  :keymap fj-compose-comment-mode-map
  :global nil
  (fj-add-font-lock-keywords))

(defvar-keymap fj-compose-mode-map
  :doc "Keymap for `fj-compose-mode'."
  :parent fj-compose-comment-mode-map
  "C-c C-t" #'fj-compose-read-title
  "C-c C-r" #'fj-compose-read-repo
  "C-c C-l" #'fj-compose-read-labels
  "C-c C-m" #'fj-compose-read-milestone
  "C-c C-S-M" #'fj-compose-remove-milestone
  "C-c C-S-L" #'fj-compose-remove-labels)

(define-minor-mode fj-compose-mode
  "Minor mode for composing issues."
  :keymap fj-compose-mode-map
  :global nil
  (fj-add-font-lock-keywords))

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

(defun fj-compose-read-labels ()
  "Read a label in the issue compose buffer."
  (interactive)
  ;; we store conses of (name . id), then fedi.el
  ;; displays nanmes in the compose docs but submits the id.
  (cl-pushnew (fj-issue-read-label fj-compose-repo nil nil :id)
              fj-compose-issue-labels
              :test #'equal)
  (fedi-post--update-status-fields))

(defun fj-compose-read-milestone ()
  "Read an existing milestone in the compose buffer.
Return a cons of title and ID."
  (interactive)
  (let* ((milestones (fj-get-milestones fj-compose-repo
                                        fj-compose-repo-owner))
         (alist (fj-milestones-alist milestones))
         (choice (completing-read "Milestone: " alist)))
    (setq fj-compose-milestone
          (assoc choice alist #'string=))
    (fedi-post--update-status-fields)))

(defun fj-compose-remove-variable (var)
  "Set VAR to nil if it is non-nil.
Update status fields."
  (if (not (symbol-value var))
      (user-error "No milestone to remove")
    (set var nil)
    (fedi-post--update-status-fields)))

(defun fj-compose-remove-labels ()
  "Remove labels from item being composed."
  (interactive)
  (fj-compose-remove-variable 'fj-compose-issue-labels))

(defun fj-compose-remove-milestone ()
  "Remove milestone from item being composed."
  (interactive)
  (fj-compose-remove-variable 'fj-compose-milestone))

(defun fj-issue-compose (&optional edit mode type init-text)
  "Compose a new post.
EDIT means we are editing.
MODE is the fj.el minor mode to enable in the compose buffer.
TYPE is a symbol of what we are composing, it may be issue or comment.
Inject INIT-TEXT into the buffer, for editing."
  (interactive)
  (setq fj-compose-last-buffer (buffer-name (current-buffer)))
  (let ((quote (when (region-active-p)
                 (buffer-substring (region-beginning)
                                   (region-end)))))
    (fedi-post--compose-buffer
     edit
     #'markdown-mode
     (or mode #'fj-compose-mode)
     (when mode "fj-compose")
     (or type 'issue)
     (list #'fj-compose-mentions-capf)
     ;; #'fj-compose-issues-capf
     ;; TODO: why not have a compose-buffer-spec rather than 10 separate vars?
     `(((name . "repo")
        (prop . compose-repo)
        (item-var . fj-compose-repo)
        (face . link))
       ((name . ,(if (eq type 'comment) "issue ""title"))
        (prop . compose-title)
        (item-var . fj-compose-issue-title)
        (face . fj-post-title-face))
       ((name . "labels")
        (prop . compose-labels)
        (item-var . fj-compose-issue-labels)
        (face . fj-post-title-face))
       ((name . "milestone")
        (prop . compose-milestone)
        (item-var . fj-compose-milestone)
        (face . fj-post-title-face)))
     init-text quote
     "fj-")
    (setq fj-compose-item-type
          (if edit
              (if (eq type 'comment)
                  'edit-comment
                'edit-issue)
            (if (eq type 'comment)
                'new-comment
              'new-issue)))))

(defun fj-compose-send (&optional prefix)
  "Submit the issue or comment to your Forgejo instance.
Call response and update functions.
With a prefix argument, also close issue if sending a comment."
  ;; FIXME: handle `fj-compose-repo-owner' being unset?
  ;; if we want to error about it, we also need a way to set it.
  (interactive "P")
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
              (pcase type
                ('new-comment
                 (fj-issue-comment repo
                                   fj-compose-repo-owner
                                   fj-compose-issue-number
                                   body prefix))
                ('edit-comment
                 (fj-issue-comment-edit repo
                                        fj-compose-repo-owner
                                        fj-compose-issue-number
                                        body))
                ('edit-issue
                 (fj-issue-patch repo
                                 fj-compose-repo-owner
                                 fj-compose-issue-number
                                 fj-compose-issue-title
                                 body))
                (_ ; new issue
                 (fj-issue-post repo
                                fj-compose-repo-owner
                                fj-compose-issue-title body
                                fj-compose-issue-labels
                                nil nil nil
                                fj-compose-milestone)))))
        (when response
          (with-current-buffer buf
            (fedi-post-kill))
          (if (not (eq type 'new-issue))
              ;; FIXME: we may have been in issues TL or issue view.
              ;; we we need prev-buffer arg?
              ;; else generic reload function
              (fj-view-reload)
            (fj-list-issues-do repo)))))))

(defun fj-search-users (query &optional limit)
  "Search instance users for QUERY.
Optionally set LIMIT to results."
  ;; FIXME: server: limit is an integer; it doesn't respect our 25, returns 2500
  ;; works:
  ;; (let ((resp (fj-search-users "mart" "10")))
  ;;   (length (alist-get 'data resp)))
  (let ((params `(("q" . ,query)
                  ("limit" . ,limit))))
    (fj-get "users/search" params)))

;; (with-current-buffer (get-buffer-create "*fj-test*")
;;   (erase-buffer)
;;   (insert
;;    (prin1-to-string
;;     (fj-search-users "mart" "10")))
;;   (pp-buffer)
;;   (emacs-lisp-mode)
;;   (switch-to-buffer-other-window (current-buffer)))

(defun fj-users-list (data)
  "Return an list of handles from users' DATA."
  ;; users have no html_url, just concat it to `fj-host'
  (cl-loop for u in data
           ;; for id = (alist-get 'id u)
           for name = (alist-get 'login u)
           collect (concat "@" name))) ;; @ needed to match for display!

(defun fj-compose-handle-exit-fun (str _status)
  "Turn completion STR into a markdown link."
  (save-excursion
    (delete-char (- 0 (length str)))
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
                (fj-max-items)))
         (data (alist-get 'data resp)))
    (fj-users-list data)))

;; (defun fj-compose-mentions-capf ()
;;   (cape-wrap-debug #'fj-compose-mentions-capf*))

(defun fj-compose-mentions-capf ()
  "Build a mentions completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf fedi-post-handle-regex
                          #'fj-compose-mentions-fun
                          nil nil
                          #'fj-compose-handle-exit-fun
                          ;; 'fj-mention
                          ))

;;; issues capf
;; TODO: we need to trigger completion on typing # alone (depends on regex)
;; TODO: we need to fetch all issues or do GET query by number
(defun fj-issues-alist (data)
  "Return an alist from issue DATA, a cons of number and title."
  (cl-loop for i in data
           collect (let-alist i
                     (cons (concat "#"
                                   (number-to-string .number)
                                   .title)))))

;; FIXME: start end not used?
(defun fj-compose-issues-fun (_start _end)
  "Given prefix str between START and END, return an alist of issues for capf."
  (let ((resp (fj-repo-get-issues fj-compose-repo fj-compose-repo-owner
                                  "all")))
    (fj-issues-alist resp)))

;; (defun fj-compose-issues-capf ()
;;   "Build an issues completion backend for `completion-at-point-functions'."
;;   (fedi-post--return-capf fedi-post-tag-regex
;;                           #'fj-compose-issues-fun
;;                           #'fj--issues-annot-fun nil
;;                           #'fj-compose-issue-exit-fun))

;; (defun fj--issues-annot-fun (candidate)
;;   "Given an issues completion CANDIDATE, return its annotation."
;;   (concat " " (cdr (assoc candidate fedi-post-completions #'equal))))

;; (defun fj-compose-issue-exit-fun (str _status)
;;   "Mark completion STR as verbatim."
;;   (save-excursion
;;     (backward-char (length str))
;;     (insert "`"))
;;   (insert "`"))

;;; NOTIFICATIONS

(defvar-keymap fj-notifications-mode-map
  :doc "Keymap for `fj-notifications-mode'."
  :parent fj-generic-map
  "C-c C-c" #'fj-notifications-unread-toggle
  "C-c C-s" #'fj-notifications-subject-cycle
  ;; FIXME: move to `fj-generic-map' when item views are ready:
  "." #'fj-next-page
  "," #'fj-prev-page
  "s" #'fj-list-issues-search)

(define-derived-mode fj-notifications-mode special-mode "fj-notifs"
  "Major mode for viewing notifications."
  :group 'fj
  (read-only-mode 1))

(defvar fj-notifications-status-types
  '("unread" "read" "pinned")
  "List of possible status types for getting notifications.")

(defun fj-get-notifications (&optional all status-types subject-type
                                       page limit before since)
  "GET notifications for `fj-user'.
ALL is a boolean, meaning also return read notifications.
STATUS-TYPES is a list the members of which must be members of
`fj-notifications-status-types'.
SUBJECT-TYPE is a list the members of which must be members of
`fj-notifications-subject-types'.
PAGE and LIMIT are for pagination."
  ;; NB: STATUS-TYPES and SUBJECT-TYPE are array strings."
  (let ((params (fedi-opt-params (all :boolean "true") status-types
                                 subject-type page limit before since)))
    (fj-get "notifications" params)))

(defun fj-get-new-notifications-count ()
  "Return the number of new notifications for `fj-user'."
  (alist-get 'new
             (fj-get "notifications/new")))

(defun fj-view-notifications (&optional all status-types subject-type
                                        page limit)
  "View notifications for `fj-user'.
ALL is either \"all\" or \"unread\", meaning which set of notifs to
display.
STATUS-TYPES must be a member of `fj-notifications-status-types'.
SUBJECT-TYPE must be a member of `fj-notifications-subject-types'.
PAGE and LIMIT are for pagination."
  (interactive)
  (let* ((all-type (if all "all" "unread"))
         (buf (format "*fj-notifications-%s%s*"
                      all-type
                      (if subject-type
                          (concat "-" subject-type "s")
                        "")))
         (data (fj-get-notifications all status-types
                                     subject-type page limit)))
    (fedi-with-buffer buf 'fj-notifications-mode nil
      (if (not data)
          (insert
           (format "No notifications of type: %s %s" all-type
                   (or subject-type "")))
        (save-excursion (fj-render-notifications data)))
      (setq fj-buffer-spec `( :viewfun fj-view-notifications
                              :viewargs
                              ( :all ,all :status-types ,status-types
                                :subject-type ,subject-type
                                :page ,page :limit ,limit))))
    ;; FIXME: make this an option in `fedi-with-buffer'?
    ;; else it just goes to point-min:
    (fj-next-tab-item)))

(defun fj-view-notifications-all (&optional status-types subject-type
                                            page limit)
  "View all notifications for `fj-user'.
STATUS-TYPES must be a member of `fj-notifications-status-types'.
SUBJECT-TYPE must be a member of `fj-notifications-subject-types'.
PAGE and LIMIT are for pagination."
  (interactive)
  (fj-view-notifications "true" status-types subject-type
                         page limit))

(defun fj-notifications-all-plist (plist)
  "Update the value of :state in PLIST and return it."
  (let* ((current (plist-get plist :all))
         (next (if current nil "true")))
    (plist-put plist :all next)))

(defun fj-notifications-subject-plist (plist)
  "Replace :subject-type in PLIST with next value.
Values are in `fj-notifications-subject-types'."
  (let* ((current (plist-get plist :subject-type))
         (next (fj-next-item-var current fj-notifications-subject-types)))
    (plist-put plist :subject-type next)))

(defun fj-notifications-subject-cycle ()
  "Cycle notifications by `fj-notifications-subject-types'.
Subject types are \"issues\" \"pulls\" \"commits\" and \"repository\"."
  (interactive)
  ;; NB: subject-type can be a list of things, but for now we just cycle
  ;; one-by-one:
  (fj-destructure-buf-spec (viewfun viewargs)
    (let ((args (fj-notifications-subject-plist viewargs)))
      (apply viewfun (fj-plist-values args)))))

(defun fj-notifications-unread-toggle ()
  "Switch between showing all notifications, and only showing unread."
  (interactive)
  (fj-destructure-buf-spec (viewfun viewargs)
    (let ((args (fj-notifications-all-plist viewargs)))
      (apply viewfun (fj-plist-values args)))))

(defun fj-render-notifications (data)
  "Render notifications DATA."
  (cl-loop for n in data
           do (fj-render-notification n)))

(defun fj-render-notification (notif)
  "Render NOTIF."
  (let-alist notif
    ;; notifs don't have item #, so we get from URL:
    (let ((number (car (last (split-string .subject.url "/"))))
          (unread (eq t .unread)))
      (insert
       "\n"
       (propertize
        (concat
         (propertize (concat "#" number)
                     'face 'fj-comment-face)
         " "
         (fj-propertize-link
          (fj-format-tl-title .subject.title nil
                              (unless unread 'fj-closed-issue-notif-face)
                              (unless unread 'fj-closed-issue-notif-verbatim-face))
          'notif number nil :noface)
         "\n"
         (propertize
          (concat .repository.owner.login "/" .repository.name)
          'face (when (not unread) 'fj-comment-face)))
        'fj-repo .repository.name
        'fj-owner .repository.owner.login
        'fj-url .subject.html_url
        'fj-notification .id
        'fj-notif-unread unread
        'fj-byline t) ; for nav
       "\n" fedi-horiz-bar fedi-horiz-bar "\n"))))

(defun fj-mark-notifs-read ()
  "Mark all notifications read."
  (interactive)
  (let* ((endpoint "notifications")
         (params '(("all" . "true")))
         (resp (fj-put endpoint params)))
    (fedi-http--triage
     resp
     (lambda (_)
       (message "All notifications read!")
       (fj-view-notifications-all)))))

(defun fj-mark-notification (status &optional id reload)
  "Mark notification at point as STATUS.
Use ID if provided.
If RELOAD, also reload the notications view."
  ;; PATCH /notifications/threads/{id}
  (let* ((id (or id (fj--property 'fj-notification)))
         (endpoint (format "notifications/threads/%s" id))
         (params `(("to-status" . ,status)))
         (resp (fj-patch endpoint params)))
    (fedi-http--triage
     resp
     (lambda (_)
       (message "Notification %s marked %s!" id status)
       ;; FIXME: needs to be optional, as `fj-mark-notification-read'
       ;; is also called when we load an item from notifs view:
       (when reload
         (fj-view-reload))))))

(defun fj-mark-notification-read (&optional id)
  "Mark notification at point as read.
Use ID if provided."
  (interactive)
  (fj-mark-notification "read" id))

(defun fj-mark-notification-unread (&optional id)
  "Mark notification at point as unread.
Use ID if provided."
  (interactive)
  (fj-mark-notification "unread" id :reload))

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
        (let* ((name
                (cond ((eq major-mode #'fj-issue-tl-mode)
                       (fj--get-tl-col 4))
                      ((eq major-mode #'fj-owned-issues-tl-mode)
                       (concat (fj--get-tl-col 5)
                               (propertize
                                (concat "\t[" (fj--get-tl-col 2) "]")
                                'face 'font-lock-comment-face)))
                      (t
                       (fj--get-tl-col 0)))))
          (push `(,name . ,(point)) alist))
        (forward-line)))
    alist))

;;; ITEMS: RENDERING HANDLES, etc.

(defun fj-render-item-user (str)
  "Render item user STR as a clickable hyperlink."
  (propertize str
              'face 'fj-item-author-face
              'fj-byline t
              'fj-tab-stop t
              'keymap fj-link-keymap
              'button t
              'mouse-face 'highlight
              'follow-link t
              'type 'handle
              'item str)) ; for links

(defun fj-do-link-action (pos)
  "Do the action of the link at POS.
Used for hitting RET on a given link."
  (interactive "d")
  (fj-destructure-buf-spec (repo owner)
    (let ((type (get-text-property pos 'type))
          (item (fj--property 'item)))
      (pcase type
        ((or 'tag 'comment-ref)
         (fj-item-view repo owner item))
        ;; ((eq type 'pull)
        ;; (fj-item-view repo owner item nil :pull))
        ('handle (fj-user-repos-tl item))
        ((or  'commit 'commit-ref)
         (fj-view-commit-diff item))
        ('issue-ref
         (fj-issue-ref-follow item))
        ('notif
         (fj-notif-link-follow item))
        ('shr (shr-browse-url))
        ('more
         (let ((inhibit-read-only t))
           (delete-region
            (save-excursion (beginning-of-line) (point))
            (save-excursion (end-of-line) (point)))
           (fj-item-view-more*)))
        (_
         (error "Unknown link type %s" type))))))

(defun fj-issue-ref-follow (item)
  "Follow an issue ref link.
ITEM is the issue's number."
  (let-alist (fj--property 'fj-item-data)
    (fj-item-view .ref_issue.repository.name .ref_issue.repository.owner
                  item)))

(defun fj-notif-link-follow (item)
  "Follow a notification link.
ITEM is the destination item's id.
After loading, also mark the notification as read."
  ;; NB: we don't use buffer spec repo/owner for notifs links:
  (let ((repo (fj--property 'fj-repo))
        (owner (fj--property 'fj-owner))
        (id (fj--property 'fj-notification))
        (unread (fj--property 'fj-notif-unread)))
    (when unread
      (fj-mark-notification-read id))
    (fj-item-view repo owner item)))

(defun fj-do-link-action-mouse (event)
  "Do the action of the link at point.
Used for a mouse-click EVENT on a link."
  (interactive "e")
  (fj-do-link-action (posn-point (event-end event))))

(defun fj-next-tab-item ()
  "Jump to next tab item."
  (interactive)
  ;; FIXME: some links only have 'shr-tabstop! some have both
  (or (fedi-next-tab-item nil 'fj-tab-stop)
      (fedi-next-tab-item nil 'shr-tab-stop)))

(defun fj-prev-tab-item ()
  "Jump to prev tab item."
  (interactive)
  (fedi-next-tab-item :prev 'fj-tab-stop))

;;; COMMITS

(defun fj-get-commit (repo owner sha)
  "Get a commit with SHA in REPO by OWNER."
  (let ((endpoint (format "repos/%s/%s/git/commits/%s" owner repo sha)))
    (fj-get endpoint)))

(defun fj-browse-commit (&optional repo owner sha)
  "Browse commit with SHA in REPO by OWNER."
  (interactive)
  ;; FIXME: make for commit at point
  (let* ((resp (fj-get-commit repo owner sha))
         (url (alist-get 'html_url resp)))
    (browse-url-generic url)))

(defvar-keymap fj-commits-mode-map
  :doc "Keymap for `fj-commits-mode'."
  :parent fj-generic-map
  "s" #'fj-list-issues-search
  "L" #'fj-repo-commit-log)

(define-derived-mode fj-commits-mode special-mode "fj-commits"
  "Major mode for viewing repo commits."
  :group 'fj
  (read-only-mode 1))

(defun fj-repo-commit-log (&optional prefix repo owner)
  "Render log of commits for REPO by OWNER.
If PREFIX arg, prompt for branch to show commits of."
  (interactive "P")
  (let* ((repo (or repo (fj--get-buffer-spec :repo)))
         (owner (or owner (fj--get-buffer-spec :owner)))
         (branch (when prefix
                   (completing-read "Branch: "
                                    (fj-repo-branches-list repo owner))))
         (data (fj-get-repo-commits repo owner branch))
         (buf (format "*fj-%s-commit-log%s*" repo
                      (if branch (format "-branch-%s" branch) ""))))
    (fedi-with-buffer buf 'fj-commits-mode nil
      ;; FIXME: use `fj-other-window-maybe'
      (setq-local header-line-format (format "Commits in branch: %s"
                                             (or branch "default")))
      (fj-render-commits data)
      (setq fj-current-repo repo)
      (setq fj-buffer-spec `(:repo ,repo :owner ,owner)))))

(defun fj-get-repo-commits (repo owner &optional branch page limit)
  ;; TODO: &optional sha path page limit not)
  ;; stat (diffs), verification, files, (optional, disable for speed)
  "Get commits of REPO by OWNER.
Optionally specify BRANCH to show commits from."
  (let ((endpoint (format "/repos/%s/%s/commits" owner repo))
        (params (append `(("sha" . ,branch))
                        (fedi-opt-params page limit))))
    (fj-get endpoint params)))

(defun fj-render-commits (commits)
  "Remder COMMITS."
  (cl-loop for c in commits
           do (fj-render-commit c)))

(defun fj-render-commit (commit)
  "Render COMMIT."
  (let-alist commit
    (let* ((cr (date-to-time .created))
           ;; (cr-str (format-time-string "%s" cr))
           (cr-display (fedi--relative-time-description cr nil :brief)))
      (insert
       (concat
        (propertize
         (fj-propertize-link (car (string-lines .commit.message))
                             'commit)
         'item .sha
         'fj-url .html_url
         'fj-item-data commit
         'fj-byline t) ; for nav
        "\n"
        ;; we just use author name and username here
        ;; need to look into author/committer difference
        (fj-propertize-link .commit.author.name
                            'handle .author.username 'fj-name-face)
        " committed "
        (propertize cr-display
                    'help-echo .created)
        (propertize
         (concat " | " (substring .sha 0 7))
         'face 'fj-comment-face
         'help-echo .sha)
        "\n" fedi-horiz-bar fedi-horiz-bar "\n\n")))))

;; GET /repos/{owner}/{repo}/activities/feeds
(defun fj-repo-get-feed (repo owner)
  "Get te activity feed of REPO by OWNER."
  (let ((endpoint (format "repos/%s/%s/activities/feeds" owner repo)))
    (fj-get endpoint)))

;;; USERS

(defvar-keymap fj-users-mode-map
  :doc "Keymap for `fj-users-mode'."
  :parent fj-generic-map
  "." #'fj-next-page
  "," #'fj-prev-page
  "s" #'fj-list-issues-search
  "L" #'fj-repo-commit-log)

(define-derived-mode fj-users-mode special-mode "fj-users"
  "Major mode for viewing users."
  :group 'fj
  (read-only-mode 1))

;;; repo users

(defun fj-repo-users (fetch-fun buf-str &optional repo owner
                                viewfun page limit)
  "Render users for REPO by OWNER.
Fetch users by calling FETCH-FUN with two args, REPO and OWNER.
BUF-STR is the name of the buffer string to use."
  (let* ((repo (or repo (fj--get-buffer-spec :repo)))
         (owner (or owner (fj--get-buffer-spec :owner)))
         (buf (format "*fj-%s-%s*" repo buf-str))
         (data (funcall fetch-fun repo owner page limit))
         (endpoint (if (eq fetch-fun #'fj-get-stargazers)
                       "stars"
                     "watchers")))
    (fedi-with-buffer buf 'fj-users-mode nil
      (fj-render-users data)
      (when repo (setq fj-current-repo repo))
      (setq fj-buffer-spec
            `( :repo ,repo :owner ,owner
               :url ,(format "%s/%s/%s/%s" fj-host owner repo endpoint)
               :viewargs ( :repo ,repo :owner ,owner
                           :page ,page :limit ,limit)
               :viewfun ,viewfun)))))

(defun fj-get-stargazers (repo owner &optional page limit)
  "Get stargazers of REPO by OWNER.
PAGE and LIMIT are for pagination."
  (let ((endpoint (format "/repos/%s/%s/stargazers" owner repo))
        (params (fedi-opt-params page limit)))
    (fj-get endpoint params)))

(defun fj-repo-stargazers (&optional repo owner page limit)
  "Render stargazers for REPO by OWNER.
PAGE and LIMIT are for pagination."
  (interactive)
  (fj-repo-users #'fj-get-stargazers "stargazers"
                 repo owner #'fj-repo-stargazers page limit))

;; (defun fj-repo-tl-stargazers ()
;;   "View a listing of stargazers of repo at point.
;; PAGE and LIMIT are for `fj-get-stargazers'."
;;   (interactive)
;;   (let* ((repo (fj--repo-name))
;;          (owner (fj--repo-owner)))
;;     (fj-repo-stargazers repo owner)))

(defun fj-stargazers-completing (&optional page limit)
  "Prompt for a repo stargazer, and view their repos.
PAGE and LIMIT are for `fj-get-stargazers'."
  (interactive)
  (let* ((repo (fj--repo-name))
         (owner (fj--repo-owner))
         (gazers (fj-get-stargazers repo owner page limit))
         (gazers-list (cl-loop for u in gazers
                               collect (alist-get 'login u)))
         (choice (completing-read "Stargazer: " gazers-list)))
    (fj-user-repos-tl choice)))

;; RET/click on stars:
(defun fj-repo-tl-stargazers (&optional _)
  "View stargazers of tabulated list repo at point."
  (interactive)
  (fj-with-repo-entry
   (let ((repo (fj--get-tl-col 0)))
     (fj-repo-stargazers repo))))

(defun fj-get-watchers (repo owner &optional page limit)
  "Get watchers of REPO by OWNER.
PAGE and LIMIT are for pagination."
  (let ((endpoint (format "/repos/%s/%s/subscribers" owner repo))
        (params (fedi-opt-params page limit)))
    (fj-get endpoint params)))

(defun fj-repo-watchers (&optional repo owner page limit)
  "Render watchers for REPO by OWNER.
PAGE and LIMIT are for pagination."
  (interactive)
  (fj-repo-users #'fj-get-watchers "watchers"
                 repo owner #'fj-repo-watchers page limit))

;; (defun fj-repo-tl-watchers ()
;;   "View a listing of watchers of repo at point.
;; PAGE and LIMIT are for `fj-get-watchers'."
;;   (interactive)
;;   (let* ((repo (fj--repo-name))
;;          (owner (fj--repo-owner)))
;;     (fj-repo-watchers repo owner)))

;;; account users

(defun fj-account-users (fetch-fun buf-str &optional user
                                   viewfun page limit)
  "Render users linked somehow to USER.
Fetch users by calling FETCH-FUN with no args.
BUF-STR is the name of the `buffer-string' to use."
  (let* ((user (or user fj-user))
         (buf (format "*fj-%s" buf-str))
         (data (funcall fetch-fun)))
    (fedi-with-buffer buf 'fj-users-mode nil
      (fj-render-users data)
      ;; (when repo (setq fj-current-repo repo))
      (setq fj-buffer-spec
            ;; FIXME: url for browsing
            `( :viewargs (:user ,user :page ,page :limit ,limit)
               :viewfun ,viewfun)))))

(defun fj-get-user-followers ()
  "Get users you `fj-user' is followed by."
  (let* ((endpoint "/user/followers"))
    (fj-get endpoint)))

(defun fj-user-followers (&optional user page limit)
  "View users who follow USER or `fj-user'.
PAGE and LIMIT are for pagination."
  (interactive)
  (fj-account-users #'fj-get-user-followers "followers"
                    user #'fj-user-followers page limit))

(defun fj-get-user-following ()
  "Get users you `fj-user' is following."
  (let* ((endpoint "/user/following"))
    (fj-get endpoint)))

(defun fj-user-following (&optional user page limit)
  "View users that USER or `fj-user' is following.
PAGE and LIMIT are for pagination."
  (interactive)
  (fj-account-users #'fj-get-user-following "following"
                    user #'fj-user-following page limit))

;;; render users

(defun fj-render-users (users)
  "Render USERS."
  (cl-loop for u in users
           do (fj-render-user u)))

(defun fj-render-user (user)
  "Render USER."
  (let-alist user
    (let* ((cr (date-to-time .created))
           ;; (cr-str (format-time-string "%s" cr))
           (cr-display (fedi--relative-time-description cr nil :brief)))
      ;; username:
      (insert
       (propertize
        (fj-propertize-link .login 'handle .login)
        'fj-url .html_url
        'fj-item-data .login_name
        'fj-byline t)) ; for nav
      ;; timestamp:
      (insert
       (propertize (concat " joined " cr-display)
                   'face 'fj-comment-face))
      (insert
       (concat
        "\n"
        ;; website:
        (unless (string-empty-p .website)
          (concat (fj-propertize-link .website 'shr nil
                                      'fj-simple-link-face)
                  "\n"))
        ;; description:
        ;; TODO: render links here:
        (unless (string-empty-p .description)
          (concat (string-clean-whitespace .description) "\n"))))
      (insert "\n" fedi-horiz-bar fedi-horiz-bar "\n\n"))))

(defun fj-watch-repo ()
  "Watch repo at point or in current view."
  (interactive)
  (let* ((owner (or (fj--get-buffer-spec :owner)
                    (fj--get-tl-col 1)))
         (repo (or (fj--get-buffer-spec :repo)
                   (fj--get-tl-col 0)))
         (endpoint (format "repos/%s/%s/subscription" owner repo))
         (resp (fj-put endpoint)))
    (fedi-http--triage resp
                       (lambda (_)
                         (message "Repo %s watched!" repo)))))

;;; TOPICS
;; topics are set in fj-transient.el

(defun fj-get-repo-topics ()
  "GET repo topics from the instance.
Returns a list of strings."
  (fj-destructure-buf-spec (repo owner)
    (let ((endpoint (format "repos/%s/%s/topics" owner repo)))
      (alist-get 'topics (fj-get endpoint)))))

(defun fj-propertize-repo-topics ()
  "Propertize topics of current repo."
  (let ((topics (fj-get-repo-topics)))
    (cl-loop for top in topics
             concat (fj-propertize-topic top))))

(defface fj-topic-face
  `((t :inherit region))
  "Face for repo topics.")

(defun fj-propertize-topic (topic)
  "Propertize TOPIC, a string."
  (propertize topic
              'face 'fj-topic-face))

;;; TAGS
;; we can create and push tags with magit, but can't delete them on the
;; server

(defun fj-get-repo-tags ()
  "Return tags data for current repo."
  (fj-destructure-buf-spec (repo owner)
    (let ((endpoint (format "repos/%s/%s/tags" owner repo)))
      (fj-get endpoint))))

(defun fj-delete-repo-tag ()
  "Prompt for a repo tag and delete it on the server."
  (interactive)
  (fj-destructure-buf-spec (repo owner)
    (let* ((tags (fj-get-repo-tags))
           (list (cl-loop for x in tags
                          collect (alist-get 'name x)))
           (choice (completing-read "Delete tag: " list))
           (endpoint (format "repos/%s/%s/tags/%s" owner repo choice))
           (resp (fj-delete endpoint)))
      (fedi-http--triage
       resp
       (lambda (_)
         (message "Tag %s deleted!" choice))))))

;;; ACTIVITIES

(defvar fj-activity-types
  '(create_repo
    rename_repo
    star_repo
    watch_repo
    commit_repo
    create_issue
    create_pull_request
    transfer_repo
    push_tag
    comment_issue
    merge_pull_request
    close_issue
    reopen_issue
    close_pull_request
    reopen_pull_request
    delete_tag
    delete_branch
    mirror_sync_push
    mirror_sync_create
    mirror_sync_delete
    approve_pull_request
    reject_pull_request
    comment_pull
    publish_release
    pull_review_dismissed
    pull_request_ready_for_review
    auto_merge_pull_request))

(defun fj-get-activities (&optional user-only)
  "Return data for recent activities timeline for `fj-user'.
If USER-ONLY, limit results to only those performed by `fj-user'."
  ;; GET /users/{username}/activities/feeds
  (let* ((endpoint (format "/users/%s/activities/feeds" fj-user)))
    ;; items to display: .op_type, .act_user.login, repo.name
    ;; .created .comment/issue.body, commit refs, etc.
    (fj-get endpoint user-only)))

;;; INSPECT DATA

(defun fj-inspect-item-data (&optional property)
  "Browse the JSON data of item at point.
Browse PROPERTY or else fj-item-data."
  (interactive)
  (let* ((prop (or property 'fj-item-data))
         (data (fedi--property prop)))
    (if (not data)
        (user-error "No property found: %s" prop)
      (with-current-buffer (get-buffer-create "*fj.el-json*")
        (insert
         (prin1-to-string data))
        (emacs-lisp-mode)
        (pp-buffer)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))

(provide 'fj)
;;; fj.el ends here
