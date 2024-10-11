;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(eval-when-compile (require 'subr-x))

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-params-async-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-tl--byline "mastodon-tl")
(autoload 'mastodon-tl--byline-author "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--content "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl")
(autoload 'mastodon-tl--insert-status "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--spoiler "mastodon-tl")
(autoload 'mastodon-tl--item-id "mastodon-tl")
(autoload 'mastodon-tl--update "mastodon-tl")
(autoload 'mastodon-views--view-follow-requests "mastodon-views")
(autoload 'mastodon-tl--current-filters "mastodon-views")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-notifications-get "mastodon")


(defgroup mastodon-tl nil
  "Nofications in mastodon.el."
  :prefix "mastodon-notifications-"
  :group 'mastodon)

(defcustom mastodon-notifications--profile-note-in-foll-reqs t
  "If non-nil, show a user's profile note in follow request notifications."
  :type '(boolean))

(defcustom mastodon-notifications--profile-note-in-foll-reqs-max-length nil
  "The max character length for user profile note in follow requests.
Profile notes are only displayed if
`mastodon-notifications--profile-note-in-foll-reqs' is non-nil.
If unset, profile notes of any size will be displayed, which may
make them unweildy."
  :type '(integer))

(defcustom mastodon-notifications--images-in-notifs nil
  "Whether to display attached images in notifications."
  :type '(boolean))

(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--display-media-p)
(defvar mastodon-mode-map)

(defvar mastodon-notifications--types-alist
  '(("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog)
    ("mention" . mastodon-notifications--mention)
    ("poll" . mastodon-notifications--poll)
    ("follow_request" . mastodon-notifications--follow-request)
    ("status" . mastodon-notifications--status)
    ("update" . mastodon-notifications--edit))
  "Alist of notification types and their corresponding function.")

(defvar mastodon-notifications--response-alist
  '(("Followed" . "you")
    ("Favourited" . "your status from")
    ("Boosted" . "your status from")
    ("Mentioned" . "you")
    ("Posted a poll" . "that has now ended")
    ("Requested to follow" . "you")
    ("Posted" . "a post")
    ("Edited" . "a post from"))
  "Alist of subjects for notification types.")

(defvar mastodon-notifications--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-mode-map)
    (define-key map (kbd "a") #'mastodon-notifications--follow-request-accept)
    (define-key map (kbd "j") #'mastodon-notifications--follow-request-reject)
    (define-key map (kbd "C-k") #'mastodon-notifications--clear-current)
    map)
  "Keymap for viewing notifications.")

(defun mastodon-notifications--byline-concat (message)
  "Add byline for TOOT with MESSAGE."
  (concat " " (propertize message 'face 'highlight)
          " " (cdr (assoc message mastodon-notifications--response-alist))))

(defun mastodon-notifications--follow-request-process (&optional reject)
  "Process the follow request at point.
With no argument, the request is accepted. Argument REJECT means
reject the request. Can be called in notifications view or in
follow-requests view."
  (if (not (mastodon-tl--find-property-range 'item-json (point)))
      (user-error "No follow request at point?")
    (let* ((item-json (mastodon-tl--property 'item-json))
           (f-reqs-view-p (string= "follow_requests"
                                   (plist-get mastodon-tl--buffer-spec 'endpoint)))
           (f-req-p (or (string= "follow_request" (alist-get 'type item-json)) ;notifs
                        f-reqs-view-p)))
      (if (not f-req-p)
          (user-error "No follow request at point?")
        (let-alist (or (alist-get 'account item-json) ;notifs
                       item-json) ;f-reqs
          (if (not .id)
              (user-error "No account result at point?")
            (let ((response
                   (mastodon-http--post
                    (mastodon-http--api
                     (format "follow_requests/%s/%s"
                             .id (if reject "reject" "authorize"))))))
              (mastodon-http--triage
               response
               (lambda (_)
                 (if f-reqs-view-p
                     (mastodon-views--view-follow-requests)
                   (mastodon-tl--reload-timeline-or-profile))
                 (message "Follow request of %s (@%s) %s!"
                          .username .acct (if reject "rejected" "accepted")))))))))))

(defun mastodon-notifications--follow-request-accept ()
  "Accept a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process))

(defun mastodon-notifications--follow-request-reject ()
  "Reject a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process :reject))

(defun mastodon-notifications--comment-note-text (str)
  "Add comment face to all text in STR with `shr-text' face only."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'face 'shr-text t))
        (add-text-properties (prop-match-beginning prop)
                             (prop-match-end prop)
                             '(face (font-lock-comment-face shr-text)))))
    (buffer-string)))

(defvar mastodon-notifications-grouped-types
  '(follow boost favourite)
  "List of notification types for which grouping is implemented.")

(defvar mastodon-notifications--action-alist
  '((reblog . "Boosted")
    (favourite . "Favourited")
    (follow-request . "Requested to follow")
    (follow . "Followed")
    (mention . "Mentioned")
    (status . "Posted")
    (poll . "Posted a poll")
    (edit . "Edited")))

(defun mastodon-notifications--alist-by-value (str field json)
  "From JSON, return the alist whose FIELD value matches STR.
JSON is a list of alists."
  (cl-some (lambda (y)
             (when (string= str (alist-get field y))
               y))
           json))

(defun mastodon-notifications--group-accounts (ids json)
  "For IDS, return account data in JSON."
  (cl-loop
   for x in ids
   collect (mastodon-notifications--alist-by-value x 'id json)))

(defun mastodon-notifications--format-note (group json type)
  "Format for a NOTE of TYPE."
  ;; FIXME: apply/refactor filtering as per/with `mastodon-tl--toot'
  ;; (if (member type mastodon-notifications-grouped-types)
  (let-alist group
    ;; .sample_account_ids .status_id .notifications_count
    ;; .most_recent_notifiation_id
    (let* ((status (mastodon-notifications--alist-by-value
                    .status_id 'id (alist-get 'statuses json)))
           (accounts (mastodon-notifications--group-accounts
                      .sample_account_ids (alist-get 'accounts json))))
      (insert (symbol-name type) "\n"
              "accounts: " (mapconcat 'identity .sample_account_ids ", ") "\n"
              (if (not (> .notifications_count (length .sample_account_ids)))
                  ""
                (concat
                 "and"
                 (number-to-string
                  (- .notifications_count
                     (length .sample_account_ids))))
                "others \n")
              ;; "count: " (number-to-string .notifications_count)
              (alist-get type mastodon-notifications--action-alist)
              "\n toot: " (or .status_id "") "\n"
              "\n\n"))
    ))

;; non-grouped notifs:
;; (let* ((id (alist-get 'id note))
;;        (profile-note
;;         (when (eq 'follow-request type)
;;           (let ((str (mastodon-tl--field
;;                       'note
;;                       (mastodon-tl--field 'account note))))
;;             (if mastodon-notifications--profile-note-in-foll-reqs-max-length
;;                 (string-limit str mastodon-notifications--profile-note-in-foll-reqs-max-length)
;;               str))))
;;        (status (mastodon-tl--field 'status note))
;;        (follower (alist-get 'username (alist-get 'account note)))
;;        (toot (alist-get 'status note))
;;        (filtered (mastodon-tl--field 'filtered toot))
;;        (filters (when filtered
;;                   (mastodon-tl--current-filters filtered))))
;;   (if (and filtered (assoc "hide" filters))
;;       nil
;;     (mastodon-tl--insert-status
;;      ;; toot
;;      (cond ((or (eq type 'follow)
;;                 (eq type 'follow-request))
;;             ;; Using reblog with an empty id will mark this as something
;;             ;; non-boostable/non-favable.
;;             (cons '(reblog (id . nil)) note))
;;            ;; reblogs/faves use 'note' to process their own json
;;            ;; not the toot's. this ensures following etc. work on such notifs
;;            ((or (eq type 'favourite)
;;                 (eq type 'boost))
;;             note)
;;            (t
;;             status))
;;      ;; body
;;      (let ((body (if-let ((match (assoc "warn" filters)))
;;                      (mastodon-tl--spoiler toot (cadr match))
;;                    (mastodon-tl--clean-tabs-and-nl
;;                     (if (mastodon-tl--has-spoiler status)
;;                         (mastodon-tl--spoiler status)
;;                       (if (eq 'follow-request type)
;;                           (mastodon-tl--render-text profile-note)
;;                         (mastodon-tl--content status)))))))
;;        (cond ((or (eq type 'follow)
;;                   (eq type 'follow-request))
;;               (if (eq type 'follow)
;;                   (propertize "Congratulations, you have a new follower!"
;;                               'face 'default)
;;                 (concat
;;                  (propertize
;;                   (format "You have a follow request from... %s"
;;                           follower)
;;                   'face 'default)
;;                  (when mastodon-notifications--profile-note-in-foll-reqs
;;                    (concat
;;                     ":\n"
;;                     (mastodon-notifications--comment-note-text body))))))
;;              ((or (eq type 'favourite)
;;                   (eq type 'boost))
;;               (mastodon-notifications--comment-note-text body))
;;              (t body)))
;;      ;; author-byline
;;      (if (or (eq type 'follow)
;;              (eq type 'follow-request)
;;              (eq type 'mention))
;;          'mastodon-tl--byline-author
;;        (lambda (_status &rest _args) ; unbreak stuff
;;          (mastodon-tl--byline-author note)))
;;      ;; action-byline
;;      (lambda (_status)
;;        (mastodon-notifications--byline-concat
;;         (alist-get type mastodon-notifications--action-alist)
;;         ))
;;      id
;;      ;; base toot
;;      (when (or (eq type 'favourite)
;;                (eq type 'boost))
;;        status))))))

(defun mastodon-notifications--by-type (groups json)
  "Filter NOTE for those listed in `mastodon-notifications--types-alist'.
Call its function in that list on NOTE."
  (cl-loop for g in groups
           for type = (alist-get 'type g)
           for fun = (cdr (assoc type mastodon-notifications--types-alist))
           for start-pos = (point)
           do (when fun
                (funcall fun g json)
                (when mastodon-tl--display-media-p
                  ;; images-in-notifs custom is handeld in
                  ;; `mastodon-tl--media-attachment', not here
                  (mastodon-media--inline-images start-pos (point))))))

(defun mastodon-notifications--timeline (json)
  "Format JSON in Emacs buffer."
  (if (seq-empty-p json)
      (user-error "Looks like you have no (more) notifications for now")
    (let ((groups (alist-get 'notification_groups json)))
      ;; (mapc (lambda (x)
      (mastodon-notifications--by-type groups json)
      ;; grouped)
      (goto-char (point-min)))))

(defun mastodon-notifications--get-mentions ()
  "Display mention notifications in buffer."
  (interactive)
  (mastodon-notifications-get "mention" "mentions"))

(defun mastodon-notifications--get-favourites ()
  "Display favourite notifications in buffer."
  (interactive)
  (mastodon-notifications-get "favourite" "favourites"))

(defun mastodon-notifications--get-boosts ()
  "Display boost notifications in buffer."
  (interactive)
  (mastodon-notifications-get "reblog" "boosts"))

(defun mastodon-notifications--get-polls ()
  "Display poll notifications in buffer."
  (interactive)
  (mastodon-notifications-get "poll" "polls"))

(defun mastodon-notifications--get-statuses ()
  "Display status notifications in buffer.
Status notifications are created when you call
`mastodon-tl--enable-notify-user-posts'."
  (interactive)
  (mastodon-notifications-get "status" "statuses"))

(defun mastodon-notifications--filter-types-list (type)
  "Return a list of notification types with TYPE removed."
  (let ((types (mapcar #'car mastodon-notifications--types-alist)))
    (remove type types)))

(defun mastodon-notifications--clear-all ()
  "Clear all notifications."
  (interactive)
  (when (y-or-n-p "Clear all notifications?")
    (let ((response
           (mastodon-http--post (mastodon-http--api "notifications/clear"))))
      (mastodon-http--triage
       response (lambda (_)
                  (when mastodon-tl--buffer-spec
                    (mastodon-tl--reload-timeline-or-profile))
                  (message "All notifications cleared!"))))))

(defun mastodon-notifications--clear-current ()
  "Dismiss the notification at point."
  (interactive)
  (let* ((id (or (mastodon-tl--property 'item-id)
                 (mastodon-tl--field 'id
                                     (mastodon-tl--property 'item-json))))
         (response
          (mastodon-http--post (mastodon-http--api
                                (format "notifications/%s/dismiss" id)))))
    (mastodon-http--triage
     response (lambda (_)
                (when mastodon-tl--buffer-spec
                  (mastodon-tl--reload-timeline-or-profile))
                (message "Notification dismissed!")))))

(defun mastodon-notifications--get-unread-count ()
  "Return the number of unread notifications for the current account."
  ;; params: limit - max 1000, default 100, types[], exclude_types[], account_id
  (let* ((endpoint "notifications/unread_count")
         (url (mastodon-http--api endpoint))
         (resp (mastodon-http--get-json url)))
    (alist-get 'count resp)))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
