;;; fedi-post.el --- Minor mode for posting to fediverse services  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Marty Hiatt <martianhiatus@riseup.net> and mastodon.el authors
;; Package-Requires: ((emacs "28.1"))
;; Version: 1.0.0
;; Homepage: https://codeberg.org/martianh/fedi.el

;; This file is not part of GNU Emacs.

;; This file is part of fedi.el.

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

;; fedi-post.el supports POSTing status data to fediverse services.

;;; Code:
(eval-when-compile (require 'subr-x))

(require 'emojify nil :noerror)
(declare-function emojify-insert-emoji "emojify")
(declare-function emojify-set-emoji-data "emojify")
(defvar emojify-emojis-dir)
(defvar emojify-user-emojis)

(require 'cl-lib)
(require 'persist)
(require 'mastodon-iso)
(require 'facemenu)
(require 'text-property-search)

(eval-when-compile
  (require 'mastodon-tl))

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--enable-proportional-fonts)
(defvar mastodon-profile-account-settings)

;; (autoload 'iso8601-parse "iso8601")
;; (autoload 'mastodon-auth--user-acct "mastodon-auth")
;; (autoload 'mastodon-http--api "mastodon-http")
;; (autoload 'mastodon-http--build-array-params-alist "mastodon-http")
;; (autoload 'mastodon-http--delete "mastodon-http")
;; (autoload 'mastodon-http--get-json "mastodon-http")
;; (autoload 'mastodon-http--get-json-async "mastodon-http")
;; (autoload 'mastodon-http--post "mastodon-http")
;; (autoload 'mastodon-http--post-media-attachment "mastodon-http")
;; (autoload 'mastodon-http--process-json "mastodon-http")
;; (autoload 'mastodon-http--put "mastodon-http")
;; (autoload 'mastodon-http--read-file-as-string "mastodon-http")
;; (autoload 'mastodon-http--triage "mastodon-http")
;; (autoload 'mastodon-profile--fetch-server-account-settings "mastodon-profile")
;; (autoload 'mastodon-profile--fetch-server-account-settings-maybe "mastodon-profile")
;; (autoload 'mastodon-profile--get-source-pref "mastodon-profile")
;; (autoload 'mastodon-profile--show-user "mastodon-profile")
;; (autoload 'mastodon-profile--update-preference "mastodon-profile")
;; (autoload 'mastodon-search--search-accounts-query "mastodon-search")
;; (autoload 'mastodon-search--search-tags-query "mastodon-search")
;; (autoload 'mastodon-tl--as-string "mastodon-tl")
;; (autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
;; (autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
;; (autoload 'mastodon-tl--do-if-toot-strict "mastodon-tl")
;; (autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'fedi--find-property-range "fedi")
(autoload 'fedi--find-property-range "fedi")
;; (autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
;; (autoload 'mastodon-tl--map-alist "mastodon-tl")
;; (autoload 'fedi--property "mastodon-tl")
;; (autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
;; (autoload 'mastodon-tl--render-text "mastodon-tl")
;; (autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
;; (autoload 'mastodon-tl--symbol "mastodon-tl")
;; (autoload 'mastodon-tl--toot-id "mastodon-tl")
;; (autoload 'fedi-post "mastodon")
;; (autoload 'mastodon-views--cancel-scheduled-toot "mastodon-views")
;; (autoload 'mastodon-views--view-scheduled-toots "mastodon-views")
;; (autoload 'org-read-date "org")

(defface fedi-post-docs-face
  `((t :inherit font-lock-comment-face))
  "Face used for documentation in post compose buffer.
If `mastodon-tl--enable-proportional-fonts' is changed,
mastodon.el needs to be re-loaded for this to be correctly set.")

(defgroup fedi-post nil
  "Posting in Mastodon."
  :prefix "fedi-post-"
  :group 'mastodon)

(defcustom fedi-post--enable-completion t
  "Whether to enable completion of mentions and hashtags.
Used for completion in post compose buffer."
  :type 'boolean)

(defcustom fedi-post--use-company-for-completion nil
  "Whether to enable company for completion.
When non-nil, `company-mode' is enabled in the post compose
buffer, and mastodon completion backends are added to
`company-capf'.

You need to install company yourself to use this."
  :type 'boolean)

(defvar-local fedi-post-content-nsfw nil
  "A flag indicating whether the post should be marked as NSFW.")

(defvar-local fedi-post-language nil
  "The language of the post being composed, in ISO 639 (two-letter).")

(defvar-local fedi-post--reply-to-id nil
  "Buffer-local variable to hold the id of the post being replied to.")

(defvar-local fedi-post--edit-post-id nil
  "The id of the post being edited.")

(defvar-local fedi-post-previous-window-config nil
  "A list of window configuration prior to composing a post.
Takes its form from `window-configuration-to-register'.")

(defvar fedi-post--max-chars nil
  "The maximum allowed characters count for a single post.")

(defvar-local fedi-post-completions nil
  "The data of completion candidates for the current completion at point.")

(defvar fedi-post-current-post-text nil
  "The text of the post being composed.")

(persist-defvar fedi-post-draft-posts-list nil
                "A list of posts that have been saved as drafts.
For the moment we just put all composed posts in here, as we want
to also capture posts that are 'sent' but that don't successfully
send.")


;;; REGEXES

(defvar fedi-post-handle-regex
  (rx (| (any ?\( "\n" "\t "" ") bol) ; preceding things
      (group-n 2 (+ ?@ (* (any ?- ?_ ?. "A-Z" "a-z" "0-9" ))) ; handle
               (? ?@ (* (not (any "\n" "\t" " "))))) ; optional domain
      (| "'" word-boundary))) ; boundary or possessive

(defvar fedi-post-tag-regex
  (rx (| (any ?\( "\n" "\t" " ") bol)
      (group-n 2 ?# (+ (any "A-Z" "a-z" "0-9")))
      (| "'" word-boundary))) ; boundary or possessive

(defvar fedi-post-url-regex
  ;; adapted from ffap-url-regexp
  (concat
   "\\(?2:\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)" ; uri prefix
   "[^ \n\t]*\\)" ; any old thing, that is, i.e. we allow invalid/unwise chars
   "\\b")) ; boundary


;;; MODE MAP

(defvar fedi-post-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") #'fedi-post-send)
    (define-key map (kbd "C-c C-k") #'fedi-post-cancel)
    (define-key map (kbd "C-c C-n") #'fedi-post-toggle-nsfw)
    (when (require 'emojify nil :noerror)
      (define-key map (kbd "C-c C-e") #'fedi-post-insert-emoji))
    (define-key map (kbd "C-c C-l") #'fedi-post-set-post-language)
    map)
  "Keymap for `fedi-post'.")

(defun fedi-post-kill (&optional cancel)
  "Kill `fedi-post-mode' buffer and window.
CANCEL means the post was not sent, so we save the post text as a draft."
  (let ((prev-window-config fedi-post-previous-window-config))
    (unless (eq fedi-post-current-post-text nil)
      (when cancel
        (cl-pushnew fedi-post-current-post-text
                    fedi-post-draft-posts-list :test 'equal)))
    ;; prevent some weird bug when cancelling a non-empty post:
    ;; (delete #'fedi-post--save-post-text after-change-functions)
    (kill-buffer-and-window)
    (fedi-post--restore-previous-window-config prev-window-config)))

(defun fedi-post-cancel ()
  "Kill new-post buffer/window. Does not POST content to Mastodon.
If post is not empty, prompt to save text as a draft."
  (interactive)
  (if (fedi-post--empty-p)
      (fedi-post-kill)
    (when (y-or-n-p "Save draft post?")
      (fedi-post--save-draft))
    (fedi-post-kill)))

(defun fedi-post--empty-p (&optional text-only)
  "Return t if post has no text, attachments, or polls.
TEXT-ONLY means don't check for attachments or polls."
  (string-empty-p (mastodon-tl--clean-tabs-and-nl
                   (fedi-post--remove-docs))))


(defun fedi-post--remove-docs ()
  "Get the body of a post from the current compose buffer."
  (let ((header-region (fedi--find-property-range 'post-post-header
                                                  (point-min))))
    (buffer-substring (cdr header-region) (point-max))))


;;; SEND POST FUNCTION

;; (defun fedi-post-send ()
;;   "POST contents of new-post buffer to Mastodon instance and kill buffer.
;; If media items have been attached and uploaded with
;; `fedi-post--attach-media', they are attached to the post.
;; If `fedi-post--edit-post-id' is non-nil, PUT contents to
;; instance to edit a post."
;;   (interactive)
;;   (let* ((post (fedi-post--remove-docs))
;;          (endpoint (if edit-id ; we are sending an edit:
;;                        (mastodon-http--api (format "statuses/%s" edit-id))
;;                      (mastodon-http--api "statuses")))
;;          (args-no-media (append `(("status" . ,post)
;;                                   ("in_reply_to_id" . ,fedi-post--reply-to-id)
;;                                   ("visibility" . ,fedi-post--visibility)
;;                                   ("sensitive" . ,(when fedi-post-content-nsfw
;;                                                     (symbol-name t)))
;;                                   ("spoiler_text" . ,cw)
;;                                   ("language" . ,fedi-post-language))
;;                                 ;; Pleroma instances can't handle null-valued
;;                                 ;; scheduled_at args, so only add if non-nil
;;                                 (when scheduled `(("scheduled_at" . ,scheduled)))))
;;          (args-media (when fedi-post--media-attachments
;;                        (mastodon-http--build-array-params-alist
;;                         "media_ids[]"
;;                         fedi-post--media-attachment-ids)))
;;          (args-poll (when fedi-post-poll
;;                       (fedi-post--build-poll-params)))
;;          ;; media || polls:
;;          (args (if fedi-post--media-attachments
;;                    (append args-media args-no-media)
;;                  (if fedi-post-poll
;;                      (append args-no-media args-poll)
;;                    args-no-media)))
;;          (prev-window-config fedi-post-previous-window-config))
;;     (cond ((and fedi-post--media-attachments
;;                 ;; make sure we have media args
;;                 ;; and the same num of ids as attachments
;;                 (or (not args-media)
;;                     (not (= (length fedi-post--media-attachments)
;;                             (length fedi-post--media-attachment-ids)))))
;;            (message "Something is wrong with your uploads. Wait for them to complete or try again."))
;;           ((and fedi-post--max-chars
;;                 (> (fedi-post--count-post-chars post cw) fedi-post--max-chars))
;;            (message "Looks like your post (inc. CW) is longer than that maximum allowed length."))
;;           ((fedi-post--empty-p)
;;            (message "Empty post. Cowardly refusing to post this."))
;;           (t
;;            (let ((response (if edit-id ; we are sending an edit:
;;                                (mastodon-http--put endpoint args)
;;                              (mastodon-http--post endpoint args))))
;;              (mastodon-http--triage
;;               response
;;               (lambda ()
;;                 (fedi-post-kill)
;;                 (if scheduled
;;                     (message "Post scheduled!")
;;                   (message "Post post!"))
;;                 ;; cancel scheduled post if we were editing it:
;;                 (when scheduled-id
;;                   (mastodon-views--cancel-scheduled-post
;;                    scheduled-id :no-confirm))
;;                 (fedi-post--restore-previous-window-config prev-window-config)
;;                 (when edit-id
;;                   (let ((pos (marker-position (cadr prev-window-config))))
;;                     (mastodon-tl--reload-timeline-or-profile pos))))))))))

(defun fedi-post--restore-previous-window-config (config)
  "Restore the window CONFIG after killing the post compose buffer.
Buffer-local variable `fedi-post-previous-window-config' holds the config."
  (set-window-configuration (car config))
  (goto-char (cadr config)))

(defun fedi-post--mentions-to-string (mentions)
  "Apply `fedi-post--process-local' function to each mention in MENTIONS.
Remove empty string (self) from result and joins the sequence with whitespace."
  (mapconcat (lambda (mention) mention)
	         (remove "" (mapcar #'fedi-post--process-local mentions))
             " "))

(defun fedi-post--process-local (acct)
  "Add domain to local ACCT and replace the curent user name with \"\".
Mastodon requires the full @user@domain, even in the case of local accts.
eg. \"user\" -> \"@user@local.social\" (when local.social is the domain of the
mastodon-instance-url).
eg. \"yourusername\" -> \"\"
eg. \"feduser@fed.social\" -> \"@feduser@fed.social\"."
  (cond ((string-match-p "@" acct) (concat "@" acct)) ; federated acct
        ((string= (mastodon-auth--user-acct) acct) "") ; your acct
        (t (concat "@" acct "@" ; local acct
                   (cadr (split-string mastodon-instance-url "/" t))))))


;;; COMPLETION (TAGS, MENTIONS)

(defun fedi-post--mentions (status)
  "Extract mentions (not the reply-to author or booster) from STATUS.
The mentioned users look like this:
Local user (including the logged in): `username`.
Federated user: `username@host.co`."
  (let* ((boosted (mastodon-tl--field 'reblog status))
         (mentions (if boosted
	               (alist-get 'mentions (alist-get 'reblog status))
	             (alist-get 'mentions status))))
    ;; reverse does not work on vectors in 24.5
    (mastodon-tl--map-alist 'acct (reverse mentions))))

(defun fedi-post--get-bounds (regex)
  "Get bounds of item before point using REGEX."
  ;; # and @ are not part of any existing thing at point
  (save-match-data
    (save-excursion
      ;; match full handle inc. domain, or tag including #
      ;; (see the regexes for subexp 2)
      (when (re-search-backward regex
                                (save-excursion (forward-whitespace -1)
                                                (point))
                                :no-error)
        (cons (match-beginning 2)
              (match-end 2))))))

(defun fedi-post--return-capf (regex completion-fun &optional
                                     annot-fun affix-fun exit-fun)
  "Return a completion at point function.
REGEX is used to get the item before point.
COMPLETION-FUN takes two args, start and end bounds of item
before point, and returns a completion table.
ANNOT-FUN takes one arg, a candidate, and returns an annotation
for it."
  (let* ((bounds (fedi-post--get-bounds regex))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start
            end
            (completion-table-dynamic ; only search when necessary
             (lambda (_)
               ;; Interruptible candidate computation, from minad/d mendler, thanks!
               (let ((result
                      (while-no-input
                        (setq fedi-post-completions
                              (funcall completion-fun start end)))))
                 (and (consp result) result))))
            :exclusive 'no
            ;; :affixation-function
            ;; (lambda (cands)
            ;; (funcall affix-fun cands))
            ;; FIXME: we "should" use :affixation-function for this but i
            ;; can't get it to work so use an exit-fun hack:
            :exit-function
            (lambda (str status)
              (funcall exit-fun str status))
            :annotation-function
            (lambda (cand)
              (concat " " (funcall annot-fun cand)))))))

(defun fedi-post--mentions-annotation-fun (candidate)
  "Given a handle completion CANDIDATE, return its annotation string, a username."
  (cdr (assoc candidate fedi-post-completions)))

(defun fedi-post--tags-annotation-fun (candidate)
  "Given a tag string CANDIDATE, return an annotation, the tag's URL."
  (cadr (assoc candidate fedi-post-completions)))


;;; REPLY

;; (defun fedi-post--reply ()
;;   "Reply to post at `point'.
;; Customize `fedi-post-display-orig-in-reply-buffer' to display
;; text of the post being replied to in the compose buffer."
;;   (interactive)
;;   (mastodon-tl--do-if-post-strict
;;    (let* ((post (fedi--property 'post-json))
;;           ;; no-move arg for base post: don't try next post
;;           (base-post (fedi--property 'base-post)) ; for new notifs handling
;;           (id (mastodon-tl--as-string (mastodon-tl--field 'id (or base-post post))))
;;           (account (mastodon-tl--field 'account post))
;;           (user (alist-get 'acct account))
;;           (mentions (fedi-post--mentions (or base-post post)))
;;           (boosted (mastodon-tl--field 'reblog (or base-post post)))
;;           (booster (when boosted
;;                      (alist-get 'acct
;;                                 (alist-get 'account post)))))
;;      (fedi-post
;;       (when user
;;         (if booster
;;             (if (and (not (equal user booster))
;;                      (not (member booster mentions)))
;;                 ;; different booster, user and mentions:
;; 		(fedi-post--mentions-to-string (append (list user booster) mentions nil))
;;               ;; booster is either user or in mentions:
;;               (if (not (member user mentions))
;;                   ;; user not already in mentions:
;; 		  (fedi-post--mentions-to-string (append (list user) mentions nil))
;;                 ;; user already in mentions:
;;                 (fedi-post--mentions-to-string (copy-sequence mentions))))
;;           ;; ELSE no booster:
;;           (if (not (member user mentions))
;;               ;; user not in mentions:
;; 	      (fedi-post--mentions-to-string (append (list user) mentions nil))
;;             ;; user in mentions already:
;;             (fedi-post--mentions-to-string (copy-sequence mentions)))))
;;       id
;;       (or base-post post)))))


;;; COMPOSE POST SETTINGS

(defun fedi-post-toggle-nsfw ()
  "Toggle `fedi-post-content-nsfw'."
  (interactive)
  (setq fedi-post-content-nsfw
        (not fedi-post-content-nsfw))
  (message "NSFW flag is now %s" (if fedi-post-content-nsfw "on" "off"))
  (fedi-post--update-status-fields))

(defun fedi-post-set-post-language ()
  "Prompt for a language and set `fedi-post-language'.
Return its two letter ISO 639 1 code."
  (interactive)
  (let* ((choice (completing-read "Language for this post: "
                                  mastodon-iso-639-1)))
    (setq fedi-post-language
          (alist-get choice mastodon-iso-639-1 nil nil 'equal))
    (message "Language set to %s" choice)
    (fedi-post--update-status-fields)))

(defun fedi-post--iso-to-human (ts)
  "Format an ISO8601 timestamp TS to be more human-readable."
  (let* ((decoded (iso8601-parse ts))
         (encoded (encode-time decoded)))
    (format-time-string "%d-%m-%y, %H:%M[%z]" encoded)))

(defun fedi-post--iso-to-org (ts)
  "Convert ISO8601 timestamp TS to something `org-read-date' can handle."
  (when ts (let* ((decoded (iso8601-parse ts)))
             (encode-time decoded))))


;;; DISPLAY KEYBINDINGS

(defun fedi-post--get-mode-kbinds (&optional mode-map)
  "Get a list of the keybindings in MODE-MAP or `fedi-post-mode.'"
  (let* ((binds (copy-tree (or mode-map fedi-post-mode-map)))
         (prefix (car (cadr binds)))
         (bindings (remove nil (mapcar (lambda (i)
                                         (when (listp i) i))
                                       (cadr binds)))))
    (mapcar (lambda (b)
              (setf (car b) (vector prefix (car b)))
              b)
            bindings)))

(defun fedi-post--format-kbind-command (cmd &optional prefix)
  "Format CMD to be more readable.
e.g. fedi-post-send -> Send."
  (let* ((str (symbol-name cmd))
         (re (concat prefix
                     "-\\(.*\\)$"))
         (str2 (save-match-data
                 (string-match re str)
                 (match-string 1 str))))
    (capitalize (replace-regexp-in-string "-" " " str2))))

(defun fedi-post--format-kbind (kbind &optional prefix)
  "Format a single keybinding, KBIND, for display in documentation."
  (let ((key (help-key-description (car kbind) nil))
        (command (fedi-post--format-kbind-command (cdr kbind) prefix)))
    (format "    %s - %s" key command)))

(defun fedi-post--format-kbinds (kbinds &optional prefix)
  "Format a list of keybindings, KBINDS, for display in documentation."
  (mapcar (lambda (kb)
            (fedi-post--format-kbind kb prefix))
          kbinds))

(defvar-local fedi-post--kbinds-pairs nil
  "Contains a list of paired post compose buffer keybindings for inserting.")

(defun fedi-post--formatted-kbinds-pairs (kbinds-list longest)
  "Return a list of strings each containing two formatted kbinds.
KBINDS-LIST is the list of formatted bindings to pair.
LONGEST is the length of the longest binding."
  (when kbinds-list
    (push (concat "\n"
                  (car kbinds-list)
                  (make-string (- (1+ longest) (length (car kbinds-list)))
                               ?\ )
                  (cadr kbinds-list))
          fedi-post--kbinds-pairs)
    (fedi-post--formatted-kbinds-pairs (cddr kbinds-list) longest))
  (reverse fedi-post--kbinds-pairs))

(defun fedi-post--formatted-kbinds-longest (kbinds-list)
  "Return the length of the longest item in KBINDS-LIST."
  (let ((lengths (mapcar #'length kbinds-list)))
    (car (sort lengths #'>))))


;;; DISPLAY DOCS

(defun fedi-post--make-mode-docs (&optional mode prefix)
  "Create formatted documentation text for the fedi-post-mode."
  (let* ((mode-map (alist-get mode minor-mode-map-alist))
         (prefix (or prefix (string-remove-suffix "-mode"
                                                  (symbol-name mode))))
         (kbinds (fedi-post--get-mode-kbinds mode-map))
         (longest-kbind (fedi-post--formatted-kbinds-longest
                         (fedi-post--format-kbinds kbinds prefix))))
    (concat
     " Compose a new post here. The following keybindings are available:"
     (mapconcat #'identity
                (fedi-post--formatted-kbinds-pairs
                 (fedi-post--format-kbinds kbinds prefix)
                 longest-kbind)
                nil))))

(defun fedi-post--display-docs-and-status-fields (&optional mode prefix)
  "Insert propertized text with documentation about MODE or `fedi-post-mode'.
Also includes and the status fields which will get updated based
on the status of NSFW, content warning flags, media attachments, etc."
  (let ((divider
         "|=================================================================|"))
    (insert
     (propertize
      (concat
       (fedi-post--make-mode-docs mode prefix) "\n"
       divider "\n"
       " "
       (propertize "Count"
                   'post-post-counter t)
       " ⋅ "
       (propertize "Language"
                   'post-post-language t)
       " "
       (propertize "NSFW"
                   'post-post-nsfw-flag t)
       "\n"
       divider
       "\n")
      'rear-nonsticky t
      'face 'fedi-post-docs-face
      'read-only "Edit your message below."
      'post-post-header t))))

(defun fedi-post--count-post-chars (post-string)
  "Count the characters in POST-STRING.
URLs always = 23, and domain names of handles are not counted.
This is how mastodon does it."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert post-string)
    (goto-char (point-min))
    ;; handle URLs
    ;; (while (search-forward-regexp mastodon-post-url-regex nil t)
    ;;                                     ; "\\w+://[^ \n]*" old regex
    ;;   (replace-match "xxxxxxxxxxxxxxxxxxxxxxx")) ; 23 x's
    ;; handle @handles
    ;; (goto-char (point-min))
    ;; (while (search-forward-regexp mastodon-post-handle-regex nil t)
    ;;   (replace-match (match-string 2))) ; replace with handle only
    ;; (+ (length cw)
    (length (buffer-substring (point-min) (point-max)))))

(defun fedi-post--update-status-fields (&rest _args)
  "Update the status fields in the header based on the current state."
  (ignore-errors  ;; called from after-change-functions so let's not leak errors
    (let* ((inhibit-read-only t)
           (header-region (fedi--find-property-range 'post-post-header
                                                     (point-min)))
           (count-region (fedi--find-property-range 'post-post-counter
                                                    (point-min)))
           (nsfw-region (fedi--find-property-range 'post-post-nsfw-flag
                                                   (point-min)))
           (lang-region (fedi--find-property-range 'post-post-language
                                                   (point-min)))
           (post-string (buffer-substring-no-properties (cdr header-region)
                                                        (point-max))))
      ;; (add-text-properties (car count-region) (cdr count-region)
      ;;                      (list 'display
      ;;                            (format "%s/%s chars"
      ;;                                    (fedi-post--count-post-chars post-string)
      ;;                                    (number-to-string fedi-post--max-chars))))
      (add-text-properties (car lang-region) (cdr lang-region)
                           (list 'display
                                 (if fedi-post-language
                                     (format "Lang: %s ⋅"
                                             fedi-post-language)
                                   "")))
      (add-text-properties (car nsfw-region) (cdr nsfw-region)
                           (list 'display
                                 (if fedi-post-content-nsfw
                                     "NSFW"
                                   "")
                                 'face 'mastodon-cw-face)))))
      ;; (add-text-properties (car cw-region) (cdr cw-region)
      ;;                      (list 'invisible (not fedi-post--content-warning)
      ;;                            'face 'mastodon-cw-face)))))



;;; PROPERTIZE TAGS AND HANDLES

(defun fedi-post--propertize-tags-and-handles (&rest _args)
  "Propertize tags and handles in post compose buffer.
Added to `after-change-functions'."
  (when (fedi-post--compose-buffer-p)
    (let ((header-region (fedi--find-property-range 'post-post-header
                                                    (point-min)))
          (face nil))
      ;; (face (when fedi-post--proportional-fonts-compose
      ;;         'variable-pitch)))
      ;; cull any prev props:
      ;; stops all text after a handle or mention being propertized:
      (set-text-properties (cdr header-region) (point-max) `(face ,face))
      (fedi-post--propertize-item fedi-post-tag-regex
                                  'success
                                  (cdr header-region))
      (fedi-post--propertize-item fedi-post-handle-regex
                                  'mastodon-display-name-face
                                  (cdr header-region))
      (fedi-post--propertize-item fedi-post-url-regex
                                  'link
                                  (cdr header-region)))))

(defun fedi-post--propertize-item (regex face start)
  "Propertize item matching REGEX with FACE starting from START."
  (save-excursion
    (goto-char start)
    (cl-loop while (search-forward-regexp regex nil :noerror)
             do (add-text-properties (match-beginning 2)
                                     (match-end 2)
                                     `(face ,face)))))

(defun fedi-post--compose-buffer-p ()
  "Return t if compose buffer is current."
  (let ((buf (buffer-name (current-buffer))))
    ;; TODO: generalize:
    (or (equal "*new post*" buf)
        (equal "*edit post*" buf))))

(defun fedi-post--fill-reply-in-compose ()
  "Fill reply text in compose buffer to the width of the divider."
  (save-excursion
    (save-match-data
      (let* ((fill-column 67))
        (goto-char (point-min))
        (when-let ((prop (text-property-search-forward 'post-reply)))
          (fill-region (prop-match-beginning prop)
                       (point)))))))


;;; COMPOSE BUFFER FUNCTION

(defun fedi-post--compose-buffer (&optional edit major minor prefix capf-funs)
  "Create a new buffer to capture text for a new post.
EDIT means we are editing an existing post, not composing a new one.
MODE is the minor-mode to enable in the buffer."
  (let* ((buffer-name (if edit "*edit post*" "*new post*"))
         (buffer-exists (get-buffer buffer-name))
         (buffer (or buffer-exists (get-buffer-create buffer-name)))
         (inhibit-read-only t)
         ;; (reply-text (alist-get 'content
         ;;                        (or (alist-get 'reblog reply-json)
         ;;                            reply-json)))
         (previous-window-config (list (current-window-configuration)
                                       (point-marker))))
    (switch-to-buffer-other-window buffer)
    (if major (funcall major) (text-mode))
    (or (funcall minor)
        (fedi-post-mode t))
    (unless buffer-exists
      (fedi-post--display-docs-and-status-fields minor prefix))
    ;; set up completion:
    (when fedi-post--enable-completion
      (set (make-local-variable 'completion-at-point-functions)
           (cl-loop for f in capf-funs
                    do (cl-pushnew f completion-at-point-functions)
                    finally return completion-at-point-functions))
      ;; company
      (when (and fedi-post--use-company-for-completion
                 (require 'company nil :no-error))
        (declare-function company-mode-on "company")
        (set (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-capf))
        (company-mode-on)))
    ;; after-change:
    (make-local-variable 'after-change-functions)
    ;; (cl-pushnew #'fedi-post--save-post-text after-change-functions)
    (cl-pushnew #'fedi-post--update-status-fields after-change-functions)
    (fedi-post--update-status-fields)
    (cl-pushnew #'fedi-post--propertize-tags-and-handles after-change-functions)
    (fedi-post--propertize-tags-and-handles)
    ;; draft post text saving:
    (setq fedi-post-current-post-text nil)
    ;; if we set this before changing modes, it gets nuked:
    (setq fedi-post-previous-window-config previous-window-config)
    ;; (when initial-text
    ;;   (insert initial-text))
    ))

;; flyspell ignore masto post regexes:
(defvar flyspell-generic-check-word-predicate)

(defun fedi-post-mode-flyspell-verify ()
  "A predicate function for `flyspell'.
Only text that is not one of these faces will be spell-checked."
  (let ((faces '(mastodon-display-name-face
                 fedi-post-docs-face font-lock-comment-face
                 success link)))
    (unless (eql (point) (point-min))
      ;; (point) is next char after the word. Must check one char before.
      (let ((f (get-text-property (1- (point)) 'face)))
        (not (memq f faces))))))

(add-hook 'fedi-post-mode-hook
    	  (lambda ()
            (setq flyspell-generic-check-word-predicate
                  'fedi-post-mode-flyspell-verify)))

;;;###autoload
;; (add-hook 'fedi-post-mode-hook
;;           #'mastodon-profile--fetch-server-account-settings-maybe)

;; disable auto-fill-mode:
(add-hook 'fedi-post-mode-hook
          (lambda ()
            (auto-fill-mode -1)))

(define-minor-mode fedi-post-mode
  "Minor mode for posting to fediverse services."
  :keymap fedi-post-mode-map
  :global nil)

(provide 'fedi-post)
;;; fedi-post.el ends here
