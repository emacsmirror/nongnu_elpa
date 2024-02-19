;;; fedi-post.el --- Minor mode for posting to fediverse services  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Marty Hiatt
;; Author: Marty Hiatt <martianhiatus@riseup.net> and mastodon.el authors
;; Version: 1.0.0
;; Homepage: https://codeberg.org/martianh/fedi.el

;; This file is not part of GNU Emacs.

;; This file is part of fedi.el.

;; fedi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fedi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fedi.el.  If not, see <http://www.gnu.org/licenses/>.

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
;; (require 'persist)
(require 'facemenu)
(require 'text-property-search)
(require 'markdown-mode)

(require 'fedi-iso)

(autoload 'iso8601-parse "iso8601")
(autoload 'fedi--find-property-range "fedi")
(autoload 'fedi--find-property-range "fedi")
(autoload 'org-read-date "org")

(defface fedi-post-docs-face
  `((t :inherit font-lock-comment-face))
  "Face used for documentation in post compose buffer.")

(defface fedi-post-success-face
  `((t :inherit success))
  "Face used for some status fields in post compose buffer.")

(defgroup fedi-post nil
  "Posting options for fedi.el."
  :prefix "fedi-post-"
  :group 'fedi)

(defcustom fedi-post--enable-completion t
  "Whether to enable completion of mentions and hashtags.
Used for completion in post compose buffer."
  :type 'boolean)

(defcustom fedi-post--use-company-for-completion nil
  "Whether to enable company for completion.
When non-nil, `company-mode' is enabled in the post compose
buffer, and fedi or your package's completion backends are added to
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

(defvar fedi-post-draft-posts-list nil
  "A list of posts that have been saved as drafts.
For the moment we just put all composed posts in here, as we want
to also capture posts that are `sent' but that don't successfully
send.")

(defvar-local fedi-post-status-fields-items nil
  "A list of alists of information about each status field to be updated.
Each alist should contain the keys name, item-var, prop, and
face. no-label is optional.")


;;; REGEXES

(defvar fedi-post-handle-regex
  (rx (| (any ?\( "\n" "\t "" ") bol) ; preceding things
      (group-n 1 ; = handle with @
        (+ ; breaks groups with instance handles!
         ?@
         (group-n 2 ; = username only
           (* (any ?- ?_ ?. "A-Z" "a-z" "0-9" )))
         (? ?@
            (group-n 3 ; = optional domain
              (* (not (any "\n" "\t" " ")))))))
      (| "'" word-boundary)) ; boundary or possessive
  "Regex for a handle, e.g. @user@instance.com.
Group 1 is for completion at point functions. Group 2 and 3 are
for forming a URL.")

(defvar fedi-post-tag-regex
  (rx (| (any ?\( "\n" "\t" " ") bol)
      (group-n 1 ?# (+ (any "A-Z" "a-z" "0-9")))
      (| "'" word-boundary)) ; boundary or possessive
  "Regex for a tag. Group 1 is for completion at point functions.")

(defvar fedi-post-url-regex
  ;; adapted from `ffap-url-regexp'
  (concat
   "\\(?1:\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)"
                                        ; uri prefix
   "[^ \n\t]*\\)" ; any old thing, that is, i.e. we allow invalid/unwise chars
   "\\b"))
                                        ; boundary


;;; MODE MAP

(defvar fedi-post-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") #'fedi-post-send)
    (define-key map (kbd "C-c C-k") #'fedi-post-cancel)
    (define-key map (kbd "C-c C-n") #'fedi-post-toggle-nsfw)
    ;; (when (require 'emojify nil :noerror)
    ;; (define-key map (kbd "C-c C-e") #'fedi-post-insert-emoji))
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
    ;; (kill-buffer-and-window)
    (quit-window 'kill)
    (fedi-post--restore-previous-window-config prev-window-config)))

(defun fedi-post-cancel ()
  "Kill new-post buffer/window. Does not POST content.
If post is not empty, prompt to save text as a draft."
  (interactive)
  (if (fedi-post--empty-p)
      (fedi-post-kill)
    ;; (when (y-or-n-p "Save draft post?")
    ;; (fedi-post--save-draft))
    (fedi-post-kill)))

(defun fedi-post--clean-tabs-and-nl (string)
  "Remove tabs and newlines from STRING."
  (replace-regexp-in-string "[\t\n ]*\\'" "" string))

(defun fedi-post--empty-p ()
  "Return t if post has no text, attachments, or polls."
  (string-empty-p (fedi-post--clean-tabs-and-nl
                   (fedi-post--remove-docs))))

(defun fedi-post--remove-docs ()
  "Get the body of a post from the current compose buffer."
  (let ((header-region (fedi--find-property-range 'post-header
                                                  (point-min))))
    (buffer-substring (cdr header-region) (point-max))))

(defun fedi-post--restore-previous-window-config (config)
  "Restore the window CONFIG after killing the post compose buffer.
Buffer-local variable `fedi-post-previous-window-config' holds the config."
  (set-window-configuration (car config))
  (goto-char (cadr config)))


;;; COMPLETION (TAGS, MENTIONS)

(defun fedi-post--get-bounds (regex)
  "Get bounds of item before point using REGEX."
  ;; # and @ are not part of any existing thing at point
  (save-match-data
    (save-excursion
      ;; match full handle inc. domain, or tag including #
      ;; (see the regexes for subexp 1)
      (when (re-search-backward regex
                                (save-excursion (forward-whitespace -1)
                                                (point))
                                :no-error)
        (cons (match-beginning 1)
              (match-end 1))))))

(defun fedi-post--return-capf (regex completion-fun &optional
                                     annot-fun _affix-fun exit-fun)
  "Return a completion at point function.
REGEX is used to get the item before point.
COMPLETION-FUN takes two args, start and end bounds of item
before point, and returns a completion table.
ANNOT-FUN takes one arg, a candidate, and returns an annotation
for it.
AFFIX-FUN is currently unused, it would be :affixation-function.
EXIT-FUN is :exit-function for capfs, it takes two args: a string
and a status."
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
                                  fedi-iso-639-1)))
    (setq fedi-post-language
          (alist-get choice fedi-iso-639-1 nil nil 'equal))
    (message "Language set to %s" choice)
    (fedi-post--update-status-fields)))

;; (defun fedi-post--iso-to-human (ts)
;;   "Format an ISO8601 timestamp TS to be more human-readable."
;;   (let* ((decoded (iso8601-parse ts))
;;          (encoded (encode-time decoded)))
;;     (format-time-string "%d-%m-%y, %H:%M[%z]" encoded)))

;; (defun fedi-post--iso-to-org (ts)
;;   "Convert ISO8601 timestamp TS to something `org-read-date' can handle."
;;   (when ts (let* ((decoded (iso8601-parse ts)))
;;              (encode-time decoded))))


;;; DISPLAY KEYBINDINGS

(defun fedi-post--get-mode-kbinds (&optional mode-map)
  "Get a list of the keybindings in MODE-MAP or `fedi-post-mode'."
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
e.g. fedi-post-send -> Send.
PREFIX is a string corresponding to the prefix of the minor mode
enabled."
  (let* ((str (symbol-name cmd))
         (re (concat prefix
                     "-\\(.*\\)$"))
         (str2 (save-match-data
                 (string-match re str)
                 (match-string 1 str))))
    (capitalize (replace-regexp-in-string "-" " " str2))))

(defun fedi-post--format-kbind (kbind &optional prefix)
  "Format a single keybinding, KBIND, for display in documentation.
PREFIX is a string corresponding to the prefix of the minor mode
enabled. It is used for constructing clean keybinding
descriptions."
  (let ((key (concat "\\`"
                     (help-key-description (car kbind) nil)
                     "'"))
        (command (fedi-post--format-kbind-command (cdr kbind) prefix)))
    (substitute-command-keys
     (format
      (concat (fedi-post-comment "    ")
              "%s"
              (fedi-post-comment " - %s"))
      key command))))

(defun fedi-post--format-kbinds (kbinds &optional prefix)
  "Format a list of keybindings, KBINDS, for display in documentation.
PREFIX is a string corresponding to the prefix of the minor mode
enabled. It is used for constructing clean keybinding
descriptions."
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

(defun fedi-post-comment (str)
  "Propertize STR with `fedi-post-docs-face'."
  (propertize str
              'face 'fedi-post-docs-face))

(defun fedi-post--make-mode-docs (&optional mode prefix type)
  "Create formatted documentation text for MODE or fedi-post-mode.
PREFIX is a string corresponding to the prefix of the minor mode
enabled. It is used for constructing clean keybinding
descriptions."
  (let* ((mode-map (alist-get mode minor-mode-map-alist))
         (prefix (or prefix (string-remove-suffix "-mode"
                                                  (symbol-name mode))))
         (kbinds (fedi-post--get-mode-kbinds mode-map))
         (longest-kbind (fedi-post--formatted-kbinds-longest
                         (fedi-post--format-kbinds kbinds prefix))))
    (concat
     (fedi-post-comment
      (format
       " Compose a new %s here. The following keybindings are available:" type))
     (mapconcat #'identity
                (fedi-post--formatted-kbinds-pairs
                 (fedi-post--format-kbinds kbinds prefix)
                 longest-kbind)
                nil))))

(defun fedi-post--concat-fields (fields)
  "Concat FIELDS for compose buffer docs."
  (cl-loop for item in fields
           for field = (alist-get 'name item)
           concat (propertize (capitalize field)
                              (intern
                               (concat "post-" (downcase field)))
                              t)))

(defun fedi-post--display-docs-and-status-fields (&optional mode prefix
                                                            fields type)
  "Insert propertized text with documentation about MODE or `fedi-post-mode'.
Also includes and the status fields which will get updated based
on the status of NSFW, language, media attachments, etc.
PREFIX is a string corresponding to the prefix of the minor mode
enabled. It is used for constructing clean keybinding
descriptions.
FIELDS is a list of alists of fields to add, using `fedi-post--concat-fields'."
  (let ((divider
         "|=================================================================|"))
    (insert
     (concat
      (fedi-post--make-mode-docs mode prefix type) "\n"
      (fedi-post-comment divider) "\n"
      (propertize
       (concat
        " "
        ;; (propertize "Count"
        ;; 'post-post-counter t)
        ;; " ⋅ "
        (if fields
            (concat (fedi-post--concat-fields fields)
                    "\n ")
          "")
        (propertize "Language"
                    'post-language t)
        " "
        (propertize "NSFW"
                    'post-nsfw t)
        "\n"
        divider
        "\n")
       'rear-nonsticky t
       'face 'fedi-post-docs-face
       'read-only "Edit your message below."
       'post-header t)))))

(defun fedi-post--count-post-chars (post-string)
  "Count the characters in POST-STRING."
  ;; URLs always = 23, and domain names of handles are not counted.
  ;; This is how mastodon does it."
  (with-temp-buffer
    ;; (switch-to-buffer (current-buffer))
    (insert post-string)
    (goto-char (point-min))
    (length (buffer-substring (point-min) (point-max)))))

(defun fedi-post--update-status-field (item)
  "ITEM."
  (let-alist item
    (let ((region (fedi--find-property-range .prop (point-min))))
      (add-text-properties (car region) (cdr region)
                           (list 'display
                                 (if (eval .item-var)
                                     (format
                                      (concat
                                       (if .no-label
                                           ""
                                         (concat (capitalize .name)
                                                 ": "))
                                       (propertize "%s"
                                                   'face .face)
                                       " ⋅ ")
                                      (eval .item-var))
                                   "")
                                 'face 'fedi-post-docs-face)))))

(defun fedi-post--update-status-fields (&rest _args)
  "Update the status fields in the header based on the current state."
  (ignore-errors ; called from `after-change-functions' so let's not leak errors
    (let* ((inhibit-read-only t)
           ;; (header-region (fedi--find-property-range 'post-header
           ;;                                           (point-min)))
           (nsfw-region (fedi--find-property-range 'post-nsfw
                                                   (point-min)))
           (lang-region (fedi--find-property-range 'post-language
                                                   (point-min))))
      ;; (post-string (buffer-substring-no-properties (cdr header-region)
      ;;                                              (point-max))))
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
                                 'face 'fedi-post-success-face))
      (cl-loop for item in fedi-post-status-fields-items
               do (fedi-post--update-status-field item)))))


;;; PROPERTIZE TAGS AND HANDLES

(defun fedi-post--propertize-tags-and-handles (&rest _args)
  "Propertize tags and handles in post compose buffer.
Added to `after-change-functions'."
  (when (fedi-post--compose-buffer-p)
    (let ((header-region (fedi--find-property-range 'post-header
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
                                  'warning
                                  (cdr header-region))
      (fedi-post--propertize-item fedi-post-url-regex
                                  'link
                                  (cdr header-region)))))

(defun fedi-post--propertize-item (regex face start)
  "Propertize item matching REGEX with FACE starting from START."
  (save-excursion
    (goto-char start)
    (cl-loop while (search-forward-regexp regex nil :noerror)
             do (add-text-properties (match-beginning 1)
                                     (match-end 1)
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

(defun fedi-post--compose-buffer
    (&optional edit major minor prefix type capf-funs fields)
  "Create a new buffer to capture text for a new post.
EDIT means we are editing an existing post, not composing a new one.
MAJOR is the major mode to enable.
MINOR is the minor mode to enable.
PREFIX is a string corresponding to the prefix of the library
that contains the compose buffer's functions. It is only required
if this differs from the minor mode.
CAPF-FUNS is a list of functions to enable.
TYPE is a string for the buffer name.
FIELDS is a list of alists containing status fields for bindings
and options display. Each alist should have a name, prop,
item-var and face elements. Element name should be a hyphen-separated
string, the other elements should be symbols."
  (let* ((buffer-name (if edit
                          (format "*edit %s*" type)
                        (format "*new %s*" type)))
         (buffer-exists (get-buffer buffer-name))
         (buffer (or buffer-exists (get-buffer-create buffer-name)))
         (inhibit-read-only t)
         (previous-window-config (list (current-window-configuration)
                                       (point-marker))))
    (switch-to-buffer-other-window buffer)
    ;; `markdown-mode' here breaks any existing docs display:
    (if major
        (unless (eq major major-mode)
          (funcall major))
      (text-mode))
    (or (funcall minor)
        (fedi-post-mode t))
    (when (eq major 'markdown-mode)
      ;; disable fontifying as it breaks our docs (we fontify by region below)
      (unless buffer-exists (font-lock-mode -1))
      (when (member 'variable-pitch-mode markdown-mode-hook)
        ;; (make-local-variable 'markdown-mode-hook) ; unneeded if we always disable?
        ;; (setq markdown-mode-hook (delete 'variable-pitch-mode markdown-mode-hook))
        (variable-pitch-mode -1)))
    (unless buffer-exists
      (fedi-post--display-docs-and-status-fields minor prefix fields type))
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
    (when fields
      (setq fedi-post-status-fields-items fields))
    (fedi-post--update-status-fields)
    ;; disable for markdown-mode:
    (unless (eq major 'markdown-mode)
      (cl-pushnew #'fedi-post--propertize-tags-and-handles
                  after-change-functions)
      (fedi-post--propertize-tags-and-handles))
    ;; draft post text saving:
    (setq fedi-post-current-post-text nil)
    ;; if we set this before changing modes, it gets nuked:
    (setq fedi-post-previous-window-config previous-window-config)
    ;; markdown fontify region:
    ;; FIXME: this is incompat with propertize-tags-and-handles
    ;; we would need to add our own propertizing to md-mode font-locking
    (when (eq major 'markdown-mode)
      (cl-pushnew #'fedi-post-fontify-body-region after-change-functions))))

(defun fedi-post-fontify-body-region (&rest _args)
  "Call `font-lock-fontify-region' on post body.
Added to `after-change-functions' as we disable markdown-mode's
font locking to not ruin our docs header."
  (save-excursion
    (let ((end-of-docs (cdr (fedi--find-property-range 'post-header
                                                       (point-min)))))
      (font-lock-fontify-region end-of-docs (point-max)))))

;; flyspell ignore our post regexes:
(defvar flyspell-generic-check-word-predicate)

(defun fedi-post-mode-flyspell-verify ()
  "A predicate function for `flyspell'.
Only text that is not one of these faces will be spell-checked."
  (let ((faces '(warning
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
