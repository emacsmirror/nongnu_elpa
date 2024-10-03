;;; mastodon-transient.el --- transient menus for mastodon.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  martian hiatus

;; Author: martian hiatus <martianhiatus@riseup.net>
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

(require 'tp)

;;; UTILS

(defun mastodon-transient-parse-source-key (key)
  "Parse mastodon source KEY.
If KEY needs to be source[key], format like so, else just return
the inner key part."
  (let* ((split (split-string key "[][]"))
         (array-key (cadr split)))
    (if (or (= 1 (length split)) ;; no split
            (member array-key '("privacy" "sensitive" "language")))
        key
      array-key)))

(defun mastodon-transient-parse-source-keys (alist)
  "Parse ALIST containing source[key] keys."
  (cl-loop for a in alist
           collect (cons (mastodon-transient-parse-source-key (car a))
                         (cdr a))))

(defun mastodon-transient-get-creds ()
  "Fetch account data."
  (mastodon-http--get-json
   (mastodon-http--api "accounts/verify_credentials")
   nil :silent))

;; fields utils:
;; to PATCH fields, we just need fields[x][name] and fields[x][value]

(defun mastodon-transient-fields-to-transient (fields)
  "Convert fields in FIELDS to transient key=val args."
  (flatten-tree
   (cl-loop
    for f in fields
    for count from 1 to 5
    collect
    (cl-loop for x in f
             collect
             (concat "fields." (number-to-string count)
                     "." (symbol-name (car x))
                     "=" (cdr x))))))

(defun mastodon-transient-field-dot-to-array (key)
  "Convert KEY from tp dot annotation to array[key] annotation."
  (let* ((split (split-string key "\\."))
         (count (length split)))
    (cond ((not key) nil)
          ((= 1 count) key)
          (t
           (concat ;; first item
            (car split)
            "_attributes"
            ;; but last item
            "[" (car (last split (1- count)))
            ;; last item:
            "][" (car (last split)) "]")))))

(defun mastodon-transient-dot-fields-to-arrays (alist)
  "Parse fields ALIST in dot notation to array notation."
  (cl-loop for y in alist
           collect
           (cons (mastodon-transient-field-dot-to-array (car y))
                 (cdr y))))

;;; TRANSIENTS

;; FIXME: PATCHing source vals as JSON request body doesn't work! existing
;; `mastodon-profile--update-preference' doesn't use it! it just uses
;; query params! strange thing is it works for non-source params
(transient-define-suffix mastodon-user-settings-update (&optional args)
  "Update current user settings on the server."
  :transient 'transient--do-exit
  ;; interactive receives args from the prefix:
  (interactive (list (transient-args 'mastodon-user-settings)))
  (let* ((alist (tp-transient-to-alist args))
         (only-changed (tp-only-changed-args alist))
         (arrays (tp-dots-to-arrays only-changed))
         (parsed-source (mastodon-transient-parse-source-keys arrays))
         (endpoint "accounts/update_credentials")
         (url (mastodon-http--api endpoint))
         (resp (mastodon-http--patch url parsed-source))) ; :json)))
    (mastodon-http--triage
     resp
     (lambda (_)
       (message "Settings updated!\n%s" (pp-to-string parsed-source))))))

(transient-define-prefix mastodon-user-settings ()
  "A transient for setting current user settings."
  :value (lambda () (tp-return-data
                     #'mastodon-transient-get-creds))
  [:description
   (lambda ()
     (format "User settings for %s" mastodon-active-user))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["Account info"
   ("n" "display name" "display_name=" :class tp-option-str)
   ("t" "update profile note" mastodon-update-profile-note)
   ("f" "update profile fields" mastodon-profile-fields)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["Account options"
   ("l" "locked" "locked=" :class tp-bool)
   ("b" "bot" "bot=" :class tp-bool)
   ("d"  "discoverable" "discoverable=" :class tp-bool)
   ("c" "hide follower/following lists" "source.hide_collections=" :class tp-bool)
   ("i" "indexable" "source.indexable=" :class tp-bool)]
  ["Tooting options"
   ("p" "default privacy" "source.privacy=" :class tp-option
    :choices mastodon-toot-visibility-settings-list)
   ;; ("public" "unlisted" "private"))
   ;; (lambda () mastodon-toot-visibility-settings-list))
   ("s" "mark sensitive" "source.sensitive=" :class tp-bool)
   ("g" "default language" "source.language=" :class tp-option
    :choices mastodon-iso-639-regional)]
  ["Update"
   ("C-c C-c" "Save settings" mastodon-user-settings-update)
   ("C-c C-k" :info "Revert all changes")]
  (interactive)
  (if (not mastodon-active-user)
      (user-error "User not set")
    (transient-setup 'mastodon-user-settings)))

(transient-define-suffix mastodon-update-profile-note ()
  "Update current user profile note."
  :transient 'transient--do-exit
  (interactive)
  (mastodon-profile--update-user-profile-note))

(transient-define-suffix mastodon-profile-fields-update (args)
  "Update current user profile fields."
  :transient 'transient--do-exit
  (interactive (list (transient-args 'mastodon-profile-fields)))
  (let* ((alist (tp-transient-to-alist args))
         ;; FIXME: maybe only changed also won't work with fields, as
         ;; perhaps what is PATCHed overwrites whatever is on the server?
         ;; (only-changed (tp-only-changed-args alist))
         (arrays (mastodon-transient-dot-fields-to-arrays alist))
         (endpoint "accounts/update_credentials")
         (url (mastodon-http--api endpoint))
         (resp (mastodon-http--patch url arrays))) ; :json)))
    (mastodon-http--triage
     resp
     (lambda (_)
       (message "Fields updated!\n%s" (pp-to-string arrays))))))

(transient-define-prefix mastodon-profile-fields ()
  "A transient for setting profile fields."
  :value
  (lambda ()
    (tp-return-data #'mastodon-transient-get-creds nil 'fields)
    (setq tp-settings-as-transient
          (mastodon-transient-fields-to-transient
           tp-server-settings)))
  [:description
   "Fields"
   ["Name"
    ("1 n" "" "fields.1.name=" :class mastodon-transient-field)
    ("2 n" "" "fields.2.name=" :class mastodon-transient-field)
    ("3 n" "" "fields.3.name=" :class mastodon-transient-field)
    ("4 n" "" "fields.4.name=" :class mastodon-transient-field)]
   ["Value"
    ("1 v" "" "fields.1.value=" :class mastodon-transient-field)
    ("2 v" "" "fields.2.value=" :class mastodon-transient-field)
    ("3 v" "" "fields.3.value=" :class mastodon-transient-field)
    ("4 v" "" "fields.4.value=" :class mastodon-transient-field)]]
  ["Update"
   ("C-c C-c" "Save settings" mastodon-profile-fields-update)
   ("C-c C-k" :info "Revert all changes")]
  (interactive)
  (if (not mastodon-active-user)
      (user-error "User not set")
    (transient-setup 'mastodon-profile-fields)))

;;; CLASSES

(defclass mastodon-transient-field (tp-option-str)
  ((always-read :initarg :always-read :initform t))
  "An infix option class for our options.
We always read.")

(defun mastodon-transient-field-changed-p (value key num)
  "T if VALUE is not equal corresponding value in `tp-server-settings'.
The latter is fetched from alist number NUM, using KEY, a symbol."
  (let ((elt (nth num) tp-server-settings))
    (not (equal value (alist-get key elt)))))

(cl-defmethod transient-format-value ((obj mastodon-transient-field))
  "Format the value of OBJ.
Format should just be a string, highlighted green if it has been
changed from the server value."
  (let* ((pair (transient-infix-value obj))
         (arg (oref obj argument))
         (value (when pair (cadr (split-string pair "="))))
         (split (split-string arg "\\."))
         (num (1- (string-to-number (nth 1 split))))
         (key (intern (substring (nth 2 split) nil -1))))
    (if (not pair)
        ""
      (propertize value
                  'face (if (mastodon-transient-field-changed-p value key num)
                            'transient-value
                          'transient-inactive-value)))))

(provide 'mastodon-transient)
;;; mastodon-transient.el ends here
