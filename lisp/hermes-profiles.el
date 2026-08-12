;;; hermes-profiles.el --- Profile browser for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Assisted-by: Hermes:MoA
;; Keywords: tools, convenience

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

;; A `tabulated-list' browser over the dashboard REST `/api/profiles':
;; every profile with its configured model and provider.  Profiles can be
;; created, renamed, and deleted through the dashboard REST API; the built-in
;; default profile is protected.  `m' picks a new model from the shared
;; `model.options' catalog and persists it into that profile's own config.yaml
;; via `PUT /api/profiles/{name}/model'.  The Reasoning column is display-only
;; for now: the dashboard exposes no
;; per-profile reasoning read or write route yet; wire it up when
;; hermes-agent grows `GET/PUT /api/profiles/{name}/reasoning'.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'url-util)
(require 'markdown-mode)
(require 'hermes-transport)
(require 'hermes-promise)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)
(require 'hermes-chat)

(defun hermes-profiles--field (profile key)
  "Return PROFILE's KEY as a string, or nil."
  (hermes-transport--scalar-string (hermes-transport--get profile key)))

(defun hermes-profiles--row (profile)
  "Return one `tabulated-list' entry for PROFILE."
  (let ((name (or (hermes-profiles--field profile 'name) "")))
    (list name
          (vector (hermes-browser--face-cell name 'hermes-browser-profile)
                  (hermes-browser--face-cell
                   (if (eq (hermes-transport--get profile 'is_default) t)
                       "*"
                     "")
                   'hermes-browser-default)
                  (hermes-browser--face-cell
                   (or (hermes-profiles--field profile 'model) "")
                   'hermes-browser-model)
                  (hermes-browser--face-cell
                   (or (hermes-profiles--field profile 'provider) "")
                   'hermes-browser-provider)
                  ;; No per-profile reasoning surface in the dashboard yet.
                  (hermes-browser--face-cell "—" 'hermes-browser-reasoning)
                  (hermes-browser--face-cell
                   (or (hermes-profiles--field profile 'description) "")
                   'hermes-browser-description)))))

(defun hermes-profiles--rows (result)
  "Return `tabulated-list' entries for an `/api/profiles' RESULT."
  (mapcar #'hermes-profiles--row
          (hermes-transport--get result 'profiles)))

(defun hermes-profiles--read-model-candidate (catalog)
  "Prompt for a provider-qualified model from `model.options' CATALOG.
Only candidates that carry a provider slug are offered: the profile model
route requires both fields."
  (let* ((candidates (seq-filter
                      (lambda (candidate)
                        (hermes-transport--non-empty-string
                         (plist-get (cdr candidate) :provider)))
                      (hermes-chat--model-candidates catalog)))
         (choice (completing-read "Profile model: "
                                  (mapcar #'car candidates) nil t)))
    (or (cdr (assoc choice candidates))
        (user-error "No model selected"))))

(defun hermes-profiles--put-model (client name candidate)
  "Return a promise setting profile NAME's model to CANDIDATE via CLIENT."
  (hermes-dashboard-transport-api-request-async
   "PUT" (format "/api/profiles/%s/model" (url-hexify-string name))
   :body `((provider . ,(plist-get candidate :provider))
           (model . ,(plist-get candidate :model)))
   :client client))

(defun hermes-profiles--api (client method path &optional body)
  "Return a profile REST METHOD PATH promise through CLIENT with BODY."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/profiles" path) :body body :client client))

(defun hermes-profiles--refresh-after-mutation (origin message)
  "Refresh live profile ORIGIN after reporting MESSAGE."
  (message "Hermes: %s" message)
  (when (hermes-browser--buffer-mode-p origin 'hermes-profiles-mode)
    (with-current-buffer origin (hermes-profiles--revert))))

(defun hermes-profiles--ensure-non-default (name action)
  "Refuse ACTION when profile NAME denotes the built-in default profile."
  (when (string-equal-ignore-case name "default")
    (user-error "Cannot %s the default profile" action)))

(defun hermes-profiles--read-create-arguments ()
  "Read a new profile name and optional clone source."
  (let* ((name (read-string "New profile name: "))
         (profiles (mapcar #'car tabulated-list-entries))
         (clone-from (completing-read "Clone profile (empty for none): "
                                      profiles nil nil)))
    (list name (unless (string-empty-p clone-from) clone-from))))

(defun hermes-profiles-create (name &optional clone-from)
  "Create profile NAME, optionally cloning identity from CLONE-FROM."
  (interactive (hermes-profiles--read-create-arguments))
  (let ((name (string-trim name))
        (clone-from (and clone-from (string-trim clone-from)))
        (origin (current-buffer)))
    (when (string-empty-p name) (user-error "Profile name is required"))
    (hermes-profiles--ensure-non-default name "create")
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-profiles--api
        client "POST" ""
        (append `((name . ,name))
                (and (hermes-transport--non-empty-string clone-from)
                     `((clone_from . ,clone-from))))))
     (lambda (_result)
       (hermes-profiles--refresh-after-mutation origin
                                                (format "created profile %s" name))))))

(defvar-local hermes-profiles-soul-profile nil
  "Profile owned by the current SOUL editor buffer.")

(defvar-keymap hermes-profiles-soul-mode-map
  :parent markdown-mode-map
  "C-c C-c" #'hermes-profiles-soul-save
  "C-c C-k" #'quit-window)

(define-derived-mode hermes-profiles-soul-mode markdown-mode "Hermes SOUL"
  "Edit one Hermes profile's SOUL.md through the dashboard API."
  :interactive nil
  (setq-local header-line-format
              '(:eval (format " Profile: %s  |  C-c C-c save  C-c C-k quit "
                              hermes-profiles-soul-profile))))

(defun hermes-profiles--soul-path (profile)
  "Return the SOUL API path for PROFILE."
  (format "/%s/soul" (url-hexify-string profile)))

(defun hermes-profiles--soul-current-p (buffer generation profile)
  "Return non-nil when BUFFER still owns GENERATION and PROFILE."
  (and (hermes-browser--request-current-mode-p
        buffer generation 'hermes-profiles-soul-mode)
       (with-current-buffer buffer
         (equal hermes-profiles-soul-profile profile))))

(defun hermes-profiles-edit-soul ()
  "Open the selected non-default profile's SOUL.md for editing."
  (interactive)
  (let ((profile (tabulated-list-get-id)))
    (unless profile (user-error "No profile on this line"))
    (hermes-profiles--ensure-non-default profile "edit SOUL for")
    (let ((target (get-buffer-create
                   (format "*Hermes Profile SOUL: %s*" profile))))
      (with-current-buffer target
        (unless (derived-mode-p 'hermes-profiles-soul-mode)
          (hermes-profiles-soul-mode))
        (setq hermes-profiles-soul-profile profile))
      (pop-to-buffer target)
      (unless (with-current-buffer target (buffer-modified-p))
        (let ((generation
               (with-current-buffer target
                 (hermes-browser--next-request-generation))))
          (hermes-browser--run-on-client
           (lambda (client)
             (hermes-profiles--api
              client "GET" (hermes-profiles--soul-path profile)))
           (lambda (result)
             (when (hermes-profiles--soul-current-p
                    target generation profile)
               (with-current-buffer target
                 (unless (buffer-modified-p)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert (or (hermes-profiles--field result 'content) ""))
                     (set-buffer-modified-p nil))))))))))))

(defun hermes-profiles-soul-save ()
  "Save the current profile SOUL editor through the dashboard API."
  (interactive)
  (unless (derived-mode-p 'hermes-profiles-soul-mode)
    (user-error "Not in a Hermes profile SOUL buffer"))
  (let* ((profile hermes-profiles-soul-profile)
         (target (current-buffer))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (generation (hermes-browser--next-request-generation)))
    (unless (hermes-transport--non-empty-string profile)
      (user-error "This SOUL buffer has no profile"))
    (hermes-profiles--ensure-non-default profile "edit SOUL for")
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-profiles--api
        client "PUT" (hermes-profiles--soul-path profile)
        `((content . ,content))))
     (lambda (_result)
       (when (hermes-profiles--soul-current-p target generation profile)
         (with-current-buffer target
           (when (equal content
                        (buffer-substring-no-properties (point-min) (point-max)))
             (set-buffer-modified-p nil)))
         (message "Hermes: saved SOUL for profile %s" profile))))))

(defun hermes-profiles-rename (new-name)
  "Rename the profile at point to NEW-NAME."
  (interactive (list (read-string "Rename profile to: ")))
  (let ((name (tabulated-list-get-id))
        (new-name (string-trim new-name))
        (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (when (string-empty-p new-name) (user-error "Profile name is required"))
    (hermes-profiles--ensure-non-default name "rename")
    (hermes-profiles--ensure-non-default new-name "rename to")
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-profiles--api
        client "PATCH" (concat "/" (url-hexify-string name))
        `((new_name . ,new-name))))
     (lambda (_result)
       (hermes-profiles--refresh-after-mutation
        origin (format "renamed profile %s to %s" name new-name))))))

(defun hermes-profiles-delete ()
  "Delete the profile at point after confirmation."
  (interactive)
  (let ((name (tabulated-list-get-id))
        (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (hermes-profiles--ensure-non-default name "delete")
    (when (yes-or-no-p (format "Delete profile %s? " name))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-profiles--api
          client "DELETE" (concat "/" (url-hexify-string name))))
       (lambda (_result)
         (hermes-profiles--refresh-after-mutation
          origin (format "deleted profile %s" name)))))))

(defun hermes-profiles-set-model ()
  "Set the model of the profile at point, persisted in its configuration."
  (interactive)
  (let ((name (tabulated-list-get-id))
        (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-then
        (hermes-dashboard-transport-call-fn
         #'hermes-dashboard-transport-model-options-cached client)
        (lambda (catalog)
          (hermes-profiles--put-model
           client name (hermes-profiles--read-model-candidate catalog)))))
     (lambda (result)
       (message "Hermes: profile %s set to %s via %s" name
                (hermes-transport--scalar-string
                 (hermes-transport--get result 'model))
                (hermes-transport--scalar-string
                 (hermes-transport--get result 'provider)))
       (when (hermes-browser--buffer-mode-p origin 'hermes-profiles-mode)
         (with-current-buffer origin
           (hermes-profiles--revert)))))))

;;;###autoload (autoload 'hermes-list-profiles "hermes-profiles" nil t)
(hermes-define-list-browser profiles
  :title "Hermes Profiles"
  :buffer "*Hermes Profiles*"
  :command hermes-list-profiles
  :doc "Major mode listing Hermes profiles with their configured runtime."
  :command-doc "Browse Hermes profiles and manage their lifecycle and model."
  :columns [("Profile" 16 t) ("Default" 7 t) ("Model" 28 t)
            ("Provider" 14 t) ("Reasoning" 9 t) ("Description" 40 nil)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-profile-list-async client))
  :rows #'hermes-profiles--rows
  :keys ("m" #'hermes-profiles-set-model
         "RET" #'hermes-profiles-set-model
         "s" #'hermes-profiles-edit-soul
         "c" #'hermes-profiles-create
         "r" #'hermes-profiles-rename
         "D" #'hermes-profiles-delete))

(provide 'hermes-profiles)
;;; hermes-profiles.el ends here
