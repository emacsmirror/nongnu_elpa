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

(require 'hermes-buffer)
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

(declare-function hermes-cron-for-profile "hermes-cron" (profile instance))

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

(defun hermes-profiles--put-model (client name candidate &optional current-p)
  "Set profile NAME's model to CANDIDATE via CLIENT under CURRENT-P."
  (hermes-dashboard-transport-api-request-async
   "PUT" (format "/api/profiles/%s/model" (url-hexify-string name))
   :body `((provider . ,(plist-get candidate :provider))
           (model . ,(plist-get candidate :model)))
   :client client :current-p current-p))

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
  (let* ((profiles (mapcar #'car tabulated-list-entries))
         (name (read-string "New profile name: "))
         (clone-from (completing-read "Clone profile (empty for none): "
                                      profiles nil nil)))
    (list name (unless (string-empty-p clone-from) clone-from))))

(defun hermes-profiles-create (name &optional clone-from)
  "Create profile NAME, optionally cloning identity from CLONE-FROM."
  (interactive
   (hermes-browser--read-owned-arguments #'hermes-profiles--read-create-arguments))
  (let ((name (string-trim name))
        (clone-from (and clone-from (string-trim clone-from)))
        (origin (current-buffer)))
    (when (string-empty-p name) (user-error "Profile name is required"))
    (hermes-profiles--ensure-non-default name "create")
    (hermes-browser--run-owned
     (lambda (client _guard)
       (hermes-profiles--api
        client "POST" ""
        (append `((name . ,name))
                (and (hermes-transport--non-empty-string clone-from)
                     `((clone_from . ,clone-from))))))
     (hermes-browser--mutation-context)
     (lambda (_result)
       (hermes-profiles--refresh-after-mutation origin
                                                (format "created profile %s" name)))
     #'hermes-browser--read-error)))

(defvar-local hermes-profiles-soul-profile nil
  "Profile owned by the current SOUL editor buffer.")

(defvar-local hermes-profiles--soul-save-pending nil
  "Request token of the pending SOUL save, or nil.
Reject another save until settlement rather than race remote writes.")

(defvar-keymap hermes-profiles-soul-mode-map
  :parent markdown-mode-map
  "C-c C-c" #'hermes-profiles-soul-save
  "C-c C-k" #'quit-window)

(define-derived-mode hermes-profiles-soul-mode markdown-mode "Hermes SOUL"
  "Edit one Hermes profile's SOUL.md through the dashboard API."
  :interactive nil
  (setq-local header-line-format
              '(:eval (hermes-profiles--soul-header-line))))

(defun hermes-profiles--soul-header-line ()
  "Return the SOUL editor header for its profile and instance."
  (concat (or (hermes-browser--instance-header-line) "")
          (format " Profile: %s  |  C-c C-c save  C-c C-k quit "
                  hermes-profiles-soul-profile)))

(defun hermes-profiles--soul-buffer-name (profile instance)
  "Return the SOUL editor buffer name for PROFILE on INSTANCE."
  (if (hermes-instance-multiple-p)
      (format "*Hermes Profile SOUL@%s: %s*"
              (hermes-instance-name instance) profile)
    (format "*Hermes Profile SOUL: %s*" profile)))

(defun hermes-profiles--soul-path (profile)
  "Return the SOUL API path for PROFILE."
  (format "/%s/soul" (url-hexify-string profile)))

(defun hermes-profiles--soul-buffer (profile instance)
  "Return the editor owned by PROFILE and INSTANCE, creating one if needed."
  (or (seq-find
       (lambda (buffer)
         (with-current-buffer buffer
           (and (hermes-buffer--owned-p 'hermes-profiles-soul-mode)
                (equal hermes-profiles-soul-profile profile)
                (equal hermes-instance instance))))
       (buffer-list))
      (let ((buffer (generate-new-buffer
                     (hermes-profiles--soul-buffer-name profile instance))))
        (with-current-buffer buffer
          (hermes-profiles-soul-mode)
          (hermes-buffer--claim 'hermes-profiles-soul-mode)
          (setq hermes-instance (copy-tree instance t)
                hermes-profiles-soul-profile (copy-sequence profile)))
        buffer)))

(defun hermes-profiles--soul-current-p (buffer generation profile instance)
  "Return non-nil when BUFFER still owns GENERATION, PROFILE and INSTANCE."
  (and (hermes-browser--request-current-mode-p
        buffer generation 'hermes-profiles-soul-mode)
       (with-current-buffer buffer
         (and (equal hermes-profiles-soul-profile profile)
              (equal hermes-instance instance)))))

(defun hermes-profiles-edit-soul ()
  "Open the selected non-default profile's SOUL.md for editing."
  (interactive nil hermes-profiles-mode)
  (let ((instance (hermes-instance-resolve))
        (profile (tabulated-list-get-id)))
    (unless profile (user-error "No profile on this line"))
    (hermes-profiles--ensure-non-default profile "edit SOUL for")
    (let ((target (hermes-profiles--soul-buffer profile instance)))
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
                    target generation profile instance)
               (with-current-buffer target
                 (unless (buffer-modified-p)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert (or (hermes-profiles--field result 'content) ""))
                     (set-buffer-modified-p nil))))))))))))

(defun hermes-profiles--save-soul (profile content tick current-p)
  "Save PROFILE's CONTENT captured at TICK while CURRENT-P owns this editor."
  (let ((target (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-profiles--api
        client "PUT" (hermes-profiles--soul-path profile) `((content . ,content))))
     (lambda (_result)
       (when (funcall current-p)
         (with-current-buffer target
           (when (= tick (buffer-chars-modified-tick))
             (set-buffer-modified-p nil)))
         (message "Hermes: saved SOUL for profile %s" profile)))
     (lambda (reason)
       (when (funcall current-p) (message "Hermes: %s" reason))))))

(defun hermes-profiles-soul-save ()
  "Save the current profile SOUL editor through the dashboard API.
Reject unavailable owners and overlapping saves without discarding the draft.
Keep edits made during a save modified."
  (interactive nil hermes-profiles-soul-mode)
  (unless (derived-mode-p 'hermes-profiles-soul-mode)
    (user-error "Not in a Hermes profile SOUL buffer"))
  (when hermes-profiles--soul-save-pending
    (user-error "SOUL save in progress; draft retained, save again after completion"))
  (let* ((profile hermes-profiles-soul-profile)
         (target (current-buffer))
         (instance hermes-instance)
         (tick (buffer-chars-modified-tick))
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (unless (and (hermes-instance--valid-p instance)
                 (member instance (hermes-instance-configured)))
      (user-error "SOUL instance unavailable; draft retained"))
    (unless (hermes-transport--non-empty-string profile)
      (user-error "This SOUL buffer has no profile"))
    (hermes-profiles--ensure-non-default profile "edit SOUL for")
    (let* ((generation (hermes-browser--next-request-generation))
           (current-p
            (lambda ()
              (and (hermes-profiles--soul-current-p
                    target generation profile instance)
                   (with-current-buffer target
                     (eq hermes-profiles--soul-save-pending generation)))))
           (release
            (lambda ()
              (when (buffer-live-p target)
                (with-current-buffer target
                  (when (eq hermes-profiles--soul-save-pending generation)
                    (setq hermes-profiles--soul-save-pending nil))))))
           dispatched)
      (setq hermes-profiles--soul-save-pending generation)
      (unwind-protect
          (prog1
              (hermes--promise-finally
               (hermes-profiles--save-soul profile content tick current-p)
               release)
            (setq dispatched t))
        ;; Client acquisition can signal before it returns a promise.
        (unless dispatched (funcall release))))))

(defun hermes-profiles-rename (new-name)
  "Rename the profile at point to NEW-NAME."
  (interactive
   (hermes-browser--read-owned-arguments
    (lambda () (list (read-string "Rename profile to: ")))
    #'tabulated-list-get-id) hermes-profiles-mode)
  (let ((name (tabulated-list-get-id))
        (new-name (string-trim new-name))
        (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (when (string-empty-p new-name) (user-error "Profile name is required"))
    (hermes-profiles--ensure-non-default name "rename")
    (hermes-profiles--ensure-non-default new-name "rename to")
    (hermes-browser--run-owned
     (lambda (client _guard)
       (hermes-profiles--api
        client "PATCH" (concat "/" (url-hexify-string name))
        `((new_name . ,new-name))))
     (hermes-browser--mutation-context #'tabulated-list-get-id)
     (lambda (_result)
       (hermes-profiles--refresh-after-mutation
        origin (format "renamed profile %s to %s" name new-name)))
     #'hermes-browser--read-error)))

(defun hermes-profiles-delete ()
  "Delete the profile at point after confirmation."
  (interactive nil hermes-profiles-mode)
  (let* ((name (tabulated-list-get-id))
         (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (hermes-profiles--ensure-non-default name "delete")
    (let ((current (hermes-browser--mutation-context #'tabulated-list-get-id)))
      (when (and (yes-or-no-p (format "Delete profile %s? " name))
                 (funcall current))
        (with-current-buffer origin
          (hermes-browser--run-owned
           (lambda (client _guard)
             (hermes-profiles--api
              client "DELETE" (concat "/" (url-hexify-string name))))
           current
           (lambda (_result)
             (hermes-profiles--refresh-after-mutation
              origin (format "deleted profile %s" name)))
           #'hermes-browser--read-error))))))

(defun hermes-profiles-set-model ()
  "Set the profile model using fresh choices from its owning dashboard.
Persist the provider and model in the profile configuration, then read it back.
If the catalogue fetch fails, do not offer stale cached choices."
  (interactive nil hermes-profiles-mode)
  (let ((name (tabulated-list-get-id))
        (origin (current-buffer)))
    (unless name (user-error "No profile on this line"))
    (hermes-browser--run-owned
     (lambda (client guard)
       (hermes--promise-then
        (hermes--promise-then
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-model-options-cached client :force t)
         (lambda (catalog)
           (when (funcall guard)
             (with-current-buffer origin
               (let ((candidate (hermes-profiles--read-model-candidate catalog)))
                 (when (funcall guard)
                   (hermes-profiles--put-model client name candidate guard)))))))
        (lambda (_receipt)
          (hermes--promise-map
           (hermes-dashboard-transport-api-request-async
            "GET" "/api/profiles" :client client :current-p guard)
           (lambda (result)
             (when (funcall guard)
               (hermes-dashboard-transport--store-profile-cache
                result (hermes-dashboard-transport--cache-base-url client)))
             result)))))
     (hermes-browser--mutation-context #'tabulated-list-get-id)
     (lambda (result)
       (hermes-profiles--render result)
       (setq hermes-browser--status "Ready")
       (message "Hermes: read back model for profile %s" name))
     #'hermes-browser--read-error)))

;;; Canonical conversations

(defun hermes-profiles--bot-registry (rpc profile)
  "Read PROFILE's canonical registry via guarded RPC."
  (hermes--promise-map
   (funcall rpc #'hermes-dashboard-transport-profiles-list :include-sessions t)
   (lambda (result)
     (unless (eq t (hermes-transport--get result 'bot_mode_protocol))
       (user-error "Backend does not advertise canonical Bot Chats"))
     (let ((rows (seq-filter
                  (lambda (row) (equal profile (hermes-transport--get row 'name)))
                  (hermes-transport--get result 'profiles))))
       (unless (= 1 (length rows))
         (user-error "Profile is missing or ambiguous: %s" profile))
       (let ((canonical (hermes-transport--get (car rows) 'canonical_session)))
         (cond ((eq canonical :json-null) nil)
               ((and (hermes-transport--object-p canonical)
                     (hermes-transport--non-empty-string
                      (hermes-transport--get canonical 'id))) canonical)
               (t (user-error "Missing canonical registry evidence"))))))))

(defun hermes-profiles--bot-row (result)
  "Validate exact-title lookup RESULT and return its unique row, or nil."
  (unless (hermes-transport--field-present-p result 'sessions)
    (user-error "Missing Bot Chat lookup result"))
  (let ((rows (hermes-transport--get result 'sessions)))
    (unless (and (vectorp rows) (<= (length rows) 1))
      (user-error "Ambiguous Bot Chat lookup"))
    (when (> (length rows) 0)
      (unless (and (equal "Bot Chat" (hermes-transport--get (aref rows 0) 'title))
                   (hermes-transport--non-empty-string
                    (hermes-transport--get (aref rows 0) 'id))
                   (hermes-transport--non-empty-string
                    (hermes-transport--get (aref rows 0) 'resolved_id)))
        (user-error "Incomplete Bot Chat identity"))
      (aref rows 0))))

(defun hermes-profiles--bot-lookup (rpc profile)
  "Look up PROFILE's exact hidden canonical title via guarded RPC."
  (hermes--promise-map
   (funcall rpc #'hermes-dashboard-transport-session-list
            :profile profile :title "Bot Chat")
   #'hermes-profiles--bot-row))

(defun hermes-profiles--bot-readback (rpc profile)
  "Require matching exact-title and registry identities for PROFILE via RPC."
  (hermes--promise-then
   (hermes-profiles--bot-lookup rpc profile)
   (lambda (row)
     (unless row (user-error "Bot Chat is not persisted; reopen Profiles to retry"))
     (hermes--promise-map
      (hermes-profiles--bot-registry rpc profile)
      (lambda (canonical)
        (unless (and (equal (hermes-transport--get row 'id)
                            (hermes-transport--get canonical 'id))
                     (equal (hermes-transport--get row 'resolved_id)
                            (hermes-transport--get canonical 'resolved_id)))
          (user-error "Bot Chat registry changed; reopen to reconcile"))
        row)))))

(defun hermes-profiles--bot-title (rpc profile created)
  "Persist CREATED's canonical title for PROFILE via RPC, then read it back."
  (let ((runtime (hermes-transport--non-empty-string
                  (hermes-transport--get created 'session_id))))
    (unless (and runtime
                 (equal profile (hermes-transport--get
                                 (hermes-transport--get created 'info) 'profile_name)))
      (user-error "Created session did not confirm the selected profile"))
    (hermes--promise-then
     (funcall rpc #'hermes-dashboard-transport-session-title
              :session-id runtime :title "Bot Chat")
     (lambda (receipt)
       (unless (and (hermes-transport--field-present-p receipt 'pending)
                    (eq (hermes-transport--get receipt 'pending) :json-false)
                    (equal "Bot Chat" (hermes-transport--get receipt 'title)))
         (user-error "Bot Chat title is not persisted; no conversation opened"))
       (hermes-profiles--bot-readback rpc profile))
     (lambda (reason)
       ;; 4022 also covers canonical rename protection.  Only fresh registry
       ;; evidence authorizes adoption; the diagnostic text is never parsed.
       (if (and (stringp reason) (> (length reason) 0)
                (eql 4022 (get-text-property 0 'hermes-rpc-code reason)))
           (hermes-profiles--bot-readback rpc profile)
         (hermes--promise-rejected reason))))))

(defun hermes-profiles--bot-resolve (rpc profile current origin)
  "Resolve PROFILE via RPC while CURRENT owns ORIGIN, creating on absence."
  (hermes--promise-then
   (hermes-profiles--bot-registry rpc profile)
   (lambda (canonical)
     (hermes--promise-then
      (hermes-profiles--bot-lookup rpc profile)
      (lambda (row)
        (cond
         (row (hermes-profiles--bot-readback rpc profile))
         (canonical (user-error "Bot Chat registry contradicts lookup; refusing creation"))
         ((not (funcall current)) (user-error "Profiles changed; no Bot Chat created"))
         (t
          (with-current-buffer origin
            (unless (yes-or-no-p (format "Create persistent Bot Chat for %s? " profile))
              (user-error "Bot Chat creation cancelled")))
          ;; RPC retains the same owner through consent, readiness and follow-ups.
          (hermes--promise-then
           (funcall rpc #'hermes-dashboard-transport-session-create
                    :profile profile :hidden t :follow-profile-config t)
           (lambda (created) (hermes-profiles--bot-title rpc profile created))))))))))

(defun hermes-profiles-open-bot-chat ()
  "Open the selected profile's backend-owned canonical Bot Chat.
Look up the exact title, never the most recent session.  Confirm creation only
on proven absence; persist and read back the title before opening or sending.
Opening never sends an introduction.  The backend may prewarm its agent."
  (interactive nil hermes-profiles-mode)
  (let* ((profile (tabulated-list-get-id))
         (origin (current-buffer))
         (instance (hermes-browser--copy-identity (hermes-instance-resolve))))
    (unless profile (user-error "No profile on this line"))
    (hermes-browser--run-owned
     (lambda (client current)
       (unless (equal (hermes-dashboard-transport--normalize-base-url
                       (hermes-instance-url instance))
                      (hermes-dashboard-transport--api-client-base-url client))
         (user-error "Bot Chat backend changed; reopen Profiles"))
       (let* ((token hermes-dashboard-transport-request-owner)
              (rpc (lambda (function &rest args)
                     (unless (funcall current) (user-error "Bot Chat operation retired"))
                     (let ((hermes-dashboard-transport-dispatch-guard current)
                           (hermes-dashboard-transport-request-lossless-result t)
                           (hermes-dashboard-transport-request-owner token))
                       (apply #'hermes-dashboard-transport-call-fn function client args)))))
         (hermes-profiles--bot-resolve rpc profile current origin)))
     (hermes-browser--mutation-context #'tabulated-list-get-id)
     (lambda (row)
       (hermes-chat-resume-session
        (hermes-transport--get row 'resolved_id) "Bot Chat" profile instance
        (hermes-transport--get row 'id)))
     #'hermes-browser--read-error)))

(defun hermes-profiles-scratch-chat ()
  "Open a separate ordinary chat for the selected profile."
  (interactive nil hermes-profiles-mode)
  (let ((profile (tabulated-list-get-id)))
    (unless profile (user-error "No profile on this line"))
    (hermes-chat profile (hermes-instance-resolve))))

(defun hermes-profiles-routines ()
  "Browse existing cron routines for the selected backend and profile.
Navigation never creates or triggers a job."
  (interactive nil hermes-profiles-mode)
  (let ((profile (tabulated-list-get-id))
        (instance (hermes-instance-resolve)))
    (unless profile (user-error "No profile on this line"))
    (require 'hermes-cron)
    (hermes-cron-for-profile profile instance)))

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
  :help (:group "Conversation"
         hermes-profiles-open-bot-chat "Open Bot Chat"
         hermes-profiles-scratch-chat "Open scratch chat"
         hermes-profiles-routines "Routines"
         :group "Settings"
         hermes-profiles-set-model "Set default model"
         hermes-profiles-edit-soul "Edit SOUL"
         :group "Profile"
         hermes-profiles-create "Create"
         hermes-profiles-rename "Rename"
         hermes-profiles-delete "Delete")
  :keys ("m" #'hermes-profiles-set-model
         "RET" #'hermes-profiles-open-bot-chat
         "N" #'hermes-profiles-scratch-chat
         "R" #'hermes-profiles-routines
         "s" #'hermes-profiles-edit-soul
         "c" #'hermes-profiles-create
         "r" #'hermes-profiles-rename
         "D" #'hermes-profiles-delete))

(provide 'hermes-profiles)
;;; hermes-profiles.el ends here
