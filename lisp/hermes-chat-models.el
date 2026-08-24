;;; hermes-chat-models.el --- Model and provider selection for Hermes chat  -*- lexical-binding: t; -*-

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

;; Model/provider selection for `hermes-chat': the `model.options'
;; completion candidates, the `config.set' model switch with its
;; expensive-model confirmation loop, and API-key provider connect.  Part
;; of the one logical chat module (see the require note in
;; `hermes-chat.el'); it preserves the existing `hermes-chat--*' symbols.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-buffer)
(require 'hermes-chat-dashboard)


(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)

(defun hermes-chat--model-id (model)
  "Return the model id string from a `model.options' MODEL entry."
  (or (hermes-transport--scalar-string model)
      (hermes-transport--scalar-string
       (hermes-transport--get-any model '(id model name)))))

(defun hermes-chat--model-price (provider model)
  "Return a compact price string for MODEL in PROVIDER row, or nil."
  (when-let* ((prices (hermes-transport--get
                      (hermes-transport--get provider 'pricing) model)))
    (string-join
     (delq nil
           (list (hermes-transport--scalar-string
                  (hermes-transport--get prices 'input))
                 (hermes-transport--scalar-string
                  (hermes-transport--get prices 'output))))
     "/")))

(defun hermes-chat--model-provider-label (provider)
  "Return a readable, provider-identity-preserving label for PROVIDER."
  (let ((name (hermes-transport--scalar-string
               (hermes-transport--get provider 'name)))
        (slug (hermes-transport--scalar-string
               (hermes-transport--get provider 'slug))))
    (cond
     ((and name slug (not (equal name slug))) (format "%s (%s)" name slug))
     (name name)
     (slug slug)
     (t "provider"))))

(defun hermes-chat--model-label (provider model)
  "Return completion label for MODEL in PROVIDER row."
  (string-join
   (delq nil
         (list (hermes-chat--model-provider-label provider)
               model
               (hermes-chat--model-price provider model)))
   " · "))

(defun hermes-chat--model-candidate (provider model)
  "Return one completion candidate for MODEL in PROVIDER row."
  (when-let* ((model-id (hermes-chat--model-id model)))
    (let* ((provider-slug (hermes-transport--scalar-string
                          (hermes-transport--get provider 'slug)))
           (label (hermes-chat--model-label provider model-id)))
      (cons label (list :model model-id
                        :provider provider-slug
                        :label label
                        :authenticated (eq (hermes-transport--get
                                            provider 'authenticated)
                                           t))))))

(defun hermes-chat--model-candidates (payload)
  "Return completion candidates from `model.options' PAYLOAD.
Each candidate is (LABEL . PLIST).  Authenticated provider rows are listed
first; model ids are not de-duplicated across providers because provider
identity is part of the selection."
  (let (authed other seen)
    (dolist (provider (hermes-transport--get payload 'providers))
      (dolist (model (hermes-transport--get provider 'models))
        (when-let* ((candidate (hermes-chat--model-candidate provider model))
                    (data (cdr candidate))
                    (key (list (plist-get data :provider)
                               (plist-get data :model))))
          (unless (member key seen)
            (push key seen)
            (if (plist-get data :authenticated)
                (push candidate authed)
              (push candidate other))))))
    (append (nreverse authed) (nreverse other))))

(defvar-local hermes-chat--model-completion-client nil
  "Client whose model catalog completion warmup is currently pending.")

(defun hermes-chat--model-completion-bounds ()
  "Return bounds of the `/model' argument at point, or nil."
  (when (and (hermes-chat--point-in-input-p)
             (hermes-chat--input-position))
    (let* ((input (hermes-chat--input-position))
           (text (buffer-substring-no-properties input (point))))
      (when (string-match "\\`/model[ \t]+\\([^\n]*\\)\\'" text)
        (cons (+ input (match-beginning 1)) (point))))))

(defun hermes-chat--model-completion-candidates (payload)
  "Return (VALUE . ANNOTATION) completion rows from cached PAYLOAD."
  (mapcar
   (lambda (candidate)
     (let ((data (cdr candidate)))
       (cons (hermes-chat--model-config-value data)
             (concat "  " (plist-get data :label)))))
   (cl-remove-if-not
    (lambda (candidate) (plist-get (cdr candidate) :authenticated))
    (hermes-chat--model-candidates payload))))

(defun hermes-chat--warm-model-completions (client)
  "Warm model completion data asynchronously for CLIENT once."
  (unless (eq client hermes-chat--model-completion-client)
    (setq hermes-chat--model-completion-client client)
    (let ((buffer (current-buffer))
          (lifetime hermes-chat--lifecycle-generation))
      (hermes-dashboard-transport-model-options-cached
       client
       :session-id hermes-chat--dashboard-active-session-id
       :resolve (lambda (_result)
                  (hermes-chat--in-lifetime buffer lifetime
                    (when (eq client hermes-chat--model-completion-client)
                      (setq hermes-chat--model-completion-client nil))))
       :reject (lambda (_message)
                 (hermes-chat--in-lifetime buffer lifetime
                   (when (eq client hermes-chat--model-completion-client)
                     (setq hermes-chat--model-completion-client nil))))))))

(defun hermes-chat--model-capf ()
  "Complete cached model arguments after `/model' in the input tail."
  (when-let* ((bounds (hermes-chat--model-completion-bounds))
              (client hermes-chat--dashboard-client))
    (if-let* ((payload
               (hermes-dashboard-transport-cached-model-options client))
              (rows (hermes-chat--model-completion-candidates payload)))
        (let ((values (mapcar #'car rows)))
          (list (car bounds) (cdr bounds) values
                :exclusive 'no
                :annotation-function
                (lambda (candidate) (cdr (assoc candidate rows)))))
      (hermes-chat--warm-model-completions client)
      nil)))

(defun hermes-chat--model-display-name (candidate)
  "Return a compact display name for CANDIDATE."
  (if (stringp candidate)
      candidate
    (or (plist-get candidate :model)
        (hermes-chat--model-config-value candidate))))

(defun hermes-chat--clear-pending-model-override ()
  "Clear a retrying create-time model after a successful live switch."
  (setq hermes-chat--dashboard-create-model nil
        hermes-chat--dashboard-create-provider nil)
  (unless (or hermes-chat--dashboard-create-reasoning-effort
              hermes-chat--dashboard-create-fast-p)
    (setq hermes-chat--create-overrides-retry-session-id nil)))

(defun hermes-chat--model-switch-context ()
  "Return the current chat identity for an asynchronous model switch."
  (list :buffer (current-buffer)
        :client hermes-chat--dashboard-client
        :session-id hermes-chat--dashboard-active-session-id
        :lifecycle-generation hermes-chat--lifecycle-generation
        :transport-generation hermes-chat--transport-generation))

(defun hermes-chat--model-switch-current-p (context)
  "Return non-nil when model-switch CONTEXT still names an idle chat."
  (or (null context)
      (and-let* ((buffer (plist-get context :buffer))
                 ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (and (hermes-chat--current-lifetime-p
                (plist-get context :lifecycle-generation))
               (eq hermes-chat--dashboard-client
                   (plist-get context :client))
               (equal hermes-chat--dashboard-active-session-id
                      (plist-get context :session-id))
               (= hermes-chat--transport-generation
                  (plist-get context :transport-generation))
               (not (hermes-chat--active-turn-p)))))))

(defun hermes-chat--apply-model (buffer client candidate confirm &optional context)
  "Set CANDIDATE on BUFFER's session via CLIENT.
CONFIRM acknowledges an expensive-model confirmation prompt.  When BUFFER
has no live session yet, the choice is stored buffer-locally and applied
through `config.set' right after the next session is created."
  (if (not (hermes-chat--model-switch-current-p context))
      (message "Hermes: model switch is stale or the chat is busy")
    (with-current-buffer buffer
      (if (hermes-chat--dashboard-session-attached-p)
          (hermes-dashboard-transport-config-set
           client "model" (hermes-chat--model-config-value candidate)
           :session-id hermes-chat--dashboard-active-session-id
           :confirm-expensive-model confirm
           :resolve (lambda (result)
                      (hermes-chat--model-set-result
                       buffer client candidate result confirm context))
           :reject (lambda (message)
                     (hermes-chat--in-buffer buffer
                       (when (hermes-chat--model-switch-current-p context)
                         (hermes-chat--command-error message)))))
        (setq hermes-chat--dashboard-create-model
              (if (stringp candidate) candidate (plist-get candidate :model))
              hermes-chat--dashboard-create-provider
              (and (not (stringp candidate))
                   (plist-get candidate :provider)))
        (hermes-chat--insert-local-status
         (format "Model set to %s (applies to next session)"
                 (hermes-chat--model-display-name candidate))
         'ready)))))

(defun hermes-chat--model-set-result
    (buffer client candidate result confirmed &optional context)
  "Report CANDIDATE switch RESULT for BUFFER, re-confirming through CLIENT.
CONFIRMED is non-nil after the user has already accepted an expensive-model
confirmation prompt."
  (when (hermes-chat--model-switch-current-p context)
    (hermes-chat--in-buffer buffer
      (if (hermes-transport--get result 'confirm_required)
          (if confirmed
              (hermes-chat--command-error
               (format "Model switch still requires confirmation: %s"
                       (or (hermes-transport--scalar-string
                            (hermes-transport--get result 'confirm_message))
                           "backend repeated confirmation request")))
            (if (yes-or-no-p
                 (or (hermes-transport--scalar-string
                      (hermes-transport--get result 'confirm_message))
                     "Confirm switching to this model? "))
                (hermes-chat--apply-model buffer client candidate t context)
              (hermes-chat--insert-local-status
               "Model switch cancelled" 'ready)))
        (when (hermes-chat--dashboard-session-attached-p)
          (hermes-chat--clear-pending-model-override))
        (hermes-chat--insert-local-status
         (format "Model set to %s"
                 (hermes-chat--model-display-name candidate))
         'ready)))))

(defun hermes-chat--prompt-and-set-model (buffer client result &optional context)
  "Prompt for a model from RESULT and apply it to BUFFER's session via CLIENT."
  (if (not (hermes-chat--model-switch-current-p context))
      (message "Hermes: model switch is stale or the chat is busy")
    (when (buffer-live-p buffer)
      (let* ((candidates
              (cl-remove-if-not
               (lambda (candidate)
                 (or client (plist-get (cdr candidate) :authenticated)))
               (hermes-chat--model-candidates result)))
             (labels (mapcar #'car candidates)))
        (if (null candidates)
            (message "Hermes: no models available to switch to")
          (let* ((choice (completing-read "Switch model: " labels nil t))
                 (candidate (cdr (assoc choice candidates)))
                 (provider (and candidate
                                (hermes-chat--find-provider
                                 result (plist-get candidate :provider)))))
            (unless (or (string-empty-p choice) (null candidate))
              (if (not (hermes-chat--model-switch-current-p context))
                  (message "Hermes: model switch is stale or the chat is busy")
                (let ((auth-type (hermes-transport--scalar-string
                                  (hermes-transport--get provider 'auth_type))))
                  (cond
                   ((plist-get candidate :authenticated)
                    (hermes-chat--apply-model
                     buffer client candidate nil context))
                   ((equal auth-type "api_key")
                    (hermes-chat--connect-provider-candidate
                     buffer client provider
                     (lambda ()
                       (hermes-chat--apply-model
                        buffer client candidate nil context))))
                   (t
                    (message
                     "Hermes: %s requires %s authentication; authenticate it before switching models"
                     (hermes-chat--model-provider-label provider)
                     (if (equal auth-type "oauth") "OAuth"
                       (or auth-type "external"))))))))))))))

(defun hermes-chat--request-model-switch (client refresh)
  "Fetch model choices through CLIENT, bypassing the cache when REFRESH is non-nil."
  (let ((buffer (current-buffer))
        (context (hermes-chat--model-switch-context)))
    (hermes-dashboard-transport-model-options-cached
     client
     :session-id hermes-chat--dashboard-active-session-id
     :force refresh
     :resolve (lambda (result)
                (hermes-chat--prompt-and-set-model
                 buffer client result context))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (when (hermes-chat--model-switch-current-p context)
                   (hermes-chat--command-error message)))))))

(defun hermes-chat-switch-model (&optional refresh)
  "Switch the model used by the current Hermes chat session.
The model list is served from the shared cache; with a prefix argument REFRESH,
refetch it from the dashboard instead.  Before the first session, a cached pick
is stored locally without connecting; a cold or refreshed catalog may open the
shared dashboard socket but does not create a session."
  (interactive "P")
  (when (hermes-chat--active-turn-p)
    (user-error "Interrupt the active turn before switching models"))
  (let ((client (and (hermes-chat--dashboard-client-live-p
                      hermes-chat--dashboard-client)
                     hermes-chat--dashboard-client))
        (cached (and (not refresh)
                     (hermes-dashboard-transport-cached-model-options))))
    (if cached
        (hermes-chat--prompt-and-set-model
         (current-buffer) client cached (hermes-chat--model-switch-context))
      (hermes-chat--request-model-switch
       (or client (hermes-chat--dashboard-control-client)) refresh))))

;; Reused from `hermes-onboarding'.  That module requires `hermes-browser',
;; which requires this file, so it is loaded lazily inside the commands below to
;; avoid a load-time cycle; by then this file is already provided.
(declare-function hermes-onboarding--read-key "hermes-onboarding")
(declare-function hermes-onboarding--choose-provider "hermes-onboarding")

(defun hermes-chat--find-provider (result slug)
  "Return the provider row in `model.options' RESULT whose slug is SLUG."
  (cl-find slug (hermes-transport--get result 'providers)
           :key (lambda (provider)
                  (hermes-transport--scalar-string
                   (hermes-transport--get provider 'slug)))
           :test #'equal))

(defun hermes-chat--connect-provider-candidate (buffer client provider
                                                       &optional on-connected)
  "Read a key for PROVIDER and save it on CLIENT scoped to BUFFER's session.
ON-CONNECTED, when given, runs after a successful save -- the model picker uses
it to apply the model the user originally chose."
  (require 'hermes-onboarding)
  (let* ((context (with-current-buffer buffer
                    (hermes-chat--model-switch-context)))
         (slug (hermes-transport--scalar-string
                (hermes-transport--get provider 'slug)))
         (name (or (hermes-transport--scalar-string
                    (hermes-transport--get provider 'name))
                   slug))
         (key (hermes-onboarding--read-key provider)))
    (unless (hermes-chat--model-switch-current-p context)
      (user-error "Hermes provider request is no longer current"))
    (with-current-buffer buffer
      (hermes-dashboard-transport-model-save-key
       client slug key
       :session-id (plist-get context :session-id)
       :resolve (lambda (_result)
                  (when (hermes-chat--model-switch-current-p context)
                    ;; Saving a key flips a provider's authentication, so drop
                    ;; the cached catalog; the next picker refetches the list.
                    (hermes-dashboard-transport-invalidate-model-options)
                    (hermes-chat--in-buffer buffer
                      (hermes-chat--insert-local-status
                       (format "Connected provider %s" name) 'ready)
                      (when on-connected (funcall on-connected)))))
       :reject (lambda (message)
                 (when (hermes-chat--model-switch-current-p context)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--command-error message))))))))

(defun hermes-chat-connect-provider ()
  "Connect an API-key provider to the current Hermes chat session.
Pick an unauthenticated provider and paste its key; the dashboard saves it
against this session's live agent."
  (interactive)
  (unless (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client)
    (user-error "Connect this chat (send a message) before connecting a provider"))
  (require 'hermes-onboarding)
  (let ((buffer (current-buffer))
        (client hermes-chat--dashboard-client)
        (context (hermes-chat--model-switch-context)))
    (hermes-dashboard-transport-model-options-cached
     client
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (result)
                (hermes-chat--in-buffer buffer
                  (when (hermes-chat--model-switch-current-p context)
                    (let ((provider (hermes-onboarding--choose-provider result)))
                      (when (hermes-chat--model-switch-current-p context)
                        (hermes-chat--connect-provider-candidate
                         buffer client provider))))))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (when (hermes-chat--model-switch-current-p context)
                   (hermes-chat--command-error message)))))))

(provide 'hermes-chat-models)
;;; hermes-chat-models.el ends here
