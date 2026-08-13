;;; hermes-onboarding.el --- Provider onboarding for Hermes  -*- lexical-binding: t; -*-

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

;; Connect providers to Hermes from Emacs.  API-key onboarding saves keys
;; through `model.save_key'.  The provider-account browser projects the
;; dashboard's OAuth catalog and uses its returned action metadata.
;;
;; Provider membership and authentication policy remain backend-owned.  Shell
;; commands returned by the dashboard are copied for the user, never executed.

;;; Code:

(require 'browse-url)
(require 'seq)
(require 'url-util)
(require 'hermes-promise)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)

(defvar hermes-onboarding-auth-changed-function #'ignore
  "Function called after provider authentication changes successfully.")

(defun hermes-onboarding-set-auth-changed-function (function)
  "Set FUNCTION to run after a provider authentication change."
  (setq hermes-onboarding-auth-changed-function function))

(defun hermes-onboarding--auth-changed ()
  "Invalidate provider data and report a successful authentication change."
  (hermes-dashboard-transport-invalidate-model-options)
  (funcall hermes-onboarding-auth-changed-function))

;;; Pure provider model

(defun hermes-onboarding--unauthed-p (provider)
  "Return non-nil when PROVIDER is not yet authenticated."
  (not (eq (hermes-transport--get provider 'authenticated) t)))

(defun hermes-onboarding--unauthed-providers (result)
  "Return the unauthenticated providers in a `model.options' RESULT.
Every unconnected provider is offered with no client-side auth classification;
the gateway accepts a pasted key or returns its own error on save."
  (seq-filter #'hermes-onboarding--unauthed-p
              (hermes-transport--get result 'providers)))

(defun hermes-onboarding--provider-name (provider)
  "Return PROVIDER's display name, falling back to its slug."
  (or (hermes-transport--scalar-string (hermes-transport--get provider 'name))
      (hermes-transport--scalar-string (hermes-transport--get provider 'slug))
      "provider"))

;;; User interaction

(defun hermes-onboarding--choose-provider (result)
  "Return an API-key provider chosen with completion from RESULT.
The candidate is the provider name; an `:annotation-function' tags each API.
Signal a `user-error' when none are connectable."
  (let ((providers (hermes-onboarding--unauthed-providers result)))
    (unless providers
      (user-error "No API-key providers to connect"))
    (let* ((candidates (mapcar (lambda (p)
                                 (cons (hermes-onboarding--provider-name p) p))
                               providers))
           (completion-extra-properties
            (list :annotation-function
                  (lambda (_name)
                    (concat "  " (propertize "API" 'face 'shadow)))))
           (choice (completing-read "Connect provider: " candidates nil t)))
      (or (cdr (assoc choice candidates))
          (user-error "No provider selected")))))

(defun hermes-onboarding--read-key (provider)
  "Read an API key for PROVIDER with `read-passwd'; signal when empty.
The key is never echoed and never stored in this module."
  (let ((key (read-passwd
              (format "%s API key: " (hermes-onboarding--provider-name provider)))))
    (if (string-empty-p key)
        (user-error "No API key entered")
      key)))

;;; Command

;;;###autoload
(defun hermes-onboarding-connect-provider ()
  "Connect an API-key provider to Hermes by pasting its key.
List the dashboard's unauthenticated API-key providers, read a key for the
chosen one, and save it through `model.save_key'.  A failed save is reported
with the dashboard's own message."
  (interactive)
  (hermes-browser--run-on-client
   (lambda (client)
     (hermes--promise-then
      (hermes-dashboard-transport-call-fn
       #'hermes-dashboard-transport-model-options-cached client)
      (lambda (result)
        (let ((provider (hermes-onboarding--choose-provider result)))
          (hermes-dashboard-transport-call-fn
           #'hermes-dashboard-transport-model-save-key
           client
           (hermes-transport--scalar-string
            (hermes-transport--get provider 'slug))
           (hermes-onboarding--read-key provider))))))
   (lambda (result)
     (hermes-onboarding--auth-changed)
     (message "Connected Hermes provider %s"
              (hermes-onboarding--provider-name
               (hermes-transport--get result 'provider))))))

;;; OAuth providers

(defvar-local hermes-onboarding-oauth--provider nil
  "Provider id shown in the current OAuth status buffer.")

(defvar-local hermes-onboarding-oauth--provider-name nil
  "Provider display name shown in the current OAuth status buffer.")

(defvar-local hermes-onboarding-oauth--session-id nil
  "OAuth session id shown in the current OAuth status buffer.")

(defvar-local hermes-onboarding-oauth--result nil
  "Latest redacted OAuth result shown in the current status buffer.")

(defvar-local hermes-onboarding-oauth--generation 0
  "Generation owning the latest OAuth request in this status buffer.")

(defun hermes-onboarding--oauth-provider-path (provider &rest segments)
  "Return OAuth API path for PROVIDER followed by SEGMENTS."
  (concat "/api/providers/oauth/" (url-hexify-string provider)
          (mapconcat (lambda (segment)
                       (concat "/" (url-hexify-string segment)))
                     segments "")))

(defun hermes-onboarding--oauth-start (client provider)
  "Return promise starting OAuth PROVIDER through CLIENT."
  (hermes-dashboard-transport-api-request-async
   "POST" (hermes-onboarding--oauth-provider-path provider "start")
   :client client))

(defun hermes-onboarding--oauth-poll (client provider session-id)
  "Return promise polling PROVIDER SESSION-ID through CLIENT."
  (hermes-dashboard-transport-api-request-async
   "GET" (hermes-onboarding--oauth-provider-path provider "poll" session-id)
   :client client))

(defun hermes-onboarding--oauth-submit (client provider session-id code)
  "Return promise submitting secret CODE for PROVIDER SESSION-ID through CLIENT."
  (hermes-dashboard-transport-api-request-async
   "POST" (hermes-onboarding--oauth-provider-path provider "submit")
   :body `((session_id . ,session-id) (code . ,code))
   :secrets (list code) :client client))

(defun hermes-onboarding--oauth-cancel (client session-id)
  "Return promise cancelling OAuth SESSION-ID through CLIENT."
  (hermes-dashboard-transport-api-request-async
   "DELETE" (concat "/api/providers/oauth/sessions/"
                    (url-hexify-string session-id))
   :client client))

(defun hermes-onboarding--oauth-disconnect (client provider)
  "Return promise disconnecting OAuth PROVIDER through CLIENT."
  (hermes-dashboard-transport-api-request-async
   "DELETE" (hermes-onboarding--oauth-provider-path provider) :client client))

(defun hermes-onboarding--oauth-provider-logged-in-p (provider)
  "Return non-nil when OAuth PROVIDER reports a logged-in status."
  (hermes-transport--get
   (hermes-transport--get provider 'status) 'logged_in))

(defun hermes-onboarding--provider-account-row (provider)
  "Return one `tabulated-list' entry for account PROVIDER."
  (let* ((status (hermes-transport--get provider 'status))
         (logged-in (hermes-onboarding--oauth-provider-logged-in-p provider))
         (id (hermes-transport--display-field provider 'id))
         (name (hermes-onboarding--provider-name provider))
         (connected (if logged-in "Connected" "Available"))
         (flow (hermes-transport--display-field provider 'flow))
         (source (or (hermes-transport--non-empty-string
                      (hermes-transport--field status 'source_label))
                     (hermes-transport--field status 'source)
                     "")))
    (list id
          (vector (hermes-browser--face-cell name 'hermes-browser-provider)
                  (hermes-browser--face-cell
                   connected (if logged-in
                                 'hermes-browser-success
                               'hermes-browser-muted))
                  (hermes-browser--face-cell flow 'hermes-browser-type)
                  (hermes-browser--face-cell source 'hermes-browser-description)))))

(defun hermes-onboarding--provider-account-rows (result)
  "Return account rows for every provider in API RESULT."
  (mapcar #'hermes-onboarding--provider-account-row
          (hermes-transport--get result 'providers)))

(defvar-local hermes-onboarding--provider-account-result nil
  "Latest provider-account API result rendered in this buffer.")

(defun hermes-onboarding--provider-account-at-point ()
  "Return the provider-account row at point or signal `user-error'."
  (let ((id (tabulated-list-get-id)))
    (or (seq-find
         (lambda (provider)
           (equal (hermes-transport--display-field provider 'id) id))
         (hermes-transport--get
          hermes-onboarding--provider-account-result 'providers))
        (user-error "No provider on this line"))))

(defun hermes-onboarding--oauth-provider-disconnectable-p (provider)
  "Return non-nil when PROVIDER can be disconnected through REST."
  (and (hermes-onboarding--oauth-provider-logged-in-p provider)
       (hermes-transport--get provider 'disconnectable)))

(defun hermes-onboarding--oauth-approved-p (result)
  "Return non-nil when OAuth RESULT reports approved credentials."
  (string= (hermes-transport--display-field result 'status) "approved"))

(defun hermes-onboarding--oauth-provider-candidates (result predicate)
  "Return OAuth provider candidates from RESULT matching PREDICATE."
  (mapcar (lambda (provider)
            (cons (hermes-onboarding--provider-name provider) provider))
          (seq-filter predicate (hermes-transport--get result 'providers))))

(defun hermes-onboarding--choose-oauth-provider (result predicate prompt)
  "Return a provider from RESULT matching PREDICATE, read with PROMPT."
  (let* ((candidates (hermes-onboarding--oauth-provider-candidates
                      result predicate))
         (choice (completing-read prompt candidates nil t)))
    (or (cdr (assoc choice candidates))
        (user-error "No OAuth provider selected"))))

(defun hermes-onboarding--oauth-status-text (result)
  "Return readable, secret-free status text from OAuth RESULT."
  (string-join
   (delq nil
         (list
          (when-let* ((flow (hermes-transport--non-empty-string
                             (hermes-transport--display-field result 'flow))))
            (format "Flow: %s" flow))
          (when-let* ((status (hermes-transport--non-empty-string
                               (hermes-transport--display-field result 'status))))
            (format "Status: %s" status))
          (when-let* ((url (hermes-transport--non-empty-string
                            (or (hermes-transport--get result 'verification_url)
                                (hermes-transport--get result 'auth_url)))))
            (format "Sign in: %s" url))
          (when-let* ((code (hermes-transport--non-empty-string
                             (hermes-transport--get result 'user_code))))
            (format "User code: %s" code))
          (when-let* ((error (hermes-transport--non-empty-string
                              (hermes-transport--get result 'error_message))))
            (format "Error: %s" error))))
   "\n"))

(defun hermes-onboarding-oauth--render ()
  "Render the current OAuth status buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "OAuth provider: %s\n\n"
                    (or hermes-onboarding-oauth--provider-name
                        hermes-onboarding-oauth--provider)))
    (let ((status (hermes-onboarding--oauth-status-text
                   hermes-onboarding-oauth--result)))
      (insert (if (string-empty-p status) "No status yet" status)))
    (insert "\n\nKeys: g poll, s submit code, c cancel, d disconnect\n")
    (goto-char (point-min))))

(defun hermes-onboarding--oauth-context ()
  "Invalidate older callbacks and return the current OAuth request context."
  (setq hermes-onboarding-oauth--generation
        (1+ hermes-onboarding-oauth--generation))
  (list :buffer (current-buffer)
        :generation hermes-onboarding-oauth--generation
        :provider hermes-onboarding-oauth--provider
        :session-id hermes-onboarding-oauth--session-id))

(defun hermes-onboarding--oauth-context-current-p (context)
  "Return non-nil when CONTEXT still owns its OAuth status buffer."
  (and (eq (current-buffer) (plist-get context :buffer))
       (derived-mode-p 'hermes-onboarding-oauth-mode)
       (= hermes-onboarding-oauth--generation
          (plist-get context :generation))
       (equal hermes-onboarding-oauth--provider
              (plist-get context :provider))
       (equal hermes-onboarding-oauth--session-id
              (plist-get context :session-id))))

(defun hermes-onboarding--oauth-apply-result (context result &optional clear-session)
  "Apply RESULT when CONTEXT is current, clearing its session when requested."
  (when-let* ((buffer (plist-get context :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (hermes-onboarding--oauth-context-current-p context)
        (let* ((prior hermes-onboarding-oauth--result)
               (merged (hermes-onboarding--oauth-merge-result prior result)))
          (setq hermes-onboarding-oauth--session-id
                (unless clear-session
                  (or (hermes-transport--non-empty-string
                       (hermes-transport--display-field merged 'session_id))
                      hermes-onboarding-oauth--session-id))
                hermes-onboarding-oauth--result merged)
          (hermes-onboarding-oauth--render)
          t)))))

(defun hermes-onboarding--oauth-merge-result (prior result)
  "Merge poll RESULT into PRIOR start payload, preserving device-code fields."
  (let* ((base (and (listp prior) (copy-sequence prior)))
         (incoming (and (listp result) result)))
    (unless incoming
      (setq incoming result))
    (if (not (and (listp base) (listp incoming)))
        incoming
      (dolist (key '(user_code verification_url auth_url authorization_url
                     flow provider))
        (when-let* ((value (hermes-transport--get base key))
                    ((null (hermes-transport--get incoming key))))
          (setq incoming (cons (cons key value) incoming))))
      incoming)))

(defun hermes-onboarding--show-oauth (provider result)
  "Show PROVIDER and OAuth RESULT, returning its request context."
  (let ((buffer (get-buffer-create "*Hermes OAuth*")) context)
    (with-current-buffer buffer
      (unless (derived-mode-p 'hermes-onboarding-oauth-mode)
        (hermes-onboarding-oauth-mode))
      (setq hermes-onboarding-oauth--provider
            (hermes-transport--display-field provider 'id)
            hermes-onboarding-oauth--provider-name
            (hermes-onboarding--provider-name provider)
            hermes-onboarding-oauth--session-id
            (hermes-transport--display-field result 'session_id)
            hermes-onboarding-oauth--result result)
      (hermes-onboarding-oauth--render)
      (setq context (hermes-onboarding--oauth-context)))
    (pop-to-buffer buffer)
    context))

(defun hermes-onboarding--oauth-start-provider (provider)
  "Start native OAuth for API-supplied PROVIDER."
  (let (context)
    (hermes-browser--run-on-client
     (lambda (client)
       (setq context
             (hermes-onboarding--show-oauth provider '((status . "starting"))))
       (hermes-onboarding--oauth-start
        client (hermes-transport--display-field provider 'id)))
     (lambda (result)
       (when (hermes-onboarding--oauth-apply-result context result)
         (when (hermes-onboarding--oauth-approved-p result)
           (hermes-onboarding--auth-changed))
         (when-let* ((url (or (hermes-transport--get result 'verification_url)
                              (hermes-transport--get result 'auth_url)))
                     ((hermes-transport--non-empty-string url)))
           (browse-url url)))))))

(defun hermes-onboarding--provider-account-copy-field (provider field label)
  "Copy PROVIDER FIELD and report it as LABEL."
  (let ((value (hermes-transport--non-empty-string
                (hermes-transport--display-field provider field))))
    (unless value
      (user-error "Provider supplied no %s" label))
    (kill-new value)
    (message "Hermes: copied provider %s" label)))

(defun hermes-onboarding--provider-account-act (provider)
  "Use the API-described connection action for PROVIDER."
  (let ((flow (hermes-transport--display-field provider 'flow)))
    (cond
     ((hermes-onboarding--oauth-provider-logged-in-p provider)
      (message "Hermes: %s is connected"
               (hermes-onboarding--provider-name provider)))
     ((member flow '("device_code" "pkce"))
      (hermes-onboarding--oauth-start-provider provider))
     ((hermes-transport--non-empty-string
       (hermes-transport--display-field provider 'cli_command))
      (hermes-onboarding--provider-account-copy-field
       provider 'cli_command "connection command"))
     ((hermes-transport--non-empty-string
       (hermes-transport--display-field provider 'docs_url))
      (browse-url (hermes-transport--display-field provider 'docs_url)))
     (t
      (user-error "Provider supplied no supported connection action")))))

(defun hermes-onboarding-provider-account-act ()
  "Connect or describe the provider account at point."
  (interactive)
  (hermes-onboarding--provider-account-act
   (hermes-onboarding--provider-account-at-point)))

(defun hermes-onboarding-provider-account-copy-command ()
  "Copy the API-supplied connection command for the provider at point."
  (interactive)
  (hermes-onboarding--provider-account-copy-field
   (hermes-onboarding--provider-account-at-point)
   'cli_command "connection command"))

(defun hermes-onboarding-provider-account-browse-docs ()
  "Open API-supplied documentation for the provider at point."
  (interactive)
  (let* ((provider (hermes-onboarding--provider-account-at-point))
         (url (hermes-transport--non-empty-string
               (hermes-transport--display-field provider 'docs_url))))
    (unless url (user-error "Provider supplied no documentation URL"))
    (browse-url url)))

(defun hermes-onboarding-oauth-poll ()
  "Poll the OAuth session shown in the current status buffer."
  (interactive)
  (unless (and hermes-onboarding-oauth--provider
               (hermes-transport--non-empty-string
                hermes-onboarding-oauth--session-id))
    (user-error "No OAuth session to poll"))
  (let ((context (hermes-onboarding--oauth-context)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-onboarding--oauth-poll
        client (plist-get context :provider) (plist-get context :session-id)))
     (lambda (result)
       (when (hermes-onboarding--oauth-apply-result context result)
         (when (hermes-onboarding--oauth-approved-p result)
           (hermes-onboarding--auth-changed)))))))

(defun hermes-onboarding-oauth-submit ()
  "Submit a secret code for the OAuth session in the current buffer."
  (interactive)
  (unless (and hermes-onboarding-oauth--provider
               (hermes-transport--non-empty-string
                hermes-onboarding-oauth--session-id))
    (user-error "No OAuth session awaiting a code"))
  (let ((code (read-passwd "OAuth code: ")))
    (when (string-empty-p code)
      (user-error "OAuth code required"))
    (let ((context (hermes-onboarding--oauth-context)))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-onboarding--oauth-submit
          client (plist-get context :provider)
          (plist-get context :session-id) code))
       (lambda (result)
         (when (hermes-onboarding--oauth-apply-result context result)
           (when (hermes-onboarding--oauth-approved-p result)
             (hermes-onboarding--auth-changed))))))))

(defun hermes-onboarding-oauth-cancel ()
  "Cancel the OAuth session shown in the current buffer."
  (interactive)
  (unless (hermes-transport--non-empty-string
           hermes-onboarding-oauth--session-id)
    (user-error "No OAuth session to cancel"))
  (let ((context (hermes-onboarding--oauth-context)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-onboarding--oauth-cancel
        client (plist-get context :session-id)))
     (lambda (result)
       (hermes-onboarding--oauth-apply-result context result t)))))

(defun hermes-onboarding-oauth-disconnect ()
  "Disconnect the OAuth provider shown in the current buffer."
  (interactive)
  (unless hermes-onboarding-oauth--provider
    (user-error "No OAuth provider to disconnect"))
  (unless (yes-or-no-p (format "Disconnect OAuth provider %s? "
                               hermes-onboarding-oauth--provider-name))
    (user-error "OAuth disconnect cancelled"))
  (let ((context (hermes-onboarding--oauth-context)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes-onboarding--oauth-disconnect
        client (plist-get context :provider)))
     (lambda (result)
       (when (hermes-onboarding--oauth-apply-result context result t)
         (hermes-onboarding--auth-changed))))))

;;;###autoload
(defun hermes-onboarding-oauth-disconnect-provider ()
  "Choose and disconnect a connected dashboard OAuth provider."
  (interactive)
  (let (provider context)
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-then
        (hermes-dashboard-transport-api-request-async
         "GET" "/api/providers/oauth" :client client)
        (lambda (result)
          (setq provider
                (hermes-onboarding--choose-oauth-provider
                 result #'hermes-onboarding--oauth-provider-disconnectable-p
                 "Disconnect OAuth provider: "))
          (unless (yes-or-no-p
                   (format "Disconnect OAuth provider %s? "
                           (hermes-onboarding--provider-name provider)))
            (user-error "OAuth disconnect cancelled"))
          (setq context
                (hermes-onboarding--show-oauth
                 provider '((status . "disconnecting"))))
          (hermes-onboarding--oauth-disconnect
           client (hermes-transport--display-field provider 'id)))))
     (lambda (result)
       (when (hermes-onboarding--oauth-apply-result context result t)
         (hermes-onboarding--auth-changed)
         (message "Hermes: disconnected OAuth provider %s"
                  (hermes-onboarding--provider-name provider)))))))

(defun hermes-onboarding-provider-account-disconnect ()
  "Disconnect the provider account at point using API-returned policy."
  (interactive)
  (let* ((provider (hermes-onboarding--provider-account-at-point))
         (name (hermes-onboarding--provider-name provider))
         (origin (current-buffer)))
    (unless (hermes-onboarding--oauth-provider-logged-in-p provider)
      (user-error "%s is not connected" name))
    (cond
     ((hermes-transport--get provider 'disconnectable)
      (when (yes-or-no-p (format "Disconnect provider %s? " name))
        (hermes-browser--run-on-client
         (lambda (client)
           (hermes-onboarding--oauth-disconnect
            client (hermes-transport--display-field provider 'id)))
         (lambda (_result)
           (hermes-onboarding--auth-changed)
           (message "Hermes: disconnected provider %s" name)
           (when (hermes-browser--buffer-mode-p
                  origin 'hermes-provider-accounts-mode)
             (with-current-buffer origin
               (hermes-provider-accounts--revert)))))))
     ((hermes-transport--non-empty-string
       (hermes-transport--display-field provider 'disconnect_command))
      (hermes-onboarding--provider-account-copy-field
       provider 'disconnect_command "disconnect command"))
     ((hermes-transport--non-empty-string
       (hermes-transport--display-field provider 'disconnect_hint))
      (message "Hermes: %s"
               (hermes-transport--display-field provider 'disconnect_hint)))
     (t
      (user-error "Provider supplied no disconnect action")))))

;;;###autoload (autoload 'hermes-onboarding-oauth-connect "hermes-onboarding" nil t)
(hermes-define-list-browser provider-accounts
  :title "Hermes Provider Accounts"
  :buffer "*Hermes Provider Accounts*"
  :command hermes-onboarding-oauth-connect
  :doc "Major mode listing provider accounts reported by the Hermes dashboard."
  :command-doc "Browse every provider account reported by the Hermes dashboard."
  :columns [("Provider" 32 t) ("Status" 11 t) ("Flow" 14 t) ("Source" 36 t)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-api-request-async
            "GET" "/api/providers/oauth" :client client))
  :rows #'hermes-onboarding--provider-account-rows
  :on-result (lambda (result)
               (setq hermes-onboarding--provider-account-result result))
  :keys ("RET" #'hermes-onboarding-provider-account-act
         "c" #'hermes-onboarding-provider-account-act
         "d" #'hermes-onboarding-provider-account-disconnect
         "b" #'hermes-onboarding-provider-account-browse-docs
         "w" #'hermes-onboarding-provider-account-copy-command))

(defvar-keymap hermes-onboarding-oauth-mode-map
  :doc "Keymap for `hermes-onboarding-oauth-mode'."
  :parent special-mode-map
  "g" #'hermes-onboarding-oauth-poll
  "s" #'hermes-onboarding-oauth-submit
  "c" #'hermes-onboarding-oauth-cancel
  "d" #'hermes-onboarding-oauth-disconnect)

(define-derived-mode hermes-onboarding-oauth-mode special-mode "Hermes OAuth"
  "Major mode for one Hermes OAuth provider flow."
  :interactive nil)

(provide 'hermes-onboarding)
;;; hermes-onboarding.el ends here
