;;; hermes-tool-setup.el --- Native tool setup for Hermes  -*- lexical-binding: t; -*-

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
;; Configure the selected instance through backend-owned provider metadata.
;; Readiness is a backend prerequisite check, not a paid tool invocation.
;; Post-setup status is server-wide: the API exposes no per-profile job ID.

;;; Code:

(require 'hermes-browser)
(require 'keymap-popup)
(require 'url-util)

(defvar-local hermes-tool-setup--name nil "Toolset owned by this buffer.")
(defvar-local hermes-tool-setup--profile nil "Profile, or nil for server default.")
(defvar-local hermes-tool-setup--config nil "Last backend provider snapshot.")
(defvar-local hermes-tool-setup--busy nil "Non-nil while a change is pending.")
(defvar-local hermes-tool-setup--post-status nil "Last server-wide setup status.")
(defvar-local hermes-tool-setup--model-catalog nil
  "Last backend model snapshot for the active provider, not the row at point.")
(defvar-local hermes-tool-setup--pending nil
  "Current (OWNER . SUFFIX) setup request, or nil when settled.")

(defun hermes-tool-setup--active-providers ()
  "Return backend-reported active provider names, preserving multiple matches."
  (or (delq nil (mapcar
                 (lambda (provider)
                   (and (eq t (hermes-transport--get provider 'is_active))
                        (hermes-transport--get provider 'name)))
                 (hermes-transport--get hermes-tool-setup--config 'providers)))
      (when-let* ((name (hermes-transport--non-empty-string
                        (hermes-transport--get hermes-tool-setup--config 'active_provider))))
        (list name))))

(defun hermes-tool-setup--setting-description (setting)
  "Return a bounded SETTING label from this owner's reported state."
  (let* ((config hermes-tool-setup--config)
         (catalog hermes-tool-setup--model-catalog)
         (pending (and hermes-tool-setup--pending
                       (hermes-tool-setup--current-p (car hermes-tool-setup--pending))
                       (cdr hermes-tool-setup--pending)))
         (value
          (cond
           (hermes-tool-setup--busy "pending change")
           ((or (equal pending "/config")
                (and (eq setting 'model) (equal pending "/models")))
            "loading")
           ((eq setting 'provider)
            (if (equal hermes-tool-setup--name "web")
                (format "search %s / extract %s"
                        (or (hermes-transport--get config 'active_search_backend) "unknown")
                        (or (hermes-transport--get config 'active_extract_backend) "unknown"))
              (when-let* ((names (hermes-tool-setup--active-providers)))
                (string-join names ", "))))
           ((and catalog (eq :false (hermes-transport--get catalog 'has_models)))
            "not exposed")
           ((member (hermes-transport--get catalog 'provider)
                    (hermes-tool-setup--active-providers))
            (hermes-transport--non-empty-string (hermes-transport--get catalog 'current)))))
         (value (or value "unknown")))
    (concat (if (eq setting 'provider) "Provider: " "Model: ")
            (propertize (truncate-string-to-width value 28 nil nil t)
                        'face 'keymap-popup-value 'help-echo value))))

(defun hermes-tool-setup--rows (config)
  "Return provider rows from CONFIG without inferring readiness from keys."
  (mapcar
   (lambda (provider)
     (let ((name (hermes-transport--get provider 'name)))
       (list name
             (vector
              name
              (if (eq t (hermes-transport--get provider 'is_active)) "Selected" "")
              (hermes-browser--status-cell
               (or (hermes-transport--get provider 'status) "unknown"))
              (mapconcat
               (lambda (env)
                 (format "%s: %s" (hermes-transport--get env 'key)
                         (if (eq t (hermes-transport--get env 'is_set))
                             "saved" "missing")))
               (hermes-transport--get provider 'env_vars) ", ")
              (or (hermes-transport--get provider 'tag) "")))))
   (hermes-transport--get config 'providers)))

(defun hermes-tool-setup--owner ()
  "Return the exact current setup owner."
  (list (current-buffer) hermes-browser--request-generation
        hermes-instance hermes-tool-setup--name hermes-tool-setup--profile))

(defun hermes-tool-setup--current-p (owner)
  "Return non-nil if OWNER still owns the setup surface."
  (and (hermes-browser--request-current-mode-p
        (nth 0 owner) (nth 1 owner) 'hermes-tool-setup-mode)
       (with-current-buffer (car owner)
         (equal owner (hermes-tool-setup--owner)))))

(defun hermes-tool-setup--idle ()
  "Signal an error while another mutation is pending."
  (unless (derived-mode-p 'hermes-tool-setup-mode)
    (user-error "Not a tool setup buffer"))
  (when hermes-tool-setup--busy (user-error "Tool setup change pending")))

(cl-defun hermes-tool-setup--request
    (method suffix success &key body query secrets absolute)
  "Request METHOD SUFFIX and call SUCCESS in the owning buffer.
BODY, QUERY and SECRETS extend the request.
ABSOLUTE uses SUFFIX as a full path.
Fence success and failure against buffer, instance and profile changes."
  (hermes-browser--next-request-generation)
  (let ((owner (hermes-tool-setup--owner))
        (path (if absolute suffix
                (concat "/api/tools/toolsets/"
                        (url-hexify-string hermes-tool-setup--name) suffix)))
        (query (append query (when hermes-tool-setup--profile
                               `((profile . ,hermes-tool-setup--profile))))))
    (setq hermes-tool-setup--pending (cons owner suffix))
    (hermes-browser--run-on-client
     (lambda (client)
       (let* ((guard hermes-dashboard-transport--api-dispatch-guard)
              (hermes-dashboard-transport--api-dispatch-guard
               (lambda () (and (hermes-tool-setup--current-p owner)
                               (or (null guard) (funcall guard))))))
         (hermes-dashboard-transport-api-request-async
          method path :client client :body body :query query :secrets secrets)))
     (lambda (result)
       (when (hermes-tool-setup--current-p owner)
         (with-current-buffer (car owner)
           (setq hermes-tool-setup--busy nil hermes-tool-setup--pending nil)
           (funcall success result))))
     (lambda (_reason)
       ;; Release this request even if its instance retired during acquisition.
       ;; Do not echo backend errors which may repeat credential input.
       (when (hermes-browser--request-current-mode-p
              (car owner) (cadr owner) 'hermes-tool-setup-mode)
         (with-current-buffer (car owner)
           (setq hermes-tool-setup--busy nil hermes-tool-setup--pending nil)))
       (when (hermes-tool-setup--current-p owner)
         (message "Hermes: tool setup request failed; refresh to check state"))))))

(defun hermes-tool-setup-refresh (&rest _)
  "Recheck provider readiness without invoking a model or a tool."
  (interactive)
  (hermes-tool-setup--idle)
  (setq hermes-tool-setup--config nil hermes-tool-setup--model-catalog nil)
  (hermes-tool-setup--request
   "GET" "/config"
   (lambda (config)
     (setq hermes-tool-setup--config config
           tabulated-list-entries (hermes-tool-setup--rows config))
     (setq-local header-line-format
                 (format " %s | %s | Profile: %s | Backend prerequisites, not a tool test%s"
                         (hermes-instance-name hermes-instance)
                         hermes-tool-setup--name
                         (or hermes-tool-setup--profile "server default")
                         (concat
                          (unless (hermes-tool-setup--install-scoped-p)
                            " | Install unavailable: explicit non-default profile required")
                          (if tabulated-list-entries "" " | No configurable providers")
                          (when hermes-tool-setup--post-status
                            (format " | Server-wide setup: %s" hermes-tool-setup--post-status))
                          (when (hermes-transport--get config 'active_search_backend)
                            (format " | Search: %s / Extract: %s"
                                    (hermes-transport--get config 'active_search_backend)
                                    (hermes-transport--get config 'active_extract_backend))))))
     (tabulated-list-print t)
     ;; Omitting provider asks the backend for its active selection.  A catalog
     ;; fetched for an inactive row is not evidence of the active model.
     (hermes-tool-setup--request
      "GET" "/models"
      (lambda (catalog) (setq hermes-tool-setup--model-catalog catalog))))))

(defun hermes-tool-setup--provider ()
  "Return the provider at point from the rendered snapshot."
  (hermes-tool-setup--idle)
  (or (seq-find (lambda (row)
                 (equal (tabulated-list-get-id)
                        (hermes-transport--get row 'name)))
               (hermes-transport--get hermes-tool-setup--config 'providers))
      (user-error "No provider on this line")))

(defun hermes-tool-setup--change
    (suffix body prompt &optional secrets method verify)
  "Confirm PROMPT then send BODY to SUFFIX, redacting SECRETS.
METHOD defaults to PUT.
Call VERIFY, or re-read readiness, after a semantically successful write."
  (hermes-tool-setup--idle)
  (let ((owner (hermes-tool-setup--owner)))
    (when (yes-or-no-p prompt)
      (unless (hermes-tool-setup--current-p owner)
        (user-error "Tool setup changed while confirming"))
      (setq hermes-tool-setup--busy t
            hermes-tool-setup--config nil hermes-tool-setup--model-catalog nil)
      (hermes-tool-setup--request
       (or method "PUT") suffix
       (lambda (result)
         (if (eq t (hermes-transport--get result 'ok))
             (progn
               (message "Hermes: change accepted; checking prerequisites (new sessions may be required)%s"
                        (if (eq t (hermes-transport--get result 'needs_nous_auth))
                            "; provider still needs Portal authentication" ""))
               (funcall (or verify #'hermes-tool-setup-refresh)))
           (message "Hermes: tool setup change refused; refresh to check state")))
       :body body :secrets secrets))))

(defun hermes-tool-setup-select-provider ()
  "Select the provider at point, optionally for a declared capability."
  (interactive)
  (let* ((provider (hermes-tool-setup--provider))
         (name (hermes-transport--get provider 'name))
         (owner (hermes-tool-setup--owner))
         (capabilities (hermes-transport--get provider 'capabilities))
         (capability (and capabilities
                          (completing-read "Capability: " capabilities nil t))))
    (unless (hermes-tool-setup--current-p owner)
      (user-error "Tool setup changed while choosing"))
    (hermes-tool-setup--change
     "/provider" (append `((provider . ,name))
                         (when capability `((capability . ,capability))))
     (format "Select %s for %s? " name hermes-tool-setup--name))))

(defun hermes-tool-setup-save-credentials ()
  "Save credentials declared by the provider at point.
Use secret input for every env field.
Blank input leaves an existing key intact."
  (interactive)
  (let* ((provider (hermes-tool-setup--provider))
         (owner (hermes-tool-setup--owner))
         (fields (hermes-transport--get provider 'env_vars))
         (env (delq nil
                    (mapcar
                     (lambda (field)
                       (unless (hermes-tool-setup--current-p owner)
                         (user-error "Tool setup changed while entering credentials"))
                       (let* ((key (hermes-transport--get field 'key))
                              (value (read-passwd (format "%s (blank keeps saved): " key))))
                         (unless (string-empty-p (string-trim value))
                           (cons key value)))) fields))))
    (unless (hermes-tool-setup--current-p owner)
      (user-error "Tool setup changed while entering credentials"))
    (unless env (user-error "No credential values entered"))
    (hermes-tool-setup--change
     "/env" `((env . ,env)) "Save these credentials on the selected backend? "
     (mapcar #'cdr env))))

(defun hermes-tool-setup--verify-model (provider model)
  "Read back PROVIDER's MODEL selection before rechecking prerequisites."
  (hermes-tool-setup--request
   "GET" "/models"
   (lambda (catalog)
     (message "Hermes: tool model %s"
              (if (equal model (hermes-transport--get catalog 'current))
                  "selection verified" "selection not confirmed by backend"))
     (hermes-tool-setup-refresh))
   :query `((provider . ,provider))))

(defun hermes-tool-setup-select-model ()
  "Fetch the selected provider's backend catalog and choose a model."
  (interactive)
  (let ((provider (hermes-transport--get (hermes-tool-setup--provider) 'name)))
    (hermes-tool-setup--request
     "GET" "/models"
     (lambda (catalog)
       (if (not (eq t (hermes-transport--get catalog 'has_models)))
           (message "Hermes: backend exposes no model catalog for this provider")
         (let* ((owner (hermes-tool-setup--owner))
                (models (hermes-transport--get catalog 'models))
                (choices (mapcar (lambda (row)
                                  (cons (format "%s [%s]" (hermes-transport--get row 'display)
                                                (hermes-transport--get row 'id))
                                        (hermes-transport--get row 'id))) models))
                (model (cdr (assoc (completing-read
                                      (format "Tool model for %s (current %s): " provider
                                              (or (hermes-transport--get catalog 'current) "unknown"))
                                      choices nil t) choices))))
           (when (and model (hermes-tool-setup--current-p owner))
             (hermes-tool-setup--change
              "/model" `((provider . ,provider) (model . ,model))
              (format "Select tool model %s? " model) nil nil
              (lambda () (hermes-tool-setup--verify-model provider model)))))))
     :query `((provider . ,provider)))))

(defun hermes-tool-setup--install-scoped-p ()
  "Return non-nil when the backend can explicitly scope an install hook."
  ;; Released backends omit the CLI override for default/current aliases.
  ;; Inherited dashboard home or a changed active_profile can redirect them.
  (and (stringp hermes-tool-setup--profile)
       (not (member (downcase (string-trim hermes-tool-setup--profile))
                    '("" "default" "current")))))

(defun hermes-tool-setup-run-post-setup ()
  "Run the provider's declared install hook on the backend after confirmation."
  (interactive)
  (unless (hermes-tool-setup--install-scoped-p)
    (user-error "Install unavailable: backend requires an explicit non-default profile for exact scope"))
  (let ((key (hermes-transport--get (hermes-tool-setup--provider) 'post_setup)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (user-error "This provider has no post-setup hook"))
    (hermes-tool-setup--change
     "/post-setup" `((key . ,key))
     (format "Run install hook %s on the backend (may install software)? " key)
     nil "POST")))

(defun hermes-tool-setup-post-setup-status ()
  "Read server-wide post-setup status, then recheck current prerequisites.
The backend has no per-profile job identity.  Do not attribute this status to
this toolset or infer readiness from the last process exit code."
  (interactive)
  (hermes-tool-setup--idle)
  (hermes-tool-setup--request
   "GET" "/api/actions/tools-post-setup/status"
   (lambda (result)
     (setq hermes-tool-setup--post-status
           (if (eq t (hermes-transport--get result 'running)) "running"
             (let ((exit (hermes-transport--get result 'exit_code)))
               (if (numberp exit) (format "exited %s" exit) "no known result"))))
     (message "Hermes: server-wide post-setup: %s; not scoped to this toolset/profile"
              hermes-tool-setup--post-status)
     (hermes-tool-setup-refresh))
   :query '((lines . 1)) :absolute t))

(defvar hermes-tool-setup-mode-map)
(keymap-popup-define hermes-tool-setup-mode-map
  "Keymap for `hermes-tool-setup-mode'."
  :parent tabulated-list-mode-map
  :description "Tool setup"
  :group "Configure"
  "p" ((lambda () (hermes-tool-setup--setting-description 'provider))
       hermes-tool-setup-select-provider
       :inapt-if (lambda () hermes-tool-setup--busy))
  "k" ("Save credentials" hermes-tool-setup-save-credentials)
  "m" ((lambda () (hermes-tool-setup--setting-description 'model))
       hermes-tool-setup-select-model
       :inapt-if (lambda () hermes-tool-setup--busy))
  :group "Setup"
  "i" ("Run install hook" hermes-tool-setup-run-post-setup)
  "s" ("Post-setup status" hermes-tool-setup-post-setup-status)
  "g" ("Check readiness" hermes-tool-setup-refresh)
  "?" ("Help" hermes-tool-setup-mode-map-popup))

(define-derived-mode hermes-tool-setup-mode tabulated-list-mode "Hermes Tool Setup"
  "Configure backend-declared providers, credentials and models.
Readiness reports backend prerequisites, not a successful tool invocation."
  :interactive nil
  (setq-local tabulated-list-format
              [("Provider" 26 t) ("Selection" 10 t) ("Readiness" 16 t)
               ("Credentials" 38 nil) ("Details" 40 nil)])
  (setq-local revert-buffer-function #'hermes-tool-setup-refresh)
  (setq-local hermes-browser--snapshot-variables '(hermes-tool-setup--config hermes-tool-setup--model-catalog hermes-tool-setup--post-status))
  (hermes-browser--next-request-generation)
  (tabulated-list-init-header))

;;;###autoload
(defun hermes-tool-setup (name &optional profile)
  "Open toolset NAME setup for PROFILE on the invoking Hermes instance.
PROFILE nil means the dashboard process's default profile."
  (interactive "sToolset: ")
  (let ((instance (hermes-instance-resolve))
        (buffer (generate-new-buffer (format "*Hermes Tool Setup: %s*" name))))
    (with-current-buffer buffer
      (hermes-tool-setup-mode)
      (hermes-browser--own-instance instance)
      (setq hermes-tool-setup--name name hermes-tool-setup--profile profile)
      (hermes-tool-setup-refresh))
    (pop-to-buffer buffer)))

(provide 'hermes-tool-setup)
;;; hermes-tool-setup.el ends here
