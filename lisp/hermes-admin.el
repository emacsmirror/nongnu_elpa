;;; hermes-admin.el --- Pairing and webhook administration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
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

;; Administrative lists for the selected instance's server profile.  These
;; views share browser request ownership but also fence mutation admission and
;; keep a one-time webhook secret out of list snapshots, messages and history.
;; No gateway restart, platform enablement, or named-profile override is implicit.

;;; Code:

(require 'keymap-popup)
(require 'hermes-browser)
(require 'url-util)

(defvar-local hermes-admin--state 'unknown
  "Current list state: unknown, loading, ready, mutating, failed or uncertain.")
(defvar-local hermes-admin--snapshot nil
  "Sanitized, authoritative list snapshot for this buffer.")
(defvar-local hermes-admin--snapshot-owner nil
  "Exact request owner that produced the current authoritative list.")
(defvar-local hermes-admin--secret-instance nil
  "Instance identity and value that own the one-time secret.")
(defvar-local hermes-admin--secret nil
  "One-time webhook secret, never part of the list snapshot.")
(defvar-local hermes-admin--secret-buffer nil
  "Explicitly revealed secret buffer owned by this browser.")

(defun hermes-admin--forget-secret ()
  "Retire this browser's one-time secret and any explicit reveal."
  (interactive)
  (when (buffer-live-p hermes-admin--secret-buffer)
    (with-current-buffer hermes-admin--secret-buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (set-buffer-modified-p nil))
    (kill-buffer hermes-admin--secret-buffer))
  (setq hermes-admin--secret-buffer nil)
  (when (stringp hermes-admin--secret) (clear-string hermes-admin--secret))
  (setq hermes-admin--secret nil))

(defun hermes-admin--stop ()
  "Retire requests and sensitive display state owned by this browser."
  (hermes-browser--next-request-generation)
  (hermes-admin--forget-secret)
  (setq hermes-admin--state 'unknown hermes-admin--snapshot nil))

(defun hermes-admin--header ()
  "Return instance scope and the current administrative state."
  (format " %s · Server profile · %s%s · ? Help "
          (if (hermes-instance--valid-p hermes-instance)
              (hermes-instance-name hermes-instance) "No instance")
          (propertize (pcase hermes-admin--state
                        ('ready (format "ready · %d rows" (length tabulated-list-entries)))
                        ('failed "Read failed; refresh or check dashboard authentication")
                        ('uncertain "Change outcome unknown; refresh before retry")
                        (_ (symbol-name hermes-admin--state)))
                      'face (pcase hermes-admin--state
                              ('ready 'success)
                              ((or 'failed 'uncertain) 'warning)
                              (_ 'shadow)))
          (if hermes-admin--secret " · One-time secret available (r / c)" "")))

(defun hermes-admin--scope (client)
  "Return CLIENT's connection generation and exact endpoint."
  (list (hermes-dashboard-transport-client-generation client)
        (copy-sequence (hermes-dashboard-transport--api-client-base-url client))))

(defun hermes-admin--instance-value (instance)
  "Return an independent value snapshot of INSTANCE."
  (mapcar #'copy-sequence
          (list (hermes-instance-id instance)
                (hermes-instance-name instance) (hermes-instance-url instance))))

(defun hermes-admin--owner ()
  "Capture the current browser's request and instance ownership."
  (list (current-buffer) hermes-browser--request-generation major-mode
        hermes-instance (hermes-admin--instance-value hermes-instance)))

(defun hermes-admin--current-p (owner)
  "Return non-nil if OWNER still owns its browser and instance."
  (pcase-let ((`(,buffer ,generation ,mode ,instance ,value) owner))
    (and (hermes-browser--request-current-mode-p buffer generation mode)
         (eq instance (buffer-local-value 'hermes-instance buffer))
         (equal (hermes-admin--instance-value instance) value))))

(defun hermes-admin--prompt-current-p (owner)
  "Return non-nil if the current buffer still owns prompt OWNER."
  (and (eq (current-buffer) (car owner)) (hermes-admin--current-p owner)))

(defun hermes-admin--require-ready ()
  "Reject administrative commands outside a current authoritative list."
  (unless (and (derived-mode-p 'hermes-admin-mode)
               (eq hermes-admin--state 'ready)
               hermes-admin--snapshot-owner
               (hermes-admin--current-p hermes-admin--snapshot-owner)
               (hermes-instance--valid-p hermes-instance))
    (user-error "Refresh this administrative list before changing it")))

(defun hermes-admin--request (client owner scope method path body mutation)
  "Request METHOD PATH BODY through CLIENT under OWNER and SCOPE.
For MUTATION, fence transport entry again after asynchronous authentication."
  (if (or (not mutation) (hermes-dashboard-transport--api-client-token client))
      (hermes-dashboard-transport-api-request-async
       method path :body body :client client :timeout 30)
    (let ((hermes-dashboard-transport-url (cadr scope)))
      (hermes--promise-then
       (hermes-dashboard-transport-api-auth-async)
       (lambda (auth)
         (unless (and (hermes-admin--current-p owner)
                      (equal scope (hermes-admin--scope client)))
           (error "Retired operation"))
         (hermes--promise-then
          (hermes-dashboard-transport--http-json-request-async
           (hermes-dashboard-transport--api-request-plist
            auth method path :body body :timeout 30))
          (lambda (response)
            (hermes-dashboard-transport--api-response-body method path response))))))))

(defun hermes-admin--fail (owner mutation)
  "Invalidate OWNER's actionable state after failure of a read or MUTATION."
  (when (hermes-admin--current-p owner)
    (with-current-buffer (car owner)
      (setq hermes-admin--state (if mutation 'uncertain 'failed)))))

(defun hermes-admin--run (method path body success &optional mutation)
  "Run METHOD PATH with BODY, then SUCCESS in the exact owning browser.
MUTATION means a failed or disconnected request has an uncertain outcome.
Never display remote error bodies: they may contain credentials."
  (hermes-browser--next-request-generation)
  (let ((owner (hermes-admin--owner)))
    (setq hermes-admin--state (if mutation 'mutating 'loading))
    (condition-case nil
        (hermes-browser--run-on-client
         (lambda (client)
           (let ((scope (hermes-admin--scope client)))
             (hermes--promise-catch
              (hermes--promise-then
               (condition-case nil
                   (if (hermes-admin--current-p owner)
                       (hermes-admin--request
                        client owner scope method path body mutation)
                     (hermes--promise-rejected "Retired operation"))
                 ((error quit) (hermes--promise-rejected "Request failed")))
               (lambda (result)
                 ;; Run before shared-client release advances its generation.
                 (unwind-protect
                     (when (hermes-admin--current-p owner)
                       (with-current-buffer (car owner)
                         (if (equal scope (hermes-admin--scope client))
                             (funcall success result)
                           (hermes-admin--fail owner mutation))))
                   ;; Retained promises must not become another secret store.
                   (when-let* ((secret (hermes-transport--get result 'secret))
                               ((stringp secret)))
                     (clear-string secret)))
                 nil))
              (lambda (_reason) (hermes-admin--fail owner mutation)))))
         nil (lambda (_reason) (hermes-admin--fail owner mutation)))
      ((error quit) (hermes-admin--fail owner mutation)))))

(defun hermes-admin--field (record key)
  "Return a plain string from RECORD's KEY, or an empty string."
  (or (hermes-transport--scalar-string (hermes-transport--get record key)) ""))

(defun hermes-admin--pairing-rows (result)
  "Return non-secret pairing rows from RESULT."
  (apply #'append
         (mapcar
          (lambda (kind)
            (mapcar
             (lambda (record)
               (let ((platform (hermes-admin--field record 'platform))
                     (user (hermes-admin--field record 'user_id))
                     (request (hermes-admin--field record 'request_id)))
                 (list (list kind platform user request)
                       (vector (hermes-browser--status-cell (symbol-name kind))
                               platform user
                               (hermes-admin--field record 'user_name)))))
             (hermes-transport--get result kind)))
          '(pending approved))))

(defun hermes-admin--webhook-rows (result)
  "Return non-secret webhook rows from RESULT."
  (mapcar
   (lambda (record)
     (let ((name (hermes-admin--field record 'name))
           (enabled (eq t (hermes-transport--get record 'enabled))))
       (list (list name enabled)
             (vector name
                     (hermes-browser--status-cell (if enabled "enabled" "disabled"))
                     (hermes-admin--field record 'deliver)
                     (hermes-admin--field record 'description)))))
   (hermes-transport--get result 'subscriptions)))

(defun hermes-admin-quit ()
  "Close the administrative view and forget its one-time secret."
  (interactive)
  (quit-window t))

(defun hermes-admin--revert (&rest _)
  "Refresh this administrative browser without retrying any mutation."
  (interactive)
  (when (eq hermes-admin--state 'mutating)
    (user-error "A change is pending; wait for its outcome before refreshing"))
  (let ((pairing (derived-mode-p 'hermes-pairing-mode)))
    (hermes-admin--run
     "GET" (if pairing "/api/pairing" "/api/webhooks") nil
     (lambda (result)
       (unless (and (hermes-transport--field-present-p
                     result (if pairing 'pending 'subscriptions))
                    (or (not pairing)
                        (hermes-transport--field-present-p result 'approved)))
         (error "Invalid administrative list"))
       (setq hermes-admin--snapshot
             (if pairing (hermes-admin--pairing-rows result)
               (hermes-admin--webhook-rows result))
             tabulated-list-entries hermes-admin--snapshot
             hermes-admin--snapshot-owner (hermes-admin--owner)
             hermes-admin--state 'ready)
       (tabulated-list-print t)))))

(defun hermes-admin--open (mode title)
  "Open MODE with TITLE for the selected instance."
  (let* ((instance (hermes-instance-resolve))
         (buffer (cl-find-if
                  (lambda (buffer)
                    (and (eq mode (buffer-local-value 'major-mode buffer))
                         (equal instance (buffer-local-value 'hermes-instance buffer))))
                  (buffer-list))))
    (unless buffer
      (setq buffer (generate-new-buffer title))
      (with-current-buffer buffer
        (funcall mode)
        (hermes-browser--own-instance instance)
        (setq-local header-line-format '(:eval (hermes-admin--header)))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-admin--revert))))

;;;###autoload
(defun hermes-list-pairing ()
  "Browse pending and approved users in the selected instance's server profile."
  (interactive)
  (hermes-admin--open 'hermes-pairing-mode "*Hermes Pairing*"))

;;;###autoload
(defun hermes-list-webhooks ()
  "Browse webhook routes in the selected instance's server profile."
  (interactive)
  (hermes-admin--open 'hermes-webhooks-mode "*Hermes Webhooks*"))

(defun hermes-admin--change (question method path body &optional created)
  "Confirm QUESTION, then send METHOD PATH BODY for the current instance.
CREATED means retain only the one-time secret from the create response."
  (hermes-admin--require-ready)
  (let ((owner (hermes-admin--owner)))
    (when (yes-or-no-p (format "%s on %s (server profile)? " question
                              (hermes-instance-name hermes-instance)))
      (unless (hermes-admin--prompt-current-p owner)
        (user-error "Administrative view changed during confirmation"))
      (hermes-admin--require-ready)
      (when created (hermes-admin--forget-secret))
      (unless (hermes-admin--prompt-current-p owner)
        (user-error "Administrative view changed during secret cleanup"))
      (hermes-admin--run
       method path body
       (lambda (result)
         (unless (if created
                     (hermes-transport--non-empty-string
                      (hermes-transport--get result 'secret))
                   (eq t (hermes-transport--get result 'ok)))
           (error "Unconfirmed administrative change"))
         (when created
           (setq hermes-admin--secret
                 (copy-sequence (hermes-transport--get result 'secret))
                 hermes-admin--secret-instance
                 (list hermes-instance (hermes-admin--instance-value hermes-instance))))
         ;; The receipt is not a list snapshot: read the authoritative list.
         (setq hermes-admin--state 'unknown)
         (hermes-admin--revert))
       t))))

(defun hermes-pairing-approve ()
  "Confirm and approve the exact pending request at point."
  (interactive)
  (hermes-admin--require-ready)
  (pcase-let ((`(,kind ,platform ,user ,request) (tabulated-list-get-id)))
    (unless (and (eq kind 'pending) (not (string-empty-p request)))
      (user-error "Select a pending request with a request ID"))
    (hermes-admin--change
     (format "Grant messaging access to %s on %s" user platform)
     "POST" "/api/pairing/approve"
     `((platform . ,platform) (request_id . ,request)))))

(defun hermes-pairing-revoke ()
  "Confirm and revoke the approved user's access at point."
  (interactive)
  (hermes-admin--require-ready)
  (pcase-let ((`(,kind ,platform ,user ,_) (tabulated-list-get-id)))
    (unless (eq kind 'approved) (user-error "Select an approved user"))
    (hermes-admin--change
     (format "Revoke messaging access for %s on %s" user platform)
     "POST" "/api/pairing/revoke" `((platform . ,platform) (user_id . ,user)))))

(defun hermes-pairing-clear-pending ()
  "Confirm and clear all pending requests across this server profile's platforms."
  (interactive)
  (unless (derived-mode-p 'hermes-pairing-mode) (user-error "Not a pairing view"))
  (hermes-admin--change "Clear ALL pending pairing requests across ALL platforms"
                       "POST" "/api/pairing/clear-pending" nil))

(defun hermes-webhooks-create ()
  "Create a confirmed log-delivered webhook with a generated one-time secret.
Creation replaces an existing route of the same name on the server.  This
command refuses names in the last list and warns about concurrent replacement.
Platform enablement is managed separately; this command never starts a gateway."
  (interactive)
  (unless (derived-mode-p 'hermes-webhooks-mode) (user-error "Not a webhook view"))
  (hermes-admin--require-ready)
  (let* ((owner (hermes-admin--owner))
         (name (read-string "Webhook name (lowercase, digits, _ or -): "))
         (description (read-string "Description: "))
         (prompt (read-string "Agent prompt (empty for default): ")))
    (unless (let ((case-fold-search nil))
              (string-match-p "\\`[a-z0-9][a-z0-9_-]*\\'" name))
      (user-error "Invalid webhook name"))
    (unless (hermes-admin--prompt-current-p owner)
      (user-error "Administrative view changed while reading webhook fields"))
    (when (cl-find name hermes-admin--snapshot :key #'caar :test #'equal)
      (user-error "A webhook with that name already exists; choose another name"))
    (hermes-admin--change
     (format "Create log-delivered webhook %s (REPLACES any concurrently created route of that name)" name)
     "POST" "/api/webhooks"
     `((name . ,name) (description . ,description) (prompt . ,prompt)
       (events . []) (skills . []) (deliver . "log") (deliver_only . :false)) t)))

(defun hermes-webhooks--selected ()
  "Return the webhook identity at point, requiring a current list."
  (unless (derived-mode-p 'hermes-webhooks-mode) (user-error "Not a webhook view"))
  (hermes-admin--require-ready)
  (let* ((identity (or (tabulated-list-get-id) (user-error "Select a webhook")))
         (name (car identity))
         ;; Python str.strip whitespace within the supported ASCII range.
         (whitespace "[\t-\r\u001c-\u0020]+"))
    ;; Emacs and Python disagree on Unicode lowercasing (for example, K).
    ;; Keep source names visible, but do not risk mutating a different key.
    (when (and (stringp name) (string-match-p "[^\0-\177]" name))
      (user-error "Unsupported webhook source name: only ASCII names can be changed"))
    ;; The backend lists source keys verbatim but mutates name.strip().lower().
    ;; Do not apply create's stricter syntax to exactly addressable legacy keys.
    (unless (and (stringp name) (not (string-empty-p name))
                 (equal name (downcase (string-trim name whitespace whitespace))))
      (user-error "The server cannot address this webhook name exactly"))
    identity))

(defun hermes-webhooks-toggle ()
  "Confirm and change the selected webhook's enabled state."
  (interactive)
  (pcase-let ((`(,name ,enabled) (hermes-webhooks--selected)))
    (hermes-admin--change
     (format "%s webhook %s" (if enabled "Disable" "Enable") name)
     "PUT" (format "/api/webhooks/%s/enabled" (url-hexify-string name))
     `((enabled . ,(if enabled :false t))))))

(defun hermes-webhooks-delete ()
  "Confirm and delete the selected webhook and its secret."
  (interactive)
  (let ((name (car (hermes-webhooks--selected))))
    (hermes-admin--change (format "Delete webhook %s and its secret" name)
                         "DELETE" (concat "/api/webhooks/" (url-hexify-string name)) nil)))

(defun hermes-webhooks--require-secret ()
  "Require a secret belonging to the current instance."
  (unless (and hermes-admin--secret
               (eq hermes-instance (car hermes-admin--secret-instance))
               (equal (hermes-admin--instance-value hermes-instance)
                      (cadr hermes-admin--secret-instance)))
    (hermes-admin--forget-secret)
    (user-error "No one-time secret is available")))

(defun hermes-webhooks-reveal-secret ()
  "Explicitly reveal the last created secret in a temporary, undo-free buffer.
The buffer is destroyed on dismissal, browser teardown, or the next creation.
It is never written to disk or inserted into a chat or message log."
  (interactive)
  (hermes-webhooks--require-secret)
  (let ((owner (hermes-admin--owner)))
    (when (yes-or-no-p "Reveal the one-time webhook secret on screen? ")
      (unless (hermes-admin--prompt-current-p owner) (user-error "View changed"))
      (unless (buffer-live-p hermes-admin--secret-buffer)
        (setq hermes-admin--secret-buffer (generate-new-buffer " *Hermes one-time secret*")))
      (let ((secret hermes-admin--secret))
        (with-current-buffer hermes-admin--secret-buffer
          (special-mode)
          (use-local-map (copy-keymap special-mode-map))
          (setq-local buffer-undo-list t)
          (setq-local buffer-offer-save nil)
          (setq-local auto-save-default nil)
          (let ((inhibit-read-only t)) (erase-buffer) (insert secret))
          (local-set-key (kbd "q") #'kill-current-buffer)))
      (pop-to-buffer hermes-admin--secret-buffer))))

(defun hermes-webhooks-copy-secret ()
  "Explicitly copy the one-time secret to the kill ring and clipboard.
Those destinations may persist or synchronize it; only do this intentionally."
  (interactive)
  (hermes-webhooks--require-secret)
  (let ((owner (hermes-admin--owner)))
    (when (yes-or-no-p "Copy secret to kill ring/clipboard (may persist or sync)? ")
      (unless (hermes-admin--prompt-current-p owner) (user-error "View changed"))
      (kill-new (copy-sequence hermes-admin--secret)))))

(keymap-popup-define hermes-pairing-mode-map
  "Pairing administration commands."
  :parent tabulated-list-mode-map :popup-key "?" :exit-key "C-g"
  :group ("Trust" :inapt-if (lambda () (not (eq hermes-admin--state 'ready))))
  "a" ("Approve request" hermes-pairing-approve)
  "d" ("Revoke access" hermes-pairing-revoke)
  "D" ("Clear all pending" hermes-pairing-clear-pending)
  :group "View"
  "g" ("Refresh" hermes-admin--revert)
  "q" ("Quit" hermes-admin-quit)
  "?" ("Help" hermes-pairing-mode-map-popup))

(keymap-popup-define hermes-webhooks-mode-map
  "Webhook administration commands."
  :parent tabulated-list-mode-map :popup-key "?" :exit-key "C-g"
  :group ("Routes" :inapt-if (lambda () (not (eq hermes-admin--state 'ready))))
  "a" ("Create route" hermes-webhooks-create)
  "t" ("Toggle enabled" hermes-webhooks-toggle)
  "d" ("Delete route" hermes-webhooks-delete)
  :group ("One-time secret" :inapt-if (lambda () (null hermes-admin--secret)))
  "r" ("Reveal" hermes-webhooks-reveal-secret)
  "c" ("Copy explicitly" hermes-webhooks-copy-secret)
  "x" ("Forget" hermes-admin--forget-secret)
  :group "View"
  "g" ("Refresh" hermes-admin--revert)
  "q" ("Quit" hermes-admin-quit)
  "?" ("Help" hermes-webhooks-mode-map-popup))

(define-derived-mode hermes-admin-mode tabulated-list-mode "Hermes Admin"
  "Parent mode for instance-owned administrative lists."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-admin--revert)
  (setq-local header-line-format '(:eval (hermes-admin--header)))
  (setq-local hermes-browser--snapshot-variables '(hermes-admin--snapshot))
  (add-hook 'kill-buffer-hook #'hermes-admin--stop nil t)
  (add-hook 'change-major-mode-hook #'hermes-admin--stop nil t))

(define-derived-mode hermes-pairing-mode hermes-admin-mode "Hermes Pairing"
  "Browse pending and approved messaging users."
  :interactive nil
  (setq tabulated-list-format [("State" 10 t) ("Platform" 12 t)
                               ("User ID" 28 t) ("Name" 24 t)])
  (tabulated-list-init-header))

(define-derived-mode hermes-webhooks-mode hermes-admin-mode "Hermes Webhooks"
  "Browse webhook routes without exposing their secrets."
  :interactive nil
  (setq tabulated-list-format [("Name" 24 t) ("State" 10 t)
                               ("Delivery" 12 t) ("Description" 40 t)])
  (tabulated-list-init-header))

(provide 'hermes-admin)
;;; hermes-admin.el ends here
