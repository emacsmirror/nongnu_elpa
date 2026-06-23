;;; hermes-onboarding.el --- Provider onboarding for Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

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

;; Connect an API-key provider to Hermes from Emacs.  When the dashboard has no
;; usable credentials a first-run user otherwise hits a raw connection failure
;; with no way forward; this lists the dashboard's unauthenticated API-key
;; providers, reads a key, and saves it through `model.save_key'.
;;
;; Only API-key providers are offered: OAuth and managed-install providers
;; cannot be connected by pasting a key and must be configured elsewhere.

;;; Code:

(require 'seq)
(require 'hermes-promise)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-browser)

;;; Pure provider model

(defun hermes-onboarding--api-key-provider-p (provider)
  "Return non-nil when PROVIDER is an unauthenticated API-key provider.
Such providers can be connected by pasting a key.  `auth_type'/`key_env' are
present only on unauthenticated skeleton rows, so this predicate also bounds the
domain to unconnected providers."
  (and (not (eq (hermes-transport--get provider 'authenticated) t))
       (equal (hermes-transport--scalar-string
               (hermes-transport--get provider 'auth_type))
              "api_key")
       (hermes-transport--non-empty-string
        (hermes-transport--get provider 'key_env))))

(defun hermes-onboarding--unauthed-providers (result)
  "Return the connectable API-key providers in a `model.options' RESULT."
  (seq-filter #'hermes-onboarding--api-key-provider-p
              (hermes-transport--get result 'providers)))

(defun hermes-onboarding--provider-name (provider)
  "Return PROVIDER's display name, falling back to its slug."
  (or (hermes-transport--scalar-string (hermes-transport--get provider 'name))
      (hermes-transport--scalar-string (hermes-transport--get provider 'slug))
      "provider"))

(defun hermes-onboarding--provider-label (provider)
  "Return a completion label for PROVIDER: its name and the env var to paste."
  (let ((name (hermes-onboarding--provider-name provider))
        (env (hermes-transport--scalar-string
              (hermes-transport--get provider 'key_env))))
    (if env (format "%s (%s)" name env) name)))

;;; User interaction

(defun hermes-onboarding--choose-provider (result)
  "Return an API-key provider chosen with completion from RESULT.
Signal a `user-error' when none are connectable."
  (let ((providers (hermes-onboarding--unauthed-providers result)))
    (unless providers
      (user-error "No API-key providers to connect; all are authenticated or use OAuth"))
    (let* ((labels (mapcar (lambda (p)
                             (cons (hermes-onboarding--provider-label p) p))
                           providers))
           (choice (completing-read "Connect provider: " labels nil t)))
      (or (cdr (assoc choice labels))
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
       #'hermes-dashboard-transport-model-options client)
      (lambda (result)
        (let ((provider (hermes-onboarding--choose-provider result)))
          (hermes-dashboard-transport-call-fn
           #'hermes-dashboard-transport-model-save-key
           client
           (hermes-transport--scalar-string
            (hermes-transport--get provider 'slug))
           (hermes-onboarding--read-key provider))))))
   (lambda (result)
     (message "Connected Hermes provider %s"
              (hermes-onboarding--provider-name
               (hermes-transport--get result 'provider))))))

(provide 'hermes-onboarding)
;;; hermes-onboarding.el ends here
