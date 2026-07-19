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

;; Connect a provider to Hermes from Emacs.  When the dashboard has no usable
;; credentials a first-run user otherwise hits a raw connection failure with
;; no way forward; this lists the dashboard's unauthenticated providers, reads
;; an API key, and saves it through `model.save_key'.
;;
;; Every unauthenticated provider is offered without client-side auth
;; classification: the gateway accepts a pasted key or returns its own error
;; on save, so OAuth/managed providers fail with the backend's message rather
;; than being filtered here.

;;; Code:

(require 'seq)
(require 'hermes-promise)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)

(defvar hermes-onboarding-connected-function #'ignore
  "Function called after provider credentials are saved successfully.")

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
The candidate is the provider name; an `:annotation-function' tags each `API'.
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
     (hermes-dashboard-transport-invalidate-model-options)
     (funcall hermes-onboarding-connected-function)
     (message "Connected Hermes provider %s"
              (hermes-onboarding--provider-name
               (hermes-transport--get result 'provider))))))

(provide 'hermes-onboarding)
;;; hermes-onboarding.el ends here
