;;; hermes-messaging.el --- Messaging platform browser for Hermes  -*- lexical-binding: t; -*-

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

;; Profile-scoped dashboard browser for messaging platforms.  It displays only
;; catalog metadata and set/unset flags, then uses the dashboard's allowed env
;; schema for set/clear operations.  Credential values never enter a browser
;; buffer or message.

;;; Code:

(require 'cl-lib)
(require 'keymap-popup)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-browser)

(defvar-local hermes-messaging-profile nil
  "Dashboard profile owned by the current messaging browser.")

(defvar-local hermes-messaging--platforms nil
  "Hash table mapping platform ids to their latest catalog objects.")

(defvar-local hermes-messaging--mutation-in-flight nil
  "Identity token for the current catalog mutation, or nil.")

(defvar hermes-messaging--display-generation 0
  "Generation of the newest displayed messaging catalog request.")

(defun hermes-messaging--field (object key)
  "Return OBJECT's KEY as a display string, or empty."
  (or (hermes-transport--field object key) ""))

(defun hermes-messaging--platform-list (result)
  "Return RESULT's platform list."
  (let ((platforms (hermes-transport--get result 'platforms)))
    (if (listp platforms) platforms nil)))

(defun hermes-messaging--boolean-label (object key true-label false-label)
  "Describe OBJECT's boolean KEY with TRUE-LABEL or FALSE-LABEL."
  (if (eq (hermes-transport--get object key) t) true-label false-label))

(defun hermes-messaging--row (platform)
  "Return one tabulated row for PLATFORM."
  (let ((id (hermes-messaging--field platform 'id)))
    (list id
          (vector
           (hermes-browser--face-cell
            (hermes-messaging--field platform 'name) 'hermes-browser-name)
           (hermes-browser--status-cell
            (hermes-messaging--boolean-label platform 'enabled "on" "off")
            'hermes-browser-enabled)
           (hermes-browser--status-cell
            (hermes-messaging--boolean-label platform 'configured "yes" "no")
            'hermes-browser-status)
           (hermes-browser--status-cell
            (hermes-messaging--field platform 'state) 'hermes-browser-state)
           (hermes-browser--face-cell
            (hermes-messaging--field platform 'description)
            'hermes-browser-description)))))

(defun hermes-messaging--rows (result)
  "Return tabulated rows from messaging catalog RESULT."
  (mapcar #'hermes-messaging--row
          (hermes-messaging--platform-list result)))

(defun hermes-messaging--query (profile)
  "Return dashboard query parameters for PROFILE."
  (and (hermes-transport--non-empty-string profile)
       `((profile . ,profile))))

(cl-defun hermes-messaging--api
    (client method path profile &optional body &key secrets)
  "Return a messaging METHOD PATH promise through CLIENT for PROFILE.
BODY is JSON data.  SECRETS are redacted from transport errors."
  (hermes-dashboard-transport-api-request-async
   method (concat "/api/messaging" path)
   :body body :query (hermes-messaging--query profile)
   :secrets secrets :client client))

(defun hermes-messaging--platform-path (id &rest segments)
  "Return a platform path for ID extended by SEGMENTS."
  (concat "/platforms/" (url-hexify-string id) (apply #'concat segments)))

(defun hermes-messaging--profile-label (profile)
  "Return PROFILE's display label."
  (or (hermes-transport--non-empty-string profile) "current"))

(defun hermes-messaging--buffer-name (profile)
  "Return the messaging browser buffer name for PROFILE."
  (format "*Hermes Messaging: %s*" (hermes-messaging--profile-label profile)))

(defun hermes-messaging--request-current-p (buffer generation profile)
  "Return non-nil when BUFFER still owns GENERATION and PROFILE."
  (and (hermes-browser--request-current-mode-p
        buffer generation 'hermes-messaging-mode)
       (with-current-buffer buffer
         (equal hermes-messaging-profile profile))))

(defun hermes-messaging--operation-current-p
    (buffer generation profile &optional display-generation)
  "Return non-nil when BUFFER still owns this messaging operation.
GENERATION and PROFILE identify the buffer request.  DISPLAY-GENERATION, when
non-nil, also requires this to be the newest cross-profile display request."
  (and (hermes-messaging--request-current-p buffer generation profile)
       (or (null display-generation)
           (eql display-generation hermes-messaging--display-generation))))

(defun hermes-messaging--require-mutation-idle ()
  "Signal `user-error' while this catalog has an unsettled mutation."
  (when hermes-messaging--mutation-in-flight
    (user-error "A messaging update is still in progress")))

(defun hermes-messaging--clear-mutation (buffer token)
  "Clear BUFFER's mutation lock when it still belongs to TOKEN."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq hermes-messaging--mutation-in-flight token)
        (setq hermes-messaging--mutation-in-flight nil)))))

(defun hermes-messaging--run-owned
    (buffer generation profile make-promise on-success
            &optional secrets display-generation)
  "Run MAKE-PROMISE while BUFFER owns this messaging operation.
MAKE-PROMISE receives a dashboard client.  ON-SUCCESS and rejection messages
apply only while BUFFER, GENERATION, PROFILE, and optional DISPLAY-GENERATION
remain current.  SECRETS are removed from current rejection messages."
  (hermes-browser--with-client
   (lambda (client done)
     (hermes--promise-catch
      (hermes--promise-then
       (condition-case err
           (hermes--promise-finally (funcall make-promise client) done)
         ((error quit)
          (funcall done)
          (hermes--promise-rejected (error-message-string err))))
       (lambda (result)
         (when (hermes-messaging--operation-current-p
                buffer generation profile display-generation)
           (funcall on-success result))))
      (lambda (reason)
        (when (hermes-messaging--operation-current-p
               buffer generation profile display-generation)
          (message
           "Hermes: %s"
           (hermes-dashboard-transport--redact-secret reason secrets))))))))

(defun hermes-messaging--remember-platforms (result)
  "Replace the current platform table from RESULT."
  (unless (hash-table-p hermes-messaging--platforms)
    (setq hermes-messaging--platforms (make-hash-table :test #'equal)))
  (clrhash hermes-messaging--platforms)
  (dolist (platform (hermes-messaging--platform-list result))
    (when-let* ((id (hermes-transport--non-empty-string
                     (hermes-messaging--field platform 'id))))
      (puthash id platform hermes-messaging--platforms))))

(defun hermes-messaging--render (result &optional buffer)
  "Render messaging catalog RESULT in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'hermes-messaging-mode)
      (hermes-messaging-mode))
    (hermes-messaging--remember-platforms result)
    (setq tabulated-list-entries (hermes-messaging--rows result))
    (tabulated-list-print t)))

(defun hermes-messaging--fetch (profile &optional display target generation)
  "Fetch PROFILE's catalog.
DISPLAY pops TARGET after a current result.  GENERATION identifies ownership."
  (let ((target (or target
                    (get-buffer-create (hermes-messaging--buffer-name profile)))))
    (with-current-buffer target
      (unless (derived-mode-p 'hermes-messaging-mode)
        (hermes-messaging-mode))
      (hermes-messaging--require-mutation-idle)
      (setq hermes-messaging-profile profile))
    (let ((generation (or generation
                          (with-current-buffer target
                            (hermes-browser--next-request-generation))))
          (display-generation
           (and display (cl-incf hermes-messaging--display-generation))))
      (hermes-messaging--run-owned
       target generation profile
       (lambda (client)
         (hermes-messaging--api client "GET" "/platforms" profile))
       (lambda (result)
         (hermes-messaging--render result target)
         (when display (pop-to-buffer target)))
       nil display-generation))))

(defun hermes-messaging--revert (&rest _)
  "Refresh the current profile's messaging catalog."
  (hermes-messaging--require-mutation-idle)
  (let ((target (current-buffer))
        (profile hermes-messaging-profile)
        (generation (hermes-browser--next-request-generation)))
    (hermes-messaging--fetch profile nil target generation)))

(defun hermes-messaging--profile-names ()
  "Return cached profile names for interactive selection."
  (mapcar (lambda (profile) (hermes-transport--field profile 'name))
          (hermes-transport--get
           (hermes-dashboard-transport-cached-profile-list) 'profiles)))

(defun hermes-messaging--read-profile ()
  "Read a dashboard profile, with blank meaning the current profile."
  (let* ((names (delq nil (hermes-messaging--profile-names)))
         (profile (if names
                      (completing-read
                       "Messaging profile (blank for current): " names nil nil)
                    (read-string "Messaging profile (blank for current): "))))
    (hermes-transport--non-empty-string (string-trim profile))))

(defun hermes-messaging--id-at-point ()
  "Return the platform id at point, or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No messaging platform on this line")))

(defun hermes-messaging--platform-at-point ()
  "Return the current platform object."
  (let ((id (hermes-messaging--id-at-point)))
    (or (and (hash-table-p hermes-messaging--platforms)
             (gethash id hermes-messaging--platforms))
        (user-error "No catalog details for platform %s" id))))

(defun hermes-messaging--env-fields (platform)
  "Return PLATFORM's documented environment field schemas."
  (let ((fields (hermes-transport--get platform 'env_vars)))
    (if (listp fields) fields nil)))

(defun hermes-messaging--env-field (platform key)
  "Return PLATFORM's environment field named KEY."
  (seq-find (lambda (field)
              (equal (hermes-messaging--field field 'key) key))
            (hermes-messaging--env-fields platform)))

(defun hermes-messaging--read-env-key (platform action)
  "Read one documented PLATFORM env key for ACTION."
  (let ((keys (delq nil
                    (mapcar (lambda (field)
                              (hermes-transport--non-empty-string
                               (hermes-messaging--field field 'key)))
                            (hermes-messaging--env-fields platform)))))
    (unless keys
      (user-error "This platform exposes no configurable env keys"))
    (completing-read (format "%s env key: " action) keys nil t)))

(defun hermes-messaging--mutate (path body &optional secrets success-message)
  "PUT PATH with BODY and refresh the current owned catalog.
SECRETS are transport-only redaction material.  SUCCESS-MESSAGE is displayed
only while the originating profile buffer still owns the operation."
  (hermes-messaging--require-mutation-idle)
  (let ((target (current-buffer))
        (profile hermes-messaging-profile)
        (generation (hermes-browser--next-request-generation))
        (token (list 'messaging-mutation)))
    (setq hermes-messaging--mutation-in-flight token
          hermes-messaging--platforms nil)
    (condition-case err
        (hermes-messaging--run-owned
         target generation profile
         (lambda (client)
           (hermes--promise-finally
            (condition-case request-error
                (hermes--promise-then
                 (hermes-messaging--api
                  client "PUT" path profile body :secrets secrets)
                 (lambda (_result)
                   (when (hermes-messaging--operation-current-p
                          target generation profile)
                     (hermes-messaging--api client "GET" "/platforms" profile))))
              ((error quit)
               (hermes--promise-rejected
                (error-message-string request-error))))
            (lambda () (hermes-messaging--clear-mutation target token))))
         (lambda (result)
           (hermes-messaging--render result target)
           (when success-message (message "Hermes: %s" success-message)))
         secrets)
      ((error quit)
       (hermes-messaging--clear-mutation target token)
       (signal (car err) (cdr err))))))

(defun hermes-messaging-toggle ()
  "Toggle the platform at point through the profile-scoped dashboard API."
  (interactive)
  (hermes-messaging--require-mutation-idle)
  (let* ((platform (hermes-messaging--platform-at-point))
         (id (hermes-messaging--id-at-point))
         (enable (not (eq (hermes-transport--get platform 'enabled) t))))
    (hermes-messaging--mutate
     (hermes-messaging--platform-path id)
     `((enabled . ,(if enable t :false))) nil
     (format "%s %s" (if enable "enabled" "disabled")
             (hermes-messaging--field platform 'name)))))

(defun hermes-messaging-set-env ()
  "Set one allowed env key for the platform at point."
  (interactive)
  (hermes-messaging--require-mutation-idle)
  (let* ((platform (hermes-messaging--platform-at-point))
         (id (hermes-messaging--id-at-point))
         (key (hermes-messaging--read-env-key platform "Set"))
         (field (hermes-messaging--env-field platform key))
         (secret-p (eq (hermes-transport--get field 'is_password) t))
         (value (if secret-p
                    (read-passwd (format "%s: " key))
                  (read-string (format "%s: " key)))))
    (when (string-empty-p (string-trim value))
      (user-error "Value is empty; use clear instead"))
    (hermes-messaging--mutate
     (hermes-messaging--platform-path id)
     `((env . ((,key . ,value))))
     (and secret-p (list value))
     (format "saved %s for %s" key
             (hermes-messaging--field platform 'name)))))

(defun hermes-messaging-clear-env ()
  "Clear one allowed env key for the platform at point."
  (interactive)
  (hermes-messaging--require-mutation-idle)
  (let* ((platform (hermes-messaging--platform-at-point))
         (id (hermes-messaging--id-at-point))
         (key (hermes-messaging--read-env-key platform "Clear")))
    (hermes-messaging--mutate
     (hermes-messaging--platform-path id)
     `((clear_env . (,key))) nil
     (format "cleared %s for %s" key
             (hermes-messaging--field platform 'name)))))

(defun hermes-messaging--safe-test-message (result platform)
  "Return a fail-closed display message for test RESULT and PLATFORM.
Only exact messages generated from the documented platform-test states are
accepted.  Arbitrary runtime error text is never displayed."
  (let* ((name (hermes-messaging--field platform 'name))
         (missing
          (delq nil
                (mapcar
                 (lambda (field)
                   (and (eq (hermes-transport--get field 'required) t)
                        (not (eq (hermes-transport--get field 'is_set) t))
                        (hermes-messaging--field field 'key)))
                 (hermes-messaging--env-fields platform))))
         (known
          (list (format "%s is disabled. Enable it, then restart the gateway."
                        name)
                (and missing
                     (format "Missing required setup: %s"
                             (string-join missing ", ")))
                "Platform setup is incomplete."
                "Gateway is not running. Restart the gateway to connect this platform."
                (format "%s is connected." name)
                "Setup looks complete, but the gateway has not reported a connection yet. Restart the gateway."))
         (raw (hermes-transport--field result 'message)))
    (cond
     ((and raw (member raw known)) raw)
     ((eq (hermes-transport--get result 'ok) t) "Platform test succeeded.")
     (t "Platform test failed."))))

(defun hermes-messaging-test ()
  "Test the platform at point and report the backend result."
  (interactive)
  (hermes-messaging--require-mutation-idle)
  (let ((target (current-buffer))
        (profile hermes-messaging-profile)
        (platform (hermes-messaging--platform-at-point))
        (id (hermes-messaging--id-at-point))
        (generation (hermes-browser--next-request-generation)))
    (hermes-messaging--run-owned
     target generation profile
     (lambda (client)
       (hermes-messaging--api
        client "POST" (hermes-messaging--platform-path id "/test") profile))
     (lambda (result)
       (message "Hermes: %s"
                (hermes-messaging--safe-test-message result platform))))))

(defun hermes-messaging--detail-text (platform profile)
  "Return safe detail text for PLATFORM in PROFILE."
  (let ((env-lines
         (mapcar
          (lambda (field)
            (format "  %-28s %-5s%s  %s"
                    (hermes-messaging--field field 'key)
                    (if (eq (hermes-transport--get field 'is_set) t)
                        "set" "unset")
                    (if (eq (hermes-transport--get field 'required) t)
                        " required" "")
                    (hermes-messaging--field field 'description)))
          (hermes-messaging--env-fields platform))))
    (string-join
     (append
      (list (format "%s\n" (hermes-messaging--field platform 'name))
            (format "Profile:    %s" (hermes-messaging--profile-label profile))
            (format "Enabled:    %s"
                    (hermes-messaging--boolean-label platform 'enabled "yes" "no"))
            (format "Configured: %s"
                    (hermes-messaging--boolean-label platform 'configured "yes" "no"))
            (format "State:      %s" (hermes-messaging--field platform 'state))
            "\nEnvironment:")
      (or env-lines '("  None")))
     "\n")))

(defun hermes-messaging-view ()
  "View safe catalog details for the platform at point."
  (interactive)
  (let* ((platform (hermes-messaging--platform-at-point))
         (id (hermes-messaging--id-at-point))
         (profile hermes-messaging-profile)
         (buffer (get-buffer-create
                  (format "*Hermes Messaging Detail: %s/%s*"
                          (hermes-messaging--profile-label profile) id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (hermes-messaging--detail-text platform profile))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun hermes-messaging-select-profile ()
  "Open the messaging catalog for another dashboard profile."
  (interactive)
  (hermes-list-messaging-platforms (hermes-messaging--read-profile)))

(defvar hermes-messaging-mode-map)

(keymap-popup-define hermes-messaging-mode-map
  "Keymap for `hermes-messaging-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes Messaging"
  :group "Platform"
  "RET" ("View details" hermes-messaging-view)
  "e" ("Enable/disable" hermes-messaging-toggle)
  "s" ("Set env key" hermes-messaging-set-env)
  "c" ("Clear env key" hermes-messaging-clear-env)
  "t" ("Test" hermes-messaging-test)
  :group "View"
  "p" ("Select profile" hermes-messaging-select-profile)
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-messaging-mode-map-popup))

(define-derived-mode hermes-messaging-mode tabulated-list-mode "Hermes Messaging"
  "Major mode for profile-scoped Hermes messaging platforms."
  :interactive nil
  (unless (hash-table-p hermes-messaging--platforms)
    (setq hermes-messaging--platforms (make-hash-table :test #'equal)))
  (setq tabulated-list-format
        [("Platform" 18 t) ("Enabled" 8 t) ("Configured" 11 t)
         ("State" 18 t) ("Description" 48 nil)])
  (setq-local revert-buffer-function #'hermes-messaging--revert)
  (tabulated-list-init-header))

;;;###autoload
(defun hermes-list-messaging-platforms (&optional profile)
  "Browse messaging platforms scoped to PROFILE.
Interactively, blank means the dashboard's current profile."
  (interactive (list (hermes-messaging--read-profile)))
  (hermes-messaging--fetch profile t))

(provide 'hermes-messaging)
;;; hermes-messaging.el ends here
