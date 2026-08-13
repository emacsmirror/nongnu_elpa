;;; hermes-config.el --- Schema config and environment browser  -*- lexical-binding: t; -*-

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

;; Schema-driven dashboard configuration and redacted environment management.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-browser)

(defvar-local hermes-config--schema nil
  "Latest dashboard configuration schema.")

(defvar-local hermes-config--config nil
  "Latest normalized dashboard configuration.")

(defvar-local hermes-config--env nil
  "Latest redacted dashboard environment catalog.")

(defun hermes-config--api (client method path &optional body)
  "Return dashboard REST METHOD PATH promise through CLIENT with BODY."
  (hermes-dashboard-transport-api-request-async
   method path :body body :client client))

(defun hermes-config--run-owned (buffer generation make-promise on-success)
  "Run MAKE-PROMISE while BUFFER owns GENERATION.
ON-SUCCESS receives the result only while BUFFER remains a current config view."
  (let ((hermes-browser--request-error-owner
         (list buffer generation 'hermes-config-mode)))
    (hermes-browser--run-on-client
     make-promise
     (lambda (result)
       (when (hermes-browser--request-current-mode-p
              buffer generation 'hermes-config-mode)
         (funcall on-success result))))))

(defvar-local hermes-config--mutation-in-flight nil
  "Identity token for the current config mutation, or nil.")

(defvar-local hermes-config--mutation-cleanup nil
  "Idempotent cleanup thunk for the current config mutation, or nil.")

(defun hermes-config--require-mutation-idle ()
  "Signal `user-error' while this config view has an unsettled mutation."
  (when hermes-config--mutation-in-flight
    (user-error "A configuration update is still in progress")))

(defun hermes-config--mutation-current-p (buffer token)
  "Return non-nil when BUFFER still owns mutation TOKEN."
  (and (hermes-browser--buffer-mode-p buffer 'hermes-config-mode)
       (eq token
           (buffer-local-value 'hermes-config--mutation-in-flight buffer))))

(defun hermes-config--teardown-mutation ()
  "Release the current config mutation during buffer teardown."
  (when hermes-config--mutation-cleanup
    (funcall hermes-config--mutation-cleanup)))

(defun hermes-config--run-mutation (buffer make-promise)
  "Run BUFFER's MAKE-PROMISE mutation, then fetch authoritative state.
Only one mutation may own BUFFER.  The lock covers the write and its refresh."
  (with-current-buffer buffer (hermes-config--require-mutation-idle))
  (let ((token (list 'config-mutation)))
    (with-current-buffer buffer
      (hermes-browser--next-request-generation)
      (setq hermes-config--mutation-in-flight token))
    (condition-case err
        (hermes-browser--with-client
         (lambda (client done)
           (let (cleanup
                 (released nil))
             (setq cleanup
                   (lambda ()
                     (unless released
                       (setq released t)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when (eq hermes-config--mutation-in-flight token)
                             (setq hermes-config--mutation-in-flight nil))
                           (when (eq hermes-config--mutation-cleanup cleanup)
                             (setq hermes-config--mutation-cleanup nil))))
                       (funcall done))))
             (with-current-buffer buffer
               (setq hermes-config--mutation-cleanup cleanup))
             (let ((write
                    (condition-case write-err
                        (funcall make-promise client)
                      ((error quit)
                       (hermes--promise-rejected
                        (error-message-string write-err))))))
               (hermes--promise-finally
                (hermes--promise-then
                 (hermes--promise-then
                  write
                  (lambda (_result)
                    (when (hermes-config--mutation-current-p buffer token)
                      (hermes-config--fetch client))))
                 (lambda (result)
                   (when (hermes-config--mutation-current-p buffer token)
                     (hermes-config--render
                      buffer (nth 0 result) (nth 1 result) (nth 2 result))))
                 (lambda (reason)
                   (when (hermes-config--mutation-current-p buffer token)
                     (message "Hermes: %s" reason))))
                cleanup)))))
      ((error quit)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq hermes-config--mutation-in-flight nil
                 hermes-config--mutation-cleanup nil)))
       (signal (car err) (cdr err))))))

(defun hermes-config--object-entries (object)
  "Return OBJECT as an alist of entries."
  (cond
   ((hash-table-p object)
    (let (entries)
      (maphash (lambda (key value) (push (cons key value) entries)) object)
      (nreverse entries)))
   ((listp object) object)
   (t nil)))

(defun hermes-config--path-value (object path)
  "Return nested OBJECT value at dotted PATH."
  (let ((value object))
    (dolist (part (split-string path "\\." t) value)
      (setq value (hermes-transport--get value part)))))

(defun hermes-config--entry-key (object key)
  "Return OBJECT's existing alist key matching string KEY, or a symbol key."
  (let ((symbol (intern key)))
    (cond
     ((assq symbol object) symbol)
     ((assoc key object) key)
     (t symbol))))

(defun hermes-config--set-path (object parts value)
  "Return OBJECT with nested PARTS set to VALUE."
  (let* ((part (car parts))
         (key (hermes-config--entry-key object part))
         (old (hermes-transport--get object part))
         (next (if (cdr parts)
                   (hermes-config--set-path
                    (and (listp old) old) (cdr parts) value)
                 value)))
    (cons (cons key next)
          (cl-remove key object :key #'car :test #'equal))))

(defun hermes-config--display-value (value)
  "Return safe display text for configuration VALUE."
  (hermes-dashboard-transport--redact-secret
   (cond
    ((eq value t) "true")
    ((memq value '(:false :json-false)) "false")
    ((null value) "")
    ((stringp value) value)
    (t (prin1-to-string value)))))

(defun hermes-config--insert-line (text property value)
  "Insert TEXT line carrying PROPERTY VALUE."
  (let ((start (point)))
    (insert text "\n")
    (add-text-properties start (point) (list property value))))

(defun hermes-config--render (buffer schema config env)
  "Render SCHEMA, CONFIG, and redacted ENV in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'hermes-config-mode)
        (setq hermes-config--schema schema
              hermes-config--config config
              hermes-config--env env)
        (let ((inhibit-read-only t)
              (fields (hermes-transport--get schema 'fields)))
          (erase-buffer)
          (insert (propertize "Hermes Configuration" 'face 'bold) "\n\n")
          (dolist (entry (hermes-config--object-entries fields))
            (let ((path (format "%s" (car entry))))
              (hermes-config--insert-line
               (format "%-36s %s" path
                       (hermes-config--display-value
                        (hermes-config--path-value config path)))
               'hermes-config-key path)))
          (insert "\n" (propertize "Environment / API keys" 'face 'bold) "\n\n")
          (dolist (entry (hermes-config--object-entries env))
            (let* ((key (format "%s" (car entry)))
                   (meta (cdr entry))
                   (value (or (hermes-transport--scalar-string
                               (hermes-transport--get meta 'redacted_value))
                              "unset")))
              (hermes-config--insert-line
               (format "%-36s %s" key value) 'hermes-env-key key)))
          (goto-char (point-min)))))))

(defun hermes-config--fetch (client)
  "Return schema, config, and environment through CLIENT."
  (hermes--promise-then
   (hermes-config--api client "GET" "/api/config/schema")
   (lambda (schema)
     (hermes--promise-then
      (hermes-config--api client "GET" "/api/config")
      (lambda (config)
        (hermes--promise-map
         (hermes-config--api client "GET" "/api/env")
         (lambda (env) (list schema config env))))))))

(defun hermes-config-refresh (&rest _)
  "Refresh the current Hermes config buffer."
  (hermes-config--require-mutation-idle)
  (let ((buffer (current-buffer))
        (generation (hermes-browser--next-request-generation)))
    (hermes-config--run-owned
     buffer generation
     #'hermes-config--fetch
     (lambda (result)
       (hermes-config--render buffer (nth 0 result) (nth 1 result)
                              (nth 2 result))))))

(defun hermes-config--coerce (text schema)
  "Return TEXT coerced according to field SCHEMA."
  (let ((type (hermes-transport--scalar-string
               (hermes-transport--get schema 'type)))
        (trimmed (string-trim text)))
    (pcase type
      ("number"
       (unless (string-match-p
                "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'"
                trimmed)
         (user-error "Enter a number"))
       (string-to-number trimmed))
      ((or "bool" "boolean")
       (pcase (downcase trimmed)
         ((or "t" "true" "yes" "1") t)
         ((or "nil" "false" "no" "0") :false)
         (_ (user-error "Enter true or false"))))
      ("list"
       (seq-filter (lambda (item) (not (string-empty-p item)))
                   (mapcar #'string-trim (split-string text ","))))
      (_ text))))

(defun hermes-config--field-schema (path)
  "Return schema entry for config PATH."
  (hermes-transport--get
   (hermes-transport--get hermes-config--schema 'fields) path))

(defun hermes-config--read-value (path schema current)
  "Read config PATH using SCHEMA, with CURRENT as initial value."
  (let ((type (hermes-transport--scalar-string
               (hermes-transport--get schema 'type))))
    (pcase type
      ((or "bool" "boolean")
       (if (y-or-n-p (format "Enable %s? " path)) t :false))
      ("select"
       (completing-read (format "%s: " path)
                        (hermes-transport--get schema 'options)
                        nil t nil nil (hermes-config--display-value current)))
      (_
       (hermes-config--coerce
        (read-string (format "%s: " path)
                     (hermes-config--display-value current))
        schema)))))

(defun hermes-config-edit ()
  "Edit the schema field at point and save it through dashboard REST."
  (interactive)
  (hermes-config--require-mutation-idle)
  (let* ((path (get-text-property (point) 'hermes-config-key))
         (current (and path (hermes-config--path-value hermes-config--config path)))
         (schema (and path (hermes-config--field-schema path))))
    (unless path (user-error "No configuration field on this line"))
    (unless schema (user-error "Dashboard schema has no field %s" path))
    (let* ((config (hermes-config--set-path
                    hermes-config--config (split-string path "\\." t)
                    (hermes-config--read-value path schema current)))
           (buffer (current-buffer)))
      (hermes-config--run-mutation
       buffer
       (lambda (client)
         (hermes-config--api client "PUT" "/api/config"
                             `((config . ,config))))))))

(defun hermes-config--env-key-at-point ()
  "Return environment key at point, or prompt for a new one."
  (or (get-text-property (point) 'hermes-env-key)
      (let ((key (string-trim (read-string "Environment key: "))))
        (when (string-empty-p key) (user-error "Environment key is required"))
        key)))

(defun hermes-config-set-env ()
  "Set the environment key at point without echoing secret input."
  (interactive)
  (hermes-config--require-mutation-idle)
  (let* ((key (hermes-config--env-key-at-point))
         (value (read-passwd (format "%s: " key)))
         (buffer (current-buffer)))
    (hermes-config--run-mutation
     buffer
     (lambda (client)
       (hermes-dashboard-transport-api-request-async
        "PUT" "/api/env" :body `((key . ,key) (value . ,value))
        :secrets (list value) :client client)))))

(defun hermes-config-delete-env ()
  "Delete the environment key at point after confirmation."
  (interactive)
  (hermes-config--require-mutation-idle)
  (let ((key (get-text-property (point) 'hermes-env-key))
        (buffer (current-buffer)))
    (unless key (user-error "No environment key on this line"))
    (when (yes-or-no-p (format "Delete environment key %s? " key))
      (hermes-config--run-mutation
       buffer
       (lambda (client)
         (hermes-config--api client "DELETE" "/api/env" `((key . ,key))))))))

(defun hermes-config-reveal-env ()
  "Reveal the environment key at point by copying it without displaying it."
  (interactive)
  (let ((key (get-text-property (point) 'hermes-env-key))
        (buffer (current-buffer)))
    (unless key (user-error "No environment key on this line"))
    (let ((generation (hermes-browser--next-request-generation)))
      (hermes-config--run-owned
       buffer generation
       (lambda (client)
         (hermes-config--api client "POST" "/api/env/reveal" `((key . ,key))))
       (lambda (result)
         (let ((value (hermes-transport--scalar-string
                       (hermes-transport--get result 'value))))
           (unless value (user-error "Dashboard returned no value"))
           (kill-new value)
           (message "Hermes: copied %s value; it was not displayed" key)))))))

(defvar-keymap hermes-config-mode-map
  :doc "Keymap for the Hermes config and environment browser."
  :parent special-mode-map
  "RET" #'hermes-config-edit
  "e" #'hermes-config-edit
  "k" #'hermes-config-set-env
  "D" #'hermes-config-delete-env
  "R" #'hermes-config-reveal-env
  "g" #'hermes-config-refresh)

(define-derived-mode hermes-config-mode special-mode "Hermes Config"
  "Major mode for dashboard configuration and environment management."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-config-refresh)
  (add-hook 'change-major-mode-hook #'hermes-config--teardown-mutation nil t)
  (add-hook 'kill-buffer-hook #'hermes-config--teardown-mutation nil t))

;;;###autoload
(defun hermes-config ()
  "Open the schema-driven Hermes configuration browser."
  (interactive)
  (let ((buffer (get-buffer-create "*Hermes Config*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hermes-config-mode)
        (hermes-config-mode)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless hermes-config--mutation-in-flight
        (hermes-config-refresh)))))

(provide 'hermes-config)
;;; hermes-config.el ends here
