;;; hermes-plugins.el --- Native agent plugin management -*- lexical-binding: t; -*-

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

;; Manage agent plugins, not Electron extensions or dashboard assets.
;; The server process profile owns all routes: no chat profile override exists.
;; Inventory status describes persisted enablement, not loaded runtime code.
;; Plugin-specific configuration schemas and restart status are not exposed by
;; this API.  Configuration opens the existing schema/environment editor;
;; arbitrary plugin files and backend-supplied shell hints are never executed.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'keymap-popup)
(require 'hermes-browser)
(require 'hermes-config)

(defvar-local hermes-plugins--snapshot nil
  "Latest authoritative agent hub snapshot.")
(defvar-local hermes-plugins--busy nil
  "Non-nil while a mutation and its readback are pending.")
(defvar-local hermes-plugins--status "Not loaded"
  "Request status, without backend error bodies or credentials.")

(defun hermes-plugins--entries (result)
  "Return native inventory entries for hub RESULT, preserving unknown states."
  (mapcar
   (lambda (row)
     (let ((get (lambda (key) (or (hermes-transport--get row key) ""))))
       (list (funcall get 'name)
             (vector (propertize (funcall get 'name) 'face 'hermes-browser-name)
                     (propertize (funcall get 'runtime_status) 'face
                                 (pcase (funcall get 'runtime_status)
                                   ("enabled" 'success)
                                   ((or "inactive" "disabled") 'shadow)
                                   (_ 'font-lock-keyword-face)))
                     (funcall get 'source) (funcall get 'version)
                     (funcall get 'description)))))
   (append (hermes-transport--get result 'plugins) nil)))

(defun hermes-plugins--header ()
  "Return the owning instance, process scope, and configuration status."
  (format "%s Server profile | Context: %s | %s"
          (or (hermes-browser--instance-header-line) "")
          (or (hermes-transport--get
               (hermes-transport--get hermes-plugins--snapshot 'providers)
               'context_engine) "unknown")
          hermes-plugins--status))

(defun hermes-plugins--guard ()
  "Return a predicate capturing this buffer's request and instance ownership."
  (let ((buffer (current-buffer))
        (generation hermes-browser--request-generation)
        (instance hermes-instance)
        (value (copy-tree hermes-instance)))
    (lambda ()
      (and (hermes-browser--request-current-mode-p
            buffer generation 'hermes-plugins-mode)
           (eq instance (buffer-local-value 'hermes-instance buffer))
           (equal value instance)))))

(defun hermes-plugins--idle ()
  "Require an idle agent plugin browser."
  (unless (derived-mode-p 'hermes-plugins-mode)
    (user-error "Open the agent plugin browser first"))
  (when hermes-plugins--busy
    (user-error "Plugin update and readback are still pending")))

(defun hermes-plugins--api (client method path body)
  "Request METHOD PATH with BODY through the owning CLIENT."
  (hermes-dashboard-transport-api-request-async
   method path :body body :client client))

(defun hermes-plugins--render (result status)
  "Render authoritative RESULT and safe STATUS in the current browser."
  (setq hermes-plugins--snapshot result
        hermes-plugins--busy nil
        hermes-plugins--status status
        tabulated-list-entries (hermes-plugins--entries result))
  (tabulated-list-print t))

(defun hermes-plugins--scope (client)
  "Return CLIENT's connection lifetime and endpoint, without credentials."
  (when (hermes-dashboard-transport-client-p client)
    (list (hermes-dashboard-transport-client-generation client)
          (hermes-dashboard-transport--api-client-base-url client))))

(defun hermes-plugins--inventory (client hub current-p)
  "Join HUB with CLIENT's published process home while CURRENT-P owns it.
Gated servers may withhold the home.  Inventory remains readable, but
filesystem actions must then fail closed rather than infer a root."
  (when (funcall current-p)
    (hermes--promise-then
     (hermes-plugins--api client "GET" "/api/status" nil)
     (lambda (status)
       (list :plugins (hermes-transport--get hub 'plugins)
             :providers (hermes-transport--get hub 'providers)
             :hermes_home (hermes-transport--get status 'hermes_home)))
     (lambda (_reason)
       (list :plugins (hermes-transport--get hub 'plugins)
             :providers (hermes-transport--get hub 'providers)
             :hermes_home nil)))))

(defun hermes-plugins--readback (client result mutation current-p)
  "Read back RESULT of MUTATION on CLIENT while CURRENT-P grants ownership."
  (when (funcall current-p)
    (when (and mutation (not (eq t (hermes-transport--get result 'ok))))
      (error "Plugin operation rejected"))
    (hermes--promise-then
     (if mutation
         (hermes-plugins--api client "GET" "/api/dashboard/plugins/hub" nil)
       (hermes--promise-resolved result))
     (lambda (hub) (hermes-plugins--inventory client hub current-p)))))

(defun hermes-plugins--setup-note (result)
  "Return safe installation setup hints from RESULT, never raw warning text."
  (let ((missing (seq-filter
                  (lambda (name)
                    (and (stringp name)
                         (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)))
                  (hermes-transport--get result 'missing_env)))
        (warnings (hermes-transport--get result 'warnings)))
    (concat (when missing
              (concat "; missing environment: " (string-join missing ", ")))
            (when (and warnings (not (seq-empty-p warnings)))
              "; backend reported installation warnings"))))

(defun hermes-plugins--run (client method path body mutation buffer current-p)
  "Run METHOD PATH BODY on CLIENT for MUTATION in BUFFER owned by CURRENT-P."
  (let* ((scope (hermes-plugins--scope client))
         setup-note
         (owned-p (lambda ()
                    (and (funcall current-p)
                         (equal scope (hermes-plugins--scope client))))))
    (hermes--promise-then
     (hermes--promise-then
      (hermes-plugins--api client method path body)
      (lambda (result)
        (when (and mutation (funcall owned-p))
          (setq setup-note (hermes-plugins--setup-note result)))
        (hermes-plugins--readback client result mutation owned-p)))
     (lambda (result)
       (when (funcall owned-p)
         (with-current-buffer buffer
           (hermes-plugins--render
            result (if mutation
                       (concat "Read back; restart/new session may be required (unverified)"
                               setup-note)
                     "Configured state; runtime activation unverified")))))
     (lambda (_reason)
       ;; Do not print arbitrary error bodies: clone failures can echo secrets.
       (when (funcall owned-p)
         (with-current-buffer buffer
           (hermes-plugins--render
            nil "Request failed; writes may have applied; refresh (details withheld)")))))))

(defun hermes-plugins--request (method path &optional body mutation)
  "Request METHOD PATH with BODY and read back a successful MUTATION."
  (hermes-plugins--idle)
  (hermes-browser--next-request-generation)
  (let* ((buffer (current-buffer))
         (generation hermes-browser--request-generation)
         (current-p (hermes-plugins--guard)))
    (setq hermes-plugins--busy mutation
          hermes-plugins--snapshot nil
          hermes-plugins--status (if mutation "Updating; awaiting readback" "Loading"))
    (hermes-browser--run-on-client
     (lambda (client)
       (let ((generation hermes-browser--request-generation))
         (hermes--promise-finally
          (hermes-plugins--run client method path body mutation buffer current-p)
          (lambda ()
            ;; Release only this request's lock, even after connection retirement.
            (when (hermes-browser--request-current-mode-p
                   buffer generation 'hermes-plugins-mode)
              (with-current-buffer buffer (setq hermes-plugins--busy nil)))))))
     #'ignore
     (lambda (_reason)
       (when (hermes-browser--request-current-mode-p
              buffer generation 'hermes-plugins-mode)
         (with-current-buffer buffer
           (setq hermes-plugins--busy nil)))
       (when (funcall current-p)
         (with-current-buffer buffer
           (hermes-plugins--render nil "Connection failed (details withheld)")))))))

(defun hermes-plugins-refresh (&rest _)
  "Refresh the server's agent plugin inventory asynchronously."
  (interactive)
  (hermes-plugins--request "GET" "/api/dashboard/plugins/hub"))

(defun hermes-plugins--selected ()
  "Return the authoritative plugin at point or signal a user error."
  (hermes-plugins--idle)
  (let ((rows (seq-filter
               (lambda (row)
                 (equal (tabulated-list-get-id) (hermes-transport--get row 'name)))
               (hermes-transport--get hermes-plugins--snapshot 'plugins))))
    (unless rows (user-error "Refresh and select an agent plugin first"))
    ;; The hub exposes bare names, not canonical nested keys.  Do not guess
    ;; which plugin a duplicate name would mutate on the backend.
    (when (cdr rows) (user-error "The backend returned an ambiguous plugin name"))
    (car rows)))

(defun hermes-plugins--directory-target (row)
  "Return the lexical top-level directory target for ROW, or refuse it.
The released API mutates directory keys, not manifest names.  Use only
an exact path under the process home published by the same server.
The backend resolves filesystem links; the hub cannot prove their destination."
  (let* ((home (hermes-transport--get hermes-plugins--snapshot 'hermes_home))
         (path (hermes-transport--get row 'path))
         (root (and (stringp home) (concat home "/plugins/")))
         (target (and root (stringp path) (string-prefix-p root path)
                      (substring path (length root)))))
    ;; Treat server paths as protocol strings, never as local/TRAMP filenames.
    (unless (and (stringp home) (string-prefix-p "/" home)
                 (not (string-suffix-p "/" home))
                 (not (string-match-p "\\\\\\|//\\|/\\.\\.?\\(?:/\\|\\'\\)" home))
                 (member (hermes-transport--get row 'source) '("user" "git"))
                 target (string-match-p "\\`[A-Za-z0-9_-][A-Za-z0-9._-]*\\'" target)
                 (not (string-match-p "\\.\\." target)))
      (user-error "Server directory identity unavailable or nested; cannot mutate safely"))
    (when (seq-some
           (lambda (other)
             (and (not (eq other row))
                  (or (equal path (hermes-transport--get other 'path))
                      (equal target (hermes-transport--get other 'name)))))
           (hermes-transport--get hermes-plugins--snapshot 'plugins))
      (user-error "The backend returned colliding plugin identities"))
    target))

(defun hermes-plugins--name-target (name)
  "Return NAME unchanged if the backend accepts it without rewriting it."
  ;; dashboard_ui._validate_plugin_name strips boundary slashes and rejects
  ;; empty names, backslashes and double dots.  Never dispatch a rewritten alias.
  (unless (and (stringp name) (not (string-empty-p name))
               (not (string-prefix-p "/" name))
               (not (string-suffix-p "/" name))
               (not (string-match-p "\\\\\\|\\.\\." name)))
    (user-error "Plugin name is invalid or would be rewritten by the backend"))
  name)

(defun hermes-plugins--action (action &optional permission)
  "Confirm ACTION on the selected plugin, requiring PERMISSION if given."
  (let* ((buffer (current-buffer))
         (row (hermes-plugins--selected))
         (name (hermes-transport--get row 'name))
         (target (if permission (hermes-plugins--directory-target row)
                   (hermes-plugins--name-target name)))
         (current-p (hermes-plugins--guard)))
    (when (and permission (not (eq t (hermes-transport--get row permission))))
      (user-error "The server does not allow this action for this plugin"))
    (when (and (yes-or-no-p
                (format "%s agent plugin %S%s on instance %S? "
                        (capitalize action) name
                        (if permission
                            (format " at directory %S (path %S; backend resolves filesystem links; destination unverified)"
                                    target (hermes-transport--get row 'path))
                          "")
                        (hermes-instance-name hermes-instance)))
               (funcall current-p))
      (with-current-buffer buffer
        (hermes-plugins--request
         (if (equal action "remove") "DELETE" "POST")
         (concat "/api/dashboard/agent-plugins/" (url-hexify-string target)
                 (unless (equal action "remove") (concat "/" action)))
         nil t)))))

(defun hermes-plugins-enable ()
  "Confirm and enable the agent plugin at point in server configuration."
  (interactive)
  (hermes-plugins--action "enable"))

(defun hermes-plugins-disable ()
  "Confirm and disable the agent plugin at point in server configuration."
  (interactive)
  (hermes-plugins--action "disable"))

(defun hermes-plugins-update ()
  "Confirm a backend-permitted Git update of the selected plugin."
  (interactive)
  (hermes-plugins--action "update" 'can_update_git))

(defun hermes-plugins-remove ()
  "Confirm and remove the selected plugin when the backend permits removal."
  (interactive)
  (hermes-plugins--action "remove" 'can_remove))

(defun hermes-plugins-install ()
  "Confirm installation of trusted plugin code on the owning server.
Install without automatically enabling, forcing replacement, or bypassing
scan refusals.  Existing enablement may persist; inspect the readback."
  (interactive)
  (hermes-plugins--idle)
  (unless hermes-plugins--snapshot
    (user-error "Refresh the plugin inventory before installing"))
  (let* ((buffer (current-buffer))
         (current-p (hermes-plugins--guard))
         (identifier (read-string "Plugin identifier or Git URL (no credentials): ")))
    (when (and (not (string-empty-p identifier))
               (funcall current-p)
               (yes-or-no-p "Install trusted code without automatically enabling (existing enablement may persist)? ")
               (funcall current-p))
      (with-current-buffer buffer
        (hermes-plugins--request
         "POST" "/api/dashboard/agent-plugins/install"
         `((identifier . ,identifier) (force . :false) (enable . :false)) t)))))

(defun hermes-plugins-select-context-engine ()
  "Choose and confirm a context engine from the backend catalog."
  (interactive)
  (hermes-plugins--idle)
  (let* ((buffer (current-buffer))
         (current-p (hermes-plugins--guard))
         (providers (hermes-transport--get hermes-plugins--snapshot 'providers))
         (names (delete-dups
                 (cons "compressor"
                       (mapcar (lambda (row) (hermes-transport--get row 'name))
                               (append (hermes-transport--get providers 'context_options) nil))))))
    (unless hermes-plugins--snapshot (user-error "Refresh the plugin inventory first"))
    (let ((name (completing-read "Context engine: " names nil t)))
      (when (and (funcall current-p)
                 (yes-or-no-p (format "Save context engine %s on this server? " name))
                 (funcall current-p))
        (with-current-buffer buffer
          (hermes-plugins--request
           "PUT" "/api/dashboard/plugin-providers" `((context_engine . ,name)) t))))))

(defun hermes-plugins-configure ()
  "Open server schema and environment configuration in the owning instance.
The agent hub exposes no arbitrary per-plugin configuration schema.  Only
backend-declared settings and environment entries are editable here."
  (interactive)
  (hermes-plugins--idle)
  (hermes-config))

(defvar hermes-plugins-mode-map)

(defun hermes-plugins-popup ()
  "Show agent plugin commands."
  (interactive)
  (keymap-popup hermes-plugins-mode-map))

(keymap-popup-define hermes-plugins-mode-map
  :parent tabulated-list-mode-map
  :group "Inventory"
  "g" ("Refresh" hermes-plugins-refresh)
  "i" ("Install" hermes-plugins-install)
  "?" ("Help" hermes-plugins-popup)
  "q" ("Quit" quit-window)
  :group "Selected plugin"
  "e" ("Enable" hermes-plugins-enable)
  "d" ("Disable" hermes-plugins-disable)
  "u" ("Update" hermes-plugins-update)
  "D" ("Remove" hermes-plugins-remove)
  :group "Configuration"
  "c" ("Schema and environment" hermes-plugins-configure)
  "x" ("Context engine" hermes-plugins-select-context-engine))

(define-derived-mode hermes-plugins-mode tabulated-list-mode "Hermes Plugins"
  "Browse server-profile agent plugins, not frontend extensions."
  (hermes-browser--next-request-generation)
  (setq-local tabulated-list-format
              [("Plugin" 24 t) ("Configured state" 18 t) ("Source" 12 t)
               ("Version" 10 t) ("Description" 0 t)])
  (setq-local hermes-browser--snapshot-variables '(hermes-plugins--snapshot hermes-plugins--busy))
  (setq-local revert-buffer-function #'hermes-plugins-refresh)
  (setq-local header-line-format '(:eval (hermes-plugins--header)))
  (tabulated-list-init-header))

;;;###autoload
(defun hermes-list-plugins ()
  "Open the native agent plugin browser for the current Hermes instance."
  (interactive)
  (let ((instance (hermes-instance-resolve))
        (buffer (generate-new-buffer "*Hermes Agent Plugins*")))
    (with-current-buffer buffer
      (hermes-plugins-mode)
      (hermes-browser--own-instance instance)
      (setq-local header-line-format '(:eval (hermes-plugins--header))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-plugins-refresh))))

(provide 'hermes-plugins)
;;; hermes-plugins.el ends here
