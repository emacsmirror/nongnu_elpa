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

(require 'hermes-buffer)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'keymap-popup)
(require 'hermes-browser)
(require 'hermes-config)

(defvar-local hermes-plugins--catalog-p nil
  "Non-nil when displaying the curated catalog instead of installed plugins.")
(defvar-local hermes-plugins--consent nil
  "Inert review text for the last valid capability-consent receipt.")

(defun hermes-plugins--text (value)
  "Return inert, single-line display text for VALUE, withholding credentials."
  (if (not (stringp value)) "unknown"
    (let ((text (substring-no-properties value))
          (case-fold-search t))
      (if (string-match-p
           "[[:cntrl:]\u202a-\u202e\u2066-\u2069]\\|://[^/]*@\\|[?&]\\|\\b[A-Za-z0-9_.-]*\\(?:token\\|secret\\|password\\|api[_-]?key\\)[A-Za-z0-9_.-]*[=:]\\|\\b[A-Za-z0-9_-]\\{48,\\}\\b"
           text)
          "[withheld]"
        text))))

(defun hermes-plugins--sha-p (value)
  "Return non-nil if VALUE is a full immutable catalog pin."
  (and (stringp value) (let ((case-fold-search nil))
                        (string-match-p "\\`[0-9a-f]\\{40\\}\\'" value))))

(defun hermes-plugins--consent-text (result)
  "Return inert review text for a complete consent RESULT, or signal an error."
  (let* ((sha (hermes-transport--get result 'sha))
         (name (hermes-transport--get result 'name))
         (delta (hermes-transport--get result 'delta))
         (lines
          (delq nil
                (mapcar
                 (lambda (key)
                   (when-let* ((values (hermes-transport--get delta key)))
                     (unless (and (or (listp values) (vectorp values))
                                  (seq-every-p #'stringp values))
                       (error "Malformed capability delta"))
                     (unless (seq-empty-p values)
                       (format "%s: %s" key
                               (mapconcat #'hermes-plugins--text values ", ")))))
                 '(capabilities tools hooks python_dependencies desktop)))))
    (unless (and (hermes-transport--field-present-p result 'ok)
                 (memq (hermes-transport--get result 'ok) '(:false :json-false))
                 (hermes-plugins--sha-p sha) (stringp name)
                 (not (string-empty-p name)) lines)
      (error "Incomplete capability-consent receipt"))
    (concat "Awaiting capability consent\n\n"
            "The backend receipt reports that this update was not applied.\n"
            "No consent retry or activation will be sent by Emacs.\n\n"
            "Catalog name: " (hermes-plugins--text name) "\nCandidate SHA: " sha "\n"
            (string-join lines "\n") "\n\n"
            "Review externally on the same backend and launch-profile home.\n"
            "Use its native plugin update workflow and review the fresh candidate\n"
            "there: catalog metadata may have changed since this receipt.\n"
            "Emacs cannot atomically bind consent to this SHA and delta.\n"
            "Leave this review to decline; explicitly Refresh to reconcile.\n")))

(defvar-local hermes-plugins--snapshot nil
  "Latest authoritative agent hub snapshot.")
(defvar-local hermes-plugins--snapshot-stale-p nil
  "Non-nil if the retained inventory may be displayed but cannot admit writes.")
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
  (format "%s Launch-home %s | Context: %s | %s"
          (or (hermes-browser--instance-header-line) "")
          (if hermes-plugins--catalog-p "catalog" "inventory")
          (or (hermes-transport--get
               (hermes-transport--get hermes-plugins--snapshot 'providers)
               'context_engine) "unknown")
          (propertize hermes-plugins--status 'face
                      (if hermes-plugins--snapshot-stale-p 'warning 'shadow))))

(defun hermes-plugins--guard ()
  "Return a predicate capturing this buffer's request and instance ownership."
  (hermes-browser--owned-predicate nil 'hermes-plugins-mode))

(defun hermes-plugins--idle ()
  "Require an idle agent plugin browser."
  (unless (derived-mode-p 'hermes-plugins-mode)
    (user-error "Open the agent plugin browser first"))
  (when (hermes-buffer--retired-p) (user-error "Reopen the Plugins view"))
  (when hermes-plugins--busy
    (user-error "Plugin update and readback are still pending")))

(defun hermes-plugins--api (client method path body)
  "Request METHOD PATH with BODY through the owning CLIENT."
  (hermes-dashboard-transport-api-request-async
   method path :body body :client client
   :current-p hermes-dashboard-transport--api-dispatch-guard))

(defun hermes-plugins--render (result status &optional current-p)
  "Render authoritative RESULT and STATUS while CURRENT-P owns the browser.
Keep native change hooks outside printer rollback and candidate bindings:
a hook may establish successor text and state that must not be restored."
  (let* ((buffer (current-buffer))
         (current-p (or current-p (hermes-plugins--guard)))
         (tick (buffer-chars-modified-tick))
         (entries (if hermes-plugins--catalog-p
                      (hermes-plugins--catalog-entries result)
                    (hermes-plugins--entries result)))
         (retired (make-symbol "retired-render")))
    (catch retired
      (combine-change-calls (point-min) (point-max)
        (unless (and (eq (current-buffer) buffer) (funcall current-p)
                     (= tick (buffer-chars-modified-tick)))
          (throw retired nil))
        (let ((tabulated-list-entries entries)
              (inhibit-modification-hooks t))
          ;; Ordinary printer errors still restore the accepted table.
          (atomic-change-group (tabulated-list-print t))
          (setq entries tabulated-list-entries
                tick (buffer-chars-modified-tick))))
      ;; After-change hooks may retire this request or establish a new draft.
      (when (and (eq (current-buffer) buffer) (funcall current-p)
                 (= tick (buffer-chars-modified-tick)))
        (setq tabulated-list-entries entries
              hermes-plugins--snapshot result
              hermes-plugins--snapshot-stale-p nil
              hermes-plugins--busy nil
              hermes-plugins--status status)))))

(defun hermes-plugins--stale (status)
  "Retain the accepted inventory but refuse mutations, displaying safe STATUS."
  (setq hermes-plugins--snapshot-stale-p t
        hermes-plugins--busy nil
        hermes-plugins--status status)
  (force-mode-line-update))

(defun hermes-plugins--inventory (client hub current-p)
  "Join HUB with CLIENT's published process home while CURRENT-P owns it.
Gated servers may withhold the home.  Inventory remains readable, but
filesystem actions must then fail closed rather than infer a root."
  (when (funcall current-p)
    (hermes--promise-then
     (let ((hermes-dashboard-transport--api-dispatch-guard current-p))
       (hermes-plugins--api client "GET" "/api/status" nil))
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
    (when (and mutation (not (hermes-transport--true-p (hermes-transport--get result 'ok))))
      (error "Plugin operation rejected"))
    (hermes--promise-then
     (if mutation
         (let ((hermes-dashboard-transport--api-dispatch-guard current-p))
           (hermes-plugins--api client "GET" "/api/dashboard/plugins/hub" nil))
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

(defun hermes-plugins--run (client method path body mutation current-p &optional catalog)
  "Run METHOD PATH BODY on CLIENT for MUTATION while CURRENT-P owns it.
Return a promise yielding the readback and safe presentation status.
When CATALOG is non-nil, also read the curated catalog."
  (let (setup-note consent)
    (hermes--promise-then
     (hermes--promise-then
      (hermes-plugins--api client method path body)
      (lambda (result)
        (when (funcall current-p)
          (if (and mutation (equal method "POST") (string-suffix-p "/update" path)
                   (hermes-transport--true-p (hermes-transport--get result 'consent_required)))
              (setq consent (hermes-plugins--consent-text result))
            (when mutation (setq setup-note (hermes-plugins--setup-note result)))
            (hermes-plugins--read-inventories client result mutation current-p catalog)))))
     (lambda (result)
       (if consent (cons 'consent consent)
         (cons result
             (if mutation
                 (concat "Read back; restart/new session may be required (unverified)"
                         setup-note)
               "Configured state; runtime activation unverified")))))))

(defun hermes-plugins--read-inventories (client result mutation current-p catalog)
  "Read RESULT after MUTATION on CLIENT under CURRENT-P, including CATALOG."
  (hermes--promise-then
   (hermes-plugins--readback client result mutation current-p)
   (lambda (hub)
     (if (not catalog) hub
       (when (funcall current-p)
         (hermes--promise-then
          (hermes-dashboard-transport-api-request-async
           "GET" "/api/dashboard/plugins/catalog" :client client :current-p current-p)
          (lambda (entries) (append hub (list :catalog entries)))))))))

(defun hermes-plugins--request (method path &optional body mutation)
  "Request METHOD PATH with BODY and read back a successful MUTATION."
  (hermes-plugins--idle)
  (hermes-browser--next-request-generation)
  (let* ((buffer (current-buffer))
         (generation hermes-browser--request-generation)
         (current-p (hermes-plugins--guard))
         (catalog hermes-plugins--catalog-p)
         settled render-current-p)
    (setq hermes-plugins--busy mutation
          hermes-plugins--consent nil
          hermes-plugins--snapshot-stale-p t
          hermes-plugins--status (if mutation "Updating; awaiting readback" "Loading"))
    (hermes-browser--run-owned
     (lambda (client active)
       (setq render-current-p active)
       (hermes-plugins--run client method path body mutation active catalog))
     current-p
     (lambda (result)
       (if (eq (car result) 'consent)
           (progn
             (setq hermes-plugins--consent
                   (concat (cdr result)
                           (format "\nInstance: %s\nLaunch home: %s\nUpdate endpoint: %s\n"
                                   (hermes-plugins--text (hermes-instance-name hermes-instance))
                                   (hermes-plugins--text
                                    (hermes-transport--get hermes-plugins--snapshot 'hermes_home))
                                   (hermes-plugins--text path))))
             (hermes-plugins--stale "Awaiting capability consent; RET review; update not applied per receipt"))
         (hermes-plugins--render (car result) (cdr result) render-current-p))
       (setq settled t))
     (lambda (_reason)
       ;; Clone failures and renderer conditions can contain private data.
       (hermes-plugins--stale
        "Request failed; snapshot stale; writes may have applied; refresh (details withheld)")
       (setq settled t))
     (lambda ()
       ;; Settlement owns the request lock, not the retired transport's results.
       (when (hermes-browser--request-current-mode-p
              buffer generation 'hermes-plugins-mode)
         (with-current-buffer buffer
           (setq hermes-plugins--busy nil)
           (when (and (not settled) (funcall current-p))
             (hermes-plugins--stale
              "Interrupted; snapshot stale; writes may have applied; refresh"))))))))

(defun hermes-plugins-refresh (&rest _)
  "Refresh the server's agent plugin inventory asynchronously."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--request "GET" "/api/dashboard/plugins/hub"))

(defun hermes-plugins--require-snapshot ()
  "Require a fresh inventory before admitting a plugin mutation."
  (unless (and hermes-plugins--snapshot (not hermes-plugins--snapshot-stale-p))
    (user-error "Refresh the plugin inventory first")))

(defun hermes-plugins--selected ()
  "Return the authoritative plugin at point or signal a user error."
  (hermes-plugins--idle)
  (hermes-plugins--require-snapshot)
  (when hermes-plugins--catalog-p
    (user-error "Return to Installed for this action"))
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
    (when (and permission (not (hermes-transport--true-p (hermes-transport--get row permission))))
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
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--action "enable"))

(defun hermes-plugins-disable ()
  "Confirm and disable the agent plugin at point in server configuration."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--action "disable"))

(defun hermes-plugins-update ()
  "Confirm a backend-permitted Git update of the selected plugin."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--action "update" 'can_update_git))

(defun hermes-plugins-remove ()
  "Confirm and remove the selected plugin when the backend permits removal."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--action "remove" 'can_remove))

(defun hermes-plugins-install ()
  "Confirm installation of trusted plugin code on the owning server.
Install without automatically enabling, forcing replacement, or bypassing
scan refusals.  Existing enablement may persist; inspect the readback."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--idle)
  (hermes-plugins--require-snapshot)
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

(defun hermes-plugins--catalog-rows (snapshot)
  "Return the catalog rows from SNAPSHOT, rejecting missing metadata."
  (let ((catalog (hermes-transport--get snapshot 'catalog)))
    (unless (and catalog (hermes-transport--field-present-p catalog 'entries)
                 (hermes-transport--field-present-p catalog 'removed))
      (error "Incomplete plugin catalog"))
    (append (hermes-transport--get catalog 'entries) nil)))

(defun hermes-plugins--catalog-removed-p (row)
  "Return non-nil if ROW matches the backend's catalog removal list."
  (seq-some
   (lambda (removed)
     (or (equal (hermes-transport--get row 'name)
                (hermes-transport--get removed 'name))
         (equal (hermes-transport--get row 'repo)
                (hermes-transport--get removed 'repo))))
   (hermes-transport--get (hermes-transport--get hermes-plugins--snapshot 'catalog) 'removed)))

(defun hermes-plugins--catalog-entries (snapshot)
  "Return native catalog entries from SNAPSHOT without inferring runtime state."
  (mapcar
   (lambda (row)
     (let ((get (lambda (key) (hermes-plugins--text (hermes-transport--get row key)))))
       (list (hermes-transport--get row 'name)
             (vector (propertize (funcall get 'name) 'face 'hermes-browser-name)
                     (pcase (hermes-transport--get row 'installed)
                       ('t "installed") ((or :false :json-false) "not installed")
                       (_ "not confirmed"))
                     (funcall get 'runtime_status) (funcall get 'tier)
                     (funcall get 'description)))))
   (hermes-plugins--catalog-rows snapshot)))

(defun hermes-plugins--catalog-selected ()
  "Return the uniquely selected catalog entry from a fresh snapshot."
  (hermes-plugins--idle)
  (hermes-plugins--require-snapshot)
  (unless hermes-plugins--catalog-p (user-error "Open the plugin Catalog first"))
  (let ((rows (seq-filter
               (lambda (row) (equal (tabulated-list-get-id)
                                    (hermes-transport--get row 'name)))
               (hermes-plugins--catalog-rows hermes-plugins--snapshot))))
    (unless (= (length rows) 1) (user-error "Select an unambiguous catalog entry"))
    (car rows)))

(defun hermes-plugins--catalog-detail (row)
  "Return inert provenance and declared capabilities for catalog ROW."
  (concat
   (mapconcat
    (lambda (field)
      (format "%s: %s" (car field)
              (propertize (hermes-plugins--text (hermes-transport--get row (cdr field)))
                          'face 'font-lock-constant-face)))
    '(("Plugin" . name) ("Repository" . repo) ("Subdirectory" . subdir)
      ("Tier" . tier) ("Catalog pin" . sha) ("Installed SHA" . installed_sha)
      ("Configured state" . runtime_status) ("Requires Hermes" . requires_hermes)) "\n")
   "\nPlatforms: "
   (hermes-plugins--text-list (hermes-transport--get row 'platforms)) "\n"
   (mapconcat
    (lambda (field)
      (format "%s: %s" (car field)
              (hermes-plugins--text-list
               (hermes-transport--get (hermes-transport--get row 'capabilities) (cdr field)))))
    '(("Declared tools" . provides_tools) ("Declared hooks" . provides_hooks)
      ("Declared middleware" . provides_middleware) ("Environment requirements" . requires_env)) "\n")
   "\n\nTier is provenance, not a safety guarantee.\n"
   "Catalog metadata and repository resolution can change before installation.\n"
   "The displayed pin is informational; install resolves the catalog again.\n"
   "This backend does not honor ref for catalog installs; no ref is sent.\n"
   "Read installed_sha after installation to identify the actual commit.\n"
   "Platform and removal checks are enforced by the backend, not this editor.\n"
   "An empty platform list declares no platform restriction.\n"
   "Runtime status is backend-reported configured state, not proof of activation.\n"
   (when (hermes-plugins--catalog-removed-p row) "REMOVED: installation unavailable.\n")))

(defun hermes-plugins--text-list (values)
  "Return inert text for declared string VALUES, preserving unknown fields."
  (cond ((null values) "(none declared or unavailable)")
        ((and (or (listp values) (vectorp values)) (seq-every-p #'stringp values))
         (if (seq-empty-p values) "(none declared)"
           (mapconcat #'hermes-plugins--text values ", ")))
        (t "unknown")))

(defun hermes-plugins-detail ()
  "Show inert catalog details or the pending capability-consent handoff.
This command never opens external links or approves a capability update."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--idle)
  (let* ((text (or hermes-plugins--consent
                   (hermes-plugins--catalog-detail (hermes-plugins--catalog-selected))))
         (buffer (generate-new-buffer "*Hermes Plugin Review*")))
    (with-current-buffer buffer
      (insert (propertize "Plugin review\n\n" 'face 'bold) text)
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(defun hermes-plugins-catalog-install ()
  "Confirm installation of the catalog entry at point in the launch home.
The backend resolves the catalog again; its displayed pin is informational.
Do not force, enable, override the catalog ref, or retry installation."
  (interactive nil hermes-plugins-mode)
  (let* ((buffer (current-buffer))
         (row (hermes-plugins--catalog-selected))
         (identity (mapcar (lambda (key)
                             (hermes-browser--copy-identity (hermes-transport--get row key)))
                           '(name sha repo subdir)))
         (name (nth 0 identity))
         (sha (nth 1 identity))
         (home (hermes-transport--get hermes-plugins--snapshot 'hermes_home))
         (current-p (hermes-plugins--guard)))
    (unless (and (stringp name) (let ((case-fold-search nil))
                                 (string-match-p "\\`[a-z0-9_-]\\{1,64\\}\\'" name))
                 (hermes-plugins--sha-p sha)
                 (stringp home) (string-prefix-p "/" home)
                 (equal home (hermes-plugins--text home)))
      (user-error "Catalog identity, immutable pin or launch home unavailable"))
    (when (hermes-plugins--catalog-removed-p row)
      (user-error "The backend lists this entry as removed"))
    (when (and
           (yes-or-no-p
            (format "Install catalog entry %s (displayed pin %s, NOT guaranteed) on instance %S, launch-profile home %S (not the selected chat profile)? This can install dependencies and run code; catalog metadata/repo resolution may change; inspect installed_sha afterward; no automatic enabling (existing enablement may persist).  Proceed? "
                    name sha (hermes-instance-name hermes-instance) home))
           (funcall current-p)
           (equal identity (mapcar (lambda (key) (hermes-transport--get row key))
                                   '(name sha repo subdir))))
      (with-current-buffer buffer
        (hermes-plugins--request
         "POST" "/api/dashboard/agent-plugins/install"
         `((identifier . "") (catalog_name . ,name) (force . :false) (enable . :false)) t)))))

(defun hermes-plugins--view (catalog)
  "Switch this browser to CATALOG or installed inventory and refresh."
  (hermes-plugins--idle)
  (unless (eq catalog hermes-plugins--catalog-p)
    (let ((tabulated-list-entries nil)) (atomic-change-group (tabulated-list-print)))
    (setq hermes-plugins--snapshot nil tabulated-list-entries nil))
  (setq hermes-plugins--catalog-p catalog
        tabulated-list-format
        (if catalog
            [("Catalog plugin" 24 t) ("Installed" 16 t) ("Configured state" 18 t)
             ("Tier" 12 t) ("Description" 0 t)]
          [("Plugin" 24 t) ("Configured state" 18 t) ("Source" 12 t)
           ("Version" 10 t) ("Description" 0 t)]))
  (tabulated-list-init-header)
  (hermes-plugins-refresh))

(defun hermes-plugins-catalog ()
  "Browse the owning backend's curated plugin catalog."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--view t))

(defun hermes-plugins-installed ()
  "Return to the owning backend's installed plugin inventory."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--view nil))

(defun hermes-plugins-select-context-engine ()
  "Choose and confirm a context engine from the backend catalog."
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--idle)
  (let* ((buffer (current-buffer))
         (current-p (hermes-plugins--guard))
         (providers (hermes-transport--get hermes-plugins--snapshot 'providers))
         (names (delete-dups
                 (cons "compressor"
                       (mapcar (lambda (row) (hermes-transport--get row 'name))
                               (append (hermes-transport--get providers 'context_options) nil))))))
    (hermes-plugins--require-snapshot)
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
  (interactive nil hermes-plugins-mode)
  (hermes-plugins--idle)
  (hermes-config))

(defvar hermes-plugins-mode-map)

(defun hermes-plugins-popup ()
  "Show agent plugin commands."
  (interactive nil hermes-plugins-mode)
  (keymap-popup hermes-plugins-mode-map))

(defun hermes-plugins--selection-unavailable-p ()
  "Return non-nil when no plugin is selected or a change is pending."
  (or hermes-plugins--busy hermes-plugins--catalog-p (not (tabulated-list-get-id))))

(keymap-popup-define hermes-plugins-mode-map
  :parent tabulated-list-mode-map
  :exit-key "C-g"
  :group "Inventory"
  "g" ("Refresh" hermes-plugins-refresh :stay-open t
       :inapt-if (lambda () hermes-plugins--busy))
  "i" ("Install" hermes-plugins-install
       :inapt-if (lambda () hermes-plugins--busy))
  "?" ("Help" hermes-plugins-popup)
  "q" ("Quit view" quit-window)
  :group "Catalog and review"
  "C" ("Catalog" hermes-plugins-catalog)
  "I" ("Installed" hermes-plugins-installed)
  "RET" ("Details / consent handoff" hermes-plugins-detail)
  "a" ("Install selected entry" hermes-plugins-catalog-install
       :if (lambda () hermes-plugins--catalog-p))
  :group ("Selected plugin" :inapt-if #'hermes-plugins--selection-unavailable-p)
  "e" ("Enable" hermes-plugins-enable)
  "d" ("Disable" hermes-plugins-disable)
  "u" ("Update" hermes-plugins-update)
  "D" ("Remove" hermes-plugins-remove)
  :row
  :group ("Configuration" :inapt-if (lambda () hermes-plugins--busy))
  "c" ("Schema and environment" hermes-plugins-configure)
  "x" ("Context engine" hermes-plugins-select-context-engine))

(put 'hermes-plugins-mode-map-popup 'command-modes '(hermes-plugins-mode))

(define-derived-mode hermes-plugins-mode tabulated-list-mode "Hermes Plugins"
  "Browse server-profile agent plugins, not frontend extensions."
  (hermes-browser--next-request-generation)
  (setq-local tabulated-list-format
              [("Plugin" 24 t) ("Configured state" 18 t) ("Source" 12 t)
               ("Version" 10 t) ("Description" 0 t)])
  (setq-local hermes-browser--snapshot-variables
              '(hermes-plugins--snapshot hermes-plugins--busy hermes-plugins--consent))
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
      (hermes-buffer--claim 'hermes-plugins-mode)
      (hermes-browser--own-instance instance)
      (setq-local header-line-format '(:eval (hermes-plugins--header))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-plugins-refresh))))

(provide 'hermes-plugins)
;;; hermes-plugins.el ends here
