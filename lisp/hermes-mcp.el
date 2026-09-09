;;; hermes-mcp.el --- MCP server browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard MCP REST endpoints.  It lists
;; configured MCP servers and supports connection tests, add/remove, catalog
;; installation, and backend-owned OAuth through the dashboard API.  It never
;; edits config files or invokes the `hermes' CLI.

;;; Code:

(require 'cl-lib)
(require 'keymap-popup)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
(require 'url-parse)
(require 'browse-url)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-promise)
(require 'hermes-browser)

(defgroup hermes-mcp nil
  "MCP server browser for Hermes Agent."
  :group 'hermes)

(defcustom hermes-mcp-buffer-name "*Hermes MCP Servers*"
  "Name of the Hermes MCP server browser buffer."
  :type 'string)

(defvar-local hermes-mcp--operation nil
  "Owned OAuth or catalog installation operation, or nil.")

(defvar-local hermes-mcp--servers nil
  "Hash table mapping MCP server names to their latest server alist.")

(defvar-local hermes-mcp--test-results nil
  "Hash table mapping MCP server names to their latest test-result alist.")

(defun hermes-mcp--secret-like-value-p (text)
  "Return non-nil for secret-bearing display value TEXT."
  (let ((case-fold-search t))
    (or (string-match-p "\\b[A-Za-z0-9_-]\\{48,\\}\\b" text)
        (string-match-p
         "\\b[A-Za-z0-9_.-]*\\(?:token\\|secret\\|password\\|api[_-]?key\\)[A-Za-z0-9_.-]*[=:]"
         text))))

(defun hermes-mcp--redact-display (text)
  "Return TEXT with secret-shaped material redacted for display."
  (let ((case-fold-search t)
        (safe (hermes-dashboard-transport--redact-secret text)))
    (setq safe
          (replace-regexp-in-string
           "\\(--\\(?:token\\|secret\\|password\\|api[-_]?key\\)\\)[ \\t]+[^[:space:],;)\"']+"
           "\\1 <redacted>" safe t nil))
    (setq safe
          (replace-regexp-in-string
           "\\b\\([A-Za-z0-9_.-]*\\(?:token\\|secret\\|password\\|api[_-]?key\\)[A-Za-z0-9_.-]*[=:]\\)[^[:space:],;)\"']+"
           "\\1<redacted>" safe t nil))
    (if (hermes-mcp--secret-like-value-p safe)
        (replace-regexp-in-string
         "\\b[A-Za-z0-9_-]\\{48,\\}\\b" "<redacted>" safe t nil)
      safe)))

(defun hermes-mcp--field (object key)
  "Return OBJECT's KEY as a redacted display string."
  (if-let* ((text (hermes-transport--field object key)))
      (hermes-mcp--redact-display text)
    ""))

(defun hermes-mcp--enabled-label (server)
  "Return SERVER's enabled state as a short display label."
  (cond
   ((not (hermes-transport--field-present-p server 'enabled)) "?")
   ((eq (hermes-transport--get server 'enabled) t) "on")
   (t "off")))

(defun hermes-mcp--enabled-p (server)
  "Return non-nil when SERVER is enabled."
  (eq (hermes-transport--get server 'enabled) t))

(defun hermes-mcp--server-type (server)
  "Return SERVER's transport/type display string."
  (or (and-let* ((transport (hermes-mcp--field server 'transport))
                 ((not (string-empty-p transport))))
        transport)
      (hermes-mcp--field server 'type)))

(defun hermes-mcp--explicit-tool-count (object)
  "Return OBJECT's explicit tool-count display string, or empty."
  (let ((count (hermes-transport--get-any
                object '(tool_count tool-count toolCount tools_count tools-count
                                     toolsCount)))
        (tools (hermes-transport--get object 'tools)))
    (cond
     ((numberp count) (number-to-string count))
     ((stringp count) (hermes-mcp--redact-display count))
     ((and (hermes-transport--field-present-p object 'tools) (listp tools))
      (number-to-string (length tools)))
     (t ""))))

(defun hermes-mcp--result-for (name &optional test-results)
  "Return NAME's stored test result from TEST-RESULTS."
  (and (hash-table-p test-results) (gethash name test-results)))

(defun hermes-mcp--test-tool-count (name &optional test-results)
  "Return NAME's latest test tool count from TEST-RESULTS.
Use `hermes-mcp--test-results' when TEST-RESULTS is nil; return an empty
string when no test result exists."
  (if-let* ((result (hermes-mcp--result-for
                    name (or test-results hermes-mcp--test-results))))
      (hermes-mcp--explicit-tool-count result)
    ""))

(defun hermes-mcp--tool-count (server &optional test-results)
  "Return SERVER's best available tool-count display string.
Prefer TEST-RESULTS over the server summary when present."
  (let* ((tested (and-let* ((name (hermes-transport--field server 'name)))
                   (hermes-mcp--test-tool-count name test-results)))
         (summary (hermes-mcp--explicit-tool-count server)))
    (if (and tested (not (string-empty-p tested)))
        tested
      summary)))

(defun hermes-mcp--status (server &optional test-results)
  "Return SERVER's display status using TEST-RESULTS when present."
  (cond
   ((and-let* ((name (hermes-transport--field server 'name))
               (result (hermes-mcp--result-for name test-results)))
      (if (eq (hermes-transport--get result 'ok) t) "ok" "failed")))
   ((and-let* ((status (hermes-mcp--field server 'status))
               ((not (string-empty-p status))))
      status))
   (t (pcase (hermes-mcp--enabled-label server)
        ("on" "configured")
        ("off" "disabled")
        (_ "unknown")))))

(defun hermes-mcp--server-list (result)
  "Return the MCP server list from RESULT."
  (let ((servers (hermes-transport--get result 'servers)))
    (cond
     ((and (listp servers)
           (or (null servers)
               (cl-every #'hermes-transport--object-p servers)))
      servers)
     ((hermes-transport--event-list-p result) result)
     (t nil))))

(defun hermes-mcp--rows (result &optional test-results)
  "Return `tabulated-list' rows for an MCP servers RESULT.
TEST-RESULTS maps server names to `test' endpoint responses."
  (mapcar
   (lambda (server)
     (let* ((raw-name (or (hermes-transport--field server 'name) ""))
            (display-name (hermes-mcp--redact-display raw-name)))
       (list raw-name
             (vector (hermes-browser--face-cell
                      display-name 'hermes-browser-name)
                     (hermes-browser--face-cell
                      (hermes-mcp--server-type server) 'hermes-browser-type)
                     (hermes-browser--status-cell
                      (hermes-mcp--enabled-label server)
                      'hermes-browser-enabled)
                     (hermes-browser--status-cell
                      (hermes-mcp--status server test-results)
                      'hermes-browser-status)
                     (hermes-browser--face-cell
                      (hermes-mcp--tool-count server test-results)
                      'hermes-browser-tool-count)))))
   (hermes-mcp--server-list result)))

(defun hermes-mcp--unsupported-api-error-p (message)
  "Return non-nil when MESSAGE indicates the dashboard lacks MCP REST APIs."
  (and (string-match-p "/api/mcp" message)
       (or (string-match-p "HTTP 404" message)
           (string-match-p "HTTP 405" message)
           (string-match-p "HTTP 501" message))))

(cl-defun hermes-mcp--api (method path &optional body query &key secrets client)
  "Return a promise of the dashboard MCP REST API METHOD PATH.
BODY and QUERY extend the request.  SECRETS are redacted from any surfaced
error.  CLIENT supplies a live dashboard session token when available."
  (hermes--promise-catch
   (hermes-dashboard-transport-api-request-async
    method (concat "/api/mcp" path) :body body :query query :secrets secrets
    :client client)
   (lambda (reason)
     (let ((message (hermes-mcp--redact-display
                     (hermes-dashboard-transport--redact-secret reason secrets))))
       (hermes--promise-rejected
        (if (hermes-mcp--unsupported-api-error-p message)
            "Hermes dashboard MCP REST API is unavailable; update Hermes Agent/dashboard"
          message))))))

(defun hermes-mcp--ensure-state ()
  "Ensure the current MCP buffer has state tables."
  (unless (hash-table-p hermes-mcp--servers)
    (setq hermes-mcp--servers (make-hash-table :test #'equal)))
  (unless (hash-table-p hermes-mcp--test-results)
    (setq hermes-mcp--test-results (make-hash-table :test #'equal))))

(defun hermes-mcp--remember-servers (result)
  "Remember server objects from RESULT in `hermes-mcp--servers'."
  (hermes-mcp--ensure-state)
  (clrhash hermes-mcp--servers)
  (dolist (server (hermes-mcp--server-list result))
    (when-let* ((name (hermes-transport--field server 'name))
                ((not (string-empty-p name))))
      (puthash name server hermes-mcp--servers))))

(defun hermes-mcp--render (result &optional buffer)
  "Render MCP servers from RESULT in BUFFER or the standard MCP buffer."
  (with-current-buffer (or buffer (get-buffer-create hermes-mcp-buffer-name))
    (unless (derived-mode-p 'hermes-mcp-mode)
      (hermes-mcp-mode))
    (hermes-mcp--remember-servers result)
    (setq tabulated-list-entries
          (hermes-mcp--rows result hermes-mcp--test-results))
    (tabulated-list-print t)))

(defun hermes-mcp--fetch (&optional display target generation)
  "Fetch and render the MCP server list asynchronously.
DISPLAY pops the buffer when non-nil; revert refreshes in place without it.
TARGET and GENERATION identify an existing buffer-owned refresh."
  (let ((instance (hermes-instance-resolve))
        (target (or target
                    (and display (get-buffer-create hermes-mcp-buffer-name))
                    (current-buffer))))
    (with-current-buffer target
      (unless (derived-mode-p 'hermes-mcp-mode)
        (hermes-mcp-mode))
      (when (and hermes-mcp--operation (not (equal hermes-instance instance)))
        (hermes-mcp--cleanup))
      (hermes-mcp--idle)
      (hermes-browser--own-instance instance))
    (let ((generation (or generation
                          (with-current-buffer target
                            (hermes-browser--next-request-generation)))))
      (with-current-buffer target
        (hermes-mcp--request
         (list target generation (copy-tree instance))
         (lambda (client)
           (hermes-mcp--api "GET" "/servers" nil nil :client client))
         (lambda (result)
           (hermes-mcp--render result target)
           (when display (pop-to-buffer target))))))))

(defun hermes-mcp--revert (&rest _)
  "Refresh the MCP server list."
  (hermes-mcp--idle)
  (let ((target (current-buffer))
        (generation (hermes-browser--next-request-generation)))
    (hermes-mcp--fetch nil target generation)))

(defun hermes-mcp--name-at-point ()
  "Return the MCP server name on the current line, or signal `user-error'."
  (or (tabulated-list-get-id) (user-error "No MCP server on this line")))

(defun hermes-mcp--server-at-point ()
  "Return the MCP server object on the current line."
  (let ((name (hermes-mcp--name-at-point)))
    (or (and (hash-table-p hermes-mcp--servers)
             (gethash name hermes-mcp--servers))
        (user-error "No MCP server details for %s"
                    (hermes-mcp--redact-display name)))))

(defun hermes-mcp--server-path (name &rest segments)
  "Return the MCP server REST path for NAME extended by SEGMENTS."
  (concat "/servers/" (url-hexify-string name) (apply #'concat segments)))

(defun hermes-mcp--message-test-result (name result)
  "Report NAME's MCP test RESULT to the minibuffer."
  (let ((display-name (hermes-mcp--redact-display name)))
    (if (eq (hermes-transport--get result 'ok) t)
        (message "Hermes: %s has %s MCP tool(s)"
                 display-name (hermes-mcp--explicit-tool-count result))
      (message "Hermes: %s test failed: %s"
               display-name
               (or (and-let* ((error (hermes-mcp--field result 'error))
                              ((not (string-empty-p error))))
                     error)
                   "unknown error")))))

(defun hermes-mcp-test ()
  "Test the MCP server at point and update its status/tool count."
  (interactive)
  (hermes-mcp--idle)
  (hermes-mcp--ensure-state)
  (let ((name (hermes-mcp--name-at-point))
        (context (hermes-mcp--context)))
    (hermes-mcp--request
     context
     (lambda (client)
       (hermes--promise-then
        (hermes-mcp--api "POST" (hermes-mcp--server-path name "/test")
                         nil nil :client client)
        (lambda (result)
          (when (hermes-mcp--current-p context)
            (with-current-buffer (car context)
              (puthash name result hermes-mcp--test-results)
              (hermes-mcp--message-test-result name result))
            (hermes-mcp--api "GET" "/servers" nil nil :client client)))))
     (lambda (result) (hermes-mcp--render result (current-buffer))))))

(defun hermes-mcp-toggle ()
  "Enable or disable the MCP server at point through the dashboard API."
  (interactive)
  (hermes-mcp--idle)
  (let* ((server (hermes-mcp--server-at-point))
         (name (or (hermes-transport--field server 'name)
                   (hermes-mcp--name-at-point))))
    (unless (hermes-transport--field-present-p server 'enabled)
      (user-error "MCP server %s has no enabled state; refresh or update Hermes Agent/dashboard"
                  (hermes-mcp--redact-display name)))
    (let ((next (not (hermes-mcp--enabled-p server)))
          (context (hermes-mcp--context)))
      (hermes-mcp--request
       context
       (lambda (client)
         (hermes-mcp--api "PUT" (hermes-mcp--server-path name "/enabled")
                          `((enabled . ,(if next t :false))) nil :client client))
       (lambda (_result)
         (message "Hermes: %s %s; change applies to new sessions/gateway reload"
                  (if next "enabled" "disabled")
                  (hermes-mcp--redact-display name))
         (remhash name hermes-mcp--test-results)
         (hermes-mcp--revert))))))

;;; Management

(defun hermes-mcp--idle ()
  "Refuse another action while an owned operation is active."
  (when hermes-mcp--operation
    (user-error "MCP operation active; cancel it first")))

(defun hermes-mcp--context ()
  "Capture the current browser request and exact instance."
  (list (current-buffer) (hermes-browser--next-request-generation)
        (copy-tree hermes-instance)))

(defun hermes-mcp--current-p (context)
  "Return non-nil when CONTEXT still owns this browser and instance."
  (and (hermes-browser--request-current-mode-p
        (car context) (cadr context) 'hermes-mcp-mode)
       (equal (nth 2 context)
              (buffer-local-value 'hermes-instance (car context)))))

(defun hermes-mcp--request (context make-promise success)
  "Run MAKE-PROMISE and SUCCESS only for current CONTEXT."
  (hermes-browser--run-on-client
   (lambda (client)
     (if (hermes-mcp--current-p context)
         (funcall make-promise client)
       (hermes--promise-resolved nil)))
   (lambda (result)
     (when (hermes-mcp--current-p context)
       (with-current-buffer (car context) (funcall success result))))
   (lambda (_reason)
     ;; Provisioning and OAuth errors can contain arbitrary upstream secrets.
     (when (hermes-mcp--current-p context)
       (message "Hermes: MCP request failed; refresh to check backend state")))))

(defun hermes-mcp--create-body (name transport target args env auth token)
  "Build a create payload for NAME, TRANSPORT and TARGET.
ARGS and ENV are stdio-only.  AUTH and TOKEN configure HTTP authentication."
  (unless (and (stringp name) (not (string-empty-p (string-trim name)))
               (stringp target) (not (string-empty-p (string-trim target))))
    (user-error "Server name and endpoint/command are required"))
  (unless (member auth '("none" "oauth" "header"))
    (user-error "Unsupported MCP authentication"))
  (pcase transport
    ("http"
     (unless (and (null args) (null env)
                  (or (not (equal auth "header"))
                      (and (stringp token) (not (string-empty-p token)))))
       (user-error "HTTP MCP requires a token for header auth, and no args/env"))
     `((name . ,name) (url . ,target) (auth . ,auth)
       ,@(and (equal auth "header") `((bearer_token . ,token)))))
    ("stdio"
     (unless (and (equal auth "none") (null token))
       (user-error "Stdio MCP uses environment credentials, not HTTP auth"))
     `((name . ,name) (command . ,target) (args . ,(vconcat args))
       (env . ,(or env (make-hash-table :test #'equal)))))
    (_ (user-error "Unsupported MCP transport"))))

(defun hermes-mcp--read-env ()
  "Read stdio environment names and hidden values without value history."
  (let (env name)
    (while (not (string-empty-p
                 (setq name (read-string "Environment name (empty to finish): "))))
      (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)
        (user-error "Invalid environment variable name"))
      (setf (alist-get name env nil nil #'equal)
            (read-passwd (format "%s value: " name))))
    env))

(defun hermes-mcp-add ()
  "Add a configured MCP server using the remote dashboard API.
Stdio arguments are entered individually, not interpreted by a shell."
  (interactive)
  (hermes-mcp--idle)
  (let* ((context (hermes-mcp--context))
         (name (read-string "MCP server name: "))
         (transport (completing-read "Transport: " '("http" "stdio") nil t))
         (target (read-string (if (equal transport "http") "MCP URL: " "Command: ")))
         (auth (if (equal transport "http")
                   (completing-read "Authentication: " '("none" "oauth" "header") nil t)
                 "none"))
         (token (and (equal auth "header") (read-passwd "Bearer token: ")))
         (args (and (equal transport "stdio") (hermes-mcp--read-args)))
         (env (and (equal transport "stdio") (hermes-mcp--read-env)))
         (body (hermes-mcp--create-body name transport target args env auth token)))
    (when (and (hermes-mcp--current-p context)
               (yes-or-no-p "Save this MCP server on the selected Hermes instance? ")
               (hermes-mcp--current-p context))
      (hermes-mcp--request
       context
       (lambda (client)
         (hermes-mcp--api "POST" "/servers" body nil
                          :secrets (append (and token (list token)) (mapcar #'cdr env))
                          :client client))
       (lambda (_result) (hermes-mcp--revert))))))

(defun hermes-mcp--read-args ()
  "Read command arguments individually, preserving their literal values."
  (let (args arg)
    (while (not (string-empty-p
                 (setq arg (read-string "Argument (empty to finish): "))))
      (push arg args))
    (nreverse args)))

(defun hermes-mcp-remove ()
  "Remove the server at point after explicit confirmation."
  (interactive)
  (hermes-mcp--idle)
  (let ((name (hermes-mcp--name-at-point))
        (context (hermes-mcp--context)))
    (when (and (yes-or-no-p (format "Remove MCP server %s from this instance? "
                                    (hermes-mcp--redact-display name)))
               (hermes-mcp--current-p context))
      (hermes-mcp--request
       context
       (lambda (client)
         (hermes-mcp--api "DELETE" (hermes-mcp--server-path name) nil nil :client client))
       (lambda (_result)
         (remhash name hermes-mcp--test-results)
         (hermes-mcp--revert))))))

(defun hermes-mcp--catalog-env (entry)
  "Read only the credential fields declared by catalog ENTRY."
  (delq nil
        (mapcar
         (lambda (spec)
           (let* ((name (hermes-transport--field spec 'name))
                  (value (read-passwd (format "%s: " (hermes-mcp--redact-display name)))))
             (when (and (eq (hermes-transport--get spec 'required) t)
                        (string-empty-p value))
               (user-error "Required catalog credential is empty"))
             (and (not (string-empty-p value)) (cons name value))))
         (hermes-transport--get entry 'required_env))))

(defun hermes-mcp--catalog-review (entry)
  "Display catalog ENTRY's execution and trust details before installation."
  (with-help-window (generate-new-buffer "*Hermes MCP Catalog Entry*")
    (dolist (field '(name description source installed enabled transport auth_type command args url
                         install_url install_ref bootstrap default_enabled post_install))
      (princ (format "%s: %s\n" field
                     (hermes-mcp--redact-display
                      (format "%s" (hermes-transport--get entry field))))))
    (princ "\nInstallation can execute the command/bootstrap on the backend.\n")))

(defun hermes-mcp--catalog-installable-p (entry)
  "Return non-nil when ENTRY explicitly declares no git installation.
The released backend cannot guarantee that bootstrap subprocesses keep the
dashboard's profile.  Do not infer remote scope from local profile settings."
  (and (hermes-transport--field-present-p entry 'install_url)
       (null (hermes-transport--get entry 'install_url))
       (null (hermes-transport--get entry 'bootstrap))))

(defun hermes-mcp-catalog ()
  "Browse the remote catalog and confirm installation of one entry.
Git-bootstrap installs are unavailable: their profile cannot be guaranteed."
  (interactive)
  (hermes-mcp--idle)
  (let ((context (hermes-mcp--context)))
    (hermes-mcp--request
     context (lambda (client) (hermes-mcp--api "GET" "/catalog" nil nil :client client))
     (lambda (result)
       (let* ((entries (hermes-transport--get result 'entries))
              (names (mapcar (lambda (entry) (hermes-transport--field entry 'name)) entries)))
         (unless entries (user-error "MCP catalog is empty or unavailable"))
         (let* ((name (completing-read "Catalog MCP: " names nil t))
                (entry (cl-find name entries :test #'equal
                                :key (lambda (item) (hermes-transport--field item 'name)))))
           (when (hermes-mcp--current-p context)
             (hermes-mcp--catalog-review entry)
             (if (not (hermes-mcp--catalog-installable-p entry))
                 (message "Hermes: catalog install unavailable; bootstrap profile cannot be guaranteed")
               (when (and (yes-or-no-p "Trust and install this catalog entry on the backend? ")
                          (hermes-mcp--current-p context))
                 (let ((env (hermes-mcp--catalog-env entry)))
                   (when (hermes-mcp--current-p context)
                     (hermes-mcp--start-operation 'install name env))))))))))))

;;; Owned remote operations

(defun hermes-mcp--operation-current-p (operation)
  "Return non-nil when OPERATION still owns its buffer and instance."
  (and (hermes-mcp--current-p (plist-get operation :context))
       (eq operation (buffer-local-value
                      'hermes-mcp--operation (car (plist-get operation :context))))
       (not (plist-get operation :closed))))

(defun hermes-mcp--cancel-flow (operation)
  "Best-effort cancel only OPERATION's exact remote OAuth flow."
  (when-let* ((id (plist-get operation :flow)))
    (setf (plist-get operation :flow) nil)
    (condition-case nil
        (hermes--promise-catch
         (hermes-mcp--api "DELETE" (concat "/oauth/flows/" (url-hexify-string id))
                          nil nil :client (plist-get operation :client))
         #'ignore)
      ((error quit) nil))))

(defun hermes-mcp--close-operation (operation &optional cancel)
  "Release OPERATION locally, and request remote OAuth cancellation if CANCEL."
  (unless (plist-get operation :closed)
    (setf (plist-get operation :closed) t)
    (when-let* ((timer (plist-get operation :timer))) (cancel-timer timer))
    (when-let* ((timeout (plist-get operation :timeout))) (cancel-timer timeout))
    (when cancel (hermes-mcp--cancel-flow operation))
    (let ((buffer (car (plist-get operation :context))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq operation hermes-mcp--operation)
            (setq hermes-mcp--operation nil)))))
    (funcall (plist-get operation :done))))

(defun hermes-mcp--cleanup ()
  "Invalidate requests and release buffer-owned operation resources."
  (hermes-browser--next-request-generation)
  (when hermes-mcp--operation
    (hermes-mcp--close-operation hermes-mcp--operation t)))

(defun hermes-mcp-cancel ()
  "Cancel owned OAuth, or stop monitoring a remote catalog installation.
Stopping installation monitoring does not terminate the backend process."
  (interactive)
  (let ((kind (plist-get hermes-mcp--operation :kind)))
    (hermes-mcp--cleanup)
    (message (if (eq kind 'install)
                 "Hermes: stopped monitoring; backend installation may still run"
               "Hermes: OAuth cancellation requested"))))

(defun hermes-mcp--authorization-url-p (text)
  "Return non-nil for a safe HTTPS authorization URL TEXT.
OAuth state validation and token exchange belong to the remote backend."
  (and (stringp text) (not (string-match-p "[[:space:][:cntrl:]]" text))
       (let ((url (url-generic-parse-url text)))
         (and (equal (url-type url) "https")
              (not (string-empty-p (or (url-host url) "")))
              (null (url-user url)) (null (url-password url))))))

(defun hermes-mcp-authenticate ()
  "Authorize the HTTP MCP server at point through backend-owned OAuth.
The browser returns to the backend callback URL, which must be reachable."
  (interactive)
  (hermes-mcp--idle)
  (let* ((server (hermes-mcp--server-at-point))
         (name (hermes-mcp--name-at-point))
         (context (hermes-mcp--context)))
    (unless (and (hermes-transport--field server 'url)
                 (not (equal (hermes-transport--field server 'auth) "header")))
      (user-error "OAuth requires an HTTP server without header authentication"))
    (when (and (yes-or-no-p "Start MCP OAuth on this instance and open its authorization site? ")
               (hermes-mcp--current-p context))
      (hermes-mcp--start-operation 'oauth name nil))))

(defun hermes-mcp--start-operation (kind name env)
  "Start an owned KIND operation for NAME with catalog ENV credentials."
  (hermes-mcp--idle)
  (let ((context (hermes-mcp--context)))
    (hermes-browser--with-client
     (lambda (client done)
       (if (not (hermes-mcp--current-p context))
           (funcall done)
         (with-current-buffer (car context)
           (hermes-mcp--begin-operation context kind name env client done)))))))

(defun hermes-mcp--begin-operation (context kind name env client done)
  "Own CONTEXT's KIND request for NAME with ENV using CLIENT and DONE cleanup."
  (let* ((operation (list :context context :kind kind :name name :client client
                          :done done :deadline (+ (float-time) 600)
                          :timer nil :timeout nil :flow nil :action nil
                          :opened nil :closed nil))
         (guard (hermes-browser--dispatch-guard client)))
    (setq hermes-mcp--operation operation)
    (setf (plist-get operation :timeout)
          (run-at-time 600 nil #'hermes-mcp--operation-failed operation))
    (hermes-mcp--operation-request
     operation
     (lambda ()
       (let ((hermes-dashboard-transport--api-dispatch-guard
              (lambda () (and (funcall guard)
                              (hermes-mcp--operation-current-p operation)))))
         (if (eq kind 'oauth)
             (hermes-mcp--api "POST" (hermes-mcp--server-path name "/auth")
                              nil nil :client client)
           (hermes-mcp--api "POST" "/catalog/install"
                            `((name . ,name) (enable . t)
                              (env . ,(or env (make-hash-table :test #'equal))))
                            nil :secrets (mapcar #'cdr env) :client client))))
     (lambda (result) (hermes-mcp--operation-started operation result)))))

(defun hermes-mcp--operation-request (operation request success)
  "Call REQUEST for OPERATION and pass its response to SUCCESS."
  (condition-case nil
      (hermes--promise-catch
       (hermes--promise-then (funcall request) success)
       (lambda (_reason) (hermes-mcp--operation-failed operation)))
    ((error quit) (hermes-mcp--operation-failed operation))))

(defun hermes-mcp--operation-failed (operation)
  "Cancel OPERATION and report failure only to its current owner."
  (let ((current (hermes-mcp--operation-current-p operation)))
    (hermes-mcp--close-operation operation t)
    (when current
      (message "Hermes: MCP operation failed or timed out; refresh to check backend state"))))

(defun hermes-mcp--operation-started (operation result)
  "Accept RESULT for OPERATION, cleaning up a late OAuth creation."
  (when (eq (plist-get operation :kind) 'oauth)
    (let ((id (hermes-transport--field result 'flow_id)))
      (when (and id (string-match-p "\\`[A-Za-z0-9_-]+\\'" id))
        (setf (plist-get operation :flow) id))))
  (if (not (hermes-mcp--operation-current-p operation))
      ;; A cancelled start may still create a flow: dispose that exact handle.
      (progn
        (hermes-mcp--close-operation operation t)
        (hermes-mcp--cancel-flow operation))
    (if (eq (plist-get operation :kind) 'oauth)
        (hermes-mcp--oauth-result operation result)
      (if (not (eq (hermes-transport--get result 'ok) t))
          (hermes-mcp--operation-failed operation)
        (if (eq (hermes-transport--get result 'background) t)
            (let ((action (hermes-transport--field result 'action)))
              (if (and action (string-match-p "\\`mcp-install-[a-z0-9-]+\\'" action))
                  (progn (setf (plist-get operation :action) action)
                         (hermes-mcp--schedule operation))
                (hermes-mcp--operation-failed operation)))
          (hermes-mcp--operation-complete operation))))))

(defun hermes-mcp--oauth-result (operation result)
  "Validate the identity and OAuth state in OPERATION's RESULT."
  (when (hermes-mcp--operation-current-p operation)
    (if (not (and (plist-get operation :flow)
                  (equal (plist-get operation :flow) (hermes-transport--field result 'flow_id))
                  (equal (plist-get operation :name) (hermes-transport--field result 'server_name))))
        (hermes-mcp--operation-failed operation)
      (pcase (hermes-transport--field result 'status)
        ("approved" (hermes-mcp--operation-complete operation))
        ("starting" (hermes-mcp--schedule operation))
        ("authorization_required"
         (let ((url (hermes-transport--field result 'authorization_url)))
           (if (not (hermes-mcp--authorization-url-p url))
               (hermes-mcp--operation-failed operation)
             (unless (plist-get operation :opened)
               (setf (plist-get operation :opened) t)
               (browse-url url))
             (hermes-mcp--schedule operation))))
        (_ (hermes-mcp--operation-failed operation))))))

(defun hermes-mcp--schedule (operation)
  "Schedule OPERATION's next bounded status read."
  (when (hermes-mcp--operation-current-p operation)
    (when-let* ((timer (plist-get operation :timer))) (cancel-timer timer))
    (setf (plist-get operation :timer)
          (run-at-time 2 nil #'hermes-mcp--poll operation))))

(defun hermes-mcp--poll (operation)
  "Read remote OPERATION status without blocking Emacs."
  (if (or (not (hermes-mcp--operation-current-p operation))
          (> (float-time) (plist-get operation :deadline)))
      (hermes-mcp--operation-failed operation)
    (hermes-mcp--operation-request
     operation
     (lambda ()
       (let ((client (plist-get operation :client)))
         (if (eq (plist-get operation :kind) 'oauth)
             (hermes-mcp--api "GET" (concat "/oauth/flows/" (plist-get operation :flow))
                              nil nil :client client)
           (hermes-dashboard-transport-api-request-async
            "GET" (concat "/api/actions/" (plist-get operation :action) "/status")
            :query '((lines . "1")) :client client))))
     (lambda (result)
       (when (hermes-mcp--operation-current-p operation)
         (if (eq (plist-get operation :kind) 'oauth)
             (hermes-mcp--oauth-result operation result)
           (cond
            ((eq (hermes-transport--get result 'running) t) (hermes-mcp--schedule operation))
            ((eql (hermes-transport--get result 'exit_code) 0)
             (hermes-mcp--operation-complete operation))
            (t (hermes-mcp--operation-failed operation)))))))))

(defun hermes-mcp--operation-complete (operation)
  "Settle OPERATION and refresh the backend-authoritative server list."
  (when (hermes-mcp--operation-current-p operation)
    (hermes-mcp--close-operation operation)
    (with-current-buffer (car (plist-get operation :context))
      (clrhash hermes-mcp--test-results)
      (message (if (plist-get operation :action)
                   "Hermes: catalog entry action succeeded; refreshing configuration (shared handle)"
                 "Hermes: MCP operation completed; refreshing configuration"))
      (hermes-mcp--revert))))

(defvar hermes-mcp-mode-map)

(keymap-popup-define hermes-mcp-mode-map
  "Keymap for `hermes-mcp-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes MCP Servers"
  :group "Server"
  "RET" ("Test server" hermes-mcp-test)
  "t" ("Test server" hermes-mcp-test)
  "e" ("Enable/disable" hermes-mcp-toggle)
  :group "Configure"
  "a" ("Add server" hermes-mcp-add)
  "d" ("Remove server" hermes-mcp-remove)
  "c" ("Install from catalog" hermes-mcp-catalog)
  :group "Authorization"
  "o" ("Authorize OAuth" hermes-mcp-authenticate)
  "k" ("Cancel / stop monitoring" hermes-mcp-cancel)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-mcp-mode-map-popup))

(define-derived-mode hermes-mcp-mode tabulated-list-mode "Hermes MCP"
  "Major mode for browsing Hermes MCP servers."
  :interactive nil
  (setq-local hermes-browser--snapshot-variables
              '(hermes-mcp--servers hermes-mcp--test-results))
  (hermes-mcp--ensure-state)
  (add-hook 'kill-buffer-hook #'hermes-mcp--cleanup nil t)
  (add-hook 'change-major-mode-hook #'hermes-mcp--cleanup nil t)
  (setq tabulated-list-format
        [("Name" 24 t) ("Type" 8 t) ("Enabled" 8 t)
         ("Status" 12 t) ("Tools" 7 t)])
  (setq-local revert-buffer-function #'hermes-mcp--revert)
  (tabulated-list-init-header))

;;;###autoload
(defun hermes-list-mcp ()
  "Browse configured Hermes MCP servers via the dashboard API."
  (interactive)
  (hermes-mcp--fetch t))

(provide 'hermes-mcp)
;;; hermes-mcp.el ends here
