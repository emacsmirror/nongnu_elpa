;;; hermes-mcp.el --- MCP server browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' browser over the dashboard MCP REST endpoints.  It lists
;; configured MCP servers, tests the selected server, and toggles its enabled
;; state through the dashboard API -- never by editing config files or invoking
;; the `hermes' CLI.

;;; Code:

(require 'cl-lib)
(require 'keymap-popup)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)
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

(defun hermes-mcp--tools (server)
  "Return SERVER's configured tools list, or nil."
  (let ((tools (hermes-transport--get server 'tools)))
    (and (listp tools) tools)))

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

(defun hermes-mcp--render (result)
  "Render MCP servers from RESULT in `hermes-mcp-buffer-name'."
  (with-current-buffer (get-buffer-create hermes-mcp-buffer-name)
    (unless (derived-mode-p 'hermes-mcp-mode)
      (hermes-mcp-mode))
    (hermes-mcp--remember-servers result)
    (setq tabulated-list-entries
          (hermes-mcp--rows result hermes-mcp--test-results))
    (tabulated-list-print t)))

(defun hermes-mcp--fetch (&optional display)
  "Fetch and render the MCP server list asynchronously.
DISPLAY pops the buffer when non-nil; revert refreshes in place without it."
  (hermes-browser--run-on-client
   (lambda (client) (hermes-mcp--api "GET" "/servers" nil nil :client client))
   (lambda (result)
     (hermes-mcp--render result)
     (when display (pop-to-buffer hermes-mcp-buffer-name)))))

(defun hermes-mcp--revert (&rest _)
  "Refresh the MCP server list."
  (hermes-mcp--fetch))

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
  (hermes-mcp--ensure-state)
  (let ((name (hermes-mcp--name-at-point))
        (buffer (current-buffer)))
    (hermes-browser--run-on-client
     (lambda (client)
       (hermes--promise-then
        (hermes-mcp--api "POST" (hermes-mcp--server-path name "/test")
                         nil nil :client client)
        (lambda (result)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (hermes-mcp--ensure-state)
              (puthash name result hermes-mcp--test-results)
              (hermes-mcp--message-test-result name result)))
          (hermes-mcp--api "GET" "/servers" nil nil :client client))))
     #'hermes-mcp--render)))

(defun hermes-mcp-toggle ()
  "Enable or disable the MCP server at point through the dashboard API."
  (interactive)
  (let* ((server (hermes-mcp--server-at-point))
         (name (or (hermes-transport--field server 'name)
                   (hermes-mcp--name-at-point))))
    (unless (hermes-transport--field-present-p server 'enabled)
      (user-error "MCP server %s has no enabled state; refresh or update Hermes Agent/dashboard"
                  (hermes-mcp--redact-display name)))
    (let ((next (not (hermes-mcp--enabled-p server))))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes--promise-then
          (hermes-mcp--api "PUT" (hermes-mcp--server-path name "/enabled")
                           `((enabled . ,(if next t :false))) nil :client client)
          (lambda (_result)
            (message "Hermes: %s %s; change applies to new sessions/gateway reload"
                     (if next "enabled" "disabled")
                     (hermes-mcp--redact-display name))
            (hermes-mcp--api "GET" "/servers" nil nil :client client))))
       #'hermes-mcp--render))))

(defvar hermes-mcp-mode-map)

(keymap-popup-define hermes-mcp-mode-map
  "Keymap for `hermes-mcp-mode'."
  :parent tabulated-list-mode-map
  :description "Hermes MCP Servers"
  :group "Server"
  "RET" ("Test server" hermes-mcp-test)
  "t" ("Test server" hermes-mcp-test)
  "e" ("Enable/disable" hermes-mcp-toggle)
  :group "View"
  "g" ("Refresh" revert-buffer)
  "?" ("Help" hermes-mcp-mode-map-popup))

(define-derived-mode hermes-mcp-mode tabulated-list-mode "Hermes MCP"
  "Major mode for browsing Hermes MCP servers."
  :interactive nil
  (hermes-mcp--ensure-state)
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
