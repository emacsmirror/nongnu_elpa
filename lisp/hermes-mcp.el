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
(require 'hermes-sessions)

(defgroup hermes-mcp nil
  "MCP server browser for Hermes Agent."
  :group 'hermes)

(defcustom hermes-mcp-buffer-name "*Hermes MCP Servers*"
  "Name of the Hermes MCP server browser buffer."
  :type 'string
  :group 'hermes-mcp)

(defvar-local hermes-mcp--servers nil
  "Hash table mapping MCP server names to their latest server alist.")

(defvar-local hermes-mcp--test-results nil
  "Hash table mapping MCP server names to their latest test-result alist.")

(defun hermes-mcp--field (object key)
  "Return OBJECT's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get object key)) ""))

(defun hermes-mcp--enabled-p (server)
  "Return non-nil when SERVER is enabled."
  (eq (hermes-transport--get server 'enabled) t))

(defun hermes-mcp--enabled-label (server)
  "Return SERVER's enabled state as a short label."
  (if (hermes-mcp--enabled-p server) "on" "off"))

(defun hermes-mcp--result-for (name test-results)
  "Return NAME's stored test result from TEST-RESULTS."
  (and (hash-table-p test-results) (gethash name test-results)))

(defun hermes-mcp--tools (object)
  "Return OBJECT's tools list, or nil."
  (let ((tools (hermes-transport--get object 'tools)))
    (and (listp tools) tools)))

(defun hermes-mcp--test-tool-count (name &optional test-results)
  "Return NAME's latest test tool count from TEST-RESULTS.
Use `hermes-mcp--test-results' when TEST-RESULTS is nil; return an empty
string when no test result exists."
  (if-let* ((result (hermes-mcp--result-for
                    name (or test-results hermes-mcp--test-results)))
            (tools (hermes-mcp--tools result)))
      (number-to-string (length tools))
    ""))

(defun hermes-mcp--configured-tool-count (server)
  "Return SERVER's configured selected-tool count as a string, or empty."
  (if-let* ((tools (hermes-mcp--tools server)))
      (number-to-string (length tools))
    ""))

(defun hermes-mcp--tool-count (server test-results)
  "Return SERVER's best available tool count using TEST-RESULTS."
  (let ((tested (hermes-mcp--test-tool-count
                 (hermes-mcp--field server 'name) test-results)))
    (if (string-empty-p tested)
        (hermes-mcp--configured-tool-count server)
      tested)))

(defun hermes-mcp--status (server test-results)
  "Return SERVER's display status using TEST-RESULTS when present."
  (cond
   ((not (hermes-mcp--enabled-p server)) "disabled")
   ((when-let* ((result (hermes-mcp--result-for
                         (hermes-mcp--field server 'name) test-results)))
      (if (eq (hermes-transport--get result 'ok) t) "ok" "failed")))
   ((and-let* ((status (hermes-mcp--field server 'status))
               ((not (string-empty-p status))))
      status))
   (t "configured")))

(defun hermes-mcp--rows (result &optional test-results)
  "Return `tabulated-list' rows for an MCP servers RESULT.
TEST-RESULTS maps server names to `test' endpoint responses."
  (mapcar
   (lambda (server)
     (let ((name (hermes-mcp--field server 'name)))
       (list name
             (vector name
                     (hermes-mcp--field server 'transport)
                     (hermes-mcp--enabled-label server)
                     (hermes-mcp--status server test-results)
                     (hermes-mcp--tool-count server test-results)))))
   (hermes-transport--get result 'servers)))

(defun hermes-mcp--unsupported-api-error-p (message)
  "Return non-nil when MESSAGE indicates the dashboard lacks MCP REST APIs."
  (and (string-match-p "/api/mcp" message)
       (or (string-match-p "HTTP 404" message)
           (string-match-p "HTTP 405" message)
           (string-match-p "HTTP 501" message))))

(cl-defun hermes-mcp--api (method path &optional body query &key secrets client)
  "Call the dashboard MCP REST API METHOD PATH.
BODY and QUERY are passed to `hermes-dashboard-transport-api-request'.
SECRETS are redacted from any surfaced error.  CLIENT supplies a live dashboard
session token when available."
  (condition-case err
      (hermes-dashboard-transport-api-request
       method (concat "/api/mcp" path) :body body :query query :secrets secrets
       :client client)
    (error
     (let ((message (hermes-dashboard-transport--redact-secret
                     (error-message-string err) secrets)))
       (if (hermes-mcp--unsupported-api-error-p message)
           (user-error
            "Hermes dashboard MCP REST API is unavailable; update Hermes Agent/dashboard")
         (signal (car err) (list message)))))))

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
  (dolist (server (hermes-transport--get result 'servers))
    (when-let* ((name (hermes-mcp--field server 'name))
                ((not (string-empty-p name))))
      (puthash name server hermes-mcp--servers))))

(defun hermes-mcp--with-client (fn)
  "Call FN with a dashboard client, reporting MCP REST errors."
  (hermes-sessions--with-client
   (lambda (client done)
     (let ((cleaned nil))
       (condition-case err
           (let ((after (funcall fn client)))
             (unless cleaned
               (setq cleaned t)
               (funcall done))
             (when (functionp after)
               (funcall after)))
         (error
          (unless cleaned
            (setq cleaned t)
            (funcall done))
          (message "Hermes: %s" (error-message-string err))))))))

(defun hermes-mcp--render (result)
  "Render MCP servers from RESULT in `hermes-mcp-buffer-name'."
  (with-current-buffer (get-buffer-create hermes-mcp-buffer-name)
    (unless (derived-mode-p 'hermes-mcp-mode)
      (hermes-mcp-mode))
    (hermes-mcp--remember-servers result)
    (setq tabulated-list-entries
          (hermes-mcp--rows result hermes-mcp--test-results))
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun hermes-mcp--fetch (&optional client)
  "Fetch and render the MCP server list, optionally through CLIENT."
  (if client
      (hermes-mcp--render
       (hermes-mcp--api "GET" "/servers" nil nil :client client))
    (hermes-mcp--with-client #'hermes-mcp--fetch)))

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
        (user-error "No MCP server details for %s" name))))

(defun hermes-mcp--server-path (name &rest segments)
  "Return the MCP server REST path for NAME extended by SEGMENTS."
  (concat "/servers/" (url-hexify-string name) (apply #'concat segments)))

(defun hermes-mcp-test ()
  "Test the MCP server at point and update its status/tool count."
  (interactive)
  (hermes-mcp--ensure-state)
  (let ((name (hermes-mcp--name-at-point))
        (buffer (current-buffer)))
    (hermes-mcp--with-client
     (lambda (client)
       (with-current-buffer buffer
         (let ((result (hermes-mcp--api
                        "POST" (hermes-mcp--server-path name "/test")
                        nil nil :client client)))
           (puthash name result hermes-mcp--test-results)
           (if (eq (hermes-transport--get result 'ok) t)
               (message "Hermes: %s has %s MCP tool(s)"
                        name (hermes-mcp--test-tool-count name))
             (message "Hermes: %s test failed: %s"
                      name
                      (hermes-dashboard-transport--redact-secret
                       (or (hermes-mcp--field result 'error) "unknown error")
                       (and (hermes-dashboard-transport-client-p client)
                            (list (hermes-dashboard-transport-client-token
                                   client)))))
           (hermes-mcp--fetch client))))))))

(defun hermes-mcp-toggle ()
  "Enable or disable the MCP server at point through the dashboard API."
  (interactive)
  (let* ((server (hermes-mcp--server-at-point))
         (name (hermes-mcp--field server 'name))
         (enabled (hermes-mcp--enabled-p server))
         (next (not enabled)))
    (hermes-mcp--with-client
     (lambda (client)
       (hermes-mcp--api
        "PUT" (hermes-mcp--server-path name "/enabled")
        `((enabled . ,(if next t :false))) nil :client client)
       (message "Hermes: %s %s; change applies to new sessions/gateway reload"
                (if next "enabled" "disabled") name)
       (hermes-mcp--fetch client)))))

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
  (hermes-mcp--fetch))

(provide 'hermes-mcp)
;;; hermes-mcp.el ends here
