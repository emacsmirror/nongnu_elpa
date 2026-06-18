;;; hermes-inventory.el --- Inventory browsers for Hermes  -*- lexical-binding: t; -*-

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

;; Read-only `tabulated-list' browsers over the dashboard inventory methods:
;; toolsets (`tools.list'), skills (`skills.manage'), running agents
;; (`agents.list'), and plugins (`plugins.list').

;;; Code:

(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-sessions)

(defun hermes-inventory--str (object key)
  "Return OBJECT's KEY as a display string."
  (or (hermes-transport--scalar-string (hermes-transport--get object key)) ""))

(defun hermes-inventory--toolset-rows (result)
  "Return inventory rows for a `tools.list' RESULT."
  (mapcar (lambda (toolset)
            (list (hermes-inventory--str toolset 'name)
                  (vector (hermes-inventory--str toolset 'name)
                          (if (hermes-transport--get toolset 'enabled) "on" "off")
                          (format "%s" (or (hermes-transport--get toolset 'tool_count) 0))
                          (hermes-inventory--str toolset 'description))))
          (hermes-transport--get result 'toolsets)))

(defun hermes-inventory--skill-rows (result)
  "Return inventory rows for a `skills.manage' list RESULT.
The result groups skill names by category."
  (let (rows)
    (dolist (entry (hermes-transport--get result 'skills))
      (let ((category (format "%s" (car entry))))
        (dolist (name (cdr entry))
          (let ((name (hermes-transport--scalar-string name)))
            (push (list (concat category "/" (or name ""))
                        (vector category (or name "")))
                  rows)))))
    (nreverse rows)))

(defun hermes-inventory--agent-rows (result)
  "Return inventory rows for an `agents.list' RESULT."
  (mapcar (lambda (process)
            (list (hermes-inventory--str process 'session_id)
                  (vector (hermes-inventory--str process 'session_id)
                          (hermes-inventory--str process 'status)
                          (format "%s" (or (hermes-transport--get process 'uptime) 0))
                          (hermes-inventory--str process 'command))))
          (hermes-transport--get result 'processes)))

(defun hermes-inventory--plugin-rows (result)
  "Return inventory rows for a `plugins.list' RESULT."
  (mapcar (lambda (plugin)
            (list (hermes-inventory--str plugin 'name)
                  (vector (hermes-inventory--str plugin 'name)
                          (hermes-inventory--str plugin 'version)
                          (if (hermes-transport--get plugin 'enabled) "on" "off"))))
          (hermes-transport--get result 'plugins)))

(defconst hermes-inventory--specs
  `(("Toolsets" "tools.list" nil
     [("Toolset" 24 t) ("On" 4 t) ("Tools" 6 t) ("Description" 50 nil)]
     ,#'hermes-inventory--toolset-rows)
    ("Skills" "skills.manage" ((action . "list"))
     [("Category" 20 t) ("Skill" 48 t)]
     ,#'hermes-inventory--skill-rows)
    ("Agents" "agents.list" nil
     [("Session" 18 t) ("Status" 10 t) ("Uptime" 8 t) ("Command" 50 nil)]
     ,#'hermes-inventory--agent-rows)
    ("Plugins" "plugins.list" nil
     [("Plugin" 30 t) ("Version" 12 t) ("On" 4 t)]
     ,#'hermes-inventory--plugin-rows))
  "Inventory categories as (LABEL METHOD PARAMS FORMAT ROW-FN).")

(defvar-local hermes-inventory--spec nil
  "The inventory spec backing the current buffer, for refresh.")

(defun hermes-inventory--revert (&rest _)
  "Re-fetch the inventory shown in the current buffer."
  (when hermes-inventory--spec
    (hermes-inventory--fetch hermes-inventory--spec)))

(define-derived-mode hermes-inventory-mode tabulated-list-mode "Hermes Inventory"
  "Major mode for read-only Hermes inventory listings."
  :interactive nil
  (setq-local revert-buffer-function #'hermes-inventory--revert))

(defun hermes-inventory--render (spec rows)
  "Display ROWS for inventory SPEC, wiring `g' to re-fetch."
  (with-current-buffer (get-buffer-create (format "*Hermes %s*" (car spec)))
    (unless (derived-mode-p 'hermes-inventory-mode)
      (hermes-inventory-mode))
    (setq hermes-inventory--spec spec)
    (setq tabulated-list-format (nth 3 spec))
    (tabulated-list-init-header)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun hermes-inventory--fetch (spec)
  "Fetch and render the inventory described by SPEC.
Reuses a live chat connection when one exists; otherwise connects a transient
client for the listing."
  (hermes-sessions--with-client
   (lambda (client done)
     (hermes-dashboard-transport-request
      client (nth 1 spec) (nth 2 spec)
      (lambda (result)
        (funcall done)
        (hermes-inventory--render spec (funcall (nth 4 spec) result)))
      (lambda (message)
        (funcall done)
        (message "Hermes: %s" message))))))

;;;###autoload
(defun hermes-list-inventory ()
  "Browse a Hermes inventory: toolsets, skills, agents, or plugins."
  (interactive)
  (hermes-inventory--fetch
   (assoc (completing-read "Hermes inventory: "
                           (mapcar #'car hermes-inventory--specs) nil t)
          hermes-inventory--specs)))

(provide 'hermes-inventory)
;;; hermes-inventory.el ends here
