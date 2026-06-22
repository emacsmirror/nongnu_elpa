;;; hermes-subagents.el --- Active subagent browser for Hermes  -*- lexical-binding: t; -*-

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

;; A `tabulated-list' view of active Hermes subagents from `delegation.status',
;; indented by spawn depth to show the delegation tree.  `k' interrupts the
;; subagent at point via `subagent.interrupt'.

;;; Code:

(require 'tabulated-list)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-sessions)

(defun hermes-subagents--rows (result)
  "Return `tabulated-list' entries for a `delegation.status' RESULT.
Each active subagent's goal is indented by its spawn depth."
  (mapcar
   (lambda (subagent)
     (let ((id (hermes-transport--scalar-string
                (hermes-transport--get subagent 'subagent_id)))
           (depth (or (hermes-transport--get subagent 'depth) 0)))
       (list id
             (vector (concat (make-string (* 2 (max 0 depth)) ?\s)
                             (or (hermes-transport--scalar-string
                                  (hermes-transport--get subagent 'goal)) ""))
                     (or (hermes-transport--scalar-string
                          (hermes-transport--get subagent 'status)) "")
                     (or (hermes-transport--scalar-string
                          (hermes-transport--get subagent 'model)) "")
                     (format "%s" (or (hermes-transport--get subagent 'tool_count) 0))))))
   (hermes-transport--get result 'active)))

(defun hermes-subagents--revert (&rest _)
  "Refresh the active subagent list."
  (hermes-list-subagents))

(defvar-keymap hermes-subagents-mode-map
  :doc "Keymap for `hermes-subagents-mode'."
  :parent tabulated-list-mode-map
  "k" #'hermes-subagents-interrupt)

(define-derived-mode hermes-subagents-mode tabulated-list-mode "Hermes Subagents"
  "Major mode listing active Hermes subagents."
  :interactive nil
  (setq tabulated-list-format
        [("Subagent" 44 t) ("Status" 12 t) ("Model" 18 t) ("Tools" 6 t)])
  (setq-local revert-buffer-function #'hermes-subagents--revert)
  (tabulated-list-init-header))

(defun hermes-subagents--render (result)
  "Display active subagents from RESULT in the subagents buffer."
  (with-current-buffer (get-buffer-create "*Hermes Subagents*")
    (unless (derived-mode-p 'hermes-subagents-mode)
      (hermes-subagents-mode))
    (setq tabulated-list-entries (hermes-subagents--rows result))
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun hermes-subagents-interrupt ()
  "Interrupt the subagent at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No subagent on this line"))
    (when (yes-or-no-p (format "Interrupt subagent %s? " id))
      (hermes-sessions--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-subagent-interrupt client id))
       (lambda (_result) (message "Hermes: interrupted %s" id))))))

;;;###autoload
(defun hermes-list-subagents ()
  "Browse active Hermes subagents as a delegation tree."
  (interactive)
  (hermes-sessions--run-on-client
   (lambda (client)
     (hermes-dashboard-transport-call-fn
      #'hermes-dashboard-transport-delegation-status client))
   (lambda (result) (hermes-subagents--render result))))

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
