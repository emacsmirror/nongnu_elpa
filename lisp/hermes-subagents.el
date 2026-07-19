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
(require 'hermes-dashboard-rpc)
(require 'hermes-browser)

(defun hermes-subagents--rows (result)
  "Return `tabulated-list' entries for a `delegation.status' RESULT.
Each active subagent's goal is indented by its spawn depth."
  (mapcar
   (lambda (subagent)
     (let ((id (hermes-transport--scalar-string
                (hermes-transport--get subagent 'subagent_id)))
           (depth (or (hermes-transport--get subagent 'depth) 0)))
       (list id
             (vector (hermes-browser--face-cell
                      (concat (make-string (* 2 (max 0 depth)) ?\s)
                              (or (hermes-transport--scalar-string
                                   (hermes-transport--get subagent 'goal)) ""))
                      'hermes-browser-goal)
                     (hermes-browser--status-cell
                      (or (hermes-transport--scalar-string
                           (hermes-transport--get subagent 'status)) "")
                      'hermes-browser-status)
                     (hermes-browser--face-cell
                      (or (hermes-transport--scalar-string
                           (hermes-transport--get subagent 'model)) "")
                      'hermes-browser-model)
                     (hermes-browser--face-cell
                      (or (hermes-transport--get subagent 'tool_count) 0)
                      'hermes-browser-tool-count)))))
   (hermes-transport--get result 'active)))

(defun hermes-subagents-interrupt ()
  "Interrupt the subagent at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No subagent on this line"))
    (when (yes-or-no-p (format "Interrupt subagent %s? " id))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-subagent-interrupt client id))
       (lambda (_result) (message "Hermes: interrupted %s" id))))))

;;;###autoload (autoload 'hermes-list-subagents "hermes-subagents" nil t)
(hermes-define-list-browser subagents
  :title "Hermes Subagents"
  :buffer "*Hermes Subagents*"
  :doc "Major mode listing active Hermes subagents."
  :command-doc "Browse active Hermes subagents as a delegation tree."
  :columns [("Subagent" 44 t) ("Status" 12 t) ("Model" 18 t) ("Tools" 6 t)]
  :fetch (lambda (client)
           (hermes-dashboard-transport-call-fn
            #'hermes-dashboard-transport-delegation-status client))
  :rows #'hermes-subagents--rows
  :keys ("k" #'hermes-subagents-interrupt))

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
