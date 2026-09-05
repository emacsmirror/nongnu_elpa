;;; hermes-subagents.el --- Active subagent browser for Hermes  -*- lexical-binding: t; -*-

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
  (let ((id (tabulated-list-get-id))
        (origin (current-buffer))
        (owner hermes-instance))
    (unless id (user-error "No subagent on this line"))
    (when (yes-or-no-p (format "Interrupt subagent %s? " id))
      (hermes-browser--run-on-client
       (lambda (client)
         (hermes-dashboard-transport-call-fn
          #'hermes-dashboard-transport-subagent-interrupt client id))
       (lambda (result)
         (when (and (hermes-browser--buffer-mode-p origin 'hermes-subagents-mode)
                    (equal owner (buffer-local-value 'hermes-instance origin)))
           (if (eq (hermes-transport--get result 'found) t)
               (message "Hermes: interrupted %s" id)
             (message "Hermes: subagent %s already finished or was not found" id))
           (with-current-buffer origin
             (hermes-subagents--revert))))))))

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

;;; Exact-session observations

(require 'hermes-chat-buffer)

(defface hermes-work-failed '((t :inherit error))
  "Face for observed failed work." :group 'hermes)
(defface hermes-work-done '((t :inherit success))
  "Face for observed completed processes." :group 'hermes)
(defface hermes-work-kind '((t :inherit font-lock-type-face))
  "Face for observed work kinds." :group 'hermes)

(defvar-local hermes-work--owner nil
  "Exact chat attachment displayed by this view, never a buffer name.")

(defun hermes-work--view-p (owner)
  "Return non-nil when OWNER still owns its exact list buffer."
  (let ((view (plist-get owner :view)))
    (and (hermes-browser--buffer-mode-p view 'hermes-work-mode)
         (eq owner (buffer-local-value 'hermes-work--owner view)))))

(defun hermes-work--current-p (owner)
  "Return non-nil when OWNER is a current chat attachment."
  (when-let* ((current (plist-get owner :current-p)))
    (funcall current owner)))

(defun hermes-work--observations (owner)
  "Return OWNER's last rows annotated with source freshness and observation time."
  (mapcan
   (lambda (kind)
     (let* ((source (plist-get owner kind))
            (current (and (hermes-work--current-p owner)
                          (memq (plist-get source :coverage) '(current partial)))))
       (mapcar (lambda (row)
                 (append (list :stale (not current)
                               :observed (plist-get source :observed)) row))
               (plist-get source :rows))))
   '(:delegates :processes)))

(defun hermes-work--state (row)
  "Return the semantic label and face for observed ROW."
  (if (plist-get row :stale)
      '("Stale" . hermes-work-unknown)
    (pcase (plist-get row :state)
      ('running '("Running" . hermes-work-running))
      ('done '("Done" . hermes-work-done))
      ('failed (cons (format "Failed (%s)" (plist-get row :exit-code)) 'hermes-work-failed))
      (_ (cons (if (equal (plist-get row :status) "exited") "Exited (?)" "Unknown")
               'hermes-work-unknown)))))

(defun hermes-work--rows (owner)
  "Return sorted, kind-qualified table entries from OWNER's observations."
  (mapcar
   (lambda (row)
     (let* ((state (hermes-work--state row))
            (started (plist-get row :started))
            (observed (plist-get row :observed)))
       (list (plist-get row :key)
             (vector (propertize (car state) 'face (cdr state))
                     (propertize (symbol-name (plist-get row :kind)) 'face 'hermes-work-kind)
                     (hermes-browser--face-cell
                      (or (plist-get row :goal) (plist-get row :command) "—") 'default)
                     (propertize (if (and started observed)
                                     (format "%.0fs" (max 0 (- observed started))) "—")
                                 'face 'shadow)))))
   (sort (hermes-work--observations owner)
         (lambda (a b)
           (let* ((states '(running unknown failed done))
                  (rank (lambda (row)
                          (or (seq-position states (if (plist-get row :stale) 'unknown
                                                     (plist-get row :state))) 1)))
                  (x (funcall rank a)) (y (funcall rank b)))
             (if (= x y)
                 (string-lessp (format "%s:%s" (plist-get a :kind) (plist-get a :id))
                               (format "%s:%s" (plist-get b :kind) (plist-get b :id)))
               (< x y)))))))

(defun hermes-work--coverage (owner kind)
  "Return OWNER's truthful observation coverage for KIND."
  (if (hermes-work--current-p owner)
      (or (plist-get (plist-get owner kind) :coverage) 'unknown)
    'stale))

(defun hermes-work--context-owner ()
  "Return this chat or work view's exact owner, without following replacements."
  (if (derived-mode-p 'hermes-work-mode) hermes-work--owner
    (and (derived-mode-p 'hermes-chat-mode) hermes-chat--work-owner)))

;;;###autoload
(defun hermes-chat-workers-label ()
  "Return a local Workers count for the invoking chat or its exact work view.
Count only observed running delegates.  Qualify stale or incomplete evidence."
  (let* ((owner (hermes-work--context-owner))
         (source (plist-get owner :delegates))
         (coverage (if owner (hermes-work--coverage owner :delegates) 'unknown))
         (unknown (seq-some (lambda (row) (eq (plist-get row :state) 'unknown))
                            (plist-get source :rows))))
    (format "Workers (%d)%s" (hermes-chat--work-running-count source)
            (if (and (eq coverage 'current) (not unknown)) ""
              (format " · %s" (if (eq coverage 'current) 'unknown coverage))))))

(defun hermes-work--scope-text (owner)
  "Return full local scope and freshness details for OWNER, even when detached."
  (concat
   (format "Scope: runtime %s; durable key %s\n\n"
           (plist-get owner :runtime) (or (plist-get owner :key) "unknown"))
   (if (buffer-live-p (plist-get owner :buffer))
       (with-current-buffer (plist-get owner :buffer)
         (let ((hermes-chat--work-owner owner)) (hermes-chat--work-details)))
     "Owner destroyed; observations are stale.\nDisappearance does not prove completion.")))

(defun hermes-work-scope-details ()
  "Display scope, freshness and limitations for this exact work view."
  (interactive)
  (let ((text (hermes-work--scope-text hermes-work--owner)))
    (with-help-window "*Hermes Work Scope*" (princ text))))

(defun hermes-work--render (owner)
  "Repaint OWNER's exact list from snapshots only, without selecting it."
  (when (hermes-work--view-p owner)
    (with-current-buffer (plist-get owner :view)
      (setq tabulated-list-entries (hermes-work--rows owner))
      ;; Tabulated lists do not truncate the final column themselves.
      (dolist (entry tabulated-list-entries)
        (let ((cells (cadr entry)))
          (aset cells 3 (truncate-string-to-width
                         (aref cells 3) (cadr (aref tabulated-list-format 3))
                         nil nil "…"))))
      ;; This read-only projection must not call modification hooks mid-print:
      ;; they can replace the view or unwind an unfinished chat settlement.
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (tabulated-list-print t)
        (save-excursion
          (goto-char (point-min))
          (let ((width (window-body-width (get-buffer-window (current-buffer) t))))
            (insert (propertize "Observed work\n" 'face 'bold)
                    (propertize
                     (truncate-string-to-width
                      (format "Agents %s · Processes %s · h Scope"
                              (hermes-work--coverage owner :delegates)
                              (hermes-work--coverage owner :processes))
                      width nil nil "…") 'face 'shadow)
                    "\n")
            (unless tabulated-list-entries
              (insert (propertize "No work observed in these sources.\n" 'face 'shadow)))))))))

(defun hermes-work--changed ()
  "Repaint the current chat's observation list after a local state change."
  (when hermes-chat--work-owner (hermes-work--render hermes-chat--work-owner)))

(defun hermes-work--detach ()
  "Release only this exact view's cross-link on kill or mode change."
  (when (eq (plist-get hermes-work--owner :view) (current-buffer))
    (setf (plist-get hermes-work--owner :view) nil)))

(defun hermes-work--resize (window)
  "Fit the work columns to WINDOW without fetching or changing ownership."
  (when (derived-mode-p 'hermes-work-mode)
    (let ((format (hermes-browser--dynamic-format
                   (- (window-body-width window) tabulated-list-padding)
                   '(("State" 6 0 nil 12) ("Kind" 4 0 nil 8)
                     ("Goal" 4 1) ("Elapsed" 3 0 nil 8)))))
      (unless (equal format tabulated-list-format)
        (setq tabulated-list-format format)
        (tabulated-list-init-header)
        (hermes-work--render hermes-work--owner)))))

(defun hermes-work-refresh ()
  "Request a refresh from this list's exact owner, if still attached."
  (interactive)
  (unless (and (hermes-work--view-p hermes-work--owner)
               (hermes-work--current-p hermes-work--owner))
    (user-error "Work owner detached; reopen from the attached chat"))
  (funcall (plist-get hermes-work--owner :refresh)))

(defun hermes-work-instance-subagents ()
  "Browse Instance subagents explicitly, outside this session's observations."
  (interactive)
  (unless (hermes-work--current-p hermes-work--owner)
    (user-error "Work owner detached"))
  (let ((hermes-instance (plist-get hermes-work--owner :instance)))
    (hermes-list-subagents)))

(defun hermes-work-details ()
  "Display inert read-only details for the observed row at point."
  (interactive)
  (let* ((owner hermes-work--owner)
         (key (tabulated-list-get-id))
         (row (seq-find (lambda (entry) (equal key (plist-get entry :key)))
                        (hermes-work--observations owner))))
    (unless row (user-error "No observed work on this line"))
    (let ((buffer (generate-new-buffer "*Hermes Work Details*")))
      (with-current-buffer buffer
        (insert (format "Observed %s — %s\n\nScope: runtime %s; durable key %s\n"
                        (plist-get row :kind) (car (hermes-work--state row))
                        (plist-get owner :runtime) (plist-get owner :key)))
        (dolist (field '((:id . "ID") (:status . "Raw status") (:observed . "Observed at")
                         (:owner . "Root owner") (:parent . "Parent") (:goal . "Goal")
                         (:model . "Model") (:command . "Command") (:cwd . "Remote cwd (inert)")
                         (:exit-code . "Exit code") (:output-tail . "Backend output tail")))
          (when (plist-member row (car field))
            (insert (format "%s: %s\n" (cdr field) (or (plist-get row (car field)) "—")))))
        (insert "\n" (hermes-work--scope-text owner))
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defvar-keymap hermes-work-mode-map
  :parent tabulated-list-mode-map
  "RET" #'hermes-work-details
  "g" #'hermes-work-refresh
  "q" #'quit-window
  "i" #'hermes-work-instance-subagents
  "h" #'hermes-work-scope-details)

(define-derived-mode hermes-work-mode tabulated-list-mode "Observed Work"
  "Browse only one chat attachment's observed delegates and processes."
  (setq-local tabulated-list-padding 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-refresh)))
  (add-hook 'window-size-change-functions #'hermes-work--resize nil t)
  (add-hook 'kill-buffer-hook #'hermes-work--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-work--detach nil t))

;;;###autoload
(defun hermes-chat-work ()
  "Browse this chat's observed work without acquiring another client."
  (interactive)
  (let ((owner (hermes-work--context-owner)))
    (unless (and (hermes-work--current-p owner)
                 (or (derived-mode-p 'hermes-chat-mode) (hermes-work--view-p owner)))
      (user-error "No attached session work owner"))
    (unless (hermes-work--view-p owner)
      (let ((buffer (generate-new-buffer "*Hermes Observed Work*")))
        (with-current-buffer buffer
          (hermes-work-mode)
          (setq hermes-work--owner owner)
          (setq-local hermes-instance (plist-get owner :instance)))
        (setf (plist-get owner :view) buffer
              (plist-get owner :view-valid-p) #'hermes-work--view-p
              (plist-get owner :render) #'hermes-work--render)))
    (with-current-buffer (plist-get owner :buffer)
      (add-hook 'hermes-chat-state-change-hook #'hermes-work--changed nil t))
    (pop-to-buffer (plist-get owner :view))
    (hermes-work--resize (selected-window))
    (hermes-work--render owner)))

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
