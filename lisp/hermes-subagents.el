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
;; indented by spawn depth to show the delegation tree.  Instance inventory
;; is read-only; the chat Work view owns session-qualified worker actions.

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
  "Refuse control from the read-only instance inventory.
Open Workers from the owning chat to request a session-qualified interrupt."
  (interactive nil hermes-subagents-mode)
  (user-error "Instance inventory is read-only; open Workers from its chat"))

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
  :rows #'hermes-subagents--rows)

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
    (and (buffer-live-p view)
         (with-current-buffer view (hermes-buffer--owned-p 'hermes-work-mode))
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
    (concat "Workers "
            (propertize (number-to-string (hermes-chat--work-running-count source))
                        'face 'keymap-popup-value)
            (unless (and (eq coverage 'current) (not unknown))
              (propertize (format " · %s" (if (eq coverage 'current) 'unknown coverage))
                          'face 'hermes-work-unknown)))))

(defun hermes-work--scope-text (owner)
  "Return full local scope and freshness details for OWNER, even when detached."
  (concat
   (format "Scope: runtime %s; durable key %s\n\n"
           (plist-get owner :runtime) (or (plist-get owner :key) "unknown"))
   "Roster visibility includes resumed lineage, not control authority.\nThe backend may refuse interrupt, steering or tail for a visible worker.\n\n"
   (if (buffer-live-p (plist-get owner :buffer))
       (with-current-buffer (plist-get owner :buffer)
         (let ((hermes-chat--work-owner owner)) (hermes-chat--work-details)))
     "Owner destroyed; observations are stale.\nDisappearance does not prove completion.")))

(defun hermes-work-scope-details ()
  "Display scope, freshness and limitations for this exact work view."
  (interactive nil hermes-work-mode)
  (let ((text (hermes-work--scope-text hermes-work--owner))
        (buffer (hermes-buffer--get "*Hermes Work Scope*" #'help-mode)))
    (with-help-window buffer (princ text))))

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
                      (format "Agents %s · Processes %s · ? Help"
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
    (setf (plist-get hermes-work--owner :view) nil)
    (hermes-work--settle-owner hermes-work--owner)))

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
  (interactive nil hermes-work-mode)
  (unless (and (hermes-work--view-p hermes-work--owner)
               (hermes-work--current-p hermes-work--owner))
    (user-error "Work owner detached; reopen from the attached chat"))
  (funcall (plist-get hermes-work--owner :refresh)))

(defun hermes-work-instance-subagents ()
  "Browse Instance subagents explicitly, outside this session's observations."
  (interactive nil hermes-work-mode)
  (unless (hermes-work--current-p hermes-work--owner)
    (user-error "Work owner detached"))
  (let ((hermes-instance (plist-get hermes-work--owner :instance)))
    (hermes-list-subagents)))

(defun hermes-work-details ()
  "Display inert read-only details for the observed row at point."
  (interactive nil hermes-work-mode)
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

;;; Session-qualified worker actions

(defun hermes-work--selected-worker ()
  "Return the exact current worker row at point, or refuse the action."
  (let* ((owner hermes-work--owner)
         (source (plist-get owner :delegates))
         (key (tabulated-list-get-id))
         (row (seq-find (lambda (entry) (equal key (plist-get entry :key)))
                        (plist-get source :rows))))
    (unless (and (hermes-work--view-p owner) (hermes-work--current-p owner)
                 (eq (plist-get source :coverage) 'current)
                 (stringp (plist-get owner :runtime))
                 (not (string-empty-p (plist-get owner :runtime)))
                 (eq (plist-get row :kind) 'delegate)
                 (eq (plist-get row :state) 'running))
      (user-error "No current running worker; refresh or reopen from its chat"))
    row))

(defun hermes-work--action-current-p (owner row)
  "Return non-nil if OWNER and exact ROW remain actionable after input."
  (and (hermes-work--view-p owner) (hermes-work--current-p owner)
       (with-current-buffer (plist-get owner :view)
         (eq row (ignore-errors (hermes-work--selected-worker))))))

(defun hermes-work--row-current-p (owner row)
  "Return non-nil if OWNER retains exact ROW, regardless of poll freshness."
  (and (hermes-work--current-p owner)
       (memq row (plist-get (plist-get owner :delegates) :rows))))

(defun hermes-work--control-current-p (operation)
  "Return non-nil if OPERATION retains its original presentation authority."
  (let ((owner (plist-get operation :owner)))
    (and (eq (plist-get operation :view) (plist-get owner :view))
         (hermes-work--view-p owner)
         (hermes-work--row-current-p owner (plist-get operation :row)))))

(defun hermes-work--control-finish (operation text)
  "Settle exact OPERATION once and report TEXT without touching any view."
  (unless (plist-get operation :settled)
    (let ((owner (plist-get operation :owner)))
      (setf (plist-get operation :settled) t
            (plist-get owner :controls) (delq operation (plist-get owner :controls)))
      (hermes-dashboard-transport-cancel-owner-requests
       (plist-get owner :client) operation)
      (message "%s" text))))

(defun hermes-work--control-retire (operation)
  "Retire OPERATION, distinguishing an attempted send from an unsent request."
  (hermes-work--control-finish
   operation
   (format "Hermes: %s worker %s in session %s: %s"
           (plist-get operation :action) (plist-get operation :id)
           (plist-get operation :session)
           (if (plist-get operation :sent)
               "outcome unknown after retirement; check before retrying"
             "not sent; owner retired"))))

(defun hermes-work--settle-owner (owner)
  "Settle OWNER's retired controls and logs independently of their views."
  (dolist (operation (copy-sequence (plist-get owner :controls)))
    (unless (hermes-work--control-current-p operation)
      (hermes-work--control-retire operation)))
  (hermes-work-log--settle-owner owner))

(defun hermes-work--control (owner row method &optional text)
  "Request METHOD for OWNER's exact ROW, with optional steering TEXT.
The backend checks control generation; roster visibility never grants it."
  (let* ((operation (list :owner owner :row row :view (plist-get owner :view)
                          :id (copy-sequence (plist-get row :id))
                          :session (copy-sequence (plist-get owner :runtime))
                          :action (if text "steer" "interrupt") :sent nil :settled nil))
         (current (lambda ()
                    (and (not (plist-get operation :settled))
                         (hermes-work--action-current-p owner row)
                         (or (not text) (plist-get row :accepting-steer)))))
         ;; This predicate runs only at the transport's final send boundary,
         ;; after authentication.  A send attempt can have an uncertain outcome.
         (hermes-dashboard-transport-dispatch-guard
          (lambda () (when (funcall current) (setf (plist-get operation :sent) t))))
         (hermes-dashboard-transport-request-owner operation)
         (hermes-dashboard-transport-request-timeout 10))
    (when (funcall current)
      (push operation (plist-get owner :controls))
      (hermes--promise-catch
       (hermes--promise-then
        (apply #'hermes-dashboard-transport-call-fn method
               (plist-get owner :client) (plist-get operation :id)
               (append (and text (list text))
                       (list :session-id (plist-get operation :session))))
        (lambda (result)
          (if (not (hermes-work--control-current-p operation))
              (hermes-work--control-retire operation)
            (hermes-work--control-finish
             operation
             (if text
                 (if (equal (hermes-transport--get result 'status) "queued")
                     "Hermes: steering queued (delivery not confirmed)"
                   "Hermes: steering rejected; worker unavailable to this session")
               (if (eq (hermes-transport--get result 'found) t)
                   "Hermes: worker interrupt accepted"
                 "Hermes: worker not found or unavailable to this session"))))))
       (lambda (reason)
         (if (hermes-work--control-current-p operation)
             (hermes-work--control-finish operation (format "Hermes: %s" reason))
           (hermes-work--control-retire operation)))))))

(defun hermes-work-interrupt ()
  "Request an interrupt of the selected worker through its owning session."
  (interactive nil hermes-work-mode)
  (let ((owner hermes-work--owner) (row (hermes-work--selected-worker)))
    (when (yes-or-no-p (format "Interrupt worker %s in this session? "
                              (plist-get row :id)))
      (hermes-work--control owner row
                            #'hermes-dashboard-transport-subagent-interrupt))))

(defun hermes-work-steer ()
  "Queue steering text for the selected worker, without claiming delivery."
  (interactive nil hermes-work-mode)
  (let ((owner hermes-work--owner) (row (hermes-work--selected-worker)))
    (unless (plist-get row :accepting-steer)
      (user-error "Worker is not accepting steering"))
    (let ((text (read-string "Steer worker: ")))
      (unless (string-empty-p (string-trim text))
        (hermes-work--control owner row #'hermes-dashboard-transport-subagent-steer text)))))

(defun hermes-work-tail ()
  "Display the selected worker's bounded live tail, without inferring a path.
Resumed lineage may be visible but unavailable to this control generation."
  (interactive nil hermes-work-mode)
  (let ((owner hermes-work--owner) (row (hermes-work--selected-worker)))
    (hermes-work-log--open owner (plist-get row :id) nil row)))

;;; Worker logs

(require 'hermes-kanban-log)

(defconst hermes-work-log--max-bytes (* 2 1024 1024)
  "Maximum decoded worker log size accepted for rendering.")

(defvar-local hermes-work-log--binding nil
  "Exact owner, worker and remote path for this log buffer.")
(defvar-local hermes-work-log--request nil
  "Unique pending request token, or nil.")

(defun hermes-work--event-log-path (event id)
  "Return ID's exact log path from a structured delegate EVENT.
Reject malformed parallel arrays and ambiguous worker identities."
  (when (and (equal (plist-get event :name) "delegate_task")
             (equal (plist-get event :event) "tool.complete")
             (not (plist-get event :subagent-id)))
    (let* ((raw (or (plist-get event :result) (plist-get event :result-text)))
           (result (if (stringp raw) (cdr (hermes-transport--json-read raw)) raw))
           (ids (hermes-transport--get result 'subagent_ids))
           (paths (hermes-transport--get result 'live_transcripts)))
      (when (and (or (vectorp ids) (proper-list-p ids))
                 (or (vectorp paths) (proper-list-p paths))
                 (= (length ids) (length paths))
                 (= (seq-count (lambda (value) (equal value id)) ids) 1))
        (let ((path (elt paths (seq-position ids id #'equal))))
          (and (stringp path) (not (string-empty-p path)) path))))))

(defun hermes-work--log-path (owner id)
  "Return the unambiguous remote log path for ID in OWNER's chat entries.
Only structured parent tool results supply paths, never text or filenames."
  (when (hermes-work--current-p owner)
    (with-current-buffer (plist-get owner :buffer)
      (let ((paths (delete-dups
                    (delq nil
                          (mapcar
                           (lambda (entry)
                             (hermes-work--event-log-path
                              (plist-get (plist-get entry :metadata) :event) id))
                           (hermes-chat--entries))))))
        (and (= (length paths) 1) (car paths))))))

(defun hermes-work-log--owner-current-p (binding)
  "Return non-nil if BINDING retains its attachment and optional tail row."
  (let ((owner (plist-get binding :owner)) (row (plist-get binding :row)))
    (and (not (plist-get binding :retired)) (hermes-work--current-p owner)
         (or (plist-get binding :path)
             ;; Poll freshness does not revoke an explicitly opened snapshot.
             (and row
                  (hermes-work--row-current-p owner row))))))

(defun hermes-work-log--retire (buffer binding)
  "Settle BUFFER's exact BINDING without repainting a retired view."
  (setf (plist-get binding :retired) t)
  (hermes-dashboard-transport-cancel-owner-requests
   (plist-get (plist-get binding :owner) :client) binding)
  (when (and (buffer-live-p buffer)
             (eq binding (buffer-local-value 'hermes-work-log--binding buffer)))
    (with-current-buffer buffer
      (setq hermes-work-log--request nil)
      (when (hermes-buffer--owned-p 'hermes-work-log-mode)
        (setq header-line-format
              "Worker log · Retired · reopen from Work in the attached chat")))))

(defun hermes-work-log--settle-owner (owner)
  "Retire OWNER's obsolete log bindings, independently of visible windows."
  (dolist (buffer (buffer-list))
    (when-let* ((binding (buffer-local-value 'hermes-work-log--binding buffer))
                ((eq owner (plist-get binding :owner)))
                ((not (hermes-work-log--owner-current-p binding))))
      (hermes-work-log--retire buffer binding))))

(defun hermes-work-log--detach ()
  "Release this log's exact read on native buffer retirement."
  (when hermes-work-log--binding
    (hermes-work-log--retire (current-buffer) hermes-work-log--binding)))

(defun hermes-work-log--fetch (buffer binding token)
  "Fetch BUFFER's BINDING under exact request TOKEN through authentication."
  (let* ((owner (plist-get binding :owner))
         (client (plist-get owner :client))
         (guard (lambda () (hermes-work-log--current-p buffer binding token)))
         (hermes-dashboard-transport-dispatch-guard guard)
         (hermes-dashboard-transport-request-owner binding)
         (hermes-dashboard-transport-request-timeout 10))
    (if-let* ((path (plist-get binding :path)))
        (hermes-dashboard-transport-api-request-async
         "GET" "/api/files/read" :client client :query (list (cons 'path path))
         :timeout 30 :current-p guard)
      (hermes-dashboard-transport-call-fn
       #'hermes-dashboard-transport-subagent-tail client (plist-get binding :id)
       :session-id (plist-get owner :runtime)))))

(defun hermes-work-log--accept-tail (buffer binding token result)
  "Render bounded tail RESULT for BUFFER's exact BINDING and TOKEN."
  (when (hermes-work-log--current-p buffer binding token)
    (let ((available (eq (hermes-transport--get result 'available) t))
          (truncated (eq (hermes-transport--get result 'truncated) t))
          (text (hermes-transport--get result 'text)))
      (unless (and (equal (hermes-transport--get result 'subagent_id)
                          (plist-get binding :id)) (stringp text))
        (error "Invalid worker tail"))
      (with-current-buffer buffer
        (hermes-work-log--render
         (if available (if (string-empty-p text) "Empty tail snapshot.\n" text)
           "Tail unavailable: no log or no control authority in this session.\nUse the Work view's Published log action for historical logs.\n"))
        (setq hermes-work-log--request nil
              header-line-format
              (format "Worker tail · available: %s · truncated: %s · g Refresh"
                      (if available "yes" "no") (if truncated "yes" "no")))))))

(defun hermes-work-log--decode (result)
  "Return log text from managed-file RESULT, or signal invalid data.
The endpoint returns whole files, not a tail or a paginated transcript."
  (decode-coding-string
   (hermes-transport-file-bytes result hermes-work-log--max-bytes) 'utf-8))

(defun hermes-work-log--current-p (buffer binding token)
  "Return non-nil if BUFFER still owns BINDING and request TOKEN."
  (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
       (not (with-current-buffer buffer (hermes-buffer--retired-p)))
       (eq binding (buffer-local-value 'hermes-work-log--binding buffer))
       (eq token (buffer-local-value 'hermes-work-log--request buffer))
       (hermes-work-log--owner-current-p binding)))

(defun hermes-work-log--render (text)
  "Replace this log with rendered TEXT, preserving point and windows."
  (hermes-browser--preserve-reading-position
   (lambda ()
     (let ((inhibit-read-only t)
           (inhibit-modification-hooks t))
       (erase-buffer)
       (insert text)))))

(defun hermes-work-log--accept (buffer binding token result)
  "Render RESULT while BUFFER retains BINDING and request TOKEN."
  (when (hermes-work-log--current-p buffer binding token)
    (let ((text (hermes-kanban--render-log-content
                 (hermes-work-log--decode result))))
      ;; Rendering invokes mode hooks in temporary buffers.  Revalidate.
      (when (hermes-work-log--current-p buffer binding token)
        (with-current-buffer buffer
          (hermes-work-log--render
           (if (string-empty-p text) "No log content yet.\n" text))
          (setq hermes-work-log--request nil
                header-line-format
                (if (string-empty-p text) "Worker log · Empty snapshot · ? Help"
                  "Worker log · Snapshot; entries may be truncated · ? Help")))))))

(defun hermes-work-log-refresh ()
  "Fetch this worker's remote log or bounded tail, preserving point.
Keep the last snapshot on failure.  Only one request may be pending per view.
A changed roster row requires reopening a tail from the Work view."
  (interactive nil hermes-work-log-mode)
  (let* ((buffer (current-buffer))
         (binding hermes-work-log--binding))
    (unless (and (derived-mode-p 'hermes-work-log-mode)
                 (not (hermes-buffer--retired-p))
                 (hermes-work-log--owner-current-p binding))
      (user-error "Worker log owner detached; reopen from the attached chat"))
    (when hermes-work-log--request (user-error "Worker log refresh already pending"))
    (let ((token (list 'request)))
      (setq hermes-work-log--request token
            header-line-format "Worker log · Loading · previous snapshot retained")
      (hermes--promise-catch
       (hermes--promise-then
        (condition-case err
            (hermes-work-log--fetch buffer binding token)
          (error (hermes--promise-rejected (error-message-string err))))
        (lambda (result)
          (unless (hermes-work-log--owner-current-p binding)
            (hermes-work-log--retire buffer binding))
          (if (plist-get binding :path)
              (hermes-work-log--accept buffer binding token result)
            (hermes-work-log--accept-tail buffer binding token result))))
       (lambda (reason)
         (unless (hermes-work-log--owner-current-p binding)
           (hermes-work-log--retire buffer binding))
         (when (hermes-work-log--current-p buffer binding token)
           (with-current-buffer buffer
             (let ((reason (hermes-dashboard-transport--redact-secret
                            (format "%s" reason))))
               (setq hermes-work-log--request nil
                     header-line-format
                     (propertize "Worker log · Failed · g Retry (hover for reason)"
                                 'help-echo reason))
               (when (= (buffer-size) 0)
                 (hermes-work-log--render (concat "Worker log unavailable: " reason "\n\ng Retry\n")))
               (message "Hermes worker log: %s" reason)))))))))

(defun hermes-work-log--open (owner id path &optional row)
  "Display OWNER's worker ID using exact remote PATH, or live tail for ROW."
  (let ((existing
         (seq-find
          (lambda (buffer)
            (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                 (not (with-current-buffer buffer (hermes-buffer--retired-p)))
                 (let ((binding (buffer-local-value 'hermes-work-log--binding buffer)))
                   (and (eq owner (plist-get binding :owner))
                        (equal id (plist-get binding :id))
                        (equal path (plist-get binding :path))
                        (eq row (plist-get binding :row))))))
          (buffer-list))))
    (if existing (pop-to-buffer existing)
      (let ((buffer (generate-new-buffer (format "*Hermes Worker Log: %s*" id))))
        (with-current-buffer buffer
          (hermes-work-log-mode)
          (when (and (eq (current-buffer) buffer)
                     (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                     (not (hermes-buffer--retired-p))
                     (hermes-work--current-p owner))
            (hermes-buffer--claim 'hermes-work-log-mode)
            (setf (plist-get owner :settle) #'hermes-work--settle-owner)
            (setq hermes-work-log--binding
                  (list :owner owner :id id :path path :row row :retired nil)
                  header-line-format "Worker log · Not fetched")
            (hermes-work-log-refresh)))
        (when (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                   (not (with-current-buffer buffer (hermes-buffer--retired-p)))
                   (eq owner (plist-get (buffer-local-value 'hermes-work-log--binding buffer)
                                       :owner))
                   (hermes-work--current-p owner))
          (pop-to-buffer buffer))))))

(defun hermes-work-log ()
  "Open the selected worker's read-only remote log, or process details.
Use only the exact path published in this parent's structured tool result."
  (interactive nil hermes-work-mode)
  (let* ((owner hermes-work--owner)
         (key (tabulated-list-get-id))
         (row (seq-find (lambda (entry) (equal key (plist-get entry :key)))
                        (hermes-work--observations owner))))
    (unless row (user-error "No observed work on this line"))
    (unless (hermes-work--current-p owner) (user-error "Work owner detached"))
    (if (not (eq (plist-get row :kind) 'delegate))
        (hermes-work-details)
      (let* ((id (plist-get row :id))
             (path (hermes-work--log-path owner id)))
        (unless path
          (user-error "No authoritative worker log path; use d for observed details"))
        (hermes-work-log--open owner id path)))))

(defvar-keymap hermes-work-log-mode-map
  :parent special-mode-map
  "g" #'hermes-work-log-refresh
  "n" #'hermes-kanban-log-next-hunk
  "p" #'hermes-kanban-log-previous-hunk)

(keymap-popup-annotate hermes-work-log-mode-map
  :popup-key "?" :exit-key "C-g" :description "Worker Log Snapshot"
  :group "Diff"
  hermes-kanban-log-next-hunk "Next hunk"
  hermes-kanban-log-previous-hunk "Previous hunk"
  :group "View"
  hermes-work-log-refresh "Refresh"
  quit-window "Quit view")

(put 'hermes-work-log-mode-map-popup 'command-modes '(hermes-work-log-mode))

(define-derived-mode hermes-work-log-mode special-mode "Worker Log"
  "Read a remote worker log or live tail without visiting a local file.
The backend may truncate individual entries or expire logs.  No full-session
history guarantee is implied.  Published-path reads time out after 30 seconds;
files over 2 MiB are not rendered, although the API transfers the whole file.
Live tails time out after 10 seconds and the backend bounds them to 16 KiB."
  (setq-local truncate-lines nil)
  (add-hook 'after-set-visited-file-name-hook #'hermes-buffer--retire nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-work-log--detach t t)
  (add-hook 'kill-buffer-hook #'hermes-work-log--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-work-log--detach nil t)
  (visual-line-mode 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-log-refresh))))

(defvar-keymap hermes-work-mode-map
  :parent tabulated-list-mode-map
  "RET" #'hermes-work-log
  "d" #'hermes-work-details
  "g" #'hermes-work-refresh
  "k" #'hermes-work-interrupt
  "s" #'hermes-work-steer
  "t" #'hermes-work-tail
  "q" #'quit-window
  "i" #'hermes-work-instance-subagents
  "h" #'hermes-work-scope-details)

(keymap-popup-annotate hermes-work-mode-map
  :popup-key "?" :exit-key "C-g" :description "Observed Chat Work"
  :group "Worker"
  hermes-work-tail "Live tail"
  hermes-work-interrupt "Interrupt"
  hermes-work-steer "Steer"
  :group "History"
  hermes-work-log "Published log"
  hermes-work-details "Observed details"
  :group "Scope"
  hermes-work-scope-details "Scope details"
  hermes-work-instance-subagents "Instance workers"
  :group "View"
  hermes-work-refresh "Refresh"
  quit-window "Quit view")

(put 'hermes-work-mode-map-popup 'command-modes '(hermes-work-mode))

(define-derived-mode hermes-work-mode tabulated-list-mode "Observed Work"
  "Browse only one chat attachment's observed delegates and processes."
  (setq-local tabulated-list-padding 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-refresh)))
  (add-hook 'window-size-change-functions #'hermes-work--resize nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-work--detach t t)
  (add-hook 'kill-buffer-hook #'hermes-work--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-work--detach nil t))

;;;###autoload
(defun hermes-chat-work ()
  "Browse this chat's observed work without acquiring another client."
  (interactive nil hermes-chat-mode hermes-work-mode)
  (let ((owner (hermes-work--context-owner)))
    (unless (and (hermes-work--current-p owner)
                 (or (derived-mode-p 'hermes-chat-mode) (hermes-work--view-p owner)))
      (user-error "No attached session work owner"))
    (unless (hermes-work--view-p owner)
      (let ((buffer (generate-new-buffer "*Hermes Observed Work*")))
        (with-current-buffer buffer
          (hermes-work-mode)
          (hermes-buffer--claim 'hermes-work-mode)
          (setq hermes-work--owner owner)
          (setq-local hermes-instance (plist-get owner :instance)))
        (setf (plist-get owner :view) buffer
              (plist-get owner :view-valid-p) #'hermes-work--view-p
              (plist-get owner :render) #'hermes-work--render
              (plist-get owner :settle) #'hermes-work--settle-owner)))
    (with-current-buffer (plist-get owner :buffer)
      (add-hook 'hermes-chat-state-change-hook #'hermes-work--changed nil t))
    (pop-to-buffer (plist-get owner :view))
    (hermes-work--resize (selected-window))
    (hermes-work--render owner)))

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
