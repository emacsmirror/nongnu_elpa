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

(defun hermes-work-log--decode (result)
  "Return log text from managed-file RESULT, or signal invalid data.
The endpoint returns whole files, not a tail or a paginated transcript."
  (let ((size (hermes-transport--get result 'size))
        (data (hermes-transport--get result 'data_url)))
    (unless (and (natnump size) (<= size hermes-work-log--max-bytes)
                 (stringp data)
                 (<= (length data) (+ 256 (* 4 (/ (+ hermes-work-log--max-bytes 2) 3))))
                 (string-match "\\`data:[^,;]+;base64," data))
      (error "Worker log unavailable: invalid response or exceeds 2 MiB display limit"))
    (let ((bytes (base64-decode-string (substring data (match-end 0)))))
      (unless (= (string-bytes bytes) size)
        (error "Worker log changed during read or has an invalid size; refresh"))
      (decode-coding-string bytes 'utf-8))))

(defun hermes-work-log--current-p (buffer binding token)
  "Return non-nil if BUFFER still owns BINDING and request TOKEN."
  (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
       (eq binding (buffer-local-value 'hermes-work-log--binding buffer))
       (eq token (buffer-local-value 'hermes-work-log--request buffer))
       (hermes-work--current-p (plist-get binding :owner))))

(defun hermes-work-log--render (text)
  "Replace this log with rendered TEXT, preserving point and windows."
  (let ((position (point))
        (windows (mapcar (lambda (window) (cons window (window-start window)))
                         (get-buffer-window-list (current-buffer) nil t))))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (insert text)
      (goto-char (min position (point-max))))
    (dolist (entry windows)
      (when (window-live-p (car entry))
        (set-window-start (car entry) (min (cdr entry) (point-max)) t)))))

(defun hermes-work-log-refresh ()
  "Fetch this worker's remote log asynchronously, preserving point.
Keep the last snapshot on failure.  Only one request may be pending per view."
  (interactive)
  (let* ((buffer (current-buffer))
         (binding hermes-work-log--binding)
         (owner (plist-get binding :owner)))
    (unless (and (derived-mode-p 'hermes-work-log-mode)
                 (hermes-work--current-p owner))
      (user-error "Worker log owner detached; reopen from the attached chat"))
    (when hermes-work-log--request (user-error "Worker log refresh already pending"))
    (let ((token (list 'request)))
      (setq hermes-work-log--request token
            header-line-format "Worker log · Loading · previous snapshot retained")
      (hermes--promise-catch
       (hermes--promise-then
        (condition-case err
            (hermes-dashboard-transport-api-request-async
             "GET" "/api/files/read" :client (plist-get owner :client)
             :query (list (cons 'path (plist-get binding :path))) :timeout 30)
          (error (hermes--promise-rejected (error-message-string err))))
        (lambda (result)
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
                        (if (string-empty-p text) "Worker log · Empty snapshot · g Refresh"
                          "Worker log · Snapshot; entries may be truncated · g Refresh"))))))))
       (lambda (reason)
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

(defun hermes-work-log--open (owner id path)
  "Display the log for OWNER's worker ID using exact remote PATH."
  (let ((existing
         (seq-find
          (lambda (buffer)
            (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                 (let ((binding (buffer-local-value 'hermes-work-log--binding buffer)))
                   (and (eq owner (plist-get binding :owner))
                        (equal id (plist-get binding :id))
                        (equal path (plist-get binding :path))))))
          (buffer-list))))
    (if existing (pop-to-buffer existing)
      (let ((buffer (generate-new-buffer (format "*Hermes Worker Log: %s*" id))))
        (with-current-buffer buffer
          (hermes-work-log-mode)
          (when (and (eq (current-buffer) buffer)
                     (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                     (hermes-work--current-p owner))
            (setq hermes-work-log--binding (list :owner owner :id id :path path)
                  header-line-format "Worker log · Not fetched")
            (hermes-work-log-refresh)))
        (when (and (hermes-browser--buffer-mode-p buffer 'hermes-work-log-mode)
                   (eq owner (plist-get (buffer-local-value 'hermes-work-log--binding buffer)
                                       :owner))
                   (hermes-work--current-p owner))
          (pop-to-buffer buffer))))))

(defun hermes-work-log ()
  "Open the selected worker's read-only remote log, or process details.
Use only the exact path published in this parent's structured tool result."
  (interactive)
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

(define-derived-mode hermes-work-log-mode special-mode "Worker Log"
  "Read a remote worker log snapshot without visiting a local file.
The backend may truncate individual entries or expire logs.  No full-session
history guarantee is implied.  Requests time out after 30 seconds; files over
2 MiB are not rendered, although the API transfers the whole file."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (setq-local revert-buffer-function (lambda (&rest _) (hermes-work-log-refresh))))

(defvar-keymap hermes-work-mode-map
  :parent tabulated-list-mode-map
  "RET" #'hermes-work-log
  "d" #'hermes-work-details
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
