;;; codex-ide-diff.el --- Diff viewer for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: ai, codex, tools, diff
;; URL: https://git.thanosapollo.org/emacs-codex-ide

;; This file is not part of GNU Emacs.

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

;; Review proposals in editable Ediff buffers asynchronously with
;; `codex-ide-diff-review'.  Acceptance returns the final edited content.
;; `codex-ide-diff-preview' retains its synchronous boolean interface and
;; keeps its proposal read-only.  Both interfaces own temporary buffers
;; and restore the original layout only while it still belongs to them.
;;
;; MCP proposals always wait for `codex-ide-diff-show'; their notification
;; timers never open Ediff or change the selected frame, tab, or window.
;; The module performs no file writes; the caller owns the target path.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup codex-ide-diff nil
  "Diff viewer for codex-ide."
  :group 'codex-ide
  :prefix "codex-ide-diff-")

(defcustom codex-ide-diff-buffer-naming 'path
  "How diff temp buffers are named.
`path' derives the name from the file path (default).
`generic' uses a counter-based generic name."
  :type '(choice (const :tag "From file path" path)
                 (const :tag "Generic counter" generic))
  :group 'codex-ide-diff)

(defcustom codex-ide-diff-accept-key (kbd "C-c C-a")
  "Key sequence bound to accept in the ediff control buffer.
Must use the Control-c prefix to avoid colliding with ediff's own
single-letter navigation keys.  The default is a Control-c prefixed
accept binding."
  :type 'key-sequence
  :group 'codex-ide-diff)

;;; Variables

(defvar codex-ide-diff--counter 0
  "Counter for generic buffer naming.
Incremented each time a generic name is produced.")

(defvar codex-ide-diff--queued-review nil
  "Receipt waiting for an explicit `codex-ide-diff-show' command.")

(defface codex-ide-diff-pending
  '((t :inherit warning))
  "Face for a pending Codex review notification."
  :group 'codex-ide-diff)

(defvar codex-ide-diff--mode-line
  '(:eval (propertize " Codex review pending"
                      'face 'codex-ide-diff-pending
                      'help-echo (substitute-command-keys
                                  "Open with \\[codex-ide-diff-show]")))
  "Mode line indicator installed only while a proposal is queued.")
(put 'codex-ide-diff--mode-line 'risky-local-variable t)

(defvar-local codex-ide-diff--header-line nil
  "Literal review header, including target path and current key bindings.")
(put 'codex-ide-diff--header-line 'risky-local-variable t)

(defvar codex-ide-diff--active nil
  "Current asynchronous review owner, or nil.")

;;; Pure helpers

(defun codex-ide-diff--buffer-name (file-path side)
  "Return a temp buffer name for FILE-PATH and SIDE (`old' or `new').
Naming follows `codex-ide-diff-buffer-naming'."
  (let ((suffix (pcase side
                  ('old "[old]")
                  ('new "[new]")
                  (_ (error "Invalid side: %S" side)))))
    (pcase codex-ide-diff-buffer-naming
      ('path
       (format "*codex-ide-diff:%s%s*"
               (file-name-nondirectory (or file-path "unknown"))
               suffix))
      ('generic
       (format "*codex-ide-diff-%d%s*"
               (cl-incf codex-ide-diff--counter)
               suffix))
      (_ (error "Invalid naming policy: %S"
                codex-ide-diff-buffer-naming)))))

(defun codex-ide-diff--make-temp-buffer (name content &optional file-path)
  "Create a read-only buffer named NAME containing CONTENT.
Use FILE-PATH to choose a major mode without visiting the target file,
running mode or file hooks, or evaluating file-local variables."
  (let ((buf (generate-new-buffer name)))
    (condition-case err
        (with-current-buffer buf
          (insert (or content ""))
          (when file-path
            (let ((buffer-file-name file-path)
                  (enable-local-variables nil)
                  (enable-local-eval nil)
                  (change-major-mode-hook nil))
              (delay-mode-hooks (set-auto-mode)))
            ;; Finish core mode setup without user hooks or local variables.
            (let ((enable-local-variables nil)
                  (enable-local-eval nil))
              (cl-progv (append delayed-mode-hooks
                                '(change-major-mode-after-body-hook
                                  after-change-major-mode-hook font-lock-mode-hook))
                  nil
                (run-mode-hooks)
                (font-lock-mode 1))))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          buf)
      ((error quit)
       (kill-buffer buf)
       (signal (car err) (cdr err))))))

(defun codex-ide-diff--make-temp-buffer-pair (old-content new-content file-path)
  "Return (BUFFER-A . BUFFER-B) holding OLD-CONTENT and NEW-CONTENT.
Use FILE-PATH for buffer naming and major mode selection."
  (let ((a (codex-ide-diff--make-temp-buffer
            (codex-ide-diff--buffer-name file-path 'old) old-content file-path)))
    (condition-case err
        (cons a (codex-ide-diff--make-temp-buffer
                 (codex-ide-diff--buffer-name file-path 'new) new-content file-path))
      ((error quit) (kill-buffer a) (signal (car err) (cdr err))))))

;;; Review lifecycle

(declare-function ediff-buffers "ediff")
(declare-function ediff-really-quit "ediff-util")
(declare-function ediff-cleanup-mess "ediff-util")
(declare-function ediff-update-registry "ediff-mult")
(defvar ediff-session-registry)
(defvar ediff-registry-buffer)
(declare-function ediff-setup-windows-plain "ediff-wind")
(defvar ediff-keep-variants)
(declare-function ediff-delete-temp-files "ediff-util")
(declare-function tab-bar--current-tab-find "tab-bar")
(defvar ediff-mode-hook)
(defvar ediff-window-setup-function)
(defvar ediff-after-quit-hook-internal)
(defvar ediff-quit-hook)
(defvar ediff-cleanup-hook)
(defvar ediff-grab-mouse)
(defvar tab-bar-closed-tabs)

(defun codex-ide-diff--window-config ()
  "Capture the layout independently of a process filter's current buffer."
  (with-current-buffer (window-buffer (selected-window))
    (current-window-configuration)))

(defun codex-ide-diff--dispose (owner)
  "Release buffers belonging to OWNER without selecting windows."
  (let ((control (plist-get owner :control)))
    (when (buffer-live-p control)
      (setq ediff-session-registry (delq control ediff-session-registry))
      (when (buffer-live-p ediff-registry-buffer) (ediff-update-registry))
      (with-current-buffer control
        (ignore-errors (ediff-delete-temp-files))
        (dolist (symbol '(ediff-diff-buffer ediff-custom-diff-buffer
                                            ediff-fine-diff-buffer ediff-tmp-buffer
                                            ediff-error-buffer ediff-msg-buffer ediff-debug-buffer))
          (when-let* ((value (and (boundp symbol) (symbol-value symbol)))
                      (buffer (get-buffer value)))
            (kill-buffer buffer))))))
  (dolist (key '(:control :buffer-a :buffer-b))
    (let ((buffer (plist-get owner key)))
      (when (and (buffer-live-p buffer)
                 (not (eq buffer (plist-get owner :dying))))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-dedicated-p window nil)
          (set-window-parameter window 'quit-restore nil))
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil))
            (set-buffer-modified-p nil)
            (kill-buffer buffer)))))))

(defun codex-ide-diff--forget-tab (owner)
  "Remove OWNER's transient marker from live and closed tabs."
  (dolist (tab (append (apply #'append
                              (mapcar (lambda (frame) (frame-parameter frame 'tabs))
                                      (frame-list)))
                       (mapcar (lambda (closed) (alist-get 'tab closed))
                               tab-bar-closed-tabs)))
    (when (eq (alist-get 'codex-ide-diff-owner tab) (plist-get owner :tab))
      (setcdr tab (assq-delete-all 'codex-ide-diff-owner (cdr tab))))))

(defun codex-ide-diff--settle (owner status)
  "Complete pending OWNER exactly once with STATUS."
  (when (and (eq owner codex-ide-diff--active)
             (eq (plist-get owner :status) 'pending))
    (let ((content (when (eq status 'accepted)
                     (with-current-buffer (plist-get owner :buffer-b)
                       (save-restriction
                         (widen)
                         (buffer-substring-no-properties (point-min) (point-max))))))
          (restore (plist-get owner :restore)))
      (setf (plist-get owner :status) status)
      (unwind-protect
          (codex-ide-diff--dispose owner)
        (when restore
          (set-window-configuration (plist-get owner :saved-layout)))
        (codex-ide-diff--forget-tab owner)
        (setq codex-ide-diff--active nil)
        (if (plist-get owner :dying)
            ;; The outer kill must finish before consumers observe cleanup.
            (setf (plist-get owner :timer)
                  (run-at-time 0 nil
                               (lambda ()
                                 (setf (plist-get owner :timer) nil)
                                 (funcall (plist-get owner :callback) status content))))
          (funcall (plist-get owner :callback) status content))))))

(defun codex-ide-diff--close (owner status)
  "Quit OWNER's Ediff session, then settle it with STATUS."
  (when (and (eq owner codex-ide-diff--active)
             (eq (plist-get owner :status) 'pending)
             (not (plist-get owner :closing)))
    (setf (plist-get owner :closing) t
          (plist-get owner :restore)
          (and (eq (selected-frame) (plist-get owner :frame))
               (eq (alist-get 'codex-ide-diff-owner (tab-bar--current-tab-find))
                   (plist-get owner :tab))
               (plist-get owner :layout)
               (window-configuration-equal-p
                (plist-get owner :layout) (codex-ide-diff--window-config))))
    (unwind-protect
        (condition-case nil
            (when (and (plist-get owner :ready)
                       (buffer-live-p (plist-get owner :control)))
              ;; Native Ediff teardown recenters before running its quit hooks.
              ;; Preserve the current layout until owner cleanup decides whether
              ;; the original layout still belongs to this review.
              (with-selected-frame (if (frame-live-p (plist-get owner :frame))
                                       (plist-get owner :frame)
                                     (selected-frame))
                (save-window-excursion
                  (with-current-buffer (plist-get owner :control)
                    (let ((ediff-grab-mouse nil)
                          (ediff-keep-variants t))
                      (ediff-really-quit nil))))))
          ((error quit) (setq status 'cancelled)))
      (codex-ide-diff--settle owner status))))

(defun codex-ide-diff-cancel (owner)
  "Cancel the review represented by OWNER.
Do nothing if OWNER has already completed or belongs to an older review."
  (codex-ide-diff--close owner 'cancelled))

(defun codex-ide-diff--killed (owner)
  "Cancel OWNER when one of its buffers is killed."
  (unless (or (plist-get owner :closing)
              (not (eq (plist-get owner :status) 'pending)))
    (setf (plist-get owner :dying) (current-buffer))
    (codex-ide-diff-cancel owner)))

(defun codex-ide-diff--prepare-control (owner)
  "Capture OWNER's control buffer before Ediff setup can fail."
  (when (eq owner codex-ide-diff--active)
    (setf (plist-get owner :control) (current-buffer))
    ;; Core defaults share these names between otherwise independent sessions.
    (dolist (symbol '(ediff-tmp-buffer ediff-msg-buffer ediff-debug-buffer))
      (set (make-local-variable symbol)
           (generate-new-buffer-name (format " *codex-%s*" symbol))))
    (setq-local ediff-quit-hook
                (list (lambda ()
                        (unless (eq (current-buffer) (plist-get owner :dying))
                          (ediff-cleanup-mess))))
                ediff-cleanup-hook nil
                ediff-after-quit-hook-internal nil)
    (add-hook 'kill-buffer-hook
              (lambda () (codex-ide-diff--killed owner)) nil t)))

(defun codex-ide-diff-accept ()
  "Accept the current Codex Ediff proposal after confirmation."
  (interactive)
  (when (and codex-ide-diff--active
             (eq (current-buffer) (plist-get codex-ide-diff--active :control))
             (y-or-n-p "Accept this diff? "))
    (codex-ide-diff--close codex-ide-diff--active 'accepted)))

(defun codex-ide-diff-reject ()
  "Reject the current Codex Ediff proposal after confirmation."
  (interactive)
  (when (and codex-ide-diff--active
             (eq (current-buffer) (plist-get codex-ide-diff--active :control))
             (y-or-n-p "Reject this diff? "))
    (codex-ide-diff--close codex-ide-diff--active 'rejected)))

(defun codex-ide-diff--prepare-keys (owner read-only)
  "Install OWNER's review commands and honor READ-ONLY compatibility mode."
  (setf (plist-get owner :ready) t)
  (with-current-buffer (plist-get owner :buffer-b)
    (setq buffer-read-only read-only))
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "q" #'codex-ide-diff-reject)
  (local-set-key codex-ide-diff-accept-key #'codex-ide-diff-accept)
  (let ((header
         (format "%s | %s accept | %s reject | %s help"
                 (file-relative-name (plist-get owner :path) (plist-get owner :root))
                 (propertize (substitute-command-keys "\\[codex-ide-diff-accept]")
                             'face 'help-key-binding)
                 (propertize (substitute-command-keys "\\[codex-ide-diff-reject]")
                             'face 'help-key-binding)
                 (propertize (substitute-command-keys "\\[ediff-toggle-help]")
                             'face 'help-key-binding))))
    (dolist (buffer (list (current-buffer) (plist-get owner :buffer-a)
                          (plist-get owner :buffer-b)))
      (with-current-buffer buffer
        (setq codex-ide-diff--header-line header
              header-line-format 'codex-ide-diff--header-line)))))

;;; Public entry points

;;;###autoload
(defun codex-ide-diff-review (old-content new-content file-path callback
                                          &optional read-only)
  "Review OLD-CONTENT and editable NEW-CONTENT for FILE-PATH asynchronously.
Return an opaque owner for `codex-ide-diff-cancel'.  Call CALLBACK once
with (STATUS CONTENT): `accepted' and the exact edited proposal string,
or `rejected'/`cancelled' and nil.  Use FILE-PATH for a label and major mode
selection only; no target files or visiting buffers are modified.  Signal
`user-error' when another review is active.  Setup errors cancel and are
re-signaled.
Non-nil READ-ONLY keeps the proposal read-only for boolean previews."
  (when (or codex-ide-diff--active codex-ide-diff--queued-review) (user-error "A Codex diff review is already active"))
  (unless (and (stringp old-content) (stringp new-content)
               (stringp file-path) (functionp callback))
    (error "Expected content strings, a file label, and a callback"))
  (require 'ediff)
  (require 'tab-bar)
  (let* ((owner (list :control nil :buffer-a nil :buffer-b nil :ready nil
                      :closing nil :dying nil :layout nil :restore nil
                      :timer nil :path file-path :root default-directory
                      :status 'pending :callback callback
                      :frame (selected-frame) :tab (make-symbol "review-tab")
                      :saved-layout (codex-ide-diff--window-config)))
         (ediff-window-setup-function #'ediff-setup-windows-plain)
         (ediff-mode-hook (cons (lambda () (codex-ide-diff--prepare-control owner))
                                ediff-mode-hook)))
    (setf (alist-get 'codex-ide-diff-owner (cdr (tab-bar--current-tab-find)))
          (plist-get owner :tab))
    (setq codex-ide-diff--active owner)
    (condition-case err
        (progn
          (setf (plist-get owner :buffer-a)
                (codex-ide-diff--make-temp-buffer
                 (codex-ide-diff--buffer-name file-path 'old) old-content file-path)
                (plist-get owner :buffer-b)
                (codex-ide-diff--make-temp-buffer
                 (codex-ide-diff--buffer-name file-path 'new) new-content file-path))
          (dolist (key '(:buffer-a :buffer-b))
            (with-current-buffer (plist-get owner key)
              (add-hook 'kill-buffer-hook
                        (lambda () (codex-ide-diff--killed owner)) nil t)))
          (ediff-buffers (plist-get owner :buffer-a) (plist-get owner :buffer-b)
                         (list (lambda ()
                                 (codex-ide-diff--prepare-keys owner read-only))))
          (setf (plist-get owner :layout) (codex-ide-diff--window-config))
          owner)
      ((error quit)
       (codex-ide-diff-cancel owner)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun codex-ide-diff-preview (old-content new-content file-path)
  "Preview OLD-CONTENT and NEW-CONTENT for FILE-PATH, returning a boolean.
Both temporary buffers are read-only.  Return t on acceptance, nil on
rejection or cancellation.  Never write the target file."
  (let (decision waiting)
    (let ((owner (codex-ide-diff-review
                  old-content new-content file-path
                  (lambda (status _content)
                    (setq decision status)
                    (when waiting (ignore-errors (exit-recursive-edit)))) t)))
      (unwind-protect
          (unless decision
            (setq waiting t)
            (recursive-edit))
        (setq waiting nil)
        (codex-ide-diff-cancel owner)))
    (eq decision 'accepted)))

;;; Review receipts

(declare-function codex-ide--session-by-buffer "codex-ide")
(declare-function codex-ide--session-by-id "codex-ide")
(declare-function codex-ide--session-live-p "codex-ide")
(defvar codex-ide-mode)
(defvar codex-ide--session-root)
(defvar codex-ide--session-id)

(defvar codex-ide-diff--receipts (make-hash-table :test 'equal)
  "Bounded review receipts retained independently of HTTP connections.")
(defvar codex-ide-diff--receipt-instance
  (format "%x-%s" (emacs-pid) (format-time-string "%s%N"))
  "Instance namespace preventing receipt ID reuse after Emacs restarts.")
(defvar codex-ide-diff--next-receipt 0
  "Monotonic receipt identifier counter.")
(defconst codex-ide-diff--receipt-limit 16
  "Maximum pending and completed receipts retained together.")
(defconst codex-ide-diff--receipt-lifetime 1800
  "Seconds allowed for a pending review and for retaining its final result.")

(defun codex-ide-diff--request-session (buffer)
  "Return BUFFER's exact registered live terminal identity, or nil."
  (when (and (buffer-live-p buffer)
             (fboundp 'codex-ide--session-by-buffer))
    (let ((session (codex-ide--session-by-buffer buffer)))
      (when (and session
                 (eq session (codex-ide--session-by-id
                              (plist-get session :root) (plist-get session :id)))
                 (codex-ide--session-live-p session)
                 (buffer-local-value 'codex-ide-mode buffer)
                 (equal (plist-get session :root)
                        (buffer-local-value 'codex-ide--session-root buffer))
                 (eql (plist-get session :id)
                      (buffer-local-value 'codex-ide--session-id buffer)))
        (list :root (copy-sequence (plist-get session :root))
              :id (plist-get session :id) :buffer buffer
              :process (plist-get session :process))))))

(defun codex-ide-diff--request-current-p (receipt)
  "Return non-nil when RECEIPT remains pending in the registry."
  (and (eq receipt (gethash (plist-get receipt :id) codex-ide-diff--receipts))
       (eq (plist-get receipt :status) 'pending)))

(defun codex-ide-diff--request-owner-live-p (receipt)
  "Return non-nil while RECEIPT's captured terminal identity is current."
  (let ((session (plist-get receipt :session)))
    (equal session (codex-ide-diff--request-session (plist-get session :buffer)))))

(defun codex-ide-diff--request-summary (receipt)
  "Return a fresh result object for RECEIPT."
  (append (list (cons "review_id" (copy-sequence (plist-get receipt :id)))
                (cons "status" (copy-sequence (symbol-name (plist-get receipt :status)))))
          (when (eq (plist-get receipt :status) 'accepted)
            (list (cons "content" (copy-sequence (plist-get receipt :content)))))))

(defun codex-ide-diff--finish-request (receipt status &optional content)
  "Settle pending RECEIPT with STATUS and accepted CONTENT exactly once."
  (when (codex-ide-diff--request-current-p receipt)
    (setq status (cond ((>= (float-time) (plist-get receipt :deadline)) 'expired)
                       ((not (codex-ide-diff--request-owner-live-p receipt)) 'cancelled)
                       (t status)))
    (setf (plist-get receipt :status) status
          (plist-get receipt :content) (when (eq status 'accepted) (copy-sequence content))
          (plist-get receipt :deadline) (+ (float-time) codex-ide-diff--receipt-lifetime))
    (dolist (key '(:timer :start-timer))
      (when-let* ((timer (plist-get receipt key))) (cancel-timer timer))
      (setf (plist-get receipt key) nil))
    (when (eq receipt codex-ide-diff--queued-review)
      (setq codex-ide-diff--queued-review nil)
      (setq global-mode-string (delq 'codex-ide-diff--mode-line global-mode-string))
      (force-mode-line-update t))
    (setf (plist-get receipt :timer)
          (run-at-time codex-ide-diff--receipt-lifetime nil
                       #'codex-ide-diff--expire-request receipt (plist-get receipt :deadline)))
    (when-let* ((owner (plist-get receipt :ui-owner)))
      (setf (plist-get receipt :ui-owner) nil)
      (codex-ide-diff-cancel owner))))

(defun codex-ide-diff--expire-request (receipt deadline)
  "Expire or purge RECEIPT if its current DEADLINE has elapsed."
  (when (and (eq receipt (gethash (plist-get receipt :id) codex-ide-diff--receipts))
             (= deadline (plist-get receipt :deadline)))
    (cond
     ((>= (float-time) deadline)
      (if (eq (plist-get receipt :status) 'pending)
          (codex-ide-diff--finish-request receipt 'expired)
        (when-let* ((timer (plist-get receipt :timer))) (cancel-timer timer))
        (remhash (plist-get receipt :id) codex-ide-diff--receipts)))
     ((and (eq (plist-get receipt :status) 'pending)
           (not (codex-ide-diff--request-owner-live-p receipt)))
      (codex-ide-diff--finish-request receipt 'cancelled)))))

(defun codex-ide-diff--refresh-requests ()
  "Reconcile expired receipts and stale pending terminal owners."
  (maphash (lambda (_id receipt)
             (codex-ide-diff--expire-request receipt (plist-get receipt :deadline)))
           codex-ide-diff--receipts))

(defun codex-ide-diff--cancel-requests (&optional buffer process)
  "Cancel pending reviews for exact BUFFER and PROCESS, or all if BUFFER is nil."
  (maphash (lambda (_id receipt)
             (let ((session (plist-get receipt :session)))
               (when (or (null buffer)
                         (and (eq buffer (plist-get session :buffer))
                              (eq process (plist-get session :process))))
                 (codex-ide-diff--finish-request receipt 'cancelled))))
           codex-ide-diff--receipts))

(defun codex-ide-diff--notify-request (receipt)
  "Notify about queued RECEIPT without changing the user's windows."
  (when (codex-ide-diff--request-current-p receipt)
    (codex-ide-diff--refresh-requests)
    (when (eq receipt codex-ide-diff--queued-review)
      (when-let* ((timer (plist-get receipt :start-timer))) (cancel-timer timer))
      (setf (plist-get receipt :start-timer) nil)
      (message "Codex review pending: %s; open with %s"
               (car (plist-get receipt :input))
               (substitute-command-keys "\\[codex-ide-diff-show]")))))

(defun codex-ide-diff--run-request (receipt)
  "Open queued RECEIPT's Ediff UI on explicit user request."
  (codex-ide-diff--refresh-requests)
  (when (and (codex-ide-diff--request-current-p receipt)
             (eq receipt codex-ide-diff--queued-review))
    (when-let* ((timer (plist-get receipt :start-timer))) (cancel-timer timer))
    (setf (plist-get receipt :start-timer) nil)
    (setq codex-ide-diff--queued-review nil
          global-mode-string (delq 'codex-ide-diff--mode-line global-mode-string))
    (force-mode-line-update t)
    (condition-case nil
        (let* ((input (plist-get receipt :input))
               (default-directory (file-name-as-directory
                                   (plist-get (plist-get receipt :session) :root)))
               (owner (codex-ide-diff-review
                       (nth 1 input) (nth 2 input) (car input)
                       (lambda (status content)
                         (codex-ide-diff--finish-request receipt status content)))))
          (if (codex-ide-diff--request-current-p receipt)
              (setf (plist-get receipt :ui-owner) owner)
            ;; A startup callback can settle before the owner is returned.
            (codex-ide-diff-cancel owner)))
      ((error quit) (codex-ide-diff--finish-request receipt 'cancelled)))))

;;;###autoload
(defun codex-ide-diff-show ()
  "Open the pending Codex proposal in the current frame and tab.
MCP proposals always wait for this command, including when their owning
session is selected.  Notification timers never open a review."
  (interactive)
  (when (active-minibuffer-window)
    (user-error "Finish minibuffer input before opening a Codex review"))
  (codex-ide-diff--refresh-requests)
  (unless codex-ide-diff--queued-review
    (user-error "No Codex review is pending"))
  (codex-ide-diff--run-request codex-ide-diff--queued-review))

(defun codex-ide-diff--request-start (buffer token path old new)
  "Queue a review for terminal BUFFER using TOKEN, PATH, OLD and NEW text."
  (unless (and (stringp buffer) (stringp token) (not (string-empty-p token))
               (stringp path) (stringp old) (stringp new))
    (user-error "Expected a terminal buffer, nonempty token, path and text strings"))
  (when (> (+ (string-bytes (encode-coding-string old 'utf-8))
              (string-bytes (encode-coding-string new 'utf-8))) (* 1024 1024))
    (user-error "Review text exceeds one MiB of UTF-8 input"))
  (codex-ide-diff--refresh-requests)
  (let* ((session (or (codex-ide-diff--request-session (get-buffer buffer))
                      (user-error "Buffer does not own a registered live Codex terminal")))
         (input (list path old new))
         (duplicate (cl-find-if
                     (lambda (receipt)
                       (and (equal session (plist-get receipt :session))
                            (equal token (plist-get receipt :token))))
                     (hash-table-values codex-ide-diff--receipts))))
    (if duplicate
        (progn
          (unless (equal input (plist-get duplicate :input))
            (user-error "Review token was already used with different input"))
          (codex-ide-diff--request-summary duplicate))
      (when (or codex-ide-diff--active codex-ide-diff--queued-review)
        (user-error "A Codex diff review is already queued or active"))
      (when (>= (hash-table-count codex-ide-diff--receipts) codex-ide-diff--receipt-limit)
        (user-error "Review receipt capacity is full; wait for retention to expire"))
      (let ((receipt (list :id (format "review-%s-%d" codex-ide-diff--receipt-instance
                                       (cl-incf codex-ide-diff--next-receipt))
                           :session session :token (copy-sequence token)
                           :input (mapcar #'copy-sequence input)
                           :status 'pending :content nil :ui-owner nil
                           :start-timer nil :timer nil
                           :deadline (+ (float-time) codex-ide-diff--receipt-lifetime))))
        (puthash (plist-get receipt :id) receipt codex-ide-diff--receipts)
        (setq codex-ide-diff--queued-review receipt)
        (unless (memq 'codex-ide-diff--mode-line global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '("")) '(codex-ide-diff--mode-line))))
        (force-mode-line-update t)
        (setf (plist-get receipt :timer)
              (run-at-time codex-ide-diff--receipt-lifetime nil
                           #'codex-ide-diff--expire-request receipt (plist-get receipt :deadline))
              (plist-get receipt :start-timer)
              (run-at-time 0 nil #'codex-ide-diff--notify-request receipt))
        (codex-ide-diff--request-summary receipt)))))

(defun codex-ide-diff--request-result (id &optional cancel)
  "Return retained review ID's result; non-nil CANCEL cancels pending review."
  (codex-ide-diff--refresh-requests)
  (let ((receipt (or (gethash id codex-ide-diff--receipts)
                     (user-error "Unknown or expired review ID: %s" id))))
    (when cancel (codex-ide-diff--finish-request receipt 'cancelled))
    (codex-ide-diff--request-summary receipt)))

(provide 'codex-ide-diff)

;;; codex-ide-diff.el ends here
