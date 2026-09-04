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

(defun codex-ide-diff--make-temp-buffer (name content)
  "Create a temp buffer named NAME, insert CONTENT, return the buffer.
The buffer is unmodified and read-only."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (insert (or content ""))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    buf))

(defun codex-ide-diff--make-temp-buffer-pair (old-content new-content file-path)
  "Return (BUFFER-A . BUFFER-B) holding OLD-CONTENT and NEW-CONTENT.
FILE-PATH is used only for buffer naming under the `path' policy."
  (let ((name-a (codex-ide-diff--buffer-name file-path 'old))
        (name-b (codex-ide-diff--buffer-name file-path 'new)))
    (cons (codex-ide-diff--make-temp-buffer name-a old-content)
          (codex-ide-diff--make-temp-buffer name-b new-content))))

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

(defun codex-ide-diff--prepare-keys (owner read-only)
  "Install OWNER's accept command and honor READ-ONLY compatibility mode."
  (setf (plist-get owner :ready) t)
  (with-current-buffer (plist-get owner :buffer-b)
    (setq buffer-read-only read-only))
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "q" (lambda ()
                       (interactive)
                       (when (y-or-n-p "Reject this diff? ")
                         (codex-ide-diff--close owner 'rejected))))
  (local-set-key
   codex-ide-diff-accept-key
   (lambda ()
     (interactive)
     (when (and (eq owner codex-ide-diff--active)
                (y-or-n-p "Accept this diff? "))
       (codex-ide-diff--close owner 'accepted)))))

;;; Public entry points

;;;###autoload
(defun codex-ide-diff-review (old-content new-content file-path callback
                                          &optional read-only)
  "Review OLD-CONTENT and editable NEW-CONTENT for FILE-PATH asynchronously.
Return an opaque owner for `codex-ide-diff-cancel'.  Call CALLBACK once
with (STATUS CONTENT): `accepted' and the exact edited proposal string,
or `rejected'/`cancelled' and nil.  FILE-PATH is only a display label;
no target files or visiting buffers are modified.  Signal `user-error'
when another review is active.  Setup errors cancel and are re-signaled.
Non-nil READ-ONLY keeps the proposal read-only for boolean previews."
  (when codex-ide-diff--active (user-error "A Codex diff review is already active"))
  (unless (and (stringp old-content) (stringp new-content)
               (stringp file-path) (functionp callback))
    (error "Expected content strings, a file label, and a callback"))
  (require 'ediff)
  (require 'tab-bar)
  (let* ((owner (list :control nil :buffer-a nil :buffer-b nil :ready nil
                      :closing nil :dying nil :layout nil :restore nil
                      :timer nil
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
                 (codex-ide-diff--buffer-name file-path 'old) old-content)
                (plist-get owner :buffer-b)
                (codex-ide-diff--make-temp-buffer
                 (codex-ide-diff--buffer-name file-path 'new) new-content))
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
  (interactive
   (list (read-string "Old content: ")
         (read-string "New content: ")
         (read-file-name "File: ")))
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

(provide 'codex-ide-diff)

;;; codex-ide-diff.el ends here
