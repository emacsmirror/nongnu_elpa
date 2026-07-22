;;; codex-ide-diff.el --- Diff viewer for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
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

;; Standalone diff-preview module for `codex-ide'.  Given old and new
;; file content, it builds read-only temp buffers, runs an ediff
;; session, lets the user accept or reject the change, then cleans up
;; temp buffers and restores the window configuration.  The public
;; entry point `codex-ide-diff-preview' returns non-nil if the change
;; was accepted and nil if rejected.
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

(defvar codex-ide-diff--decision nil
  "Bound dynamically around an ediff session to capture the decision.
Set to `accepted' or `rejected'; defaults to `rejected'.")

(defvar codex-ide-diff--run-ediff-function nil
  "Override for `codex-ide-diff--run-ediff' in tests.
When non-nil, called instead of the real ediff session.")

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

(defun codex-ide-diff--build-state (tab-name buffer-A buffer-B
                                      file-exists saved-winconf
                                      &optional old-content)
  "Return a diff state alist for the given components.
TAB-NAME is the session label.  BUFFER-A and BUFFER-B are live temp
buffers.  FILE-EXISTS is non-nil if the target file exists on disk.
SAVED-WINCONF is the window configuration to restore on cleanup.
OLD-CONTENT, when non-nil, is the original content snapshot."
  (let ((state `((tab-name . ,tab-name)
                 (buffer-A . ,buffer-A)
                 (buffer-B . ,buffer-B)
                 (file-exists . ,file-exists)
                 (saved-winconf . ,saved-winconf))))
    (when old-content
      (setq state (append state `((old-content . ,old-content)))))
    state))

;;; Boundary layer

;; Forward declarations: ediff is required lazily inside
;; `codex-ide-diff--run-ediff' so loading this module stays light.
(declare-function ediff-buffers "ediff")
(declare-function ediff-really-quit "ediff-util")
(defvar ediff-after-quit-hook-internal)

(defun codex-ide-diff--save-window-config ()
  "Capture and return the current window configuration."
  (current-window-configuration))

(defun codex-ide-diff--run-ediff (buffer-A buffer-B _tab-name)
  "Run an ediff session between BUFFER-A and BUFFER-B.
Blocks via `recursive-edit' until the user quits ediff.  Returns
`accepted' or `rejected' based on which key the user pressed to exit.
TAB-NAME is accepted for API symmetry with the stub override but is
currently unused; ediff shows buffer names in its own UI."
  (require 'ediff)
  (let ((codex-ide-diff--decision 'rejected)
        ;; Defined here so both the accept key and the quit hook can
        ;; close the recursive-edit.  Ediff is already required above,
        ;; so `ediff-really-quit' and `exit-recursive-edit' are
        ;; resolved.
        (accept-fn
         (lambda ()
           "Confirm accepting this diff, then quit ediff."
           (interactive)
           (when (y-or-n-p "Accept this diff? ")
             (setq codex-ide-diff--decision 'accepted)
             ;; `ediff-quit' asks an additional cancelable quit
             ;; question.  We already confirmed accept above, so use
             ;; the underlying quit path and set the decision only for
             ;; this no-cancel branch.
             (ediff-really-quit nil))))
        (exit-fn
         (lambda ()
           (ignore-errors (exit-recursive-edit)))))
    (ediff-buffers
     buffer-A buffer-B
     (list (lambda ()
             ;; Startup hook runs in the ediff control buffer.  Install
             ;; the recursive-edit exit into ediff's after-quit list so
             ;; ediff cleanup finishes before we return to the caller.
             (setq-local ediff-after-quit-hook-internal
                         (append ediff-after-quit-hook-internal
                                 (list exit-fn)))
             ;; C-c prefix is safe: ediff binds only bare keys.
             (local-set-key codex-ide-diff-accept-key accept-fn))))
    ;; ediff-buffers returns immediately after setup; block until the
    ;; user quits (which fires exit-fn via ediff's after-quit hook).
    (ignore-errors (recursive-edit))
    codex-ide-diff--decision))

(defun codex-ide-diff--preview (state)
  "Preview the diff in STATE via ediff and prompt for accept/reject.
Returns t if the change is accepted, nil if rejected.  Does NOT kill
temp buffers; cleanup is the caller's responsibility."
  (let ((buffer-A (cdr (assq 'buffer-A state)))
        (buffer-B (cdr (assq 'buffer-B state)))
        (tab-name (cdr (assq 'tab-name state))))
    (let ((decision
           (if codex-ide-diff--run-ediff-function
               (funcall codex-ide-diff--run-ediff-function
                        buffer-A buffer-B tab-name)
             (codex-ide-diff--run-ediff buffer-A buffer-B tab-name))))
      (eq decision 'accepted))))

(defun codex-ide-diff--cleanup (state)
  "Kill temp buffers in STATE and restore the saved window configuration.
Safe to call multiple times; checks buffer liveness before killing."
  (let ((buffer-A (cdr (assq 'buffer-A state)))
        (buffer-B (cdr (assq 'buffer-B state)))
        (winconf (cdr (assq 'saved-winconf state))))
    (when (buffer-live-p buffer-A)
      (kill-buffer buffer-A))
    (when (buffer-live-p buffer-B)
      (kill-buffer buffer-B))
    (when (window-configuration-p winconf)
      (set-window-configuration winconf))))

;;; Public entry point

;;;###autoload
(defun codex-ide-diff-preview (old-content new-content file-path)
  "Preview a diff between OLD-CONTENT and NEW-CONTENT for FILE-PATH.
Builds temp buffers, runs ediff, prompts for accept/reject, and cleans
up.  Returns t if accepted, nil if rejected."
  (interactive
   (list (read-string "Old content: ")
         (read-string "New content: ")
         (read-file-name "File: ")))
  (let* ((saved-winconf (codex-ide-diff--save-window-config))
         (file-exists (file-exists-p file-path))
         (pair (codex-ide-diff--make-temp-buffer-pair
                old-content new-content file-path))
         (buffer-A (car pair))
         (buffer-B (cdr pair))
         (tab-name (format "*codex-ide-diff:%s*"
                           (file-name-nondirectory
                            (or file-path "unknown"))))
         (state (codex-ide-diff--build-state
                 tab-name buffer-A buffer-B file-exists
                 saved-winconf old-content)))
    (unwind-protect
        (codex-ide-diff--preview state)
      (codex-ide-diff--cleanup state))))

(provide 'codex-ide-diff)

;;; codex-ide-diff.el ends here
