;;; hermes-buffer.el --- Owned named buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Display names are not ownership.  Keep a local claim on buffers created
;; here, and retire it on mode changes or file association.  This leaf library
;; has no transport or UI dependencies.

;;; Code:

(require 'seq)

(defvar-local hermes-buffer--owner nil
  "View claim (KEY . MODE), nil if unclaimed, or `retired'.
KEY is a logical name for reusable views, or t for unique allocations.")

(defun hermes-buffer--retire ()
  "Retire this view after changing its visited file, even if detached later."
  (setq hermes-buffer--owner 'retired))

(defun hermes-buffer--retired-p ()
  "Return non-nil if the current buffer has been reassociated with a file."
  (or buffer-file-name (eq hermes-buffer--owner 'retired)))

(defun hermes-buffer--owned-p (&optional mode)
  "Return non-nil if this buffer retains a live claim, optionally in MODE."
  (and (consp hermes-buffer--owner)
       (eq major-mode (cdr hermes-buffer--owner))
       (or (null mode) (eq major-mode mode))
       (not buffer-file-name)))

(defvar warm-restart-regenerable)
(defvar warm-restart-blocker-functions)

(defconst hermes-buffer--editor-modes
  '(hermes-kanban-body-mode hermes-profiles-soul-mode hermes-skill-content-mode)
  "Views whose text the user edits and saves back to Hermes.")

(defconst hermes-buffer--draft-modes '(hermes-endpoint-edit-mode)
  "Views holding an unsaved draft outside their text.")

(defun hermes-buffer--warm-restart-blocker ()
  "Return why this editor cannot be handed to another Emacs now, or nil."
  (cond ((memq major-mode hermes-buffer--draft-modes)
         "unsaved Hermes draft; save or quit it first")
        ((buffer-modified-p)
         (format "unsaved %s edit; save or discard it first" major-mode))))

(defun hermes-buffer--claim (mode &optional name)
  "Claim a newly initialized view in MODE, optionally reusable by NAME.
Call only from a constructor, never from a major-mode body."
  (unless (and (eq major-mode mode) (not (hermes-buffer--retired-p)))
    (user-error "Hermes view changed during mode initialization"))
  (setq hermes-buffer--owner (cons (or name t) mode))
  ;; A warm restart lists views rebuilt from live state instead of
  ;; carrying them.  The mark is per buffer: MODE may be a generic mode
  ;; such as `diff-mode' that the user also uses.  Editors block while
  ;; they hold unsaved work.
  (cond ((memq mode (append hermes-buffer--editor-modes hermes-buffer--draft-modes))
         (add-hook 'warm-restart-blocker-functions #'hermes-buffer--warm-restart-blocker nil t)
         (setq-local warm-restart-regenerable
                     "Hermes editor without unsaved edits; reopen it from Hermes"))
        ((not (local-variable-p 'desktop-save-buffer))
         (setq-local warm-restart-regenerable "Hermes view; reopen it from Hermes")))
  (add-hook 'after-set-visited-file-name-hook #'hermes-buffer--retire nil t))

(defun hermes-buffer--find (name mode)
  "Return the buffer still owned by the named view NAME in MODE, or nil."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (not buffer-file-name)
            (eq major-mode mode)
            (equal hermes-buffer--owner (cons name mode)))))
   (buffer-list)))

(defun hermes-buffer--get (name mode &optional reset)
  "Return an owned view for NAME in MODE, creating it if necessary.
Reuse only an explicit claim in the expected mode, never a visited file.
RESET reinitializes MODE even on reuse, for views that replace local state.
A mode change clears the claim; file association permanently retires it."
  (let* ((delay delay-mode-hooks)
         (owned (hermes-buffer--find name mode))
         (buffer (or owned (generate-new-buffer name))))
    (with-current-buffer buffer
      (when (or reset (not owned))
        (if delay (delay-mode-hooks (funcall mode)) (funcall mode))
        (hermes-buffer--claim mode name)))
    buffer))

(provide 'hermes-buffer)
;;; hermes-buffer.el ends here
