;;; hermes-context.el --- On-demand backend context inspection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Project the owning session's reported context categories and file manifest.
;; Estimates and provider usage remain separate.  No polling or file access.

;;; Code:
(require 'hermes-browser)

(defvar-local hermes-context--owner nil "Exact originating chat attachment.")

(defun hermes-context--value (object key)
  "Return OBJECT's KEY as display text, preserving unknown values."
  (if (hermes-transport--field-present-p object key)
      (propertize (format "%s" (hermes-transport--get object key))
                  'face 'font-lock-constant-face)
    (propertize "Unknown" 'face 'shadow)))

(defun hermes-context--boolean (object key)
  "Return OBJECT's boolean KEY without equating missing with false."
  (if (hermes-transport--field-present-p object key)
      (propertize
       (if (hermes-transport--true-p (hermes-transport--get object key)) "yes" "no")
       'face 'font-lock-constant-face)
    (propertize "Unknown" 'face 'shadow)))

(defun hermes-context--text (result)
  "Format RESULT without inventing token accounting or accessing file paths."
  (let ((categories (hermes-transport--get result 'categories))
        (files (hermes-transport--get result 'context_files)))
    (unless (and (listp categories) (listp files))
      (error "Malformed context breakdown"))
    (concat
     (propertize "Reported usage\n" 'face 'bold)
     (format "Used: %s / Maximum: %s\nSource: %s · Estimated: %s\nModel: %s\n\n"
             (hermes-context--value result 'context_used)
             (hermes-context--value result 'context_max)
             (hermes-context--value result 'context_source)
             (hermes-context--boolean result 'context_estimated)
             (hermes-context--value result 'model))
     (propertize "Category estimates (not reconciled to reported usage)\n" 'face 'bold)
     (if categories
         (concat (mapconcat (lambda (row)
                              (format "%s: %s tokens (estimate)"
                                      (hermes-context--value row 'label)
                                      (hermes-context--value row 'tokens))) categories "\n")
                 (format "\nEstimated total: %s\n" (hermes-context--value result 'estimated_total)))
       (format "Unavailable / not built; returned estimated_total: %s\n"
               (hermes-context--value result 'estimated_total)))
     (propertize "\nContext files: " 'face 'bold)
     (if files
         (concat "Backend-reported rows; manifest completeness unknown\n"
                 (mapconcat
                  (lambda (row)
                    (format "%s\n  Path label: %s\n  Loaded: %s · Status: %s · Chars: %s · Estimated tokens: %s"
                            (hermes-context--value row 'label) (hermes-context--value row 'path)
                            (hermes-context--boolean row 'loaded) (hermes-context--value row 'status)
                            (hermes-context--value row 'chars) (hermes-context--value row 'est_tokens)))
                  files "\n"))
       "Unknown / unavailable; an empty manifest does not prove zero file cost")
     "\n\nPaths are backend labels, not local links.\nExplicit refresh can rebuild provider prompt blocks and invoke backend memory hooks.\nNo automatic polling.\n")))

(defun hermes-context--chat-owner ()
  "Capture this chat's exact ready runtime, transport and constructor claim."
  (unless (and (derived-mode-p 'hermes-chat-mode)
               (hermes-buffer--owned-p)
               hermes-chat--dashboard-session-ready-p
               hermes-chat--dashboard-active-session-id
               (hermes-chat--dashboard-client-live-p hermes-chat--dashboard-client))
    (user-error "Context inspection needs this chat's connected session"))
  (list (current-buffer) hermes-buffer--owner hermes-instance
        (hermes-browser--copy-identity hermes-instance)
        hermes-chat--dashboard-client
        (copy-sequence hermes-chat--dashboard-active-session-id)
        hermes-chat--transport-generation hermes-chat--lifecycle-generation))

(defun hermes-context--current-p (owner)
  "Return non-nil if OWNER still describes its original chat attachment."
  (and (buffer-live-p (car owner))
       (with-current-buffer (car owner)
         (condition-case nil
             (let ((current (hermes-context--chat-owner)))
               (and (eq (nth 1 owner) (nth 1 current))
                    (eq (nth 2 owner) (nth 2 current))
                    (eq (nth 4 owner) (nth 4 current))
                    (equal owner current)))
           (user-error nil)))))

(defun hermes-context-refresh ()
  "Explicitly request context detail on the original chat runtime.
This can rebuild backend provider prompt blocks; it is not a pure operation."
  (interactive nil hermes-context-mode)
  (unless (and (hermes-buffer--owned-p) (hermes-context--current-p hermes-context--owner))
    (user-error "Context attachment retired; reopen from the desired chat"))
  (hermes-browser--next-request-generation)
  (let* ((owner hermes-context--owner)
         (view (hermes-browser--owned-predicate '(hermes-context--owner)))
         (guard (lambda () (and (funcall view) (hermes-context--current-p owner))))
         (buffer (current-buffer)))
    (setq hermes-browser--status "Loading")
    (hermes-browser--start-owned
     (nth 4 owner) #'ignore
     (lambda (client _active)
       (hermes-dashboard-transport-call client "session.context_breakdown"
                                        `((session_id . ,(nth 5 owner)))))
     guard
     (lambda (result)
       (let ((text (hermes-context--text result)) (inhibit-read-only t))
         (atomic-change-group (erase-buffer) (insert text))
         (goto-char (point-min)))
       (setq hermes-browser--status "Backend snapshot; g explicit refresh"))
     (lambda (reason)
       (setq hermes-browser--status
             (format "Computation failed: %s; backend prompt hooks may have run" reason)))
     (lambda ()
       (when (and (buffer-live-p buffer) (funcall view)
                  (not (hermes-context--current-p owner)))
         (with-current-buffer buffer
           (setq hermes-browser--status "Attachment retired; reopen from chat")))))))

(defvar-keymap hermes-context-mode-map
  :parent special-mode-map
  "g" #'hermes-context-refresh "n" #'forward-line "p" #'previous-line
  "f" #'forward-char "b" #'backward-char)
(keymap-popup-annotate hermes-context-mode-map
  :popup-key "?" :exit-key "C-g" :description "Context budget"
  :group "Inspect" hermes-context-refresh "Explicit refresh" quit-window "Quit")
(define-derived-mode hermes-context-mode special-mode "Hermes Context"
  "Inspect backend context estimates and file status without local file access."
  (hermes-browser--setup-status))

;;;###autoload
(defun hermes-chat-context ()
  "Inspect this chat's context budget in an on-demand native read-only view.
The backend may rebuild prompt blocks and invoke memory hooks.  No inference
is requested, and no refresh runs automatically."
  (interactive nil hermes-chat-mode)
  (let* ((owner (hermes-context--chat-owner))
         (buffer (hermes-buffer--get
                  (generate-new-buffer-name "*Hermes Context*") #'hermes-context-mode)))
    (with-current-buffer buffer
      (hermes-browser--own-instance (nth 2 owner))
      (setq hermes-context--owner owner
            header-line-format
            (format " %s · Runtime %s · Explicit refresh may invoke backend memory hooks "
                    (propertize (hermes-instance-name (nth 2 owner))
                                'face 'hermes-browser-profile)
                    (propertize (nth 5 owner) 'face 'font-lock-constant-face))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer (hermes-context-refresh))))

(provide 'hermes-context)
;;; hermes-context.el ends here
