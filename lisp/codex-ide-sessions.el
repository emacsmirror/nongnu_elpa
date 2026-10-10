;;; codex-ide-sessions.el --- Live session list for codex-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo
;; Keywords: tools, terminals
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

;; Browse live Codex sessions with `codex-ide-list-sessions'.

;;; Code:

(require 'tabulated-list)
(require 'keymap-popup)
(require 'codex-ide)

(defvar codex-ide-sessions--buffer nil
  "Last owned session list buffer, if still available.")

(defvar-local codex-ide-sessions--source-buffer nil
  "Source buffer from which the session list was opened.")

(defun codex-ide-sessions--entry (session)
  "Return a tabulated list entry for SESSION."
  (let* ((root (plist-get session :root))
         (buffer (plist-get session :buffer))
         (active (equal (plist-get session :id)
                        (gethash root codex-ide--active-session-ids))))
    (list session
          (vector (file-name-nondirectory (directory-file-name root))
                  (or (buffer-local-value 'codex-ide--session-label buffer)
                      (buffer-name buffer))
                  (symbol-name (or (buffer-local-value
                                    'codex-ide-term--backend buffer) 'unknown))
                  (propertize (if active "Active" "Running")
                              'face (if active 'success 'shadow))
                  (abbreviate-file-name root)))))

(defun codex-ide-sessions--entries ()
  "Return rows for all live Codex sessions."
  (mapcar #'codex-ide-sessions--entry
          (cl-remove-if-not #'codex-ide--session-live-p
                            (codex-ide--all-sessions))))

(defun codex-ide-sessions--session-at-point ()
  "Return the exact live session at point, or signal a user error."
  (let ((session (tabulated-list-get-id)))
    (unless (and session
                 (eq session (codex-ide--session-by-id
                              (plist-get session :root)
                              (plist-get session :id)))
                 (codex-ide--session-live-p session))
      (user-error "No live Codex session at point; refresh the list"))
    session))

(defun codex-ide-sessions-switch ()
  "Switch to the session at point using the configured display policy."
  (interactive)
  (let ((session (codex-ide-sessions--session-at-point)))
    (when (buffer-live-p codex-ide-sessions--source-buffer)
      (codex-ide--record-source-buffer
       (plist-get session :root) codex-ide-sessions--source-buffer))
    (codex-ide--switch-to-session session)))

(defun codex-ide-sessions-kill ()
  "Kill the exact session at point after confirmation, then refresh."
  (interactive)
  (let* ((session (codex-ide-sessions--session-at-point))
         (buffer (plist-get session :buffer)))
    (when (yes-or-no-p (format "Kill Codex session %s? " (buffer-name buffer)))
      ;; Confirmation can run timers: refuse a replaced or departed owner.
      (unless (eq session (codex-ide-sessions--session-at-point))
        (user-error "Codex session changed during confirmation"))
      (kill-buffer buffer)
      (revert-buffer))))

(defun codex-ide-sessions-rename ()
  "Set the session label at point; use empty input for automatic naming."
  (interactive)
  (let* ((session (codex-ide-sessions--session-at-point))
         (label (read-string
                 "Session label (empty clears): "
                 (buffer-local-value 'codex-ide--session-label
                                     (plist-get session :buffer)))))
    (codex-ide--set-session-label session (string-trim label))
    (revert-buffer)))

(defvar codex-ide-sessions-mode-map)

(keymap-popup-define codex-ide-sessions-mode-map
  :popup-key "?"
  :description "Codex Sessions"
  :group "Session"
  "RET" ("Switch" codex-ide-sessions-switch)
  "k" ("Kill" codex-ide-sessions-kill)
  "r" ("Rename" codex-ide-sessions-rename)
  :group "List"
  "n" ("Next" next-line :stay-open)
  "p" ("Previous" previous-line :stay-open)
  "g" ("Refresh" revert-buffer :stay-open))

;; Define ? before inheriting describe-mode there: keymap-popup 0.4.1
;; refuses to replace a parent's binding when it installs the launcher.
(set-keymap-parent codex-ide-sessions-mode-map tabulated-list-mode-map)

(define-derived-mode codex-ide-sessions-mode tabulated-list-mode "Codex Sessions"
  "Browse live sessions.
\\<codex-ide-sessions-mode-map>
Use \\[codex-ide-sessions-switch] to switch, \\[codex-ide-sessions-kill] to kill,
\\[codex-ide-sessions-rename] to label, and \\[revert-buffer] to refresh.
Use \\[next-line] and \\[previous-line] to move, and ? for popup help."
  (setq tabulated-list-format [("Project" 18 t)
                               ("Label / Buffer" 30 t)
                               ("Backend" 10 t)
                               ("Status" 9 t)
                               ("Directory" 0 t)]
        tabulated-list-sort-key '("Project" . nil)
        tabulated-list-entries #'codex-ide-sessions--entries)
  (tabulated-list-init-header))

(defun codex-ide-sessions--owned-buffer-p (buffer)
  "Return non-nil when BUFFER is an unmodified, non-file session list."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'codex-ide-sessions-mode)
              (not buffer-file-name)
              (not (buffer-modified-p))))))

;;;###autoload
(defun codex-ide-list-sessions ()
  "Display all live Codex sessions in `*Codex Sessions*'.
Refresh with g, switch with RET, kill with k (after confirmation),
and label with r.  Use `codex-ide-switch-session' for completion."
  (interactive)
  (let* ((origin (current-buffer))
         (directory default-directory)
         (candidate (or (and (buffer-live-p codex-ide-sessions--buffer)
                             codex-ide-sessions--buffer)
                        (get-buffer "*Codex Sessions*")))
         (buffer (if (codex-ide-sessions--owned-buffer-p candidate) candidate
                   (generate-new-buffer "*Codex Sessions*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'codex-ide-sessions-mode)
        (codex-ide-sessions-mode))
      ;; Mode hooks can repurpose a new buffer; only refresh a retained owner.
      (unless (codex-ide-sessions--owned-buffer-p buffer)
        (user-error "Session list buffer was repurposed"))
      (unless (eq origin buffer)
        (setq codex-ide-sessions--source-buffer origin
              default-directory directory))
      (tabulated-list-print t))
    (setq codex-ide-sessions--buffer buffer)
    (pop-to-buffer buffer)))

(provide 'codex-ide-sessions)

;;; codex-ide-sessions.el ends here
