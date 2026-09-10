;;; hermes-chat-todos.el --- Session-owned live task projection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A read-only companion view of backend task snapshots.  The chat owns its
;; state; opening the panel neither fetches history nor creates a planner.
;; Ordinary transport callbacks provide the lifetime and connection fences.
;; A terminal turn settles the display, never the individual task statuses.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'button)
(require 'hermes-chat-buffer)

(defvar-local hermes-chat--todos nil
  "Last backend task projection, including its runtime and turn owners.")
(defvar-local hermes-chat--todos-panel nil
  "Companion buffer explicitly opened for this chat's task projection.")
(defvar-local hermes-chat-todos--owner nil
  "Chat buffer owning this task panel.")
(defvar-local hermes-chat-todos--ewoc nil
  "EWOC displaying the task projection.")

(defun hermes-chat-todos--snapshot-state (snapshot runtime turn accepting live)
  "Return projection of SNAPSHOT for RUNTIME, TURN, ACCEPTING and LIVE state."
  (let* ((items (plist-get snapshot :items))
         (active (and live (cl-some
                            (lambda (item)
                              (member (plist-get item :status)
                                      '("pending" "in_progress"))) items))))
    (list :runtime runtime :turn turn :accepting accepting
          :revision (plist-get snapshot :revision) :items items
          :status (cond ((null items) 'empty) (active 'active) (t 'settled)))))

(defun hermes-chat-todos--reduce (state event runtime turn)
  "Return STATE after normalized EVENT for current RUNTIME and TURN.
Accept only full snapshots for the current accepting turn.  A revision is a
runtime watermark, not a turn counter; unversioned legacy updates cannot
replace a versioned snapshot.  Terminal events retain unfinished items."
  (pcase (plist-get event :type)
    ('todo
     (let* ((snapshot (plist-get event :snapshot))
            (revision (plist-get snapshot :revision))
            (watermark (plist-get state :revision)))
       (if (and snapshot runtime turn (plist-get state :accepting)
                (equal turn (plist-get state :turn))
                (equal runtime (plist-get event :session-id))
                (or (null (plist-get state :runtime))
                    (equal runtime (plist-get state :runtime)))
                (or (null watermark)
                    (and revision (> revision watermark))))
           (hermes-chat-todos--snapshot-state snapshot runtime turn t t)
         state)))
    ((or 'done 'error 'suppressed-terminal)
     (if (equal turn (plist-get state :turn))
         (hermes-chat--entry-with
          state :accepting nil
          :status (if (eq (plist-get state :status) 'stale) 'stale
                    (if (plist-get state :items) 'settled 'empty)))
       state))
    (_ state)))

(defun hermes-chat-todos--begin (turn)
  "Begin accepting snapshots for TURN, retaining the prior list as settled."
  (unless (equal turn (plist-get hermes-chat--todos :turn))
    (let* ((runtime hermes-chat--dashboard-active-session-id)
           (old-runtime (plist-get hermes-chat--todos :runtime))
           (state (and (or (null runtime) (equal runtime old-runtime))
                       hermes-chat--todos)))
      (setq hermes-chat--todos
            (hermes-chat--entry-with
             state :runtime runtime :turn turn :accepting t
             :status (if (plist-get state :items) 'settled 'empty)))
      (hermes-chat-todos--refresh))))

(defun hermes-chat-todos--rotate (old new)
  "Transfer the current todo turn from interim assistant OLD to NEW."
  (when (equal old (plist-get hermes-chat--todos :turn))
    (setq hermes-chat--todos (hermes-chat--entry-with hermes-chat--todos :turn new))))

(defun hermes-chat-todos--accept (turn event)
  "Reduce EVENT for TURN and refresh only an already-open companion."
  (let ((next (hermes-chat-todos--reduce
               hermes-chat--todos event hermes-chat--dashboard-active-session-id turn)))
    (unless (equal next hermes-chat--todos)
      (setq hermes-chat--todos next)
      (hermes-chat-todos--refresh))))

(defun hermes-chat-todos--attach (runtime &optional value running)
  "Bind the projection to RUNTIME, optionally restoring VALUE when RUNNING.
Use only the backend's optional full snapshot, not client history parsing.
An idle restored list is settled even if its tasks remain unfinished."
  (let* ((previous (plist-get hermes-chat--todos :runtime))
         (same (or (null previous) (equal previous runtime)))
         (state (and same hermes-chat--todos))
         (snapshot (hermes-transport--todo-snapshot value))
         (revision (plist-get snapshot :revision))
         (watermark (plist-get state :revision))
         (turn hermes-chat--pending-assistant-id))
    (setq hermes-chat--todos
          (if (and snapshot
                   (or (plist-get snapshot :items) (and revision (> revision 0)))
                   (or (null watermark) (and revision (>= revision watermark))))
              (hermes-chat-todos--snapshot-state snapshot runtime turn
                                                 (or running turn) running)
            (hermes-chat--entry-with
             state :runtime runtime :turn (or (plist-get state :turn) turn)
             :accepting (or (plist-get state :accepting) turn))))
    (hermes-chat-todos--refresh)))

(defun hermes-chat-todos--reattach ()
  "Accept fresh snapshots for the reattached live turn, without reviving history."
  (setq hermes-chat--todos
        (hermes-chat--entry-with
         hermes-chat--todos :turn hermes-chat--pending-assistant-id :accepting t))
  (hermes-chat-todos--refresh))

(defun hermes-chat-todos--disconnect ()
  "Retain the last task list as stale after its live attachment is lost."
  (when (or hermes-chat--todos hermes-chat--todos-panel)
    (setq hermes-chat--todos
          (hermes-chat--entry-with hermes-chat--todos :accepting nil :status 'stale))
    (hermes-chat-todos--refresh)))

(defun hermes-chat-todos--panel-p (panel owner)
  "Return non-nil if PANEL is still the task companion belonging to OWNER."
  (and (buffer-live-p panel)
       (with-current-buffer panel
         (and (derived-mode-p 'hermes-chat-todos-mode)
              (eq owner hermes-chat-todos--owner)))))

(defun hermes-chat-todos--clear ()
  "Clear the projection and retire only this chat's still-owned panel."
  (let ((panel hermes-chat--todos-panel))
    (setq hermes-chat--todos nil hermes-chat--todos-panel nil)
    (when (hermes-chat-todos--panel-p panel (current-buffer))
      (kill-buffer panel))))

(defun hermes-chat-todos--summary (state)
  "Return a compact, faced summary of task STATE."
  (let* ((items (plist-get state :items))
         (counted (cl-remove-if (lambda (item)
                                  (equal (plist-get item :status) "cancelled")) items))
         (done (cl-count "completed" counted :key (lambda (item) (plist-get item :status))
                         :test #'equal))
         (status (plist-get state :status))
         (label (pcase status
                  ('active "Active") ('settled "Settled · last list")
                  ('stale "Disconnected / stale") (_ "No task list"))))
    (concat (propertize label 'face (pcase status
                                      ('active 'font-lock-type-face)
                                      ('stale 'warning) (_ 'shadow)))
            (when items (format " · %d/%d" done (length counted))) "\n\n")))

(defun hermes-chat-todos--print-item (item)
  "Insert one read-only task ITEM, preserving its backend status."
  (pcase-let* ((status (plist-get item :status))
              (`(,mark ,face) (pcase status
                               ("completed" '("[X]" success))
                               ("in_progress" '("[-]" font-lock-type-face))
                               ("cancelled" '("[/]" shadow))
                               (_ '("[ ]" default)))))
    (insert (propertize (concat mark " " (plist-get item :content))
                        'face face 'help-echo status)
            "\n")))

(defun hermes-chat-todos--refresh ()
  "Refresh this chat's existing panel without displaying or selecting it."
  (let ((panel hermes-chat--todos-panel)
        (owner (current-buffer))
        (state hermes-chat--todos))
    (when (hermes-chat-todos--panel-p panel owner)
      (with-current-buffer panel
        (let ((inhibit-read-only t))
          (save-excursion
            (ewoc-set-hf hermes-chat-todos--ewoc
                         (hermes-chat-todos--summary state) "")
            (let ((node (ewoc-nth hermes-chat-todos--ewoc 0)))
              (dolist (item (plist-get state :items))
                (if node
                    (progn
                      (unless (equal item (ewoc-data node))
                        (ewoc-set-data node item)
                        (ewoc-invalidate hermes-chat-todos--ewoc node))
                      (setq node (ewoc-next hermes-chat-todos--ewoc node)))
                  (ewoc-enter-last hermes-chat-todos--ewoc item)))
              (while node
                (let ((next (ewoc-next hermes-chat-todos--ewoc node)))
                  (ewoc-delete hermes-chat-todos--ewoc node)
                  (setq node next))))))))))

(defun hermes-chat-todos--detach ()
  "Detach this companion on kill or major mode change."
  (when (buffer-live-p hermes-chat-todos--owner)
    (let ((panel (current-buffer)))
      (with-current-buffer hermes-chat-todos--owner
        (when (eq panel hermes-chat--todos-panel)
          (setq hermes-chat--todos-panel nil))))))

(defun hermes-chat-todos-return-to-chat (&optional _button)
  "Return to the owning chat, optionally from a clicked BUTTON."
  (interactive)
  (unless (and (buffer-live-p hermes-chat-todos--owner)
               (with-current-buffer hermes-chat-todos--owner
                 (derived-mode-p 'hermes-chat-mode)))
    (user-error "The owning chat is no longer available"))
  (pop-to-buffer hermes-chat-todos--owner))

(defun hermes-chat-todos-next ()
  "Move to the next line of the task panel."
  (interactive)
  (forward-line 1))

(defun hermes-chat-todos-previous ()
  "Move to the previous line of the task panel."
  (interactive)
  (forward-line -1))

(defvar-keymap hermes-chat-todos-mode-map
  :parent special-mode-map
  "n" #'hermes-chat-todos-next
  "p" #'hermes-chat-todos-previous
  "RET" #'hermes-chat-todos-return-to-chat)

(define-derived-mode hermes-chat-todos-mode special-mode "Hermes Tasks"
  "Read-only live task projection for one Hermes chat."
  :interactive nil
  (add-hook 'kill-buffer-hook #'hermes-chat-todos--detach nil t)
  (add-hook 'change-major-mode-hook #'hermes-chat-todos--detach nil t))

;;;###autoload
(defun hermes-chat-show-todos ()
  "Show this chat's read-only live task panel without moving composer point.
The panel updates only from structured live backend events, never by polling
or interpreting old transcript text.  Settled lists retain unfinished tasks."
  (interactive)
  (unless (derived-mode-p 'hermes-chat-mode)
    (user-error "Open the task panel from a Hermes chat"))
  ;; Also support chats created before this module was loaded into Emacs.
  (add-hook 'hermes-chat-lifecycle-invalidation-hook #'hermes-chat-todos--clear nil t)
  (when (and hermes-chat--pending-assistant-id
             (null (plist-get hermes-chat--todos :turn)))
    (hermes-chat-todos--begin hermes-chat--pending-assistant-id))
  (unless (hermes-chat-todos--panel-p hermes-chat--todos-panel (current-buffer))
    (let ((owner (current-buffer))
          (panel (generate-new-buffer (format "*Hermes Tasks: %s*" (buffer-name)))))
      (setq hermes-chat--todos-panel panel)
      (with-current-buffer panel
        (hermes-chat-todos-mode)
        (setq hermes-chat-todos--owner owner)
        (let ((inhibit-read-only t))
          (insert-text-button "Return to chat" 'action #'hermes-chat-todos-return-to-chat
                              'follow-link t)
          (insert "\n\n")
          (setq hermes-chat-todos--ewoc (ewoc-create #'hermes-chat-todos--print-item nil nil t))))))
  (hermes-chat-todos--refresh)
  (display-buffer hermes-chat--todos-panel)
  hermes-chat--todos-panel)

(provide 'hermes-chat-todos)
;;; hermes-chat-todos.el ends here
