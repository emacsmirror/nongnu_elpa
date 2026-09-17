;;; hermes-gnosis.el --- Optional Gnosis practice handoff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Explicitly enable `hermes-gnosis-mode', then bind an exact retained practice
;; batch, open SQLite connection and ready chat with `hermes-gnosis-bind'.
;; Nothing loads Gnosis during ordinary Hermes startup.  Completion attempts one
;; plainly labelled ordinary prompt, never steering, retries or another queue.
;; Busy/unavailable delivery requires manual recovery.  The wire role is user,
;; but both the message and its retained history identify application authorship.
;; As with ordinary image sends, do not share this backend session with other
;; image-sending clients: released APIs cannot atomically exclude remote staging.

;;; Code:

(require 'hermes-chat)
(require 'sqlite)

(defvar gnosis-db)
(defvar gnosis-practice-completed-hook)
(declare-function gnosis-agent-status "gnosis-agent" (session-id))
(declare-function gnosis-agent-results "gnosis-agent" (session-id))

(defvar hermes-gnosis--bindings nil "Explicit process-local batch associations.")
(defvar-local hermes-gnosis--binding nil "This chat's exact batch association.")
(defvar hermes-gnosis-mode nil)

(defun hermes-gnosis--destination ()
  "Return the current chat's routing values, without acquiring any client."
  (let ((client hermes-chat--dashboard-client))
    (list hermes-chat--lifecycle-generation
          (hermes-dashboard-transport-client-generation client)
          (hermes-dashboard-transport--api-client-base-url client)
          (hermes-instance-id hermes-instance)
          (hermes-instance-name hermes-instance)
          (hermes-instance-url hermes-instance) hermes-chat--profile
          hermes-chat--dashboard-active-session-id hermes-chat--session-id)))

(defun hermes-gnosis--ready-p ()
  "Return non-nil if the current owned chat has an attached ready session."
  (let ((client hermes-chat--dashboard-client))
    (and (hermes-buffer--owned-p 'hermes-chat-mode)
         (hermes-chat--dashboard-default-transport-p)
         (hermes-dashboard-transport-client-p client)
         (hermes-dashboard-transport-client-ready-p client)
         (not (hermes-dashboard-transport-client-stopping-p client))
         (not (hermes-dashboard-transport-client-reconnecting-p client))
         (hermes-dashboard-transport-client-websocket client)
         hermes-chat--dashboard-session-ready-p
         (stringp hermes-chat--dashboard-active-session-id)
         (stringp hermes-chat--session-id))))

(defun hermes-gnosis--current-p (binding)
  "Return non-nil if BINDING still owns its original chat and connection."
  (and hermes-gnosis-mode (not (plist-get binding :retired))
       (buffer-live-p (plist-get binding :buffer))
       (with-current-buffer (plist-get binding :buffer)
         (and (eq hermes-gnosis--binding binding)
              (hermes-gnosis--ready-p)
              (not hermes-chat--interrupted-assistant-id)
              (not hermes-chat--interrupt-request-pending-p)
              (eq hermes-buffer--owner (plist-get binding :claim))
              (eq hermes-chat--dashboard-client (plist-get binding :client))
              (eq (hermes-dashboard-transport-client-websocket
                   hermes-chat--dashboard-client) (plist-get binding :socket))
              (equal (hermes-gnosis--destination) (plist-get binding :destination))))))

(defun hermes-gnosis--database (connection)
  "Return CONNECTION's main filename, refusing nil or closed SQLite objects."
  (unless (and connection (sqlitep connection))
    (user-error "An exact open Gnosis SQLite connection is required"))
  (or (nth 2 (assoc 0 (sqlite-select connection "PRAGMA database_list")))
      (user-error "Gnosis origin has no main database")))

(defun hermes-gnosis--read (binding &optional results)
  "Read BINDING's exact batch status, or detailed RESULTS, without reopening."
  (unless (equal (plist-get binding :database)
                 (hermes-gnosis--database (plist-get binding :connection)))
    (user-error "Gnosis database origin changed"))
  (let* ((gnosis-db (plist-get binding :connection))
         (status (funcall (if results #'gnosis-agent-results #'gnosis-agent-status)
                          (plist-get binding :batch))))
    (unless (and (equal (plist-get status :api-version) 1)
                 (equal (plist-get status :mode) "practice")
                 (equal (plist-get status :session-id) (plist-get binding :batch))
                 (equal (plist-get status :database) (plist-get binding :database)))
      (user-error "Gnosis batch origin is unavailable"))
    status))

(defun hermes-gnosis--retire ()
  "Permanently retire this chat's association without touching its draft."
  (when hermes-gnosis--binding
    (setf (plist-get hermes-gnosis--binding :retired) t)))

(defun hermes-gnosis--changed ()
  "Retire an association whose chat was stopped or replaced."
  (when (and hermes-gnosis--binding
             (not (hermes-gnosis--current-p hermes-gnosis--binding)))
    (hermes-gnosis--retire)))

(defun hermes-gnosis-unbind ()
  "Forget this chat's Gnosis association and invalidate its result handle."
  (interactive nil hermes-chat-mode)
  (hermes-gnosis--retire)
  (setq hermes-gnosis--bindings (delq hermes-gnosis--binding hermes-gnosis--bindings)
        hermes-gnosis--binding nil)
  (remove-hook 'hermes-chat-lifecycle-invalidation-hook #'hermes-gnosis--retire t)
  (remove-hook 'hermes-chat-state-change-hook #'hermes-gnosis--changed t)
  (remove-hook 'after-set-visited-file-name-hook #'hermes-gnosis--retire t)
  (remove-hook 'change-major-mode-hook #'hermes-gnosis-unbind t)
  (remove-hook 'kill-buffer-hook #'hermes-gnosis-unbind t))

(defun hermes-gnosis-bind (batch connection chat)
  "Bind exact Gnosis BATCH and open SQLite CONNECTION to CHAT, returning a handle.
CHAT must be an owned, connected Hermes chat with an attached session.
Enable `hermes-gnosis-mode' explicitly first.  One binding is retained per chat;
rebinding the same live tuple preserves its handle and completion attempt.
Replacement invalidates the old handle; `hermes-gnosis-unbind' revokes it
explicitly.  This does not start or resume practice, read answers, submit
existing completion, or acquire a backend connection."
  (unless hermes-gnosis-mode (user-error "Enable hermes-gnosis-mode first"))
  (unless (and (stringp batch) (not (string-empty-p batch)) (buffer-live-p chat))
    (user-error "An exact batch ID and live chat buffer are required"))
  (with-current-buffer chat
    (unless (hermes-gnosis--ready-p) (user-error "Hermes chat is not attached and ready"))
    (let* ((binding (list :batch (copy-sequence batch) :connection connection
                          :database (copy-sequence (hermes-gnosis--database connection))
                          :buffer chat :claim hermes-buffer--owner
                          :client hermes-chat--dashboard-client
                          :socket (hermes-dashboard-transport-client-websocket
                                   hermes-chat--dashboard-client)
                          :destination (mapcar (lambda (value)
                                                 (if (stringp value)
                                                     (copy-sequence value) value))
                                               (hermes-gnosis--destination))
                          :handle (hermes-dashboard-transport--generate-token)
                          :retired nil :attempted nil)))
      (hermes-gnosis--read binding)
      (unless (and hermes-gnosis--binding
                   (hermes-gnosis--current-p hermes-gnosis--binding)
                   (eq connection (plist-get hermes-gnosis--binding :connection))
                   (equal batch (plist-get hermes-gnosis--binding :batch))
                   (equal (plist-get binding :database)
                          (plist-get hermes-gnosis--binding :database)))
        (hermes-gnosis-unbind)
        (setq hermes-gnosis--binding binding)
        (push binding hermes-gnosis--bindings)
        (add-hook 'hermes-chat-lifecycle-invalidation-hook #'hermes-gnosis--retire nil t)
        (add-hook 'hermes-chat-state-change-hook #'hermes-gnosis--changed nil t)
        (add-hook 'after-set-visited-file-name-hook #'hermes-gnosis--retire nil t)
        (add-hook 'change-major-mode-hook #'hermes-gnosis-unbind nil t)
        (add-hook 'kill-buffer-hook #'hermes-gnosis-unbind nil t))
      (plist-get hermes-gnosis--binding :handle))))

(defun hermes-gnosis-results (handle)
  "Read results for exact process-local HANDLE's retained batch and owner.
Revalidate the original chat, SQLite object, filename and batch.  Never reopen
or fall back to the current database.  Results contain private learner data;
only request them for the explicitly associated study conversation."
  (let ((binding (seq-find (lambda (entry) (equal handle (plist-get entry :handle)))
                           hermes-gnosis--bindings)))
    (unless (and binding (hermes-gnosis--current-p binding))
      (user-error "Gnosis result handle is unavailable or retired"))
    (let ((result (hermes-gnosis--read binding t)))
      (unless (equal (plist-get result :status) "completed")
        (user-error "Gnosis batch is not completed"))
      result)))

(defun hermes-gnosis--notice (binding)
  "Return minimal application-origin completion text for BINDING."
  (format (concat "[Gnosis application event; not learner-authored]\n"
                  "event=practice-completed; api-version=1; batch=%S\n"
                  "Completion is not a mastery claim.\n"
                  "Read its exact retained results, if needed, with Emacs Lisp:\n"
                  "(hermes-gnosis-results %S)\n"
                  "Do not grade, mutate study data, or start another batch automatically.\n"
                  "[/Gnosis application event]")
          (plist-get binding :batch) (plist-get binding :handle)))

(defun hermes-gnosis-copy-notice ()
  "Copy this chat's completed batch notice for explicit manual recovery.
Inspect chat history first: a previous attempt may have been accepted.
This never sends a prompt or changes the composer."
  (interactive nil hermes-chat-mode)
  (unless (and hermes-gnosis--binding
               (hermes-gnosis--current-p hermes-gnosis--binding)
               (equal "completed" (plist-get (hermes-gnosis--read
                                              hermes-gnosis--binding) :status)))
    (user-error "No completed batch with a current association; explicitly bind again"))
  (kill-new (hermes-gnosis--notice hermes-gnosis--binding))
  (message "Gnosis notice copied; inspect history before manually sending"))

(defun hermes-gnosis--admit-p (binding)
  "Return non-nil if BINDING still owns its destination and completed origin."
  (and (hermes-gnosis--current-p binding)
       (condition-case nil
           (equal "completed" (plist-get (hermes-gnosis--read binding) :status))
         ((error quit) nil))))

(defun hermes-gnosis--deliver (binding)
  "Attempt BINDING once, or retain a visible manual-recovery notice."
  (setf (plist-get binding :attempted) t)
  (if (not (hermes-gnosis--current-p binding))
      (message "Gnosis completion not sent: chat retired; explicitly bind again")
    (with-current-buffer (plist-get binding :buffer)
      (if (or (hermes-chat--active-turn-p) hermes-chat--queued-messages
              hermes-chat--session-bootstrap (hermes-chat--pending-prompt-p)
              (gethash (hermes-chat--image-session-key) hermes-chat--image-prior-submits))
          (hermes-chat--insert-local-status
           "Gnosis completed; not sent while busy.  Use M-x hermes-gnosis-copy-notice" 'done)
        (unless (hermes-chat--submit-content
                 (hermes-gnosis--notice binding) nil nil
                 (lambda () (hermes-gnosis--admit-p binding)))
          (message "Gnosis delivery unavailable; inspect history and use hermes-gnosis-copy-notice"))))))

(defun hermes-gnosis--completed (event)
  "Consume Gnosis EVENT only for an explicitly associated exact origin."
  (when (and hermes-gnosis-mode (equal (plist-get event :api-version) 1)
             (equal (plist-get event :mode) "practice"))
    (dolist (binding (copy-sequence hermes-gnosis--bindings))
      (when (and (not (plist-get binding :attempted))
                 (eq (plist-get event :connection) (plist-get binding :connection))
                 (equal (plist-get event :database) (plist-get binding :database))
                 (equal (plist-get event :session-id) (plist-get binding :batch)))
        (condition-case nil
            (when (equal "completed" (plist-get (hermes-gnosis--read binding) :status))
              (hermes-gnosis--deliver binding))
          ((error quit)
           (setf (plist-get binding :attempted) t)
           (message "Gnosis completion unavailable; explicitly recheck origin and history")))))))

;;;###autoload
(define-minor-mode hermes-gnosis-mode
  "Enable optional, explicitly bound Gnosis practice completion handoffs.
No automatic binding, retry, database opening or answer publication occurs.
Disabling forgets all bindings and invalidates their result handles."
  :global t :group 'hermes
  (if hermes-gnosis-mode
      (condition-case err
          (unless (and (require 'gnosis-agent nil t)
                       (boundp 'gnosis-practice-completed-hook)
                       (fboundp 'gnosis-agent-status) (fboundp 'gnosis-agent-results))
            (user-error "Gnosis practice completion API is unavailable"))
        ((error quit)
         (setq hermes-gnosis-mode nil)
         (signal (car err) (cdr err))))
    (dolist (binding (copy-sequence hermes-gnosis--bindings))
      (when (buffer-live-p (plist-get binding :buffer))
        (with-current-buffer (plist-get binding :buffer) (hermes-gnosis-unbind))))
    (setq hermes-gnosis--bindings nil))
  (if hermes-gnosis-mode
      (add-hook 'gnosis-practice-completed-hook #'hermes-gnosis--completed)
    (remove-hook 'gnosis-practice-completed-hook #'hermes-gnosis--completed)))

(provide 'hermes-gnosis)
;;; hermes-gnosis.el ends here
