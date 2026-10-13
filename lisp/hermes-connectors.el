;;; hermes-connectors.el --- In-chat connector authorization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A running tool can open a connection operation: a connector to
;; authorize, an account to repair, or an install to approve.  The backend
;; sends `connection.request' and full-snapshot `connection.update' frames;
;; this module projects the newest snapshot as a compact card under the
;; originating tool row, or as its own transcript entry until that row
;; exists.  Only the backend changes target state.  Authorization links live
;; in memory as button data; credentials are never collected or stored.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'browse-url)
(require 'keymap-popup)
(require 'subr-x)
(require 'hermes-buffer)
(require 'hermes-chat-render)
(require 'hermes-chat-buffer)
(require 'hermes-dashboard-transport)
(require 'hermes-transport)

(defvar hermes-chat-cleanup-functions)

(defvar-local hermes-connectors--operations nil
  "Connection records for this chat, newest first.
Each record is a plist (:snapshot SNAPSHOT :context CONTEXT :pending BOOL
:error TEXT :closed BOOL).  CONTEXT is the exact chat attachment allowed to
act on SNAPSHOT.")

;;; Pure projection

(defconst hermes-connectors--request-keys '(:tool-call-id :suggested)
  "Snapshot fields that only `connection.request' carries.")

(defun hermes-connectors--newer (old new)
  "Return the snapshot after NEW arrives on OLD, or OLD when unchanged.
Target state follows the higher sequence number.  Request fields are
immutable identity, independent of sequencing: the backend emits its
preparation updates first, so the request can arrive at an equal or older
sequence, and later updates omit those fields.  Keep each from whichever
frame carried it."
  (let* ((stale (and old (<= (plist-get new :seq) (plist-get old :seq))))
         (winner (if stale old new))
         (loser (if stale new old))
         (adopted (cl-loop for key in hermes-connectors--request-keys
                           for value = (plist-get loser key)
                           when (and value (not (plist-get winner key)))
                           append (list key value))))
    (if adopted
        (append adopted
                (cl-loop for (key value) on winner by #'cddr
                         unless (plist-member adopted key)
                         append (list key value)))
      winner)))

(defun hermes-connectors--unresolved-p (target)
  "Return non-nil when TARGET still needs the user."
  (or (not (member (plist-get target :state)
                   '("connected" "skipped" "not_connected")))
      (plist-get target :discovery-error)))

(defun hermes-connectors--open-p (record)
  "Return non-nil when RECORD's operation is still open on the backend."
  (not (or (plist-get (plist-get record :snapshot) :settled)
           (plist-get record :closed))))

(defun hermes-connectors--actions (snapshot target)
  "Return the actions the backend accepts for TARGET in open SNAPSHOT.
TARGET is nil for a directory operation, which only continues."
  (let ((state (plist-get target :state))
        (url (plist-get target :connect-url))
        (needs (plist-get target :needs)))
    (append
     (when (and url (member state '("pending" "initiated"))) '(open))
     (when (and url (equal state "initiated")) '(wake))
     (when (and (equal state "pending") (not needs)
                (not (equal (plist-get target :kind) "connector")))
       '(approve))
     (when (and (member state '("failed" "expired")) (not needs)) '(retry))
     (when (and target (hermes-connectors--unresolved-p target)
                (not (equal state "connected")))
       '(decline))
     (when (or (null target)
               (seq-some #'hermes-connectors--unresolved-p
                         (plist-get snapshot :targets)))
       '(continue)))))

(defun hermes-connectors--title (target)
  "Return TARGET's card title, such as \"Reconnect github\"."
  (format "%s %s"
          (pcase (plist-get target :action)
            ("authorize" "Authorize") ("enable" "Enable")
            ("install" "Install") ("reconnect" "Reconnect") (_ "Connect"))
          (plist-get target :name)))

(defun hermes-connectors--status (target)
  "Return TARGET's backend state as a short faced label."
  (pcase (plist-get target :state)
    ("pending" (propertize "pending" 'face 'warning))
    ("initiated" (if (plist-get target :connect-url)
                     (propertize "waiting for browser" 'face 'warning)
                   (propertize "working" 'face 'shadow)))
    ("connected" (if (plist-get target :discovery-error)
                     (propertize "connected, tools unavailable" 'face 'warning)
                   (propertize "connected" 'face 'success)))
    ("failed" (propertize "failed" 'face 'error))
    ("expired" (propertize "link expired" 'face 'error))
    ("not_connected" (propertize "not connected" 'face 'shadow))
    (state (propertize (string-replace "_" " " state) 'face 'shadow))))

(defun hermes-connectors--notes (target open)
  "Return TARGET's secondary lines, with guidance when OPEN."
  (delq nil
        (list (when open
                (or (plist-get target :instructions)
                    (and (equal (plist-get target :state) "initiated")
                         (plist-get target :connect-url)
                         "Finish authorization in your browser.")))
              (when-let* ((needs (and open (plist-get target :needs))))
                (format "Needs %s; enter credentials in Hermes Desktop."
                        (string-join needs ", ")))
              (plist-get target :detail)
              (plist-get target :discovery-error))))

;;; Ownership

(defun hermes-connectors--context ()
  "Capture the exact current chat attachment."
  (list :buffer (current-buffer) :claim hermes-buffer--owner
        :lifetime hermes-chat--lifecycle-generation
        :client hermes-chat--dashboard-client
        :connection (and hermes-chat--dashboard-client
                         (hermes-dashboard-transport-client-generation
                          hermes-chat--dashboard-client))
        :session hermes-chat--dashboard-active-session-id
        :profile hermes-chat--profile))

(defun hermes-connectors--current-p (context)
  "Return non-nil while CONTEXT still owns this chat attachment."
  (and (eq (current-buffer) (plist-get context :buffer))
       (hermes-buffer--owned-p 'hermes-chat-mode)
       (eq hermes-buffer--owner (plist-get context :claim))
       (hermes-chat--current-lifetime-p (plist-get context :lifetime))
       (plist-get context :session)
       (equal hermes-chat--dashboard-active-session-id
              (plist-get context :session))
       (equal hermes-chat--profile (plist-get context :profile))
       hermes-chat--dashboard-client
       (eq hermes-chat--dashboard-client (plist-get context :client))
       (equal (hermes-dashboard-transport-client-generation
               hermes-chat--dashboard-client)
              (plist-get context :connection))))

;;; Records and transcript placement

(defun hermes-connectors--record (id)
  "Return this chat's record for operation ID."
  (seq-find (lambda (record)
              (equal id (plist-get (plist-get record :snapshot) :id)))
            hermes-connectors--operations))

(defun hermes-connectors--entry-call (entry)
  "Return the tool call id of transcript ENTRY, or nil."
  (and (eq (plist-get entry :role) 'tool)
       (plist-get (plist-get (plist-get entry :metadata) :event)
                  :tool-call-id)))

(defun hermes-connectors--tool-entry (snapshot)
  "Return the tool entry SNAPSHOT is bound to, or nil."
  (when-let* ((call (plist-get snapshot :tool-call-id)))
    (seq-find (lambda (entry)
                (equal call (hermes-connectors--entry-call entry)))
              (hermes-chat--entries))))

(defun hermes-connectors--sync (record)
  "Render RECORD under its tool row, or as its own entry until one exists."
  (let* ((snapshot (plist-get record :snapshot))
         (tool (hermes-connectors--tool-entry snapshot))
         (id (concat "connection:" (plist-get snapshot :id))))
    (if tool
        (progn
          (hermes-chat--remove-entry id t)
          (hermes-chat--update-entry (plist-get tool :id) #'identity))
      (unless (hermes-chat--update-entry id #'identity)
        (hermes-chat--insert-entry
         (hermes-chat--make-entry 'connection "" 'done id
                                  (list :operation-id (plist-get snapshot :id)))
         (hermes-chat--pending-assistant-node))))))

(defun hermes-connectors--accept (snapshot)
  "Project authoritative SNAPSHOT for the current attachment.
A snapshot delivered to the current attachment also rebinds an older card to
it, releasing any request the previous connection left pending."
  (let* ((context (hermes-connectors--context))
         (record (or (hermes-connectors--record (plist-get snapshot :id))
                     (car (push (list :snapshot nil :context context
                                      :pending nil :error nil :closed nil)
                                hermes-connectors--operations))))
         (old (plist-get record :snapshot))
         (next (hermes-connectors--newer old snapshot))
         (rebound (not (hermes-connectors--current-p
                        (plist-get record :context)))))
    (when rebound
      (setf (plist-get record :context) context
            (plist-get record :pending) nil))
    (when (or rebound (not (eq old next)))
      (setf (plist-get record :snapshot) next
            (plist-get record :error) nil)
      (hermes-connectors--sync record))))

(defun hermes-connectors--handle-event (event)
  "Consume connection EVENT for the active session; return non-nil if owned.
Connection frames never change the chat's turn state."
  (when (eq (plist-get event :type) 'connection)
    (when (and hermes-chat--dashboard-active-session-id
               (equal (plist-get event :session-id)
                      hermes-chat--dashboard-active-session-id))
      (hermes-connectors--accept (plist-get event :operation)))
    t))

(defun hermes-connectors--restore (result)
  "Restore the open operation from an owned session resume RESULT.
Any other card this chat still shows as open is no longer open there."
  (let ((snapshot (hermes-transport--connection-snapshot
                   (hermes-transport--get result 'pending_connection))))
    (dolist (record hermes-connectors--operations)
      (when (and (hermes-connectors--open-p record)
                 (not (equal (plist-get snapshot :id)
                             (plist-get (plist-get record :snapshot) :id))))
        (setf (plist-get record :closed) t
              (plist-get record :pending) nil)
        (hermes-connectors--sync record)))
    (when snapshot
      (hermes-connectors--accept snapshot))))

(defun hermes-connectors--observe (event)
  "Keep cards in step with tool rows and the connection after EVENT."
  (pcase (plist-get event :type)
    ('tool
     (when-let* ((call (plist-get event :tool-call-id)))
       (dolist (record hermes-connectors--operations)
         (when (equal call (plist-get (plist-get record :snapshot)
                                      :tool-call-id))
           (hermes-connectors--sync record)))))
    ('status
     ;; A lost or replaced connection retires live links and actions.
     (when (member (plist-get event :status)
                   '("closed" "reconnecting" "reconnected"))
       (dolist (record hermes-connectors--operations)
         (when (hermes-connectors--open-p record)
           (hermes-connectors--sync record)))))))

(defun hermes-connectors--clear ()
  "Forget this chat's connection records."
  (setq hermes-connectors--operations nil))

;;; Actions

(defun hermes-connectors--selection ()
  "Return (RECORD . TARGET) for the card at point, or nil."
  (when-let* ((selection (get-text-property (point) 'hermes-connector))
              (record (car selection))
              ((memq record hermes-connectors--operations)))
    (cons record
          (seq-find (lambda (row) (equal (plist-get row :name) (cdr selection)))
                    (plist-get (plist-get record :snapshot) :targets)))))

(defun hermes-connectors--record-actions (record target)
  "Return the actions RECORD's owner may take on TARGET now."
  (when (and (hermes-connectors--open-p record)
             (not (plist-get record :pending))
             (eq record (car hermes-connectors--operations))
             (hermes-connectors--current-p (plist-get record :context)))
    (hermes-connectors--actions (plist-get record :snapshot) target)))

(defun hermes-connectors--available-p (action)
  "Return non-nil when ACTION is valid for the card at point."
  (pcase-let ((`(,record . ,target) (hermes-connectors--selection)))
    (and record
         (memq action (hermes-connectors--record-actions record target)))))

(defun hermes-connectors--rpc (record method params &optional snapshot-p)
  "Send METHOD with PARAMS as RECORD's session owner.
With SNAPSHOT-P, project the operation snapshot the result carries.  Late
replies are dropped once the record's attachment is no longer current."
  (let* ((context (plist-get record :context))
         (buffer (current-buffer))
         (id (plist-get (plist-get record :snapshot) :id))
         (current (lambda ()
                    (and (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (and (memq record hermes-connectors--operations)
                                (hermes-connectors--current-p context))))))
         (finish
          (lambda (result error-text)
            (when (funcall current)
              (with-current-buffer buffer
                (setf (plist-get record :pending) nil
                      (plist-get record :error) error-text)
                (if-let* ((next (and snapshot-p
                                     (hermes-transport--connection-snapshot
                                      result)))
                          ((equal id (plist-get next :id))))
                    (hermes-connectors--accept next)
                  (hermes-connectors--sync record))))))
         (hermes-dashboard-transport-dispatch-guard current)
         (hermes-dashboard-transport-request-owner buffer))
    (setf (plist-get record :pending) t
          (plist-get record :error) nil)
    (hermes-connectors--sync record)
    (condition-case err
        (hermes-dashboard-transport-request
         (plist-get context :client) method
         `((owner . ((type . "session")
                     (session_id . ,(plist-get context :session))))
           ,@params)
         (lambda (result) (funcall finish result nil))
         (lambda (reason) (funcall finish nil (format "%s" reason))))
      (error (funcall finish nil (error-message-string err))))))

(defun hermes-connectors--act (action)
  "Perform ACTION on the card at point, if it is still valid."
  (unless (hermes-connectors--available-p action)
    (user-error "No such connector action here"))
  (pcase-let* ((`(,record . ,target) (hermes-connectors--selection))
               (name (plist-get target :name))
               (op `((op_id . ,(plist-get (plist-get record :snapshot) :id))))
               (answer (lambda (status)
                         `(,@op (result . ((targets . [((name . ,name)
                                                        (status . ,status))])))))))
    (pcase-exhaustive action
      ('open (browse-url (plist-get target :connect-url)))
      ('wake (hermes-connectors--rpc record "connectors.operation.wake" op))
      ('approve (hermes-connectors--rpc record "connection.respond"
                                        (funcall answer "approved")))
      ('decline (hermes-connectors--rpc record "connection.respond"
                                        (funcall answer "skipped")))
      ('continue (hermes-connectors--rpc
                  record "connection.respond"
                  `(,@op (result . ((settled_by . "continue"))))))
      ('retry (hermes-connectors--rpc
               record "connectors.connect"
               `((connectors . [,name]) (reconnect . t))
               t)))))

(defun hermes-connectors-open ()
  "Open the authorization link of the connector at point."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'open))

(defun hermes-connectors-wake ()
  "Ask the backend to check the authorization at point now."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'wake))

(defun hermes-connectors-approve ()
  "Approve the pending install at point."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'approve))

(defun hermes-connectors-retry ()
  "Start a fresh authorization for the failed or expired connector at point."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'retry))

(defun hermes-connectors-decline ()
  "Skip the connector at point."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'decline))

(defun hermes-connectors-continue ()
  "Let the tool continue with the outcomes reached so far."
  (interactive nil hermes-chat-mode)
  (hermes-connectors--act 'continue))

(keymap-popup-define hermes-connectors-map
  "Actions for the connector card at point."
  :parent button-map
  :popup-key "?"
  :group "Connector"
  "o" ("Open link" hermes-connectors-open
       :if (lambda () (hermes-connectors--available-p 'open)))
  "g" ("Check now" hermes-connectors-wake
       :if (lambda () (hermes-connectors--available-p 'wake)))
  "a" ("Approve" hermes-connectors-approve
       :if (lambda () (hermes-connectors--available-p 'approve)))
  "r" ("Retry" hermes-connectors-retry
       :if (lambda () (hermes-connectors--available-p 'retry)))
  :group "Operation"
  "d" ("Skip" hermes-connectors-decline
       :if (lambda () (hermes-connectors--available-p 'decline)))
  "c" ("Continue" hermes-connectors-continue
       :if (lambda () (hermes-connectors--available-p 'continue))))

;;; Rendering

(defun hermes-connectors--button-action (action)
  "Return a button function performing ACTION at the button."
  (lambda (button)
    (goto-char (button-start button))
    (hermes-connectors--act action)))

(defun hermes-connectors--insert-notes (notes width &optional face)
  "Insert NOTES filled to WIDTH columns under the title, in FACE or shadow."
  (dolist (note notes)
    (let ((start (point))
          (fill-column width)
          (fill-prefix "    "))
      (insert "    " (replace-regexp-in-string "[\n\r\t ]+" " " note) "\n")
      (fill-region start (point))
      (add-face-text-property start (point) (or face 'shadow) t))))

(defun hermes-connectors--insert-buttons (actions)
  "Insert a button row for the primary ACTIONS, if any."
  (let ((buttons (seq-filter (lambda (spec) (memq (car spec) actions))
                             '((open "Authorize" "Open the authorization link")
                               (approve "Approve" "Approve this install")
                               (retry "Retry" "Start a fresh authorization")))))
    (when buttons
      (insert "   ")
      (pcase-dolist (`(,action ,label ,help) buttons)
        (insert " ")
        (insert-text-button label 'follow-link t 'face 'link 'help-echo help
                            'action (hermes-connectors--button-action action)))
      (insert "\n"))))

(defun hermes-connectors--print-target (record target width)
  "Insert RECORD's lines for TARGET, filling notes to WIDTH."
  (let ((actions (hermes-connectors--record-actions record target)))
    (insert "  " (propertize (hermes-connectors--title target) 'face 'bold))
    (when-let* ((alias (plist-get target :alias)))
      (insert (propertize (concat " · " alias) 'face 'shadow)))
    (insert "  " (hermes-connectors--status target) "\n")
    (hermes-connectors--insert-notes
     (hermes-connectors--notes target
                               (and (hermes-connectors--open-p record)
                                    (hermes-connectors--unresolved-p target)))
     width)
    (hermes-connectors--insert-buttons actions)))

(defun hermes-connectors--print-directory (record)
  "Insert RECORD's connector directory request."
  (insert "  " (propertize "Connector directory" 'face 'bold))
  (when-let* ((suggested (plist-get (plist-get record :snapshot) :suggested)))
    (insert (propertize (concat " · suggested " (string-join suggested ", "))
                        'face 'shadow)))
  (insert "\n"))

(defun hermes-connectors--print (record)
  "Insert RECORD's card with the actions valid for its owner."
  (let ((start (point))
        (width (max 30 (hermes-chat--table-window-width)))
        (targets (plist-get (plist-get record :snapshot) :targets))
        (selection (cons record nil)))
    (if (null targets)
        (hermes-connectors--print-directory record)
      (dolist (target targets)
        (let ((row (point)))
          (setq selection (cons record (plist-get target :name)))
          (hermes-connectors--print-target record target width)
          (put-text-property row (point) 'hermes-connector selection))))
    (let ((tail (point)))
      (when (seq-some (lambda (target)
                        (hermes-connectors--record-actions record target))
                      (or targets '(nil)))
        (insert "    " (propertize "?" 'face 'help-key-binding)
                (propertize " connector actions" 'face 'shadow) "\n"))
      (when (plist-get record :pending)
        (hermes-connectors--insert-notes '("Sending…") width))
      (when-let* ((error-text (plist-get record :error)))
        (hermes-connectors--insert-notes (list error-text) width 'error))
      (when (and (plist-get record :closed)
                 (not (plist-get (plist-get record :snapshot) :settled)))
        (hermes-connectors--insert-notes
         '("No longer open in this session.") width))
      (put-text-property tail (point) 'hermes-connector selection))
    (unless targets
      (put-text-property start (point) 'hermes-connector selection))
    (put-text-property start (point) 'keymap hermes-connectors-map)))

(defun hermes-connectors--print-entry (entry)
  "Print the standalone connection ENTRY."
  (when-let* ((record (hermes-connectors--record
                       (plist-get (plist-get entry :metadata) :operation-id))))
    (hermes-connectors--print record)))

(defun hermes-connectors--print-suffix (entry)
  "Append the connection cards bound to tool ENTRY."
  (when-let* ((call (hermes-connectors--entry-call entry)))
    (dolist (record (reverse hermes-connectors--operations))
      (when (equal call (plist-get (plist-get record :snapshot) :tool-call-id))
        (hermes-connectors--print record)))))

(setf (alist-get 'connection hermes-chat-entry-printers)
      #'hermes-connectors--print-entry)
(add-hook 'hermes-chat--entry-suffix-functions #'hermes-connectors--print-suffix)
(add-hook 'hermes-chat-cleanup-functions #'hermes-connectors--clear)

(provide 'hermes-connectors)
;;; hermes-connectors.el ends here
