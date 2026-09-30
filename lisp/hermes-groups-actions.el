;;; hermes-groups-actions.el --- Participate in hosted rooms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Explicit same-gateway actions only.  The server owns scheduling, member
;; sessions, approvals and durable tasks.  Lost receipts never replay writes.

;;; Code:

(require 'hermes-groups)
(require 'string-edit)

(defun hermes-groups--scope ()
  "Return this view's stable room identity, authority and roster."
  (list hermes-groups--room-id
        (and hermes-groups--room (hermes-groups--authority hermes-groups--room))
        (mapcar (lambda (member)
                  (append (mapcar (lambda (key) (hermes-transport--get member key))
                                  '(member_id profile handle display_name))
                          (let ((target (hermes-transport--get member 'target)))
                            (mapcar (lambda (key) (hermes-transport--get target key))
                                    '(kind profile peer_id installation_id)))))
                (hermes-transport--get hermes-groups--room 'members))))

(defun hermes-groups--require-active (context)
  "Refuse further input or effects when CONTEXT has retired."
  (unless (funcall (plist-get context :active))
    (user-error "Room or backend changed; reopen before trying again")))

(defun hermes-groups--read (context reader)
  "Call READER in CONTEXT, checking ownership before and after recursive input."
  (hermes-groups--require-active context)
  (save-current-buffer
    (let ((value (funcall reader)))
      (hermes-groups--require-active context)
      (hermes-browser--copy-identity value))))

(defun hermes-groups--confirm (context prompt)
  "Ask PROMPT under CONTEXT without granting authority to a replacement."
  (unless (hermes-groups--read context (lambda () (yes-or-no-p prompt)))
    (user-error "Cancelled; nothing sent")))

(defun hermes-groups--new-id ()
  "Return a new stable identity for one explicit user operation."
  (secure-hash 'sha256 (hermes-dashboard-transport--random-bytes 32)))

(defun hermes-groups--discussion-id-p (value)
  "Return non-nil if VALUE is a literal released Discussion identifier."
  (let ((case-fold-search nil))
    (and (stringp value) (<= (length value) 128)
         (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._:-]*\\'" value))))

(defun hermes-groups--write-capabilities (result method)
  "Validate RESULT for METHOD with a ready hosted driver."
  (hermes-groups--validate-capabilities result)
  (unless (and (hermes-transport--true-p (hermes-transport--get result 'driver))
               (member "idempotent_send" (hermes-transport--get result 'features))
               (member method (hermes-transport--get result 'methods)))
    (user-error "Hosted driver or %s unavailable; history remains readable" method))
  result)

(defun hermes-groups--local-room ()
  "Refuse mutation of retired or nonlocal room authority."
  (unless (and hermes-groups--room (not hermes-groups--terminal)
               (not (hermes-transport--get hermes-groups--room 'disbanded_at))
               (equal (car (hermes-groups--authority hermes-groups--room))
                      (hermes-transport--get hermes-groups--capabilities 'authority_gateway_id))
               (seq-every-p
                (lambda (member)
                  (let ((target (hermes-transport--get member 'target)))
                    (or (null target)
                        (equal (hermes-transport--get target 'kind) "local"))))
                (hermes-transport--get hermes-groups--room 'members)))
    (user-error "Select a current same-gateway room")))

(defun hermes-groups--participate (method action &optional create)
  "Run explicit METHOD using ACTION with freshly fetched capabilities and room.
CREATE means the action creates a room rather than mutating this room."
  (hermes-groups--assert-owned)
  (when hermes-groups--mutation (user-error "Room operation pending"))
  (unless (or create hermes-groups--room-id) (user-error "Open a room first"))
  (when hermes-groups--terminal (user-error "This room is retired"))
  (hermes-groups--stop-follow)
  (hermes-groups--retire-timer)
  (let* ((current (hermes-browser--mutation-context #'hermes-groups--scope))
         (buffer (current-buffer))
         (op (list :entered nil :settled nil)))
    (setq hermes-groups--mutation op hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client active)
       (let ((context (list :client client :active active :buffer buffer :operation op
                            :token hermes-dashboard-transport-request-owner)))
         (hermes-groups--then
          context (hermes-groups--rpc context "groups.capabilities" nil)
          (lambda (result)
            (setq hermes-groups--capabilities (hermes-groups--write-capabilities result method))
            (if create (funcall action context)
              (hermes-groups--then
               context (hermes-groups--state context)
               (lambda (_)
                 (hermes-groups--local-room)
                 (if (equal method "groups.approve")
                     (hermes-groups--then context (hermes-groups--approval-history context)
                                          (lambda (_) (funcall action context)))
                   (funcall action context)))))))))
     current
     (lambda (status)
       (setf (plist-get op :settled) t)
       (setq hermes-browser--status status))
     (lambda (reason)
       (setf (plist-get op :settled) t)
       (setq hermes-browser--status
             (propertize
              (if (plist-get op :entered)
                  "Receipt/readback uncertain; g refresh, R reconcile message; no automatic retry"
                "Operation not sent; try again")
              'face 'warning 'help-echo (format "%s" reason))))
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eq hermes-groups--mutation op)
             (setq hermes-groups--mutation nil)
             (when (and (funcall current) (not (plist-get op :settled)))
               (setq hermes-browser--status
                     "Monitoring stopped; backend work can continue; reconcile before retry")))))))))

(defun hermes-groups--write (context method params)
  "Dispatch METHOD and captured PARAMS once under CONTEXT."
  (hermes-groups--require-active context)
  (setf (plist-get (plist-get context :operation) :entered) t)
  (hermes-groups--rpc context method params))

(defun hermes-groups--readback (context notice)
  "Read state and log under CONTEXT, then display truthful NOTICE."
  (setq hermes-groups--notice notice)
  (hermes-groups--then
   context (hermes-groups--state context)
   (lambda (_)
     (hermes-groups--then context (hermes-groups--log context 20 nil)
                          (lambda (_) notice)))))

(defun hermes-groups--event-matches-p (event submission)
  "Return non-nil if EVENT proves exact SUBMISSION acceptance."
  (let* ((params (plist-get submission :params))
         (payload (hermes-transport--get event 'payload))
         (expected (alist-get 'payload params)))
    (and (equal (hermes-transport--get event 'room_id) (alist-get 'room_id params))
         (equal (hermes-transport--get event 'event_id)
                (concat "user:" (secure-hash 'sha256
                                            (encode-coding-string (alist-get 'event_id params) 'utf-8))))
         (equal (hermes-transport--get event 'kind) "message.user")
         (equal (hermes-transport--get (hermes-transport--get event 'actor) 'kind) "user")
         (equal (hermes-transport--get event 'authority_epoch)
                (cdr (plist-get submission :authority)))
         (equal (hermes-transport--get payload 'text) (alist-get 'text expected))
         (equal (hermes-transport--get payload 'thread_id) (alist-get 'thread_id expected)))))

(defun hermes-groups--send-submission (context submission)
  "Send retained SUBMISSION once under CONTEXT, then reconcile history."
  (setq hermes-groups--submission submission)
  (hermes-groups--then
   context (hermes-groups--write context "groups.send" (plist-get submission :params))
   (lambda (result)
     (unless (and (hermes-transport--true-p (hermes-transport--get result 'accepted))
                  (equal (hermes-transport--get result 'client_event_id)
                         (alist-get 'event_id (plist-get submission :params)))
                  (hermes-groups--event-matches-p (hermes-transport--get result 'event) submission))
       (error "Uncorrelated send receipt; reconcile history"))
     (setq hermes-groups--submission nil)
     (hermes-groups--readback context "Accepted message; member turns may still be pending"))))

;;;###autoload
(defun hermes-groups-send ()
  "Send one literal message to this hosted room, without waiting for completion."
  (interactive nil hermes-groups-mode)
  (when hermes-groups--submission (user-error "Reconcile the uncertain submission with R first"))
  (hermes-groups--participate
   "groups.send"
   (lambda (context)
     (let* ((thread (hermes-groups--read context (lambda () (read-string "Thread ID: " "main"))))
            (text (hermes-groups--read context (lambda () (read-string-from-buffer "Room message: " "")))))
       (unless (and (hermes-groups--discussion-id-p thread) (hermes-groups--id-p text)
                    (<= (string-bytes (encode-coding-string text 'utf-8)) 65536))
         (user-error "Cancelled or empty message; nothing sent"))
       (hermes-groups--confirm context (format "Send to %s on %s? "
                                              (hermes-groups--text hermes-groups--room 'name)
                                              (hermes-instance-name hermes-groups--instance)))
       (hermes-groups--send-submission
        context (list :authority (hermes-groups--authority hermes-groups--room)
                      :params `((room_id . ,hermes-groups--room-id)
                                (event_id . ,(hermes-groups--new-id))
                                (payload . ((text . ,text) (thread_id . ,thread))))))))))

;;;###autoload
(defun hermes-groups-resend ()
  "Reconcile the uncertain message against history before offering same-ID retry."
  (interactive nil hermes-groups-mode)
  (hermes-groups--assert-owned)
  (unless hermes-groups--submission (user-error "No uncertain message to reconcile"))
  (let ((submission hermes-groups--submission))
    (hermes-groups--participate
     "groups.send"
     (lambda (context)
       (unless (equal (plist-get submission :authority) (hermes-groups--authority hermes-groups--room))
         (user-error "Authority changed; retained submission cannot be retried"))
       (hermes-groups--then
        context (hermes-groups--log context 20 nil)
        (lambda (status)
          (if (seq-some (lambda (event) (hermes-groups--event-matches-p event submission))
                        hermes-groups--events)
              (progn (setq hermes-groups--submission nil)
                     "Accepted message found in log; not resent")
            (unless (equal status "Connected; history current")
              (user-error "History incomplete; continue with R before retry"))
            (hermes-groups--confirm context "Message absent from current log; retry this exact submission with the same ID? ")
            (hermes-groups--send-submission context submission))))))))

(defun hermes-groups--profiles (context)
  "Read the same backend profile catalogue under CONTEXT, including auth waits."
  (hermes-groups--then
   context
   (hermes-dashboard-transport-api-request-async
    "GET" "/api/profiles" :client (plist-get context :client) :current-p (plist-get context :active))
   (lambda (result)
     (let ((names (mapcar (lambda (row) (hermes-transport--get row 'name))
                          (hermes-transport--get result 'profiles))))
       (unless (and names (seq-every-p #'hermes-groups--discussion-id-p names))
         (error "Profile catalogue unavailable"))
       names))))

(defun hermes-groups--read-roster (context names)
  "Select two to six distinct existing profile NAMES under CONTEXT."
  (let ((count (hermes-groups--read context (lambda () (read-number "Members (2–6): " 2)))))
    (unless (and (integerp count) (<= 2 count 6)) (user-error "Choose two to six members"))
    (let (selected)
      (cl-loop for n from 1 to count collect
               (let* ((available (seq-remove (lambda (name) (member (downcase name) selected)) names))
                      (profile (hermes-groups--read
                                context (lambda () (completing-read "Profile: " available nil t)))))
                 (unless (member profile available) (user-error "Select an existing distinct profile"))
                 (push (downcase profile) selected)
                 `((member_id . ,(format "member-%s" n)) (profile . ,profile)
                   (handle . ,(format "member%s" n)) (display_name . ,profile)))))))

(defun hermes-groups--create-room (context name members)
  "Create NAME with frozen MEMBERS under CONTEXT after catalogue revalidation."
  (let ((id (hermes-groups--new-id)))
    (hermes-groups--then
     context (hermes-groups--profiles context)
     (lambda (names)
       (unless (seq-every-p (lambda (member) (member (alist-get 'profile member) names)) members)
         (error "Selected profile no longer exists"))
       (hermes-groups--then
        context (hermes-groups--write context "groups.create"
                                     `((room_id . ,id) (name . ,name) (members . ,(vconcat members))))
        (lambda (result)
          (unless (equal id (hermes-transport--get (hermes-transport--get result 'room) 'room_id))
            (error "Uncorrelated create receipt; inspect the room list before retry"))
          (hermes-groups--then
           context (hermes-groups--rpc context "groups.state" `((room_id . ,id)))
           (lambda (state)
             (let ((room (hermes-groups--validate-room (hermes-transport--get state 'room))))
               (unless (and (equal id (hermes-transport--get room 'room_id))
                            (equal name (hermes-transport--get room 'name))
                            (equal (car (hermes-groups--authority room))
                                   (hermes-transport--get hermes-groups--capabilities 'authority_gateway_id))
                            (hermes-groups--roster-matches-p
                             (hermes-transport--get room 'members) members))
                 (error "Room readback differs from selected roster")))
             (hermes-groups--then context (hermes-groups--list context 0)
                                  (lambda (_) "Room created and read back; open it with RET"))))))))))

(defun hermes-groups--room-name-p (name)
  "Return non-nil if NAME survives the backend's boundary whitespace trimming."
  (let ((whitespace '(9 10 11 12 13 28 29 30 31 32 133 160 5760
                       8192 8193 8194 8195 8196 8197 8198 8199 8200 8201 8202
                       8232 8233 8239 8287 12288)))
    (and (hermes-groups--id-p name) (<= (length name) 200)
         (not (memq (aref name 0) whitespace))
         (not (memq (aref name (1- (length name))) whitespace)))))

;;;###autoload
(defun hermes-groups-create ()
  "Create a small room from existing profiles on this backend."
  (interactive nil hermes-groups-mode)
  (when hermes-groups--room-id (user-error "Create rooms from the room list"))
  (hermes-groups--participate
   "groups.create"
   (lambda (context)
     (hermes-groups--then
      context (hermes-groups--profiles context)
      (lambda (names)
        (let* ((name (hermes-groups--read context (lambda () (read-string "Room name: "))))
               (members (hermes-groups--read-roster context names)))
          (unless (hermes-groups--room-name-p name)
            (user-error "Use a room name of 1–200 characters without boundary whitespace"))
          (hermes-groups--confirm
           context (format "Create %s on %s with %s? " name
                           (hermes-instance-name hermes-groups--instance)
                           (mapconcat (lambda (row) (alist-get 'profile row)) members ", ")))
          (hermes-groups--create-room context name members))))) t))

(defun hermes-groups--action-id (action)
  "Return ACTION's exact approval execution identity."
  (mapcar (lambda (field) (hermes-transport--get action field))
          '(member_id task_id execution_generation request_id)))

(defun hermes-groups--roster-matches-p (actual expected)
  "Return non-nil when ACTUAL preserves every selected EXPECTED member field."
  (and (= (length actual) (length expected))
       (cl-every
        (lambda (row selected)
          (and (seq-every-p
                (lambda (pair) (equal (hermes-transport--get row (car pair)) (cdr pair))) selected)
               (let ((target (hermes-transport--get row 'target)))
                 (or (null target)
                     (and (equal (hermes-transport--get target 'kind) "local")
                          (equal (hermes-transport--get target 'profile) (alist-get 'profile selected)))))))
        actual expected)))

(defvar-local hermes-groups--approval-proof nil
  "Driver and cursor snapshot backed by a complete approval history read.")

(defun hermes-groups--approval-snapshot ()
  "Return the current backend task evidence identity."
  (list hermes-groups--room-id (hermes-groups--authority hermes-groups--room)
        hermes-groups--driver hermes-groups--cursor
        (hermes-transport--get hermes-groups--room 'latest_seq)))

(defun hermes-groups--task-events ()
  "Return validated backend task retirement events, refusing ambiguous evidence."
  (seq-filter
   (lambda (event)
     (when (member (hermes-transport--get event 'kind)
                   '("turn.cancelled" "turn.failed" "turn.settled" "turn.deferred"))
       (let ((payload (hermes-transport--get event 'payload))
             (actor (hermes-transport--get event 'actor)))
         (unless (and (hermes-groups--event-valid-p event hermes-groups--room)
                      (equal (hermes-transport--get actor 'kind) "gateway")
                      (equal (hermes-transport--get actor 'id)
                             (car (hermes-groups--authority hermes-groups--room)))
                      (equal (hermes-transport--get event 'authority_epoch)
                             (cdr (hermes-groups--authority hermes-groups--room)))
                      (hermes-groups--id-p (hermes-transport--get payload 'task_id))
                      (hermes-groups--id-p (hermes-transport--get payload 'member_id))
                      (or (not (equal (hermes-transport--get event 'kind) "turn.deferred"))
                          (let ((generation (hermes-transport--get payload 'execution_generation)))
                            (and (integerp generation) (> generation 0)))))
           (user-error "Task history authority incomplete; refresh before approval")))
       t))
   hermes-groups--events))

(defun hermes-groups--approval-history (context)
  "Read complete task-qualified approval evidence under CONTEXT."
  (setq hermes-groups--approval-proof nil)
  (hermes-groups--then
   context (hermes-groups--log context 20 nil)
   (lambda (status)
     (unless (and (equal status "Connected; history current")
                  (hermes-groups--nat-p (hermes-transport--get hermes-groups--room 'latest_seq))
                  (>= hermes-groups--cursor (hermes-transport--get hermes-groups--room 'latest_seq)))
       (user-error "Task history incomplete; refresh before approval"))
     (let* ((events (hermes-groups--task-events))
            (terminal-tasks
             (delete-dups
              (mapcar (lambda (event) (hermes-transport--get
                                      (hermes-transport--get event 'payload) 'task_id))
                      (seq-remove (lambda (event) (equal (hermes-transport--get event 'kind) "turn.deferred"))
                                  events))))
            (counts (hermes-transport--get hermes-groups--driver 'counts))
            (final-counts (mapcar (lambda (key) (or (hermes-transport--get counts key) 0))
                                  '(cancelled failed settled))))
       ;; A lower bound detects unpublished outcomes, not completeness: task
       ;; pruning leaves older log events, and state/log are not atomic.
       (unless (and (seq-every-p #'hermes-groups--nat-p final-counts)
                    (<= (apply #'+ final-counts) (length terminal-tasks)))
         (user-error "Task outcomes not yet published; refresh before approval")))
     (setq hermes-groups--approval-proof
           (hermes-browser--copy-identity (hermes-groups--approval-snapshot))))))

(defun hermes-groups--approval-retired-p (action)
  "Return non-nil if backend evidence retires ACTION's task or execution."
  (let ((task (hermes-transport--get action 'task_id))
        (generation (hermes-transport--get action 'execution_generation)))
    (or (member (hermes-groups--action-id action) hermes-groups--retired-approvals)
        (seq-some (lambda (row) (equal task (hermes-transport--get row 'task_id)))
                  (hermes-groups--actions "retry"))
        (seq-some
         (lambda (event)
           (let ((payload (hermes-transport--get event 'payload)))
             (and (equal task (hermes-transport--get payload 'task_id))
                  (or (not (equal (hermes-transport--get event 'kind) "turn.deferred"))
                      (not (integerp generation))
                      (<= generation (hermes-transport--get payload 'execution_generation))))))
         (hermes-groups--task-events)))))

(defun hermes-groups--actions (kind)
  "Return pending rows of KIND backed by the owning backend's task evidence."
  (seq-filter
   (lambda (action)
     (and (equal (hermes-transport--get action 'kind) kind)
          (or (equal kind "retry")
              (and (equal hermes-groups--approval-proof (hermes-groups--approval-snapshot))
                   (hermes-transport--true-p (hermes-transport--get hermes-groups--driver 'working))
                   ;; Stopping has no task-qualified published row until settlement.
                   (zerop (or (hermes-transport--get
                               (hermes-transport--get hermes-groups--driver 'counts) 'stopping) 0))
                   (not (hermes-groups--approval-retired-p action))))))
   (hermes-transport--get hermes-groups--driver 'pending_actions)))

(defun hermes-groups--select-action (context kind)
  "Select a backend-published pending action of KIND under CONTEXT."
  (let* ((rows (hermes-groups--actions kind))
         (choices (cl-loop for row in rows for n from 1 collect
                           (cons (format "%s: %s · %s" n (hermes-groups--text row 'member_id)
                                         (hermes-groups--text row 'task_id)) row))))
    (unless choices (user-error "No safely actionable %s; refresh backend state" kind))
    (let* ((choice (hermes-groups--read context (lambda () (completing-read "Pending task: " choices nil t))))
           (action (cdr (assoc choice choices))))
      (unless action (user-error "Select a pending task"))
      (hermes-browser--copy-identity action))))

(defun hermes-groups--action-current-p (action kind)
  "Return non-nil if exact ACTION remains available under KIND."
  (member action (hermes-groups--actions kind)))

(defun hermes-groups--action-write (context action kind method params)
  "Re-read ACTION of KIND before METHOD with PARAMS under CONTEXT."
  (unless (hermes-groups--action-current-p action kind)
    (user-error "Pending request changed during input; nothing sent"))
  (hermes-groups--then
   context (hermes-groups--then
            context (hermes-groups--state context)
            (lambda (_)
              (if (equal kind "approval") (hermes-groups--approval-history context)
                (hermes--promise-resolved nil))))
   (lambda (_)
     (let* ((active (plist-get context :active))
            (buffer (current-buffer))
            (guard (lambda () (and (funcall active)
                                    (with-current-buffer buffer
                                      (hermes-groups--action-current-p action kind)))))
            (write-context (plist-put (copy-sequence context) :active guard)))
       (unless (funcall guard) (user-error "Pending request changed; nothing sent"))
       (hermes-groups--then
        context (hermes-groups--write write-context method params)
        (lambda (receipt)
          (hermes-groups--readback context (hermes-groups--action-receipt method params receipt))))))))

(defun hermes-groups--action-receipt (method params receipt)
  "Describe correlated RECEIPT for METHOD and PARAMS without inferring completion."
  (if (equal method "groups.approve")
      (let ((resolved (hermes-transport--get (hermes-transport--get receipt 'result) 'resolved)))
        (unless (and (hermes-transport--true-p (hermes-transport--get receipt 'approved))
                     (hermes-groups--nat-p resolved))
          (error "Uncertain approval receipt; refresh before acting"))
        (if (zerop resolved) "Approval unresolved; inspect fresh state"
          "Approval response handled; member turn may still be pending"))
    (let ((task (hermes-transport--get receipt 'task)))
      (unless (and (hermes-transport--true-p (hermes-transport--get receipt 'retried))
                   (equal (hermes-transport--get task 'room_id) (alist-get 'room_id params))
                   (equal (hermes-transport--get task 'task_id) (alist-get 'task_id params)))
        (error "Uncorrelated retry receipt; inspect task state"))
      "Retry request handled; task completion remains backend-owned")))

;;;###autoload
(defun hermes-groups-approve ()
  "Answer one exact room/member/task/execution/request approval."
  (interactive nil hermes-groups-mode)
  (hermes-groups--participate
   "groups.approve"
   (lambda (context)
     (let* ((action (hermes-groups--select-action context "approval"))
            (identity (hermes-groups--action-id action))
            (approval (hermes-transport--get action 'approval))
            (choice (hermes-groups--read
                     context (lambda () (completing-read
                                         (format "%s\n%s\nDecision: " (hermes-groups--text approval 'command)
                                                 (hermes-groups--text approval 'description))
                                         '("once" "deny") nil t)))))
       (unless (and (seq-every-p #'hermes-groups--id-p
                                 (list (nth 0 identity) (nth 1 identity) (nth 3 identity)))
                    (integerp (nth 2 identity)) (> (nth 2 identity) 0)
                    (member choice '("once" "deny")))
         (user-error "Invalid approval identity or decision"))
       (hermes-groups--action-write
        context action "approval" "groups.approve"
        `((room_id . ,hermes-groups--room-id) (member_id . ,(nth 0 identity))
          (task_id . ,(nth 1 identity)) (execution_generation . ,(nth 2 identity))
          (request_id . ,(nth 3 identity)) (choice . ,choice)))))))

;;;###autoload
(defun hermes-groups-retry ()
  "Explicitly retry one named uncertain task, including backend-deferred work."
  (interactive nil hermes-groups-mode)
  (hermes-groups--participate
   "groups.retry"
   (lambda (context)
     (let* ((action (hermes-groups--select-action context "retry"))
            (task (hermes-transport--get action 'task_id)))
       (unless (hermes-groups--id-p task) (user-error "Malformed task identity"))
       (hermes-groups--confirm
        context (format "Retry uncertain task %s in %s? Prior execution is uncertain and may have had effects. "
                        task (hermes-groups--text hermes-groups--room 'name)))
       (hermes-groups--action-write context action "retry" "groups.retry"
                                    `((room_id . ,hermes-groups--room-id) (task_id . ,task)))))))

;;;###autoload
(defun hermes-groups-stop ()
  "Request durable cancellation for this room and read its state back.
This does not terminate the gateway or guarantee global process termination."
  (interactive nil hermes-groups-mode)
  (hermes-groups--participate
   "groups.stop"
   (lambda (context)
     (let ((params `((room_id . ,hermes-groups--room-id) (cancel_id . ,(hermes-groups--new-id)))))
       (hermes-groups--confirm context (format "Stop work in %s only? " (hermes-groups--text hermes-groups--room 'name)))
       (setq hermes-groups--retired-approvals
             (append (mapcar #'hermes-groups--action-id
                             (seq-filter (lambda (row) (equal (hermes-transport--get row 'kind) "approval"))
                                         (hermes-transport--get hermes-groups--driver 'pending_actions)))
                     hermes-groups--retired-approvals))
       (hermes-groups--then
        context (hermes-groups--write context "groups.stop" params)
        (lambda (receipt)
          (unless (hermes-groups--nat-p (hermes-transport--get receipt 'cancelled))
            (error "Uncertain stop receipt; inspect room state"))
          (hermes-groups--readback context "Stop requested for this room; gateway and other work may continue")))))))

(provide 'hermes-groups-actions)
;;; hermes-groups-actions.el ends here
