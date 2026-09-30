;;; hermes-groups.el --- Read hosted Group Chats -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read-only protocol-v2 hosted rooms.  The gateway owns room identity,
;; authority, tasks and retirement; this view never runs an orchestrator.
;; Public messages and typed activity are inert text, not chat events.

;;; Code:

(require 'hermes-buffer)
(require 'hermes-browser)
(require 'hermes-dashboard-transport)

(defvar-local hermes-groups--instance nil "Immutable backend identity for this view.")
(defvar-local hermes-groups--room-id nil "Exact room being read, or nil for the list.")
(defvar-local hermes-groups--room nil "Last rendered room state.")
(defvar-local hermes-groups--driver nil "Last returned room driver status.")
(defvar-local hermes-groups--capabilities nil "Last validated capability response.")
(defvar-local hermes-groups--events nil "Validated, rendered events in sequence order.")
(defvar-local hermes-groups--cursor 0 "Last successfully rendered log cursor.")
(defvar-local hermes-groups--next nil "Returned next list offset, or nil.")
(defvar-local hermes-groups--rooms nil "Accepted list page.")
(defvar-local hermes-groups--terminal nil "Permanent room retirement: disbanded or expired.")
(defvar-local hermes-groups--notice nil "Explicit reconciliation notice.")
(defvar-local hermes-groups--follow nil "Non-nil means refresh this room while owned.")
(defvar-local hermes-groups--timer-cleanup nil "Owned one-shot refresh timer cleanup.")
(defvar-local hermes-groups--follow-cleanup nil "Exact persistent Follow lease cleanup.")
(defvar-local hermes-groups--failures 0 "Consecutive automatic read failures.")

(define-error 'hermes-groups-reconcile "Room log requires state reconciliation")

(defun hermes-groups--text (object key)
  "Return literal OBJECT field KEY as text, or an explicit unknown value."
  (let ((value (hermes-transport--get object key)))
    (if (stringp value) (substring-no-properties value) "unknown")))

(defun hermes-groups--id-p (value)
  "Return non-nil if VALUE is a nonempty wire identifier."
  (and (stringp value) (not (string-empty-p value))))

(defun hermes-groups--nat-p (value)
  "Return non-nil if VALUE is a nonnegative integer."
  (and (integerp value) (>= value 0)))

(defun hermes-groups--authority (room)
  "Return ROOM's gateway and epoch pair."
  (cons (hermes-transport--get room 'authority_gateway_id)
        (hermes-transport--get room 'authority_epoch)))

(defun hermes-groups--validate-room (room)
  "Validate ROOM's required identity and presentation fields."
  (unless (and (hermes-groups--id-p (hermes-transport--get room 'room_id))
               (stringp (hermes-transport--get room 'name))
               (listp (hermes-transport--get room 'members))
               (hermes-groups--id-p (car (hermes-groups--authority room)))
               (integerp (cdr (hermes-groups--authority room)))
               (> (cdr (hermes-groups--authority room)) 0))
    (error "Malformed hosted room"))
  room)

(defun hermes-groups--validate-capabilities (result)
  "Require the read-only protocol contract in RESULT, not driver readiness."
  (unless (and (eql (hermes-transport--get result 'protocol_version) 2)
               (hermes-groups--id-p (hermes-transport--get result 'authority_gateway_id))
               (seq-every-p
                (lambda (feature) (member feature (hermes-transport--get result 'features)))
                '("authority_epoch" "room_identity" "monotonic_log" "typed_events"
                  "actor_identity" "replayable_disband"))
               (seq-every-p
                (lambda (method) (member method (hermes-transport--get result 'methods)))
                '("groups.capabilities" "groups.list" "groups.state" "groups.log"))
               (integerp (hermes-transport--get result 'max_log_limit))
               (> (hermes-transport--get result 'max_log_limit) 0))
    (error "Hosted room reading requires protocol 2 and its advertised read methods/features"))
  result)

(defun hermes-groups--event-valid-p (event room)
  "Return non-nil if EVENT has an inert display envelope for ROOM."
  (let ((kind (hermes-transport--get event 'kind))
        (actor (hermes-transport--get event 'actor))
        (epoch (hermes-transport--get event 'authority_epoch))
        (payload (hermes-transport--get event 'payload)))
    (and (equal (hermes-transport--get event 'room_id)
                (hermes-transport--get room 'room_id))
         (hermes-groups--id-p (hermes-transport--get event 'event_id))
         (integerp (hermes-transport--get event 'seq))
         (> (hermes-transport--get event 'seq) 0)
         (hermes-groups--id-p kind)
         (hermes-groups--id-p (hermes-transport--get actor 'kind))
         (hermes-groups--id-p (hermes-transport--get actor 'id))
         (numberp (hermes-transport--get event 'created_at))
         (or (null epoch) (and (integerp epoch) (> epoch 0)
                              (<= epoch (cdr (hermes-groups--authority room)))))
         (hermes-transport--object-p payload)
         (or (not (member kind '("message.user" "message.member")))
             (stringp (hermes-transport--get payload 'text))))))

(defun hermes-groups--page-events (page room accepted cursor)
  "Validate PAGE for ROOM after ACCEPTED events and CURSOR.
Return only unseen events.  Conflicts and gaps require explicit state
reconciliation; never synthesize a cursor or infer continuity from text."
  (let* ((authority (hermes-transport--get page 'authority))
         (events (hermes-transport--get page 'events))
         (end (hermes-transport--get page 'cursor))
         (latest (hermes-transport--get page 'latest_seq))
         (more (if (hermes-transport--field-present-p page 'has_more)
                   (hermes-transport--get page 'has_more) 'missing))
         (seqs (make-hash-table :test #'eql))
         (ids (make-hash-table :test #'equal))
         (next cursor) unseen)
    (unless (and (equal (hermes-groups--authority room)
                        (cons (hermes-transport--get authority 'gateway_id)
                              (hermes-transport--get authority 'epoch)))
                 (hermes-transport--field-present-p page 'events)
                 (listp events) (hermes-groups--nat-p end)
                 (hermes-groups--nat-p latest) (>= end cursor) (>= latest end)
                 (memq more '(nil :false t))
                 (eq (hermes-transport--true-p more) (< end latest)))
      (signal 'hermes-groups-reconcile '("Invalid page authority or cursor envelope")))
    (dolist (event accepted)
      (puthash (hermes-transport--get event 'seq) event seqs)
      (puthash (hermes-transport--get event 'event_id) event ids))
    (dolist (event events)
      (unless (hermes-groups--event-valid-p event room)
        (signal 'hermes-groups-reconcile '("Malformed room event")))
      (let* ((seq (hermes-transport--get event 'seq))
             (id (hermes-transport--get event 'event_id))
             (old (gethash seq seqs)))
        (cond
         ((and old (equal old event)))
         ((or old (gethash id ids) (/= seq (1+ next)))
          (signal 'hermes-groups-reconcile '("Conflicting replay or sequence gap")))
         (t (push event unseen)
            (setq next seq)
            (puthash seq event seqs)
            (puthash id event ids)))))
    (unless (and (= next end) (or (not (hermes-transport--true-p more)) (> end cursor)))
      (signal 'hermes-groups-reconcile '("Non-progressing or unrendered cursor")))
    (nreverse unseen)))

(defun hermes-groups--retire-timer ()
  "Cancel the exact refresh timer without releasing the Follow lease."
  (when hermes-groups--timer-cleanup (funcall hermes-groups--timer-cleanup)))

(defun hermes-groups--retire ()
  "Retire pending requests and automatic refresh before losing this view."
  (hermes-groups--stop-follow)
  (hermes-groups--retire-timer)
  (hermes-browser--retire-owned))

(defun hermes-groups--header ()
  "Return separate connection, driver and local read state."
  (concat
   (propertize "Group Chat · read-only · Last read: " 'face 'shadow)
   (propertize (or hermes-browser--status "Not fetched") 'face 'font-lock-type-face)
   (propertize " · Driver: " 'face 'shadow)
   (propertize
    (if (null hermes-groups--capabilities) "unknown"
      (if (hermes-transport--true-p (hermes-transport--get hermes-groups--capabilities 'driver))
          "ready" "unavailable (history readable)"))
    'face 'font-lock-constant-face)
   (propertize (if hermes-groups--follow " · Follow: on" " · Follow: off")
               'face 'shadow)))

(defvar-keymap hermes-groups-mode-map
  :parent special-mode-map
  "g" #'hermes-groups-refresh "m" #'hermes-groups-more
  "RET" #'hermes-groups-view "f" #'hermes-groups-view
  "F" #'hermes-groups-follow "b" #'quit-window
  "n" #'next-line "p" #'previous-line)

(keymap-popup-annotate hermes-groups-mode-map
  :popup-key "?" :exit-key "C-g" :description "Hosted Group Chats (read-only)"
  :group "Read"
  hermes-groups-view "Open room" hermes-groups-refresh "Refresh / reconnect"
  hermes-groups-more "Next room page" hermes-groups-follow "Follow activity"
  :group "Navigate"
  next-line "Next line" previous-line "Previous line" quit-window "Back")

(define-derived-mode hermes-groups-mode special-mode "Hermes Groups"
  "Read hosted rooms without sending messages or managing room workers."
  (setq-local header-line-format '(:eval (hermes-groups--header)))
  (add-hook 'kill-buffer-hook #'hermes-groups--retire nil t)
  (add-hook 'change-major-mode-hook #'hermes-groups--retire nil t)
  (add-hook 'after-set-visited-file-name-hook #'hermes-groups--retire nil t))

(defun hermes-groups--assert-owned ()
  "Refuse fresh commands in a retired or unclaimed view."
  (unless (and (hermes-buffer--owned-p 'hermes-groups-mode)
               (equal hermes-instance hermes-groups--instance)
               (equal (hermes-instance-context) hermes-groups--instance))
    (user-error "Reopen the hosted room view")))

(defun hermes-groups--print-text (text)
  "Replace this owned buffer's text with prepared TEXT."
  (erase-buffer)
  (insert text))

(defun hermes-groups--install-text (text active)
  "Render TEXT transactionally under the original read predicate ACTIVE.
Roll back failed native notifications only while this view still owns the
buffer.  A successor's edits are not ours to undo, even on error or quit."
  (unless (funcall active) (error "Retired hosted room read"))
  (let ((owner (hermes-browser--owned-predicate '(hermes-groups--room-id)))
        (tick (buffer-modified-tick))
        (group (prepare-change-group)) accepted)
    (hermes-browser--preserve-reading-position
     (lambda ()
       (unwind-protect
           (progn
             (activate-change-group group)
             ;; Emacs 29's change combiner needs a nonempty undo tail even
             ;; when activating a transaction in an undo-disabled view.
             (unless buffer-undo-list (push nil buffer-undo-list))
             (let ((inhibit-read-only t))
               (combine-change-calls (point-min) (point-max)
                 (unless (and (funcall active) (= tick (buffer-modified-tick)))
                   (error "Room read changed before rendering"))
                 (let ((inhibit-modification-hooks t))
                   (hermes-groups--print-text text))))
             (unless (and (funcall active)
                          (equal-including-properties text (buffer-string)))
               (error "Room read changed during rendering"))
             (setq accepted t))
         (let ((inhibit-read-only t) (inhibit-modification-hooks t))
           (if (or accepted (not (funcall owner)))
               (accept-change-group group)
             (cancel-change-group group))))))
    (unless (funcall active) (error "Retired hosted room read"))))

(defun hermes-groups--list-text (rooms next)
  "Format ROOMS and returned NEXT page marker as native selectable rows."
  (concat (propertize "Hosted rooms (including disbanded history)\n\n" 'face 'bold)
          (if rooms
              (mapconcat
               (lambda (room)
                 (propertize
                  (format "%s  [%s]\n" (hermes-groups--text room 'name)
                          (if (hermes-transport--get room 'disbanded_at) "disbanded" "hosted"))
                  'hermes-group-room room 'face 'link))
               rooms "")
            "No rooms on this page.\n")
          (if next "\nm: next page (backend offset)\n" "\nEnd of room list.\n")))

(defun hermes-groups--event-text (event)
  "Format EVENT as literal public text or a distinct typed activity record."
  (let* ((kind (hermes-groups--text event 'kind))
         (actor (hermes-transport--get event 'actor))
         (payload (hermes-transport--get event 'payload))
         (speaker (or (hermes-transport--get actor 'display_name)
                      (hermes-transport--get actor 'id)))
         (public (member kind '("message.user" "message.member"))))
    (concat
     (propertize (format "#%s %s · %s\n" (hermes-transport--get event 'seq)
                         speaker kind)
                 'face (if public 'font-lock-type-face 'shadow))
     (if public (concat (hermes-groups--text payload 'text) "\n\n")
       (format "  Member: %s · Task: %s · Status: %s\n  %s\n\n"
               (hermes-groups--text payload 'member_id)
               (hermes-groups--text payload 'task_id)
               (hermes-groups--text payload 'status)
               (or (hermes-transport--get payload 'error)
                   (hermes-transport--get payload 'reason) ""))))))

(defun hermes-groups--detail-text (room driver events terminal notice)
  "Format ROOM, DRIVER, EVENTS, TERMINAL and NOTICE without executing payloads."
  (let ((actions (hermes-transport--get driver 'pending_actions)))
    (concat
     (propertize (concat (hermes-groups--text room 'name) "\n") 'face 'bold)
     (format "Room: %s\nAuthority: %s · Epoch: %s\n"
             (hermes-groups--text room 'room_id) (car (hermes-groups--authority room))
             (cdr (hermes-groups--authority room)))
     (propertize (format "History: %s\n%s\n" (or terminal "hosted") (or notice ""))
                 'face (if (or terminal notice) 'warning 'shadow))
     (propertize "Member activity / task status\n" 'face 'bold)
     (format "Worker: %s · Work: %s · Blocked: %s\n"
             (hermes-groups--boolean driver 'running)
             (hermes-groups--boolean driver 'working)
             (hermes-groups--boolean driver 'blocked))
     (format "Task counts (backend): %S\n" (or (hermes-transport--get driver 'counts) 'unknown))
     (mapconcat (lambda (action)
                  (format "Pending %s · Member: %s · Task: %s\n"
                          (hermes-groups--text action 'kind)
                          (hermes-groups--text action 'member_id)
                          (hermes-groups--text action 'task_id))) actions "")
     (mapconcat #'hermes-groups--event-text
                (seq-remove (lambda (event) (member (hermes-transport--get event 'kind)
                                                   '("message.user" "message.member"))) events) "")
     (propertize "\nPublic messages (chronological)\n\n" 'face 'bold)
     (mapconcat #'hermes-groups--event-text
                (seq-filter (lambda (event) (member (hermes-transport--get event 'kind)
                                                   '("message.user" "message.member"))) events) ""))))

(defun hermes-groups--boolean (object key)
  "Describe OBJECT's boolean KEY without treating absence as false."
  (pcase (if (hermes-transport--field-present-p object key)
             (hermes-transport--get object key) 'missing)
    ('t "yes") ((or 'nil :false) "no") (_ "unknown")))

(defun hermes-groups--accept-state (result active)
  "Render RESULT under ACTIVE before publishing identity and cursor reset."
  (let* ((room (hermes-groups--validate-room (hermes-transport--get result 'room)))
         (old (and hermes-groups--room (hermes-groups--authority hermes-groups--room)))
         (new (hermes-groups--authority room))
         (changed (and old (not (equal old new))))
         (terminal (or hermes-groups--terminal
                       (and (hermes-transport--get room 'disbanded_at) 'disbanded)))
         (events (unless changed hermes-groups--events))
         (notice (if changed "Authority changed; replaying history from sequence zero."
                   hermes-groups--notice))
         (driver (hermes-transport--get result 'driver_status)))
    (unless (equal hermes-groups--room-id (hermes-transport--get room 'room_id))
      (error "State belongs to another room"))
    (when (or (and changed (<= (cdr new) (cdr old)))
              (and hermes-groups--terminal (not (hermes-transport--get room 'disbanded_at))))
      (error "Regressed authority or retired room state"))
    (hermes-groups--install-text (hermes-groups--detail-text room driver events terminal notice) active)
    (setq hermes-groups--room room hermes-groups--driver driver
          hermes-groups--events events hermes-groups--terminal terminal
          hermes-groups--notice notice)
    (when terminal (setq hermes-groups--follow nil))
    (when changed (setq hermes-groups--cursor 0))))

(defun hermes-groups--accept-log (page active)
  "Render validated PAGE under ACTIVE before advancing the accepted cursor."
  (let* ((unseen (hermes-groups--page-events
                  page hermes-groups--room hermes-groups--events hermes-groups--cursor))
         (events (append hermes-groups--events unseen))
         (terminal (or hermes-groups--terminal
                       (and (seq-some (lambda (event)
                                        (equal (hermes-transport--get event 'kind) "room.disbanded"))
                                      unseen) 'disbanded))))
    (hermes-groups--install-text
     (hermes-groups--detail-text hermes-groups--room hermes-groups--driver events
                                 terminal hermes-groups--notice) active)
    (setq hermes-groups--events events hermes-groups--terminal terminal
          hermes-groups--cursor (hermes-transport--get page 'cursor))
    (when terminal (setq hermes-groups--follow nil))))

(defun hermes-groups--rpc (context method params)
  "Read METHOD with PARAMS under the exact CONTEXT through deferred readiness."
  (unless (funcall (plist-get context :active)) (error "Retired hosted room read"))
  (let ((hermes-dashboard-transport-dispatch-guard (plist-get context :active))
        (hermes-dashboard-transport-request-owner (plist-get context :token))
        (hermes-dashboard-transport-request-structured-error t))
    (hermes-dashboard-transport-call (plist-get context :client) method params)))

(defun hermes-groups--then (context promise function)
  "Run FUNCTION for PROMISE only in the current CONTEXT's owning buffer."
  (hermes--promise-then promise
    (lambda (value)
      (unless (funcall (plist-get context :active)) (error "Retired hosted room read"))
      (with-current-buffer (plist-get context :buffer) (funcall function value)))))

(defun hermes-groups--state (context)
  "Read and reconcile the current room state under CONTEXT."
  (hermes-groups--then
   context (hermes-groups--rpc context "groups.state"
                              `((room_id . ,hermes-groups--room-id) (include_disbanded . t)))
   (lambda (result) (hermes-groups--accept-state result (plist-get context :active)))))

(defun hermes-groups--log (context budget reconciled)
  "Read at most BUDGET pages under CONTEXT; RECONCILED bounds state recovery."
  (hermes-groups--then
   context
   (hermes-groups--rpc context "groups.log"
                      `((room_id . ,hermes-groups--room-id) (since_seq . ,hermes-groups--cursor)
                        (limit . ,(min 100 (hermes-transport--get hermes-groups--capabilities 'max_log_limit)))
                        (include_disbanded . t)))
   (lambda (page)
     (condition-case err
         (progn
           (hermes-groups--accept-log page (plist-get context :active))
           (if (hermes-transport--true-p (hermes-transport--get page 'has_more))
               (if (> budget 1) (hermes-groups--log context (1- budget) reconciled)
                 "Partial history; g continues")
             "Connected; history current"))
       (hermes-groups-reconcile
        (if reconciled (signal (car err) (cdr err))
          (setq hermes-groups--notice "Log discontinuity; reconciling state without skipping events.")
          (hermes-groups--then context (hermes-groups--state context)
                               (lambda (_) (hermes-groups--log context budget t)))))))))

(defun hermes-groups--list (context offset)
  "Read one list page at the exact returned OFFSET under CONTEXT."
  (hermes-groups--then
   context (hermes-groups--rpc context "groups.list"
                              `((offset . ,offset) (limit . 100) (include_disbanded . t)))
   (lambda (result)
     (let ((rooms (if (hermes-transport--field-present-p result 'rooms)
                      (hermes-transport--get result 'rooms) 'missing))
           (next (hermes-transport--get result 'next_offset)))
       (unless (and (listp rooms) (hermes-transport--field-present-p result 'next_offset)
                    (or (null next) (and (integerp next) (> next offset))))
         (error "Malformed hosted room list page"))
       (mapc #'hermes-groups--validate-room rooms)
       (hermes-groups--install-text (hermes-groups--list-text rooms next)
                                    (plist-get context :active))
       (setq hermes-groups--rooms rooms hermes-groups--next next)
       "Connected; room list current"))))

(defun hermes-groups--failure (reason active)
  "Mark REASON under ACTIVE while preserving the last rendered cursor."
  (cl-incf hermes-groups--failures)
  (if (equal (hermes-transport--get (hermes-transport--get reason 'data) 'reason)
             "room_history_expired")
      (progn
        (hermes-groups--install-text
         (hermes-groups--detail-text hermes-groups--room nil hermes-groups--events
                                     'expired "Backend history expired; retained text is a local snapshot.")
         active)
        (setq hermes-groups--terminal 'expired hermes-groups--follow nil)
        (setq hermes-browser--status "History expired; room permanently retired"))
    (setq hermes-browser--status
          (propertize "Read failed; retained snapshot stale; g retry" 'face 'warning
                      'help-echo (format "%s" reason))))
  (when (>= hermes-groups--failures 3) (setq hermes-groups--follow nil)))

(defun hermes-groups--stop-follow ()
  "Release the exact Follow lease and retire its timer and pending read."
  (setq hermes-groups--follow nil)
  (when hermes-groups--follow-cleanup (funcall hermes-groups--follow-cleanup)))

(defun hermes-groups--retain-follow (client active)
  "Retain CLIENT across Follow timers while ACTIVE owns the current read."
  (when (and hermes-groups--follow (not hermes-groups--follow-cleanup))
    (let* ((buffer (current-buffer))
           (hermes-dashboard-transport-url (hermes-instance-url hermes-groups--instance))
           (lease (hermes-dashboard-transport-acquire :callback #'ignore))
           subscription cleanup closed)
      (setq cleanup
            (lambda ()
              (unless closed
                (setq closed t)
                (when subscription (hermes-dashboard-transport-unsubscribe lease subscription))
                (unwind-protect
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (when (eq hermes-groups--follow-cleanup cleanup)
                          (setq hermes-groups--follow nil)
                          (hermes-groups--retire-timer)
                          (setq hermes-groups--follow-cleanup nil)
                          (hermes-browser--retire-owned))))
                  (hermes-dashboard-transport-release lease)))))
      (if (not (and (eq client lease) (funcall active)))
          (progn (funcall cleanup) (error "Follow client changed during acquisition"))
        (setq hermes-groups--follow-cleanup cleanup
              subscription (hermes-dashboard-transport-subscribe lease nil cleanup))))))

(defun hermes-groups--schedule (client)
  "Arm one owned five-second refresh on CLIENT, never resurrecting retirement."
  (when (and hermes-groups--follow (not hermes-groups--terminal)
             (< hermes-groups--failures 3))
    (let ((buffer (current-buffer))
          (current (hermes-browser--dispatch-guard
                    client (hermes-browser--owned-predicate '(hermes-groups--room-id))))
          (lease-cleanup hermes-groups--follow-cleanup)
          timer closed cleanup)
      (setq cleanup
            (lambda ()
              (unless closed
                (setq closed t)
                (when timer (cancel-timer timer))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (when (eq hermes-groups--timer-cleanup cleanup)
                      (setq hermes-groups--timer-cleanup nil))
                    (when (eq hermes-browser--owned-cleanup lease-cleanup)
                      (setq hermes-browser--owned-cleanup nil)))))))
      (setq hermes-groups--timer-cleanup cleanup
            hermes-browser--owned-cleanup lease-cleanup
            timer (run-at-time
                   5 nil
                   (lambda ()
                     (unless closed
                       (let ((valid (funcall current)))
                         (funcall cleanup)
                         (if valid
                             (with-current-buffer buffer (hermes-groups-refresh))
                           (funcall lease-cleanup))))))))))

(defun hermes-groups-refresh (&optional offset)
  "Refresh this view, resuming its accepted cursor after reconnect.
Optional OFFSET is a returned room-list offset.  Each read follows at most
20 log pages; another refresh continues larger histories without gaps."
  (interactive nil hermes-groups-mode)
  (hermes-groups--assert-owned)
  (when (eq hermes-groups--terminal 'expired) (user-error "This room's history expired"))
  (hermes-groups--retire-timer)
  (hermes-browser--next-request-generation)
  (setq hermes-browser--status "Loading")
  (let ((current (hermes-browser--owned-predicate '(hermes-groups--room-id)))
        (buffer (current-buffer))
        client-read client-scope read-active follow-cleanup)
    (hermes-browser--run-owned
     (lambda (client active)
       (setq client-read client read-active active
             client-scope (hermes-browser--client-scope client))
       (hermes-groups--retain-follow client active)
       (setq follow-cleanup hermes-groups--follow-cleanup)
       (let ((context (list :client client :active active :buffer (current-buffer)
                            :token hermes-dashboard-transport-request-owner)))
         (hermes-groups--then
          context (hermes-groups--rpc context "groups.capabilities" nil)
          (lambda (result)
            (setq hermes-groups--capabilities (hermes-groups--validate-capabilities result))
            (if hermes-groups--room-id
                (hermes-groups--then context (hermes-groups--state context)
                                     (lambda (_) (hermes-groups--log context 20 nil)))
              (hermes-groups--list context (or offset 0)))))))
     current
     (lambda (status) (setq hermes-browser--status status hermes-groups--failures 0))
     (lambda (reason) (hermes-groups--failure reason (or read-active current)))
     (lambda ()
       (if (not (and (funcall current) client-read
                     (hermes-browser--client-current-p client-read client-scope)))
           (progn
             (when follow-cleanup (funcall follow-cleanup))
             (when (funcall current)
               (with-current-buffer buffer (hermes-groups--stop-follow))))
         (with-current-buffer buffer
           (if (or (not hermes-groups--follow)
                   (equal hermes-browser--status "Interrupted; g reconcile"))
               (hermes-groups--stop-follow)
             (hermes-groups--schedule client-read))))))))

(defun hermes-groups--buffer (instance room-id)
  "Return an owned view for exact INSTANCE and ROOM-ID, creating if needed."
  (or (seq-find
       (lambda (buffer)
         (with-current-buffer buffer
           (and (hermes-buffer--owned-p 'hermes-groups-mode)
                (equal instance hermes-instance)
                (equal instance hermes-groups--instance)
                (equal room-id hermes-groups--room-id))))
       (buffer-list))
      (let ((buffer (generate-new-buffer
                     (format "*Hermes Groups: %s%s*" (hermes-instance-name instance)
                             (if room-id (concat " / " room-id) "")))))
        (with-current-buffer buffer
          (hermes-groups-mode)
          (hermes-buffer--claim 'hermes-groups-mode)
          (setq-local hermes-instance instance)
          (setq hermes-groups--room-id room-id
                hermes-groups--instance (hermes-browser--copy-identity instance)))
        buffer)))

(defun hermes-groups--display (buffer)
  "Display BUFFER and reject any ownership transfer during display hooks."
  (let ((owner (with-current-buffer buffer (hermes-browser--owned-predicate))))
    (pop-to-buffer buffer)
    (unless (funcall owner) (user-error "Room view changed during display"))))

;;;###autoload
(defun hermes-list-groups ()
  "Open a read-only hosted-room list on the selected Hermes backend."
  (interactive)
  (let* ((instance (hermes-instance-resolve))
         (buffer (hermes-groups--buffer instance nil)))
    (hermes-groups--display buffer)
    (with-current-buffer buffer (hermes-groups-refresh))
    buffer))

(defun hermes-groups-view ()
  "Open the selected hosted room's public history and activity."
  (interactive nil hermes-groups-mode)
  (hermes-groups--assert-owned)
  (when hermes-groups--room-id (user-error "Already reading a room"))
  (let* ((room (get-text-property (line-beginning-position) 'hermes-group-room))
         (instance hermes-instance)
         (id (hermes-transport--get room 'room_id)))
    (unless (and room (memq room hermes-groups--rooms)) (user-error "No room on this line"))
    (let ((buffer (hermes-groups--buffer instance id)))
      (hermes-groups--display buffer)
      (with-current-buffer buffer (hermes-groups-refresh))
      buffer)))

(defun hermes-groups-more ()
  "Read the next room-list page using the backend's returned offset."
  (interactive nil hermes-groups-mode)
  (hermes-groups--assert-owned)
  (unless (and (not hermes-groups--room-id) hermes-groups--next)
    (user-error "No next room page"))
  (hermes-groups-refresh hermes-groups--next))

(defun hermes-groups-follow ()
  "Toggle bounded five-second activity refresh for this hosted room.
Stop after three failed reads or transport retirement.  Refresh explicitly
to reconnect; disbanded or expired rooms never start automatic refresh."
  (interactive nil hermes-groups-mode)
  (hermes-groups--assert-owned)
  (unless (and hermes-groups--room-id (not hermes-groups--terminal))
    (user-error "Select a non-retired room first"))
  (setq hermes-groups--follow (not hermes-groups--follow) hermes-groups--failures 0)
  (hermes-groups--retire-timer)
  (if hermes-groups--follow (hermes-groups-refresh)
    (hermes-groups--stop-follow)))

(provide 'hermes-groups)
;;; hermes-groups.el ends here
