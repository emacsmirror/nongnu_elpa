;;; hermes-chat-failure.el --- Classified turn failures with recovery actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A turn that fails with the backend's structured failure facts
;; (`hermes-transport-turn-failure': the advisory `error_surface' and the
;; billing-wall descriptor) renders as a compact block under the reply:
;;
;;   ! The model provider is rate-limiting requests (OpenRouter)
;;     provider · rate_limit · gpt-5
;;     [Retry] [Switch model]  ▸ Details
;;
;; The title comes from the surface's layer and code, never from the
;; backend's human-readable prose.  The raw provider detail sits behind the
;; usual disclosure.  Only the applicable actions appear: Retry when the
;; surface says a retry can help, Switch model when the fix lies in provider
;; or model choice, and the billing page when the backend sent a link.
;; Every action is an explicit command; nothing retries or opens on its own.
;; With point on the block, `?' lists the same actions in a popup.
;;
;; Failures without structured facts keep their ordinary rendering.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'keymap-popup)
(require 'hermes-chat-render)
(require 'hermes-chat-buffer)

(declare-function hermes-chat--dashboard-slash-exec "hermes-chat-slash"
                  (name arg raw))
(declare-function hermes-chat-switch-model "hermes-chat-models"
                  (&optional refresh))

;;;; Pure presentation

(defconst hermes-chat-failure--code-titles
  '(("auth" . "The model provider rejected the API key")
    ("auth_permanent" . "The model provider rejected the API key")
    ("billing" . "The model provider reports no credit left")
    ("billing_unverified" . "The model provider reports no credit left")
    ("content_policy_blocked"
     . "The model provider refused this request (content policy)")
    ("context_overflow" . "The conversation is too long for this model")
    ("format_error" . "The model provider rejected the request format")
    ("model_not_found" . "The model provider does not know this model")
    ("overloaded" . "The model provider is overloaded")
    ("payload_too_large" . "The request was too large for this model")
    ("provider_policy_blocked"
     . "The model provider refused this request (account policy)")
    ("rate_limit" . "The model provider is rate-limiting requests")
    ("server_error" . "The model provider had an internal error")
    ("ssl_cert_verification"
     . "The connection to the model provider could not be verified (TLS)")
    ("timeout" . "The model provider did not answer in time")
    ("upstream_blocked"
     . "A firewall/CDN in front of the model provider blocked the request")
    ("upstream_rate_limit" . "The model provider is rate-limiting requests"))
  "Failure titles by `error_surface' code, as the released TUI words them.")

(defconst hermes-chat-failure--layer-titles
  '(("auth" . "The model provider rejected the credentials")
    ("billing" . "The model provider reports no credit left")
    ("disk" . "The disk is full, so Hermes could not save the turn")
    ("endpoint" . "Your custom model endpoint did not answer")
    ("gateway" . "Hermes hit an internal error while running this turn")
    ("provider" . "The model provider returned an error")
    ("streaming" . "The connection to the model provider dropped mid-reply"))
  "Failure titles by `error_surface' layer when the code has none.")

(defconst hermes-chat-failure--switch-layers
  '("auth" "billing" "endpoint" "provider")
  "Surface layers whose fix is another provider or model.")

(defun hermes-chat-failure--provider-name (plist)
  "Return the provider name to show for surface or billing PLIST, or nil."
  (or (plist-get plist :provider-label) (plist-get plist :provider)))

(defun hermes-chat-failure-title (failure)
  "Return the one-line title for structured FAILURE.
A billing wall names the provider whose credit ran out; otherwise the
surface's code, then its layer, selects the title."
  (let ((billing (plist-get failure :billing))
        (surface (plist-get failure :surface)))
    (cond
     ((plist-get billing :nous) "Out of Nous credits")
     (billing
      (format "Out of credits · %s"
              (or (hermes-chat-failure--provider-name billing)
                  (hermes-chat-failure--provider-name surface)
                  "your provider")))
     (t
      (let ((title (or (cdr (assoc (plist-get surface :code)
                                   hermes-chat-failure--code-titles))
                       (cdr (assoc (plist-get surface :layer)
                                   hermes-chat-failure--layer-titles))
                       "The request failed"))
            (provider (hermes-chat-failure--provider-name surface)))
        (if provider (format "%s (%s)" title provider) title))))))

(defun hermes-chat-failure-cause (failure)
  "Return FAILURE's compact cause line: layer, code and model.
A code that repeats the layer or says only \"unknown\" is left out."
  (let* ((surface (plist-get failure :surface))
         (billing (plist-get failure :billing))
         (layer (or (plist-get surface :layer) "billing"))
         (code (or (plist-get surface :code) (plist-get failure :reason))))
    (string-join
     (delq nil
           (list layer
                 (and code (not (member code (list layer "unknown"))) code)
                 (or (plist-get surface :model) (plist-get billing :model))))
     " · ")))

(defun hermes-chat-failure-details (failure)
  "Return FAILURE's raw detail text for the disclosure, or nil."
  (let* ((detail (plist-get failure :detail))
         (guidance (plist-get (plist-get failure :billing) :message))
         (parts (delete-dups (delq nil (list detail guidance)))))
    (and parts (string-join parts "\n"))))

(defun hermes-chat-failure-actions (failure)
  "Return the recovery actions that apply to FAILURE, in display order.
Each is `retry', `switch-model' or `billing'.  Retry needs a surface that
does not call the failure deterministic; a billing wall never retries."
  (let* ((surface (plist-get failure :surface))
         (billing (plist-get failure :billing)))
    (delq nil
          (list (and surface (plist-get surface :retryable) (not billing)
                     'retry)
                (and (or billing
                         (member (plist-get surface :layer)
                                 hermes-chat-failure--switch-layers))
                     'switch-model)
                (and (plist-get billing :url) 'billing)))))

(defun hermes-chat-failure--action-label (action failure)
  "Return the button label for ACTION on FAILURE."
  (pcase action
    ('retry "[Retry]")
    ('switch-model "[Switch model]")
    ('billing (if (plist-get (plist-get failure :billing) :nous)
                  "[Top up]"
                "[Open billing page]"))))

(defconst hermes-chat-failure--action-commands
  '((retry hermes-chat-failure-retry "Resend the failed turn (/retry)")
    (switch-model hermes-chat-failure-switch-model
                  "Pick another model for this chat")
    (billing hermes-chat-failure-open-billing
             "Open the provider's billing page in a browser"))
  "Alist of (ACTION COMMAND HELP) for failure recovery buttons.")

;;;; Entry state

(defun hermes-chat-failure-retained-entries (retained &optional prompt)
  "Return the chat entries rebuilding a RETAINED failed turn on resume.
RETAINED is a `hermes-transport-retained-turn-failure' plist.  With PROMPT,
its user text opens the turn; otherwise the transcript already shows it.
A structured failure renders as a failure block under the partial reply;
without one, the reply and raw error render like a plain failure."
  (let ((user (plist-get retained :user)))
    (delq nil
          (list (and prompt user (hermes-chat--make-entry 'user user 'done))
                (hermes-chat-failure-retained-reply
                 (hermes-chat--make-entry 'assistant "" 'error)
                 retained)))))

(defun hermes-chat-failure-retained-reply (entry retained)
  "Return assistant ENTRY settled as the reply of RETAINED's failed turn.
ENTRY's text is the reply when it has one, as when history already holds
the partial; else the retained partial is.  Applying it again is a no-op.  Pure."
  (let* ((failure (plist-get retained :failure))
         (error (plist-get retained :error))
         (text (or (hermes-transport--non-empty-string (plist-get entry :content))
                   (plist-get retained :partial-text)))
         (content (if (or failure (null error)
                          (and text (string-suffix-p error text)))
                      (or text "")
                    (string-join (delq nil (list text error)) "\n\n"))))
    (hermes-chat--entry-with
     entry :status 'error :content content
     :metadata (if failure
                   (plist-put (copy-sequence (plist-get entry :metadata))
                              :failure failure)
                 (plist-get entry :metadata)))))

(defun hermes-chat--entry-failure (entry)
  "Return the structured failure stored on ENTRY, or nil."
  (plist-get (plist-get entry :metadata) :failure))

(defun hermes-chat--turn-reply-ids (assistant-id)
  "Return the ids of ASSISTANT-ID's reply segments, oldest first.
The reply is every assistant entry since the last user entry before
ASSISTANT-ID, the prompt or an accepted redirect, or since the assistant
entry opening a backend-owned turn, through ASSISTANT-ID itself."
  (let ((node (and hermes-chat--nodes (gethash assistant-id hermes-chat--nodes)))
        ids)
    (while node
      (let ((entry (ewoc-data node)))
        (if (eq (plist-get entry :role) 'user)
            (setq node nil)
          (when (eq (plist-get entry :role) 'assistant)
            (push (plist-get entry :id) ids))
          (setq node (and (not (plist-get entry :turn-start))
                          (ewoc-prev hermes-chat--ewoc node))))))
    ids))

(defun hermes-chat--mark-assistant-failure (assistant-id failure status)
  "Settle ASSISTANT-ID in STATUS carrying structured FAILURE.
A retained partial reply is the backend's whole text for the turn since
its last user boundary, so it replaces every reply segment streamed since
then with one segment instead of extending them; the backend's prose
account is not shown, because the failure block carries the same facts."
  (when-let* ((partial (plist-get failure :partial-text))
              (ids (hermes-chat--turn-reply-ids assistant-id)))
    ;; A redirect's saved prefix belongs to the turn before the boundary.
    (let ((text (hermes-chat--assistant-segment-content (car ids) partial)))
      (dolist (id (butlast ids))
        (hermes-chat--remove-entry id t))
      (hermes-chat--mark-assistant assistant-id status text t)))
  (hermes-chat--update-entry
   assistant-id
   (lambda (entry)
     (hermes-chat--entry-with
      entry :status status
      :metadata (plist-put (copy-sequence (plist-get entry :metadata))
                           :failure failure)))))

(defun hermes-chat--failure-node (&optional id)
  "Return the EWOC node of failure entry ID, or of the failure at point."
  (let ((id (or id (get-text-property (point) 'hermes-chat-failure-id))))
    (or (and id hermes-chat--nodes (gethash id hermes-chat--nodes))
        (user-error "No turn failure at point"))))

(defun hermes-chat--failure-latest-p (node)
  "Return non-nil when no user turn follows failure NODE."
  (let ((next (ewoc-next hermes-chat--ewoc node))
        (latest t))
    (while (and next latest)
      (when (eq (plist-get (ewoc-data next) :role) 'user)
        (setq latest nil))
      (setq next (ewoc-next hermes-chat--ewoc next)))
    latest))

(defun hermes-chat--failure-entry-actions (entry)
  "Return the recovery actions that apply to ENTRY's failure now.
Retry applies only while no user turn follows the failure, because the
backend's /retry rewinds to the last user message.  An entry not yet in
the node table is being inserted last, so it counts as the latest."
  (let ((actions (hermes-chat-failure-actions
                  (hermes-chat--entry-failure entry)))
        (node (and hermes-chat--nodes
                   (gethash (plist-get entry :id) hermes-chat--nodes))))
    (if (and (memq 'retry actions) node
             (not (hermes-chat--failure-latest-p node)))
        (remq 'retry actions)
      actions)))

(defun hermes-chat--failure-action-p (action)
  "Return non-nil when ACTION is a current action of the failure at point."
  (when-let* ((id (get-text-property (point) 'hermes-chat-failure-id))
              (node (and hermes-chat--nodes (gethash id hermes-chat--nodes))))
    (memq action (hermes-chat--failure-entry-actions (ewoc-data node)))))

(defun hermes-chat--failure-require (action id)
  "Return (NODE . FAILURE) for failure ID with ACTION applicable.
Signal `user-error' otherwise."
  (let* ((node (hermes-chat--failure-node id))
         (entry (ewoc-data node))
         (failure (hermes-chat--entry-failure entry)))
    (unless (memq action (hermes-chat--failure-entry-actions entry))
      (user-error (if (memq action (hermes-chat-failure-actions failure))
                      "A newer turn follows this failure; retry only the latest"
                    "This failure offers no such recovery")))
    (cons node failure)))

(defun hermes-chat--failure-retire-retry (node)
  "Re-render the failure that user entry NODE now follows.
Its Retry no longer applies.  Older failures were refreshed when their
own following user entry arrived, so the walk stops at the previous one."
  (when (eq (plist-get (ewoc-data node) :role) 'user)
    (let ((prev (ewoc-prev hermes-chat--ewoc node))
          (done nil))
      (while (and prev (not done))
        (let ((entry (ewoc-data prev)))
          (cond
           ((eq (plist-get entry :role) 'user) (setq done t))
           ((memq 'retry (hermes-chat-failure-actions
                          (hermes-chat--entry-failure entry)))
            (setq done t)
            (hermes-chat--preserve-input-point
             (let ((inhibit-read-only t)
                   (buffer-undo-list t))
               (hermes-chat--preserve-readers
                 (ewoc-invalidate hermes-chat--ewoc prev)))))))
        (setq prev (ewoc-prev hermes-chat--ewoc prev))))))

(add-hook 'hermes-chat--entry-inserted-functions
          #'hermes-chat--failure-retire-retry)

(defun hermes-chat-failure-refresh-actions ()
  "Re-render every failure block so its actions fit the transcript now.
A rebuild that bypasses `hermes-chat--entry-inserted-functions' calls this
once, since Retry applies only while no user turn follows a failure."
  (hermes-chat--preserve-input-point
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (hermes-chat--preserve-readers
        (ewoc-map #'hermes-chat--entry-failure hermes-chat--ewoc)))))

;;;; Commands

(defun hermes-chat-failure-retry (&optional id)
  "Resend the failed turn ID, or the one at point, through /retry.
Only the latest turn retries: the backend rewinds to the last user
message, so a failure with a later turn would resend the wrong one."
  (interactive nil hermes-chat-mode)
  (hermes-chat--failure-require 'retry id)
  (hermes-chat--ensure-submit-allowed)
  (hermes-chat--dashboard-slash-exec "retry" "" "retry"))

(defun hermes-chat-failure-switch-model (&optional id)
  "Open the model picker to recover from failure ID, or the one at point."
  (interactive nil hermes-chat-mode)
  (hermes-chat--failure-require 'switch-model id)
  (call-interactively #'hermes-chat-switch-model))

(defun hermes-chat-failure-open-billing (&optional id)
  "Open the billing page of failure ID, or the one at point, in a browser."
  (interactive nil hermes-chat-mode)
  (let ((failure (cdr (hermes-chat--failure-require 'billing id))))
    (browse-url (plist-get (plist-get failure :billing) :url))))

(defun hermes-chat--failure-button (button)
  "Run BUTTON's failure recovery command for its entry."
  (funcall (button-get button 'hermes-chat-failure-command)
           (button-get button 'hermes-chat-failure-id)))

(keymap-popup-define hermes-chat-failure-map
  "Recover from the failed turn at point."
  :popup-key "?"
  :exit-key "q"
  :group "Recover"
  "r" ("Retry the turn" hermes-chat-failure-retry
       :if (lambda () (hermes-chat--failure-action-p 'retry)))
  "m" ("Switch model" hermes-chat-failure-switch-model
       :if (lambda () (hermes-chat--failure-action-p 'switch-model)))
  "b" ("Open billing page" hermes-chat-failure-open-billing
       :if (lambda () (hermes-chat--failure-action-p 'billing))))

(put 'hermes-chat-failure-map-popup 'command-modes '(hermes-chat-mode))

;;;; Rendering

(defun hermes-chat--insert-failure-details-toggle (entry expanded)
  "Insert ENTRY's `Details' disclosure in EXPANDED state."
  (insert-text-button
   (if expanded "▾ Details" "▸ Details")
   'face 'shadow 'mouse-face 'highlight 'follow-link t
   'help-echo (if expanded "Hide the provider's detail"
                "Show the provider's detail")
   'hermes-chat-entry-id (plist-get entry :id)
   'action #'hermes-chat--toggle-entry-button))

(defun hermes-chat--insert-failure-actions (entry failure)
  "Insert the recovery buttons that apply to FAILURE on ENTRY, if any."
  (let ((first t))
    (dolist (action (hermes-chat--failure-entry-actions entry))
      (pcase-let ((`(,command ,help)
                   (alist-get action hermes-chat-failure--action-commands)))
        (unless first (insert " "))
        (setq first nil)
        (insert-text-button
         (hermes-chat-failure--action-label action failure)
         'face 'link 'mouse-face 'highlight 'follow-link t
         'help-echo (concat help "; ? for all actions")
         'hermes-chat-failure-id (plist-get entry :id)
         'hermes-chat-failure-command command
         'action #'hermes-chat--failure-button)))
    (not first)))

(defun hermes-chat--insert-failure (entry)
  "Insert ENTRY's structured failure block when it has one."
  (when-let* ((failure (hermes-chat--entry-failure entry)))
    (let ((start (point))
          (details (hermes-chat-failure-details failure))
          (message (plist-get (plist-get failure :surface) :message))
          (expanded (hermes-chat--entry-expanded-p entry)))
      (insert (propertize "  ! " 'face 'error)
              (propertize (hermes-chat-failure-title failure) 'face 'error)
              "\n")
      (when message
        (insert "    " message "\n"))
      (insert "    " (propertize (hermes-chat-failure-cause failure) 'face 'shadow)
              "\n")
      (let ((line (point)))
        (insert "    ")
        (let ((actions (hermes-chat--insert-failure-actions entry failure)))
          (when details
            (when actions (insert "  "))
            (hermes-chat--insert-failure-details-toggle entry expanded))
          (if (or actions details)
              (insert "\n")
            (delete-region line (point)))))
      (when (and details expanded)
        (hermes-chat--insert-detail-body
         entry (lambda ()
                 (insert (propertize
                          (concat (replace-regexp-in-string "^" "    " details)
                                  "\n")
                          'face 'shadow)))))
      (add-text-properties start (point)
                           (list 'hermes-chat-failure-id (plist-get entry :id)
                                 'keymap hermes-chat-failure-map
                                 'wrap-prefix "    ")))))

(add-hook 'hermes-chat--entry-suffix-functions #'hermes-chat--insert-failure)

(provide 'hermes-chat-failure)
;;; hermes-chat-failure.el ends here
