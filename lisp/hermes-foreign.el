;;; hermes-foreign.el --- Backend foreign conversation browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo
;; Author: Thanos Apollo <public@thanosapollo.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Browse serving-backend histories by opaque handle.  Preview never imports;
;; explicit import reads back the backend-returned session before offering resume.

;;; Code:
(require 'hermes-browser)
(require 'hermes-sessions)

(defvar-local hermes-foreign--profile nil "Exact destination profile.")
(defvar-local hermes-foreign--source nil "Selected foreign source, or nil for both.")
(defvar-local hermes-foreign--next nil "Backend-returned next offset.")
(defvar-local hermes-foreign--page nil "Accepted page and its host label.")
(defvar-local hermes-foreign--row nil "Exact previewed foreign row.")
(defvar-local hermes-foreign--session nil "Read-back imported stored session.")
(defvar-local hermes-foreign--resume-current nil "Predicate retaining import readback authority.")

(defvar-local hermes-foreign--endpoint nil
  "Copied serving-instance authority retained from the public constructor.")

(defun hermes-foreign--backend-current-p ()
  "Return non-nil if this view still resolves to its original serving backend."
  (and hermes-foreign--endpoint
       (equal hermes-foreign--endpoint (hermes-instance-context))))

(defun hermes-foreign--guard ()
  "Capture the current foreign view, serving backend and selection."
  (unless (and (hermes-buffer--owned-p) (hermes-foreign--backend-current-p))
    (user-error "Foreign view or backend changed; reopen from Sessions"))
  (hermes-browser--next-request-generation)
  (let ((buffer (current-buffer))
        (owner (hermes-browser--owned-predicate
                '(hermes-foreign--profile hermes-foreign--source
                  hermes-foreign--row hermes-foreign--endpoint))))
    (lambda ()
      (and (funcall owner)
           (with-current-buffer buffer (hermes-foreign--backend-current-p))))))

(defun hermes-foreign--verified-predicate ()
  "Capture verified stored-session authority for this preview.
Transient reads do not change its lifetime.  Retain the exact constructor,
backend, destination, handle and verified session.
A new request is not a new owner; stored history survives transport retirement."
  (let ((buffer (current-buffer))
        (claim hermes-buffer--owner)
        (instance hermes-instance)
        (endpoint (hermes-browser--copy-identity (hermes-instance-context)))
        (values (hermes-browser--copy-identity
                 (list hermes-instance hermes-foreign--profile
                       hermes-foreign--row hermes-foreign--session))))
    (lambda ()
      (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (and (hermes-buffer--owned-p)
                  (eq major-mode 'hermes-foreign-preview-mode)
                  (eq claim hermes-buffer--owner)
                  (eq instance hermes-instance)
                  (equal endpoint (hermes-instance-context))
                  (equal values (list hermes-instance hermes-foreign--profile
                                      hermes-foreign--row hermes-foreign--session))))))))

(defun hermes-foreign--verified-p ()
  "Return non-nil if this preview still owns its verified stored session."
  (and hermes-foreign--session hermes-foreign--resume-current
       (funcall hermes-foreign--resume-current)))

(defun hermes-foreign--header ()
  "Return persistent backend and destination labels, including on read failure."
  (format " %s · Host %s · Source %s · Destination %s · Unreadable %s "
          (hermes-instance-name hermes-instance)
          (or (hermes-transport--get hermes-foreign--page 'host) "unknown")
          (or hermes-foreign--source "all")
          (propertize (or hermes-foreign--profile "unknown") 'face 'hermes-browser-profile)
          (or (hermes-transport--get hermes-foreign--page 'unreadable) "unknown")))

(defun hermes-foreign--error (reason)
  "Display REASON without retrying an import."
  (setq hermes-browser--status
        (if (and (stringp reason) (string-prefix-p "unknown method: session.foreign." reason))
            "Backend does not support foreign histories; use a compatible backend"
          (format "Failed: %s; g retry read" reason))))

(defun hermes-foreign--rpc (client method params profile guard)
  "Call foreign METHOD with PARAMS on CLIENT for PROFILE under GUARD.
Validate the explicit profile before dispatch; never accept backend fallback.
The catalogue is point-in-time evidence, not an atomic profile lock."
  (let* ((token hermes-dashboard-transport-request-owner)
         (dispatch
          (lambda ()
            (unless (funcall guard) (error "Foreign view retired"))
            (let ((hermes-dashboard-transport-dispatch-guard guard)
                  (hermes-dashboard-transport-request-owner token))
              (hermes-dashboard-transport-call
               client (concat "session.foreign." method)
               (append params (and profile `((profile . ,profile)))))))))
    (if (null profile) (funcall dispatch)
      (hermes--promise-then
       (hermes-dashboard-transport-api-request-async
        "GET" "/api/profiles" :client client :current-p guard)
       (lambda (result)
         (unless (seq-some (lambda (row)
                            (equal profile (hermes-transport--get row 'name)))
                          (hermes-transport--get result 'profiles))
           (error "Destination profile unavailable; reopen from Sessions"))
         (funcall dispatch))))))

(defun hermes-foreign--page-rows (result offset)
  "Validate RESULT for OFFSET and return native list rows."
  (let ((rows (hermes-transport--get result 'sessions))
        (next (hermes-transport--get result 'next_offset))
        (unreadable (hermes-transport--get result 'unreadable)))
    (unless (and (hermes-transport--field-present-p result 'sessions)
                 (listp rows) (stringp (hermes-transport--get result 'host))
                 (integerp unreadable) (>= unreadable 0)
                 (hermes-transport--field-present-p result 'next_offset)
                 (or (null next) (and (integerp next) (> next offset)))
                 (seq-every-p
                  (lambda (row)
                    (and (hermes-transport--non-empty-string
                          (hermes-transport--get row 'id))
                         (member (hermes-transport--get row 'source) '("claude" "codex"))
                         (stringp (hermes-transport--get row 'title)))) rows))
      (error "Malformed foreign page; g retry read"))
    (mapcar (lambda (row)
              (list (hermes-transport--get row 'id)
                    (vector (hermes-browser--face-cell
                             (hermes-transport--get row 'title) 'hermes-browser-title)
                            (hermes-transport--display-field row 'source)
                            (hermes-transport--display-field row 'turn_count)
                            (hermes-transport--display-field row 'cwd)))) rows)))

(defun hermes-foreign--fetch (offset)
  "Read one explicit foreign page at OFFSET, even after an empty page."
  (let ((guard (hermes-foreign--guard))
        (source hermes-foreign--source))
    (setq hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client active)
       (hermes-foreign--rpc client "list"
                            `((offset . ,offset) (limit . 25)
                              ,@(and source `((source . ,source)))) nil active))
     guard
     (lambda (result)
       (let ((entries (hermes-foreign--page-rows result offset)))
         (let ((tabulated-list-entries entries))
           (atomic-change-group (tabulated-list-print t)))
         (setq tabulated-list-entries entries
               hermes-foreign--page result
               hermes-foreign--next (hermes-transport--get result 'next_offset)
               hermes-browser--status
               (format "Host %s · Source %s · Destination %s · Unreadable %s · %s"
                       (hermes-transport--get result 'host) (or source "all")
                       hermes-foreign--profile (hermes-transport--get result 'unreadable)
                       (if hermes-foreign--next "> next page" "End")))))
     #'hermes-foreign--error)))

(defun hermes-foreign-refresh ()
  "Read the first foreign page again, without importing anything."
  (interactive nil hermes-foreign-mode)
  (hermes-foreign--fetch 0))

(defun hermes-foreign-next-page ()
  "Read the backend's next page, including after unreadable-only pages."
  (interactive nil hermes-foreign-mode)
  (unless hermes-foreign--next (user-error "No next page; g refresh"))
  (hermes-foreign--fetch hermes-foreign--next))

(defun hermes-foreign--preview-text (result)
  "Return inert bounded preview text from RESULT."
  (let ((messages (hermes-transport--get result 'messages)))
    (unless (and (hermes-transport--field-present-p result 'messages)
                 (listp messages) (integerp (hermes-transport--get result 'total))
                 (seq-every-p (lambda (row)
                               (and (stringp (hermes-transport--get row 'role))
                                    (stringp (hermes-transport--get row 'content)))) messages))
      (error "Malformed foreign preview"))
    (concat
     (format "Bounded tail preview — at most 40 turns / 8000 characters per turn\nTotal turns: %s\nTruncated: %s\nAlready imported: %s\nBackend directory label: %s\n\n"
             (hermes-transport--get result 'total)
             (if (hermes-transport--field-present-p result 'truncated)
                 (if (hermes-transport--true-p (hermes-transport--get result 'truncated)) "yes" "no")
               "unknown")
             (or (hermes-transport--get result 'already_imported) "no")
             (or (hermes-transport--get result 'cwd) "unknown"))
     (mapconcat (lambda (row)
                  (concat (propertize (hermes-transport--get row 'role) 'face 'bold)
                          "\n" (hermes-transport--get row 'content))) messages "\n\n"))))

(defun hermes-foreign-preview-refresh ()
  "Refresh the bounded preview on its original destination profile."
  (interactive nil hermes-foreign-preview-mode)
  (let ((guard (hermes-foreign--guard))
        (id (hermes-transport--get hermes-foreign--row 'id))
        (profile hermes-foreign--profile))
    (setq hermes-browser--status "Loading")
    (hermes-browser--run-owned
     (lambda (client active)
       (hermes-foreign--rpc client "preview" `((id . ,id)) profile active))
     guard
     (lambda (result)
       (let ((text (hermes-foreign--preview-text result)) (inhibit-read-only t))
         (atomic-change-group (erase-buffer) (insert text))
         (goto-char (point-min)))
       (setq hermes-browser--status
             (if (hermes-foreign--verified-p)
                 "Imported session verified; RET resume"
               "Preview only; i explicitly imports")))
     #'hermes-foreign--error)))

(defun hermes-foreign-preview ()
  "Open a passive, inert preview of the selected serving-backend history."
  (interactive nil hermes-foreign-mode)
  (unless (and (hermes-buffer--owned-p) (hermes-foreign--backend-current-p))
    (user-error "Foreign view or backend changed; reopen from Sessions"))
  (let* ((id (tabulated-list-get-id))
         (row (seq-find (lambda (entry) (equal id (hermes-transport--get entry 'id)))
                        (hermes-transport--get hermes-foreign--page 'sessions)))
         (instance hermes-instance) (profile hermes-foreign--profile)
         (endpoint (hermes-browser--copy-identity hermes-foreign--endpoint))
         (host (hermes-transport--get hermes-foreign--page 'host)))
    (unless row (user-error "No foreign session on this line; g refresh"))
    (let ((buffer (hermes-buffer--get
                   (generate-new-buffer-name "*Hermes Foreign Preview*")
                   #'hermes-foreign-preview-mode)))
      (with-current-buffer buffer
        (hermes-browser--own-instance instance)
        (setq hermes-foreign--profile profile hermes-foreign--row row
              hermes-foreign--endpoint endpoint
              header-line-format
              (format "Host %s · Source %s · Destination %s"
                      host (hermes-transport--get row 'source) profile)))
      (pop-to-buffer buffer)
      (with-current-buffer buffer (hermes-foreign-preview-refresh)))))

(defun hermes-foreign-import ()
  "Explicitly import this preview, then read back the returned stored session.
A lost import receipt is uncertain; never automatically resubmit it."
  (interactive nil hermes-foreign-preview-mode)
  (when hermes-browser--owned-cleanup (user-error "Wait for the pending read or import"))
  (let ((guard (hermes-foreign--guard)) (buffer (current-buffer))
        (id (hermes-transport--get hermes-foreign--row 'id))
        (profile hermes-foreign--profile)
        (title (hermes-transport--display-field hermes-foreign--row 'title)))
    (when (yes-or-no-p (format "Import %s into %s on %s? " title profile
                              (hermes-instance-url hermes-foreign--endpoint)))
      (unless (funcall guard) (user-error "Foreign view changed during confirmation"))
      (with-current-buffer buffer
        (setq hermes-browser--status "Saving" hermes-foreign--session nil)
        (hermes-browser--run-owned
         (lambda (client active)
           (hermes--promise-then
            (hermes-foreign--rpc client "import" `((id . ,id)) profile active)
            (lambda (result)
              (let ((session-id (hermes-transport--get result 'session_id)))
                (unless (hermes-transport--non-empty-string session-id)
                  (error "Import returned no session identity"))
                (hermes--promise-then
                 (hermes-dashboard-transport-api-request-async
                  "GET" (concat "/api/sessions/" (url-hexify-string session-id))
                  :query `((profile . ,profile)) :client client :current-p active)
                 (lambda (session)
                   (unless (and (equal session-id (hermes-transport--get session 'id))
                                (equal profile (hermes-transport--get session 'profile)))
                     (error "Imported session readback did not match destination"))
                   session))))))
         guard
         (lambda (session)
           (setq hermes-foreign--session session
                 hermes-foreign--resume-current (hermes-foreign--verified-predicate)
                 hermes-browser--status "Imported session verified; RET resume"))
         (lambda (reason)
           (setq hermes-browser--status
                 (format "Import uncertain / readback failed: %s; g inspect before retry" reason))))))))

(defun hermes-foreign-resume ()
  "Resume only the backend-returned, profile-verified imported session."
  (interactive nil hermes-foreign-preview-mode)
  (unless (hermes-foreign--verified-p)
    (user-error "Import and verify this session first"))
  (hermes-chat-resume-session
   (hermes-transport--get hermes-foreign--session 'id)
   (hermes-transport--display-field hermes-foreign--session 'title)
   hermes-foreign--profile hermes-instance nil
   (hermes-instance-url hermes-foreign--endpoint)))

(defvar-keymap hermes-foreign-mode-map
  :parent tabulated-list-mode-map
  "g" #'hermes-foreign-refresh ">" #'hermes-foreign-next-page
  "RET" #'hermes-foreign-preview "f" #'hermes-foreign-preview "b" #'quit-window)
(keymap-popup-annotate hermes-foreign-mode-map
  :popup-key "?" :exit-key "C-g" :description "Backend foreign histories"
  :group "Read" hermes-foreign-preview "Preview" hermes-foreign-next-page "Next page"
  hermes-foreign-refresh "Refresh" quit-window "Quit")
(define-derived-mode hermes-foreign-mode tabulated-list-mode "Foreign histories"
  "Browse opaque handles published by the serving backend."
  (setq tabulated-list-format [("Title" 40 t) ("Source" 10 t) ("Turns" 8 t)
                               ("Backend directory label" 35 t)])
  (tabulated-list-init-header)
  (hermes-browser--setup-status))

(defvar-keymap hermes-foreign-preview-mode-map
  :parent special-mode-map
  "g" #'hermes-foreign-preview-refresh "i" #'hermes-foreign-import
  "RET" #'hermes-foreign-resume "n" #'forward-line "p" #'previous-line
  "f" #'forward-char "b" #'backward-char)
(keymap-popup-annotate hermes-foreign-preview-mode-map
  :popup-key "?" :exit-key "C-g" :description "Foreign preview"
  :group "History" hermes-foreign-import "Import" hermes-foreign-resume "Resume imported"
  :group "Read" hermes-foreign-preview-refresh "Refresh preview" quit-window "Quit")
(define-derived-mode hermes-foreign-preview-mode special-mode "Foreign preview"
  "Show a bounded, inert backend history preview, never a local file."
  (hermes-browser--setup-status))

;;;###autoload
(defun hermes-list-foreign-sessions ()
  "Browse foreign histories on the selected backend, not this Emacs host.
Choose the exact destination profile; it is validated before preview/import."
  (interactive)
  (let* ((origin (current-buffer))
         (guard (hermes-browser--mutation-context))
         (instance (hermes-instance-resolve))
         (profile (read-string "Destination backend profile: " "default"))
         (source (completing-read "Foreign source: " '("all" "claude" "codex") nil t nil nil "all")))
    (unless (and (funcall guard) (not (string-empty-p profile)))
      (user-error "Origin changed or empty destination profile"))
    (with-current-buffer origin
      (let ((buffer (hermes-buffer--get
                     (generate-new-buffer-name "*Hermes Foreign Sessions*") #'hermes-foreign-mode)))
        (with-current-buffer buffer
          (hermes-browser--own-instance instance)
          (setq hermes-foreign--profile profile
                hermes-foreign--endpoint (hermes-browser--copy-identity instance)
                hermes-foreign--source (unless (equal source "all") source))
          (hermes-browser--show-context '(:eval (hermes-foreign--header))))
        (pop-to-buffer buffer)
        (with-current-buffer buffer (hermes-foreign-refresh))))))

(provide 'hermes-foreign)
;;; hermes-foreign.el ends here
