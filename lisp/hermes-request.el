;;; hermes-request.el --- Headless isolated Hermes prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools, convenience

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

;; One prompt, one fresh profile-qualified session, one asynchronous result.
;; Session closure is best effort; stored history follows backend retention.

;;; Code:

(require 'hermes-dashboard-rpc)
(require 'seq)
(require 'subr-x)

(defgroup hermes-request nil
  "Headless Hermes requests."
  :group 'tools)

(defcustom hermes-request-timeout 180
  "Seconds before a headless request fails, including startup."
  :type 'number
  :group 'hermes-request)

(defun hermes-request--live-p (op)
  "Return non-nil while OP may accept results."
  (not (plist-get op :settled)))

(defun hermes-request--notify (callback value)
  "Call CALLBACK with VALUE outside the transport filter, containing failures."
  (condition-case nil (funcall callback value)
    ((error quit) (message "Hermes request callback failed"))))

(defun hermes-request--release (op)
  "Release OP's subscription and client exactly once."
  (when-let* ((client (plist-get op :client)))
    (setf (plist-get op :client) nil)
    (when-let* ((token (plist-get op :subscription)))
      (hermes-dashboard-transport-unsubscribe client token))
    (hermes-dashboard-transport-cancel-owner-requests client op)
    (hermes-dashboard-transport-release client)))

(defun hermes-request--cleanup (op)
  "Close OP's exact session, retaining late-create ownership when necessary."
  (when (and (plist-get op :client) (not (plist-get op :creating))
             (not (plist-get op :closing)))
    (setf (plist-get op :closing) t)
    (if-let* ((sid (plist-get op :session))
              (client (plist-get op :client))
              ((equal (plist-get op :generation)
                      (hermes-dashboard-transport-client-generation client)))
              ((not (hermes-dashboard-transport-client-stopping-p client))))
        (condition-case nil
            (let ((hermes-dashboard-transport-request-timeout 30)
                  (hermes-dashboard-transport-dispatch-guard
                   (lambda ()
                     (equal (plist-get op :generation)
                            (hermes-dashboard-transport-client-generation client))))
                  (hermes-dashboard-transport-request-owner nil))
              (hermes-dashboard-transport-session-close
               (plist-get op :client) :session-id sid
               :resolve (lambda (_result) (hermes-request--release op))
               :reject (lambda (_reason) (hermes-request--release op))))
          ((error quit) (hermes-request--release op)))
      (hermes-request--release op))))

(defun hermes-request--settle (op success value)
  "Settle OP once with SUCCESS and VALUE, then dispose its session."
  (when (hermes-request--live-p op)
    (setf (plist-get op :settled) t)
    (when-let* ((timer (plist-get op :timer))) (cancel-timer timer))
    (run-at-time 0 nil #'hermes-request--notify
                 (plist-get op (if success :resolve :reject)) value)
    (hermes-request--cleanup op)))

(defun hermes-request--fail (op reason)
  "Reject OP with REASON."
  (hermes-request--settle op nil reason))

(defun hermes-request--finish (op)
  "Resolve OP only after both admission and a valid terminal response."
  (when (and (hermes-request--live-p op) (plist-get op :accepted)
             (plist-get op :terminal))
    (hermes-request--settle op t (plist-get op :terminal))))

(defun hermes-request--event (op event)
  "Consume only OP's owned terminal EVENT; ignore deltas and unrelated output."
  (when (hermes-request--live-p op)
    (let ((sid (plist-get event :session-id)))
      (cond
       ((and (not sid) (eq (plist-get event :type) 'error))
        (hermes-request--fail op "Hermes transport failed"))
       ((and sid (equal sid (plist-get op :session)))
        (cond
         ((or (eq (plist-get event :type) 'error)
              (plist-get event :prompt-request-p)
              (equal (plist-get event :event) "session.reclaimed"))
          (hermes-request--fail op "Hermes turn failed or requires interaction"))
         ((and (plist-get op :submitted)
               (equal (plist-get event :event) "message.complete")
               (not (plist-get op :terminal)))
          (if (and (equal (plist-get event :status) "complete")
                   (stringp (plist-get event :final-text))
                   (not (string-empty-p (plist-get event :final-text))))
              (progn
                (setf (plist-get op :terminal) (plist-get event :final-text))
                (hermes-request--finish op))
            (hermes-request--fail op "Invalid Hermes terminal response")))))))))

(defun hermes-request--submit (op)
  "Submit OP's prompt after its session and profile have been verified."
  (setf (plist-get op :submitted) t)
  (let ((hermes-dashboard-transport-dispatch-guard
         (lambda () (hermes-request--live-p op)))
        (hermes-dashboard-transport-request-owner op))
    (hermes-dashboard-transport-prompt-submit
     (plist-get op :client) (plist-get op :prompt)
     :session-id (plist-get op :session)
     :resolve
     (lambda (result)
       (when (hermes-request--live-p op)
         (if (equal (hermes-transport--get result 'status) "streaming")
             (progn (setf (plist-get op :accepted) t)
                    (hermes-request--finish op))
           (hermes-request--fail op "Hermes refused isolated prompt admission"))))
     :reject (lambda (reason) (hermes-request--fail op reason)))))

(defun hermes-request--created (op result)
  "Accept OP's fresh session RESULT, or close the exact late session."
  (when (plist-get op :creating)
    (setf (plist-get op :creating) nil
          (plist-get op :session)
          (hermes-transport-work-string result 'session_id))
    (cond
     ((not (hermes-request--live-p op)) (hermes-request--cleanup op))
     ((not (and (plist-get op :session)
                (equal (hermes-transport--get
                        (hermes-transport--get result 'info) 'profile_name)
                       (plist-get op :profile))
                (equal (hermes-transport--get result 'message_count) 0)))
      (hermes-request--fail op "Hermes did not create the requested empty profile session"))
     (t
      (hermes-dashboard-transport-subscribe-session
       (plist-get op :client) (plist-get op :subscription)
       (plist-get op :session))
      (condition-case reason (hermes-request--submit op)
        ((error quit) (hermes-request--fail op reason)))))))

(defun hermes-request--profile-model (profile catalogue)
  "Return PROFILE's exact model and provider from CATALOGUE, or signal."
  (let* ((row (seq-find
               (lambda (row) (equal profile (hermes-transport--get row 'name)))
               (hermes-transport--get catalogue 'profiles)))
         (model (hermes-transport--get row 'model))
         (provider (hermes-transport--get row 'provider))
         ;; Python str.strip whitespace, independent of Emacs syntax tables.
         (space "[\t-\r\u001c-\u0020\u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]"))
    (unless row
      (error "Requested profile is absent from this backend's catalogue"))
    (unless (seq-every-p
             (lambda (value)
               (and (stringp value) (not (string-empty-p value))
                    (equal value (string-trim value space space))
                    (not (string-match-p "[[:cntrl:]]" value))))
             (list model provider))
      (error "Requested profile lacks a valid configured model or provider"))
    (list :model model :provider provider)))

(defun hermes-request--create (op catalogue)
  "Create OP's session with the profile model captured from CATALOGUE."
  (when (hermes-request--live-p op)
    (let ((model (hermes-request--profile-model (plist-get op :profile) catalogue)))
      (setf (plist-get op :creating) t)
      ;; Keep the create callback after cancellation to close a late exact handle.
      (condition-case reason
          (let ((hermes-dashboard-transport-request-timeout 30)
                (hermes-dashboard-transport-request-owner nil)
                (hermes-dashboard-transport-dispatch-guard
                 (lambda () (hermes-request--live-p op))))
            (hermes-dashboard-transport-session-create
             (plist-get op :client) :profile (plist-get op :profile)
             ;; Explicit overrides beat the backend's launch environment.
             ;; Credentials and configured fallbacks remain backend-owned.
             :model (plist-get model :model) :provider (plist-get model :provider)
             :messages [] :hidden t :close-on-disconnect t
             :resolve (lambda (result) (hermes-request--created op result))
             :reject (lambda (failure)
                       (setf (plist-get op :creating) nil)
                       (hermes-request--fail op failure)
                       (hermes-request--cleanup op))))
        ((error quit)
         (setf (plist-get op :creating) nil)
         (hermes-request--fail op reason)
         (hermes-request--cleanup op))))))

(defun hermes-request--catalogue (op)
  "Read OP's catalogue after the owning dashboard becomes ready."
  (when (hermes-request--live-p op)
    (condition-case reason
        (hermes--promise-catch
         (hermes--promise-then
          (hermes-dashboard-transport-api-request-async
           "GET" "/api/profiles" :client (plist-get op :client) :timeout 30
           :current-p (lambda () (hermes-request--live-p op)))
          (lambda (catalogue) (hermes-request--create op catalogue)))
         (lambda (failure) (hermes-request--fail op failure)))
      ((error quit) (hermes-request--fail op reason)))))

(defun hermes-request--start (op)
  "Acquire OP's dashboard and asynchronously verify its profile catalogue."
  (condition-case reason
      (let* ((client (hermes-dashboard-transport-acquire))
             (current-p (lambda () (hermes-request--live-p op))))
        (setf (plist-get op :client) client
              (plist-get op :generation)
              (hermes-dashboard-transport-client-generation client))
        (if (not (funcall current-p)) (hermes-request--cleanup op)
          (setf (plist-get op :subscription)
                (hermes-dashboard-transport-subscribe
                 client (lambda (event) (hermes-request--event op event))
                 (lambda () (hermes-request--fail op "Hermes disconnected"))))
          ;; A cold spawn has a token before its HTTP listener is ready.
          (hermes-dashboard-transport--when-ready
           client
           (lambda () (hermes-request--catalogue op))
           (lambda (failure) (hermes-request--fail op failure)))))
    ((error quit) (hermes-request--fail op reason))))

;;;###autoload
(defun hermes-request (request resolve reject)
  "Run REQUEST in a fresh Hermes session; return a cancellation thunk.
REQUEST is a plist with :prompt string and :profile exact backend profile
name (1-64 lowercase ASCII letters, digits, underscores or hyphens, starting
with a letter or digit).  No current chat history is used or changed.
Capture the profile's current configured model and provider from the owning
backend catalogue; reject missing or malformed choices instead of using launch
defaults.  Credentials and configured fallbacks remain backend-owned.
RESOLVE receives the full final assistant string only after successful
admission and completion.  REJECT receives a failure reason on cancellation,
timeout, disconnect, invalid responses or required interactive input.
Callbacks run once, asynchronously on the Emacs event loop, with no buffer
context guarantee.  Invalid arguments signal before starting a request.
Use `hermes-request-timeout' for the overall deadline.  Session closure is
best effort, including late creation after cancellation; lost creation replies
cannot be cleaned up by handle.  Backend history retention still applies.
The selected profile controls tools and instructions; this is not a sandbox."
  (let ((prompt (plist-get request :prompt))
        (profile (plist-get request :profile))
        (case-fold-search nil))
    (unless (and (stringp prompt) (stringp profile)
                 (string-match-p "\\`[a-z0-9][a-z0-9_-]\\{0,63\\}\\'" profile)
                 (functionp resolve) (functionp reject)
                 (numberp hermes-request-timeout) (> hermes-request-timeout 0))
      (error "Invalid Hermes request, callbacks or timeout"))
    (let ((op (list :prompt (substring-no-properties prompt)
                    :profile (substring-no-properties profile)
                    :resolve resolve :reject reject :settled nil :timer nil
                    :client nil :generation nil :subscription nil :creating nil :closing nil
                    :session nil :submitted nil :accepted nil :terminal nil)))
      (setf (plist-get op :timer)
            (run-at-time hermes-request-timeout nil
                         #'hermes-request--fail op "Hermes request timed out"))
      (hermes-request--start op)
      (lambda () (hermes-request--fail op "Hermes request cancelled")))))

(provide 'hermes-request)
;;; hermes-request.el ends here
