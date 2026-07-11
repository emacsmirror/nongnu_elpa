;;; hermes-chat-handoff.el --- Session handoff for Hermes chat  -*- lexical-binding: t; -*-

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

;; Session handoff for `hermes-chat': the `handoff.request' command, the
;; live-platform target prompt from `complete.slash', and the
;; backoff-polled `handoff.state' watcher with its timeout fallback to
;; `handoff.fail'.  Part of the one logical chat module (see the require
;; note in `hermes-chat.el'); it preserves the existing `hermes-chat--*'
;; symbols.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hermes-transport)
(require 'hermes-dashboard-transport)
(require 'hermes-dashboard-rpc)
(require 'hermes-chat-buffer)
(require 'hermes-chat-dashboard)


(defvar hermes-chat--dashboard-active-session-id)
(defvar hermes-chat--dashboard-client)

;;; Session handoff

(defconst hermes-chat--handoff-poll-initial-delay 1
  "Seconds before the first `handoff.state' poll after a queued handoff.")

(defconst hermes-chat--handoff-poll-max-delay 8
  "Ceiling for the doubling delay between `handoff.state' polls.")

(defconst hermes-chat--handoff-poll-deadline 120
  "Seconds to keep polling `handoff.state' before marking the handoff failed.")

(defvar-local hermes-chat--handoff-poll nil
  "Active handoff poll state: a plist of :timer :backoff :platform :deadline.")

(defun hermes-chat--handoff-stop ()
  "Cancel any active handoff poll timer in the current buffer."
  (when hermes-chat--handoff-poll
    (when-let* ((timer (plist-get hermes-chat--handoff-poll :timer)))
      (cancel-timer timer))
    (setq hermes-chat--handoff-poll nil)))

(defun hermes-chat--handoff-schedule (buffer delay)
  "Schedule the next `handoff.state' poll for BUFFER after DELAY seconds."
  (when hermes-chat--handoff-poll
    (setq hermes-chat--handoff-poll
          (plist-put hermes-chat--handoff-poll :timer
                     (run-at-time delay nil
                                  #'hermes-chat--handoff-poll-tick buffer)))))

(defun hermes-chat--handoff-reschedule (buffer)
  "Double the BUFFER handoff poll backoff (capped) and schedule the next poll."
  (when hermes-chat--handoff-poll
    (let ((delay (min hermes-chat--handoff-poll-max-delay
                      (* 2 (plist-get hermes-chat--handoff-poll :backoff)))))
      (setq hermes-chat--handoff-poll
            (plist-put hermes-chat--handoff-poll :backoff delay))
      (hermes-chat--handoff-schedule buffer delay))))

(defun hermes-chat--handoff-report-failure (platform reason)
  "Surface a failed handoff to PLATFORM, appending REASON when non-empty."
  (hermes-chat--command-error
   (if (or (null reason) (string-empty-p reason))
       (format "Handoff to %s failed" platform)
     (format "Handoff to %s failed: %s" platform reason))))

(defun hermes-chat--handoff-handle-state (buffer result)
  "Settle or continue the BUFFER handoff given gateway RESULT."
  (let ((state (hermes-chat--result-string result 'state))
        (platform (plist-get hermes-chat--handoff-poll :platform)))
    (pcase (downcase (or state ""))
      ("completed"
       (hermes-chat--handoff-stop)
       (hermes-chat--insert-local-status
        (format "Session handed off to %s" platform) 'done)
       (hermes-chat--set-header-state
        :status 'done :activity (format "Handed off → %s" platform)))
      ("failed"
       (hermes-chat--handoff-stop)
       (hermes-chat--handoff-report-failure
        platform (hermes-chat--result-string result 'error)))
      (_ (hermes-chat--handoff-reschedule buffer)))))

(defun hermes-chat--handoff-timeout (buffer)
  "Fail the BUFFER handoff after the poll deadline elapses."
  (let ((platform (plist-get hermes-chat--handoff-poll :platform)))
    (hermes-chat--handoff-stop)
    (hermes-dashboard-transport-handoff-fail
     hermes-chat--dashboard-client
     :error "client poll timed out"
     :session-id hermes-chat--dashboard-active-session-id
     :resolve #'ignore :reject #'ignore)
    (hermes-chat--in-buffer buffer
      (hermes-chat--command-error
       (format "Handoff to %s timed out" platform)))))

(defun hermes-chat--handoff-poll-tick (buffer)
  "Run one `handoff.state' poll for BUFFER, settling or rescheduling."
  (hermes-chat--in-buffer buffer
    (when hermes-chat--handoff-poll
      (if (time-less-p (plist-get hermes-chat--handoff-poll :deadline)
                       (current-time))
          (hermes-chat--handoff-timeout buffer)
        (hermes-dashboard-transport-handoff-state
         hermes-chat--dashboard-client
         :session-id hermes-chat--dashboard-active-session-id
         :resolve (lambda (result)
                    (hermes-chat--in-buffer buffer
                      (hermes-chat--handoff-handle-state buffer result)))
         :reject (lambda (_message)
                   (hermes-chat--in-buffer buffer
                     (hermes-chat--handoff-reschedule buffer))))))))

(defun hermes-chat--handoff-start-poll (platform)
  "Begin bounded polling of `handoff.state' for PLATFORM in the current buffer."
  (hermes-chat--handoff-stop)
  (setq hermes-chat--handoff-poll
        (list :platform platform
              :backoff hermes-chat--handoff-poll-initial-delay
              :deadline (time-add (current-time)
                                  hermes-chat--handoff-poll-deadline)))
  (hermes-chat--handoff-schedule (current-buffer)
                                 hermes-chat--handoff-poll-initial-delay))

(defun hermes-chat--handoff-targets (result)
  "Return (PLATFORM . META) cells from a `complete.slash' RESULT.
Absent text or meta become empty strings so callers never see nil."
  (let ((items (hermes-transport--get result 'items)))
    (mapcar (lambda (item)
              (cons (or (hermes-chat--result-string item 'text) "")
                    (or (hermes-chat--result-string item 'meta) "")))
            (if (vectorp items) (append items nil) items))))

(defun hermes-chat--handoff-read-target (result)
  "Read a handoff platform from RESULT's live targets.
Offers the gateway's connected platforms with their home-channel hint, and
falls back to free-form input when the gateway reports none."
  (let ((targets (seq-remove (lambda (cell) (string-empty-p (car cell)))
                             (hermes-chat--handoff-targets result))))
    (downcase
     (string-trim
      (if targets
          (let ((completion-extra-properties
                 (list :annotation-function
                       (lambda (cand)
                         (and-let* ((meta (cdr (assoc cand targets)))
                                    ((not (string-empty-p meta))))
                           (concat "  " meta))))))
            (completing-read "Hand off to: " (mapcar #'car targets) nil nil))
        (read-string "Hand off to (no live platforms found): "))))))

(defun hermes-chat--handoff-begin (buffer platform)
  "Queue a handoff of BUFFER's session to PLATFORM and start polling."
  (hermes-chat--in-buffer buffer
    (hermes-chat--insert-local-status
     (format "Requesting handoff to %s…" platform) 'handoff)
    (hermes-chat--set-header-state :status 'handoff :activity platform)
    (hermes-dashboard-transport-handoff-request
     hermes-chat--dashboard-client platform
     :session-id hermes-chat--dashboard-active-session-id
     :resolve (lambda (_result)
                (hermes-chat--in-buffer buffer
                  (hermes-chat--handoff-start-poll platform)))
     :reject (lambda (message)
               (hermes-chat--in-buffer buffer
                 (hermes-chat--command-error message))))))

(defun hermes-chat--handoff-prompt-platform (buffer)
  "Fetch live handoff targets for BUFFER, then prompt and begin the handoff."
  (let ((pick (lambda (result)
                (hermes-chat--in-buffer buffer
                  (let ((platform (hermes-chat--handoff-read-target result)))
                    (unless (string-empty-p platform)
                      (hermes-chat--handoff-begin buffer platform)))))))
    (hermes-dashboard-transport-complete-slash
     hermes-chat--dashboard-client "/handoff "
     :resolve pick
     :reject (lambda (_message) (funcall pick nil)))))

(defun hermes-chat-handoff (&optional platform)
  "Hand off the current session to a messaging PLATFORM and poll for the result.
Without PLATFORM, prompt with the gateway's live connected platforms -- the same
targets the dashboard offers -- rather than a fixed list.  The chat must have an
attached, idle session; the gateway transfers it to the platform's home channel."
  (interactive)
  (unless (hermes-chat--dashboard-session-attached-p)
    (user-error "This Hermes chat has no session to hand off"))
  (when (hermes-chat--active-turn-p)
    (user-error "Wait for the current turn to finish before handing off"))
  (let ((buffer (current-buffer))
        (given (and platform
                    (not (string-empty-p (string-trim platform)))
                    (downcase (string-trim platform)))))
    (if given
        (hermes-chat--handoff-begin buffer given)
      (hermes-chat--handoff-prompt-platform buffer))))

(provide 'hermes-chat-handoff)
;;; hermes-chat-handoff.el ends here
