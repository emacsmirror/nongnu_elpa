;;; hermes-promise.el --- Minimal single-shot promises  -*- lexical-binding: t; -*-

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

;; A tiny single-shot promise used to compose the dashboard RPC channel
;; without nesting `:resolve'/`:reject' callbacks at every call site.  A
;; promise settles once -- to resolved or rejected -- and fans its value
;; out to the handlers registered with `hermes--promise-then'.  Callbacks
;; already arrive from timers and the websocket filter, so there is no
;; microtask queue: handlers fire synchronously when the promise settles
;; (or immediately, if it has settled already).
;;
;; This is deliberately not a full Promises/A+ implementation; it covers
;; the single-request, resolve-once shape the transport needs.

;;; Code:

(require 'cl-lib)

(cl-defstruct (hermes--promise (:constructor hermes--promise-make))
  "A single-shot promise that settles once to resolved or rejected.
STATE is `pending', `resolved', or `rejected'.  VALUE is the resolved value
or the rejection reason.  ON-RESOLVE and ON-REJECT are the pending callback
queues, dropped once the promise settles."
  (state 'pending)
  value
  on-resolve
  on-reject)

(defun hermes--promise-settle (promise state value callbacks)
  "Settle PROMISE to STATE with VALUE, running queued CALLBACKS once.
A no-op when PROMISE is already settled, giving resolve-once semantics."
  (when (eq (hermes--promise-state promise) 'pending)
    (setf (hermes--promise-state promise) state
          (hermes--promise-value promise) value
          (hermes--promise-on-resolve promise) nil
          (hermes--promise-on-reject promise) nil)
    (dolist (cb (reverse callbacks))
      (funcall cb value))))

(defun hermes--promise-resolve (promise value)
  "Resolve PROMISE with VALUE unless it has already settled."
  (hermes--promise-settle promise 'resolved value
                          (hermes--promise-on-resolve promise)))

(defun hermes--promise-reject (promise reason)
  "Reject PROMISE with REASON unless it has already settled."
  (hermes--promise-settle promise 'rejected reason
                          (hermes--promise-on-reject promise)))

(defun hermes--promise-subscribe (promise on-resolve on-reject)
  "Register ON-RESOLVE and ON-REJECT on PROMISE, firing now if already settled."
  (pcase (hermes--promise-state promise)
    ('pending
     (push on-resolve (hermes--promise-on-resolve promise))
     (push on-reject (hermes--promise-on-reject promise)))
    ('resolved (funcall on-resolve (hermes--promise-value promise)))
    ('rejected (funcall on-reject (hermes--promise-value promise)))))

(defun hermes--promise-run (next handler value resolved)
  "Apply HANDLER to VALUE and settle NEXT with the outcome.
When HANDLER is nil the settlement passes through: NEXT resolves with VALUE if
RESOLVED is non-nil, else rejects with it.  A HANDLER that returns a promise is
adopted; one that signals rejects NEXT with the error message.  `quit' is
caught too: a \\[keyboard-quit] inside a handler (say, at a minibuffer prompt
mid-chain) must still settle NEXT, or `hermes--promise-finally' cleanups
downstream never run."
  (cond
   ((null handler)
    (if resolved
        (hermes--promise-resolve next value)
      (hermes--promise-reject next value)))
   (t
    (condition-case err
        (let ((result (funcall handler value)))
          (if (hermes--promise-p result)
              (hermes--promise-subscribe
               result
               (lambda (v) (hermes--promise-resolve next v))
               (lambda (r) (hermes--promise-reject next r)))
            (hermes--promise-resolve next result)))
      ((error quit) (hermes--promise-reject next (error-message-string err)))))))

(defun hermes--promise-then (promise on-resolve &optional on-reject)
  "Return a promise applying ON-RESOLVE or ON-REJECT to PROMISE's settled value.
ON-RESOLVE receives the resolved value; ON-REJECT receives the rejection reason.
A handler's return value settles the new promise (another promise is chained); a
missing handler passes the settlement through; a signalling handler rejects."
  (let ((next (hermes--promise-make)))
    (hermes--promise-subscribe
     promise
     (lambda (value) (hermes--promise-run next on-resolve value t))
     (lambda (reason) (hermes--promise-run next on-reject reason nil)))
    next))

(defun hermes--promise-map (promise fn)
  "Return a promise resolving to FN applied to PROMISE's resolved value."
  (hermes--promise-then promise fn))

(defun hermes--promise-catch (promise fn)
  "Return a promise applying FN to PROMISE's rejection reason."
  (hermes--promise-then promise nil fn))

(defun hermes--promise-all (promises)
  "Return a promise resolving to a list of PROMISES' values, in order.
It rejects with the reason of the first of PROMISES to reject."
  (let* ((next (hermes--promise-make))
         (total (length promises))
         (results (make-vector total nil))
         (remaining total)
         (index 0))
    (if (zerop total)
        (hermes--promise-resolve next nil)
      (dolist (promise promises)
        (let ((slot index))
          (hermes--promise-subscribe
           promise
           (lambda (value)
             (aset results slot value)
             (when (zerop (setq remaining (1- remaining)))
               (hermes--promise-resolve next (append results nil))))
           (lambda (reason) (hermes--promise-reject next reason))))
        (setq index (1+ index))))
    next))

(defun hermes--promise-resolved (value)
  "Return a promise already resolved with VALUE."
  (let ((promise (hermes--promise-make)))
    (hermes--promise-resolve promise value)
    promise))

(defun hermes--promise-rejected (reason)
  "Return a promise already rejected with REASON."
  (let ((promise (hermes--promise-make)))
    (hermes--promise-reject promise reason)
    promise))

(defun hermes--promise-finally (promise fn)
  "Run FN for its side effect when PROMISE settles, passing the settlement on.
Return a new promise that mirrors PROMISE's resolution or rejection after FN.
If FN signals (including `quit'), the returned promise rejects with that
error rather than stranding the chain in a pending state."
  (let ((next (hermes--promise-make)))
    (hermes--promise-subscribe
     promise
     (lambda (value)
       (condition-case err
           (progn (funcall fn) (hermes--promise-resolve next value))
         ((error quit) (hermes--promise-reject next (error-message-string err)))))
     (lambda (reason)
       (condition-case err
           (progn (funcall fn) (hermes--promise-reject next reason))
         ((error quit) (hermes--promise-reject next (error-message-string err))))))
    next))

(provide 'hermes-promise)
;;; hermes-promise.el ends here
