;;; jabber-test-keepalive.el --- Tests for jabber-keepalive  -*- lexical-binding: t; -*-

;;; Commentary:

;; Keepalive timer lifecycle.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'jabber-keepalive)

(defun jabber-test-keepalive--mock-timer-p (object)
  "Return non-nil when OBJECT is a mock keepalive timer."
  (and (consp object) (eq (car object) 'mock-timer)))

(ert-deftest jabber-test-keepalive-do-replaces-timeout-timer ()
  "Rearming keepalive cancels the previous timeout timer."
  (let ((jabber-connections '(first second))
        (jabber-keepalive-timeout-timer nil)
        (jabber-keepalive-pending nil)
        (live-timers nil)
        (cancelled-timers nil)
        (next-timer-id 0))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args)
                 (let ((timer (list 'mock-timer (cl-incf next-timer-id))))
                   (push timer live-timers)
                   timer)))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq live-timers (delq timer live-timers))
                 (push timer cancelled-timers)))
              ((symbol-function 'timerp)
               #'jabber-test-keepalive--mock-timer-p)
              ((symbol-function 'jabber-ping-send) #'ignore))
      (jabber-keepalive-do)
      (let ((first-timer jabber-keepalive-timeout-timer))
        (jabber-keepalive-do)
        (should (equal cancelled-timers (list first-timer)))
        (should (= (length live-timers) 1))
        (should (eq (car live-timers) jabber-keepalive-timeout-timer))
        (should-not (eq first-timer jabber-keepalive-timeout-timer))))))

(ert-deftest jabber-test-keepalive-start-clears-previous-round ()
  "Restarting keepalive clears timers and pending state from the old round."
  (let* ((recurring-timer (list 'mock-timer 'recurring))
         (timeout-timer (list 'mock-timer 'timeout))
         (replacement-timer (list 'mock-timer 'replacement))
         (jabber-keepalive-timer recurring-timer)
         (jabber-keepalive-timeout-timer timeout-timer)
         (jabber-keepalive-pending '(stale))
         (jabber-post-disconnect-hook nil)
         (cancelled-timers nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args) replacement-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer cancelled-timers)))
              ((symbol-function 'timerp)
               #'jabber-test-keepalive--mock-timer-p))
      (jabber-keepalive-start)
      (should (eq jabber-keepalive-timer replacement-timer))
      (should (null jabber-keepalive-timeout-timer))
      (should (null jabber-keepalive-pending))
      (should (equal cancelled-timers
                     (list timeout-timer recurring-timer))))))

(ert-deftest jabber-test-keepalive-start-clears-timeout-without-recurring-timer ()
  "Restarting keepalive clears a stale round without a recurring timer."
  (let* ((timeout-timer (list 'mock-timer 'timeout))
         (replacement-timer (list 'mock-timer 'replacement))
         (jabber-keepalive-timer nil)
         (jabber-keepalive-timeout-timer timeout-timer)
         (jabber-keepalive-pending '(stale))
         (jabber-post-disconnect-hook nil)
         (cancelled-timers nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args) replacement-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer cancelled-timers)))
              ((symbol-function 'timerp)
               #'jabber-test-keepalive--mock-timer-p))
      (jabber-keepalive-start)
      (should (eq jabber-keepalive-timer replacement-timer))
      (should (null jabber-keepalive-timeout-timer))
      (should (null jabber-keepalive-pending))
      (should (equal cancelled-timers (list timeout-timer))))))

(provide 'jabber-test-keepalive)

;;; jabber-test-keepalive.el ends here
