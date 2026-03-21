;;; jabber-modeline-tests.el --- Tests for jabber-modeline debounce  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../lisp/jabber-modeline.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: jabber-mode-line-count-contacts debounce

(ert-deftest jabber-modeline-test-debounce-coalesces-calls ()
  "Rapid calls to jabber-mode-line-count-contacts leave exactly one pending timer."
  (let ((jabber-mode-line--recount-timer nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_delay _repeat fn)
                 (list 'mock-timer fn)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'timerp) (lambda (x) (and (consp x) (eq (car x) 'mock-timer)))))
      (jabber-mode-line-count-contacts)
      (let ((first jabber-mode-line--recount-timer))
        (jabber-mode-line-count-contacts)
        (jabber-mode-line-count-contacts)
        ;; Only the last timer is kept; there is exactly one pending timer.
        (should (timerp jabber-mode-line--recount-timer))
        ;; Each new call replaced the previous.
        (should-not (eq first jabber-mode-line--recount-timer))))))

(ert-deftest jabber-modeline-test-debounce-timer-fires-and-clears ()
  "When the debounce timer fires, jabber-mode-line--recount-timer is set to nil."
  (let ((jabber-mode-line--recount-timer nil)
        (jabber-connections nil)
        (jabber-mode-line-compact t)
        (jabber-mode-line-contacts ""))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (jabber-mode-line--do-count-contacts)
      (should (null jabber-mode-line--recount-timer)))))

(ert-deftest jabber-modeline-test-on-disconnect-cancels-timer ()
  "jabber-modeline--on-disconnect cancels a pending timer and leaves it nil."
  (let ((jabber-mode-line--recount-timer (list 'mock-timer))
        (jabber-connections nil)
        (jabber-mode-line-compact t)
        (jabber-mode-line-contacts "")
        (jabber-mode-line-presence "")
        (*jabber-disconnecting* t)
        (*jabber-current-show* nil))
    (cl-letf (((symbol-function 'timerp) (lambda (x) (and (consp x) (eq (car x) 'mock-timer))))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'force-mode-line-update) #'ignore)
              ((symbol-function 'jabber-activity--on-disconnect) #'ignore)
              ((symbol-function 'jabber-mode-line-presence-update) #'ignore))
      (jabber-modeline--on-disconnect)
      (should (null jabber-mode-line--recount-timer)))))

(provide 'jabber-modeline-tests)

;;; jabber-modeline-tests.el ends here
