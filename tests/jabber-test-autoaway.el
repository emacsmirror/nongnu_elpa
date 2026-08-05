;;; jabber-test-autoaway.el --- Tests for jabber-autoaway  -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoaway presence selection.

;;; Code:

(require 'ert)
(require 'jabber-autoaway)

(defvar jabber-current-priority)
(defvar jabber-current-show)
(defvar jabber-current-status)
(defvar jabber-default-status)

(defun jabber-test-autoaway--presence (xa show away-priority xa-priority)
  "Return presence sent for XA and SHOW using AWAY-PRIORITY and XA-PRIORITY."
  (let ((jabber-current-show show)
        (jabber-current-status "Available")
        (jabber-current-priority 1)
        (jabber-default-status "Available")
        (jabber-autoaway-status "Idle")
        (jabber-autoaway-xa-status "Extended away")
        (jabber-autoaway-priority away-priority)
        (jabber-autoaway-xa-priority xa-priority)
        sent)
    (cl-letf (((symbol-function 'jabber-send-presence)
               (lambda (&rest args) (setq sent args)))
              ((symbol-function 'jabber-autoaway-get-idle-time)
               (lambda () 600))
              ((symbol-function 'run-with-timer) #'ignore))
      (jabber-autoaway-set-idle xa))
    sent))

(ert-deftest jabber-test-autoaway-uses-away-priority ()
  "Away presence uses `jabber-autoaway-priority'."
  (should (equal '("away" "Idle" 5)
                 (jabber-test-autoaway--presence nil nil 5 9))))

(ert-deftest jabber-test-autoaway-uses-xa-priority ()
  "Extended-away presence uses `jabber-autoaway-xa-priority'."
  (should (equal '("xa" "Extended away" 9)
                 (jabber-test-autoaway--presence t "away" 5 9))))

(ert-deftest jabber-test-autoaway-nil-priorities-preserve-current-priority ()
  "Nil away and XA priorities preserve the current priority."
  (should (equal '("away" "Idle" 1)
                 (jabber-test-autoaway--presence nil nil nil 9)))
  (should (equal '("xa" "Extended away" 1)
                 (jabber-test-autoaway--presence t "away" 5 nil))))

(ert-deftest jabber-test-autoaway-preserves-dnd-and-xa ()
  "Autoaway does not replace explicit DND or extended-away presence."
  (should-not (jabber-test-autoaway--presence nil "dnd" 5 9))
  (should-not (jabber-test-autoaway--presence nil "xa" 5 9)))

(provide 'jabber-test-autoaway)
;;; jabber-test-autoaway.el ends here
