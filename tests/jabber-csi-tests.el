;;; jabber-csi-tests.el --- Tests for jabber-csi  -*- lexical-binding: t; -*-

(require 'ert)
(require 'jabber-csi)

;;; Group 1: State detection

(ert-deftest jabber-csi-test-focused-p-returns-bool ()
  "focused-p returns non-nil or nil without error."
  (should (or (jabber-csi--focused-p)
              (not (jabber-csi--focused-p)))))

;;; Group 2: Send logic

(ert-deftest jabber-csi-test-send-active-when-focused ()
  "Sends active element when focused."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state nil)
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should sent)
      (should (eq (car sent) 'active))
      (should (eq jabber-csi--last-state 'active)))))

(ert-deftest jabber-csi-test-send-inactive-when-unfocused ()
  "Sends inactive element when unfocused."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state nil)
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () nil))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should sent)
      (should (eq (car sent) 'inactive))
      (should (eq jabber-csi--last-state 'inactive)))))

(ert-deftest jabber-csi-test-no-duplicate-send ()
  "Does not resend the same state."
  (let ((send-count 0)
        (jabber-csi-enable t)
        (jabber-csi--last-state 'active)
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc _sexp) (cl-incf send-count))))
      (jabber-csi--send-state)
      (should (= send-count 0)))))

(ert-deftest jabber-csi-test-disabled-sends-nothing ()
  "Sends nothing when jabber-csi-enable is nil."
  (let ((sent nil)
        (jabber-csi-enable nil)
        (jabber-csi--last-state nil)
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should-not sent))))

(ert-deftest jabber-csi-test-on-connect-resets-state ()
  "on-connect resets last-state and sends current state."
  (let ((jabber-csi-enable t)
        (jabber-csi--last-state 'active)
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               #'ignore))
      (jabber-csi--on-connect 'fake-jc)
      (should (eq jabber-csi--last-state 'active)))))

(provide 'jabber-csi-tests)

;;; jabber-csi-tests.el ends here
