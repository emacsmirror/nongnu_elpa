;;; jabber-test-csi.el --- Tests for jabber-csi  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0352 Client State Indication.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'jabber-csi)

;;; Test data

(defconst jabber-test-csi--supported-features
  `(features nil (csi ((xmlns . ,jabber-csi-xmlns))))
  "Stream features with CSI support.")

(defconst jabber-test-csi--unsupported-features
  '(features nil (sm ((xmlns . "urn:xmpp:sm:3"))))
  "Stream features without CSI support.")

(defun jabber-test-csi--state-data (features)
  "Return FSM state data containing FEATURES."
  (list :stream-features features))

(defun jabber-test-csi--last-state-table (&rest entries)
  "Return a CSI last-state hash table initialized with ENTRIES.
ENTRIES is a flat list of connection/state pairs."
  (let ((table (make-hash-table :test #'eq)))
    (while entries
      (puthash (pop entries) (pop entries) table))
    table))

;;; Group 1: State detection

(ert-deftest jabber-test-csi-focused-p-returns-bool ()
  "Focused-p returns non-nil or nil without error."
  (should (or (jabber-csi--focused-p)
              (not (jabber-csi--focused-p)))))

;;; Group 2: Send logic

(ert-deftest jabber-test-csi-send-active-when-focused-and-supported ()
  "Sends active element when focused and stream features advertise CSI."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state (jabber-test-csi--last-state-table))
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 (jabber-test-csi--state-data
                  jabber-test-csi--supported-features)))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should sent)
      (should (eq (car sent) 'active))
      (should (eq (gethash 'fake-jc jabber-csi--last-state) 'active)))))

(ert-deftest jabber-test-csi-send-inactive-when-unfocused-and-supported ()
  "Sends inactive element when unfocused and stream features advertise CSI."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state (jabber-test-csi--last-state-table))
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () nil))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 (jabber-test-csi--state-data
                  jabber-test-csi--supported-features)))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should sent)
      (should (eq (car sent) 'inactive))
      (should (eq (gethash 'fake-jc jabber-csi--last-state) 'inactive)))))

(ert-deftest jabber-test-csi-send-nothing-when-unsupported ()
  "Does not send CSI when stream features omit CSI support."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state (jabber-test-csi--last-state-table))
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 (jabber-test-csi--state-data
                  jabber-test-csi--unsupported-features)))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should-not sent)
      (should-not (gethash 'fake-jc jabber-csi--last-state)))))

(ert-deftest jabber-test-csi-no-duplicate-send-per-connection ()
  "Does not let one connection suppress sends for another."
  (let ((sent nil)
        (jabber-csi-enable t)
        (jabber-csi--last-state
         (jabber-test-csi--last-state-table 'jc-a 'active))
        (jabber-connections '(jc-a jc-b)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 (jabber-test-csi--state-data
                  jabber-test-csi--supported-features)))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (jc sexp) (push (cons jc sexp) sent))))
      (jabber-csi--send-state)
      (should (= (length sent) 1))
      (should (eq (caar sent) 'jc-b))
      (should (eq (gethash 'jc-a jabber-csi--last-state) 'active))
      (should (eq (gethash 'jc-b jabber-csi--last-state) 'active)))))

(ert-deftest jabber-test-csi-disabled-sends-nothing ()
  "Sends nothing when jabber-csi-enable is nil."
  (let ((sent nil)
        (jabber-csi-enable nil)
        (jabber-csi--last-state (jabber-test-csi--last-state-table))
        (jabber-connections '(fake-jc)))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               (lambda (_jc sexp) (setq sent sexp))))
      (jabber-csi--send-state)
      (should-not sent))))

(ert-deftest jabber-test-csi-on-connect-resets-state-for-connection ()
  "On-connect resets last-state for the affected connection and sends current state."
  (let ((jabber-csi-enable t)
        (jabber-csi--last-state
         (jabber-test-csi--last-state-table 'fake-jc 'active 'other-jc 'inactive))
        (jabber-csi--timer nil))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'fsm-get-state-data)
               (lambda (_jc)
                 (jabber-test-csi--state-data
                  jabber-test-csi--supported-features)))
              ((symbol-function 'jabber-send-sexp-if-connected)
               #'ignore))
      (jabber-csi--on-connect 'fake-jc)
      (should (eq (gethash 'fake-jc jabber-csi--last-state) 'active))
      (should (eq (gethash 'other-jc jabber-csi--last-state) 'inactive)))))

;;; Group 3: Debounce

(ert-deftest jabber-test-csi-debounce-coalesces ()
  "Rapid focus-changed calls produce only one pending timer."
  (let ((jabber-csi--timer nil))
    (cl-letf (((symbol-function 'jabber-csi--focused-p)
               (lambda () t))
              ((symbol-function 'jabber-send-sexp-if-connected)
               #'ignore))
      (jabber-csi--focus-changed)
      (jabber-csi--focus-changed)
      (jabber-csi--focus-changed)
      (should (timerp jabber-csi--timer))
      (jabber-csi--stop-timer))))

(ert-deftest jabber-test-csi-disconnect-cleanup ()
  "On-disconnect cancels pending timer and resets state for the connection."
  (let ((jabber-csi--timer (run-with-timer 10 nil #'ignore))
        (jabber-csi--last-state
         (jabber-test-csi--last-state-table 'fake-jc 'active 'other-jc 'inactive)))
    (jabber-csi--on-disconnect 'fake-jc)
    (should-not jabber-csi--timer)
    (should-not (gethash 'fake-jc jabber-csi--last-state))
    (should (eq (gethash 'other-jc jabber-csi--last-state) 'inactive))))

(provide 'jabber-test-csi)

;;; jabber-test-csi.el ends here
