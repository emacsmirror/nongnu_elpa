;;; jabber-presence-tests.el --- Tests for jabber-presence  -*- lexical-binding: t; -*-

(require 'ert)

;; Pre-define variables expected at load time.
(defvar jabber-body-printers nil)
(defvar jabber-message-chain nil)
(defvar jabber-presence-chain nil)
(defvar jabber-iq-chain nil)

(require 'jabber-presence)

;;; ---- Group 1: jabber--roster-valid-push-p ----

(ert-deftest jabber-presence-test-valid-push-nil-from ()
  "Absent from attribute is valid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should (jabber--roster-valid-push-p nil state))))

(ert-deftest jabber-presence-test-valid-push-bare-server ()
  "From matching bare server is valid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should (jabber--roster-valid-push-p "example.com" state))))

(ert-deftest jabber-presence-test-valid-push-bare-jid ()
  "From matching bare JID is valid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should (jabber--roster-valid-push-p "alice@example.com" state))))

(ert-deftest jabber-presence-test-valid-push-full-jid ()
  "From matching full JID is valid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should (jabber--roster-valid-push-p "alice@example.com/emacs" state))))

(ert-deftest jabber-presence-test-invalid-push-wrong-server ()
  "From with wrong server is invalid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should-not (jabber--roster-valid-push-p "evil.com" state))))

(ert-deftest jabber-presence-test-invalid-push-wrong-jid ()
  "From with wrong JID is invalid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should-not (jabber--roster-valid-push-p "bob@example.com" state))))

(ert-deftest jabber-presence-test-invalid-push-wrong-resource ()
  "From with wrong resource is invalid."
  (let ((state '(:username "alice" :server "example.com" :resource "emacs")))
    (should-not (jabber--roster-valid-push-p "alice@example.com/phone" state))))

;;; ---- Group 2: jabber--roster-process-item ----

(defmacro jabber-presence-test-with-obarray (&rest body)
  "Run BODY with a fresh `jabber-jid-obarray'."
  (declare (indent 0))
  `(let ((jabber-jid-obarray (make-vector 127 0)))
     ,@body))

(ert-deftest jabber-presence-test-process-item-new ()
  "New roster item returns (new . sym) and sets properties."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (name . "Bob")
                         (subscription . "both"))
                        (group () "Friends")))
           (result (jabber--roster-process-item item nil nil)))
      (should (eq (car result) 'new))
      (let ((sym (cdr result)))
        (should (equal (get sym 'name) "Bob"))
        (should (equal (get sym 'subscription) "both"))
        (should (equal (get sym 'groups) '("Friends")))))))

(ert-deftest jabber-presence-test-process-item-changed ()
  "Existing roster item returns (changed . sym)."
  (jabber-presence-test-with-obarray
    (let* ((jid (intern "bob@example.com" jabber-jid-obarray))
           (roster (list jid))
           (item '(item ((jid . "bob@example.com")
                         (name . "Bobby")
                         (subscription . "both"))))
           (result (jabber--roster-process-item item roster nil)))
      (should (eq (car result) 'changed))
      (should (eq (cdr result) jid))
      (should (equal (get jid 'name) "Bobby")))))

(ert-deftest jabber-presence-test-process-item-deleted ()
  "Item with subscription=remove returns (deleted . sym)."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (subscription . "remove"))))
           (result (jabber--roster-process-item item nil nil)))
      (should (eq (car result) 'deleted)))))

(ert-deftest jabber-presence-test-process-item-initial-clears-plist ()
  "Initial push clears existing plist properties."
  (jabber-presence-test-with-obarray
    (let* ((jid (intern "bob@example.com" jabber-jid-obarray))
           (item '(item ((jid . "bob@example.com")
                         (name . "Bob")
                         (subscription . "both")))))
      (put jid 'show "away")
      (put jid 'connected t)
      (jabber--roster-process-item item nil t)
      ;; Stale properties should be gone.
      (should-not (get jid 'show))
      (should-not (get jid 'connected))
      ;; Fresh properties should be set.
      (should (equal (get jid 'name) "Bob")))))

(ert-deftest jabber-presence-test-process-item-multiple-groups ()
  "Multiple group elements are collected."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (subscription . "both"))
                        (group () "Friends")
                        (group () "Coworkers")))
           (result (jabber--roster-process-item item nil nil)))
      (should (equal (get (cdr result) 'groups)
                     '("Friends" "Coworkers"))))))

(ert-deftest jabber-presence-test-process-item-ask-property ()
  "The ask attribute is stored on the symbol."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (subscription . "none")
                         (ask . "subscribe"))))
           (result (jabber--roster-process-item item nil nil)))
      (should (equal (get (cdr result) 'ask) "subscribe")))))

(provide 'jabber-presence-tests)
;;; jabber-presence-tests.el ends here
