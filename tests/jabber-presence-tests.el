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

(ert-deftest jabber-presence-test-process-item-no-groups ()
  "Item with no group children yields nil groups property."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (name . "Bob")
                         (subscription . "both"))))
           (result (jabber--roster-process-item item nil nil)))
      (should-not (get (cdr result) 'groups)))))

(ert-deftest jabber-presence-test-process-item-no-name ()
  "Item with no name attribute yields nil name but sets subscription."
  (jabber-presence-test-with-obarray
    (let* ((item '(item ((jid . "bob@example.com")
                         (subscription . "to"))))
           (result (jabber--roster-process-item item nil nil)))
      (should-not (get (cdr result) 'name))
      (should (equal (get (cdr result) 'subscription) "to")))))

;;; ---- Group 3: jabber-presence--extract-metadata ----

(ert-deftest jabber-presence-test-extract-metadata-all-fields ()
  "All fields are extracted from a fully populated presence stanza."
  (let* ((xml '(presence ((from . "bob@example.com/phone"))
                         (show () "away")
                         (status () "On the phone")
                         (priority () "5")
                         (error ((type . "modify"))
                                (bad-request ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas"))))))
         (result (jabber-presence--extract-metadata xml)))
    (should (equal (plist-get result :show) "away"))
    (should (equal (plist-get result :status) "On the phone"))
    (should (equal (plist-get result :priority) 5))
    (should (consp (plist-get result :error)))))

(ert-deftest jabber-presence-test-extract-metadata-missing-elements ()
  "Missing child elements yield nil (or 0 for priority)."
  (let* ((xml '(presence ((from . "bob@example.com"))))
         (result (jabber-presence--extract-metadata xml)))
    (should (null (plist-get result :show)))
    (should (null (plist-get result :status)))
    (should (equal (plist-get result :priority) 0))
    (should (null (plist-get result :error)))))

(ert-deftest jabber-presence-test-extract-metadata-only-status ()
  "Only the status element is present."
  (let* ((xml '(presence ((from . "bob@example.com"))
                         (status () "BRB")))
         (result (jabber-presence--extract-metadata xml)))
    (should (null (plist-get result :show)))
    (should (equal (plist-get result :status) "BRB"))
    (should (equal (plist-get result :priority) 0))
    (should (null (plist-get result :error)))))

(ert-deftest jabber-presence-test-extract-metadata-negative-priority ()
  "Negative priority is parsed correctly."
  (let* ((xml '(presence ((from . "bob@example.com"))
                         (priority () "-1")))
         (result (jabber-presence--extract-metadata xml)))
    (should (equal (plist-get result :priority) -1))))

;;; ---- Group 4: jabber-presence--update-resource ----

(ert-deftest jabber-presence-test-update-resource-normal-presence ()
  "Normal presence sets connected, show, status, priority on resource plist."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show "away" :status "BRB" :priority 5 :error nil))
           (result (jabber-presence--update-resource buddy nil "phone" metadata))
           (newstatus (car result))
           (rplist (cdr result)))
      (should (equal newstatus "away"))
      (should (eq (plist-get rplist 'connected) t))
      (should (equal (plist-get rplist 'show) "away"))
      (should (equal (plist-get rplist 'status) "BRB"))
      (should (equal (plist-get rplist 'priority) 5)))))

(ert-deftest jabber-presence-test-update-resource-unavailable ()
  "Unavailable presence clears connected and show on resource plist."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show nil :status "Goodbye" :priority 0 :error nil))
           (result (jabber-presence--update-resource
                    buddy "unavailable" "phone" metadata))
           (newstatus (car result))
           (rplist (cdr result)))
      (should (null newstatus))
      (should (null (plist-get rplist 'connected)))
      (should (null (plist-get rplist 'show)))
      (should (equal (plist-get rplist 'status) "Goodbye")))))

(ert-deftest jabber-presence-test-update-resource-bare-jid-unavailable ()
  "Bare JID unavailable clears all buddy resources and properties."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray)))
      (put buddy 'resources '(("phone" connected t show "away")))
      (put buddy 'connected t)
      (put buddy 'show "away")
      (let* ((metadata '(:show nil :status "Gone" :priority 0 :error nil))
             (result (jabber-presence--update-resource
                      buddy "unavailable" "" metadata))
             (newstatus (car result))
             (rplist (cdr result)))
        (should (null newstatus))
        (should (null rplist))
        (should (null (get buddy 'resources)))
        (should (null (get buddy 'connected)))
        (should (null (get buddy 'show)))
        (should (equal (get buddy 'status) "Gone"))))))

(ert-deftest jabber-presence-test-update-resource-error ()
  "Error presence sets show to error and connected to nil."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show nil :status "something" :priority 0 :error nil))
           (result (jabber-presence--update-resource
                    buddy "error" "phone" metadata))
           (newstatus (car result))
           (rplist (cdr result)))
      (should (equal newstatus "error"))
      (should (null (plist-get rplist 'connected)))
      (should (equal (plist-get rplist 'show) "error"))
      (should (equal (plist-get rplist 'status) "something")))))

(ert-deftest jabber-presence-test-update-resource-subscribed ()
  "Subscribed type sets newstatus without modifying resource plist."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show nil :status nil :priority 0 :error nil))
           (result (jabber-presence--update-resource
                    buddy "subscribed" "phone" metadata))
           (newstatus (car result)))
      (should (equal newstatus "subscribed")))))

(ert-deftest jabber-presence-test-update-resource-default-show ()
  "Normal presence with nil show defaults show to empty string."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show nil :status "Online" :priority 0 :error nil))
           (result (jabber-presence--update-resource buddy nil "laptop" metadata))
           (newstatus (car result))
           (rplist (cdr result)))
      (should (equal newstatus ""))
      (should (equal (plist-get rplist 'show) "")))))

(ert-deftest jabber-presence-test-update-resource-unsubscribe ()
  "Unsubscribe type returns (\"unsubscribe\" . nil)."
  (jabber-presence-test-with-obarray
    (let* ((buddy (intern "bob@example.com" jabber-jid-obarray))
           (metadata '(:show nil :status nil :priority 0 :error nil))
           (result (jabber-presence--update-resource
                    buddy "unsubscribe" "phone" metadata))
           (newstatus (car result)))
      (should (equal newstatus "unsubscribe")))))

(provide 'jabber-presence-tests)
;;; jabber-presence-tests.el ends here
