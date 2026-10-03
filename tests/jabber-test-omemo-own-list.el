;;; jabber-test-omemo-own-list.el --- Own OMEMO list safety tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is part of emacs-jabber.
;; emacs-jabber is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Exercise the real own-list mutation callers and PubSub response boundary.
;; Native keys, persistence and wire effects use disposable synthetic fixtures.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'jabber-omemo)
(require 'xml)

(defun jabber-test-own-list--result (ids)
  "Return a successful own device-list response containing IDS."
  `(iq ((type . "result") (from . "me@example.org"))
       (pubsub ((xmlns . ,jabber-pubsub-xmlns))
               (items ((node . ,jabber-omemo-devicelist-node))
                      (item ((id . "current"))
                            ,(jabber-omemo--build-device-list-xml ids))))))

(defun jabber-test-own-list--error (condition)
  "Return an error response with standard stanza CONDITION."
  `(iq ((type . "error") (from . "me@example.org"))
       (error ((type . "cancel"))
              (,condition ((xmlns . ,jabber-stanzas-xmlns))))))

(defmacro jabber-test-own-list--with-fixture (&rest body)
  "Evaluate BODY with held PubSub callbacks and counted effects."
  (declare (indent 0) (debug t))
  `(let ((state (list :username "me" :server "example.org"
                      :connection (list 'transport) :session-id "stream"
                      :blocking-session (list nil)))
         (jabber-omemo--device-lists (make-hash-table :test #'equal))
         success failure published deleted cleaned persisted deactivated
         (completed 0))
     (cl-letf (((symbol-function 'fsm-get-state-data) (lambda (_) state))
               ((symbol-function 'jabber-connection-bare-jid)
                (lambda (_) "me@example.org"))
               ((symbol-function 'jabber-omemo--get-device-id) (lambda (_) 222))
               ((symbol-function 'jabber-pubsub-request)
                (lambda (_jc _jid _node ok err)
                  (setq success ok failure err)))
               ((symbol-function 'jabber-omemo--publish-device-list)
                (lambda (_jc ids) (push ids published)))
               ((symbol-function 'jabber-omemo--cleanup-stale-devices)
                (lambda (_jc ids) (push ids cleaned)))
               ((symbol-function 'jabber-omemo--delete-bundle-node)
                (lambda (_jc id) (push id deleted)))
               ((symbol-function 'jabber-pubsub-delete-node)
                (lambda (jc _jid node &optional callback _error)
                  (push node deleted)
                  (when callback (funcall callback jc nil nil))))
               ((symbol-function 'jabber-omemo-store-save-device)
                (lambda (_account _jid id) (push id persisted)))
               ((symbol-function 'jabber-omemo--deactivate-stale-devices)
                (lambda (_account _jid ids) (push ids deactivated))))
       ;; Each journey inspects only the effects relevant to its contract.
       (ignore success failure published deleted cleaned persisted deactivated completed)
       ,@body)))

(defun jabber-test-own-list--mutate (operation)
  "Start the own-list mutation named by OPERATION."
  (pcase operation
    ('ensure (jabber-omemo--ensure-device-listed 'account))
    ('stale (jabber-omemo--remove-stale-devices 'account '(111)))
    ('remove (jabber-omemo--remove-device 'account 111))))

(ert-deftest jabber-test-own-list-failure-no-destructive-effects ()
  "All own-list mutations refuse uncertain errors without cache or DB changes."
  (dolist (operation '(ensure stale remove))
    (dolist (condition '(service-unavailable remote-server-timeout forbidden
                        internal-server-error feature-not-implemented))
      (jabber-test-own-list--with-fixture
        (jabber-test-own-list--mutate operation)
        (funcall failure 'account (jabber-test-own-list--error condition) nil)
        (should-not published)
        (should-not deleted)
        (should-not cleaned)
        (should-not persisted)
        (should-not deactivated)
        (should (= (hash-table-count jabber-omemo--device-lists) 0))))))

(ert-deftest jabber-test-own-list-first-install ()
  "A successful empty list or a missing node permits first-install publication."
  (dolist (missing '(nil t))
    (jabber-test-own-list--with-fixture
      (jabber-omemo--ensure-device-listed 'account)
      (if missing
          (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
        (funcall success 'account (jabber-test-own-list--result nil) nil))
      (should (equal published '((222))))
      (should (equal cleaned '(nil))))))

(ert-deftest jabber-test-own-list-success-preserves-other-devices ()
  "Fresh publication preserves other IDs; removals filter only selected IDs."
  (dolist (operation '(ensure stale remove))
    (jabber-test-own-list--with-fixture
      (jabber-test-own-list--mutate operation)
      (funcall success 'account (jabber-test-own-list--result '(111 333)) nil)
      (should (equal published (if (eq operation 'ensure)
                                  '((222 111 333)) '((333)))))
      (should (equal (sort persisted #'<) '(111 333)))
      (should (equal deactivated '((111 333)))))))

(ert-deftest jabber-test-own-list-already-listed ()
  "An already listed device requires no publication or stale cleanup."
  (jabber-test-own-list--with-fixture
    (jabber-omemo--ensure-device-listed 'account)
    (funcall success 'account (jabber-test-own-list--result '(111 222)) nil)
    (should-not published)
    (should-not cleaned)))

(ert-deftest jabber-test-own-list-no-response ()
  "Waiting for a response never publishes or deletes anything."
  (dolist (operation '(ensure stale remove))
    (jabber-test-own-list--with-fixture
      (jabber-test-own-list--mutate operation)
      (should success)
      (should failure)
      (should-not published)
      (should-not deleted)
      (should-not deactivated))))

(ert-deftest jabber-test-own-list-retired-session ()
  "Both success and missing-node replies from a retired session are refused."
  (dolist (operation '(ensure stale remove))
    (dolist (field '(:connection :session-id :blocking-session :username :server))
      (dolist (missing '(nil t))
        (jabber-test-own-list--with-fixture
          (jabber-test-own-list--mutate operation)
          (setq state (plist-put (copy-sequence state) field (list 'replacement)))
          (if missing
              (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
            (funcall success 'account (jabber-test-own-list--result '(111)) nil))
          (should-not published)
          (should-not deleted)
          (should-not persisted)
          (should-not deactivated))))))

(ert-deftest jabber-test-own-list-same-session-state-replacement ()
  "Ordinary FSM state copying does not retire a valid own-list response."
  (jabber-test-own-list--with-fixture
    (jabber-omemo--ensure-device-listed 'account)
    (setq state (copy-sequence state))
    (funcall success 'account (jabber-test-own-list--result '(111)) nil)
    (should (equal published '((222 111))))))

(ert-deftest jabber-test-own-list-malformed-result ()
  "Incomplete and malformed payloads cannot authorize destructive mutations."
  (dolist (xml (list '(iq ((type . "result")))
                    '(iq ((type . "result")) (pubsub nil (items nil)))
                    (jabber-test-own-list--result '(0))
                    (jabber-test-own-list--result '(2147483648))
                    '(iq ((type . "result"))
                         (pubsub ((xmlns . "http://jabber.org/protocol/pubsub"))
                                 (items ((node . "wrong")))))))
    (jabber-test-own-list--with-fixture
      (jabber-omemo--ensure-device-listed 'account)
      (funcall success 'account xml nil)
      (should-not published)
      (should-not deactivated))))

(ert-deftest jabber-test-own-list-legacy-peer-callback ()
  "Legacy peer callbacks remain fixed one-argument functions with nil failure."
  (jabber-test-own-list--with-fixture
    (jabber-omemo--fetch-device-list
     'account "peer@example.net"
     (lambda (ids) (cl-incf completed) (should-not ids)))
    (funcall failure 'account (jabber-test-own-list--error 'service-unavailable) nil)
    (should (= completed 1)))
  (jabber-test-own-list--with-fixture
    (jabber-omemo--fetch-device-list
     'account "peer@example.net"
     (lambda (ids) (cl-incf completed) (should (equal ids '(111)))))
    (funcall success 'account (jabber-test-own-list--result '(111)) nil)
    (should (= completed 1))))

(ert-deftest jabber-test-own-list-empty-items-and-removals ()
  "Authoritative empty items and missing nodes permit each explicit mutation."
  (dolist (operation '(ensure stale remove))
    (dolist (missing '(nil t))
      (jabber-test-own-list--with-fixture
        (jabber-test-own-list--mutate operation)
        (if missing
            (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
          (funcall success 'account
                   `(iq ((type . "result"))
                        (pubsub ((xmlns . ,jabber-pubsub-xmlns))
                                (items ((node . ,jabber-omemo-devicelist-node))))) nil))
        (should (equal published (if (eq operation 'ensure) '((222)) '(nil))))
        (should (eq (not (null deleted)) (not (eq operation 'ensure))))))))

(ert-deftest jabber-test-own-list-failure-preserves-cache-and-completion ()
  "Failed removal preserves a known cache and does not claim completion."
  (jabber-test-own-list--with-fixture
    (puthash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
             '(111 222) jabber-omemo--device-lists)
    (jabber-omemo--remove-device 'account 111 (lambda () (cl-incf completed)))
    (funcall failure 'account (jabber-test-own-list--error 'service-unavailable) nil)
    (should (= completed 0))
    (should-not published)
    (should-not deleted)
    (should (equal (gethash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
                           jabber-omemo--device-lists) '(111 222)))))

(ert-deftest jabber-test-own-list-success-completion-once ()
  "Successful removal completes once, ignoring repeated or opposite replies."
  (jabber-test-own-list--with-fixture
    (jabber-omemo--remove-device 'account 111 (lambda () (cl-incf completed)))
    (funcall success 'account (jabber-test-own-list--result '(111 222)) nil)
    (should (= completed 1))
    (should (equal published '((222))))
    (should (= (length deleted) 1))
    (funcall success 'account (jabber-test-own-list--result nil) nil)
    (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
    (should (= completed 1))
    (should (equal published '((222))))
    (should (= (length deleted) 1))))

(ert-deftest jabber-test-own-list-error-settles-before-late-success ()
  "A failure cannot be revived into a successful empty snapshot."
  (jabber-test-own-list--with-fixture
    (jabber-omemo--ensure-device-listed 'account)
    (funcall failure 'account (jabber-test-own-list--error 'service-unavailable) nil)
    (funcall success 'account (jabber-test-own-list--result nil) nil)
    (should-not published)
    (should-not deactivated)))

(ert-deftest jabber-test-own-list-cancelled-request ()
  "Cancellation retires retained success and error callbacks."
  (jabber-test-own-list--with-fixture
    (cl-letf (((symbol-function 'jabber-omemo--request-peer)
               (lambda (_jc _jid _node ok err cancel)
                 (setq success ok failure err)
                 (funcall cancel nil))))
      (jabber-omemo--ensure-device-listed 'account))
    (funcall success 'account (jabber-test-own-list--result nil) nil)
    (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
    (should-not published)
    (should-not deactivated)))

(ert-deftest jabber-test-own-list-synchronous-unwind ()
  "Error and quit before response admission retire retained callbacks."
  (dolist (condition '(error quit))
    (jabber-test-own-list--with-fixture
      (cl-letf (((symbol-function 'jabber-pubsub-request)
                 (lambda (_jc _jid _node ok err)
                   (setq success ok failure err)
                   (signal condition nil))))
        (let (caught)
          (condition-case err
              (jabber-omemo--ensure-device-listed 'account)
            ((error quit) (setq caught (car err))))
          (should (eq caught condition))))
      (funcall success 'account (jabber-test-own-list--result nil) nil)
      (funcall failure 'account (jabber-test-own-list--error 'item-not-found) nil)
      (should-not published)
      (should-not deactivated))))

(ert-deftest jabber-test-own-list-foreign-response ()
  "Foreign receiving connections and senders cannot supply own snapshots."
  (dolist (missing '(nil t))
    (dolist (foreign-jc '(nil t))
      (jabber-test-own-list--with-fixture
        (jabber-omemo--ensure-device-listed 'account)
        (let ((xml (copy-tree (if missing (jabber-test-own-list--error 'item-not-found)
                               (jabber-test-own-list--result nil)))))
          (unless foreign-jc (setcdr (assq 'from (cadr xml)) "other@example.org"))
          (funcall (if missing failure success) (if foreign-jc 'other 'account) xml nil))
        (should-not published)
        (should-not deactivated)))))

(ert-deftest jabber-test-own-list-malformed-missing-node-error ()
  "Wrong namespaces or conflicting conditions do not establish an absent node."
  (dolist (xml '((iq ((type . "error")) (error nil (item-not-found ((xmlns . "wrong")))))
                 (iq ((type . "error"))
                     (error nil (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))
                            (service-unavailable ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))
                 (iq ((type . "result"))
                     (error nil (item-not-found ((xmlns . "urn:ietf:params:xml:ns:xmpp-stanzas")))))))
    (jabber-test-own-list--with-fixture
      (jabber-omemo--ensure-device-listed 'account)
      (funcall failure 'account xml nil)
      (should-not published)
      (should-not deactivated))))

(ert-deftest jabber-test-own-list-stale-cleanup-session-ownership ()
  "An old bundle comparison cannot initiate a fresh-session list mutation."
  (let ((cleanup (symbol-function 'jabber-omemo--cleanup-stale-devices)))
    (dolist (retired '(nil t))
      (jabber-test-own-list--with-fixture
        (let (bundle-callback removals)
          (cl-letf (((symbol-function 'jabber-omemo--get-store) (lambda (_) 'store))
                    ((symbol-function 'jabber-omemo-get-bundle)
                     (lambda (_) '(:identity-key "fixture-key")))
                    ((symbol-function 'jabber-omemo--fetch-bundle)
                     (lambda (_jc _jid _id callback) (setq bundle-callback callback)))
                    ((symbol-function 'jabber-omemo--remove-stale-devices)
                     (lambda (_jc ids) (push ids removals))))
            (funcall cleanup 'account '(111))
            (setq state (copy-sequence state))
            (when retired (plist-put state :connection (list 'successor)))
            (funcall bundle-callback '(:identity-key "fixture-key"))
            (should (equal removals (unless retired '((111)))))))))))

(ert-deftest jabber-test-own-list-native-iq-failure-boundary ()
  "Real PubSub/IQ dispatch refuses failure for every own-list mutation caller."
  (let ((request (symbol-function 'jabber-pubsub-request)))
    (dolist (operation '(ensure stale remove))
      (jabber-test-own-list--with-fixture
        (let ((jabber-open-info-queries nil) wire)
          (cl-letf (((symbol-function 'jabber-pubsub-request) request)
                    ((symbol-function 'jabber-send-sexp)
                     (lambda (_jc xml) (setq wire xml))))
            (jabber-test-own-list--mutate operation)
            (should (equal (jabber-xml-get-attribute wire 'to) "me@example.org"))
            (should (equal (jabber-xml-get-attribute wire 'type) "get"))
            (let ((xml (copy-tree (jabber-test-own-list--error 'service-unavailable))))
              (push (cons 'id (jabber-xml-get-attribute wire 'id)) (cadr xml))
              (jabber-process-iq 'account xml))
            (should-not jabber-open-info-queries)
            (should-not published)
            (should-not deleted)
            (should-not cleaned)
            (should-not deactivated)))))))

(ert-deftest jabber-test-own-list-native-iq-first-install ()
  "Real PubSub/IQ dispatch admits a standard absent-node first installation."
  (let ((request (symbol-function 'jabber-pubsub-request)))
    (jabber-test-own-list--with-fixture
      (let ((jabber-open-info-queries nil) wire)
        (cl-letf (((symbol-function 'jabber-pubsub-request) request)
                  ((symbol-function 'jabber-send-sexp)
                   (lambda (_jc xml) (setq wire xml))))
          (jabber-omemo--ensure-device-listed 'account)
          (let ((xml (copy-tree (jabber-test-own-list--error 'item-not-found))))
            (push (cons 'id (jabber-xml-get-attribute wire 'id)) (cadr xml))
            (jabber-process-iq 'account xml))
          (should-not jabber-open-info-queries)
          (should (equal published '((222)))))))))

(defun jabber-test-own-list--parse-wire (contents)
  "Parse a successful own-list IQ containing PubSub CONTENTS."
  (with-temp-buffer
    (insert (concat "<iq type='result' from='me@example.org'>" contents "</iq>"))
    (car (xml-parse-region (point-min) (point-max)))))

(ert-deftest jabber-test-own-list-native-iq-malformed-content ()
  "Malformed content at every snapshot level has no mutation effects."
  (let* ((request (symbol-function 'jabber-pubsub-request))
         (valid (jabber-test-own-list--result '(111 333)))
         (paths '((pubsub) (pubsub items) (pubsub items item)
                  (pubsub items item list) (pubsub items item list device))))
    (dolist (operation '(ensure stale remove))
      ;; Text-only items/list reproduce the destructive authoritative-empty bug.
      ;; Mixed text and unknown children also fail, even with valid device IDs.
      (dolist (path (cons nil paths))
        (dolist (bad-child '("not-an-empty-snapshot" (unknown nil)))
          (dolist (text-only '(nil t))
            (jabber-test-own-list--with-fixture
              (let ((jabber-open-info-queries nil)
                    (xml (copy-tree valid)) wire)
                (let ((node xml))
                  (dolist (name path)
                    (setq node (car (jabber-xml-get-children node name))))
                  (setcdr (cdr node)
                          (cons bad-child (unless text-only (cddr node)))))
                (setq xml (with-temp-buffer
                            (insert (jabber-sexp2xml xml))
                            (car (xml-parse-region (point-min) (point-max)))))
                (puthash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
                         '(111 222) jabber-omemo--device-lists)
                (cl-letf (((symbol-function 'jabber-pubsub-request) request)
                          ((symbol-function 'jabber-send-sexp)
                           (lambda (_jc stanza) (setq wire stanza))))
                  (jabber-test-own-list--mutate operation)
                  (push (cons 'id (jabber-xml-get-attribute wire 'id)) (cadr xml))
                  (jabber-process-iq 'account xml))
                (should-not jabber-open-info-queries)
                (should-not published)
                (should-not deleted)
                (should-not cleaned)
                (should-not persisted)
                (should-not deactivated)
                (should (equal (gethash (jabber-omemo--device-list-key
                                        "me@example.org" "me@example.org")
                                       jabber-omemo--device-lists) '(111 222)))))))))))

(ert-deftest jabber-test-own-list-native-iq-malformed-namespace ()
  "Namespace overrides cannot authorize any own-list mutation."
  (let ((request (symbol-function 'jabber-pubsub-request)))
    (dolist (operation '(ensure stale remove))
      (dolist (path '((pubsub) (pubsub items) (pubsub items item)
                     (pubsub items item list) (pubsub items item list device)))
        (dolist (namespace '("urn:wrong" ""))
          (jabber-test-own-list--with-fixture
            (let ((jabber-open-info-queries nil)
                  (xml (jabber-test-own-list--result '(111 333))) wire)
              (let ((node xml))
                (dolist (name path)
                  (setq node (car (jabber-xml-get-children node name))))
                (setf (alist-get 'xmlns (cadr node)) namespace))
              (setq xml (with-temp-buffer
                          (insert (jabber-sexp2xml xml))
                          (car (xml-parse-region (point-min) (point-max)))))
              (puthash (jabber-omemo--device-list-key "me@example.org" "me@example.org")
                       '(111 222) jabber-omemo--device-lists)
              (cl-letf (((symbol-function 'jabber-pubsub-request) request)
                        ((symbol-function 'jabber-send-sexp)
                         (lambda (_jc stanza) (setq wire stanza))))
                (jabber-test-own-list--mutate operation)
                (push (cons 'id (jabber-xml-get-attribute wire 'id)) (cadr xml))
                (jabber-process-iq 'account xml))
              (should-not jabber-open-info-queries)
              (should-not published)
              (should-not deleted)
              (should-not cleaned)
              (should-not persisted)
              (should-not deactivated)
              (should (equal (gethash (jabber-omemo--device-list-key
                                      "me@example.org" "me@example.org")
                                     jabber-omemo--device-lists) '(111 222))))))))))

(ert-deftest jabber-test-own-list-native-iq-whitespace-and-inheritance ()
  "Valid whitespace, explicit and inherited namespaces preserve device IDs."
  (let ((request (symbol-function 'jabber-pubsub-request)))
    (dolist (operation '(ensure stale remove))
      (dolist (contents
               '("\n<pubsub xmlns='http://jabber.org/protocol/pubsub'>\n<items node='eu.siacs.conversations.axolotl.devicelist'>\n</items>\n</pubsub>\n"
                 "<pubsub xmlns='http://jabber.org/protocol/pubsub'><items node='eu.siacs.conversations.axolotl.devicelist'><item><list xmlns='eu.siacs.conversations.axolotl'>\n</list></item></items></pubsub>"
                 "\n<pubsub xmlns='http://jabber.org/protocol/pubsub'>\n<items node='eu.siacs.conversations.axolotl.devicelist'>\n<item>\n<list xmlns='eu.siacs.conversations.axolotl'>\n<device id='111'> \t\r\n</device>\n<device id='333'/>\n</list>\n</item>\n</items>\n</pubsub>\n"
                 "<pubsub xmlns='http://jabber.org/protocol/pubsub'><items xmlns='http://jabber.org/protocol/pubsub' node='eu.siacs.conversations.axolotl.devicelist'><item xmlns='http://jabber.org/protocol/pubsub'><list xmlns='eu.siacs.conversations.axolotl'><device xmlns='eu.siacs.conversations.axolotl' id='111'/><device xmlns='eu.siacs.conversations.axolotl' id='333'/></list></item></items></pubsub>"))
        (jabber-test-own-list--with-fixture
          (let ((jabber-open-info-queries nil)
                (xml (jabber-test-own-list--parse-wire contents)) wire
                (ids (when (string-match-p "<device " contents) '(111 333))))
            (cl-letf (((symbol-function 'jabber-pubsub-request) request)
                      ((symbol-function 'jabber-send-sexp)
                       (lambda (_jc stanza) (setq wire stanza))))
              (jabber-test-own-list--mutate operation)
              (push (cons 'id (jabber-xml-get-attribute wire 'id)) (cadr xml))
              (jabber-process-iq 'account xml))
            (should-not jabber-open-info-queries)
            (should (equal published (if (eq operation 'ensure)
                                        (list (cons 222 ids))
                                      (list (remq 111 ids)))))
            (should (eq (not (null deleted)) (not (eq operation 'ensure))))
            (should (equal (sort persisted #'<) ids))
            (should (equal deactivated (list ids)))
            (should (equal (gethash (jabber-omemo--device-list-key
                                    "me@example.org" "me@example.org")
                                   jabber-omemo--device-lists)
                           ids))))))))

(provide 'jabber-test-omemo-own-list)
;;; jabber-test-omemo-own-list.el ends here
