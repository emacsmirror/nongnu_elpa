;;; jabber-test-carbons.el --- Tests for jabber-carbons  -*- lexical-binding: t; -*-

;;; Commentary:

;; XEP-0280 Message Carbons processing.

;;; Code:

;; Copyright (C) 2026 - Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'ert)
(require 'jabber-chat)
(require 'jabber-db)
(require 'jabber-message-correct)

;;; Test helpers

(defun jabber-test-carbons--make-carbon (type from inner-from inner-to
                                              &optional id body extra-elements)
  "Build a carbon-wrapped message stanza.
TYPE is `sent' or `received'.  FROM is the outer stanza's from.
INNER-FROM and INNER-TO are attributes on the inner message.
Optional ID is the inner message's stanza id.
Optional BODY overrides the default \"Hello\".
EXTRA-ELEMENTS are appended to the inner message."
  (let ((inner-attrs `((from . ,inner-from)
                       (to . ,inner-to)
                       (type . "chat"))))
    (when id (push `(id . ,id) inner-attrs))
    `(message ((from . ,from) (type . "chat"))
              (,type ((xmlns . "urn:xmpp:carbons:2"))
                     (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                (message ,inner-attrs
                                         (body nil ,(or body "Hello"))
                                         ,@extra-elements))))))

(defun jabber-test-carbons--make-carbon-with-namespaces
    (type wrapper-xmlns forwarded-xmlns)
  "Build a carbon-like stanza with explicit namespace values.
TYPE is `sent' or `received'.  WRAPPER-XMLNS is the namespace for
the carbon wrapper.  FORWARDED-XMLNS is the namespace for
<forwarded/>."
  `(message ((from . "me@example.com") (type . "chat"))
            (,type ((xmlns . ,wrapper-xmlns))
                   (forwarded ((xmlns . ,forwarded-xmlns))
                              (message ((from . "me@example.com/phone")
                                        (to . "friend@example.com")
                                        (type . "chat"))
                                       (body nil "Hello"))))))

(defun jabber-test-carbons--make-plain-message (from to)
  "Build a plain (non-carbon) message stanza."
  `(message ((from . ,from) (to . ,to) (type . "chat"))
            (body nil "Hello")))

;;; Group 1: jabber-chat--extract-carbon

(ert-deftest jabber-chat-test-extract-carbon-sent ()
  "Extract-carbon returns (sent . msg) for a sent carbon."
  (let* ((stanza (jabber-test-carbons--make-carbon
                  'sent "me@example.com" "me@example.com/phone"
                  "friend@example.com"))
         (result (jabber-chat--extract-carbon stanza)))
    (should result)
    (should (eq (car result) 'sent))
    (should (equal (jabber-xml-get-attribute (cdr result) 'to)
                   "friend@example.com"))))

(ert-deftest jabber-chat-test-extract-carbon-received ()
  "Extract-carbon returns (received . msg) for a received carbon."
  (let* ((stanza (jabber-test-carbons--make-carbon
                  'received "me@example.com" "friend@example.com"
                  "me@example.com/phone"))
         (result (jabber-chat--extract-carbon stanza)))
    (should result)
    (should (eq (car result) 'received))
    (should (equal (jabber-xml-get-attribute (cdr result) 'from)
                   "friend@example.com"))))

(ert-deftest jabber-chat-test-extract-carbon-plain ()
  "Extract-carbon returns nil for a plain message."
  (let* ((stanza (jabber-test-carbons--make-plain-message
                  "friend@example.com" "me@example.com"))
         (result (jabber-chat--extract-carbon stanza)))
    (should-not result)))

(ert-deftest jabber-chat-test-extract-carbon-rejects-wrapper-namespace ()
  "Extract-carbon rejects sent/received elements outside carbons."
  (let ((stanza (jabber-test-carbons--make-carbon-with-namespaces
                 'sent "urn:example:not-carbons" "urn:xmpp:forward:0")))
    (should-not (jabber-chat--extract-carbon stanza))))

(ert-deftest jabber-chat-test-extract-carbon-rejects-forwarded-namespace ()
  "Extract-carbon rejects forwarded elements outside XEP-0297."
  (let ((stanza (jabber-test-carbons--make-carbon-with-namespaces
                 'sent "urn:xmpp:carbons:2" "urn:example:not-forward")))
    (should-not (jabber-chat--extract-carbon stanza))))

;;; Group 2: jabber-chat--unwrap-carbon

(ert-deftest jabber-chat-test-unwrap-carbon-rejects-forged ()
  "Unwrap-carbon drops carbon framing when outer from doesn't match our JID."
  (let* ((stanza (jabber-test-carbons--make-carbon
                  'sent "evil@attacker.com" "evil@attacker.com/phone"
                  "victim@example.com")))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((result (jabber-chat--unwrap-carbon 'fake-jc stanza)))
        ;; Should return original stanza unchanged (carbon rejected)
        (should (eq (car result) stanza))
        (should-not (cdr result))))))

(ert-deftest jabber-chat-test-unwrap-carbon-valid-sent ()
  "Unwrap-carbon finds an existing buffer for a valid sent carbon."
  (let* ((stanza (jabber-test-carbons--make-carbon
                  'sent "me@example.com" "me@example.com/phone"
                  "friend@example.com"))
         (test-buffer (generate-new-buffer " *test-carbon*")))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-chat--find-buffer)
               (lambda (to)
                 (should (equal to "friend@example.com"))
                 test-buffer))
              ((symbol-function 'jabber-chat-create-buffer)
               (lambda (&rest _)
                 (ert-fail "Carbon unwrapping created a chat buffer"))))
      (unwind-protect
          (let ((result (jabber-chat--unwrap-carbon 'fake-jc stanza)))
            (should (equal (jabber-xml-get-attribute (car result) 'to)
                           "friend@example.com"))
            (should (eq (cdr result) test-buffer)))
        (kill-buffer test-buffer)))))

(ert-deftest jabber-chat-test-bodyless-sent-carbon-does-not-create-buffer ()
  "A bodyless sent carbon does not create a chat buffer."
  (let ((stanza
         '(message ((from . "me@example.com") (type . "chat"))
                   (sent ((xmlns . "urn:xmpp:carbons:2"))
                         (forwarded
                          ((xmlns . "urn:xmpp:forward:0"))
                          (message ((from . "me@example.com/phone")
                                    (to . "friend@example.com")
                                    (type . "chat"))
                                   (active
                                    ((xmlns . "http://jabber.org/protocol/chatstates"))))))))
        created)
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-chat-create-buffer)
               (lambda (&rest _) (setq created t)))
              ((symbol-function 'jabber-chat--decrypt-if-needed)
               (lambda (_jc inner) inner))
              ((symbol-function 'jabber-chat--store-carbon) #'ignore))
      (let ((jabber-chat-printers nil))
        (jabber-process-chat 'connection stanza))
      (should-not created))))

(ert-deftest jabber-chat-test-unwrap-carbon-valid-received ()
  "Unwrap-carbon returns inner message with no buffer for valid received carbon."
  (let* ((stanza (jabber-test-carbons--make-carbon
                  'received "me@example.com" "friend@example.com"
                  "me@example.com/phone")))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((result (jabber-chat--unwrap-carbon 'fake-jc stanza)))
        (should (equal (jabber-xml-get-attribute (car result) 'from)
                       "friend@example.com"))
        (should-not (cdr result))))))

;;; Group 3: jabber-chat--store-carbon

(defvar jabber-db-path)
(defvar jabber-db--connection)
(defvar jabber-backlog-days)
(defvar jabber-backlog-number)
(declare-function jabber-db-ensure-open "jabber-db" ())
(declare-function jabber-db-close "jabber-db" ())

(defmacro jabber-test-carbons-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database."
  (declare (indent 0) (debug t))
  `(let* ((jabber-test-carbons--dir (make-temp-file "jabber-carbons-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-test-carbons--dir))
          (jabber-db--connection nil)
          (jabber-backlog-days 3.0)
          (jabber-backlog-number 10))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-test-carbons--dir)
         (delete-directory jabber-test-carbons--dir t)))))

(ert-deftest jabber-chat-test-store-carbon-sent ()
  "Sent carbon is stored with direction=out and peer=recipient."
  (jabber-test-carbons-with-db
    (let ((xml-data (jabber-test-carbons--make-carbon
                     'sent "me@example.com" "me@example.com/phone"
                     "friend@example.com" "msg-001" "Hi from phone")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-chat-create-buffer)
                 (lambda (_jc _to) (generate-new-buffer " *test*"))))
        (let* ((unwrapped (jabber-chat--unwrap-carbon 'fake-jc xml-data))
               (inner (car unwrapped)))
          (jabber-chat--store-carbon 'fake-jc inner)
          (let ((row (car (sqlite-select
                           jabber-db--connection
                           "SELECT peer, direction, body FROM message"))))
            (should row)
            (should (equal (nth 0 row) "friend@example.com"))
            (should (equal (nth 1 row) "out"))
            (should (equal (nth 2 row) "Hi from phone"))))))))

(ert-deftest jabber-chat-test-store-carbon-received ()
  "Received carbon is stored with direction=in and peer=sender."
  (jabber-test-carbons-with-db
    (let ((xml-data (jabber-test-carbons--make-carbon
                     'received "me@example.com" "friend@example.com/laptop"
                     "me@example.com/emacs" "msg-002" "Hi from laptop")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com")))
        (let* ((unwrapped (jabber-chat--unwrap-carbon 'fake-jc xml-data))
               (inner (car unwrapped)))
          (jabber-chat--store-carbon 'fake-jc inner)
          (let ((row (car (sqlite-select
                           jabber-db--connection
                           "SELECT peer, direction, body FROM message"))))
            (should row)
            (should (equal (nth 0 row) "friend@example.com"))
            (should (equal (nth 1 row) "in"))
            (should (equal (nth 2 row) "Hi from laptop"))))))))

(ert-deftest jabber-chat-test-store-carbon-dedup ()
  "Duplicate carbon with same stanza-id is not stored twice."
  (jabber-test-carbons-with-db
    (let ((xml-data (jabber-test-carbons--make-carbon
                     'sent "me@example.com" "me@example.com/phone"
                     "friend@example.com" "msg-dup" "Hello")))
      (cl-letf (((symbol-function 'jabber-connection-bare-jid)
                 (lambda (_jc) "me@example.com"))
                ((symbol-function 'jabber-chat-create-buffer)
                 (lambda (_jc _to) (generate-new-buffer " *test*"))))
        (let* ((unwrapped (jabber-chat--unwrap-carbon 'fake-jc xml-data))
               (inner (car unwrapped)))
          (jabber-chat--store-carbon 'fake-jc inner)
          (jabber-chat--store-carbon 'fake-jc inner)
          (let ((count (caar (sqlite-select
                              jabber-db--connection
                              "SELECT COUNT(*) FROM message"))))
            (should (= 1 count))))))))

(ert-deftest jabber-chat-test-sent-carbon-thread-root-displays-before-storage ()
  "A first-seen sent-carbon thread root remains visible and stored."
  (jabber-test-carbons-with-db
    (let ((buffer (generate-new-buffer " *carbon-thread-root*"))
          (display-count 0)
          displayed-message
          displayed-buffer)
      (unwind-protect
          (let ((stanza
                 (jabber-test-carbons--make-carbon
                  'sent "me@example.com" "me@example.com/phone"
                  "friend@example.com" "root-1" "Thread root"
                  '((thread () "thread-1")))))
            (cl-letf
                (((symbol-function 'jabber-connection-bare-jid)
                  (lambda (_jc) "me@example.com"))
                 ((symbol-function 'jabber-chat-create-buffer)
                  (lambda (_jc jid)
                    (should (equal jid "friend@example.com"))
                    buffer))
                 ((symbol-function 'jabber-chat--decrypt-if-needed)
                  (lambda (_jc inner) inner))
                 ((symbol-function 'jabber-chat--display-message)
                  (lambda (_jc _xml target _local _from msg)
                    (setq display-count (1+ display-count)
                          displayed-buffer target
                          displayed-message msg))))
              (let ((jabber-chat-printers (list (lambda (&rest _) t))))
                (jabber-process-chat 'connection stanza)))
            (should (= display-count 1))
            (should (eq displayed-buffer buffer))
            (should (equal "thread-1"
                           (plist-get displayed-message :thread-id)))
            (should
             (equal '("thread-1")
                    (car
                     (sqlite-select
                      jabber-db--connection
                      "SELECT thread_id FROM message"))))
            (should
             (equal "thread-1"
                    (plist-get
                     (jabber-db-message-thread-summary
                      "me@example.com" "friend@example.com" "chat"
                      "thread-1")
                     :thread-id))))
        (kill-buffer buffer)))))

(ert-deftest jabber-chat-test-sent-correction-uses-recipient-buffer ()
  "A sent-carbon correction redraws the recipient chat buffer."
  (let ((recipient-buffer (generate-new-buffer " *carbon-recipient*"))
        applied-buffer)
    (unwind-protect
        (let ((inner `(message ((from . "me@example.com/phone")
                                (to . "friend@example.com")
                                (type . "chat")
                                (id . "correction-1"))
                               (body () "corrected")
                               (replace ((xmlns . ,jabber-message-correct-xmlns)
                                         (id . "original-1"))))))
          (cl-letf (((symbol-function 'jabber-muc-message-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'jabber-chat--unwrap-carbon)
                     (lambda (&rest _) (cons inner recipient-buffer)))
                    ((symbol-function 'jabber-chat--decrypt-if-needed)
                     (lambda (_jc stanza) stanza))
                    ((symbol-function 'jabber-chat--store-carbon) #'ignore)
                    ((symbol-function 'jabber-connection-bare-jid)
                     (lambda (_jc) "me@example.com"))
                    ((symbol-function 'jabber-message-correct--apply)
                     (lambda (&rest args)
                       (setq applied-buffer
                             (car (funcall (nth 4 args)
                                           '(:row-id 7)))))))
            (jabber-process-chat
             'fake-jc
             '(message ((from . "me@example.com/resource"))))
            (should (eq applied-buffer recipient-buffer))))
      (kill-buffer recipient-buffer))))

(provide 'jabber-test-carbons)

;;; jabber-test-carbons.el ends here
