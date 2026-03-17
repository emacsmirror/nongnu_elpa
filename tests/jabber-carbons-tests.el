;;; jabber-carbons-tests.el --- Tests for XEP-0280 carbon handling  -*- lexical-binding: t; -*-

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

;;; Test helpers

(defun jabber-carbons-test--make-carbon (type from inner-from inner-to
                                              &optional id body)
  "Build a carbon-wrapped message stanza.
TYPE is `sent' or `received'.  FROM is the outer stanza's from.
INNER-FROM and INNER-TO are attributes on the inner message.
Optional ID is the inner message's stanza id.
Optional BODY overrides the default \"Hello\"."
  (let ((inner-attrs `((from . ,inner-from)
                       (to . ,inner-to)
                       (type . "chat"))))
    (when id (push `(id . ,id) inner-attrs))
    `(message ((from . ,from) (type . "chat"))
              (,type ((xmlns . "urn:xmpp:carbons:2"))
                     (forwarded ((xmlns . "urn:xmpp:forward:0"))
                                (message ,inner-attrs
                                         (body nil ,(or body "Hello"))))))))

(defun jabber-carbons-test--make-plain-message (from to)
  "Build a plain (non-carbon) message stanza."
  `(message ((from . ,from) (to . ,to) (type . "chat"))
            (body nil "Hello")))

;;; Group 1: jabber-chat--extract-carbon

(ert-deftest jabber-chat-test-extract-carbon-sent ()
  "Extract-carbon returns (sent . msg) for a sent carbon."
  (let* ((stanza (jabber-carbons-test--make-carbon
                  'sent "me@example.com" "me@example.com/phone"
                  "friend@example.com"))
         (result (jabber-chat--extract-carbon stanza)))
    (should result)
    (should (eq (car result) 'sent))
    (should (equal (jabber-xml-get-attribute (cdr result) 'to)
                   "friend@example.com"))))

(ert-deftest jabber-chat-test-extract-carbon-received ()
  "Extract-carbon returns (received . msg) for a received carbon."
  (let* ((stanza (jabber-carbons-test--make-carbon
                  'received "me@example.com" "friend@example.com"
                  "me@example.com/phone"))
         (result (jabber-chat--extract-carbon stanza)))
    (should result)
    (should (eq (car result) 'received))
    (should (equal (jabber-xml-get-attribute (cdr result) 'from)
                   "friend@example.com"))))

(ert-deftest jabber-chat-test-extract-carbon-plain ()
  "Extract-carbon returns nil for a plain message."
  (let* ((stanza (jabber-carbons-test--make-plain-message
                  "friend@example.com" "me@example.com"))
         (result (jabber-chat--extract-carbon stanza)))
    (should-not result)))

;;; Group 2: jabber-chat--unwrap-carbon

(ert-deftest jabber-chat-test-unwrap-carbon-rejects-forged ()
  "Unwrap-carbon drops carbon framing when outer from doesn't match our JID."
  (let* ((stanza (jabber-carbons-test--make-carbon
                  'sent "evil@attacker.com" "evil@attacker.com/phone"
                  "victim@example.com")))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com")))
      (let ((result (jabber-chat--unwrap-carbon 'fake-jc stanza)))
        ;; Should return original stanza unchanged (carbon rejected)
        (should (eq (car result) stanza))
        (should-not (cdr result))))))

(ert-deftest jabber-chat-test-unwrap-carbon-valid-sent ()
  "Unwrap-carbon returns inner message and buffer for valid sent carbon."
  (let* ((stanza (jabber-carbons-test--make-carbon
                  'sent "me@example.com" "me@example.com/phone"
                  "friend@example.com"))
         (test-buffer (generate-new-buffer " *test-carbon*")))
    (cl-letf (((symbol-function 'jabber-connection-bare-jid)
               (lambda (_jc) "me@example.com"))
              ((symbol-function 'jabber-chat-create-buffer)
               (lambda (_jc _to) test-buffer)))
      (unwind-protect
          (let ((result (jabber-chat--unwrap-carbon 'fake-jc stanza)))
            (should (equal (jabber-xml-get-attribute (car result) 'to)
                           "friend@example.com"))
            (should (eq (cdr result) test-buffer)))
        (kill-buffer test-buffer)))))

(ert-deftest jabber-chat-test-unwrap-carbon-valid-received ()
  "Unwrap-carbon returns inner message with no buffer for valid received carbon."
  (let* ((stanza (jabber-carbons-test--make-carbon
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

(defmacro jabber-carbons-test-with-db (&rest body)
  "Run BODY with a fresh temp SQLite database."
  (declare (indent 0) (debug t))
  `(let* ((jabber-carbons-test--dir (make-temp-file "jabber-carbons-test" t))
          (jabber-db-path (expand-file-name "test.sqlite"
                                            jabber-carbons-test--dir))
          (jabber-db--connection nil)
          (jabber-backlog-days 3.0)
          (jabber-backlog-number 10))
     (unwind-protect
         (progn
           (jabber-db-ensure-open)
           ,@body)
       (jabber-db-close)
       (when (file-directory-p jabber-carbons-test--dir)
         (delete-directory jabber-carbons-test--dir t)))))

(ert-deftest jabber-chat-test-store-carbon-sent ()
  "Sent carbon is stored with direction=out and peer=recipient."
  (jabber-carbons-test-with-db
    (let ((xml-data (jabber-carbons-test--make-carbon
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
  (jabber-carbons-test-with-db
    (let ((xml-data (jabber-carbons-test--make-carbon
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
  (jabber-carbons-test-with-db
    (let ((xml-data (jabber-carbons-test--make-carbon
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

(provide 'jabber-carbons-tests)

;;; jabber-carbons-tests.el ends here
