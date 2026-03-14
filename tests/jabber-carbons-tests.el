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

;;; Test helpers

(defun jabber-carbons-test--make-carbon (type from inner-from inner-to)
  "Build a carbon-wrapped message stanza.
TYPE is `sent' or `received'.  FROM is the outer stanza's from.
INNER-FROM and INNER-TO are attributes on the inner message."
  `(message ((from . ,from) (type . "chat"))
            (,type ((xmlns . "urn:xmpp:carbons:2"))
                   (forwarded ((xmlns . "urn:xmpp:forward:0"))
                              (message ((from . ,inner-from)
                                        (to . ,inner-to)
                                        (type . "chat"))
                                       (body nil "Hello"))))))

(defun jabber-carbons-test--make-plain-message (from to)
  "Build a plain (non-carbon) message stanza."
  `(message ((from . ,from) (to . ,to) (type . "chat"))
            (body nil "Hello")))

(defvar jabber-carbons-test--fake-jc-data
  '(:username "me" :server "example.com" :resource "emacs")
  "Fake connection state data for tests.")

(defun jabber-carbons-test--make-fake-jc ()
  "Return a mock JC object that `jabber-connection-bare-jid' can use."
  (start-fsm 'jabber-carbons-test-fsm))

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

(provide 'jabber-carbons-tests)

;;; jabber-carbons-tests.el ends here
