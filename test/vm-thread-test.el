;;; vm-thread-test.el --- Tests for vm-thread.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM thread functions in vm-thread.el

;;; Code:

(require 'vm-test-init)
(require 'vm-thread)

;;; Thread symbol accessor tests

(ert-deftest vm-thread-test-youngest-date-accessors ()
  "Test vm-th-youngest-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-01-01")
    (should (equal (vm-th-youngest-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-oldest-date-accessors ()
  "Test vm-th-oldest-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-oldest-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-oldest-subject-accessors ()
  "Test vm-th-oldest-subject-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-oldest-subject-of sym "Test Subject")
    (should (equal (vm-th-oldest-subject-of sym) "Test Subject"))))

(ert-deftest vm-thread-test-message-accessors ()
  "Test vm-th-message-of and setter."
  (let ((sym (make-symbol "test"))
        (msg 'test-message))
    (vm-th-set-message-of sym msg)
    (should (eq (vm-th-message-of sym) msg))))

(ert-deftest vm-thread-test-messages-accessors ()
  "Test vm-th-messages-of and setter."
  (let ((sym (make-symbol "test"))
        (msgs '(msg1 msg2 msg3)))
    (vm-th-set-messages-of sym msgs)
    (should (equal (vm-th-messages-of sym) msgs))))

(ert-deftest vm-thread-test-parent-accessors ()
  "Test vm-th-parent-of and setter."
  (let ((sym (make-symbol "test"))
        (parent (make-symbol "parent")))
    (vm-th-set-parent-of sym parent)
    (should (eq (vm-th-parent-of sym) parent))))

(ert-deftest vm-thread-test-children-accessors ()
  "Test vm-th-children-of and setter."
  (let ((sym (make-symbol "test"))
        (children '(child1 child2)))
    (vm-th-set-children-of sym children)
    (should (equal (vm-th-children-of sym) children))))

(ert-deftest vm-thread-test-date-accessors ()
  "Test vm-th-date-of and setter."
  (let ((sym (make-symbol "test")))
    (vm-th-set-date-of sym "2024-06-15")
    (should (equal (vm-th-date-of sym) "2024-06-15"))))

;;; vm-th-add-child / vm-th-delete-child tests

(ert-deftest vm-thread-test-add-child ()
  "Test adding a child to a thread."
  (let ((parent (make-symbol "parent"))
        (child (make-symbol "child")))
    (vm-th-set-children-of parent nil)
    (vm-th-add-child parent child)
    (should (memq child (vm-th-children-of parent)))))

(ert-deftest vm-thread-test-delete-child ()
  "Test deleting a child from a thread."
  (let ((parent (make-symbol "parent"))
        (child1 (make-symbol "child1"))
        (child2 (make-symbol "child2")))
    (vm-th-set-children-of parent (list child1 child2))
    (vm-th-delete-child parent child1)
    (should-not (memq child1 (vm-th-children-of parent)))
    (should (memq child2 (vm-th-children-of parent)))))

;;; Thread subject symbol accessor tests
;; Note: ts-* functions require the symbol to have a vector value

(ert-deftest vm-thread-test-ts-root-accessors ()
  "Test vm-ts-root-of and setter."
  (let ((sym (make-symbol "subj"))
        (root (make-symbol "root")))
    ;; Initialize with a 4-element vector
    (set sym (make-vector 4 nil))
    (vm-ts-set-root-of sym root)
    (should (eq (vm-ts-root-of sym) root))))

(ert-deftest vm-thread-test-ts-root-date-accessors ()
  "Test vm-ts-root-date-of and setter."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-root-date-of sym "2024-01-01")
    (should (equal (vm-ts-root-date-of sym) "2024-01-01"))))

(ert-deftest vm-thread-test-ts-members-accessors ()
  "Test vm-ts-members-of and setter."
  (let ((sym (make-symbol "subj"))
        (members '(m1 m2 m3)))
    (set sym (make-vector 4 nil))
    (vm-ts-set-members-of sym members)
    (should (equal (vm-ts-members-of sym) members))))

(ert-deftest vm-thread-test-ts-messages-accessors ()
  "Test vm-ts-messages-of and setter."
  (let ((sym (make-symbol "subj"))
        (messages '(msg1 msg2)))
    (set sym (make-vector 4 nil))
    (vm-ts-set-messages-of sym messages)
    (should (equal (vm-ts-messages-of sym) messages))))

;;; vm-th-thread-date-of tests
;; Note: uses symbol properties 'youngest-date and 'oldest-date

(ert-deftest vm-thread-test-thread-date-youngest ()
  "Test vm-th-thread-date-of with youngest-date criterion."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-12-31")
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-thread-date-of sym 'youngest-date) "2024-12-31"))))

(ert-deftest vm-thread-test-thread-date-oldest ()
  "Test vm-th-thread-date-of with oldest-date criterion."
  (let ((sym (make-symbol "test")))
    (vm-th-set-youngest-date-of sym "2024-12-31")
    (vm-th-set-oldest-date-of sym "2024-01-01")
    (should (equal (vm-th-thread-date-of sym 'oldest-date) "2024-01-01"))))

;;; vm-th-child-messages-of tests

(ert-deftest vm-thread-test-child-messages-of-empty ()
  "Test vm-th-child-messages-of with no children."
  (let ((sym (make-symbol "test")))
    (vm-th-set-children-of sym nil)
    (should (null (vm-th-child-messages-of sym)))))

;;; vm-th-safe-parent-p tests

(ert-deftest vm-thread-test-safe-parent-p-same-symbol ()
  "Test vm-th-safe-parent-p rejects same symbol as parent."
  (let ((sym (make-symbol "test")))
    (should-not (vm-th-safe-parent-p sym sym))))

(ert-deftest vm-thread-test-safe-parent-p-valid ()
  "Test vm-th-safe-parent-p accepts valid parent."
  (let ((child (make-symbol "child"))
        (parent (make-symbol "parent")))
    (vm-th-set-parent-of parent nil)  ; parent has no parent
    (should (vm-th-safe-parent-p child parent))))

;;; vm-th-root tests
;; Note: vm-th-root traverses parent pointers and requires
;; symbols to have proper values set. These are complex tests
;; that need full threading infrastructure.

;;; vm-ts-add-members tests

(ert-deftest vm-thread-test-ts-add-members ()
  "Test vm-ts-add-members adds to existing members."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-members-of sym '(m1))
    (vm-ts-add-members sym '(m2 m3))
    (let ((members (vm-ts-members-of sym)))
      (should (memq 'm1 members))
      (should (memq 'm2 members))
      (should (memq 'm3 members)))))

;;; vm-ts-add-messages tests

(ert-deftest vm-thread-test-ts-add-messages ()
  "Test vm-ts-add-messages adds to existing messages."
  (let ((sym (make-symbol "subj")))
    (set sym (make-vector 4 nil))
    (vm-ts-set-messages-of sym '(msg1))
    (vm-ts-add-messages sym '(msg2))
    (let ((messages (vm-ts-messages-of sym)))
      (should (memq 'msg1 messages))
      (should (memq 'msg2 messages)))))

;;; vm-ts-merge tests

(ert-deftest vm-thread-test-ts-merge ()
  "Test vm-ts-merge merges two subject symbols."
  (let ((sym1 (make-symbol "subj1"))
        (sym2 (make-symbol "subj2")))
    ;; Initialize vectors
    (set sym1 (make-vector 4 nil))
    (set sym2 (make-vector 4 nil))
    ;; Set up data
    (vm-ts-set-members-of sym1 '(m1))
    (vm-ts-set-messages-of sym1 '(msg1))
    (vm-ts-set-members-of sym2 '(m2))
    (vm-ts-set-messages-of sym2 '(msg2))
    ;; Merge sym2 into sym1
    (vm-ts-merge sym1 sym2)
    ;; sym1 should have all members and messages
    (let ((members (vm-ts-members-of sym1))
          (messages (vm-ts-messages-of sym1)))
      (should (memq 'm1 members))
      (should (memq 'm2 members))
      (should (memq 'msg1 messages))
      (should (memq 'msg2 messages)))))

;;; vm-ts-subject-symbol tests
;; Note: vm-ts-subject-symbol uses 'oldest-subject property,
;; not 'subject. The function checks for proper thread setup.

;;; Integration tests with folders

(defconst vm-thread-test-folder
  "From sender1@example.com Mon Jan  1 00:00:00 2024
From: sender1@example.com
To: recipient@example.com
Subject: Thread Test
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <thread1@example.com>

First message in thread.

From sender2@example.com Tue Jan  2 00:00:00 2024
From: sender2@example.com
To: recipient@example.com
Subject: Re: Thread Test
Date: Tue, 02 Jan 2024 11:00:00 +0000
Message-ID: <thread2@example.com>
In-Reply-To: <thread1@example.com>
References: <thread1@example.com>

Reply to first message.

"
  "Test folder with threading references.")

(ert-deftest vm-thread-test-folder-parse-references ()
  "Test that References header is parsed correctly."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg2 (nth 1 vm-message-list)))
      (let ((refs (vm-get-header-contents msg2 "References:")))
        (should (stringp refs))
        (should (string-match "thread1@example.com" refs))))))

(ert-deftest vm-thread-test-folder-parse-in-reply-to ()
  "Test that In-Reply-To header is parsed correctly."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg2 (nth 1 vm-message-list)))
      (let ((irt (vm-get-header-contents msg2 "In-Reply-To:")))
        (should (stringp irt))
        (should (string-match "thread1@example.com" irt))))))

;;; vm-thread-indentation tests

(ert-deftest vm-thread-test-indentation-unset ()
  "Test vm-thread-indentation returns empty when not threaded."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; Before threading, indentation should be empty or 0
      (let ((indent (vm-thread-indentation msg)))
        (should (or (null indent) (= indent 0)))))))

;;; vm-thread-list tests

(ert-deftest vm-thread-test-thread-list-returns-list ()
  "Test vm-thread-list returns a list."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; vm-thread-list auto-generates the list if not set
      (should (listp (vm-thread-list msg))))))

;;; vm-thread-root-p tests

(ert-deftest vm-thread-test-root-p-unthreaded ()
  "Test vm-thread-root-p for unthreaded message."
  (vm-test-with-folder vm-thread-test-folder
    (let ((msg (car vm-message-list)))
      ;; For unthreaded messages, should return nil
      (should (null (vm-thread-root-p msg))))))

(provide 'vm-thread-test)

;;; vm-thread-test.el ends here
