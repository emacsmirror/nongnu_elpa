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

(provide 'vm-thread-test)

;;; vm-thread-test.el ends here
