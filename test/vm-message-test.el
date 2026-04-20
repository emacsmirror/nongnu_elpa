;;; vm-message-test.el --- Tests for vm-message.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM message struct accessors in vm-message.el

;;; Code:

(require 'vm-test-init)
(require 'vm-message)

;;; Constants tests

(ert-deftest vm-message-test-location-data-vector-length ()
  "Test location data vector length constant."
  (should (= vm-location-data-vector-length 6)))

(ert-deftest vm-message-test-softdata-vector-length ()
  "Test softdata vector length constant."
  (should (= vm-softdata-vector-length 23)))

(ert-deftest vm-message-test-attributes-vector-length ()
  "Test attributes vector length constant."
  (should (= vm-attributes-vector-length 20)))

(ert-deftest vm-message-test-cached-data-vector-length ()
  "Test cached data vector length constant."
  (should (= vm-cached-data-vector-length 50)))

(ert-deftest vm-message-test-mirror-data-vector-length ()
  "Test mirror data vector length constant."
  (should (= vm-mirror-data-vector-length 6)))

;;; vm-make-message tests

(ert-deftest vm-message-test-make-message-creates-vector ()
  "Test that vm-make-message creates a valid message struct."
  (let ((m (vm-make-message)))
    (should (vectorp m))
    (should (= (length m) 5))))

(ert-deftest vm-message-test-make-message-subvectors ()
  "Test that vm-make-message creates proper sub-vectors."
  (let ((m (vm-make-message)))
    ;; Check location data
    (should (vectorp (vm-location-data-of m)))
    (should (= (length (vm-location-data-of m)) vm-location-data-vector-length))
    ;; Check softdata
    (should (vectorp (vm-softdata-of m)))
    (should (= (length (vm-softdata-of m)) vm-softdata-vector-length))
    ;; Check mirror data
    (should (vectorp (vm-mirror-data-of m)))
    (should (= (length (vm-mirror-data-of m)) vm-mirror-data-vector-length))))

;;; Location data accessor tests

(ert-deftest vm-message-test-set-start-of ()
  "Test setting start marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-min-marker)))
        (vm-set-start-of m marker)
        (should (eq (vm-start-of m) marker))))))

(ert-deftest vm-message-test-set-headers-of ()
  "Test setting headers marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-marker)))
        (vm-set-headers-of m marker)
        (should (eq (vm-headers-of m) marker))))))

(ert-deftest vm-message-test-set-text-of ()
  "Test setting text marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-max-marker)))
        (vm-set-text-of m marker)
        (should (eq (vm-text-of m) marker))))))

(ert-deftest vm-message-test-set-end-of ()
  "Test setting end marker."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (insert "test")
      (let ((marker (point-max-marker)))
        (vm-set-end-of m marker)
        (should (eq (vm-end-of m) marker))))))

;;; Softdata accessor tests

(ert-deftest vm-message-test-set-number-of ()
  "Test setting message number."
  (let ((m (vm-make-message)))
    (vm-set-number-of m "42")
    (should (string= (vm-number-of m) "42"))))

(ert-deftest vm-message-test-set-padded-number-of ()
  "Test setting padded message number."
  (let ((m (vm-make-message)))
    (vm-set-padded-number-of m "  42")
    (should (string= (vm-padded-number-of m) "  42"))))

(ert-deftest vm-message-test-set-mark-of ()
  "Test setting message mark."
  (let ((m (vm-make-message)))
    (vm-set-mark-of m t)
    (should (eq (vm-mark-of m) t))
    (vm-set-mark-of m nil)
    (should (null (vm-mark-of m)))))

(ert-deftest vm-message-test-set-buffer-of ()
  "Test setting message buffer."
  (let ((m (vm-make-message))
        (buf (current-buffer)))
    (vm-set-buffer-of m buf)
    (should (eq (vm-buffer-of m) buf))))

(ert-deftest vm-message-test-set-message-type-of ()
  "Test setting message type."
  (let ((m (vm-make-message)))
    (vm-set-message-type-of m 'From_)
    (should (eq (vm-message-type-of m) 'From_))))

(ert-deftest vm-message-test-set-thread-indentation ()
  "Test setting thread indentation."
  (let ((m (vm-make-message)))
    (vm-set-thread-indentation-of m 3)
    (should (= (vm-thread-indentation-of m) 3))))

(ert-deftest vm-message-test-set-thread-list ()
  "Test setting thread list."
  (let ((m (vm-make-message)))
    (vm-set-thread-list-of m '(a b c))
    (should (equal (vm-thread-list-of m) '(a b c)))))

;;; Mirror data accessor tests

(ert-deftest vm-message-test-set-edit-buffer ()
  "Test setting edit buffer."
  (let ((m (vm-make-message)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (vm-set-edit-buffer-of m buf)
        (should (eq (vm-edit-buffer-of m) buf))))))

(ert-deftest vm-message-test-set-virtual-messages ()
  "Test setting virtual messages list."
  (let ((m (vm-make-message)))
    (vm-set-virtual-messages-of m '(vm1 vm2))
    (should (equal (vm-virtual-messages-of m) '(vm1 vm2)))))

(ert-deftest vm-message-test-set-stuff-flag ()
  "Test setting stuff flag."
  (let ((m (vm-make-message)))
    (vm-set-stuff-flag-of m t)
    (should (eq (vm-stuff-flag-of m) t))))

;;; Real/mirrored message accessor tests

(ert-deftest vm-message-test-set-real-message-sym ()
  "Test setting real message symbol."
  (let ((m (vm-make-message))
        (sym (make-symbol "test-message")))
    (vm-set-real-message-sym-of m sym)
    (should (eq (vm-real-message-sym-of m) sym))))

(ert-deftest vm-message-test-real-message-of ()
  "Test vm-real-message-of returns the message from symbol."
  (let ((m (vm-make-message)))
    ;; Set up real message symbol with message as its value
    (let ((sym (make-symbol "real-msg")))
      (set sym m)
      (vm-set-real-message-sym-of m sym)
      (should (eq (vm-real-message-of m) m)))))

;;; Attribute accessor tests

(ert-deftest vm-message-test-attributes-vector ()
  "Test that attributes vector can be set and retrieved."
  (let ((m (vm-make-message))
        (attrs (make-vector vm-attributes-vector-length nil)))
    (vm-set-attributes-of m attrs)
    (should (eq (vm-attributes-of m) attrs))))

;;; Cached data accessor tests

(ert-deftest vm-message-test-cached-data-vector ()
  "Test that cached data vector can be set and retrieved."
  (let ((m (vm-make-message))
        (cache (make-vector vm-cached-data-vector-length nil)))
    (vm-set-cached-data-of m cache)
    (should (eq (vm-cached-data-of m) cache))))

;;; Field constants tests

(ert-deftest vm-message-test-message-fields ()
  "Test that message fields constant is valid."
  (should (vectorp vm-message-fields))
  (should (= (length vm-message-fields) 5)))

(ert-deftest vm-message-test-location-data-fields ()
  "Test that location data fields constant is valid."
  (should (vectorp vm-location-data-fields))
  (should (= (length vm-location-data-fields) vm-location-data-vector-length)))

(ert-deftest vm-message-test-softdata-fields ()
  "Test that softdata fields constant is valid."
  (should (vectorp vm-softdata-fields))
  (should (= (length vm-softdata-fields) vm-softdata-vector-length)))

(ert-deftest vm-message-test-mirror-data-fields ()
  "Test that mirror data fields constant is valid."
  (should (vectorp vm-mirror-data-fields))
  (should (= (length vm-mirror-data-fields) vm-mirror-data-vector-length)))

(provide 'vm-message-test)

;;; vm-message-test.el ends here