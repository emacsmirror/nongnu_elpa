;;; vm-accessors-test.el --- Tests for VM message accessor functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM message accessor functions defined in vm-message.el
;; and vm-undo.el.  Tests cover:
;; - Flag accessors (vm-*-flag functions)
;; - Flag setters with undo (vm-set-*-flag functions)
;; - Direct flag setters (vm-set-*-flag-of functions)
;; - Location data accessors
;; - Softdata accessors
;; - Cached data accessors
;; - Mirror data accessors

;;; Code:

(require 'vm-test-init)
(require 'vm-undo)

;;; Test folder content

(defconst vm-accessors-test-folder
  "From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
To: recipient@example.com
Subject: Test Message
Date: Mon, 01 Jan 2024 10:00:00 +0000
Message-ID: <test123@example.com>

This is the test message body.

"
  "Simple test folder with one message for accessor tests.")

;;; Flag accessor tests

(ert-deftest vm-accessors-test-deleted-flag ()
  "Test vm-deleted-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Initially not deleted
      (should-not (vm-deleted-flag msg))
      ;; Set flag directly and verify accessor returns it
      (vm-set-deleted-flag-of msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag-of msg nil)
      (should-not (vm-deleted-flag msg)))))

(ert-deftest vm-accessors-test-new-flag ()
  "Test vm-new-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Test accessor after setting
      (vm-set-new-flag-of msg t)
      (should (vm-new-flag msg))
      (vm-set-new-flag-of msg nil)
      (should-not (vm-new-flag msg)))))

(ert-deftest vm-accessors-test-unread-flag ()
  "Test vm-unread-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-unread-flag-of msg t)
      (should (vm-unread-flag msg))
      (vm-set-unread-flag-of msg nil)
      (should-not (vm-unread-flag msg)))))

(ert-deftest vm-accessors-test-filed-flag ()
  "Test vm-filed-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-filed-flag msg))
      (vm-set-filed-flag-of msg t)
      (should (vm-filed-flag msg)))))

(ert-deftest vm-accessors-test-replied-flag ()
  "Test vm-replied-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-replied-flag msg))
      (vm-set-replied-flag-of msg t)
      (should (vm-replied-flag msg)))))

(ert-deftest vm-accessors-test-forwarded-flag ()
  "Test vm-forwarded-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-forwarded-flag msg))
      (vm-set-forwarded-flag-of msg t)
      (should (vm-forwarded-flag msg)))))

(ert-deftest vm-accessors-test-written-flag ()
  "Test vm-written-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-written-flag msg))
      (vm-set-written-flag-of msg t)
      (should (vm-written-flag msg)))))

(ert-deftest vm-accessors-test-edited-flag ()
  "Test vm-edited-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-edited-flag msg))
      ;; vm-set-edited-flag-of is defined in vm-message.el, not -of form
      (aset (vm-attributes-of msg) 7 t)
      (should (vm-edited-flag msg)))))

(ert-deftest vm-accessors-test-redistributed-flag ()
  "Test vm-redistributed-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-redistributed-flag msg))
      (vm-set-redistributed-flag-of msg t)
      (should (vm-redistributed-flag msg)))))

(ert-deftest vm-accessors-test-flagged-flag ()
  "Test vm-flagged-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-flagged-flag msg))
      (vm-set-flagged-flag-of msg t)
      (should (vm-flagged-flag msg)))))

(ert-deftest vm-accessors-test-folded-flag ()
  "Test vm-folded-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-folded-flag msg))
      (vm-set-folded-flag-of msg t)
      (should (vm-folded-flag msg)))))

(ert-deftest vm-accessors-test-watched-flag ()
  "Test vm-watched-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-watched-flag msg))
      (vm-set-watched-flag-of msg t)
      (should (vm-watched-flag msg)))))

(ert-deftest vm-accessors-test-ignored-flag ()
  "Test vm-ignored-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-ignored-flag msg))
      (vm-set-ignored-flag-of msg t)
      (should (vm-ignored-flag msg)))))

(ert-deftest vm-accessors-test-read-receipt-flag ()
  "Test vm-read-receipt-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-read-receipt-flag msg))
      (vm-set-read-receipt-flag-of msg t)
      (should (vm-read-receipt-flag msg)))))

(ert-deftest vm-accessors-test-read-receipt-sent-flag ()
  "Test vm-read-receipt-sent-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-read-receipt-sent-flag msg))
      (vm-set-read-receipt-sent-flag-of msg t)
      (should (vm-read-receipt-sent-flag msg)))))

(ert-deftest vm-accessors-test-attachments-flag ()
  "Test vm-attachments-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-attachments-flag msg))
      (vm-set-attachments-flag-of msg t)
      (should (vm-attachments-flag msg)))))

(ert-deftest vm-accessors-test-thread-root-flag ()
  "Test vm-thread-root-flag accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; No setter for thread-root-flag, just test accessor works
      (should-not (vm-thread-root-flag msg))
      ;; Set directly via aset
      (aset (vm-attributes-of msg) 16 t)
      (should (vm-thread-root-flag msg)))))

;;; Direct flag setter tests (vm-set-*-flag-of)
;;; These setters bypass undo recording

(ert-deftest vm-accessors-test-set-flag-of-functions ()
  "Test all vm-set-*-flag-of direct setter functions."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Test each setter sets the correct flag
      (vm-set-new-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 0) t))

      (vm-set-unread-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 1) t))

      (vm-set-deleted-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 2) t))

      (vm-set-filed-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 3) t))

      (vm-set-replied-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 4) t))

      (vm-set-written-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 5) t))

      (vm-set-forwarded-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 6) t))

      (vm-set-redistributed-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 8) t))

      (vm-set-flagged-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 9) t))

      (vm-set-folded-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 10) t))

      (vm-set-watched-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 11) t))

      (vm-set-ignored-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 12) t))

      (vm-set-read-receipt-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 13) t))

      (vm-set-read-receipt-sent-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 14) t))

      (vm-set-attachments-flag-of msg t)
      (should (eq (aref (vm-attributes-of msg) 15) t)))))

(ert-deftest vm-accessors-test-set-flag-of-clear ()
  "Test that vm-set-*-flag-of can clear flags."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Set and then clear each flag
      (vm-set-deleted-flag-of msg t)
      (should (vm-deleted-flag msg))
      (vm-set-deleted-flag-of msg nil)
      (should-not (vm-deleted-flag msg))

      (vm-set-flagged-flag-of msg t)
      (should (vm-flagged-flag msg))
      (vm-set-flagged-flag-of msg nil)
      (should-not (vm-flagged-flag msg)))))

;;; Location data accessor tests

(ert-deftest vm-accessors-test-location-data-of ()
  "Test vm-location-data-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (vectorp (vm-location-data-of msg)))
      (should (= (length (vm-location-data-of msg)) vm-location-data-vector-length)))))

(ert-deftest vm-accessors-test-start-of ()
  "Test vm-start-of accessor for message start position."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (markerp (vm-start-of msg)))
      (should (= (vm-start-of msg) 1)))))

(ert-deftest vm-accessors-test-headers-of ()
  "Test vm-headers-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (markerp (vm-headers-of msg)))
      ;; Headers start after From_ line
      (should (> (vm-headers-of msg) (vm-start-of msg))))))

(ert-deftest vm-accessors-test-text-of ()
  "Test vm-text-of accessor for message body start."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-find-and-set-text-of msg)
      (should (markerp (vm-text-of msg)))
      ;; Text starts after headers (blank line separator)
      (should (> (vm-text-of msg) (vm-headers-of msg))))))

(ert-deftest vm-accessors-test-text-end-of ()
  "Test vm-text-end-of accessor for message body end."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-find-and-set-text-of msg)
      (should (markerp (vm-text-end-of msg)))
      (should (>= (vm-text-end-of msg) (vm-text-of msg))))))

(ert-deftest vm-accessors-test-end-of ()
  "Test vm-end-of accessor for message end position."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (markerp (vm-end-of msg)))
      (should (> (vm-end-of msg) (vm-start-of msg))))))

;;; Location data setter tests

(ert-deftest vm-accessors-test-set-location-data ()
  "Test vm-set-*-of location data setters."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list))
          (marker (point-marker)))
      ;; Test setters work
      (vm-set-start-of msg marker)
      (should (eq (vm-start-of msg) marker))

      (vm-set-headers-of msg marker)
      (should (eq (aref (vm-location-data-of msg) 1) marker))

      (vm-set-vheaders-of msg marker)
      (should (eq (aref (vm-location-data-of msg) 2) marker))

      (vm-set-text-of msg marker)
      (should (eq (aref (vm-location-data-of msg) 3) marker))

      (vm-set-text-end-of msg marker)
      (should (eq (aref (vm-location-data-of msg) 4) marker))

      (vm-set-end-of msg marker)
      (should (eq (aref (vm-location-data-of msg) 5) marker)))))

;;; Softdata accessor tests

(ert-deftest vm-accessors-test-softdata-of ()
  "Test vm-softdata-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (vectorp (vm-softdata-of msg)))
      (should (= (length (vm-softdata-of msg)) vm-softdata-vector-length)))))

(ert-deftest vm-accessors-test-number-of ()
  "Test vm-number-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Number may be nil initially
      (vm-set-number-of msg "1")
      (should (equal (vm-number-of msg) "1")))))

(ert-deftest vm-accessors-test-padded-number-of ()
  "Test vm-padded-number-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-padded-number-of msg "  1")
      (should (equal (vm-padded-number-of msg) "  1")))))

(ert-deftest vm-accessors-test-mark-of ()
  "Test vm-mark-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should-not (vm-mark-of msg))
      (vm-set-mark-of msg t)
      (should (vm-mark-of msg)))))

(ert-deftest vm-accessors-test-su-start-and-end-of ()
  "Test vm-su-start-of and vm-su-end-of accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-su-start-of msg 100)
      (vm-set-su-end-of msg 200)
      (should (= (vm-su-start-of msg) 100))
      (should (= (vm-su-end-of msg) 200)))))

(ert-deftest vm-accessors-test-real-message-sym-of ()
  "Test vm-real-message-sym-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (symbolp (vm-real-message-sym-of msg))))))

(ert-deftest vm-accessors-test-real-message-of ()
  "Test vm-real-message-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; For a non-virtual message, real message is itself
      (should (eq (vm-real-message-of msg) msg)))))

(ert-deftest vm-accessors-test-message-type-of ()
  "Test vm-message-type-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-message-type-of msg 'From_)
      (should (eq (vm-message-type-of msg) 'From_)))))

(ert-deftest vm-accessors-test-message-id-number-of ()
  "Test vm-message-id-number-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Message ID number is set during message creation
      (should (stringp (vm-message-id-number-of msg))))))

(ert-deftest vm-accessors-test-buffer-of ()
  "Test vm-buffer-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (bufferp (vm-buffer-of msg)))
      (should (eq (vm-buffer-of msg) (current-buffer))))))

(ert-deftest vm-accessors-test-thread-indentation-of ()
  "Test vm-thread-indentation-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-thread-indentation-of msg 2)
      (should (= (vm-thread-indentation-of msg) 2)))))

(ert-deftest vm-accessors-test-thread-list-of ()
  "Test vm-thread-list-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-thread-list-of msg '(a b c))
      (should (equal (vm-thread-list-of msg) '(a b c))))))

(ert-deftest vm-accessors-test-babyl-frob-flag-of ()
  "Test vm-babyl-frob-flag-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-babyl-frob-flag-of msg t)
      (should (vm-babyl-frob-flag-of msg)))))

(ert-deftest vm-accessors-test-mime-layout-of ()
  "Test vm-mime-layout-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mime-layout-of msg '(mime layout))
      (should (equal (vm-mime-layout-of msg) '(mime layout))))))

(ert-deftest vm-accessors-test-mime-encoded-header-flag-of ()
  "Test vm-mime-encoded-header-flag-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-mime-encoded-header-flag-of msg t)
      (should (vm-mime-encoded-header-flag-of msg)))))

(ert-deftest vm-accessors-test-message-access-method-of ()
  "Test vm-message-access-method-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-message-access-method-of msg 'imap)
      (should (eq (vm-message-access-method-of msg) 'imap)))))

(ert-deftest vm-accessors-test-thread-subtree-of ()
  "Test vm-thread-subtree-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-thread-subtree-of msg '(subtree))
      (should (equal (vm-thread-subtree-of msg) '(subtree))))))

(ert-deftest vm-accessors-test-thread-indentation-offset-of ()
  "Test vm-thread-indentation-offset-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-thread-indentation-offset-of msg 5)
      (should (= (vm-thread-indentation-offset-of msg) 5)))))

;;; Cached data accessor tests

(ert-deftest vm-accessors-test-cached-data-of ()
  "Test vm-cached-data-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (vectorp (vm-cached-data-of msg)))
      (should (= (length (vm-cached-data-of msg)) vm-cached-data-vector-length)))))

(ert-deftest vm-accessors-test-byte-count-of ()
  "Test vm-byte-count-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-byte-count-of msg "1234")
      (should (equal (vm-byte-count-of msg) "1234")))))

(ert-deftest vm-accessors-test-date-components ()
  "Test date component accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-weekday-of msg "Mon")
      (vm-set-monthday-of msg "01")
      (vm-set-month-of msg "Jan")
      (vm-set-year-of msg "2024")
      (vm-set-hour-of msg "10:00:00")
      (vm-set-zone-of msg "+0000")

      (should (equal (vm-weekday-of msg) "Mon"))
      (should (equal (vm-monthday-of msg) "01"))
      (should (equal (vm-month-of msg) "Jan"))
      (should (equal (vm-year-of msg) "2024"))
      (should (equal (vm-hour-of msg) "10:00:00"))
      (should (equal (vm-zone-of msg) "+0000")))))

(ert-deftest vm-accessors-test-from-of ()
  "Test vm-from-of (alias for vm-decoded-from-of) accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-from-of msg "sender@example.com")
      (should (equal (vm-from-of msg) "sender@example.com"))
      (should (equal (vm-decoded-from-of msg) "sender@example.com")))))

(ert-deftest vm-accessors-test-full-name-of ()
  "Test vm-full-name-of (alias for vm-decoded-full-name-of) accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-full-name-of msg "John Doe")
      (should (equal (vm-full-name-of msg) "John Doe"))
      (should (equal (vm-decoded-full-name-of msg) "John Doe")))))

(ert-deftest vm-accessors-test-message-id-of ()
  "Test vm-message-id-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-message-id-of msg "<test@example.com>")
      (should (equal (vm-message-id-of msg) "<test@example.com>")))))

(ert-deftest vm-accessors-test-line-count-of ()
  "Test vm-line-count-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-line-count-of msg "42")
      (should (equal (vm-line-count-of msg) "42")))))

(ert-deftest vm-accessors-test-subject-of ()
  "Test vm-subject-of (alias for vm-decoded-subject-of) accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-subject-of msg "Test Subject")
      (should (equal (vm-subject-of msg) "Test Subject"))
      (should (equal (vm-decoded-subject-of msg) "Test Subject")))))

(ert-deftest vm-accessors-test-to-of ()
  "Test vm-to-of (alias for vm-decoded-to-of) accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-to-of msg "recipient@example.com")
      (should (equal (vm-to-of msg) "recipient@example.com")))))

(ert-deftest vm-accessors-test-to-names-of ()
  "Test vm-to-names-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-to-names-of msg "Jane Doe")
      (should (equal (vm-to-names-of msg) "Jane Doe")))))

(ert-deftest vm-accessors-test-month-number-of ()
  "Test vm-month-number-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-month-number-of msg "01")
      (should (equal (vm-month-number-of msg) "01")))))

(ert-deftest vm-accessors-test-sortable-datestring-of ()
  "Test vm-sortable-datestring-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-sortable-datestring-of msg "20240101100000")
      (should (equal (vm-sortable-datestring-of msg) "20240101100000")))))

(ert-deftest vm-accessors-test-sortable-subject-of ()
  "Test vm-sortable-subject-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-sortable-subject-of msg "test subject")
      (should (equal (vm-sortable-subject-of msg) "test subject")))))

(ert-deftest vm-accessors-test-parent-of ()
  "Test vm-parent-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-parent-of msg "parent-id")
      (should (equal (vm-parent-of msg) "parent-id")))))

(ert-deftest vm-accessors-test-references-of ()
  "Test vm-references-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-references-of msg '("<ref1>" "<ref2>"))
      (should (equal (vm-references-of msg) '("<ref1>" "<ref2>"))))))

(ert-deftest vm-accessors-test-body-flags ()
  "Test body retrieval flag accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-body-to-be-retrieved-of msg t)
      (should (vm-body-to-be-retrieved-of msg))
      (should-not (vm-body-retrieved-of msg))

      (vm-set-body-to-be-retrieved-of msg nil)
      (should-not (vm-body-to-be-retrieved-of msg))
      (should (vm-body-retrieved-of msg)))))

(ert-deftest vm-accessors-test-pop-uidl-of ()
  "Test vm-pop-uidl-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-pop-uidl-of msg "uidl123")
      (should (equal (vm-pop-uidl-of msg) "uidl123")))))

(ert-deftest vm-accessors-test-imap-uid-of ()
  "Test vm-imap-uid-of accessor (shares slot with pop-uidl)."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-imap-uid-of msg 12345)
      (should (equal (vm-imap-uid-of msg) 12345)))))

(ert-deftest vm-accessors-test-imap-uid-validity-of ()
  "Test vm-imap-uid-validity-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-imap-uid-validity-of msg 67890)
      (should (equal (vm-imap-uid-validity-of msg) 67890)))))

(ert-deftest vm-accessors-test-spam-score-of ()
  "Test vm-spam-score-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-spam-score-of msg 95)
      (should (equal (vm-spam-score-of msg) 95)))))

(ert-deftest vm-accessors-test-declared-parent-of ()
  "Test vm-declared-parent-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-declared-parent-of msg "<parent@example.com>")
      (should (equal (vm-declared-parent-of msg) "<parent@example.com>")))))

(ert-deftest vm-accessors-test-declared-duplicates-of ()
  "Test vm-declared-duplicates-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-declared-duplicates-of msg '("<dup1>" "<dup2>"))
      (should (equal (vm-declared-duplicates-of msg) '("<dup1>" "<dup2>"))))))

(ert-deftest vm-accessors-test-delivery-date-components ()
  "Test delivery date component accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-d-weekday-of msg "Tue")
      (vm-set-d-monthday-of msg "02")
      (vm-set-d-month-of msg "Feb")
      (vm-set-d-year-of msg "2024")
      (vm-set-d-hour-of msg "14:30:00")
      (vm-set-d-zone-of msg "-0500")

      (should (equal (vm-d-weekday-of msg) "Tue"))
      (should (equal (vm-d-monthday-of msg) "02"))
      (should (equal (vm-d-month-of msg) "Feb"))
      (should (equal (vm-d-year-of msg) "2024"))
      (should (equal (vm-d-hour-of msg) "14:30:00"))
      (should (equal (vm-d-zone-of msg) "-0500")))))

(ert-deftest vm-accessors-test-reply-to-accessors ()
  "Test Reply-To header accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-reply-to-name-of msg "Reply Name")
      (vm-set-decoded-reply-to-of msg "reply@example.com")

      (should (equal (vm-reply-to-name-of msg) "Reply Name"))
      (should (equal (vm-reply-to-of msg) "reply@example.com")))))

(ert-deftest vm-accessors-test-to-cc-accessors ()
  "Test To/CC recipient accessors."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-to-cc-names-of msg "John, Jane")
      (vm-set-decoded-to-cc-of msg "john@example.com, jane@example.com")

      (should (equal (vm-to-cc-names-of msg) "John, Jane"))
      (should (equal (vm-to-cc-of msg) "john@example.com, jane@example.com")))))

;;; Mirror data accessor tests

(ert-deftest vm-accessors-test-mirror-data-of ()
  "Test vm-mirror-data-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (vectorp (vm-mirror-data-of msg)))
      (should (= (length (vm-mirror-data-of msg)) vm-mirror-data-vector-length)))))

(ert-deftest vm-accessors-test-edit-buffer-of ()
  "Test vm-edit-buffer-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-edit-buffer-of msg (current-buffer))
      (should (eq (vm-edit-buffer-of msg) (current-buffer))))))

(ert-deftest vm-accessors-test-stuff-flag-of ()
  "Test vm-stuff-flag-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-stuff-flag-of msg t)
      (should (vm-stuff-flag-of msg))
      (vm-set-stuff-flag-of msg nil)
      (should-not (vm-stuff-flag-of msg)))))

(ert-deftest vm-accessors-test-labels-of ()
  "Test vm-labels-of (alias for vm-decoded-labels-of) accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-labels-of msg '("important" "todo"))
      (should (equal (vm-labels-of msg) '("important" "todo"))))))

(ert-deftest vm-accessors-test-label-string-of ()
  "Test vm-label-string-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-decoded-label-string-of msg "important, todo")
      (should (equal (vm-label-string-of msg) "important, todo")))))

(ert-deftest vm-accessors-test-attribute-modflag-of ()
  "Test vm-attribute-modflag-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (vm-set-attribute-modflag-of msg t)
      (should (vm-attribute-modflag-of msg)))))

;;; Message struct accessor tests

(ert-deftest vm-accessors-test-attributes-of ()
  "Test vm-attributes-of accessor."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      (should (vectorp (vm-attributes-of msg)))
      (should (= (length (vm-attributes-of msg)) vm-attributes-vector-length)))))

(ert-deftest vm-accessors-test-set-attributes-of ()
  "Test vm-set-attributes-of setter."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list))
          (new-attrs (make-vector vm-attributes-vector-length nil)))
      (aset new-attrs 0 t)  ; new flag
      (vm-set-attributes-of msg new-attrs)
      (should (eq (vm-attributes-of msg) new-attrs))
      (should (vm-new-flag msg)))))

(ert-deftest vm-accessors-test-set-cached-data-of ()
  "Test vm-set-cached-data-of setter."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list))
          (new-cache (make-vector vm-cached-data-vector-length nil)))
      (aset new-cache 0 "500")  ; byte count
      (vm-set-cached-data-of msg new-cache)
      (should (eq (vm-cached-data-of msg) new-cache))
      (should (equal (vm-byte-count-of msg) "500")))))

;;; Helper function tests

(ert-deftest vm-accessors-test-virtual-message-p ()
  "Test vm-virtual-message-p predicate."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list)))
      ;; Regular message is not virtual
      (should-not (vm-virtual-message-p msg)))))

(ert-deftest vm-accessors-test-vm-make-message ()
  "Test vm-make-message creates proper message struct."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((new-msg (vm-make-message)))
      ;; Should have all required vectors
      (should (vectorp (vm-location-data-of new-msg)))
      (should (vectorp (vm-softdata-of new-msg)))
      (should (vectorp (vm-mirror-data-of new-msg)))
      ;; Should have symbols set up
      (should (symbolp (vm-real-message-sym-of new-msg)))
      ;; Buffer should be current buffer
      (should (eq (vm-buffer-of new-msg) (current-buffer)))
      ;; Message ID number should be set
      (should (stringp (vm-message-id-number-of new-msg))))))

;;; Special flag setter tests (vm-set-*-flag with undo)
;;; These require the vm-undo module

(ert-deftest vm-accessors-test-vm-set-deleted-flag-with-norecord ()
  "Test vm-set-deleted-flag with norecord option."
  (vm-test-with-folder vm-accessors-test-folder
    (let ((msg (car vm-message-list))
          (vm-undo-record-list nil))
      ;; Using norecord should not create undo record
      (vm-set-deleted-flag msg t 'norecord)
      (should (vm-deleted-flag msg))
      ;; Undo list should be empty (or only contain boundaries)
      (should (or (null vm-undo-record-list)
                  (null (car vm-undo-record-list)))))))

;;; Vector helper tests

(ert-deftest vm-accessors-test-set-deleted-flag-in-vector ()
  "Test vm-set-deleted-flag-in-vector helper."
  (let ((v (make-vector 20 nil)))
    (vm-set-deleted-flag-in-vector v t)
    (should (eq (aref v 2) t))))

(ert-deftest vm-accessors-test-set-new-flag-in-vector ()
  "Test vm-set-new-flag-in-vector helper."
  (let ((v (make-vector 20 nil)))
    (vm-set-new-flag-in-vector v t)
    (should (eq (aref v 0) t))))

(provide 'vm-accessors-test)

;;; vm-accessors-test.el ends here
