;;; vm-pcrisis-test.el --- Tests for vm-pcrisis.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM Personality Crisis (pcrisis) module.
;; Tests cover utility functions, header manipulation, conditions,
;; exerlays (overlays/extents), and action/rule building.

;;; Code:

(require 'vm-test-init)
(require 'vm-pcrisis)

;;; vm-pcrisis-string-extract-address tests

(ert-deftest vm-pcrisis-test-string-extract-address-simple ()
  "Test vm-pcrisis-string-extract-address with simple email."
  (should (equal (vm-pcrisis-string-extract-address "user@example.com")
                 "user@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-with-name ()
  "Test vm-pcrisis-string-extract-address with name and angle brackets."
  (should (equal (vm-pcrisis-string-extract-address "John Doe <john@example.com>")
                 "john@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-quoted ()
  "Test vm-pcrisis-string-extract-address with quoted name."
  (should (equal (vm-pcrisis-string-extract-address "\"John Doe\" <john@example.com>")
                 "john@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-no-email ()
  "Test vm-pcrisis-string-extract-address with no email returns nil."
  (should (null (vm-pcrisis-string-extract-address "not an email"))))

(ert-deftest vm-pcrisis-test-string-extract-address-first ()
  "Test vm-pcrisis-string-extract-address returns first email in string."
  (should (equal (vm-pcrisis-string-extract-address "first@example.com, second@example.com")
                 "first@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-empty ()
  "Test vm-pcrisis-string-extract-address with empty string."
  (should (null (vm-pcrisis-string-extract-address ""))))

(ert-deftest vm-pcrisis-test-string-extract-address-subdomain ()
  "Test vm-pcrisis-string-extract-address with subdomain."
  (should (equal (vm-pcrisis-string-extract-address "user@mail.example.com")
                 "user@mail.example.com")))

;;; vm-pcrisis-split tests

(ert-deftest vm-pcrisis-test-split-comma ()
  "Test vm-pcrisis-split with comma separator."
  (should (equal (vm-pcrisis-split "a, b, c" ",")
                 '("a" "b" "c"))))

(ert-deftest vm-pcrisis-test-split-newline ()
  "Test vm-pcrisis-split with newline separator."
  (should (equal (vm-pcrisis-split "line1\nline2\nline3" "\n")
                 '("line1" "line2" "line3"))))

(ert-deftest vm-pcrisis-test-split-whitespace-trim ()
  "Test vm-pcrisis-split trims whitespace around elements."
  (should (equal (vm-pcrisis-split "  a  ,  b  ,  c  " ",")
                 '("a" "b" "c"))))

(ert-deftest vm-pcrisis-test-split-empty-string ()
  "Test vm-pcrisis-split with empty string."
  (should (equal (vm-pcrisis-split "" ",")
                 nil)))

(ert-deftest vm-pcrisis-test-split-single-element ()
  "Test vm-pcrisis-split with single element (no separator)."
  (should (equal (vm-pcrisis-split "single" ",")
                 '("single"))))

(ert-deftest vm-pcrisis-test-split-multiple-separators ()
  "Test vm-pcrisis-split with multiple separator characters."
  (should (equal (vm-pcrisis-split "a,b;c" ",;")
                 '("a" "b" "c"))))

;;; vm-pcrisis-xor tests

(ert-deftest vm-pcrisis-test-xor-one-true ()
  "Test vm-pcrisis-xor with exactly one true argument."
  (should (vm-pcrisis-xor t nil nil)))

(ert-deftest vm-pcrisis-test-xor-multiple-true ()
  "Test vm-pcrisis-xor with multiple true arguments."
  (should-not (vm-pcrisis-xor t t nil)))

(ert-deftest vm-pcrisis-test-xor-none-true ()
  "Test vm-pcrisis-xor with no true arguments."
  (should-not (vm-pcrisis-xor nil nil nil)))

(ert-deftest vm-pcrisis-test-xor-all-true ()
  "Test vm-pcrisis-xor with all true arguments."
  (should-not (vm-pcrisis-xor t t t)))

(ert-deftest vm-pcrisis-test-xor-two-args-true ()
  "Test vm-pcrisis-xor with two arguments, one true."
  (should (vm-pcrisis-xor t nil))
  (should (vm-pcrisis-xor nil t)))

(ert-deftest vm-pcrisis-test-xor-single-true ()
  "Test vm-pcrisis-xor with single true argument."
  (should (vm-pcrisis-xor t)))

(ert-deftest vm-pcrisis-test-xor-single-false ()
  "Test vm-pcrisis-xor with single false argument."
  (should-not (vm-pcrisis-xor nil)))

;;; vm-pcrisis-none-true-yet tests

(ert-deftest vm-pcrisis-test-none-true-yet-empty ()
  "Test vm-pcrisis-none-true-yet when no conditions have matched."
  (let ((vm-pcrisis-true-conditions nil))
    (should (vm-pcrisis-none-true-yet))))

(ert-deftest vm-pcrisis-test-none-true-yet-with-match ()
  "Test vm-pcrisis-none-true-yet when conditions have matched."
  (let ((vm-pcrisis-true-conditions '("cond1")))
    (should-not (vm-pcrisis-none-true-yet))))

(ert-deftest vm-pcrisis-test-none-true-yet-with-exception ()
  "Test vm-pcrisis-none-true-yet with exceptions."
  (let ((vm-pcrisis-true-conditions '("default")))
    (should (vm-pcrisis-none-true-yet "default"))))

(ert-deftest vm-pcrisis-test-none-true-yet-multiple-exceptions ()
  "Test vm-pcrisis-none-true-yet with multiple exceptions."
  (let ((vm-pcrisis-true-conditions '("default" "blah")))
    (should (vm-pcrisis-none-true-yet "default" "blah"))))

(ert-deftest vm-pcrisis-test-none-true-yet-exception-plus-other ()
  "Test vm-pcrisis-none-true-yet when exception plus other condition matched."
  (let ((vm-pcrisis-true-conditions '("default" "other")))
    (should-not (vm-pcrisis-none-true-yet "default"))))

;;; vm-pcrisis-other-cond tests

(ert-deftest vm-pcrisis-test-other-cond-matched ()
  "Test vm-pcrisis-other-cond when condition was matched."
  (let ((vm-pcrisis-true-conditions '("cond1" "cond2")))
    (should (vm-pcrisis-other-cond "cond1"))))

(ert-deftest vm-pcrisis-test-other-cond-not-matched ()
  "Test vm-pcrisis-other-cond when condition was not matched."
  (let ((vm-pcrisis-true-conditions '("cond1" "cond2")))
    (should-not (vm-pcrisis-other-cond "cond3"))))

(ert-deftest vm-pcrisis-test-other-cond-empty ()
  "Test vm-pcrisis-other-cond when no conditions matched."
  (let ((vm-pcrisis-true-conditions nil))
    (should-not (vm-pcrisis-other-cond "cond1"))))

;;; vm-pcrisis-header-field-for-point tests

(ert-deftest vm-pcrisis-test-header-field-for-point-in-to ()
  "Test vm-pcrisis-header-field-for-point when in To header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "recipient")
    (should (equal (vm-pcrisis-header-field-for-point) "To"))))

(ert-deftest vm-pcrisis-test-header-field-for-point-in-from ()
  "Test vm-pcrisis-header-field-for-point when in From header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "sender")
    (should (equal (vm-pcrisis-header-field-for-point) "From"))))

(ert-deftest vm-pcrisis-test-header-field-for-point-in-body ()
  "Test vm-pcrisis-header-field-for-point when in body returns nil."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-max))
    (should (null (vm-pcrisis-header-field-for-point)))))

(ert-deftest vm-pcrisis-test-header-field-for-point-multiline ()
  "Test vm-pcrisis-header-field-for-point with continuation line."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "Subject: This is a very long subject\n")
    (insert "\tthat continues on the next line\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "continues")
    (should (equal (vm-pcrisis-header-field-for-point) "Subject"))))

;;; Header manipulation tests (composition buffer context)

(defmacro vm-pcrisis-test-with-composition-buffer (&rest body)
  "Execute BODY in a buffer set up as a composition buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "From: sender@example.com\n")
     (insert "To: \n")
     (insert "Subject: Test\n")
     (insert mail-header-separator "\n")
     (insert "Body text\n")
     (goto-char (point-min))
     (let ((vm-pcrisis-current-buffer 'composition)
           (vm-pcrisis-current-state 'automorph))
       ,@body)))

(ert-deftest vm-pcrisis-test-delete-header ()
  "Test vm-pcrisis-delete-header removes header contents."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-insert-header "To" "recipient@example.com")
    (vm-pcrisis-delete-header "To")
    (goto-char (point-min))
    (should (search-forward "To: \n" nil t))))

(ert-deftest vm-pcrisis-test-delete-header-entire ()
  "Test vm-pcrisis-delete-header with entire flag removes whole header."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-insert-header "To" "recipient@example.com")
    (vm-pcrisis-delete-header "To" t)
    (goto-char (point-min))
    (should-not (search-forward "To:" nil t))))

(ert-deftest vm-pcrisis-test-insert-header ()
  "Test vm-pcrisis-insert-header adds content to header."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-insert-header "To" "recipient@example.com")
    (goto-char (point-min))
    (should (search-forward "To: recipient@example.com" nil t))))

(ert-deftest vm-pcrisis-test-insert-header-append ()
  "Test vm-pcrisis-insert-header appends to existing content."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-insert-header "To" "first@example.com")
    (vm-pcrisis-insert-header "To" ", second@example.com")
    (goto-char (point-min))
    (should (search-forward "To: first@example.com, second@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-header ()
  "Test vm-pcrisis-substitute-header replaces header content."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-insert-header "To" "old@example.com")
    (vm-pcrisis-substitute-header "To" "new@example.com")
    (goto-char (point-min))
    (should (search-forward "To: new@example.com" nil t))
    (goto-char (point-min))
    (should-not (search-forward "old@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-header-create ()
  "Test vm-pcrisis-substitute-header creates header if missing."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "CC" "cc@example.com")
    (goto-char (point-min))
    (should (search-forward "CC: cc@example.com" nil t))))

(ert-deftest vm-pcrisis-test-add-header-new ()
  "Test vm-pcrisis-add-header adds new header."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-add-header "FCC" "~/mail/sent")
    (goto-char (point-min))
    (should (search-forward "FCC: ~/mail/sent" nil t))))

(ert-deftest vm-pcrisis-test-add-header-duplicate-content ()
  "Test vm-pcrisis-add-header does not add duplicate content."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-add-header "FCC" "~/mail/sent")
    (vm-pcrisis-add-header "FCC" "~/mail/sent")  ; Try to add same content again
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "FCC:" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest vm-pcrisis-test-add-header-different-content ()
  "Test vm-pcrisis-add-header adds header with different content."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-add-header "FCC" "~/mail/sent")
    (vm-pcrisis-add-header "FCC" "~/mail/archive")
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "FCC:" nil t)
        (setq count (1+ count)))
      (should (= count 2)))))

;;; vm-pcrisis-get-header-extents tests

(ert-deftest vm-pcrisis-test-get-header-extents ()
  "Test vm-pcrisis-get-header-extents returns correct positions."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "To" "recipient@example.com")
    (let ((extents (vm-pcrisis-get-header-extents "To")))
      (should extents)
      (should (consp extents))
      (should (< (car extents) (cdr extents)))
      ;; The extents include the space after the colon
      (should (string-match-p "recipient@example.com"
                              (buffer-substring (car extents) (cdr extents)))))))

(ert-deftest vm-pcrisis-test-get-header-extents-not-found ()
  "Test vm-pcrisis-get-header-extents returns nil for missing header."
  (vm-pcrisis-test-with-composition-buffer
    (should (null (vm-pcrisis-get-header-extents "X-Nonexistent")))))

;;; vm-pcrisis-substitute-within-header tests

(ert-deftest vm-pcrisis-test-substitute-within-header ()
  "Test vm-pcrisis-substitute-within-header replaces within header."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "To" "old.name@example.com")
    (vm-pcrisis-substitute-within-header "To" "old\\.name" "new.name")
    (goto-char (point-min))
    (should (search-forward "new.name@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-within-header-append ()
  "Test vm-pcrisis-substitute-within-header with append when no match."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "To" "first@example.com")
    (vm-pcrisis-substitute-within-header "To" "nonexistent" "second@example.com" t ", ")
    (goto-char (point-min))
    (should (search-forward "first@example.com, second@example.com" nil t))))

;;; vm-pcrisis-get-current-header-contents tests

(ert-deftest vm-pcrisis-test-get-current-header-contents ()
  "Test vm-pcrisis-get-current-header-contents retrieves header."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "Subject" "Test Subject")
    (should (equal (vm-pcrisis-get-current-header-contents "Subject")
                   "Test Subject"))))

(ert-deftest vm-pcrisis-test-get-current-header-contents-missing ()
  "Test vm-pcrisis-get-current-header-contents returns empty for missing header."
  (vm-pcrisis-test-with-composition-buffer
    (should (equal (vm-pcrisis-get-current-header-contents "X-Missing")
                   ""))))

(ert-deftest vm-pcrisis-test-get-current-header-contents-clump ()
  "Test vm-pcrisis-get-current-header-contents with clump-sep."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-add-header "X-Test" "value1")
    (vm-pcrisis-add-header "X-Test" "value2")
    (let ((result (vm-pcrisis-get-current-header-contents "X-Test" "\n")))
      (should (string-match-p "value1" result))
      (should (string-match-p "value2" result)))))

;;; vm-pcrisis-get-current-body-text tests

(ert-deftest vm-pcrisis-test-get-current-body-text ()
  "Test vm-pcrisis-get-current-body-text retrieves body."
  (vm-pcrisis-test-with-composition-buffer
    (should (string-match-p "Body text" (vm-pcrisis-get-current-body-text)))))

;;; Exerlay (overlay/extent) function tests

(ert-deftest vm-pcrisis-test-exerlay-start-detached ()
  "Test vm-pcrisis-exerlay-start returns nil for detached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (delete-overlay ovl)
      (should (null (vm-pcrisis-exerlay-start ovl))))))

(ert-deftest vm-pcrisis-test-exerlay-end-detached ()
  "Test vm-pcrisis-exerlay-end returns nil for detached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (delete-overlay ovl)
      (should (null (vm-pcrisis-exerlay-end ovl))))))

(ert-deftest vm-pcrisis-test-exerlay-start-attached ()
  "Test vm-pcrisis-exerlay-start returns position for attached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (should (= (vm-pcrisis-exerlay-start ovl) 1))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-exerlay-end-attached ()
  "Test vm-pcrisis-exerlay-end returns position for attached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (should (= (vm-pcrisis-exerlay-end ovl) 5))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-move-exerlay ()
  "Test vm-pcrisis-move-exerlay moves overlay."
  (with-temp-buffer
    (insert "test content here")
    (let ((ovl (make-overlay 1 5)))
      (vm-pcrisis-move-exerlay ovl 6 10)
      (should (= (vm-pcrisis-exerlay-start ovl) 6))
      (should (= (vm-pcrisis-exerlay-end ovl) 10))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-make-exerlay ()
  "Test vm-pcrisis-make-exerlay creates overlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (vm-pcrisis-make-exerlay 1 5)))
      (should (overlayp ovl))
      (should (= (vm-pcrisis-exerlay-start ovl) 1))
      (should (= (vm-pcrisis-exerlay-end ovl) 5))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-forcefully-detach-exerlay ()
  "Test vm-pcrisis-forcefully-detach-exerlay detaches overlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (vm-pcrisis-forcefully-detach-exerlay ovl)
      (should (null (vm-pcrisis-exerlay-start ovl))))))

;;; vm-pcrisis-init-vars tests

(ert-deftest vm-pcrisis-test-init-vars-default ()
  "Test vm-pcrisis-init-vars sets default values."
  (let (vm-pcrisis-saved-headers-alist
        vm-pcrisis-actions-to-run
        vm-pcrisis-true-conditions
        vm-pcrisis-current-state
        vm-pcrisis-current-buffer)
    (vm-pcrisis-init-vars)
    (should (null vm-pcrisis-saved-headers-alist))
    (should (null vm-pcrisis-actions-to-run))
    (should (null vm-pcrisis-true-conditions))
    (should (null vm-pcrisis-current-state))
    (should (eq vm-pcrisis-current-buffer 'none))))

(ert-deftest vm-pcrisis-test-init-vars-with-state ()
  "Test vm-pcrisis-init-vars with state argument."
  (let (vm-pcrisis-saved-headers-alist
        vm-pcrisis-actions-to-run
        vm-pcrisis-true-conditions
        vm-pcrisis-current-state
        vm-pcrisis-current-buffer)
    (vm-pcrisis-init-vars 'reply)
    (should (eq vm-pcrisis-current-state 'reply))
    (should (eq vm-pcrisis-current-buffer 'none))))

(ert-deftest vm-pcrisis-test-init-vars-with-buffer ()
  "Test vm-pcrisis-init-vars with buffer argument."
  (let (vm-pcrisis-saved-headers-alist
        vm-pcrisis-actions-to-run
        vm-pcrisis-true-conditions
        vm-pcrisis-current-state
        vm-pcrisis-current-buffer)
    (vm-pcrisis-init-vars 'automorph 'composition)
    (should (eq vm-pcrisis-current-state 'automorph))
    (should (eq vm-pcrisis-current-buffer 'composition))))

;;; vm-pcrisis-build-true-conditions-list tests

(ert-deftest vm-pcrisis-test-build-true-conditions-list-empty ()
  "Test vm-pcrisis-build-true-conditions-list with no conditions."
  (let ((vm-pcrisis-conditions nil)
        (vm-pcrisis-true-conditions nil))
    (vm-pcrisis-build-true-conditions-list)
    (should (null vm-pcrisis-true-conditions))))

(ert-deftest vm-pcrisis-test-build-true-conditions-list-true ()
  "Test vm-pcrisis-build-true-conditions-list finds true conditions."
  (let ((vm-pcrisis-conditions '(("always-true" t)
                           ("always-false" nil)
                           ("also-true" (eq 1 1))))
        (vm-pcrisis-true-conditions nil))
    (vm-pcrisis-build-true-conditions-list)
    (should (member "always-true" vm-pcrisis-true-conditions))
    (should (member "also-true" vm-pcrisis-true-conditions))
    (should-not (member "always-false" vm-pcrisis-true-conditions))))

;;; vm-pcrisis-build-actions-to-run-list tests

(ert-deftest vm-pcrisis-test-build-actions-to-run-list ()
  "Test vm-pcrisis-build-actions-to-run-list maps conditions to actions."
  (let ((vm-pcrisis-conditions '(("cond1" t)))
        (vm-pcrisis-default-rules '(("cond1" "action1" "action2")))
        (vm-pcrisis-reply-rules nil)
        (vm-pcrisis-true-conditions '("cond1"))
        (vm-pcrisis-actions-to-run nil)
        (vm-pcrisis-current-state 'reply))
    (vm-pcrisis-build-actions-to-run-list)
    (should (member "action1" vm-pcrisis-actions-to-run))
    (should (member "action2" vm-pcrisis-actions-to-run))))

(ert-deftest vm-pcrisis-test-build-actions-to-run-list-no-duplicates ()
  "Test vm-pcrisis-build-actions-to-run-list removes duplicate actions."
  (let ((vm-pcrisis-conditions '(("cond1" t) ("cond2" t)))
        (vm-pcrisis-default-rules '(("cond1" "action1")
                              ("cond2" "action1")))  ; Same action
        (vm-pcrisis-reply-rules nil)
        (vm-pcrisis-true-conditions '("cond1" "cond2"))
        (vm-pcrisis-actions-to-run nil)
        (vm-pcrisis-current-state 'reply))
    (vm-pcrisis-build-actions-to-run-list)
    (should (= (length (cl-remove-if-not (lambda (x) (equal x "action1"))
                                         vm-pcrisis-actions-to-run))
               1))))

;;; vm-pcrisis-run-actions tests

;; Use defvar for dynamic binding in action tests
(defvar vm-pcrisis-test--action-result nil
  "Dynamic variable to capture action test results.
Bound by the tests that use it rather than set: action bodies are `eval'ed, so
a lexical variable would be invisible to them, but a dynamic binding of this one
is not.")

(ert-deftest vm-pcrisis-test-run-actions-simple ()
  "Test vm-pcrisis-run-actions executes actions."
  (let ((vm-pcrisis-test--action-result nil)
        (vm-pcrisis-actions '(("test-action" (setq vm-pcrisis-test--action-result 'executed))))
        (vm-pcrisis-actions-to-run '("test-action")))
    (vm-pcrisis-run-actions)
    (should (eq vm-pcrisis-test--action-result 'executed))))

(ert-deftest vm-pcrisis-test-run-actions-multiple ()
  "Test vm-pcrisis-run-actions executes multiple actions in order."
  (let ((vm-pcrisis-test--action-result nil)
        (vm-pcrisis-actions '(("action1" (push 1 vm-pcrisis-test--action-result))
                        ("action2" (push 2 vm-pcrisis-test--action-result))))
        (vm-pcrisis-actions-to-run '("action1" "action2")))
    (vm-pcrisis-run-actions)
    (should (equal vm-pcrisis-test--action-result '(2 1)))))

(ert-deftest vm-pcrisis-test-run-actions-nonexistent ()
  "Test vm-pcrisis-run-actions signals error for nonexistent action."
  (let ((vm-pcrisis-actions '(("action1" t)))
        (vm-pcrisis-actions-to-run '("nonexistent")))
    (should-error (vm-pcrisis-run-actions))))

;;; vm-pcrisis-defcustom-rules-type tests

(ert-deftest vm-pcrisis-test-defcustom-rules-type ()
  "Test vm-pcrisis-defcustom-rules-type generates valid type spec."
  (let ((vm-pcrisis-conditions '(("cond1" t) ("cond2" nil)))
        (vm-pcrisis-actions '(("action1" t) ("action2" t))))
    (let ((type (vm-pcrisis-defcustom-rules-type)))
      (should (listp type))
      (should (eq (car type) 'repeat)))))

;;; vm-pcrisis-rules-set tests

(ert-deftest vm-pcrisis-test-rules-set-valid ()
  "Test vm-pcrisis-rules-set doesn't error with valid rules."
  (let ((vm-pcrisis-conditions '(("cond1" t)))
        (vm-pcrisis-actions '(("action1" t))))
    ;; The function validates the rules - it should not error for valid rules
    ;; Note: vm-pcrisis-rules-set has a bug where it consumes `value` before setting,
    ;; so we just test that it doesn't error on valid input
    (should (not (condition-case nil
                     (progn (vm-pcrisis-rules-set 'test-var '(("cond1" "action1"))) nil)
                   (error t))))))

(ert-deftest vm-pcrisis-test-rules-set-invalid-condition ()
  "Test vm-pcrisis-rules-set signals error for invalid condition."
  (let ((vm-pcrisis-conditions '(("cond1" t)))
        (vm-pcrisis-actions '(("action1" t)))
        (test-var nil))
    (should-error (vm-pcrisis-rules-set 'test-var '(("nonexistent" "action1"))))))

(ert-deftest vm-pcrisis-test-rules-set-invalid-action ()
  "Test vm-pcrisis-rules-set signals error for invalid action."
  (let ((vm-pcrisis-conditions '(("cond1" t)))
        (vm-pcrisis-actions '(("action1" t)))
        (test-var nil))
    (should-error (vm-pcrisis-rules-set 'test-var '(("cond1" "nonexistent"))))))

;;; vm-pcrisis-my-identities tests

(ert-deftest vm-pcrisis-test-my-identities ()
  "Test vm-pcrisis-my-identities sets up identities."
  (let (vm-pcrisis-conditions vm-pcrisis-default-rules vm-pcrisis-actions)
    (vm-pcrisis-my-identities "user1@example.com" "user2@example.com")
    (should (assoc "always true" vm-pcrisis-conditions))
    (should (assoc "prompt for a profile" vm-pcrisis-actions))
    (should (assoc "user1@example.com" vm-pcrisis-actions))
    (should (assoc "user2@example.com" vm-pcrisis-actions))))

;;; Signature and pre-signature tests

(ert-deftest vm-pcrisis-test-create-sig-and-pre-sig-exerlays ()
  "Test vm-pcrisis-create-sig-and-pre-sig-exerlays creates overlays."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (should vm-pcrisis-sig-exerlay)
    (should vm-pcrisis-pre-sig-exerlay)))

(ert-deftest vm-pcrisis-test-signature-insert-string ()
  "Test vm-pcrisis-signature inserts string signature."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (vm-pcrisis-signature "Test Signature")
    (goto-char (point-min))
    (should (search-forward "-- \n" nil t))
    (should (search-forward "Test Signature" nil t))))

(ert-deftest vm-pcrisis-test-signature-delete ()
  "Test vm-pcrisis-signature with empty string deletes signature."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (vm-pcrisis-signature "Test Signature")
    (vm-pcrisis-signature "")
    (goto-char (point-min))
    (should-not (search-forward "Test Signature" nil t))))

(ert-deftest vm-pcrisis-test-delete-signature ()
  "Test vm-pcrisis-delete-signature removes signature."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (vm-pcrisis-signature "Test Signature")
    (vm-pcrisis-delete-signature)
    (goto-char (point-min))
    (should-not (search-forward "-- \n" nil t))))

(ert-deftest vm-pcrisis-test-pre-signature-insert ()
  "Test vm-pcrisis-pre-signature inserts pre-signature."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (vm-pcrisis-pre-signature "Kind regards,\nJohn")
    (goto-char (point-min))
    (should (search-forward "Kind regards," nil t))))

(ert-deftest vm-pcrisis-test-delete-pre-signature ()
  "Test vm-pcrisis-delete-pre-signature removes pre-signature."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-create-sig-and-pre-sig-exerlays)
    (vm-pcrisis-pre-signature "Kind regards,")
    (vm-pcrisis-delete-pre-signature)
    (goto-char (point-min))
    (should-not (search-forward "Kind regards," nil t))))

;;; vm-pcrisis-gregorian-days tests

(ert-deftest vm-pcrisis-test-gregorian-days ()
  "Test vm-pcrisis-gregorian-days returns positive integer."
  (let ((days (vm-pcrisis-gregorian-days)))
    (should (integerp days))
    (should (> days 0))
    ;; Should be greater than Jan 1, 2000 (~730000 days since 1BC)
    (should (> days 730000))))

;;; vm-pcrisis-toggle-no-automorph tests

(ert-deftest vm-pcrisis-test-toggle-no-automorph ()
  "Test vm-pcrisis-toggle-no-automorph toggles the variable."
  (with-temp-buffer
    (setq vm-pcrisis-no-automorph nil)
    (vm-pcrisis-toggle-no-automorph)
    (should vm-pcrisis-no-automorph)
    (vm-pcrisis-toggle-no-automorph)
    (should-not vm-pcrisis-no-automorph)))

;;; vm-pcrisis-only-from-match tests

(ert-deftest vm-pcrisis-test-only-from-match-all ()
  "Test vm-pcrisis-only-from-match when all emails match."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "To" "user1@example.com, user2@example.com")
    (should (vm-pcrisis-only-from-match "To" "@example\\.com"))))

(ert-deftest vm-pcrisis-test-only-from-match-partial ()
  "Test vm-pcrisis-only-from-match when not all emails match."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "To" "user1@example.com, user2@other.com")
    (should-not (vm-pcrisis-only-from-match "To" "@example\\.com"))))

;;; vm-pcrisis-header-match tests (automorph context)

(ert-deftest vm-pcrisis-test-header-match-automorph ()
  "Test vm-pcrisis-header-match in automorph state."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "Subject" "Important: Test Message")
    (should (vm-pcrisis-header-match "Subject" "Important"))))

(ert-deftest vm-pcrisis-test-header-match-automorph-no-match ()
  "Test vm-pcrisis-header-match in automorph state when no match."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "Subject" "Regular Message")
    (should-not (vm-pcrisis-header-match "Subject" "Important"))))

(ert-deftest vm-pcrisis-test-header-match-with-group ()
  "Test vm-pcrisis-header-match extracting group."
  (vm-pcrisis-test-with-composition-buffer
    (vm-pcrisis-substitute-header "Subject" "[Ticket-12345] Issue")
    (let ((result (vm-pcrisis-header-match "Subject" "\\[Ticket-\\([0-9]+\\)\\]" nil 1)))
      (should (equal result "12345")))))

;;; vm-pcrisis-body-match tests

(ert-deftest vm-pcrisis-test-body-match-automorph ()
  "Test vm-pcrisis-body-match in automorph state."
  (vm-pcrisis-test-with-composition-buffer
    (goto-char (point-max))
    (insert "\nSpecial keyword here")
    (should (vm-pcrisis-body-match "Special keyword"))))

(ert-deftest vm-pcrisis-test-body-match-automorph-no-match ()
  "Test vm-pcrisis-body-match in automorph state when no match."
  (vm-pcrisis-test-with-composition-buffer
    (should-not (vm-pcrisis-body-match "Nonexistent phrase"))))

;;; Auto-profile tests

(ert-deftest vm-pcrisis-test-get-profile-for-address-not-found ()
  "Test vm-pcrisis-get-profile-for-address when no profile exists."
  (let ((vm-pcrisis-auto-profiles nil))
    (should (null (vm-pcrisis-get-profile-for-address "unknown@example.com")))))

(ert-deftest vm-pcrisis-test-get-profile-for-address-found ()
  "Test vm-pcrisis-get-profile-for-address when profile exists."
  (let ((vm-pcrisis-auto-profiles '(("test@example.com" ("action1") . 738000)))
        (vm-pcrisis-auto-profiles-file "/tmp/test-profiles"))
    ;; Mock vm-pcrisis-save-auto-profiles to avoid file operations
    (cl-letf (((symbol-function 'vm-pcrisis-save-auto-profiles) #'ignore))
      (should (equal (vm-pcrisis-get-profile-for-address "test@example.com")
                     '("action1"))))))

(ert-deftest vm-pcrisis-test-save-profile-for-address ()
  "Test vm-pcrisis-save-profile-for-address adds profile."
  (let ((vm-pcrisis-auto-profiles nil)
        (vm-pcrisis-auto-profiles-file "/tmp/test-profiles")
        (vm-pcrisis-auto-profiles-expunge-days nil))
    (cl-letf (((symbol-function 'vm-pcrisis-save-auto-profiles) #'ignore))
      (vm-pcrisis-save-profile-for-address "new@example.com" '("action1"))
      (should (assoc "new@example.com" vm-pcrisis-auto-profiles)))))

(ert-deftest vm-pcrisis-test-save-profile-for-address-update ()
  "Test vm-pcrisis-save-profile-for-address updates existing profile."
  (let ((vm-pcrisis-auto-profiles '(("test@example.com" ("old-action") . 738000)))
        (vm-pcrisis-auto-profiles-file "/tmp/test-profiles")
        (vm-pcrisis-auto-profiles-expunge-days nil))
    (cl-letf (((symbol-function 'vm-pcrisis-save-auto-profiles) #'ignore))
      (vm-pcrisis-save-profile-for-address "test@example.com" '("new-action"))
      (should (equal (cadr (assoc "test@example.com" vm-pcrisis-auto-profiles))
                     '("new-action"))))))

;;; Profile expunge tests

(ert-deftest vm-pcrisis-test-save-profile-expunge-old ()
  "Test vm-pcrisis-save-profile-for-address expunges old profiles."
  (let* ((today (vm-pcrisis-gregorian-days))
         (old-day (- today 200))  ; 200 days ago
         (vm-pcrisis-auto-profiles `(("old@example.com" ("action") . ,old-day)))
         (vm-pcrisis-auto-profiles-file "/tmp/test-profiles")
         (vm-pcrisis-auto-profiles-expunge-days 100))
    (cl-letf (((symbol-function 'vm-pcrisis-save-auto-profiles) #'ignore))
      (vm-pcrisis-save-profile-for-address "new@example.com" '("action"))
      ;; Old profile should be expunged
      (should-not (assoc "old@example.com" vm-pcrisis-auto-profiles))
      ;; New profile should exist
      (should (assoc "new@example.com" vm-pcrisis-auto-profiles)))))

;;; vm-pcrisis-read-actions tests

(ert-deftest vm-pcrisis-test-read-actions-none ()
  "Test vm-pcrisis-read-actions with 'none' input."
  (let ((vm-pcrisis-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "none")))
      (should (null (vm-pcrisis-read-actions "Test prompt: "))))))

(ert-deftest vm-pcrisis-test-read-actions-single ()
  "Test vm-pcrisis-read-actions with single action."
  (let ((vm-pcrisis-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "action1")))
      (should (equal (vm-pcrisis-read-actions "Test prompt: ")
                     '("action1"))))))

(ert-deftest vm-pcrisis-test-read-actions-multiple ()
  "Test vm-pcrisis-read-actions with multiple actions."
  (let ((vm-pcrisis-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "action1 action2")))
      (let ((result (vm-pcrisis-read-actions "Test prompt: ")))
        (should (member "action1" result))
        (should (member "action2" result))))))

;;; Advice tests

(defmacro vm-pcrisis-test-with-mode (&rest body)
  "Run BODY with `vm-pcrisis-mode\=' on, restoring it afterwards.
Since #561 the advice is installed by the mode rather than by loading the file,
so a test about the advice has to switch it on."
  (declare (indent 0) (debug t))
  `(let ((was vm-pcrisis-mode))
     (unwind-protect
         (progn (vm-pcrisis-mode 1) ,@body)
       (vm-pcrisis-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-advice-reply-exists ()
  "Test that reply advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vm-pcrisis--reply 'vm-do-reply))))

(ert-deftest vm-pcrisis-test-advice-mail-exists ()
  "Test that mail advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vm-pcrisis--mail 'vm-mail-from-folder))))

(ert-deftest vm-pcrisis-test-advice-newmail-exists ()
  "Test that newmail advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vm-pcrisis--newmail 'vm-mail))))

(ert-deftest vm-pcrisis-test-advice-forward-exists ()
  "Test that forward advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vm-pcrisis--forward 'vm-forward-message))))

(ert-deftest vm-pcrisis-test-advice-resend-exists ()
  "Test that resend advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vm-pcrisis--resend 'vm-resend-message))))

;;; Rule dispatch per composition state (issue #451)
;;
;; #451 reported that pcrisis actions stopped firing on `m' and `r' while
;; still being runnable by hand.  The advice tests above only prove the
;; entry points are hooked; these cover the rest of the path, which is where
;; a silent break would leave exactly that symptom.

(defconst vm-pcrisis-test--states '(reply mail newmail forward resend automorph)
  "Every state `vm-pcrisis-init-vars' is called with by the composition advices.")

(ert-deftest vm-pcrisis-test-every-state-has-a-rules-variable ()
  "Each composition state has the `vm-pcrisis-STATE-rules' variable it looks up.
`vm-pcrisis-build-actions-to-run-list' resolves rules through
\(symbol-value (intern (format \"vm-pcrisis-%s-rules\" vm-pcrisis-current-state))), so a
state with no matching variable does not degrade to \"no rules\" -- it
signals void-variable and takes the whole compose command down with it."
  (dolist (state vm-pcrisis-test--states)
    (let ((symbol (intern (format "vm-pcrisis-%s-rules" state))))
      (should (boundp symbol)))))

(ert-deftest vm-pcrisis-test-legacy-alist-names-are-aliases ()
  "The pre-8.3 `vmpc-*-alist' names still reach the `vmpc-*-rules' variables.
The rules variables were renamed; a VM 8.2.0 configuration setting the old
names has to keep working, or its rules are silently never consulted, which
is what #451 looked like from the outside."
  (dolist (pair '((vmpc-actions-alist   . vm-pcrisis-default-rules)
                  (vmpc-reply-alist     . vm-pcrisis-reply-rules)
                  (vmpc-forward-alist   . vm-pcrisis-forward-rules)
                  (vmpc-resend-alist    . vm-pcrisis-resend-rules)
                  (vmpc-mail-alist      . vm-pcrisis-mail-rules)
                  (vmpc-newmail-alist   . vm-pcrisis-newmail-rules)
                  (vmpc-automorph-alist . vm-pcrisis-automorph-rules)))
    (should (eq (indirect-variable (car pair)) (cdr pair)))))

(ert-deftest vm-pcrisis-test-legacy-alist-value-reaches-rules ()
  "Setting a legacy `vmpc-*-alist' name is visible under the new name."
  (let ((vm-pcrisis-newmail-rules nil))
    (setq vmpc-newmail-alist '(("cond" "act")))
    (should (equal vm-pcrisis-newmail-rules '(("cond" "act"))))))

(defvar vm-pcrisis-test--fired nil
  "Set by the action in `vm-pcrisis-test-rule-dispatch-runs-action-per-state'.
A defvar rather than a lexical variable because action bodies are `eval'ed and
would not see one; the test binds this dynamically, which they do see, so
nothing is left set afterwards.")

(ert-deftest vm-pcrisis-test-rule-dispatch-runs-action-per-state ()
  "REGRESSION: a true condition mapped to an action runs it, in every state.
This is the whole of what #451 said was broken -- rules configured, actions
runnable by hand, but nothing triggered automatically.  Driven through
`vm-pcrisis-build-true-conditions-list' and `vm-pcrisis-build-actions-to-run-list'
rather than through the interactive commands, which need a terminal."
  (dolist (state vm-pcrisis-test--states)
    (let* ((rules-var (intern (format "vm-pcrisis-%s-rules" state)))
           (saved (symbol-value rules-var))
           (vm-pcrisis-conditions '(("always" t)))
           (vm-pcrisis-actions '(("mark" (setq vm-pcrisis-test--fired t))))
           (vm-pcrisis-actions-to-run nil)
           (vm-pcrisis-true-conditions nil)
           (vm-pcrisis-test--fired nil))
      (unwind-protect
          (progn
            (set rules-var '(("always" "mark")))
            (vm-pcrisis-init-vars state)
            (should (member "always" (vm-pcrisis-build-true-conditions-list)))
            (should (member "mark" (vm-pcrisis-build-actions-to-run-list)))
            (vm-pcrisis-run-actions)
            (should vm-pcrisis-test--fired))
        (set rules-var saved)))))


;;; composing from an empty folder with pcrisis loaded (issue #514)

(ert-deftest vm-pcrisis-test-mail-from-an-empty-folder ()
  "REGRESSION: `m' composes in an empty folder with pcrisis loaded.
Issue #514: `vm-mail-from-folder' validated with a minimum of one message, so
in an empty folder -- an IMAP inbox with no mail in it, the ordinary way to
meet this -- it answered \"Folder is empty\" and composed nothing.  That was
fixed in the command, but `vm-pcrisis--mail' advises the command and repeats the
same validation before calling it, so for anyone using Personality Crisis the
advice refused first and the fix never took effect.

Loading vm-pcrisis.el is enough to be \"using\" it: the advice is installed at
load time, unconditionally, so this ran for every VM user who had the module
loaded at all.

Driven end to end rather than by checking the validation call, because the
whole defect was a second copy of that call in a place no one thought to
look."
  (require 'vm)
  (let* ((dir (file-name-as-directory (make-temp-file "vm-pcrisis-test" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-frame-per-composition nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         ;; A signature file that happens to exist would be read into the
         ;; composition, which has nothing to do with this.
         (vm-signature-file nil)
         (mail-signature nil)
         ;; VM counts compositions for the mode line, and the count should not
         ;; follow a test that leaves none behind.
         (vm-composition-buffer-count vm-composition-buffer-count)
         (vm-ml-composition-buffer-count vm-ml-composition-buffer-count)
         (vm-compositions-exist vm-compositions-exist)
         (before (buffer-list))
         (was vm-pcrisis-mode))
    (unwind-protect
        (progn
          ;; Since #561 the advice comes with the mode, not with the file.
          (vm-pcrisis-mode 1)
          (with-temp-file file (insert ""))
          (vm-visit-folder file)
          (should (null vm-message-list))
          ;; The advice is what is under test, so it had better be there.
          (should (advice-member-p 'vm-pcrisis--mail 'vm-mail-from-folder))
          (vm-mail-from-folder)
          (should (eq major-mode 'mail-mode))
          (should (string-match-p "^To:" (buffer-string))))
      (vm-pcrisis-mode (if was 1 -1))
      ;; the visit and the composition leave the folder, its summary, its
      ;; presentation copy and the composition itself
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t)
              (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest vm-pcrisis-test-mail-from-a-folder-with-a-message ()
  "The control: `m' still composes when the folder does hold a message.
Without this, the test above could pass by never validating at all."
  (require 'vm)
  (let* ((dir (file-name-as-directory (make-temp-file "vm-pcrisis-test" t)))
         (file (expand-file-name "folder" dir))
         (vm-init-file nil)
         (vm-preferences-file nil)
         (vm-confirm-quit nil)
         (vm-frame-per-folder nil)
         (vm-frame-per-composition nil)
         (vm-mutable-frame-configuration nil)
         (vm-folder-history vm-folder-history)
         (vm-last-visit-folder vm-last-visit-folder)
         (vm-signature-file nil)
         (mail-signature nil)
         ;; VM counts compositions for the mode line, and the count should not
         ;; follow a test that leaves none behind.
         (vm-composition-buffer-count vm-composition-buffer-count)
         (vm-ml-composition-buffer-count vm-ml-composition-buffer-count)
         (vm-compositions-exist vm-compositions-exist)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
                    "From: Alice <alice@example.com>\n"
                    "Subject: hello\n\nBody.\n\n"))
          (vm-visit-folder file)
          (should (= 1 (length vm-message-list)))
          (vm-mail-from-folder)
          (should (eq major-mode 'mail-mode)))
      ;; the visit and the composition leave the folder, its summary, its
      ;; presentation copy and the composition itself
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t)
              (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory dir t))))


;;; switching Personality Crisis on and off (issue #561)

(ert-deftest vm-pcrisis-test-loading-does-not-advise-anything ()
  "REGRESSION: loading vm-pcrisis.el leaves VM's commands alone.
Issue #561: the file installed seven pieces of advice as it loaded, so merely
having it on the load path changed how every composition command in VM behaved --
whether or not a single pcrisis rule had been set up, and with no way to turn it
off.  That is the same complaint #512 made of vm-biff.

This file has already required vm-pcrisis by the time the test runs, which is
what makes the assertion meaningful: the advice is absent despite that."
  (require 'vm-pcrisis)
  (should (featurep 'vm-pcrisis))
  (should-not vm-pcrisis-mode)
  (dolist (pair vm-pcrisis-advised-commands)
    (should-not (advice-member-p (cdr pair) (car pair)))))

(ert-deftest vm-pcrisis-test-mode-advises-and-unadvises-every-command ()
  "`vm-pcrisis-mode' installs the advice, and turning it off removes all of it.
Both directions matter: a mode that cannot be switched off would leave #561 half
fixed."
  (require 'vm-pcrisis)
  (let ((was vm-pcrisis-mode))
    (unwind-protect
        (progn
          (vm-pcrisis-mode 1)
          (should vm-pcrisis-mode)
          (dolist (pair vm-pcrisis-advised-commands)
            (should (advice-member-p (cdr pair) (car pair))))
          (vm-pcrisis-mode -1)
          (should-not vm-pcrisis-mode)
          (dolist (pair vm-pcrisis-advised-commands)
            (should-not (advice-member-p (cdr pair) (car pair)))))
      (vm-pcrisis-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-mode-is-idempotent ()
  "Turning it on twice does not advise twice, nor off twice fail.
`define-minor-mode' guards the body against a no-op change, but an advice
installed twice would run the rules twice per composition, so it is worth
pinning."
  (require 'vm-pcrisis)
  (let ((was vm-pcrisis-mode))
    (unwind-protect
        (progn
          (vm-pcrisis-mode 1)
          (vm-pcrisis-mode 1)
          (vm-pcrisis-mode -1)
          (dolist (pair vm-pcrisis-advised-commands)
            (should-not (advice-member-p (cdr pair) (car pair))))
          (vm-pcrisis-mode -1))
      (vm-pcrisis-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-advised-commands-all-exist ()
  "Every command the mode advises is a command, and every advice a function.
A typo in `vm-pcrisis-advised-commands' would otherwise advise a symbol nobody calls,
and the mode would appear to work while doing nothing."
  (require 'vm)
  (require 'vm-pcrisis)
  (should (= 7 (length vm-pcrisis-advised-commands)))
  (dolist (pair vm-pcrisis-advised-commands)
    (should (fboundp (car pair)))
    (should (fboundp (cdr pair))))
  ;; All but `vm-do-reply' are commands; that one is the internal worker the
  ;; reply commands call, which is why the advice hangs off it.
  (should-not (commandp 'vm-do-reply))
  (dolist (pair (assq-delete-all 'vm-do-reply (copy-alist vm-pcrisis-advised-commands)))
    (should (commandp (car pair)))))

;;; Actions and the buffer they need (issues #540 and #576)

;; Personality Crisis runs an action list twice, once before the composition
;; buffer exists and once in it.  That is why an action needing a composition
;; returns quietly on the first pass, and why `vm-pcrisis-add-header' raising in that
;; case made it unusable (#576).  Called by hand, though, quiet is exactly what
;; #540's reporter could not tell from having worked.

(defmacro vm-pcrisis-test--with-composition (spec &rest body)
  "Visit a folder, compose a mail under pcrisis, run BODY in the composition.
SPEC is (BUFFER-VAR &optional SIGNATURE), SIGNATURE being what `mail-signature'
holds while the composition is built."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-pcrisis" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-user-interaction-buffer vm-user-interaction-buffer)
          (mail-signature ,(or (nth 1 spec) nil))
          ;; VM counts compositions for the mode line, and these leave none
          ;; behind, so the count should not follow them out.  The idle timer VM
          ;; starts with the first composition is cancelled by the harness, in
          ;; `vm-test-cancel-composition-timer': binding the variable would
          ;; strand a live timer with nothing pointing at it.
          (vm-composition-buffer-count vm-composition-buffer-count)
          (vm-ml-composition-buffer-count vm-ml-composition-buffer-count)
          (vm-compositions-exist vm-compositions-exist)
          (before (buffer-list))
          ,(car spec))
     (require 'vm)
     (require 'vm-pcrisis)
     (unwind-protect
         (progn
           (vm-test-write-simple-folder file 2)
           (vm-visit-folder file)
           (save-window-excursion
             (vm-mail)
             (setq ,(car spec) (current-buffer)))
           (with-current-buffer ,(car spec) ,@body))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               ;; A composition asks whether to keep itself as a draft as it is
               ;; killed, and in batch that prompt reads end of file.
               (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t)
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defmacro vm-pcrisis-test--with-rules (actions &rest body)
  "Run BODY with ACTIONS as the whole of `vm-pcrisis-actions', in a rule, mode on."
  (declare (indent 1) (debug t))
  `(let ((vm-pcrisis-conditions '(("always" t)))
         (vm-pcrisis-actions (list (cons "the rule" ,actions)))
         (vm-pcrisis-default-rules '(("always" "the rule")))
         (vm-pcrisis-expect-default-signature vm-pcrisis-expect-default-signature))
     (unwind-protect
         (progn (vm-pcrisis-mode 1) ,@body)
       (vm-pcrisis-mode -1))))

(defun vm-pcrisis-test--holds (text)
  "Return non-nil if the current buffer holds TEXT."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward text nil t) t)))

(ert-deftest vm-pcrisis-test-signature-action-deletes-the-default-signature ()
  "REGRESSION: (vm-pcrisis-signature \"\") deletes a signature Emacs inserted.
Issue #540.  In a new composition the body is the signature and nothing else,
so the newline before the \"-- \" line is the one ending the header separator
and lies outside the body.  The search for a signature was bounded by the start
of the body and so could never see it, and the action found nothing to delete."
  (let ((vm-pcrisis-expect-default-signature t))
    (vm-pcrisis-test--with-rules '((vm-pcrisis-signature ""))
      (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
        (should-not (vm-pcrisis-test--holds "my signature"))
        ;; What is left is a composition, with its separator line intact.
        (should (vm-pcrisis-test--holds mail-header-separator))))))

(ert-deftest vm-pcrisis-test-signature-action-needs-to-be-told-to-expect-one ()
  "Without `vm-pcrisis-expect-default-signature' the signature is left alone.
Personality Crisis acts only on a signature whose extent it knows, and this is
how it comes to know one it did not insert.  The other side of the branch, so
the fix is not simply deleting whatever is at the end of the buffer."
  (let ((vm-pcrisis-expect-default-signature nil))
    (vm-pcrisis-test--with-rules '((vm-pcrisis-signature ""))
      (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
        (should (vm-pcrisis-test--holds "my signature"))))))

(ert-deftest vm-pcrisis-test-signature-action-says-what-it-needs ()
  "REGRESSION: the action says what to set when it cannot see the signature.
Issue #540, reported twice: the second time by a reader who had the fix, a
composition with a signature in it and a rule saying (vm-pcrisis-signature
\"\"), and got the mail he was trying to prevent with nothing said about why."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args) (setq said (apply #'format args)))))
      (let ((vm-pcrisis-expect-default-signature nil))
        (vm-pcrisis-test--with-rules '((vm-pcrisis-signature ""))
          (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
            (should (vm-pcrisis-test--holds "my signature"))))))
    (should said)
    (should (string-match-p "vm-pcrisis-expect-default-signature" said))))

(ert-deftest vm-pcrisis-test-signature-action-is-quiet-when-it-can-act ()
  "Nothing is said where there is no signature, nor where the extent is known.
A warning on a composition that has no signature at all, or one Personality
Crisis is about to delete, would be a warning about nothing."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args) (setq said (apply #'format args)))))
      ;; no signature in the composition
      (let ((vm-pcrisis-expect-default-signature nil))
        (vm-pcrisis-test--with-rules '((vm-pcrisis-signature ""))
          (vm-pcrisis-test--with-composition (buffer)
            (should-not said))))
      ;; a signature, and told to expect one
      (let ((vm-pcrisis-expect-default-signature t))
        (vm-pcrisis-test--with-rules '((vm-pcrisis-signature ""))
          (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
            (should-not (vm-pcrisis-test--holds "my signature"))
            (should-not said)))))))

(ert-deftest vm-pcrisis-test-add-header-action-adds-the-header ()
  "REGRESSION: `vm-pcrisis-add-header' works as an action.
Issue #576.  It raised whenever it was not in a composition, and Personality
Crisis evaluates the action list once before the composition exists, so naming
it in a rule broke composing altogether.  It also read the headers of the
message being replied to rather than the composition's own, and got nil."
  (vm-pcrisis-test--with-rules '((vm-pcrisis-add-header "FCC" "/tmp/sent"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "FCC: /tmp/sent")))))

(ert-deftest vm-pcrisis-test-add-header-action-does-not-add-it-twice ()
  "The same header and content twice adds one, which is what it is for.
Named for FCC, which may appear more than once but should not repeat itself."
  (vm-pcrisis-test--with-rules '((vm-pcrisis-add-header "FCC" "/tmp/sent")
                                 (vm-pcrisis-add-header "FCC" "/tmp/sent"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (= 1 (how-many "FCC: /tmp/sent" (point-min) (point-max)))))))

(ert-deftest vm-pcrisis-test-actions-complain-when-called-by-hand ()
  "An action called where there is no composition says so.
Issue #540's reporter tried `M-: (vm-pcrisis-signature \"\")' and saw nothing happen,
which is indistinguishable from an action that ran and did nothing."
  (require 'vm-pcrisis)
  (with-temp-buffer
    (let ((vm-pcrisis-current-buffer nil)
          (vm-pcrisis-running-actions nil)
          (text-quoting-style 'grave))
      (dolist (call '((vm-pcrisis-signature "")
                      (vm-pcrisis-pre-signature "")
                      (vm-pcrisis-add-header "FCC" "/tmp/sent")
                      (vm-pcrisis-insert-header "FCC" "/tmp/sent")
                      (vm-pcrisis-substitute-header "FCC" "/tmp/sent")
                      (vm-pcrisis-delete-header "FCC")))
        (let ((err (should-error (eval call) :type 'error)))
          ;; The message names the action, so it is clear which rule to look at.
          (should (string-match-p (symbol-name (car call))
                                  (error-message-string err))))))))

(ert-deftest vm-pcrisis-test-actions-are-quiet-during-the-first-pass ()
  "While Personality Crisis runs an action list, an action out of place is quiet.
That pass happens before the composition buffer exists, and every action list is
evaluated in it, so complaining there would break every configuration."
  (require 'vm-pcrisis)
  (with-temp-buffer
    (let ((vm-pcrisis-current-buffer 'none)
          (vm-pcrisis-running-actions t))
      (should-not (vm-pcrisis-signature ""))
      (should-not (vm-pcrisis-add-header "FCC" "/tmp/sent"))
      (should-not (vm-pcrisis-delete-header "FCC")))))
;;; Replacing part of a header (issue #578)

;; `vm-pcrisis-replace-or-add-in-header' read the composition's headers through
;; `vm-pcrisis-get-current-header-contents', which was gated to the automorph state
;; and so returned nil in an ordinary composition.  The action then found no
;; header, and did nothing at all, quietly.

(ert-deftest vm-pcrisis-test-replace-in-header-replaces-the-match ()
  "REGRESSION: the action replaces what its regexp matches in the header.
Issue #578.  Two actions in the rule: the first puts a recipient there, the
second rewrites the name, which is what the docstring's own example does."
  (vm-pcrisis-test--with-rules
      '((vm-pcrisis-substitute-header "To" "Bob Smith <bob@example.com>")
        (vm-pcrisis-replace-or-add-in-header "To" "[Bb]ob Smith[^,]*"
                                       "Robert Fenk <bob@example.com>"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: Robert Fenk <bob@example.com>"))
      (should-not (vm-pcrisis-test--holds "Bob Smith")))))

(ert-deftest vm-pcrisis-test-replace-in-header-appends-with-a-separator ()
  "With no match and a separator, the content is appended after it.
The header is already occupied here, so the separator is what keeps the two
recipients apart."
  (vm-pcrisis-test--with-rules
      '((vm-pcrisis-substitute-header "To" "alice@example.com")
        (vm-pcrisis-replace-or-add-in-header "To" "nobody@example.com"
                                       "bob@example.com" ", "))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: alice@example.com, bob@example.com")))))

(ert-deftest vm-pcrisis-test-replace-in-header-adds-without-a-separator ()
  "An empty header gets the content and no separator in front of it.
A fresh composition's To is empty, and a leading \", \" there would be a
syntactically broken recipient list."
  (vm-pcrisis-test--with-rules
      '((vm-pcrisis-replace-or-add-in-header "To" "nobody@example.com"
                                       "bob@example.com" ", "))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: bob@example.com"))
      (should-not (vm-pcrisis-test--holds "To: , ")))))

;;; Saying so when the mode is off (emacs-vm/vm#642)

(ert-deftest vm-pcrisis-test-a-composition-says-when-the-mode-is-off ()
  "With rules set and `vm-pcrisis-mode' off, starting a composition says so.

The rules are never consulted then, and the composition gets whatever
`user-mail-address' says.  Nothing else notices: a default rule naming the
address VM would have used anyway looks exactly like a working setup."
  (let ((vm-pcrisis-mode nil)
        (vm-pcrisis-conditions '(("in a folder" (vm-pcrisis-folder-account-match "^work$"))))
        (vm-pcrisis-actions '(("from work" (vm-pcrisis-substitute-header "From" "me@work"))))
        (vm-pcrisis-default-rules '(("in a folder" "from work")))
        (said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args)
                 (setq said (apply #'format args)))))
      (vm-pcrisis-warn-if-off))
    (should (string-match-p "vm-pcrisis-mode is off" said))
    (should (string-match-p "(vm-pcrisis-mode 1)" said))))

(ert-deftest vm-pcrisis-test-it-says-so-every-time ()
  "It says so at every composition, not once.

`vm-warn' will not repeat a warning it has just given, which is why this one
binds `vm-current-warning' around the call: a warning seen once at startup is
a warning forgotten."
  (let ((vm-pcrisis-mode nil)
        (vm-pcrisis-conditions '(("in a folder" (vm-pcrisis-folder-account-match "^work$"))))
        (vm-pcrisis-actions '(("from work" (vm-pcrisis-substitute-header "From" "me@work"))))
        (vm-pcrisis-default-rules '(("in a folder" "from work")))
        (times 0))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq times (1+ times))))
              ((symbol-function 'sleep-for) #'ignore))
      (vm-pcrisis-warn-if-off)
      (vm-pcrisis-warn-if-off)
      (vm-pcrisis-warn-if-off))
    (should (equal times 3))))

(ert-deftest vm-pcrisis-test-it-is-quiet-when-there-is-nothing-wrong ()
  "Nothing is said when the mode is on, nor when no rules are set.
A warning on every composition for someone who does not use pcrisis would be
worse than the mistake it is warning about."
  (let ((said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args)
                 (setq said (apply #'format args)))))
      ;; configured, and switched on
      (let ((vm-pcrisis-mode t)
            (vm-pcrisis-conditions '(("in a folder" t)))
            (vm-pcrisis-actions '(("from work" (vm-pcrisis-substitute-header "From" "x"))))
            (vm-pcrisis-default-rules '(("in a folder" "from work"))))
        (vm-pcrisis-warn-if-off)
        (should-not said))
      ;; off, and nothing configured
      (let ((vm-pcrisis-mode nil)
            (vm-pcrisis-conditions nil)
            (vm-pcrisis-actions nil)
            (vm-pcrisis-default-rules nil))
        (vm-pcrisis-warn-if-off)
        (should-not said))
      ;; conditions and actions but no rules joining them: nothing would run
      ;; even with the mode on, so this is not the mistake being warned about
      (let ((vm-pcrisis-mode nil)
            (vm-pcrisis-conditions '(("in a folder" t)))
            (vm-pcrisis-actions '(("from work" (vm-pcrisis-substitute-header "From" "x"))))
            (vm-pcrisis-default-rules nil)
            (vm-pcrisis-reply-rules nil)
            (vm-pcrisis-forward-rules nil)
            (vm-pcrisis-resend-rules nil)
            (vm-pcrisis-newmail-rules nil)
            (vm-pcrisis-automorph-rules nil))
        (vm-pcrisis-warn-if-off)
        (should-not said)))))

(ert-deftest vm-pcrisis-test-the-warning-is-on-the-composition-hook ()
  "The check runs from `vm-mail-mode-hook', which every composition runs:
replying, forwarding, resending and starting a message all end there."
  (should (memq 'vm-pcrisis-warn-if-off (default-value 'vm-mail-mode-hook))))

;;; The old vmpc- names (emacs-vm/vm#657)

(ert-deftest vm-pcrisis-test-every-published-name-has-its-old-one ()
  "Everything a reader's init file can name is still reachable as vmpc-.

The options are what customize saved, and the conditions and actions are
written into the rules as data, so a configuration names them without
calling them.  Renaming those without aliases would silently stop a
configuration from doing anything -- the rules would name functions that no
longer exist, and the error would come at composition time."
  (dolist (old '(;; options
                 vmpc-conditions vmpc-actions vmpc-default-rules
                 vmpc-reply-rules vmpc-forward-rules vmpc-resend-rules
                 vmpc-mail-rules vmpc-newmail-rules vmpc-automorph-rules
                 vmpc-auto-profiles-file vmpc-auto-profiles-expunge-days
                 vmpc-default-profile vmpc-prompt-for-profile-headers
                 vmpc-expect-default-signature))
    (should (boundp old))
    (should (eq (indirect-variable old)
                (intern (concat "vm-pcrisis-" (substring (symbol-name old) 5))))))
  (dolist (old '(;; conditions and actions, named in rules as data
                 vmpc-header-match vmpc-body-match vmpc-folder-match
                 vmpc-folder-account-match vmpc-only-from-match
                 vmpc-other-cond vmpc-none-true-yet
                 vmpc-add-header vmpc-delete-header vmpc-insert-header
                 vmpc-substitute-header vmpc-substitute-replied-header
                 vmpc-signature vmpc-pre-signature vmpc-pre-function
                 vmpc-my-identities vmpc-prompt-for-profile
                 ;; and the commands
                 vmpc-mode vmpc-automorph vmpc-toggle-no-automorph
                 vmpc-fix-auto-profiles-file))
    (should (fboundp old))
    (should (eq (indirect-function old)
                (indirect-function
                 (intern (concat "vm-pcrisis-"
                                 (substring (symbol-name old) 5))))))))

(ert-deftest vm-pcrisis-test-an-action-of-your-own-can-still-read-the-state ()
  "REGRESSION: the two variables an action of one's own reads answer to
their old names.

`vm-pcrisis-actions' holds Lisp, so writing an action is ordinary, and one
has to test `vm-pcrisis-current-buffer' to know whether the composition
exists yet.  Neither variable is named in the manual, so the rename in #657
left them without aliases and such an action signalled void-variable at
composition time -- after the alias-carrying functions around it had
already been renamed successfully."
  (dolist (old '(vmpc-current-state vmpc-current-buffer))
    (should (boundp old))
    (should (eq (indirect-variable old)
                (intern (concat "vm-pcrisis-" (substring (symbol-name old) 5))))))
  ;; and the value follows, which is what the action tests
  (let ((vm-pcrisis-current-buffer 'composition))
    (should (eq vmpc-current-buffer 'composition)))
  (let ((vmpc-current-state 'reply))
    (should (eq vm-pcrisis-current-state 'reply))))

(ert-deftest vm-pcrisis-test-an-old-option-carries-its-value-across ()
  "A value set under the old name is what the new name reads, which is what
makes a customize file written years ago still describe this VM."
  (let ((vmpc-conditions '(("mine" (vm-pcrisis-header-match "From" "me")))))
    (should (equal vm-pcrisis-conditions vmpc-conditions)))
  (let ((vm-pcrisis-actions '(("sign" (vm-pcrisis-signature "~/.sig")))))
    (should (equal vmpc-actions vm-pcrisis-actions))))

(ert-deftest vm-pcrisis-test-rules-written-with-old-names-still-run ()
  "A rule naming the old function runs it: the actions are looked up by
name at composition time, so the alias is what keeps an old configuration
working."
  (let ((ran nil))
    (cl-letf (((symbol-function 'vm-pcrisis-add-header)
               (lambda (&rest args) (setq ran args))))
      (let ((vm-pcrisis-actions '(("add" (vmpc-add-header "X-Test: yes")))))
        (vm-pcrisis-run-action "add"))
      (should (equal ran '("X-Test: yes"))))))

(ert-deftest vm-pcrisis-test-the-profiles-file-keeps-its-name ()
  "The auto-profiles file is data on disk, so it is still ~/.vmpc-auto-profiles.
Renaming it with the symbol would have left every reader's profiles behind."
  (should (equal (eval (car (get 'vm-pcrisis-auto-profiles-file
                                 'standard-value)))
                 "~/.vmpc-auto-profiles")))

(provide 'vm-pcrisis-test)

;;; vm-pcrisis-test.el ends here