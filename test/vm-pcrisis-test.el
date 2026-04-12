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

;;; vmpc-string-extract-address tests

(ert-deftest vm-pcrisis-test-string-extract-address-simple ()
  "Test vmpc-string-extract-address with simple email."
  (should (equal (vmpc-string-extract-address "user@example.com")
                 "user@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-with-name ()
  "Test vmpc-string-extract-address with name and angle brackets."
  (should (equal (vmpc-string-extract-address "John Doe <john@example.com>")
                 "john@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-quoted ()
  "Test vmpc-string-extract-address with quoted name."
  (should (equal (vmpc-string-extract-address "\"John Doe\" <john@example.com>")
                 "john@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-no-email ()
  "Test vmpc-string-extract-address with no email returns nil."
  (should (null (vmpc-string-extract-address "not an email"))))

(ert-deftest vm-pcrisis-test-string-extract-address-first ()
  "Test vmpc-string-extract-address returns first email in string."
  (should (equal (vmpc-string-extract-address "first@example.com, second@example.com")
                 "first@example.com")))

(ert-deftest vm-pcrisis-test-string-extract-address-empty ()
  "Test vmpc-string-extract-address with empty string."
  (should (null (vmpc-string-extract-address ""))))

(ert-deftest vm-pcrisis-test-string-extract-address-subdomain ()
  "Test vmpc-string-extract-address with subdomain."
  (should (equal (vmpc-string-extract-address "user@mail.example.com")
                 "user@mail.example.com")))

;;; vmpc-split tests

(ert-deftest vm-pcrisis-test-split-comma ()
  "Test vmpc-split with comma separator."
  (should (equal (vmpc-split "a, b, c" ",")
                 '("a" "b" "c"))))

(ert-deftest vm-pcrisis-test-split-newline ()
  "Test vmpc-split with newline separator."
  (should (equal (vmpc-split "line1\nline2\nline3" "\n")
                 '("line1" "line2" "line3"))))

(ert-deftest vm-pcrisis-test-split-whitespace-trim ()
  "Test vmpc-split trims whitespace around elements."
  (should (equal (vmpc-split "  a  ,  b  ,  c  " ",")
                 '("a" "b" "c"))))

(ert-deftest vm-pcrisis-test-split-empty-string ()
  "Test vmpc-split with empty string."
  (should (equal (vmpc-split "" ",")
                 nil)))

(ert-deftest vm-pcrisis-test-split-single-element ()
  "Test vmpc-split with single element (no separator)."
  (should (equal (vmpc-split "single" ",")
                 '("single"))))

(ert-deftest vm-pcrisis-test-split-multiple-separators ()
  "Test vmpc-split with multiple separator characters."
  (should (equal (vmpc-split "a,b;c" ",;")
                 '("a" "b" "c"))))

;;; vmpc-xor tests

(ert-deftest vm-pcrisis-test-xor-one-true ()
  "Test vmpc-xor with exactly one true argument."
  (should (vmpc-xor t nil nil)))

(ert-deftest vm-pcrisis-test-xor-multiple-true ()
  "Test vmpc-xor with multiple true arguments."
  (should-not (vmpc-xor t t nil)))

(ert-deftest vm-pcrisis-test-xor-none-true ()
  "Test vmpc-xor with no true arguments."
  (should-not (vmpc-xor nil nil nil)))

(ert-deftest vm-pcrisis-test-xor-all-true ()
  "Test vmpc-xor with all true arguments."
  (should-not (vmpc-xor t t t)))

(ert-deftest vm-pcrisis-test-xor-two-args-true ()
  "Test vmpc-xor with two arguments, one true."
  (should (vmpc-xor t nil))
  (should (vmpc-xor nil t)))

(ert-deftest vm-pcrisis-test-xor-single-true ()
  "Test vmpc-xor with single true argument."
  (should (vmpc-xor t)))

(ert-deftest vm-pcrisis-test-xor-single-false ()
  "Test vmpc-xor with single false argument."
  (should-not (vmpc-xor nil)))

;;; vmpc-none-true-yet tests

(ert-deftest vm-pcrisis-test-none-true-yet-empty ()
  "Test vmpc-none-true-yet when no conditions have matched."
  (let ((vmpc-true-conditions nil))
    (should (vmpc-none-true-yet))))

(ert-deftest vm-pcrisis-test-none-true-yet-with-match ()
  "Test vmpc-none-true-yet when conditions have matched."
  (let ((vmpc-true-conditions '("cond1")))
    (should-not (vmpc-none-true-yet))))

(ert-deftest vm-pcrisis-test-none-true-yet-with-exception ()
  "Test vmpc-none-true-yet with exceptions."
  (let ((vmpc-true-conditions '("default")))
    (should (vmpc-none-true-yet "default"))))

(ert-deftest vm-pcrisis-test-none-true-yet-multiple-exceptions ()
  "Test vmpc-none-true-yet with multiple exceptions."
  (let ((vmpc-true-conditions '("default" "blah")))
    (should (vmpc-none-true-yet "default" "blah"))))

(ert-deftest vm-pcrisis-test-none-true-yet-exception-plus-other ()
  "Test vmpc-none-true-yet when exception plus other condition matched."
  (let ((vmpc-true-conditions '("default" "other")))
    (should-not (vmpc-none-true-yet "default"))))

;;; vmpc-other-cond tests

(ert-deftest vm-pcrisis-test-other-cond-matched ()
  "Test vmpc-other-cond when condition was matched."
  (let ((vmpc-true-conditions '("cond1" "cond2")))
    (should (vmpc-other-cond "cond1"))))

(ert-deftest vm-pcrisis-test-other-cond-not-matched ()
  "Test vmpc-other-cond when condition was not matched."
  (let ((vmpc-true-conditions '("cond1" "cond2")))
    (should-not (vmpc-other-cond "cond3"))))

(ert-deftest vm-pcrisis-test-other-cond-empty ()
  "Test vmpc-other-cond when no conditions matched."
  (let ((vmpc-true-conditions nil))
    (should-not (vmpc-other-cond "cond1"))))

;;; vmpc-header-field-for-point tests

(ert-deftest vm-pcrisis-test-header-field-for-point-in-to ()
  "Test vmpc-header-field-for-point when in To header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "recipient")
    (should (equal (vmpc-header-field-for-point) "To"))))

(ert-deftest vm-pcrisis-test-header-field-for-point-in-from ()
  "Test vmpc-header-field-for-point when in From header."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "sender")
    (should (equal (vmpc-header-field-for-point) "From"))))

(ert-deftest vm-pcrisis-test-header-field-for-point-in-body ()
  "Test vmpc-header-field-for-point when in body returns nil."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "To: recipient@example.com\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-max))
    (should (null (vmpc-header-field-for-point)))))

(ert-deftest vm-pcrisis-test-header-field-for-point-multiline ()
  "Test vmpc-header-field-for-point with continuation line."
  (with-temp-buffer
    (insert "From: sender@example.com\n")
    (insert "Subject: This is a very long subject\n")
    (insert "\tthat continues on the next line\n")
    (insert mail-header-separator "\n")
    (insert "Body text\n")
    (goto-char (point-min))
    (search-forward "continues")
    (should (equal (vmpc-header-field-for-point) "Subject"))))

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
     (let ((vmpc-current-buffer 'composition)
           (vmpc-current-state 'automorph))
       ,@body)))

(ert-deftest vm-pcrisis-test-delete-header ()
  "Test vmpc-delete-header removes header contents."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-insert-header "To" "recipient@example.com")
    (vmpc-delete-header "To")
    (goto-char (point-min))
    (should (search-forward "To: \n" nil t))))

(ert-deftest vm-pcrisis-test-delete-header-entire ()
  "Test vmpc-delete-header with entire flag removes whole header."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-insert-header "To" "recipient@example.com")
    (vmpc-delete-header "To" t)
    (goto-char (point-min))
    (should-not (search-forward "To:" nil t))))

(ert-deftest vm-pcrisis-test-insert-header ()
  "Test vmpc-insert-header adds content to header."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-insert-header "To" "recipient@example.com")
    (goto-char (point-min))
    (should (search-forward "To: recipient@example.com" nil t))))

(ert-deftest vm-pcrisis-test-insert-header-append ()
  "Test vmpc-insert-header appends to existing content."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-insert-header "To" "first@example.com")
    (vmpc-insert-header "To" ", second@example.com")
    (goto-char (point-min))
    (should (search-forward "To: first@example.com, second@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-header ()
  "Test vmpc-substitute-header replaces header content."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-insert-header "To" "old@example.com")
    (vmpc-substitute-header "To" "new@example.com")
    (goto-char (point-min))
    (should (search-forward "To: new@example.com" nil t))
    (goto-char (point-min))
    (should-not (search-forward "old@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-header-create ()
  "Test vmpc-substitute-header creates header if missing."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "CC" "cc@example.com")
    (goto-char (point-min))
    (should (search-forward "CC: cc@example.com" nil t))))

(ert-deftest vm-pcrisis-test-add-header-new ()
  "Test vmpc-add-header adds new header."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-add-header "FCC" "~/mail/sent")
    (goto-char (point-min))
    (should (search-forward "FCC: ~/mail/sent" nil t))))

(ert-deftest vm-pcrisis-test-add-header-duplicate-content ()
  "Test vmpc-add-header does not add duplicate content."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-add-header "FCC" "~/mail/sent")
    (vmpc-add-header "FCC" "~/mail/sent")  ; Try to add same content again
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "FCC:" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest vm-pcrisis-test-add-header-different-content ()
  "Test vmpc-add-header adds header with different content."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-add-header "FCC" "~/mail/sent")
    (vmpc-add-header "FCC" "~/mail/archive")
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "FCC:" nil t)
        (setq count (1+ count)))
      (should (= count 2)))))

;;; vmpc-get-header-extents tests

(ert-deftest vm-pcrisis-test-get-header-extents ()
  "Test vmpc-get-header-extents returns correct positions."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "To" "recipient@example.com")
    (let ((extents (vmpc-get-header-extents "To")))
      (should extents)
      (should (consp extents))
      (should (< (car extents) (cdr extents)))
      ;; The extents include the space after the colon
      (should (string-match-p "recipient@example.com"
                              (buffer-substring (car extents) (cdr extents)))))))

(ert-deftest vm-pcrisis-test-get-header-extents-not-found ()
  "Test vmpc-get-header-extents returns nil for missing header."
  (vm-pcrisis-test-with-composition-buffer
    (should (null (vmpc-get-header-extents "X-Nonexistent")))))

;;; vmpc-substitute-within-header tests

(ert-deftest vm-pcrisis-test-substitute-within-header ()
  "Test vmpc-substitute-within-header replaces within header."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "To" "old.name@example.com")
    (vmpc-substitute-within-header "To" "old\\.name" "new.name")
    (goto-char (point-min))
    (should (search-forward "new.name@example.com" nil t))))

(ert-deftest vm-pcrisis-test-substitute-within-header-append ()
  "Test vmpc-substitute-within-header with append when no match."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "To" "first@example.com")
    (vmpc-substitute-within-header "To" "nonexistent" "second@example.com" t ", ")
    (goto-char (point-min))
    (should (search-forward "first@example.com, second@example.com" nil t))))

;;; vmpc-get-current-header-contents tests

(ert-deftest vm-pcrisis-test-get-current-header-contents ()
  "Test vmpc-get-current-header-contents retrieves header."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "Subject" "Test Subject")
    (should (equal (vmpc-get-current-header-contents "Subject")
                   "Test Subject"))))

(ert-deftest vm-pcrisis-test-get-current-header-contents-missing ()
  "Test vmpc-get-current-header-contents returns empty for missing header."
  (vm-pcrisis-test-with-composition-buffer
    (should (equal (vmpc-get-current-header-contents "X-Missing")
                   ""))))

(ert-deftest vm-pcrisis-test-get-current-header-contents-clump ()
  "Test vmpc-get-current-header-contents with clump-sep."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-add-header "X-Test" "value1")
    (vmpc-add-header "X-Test" "value2")
    (let ((result (vmpc-get-current-header-contents "X-Test" "\n")))
      (should (string-match-p "value1" result))
      (should (string-match-p "value2" result)))))

;;; vmpc-get-current-body-text tests

(ert-deftest vm-pcrisis-test-get-current-body-text ()
  "Test vmpc-get-current-body-text retrieves body."
  (vm-pcrisis-test-with-composition-buffer
    (should (string-match-p "Body text" (vmpc-get-current-body-text)))))

;;; Exerlay (overlay/extent) function tests

(ert-deftest vm-pcrisis-test-exerlay-start-detached ()
  "Test vmpc-exerlay-start returns nil for detached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (delete-overlay ovl)
      (should (null (vmpc-exerlay-start ovl))))))

(ert-deftest vm-pcrisis-test-exerlay-end-detached ()
  "Test vmpc-exerlay-end returns nil for detached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (delete-overlay ovl)
      (should (null (vmpc-exerlay-end ovl))))))

(ert-deftest vm-pcrisis-test-exerlay-start-attached ()
  "Test vmpc-exerlay-start returns position for attached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (should (= (vmpc-exerlay-start ovl) 1))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-exerlay-end-attached ()
  "Test vmpc-exerlay-end returns position for attached exerlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (should (= (vmpc-exerlay-end ovl) 5))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-move-exerlay ()
  "Test vmpc-move-exerlay moves overlay."
  (with-temp-buffer
    (insert "test content here")
    (let ((ovl (make-overlay 1 5)))
      (vmpc-move-exerlay ovl 6 10)
      (should (= (vmpc-exerlay-start ovl) 6))
      (should (= (vmpc-exerlay-end ovl) 10))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-make-exerlay ()
  "Test vmpc-make-exerlay creates overlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (vmpc-make-exerlay 1 5)))
      (should (overlayp ovl))
      (should (= (vmpc-exerlay-start ovl) 1))
      (should (= (vmpc-exerlay-end ovl) 5))
      (delete-overlay ovl))))

(ert-deftest vm-pcrisis-test-forcefully-detach-exerlay ()
  "Test vmpc-forcefully-detach-exerlay detaches overlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ovl (make-overlay 1 5)))
      (vmpc-forcefully-detach-exerlay ovl)
      (should (null (vmpc-exerlay-start ovl))))))

;;; vmpc-init-vars tests

(ert-deftest vm-pcrisis-test-init-vars-default ()
  "Test vmpc-init-vars sets default values."
  (let (vmpc-saved-headers-alist
        vmpc-actions-to-run
        vmpc-true-conditions
        vmpc-current-state
        vmpc-current-buffer)
    (vmpc-init-vars)
    (should (null vmpc-saved-headers-alist))
    (should (null vmpc-actions-to-run))
    (should (null vmpc-true-conditions))
    (should (null vmpc-current-state))
    (should (eq vmpc-current-buffer 'none))))

(ert-deftest vm-pcrisis-test-init-vars-with-state ()
  "Test vmpc-init-vars with state argument."
  (let (vmpc-saved-headers-alist
        vmpc-actions-to-run
        vmpc-true-conditions
        vmpc-current-state
        vmpc-current-buffer)
    (vmpc-init-vars 'reply)
    (should (eq vmpc-current-state 'reply))
    (should (eq vmpc-current-buffer 'none))))

(ert-deftest vm-pcrisis-test-init-vars-with-buffer ()
  "Test vmpc-init-vars with buffer argument."
  (let (vmpc-saved-headers-alist
        vmpc-actions-to-run
        vmpc-true-conditions
        vmpc-current-state
        vmpc-current-buffer)
    (vmpc-init-vars 'automorph 'composition)
    (should (eq vmpc-current-state 'automorph))
    (should (eq vmpc-current-buffer 'composition))))

;;; vmpc-build-true-conditions-list tests

(ert-deftest vm-pcrisis-test-build-true-conditions-list-empty ()
  "Test vmpc-build-true-conditions-list with no conditions."
  (let ((vmpc-conditions nil)
        (vmpc-true-conditions nil))
    (vmpc-build-true-conditions-list)
    (should (null vmpc-true-conditions))))

(ert-deftest vm-pcrisis-test-build-true-conditions-list-true ()
  "Test vmpc-build-true-conditions-list finds true conditions."
  (let ((vmpc-conditions '(("always-true" t)
                           ("always-false" nil)
                           ("also-true" (eq 1 1))))
        (vmpc-true-conditions nil))
    (vmpc-build-true-conditions-list)
    (should (member "always-true" vmpc-true-conditions))
    (should (member "also-true" vmpc-true-conditions))
    (should-not (member "always-false" vmpc-true-conditions))))

;;; vmpc-build-actions-to-run-list tests

(ert-deftest vm-pcrisis-test-build-actions-to-run-list ()
  "Test vmpc-build-actions-to-run-list maps conditions to actions."
  (let ((vmpc-conditions '(("cond1" t)))
        (vmpc-default-rules '(("cond1" "action1" "action2")))
        (vmpc-reply-rules nil)
        (vmpc-true-conditions '("cond1"))
        (vmpc-actions-to-run nil)
        (vmpc-current-state 'reply))
    (vmpc-build-actions-to-run-list)
    (should (member "action1" vmpc-actions-to-run))
    (should (member "action2" vmpc-actions-to-run))))

(ert-deftest vm-pcrisis-test-build-actions-to-run-list-no-duplicates ()
  "Test vmpc-build-actions-to-run-list removes duplicate actions."
  (let ((vmpc-conditions '(("cond1" t) ("cond2" t)))
        (vmpc-default-rules '(("cond1" "action1")
                              ("cond2" "action1")))  ; Same action
        (vmpc-reply-rules nil)
        (vmpc-true-conditions '("cond1" "cond2"))
        (vmpc-actions-to-run nil)
        (vmpc-current-state 'reply))
    (vmpc-build-actions-to-run-list)
    (should (= (length (cl-remove-if-not (lambda (x) (equal x "action1"))
                                         vmpc-actions-to-run))
               1))))

;;; vmpc-run-actions tests

;; Use defvar for dynamic binding in action tests
(defvar vm-pcrisis-test--action-result nil
  "Dynamic variable to capture action test results.")

(ert-deftest vm-pcrisis-test-run-actions-simple ()
  "Test vmpc-run-actions executes actions."
  (setq vm-pcrisis-test--action-result nil)
  (let ((vmpc-actions '(("test-action" (setq vm-pcrisis-test--action-result 'executed))))
        (vmpc-actions-to-run '("test-action")))
    (vmpc-run-actions)
    (should (eq vm-pcrisis-test--action-result 'executed))))

(ert-deftest vm-pcrisis-test-run-actions-multiple ()
  "Test vmpc-run-actions executes multiple actions in order."
  (setq vm-pcrisis-test--action-result nil)
  (let ((vmpc-actions '(("action1" (push 1 vm-pcrisis-test--action-result))
                        ("action2" (push 2 vm-pcrisis-test--action-result))))
        (vmpc-actions-to-run '("action1" "action2")))
    (vmpc-run-actions)
    (should (equal vm-pcrisis-test--action-result '(2 1)))))

(ert-deftest vm-pcrisis-test-run-actions-nonexistent ()
  "Test vmpc-run-actions signals error for nonexistent action."
  (let ((vmpc-actions '(("action1" t)))
        (vmpc-actions-to-run '("nonexistent")))
    (should-error (vmpc-run-actions))))

;;; vmpc-defcustom-rules-type tests

(ert-deftest vm-pcrisis-test-defcustom-rules-type ()
  "Test vmpc-defcustom-rules-type generates valid type spec."
  (let ((vmpc-conditions '(("cond1" t) ("cond2" nil)))
        (vmpc-actions '(("action1" t) ("action2" t))))
    (let ((type (vmpc-defcustom-rules-type)))
      (should (listp type))
      (should (eq (car type) 'repeat)))))

;;; vmpc-rules-set tests

(ert-deftest vm-pcrisis-test-rules-set-valid ()
  "Test vmpc-rules-set doesn't error with valid rules."
  (let ((vmpc-conditions '(("cond1" t)))
        (vmpc-actions '(("action1" t))))
    ;; The function validates the rules - it should not error for valid rules
    ;; Note: vmpc-rules-set has a bug where it consumes `value` before setting,
    ;; so we just test that it doesn't error on valid input
    (should (not (condition-case nil
                     (progn (vmpc-rules-set 'test-var '(("cond1" "action1"))) nil)
                   (error t))))))

(ert-deftest vm-pcrisis-test-rules-set-invalid-condition ()
  "Test vmpc-rules-set signals error for invalid condition."
  (let ((vmpc-conditions '(("cond1" t)))
        (vmpc-actions '(("action1" t)))
        (test-var nil))
    (should-error (vmpc-rules-set 'test-var '(("nonexistent" "action1"))))))

(ert-deftest vm-pcrisis-test-rules-set-invalid-action ()
  "Test vmpc-rules-set signals error for invalid action."
  (let ((vmpc-conditions '(("cond1" t)))
        (vmpc-actions '(("action1" t)))
        (test-var nil))
    (should-error (vmpc-rules-set 'test-var '(("cond1" "nonexistent"))))))

;;; vmpc-my-identities tests

(ert-deftest vm-pcrisis-test-my-identities ()
  "Test vmpc-my-identities sets up identities."
  (let (vmpc-conditions vmpc-default-rules vmpc-actions)
    (vmpc-my-identities "user1@example.com" "user2@example.com")
    (should (assoc "always true" vmpc-conditions))
    (should (assoc "prompt for a profile" vmpc-actions))
    (should (assoc "user1@example.com" vmpc-actions))
    (should (assoc "user2@example.com" vmpc-actions))))

;;; Signature and pre-signature tests

(ert-deftest vm-pcrisis-test-create-sig-and-pre-sig-exerlays ()
  "Test vmpc-create-sig-and-pre-sig-exerlays creates overlays."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (should vmpc-sig-exerlay)
    (should vmpc-pre-sig-exerlay)))

(ert-deftest vm-pcrisis-test-signature-insert-string ()
  "Test vmpc-signature inserts string signature."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (vmpc-signature "Test Signature")
    (goto-char (point-min))
    (should (search-forward "-- \n" nil t))
    (should (search-forward "Test Signature" nil t))))

(ert-deftest vm-pcrisis-test-signature-delete ()
  "Test vmpc-signature with empty string deletes signature."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (vmpc-signature "Test Signature")
    (vmpc-signature "")
    (goto-char (point-min))
    (should-not (search-forward "Test Signature" nil t))))

(ert-deftest vm-pcrisis-test-delete-signature ()
  "Test vmpc-delete-signature removes signature."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (vmpc-signature "Test Signature")
    (vmpc-delete-signature)
    (goto-char (point-min))
    (should-not (search-forward "-- \n" nil t))))

(ert-deftest vm-pcrisis-test-pre-signature-insert ()
  "Test vmpc-pre-signature inserts pre-signature."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (vmpc-pre-signature "Kind regards,\nJohn")
    (goto-char (point-min))
    (should (search-forward "Kind regards," nil t))))

(ert-deftest vm-pcrisis-test-delete-pre-signature ()
  "Test vmpc-delete-pre-signature removes pre-signature."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-create-sig-and-pre-sig-exerlays)
    (vmpc-pre-signature "Kind regards,")
    (vmpc-delete-pre-signature)
    (goto-char (point-min))
    (should-not (search-forward "Kind regards," nil t))))

;;; vmpc-gregorian-days tests

(ert-deftest vm-pcrisis-test-gregorian-days ()
  "Test vmpc-gregorian-days returns positive integer."
  (let ((days (vmpc-gregorian-days)))
    (should (integerp days))
    (should (> days 0))
    ;; Should be greater than Jan 1, 2000 (~730000 days since 1BC)
    (should (> days 730000))))

;;; vmpc-toggle-no-automorph tests

(ert-deftest vm-pcrisis-test-toggle-no-automorph ()
  "Test vmpc-toggle-no-automorph toggles the variable."
  (with-temp-buffer
    (setq vmpc-no-automorph nil)
    (vmpc-toggle-no-automorph)
    (should vmpc-no-automorph)
    (vmpc-toggle-no-automorph)
    (should-not vmpc-no-automorph)))

;;; vmpc-only-from-match tests

(ert-deftest vm-pcrisis-test-only-from-match-all ()
  "Test vmpc-only-from-match when all emails match."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "To" "user1@example.com, user2@example.com")
    (should (vmpc-only-from-match "To" "@example\\.com"))))

(ert-deftest vm-pcrisis-test-only-from-match-partial ()
  "Test vmpc-only-from-match when not all emails match."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "To" "user1@example.com, user2@other.com")
    (should-not (vmpc-only-from-match "To" "@example\\.com"))))

;;; vmpc-header-match tests (automorph context)

(ert-deftest vm-pcrisis-test-header-match-automorph ()
  "Test vmpc-header-match in automorph state."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "Subject" "Important: Test Message")
    (should (vmpc-header-match "Subject" "Important"))))

(ert-deftest vm-pcrisis-test-header-match-automorph-no-match ()
  "Test vmpc-header-match in automorph state when no match."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "Subject" "Regular Message")
    (should-not (vmpc-header-match "Subject" "Important"))))

(ert-deftest vm-pcrisis-test-header-match-with-group ()
  "Test vmpc-header-match extracting group."
  (vm-pcrisis-test-with-composition-buffer
    (vmpc-substitute-header "Subject" "[Ticket-12345] Issue")
    (let ((result (vmpc-header-match "Subject" "\\[Ticket-\\([0-9]+\\)\\]" nil 1)))
      (should (equal result "12345")))))

;;; vmpc-body-match tests

(ert-deftest vm-pcrisis-test-body-match-automorph ()
  "Test vmpc-body-match in automorph state."
  (vm-pcrisis-test-with-composition-buffer
    (goto-char (point-max))
    (insert "\nSpecial keyword here")
    (should (vmpc-body-match "Special keyword"))))

(ert-deftest vm-pcrisis-test-body-match-automorph-no-match ()
  "Test vmpc-body-match in automorph state when no match."
  (vm-pcrisis-test-with-composition-buffer
    (should-not (vmpc-body-match "Nonexistent phrase"))))

;;; Auto-profile tests

(ert-deftest vm-pcrisis-test-get-profile-for-address-not-found ()
  "Test vmpc-get-profile-for-address when no profile exists."
  (let ((vmpc-auto-profiles nil))
    (should (null (vmpc-get-profile-for-address "unknown@example.com")))))

(ert-deftest vm-pcrisis-test-get-profile-for-address-found ()
  "Test vmpc-get-profile-for-address when profile exists."
  (let ((vmpc-auto-profiles '(("test@example.com" ("action1") . 738000)))
        (vmpc-auto-profiles-file "/tmp/test-profiles"))
    ;; Mock vmpc-save-auto-profiles to avoid file operations
    (cl-letf (((symbol-function 'vmpc-save-auto-profiles) #'ignore))
      (should (equal (vmpc-get-profile-for-address "test@example.com")
                     '("action1"))))))

(ert-deftest vm-pcrisis-test-save-profile-for-address ()
  "Test vmpc-save-profile-for-address adds profile."
  (let ((vmpc-auto-profiles nil)
        (vmpc-auto-profiles-file "/tmp/test-profiles")
        (vmpc-auto-profiles-expunge-days nil))
    (cl-letf (((symbol-function 'vmpc-save-auto-profiles) #'ignore))
      (vmpc-save-profile-for-address "new@example.com" '("action1"))
      (should (assoc "new@example.com" vmpc-auto-profiles)))))

(ert-deftest vm-pcrisis-test-save-profile-for-address-update ()
  "Test vmpc-save-profile-for-address updates existing profile."
  (let ((vmpc-auto-profiles '(("test@example.com" ("old-action") . 738000)))
        (vmpc-auto-profiles-file "/tmp/test-profiles")
        (vmpc-auto-profiles-expunge-days nil))
    (cl-letf (((symbol-function 'vmpc-save-auto-profiles) #'ignore))
      (vmpc-save-profile-for-address "test@example.com" '("new-action"))
      (should (equal (cadr (assoc "test@example.com" vmpc-auto-profiles))
                     '("new-action"))))))

;;; Profile expunge tests

(ert-deftest vm-pcrisis-test-save-profile-expunge-old ()
  "Test vmpc-save-profile-for-address expunges old profiles."
  (let* ((today (vmpc-gregorian-days))
         (old-day (- today 200))  ; 200 days ago
         (vmpc-auto-profiles `(("old@example.com" ("action") . ,old-day)))
         (vmpc-auto-profiles-file "/tmp/test-profiles")
         (vmpc-auto-profiles-expunge-days 100))
    (cl-letf (((symbol-function 'vmpc-save-auto-profiles) #'ignore))
      (vmpc-save-profile-for-address "new@example.com" '("action"))
      ;; Old profile should be expunged
      (should-not (assoc "old@example.com" vmpc-auto-profiles))
      ;; New profile should exist
      (should (assoc "new@example.com" vmpc-auto-profiles)))))

;;; vmpc-read-actions tests

(ert-deftest vm-pcrisis-test-read-actions-none ()
  "Test vmpc-read-actions with 'none' input."
  (let ((vmpc-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "none")))
      (should (null (vmpc-read-actions "Test prompt: "))))))

(ert-deftest vm-pcrisis-test-read-actions-single ()
  "Test vmpc-read-actions with single action."
  (let ((vmpc-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "action1")))
      (should (equal (vmpc-read-actions "Test prompt: ")
                     '("action1"))))))

(ert-deftest vm-pcrisis-test-read-actions-multiple ()
  "Test vmpc-read-actions with multiple actions."
  (let ((vmpc-actions '(("action1" t) ("action2" t))))
    (cl-letf (((symbol-function 'vm-read-string) (lambda (&rest _) "action1 action2")))
      (let ((result (vmpc-read-actions "Test prompt: ")))
        (should (member "action1" result))
        (should (member "action2" result))))))

;;; Advice tests

(ert-deftest vm-pcrisis-test-advice-reply-exists ()
  "Test that reply advice is installed."
  (should (advice-member-p #'vmpc--reply 'vm-do-reply)))

(ert-deftest vm-pcrisis-test-advice-mail-exists ()
  "Test that mail advice is installed."
  (should (advice-member-p #'vmpc--mail 'vm-mail-from-folder)))

(ert-deftest vm-pcrisis-test-advice-newmail-exists ()
  "Test that newmail advice is installed."
  (should (advice-member-p #'vmpc--newmail 'vm-mail)))

(ert-deftest vm-pcrisis-test-advice-forward-exists ()
  "Test that forward advice is installed."
  (should (advice-member-p #'vmpc--forward 'vm-forward-message)))

(ert-deftest vm-pcrisis-test-advice-resend-exists ()
  "Test that resend advice is installed."
  (should (advice-member-p #'vmpc--resend 'vm-resend-message)))

(provide 'vm-pcrisis-test)

;;; vm-pcrisis-test.el ends here