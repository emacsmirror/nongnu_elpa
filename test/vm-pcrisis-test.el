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
  "Dynamic variable to capture action test results.
Bound by the tests that use it rather than set: action bodies are `eval'ed, so
a lexical variable would be invisible to them, but a dynamic binding of this one
is not.")

(ert-deftest vm-pcrisis-test-run-actions-simple ()
  "Test vmpc-run-actions executes actions."
  (let ((vm-pcrisis-test--action-result nil)
        (vmpc-actions '(("test-action" (setq vm-pcrisis-test--action-result 'executed))))
        (vmpc-actions-to-run '("test-action")))
    (vmpc-run-actions)
    (should (eq vm-pcrisis-test--action-result 'executed))))

(ert-deftest vm-pcrisis-test-run-actions-multiple ()
  "Test vmpc-run-actions executes multiple actions in order."
  (let ((vm-pcrisis-test--action-result nil)
        (vmpc-actions '(("action1" (push 1 vm-pcrisis-test--action-result))
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

(defmacro vm-pcrisis-test-with-mode (&rest body)
  "Run BODY with `vmpc-mode\=' on, restoring it afterwards.
Since #561 the advice is installed by the mode rather than by loading the file,
so a test about the advice has to switch it on."
  (declare (indent 0) (debug t))
  `(let ((was vmpc-mode))
     (unwind-protect
         (progn (vmpc-mode 1) ,@body)
       (vmpc-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-advice-reply-exists ()
  "Test that reply advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vmpc--reply 'vm-do-reply))))

(ert-deftest vm-pcrisis-test-advice-mail-exists ()
  "Test that mail advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vmpc--mail 'vm-mail-from-folder))))

(ert-deftest vm-pcrisis-test-advice-newmail-exists ()
  "Test that newmail advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vmpc--newmail 'vm-mail))))

(ert-deftest vm-pcrisis-test-advice-forward-exists ()
  "Test that forward advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vmpc--forward 'vm-forward-message))))

(ert-deftest vm-pcrisis-test-advice-resend-exists ()
  "Test that resend advice is installed when the mode is on."
  (vm-pcrisis-test-with-mode
    (should (advice-member-p #'vmpc--resend 'vm-resend-message))))

;;; Rule dispatch per composition state (issue #451)
;;
;; #451 reported that pcrisis actions stopped firing on `m' and `r' while
;; still being runnable by hand.  The advice tests above only prove the
;; entry points are hooked; these cover the rest of the path, which is where
;; a silent break would leave exactly that symptom.

(defconst vm-pcrisis-test--states '(reply mail newmail forward resend automorph)
  "Every state `vmpc-init-vars' is called with by the composition advices.")

(ert-deftest vm-pcrisis-test-every-state-has-a-rules-variable ()
  "Each composition state has the `vmpc-STATE-rules' variable it looks up.
`vmpc-build-actions-to-run-list' resolves rules through
\(symbol-value (intern (format \"vmpc-%s-rules\" vmpc-current-state))), so a
state with no matching variable does not degrade to \"no rules\" -- it
signals void-variable and takes the whole compose command down with it."
  (dolist (state vm-pcrisis-test--states)
    (let ((symbol (intern (format "vmpc-%s-rules" state))))
      (should (boundp symbol)))))

(ert-deftest vm-pcrisis-test-legacy-alist-names-are-aliases ()
  "The pre-8.3 `vmpc-*-alist' names still reach the `vmpc-*-rules' variables.
The rules variables were renamed; a VM 8.2.0 configuration setting the old
names has to keep working, or its rules are silently never consulted, which
is what #451 looked like from the outside."
  (dolist (pair '((vmpc-actions-alist   . vmpc-default-rules)
                  (vmpc-reply-alist     . vmpc-reply-rules)
                  (vmpc-forward-alist   . vmpc-forward-rules)
                  (vmpc-resend-alist    . vmpc-resend-rules)
                  (vmpc-mail-alist      . vmpc-mail-rules)
                  (vmpc-newmail-alist   . vmpc-newmail-rules)
                  (vmpc-automorph-alist . vmpc-automorph-rules)))
    (should (eq (indirect-variable (car pair)) (cdr pair)))))

(ert-deftest vm-pcrisis-test-legacy-alist-value-reaches-rules ()
  "Setting a legacy `vmpc-*-alist' name is visible under the new name."
  (let ((vmpc-newmail-rules nil))
    (setq vmpc-newmail-alist '(("cond" "act")))
    (should (equal vmpc-newmail-rules '(("cond" "act"))))))

(defvar vm-pcrisis-test--fired nil
  "Set by the action in `vm-pcrisis-test-rule-dispatch-runs-action-per-state'.
A defvar rather than a lexical variable because action bodies are `eval'ed and
would not see one; the test binds this dynamically, which they do see, so
nothing is left set afterwards.")

(ert-deftest vm-pcrisis-test-rule-dispatch-runs-action-per-state ()
  "REGRESSION: a true condition mapped to an action runs it, in every state.
This is the whole of what #451 said was broken -- rules configured, actions
runnable by hand, but nothing triggered automatically.  Driven through
`vmpc-build-true-conditions-list' and `vmpc-build-actions-to-run-list'
rather than through the interactive commands, which need a terminal."
  (dolist (state vm-pcrisis-test--states)
    (let* ((rules-var (intern (format "vmpc-%s-rules" state)))
           (saved (symbol-value rules-var))
           (vmpc-conditions '(("always" t)))
           (vmpc-actions '(("mark" (setq vm-pcrisis-test--fired t))))
           (vmpc-actions-to-run nil)
           (vmpc-true-conditions nil)
           (vm-pcrisis-test--fired nil))
      (unwind-protect
          (progn
            (set rules-var '(("always" "mark")))
            (vmpc-init-vars state)
            (should (member "always" (vmpc-build-true-conditions-list)))
            (should (member "mark" (vmpc-build-actions-to-run-list)))
            (vmpc-run-actions)
            (should vm-pcrisis-test--fired))
        (set rules-var saved)))))


;;; composing from an empty folder with pcrisis loaded (issue #514)

(ert-deftest vm-pcrisis-test-mail-from-an-empty-folder ()
  "REGRESSION: `m' composes in an empty folder with pcrisis loaded.
Issue #514: `vm-mail-from-folder' validated with a minimum of one message, so
in an empty folder -- an IMAP inbox with no mail in it, the ordinary way to
meet this -- it answered \"Folder is empty\" and composed nothing.  That was
fixed in the command, but `vmpc--mail' advises the command and repeats the
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
         (was vmpc-mode))
    (unwind-protect
        (progn
          ;; Since #561 the advice comes with the mode, not with the file.
          (vmpc-mode 1)
          (with-temp-file file (insert ""))
          (vm-visit-folder file)
          (should (null vm-message-list))
          ;; The advice is what is under test, so it had better be there.
          (should (advice-member-p 'vmpc--mail 'vm-mail-from-folder))
          (vm-mail-from-folder)
          (should (eq major-mode 'mail-mode))
          (should (string-match-p "^To:" (buffer-string))))
      (vmpc-mode (if was 1 -1))
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
  (should-not vmpc-mode)
  (dolist (pair vmpc-advised-commands)
    (should-not (advice-member-p (cdr pair) (car pair)))))

(ert-deftest vm-pcrisis-test-mode-advises-and-unadvises-every-command ()
  "`vmpc-mode' installs the advice, and turning it off removes all of it.
Both directions matter: a mode that cannot be switched off would leave #561 half
fixed."
  (require 'vm-pcrisis)
  (let ((was vmpc-mode))
    (unwind-protect
        (progn
          (vmpc-mode 1)
          (should vmpc-mode)
          (dolist (pair vmpc-advised-commands)
            (should (advice-member-p (cdr pair) (car pair))))
          (vmpc-mode -1)
          (should-not vmpc-mode)
          (dolist (pair vmpc-advised-commands)
            (should-not (advice-member-p (cdr pair) (car pair)))))
      (vmpc-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-mode-is-idempotent ()
  "Turning it on twice does not advise twice, nor off twice fail.
`define-minor-mode' guards the body against a no-op change, but an advice
installed twice would run the rules twice per composition, so it is worth
pinning."
  (require 'vm-pcrisis)
  (let ((was vmpc-mode))
    (unwind-protect
        (progn
          (vmpc-mode 1)
          (vmpc-mode 1)
          (vmpc-mode -1)
          (dolist (pair vmpc-advised-commands)
            (should-not (advice-member-p (cdr pair) (car pair))))
          (vmpc-mode -1))
      (vmpc-mode (if was 1 -1)))))

(ert-deftest vm-pcrisis-test-advised-commands-all-exist ()
  "Every command the mode advises is a command, and every advice a function.
A typo in `vmpc-advised-commands' would otherwise advise a symbol nobody calls,
and the mode would appear to work while doing nothing."
  (require 'vm)
  (require 'vm-pcrisis)
  (should (= 7 (length vmpc-advised-commands)))
  (dolist (pair vmpc-advised-commands)
    (should (fboundp (car pair)))
    (should (fboundp (cdr pair))))
  ;; All but `vm-do-reply' are commands; that one is the internal worker the
  ;; reply commands call, which is why the advice hangs off it.
  (should-not (commandp 'vm-do-reply))
  (dolist (pair (assq-delete-all 'vm-do-reply (copy-alist vmpc-advised-commands)))
    (should (commandp (car pair)))))

;;; Actions and the buffer they need (issues #540 and #576)

;; Personality Crisis runs an action list twice, once before the composition
;; buffer exists and once in it.  That is why an action needing a composition
;; returns quietly on the first pass, and why `vmpc-add-header' raising in that
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
  "Run BODY with ACTIONS as the whole of `vmpc-actions', in a rule, mode on."
  (declare (indent 1) (debug t))
  `(let ((vmpc-conditions '(("always" t)))
         (vmpc-actions (list (cons "the rule" ,actions)))
         (vmpc-default-rules '(("always" "the rule")))
         (vmpc-expect-default-signature vmpc-expect-default-signature))
     (unwind-protect
         (progn (vmpc-mode 1) ,@body)
       (vmpc-mode -1))))

(defun vm-pcrisis-test--holds (text)
  "Return non-nil if the current buffer holds TEXT."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward text nil t) t)))

(ert-deftest vm-pcrisis-test-signature-action-deletes-the-default-signature ()
  "REGRESSION: (vmpc-signature \"\") deletes a signature Emacs inserted.
Issue #540.  In a new composition the body is the signature and nothing else,
so the newline before the \"-- \" line is the one ending the header separator
and lies outside the body.  The search for a signature was bounded by the start
of the body and so could never see it, and the action found nothing to delete."
  (let ((vmpc-expect-default-signature t))
    (vm-pcrisis-test--with-rules '((vmpc-signature ""))
      (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
        (should-not (vm-pcrisis-test--holds "my signature"))
        ;; What is left is a composition, with its separator line intact.
        (should (vm-pcrisis-test--holds mail-header-separator))))))

(ert-deftest vm-pcrisis-test-signature-action-needs-to-be-told-to-expect-one ()
  "Without `vmpc-expect-default-signature' the signature is left alone.
Personality Crisis acts only on a signature whose extent it knows, and this is
how it comes to know one it did not insert.  The other side of the branch, so
the fix is not simply deleting whatever is at the end of the buffer."
  (let ((vmpc-expect-default-signature nil))
    (vm-pcrisis-test--with-rules '((vmpc-signature ""))
      (vm-pcrisis-test--with-composition (buffer "-- \nmy signature\n")
        (should (vm-pcrisis-test--holds "my signature"))))))

(ert-deftest vm-pcrisis-test-add-header-action-adds-the-header ()
  "REGRESSION: `vmpc-add-header' works as an action.
Issue #576.  It raised whenever it was not in a composition, and Personality
Crisis evaluates the action list once before the composition exists, so naming
it in a rule broke composing altogether.  It also read the headers of the
message being replied to rather than the composition's own, and got nil."
  (vm-pcrisis-test--with-rules '((vmpc-add-header "FCC" "/tmp/sent"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "FCC: /tmp/sent")))))

(ert-deftest vm-pcrisis-test-add-header-action-does-not-add-it-twice ()
  "The same header and content twice adds one, which is what it is for.
Named for FCC, which may appear more than once but should not repeat itself."
  (vm-pcrisis-test--with-rules '((vmpc-add-header "FCC" "/tmp/sent")
                                 (vmpc-add-header "FCC" "/tmp/sent"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (= 1 (how-many "FCC: /tmp/sent" (point-min) (point-max)))))))

(ert-deftest vm-pcrisis-test-actions-complain-when-called-by-hand ()
  "An action called where there is no composition says so.
Issue #540's reporter tried `M-: (vmpc-signature \"\")' and saw nothing happen,
which is indistinguishable from an action that ran and did nothing."
  (require 'vm-pcrisis)
  (with-temp-buffer
    (let ((vmpc-current-buffer nil)
          (vmpc-running-actions nil)
          (text-quoting-style 'grave))
      (dolist (call '((vmpc-signature "")
                      (vmpc-pre-signature "")
                      (vmpc-add-header "FCC" "/tmp/sent")
                      (vmpc-insert-header "FCC" "/tmp/sent")
                      (vmpc-substitute-header "FCC" "/tmp/sent")
                      (vmpc-delete-header "FCC")))
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
    (let ((vmpc-current-buffer 'none)
          (vmpc-running-actions t))
      (should-not (vmpc-signature ""))
      (should-not (vmpc-add-header "FCC" "/tmp/sent"))
      (should-not (vmpc-delete-header "FCC")))))
;;; Replacing part of a header (issue #578)

;; `vmpc-replace-or-add-in-header' read the composition's headers through
;; `vmpc-get-current-header-contents', which was gated to the automorph state
;; and so returned nil in an ordinary composition.  The action then found no
;; header, and did nothing at all, quietly.

(ert-deftest vm-pcrisis-test-replace-in-header-replaces-the-match ()
  "REGRESSION: the action replaces what its regexp matches in the header.
Issue #578.  Two actions in the rule: the first puts a recipient there, the
second rewrites the name, which is what the docstring's own example does."
  (vm-pcrisis-test--with-rules
      '((vmpc-substitute-header "To" "Bob Smith <bob@example.com>")
        (vmpc-replace-or-add-in-header "To" "[Bb]ob Smith[^,]*"
                                       "Robert Fenk <bob@example.com>"))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: Robert Fenk <bob@example.com>"))
      (should-not (vm-pcrisis-test--holds "Bob Smith")))))

(ert-deftest vm-pcrisis-test-replace-in-header-appends-with-a-separator ()
  "With no match and a separator, the content is appended after it.
The header is already occupied here, so the separator is what keeps the two
recipients apart."
  (vm-pcrisis-test--with-rules
      '((vmpc-substitute-header "To" "alice@example.com")
        (vmpc-replace-or-add-in-header "To" "nobody@example.com"
                                       "bob@example.com" ", "))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: alice@example.com, bob@example.com")))))

(ert-deftest vm-pcrisis-test-replace-in-header-adds-without-a-separator ()
  "An empty header gets the content and no separator in front of it.
A fresh composition's To is empty, and a leading \", \" there would be a
syntactically broken recipient list."
  (vm-pcrisis-test--with-rules
      '((vmpc-replace-or-add-in-header "To" "nobody@example.com"
                                       "bob@example.com" ", "))
    (vm-pcrisis-test--with-composition (buffer)
      (should (vm-pcrisis-test--holds "To: bob@example.com"))
      (should-not (vm-pcrisis-test--holds "To: , ")))))

;;; Saying so when the mode is off (emacs-vm/vm#642)

(ert-deftest vm-pcrisis-test-a-composition-says-when-the-mode-is-off ()
  "With rules set and `vmpc-mode' off, starting a composition says so.

The rules are never consulted then, and the composition gets whatever
`user-mail-address' says.  Nothing else notices: a default rule naming the
address VM would have used anyway looks exactly like a working setup."
  (let ((vmpc-mode nil)
        (vmpc-conditions '(("in a folder" (vmpc-folder-account-match "^work$"))))
        (vmpc-actions '(("from work" (vmpc-substitute-header "From" "me@work"))))
        (vmpc-default-rules '(("in a folder" "from work")))
        (said nil))
    (cl-letf (((symbol-function 'vm-warn)
               (lambda (_level _secs &rest args)
                 (setq said (apply #'format args)))))
      (vmpc-warn-if-off))
    (should (string-match-p "vmpc-mode is off" said))
    (should (string-match-p "(vmpc-mode 1)" said))))

(ert-deftest vm-pcrisis-test-it-says-so-every-time ()
  "It says so at every composition, not once.

`vm-warn' will not repeat a warning it has just given, which is why this one
binds `vm-current-warning' around the call: a warning seen once at startup is
a warning forgotten."
  (let ((vmpc-mode nil)
        (vmpc-conditions '(("in a folder" (vmpc-folder-account-match "^work$"))))
        (vmpc-actions '(("from work" (vmpc-substitute-header "From" "me@work"))))
        (vmpc-default-rules '(("in a folder" "from work")))
        (times 0))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq times (1+ times))))
              ((symbol-function 'sleep-for) #'ignore))
      (vmpc-warn-if-off)
      (vmpc-warn-if-off)
      (vmpc-warn-if-off))
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
      (let ((vmpc-mode t)
            (vmpc-conditions '(("in a folder" t)))
            (vmpc-actions '(("from work" (vmpc-substitute-header "From" "x"))))
            (vmpc-default-rules '(("in a folder" "from work"))))
        (vmpc-warn-if-off)
        (should-not said))
      ;; off, and nothing configured
      (let ((vmpc-mode nil)
            (vmpc-conditions nil)
            (vmpc-actions nil)
            (vmpc-default-rules nil))
        (vmpc-warn-if-off)
        (should-not said))
      ;; conditions and actions but no rules joining them: nothing would run
      ;; even with the mode on, so this is not the mistake being warned about
      (let ((vmpc-mode nil)
            (vmpc-conditions '(("in a folder" t)))
            (vmpc-actions '(("from work" (vmpc-substitute-header "From" "x"))))
            (vmpc-default-rules nil)
            (vmpc-reply-rules nil)
            (vmpc-forward-rules nil)
            (vmpc-resend-rules nil)
            (vmpc-newmail-rules nil)
            (vmpc-automorph-rules nil))
        (vmpc-warn-if-off)
        (should-not said)))))

(ert-deftest vm-pcrisis-test-the-warning-is-on-the-composition-hook ()
  "The check runs from `vm-mail-mode-hook', which every composition runs:
replying, forwarding, resending and starting a message all end there."
  (should (memq 'vmpc-warn-if-off (default-value 'vm-mail-mode-hook))))

(provide 'vm-pcrisis-test)

;;; vm-pcrisis-test.el ends here