;;; vm-avirtual-test.el --- Tests for vm-avirtual.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM additional virtual folder selectors in vm-avirtual.el

;;; Code:

(require 'vm-test-init)
(require 'vm-avirtual)

;;; vm-mail-vs-any tests

(ert-deftest vm-avirtual-test-mail-vs-any-returns-t ()
  "Test that vm-mail-vs-any always returns t."
  (should (eq t (vm-mail-vs-any))))

;;; vm-mail-vs-unknown tests

(ert-deftest vm-avirtual-test-mail-vs-unknown-returns-nil ()
  "Test that vm-mail-vs-unknown returns nil."
  (should (null (vm-mail-vs-unknown)))
  (should (null (vm-mail-vs-unknown "any-arg")))
  (should (null (vm-mail-vs-unknown '(complex arg)))))

;;; vm-mail-vs-header tests

(ert-deftest vm-avirtual-test-mail-vs-header-finds-match ()
  "Test vm-mail-vs-header finds matching header."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert "Subject: Test Subject\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (vm-mail-vs-header "Subject:.*Test"))))

(ert-deftest vm-avirtual-test-mail-vs-header-no-match ()
  "Test vm-mail-vs-header returns nil when no match."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (null (vm-mail-vs-header "X-Missing:")))))

(ert-deftest vm-avirtual-test-mail-vs-header-not-in-body ()
  "Test vm-mail-vs-header doesn't match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Subject: in body\n")
    (should (null (vm-mail-vs-header "Subject: in body")))))

;;; vm-mail-vs-text tests

(ert-deftest vm-avirtual-test-mail-vs-text-finds-match ()
  "Test vm-mail-vs-text finds matching text in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body contains special word\n")
    (should (vm-mail-vs-text "special word"))))

(ert-deftest vm-avirtual-test-mail-vs-text-no-match ()
  "Test vm-mail-vs-text returns nil when no match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body text\n")
    (should (null (vm-mail-vs-text "nonexistent")))))

;;; vm-mail-vs-header-or-text tests

(ert-deftest vm-avirtual-test-mail-vs-header-or-text-finds-header ()
  "Test vm-mail-vs-header-or-text finds match in header."
  (with-temp-buffer
    (insert "From: unique@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "Body\n")
    (should (vm-mail-vs-header-or-text "unique@example"))))

(ert-deftest vm-avirtual-test-mail-vs-header-or-text-finds-body ()
  "Test vm-mail-vs-header-or-text finds match in body."
  (with-temp-buffer
    (insert "From: test@example.com\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "unique-body-text\n")
    (should (vm-mail-vs-header-or-text "unique-body-text"))))

;;; vm-mail-vs-more-chars-than tests

(ert-deftest vm-avirtual-test-mail-vs-more-chars-than-true ()
  "Test vm-mail-vs-more-chars-than returns t when buffer is larger."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert (make-string 100 ?a))
    (should (vm-mail-vs-more-chars-than 50))))

(ert-deftest vm-avirtual-test-mail-vs-more-chars-than-false ()
  "Test vm-mail-vs-more-chars-than returns nil when buffer is smaller."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short")
    (should (null (vm-mail-vs-more-chars-than 1000)))))

;;; vm-mail-vs-less-chars-than tests

(ert-deftest vm-avirtual-test-mail-vs-less-chars-than-true ()
  "Test vm-mail-vs-less-chars-than returns t when buffer is smaller."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short")
    (should (vm-mail-vs-less-chars-than 1000))))

(ert-deftest vm-avirtual-test-mail-vs-less-chars-than-false ()
  "Test vm-mail-vs-less-chars-than returns nil when buffer is larger."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert (make-string 100 ?a))
    (should (null (vm-mail-vs-less-chars-than 50)))))

;;; vm-mail-vs-more-lines-than tests

(ert-deftest vm-avirtual-test-mail-vs-more-lines-than-true ()
  "Test vm-mail-vs-more-lines-than returns t when buffer has more lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (dotimes (_ 10) (insert "line\n"))
    (should (vm-mail-vs-more-lines-than 5))))

(ert-deftest vm-avirtual-test-mail-vs-more-lines-than-false ()
  "Test vm-mail-vs-more-lines-than returns nil when buffer has fewer lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "one line\n")
    (should (null (vm-mail-vs-more-lines-than 100)))))

;;; vm-mail-vs-less-lines-than tests

(ert-deftest vm-avirtual-test-mail-vs-less-lines-than-true ()
  "Test vm-mail-vs-less-lines-than returns t when buffer has fewer lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (insert "short\n")
    (should (vm-mail-vs-less-lines-than 100))))

(ert-deftest vm-avirtual-test-mail-vs-less-lines-than-false ()
  "Test vm-mail-vs-less-lines-than returns nil when buffer has more lines."
  (with-temp-buffer
    (insert "From: x\n")
    (insert mail-header-separator)
    (insert "\n")
    (dotimes (_ 20) (insert "line\n"))
    (should (null (vm-mail-vs-less-lines-than 5)))))

;;; vm-mail-vs-eval tests

(ert-deftest vm-avirtual-test-mail-vs-eval ()
  "Test vm-mail-vs-eval evaluates expression."
  (should (= 6 (vm-mail-vs-eval nil '(+ 1 2 3))))
  (should (string= "hello" (vm-mail-vs-eval nil '"hello"))))

;;; Selector combinator tests

(ert-deftest vm-avirtual-test-mail-vs-and-all-true ()
  "Test vm-mail-vs-and returns t when all selectors match."
  (should (vm-mail-vs-and '(any) '(any))))

(ert-deftest vm-avirtual-test-mail-vs-and-one-false ()
  "Test vm-mail-vs-and returns nil when any selector fails."
  (should (null (vm-mail-vs-and '(any) '(new)))))  ; new returns nil in mail mode

(ert-deftest vm-avirtual-test-mail-vs-or-one-true ()
  "Test vm-mail-vs-or returns t when any selector matches."
  (should (vm-mail-vs-or '(new) '(any))))  ; any always returns t

(ert-deftest vm-avirtual-test-mail-vs-or-all-false ()
  "Test vm-mail-vs-or returns nil when all selectors fail."
  (should (null (vm-mail-vs-or '(new) '(deleted)))))  ; both return nil in mail mode

(ert-deftest vm-avirtual-test-mail-vs-not ()
  "Test vm-mail-vs-not inverts result."
  (should (vm-mail-vs-not '(new)))  ; new returns nil, so not-nil = t
  (should (null (vm-mail-vs-not '(any)))))  ; any returns t, so not-t = nil

;;; vm-virtual-check-case-fold-search tests

(ert-deftest vm-avirtual-test-case-fold-search-default ()
  "Test that vm-virtual-check-case-fold-search defaults to t."
  (should (eq t vm-virtual-check-case-fold-search)))

;;; Mail selector alist tests

(ert-deftest vm-avirtual-test-mail-selector-alist-complete ()
  "Test that mail selector alist has required entries."
  (dolist (sel '(and or not any header text recipient author subject
                 new unread read deleted filed written edited marked
                 undeleted unfiled unwritten unedited unmarked))
    (should (assq sel vm-mail-virtual-selector-function-alist))))

;;; Interactive command tests

(ert-deftest vm-avirtual-test-commands-interactive ()
  "Test that commands are interactive."
  (should (commandp 'vm-add-spam-word))
  (should (commandp 'vm-spam-words-rebuild))
  (should (commandp 'vm-virtual-auto-delete-message))
  (should (commandp 'vm-virtual-save-message)))

;;; Customization group tests

(ert-deftest vm-avirtual-test-customization-group ()
  "Test that vm-avirtual customization group is defined."
  (should (get 'vm-avirtual 'custom-group)))

(provide 'vm-avirtual-test)

;;; vm-avirtual-test.el ends here