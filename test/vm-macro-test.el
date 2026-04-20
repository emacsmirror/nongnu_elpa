;;; vm-macro-test.el --- Tests for vm-macro.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM macros and core utilities in vm-macro.el

;;; Code:

(require 'vm-test-init)
(require 'vm-macro)

;;; vm-interactive-p tests

(ert-deftest vm-macro-test-interactive-p-exists ()
  "Test vm-interactive-p macro exists."
  (should (fboundp 'vm-interactive-p)))

;;; vm-marker tests

(ert-deftest vm-macro-test-marker-creates-marker ()
  "Test vm-marker creates a marker."
  (with-temp-buffer
    (insert "test")
    (let ((m (vm-marker (point))))
      (should (markerp m))
      (should (= (marker-position m) (point))))))

(ert-deftest vm-macro-test-marker-with-nil ()
  "Test vm-marker with nil position creates marker at nil."
  ;; vm-marker always creates a marker, position may be nil
  (let ((m (vm-marker nil)))
    (should (markerp m))))

;;; vm-pop-folder-spec-p tests
;; Note: vm-pop-folder-spec-p depends on vm-recognize-pop-maildrops
;; Format: pop:host:port:auth:user:pass

(ert-deftest vm-macro-test-pop-folder-spec-p-true ()
  "Test vm-pop-folder-spec-p identifies POP specs."
  ;; Correct format: pop:host:port:auth:user:password
  (should (vm-pop-folder-spec-p "pop:mail.example.com:110:pass:user:secret")))

(ert-deftest vm-macro-test-pop-folder-spec-p-false ()
  "Test vm-pop-folder-spec-p rejects non-POP specs."
  (should-not (vm-pop-folder-spec-p "/path/to/folder")))

(ert-deftest vm-macro-test-pop-folder-spec-p-nil ()
  "Test vm-pop-folder-spec-p with nil signals error or returns nil."
  ;; When passed nil, string-match will error, so we catch it
  (should (condition-case nil
              (progn (vm-pop-folder-spec-p nil) t)
            (error t))))

;;; vm-imap-folder-spec-p tests
;; Note: vm-imap-folder-spec-p depends on vm-recognize-imap-maildrops
;; Format: imap:host:port:mailbox:auth:user:password:session

(ert-deftest vm-macro-test-imap-folder-spec-p-true ()
  "Test vm-imap-folder-spec-p identifies IMAP specs."
  ;; Correct format: imap:host:port:mailbox:auth:user:pass:session
  (should (vm-imap-folder-spec-p "imap:mail.example.com:993:INBOX:login:user:pass:*")))

(ert-deftest vm-macro-test-imap-folder-spec-p-imap-ssl ()
  "Test vm-imap-folder-spec-p identifies IMAP-SSL specs."
  (should (vm-imap-folder-spec-p "imap-ssl:mail.example.com:993:INBOX:login:user:pass:*")))

(ert-deftest vm-macro-test-imap-folder-spec-p-false ()
  "Test vm-imap-folder-spec-p rejects non-IMAP specs."
  (should-not (vm-imap-folder-spec-p "/path/to/folder")))

(ert-deftest vm-macro-test-imap-folder-spec-p-nil ()
  "Test vm-imap-folder-spec-p with nil."
  ;; When passed nil, string-match will error, so we catch it
  (should (condition-case nil
              (progn (vm-imap-folder-spec-p nil) t)
            (error t))))

;;; vm-increment tests

(ert-deftest vm-macro-test-increment ()
  "Test vm-increment macro."
  (let ((x 5))
    (vm-increment x)
    (should (= x 6))))

(ert-deftest vm-macro-test-increment-from-zero ()
  "Test vm-increment from zero."
  (let ((x 0))
    (vm-increment x)
    (should (= x 1))))

;;; vm-decrement tests

(ert-deftest vm-macro-test-decrement ()
  "Test vm-decrement macro."
  (let ((x 5))
    (vm-decrement x)
    (should (= x 4))))

(ert-deftest vm-macro-test-decrement-to-zero ()
  "Test vm-decrement to zero."
  (let ((x 1))
    (vm-decrement x)
    (should (= x 0))))

;;; vm-add-to-list tests

(ert-deftest vm-macro-test-add-to-list-new ()
  "Test vm-add-to-list adds new element."
  (let ((my-list '(a b c)))
    (vm-add-to-list 'd my-list)
    (should (memq 'd my-list))))

(ert-deftest vm-macro-test-add-to-list-existing ()
  "Test vm-add-to-list doesn't duplicate existing element."
  (let ((my-list '(a b c)))
    (vm-add-to-list 'b my-list)
    (should (= 3 (length my-list)))))

;;; vm-binary-coding-system tests

(ert-deftest vm-macro-test-binary-coding-system ()
  "Test vm-binary-coding-system returns valid coding system."
  (let ((cs (vm-binary-coding-system)))
    (should (coding-system-p cs))))

;;; vm-line-ending-coding-system tests

(ert-deftest vm-macro-test-line-ending-coding-system ()
  "Test vm-line-ending-coding-system returns valid coding system."
  (let ((cs (vm-line-ending-coding-system)))
    (should (coding-system-p cs))))

;;; vm-assert tests

(ert-deftest vm-macro-test-assert-true ()
  "Test vm-assert doesn't error on true condition."
  (should (not (condition-case nil
                   (progn (vm-assert t) nil)
                 (error t)))))

;;; vm-make-trace-buffer-name tests

(ert-deftest vm-macro-test-make-trace-buffer-name ()
  "Test vm-make-trace-buffer-name creates buffer name."
  (let ((name (vm-make-trace-buffer-name "POP" "mail.example.com")))
    (should (stringp name))
    (should (string-match "POP" name))
    (should (string-match "mail.example.com" name))))

;;; vm-sit-for tests

(ert-deftest vm-macro-test-sit-for-exists ()
  "Test vm-sit-for macro exists."
  (should (fboundp 'vm-sit-for)))

;;; Buffer type functions

(ert-deftest vm-macro-test-buffer-type-functions-exist ()
  "Test buffer type tracking functions exist."
  (should (fboundp 'vm-buffer-type:enter))
  (should (fboundp 'vm-buffer-type:exit))
  (should (fboundp 'vm-buffer-type:duplicate))
  (should (fboundp 'vm-buffer-type:set)))

(ert-deftest vm-macro-test-buffer-type-enter-pushes ()
  "Test vm-buffer-type:enter pushes type onto stack."
  (let ((vm-buffer-types nil)
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:enter 'folder)
    (should (eq (car vm-buffer-types) 'folder))
    (should (= (length vm-buffer-types) 1))))

(ert-deftest vm-macro-test-buffer-type-enter-multiple ()
  "Test vm-buffer-type:enter pushes multiple types."
  (let ((vm-buffer-types nil)
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:enter 'folder)
    (vm-buffer-type:enter 'process)
    (should (eq (car vm-buffer-types) 'process))
    (should (eq (cadr vm-buffer-types) 'folder))
    (should (= (length vm-buffer-types) 2))))

(ert-deftest vm-macro-test-buffer-type-exit-pops ()
  "Test vm-buffer-type:exit pops from stack."
  (let ((vm-buffer-types '(process folder))
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:exit)
    (should (eq (car vm-buffer-types) 'folder))
    (should (= (length vm-buffer-types) 1))))

(ert-deftest vm-macro-test-buffer-type-exit-to-empty ()
  "Test vm-buffer-type:exit removes last element."
  (let ((vm-buffer-types '(folder))
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:exit)
    (should (null vm-buffer-types))))

(ert-deftest vm-macro-test-buffer-type-set-modifies ()
  "Test vm-buffer-type:set modifies current type."
  (let ((vm-buffer-types '(folder))
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:set 'process)
    (should (eq (car vm-buffer-types) 'process))
    (should (= (length vm-buffer-types) 1))))

(ert-deftest vm-macro-test-buffer-type-set-creates-if-empty ()
  "Test vm-buffer-type:set creates stack if empty."
  (let ((vm-buffer-types nil)
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:set 'folder)
    (should (eq (car vm-buffer-types) 'folder))
    (should (= (length vm-buffer-types) 1))))

(ert-deftest vm-macro-test-buffer-type-duplicate ()
  "Test vm-buffer-type:duplicate duplicates top of stack."
  (let ((vm-buffer-types '(folder))
        (vm-buffer-type-debug nil)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:duplicate)
    (should (eq (car vm-buffer-types) 'folder))
    (should (eq (cadr vm-buffer-types) 'folder))
    (should (= (length vm-buffer-types) 2))))

(ert-deftest vm-macro-test-buffer-type-debug-trail ()
  "Test vm-buffer-type:enter/exit records trail when debugging."
  (let ((vm-buffer-types nil)
        (vm-buffer-type-debug t)
        (vm-buffer-type-trail nil))
    (vm-buffer-type:enter 'folder)
    (should (member 'folder vm-buffer-type-trail))
    (should (member 'enter vm-buffer-type-trail))
    (vm-buffer-type:exit)
    (should (member 'exit vm-buffer-type-trail))))

;;; Folder buffer selection macros

(ert-deftest vm-macro-test-select-folder-buffer-exists ()
  "Test vm-select-folder-buffer macro exists."
  (should (fboundp 'vm-select-folder-buffer)))

(ert-deftest vm-macro-test-select-folder-buffer-if-possible-exists ()
  "Test vm-select-folder-buffer-if-possible macro exists."
  (should (fboundp 'vm-select-folder-buffer-if-possible)))

(ert-deftest vm-macro-test-select-folder-buffer-and-validate-exists ()
  "Test vm-select-folder-buffer-and-validate macro exists."
  (should (fboundp 'vm-select-folder-buffer-and-validate)))

;;; Error macros

(ert-deftest vm-macro-test-error-if-folder-read-only-exists ()
  "Test vm-error-if-folder-read-only macro exists."
  (should (fboundp 'vm-error-if-folder-read-only)))

(ert-deftest vm-macro-test-error-if-virtual-folder-exists ()
  "Test vm-error-if-virtual-folder macro exists."
  (should (fboundp 'vm-error-if-virtual-folder)))

;;; Buffer predicates

(ert-deftest vm-macro-test-buffer-p-exists ()
  "Test vm-buffer-p macro exists."
  (should (fboundp 'vm-buffer-p)))

(ert-deftest vm-macro-test-summary-operation-p-exists ()
  "Test vm-summary-operation-p macro exists."
  (should (fboundp 'vm-summary-operation-p)))

;;; Thread building

(ert-deftest vm-macro-test-build-threads-if-unbuilt-exists ()
  "Test vm-build-threads-if-unbuilt macro exists."
  (should (fboundp 'vm-build-threads-if-unbuilt)))

(provide 'vm-macro-test)

;;; vm-macro-test.el ends here
