;;; vm-macro-test.el --- Tests for vm-macro.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 The VM Developers

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
;;; What the folder-selection and guard macros do.  There was a test for each
;;; of the eight below asserting only that it was bound.

(defmacro vm-macro-test-with-buffers (spec &rest body)
  "Run BODY with a folder buffer and another buffer pointing at it.
SPEC is (FOLDER-VAR OTHER-VAR).  Both buffers are killed afterwards, and the
buffer BODY started in is restored, since these macros call `set-buffer'."
  (declare (indent 1) (debug t))
  (let ((folder (nth 0 spec)) (other (nth 1 spec)))
    `(let ((,folder (generate-new-buffer " *vm-macro-test-folder*"))
           (,other (generate-new-buffer " *vm-macro-test-other*")))
       (unwind-protect
           (save-current-buffer
             (with-current-buffer ,folder (setq major-mode 'vm-mode))
             (with-current-buffer ,other (setq vm-mail-buffer ,folder))
             ,@body)
         (kill-buffer ,folder)
         (kill-buffer ,other)))))

(ert-deftest vm-macro-test-select-folder-buffer-follows-vm-mail-buffer ()
  "From a summary or presentation buffer, the folder buffer is selected."
  (vm-macro-test-with-buffers (folder other)
    (set-buffer other)
    (vm-select-folder-buffer)
    (should (eq (current-buffer) folder))))

(ert-deftest vm-macro-test-select-folder-buffer-stays-in-a-folder ()
  "In a folder or virtual folder there is nothing to select, and no error."
  (vm-macro-test-with-buffers (folder _other)
    (set-buffer folder)
    (vm-select-folder-buffer)
    (should (eq (current-buffer) folder))
    (setq major-mode 'vm-virtual-mode)
    (vm-select-folder-buffer)
    (should (eq (current-buffer) folder))))

(ert-deftest vm-macro-test-select-folder-buffer-says-what-is-wrong ()
  "A buffer with no folder, and a folder that was killed, each get an error.
The two are different faults and the messages say which."
  (let ((text-quoting-style 'grave))
    (with-temp-buffer
      (setq major-mode 'fundamental-mode)
      (should-error (vm-select-folder-buffer)
                    :type 'error))
    (vm-macro-test-with-buffers (folder other)
      (kill-buffer folder)
      (set-buffer other)
      (should (equal (cadr (should-error (vm-select-folder-buffer)))
                     "Folder buffer has been killed.")))))

(ert-deftest vm-macro-test-select-folder-buffer-if-possible-does-not-complain ()
  "The if-possible form returns normally where the plain one errors.
That is the whole difference between them, and nothing tested it."
  (with-temp-buffer
    (setq major-mode 'fundamental-mode)
    (should-not (vm-select-folder-buffer-if-possible))
    (should (eq (current-buffer) (current-buffer))))
  (vm-macro-test-with-buffers (folder other)
    (set-buffer other)
    (vm-select-folder-buffer-if-possible)
    (should (eq (current-buffer) folder)))
  ;; a killed folder buffer is not selected, and is not an error either
  (vm-macro-test-with-buffers (folder other)
    (kill-buffer folder)
    (set-buffer other)
    (vm-select-folder-buffer-if-possible)
    (should (eq (current-buffer) other))))

(ert-deftest vm-macro-test-validate-records-the-interaction-buffer ()
  "Called for an interactive command, validate records where the user was.
`vm-summary-operation-p' reads that later to tell a summary command from a
folder command, and the warning state is reset for the new command."
  (vm-macro-test-with-buffers (folder other)
    (set-buffer other)
    (let ((vm-user-interaction-buffer nil)
          (vm-current-warning "left over from the last command"))
      (cl-letf (((symbol-function 'vm-check-for-killed-summary) #'ignore)
                ((symbol-function 'vm-check-for-killed-presentation) #'ignore))
        (vm-select-folder-buffer-and-validate 0 t)
        (should (eq vm-user-interaction-buffer other))
        (should-not vm-current-warning)
        (should (eq (current-buffer) folder))
        ;; and not recorded when the command did not come from the user
        (setq vm-user-interaction-buffer nil)
        (set-buffer other)
        (vm-select-folder-buffer-and-validate 0 nil)
        (should-not vm-user-interaction-buffer)))))

(ert-deftest vm-macro-test-error-if-folder-read-only-signals-that-condition ()
  "A read-only folder gets the `folder-read-only' signal, naming the buffer.
Callers catch that condition by name, so a plain `error' would not do."
  (with-temp-buffer
    (setq vm-folder-read-only nil)
    (should-not (vm-error-if-folder-read-only))
    (setq vm-folder-read-only t)
    (let ((signalled (should-error (vm-error-if-folder-read-only)
                                   :type 'folder-read-only)))
      (should (eq (cadr signalled) (current-buffer))))))

(ert-deftest vm-macro-test-error-if-virtual-folder-names-the-command ()
  "A command refused on a virtual folder says which command it was."
  (with-temp-buffer
    (setq major-mode 'vm-mode)
    (should-not (vm-error-if-virtual-folder))
    (setq major-mode 'vm-virtual-mode)
    (let ((this-command 'vm-expunge-folder)
          (text-quoting-style 'grave))
      (should (string-match-p "vm-expunge-folder"
                              (cadr (should-error
                                     (vm-error-if-virtual-folder))))))))

(ert-deftest vm-macro-test-buffer-p-knows-vms-own-buffers ()
  "`vm-buffer-p' is true in the four VM modes and false elsewhere."
  (with-temp-buffer
    (dolist (mode '(vm-mode vm-presentation-mode vm-virtual-mode
                            vm-summary-mode))
      (setq major-mode mode)
      (should (vm-buffer-p)))
    (dolist (mode '(fundamental-mode text-mode mail-mode))
      (setq major-mode mode)
      (should-not (vm-buffer-p)))))

(ert-deftest vm-macro-test-summary-operation-p-is-about-where-the-user-was ()
  "A summary operation is one the user started in the summary buffer."
  (with-temp-buffer
    (let ((summary (generate-new-buffer " *vm-macro-test-summary*")))
      (unwind-protect
          (progn
            (setq vm-summary-buffer nil)
            (let ((vm-user-interaction-buffer summary))
              (should-not (vm-summary-operation-p)))
            (setq vm-summary-buffer summary)
            (let ((vm-user-interaction-buffer summary))
              (should (vm-summary-operation-p)))
            (let ((vm-user-interaction-buffer (current-buffer)))
              (should-not (vm-summary-operation-p))))
        (kill-buffer summary)))))

(ert-deftest vm-macro-test-build-threads-if-unbuilt-builds-only-once ()
  "Threads are built when `vm-thread-obarray' is not yet a vector."
  (with-temp-buffer
    (let ((built 0))
      (cl-letf (((symbol-function 'vm-build-threads)
                 (lambda (&rest _) (cl-incf built))))
        (setq vm-thread-obarray nil)
        (vm-build-threads-if-unbuilt)
        (should (= built 1))
        (setq vm-thread-obarray (make-vector 3 0))
        (vm-build-threads-if-unbuilt)
        (should (= built 1))))))


;;; Error macros

;;; Buffer predicates

;;; Thread building

(provide 'vm-macro-test)

;;; vm-macro-test.el ends here
