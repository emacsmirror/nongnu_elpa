;;; vm-test-init-test.el --- Tests for the test infrastructure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Tests for vm-test-init.el itself -- specifically the isolation that keeps one
;; test's effect on VM's global state out of the next test (issue #559).
;;
;; These run a synthetic test through `ert-run-test', which is where the
;; isolation is installed, and then look at what survived.  A test cannot check
;; its own isolation, because that happens after it returns.

;;; Code:

(require 'vm-test-init)
(require 'vm-vars)

(defvar vm-test-init-test--scribble nil
  "A VM global for a test to leave a mark on.")

(defun vm-test-init-test--run (body)
  "Run BODY as an ert test, the way the suite would, and return its result."
  (ert-run-test (make-ert-test :name 'vm-test-init-test--inner :body body)))

(ert-deftest vm-test-init-test-isolation-restores-a-global ()
  "A test's changes to VM's global variables do not outlive it.
Issue #559: VM keeps its session state in global variables, so a test that
drives a real command sets them just as a running VM would, and whatever it
leaves behind becomes the starting state of every later test.  That makes a
test's result depend on what ran before it -- passing under `make test-one'
and failing in `make test' for a reason that is in neither test."
  (let ((vm-test-isolate-global-state t))
    (unwind-protect
        (progn
          (setq-default vm-test-init-test--scribble 'before)
          (vm-test-init-test--run
           (lambda () (setq-default vm-test-init-test--scribble 'during)))
          (should (eq 'before (default-value 'vm-test-init-test--scribble))))
      ;; A test about leaving state behind had better not leave any: the mark
      ;; has to be a global for the isolation to have something to restore, so
      ;; it is put back by hand, as the control below already does.
      (setq-default vm-test-init-test--scribble nil))))

(ert-deftest vm-test-init-test-isolation-can-be-turned-off ()
  "Without isolation the change does outlive the test.
The control for the test above: without this, that one could pass because
nothing ever wrote the variable."
  (let ((vm-test-isolate-global-state nil))
    (setq-default vm-test-init-test--scribble 'before)
    (unwind-protect
        (progn
          (vm-test-init-test--run
           (lambda () (setq-default vm-test-init-test--scribble 'during)))
          (should (eq 'during (default-value 'vm-test-init-test--scribble))))
      (setq-default vm-test-init-test--scribble nil))))

(ert-deftest vm-test-init-test-isolation-kills-leaked-buffers ()
  "A buffer a test leaves behind does not outlive it either.
A folder buffer carries a folder's worth of buffer-local state and a name that
the next test's `get-buffer' will find, so leaving one behind is as much a leak
as setting a variable is."
  (let ((vm-test-isolate-global-state t)
        (name " *vm-test-init-test-leak*"))
    (should-not (get-buffer name))
    (vm-test-init-test--run (lambda () (get-buffer-create name)))
    (should-not (get-buffer name))))

(ert-deftest vm-test-init-test-isolation-spares-the-sequences ()
  "Counters that hand out identities keep counting across tests.
Winding `vm-message-id-number' back would let two messages alive at once claim
the same id, which is the sort of cross-test interference the isolation exists
to prevent, so it is exempt."
  (should (memq 'vm-message-id-number vm-test-isolation-exceptions))
  (should-not (memq 'vm-message-id-number (vm-test-isolated-variables)))
  (let ((vm-test-isolate-global-state t)
        (before vm-message-id-number))
    (vm-test-init-test--run
     (lambda () (setq vm-message-id-number (1+ vm-message-id-number))))
    (should (= (1+ before) vm-message-id-number))))

(ert-deftest vm-test-init-test-isolation-covers-vm-variables ()
  "The saved set is VM's variables, found by name rather than by a list.
A list would go out of date every time VM gained a variable, and the point of
finding them by name is that it cannot."
  (let ((vars (vm-test-isolated-variables)))
    (should (memq 'vm-message-list vars))
    (should (memq 'vm-mail-buffer vars))
    (should (memq 'vm-imap-passwords vars))
    ;; and nothing outside VM
    (should-not (memq 'features vars))
    (should-not (memq 'load-path vars))))

;;; Nothing a run writes lands outside the tree

(ert-deftest vm-test-init-test-the-imap-cache-goes-in-the-tree ()
  "A cache file VM writes during a run is inside the test directory.
`vm-imap-make-filename-for-spec' falls back to `vm-folder-directory' and then
to $HOME, so with neither set the live IMAP tests wrote imap-cache-<md5> into
the developer's home directory and Emacs left a backup beside it.  The suite
sets `vm-imap-folder-cache-directory' to `vm-test-scratch-dir' for that reason,
and `make clean' removes it."
  (require 'vm-imap)
  (should vm-imap-folder-cache-directory)
  (should (equal (file-name-as-directory vm-imap-folder-cache-directory)
                 vm-test-scratch-dir))
  ;; under the test directory, and so not under $HOME by accident
  (should (string-prefix-p (expand-file-name vm-test-dir)
                           (expand-file-name vm-test-scratch-dir)))
  ;; and that is where a real cache name comes out
  (let ((file (vm-imap-make-filename-for-spec
               "imap:mail.example.com:143:INBOX:login:someone:secret")))
    (should (string-prefix-p (expand-file-name vm-test-scratch-dir)
                            (expand-file-name file)))
    (should (vm-cache-folder-name-p file))
    (should-not (equal (file-name-directory (expand-file-name file))
                       (file-name-as-directory (expand-file-name "~"))))))

;;; Asking for the mock servers on a machine that has a live one

(defun vm-test-init-test--live-wanted (value)
  "Whether the live tests are wanted with VM_TEST_LIVE set to VALUE."
  (let ((process-environment
         (cons (concat vm-test-live-environment-variable "=" value)
               process-environment)))
    (vm-test-live-wanted-p)))

(ert-deftest vm-test-init-test-live-servers-are-wanted-by-default ()
  "Unset, or set to anything that is not a refusal, means use the live
servers -- the config file is the opt-in, and the variable only takes it
away."
  (let ((process-environment
         (cons (concat vm-test-live-environment-variable "=")
               process-environment)))
    (should (vm-test-live-wanted-p)))
  (should (vm-test-init-test--live-wanted "1"))
  (should (vm-test-init-test--live-wanted "yes")))

(ert-deftest vm-test-init-test-the-live-servers-can-be-refused ()
  "VM_TEST_LIVE=0, and the other ways of saying no, run the mock servers
alone on a machine that has a live one configured.  That is how the mock
tests are checked where a live run would otherwise cover for them."
  (should-not (vm-test-init-test--live-wanted "0"))
  (should-not (vm-test-init-test--live-wanted "no"))
  (should-not (vm-test-init-test--live-wanted "off"))
  (should-not (vm-test-init-test--live-wanted "mock"))
  (should-not (vm-test-init-test--live-wanted "MOCK")))

(provide 'vm-test-init-test)

;;; vm-test-init-test.el ends here
