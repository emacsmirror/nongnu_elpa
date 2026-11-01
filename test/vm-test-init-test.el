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

(ert-deftest vm-test-init-test-a-password-cannot-be-asked-for ()
  "REGRESSION: `read-passwd' is stubbed whether or not isolation is on.

A batch Emacs reads a password prompt from standard input and waits for ever:
no timer runs while it waits, so it is a hang rather than a failure and it
takes the run with it.  The stub used to be inside the isolation, and
`leak-report.el' turns the isolation off, so the leaks pass ran without it --
a release check sat forty minutes on an IMAP password prompt, its last
printed line being the name of the gate.

Checked by looking at the function binding rather than by calling it: a test
that called `read-passwd' with the stub gone would hang, which is the thing
being prevented."
  (dolist (isolated '(t nil))
    (let ((vm-test-isolate-global-state isolated)
          (seen nil))
      ;; `read-passwd' is bound away first, because this test is itself run
      ;; through the advice and would otherwise see the stub that its own
      ;; isolation installed whatever the inner run did.
      (cl-letf (((symbol-function 'read-passwd) #'ignore))
        (vm-test-init-test--run
         (lambda () (setq seen (symbol-function 'read-passwd)))))
      (should (eq seen #'vm-test-no-reader-here)))))

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

;;; A scratch directory of the pass's own, issue #929

(defun vm-test-init-test--scratch-dir-with (value)
  "The `vm-test-scratch-dir' a fresh Emacs takes with VM_TEST_SCRATCH VALUE.
A child, because the variable is worked out when `vm-test-init.el' loads and
this Emacs loaded it long ago.  VALUE nil leaves the variable unset."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (init (expand-file-name "vm-test-init.el" vm-test-dir))
        (process-environment
         (if value
             (cons (concat "VM_TEST_SCRATCH=" value) process-environment)
           (let ((env nil))
             (dolist (entry process-environment (nreverse env))
               (unless (string-prefix-p "VM_TEST_SCRATCH=" entry)
                 (push entry env)))))))
    (with-temp-buffer
      (call-process emacs nil t nil "-Q" "--batch"
                    "-L" vm-test-lisp-dir "-L" vm-test-dir
                    "-l" init
                    "--eval" "(princ vm-test-scratch-dir)")
      ;; The last line: loading VM says things of its own on the way past,
      ;; "Loading vm-version-conf..." among them.
      (car (last (split-string (buffer-string) "\n" t "[ \t\r]+"))))))

(ert-deftest vm-test-init-test-the-scratch-directory-is-the-passs-own ()
  "VM_TEST_SCRATCH says where VM writes during a pass, under test/.
The runner gives each pass one of its own so that two passes running at once
cannot write the same IMAP cache file: `vm-imap-make-filename-for-spec' names
it after the MD5 of the maildrop and nothing else, so one maildrop is one
file whoever visits it."
  ;; a name of its own under test/, which is what the runner gives a pass
  (let ((relative "scratch/vm-test-init-test"))
    (unwind-protect
        (should (equal (vm-test-init-test--scratch-dir-with relative)
                       (file-name-as-directory
                        (expand-file-name relative vm-test-dir))))
      (delete-directory (expand-file-name relative vm-test-dir) t)))
  ;; an absolute name is taken as it stands
  (let ((absolute (make-temp-file "vm-test-init-test-scratch" t)))
    (unwind-protect
        (should (equal (vm-test-init-test--scratch-dir-with absolute)
                       (file-name-as-directory absolute)))
      (delete-directory absolute t)))
  ;; unset, the name it has always had
  (should (equal (vm-test-init-test--scratch-dir-with nil)
                 (file-name-as-directory
                  (expand-file-name "scratch" vm-test-dir)))))

;;; The suite split over several Emacsen, issue #930

(defun vm-test-init-test--files-for (spec files)
  "Those of FILES that VM_TEST_SHARD SPEC asks for."
  (let ((process-environment
         (cons (concat "VM_TEST_SHARD=" spec) process-environment)))
    (vm-test-files-for-shard files)))

(ert-deftest vm-test-init-test-the-shards-cover-every-file-once ()
  "Each file goes to exactly one shard, dealt out one at a time.
Not cut into blocks: the time is in a handful of files and neighbours in the
sorted list tend to be alike, so blocks would gather the slow ones together."
  (let ((files '("a" "b" "c" "d" "e" "f" "g")))
    (should (equal (vm-test-init-test--files-for "1/3" files) '("a" "d" "g")))
    (should (equal (vm-test-init-test--files-for "2/3" files) '("b" "e")))
    (should (equal (vm-test-init-test--files-for "3/3" files) '("c" "f")))
    (should (equal (sort (append (vm-test-init-test--files-for "1/3" files)
                                 (vm-test-init-test--files-for "2/3" files)
                                 (vm-test-init-test--files-for "3/3" files))
                         #'string<)
                   files))
    ;; one shard, or none asked for, is the whole list
    (should (equal (vm-test-init-test--files-for "1/1" files) files))
    (should (equal (vm-test-init-test--files-for "" files) files))))

(ert-deftest vm-test-init-test-a-shard-that-cannot-be-read-is-an-error ()
  "A VM_TEST_SHARD that is not INDEX/COUNT stops the run.
Silently running the whole suite instead would put every test in every shard
and look only slow, which is the kind of failure nobody goes looking for."
  (should-error (vm-test-init-test--files-for "1-3" '("a")))
  (should-error (vm-test-init-test--files-for "two/3" '("a")))
  (should-error (vm-test-init-test--files-for "0/3" '("a")))
  (should-error (vm-test-init-test--files-for "4/3" '("a")))
  (should-error (vm-test-init-test--files-for "1/0" '("a"))))

(provide 'vm-test-init-test)

;;; vm-test-init-test.el ends here
