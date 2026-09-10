;;; vm-test-init.el --- Test infrastructure for VM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Test infrastructure for VM including:
;; - Load path setup
;; - Fixture loading helpers
;; - Temporary directory/buffer macros
;; - Common test utilities

;;; Code:

;; Never test stale bytecode.  Emacs loads a .elc in preference to its
;; .el even when the source is newer -- it warns, but it still loads the
;; old build -- so a test run after an edit can silently exercise the
;; previous compile and pass.  This has to be set before any vm module
;; is required, and it lives here rather than in run-tests.el because
;; `make test-one', `make test-coverage' and hand-written
;; `emacs -batch' invocations all load this file but not that one.
(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

;;; Path setup

(defvar vm-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing test files.")

(defvar vm-test-lisp-dir
  (expand-file-name "../lisp" vm-test-dir)
  "Directory containing VM lisp source files.")

(defvar vm-test-fixtures-dir
  (expand-file-name "fixtures" vm-test-dir)
  "Directory containing test fixtures.")

(defvar vm-test-scratch-dir
  (file-name-as-directory (expand-file-name "scratch" vm-test-dir))
  "Directory for files the tests make VM write of its own accord.
In the tree, so that `make clean' removes it and a developer can see what was
left behind; a run leaves nothing anywhere else.

The IMAP cache is why this exists.  `vm-imap-make-filename-for-spec' names a
cache file after the MD5 of the maildrop, in `vm-imap-folder-cache-directory'
or `vm-folder-directory' or, failing both, $HOME -- and the tests set neither,
so visiting a folder in the live IMAP tests wrote imap-cache-<md5> into the
developer's home directory and Emacs left a backup beside it.")

;; Add VM lisp directory to load path
(add-to-list 'load-path vm-test-lisp-dir)

;;; What this run wants: live servers, optional packages

(defconst vm-test-live-environment-variable "VM_TEST_LIVE"
  "Environment variable saying whether the live tests may use the network.
Set it to 0, no, off or mock to run the mock servers alone on a machine that
has test/vm-live-config.el.  The mock tests always run either way.")

(defconst vm-test-optional-environment-variable "VM_TEST_OPTIONAL"
  "Environment variable saying whether VM\\='s optional companions are in play.
Set it to 0, no or off to run as a machine without BBDB, emacs-w3m and vcard
does, on one where `make optional-packages\\=' has installed them.")

(defun vm-test-environment-refuses-p (variable)
  "Whether VARIABLE, an environment variable, says no."
  (member (downcase (or (getenv variable) ""))
          '("0" "no" "off" "mock" "false")))

(defun vm-test-live-wanted-p ()
  "Whether the live tests may run, according to the environment.
True unless `vm-test-live-environment-variable' turns them off.  This is the
default of `vm-imap-live-enabled' and `vm-pop-live-enabled', which the config
file and a `let' can still override -- the variable is how a run says which
servers it wants, not whether any are configured."
  (not (vm-test-environment-refuses-p vm-test-live-environment-variable)))

(defun vm-test-optional-wanted-p ()
  "Whether the optional packages may be used, according to the environment.
The tests that need one skip when it is missing, so refusing them here runs
the suite as an ordinary checkout runs it."
  (not (vm-test-environment-refuses-p vm-test-optional-environment-variable)))

;;; VM's optional companions

(defvar vm-test-optional-dir
  (expand-file-name "opt/elpa" vm-test-dir)
  "Where `make optional-packages' installs BBDB, emacs-w3m and vcard.
Absent unless that has been run; the tests that need one skip without it.")

(defun vm-test-optional-load-path ()
  "Put each installed optional package on `load-path'.
Unless VM_TEST_OPTIONAL says not to: see `vm-test-optional-wanted-p'."
  (when (and (vm-test-optional-wanted-p)
             (file-directory-p vm-test-optional-dir))
    (dolist (dir (directory-files vm-test-optional-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(vm-test-optional-load-path)

;; Loaded here rather than by the tests that use them: each is loaded once per
;; Emacs and keeps buffers and variables, so whichever test required one first
;; was reported as leaking them.  `bbdb-initialize' is *not* called -- that is
;; what hooks BBDB into VM, and it would change every other test's world.
(dolist (feature '(bbdb bbdb-com vcard))
  (require feature nil t))

;; ...and the test directory itself, so a test file can `require' a helper
;; module that lives beside it, such as vm-imap-live-init.
(add-to-list 'load-path vm-test-dir)

;;; Load VM modules

(require 'vm-macro)
(require 'vm-vars)

;; An IMAP cache file goes in the tree, not in $HOME.  Set rather than
;; let-bound: what writes it is VM, deep inside a folder visit, and no test is
;; on the stack to bind anything.  `vm-folder-directory' is deliberately left
;; alone -- it decides where a user's folders are, and tests that care about it
;; set it themselves.
(make-directory vm-test-scratch-dir t)
(setq vm-imap-folder-cache-directory vm-test-scratch-dir)
(require 'vm-menu)

;;; Menus

;; Installing the menus is the one thing a folder visit does that no fixture can
;; put back.  `vm-menu-install-visited-folders-menu' splices the visited folders
;; into `vm-menu-folder-menu' with `setcdr' and rebuilds
;; `vm-menu-fsfemacs-folder-menu' from the result, so after a test the shared
;; menu holds the name of whatever folder the test invented in /tmp, and 89 tests
;; reported it (issue #559).  Nothing accumulates -- the next visit replaces the
;; spliced tail -- but the state is left behind all the same.
;;
;; So the suite runs with the menus switched off, and
;; `vm-menu-test-visiting-a-folder-fills-in-the-folder-menu' covers the
;; installation deliberately, once, putting the menus back itself.
;;
;; The menu map is initialized first: it is what defines the
;; `vm-menu-fsfemacs-*-menu' variables, a visit skips it with `vm-use-menus'
;; nil, and building a presentation buffer then reads one of them unbound.
;; Doing it here means it is done once for every test rather than by whichever
;; test visits a folder first.
(vm-menu-initialize-vm-mode-menu-map)

(defvar vm-test-vm-use-menus (default-value 'vm-use-menus)
  "The value `vm-use-menus' has outside the test suite.
Bind `vm-use-menus' to this in a test that means to exercise the menus.")

(setq vm-use-menus nil)

;;; Session initialization

;; Starting VM for the first time in an Emacs session does a pile of things
;; once: reads the init file, installs the addons, builds the window
;; configurations and the two obarrays VM uses as sets, adds to
;; `post-command-hook' and `kill-emacs-hook', and sets `vm-session-beginning'
;; nil.  Whichever test got there first paid for all of it and was reported as
;; leaking ten variables (issue #559), and two of those hooks are not VM's, so
;; the isolation could not put them back at all.
;;
;; Done here instead, so it belongs to no test.  The init file and the
;; preferences file are bound away: the suite must not read the developer's own.
;; No timer starts here -- `vm' starts those, not this.
(require 'vm)
(let ((vm-init-file nil)
      (vm-preferences-file nil))
  (vm-session-initialization))

;; No timers.  `vm-start-itimers-if-needed' runs when a folder is visited and
;; starts two repeating timers from these intervals: one to flush cached data
;; every 90 seconds, one to *check the maildrops for new mail* every 300.  A
;; suite that visits folders therefore leaves both running, where they fire
;; between later tests -- and the mail check would talk to whatever server
;; test/vm-live-config.el names.  Neither is something a test should be
;; sitting behind, and the leak report cannot see a timer.
;;
;; `vm-start-itimers-if-needed' does nothing at all when none of the three
;; intervals is a number, so this is the switch rather than cancelling them
;; afterwards.  A test that wants the timers binds these back.
;; `vm-auto-get-new-mail' is left alone: its default is t, which is not a
;; number, so it starts no timer -- and nil would stop a folder visit fetching
;; mail at all, which the live IMAP tests are about.
(setq vm-flush-interval nil
      vm-mail-check-interval nil)

;; No signature.  Starting a composition inserts one, and `mail-signature'
;; defaults to t, which means the developer's own ~/.signature -- so the tests
;; either read a file that is none of their business or, where there is none,
;; warn about it.  `vm-warn' sleeps for the two seconds it names, and eleven
;; tests start a composition, so that alone was 22 seconds of the suite.
(setq mail-signature nil)

;; The placeholders `vm-set-window-configuration' names for a summary,
;; composition or edit buffer that is not there.  It only ever looks them up;
;; what creates them is `set-tapestry', restoring a configuration that names
;; one.  So the first test to restore a window configuration made them and was
;; reported as leaking them.  Made here, so they belong to no test.
(get-buffer-create " *vm-nonexistent*")
(get-buffer-create " *vm-nonexistent-summary*")

;; And the scratch buffer `vm-pcrisis-split' works in, for the same reason: it is
;; created on first use and kept, so whichever test split a string first was
;; reported as leaving it behind.
(get-buffer-create " *split*")

;; And mail-extr.el's two, kept for the same reason.  Anything that pulls an
;; address apart with `mail-extract-address-components' makes them --
;; vm-smime.el does, reading the recipients of a composition -- so without
;; these the first test to do so is reported as leaking them.
(get-buffer-create " *canonical address*")
(get-buffer-create " *extract address components*")

;;; The toolbar

;; Like the menus: installing it defines tool-bar keys in `vm-mode-map', a
;; shared keymap, and records that in `vm-fsfemacs-toolbar-installed-p', so the
;; first test to visit a folder was reported as leaving both behind.  Off for the
;; suite, and `vm-toolbar-test-installing-defines-the-buttons' covers the
;; installation deliberately, with the keymap bound to a copy.
(defvar vm-test-vm-use-toolbar (default-value 'vm-use-toolbar)
  "The value `vm-use-toolbar' has outside the test suite.
Bind `vm-use-toolbar' to this in a test that means to exercise the toolbar.")

(setq vm-use-toolbar nil)

;; The summary sets the arrow strings from `vm-summary-arrow' globally as its
;; mode is set up, which is a first-visit-pays-for-everybody initialization like
;; the menu map, so it happens here.
(with-temp-buffer
  (vm-summary-mode-internal))

;; The compiled summary format is memoised in
;; `vm-summary-tokenized-compiled-format-alist', keyed by the format string, so
;; the first test to generate a summary filled in the entry for the default
;; format.  Compiled here for the same reason.  A test using a format of its own
;; still has to bind the variable.
(vm-summary-compile-format vm-summary-format t)

;;; Fixture helpers

(defun vm-test-fixture-path (category filename)
  "Return full path to fixture file FILENAME in CATEGORY subdirectory."
  (expand-file-name filename (expand-file-name category vm-test-fixtures-dir)))

(defun vm-test-read-fixture (category filename)
  "Read fixture file FILENAME from CATEGORY as a string."
  (with-temp-buffer
    (insert-file-contents (vm-test-fixture-path category filename))
    (buffer-string)))

(defun vm-test-fixture-exists-p (category filename)
  "Return non-nil if fixture file FILENAME exists in CATEGORY."
  (file-exists-p (vm-test-fixture-path category filename)))

;;; Temporary directory macro

(defmacro vm-test-with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a temporary directory.
The directory is created before BODY and deleted after, even on error."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "vm-test-" t))
          (default-directory (file-name-as-directory temp-dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;;; Temporary buffer macro

(defmacro vm-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.
Point is positioned at the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Mock process infrastructure for POP/IMAP testing

(defvar vm-test-mock-process nil
  "The current mock process object.")

(defvar vm-test-mock-buffer nil
  "Buffer associated with the mock process.")

(defvar vm-test-mock-responses nil
  "Queue of mock responses to return from mocked network functions.")

(defvar vm-test-mock-commands nil
  "List of commands sent to mocked network functions (most recent first).")

(defvar vm-test-mock-process-status 'open
  "Status to return for mock process.")

(defun vm-test-mock-process-p (obj)
  "Return t if OBJ is our mock process."
  (and (consp obj) (eq (car obj) 'vm-mock-process)))

(defun vm-test-mock-open-network-stream (name buffer host service &rest params)
  "Mock `open-network-stream' that returns a fake process."
  (ignore name host service params)
  ;; Use provided buffer or create one
  (setq vm-test-mock-buffer (or buffer (generate-new-buffer " *mock-net*")))
  (setq vm-test-mock-process (cons 'vm-mock-process vm-test-mock-buffer))
  vm-test-mock-process)

(defun vm-test-mock-process-status (process)
  "Mock `process-status' for our mock process."
  (cond ((null process) nil)
        ((vm-test-mock-process-p process) vm-test-mock-process-status)
        ;; For any other process in test context, assume closed
        (t nil)))

(defun vm-test-mock-process-buffer (process)
  "Mock `process-buffer' for our mock process."
  (cond ((null process) nil)
        ((vm-test-mock-process-p process) (cdr process))
        ;; For any other process in test context, return nil
        (t nil)))

(defun vm-test-mock-buffer-live-p (buffer)
  "Check if BUFFER is live (works with mock process buffers)."
  (buffer-live-p buffer))

(defun vm-test-mock-process-send-string (process string)
  "Mock `process-send-string' that records STRING and injects response."
  (push string vm-test-mock-commands)
  ;; After sending a command, inject the next response into the buffer
  (when vm-test-mock-responses
    (let ((response (pop vm-test-mock-responses))
          (buf (cond ((null process) vm-test-mock-buffer)
                     ((vm-test-mock-process-p process) (cdr process))
                     (t vm-test-mock-buffer))))
      (when (and response buf (buffer-live-p buf))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert response))))))

(defun vm-test-mock-accept-process-output (process &optional _seconds _millisec _just-this-one)
  "Mock `accept-process-output' - data already injected by send-string."
  (ignore process)
  ;; If there are still responses queued, inject one
  ;; This handles cases where accept-process-output is called without send-string
  (when (and vm-test-mock-responses
             vm-test-mock-buffer
             (buffer-live-p vm-test-mock-buffer))
    (let ((response (pop vm-test-mock-responses)))
      (when response
        (with-current-buffer vm-test-mock-buffer
          (goto-char (point-max))
          (insert response)))))
  t)

(defun vm-test-mock-delete-process (process)
  "Mock `delete-process' for mock processes."
  (when (vm-test-mock-process-p process)
    (setq vm-test-mock-process-status 'closed)))

(defun vm-test-mock-set-process-sentinel (process sentinel)
  "Mock `set-process-sentinel' - no-op for all processes in test context."
  (ignore process sentinel))

(defun vm-test-mock-process-sentinel (process)
  "Mock `process-sentinel' - returns nil for all processes in test context."
  (ignore process)
  nil)

(defun vm-test-mock-processp (obj)
  "Mock `processp' that returns t for mock processes."
  (vm-test-mock-process-p obj))

(defmacro vm-test-with-mock-process (responses &rest body)
  "Execute BODY with a fully mocked network process layer.
RESPONSES is a list of strings that will be injected as server responses.
Commands sent are recorded in `vm-test-mock-commands' (most recent first).

The mock process passes all standard process checks:
- `process-status' returns 'open
- `process-buffer' returns the associated buffer
- `process-send-string' records commands and injects responses
- `accept-process-output' returns t (data already injected)

Example:
  (vm-test-with-mock-process
      \\='(\"+OK POP3 ready\\r\\n\"
        \"+OK\\r\\n\"
        \"+OK 5 12345\\r\\n\")
    ;; Test code that makes POP calls
    (should (member \"STAT\\r\\n\" vm-test-mock-commands)))"
  (declare (indent 1) (debug t))
  `(let ((vm-test-mock-responses (copy-sequence ,responses))
         (vm-test-mock-commands nil)
         (vm-test-mock-process nil)
         (vm-test-mock-buffer nil)
         (vm-test-mock-process-status 'open))
     (cl-letf (((symbol-function 'open-network-stream)
                #'vm-test-mock-open-network-stream)
               ((symbol-function 'processp)
                #'vm-test-mock-processp)
               ((symbol-function 'process-status)
                #'vm-test-mock-process-status)
               ((symbol-function 'process-buffer)
                #'vm-test-mock-process-buffer)
               ((symbol-function 'process-send-string)
                #'vm-test-mock-process-send-string)
               ((symbol-function 'accept-process-output)
                #'vm-test-mock-accept-process-output)
               ((symbol-function 'delete-process)
                #'vm-test-mock-delete-process)
               ((symbol-function 'set-process-sentinel)
                #'vm-test-mock-set-process-sentinel)
               ((symbol-function 'process-sentinel)
                #'vm-test-mock-process-sentinel))
       (unwind-protect
           (progn ,@body)
         (when (and vm-test-mock-buffer (buffer-live-p vm-test-mock-buffer))
           (kill-buffer vm-test-mock-buffer))))))

;; Alias for backward compatibility
(defalias 'vm-test-with-mock-network 'vm-test-with-mock-process)

;;; POP-specific mock helpers

(defmacro vm-test-with-pop-session (responses &rest body)
  "Execute BODY with a mocked POP session buffer already set up.
RESPONSES are injected as POP server responses.
The buffer has `vm-pop-read-point' initialized.
`vm-test-mock-process' is set to a valid mock process."
  (declare (indent 1) (debug t))
  `(vm-test-with-mock-process ,responses
     (let ((pop-buffer (generate-new-buffer " *mock-pop*")))
       (unwind-protect
           (with-current-buffer pop-buffer
             ;; Create mock process connected to this buffer
             (setq vm-test-mock-buffer pop-buffer)
             (setq vm-test-mock-process (cons 'vm-mock-process pop-buffer))
             (make-local-variable 'vm-pop-read-point)
             (setq vm-pop-read-point (point-min-marker))
             ;; Inject greeting
             (when vm-test-mock-responses
               (insert (pop vm-test-mock-responses)))
             (goto-char (point-min))
             ,@body)
         (kill-buffer pop-buffer)))))

;;; IMAP-specific mock helpers

(defmacro vm-test-with-imap-session (responses &rest body)
  "Execute BODY with a mocked IMAP session buffer already set up.
RESPONSES are injected as IMAP server responses.
The buffer has IMAP-related variables initialized.
`vm-test-mock-process' is set to a valid mock process."
  (declare (indent 1) (debug t))
  `(vm-test-with-mock-process ,responses
     (let ((imap-buffer (generate-new-buffer " *mock-imap*"))
           ;; The IMAP functions assert that they are in a process buffer, and
           ;; every real caller says so with `vm-buffer-type:enter'.  Without
           ;; this the tests exercise them in a state no session is ever in,
           ;; and pass only because `vm-assertion-checking-off' defaults to t.
           (vm-buffer-types '(process)))
       (unwind-protect
           (with-current-buffer imap-buffer
             ;; Create mock process connected to this buffer
             (setq vm-test-mock-buffer imap-buffer)
             (setq vm-test-mock-process (cons 'vm-mock-process imap-buffer))
             (make-local-variable 'vm-imap-read-point)
             (setq vm-imap-read-point (point-min-marker))
             (make-local-variable 'vm-imap-session-done)
             (setq vm-imap-session-done nil)
             ;; Inject greeting
             (when vm-test-mock-responses
               (insert (pop vm-test-mock-responses)))
             (goto-char (point-min))
             ,@body)
         (kill-buffer imap-buffer)))))

;;; Test skip helpers

(defmacro vm-test-skip-unless (condition &optional message)
  "Skip the current test unless CONDITION is true.
Optional MESSAGE explains why the test was skipped, and says what to do about
it where there is anything to do.

The reason is printed as well as handed to `ert-skip'.  A batch run shows only
the name of a skipped test -- neither the per-test line nor the summary
carries the reason -- so a message given to `ert-skip' alone is invisible
exactly where someone is reading the output and wondering what went wrong."
  `(unless ,condition
     (let ((reason (or ,message "Precondition not met")))
       (message "Skipping %s: %s"
                (or (ignore-errors (ert-test-name (ert-running-test)))
                    "this test")
                reason)
       (ert-skip reason))))

;;; Folder setup helpers

;; Load additional modules needed for folder operations
(require 'vm-folder)
(require 'vm-message)
(require 'vm-mime)

(defvar vm-test-folder-default-variables
  '((vm-folder-type . nil)
    (vm-folder-access-method . nil)
    (vm-message-list . nil)
    (vm-message-pointer . nil)
    (vm-summary-buffer . nil)
    (vm-presentation-buffer . nil)
    (vm-system-state . nil)
    (vm-undo-record-list . nil)
    (vm-undo-record-pointer . nil)
    (vm-message-order-changed . nil)
    (vm-message-order-header-present . nil)
    (vm-numbering-redo-start-point . nil)
    (vm-numbering-redo-end-point . nil)
    (vm-summary-redo-start-point . nil)
    (vm-folder-read-only . nil)
    (vm-modification-counter . 0)
    (vm-message-list-generation . 0)
    (vm-messages-not-on-disk . 0)
    (vm-totals . nil)
    (vm-thread-obarray . nil)
    (vm-thread-subject-obarray . nil)
    (vm-buffers-needing-display-update . nil)
    (vm-default-From_-folder-type . From_)
    (vm-trust-content-length . nil)
    (vm-display-using-mime . t)
    (vm-auto-decode-mime-messages . t)
    (vm-mime-charset-converter-alist . nil)
    (vm-mime-default-face-charsets . nil))
  "Default variable bindings for test folder buffers.")

(defun vm-test-init-folder-variables ()
  "Initialize VM folder variables in current buffer."
  (dolist (var-val vm-test-folder-default-variables)
    (set (make-local-variable (car var-val)) (cdr var-val)))
  ;; Initialize hash table for buffers needing update
  (setq vm-buffers-needing-display-update (make-vector 29 0)))

(defun vm-test-init-message-data (message)
  "Initialize attributes and cached-data arrays for MESSAGE.
This is needed because vm-make-message doesn't create these arrays."
  ;; Initialize attributes array (element 2) if not present
  (unless (vm-attributes-of message)
    (vm-set-attributes-of message (make-vector vm-attributes-vector-length nil)))
  ;; Initialize cached-data array (element 3) if not present
  (unless (vm-cached-data-of message)
    (vm-set-cached-data-of message (make-vector vm-cached-data-vector-length nil))))

(defmacro vm-test-with-folder (content &rest body)
  "Execute BODY in a temp buffer set up as a VM folder with CONTENT.
The buffer is initialized with VM folder variables and messages are parsed.
`vm-message-list' will contain the parsed messages.

CONTENT should be a string containing raw email(s) in mbox format.

Example:
  (vm-test-with-folder
    \"From sender@example.com Mon Jan  1 00:00:00 2024
From: sender@example.com
Subject: Test

Body text
\"
    (should (= (length vm-message-list) 1)))"
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (vm-test-init-folder-variables)
     (insert ,content)
     (goto-char (point-min))
     ;; Parse messages
     (vm-build-message-list)
     ;; Initialize attributes and cached-data for each message
     (dolist (msg vm-message-list)
       (vm-test-init-message-data msg))
     ;; Set message pointer to first message
     (setq vm-message-pointer vm-message-list)
     (unwind-protect
         (progn ,@body)
       ;; `with-temp-buffer' takes the folder buffer away, but not a
       ;; presentation copy made from it, which is a buffer of its own and
       ;; outlives the test (issue #559).
       (when (buffer-live-p vm-presentation-buffer)
         (kill-buffer vm-presentation-buffer)))))

(defun vm-test-write-simple-folder (file n &optional threaded)
  "Write a folder of N messages to FILE, subjects \"subject 0\" upwards.
THREADED non-nil has each message reference the one before it."
  (with-temp-file file
    (dotimes (i n)
      (insert "From alice@example.com Mon Jan  1 00:00:00 2024\n"
              "From: alice@example.com\n"
              (format "Subject: subject %d\n" i)
              (format "Message-ID: <plain-%d@example.com>\n" i)
              (if (and threaded (> i 0))
                  (format "References: <plain-%d@example.com>\n" (1- i))
                "")
              "\n"
              (format "Body %d.\n\n" i)))))

(defmacro vm-test-with-real-folder (spec &rest body)
  "Visit a generated folder with `vm-visit-folder' and run BODY in its buffer.
SPEC is (N &optional THREADED), the arguments of
`vm-test-write-simple-folder'.  Everything the visit created is killed
afterwards, and killing one of those buffers inside BODY is allowed.

The difference from `vm-test-with-folder' is that this is a folder VM visited:
it has a summary, a mode line, an undo list and a folder file on disk, so
commands that expect all that work without being stubbed.  It costs about a
millisecond, so prefer `vm-test-with-folder' where the lighter one does.

Variables a visit records are bound rather than set, so the folder invented
here does not turn up in a later test's history."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-real" t)))
          (file (expand-file-name "folder" dir))
          (vm-init-file nil)
          (vm-preferences-file nil)
          (vm-confirm-quit nil)
          (vm-frame-per-folder nil)
          (vm-mutable-frame-configuration nil)
          (vm-summary-show-threads nil)
          (vm-folder-history vm-folder-history)
          (vm-last-visit-folder vm-last-visit-folder)
          (vm-user-interaction-buffer vm-user-interaction-buffer)
          (before (buffer-list)))
     (require 'vm)
     (unwind-protect
         (progn
           (vm-test-write-simple-folder file ,(car spec) ,(nth 1 spec))
           (vm-visit-folder file)
           ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defmacro vm-test-with-folder-fixture (category filename &rest body)
  "Execute BODY with a VM folder loaded from fixture file.
CATEGORY and FILENAME specify the fixture to load."
  (declare (indent 2) (debug t))
  `(vm-test-with-folder (vm-test-read-fixture ,category ,filename)
     ,@body))

(defun vm-test-message-count ()
  "Return the number of messages in current folder."
  (length vm-message-list))

(defun vm-test-first-message ()
  "Return the first message in current folder."
  (car vm-message-list))

(defun vm-test-nth-message (n)
  "Return the Nth message (0-indexed) in current folder."
  (nth n vm-message-list))

(defun vm-test-reverse-links-consistent-p (&optional message-list)
  "Return non-nil if every reverse link in MESSAGE-LIST is the cons before it.
Defaults to `vm-message-list'.  The first message must have no link.

This is the invariant `vm-expunge-message' relies on to decide which cons to
splice out, so a folder whose links have drifted can lose the wrong message
while deleting the right one's text.  Issue #453 moved the links out of the
message vectors into `vm-reverse-link-table'; this says what has to stay true
of them however they are stored."
  (let ((mp (or message-list vm-message-list))
        (prev nil)
        (ok t))
    (while mp
      (unless (eq (vm-reverse-link-of (car mp)) prev)
        (setq ok nil))
      (setq prev mp mp (cdr mp)))
    ok))

(defun vm-test-message-body (m)
  "Return the body text of message M as a string."
  (save-excursion
    (vm-find-and-set-text-of m)
    (buffer-substring-no-properties
     (vm-text-of m)
     (vm-text-end-of m))))

(defun vm-test-message-header (m header-name)
  "Return the value of HEADER-NAME from message M."
  (save-excursion
    (goto-char (vm-headers-of m))
    (let ((case-fold-search t)
          (limit (or (vm-text-of m) (vm-text-end-of m))))
      (when (re-search-forward
             (concat "^" (regexp-quote header-name) ":[ \t]*")
             limit t)
        (let ((start (point)))
          ;; Handle multi-line headers
          (while (progn
                   (forward-line 1)
                   (and (< (point) limit)
                        (looking-at "[ \t]"))))
          (buffer-substring-no-properties start (1- (point))))))))

;;; Test isolation

;; VM keeps much of its state in global variables -- the session flags, the
;; password caches, the folder history, the compiled summary format cache --
;; and a test that drives a real command sets them, exactly as a running VM
;; would.  Whatever a test leaves behind is then the starting state of every
;; test after it, so a test can pass under `make test-one' and fail in `make
;; test' for a reason that appears in neither test.  That is issue #559, and it
;; had already cost one test: the end-to-end half of #514 was dropped because
;; of it.
;;
;; The alternative -- each test binding what it might touch -- needs the author
;; to know everything the code under test reaches, and goes quietly out of date
;; as VM changes.  So instead every test runs with the global default of every
;; VM variable saved beforehand and put back afterwards, whether the test
;; thought about it or not.

(defvar vm-test-isolate-global-state t
  "When non-nil, undo a test's effect on global state when it finishes.
That means the global value of VM's variables, and the buffers the test
created.  Set to nil to see the suite as it behaves without isolation, which
is how issue #559 was diagnosed.")

(defvar vm-test-isolation-exceptions
  '(;; Sequences, not settings: these hand out values that must never be
    ;; handed out twice, so winding them back would make two live objects
    ;; share an identity -- the very kind of cross-test interference the
    ;; rest of this is here to prevent.
    vm-message-id-number
    vm-imap-live--mailbox-counter
    vm-pop-live--id-counter
    ;; The isolation's own bookkeeping.
    vm-test--isolated-variables
    vm-test--isolated-variables-features)
  "VM variables whose value must survive from one test to the next.")

(defvar vm-test--isolated-variables nil
  "Cached list of VM variables to save and restore.")

(defvar vm-test--isolated-variables-features nil
  "Value of `features' when `vm-test--isolated-variables' was computed.")

(defun vm-test-isolated-variables ()
  "Return the VM variables whose global value is saved around each test.
Recomputed when a new module has been loaded, since that is when new
variables come into existence; scanning the obarray for every test would
cost more than the tests do."
  (unless (eq features vm-test--isolated-variables-features)
    (setq vm-test--isolated-variables-features features)
    (setq vm-test--isolated-variables nil)
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (not (keywordp symbol))
                  (string-prefix-p "vm-" (symbol-name symbol))
                  (not (memq symbol vm-test-isolation-exceptions)))
         (push symbol vm-test--isolated-variables)))))
  vm-test--isolated-variables)

(defun vm-test-snapshot-global-state ()
  "Return the current global value of every variable to be isolated."
  (let ((state nil))
    (dolist (symbol (vm-test-isolated-variables))
      ;; A variable can be special but have no default value -- `defvar' with
      ;; no value, or a buffer-local-only variable -- and asking for one then
      ;; signals rather than returning nil.
      (condition-case nil
          (push (cons symbol (default-value symbol)) state)
        (error nil)))
    state))

(defun vm-test-restore-global-state (state)
  "Put back the global values recorded by `vm-test-snapshot-global-state'."
  (dolist (entry state)
    ;; Only where it actually differs: assigning a default that was already
    ;; correct is not harmless for a variable that has none, since it would
    ;; give it one.
    (unless (condition-case nil
                (eq (default-value (car entry)) (cdr entry))
              (error nil))
      (condition-case nil
          (set-default (car entry) (cdr entry))
        (error nil)))))

(defun vm-test-kill-new-buffers (buffers)
  "Kill every live buffer that is not in BUFFERS.
A folder buffer outlives its test just as readily as a variable does, and it
carries a whole folder's worth of buffer-local state plus a name that the next
test's `get-buffer' will find.  Session buffers are the common case, since VM
keeps them for reuse and the variable holding them has just been wound back.

Liveness is asked again after the process goes: deleting one runs its
sentinel, and a sentinel may kill buffers -- an asynchronous session tidies up
its own when its connection dies, and that buffer is usually further down this
very list.  Asking once left `set-buffer' with a killed buffer, which took
down the whole run rather than the one test."
  (dolist (buffer (buffer-list))
    (unless (memq buffer buffers)
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when process
            ;; Killing a buffer whose process is still live asks for
            ;; confirmation, and a question in batch reads stdin.
            (set-process-query-on-exit-flag process nil)
            (ignore-errors (delete-process process)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          ;; `vm-postpone' offers to save a composition as a draft from
          ;; `kill-buffer-hook', which is another question.
          (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))
        (ignore-errors (kill-buffer buffer))))))

(defun vm-test-cancel-composition-timer ()
  "Cancel the idle timer VM starts to rename composition buffers.
`vm-mail-internal' starts it with the first composition and nothing stops it,
so a test that composed left it running.  Restoring the variable is not enough
and is worse: the timer would still be scheduled with nothing naming it, VM
would start a second one for the next composition, and both would fire in the
middle of later tests -- which is where a stray `*Warnings*' buffer was coming
from.  No test leaves a composition behind, so the timer has nothing to rename."
  (when (and (boundp 'vm-update-composition-buffer-name-timer)
             vm-update-composition-buffer-name-timer)
    (cancel-timer vm-update-composition-buffer-name-timer)
    (setq vm-update-composition-buffer-name-timer nil)))

(defun vm-test-no-reader-here (&rest _)
  "Refuse to ask for a password, which is what batch has to do.

`read-passwd' in a batch Emacs waits on standard input for ever: a test that
reached a password prompt did not fail, it hung, and took the whole run with
it -- an hour of a suite for one test asking a question nobody was there to
answer.  A test that means to be asked binds this away with `cl-letf' and
answers for itself."
  (error "No reader here to give a password to"))

(defun vm-test-run-test-isolated (run-test test)
  "Run TEST through RUN-TEST, then undo its effect on global state."
  (if (not vm-test-isolate-global-state)
      (funcall run-test test)
    (let ((state (vm-test-snapshot-global-state))
          (buffers (buffer-list)))
      (unwind-protect
          (cl-letf (((symbol-function 'read-passwd) #'vm-test-no-reader-here))
            (funcall run-test test))
        (vm-test-cancel-composition-timer)
        (vm-test-restore-global-state state)
        (vm-test-kill-new-buffers buffers)))))

(advice-add 'ert-run-test :around #'vm-test-run-test-isolated)

;;; Test file discovery

(defconst vm-test-excluded-files
  '("vm-send-live-test.el")
  "Test files `make test' does not load, run by a target of their own.
The live IMAP and POP tests are not here: they talk to a server on localhost,
which costs nothing, so once a config exists they run with everything else.
Sending mail leaves the machine, so it is asked for by name -- `make
test-send\', which loads that file itself.")

(defun vm-test-discover-test-files ()
  "Return list of test files in `vm-test-dir'.
Files match pattern vm-*-test.el, excluding vm-test-init.el and the files in
`vm-test-excluded-files\'."
  (sort (seq-remove (lambda (f) (member f vm-test-excluded-files))
                    (directory-files vm-test-dir nil "^vm-.*-test\\.el$"))
        #'string<))

(defun vm-test-load-all-test-files ()
  "Load all test files from `vm-test-dir'."
  (let ((test-files (vm-test-discover-test-files)))
    (message "Loading %d test files..." (length test-files))
    (dolist (f test-files)
      (load (expand-file-name f vm-test-dir)))))

(provide 'vm-test-init)

;;; vm-test-init.el ends here
