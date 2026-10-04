;;; run-pop-tests.el --- Runner for the POP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Runs the POP tests only, for when that is all you care about.  They are not
;; exclusive to this runner: both files also run as part of `make test'.
;;
;; Run with: make test-pop
;;
;; Two kinds are loaded.  The mock tests in vm-pop-mock-test.el need nothing:
;; they serve POP3 on a local port and always run.  The live tests in
;; vm-pop-live-test.el need test/vm-live-config.el to set
;; `vm-pop-test-servers', and skip without it, so an unconfigured machine is
;; not a failure -- see dev/docs/design/imap-live-tests.org.

;;; Code:

(setq load-prefer-newer t)

(defvar vm-pop-test-runner-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "vm-test-init.el" vm-pop-test-runner-dir))

(require 'vm-pop-live-init)

(unless vm-pop-test-servers
  (message "No POP server in test/vm-live-config.el.")
  (message "The live POP tests will skip; the mock ones still run.  See %s"
           "dev/docs/design/imap-live-tests.org"))

(load (expand-file-name "vm-pop-mock-test.el" vm-pop-test-runner-dir))
(load (expand-file-name "vm-pop-live-test.el" vm-pop-test-runner-dir))

(when noninteractive
  (ert-run-tests-batch-and-exit "^vm-pop-\\(mock\\|live\\)-test-"))

;;; run-pop-tests.el ends here
