;;; run-imap-tests.el --- Runner for the live IMAP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Runs the IMAP tests, for when that is all you care about.  They are not
;; exclusive to this runner: both files also run as part of `make test'.
;;
;; Run with: make test-imap
;;
;; Two kinds are loaded.  The mock tests in vm-imap-mock-test.el serve IMAP on
;; a local port and always run -- including on a machine that has a live
;; server configured, so that the mock cannot rot unnoticed behind a live run
;; that covers the same ground.  The live tests in vm-imap-live-test.el need
;; test/vm-live-config.el and skip without it, so an unconfigured machine is
;; not a failure -- but a configured server that cannot be reached is, see
;; dev/docs/design/imap-live-tests.org.

;;; Code:

(setq load-prefer-newer t)

(defvar vm-imap-test-runner-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "vm-test-init.el" vm-imap-test-runner-dir))

(require 'vm-imap-live-init)

(unless vm-imap-test-servers
  (message "No test/vm-live-config.el, or it configured no servers.")
  (message "The live IMAP tests will skip; the mock ones still run.  See %s"
           "dev/docs/design/imap-live-tests.org"))

(load (expand-file-name "vm-imap-mock-test.el" vm-imap-test-runner-dir))
(load (expand-file-name "vm-imap-live-test.el" vm-imap-test-runner-dir))

(when noninteractive
  (ert-run-tests-batch-and-exit "^vm-imap-\\(mock\\|live\\)-test-"))

;;; run-imap-tests.el ends here
