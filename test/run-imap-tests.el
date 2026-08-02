;;; run-imap-tests.el --- Runner for the live IMAP tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Runs only the live IMAP tests, with `vm-imap-live-enabled' set.  This
;; runner is the sole thing that sets that flag, which is what keeps `make
;; test' from ever contacting a server.
;;
;; Run with: make test-imap
;;
;; Requires test/vm-imap-config.el.  Without it every test skips and the run
;; still succeeds, so an unconfigured machine is not a failure -- but a
;; configured server that cannot be reached is, see
;; dev/docs/design/imap-live-tests.org.

;;; Code:

(setq load-prefer-newer t)

(defvar vm-imap-test-runner-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "vm-test-init.el" vm-imap-test-runner-dir))

(require 'vm-imap-live-init)

(setq vm-imap-live-enabled t)

(unless vm-imap-test-servers
  (message "No test/vm-imap-config.el, or it configured no servers.")
  (message "Every live IMAP test will skip.  See %s"
           "dev/docs/design/imap-live-tests.org"))

(load (expand-file-name "vm-imap-live-test.el" vm-imap-test-runner-dir))

(when noninteractive
  (ert-run-tests-batch-and-exit "^vm-imap-live-test-"))

;;; run-imap-tests.el ends here
