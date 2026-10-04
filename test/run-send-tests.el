;;; run-send-tests.el --- Runner for the mail-sending tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Runs the tests that send real mail.  Nothing else runs them: unlike the live
;; IMAP and POP tests, these are not part of `make test', because sending mail
;; leaves the machine.
;;
;; Run with: make test-send
;;
;; Requires a `vm-send-test-config' in test/vm-live-config.el.  Without it every
;; test skips and the run still succeeds.

;;; Code:

(setq load-prefer-newer t)

(defvar vm-send-test-runner-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "vm-test-init.el" vm-send-test-runner-dir))

(require 'vm-send-live-init)

(if (vm-send-live-configured-p)
    (message "Sending test mail from %s to %s, checking %s on %s"
             (vm-send-live-config :from) (vm-send-live-config :to)
             (vm-send-live-config :verify-mailbox "INBOX")
             (vm-send-live-config :verify-server))
  (message "No vm-send-test-config in test/vm-live-config.el.")
  (message "Every mail-sending test will skip.  See test/vm-live-config.el.template"))

(load (expand-file-name "vm-send-live-test.el" vm-send-test-runner-dir))

(when noninteractive
  (ert-run-tests-batch-and-exit "^vm-send-live-test-"))

;;; run-send-tests.el ends here
