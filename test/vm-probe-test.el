;;; vm-probe-test.el --- Tests for vm-test-probe.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The probe is what test/test-runner prints before it runs anything, so that a
;; failure can be told from a machine that is short of a server.  These tests
;; contact nothing: the per-server check is stubbed, which is the only part
;; that would.

;;; Code:

(require 'vm-test-init)
(require 'vm-test-probe)

(defconst vm-probe-test--servers
  '((:name "plain" :host "127.0.0.1" :port 143 :accounts (("u" . "p")))
    (:name "tls" :host "localhost" :port 993 :tls t :accounts (("u" . "p"))))
  "Two servers to report on, structured as test/vm-live-config.el sets them.")

(defmacro vm-probe-test--reporting (answers &rest body)
  "Run BODY with the servers above configured and each probed by ANSWERS.
ANSWERS maps a server name to nil for reachable or to why it is not, which
is what `vm-test-probe-imap-server' returns."
  (declare (indent 1) (debug t))
  `(let ((vm-imap-test-servers vm-probe-test--servers)
         (vm-pop-test-servers nil)
         (vm-send-test-config nil))
     (cl-letf (((symbol-function 'vm-test-probe-imap-server)
                (lambda (server)
                  (cdr (assoc (plist-get server :name) ,answers)))))
       ,@body)))

(defun vm-probe-test--value (report key)
  "The value of KEY in REPORT."
  (cdr (assoc key report)))

(ert-deftest vm-probe-test-a-server-that-answers-is-reached ()
  "A server that logs in is listed as reached and nothing is said against it."
  (vm-probe-test--reporting '(("plain") ("tls"))
    (let ((report (vm-test-probe-report)))
      (should (equal (vm-probe-test--value report "imap_servers") "plain tls"))
      (should (equal (vm-probe-test--value report "imap_reached") "plain tls"))
      (should (equal (vm-probe-test--value report "imap_unreached") "")))))

(ert-deftest vm-probe-test-a-server-that-does-not-answer-says-why ()
  "A configured server that cannot be reached is named with the reason, which
is the whole point: its tests fail rather than skip, and the failures alone do
not say that the server was the problem."
  (vm-probe-test--reporting '(("plain") ("tls" . "Connection refused"))
    (let ((report (vm-test-probe-report)))
      (should (equal (vm-probe-test--value report "imap_reached") "plain"))
      (should (equal (vm-probe-test--value report "imap_unreached")
                     "tls: Connection refused")))))

(ert-deftest vm-probe-test-every-key-is-always-there ()
  "The report has every key whether or not anything is configured, so the
runner reads an empty value rather than having to tell a missing key from an
empty one."
  (let ((vm-imap-test-servers nil)
        (vm-pop-test-servers nil)
        (vm-send-test-config nil))
    (let ((report (vm-test-probe-report)))
      (dolist (key '("config_file" "config_present" "imap_servers"
                     "imap_reached" "imap_unreached" "pop_servers"
                     "pop_reached" "pop_unreached" "send_configured"
                     "send_to" "send_verify_server" "gpg"))
        (should (assoc key report))
        (should (stringp (vm-probe-test--value report key))))
      (should (equal (vm-probe-test--value report "send_configured") "no")))))

(ert-deftest vm-probe-test-the-report-is-one-line-per-key ()
  "`vm-test-probe-batch' prints key=value lines and the shell reads them with
`sed', so a value carrying a newline would be read as another key.  Server
errors are multi-line often enough for this to matter."
  (should (equal (vm-test-probe--reason '(error "one\ntwo\r\nthree"))
                 "one two three")))

(provide 'vm-probe-test)

;;; vm-probe-test.el ends here
