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

(defconst vm-probe-test--config-template
  (expand-file-name "vm-live-config.el.template" vm-test-dir)
  "The file a reader copies to make test/vm-live-config.el.")

(defun vm-probe-test--template-forms ()
  "Every top-level form of the config template, read and not evaluated.
Evaluating it would reach for a secret store and set the live servers."
  (with-temp-buffer
    (insert-file-contents vm-probe-test--config-template)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest vm-probe-test-the-config-template-is-readable-lisp ()
  "The template parses, and nothing else reads it.

It is copied to `vm-live-config.el' and loaded, so a syntax error in it is
found by whoever next sets up the live tests and by nobody before them.
Reading is as far as this goes: the template defines a function that asks a
secret store for a password, and the `setq' forms would replace the servers
this file configures for its own tests."
  (should (file-readable-p vm-probe-test--config-template))
  (let ((forms (vm-probe-test--template-forms)))
    (should forms)
    (should (seq-every-p #'consp forms))))

(ert-deftest vm-probe-test-the-config-template-shows-a-secret-store ()
  "REGRESSION: the template offers a password that is not written in the file.

emacs-vm/vm#906.  `vm-live-config-keychain' has to raise rather than answer
the empty string: a lookup that returns nothing sends an empty password, and
`LOGIN \"user\" \"\" => NO [AUTHENTICATIONFAILED]' names authentication
instead of the secret store that has lost the item.  Four send tests failed
that way with nothing pointing at the cause.

The template is the only place this pattern is written down, `vm-live-config.el'
itself being gitignored."
  (let ((definition (seq-find (lambda (form)
                                (and (eq (car-safe form) 'defun)
                                     (eq (nth 1 form) 'vm-live-config-keychain)))
                              (vm-probe-test--template-forms))))
    (should definition)
    ;; `error' rather than a bare return is the whole point of it.
    (should (seq-contains-p (flatten-tree definition) 'error))
    ;; stderr is kept, which is where `security' says the item is missing.
    (should (seq-contains-p (flatten-tree definition) 'call-process))))

(provide 'vm-probe-test)

;;; vm-probe-test.el ends here
