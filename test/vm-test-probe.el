;;; vm-test-probe.el --- What this machine can test -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Answers, before any test runs, the question a non-zero exit status cannot:
;; is the machine short of a server, or is VM broken?  A missing
;; test/vm-live-config.el makes the live tests skip, and a configured server
;; that cannot be reached makes them fail -- on purpose, since a suite that
;; quietly skips a server it was told about has no coverage.  Told apart only
;; by looking at what is there, which is what this does.
;;
;; test/test-runner reads it as `key=value' lines, one per line, no quoting:
;;
;;     emacs -batch -Q -L lisp -l test/vm-test-probe.el -f vm-test-probe-batch
;;
;; Reading the same servers with the live harness' own client, not with
;; vm-imap.el, for the reason that harness gives: a check written with the code
;; under test can pass because both are wrong.

;;; Code:

(require 'cl-lib)
(require 'epg-config)
(require 'vm-test-init)
(require 'vm-imap-live-init)
(require 'vm-pop-live-init)
(require 'vm-send-live-init)

(defun vm-test-probe--reason (err)
  "ERR as one line, since the report is a line per key."
  (replace-regexp-in-string "[\r\n]+" " " (error-message-string err)))

(defun vm-test-probe-imap-server (server)
  "Log in to SERVER and out again.  Return nil if that worked, else why not."
  (condition-case err
      (let ((conn (vm-imap-live--open server)))
        (unwind-protect
            (progn (vm-imap-live-login conn server
                                       (car (plist-get server :accounts)))
                   nil)
          (ignore-errors (vm-imap-live-close conn))))
    (error (vm-test-probe--reason err))))

(defun vm-test-probe-pop-server (server)
  "Log in to POP SERVER and out again.  Return nil if that worked, else why not."
  (condition-case err
      (let ((conn (vm-pop-live--open server)))
        (unwind-protect
            (progn (vm-pop-live-login conn server
                                      (car (plist-get server :accounts)))
                   nil)
          (ignore-errors (vm-pop-live-close conn))))
    (error (vm-test-probe--reason err))))

(defun vm-test-probe--servers (servers probe)
  "Probe each of SERVERS with PROBE.
Return (REACHED . UNREACHED), REACHED a list of names and UNREACHED a list
of \"name: why not\"."
  (let (reached unreached)
    (dolist (server servers)
      (let* ((name (plist-get server :name))
             (why (funcall probe server)))
        (if why
            (push (format "%s: %s" name why) unreached)
          (push name reached))))
    (cons (nreverse reached) (nreverse unreached))))

(defun vm-test-probe-report ()
  "What this machine can test, as an alist of key to string.
Every key is always present, so a reader need not tell a missing key from an
empty one."
  (let* ((imap (vm-test-probe--servers vm-imap-test-servers
                                       #'vm-test-probe-imap-server))
         (pop (vm-test-probe--servers vm-pop-test-servers
                                      #'vm-test-probe-pop-server)))
    (list (cons "config_file" vm-live-config-file)
          (cons "config_present"
                (if (file-readable-p vm-live-config-file) "yes" "no"))
          (cons "imap_servers"
                (string-join (mapcar (lambda (s) (plist-get s :name))
                                     vm-imap-test-servers)
                             " "))
          (cons "imap_reached" (string-join (car imap) " "))
          (cons "imap_unreached" (string-join (cdr imap) ", "))
          (cons "pop_servers"
                (string-join (mapcar (lambda (s) (plist-get s :name))
                                     vm-pop-test-servers)
                             " "))
          (cons "pop_reached" (string-join (car pop) " "))
          (cons "pop_unreached" (string-join (cdr pop) ", "))
          (cons "send_configured"
                (if (vm-send-live-configured-p) "yes" "no"))
          (cons "send_to" (or (vm-send-live-config :to) ""))
          (cons "send_verify_server"
                (or (vm-send-live-config :verify-server) ""))
          ;; the program, not `epg-check-configuration': that calls gpg 2.5
          ;; unsupported, and the tests using it pass all the same
          (cons "gpg" (if (ignore-errors
                            (alist-get 'program (epg-find-configuration 'OpenPGP)))
                          "yes" "no")))))

(defun vm-test-probe-batch ()
  "Print `vm-test-probe-report' as key=value lines and exit."
  (dolist (pair (vm-test-probe-report))
    (princ (format "%s=%s\n" (car pair) (cdr pair))))
  (kill-emacs 0))

(provide 'vm-test-probe)

;;; vm-test-probe.el ends here
