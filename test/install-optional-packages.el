;;; install-optional-packages.el --- fetch VM's optional companions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; VM integrates with packages it does not require: BBDB for addresses,
;; emacs-w3m for HTML, vcard for business cards.  Those code paths are the
;; least tested in VM, because a machine without the package cannot run them
;; at all -- the whole test suite prints "Could not load feature bbdb" and
;; carries on.
;;
;; This installs them under test/opt/elpa, where `vm-test-init.el' finds them
;; and the tests that need them stop skipping.  Run it with
;;
;;   make optional-packages
;;
;; It needs the network.  Nothing else in the tests does, and nothing here is
;; committed: test/opt is ignored.

;;; Code:

(require 'package)

(defvar vm-optional-packages
  '((bbdb . "gnu")                      ; addresses; VM's biggest integration
    (vcard . "gnu")                     ; vm-vcard.el
    (w3m . "melpa"))                    ; emacs-w3m; not on GNU or nonGNU ELPA
  "Packages to install, and the archive each comes from.")

(defvar vm-optional-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa" . "https://melpa.org/packages/")))

(defun vm-optional-install (directory)
  "Install `vm-optional-packages' into DIRECTORY, reporting what happened.
Returns the ones that failed, as (NAME . REASON), or nil if all are there."
  (let ((package-user-dir (expand-file-name "elpa" directory))
        (package-archives vm-optional-archives)
        (installed nil)
        (failed nil))
    (make-directory package-user-dir t)
    (package-initialize)
    (package-refresh-contents)
    (dolist (entry vm-optional-packages)
      (let ((name (car entry)))
        (condition-case err
            (progn
              (unless (package-installed-p name)
                (package-install name))
              (push name installed))
          (error
           (push (cons name (error-message-string err)) failed)))))
    (message "optional packages installed: %s"
             (mapconcat #'symbol-name (nreverse installed) " "))
    (setq failed (nreverse failed))
    (dolist (f failed)
      (message "optional package %s FAILED: %s" (car f) (cdr f)))
    failed))

(defun vm-optional-install-batch ()
  "Install into the directory named on the command line.
Exits non-zero if any package did not install.  The tests tolerate a missing
companion -- they skip -- but this command does not: a caller told the fetch
worked will believe the package is there, and `make test-no-opt' is how to
run the tests without it."
  (let ((directory (or (car command-line-args-left)
                       (error "Usage: -f vm-optional-install-batch DIRECTORY"))))
    (setq command-line-args-left (cdr command-line-args-left))
    (kill-emacs (if (vm-optional-install directory) 1 0))))

(provide 'install-optional-packages)

;;; install-optional-packages.el ends here
