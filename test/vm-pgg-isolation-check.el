;;; vm-pgg-isolation-check.el --- What loading vm-pgg does to VM -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Run in its own Emacs by `vm-pgg-isolation-test.el'.  The suite's Emacs has
;; every VM module loaded already, so it cannot tell what a load does; only a
;; fresh one can, and only in this order.

;; Prints one RESULT line per question, for the test to read.

;;; Code:

(require 'vm-autoloads)
(require 'vm-cus-load)
(require 'vm-epg)

(defun vm-pgg-isolation-check--handler-file ()
  "Which VM file holds the multipart/encrypted handler, compiled or not."
  (let ((file (symbol-file 'vm-mime-display-internal-multipart/encrypted)))
    (if file (file-name-base file) "none")))

(princ (format "RESULT epg-holds-the-handler %s\n"
               (vm-pgg-isolation-check--handler-file)))

;; What `C-h v' on a VM option does: Customize asks about the group the
;; option is in, and loads every file registered for its parents.
(custom-load-symbol 'vm-ext)
(princ (format "RESULT vm-ext-loaded-vm-pgg %s\n" (featurep 'vm-pgg)))

;; And when something does load vm-pgg -- opening its own group, say -- it
;; must not take PGP from vm-epg.
(let ((inhibit-message t))
  (require 'vm-pgg))
(princ (format "RESULT vm-pgg-loaded %s\n" (featurep 'vm-pgg)))
(princ (format "RESULT handler-after-vm-pgg %s\n"
               (vm-pgg-isolation-check--handler-file)))
(princ (format "RESULT compose-hook-has-vm-pgg %s\n"
               (and (memq 'vm-pgg-compose-mode-activate vm-mail-mode-hook) t)))
(princ (format "RESULT compose-hook-has-vm-epg %s\n"
               (and (memq 'vm-epg-compose-mode-activate vm-mail-mode-hook) t)))
(princ (format "RESULT vm-pgg-advises-decode %s\n"
               (and (advice-member-p #'vm-pgg--clear-state 'vm-decode-mime-message) t)))

;;; vm-pgg-isolation-check.el ends here
