;;; vm-custom-check.el --- check VM defcustoms against their types -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Prints every VM `defcustom' whose `:type' does not accept its own default
;; value, and exits non-zero if there are any.  Run by
;; `vm-custom-test-every-type-accepts-its-own-default', and by hand as
;;
;;     emacs -Q --batch -L lisp -l test/vm-custom-check.el
;;
;; This is a program rather than part of the test because it has to load every
;; VM module to see every declaration, and those loads add hooks and rewrite
;; menus.  Doing that inside the suite would leave the state behind for the
;; tests that follow, so the test runs this in an Emacs of its own.

;;; Code:

(require 'wid-edit)
;; The `hook' widget and the other Customize types are defined in cus-edit, not
;; in wid-edit; converting one without it fails rather than saying it cannot.
(require 'cus-edit)

(defvar vm-custom-check-minimum 300
  "Fewer declarations than this means the load went wrong, not that VM is small.")

(defun vm-custom-check-load-everything ()
  "Load every VM module, so that every `defcustom' has been seen."
  (require 'vm)
  ;; Some types name functions from libraries VM does not load itself, and the
  ;; `function' widget matches a symbol only once it is `fboundp'.  Without
  ;; this, `vm-dnd-protocol-alist' reads as a mismatch and is not one.
  (require 'dnd nil t)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (dolist (file (directory-files (expand-file-name "../lisp" dir)
                                   nil "\\`vm-.*\\.el\\'"))
      (let ((feature (intern (file-name-sans-extension file))))
        (unless (memq feature '(vm-autoloads vm-cus-load vm-version-conf))
          ;; Not merely a missing file: vm-w3m.el pushes onto a w3m variable as
          ;; it loads, so without w3m installed it raises rather than failing to
          ;; be found.  Either way, what it declares goes unchecked.
          (ignore-errors (require feature nil t)))))))

(defun vm-custom-check-customs ()
  "Return every VM variable that has a `:type', sorted by name."
  (let ((out nil))
    (mapatoms
     (lambda (sym)
       (when (and (string-prefix-p "vm-" (symbol-name sym))
                  (get sym 'custom-type)
                  (boundp sym))
         (push sym out))))
    (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun vm-custom-check-report ()
  "Print the mismatches and how many variables were checked.
Return a cons of (CHECKED . MISMATCHES)."
  (let ((customs (vm-custom-check-customs))
        (mismatched nil))
    (dolist (sym customs)
      (let ((type (get sym 'custom-type))
            (value (default-value sym)))
        (condition-case err
            (unless (widget-apply (widget-convert type) :match value)
              (push (format "%s: type %S rejects default %S" sym type value)
                    mismatched))
          (error (push (format "%s: type %S could not be converted (%s)"
                               sym type (error-message-string err))
                       mismatched)))))
    (dolist (line (nreverse mismatched))
      (princ (format "MISMATCH %s\n" line)))
    (princ (format "checked %d VM defcustoms\n" (length customs)))
    (cons (length customs) (length mismatched))))

(when noninteractive
  (vm-custom-check-load-everything)
  (let* ((result (vm-custom-check-report))
         (checked (car result))
         (mismatches (cdr result)))
    (kill-emacs (cond ((< checked vm-custom-check-minimum) 2)
                      ((> mismatches 0) 1)
                      (t 0)))))

(provide 'vm-custom-check)

;;; vm-custom-check.el ends here
