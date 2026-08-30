;;; vm-pgg-isolation-test.el --- vm-pgg keeps to itself -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; vm-pgg is deprecated and defines the same three MIME handlers as vm-epg,
;; so whichever loaded last used to win.  Loading it is not always
;; deliberate, which is what made that a fault rather than a footnote: it
;; declared its customization group as a child of `vm-ext', so `C-h v' on any
;; VM option loaded vm-pgg, and PGP stopped working for a reader who had never
;; asked for it (emacs-vm/vm#785).

;; Asked in a fresh Emacs, because the suite's own has every module loaded
;; already and so cannot tell what a load does.

;;; Code:

(require 'vm-test-init)

(defun vm-pgg-isolation-test--answers ()
  "Run test/vm-pgg-isolation-check.el in a fresh Emacs; answer its results.
An alist of the name it printed against the value, both as strings."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (program (expand-file-name "vm-pgg-isolation-check.el" vm-test-dir)))
    (with-temp-buffer
      (let ((status (call-process emacs nil t nil
                                  "-Q" "--batch"
                                  "-L" vm-test-lisp-dir
                                  "-l" program))
            answers)
        (goto-char (point-min))
        (while (re-search-forward "^RESULT \\([^ ]+\\) \\(.*\\)$" nil t)
          (push (cons (match-string 1) (match-string 2)) answers))
        (unless (equal status 0)
          (error "The check exited %S: %s" status (buffer-string)))
        (nreverse answers)))))

(defvar vm-pgg-isolation-test--cache nil
  "The answers, run once: a fresh Emacs loading all of VM takes a second.")

(defun vm-pgg-isolation-test--answer (name)
  (unless vm-pgg-isolation-test--cache
    (setq vm-pgg-isolation-test--cache (vm-pgg-isolation-test--answers)))
  (cdr (assoc name vm-pgg-isolation-test--cache)))

(ert-deftest vm-pgg-isolation-test-asking-about-a-vm-option-does-not-load-vm-pgg ()
  "`custom-load-symbol' on `vm-ext' leaves vm-pgg alone.
That is what `C-h v' on a VM option does, and it loaded vm-pgg because
vm-pgg.el defined the `vm-pgg' group as a child of `vm-ext', which put the
file in that group's load list.  The group is declared in vm-vars.el now, so
only opening the group itself loads the file."
  ;; the premise: vm-epg holds the handlers to begin with
  (should (equal (vm-pgg-isolation-test--answer "epg-holds-the-handler")
                 "vm-epg"))
  (should (equal (vm-pgg-isolation-test--answer "vm-ext-loaded-vm-pgg") "nil")))

(ert-deftest vm-pgg-isolation-test-loading-vm-pgg-leaves-vm-epg-alone ()
  "Loading vm-pgg over vm-epg changes nothing: vm-epg keeps PGP.
Whichever file loaded last used to hold the three
`vm-mime-display-internal-\\=' handlers, and vm-pgg's advice and compose hook
went in whether or not vm-epg was there.  vm-pgg now installs none of it when
vm-epg is loaded, so no way of reaching vm-pgg can break a reader's PGP."
  (should (equal (vm-pgg-isolation-test--answer "vm-pgg-loaded") "t"))
  (should (equal (vm-pgg-isolation-test--answer "handler-after-vm-pgg")
                 "vm-epg"))
  (should (equal (vm-pgg-isolation-test--answer "compose-hook-has-vm-pgg") "nil"))
  (should (equal (vm-pgg-isolation-test--answer "compose-hook-has-vm-epg") "t"))
  (should (equal (vm-pgg-isolation-test--answer "vm-pgg-advises-decode") "nil")))

(provide 'vm-pgg-isolation-test)

;;; vm-pgg-isolation-test.el ends here
