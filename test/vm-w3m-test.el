;;; vm-w3m-test.el --- Tests for vm-w3m.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; vm-w3m.el is the emacs-w3m integration.  These tests do not need emacs-w3m
;; installed: the one command they cover is about which w3m function VM calls
;; and what it does to the buffer first, both of which can be watched with the
;; w3m side stubbed.  `make optional-packages' plus vm-optional-test.el is
;; what checks VM against the real thing.

;;; Code:

(require 'vm-test-init)
(require 'vm)
(require 'vm-w3m)

(defmacro vm-w3m-test-with-rendered-part (&rest body)
  "Run BODY in a buffer that looks like a w3m-rendered presentation.
The text carries a `w3m-safe-url-regexp' property, which is how emacs-w3m
records what it was willing to load when it rendered."
  (declare (indent 0))
  `(with-temp-buffer
     (setq major-mode 'vm-presentation-mode)
     (insert "an image goes here\n")
     (put-text-property (point-min) (point-max)
                        'w3m-safe-url-regexp "\\`cid:")
     ,@body))

(ert-deftest vm-w3m-test-toggling-images-calls-a-function-w3m-has ()
  "REGRESSION: the command called `w3m-safe-toggle-inline-images'.
emacs-w3m has no such function -- it went years ago -- so
`vm-w3m-safe-toggle-inline-images' failed with `void-function' for anyone who
tried it.  Found by running VM's calls into emacs-w3m against the installed
emacs-w3m; see vm-optional-test.el."
  (let ((called nil))
    (cl-letf (((symbol-function 'w3m-toggle-inline-images)
               (lambda (&rest args) (setq called (or args t)))))
      (vm-w3m-test-with-rendered-part
        (vm-w3m-safe-toggle-inline-images)
        (should called)))))

(ert-deftest vm-w3m-test-a-prefix-argument-drops-the-safe-regexp ()
  "With a prefix argument every image counts as safe.
emacs-w3m decides from the `w3m-safe-url-regexp' text property it left on the
rendered text, and refuses any image whose URL does not match it, so removing
the property is what makes them all safe.  The old `w3m-safe-toggle-inline-images'
took an argument for this; its replacement does not."
  (cl-letf (((symbol-function 'w3m-toggle-inline-images) #'ignore))
    ;; without the prefix argument the property stays
    (vm-w3m-test-with-rendered-part
      (vm-w3m-safe-toggle-inline-images)
      (should (get-text-property (point-min) 'w3m-safe-url-regexp)))
    ;; with it, the property goes
    (vm-w3m-test-with-rendered-part
      (vm-w3m-safe-toggle-inline-images t)
      (should-not (get-text-property (point-min) 'w3m-safe-url-regexp)))))

(ert-deftest vm-w3m-test-marking-images-safe-leaves-the-buffer-unmodified ()
  "Dropping the property does not make the presentation look edited.
A presentation buffer is a copy, but one that reports itself modified prompts
about saving and confuses `vm-discard-cached-data'."
  (vm-w3m-test-with-rendered-part
    (set-buffer-modified-p nil)
    (vm-w3m-mark-images-safe)
    (should-not (buffer-modified-p))
    (should-not (get-text-property (point-min) 'w3m-safe-url-regexp))))

(ert-deftest vm-w3m-test-toggling-needs-a-presentation ()
  "Called where there is no presentation buffer, the command does nothing.
It is bound in the summary and folder buffers too, where the presentation is
found through `vm-presentation-buffer' and may be nil."
  (let ((called nil))
    (cl-letf (((symbol-function 'w3m-toggle-inline-images)
               (lambda (&rest _) (setq called t))))
      (with-temp-buffer
        (setq major-mode 'vm-mode)
        (setq-local vm-presentation-buffer nil)
        (vm-w3m-safe-toggle-inline-images)
        (should-not called)))))

(provide 'vm-w3m-test)

;;; vm-w3m-test.el ends here
