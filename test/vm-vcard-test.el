;;; vm-vcard-test.el --- Tests for vm-vcard.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM's vCard display in vm-vcard.el.  vcard.el is an optional
;; companion VM does not ship, so both sides matter: naming it here puts this
;; file in the runner's no-optional pass as well, where the absence is real
;; rather than stubbed.

;;; Code:

(require 'vm-test-init)
(require 'vm-vcard)

(defconst vm-vcard-test--card
  "BEGIN:VCARD\nVERSION:2.1\nFN:Alice Smith\nEND:VCARD\n"
  "A vCard for the display to render.")

(defun vm-vcard-test--layout ()
  "A MIME layout for a 7bit text/x-vcard part filling the current buffer."
  (let ((layout (make-vector 17 nil)))
    (aset layout 0 '("text/x-vcard"))    ; type
    (aset layout 2 "7bit")               ; encoding
    (aset layout 9 (point-min))          ; body start
    (aset layout 10 (point-max))         ; body end
    layout))

(defmacro vm-vcard-test--without-vcard-el (&rest body)
  "Run BODY with vcard.el reported absent.
Said through `vm-vcard-available-p' rather than by emptying `features',
which is not a special variable and so cannot be let-bound."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'vm-vcard-available-p) #'ignore))
     ,@body))

(ert-deftest vm-vcard-test-declines-when-vcard-el-is-missing ()
  "The vCard displayer answers nil and writes nothing without vcard.el.
It reached vcard.el's own variables regardless, so displaying a vCard part
on a machine without the package signalled void-variable."
  (with-temp-buffer
    (insert vm-vcard-test--card)
    (let ((layout (vm-vcard-test--layout)))
      (vm-vcard-test--without-vcard-el
        (should (null (vm-mime-display-internal-text/x-vcard layout))))
      (should (equal (buffer-string) vm-vcard-test--card)))))

(ert-deftest vm-vcard-test-the-other-two-types-decline-too ()
  "text/vcard and text/directory answer for the same parts, so they decline too."
  (dolist (displayer '(vm-mime-display-internal-text/vcard
                       vm-mime-display-internal-text/directory))
    (with-temp-buffer
      (insert vm-vcard-test--card)
      (let ((layout (vm-vcard-test--layout)))
        (vm-vcard-test--without-vcard-el
          (should (null (funcall displayer layout))))
        (should (equal (buffer-string) vm-vcard-test--card))))))

(ert-deftest vm-vcard-test-declines-on-a-machine-that-really-lacks-it ()
  "The same, with nothing stubbed, in the runner's no-optional pass.
Skipped where vcard.el is installed, which is what the stubbed test above
covers instead.  It asks `require' rather than `vm-vcard-available-p' so
that it still runs against a vm-vcard.el that has no such function."
  (skip-unless (not (require 'vcard nil t)))
  (with-temp-buffer
    (insert vm-vcard-test--card)
    (should (null (vm-mime-display-internal-text/x-vcard
                   (vm-vcard-test--layout))))
    (should (equal (buffer-string) vm-vcard-test--card))))

(ert-deftest vm-vcard-test-renders-a-card-with-vcard-el ()
  "With vcard.el installed the part is rendered and the displayer answers t."
  (skip-unless (vm-vcard-available-p))
  (with-temp-buffer
    (insert vm-vcard-test--card)
    (let ((layout (vm-vcard-test--layout))
          (start (point-max)))
      (goto-char start)
      (should (eq t (vm-mime-display-internal-text/x-vcard layout)))
      (should (string-match-p "Alice Smith"
                              (buffer-substring start (point-max)))))))

(provide 'vm-vcard-test)

;;; vm-vcard-test.el ends here
