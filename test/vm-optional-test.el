;;; vm-optional-test.el --- Tests of VM's optional integrations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; VM integrates with packages it does not require: BBDB for addresses,
;; emacs-w3m for HTML, vcard for business cards.  Those paths were the least
;; tested code in VM, and not for want of tests -- a machine without the
;; package cannot run them at all.  The suite has been printing "Could not
;; load feature bbdb" and carrying on for as long as anyone can remember.
;;
;; `make optional-packages' installs them under test/opt/elpa; every test here
;; skips without them, so the suite still runs on a machine that has none.
;;
;; What these check is the seam: that VM's integration files load against the
;; installed version, and that every function and variable VM names in that
;; package still exists there.  A companion renaming something is how these
;; integrations break, and it breaks silently -- the call is in a branch that
;; only runs for someone who has the package.

;;; Code:

(require 'vm-test-init)
(require 'vm)

(defvar vm-optional-test-packages
  '((bbdb    . "bbdb-")
    (w3m     . "w3m-")
    (vcard   . "vcard-"))
  "Each optional package and the prefix of the names it owns.")

(defun vm-optional-test--installed-p (package)
  (and (locate-library (symbol-name package)) t))

(defun vm-optional-test--vm-files ()
  (seq-remove (lambda (file)
                (string-match-p "autoloads\\|cus-load\\|version-conf"
                                (file-name-nondirectory file)))
              (directory-files vm-test-lisp-dir t "\\.el\\'")))

(defun vm-optional-test--walk (form prefix names)
  "Collect into NAMES, a hash table, PREFIX symbols appearing in FORM.
The Lisp reader is used rather than a regexp so that a name mentioned in a
comment or a docstring does not count: only code does."
  (cond ((symbolp form)
         (when (and form (string-prefix-p prefix (symbol-name form)))
           (puthash form t names)))
        ((consp form)
         (vm-optional-test--walk (car form) prefix names)
         (vm-optional-test--walk (cdr form) prefix names))
        ((vectorp form)
         (mapc (lambda (element) (vm-optional-test--walk element prefix names))
               form))))

(defun vm-optional-test--names-used (prefix)
  "Every symbol starting with PREFIX that VM's own code names."
  (let ((names (make-hash-table :test 'eq)))
    (dolist (file (vm-optional-test--vm-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t (vm-optional-test--walk (read (current-buffer)) prefix names))
          (end-of-file nil))))
    (sort (hash-table-keys names) #'string<)))

(defun vm-optional-test--unresolved (prefix)
  "Those of PREFIX's names VM uses that the installed package does not define."
  (seq-remove (lambda (symbol)
                (or (fboundp symbol) (boundp symbol) (macrop symbol)
                    ;; A library of the companion's, named in a `require'
                    ;; rather than called: bbdb-com, bbdb-sc, w3m-util.
                    (featurep symbol)
                    (locate-library (symbol-name symbol))))
              (vm-optional-test--names-used prefix)))

;;; The integration files load

(ert-deftest vm-optional-test-integration-files-load ()
  "Each integration file loads against the installed companion.
vm-w3m.el pushes onto a w3m variable as it loads, so without emacs-w3m it
does not merely fail to find the package -- it raises, and the generated
manual appendix silently loses its options.  That is what this catches."
  (dolist (case '((w3m . vm-w3m) (vcard . vm-vcard) (bbdb . vm-avirtual)))
    (when (vm-optional-test--installed-p (car case))
      (should (require (cdr case) nil t)))))

(ert-deftest vm-optional-test-bbdb-integration-loads ()
  "The three files that call into BBDB load with BBDB present."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (dolist (feature '(vm-avirtual vm-pcrisis vm-rfaddons))
    (should (require feature nil t))))

;;; Every name VM uses still exists

(ert-deftest vm-optional-test-w3m-names-resolve ()
  "Every emacs-w3m function and variable VM names exists in emacs-w3m."
  (skip-unless (vm-optional-test--installed-p 'w3m))
  (require 'vm-w3m)
  (require 'w3m)
  (should (equal nil (vm-optional-test--unresolved "w3m-"))))

(ert-deftest vm-optional-test-vcard-names-resolve ()
  "Every vcard function and variable VM names exists in the vcard package."
  (skip-unless (vm-optional-test--installed-p 'vcard))
  (require 'vm-vcard)
  (require 'vcard)
  (should (equal nil (vm-optional-test--unresolved "vcard-"))))

(ert-deftest vm-optional-test-bbdb-names-that-do-not-resolve ()
  "VM's BBDB integration is written against BBDB 2.x, and this is the list.
Not an assertion that VM is right -- it is not, and #567 is the porting work.
It pins the size of that job, so that a name leaving or arriving on either
side shows up as a change here rather than as a void-function for whoever has
BBDB installed.

The three variables are 2.x's; the six functions were renamed:

    bbdb-record-net          -> bbdb-record-mail
    bbdb-record-raw-notes    -> bbdb-record-xfields
    bbdb-record-putprop      -> bbdb-record-set-xfield
    bbdb-get-field           -> bbdb-record-field
    bbdb-save-db             -> bbdb-save
    bbdb-get-addresses       -> gone; bbdb-message-search is the way in"
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (require 'vm-pcrisis)
  (require 'vm-rfaddons)
  (should (equal '(bbdb-get-addresses
                   bbdb-get-addresses-headers
                   bbdb-get-field
                   bbdb-get-only-first-address-p
                   bbdb-record-net
                   bbdb-record-putprop
                   bbdb-record-raw-notes
                   bbdb-save-db
                   bbdb-user-mail-names)
                 (vm-optional-test--unresolved "bbdb-"))))

;;; What works: the in-bbdb selector

(defmacro vm-optional-test-with-bbdb (&rest body)
  "Run BODY with an empty BBDB database in a temporary file."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "vm-bbdb" t)))
          (bbdb-file (expand-file-name "bbdb" dir))
          (bbdb-db-buffer nil))
     (unwind-protect
         (progn (bbdb-records) ,@body)
       (let ((buffer (get-file-buffer bbdb-file)))
         (when buffer
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory dir t))))

(ert-deftest vm-optional-test-in-bbdb-selector-searches-the-database ()
  "The `in-bbdb' virtual folder selector reaches BBDB and answers.
It goes through `bbdb-message-search', which is the one 2.x call that modern
BBDB kept -- so this is the part of the integration that still works, and the
test says which part that is."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (should (fboundp 'bbdb-message-search))
  (vm-optional-test-with-bbdb
    (bbdb-create-internal "Alice Example" nil nil nil '("alice@example.com") nil)
    (should (bbdb-message-search "Alice Example" "alice@example.com"))
    (should-not (bbdb-message-search "Nobody" "nobody@example.com"))))

(provide 'vm-optional-test)

;;; vm-optional-test.el ends here
