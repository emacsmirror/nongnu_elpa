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
  (dolist (feature '(vm-avirtual vm-pcrisis))
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

(ert-deftest vm-optional-test-bbdb-names-resolve ()
  "Every BBDB function and variable VM names exists in BBDB.
It did not until #567: VM was written against BBDB 2.x and nine of the names
it used were gone -- `bbdb-record-net', `bbdb-record-raw-notes',
`bbdb-record-putprop', `bbdb-get-field', `bbdb-save-db',
`bbdb-get-addresses' and three 2.x variables.  Two more were present but
had changed meaning: `bbdb-split' swapped its arguments, and
`bbdb-create-internal' reordered its positional ones so that an address
landed in the record\\='s `aka' field."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (require 'vm-pcrisis)
  ;; and vm-serial, which names `bbdb-sc-get-attrib' and is what loads
  ;; bbdb-sc.el where it is defined.  Without this the test passed only
  ;; because some other file in the run had loaded vm-serial first, and
  ;; `./test-runner --one vm-optional-test.el' failed on its own.
  (require 'vm-serial)
  (should (equal nil (vm-optional-test--unresolved "bbdb-"))))

(defconst vm-optional-test--manual
  (expand-file-name "../info/vm.texinfo" vm-test-dir)
  "The manual, whose examples name the companion packages too.")

(defun vm-optional-test--names-the-manual-uses (prefix)
  "Every symbol starting with PREFIX that a Lisp example in the manual names.
Each @lisp and @example block is read as Lisp and walked; a block that is
not Lisp, a shell command or a maildrop specification, is skipped."
  (let ((names (make-hash-table :test 'eq)))
    (with-temp-buffer
      (insert-file-contents vm-optional-test--manual)
      (goto-char (point-min))
      (while (re-search-forward "^@\\(lisp\\|example\\)\n" nil t)
        (let ((start (point))
              (end (save-excursion
                     (and (re-search-forward "^@end \\(lisp\\|example\\)" nil t)
                          (match-beginning 0)))))
          (when end
            (let ((block (buffer-substring-no-properties start end)))
              (goto-char end)
              (with-temp-buffer
                (insert block)
                (goto-char (point-min))
                (ignore-errors
                  (while t
                    (vm-optional-test--walk (read (current-buffer))
                                            prefix names)))))))))
    (sort (hash-table-keys names) #'string<)))

(ert-deftest vm-optional-test-bbdb-names-in-the-manual-resolve ()
  "Every BBDB name the manual tells a reader to call exists in BBDB.

The Address book section put `bbdb-force-record-create\=' on `vm-reply-hook\='.
BBDB 2 had that function and BBDB 3 does not, so a reader who copied the
example got a void function the first time they replied, in a hook they had
no reason to suspect.  The companion of
`vm-optional-test-bbdb-names-resolve\=': that one holds the names VM calls,
this one the names the manual tells the reader to."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'bbdb)
  (require 'bbdb-com)
  (require 'bbdb-mua)
  (require 'bbdb-vm)
  (let ((missing (seq-remove
                  (lambda (symbol)
                    (or (fboundp symbol) (boundp symbol) (macrop symbol)
                        (featurep symbol)
                        (locate-library (symbol-name symbol))))
                  (vm-optional-test--names-the-manual-uses "bbdb-"))))
    (should (equal nil missing))))

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

(defun vm-optional-test--message-from (from &optional to)
  "A folder holding one message with the given From and To."
  (concat "From " (or from "a@example.com") "  Thu Jan  1 00:00:00 2026\n"
          "From: " (or from "a@example.com") "\n"
          "To: " (or to "someone@example.com") "\n"
          "Subject: test\n\nbody\n"))

(ert-deftest vm-optional-test-in-bbdb-selector-answers-for-a-message ()
  "The `in-bbdb' selector says whether BBDB knows the message's correspondent.
The whole path: VM reads the headers, parses the addresses and asks BBDB.  It
used to hand the job to `bbdb-get-addresses', which BBDB 3 does not have."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (vm-optional-test-with-bbdb
    (bbdb-create-internal :name "Alice Example" :mail "alice@example.com")
    (vm-test-with-folder (vm-optional-test--message-from "Alice <alice@example.com>")
      (should (vm-vs-in-bbdb (car vm-message-pointer))))
    (vm-test-with-folder (vm-optional-test--message-from "bob@example.com")
      (should-not (vm-vs-in-bbdb (car vm-message-pointer))))))

(ert-deftest vm-optional-test-in-bbdb-selector-takes-an-address-class ()
  "`(in-bbdb recipients)' looks at the recipient headers and not the sender."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (vm-optional-test-with-bbdb
    (bbdb-create-internal :name "Alice Example" :mail "alice@example.com")
    (vm-test-with-folder (vm-optional-test--message-from
                          "bob@example.com" "Alice <alice@example.com>")
      (should (vm-vs-in-bbdb (car vm-message-pointer) 'recipients))
      (should-not (vm-vs-in-bbdb (car vm-message-pointer) 'authors)))
    ;; and the other way round
    (vm-test-with-folder (vm-optional-test--message-from
                          "Alice <alice@example.com>" "bob@example.com")
      (should (vm-vs-in-bbdb (car vm-message-pointer) 'authors))
      (should-not (vm-vs-in-bbdb (car vm-message-pointer) 'recipients)))))

(ert-deftest vm-optional-test-in-bbdb-selector-names-the-classes-it-has ()
  "An address class that does not exist says which ones do."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (let ((text-quoting-style 'grave))
    (vm-test-with-folder (vm-optional-test--message-from "a@example.com")
      (should (string-match-p
               "authors and recipients"
               (cadr (should-error
                      (vm-vs-in-bbdb (car vm-message-pointer) 'senders))))))))

(ert-deftest vm-optional-test-in-bbdb-selector-in-a-composition ()
  "The same selector, for the message being written."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (vm-optional-test-with-bbdb
    (bbdb-create-internal :name "Alice Example" :mail "alice@example.com")
    (with-temp-buffer
      (mail-mode)
      (insert "To: Alice <alice@example.com>\nSubject: hello\n"
              mail-header-separator "\nbody\n")
      (should (vm-mail-vs-in-bbdb))
      (should (vm-mail-vs-in-bbdb 'recipients))
      (should-not (vm-mail-vs-in-bbdb 'authors)))))

;;; Personality Crisis profiles kept in BBDB

(ert-deftest vm-optional-test-pcrisis-profiles-round-trip-through-bbdb ()
  "A profile stored on a BBDB record is read back.
`vm-pcrisis-auto-profiles-file' set to BBDB keeps each address's profile in the
record's `vmpc-profile' field.  Writing it used `bbdb-record-putprop' and
reading it `bbdb-get-field', neither of which BBDB 3 has."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-pcrisis)
  (vm-optional-test-with-bbdb
    (let ((vm-pcrisis-auto-profiles-file 'BBDB)
          (vm-pcrisis-auto-profiles nil)
          (vm-pcrisis-auto-profiles-expunge-days nil))
      (vm-pcrisis-save-profile-for-address "alice@example.com" '("work"))
      ;; forget what is in memory and read it back from the database
      (setq vm-pcrisis-auto-profiles nil)
      (vm-pcrisis-load-auto-profiles)
      (should (equal '("work") (cadr (assoc "alice@example.com"
                                            vm-pcrisis-auto-profiles)))))))

;;; The virtual folders BBDB records ask for

(ert-deftest vm-optional-test-avirtual-builds-virtual-folders-from-bbdb ()
  "A record with a `vm-virtual' field gets a virtual folder of its addresses.
The field was read with `bbdb-record-raw-notes' and the addresses with
`bbdb-record-net', both gone; and the mail-alias variant split its aliases
with `bbdb-split', whose arguments BBDB 3 takes the other way round."
  (skip-unless (vm-optional-test--installed-p 'bbdb))
  (require 'vm-avirtual)
  (vm-optional-test-with-bbdb
    (let ((vm-virtual-folder-alist nil)
          (vm-primary-inbox "~/INBOX"))
      (bbdb-create-internal :name "Alice Example" :mail "alice@example.com"
                            :xfields '((vm-virtual . "friends")))
      (bbdb/vm-set-virtual-folder-alist)
      (let ((folder (assoc "friends" vm-virtual-folder-alist)))
        (should folder)
        (should (string-match-p "alice@example" (format "%S" folder)))
        (should (string-match-p "author-or-recipient" (format "%S" folder)))))))

(provide 'vm-optional-test)

;;; vm-optional-test.el ends here
