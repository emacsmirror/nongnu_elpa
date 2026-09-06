;;; vm-org-test.el --- Org links to VM messages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; vm-org.el was contrib/org-vm.el, which nothing built, nothing tested and
;; nothing documented, and which had gone stale where it touched Org: the
;; link type was registered with a call obsolete since Org 9.0, and link
;; storing hung on a variable Org 9.3 removed, so it did nothing and said so
;; nowhere (emacs-vm/vm#812).  These hold the port shut.

;;; Code:

(require 'vm-test-init)
(require 'vm-org)

(ert-deftest vm-org-test-the-link-type-carries-both-halves ()
  "REGRESSION: Org can both follow a `vm:' link and store one.

emacs-vm/vm#812.  Following worked: `org-add-link-type' is obsolete but
still functions.  Storing did not, and silently: it hung
`org-vm-store-link' on `org-store-link-functions', which Org 9.3 removed, so
`add-hook' made a variable nothing reads.  Both are properties of the link
type now, which is where Org looks for them."
  (should (assoc "vm" org-link-parameters))
  (should (equal 'vm-org-open (org-link-get-parameter "vm" :follow)))
  (should (equal 'vm-org-store-link (org-link-get-parameter "vm" :store))))

(ert-deftest vm-org-test-storing-outside-a-folder-answers-nil ()
  "Org asks every link type to store, so ours must decline quietly.
Signalling here would stop `org-store-link' wherever the reader happened to
be."
  (with-temp-buffer
    (fundamental-mode)
    (should (equal nil (vm-org-store-link)))))

(ert-deftest vm-org-test-a-remote-folder-becomes-a-tramp-name ()
  "REGRESSION: a link naming a user does not answer a doubled at sign.

The group matching the user took in the `@' that follows it and the `@' was
then written again, so //me@host answered /me@@host, which Tramp cannot
open.  Carried over from contrib and found by running it."
  (should (equal "/me@host.example:/var/mail/me"
                 (vm-org-remote-folder "//me@host.example:/var/mail/me")))
  ;; with no user named, the one running Emacs is meant
  (should (equal (format "/%s@host.example:/var/mail/me" (user-login-name))
                 (vm-org-remote-folder "//host.example:/var/mail/me")))
  ;; and a plain folder is not a remote one
  (should (equal nil (vm-org-remote-folder "inbox")))
  (should (equal nil (vm-org-remote-folder "~/mail/inbox"))))

(ert-deftest vm-org-test-a-malformed-link-says-what-one-looks-like ()
  "A link that is not one says so, with the form of a good one."
  (let ((message (condition-case caught (progn (vm-org-open "") nil)
                   (error (error-message-string caught)))))
    (should (stringp message))
    (should (string-match-p "vm:inbox#" message))))

(ert-deftest vm-org-test-a-folder-under-the-folder-directory-is-shortened ()
  "A link holds the folder relative to `vm-folder-directory'.
So that a link made on one machine is followed on another whose mail lives
somewhere else.  A folder outside that directory keeps its name, abbreviated
against the home directory, for the same reason."
  (let ((vm-folder-directory (expand-file-name "~/mail/")))
    (should (equal "inbox"
                   (vm-org-shorten-folder (expand-file-name "~/mail/inbox"))))
    (should (equal "work/2026"
                   (vm-org-shorten-folder (expand-file-name "~/mail/work/2026"))))
    (should (equal "~/archive/2026"
                   (vm-org-shorten-folder (expand-file-name "~/archive/2026")))))
  ;; with no folder directory set, nothing is shortened away
  (let ((vm-folder-directory nil))
    (should (equal "~/mail/inbox"
                   (vm-org-shorten-folder (expand-file-name "~/mail/inbox"))))))

(provide 'vm-org-test)

;;; vm-org-test.el ends here
