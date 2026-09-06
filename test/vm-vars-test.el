;;; vm-vars-test.el --- Tests for VM's variables and optional bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The optional key bindings a preferences file installs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

;;; The keys VM 8 gives its own commands (emacs-vm/vm#632)
;;
;; These keys were left unbound once, because they had meant different things
;; in different versions, and typing one reported that it had an optional
;; binding a preferences file could install.  They are bound in the maps now,
;; and `vm-v8-key-bindings' stays only because the manual told readers to
;; call it.

(defmacro vm-vars-test--with-a-copy-of-the-maps (&rest body)
  "Run BODY with copies of the keymaps the binding function writes to.
The function calls `define-key' on the keymap the variable holds, so a test
that let-binds the variables to copies leaves the real maps alone.  Nothing
else would: a keymap is an object, and the harness restores VM's variables
rather than what their values point at."
  (declare (indent 0) (debug t))
  `(let ((vm-mode-map (copy-keymap vm-mode-map))
         (vm-mode-virtual-map (copy-keymap vm-mode-virtual-map))
         (vm-summary-mode-map (copy-keymap vm-summary-mode-map)))
     ,@body))

(ert-deftest vm-vars-test-the-keys-are-bound-without-being-asked-for ()
  "REGRESSION: `!', `<' and `>' work in a folder as they stand.

They were bound to a stub that reported an optional binding, so typing `!'
on the key the manual gives for flagging a message answered an error.  The
stub and the VM 7 set that went with it are gone, and the bindings are in
`vm-mode-map' itself."
  (should (eq (lookup-key vm-mode-map "!") 'vm-toggle-flag-message))
  (should (eq (lookup-key vm-mode-map "<") 'vm-promote-subthread))
  (should (eq (lookup-key vm-mode-map ">") 'vm-demote-subthread))
  ;; and the keys the VM 7 set used for other things are free
  (dolist (key '("a" "b" "e" "i" "w" "L" "*" "%" "="))
    (should (equal (list key nil) (list key (lookup-key vm-mode-map key))))))

(ert-deftest vm-vars-test-the-installer-binds-what-is-bound-already ()
  "`vm-v8-key-bindings' is kept for preferences files that call it.
It binds what the maps already carry, so calling it changes nothing.  The
documented alias still answers to its name."
  (should (eq (symbol-function 'vm-current-key-bindings) 'vm-v8-key-bindings))
  (vm-vars-test--with-a-copy-of-the-maps
    (vm-v8-key-bindings)
    (should (eq (lookup-key vm-mode-map "!") 'vm-toggle-flag-message))
    (should (eq (lookup-key vm-mode-virtual-map "U")
                'vm-virtual-update-folders))))

(ert-deftest vm-vars-test-the-vm-7-bindings-are-gone ()
  "The VM 7 set and the stub that pointed at it are removed.
A preferences file naming them gets a void-function error, which says what
happened, rather than a set of keys that no longer matches the manual."
  (should-not (fboundp 'vm-v7-key-bindings))
  (should-not (fboundp 'vm-legacy-key-bindings))
  (should-not (fboundp 'vm-optional-key)))

(ert-deftest vm-vars-test-the-virtual-map-gets-its-keys ()
  "VM 8 binds the virtual folder keys, which VM 7 had no map for."
  (vm-vars-test--with-a-copy-of-the-maps
    (vm-v8-key-bindings)
    (should (eq (lookup-key vm-mode-virtual-map "U")
                'vm-virtual-update-folders))
    (should (eq (lookup-key vm-mode-virtual-map "?")
                'vm-virtual-check-selector-interactive))))

(provide 'vm-vars-test)

;;; vm-vars-test.el ends here
