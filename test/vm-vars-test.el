;;; vm-vars-test.el --- Tests for VM's variables and optional bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; The optional key bindings a preferences file installs.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-when-compile (require 'vm-test-init))

;;; The optional key bindings (emacs-vm/vm#632)
;;
;; vm-v8-key-bindings and vm-v7-key-bindings install the keys that changed
;; between VM 7 and VM 8, and the documented names for them are the aliases
;; vm-current-key-bindings and vm-legacy-key-bindings.  A user puts one of
;; those in their preferences file.  No test called any of the four.

(defmacro vm-vars-test--with-a-copy-of-the-maps (&rest body)
  "Run BODY with copies of the keymaps the binding functions write to.
The functions call `define-key' on the keymap the variable holds, so a test
that let-binds the variables to copies leaves the real maps alone.  Nothing
else would: a keymap is an object, and the harness restores VM's variables
rather than what their values point at."
  (declare (indent 0) (debug t))
  `(let ((vm-mode-map (copy-keymap vm-mode-map))
         (vm-mode-virtual-map (copy-keymap vm-mode-virtual-map))
         (vm-summary-mode-map (copy-keymap vm-summary-mode-map)))
     ,@body))

(ert-deftest vm-vars-test-the-two-key-binding-sets-differ ()
  "VM 8 and VM 7 bind the same keys to different commands.
`<' and `>' move about a thread in VM 8 and about a message in VM 7, and `!'
flags a message in VM 8 where VM 7 gave it to the shell.  Installing one has
to undo the other, or a user who asks for the old keys gets half of each."
  (vm-vars-test--with-a-copy-of-the-maps
    (vm-v8-key-bindings)
    (should (eq (lookup-key vm-mode-map "<") 'vm-promote-subthread))
    (should (eq (lookup-key vm-mode-map ">") 'vm-demote-subthread))
    (should (eq (lookup-key vm-mode-map "!") 'vm-toggle-flag-message))
    (vm-v7-key-bindings)
    (should (eq (lookup-key vm-mode-map "<") 'vm-beginning-of-message))
    (should (eq (lookup-key vm-mode-map ">") 'vm-end-of-message))
    (should (eq (lookup-key vm-mode-map "!") 'shell-command))))

(ert-deftest vm-vars-test-the-documented-names-are-the-aliases ()
  "`vm-current-key-bindings' and `vm-legacy-key-bindings' are the names the
manual gives, and they are aliases for the versioned functions.  A user's
preferences file calls them, so they have to keep working under those names."
  (should (eq (symbol-function 'vm-current-key-bindings) 'vm-v8-key-bindings))
  (should (eq (symbol-function 'vm-legacy-key-bindings) 'vm-v7-key-bindings))
  (vm-vars-test--with-a-copy-of-the-maps
    (vm-legacy-key-bindings)
    (should (eq (lookup-key vm-mode-map "e") 'vm-edit-message))
    (vm-current-key-bindings)
    (should (eq (lookup-key vm-mode-map "!") 'vm-toggle-flag-message))))

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
