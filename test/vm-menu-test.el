;;; vm-menu-test.el --- Tests for vm-menu.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The VM Developers

;; This file is part of VM.

;;; Commentary:

;; Unit tests for VM menu functions in vm-menu.el

;;; Code:

(require 'vm-test-init)
(require 'vm-menu)

;;; Menu constant existence tests

(ert-deftest vm-menu-test-menu-constants-exist ()
  "Test that menu constant definitions exist."
  (should (boundp 'vm-menu-folders-menu))
  (should (boundp 'vm-menu-folder-menu))
  (should (boundp 'vm-menu-dispose-menu)))

;;; Function existence tests

(ert-deftest vm-menu-test-popup-functions-exist ()
  "Test that popup menu functions exist."
  (should (fboundp 'vm-menu-popup-mode-menu))
  (should (fboundp 'vm-menu-popup-context-menu))
  (should (fboundp 'vm-menu-popup-url-browser-menu))
  (should (fboundp 'vm-menu-popup-mime-dispose-menu))
  (should (fboundp 'vm-menu-popup-fsfemacs-menu)))

(ert-deftest vm-menu-test-install-functions-exist ()
  "Test that menu installation functions exist."
  (should (fboundp 'vm-menu-install-menus))
  (should (fboundp 'vm-menu-install-mail-mode-menu))
  (should (fboundp 'vm-menu-install-visited-folders-menu))
  (should (fboundp 'vm-menu-install-known-virtual-folders-menu)))

(ert-deftest vm-menu-test-folder-functions-exist ()
  "Test that folder menu functions exist."
  (should (fboundp 'vm-menu-hm-make-folder-menu))
  (should (fboundp 'vm-menu-hm-tree-make-menu)))

;;; Variable existence tests

(ert-deftest vm-menu-test-variables-exist ()
  "Test that menu-related variables exist."
  (should (boundp 'vm-use-menus))
  (should (boundp 'vm-popup-menu-on-mouse-3)))

;;; Menu structure tests

(ert-deftest vm-menu-test-folder-menu-is-list ()
  "Test that vm-menu-folder-menu is a list."
  (should (listp vm-menu-folder-menu)))

(ert-deftest vm-menu-test-folder-menu-has-name ()
  "Test that vm-menu-folder-menu has a name."
  (should (stringp (car vm-menu-folder-menu))))

(ert-deftest vm-menu-test-dispose-menu-is-list ()
  "Test that vm-menu-dispose-menu is a list."
  (should (listp vm-menu-dispose-menu)))

(ert-deftest vm-menu-test-dispose-menu-has-name ()
  "Test that vm-menu-dispose-menu has a name."
  (should (stringp (car vm-menu-dispose-menu))))

;;; Predicate function tests

(ert-deftest vm-menu-test-can-save-p-without-folder ()
  "Test vm-menu-can-save-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-save-p)))))

(ert-deftest vm-menu-test-can-revert-p-without-folder ()
  "Test vm-menu-can-revert-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-revert-p)))))

(ert-deftest vm-menu-test-can-recover-p-without-folder ()
  "Test vm-menu-can-recover-p returns nil outside folder context."
  (with-temp-buffer
    (should (null (vm-menu-can-recover-p)))))

;;; Installing the menus (issue #559)

;; The suite runs with `vm-use-menus' nil, because installing the menus splices
;; the visited folders into `vm-menu-folder-menu' and rebuilds
;; `vm-menu-fsfemacs-folder-menu' from it, which no fixture puts back.  See the
;; comment in vm-test-init.el.  This is the one test that switches them on, so it
;; is the only cover the installation has: everything it touches is bound here,
;; the two menu structures deeply, since the splice is by `setcdr'.

(ert-deftest vm-menu-test-visiting-a-folder-fills-in-the-folder-menu ()
  "Visiting a folder adds it to the Folder menu and rebuilds the keymap.
`vm-menu-install-visited-folders-menu' splices `vm-folder-history' into
`vm-menu-folder-menu' after its \"---\" marker and defines
`vm-menu-fsfemacs-folder-menu' from the result."
  (require 'vm)
  (let ((vm-use-menus vm-test-vm-use-menus)
        (vm-menu-folder-menu (copy-tree vm-menu-folder-menu))
        (vm-menu-virtual-menu (copy-tree vm-menu-virtual-menu))
        (vm-menu-fsfemacs-folder-menu (bound-and-true-p
                                       vm-menu-fsfemacs-folder-menu))
        (vm-menu-fsfemacs-virtual-menu (bound-and-true-p
                                        vm-menu-fsfemacs-virtual-menu)))
    (vm-test-with-real-folder (2)
      (let ((file (buffer-file-name)))
        (should file)
        ;; the folder is in the menu, as an item whose suffix is its name
        (should (seq-find (lambda (item)
                            (and (vectorp item)
                                 (equal (plist-get (append item nil) :suffix)
                                        file)))
                          vm-menu-folder-menu))
        ;; and the keymap was rebuilt, so it is a keymap naming the same folder
        (should (keymapp vm-menu-fsfemacs-folder-menu))
        (should (string-match-p (regexp-quote file)
                                (format "%S" vm-menu-fsfemacs-folder-menu)))))))

(ert-deftest vm-menu-test-mail-mode-menu-goes-on-the-mail-map ()
  "Installing the composition menu puts it on `mail-mode-map'.
`vm-reply' calls this as a composition begins, under `vm-use-menus', which the
suite has off, so this is the cover it has.  Both keymaps are bound to copies:
the function defines keys in shared maps, which is state no fixture restores."
  (require 'vm)
  (let ((mail-mode-map (copy-keymap mail-mode-map))
        (vm-mail-mode-map (copy-keymap vm-mail-mode-map))
        (vm-use-menus vm-test-vm-use-menus))
    ;; Emacs has its own Mail menu there, so this has to replace it
    (should-not (eq (lookup-key mail-mode-map [menu-bar mail])
                    vm-menu-fsfemacs-mail-menu))
    (vm-menu-install-mail-mode-menu)
    (let ((entry (lookup-key mail-mode-map [menu-bar mail])))
      (should (keymapp entry))
      (should (eq entry vm-menu-fsfemacs-mail-menu)))))

(ert-deftest vm-menu-test-menubar-buttons-are-not-possible-without-a-window-system ()
  "`vm-menubar-buttons-possible-p' answers for the current windowing system.
A menubar button is a menu entry that acts when clicked, which GTK and NeXTstep
do not allow.  In batch there is no window system, so they are possible, which
is the branch that decides whether `vm-menu-fsfemacs-add-vm-menu' makes a button
or a menu."
  (should (null window-system))
  (should (vm-menubar-buttons-possible-p)))

;;; What the menu predicates say.  Each of the six had only a test that it
;;; was bound.  They all answer for the folder buffer, from whatever buffer
;;; the menu was raised in, and they all answer nil rather than signalling
;;; when there is no folder -- a menu cannot show a backtrace.

(defmacro vm-menu-test-with-folder-buffer (spec &rest body)
  "Run BODY in a buffer whose folder buffer is the one SPEC names.
SPEC is (FOLDER-VAR): it is bound to a buffer in `vm-mode', and BODY runs in
a second buffer pointing at it, as a summary or presentation buffer does."
  (declare (indent 1) (debug t))
  (let ((folder (nth 0 spec)))
    `(let ((,folder (generate-new-buffer " *vm-menu-test-folder*"))
           (other (generate-new-buffer " *vm-menu-test-other*")))
       (unwind-protect
           (with-current-buffer other
             (with-current-buffer ,folder (setq major-mode 'vm-mode))
             (setq vm-mail-buffer ,folder)
             ,@body)
         (with-current-buffer ,folder (set-buffer-modified-p nil))
         (kill-buffer ,folder)
         (kill-buffer other)))))

(ert-deftest vm-menu-test-predicates-are-nil-without-a-folder ()
  "Outside a folder every one of them is nil, not an error."
  (with-temp-buffer
    (setq major-mode 'fundamental-mode)
    (dolist (predicate '(vm-menu-can-get-new-mail-p vm-menu-can-save-p
                         vm-menu-can-revert-p vm-menu-can-recover-p
                         vm-menu-can-expunge-pop-messages-p
                         vm-menu-can-expunge-imap-messages-p))
      (should-not (funcall predicate)))))

(ert-deftest vm-menu-test-can-get-new-mail-p-reads-the-folder ()
  "New mail can be got unless the folder is read-only or blocked.
The answer comes from the folder buffer even though the menu was raised in
the summary."
  (vm-menu-test-with-folder-buffer (folder)
    (with-current-buffer folder
      (setq vm-block-new-mail nil vm-folder-read-only nil))
    (should (vm-menu-can-get-new-mail-p))
    (with-current-buffer folder (setq vm-folder-read-only t))
    (should-not (vm-menu-can-get-new-mail-p))
    (with-current-buffer folder
      (setq vm-folder-read-only nil vm-block-new-mail t))
    (should-not (vm-menu-can-get-new-mail-p))
    ;; a virtual folder has none of its own, and says yes anyway: the
    ;; command gets mail for the folders it is made of
    (with-current-buffer folder (setq major-mode 'vm-virtual-mode))
    (should (vm-menu-can-get-new-mail-p))))

(ert-deftest vm-menu-test-can-save-p-is-about-changes ()
  "Saving is offered when the folder has changes, or is virtual."
  (vm-menu-test-with-folder-buffer (folder)
    (should-not (vm-menu-can-save-p))
    (with-current-buffer folder (insert "changed"))
    (should (vm-menu-can-save-p))
    (with-current-buffer folder
      (set-buffer-modified-p nil)
      (setq major-mode 'vm-virtual-mode))
    (should (vm-menu-can-save-p))))

(ert-deftest vm-menu-test-can-revert-p-wants-a-file-to-revert-to ()
  "Reverting needs both changes and a file: an unsaved folder has nothing
to revert to."
  (vm-menu-test-with-folder-buffer (folder)
    (with-current-buffer folder (insert "changed"))
    (should-not (vm-menu-can-revert-p))
    (with-current-buffer folder
      (setq buffer-file-name "/nonexistent/vm-menu-test/INBOX"))
    (should (vm-menu-can-revert-p))
    (with-current-buffer folder (set-buffer-modified-p nil))
    (should-not (vm-menu-can-revert-p))))

(ert-deftest vm-menu-test-can-recover-p-compares-the-auto-save-file ()
  "Recovery is offered when the auto-save file is newer than the folder."
  (let* ((dir (file-name-as-directory (make-temp-file "vm-menu-test" t)))
         (file (expand-file-name "INBOX" dir))
         (auto (expand-file-name "#INBOX#" dir)))
    (unwind-protect
        (vm-menu-test-with-folder-buffer (folder)
          (write-region "folder\n" nil file nil 'quiet)
          (with-current-buffer folder
            (setq buffer-file-name file
                  buffer-auto-save-file-name nil))
          (should-not (vm-menu-can-recover-p))
          (with-current-buffer folder
            (setq buffer-auto-save-file-name auto))
          ;; no auto-save file yet
          (should-not (vm-menu-can-recover-p))
          (write-region "newer\n" nil auto nil 'quiet)
          (set-file-times auto (time-add (current-time) 60))
          (should (vm-menu-can-recover-p)))
      (delete-directory dir t))))

(ert-deftest vm-menu-test-can-expunge-maildrop-messages-is-for-local-folders ()
  "Expunging the maildrop is offered in a local folder, not in a POP or IMAP
one.  It reads inverted, and is not: `vm-expunge-pop-messages' deletes from
the server the messages a *local* folder retrieved, while in a POP folder
the messages live on the server and `vm-expunge-folder' is the command."
  (vm-menu-test-with-folder-buffer (folder)
    (with-current-buffer folder (setq vm-folder-access-method nil))
    (should (vm-menu-can-expunge-pop-messages-p))
    (should (vm-menu-can-expunge-imap-messages-p))
    (with-current-buffer folder (setq vm-folder-access-method 'pop))
    (should-not (vm-menu-can-expunge-pop-messages-p))
    (should (vm-menu-can-expunge-imap-messages-p))
    (with-current-buffer folder (setq vm-folder-access-method 'imap))
    (should (vm-menu-can-expunge-pop-messages-p))
    (should-not (vm-menu-can-expunge-imap-messages-p))))

(provide 'vm-menu-test)

;;; vm-menu-test.el ends here